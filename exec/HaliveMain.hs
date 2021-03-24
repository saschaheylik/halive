{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Banner
import System.Environment
import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.IORef
import Control.Concurrent.STM
import Halive.SubHalive
import Halive.Recompiler
import Halive.Args
import Halive.Cfg (Cfg(..), decodeCfg)
import NeatInterpolation
import qualified Data.Text.IO as T (putStrLn)

usage :: String
usage = "Usage: halive [-cfg] [-c|--compiled] [-- <args to myapp>]\n\
         Available options:\n\
           -cfg <halive.json>              Path to config file (default: halive.json)\n\
           -c, --compiled                  Faster code (but slower compilation)"

tryReadCfg :: String -> IO (Maybe Cfg)
tryReadCfg cfgPath = do
    res <- try (readFile cfgPath) :: IO (Either SomeException String)
    case res of
        Left _ -> do
            putStrLn $ "\nCould not read the config file at " ++ cfgPath
            putStrLn $ "\nYou have to create a halive.json in your project directory."
            putStrLn $ "Example:\n"
            T.putStrLn [trimming|
                {
                    "cfgFileTypes": [
                        "hs",
                        "pd",
                        "frag",
                        "vert"
                    ],
                    "cfgMainFilePath": "app/Main.hs",
                    "cfgExtensions": [
                        "OverloadedStrings"
                    ],
                    "cfgDisableExtensions": [
                        "ImplicitPrelude"
                    ],
                    "cfgIncludeDirs": [
                        "src"
                    ]
                }
            |]
            putStrLn ""
            return Nothing
        Right file -> do
            case (decodeCfg file) of
                Left e -> do
                    putStrLn $ "Error while trying to parse: " ++ cfgPath
                    putStrLn $ "Error: "
                    putStrLn e
                    return Nothing
                Right cfg -> return $ Just cfg

main :: IO ()
main = do
    args <- parseArgs <$> getArgs
    case args of
        Nothing -> putStrLn usage
        Just Args {..} -> do
            setEnv "Halive Active" "Yes"
            putStrLn banner
            let compMode = if shouldCompile then Compiled else Interpreted
            let cfgPath' = case cfgPath of
                            Nothing -> "halive.json"
                            Just x -> x

            result <- tryReadCfg cfgPath'
            case result of
                Nothing -> putStrLn usage
                Just cfg -> withArgs targetArgs $ startRecompiler cfg compMode

printBanner :: String -> IO ()
printBanner title = putStrLn $ ribbon ++ " " ++ title ++ " " ++ ribbon
    where ribbon = replicate 25 '*'

startRecompiler :: Cfg -> CompilationMode -> IO b
startRecompiler cfg compMode = do
    ghc <- startGHC
        (defaultGHCSessionConfig
            { gscImportPaths = cfgIncludeDirs cfg
            , gscCompilationMode = compMode
            , gscLanguageExtensions = cfgExtensions cfg
            , gscNoLanguageExtensions = cfgDisableExtensions cfg
            })

    recompiler <- recompilerWithConfig ghc RecompilerConfig
        { rccWatchAll = Just (".", (cfgFileTypes cfg))
        , rccExpressions = ["main"]
        , rccFilePath = cfgMainFilePath cfg
        }

    mainThreadId <- myThreadId

    newCodeTChan <- newTChanIO
    isMainRunning <- newIORef False
    _ <- forkIO $ forever $ do
        result <- atomically $ readTChan (recResultTChan recompiler)
        case result of
            Left errors -> do
                printBanner "Compilation Errors, Waiting...     "
                putStrLn errors
            Right values -> do
                printBanner "Compilation Success, Relaunching..."
                case values of
                    [newCode] -> do
                        atomically $ writeTChan newCodeTChan newCode
                        mainIsRunning <- readIORef isMainRunning
                        when mainIsRunning $ killThread mainThreadId
                    _ ->
                        error "Unexpected number of values received on recResultTChan"

    forever $ do
        newCode <- atomically $ readTChan newCodeTChan
        case getCompiledValue newCode of
            Just (mainFunc :: IO ()) -> do
                writeIORef isMainRunning True
                mainFunc `catch` (\x ->
                    putStrLn ("App killed: " ++ show (x :: SomeException)))
                writeIORef isMainRunning False
            Nothing -> do
                putStrLn "main was not of type IO ()"
