{-# LANGUAGE RecordWildCards #-}

module Halive.Args
    ( Args(..)
    , FileType
    , parseArgs
    ) where

type FileType = String

data Args = Args
    { targetArgs    :: [String]
    , shouldCompile :: Bool
    , cfgPath :: Maybe String
    }

parseArgs :: [String] -> Maybe Args
parseArgs args = go args (Args [] False Nothing)
    where
        go :: [String] -> Args -> Maybe Args
        go [] args' = Just args'
        go (x : xs) args'
            | x == "--" = Just args' { targetArgs = xs }
            | x == "-c" || x == "--compiled" =
                go xs $ args' { shouldCompile = True }
            | x == "-cfg" =
                case xs of
                    []               -> Nothing
                    ("--" : _)       -> Nothing
                    (cfgPath' : xs') -> go xs' $ args' { cfgPath = Just cfgPath' }
            | otherwise = Nothing
