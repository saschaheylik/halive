{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Halive.ArgsSpec where

import Test.Hspec
import Halive.Args
import Data.Maybe
import Data.Foldable

deriving instance Show Args

spec :: Spec
spec = 
    describe "Args.parseArgs" $ do
        describe "invalid option" $ do
            it "should return Nothing (results in usage being shown)" $
                parseArgs (words "foo") `shouldSatisfy` isNothing

        describe "compiled flag" $ do
            it "is optional and defaults to False" $
                case parseArgs (words "") of
                    Just Args {..} -> shouldCompile `shouldBe` False
            it "can be set to true" $
                case parseArgs (words "-c") of
                    Just Args {..} -> shouldCompile `shouldBe` True
            it "can be set to true" $
                case parseArgs (words "--compiled") of
                    Just Args {..} -> shouldCompile `shouldBe` True

        describe "cfg flag" $ do
            it "is optional and defaults to Nothing" $
                case parseArgs (words "") of
                    Just Args {..} -> cfgPath `shouldBe` Nothing
            it "can be parsed" $
                case parseArgs (words "-cfg test.json") of
                    Just Args {..} -> cfgPath `shouldBe` (Just "test.json")

        describe "target args" $ do
            it "are optional" $
                case parseArgs (words "") of
                    Just Args {..} -> targetArgs `shouldBe` []

            it "capture everything after `--`" $ 
                case parseArgs (words "-cfg test.json -- -f a b c") of
                    Just Args {..} -> targetArgs `shouldBe` ["-f", "a", "b", "c"]
