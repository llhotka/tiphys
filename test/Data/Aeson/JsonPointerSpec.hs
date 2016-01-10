{-# LANGUAGE OverloadedStrings #-}
module Data.Aeson.JsonPointerSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, Value(..))
import Data.Aeson.JsonPointer

spec :: Spec
spec = do
    describe "descent" $ do
        it "object in object" $ getValue (tloc >>= descend "/root/bar")
            `shouldBe` Bool True
        it "nonexistent member" $ getValue (tloc >>= descend "/root/baz")
            `shouldBe` Null
        it "second entry of foo" $ getValue (tloc >>= descend "/root/foo/1")
            `shouldBe` Number 2
        it "in two steps" $
            getValue (tloc >>= descend "/root" >>= descend "/foo/1")
            `shouldBe` Number 2
        it "nonexistent array entry"
            $ getValue (tloc >>= descend "/root/foo/-") `shouldBe` Null
  where
    tloc = decode "{\"root\": {\"foo\": [1,2], \"bar\": true}}" >>= anchor
