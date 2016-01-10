{-# LANGUAGE OverloadedStrings #-}
module Data.Aeson.ZipperSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, Value(..))
import Data.Aeson.Zipper
import qualified Data.Vector as V

spec :: Spec
spec = do
    describe "motion" $ do
        it "up of top" $ getValue (tloc >>= up) `shouldBe` Null
        it "bar" $ getValue bar `shouldBe` Bool True
        it "second entry of foo" $ getValue (bar >>= sibling "foo" >>= entry 1)
            `shouldBe` Number 2
    describe "edits" $ do
        it "change value" $
            getValue (foo >>= entry 0 >>= replace (Number 3) >>= up)
            `shouldBe` Array (V.fromList [Number 3, Number 2])
        it "add sibling" $ getValue (bar >>= addSibling "baz" (String "hi"))
            `shouldBe` String "hi"
        it "add preceding entry" $
            getValue (foo >>= entry 1 >>= addBefore (Number 4) >>= up)
            `shouldBe` Array (V.fromList [Number 1, Number 4, Number 2])
        it "add following entry" $
            getValue (foo >>= entry 1 >>= addAfter (Number 5) >>= up)
            `shouldBe` Array (V.fromList [Number 1, Number 2, Number 5])
  where
    tloc = decode "{\"root\": {\"foo\": [1,2], \"bar\": true}}" >>= anchor
    bar = tloc >>= child "root" >>= child "bar"
    foo = tloc >>= child "root" >>= child "foo"
