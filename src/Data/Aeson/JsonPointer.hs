-- |
-- Module:      Data.Aeson.JsonPointer
-- Copyright:   (C) 2016 Ladislav Lhotka
-- License:     BSD3
-- Maintainer:  Ladislav Lhotka <lhotka@nic.cz>
-- Stability:   experimental
-- Portability: portable
--
-- JSON Pointer evaluation.

module Data.Aeson.JsonPointer
    ( module Data.Aeson.Zipper
    -- * Using JSON Pointer
    -- $usage

    -- * Example
    -- $example

    -- * JSON Pointer evaluation
    , descend
    ) where

import Data.Aeson.JsonPointer.Internal
import Data.Aeson.Zipper

-- $usage
--
-- This module extends the "Data.Aeson.Zipper" interface with an
-- additional function, 'descend', that allows for moving to any
-- location inside a JSON document in one step. The \"address\" of the
-- target location is specified using JSON Pointer
-- \[<https://tools.ietf.org/html/rfc6901 RFC 6901>\].

-- $example
--
-- Decode a JSON document and anchor it to the top-level zipper context:
--
-- >>> let tloc = decode "{\"root\": {\"foo\": [1,2], \"bar\": true}}" >>= anchor
--
-- Move to the zeroth entry of the @foo@ array using JSON Pointer:
--
-- >>> let foo0 = tloc >>= descend "/root/foo/0"
--
-- The value in that location is number 1:
--
-- >>> getValue foo0
-- Number 1.0
--
-- And we change this entry to the string @hi@ and return to the top:
--
-- >>> let tloc' = foo0 >>= replace (String "hi") >>= top
--
-- Now we can check the modified JSON document:
--
-- >>> encode $ getValue tloc'
-- "{\"root\":{\"foo\":[\"hi\",2],\"bar\":true}}"

