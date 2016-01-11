{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Data.Aeson.Zipper
-- Copyright:   (C) 2016 Ladislav Lhotka
-- License:     BSD3
-- Maintainer:  Ladislav Lhotka <lhotka@nic.cz>
-- Stability:   experimental
-- Portability: portable
--
-- Zipper interface for JSON 'Data.Aeson.Value'.

module Data.Aeson.Zipper
    (
    -- * Using the zipper interface
    -- $usage

    -- * Example
    -- $example

    -- * Zipper types
      Context
    , Location(..)
    -- * Adding and removing context
    , anchor
    , value
    , getValue
    -- * Functions for all locations
    , replace
    , up
    , top
    -- * Functions for object locations
    , child
    -- * Functions for object member locations
    , sibling
    , addSibling
    -- * Functions for array locations
    , entry
    , firstEntry
    , lastEntry
    -- * Functions for array entry locations
    , next
    , previous
    , back
    , forward
    , jump
    , addBefore
    , addAfter
    ) where

import Data.Aeson.Zipper.Internal

-- $usage
--
-- This module implements a zipper interface for JSON 'Value's pretty
-- much along the lines of Gérard Huet's original
-- <https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf paper>: it defines the 'Location' type containing
--
-- * a JSON 'Value' that is the current focus, and
--
-- * context representing essentially the rest of the JSON document
-- with a hole in the place of the focused value.
--
-- Due to the heterogeneity of JSON data, the API is not as uniform as
-- for "neat" data structures in that some operations are only
-- intended to work for certain 'Location' types. In the following
-- subsections, the functions are classified according to the context
-- for which they are designed.
--
-- Typically, the motion and editing operations will be executed as
-- actions in the 'Maybe' monad. Such an action may fail if
--
-- * it is executed in a wrong location, or
--
-- * the value being addressed doesn't exist (e.g. executing
-- 'previous' if the focus is on the first entry of an array).

-- $example
--
-- Decode a simple JSON document and put it into the top-level zipper context:
--
-- >>> let tloc = decode "{\"root\": {\"foo\": [1,2], \"bar\": true}}" >>= anchor
--
-- Move all the way to entry #1 of the @foo@ array:
--
-- >>> let foo1 = tloc >>= child "root" >>= child "foo" >>= entry 1
--
-- The value at this location is number 2:
--
-- >>> getValue foo1
-- Number 2.0
--
-- Add a new entry – number 3 – before that entry, and go back to the top location:
--
-- >>> let tloc' = foo1 >>= addBefore (Number 3) >>= top
--
-- Now we can extract and encode the modified JSON document:
--
-- >>> encode $ getValue tloc'
-- "{\"root\":{\"foo\":[1,3,2],\"bar\":true}}"
