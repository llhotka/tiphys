{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Data.Aeson.Zipper.Internal
-- Copyright:   © 2016 Ladislav Lhotka
-- License:     GPL-3
-- Maintainer:  Ladislav Lhotka <lhotka@nic.cz>
-- Stability:   experimental
-- Portability: portable

-- Implementation of the zipper interface.

module Data.Aeson.Zipper.Internal
    (
    -- * Zipper types
      Context(..)
    , Location(..)
    -- * Adding and removing context
    , anchor
    , value
    , getValue
    -- * Motion primitives
    , child
    , sibling
    , entry
    , firstEntry
    , lastEntry
    , next
    , previous
    , up
    , top
    -- * Updates
    , replace
    , addSibling
    , addBefore
    , addAfter
    ) where

import Data.Aeson (Object, Value(..))
import Data.List (uncons)
import Data.Text (Text)
import qualified Data.Vector as V 
import qualified Data.HashMap.Strict as H

type ValueList = [Value]

-- | Zipper context.
data Context
    = Top                                   -- ^ top-level context
    | Member !Text !Object !Context         -- ^ object member context
    | Entry !ValueList !ValueList Context   -- ^ array entry context
    deriving (Show)

-- | Value with zipper context.
data Location = Loc Value Context deriving (Show)

-- | Add top-level context to a value.
anchor :: Value -> Maybe Location
anchor v = Just $ Loc v Top

-- | Return the location's value.
value :: Location -> Value
value (Loc v _) = v

-- | Return the value that may not be present. 'Null' represents a
-- missing value.
getValue :: Maybe Location -> Value
getValue = maybe Null value

-- | When at object location, go to the child member with the
-- specified name.
child :: Text -> Location -> Maybe Location
child k (Loc (Object obj) ctx) = do
    ch <- H.lookup k obj
    return $ Loc ch (Member k (H.delete k obj) ctx)
child _ _ = Nothing

-- | When at object member location, go to the sibling member with
-- the specified name.
sibling :: Text -> Location -> Maybe Location
sibling k (Loc v (Member k' obj ctx)) = do
    s <- H.lookup k obj
    return $ Loc s $ Member k (H.insert k' v $ H.delete k obj) ctx
sibling _ _ = Nothing

-- | When at array location, go to the entry with the specified
-- position.
entry :: Int -> Location -> Maybe Location
entry n (Loc (Array ary) ctx) = do
    e <- s V.!? 0
    return $ Loc e $ Entry (V.toList p) (V.toList $ V.tail s) ctx
  where
    (p,s) = V.splitAt n ary 
entry _ _ = Nothing

-- | When at array location, go to the first entry.
firstEntry :: Location -> Maybe Location
firstEntry (Loc (Array ary) ctx) =
    if V.null ary
    then Nothing
    else Just $ Loc (V.head ary) $ Entry [] (V.toList $ V.tail ary) ctx

-- | When at array location, go to the last entry.
lastEntry :: Location -> Maybe Location
lastEntry (Loc (Array ary) ctx) =
    if V.null ary
    then Nothing
    else Just $ Loc (V.last ary) $ Entry [] (V.toList $ V.init ary) ctx

-- | When at array entry location, move to the following entry.
next :: Location -> Maybe Location
next (Loc v (Entry p s ctx)) = do
    (v',s') <- uncons s
    return $ Loc v' $ Entry (v:p) s' ctx
next _ = Nothing

-- | When at array entry location, move to the preceding entry.
previous :: Location -> Maybe Location
previous (Loc v (Entry p s ctx)) = do
    (v',p') <- uncons p
    return $ Loc v' $ Entry p' (v:s) ctx
previous _ = Nothing

-- | Ascend to the parent location.
up :: Location -> Maybe Location
up (Loc _ Top) = Nothing
up (Loc v (Member k obj ctx)) = Just $ Loc (Object $ H.insert k v obj) ctx
up (Loc v (Entry p s ctx)) = Just $
    Loc (Array . V.fromList $ reverse p ++ v:s) ctx

-- | Move to the top.
top :: Location -> Maybe Location
top loc = case up loc of
    Nothing -> Just loc
    Just p -> top p

-- | Place a new value at the current location.
replace :: Value -> Location -> Maybe Location
replace v (Loc _ ctx) = Just $ Loc v ctx

-- | When at object member location, add a new sibling with the given name
-- and value, and move the focus to the new sibling. 'Nothing' is
-- returned if a sibling of that name already exists.
addSibling :: Text -> Value -> Location -> Maybe Location
addSibling k v (Loc v' (Member k' obj ctx)) =
    if H.member k obj
    then Nothing
    else Just $ Loc v (Member k (H.insert k' v' obj) ctx)
addSibling _ _ _ = Nothing

-- | When at array entry location, add a new entry /before/ the current
-- location, and move the focus to the new entry.
addBefore :: Value -> Location -> Maybe Location
addBefore v (Loc v' (Entry p s ctx)) = Just $ Loc v $ Entry p (v':s) ctx
addBefore _ _ = Nothing

-- | When at array entry location, add a new entry /after/ the current
-- location, and move the focus to the new entry.
addAfter :: Value -> Location -> Maybe Location
addAfter v (Loc v' (Entry p s ctx)) = Just $ Loc v $ Entry (v':p) s ctx
addAfter _ _ = Nothing
