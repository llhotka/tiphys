-- |
-- Module:      Data.Aeson.JsonPointer.Internal
-- Copyright:   © 2016 Ladislav Lhotka
-- License:     GPL-3
-- Maintainer:  Ladislav Lhotka <lhotka@nic.cz>
-- Stability:   experimental
-- Portability: portable

-- Implementation of the JSON Pointer interface.

module Data.Aeson.JsonPointer.Internal
    (
    -- * Parsers
      entryToken
    , memberToken
    -- * JSON Pointer evaluation
    , descend
    ) where

import Control.Applicative ((<|>))
import Control.Error (hush)
import Control.Monad (foldM)
import Data.Aeson (Value(..))
import Data.Aeson.Zipper
import Data.Attoparsec.Text (Parser, (<?>), atEnd, char, choice,
                             decimal, endOfInput, parseOnly, takeTill)
import Data.Text (Text)
import qualified Data.Text as T

-- | Parse an array entry reference token.
entryToken :: Parser Int
entryToken = choice
    [ char '-' *> fail "reference to nonexistent array entry"
    , char '0' *> return 0
    , decimal
    ] <* (endOfInput <?> "bad entry reference, expected")

-- | Parse an object member reference token.
memberToken :: Parser Text
memberToken = do
    pref <- takeTill (== '~')
    stop <- atEnd
    if stop then return pref else do
        ec <- unesc
        suff <- memberToken
        return $ pref `T.append` T.cons ec suff
  where
    unesc = char '~' *>
        ((char '0' *> return '~') <|> (char '1' *> return '/'))

-- | Evaluate the JSON pointer and move to the corresponding location.
descend :: Text                 -- ^ JSON pointer
        -> Location             -- ^ starting location
        -> Maybe Location       -- ^ target location
descend jp v
    | T.head jp /= '/' = Nothing
    | otherwise        =
        foldM step v $ T.split (== '/') (T.tail jp)
  where
    step loc@(Loc (Object _) _) ref = do
        pr <- hush $ parseOnly memberToken ref
        child pr loc
    step loc@(Loc (Array _) _) ref = do
        pr <- hush $ parseOnly entryToken ref
        entry pr loc
