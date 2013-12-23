module Symtable ( Symbol
                , Symtable
                , empty
                , Symtable.lookup
                , Symtable.elem
                , insert
                )
where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B

-- |For now, a symbol is simply a bytestring.
type Symbol = B.ByteString

-- |Mapping from symbols to abstract attributes.
newtype Symtable a = MkSymtable (M.Map Symbol a)
    deriving (Show)

-- |Create an empty symbol table.
empty :: Symtable a
empty = MkSymtable (M.empty)

-- |Lookup a symbol in the table and return its associated attribute.
-- Might throw an exception.
lookup :: Symbol -> Symtable a -> a
lookup sym (MkSymtable t) = t M.! sym

-- |Test whether symbol is defined in the table.
elem :: Symbol -> Symtable a -> Bool
elem sym (MkSymtable t) = sym `M.member` t

-- |Add a new symbol/attribute pair into a table.
insert :: Symbol -> a -> Symtable a -> Symtable a
insert sym info (MkSymtable t) = MkSymtable (M.insert sym info t)

-- |Merge the symbol information of two tables together.
-- If a symbol is defined in both, the definition from the
-- left table is considered.
combine :: Symtable a -> Symtable a -> Symtable a
combine (MkSymtable s) (MkSymtable t) = MkSymtable (M.union s t)

instance Monoid (Symtable a) where
    mempty  = empty
    mappend = combine

instance Functor Symtable where
    fmap f (MkSymtable t) = MkSymtable (fmap f t)
