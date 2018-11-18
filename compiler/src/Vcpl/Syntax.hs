module Vcpl.Syntax
  ( -- * Names
    Identifier (..)

    -- * Definitions
  , Definition (..)

    -- * Expressions
  , Universe (..), Term, Type, pattern Terms, pattern Types
  , Expression (..)
  ) where

import Data.ByteString (ByteString)
import GHC.TypeLits (type (+), Nat)

--------------------------------------------------------------------------------
-- Names

-- |
-- Identifiers are used as names for things.
newtype Identifier
  = AlphanumericIdentifier ByteString
  deriving stock (Eq, Ord, Read, Show)

--------------------------------------------------------------------------------
-- Definitions

-- |
-- Top-level definitions.
data Definition
  = ValueDefinition Identifier (Expression Type) (Expression Term)

--------------------------------------------------------------------------------
-- Expressions

-- |
-- Expressions are either terms, types, or kinds. Type checking and inference
-- is nearly the same at all universes, so we make the AST generic in universe.
data Universe :: Nat -> * where
  UniverseZero :: Universe 0
  UniverseSucc :: Universe n -> Universe (n + 1)

type Term = 0
type Type = 1

pattern Terms = UniverseZero
pattern Types = UniverseSucc UniverseZero

-- |
-- Universe-indexed expressions. Some expressions may occur only at certain
-- universes, hence this is a GADT.
data Expression :: Nat -> * where
  VariableExpression :: Identifier -> Expression u
  ApplicationExpression :: Expression u -> Expression u -> Expression u
  LambdaExpression :: Identifier -> Expression Term -> Expression Term
  ForallExpression :: Identifier -> Expression Type -> Expression Type
