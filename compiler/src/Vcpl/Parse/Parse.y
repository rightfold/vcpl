{
{-# LANGUAGE NoStrictData #-}

module Vcpl.Parse.Parse where

import Vcpl.Syntax

import Vcpl.Parse.Lex (Token (..))
}

%name parse

%tokentype { Token }

%token
  keywordIdentification                 { KeywordIdentification }
  keywordComputation                    { KeywordComputation }
  keywordDivision                       { KeywordDivision }
  keywordForall                         { KeywordForall }
  keywordLambda                         { KeywordLambda }
  keywordValue                          { KeywordValue }
  keywordType                           { KeywordType }

  punctuationHyphenGreaterThan          { PunctuationHyphenGreaterThan }
  punctuationLeftParenthesis            { PunctuationLeftParenthesis }
  punctuationRightParenthesis           { PunctuationRightParenthesis }
  punctuationPeriod                     { PunctuationPeriod }

  identifier                            { Identifier $$ }

%%

--------------------------------------------------------------------------------
-- Definitions

Definition :: { Definition }
Definition
  : keywordIdentification keywordDivision
    keywordValue Identifier
    keywordType Expression

    keywordComputation keywordDivision
    Expression

    {
      ValueDefinition $4 (expression $6 Types) (expression $9 Terms)
    }

--------------------------------------------------------------------------------
-- Expressions

Expression :: { ExpressionFactory }
Expression
  : Expression2
      { $1 }

Expression2 :: { ExpressionFactory }
Expression2
  : Expression1
      { $1 }
  | Expression2 Expression1
      { ExpressionFactory $ \u ->
          ApplicationExpression (expression $1 u)
                                (expression $2 u) }

Expression1 :: { ExpressionFactory }
Expression1
  : Identifier
      { ExpressionFactory $ \_ -> VariableExpression $1 }
  | keywordLambda Identifier punctuationPeriod Expression
      { ExpressionFactory $ \case
          u@Terms -> LambdaExpression $2 (expression $4 u)
          _       -> error "WRONG" }
  | keywordForall Identifier punctuationPeriod Expression
      { ExpressionFactory $ \case
          u@Types -> ForallExpression $2 (expression $4 u)
          _       -> error "WRONG" }
  | punctuationLeftParenthesis Expression punctuationRightParenthesis
      { $2 }

--------------------------------------------------------------------------------
-- Miscellaneous

Identifier :: { Identifier }
Identifier
  : identifier
      { AlphanumericIdentifier $1 }

{
-- |
-- Newtype wrapper for functions that create expressions in a given universe.
-- The universe is propagated for case analysis using GADT matching. It has to
-- be a newtype to avoid monomorphization in Happy-generated code.
newtype ExpressionFactory =
  ExpressionFactory
    { expression :: forall u. Universe u -> Expression u }

happyError :: [Token] -> a
happyError = error . show
}
