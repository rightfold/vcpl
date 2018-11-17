{
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}
{-# LANGUAGE NoStrictData #-}

module Vcpl.Parse.Lex where

import Control.Lens ((^.), _4, to)
import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as BS.C8
}

%wrapper "monad"

tokens :-
  [\ \t\r\n]+                           ;
  "*>".*                                ;

  "IDENTIFICATION"                      { \_ _ -> pure $ KeywordIdentification }
  "COMPUTATION"                         { \_ _ -> pure $ KeywordComputation }
  "DIVISION"                            { \_ _ -> pure $ KeywordDivision }
  "FORALL"                              { \_ _ -> pure $ KeywordForall }
  "LAMBDA"                              { \_ _ -> pure $ KeywordLambda }
  "VALUE"                               { \_ _ -> pure $ KeywordValue }
  "TYPE"                                { \_ _ -> pure $ KeywordType }

  "->"                                  { \_ _ -> pure $ PunctuationHyphenGreaterThan }
  "("                                   { \_ _ -> pure $ PunctuationLeftParenthesis }
  ")"                                   { \_ _ -> pure $ PunctuationRightParenthesis }
  "."                                   { \_ _ -> pure $ PunctuationPeriod }

  [a-z][a-z\-]*                         { \i n -> pure $ Identifier (i ^. _4 . to (take n) . to BS.C8.pack) }

{
data Token :: * where
  EOF                                   :: Token

  KeywordIdentification                 :: Token
  KeywordComputation                    :: Token
  KeywordDivision                       :: Token
  KeywordForall                         :: Token
  KeywordLambda                         :: Token
  KeywordValue                          :: Token
  KeywordType                           :: Token

  PunctuationHyphenGreaterThan          :: Token
  PunctuationLeftParenthesis            :: Token
  PunctuationRightParenthesis           :: Token
  PunctuationPeriod                     :: Token

  Identifier                            :: !ByteString -> Token

  deriving stock (Eq, Ord, Read, Show)

alexEOF :: Applicative f => f Token
alexEOF = pure EOF

scanToken :: Alex Token
scanToken = alexMonadScan

scanTokens :: Alex [Token]
scanTokens =
  scanToken >>= \case
    EOF -> pure [EOF]
    tok -> (tok :) <$> scanTokens
}
