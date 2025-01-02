{-# LANGUAGE LambdaCase #-}
module Tokens where

-- Main token type that encompasses all possible tokens
import Text.ParserCombinators.ReadP (string)
import Data.Map (Map, fromList, lookup)
import Data.Char (toUpper)
import qualified Data.Map as Map

-- All possible token types
data Token = 
    TIdentifier String    -- Regular identifiers
  | TKeyword Keyword     -- SQL keywords
  | TOperator Operator   -- Both binary and unary operators
  | TLiteral Literal     -- Literal values
  | TSymbol Symbol       -- Special symbols
  | TPipe                -- Pipeline operator
  | TEOF                 -- End of file marker
  deriving (Show, Eq)

-- SQL Keywords
data Keyword =
    Select
  | From
  | Where
  | Insert
  | Into
  | Values
  | Update
  | Set
  | Delete
  | Inner
  | Join
  | JLeft -- prefix with J so it doesnt overlap with Haskell's keywords
  | JRight
  | Full
  | Outer
  | On
  | Group
  | By
  | Order
  | Asc
  | Desc
  | Count
  | Sum
  | Avg
  | Min
  | Max
  | As
  | Aggregate
  deriving (Show, Ord, Eq)

-- Operators (combining binary and unary)
data Operator =
    -- Binary operators
    Equals            -- =
  | GreaterThan      -- >
  | LessThan         -- <
  | GreaterEquals    -- >=
  | LessEquals       -- <=
  | NotEquals        -- <>
  | Like             -- LIKE
  | And              -- AND
  | Or               -- OR
    -- Unary operators
  | Not              -- NOT
  deriving (Show, Eq)

-- Special symbols
data Symbol =
    LeftParen        -- (
  | RightParen       -- )
  | Semicolon        -- ;
  deriving (Show, Eq)

-- Literal values
data Literal =
    StringLit String
  | NumberLit Double
  | BoolLit Bool
  | NullLit
  deriving (Show, Eq)
-- Bidirectional maps for keywords
keywordToStringMap :: Map Keyword String
keywordToStringMap = fromList [ 
    (Select, "SELECT"),
    (From, "FROM"),
    (Where, "WHERE"),
    (Insert, "INSERT"),
    (Into, "INTO"),
    (Values, "VALUES"),
    (Update, "UPDATE"),
    (Set, "SET"),
    (Delete, "DELETE"),
    (Inner, "INNER"),
    (Join, "JOIN"),
    (JLeft, "LEFT"),
    (JRight, "RIGHT"),
    (Full, "FULL"),
    (Outer, "OUTER"),
    (On, "ON"),
    (Group, "GROUP"),
    (By, "BY"),
    (Order, "ORDER"),
    (Asc, "ASC"),
    (Desc, "DESC"),
    (Count, "COUNT"),
    (Sum, "SUM"),
    (Avg, "AVG"),
    (Min, "MIN"),
    (Max, "MAX"),
    (As, "AS"),
    (Aggregate, "AGGREGATE")
    ]

stringToKeywordMap :: Map String Keyword
stringToKeywordMap = fromList [(str, kw) | (kw, str) <- Map.toList keywordToStringMap]

-- Convert Keyword to String
keywordToString :: Keyword -> String
keywordToString kw = case Map.lookup kw keywordToStringMap of
    Just str -> str
    Nothing -> error $ "Invalid keyword: " ++ show kw

-- Convert String to Maybe Keyword
stringToKeyword :: String -> Maybe Keyword
stringToKeyword str = Map.lookup (map toUpper str) stringToKeywordMap

-- Helper function to convert operators to strings
operatorToString :: Operator -> String
operatorToString = \case
    Equals -> "="
    GreaterThan -> ">"
    LessThan -> "<"
    GreaterEquals -> ">="
    LessEquals -> "<="
    NotEquals -> "<>"
    Like -> "LIKE"
    And -> "AND"
    Or -> "OR"
    Not -> "NOT"

-- Helper function to convert symbols to strings
symbolToString :: Symbol -> String
symbolToString = \case
    LeftParen -> "("
    RightParen -> ")"
    Semicolon -> ";"