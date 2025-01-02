module Lexer where

import Data.Char (isAlpha, isDigit, isSpace, toUpper)
import Data.List (isPrefixOf)
import Tokens

-- Convert SQL statement to a list of tokens
tokenize :: String -> [Token]
tokenize str = 
    let input = removeLeadingWhitespace str in
    case input of
        [] -> [TEOF]
        (x:xs) 
            | isAlpha x -> 
                let (word, rest) = span isIdentChar input
                    upperWord = map toUpper word
                in case matchOperator upperWord of
                    Just op -> TOperator op : tokenize rest
                    Nothing -> case stringToKeyword upperWord of
                        Just kw -> TKeyword kw : tokenize rest
                        Nothing -> TIdentifier word : tokenize rest
            | isDigit x -> lexNumber input
            | x == '"' || x == '\'' -> lexString input
            | x == '(' -> TSymbol LeftParen : tokenize xs
            | x == ')' -> TSymbol RightParen : tokenize xs
            | x == ';' -> TSymbol Semicolon : tokenize xs
            | x == '|' && (not . null) xs && head xs == '>' -> 
                TPipe : tokenize (tail xs)
            | otherwise -> lexOperator input

matchOperator :: String -> Maybe Operator
matchOperator word = case word of
    "AND" -> Just And
    "OR" -> Just Or
    "NOT" -> Just Not
    "LIKE" -> Just Like
    _ -> Nothing

removeLeadingWhitespace :: String -> String
removeLeadingWhitespace [] = []
removeLeadingWhitespace (x:xs)
    | isSpace x = removeLeadingWhitespace xs
    | otherwise = x:xs

lexIdentifier :: String -> [Token]
lexIdentifier str = 
    let (ident, rest) = span isIdentChar str
        upperIdent = map toUpper ident
        token = case stringToKeyword upperIdent of
            Just kw -> TKeyword kw
            Nothing -> TIdentifier ident
    in token : tokenize rest

lexNumber :: String -> [Token]
lexNumber str =
    let (numStr, rest) = span (\c -> isDigit c || c == '.') str
        num = read numStr :: Double
    in TLiteral (NumberLit num) : tokenize rest

lexString :: String -> [Token]
lexString (quote:rest) =
    let (str, remaining) = break (== quote) rest
    in case remaining of
        [] -> error "Unterminated string literal"
        (_:rest') -> 
            TLiteral (StringLit str) : tokenize rest'
lexString [] = error "Expected string literal"

lexOperator :: String -> [Token]
lexOperator str = 
    let operators = [
            (">=", GreaterEquals),
            ("<=", LessEquals),
            ("<>", NotEquals),
            ("=", Equals),
            (">", GreaterThan),
            ("<", LessThan)
            ]
        matchOp = findOperator operators str
    in case matchOp of
        Just (op, rest) -> TOperator op : tokenize rest
        Nothing -> error $ "Invalid operator at: " ++ take 10 str

findOperator :: [(String, Operator)] -> String -> Maybe (Operator, String)
findOperator [] _ = Nothing
findOperator ((opStr, op):rest) input
    | opStr `isPrefixOf` input = Just (op, drop (length opStr) input)
    | otherwise = findOperator rest input

isIdentChar :: Char -> Bool
isIdentChar c = isAlpha c || isDigit c || c == '_'
