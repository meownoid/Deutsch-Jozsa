module FunctionParser (
    tryMakeFunction
) where

import Utils
import Data.List (elemIndex)
import Data.Char (isAlpha, isAlphaNum)

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Expression = Bit Int
                | Varible Int
                | VaribleS String
                | And Expression Expression
                | Or Expression Expression
                | Xor Expression Expression
                | Not Expression deriving (Show)

evalFunction :: Expression -> [Int] -> Int
evalFunction (Bit n) _ = n
evalFunction (Varible n) xs = xs !! n
evalFunction (And expl expr) xs = andi (evalFunction expl xs) (evalFunction expr xs)
evalFunction (Or expl expr) xs = ori (evalFunction expl xs) (evalFunction expr xs)
evalFunction (Xor expl expr) xs = xori (evalFunction expl xs) (evalFunction expr xs)
evalFunction (Not exp) xs = noti (evalFunction exp xs)

lexer = Token.makeTokenParser $ emptyDef {
    Token.reservedOpNames = ["!", "^", "&", "|"],
    Token.reservedNames   = [ "1", "0"],
    Token.identStart      = letter,
    Token.identLetter     = alphaNum
}

identifier = Token.identifier lexer
operator   = Token.reservedOp lexer
parens     = Token.parens     lexer
whiteSpace = Token.whiteSpace lexer
reserved   = Token.reserved   lexer

table = [[
             Prefix (operator "!" >> return Not)
        ],[
             Infix  (operator "^" >> return Xor) AssocLeft,
             Infix  (operator "&" >> return And) AssocLeft,
             Infix  (operator "|" >> return Or) AssocLeft
        ]]

term = whiteSpace >> (parens expressionParser
      <|> (reserved "0"  >> return (Bit 0))
      <|> (reserved "1"  >> return (Bit 1))
      <|> liftM VaribleS identifier)

expressionParser :: Parser Expression
expressionParser = buildExpressionParser table term

preprocess :: String -> Either String (String, [String], String)
preprocess str
    | null str  = Left "Empty string"
    | null left = Left "Expecting function declaration"
    | not . validWord $ name = Left "Incorrect function name"
    | null args  = Left "Function must have at least one argument"
    | any (not . validWord) args = Left "Incorrect argument name"
    | null right = Left "Expecting '='"
    | otherwise = Right (name, args, right)
    where (left : right : _) = splitBy '=' str ++ [[]]
          (name : args) = if null left then [[], []] else words left
          validWord xs = (isAlpha . head $ xs) && (all isAlphaNum $ tail xs)

postprocess :: [String] -> Expression -> Either String Expression
postprocess args (VaribleS var) = case elemIndex var args of
    Nothing -> Left $ "Unknown varible " ++ var
    (Just n) -> Right $ Varible n
postprocess args b@(Bit _) = Right b
postprocess args (Not e) = Not <$> (postprocess args e)
postprocess args (And el er) = case (postprocess args el) of
    Left e -> Left e
    Right exprl -> And exprl <$> postprocess args er
postprocess args (Or el er) = case (postprocess args el) of
    Left e -> Left e
    Right exprl -> Or exprl <$> postprocess args er
postprocess args (Xor el er) = case (postprocess args el) of
    Left e -> Left e
    Right exprl -> Xor exprl <$> postprocess args er

parseFunction :: String -> Either String (String, Int, Expression)
parseFunction str = case preprocess str of
    Left e -> Left e
    Right (name, args, expr) -> case parse expressionParser "function" expr of
        Left e -> Left $ show e
        Right pexpr -> (\p -> (name, length args, p)) <$> postprocess args pexpr

tryMakeFunction :: String -> Either String (String, Int, [Int] -> Int)
tryMakeFunction = fmap (\(a, n, f) -> (a, n, evalFunction f)) . parseFunction
