{-# LANGUAGE GADTs #-}

module Main where

import System.Directory
import System.IO
import Data.Void
import Text.Megaparsec 
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexeme
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad
import Control.Monad.State
import Data.List
import qualified Text.Megaparsec.Byte.Lexer as L
import Text.Megaparsec.Debug
import Data.List.Duplicate
import Data.List.Split


data PyToken a =
  Identifier String
  | PyFloat Float
  | PyInt Int
  | True | False
  | And | Break | Def | Elif | Else | For | If | Not | Or | Return | While
  | Assign | Plus | Minus | Times | DividedBy | Eq | Neq | Gt | Gte | Lt | Lte
  | LParen | RParen | Comma | Colon
  | Newline | Indent | Dedent | Space | Comment
  deriving (Show, Eq)

type Parser = Parsec Void String
type IndentParser = StateT Int Parser

--[[
--   Function that chains an applicative n number of times, then collect the final last result.
--]]
repeatM :: Monad m => Int -> m a -> m a
repeatM n m = do 
  list <- replicateM n m
  return $ last list


lexBool :: Parser (PyToken a)
lexBool = choice [
                      Main.True <$ string "True",
                      Main.False <$ string "False"
                   ]

lexIdentifier :: Parser (PyToken a)
lexIdentifier = do
  val <- (:) <$> alphaNumChar <*> Text.Megaparsec.many (alphaNumChar <|> char '_')
  return $ Identifier val

-- This function parses signed floating point numbers as well, just for the hell of it.
lexFloat :: Parser (PyToken a)
lexFloat = do
  val <- (Lexeme.signed space Lexeme.float)
  return $ PyFloat val

lexInteger :: Parser (PyToken a)
lexInteger = do
  val <- Lexeme.signed space decimal
  return $ PyInt val

lexIdent :: Parser (PyToken a)
lexIdent = choice [
                      And <$ string "and",
                      Break <$ string "break",
                      Def <$ string "def",
                      Elif <$ string "elif",
                      Else <$ string "else",
                      For <$ string "for",
                      If <$ string "if",
                      Not <$ string "not",
                      Or <$ string "or",
                      Return <$ string "return",
                      While <$ string "while"
                    ]

lexOperator :: Parser (PyToken a)
lexOperator = choice [
                          Assign <$ char '=',
                          Plus <$ string "+",
                          Minus <$ string "-",
                          Times <$ string "*",
                          DividedBy <$ string "/",
                          Eq <$ string "==",
                          Neq <$ string "!=",
                          Main.Gt <$ string ">",
                          Gte <$ string ">=",
                          Lt <$ string "<",
                          Lte <$ string "<="
                       ]

lexPunctuation :: Parser (PyToken a)
lexPunctuation = choice [
                            LParen <$ char '(',
                            RParen <$ char ')',
                            Comma <$ char ',',
                            Colon <$ char ':',
                            Indent <$ repeatM 4 (char ' ' <|> tab)
                          ]

lexString :: Parser (PyToken a)
lexString = lexPunctuation <|> lexBool <|> try lexFloat <|> try lexInteger <|> lexOperator <|> lexIdent <|>  lexIdentifier
                   
whiteSpaceParser :: Parser (PyToken a)
whiteSpaceParser = choice [
                             Space <$ hspace1,
                             Comment <$ (char '#' >> manyTill anySingle (lookAhead newline))
                          ]

stateMap :: ((a, b) -> (a, b)) -> a -> [b] -> [b]
stateMap _ _ [] = []
stateMap f base (x:xs) = (snd nextBase) : stateMap f (fst nextBase) xs where nextBase = f (base, x)

prependN :: Int -> a -> [a] -> [a]
prependN num elem lst = if num <= 0 then lst else elem : (prependN (num - 1) elem lst)

lexLine :: Parser [PyToken a]
lexLine = many ((try lexString) <|> (try whiteSpaceParser))

cleanResults :: [[PyToken a]] -> [[PyToken a]]
cleanResults lst = filter (/= Space) <$> Prelude.filter (/= [Comment]) lst

cleanIndents :: [[PyToken a]] -> [[PyToken a]]
cleanIndents lst = let clearIndents xs = Prelude.filter (/= Indent) xs; numIndents xs = length $ Prelude.filter (==Indent) xs; absDiff a = abs . (-) a in  
                   stateMap (\(num, elem) -> (numIndents elem, case compare (numIndents elem) num of
                                                                  EQ -> clearIndents elem
                                                                  LT -> prependN ((numIndents elem) `absDiff` num) Dedent (clearIndents elem)
                                                                  GT -> prependN ((numIndents elem) `absDiff` num) Indent (clearIndents elem)
                                              )) 0 lst 
                                                           
                                                           

                                               
                                               
printResults :: String -> Maybe String
printResults contents = case parse (lexLine `Text.Megaparsec.sepBy` (some newline)) "" contents of
                 Left _ -> Nothing
                 Right results -> Just $ show $ (cleanIndents . cleanResults) results

main :: IO ()
main = do
  files <- listDirectory "testing_code"
  mapM_ (\f -> do
    handle <- openFile ("testing_code/" <> f) ReadMode
    contents <- hGetContents handle
    putStrLn $ "=== Lexing file " <> f <> " ===" -- a very simple parsing system that i cobbled together because we will probably just throw it out in assignment 2
    print $ printResults contents
    ) files
