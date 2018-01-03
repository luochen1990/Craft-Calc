{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, UndecidableInstances, FlexibleContexts  #-}

import Prelude hiding (getLine)
import Control.Monad
import Control.Arrow ((&&&))
import Control.Applicative ((<$), empty, (<*), (*>), liftA2)
import Data.Char (isSpace, isLetter, isAlphaNum)
import Data.List hiding (lines)
import Data.Monoid
import Text.Parsec --hiding (many)
import Text.Parsec.String (Parser)

import qualified Data.ByteString as S
import qualified Data.Text.Encoding as T
import Data.Text (unpack)

import System.IO hiding (getLine)
import Numeric
import Data.Either (rights)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Map.Strict as M
import GHC.Exts
import Debug.Trace

readTextFile path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    s <- hGetContents h
    return s

data Name = Name {getName :: String} deriving (Eq, Ord, Show)

data Rule = DefExpr Name Expr
          | DefSet Name Set
          | DefAlias Name Name
          deriving (Eq, Show)

data Expr = Null
          | Item Name
          | Mul Expr Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mod Expr Set
          | Div Expr Set --TODO: not supported by evalNF yet
          | Del Expr Set --TODO: not supported by evalNF yet
          deriving (Eq, Show)

data Set = Set [Name] deriving (Eq, Show)

wrap p = try (spaces' *> p <* spaces') where
    spaces' = many space' <?> "spaces"
    space' = satisfy (\c -> isSpace c && c /= '\n' && c /= '\r') <?> "space"

name = fmap Name (liftA2 (:) letter' (many alphaNum')) where
    special_chars = "、，（）(),"
    letter' = satisfy (\c -> isLetter c && not (c `elem` special_chars))
    alphaNum' = satisfy (\c -> isAlphaNum c && not (c `elem` special_chars))

number = (fmap (read :: String -> Int)) (many1 digit)

expr = exprItem <?> "expr" where
    --exprItem = chainl1 exprTerm (Add <$ wrap (char '+') <|> Sub <$ wrap (char '-'))
    -- "a + b - c"
    exprItem = chainl1 exprTerm (wrap (Add <$ (char '+') <|> Sub <$ (char '-'))) <?> "exprItem"

    exprTerm = try (do
        a <- exprPrim
        f <- Mul <$ wrap (char '*')
        b <- number
        return (f a b)
        )
        <|> try (do
        a <- exprPrim
        f <- Del <$ wrap (string "//")
        b <- set
        return (f a b)
        )
        <|> try (do
        a <- exprPrim
        f <- Div <$ wrap (char '/')
        b <- set
        return (f a b)
        )
        <|> try (do
        a <- exprPrim
        f <- Mod <$ wrap (char '%')
        b <- set
        return (f a b)
        )
        <|> exprPrim

    exprPrim = (char '(' <|> char '（') *> wrap expr <* (char ')' <|> char '）')
         <|> fmap Item name

set = fmap Set (name `sepBy` wrap ((char '、') <|> (char '，') <|> (char ',') <?> "spliter"))
--set = fmap Set ((name <* spaces') `sepBy` (char ',' <* spaces'))

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (expr <* eof) "syntax error"


rule = try (do
    a <- name
    wrap (char '=')
    e <- expr
    return (DefExpr a e)
    )
    <|> try (do
    a <- name
    wrap ((char ':') <|> (char '：'))
    s <- set
    return (DefSet a s)
    )
    <|> try (do
    a <- name
    wrap ((string "即") <|> (string "as"))
    b <- name
    return (DefAlias a b)
    )
    <?> "rule"

--newlineSeperated p =

--rules = (rule `sepBy` wrap ((string "&") <|> many1 newline)) <* many1 newline

parseRule :: String -> Either ParseError Rule
parseRule = parse (rule <* eof) "syntax error"

parseRules :: String -> [Rule]
parseRules = rights . map parseRule . lines


test1 = mapM_ putStrLn [
    (show (parseRules "a=b\nc=d")),
    (show (parseRules "a=b+c&d=e+f")),
    (show (parseRules "a=b+c\nd=e+f")),
    (show (parseRules "a:b ,c\nd：e , f")),
    (show (parseRules "a:b ， c\nd：e 、 f")),
    (show (parse name "" "hello, world")),
    (show (parseRule "a:b,c")),
    (show (parse set "" "b,c")),
    (show (parse set "" "b , c"))
    ]

test2 = mapM_ putStrLn [
    (show (parseExpr "a")),
    (show (parseExpr "a + b")),
    (show (parseExpr "a / c")),
    (show (parseExpr "a // c")),
    (show (parseExpr "(a + b) / c")),
    (show (parseExpr "(a + b) % c")),
    (show (parseExpr "a % c"))
    ]

classifyRules :: [Rule] -> ([Rule], [Rule], [Rule])
classifyRules rules = iter rules [] [] [] where
    iter [] xs ys zs = (xs, ys, zs)
    iter (r:rs) xs ys zs = case r of
        DefExpr _ _ -> iter rs (r:xs) ys zs
        DefSet _ _ -> iter rs xs (r:ys) zs
        DefAlias _ _ -> iter rs xs ys (r:zs)

getAliasRootVia aliasRules = let
    mp = M.fromList . map (\(DefAlias a b) -> (a, b)) $ aliasRules
    getRoot x = case M.lookup x mp of
        Just y -> getRoot y
        Nothing -> x
    in getRoot

getFlatSetVia (getAliasRoot, setRules) = let
    mp = M.fromList . map (\(DefSet a (Set s)) -> (getAliasRoot a, map getAliasRoot s)) $ setRules
    getSet x = case M.lookup (getAliasRoot x) mp of
        Just s -> concatMap getSet s
        Nothing -> [x]
    in getSet

evalVia rules = let
    (exprRules, setRules, aliasRules) = classifyRules rules
    getAliasRoot = getAliasRootVia aliasRules
    getFlatSet = getFlatSetVia (getAliasRoot, setRules)
    mp = M.fromList . map (\(DefExpr a e) -> (getAliasRoot a, e)) $ exprRules
    evalNF Null = M.empty
    evalNF (Item x) = M.singleton (getAliasRoot x) 1
    evalNF (Mul x n) = M.map (* n) (evalNF x)
    evalNF (Add x y) = M.unionWith (+) (evalNF x) (evalNF y)
    evalNF (Sub x y) = M.unionWith (+) (evalNF x) (M.map negate (evalNF y))
    evalNF (Mod x (Set s)) = M.unionWith (+) both only1_expanded where
        nf = evalNF x
        st = M.fromList . map (\x -> (x, ())) $ concatMap getFlatSet s

        expand (k, v) | isJust (M.lookup k st) = M.singleton k v
        expand (k, v) = case (M.lookup k mp) of
            Just e -> M.map (* v) $ evalNF (Mod e (Set s))
            Nothing -> M.singleton k v

        merge k a b = Just a

        both = M.intersection nf st
        only1 = M.difference nf st
        only1_expanded = foldr (M.unionWith (+)) M.empty $ map expand $ M.toList only1

    exprFromNF nf = foldr Add Null $ items where
        items = mapMaybe (\(k, v) -> if v > 0 then Just (if v == 1 then Item k else Mul (Item k) v) else Nothing) $ M.toList nf

    showNF nf = intercalate " + " items where
        items = mapMaybe (\(k, v) -> if v > 0 then Just (if v == 1 then getName k else getName k ++ " * " ++ show v) else Nothing) $ sortWith (negate . snd) $ M.toList nf

    eval :: Expr -> String
    eval = showNF . evalNF
    in eval

test3 = do
    rule_desc <- readTextFile "rules.txt"
    --mapM_ putStrLn (lines rule_desc)
    --mapM_ print $ map (id &&& parseRule) $ lines rule_desc
    let rules = parseRules rule_desc
    let (exprRules, setRules, aliasRules) = classifyRules rules
    let getAliasRoot = getAliasRootVia aliasRules
    let getFlatSet = getFlatSetVia (getAliasRoot, setRules)
    putStrLn $ (getName $ getAliasRoot (Name "材料1"))
    putStrLn $ intercalate ", " $ map getName $ getFlatSet (Name "材料2")

getLine :: IO String
getLine = fmap unpack $ fmap T.decodeUtf8 S.getLine

main = do
    rule_desc <- readTextFile "rules.txt"
    let rules = parseRules rule_desc

    let eval = evalVia rules
    let parseEval = fmap eval . parseExpr

    let repl s = case parseEval s of Right r -> putStrLn ("= " ++ r ++ "\n"); Left e -> print e

    inputs <- fmap lines $ readTextFile "input.txt"
    forM_ inputs $ \line -> do
        putStrLn line
        repl line

    --repl "太阳能发电机 % 基础材料"
    --forever $ getLine >>= repl --TODO: there is encoding problem here

