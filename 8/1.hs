{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Arrow
import Control.Lens hiding ((.>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Data.Char
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Either
import Data.Function
import Data.Functor

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import Flow

type Line = ([[Int]],[[Int]])
type Perm = [Maybe Int]

cti :: Char -> Int
cti c = ord c - 97

refine :: Perm -> [Perm]
refine perm = do
  x <- Just <$> [0..6]
  guard $ x `notElem` perm
  y <- [0..6]
  guard $ isNothing $ perm !! y
  return $ [ if i == y then x else p | (i,p) <- zip [0..6] perm ]


nums :: [[Int]]
nums = map cti <$> ["abcefg","cf","acdeg","acdfg","bcdf","abdfg","abdefg","acf","abcdefg","abcdfg"]

validate :: Perm -> [Int] -> Bool
validate perm word = any (validateFor perm word) nums

validateFor :: Perm -> [Int] -> [Int] -> Bool
validateFor perm word target = length word == length target && and ( do
  i <- [0..6]
  return $ case perm !! i of
    Just x -> ( i `elem` word && x `elem` target ) || (i `notElem` word && x `notElem` target)
    Nothing -> True
                                                               )

findPerm :: [[Int]] -> Perm
findPerm ws = findPerm' ws (replicate 7 Nothing)

findPerm' :: [[Int]] -> Perm -> Perm
findPerm' ws perm = if all isJust perm 
                          then perm
                          else head $ do
                            perm' <- refine perm
                            guard $ all (validate perm') ws 
                            return $ findPerm' ws perm'


lookupPerm :: Perm -> [Int] -> Int
lookupPerm perm word = let
  word' = sort [ x | i <- word , let Just x = perm !! i ]
    in head [ i | i <- [0..9] , nums !! i == word' ]


solveLine :: Line -> [Int]
solveLine (ws,outws) = let
  perm = findPerm ws
    in map (lookupPerm perm) outws

parseLine :: String -> Line
parseLine w = let
  [ws',ows'] = splitOn "|" w
  ws = map cti <$> words ws'
  ows = map cti <$> words ows'
    in (ws,ows)

runLine :: String -> [Int]
runLine = parseLine .> solveLine

main :: IO ()
main = do
    txt <- readFile "input"
    let sols = runLine <$> lines txt
    print $ length $ filter (`elem` [1,4,7,8]) (concat sols)
