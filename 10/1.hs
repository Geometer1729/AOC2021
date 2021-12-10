{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Arrow
import Control.Lens hiding ((.>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

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

chunkValidate :: String -> Either Char String
chunkValidate w = execStateT (mapM scanChar w) ""

scanChar :: Char -> StateT String (Either Char) ()
scanChar c = case closes c of
               Just c' -> modify (c':)
               Nothing -> do
                      stack <- get
                      case stack of
                        (sc:scs)
                          | sc == c -> put scs
                        _ -> lift $ Left c

closes :: Char -> Maybe Char
closes = \case
  '(' -> Just ')'
  '[' -> Just ']'
  '{' -> Just '}'
  '<' -> Just '>'
  _ -> Nothing

opens :: Char -> Maybe Char
opens = \case
  ')' -> Just '('  
  ']' -> Just '[' 
  '}' -> Just '{' 
  '>' -> Just '<' 
  _ -> Nothing

score :: Char -> Int
score = \case
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137 
  _ -> error "ree"

scoreLine :: String -> Maybe Int
scoreLine w = case chunkValidate w of
                Left c -> Just $ score c
                Right _ -> Nothing

main :: IO ()
main = do
    txt <- readFile "input"
    print $ sum . catMaybes $ (scoreLine <$> lines txt)
