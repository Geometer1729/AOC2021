{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}

--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Maybe
import Data.Function
import Data.Functor

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

data Ins = F Int | D Int | U Int
type Vec = (Int,Int,Int)

readIns :: String -> Ins
readIns w = let 
  [t,n] = words w
             in (case t of
                "forward" -> F
                "down" -> D
                "up" -> U
                  ) $ read n

foldMe :: Vec -> Ins -> Vec
foldMe (h,aim,depth) = \case
  D n -> (h,aim+n,depth)
  U n -> (h,aim-n,depth)
  F n -> (h+n,aim,depth+aim*n)

main :: IO ()
main = do
    txt <- readFile "input"
    let inss = readIns <$> lines txt 
    let (h,_,d) = foldl foldMe (0,0,0) inss
    print $ h*d
