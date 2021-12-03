{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

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
type Vec = (Int,Int)

readIns :: String -> Ins
readIns w = let 
  [t,n] = words w
             in (case t of
                "forward" -> F
                "down" -> D
                "up" -> U
                  ) $ read n

insToVec :: Ins -> Vec
insToVec (F n) = (n,0)
insToVec (D n) = (0,n)
insToVec (U n) = (0,-n)

addVec :: Vec -> Vec -> Vec
addVec (a,b) (c,d) = (a+c,b+d)

main :: IO ()
main = do
    txt <- readFile "input"
    let inss = readIns <$> lines txt 
    print $ foldl addVec (0,0) (insToVec <$> inss)

