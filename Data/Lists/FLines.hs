-- |
-- Module      :  Data.Lists.FLines
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Additional data and structures to some 'String'-related lists.
--

{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}

module Data.Lists.FLines where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import System.IO

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif


-- | Auxiliary printing function to define the line ending needed to be shown and printed. Is primarily defined in the @uniqueness-periods-vector-general@ package (https://hackage.haskell.org/package/uniqueness-periods-vector-general).
newLineEnding :: String
newLineEnding
  | nativeNewline == LF = "\n"
  | otherwise = "\r\n"

data FLines a = F1 a | FL [a] deriving Eq

type Flines = FLines String

-- | Is intended to be printed and so adds the new line ending.
instance Show (FLines String) where
  show (F1 xs) = mconcat [xs, newLineEnding]
  show (FL (xs:xss)) = mconcat (xs:newLineEnding:[show (FL xss)])
  show _ = newLineEnding

class GetN a b where
  getN :: Int -> a -> Maybe b

instance GetN (FLines String) String where
  getN n (F1 xs)
    | null xs = Nothing
    | n == 1 = Just xs
    | otherwise = Nothing
  getN n (FL (xs:xss))
   | n == 1 = if null xs then Nothing else Just xs
   | compare n 1 == LT = Nothing
   | otherwise = getN (n - 1) (FL xss)
  getN _ _ = Nothing

class GetNTrans a b where
  getNT :: Int -> (b -> b) -> a -> Maybe b

instance GetNTrans (FLines String) String where
  getNT n g (F1 xs)
    | null (g xs) = Nothing
    | n == 1  = Just (g xs)
    | otherwise = Nothing
  getNT n g (FL (xs:xss))
   | n == 1 = if null (g xs) then Nothing else Just (g xs)
   | compare n 1 == LT = Nothing
   | otherwise = getNT (n - 1) g (FL xss)
  getNT _ _ _ = Nothing

class GetTransL a b where
  getTL :: (b -> b) -> a -> b

