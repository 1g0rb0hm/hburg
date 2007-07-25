-----------------------------------------------------------------------------
-- |
-- Module      :  Util
-- Copyright   :  Copyright (c) 2007 Igor Böhm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Böhm  <igor@bytelabs.org>
--
--
-- General utility functions.
--
--
-----------------------------------------------------------------------------

module Util (
		-- Functions
		stringToInt,
		stringToUpper,
		stringFoldr,
	) where

import Data.Char

------------------------------------------------------------------------------------

stToInt	:: Int -> String -> Int
stToInt base digits
    = sign * (foldl acc 0 $ concatMap digToInt digits1)
      where
      splitSign ('-' : ds) = ((-1), ds)
      splitSign ('+' : ds) = ( 1  , ds)
      splitSign ds         = ( 1  , ds)
      (sign, digits1)      = splitSign digits
      digToInt c  | c >= '0' && c <= '9'
						= [ord c - ord '0']
				  | c >= 'A' && c <= 'Z'
						=  [ord c - ord 'A' + 10]
				  | c >= 'a' && c <= 'z'
						=  [ord c - ord 'a' + 10]
				  | otherwise = []
      acc i1 i0 = i1 * base + i0

stringToInt :: String -> Int
stringToInt str = stToInt 10 str

stringToUpper :: String -> String
stringToUpper str = map (\char -> toUpper char) (str)

-- | Since we use stringFolds quite a bit and do not want to
--	explitcitely check for an empty list before using foldr1,
--	we provide this function which returns the empty string
--	if the list to fold is empty, otherwise it returns the
--	desired string listed folded into one string.
stringFoldr :: (String -> String -> String) -> [String] -> String
stringFoldr f [] = ""
stringFoldr f xs = foldr1 f xs

