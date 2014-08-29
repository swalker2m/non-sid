{-# LANGUAGE PatternGuards, ScopedTypeVariables, NoMonomorphismRestriction,FlexibleInstances,  FlexibleContexts, RankNTypes, ScopedTypeVariables #-}

module Horizons where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Utils

-- Target Model ------------------------------

data Sign = Neg
          | Pos
          deriving (Eq, Ord, Read, Show, Enum)

signedDeg :: Sign -> Double -> Double
signedDeg Neg = negate
signedDeg Pos = id

segToDegrees :: Sign -> Int -> Int -> Double -> Double
segToDegrees sn x min sec = let sf = signedDeg sn
                                xx = fromIntegral x
                                mm = fromIntegral min
                            in  sf $ xx + mm / 60.0 + sec / 3600.0

  -- Right Ascension
data Hms = Hms { hmsSign  :: Sign
               , hmsHours :: Int
               , hmsMin   :: Int
               , hmsSec   :: Double}
  deriving (Show)

hmsToDegrees :: Hms -> Double
hmsToDegrees hms = let dg = 15.0 * segToDegrees (hmsSign hms)
                                                (hmsHours hms)
                                                (hmsMin hms)
                                                (hmsSec hms)
               in if (dg < 0) then 360.0 + dg else dg

  -- Declination
data Dms = Dms { dmsSign   :: Sign
               , dmsDeg    :: Int
               , dmsArcmin :: Int
               , dmsArcsec :: Double}
  deriving (Show)

dmsToDegrees :: Dms -> Double
dmsToDegrees dms = segToDegrees (dmsSign dms)
                                (dmsDeg dms)
                                (dmsArcmin dms)
                                (dmsArcsec dms)

data Coordinate = Coordinate { coordRa  :: Hms
                             , coordDec :: Dms }
  deriving (Show)


-- Parsing -----------------------------------

pSep :: Parser Char
pSep = pSym ':'

pSign :: Parser Sign
pSign = (Pos <$ (pSym '+')) <|> (Neg <$ (pSym '-')) `opt` Pos

pSexagesimal :: (Sign -> Int -> Int -> Double -> a) -> Parser a
pSexagesimal f = f <$> pSign <*> pNaturalRaw
                    <* pSep  <*> pNaturalRaw
                    <* pSep  <*> pDoubleRaw

pHms :: Parser Hms
pHms = pSexagesimal Hms

pDms :: Parser Dms
pDms = pSexagesimal Dms

pCoord :: Parser Coordinate
pCoord = Coordinate <$> pHms <* pSpaces <*> pDms

pAdd :: Parser Integer
pAdd = pChainl (pure (+)) pInteger
