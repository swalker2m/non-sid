module NonSid where

type Magnitude = Maybe Double

data Coordinates = Coordinates { ra :: Double, dec :: Double }

data EphemerisElement = EphemerisElement Coordinates Magnitude

data NonSiderealTarget = NonSiderealTarget {
                             name      :: String
                           , ephemeris :: [EphemerisElement]
                         }
