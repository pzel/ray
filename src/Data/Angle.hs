{-# LANGUAGE GeneralizedNewtypeDeriving, UnicodeSyntax #-}
module Data.Angle (
  Angle
 ,deg
 ,rightP
 ,toRadians
 ,upP
 ) where

type Angle = Float

deg :: Float -> Angle
deg x = (x `nonIntRem` 360) where
  nonIntRem x y = x - (y * (fromIntegral $ truncate (x/y)))

toRadians :: Angle -> Float
toRadians x = x/180*pi

rightP, upP :: Angle -> Bool
upP d = d >= 0.0 && 180.0 > d
rightP d = d <= 90.0 || 270.0 < d