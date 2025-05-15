{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tiling (hello) where

import Codec.Picture.Types
import Control.Applicative
import Data.Coerce (coerce)
import Data.Functor.Compose
import Data.List (transpose)
import Data.Word (Word8)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

hello :: String
hello = "Hello from Tiling"

data Tile a where
  Cw :: Tile a -> Tile a
  FlipH :: Tile a -> Tile a
  Above :: Tile a -> Tile a -> Tile a
  -- NOTE: the following will allowo us to build tiles via fmap, pure and <*>
  -- exactly like the Free Monad/Applicative approach does
  Pure :: a -> Tile a
  Ap :: Tile (b -> a) -> Tile b -> Tile a

-- NOTE: not needed, implemented with `cw`
-- Ccw :: Tile a -> Tile a

-- NOTE: not needed, implemented with `flipH`
-- FlipV :: Tile a -> Tile a

-- NOTE: not needed, implemented in terms of `above` and `cw` and `ccw`
-- Beside :: Tile a -> Tile a -> Tile a

-- NOTE: not needed, implemented in terms of `above` and `beside`
-- Quad :: Tile a -> Tile a -> Tile a -> Tile a -> Tile a

-- NOTE: not needed, implemented in terms of the Tile Monoid instance
-- Behind :: Monoid a => Tile a -> Tile a -> Tile a

-- NOTE: not needed, implemented in terms of `quad` and `cw`
-- Swirl :: Tile a -> Tile a

-- NOTE: not needed, implemented with `mempty` of `a` and `pure`
-- Empty :: Monoid a => Tile a

-- NOTE: not needed because can be represented with Ap and Pure, see Functor instance
-- Fmap :: (a -> b) -> Tile a -> Tile b

cw :: Tile a -> Tile a
cw (Cw (Cw (Cw t))) = t -- encodes the law `cw . cw . cw . cw = id`
cw t = Cw t

ccw :: Tile a -> Tile a
ccw (Cw t) = t -- encodes the law `ccw . cw = id`
ccw t = cw . cw . cw $ t -- encodes the law `ccw = cw . cw . cw`

flipH :: Tile a -> Tile a
flipH (FlipH t) = t
flipH t = FlipH t

flipV :: Tile a -> Tile a
flipV = ccw . flipH . cw

quad :: Tile a -> Tile a -> Tile a -> Tile a -> Tile a
quad t1 t2 t3 t4 = (t1 `beside` t2) `above` (t3 `beside` t4)

swirl :: Tile a -> Tile a
swirl t = quad t (cw t) (cw . cw $ t) (cw . cw . cw $ t)

beside :: Tile a -> Tile a -> Tile a
beside t1 t2 = ccw $ cw t1 `above` cw t2

above :: Tile a -> Tile a -> Tile a
above = Above

empty :: (Monoid a) => Tile a
empty = pure mempty

behind :: (Monoid a) => Tile a -> Tile a -> Tile a
-- behind t1 t2 = t2 <> t1
behind = flip (<>)

-- NOTE: fmap, pure and <*> are given by implementing the type classes below

-- Instances

instance (Semigroup a) => Semigroup (Tile a) where
  (<>) :: (Semigroup a) => Tile a -> Tile a -> Tile a
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (Tile a) where
  mempty :: (Monoid a) => Tile a
  mempty = pure mempty

instance Applicative Tile where
  pure :: (Applicative Tile) => a -> Tile a
  pure = Pure

  (<*>) :: Tile (a -> b) -> Tile a -> Tile b
  (<*>) = Ap

instance Functor Tile where
  fmap :: (Functor Tile) => (a -> b) -> Tile a -> Tile b
  fmap f = (pure f <*>)

instance (Show a) => Show (Tile a) where
  show :: (Show a) => Tile a -> String
  show (Cw t) = "cw (" ++ show t ++ ")"
  show (FlipH t) = "flipH (" ++ show t ++ ")"
  show (Above t1 t2) = "above (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (Pure a) = "pure (" ++ show a ++ ")"
  -- NOTE: we could have shown the second argument which is a `Tile b` but we
  -- don't have constraints on `b` which could be not showable
  show (Ap _ _) = "ap _ _"

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Tile a) where
  arbitrary :: (Arbitrary a) => Gen (Tile a)
  arbitrary = sized $ \n ->
    if n <= 1
      then pure <$> arbitrary
      else
        frequency
          [ (3,) $ pure <$> arbitrary,
            (9,) $ beside <$> decay 2 <*> decay 2,
            (9,) $ above <$> decay 2 <*> decay 2,
            (2,) $ cw <$> arbitrary,
            (2,) $ ccw <$> arbitrary,
            (4,) $ flipH <$> arbitrary,
            (4,) $ flipV <$> arbitrary,
            (6,) $ swirl <$> decay 4,
            (3,) $ quad <$> decay 4 <*> decay 4 <*> decay 4 <*> decay 4
            -- TODO: unable to make it compile
            -- , (2,) $ (<*>) <$> (decay 2) @(Tile (a -> a)) <*> decay 2
            -- , (2,) $ (<*>) <$> (scale (`div` 2) arbitrary @(Tile a)) <*> decay 2
          ]
    where
      decay :: (Arbitrary a) => Int -> Gen a
      decay n = scale (`div` n) arbitrary

  shrink (Cw t) = t : (cw <$> shrink t)
  shrink (FlipH t) = t : (flipH <$> shrink t)
  shrink (Above t1 t2) = t1 : t2 : (above <$> shrink t1 <*> shrink t2)
  shrink (Pure a) = pure <$> shrink a
  shrink (Ap _ _) = []

-- Observation

rasterize :: Int -> Int -> Tile a -> [[a]]
rasterize w h (FlipH t) = reverse <$> rasterize w h t
rasterize w h (Cw t) = rotate2d $ rasterize w h t
  where
    rotate2d :: [[a]] -> [[a]]
    rotate2d = fmap reverse . transpose
rasterize w h (Above t1 t2) = rasterize w (div h 2) t1 <> rasterize w (h - div h 2) t2
rasterize w h (Pure a) = replicate h $ replicate w a
rasterize w h (Ap f t) = coerce $ rasterize' w h f <*> rasterize' w h t

rasterize' :: Int -> Int -> Tile a -> Compose ZipList ZipList a
rasterize' w h t = coerce $ rasterize w h t

-- Color, our `a` or our `pixel`

type Color = PixelRGBA8

instance Semigroup Color where
  (<>) = _over

instance Monoid Color where
  mempty = rgba 0 0 0 0

color :: Word8 -> Word8 -> Word8 -> Word8 -> Tile Color
color r g b a = pure $ rgba r g b a

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Color
rgba = PixelRGBA8

invert :: Color -> Color
invert (PixelRGBA8 r g b a) = PixelRGBA8 (1 - r) (1 - g) (1 - b) a

_over :: Color -> Color -> Color
_over (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  let aa = norm a1
      ab = norm a2
      a' = aa + ab * (1 - aa)
      norm :: Word8 -> Double
      norm x = fromIntegral x / 255
      unnorm :: Double -> Word8
      unnorm x = round $ x * 255
      f :: Word8 -> Word8 -> Word8
      f a b = unnorm $ (norm a * aa + norm b * ab * (1 - aa)) / a'
   in PixelRGBA8 (f r1 r2) (f g1 g2) (f b1 b2) (unnorm a')

instance CoArbitrary PixelRGBA8 where
  coarbitrary (PixelRGBA8 r g b a) = coarbitrary (r, g, b, a)

instance Arbitrary PixelRGBA8 where
  arbitrary = do
    a <- choose (0, 255)
    case a == 0 of
      True -> pure mempty
      False -> PixelRGBA8 <$> choose (0, 255) <*> choose (0, 255) <*> choose (0, 255) <*> pure a

-- instance Observe
