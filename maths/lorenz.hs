#!/usr/bin/env cabal
{- cabal:
default-language: GHC2021
ghc-options:
  -Wall -Wno-missing-signatures
  -O2
  -threaded
  -with-rtsopts=-N
build-depends:
  base,
  parallel
-}
{- project:
index-state: 2023-04-17T00:00:00Z
-}

module Main where

import           Control.Monad               (when)
import           Control.Parallel.Strategies (parBuffer, rdeepseq, using)
import           Data.List                   (foldl', intercalate)
import           Numeric                     (showFFloat)

--------------------------------------------------------------------------------
-- Fixed 18 Decimal Type
--------------------------------------------------------------------------------
newtype Fixed18D = Fixed18D Integer

n18D = 1000000000000000000

toFixed18D :: Double -> Fixed18D
toFixed18D a = Fixed18D (floor (a * 1e18))

instance Show Fixed18D where show (Fixed18D a) = show a
instance Num Fixed18D where
  fromInteger a = Fixed18D $ fromInteger a
  (Fixed18D a) + (Fixed18D b) = Fixed18D (a + b)
  (Fixed18D a) - (Fixed18D b) = Fixed18D (a - b)
  (Fixed18D a) * (Fixed18D b) = Fixed18D (a * b `quot` n18D)
  signum (Fixed18D a) = Fixed18D (signum a)
  abs (Fixed18D a) = Fixed18D (abs a)

--------------------------------------------------------------------------------
-- Lorenz Attractor Function
--------------------------------------------------------------------------------

getX (x, _, _) = x; getY (_, y, _) = y; getZ (_, _, z) = z

lorenz (sigma, rho, beta) dt (x, y, z) =
  ( x + dt * (sigma * (y - x))
  , y + dt * (x * (rho - z) - y)
  , z + dt * (x * y - beta * z)
  )

countFlips f ps = fst $
  foldl' (\(n, s) -> \p ->
             let s' = signum (f p)
             in (if s' == s then n else n + 1, s'))
  (0, 0) ps

average xs = (sum xs) / fromIntegral(length xs)

showFullFloat :: Double -> ShowS
showFullFloat = Numeric.showFFloat Nothing

defFloatParms :: (Double, Double, Double)
defFloatParms = (10.0, 28, 8.0 / 3.0)
defFixedParams :: (Fixed18D, Fixed18D, Fixed18D)
defFixedParams = let (a,b,c) = defFloatParms
                 in (toFixed18D a, toFixed18D b, toFixed18D c)

-- test set of: x-axis positions
testset_xs = (map (defFloatParms,) . map (,1.0,1.0) . map (/1000.0) $ [0..1000], getX)

-- gnuplot -p -e "plot '-'"
gnuplot2d_rs = mapM
  (\(x, v) -> putStrLn (showFullFloat x . ("\t" ++) . showFullFloat v $ ""))

-- gnuplot -p -e "splot '-'"
gnuplot3d_ps = mapM
  (\(x, y, z) -> putStrLn (show x ++ "\t" ++ show y ++ "\t" ++ show z))

gen_snarkjs_input (sigma, rho, beta) dt ps = do
  putStrLn "{"
  putStrLn $ "  \"sigma\": " ++ toJsonString sigma ++ ","
  putStrLn $ "  \"rho\": "   ++ toJsonString rho ++ ","
  putStrLn $ "  \"beta\":"   ++ toJsonString beta ++ ","
  putStrLn $ "  \"dt\":"     ++ toJsonString dt ++ ","
  putStrLn $ intercalate ",\n" $
    map (\(t, f) -> "  \"" ++ t ++ "\": ["
          ++ intercalate "," (map (toJsonString . f) ps)
          ++ "]")
    [ ("x",  getX)
    , ("y",  getY)
    , ("z",  getZ)
    ]
  putStrLn "}"
  where
    toJsonString a = "\"" ++ show a ++ "\""

run_testset dt n ts fx fy fo =
  fo (rs `using` parBuffer 100 rdeepseq) -- rs <-- if without using parallel, then slow
  where pss = map (\(params, p0) -> (p0, take n $ iterate (lorenz params dt) p0)) ts
        rs = map (\(p0, ps) -> (fx p0, fy ps)) pss

main = do
  let c = 2 :: Integer -- program mode
  when (c == 0) $ do -- process 2d testsets
    let dt = 0.01
    let n = 10000000
    let (ts, fx) = testset_xs
    let fy = countFlips getY        -- count y-axis flips
    -- let fy = average . map getY  -- average y-values
    let fo = gnuplot2d_rs
    _ <- run_testset dt n ts fx fy fo
    return ()
  when (c == 1) $ do -- process 3d points
    let params = defFloatParms
    let dt = 0.01
    let n = 10000
    let p0 = (-1, 1, 1)
    let ps = take n $ iterate (lorenz params dt) p0
    _ <- gnuplot3d_ps (ps `using` parBuffer 100 rdeepseq)
    return ()
  when (c == 2) $ do -- generate snarkjs input using
    let params = defFixedParams
    let dt = toFixed18D 0.001
    let n = 1000
    let p0 = (toFixed18D 1, toFixed18D 1, toFixed18D 1)
    let ps = take n $ iterate (lorenz params dt) p0
    _ <- gen_snarkjs_input params dt ps
    return ()
