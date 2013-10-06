import Data.Bits
import Data.List
import Data.Ord
import Vec

-- constants
myBits = [38801, 38033, 38033, 42129, 58513, 38033, 38037, 38043, 59281]

-- uh, TODO
randR :: Fractional a => a
randR = 0.5

-- ray trace
data TraceResult = HIT | UPMISS | DOWNMISS deriving Eq

tracef :: Vector3 Double -> Vector3 Double -> Int -> Int -> Int -> (TraceResult, Vector3 Double, Double)
tracef o d mask j k = if (masked && (q > 0) && (s > 0.01) && (s < pt))
        then (HIT, vunit (vp + (vscale d s)), s)
        else (pm, Vector3 0 0 1, pt)
        where
          vp = o + (Vector3 (fromIntegral $ -k) 0 ((fromIntegral $ -j) - 4))
          b = vdot vp d
          c = (vdot vp vp) - 1
          q = b * b - c
          s = (-b) - sqrt q
          masked = ((.&.) mask $ shiftL 1 k) > 0
          p = -((v3z o) / (v3z d))
          pm = if 0.01 < p then DOWNMISS else UPMISS
          pt = if 0.01 < p then p else 1e9

trace :: Vector3 Double -> Vector3 Double -> (TraceResult, Vector3 Double, Double)
trace o d = minimumBy (comparing (\(m, n, t) -> t)) [tracef o d mask j k | (mask, j) <- zip myBits [0 .. 8], k <- [0 .. 18]]

-- world sampler
sampler :: Vector3 Double -> Vector3 Double -> Vector3 Double
sampler o d = if (m == UPMISS)
                then vscale (Vector3 0.7 0.6 1) ((1 - (v3z d)) ** 4)
                else
                  if (m == DOWNMISS)
                     then
                       if (even (toInteger ((ceiling $ v3x h2) + (ceiling $ v3y h2))))
                          then Vector3 3 1 1
                          else vscale (Vector3 3 3 3) (b2 * 0.2 + 0.1)
                     else (Vector3 p p p) + (sampler h r) * 0.5
        where
          (m, n, t) = trace o d
          h = vscale (o + d) t
          l = vunit $ (Vector3 (9 + randR) (9 + randR) 16) + (-h)
          nd = vdot n d
          r = vscale (d + n) (nd * (-2))
          b = vdot l n
          (m2, _, _) = trace h l
          b2 = if (b < 0 || m2 /= UPMISS) then 0 else b
          h2 = h * 0.2
          p = if b > 0 then (vdot l r) ** 99 else 0

-- multi-sampling and view transform
cameraForward = vunit (Vector3 (-6) (-16) 0)
cameraUp = (vunit ((Vector3 0 0 1) `vcross` cameraForward)) * 0.002
cameraRight = (vunit (cameraForward `vcross` cameraUp)) * 0.002
eyeOffset = (cameraUp + cameraRight) * (-256) + cameraForward

sample :: Int -> Int -> Int -> Vector3 Double
sample x y r = p * 3.5 where
        base = Vector3 16 18 8
        rr = (fromIntegral r) / 64 -- FIXME: Uniform sampling instead of Monte Carlo
        t = (cameraUp * (rr - 0.5)) * 99 + (cameraRight * (rr - 0.5)) * 99
        rx = (rr + fromIntegral x)
        ry = (rr + fromIntegral y)
        dir = vunit (-t + ((cameraUp * rx) + (cameraRight * ry) + eyeOffset) * 16)
        p = sampler (base + t) dir

sample64 :: Int -> Int -> Vector3 Double
sample64 x y = foldl (+) (Vector3 13 13 13) [sample x y r | r <- [0 .. 63]]

-- program
main :: IO ()
main = do
        let pixels = [sample64 (511 - x) (511 - y) | y <- [0 .. 511], x <- [0 .. 511]]
        toPPM pixels

-- utils
toPPM :: [Vector3 Double] -> IO ()
toPPM pixels = do
        putStrLn "P3 512 512 255 " -- FIXME
        mapM_ fmtPixel pixels
        where fmtPixel :: Vector3 Double -> IO ()
              fmtPixel (Vector3 r g b) = putStrLn (show (floor r) ++ " " ++ show (floor g) ++ " " ++ show (floor b))
