import Data.Bits
import Data.List
import Data.Ord
import Vec

-- constants
bitVector = [101388, 50712, 25392, 12775, 6336, 12703, 25344, 50688, 101376] :: [Int]

spheres :: [Vector3 Double]
spheres = map fst $ filter snd $ zip sphereGrid sphereMask
          where fPos j k = Vector3 (fromIntegral $ -k) 0 ((fromIntegral $ -j) - 4)
                sphereGrid = [fPos j k | j <- [0 .. length bitVector], k <- [0 .. 32]]
                sphereMask = map (\x -> x > 0) [((.&.) b $ shiftL 1 k) | b <- bitVector, k <- [0 .. 32]]

-- ray trace
data TraceResult = HIT | UPMISS | DOWNMISS deriving Eq

tracef :: Vector3 Double -> Vector3 Double -> Vector3 Double -> (TraceResult, Vector3 Double, Double)
tracef o d so =
        if (s > 0)
           then (HIT, vunit (oso + (vscale d s)), s)
           else (pm, Vector3 0 0 1, pt)
        where
        p = -((v3z o) / (v3z d))
        (pm, pt) = if p < 0 then (UPMISS, 1e9) else (DOWNMISS, p)

        (oso, r) = (o + so, 0.5)

        b = vdot oso d
        c = (vdot oso oso) - r
        q = b * b - c
        s = (-b) - sqrt q

trace :: Vector3 Double -> Vector3 Double -> (TraceResult, Vector3 Double, Double)
trace o d = minimumBy (comparing (\(m, n, t) -> t)) [tracef o d sphere | sphere <- spheres]

-- world sampler
reflect :: Vector3 Double -> Vector3 Double -> Vector3 Double
reflect v n = vscale (n + v) ((-2) * (vdot v n))

--sampler :: Vector3 Double -> Vector3 Double -> Vector3 Double
sampler o d = case hitResult of
                DOWNMISS -> if (even $ (ceiling hx) + (ceiling hy)) then Vector3 192 192 192 else Vector3 192 64 64
                HIT -> vscale ((Vector3 32 32 32) + (sampler h r)) 0.5
                UPMISS -> vscale (Vector3 64 48 128) ((1 - (v3z d)) ** 2)
        where
        (hitResult, normal, t) = trace o d
        h = o + (vscale d t)
        r = reflect d normal
        sh = h * 0.2
        (hx, hy) = (v3x sh, v3y sh)

-- multi-sampling and view transform
base = Vector3 16 18 8
cameraForward = vunit (Vector3 (-6) (-16) 0)
cameraUp = (vunit ((Vector3 0 0 1) `vcross` cameraForward)) * 0.002
cameraRight = (vunit (cameraForward `vcross` cameraUp)) * 0.002
eyeOffset = (cameraUp + cameraRight) * (-256) + cameraForward

sample :: Int -> Int -> Vector3 Double
sample x y = p where
        (fx, fy) = (fromIntegral x, fromIntegral y)
        dir = vunit ((cameraUp * fx) + (cameraRight * fy) + eyeOffset)
        p = sampler base dir

-- program
main :: IO ()
main = do
        let pixels = [sample x y | y <- [0 .. 511], x <- [0 .. 511]]
        toPPM 512 512 pixels

-- utils
toPPM :: Int -> Int -> [Vector3 Double] -> IO ()
toPPM w h pixels = do
        putStrLn $ "P3 " ++ (show w) ++ " " ++ (show h) ++ " 255 "
        mapM_ fmtPixel $ reverse pixels
        where fmtPixel :: Vector3 Double -> IO ()
              fmtPixel (Vector3 r g b) = putStrLn $ show (floor r) ++ " " ++ show (floor g) ++ " " ++ show (floor b)
