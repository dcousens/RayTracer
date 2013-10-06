module Vec where

data Vector3 a = Vector3 { v3x, v3y, v3z :: !a } deriving (Eq, Show)

vfold :: (b -> t -> b) -> b -> Vector3 t -> b
vfold f i (Vector3 x y z) = ((i `f` x) `f` y) `f` z

vzip :: (t -> b -> a) -> Vector3 t -> Vector3 b -> Vector3 a
vzip f (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = Vector3 (f x0 x1) (f y0 y1) (f z0 z1)

instance Fractional a => Fractional (Vector3 a) where
        (/) = vzip (/)
        fromRational s = Vector3 (fromRational s) (fromRational s) (fromRational s)

instance Functor Vector3 where
        fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

instance Num a => Num (Vector3 a) where
        (+) = vzip (+)
        (-) = vzip (-)
        (*) = vzip (*)
        abs = fmap abs
        negate = fmap negate
        signum = fmap signum
        fromInteger s = Vector3 (fromInteger s) (fromInteger s) (fromInteger s)

fromFloat :: (Fractional a, Real b) => b -> a
fromFloat x = fromRational $ toRational x

vcross :: Num a => Vector3 a -> Vector3 a -> Vector3 a
vcross (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = Vector3 (y0 * z1 - z0 * y1) (z0 * x1 - x0 * z1) (x0 * y1 - y0 * x1)

vdot :: Num t => Vector3 t -> Vector3 t -> t
vdot v0 v1 = vfold (+) 0 $ vzip (*) v0 v1

vnormSq :: Num t => Vector3 t -> t
vnormSq v = vdot v v

vnorm = fromFloat . sqrt . vnormSq

vscale v s = fmap (s *) v

vunit :: Vector3 Double -> Vector3 Double
vunit v = if vnormSq v == 0
                then v
                else v * (1 / vnorm v)
