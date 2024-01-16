data Vec a = Vec a a
mapV f (Vec a1 b1) = Vec (f a1) (f b1)
mapV2 f (Vec a1 b1) (Vec a2 b2) = Vec (f a1 a2) (f b1 b2)

instance Num a => Num (Vec a) where
    (+) = mapV2 (+)
    (*) = mapV2 (*)
    negate = mapV negate
    signum = mapV signum
    abs = mapV abs
    fromInteger x = let i = fromInteger x in Vec i i

instance Semigroup a => Semigroup (Vec a) where
    Vec a1 b1 <> Vec a2 b2 = Vec (a1 <> a2) (b1 <> b2)

instance Monoid a => Monoid (Vec a) where
    mempty = Vec mempty mempty
