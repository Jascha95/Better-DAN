{-# LANGUAGE GADTs, TypeFamilies #-}
module AGExample where

-- nonterminals
data BIT
data BITS
data NUM

data Tree nt where
    BIT_0         :: Tree BIT
    BIT_1         :: Tree BIT
    BITS_BIT      :: Tree BIT  -> Tree BITS
    BITS_BITS_BIT :: Tree BITS -> Tree BIT -> Tree BITS
    NUM_BITS      :: Tree BITS -> Tree NUM
    NUM_BITS_BITS :: Tree BITS -> Tree BITS -> Tree NUM

class Evaluate nt where
    data Inh nt
    data Syn nt
    eval :: Tree nt -> Inh nt -> Syn nt

instance Evaluate BIT where
    data Inh BIT = InhBIT { sBIT :: Int }
    data Syn BIT = SynBIT { vBIT :: Double }
    eval BIT_0 inh =
        SynBIT { vBIT = 0 }
    eval BIT_1 inh =
        SynBIT { vBIT = 2 ^^ sBIT inh }

instance Evaluate BITS where
    data Inh BITS = InhBITS { sBITS :: Int }
    data Syn BITS = SynBITS { vBITS :: Double, lBITS :: Int }
    eval (BITS_BIT tbit) inh =
        let inhbit = InhBIT { sBIT = sBITS inh }
            synbit = eval tbit inhbit
        in  SynBITS { vBITS = vBIT synbit, lBITS = 1 }
    eval (BITS_BITS_BIT tbits tbit) inh =
        let inhbits = InhBITS { sBITS = sBITS inh + 1 }
            synbits = eval tbits inhbits
            inhbit  = InhBIT { sBIT = sBITS inh }
            synbit = eval tbit inhbit
        in  SynBITS { vBITS = vBITS synbits + vBIT synbit, lBITS = lBITS synbits + 1 }

instance Evaluate NUM where
    data Inh NUM = InhNUM {}
    data Syn NUM = SynNUM { vNUM :: Double }
    eval (NUM_BITS tbits) inh =
        let inhbits = InhBITS { sBITS = 0 }
            synbits = eval tbits inhbits
        in  SynNUM { vNUM = vBITS synbits }
    eval (NUM_BITS_BITS tbits1 tbits2) inh =
        let inhbits1 = InhBITS { sBITS = 0 }
            synbits1 = eval tbits1 inhbits1
            inhbits2 = InhBITS { sBITS = - lBITS synbits2 }
            synbits2 = eval tbits2 inhbits2
        in  SynNUM { vNUM = vBITS synbits1 + vBITS synbits2 }

-- examples

t1 = BIT_0
t2 = (BITS_BITS_BIT (BITS_BIT BIT_1) BIT_0)
t3 = (BITS_BITS_BIT (BITS_BIT BIT_1) BIT_1)
t4 = (BITS_BITS_BIT (BITS_BITS_BIT (BITS_BIT BIT_1) BIT_1) BIT_0)
n2 = NUM_BITS t2
n3 = NUM_BITS t3
n4 = NUM_BITS t4
n5 = NUM_BITS_BITS t2 t3
n6 = NUM_BITS_BITS t3 t3
