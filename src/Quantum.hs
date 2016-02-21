module Quantum (
    QRegister,
    QGate,
    C,
    qbitsNum,
    dimensionsNum,
    qbit,
    qregister,
    qbitL,
    qregisterL,
    makeGateC,
    makeGate,
    (|>),
    (|+>),
    measure
) where

import System.Random (randomRIO)

import Data.List (sort)
import Data.Complex
import Matrix
import Utils

type C = Complex Double

data QRegister = QRegister {
    qbitsNum :: Int,
    states :: Matrix C
}

instance Show QRegister where
    show (QRegister n xm) = show xm

data QGate = QGate {
    dimensionsNum :: Int,
    operator :: Matrix C
}

instance Show QGate where
    show (QGate n xm) = show xm

class Entanglable a where
    entangle :: a -> a -> a

instance Entanglable QRegister where
    entangle (QRegister n1 xm) (QRegister n2 ym) = QRegister n zm
        where n = n1 + n2
              zm = kron xm ym

instance Entanglable QGate where
    entangle (QGate n1 xm) (QGate n2 ym) = QGate n zm
        where n = n1 + n2
              zm = kron xm ym

qbitL :: Int -> [C]
qbitL 0 = [1 :+ 0, 0 :+ 0]
qbitL 1 = [0 :+ 0, 1 :+ 0]
qbitL _ = error "Expecting |0> or |1>"

qbit :: Int -> QRegister
qbit = QRegister 1 . vectorV . qbitL

applyGate :: QGate -> QRegister -> QRegister
applyGate (QGate n1 xm) (QRegister n2 ym)
    | n1 /= n2 = error "Dimensions of gate and number of qbits in register disagree"
    | otherwise = QRegister (n2) (mult xm ym)

qregister :: [Int] -> QRegister
qregister = foldl1 entangle . map qbit

qregisterL :: [Int] -> [C]
qregisterL = concat . toList . states . qregister

makeGateC :: Matrix C -> QGate
makeGateC xm
    | r /= c = error "Gate matrix must be square"
    | not . powerOfTwo $ r = error "Gate matrix dimensions must be power of two"
    | otherwise = QGate n xm
    where r = rowsNum xm
          c = colsNum xm
          n = log2 r
          powerOfTwo n  = odd n || (elem n . take (log2 n + 1) . map (2 ^) $ [1..])
          log2 = fromIntegral . truncate . logBase 2.0 . fromIntegral

makeGate :: Matrix Double -> QGate
makeGate = makeGateC . fmap (:+ 0.0)

infixl 7 |>
(|>) :: QRegister -> QGate -> QRegister
(|>) = flip applyGate

infixl 8 |+>
(|+>) :: Entanglable a => a -> a -> a
(|+>) = entangle

measure :: QRegister -> IO Int
measure (QRegister n xm) = do
    rnd <- randomRIO (0, 1) :: IO Double
    let state = length . takeWhile (< rnd) $ probs
    return (state - 1)
    where probs = sort . (\xs -> 0.0 : xs ++ [1]) . scanl1 (+) . map (^ 2) . map magnitude . concat . toList $ xm