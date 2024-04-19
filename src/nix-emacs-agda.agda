module nix-emacs-agda where
open import Data.Nat
open import Data.Complex
open import Data.List
open import Data.Fin
open import Data.Vec
open import Data.Float
open import Data.Maybe
open import Relation.Binary.PropositionalEquality

module TonalPitchSpace where

record Mod (n : ℕ) : Set where
  constructor mkMod
  field
    val : ℕ
    p : val < n

open Mod

ModEq : ∀ {n} → Mod n → Mod n → Bool
ModEq {n} m1 m2 = (val m1) ≡ (val m2)

toMod : ∀ {n} → ℕ → Mod (suc n)
toMod {n} x = mkMod (x % (suc n)) (s≤s (m≤m+n x n))

unMod : ∀ {n} → Mod (suc n) → ℕ
unMod m = val m

data Chord (n : ℕ) : Set where
  MkChord : ∀ {a} → Vec a n → Chord n

tk6 : List (Mod 12) → Vec Float 6
tk6 ns = tk6' cs'
  where
    cs' = cs ns

postulate
  dft : Vec (Complex Float) 12 → Vec (Complex Float) 12

tk6' : Vec (Complex Float) 12 → Vec (Complex Float) 6
tk6' cs' = vecZipWith _*_ wks dftd'
  where
    dftd' = vecTake 6 (dft cs')
    cbar = vecFoldr _+_ 0 cs'
    wks = vecMap (/_ cbar) ws

cs : List (Mod 12) → Vec (Complex Float) 12
cs = foldr go zeroVector
  where
    go : Mod 12 → Vec (Complex Float) 12 → Vec (Complex Float) 12
    go v acc = replace (fromJust (lookup (unMod v) indices)) 1 acc

indices : List (Fin 12)
indices = 0 ∷ 1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ 6 ∷ 7 ∷ 8 ∷ 9 ∷ 10 ∷ 11 ∷ []

replace : {A : Set} {n : ℕ} → Fin n → A → Vec A n → Vec A n
replace zero x (y ∷ ys) = x ∷ ys
replace (suc i) x (y ∷ ys) = y ∷ replace i x ys

zeroVector : Vec (Complex Float) 12
zeroVector = replicate 12 (complex 0.0 0.0)

ws : Vec Float 6
ws = 2.0 ∷ 11.0 ∷ 17.0 ∷ 16.0 ∷ 19.0 ∷ 7.0 ∷ []

vecZipWith : {A B C : Set} {n : ℕ} → (A → B → C) → Vec A n → Vec B n → Vec C n
vecZipWith f [] [] = []
vecZipWith f (x ∷ xs) (y ∷ ys) = f x y ∷ vecZipWith f xs ys

-- Agda code (continuation)

cosineDistance : {A : Set} {n : ℕ} → Vec (Complex A) n → Vec (Complex A) n → Complex A
cosineDistance v1 v2 = (vecSum (vecZipWith _*_ v1 (vecMap conjugate v2))) / (norm v1 * norm v2)

consonance : {A : Set} → List (Mod ℕ 12) → A
consonance xs = (realPart (norm (toTk xs))) / 32.86335345030997

ewma : {A : Set} {n : ℕ} → A → Vec (Complex A) n → Vec (Complex A) n → Vec (Complex A) n
ewma a acc x = vecZipWith _*_ (vecZipWith _-_ ones alpha) (vecZipWith _*_ (vecZipWith _+_ acc alpha) x)
  where
    ones = vecTabulate (λ _ → 1)
    alpha = vecTabulate (λ _ → toComplex a)

cononanceExpWeighted :
  {A : Set} →
  A →
  Vec (Complex A) 12 →
  Vec (Complex A) 12 →
  (Vec (Complex A) 12, A)
cononanceExpWeighted a acc x = (acc', realPart (norm (toTk' acc')))
  where
    acc' = ewma a x acc

cosineDistanceExpWeighted :
  {A : Set} {n : ℕ} →
  A →
  Vec (Complex A) n →
  Vec (Complex A) n →
  (Vec (Complex A) n, A)
cosineDistanceExpWeighted a acc x = (acc', d)
  where
    d = realPart (cosineDistance acc x)
    acc' = ewma a x acc

-- Note: Continue with the remaining functions and types


-- Agda code (continuation)

record RhythmicVariation (A : Set) : Set where
  constructor mkRhythmicVariation
  field
    offset : Mod ℕ 12
    vec    : Vec (Complex A) 12

rhythmicVariation : {A : Set} → RhythmicVariation A → List (Mod ℕ 12) → List (Mod ℕ 12)
rhythmicVariation rv xs = fmap (λ x → x + RhythmicVariation.offset rv) xs

applyRhythmicVariation : {A : Set} → RhythmicVariation A → Vec (Complex A) 12 → Vec (Complex A) 12
applyRhythmicVariation rv x = vecRotate (RhythmicVariation.offset rv) (vecZipWith _*_ x (RhythmicVariation.vec rv))

unapplyRhythmicVariation : {A : Set} → RhythmicVariation A → Vec (Complex A) 12 → Vec (Complex A) 12
unapplyRhythmicVariation rv x = vecRotate (negate (RhythmicVariation.offset rv)) (vecZipWith _*_ x (vecMap reciprocal (RhythmicVariation.vec rv)))

consonanceRhythmicVariation :
  {A : Set} →
  RhythmicVariation A →
  List (Mod ℕ 12) →
  A
consonanceRhythmicVariation rv xs = consonance (rhythmicVariation rv xs)

consonanceExpWeightedRhythmicVariation :
  {A : Set} →
  A →
  RhythmicVariation A →
  Vec (Complex A) 12 →
  Vec (Complex A) 12 →
  (Vec (Complex A) 12, A)
consonanceExpWeightedRhythmicVariation a rv acc x = cononanceExpWeighted a (applyRhythmicVariation rv acc) x

cosineDistanceExpWeightedRhythmicVariation :
  {A : Set} {n : ℕ} →
  A →
  RhythmicVariation A →
  Vec (Complex A) n →
  Vec (Complex A) n →
  (Vec (Complex A) n, A)
cosineDistanceExpWeightedRhythmicVariation a rv acc x = cosineDistanceExpWeighted a (applyRhythmicVariation rv acc) x

-- Note: The translation is now complete. You may need to adjust the code to fit your specific Agda setup and imports.


-- Agda code

module DFT where

open import Data.Nat using (ℕ; _+_; _*_; _^_; _≤_; s≤s; z≤n)
open import Data.Vec using (Vec; []; _∷_; lookup; _∷ʳ_; _++_; replicate)
open import Data.List using (List; _++_; map)
open import Data.Complex using (Complex; _+_; _*_; cis)
open import Data.Rational using (fromInteger)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

dft : ∀ {n} → Vec (Complex Float) n → Vec (Complex Float) n
dft {n = 0} [] = []
dft {n = 1} (x ∷ []) = x ∷ []
dft {n} xs with n divMod 2
... | (q, 0) = dft evens ++ dft odds
  where
    evens = [ lookup xs (2 * i) | i ← [0 .. q - 1] ]
    odds = [ lookup xs (2 * i + 1) | i ← [0 .. q - 1] ]

    dftEvens = dft evens
    dftOdds = dft odds

    dft' = dftEvens ++ dftOdds

    -- Twiddle factors
    twiddles = [ cis (-2.0 * pi * (fromInteger (toFloat i)) / (fromInteger (toFloat n))) | i ← [0 .. n - 1] ]

    -- Apply twiddle factors to the odd-indexed DFT components
    twiddledOdds = zipWith _*_ dftOdds twiddles

    -- Combine the even-indexed and odd-indexed DFT components
    _++_ : ∀ {n} → Vec (Complex Float) n → Vec (Complex Float) n → Vec (Complex Float) n
    []       ++ ys = ys
    (x ∷ xs) ++ ys = x ∷ (xs ++ ys)

dft {n} xs | (q, r) = xs
  where
    _divMod_ : ℕ → ℕ → (ℕ × ℕ)
    m divMod n = divMod′ m n 0
      where
        divMod′ : ℕ → ℕ → ℕ → (ℕ × ℕ)
        divMod′ 0       n acc = (acc , 0)
        divMod′ (suc m) n acc with suc acc ≤? n
        ... | true  = divMod′ m n (suc acc)
        ... | false = (acc , suc m)
