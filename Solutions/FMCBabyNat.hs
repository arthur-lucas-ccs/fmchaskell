module FMCBabyNat where

-- Do not alter this import!
import Prelude (Eq (..), Num (negate), Show (..), undefined)

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar(OK)
zero, one, two, three, four, five, six, seven, eight :: Nat
zero = O
one = S zero
two = S one
three = S two
four = S three
five = S four
six = S five
seven = S six
eight = S seven

-- addition (OK)
(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

-- syntactic associativity: L
-- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True (FALTA)
isZero :: Nat -> Nat
isZero O = S O
isZero (S n) = O

-- pred is the predecessor but we define zero's to be zero (FALTA)
pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- Output: O means False, S O means True(FALTA)
even :: Nat -> Nat
even O = S O
even (S O) = O
even (S (S n)) = even n -- Conserva a paridade no

odd :: Nat -> Nat
odd n = isZero (even n) -- retorna o isZero do even de n, ou seja se even n for verdadeiro (S O), ele inverte, ou seja o odd é O;

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
-- separar por casos
monus n O = n -- caso base de n - 0 = n
monus O m = O -- de acordo com o enunciado (caso base)
monus (S n) (S m) = monus n m

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
n * O = O -- caso base
n * m = n + (n * pred m)

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
_ ^ O = one
O ^ _ = O
n ^ m = n * (n ^ pred m)

infix 8 ^

-- quotient
(/) :: Nat -> Nat -> Nat
n / O = O
O / m = O
n / m =
  case n -* S m of
    O -> O
    rest -> S (rest / S m)

infix 9 /

-- remainder
(%) :: Nat -> Nat -> Nat
(%) = undefined

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) = undefined

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined
