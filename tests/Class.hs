module Class(main) where
import Primitives

class Eqq a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool
  x /== y = not (x === y)

instance Eqq Int where
  (===) = primIntEQ

instance Eqq Char where
  (===) = primCharEQ

instance forall a . Eqq a => Eqq [a] where
  []     === []      =  True
  (x:xs) === (y:ys)  =  x === y && xs === ys
  _      === _       =  False

class (Eqq a) => Ordd a where
  (<==) :: a -> a -> Bool

instance Ordd Int where
  (<==) = (<=)

instance forall a b . (Eqq a, Eqq b) => Eqq (a, b) where
  (a, b) === (a', b')  =  a === a' && b === b'

f :: forall a . Eqq a => a -> Bool
f x = x === x

g :: forall a . Ordd a => a -> Bool
g x = x /== x

h :: forall a b . (Eqq a, Eqq b) => a -> b -> Bool
h a b = a === a && b === b

class A a b where
  a :: a -> b

class (A a a, A a a) => B1 a where
  b :: a -> a
  -- Multiple solutions
  b = a

class A a b => B a b
class A a b => C a b
class (B a b, C a b) => D a b where
  d :: a -> b
  -- Multiple solutions
  d = a

class E a where
  e :: a -> a

instance A Int Int where
  a x = x

-- Class synonym instances
type F a = E a
instance F Int where
  e x = x

-- Multiple instances at once
instance (B Int Int, C Int Int)

-- Multiple instances with methods
instance (A Char Char, E Char) where
  a x = x
  e x = x

main :: IO ()
main = do
  print $ f (5::Int)
  print $ g (5::Int)
  print $ h (5::Int) 'a'
  print $ f [88::Int]
  print $ f (1::Int, 'a')
  print $ (a ('a'::Char) :: Char)
  print $ e ('b'::Char)
