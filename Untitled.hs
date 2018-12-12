{-// The MIT License (MIT)-}
{-//-}
{-// Copyright (c) 2018 William Emerison Six-}
{-//-}
{-// Permission is hereby granted, free of charge, to any person obtaining a copy-}
{-// of this software and associated documentation files (the "Software"), to deal-}
{-// in the Software without restriction, including without limitation the rights-}
{-// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell-}
{-// copies of the Software, and to permit persons to whom the Software is-}
{-// furnished to do so, subject to the following conditions:-}
{-//-}
{-// The above copyright notice and this permission notice shall be included in all-}
{-// copies or substantial portions of the Software.-}
{-//-}
{-// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR-}
{-// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,-}
{-// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE-}
{-// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER-}
{-// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,-}
{-// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE-}
{-// SOFTWARE.-}

{-= Untitled-}
{-:author: William Emerison Six <billsix@gmail.com>-}
{-:doctype: book-}
{-:toc:-}

{-[preface]-}
{-= Preface-}

{-[source,Haskell,linenums]-}
{------}
module Untitled where
{------}


{-[source,Haskell,linenums]-}
{------}
data PeanoInteger = Zero | Succ PeanoInteger
     deriving (Show)


instance Eq PeanoInteger where
         (==) = peanoIntegerEqual where
              peanoIntegerEqual Zero Zero         = True
              peanoIntegerEqual Zero (Succ a)     = False
              peanoIntegerEqual (Succ b) Zero     = False
              peanoIntegerEqual (Succ c) (Succ d) = peanoIntegerEqual c d
{------}

{-[source,Haskell,linenums]-}
{------}
add :: (PeanoInteger -> (PeanoInteger -> PeanoInteger))
add a Zero = a
add a (Succ b) = Succ (add a b)
{------}

{-[source,Haskell,linenums]-}
{------}
multiply :: (PeanoInteger -> (PeanoInteger -> PeanoInteger))
multiply a Zero = Zero
multiply Zero b = Zero
multiply a (Succ Zero) = a
multiply a (Succ b) = add a (multiply a b)
{------}


{-[source,Haskell,linenums]-}
{------}
data SignedInteger = Positive PeanoInteger | Negative PeanoInteger
     deriving (Show)


instance Eq SignedInteger where
         (==) = signedIntegerEqual where
              signedIntegerEqual (Positive x)    (Positive y)    = x == y
              signedIntegerEqual (Negative x)    (Negative y)    = x == y
              signedIntegerEqual (Positive Zero) (Negative Zero) = True
              signedIntegerEqual (Negative Zero) (Positive Zero) = True
              signedIntegerEqual _ _ = False
{------}


{-[source,Haskell,linenums]-}
{------}
toSigned :: (PeanoInteger -> SignedInteger)
toSigned a = Positive a
{------}


{-[source,Haskell,linenums]-}
{------}
subtract :: (PeanoInteger -> (PeanoInteger -> SignedInteger))
subtract a Zero = Positive a
subtract Zero a = Negative a
subtract (Succ a) (Succ b) = Untitled.subtract a b
{------}





{-[source,Haskell,linenums]-}
{------}
signedIntegerAdd :: (SignedInteger -> (SignedInteger -> SignedInteger))
signedIntegerAdd (Positive a) (Positive b) = Positive (add a b)
signedIntegerAdd (Positive a) (Negative b) = Untitled.subtract a b
signedIntegerAdd (Negative a) (Positive b) = Untitled.subtract b a
signedIntegerAdd (Negative a) (Negative b) = Negative (add a b)
{------}


{-[source,Haskell,linenums]-}
{------}
signedIntegerNegate :: (SignedInteger -> SignedInteger)
signedIntegerNegate (Positive a)  = Negative a
signedIntegerNegate (Negative a)  = Positive a
{------}


{-[source,Haskell,linenums]-}
{------}
signedIntegerSubtract :: (SignedInteger -> (SignedInteger -> SignedInteger))
signedIntegerSubtract a b = signedIntegerAdd a (signedIntegerNegate b)
{------}

{-[source,Haskell,linenums]-}
{------}
signedIntegerMultiply :: (SignedInteger -> (SignedInteger -> SignedInteger))
signedIntegerMultiply (Positive a) (Positive b) = Positive (multiply a b)
signedIntegerMultiply (Positive a) (Negative b) = Negative (multiply a b)
signedIntegerMultiply (Negative a) (Positive b) = Negative (multiply a b)
signedIntegerMultiply (Negative a) (Negative b) = Positive (multiply a b)
{------}


{-[source,Haskell,linenums]-}
{------}
instance Enum PeanoInteger where
         fromEnum = peanoIntegerFromEnum where
                  peanoIntegerFromEnum Zero = 0
                  peanoIntegerFromEnum (Succ x) = (peanoIntegerFromEnum x) + 1
         toEnum = peanoIntegerToEnum where
                  peanoIntegerToEnum 0 = Zero
                  peanoIntegerToEnum x = Succ (peanoIntegerToEnum (x - 1))
{------}
