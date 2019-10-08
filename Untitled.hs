--  %%Copyright (c) 2019 William Emerison Six
--
--
-- \documentclass[twoside]{book}
-- \pagenumbering{gobble}
-- \usepackage[paperwidth=8.25in,
--             paperheight=10.75in,
--             bindingoffset=1.0in,
--             left=0.5in,
--             right=0.5in,
--             textheight=8.25in,
--             footskip=0.1in,
--             voffset=0.5in]{geometry}
-- \usepackage{times}
-- \usepackage{listings}
-- \lstloadlanguages{Haskell}
-- \usepackage{courier}
-- \usepackage{color}
-- \usepackage{makeidx}
-- \usepackage{amsmath}
-- \usepackage{amssymb}
-- \usepackage{titlesec}
-- \usepackage{stackengine}
-- \usepackage{appendix}

-- \lstnewenvironment{code}
--     {\lstset{
--       frame=single,
--       basicstyle=\ttfamily,
--       columns=fixed,
--       flexiblecolumns=false,
--       basewidth={0.5em,0.45em},
--       mathescape,
--       literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1  {=}{{$\triangleq$}}1 {==}{{==}}1
--                {>}{{$>$}}1 {<}{{$<$}}1 {==>}{{$\implies$}}1
--                {\\\\}{{\char`\\\char`\\}}1
--                {\\}{{$\lambda$}}1
--                {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
--                {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2
--                {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
--                {>>}{{>>}}2 {>>=}{{>>=}}2
--                {|}{{$\mid$}}1
--     }}
--     {}


-- \lstnewenvironment{examplecode}
--     {\lstset{
--       basicstyle=\ttfamily,
--       flexiblecolumns=false,
--       columns=fixed,
--       basewidth={0.5em,0.45em},
--       mathescape,
--       literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1 {==}{{==}}1
--                {>}{{$>$}}1 {<}{{$<$}}1 {==>}{{$\implies$}}1
--                {\\\\}{{\char`\\\char`\\}}1
--                {\\}{{$\lambda$}}1
--                {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
--                {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2
--                {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
--                {>>}{{>>}}2 {>>=}{{>>=}}2
--                {|}{{$\mid$}}1
--     }}
--     {}



-- \raggedbottom
-- \makeindex
-- \titleformat{\chapter}[display]
--  {\normalsize \huge  \color{black}}%
--  {\flushright\normalsize \color{black}%
--   \MakeUppercase{\chaptertitlename}\hspace{1ex}%
--   {\fontfamily{courier}\fontsize{60}{60}\selectfont\thechapter}}%
--  {10 pt}%
--  {\bfseries\huge}%
-- \date{}
-- \begin{document}
-- \bibliographystyle{alpha}
-- % Article top matter
-- \title{Untitled}
--
-- \author{Bill Six}
--
-- \maketitle
-- \null\vfill
-- \noindent
-- Copyright \textcopyright 2019 -- William Emerison Six\\
-- All rights reserved \\
-- Distributed under LGPL 2.1 or Apache 2.0 \\
-- Source code - http://github.com/billsix/untitled \\
-- Book generated from Git commit ID - \input{version.tex}
-- \newpage
-- \break
--
-- \vspace*{\fill}
-- \begin{center}
--  \begin{minipage}{.9\textwidth}
--
--  \noindent
--  EITHER
--
--  \vspace{1cm}
--  \noindent
--
--  \noindent
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--  \noindent
--       http://www.apache.org/licenses/LICENSE-2.0
--
--  \noindent
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.
--
--  \vspace{1cm}
--  \noindent
--  OR
--
--  \vspace{1cm}
--  \noindent
--
--  \noindent
--    This library is free software; you can redistribute it and/or
--    modify it under the terms of the GNU Lesser General Public
--    License as published by the Free Software Foundation; either
--    version 2.1 of the License, or (at your option) any later version.
--
--  \noindent
--    This library is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--    Lesser General Public License for more details.
--
--  \noindent
--    You should have received a copy of the GNU Lesser General Public
--    License along with this library; if not, write to the Free Software
--    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
--
--  \end{minipage}
--  \end{center}
--  \vfill
--  \clearpage
-- \newpage
-- \thispagestyle{empty}
-- \mbox{}
--
--
--  \clearpage
--
-- \chapter*{Preface}
--
-- Foo bar baz
--
--

-- \tableofcontents
-- \break
-- \chapter{Introduction}
-- \pagenumbering{arabic}


-- \begin{code}
module Untitled where

import Prelude hiding (subtract, and, or, not, Integer, all)
-- \end{code}


-- %% TODO make examplecode
-- \begin{examplecode}
-- data Bool = True | False
-- \end{examplecode}


-- %% TODO latex style to show that this is not part of the code
-- \begin{examplecode}
-- myIf ::  Bool -> (x -> (x -> x))
-- myIf True  a b = a
-- myIf False a b = b
-- \end{examplecode}


-- \begin{examplecode}
-- instance Show Bool where
--    show True = "True"
--    show False = "False"

-- Prelude> :l Untitled
-- [1 of 1] Compiling Untitled         ( Untitled.hs, interpreted )
-- Ok, one module loaded.
-- *Untitled> x = True
-- *Untitled> :t x
-- x :: Bool
-- *Untitled> show x
-- "True"
-- \end{examplecode}

-- %% TODO - add in commented out example enum instance for bool,
-- \begin{code}
booleans = enumFrom False
-- \end{code}


-- \begin{code}
not :: Bool -> Bool
not True   = False
not False  = True
-- \end{code}

-- \begin{examplecode}
-- Prelude> :l Untitled
-- [1 of 1] Compiling Untitled         ( Untitled.hs, interpreted )
-- Ok, one module loaded.
-- *Untitled> not True
-- False
-- \end{examplecode}



-- \begin{code}[caption={Some Java code},label=(lst:label),language=Java]
and :: Bool -> (Bool -> Bool)
and True  a  = a
and False a  = False
-- \end{code}


-- \begin{examplecode}
-- *Untitled> not True
-- False
-- \end{examplecode}


-- %% TODO - prove that the following hold
-- \begin{examplecode}
-- and True True   = True
-- and True False  = False
-- and False True  = False
-- and False False = False

-- *Untitled> and True False
-- False
-- *Untitled> curried = and True
-- *Untitled> :t curried
-- curried :: Bool -> Bool
-- *Untitled> curried True
-- True
-- *Untitled> curried False
-- False
-- \end{examplecode}


-- \begin{code}
or :: Bool -> (Bool -> Bool)
or False False = False
or _     _     = True
-- \end{code}


-- %% TODO prove the following
-- \begin{examplecode}
-- or True True   = True
-- or True False  = True
-- or False True  = True
-- or False False = False
-- \end{examplecode}

-- \begin{code}
xor :: Bool -> (Bool -> Bool)
xor True True   = False
xor True False  = True
xor False True  = True
xor False False = False
-- \end{code}

-- Implies must return False whenever p is True but q is False.  Should p
-- be False, the implication is not yet disproven, so return True.

-- \begin{code}
implies :: Bool -> (Bool -> Bool)
implies p@True  q = q
implies p@False q = neitherConfirmNorDeny where neitherConfirmNorDeny = True

infix 1 ==>
(==>) :: Bool -> Bool -> Bool
p ==> q = implies p q
-- \end{code}


-- \begin{examplecode}
-- *Untitled> True ==> True
-- True
-- *Untitled> True ==> False
-- False
-- \end{examplecode}

-- \begin{examplecode}
-- *Untitled>  [x ==> y | x <- booleans, y <- booleans]
-- [True,True,False,True]
-- *Untitled> True ==> False
-- False
-- \end{examplecode}

-- \subsection{Equivalence}

-- \begin{code}
equivalent :: Bool -> (Bool -> Bool)
equivalent p q = and (p ==> q) (q ==> p)
-- \end{code}

-- \begin{examplecode}
-- *Untitled> equivalent True True
-- True
-- *Untitled> equivalent True False
-- False
-- *Untitled> equivalent False True
-- False
-- *Untitled> equivalent False False
-- True

infix 1 <==>
(<==>) :: Bool -> (Bool -> Bool)
p <==> q = equivalent p q
-- \end{examplecode}

-- %% discuss function equivalence.  for all inputs, give same output, including termination/nontermination
-- \begin{examplecode}
-- equivalent = not . xor
-- \end{examplecode}




-- \subsection{Contrapositive}

-- \begin{examplecode}
-- *Untitled> [(x ==> y) ==> ((not y) ==> (not x)) | x <- booleans, y <- booleans]
-- [True,True,True,True]
-- \end{examplecode}


-- \begin{examplecode}
-- (\x y -> True) = (\x y -> implies (implies x y) (implies (not y) (not x)))
-- \x y -> True = \x y -> implies (implies x y) (implies (not y) (not x))
-- True = implies (implies x y) (implies (not y) (not x))
-- \end{examplecode}


-- \begin{examplecode}
-- *Untitled> [(x ==> y) `equivalent` ((not y) ==> (not x)) | x <- booleans, y <- booleans]
-- [True,True,True,True]
-- \end{examplecode}

-- \begin{examplecode}
-- \x y -> x ==> y = \x y -> (not y) ==> (not x)
-- x ==> y = (not y) ==> (not x)
-- \end{examplecode}


-- \subsection{Modus Ponens}
-- \begin{examplecode}
-- *Untitled>  [(x `and` (x ==> y)) ==> y | x <- booleans, y <- booleans]
-- [True,True,True,True]
-- \end{examplecode}

-- \begin{examplecode}
-- *Untitled> [y ==> (x `and` (x ==> y)) | x <- booleans, y <- booleans]
-- [True,False,True,True]
-- \end{examplecode}

-- \begin{code}
all :: [Bool] -> Bool
all []             = True
all (True  : rest) = all rest
all (False : rest) = False
-- \end{code}


-- \begin{examplecode}
-- *Untitled> all [(x `and` (x ==> y)) ==> y | x <- booleans, y <- booleans]
-- True
-- \end{examplecode}

-- \begin{examplecode}
-- *Untitled> all [y ==> (x `and` (x ==> y)) | x <- booleans, y <- booleans]
-- False
-- \end{examplecode}



-- \begin{examplecode}
-- *Untitled> all [((x ==> y) `and` (y ==> z)) ==> (x ==> z) | x <- booleans, y <- booleans, z <- booleans]
-- True
-- \end{examplecode}


-- \subsection{Conjunction}
-- \begin{examplecode}
-- *Untitled>  all [(x `and` y) ==> x | x <- booleans, y <- booleans]
-- True
-- *Untitled>  all [(x `and` y) ==> y | x <- booleans, y <- booleans]
-- True
-- \end{examplecode}

-- \subsection{aeouaoeu}
-- \begin{examplecode}
-- *Untitled> all [x ==> (y ==> x) | x <- booleans, y <- booleans]
-- True
-- \end{examplecode}



-- %% TODO - cons cells

-- \section{Equality}

-- For all data types created which are to be compared for
-- equality, the data type must be an instance of the
-- Eq class, which requires a == b to be defined.

-- In order to allow say equal expressions of type Foo may be
-- freely between equations, we make constraints on the definitions
-- of equality.

-- Reflexivity means for elements a of type Foo,

-- \begin{examplecode}
-- True = (a == a)
-- \end{examplecode}

-- Symmetry means for all elements a and b of type Foo,

-- \begin{examplecode}
-- True = ((a == b) == (b == a))
-- \end{examplecode}

-- Transitivity means for all elements a, b, and c of type Foo,

-- \begin{examplecode}
--  True = implies (a == b `and` b == c) (a == c)
-- \end{examplecode}


-- \begin{examplecode}
-- instance Eq Boolean where
--    True  == True  = True
--    True  == False = False
--    False == True  = False
--    False == False = True
-- \end{examplecode}

-- %% TODO - prove reflexivity, symmetry, and transitivity for Bool

-- \section{Substitution}

-- \section{Integers}


-- \begin{code}
data Integer = Zero | Succ Integer
-- \end{code}

-- \begin{code}
instance Show Integer where
    show Zero     = "Zero"
    show (Succ x) = "Succ (" ++ (show x) ++ ")"
-- \end{code}



-- \begin{code}
instance Eq Integer where
     Zero     == Zero     = True
     Zero     == (Succ a) = False
     (Succ a) == Zero     = False
     (Succ a) == (Succ b) = a == b
-- \end{code}

-- %% TODO - prove reflexivity, symmetry, and transitivity for PeanoInterger.  Rename to Integer


-- \section{Enumerations}

-- %% TODO - describe what this is, figure out if it's related to sets
-- %% TODO - figure out how to generically combine enums from sub structs, i.e., how to enumerate rationals, removing duplicates

-- \begin{code}
instance Enum Integer where
     fromEnum Zero     = 0
     fromEnum (Succ x) = (fromEnum x) + 1

     toEnum 0 = Zero
     toEnum x = Succ (toEnum (x - 1))
-- \end{code}


-- %% TODO describe
-- \begin{code}
peanoIntegers = enumFrom Zero
-- \end{code}

-- \begin{code}
-- take 3 peanoIntegers
-- [Zero,Succ (Zero),Succ (Succ (Zero))]
-- \end{code}


-- %% TODO - use typeclasses instead to define addition and multiplications

-- \begin{code}
add :: Integer -> (Integer -> Integer)
add a Zero     = a
add a (Succ b) = Succ (add a b)
-- \end{code}

-- %% TODO - prove that addition is commutative

-- \begin{code}
multiply :: (Integer -> (Integer -> Integer))
multiply a    Zero     = Zero
multiply Zero b        = Zero
multiply a    (Succ b) = add a (multiply a b)
-- \end{code}


-- \section{Plots}

-- \section{Signed Integers}


-- \begin{code}
data SignedInteger = Positive Integer | Negative Integer
     deriving (Show)


instance Eq SignedInteger where
     (Positive x)    == (Positive y)    = x == y
     (Negative x)    == (Negative y)    = x == y
     (Positive Zero) == (Negative Zero) = True
     (Negative Zero) == (Positive Zero) = True
     _               == _               = False
-- \end{code}


-- \begin{code}
toSigned :: (Integer -> SignedInteger)
toSigned a = Positive a
-- \end{code}


-- \begin{code}
subtract :: (Integer -> (Integer -> SignedInteger))
subtract a Zero            = Positive a
subtract Zero a            = Negative a
subtract (Succ a) (Succ b) = subtract a b
-- \end{code}





-- \begin{code}
signedIntegerAdd :: (SignedInteger -> (SignedInteger -> SignedInteger))
signedIntegerAdd (Positive a) (Positive b) = Positive (add a b)
signedIntegerAdd (Positive a) (Negative b) = subtract a b
signedIntegerAdd (Negative a) (Positive b) = subtract b a
signedIntegerAdd (Negative a) (Negative b) = Negative (add a b)
-- \end{code}


-- \begin{code}
signedIntegerNegate :: (SignedInteger -> SignedInteger)
signedIntegerNegate (Positive a)  = Negative a
signedIntegerNegate (Negative a)  = Positive a
-- \end{code}


-- \begin{code}
signedIntegerSubtract :: (SignedInteger -> (SignedInteger -> SignedInteger))
signedIntegerSubtract a b = signedIntegerAdd a (signedIntegerNegate b)
-- \end{code}

-- \begin{code}
signedIntegerMultiply :: (SignedInteger -> (SignedInteger -> SignedInteger))
signedIntegerMultiply (Positive a) (Positive b) = Positive (multiply a b)
signedIntegerMultiply (Positive a) (Negative b) = Negative (multiply a b)
signedIntegerMultiply (Negative a) (Positive b) = Negative (multiply a b)
signedIntegerMultiply (Negative a) (Negative b) = Positive (multiply a b)
-- \end{code}

-- \section{Rationals}

-- \section{Intervals}
-- \end{document}  %End of document.
