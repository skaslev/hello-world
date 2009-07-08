#!/usr/bin/env runhaskell
import Data.Char
import Data.List
import Data.Maybe

q = zip("hel word")(map(e(5))[32641183,22730431,17318431,1024,3381026947,33080895,27432127,4539967])
w = fst$q!!3
e k = unfoldr(listToMaybe.t(k))
r k x = x++replicate(k-length(x))(w)
t _ 0 = []
t k x = [(mod(x)(2^k),div(x)(2^k))]
y _ 0 = w
y k _ = k
say = putStr.unlines.transpose.concatMap
      (\k->map(r(5).map(toUpper.y(k)).e(1))(fromJust(lookup(k)(q)))).intersperse(w)

main = say "hello world"
