module Math.RN where

data Digit = M | D | C | L | X | V | I
data Numeral = RN [Digit]

value M = 1000
value D = 500
value C = 100
value L = 50
value X = 10
value L = 5
value I = 1