module Exinst.Internal.Show where

-- Internal wrappers used to avoid writing the string manipulation in 'Show'.
data Some1'Show r1 x = Some1 r1 x deriving (Show)
data Some2'Show r2 r1 x = Some2 r2 r1 x deriving (Show)
data Some3'Show r3 r2 r1 x = Some3 r3 r2 r1 x deriving (Show)
data Some4'Show r4 r3 r2 r1 x = Some4 r4 r3 r2 r1 x deriving (Show)
