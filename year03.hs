{-
Year.hs
Haskell program to generate equations for the numbers 1 to 100 using the digits of the year
See https://gibhub.com/alfille/Year_digits

by Paul H Alfille 2023
MIT License

-}

module Year where

-- for permutation
import Data.List

-- | uniq eliminates duplicates in a list
uniq :: Eq a => [a] -> [a]
-- ^
uniq [] = []
uniq (x:xs)
    | elem x xs = rest
    | otherwise = x:rest
    where rest = uniq xs


-- | splode separates list into list of lists -- e.g. "2023" into ["2","0","2","3"]
splode :: [a] -> [[a]]
-- ^
splode x = [ [l] | l <- x ]

-- | cperm (aka character permute) creates a list of all possible concatination of letters in order
-- e.g. cperm $ splode "12" = [["1","2"],["12"]]
cperm :: [[a]] -> [[[a]]]
-- ^
cperm x = case (length x) of
    0 -> [[]]
    1 -> [x]
    2 -> [[x1,x2],[x3]]
    _ -> cperm (x3:xs) ++ ( map ([x1]++) $ cperm (x2:xs) )
    where (x1:x2:xs) = x
          x3 = x1++x2

-- | orderList generates the cpermutations of the year's digits in order
orderList :: String -> [[String]]
-- ^
orderList x = cperm $ splode x 

-- | orderList generates the cpermutations of the year's digits in any order
unorderList :: String -> [[String]]
-- ^
unorderList x = foldl1 (++) $ map cperm $ uniq $ permutations $ splode x 

-- | Operation type holds information on unitary and binary operations
data Operation =
        UnaryOp String (Integer -> Maybe Integer) |
        BinaryOp String (Integer -> Integer -> Maybe Integer)
-- ^

{-
-- factorial optional 
factorial n
    | n<2 = 1
    | otherwise = n * factorial (n-1)
-}

-- | sub_str substitute a string for an underscore -- used in explaining an equation
-- replace only first occurrence of '_' with variable
sub_str :: String -> String -> String -- used as String -> String -> String
-- ^
sub_str [] _ = []
sub_str ('_':xs) v = v++xs
sub_str (x:xs) v = (x:(sub_str xs v))
    
-- | Expression type holds intermediate and final equation information
-- includes value and String explaining calculation to date
-- Also has Invalid type for redundant expressions or divide-by-zero
data Expression =
            Invalid String | -- String is explanation
            Single { value :: Integer
            , operators :: Integer
            , literal :: String
            }| -- equation and result of calculation: value, ops, equation_text
            Pair Expression Expression  -- pair of expressions ready for binary operation
            deriving(Show)
-- ^

-- | pair_up creates all possible pairs of a list (for binary operators)
-- i.e. [a,b,c] -> (a,(b,c) + ((a,b),c)
-- results wrapped up in Expression types
pair_up :: [String] -> [Expression]
-- ^
pair_up xs = case len of
    0 -> [Invalid "No input"]
    1 -> [Single {value=(read x1::Integer), operators=0, literal=x1} ] -- Also interpret literal to numeric
    _ -> [Pair f s | w<-[ (pair_up $ fst u, pair_up $ snd u) | u <- [ splitAt n xs | n <- [ 1 .. (len-1) ] ] ], f<-fst w, s <-snd w]
    where len = length xs
          (x1:xx) = xs 

-- | operator lists, unary and binary
-- minus removed since x-y == x+(-y) so would be double counted
-- Exponentiation 
unaryList = [
    UnaryOp "_" (\x->Just (x) )
    ,UnaryOp "(-_)" (\x->Just (-x) )
--    ,UnaryOp "_!" (\x-> if (x<0) || (x>10) then Nothing else Just (factorial x) )
    ]

binaryList = [
    BinaryOp "(_+_)" (\x y->Just (x+y) )
--    ,BinaryOp "(_-_)" (\x y->if y<0 then Nothing else Just (x-y) )
    ,BinaryOp "(_*_)" (\x y->Just (x*y) )
    ,BinaryOp "(_/_)" (\x y->if (y==0) || ((mod x y)/=0) then Nothing else Just (div x y) )
    ,BinaryOp "(_^_)" (\x y->if (y<=0) || (x<0) then Nothing else Just (x^y) )
--    ,BinaryOp "(_ mod _)" (\x y->if (y<1) then Nothing else Just (mod x y) )
    ]

-- | unary_calc apply unitary operator
-- Single and op to calculated Single
unary_calc :: Expression -> Operation -> Expression
-- ^
unary_calc (Invalid s) _ = Invalid s
unary_calc single@(Single {value=i1, operators=o1, literal=s1}) (UnaryOp s f) = case c of
    Nothing -> Invalid sout
    Just(i) -> single { value=i, literal=sout} 
    where sout = sub_str s s1
          c =  f i1
-- | binary_calc apply an operation to a pair of values
-- Two Singletons and Op to calculated Single
binary_calc :: Expression -> Expression -> Operation -> Expression
-- ^
binary_calc (Invalid s) _ _ = Invalid s
binary_calc _ (Invalid s) _ = Invalid s
binary_calc (Single {value=i1, operators=o1, literal=s1}) (Single {value=i2, operators=o2, literal=s2}) (BinaryOp s f) = case c of
    Nothing -> Invalid sout
    Just(i) -> Single { value=i, operators=o1+o2+1, literal=sout}
    where sout = sub_str (sub_str s s1) s2
          c =  f i1 i2

-- | unary_multicalc Apply all unitary ops
unary_multicalc :: Expression -> [Expression]
-- ^
unary_multicalc (Invalid s) = [Invalid s]
unary_multicalc sing = [ unary_calc sing op | op <- unaryList ] 

-- binary_multicalc Apply binary opps (recursively)
binary_multicalc :: Expression -> [Expression]
-- ^
binary_multicalc (Invalid s) = [Invalid s]
binary_multicalc (Pair p1 p2) = [
            unary_calc (binary_calc pp1 pp2 o2) o1 |
                pp1 <- (binary_multicalc p1),
                pp2 <- (binary_multicalc p2),
                o2 <- binaryList,
                o1 <- unaryList ]  
binary_multicalc s = unary_multicalc s -- Single implied

-- | total_calc apply calc to a list of expressions
total_calc :: [Expression] -> [Expression]
-- ^
total_calc es = foldr1 (++) [ binary_multicalc e | e <- es ]

-- | orderedPairs and unorderedPairs -- create the list of possible equations from a year 
-- literal number -> digits -> (-> optional permutations) -> combinations of digits -> All binary pairs in an Expression
orderedPairs :: String -> [Expression]
unorderedPairs :: String -> [Expression]
-- ^
orderedPairs x = foldr1 (++) $ fmap pair_up $ orderList x
unorderedPairs  x = foldr1 (++) $ fmap pair_up $ unorderList  x

-- | goodcalc for filtering list of expressions and removing inappropriate ones
-- filter for solutions to restring to 1 .. 100
goodcalc :: Expression -> Bool
-- ^
goodcalc (Invalid _) = False
goodcalc (Single {value=i}) = (i>0) && (i<101)

-- | compare single for sorting solved expressions
-- sorting comparison value, then string length
compareSingle :: Expression -> Expression -> Ordering
-- ^
compareSingle (Single {value=i1, operators=o1}) (Single {value=i2, operators=o2}) = compare (i1,o2) (i2,o2)

bestcalc :: [Expression] -> [Expression]
-- take only first value from the sorted list
bestcalc [] = []
bestcalc [e] = [e]
bestcalc e@(e1@(Single {value=i1}):e2@(Single {value=i2}):es)
    | i1==i2 = bestcalc (e1:es)
    | otherwise = (e1:(bestcalc (e2:es)))

split_chars _ [] = []
split_chars c x
    | b == "" = [a]
    | a == "" = [[b']] ++ (split_chars c bs)
    | otherwise = [a,[b']] ++ (split_chars c bs) 
    where (a,b) = break (\q->elem q c) x
          (b':bs) = b

eparse x = split_chars "*()-+/^" $ filter (' '/=) x

simplify [] = []
simplify ("+":"-":ds) = ("-":(simplify ds))
simplify ("(":b:")":ds) = (b:(simplify ds))
simplify ("(":"-":b:")":ds) = ("-":b:(simplify ds))
simplify (x:xs) = x:(simplify xs)

resimple x
    | (length x) == (length sx) = x
    | otherwise = resimple sx
    where sx = simplify x

simple x = resimple x

-- | Year Digits problem -- make as many of the numbers between 1 and 100 using the digits of the current year
-- solve the problem
main :: IO ()
-- ^
main = do
  putStrLn "Enter the year:"
  year <- getLine

  putStrLn "Ordered solution:"
  let solution1 = bestcalc
                 $ sortBy compareSingle
                 $ filter goodcalc
                 $ total_calc
                 $ orderedPairs year 
  mapM_ putStrLn $ map show solution1
  mapM_ putStrLn $ [ concat $ simple $ eparse $ literal s | s <- solution1 ]

  putStrLn "Unordered solution:"
  let solution2 = bestcalc
                 $ sortBy compareSingle
                 $ filter goodcalc
                 $ total_calc
                 $ unorderedPairs year 
  mapM_ putStrLn $ map show solution2
  mapM_ putStrLn $ [ concat $ simple $ eparse $ literal s | s <- solution2 ]
