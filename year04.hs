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

-- | cperm (aka character permute) creates a list of all possible concatination of letters in order
-- e.g. cperm ["1","2"] = [["1","2"],["12"]]
-- no leading 0
cperm :: [String] -> [[String]]
-- ^
cperm xs =
    filter (\x -> all nolead0 x) $ cperm' xs
    where
        nolead0 "0" = True
        nolead0 ('0':xs) = False
        nolead0 _ = True
        cperm' x = case (length x) of
            0 -> [[]]
            1 -> [x]
            2 -> [[x1,x2],[x3]]
            _ -> cperm' (x3:xs) ++ ( map ([x1]++) $ cperm' (x2:xs) )
            where (x1:x2:xs) = x
                  x3 = x1++x2

-- | makeSolutionList -- [ordered solutions,unordered solutions]
-- Strategy should be explained:
-- Split year in separate digits
-- make a list 0. the digits in order, 1. all the unordered permutations (unique)
-- make cperm assortment with digits combined or separate
-- make all possible pairings
-- place all possible operators for pairs and +/- for each term
-- calculate
-- exclude bad values
-- assigned a flag for ordered/unordered and recombine
-- sort for value, # operators and sorted
-- choose best of each
-- return that list
makeSolutionList :: String -> [Expression]
-- ^
makeSolutionList year = bestcalc $ sortBy compareSingle rawConcat
    where rawSolve = fmap ((filter goodcalc) . total_calc . (foldr1 (++)) . (fmap pair_up) ) $ makeRawLists year
          rawConcat = [ s{ outorder = False } | s <- (rawSolve!!0) ] ++ (rawSolve!!1)
          compareSingle s1 s2 = compare (value s1, operators s1, outorder s1) (value s2, operators s2, outorder s2)
          -- filter out Invalids and values out of range
          goodcalc (Invalid _) = False
          goodcalc (Single {value=i}) = (i>0) && (i<101)
          -- take only first value from the sorted list
          bestcalc [] = []
          bestcalc [e] = [e]
          bestcalc e@(e1@(Single {value=i1}):e2@(Single {value=i2}):es)
              | i1==i2 = bestcalc (e1:es)
              | otherwise = (e1:(bestcalc (e2:es)))
          makeRawLists year = [ cperm ordered, foldr1 (++) $ map cperm $ filter (/=ordered) $ map head . group . sort $ permutations ordered]
              where ordered = [[y]|y<-year]

-- | Operation type holds information on unitary and binary operations
data Operation =
        UnaryOp (Equation->Equation) (Integer -> Maybe Integer) |
        BinaryOp (Equation->Equation->Equation) (Integer -> Integer -> Maybe Integer)
-- ^

{-
-- factorial optional 
factorial n
    | n<2 = 1
    | otherwise = n * factorial (n-1)
-}

-- | Equation holds the calculation in a form easy to display
-- ^
data Equation =
        EqNull
        | EqTerm Integer
        | EqFactorial Equation
        | EqSum [Equation]
        | EqProduct [Equation]
        | EqDiv { numerator :: Equation, denominator :: Equation }
        | EqExp { neg:: Bool, base :: Equation, power :: Equation }

instance Show Equation where
    show (EqNull) = "blank"
    show (EqTerm i) = show( i )
    show (EqExp{ neg=m, base=b, power=p}) = (if m then "-" else "") ++ (subshow b) ++ "^" ++ (subshow p)
      where
        subshow e@(EqTerm i) = show e
        subshow e@(EqExp {}) = show e
        subshow e = "(" ++ (show e) ++ ")"
    show (EqSum es) = pm $ intercalate "+" [show e | e <- es]
      where
        pm ('+':'-':xs) = "-" ++ (pm xs)
        pm (x:xs) = x:(pm xs)
        pm x = x
    show (EqDiv{ numerator=n, denominator=d}) = ( subshow n ) ++ "/" ++ ( subshow d ) 
      where
        subshow e@(EqTerm i) = show e
        subshow e@(EqExp {}) = show e
        subshow e = "(" ++ (show e) ++ ")"
    show (EqProduct es) = intercalate "*" [subshow e | e <- es]
      where
        subshow e@(EqTerm i) = show e
        subshow e@(EqExp {}) = show e
        subshow e = "(" ++ (show e) ++ ")"
    show (EqFactorial e) = (subshow e) ++ "!"
      where
        subshow e@(EqTerm i) = show e
        subshow e = "(" ++ (show e) ++ ")"


eqneg :: Equation -> Equation
eqneg (EqTerm i) = EqTerm (-i)
eqneg (EqExp{neg=m, base=b,power=p}) = EqExp {neg=(not m), base=b,power=p}
eqneg (EqProduct (e:es)) = EqProduct $ (eqneg e):es
eqneg (EqDiv {numerator=n, denominator=d}) = EqDiv {numerator=(eqneg n),denominator=d}
eqneg (EqSum e) = EqSum [eqneg ee| ee <- e]

eqnegtest (EqTerm i) = i<0
eqnegtest (EqExp{neg=m}) = m
eqnegtest (EqProduct (e:es)) = eqnegtest e
eqnegtest (EqDiv {numerator=n}) = eqnegtest n
eqnegtest e = False

eqadd (EqSum e1) (EqSum e2) = EqSum (e1++e2)
eqadd (EqSum e1) e2 = EqSum (e1++[e2])
eqadd e1 (EqSum e2) = EqSum ([e1]++e2)
eqadd e1 e2 = EqSum [e1,e2]

eqdiv n d
    | eqnegtest d = EqDiv { numerator=(eqneg n), denominator=(eqneg d) }
    | otherwise = EqDiv { numerator=n, denominator=d }

eqexp b p = EqExp { neg=False, base=b, power=p }

eqmult e1@(EqProduct es1) e2@(EqProduct es2)
    | eqnegtest e2 = eqmult (eqneg e1) (eqneg e2)
    | otherwise = EqProduct $ es1++es2
eqmult e1@(EqProduct es1) e2
    | eqnegtest e2 = eqmult (eqneg e1) (eqneg e2)
    | otherwise = EqProduct $ es1++[e2]
eqmult e1 e2@(EqProduct es2)
    | eqnegtest e2 = eqmult (eqneg e1) (eqneg e2)
    | otherwise = EqProduct $ [e1]++es2
eqmult e1 e2
    | eqnegtest e2 = EqProduct [eqneg e1,eqneg e2]
    | otherwise = EqProduct [e1,e2]

eqfact e = EqFactorial e


-- | Expression type holds intermediate and final equation information
-- includes value and String explaining calculation to date
-- Also has Invalid type for redundant expressions or divide-by-zero
data Expression =
            Invalid Equation | -- String is explanation
            Single { value :: Integer
            , operators :: Int
            , equation :: Equation
            , outorder :: Bool
            }| -- equation and result of calculation: value, ops, equation_text
            Pair Expression Expression  -- pair of expressions ready for binary operation
-- ^

instance Show (Expression) where
    show (Invalid e) = "Invalid | eqn= " ++ (show e)
    show (Single { value=i, operators=o, equation=e, outorder=r }) = "Value= " ++ (show i) ++ " |ops= " ++ (show o) ++ " |order= " ++ (show $ not r) ++ " | eqn= " ++ (show e)
    show (Pair e1 e2) = show (e1,e2)

-- | pair_up creates all possible pairs of a list (for binary operators)
-- i.e. [a,b,c] -> (a,(b,c) + ((a,b),c)
-- results wrapped up in Expression types
pair_up :: [String] -> [Expression]
-- ^
pair_up xs = case len of
    0 -> [Invalid EqNull]
    1 -> [Single {value=i, operators=0, equation=(EqTerm i), outorder=True} ] -- Also interpret equation to numeric
    _ -> [Pair f s | w<-[ (pair_up $ fst u, pair_up $ snd u) | u <- [ splitAt n xs | n <- [ 1 .. (len-1) ] ] ], f<-fst w, s <-snd w]
    where len = length xs
          (x1:xx) = xs
          i = read x1 :: Integer 

-- | operator lists, unary and binary
-- minus removed since x-y == x+(-y) so would be double counted
-- Exponentiation 
unaryList = [
    UnaryOp id (\x->Just (x) )
    ,UnaryOp eqneg (\x->Just (-x) )
--    ,UnaryOp eqfact (\x-> if (x<3) || (x>9) then Nothing else Just (factorial x) )
    ]

logMax = (log $ fromIntegral (maxBound::Int)) /4

binaryList = [
    BinaryOp eqadd (\x y->Just (x+y) )
    ,BinaryOp eqmult (\x y->Just (x*y) )
    ,BinaryOp eqdiv (\x y->if (y==0) || ((mod x y)/=0) then Nothing else Just (div x y) )
    ,BinaryOp eqexp (\x y->
        if (y<0) || (x<0)   then Nothing else
        if (x==0) && (y==0) then Nothing else
        if (x==1) || (y<15) then Just(x^y) else
        Nothing )
--    ,BinaryOp "(_ mod _)" (\x y->if (y<1) then Nothing else Just (mod x y) )
    ]

-- | unary_calc apply unitary operator
-- Single and op to calculated Single
unary_calc :: Expression -> Operation -> Expression
-- ^
unary_calc (Invalid s) _ = Invalid s
unary_calc single@(Single {value=i1, operators=o1, equation=s1}) (UnaryOp eqf f) = case c of
    Nothing -> Invalid sout
    Just(i) -> single { value=i, equation=sout} 
    where sout = eqf s1
          c =  f i1
-- | binary_calc apply an operation to a pair of values
-- Two Singletons and Op to calculated Single
binary_calc :: Expression -> Expression -> Operation -> Expression
-- ^
binary_calc (Invalid s) _ _ = Invalid s
binary_calc _ (Invalid s) _ = Invalid s
binary_calc (Single {value=i1, operators=o1, equation=s1}) (Single {value=i2, operators=o2, equation=s2}) (BinaryOp eqf f) = case c of
    Nothing -> Invalid sout
    Just(i) -> Single { value=i, operators=o1+o2+1, equation=sout, outorder=True}
    where sout = eqf s1 s2
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

-- | Year Digits problem -- make as many of the numbers between 1 and 100 using the digits of the current year
-- solve the problem
main :: IO ()
-- ^
main = do
  putStrLn "Find equations for 1 to 100 using a year's digits"
  putStrLn "Enter the year:"
  year <- getLine
  let solution = makeSolutionList year

  putStrLn $ "  Total covered = " ++ (show $ length solution)
  mapM_ putStrLn $ map show $ solution

range = do
  putStrLn "Show how many numbers from 1 to 100 can be found for each year in range"
  putStrLn "Enter the starting year:"
  year1 <- getLine
  putStrLn "Enter the finishing year:"
  year2 <- getLine
  mapM_ putStrLn $ [ (show y) ++ ", " ++ (show $ length $ makeSolutionList $ show y) | y <- [(read year1 :: Int) .. (read year2 :: Int)] ]
  
