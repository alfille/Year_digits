import Data.List

uniq [] = []
uniq (x:xs)
    | elem x xs = rest
    | otherwise = x:rest
    where rest = uniq xs

splode x = [ [l] | l <- x ]

cperm x = case (length x) of
    0 -> [[]]
    1 -> [x]
    2 -> [[x1,x2],[x3]]
    _ -> cperm (x3:xs) ++ ( map ([x1]++) $ cperm (x2:xs) )
    where (x1:x2:xs) = x
          x3 = x1++x2

-- only in order
shortlist :: String -> [[String]]
shortlist x = cperm $ splode x 

-- any order
longlist :: String -> [[String]]
longlist x = foldl1 (++) $ map cperm $ uniq $ permutations $ splode x 

data Operation =
        UnaryOp String (Integer -> Maybe Integer) |
        BinaryOp String (Integer -> Integer -> Maybe Integer)


factorial n
    | n<2 = 1
    | otherwise = n * factorial (n-1)

-- replace only first occurrence of '_' with v
sub_str [] _ = []
sub_str ('_':xs) v = v++xs
sub_str (x:xs) v = (x:(sub_str xs v))
    
sub_op ( UnaryOp n f ) (v:vs) = sub_str n v
sub_op ( BinaryOp n f ) (v:w:vs) = sub_str (sub_str n v) w

divvy_up x = case len of
    0 -> [[]]
    1 -> [[x]]
    _ -> [ [fst u, snd u] | u <- [ splitAt n x | n <- [ 1 .. (len-1) ] ] ]
    where len = length x

data Expression =
            Invalid String | -- String is explanation
            Single Integer String | -- equation and result of calculation
            Pair Expression Expression  -- pair of expressions ready for binary operation
            deriving(Show)

-- Creates all possible pairs of a list (for binary operators)
-- i.e. [a,b,c] -> (a,(b,c) + ((a,b),c)
pair_up :: [String] -> [Expression]
pair_up xs = case len of
    0 -> [Invalid "No input"]
    1 -> [Single (read x1::Integer) x1 ] -- Also interpret literal to numeric
    _ -> [Pair f s | w<-[ (pair_up $ fst u, pair_up $ snd u) | u <- [ splitAt n xs | n <- [ 1 .. (len-1) ] ] ], f<-fst w, s <-snd w]
    where len = length xs
          (x1:xx) = xs 

op1List = [
    UnaryOp "_" (\x->Just (x) )
    ,UnaryOp "(-_)" (\x->Just (-x) )
--    ,UnaryOp "_!" (\x-> if (x<0) || (x>10) then Nothing else Just (factorial x) )
    ]

op2List = [
    BinaryOp "(_+_)" (\x y->Just (x+y) )
--    ,BinaryOp "(_-_)" (\x y->if y<0 then Nothing else Just (x-y) )
    ,BinaryOp "(_*_)" (\x y->Just (x*y) )
    ,BinaryOp "(_/_)" (\x y->if (y==0) || ((mod x y)==0) then Nothing else Just (div x y) )
    ,BinaryOp "(_^_)" (\x y->if y<=0 then Nothing else Just (x^y) )
    ,BinaryOp "(_ mod _)" (\x y->if (y<1) then Nothing else Just (mod x y) )
    ]

-- Single and op to calculated Single
unary_calc :: Expression -> Operation -> Expression
unary_calc (Invalid s) _ = Invalid s
unary_calc (Single i1 s1) (UnaryOp s f) = case c of
    Nothing -> Invalid sout
    Just(i) -> Single i sout 
    where sout = sub_str s s1
          c =  f i1

-- Two Singletons and Op to calculated Single
binary_calc :: Expression -> Expression -> Operation -> Expression
binary_calc (Invalid s) _ _ = Invalid s
binary_calc _ (Invalid s) _ = Invalid s
binary_calc (Single i1 s1) (Single i2 s2) (BinaryOp s f) = case c of
    Nothing -> Invalid sout
    Just(i) -> Single i sout
    where sout = sub_str (sub_str s s1) s2
          c =  f i1 i2

unary_multicalc :: Expression -> [Expression]
unary_multicalc (Invalid s) = [Invalid s]
unary_multicalc sing = [ unary_calc sing op | op <- op1List ] 

binary_multicalc :: Expression -> [Expression]
binary_multicalc (Invalid s) = [Invalid s]
binary_multicalc p@(Single i s) = unary_multicalc p
binary_multicalc (Pair p1 p2) = [
            unary_calc (binary_calc pp1 pp2 o2) o1 |
                pp1 <- (binary_multicalc p1),
                pp2 <- (binary_multicalc p2),
                o2 <- op2List,
                o1 <- op1List ]  

total_calc :: [Expression] -> [Expression]
total_calc es = foldr1 (++) [ binary_multicalc e | e <- es ]

-- literal number -> digits -> (-> optional permutations) -> combinations of digits -> All binary pairs in an Expression
-- returns [Expression]
shortpair_up x = foldl1 (++) $ fmap pair_up $ shortlist x
longpair_up  x = foldl1 (++) $ fmap pair_up $ longlist  x

-- filter for solutions to restring to 1 .. 100
goodcalc (Invalid _) = False
goodcalc (Single i _) = (i>0) && (i<101)

-- sorting comparison value, then string length
compareSingle (Single i1 s1) (Single i2 s2)
    | i1 < i2 = LT
    | i1 > i2 = GT
    | otherwise = compare (length s1) (length s2)

bestcalc :: [Expression] -> [Expression]
-- take only first value from the sorted list
bestcalc [] = []
bestcalc [e] = [e]
bestcalc e@(e1@(Single i1 _):e2@(Single i2 _):es)
    | i1==i2 = bestcalc (e1:es)
    | otherwise = (e1:(bestcalc (e2:es)))
