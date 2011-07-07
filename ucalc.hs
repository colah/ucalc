
--Parsec is a super awesome parsing library!
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
-- Some convenience stuff
import Control.Monad
import Data.List
-- For UI
import System.Console.Readline
import Codec.Binary.UTF8.String

--Some convenience functions, self explanatory

sortByLength :: [String] -> [String]
sortByLength l = reverse $ sortBy 
	(\a b -> compare (length a) (length b))
	l

sortByLength1 :: [(String, a)] -> [(String, a)]
sortByLength1 l = reverse $ sortBy 
	(\a b -> compare (length (fst a)) (length (fst b)))
	l

listOneOf :: [String] -> GenParser Char st String
listOneOf [] = pzero
listOneOf (x:xs) = try (string x) <|> listOneOf xs

listOneOfT :: [(String, a)] -> GenParser Char st a
listOneOfT [] = pzero
listOneOfT (x:xs) = try (string (fst x) >> return (snd x)) 
		    <|> listOneOfT xs

listOneOfB :: [(String, a)] -> GenParser Char st (String, a)
listOneOfB [] = pzero
listOneOfB (x:xs) = try (string (fst x) >> return x) 
		    <|> listOneOfB xs
fNaN = sqrt(-1)

-- Our internal numeric data type. Implement show, provide courcing to float, simplification, use ##operator for internal operators. Rather messy, I know.

data InNumeric = InRational Integer Integer 
		| InFloat Float 
		| InSymb String Float 
		| InUnary String InNumeric
		| InBinary String InNumeric InNumeric
		| InMulti String [InNumeric]

subExprShow (InMulti "+" l) = "(" ++ (show (InMulti "+" l) ) ++ ")"
subExprShow a = show a

instance Show InNumeric where
	show (InFloat a) = show a
	show (InRational a b) = let (c,d) = (div a (gcd a b), div b (gcd a b) ) in
		case (c,d) of 
			(_,1) -> show c
			(1,2) -> "½"
			(1,3) -> "⅓"
			(1,4) -> "¼"
			(1,5) -> "⅕"
			(1,6) -> "⅙"
			(1,8) -> "⅛"
			(2,3) -> "⅔"
			(2,5) -> "⅖"
			(3,4) -> "¾"
			(_,_) -> (show c) ++ "/" ++ (show d)
				--if (c `div` d >= 1) 
				--then (show $ c `div` d) ++ " + " ++ (show $ InRational (c `mod` d) d) 
				--else (show c) ++ "/" ++ (show d)
	show (InSymb s n) = s
	show (InUnary "abs" n) = "|" ++ (show n) ++ "|"
	show (InUnary "sqrt" n) = "√(" ++ (show n) ++ ")"
	show (InUnary "recip" n) = (subExprShow n) ++ "⁻¹"
	show (InUnary s n) = s ++ "(" ++ (show n) ++ ")"
	show (InMulti "+" l) = 	show (InMulti " + " (filter (\s -> (show s) /= "0") l))
	show (InMulti "*" ((InRational a b):l)) = if (show $ InRational a b) == "1" then show $ InMulti "*" l else
		 (show $ InRational a b) ++ (show $ InMulti "*" l)
	show (InMulti "*" l) = 	 foldl1 (\a b -> a ++ "*" ++ b) $ map subExprShow $(filter (\s -> (show s) /= "1") l)
	show (InMulti s l) = foldl1 (\a b -> a ++ s ++ b) $ map subExprShow l
	show (InBinary "^" a b) = let
			charToSup c = case c of
				'0' -> '⁰'
				'1' -> '¹'
				'2' -> '²'
				'3' -> '³'
				'4' -> '⁴'
				'5' -> '⁵'
				'6' -> '⁶'
				'7' -> '⁷'
				'8' -> '⁸'
				'9' -> '⁹'
				'-' -> '⁻'
			toSup :: String -> String
			toSup [] = []
			toSup (c:cs) = (charToSup c):(toSup cs)
		in (show a) ++ case b of
			InRational n 1 -> toSup $ show n
			a -> "^(" ++ (show a) ++ ")"
	show (InBinary s a b) = (show a) ++ s ++ (show b)

instance Eq InNumeric where
	InFloat a == InFloat b = a == b
	InRational a b == InRational c d = a*d == b*c
	InSymb s1 n1 == InSymb s2 n2 = s1 == s2 && n1 == n2
	InUnary s1 n1 == InUnary s2 n2 = s1 == s2 && n1 == n2
	InMulti s1 l1 == InMulti s2 l2 = s1 == s2 && l1 == l2



inNumericToFloat :: InNumeric -> Float
inNumericToFloat (InRational a b) = (fromInteger a :: Float)/(fromInteger b :: Float)
inNumericToFloat (InFloat a)      = a
inNumericToFloat (InSymb s n)      = n
inNumericToFloat (InUnary "abs" a)= abs $ inNumericToFloat a
inNumericToFloat (InUnary "cos" a)= cos $ inNumericToFloat a
inNumericToFloat (InUnary "sin" a)= sin $ inNumericToFloat a
inNumericToFloat (InUnary "tan" a)= tan $ inNumericToFloat a
inNumericToFloat (InUnary "sqrt" a)= sqrt $ inNumericToFloat a
inNumericToFloat (InUnary _ a)    = fNaN
inNumericToFloat (InBinary "^" a b)    = (inNumericToFloat a) ** (inNumericToFloat b)
inNumericToFloat (InMulti "+" l)  = sum $ map inNumericToFloat l
inNumericToFloat (InMulti "*" l)  = product $ map inNumericToFloat l
inNumericToFloat (InMulti _ l)    = fNaN


inNumAbs :: InNumeric -> InNumeric
inNumAbs (InFloat a) = InFloat $ abs a
inNumAbs (InRational a b) = InRational (abs a) (abs b)
inNumAbs a = InUnary "abs" a


inNeg :: InNumeric -> InNumeric
inNeg a = (InRational (-1) 1) ##* a

splitList f l = (filter f l, filter (not . f) l)

numSimplify (InMulti "+" list) = 
	InMulti "+"  $
	(let 
		sameButOrder :: Eq a => [a] -> [a] -> Bool
		sameButOrder [] [] = True
		sameButOrder a []  = False
		sameButOrder [] a  = False
		sameButOrder (x:xs) y = let ((a,b),(c,d)) = (splitList (==x) (x:xs), splitList (==x) y) in
			length a == length c && sameButOrder b d
		collectLike :: InNumeric -> [InNumeric] -> (InNumeric,[InNumeric])
		collectLike n [] = (n, [])
		collectLike n (x:xs) = case (n,x) of
			((InMulti "*" ((InRational a b):xs1)), (InMulti "*" ((InRational c d):xs2)))
				-> if sameButOrder xs1 xs2 then 
				      collectLike (InMulti "*" ((InRational a b ##+ (InRational c d)):xs1)) xs
				   else 
				      ((fst $ collectLike n xs), x:(snd $ collectLike n xs))
			((InMulti "*" ((InRational a b):xs1)),(InMulti "*" xs2))
				-> if sameButOrder xs1 xs2 then 
				      collectLike (InMulti "*" ((InRational a b ##+ (InRational 1 1)):xs1)) xs
				   else 
				      ((fst $ collectLike n xs), x:(snd $ collectLike n xs))
			((InMulti "*" xs1),(InMulti "*" ((InRational c d):xs2)))
				-> if sameButOrder xs1 xs2 then 
				      collectLike (InMulti "*" ((InRational 1 1 ##+ (InRational c d)):xs1)) xs
				   else 
				      ((fst $ collectLike n xs), x:(snd $ collectLike n xs))
			(_,_) ->  ((fst $ collectLike n xs), x:(snd $ collectLike n xs))
		collectLikeTerms :: [InNumeric] -> [InNumeric]
		collectLikeTerms [] = []
		collectLikeTerms (x:xs) = case collectLike x xs of
			(n,[]) -> [n]
			(n,l) -> n:(collectLikeTerms l)
	in
		collectLikeTerms
	) .
	(let symb2mult n = case n of 
		InSymb s m -> (InRational 1 1) ##* (InSymb s m)
		InUnary s m -> (InRational 1 1) ##* (InUnary s m)
		_ -> n
	in
		\l -> case splitList 
		(\n -> case n of 
			(InRational _ _) -> True
			_ -> False
		) l
	of
		([], _) ->  (map (numSimplify . symb2mult) l)
		(a,  b) -> ([foldl1 (##+) a] ++ (map (numSimplify . symb2mult) b))

	)  $ list


numSimplify (InMulti "*" l) =  
	case splitList (\n -> case n of 
		(InRational _ _) -> True
		_ -> False
	) l
	of
		([], _) -> InMulti "*" (map numSimplify l)
		(a,  b) -> InMulti "*" ([foldl1 (##*) a] ++ (map numSimplify b))

numSimplify (InUnary "cos" n) = case (numSimplify n) of
	InSymb "τ" _ -> InRational 1 1
	InMulti "*" [InRational a b, InSymb "τ" _] -> case (a `mod` b, b) of
		(0,1) -> InRational 1 1
		(1,1) -> InRational 1 1
		(1,2) -> InRational (-1) 1
		(1,4) -> InRational 0 1
		(3,4) -> InRational 0 1
		_     -> InUnary "cos" (numSimplify n)
	_ -> InUnary "cos" (numSimplify n)
numSimplify (InUnary "sin" n) = case (numSimplify n) of
	InSymb "τ" _ -> InRational 1 1
	InMulti "*" [InRational a b, InSymb "τ" _] -> case (a `mod` b, b) of
		(0,1) -> InRational 0 1
		(1,1) -> InRational 0 1
		(1,2) -> InRational 0 1
		(1,4) -> InRational 1 1
		(3,4) -> InRational (-1) 1
		_ -> InUnary "sin" (numSimplify n)
	_ -> InUnary "sin" (numSimplify n)
numSimplify (InUnary "tan" n) = case (numSimplify n) of
	InSymb "τ" _ -> InRational 1 1
	InMulti "*" [InRational a b, InSymb "τ" _] -> case (a `mod` b, b) of
		(0,1) -> InRational 0 1
		(1,1) -> InRational 0 1
		(1,2) -> InRational 0 1
		(1,4) -> InRational 1 1
		(3,4) -> InRational (-1) 1
		_ -> InUnary "sin" (numSimplify n)
	_ -> InUnary "sin" (numSimplify n)
numSimplify (InUnary "sqrt" n) =
	let 
		factorByList 0 _ = [0]
		factorByList n [] = [n]
		factorByList n (p:ps) = 
			if n `mod` p == 0 then p:(factorByList (div n p) (p:ps))
			else factorByList n ps
		babyFactor = \n -> factorByList n [2,3,5,7,11,13,17,19,23,27,31]
		sortNths n [] = ([],[])
		sortNths n (x:xs) = let (a,b) = splitList (==x) (x:xs) in
			([x | m <- [0.. (div (length a) n) - 1]] ++ (fst $ sortNths n b),
			 [x | m <- [0.. (mod (length a) n) - 1]] ++ (snd $ sortNths n b))
	in
		case (numSimplify n) of
			InRational a b -> let ((a1,a2),(b1,b2)) = ((sortNths 2 $ babyFactor a),(sortNths 2 $ babyFactor b)) 
				in if (product a2)== 1 && (product b2) == 1 then InRational (product a1) (product b1) else
					InRational (product a1) (product b1) 
					##* InUnary "sqrt" (InRational (product a2) (product b2))
			_ -> InUnary "sqrt" (numSimplify n)
				
numSimplify (InUnary s n) = InUnary s (numSimplify n)


numSimplify a = a

(##+) :: InNumeric -> InNumeric -> InNumeric
InFloat a   ##+   b     = InFloat (a + (inNumericToFloat b))
a   ##+   InFloat b     = InFloat ((inNumericToFloat a) + b)
InRational a1 a2 ##+ InRational b1 b2 = InRational (a1*b2 + b1*a2) (a2*b2)
(InMulti "+" a)  ##+  (InMulti "+" b) = InMulti "+" (a ++ b)
(InMulti "+" a)  ##+  b = InMulti "+" (a ++ [b])
a  ##+  (InMulti "+" b) = InMulti "+" ([a] ++ b)
a ##+ b = InMulti "+" [a, b]


(##-) :: InNumeric -> InNumeric -> InNumeric
a ##- b = a ##+ (inNeg b)

(##*) :: InNumeric -> InNumeric -> InNumeric
InFloat a ##* b = InFloat (a * (inNumericToFloat b))
a ##* InFloat b = InFloat ((inNumericToFloat a) * b)
InRational a1 a2 ##* InRational b1 b2 = InRational (a1*b1) (a2*b2)
(InMulti "+" a)  ##*  (InMulti "+" b) = InMulti "+" (do{ c <- a; d <- b; return $ c ##* d;})
(InMulti "+" a)  ##*  b = InMulti "+" (map (##* b) a )
a  ##*  (InMulti "+" b) = InMulti "+" (map (a ##*) b)
(InMulti "*" a)  ##*  (InMulti "*" b) = InMulti "*" (a ++ b)
(InMulti "*" a)  ##*  b = InMulti "*" (a ++ [b])
a  ##*  (InMulti "*" b) = InMulti "*" ([a] ++ b)
a ##* b = InMulti "*" [a, b]

(##/) :: InNumeric -> InNumeric -> InNumeric
InFloat a ##/ b = InFloat (a / (inNumericToFloat b))
a ##/ InFloat b = InFloat ((inNumericToFloat a) / b)
InRational a1 a2 ##/ InRational b1 b2 = InRational (a1*b2) (a2*b1)
a ##/ InRational b1 b2 = a ##* InRational b2 b1
a ##/ b = a ##* InFloat (1/(inNumericToFloat b))

(##^) :: InNumeric -> InNumeric -> InNumeric
InFloat a ##^ b = InFloat (a ** (inNumericToFloat b))
a ##^ InFloat b = InFloat ((inNumericToFloat a) ** b)
InMulti "*" l ##^ b = InMulti "*" (map (##^ b) l)
InRational a1 a2 ##^ InRational b1 b2 = case (b1 `div` (gcd b1 b2), b2 `div` (gcd b1 b2))  of
	(n, 1) -> InRational (a1^n) (a2^n)
	(_,_) -> (InRational a1 a2) ##^ (InFloat . inNumericToFloat $ (InRational b1 b2))
a ##^ InRational b1 b2 = case (b1 `div` (gcd b1 b2), b2 `div` (gcd b1 b2)) of
	(n, 1) -> if n > 0 then foldl1 (##*) [a| m <- [1,2..n]] else
			if n == 0 then InRational 1 1
			else InRational 1 1 ##/ InMulti "*" [a| m <- [1,2..n]]
	(_,_) -> a ##^ (InFloat . inNumericToFloat $ (InRational b1 b2))



-- More generic internal objects.

data InObj = InNum InNumeric | InVec3 (InObj, InObj, InObj) | Error String

instance Show InObj where
	show (InNum a) = show a
	show (InVec3 a) = show a
	show (Error a) = "error:" ++ a

errorPass :: InObj -> String -> InObj
errorPass (Error a) msg = Error a
errorPass _ msg = Error msg

errorPass2 :: InObj -> InObj -> String -> InObj
errorPass2 (Error a) b msg = Error a
errorPass2 a (Error b) msg = Error b
errorPass2 _ _ msg = Error msg

(#+) :: InObj -> InObj -> InObj
InNum a #+ InNum b = InNum (a##+b)
InVec3 (a1, a2, a3) #+ InVec3 (b1, b2, b3) = InVec3 (a1 #+ b1, a2 #+ b2, a3 #+ b3)
a #+ b = errorPass2 a b "Invalid Types for Addition"

(#-) :: InObj -> InObj -> InObj
InNum a #- InNum b = InNum (a##-b)
InVec3 (a1, a2, a3) #- InVec3 (b1, b2, b3) = InVec3 (a1 #- b1, a2 #- b2, a3 #- b3)
a #- b = errorPass2 a b "Invalid Types for Subtraction"

(#*) :: InObj -> InObj -> InObj
InNum a #* InNum b = InNum (a##*b)
InNum a #* InVec3 (b1, b2, b3) = InVec3 ((InNum a)#*b1, (InNum a)#*b2, (InNum a)#*b3)
a #* b = errorPass2 a b "Invalid Types for Mupltiplication"

(#/) :: InObj -> InObj -> InObj
InNum a #/ InNum b = InNum (a##/b)
InVec3 (a1, a2, a3) #/ InNum b = InVec3 (a1 #/ (InNum b), a2 #/ (InNum b), a3 #/ (InNum b))
a #/ b = errorPass2 a b "Invalid Types for Division"

(#^) :: InObj -> InObj -> InObj
InNum a #^ InNum b = InNum (a##^b)
a #^ b = errorPass2 a b "Invalid Types for exponentiation"

(#⋅) :: InObj -> InObj -> InObj
InVec3 (a1, a2, a3) #⋅ InVec3 (b1, b2, b3) = (a1 #* b1) #+ (a2 #* b2) #+ (a3 #* b3)
a #⋅ b = errorPass2 a b"Invalid Types for dot product"

(#×) :: InObj -> InObj -> InObj
InVec3 (a1, a2, a3) #× InVec3 (b1, b2, b3) = InVec3 (a2 #* b3 #- b2 #* a3, a3 #* b1 #- b3 #* a1, a1 #* b2 #- b1 #* a2)
a #× b = errorPass2 a b "Invalid Types for cross product"

wrapFloatFunc f (InNum a) = InNum . InFloat . f . inNumericToFloat $ a
wrapFloatFunc f (Error a) = Error a
wrapFloatFunc f _ = Error "Invalid type for real valued function"

inAbs :: InObj -> InObj
inAbs (InNum a) = InNum $ inNumAbs a
inAbs (InVec3 a) = wrapFloatFunc sqrt ((InVec3 a) #⋅ (InVec3 a))
inAbs a = errorPass a "Bad type for absolute value/norm"

-- Onto the parser!

number :: GenParser Char st InObj
number = try (do {
		a <- (many1 digit);
		char '.';
		b <- (many1 digit);
		return $ InNum $ InFloat ( read (a ++ "." ++ b) :: Float);
	})
	<|> (do { n <- many1 digit; return $ InNum $ InRational (read n) 1})
	<|> (char 'π' >> return (InNum $ (InRational 1 2) ##* (InSymb "τ" (2*pi::Float))))
	<|> (char 'τ' >> return (InNum $ InSymb "τ" (2*pi::Float)))
	<|> (char 'e' >> return (InNum $ InSymb "e" (exp 1 :: Float) ))
	<|> (char '½' >> return (InNum $ InRational 1 2))
	<|> (char '⅓' >> return (InNum $ InRational 1 3))
	<|> (char '¼' >> return (InNum $ InRational 1 4))
	<|> (char '⅕' >> return (InNum $ InRational 1 5))
	<|> (char '⅔' >> return (InNum $ InRational 2 3))
	<|> (char '⅖' >> return (InNum $ InRational 2 5))
	<|> (char '¾' >> return (InNum $ InRational 3 4))

supDigit :: GenParser Char st Char
supDigit = do 
		c <- oneOf "⁰¹²³⁴⁵⁶⁷⁸⁹"
		return $ case c of
			'⁰' -> '0'
			'¹' -> '1'
			'²' -> '2'
			'³' -> '3'
			'⁴' -> '4'
			'⁵' -> '5'
			'⁶' -> '6'
			'⁷' -> '7'
			'⁸' -> '8'
			'⁹' -> '9'

supInt :: GenParser Char st Integer
supInt = liftM read (many1 supDigit)

subDigit :: GenParser Char st Char
subDigit = do 
		c <- oneOf "₀₁₂₃₄₅₆₇₈₉"
		return $ case c of
			'₀' -> '0'
			'₁' -> '1'
			'₂' -> '2'
			'₃' -> '3'
			'₄' -> '4'
			'₅' -> '5'
			'₆' -> '6'
			'₇' -> '7'
			'₈' -> '8'
			'₉' -> '9'

subInt :: GenParser Char st Integer
subInt = liftM read (many1 subDigit)


vector :: GenParser Char st InObj
vector = try (do {
		char '(';
		a1 <- expr;
		char ',';
		a2 <- expr;
		char ')';
		return $ InVec3 (a1, a2, (InNum $ InRational 0 1))
	}) <|> (do {
		char '(';
		a1 <- expr;
		char ',';
		a2 <- expr;
		char ',';
		a3 <- expr;
		char ')';
		return $ InVec3 (a1, a2, a3)
	}) 


bracketExpr :: GenParser Char st InObj
bracketExpr =  
	do { 
		char '(';
		x <- expr;
		char ')';
		return x;
	}

function :: GenParser Char st (InObj -> InObj)
function = try (do {
		string "log";
		base <- subInt; 
		return $ wrapFloatFunc (logBase $ fromInteger base)
	})
	<|> (listOneOfT . sortByLength1 . (map (\ (a,b) -> (a, wrapFloatFunc b) ) )$
		[("sin", sin),
		 ("cos", cos),
		 ("tan", tan),
		 ("sqrt", sqrt),
		 ("log", log)]
	)
		

factor :: GenParser Char st InObj
factor =    try vector
	<|> try bracketExpr
	<|> try (do{ 
		string "cos";
		x <- bracketExpr; 
		return (case x of {
			InNum a -> (InNum ( InUnary "cos" a)); 
			_ -> Error "Bad Type to function";
		});
	})
	<|> try (do{ 
		string "sin";
		x <- bracketExpr; 
		return (case x of {
			InNum a -> (InNum ( InUnary "sin" a)); 
			_ -> Error "Bad Type to function";
		});
	})
	<|> try (do{ 
		string "tan";
		x <- bracketExpr; 
		return (case x of {
			InNum a -> (InNum ( InUnary "tan" a)); 
			_ -> Error "Bad Type to function";
		});
	})
	<|> try (do{ 
		string "sqrt";
		x <- bracketExpr; 
		return (case x of {
			InNum a -> (InNum ( InUnary "sqrt" a)); 
			_ -> Error "Bad Type to function";
		});
	})
	<|> try (do{
		a <- function;
		b <-  bracketExpr;
		return $ a (b);
	})
	<|> try (do{
		char '|';
		b <-  factor <|> expr;
		char '|';
		return $ inAbs (b);
	})
	<|> try (do{
		string "||";
		b <-  vector;
		string "||";
		return $ inAbs (b);
	})
	<|> number
	<|> (do { char '-'; n <- factor; return ((InNum $ InRational (-1) 1) #* n);})

factorPlus = try (do {
		a <- factor;
		b <- supInt;
		return (a #^ (InNum $ InRational b 1));
	})
	<|> try (do{
		a <- number;
		b <- (try vector) <|> (try factorPlus) <|> (do { char '('; x <- expr; char ')'; return x;});
		return $ a #* b;
	})
	<|> factor
	<|> (char ' ' >> factorPlus)

table = [ [ op "^" (#^) AssocLeft ],
	  [ op "*" (#*) AssocLeft,
	    op "/" (#/) AssocLeft,
	    op "⋅" (#⋅) AssocLeft,
	    op "×" (#×) AssocLeft ],
	  [ op "+" (#+) AssocLeft,
	    op "-" (#-) AssocLeft ] ]
	where op s f assoc = Infix ( skipMany (char ' ') >> string s >> skipMany (char ' ')  >> return f) assoc

expr :: GenParser Char st InObj
expr = buildExpressionParser table factorPlus 

-- And, finally, UI.

calc :: String -> String
calc s = let n = parse expr "" s in
		case n of
			Right (InNum m) -> case numSimplify m of
				InRational a b -> if b == 1 then (show a) else
					show m ++ " ≅ " ++ (show $ inNumericToFloat m)
				InFloat a -> show a
				k -> show k ++ " ≅ " ++ (show $ inNumericToFloat k)
			Right a -> show a
			Left b -> "parsing error:" ++ (show b)


calcLoop :: IO()
calcLoop = do{
	maybeLine <- readline "==>";
	case maybeLine of
		Nothing -> return ()
		Just "exit" -> return ()
		Just line -> do {addHistory line;
				 putStrLn $ calc $ decodeString line;
				 calcLoop;
				}
	}


main = calcLoop
