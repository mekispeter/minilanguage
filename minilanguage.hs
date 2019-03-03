{-
This program parses and runs programs written in an imperative minilanguage
with very limited syntax. It supports simple control flow, if-then-else and
while-do loops.

Every program has the form 'comm1;comm2;...|vi1,vi2,...', where
'comm1;comm2,...' is a sequence of commands, and vi1, vi2, ... is a list of
variables. Commands are either of three kinds:
- simple variable assignments of the form 'vn = t', where vn is a variable,
  and t is a term;
- while loops of the form 'while(cond)do[seq]', where cond is a condition,
  and seq is a command sequence (parenthising the condition is optional),
- if constructs of the form 'if(cond)then{seq1}else{seq2}', where cond is a
  condition, and seq1 and seq2 are command sequences (parenthising the
  condition is optional).
Conditions consist of two terms related by '=' or '<', optionally
parenthesized. The variable list at the end of the program is comma separated.
Commands in a command sequence are separated by semicolons.

Variable names are nonnegative integer indices prefixed with a v: v0, v1, ....
Terms are arithmetic expressions with integers, variables, and the operations
{+,*,-,/,^}. The only supported data type is integer. Negative integers have
to be bracketed in the program strings: "3*(-2)" is parsed as "3*(0-2)", but
"3*-2" is parsed as "(3*0)-2". '/' is integer division: "7/3" is evaluated as 2.

Terms are parsed in a left-to-right fashion, with standard operator precedence
(^ > *,/ > +,-): "11+9-3*2^3+5" is parsed into the same term as
"((11+(9-(3*(2^3))))+5)".

Programs can be run by calling the 'run' function. Its type is
[Integer] -> String -> [Integer]. The first argument is an integer list
[n0,...,nk] that assigns values to an initial segment of the variable list:
ni is assigned to the variable vi. 'run [1,2] progstr' thus runs progstr with
v0=1 and v1=2. The rest of the variables are assigned zero until changed at
runtime by an assignment command.

The variable list at the end of a program specifies which variables' values
will be printed after execution.

Whitespace (space, tab, line break) is allowed anywhere in the code, and is
ignored.

The language is Turing complete. In principle lists and strings can be
simulated via Gödel encodings, but that is a an unpleasant way to go. In fact,
even if-then-else constructs are redundant; they can be simulated by while
loops (also an unpleasant way to go).

The parser part is ugly. It works, but it has to be rewritten.

ToDo:
  - Boolean operators in conditions
  - more liberal variable naming
  - remarks
  - reading program from file
  - make parser functions less ugly
-}

{-
Add few programs written in the minilanguage
-}

-- n mod m. Eg. for n=27, m=12:
-- > run [27,12] mod'
-- 3
mod' :: String
mod' =
  " v2 = v0 - (v0 / v1) * v1 \n\
  \ | v2"

-- Pythagorean triple checker: 1 for valid triples, 0 otherwise. Eg:
-- run [3,4,5] pyth3ch
-- [1]
pyth3ch :: String
pyth3ch =
  " if v2 * v2 == v0 * v0 + v1 * v1   \n\
  \ then {v3 = 1}                     \n\
  \ else {v3 = 0}                     \n\
  \ | v3"

-- Factorial of n. Eg. for n=7:
-- > run [7] factorial
-- [5040]
factorial :: String
factorial =
  " v1 = 1;                     \n\
  \ v2 = 1;                     \n\
  \ while v1 < v0 + 1 do {      \n\
  \   v2 = v2 * v1;             \n\
  \   v1 = v1 + 1               \n\
  \ }                           \n\
  \ | v2"

-- nth number in the Fibonacci sequence. Eg. for n=10:
-- > run [10] fibonacci
-- [55]
fibonacci :: String
fibonacci =
  " v1 = 1;               \n\
  \ v2 = 0;               \n\
  \ v3 = 1;               \n\
  \ while v1 < v0 do {    \n\
  \   v4 = v2 + v3;       \n\
  \   v2 = v3;            \n\
  \   v3 = v4;            \n\
  \   v1 = v1 + 1         \n\
  \ }                     \n\
  \ | v3"

-- Greatest common divisor of n and m. Eg. for n=10, m=72:
-- > run [120,72] gcd'
-- [24]
gcd' :: String
gcd' =
  " while 0 < v1 do {             \n\
  \   v2 = v0 - (v0 / v1) * v1;   \n\
  \   v0 = v1;                    \n\
  \   v1 = v2                     \n\
  \ }                             \n\
  \ | v0"

-- Number of divisors of n. Eg for n=30:
-- > run [30] divisors
-- [8]
divisors :: String
divisors =
  " v1 = 0;                               \n\
  \ v2 = 1;                               \n\
  \ while v2 <= v0 do {                   \n\
  \   if v0 - (v0 / v2) * v2 == 0         \n\
  \   then {v1 = v1 + 1}                  \n\
  \   else {v1 = v1};                     \n\
  \   v2 = v2 + 1                         \n\
  \ }                                     \n\
  \ | v1"

-- Prime checker: 1 if n is a prime, 0 otherwise. Eg. for n = 2019 and n = 2017:
-- > run [2019] primecheck
-- [0]
-- > run [2017] primecheck
-- [1]
primecheck :: String
primecheck =
  " v1 = 2;                               \n\
  \ v2 = 1;                               \n\
  \ while v1 * v1 <= v0 do {              \n\
  \   if v0 - (v0 / v1) * v1 == 0         \n\
  \   then {v2=0}                         \n\
  \   else {v2 = v2};                     \n\
  \   v1 = v1 + 1                         \n\
  \ }                                     \n\
  \ | v2"

-- This is a test to make sure that the code is not broken and samples run as
-- expected.
test :: Bool
test =
  run [27,12] mod' == [3] &&
  run [3,4,5] pyth3ch == [1] &&
  run [4,5,6] pyth3ch == [0] &&
  run [7] factorial == [5040] &&
  run [10] fibonacci == [55] &&
  run [120,72] gcd' == [24] &&
  run [30] divisors == [8] &&
  run [2019] primecheck == [0] &&
  run [2017] primecheck == [1] &&
  run [] "while(((v0+-1)==2^4*5/(3+2)))do{if(v0<-7)then{v0=1}else{v2=1}}|v0,v1" == [0,0]

{-
Types and atomic constituents
-}

-- Terms are binary trees, with operators at the nodes, and integers at the
-- leaves.
data Term       = VarT Variable | Integ Integer |
                  Add Term Term | Mult Term Term |
                  Subt Term Term | Div Term Term | Pow Term Term
                  deriving (Eq, Show)
type Operator   = Term -> Term -> Term
data Variable   = Var Integer deriving (Eq, Show)
-- Every variable is assigned an integer value. The execution of a command or a
-- sequence of commands transforms one variable assignment into another.
type Assign     = Variable -> Integer
-- Four relations are supported in conditions: ==, <=, <, and !=.
data Condition  = Equ Term Term | Leq Term Term |
                  Less Term Term | Neq Term Term
                  deriving (Eq, Show)
type Relation   = Term -> Term -> Condition
-- Commands are either variable assignments, while-do loops, or if-then-else
-- constructs.
data Command    = Let Variable Term |
                  While Condition Sequence |
                  If Condition Sequence Sequence
                  deriving (Eq, Show)
-- a command sequence is a list of commands, executed sequentially.
type Sequence   = [Command]
-- A program is a pair of a command sequence and a return list, a list of
-- variables to be returned after execution.
type Returnlist = [Variable]
type Program    = (Sequence,Returnlist)

opStrings :: [String]
opStrings = ["+","*","-","/","^"]

-- Translates operator characters to operators.
op :: String -> Operator
op "+"  = Add
op "*"  = Mult
op "-"  = Subt
op "/"  = Div
op "^"  = Pow
op s    = error ("wrong operator: " ++ s)

ops :: [Operator]
ops = map op opStrings

-- Operator precedence: ^ > *,/ > +,-
opPrec :: String -> Integer
opPrec "+"  = 0
opPrec "*"  = 1
opPrec "-"  = 0
opPrec "/"  = 1
opPrec "^"  = 2
opPrec s    = error ("wrong operator: " ++ s)

relStrings = ["==","<=","<","!="]
rel :: String -> Relation
rel "=="  = Equ
rel "<="  = Leq
rel "<"   = Less
rel "!="  = Neq
rel s     = error ("wrong relation symbol: " ++ s)

rels :: [Relation]
rels = map rel relStrings

var :: String -> Variable
var (c:s)
  | c == 'v' && all (`elem` digits) s = Var (read s :: Integer)
  | otherwise                       = error ("Ill-formed variable: " ++ c:s)

-- List of operators, digits, and variables
digits :: [Char]
digits = ['0'..'9']
varints :: [Integer]
varints = [0..] -- beware the infinite list!!! :)
vars :: [Variable]
vars = map Var varints

{-
  Assignments
-}

-- the initial variable assignment: every variable is assigned 0
initAssign :: Assign
initAssign x = 0

-- updates the assignment with an index-value pair
assignUpdate :: (Variable,Integer) -> Assign -> Assign
assignUpdate (v,m) g = \x -> if x == v then m else g x

-- updates the assignment with a list of values, starting from index n
assignParam :: [Integer] -> Integer -> Assign -> Assign
assignParam [] n g    = g
assignParam (m:s) n g = assignUpdate (Var n,m) (assignParam s (n+1) g)

{-
  Evaluation and program execution
-}

-- evaluates a term
-- division by zero and negative exponents are taken care of by Haskell's
-- arithmetic module
evalTerm :: Term -> Assign -> Integer
evalTerm (VarT x) g   = g x
evalTerm (Integ x) g  = x
evalTerm (Add x y) g  = evalTerm x g + evalTerm y g
evalTerm (Mult x y) g = evalTerm x g * evalTerm y g
evalTerm (Subt x y) g = evalTerm x g - evalTerm y g
evalTerm (Div x y) g  = evalTerm x g `div` evalTerm y g
evalTerm (Pow x y) g  = evalTerm x g ^ evalTerm y g

-- evaluates a condition
evalCond :: Condition -> Assign -> Bool
evalCond (Equ t t') g   = evalTerm t g == evalTerm t' g
evalCond (Leq t t') g   = evalTerm t g <= evalTerm t' g
evalCond (Less t t') g  = evalTerm t g < evalTerm t' g
evalCond (Neq t t') g   = evalTerm t g /= evalTerm t' g

-- evaluates a command
evalComm :: Command -> Assign -> Assign
evalComm (Let v term) g     = assignUpdate (v, evalTerm term g) g
evalComm (While cond prog) g
  | not (evalCond cond g) = g
  | otherwise             = evalComm (While cond prog) (evalSeq prog g)
evalComm (If cond prog1 prog2) g
  | evalCond cond g       = evalSeq prog1 g
  | otherwise             = evalSeq prog2 g

-- evaluates a sequence of commands
evalSeq :: Sequence -> Assign -> Assign
evalSeq [] g          = g
evalSeq (comm:prog) g = evalSeq prog (evalComm comm g)

{-
run has two arguments:
- a list of parameters (assigned to the first variables)
- the program itself
-}
run :: [Integer] -> String -> [Integer]
run parameters progString = runPow parameters seqPart retPart initAssign where
  parts = parseProg progString
  seqPart = fst parts
  retPart = snd parts
  runPow :: [Integer] -> Sequence -> Returnlist -> Assign -> [Integer]
  runPow parameters seqPart retPart g = map (evalSeq seqPart (assignParam parameters 0 g)) retPart

{-
  Parsing
-}

-- parses a numeral
parseNum :: String -> Integer
parseNum s
  | s == ""                           = 0
  | not (all (`elem` "0123456789") s) = error ("ill-formed numeral: " ++ s)
  | s == "0"                          = 0
  | head s == '0'                     = error ("ill-formed numeral: " ++ s)
  | otherwise                         = read s :: Integer

-- parses a variable
-- "v0", "v12"
parseVariable :: String -> Variable
parseVariable s
  | s == "" || tail s == ""           = error ("ill-formed variablel: " ++ s)
  | head s == 'v'                     = Var (parseNum (tail s))
  | otherwise                         = error ("ill-formed variable: " ++ s)

parseTerm :: String -> Term
parseTerm s = termFromTree (createTree (splitAtOps s) [] opPrec) where
  termFromTree :: StrTree -> Term
  termFromTree (Leaf s)
    | hasOuterPar s           = parseTerm (init (tail s))
    | s == ""                 = Integ 0
    | head s == 'v'           =  VarT (parseVariable s)
    | otherwise               = Integ (parseNum s)
  termFromTree (Node s t1 t2) = (op s) (termFromTree t1) (termFromTree t2)
  splitAtOps :: String -> [String]
  splitAtOps s = splitAtDelimitersN s opStrings ("(",")")

-- parses a condition:
-- "term1==term2", "term1<=term2", "term1!=term2", s"term1<term2"
parseCond :: String -> Condition
parseCond s
  | s == ""                 = error "empty condition!"
  | hasOuterPar s           = parseCond (init (tail s))
  | tail (fst parts) == []  = error ("ill-formed condition: " ++ s)
  | tail (snd parts) /= []  = error ("ill-formed condition: " ++ s)
  | otherwise = rel relPart (parseTerm leftPart) (parseTerm rightPart)
  where
    parts = splitAtDelimiters s relStrings ("","")
    leftPart = head (fst parts)
    rightPart = head (tail (fst parts))
    relPart = head (snd parts)

-- parses a command commands can be:
-- "vi = term"
-- "while(cond)do{seq}"
-- "if(cond)then{seq1}else{seq2}""
parseComm :: String -> Command
parseComm s
  | s == ""       = error "empty command!"
  | head s == 'v' = parseLet s
  | head s == 'w' = parseWhile s
  | head s == 'i' = parseIf s
  | otherwise     = error ("unknown keyword: " ++ s)
  where
    parseLet :: String -> Command
    parseLet s
      | length parts /= 2 = error ("ill-formed assignment: " ++ s)
      | otherwise         = Let (parseVariable varPart) (parseTerm termPart)
      where
        parts = fst (splitAtDelimiters s ["="] ("",""))
        varPart = head parts
        termPart = head (tail parts)
    parseWhile :: String -> Command
    parseWhile s
      | length parts < 3        = error ("while-do parts missing: " ++ s)
      | length parts /= 3       = error ("too many while-do parts: " ++ s)
      | nullPart /= ""          = error ("unknown keyword -- did you mean while-do? " ++ s)
      | dels /= ["while","do"]  = error ("unknown keyword -- did you mean while-do? " ++ s)
      | otherwise               = While (parseCond condPart) (parseSeq seqPart)
      where
        split     = splitAtDelimiters s ["while", "do"] ("{","}")
        parts     = fst split
        dels      = snd split
        nullPart  = parts !! 0
        condPart  = parts !! 1
        seqPart   = parts !! 2
    parseIf :: String -> Command
    parseIf s
      | length parts < 4             = error ("if-then-else parts missing: " ++ s)
      | 4 < length parts             = error ("too many if-then-else parts: " ++ s)
      | nullPart /= ""               = error ("unknown keyword -- did you mean if-then-else? " ++ s)
      | dels /= ["if","then","else"] = error ("unknown keyword -- did you mean if-then-else? " ++ s)
      | otherwise = If (parseCond condPart) (parseSeq seq1Part) (parseSeq seq2Part)
      where
        split     = splitAtDelimiters s ["if","then","else"] ("{","}")
        parts     = fst split
        dels      = snd split
        nullPart  = parts !! 0
        condPart  = parts !! 1
        seq1Part  = parts !! 2
        seq2Part  = parts !! 3

-- parses a command sequence
parseSeq :: String -> Sequence
parseSeq s
  | s == ""                         = error "empty sequence!"
  | head s == '{' && last s == '}'  = parseSeq (init (tail s))
  | otherwise                       = map parseComm parts
  where
    parts = fst (splitAtDelimiters s [";"] ("{","}"))

-- parses a return list
parseRet :: String -> Returnlist
parseRet s
  | parts == [] = error "empty return list!"
  | otherwise   = map parseVariable parts
  where
    parts = fst (splitAtDelimiters s [","] ("",""))

-- parses a program
parseProg :: String -> Program
parseProg s
  | parts == []       = error ("not a well-formed program: " ++ s)
  | tail parts == []  = error ("not a well-formed program: " ++ s)
  | otherwise         = (parseSeq (head parts), parseRet (head(tail parts)))
  where
    parts = fst (splitAtDelimiters (purgeWhitespace s) ["|"] ("",""))

{-
  String manipulation functions for parsing
-}

{-
  Splits a string at any of the delimiters in the delimiter list at the top
  level wrt the parentheses in the parentheses list.
  - delimiters are strings, eg. [",", ";", "|", "+", "::"]
  -parentheses are characters, eg. ("({[", "]})")
  examples:
    > splitAtDelimiters "a|b|c" ["|"] ("(",")")
    (["a","b","c"],["|","|"])
    > splitAtDelimiters "a|(b|c)" ["|"] ("(",")")
    (["a","(b|c)"],["|"])
    > splitAtDelimiters "a|{(b|c)|d}" ["|"] ("(",")")
    (["a","{(b|c)","d}"],["|","|"])
    > splitAtDelimiters "a|{(b|c)|d}" ["|"] ("({",")}")
    (["a","{(b|c)|d}"],["|"])
    > splitAtDelimiters "v0+7*v1-3" ["+","*","-"] ("(",")")
    (["v0","7","v1","3"],["+","*","-"])
    > splitAtDelimiters "(v0+7)*(v1-3)" ["+","*","-"] ("(",")")
    (["(v0+7)","(v1-3)"],["*"]
    > splitAtDelimiters "v0<=7" ["<=","<","="] ("(",")")
    (["v0","7"],["<="])
    > splitAtDelimiters "v0<=7" ["<","=","<="] ("(",")")
    (["v0","","7"],["<","="])
-}
splitAtDelimiters :: String -> [String] -> (String,String) -> ([String],[String])
splitAtDelimiters s del_l par_l = splitAtDelAux s del_l par_l 0 [] [] "" where
  splitAtDelAux :: String -> [String] -> (String,String) -> Integer -> [String] -> [String] -> String -> ([String],[String])
  splitAtDelAux "" del_l par_l n parts delimiters currentpart = (parts ++ [currentpart], delimiters)
  splitAtDelAux s del_l par_l n parts delimiters currentpart
    | s == "" = (parts ++ [currentpart], delimiters)
    | (head s) `elem` (fst par_l)         = splitAtDelAux (tail s) del_l par_l (n+1) parts delimiters (currentpart ++ [head s])
    | (head s) `elem` (snd par_l)         = splitAtDelAux (tail s) del_l par_l (n-1) parts delimiters (currentpart ++ [head s])
    | any (beginsWith s) del_l && n == 0  = splitAtDelAux (fst (splitHead s del_l)) del_l par_l n (parts ++ [currentpart]) (delimiters ++ [snd (splitHead s del_l)]) ""
    | otherwise                           = splitAtDelAux (tail s) del_l par_l n parts delimiters (currentpart ++ [head s])
  beginsWith :: String -> String -> Bool
  beginsWith s1 s2
    | s2 == ""            = True
    | s1 == ""            = False
    | head s1 == head s2  = beginsWith (tail s1) (tail s2)
    | otherwise           = False
  splitHead :: String -> [String] -> (String, String)
  splitHead s []  = (s, "")
  splitHead s (x:l)
    | beginsWith s x  = (drop (length x) s, x)
    | otherwise   = splitHead s l

{-
  Does the same as the previous one, except it returns a single list:
  examples:
    > splitAtDelimitersN "a|b|c" ["|"] ("(",")")
    ["a","|","b","|","c"]
    > splitAtDelimitersN "a|(b|c)" ["|"] ("(",")")
    ["a","|","(b|c)"]
    > splitAtDelimitersN "a|{(b|c)|d}" ["|"] ("(",")")
    ["a","|","{(b|c)","|","d}"]
    > splitAtDelimitersN "a|{(b|c)|d}" ["|"] ("({",")}")
    ["a","|","{(b|c)|d}"]
    > splitAtDelimitersN "v0+7*v1-3" ["+","*","-"] ("(",")")
    ["v0","+","7","*","v1","-","3"]
    > splitAtDelimitersN "(v0+7)*(v1-3)" ["+","*","-"] ("(",")")
    ["(v0+7)","*","(v1-3)"]
    > splitAtDelimitersN "v0<=7" ["<=","<","="] ("(",")")
    ["v0","<=","7"]
    > splitAtDelimitersN "v0<=7" ["<","=","<="] ("(",")")
    ["v0","<","","=","7"]
-}
splitAtDelimitersN :: String -> [String] -> (String,String) -> [String]
splitAtDelimitersN s dels pars = splitAtDelAuxN s dels pars 0 "" where
  splitAtDelAuxN :: String -> [String] -> (String,String) -> Integer -> String -> [String]
  splitAtDelAuxN s dels pars depth currentpart
    | s == "" = [currentpart]
    | (head s) `elem` (fst pars)            = splitAtDelAuxN (tail s) dels pars (depth+1) (currentpart ++ [head s])
    | (head s) `elem` (snd pars)            = splitAtDelAuxN (tail s) dels pars (depth-1) (currentpart ++ [head s])
    | any (beginsWith s) dels && depth == 0 = currentpart : ((snd (splitHead s dels)) : (splitAtDelAuxN (fst (splitHead s dels)) dels pars depth ""))
    | otherwise                             = splitAtDelAuxN (tail s) dels pars depth (currentpart ++ [head s])
  beginsWith :: String -> String -> Bool
  beginsWith s1 s2
    | s2 == ""            = True
    | s1 == ""            = False
    | head s1 == head s2  = beginsWith (tail s1) (tail s2)
    | otherwise           = False
  splitHead :: String -> [String] -> (String, String)
  splitHead s []  = (s, "")
  splitHead s (x:l)
    | beginsWith s x  = (drop (length x) s, x)
    | otherwise   = splitHead s l

data StrTree = Leaf String | Node String StrTree StrTree deriving (Eq, Show)

-- creates a tree from the list splitAtDelimitersN returns, and a function that
-- precedence assigns precedence value to the delimiters.
createTree :: [String] -> [String] -> (String -> Integer) -> StrTree
createTree leftParts rightParts delPrec
  | rightParts == [] && tail leftParts == []  = Leaf (head leftParts)
  | rightParts == []                          = createTree (init (init leftParts)) [last (init leftParts), last leftParts] delPrec
  | isLow (head rightParts) leftParts delPrec = (Node (head rightParts)) (createTree leftParts [] delPrec) (createTree (tail rightParts) [] delPrec)
  | otherwise                                 = createTree (init (init leftParts)) ([last (init  leftParts), last leftParts] ++ rightParts) delPrec
isLow :: String -> [String] -> (String -> Integer) -> Bool
isLow del parts delPrec
  | tail parts == []                          = True
  | delPrec (head (tail parts)) < delPrec del = False
  | otherwise                                 = isLow del (tail (tail parts)) delPrec

-- checks whether a pair of parentheses surrounds a string:
-- "(a(a)a)" => True
-- "(a)a(a)" => False -- outmost parentheses do not belong together
hasOuterPar :: String -> Bool
hasOuterPar s
  | s == ""       = False
  | head s /= '(' = False
  | last s /= ')' = False
  | otherwise = closesAtLast 1 (tail s)
  where
    closesAtLast :: Integer -> String -> Bool
    closesAtLast n s
      | n == 0 && s == "" = True
      | s == []           = False
      | n == 0            = False
      | head s == '('     = closesAtLast (n+1) (tail s)
      | head s == ')'     = closesAtLast (n-1) (tail s)
      | otherwise         = closesAtLast n (tail s)

-- erases all apaces, tabs, and linebreaks
-- thus "whi le" is read as just while, and "v 0" as just "v0", which may be
-- too much
purgeWhitespace :: String -> String
purgeWhitespace s = [c | c <- s, not (c `elem` " \t\n\r")]
