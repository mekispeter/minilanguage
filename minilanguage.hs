import Data.Map (Map, (!))
import qualified Data.Map as Map
{-
  This program parses and runs programs written in an imperative minilanguage
  with very limited syntax. It supports simple control flow, if-then-else
  contstructs and while-do loops. The only supported data type is integer.

  Every program has the form 'v1,v2,...|comm1;comm2;...|w1,w2,...', where
  'comm1;comm2,...' is a sequence of commands, and v1, v2, ... and w1, w2, ...
  are list of variables. Both variable lists are comma separated. The command
  sequence is semicolon separated.

  The first list of variables sets which variables the parameters of the
  program will be assigned to. If there are less parameters than variables in
  this list, the rest of the variables will be assigned zero. If there are more
  parameters than variables, the rest of the parameters will be ignored.

  The second list of variables specifies the values that will be printed after
  execution. The two lists may overlap, and any of them may be empty.

  Commands are either of three kinds:
  - simple variable assignments of the form 'v = t', where v is a variable,
    and t is a term;
  - while loops of the form 'while(cond)do{seq}', where cond is a condition,
    and seq is a command sequence (parenthesizing the condition is optional),
  - if constructs of the form 'if(cond)then{seq1}else{seq2}', where cond is a
    condition, and seq1 and seq2 are command sequences (parenthesizing the
    condition is optional).

  Conditions are
  - one of the Boolean values "true" and "false"
  - two terms related by "==", "!=", "<=", "<", "=>", or ">", optionally
    parenthesized
  - Boolean operations of conditions: "and", "or", "not".

  Variable names can be any string of smallcase letters and underscores: "x",
  "max_twin_prime", or even "___". Numbers, uppercase letters and other symbols
  are not allowed in variable names.

  Terms are arithmetic expressions with integers, variables, and the operations
  {+,*,-,/,^}. Terms are parsed in a left-to-right fashion, with standard
  operator precedence (^ > *,/ > +,-): "11+9-3*2^3+5" is parsed into the same
  term as "((11+(9-(3*(2^3))))+5)". Negative integers have to be bracketed:
  "3*(-2)" is parsed as "3*(0-2)", but "3*-2" is parsed as "(3*0)-2". '/' is
  integer division: "7/3" is evaluated as 2. Division by zero and negative
  exponents raise error.

  Programs can be run by calling the 'run' function. Its type is
  [Integer] -> String -> [Integer]. The first argument is a parameter list
  [n0,...,nk] that provedes values to an initial list of variables: if prog1
  starts with "n,m|...", and it is run from ghci with the call
  "run [1,2] prog1", then n will be assigned 1 and m will be assigned 2.

  Whitespace (space, tab, line break) is allowed anywhere in the code, even in
  keywords and numerals, and is ignored. Comments start with '#' and end with
  either '#' or '\n', and are ignored at runtime. The wollowing two program
  strings parse into the same program:
  - "n|i=1;fact=1;whilei<=ndo{fact=fact*i;i=i+1}|fact"
  - "# factorial function                                                     \n
     n # argument of the function # | # delimiter                             \n
     i = 1; # counter variable starts from 1                                  \n
     fact = 1; # initial value of the factorial                               \n
     while i <= n # counting will stop at n -- note that pars are optional #ín\n
       fact = fact * i; # the factorial is multiplied with the next number    \n
       i = i + 1 # counter is incremented -- note the absence of ;            \n
     }
     | fact # this is the return value of the function"

  The language is Turing complete. In principle lists and strings can be
  simulated via Gödel encodings, although that is an unpleasant way to go.
  In fact, even if-then-else constructs are redundant; they can be simulated by
  while loops (also an unpleasant way to go).

  The parser part is ugly. It works, but it has to be simplified and made more
  readable.

  ToDo:
    - reading program from file
    - make parser functions slightly less ugly
    - error message if variable is used before assignment 
-}

{-
A few sample programs written in the minilanguage
-}

-- n mod m. Eg. for n=27, m=12:
-- > run [27,12] mod'
-- "mod:3"
mod' :: String
mod' =
  " n, m |                              \n\
  \ mod = n - (n / m) * m               \n\
  \ | mod"

-- Pythagorean triple checker: 1 for valid triples, 0 otherwise. Eg:
-- run [3,4,5] pyth3ch
-- "is_pyth_triple:1"
-- run [4,5,6] pyth3ch
-- "is_pyth_triple:0"
pyth3ch :: String
pyth3ch =
  " a, b, c |                           \n\
  \ if c^2 == a^2 + b^2                 \n\
  \ then {is_pyth_triple = 1}           \n\
  \ else {is_pyth_triple = 0}           \n\
  \ | is_pyth_triple"

-- Factorial of n. Eg. for n=7:
-- > run [7] factorial
-- "factorial:5040"
factorial :: String
factorial =
  " n |                                 \n\
  \ i = 1;                              \n\
  \ factorial = 1;                      \n\
  \ while i <= n do {                   \n\
  \   factorial = factorial * i;        \n\
  \   i = i + 1                         \n\
  \ }                                   \n\
  \ | factorial"

-- nth number in the Fibonacci sequence. Eg. for n=10:
-- > run [10] fibonacci
-- "fib:55"
fibonacci :: String
fibonacci =
  " n |                                 \n\
  \ i = 1;                              \n\
  \ fib_prev = 0;                       \n\
  \ fib = 1;                            \n\
  \ while i < n do {                    \n\
  \   fib_new = fib_prev + fib;         \n\
  \   fib_prev = fib;                   \n\
  \   fib = fib_new;                    \n\
  \   i = i + 1                         \n\
  \ }                                   \n\
  \ | fib"

-- Greatest common divisor of n and m. Eg. for n=10, m=72:
-- > run [120,72] gcd'
-- "gcd:24"
gcd' :: String
gcd' =
  " n, m |                              \n\
  \ while 0 < m do {                    \n\
  \   n_red = n - (n / m) * m;          \n\
  \   n = m;                            \n\
  \   m = n_red                         \n\
  \ };                                   \n\
  \ gcd = n                             \n\
  \ | gcd"

-- Number of divisors of n. Eg for n=30:
-- > run [30] divisors
-- "divisors:8
divisors :: String
divisors =
  " n |                                 \n\
  \ divisors = 0;                       \n\
  \ i = 1;                              \n\
  \ while i <= n do {                   \n\
  \   if n - (n / i) * i == 0           \n\
  \   then {divisors = divisors + 1}    \n\
  \   else {divisors = divisors};       \n\
  \   i = i + 1                         \n\
  \ }                                   \n\
  \ | divisors"

-- Prime checker: 1 if n is a prime, 0 otherwise. Eg. for n = 2019 and n = 2017:
-- > run [2019] primecheck
-- "is_prime:0"
-- > run [2017] primecheck
-- "is_prime:1"
primecheck :: String
primecheck =
  " n |                                 \n\
  \ i = 2;                              \n\
  \ is_prime = 1;                       \n\
  \ while i^2 <= n do {                 \n\
  \   if n - (n / i) * i == 0           \n\
  \   then {is_prime=0}                 \n\
  \   else {is_prime = is_prime};       \n\
  \   i = i + 1                         \n\
  \ }                                   \n\
  \ | is_prime"

-- This is a test to make sure that the code is not broken and samples run as
-- expected.
test :: Bool
test =
  run [27,12] mod' == "mod:3" &&
  run [3,4,5] pyth3ch == "is_pyth_triple:1" &&
  run [4,5,6] pyth3ch == "is_pyth_triple:0" &&
  run [7] factorial == "factorial:5040" &&
  run [10] fibonacci == "fib:55" &&
  run [120,72] gcd' == "gcd:24" &&
  run [30] divisors == "divisors:8" &&
  run [2019] primecheck == "is_prime:0" &&
  run [2017] primecheck == "is_prime:1" &&
  run [] "__|while((2^4*5/(3+2)==(+-1)))do{if(trueorfalse)then{__=-1}else{__=1}}|__" == "__:0"

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
data Variable   = Var String deriving (Eq, Show, Ord)
-- Every variable is assigned an integer value. The execution of a command or a
-- sequence of commands transforms one variable assignment into another.
-- Assignments are easier to handle as dictionaries than as functions, since
-- there is only a limited list of variables, and both the list and the values
-- are updated regularly.
type Assign     = Map Variable Integer
-- Four relations are supported in conditions: ==, <=, <, and !=.
data Condition  = TrueC | FalseC |
                  Equ Term Term | Neq Term Term |
                  Leq Term Term | Less Term Term |
                  Geq Term Term | Gtr Term Term |
                  And Condition Condition | Or Condition Condition |
                  Not Condition Condition
                  deriving (Eq, Show)
type Relation   = Term -> Term -> Condition
-- Commands are either variable assignments, while-do loops, or if-then-else
-- constructs.
type BoolOp     = Condition -> Condition -> Condition
data Command    = Let Variable Term |
                  While Condition Sequence |
                  If Condition Sequence Sequence
                  deriving (Eq, Show)
-- A command sequence is a list of commands, executed sequentially.
type Sequence   = [Command]
-- A program is a triple of an init list, a command sequence and a return list.
-- The command sequence is the program body.
-- An init list is a list of variables that the parameters are passed to.
-- A return list
type Varlist = [Variable]
type Program    = (Varlist,Sequence,Varlist)

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

relStrings = ["==","!=","<=","<",">=",">"]
rel :: String -> Relation
rel "=="  = Equ
rel "!="  = Neq
rel "<="  = Leq
rel "<"   = Less
rel ">="  = Geq
rel ">"   = Gtr
rel s     = error ("wrong relation symbol: " ++ s)

rels :: [Relation]
rels = map rel relStrings

boolOpStrings :: [String]
boolOpStrings = ["and", "or", "not"]
boolOp :: String -> BoolOp
boolOp "and"  = And
boolOp "or"   = Or
boolOp "not" = Not
boolOp s      = error ("wrong Boolean operator: " ++ s)
boolOps = map boolOp boolOpStrings

-- Boolean operator precedence: ^ > *,/ > +,-
boolOpPrec :: String -> Integer
boolOpPrec "and"    = 1
boolOpPrec "or"     = 0
boolOpPrec "not"   = 2
boolOpPrec s        = error ("wrong Boolean operator: " ++ s)

boolStrings :: [String]
boolStrings = ["true", "false"]
bool :: String -> Condition
bool "true" = TrueC
bool "false" = FalseC
bools = map bool boolStrings

-- List digits
digits :: [Char]
digits = ['0'..'9']
varChars :: [Char]
varChars = '_':['a'..'z']

{-
  Assignments
-}

{-
  Evaluation and program execution
-}

-- evaluates a term
-- division by zero and negative exponents are taken care of by Haskell's
-- arithmetic module
evalTerm :: Term -> Assign -> Integer
evalTerm (VarT x) g   = g ! x
evalTerm (Integ x) g  = x
evalTerm (Add x y) g  = evalTerm x g + evalTerm y g
evalTerm (Mult x y) g = evalTerm x g * evalTerm y g
evalTerm (Subt x y) g = evalTerm x g - evalTerm y g
evalTerm (Div x y) g  = evalTerm x g `div` evalTerm y g
evalTerm (Pow x y) g  = evalTerm x g ^ evalTerm y g

-- evaluates a condition
evalCond :: Condition -> Assign -> Bool
evalCond (TrueC) g      = True
evalCond (FalseC) g     = False
evalCond (Equ t t') g   = evalTerm t g == evalTerm t' g
evalCond (Neq t t') g   = evalTerm t g /= evalTerm t' g
evalCond (Leq t t') g   = evalTerm t g <= evalTerm t' g
evalCond (Less t t') g  = evalTerm t g < evalTerm t' g
evalCond (Geq t t') g   = evalTerm t g >= evalTerm t' g
evalCond (Gtr t t') g  = evalTerm t g > evalTerm t' g
evalCond (And c c') g   = evalCond c g && evalCond c' g
evalCond (Or c c') g    = evalCond c g || evalCond c' g
evalCond (Not c c') g  = not (evalCond c' g)

-- evaluates a command
evalComm :: Command -> Assign -> Assign
evalComm (Let v term) g     = Map.insert v (evalTerm term g) g
evalComm (While cond seq1) g
  | not (evalCond cond g) = g
  | otherwise             = evalComm (While cond seq1) (evalSeq seq1 g)
evalComm (If cond seq1 seq2) g
  | evalCond cond g       = evalSeq seq1 g
  | otherwise             = evalSeq seq2 g

-- evaluates a sequence of commands
evalSeq :: Sequence -> Assign -> Assign
evalSeq [] g          = g
evalSeq (comm:prog) g = evalSeq prog (evalComm comm g)

{-
run has two arguments:
- a list of parameters (assigned to the first variables)
- the program itself
-}
run :: [Integer] -> String -> String
run parameters progString = showValues (evalSeq seqPart initAssign) retPart where
  initPart :: Varlist
  seqPart :: Sequence
  retPart :: Varlist
  (initPart, seqPart, retPart) = parseProg progString
  extParameters :: [Integer]
  extParameters = parameters ++ [0 | x <- [0..]]
  initAssign :: Assign
  initAssign = Map.fromList (zip initPart extParameters)
  showValues :: Assign -> Varlist -> String
  showValues assign varlst = init (concat (map (showValue assign) varlst))
  showValue :: Assign -> Variable -> String
  showValue assign (Var s) = s ++ ":" ++ (show (assign ! (Var s))) ++ " "

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
  | s == ""                 = error ("empty variable: " ++ s)
  | all (`elem` varChars) s = Var s
  | otherwise               = error ("ill-formed variable: " ++ s)

-- terms are inegers, variables, and operations
parseTerm :: String -> Term
parseTerm s = termFromTree (createTree (splitAtOps s) [] opPrec) where
  termFromTree :: StrTree -> Term
  termFromTree (Leaf s)
    | hasOuterPar s           = parseTerm (init (tail s))
    | all (`elem` digits) s   = Integ (parseNum s)
    | otherwise               =  VarT (parseVariable s)
  termFromTree (Node s t1 t2) = (op s) (termFromTree t1) (termFromTree t2)
  splitAtOps :: String -> [String]
  splitAtOps s = splitAtDelimiters s opStrings ("(",")")

-- parses a condition:
-- "term1==term2", "term1<=term2", "term1!=term2", s"term1<term2"
parseRel :: String -> Condition
parseRel s
  | hasOuterPar s     = parseRel (init (tail s))
  | length parts < 3  = error ("relation symbol missing: " ++ s)
  | 3 < length parts  = error ("too many relation symbols: " ++ s)
  | otherwise         = rel relPart (parseTerm leftPart) (parseTerm rightPart)
  where
    parts = splitAtDelimiters s relStrings ("","")
    leftPart = parts !! 0
    relPart = parts !! 1
    rightPart = parts !! 2

parseCond :: String -> Condition
parseCond s = condFromTree (createTree (splitAtBoolOps s) [] boolOpPrec) where
  condFromTree :: StrTree -> Condition
  condFromTree (Leaf s)
    | hasOuterPar s           = parseCond (init (tail s))
    | s == ""                 = FalseC
    | s == "true"             = TrueC
    | s == "false"            = FalseC
    | otherwise               = parseRel s
  condFromTree (Node s t1 t2) = (boolOp s) (condFromTree t1) (condFromTree t2)
  splitAtBoolOps :: String -> [String]
  splitAtBoolOps s = splitAtDelimiters s boolOpStrings ("(",")")

-- parses a command commands can be:
-- "var = term"
-- "while(cond)do{seq}"
-- "if(cond)then{seq1}else{seq2}""
parseComm :: String -> Command
parseComm s
  | s == ""               = error "empty command!"
  | s `beginsWith`"while" = parseWhile s
  | s `beginsWith` "if"   = parseIf s
  |  otherwise            = parseLet s
  where
    parseLet :: String -> Command
    parseLet s
      | length parts /= 3 = error ("unknown command format: " ++ s)
      | otherwise         = Let (parseVariable varPart) (parseTerm termPart)
      where
        parts = splitAtDelimiters s ["="] ("(",")")
        varPart = head parts
        termPart = last parts
    parseWhile :: String -> Command
    parseWhile s
      | length parts < 3        = error ("while-do parts missing: " ++ s)
      | length parts /= 3       = error ("too many while-do parts: " ++ s)
      | dels /= ["while","do"]  = error ("unknown keyword -- did you mean while-do? " ++ s)
      | otherwise               = While (parseCond condPart) (parseSeq seqPart)
      where
        split     = splitAtDelimiters s ["while", "do"] ("{","}")
        parts     = partsFromSplit split
        dels      = delimitersFromSplit split
        nullPart  = parts !! 0
        condPart  = parts !! 1
        seqPart   = parts !! 2
    parseIf :: String -> Command
    parseIf s
      | length parts < 4             = error ("if-then-else parts missing: " ++ s)
      | 4 < length parts             = error ("too many if-then-else parts: " ++ s)
      | dels /= ["if","then","else"] = error ("did you mean if-then-else? " ++ s)
      | otherwise = If (parseCond condPart) (parseSeq seq1Part) (parseSeq seq2Part)
      where
        split     = splitAtDelimiters s ["if","then","else"] ("{","}")
        parts     = partsFromSplit split
        dels      = delimitersFromSplit split
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
    parts = partsFromSplit (splitAtDelimiters s [";"] ("{","}"))

-- parses a return list
parseVarList :: String -> Varlist
parseVarList s
  | parts == [] = error "empty variable list!"
  | otherwise   = map parseVariable parts
  where
    parts = partsFromSplit (splitAtDelimiters s [","] ("",""))

-- parses a program
parseProg :: String -> Program
parseProg s
  | length parts < 3  = error ("program parts missing: " ++ s)
  | 3 < length parts  = error ("not a well-formed program: " ++ s)
  | otherwise         = (initPart, seqPart, retPart)
  where
    parts :: [String]
    parts = partsFromSplit (splitAtDelimiters (prepare s) ["|"] ("",""))
    prepare :: String -> String
    prepare s = purgeWhitespace (removeComments s)
    initPart = parseVarList (parts !! 0)
    seqPart = parseSeq (parts !! 1)
    retPart = parseVarList (parts !! 2)

{-
  String manipulation functions for parsing
-}

beginsWith :: String -> String -> Bool
beginsWith sBig sSmall
  | sSmall == ""              = True
  | sBig == ""                = False
  | head sBig == head sSmall  = beginsWith (tail sBig) (tail sSmall)
  | otherwise                 = False

{-
  Splits a string at any of the delimiters in the delimiter list at the top
  level wrt the parentheses in the parentheses list.
  - delimiters are strings, eg. [",", ";", "|", "+", "::"]
  - parentheses are characters, eg. ("({[", "]})")
  A single list is returned, containing the delimiters.
  Examples:
    > splitAtDelimiters "a|b|c" ["|"] ("(",")")
    ["a","|","b","|","c"]
    > splitAtDelimiters "a|(b|c)" ["|"] ("(",")")
    ["a","|","(b|c)"]
    > splitAtDelimiters "a|{(b|c)|d}" ["|"] ("(",")")
    ["a","|","{(b|c)","|","d}"]
    > splitAtDelimiters "a|{(b|c)|d}" ["|"] ("({",")}")
    ["a","|","{(b|c)|d}"]
    > splitAtDelimiters "v0+7*v1-3" ["+","*","-"] ("(",")")
    ["v0","+","7","*","v1","-","3"]
    > splitAtDelimiters "(v0+7)*(v1-3)" ["+","*","-"] ("(",")")
    ["(v0+7)","*","(v1-3)"]
    > splitAtDelimiters "v0<=7" ["<=","<","="] ("(",")")
    ["v0","<=","7"]
    > splitAtDelimiters "v0<=7" ["<","=","<="] ("(",")")
    ["v0","<","","=","7"]
-}
splitAtDelimiters :: String -> [String] -> (String,String) -> [String]
splitAtDelimiters s dels pars = splitAtDelAux s dels pars 0 "" where
  splitAtDelAux :: String -> [String] -> (String,String) -> Integer -> String -> [String]
  splitAtDelAux s dels pars depth currentpart
    | s == "" = [currentpart]
    | (head s) `elem` (fst pars)            = splitAtDelAux (tail s) dels pars (depth+1) (currentpart ++ [head s])
    | (head s) `elem` (snd pars)            = splitAtDelAux (tail s) dels pars (depth-1) (currentpart ++ [head s])
    | any (beginsWith s) dels && depth == 0 = currentpart : ((snd (splitHead s dels)) : (splitAtDelAux (fst (splitHead s dels)) dels pars depth ""))
    | otherwise                             = splitAtDelAux (tail s) dels pars depth (currentpart ++ [head s])
  splitHead :: String -> [String] -> (String, String)
  splitHead s []  = (s, "")
  splitHead s (x:l)
    | beginsWith s x  = (drop (length x) s, x)
    | otherwise   = splitHead s l

delimitersFromSplit :: [String] -> [String]
delimitersFromSplit sl = [sl !! n | n <- [0..((length sl)-1)], odd n]

partsFromSplit :: [String] -> [String]
partsFromSplit sl = [sl !! n | n <- [0..((length sl)-1)], even n]

data StrTree = Leaf String | Node String StrTree StrTree deriving (Eq, Show)

-- creates a tree from the list splitAtDelimiters returns, and a function that
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

-- # opens switches from code to comment and back; linebreak only switches back
-- to code
-- aaa#bbb\nccc => aaa\nccc
-- aaa#bbb#ccc  => aaaccc
removeComments :: String -> String
removeComments s = removeCommentsAux s False where
  removeCommentsAux :: String -> Bool -> String
  removeCommentsAux [] is_comment       = []
  removeCommentsAux ('#':s) is_comment  = removeCommentsAux s (not is_comment)
  removeCommentsAux ('\n':s) is_comment = '\n':(removeCommentsAux s False)
  removeCommentsAux (c:s) True          = removeCommentsAux s True
  removeCommentsAux (c:s) False         = c:(removeCommentsAux s False)
