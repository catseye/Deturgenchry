module Deturgenchry where

import Text.ParserCombinators.Parsec

-- ========== MAPS ========== --

data Map k v = Binding k v (Map k v)
             | EmptyMap
             deriving (Eq, Ord)

get _ EmptyMap = Nothing
get key (Binding key' val map)
    | key == key' = Just val
    | otherwise   = get key map

set key val map = Binding key val (strip key map)

strip key EmptyMap = EmptyMap
strip key (Binding key' val map)
    | key == key' = strip key map
    | otherwise   = Binding key' val (strip key map)

merge map EmptyMap = map
merge map (Binding key val rest) =
    merge (set key val map) rest

instance (Show k, Show v) => Show (Map k v) where
    show EmptyMap = "[]"
    show (Binding k v map) = (show k) ++ "=" ++ (show v) ++ "\n" ++ show map

-- ========== Grammar ========== --

-- Program      ::= {ClassDefn}.
-- ClassDefn    ::= "class" name "{" {MethodDefn} "}".
-- MethodDefn   ::= "method" name "(" [name {"," name}] ")" Statement.
-- Statement    ::= Block | Conditional | Transfer | Assignment.
-- Block        ::= "{" {Statement} "}".
-- Conditional  ::= "if" Expr Statement "else" Statement.
-- Transfer     ::= "pass" Expr Expr.
-- Assignment   ::= name "=" Expr.
-- Expr         ::= RefExpr | "new" name | IntegerLiteral.
-- RefExpr      ::= name {"." name} [SetExpr | CallExpr].
-- SetExpr      ::= "[" name "=" Expr {"," name "=" Expr} "]".
-- CallExpr     ::= "(" Expr {"," Expr} ")".

-- ========== AST ========== --

type Name = String

data Program    = Program [ClassDefn]
    deriving (Show, Ord, Eq)

data ClassDefn  = ClassDefn Name [MethodDefn]
    deriving (Show, Ord, Eq)

data MethodDefn = MethodDefn Name [Name] Statement
    deriving (Show, Ord, Eq)

data Statement  = Block [Statement]
                | Conditional Expr Statement Statement
                | Transfer Expr Expr
                | Assign Name Expr
    deriving (Show, Ord, Eq)

data Expr       = Get [Name]
                | Call [Name] [Expr]
                | Mod [Name] [(Name, Expr)]
                | IntLit String
                | New Name
    deriving (Show, Ord, Eq)

-- ========== PARSER ========== --

name :: Parser Name
name = do
    c <- letter
    cs <- many alphaNum
    spaces
    return (c:cs)

classDefn :: Parser ClassDefn
classDefn = do
    string "class"
    spaces
    n <- name
    string "{"
    spaces
    ms <- many (methodDefn)
    string "}"
    spaces
    return (ClassDefn n ms)

methodDefn :: Parser MethodDefn
methodDefn = do
    string "method"
    spaces
    n <- name
    string "("
    spaces
    ps <- sepBy (name) (string "," >> spaces)
    string ")"
    spaces
    s <- statement
    return (MethodDefn n ps s)

statement :: Parser Statement
statement = (blockStatement <|> condStatement <|> xferStatement <|> assignment)

blockStatement :: Parser Statement
blockStatement = do
    string "{"
    spaces
    ss <- many (statement)
    string "}"
    spaces
    return (Block ss)

condStatement :: Parser Statement
condStatement = do
    string "if"
    spaces
    e <- expr
    s1 <- statement
    string "else"
    spaces
    s2 <- statement
    return (Conditional e s1 s2)

xferStatement :: Parser Statement
xferStatement = do
    string "pass"
    spaces
    dest <- expr
    stuff <- expr
    return (Transfer dest stuff)

assignment :: Parser Statement
assignment = do
    n <- name
    string "="
    spaces
    e <- expr
    return (Assign n e)

expr :: Parser Expr
expr = (try intLit) <|> (try newExpr) <|> refExpr

intLit :: Parser Expr
intLit = do
    d <- digit
    ds <- many digit
    spaces
    return (IntLit (d:ds))

newExpr :: Parser Expr
newExpr = do
    string "new"
    spaces
    n <- name
    return (New n)

refExpr :: Parser Expr
refExpr = do
    names <- sepBy1 (name) (string ".")
    spaces
    e <- (modExpr names <|> callExpr names <|> getExpr names)
    return e

modExpr names = do
    string "["
    spaces
    pairs <- sepBy1 (modification) (string "," >> spaces)
    string "]"
    spaces
    return (Mod names pairs)
  where
    modification = do
        n <- name
        string "="
        spaces
        e <- expr
        return (n, e)

callExpr names = do
    string "("
    spaces
    es <- sepBy (expr) (string "," >> spaces)
    string ")"
    spaces
    return (Call names es)

getExpr names = do
    return (Get names)

program :: Parser Program
program = do
    cs <- many (classDefn)
    return (Program cs)

-- ========== RUNTIME ========== --

--
-- A ContObj is what continuations pass along to each other.
-- It can be a context (environment), a single object, or a list of objects.
--
data ContObj = Ctx (Map Name Object)
             | Obj Object
             | Objs [Object]

--
-- A continuation represents the remaining (sub-)computation(s) in a
-- computation.
--
data Continuation = Continuation (ContObj -> ContObj)

instance Show Continuation where
    show (Continuation _) = ""
instance Eq Continuation where
    Continuation _ == Continuation _ = False

continue (Continuation f) contObj =
    f contObj  -- straightforward enuff

--
-- An object is anything else.  Including, maybe, a continuation.
--
data Object = IntVal Integer
            | ObjVal String (Map Name Object)
            | ContVal (Map Name Object) Continuation
            | Null
    deriving (Show, Eq)

only name Nothing = error ("No such attribute " ++ name ++ " on value")
only _ (Just x) = x

getAttribute name (ObjVal c m) = only name (get name m)
getAttribute name (ContVal m k) = only name (get name m)
getAttribute name value = error ("Can't get attributes from value " ++ (show value))

-- ========== INTERPRETER ========== --

interpret prog = do
    print (evalProg prog)

---------------------------------------------------
evalProg :: Program -> Object

evalProg p =
    case (getClass "Main" p) of
        Nothing -> error "No Main class with main() method found"
        Just mainClass ->
            case (getMethod "main" mainClass) of
                Nothing -> error "No Main class with main() method found"
                Just mainMethod ->
                    let
                        final = Continuation id
                        r = callMethod p (ContVal EmptyMap final) mainMethod []
                    in
                        case r of
                            Ctx c -> Null
                            Obj o -> o

getClass name (Program []) = Nothing
getClass name (Program (c@(ClassDefn candidate _methods):rest))
    | candidate == name = Just c
    | otherwise         = getClass name (Program rest)

getMethod name (ClassDefn _ []) = Nothing
getMethod name (ClassDefn className (m@(MethodDefn candidate _args _stmt):rest))
    | candidate == name = Just m
    | otherwise         = getMethod name (ClassDefn className rest)

callMethod p other (MethodDefn name formals stmt) actuals =
    case (length actuals) - (length formals) of
        0 ->
            let
                self = (ContVal EmptyMap (Continuation id))  -- NO NOT REALLY
                ctx = buildContext formals actuals
                ctx' = set "self" self ctx
                ctx'' = set "other" other ctx'
            in
                evalStatement p ctx'' stmt (Continuation id)
        n | n > 0 ->
            error "Too many parameters passed to method"
        n | n < 0 ->
            error "Too few parameters passed to method"

buildContext [] [] = EmptyMap
buildContext (formal:formals) (actual:actuals) =
    set formal actual (buildContext formals actuals)

---------------------------------------------------
evalStatement :: Program -> (Map Name Object) -> Statement -> Continuation -> ContObj

evalStatement p ctx (Block []) cc =
    Ctx ctx
evalStatement p ctx (Block (stmt:rest)) cc =
    evalStatement p ctx stmt (Continuation $ \(Ctx ctx') ->
        evalStatement p ctx' (Block rest) cc)

evalStatement p ctx (Conditional e s1 s2) cc =
    evalExpr p ctx e (Continuation $ \(Obj value) ->
        case value of
            Null -> evalStatement p ctx s2 cc
            _    -> evalStatement p ctx s1 cc)

evalStatement p ctx (Transfer dest e) _ =
    evalExpr p ctx e (Continuation $ \(Obj value) ->
        evalExpr p ctx dest (Continuation $ \(Obj (ContVal m (Continuation k))) ->
            k $ Obj value))

evalStatement p ctx (Assign name e) cc =
    evalExpr p ctx e (Continuation $ \(Obj value) ->
        case get name ctx of
            Nothing -> continue cc $ Ctx $ set name value ctx
            Just _  -> error ("Attempted re-assignment of bound name " ++ name))

---------------------------------------------------
evalExpr :: Program -> (Map Name Object) -> Expr -> Continuation -> ContObj

evalExpr p ctx (Get [name]) cc =
    case get name ctx of
        Nothing -> error ("Name " ++ name ++ " not in scope")
        Just val -> continue cc $ Obj val
evalExpr p ctx (Get (name:names)) cc =
    evalExpr p ctx (Get names) (Continuation $ \(Obj value) ->
        continue cc $ Obj $ getAttribute name value)

evalExpr p ctx (Call [localName, methodName] exprs) cc =
    evalExprs p ctx exprs [] (Continuation $ \(Objs actuals) ->
        evalExpr p ctx (Get [localName]) (Continuation $ \(Obj (ObjVal className attrs)) ->
            let
                Just klass = getClass className p
                Just method = getMethod methodName klass
                newOther = ContVal ctx $ cc
            in
                callMethod p newOther method actuals))

evalExpr p ctx (Mod names pairs) cc =
    continue cc $ Obj Null

evalExpr p ctx (IntLit i) cc =
    continue cc $ Obj $ IntVal (evalIntLit i)

evalExpr p ctx (New className) cc =
    continue cc $ Obj $ ObjVal className EmptyMap

---------------------------------------------------

evalExprs p ctx [] vals cc =
    continue cc $ Objs vals

evalExprs p ctx (expr:exprs) vals cc =
    evalExpr p ctx expr (Continuation $ \(Obj val) ->
        evalExprs p ctx exprs (val:vals) cc)

---------------------------------------------------

evalIntLit [] = 0
evalIntLit (d:ds) =
    (evalIntLit ds) * 10 + (digitVal d)

digitVal '0' = 0
digitVal '1' = 1
digitVal '2' = 2
digitVal '3' = 3
digitVal '4' = 4
digitVal '5' = 5
digitVal '6' = 6
digitVal '7' = 7
digitVal '8' = 8
digitVal '9' = 9

-- ========== DRIVER ========== --

pa x = do
  parseTest program x

runDeturgenchry programText =
    case parse (program) "" programText of
        Left err -> error (show err)
        Right prog -> show (evalProg prog)
