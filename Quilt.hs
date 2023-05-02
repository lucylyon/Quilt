-- CSCI 360, Spring 2023
-- Project 3: the Quilt language

{-# LANGUAGE GADTs #-}


module Quilt where
import Parsing2
import Text.Parsec.Error (Message(Message))
import qualified Data.Map as M

 --------------------
 ---- Data Types ----        
 --------------------

-- | A color is a list of red, green, and blue values between 0.0 - 1.0.
--   For example, [0,0,0] is black, [1,1,1] is white, [0.5, 0, 0.5] is a
--   darkish purple, and so on.
type Color = [Double]

-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].
type QuiltFun = Double -> Double -> Color

data Quilt where
    ColorLit    :: Color -> Quilt  -- COLORLIT
    Number      :: Double -> Quilt --NUMBERLOT
    BoolLit     :: Bool -> Quilt --SAME 
    RGB         :: Quilt -> Quilt -> Quilt -> Quilt -- three numbers: red, green, blue values --TRIPLE 
    Con         :: Quilt -> Quilt -> Quilt -> Quilt -- bool bool else  --IF
    Bin         :: Op -> Quilt -> Quilt -> Quilt 
    Un          :: Op -> Quilt -> Quilt 
    Coordinates :: Coord -> Quilt -- PARAM
    QuiltOp     :: Quilt -> Quilt -> Quilt -> Quilt -> Quilt -- takes 4 bools and makes new bool, or 4 numbers and maks new num, etc. --QUILTOP
    Var         :: String -> Quilt
    Let         :: String -> Quilt -> Quilt -> Quilt 
    deriving (Eq, Show)

    -- add sin/etc. instead of let?

data Coord where
    X :: Coord
    Y :: Coord
    deriving (Eq, Show)

data Op where               -- later change to binOp, boolOp 
    Plus :: Op
    Minus :: Op
    Times :: Op
    Divide :: Op 
    Less  :: Op
    LessEqual :: Op
    Equal :: Op
    NotEqual :: Op
    Greater :: Op
    GreaterEqual :: Op 
    Neg :: Op
    Not :: Op 
    And :: Op 
    Or :: Op 
    deriving (Eq, Show)

data Type where
    TypeBool  :: Type     -- represents Boolean values
    TypeNum   :: Type     -- represents floating-point numbers
    TypeColor :: Type     -- represents a triple of floating-point numbers
    deriving (Eq, Show)


-- data Uop where
--     Negate :: Uop
--     Not :: Uop
--     deriving (Eq, Show)

-- data ArithOp where
--     Plus :: ArithOp
--     Minus :: ArithOp
--     Times :: ArithOp
--     Divide :: ArithOp
--     deriving (Eq, Show)

-- data CompOp where
--     Less  :: CompOp
--     LessEqual :: CompOp
--     Equal :: CompOp
--     NotEqual :: CompOp
--     Greater :: CompOp
--     GreaterEqual :: CompOp
--     deriving (Eq, Show)

-- data BoolOp where
--     And :: BoolOp
--     Or :: BoolOp 
--     deriving (Eq, Show)
    
 ---------------------
 ----- Parsers -------       
 ---------------------

parens :: Parser a -> Parser a
parens = getParens lexer

identifier :: Parser String
identifier = getIdentifier lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

integer :: Parser Integer
integer    = getInteger lexer

float :: Parser Double
float = getFloat lexer

lexer :: TokenParser u
lexer = makeTokenParser $ emptyDef
    { reservedNames = [ "pink", "red", "orange", "yellow", "green", "blue", "purple", "black", "white", "gray", "x", "y", "if", "then", "else", "let", "in" ] }

quilt :: Parser Quilt
quilt = whiteSpace *> parseQuilt <* eof

parseQuilt :: Parser Quilt
parseQuilt = buildExpressionParser table parseQuiltAtom
  where
    table = [ [ Prefix (Un Neg       <$ reservedOp "-")
            ,   Prefix (Un Not       <$ reservedOp "!") ]
            , [ Infix (Bin Times     <$ reservedOp "*") AssocLeft      -- ^ is AssocRight so it follows math rules
            ,   Infix (Bin Divide    <$ reservedOp "/") AssocLeft ]     -- ex. 2^3^4 = 2^(3^4) instead of (2^3)^4 
            , [ Infix (Bin Plus      <$ reservedOp "+") AssocLeft
            ,   Infix (Bin Minus     <$ reservedOp "-") AssocLeft ]
            , [ Prefix (Un Not       <$ reservedOp "!") ]
            , [ Infix (Bin Less      <$ reservedOp "<"  ) AssocNone
            ,   Infix (Bin LessEqual <$ reservedOp "<=" ) AssocNone
            ,   Infix (Bin Equal     <$ reservedOp "="  ) AssocNone
            ,   Infix (Bin NotEqual  <$ reservedOp "!=" ) AssocNone
            ,   Infix (Bin Greater   <$ reservedOp ">"  ) AssocNone
            ,   Infix (Bin Greater   <$ reservedOp ">=" ) AssocNone ]
            , [ Infix (Bin And       <$ reservedOp "&&" ) AssocRight
            ,   Infix (Bin Or        <$ reservedOp "||" ) AssocRight ]
              ]

parseColorLit :: Parser Quilt
parseColorLit =
    ColorLit (toColor "pink")   <$ reserved "pink"    <|>
    ColorLit (toColor "red")    <$ reserved "red"     <|>
    ColorLit (toColor "orange") <$ reserved "orange"  <|>
    ColorLit (toColor "yellow") <$ reserved "yellow"  <|>
    ColorLit (toColor "green")  <$ reserved "green"   <|>
    ColorLit (toColor "blue")   <$ reserved "blue"    <|>
    ColorLit (toColor "purple") <$ reserved "purple"  <|>
    ColorLit (toColor "black")  <$ reserved "black"   <|>
    ColorLit (toColor "white")  <$ reserved "white"   <|>
    ColorLit (toColor "gray")   <$ reserved "gray" 

parseRGB :: Parser Quilt
parseRGB =  RGB
    <$> (reservedOp "["  *> parseQuilt)
    <*  reservedOp  "," <*> parseQuilt
    <*  reservedOp  "," <*> parseQuilt
    <*  reservedOp  "]"

parseNumber :: Parser Quilt
parseNumber = Number <$> (try float <|> fromIntegral <$> integer)

parseBool :: Parser Quilt
parseBool =
        BoolLit True  <$ reservedOp "True"
    <|> BoolLit False <$ reservedOp "False"

parseCoordinates :: Parser Quilt
parseCoordinates = Coordinates X <$ reserved "x" <|> Coordinates Y <$ reserved "y"

parseCon :: Parser Quilt 
parseCon = Con
     <$> (reserved "if"     *> parseQuilt)
     <*> (reserved "then"   *> parseQuilt)
     <*> (reserved "else"   *> parseQuilt)

parseQuiltOp :: Parser Quilt
parseQuiltOp = QuiltOp <$  reservedOp "quilt" <*> parseQuilt <*> parseQuilt <*> parseQuilt <*> parseQuilt
        
parseVar :: Parser Quilt
parseVar = Var <$> identifier

parseLet :: Parser Quilt
parseLet = Let 
    <$> (reserved    "let" *> identifier)
    <*> (reservedOp  "="   *> parseQuilt)
    <*> (reserved    "in"  *> parseQuilt)


parseQuiltAtom :: Parser Quilt
parseQuiltAtom =  parseColorLit 
 <|> parseNumber 
 <|> parseBool
 <|> parseRGB
 <|> parseCoordinates
 <|> parseCon
 <|> parseQuiltOp
 <|> parseVar
 <|> parseLet
 <|> parens parseQuilt

toColor :: String -> Color
toColor "pink"   = [1,   0,   1]
toColor "red"    = [1,   0,   0]
toColor "orange" = [1,   0.5, 0]
toColor "yellow" = [1,   1,   0]
toColor "green"  = [0,   0.5, 0]
toColor "blue"   = [0,   0,   1]
toColor "purple" = [0.5, 0, 0.5]
toColor "black"  = [0,   0,   0]
toColor "white"  = [1,   1,   1]
toColor "gray"   = [0.5, 0.5, 0.5]

colorToBool :: Color -> Bool -- to bool 
colorToBool (x:xs) = x == 1.0

boolToColor :: Bool -> Color -- from bool
boolToColor True  = [1, 1, 1]
boolToColor False = [0, 0, 0]

---------------------
--- Type Checking ---
---------------------

type Ctx = M.Map String Type

data TypeError where
    DoLater :: TypeError
    TypeMismatchError :: TypeError 
    deriving (Show)


showTypeError :: TypeError -> String
showTypeError DoLater = "undefined"
showTypeError TypeMismatchError = "These types don't match"

inferType :: Quilt -> Either TypeError ()
inferType _ = Right () -- TODO

checkSubType :: Type -> Type -> Bool 
checkSubType TypeNum TypeColor = True   -- num is a subtype of color
checkSubType type1 type2 
    | type1 == type2           = True   -- if the two types are the same, counts as subtype
    | otherwise                = False  -- anything else: not a subtype 


-- typechecking: recursively infer the types of subexpressions
-- then make sure types are compatible 
-- then compute appropriate output type
-- should make multiple functions & helpers for this 

---------------------
---- Interpretor ----
---------------------
type Env = M.Map String QuiltFun

data InterpError where
  UndefinedVar :: String -> InterpError
  Other        :: InterpError

showInterpError :: InterpError -> String
showInterpError (UndefinedVar x) = x ++ "is undefined"
showInterpError Other            = "this error should never show up. uh oh." 

interpQuilt :: Env -> Quilt -> Either InterpError QuiltFun
interpQuilt env (ColorLit c) = Right $ \x y -> c
interpQuilt env (Number num) = Right $ \x y -> [num, num, num]
interpQuilt env (BoolLit z) = Right $ \x y -> boolToColor z   
interpQuilt env (RGB r g b) = interpQuilt env r >>= \r' -> interpQuilt env g >>= \g' -> interpQuilt env b >>= \b' 
                                -> Right $ \x y -> [head $ r' x y, head $ g' x y, head $ b' x y]
interpQuilt env (Con i t e) = interpQuilt env i >>= \i' -> interpQuilt env t >>= \t' -> interpQuilt env e >>= \e' 
                                -> Right $ \x y -> if colorToBool $ i' x y then t' x y else e' x y
interpQuilt env (Bin op quilt1 quilt2) = runBin op <$> interpQuilt env quilt1 <*> interpQuilt env quilt2
    where
        runBin Plus  = applyArithm (+)
        runBin Minus = applyArithm (-)
        runBin Times = applyArithm (*)
        runBin Divide = applyArithm (/)
        runBin Less = applyComp (<)
        runBin LessEqual = applyComp (<=)
        runBin Equal = applyComp (==)
        runBin Greater = applyComp (>)
        runBin GreaterEqual = applyComp (>=)
        applyArithm op f1 f2 x y = zipWith op (f1 x y) (f2 x y)
        applyComp op f1 f2 x y = boolToColor $ op (f1 x y) (f2 x y)
interpQuilt env (Un op quilt) = undefined --- HERE
interpQuilt env (Coordinates X) = Right $ \x _ -> [x,x,x]
interpQuilt env (Coordinates Y) = Right $ \_ y -> [y,y,y]
interpQuilt env (QuiltOp q1 q2 q3 q4) = undefined --- HERE
interpQuilt env (Var name) = undefined --- HERE 
interpQuilt env (Let var value expr) = undefined --- HERE 


---------------
---- Eval -----
---------------

evalQuilt :: String -> Either String QuiltFun
evalQuilt s = case parse quilt s of
    Left err -> Left (show err) -- FIXME
    Right expr -> case inferType expr of
        Left typeErr -> Left $ showTypeError typeErr
        Right _ -> case interpQuilt M.empty expr of
            Left interpErr -> Left $ showInterpError interpErr
            Right f -> Right f

-- evalQuilt :: String -> Either String QuiltFun
-- evalQuilt s = case parse quilt s of
--     Left err -> Left $ show err -- FIXME
--     Right expr -> let desugared = desugar expr in
        -- case inferType M.empty desugared of
        -- Left inferErr -> Left $ showInferError inferErr
        -- Right _ -> case interpQuilt M.empty desugared of
        --     Left interpErr -> Left $ showInterpError interpErr
        --     Right f -> Right f
-- evalQuilt s = Right $ \x y -> [1, 0, 1]



