-- CSCI 360, Spring 2023
-- Project 3: the Quilt language

{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module Quilt where
import Parsing2
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
    ColorLit    :: Color -> Quilt  
    Number      :: Double -> Quilt 
    BoolLit     :: Bool -> Quilt 
    RGB         :: Quilt -> Quilt -> Quilt -> Quilt -- three numbers: red, green, blue values
    Con         :: Quilt -> Quilt -> Quilt -> Quilt 
    Bin         :: Op -> Quilt -> Quilt -> Quilt 
    Un          :: UOp -> Quilt -> Quilt 
    Coordinates :: Coord -> Quilt 
    QuiltOp     :: Quilt -> Quilt -> Quilt -> Quilt -> Quilt -- takes 4 bools and makes new bool, or 4 numbers and maks new num, etc.
    Var         :: String -> Quilt
    Let         :: String -> Quilt -> Quilt -> Quilt 
    deriving (Eq, Show)

data Coord where
    X :: Coord
    Y :: Coord
    deriving (Eq, Show)

data Op where    
    Plus    :: Op
    Minus   :: Op
    Times   :: Op
    Divide  :: Op 
    Less    :: Op
    Equal   :: Op
    Greater :: Op
    NotEqual :: Op 
    And     :: Op 
    Or      :: Op 
    deriving (Eq, Show)

data UOp where
    Neg :: UOp
    Not :: UOp
    deriving (Eq, Show) 

data Type where
    TypeBool  :: Type     -- represents Boolean values
    TypeNum   :: Type     -- represents floating-point numbers
    TypeColor :: Type     -- represents a triple of floating-point numbers
    deriving (Eq, Show)
    
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
    { reservedNames = [ "pink", "red", "orange", "yellow", "green", "blue", "purple", "black", "white", "gray", "x", "y", "if", "then", "else", "let", "in", "and", "or", "not" ] }

quilt :: Parser Quilt
quilt = whiteSpace *> parseQuilt <* eof

parseQuilt :: Parser Quilt
parseQuilt = buildExpressionParser table parseQuiltAtom
  where
    table = [ [ Prefix (Un Neg       <$ reservedOp "-")
            ,   Prefix (Un Not       <$ reservedOp "!") ]
            , [ Infix (Bin Times     <$ reservedOp "*") AssocLeft     
            ,   Infix (Bin Divide    <$ reservedOp "/") AssocLeft ]     
            , [ Infix (Bin Plus      <$ reservedOp "+") AssocLeft
            ,   Infix (Bin Minus     <$ reservedOp "-") AssocLeft ]
            , [ Prefix (Un Not       <$ reservedOp "!") ]
            , [ Infix (Bin Less      <$ reservedOp "<"  ) AssocNone
            ,   Infix (Bin Equal     <$ reservedOp "="  ) AssocNone
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

toColor :: String -> Color                  -- converts strings to colors
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


colorToBool :: Color -> Bool 
colorToBool [] = False
colorToBool (x:_) = x == 1.0

boolToColor :: Bool -> Color 
boolToColor True  = [1, 1, 1]
boolToColor False = [0, 0, 0]

---------------------
--- Type Checking ---
---------------------

type Ctx = M.Map String Type

data TypeError where
    DoLater :: TypeError
    TypeMismatchError :: TypeError 
    NotABool :: TypeError
    NotALit :: TypeError
    UndefinedVar :: String -> TypeError
    deriving (Show)

showTypeError :: TypeError -> String
showTypeError DoLater           = "this is a future problem"
showTypeError TypeMismatchError = "These types don't match"
showTypeError NotABool          = "This should be a boolean"
showTypeError NotALit           = "This should be a number "
showTypeError (UndefinedVar x)  = x ++ " is undefined"

inferType :: Ctx -> Quilt -> Either TypeError Type
inferType _ (ColorLit _)    = Right TypeColor
inferType _ (Number _)      = Right TypeNum
inferType _ (BoolLit _)     = Right TypeBool
inferType _ (Coordinates _) = Right TypeNum
inferType c (Let name q1 q2) = inferType c q1 >>= \t -> inferType (M.insert name t c) q2
inferType c (Var name)      = case M.lookup name c of
                                Just x -> Right x
                                Nothing -> Left $ UndefinedVar name
inferType c (Bin op e1 e2)  = inferType c e1 >>= \e1' ->
                              inferType c e2 >>= \e2' -> 
                              inferBOp op e1' e2'
inferType c (RGB r g b)     = inferType c r >>= \r' ->
                              inferType c g >>= \g' ->
                              inferType c b >>= \b' -> 
                                if r' == TypeNum && g' == TypeNum && b' == TypeNum 
                                then Right TypeColor else Left NotALit
inferType c (Con i t e)     = inferType c i >>= \i' ->
                                inferType c t >>= \t' ->
                                inferType c e >>= \e' -> 
                                (\i'' t'' e'' -> 
                                    if i'' == TypeBool
                                        then if t'' == e''
                                            then Right t''
                                            else Left TypeMismatchError
                                        else Left NotABool) i' t' e'
inferType c (QuiltOp q1 q2 q3 q4) = inferType c q1 >>= \t1 ->
                                      inferType c q2 >>= \t2 ->
                                      inferType c q3 >>= \t3 ->
                                      inferType c q4 >>= \t4 ->
                                        checkSubTypeHelper t1 t2 >>= \t1' ->
                                        checkSubTypeHelper t3 t4 >>= \t2' ->
                                        case checkSubTypeHelper t1' t2' of
                                                Left err -> Left err
                                                Right r -> Right r
inferType ctx (Un Neg e)          = case inferType ctx e of
                                        Right TypeBool -> Left NotABool
                                        Right ty -> Right ty
                                        Left err -> Left err
inferType ctx (Un Not e)          = case inferType ctx e of
                                        Right _ -> Right TypeBool
                                        Left err -> Left err

inferBoolBOp :: Op -> Type -> Type -> Either TypeError Type -- checks if types are compatible for boolean stuff
inferBoolBOp _ t1 t2 = case checkSubTypeHelper t1 t2 of
                        Right _  -> Right TypeBool
                        Left err -> Left err

inferBOp :: Op -> Type -> Type -> Either TypeError Type -- checks if types are compatible for binary math 
inferBOp Plus t1 t2   = checkSubTypeHelper t1 t2
inferBOp Minus t1 t2  = checkSubTypeHelper t1 t2
inferBOp Times t1 t2  = checkSubTypeHelper t1 t2
inferBOp Divide t1 t2 = checkSubTypeHelper t1 t2
inferBOp op  t1 t2    = inferBoolBOp op t1 t2           -- anything else is a boolean

checkSubTypeHelper :: Type -> Type -> Either TypeError Type -- checks if they are subtypes
checkSubTypeHelper TypeNum TypeColor = Right TypeColor
checkSubTypeHelper TypeColor TypeNum = Right TypeNum
checkSubTypeHelper type1 type2 
    | type1 == type2                 = Right type2
    | otherwise                      = Left TypeMismatchError

checkSubType :: Ctx -> Quilt -> Quilt -> Either TypeError Type
checkSubType ctx e1 e2 = inferType ctx e1 >>= \t1 -> inferType ctx e2 >>= \t2 ->
    checkSubTypeHelper t1 t2

---------------------
---- Interpretor ----
---------------------
type Env = M.Map String QuiltFun

data InterpError where
  Other        :: InterpError

showInterpError :: InterpError -> String
showInterpError Other            = "this error should never show up. uh oh." 

interpQuilt :: Env -> Quilt -> Either InterpError QuiltFun
interpQuilt _ (ColorLit c)    = Right $ \x y -> c
interpQuilt _ (Number num)    = Right $ \x y -> [num, num, num]
interpQuilt _ (BoolLit z)     = Right $ \x y -> boolToColor z   
interpQuilt _ (Coordinates X) = Right $ \x _ -> [x,x,x]
interpQuilt _ (Coordinates Y) = Right $ \_ y -> [y,y,y]
interpQuilt env (Var v)       = case M.lookup v env of
                                Just x -> Right x
                                Nothing -> Left Other
interpQuilt env (Let n e1 e2) = interpQuilt env e1 >>= (\x -> interpQuilt (M.insert n x env) e2)
interpQuilt env (RGB r g b)   = interpQuilt env r >>= \r' -> interpQuilt env g >>= \g' -> interpQuilt env b >>= \b' 
                                -> Right $ \x y -> [head $ r' x y, head $ g' x y, head $ b' x y]
interpQuilt env (Con i t e)   = interpQuilt env i >>= \i' -> interpQuilt env t >>= \t' -> interpQuilt env e >>= \e' 
                                  -> Right $ \x y -> if colorToBool $ i' x y then t' x y else e' x y
interpQuilt env (Un Neg e1)   = (\ f1 x y -> map (\ z -> - z) (f1 x y)) <$> interpQuilt env e1
interpQuilt env (Un Not e1)   = case interpQuilt env e1 of
                                  Left err -> Left err
                                  Right f1 -> Right $ \x y -> boolToColor (not (colorToBool (f1 x y)))
interpQuilt env (Bin And b1 b2)        = interpQuilt env (Con b1 b2 (BoolLit False))
interpQuilt env (Bin Or  b1 b2)        = interpQuilt env (Con b1 (BoolLit True) b2)
interpQuilt env (Bin NotEqual  b1 b2)  = interpQuilt env (Con (Bin Equal b1 b2) (BoolLit False) (BoolLit True))
interpQuilt env (Bin op quilt1 quilt2) = doMath op <$> interpQuilt env quilt1 <*> interpQuilt env quilt2 where
                                    doMath Plus  = binMath (+)
                                    doMath Minus = binMath (-)
                                    doMath Times = binMath (*)
                                    doMath Divide = binMath (/)
                                    doMath Less = comparison (<)
                                    doMath Equal = comparison (==)
                                    doMath Greater = comparison (>)                                  
                                    binMath o f1 f2 x y = zipWith o (f1 x y) (f2 x y)
                                    comparison o f1 f2 x y = boolToColor $ o (f1 x y) (f2 x y)
interpQuilt env (QuiltOp q1 q2 q3 q4) = (\q1' q2' q3' q4' x y ->
    case (x < 0, y > 0) of
      (True, True) -> q1' (x * 2 + 1) (y * 2 - 1)
      (False, True) -> q2' (x * 2 - 1) (y * 2 - 1)
      (True, False) -> q3' (x * 2 + 1) (y * 2 + 1)
      (False, False) -> q4' (x * 2 - 1) (y * 2 + 1))
       <$> interpQuilt env q1 <*> interpQuilt env q2 <*> interpQuilt env q3 <*> interpQuilt env q4


-- let checks  = quilt black white white black in let checks2 = quilt checks checks checks black in let checks3 = quilt checks2 checks2 checks2 black in let checks4 = quilt checks3 checks3 checks3 black in let checks5 = quilt checks4 checks4 checks4 black in checks5
-- quilt if x < y then red else blue let checks  = quilt black white white black in let checks2 = quilt checks checks checks black in let checks3 = quilt checks2 checks2 checks2 black in let checks4 = quilt checks3 checks3 checks3 black in let checks5 = quilt checks4 checks4 checks4 black in checks5 [(x+1)/2, (y+1)/2, 0.5] (quilt green orange blue purple)
-- let var = 3 in if ((var < 4) && (var > 0)) then green else purple
---------------
---- Eval -----
---------------

evalQuilt :: String -> Either String QuiltFun
evalQuilt s = case parse quilt s of
    Left err -> Left $ show err
    Right e  -> case inferType M.empty e of
        Left err -> Left $ showTypeError err
        Right _       -> case interpQuilt M.empty e of
            Left intpErr -> Left $ showInterpError intpErr
            Right v      -> Right v


