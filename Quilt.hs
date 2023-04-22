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

-- data Quilt where
--     QuiltColor :: Color -> Quilt
--     QuiltCoord :: Coord -> Quilt 
--     Num :: Double -> Quilt -- needs to be integer or floating point
--     QuiltBool :: Bool -> Quilt

data Quilt where
    ColorLit :: Color -> Quilt 
    Number   :: Double -> Quilt
    Coord    :: Double -> Double -> Quilt      
    BoolLit  :: Bool -> Quilt
    LitColorTriple :: Quilt -> Quilt -> Quilt -> Quilt -- three numbers
    BoolOp :: Quilt -> Quilt -- BoolLit -> BoolLit
    Comparison :: Quilt -> Quilt -> Quilt -- number -> number -> boolLit
    QuiltExpr ::  Quilt -> Quilt -> Quilt -> Quilt -> Quilt -- takes 4 bools and makes new bool, or 4 numbers and maks new num, etc. 
    Bin :: Op -> Quilt -> Quilt -> Quilt 
    deriving (Eq, Show)

data Op where
    Plus :: Op
    Minus :: Op
    Times :: Op
    Divide :: Op 
    deriving (Eq, Show)

-- arithmetic operators??

data QuiltType where
    TypeBool  :: QuiltType     -- represents Boolean values
    TypeNum   :: QuiltType     -- represents floating-point numbers
    TypeColor :: QuiltType     -- represents a triple of floating-point numbers
    deriving (Eq, Show)


    --       data Quilt where
    -- ColorLit :: Color -> Quilt
    -- NumberLit :: Double -> Quilt
    -- BoolLit :: Bool -> Quilt
    -- Triple :: Quilt -> Quilt -> Quilt -> Quilt
    -- Param :: Coord -> Quilt
    -- Bin :: Op -> Quilt -> Quilt -> Quilt
    -- Un :: Op -> Quilt -> Quilt
    -- If :: Quilt -> Quilt -> Quilt -> Quilt
    -- QuiltOp :: Quilt -> Quilt -> Quilt -> Quilt -> Quilt
    -- Var :: String -> Quilt
    -- Let :: String -> Quilt -> Quilt -> Quilt
    -- deriving (Show)

-- data Coord where
--     CoordX :: Coord
--     CoordY :: Coord
--     deriving (Show)

-- | A color is a list of red, green, and blue values between 0.0 - 1.0.
--   For example, [0,0,0] is black, [1,1,1] is white, [0.5, 0, 0.5] is a
--   darkish purple, and so on.
type Color = [Double]

-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].
type QuiltFun = Double -> Double -> Color
    

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
    { reservedNames = [ "pink", "red", "orange", "yellow", "green", "blue", "purple", "black", "white", "gray" ] }

quilt :: Parser Quilt
quilt = whiteSpace *> parseQuilt <* eof

parseColorLit :: Parser Quilt
parseColorLit =
        makeColorLitParser "pink"
    <|> makeColorLitParser "red"
    <|> makeColorLitParser "orange"
    <|> makeColorLitParser "yellow"
    <|> makeColorLitParser "green"
    <|> makeColorLitParser "blue"
    <|> makeColorLitParser "purple"
    <|> makeColorLitParser "black"
    <|> makeColorLitParser "white"
    <|> makeColorLitParser "gray"



 
parseNumber :: Parser Quilt
parseNumber = Number <$> (try float <|> fromIntegral <$> integer)

makeColorLitParser :: String -> Parser Quilt
makeColorLitParser s = (ColorLit $ toColor s) <$ reserved s

parseBool :: Parser Quilt
parseBool =
        BoolLit True  <$ reservedOp "True"
    <|> BoolLit False <$ reservedOp "False"

parseQuiltAtom :: Parser Quilt
parseQuiltAtom =  parseColorLit <|> parseNumber 
-- <|> parseBool

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

---------------------
--- Type Checking ---
---------------------

type Ctx = M.Map String QuiltType

-- checks if the 1st is a subtype of the 2nd 
isSubtypeOf :: QuiltType -> QuiltType -> Bool -- CHANGE TO CHECK SUBTYPE
isSubtypeOf TypeColor  TypeColor  = True
isSubtypeOf TypeNum TypeNum = True
isSubtypeOf TypeBool   TypeBool   = True
isSubtypeOf TypeNum TypeColor  = True
isSubtypeOf _        _        = False

-- Try to separate type inference involving subtyping into two phases: first, recursively infer the types of subexpressions; second, 
-- make sure their types are appropriately compatible, and if so compute an appropriate output type. You will probably want one or 
-- more helper functions to do this.

---------------------
---- Interpretor ----
---------------------

data InterpError where
  UndefinedVar :: String -> InterpError

showInterpError :: InterpError -> String
showInterpError (UndefinedVar x) = x ++ "is undefined"

type Env = M.Map String QuiltFun

interpQuilt :: Env -> Quilt -> Either InterpError QuiltFun
interpQuilt env (ColorLit c) = Right $ \x y -> c
interpQuilt env (Number z) = Right $ \x y -> [z,z,z]

---------------
---- Eval -----
---------------

evalQuilt :: String -> Either String QuiltFun
evalQuilt input = case parse quilt input of
  Left err -> Left (show err)       --ERROR Message
  Right expr -> case interpQuilt M.empty expr of 
        Left l -> Left (showInterpError l) 
        Right r -> Right r 

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



-- % > calc :: String -> String
-- % > calc input = case parse (parseArith <* eof) input of         
-- % >          Left l -> show l                               -- if there's a parse error, show it
-- % >          Right r -> case interpArith M.empty r of       -- interpret it
-- % >                      Left err -> showInterpError err    -- if there's an interpretation error, show it
-- % >                      Right answer -> prettyPrint r ++ "\n   = " ++ printf "%f" answer 
-- % >                      -- everything works, do the math, display it 

-- > calc2 :: String -> String
-- > calc2 input = case parse (parseArith <* eof) input of 
-- >         Left l -> show l 
-- >         --Right expr ->  case inferType expr of 
-- >              --  Left l -> showTypeError l 
-- >         Right expr -> case interpArith M.empty expr of 
-- >                 Left err -> showInterpError err 
-- >                 Right answer ->  showAs answer 


parseQuilt :: Parser Quilt
parseQuilt = buildExpressionParser table parseQuiltAtom
  where
    table = [ 
            [ Infix (Bin Times <$ reservedOp "*") AssocLeft
              , Infix (Bin Divide   <$ reservedOp "/") AssocLeft
              ]
            , [ Infix (Bin Plus  <$ reservedOp "+") AssocLeft
              , Infix (Bin Minus <$ reservedOp "-") AssocLeft
              ]
              ]
