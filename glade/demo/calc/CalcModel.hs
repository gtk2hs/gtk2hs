-- A simple push button calcualtor without operator precedence

module CalcModel (
  Number,
  Calc,
  BinOp, plus, minus, times, divide,
  clearCalc, enterDigit, enterDecimalPoint, enterBinOp, evaluate
  ) where

import Char     (isDigit)
import Monad    (when)
import Numeric  (showGFloat)

-- we could change this to rational
type Number = Double

data Calc = Calc {
    number   :: [Digit],
    operator :: BinOp,
    total    :: Number,
    resetOnNum :: Bool    -- a state flag, after pressing '=', if we enter an
  }                       -- operator then we're carrying on the previous
                          -- calculation, otherwise we should start a new one.

data Digit = Digit Int    -- in range [0..9]
           | DecimalPoint
  deriving Eq

data BinOp = BinOp (Number -> Number -> Number)

plus, minus, times, divide :: BinOp
plus   = BinOp (+)
minus  = BinOp (-)
times  = BinOp (*)
divide = BinOp (/)

clearCalc :: Calc
clearCalc = Calc {
    number = [],
    operator = plus,
    total = 0,
    resetOnNum = True
  }

-- Maybe for the case when the operation makes no sense
enterDigit :: Int -> Calc -> Maybe (String, Calc)
enterDigit digit calc
  | digit `elem` [0..9]
 && not (number calc == [] && digit == 0)
  = let newNumber = number calc ++ [Digit digit]
     in if resetOnNum calc
          then Just (show newNumber,
                     calc {
                       number = newNumber,
                       total = 0,
                       resetOnNum = False
                     })
          else Just (show newNumber, calc { number = newNumber })
  | otherwise = Nothing

enterDecimalPoint :: Calc -> Maybe (String, Calc)
enterDecimalPoint calc
  | DecimalPoint `notElem` number calc
  = let newNumber = number calc ++ [DecimalPoint]
     in if resetOnNum calc
          then Just (show newNumber,
                     calc {
                       number = newNumber,
                       total = 0,
                       resetOnNum = False
                     })
          else Just (show newNumber, calc { number = newNumber })
  | otherwise = Nothing

enterBinOp :: BinOp -> Calc -> Maybe (String, Calc)
enterBinOp binop calc =
  let newTotal = (case operator calc of BinOp op -> op)
                   (total calc)
                   (digitsToNumber (number calc))
   in Just (showNumber newTotal,
            Calc {
              number = [],
              operator = binop,
              total = newTotal,
              resetOnNum = False
            })

evaluate :: Calc -> Maybe (String, Calc)
evaluate calc = 
  let newTotal = (case operator calc of BinOp op -> op)
                   (total calc)
                   (digitsToNumber (number calc))
   in Just (showNumber newTotal,
            Calc {
              number = [],
              operator = plus,
              total = newTotal,
              resetOnNum = True
            })

instance Show Digit where
  show (Digit n) = show n
  show DecimalPoint = "."
  showList = showString . concatMap show

digitsToNumber :: [Digit] -> Number
digitsToNumber [] = 0
digitsToNumber digits@(DecimalPoint:_) = digitsToNumber (Digit 0:digits)
digitsToNumber digits | last digits == DecimalPoint
                                  = digitsToNumber (init digits)
                      | otherwise = read (show digits) --CHEAT!

precision = Just 5  --digits of precision, or Nothing for as much as possible

showNumber :: Number -> String
showNumber num =
  if '.' `elem` numStr then stripTrailingZeros numStr
                       else numStr
  where numStr = showGFloat precision num ""
        stripTrailingZeros =
            reverse
          . (\str -> if head str == '.' then tail str else str)
          . dropWhile (\c -> c=='0')
          . reverse

testProg :: IO ()
testProg = do
  evalLoop clearCalc
  
  where evalLoop :: Calc -> IO ()
        evalLoop calc = do
          putStr "calc> "
          line <- getLine
          when (line /= "q") $ do
            result <- case line of
                        [digit] | isDigit digit
                            -> return $ enterDigit (read [digit]) calc 
                        "." -> return $ enterDecimalPoint calc
                        "+" -> return $ enterBinOp plus calc
                        "-" -> return $ enterBinOp minus calc
                        "*" -> return $ enterBinOp times calc
                        "/" -> return $ enterBinOp divide calc
                        "=" -> return $ evaluate calc
                        "c" -> return $ Just ("0",clearCalc)
                        _ -> do putStrLn "invalid input"
                                return Nothing
            case result of
              Nothing -> evalLoop calc
              Just (display, calc') -> do putStrLn display
                                          evalLoop calc'
