-- pex6.hs 
-- unKnot Haskell

-- name: Will Lockhart

{- DOCUMENTATION: None
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
    | null tripCode = "unknot"
    | type1Execution tripCode /= tripCode = unKnot (type1Execution tripCode)
    | type2Execution tripCode /= tripCode = unKnot (type2Execution tripCode)
    | otherwise = "tangle - resulting trip code: " ++ show tripCode

-- typeIknot :: [(Char, Char)] -> String
-- typeIknot tripCode

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print "   test case t01 - tripcode: " 
   print t01
   print("   result:" ++ unKnot t01)




-- ============================================================================
-- Type 1 Functions
-- ============================================================================

-- Determines if there is a type 1 case
type1Find :: [(Char, Char)] -> Bool
type1Find list
    | length list < 2 = False
    | fst (head list) == fst (head (tail list)) = True
    | fst (head list) == fst (lastElem list) = True
    | otherwise = type1Find (tail list)

-- Determines if the type 1 case happens at the First and Last indexs
type1FirstLastCase ::[(Char, Char)] -> Bool
type1FirstLastCase list
    | fst (head list) == fst (lastElem list) = True
    | otherwise = False

-- Returns the index of the last occurence of the start of a type 1 case
type1NormalCaseFindIndex list
    | not (type1Find list) = 0
    | otherwise = 1 + type1NormalCaseFindIndex (tail list)

-- Removes type 1 cases
type1Execution :: [(Char, Char)] -> [(Char, Char)]
type1Execution list
    | not (type1Find list) = list
    | type1FirstLastCase list = type1Execution (drop 1 (take (length list - 1) list))
    | otherwise = type1Execution (take (type1NormalCaseFindIndex list - 1) list ++ drop (type1NormalCaseFindIndex list + 1) list)


-- ============================================================================
-- Type 2 Functions
-- ============================================================================


-- First call should always be 1 to get index
type2FindFirst :: Num t => [(Char, Char)] -> t -> ((Char, Char), (t, Char))
type2FindFirst list index
    | length list < 2 = (('\0','\0'),(0, '\0'))
    | snd (head list) == snd (head (tail list)) = ((fst (head list), fst (head (tail list))), (index, snd (head list)))
    | otherwise = type2FindFirst (tail list) (index + 1)


type2FindSecond list firstInfo
    | snd (snd firstInfo) == '\0' = (0, 0)
-- Case when input is 'o'
    | elem (fst (fst firstInfo), 'u') list && elem (snd (fst firstInfo), 'u') list = (fst (snd firstInfo), type2FindSecondIndex list firstInfo 'u')
-- Case when input is 'u'
    | elem (fst (fst firstInfo), 'o') list && elem (snd (fst firstInfo), 'o') list = (fst (snd firstInfo), type2FindSecondIndex list firstInfo 'o')
    | otherwise = (0,0)

type2FindSecondIndex list firstInfo letter
    | length list < 2 = 0
    | elem (fst (fst firstInfo), letter) list && elem (snd (fst firstInfo), letter) list = 1 + type2FindSecondIndex (tail list) firstInfo letter
    | otherwise = 0

type2Execution list
    | fst (type2FindSecond list (type2FindFirst list 1)) == 0 = list
    | otherwise = take (snd (type2FindSecond list (type2FindFirst list 1)) - 3) (take (fst (type2FindSecond list (type2FindFirst list 1)) - 1) list ++ drop (fst (type2FindSecond list (type2FindFirst list 1)) + 1) list) ++ drop (snd (type2FindSecond list (type2FindFirst list 1)) - 1) (take (fst (type2FindSecond list (type2FindFirst list 1)) - 1) list ++ drop (fst (type2FindSecond list (type2FindFirst list 1)) + 1) list)


-- ============================================================================
-- Misc Functions
-- ============================================================================

-- Returns the last element of a list
lastElem :: [a] -> a
lastElem list
    | length list < 2 = head list
    | otherwise = lastElem (tail list)
