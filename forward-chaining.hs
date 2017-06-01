--Use forward chaining with the bottom-up procedure to compute the well-founded model
--Feed the program an input file containing a set of rules, and the sets of true and false atoms will be spat out to an output file.

import Data.String
import Data.Char
import System.Environment
import Data.List

data Rule = Rule Atom [Atom] [Atom] deriving (Show, Eq)
type Atom = String

getHead (Rule h _ _) = h
getT (Rule _ t _) = t
getF (Rule _ _ f) = f

ex1 = Rule "a" ["b1","b2"] ["c","c10"]
ex2 = Rule "h" ["a","b","c"] []
ex3 = Rule "g" [] ["e","f"]
ex4 = Rule "f" [] []
ex5 = Rule "c" ["f"] []
exes = [ex1, ex2, ex3, ex4, ex5]

main = do
    [fileName] <- getArgs
    contents <- readFile fileName
    let contentLines = map trim (lines contents)
    let rules = parseLines contentLines
    let result = "Sets T and F at each step:\n" ++ (unlines $ map stringify $ getWFM rules) where
        stringify :: ([Atom],[Atom]) -> String
        stringify (ts, fs) = "T = {" ++ (unwords ts) ++ "} F = {" ++ (unwords fs) ++ "}"
    putStrLn $ result
    writeFile (fileName ++ ".out") result
    where trim = f . f 
          f = reverse . dropWhile isSpace


parseLines :: [String] -> [Rule]
parseLines loins = map parseRuleString $ stripComments . stripLines $ loins where
    stripLines = filter f where
        f [] = False
        f (x:xs) =  not (isSpace x || (x == '#')) 
    stripComments = map (takeWhile (/= '#'))


parseRuleString :: String -> Rule
parseRuleString str = Rule h (str2list tlist) (str2list flist) where
    nospaces = filter (not . isSpace) str
    inner = extract '[' ']' nospaces
    h = takeWhile isAlphaNum inner
    (tlist, flist) = case (elemIndex ']' inner) of
                        Just index -> splitAt (index + 1) inner
                        Nothing -> error $ "Error, malformed inner rule: " ++ inner

--takes string of form "[a, bb,c]", returns ["a"," bb","c"]
str2list :: String -> [String]
str2list str = explode (extract '[' ']' str) ','

explode :: (Eq a) => [a] -> a -> [[a]]
explode [] _ = [] --monads are like burritos that eat other burritos
explode list sep = filter (not . null) $ beforeSep : explode (afterSep rest) sep where
    (beforeSep, rest) = span (/= sep) list 
    afterSep [] = []
    afterSep (x:xs) = xs

--returns sublist surrounded by l and r. If no such sublist returns empty list.
extract :: (Eq a) => a -> a -> [a] -> [a]
extract l r list = beforerafterl where
    beforerafterl = reverse $ tail' $ dropWhile (/= r) $ reverse afterl
    afterl = (tail' . (dropWhile (/= l))) list 
    tail' [] = []
    tail' a = tail a

----------------------- Computing the well-founded model ------------------
r1 = Rule "p" ["q"] ["r"]
r2 = Rule "p" ["s"] []
r3 = Rule "q" ["u"] []
r4 = Rule "u" [] ["s"]
r5 = Rule "r" [] ["t"]
r6 = Rule "t" [] []
r7 = Rule "s" ["w"] []
r8 = Rule "n" ["m"] []
r9 = Rule "m" ["n"] []
rs = [r1, r2, r3, r4, r5, r6, r7, r8, r9]

getWFM :: [Rule] -> [([Atom], [Atom])]
getWFM rules = loop rules ([],[])

loop :: [Rule] -> ([Atom], [Atom]) -> [([Atom],[Atom])]
loop rules oldies@(ts, fs)
    | {-(ts == newTs) && (fs == newFs) && (rules == newRules)-} length rules == 0   = []
    | otherwise = (:) ans $ loop newRules ans where
        ans = (ts ++ newTs, fs ++ newFs ) 
        (newRules', (newTs, _)) = runTrue rules
        (newRules, (_, newFs)) = runFalse newRules'

runTrue :: [Rule] -> ([Rule], ([Atom],[Atom]))
runTrue rules = (newRules, (ts, [])) where
    ts = bottomUp $ filter noNeg rules
    newRules = removeFromTrue ts $ filter (notInHeadOrNegs) rules where
        notInHeadOrNegs (Rule h _ negs) = (h `notElem` ts) && (negs `intersect` ts == [])


runFalse :: [Rule] -> ([Rule], ([Atom],[Atom]))
runFalse rules = (newRules, ([], fs)) where
    tpos = bottomUp $ map removeNegs rules where
        removeNegs (Rule h t _) = Rule h t []
    fs = filter (`notElem` tpos) allAtoms where
        allAtoms = getAllAtoms rules
    newRules = removeFromFalse fs $ filter notInHeadOrPos rules where
        notInHeadOrPos (Rule h pos _) = (h `notElem` fs) && (pos `intersect` fs == [])


--runs bottom-up procedure on ruleset and returns true atoms
--rules must not have any negated atoms
--1) scan for bodiless heads
--2) remove those found from bodies
--3) repeat 1) & 2) until you find no new heads
bottomUp :: [Rule] -> [Atom]
bottomUp [] = []
bottomUp rules 
    | null heads = []
    | otherwise  = heads ++ (bottomUp $ removeFromTrue heads $ removeBodilessRules rules) where
        heads = getJustHeads rules 
        removeBodilessRules :: [Rule] -> [Rule]
        removeBodilessRules rules = filter (not . noBody) rules

--returns heads of rules with no bodies
getJustHeads :: [Rule] -> [Atom]
getJustHeads rules = map getHead $ filter noBody rules

getAllHeads :: [Rule] -> [Atom]
getAllHeads [] = []
getAllHeads ((Rule h _ _) : rest) = h : getAllHeads rest

getAllAtoms :: [Rule] -> [Atom]
getAllAtoms [] = []
getAllAtoms rules = nub $ getAllAtoms' rules where --remove dupes
    getAllAtoms' ((Rule h ts fs) : rest) = h : (ts ++ fs ++ getAllAtoms rest)

--removes given atoms from the true atoms of every rule. No rules are removed.
removeFromTrue :: [Atom] -> [Rule] -> [Rule]
removeFromTrue _ [] = []
removeFromTrue atoms rules = map f rules where
    f (Rule h ts fs) = Rule h (filter (\ x -> notElem x atoms) ts) fs

--removes given atoms from the false atoms of every rule. No rules are removed.
removeFromFalse :: [Atom] -> [Rule] -> [Rule]
removeFromFalse _ [] = []
removeFromFalse atoms rules = map f rules where
    f (Rule h ts fs) = Rule h ts (filter (\ x -> notElem x atoms) fs)

noBody rule = noPos rule && noNeg rule

noNeg :: Rule -> Bool
noNeg (Rule _ _ []) = True
noNeg _ = False

noPos :: Rule -> Bool
noPos (Rule _ [] _) = True
noPos _ = False

removeNegation :: Rule -> Rule
removeNegation (Rule h t _) = Rule h t []
