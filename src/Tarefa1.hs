import System.Environment
import Text.Read
import Data.Maybe
import System.Random

mapa :: Int -> Int -> [String]
mapa x y =(reString x (replace (gama (mkRandom m y)) (concat (mkmapGen x)))) ++ (chkPwUp x (replace (gama2 (mkRandom m y)) (concat (mkmapGen x))))
 where 
  chkPwUp:: Int -> String -> [String]
  chkPwUp x (a:as) = sortStr $ toString (tradutor (chkaux (a:as) 0) x) 
   where
    sortStr:: [String] -> [String]
    sortStr [] = []
    sortStr (h:t) = sortB (h:t) ++ sortF (h:t)
     where
      sortB [] = []
      sortB (h:t) = if head h == '+' then h: sortB t
                                     else sortB t
      sortF [] = []
      sortF (h:t) = if head h == '!' then h: sortF t
                                     else sortF t   
    toString:: [(Char,(Int,Int))] -> [String] 
    toString [] = []
    toString ((v,(z,q)):qs) = (v:" " ++ (show z ++ " " ++ show q)) : toString qs
    tradutor:: [(Char,Int)] -> Int -> [(Char,(Int,Int))]
    tradutor [] x = []
    tradutor ((v,z):vs) x = (v,((kaux z x),(taux z x))) : tradutor vs x
     where 
      taux:: Int -> Int -> Int 
      taux z x = if z>x then 1+ (taux (z-x) x)
                        else 0 
      kaux:: Int -> Int -> Int
      kaux z x = if z>x then kaux (z-x) x
                        else z
    chkaux:: String -> Int -> [(Char,Int)]
    chkaux [] i = []
    chkaux (a:as) i | a=='+' = [(a,i)] ++ chkaux as (i+1)
                    | a=='!' = [(a,i)] ++ chkaux as (i+1)
                    | otherwise = chkaux as (i+1)  
  gama2:: [Int] -> String
  gama2 [] = []
  gama2 (h:t) | h==0 || h==1 = '+' : gama2 t
              | h==2 || h==3 = '!' : gama2 t
              | h >= 4 && h<=39 = '?' : gama2 t
              | h >= 40 && h<= 99 = ' ' :gama2 t  
  reString:: Int -> String -> [String]
  reString x [] = []
  reString x (n:b) = r : reString x d where
     d = drop x (n:b)
     r = take x (n:b)
  replace::String -> String -> String
  replace l1 [] = []
  replace (f:g) (h:t) = if h=='.' then f: replace g t else h: replace (f:g) t
  m=x*x
  gama:: [Int] -> String
  gama [] = []
  gama (h:t) | h >= 0 && h<=39 = '?' : gama t
             | h >= 40 && h<= 99 = ' ' : gama t
  mkRandom:: Int -> Int -> [Int]
  mkRandom m y = take m $ randomRs (0,99) (mkStdGen y)
  mkmapGen :: Int -> [String]
  mkmapGen x = if x == 5 then mkmap5 1 else mkmap x 1
   where 
      mkmap5 n | n==1 = replicate 5 '#' : (mkmap5 (n+1))
               | n==2 || n==4 = ('#' : [' ',' ',' '] ++ "#") : (mkmap5 (n+1))
               | n==3 = "# # #" : (mkmap5 (n+1))
               | n==5 = replicate 5 '#' : []

      mkmap x n | n==1 = replicate x '#' : (mkmap x (n+1))
                | n==2 = ('#' : [' ',' '] ++ replicate j '.' ++ "  #") : (mkmap x (n+1))
                | n==3 = ("# " ++ (concat (replicate o "#.")) ++ "# #") : (mkmap x (n+1))
                | n==(x-2) = ("# " ++ (concat (replicate o "#.")) ++ "# #") : (mkmap x (n+1))
                | n==(x-1) = ('#' : [' ',' '] ++ replicate j '.' ++ "  #") : (mkmap x (n+1))
                | n==x = replicate x '#' : []
                | n>3 = if even n then ( '#' : replicate k '.' ++ "#") : (mkmap x (n+1))
                                  else oddaux x 1 : (mkmap x (n+1)) where 
                                                                     oddaux x w = if w >= x then "#"
                                                                                            else "#." ++ oddaux x (w+2)
      j = x-6
      k = x-2
      o = (div j 2) +1

main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"
