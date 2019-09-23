module Tarefa2s where

import Data.Char (isDigit)
import System.Environment
import Data.List
    
move :: [String] -> Int -> Char -> [String]  
move k1 x y  = upd (chkPlayer k1 x y) x y (pl k1 x)

chkPlayer:: [String] -> Int -> Char -> [String]
chkPlayer [] x y = []
chkPlayer k1 x y  | x==0 = simp2 k1 ++ fPl0 k1 (simp k1) x y
                  | x==1 = simp2 k1 ++ fPl1 k1 (simp k1) x y
                  | x==2 = simp2 k1 ++ fPl2 k1 (simp k1) x y
                  | x==3 = simp2 k1 ++ fPl3 k1 (simp k1) x y
                  | otherwise = k1
fPl0::[String] -> [String] -> Int -> Char -> [String]
fPl0 k1 [] x y = simp k1
fPl0 k1 (z:zs) x y | y=='B' = comandoB (simp k1) x (pl k1 x)
                   | head z == '0' = comando k1 y z : zs
                   | otherwise = z: fPl0 k1 zs x y
fPl1::[String] -> [String] -> Int -> Char -> [String]
fPl1 k1 [] x y = simp k1
fPl1 k1 (z:zs) x y | y=='B' = comandoB (simp k1) x (pl k1 x)
                   | head z == '1' = comando k1 y z : zs
                   | otherwise = z: fPl1 k1 zs x y
fPl2::[String] -> [String] -> Int -> Char -> [String]
fPl2 k1 [] x y = simp k1
fPl2 k1 (z:zs) x y | y=='B' = comandoB (simp k1) x (pl k1 x)
                   | head z == '2' = comando k1 y z : zs
                   | otherwise = z: fPl2 k1 zs x y
fPl3::[String] -> [String] -> Int -> Char -> [String]
fPl3 k1 [] x y = simp k1
fPl3 k1 (z:zs) x y | y=='B' = comandoB (simp k1) x (pl k1 x)
                   | head z == '3' = comando k1 y z : zs
                   | otherwise = z: fPl3 k1 zs x y

comando::[String] -> Char -> String -> String
comando k1 y [] = []
comando k1 y s2 | y == 'U' = if chkmove k1 s2 y then chkPu k1 (up s2) else s2
                | y == 'D' = if chkmove k1 s2 y then chkPu k1 (dw s2) else s2
                | y == 'R' = if chkmove k1 s2 y then chkPu k1 (rt s2) else s2
                | y == 'L' = if chkmove k1 s2 y then chkPu k1 (lf s2) else s2
                | otherwise = s2
chkmove:: [String] -> String -> Char -> Bool
chkmove k1 [] y = False
chkmove k1 s2 y | y=='U' = if m=='#'|| m=='?' then False else True 
                | y=='D' = if t=='#'|| t=='?' then False else True
                | y=='R' = if l=='#'|| l=='?' then False else True
                | y=='L' = if r=='#'|| r=='?' then False else True
 where
  m=charChk k1 (posGet (up s2))
  t=charChk k1 (posGet (dw s2))
  l=charChk k1 (posGet (rt s2))
  r=charChk k1 (posGet (lf s2)) 

posGet:: String -> (Int,Int)
posGet s2 =  (read (spchk s2 0) ::Int,read (spchk s2 1)::Int)
charChk::[String] -> (Int,Int) -> Char
charChk k1 (x,y) = posSch (stringSch k1 y) x
stringSch:: [String] -> Int -> String
stringSch l1 0 = head l1
stringSch (h:t) n = stringSch t (n-1) 
posSch:: String -> Int -> Char
posSch l2 n = (!!) l2 n
up:: String -> String
up s1 = tkspe s1 0 2 ++ show ((read(spchk s1 1)::Int) -1) ++ " " ++ dpspe s1 0 3
dw:: String -> String
dw s1 = tkspe s1 0 2 ++ show ((read(spchk s1 1)::Int) +1) ++ " " ++ dpspe s1 0 3
lf:: String -> String
lf s1 = tkspe s1 0 1 ++ show ((read(spchk s1 0)::Int) -1) ++ " " ++ dpspe s1 0 2
rt:: String -> String
rt s1 = tkspe s1 0 1 ++ show ((read(spchk s1 0)::Int) +1) ++ " " ++ dpspe s1 0 2
tkspe:: String -> Int -> Int -> String
tkspe [] n k = []
tkspe (x:y) n k | n==k = []
                | x==' ' = x : tkspe y (n+1) k
                | otherwise = x: tkspe y n k
tkspe2:: String -> Int -> Int -> String
tkspe2 [] n k = []
tkspe2 (x:y) n k | x==' ' && n== k-1 = []
                 | x==' ' = x : tkspe2 y (n+1) k
                 | otherwise = x: tkspe2 y n k
dpspe:: String -> Int -> Int -> String
dpspe [] n k = []
dpspe (x:y) n k | n==k = (x:y)
                | x==' ' = dpspe y (n+1) k
                | otherwise = dpspe y n k
spchk:: String -> Int -> String
spchk [] n = []
spchk (x:y:z) n | x==' ' && n==0 = spaux (y:z) ++ ""
                | x==' ' = spchk (y:z) (n-1)
                | otherwise = spchk (y:z) n
 where 
       spaux [] = []
       spaux (x:y) = if x==' ' then [] else x: spaux y 
simp::[String]->[String]
simp [] = []
simp (h:t) | elem (head h) "+!*0123" = (h:t)
           | otherwise = simp t 
simp2::[String]->[String]
simp2 [] = []
simp2 (h:t) | elem (head h) "+!*0123" = []
            | otherwise = h: simp2 t  
chkPu:: [String] -> String -> String
chkPu ss2 s2 = if c == "NOPE" then s2 else puGiv s2 c where
  c= posPu (simp ss2) (tkspe2 (dpspe s2 0 1) 0 2)
posPu:: [String] -> String -> String
posPu [] s2 = "NOPE"
posPu (h:hs) s2 = if (tkspe2 (dpspe h 0 1) 0 2)==s2 && elem (head h) "+!" then h else posPu hs s2 
givaux:: String -> Char -> String
givaux [] y = [y]
givaux (h:hs) y | h=='+' = h: givaux hs y
                | h==' ' = y: (h:hs)
                | h=='!' = y: (h:hs)
auxgiv:: String -> Char -> String
auxgiv [] y = [y]
auxgiv [h] y = if h=='+' then h : [y] else h : [y]
auxgiv (h:hs) y | h=='+' || h=='!' = h: auxgiv hs y
                | h==' ' = h:y:hs
puGiv:: String -> String -> String
puGiv [] [] = []
puGiv s2 s3 | head s3=='+' = if dpspe s2 0 3 == [] then tkspe s2 0 3 ++ " " ++ givaux (dpspe s2 0 3) '+' else tkspe s2 0 3 ++ givaux (dpspe s2 0 3) '+'
            | head s3=='!' = if dpspe s2 0 3 == [] then tkspe s2 0 3 ++ " " ++ auxgiv (dpspe s2 0 3) '!' else tkspe s2 0 3 ++ auxgiv (dpspe s2 0 3) '!'

comandoB:: [String] -> Int -> String -> [String]
comandoB k1s x s1 = if (plBb k1s s1) && (chkBb k1s s1) then grabPu k1s ++ (ordn (grabBb k1s) b) ++ grabPl k1s else k1s where
    b= "* " ++ (tkspe2 (dpspe s1 0 1) 0 2) ++ " " ++ show x ++ " " ++ show (nPuF s1) ++ " 10"
chkBb:: [String] -> String -> Bool
chkBb [] s1 = True
chkBb (k:ks) s1 | head k=='*' = if (tkspe2 (dpspe s1 0 1) 0 2)==(tkspe2 (dpspe k 0 1) 0 2) then False else chkBb ks s1
                | otherwise = chkBb ks s1
plBb:: [String] -> String -> Bool
plBb k1s [] = False
plBb k1s s1 | head s1 == '0' = if nPuB s1 > nBb k1s '0' then True else False
            | head s1 == '1' = if nPuB s1 > nBb k1s '1' then True else False
            | head s1 == '2' = if nPuB s1 > nBb k1s '2' then True else False
            | head s1 == '3' = if nPuB s1 > nBb k1s '3' then True else False
nBb:: [String] -> Char -> Int
nBb [] y = 0
nBb (k:ks) y | head k == '*' = if head (dpspe k 0 3) == y then 1+ nBb ks y else nBb ks y
             | otherwise = nBb ks y
nPuB:: String -> Int
nPuB s1 = length (elemIndices '+' s1) + 1

nPuF:: String -> Int
nPuF s1 = length (elemIndices '!' s1) + 1
grabPu:: [String] -> [String]
grabPu [] = []
grabPu (i:is) = if elem (head i) "+!" then i : grabPu is else grabPu is

grabBb:: [String] -> [String]
grabBb [] = []
grabBb (i:is) = if elem (head i) "*" then i : grabBb is else grabBb is

grabPl:: [String] -> [String]
grabPl [] = []
grabPl (i:is) = if elem (head i) "0123" then i : grabPl is else grabPl is

ordn:: [String] -> String -> [String]
ordn [] sb = [sb]
ordn (k:ks) sb | (read (spchk sb 1) ::Int) < (read (spchk k 1) ::Int) = sb : (k:ks)
               | (read (spchk sb 1) ::Int) == (read (spchk k 1) ::Int) = if (read (spchk sb 0) ::Int) < (read (spchk k 0) ::Int) then sb: (k:ks) else k: ordn ks sb
               | (read (spchk sb 1) ::Int) > (read (spchk k 1) ::Int) = k: ordn ks sb 

pl:: [String] -> Int -> String
pl [] x = []
pl (z:zs) x = if (head z : "") == show x then z else pl zs x

upd:: [String] -> Int -> Char -> String -> [String]
upd f1 x y [] = f1
upd f1 x y d1 | y=='B' = f1
              | y=='U' = if pl f1 x /= up d1 && pl f1 x /= d1 then delete (posPu (simp f1) (tkspe2 (dpspe (up d1) 0 1) 0 2)) f1 else f1
              | y=='D' = if pl f1 x /= dw d1 && pl f1 x /= d1 then delete (posPu (simp f1) (tkspe2 (dpspe (dw d1) 0 1) 0 2)) f1 else f1
              | y=='R' = if pl f1 x /= rt d1 && pl f1 x /= d1 then delete (posPu (simp f1) (tkspe2 (dpspe (rt d1) 0 1) 0 2)) f1 else f1
              | y=='L' = if pl f1 x /= lf d1 && pl f1 x /= d1 then delete (posPu (simp f1) (tkspe2 (dpspe (lf d1) 0 1) 0 2)) f1 else f1
