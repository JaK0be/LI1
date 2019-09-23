module Tarefa4s where

import Tarefa2s
import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe
import Data.List

{-Função basica que faz o decorrer de 1 instante de tempo-}
avanca:: [String] -> Int -> [String]
avanca k1 ti = checktime k1 ti 

{-Vai verificar se o tempo e igual a (n-2)^2, sendo n a dimensão do mapa, se for igual entao vai começar o efeito espiral, se ñ for remove 1 unidade de tempo,
e faz as respetivas alterações ao mapa-}
checktime:: [String] -> Int -> [String]
checktime k1 ti = if ti<=(x-2)^2 then chgchk (fSpiral k1 ti x) x else chgchk k1 x where 
    x = length (head k1) 

{-Vai testar se o mapa vai sofrer alteracoes ou nao-}
chgchk:: [String] -> Int -> [String]
chgchk k1 x = if chkBomb (simp k1) then timeRemove k1 x else k1

{-Esta vai verificar se existem bombas no mapa (visto que elas são as unicas afetadas pela passagem do tempo)-}
chkBomb:: [String] -> Bool
chkBomb [] = False
chkBomb (k:ks) = if head k == '*' then True else chkBomb ks 

{-Vai fazer alteração ao mapa, remover uma unidade de tempo ou explodir bombas-}
timeRemove:: [String] -> Int -> [String]
timeRemove k1 x = if xplodeChk (simp k1) then bombXplode k1 (bombGet k1) x else (simp2 k1) ++ (timeRemoveAux (simp k1))

{-Vai pesquisar a string e ver se existe alguma bomba com o timer em 1, se houver significa que esta vai explodir e destruir cenas-}
xplodeChk:: [String] -> Bool
xplodeChk [] = False
xplodeChk (k:ks) | head k == '*' = if spchk k 4 == "1" then True else xplodeChk ks
                 | otherwise = xplodeChk ks

{-Vai remover 1 unidade de tempo a todas as bombas-}
timeRemoveAux:: [String] -> [String]
timeRemoveAux [] = []
timeRemoveAux (k:ks) | head k == '*' = (tkspe k 0 5 ++ show ((read(spchk k 4)::Int) -1)) : timeRemoveAux ks
                     | otherwise = k: timeRemoveAux ks

{-Controlador geral das funções para quando existem bombas que vão explodir-}
bombXplode:: [String] -> [String] -> Int -> [String]
bombXplode k1 [] x = k1 
bombXplode k1 (b:bs) x = toBombChg (timeRemoveAux (deletEB (bombXplode (bombXplodeAux k1 b x) bs x))) (b:bs) x

{--}
bombXplodeAux:: [String] -> String -> Int -> [String]
bombXplodeAux k1 b x = qChk b (delPw (playerDChk (augusto k1 (pathCheck k1 (charGet k1 (read (spchk b 3)::Int) (posGet b) x))) (pathCheckSp $ charGet k1 (read (spchk b 3)::Int) (posGet b) x)) (pwChk k1 (pathCheckSp $ charGet k1 (read (spchk b 3)::Int) (posGet b) x)))  

{-Vai a lista de strings e vai pegar so nas bombas cujo o timer se encontra a 1-}
bombGet:: [String] -> [String]
bombGet [] = []
bombGet (k:ks) | head k == '*' = if spchk k 4 == "1" then k: bombGet ks else bombGet ks
               | otherwise = bombGet ks 

{-Vai dar uma lista de listas de strings, com os diferentes caracteres que se encontram no raio de explosão da bomba-}
charGet:: [String] -> Int -> (Int,Int) -> Int -> [[String]]
charGet k1 r (c,l) x = reverse (spup k1 r (c,l) x) : reverse (spdw k1 r (c,l) x) : reverse (splf k1 r (c,l) x) : [reverse (sprt k1 r (c,l) x)]

{-Vai dar os caracteres que se encontram a cima da posiçao da bomba-}
spup:: [String] -> Int -> (Int,Int) -> Int -> [String]
spup k1 0 (c,l) x = []
spup k1 r (c,l) x | l-r < 0 = spup k1 (r-1) (c,l) x
                  | otherwise = ((charChk k1 (c,l-r) : " ") ++ show c ++" "++ show (l-r)) : spup k1 (r-1) (c,l) x 

{-Vai dar os caracteres que se encontram em baixo da posiçao da bomba-}
spdw:: [String] -> Int -> (Int,Int) -> Int -> [String]
spdw k1 0 (c,l) x = []
spdw k1 r (c,l) x | l+r > (x-1) = spdw k1 (r-1) (c,l) x
                  | otherwise = ((charChk k1 (c,l+r) : " ") ++ show c ++" "++ show (l+r)) : spdw k1 (r-1) (c,l) x 

{-Vai dar os caracteres que se encontram a esquerda da posiçao da bomba-}
splf:: [String] -> Int -> (Int,Int) -> Int -> [String]
splf k1 0 (c,l) x = []
splf k1 r (c,l) x | c-r < 0 = splf k1 (r-1) (c,l) x
                  | otherwise = ((charChk k1 (c-r,l) : " ") ++ show (c-r) ++" "++ show l) : splf k1 (r-1) (c,l) x 

{-Vai dar os caracteres que se encontram a direita da posiçao da bomba-}
sprt:: [String] -> Int -> (Int,Int) -> Int -> [String]
sprt k1 0 (c,l) x = []
sprt k1 r (c,l) x | c+r > (x-1) = sprt k1 (r-1) (c,l) x
                  | otherwise = ((charChk k1 (c+r,l) : " ") ++ show (c+r) ++" "++ show l) : sprt k1 (r-1) (c,l) x 

{-Vai buscar os blocos Destrutiveis ou Indestrutiveis originantes da charGet, visto a saber onde e que o raio da bomba para e que blocos explode-}
pathCheck:: [String] -> [[String]] -> [String]
pathCheck k1 [] = []
pathCheck k1 (k:ks) = pathCheckAux k1 k : pathCheck k1 ks

{-Ajuda a de cima-}
pathCheckAux:: [String] -> [String] -> String
pathCheckAux k1 [] = []
pathCheckAux k1 [k] = k
pathCheckAux k1 (k:ks) | head k == '#' = k
                       | head k == '?' = k
                       | head k == ' ' = if pwChkpos k1 ('_':tail k) then k else pathCheckAux k1 ks

{-Esta função vai substituir os tijolos ? que foram afetados pela explosão da bomba por espaços vazios-}
augusto:: [String] -> [String] -> [String]
augusto k1 [] = k1
augusto k1 (c:cs) = augusto (stringSchSp k1 (posGet c) (head c)) cs

{-Vai procupar uma String especifica no mapa-}
stringSchSp:: [String] -> (Int,Int) -> Char -> [String]
stringSchSp [] (y,x) s = []
stringSchSp (k:ks) (y,x) s | s == ' ' || s == '#' = (k:ks)
                           | x>0 = k: stringSchSp ks (y,x-1) s
                           | x==0 = (posSchSp k y s) : ks

{-Vai procurar o char ? na String previamente procurada e substituilo por um espaço vazio-}
posSchSp::  String -> Int -> Char -> String
posSchSp (k:ks) y s | y>0 = k: posSchSp ks (y-1) s
                    | y==0 = ' ' : ks

{-Vai dar delete as bombas que explodiram nesta ronda-}
deletEB:: [String] -> [String]
deletEB [] = []
deletEB (k:ks) | head k == '*' = if spchk k 4 == "1" then deletEB ks else k: deletEB ks 
               | otherwise = k: deletEB ks

{-Vai buscar as posições pelo qual o raio da bomba vai passar para ver se alguma coincide com as posições dos jogadores-}
pathCheckSp:: [[String]] -> [[String]]
pathCheckSp [] = []
pathCheckSp (k:ks) = pathCheckSpAux k : pathCheckSp ks

{-Ajuda a de cima-}
pathCheckSpAux:: [String] -> [String]
pathCheckSpAux [] = []
pathCheckSpAux (k:ks) | head k == '#' = []
                      | head k == '?' = []
                      | head k == ' ' = ('_':tail k) : pathCheckSpAux ks

{-Vai verificar se existe algum jogador no caminho da bomba-}
playerDChk:: [String] -> [[String]] -> [String]
playerDChk [] c1 = []
playerDChk (k:ks) c1 | elem (head k) "0123" = if playerDChkAux k c1 then playerDChk ks c1 else k: playerDChk ks c1
                     | otherwise = k : playerDChk ks c1

{-Ajuda a de cima-}
playerDChkAux:: String -> [[String]] -> Bool
playerDChkAux k [] = False
playerDChkAux k (c:cs) = if playerDChkAux2 k c then True else playerDChkAux k cs

{-Ajuda a de cima-}
playerDChkAux2:: String -> [String] -> Bool
playerDChkAux2 k [] = False
playerDChkAux2 k (c:cs) = if posGet k == posGet c then True else playerDChkAux2 k cs

{-Vai verificar se existe algum jogador na posição onde a bomba se encontra-}
qChk:: String -> [String] -> [String]
qChk b  [] = []
qChk b (k:ks) | elem (head k) "0123" = if posGet b == posGet k then qChk b ks else k: qChk b ks 
              | otherwise = k: qChk b ks

{-Vai verificar se existe alguma bomba que foi apanhada no raio de explosão da outra e se sim vai meter-lhe o timer de explosão para 1-}
bombEchk:: [String] -> [[String]] -> [String]
bombEchk [] c1 = []
bombEchk (k:ks) c1 | head k == '*' = if bombEchkAux k c1 then (tkspe k 0 5 ++ "1") : bombEchk ks c1 else k: bombEchk ks c1
                   | otherwise = k : bombEchk ks c1

{-Ajuda a de cima-}
bombEchkAux:: String -> [[String]] -> Bool
bombEchkAux k [] = False
bombEchkAux k (c:cs) = if bombEchkAux2 k c then True else bombEchkAux k cs


{-Ajuda a de cima-}
bombEchkAux2:: String -> [String] -> Bool
bombEchkAux2 k [] = False
bombEchkAux2 k (c:cs) = if posGet k == posGet c then True else bombEchkAux2 k cs

{-Vai mudar os timers das bombas que se encontram no raio de explosão de uma para 1-}
toBombChg:: [String] -> [String] -> Int -> [String]
toBombChg k1 [] x = k1
toBombChg k1 (b:bs) x = toBombChg (toBombChgAux k1 b x) bs x

{-Ajuda a de cima-}
toBombChgAux:: [String] -> String -> Int -> [String]
toBombChgAux k1 b x = bombEchk k1 (pathCheckSp $ charGet k1 (read (spchk b 3)::Int) (posGet b) x)

{-Vai verificar se existe algum Pwup que se encontra no raio de explosão da bomba-}
pwChk:: [String] -> [[String]] -> [[String]]
pwChk k1 [] = []
pwChk k1 (c:cs) = pwChkAux k1 c : pwChk k1 cs

{-Ajuda a de cima-}
pwChkAux:: [String] -> [String] -> [String]
pwChkAux k1 [] = []
pwChkAux k1 (c:cs) | head c == '#' || head c == '?' = c: pwChkAux k1 cs
                   | head c == '_' = if pwChkpos k1 c then ('@' : drop 1 c) : pwChkAux k1 cs else c: pwChkAux k1 cs

{-Ajuda a de cima-}
pwChkpos:: [String] -> String -> Bool
pwChkpos [] c = False
pwChkpos (k:ks) c | elem (head k) "!+" = if posGet k == posGet c then True else pwChkpos ks c 
                  | otherwise = pwChkpos ks c

{-Vai apagar o pw's que foram afetados pelo raio da bomba, caso existam-}
delPw:: [String] -> [[String]] -> [String]
delPw k1 [] = k1
delPw k1 (c:cs) = delPw (delPwAux k1 c) cs

{-Ajuda a de cima-}
delPwAux:: [String] -> [String] -> [String]
delPwAux k1 [] = k1
delPwAux k1 (c:cs) = if head c == '@' then delPwAux (delPwAux2 k1 c) cs else delPwAux k1 cs

{-Ajuda a de cima-}
delPwAux2:: [String] -> String -> [String]
delPwAux2 [] c = []
delPwAux2 (k:ks) c | elem (head k) "!+" = if posGet k == posGet c then ks else k: delPwAux2 ks c 
                   | otherwise = k: delPwAux2 ks c

{-Cria a espiral-}
fSpiral:: [String] -> Int -> Int -> [String]
fSpiral k1 0 x = fSpiral k1 1 x
fSpiral k1 ti x = forward k1 (1,1) ((x-2)^2) ((x-2)^2) x (((x-2)^2)-ti) ((nbgiv x)++[1])

forward:: [String] -> (Int,Int) -> Int -> Int -> Int -> Int -> [Int] -> [String]
forward (k:ks) (c,l) ti te x 0 (n:bs) = stsch (k:ks) (c,l)
forward (k:ks) (c,l) ti te x tl (n:bs) | te == (ti-n) = downward (dlt (stsch (k:ks) (c,l)) (c,l)) (c,l+1) (ti-n) te x (tl-1) bs
                                       | otherwise = forward (dlt (stsch (k:ks) (c,l)) (c,l)) (c+1,l) ti (te-1) x (tl-1) (n:bs) 
 
downward:: [String] -> (Int,Int) -> Int -> Int -> Int -> Int -> [Int] -> [String]
downward (k:ks) (c,l) ti te x 0 (n:bs) = stsch (k:ks) (c,l)
downward (k:ks) (c,l) ti te x tl (n:bs) | te == (ti-n) = backward (dlt (stsch (k:ks) (c,l)) (c,l)) (c-1,l) (ti-n) te x (tl-1) bs
                                        | otherwise = downward (dlt (stsch (k:ks) (c,l)) (c,l)) (c,l+1) ti (te-1) x (tl-1) (n:bs) 

backward:: [String] -> (Int,Int) -> Int -> Int -> Int -> Int -> [Int] -> [String]
backward (k:ks) (c,l) ti te x 0 (n:bs) = stsch (k:ks) (c,l)
backward (k:ks) (c,l) ti te x tl (n:bs) | te == (ti-n) = upward (dlt (stsch (k:ks) (c,l)) (c,l)) (c,l-1) (ti-n) te x (tl-1) bs
                                        | otherwise = backward (dlt (stsch (k:ks) (c,l)) (c,l)) (c-1,l) ti (te-1) x (tl-1) (n:bs)
  
upward:: [String] -> (Int,Int) -> Int -> Int -> Int -> Int -> [Int] -> [String]
upward (k:ks) (c,l) ti te x 0 (n:bs) = stsch (k:ks) (c,l)
upward (k:ks) (c,l) ti te x tl (n:bs) | te == (ti-n) = forward (dlt (stsch (k:ks) (c,l)) (c,l)) (c+1,l) (ti-n) te x (tl-1) bs
                                      | otherwise = upward (dlt (stsch (k:ks) (c,l)) (c,l)) (c,l-1) ti (te-1) x (tl-1) (n:bs) 

{-Vai procurar por uma x linha no mapa-}
stsch:: [String] -> (Int,Int) -> [String]
stsch (k:ks) (c,0) = (posSchSpacial k c) : ks
stsch (k:ks) (c,l) = k: stsch ks (c,l-1)

{-vai substituir um x lugar na string obtida na stsch por uma pedra (efeito da espiral)-}
posSchSpacial:: String -> Int -> String
posSchSpacial (k:ks) 0 = '#' : ks
posSchSpacial (k:ks) c = k: posSchSpacial ks (c-1) 

{-Vai dar o numero de operações que cada função vai ter de fazer e quntas vezes cada função (upward,backward, etc) vai ter de ser executada-}
nbgiv:: Int -> [Int]
nbgiv x = (x-3) : nbgivAux (x-4)

{-Ajuda a de cima-}
nbgivAux:: Int -> [Int]
nbgivAux 0 = []
nbgivAux x = replicate 2 x ++ nbgivAux (x-1)

{-Vai dar delete a um PwU, Bomba, ou jogador que se encontre no lugar onde o novo tijolo foi colocado-}
dlt:: [String] -> (Int,Int) -> [String]
dlt [] (x,y) = []
dlt (k:ks) (x,y) | elem (head k) "*+!0123" = if posGet k == (x,y) then dlt ks (x,y) else k: dlt ks (x,y)
                 | otherwise = k: dlt ks (x,y)

{-["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]-}
{---------------------------------------------------------------------------------------------------------------------------------------------------------------}

