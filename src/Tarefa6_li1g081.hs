module Tarefa6_li1g081 where

{- | A Função __@/bot/@__ recebe um Estado de Jogo e produz uma ação consoante esse mesmo Estado.
-}
bot :: [String] -> Int -> Int -> Maybe Char
bot mapa player ticks = if elem (princ mapa player ticks) "BURDL" then Just (princ mapa player ticks) else Nothing

{- | A Função __@/princ/@__ é a função principal.
-}
princ:: [String] -> Int -> Int -> Char
princ k1 pln ti = if ti<=((length (head k1))-2)^2 then yesSpiral k1 pln else noSpiral k1 pln

{- | A Função __@/noSpiral/@__ é a Função Principal, antes da Espiral começar.
-}
noSpiral:: [String] -> Int -> Char
noSpiral k1 pln = if bbDet k1 (bombGet k1) (pll k1 pln) x == 'N' then comand k1 (posGet (pll k1 pln)) x pln else bbDet k1 (bombGet k1) (pll k1 pln) x
 where
  x= length (head k1)

{- | A Função __@/comand/@__ gera os Comandos do bot, quando este não está no raio de explosão de bombas.
-}
comand:: [String] -> (Int,Int) -> Int -> Int -> Char
comand k1 (c,l) x pln = if chkNear k1 (c,l) x then bomblimit k1 (c,l) x else getComand k1 (c,l) x

{- | A Função __@/chkNear/@__ verifica se existe algum bloco destrutivel perto do jogador, caso exista, coloca uma bomba.
-}
chkNear:: [String] -> (Int,Int) -> Int -> Bool
chkNear k1 (c,l) x = if charChk k1 (c+1,l) == '?' || charChk k1 (c,l+1) == '?' || charChk k1 (c-1,l) == '?' || charChk k1 (c,l-1) == '?' then True else False

{- | A Função __@/getComand/@__ diz o comando que o bot irá executar, para este se  aproximar de um bloco destrutível.
-}
getComand:: [String] -> (Int,Int) -> Int -> Char
getComand k1 (c,l) x = up k1 (c,l) x 0

{- | A Função __@/up/@__ verifica se existem blocos na posição acima do bot.
-}
up:: [String] -> (Int,Int) -> Int -> Int -> Char
up k1 (c,l) x 10 = spotSchUp k1 (c,l) 0
up k1 (c,l) x i | (l-2-i) <= 0 = right k1 (c,l) x i
                | (l-2-i) > 0 = if charChk k1 (c,l-2-i) == '?' then spotSchUp k1 (c,l) 0 else right k1 (c,l) x i

{- | A Função __@/right/@__ verifica se existem blocos na posição à direita do bot.
-}
right:: [String] -> (Int,Int) -> Int -> Int -> Char
right k1 (c,l) x 10 = spotSchRt k1 (c,l) 0
right k1 (c,l) x i | (c+2+i) >= x = down k1 (c,l) x i
                   | (c+2+i) < x = if charChk k1 (c+2+i,l) == '?' then spotSchRt k1 (c,l) 0 else down k1 (c,l) x i

{- | A Função __@/down/@__ verifica se existem blocos na posição abaixo do bot.
-}
down:: [String] -> (Int,Int) -> Int -> Int -> Char
down k1 (c,l) x 10 = spotSchDw k1 (c,l) 0
down k1 (c,l) x i | (l+2+i) >= x = left k1 (c,l) x i
                  | (l+2+i) < x = if charChk k1 (c,l+2+i) == '?' then spotSchDw k1 (c,l) 0 else left k1 (c,l) x i

{- | A Função __@/left/@__ verifica se existem blocos na posição à esquerda do bot.
-}
left:: [String] -> (Int,Int) -> Int -> Int -> Char
left k1 (c,l) x 10 = spotSchLf k1 (c,l) 0
left k1 (c,l) x i | (c-2-i) <= 0 = up k1 (c,l) x (i+1)
                  | (c-2-i) > 0 = if charChk k1 (c-2-i,l) == '?' then spotSchLf k1 (c,l) 0 else up k1 (c,l) x (i+1)

{- | A Função __@/getExp/@__ obtem uma lista com elementos afetadas pelo raio da bomba.
-}
getExp:: [String] -> String -> Int -> [[String]]
getExp k1 b x = [tkspe2 b 0 3] : (pathCheckSp $ charGet k1 (read (spchk b 3)::Int) (posGet b) x)

{- | A Funçãoc__@/bbDet/@__ é o __Controlador Geral__ das funções que originam comandos quando existem bomas perto do bot.
-}
bbDet:: [String] -> [String] -> String -> Int -> Char
bbDet k1 [] pl x = 'N'
bbDet k1 (b:bs) pl x = if plrCheck (posGet pl) (getExp k1 b x) then moveChk k1 pl b else bbDet k1 bs pl x

{- | A Função __@/plrCheck/@__ verifica se o bot se encontra dentro dos elementos afetados pelo raio de explosão de uma bomba.
-}
plrCheck:: (Int,Int) -> [[String]] -> Bool
plrCheck pl [] = False
plrCheck pl (b:bs) = if plrCheckAux pl b then True else plrCheck pl bs

{- | A Função __@/plrCheckAux/@__ é uma função auxiliar da __@/plrCheck/@__.
-}
plrCheckAux:: (Int,Int) -> [String] -> Bool
plrCheckAux pl [] = False
plrCheckAux pl (b:bs) = if pl == posGet b then True else plrCheckAux pl bs

{- | A Função __@/moveChk/@__ verifica a posição do bot face à bomba, e dá um comando com base nessa posição.
-}
moveChk:: [String] -> String -> String -> Char
moveChk k1 pl [] = 'N'
moveChk k1 pl b | posGet pl == posGet b = spotSchUp k1 (posGet pl) 0 {-O Jogador encontra-se na mesma posição que a Bomba-}
                | (read (spchk pl 1) ::Int) < (read (spchk b 1) ::Int) = spotSchLf k1 (posGet pl) 0 {-O Jogador encontra-se acima da bomba-}
                | (read (spchk pl 1) ::Int) > (read (spchk b 1) ::Int) = spotSchRt k1 (posGet pl) 0 {-O Jogador encontra-se debaixo da bomba-}
                | ((read (spchk pl 1) ::Int) == (read (spchk b 1) ::Int)) && ((read (spchk pl 0) ::Int) < (read (spchk b 0) ::Int)) = spotSchDw k1 (posGet pl) 0 {-O Jogador encontra-se a esquerda da bomba-}
                | ((read (spchk pl 1) ::Int) == (read (spchk b 1) ::Int)) && ((read (spchk pl 0) ::Int) > (read (spchk b 0) ::Int)) = spotSchUp k1 (posGet pl) 0 {-O Jogador encontra-se a direita da bomba-}

{- | A Função __@/spotSchUp/@__ verifica se é possível mover para uma posição acima da atual.
-}
spotSchUp:: [String] -> (Int,Int) -> Int -> Char
spotSchUp k1 (c,l) 5 = onPathChk k1 (c,l-1) 'U'
spotSchUp k1 (c,l) i = if charChk k1 (c,l-1) == '?' || charChk k1 (c,l-1) == '#' then spotSchRt k1 (c,l) (i+1) else onPathChk k1 (c,l) 'U'

{- | A Função __@/spotSchRt/@__ verifica se é possível mover para uma posição à direita da atual.
-}
spotSchRt:: [String] -> (Int,Int) -> Int -> Char
spotSchRt k1 (c,l) 5 = onPathChk k1 (c+1,l) 'R'
spotSchRt k1 (c,l) i = if charChk k1 (c+1,l) == '?' || charChk k1 (c+1,l) == '#' then spotSchDw k1 (c,l) (i+1) else onPathChk k1 (c,l) 'R'

{- | A Função __@/spotSchDw/@__ verifica se é possível mover para uma posição abaixo da atual.
-}
spotSchDw:: [String] -> (Int,Int) -> Int -> Char
spotSchDw k1 (c,l) 5 = onPathChk k1 (c,l+1) 'D'
spotSchDw k1 (c,l) i = if charChk k1 (c,l+1) == '?' || charChk k1 (c,l+1) == '#' then spotSchLf k1 (c,l) (i+1) else onPathChk k1 (c,l) 'D'

{- | A Função __@/spotSchLf/@__ verifica se é possível mover para uma posição à esquerda da atual.
-}
spotSchLf:: [String] -> (Int,Int) -> Int -> Char
spotSchLf k1 (c,l) 5 = onPathChk k1 (c-1,l) 'L'
spotSchLf k1 (c,l) i = if charChk k1 (c-1,l) == '?' || charChk k1 (c-1,l) == '#' then spotSchUp k1 (c,l) (i+1) else onPathChk k1 (c,l) 'L'

{- | A Função __@/onPathChk/@__ verifica se o lugar para onde o bot se pretende mover está dentro do raio de explosão de uma bomba, se sim ele este não muda de posição, se não tem permissão para se mover.
-}
onPathChk:: [String] -> (Int,Int) -> Char -> Char
onPathChk k1 pl y = onPathChkAux k1 pl y (bombGet k1) (length (head k1))

{- | A Função __@/onPathChkAux/@__ é uma função auxiliar da __@/onPathChk/@__.
-}
onPathChkAux:: [String] -> (Int,Int) -> Char -> [String] -> Int -> Char
onPathChkAux k1 pl y [] x = y
onPathChkAux k1 (c,l) y (b:bs) x | y=='U' = if onPathChkAux3 k1 (c,l) (b:bs) x then 'U' else onPathChkAux2 k1 (c,l-1) y (b:bs) x
                                 | y=='R' = if onPathChkAux3 k1 (c,l) (b:bs) x then 'R' else onPathChkAux2 k1 (c+1,l) y (b:bs) x
                                 | y=='D' = if onPathChkAux3 k1 (c,l) (b:bs) x then 'D' else onPathChkAux2 k1 (c,l+1) y (b:bs) x
                                 | y=='L' = if onPathChkAux3 k1 (c,l) (b:bs) x then 'L' else onPathChkAux2 k1 (c-1,l) y (b:bs) x

{- | A Função __@/onPathChkAux2/@__ é uma função auxiliar da __@/onPathChkAux/@__ (Verifica se o bot se encontra no raio de explosão da bomba).
-}
onPathChkAux2:: [String] -> (Int,Int) -> Char -> [String] -> Int -> Char
onPathChkAux2 k1 pl y [] x = y
onPathChkAux2 k1 pl y (b:bs) x = if plrCheck pl (getExp k1 b x) then 'S' else onPathChkAux2 k1 pl y bs x

{- | A Função __@/onPathChkAux3/@__ é uma função auxiliar da __@/onPathChkAux2/@__ (Verifica se o bot se encontrava inicialmente no raio de explosão da bomba).
-}
onPathChkAux3:: [String] -> (Int,Int) -> [String] -> Int -> Bool
onPathChkAux3 k1 pl [] x = False
onPathChkAux3 k1 pl (b:bs) x = if plrCheck pl (getExp k1 b x) then True else onPathChkAux3 k1 pl bs x

{- | A Função __@/bomblimit/@__ cria um limite (raio 3 blocos) que impede que o bot fique preso entre duas bombas e/ou blocos.
-}
bomblimit:: [String] -> (Int,Int) -> Int -> Char
bomblimit k1 pl x = if bomblimitAux (bombGet k1) (getExp2 k1 pl x) then 'S' else 'B'

{- | A Função __@/bomblimitAux/@__ é uma função auxiliar da __@/bomblimit/@__.
-}
bomblimitAux:: [String] -> [[String]] -> Bool
bomblimitAux [] re = False
bomblimitAux (b:bs) re = if plrCheck (posGet b) re then True else bomblimitAux bs re

{- | A Função __@/getExp2/@__ é uma função modificada da __@/getExp/@__ para poder ser utilizada na função __@/bomblimit/@__.
-}
getExp2:: [String] -> (Int,Int) -> Int -> [[String]]
getExp2 k1 (c,l) x = ["_ " ++ (show c)++ " " ++ (show l)] : (pathCheckSp $ charGet k1 3 (c,l) x)

{- | A Função __@/yesSpiral/@__ é a Função Principal depois da espiral ter começado.
-}
yesSpiral:: [String] -> Int -> Char
yesSpiral k1 pln = goCenter k1 (posGet (pll k1 pln)) pln

{- | A Função __@/goCenter/@__ tenta redirecionar o bot para o centro do mapa, para este não ser eliminado pela espiral.
-}
goCenter:: [String] -> (Int,Int) -> Int -> Char
goCenter k1 (c,l) 5 = 'S'
goCenter k1 (c,l) pln | (c==(2+j-1) || c==(2+j+1)) && (l==(2+j-1) || l==(2+j+1)) && ((charChk k1 (2+j,2+j))=='?') = 'B'
                      | (c==(2+j-1) || c==(2+j+1)) && (l==(2+j-1) || l==(2+j+1)) && ((charChk k1 (2+j,2+j))=='#') = 'S'
                      | l/=(2+j) = if l<(2+j) then spotSchDw k1 (c,l) 0 else spotSchUp k1 (c,l) 0
                      | l==(2+j) && c/=(2+j) = if c<(2+j) then spotSchRt k1 (c,l) 0 else spotSchLf k1 (c,l) 0
                      | l==(2+j) && c==(2+j) = if bbDet k1 (bombGet k1) (pll k1 pln) x == 'N' then 'S' else bbDet k1 (bombGet k1) (pll k1 pln) x
 where
  x= length (head k1)
  j= posCount (length (head k1))

{- | A Função __@/posCount/@__ conta o número de vezes que é preciso subtrair 2 à dimensão do mapa, para obter o valor da coordenada central.
-}
posCount:: Int -> Int
posCount x = if x==5 then 0 else 1+ posCount (x-2)

{- | Função definida numa Tarefa Anterior.
-}
spchk:: String -> Int -> String
spchk [] n = []
spchk (x:y:z) n | x==' ' && n==0 = spaux (y:z) ++ ""
                | x==' ' = spchk (y:z) (n-1)
                | otherwise = spchk (y:z) n
  where 
         spaux [] = []
         spaux (x:y) = if x==' ' then [] else x: spaux y

{- | Função definida numa Tarefa Anterior.
-}
posGet:: String -> (Int,Int)
posGet s2 =  (read (spchk s2 0) ::Int,read (spchk s2 1)::Int)

{- | Função definida numa Tarefa Anterior.
-}
pll:: [String] -> Int -> String
pll [] x = []
pll (z:zs) x = if (head z : "") == show x then z else pll zs x

{- | Função definida numa Tarefa Anterior.
-}
charChk::[String] -> (Int,Int) -> Char
charChk k1 (x,y) = posSch (stringSch k1 y) x

{- | Função definida numa Tarefa Anterior.
-}
stringSch:: [String] -> Int -> String
stringSch l1 0 = head l1
stringSch (h:t) n = stringSch t (n-1) 

{- | Função definida numa Tarefa Anterior.
-}
posSch:: String -> Int -> Char
posSch l2 n = (!!) l2 n

{- | Função definida numa Tarefa Anterior.
-}
charGet:: [String] -> Int -> (Int,Int) -> Int -> [[String]]
charGet k1 r (c,l) x = reverse (spup k1 r (c,l) x) : reverse (spdw k1 r (c,l) x) : reverse (splf k1 r (c,l) x) : [reverse (sprt k1 r (c,l) x)]

{- | Função definida numa Tarefa Anterior.
-}
spup:: [String] -> Int -> (Int,Int) -> Int -> [String]
spup k1 0 (c,l) x = []
spup k1 r (c,l) x | l-r < 0 = spup k1 (r-1) (c,l) x
                  | otherwise = ((charChk k1 (c,l-r) : " ") ++ show c ++" "++ show (l-r)) : spup k1 (r-1) (c,l) x 

{- | Função definida numa Tarefa Anterior.
-}
spdw:: [String] -> Int -> (Int,Int) -> Int -> [String]
spdw k1 0 (c,l) x = []
spdw k1 r (c,l) x | l+r > (x-1) = spdw k1 (r-1) (c,l) x
                  | otherwise = ((charChk k1 (c,l+r) : " ") ++ show c ++" "++ show (l+r)) : spdw k1 (r-1) (c,l) x 

{- | Função definida numa Tarefa Anterior.
-}
splf:: [String] -> Int -> (Int,Int) -> Int -> [String]
splf k1 0 (c,l) x = []
splf k1 r (c,l) x | c-r < 0 = splf k1 (r-1) (c,l) x
                  | otherwise = ((charChk k1 (c-r,l) : " ") ++ show (c-r) ++" "++ show l) : splf k1 (r-1) (c,l) x 

{- | Função definida numa Tarefa Anterior.
-}
sprt:: [String] -> Int -> (Int,Int) -> Int -> [String]
sprt k1 0 (c,l) x = []
sprt k1 r (c,l) x | c+r > (x-1) = sprt k1 (r-1) (c,l) x
                  | otherwise = ((charChk k1 (c+r,l) : " ") ++ show (c+r) ++" "++ show l) : sprt k1 (r-1) (c,l) x 

{- | Função definida numa Tarefa Anterior.
-}
pathCheckSp:: [[String]] -> [[String]]
pathCheckSp [] = []
pathCheckSp (k:ks) = pathCheckSpAux k : pathCheckSp ks

{- | Função definida numa Tarefa Anterior.
-}
pathCheckSpAux:: [String] -> [String]
pathCheckSpAux [] = []
pathCheckSpAux (k:ks) | head k == '#' = []
                      | head k == '?' = []
                      | head k == ' ' = ('_':tail k) : pathCheckSpAux ks

{- | Função definida numa Tarefa Anterior.
-}
bombGet:: [String] -> [String]
bombGet [] = []
bombGet (k:ks) | head k == '*' = k: bombGet ks 
               | otherwise = bombGet ks 

{- | Função definida numa Tarefa Anterior.
-}
tkspe2:: String -> Int -> Int -> String {-tkspe2 b 0 3-}
tkspe2 [] n k = []
tkspe2 (x:y) n k | x==' ' && n== k-1 = []
                 | x==' ' = x : tkspe2 y (n+1) k
                 | otherwise = x: tkspe2 y n k