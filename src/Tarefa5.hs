module Main where

import Tarefa1s (mapa)
import Tarefa2s (move,spchk)
import Tarefa4s (avanca,fSpiral)
import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game
import System.Environment
import Text.Read
import Data.Maybe
import System.Random
import Data.Char (isDigit)
import Data.List

{- | A Função __@/retira/@__ separa a __String__ com o Estado de Jogo, numa __String__ apenas com o Mapa.
-}
retira::String -> String
retira [] = []
retira (w:ws) = if elem w "+!*0123" then [] 
                                    else w : (retira ws)

{- | A Função __@/retira2/@__ separa a __[String]__ com o Estado de Jogo, numa __[String]__ apenas com as __Coordenadas dos Jogadores, Bombas e Power-Ups__.
-}
retira2::[String] -> [String]
retira2 [] = []
retira2 (w:ws) = if elem (head w) "*0123" then w : (retira2 ws)
                                          else retira2 ws  

{- | A Função __@/posGet2/@__ dá-nos a __Coordenada dos YY__ de um dado __Jogador, Power-Up ou Bomba__.
-}
posGet2:: String -> Float
posGet2 s2 = read (spchk s2 0) ::Float

{- | A Função __@/posGet2/@__ dá-nos a __Coordenada dos XX__ de um dado __Jogador, Power-Up ou Bomba__.
-}
posGet3::String -> Float
posGet3 s2 = read (spchk s2 1)::Float

{- | A Função __@/jogadorVivo/@__ verifica se ainda existem __Jogadores__ vivos.
-}
jogadorVivo::[String] -> Bool
jogadorVivo [] = False
jogadorVivo (h:t) = if elem (head h) "0123" then True
                                            else jogadorVivo t
----------------------------------------------------------------------------------------------------------------------------------------
-- | Uma representação do estado do jogo.
type Estado = ((Float,Float),Float,[String],[Picture],Float)

{- | O estado inicial do jogo.
-}
estadoInicial :: IO Estado
estadoInicial = do putStrLn "Dimensão? (Inteiro/Ímpar/Superior a 5)"
                   d <- getLine
                   let x = read d :: Float
                   let z = read d :: Int
                   --if (odd x) && (x>=5) then x else putStrLn "Dimensão? (Inteiro/Ímpar/Superior a 5)"
                   putStrLn "Semente? (Inteiro/Superior a 0)"
                   s <- getLine
                   let y = read s :: Float
                   let w = read s :: Int
                   --if isDigit y && (y>=0) then y else putStrLn "Semente? (Inteiro/Superior a 0)" 
                   batman <- loadBMP "batman.bmp"
                   spider <- loadBMP "spider-man.bmp"
                   pedra <- loadBMP "pedra.bmp"
                   tijolo <- loadBMP "tijolo.bmp"
                   bomba <- loadBMP "bomba.bmp"
                   fundo <- loadBMP "fundo.bmp"
                   return ((x*30,x*30),x,((mapa z w) ++ ["0 1 1"]),[batman,spider,pedra,tijolo,bomba,fundo],(22*x))

{- | Função que desenha o jogo.
-}
desenhaEstado :: Estado -> Picture
desenhaEstado ((dimX,dimY),dim,mP,[batman,spider,pedra,tijolo,bomba,fundo],tempo) | (tempo > 0) && (jogadorVivo mP) = Pictures ((desenha (retira (unlines $ mP)) [batman,spider,pedra,tijolo,bomba,fundo] dimX 210 dim 0 0) ++ (desenha2 (retira2 (mP)) [batman,spider,pedra,tijolo,bomba,fundo] dimX dimY) ++ [Translate (100) (45) $ Scale 0.2 0.2 $ Text $ show $ tempo])
                                                                                  | (tempo <= ((dim-2)^2)) && (jogadorVivo mP) = Pictures ((desenha (retira (unlines $ mP)) [batman,spider,pedra,tijolo,bomba,fundo] dimX 210 dim 0 0) ++ (desenha2 (retira2 (mP)) [batman,spider,pedra,tijolo,bomba,fundo] dimX dimY) ++ [Color red $ Translate (100) (45) $ Scale 0.2 0.2 $ Text $ show $ tempo])
                                                                                  | (tempo == 0) && (jogadorVivo mP) = Pictures [Scale 0.2 0.2 $ Text $ show $ "FIM"]
                                                                                  | (jogadorVivo mP == False) = Pictures [Scale 0.2 0.2 $ Text $ show $ "FIM"]
                                                                                  | otherwise = Pictures [Scale 0.2 0.2 $ Text $ show $ "FIM"]

{- | A Função __@/desenha/@__ desenha o Mapa de Jogo, através da __String__ fornecida pela __@/retira/@__.
-}
desenha::String -> [Picture] -> Float -> Float -> Float -> Float -> Float -> [Picture]
desenha [] _ _ _ _ _ _ = []
desenha (p:ps) [batman,spider,pedra,tijolo,bomba,fundo] r z t m k | (m>=0) && (m<=(t-1)) = Translate ((-(r/2))+30*m) (z/2) (coiso p [batman,spider,pedra,tijolo,bomba,fundo]) : desenha ps [batman,spider,pedra,tijolo,bomba,fundo] r z t (m+1) k 
                                                                  | (m==t) = desenha ps [batman,spider,pedra,tijolo,bomba,fundo] r ((210)-(60*(k+1))) t 0 (k+1)
                                                                  | otherwise = []
    where
      coiso x [batman,spider,pedra,tijolo,bomba,fundo] | (x=='#') = pedra--Color red $ rectangleSolid 30 30  
                                                       | (x==' ') = fundo--Color blue $ rectangleSolid 30 30 
                                                       | (x=='?') = tijolo--Color orange $ rectangleSolid 30 30 
                                                       | otherwise = Color (greyN 0.5) $ rectangleSolid 30 30

{- | A Função __@/desenha2/@__ desenha os Jogadores e as Bombas, através da __[String]__ fornecida pela __@/retira2/@__.
-}
desenha2::[String] -> [Picture] -> Float -> Float -> [Picture]--Alterar imagem dos players
desenha2 [] _ _ _ = []
desenha2 (p:ps) [batman,spider,pedra,tijolo,bomba,fundo] z w = (coiso2 p [batman,spider,pedra,tijolo,bomba,fundo] z w) : (desenha2 ps [batman,spider,pedra,tijolo,bomba,fundo] z w)
    where
      coiso2 x [batman,spider,pedra,tijolo,bomba,fundo] c v | ((head x)=='*') = Translate (-(c/2)+30*(posGet2 x)) ((105)-30*(posGet3 x)) bomba
                                                            | ((head x)=='0') = Translate (-(c/2)+30*(posGet2 x)) ((105)-30*(posGet3 x)) batman
                                                            | otherwise = Translate c v (Color (greyN 0.5) $ rectangleSolid 30 30)
 
{- | Função que altera o estado do jogo quando acontece um evento.
-}
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (a,b,c,d,e) = (a,b,(move c 0 'U'),d,e)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (a,b,c,d,e) = (a,b,(move c 0 'D'),d,e)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (a,b,c,d,e) = (a,b,(move c 0 'L'),d,e)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (a,b,c,d,e) = (a,b,(move c 0 'R'),d,e)
reageEvento (EventKey (SpecialKey KeySpace) Down _ _) (a,b,c,d,e) = (a,b,(move c 0 'B'),d,e)                             
reageEvento _ mapa = mapa 

{- | Função que altera o estado do jogo quando o tempo avança @n@ segundos.
-}
reageTempo :: Float -> Estado -> Estado--Fazer função para a espiral
reageTempo f (a,b,c,d,e) | (e>=((b-2)^2)) = (a,b,(avanca c (ceiling e)),d,(e-f))
                         | (e<=((b-2)^2)) = (a,b,(fSpiral (avanca c (ceiling e)) (ceiling e) (ceiling b)),d,(e-f))
                         | otherwise = (a,b,c,d,(e-f))
{- | Frame rate.
-}
fr :: Int
fr = 50

{- | Display mode.
-}
dm :: Display
dm = InWindow "Novo Jogo" (800, 600) (0, 0)
    
{- | Função principal que invoca o jogo.
-}
joga :: Estado -> IO ()
joga inicio = play
    (InWindow "Novo Jogo" (800, 800) (0, 0))  -- tamanho da janela do jogo
    (greyN 0.5)                               -- côr do fundo da janela
    fr                                        -- refresh rate
    inicio                                    -- estado inicial
    desenhaEstado                             -- função que desenha o estado do jogo
    reageEvento                               -- reage a um evento
    reageTempo                                -- reage ao passar do tempo

{-| Função principal que invoca o jogo.
-}
main :: IO ()
main = do
    inicio <- estadoInicial 
    joga inicio