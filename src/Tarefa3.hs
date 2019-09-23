import System.Environment

encode :: [String] -> String
encode l = unlines (enc l)

enc:: [String] -> [String]
enc k1 = if length (head k1) == 5 then ["","",""] ++ deletp (grabPu (simp k1)) ++ deletp (grabBb (simp k1)) ++ deletsp3 (grabPl (simp k1)) ++ [show (length (head k1))] ++ [show (somaPb (grabPu (simp k1)))] ++ [show (somaPf (grabPu (simp k1)))] ++ [show (somaBb (grabBb (simp k1)))] else spToNb $ deleteTo $ deletEp $ deletc (simp2 k1) ++ deletp (grabPu (simp k1)) ++ deletp (grabBb (simp k1)) ++ deletsp3 (grabPl (simp k1)) ++ [show (length (head k1))] ++ [show (somaPb (grabPu (simp k1)))] ++ [show (somaPf (grabPu (simp k1)))] ++ [show (somaBb (grabBb (simp k1)))]

deletc:: [String] -> [String]
deletc [] = []
deletc (k:ks) = deletcaux k : deletc ks
deletcaux:: String -> String
deletcaux [] = []
deletcaux (k:ks) = if k=='#' then deletcaux ks else k: deletcaux ks
deletp:: [String] -> [String]
deletp [] = []
deletp (k:ks) = (drop 2 k) : deletp ks
somaPb:: [String] -> Int
somaPb [] = 0
somaPb (k:ks) = if head k == '+' then 1 + somaPb ks else somaPb ks
somaPf:: [String] -> Int
somaPf [] = 0
somaPf (k:ks) = if head k == '!' then 1 + somaPf ks else somaPf ks
somaBb:: [String] -> Int
somaBb [] = 0
somaBb (k:ks) = if head k == '*' then 1 + somaBb ks else somaBb ks
deletEp:: [String] -> [String]
deletEp [] = []
deletEp (k:ks) = if k== "" then deletEp ks else k: deletEp ks
deletsp2aux:: String -> String
deletsp2aux k1 = take ((length k1) -4) (drop 2 k1)
deletsp1aux:: String -> String
deletsp1aux k1 = take ((length k1) -2) (drop 1 k1)
deletsp2:: [String] -> [String]
deletsp2 k1 = deletsp2aux (head k1) : take ((length k1) -2) (drop 1 k1) ++ [deletsp2aux (last k1)]
deletsp1:: [String] -> [String]
deletsp1 k1 = head k1 : deletsp1aux (head (drop 1 k1)) : take ((length k1) -4) (drop 2 k1) ++ [deletsp1aux (head (drop ((length k1) -4) (drop 2 k1)))] ++ [last k1]
deleteTo:: [String] -> [String]
deleteTo k1 = deletsp1 (deletsp2 (simp2 k1)) ++ simp k1
deletsp3:: [String] -> [String]
deletsp3 [] = []
deletsp3 ((k:ks:kss):ksss) = (k:kss) : deletsp3 ksss
spToNb:: [String] -> [String]
spToNb [] = []
spToNb k1 = spToNbaux (simp2 k1) ++ simp k1
spnbgiv:: String -> String
spnbgiv [] = []
spnbgiv (k:ks) | k== ' ' = " " ++ spnbgiv ks
               | k== '?' = "?"
stspnbgiv:: String -> [String]
stspnbgiv [] = []
stspnbgiv k1 = spnbgiv k1 : stspnbgiv (drop (length (spnbgiv k1)) k1)
ttnbgiv:: [String] -> String
ttnbgiv [] = []
ttnbgiv (k:ks) | elem '?' k = if (length k) - 1 == 0 then "?" ++ ttnbgiv ks else show ((length k) - 1) ++ "?" ++ ttnbgiv ks
               | otherwise = show (length k) ++ ttnbgiv ks
spToNbaux:: [String] -> [String]
spToNbaux [] = []
spToNbaux (k:ks) = ttnbgiv (stspnbgiv k) : spToNbaux ks

{------------------------------------------------------------------------------------------------------------------------------------------------}

decode :: String -> [String]
decode l = dec (lines l)

dec:: [String] -> [String]
dec k1 = take (length (del k1) -4) (del k1)
del:: [String] -> [String]
del k1 = if x == 5 then mkmapGen 5 ++ rePw (drop (length (take (x-2) k1)) k1) (read (head (tail (drop (length (k1) -4) k1))):: Int) (read (head (tail (drop (length (k1) -3) k1))):: Int) (read (last (drop (length (k1) -4) k1)):: Int) else reString x (replace (concat (reSpc (take (x-2) k1))) (concat (mkmapGen x))) ++ rePw (drop (length (take (x-2) k1)) k1) (read (head (tail (drop (length (k1) -4) k1))):: Int) (read (head (tail (drop (length (k1) -3) k1))):: Int) (read (last (drop (length (k1) -4) k1)):: Int)
 where
    x= read (head (drop (length (k1) - 4) k1)):: Int
rePw:: [String] -> Int -> Int -> Int -> [String]
rePw k1 0 0 0 = rePl k1
rePw (k:ks) y z g | y /= 0 = ("+ " ++ k) : rePw ks (y-1) z g
                  | y == 0 && z /= 0 = ("! " ++ k) : rePw ks y (z-1) g
                  | y == 0 && z == 0 = ("* " ++ k) : rePw ks y z (g-1)
rePl:: [String] -> [String]
rePl [] = []
rePl ((k:ks):kss) = (k:' ':ks) : rePl kss    

reSpc:: [String] -> [String]
reSpc [] = []
reSpc (k:ks) = reSpcAux k : reSpc ks
reSpcAux:: String -> String
reSpcAux [] = []
reSpcAux (k:ks) | k=='?' = '?': reSpcAux ks
                | otherwise = replicate (read (k:" ") ::Int) ' ' ++ reSpcAux ks
       

{------------------------------------------------------------------------------------------------------------------------------------------------}

simp::[String]->[String]
simp [] = []
simp (h:t) | elem (head h) "+!*0123456789" = (h:t)
           | otherwise = simp t 
simp2::[String]->[String]
simp2 [] = []
simp2 (h:t) | elem (head h) "+!*0123456789" = []
            | otherwise = h: simp2 t
grabPu:: [String] -> [String]
grabPu [] = []
grabPu (i:is) = if elem (head i) "+!" then i : grabPu is else grabPu is

grabBb:: [String] -> [String]
grabBb [] = []
grabBb (i:is) = if elem (head i) "*" then i : grabBb is else grabBb is

grabPl:: [String] -> [String]
grabPl [] = []
grabPl (i:is) = if elem (head i) "0123" then i : grabPl is else grabPl is

mkmapGen :: Int -> [String]
mkmapGen x = if x == 5 then mkmap5 1 else mkmap x 1
   where 
      mkmap5 n | n==1 = replicate 5 '#' : (mkmap5 (n+1))
               | n==2 || n==4 = ('#' : [' ',' ',' '] ++ "#") : (mkmap5 (n+1))
               | n==3 = "# # #" : (mkmap5 (n+1))
               | n==5 = replicate 5 '#' : []

      mkmap x n | n==1 = replicate x '#' : (mkmap x (n+1))
                | n==2 = ('#' : [' ',' '] ++ replicate (x-6) '.' ++ "  #") : (mkmap x (n+1))
                | n==3 = ("# " ++ (concat (replicate ((div (x-6) 2) +1) "#.")) ++ "# #") : (mkmap x (n+1))
                | n==(x-2) = ("# " ++ (concat (replicate ((div (x-6) 2) +1) "#.")) ++ "# #") : (mkmap x (n+1))
                | n==(x-1) = ('#' : [' ',' '] ++ replicate (x-6) '.' ++ "  #") : (mkmap x (n+1))
                | n==x = replicate x '#' : []
                | n>3 = if even n then ( '#' : replicate (x-2) '.' ++ "#") : (mkmap x (n+1))
                                  else oddaux x 1 : (mkmap x (n+1)) where 
                                                                     oddaux x w = if w >= x then "#"
                                                                                            else "#." ++ oddaux x (w+2)
reString:: Int -> String -> [String]
reString x [] = []
reString x (n:b) = r : reString x d where
   d = drop x (n:b)
   r = take x (n:b)

replace::String -> String -> String
replace [] l2 = l2
replace l1 [] = []
replace (f:g) (h:t) = if h=='.' then f: replace g t else h: replace (f:g) t


main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"
