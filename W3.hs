module W3 where

import Control.Monad
import Data.List
import Data.IORef
import System.IO

-- ATTENZION! Palauta vain tiedosto joka _kääntyy_. Tämä tarkoittaa
-- sitä että komennon "runhaskell W3Test.hs" pitää toimia. Yksittäiset
-- testit siis saavat olla menemättä läpi mutta testien ajamisen tulee
-- toimia.
--
-- Ei myöskään ole suositeltavaa poistaa tässä pohjassa olevia
-- tyyppiannotaatioita. Ne kertovat mikä funktion tyypin _pitää_ olla.
-- Jos saat tyyppivirheitä, vika on toteutuksessasi, ei tehtäväpohjan
-- mukana tulevissa tyypeissä.

-- Tehtävä 1: Määrittele operaatio hei, joka tulostaa kaksi riviä,
-- joista ensimmäinen on "HEI" ja toinen on "MAAILMA".

hei :: IO ()
hei = do
  putStrLn "HEI" 
  putStrLn "MAAILMA"
-- Tehtävä 2: Määrittele operaatio tervehdi siten, että tervehdi nimi
-- tulostaa "HEI nimi"

tervehdi :: String -> IO ()
tervehdi s = putStrLn $ "HEI " ++ s

-- Tehtävä 3: Määrittele operaatio tervehdi', joka lukee nimen
-- näppäimistöltä ja sitten tervehtii kuten edellisessä tehtävässä.

tervehdi' :: IO ()
tervehdi' = do
  s <- getLine
  putStrLn $ "HEI " ++ s

-- Tehtävä 4: Määrittele operaatio lueSanat n joka lukee käyttäjältä n
-- sanaa (yksi per rivi) ja palauttaa ne aakkosjärjestyksessä

lueSanat :: Int -> IO [String]
lueSanat n = do
	s <- replicateM n getLine
	return $ sort s
	

-- Tehtävä 5: Määrittele operaatio lueKunnes f, joka lukee käyttäjältä
-- merkkijonoja ja palauttaa ne listana. Lukeminen lopetetaan kun f
-- palauttaa luetulle alkiolle True. (Sitä alkiota jolle f palauttaa
-- True ei liitetä listaan).

lueKunnes :: (String -> Bool) -> IO [String]
lueKunnes f = do
  s <- getLine
  if (f s) then return [] else do
     ss <- lueKunnes f
     return $ s:ss

-- Tehtävä 6: Määrittele operaatio printFibs n, joka tulostaa n
-- ensimmäistä fibonaccin lukua, yhden per rivi


---
---- Tulostaa väärässä järjestyksessä luvut, unohtui korjata :/
--
printFibs :: Int -> IO ()
printFibs 1 = putStrLn $ "1"
printFibs n = do
  print $ fibs 1 1 n
  printFibs (n-1)

fibs :: Int -> Int -> Int -> Int
fibs x _ 1 = x
fibs x y n = fibs y (x+y) (n-1)
-- Tehtävä 7: Määrittele operaatio isums n, joka lukee käyttäjältä n
-- lukua ja palauttaa niitten summan. Lisäksi jokaisen luvun jälkeen
-- tulostetaan siihenastinen summa luvuista.

isums :: Int -> IO Int
isums n = isums' n 0

isums' :: Int -> Int -> IO Int
isums' 0 sum = return sum
isums' n sum = do
  s <- getLine
  print $ read s+sum
  isums' (n-1) $ read s+sum
   

-- Tehtävä 8: when on hyödyllinen funktio, mutta sen ensimmäien
-- argumentti on tyyppiä Bool. Toteuta whenM joka toimii samoin mutta
-- ehto on tyyppiä IO Bool.

whenM :: IO Bool -> IO () -> IO ()
whenM cond op = do
	b <- cond
	when b op

-- Tehtävä 9: Toteuta funktio while ehto operaatio, joka suorittaa
-- operaatiota niin kauan kun ehto palauttaa True.
-- 
-- Esimerkkejä:
-- while (return False) (putStrLn "MAHDOTONTA")  -- ei tulosta mitään
-- 
-- let kysy :: IO Bool
--     kysy = do putStrLn "K/E?"
--               line <- getLine
--               return $ line == "K"
-- in while kysy (putStrLn "JEE!") 
--
-- Tämä tulostaa JEE niin kauan kuin käyttäjä vastaa K

while :: IO Bool -> IO () -> IO ()
while ehto op = do
  b <- ehto
  when b op
  when b $ while ehto op

-- Tehtävä 10: Toteuta funktio debug, joka ottaa merkkijonon s ja
-- IO-operaation op, ja palauttaa IO-operaation joka tulostaa annetun
-- s, kutsuu op, ja tulostaa jälleen s. Lopuksi operaation pitäisi
-- palauttaa op:n palautusarvo.

debug :: String -> IO a -> IO a
debug s op = do
  putStrLn s
  arvo <- op
  putStrLn s
  return arvo
-- Tehtävä 11: Toteuta itse funktio mapM_. Saat käyttää (puhtaita)
-- listafunktioita ja listojen hahmontunnistusta

mymapM_ :: (a -> IO b) -> [a] -> IO ()
mymapM_ f [] = return()
mymapM_ f (x:as) = do
  f x
  mymapM_ f as

-- Tehtävä 12: Toteuta itse funktio forM. Saat käyttää (puhtaita)
-- listafunktioita ja listojen hahmontunnistusta

myforM :: [a] -> (a -> IO b) -> IO [b]
myforM [] f = return []
myforM (x:as) f = do
  xx <- f x
  aa <- myforM as f
  return $  xx : aa

-- Tehtävä 13: Joskus törmää IO-operaatioihin jotka palauttavat
-- IO-operaatiota. Esimerkiksi IO-operaatio joka palauttaa
-- IO-operaation joka palauttaa Intin on tyypiltään IO (IO Int).
--
-- Toteuta funktio tuplaKutsu, joka ottaa IO-operaation joka palauttaa
-- IO operaation. tuplaKutsu op palauttaa IO-operaation joka
--   1. kutsuu op
--   2. kutsuu op:n palauttamaa operaatiota
--   3. palauttaa tämän palauttaman arvon
--
-- Esimerkkejä: 
--   - tuplaKutsu (return (return 3)) on sama kuin return 3
--
--   - let op :: IO (IO [String])
--         op = do l <- readLn
--                 return $ replicateM l getLine
--     in tuplaKutsu op
--
--     toimii kuten
--
--     do l <- readLn
--        replicateM l getLine
--
-- Tämä tehtävä on siitä mielenkiintoinen että jos saat sen menemään
-- tyypintarkastuksesta läpi, se on lähes välttämättä oikein.

tuplaKutsu :: IO (IO a) -> IO a
tuplaKutsu op = do
  op' <- op
  op'' <- op'
  return op''

-- Tehtävä 14: Monesti IO-operaatioita halutaan ketjuttaa. Toteuta
-- funktio yhdista joka toimii hieman kuten operaattori (.)
-- funktioille. yhdista siis ottaa operaation op1 tyyppiä
--     a -> IO b
-- ja operaation op2 tyyppiä
--     c -> IO a
-- ja arvon tyyppiä
--     c
-- ja palauttaa operaation op3 tyyppiä
--     IO b
-- op3 tekee tietenkin seuraavaa:
--   1. ottaa argumenttinsa (tyyppiä c) ja syöttää sen op2:lle
--   2. ottaa tämän lopputuloksen (tyyppiä a) ja syöttää sen op1:lle
--   3. palauttaa lopputuloksen (tyyppiä b)
--
-- Tämä tehtävä on siitä mielenkiintoinen että jos saat sen menemään
-- tyypintarkastuksesta läpi, se on lähes välttämättä oikein.

yhdista :: (a -> IO b) -> (c -> IO a) -> c -> IO b
yhdista op1 op2 c = do
  aa <- op2 c
  bb <- op1 aa
  return bb

-- Tehtävä 15: Tutustu modulin Data.IORef dokumentaatioon
-- <http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-IORef.html>
--
-- Toteuta funktio mkCounter, joka palauttaa operaatiot inc :: IO ()
-- ja get :: IO Int. Näitten operaatioitten tulee toimia yhteen seuraavasti:
--
-- 1. jos operaatiota inc ei ole ajettu kertaakaan, palauttaa get arvon 0
-- 2. operaation inc ajaminen kasvattaa seuraavien get-kutsujen palautusarvoa
--
-- Kyseessä on siis yksinkertainen tilallinen laskuri

mkCounter :: IO (IO (), IO Int)
mkCounter = do
  cnt <- newIORef 0
  let get = readIORef cnt
  let inc = modifyIORef cnt (+1)
  return (inc,get)

-- Tehtävä 16: Toteuta operaatio hFetchLines, joka hakee annetusta
-- tiedostoskahvasta rivit, joitten rivinumerot (rivinumerointi alkaa
-- 1:stä) ovat annetussa listassa. Voit olettaa että rivinumerolista
-- on nousevassa järjestyksessä.
--
-- Modulin System.IO dokumentaatio auttanee.

hFetchLines :: Handle -> [Int] -> IO [String]

hFetchLines h nums = undefined

-- Tehtävä 17: CSV on tiedostoformaatti, jossa taulukollinen arvoja on
-- tallenettu tiedostoon niin, että tiedoston yksi rivi vastaa
-- taulukon yhtä riviä, ja rivin alkiot on eroteltu ,-merkeillä.
--
-- Tee funktio readCSV joka lukee CSV-tiedoston listaksi listoja.
--
-- Huom! Funktiosi ei tarvitse osata käsitellä lainausmerkkejä,
-- kenoviivoja, eikä muitakaan erinäisten CSV-formaattien hienouksia.
-- Voit olettaa että jokainen kerkki , syötteessä on kentän raja.
--
-- Huom! Eri riveillä voi olla eri määrä kenttiä

readCSV :: FilePath -> IO [[String]]
readCSV = undefined

-- Tehtävä 18: Toteuta operaatio compareFiles, joka saa kaksi
-- tiedostonimeä, a ja b. Tiedostojen sisältöjen haluttaisiin olevan
-- samat, mutta niissä on jotakin eroja. Siispä kun tiedostojen a ja b
-- rivit nro i poikkeavat toisistaan, tulostaa ohjelma:
--
-- < tiedoston a versio rivistä
-- > tiedoston b versio rivistä 
--
-- Esimerkki:
--
-- Tiedoston a sisältö:
-- a
-- aa
-- x
-- aa
-- bb
-- cc
--
-- Tiedoston b sisältö:
-- a
-- aa
-- bb
-- aa
-- cc
-- dd
-- 
-- Tulostus:
-- < x  
-- > bb
-- < bb 
-- > cc
-- < cc
-- > dd
--
-- Huom! Voit olettaa että tiedostoissa on sama määrä rivejä.
--
-- Vihje! Eroavien rivien löytäminen on hyödyllistä erottaa omaksi
-- puhtaaksi funktiokseen (jonka tyyppi voi olla vaikkapa [String] ->
-- [String] -> [String]).

compareFiles :: FilePath -> FilePath -> IO ()
compareFiles a b = do
  acont <- readFile a
  bcont <- readFile b
  let al = lines acont
  let bl = lines bcont
  compare' al bl (length al)

compare' :: [String] -> [String] -> Int -> IO ()
compare' [] [] _ = putStr ""
compare' al bl n = do
  if head al == head bl then putStr "" else putStrLn $ "< " ++ (head al) ++ "\n> " ++ (head bl)
  compare' (tail al) (tail bl) (n-1)

-- Tehtävä 19: Tässä tehtävässä näet miten funktionaalisessa
-- ohjelmassa logiikan voi toteuttaa puhtaana funktiona, jota ympäröi
-- yksinkertainen IO-"ajuri".
--
-- Toteuta funktio interact', joka ottaa puhtaan funktion f tyyppiä
--   (String,a) -> (Bool,String,a)
-- ja alkutilan tyyppiä a ja palauttaa IO-operaation tyyppiä IO a
-- 
-- interact':n tulisi toimia niin että se lukee käyttäjältä rivin,
-- syöttää rivin ja tämänhetkisen tilan f:lle. f palauttaa booleanin,
-- tulosteen ja uuden tilan. f:n palauttama tuloste tulostetaan
-- ruudulle, ja jos palautettu boolean on True, jatketaan f:n
-- suorittamista uudella tilalla. Jos palautettu boolean on False,
-- loppuu suoritus ja operaatio palauttaa lopputilan.
--
-- Esimerkki:
--

-- let f :: (String,Integer) -> (Bool,String,Integer)
--     f ("inc",n)   = (True,"",n+1)
--     f ("print",n) = (True,show n,n)
--     f ("quit",n)  = (False,"bye bye",n)
-- in interact' f 0
--

interact' :: ((String,a) -> (Bool,String,a)) -> a -> IO a
interact' f state = do
  cmd <- getLine
  let (cond,out,newstate) = f (cmd,state)
  putStr out
  if cond then interact' f newstate else return newstate




