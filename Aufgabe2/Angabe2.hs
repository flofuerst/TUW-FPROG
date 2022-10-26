module Angabe2 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
-}

-- Fuer A.1 bis A.4

data Lotterielos = Treffer 
                   | Niete 
                   | Freilos deriving (Eq,Show)
 
data Liste = Schluss Lotterielos
             | Kopf Lotterielos Liste deriving (Eq,Show)

data Baum = Blatt Lotterielos
            | Gabel Baum Lotterielos Baum deriving (Eq,Show)

data Liste' = Schluss' Baum
              | Kopf' Baum Liste' deriving (Eq,Show)

data Baum' = Blatt' Liste
             | Gabel' Baum' Liste Baum' deriving (Eq,Show)

type Loswert    = Lotterielos
type Auswertung = Ordering


-- Aufgabe A.1

analysiere :: Liste -> Loswert -> Loswert -> Auswertung
-- beide Typen durch durchzählen und dann einfach vergleichen
analysiere list lw lw' 
   | count list lw > count list lw' = GT
   | count list lw == count list lw' = EQ
   | otherwise = LT

count :: Liste -> Loswert -> Int
count (Schluss current_element) lw 
   | lw == current_element = 1
   | otherwise = 0
count (Kopf current_element list) lw 
   | lw == current_element = 1 + count list lw
   | otherwise = count list lw

{- Zählt die Anzahl Vorkommnisse der beiden Loswerte in der Liste mit der Hilfsfunktion count und 
   vergleicht anschließend die Summe der Vorkommnisse
-}



-- Aufgabe A.2

analysiere' :: Baum -> Loswert -> Loswert -> Auswertung
analysiere' tree lw lw'
   | count' tree lw > count' tree lw' = GT
   | count' tree lw == count' tree lw' = EQ
   | otherwise = LT

count' :: Baum -> Loswert -> Int
count' (Blatt current_element) lw
   | lw == current_element = 1
   | otherwise = 0
count' (Gabel tree_l current_element tree_r) lw
   | lw == current_element = 1 + count' tree_l lw + count' tree_r lw
   | otherwise = count' tree_l lw + count' tree_r lw

{- Zählt die Anzahl Vorkommnisse der beiden Loswerte im Baum mit der Hilfsfunktion count' und 
   vergleicht anschließend die Summe der Vorkommnisse
-}



-- Aufgabe A.3

analysiere'' :: Liste' -> Loswert -> Loswert -> Auswertung
analysiere'' list' lw lw'
   | count'' list' lw > count'' list' lw' = GT
   | count'' list' lw == count'' list' lw' = EQ
   | otherwise = LT

count'' :: Liste' -> Loswert -> Int
count'' (Schluss' tree) lw = count' tree lw
count'' (Kopf' tree list') lw = count' tree lw + count'' list' lw

{- Zählt die Anzahl Vorkommnisse der beiden Loswerte in der Liste' mit der Hilfsfunktion count'' und 
   vergleicht anschließend die Summe der Vorkommnisse. Hierbei wird für den Baum die bereits vorhandene Hilfsfunktion
   count' aufgerufen und für die Liste' die Hilfsfunktion count''
-}


-- Aufgabe A.4

analysiere''' :: Baum' -> Loswert -> Loswert -> Auswertung
analysiere''' tree' lw lw'
   | count''' tree' lw > count''' tree' lw' = GT
   | count''' tree' lw == count''' tree' lw' = EQ
   | otherwise = LT

count''' :: Baum' -> Loswert -> Int
count''' (Blatt' list) lw = count list lw 
count''' (Gabel' tree_l' list tree_r') lw = count''' tree_l' lw + count list lw + count''' tree_r' lw
{- Zählt die Anzahl Vorkommnisse der beiden Loswerte in Baum' mit der Hilfsfunktion count''' und 
   vergleicht anschließend die Summe der Vorkommnisse. Hierbei wird für die Liste die bereits vorhandene Hilfsfunktion
   count aufgerufen und für den Baum' die Hilfsfunktion count'''
-}
