module Angabe7 where


{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
   6. Kopieren Sie Ihre Implementierungen von Angabe 3 bzw. 6 an den
      entsprechenden Stellen ein. Beachten Sie, dass dafür drei Umbennennungen
      erforderlich sind, um Namenskonflikte zwischen Bezeichnungen von
      Angabe 3 und 6 zu vermeiden.
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1
type Nat2023 = Int     -- Natürliche Zahlen beginnend mit 2023

newtype EUR  = EUR { euro :: Nat1 } deriving (Eq,Ord,Show)

data Skonto  = Kein_Skonto 
               | DreiProzent  
               | FuenfProzent 
               | ZehnProzent deriving (Eq,Ord,Show)

data Waschmaschine    = M1 | M2 | M3 | M4 | M5 deriving (Eq,Ord,Show)
data Waeschetrockner  = T1 | T2 | T3 | T4 deriving (Eq,Ord,Show)
data Waescheschleuder = S1 | S2 | S3 deriving (Eq,Ord,Show)

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder deriving (Eq,Ord,Show)

data Quartal       = Q4 | Q3 | Q2 | Q1 deriving (Eq,Ord,Show)
type Jahr          = Nat2023
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr 
                        } deriving (Eq,Show)

instance Ord Lieferfenster where
    compare lfe1 lfe2 = compLfe lfe1 lfe2

compLfe :: Lieferfenster -> Lieferfenster -> Ordering
compLfe lfe1@(LF q1 j1) lfe2@(LF q2 j2)
    | lfe1 == lfe2 = EQ
    | j1 < j2 = LT 
    | j1 > j2 = GT 
    | otherwise = case (q1 > q2) of {True -> LT; False -> GT}

newtype Lieferausblick  = LA (Lieferfenster -> Nat0)
newtype Lieferausblick' = LA' [(Lieferfenster,Nat0)] deriving (Eq,Show)

data Datensatz 
   = DS { preis_in_euro :: Nat1,
          sofort_lieferbare_stueckzahl :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto :: Skonto
        }
     | Nicht_im_Sortiment

data Datensatz' 
   = DS' { preis_in_euro' :: Nat1,
           sofort_lieferbare_stueckzahl' :: Nat0,
           lieferbare_stueckzahl_im_Zeitfenster' :: Lieferausblick',
           skonto' :: Skonto
        }
     | Nicht_im_Sortiment' deriving (Eq,Show)

newtype Sortiment  = Sort (Typ -> Datensatz)
newtype Sortiment' = Sort' [(Typ,Datensatz')] deriving (Eq,Show)

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 deriving (Eq,Ord,Show)

newtype Markt = Mt (Haendler -> Sortiment)
newtype Markt' = Mt' [(Haendler,Sortiment')] deriving (Eq,Show)

data Betroffen = Betroffen | NichtBetroffen deriving (Eq,Show)

newtype Betroffene_Haendler = BH (Haendler -> Betroffen)

type AbLieferfenster = Lieferfenster


-- Aufgabe A.1

lst2fkt_la :: [(Lieferfenster,Nat0)] -> (Lieferfenster -> Nat0)
lst2fkt_la liste = lst2fkt_la_hilfe liste

lst2fkt_la_hilfe :: [(Lieferfenster,Nat0)] -> Lieferfenster -> Nat0
lst2fkt_la_hilfe [] _ = error "undefiniert"
lst2fkt_la_hilfe (x:xs) lieferfenster
   | fst x == lieferfenster = snd x
   | otherwise = lst2fkt_la_hilfe xs lieferfenster
      

lst2fkt_so :: [(Typ,Datensatz')] -> (Typ -> Datensatz)
lst2fkt_so liste = lst2fkt_so_hilfe liste

lst2fkt_so_hilfe :: [(Typ,Datensatz')] -> Typ -> Datensatz
lst2fkt_so_hilfe [] _ = error "undefiniert"
lst2fkt_so_hilfe (x:xs) typ
   | fst x == typ = case snd x of 
      DS'{preis_in_euro' = preis, 
            sofort_lieferbare_stueckzahl' = sofort_verfuegbar, 
            lieferbare_stueckzahl_im_Zeitfenster' = (LA' verfuegbar_in_zeitslot), 
            skonto' = skont} -> DS{preis_in_euro = preis, 
                                    sofort_lieferbare_stueckzahl = sofort_verfuegbar, 
                                    lieferbare_stueckzahl_im_Zeitfenster = LA (lst2fkt_la verfuegbar_in_zeitslot),
                                    skonto = skont}
      Nicht_im_Sortiment' -> Nicht_im_Sortiment
   | otherwise = lst2fkt_so_hilfe xs typ


lst2fkt_ab :: [(Haendler,Sortiment')] -> (Haendler -> Sortiment)
lst2fkt_ab liste = lst2fkt_ab_hilfe liste

lst2fkt_ab_hilfe [] _ = error "undefiniert"
lst2fkt_ab_hilfe (x:xs) haendler
   |  fst x == haendler = case snd x of Sort' sortiment -> Sort (lst2fkt_so sortiment)
   |  otherwise = lst2fkt_ab_hilfe xs haendler



-- Aufgabe A.2

lst2fkt_la' :: Lieferausblick' -> Lieferausblick
lst2fkt_la' (LA' lieferausblick) = LA (lst2fkt_la lieferausblick)

lst2fkt_so' :: Sortiment' -> Sortiment
lst2fkt_so' (Sort' sortiment) = Sort (lst2fkt_so sortiment)

lst2fkt_ab' :: Markt' -> Markt
lst2fkt_ab' (Mt' markt) = Mt (lst2fkt_ab markt)


-- Aufgabe A.4

preisanpassung :: Markt -> Markt
preisanpassung markt_schlecht = Mt (markt_verbesserung markt_schlecht)

markt_verbesserung :: Markt -> Haendler -> Sortiment
markt_verbesserung (Mt markt_schlecht) haendler = Sort (sortiment_verbesserung 
   (Mt markt_schlecht) (markt_schlecht haendler))

sortiment_verbesserung :: Markt -> Sortiment -> Typ -> Datensatz
sortiment_verbesserung markt_schlecht (Sort sortiment_schlecht) typ = 
   datensatz_verbesserung (besten_preis_finden markt_schlecht typ)(sortiment_schlecht typ)

datensatz_verbesserung :: Nat0 -> Datensatz -> Datensatz
datensatz_verbesserung preis_verbessert DS{sofort_lieferbare_stueckzahl = sofort_verfuegbar, 
                        lieferbare_stueckzahl_im_Zeitfenster = verfuegbar_in_zeitslot,
                        skonto = skont} = DS {preis_in_euro = preis_verbessert, 
                              sofort_lieferbare_stueckzahl = sofort_verfuegbar, 
                              lieferbare_stueckzahl_im_Zeitfenster = verfuegbar_in_zeitslot,
                              skonto = skont}
datensatz_verbesserung _ Nicht_im_Sortiment = Nicht_im_Sortiment

preis_finden :: Sortiment -> Typ -> Maybe Nat0
preis_finden (Sort sortiment) typ = case sortiment typ of 
   DS{preis_in_euro = preis} -> Just preis
   Nicht_im_Sortiment -> Nothing

besten_preis_finden :: Markt -> Typ -> Nat0
besten_preis_finden (Mt markt) typ = 
   minimum (map (\ (Just a) -> a) (filter (\ b -> b /= Nothing) [preis_finden (markt h) typ | 
      h <- [H1, H2, H3, H4, H5, H6, H7, H8, H9, H10]]))


-- Aufgabe A.5

berichtige :: Markt -> Betroffene_Haendler -> AbLieferfenster -> Markt
berichtige markt betroffene zeitpunkt_lieferfenster = Mt (markt_zusammenbruch markt betroffene zeitpunkt_lieferfenster)

markt_zusammenbruch :: Markt -> Betroffene_Haendler -> AbLieferfenster -> Haendler -> Sortiment
markt_zusammenbruch (Mt markt) (BH betroffene) zeitpunkt_lieferfenster haendler = 
   Sort (sortiment_zusammenbruch (betroffene haendler) zeitpunkt_lieferfenster (markt haendler))

sortiment_zusammenbruch :: Betroffen -> AbLieferfenster -> Sortiment -> Typ -> Datensatz
sortiment_zusammenbruch betroffen zeitpunkt_lieferfenster (Sort sortiment) typ 
   = datensatz_zusammenbruch betroffen zeitpunkt_lieferfenster (sortiment typ)

datensatz_zusammenbruch :: Betroffen -> AbLieferfenster -> Datensatz -> Datensatz
datensatz_zusammenbruch NichtBetroffen _ datensatz = datensatz
datensatz_zusammenbruch Betroffen zeitpunkt_lieferfenster DS{preis_in_euro = preis, 
                        sofort_lieferbare_stueckzahl = sofort_verfuegbar, 
                        lieferbare_stueckzahl_im_Zeitfenster = verfuegbar_in_zeitslot,
                        skonto = skont} = DS {preis_in_euro = preis, 
                        sofort_lieferbare_stueckzahl = sofort_verfuegbar, 
                        lieferbare_stueckzahl_im_Zeitfenster = LA (lieferausblick_zusammenbruch zeitpunkt_lieferfenster verfuegbar_in_zeitslot),
                        skonto = skont}
datensatz_zusammenbruch Betroffen _ Nicht_im_Sortiment = Nicht_im_Sortiment

lieferausblick_zusammenbruch :: AbLieferfenster -> Lieferausblick -> Lieferfenster -> Nat0
lieferausblick_zusammenbruch zeitpunkt_lieferfenster (LA lieferausblick) lieferfenster
   | lieferfenster >= zeitpunkt_lieferfenster  = 0
   | otherwise = lieferausblick lieferfenster
