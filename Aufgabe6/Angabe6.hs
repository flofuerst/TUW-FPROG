module Angabe6 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1

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

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
data Jahr          = J2023 | J2024 | J2025 deriving (Eq,Ord,Show)
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr 
                        } deriving (Eq,Ord,Show)

newtype Lieferausblick = LA (Lieferfenster -> Nat0)

data Datensatz 
   = DS { preis_in_euro :: Nat1,
          sofort_lieferbare_stueckzahl :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto :: Skonto
        } 
     | Nicht_im_Sortiment

newtype Sortiment = Sort (Typ -> Datensatz)

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 deriving (Eq,Ord,Show)

newtype Anbieter = A (Haendler -> Sortiment)

type Suchanfrage = Typ  

class Wgf a where                -- Wgf fuer `wohlgeformt'
 ist_wgf    :: a -> Bool         -- ist_wgf fuer `ist wohlgeformt'
 ist_nwgf   :: a -> Bool         -- ist_nwgf fuer `ist nicht wohlgeformt'
 wgf_fehler :: a -> a
 -- Protoimplementierungen
 ist_wgf x  = not (ist_nwgf x)
 ist_nwgf x = not (ist_wgf x)
 wgf_fehler = \x -> error "Argument fehlerhaft"


-- Aufgabe A.1

wg_la :: Lieferausblick -> [(Lieferfenster,Nat0)]
wg_la (LA la) = [(LF {quartal = q, jahr = j}, la (LF {quartal = q, jahr = j})) | q <- quartal, j <- jahre]
   where
      jahre = [J2023, J2024, J2025]
      quartal = [Q1, Q2, Q3, Q4]

wg_so :: Sortiment -> [(Typ,Datensatz)]
wg_so (Sort sortiment) = [((M m), sortiment (M m)) | m <- waschm] ++ [((T t) ,sortiment (T t)) | t <- trockner] ++ [((S s),sortiment (S s)) | s <- schleuder]
   where
      waschm = [M1, M2, M3, M4, M5]
      trockner = [T1, T2, T3, T4]
      schleuder = [S1, S2, S3]


wg_ab :: Anbieter ->  [(Haendler,Sortiment)]
wg_ab (A anbieter) = [(h, anbieter h) | h <- hs]
   where
      hs = [H1, H2, H3, H4, H5, H6, H7, H8, H9, H10]


-- Aufgabe A.2

instance Wgf Lieferausblick where
 ist_wgf las = all (\la->all (\anderer->la == anderer || fst la /= fst anderer || snd la == snd anderer) (wg_la las)) (wg_la las)
 wgf_fehler = error "Ausblickfehler"

instance Wgf Sortiment where
 ist_wgf sorts = all(\(typ, datensatz)->case datensatz of 
            Nicht_im_Sortiment->True
            DS{lieferbare_stueckzahl_im_Zeitfenster=ls}->(length.filter(\(x, _)->x==typ)) (wg_so sorts) == 1 && ist_wgf ls) (wg_so sorts)
 wgf_fehler = error "Sortimentfehler"

instance Wgf Anbieter where
 ist_wgf sorts = all (\(haendler, sort)->(length.filter(\(h, _)->h==haendler)) (wg_ab sorts) == 1 && ist_wgf sort) (wg_ab sorts)
 wgf_fehler = error "Anbieterfehler"


-- Aufgabe A.5

type Haendlerliste = [Haendler]

sofort_lieferfaehig :: Suchanfrage -> Anbieter -> Haendlerliste
sofort_lieferfaehig suchanfrage (A anbieter)
   | ist_nwgf (A anbieter) = error "Anbieterargumentfehler"
   | otherwise = [h | h <- [H10, H9, H8, H7, H6, H5, H4, H3, H2, H1], 
   fst (sofort_lieferfaehig_aus_sortiment (anbieter h) suchanfrage) > 0]

sofort_lieferfaehig_aus_sortiment :: Sortiment -> Suchanfrage -> (Stueckzahl,Gesamtpreis)
sofort_lieferfaehig_aus_sortiment (Sort sortiment) suchanfrage = case sortiment suchanfrage of 
   Nicht_im_Sortiment -> (0, 0)
   DS{preis_in_euro=preis, sofort_lieferbare_stueckzahl=stueckzahl} -> (stueckzahl, stueckzahl * preis)


-- Aufgabe A.6

type Stueckzahl  = Nat0
type Gesamtpreis = Nat0
 
sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Anbieter -> (Stueckzahl,Gesamtpreis)
sofort_erhaeltliche_Stueckzahl suchanfrage (A anbieter)
   | ist_nwgf (A anbieter) = error "Anbieterargumentfehler"
   | otherwise = foldr (\ a b -> (fst a + fst b, snd a + snd b)) (0, 0) 
      [(sofort_lieferfaehig_aus_sortiment (anbieter h) suchanfrage) | h <- [H10, H9, H8, H7, H6, H5, H4, H3, H2, H1]]

-- Aufgabe A.7

type Preis = EUR
guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Anbieter -> Maybe Haendlerliste
guenstigste_Lieferanten suchanfrage lieferfenster (A anbieter) 
   | ist_nwgf (A anbieter) = error "Anbieterargumentfehler"
   | null ergebnis = Nothing
   | otherwise = Just ergebnis
   where 
     preise = [preis_herausfinden suchanfrage lieferfenster (A anbieter) 1 h (\a b -> a)| h <- alle_haendler]
     niedrigster_preis = foldr (min) maxBound (filter (\ p -> p /= 0) preise)
     ergebnis = filter (\ h -> (preis_herausfinden suchanfrage lieferfenster (A anbieter) 1 h (\ a _ -> a)) == 
      niedrigster_preis && (preis_herausfinden suchanfrage lieferfenster (A anbieter) 1 h (\ a _ -> a)) /= 0) alle_haendler
     alle_haendler = [H10, H9, H8, H7, H6, H5, H4, H3, H2, H1]

preis_herausfinden :: Suchanfrage -> Lieferfenster -> Anbieter -> Stueckzahl -> Haendler -> (Nat1 -> Skonto -> Nat1) -> Nat0
preis_herausfinden suchanfrage lieferfenster (A anbieter) stueckzahl haendler s = case sortiment suchanfrage of
   Nicht_im_Sortiment -> 0
   DS{preis_in_euro=preis, lieferbare_stueckzahl_im_Zeitfenster=la, skonto=skont} -> 
      berechne_preis preis (case la of LA l -> l lieferfenster) stueckzahl s skont
  where
   Sort sortiment = anbieter haendler

berechne_preis :: Nat0 -> Stueckzahl -> Stueckzahl -> (Nat1 -> Skonto -> Nat1) -> Skonto -> Nat0
berechne_preis preis stueckzahl_erhaeltlich stueckzahl_benoetigt s skont
  | stueckzahl_erhaeltlich < stueckzahl_benoetigt = 0
  | otherwise = s (preis * stueckzahl_benoetigt) skont


-- Aufgabe A.8

type RabattierterPreis = EUR

guenstigste_Lieferanten_im_Lieferfenster :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> [(Haendler,RabattierterPreis)]
guenstigste_Lieferanten_im_Lieferfenster anfrage fenster stueckzahl (A as) 
  = [(h, EUR (preis_herausfinden anfrage fenster (A as) stueckzahl h rabattierung)) | h <- haendlers, 
        (preis_herausfinden anfrage fenster (A as) stueckzahl h rabattierung) /= 0 && (preis_herausfinden anfrage fenster (A as) stueckzahl h rabattierung) == lowest ]
   where 
     haendlers = [H10, H9, H8, H7, H6, H5, H4, H3, H2, H1]
     prices = [preis_herausfinden anfrage fenster (A as) stueckzahl h rabattierung | h <- haendlers]
     lowest = foldr (min) maxBound (filter (\ p -> p /= 0) prices)

rabattierung :: Nat1 -> Skonto -> Nat1
rabattierung preis Kein_Skonto = preis
rabattierung preis DreiProzent =  ceiling (0.97 * 0.1 * (fromIntegral preis)) * 10
rabattierung preis FuenfProzent = ceiling (0.95 * 0.1 * (fromIntegral preis)) * 10
rabattierung preis ZehnProzent =  ceiling (0.90 * 0.1 * (fromIntegral preis)) * 10

