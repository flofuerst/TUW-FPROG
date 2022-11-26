module Angabe5 where
import Data.List

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
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

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
type Jahr          = Nat2023
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr
                        } deriving (Eq,Ord,Show)

newtype Lieferausblick = LA [(Lieferfenster,Nat0)] deriving (Eq,Ord,Show)

data Datensatz
   = DS { preis_in_euro :: Nat1,
          sofort_lieferbare_stueckzahl :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto :: Skonto
        }
     | Nicht_im_Sortiment deriving (Eq,Ord,Show)

newtype Sortiment = Sort [(Typ,Datensatz)] deriving (Eq,Ord,Show)

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 deriving (Eq,Ord,Show)

newtype Anbieter = A [(Haendler,Sortiment)] deriving (Eq,Ord,Show)

type Suchanfrage = Typ

class Wgf a where                -- Wgf fuer `wohlgeformt'
 ist_wgf    :: a->Bool         -- ist_wgf fuer `ist wohlgeformt'
 ist_nwgf   :: a->Bool         -- ist_nwgf fuer `ist nicht wohlgeformt'
 wgf_fehler :: a->a
 -- Protoimplementierungen
 ist_wgf x  = not (ist_nwgf x)
 ist_nwgf x = not (ist_wgf x)
 wgf_fehler = error "Argument fehlerhaft"


-- Aufgabe A.1

instance Wgf Lieferausblick where
 ist_wgf (LA las) = all (\la->all (\anderer->la == anderer || fst la /= fst anderer || snd la == snd anderer) las) las
 wgf_fehler = error "Ausblickfehler"

instance Wgf Sortiment where
 ist_wgf (Sort sorts) = all(\(typ, datensatz)->case datensatz of 
            Nicht_im_Sortiment->True
            DS{lieferbare_stueckzahl_im_Zeitfenster=ls}->(length.filter(\(x, _)->x==typ)) sorts == 1 && ist_wgf ls) sorts
 wgf_fehler = error "Sortimentfehler"

instance Wgf Anbieter where
 ist_wgf (A sorts) = all (\(haendler, sort)->(length.filter(\(h, _)->h==haendler)) sorts == 1 && ist_wgf sort) sorts
 wgf_fehler = error "Anbieterfehler"


-- Aufgabe A.5

type Haendlerliste = [Haendler]

sofort_lieferfaehig :: Suchanfrage->Anbieter->Haendlerliste
sofort_lieferfaehig suchanfrage (A as)
    | ist_nwgf (A as) = error "Anbieterargumentfehler"
    | otherwise = [h | h <- [H10, H9, H8, H7, H6, H5, H4, H3, H2, H1],  any (\(haendler, Sort(sortiment))->haendler == h && any (\(x, datensatz)->case datensatz of 
                 Nicht_im_Sortiment->False
                 DS{sofort_lieferbare_stueckzahl=sls}->x==suchanfrage && sls > 0) sortiment) as]

-- Aufgabe A.6

type Stueckzahl  = Nat0
type Gesamtpreis = Nat0
 
sofort_erhaeltliche_Stueckzahl :: Suchanfrage->Anbieter->(Stueckzahl,Gesamtpreis)
sofort_erhaeltliche_Stueckzahl suchanfrage (A as)
    | ist_nwgf (A as) = error "Anbieterargumentfehler"
    | otherwise = 
      foldr (\(a,b) (c,d)->(a+c, b+d)) (0,0) ((map(\(_, DS{preis_in_euro=preis, sofort_lieferbare_stueckzahl=stueckzahl})->
         (stueckzahl, preis*stueckzahl)).filter(\(x, _)->suchanfrage == x).concat.map(\(_, Sort(sort))->sort)) as)


-- Aufgabe A.7


type Preis = EUR
guenstigste_Lieferanten :: Suchanfrage->Lieferfenster->Anbieter->Maybe Haendlerliste
guenstigste_Lieferanten suchanfrage lieferfenster (A as) 
    | ist_nwgf (A as) = error "Anbieterargumentfehler"
    | length guenstigste_haendler == 0 = Nothing
    | otherwise = Just guenstigste_haendler
    where
        lieferausblick_haendler = (concat.map(\(haendler, Sort(sortiment))->
            (map( \(_, DS{preis_in_euro=preis, lieferbare_stueckzahl_im_Zeitfenster=(LA lz)})->
               (haendler, preis, find(\(lf, _)->lf==lieferfenster) lz)).filter(\(x,_)->x==suchanfrage)) sortiment)) as
        preise_von_haendler = [(haendler, preis) | (haendler, preis, Just (_, stueck)) <- lieferausblick_haendler, stueck > 0]
        guenstigster_preis = foldr (min) maxBound (map(\(_, preis)->preis) preise_von_haendler)
        guenstigste_haendler = (reverse.sort.map(\(h,_)->h).filter(\(_,preis)->preis==guenstigster_preis)) preise_von_haendler


-- Aufgabe A.8

type RabattierterPreis = EUR

guenstigste_Lieferanten_im_Lieferfenster :: Suchanfrage->Lieferfenster->Stueckzahl->Anbieter->[(Haendler,RabattierterPreis)]
guenstigste_Lieferanten_im_Lieferfenster suchanfrage lieferfenster stueckzahl (A as)
    | ist_nwgf (A as) = error "Anbieterargumentfehler"
    | otherwise = (reverse.sort.map(\(h,preis)->(h, EUR preis)).filter(\(_,preis)->preis==guenstigster_preis)) preise_von_haendler
    where
        lieferung_von_haendler = (concat.map(\(haendler, Sort(sortiment))->
            (map( \(_, datensatz)->case datensatz of 
               Nicht_im_Sortiment->(haendler, 0, Kein_Skonto, Nothing) 
               DS{preis_in_euro=preis, skonto=s, lieferbare_stueckzahl_im_Zeitfenster=(LA ls)}->(haendler, preis, s, find(\(lf, _)->lf==lieferfenster) ls)).filter(\(x,_)->x==suchanfrage)) sortiment)) as
        preise_von_haendler = [(haendler, rabattierung sk preis stueckzahl) | (haendler, preis, sk, Just (_, stueck)) <- lieferung_von_haendler, stueck >= stueckzahl]
        guenstigster_preis = foldr (min) maxBound (map(\(_, preis)->preis) preise_von_haendler)

rabattierung :: Skonto->Nat1->Stueckzahl->Nat1
rabattierung skonto preis stueck
  | Kein_Skonto <- skonto = (preis * stueck)
  | DreiProzent <- skonto =  ceiling (0.97 * 0.1 * (fromIntegral (preis * stueck))) * 10
  | FuenfProzent <- skonto = ceiling (0.95 * 0.1 * (fromIntegral (preis * stueck))) * 10
  | ZehnProzent <- skonto =  ceiling (0.90 * 0.1 * (fromIntegral (preis * stueck))) * 10