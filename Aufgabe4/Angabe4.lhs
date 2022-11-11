
> module Angabe4 where

1. Vervollst�ndigen Sie gemaess Angabentext!
2. Vervollst�ndigen Sie auch die vorgegebenen Kommentaranf�nge!
3. L�schen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisung!
4. Achten Sie darauf, dass `Gruppe' Leserechte f�r Ihre Abgabedatei hat!
5. Hinweis: Kommentar- und Programmzeilen m�ssen durch mindestens eine Leerzeile getrennt sein!


> type Nat0    = Int     -- Nat�rliche Zahlen beginnend mit 0
> type Nat1    = Int     -- Nat�rliche Zahlen beginnend mit 1
> type Nat2023 = Int     -- Nat�rliche Zahlen beginnend mit 2023

> newtype EUR  = EUR { euro :: Nat1 } deriving(Eq,Ord,Show)

> data Skonto  = Kein_Skonto 
>                | DreiProzent  
>                | FuenfProzent 
>                | ZehnProzent

> data Waschmaschinentyp   = WM_Typ1 | WM_Typ2 | WM_Typ3 | WM_Typ4 | WM_Typ5
> data Waeschetrocknertyp  = WT_Typ1 | WT_Typ2 | WT_Typ3 | WT_Typ4
> data Waescheschleudertyp = WS_Typ1 | WS_Typ2 | WS_Typ3

> data Typ = WM Waschmaschinentyp
>            | WT Waeschetrocknertyp
>            | WS Waescheschleudertyp

> data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
> type Jahr          = Nat2023
> data Lieferfenster = LF { quartal :: Quartal,
>                           jahr    :: Jahr 
>                         }

> data Datensatz
>   = DS { preis_in_euro :: Nat1,
>          sofort_lieferbare_stueckzahl :: Nat0,
>          lieferbare_stueckzahl_im_Zeitfenster :: Lieferfenster -> Nat0,
>          skonto :: Skonto
>        }
>     | Nicht_im_Sortiment

> data Sortiment 
>   = WMS {wm   :: Waschmaschinentyp   -> Datensatz}
>     | WTS {wt :: Waeschetrocknertyp  -> Datensatz}
>     | WSS {ws :: Waescheschleudertyp -> Datensatz}

> data Lieferantenname = L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8 | L9 | L10 deriving(Eq,Ord,Show)

> type Lieferanten = Lieferantenname -> Sortiment

> type Suchanfrage = Typ  

Aufgabe A.1

> type Lieferantenliste = [Lieferantenname]

> sofort_erhaeltlich_bei :: Suchanfrage -> Lieferanten  -> Lieferantenliste

> sofort_erhaeltlich_bei anfrage lieferanten = 
>  [l |
>  l <- [L1, L2, L3, L4, L5, L6, L7, L8, L9, L10], 
>  fst (gegenstaende_sofort_erhaeltlich (datensatz_aus_sortiment (lieferanten l) anfrage)) > 0]

> datensatz_aus_sortiment :: Sortiment -> Typ -> Datensatz
> datensatz_aus_sortiment (WMS waschmaschiene) (WM typ) = waschmaschiene typ
> datensatz_aus_sortiment (WTS waeschetrockner) (WT typ) = waeschetrockner typ
> datensatz_aus_sortiment (WSS waescheschleuder) (WS typ) = waescheschleuder typ
> datensatz_aus_sortiment _ _ = Nicht_im_Sortiment

> gegenstaende_sofort_erhaeltlich :: Datensatz -> (Stueckzahl, Gesamtpreis)
> gegenstaende_sofort_erhaeltlich Nicht_im_Sortiment = (0, 0)
> gegenstaende_sofort_erhaeltlich DS {sofort_lieferbare_stueckzahl = stueckzahl, preis_in_euro = preis} = 
>  (stueckzahl, stueckzahl*preis)


Aufgabe A.2

> type Stueckzahl  = Nat0
> type Gesamtpreis = Nat0

> sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Lieferanten -> (Stueckzahl,Gesamtpreis)
> sofort_erhaeltliche_Stueckzahl anfrage lieferanten = 
>  (sum (map (\(x,_) -> x) liste), sum (map (\(_,x) -> x) liste))
>  where
>   liste = [gegenstaende_sofort_erhaeltlich (datensatz_aus_sortiment (lieferanten l) anfrage) | 
>    l <- [L1, L2, L3, L4, L5, L6, L7, L8, L9, L10]]

aufgabe haette auch mit foldr realisiert werden koennen


Aufgabe A.3

> type Preis = EUR

> guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Lieferanten -> Maybe Lieferantenliste
> guenstigste_Lieferanten anfrage lieferfenster lieferanten 
>  | length finde_guenstigsten_preise > 0 = 
>   Just [l | (l,_) <- finde_guenstigsten_lieferanten preise_lieferanten]
>  | otherwise = Nothing
>  where
>    finde_guenstigsten_preise = [l | (l,_) <- finde_guenstigsten_lieferanten preise_lieferanten]
>    preise_lieferanten = 
>     [(l, finde_preise_in_lieferfenster (datensatz_aus_sortiment (lieferanten l) anfrage) lieferfenster)
>     | l <- [L1, L2, L3, L4, L5, L6, L7, L8, L9, L10]]

> finde_preise_in_lieferfenster :: Datensatz -> Lieferfenster -> Maybe Nat1
> finde_preise_in_lieferfenster Nicht_im_Sortiment _ = Nothing -- 'Nothing' wenn nicht im Sortiment
> finde_preise_in_lieferfenster DS{lieferbare_stueckzahl_im_Zeitfenster=stueckzahl_zeitraum, preis_in_euro=preis} lieferfenster = 
>  if ((stueckzahl_zeitraum lieferfenster) > 0) then Just preis else Nothing 

> finde_guenstigsten_lieferanten :: [(Lieferantenname, Maybe Nat1)] -> [(Lieferantenname, EUR)]
> finde_guenstigsten_lieferanten [(name_lieferant, Just preis_lieferant)] = [(name_lieferant, EUR preis_lieferant)]
> finde_guenstigsten_lieferanten ((name_lieferant, Just preis_lieferant):lieferanten) 
>  | length andere_lieferanten <= 0 -- wenn keine anderen Lieferanten mehr vorhanden sind
>  || EUR preis_lieferant < snd (andere_lieferanten !! 0) = [(name_lieferant, EUR preis_lieferant)] -- wenn aktueller Lieferant guenstiger ist
>  | EUR preis_lieferant == snd (andere_lieferanten !! 0) = ((name_lieferant, EUR preis_lieferant):andere_lieferanten) --wenn andere Lieferanten teurer sind
>  | otherwise = andere_lieferanten
>  where
>    andere_lieferanten = finde_guenstigsten_lieferanten lieferanten
> finde_guenstigsten_lieferanten (_:lieferanten) = finde_guenstigsten_lieferanten lieferanten -- wenn Preis von naechstem element 'nothing' ist
> finde_guenstigsten_lieferanten _ = [] -- leere Liste


Aufgabe A.4

> type RabattierterPreis = EUR

> guenstigste_Lieferanten_im_Lieferfenster ::  Suchanfrage -> Lieferfenster -> Stueckzahl -> Lieferanten -> [(Lieferantenname,RabattierterPreis)]
> guenstigste_Lieferanten_im_Lieferfenster anfrage lieferfenster stueckzahl lieferanten = guenstigsten_preise
>  where
>    guenstigsten_preise = finde_guenstigsten_lieferanten lieferanten_preise
>    lieferanten_preise = 
>     [(l, skontorabatt_in_lieferfenster (datensatz_aus_sortiment (lieferanten l) anfrage) lieferfenster stueckzahl)
>     | l <- [L1, L2, L3, L4, L5, L6, L7, L8, L9, L10]]


> skontorabatt_in_lieferfenster :: Datensatz -> Lieferfenster -> Stueckzahl -> Maybe Nat1
> skontorabatt_in_lieferfenster DS{lieferbare_stueckzahl_im_Zeitfenster=stueckzahl_zeitraum, preis_in_euro=preis, skonto=skonto} fenster stueck
>  | (stueckzahl_zeitraum fenster) < stueck = Nothing
>  | otherwise = Just (rabattierung (preis * stueck) skonto)
> skontorabatt_in_lieferfenster Nicht_im_Sortiment _ _ = Nothing -- 'Nothing' wenn nicht im Sortiment


> rabattierung :: Nat1 -> Skonto -> Nat1 -- Hilfsfunktion zur Anwendung der Rabatte
> rabattierung preis Kein_Skonto = preis
> rabattierung preis DreiProzent = (ceiling (0.97 * fromIntegral preis))
> rabattierung preis FuenfProzent = (ceiling (0.95 * fromIntegral preis))
> rabattierung preis ZehnProzent = (ceiling (0.9 * fromIntegral preis))