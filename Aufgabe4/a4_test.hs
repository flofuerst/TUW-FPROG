import Angabe4 

fenster1 :: Lieferfenster -> Nat0
fenster1 f = case f of
  (LF Q1 2023) -> 1
  (LF Q2 2023) -> 3
  (LF Q2 2025) -> 0
  (LF Q3 2025) -> 4
  (LF Q4 2025) -> 0
  (LF Q1 2026) -> 4
  (LF Q4 2026) -> 2
  _ -> 0

fenster2 :: Lieferfenster -> Nat0
fenster2 f = case f of
  (LF Q1 2023) -> 3
  (LF Q2 2023) -> 4
  (LF Q1 2025) -> 2
  _ -> 0

lieferanten :: Lieferantenname -> Sortiment
lieferanten name = case name of
  L1 -> WMS (\t ->
    case t of
      WM_Typ1 -> DS 69 0 fenster1 DreiProzent
      WM_Typ2 -> DS 75 2 fenster2 Kein_Skonto
      WM_Typ3 -> DS 101 2 fenster1 FuenfProzent
      WM_Typ4 -> DS 95 22 fenster2 Kein_Skonto
      WM_Typ5 -> DS 140 1 fenster1 DreiProzent)
  L2 -> WMS (\t ->
    case t of
      WM_Typ1 -> DS 65 1 fenster1 Kein_Skonto
      WM_Typ4 -> DS 99 2 fenster2 ZehnProzent
      WM_Typ5 -> DS 44 2 fenster2 DreiProzent
      _ -> Nicht_im_Sortiment)
  L3 -> WSS (\t ->
    case t of
      WS_Typ1 -> DS 117 2 fenster2 Kein_Skonto
      WS_Typ3 -> DS 103 0 fenster1 Kein_Skonto
      _ -> Nicht_im_Sortiment)
  L4 -> lieferanten L2
  L5 -> lieferanten L2
  L8 -> WSS (\t ->
    case t of
      WS_Typ1 -> DS 117 0 fenster2 DreiProzent
      _ -> Nicht_im_Sortiment)
  L9 -> WTS (\t ->
    case t of
      WT_Typ4 -> DS 250 0 fenster2 ZehnProzent
      _ -> Nicht_im_Sortiment)
  L10 -> lieferanten L1
  _ -> WMS (\t -> Nicht_im_Sortiment)

lieferantenEmpty :: Lieferantenname -> Sortiment
lieferantenEmpty _ = WSS (\t -> Nicht_im_Sortiment)

lieferantenEmptyFenster :: Lieferantenname -> Sortiment
lieferantenEmptyFenster _ = WTS (\t -> DS 130 0 (\_ -> 0) ZehnProzent)

main :: IO ()
main =  do

    print (sofort_erhaeltlich_bei (WM WM_Typ5) lieferantenEmpty == [])
    print (sofort_erhaeltlich_bei (WM WM_Typ5) lieferanten == [L1,L2,L4,L5,L10])
    print (sofort_erhaeltlich_bei (WS WS_Typ1) lieferanten == [L3])
    print (sofort_erhaeltlich_bei (WS WS_Typ3) lieferanten == [])
    print (sofort_erhaeltlich_bei (WT WT_Typ4) lieferanten == [])

    print (sofort_erhaeltliche_Stueckzahl (WM WM_Typ2) lieferantenEmpty == (0, 0))
    print (sofort_erhaeltliche_Stueckzahl (WM WM_Typ2) lieferanten == (4, 300))
    print (sofort_erhaeltliche_Stueckzahl (WS WS_Typ1) lieferanten == (2, 117*2))
    print (sofort_erhaeltliche_Stueckzahl (WS WS_Typ3) lieferanten == (0, 0))
    print (sofort_erhaeltliche_Stueckzahl (WT WT_Typ3) lieferanten == (0, 0))

    print (guenstigste_Lieferanten (WM WM_Typ1) (LF Q1 2023) lieferantenEmpty == Nothing)
    print (guenstigste_Lieferanten (WM WM_Typ1) (LF Q1 2023) lieferantenEmptyFenster == Nothing)
    print (guenstigste_Lieferanten (WM WM_Typ1) (LF Q1 2023) lieferanten == Just [L2,L4,L5])
    print (guenstigste_Lieferanten (WM WM_Typ1) (LF Q2 2025) lieferanten == Nothing)
    print (guenstigste_Lieferanten (WM WM_Typ4) (LF Q1 2023) lieferanten == Just [L1,L10])
    print (guenstigste_Lieferanten (WM WM_Typ1) (LF Q1 2060) lieferanten == Nothing)
    print (guenstigste_Lieferanten (WT WT_Typ4) (LF Q1 2025) lieferanten == Just [L9])
    print (guenstigste_Lieferanten (WT WT_Typ4) (LF Q1 9999) lieferanten == Nothing)

    print("aufgabe4")
    print (guenstigste_Lieferanten_im_Lieferfenster (WS WS_Typ1) (LF Q2 2023) 4 lieferantenEmpty == [])
    print (guenstigste_Lieferanten_im_Lieferfenster (WS WS_Typ1) (LF Q2 2023) 4 lieferantenEmptyFenster == [])
    print (guenstigste_Lieferanten_im_Lieferfenster (WM WM_Typ1) (LF Q1 2023) 999 lieferanten == [])
    print (guenstigste_Lieferanten_im_Lieferfenster (WM WM_Typ4) (LF Q1 2025) 3 lieferanten == [])
    print (guenstigste_Lieferanten_im_Lieferfenster (WM WM_Typ4) (LF Q1 2025) 2 lieferanten == [(L2, EUR 179),(L4, EUR 179),(L5, EUR 179)])
    print (guenstigste_Lieferanten_im_Lieferfenster (WS WS_Typ1) (LF Q2 2023) 4 lieferanten == [(L8, EUR 454)])