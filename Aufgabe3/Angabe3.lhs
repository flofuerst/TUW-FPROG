> module Angabe3 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollst�ndigen Sie auch die vorgegebenen Kommentaranf�nge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
5. Hinweis: Kommentar- und Programmzeilen muessen durch mindestens eine Leerzeile getrennt sein!


Als erstes fuehren wir den Typalias Nat1 ein; seine Werte verwenden wir fuer
die Darstellung des Typs von Matrizen:

> type Nat1 = Int
> type Typ  = (Nat1,Nat1)

Matrizen modellieren wir als Listen von (Matrix-) Zeilen ueber entsprechenden
selbstdefierten Listentypen:

Zur Namenswahl: LE fuer `Letztes Element', E fuer `Element'

> data Zeile = LE Int                       
>              | E Int Zeile deriving Show

Zur Namenswahl: LZ fuer `Letzte Zeile, Z fuer `Zeile'

> data Matrix = LZ Zeile                       
>               | Z Zeile Matrix deriving Show  

Um mit Argumenten umzugehen, die keine Matrix darstellen oder im Typ nicht
zueinander passen, fuehren wir den Wert fehler als fehleranzeigenden Wert
ein (aufgrund unserer Festlegung von fehler bedeutet das, dass die Rueckgabe
dieses Werts gleichbedeutend mit dem Aufruf der Funktion error mit dem 
Argument "Argument(e) typfehlerhaft" ist und die Programmausfuehrung mit 
Ausgabe der Zeichenreihe "Argument(e) typfehlerhaft" endet).

> fehler = error "Argument(e) typfehlerhaft"

Abschliessend fuehren wir den algebraischen Datentyp Matrixtyp ein:

> data Matrixtyp = Matrix_vom_Typ Typ 
>                   | KeineMatrix deriving (Eq,Show)


Aufgabe A.1

> matrixtyp :: Matrix -> Matrixtyp
> matrixtyp (LZ element) = Matrix_vom_Typ (1, lineLength element)
> matrixtyp (Z element nextLine) = compareMatrix (lineLength element) (matrixtyp nextLine)

> lineLength :: Zeile -> Nat1
> lineLength (LE element) = 1
> lineLength (E element nextLine) = 1 + lineLength nextLine  

> compareMatrix :: Nat1 -> Matrixtyp -> Matrixtyp
> compareMatrix _ KeineMatrix = KeineMatrix
> compareMatrix counter (Matrix_vom_Typ typ) 
>   | counter == snd typ = Matrix_vom_Typ (fst typ + 1, snd typ)
>   | otherwise = KeineMatrix


Knapp, aber gut nachvollziebar geht matrixtyp folgendermassen vor: 
Zählt wieviele Einträge in einer Zeile stehen und vergleicht dann diese Anzahl mit den anderen Zeilen


Aufgabe A.2

> instance Eq Matrix where
>  m1 == m2 = if equalType m1 m2 then equalMatrix m1 m2 else fehler
>  m1 /= m2 = if equalType m1 m2 then not(equalMatrix m1 m2) else fehler

> equalMatrix :: Matrix -> Matrix -> Bool
> equalMatrix (LZ line1) (LZ line2) = compareLine line1 line2
> equalMatrix (Z _ _) (LZ _) = False
> equalMatrix (LZ _) (Z _ _) = False
> equalMatrix (Z line1 matrix1) (Z line2 matrix2) = compareLine line1 line2 && equalMatrix matrix1 matrix2


> compareLine :: Zeile -> Zeile -> Bool
> compareLine (LE element1) (LE element2) = element1 == element2
> compareLine (LE _) (E _ _ ) = False
> compareLine (E _ _) (LE _) = False
> compareLine (E element1 line1) (E element2 line2) = compareLine line1 line2 && element1 == element2

Knapp, aber gut nachvollziehbar geht die Instanzdeklaration fuer Eq folgendermassen vor:
Vergleicht Zeile für Zeile bzw. Element für Element der Matrix und gibt nur True zurück wenn wirklich alles übereinstimmt
 

Aufgabe A.3

> instance Num Matrix where
>  m1 + m2 = if equalType m1 m2 then calcMatrix m1 m2 (+) else fehler

>  m1 - m2 = if equalType m1 m2 then calcMatrix m1 m2 (-) else fehler
>  abs m = if matrixtyp m /= KeineMatrix then calcMatrix m m (\element1 element2 -> abs element1) else fehler
>  fromInteger z = LZ (LE (fromIntegral z))

>  m1 * m2 = error "(*) bleibt unimplementiert!"
>  negate m = error "negate bleibt unimplementiert!"
>  signum m = error "signum bleibt unimplementiert!"

> equalType :: Matrix -> Matrix -> Bool
> equalType matrix1 matrix2
>   | matrixtyp matrix1 == KeineMatrix = False
>   | matrixtyp matrix1 == matrixtyp matrix2 = True
>   | otherwise = False

> calcMatrix :: Matrix -> Matrix -> (Int -> Int -> Int) -> Matrix
> calcMatrix (LZ line1) (LZ line2) operation = LZ (calcLine line1 line2 operation)
> calcMatrix (Z line1 matrix1) (Z line2 matrix2) operation = Z (calcLine line1 line2 operation) (calcMatrix matrix1 matrix2 operation)

> calcLine :: Zeile -> Zeile -> (Int -> Int -> Int) -> Zeile
> calcLine (LE element1) (LE element2) operation = LE (operation element1 element2)
> calcLine (E element1 line1) (E element2 line2) operation = E (operation element1 element2) (calcLine line1 line2 operation)

Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Num folgendermassen vor:
Führt die Operation Zeile für Zeile bzw. Element für Element einzeln aus und gibt am Schluss die neue Matrix zurück






