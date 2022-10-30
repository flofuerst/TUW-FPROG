import Angabe3


m31 = Z (LE 1)(Z (LE 1)(LZ (LE 1)))
m22 = Z (E 1 (LE 1)) (LZ (E 1 (LE 1)))
m32 = Z(E 1 (LE 1)) (Z (E 1 (LE 1)) (LZ (E 1 (LE 1))))
m32' = Z(E 1 (LE 1)) (Z (E 1 (LE 1)) (LZ (E 1 (LE 1))))
m11 = LZ (LE 1)
m11' = LZ (LE 1)
m14 = LZ (E 1(E 1(E 1 (LE 1))))
m14' = LZ (E 1(E 1(E 1 (LE 1))))
m24 = Z (E 1(E 1(E 1 (LE 1)))) (LZ (E 1(E 1(E 1 (LE 1)))))
m24' = Z (E 1(E 1(E 1 (LE 1)))) (LZ (E 1(E 1(E 1 (LE 1)))))
n32 = Z(E 2 (LE (-2))) (Z (E 1 (LE (-1))) (LZ (E (-10) (LE 1))))
n32' = Z(E 1 (LE 1)) (Z (E 1 (LE 3)) (LZ (E 1 (LE 1))))
n11 = LZ (LE 5)
n11' = LZ (LE 10)
n14 = LZ (E 1(E 1(E 1 (LE 21))))
n14' = LZ (E 1(E 1(E 2 (LE 1))))
n24 = Z (E 1(E 1(E 1 (LE 1)))) (LZ (E 3(E 1(E 1 (LE 1)))))
n24' = Z (E (-11)(E (-99)(E 2321 (LE (-1231))))) (LZ (E (-11)(E 1231(E (-8888) (LE (-2))))))
mk1 = Z (E 2 (LE 1))(LZ (LE 10)) 
mk2 = Z (E 2 (LE 1))(LZ (E 10 (E 10 (E 2 (LE 2))))) 
mk3 = Z (E 1(E 1(LE 1))) (LZ (E 1(E 1(E 1 (LE 1)))))
infiniRow = E 0 infiniRow
infiniMatrix = Z (LE 1) (Z (E 2 (LE 3)) (Z infiniRow (Z (E (-3)(LE (-2))) (LZ (LE (-1))))))
infiniMatrix2 = Z (LE 1) (Z infiniRow (Z infiniRow (Z infiniRow (LZ (LE (-1))))))

mkz::Int->Int->Zeile
mkz x 1 = LE x
mkz x n = E (n*x) (mkz x (n-1)) 
mkm::Int->Int->Int->Matrix
mkm x 1 n = LZ (mkz x n)
mkm x m n = Z (mkz x n) (mkm x (m-1) n)
main = do
    print(" Matrixtyp check for task 1")              
    print(matrixtyp mk1 == KeineMatrix)
    print(matrixtyp mk2 == KeineMatrix)
    print(matrixtyp mk3 == KeineMatrix)
    print(matrixtyp m32 == Matrix_vom_Typ (3,2))
    print(matrixtyp n11 == Matrix_vom_Typ (1,1))       
    print(matrixtyp n24 == Matrix_vom_Typ (2,4))

    putStr("\n Equality check for task 2\n")
    print(m32 == m32')
    print(m24 == m24')
    print(m11 /= n11)
    print(m32 /= n32)
    print(n14 /= n14')
    print(m14 == m14')

    putStr("\n + Test for task 3\n")
    print(n11 + n11' ==  LZ (LE 15))
    print(n32 + n32 == Z (E 4 (LE (-4))) (Z (E 2 (LE (-2))) (LZ (E (-20) (LE 2)))))
    print(m32 + n32 == Z (E 3 (LE (-1))) (Z (E 2 (LE 0)) (LZ (E (-9) (LE 2)))))
 

    putStr("\n - Test for task 3\n")
    print(n11 - n11' ==  LZ (LE (-5)))
    print(n32 - n32 == Z (E 0 (LE 0)) (Z (E 0 (LE 0)) (LZ (E 0 (LE 0)))))
    print(m32 - n32 == Z (E (-1) (LE 3)) (Z (E 0 (LE 2)) (LZ (E 11 (LE 0)))))

    putStr("\n abs Test for task 3\n")
    print(abs n11' ==  LZ (LE 10))
    print(abs n32 == Z (E 2 (LE 2)) (Z (E 1 (LE 1)) (LZ (E 10 (LE 1)))))
    print(abs n24' == Z (E 11 (E 99 (E 2321 (LE 1231)))) (LZ (E 11 (E 1231 (E 8888 (LE 2))))))

    putStr("\n fromInteger Test for task 3\n")
    print(fromInteger 22 == LZ (LE 22))
    print(LZ (LE 5) + fromInteger 7 == LZ (LE 12))
    print(fromInteger (-1) == LZ (LE (-1)))


    putStr("\n mixed Test\n")
    print(n32 + abs n32 ==  Z (E 4 (LE 0)) (Z (E 2 (LE 0)) (LZ (E 0 (LE 2)))))
    print(abs m24 - abs n24' == Z (E (-10) (E (-98) (E (-2320) (LE (-1230))))) (LZ (E (-10) (E (-1230) (E (-8887) (LE (-1)))))))
    print(n24'-abs n24' == Z (E (-22) (E (-198) (E 0 (LE (-2462))))) (LZ (E (-22) (E 0 (E (-17776) (LE (-4)))))))

    {--putStr("\n error Test\n")
    print((m14 + n11) == error "Argument(e) typfehlerhaft")
    print((m14 + n11) == error "Argument(e) typfehlerhaft")
    print((abs m32-n24) == error "Argument(e) typfehlerhaft")
    print((n24' == m32) == error "Argument(e) typfehlerhaft")
    print((m31 == mk1) == error "Argument(e) typfehlerhaft")
    print((mk1 /= m31) == error "Argument(e) typfehlerhaft")
    
    putStr("\n row 3/5 is infinite \n")
    print(matrixtyp infiniMatrix == KeineMatrix)
    print(infiniMatrix+m32 == error "Argument(e) typfehlerhaft")
    print(n24'- infiniMatrix == error "Argument(e) typfehlerhaft")
    print(abs infiniMatrix == error "Argument(e) typfehlerhaft")        
    print((infiniMatrix == infiniMatrix) == error "Argument(e) typfehlerhaft")
    print((infiniMatrix /= infiniMatrix) == error "Argument(e) typfehlerhaft")

    putStr("\n Stress Test\n")
    print(matrixtyp (mkm 0 20000 20000) == Matrix_vom_Typ (20000,20000))
    print((mkm 0 2000 2000==mkm 0 2000 2000) == True)
    print((mkm 0 2000 2000/=mkm 0 2000 2000) == False)
    print(mkm 3 2000 2000 + mkm 2 2000 2000 == mkm 5 2000 2000)
    print(mkm 3 2000 2000 - mkm 1 2000 2000 == mkm 2 2000 2000)
    print(abs (mkm (-3) 500 500) == mkm 3 500 500)
    print(mkm 3 500 500 - abs (mkm (-2) 500 500) + mkm 4 500 500 == mkm 5 500 500) --}