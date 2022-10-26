import Angabe1

main = do
    -- haeufigkeit
    print ("haeufigkeit \"AMOGUS\"")
    print (haeufigkeit "AMOGUS" == [('A',1),('M',1),('O',1),('G',1),('U',1),('S',1)])

    print ("haeufigkeit \"OOOOO\"")
    print (haeufigkeit "OOOOO" == [('O',5)])

    print ("haeufigkeit \"Funktionale Programmierung\n\"")
    print (haeufigkeit "Funktionale Programmierung\n" == [('F',1),('u',2),('n',3),('k',1),('t',1),('i',2),('o',2),('a',2),('l',1),('e',2),(' ',1),('P',1),('r',3),('g',2),('m',2),('\n',1)])
    
    print ("haeufigkeit \"\"")
    print (haeufigkeit "" == [])
    
    print ("haeufigkeit \"[]\"")
    print (haeufigkeit "[]" == [('[',1), (']',1)])
    
    print ("haeufigkeit \"jasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjg\"")
    print (haeufigkeit "jasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjg" == [('j',216),('a',36),('s',144),('d',108),('f',126),('k',144),('h',18),('g',90)])
    
    -- gewicht
    print ("gewicht \"Haskell\" [('a',2),('k',14),('H',10),('l',8)]")
    print (gewicht "Haskell" [('a',2),('k',14),('H',10),('l',8)] == 42)
    
    print ("gewicht \"\" [('a',2),('k',14),('H',10),('l',8)]")
    print (gewicht "" [('a',2),('k',14),('H',10),('l',8)] == 0)
    
    print ("gewicht \"Haskell\" []")
    print (gewicht "Haskell" [] == 0)
    
    print ("gewicht \"jasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjg\" []")
    print (gewicht "jasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjgjasdfjksajfkjfjhgsdfgsjkjsdfkjksjfdgkjsdfkjgksdjg" [] == 0)
    
    print ("gewicht \"Haskell\" [('H', 1), ('l', 10), ('H', 0)]")
    print (gewicht "Haskell" [('H', 1), ('l', 10), ('H', 0)] == -1)

    print ("gewicht \"\" [('H', 1), ('a', 9), ('l',5)] == 0")
    print (gewicht "" [('H', 1), ('a', 9), ('l',5)] == 0)

    print ("gewicht \"\" [] == 0")
    print (gewicht "" [] == 0)

    -- korrigiere
    print ("korrigiere [('H', 1), ('a', 9), ('l',5)]")
    print (korrigiere [('H', 1), ('a', 9), ('l',5)] == [('H', 1), ('a', 9), ('l',5)])
    
    print ("korrigiere [('a',2),('k',14),('H',10),('l',8),('H',10),('k',9)]")
    print (korrigiere [('a',2),('k',14),('H',10),('l',8),('H',10),('k',9)] == [('a',2),('k',14),('H',10),('l',8)])
    
    print ("korrigiere []")
    print (korrigiere [] == [])
    
    print ("korrigiere [('a',1),('a',1)]")
    print (korrigiere [('a',1),('a',1)] == [('a',1)])
    

    -- korrigiere'
    print ("korrigiere' [('H', 1), ('a', 9), ('l',5)] == [('H', 1), ('a', 9), ('l',5)]")
    print (korrigiere' [('H', 1), ('a', 9), ('l',5)] == [('H', 1), ('a', 9), ('l',5)])

    print ("korrigiere' [('a',2),('k',14),('H',10),('l',8),('H',10),('k',9)] == [('a',2),('k',23),('H',20),('l',8)]")
    print (korrigiere' [('a',2),('k',14),('H',10),('l',8),('H',10),('k',9)] == [('a',2),('k',23),('H',20),('l',8)])

    print ("korrigiere' [] == []")
    print (korrigiere' [] == [])

    print ("korrigiere' [('a',1),('a',1)] == [('a',2)]")
    print (korrigiere' [('a',1),('a',1)] == [('a',2)])
