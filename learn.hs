double x = x + x

righttriangle a b c = (square a) + (square b) - (square c) == 0

square x = x^2

doublesmall x = if x>100 then x else x*2

soumya = "Soumya Deepta Sanyal"

revhelper x y = case x of
 []-> y
 _ -> revhelper (tail x) ((head x): y)

rev x = if x==[] then x else revhelper x []


listaddhelper x y flipped = case flipped of
 False -> listaddhelper (rev x) y True
 True -> case x of 
  [] -> y
  _ -> listaddhelper (tail x) ((head x): y) True

listadd x y = if x == [] then y else listaddhelper x y False

