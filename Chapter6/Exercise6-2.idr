import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Int)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]


data Format = Number Format
            | Dble Format
            | Chr Format
            | Str Format
            | Lit String Format
            | End

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Dble fmt) = (i : Double) -> PrintfType fmt
PrintfType (Chr fmt) = (i : Char) -> PrintfType fmt
PrintfType (Str fmt) = (stf : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Dble fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt (Chr fmt) acc = \c => printfFmt fmt (reverse (strCons c acc))
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dble (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

-- *Exercise6-2> :t printf "%c %f"
-- printf "%c %f" : Char -> Double -> String

-- *Exercise6-2> printf "%c %f" 'X' 24
-- "X 24.0" : String


TupleVect : (size : Nat) -> Type -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = Pair ty (TupleVect k ty)

test : TupleVect 4 Nat
test = (1,2,3,4,())