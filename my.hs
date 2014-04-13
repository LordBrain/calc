import Parcalc

main = do
    print $ pExp $ myLexer "3 + 2" -- ==?> Ok (EAdd (EInt 3) (EInt 2))
