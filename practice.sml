fun map (f, xs) =
    case xs of
        [] => []
     | hd::tl => f hd::map(f, tl)

fun fold (f, acc, xs) =
    case xs of
        [] => acc
     | x::xs => fold(f, f(acc, x), xs)
