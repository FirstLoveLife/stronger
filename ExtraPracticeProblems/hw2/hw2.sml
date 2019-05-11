fun alternate(data: int list) =
    case data of
        [] => 0
      | [hd] => hd
      | a::b::hd => a - b + alternate(hd)

fun min_max(ints:int list) =
    let
        val min = 99999999
        val max = ~99999999
        fun min_max_aux(ints, min, max) =
            case ints of
                [] => (min, max)
              | one::tl =>
                case (min > one, max < one) of
                    (true, true) => min_max_aux(tl, one, one)
                  | (true, false) => min_max_aux(tl, one, max)
                 | (false, true) => min_max_aux(tl, min, one)
                 | (false, false) => min_max_aux(tl, min, max)
    in
        min_max_aux(ints, min, max)
    end

fun cumsum(ints:int list) =
    let
        fun cumsum_aux(old, new, count) =
            case old of
                [] => new
              | hd::tl => cumsum_aux(tl, new@[count + hd], count + hd)
    in
        cumsum_aux(ints, [], 0)
    end

fun greeting(greet:string option) =
    case greet of
        NONE => "Hello, there, you!"
     | SOME name => "Hello, there, "^name

fun repeat(l1:int list, l2:int list) =
    let
        fun n_elem(a, b) =
            case b of
                0 => []
             | x => a::n_elem(a, b-1)
    in
        case (l1, l2) of
            ([], []) => []
          | (h1::t1, h2::t2) =>
            let
                val a = n_elem(h1, h2)
            in
                case a of
                    [] => repeat(t1, t2)
                  | l => l@repeat(t1, t2)
            end
    end

fun addOpt(i1:int option, i2:int option) =
    case (i1, i2) of
        (NONE, _) => NONE
      | (_, NONE) => NONE
      | (SOME i1, SOME i2) =>  SOME (i1 + i2)

fun foldl(f, init, list) =
    case list of
        [] => init
      | hd::tl => foldl(f, f(hd, init), tl)

fun filter(f, list) =
    case list of
        [] => []
      | hd::tl =>
        case f(hd) of
            true => hd::filter(f, tl)
          | false => filter(f, tl)

fun addAllOpt(ints:int option list) =
    let
        val rest = filter(isSome, ints)
    in
        case rest of
            [] => NONE
          | rest => foldl(fn(a, b) => SOME ((valOf a) + (valOf b)), SOME 0, rest)
    end

fun any(bools) =
    foldl(fn(a, b) => a orelse b, false, bools)

fun all(bools) =
    foldl(fn(a, b) => a andalso b, true, bools)

fun zip(l1, l2) =
    case (l1, l2) of
        ([], _) => []
     | (_, []) => []
     | (h1::t1, h2::t2) => (h1, h2)::zip(t1, t2)

datatype Cmp = Equal | Greater | Less
fun zipRecycle(l1, l2) =
    let
        val cmp = length l1 - length l2
        val res = if cmp = 0 then Equal else if cmp > 0 then Greater else Less
        fun zipGreater(l1', l2') =
            case (l1, l2) of
                (rest, []) => zipGreater(rest, l2)
              | ([], _) => []
              | (h1::t1, h2::t2) => (h1, h2)::zipGreater(t1, t2)
        fun zipLess(l1', l2') =
            case (l1', l2') of
                ([], rest) => zipLess(l1, rest)
              | (_, []) => []
              | (h1::t1, h2::t2) => (h1, h2)::zipLess(t1, t2)
    in
        case res of
            Equal => zip(l1, l2)
         |  Greater => zipGreater(l1, l2)
         |  Less => zipLess(l1, l2)
    end

fun lookup(l:(string * int) list, s:string) =
    case l of
        [] => NONE
      | hd::tl =>
        case #1 hd = s of
            true => SOME (#2 hd)
         | false => lookup(tl, s)

fun AppendPairList((l1, l2), (l3, l4)) = (l1@l3, l2@l4)

fun splitup(ints: int list) =
    case ints of
        [] => ([], [])
      | hd::tl =>
        if hd >= 0 then AppendPairList(([hd], []), splitup(tl))
        else AppendPairList(([], [hd]), splitup(tl))

fun splitAt(ints: int list, threshold) =
    case ints of
        [] => ([], [])
      | hd::tl =>
        if hd >= threshold then AppendPairList(([hd], []), splitAt(tl, threshold))
        else AppendPairList(([], [hd]), splitAt(tl, threshold))

fun isSorted(ints) =
    case ints of
        [one] => true
      | [] => true
      | one::tl => one <= hd tl andalso isSorted(tl)

fun isDeSorted(ints) =
    case ints of
        [one] => true
      | [] => true
      | one::tl => one >= hd tl andalso isDeSorted(tl)


fun isAnySorted(ints) =
    isSorted(ints) orelse isDeSorted(ints)

fun sortedMerge(l1, l2) =
    case (l1, l2) of
        ([], []) => []
      | (left, []) => left
      | ([], right) => right
      | (left, right) =>
        if hd left >= hd right
        then hd right::sortedMerge(left, tl right)
        else hd left::sortedMerge(tl left, right)

fun qsort(ints) =
    case ints of
        [] => []
     |  [one] => [one]
     | ints =>
       let
           val (left, right) = splitAt(tl ints, hd ints)
       in
           qsort(right)@[hd ints]@qsort(left)
       end

fun divide(ints) =
    let
        datatype pos = left | right
        fun divide_aux(ints, flag) =
            case ints of
                [] => ([], [])
              | hd::tl =>
                case flag of
                    left => AppendPairList(([hd], []), divide_aux(tl, right))
                 |  right => AppendPairList(([], [hd]), divide_aux(tl, left))

    in
        divide_aux(ints, left)
    end

fun fullDivide(a, b) =
    let
        val m = b mod a
        fun fullDivideAux(a, b, acc) =
            case (b div a) mod a of
                  0 => fullDivideAux(a, b div a, acc + 1)
                | n => (acc + 1, b div a)
    in
        case m of
            0 => fullDivideAux(a, b, 0)
          | _ => (0, b)
    end

fun Bits(n) =
    let
        fun BitsAux(n, res) =
            if n = 1
            then res + 1
            else BitsAux(n div 2, res + 1)
    in
        BitsAux(n, 0)
    end

fun pow(x, y) =
    let
        fun aux(ans, acc) =
            case acc of
                0 => ans
              | n => aux(ans * x, acc - 1)
    in
        aux(1, y)
    end

fun IntegerSquareRoot(n) =
    let
        val old = n
        fun IntegerSquareRootAux(ans, left, right) =
            case (ans * ans > old, (ans - 1) * (ans - 1) <= old) of
                (true, true) => ans - 1
             |  (false, false) => old
              | (true, false) => IntegerSquareRootAux((ans + left ) div 2, left, (left + right) div 2)
              | (false, true) => IntegerSquareRootAux((ans + right ) div 2, (left + right) div 2, right)
    in
        case n of
            1 => 1
         |  2 => 2
         | _ =>
           IntegerSquareRootAux(n div 2, 0, n)
    end

fun factorize(x) =
    let
        val sqr = IntegerSquareRoot(x)
        fun aux(y) =
            case x >= y of
                false => []
             |  true =>
                if x mod y = 0
                then
                    let
                        fun isPrime(a, b) =
                            case a = b of
                                true => true
                             |  false =>
                                if a mod b = 0
                                then false
                                else true andalso isPrime(a, b+1)
                    in
                        if isPrime(y, 2) orelse y = 2
                        then case fullDivide(y,x) of
                                 (m, n) => (y, m)::aux(y + 1)
                        else aux(y + 1)
                    end

                else aux(y + 1)
    in
        aux(2)
    end

fun multiply(intPairList) =
    case intPairList of
        (l, r)::tl => pow(l, r) * multiply(tl)
      | [] => 1

fun all_products(intPairList) =
