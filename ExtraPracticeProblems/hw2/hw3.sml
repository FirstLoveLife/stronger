use "hw2.sml";

type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

fun pass_or_fail({grade:int option,id:'a}) =
    case grade of
        NONE => fail
      | SOME grade =>
        if grade >= 75
        then pass
        else fail

fun has_passed({grade:int option,id:'a}) =
    case pass_or_fail({grade = grade, id = id}) of
        pass => true
      | fail => false

fun number_passed(l:final_grade list) =
    let
        fun aux(l, ans) =
            case l of
                [] => ans
              | hd::tl =>
                case hd of
                    {id = id, grade = grade} =>
                    if has_passed({grade = grade, id = id})
                    then aux(tl, ans + 1)
                    else aux(tl, ans)
    in
        aux(l, 0)
    end

fun number_misgraded(l) =
    case l of
        [] => 0
      | hd::tl =>
        case hd of
            (pass, x) =>
            if has_passed(x) then number_misgraded(tl) else 1 + number_misgraded(tl)
          | (fail, x) =>
            if has_passed(x) then 1 + number_misgraded(tl) else number_misgraded(tl)

datatype 'a tree = leaf
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

fun max(a, b) = if a < b then b else a
fun tree_hight(tree:'a tree) =
    case tree of
       leaf => 0
      | node {value = value, left = l, right = r} => max(1 + tree_hight(l), 1+tree_hight(r))

fun sum_tree(tree) =
    case tree of
        leaf => 0
      | node {value = value, left = l, right = r} => value + sum_tree(l) + sum_tree(r)

fun gardener(tree) =
    case tree of
        leaf => leaf
      | node {value = value, left = l, right = r} =>
        (
          case value of
            leave_me_alone => node {value = value, left = l, right = r}
           | prune_me => leaf
        )
datatype nat = ZERO | SUCC of nat

fun is_positive(n) =
    case n of
        ZERO => false
      | _ =>  true

exception Negative
fun pred(n) =
    case n of
        ZERO => raise Negative
     | SUCC(n) => n

fun nat_to_int(n) =
    case n of
        ZERO => 0
      | SUCC(n) => 1 + nat_to_int(n)

fun int_to_nat(i) =
    if i >= 0
    then
        case i of
            0 => ZERO
          | i => SUCC(int_to_nat(i-1))
    else
        raise Negative

fun add(n1, n2) =
    case n1 of
        ZERO => n2
      | SUCC(n1) => add(n1, SUCC(n2))

fun sub(n1, n2) =
    case n2 of
        ZERO => n1
      | n2 => sub(pred(n1), pred(n2))

fun mult(n1, n2) =
    let
        fun mult_aux(ans, n2) =
            case n2 of
                ZERO => ZERO
              | SUCC(ZERO) => ans
              | n2 => mult_aux(add(n1, ans), pred(n2))
    in
        mult_aux(n1, n2)
    end

fun less_than(n1, n2) =
    case (n1, n2) of
        (ZERO, ZERO) => false
      | (ZERO, _) => true
      | (_, ZERO) => false
      | (n1, n2) => less_than(pred(n1), pred(n2))

datatype intSet =
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the tw
