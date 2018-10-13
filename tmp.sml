fun test n =
if n = 1
then raise Fail ("hey")
else 2

val t = test 1
