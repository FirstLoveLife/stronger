signature CLIST =
  sig
    type 'a clist

    val cnil : 'a clist
    val cons : 'a -> 'a clist -> 'a clist
    val hd : 'a clist -> 'a
  end
structure Clist : CLIST =
struct
  open CML

  datatype 'a request = CONS of 'a | HEAD

  datatype 'a clist = CLIST of { reqCh : 'a request chan, replyCh : 'a chan }

  (* create a clist *)
  val cnil =
    let
      val reqCh = channel()
      val replyCh = channel()
      fun loop l = case recv reqCh of
          CONS x =>
            (loop (x::l))
        |
          HEAD => (let fun head (h::t) = h | head [] = Empty in send(replyCh, head(l)) end ; loop l)
    in
      spawn(fn () => loop nil);
      CLIST {reqCh = channel(), replyCh = channel() }
    end


  fun cons x (CLIST {reqCh, replyCh})=
    (send (reqCh, CONS x); CLIST {reqCh = reqCh, replyCh = replyCh})

  fun hd (CLIST {reqCh, replyCh}) = (send (reqCh, HEAD); recv replyCh)
end
