CM.make "$cml/cml.cm";
open CML; 

 val chan1: int chan=channel();
 val chan2: int chan=channel();
 val chan3: int chan=channel();

 fun sender1 i 0=()
    |sender1 i n=(send(chan1,i);sender1 (recv chan2) (n-1))

  fun sender2 v=
  let val i=recv chan1
      val new=i + v
  in 
    send(chan3,i);
    send(chan2,new);
    sender2 (i)
  end

  fun receiver()=
  let val i=recv chan3
  in
    TextIO.print((Int.toString i)^"\n");
    receiver()
  end

fun main ()=
let
  val _=spawn(fn()=>sender1 0 43)
  val _=spawn(fn()=>sender2 1)
  val _=spawn(fn()=>receiver())
in
()
end;
RunCML.doit(main, NONE);
