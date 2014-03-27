CM.make "$cml/cml.cm";
open CML;

 val chan: int chan=channel();

  fun sender i=
    if i>100 then  ()
    else (send(chan,i);sender(i+1);())   

  fun receiver()=
  let val i=recv chan
  in
    TextIO.print ((Int.toString i)^" ");
    receiver()
  end

fun main()=
let
  val _=spawn(fn()=>sender 0)
  val _=spawn(fn()=>receiver()) 
in
  ()
end;

RunCML.doit(main, NONE);
