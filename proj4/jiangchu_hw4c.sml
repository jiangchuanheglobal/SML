CM.make "$cml/cml.cm";
open CML;


  fun mailbox inCh outCh l = (
    if length l=0 then 
      (select[wrap(recvEvt inCh, fn a=>(mailbox inCh outCh [a]))])

    else 
      (
      select[wrap(sendEvt (outCh,hd l),fn()=>(
                             mailbox inCh outCh (tl l))),
                             wrap(recvEvt inCh, fn a=>(
                             mailbox inCh outCh (l@[a])) )
                               ]))
                               
   fun receiver ch = (
    sync(wrap(recvEvt ch,fn a=>(TextIO.print(Int.toString(a)^"\n"))));
    receiver ch)

   fun sender ch n=if n>=0 then
    (sync(wrap(sendEvt(ch,n),fn()=>()));sender ch
    (n-1))
                        else ()

   

   fun generator inCh outCh n = if n=0 then outCh 
                               else( 
                               spawn(fn()=>mailbox inCh outCh []);
                               if n=1 then outCh 
                               else 
                               generator outCh (channel()) (n-1)) 
fun main()=
let 
  val inCh = channel()
  val outCh = generator inCh (channel()) 100 

  val _=spawn(fn()=>receiver outCh);
  val _=spawn(fn()=>sender inCh 50); 
  val _=spawn(fn()=>sender inCh 50);

in
  ()
end;
RunCML.doit(main, NONE);
