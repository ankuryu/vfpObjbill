* Program to test the ObjParty
* requires files objParty and  frmObjparty.scx


set proc to objparty.prg
clea all
kontP1 = createobject("kPrty")
kontp1.fname = "tmpq4.dbf"  &&   file name of the party containing the objects
*select pcode,pname,mvat from (kontp1.fname) order by pcode noconsole nowait 

*kontP1.rd_obj()
kontP1.brw_obj("S")
read event
wait window "over..." nowait



 kontP1.frmobj  = NULL &&   remove the form
 kontp1.frmbrw = NULL  &&    remove the form to browse 
KontP1 = NULL
clea all
return


* repl all mvat with  subst(tmpq.p_addr,at("VAT NO",tmpq.p_addr)+7,11)
* above command is for  generating the MVAT number from  the address fields of 
* the invoice files.
