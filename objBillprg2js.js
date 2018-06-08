* File name  objbillprg2js.js 
* Uses jquery
* uses sqlite database
* Uses the following  Files : 
* By : Sunil Gandhi
* As On : 16/05/2016 17:31 IST

*******************
* Global settings
******************
$(document).ready()function(){

}




pmbknt = createobject("kntBill")
with pmbknt
 .co = 'xy'   && set the co code
 .yr = '15'   && et the yr code
 .bk = '1'    && set the book code 
 .set_cyb()
endwith


do form frmtup name m.frmtup noshow 
do form frmitm name m.frmitm noshow && with pmbknt.kurbil.itmLst
m.frmtup.bkobj = pmbknt   && set the refrence to the obj
m.frmitm.bkobj = pmbknt   && set the refrence to the obj
frmitm.oitmkont = frmitm.bkobj.kurbil.itmlst
frmitm.oitm =  frmitm.bkobj.kurbil.itmlst.oitm
pmbknt.rdfrm = m.frmtup
pmbknt.kurbil.itmlst.rdfrm = m.frmitm




clear
*wait window " A D D I N G     N E W    B I L L    !!! "
*pmbknt.addbill()



wait window  "  LOOPING ....... PRESS ESC to EXIT"

do while .t.
select f1no,f1dt,pname from  (pmbknt.fname) order by f1no into cursor billdtl
 sele billdtl
 brow
 if lastkey() = 27
   exit
 endif\
 pmbknt.kurbil.bno = billdtl.f1no
  m.bno = billdtl.f1no
 *pmbknt.getBill( m.bno)
 pmbknt.ediBill(m.bno)
 *wait window  " PRINTING ...."
 *pmbknt.prnBill(m.bno)
enddo 
return 

*pmbknt.addBill()
pmbknt.kurbil.itmLst.addItm()
pmbknt.kurbil.itmLst.addItm()
pmbknt.kurbil.itmLst.addItm()
pmbknt.kurbil.itmLst.addItm()
pmbknt.kurbil.prn_bill()



set alter off
set textmerge off
set alter to
modi comm tmp.txt


********************  End of   TESTING MAIN LOOP **************



****************** Start of Definition of Classes **************





/***********************************************

*    K N T B I L L   C L A S S 

***********************************************/
function Kntbill() {
 var kurbil = NULL ;
 var ttlbil = 0    ;
 var lstbno = ""   ;
 var co = ""       ;
 var yr = "" 	   ;
 var bk = ""       ;
 var fname = ""    ;
 var rdfrm = NULL  ;
// use self invoking anonymous functions for init() 
 function init() {
  this.curbill = new CreaFrm() ;
  this.ttlbil = 0	       ;

  }


 }

/* Define class kntBill  as custom && Defines the container obj for Bills
  kurbil = NULL  && current bill
  ttlbil = 0     && total bill in kont
  lstbno = ""    &&  Last bill no.
  co = ""        &&  Copmany Code 
  yr = ""        &&  Year Code
  bk = ""        &&  Book Code
  fname = ""      && contains the dbf file containing bills
  rdfrm = NULL   &&  Form for reading in data
  
  function init()
   this.kurbil = createobject("Bill")
   this.ttlbil = 0

  return
 
  function nx_bill
   select max(f1no) from (this.fname) into array armx
   this.lstbno = iif (type( 'armx[1]') = 'C', armx[1],'000000')
   nxbil =  tran(val(this.lstbno) + 1,'@l ######')
   return nxbil
   
  function set_cyb  &&  Sets the company Year and book  given the param co,yr,bk
   Priv m.co ,m.yr , m.bk 
   m.co = this.co
   m.yr = this.yr
   m.bk = this.bk
  
   Priv rv,tfname, ifname, vfname  && fname of top,itm and inv
   rv = .t.                                 && set the return value to true
   tfname = m.co + m.yr + m.bk + 'top.dbf'
   ifname = m.co + m.yr + m.bk + 'itm.dbf'
   vfname = m.co + m.yr + 'inv.dbf'
   this.fname = m.tfname                    &&  set the topfile  database name
   this.kurbil.itmlst.fname  = m.ifname     &&  set the itemfile databse  name
   this.kurbil.itmlst.ikifname =  m.vfname  && set the inv file  database name
   msg  = iif( file(m.tfname),"",m.tfname+' ')                 && check if all files are avaialable and 
   msg  = msg + iif(file(m.ifname), "", m.ifname + ' ')        && construct a msg showing which file
   msg  = msg + iif(file(m.vfname), "", m.vfname = ' ')        && not available
   if len(msg) > 0                                             && if msg length > 0 , file absent
     rv = .f.
     msg  = "Files " + msg + "Not Found"                       && show msg accordingly
     wait window msg
   endif
  return rv
  
  

  function addBill()  && Adds a new Bill to container
  
 *  this.kurbil = createobject('bill')
   this.set_cyb()
   this.kurbil.bno = this.nx_bill()
   this.kurbil.bdt = date()
   this.kurbil.f2no = this.kurbil.bno
   this.kurbil.f2dt = date()
   this.kurbil.kntbill = this
   this.rdfrm = frmtup
   this.kurbil.itmlst.rdfrm = frmitm
   =iif(this.rdBill(),this.putBill(this.kurbil.bno),"")
  return
   
   
  function ediBill(pbno)  && Edits the current Bill pbno
   this.getBill(pbno)      && Get the bill no pbno
   =iif(this.rdBill(),this.putBill(pbno),"")
   *this.putBill(pbno)
   return 

  function delBill(pbno)  && Deletes the current Bill pbno

  function prnBill(pbno) && Prints the Bill pbno
   this.getBill(pbno)
   this.kurbil.prn_bill()
  return

  function putBill(pbno)  && Puts the current bill on storage pbno
  * check if bno exists ?  if so  use sql  update else sql insert
  select f1no from (this.fname)  where f1no = m.pbno into array arbno
  if type('arbno[1]')= 'C'
     * update
     with this.kurbil
      m.f1dt = .bdt
      m.f2no = .f2no
      m.f2dt = .f2dt
      m.pcode = .pcode
      m.form = .frm
      m.pname = .pname
      m.p_addr = .p_addr
      m.cr = .cr
     endwith 
  
      update (this.fname)  set f1dt = m.f1dt,;
                               f2no = m.f2no, ;
                               f2dt = m.f2dt , ;
                               pcode= m.pcode, ;
                               form = m.form, ;
                               pname = m.pname, ;
                               p_addr = m.p_addr, ;
                               cr = m.cr ;
                               where f1no  = m.pbno
    
   else
      with this
      insert into(this.fname)  (f1no,f1dt,f2no, f2dt,pcode,form,pname,p_addr,cr)   values (m.pbno,.kurbil.bdt,.kurbil.f2no,.kurbil.f2dt,.kurbil.pcode,.kurbil.frm,.kurbil.pname,.kurbil.p_addr,.kurbil.cr)  
      endwith 
  endif
  
  
  * now  get the total items of the bill in file and acoordingly use upd/ins sql
  this.kurbil.itmlst.put_itms() && put the items into the item container file
  

return 

  
function getBill(pbno)  && Gets the bill from storage into curbill obj
  Private m.bno, obno

  m.obno = this.kurbil  && store the Bill object temporarily in the memory.object (m.obno)
  m.bno = m.obno.bno    && get the bno from the object

  select * from (this.fname) where f1no = m.bno into array ar1   && get the details of the object from the file using (this.fname) into array ar1

obno.bno = ar1[1]    && Bill No
obno.bdt = ar1[2]   && Bill Date
obno.f2no = ar1[3]   && Challan No
obno.f2dt = ar1[4]  && Challan date
obno.pcode =  ar1[5]      && party Code
obno.famt = ar1[6]         && Form Amount
obno.frm = ar1[7]         && Form if any
obno.pname = ar1[8]       && Party Name
obno.p_addr = ar1[9]      && Party Addres
obno.oOrd = NULL      && Object containing Order Details
obno.oDlv = NULL      && Object containig Delivery Details
obno.cr = ar1[12]           && Credit Days
obno.co = ar1[13]          && Company
obno.pobj =  NULL     && Party object


* Now get the items
obno.itmlst.bno = ar1[1]     && set the billno
obno.itmlst.get_itms(m.bno)  && get the items into the items list


return


function rdBill
Priv rv
Priv m.f1no,m.f1dt, m.f2no, m.f2dt, m.pcode, m.pname , m.p_addr
Priv m.cr
with this.kurbil
 m.f1no = .bno
 m.f1dt = .bdt
 m.f2no = .f2no
 m.f2dt = .f2dt
 m.pcode= .pcode
 m.pname = .pname
 m.p_addr = .p_addr
 m.frm = .frm
 m.cr = .cr
endwith

xval= this.rdfrm.show()

read events
rv = .f.
if this.rdfrm.xit = 1
  with this.kurbil
   .bno = m.f1no 
   .bdt  = m.f1dt
   .f2no = m.f2no
   .f2dt = m.f2dt
   .pcode = m.pcode 
   .pname = m.pname
   .p_addr = m.p_addr
   .frm = m.frm
   .cr = m.cr
   rv = .t.
  endwith
endif


return rv


function prn_bill
acti scree
clear
with this.kurbil
text
                                 Panalal Mohanlal & Co
=========+=========+=========+=========+=========+=========+=========+=========+
<< this.pname>>              B no <<.bno>>    Dt << .bdt>>
<< mline(.p_addr,1)>>       Ch no<<.f2no>>   Dt << .f2dt>>
<< mline(.p_addr,2)>>                              
<< mline(.p_addr,3)>>
<< mline(.p_addr,4)>>
=========+=========+=========+=========+=========+=========+=========+=========+
Sr|12345678901234567890123456|123Qty789|123Unt789|12Rate789|1Disc|1Tax5|12Amt67|
  |                          |         |         |         |     |     |       |

endtext

Priv n
mxitm = .itmlst.itmknt

m.itmlst = .itmlst
for n = 1 to mxitm
 m.itm = itmlst.lstItm[n]
 ? m.itm.sr + "|" 
 ?? left(mline(m.itm.descr,1),26) + "|"
 ?? tran(m.itm.qty,"####") + "|"
 ?? padr(m.itm.unt,9) + "|"
 ?? tran(m.itm.rate,'#####.##') + "|"
 ?? tran(m.itm.dsc,"###.##") + "%\|"
 ?? tran(m.itm.tax,"###.##") + "%\|"
 ?? tran(m.itm.amt,'#######.##')+'|'
 ?
next n
endwith
return




Enddefine  && End of objet kntbill

*/
*********************************************

***********************************************

*          B I L L   C L A S S 

***********************************************

Defin Class bill as custom   && Individual Bill obj
bno = spac(6)    && Bill No
bdt = ctod("")   && Bill Date
f2no = spac(6)   && Challan No
f2dt = this.bdt  && Challan date
pcode =  ""      && party Code
famt = 0         && Form Amount
frm = ""         && Form if any
pname = ""       && Party Name
p_addr = ""      && Party Addres
oOrd = NULL      && Object containing Order Details
oDlv = NULL      && Object containig Delivery Details
cr = 0           && Credit Days
co = ""          && Company
pobj =  NULL     && Party object
itmlst = NULL    && Knt obj of items 
itmknt = 0       && Kount of ttl items 


cr = 0 
* rdfrm = NULL  && Form to read it
kntbill = NULL && Kontainer object of bills

function init()
 wait windo "Initiating...Bill Object" nowait

 this.itmlst = createobject("kntItm",this.bno)
 




Enddefine
***********************************************

***********************************************

*    K N T I T M     C L A S S 

***********************************************

Define class kntitm  as custom  && Define kontainer obj of items
bno = spac(6)
Dime lstItm[1] = NULL         && list of items
itmknt  = 0                   &&  item kounts
oItm  = NULL                  && Item object
cursr  = 0                    && current serial
fname = ""                    && file name of items
ikifname = ""                 && iki file name
rdfrm  = NULL                 &&  form to read items

function init(m.bno)
 wait window "Creating item Container..."
 this.bno = m.bno
 this.oItm = createobject("itm")
 this.itmKnt = 0
 
 
return

function addItm()
Priv i,m.sr
 this.itmknt = this.itmknt + 1
 i = this.itmknt
 m.sr = tran(i,'@l ##')
 Dime this.lstItm[i]
 
 this.lstItm[i] = createobject('itm',this.bno,m.sr)
 this.oItm = this.lstItm[i]
 this.oItm.sr = m.sr
 wait window "Bill no : " + this.bno
 * set step on
 this.oItm.bno =  this.bno  && m.bno is presumed to be the current bno
 this.rdfrm.oitm = this.oitm
 with this.oitm
   .typ = 'INV'
   .unt = 'NOS'
   .amt = 0
  endwith 
 
  
 this.rdItm()
 
 
return

 
function ediItm()
* set step on
 wait window "Editing ... Item Sr " + itms.sr
 if val(itms.sr) > 0
   this.rdfrm.oitm = this.lstItm[val(itms.sr)]
   this.rdItm()
 endif  
 return
 
function delItm()

function put_itms()
 wait window "putting items into the file... " nowait
*  set step on
 select sum(1) from (this.fname)as itmk where this.bno == itmk.bno into array aritm
 * if items don't exist insert all the items as new
 xitm = iif(type("aritm")= "U",0,aritm[1])
 Do case
 case xitm = 0                       &&  No items previously
   this.upd_itm("ins",1,this.Itmknt)      && insert all the items
 case xitm = this.Itmknt             &&  same number of items as before
   this.upd_itm("upd",1,this.Itmknt)      && update all the items
 case xitm > this.Itmknt             &&  More items previously , remove space
   this.upd_itm("upd",1,this.Itmknt)      && update the items
   this.upd_itm("rmv",this.Itmknt+1,xitm) && remove the excess items
 case xitm < this.Itmknt             &&  More items now , make space
   this.upd_itm("upd",1,xitm)             &&  update the existing items
   this.upd_Itm("ins",xitm+1,this.Itmknt) &&  insert the balance items
 endcase

return

function upd_itm(opr,st,ed)       &&  operation/ start / end
 Priv n,ot,tmp1,tmp2
 store  left(this.fname,at(".dbf",this.fname)) to tmp1,tmp2
 tmp1 = tmp1+ "desc"
 tmp2 = tmp2 + "type"


   for n = st to ed
     ot = this.lstItm[n]
     with this.lstItm[n]
     Do Case 
     Case opr = "ins"

       Insert into (this.fname) (bno,sr,type,qty,unt,rate,dsc,tax,amt,iki,desc ) ;
        values(.bno,.sr,.typ,.qty,.unt,.rate,.dsc,.tax,.amt,.iki,.descr) 
  
      Case opr = "upd"
*        tmpvar = this.fname + ".desc" && this has to be done as desc is reserved word
*        tmp2 = this.fname + ".type"   && same as above
        UPDATE (this.fname) set bno= ot.bno, sr = ot.sr , &tmp2 = ot.typ , qty = ot.qty ,;
                                unt = ot.unt, rate = ot.rate , dsc = ot.dsc , tax = ot.tax , ;
                                iki = ot.iki , &tmp1 = ot.descr ;
                                where bno == ot.bno .and. sr == ot.sr
      Case opr = "rmv"
        
      Endcase
     endwith
    Next n
  
 return


function get_itms()   && gets items for a particular form
Para fno
select * from (this.fname) where bno = m.fno into table itms
 		
this.Itmknt = reccount("itms")

if this.Itmknt > 0
  Dime this.lstItm[this.itmKnt]
  select itms
  go top
  For n = 1 to this.itmKnt
    scatter memvar memo
    this.lstItm[n] = createobject('itm',m.bno,m.sr)
    v = this.lstItm[n]
    *v.bno = m.bno
    *v.sr =  m.sr
    v.typ = m.type           && Type
    v.qty = m.qty            && Qty
    v.unt = m.unt
    v.rate = m.rate           && 
    v.dsc = m.dsc            &&
    v.tax = m.tax            &&
    v.amt = m.amt            &&
    v.iki = m.iki           &&
    v.descr = m.desc         &&
    select itms              && Now transfer the data to read/write cursor
*    append blank             &&
*    gather memvar memo       && itms so that we can edit the data
*    select tmp1              && restore it back to tmp1
    skip
  Next n
 else
   Dime this.lstItm[1]       && Doesn't have items.
 endif 

function rdItm()
 this.rdfrm.oitmkont = this
 frmitm.grditm.recordsource = "itms"
 
 this.rdfrm.show()
 this.rdfrm.grditm.doscroll(2) && scroll up a page
 this.rdfrm.cmbTyp.setfocus()
 read events
return

 
function dsp_itms()

Enddefine


*****************************************


***********************************************

*          I T M     C L A S S 

***********************************************

define class itm  as custom
bno = ""           && Bill No
sr = ""            && Sr no
typ = ""           && Type
qty = 0            && Qty
unt = ""           && Unit
rate = 0           && 
dsc = 0            &&
tax = 0            &&
amt = 0            &&
iki = ""           &&
descr = ""         &&
* rdfrm  = NULL      &&  form to read items
kntitm = NULL      &&  pointer to the container of items
omcs = NULL        &&  invobj
tcurs = "itms"         &&  temp cursor name for retrieving and saving item data

function init(m.bno,m.sr)
 wait window "Creating item ..."
 this.bno = m.bno
 this.sr =  m.sr                             &&spac(m.sr)
 with this
   store 0 to .qty,.rate,.dsc,.tax,.amt
   store "" to .unt,.iki,.descr
 endwith
 return
 

function get_itm2c(m.sr)
 wait window "get_Itm2c(m.sr)"
return
 priv a
 select * from  (this.tcurs) where itms.sr == m.sr into cursor juju
  scatter memvar memo
  use juju

function put_itm2c(m.sr)
priv fn2

with this
 .amt = iif(.typ $ "ITMINV",round(.rate * .qty , 2),.amt)
  m.type = .typ
  m.qty = .qty
  m.unt = .unt
  m.rate = .rate
  m.dsc = .dsc
  m.tax = .tax
  m.amt = .amt 
  m.desc = .descr
endwith
 wait window "put_itm2c(m.sr)"
* uflg = .f.
 
*  set step on
 select sum(1) from (this.tcurs) into array ar_sr where sr = m.sr
if _tally = 0
 insert into (this.tcurs) (sr,type,qty,unt,rate,dsc,tax,amt,desc ) ;
 values (m.sr,m.type,m.qty,m.unt,m.rate,m.dsc,m.tax,m.amt,m.desc)
else 
 fn2 = this.tcurs
 update (fn2) set bno=m.bno,itms.type=m.type,qty=m.qty,unt=m.unt,rate=m.rate,dsc = m.dsc, ;
  tax = m.tax, amt = m.amt , itms.desc = m.desc ;
   where  sr == m.sr
endif 

Enddefine




**********************************************



**********************************************
Define Class omcs as custom
 rcno = ''          && recno used for updating the records.
 mfg = spac(3)      && Mfg code
 icode = spac(10)   && Item Code
 asize = spac(3)    && ASize Code
 iki =  repl(6,'0') && iki for relating to invoice/inv

 oitm = NULL        && ref to the itm object 
 kntiki = NULL      && container for the iki objects
 fname = ""                    && file name of inventory file

function init()

return


function nxt_iki()  &&  Gets the next iki sequentially
  Priv ar1
  select max(iki) from (this.fname) where iki # '999999' into array ar1
  if type('ar1[1]' # 'C')
     rv ='000001'
  else
     rv = ar[1]
  endif
return rv


 function get_iki(iki)
 Priv ar1,rv 
 rv = .f.
 select recno(),mfg,icode,asize from (this.fname) into array ar1
 if (type('ar1[1]') = 'U')
   with this
     .rcno = ar[1]
     .mfg = ar[2]
     .icode = ar[3]
     .asize = ar[4]
     .iki = m.iki
   endwith  
   rv = .t.
  endif
  return rv
   

 return
 
 
 
 function put_iki(iki)
 

 return


 function add_iki(iki)

 return

 function edi_iki(iki)

 return

 function del_iki(iki)

 return



 
 
 function rd_mcs()
   
 return 
 
 
 function get_desc
 
 return
 
 function get_pri
 
 return
 

Enddefine

*********************************************
Define class oPrty  as custom
pcode = ''         && party code
pname = ''         && Party Name
p_addr = ''        && Party Address
mvat = ''          && Mvat Number
pan = ''           && Pan Number of Party
fname = NULL       &&  File containing the Details of 

enddefine



Define class  oord as custom  
ordno = ''
orddt = ctod('')

function ini()

return


Enddefine





Define class otrsp as custom

dest = ''
trsp = ''
lrno = ''
lrdt = ctod('')

function init()

return


enddefine





