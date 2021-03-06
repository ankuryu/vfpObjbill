* File name  objbill.prg 
* Uses the following screen files FRMTUP.SCX  & FRMITM.SCX
* uses the following Databases : 
* Uses the following Report Files : 
* By : Sunil Gandhi
* As On : 22/07/2017 0:30 am ist

*******************
* Global settings
******************

clear 
clea window all
clear all
set excl off
set date brit



*set alter to tmp.txt
*set alter on
*set textmerge on
set path to \\samba24\pmc\pm\cm

*if file("itms.dbf")
*  use itms excl
*  zap
*  use
*  sele 0
*endif

create cursor itms( bno c(6),sr c(2), type c(3), qty n(9,2),unt c(5), rate n(9,2), ;
              dsc n(6,2) , tax n(6,2), amt n(10,2), iki c(8), desc m, hsn c(15) )

set procedure to objmcs.prg             && Class definitions related to mcs data whereas omcs is define here.
set procedure to objparty.prg additive  &&  Class definitions of objparty

m.frmtup=NULL
m.frmitm=NULL
m.frmopr = NULL

*  Party container object initiated
*kntp1 = createobject("kPrty")
*kntp1.fname = "tmpq3.dbf"  && file name ofthe party containingthe objects

* mcs object and caontainer
*omcs1 = createobject("omcs")
*kmcs1 = createobject("kmcs",omcs1)

*

m.pmbknt = createobject("kntBill")
with m.pmbknt
 .co = 'PM'   && set the co code
 .yr = '15'   && et the yr code
 .bk = '2'    && set the book code 
 .set_cyb()
endwith


do form frmtup name m.frmtup noshow 
do form frmitm name m.frmitm noshow && with pmbknt.kurbil.itmLst
sele 0
do form frmopr name m.frmopr noshow  && main operation form for select co,yr bk, & add/edit/print etc


m.frmtup.bkobj = m.pmbknt   && set the refrence to the obj
m.frmitm.bkobj = m.pmbknt   && set the refrence to the obj

m.frmopr.oblkont = m.pmbknt && 
*susp
m.frmopr.obill = m.pmbknt.kurbil

m.frmitm.oitmkont = m.frmitm.bkobj.kurbil.itmlst
m.frmitm.oitm =  m.frmitm.bkobj.kurbil.itmlst.oitm
m.pmbknt.rdfrm = m.frmtup
m.pmbknt.kurbil.itmlst.rdfrm = m.frmitm

*=sw_win("bill","itm")
*wait wind
*=sw_win("itm","bill")
*wait wind


clear
 m.frmopr.show()
 m.frmopr.cboCo.setfocus()
 read event
 close data
 Clea windows all
clea all
 wait window "Over"
 ********************************************************************************** 
return 

***** Start of functions ***************************


**************************************
function sw_win(shw,hid)  && function to show hide windows
**************************************
 *wait window "Switching windows"
 if shw = "bill" 
   =frmitm.hide()
   =frmtup.show()
  
 else
   sele itms
   go top
   =frmtup.hide()
   =frmitm.show()
  
 endif
 return
**************************************
function prn_1bn(m.blno,cn,kobj)
**************************************
&& used to print a Bill
* cn is the copy number ranging from 1 to 3
*set step on
cn = iif(!between(cn,1,3),1,cn)
Dime arc[3]
arc[1] = "Original for Buyer"
arc[2] = "Triplicate for Seller"
arc[3] = "Duplicate for Transporter"

   Priv m.Inv_copy,m.curform,m.certify,m.ftype,m.co_name,m.vat_no,m.vgst,m.f_type,m.fname_t,m.fname_i,m.bno

   m.Inv_copy = arc[cn]
   debugo m.Inv_copy
   m.curform = 'vcmgstsql_pln1' && this is the current form
   clear

   *m.certify = "I/We hereby Certify that my/our registration Certificate under the Maharashtra Value Added Tax Act 2002 is in force "
   *m.certify = m.certify +  "on the date on which the sale of the goods specified in this Tax Invoice is made by me/us and that and that the transaction of Sale covered by this Tax Invoice has been effected by me/us and it shall be accounted for in the turnover of sales while"
   *m.certify = m.certify +   "filing of return and th due tax if any, payable on the sale has been paid or shall be paid."
   
   m.certify = "I/We hererby declare that this invoice shows the actual price of the goods described "
   m.certify = m.certify + "and that all particulars are true and correct."
   m.ftype = "Cash Memo"

   m.co_name = iif(m.co = 'PM',"Panalal Mohanlal & Co",'Kamlesh Corporation')
   m.vat_no = iif(m.co = 'PM','27AAAFP1396C1ZG','27AAAFK1972D1ZL')
   m.vgst = m.vat_no  && vendor's gst number
   m.f_type = iif(m.bkn = '1',"Cash Memo (Tax Invoice)",'Tax Invoice')

   m.f_type = iif(m.bkn > '2','Delivery Challan',m.f_type)

   m.fname_t = kobj.fname
   m.fname_i =  kobj.kurbil.itmlst.fname

Private bno,m.bnlo
private artop
m.bno = m.blno
select * from (m.fname_t)as tup where tup.f1no = m.bno into array artop  && this shall contain the party details


if _tally # 1 
  return -1  && error
endif

m.txform = artop[7]       && Get the form
m.o_dtl = artop[10]        &&  get the order details
m.d_dtl = artop[11]       && get the Despatch Details
m.bgst =  artop[15]        && get the buyer's gst number
m.stcd = left(m.bgst,2)    && get the state code
if file("stmaster.dbf")
 select sname,sabbr from stmaster where scode == m.stcd into array s1
 if _tally = 1
   m.stname  = s1[1,1]
   m.sabr = s1[1,2]
 else 
   m.stname = ""
   m.sabr= ""
 endif
else
  Wait window  "State Master file not there" 
  m.stname = ''
  m.sabr =  ''
endif

if !empty(bgst)
 igstflg = !(left(bgst,2) == left(vgst,2))             &&  Flag for  IGST / SCGS+CGST
                                                       && if both GST numbers from same state IGSTFLG false else true
  store '' to m.ord_dt1,m.ord_dt2
else
 igstflg = .f.                                          && Unregistered dealer requires sgst/cgst
endif
                                             
if memline(m.o_dtl) > 0
  m.ord_dt1 = mline(m.o_dtl,1)
  m.ord_dt2 = mline(m.o_dtl,2)
endif

Private m.dst,m.trspt,m.lrno,m.lr_dt,m.d_dtl

store '' to m.dst,m.trspt,m.lrno,m.lr_dt

if memline(m.d_dtl) > 0
 m.dst = mline(m.d_dtl,1)
 m.trspt = mline(m.d_dtl,2)
 m.lrno = mline(m.d_dtl,3)
 m.lr_dt = mline(m.d_dtl,4)
 
endif
* Now get the bill dtails

select * from (m.fname_i) as itm where itm.bno = m.bno into cursor billtmp order by bno,sr

select dsc as rt,sum(rate*qty) as amt, sum(rate*qty*(dsc/100)) from billtmp where billtmp.bno=m.bno into array dsc_ar group by dsc

select tax as rt,sum(rate*qty*(1+dsc/100)) as amt,sum(rate*qty*(1+dsc/100)*(tax/100)) as tx ;
   from (m.fname_i) as itm where itm.bno = m.bno into array tax_ar group by tax
select tax as rt,sum(amt) as amt , sum(amt*(tax/100)) as tx  from (m.fname_i) as itm where itm.bno = m.bno ;
   .and. (rate*qty = 0 .and. tax > 0) into array tax_ar2 group by tax
       
* Now calculate the various totals / and other figures
m.gttl = 0 
select sum(billtmp.amt) from billtmp into array tmpar
if type("tmpar") = "U"
  Dime tmpar[1]
  tmpar[1] = 0
endif
if type("dsc_ar")= "U"
   Dime dsc_ar[1,3]
   store 0 to dsc_ar[1,1],dsc_ar[1,2],dsc_ar[1,3]
endif
if type("tax_ar")= "U"
  Dime tax_ar[1,3]
  store 0 to tax_ar[1,1],tax_ar[1,2],tax_ar[1,3]
endif
if type("tax_ar2") = "U"
  Dime tax_ar2[1,3]
  store 0 to tax_ar2[1,1],tax_ar2[1,2],tax_ar2[1,3]
endif  
m.gttl = m.gttl+ tmpar[1]
store 0 to m.discable ,m.DiscRs
For n = 1 to alen(dsc_ar,1)
  m.discable = m.discable + dsc_ar[n,2]
  m.discRs = m.DiscRs + dsc_ar[n,3]
next n
 m.DiscRs = round(m.DiscRs,2)
store 0 to m.Taxable ,m.TaxRs
For n = 1 to alen(tax_ar,1)
  m.taxable = m.taxable + tax_ar[n,2]
  m.TaxRs = m.TaxRs + tax_ar[n,3]
next n
if type('tax_ar2') # 'U'
  For n = 1 to alen(tax_ar2,1)
    m.taxable = m.taxable + tax_ar2[n,2]
    m.TaxRs = m.TaxRs + tax_ar2[n,3]
  Next n
endif && undefined tax_ar2  
  m.TaxRs = round(m.TaxRs,0)
*m.gttl = m.gttl + m.DiscRs + m.TaxRs
*m.roff = round(m.gttl,0) - m.gttl
*m.gttl= round(m.gttl,0)


 m.taxable1 = tran(m.taxable,'#######')
 m.taxable2 = m.taxable1
 m.tx2hdg = ""
if igstflg
* set the headging, rate, tax amt for igst
  m.tx1hdg = "IGST @ " 
  m.taxable2 = 0
  m.tax2Rs = ""
  m.tax1Rs = tran(m.taxRs, '######')
  store "" to tx2hdg
else
  m.tx1hdg = "SGST @ "
  m.tx2hdg = "CGST @ "
  m.tax1Rs  = tran( round(m.taxRs/2,0), "########")  
  m.tax2Rs = m.tax1Rs
endif
m.gttl = m.gttl + m.DiscRs + val(m.Tax1Rs) + val(m.tax2Rs)
m.roff = round(m.gttl,0) - m.gttl
m.gttl= round(m.gttl,0)

sele billtmp
*susp
repo form &curform  preview


****************************************************   
* Start of Printing Related Functions
****************************************************
**************************************
Function amt_xwrd
**************************************
Para pp
Priv vx,rupi,paise
rupi = int(pp)
paise = pp-rupi
paiw = iif(paise > 0,' and Paise '+x_wrd(paise*100),'')
vx = 'Rupees '+x_wrd(pp) + paiw +  ' Only.'
return vx
**************************************
Function x_wrd
**************************************
* Function to convert figures to Words.
Para pv
Dime f[20],f2[9]
Priv m.amt
f[1] = 'One '
f[2] = 'Two '
f[3] = 'Three '
f[4] = 'Four '
f[5] = 'Five '
f[6] = 'Six '
f[7] = 'Seven '
f[8] = 'Eight '
f[9] = 'Nine '
f[10] = 'Ten '
f[11] = 'Eleven '
f[12] = 'Twelve '
f[13] = 'Thirteen '
f[14] = 'Fourteen '
f[15] = 'Fifteen '
f[16] = 'Sixteen '
f[17] = 'Seventeen '
f[18] = 'Eighteen '
f[19] = 'Nineteen '

f2[1] = 'Ten '
f2[2] = 'Twenty '
f2[3] = 'Thirty '
f2[4] = 'Forty '
f2[5] = 'Fifty '
f2[6] = 'Sixty '
f2[7] = 'Seventy '
f2[8] = 'Eighty '
f2[9] = 'Ninety '


 m.amt = pv
 return convt(m.amt)

**************************************
Function convt
**************************************
Para bal
* Check for Crore
Priv Rs
Rs = ''
bal=chkit(bal,1e7,'Crore ')
bal=chkit(bal,1e5,'Lakh ')
bal=chkit(bal,1e3,'Thousand ')
bal=chkit(bal,1e2,'Hundred ')
bal=chkit(bal,1,' ')
return Rs

**************************************
Function chkit
**************************************
Para b,d,s
priv ps,v,rv
ps = s
v = int(b/d)
rv = mod(b,d)
if v > 0
  if v < 20
    ps = f[v]+ ps
  else
    ps = f2[int(v/10)]+ iif(mod(v,10) # 0,f[mod(v,10)],'') + ps
  endif  
  Rs = Rs + ps
endif  
return rv
    



**************** End of functions ***********************************************************************************

********************  End of   TESTING MAIN LOOP **************



****************** Start of Definition of Classes **************

***********************************************

*    K N T B I L L   C L A S S 

***********************************************
Define class kntBill  as custom && Defines the container obj for Bills
  kurbil = NULL  && current bill
  ttlbil = 0     && total bill in kont
  lstbno = ""    &&  Last bill no.
  co = ""        &&  Copmany Code 
  yr = ""        &&  Year Code
  bk = ""        &&  Book Code
  fname = ""      && contains the dbf file containing bills
  rdfrm = NULL   &&  Form for reading in data
  
  function init()
   this.kurbil = createobject("Bill",this)
   this.kurbil.kntbill = this  && Points to the parent container object
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
  
  ****************************** 
  * FOR DEBUG PURPOSE PLEASE REMOVE LATER
  *
  *m.co = "PM"
  *m.yr = "17"
  *m.bk = "2"
  
  * FOR DEBUG PURPOSE PLEASE REMOVE LATER
  *****************************
  
  
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
   msg  = msg + iif(file(m.vfname), "", m.vfname + ' ')        && not available
   if len(msg) > 0                                             && if msg length > 0 , file absent
     rv = .f.
     msg  = "Files " + msg + "Not Found"                       && show msg accordingly
     wait window msg
   endif
  return rv
  
  

  function addBill()  && Adds a new Bill to container
  
 *  this.kurbil = createobject('bill')
   this.set_cyb()
   this.kurbil.clnit()
   this.kurbil.bno = this.nx_bill()
   this.kurbil.bdt = date()
   this.kurbil.f2no = this.kurbil.bno
   this.kurbil.f2dt = date()
   this.kurbil.kntbill = this
   =iif(this.rdBill() .and. .f.,this.putBill(this.kurbil.bno),"")
  return
   
   
  function ediBill(pbno)  && Edits the current Bill pbno
   priv v
   this.getBill(pbno)      && Get the bill no pbno
   v = this.rdBill()
   *=iif(v,this.putBill(pbno),"")
   return 



  function delBill(pbno)  && Deletes the current Bill pbno



  function prnBill(pbno,nc) && Prints the Bill pbno
   this.getBill(pbno)
   with this
     m.co = .co
     m.bk = .bk
     m.yr = .yr
     m.bkn = m.bk
   endwith
   
   *susp
   Priv m.Inv_copy,m.curform,m.certify,m.ftype,m.co_name,m.vat_no,m.vgst,m.f_type,m.fname_t,m.fname_i,m.bno

   m.Inv_copy = "Original for Buyer"
   m.curform = 'vcmgstsql_pln1' && this is the current form
   clear

   m.certify = "I/We hereby Certify that my/our registration Certificate under the Maharashtra Value Added Tax Act 2002 is in force "
   m.certify = m.certify +  "on the date on which the sale of the goods specified in this Tax Invoice is made by me/us and that and that the transaction of Sale covered by this Tax Invoice has been effected by me/us and it shall be accounted for in the turnover of sales while"
   m.certify = m.certify +   "filing of return and th due tax if any, payable on the sale has been paid or shall be paid."
   m.ftype = "Cash Memo"

   m.co_name = iif(m.co = 'PM',"Panalal Mohanlal & Co",'Kamlesh Corporation')
   m.vat_no = iif(m.co = 'PM','27AAAFP1396C1ZG','27AAAFK1972D1ZL')
   m.vgst = m.vat_no  && vendor's gst number
   m.f_type = iif(m.bkn = '1',"Cash Memo (Tax Invoice)",'Tax Invoice')

   m.f_type = iif(m.bkn > '2','Delivery Challan',m.f_type)

   m.fname_t = this.fname
   m.fname_i =  this.kurbil.itmlst.fname
   m.bno = this.kurbil.bno
   For jj = 1 to nc
     =prn_1bn(m.bno,jj,this) && send the bno,no of copies & object
   Endfor  
   m.frmopr.show()
  return

  function putBill(pbno)  && Puts the current bill on storage pbno
  wait window "Putting Bill" nowait
  * check if bno exists ?  if so  use sql  update else sql insert
  select f1no from (this.fname)  where f1no = m.pbno into array arbno
  * set step on
 
  if type('arbno[1]')= 'C'
     * update
     *with this.kurbil
     * m.f1dt = .bdt
     * m.f2no = .f2no
     * m.f2dt = .f2dt
     * m.pcode = .pcode
     * m.form = .frm
     * m.pname = .pname
     * m.p_addr = .p_addr
     * m.cr = .cr
     * m.gst = .gst
     *endwith 
      

      update (this.fname)  set f1dt = m.f1dt,;
                               f2no = m.f2no, ;
                               f2dt = m.f2dt , ;
                               pcode= m.pcode, ;
                               pname = m.pname, ;
                               p_addr = m.p_addr, ;
                               cr = m.cr ,;
                               gst = m.gst ;
                               where f1no  = m.pbno
                               
    
   else
      with this
        .kurbil.gst = m.gst
        .kurbil.cr = m.cr
              
      insert into(this.fname)  (f1no,f1dt,f2no, f2dt,pcode,form,pname,p_addr,cr,gst)   values (m.pbno,.kurbil.bdt,.kurbil.f2no,.kurbil.f2dt,.kurbil.pcode,.kurbil.frm,.kurbil.pname,.kurbil.p_addr,.kurbil.cr,.kurbil.gst)  
      endwith 
  endif
  with this.kurbil
       * Now update the Order details
   .oOrd.ordno = m.ordno
   .oOrd.orddt = m.orddt        
   .oOrd.put_obj()
       * Also update the Trspt details
   .oDlv.trsp = m.trspt
   .oDlv.Dest = m.dest
   .oDlv.LrNo = m.lrno
   .oDlv.lrDt = m.lrdt     
   .oDlv.put_obj()
  
  * now  get the total items of the bill in file and acoordingly use upd/ins sql
   .itmlst.put_itms() && put the items into the item container file
  endwith

return 

  
function getBill(pbno)  && Gets the bill from storage into curbill obj
  Private m.bno, obno
* set step on
  m.obno = this.kurbil  && store the Bill object temporarily in the memory.object (m.obno)
  m.bno = m.obno.bno    && get the bno from the object

*  select * from (this.fname) where f1no = m.bno into array ar1   && get the details of the object from the file using (this.fname) into array ar1
   select * from (this.fname) where f1no = m.pbno into array ar1   && get the details of the object from the file using (this.fname) into array ar1
   
   
obno.bno = ar1[1]    && Bill No
obno.bdt = ar1[2]   && Bill Date
obno.f2no = ar1[3]   && Challan No
obno.f2dt = ar1[4]  && Challan date
obno.pcode =  ar1[5]      && party Code
obno.famt = ar1[6]         && Form Amount
obno.frm = ar1[7]         && Form if any
obno.pname = ar1[8]       && Party Name
obno.p_addr = ar1[9]      && Party Addres
obno.cr = ar1[12]           && Credit Days
obno.co = ar1[13]          && Company
obno.gst = ar1[15]         && get the gst after skipping ord/lr/itxt
obno.pobj =  NULL     && Party object

* Now get the order details
       * Now update the Order details
       with obno.oOrd
         .get_obj()
         m.ordno = .ordno
         m.orddt = .orddt       
       endwith  
       * Also update the Trspt details
       with obno.oDlv
          .get_obj()
          m.trspt= .trsp  
          m.dest = .Dest  
          m.lrno = .LrNo  
          m.lrdt = .lrDt     
         .put_obj()
       endwith  
  
* Now get the Transport details

* Now get the items
obno.itmlst.bno = ar1[1]     && set the billno
m.bno = ar1[1]
obno.itmlst.get_itms(m.bno)  && get the items into the items list


return


function rdBill
Priv rv
Priv m.f1no,m.f1dt, m.f2no, m.f2dt, m.pcode, m.pname , m.p_addr
Priv m.cr,m.gst
Priv m.ordno,m.orddt,m.trspt,m.lrno,m.lrdt,m.dest

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
 m.gst = .gst
 m.ordno = .oOrd.ordno
 m.orddt = .oOrd.orddt
 m.trspt = .oDlv.trsp
 m.lrno =  .oDlv.lrno
 m.lrdt =  .oDlv.lrdt
 m.dest =  .oDlv.dest
 

endwith
* set step on
xval= this.rdfrm.show(1)

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
   .gst = m.gst
   * now process the order details
   .oOrd.ordno =  m.ordno
   .oOrd.orddt =  m.orddt
   .oDlv.trsp = m.trspt
   .oDlv.lrno = m.lrno
   .oDlv.lrdt = m.lrdt
   .oDlv.dest = m.dest
   rv = .t.
  endwith
endif

assert ("rdBill Exiting with " + iif(rv,'True','False'))
return rv


function prn_bill


return






function nul_one()
***********************************************

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
gst = ""         && GST
pobj =  NULL     && Party object
kprty = NULL     && Container object of party
itmlst = NULL    && Knt obj of items 
itmknt = 0       && Kount of ttl items 
kntbill = NULL   && Kontainer object of bills



* rdfrm = NULL  && Form to read it


function init()
lpara m.pkntbill
 wait windo "Initiating...Bill Object" nowait

 this.itmlst = createobject("kntItm",this)
 this.oOrd = createobject("oOrd")                 && initiate the Orderobj
 this.oDlv = createobject("oTrsp")                && initiate the trsp object
 this.oOrd.obill = this                           && store the pointer to the paretn obill
 this.oDlv.obill = this
 this.kntbill = m.pkntbill                        && point to the calling container
 
 this.kprty = createobject("kPrty")               && create the Party container obj
 this.kprty.fname = "tmpq4.dbf"                   && set the file name of the party dbf
 this.pobj = this.kprty.oPrty                     && set the party object on the bill
 

  function clnit
   with this
    .bno = spac(6)
    .bdt = date()
    .f2no = .bno
    .f2dt = .bdt
    .pcode = ""
    .famt = 0 
    .frm  = "NA"
    .pname = ""
    .p_addr = ""
    .cr = 0
    .co = ""
    .gst = ""
    .itmlst.clnit()
   endwith
  return

function gt_prty(srch_str)
* gets the party using the objparty party obje
  this.kprty.frmbrw.prvfrm = _screen.activeform
  this.kPrty.brw_obj(srch_str)
  m.pmsg = this.KPrty.oPrty.pname
  wait window m.pmsg
  
*  this.kprty.frmbrw.prvform.show
  
  
  
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
oBill  = NULL                 && points to the objbill


function init(m.pbobj)
 wait window "Creating item Container..."
 this.oBill = m.pbobj 
 this.oItm = createobject("itm",this)
 this.oitm.kntitm = this       &&  point to the container of  this item
 this.oitm.omcs.obill = this.obill         && set the pointer to obill
 this.oitm.omcs.fname =  this.ikifname && set the file name for further operations
 this.oItm.bno = this.bno
 this.itmKnt = 0
 
 
return

function clnit
 with this
  Dime lstItm[1] = NULL
  .itmknt = 0
  .bno = '000000'
  .oItm.clnit()
 endwith 
return


function addItm()
Priv i,m.sr
 this.itmknt = this.itmknt + 1
 i = this.itmknt
 m.sr = tran(i,'@l ##')
 Dime this.lstItm[i]
 this.cursr = i 
 this.lstItm[i] = createobject('itm',this)
 this.oItm = this.lstItm[i]
 this.oItm.kntItm = this
 this.oitm.omcs.fname = this.oitm.kntItm.ikifname
*  set step on
 this.bno = this.obill.bno
 this.oItm.bno = this.bno
 this.oItm.sr = m.sr
 wait window "Bill no : " + this.bno
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
 wait window "Editing ... Item Sr " + tran(itms.sr,'@l ##')  && <- Data Mismatch / Corrected
 if val(itms.sr) > 0
   this.oItm = this.lstItm[val(itms.sr)]
   this.rdfrm.oitm = this.oitm
   prvtyp = this.oitm.typ                       && keep a record of the type for removing/updating of INV item
   debugo "Moving to Rditm"
   this.rdItm()
   debugo  "back from Rditm"
 endif  
   
 return
 
function delItm()

function put_itms()
 wait window "putting items into the file... " nowait
 * Exit if there are no items to add
 if this.ItmKnt > 0
   
 
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
endif

return

function upd_itm(opr,st,ed)       &&  operation/ start / end
 Priv n,ot,tmp1,tmp2
 priv tmpiki
 store  left(this.fname,at(".dbf",this.fname)) to tmp1,tmp2
 tmp1 = tmp1+ "desc"
 tmp2 = tmp2 + "type"

* set step on
   for n = st to ed
     ot = this.lstItm[n]
     with ot
      ot.bno = this.obill.bno  && set the billno for all items
      * If inv item update  inv file
      if ot.typ =="INV"
       * Upate/insert the inv file
       wait window "Updating Inventory..."
     * set step on  &&  to check why new records are being added for same itme when edited
     debugo "iki " + ot.iki + "Before entering"
       
       ot.iki = ot.omcs.put_iki(ot.iki)
      endif
     m.sr = tran(ot.sr, '@l ##')
     Do Case  
     Case opr = "ins"      
       Insert into (this.fname) (bno,sr,type,qty,unt,rate,dsc,tax,amt,iki,desc,hsn ) ;
        values(.bno,m.sr,.typ,.qty,.unt,.rate,.dsc,.tax,.amt,.iki,.descr,.hsn)
        * now if type is inv insert into inv file 
        *ot.omcs.put_iki(m.tmpiki)
      Case opr = "upd"         
        UPDATE (this.fname) set bno= ot.bno, sr = m.sr , &tmp2 = ot.typ , qty = ot.qty ,;
                                unt = ot.unt, rate = ot.rate , dsc = ot.dsc , tax = ot.tax , ;
                                hsn = ot.hsn ,amt = ot.amt,;
                                iki = ot.iki , &tmp1 = ot.descr ;
                                where bno == ot.bno .and. sr == m.sr                                   
      Case opr = "rmv"
        
      Endcase
     endwith
    Next n
  
 return


function get_itms(fno)   && gets items for a particular form

select * from (this.fname) where bno = m.fno into array arz
sele itms
zap
if _tally > 0
  insert into itms from array arz
  this.ItmKnt = _tally
  Dime this.lstItm[this.itmKnt]
  sele itms
  go top
  For n = 1 to this.itmKnt
    this.cursr = n
    scatter memvar memo
    this.lstItm[n] = createobject('itm',this)
    v = this.lstitm[n]
    v.omcs.fname = this.ikifname
        v.typ = m.type           && Type
    v.qty = m.qty            && Qty
    v.unt = m.unt
    v.rate = m.rate           && 
    v.dsc = m.dsc            &&
    v.tax = m.tax            &&
    v.amt = m.amt            &&
    v.iki = m.iki           &&
    v.descr = m.desc         &&
    v.hsn = m.hsn
  *  set step on
    if !v.iki == "" .and. v.typ = "INV"
      v.omcs.get_iki(v.iki)  && get the iki obj populated
    endif      
    select itms              && Now transfer the data to read/write cursor
    skip 
*    insert into itms from memvar
  Next n
else
   Dime this.lstItm[1]       && Doesn't have items.
endif


function nul_one
* select * from (this.fname) where bno = m.fno into table itms 		
this.Itmknt = reccount("itms")

if this.Itmknt > 0
  Dime this.lstItm[this.itmKnt]
  select itms
  go top
  For n = 1 to this.itmKnt
    this.cursr = n                  && set the curent serial no for the item
    scatter memvar memo
    this.lstItm[n] = createobject('itm',this)
    v = this.lstItm[n]
    v.omcs.fname = this.ikifname
    v.typ = m.type           && Type
    v.qty = m.qty            && Qty
    v.unt = m.unt
    v.rate = m.rate           && 
    v.dsc = m.dsc            &&
    v.tax = m.tax            &&
    v.amt = m.amt            &&
    v.iki = m.iki           &&
    v.descr = m.desc         &&
    v.hsn = m.hsn
  *  set step on
    if !v.iki == "" .and. v.typ = "INV"
      v.omcs.get_iki(v.iki)  && get the iki obj populated
    endif  
    
    select itms              && Now transfer the data to read/write cursor
    skip
  Next n
 else
   Dime this.lstItm[1]       && Doesn't have items.
 endif 
return  &&&   Function nul_two


function rdItm()
 this.rdfrm.oitmkont = this

 
 this.rdfrm.show()            && Make it a Modal form
 debugo  "Now Scrolling" 
 this.rdfrm.grditm.doscroll(0) && scroll up 2 = page 0 = line  1 = scroll down line 3 = scroll down page
 debugo  "Now setting focus on cmbtyp"
 this.rdfrm.cmbTyp.setfocus()
 debugo  "Now returning back to Edi itm"
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
hsn = ""           &&  hsn code for GST
kntitm = NULL      &&  pointer to the container of items
omcs = NULL        &&  invobj
kmcs = NULL        &&  Kontainer obj for mcs, contains also mcsd used for retrieving descripition /prices etc
tcurs = "itms"         &&  temp cursor name for retrieving and saving item data

function init(pkntItm)
 this.kntItm = m.pkntItm 
 wait window "Creating item ..."
 
 this.sr =  this.kntitm.cursr                             &&spac(m.sr)
 this.omcs = createobject("omcs",this)
 this.kmcs = createobject("kmcs",this.omcs)   && 
 return
 
Function clnit
 Para bno
 with this
  .bno = iif( type("bno") = "U","",bno)
  .sr = "00"
  .typ = "ITM"
  .qty = 0 
  .unt = "NOS"
  .rate = 0
  .dsc = 0
  .tax = 13.5
  .amt = 0
  .descr = ""
  .iki = ''
  .hsn = ""
 endwith 
return


function get_itm2c(m.sr)
 wait window "Ouch get_Itm2c(m.sr)"
return
 priv a
 select * from  (this.tcurs) where itms.sr == m.sr into cursor juju
  scatter memvar memo
  use juju

function put_itm2c(m.sr)
priv fn2
* set step on
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
  m.hsn  = .hsn
endwith
 wait window "put_itm2c(m.sr)"
 select sum(1) from (this.tcurs) into array ar_sr where sr = m.sr &&   <-Data mismatch /hopefull corrected, param m.sr comes as string now
if _tally = 0
 insert into (this.tcurs) (sr,type,qty,unt,rate,dsc,tax,amt,desc,hsn ) ;
 values (m.sr,m.type,m.qty,m.unt,m.rate,m.dsc,m.tax,m.amt,m.desc,m.hsn)
else 
 fn2 = this.tcurs
 update (fn2) set itms.type=m.type, ;
                  itms.qty=m.qty,   ;
                  itms.unt=m.unt,   ;
                  itms.rate=m.rate, ;
                  itms.dsc = m.dsc, ;
                  itms.tax = m.tax, ;
                  itms.amt = m.amt ,;
                  itms.desc = m.desc, ;
                  itms.hsn = m.hsn ;
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
 iki =  repl('0',8) && iki for relating to invoice/inv

 oitm = NULL        && ref to the itm object 
 fname = ""         && file name of inventory file
 frmmcs = ""        && Form to read the mcs
 obill = NULL       &&  will contain the parent bill object which cotains this obj
 kmcs = NULL        && Container object of mcs which needed for getting other data
 

function init( poitm)
 this.kmcs = createobject("kmcs",this)  && sent this mcs object as para
 this.oitm = m.poitm
 this.obill = this.oitm.kntitm.oBill
return


function nxt_iki()  &&  Gets the next iki sequentially
  Priv ar1
*   set step on
  select max(iki) from (this.fname) where iki # '99999999' into array ar1
  if type('ar1[1]') # 'C'
     rv ='00000001'
  else
     rv = tran(val(ar1[1])+1,'@l ########')
  endif
return rv


 function get_iki(m.iki)  && gets the record from file based on iki into obj
 Priv ar1,rv 
 rv = .f.
 select recno(),mfg,icode,asize from (this.fname) where iki == m.iki into array ar1
 * set step on
 if !(type('ar1[1]') = 'U')
   with this
     .rcno = ar1[1]
     .mfg = ar1[2]
     .icode = ar1[3]
     .asize = ar1[4]
     .iki = m.iki
   endwith  
   rv = .t.
  endif
  return rv
   

 return
 
 
 
 function put_iki(m.iki)
   Priv ar,m.iki
   debugo " in Put_iki " + m.iki
   *set step on
   if Empty(m.iki) && If there are no iki get a fresh iki and add the entry to file
     * get the next iki
     m.iki = this.nxt_iki()  && get fresh iki 
     m.iki = this.add_iki(m.iki)  && add it to file
   else       
     m.bobj = this.obill && tmp save the bill obj
     m.iobj = this.oitm  && also save to itm obje
     * check if Iki exists in the file ?
      select iki from (this.fname) where iki == m.iki into array ar
     if _tally = 1
       debugo  "updating "+ m.iki + "in put_iki" && update the iki if it already exists in file
       update (this.fname) set b_no = m.bobj.bno,;   
                         b_dt = m.bobj.bdt,       ;
                         Dlv_no = m.bobj.f2no,    ;
                         Dt =  m.bobj.f2dt,       ;
                         mfg  = this.mfg,       ;
                         icode = this.icode,    ;
                         asize = this.asize,    ;
                         qty  = m.iobj.qty,       ;
                         rate=m.iobj.rate,        ;
                         dsc = m.iobj.dsc,        ;
                         tax = m.iobj.tax        ;
                         where  Iki == m.iki
     else
       debugo "Inserting " + iki + " in put_iki"
       m.iki = this.add_iki(iki)  && else you need to add this new iki and store this iki in itm
     endif  && _tally = 1
   endif  && enpty iki  
 return m.iki   && return iki value to store in item


 function add_iki(m.iki) && this just adds the iki entry into the file
  priv b_no,m.b_dt,Dlv_no,Dt,mfg,icode,asize,qty,rate,dsc,tax
  with this.obill
   m.b_no = .bno
   m.b_dt = .bdt
   m.dlv_dt = .f2dt
   m.Dlv_no = .f2no
  endwith
  with this.oitm
    m.mfg = this.mfg
    m.icode = this.icode
    m.asize = this.asize
    m.qty = .qty
    m.rate = .rate
    m.dsc = .dsc
    m.tax = .tax
*    m.iki  = .iki
  endwith 
  insert into (this.fname) (B_no,B_dt,mfg,icode,asize,qty,rate,dsc,tax,dlv_no,dlv_dt,dt,iki) ;
               values (m.b_no,m.b_dt,m.mfg,m.icode,m.asize,m.qty,m.rate,m.dsc,m.tax,      ;
               m.dlv_no,m.dlv_dt,m.dlv_dt,m.iki )
 return m.iki


 function del_iki(iki)  && this removes the iki entry by making it '999999' and nullifying vals
                        && help of the recno is taken for this.
  update (this.fname) set b_no = '' ,;
                      b_dt = '' ,    ;
                      Dlv_no = '',   ;
                      Dt =  "",      ;
                      mfg  = "",     ;
                      icode = "",    ;
                      asize = "",    ;
                      qty  = 0,      ;
                      rate= 0,       ;
                      dsc = 0 ,      ;
                      tax = 0,       ;
                      iki = '99999999' ;
                      where  recno() = this.rcno 
 return



 
 
 function rd_mcs()
   
 return 
 
 
 function get_desc
 Priv rv
 return rv  && returns   Desc in rv
 
 function get_pri
 Priv rv
 
 return rv   && returns price in rv
 

Enddefine

*********************************************
* removed below definition as the full definition is in  objparty.prg set as procedure

*Define class oPrty  as custom
*pcode = ''         && party code
*pname = ''         && Party Name
*p_addr = ''        && Party Address
*mvat = ''          && Mvat Number
*pan = ''           && Pan Number of Party
*fname = NULL       &&  File containing the Details of 
*obill = NULL       &&  will contain the parent bill object which cotains this obj
*enddefine



Define class  oord as custom 
* First line in memo is Ordno and 2nd line is Orddt 

ordno = ''                 && first line in memor
orddt = ctod('')           && second line in memo
obill = NULL       &&  will contain the parent bill object which cotains this obj

function ini()

return

function clnit
* clean up for new data
with this
 .ordno = ''
 .orddt = ctod("")
endwith
return

function put_obj  && puts the object into the respective container fields
 wait window "Putting the  Order Details"

 Priv js,fname,bno,cr1
 
 cr1 = chr(13)+chr(10)
 m.bno = this.oBill.bno
 m.fname = this.oBill.kntBill.fname  && get the file name

 js = this.ordno +  cr1 + dtoc(this.orddt)
 update (m.fname) set o_det = js  where f1no = m.bno
 
 ************************************************

function get_obj  && gets the object from the respective container fields
wait window "Getting the Order Details"

 Priv ar1 ,m.bno,m.fname && array to hold resulsts
 m.bno = this.oBill.bno
 m.fname = this.oBill.kntBill.fname  && get the file name
 select o_det from (m.fname) where f1no == m.bno into array ar1
 if type('ar1') # 'U'
* set step on
   this.ordno = mline(ar1[1,1],1)  && get the dest
   this.orddt = ctod(mline(ar1[1,1],2)) && get the lr date
 endif  

Enddefine





Define class otrsp as custom

dest = ''   &&  1st line
trsp = ''   &&  2nd Line in memo
lrno = ''   &&  3rd line in memo
lrdt = ctod('')  && 4th line in memo
obill = NULL       &&  will contain the parent bill object which cotains this obj

function init()

return

function clnit
 with this
   .dest = ''
   .trsp = ''
   .lrno = ''
   .lrdt = date()
 endwith
return


function put_obj  && puts the object into the respective container fields
 wait window "Putting the  Trspt Details"

 Priv js,fname,bno,cr
 m.cr = chr(13)+chr(10)
 m.bno = this.oBill.bno
 m.fname = this.oBill.kntBill.fname  && get the file name

 js = this.dest + m.cr + this.trsp + m.cr + this.lrno + m.cr + dtoc(this.lrdt)
 update (m.fname) set d_det = js  where f1no = m.bno
return
 

function get_obj  && gets the object from the respective container fields
 wait window "Getting the Trspt Details"

 Priv ar1 ,m.bno,m.fname && array to hold resulsts
 m.bno = this.oBill.bno
 m.fname = this.oBill.kntBill.fname  && get the file name
 select d_det from (m.fname) where f1no == m.bno into array ar1
 if type('ar1') # 'U'
   this.dest = mline(ar1[1,1],1)  && get the dest
   this.trsp = mline(ar1(1,1),2) && get the traspt
   this.lrno = mline(ar1(1,1),3) && get the lrno
   this.lrdt = ctod(mline(ar1[1,1],4)) && get the lr date
 endif  

enddefine





