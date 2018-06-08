* File name  objbillv400.prg
* Change.log ->  Work in progress for proforma and quotation as on 27/05/18
* uses Form frmitm2 upgraded to take into account the calculation for quotes
* -------------------------------------
* Change.log ->  incorporating  Class_kgst for  tax calcultation and other
* Uses the following screen files FRMTUP.SCX  & FRMITM.SCX
* uses the following Databases :
* Uses the following Report Files :
* By : Sunil Gandhi
* As On : 8/06/2018 4:30 pm ist
* version 4.00  changed directory struc and have incorporated for proforma / quot
*               Various changes to screens in order to take in details for prf/quo
*               new scree added for tnc of quo
* version 3.04 trying to remove the bugs present


* Following Bugs need to be attended to
* General :   VVIMP   same company/year/book cannot be opened , incorporate locking system

*  Party top related
*  a)Problem with the dates, remove century
*  b)Doesnt save the Party name and addr during the first timee, however
*  c)save new party in master when entering here

*  Items  related

*******************
* Global settings
******************

Clear
Clea window all
Clear all
SET excl off
SET date brit
m.ver = 4.00
* added IEC code in the printout
WAIT window  "Starting version " + tran(m.ver, "@l ##.##")


dd = sys(5)


IF adir(j3,"\\samba24\pmc\pm\cm\*") # 0
  cmpth =  ',\\samba24\pmc\pm\cm'
ELSE

  cmpth =  ',f:\pm\daily.0\cm'
ENDIF
pth = '.,..\data,..\screens,..\repo' + cmpth
set path to (pth)



CREATE cursor itms( bno c(6),sr c(2), type c(3), qty n(9,2),unt c(5), rate n(9,2), ;
  dsc n(6,2) , tax n(6,2), amt n(10,2), iki c(8), desc m, hsn c(15) )

SET procedure to objmcs.prg             && Class definitions related to mcs data whereas omcs is define here.
SET procedure to objparty.prg additive  &&  Class definitions of objparty
SET procedure to Class_kgst.prg  additive

m.frmtup=NULL
m.frmitm=NULL
m.frmopr = NULL


*
m.kgst1 = createobject("kgst")       && Create the Container and its object for GST
m.pmbknt = createobject("kntBill")
WITH m.pmbknt
  .co = 'PM'   && set the co code
  .yr = '18'   && et the yr code
  .bk = '2'    && set the book code
  .set_cyb()
ENDWITH


DO form frmtup name m.frmtup noshow
DO form frmitm2 name m.frmitm noshow && with pmbknt.kurbil.itmLst
SELE 0
DO form frmopr name m.frmopr noshow  && main operation form for select co,yr bk, & add/edit/print etc


m.frmtup.bkobj = m.pmbknt   && set the refrence to the bill cont obj
m.frmitm.bkobj = m.pmbknt   && set the refrence to the bill cont obj

m.frmopr.oblkont = m.pmbknt && set the refrence to the bill cont obj

m.frmopr.obill = m.pmbknt.kurbil  && set the refrence to the bill obj

m.frmitm.oitmkont = m.frmitm.bkobj.kurbil.itmlst && set ref to itm cont obj
m.frmitm.oitm =  m.frmitm.bkobj.kurbil.itmlst.oitm && set ref to itm obj
m.pmbknt.rdfrm = m.frmtup                 && set ref to  bill obj reading form
m.pmbknt.kurbil.itmlst.rdfrm = m.frmitm   && set ref to itm obj reading form 




Clear
m.frmopr.show()     && start the main operation form
m.frmopr.cboCo.setfocus() && set the focus and start of events
READ event
CLOSE data               && exit from the event loop
Clea windows all         && clea all windows & objs
Clea all
WAIT window "Over"
**********************************************************************************
RETURN

***** Start of functions ***************************


**************************************
FUNCTION sw_win(shw,hid)  && function to show hide windows
**************************************

IF shw = "bill"
  =frmitm.hide()
  =frmtup.show()

ELSE
  SELE itms
  GO top
  =frmtup.hide()
  =frmitm.show()

ENDIF
RETURN

************ Global  Functions to print ***************


**************************************
FUNCTION prn_1bn(m.blno,cn,kobj)
**************************************
* used to print a Bill
* cn is the copy number ranging from 1 to 3

cn = iif(!between(cn,1,3),1,cn)
DIME arc[3]
arc[1] = "Original for Buyer"
arc[2] = "Triplicate for Seller"
arc[3] = "Duplicate for Transporter"

PRIV m.Inv_copy,m.curform,m.certify,m.ftype,m.co_name,m.vat_no,m.vgst,m.f_type,m.fname_t,m.fname_i,m.bno

m.Inv_copy = arc[cn]
m.curform = 'vcmgstsql_pln1_2' && this is the current form
Clear

m.certify = "I/We hererby declare that this invoice shows the actual price of the goods described "
m.certify = m.certify + "and that all particulars are true and correct."
m.ftype = "Cash Memo"

m.co_name = iif(m.co = 'PM',"Panalal Mohanlal & Co",'Kamlesh Corporation')
m.vat_no = iif(m.co = 'PM','27AAAFP1396C1ZG','27AAAFK1972D1ZL')
m.vgst = m.vat_no  && vendors gst number
m.iec = iif(m.co = 'PM','AAAFP1396C','')
m.f_type = "Tax Invoice"
m.f_type = iif(m.bkn = '1',"Cash Memo (Tax Invoice)",m.f_type)
m.f_type = iif(m.bkn = '2','Tax Invoice',m.f_type)
m.f_type = iif(m.bkn = '3','Delivery Challan',m.f_type)
m.f_type = iif(m.bkn = '4','Proforma Invoice',m.f_type)
m.f_type = iif(m.bkn = '5','Quotation',m.f_type)

m.fname_t = kobj.fname
m.fname_i =  kobj.kurbil.itmlst.fname

PRIVATE bno,m.bnlo
PRIVATE artop
m.bno = m.blno
SELECT * from (m.fname_t)as tup where tup.f1no = m.bno into array artop  && this shall contain the party details


IF _tally # 1
  RETURN -1  && error
ENDIF

m.txform = artop[7]       && Get the form
m.o_dtl = artop[10]        &&  get the order details
m.d_dtl = artop[11]       && get the Despatch Details
m.bgst =  artop[15]        && //get the buyer's gst number
m.stcd = left(m.bgst,2)    && get the state code
IF file("stmaster.dbf")
  SELECT sname,sabbr from stmaster where scode == m.stcd into array s1
  IF _tally = 1
    m.stname  = s1[1,1]
    m.sabr = s1[1,2]
  ELSE
    m.stname = ""
    m.sabr= ""
  ENDIF
ELSE
  WAIT window  "State Master file not there"
  m.stname = ''
  m.sabr =  ''
ENDIF

IF !empty(bgst)
  igstflg = !(left(bgst,2) == left(vgst,2))             &&  Flag for  IGST / SCGS+CGST
&& if both GST numbers from same state IGSTFLG false else true
  STORE '' to m.ord_dt1,m.ord_dt2
ELSE
  igstflg = .f.                                          && Unregistered dealer requires sgst/cgst
ENDIF

IF memline(m.o_dtl) > 0
  m.ord_dt1 = mline(m.o_dtl,1)
  m.ord_dt2 = mline(m.o_dtl,2)
ENDIF

PRIVATE m.dst,m.trspt,m.lrno,m.lr_dt,m.d_dtl

STORE '' to m.dst,m.trspt,m.lrno,m.lr_dt

IF memline(m.d_dtl) > 0
  m.dst = mline(m.d_dtl,1)
  m.trspt = mline(m.d_dtl,2)
  m.lrno = mline(m.d_dtl,3)
  m.lr_dt = mline(m.d_dtl,4)

ENDIF
m.kgst1 = createobject("kgst")
m.kgst1.bno = m.bno
m.kgst1.gst = m.bgst
m.kgst1.iflg = m.igstflg
m.kgst1.ftup = m.fname_t
m.kgst1.fitm = m.fname_i

m.kgst1.cr_crs()
m.kgst1.calc()
m.gttl = kgst1.gamt &&  Get the gross amount from the object



****************************   Commented out from here

** Now get the bill dtails

SELECT * from (m.fname_i) as itm where itm.bno = m.bno into cursor billtmp order by bno,sr



***********************************  Commended Material ends here

STORE 0 to m.discable ,m.DiscRs
WITH kgst1
*set step on
  FOR n = 1 to .dcnt                 && number of items in the array  of discount
    m.discable = m.discable + .oDed[n].amt
    m.DiscRs = m.DiscRs + .oDed[n].ded
  NEXT n
  m.DiscRs = round(m.DiscRs,2)
  STORE 0 to m.Taxable ,m.TaxRs
  str_tx = ""

  FOR n = 1 to .gcnt             && number of items in the array of tax
    m.Taxable = m.Taxable + .oGst[n].txbl
    m.TaxRs = m.TaxRs +  iif(igstflg,.oGst[n].igst,.oGst[n].sgst)
    str_tx = str_tx + tran(.oGst[n].txrt,'@l ##.##')
  NEXT n

ENDWITH


IF type('tax_ar2') # 'U'
  FOR n = 1 to alen(tax_ar2,1)
    m.Taxable = m.Taxable + tax_ar2[n,2]
    m.TaxRs = m.TaxRs + tax_ar2[n,3] * iif(igstflg,1,0.5)
    m.tmp1 = tran(tax_ar[n,1]* iif(igstflg,1,0.5) ,'@l ##.##')
    str_tx = str_tx + iif(m.tmp1 == str_tx,"","/"+m.tmp1)
  NEXT n
ENDIF && undefined tax_ar2
m.TaxRs = round(m.TaxRs,2)


m.taxable1 = tran(m.Taxable,'@z ######.##')
m.taxable2 = m.taxable1
m.tx2hdg = ""
IF igstflg
* set the headging, rate, tax amt for igst
  m.tx1hdg = "IGST @ "+ str_tx +'%'
  m.taxable2 = 0
  m.tax2Rs = ""
  m.tax1Rs = tran(round(m.TaxRs,0), '@z ######.##')
  STORE "" to tx2hdg
ELSE
  m.tx1hdg = "SGST @ "+ str_tx +'%'
  m.tx2hdg = "CGST @ "+ str_tx +'%'
  m.tax1Rs  = tran( round(m.TaxRs,0), "@z ########.##")
  m.tax2Rs = m.tax1Rs
ENDIF
m.gttl = m.gttl + m.DiscRs + val(m.tax1Rs) + val(m.tax2Rs)
m.roff = round(m.gttl,0) - m.gttl
m.gttl= round(m.gttl,0)

SELE billtmp
if m.bk = "5"
  m.curform = 'vquo_pln1'
endif
REPO form &curform  preview


****************************************************
* Start of Printing Related Functions
****************************************************
**************************************
FUNCTION amt_xwrd
**************************************
PARA pp
PRIV vx,rupi,paise
rupi = int(pp)
paise = pp-rupi
paiw = iif(paise > 0,' and Paise '+x_wrd(paise*100),'')
vx = 'Rupees '+x_wrd(pp) + paiw +  ' Only.'
RETURN vx
**************************************
FUNCTION x_wrd
**************************************
* Function to convert figures to Words.
PARA pv
DIME f[20],f2[9]
PRIV m.amt
F[1] = 'One '
F[2] = 'Two '
F[3] = 'Three '
F[4] = 'Four '
F[5] = 'Five '
F[6] = 'Six '
F[7] = 'Seven '
F[8] = 'Eight '
F[9] = 'Nine '
F[10] = 'Ten '
F[11] = 'Eleven '
F[12] = 'Twelve '
F[13] = 'Thirteen '
F[14] = 'Fourteen '
F[15] = 'Fifteen '
F[16] = 'Sixteen '
F[17] = 'Seventeen '
F[18] = 'Eighteen '
F[19] = 'Nineteen '

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
RETURN convt(m.amt)

**************************************
FUNCTION convt
**************************************
PARA bal
* Check for Crore
PRIV Rs
Rs = ''
bal=chkit(bal,1e7,'Crore ')
bal=chkit(bal,1e5,'Lakh ')
bal=chkit(bal,1e3,'Thousand ')
bal=chkit(bal,1e2,'Hundred ')
bal=chkit(bal,1,' ')
RETURN Rs

**************************************
FUNCTION chkit
**************************************
PARA b,d,s
PRIV ps,v,rv
ps = s
v = int(b/d)
rv = mod(b,d)
IF v > 0
  IF v < 20
    ps = f[v]+ ps
  ELSE
    ps = f2[int(v/10)]+ iif(mod(v,10) # 0,f[mod(v,10)],'') + ps
  ENDIF
  Rs = Rs + ps
ENDIF
RETURN rv


**************** End of functions ***********************************************************************************

********************  End of   TESTING MAIN LOOP **************









****************** Start of Definition of Classes **************

***********************************************

*    K N T B I L L   C L A S S

***********************************************
DEFINE class kntBill  as custom && Defines the container obj for Bills
  kurbil = NULL  && current bill
  ttlbil = 0     && total bill in kont
  lstbno = ""    &&  Last bill no.
  co = ""        &&  Copmany Code
  yr = ""        &&  Year Code
  bk = ""        &&  Book Code
  fname = ""      && contains the dbf file containing bills
  rdfrm = NULL   &&  Form for reading in data

  FUNCTION init()
  this.kurbil = createobject("Bill",this)
  this.kurbil.kntBill = this  && Points to the parent container object
  this.ttlbil = 0

  RETURN

  FUNCTION nx_bill
  SELECT max(f1no) from (this.fname) into array armx
  this.lstbno = iif (type( 'armx[1]') = 'C', armx[1],'000000')
  nxbil =  tran(val(this.lstbno) + 1,'@l ######')
  RETURN nxbil

  FUNCTION set_cyb  &&  Sets the company Year and book  given the param co,yr,bk
  PRIV m.co ,m.yr , m.bk
  m.co = this.co
  m.yr = this.yr
  m.bk = this.bk

******************************


  PRIV rv,tfname, ifname, vfname  && fname of top,itm and inv
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
  IF len(msg) > 0                                             && if msg length > 0 , file absent
    rv = .f.
    msg  = "Files " + msg + "Not Found"                       && show msg accordingly
    WAIT window msg
  ENDIF
  RETURN rv



  FUNCTION addBill()  && Adds a new Bill to container

  this.set_cyb()
  this.kurbil.clnit()
  this.kurbil.bno = this.nx_bill()
  this.kurbil.bdt = date()
  this.kurbil.f2no = this.kurbil.bno
  this.kurbil.f2dt = date()
  this.kurbil.kntBill = this
  =iif(this.rdBill() .and. .f.,this.putBill(this.kurbil.bno),"")
  RETURN


  FUNCTION ediBill(pbno)  && Edits the current Bill pbno
  PRIV v
  this.getBill(pbno)      && Get the bill no pbno
  v = this.rdBill()

  RETURN



  FUNCTION delBill(pbno)  && Deletes the current Bill pbno



  FUNCTION prnBill(pbno,nc) && Prints the Bill pbno
  this.getBill(pbno)
  WITH this
    m.co = .co
    m.bk = .bk
    m.yr = .yr
    m.bkn = m.bk
  ENDWITH


  PRIV m.Inv_copy,m.curform,m.certify,m.ftype,m.co_name,m.vat_no,m.vgst,m.f_type,m.fname_t,m.fname_i,m.bno

  m.Inv_copy = "Original for Buyer"
  m.curform = 'vcmgstsql_pln1_2' && this is the current form with  IEC code
  Clear

  m.certify = "I/We hereby Certify that my/our registration Certificate under the Maharashtra Value Added Tax Act 2002 is in force "
  m.certify = m.certify +  "on the date on which the sale of the goods specified in this Tax Invoice is made by me/us and that and that the transaction of Sale covered by this Tax Invoice has been effected by me/us and it shall be accounted for in the turnover of sales while"
  m.certify = m.certify +   "filing of return and th due tax if any, payable on the sale has been paid or shall be paid."
  m.ftype = "Cash Memo"

  m.co_name = iif(m.co = 'PM',"Panalal Mohanlal & Co",'Kamlesh Corporation')
  m.vat_no = iif(m.co = 'PM','27AAAFP1396C1ZG','27AAAFK1972D1ZL')
  m.vgst = m.vat_no  && vendors gst number
  m.f_type = iif(m.bkn = '1',"Cash Memo (Tax Invoice)",'Tax Invoice')

  m.f_type = iif(m.bkn > '2','Delivery Challan',m.f_type)
  m.f_type = iif(m.bkn = '5','Quotation',m.f_type)

  m.fname_t = this.fname
  m.fname_i =  this.kurbil.itmlst.fname
  m.bno = this.kurbil.bno
  Do Case
  case m.bk < "4"
    FOR jj = 1 to nc
      =prn_1bn(m.bno,jj,this) && send the bno,no of copies & object
    ENDFOR
  case m.bk = "4"
     m.f_type = 'PROFORMA INVOICE'
     =prn_1bn(m.bno,1,this) && send for proforma
  case m.bk = "5"  
    m.f_type = 'QUOTATION'
    =prn_1bn(m.bno,1,this)
    
  Endcase
  m.frmopr.show()
  RETURN

  FUNCTION putBill(pbno)  && Puts the current bill on storage pbno
  WAIT window "Putting Bill" nowait
* check if bno exists ?  if so  use sql  update else sql insert
  SELECT f1no from (this.fname)  where f1no = m.pbno into array arbno


  IF type('arbno[1]')= 'C'

    UPDATE (this.fname)  set f1dt = m.f1dt,;
      f2no = m.f2no, ;
      f2dt = m.f2dt , ;
      pcode= m.pcode, ;
      pname = m.pname, ;
      p_addr = m.p_addr, ;
      cr = m.cr ,;
      gst = m.gst ;
      where f1no  = m.pbno


  ELSE
    WITH this
      .kurbil.gst = m.gst
      .kurbil.cr = m.cr

      INSERT into(this.fname)  (f1no,f1dt,f2no, f2dt,pcode,form,pname,p_addr,cr,gst)   values (m.pbno,.kurbil.bdt,.kurbil.f2no,.kurbil.f2dt,.kurbil.pcode,.kurbil.frm,.kurbil.pname,.kurbil.p_addr,.kurbil.cr,.kurbil.gst)
    ENDWITH
  ENDIF
  WITH this.kurbil
* Now update the Order details
    .oOrd.ordno = m.ordno
    .oOrd.orddt = m.orddt
    .oOrd.put_obj()
* Also update the Trspt details
    .oDlv.trsp = m.trspt
    .oDlv.Dest = m.dest
    .oDlv.lrno = m.lrno
    .oDlv.lrDt = m.lrDt
    .oDlv.put_obj()

* now  get the total items of the bill in file and acoordingly use upd/ins sql
    .itmlst.put_itms() && put the items into the item container file
  ENDWITH

  RETURN

  FUNCTION getBill(pbno)  && Gets the bill from storage into curbill obj
  PRIVATE m.bno, obno
  m.obno = this.kurbil  && store the Bill object temporarily in the memory.object (m.obno)
  m.bno = m.obno.bno    && get the bno from the object

  SELECT * from (this.fname) where f1no = m.pbno into array ar1   && get the details of the object from the file using (this.fname) into array ar1


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
  WITH obno.oOrd
    .get_obj()
    m.ordno = .ordno
    m.orddt = .orddt
  ENDWITH
* Also update the Trspt details
  WITH obno.oDlv
    .get_obj()
    m.trspt= .trsp
    m.dest = .Dest
    m.lrno = .lrno
    m.lrDt = .lrDt
    .put_obj()
  ENDWITH

* Now get the Transport details

* Now get the items
  obno.itmlst.bno = ar1[1]     && set the billno
  m.bno = ar1[1]
  obno.itmlst.get_itms(m.bno)  && get the items into the items list

  RETURN


  FUNCTION rdBill
  PRIV rv
  PRIV m.f1no,m.f1dt, m.f2no, m.f2dt, m.pcode, m.pname , m.p_addr
  PRIV m.cr,m.gst
  PRIV m.ordno,m.orddt,m.trspt,m.lrno,m.lrDt,m.dest

  WITH this.kurbil
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
    m.lrDt =  .oDlv.lrDt
    m.dest =  .oDlv.dest


  ENDWITH
  xval= this.rdfrm.show(1)

  READ events
  rv = .f.
  IF this.rdfrm.xit = 1
    WITH this.kurbil
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
      .oDlv.lrDt = m.lrDt
      .oDlv.dest = m.dest
      rv = .t.
    ENDWITH
  ENDIF

  ASSERT ("rdBill Exiting with " + iif(rv,'True','False'))
  RETURN rv


  FUNCTION prn_bill


  RETURN






  FUNCTION nul_one()
***********************************************

  Acti scree
  Clear
  WITH this.kurbil
TEXT
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

ENDTEXT

    PRIV n
    mxitm = .itmlst.itmknt

    m.itmlst = .itmlst
    FOR n = 1 to mxitm
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
    NEXT n
  ENDWITH
  RETURN





ENDDEFINE  && End of objet kntbill


*********************************************

***********************************************

*          B I L L   C L A S S

***********************************************

DEFIN Class bill as custom   && Individual Bill obj
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
  kntBill = NULL   && Kontainer object of bills






  FUNCTION init()
  LPARA m.pkntbill
  this.itmlst = createobject("kntItm",this)
  this.oOrd = createobject("oOrd")                 && initiate the Orderobj
  this.oDlv = createobject("oTrsp")                && initiate the trsp object
  this.oOrd.obill = this                           && store the pointer to the paretn obill
  this.oDlv.obill = this
  this.kntBill = m.pkntbill                        && point to the calling container
  this.kprty = createobject("kPrty")               && create the Party container obj
  this.kprty.fname = "tmpq5.dbf"                   && set the file name of the party dbf
  this.pobj = this.kprty.oPrty                     && set the party object on the bill


  FUNCTION clnit
  WITH this
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
  ENDWITH
  RETURN

  FUNCTION gt_prty(srch_str)
* gets the party using the objparty party obje
  this.kprty.frmbrw.prvfrm = _screen.activeform
  this.kprty.brw_obj(srch_str)
  m.pmsg = this.kprty.oPrty.pname
  WAIT window m.pmsg

*  this.kprty.frmbrw.prvform.show



ENDDEFINE
***********************************************

***********************************************

*    K N T I T M     C L A S S

***********************************************

DEFINE class kntitm  as custom  && Define kontainer obj of items
  bno = spac(6)
  DIME lstItm[1] = NULL         && list of items
  itmknt  = 0                   &&  item kounts
  oitm  = NULL                  && Item object
  cursr  = 0                    && current serial
  fname = ""                    && file name of items
  ikifname = ""                 && iki file name
  rdfrm  = NULL                 &&  form to read items
  obill  = NULL                 && points to the objbill


  FUNCTION init(m.pbobj)
* wait window "Creating item Container..."
  this.obill = m.pbobj
  this.oitm = createobject("itm",this)
  this.oitm.kntitm = this       &&  point to the container of  this item
  this.oitm.omcs.obill = this.obill         && set the pointer to obill
  this.oitm.omcs.fname =  this.ikifname && set the file name for further operations
  this.oitm.bno = this.bno
  this.itmknt = 0


  RETURN

  FUNCTION clnit
  WITH this
    DIME lstItm[1] = NULL
    .itmknt = 0
    .bno = '000000'
    .oitm.clnit()
  ENDWITH
  RETURN


  FUNCTION addItm()
  PRIV i,m.sr
  this.itmknt = this.itmknt + 1
  i = this.itmknt
  m.sr = tran(i,'@l ##')
  DIME this.lstItm[i]
  this.cursr = i
  this.lstItm[i] = createobject('itm',this)
  this.oitm = this.lstItm[i]
  this.oitm.kntitm = this
  this.oitm.omcs.fname = this.oitm.kntitm.ikifname
  this.bno = this.obill.bno
  this.oitm.bno = this.bno
  this.oitm.sr = m.sr
  WAIT window "Bill no : " + this.bno
  this.oitm.bno =  this.bno  && m.bno is presumed to be the current bno
  this.rdfrm.oitm = this.oitm
  WITH this.oitm
    .typ = 'INV'
    .unt = 'NOS'
    .amt = 0
  ENDWITH


  this.rdItm()


  RETURN


  FUNCTION ediItm()
  WAIT window "Editing ... Item Sr " + tran(itms.sr,'@l ##')  && <- Data Mismatch / Corrected
  IF val(itms.sr) > 0
    this.oitm = this.lstItm[val(itms.sr)]
    this.rdfrm.oitm = this.oitm
    prvtyp = this.oitm.typ                       && keep a record of the type for removing/updating of INV item
    DEBUGO "Moving to Rditm"
    this.rdItm()
    DEBUGO  "back from Rditm"
  ENDIF

  RETURN

  FUNCTION delItm()

  FUNCTION put_itms()
  WAIT window "putting items into the file... " nowait
* Exit if there are no items to add
  IF this.itmknt > 0

    SELECT sum(1) from (this.fname)as itmk where this.bno == itmk.bno into array aritm
* if items don't exist insert all the items as new
    xitm = iif(type("aritm")= "U",0,aritm[1])
    DO case
    CASE xitm = 0                       &&  No items previously
      this.upd_itm("ins",1,this.itmknt)      && insert all the items
    CASE xitm = this.itmknt             &&  same number of items as before
      this.upd_itm("upd",1,this.itmknt)      && update all the items
    CASE xitm > this.itmknt             &&  More items previously , remove space
      this.upd_itm("upd",1,this.itmknt)      && update the items
      this.upd_itm("rmv",this.itmknt+1,xitm) && remove the excess items
    CASE xitm < this.itmknt             &&  More items now , make space
      this.upd_itm("upd",1,xitm)             &&  update the existing items
      this.upd_itm("ins",xitm+1,this.itmknt) &&  insert the balance items
    ENDCASE
  ENDIF

  RETURN

  FUNCTION upd_itm(opr,st,ed)       &&  operation/ start / end
  PRIV n,ot,tmp1,tmp2
  PRIV tmpiki
  STORE  left(this.fname,at(".dbf",this.fname)) to tmp1,tmp2
  tmp1 = tmp1+ "desc"
  tmp2 = tmp2 + "type"

  FOR n = st to ed
    ot = this.lstItm[n]
    WITH ot
      ot.bno = this.obill.bno  && set the billno for all items
* If inv item update  inv file
      IF ot.typ =="INV"
* Upate/insert the inv file
        WAIT window "Updating Inventory..."
* set step on  &&  to check why new records are being added for same itme when edited
        DEBUGO "iki " + ot.iki + "Before entering"

        ot.iki = ot.omcs.put_iki(ot.iki)
      ENDIF
      m.sr = tran(ot.sr, '@l ##')
      DO Case
      CASE opr = "ins"
        INSERT into (this.fname) (bno,sr,type,qty,unt,rate,dsc,tax,amt,iki,desc,hsn ) ;
          values(.bno,m.sr,.typ,.qty,.unt,.rate,.dsc,.tax,.amt,.iki,.descr,.hsn)
* now if type is inv insert into inv file
*ot.omcs.put_iki(m.tmpiki)
      CASE opr = "upd"
        UPDATE (this.fname) set bno= ot.bno, sr = m.sr , &tmp2 = ot.typ , qty = ot.qty ,;
          unt = ot.unt, rate = ot.rate , dsc = ot.dsc , tax = ot.tax , ;
          hsn = ot.hsn ,amt = ot.amt,;
          iki = ot.iki , &tmp1 = ot.descr ;
          where bno == ot.bno .and. sr == m.sr
      CASE opr = "rmv"

      ENDCASE
    ENDWITH
  NEXT n

  RETURN


  FUNCTION get_itms(fno)   && gets items for a particular form

  SELECT * from (this.fname) where bno = m.fno into array arz
  SELE itms
  ZAP
  IF _tally > 0
    INSERT into itms from array arz
    this.itmknt = _tally
    DIME this.lstItm[this.itmKnt]
    SELE itms
    GO top
    FOR n = 1 to this.itmknt
      this.cursr = n
      SCATTER memvar memo
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

      IF !v.iki == "" .and. v.typ = "INV"
        v.omcs.get_iki(v.iki)  && get the iki obj populated
      ENDIF
      SELECT itms              && Now transfer the data to read/write cursor
      Skip
*    insert into itms from memvar
    NEXT n
  ELSE
    DIME this.lstItm[1]       && Doesnt have items.
  ENDIF


  FUNCTION nul_one

  this.itmknt = reccount("itms")

  IF this.itmknt > 0
    DIME this.lstItm[this.itmKnt]
    SELECT itms
    GO top
    FOR n = 1 to this.itmknt
      this.cursr = n                  && set the curent serial no for the item
      SCATTER memvar memo
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

      IF !v.iki == "" .and. v.typ = "INV"
        v.omcs.get_iki(v.iki)  && get the iki obj populated
      ENDIF

      SELECT itms              && Now transfer the data to read/write cursor
      Skip
    NEXT n
  ELSE
    DIME this.lstItm[1]       && Doesnt have items.
  ENDIF
  RETURN  &&   Function nul_two


  FUNCTION rdItm()
  this.rdfrm.oitmkont = this


  this.rdfrm.show()            && Make it a Modal form
  DEBUGO  "Now Scrolling"
  this.rdfrm.grditm.doscroll(0) && scroll up 2 = page 0 = line  1 = scroll down line 3 = scroll down page
  DEBUGO  "Now setting focus on cmbtyp"
  this.rdfrm.cmbTyp.setfocus()
  DEBUGO  "Now returning back to Edi itm"
  RETURN


  FUNCTION dsp_itms()

ENDDEFINE


*****************************************


***********************************************

*          I T M     C L A S S

***********************************************

DEFINE class itm  as custom
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
  Descr = ""         &&
  hsn = ""           &&  hsn code for GST
  kntitm = NULL      &&  pointer to the container of items
  omcs = NULL        &&  invobj
  kmcs = NULL        &&  Kontainer obj for mcs, contains also mcsd used for retrieving descripition /prices etc
  tcurs = "itms"         &&  temp cursor name for retrieving and saving item data

  FUNCTION init(pkntItm)
  this.kntitm = m.pkntItm
* wait window "Creating item ..."

  this.sr =  this.kntitm.cursr                             &&spac(m.sr)
  this.omcs = createobject("omcs",this)
  this.kmcs = createobject("kmcs",this.omcs)   &&
  RETURN

  FUNCTION clnit
  PARA bno
  WITH this
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
  ENDWITH
  RETURN


  FUNCTION get_itm2c(m.sr)
* wait window "Ouch get_Itm2c(m.sr)"
  RETURN
  PRIV a
  SELECT * from  (this.tcurs) where itms.sr == m.sr into cursor juju
  SCATTER memvar memo
  USE juju

  FUNCTION put_itm2c(m.sr)
  PRIV fn2
* set step on
  WITH this
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
  ENDWITH
  WAIT window "put_itm2c(m.sr)"
  SELECT sum(1) from (this.tcurs) into array ar_sr where sr = m.sr &&   <-Data mismatch /hopefull corrected, param m.sr comes as string now
  IF _tally = 0
    INSERT into (this.tcurs) (sr,type,qty,unt,rate,dsc,tax,amt,desc,hsn ) ;
      values (m.sr,m.type,m.qty,m.unt,m.rate,m.dsc,m.tax,m.amt,m.desc,m.hsn)
  ELSE
    fn2 = this.tcurs
    UPDATE (fn2) set itms.type=m.type, ;
      itms.qty=m.qty,   ;
      itms.unt=m.unt,   ;
      itms.rate=m.rate, ;
      itms.dsc = m.dsc, ;
      itms.tax = m.tax, ;
      itms.amt = m.amt ,;
      itms.desc = m.desc, ;
      itms.hsn = m.hsn ;
      where  sr == m.sr
  ENDIF

ENDDEFINE




**********************************************



**********************************************
DEFINE Class omcs as custom
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


  FUNCTION init( poitm)
  this.kmcs = createobject("kmcs",this)  && sent this mcs object as para
  this.oitm = m.poitm
  this.obill = this.oitm.kntitm.obill
  RETURN


  FUNCTION nxt_iki()  &&  Gets the next iki sequentially
  PRIV ar1
*   set step on
  SELECT max(iki) from (this.fname) where iki # '99999999' into array ar1
  IF type('ar1[1]') # 'C'
    rv ='00000001'
  ELSE
    rv = tran(val(ar1[1])+1,'@l ########')
  ENDIF
  RETURN rv


  FUNCTION get_iki(m.iki)  && gets the record from file based on iki into obj
  PRIV ar1,rv
  rv = .f.
  SELECT recno(),mfg,icode,asize from (this.fname) where iki == m.iki into array ar1
* set step on
  IF !(type('ar1[1]') = 'U')
    WITH this
      .rcno = ar1[1]
      .mfg = ar1[2]
      .icode = ar1[3]
      .asize = ar1[4]
      .iki = m.iki
    ENDWITH
    rv = .t.
  ENDIF
  RETURN rv


  RETURN

  FUNCTION put_iki(m.iki)
  PRIV ar,m.iki
  DEBUGO " in Put_iki " + m.iki
*set step on
  IF Empty(m.iki) && If there are no iki get a fresh iki and add the entry to file
* get the next iki
    m.iki = this.nxt_iki()  && get fresh iki
    m.iki = this.add_iki(m.iki)  && add it to file
  ELSE
    m.bobj = this.obill && tmp save the bill obj
    m.iobj = this.oitm  && also save to itm obje
* check if Iki exists in the file ?
    SELECT iki from (this.fname) where iki == m.iki into array ar
    IF _tally = 1
      DEBUGO  "updating "+ m.iki + "in put_iki" && update the iki if it already exists in file
      UPDATE (this.fname) set b_no = m.bobj.bno,;
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
        where  iki == m.iki
    ELSE
      DEBUGO "Inserting " + iki + " in put_iki"
      m.iki = this.add_iki(iki)  && else you need to add this new iki and store this iki in itm
    ENDIF  && _tally = 1
  ENDIF  && enpty iki
  RETURN m.iki   && return iki value to store in item


  FUNCTION add_iki(m.iki) && this just adds the iki entry into the file
  PRIV b_no,m.b_dt,Dlv_no,Dt,mfg,icode,asize,qty,rate,dsc,tax
  WITH this.obill
    m.b_no = .bno
    m.b_dt = .bdt
    m.dlv_dt = .f2dt
    m.Dlv_no = .f2no
  ENDWITH
  WITH this.oitm
    m.mfg = this.mfg
    m.icode = this.icode
    m.asize = this.asize
    m.qty = .qty
    m.rate = .rate
    m.dsc = .dsc
    m.tax = .tax

  ENDWITH
  INSERT into (this.fname) (b_no,b_dt,mfg,icode,asize,qty,rate,dsc,tax,Dlv_no,dlv_dt,Dt,iki) ;
    values (m.b_no,m.b_dt,m.mfg,m.icode,m.asize,m.qty,m.rate,m.dsc,m.tax,      ;
    m.Dlv_no,m.dlv_dt,m.dlv_dt,m.iki )
  RETURN m.iki


  FUNCTION del_iki(iki)  && this removes the iki entry by making it '999999' and nullifying vals
&& help of the recno is taken for this.
  UPDATE (this.fname) set b_no = '' ,;
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
  RETURN





  FUNCTION rd_mcs()

  RETURN


  FUNCTION get_desc
  PRIV rv
  RETURN rv  && returns   Desc in rv

  FUNCTION get_pri
  PRIV rv

  RETURN rv   && returns price in rv


ENDDEFINE

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



DEFINE class  oOrd as custom
* First line in memo is Ordno and 2nd line is Orddt

  ordno = ''                 && first line in memor
  orddt = ctod('')           && second line in memo
  obill = NULL       &&  will contain the parent bill object which cotains this obj

  FUNCTION ini()

  RETURN

  FUNCTION clnit
* clean up for new data
  WITH this
    .ordno = ''
    .orddt = ctod("")
  ENDWITH
  RETURN

  FUNCTION put_obj  && puts the object into the respective container fields

  PRIV js,fname,bno,cr1

  cr1 = chr(13)+chr(10)
  m.bno = this.obill.bno
  m.fname = this.obill.kntBill.fname  && get the file name

  js = this.ordno +  cr1 + dtoc(this.orddt)
  UPDATE (m.fname) set o_det = js  where f1no = m.bno

************************************************

  FUNCTION get_obj  && gets the object from the respective container fields
* wait window "Getting the Order Details"

  PRIV ar1 ,m.bno,m.fname && array to hold resulsts
  m.bno = this.obill.bno
  m.fname = this.obill.kntBill.fname  && get the file name
  SELECT o_det from (m.fname) where f1no == m.bno into array ar1
  IF type('ar1') # 'U'
* set step on
    this.ordno = mline(ar1[1,1],1)  && get the dest
    this.orddt = ctod(mline(ar1[1,1],2)) && get the lr date
  ENDIF

ENDDEFINE





DEFINE class otrsp as custom

  Dest = ''   &&  1st line
  trsp = ''   &&  2nd Line in memo
  lrno = ''   &&  3rd line in memo
  lrDt = ctod('')  && 4th line in memo
  obill = NULL       &&  will contain the parent bill object which cotains this obj

  FUNCTION init()

  RETURN

  FUNCTION clnit
  WITH this
    .dest = ''
    .trsp = ''
    .lrno = ''
    .lrDt = date()
  ENDWITH
  RETURN


  FUNCTION put_obj  && puts the object into the respective container fields

  PRIV js,fname,bno,cr
  m.cr = chr(13)+chr(10)
  m.bno = this.obill.bno
  m.fname = this.obill.kntBill.fname  && get the file name

  js = this.dest + m.cr + this.trsp + m.cr + this.lrno + m.cr + dtoc(this.lrDt)
  UPDATE (m.fname) set d_det = js  where f1no = m.bno
  RETURN


  FUNCTION get_obj  && gets the object from the respective container fields

  PRIV ar1 ,m.bno,m.fname && array to hold resulsts
  m.bno = this.obill.bno
  m.fname = this.obill.kntBill.fname  && get the file name
  SELECT d_det from (m.fname) where f1no == m.bno into array ar1
  IF type('ar1') # 'U'
    this.dest = mline(ar1[1,1],1)  && get the dest
    this.trsp = mline(ar1(1,1),2) && get the traspt
    this.lrno = mline(ar1(1,1),3) && get the lrno
    this.lrDt = ctod(mline(ar1[1,1],4)) && get the lr date
  ENDIF

ENDDEFINE




************************************************************************
*********************************   END   OF   FILE    *****************
************************************************************************