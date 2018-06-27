* File name  objbillv400.prg as on 23/06/18
* Change.log ->  Work in progress for proforma and quotation as on 27/05/18
* uses Form frmitm2 upgraded to take into account the calculation for quotes
* -------------------------------------
* Change.log ->  incorporating  Class_kgst for  tax calcultation and other
* Uses the following screen files FRMTUP.SCX  & FRMITM.SCX
* uses the following Databases :
* Uses the following Report Files :
* By : Sunil Gandhi
* As On : 16/04/2018 4:30 pm ist
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

CLEAR
CLEA WINDOW ALL
CLEAR ALL
SET EXCL OFF
SET DATE brit
m.ver = 4.00
* added IEC code in the printout
WAIT WINDOW  "Starting version " + TRAN(m.ver, "@l ##.##")




IF ADIR(j3,"\\samba24\pmc\pm\cm\*") # 0
*  SET path to \\samba24\pmc\pm\cm
  SET PATH TO .\src\,.\screens,.\REPO,.\DATA,\\samba24\pmc\pm\cm
ELSE

  SET PATH TO F:\pm\daily.0\cm
ENDIF



CREATE CURSOR itms( bno c(6),sr c(2), TYPE c(3), qty N(9,2),unt c(5), rate N(9,2), ;
  dsc N(6,2) , tax N(6,2), amt N(10,2), iki c(8), DESC m, hsn c(15) )

SET PROCEDURE TO objmcs.prg             && Class definitions related to mcs data whereas omcs is define here.
SET PROCEDURE TO objparty.prg ADDITIVE  &&  Class definitions of objparty
SET PROCEDURE TO Class_kgst.prg  ADDITIVE

m.frmtup=NULL
m.frmitm=NULL
m.frmtnc=NULL   && Terms & Conditions form for quotation
m.frmopr = NULL


*
m.kgst1 = CREATEOBJECT("kgst")       && Create the Container and its object for GST
m.pmbknt = CREATEOBJECT("kntBill")
WITH m.pmbknt
  .co = 'PM'   && set the co code
  .yr = '18'   && et the yr code
  .bk = '2'    && set the book code
  .set_cyb()
ENDWITH


DO FORM frmtup NAME m.frmtup NOSHOW
DO FORM frmitm2 NAME m.frmitm NOSHOW && with pmbknt.kurbil.itmLst
DO FORM frmtnc NAME m.frmtnc NOSHOW
SELE 0
DO FORM frmopr NAME m.frmopr NOSHOW  && main operation form for select co,yr bk, & add/edit/print etc


m.frmtup.bkobj = m.pmbknt   && set the refrence to the bill cont obj
m.frmitm.bkobj = m.pmbknt   && set the refrence to the bill cont obj

m.frmopr.oblkont = m.pmbknt && set the refrence to the bill cont obj

m.frmopr.obill = m.pmbknt.kurbil  && set the refrence to the bill obj

m.frmitm.oitmkont = m.frmitm.bkobj.kurbil.itmlst && set ref to itm cont obj
m.frmitm.oitm =  m.frmitm.bkobj.kurbil.itmlst.oitm && set ref to itm obj
m.pmbknt.rdfrm = m.frmtup                 && set ref to  bill obj reading form
m.pmbknt.kurbil.itmlst.rdfrm = m.frmitm   && set ref to itm obj reading form




CLEAR
m.frmopr.SHOW()     && start the main operation form
m.frmopr.cboCo.SETFOCUS() && set the focus and start of events
READ EVENT
CLOSE DATA               && exit from the event loop
CLEA WINDOWS ALL         && clea all windows & objs
CLEA ALL
WAIT WINDOW "Over"
**********************************************************************************
RETURN

***** Start of functions ***************************


**************************************
FUNCTION sw_win(shw,hid)  && function to show hide windows
**************************************
  DO CASE
    CASE shw = 'bill'
      =frmitm.HIDE()
      =frmtup.SHOW()
    CASE shw = 'itm'
      SELE itms
      GO TOP
      =frmtup.HIDE()
      =frmtnc.HIDE()
      =frmitm.SHOW()
    CASE shw = 'tnc'
      frmitm.HIDE()
      frmtnc.SHOW()
  ENDCASE


*IF shw = "bill"
*  =frmitm.hide()
*  =frmtup.show()

*ELSE
*  SELE itms
*  GO top
*  =frmtup.hide()
*  =frmitm.show()

*ENDIF
  RETURN

************ Global  Functions to print ***************


**************************************
FUNCTION prn_1bn(m.blno,cn,kobj)
**************************************
* used to print a Bill
* cn is the copy number ranging from 1 to 3

  cn = IIF(!BETWEEN(cn,1,3),1,cn)
  DIME arc[3]
  arc[1] = "Original for Buyer"
  arc[2] = "Triplicate for Seller"
  arc[3] = "Duplicate for Transporter"

  PRIV m.Inv_copy,m.curform,m.certify,m.ftype,m.co_name,m.vat_no,m.vgst,m.f_type,m.fname_t,m.fname_i,m.bno
  m.bk_type = ""
  m.Inv_copy = arc[cn]
  m.curform = 'vcmgstsql_pln1_2' && this is the current form
  CLEAR

  m.certify = "I/We hererby declare that this invoice shows the actual price of the goods described "
  m.certify = m.certify + "and that all particulars are true and correct."
  m.ftype = "Cash Memo"

  m.co_name = IIF(m.co = 'PM',"Panalal Mohanlal & Co",'Kamlesh Corporation')
  m.vat_no = IIF(m.co = 'PM','27AAAFP1396C1ZG','27AAAFK1972D1ZL')
  m.vgst = m.vat_no  && vendors gst number
  m.iec = IIF(m.co = 'PM','AAAFP1396C','')
  m.f_type = "Tax Invoice"
  m.f_type = IIF(m.bkn = '1',"Cash Memo (Tax Invoice)",m.f_type)
  m.f_type = IIF(m.bkn = '2','Tax Invoice',m.f_type)
  m.f_type = IIF(m.bkn = '3','Delivery Challan',m.f_type)
  m.f_type = IIF(m.bkn = '4','Proforma Invoice',m.f_type)
  m.f_type = IIF(m.bkn = '5','Quotation',m.f_type)

  m.fname_t = kobj.fname
  m.fname_i =  kobj.kurbil.itmlst.fname

  PRIVATE bno,m.bnlo
  PRIVATE artop
  m.bno = m.blno
  SELECT * FROM (m.fname_t)AS tup WHERE tup.f1no = m.bno INTO ARRAY artop  && this shall contain the party details


  IF _TALLY # 1
    RETURN -1  && error
  ENDIF

  m.txform = artop[7]       && Get the form
  m.o_dtl = artop[10]        &&  get the order details
  m.d_dtl = artop[11]       && get the Despatch Details
  m.bgst =  artop[15]        && //get the buyer's gst number
  m.stcd = LEFT(m.bgst,2)    && get the state code
  IF FILE("stmaster.dbf")
    SELECT sname,sabbr FROM stmaster WHERE scode == m.stcd INTO ARRAY s1
    IF _TALLY = 1
      m.stname  = s1[1,1]
      m.sabr = s1[1,2]
    ELSE
      m.stname = ""
      m.sabr= ""
    ENDIF
  ELSE
    WAIT WINDOW  "State Master file not there"
    m.stname = ''
    m.sabr =  ''
  ENDIF

  IF !EMPTY(bgst)
    igstflg = !(LEFT(bgst,2) == LEFT(vgst,2))             &&  Flag for  IGST / SCGS+CGST
&& if both GST numbers from same state IGSTFLG false else true
    STORE '' TO m.ord_dt1,m.ord_dt2
  ELSE
    igstflg = .F.                                          && Unregistered dealer requires sgst/cgst
  ENDIF

  IF MEMLINE(m.o_dtl) > 0
    m.ord_dt1 = MLINE(m.o_dtl,1)
    m.ord_dt2 = MLINE(m.o_dtl,2)
  ENDIF

  PRIVATE m.dst,m.trspt,m.lrno,m.lr_dt,m.d_dtl

  STORE '' TO m.dst,m.trspt,m.lrno,m.lr_dt

  IF MEMLINE(m.d_dtl) > 0
    m.dst = MLINE(m.d_dtl,1)
    m.trspt = MLINE(m.d_dtl,2)
    m.lrno = MLINE(m.d_dtl,3)
    m.lr_dt = MLINE(m.d_dtl,4)

  ENDIF
  m.kgst1 = CREATEOBJECT("kgst")
  m.kgst1.bno = m.bno
  m.kgst1.gst = m.bgst
  m.kgst1.iflg = m.igstflg
  m.kgst1.ftup = m.fname_t
  m.kgst1.fitm = m.fname_i

  m.kgst1.cr_crs()
  m.kgst1.CALC()
  m.gttl = kgst1.gamt &&  Get the gross amount from the object



****************************   Commented out from here

** Now get the bill dtails

  SELECT * FROM (m.fname_i) AS itm WHERE itm.bno = m.bno INTO CURSOR billtmp ORDER BY bno,sr



***********************************  Commended Material ends here

  STORE 0 TO m.discable ,m.DiscRs
  WITH kgst1
*set step on
    FOR N = 1 TO .dcnt                 && number of items in the array  of discount
      m.discable = m.discable + .oDed[n].amt
      m.DiscRs = m.DiscRs + .oDed[n].ded
    NEXT N
    m.DiscRs = ROUND(m.DiscRs,2)
    STORE 0 TO m.Taxable ,m.TaxRs
    str_tx = ""

    FOR N = 1 TO .gcnt             && number of items in the array of tax
      m.Taxable = m.Taxable + .oGst[n].txbl
      m.TaxRs = m.TaxRs +  IIF(igstflg,.oGst[n].igst,.oGst[n].sgst)
      str_tx = str_tx + TRAN(.oGst[n].txrt,'@l ##.##')
    NEXT N

  ENDWITH


  IF TYPE('tax_ar2') # 'U'
    FOR N = 1 TO ALEN(tax_ar2,1)
      m.Taxable = m.Taxable + tax_ar2[n,2]
      m.TaxRs = m.TaxRs + tax_ar2[n,3] * IIF(igstflg,1,0.5)
      m.tmp1 = TRAN(tax_ar[n,1]* IIF(igstflg,1,0.5) ,'@l ##.##')
      str_tx = str_tx + IIF(m.tmp1 == str_tx,"","/"+m.tmp1)
    NEXT N
  ENDIF && undefined tax_ar2
  m.TaxRs = ROUND(m.TaxRs,2)


  m.taxable1 = TRAN(m.Taxable,'@z ######.##')
  m.taxable2 = m.taxable1
  m.tx2hdg = ""
  IF igstflg
* set the headging, rate, tax amt for igst
    m.tx1hdg = "IGST @ "+ str_tx +'%'
    m.taxable2 = 0
    m.tax2Rs = ""
    m.tax1Rs = TRAN(ROUND(m.TaxRs,0), '@z ######.##')
    STORE "" TO tx2hdg
  ELSE
    m.tx1hdg = "SGST @ "+ str_tx +'%'
    m.tx2hdg = "CGST @ "+ str_tx +'%'
    m.tax1Rs  = TRAN( ROUND(m.TaxRs,0), "@z ########.##")
    m.tax2Rs = m.tax1Rs
  ENDIF
  m.gttl = m.gttl + m.DiscRs + VAL(m.tax1Rs) + VAL(m.tax2Rs)
  m.roff = ROUND(m.gttl,0) - m.gttl
  m.gttl= ROUND(m.gttl,0)

  SELE billtmp
  IF m.bk = "5"
    DIME tnc[7]
    =gen_tnc()   && generate the terms and conditions using the m.lrno
    m.curform = 'vquo_pln1'
  ENDIF
  REPO FORM &curform  PREVIEW


****************************************************
* Start of Printing Related Functions
****************************************************

**************************************
FUNCTION gen_tnc
**************************************
  m.lrno = IIF(m.lrno # '',m.lrno ,'01020101020303')
  SELECT dscr FROM tncmas WHERE tnc = 'TC01' INTO ARRAY tnc01
  SELECT dscr FROM tncmas WHERE tnc = 'TC02' INTO ARRAY tnc02
  SELECT dscr FROM tncmas WHERE tnc = 'TC03' INTO ARRAY tnc03
  SELECT dscr FROM tncmas WHERE tnc = 'TC04' INTO ARRAY tnc04
  SELECT dscr FROM tncmas WHERE tnc = 'TC05' INTO ARRAY tnc05
  SELECT dscr FROM tncmas WHERE tnc = 'TC06' INTO ARRAY tnc06
  SELECT dscr FROM tncmas WHERE tnc = 'TC07' INTO ARRAY tnc07
  tnc[1] = tnc01[val(subst(m.lrno,1,2))]
  tnc[2] = tnc02[val(subst(m.lrno,3,2))]
  tnc[3] = tnc03[val(subst(m.lrno,5,2))]
  tnc[4] = tnc04[val(subst(m.lrno,7,2))]
  tnc[5] = tnc05[val(subst(m.lrno,9,2))]
  tnc[6] = tnc06[val(subst(m.lrno,11,2))]
  tnc[7] = tnc07[val(subst(m.lrno,13,2))]

  RETURN
**************************************
FUNCTION amt_xwrd
**************************************
  PARA pp
  PRIV vx,rupi,paise
  rupi = INT(pp)
  paise = pp-rupi
  paiw = IIF(paise > 0,' and Paise '+x_wrd(paise*100),'')
  vx = 'Rupees '+x_wrd(pp) + paiw +  ' Only.'
  RETURN vx
**************************************
FUNCTION x_wrd
**************************************
* Function to convert figures to Words.
  PARA PV
  DIME F[20],f2[9]
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


  m.amt = PV
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
  v = INT(b/d)
  rv = MOD(b,d)
  IF v > 0
    IF v < 20
      ps = F[v]+ ps
    ELSE
      ps = f2[int(v/10)]+ IIF(MOD(v,10) # 0,F[mod(v,10)],'') + ps
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
DEFINE CLASS kntBill  AS CUSTOM && Defines the container obj for Bills
  kurbil = NULL  && current bill
  ttlbil = 0     && total bill in kont
  lstbno = ""    &&  Last bill no.
  co = ""        &&  Copmany Code
  yr = ""        &&  Year Code
  bk = ""        &&  Book Code
  fname = ""      && contains the dbf file containing bills
  rdfrm = NULL   &&  Form for reading in data

  FUNCTION INIT()
    THIS.kurbil = CREATEOBJECT("Bill",THIS)
    THIS.kurbil.kntBill = THIS  && Points to the parent container object
    THIS.ttlbil = 0

    RETURN

  FUNCTION nx_bill
    SELECT MAX(f1no) FROM (THIS.fname) INTO ARRAY armx
    THIS.lstbno = IIF (TYPE( 'armx[1]') = 'C', armx[1],'000000')
    nxbil =  TRAN(VAL(THIS.lstbno) + 1,'@l ######')
    RETURN nxbil

  FUNCTION set_cyb  &&  Sets the company Year and book  given the param co,yr,bk
    PRIV m.co ,m.yr , m.bk
    m.co = THIS.co
    m.yr = THIS.yr
    m.bk = THIS.bk

******************************


    PRIV rv,tfname, ifname, vfname  && fname of top,itm and inv
    rv = .T.                                 && set the return value to true
    tfname = m.co + m.yr + m.bk + 'top.dbf'
    ifname = m.co + m.yr + m.bk + 'itm.dbf'
    vfname = m.co + m.yr + 'inv.dbf'
    THIS.fname = m.tfname                    &&  set the topfile  database name
    THIS.kurbil.itmlst.fname  = m.ifname     &&  set the itemfile databse  name
    THIS.kurbil.itmlst.ikifname =  m.vfname  && set the inv file  database name
    msg  = IIF( FILE(m.tfname),"",m.tfname+' ')                 && check if all files are avaialable and
    msg  = msg + IIF(FILE(m.ifname), "", m.ifname + ' ')        && construct a msg showing which file
    msg  = msg + IIF(FILE(m.vfname), "", m.vfname + ' ')        && not available
    IF LEN(msg) > 0                                             && if msg length > 0 , file absent
      rv = .F.
      msg  = "Files " + msg + "Not Found"                       && show msg accordingly
      WAIT WINDOW msg
    ENDIF
    RETURN rv



  FUNCTION addBill()  && Adds a new Bill to container

    THIS.set_cyb()
    THIS.kurbil.clnit()
    THIS.kurbil.bno = THIS.nx_bill()
    THIS.kurbil.bdt = DATE()
    THIS.kurbil.f2no = THIS.kurbil.bno
    THIS.kurbil.f2dt = DATE()
    THIS.kurbil.kntBill = THIS
    =IIF(THIS.rdBill() .AND. .F.,THIS.putBill(THIS.kurbil.bno),"")
    RETURN


  FUNCTION ediBill(pbno)  && Edits the current Bill pbno
    PRIV v
    THIS.getBill(pbno)      && Get the bill no pbno
    v = THIS.rdBill()

    RETURN



  FUNCTION delBill(pbno)  && Deletes the current Bill pbno



  FUNCTION prnBill(pbno,nc) && Prints the Bill pbno
    THIS.getBill(pbno)
    WITH THIS
      m.co = .co
      m.bk = .bk
      m.yr = .yr
      m.bkn = m.bk
    ENDWITH


    PRIV m.Inv_copy,m.curform,m.certify,m.ftype,m.co_name,m.vat_no,m.vgst,m.f_type,m.fname_t,m.fname_i,m.bno

    m.Inv_copy = "Original for Buyer"
    m.curform = 'vcmgstsql_pln1_2' && this is the current form with  IEC code
    CLEAR

    m.certify = "I/We hereby Certify that my/our registration Certificate under the Maharashtra Value Added Tax Act 2002 is in force "
    m.certify = m.certify +  "on the date on which the sale of the goods specified in this Tax Invoice is made by me/us and that and that the transaction of Sale covered by this Tax Invoice has been effected by me/us and it shall be accounted for in the turnover of sales while"
    m.certify = m.certify +   "filing of return and th due tax if any, payable on the sale has been paid or shall be paid."
    m.ftype = "Cash Memo"

    m.co_name = IIF(m.co = 'PM',"Panalal Mohanlal & Co",'Kamlesh Corporation')
    m.vat_no = IIF(m.co = 'PM','27AAAFP1396C1ZG','27AAAFK1972D1ZL')
    m.vgst = m.vat_no  && vendors gst number
    m.f_type = IIF(m.bkn = '1',"Cash Memo (Tax Invoice)",'Tax Invoice')

    m.f_type = IIF(m.bkn > '2','Delivery Challan',m.f_type)
    m.f_type = IIF(m.bkn = '5','Quotation',m.f_type)

    m.fname_t = THIS.fname
    m.fname_i =  THIS.kurbil.itmlst.fname
    m.bno = THIS.kurbil.bno
    DO CASE
      CASE m.bk < "4"
        FOR jj = 1 TO nc
          =prn_1bn(m.bno,jj,THIS) && send the bno,no of copies & object
        ENDFOR
      CASE m.bk = "4"
        m.f_type = 'PROFORMA INVOICE'
        =prn_1bn(m.bno,1,THIS) && send for proforma
      CASE m.bk = "5"
        m.f_type = 'QUOTATION'
        =prn_1bn(m.bno,1,THIS)

    ENDCASE
    m.frmopr.SHOW()
    RETURN

  FUNCTION putBill(pbno)  && Puts the current bill on storage pbno
    WAIT WINDOW "Putting Bill" NOWAIT
* check if bno exists ?  if so  use sql  update else sql insert
    SELECT f1no FROM (THIS.fname)  WHERE f1no = m.pbno INTO ARRAY arbno


    IF TYPE('arbno[1]')= 'C'

      UPDATE (THIS.fname)  SET f1dt = m.f1dt,;
        f2no = m.f2no, ;
        f2dt = m.f2dt , ;
        pcode= m.pcode, ;
        pname = m.pname, ;
        p_addr = m.p_addr, ;
        cr = m.cr ,;
        gst = m.gst ;
        WHERE f1no  = m.pbno


    ELSE
      WITH THIS
        .kurbil.gst = m.gst
        .kurbil.cr = m.cr

        INSERT INTO(THIS.fname)  (f1no,f1dt,f2no, f2dt,pcode,FORM,pname,p_addr,cr,gst)   VALUES (m.pbno,.kurbil.bdt,.kurbil.f2no,.kurbil.f2dt,.kurbil.pcode,.kurbil.frm,.kurbil.pname,.kurbil.p_addr,.kurbil.cr,.kurbil.gst)
      ENDWITH
    ENDIF
    WITH THIS.kurbil
* Now update the Order details
      .oOrd.ordno = m.ordno
      .oOrd.orddt = m.orddt
      .oOrd.put_obj()
* Also update the Trspt details
      .oDlv.trsp = m.trspt
      .oDlv.DEST = m.dest
      .oDlv.lrno = iif(m.pmbknt.bk # '5',m.lrno,.oDlv.lrno)
      .oDlv.lrDt = m.lrDt
      .oDlv.put_obj()

* now  get the total items of the bill in file and acoordingly use upd/ins sql
      .itmlst.put_itms() && put the items into the item container file
    ENDWITH

    RETURN

  FUNCTION getBill(pbno)  && Gets the bill from storage into curbill obj
    PRIVATE m.bno, obno
    m.obno = THIS.kurbil  && store the Bill object temporarily in the memory.object (m.obno)
    m.bno = m.obno.bno    && get the bno from the object

    SELECT * FROM (THIS.fname) WHERE f1no = m.pbno INTO ARRAY ar1   && get the details of the object from the file using (this.fname) into array ar1


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
      m.dest = .DEST
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

    WITH THIS.kurbil
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
      m.dest =  .oDlv.DEST


    ENDWITH
    xval= THIS.rdfrm.SHOW(1)

    READ EVENTS
    rv = .F.
    IF THIS.rdfrm.xit = 1
      WITH THIS.kurbil
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
        .oDlv.DEST = m.dest
        rv = .T.
      ENDWITH
    ENDIF

    ASSERT ("rdBill Exiting with " + IIF(rv,'True','False'))
    RETURN rv


  FUNCTION prn_bill


    RETURN






  FUNCTION nul_one()
***********************************************

    ACTI SCREE
    CLEAR
    WITH THIS.kurbil
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

      PRIV N
      mxitm = .itmlst.itmknt

      m.itmlst = .itmlst
      FOR N = 1 TO mxitm
        m.itm = itmlst.lstItm[n]
        ? m.itm.sr + "|"
        ?? LEFT(MLINE(m.itm.DESCR,1),26) + "|"
        ?? TRAN(m.itm.qty,"####") + "|"
        ?? PADR(m.itm.unt,9) + "|"
        ?? TRAN(m.itm.rate,'#####.##') + "|"
        ?? TRAN(m.itm.dsc,"###.##") + "%\|"
        ?? TRAN(m.itm.tax,"###.##") + "%\|"
        ?? TRAN(m.itm.amt,'#######.##')+'|'
        ?
      NEXT N
    ENDWITH
    RETURN





ENDDEFINE  && End of objet kntbill


*********************************************

***********************************************

*          B I L L   C L A S S

***********************************************

DEFIN CLASS bill AS CUSTOM   && Individual Bill obj
  bno = SPAC(6)    && Bill No
  bdt = CTOD("")   && Bill Date
  f2no = SPAC(6)   && Challan No
  f2dt = THIS.bdt  && Challan date
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






  FUNCTION INIT()
    LPARA m.pkntbill
    THIS.itmlst = CREATEOBJECT("kntItm",THIS)
    THIS.oOrd = CREATEOBJECT("oOrd")                 && initiate the Orderobj
    THIS.oDlv = CREATEOBJECT("oTrsp")                && initiate the trsp object
    THIS.oOrd.obill = THIS                           && store the pointer to the paretn obill
    THIS.oDlv.obill = THIS
    THIS.kntBill = m.pkntbill                        && point to the calling container
    THIS.kprty = CREATEOBJECT("kPrty")               && create the Party container obj
    THIS.kprty.fname = "tmpq5.dbf"                   && set the file name of the party dbf
    THIS.pobj = THIS.kprty.oPrty                     && set the party object on the bill


  FUNCTION clnit
    WITH THIS
      .bno = SPAC(6)
      .bdt = DATE()
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
    THIS.kprty.frmbrw.prvfrm = _SCREEN.ACTIVEFORM
    THIS.kprty.brw_obj(srch_str)
    m.pmsg = THIS.kprty.oPrty.pname
    WAIT WINDOW m.pmsg

*  this.kprty.frmbrw.prvform.show



ENDDEFINE
***********************************************

***********************************************

*    K N T I T M     C L A S S

***********************************************

DEFINE CLASS kntitm  AS CUSTOM  && Define kontainer obj of items
  bno = SPAC(6)
  DIME lstItm[1] = NULL         && list of items
  itmknt  = 0                   &&  item kounts
  oitm  = NULL                  && Item object
  cursr  = 0                    && current serial
  fname = ""                    && file name of items
  ikifname = ""                 && iki file name
  rdfrm  = NULL                 &&  form to read items
  obill  = NULL                 && points to the objbill


  FUNCTION INIT(m.pbobj)
* wait window "Creating item Container..."
    THIS.obill = m.pbobj
    THIS.oitm = CREATEOBJECT("itm",THIS)
    THIS.oitm.kntitm = THIS       &&  point to the container of  this item
    THIS.oitm.omcs.obill = THIS.obill         && set the pointer to obill
    THIS.oitm.omcs.fname =  THIS.ikifname && set the file name for further operations
    THIS.oitm.bno = THIS.bno
    THIS.itmknt = 0


    RETURN

  FUNCTION clnit
    WITH THIS
      DIME lstItm[1] = NULL
      .itmknt = 0
      .bno = '000000'
      .oitm.clnit()
    ENDWITH
    RETURN


  FUNCTION addItm()
    PRIV i,m.sr
    THIS.itmknt = THIS.itmknt + 1
    i = THIS.itmknt
    m.sr = TRAN(i,'@l ##')
    DIME THIS.lstItm[i]
    THIS.cursr = i
    THIS.lstItm[i] = CREATEOBJECT('itm',THIS)
    THIS.oitm = THIS.lstItm[i]
    THIS.oitm.kntitm = THIS
    THIS.oitm.omcs.fname = THIS.oitm.kntitm.ikifname
    THIS.bno = THIS.obill.bno
    THIS.oitm.bno = THIS.bno
    THIS.oitm.sr = m.sr
    WAIT WINDOW "Bill no : " + THIS.bno
    THIS.oitm.bno =  THIS.bno  && m.bno is presumed to be the current bno
    THIS.rdfrm.oitm = THIS.oitm
    WITH THIS.oitm
      .typ = 'INV'
      .unt = 'NOS'
      .amt = 0
    ENDWITH


    THIS.rdItm()


    RETURN


  FUNCTION ediItm()
    WAIT WINDOW "Editing ... Item Sr " + TRAN(itms.sr,'@l ##')  && <- Data Mismatch / Corrected
    IF VAL(itms.sr) > 0
      THIS.oitm = THIS.lstItm[val(itms.sr)]
      THIS.rdfrm.oitm = THIS.oitm
      prvtyp = THIS.oitm.typ                       && keep a record of the type for removing/updating of INV item
      DEBUGO "Moving to Rditm"
      THIS.rdItm()
      DEBUGO  "back from Rditm"
    ENDIF

    RETURN

  FUNCTION delItm()

  FUNCTION put_itms()
    WAIT WINDOW "putting items into the file... " NOWAIT
* Exit if there are no items to add
    IF THIS.itmknt > 0

      SELECT SUM(1) FROM (THIS.fname)AS itmk WHERE THIS.bno == itmk.bno INTO ARRAY aritm
* if items don't exist insert all the items as new
      xitm = IIF(TYPE("aritm")= "U",0,aritm[1])
      DO CASE
        CASE xitm = 0                       &&  No items previously
          THIS.upd_itm("ins",1,THIS.itmknt)      && insert all the items
        CASE xitm = THIS.itmknt             &&  same number of items as before
          THIS.upd_itm("upd",1,THIS.itmknt)      && update all the items
        CASE xitm > THIS.itmknt             &&  More items previously , remove space
          THIS.upd_itm("upd",1,THIS.itmknt)      && update the items
          THIS.upd_itm("rmv",THIS.itmknt+1,xitm) && remove the excess items
        CASE xitm < THIS.itmknt             &&  More items now , make space
          THIS.upd_itm("upd",1,xitm)             &&  update the existing items
          THIS.upd_itm("ins",xitm+1,THIS.itmknt) &&  insert the balance items
      ENDCASE
    ENDIF

    RETURN

  FUNCTION upd_itm(opr,st,ed)       &&  operation/ start / end
    PRIV N,ot,tmp1,tmp2
    PRIV tmpiki
    STORE  LEFT(THIS.fname,AT(".dbf",THIS.fname)) TO tmp1,tmp2
    tmp1 = tmp1+ "desc"
    tmp2 = tmp2 + "type"

    FOR N = st TO ed
      ot = THIS.lstItm[n]
      WITH ot
        ot.bno = THIS.obill.bno  && set the billno for all items
* If inv item update  inv file
        IF ot.typ =="INV"
* Upate/insert the inv file
          WAIT WINDOW "Updating Inventory..."
* set step on  &&  to check why new records are being added for same itme when edited
          DEBUGO "iki " + ot.iki + "Before entering"

          ot.iki = ot.omcs.put_iki(ot.iki)
        ENDIF
        m.sr = TRAN(ot.sr, '@l ##')
        DO CASE
          CASE opr = "ins"
            INSERT INTO (THIS.fname) (bno,sr,TYPE,qty,unt,rate,dsc,tax,amt,iki,DESC,hsn ) ;
              VALUES(.bno,m.sr,.typ,.qty,.unt,.rate,.dsc,.tax,.amt,.iki,.DESCR,.hsn)
* now if type is inv insert into inv file
*ot.omcs.put_iki(m.tmpiki)
          CASE opr = "upd"
            UPDATE (THIS.fname) SET bno= ot.bno, sr = m.sr , &tmp2 = ot.typ , qty = ot.qty ,;
              unt = ot.unt, rate = ot.rate , dsc = ot.dsc , tax = ot.tax , ;
              hsn = ot.hsn ,amt = ot.amt,;
              iki = ot.iki , &tmp1 = ot.DESCR ;
              WHERE bno == ot.bno .AND. sr == m.sr
          CASE opr = "rmv"

        ENDCASE
      ENDWITH
    NEXT N

    RETURN


  FUNCTION get_itms(fno)   && gets items for a particular form

    SELECT * FROM (THIS.fname) WHERE bno = m.fno INTO ARRAY arz
    SELE itms
    ZAP
    IF _TALLY > 0
      INSERT INTO itms FROM ARRAY arz
      THIS.itmknt = _TALLY
      DIME THIS.lstItm[this.itmKnt]
      SELE itms
      GO TOP
      FOR N = 1 TO THIS.itmknt
        THIS.cursr = N
        SCATTER MEMVAR MEMO
        THIS.lstItm[n] = CREATEOBJECT('itm',THIS)
        v = THIS.lstItm[n]
        v.omcs.fname = THIS.ikifname
        v.typ = m.type           && Type
        v.qty = m.qty            && Qty
        v.unt = m.unt
        v.rate = m.rate           &&
        v.dsc = m.dsc            &&
        v.tax = m.tax            &&
        v.amt = m.amt            &&
        v.iki = m.iki           &&
        v.DESCR = m.desc         &&
        v.hsn = m.hsn

        IF !v.iki == "" .AND. v.typ = "INV"
          v.omcs.get_iki(v.iki)  && get the iki obj populated
        ENDIF
        SELECT itms              && Now transfer the data to read/write cursor
        SKIP
*    insert into itms from memvar
      NEXT N
    ELSE
      DIME THIS.lstItm[1]       && Doesnt have items.
    ENDIF


  FUNCTION nul_one

    THIS.itmknt = RECCOUNT("itms")

    IF THIS.itmknt > 0
      DIME THIS.lstItm[this.itmKnt]
      SELECT itms
      GO TOP
      FOR N = 1 TO THIS.itmknt
        THIS.cursr = N                  && set the curent serial no for the item
        SCATTER MEMVAR MEMO
        THIS.lstItm[n] = CREATEOBJECT('itm',THIS)
        v = THIS.lstItm[n]
        v.omcs.fname = THIS.ikifname
        v.typ = m.type           && Type
        v.qty = m.qty            && Qty
        v.unt = m.unt
        v.rate = m.rate           &&
        v.dsc = m.dsc            &&
        v.tax = m.tax            &&
        v.amt = m.amt            &&
        v.iki = m.iki           &&
        v.DESCR = m.desc         &&
        v.hsn = m.hsn

        IF !v.iki == "" .AND. v.typ = "INV"
          v.omcs.get_iki(v.iki)  && get the iki obj populated
        ENDIF

        SELECT itms              && Now transfer the data to read/write cursor
        SKIP
      NEXT N
    ELSE
      DIME THIS.lstItm[1]       && Doesnt have items.
    ENDIF
    RETURN  &&   Function nul_two


  FUNCTION rdItm()
    THIS.rdfrm.oitmkont = THIS


    THIS.rdfrm.SHOW()            && Make it a Modal form
    DEBUGO  "Now Scrolling"
    THIS.rdfrm.grditm.DOSCROLL(0) && scroll up 2 = page 0 = line  1 = scroll down line 3 = scroll down page
    DEBUGO  "Now setting focus on cmbtyp"
    THIS.rdfrm.cmbTyp.SETFOCUS()
    DEBUGO  "Now returning back to Edi itm"
    RETURN


  FUNCTION dsp_itms()

ENDDEFINE


*****************************************


***********************************************

*          I T M     C L A S S

***********************************************

DEFINE CLASS itm  AS CUSTOM
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
  DESCR = ""         &&
  hsn = ""           &&  hsn code for GST
  kntitm = NULL      &&  pointer to the container of items
  omcs = NULL        &&  invobj
  kmcs = NULL        &&  Kontainer obj for mcs, contains also mcsd used for retrieving descripition /prices etc
  tcurs = "itms"         &&  temp cursor name for retrieving and saving item data

  FUNCTION INIT(pkntItm)
    THIS.kntitm = m.pkntItm
* wait window "Creating item ..."

    THIS.sr =  THIS.kntitm.cursr                             &&spac(m.sr)
    THIS.omcs = CREATEOBJECT("omcs",THIS)
    THIS.kmcs = CREATEOBJECT("kmcs",THIS.omcs)   &&
    RETURN

  FUNCTION clnit
    PARA bno
    WITH THIS
      .bno = IIF( TYPE("bno") = "U","",bno)
      .sr = "00"
      .typ = "ITM"
      .qty = 0
      .unt = "NOS"
      .rate = 0
      .dsc = 0
      .tax = 13.5
      .amt = 0
      .DESCR = ""
      .iki = ''
      .hsn = ""
    ENDWITH
    RETURN


  FUNCTION get_itm2c(m.sr)
* wait window "Ouch get_Itm2c(m.sr)"
    RETURN
    PRIV a
    SELECT * FROM  (THIS.tcurs) WHERE itms.sr == m.sr INTO CURSOR juju
    SCATTER MEMVAR MEMO
    USE juju

  FUNCTION put_itm2c(m.sr)
    PRIV fn2
* set step on
    WITH THIS
      .amt = IIF(.typ $ "ITMINV",ROUND(.rate * .qty , 2),.amt)
      m.type = .typ
      m.qty = .qty
      m.unt = .unt
      m.rate = .rate
      m.dsc = .dsc
      m.tax = .tax
      m.amt = .amt
      m.desc = .DESCR
      m.hsn  = .hsn
    ENDWITH
    WAIT WINDOW "put_itm2c(m.sr)"
    SELECT SUM(1) FROM (THIS.tcurs) INTO ARRAY ar_sr WHERE sr = m.sr &&   <-Data mismatch /hopefull corrected, param m.sr comes as string now
    IF _TALLY = 0
      INSERT INTO (THIS.tcurs) (sr,TYPE,qty,unt,rate,dsc,tax,amt,DESC,hsn ) ;
        VALUES (m.sr,m.type,m.qty,m.unt,m.rate,m.dsc,m.tax,m.amt,m.desc,m.hsn)
    ELSE
      fn2 = THIS.tcurs
      UPDATE (fn2) SET itms.TYPE=m.type, ;
        itms.qty=m.qty,   ;
        itms.unt=m.unt,   ;
        itms.rate=m.rate, ;
        itms.dsc = m.dsc, ;
        itms.tax = m.tax, ;
        itms.amt = m.amt ,;
        itms.DESC = m.desc, ;
        itms.hsn = m.hsn ;
        WHERE  sr == m.sr
    ENDIF

ENDDEFINE




**********************************************



**********************************************
DEFINE CLASS omcs AS CUSTOM
  rcno = ''          && recno used for updating the records.
  mfg = SPAC(3)      && Mfg code
  icode = SPAC(10)   && Item Code
  asize = SPAC(3)    && ASize Code
  iki =  REPL('0',8) && iki for relating to invoice/inv

  oitm = NULL        && ref to the itm object
  fname = ""         && file name of inventory file
  frmmcs = ""        && Form to read the mcs
  obill = NULL       &&  will contain the parent bill object which cotains this obj
  kmcs = NULL        && Container object of mcs which needed for getting other data


  FUNCTION INIT( poitm)
    THIS.kmcs = CREATEOBJECT("kmcs",THIS)  && sent this mcs object as para
    THIS.oitm = m.poitm
    THIS.obill = THIS.oitm.kntitm.obill
    RETURN


  FUNCTION nxt_iki()  &&  Gets the next iki sequentially
    PRIV ar1
*   set step on
    SELECT MAX(iki) FROM (THIS.fname) WHERE iki # '99999999' INTO ARRAY ar1
    IF TYPE('ar1[1]') # 'C'
      rv ='00000001'
    ELSE
      rv = TRAN(VAL(ar1[1])+1,'@l ########')
    ENDIF
    RETURN rv


  FUNCTION get_iki(m.iki)  && gets the record from file based on iki into obj
    PRIV ar1,rv
    rv = .F.
    SELECT RECNO(),mfg,icode,asize FROM (THIS.fname) WHERE iki == m.iki INTO ARRAY ar1
* set step on
    IF !(TYPE('ar1[1]') = 'U')
      WITH THIS
        .rcno = ar1[1]
        .mfg = ar1[2]
        .icode = ar1[3]
        .asize = ar1[4]
        .iki = m.iki
      ENDWITH
      rv = .T.
    ENDIF
    RETURN rv


    RETURN

  FUNCTION put_iki(m.iki)
    PRIV ar,m.iki
    DEBUGO " in Put_iki " + m.iki
*set step on
    IF EMPTY(m.iki) && If there are no iki get a fresh iki and add the entry to file
* get the next iki
      m.iki = THIS.nxt_iki()  && get fresh iki
      m.iki = THIS.add_iki(m.iki)  && add it to file
    ELSE
      m.bobj = THIS.obill && tmp save the bill obj
      m.iobj = THIS.oitm  && also save to itm obje
* check if Iki exists in the file ?
      SELECT iki FROM (THIS.fname) WHERE iki == m.iki INTO ARRAY ar
      IF _TALLY = 1
        DEBUGO  "updating "+ m.iki + "in put_iki" && update the iki if it already exists in file
        UPDATE (THIS.fname) SET b_no = m.bobj.bno,;
          b_dt = m.bobj.bdt,       ;
          Dlv_no = m.bobj.f2no,    ;
          Dt =  m.bobj.f2dt,       ;
          mfg  = THIS.mfg,       ;
          icode = THIS.icode,    ;
          asize = THIS.asize,    ;
          qty  = m.iobj.qty,       ;
          rate=m.iobj.rate,        ;
          dsc = m.iobj.dsc,        ;
          tax = m.iobj.tax        ;
          WHERE  iki == m.iki
      ELSE
        DEBUGO "Inserting " + iki + " in put_iki"
        m.iki = THIS.add_iki(iki)  && else you need to add this new iki and store this iki in itm
      ENDIF  && _tally = 1
    ENDIF  && enpty iki
    RETURN m.iki   && return iki value to store in item


  FUNCTION add_iki(m.iki) && this just adds the iki entry into the file
    PRIV b_no,m.b_dt,Dlv_no,Dt,mfg,icode,asize,qty,rate,dsc,tax
    WITH THIS.obill
      m.b_no = .bno
      m.b_dt = .bdt
      m.dlv_dt = .f2dt
      m.Dlv_no = .f2no
    ENDWITH
    WITH THIS.oitm
      m.mfg = THIS.mfg
      m.icode = THIS.icode
      m.asize = THIS.asize
      m.qty = .qty
      m.rate = .rate
      m.dsc = .dsc
      m.tax = .tax

    ENDWITH
    INSERT INTO (THIS.fname) (b_no,b_dt,mfg,icode,asize,qty,rate,dsc,tax,Dlv_no,dlv_dt,Dt,iki) ;
      VALUES (m.b_no,m.b_dt,m.mfg,m.icode,m.asize,m.qty,m.rate,m.dsc,m.tax,      ;
      m.Dlv_no,m.dlv_dt,m.dlv_dt,m.iki )
    RETURN m.iki


  FUNCTION del_iki(iki)  && this removes the iki entry by making it '999999' and nullifying vals
&& help of the recno is taken for this.
    UPDATE (THIS.fname) SET b_no = '' ,;
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
      WHERE  RECNO() = THIS.rcno
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



DEFINE CLASS  oOrd AS CUSTOM
* First line in memo is Ordno and 2nd line is Orddt

  ordno = ''                 && first line in memor
  orddt = CTOD('')           && second line in memo
  obill = NULL       &&  will contain the parent bill object which cotains this obj

  FUNCTION ini()

    RETURN

  FUNCTION clnit
* clean up for new data
    WITH THIS
      .ordno = ''
      .orddt = CTOD("")
    ENDWITH
    RETURN

  FUNCTION put_obj  && puts the object into the respective container fields

    PRIV js,fname,bno,cr1

    cr1 = CHR(13)+CHR(10)
    m.bno = THIS.obill.bno
    m.fname = THIS.obill.kntBill.fname  && get the file name

    js = THIS.ordno +  cr1 + DTOC(THIS.orddt)
    UPDATE (m.fname) SET o_det = js  WHERE f1no = m.bno

************************************************

  FUNCTION get_obj  && gets the object from the respective container fields
* wait window "Getting the Order Details"

    PRIV ar1 ,m.bno,m.fname && array to hold resulsts
    m.bno = THIS.obill.bno
    m.fname = THIS.obill.kntBill.fname  && get the file name
    SELECT o_det FROM (m.fname) WHERE f1no == m.bno INTO ARRAY ar1
    IF TYPE('ar1') # 'U'
* set step on
      THIS.ordno = MLINE(ar1[1,1],1)  && get the dest
      THIS.orddt = CTOD(MLINE(ar1[1,1],2)) && get the lr date
    ENDIF

ENDDEFINE





DEFINE CLASS otrsp AS CUSTOM

  DEST = ''   &&  1st line
  trsp = ''   &&  2nd Line in memo
  lrno = ''   &&  3rd line in memo
  lrDt = CTOD('')  && 4th line in memo
  obill = NULL       &&  will contain the parent bill object which cotains this obj

  FUNCTION INIT()

    RETURN

  FUNCTION clnit
    WITH THIS
      .DEST = ''
      .trsp = ''
      .lrno = ''
      .lrDt = DATE()
    ENDWITH
    RETURN


  FUNCTION put_obj  && puts the object into the respective container fields

    PRIV js,fname,bno,cr
    m.cr = CHR(13)+CHR(10)
    m.bno = THIS.obill.bno
    m.fname = THIS.obill.kntBill.fname  && get the file name

    js = THIS.DEST + m.cr + THIS.trsp + m.cr + THIS.lrno + m.cr + DTOC(THIS.lrDt)
    UPDATE (m.fname) SET d_det = js  WHERE f1no = m.bno
    RETURN


  FUNCTION get_obj  && gets the object from the respective container fields

    PRIV ar1 ,m.bno,m.fname && array to hold resulsts
    m.bno = THIS.obill.bno
    m.fname = THIS.obill.kntBill.fname  && get the file name
    SELECT d_det FROM (m.fname) WHERE f1no == m.bno INTO ARRAY ar1
    IF TYPE('ar1') # 'U'
      THIS.DEST = MLINE(ar1[1,1],1)  && get the dest
      THIS.trsp = MLINE(ar1(1,1),2) && get the traspt
      THIS.lrno = MLINE(ar1(1,1),3) && get the lrno
      THIS.lrDt = CTOD(MLINE(ar1[1,1],4)) && get the lr date
    ENDIF

ENDDEFINE




************************************************************************
*********************************   END   OF   FILE    *****************
************************************************************************
