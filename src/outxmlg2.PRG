* Program to output XML for Taly GST
* as on 2nd May 2018
* incorporated export sales to exporter /also address made xml compliant /incorporated vno into remote id for unique signature
* By Sunil Gandhi


close data
clea all
set date brit
set century on

set excl off
set conf on
dfdrv = sys(5)
dfpth = dfdrv+ "\objbill"
set path to
if file("\\samba24\pmc\pm\cm\cf.dbf")
  dfpth2 = "\\samba24\pmc\, \\samba24\pmc\pm\cm,.\data,.\src"
else
  dfpth2 = "c: ; c:\pm\cm"
endif
set path to (dfpth2)
? set("path")
wait window
clear
if !File("gstmast.dbf")
  && create the necessary ledgers if they do not exists
  create table gstmast(code c(10),ldgr c(25))
  insert into gstmast values("SLSINTRA" ,"GST INTRA SALE")
  insert into gstmast values("SLSINTER", "GST INTER SALE")
  insert into gstmast values("TAXCGST","TAX CGST")
  insert into gstmast values("TAXSGST","TAX IGST")
  insert into gstmast values("TAXSGST","TAX SGST")
endif
m.bk = "CM"
m.co = "PM"


*set step on
m.year = '13' 
m.yr = year(date())- iif(month(date())<4,1,0)
m.year = right(str(m.yr),2)
if month(date()) = 4
  msg = "Press (1) for " + str(m.yr-1) + " (2) for Year " + str(yr)
  wait window msg to ans
  m.year = right(str(iif( ans = "1" , m.yr -1,m.yr)),2)
endif

frm1 = createobje("form")
 =fill_frm1()
 frm1.show
 read events
 frm1= NULL
 activate screen
 
 if empt(m.co) .and. empt(m.bk) 
   return 
 endif
m.slrgst = iif(m.co = "PM","27AAAFP1396C1ZG", "27AAAFK1972D1ZL")

* set step on
if !rtrv_ldg(m.dfpth)
   wait window "Master File Does Not Exist !  IMport Data and TRY"
   return
endif  

select acctldg
if reccount() > 0
  index to tmp1 on uppe(lname)
endif

 m.ofil = m.co + m.bk  + "tmp.xml"
 m.ftop = '\pm\cm\'+ m.co + m.year+ iif(m.bk = 'CM','1','2') + 'top.dbf'
 m.fitm = '\pm\cm\'+ m.co + m.year+ iif(m.bk = 'CM','1','2') + 'itm.dbf'
 m.company = m.co
 ? m.ftop
 ? m.fitm
 wait
set textmerge to (m.ofil)
set textmerge on


Dime m.addr[4] && Address Array to parse the address and VAT from the Invoice for new Master.xml
sele 0
use (m.ftop) alia tup
go bott
kgst1 = createobject("kgst") && create the GST object for calculation
kgst1.ftup = m.ftop
kgst1.fitm = m.fitm
vou1 = createobject("t9vou")
oKont = createobj("ktlym")  && create the container object
oKont.co = m.co             && set the company name.
oKont.gen_fname()           && generate the filename for xml
vou1.out_hdr()
Do while .t.
  sele tup
  
  brow nomo fields f1no,f1dt,pname,gst,famt
  if lastkey() = 27
     exit
  endif

  scatter memvar
  * search for  the party existing
  Do Case
  Case m.bk = 'BILL'
    if !seek(uppe(alltrim(tup.pname)),"acctldg")
      sele acctldg
      goto recno(0)
      brow nomo
      if lastkey() # 27
        m.pname = acctldg.lname
      else
        wait window "Add this party to master ? (Y/N)" to yno
        if uppe(yno) = "N"
          m.pname = "New Party"
        else
          * addtomas()  &&  add to master using gentmas in batch mode
          oKont.add_tlm(tup.pname, tup.p_addr,tup.gst)

           m.pname = tup.pname
        endif  
      endif
    else
      m.pname = acctldg.lname  
    endif
  Case m.bk = "CM"
     * Nothing to do right now
  EndCase  
  *m.f1no = tup.f1no
  *m.ki = tup.f1no
  vou1.dt = tup.f1dt
  vou1.gst = m.gst
  
  
  igstflg =  !(left(vou1.gst,2) == left(slrgst,2))  &&  if states diff igst else sgst+cgst
  
  
 *  select * from (m.fitm) as itm ;
 * left join  (m.ftop) as tup on tup.f1no = itm.bno ;
 *  where  itm.bno = m.f1no ;
 *  into cursor frm1
   
 *  select sum(amt*(1+dsc/100)),tax,sum(tax/100*amt*(1+dsc/100)) ;
 *  from frm1 into arra vl ;
 *  group by tax ;
 *  where frm1.type # 'NAR'

 * if type('vl') # 'N'
 *    wait window "Error , Looping Back"
 *   loop
 *  endif
 *  txcat = alen(vl,1)
 *  m.amt = 0

   ? iif(m.bk = "CM","Cash Memo ","Bill No : ") + m.f1no
   ?? ' Dt ' + dtoc(m.f1dt)
   ?? iif(m.bk = "CM","Counter Sale ",' Party : ') + m.pname

   ? ' Amount     Tax Rate   Tax'
      
 *  For n = 1 to txcat
 *     vl[n,1] = round(vl[n,1],2)
 *    * vl[n,3] = round(vl[n,3],2)
 *    vl[n,3] = round(vl[n,3],0)
 *    * m.amt = m.amt + vl[n,1]+ round(vl[n,3],2)
 *    m.amt = m.amt + vl[n,1]+ vl[n,3]
 *   ? tran(vl[n,1],'#######.##') + spac(4)+ tran(vl[n,2],'##%') + spac(1) + tran(vl[n,3],'#######.##')
 *  Next n
 *  m.vatav = round(m.amt,0)-m.amt
 *  m.amt = round(m.amt,0)  
 *  if m.vatav # 0
 *    ? "Vatav : " + tran(m.vatav,'#.##')
 *  endif
 *  ? 'Amount : ' + tran(m.amt,'######.##')
   wait wind 
   clea
 
   m.ldgrname = iif(m.bk = 'CM',UPPE('Counter Sales'),uppe(m.pname))
   m.dt = ''
   =ini_txtmrgv()   && simply prepares the date in character format
  * vou1 = createobject("t9vou")
  vou1.vno = m.f1no
  kgst1.bno = m.f1no
*  kgst1.bno = vou1.vno
  kgst1.bdt = vou1.dt
  kgst1.gst = vou1.gst
  kgst1.cr_crs()
  kgst1.calc()
 * set step on
   vou1.out_vou(kgst1 ,m.ldgrname)
  =vou1.rst_vou() 
  sele tup   
Enddo
vou1.out_ftr()
vou1 = NULL 
set textmerge off
set textmerge to
* Now output the generated new masterrecords if new records have been added
if oKont.knt > 0
?
 ? "Creating the master File for New Parties......"
  oKont.outxml()
 ? "Done ................."
  
 ? "Updating the Ledgers.........."
  oKont.upd_ldgr()
 ? "Done............."
 ?
endif  

m.co = m.company
wait wind "Do you want to save the Master Data ? (Y/N)" to yn
if uppe(yn) = "Y"
  select acctldg
m.pth = "\TALLY72\DATA\" + iif(m.co = "PM","0001\","0003\")
m.pth = ""
  ? (m.pth+m.co+"acctldg.txt")
   copy to (m.pth+m.co+"acctldg.txt") type deli
endif
set path to   (oKont.cur_pth)    && reset to the old path
oKont = NULL                     && remove the object
close all

******************************************************************
*********************************FUNCTIONS START******************
******************************************************************



**********************************
Function fill_frm1
**********************************

with frm1
  .addproperty('co', '')
  .addproperty('bk', '')
  .top = 120
  .left = 10
  .height = 70
  .width = 245
  
  .addobject("cmdOK1","cmdOK")
  with .cmdOK1
   Endwith

  .addobject("cmdCanc1","cmdCanc")
  with .cmdCanc1
   Endwith
  
  
  .addobject("cboCo1",'cboCo')
  .addobject('cboBk1','cboBk')
Endwith

return

**********************************
Function rtrv_ldg(m.pth2)
**********************************

Priv rv
 if file(m.pth2+"\"+m.co+"acctldg.txt")
   create cursor acctldg ( ;
     lname c(100);
    ,lparent c(100);
     )
   append from (m.pth2 + "\" + m.co + "acctldg.txt") type deli
   rv = .t.
 else
  rv = .f.  
 endif
return rv

**********************************
Function addtomas                &&  Adds the current ledger to cotlymas.xml file
**********************************


return


******************************
Function ini_txtmrgv
******************************

m.dt = dtoc(m.f1dt,1)

return

* * * * * * * * * * *
function esc_it(st)  && Escapes the String for XML
* * * * * * * * * * *

Priv rs_str
&& checks for  XML Escape sequences and substitutes with necessary
rs_str = esc_chr(st,'&','&amp;')
rs_str = esc_chr(rs_str,'<','&lt;')
rs_str = esc_chr(rs_str,'>','&gt;')
rs_str = esc_chr(rs_str,"'",'&apos;')
rs_str = esc_chr(rs_str,'"','&quot;')

return rs_str


* * * * * * * * * * *
function esc_chr
* * * * * * * * * * *

Para str,chr,sub

j = 1
k = 1  && occurence
do while j > 0
 j = at(chr,str,k)
 str = iif(j>0,stuff(str,j,1,sub),str)
 k = k+1
enddo 
return str
*****************************************************************
*          END  OF   FUNCTIONS                                  *
*****************************************************************













*****************************************************************
*                                                               *
*                                                               *
*       C L A S S    D E F I N I T I O N S                      *
*                                                               *
*                                                               *
*****************************************************************




*********************************************
Define Class  t9vou as custom
*********************************************

voudtl=""  && Object containing voucher details
dt = ctod("")  && Date of vou
guid =""   && Useless for us
nar =""    && Naration
vtypeN =""  && Vouchertypename
vno = ""   && Voucher No
pldg = ""  && Party Ledgername
gst = ""   &&  GST Number                && as on 01/07/2017
isopt = .f. && Is optional
effdt = ctod("")  && Effective Date
iscanc = .f.    && Is cancelled
ispstdtd = .f.  && Is postdated
usetrno  = .f.  && Use tracking no
Dime ldgentlst(1)
ldgentcnt = 0 

function rst_vou()

this.ldgentcnt = 0
Dime this.ldgentlst[1] = 
this.ldgentlst = NULL
this.gst = ""
this.pldg = ""
this.vno = ""
this.nar = ""
this.dt = ctod("")
this.voudtl = ""
return

*******************
function out_vou(k_gst,pname)  &&
*******************
   if k_gst.gcnt > 1
     wait window  "Program not working for multi taxrate yet"
     return
   endif
   
    m.dt = k_gst.bdt
    m.f1no = k_gst.bno
    
    m.amt = k_gst.bamt
    m.ldgrname = m.pname
    this.dt = m.dt
    this.gst = k_gst.gst
    igstflg = !(left(this.gst,2) = '27')
    this.voudtl = "Type = 'Sales' action = 'Create'"
    this.vtypeN = "Sales"
    this.dt = m.dt
    this.vno = m.f1no
    m.esc_itLdgrname = alltrim(esc_it(m.ldgrname))
    this.pldg = m.esc_itldgrname
    this.nar = "Auto on " + dtoc(date()) + " Party : " + vou1.pldg
    m.ale = this.add_ldgent()
    m.ale.isdpos = .t.
    m.ale.ldgname = this.pldg
    m.ale.amt = -m.amt
    m.bla = m.ale.add_bau()
    m.bla.aname = m.f1no
    m.bla.atype = "New Ref"
    m.bla.amt =  -m.amt

   * set step on
   if k_gst.gcnt = 1
     m.txrt = k_gst.txrt
     m.txbl = k_gst.txbl
     m.tax =  k_gst.tax
     m.cgst = m.tax
     m.sgst = m.tax
     m.igst = k_gst.ogst[1].igst
   endif

   
    Do Case
    
    Case !igstflg
     m.ale2 = this.add_ldgent()
     m.ale2.isdpos = .f.
     *ram = k_gst.gstldgr(txrt,'L')
     *susp
     m.ale2.ldgname = k_gst.gstldgr(txrt,"L")  && Net sales as per tax rate GST
     m.ale2.amt = m.txbl
     
     m.ale3 = this.add_ldgent()
     m.ale3.ldgname = k_gst.gstldgr(m.txrt,'C') 
     m.ale3.isdpos = .f.
     m.ale3.amt = k_gst.ogst[1].cgst
     
     m.ale4 = this.add_ldgent()
     m.ale4.ldgname = k_gst.gstldgr(m.txrt,'S')
     m.ale4.isdpos = .f.
     m.ale4.amt = k_gst.ogst[1].sgst
    otherwise
     m.ale2 = this.add_ldgent()
     m.ale2.isdpos = .f.
     m.ale2.ldgname = k_gst.gstldgr(txrt,"O")  && Net sales as per tax rate GST
     m.ale2.amt = k_gst.txbl
     m.ale3 = this.add_ldgent()
     m.ale3.ldgname = k_gst.gstldgr(m.txrt,"I")
     m.ale3.isdpos = .f.
     m.ale3.amt = m.igst
    endcase 
    m.vatav =  k_gst.vatav                 &&m.amt - (m.cgst+m.igst +m.sgst + m.taxbl)
    if m.vatav # 0
      m.ale5 = this.add_ldgent()
      m.ale5.isdpos = .f.
      m.ale5.ldgname = "vatav kasar"
      m.ale5.amt = m.vatav
*      =out_vatav(m.vatav)
    endif   
     this.out_xml()  
      
return 





**********************************************
function add_dtl()


return

function add_ldgent
cnt = this.ldgentcnt+1
Dime this.ldgentlst(cnt)
 this.ldgentlst(cnt)=  createobject("ldgent")
 this.ldgentcnt = cnt


return (this.ldgentlst(cnt)) && return the object

function out_xml
* Outputs an xml string 
* in Tally 9 format
with vou1
text
<TALLYMESSAGE xmlns:UDF="TallyUDF">
<VOUCHER REMOTEID="c4aab193-f0e4-4462-8e73-304da9931651-<<padl(.vno,8,'0')>>" VCHTYPE="Sales" ACTION="Create">
<DATE><<this.dt>></DATE>
<NARRATION><<.nar>></NARRATION>
<VOUCHERTYPENAME><<.vtypeN>></VOUCHERTYPENAME>
<VOUCHERNUMBER><<.vno>></VOUCHERNUMBER>
<PARTYLEDGERNAME><<.pldg>></PARTYLEDGERNAME>
  <CSTFORMISSUETYPE /> 
  <CSTFORMRECVTYPE /> 
  <FBTPAYMENTTYPE>Default</FBTPAYMENTTYPE> 
  <VCHGSTCLASS /> 
  <DIFFACTUALQTY>No</DIFFACTUALQTY> 
  <AUDITED>No</AUDITED> 
  <FORJOBCOSTING>No</FORJOBCOSTING> 
  <ISOPTIONAL><<iif(.isopt,"Yes","NO")>></ISOPTIONAL> 
  <EFFECTIVEDATE><<.dt>></EFFECTIVEDATE> 
  <USEFORINTEREST>No</USEFORINTEREST> 
  <USEFORGAINLOSS>No</USEFORGAINLOSS> 
  <USEFORGODOWNTRANSFER>No</USEFORGODOWNTRANSFER> 
  <USEFORCOMPOUND>No</USEFORCOMPOUND> 
  <ALTERID></ALTERID> 
  <EXCISEOPENING>No</EXCISEOPENING> 
  <ISCANCELLED><<iif(.iscanc,"YES","NO")>></ISCANCELLED> 
  <HASCASHFLOW>No</HASCASHFLOW> 
  <ISPOSTDATED><<iif(.ispstdtd,"YES","NO")>></ISPOSTDATED> 
  <USETRACKINGNUMBER><<iif(.usetrno,"YES","NO")>></USETRACKINGNUMBER> 
  <ISINVOICE>No</ISINVOICE> 
  <MFGJOURNAL>No</MFGJOURNAL> 
  <HASDISCOUNTS>No</HASDISCOUNTS> 
  <ASPAYSLIP>No</ASPAYSLIP> 
  <ISDELETED>No</ISDELETED> 
  <ASORIGINAL>No</ASORIGINAL> 
endtext
endwith
For n = 1 to this.ldgentcnt
  ldgent = this.ldgentlst[n]
  * Now output each ledgerentry list
text
<ALLLEDGERENTRIES.LIST>
<LEDGERNAME><<ldgent.ldgname>></LEDGERNAME> 
<GSTCLASS /> 
<ISDEEMEDPOSITIVE><<iif(ldgent.isdpos,"YES","NO")>></ISDEEMEDPOSITIVE> 
<LEDGERFROMITEM>No</LEDGERFROMITEM> 
<REMOVEZEROENTRIES>No</REMOVEZEROENTRIES> 
<ISPARTYLEDGER>Yes</ISPARTYLEDGER> 
<AMOUNT><<ldgent.amt>></AMOUNT> 
endtext
  cnt1 = ldgent.baucnt
  if (cnt1 > 0)
    text
    <BILLALLOCATIONS.LIST>
    endtext
    for i = 1 to cnt1
      bau = ldgent.baulst[i]
      text
      <NAME><<bau.aname>></NAME> 
      <BILLCREDITPERIOD><<bau.cr >>Days</BILLCREDITPERIOD> 
      <BILLTYPE><<bau.atype>></BILLTYPE> 
      <AMOUNT><<bau.amt>></AMOUNT> 
      endtext
    Next n
    text
    </BILLALLOCATIONS.LIST>
    endtext
  endif  
  cnt2 = ldgent.taxclscnt
  if (cnt2 > 0)
    for j = 1 to cnt2
      taxcls = ldgent.taxclslst[j]
      text
      <TAXCLASSIFICATIONNAME></TAXCLASSIFICATIONNAME>
      <VATASSESSABLEVALUE></VATASSESSABLEVALUE>
      endtext
    next j
  endif
text
</ALLLEDGERENTRIES.LIST>
endtext 
Next n
text
</VOUCHER>
</TALLYMESSAGE>
endtext

return

function out_hdr()
text
<ENVELOPE>
  <HEADER>
    <TALLYREQUEST>Import Data</TALLYREQUEST> 
  </HEADER>
  <BODY>
  <IMPORTDATA>
  <REQUESTDESC>
    <REPORTNAME>All Masters</REPORTNAME> 
    <STATICVARIABLES>
      <SVCURRENTCOMPANY>KC 07-08 - (Open,Current)</SVCURRENTCOMPANY> 
    </STATICVARIABLES>
  </REQUESTDESC>
  <REQUESTDATA>
endtext

function out_ftr
text
  </REQUESTDATA>
  </IMPORTDATA>
  </BODY>
  </ENVELOPE>
endtext





*============================================
Enddefine
*============================================

*********************************************
Define Class Ldgent as custom
*********************************************

ldgname = ""
amt = 0
isdpos = .f.
ldgfrmi = .f.
rmvzent = .f.
ispldg = .f.
Dime baulst(1) 
baucnt = 0
Dime taxclslst(1)
taxclscnt = 0

***************************************
function add_bau
*Para cnt
m.cnt  = this.baucnt+1
Dime this.baulst[cnt]
this.baulst[cnt] = createobject("bau")
this.baucnt = m.cnt

return this.baulst[cnt] && return the object created

**************************************
function add_taxcls
* para cnt
m.cnt = this.taxclscnt+1
Dime this.taxclslst[cnt]
this.taxclslst[cnt] = createobject("taxcls")
this.taxclscnt = m.cnt

return this.taxclslst[cnt]


*============================================
Enddefine && ldgent
*============================================



*********************************************
Define Class bau as custom   && Bill Allocation Unit
*********************************************
aname = ""
cr = 0
 atype = ""
amt = ""

*============================================
Enddefine &&bau
*============================================


*********************************************
Define Class taxcls as custom  && Vat Classification *
*********************************************

tname = ""
tval = 0

*============================================
Enddefine && taxcls
*============================================

* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
* 
* CLASESS from  GENTMAS  for generating TallyMas Account XML
*
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

*===========================================================================
Define class ktlym as custom   && container class for tlymas
*===========================================================================

* * * * * * * * * * * * * * * * * *
*                                 *
*         P R O P E R T I E S     *
*                                 *
* * * * * * * * * * * * * * * * * *


  knt = 0             && total number of tlymas obj
  Dime kntlst[1]      &&
  ofname=""           && Output XML File name to write the xml
  ldgfname = ''       && the ledger file which needs to be updated
  otlym = NULL        && object of tlymas              
  co = 'PM'           && company code
  frmRd = NULL        && form to read the items
  cur_pth = ""        && tmp path to store the current path
  

* * * * * * * * * * * * * * * * * *
*                                 *
*         M E T H O D S           *
*                                 *
* * * * * * * * * * * * * * * * * *

* * * * * * * * * *
function init     &&
* * * * * * * * * * 

 this.cur_pth = set("path")
 set path to (this.cur_pth + ";x:\tally72\data\0001;x:\tally72\data\0003")
 do form frmtlymas name this.frmRd noshow  && set the frnRd

* * * * * * * * * * *
function gen_fname  && makes the necessary filenames
* * * * * * * * * * *

  this.ofname = this.co + "tlymasout.xml"
  this.ldgfname = this.co + "acctldg.txt"
 * modi comm (this.ldgfname) 
return


* * * * * * * * * * *  
function outxml     &&
* * * * * * * * * * *
*set step on

 Priv i,obj
 m.obj = this.otlym
 with this
 .opn_out()
 for i = 1 to .knt
   m.obj = .kntlst[i]
   this.otlym = m.obj
   .rd_tlym()

   if !m.obj.pmvat == ""
     m.obj.epname = m.obj.esc_it(m.obj.pname)
     m.obj.adar = m.obj.esc_it(m.obj.adar)
     .out_ldg(m.obj.epname,"Sundry Debtors",m.obj.pmvat,m.obj.adar)
   endif  
 next i
 .cls_out()
 endwith
return


* * * * * * * * * * *
function upd_ldgr  && Updates the ledger used during outxml
* * * * * * * * * * *
*set step on
 acti scree
 Priv fh,i,j
 fh = fopen( this.ldgfname,12) && Open the file for writing
 =fseek(fh,0,2) && move the file to the end of the file
 for i = 1 to this.knt
   this.otlym = this.kntlst[i]
   if !this.otlym.pmvat == ""
    j= this.otlym.pname + iif( fputs(fh,'"'+this.otlym.pname+'","Sundry Debtors"')> 0 ,"Added ","Could not Add")+ " In List"
    ? j
    if Right(j,5) # "Added"
      insert into acctldg (lname,lparent) values (this.otlym.pname,"Sundry Debtors") 
    endif
   endif 
 next i   
 =fclose(fh)
return


* * * * * * * * * * * * * *
function add_tlm(pn,pa,gs)    && adds a tallymas object
* * * * * * * * * * * * * *
if ascan(this.kntlst,pn) = 0  && check if name doesnt already exist in the array for adding to master Add if not exist
 Priv n
 n = this.knt+1

 Dime this.kntlst[n]
 this.kntlst[n] = createobject("tlymas")
 this.otlym = this.kntlst[n]
 this.otlym.pname = pn
 this.otlym.paddr = pa
 this.otlym.pmvat  =gs
 this.knt = n
endif
 return

* * * * * * * * * * * 
 function rd_tlym   && reads the using frmRd
* * * * * * * * * * *

  Priv i,j , ttl
  m.p_addr = this.otlym.paddr
    m.obj = this.otlym
  ttl = memlines(m.p_addr)
  For i = 1 to 4 
    m.addr[i] = mline(m.p_addr,i)
    if at("VAT",m.addr[i])> 0
      m.vattxt = m.addr[i]
      m.vattin = obj.pars_vatno(m.vattxt)
    endif
    m.vattin = m.obj.pmvat
  next i

  with this.frmRd
  .lblConame.caption = obj.pname
  .edtAddr.controlsource = "m.p_addr"
  .txtVat.controlsource  = "m.vattin"
  .txtAd1.controlsource = "m.addr[1]"
  .txtAd2.controlsource = "m.addr[2]"
  .txtAd3.controlsource = "m.addr[3]"
  .txtAd4.controlsource = "m.addr[4]"
  .show()
  Endwith
  read events
  if this.frmRd.ok
    obj.pname = alltrim(obj.pname)
    obj.pmvat = alltrim(m.vattin)
    i = 1
    j = 1
    for j = 1 to 4
      dime obj.adar[i]
      if len(alltrim(m.addr[j]))> 0
        obj.adar[i] = alltrim(m.addr[j])
        i = i+1
      endif
    next j     
  else
    obj.vat = ""
  endif  
 return

 function Opn_out  && send the premable for master import
  set textmerge on to (this.ofname) noshow
Text
<ENVELOPE>
<HEADER>
<TALLYREQUEST>Import Data</TALLYREQUEST>
</HEADER>
<BODY>
<IMPORTDATA>
<REQUESTDESC>
<REPORTNAME>All Masters</REPORTNAME>
<STATICVARIABLES>
<SVCURRENTCOMPANY>X</SVCURRENTCOMPANY>
</STATICVARIABLES>
</REQUESTDESC>
Endtext
 set textmerge off
 return
 
* * * * * * * * * * * 
function out_ldg    && 
* * * * * * * * * * *

Para pname,parnt,vatno,adrar  && party name , parent ldg name  , vat Number and address array
* set step on
set textmerge on to (this.ofname) addi noshow
Text
<TALLYMESSAGE xmlns:UDF="TallyUDF">
<LEDGER NAME="<<m.pname>>" RESERVEDNAME="">
<NAME.LIST>
<NAME><<m.pname>></NAME>
</NAME.LIST>
<ADDITIONALNAME><<m.pname>></ADDITIONALNAME><ISINTERESTON>No</ISINTERESTON>
 <ADDRESS.LIST>
Endtext
For N = 1 to alen(m.addr)
  m.ad =esc_it(alltrim(addr[n]))
  if !empt(m.ad)
    \ <ADDRESS><<m.ad>></ADDRESS>
  endif
next n
Text
  </ADDRESS.LIST>
<VATTINNUMBER><<vatno>></VATTINNUMBER>
<PARENT><<m.parnt>></PARENT>
<ISBILLWISEON>YES</ISBILLWISEON>
<ISCOSTCENTRESON>No</ISCOSTCENTRESON>
<AFFECTSSTOCK>No</AFFECTSSTOCK>
<ISCONDENSED>No</ISCONDENSED>
<SORTPOSITION> 1000</SORTPOSITION>
<INTERESTONBILLWISE>No</INTERESTONBILLWISE>
<OVERRIDEINTEREST>No</OVERRIDEINTEREST>
<OVERRIDEADVINTEREST>No</OVERRIDEADVINTEREST>
<FORPAYROLL>No</FORPAYROLL>
<PAYTYPE/>
<SHOWINPAYSLIP>No</SHOWINPAYSLIP>
<ASSLABRATE>No</ASSLABRATE>
<TDSDEDUCTEEISSPECIALRATE>No</TDSDEDUCTEEISSPECIALRATE>
<TDSDEDUCTEEISCACERTIFIED>No</TDSDEDUCTEEISCACERTIFIED>
<ISTDSAPPLICABLE>No</ISTDSAPPLICABLE>
<USEFORVAT>No</USEFORVAT>
<IGNORETDSEXEMPT>No</IGNORETDSEXEMPT>
<TDSDEDUCTEETYPE/>
<TAXTYPE/>
<TAXCLASSIFICATIONNAME/>
<TDSRATENAME/>
<FORSERVICETAX>No</FORSERVICETAX>
<ISINPUTCREDIT>No</ISINPUTCREDIT>
<ISEXEMPTED>No</ISEXEMPTED>
<SERVICECATEGORY/>
</LEDGER>
</TALLYMESSAGE>

endtext
set textmerge off 
return


* * * * * * * * * * *
function cls_out()  &&
* * * * * * * * * * *

set textmerge on to (this.ofname) addi noshow
Text
</IMPORTDATA>
</BODY>
</ENVELOPE>
endtext
set textmerge off  

return

enddefine

Define class tlymas as custom
pname = ''         &&  Party name
epname = ''        &&  Escaped Party name
pmvat = spac(15)   &&  Parties Vat No
paddr = ''         && parties address
oxml = ''          && this will contain the xmlstring
dime adar[1]       && address array


* * * * * * * * * * *
function esc_it(st)  && Escapes the String for XML
* * * * * * * * * * *

Priv rs_str
&& checks for  XML Escape sequences and substitutes with necessary
rs_str = this.esc_chr(st,'&','&amp;')
rs_str = this.esc_chr(rs_str,'<','&lt;')
rs_str = this.esc_chr(rs_str,'>','&gt;')
rs_str = this.esc_chr(rs_str,"'",'&apos;')
rs_str = this.esc_chr(rs_str,'"','&quot;')

return rs_str


* * * * * * * * * * *
function esc_chr
* * * * * * * * * * *

Para str,chr,sub

j = 1
k = 1  && occurence
do while j > 0
 j = at(chr,str,k)
 str = iif(j>0,stuff(str,j,1,sub),str)
 k = k+1
enddo 
return str



* * * * * * * * * * *
function pars_vatno(j)  && parse the VAT No in the string and return it
* * * * * * * * * * *

 Priv l,i,rv,o
 rv = ""
 l = len(j)
 for i = 1 to l
   o = subs(j,i,1)
   rv = rv + iif(isdig(o),o,"")
   if len(rv) = 11
      exit
   endif
 next i 
 return rv 
  
Enddefine




Define Class kGst as custom   && Container object for discounts and gst objects
bno = ""                      && Bill No
bdt = ctod("")                && Bill Date
bamt = 0                      && Bill Amount
txbl  = 0                     && Bill Taxable amt
tax   = 0                     && Bill taxes
txrt = 0                      && Bill Tax Rate
vatav = 0                     && Vatav 
gst  = ""                     && GST no of party
iflg = .f.                    && Flag for IGST
Dime ogst[1] = NULL           && Array of  GST objects
Dime oDed[1] = NULL           && Array of  Ded objects (discount objects)
gcnt = 0                       && Count of GST objects
dcnt = 0
ftup = ""                     && File name of the top file
fitm = ""                     && File name of item.


function init
if file("taxldgrcd.csv")
 create cursor txldgrcd ( tx c(2), ldgname c(50),al c(2),igf L ,sflg L,sl c(1))
 append from taxldgrcd.csv type deli fields tx,ldgname,al,igf,sflg,sl
else
 wait window "'taxldgrcd.csv' file doesn't exists in path,  Cannot Continue"
 return .f.

endif
return .t.
 

function cr_crs
 select gtp.type,iif(gtp.type  $ 'INVITM' ,(gtp.qty* rate), gtp.amt) as amt, gtp.qty,gtp.rate,gtp.dsc,gtp.tax from (this.fitm) as gtp ;
  into cursor gtmpi  ;
  where this.bno == gtp.bno  
  this.iflg = !( left(this.gst,2) = "27")
  Dime kgst1.ogst[1] = NULL
  Dime kgst1.oDed[1] = NULL 
  store 0 to bamt,txbl,tax,vatav,gcnt,dcnt
return


function pop_ded &&  populate the deduction array
  priv n,i,obj
  select gtmpi.dsc, gtmpi.amt, sum(gtmpi.amt*gtmpi.dsc/100) from gtmpi into array de1 where gtmpi.dsc # 0 group by gtmpi.dsc
  n = _tally
  if n = 0
   this.dcnt = 0
   return
  endif
  for i = 1 to n
    dime this.oded[i] 
    this.oded[i] =  createobject("ded")
    obj = this.oded[i]
    obj.rate = de1[i,1]
    obj.amt = de1[i,2] 
    obj.ded = de1[i,3]
  next i
  this.dcnt = n   
  return
  


 function pop_gst  && Populate the gst array
  private obj,n,i,k
  m.iflg = this.iflg
  ifac = iif(m.iflg,1,.5)
  select gtmpi.tax, sum((1+gtmpi.dsc/100)*gtmpi.amt),sum((1+gtmpi.dsc/100)*gtmpi.amt)* gtmpi.tax/100 from gtmpi  into array tx1 group by gtmpi.tax where tax # 0
  n = _tally
  if n = 0
   this.gcnt = 0
   return
  endif
  if m.iflg   && Process for IGST
    for i = 1 to n
      Dime this.ogst[i] 
      this.ogst[i] = createobject("gst")
      m.obj = this.ogst[i]
      obj.txrt = tx1[i,1]
      obj.txbl = round(tx1[i,2],2) 
      obj.igst = round(tx1[i,3],0)
    Next i
    this.gcnt = n
  else  && process for SGST & CGST
    k = 0
    For  i = 1 to n
*      if tx1[i,1] == 0
*        loop
*      endif
      k = k +1
      Dime this.ogst[k]
      this.ogst[k] = createobject("gst")
      m.obj = this.ogst[k]
      obj.txrt = tx1[k,1]/2
      obj.txbl = round(tx1[k,2],2)
      obj.cgst = round(tx1[k,3]/2,0)
      obj.sgst = obj.cgst
    next i  
  Endif
  
  
  this.gcnt = n   

    this.tax = iif(n = 1,obj.sgst,0)
    this.txbl = iif(n= 1,obj.txbl,0)
    this.txrt = iif(n= 1,obj.txrt,0)
   
    
  return
  

function calc
this.pop_ded()
this.pop_gst()

Priv ttl,sttl,dabl,deda,gabl,gtax
priv m.igst,m.cgst,m.cgst,m.iflg
priv txrtstr 

m.iflg = this.iflg
store 0 to dabl,deda, sttl
for n = 1 to this.dcnt
 obj = this.oded[n]
 dabl  = dabl + obj.amt
 deda  = deda + abs(obj.ded)
 
next  n
store 0 to gabl,gtax,m.igst,m.sgst,m.cgst
store "" to txrtstr


For n = 1 to this.gcnt
  obj = this.ogst[n]
  gabl = gabl + obj.txbl
  m.igst = m.igst + obj.igst
  m.sgst = m.sgst + obj.sgst 
  m.cgst = m.cgst + obj.cgst
  txtstr = str(obj.txrt)                       &&&alltrim(str(obj.txrt*iif(m.iflg,1,.5)))
next n  

select sum(gtmpi.amt)  from gtmpi into array gtar
sttl = gtar[1]
m.igst = round(m.igst,0)
m.sgst = round(m.sgst,0)
m.cgst = m.sgst
*if !this.iflg
*  gtax  = gtax
*  m.igst = 0
*  m.sgst = round(gtax,0)
*  m.cgst = m.sgst
*else
*  m.igst = round(m.gtax,0)
*  store 0 to m.cgst,m.sgst
*endif
m.gttl = round(m.sttl  - m.deda + m.igst + m.cgst+ m.sgst,2)
m.vatav = round(gttl,0) - m.gttl
this.bamt = round(m.gttl,0)
this.vatav = round(m.vatav,2)

return


Function gstLdgr(tr,cat)  &&   get the GST ledger based on the various criteria
                          && tr is  tax Rate duly stringyfied
                          && cat  various    for   type of ledger   L= local sale ; O = OMS sale ; C = CGST S = SGST I = IGST
priv rs

tsrch = tran(tr,'@l ##')
cat = iif(cat $ "SLOCSVEI",cat,"")
if this.iflg 
 sele * from txldgrcd where tx == tsrch .and. sl == m.cat .and. sflg .and. igf into array rs
else
 sele * from txldgrcd where tx == tsrch .and. sl == m.cat .and.sflg .and. !igf into array rs
endif
 * susp
return rs[1,2]



set textmerge on
text

===============================================
  Bill no : <<this.bno>>  Bill Date <<this.bdt>>   Bill Amt <<this.bamt>>
  
  Subtotal .....<<m.sttl >>
  Discount on Rs <<m.dabl  >>      Rs <<m.deda*-1 >>
  Taxes
  
  IGST  @ <<iif(!iflg,"",txtstr)>>%  on Rs <<m.gabl*iif(m.iflg,1,0)>>  ==== <<m.igst >>
  SGST  @ <<iif(iflg,"",txtstr)>>%  on Rs <<m.gabl*iif(m.iflg,0,1)>> ====  <<m.sgst >>
  CGST  @ <<iif(iflg,"",txtstr)>>% on  Rs <<m.gabl*iif(m.iflg,0,1)>> ====  <<m.cgst >>
  Rounding off                      <<m.vatav >>
  Total Bill Amt                    <<round(m.gttl,0) >>
 
 ================================================ 
endtext
set textmerge off
*set step on
return

enddefine


Define Class gst as custom
txbl =  0
igst =  0
cgst =  0
sgst =  0
txrt =  0



enddefine

Define class ded as custom
 amt = 0
 rate = 0
 ded = 0
 
enddefine 


*------------------------------------------------------------------------------



*  Classes from outsdf2

Define Class cboBk as  Combobox
    height = 24
    width = 60
    top = 12
    left =  120
    rowsourcetype = 1
    visible = .t.
    rowsource = 'CM,BILL'
    TabIndex = 2
    controlsource = "bk"
    
Enddefine

Define Class cboCo as ComboBox
    height = 24
    width = 60
    top = 12
    left =  24
    rowsourcetype = 1
    visible = .t.
    rowsource = 'PM,KC'
    TabIndex = 1
    controlsource = "co"
Enddefine

Define Class cmdCanc as Commandbutton
    height = 30
    width = 50
    top = 48
    left =  156
    caption = "Cancel"
    Cancel = .t.
    visible = .t.
    TabIndex = 4
  Proc Click
    m.co = ""
    m.bk = ""
    thisform.hide
    clear events
    return  
  Endproc  
Enddefine

Define Class cmdOk as commandbutton
    height = 30
    width = 50
    top = 48
    left = 60   && 156
    caption = "OK"
    default = .t.
    visible = .t.
    TabIndex = 3
 Proc click
   thisform.hide
   clear events
   return
 Endproc

Enddefine
