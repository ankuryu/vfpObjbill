* Filename ochqxml1.prg
* As on  29May2017
* Uses the cheq file  Cheq.dbf  and generates an XML file for import into Tally9
* Outputs file XXOutChq.xml
* 

* File Out XML
* Reqwuires the  Cheq.dbf and master.dbf for master accounts and billspayable.txt
* master and billspayable are generated from xml file
* 

text
- <TALLYMESSAGE xmlns:UDF="TallyUDF">
- <VOUCHER REMOTEID="5ac20ae0-d3f7-11da-ae64-000d88457808-00009a59" VCHTYPE="Payment" ACTION="Create">
- <BANKERSDATE.LIST>
  <BANKERSDATE>20160331</BANKERSDATE> 
  </BANKERSDATE.LIST>
  <DATE>20160331</DATE> 
  <GUID>5ac20ae0-d3f7-11da-ae64-000d88457808-00009a59</GUID> 
  <NARRATION>Ch. No. :INTEREST FOR THE PRIOED 29/2/2016 TO 30/03/2016</NARRATION> 
  <VOUCHERTYPENAME>Payment</VOUCHERTYPENAME> 
  <VOUCHERNUMBER>590</VOUCHERNUMBER> 
  <PARTYLEDGERNAME>AMCO BANK (HYPO) 20229 (8.LAK)</PARTYLEDGERNAME> 
  <CSTFORMISSUETYPE /> 
  <CSTFORMRECVTYPE /> 
  <FBTPAYMENTTYPE>Default</FBTPAYMENTTYPE> 
  <VCHGSTCLASS /> 
  <DIFFACTUALQTY>No</DIFFACTUALQTY> 
  <AUDITED>No</AUDITED> 
  <FORJOBCOSTING>No</FORJOBCOSTING> 
  <ISOPTIONAL>No</ISOPTIONAL> 
  <EFFECTIVEDATE>20160331</EFFECTIVEDATE> 
  <USEFORINTEREST>No</USEFORINTEREST> 
  <USEFORGAINLOSS>No</USEFORGAINLOSS> 
  <USEFORGODOWNTRANSFER>No</USEFORGODOWNTRANSFER> 
  <USEFORCOMPOUND>No</USEFORCOMPOUND> 
  <ALTERID>71506</ALTERID> 
  <EXCISEOPENING>No</EXCISEOPENING> 
  <ISCANCELLED>No</ISCANCELLED> 
  <HASCASHFLOW>Yes</HASCASHFLOW> 
  <ISPOSTDATED>No</ISPOSTDATED> 
  <USETRACKINGNUMBER>No</USETRACKINGNUMBER> 
  <ISINVOICE>No</ISINVOICE> 
  <MFGJOURNAL>No</MFGJOURNAL> 
  <HASDISCOUNTS>No</HASDISCOUNTS> 
  <ASPAYSLIP>No</ASPAYSLIP> 
  <ISDELETED>No</ISDELETED> 
  <ASORIGINAL>No</ASORIGINAL> 
- <ALLLEDGERENTRIES.LIST>
  <LEDGERNAME>INT. On O.D. (A/C AMCO HYPO.[20229.]</LEDGERNAME> 
  <GSTCLASS /> 
  <ISDEEMEDPOSITIVE>Yes</ISDEEMEDPOSITIVE> 
  <LEDGERFROMITEM>No</LEDGERFROMITEM> 
  <REMOVEZEROENTRIES>No</REMOVEZEROENTRIES> 
  <ISPARTYLEDGER>No</ISPARTYLEDGER> 
  <AMOUNT>-932.00</AMOUNT> 
  </ALLLEDGERENTRIES.LIST>
- <ALLLEDGERENTRIES.LIST>
  <LEDGERNAME>AMCO BANK (HYPO) 20229 (8.LAK)</LEDGERNAME> 
  <GSTCLASS /> 
  <ISDEEMEDPOSITIVE>No</ISDEEMEDPOSITIVE> 
  <LEDGERFROMITEM>No</LEDGERFROMITEM> 
  <REMOVEZEROENTRIES>No</REMOVEZEROENTRIES> 
  <ISPARTYLEDGER>Yes</ISPARTYLEDGER> 
  <AMOUNT>932.00</AMOUNT> 
  </ALLLEDGERENTRIES.LIST>
  </VOUCHER>
  </TALLYMESSAGE>
endtext



close data
clea all
set date brit
set century on

set excl off
set conf on





 

 m.ofil = "tmp.xml"
 set textmerge to (m.ofil)
 set textmerge on
use cheq && 
go bott

* use the latest payable file and populate it.
create cursor pybls  (dt c(12),vno c(40),pname c(60),amt n(12,2),dudt c(12),ddays c(4))
append from x:\tally72\data\0001\pmpybl.txt type deli

bnkv1 = createobject("bnkvou")   && create the bank voucher
vou1 = createobject("t9vou")
Dime bnkname[5,2]
bnkname[1,1] =  "AMCOFD02"            && AMCOHYPO
bnkname[1,2] =  "AMCO BANK (HYPO) 20229 (8.LAK)"
bnkname[2,1] =  "AMCOHYPO"            && AMCOFD01
bnkname[2,2] =  "AMCO BANK F/D.O/D 30511.[7.44]"
bnkname[3,1] =  "CBIFD01"            && CBIFD01
bnkname[3,2] =  "CENTRAL BANK OF INDIA 1162091145."
bnkname[4,1] =  "UBIFD"               && UBIFDOD
bnkname[4,2] =  "UBI (FD/OD) A/c 318404010132181"
bnkname[5,1] =  "UBICA"           && UBICA
bnkname[5,2] =  "Union Bank of India. C/A  318401010136876"


*oKont = createobj("ktlym")  && create the master account container object
*oKont.co = m.co             && set the company name.
*oKont.gen_fname()           && generate the filename for xml
*vou1.out_hdr()

****************************THE LOOP****************************************

Do while .t.
  sele cheq
  brow nomo 
  if lastkey() = 27
     exit
  endif

  scatter memvar
  * search for  the party existing
  * =ini_txtmrgv()   && simply prepares the date in character format
  * set step on
  *vou1 = createobject("t9vou")
   =out_vou()
   
  sele cheq  
Enddo

*****************************  EndofMAINLLOOP ******************************

vou1.out_ftr()
vou1 = NULL 
set textmerge off
set textmerge to

set path to   (oKont.cur_pth)    && reset to the old path
oKont = NULL                     && remove the object
close all


*************************************************************************
*                     MAIN   LOOP    ENDS  HERE                         *
*************************************************************************


******************************
Function out_vou
******************************


vou1.voudtl = "Type = 'Payment' action = 'Create'"
                 No
vou1.isopt = .f.  && is optional

vou1.vtypeN = "Payment"
vou1.dt = m.dt

vou1.dt = m.dt

vou1.iscanc = .f.

vou1.usetrno = .f.

vou1.ispstdtd = .f.

vou1.vno = m.f1no

m.esc_itLdgrname = alltrim(esc_it(m.ldgrname))
vou1.pldg = m.esc_itldgrname
vou1.nar = "Auto on " + dtoc(date())+ " Party : "+ esc_it(m.pname)
* Now for each Entry send the All ledger Entry

 m.ale = vou1.add_ldgent()

 m.ale.isdpos = .t.

m.esc_pname = esc_it(upper(m.pname))  && change to upper name and then esc it else the esc char get changed to upper making it invalid
* m.ale.ldgname = iif(m.bk = 'CM',UPPE('Counter Sales'),m.esc_pname) &&'Counter Sales'  && this is the Ledger name
* ale[3] = -m.amt   && amount of the ledger 
 m.ale.amt = -m.amt 
 

m.bla =  m.ale.add_bau()

m.bla.aname = m.f1no

m.bla.atype = 'New Ref'

m.bla.amt = - m.amt



 

* send a blank carraige return to seperate the next entry
*\
vou1.out_xml()
return && out_vou




******************************
Function ini_txtmrgv
******************************

m.dt = dtoc(m.f1dt,1)

return

******************************
function esc_it  
******************************
para st
Priv rs_str
&& checks for  XML Escape sequences and substitutes with necessary
rs_str = esc_chr(st,'&','&amp;')
rs_str = esc_chr(rs_str,'<','&lt;')
rs_str = esc_chr(rs_str,'>','&gt;')
rs_str = esc_chr(rs_str,"'",'&apos;')
rs_str = esc_chr(rs_str,'"','&quot;')

return rs_str  && resturns the Str converted with Esc Seq for XML

**********************************
function esc_chr
**********************************
* used by  esc_it
*
Para str,chr,sub

j = 1
k = 1  && occurence
do while j > 0
 j = at(chr,str,k)
 str = iif(j>0,stuff(str,j,1,sub),str)
 k = k+1
enddo 
return str



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
Function rtrv_ldg()
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


******************************************************
Define   Clase    bnkvou   as    custom
******************************************************


co = ""       && company code
dt = ctod("") && Cheque Data
bnk= ""   && bankcode
chqno = "" && Chequeno
amt = 0    &&  Amount
nar = ""   && Narration
apflg = .f. && ac payee
brflg = .f. && bearer flag
pflg =  .f. &&
Vouno = ""  && Voucher Number
voutxt = "" && Text of the voucher
dime acldg[1]  &&  Dim of account ledgers affected
pd  = .f.  && PD flg
t9vou = NULL  &&  tally voucher for exporting
chqfname = "" && cheq file name


function get_chq(bnkc, chqn)  && bankcode and chqno 
* gets the cheq details into the  object
 Priv ar1,rv
 select co,name,dt,bnk,chqno,amt,nar,db_ac from (this.chqfnam) as chq where m.bnkc = chq.bnk .and. chq.chqno == this.chqno into array ar1
 if _tally = 1  && only one entry should be there
   with this
    .co       = ar1[1] && co
    .party    = ar1[2] && pname
    .dt       = ar1[3] && dt
    .bnk      = ar1[4] && bankcode
    .chqno    = ar1[5] && chqno
    .amt      = ar1[6] && amt
    .nar      = ar1[7] && nar
    .acldg[1] = ar1[8] && debit a/c
   endwith
   rv = .t.
 else
   wait window "invalid data giving it a miss"
 endif
return  rv
 
 
 
function gen_t9vou  && generates the t9vou using bankvou details
with t9
 .voudtl = 
 .dt = this.dt
 .vtypN = "Payment"
 .vno = this.chqno
 .pldg = this.acldg[1] 
 .isopt =
 .effdt =
 .iscanc = 
 .ispstdtd =
 .usetrno =
endwith
 
 


function upd_chq
* updates the tally xml data generated into the cheqe as well as the narration
* and the voucher text

function gentxml  
* generates the tally xml  data for importing to t9



*+++++++++++++++++++++++++++++++++++++++++++++++++
enddefine 
*+++++++++++++++++++++++++++++++++++++++++++++++++



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
isopt = .f. && Is optional
effdt = ctod("")  && Effective Date
iscanc = .f.    && Is cancelled
ispstdtd = .f.  && Is postdated
usetrno  = .f.  && Use tracking no
Dime ldgentlst(1)
ldgentcnt = 0 

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
<VOUCHER REMOTEID="c4aab193-f0e4-4462-8e73-304da9931651-000026de" VCHTYPE="Sales" ACTION="Create">
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
     .out_ldg(m.obj.epname,"Sundry Debtors",m.obj.pmvat,m.obj.adar)
   endif  
 next i
 .cls_out()
 endwith
return


* * * * * * * * * * *
function upd_ldgr  && Updates the ledger used during outxml
* * * * * * * * * * *

 acti scree
 Priv fh,i,j
 fh = fopen( this.ldgfname,12) && Open the file for writing
 =fseek(fh,0,2) && move the file to the end of the file
 for i = 1 to this.knt
   this.otlym = this.kntlst[i]
   if !this.otlym.pmvat == ""
    j= this.otlym.pname + iif( fputs(fh,'"'+this.otlym.pname+'","Sundry Debtors"')> 0 ,"Added ","Could not Add")+ " In List"
    ? j
   endif 
 next i   
 =fclose(fh)
return


* * * * * * * * * * * * * *
function add_tlm(pn,pa)    && adds a tallymas object
* * * * * * * * * * * * * *

 Priv n
 n = this.knt+1

 Dime this.kntlst[n]
 this.kntlst[n] = createobject("tlymas")
 this.otlym = this.kntlst[n]
 this.otlym.pname = pn
 this.otlym.paddr = pa
 
 this.knt = n
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
  m.ad =alltrim(addr[n])
  if !empt(m.ad)
    \ <ADDRESS><<m.ad>></ADDRESS>
  endif
next n
Text
  </ADDRESS.LIST>
<VATTINNUMBER><<vatno>></VATTINNUMBER>
<PARENT><<m.parnt>></PARENT><CURRENCYNAME>Rs.</CURRENCYNAME>
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
pmvat = spac(12)   &&  Parties Vat No
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


enddefine
