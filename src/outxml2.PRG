* Filename outxml2.prg
* As on  19Apr2016
* Changes made to outxml.prg to take into account 5.5% MVAT TAx for SAles effective from 01/04/2016

* File Out XML
* uses the invoice/ cm files to 
* generate an XML file
* for importing into Tally9





* Program to output SDF file for Tally 72
*set udf 
* Incorporates CST @ 3%  also
* Also allows Bill Entry
* Master imported from  pmmaster.sdf from tally72 directory
close data
clea all
set date brit
set century on

set excl off
set conf on


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
m.pth = "\TALLY72\DATA\" + iif(m.co = "PM","0001\","0003\")+ 'master.sdf'
m.pth2 = "\TALLY72\DATA\" + iif(m.co = "PM","0001\","0003\")
* Now get the master ledgers of Tally72 for use with Accounts
close data
wait window "Import New Ledger Data ? (Y/N)" to yn
if uppe(yn) = 'Y'
  do tly7ldg with "Sundry Debtors" ,m.pth && gives a list of accounts in acctldg and path
else
  if !rtrv_ldg()
    wait window "Master File Does Not Exist !  IMport Data and TRY"
    return
  endif  
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



*m.vou = 'VO'
*m.ki = '000001
*m.lvl = '1'
*m.voutype = 'Sales'
*close data
sele 0
use (m.ftop) alia tup
go bott
vou1 = createobject("t9vou")
vou1.out_hdr()
Do while .t.
  sele tup
  brow nomo fields f1no,f1dt,pname,famt
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
        m.pname = "New Party"
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
   select * from (m.fitm) as itm ;
   left join  (m.ftop) as tup on tup.f1no = itm.bno ;
   where  itm.bno = m.f1no ;
   into cursor frm1
   
   select sum(amt*(1+dsc/100)),tax,sum(tax/100*amt*(1+dsc/100)) ;
   from frm1 into arra vl ;
   group by tax ;
   where frm1.type # 'NAR'

   if type('vl') # 'N'
     wait window "Error , Looping Back"
     loop
   endif
   txcat = alen(vl,1)
   m.amt = 0

   ? iif(m.bk = "CM","Cash Memo ","Bill No : ") + m.f1no
   ?? ' Dt ' + dtoc(m.f1dt)
   ?? iif(m.bk = "CM","Counter Sale ",' Party : ') + m.pname

   ? ' Amount     Tax Rate   Tax'
      
   For n = 1 to txcat
      vl[n,1] = round(vl[n,1],2)
      vl[n,3] = round(vl[n,3],2)
     m.amt = m.amt + vl[n,1]+ round(vl[n,3],2)
    ? tran(vl[n,1],'#######.##') + spac(4)+ tran(vl[n,2],'##%') + spac(1) + tran(vl[n,3],'#######.##')
   Next n
   m.vatav = round(m.amt,0)-m.amt
   m.amt = round(m.amt,0)  
   if m.vatav # 0
     ? "Vatav : " + tran(m.vatav,'#.##')
   endif
   ? 'Amount : ' + tran(m.amt,'######.##')
   wait wind 
   clea
   m.ldgrname = iif(m.bk = 'CM',UPPE('Counter Sales'),uppe(m.pname))
   m.dt = ''
   =ini_txtmrgv()   && simply prepares the date in character format
  * set step on
  vou1 = createobject("t9vou")
   =out_vou()
   
  sele tup   
Enddo
vou1.out_ftr()
vou1 = NULL 
set textmerge off
set textmerge to
m.co = m.company
wait wind "Do you want to save the Master Data ? (Y/N)" to yn
if uppe(yn) = "Y"
  select acctldg
m.pth = "\TALLY72\DATA\" + iif(m.co = "PM","0001\","0003\")
  ? (m.pth+m.co+"acctldg.txt")
   copy to (m.pth+m.co+"acctldg.txt") type deli
endif

close all

Function out_vou
vou1.voudtl = "Type = 'Sales' action = 'Create'"
*\\U<<m.vou>><<m.ki>><<m.lvl>>                              
*text
*U<<m.vou>><<m.ki>><<m.lvl>>Is Optional                   No
vou1.isopt = .f.  && is optional

*U<<m.vou>><<m.ki>><<m.lvl>>UseForGainLoss                No
*U<<m.vou>><<m.ki>><<m.lvl>>UseForCompound                No
*U<<m.vou>><<m.ki>><<m.lvl>>Voucher Type Name             <<M.voutype>>
vou1.vtypeN = "Sales"
*U<<m.vou>><<m.ki>><<m.lvl>>Date                          <<m.dt>>
vou1.dt = m.dt
*U<<m.vou>><<m.ki>><<m.lvl>>Effective Date                <<m.dt>>
vou1.dt = m.dt
*U<<m.vou>><<m.ki>><<m.lvl>>Is Cancelled                  No
vou1.iscanc = .f.
*U<<m.vou>><<m.ki>><<m.lvl>>Use Tracking Number           No
vou1.usetrno = .f.
*U<<m.vou>><<m.ki>><<m.lvl>>Is Post Dated                 No
vou1.ispstdtd = .f.
*U<<m.vou>><<m.ki>><<m.lvl>>Is Invoice                    No
*U<<m.vou>><<m.ki>><<m.lvl>>Diff Actual Qty               No
*U<<m.vou>><<m.ki>><<m.lvl>>Voucher Number                <<m.f1no>>
vou1.vno = m.f1no
*U<<m.vou>><<m.ki>><<m.lvl>>Party Ledger Name             <<m.ldgrname>>
m.esc_itLdgrname = alltrim(esc_it(m.ldgrname))
vou1.pldg = m.esc_itldgrname
*U<<m.vou>><<m.ki>><<m.lvl>>As Payslip                    No
*U<<m.vou>><<m.ki>><<m.lvl>>Narration                     Auto on <<date()>> Party : <<m.pname>>
vou1.nar = "Auto on " + dtoc(date())+ " Party : "+ esc_it(m.pname)
*Endtext
*U<<m.vou>><<m.ki>><<m.lvl>>GUID                          5ac20ae0-d3f7-11da-ae64-000d88457808-000014c0
*U<<m.vou>><<m.ki>><<m.lvl>>AlterID                        8250

* Now for each Entry send the All ledger Entry
*Dime ale[3] && All ledger Entries array
 m.ale = vou1.add_ldgent()

* ale[1] = .t.  && Is deemed positive = .t.
 m.ale.isdpos = .t.
* ale[2] = iif(m.bk = 'CM',UPPE('Counter Sales'),uppe(m.pname)) &&'Counter Sales'  && this is the Ledger name
m.esc_pname = esc_it(upper(m.pname))  && change to upper name and then esc it else the esc char get changed to upper making it invalid
 m.ale.ldgname = iif(m.bk = 'CM',UPPE('Counter Sales'),m.esc_pname) &&'Counter Sales'  && this is the Ledger name
* ale[3] = -m.amt   && amount of the ledger 
 m.ale.amt = -m.amt 
 
*Dime bla[3]  && Bill Allocation Array
m.bla =  m.ale.add_bau()
*  bla[1] = m.f1no  && gives the refrence number
m.bla.aname = m.f1no
*  bla[2] = 'New Ref' && type of Ref
m.bla.atype = 'New Ref'
*  bla[3] = -m.amt  && the amount to allocate
m.bla.amt = - m.amt
*  =Out_AldgE(@ale,.t.,@bla)  && gives array ale, true for blaflg , bill alloc array


* If no Form Exists    
For n = 1 to txcat

 Do Case
 Case vl[n,2] = 12.5 && Vat 12% Entry
  m.ale2 = vou1.add_ldgent()
  m.ale2.isdpos = .f.
  m.ale2.ldgname = 'Sales under Vat 12.5%'
  m.ale2.amt = vl[n,1]
  m.ale3 = vou1.add_ldgent()
  m.ale3.ldgname = 'Sales Tax Vat 12.5%'
  m.ale3.isdpos = .f.
  m.ale3.amt =  vl[n,3]
 Case vl[n,2] = 4  .and. f1dt < {^2010/04/01} && Vat 4% Entry and date till 31/03/2010
  m.ale2 = vou1.add_ldgent()
  m.ale2.isdpos = .f.
  m.ale2.ldgname = 'Sales under Vat 4%'
  m.ale2.amt = vl[n,1]
  m.ale3 = vou1.add_ldgent()
  m.ale3.ldgname = .f.
  m.ale3.isdpos = 'Sales Tax Vat 4%'
  m.ale3.amt = vl[n,3]
 Case vl[n,2] = 5.5 .and. f1dt >= {^2016/04/01} && Vat 5.5% Effectvie from 1/04/2016
  m.ale2 = vou1.add_ldgent()
  m.ale2.ldgname = "Sales Under Vat 5.50%"
  m.ale2.isdpos = .f.
  m.ale2.amt = vl[n,1]
  m.ale3 = vou1.add_ldgent()
  m.ale3.ldgname = 'Sales Tax VAT 5.50%'
  m.ale3.isdpos = .f.
  m.ale3.amt =vl[n,3]
 Case vl[n,2] = 5 .and. f1dt >= {^2010/04/01} .and. f1dt < {^2016/04/01} && Vat 5% Effective from  1/04/2010
  m.ale2 = vou1.add_ldgent()
  m.ale2.ldgname = 'Sales under Vat 5%'
  m.ale2.isdpos = .f.
  m.ale2.amt = vl[n,1]
  m.ale3 = vou1.add_ldgent()
  m.ale3.ldgname = 'Sales Tax Vat 5%'
  m.ale3.isdpos = .f.
  m.ale3.amt =vl[n,3]
 Case vl[n,2] = 3 .and. between(f1dt,{^2007/04/01},{^2009/03/31})  && Check if it is Form C from 1/4/2007

  m.ale2 = vou1.add_ldgent()
  m.ale2.ldgname = 'OMS Sale'
  m.ale2.isdpos = .f.
  m.ale2.amt = vl[n,1]

  m.ale3 = vou1.add_ldgent()
  m.ale3.ldgname = 'CST (OMS Tax 3%)'
  m.ale3.isdpos = .f.
  m.ale3.amt = vl[n,1]
Case vl[n,2] = 2 .and. f1dt > {^2008/04/01}  && Check if it is  Form C & 2% from 1/4/2008 onwards   
  m.ale2 = vou1.add_ldgent()
  m.ale2.ldgname = 'OMS Sale'
  m.ale2.isdpos = .f.
  m.ale2.amt = vl[n,1]
  m.ale3 = vou1.add_ldgent()
  m.ale3.ldgname = 'CST (OMS Tax 2%)'
  m.ale3.isdpos = .f.
  m.ale3.amt = vl[n,3]
 Case vl[n,2] = 0  .and. upper(tup.form) = "H FORM"&& could  be freight or other charges or if form shows H or I 
   *if !empty(tup.form)
      && Process for H & I Form
      m.ale2 = vou1.add_ldgent()
      m.ale2.ldgname = "Sales Under H Form."
      m.ale2.isdpos = .f.
      m.ale2.amt = vl[n,1]  
   *endif
*   
 Otherwise         && wrong Entry 
  wait window "Invalid Entry !!! "
 Endcase
Next n

* Now check for the vatavkasar entry
 if m.vatav # 0
   =out_vatav(m.vatav)
 endif
* send a blank carraige return to seperate the next entry
*\
vou1.out_xml()
return && out_vou

Function Out_vatav
Para am
m.ale3 = vou1.add_ldgent()
*ale[1] = .f.
m.ale3.ldgname = 'Vatav Kasar'
*ale[2] = 'Vatav Kasar'
*ale[3] = m.am
m.ale3.amt = m.am
*Out_aldge(@ale,.f.)
return



Function ini_txtmrgv

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




Function fill_frm1

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

Function rtrv_ldg()
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
