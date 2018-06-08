set procedure to outxmlg2.prg
set path to \\samba24\pmc\pm\cm                   && as all data files store here
set date brit
create cursor ldgrmas (lname c(50), topname c(50)) && prepare the cursor of Master party lists
append from pmacctldg.txt type deli                && Populate it



set textmerge to "pmbilltmp.xml"                  &&  This is a direct shortcut to be removed for generating the output file
set textmerge on                                  && start for preparing objfile

frmgrd1 = createobject("frmgrd")                  && Form object for selecting the master based on the Bill's party name
vou1 = createobject('t9vou')                      && Tally 9 Voucher object which will be used to create a single voucher xml
oKont = createob("ktlym")                         && Container object for  Tally master to be created for not existing in masters now
oKont.co = 'PM'                                   && Short cut again  restricts to "PM" only
oKont.gen_fname()                                 &&  Generates the file name for retriving / outputing data


* ============================================Start of Hardwired Code for CSV import =====================================

create cursor csvbl ( gst c(15),;
bno c(6),bdt d,amt n(12,2),pos c(5),;

rchrg l, invtyp c(3),egst c(15),taxr n(5,2),taxbl n(12,2),;
ces n(12,2), igst n(12,2), cgst n(12,2), sgst n(12,2),pname c(50))
   
   append from  exportdata.csv type deli
  repl all bno with tran(val(csvbl.bno),'@l ######')
  *set step on
  
   go top
   clea
   knt = 1
   vou1.out_hdr()
   *set step on
   do while !eof()
    ? recno()
   vou1 = createobject('t9vou')  
      m.fname = iif(left(bno,3) = '024',"pm172top",'pm171top')
    select pname from (m.fname) as tup where tup.f1no == csvbl.bno into array ar3
    m.bno = csvbl.bno
    ? m.bno

    if _tally > 0
       scatter memvar
      ? ar3[1]
      *update csvbl set pname = ar3[1] where csvbl.bno == m.bno
      m.ldgrname = ar3[1]
      *set step on
      Ln = chk_ldgrname(alltrim(m.ldgrname))
      
      if empty(Ln)
         Ln =add_new(m.ldgrname)
      endif   
      m.ldgrname = m.Ln  
       
      =out_vou()
    endif
  
     
     knt = knt+1
    sele csvbl
    goto (knt)
    skip
    if eof()
      exit
    endif
   enddo
 *+++++++++++++++++++++++++++++++++++++++++++++++++++End of Hardwired Code for CSV ++++++++++++++++++++++++++++++++++++++++
   vou1.out_ftr()
   set textmerge off
   set textmerge to
   vou1 = NULL
  if oKont.knt > 0                                        &&  Check if any new masters need to be added ?
    ?
    ? "Creating the master File for New Parties......"
    oKont.outxml()                                        &&  Create the XML file if need to add them
    ? "Done ................."
  
    ? "Updating the Ledgers.........."
    oKont.upd_ldgr()                                       &&  Also update the CSV file which maintains the master ledger lis
    ? "Done............."
    ?
endif 


m.co = m.company
wait wind "Do you want to save the Master Data ? (Y/N)" to yn
if uppe(yn) = "Y"
  select acctldg
m.pth = "\\samba24\pmc\pm\cm\")
  ? (m.pth+m.co+"acctldg.txt")
   copy to (m.pth+m.co+"acctldg.txt") type deli
endif
set path to   (oKont.cur_pth)    && reset to the old path
oKont = NULL                     && remove the object
close all

*********************************************     END   OF    MAIN  LOOOP  ***************************************


*#######################################
*    Functions  Start from here        *
*#######################################

*******************
function out_vou  &&
*******************
    m.dt = csvbl.bdt
    m.f1no = csvbl.bno
    m.amt = csvbl.amt
    *m.ldgrname = csvbl.pname
    vou1.dt = csvbl.bdt
    vou1.gst = csvbl.gst
    igstflg = !(left(vou1.gst,2) = '27')
    vou1.voudtl = "Type = 'Sales' action = 'Create'"
    vou1.vtypeN = "Sales"
    vou1.dt = m.dt
    vou1.vno = csvbl.bno
    m.esc_itLdgrname = alltrim(esc_it(m.ldgrname))
    vou1.pldg = m.esc_itldgrname
    vou1.nar = "Auto on " + dtoc(date()) + " Party : " + vou1.pldg
    m.ale = vou1.add_ldgent()
    m.ale.isdpos = .t.
    m.ale.ldgname = vou1.pldg
    m.ale.amt = -m.amt
    m.bla = m.ale.add_bau()
    m.bla.aname = m.f1no
    m.bla.atype = "New Ref"
    m.bla.amt =  -m.amt

     m.ale2 = vou1.add_ldgent()
     m.ale2.isdpos = .f.
     m.ale2.ldgname = "01"  && Net sales 18% GST
     m.ale2.amt = csvbl.taxbl

    Do Case
    
    Case !igstflg
     m.ale3 = vou1.add_ldgent()
     m.ale3.ldgname = "02"
     m.ale3.isdpos = .f.
     m.ale3.amt = csvbl.cgst
     m.ale4 = vou1.add_ldgent()
     m.ale4.ldgname = "03"
     m.ale4.isdpos = .f.
     m.ale4.amt = csvbl.sgst
    otherwise
     m.ale3 = vou1.add_ldgent()
     m.ale3.ldgname = "04"
     m.ale3.isdpos = .f.
     m.ale3.amt = csvbl.igst
    endcase 
    m.vatav = m.amt - (m.cgst+m.igst +m.sgst + m.taxbl)
    if m.vatav # 0
      =out_vatav(m.vatav)
    endif   
     vou1.out_xml()  
      
return 

******************************
function esc_it             &&
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

************************
function esc_chr      &&
************************
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

************************
Function Out_vatav    &&
************************

Para am
m.ale3 = vou1.add_ldgent()
*ale[1] = .f.
m.ale3.ldgname = 'Vatav Kasar'
*ale[2] = 'Vatav Kasar'
*ale[3] = m.am
m.ale3.amt = m.am
*Out_aldge(@ale,.f.)
return

function chk_ldgrname( party)
 priv rv
 rv = ""
 select lname from ldgrmas where lname = party into cursor tt2
 if _tally # 1
   party2 = party + "%"
   do while _tally = 0 .and. len(party2)> 2
     party2 = left(party2,Len(party2)-2) + "%"
     select lname from ldgrmas where lname like party2 into array ar4
   enddo 
   selec  lname from ldgrmas where lname like party2 into cursor tt2
   frmGrd1.msg = party
   frmGrd1.lbl1.caption = m.party
   frmGrd1.grd1.recordsource = "tt2"
   frmGrd1.show()
   read event
   acti scree
   if frmGrd1.selst
      rv = frmGrd1.vlu
   endif
 else
    rv = tt2.lname
 endif
     

return rv

function add_new(party)
 * set step on
 priv rv
 oKont.add_tlm(party,"")
return party



* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

*==================================================================
Define class ktlym as custom   && container class for tlymas     &&
*==================================================================

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
function init     && object ktylmas
* * * * * * * * * * 

 this.cur_pth = set("path")
 set path to (this.cur_pth + ";\\samba24\pmc\pm\cm")
 do form frmtlymas name this.frmRd noshow  && set the frnRd

* * * * * * * * * * * 
function gen_fname  && object:ktlymas makes the necessary filenames based on the company
* * * * * * * * * * *

  this.ofname = this.co + "tlymasout.xml"
  this.ldgfname = this.co + "acctldg.txt"
 * modi comm (this.ldgfname) 
return


* * * * * * * * * * *  
function outxml     && object:ktlymas
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
function upd_ldgr  && obj:ktlymas Updates the ledger (CSV) with new parties added during this run used during outxml
* * * * * * * * * * *

 acti scree
 Priv fh,i,j
 fh = fopen( this.ldgfname,12) && Open the file for writing
 =fseek(fh,0,2) && move the file to the end of the file
 for i = 1 to this.knt
   this.otlym = this.kntlst[i]
   if !this.otlym.pgst == ""
    j= this.otlym.pname + iif( fputs(fh,'"'+this.otlym.pname+'","Sundry Debtors"')> 0 ,"Added ","Could not Add")+ " In List"
    ? j
    if Right(j,5) = "Added"
      insert into acctldg (lname,lparent) values (otlym.pname,"Sundry Debtors") 
    endif
   endif 
 next i   
 =fclose(fh)
return


* * * * * * * * * * * * * *
function add_tlm(pn,pa)    &&obj:ktlymas-> adds a tallymas object
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
 function rd_tlym   && obj:ktlymas->reads the tallymas data using frmRd and verification by the user
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
  m.obj.pgst = m.gst                         && Changes made to incorporate the GST
  m.vattin = m.gst                           && VatTin is now Defunct
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

 function Opn_out  && obj:ktlymas->send the premable for master import
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
function out_ldg    && obj:ktlymas->Will Output the Master ledger as XML
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
function cls_out()  && obj:ktlymas-> Closing amble for XML
* * * * * * * * * * *

set textmerge on to (this.ofname) addi noshow
Text
</IMPORTDATA>
</BODY>
</ENVELOPE>
endtext
set textmerge off  

return

enddefine   && ******************************  Endo of DEFINE   : ktlymas



Define class tlymas as custom

pname = ''         &&  Party name
epname = ''        &&  Escaped Party name
pmvat = spac(15)   &&  Parties Vat No
pgst = spac(15)    &&  Party's GST Number
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


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   D E F I N I T I O N S    O F  F O R M S &  A L L I E D  *
*                                                           *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Define  class frmGrd as form
height = 300
width =  500
left = 150
top = 50
vlu = ""
selst = .f.
msg = ""

function init
 this.addobject("cmdOK","cmdOK")
 this.addobject("cmdCanc","cmdCanc")
 this.addobject("cmdExit","cmdExit")
 this.addobject('grd1',"grid")
 this.addobject('lbl1','label')
  this.cmdOk.left = thisform.width - (thisform.cmdOK.width) - 5
  this.cmdCanc.left  = thisform.width - (thisform.cmdCanc.width) - 5
  this.cmdExit.left =thisform.width - 1*((thisform.cmdCanc.width) + 5)
  this.cmdok.top = thisform.height - (thisform.cmdok.height +3)*4
  this.cmdCanc.top = thisform.height - (thisform.cmdCanc.height +3)*3
  this.cmdexit.top = thisform.height - (thisform.cmdExit.height +5) 
  this.grd1.top = 5
  this.grd1.left = 5
  this.grd1.visible = .t.
  this.lbl1.visible = .t.
  this.cmdOk.visible = .t.
  this.lbl1.top = thisform.height - 30
  this.lbl1.caption = thisform.msg
  this.lbl1.width = 130
  this.lbl1.left = (thisform.width+ thisform.lbl1.width)/2
return 

enddefine


Define class cmdOK as commandbutton
 height = 30
 width = 50
 caption = "OK"
 visible = .t.
 
function click()
 * set step on
 thisform.vlu = thisform.grd1.columns[1].text1.text
 thisform.selst = .t.
 thisform.hide
 clear events
enddefine



Define class cmdCanc as commandbutton
height = 30
width = 50
caption = "Cancel"
visible = .t.

function click()
 thisform.vlu = ""
 thisform.selst = .f.
 thisform.hide
 clear events
enddefine

Define Class cmdExit as commandbutton
height  =30
width = 50
caption = "Exit"
visible = .t.

function click()
thisform.hide
clear events

enddefine