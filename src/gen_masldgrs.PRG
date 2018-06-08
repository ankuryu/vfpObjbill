* File name gen_masldgrs
* used to generate the Master Ledgers under TAlly
* using the Database file (Foxpro)
* as on 16/08/2017
* By SVG
* ldgmast has the following stru
* code text, ldgname text , parent

close data
set date brit
set excl off
k1 = createobject("kldgrmas")
k1.fname = "tlymasout3.xml"
k1.fraw = "tlymas2.csv"

k1.imp_csv()
sele one
do while .t.
 brow nomo
 if lastkey() = 27
   exit
 endif
 k1.add_ldg2lst()
 skip
enddo
k1.opn_out()
for n = 1 to k1.cnt
  k1.out_ldg(k1.lst[n])
next n
k1.cls_out()
modi comm (k1.fname)
return




Define class kldgrmas as custom
  dime lst[1]    && Array of the objects to output
  fname = ""     && filename to outpur
  fraw = ""      && CSV filename to import
  cnt = 0        && count of the ledgers



Function imp_csv
create cursor one (als c(10), ldgr c(30), prnt c(30) )
append from (this.fraw) type deli                   && import the data

return


Function add_ldg2lst
* Adds a ledger to list , data from current record
 scatter memvar
 this.cnt = this.cnt+1
 Dime this.lst[this.cnt]
 this.lst[this.cnt] = createobject("ldgrmas")
 to2 = this.lst[this.cnt]
 to2.als = this.esc_it(m.als)
 to2.ldgr = this.esc_it(m.ldgr)
 to2.prnt = this.esc_it(m.prnt)
 
 return
 


function out_ldg(o2)
Text
<TALLYMESSAGE xmlns:UDF="TallyUDF">
<LEDGER NAME="<<o2.ldgr>>" RESERVEDNAME="">
<ADDITIONALNAME><<o2.ldgr>></ADDITIONALNAME><ISINTERESTON>No</ISINTERESTON>
<PARENT><<o2.prnt>></PARENT>
<ISBILLWISEON>NO</ISBILLWISEON>
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
<LANGUAGENAME.LIST>
  <NAME.LIST>
     <NAME><<o2.ldgr>></NAME>
     <NAME><<o2.als>></NAME>
  </NAME.LIST>
  <LANGUAGEID> 1033</LANGUAGEID>
</LANGUAGENAME.LIST>

</LEDGER>
</TALLYMESSAGE>

endtext
return




function opn_out()
set textmerge to (this.fname)
set textmerge on 

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

return


function cls_out()
Text
</IMPORTDATA>
</BODY>
</ENVELOPE>
endtext
set textmerge off
set textmerge to
return


function esc_it
para st
Priv rs_str
&& checks for  XML Escape sequences and substitutes with necessary
with this
  rs_str = .esc_chr(st,'&','&amp;')
  rs_str = .esc_chr(rs_str,'<','&lt;')
  rs_str = .esc_chr(rs_str,'>','&gt;')
  rs_str = .esc_chr(rs_str,"'",'&apos;')
  rs_str = .esc_chr(rs_str,'"','&quot;')
endwith
return rs_str


function esc_chr
Para str,chr,sub

j = 1
k = 1  && occurence
do while j > 0
 j = at(chr,str,k)
 str = iif(j>0,stuff(str,j,1,sub),str)
 k = k+1
enddo 
return str

Enddefine


Define class ldgrmas as custom
  als = ""
  ldgr = ""
  prnt = ""
  
Enddefine


text

<TALLYMESSAGE xmlns:UDF="TallyUDF">
 <LEDGER NAME="[01] Net Sales 18% GST" RESERVEDNAME="">
  <ADDITIONALNAME.LIST>
    <ADDITIONALNAME>[01] Net Sales 18% GST</ADDITIONALNAME>
  </ADDITIONALNAME.LIST>
  <CURRENCYNAME>Rs.</CURRENCYNAME>
  <PARENT>Sales Accounts</PARENT>
  <TAXCLASSIFICATIONNAME/>
  <GSTTYPE/>
  <SERVICECATEGORY/>
  <EXCISELEDGERCLASSIFICATION>Default</EXCISELEDGERCLASSIFICATION>
  <EXCISEDUTYTYPE/>
  <TRADERLEDNATUREOFPURCHASE/>
  <TDSDEDUCTEETYPE/>
  <TDSRATENAME/>
  <LEDGERFBTCATEGORY/>
  <ISBILLWISEON>No</ISBILLWISEON>
  <ISCOSTCENTRESON>No</ISCOSTCENTRESON>
  <ISINTERESTON>No</ISINTERESTON>
  <ALLOWINMOBILE>No</ALLOWINMOBILE>
  <ISCONDENSED>No</ISCONDENSED>
  <AFFECTSSTOCK>No</AFFECTSSTOCK>
  <FORPAYROLL>No</FORPAYROLL>
  <INTERESTONBILLWISE>No</INTERESTONBILLWISE>
  <OVERRIDEINTEREST>No</OVERRIDEINTEREST>
  <OVERRIDEADVINTEREST>No</OVERRIDEADVINTEREST>
  <USEFORVAT>No</USEFORVAT>
  <IGNORETDSEXEMPT>No</IGNORETDSEXEMPT>
  <ISTCSAPPLICABLE>No</ISTCSAPPLICABLE>
  <ISTDSAPPLICABLE>No</ISTDSAPPLICABLE>
  <ISFBTAPPLICABLE>No</ISFBTAPPLICABLE>
  <ISGSTAPPLICABLE>No</ISGSTAPPLICABLE>
  <SHOWINPAYSLIP>No</SHOWINPAYSLIP>
  <USEFORGRATUITY>No</USEFORGRATUITY>
  <FORSERVICETAX>No</FORSERVICETAX>
  <ISINPUTCREDIT>No</ISINPUTCREDIT>
  <ISEXEMPTED>No</ISEXEMPTED>
  <ISABATEMENTAPPLICABLE>No</ISABATEMENTAPPLICABLE>
  <TDSDEDUCTEEISSPECIALRATE>No</TDSDEDUCTEEISSPECIALRATE>
  <AUDITED>No</AUDITED>
  <SORTPOSITION> 1000</SORTPOSITION>
  <LANGUAGENAME.LIST>
   <NAME.LIST>
     <NAME>[01] Net Sales 18% GST</NAME>
     <NAME>01</NAME>
   </NAME.LIST>
   <LANGUAGEID> 1033</LANGUAGEID>
  </LANGUAGENAME.LIST>
 </LEDGER>
</TALLYMESSAGE>

endtext