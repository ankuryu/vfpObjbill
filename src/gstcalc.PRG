* Objects for GST Calculation

set excl off
set date brit
set path to \\samba24\pmc\pm\cm
clea all

clea


kgst1 = createobject("kGst")
kGst1.ftup = "pm172top"
kGst1.fitm = "pm172itm"


use (kgst1.ftup) alia kgstcurs
go bott
do while .t.
 brow
 if lastkey() = 27
  exit
 endif
kgst1.bno = kgstcurs.f1no
kgst1.bdt = kgstcurs.f1dt
kgst1.gst = kgstcurs.gst
kgst1.cr_crs()
kgst1.pop_ded()
kgst1.pop_gst()
kgst1.calc()
wait window

sele kgstcurs
enddo




Define Class kGst as custom   && Container object for discounts and gst objects
bno = ""                      && Bill No
bdt = ctod("")                && Bill Date
bamt = 0                      && Bill Amount
txbl  = 0                     && Bill Taxable amt
tax   = 0                     && Bill taxes
vatav = 0                     && Vatav 
gst  = ""                     && GST no of party
iflg = .f.                    && Flag for IGST
Dime ogst[1] = NULL           && Array of  GST objects
Dime oDed[1] = NULL           && Array of  Ded objects (discount objects)
gcnt = 0                       && Count of GST objects
dcnt = 0
ftup = ""                     && File name of the top file
fitm = ""                     && File name of item.




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
  private obj,n,i
  ifac = iif(this.iflg,1,.5)
  select gtmpi.tax, sum((1+gtmpi.dsc/100)*gtmpi.amt),sum((1+gtmpi.dsc/100)*gtmpi.amt)* gtmpi.tax/100 from gtmpi  into array tx1 group by gtmpi.tax
  n = _tally
  if n = 0
   this.gcnt = 0
   return
  endif
  for i = 1 to n
    dime this.ogst[i] 
    this.ogst[i] =  createobject("gst")
    m.iflg = this.iflg
    obj = this.ogst[i]
    if m.iflg
     obj.txrt = tx1[i,1]
     obj.txbl = tx1[i,2] 
     obj.igst = tx1[i,3]
    else
      obj.txrt = tx1[i,1]/2
      obj.txbl = tx1[i,2]
      obj.cgst = round(tx1[i,3]/2,0)
      obj.sgst = obj.cgst
    endif   
  next i
  this.gcnt = n   
  return
  

function calc
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
gttl = sttl  - m.deda + m.igst + m.cgst+ m.sgst
vatav = round(gttl,0) - m.gttl





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
