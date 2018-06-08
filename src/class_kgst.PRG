* File name Class_Kgst.prg
* as on 31Aug17
* By Sunil G
* uses the Invoice Files to create an GST obj and calculate Discounts/GST
* Used by Invoice Prog and OutXML


Define Class kGst as custom   && Container object for discounts and gst objects
bno = ""                      && Bill No
bdt = ctod("")                && Bill Date
bamt = 0                      && Bill Amount
gamt = 0                      && Gross Amount
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
  select gtmpi.dsc, sum(gtmpi.amt), sum(gtmpi.amt*gtmpi.dsc/100) from gtmpi into array de1 where gtmpi.dsc # 0 group by gtmpi.dsc
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
  select sum(gtmpi.amt)  from gtmpi into array am
  this.gamt = iif(_tally > 0, am[1],0)
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

