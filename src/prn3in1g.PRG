* File prn3in1G.prg
*  as on 29/06/2017
* Files used : xxyy?top/itm/ dbf files
* Repo file :
* Screen (scx) prnfrm.scx
*
* Prints bills in GST format on plain paper
* 
* also require the data files  coyrxtop.dbf coyrxitm.dbf 
* Program to print the Cash Memo / Bill Program
* using SQL commands and the current 
* Data Files
*
* * * * * * * * * * * * * * *
* 
*
* * * * * * * * 


set date brit
set excl off
set path to \\samba24\pmc\pm\cm
*set step on
do form prnfrmG name   bilfrm linked
bilfrm.show()
read events
close data
clea all
clea
return



function prn_loop(m.co,m.yr,m.bkn,m.bno)
m.Inv_copy = "Original for Buyer"
m.curform = 'vcmgstsql_pln1' && this is the current form
clear


********************* OMIT THIS USING TEXT *****************
text
? "Current Printer : " + sys(6) && Print the current selected printer
 wait wind "Change printer (Y/N) " to yn
 if uppe(yn) = 'Y'
   set printer to name getprinter()
 endif
 ? "Current Printer : " + sys(6)
 wait wind
 clear


endtext


m.certify = "I/We hereby Certify that my/our registration Certificate under the Maharashtra Value Added Tax Act 2002 is in force "

m.certify = m.certify +  "on the date on which the sale of the goods specified in this Tax Invoice is made by me/us and that and that the transaction of Sale covered by this Tax Invoice has been effected by me/us and it shall be accounted for in the turnover of sales while"

m.certify = m.certify +   "filing of return and th due tax if any, payable on the sale has been paid or shall be paid."
m.ftype = "Cash Memo"
m.co_name = iif(m.co = 'PM',"Panalal Mohanlal & Co",'Kamlesh Corporation')
m.vat_no = iif(m.co = 'PM','27AAAFP1396C1ZG','27AAAFK1927D1ZL')
m.vgst = m.vat_no  && vendor's gst number
m.f_type = iif(m.bkn = '1',"Cash Memo (Tax Invoice)",'Tax Invoice')

m.f_type = iif(m.bkn > '2','Delivery Challan',m.f_type)
*m.bkn = iif(m.bkn = '3','2',m.bkn)
m.fnme_t =  bilfrm.fntup
m.fnme_i =  bilfrm.fnitm
m.bno = bilfrm.bno
prn_1bn(m.bno)

*====================================================================
return 

function prn_1bn(m.blno)
Private bno,m.bnlo
m.bno = m.blno
select * from (m.fnme_t)as tup where tup.f1no = m.bno into array artop  && this shall contain the party details


if _tally # 1 
  return -1  && error
endif

m.txform = artop[7]       && Get the form
m.o_dtl = artop[10]        &&  get the order details
m.d_dtl = artop[11]       && get the Despatch Details
m.bgst =  artop[15]        && get the buyer's gst number
igstflg = !(left(bgst,2) == left(vgst,2))             &&  Flag for  IGST / SCGS+CGST
                                             && if both GST numbers from same state IGSTFLG false else true
store '' to m.ord_dt1,m.ord_dt2
if memline(m.o_dtl) > 0
  m.ord_dt1 = mline(m.o_dtl,1)
  m.ord_dt2 = mline(m.o_dtl,2)
endif


store '' to m.dst,m.trspt,m.lrno,m.lr_dt
if memline(m.d_dtl) > 0
 m.dst = mline(m.d_dtl,1)
 m.trspt = mline(m.d_dtl,2)
 m.lrno = mline(m.d_dtl,3)
 m.lr_dt = mline(m.d_dtl,4)
 
endif
* Now get the bill dtails

select * from (m.fnme_i) as itm where itm.bno = m.bno into cursor billtmp order by bno,sr

select dsc as rt,sum(rate*qty) as amt, sum(rate*qty*(dsc/100)) from billtmp where billtmp.bno=m.bno into array dsc_ar group by dsc

select tax as rt,sum(rate*qty*(1+dsc/100)) as amt,sum(rate*qty*(1+dsc/100)*(tax/100)) as tx ;
   from (m.fnme_i) as itm where itm.bno = m.bno into array tax_ar group by tax
select tax as rt,sum(amt) as amt , sum(amt*(tax/100)) as tx  from (m.fnme_i) as itm where itm.bno = m.bno ;
   .and. (rate*qty = 0 .and. tax > 0) into array tax_ar2 group by tax
       
* Now calculate the various totals / and other figures
m.gttl = 0 
select sum(billtmp.amt) from billtmp into array tmpar
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
  m.TaxRs = round(m.TaxRs,2)
m.gttl = m.gttl + m.DiscRs + m.TaxRs
m.roff = round(m.gttl,0) - m.gttl
m.gttl= round(m.gttl,0)


 m.taxable1 = tran(m.taxable,'#######')
 m.taxable2 = m.taxable1
if igstflg
* set the headging, rate, tax amt for igst
  m.tx1hdg = "IGST @ " 
  m.tax1Rs = tran(m.taxRs, '######')
  store "" to tx2hdg,taxable1,tax1Rs
else
  m.tx1hdg = "SGST @ "
  m.tx2hdg = "CGST @ "
  m.tax1Rs  = tran( round(m.taxRs/2,0), "########")  
  m.tax2Rs = m.tax1Rs
endif

sele billtmp
susp
repo form &curform  preview
****************************************************   
* Start of Functions
****************************************************

Function amt_xwrd
Para pp
Priv vx,rupi,paise
rupi = int(pp)
paise = pp-rupi
paiw = iif(paise > 0,' and Paise '+x_wrd(paise*100),'')
vx = 'Rupees '+x_wrd(pp) + paiw +  ' Only.'
return vx
* * * * * * * * *
Function x_wrd
* * * * * * * * *
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


Function convt
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

Function chkit
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
    

