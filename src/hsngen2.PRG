 * Program name hsngen.prg
 * as on  9 may 2018
 * Program to generate HSN code details
 *
 Para  co,mth,yr
m.co = iif(type('co') # "C", "PM", m.co)
if type('yr') # 'C'
  year = year(date()) -2000
  year = year - iif(between(month(date()),4,12),0,1)
yr =alltrim(str(year))
endif
fnbitm = m.co + m.yr + "2itm"
fncitm = m.co + m.yr + "1itm"
fnbtup = m.co + m.yr + "2top"
fnctup = m.co + m.yr + "1top"
 if type('mth') # 'N'
   mth = month(date())
 * mth = alltrim(str(mthn))

 endif
 set path to x:\pm\cm 


* select  *,left(gst,2)=='27' .or. left(gst,1)=='0' as iflg from  (fncitm) as itm left join (fnctup) as tup on tup.f1no == itm.bno where month(tup.f1dt)= mth ;
* union ;
*  select  *,left(gst,2)=='27' .or. left(gst,1)=='0' as iflg from (fnbitm) as itm left join (fnbtup) as tup on tup.f1no == itm.bno where month(tup.f1dt)= mth ;
*  into cursor hsnall

 select  *,left(gst,2)=='27'  as sflg from  (fncitm) as itm left join (fnctup) as tup on tup.f1no == itm.bno where month(tup.f1dt)= mth ;
 union ;
  select  *,left(gst,2)=='27' as sflg from (fnbitm) as itm left join (fnbtup) as tup on tup.f1no == itm.bno where month(tup.f1dt)= mth ;
  into cursor hsnall

* select  *,(left(hsn,2=='27').or.left(hsn,1)='0')as iflg from pm172itm as itm left join pm172top as tup on tup.f1no == itm.bno where month(tup.f1dt) = mth ;  
*  union ;
* select *,(left(hsn,2=='27').or.left(hsn,1)='0')as iflg from pm171itm as itm left join pm171top as tup on tup.f1no == itm.bno where month(tup.f1dt) = mth   into cursor kangroo
 
* select hsn,sum(qty),sum((1+dsc/100)*amt),
 select hsn,"" as desc,unt,sum(qty),0 as ttlv, sum( round((amt*(1+dsc/100)),2) ) as txbl,;
sum( amt*(1+dsc/100) * tax/100* iif(sflg,0,1) ) as igst, ;
sum( amt*(1+dsc/100) * tax/200* iif(sflg,1,0) ) as sgst, ;
sum( amt*(1+dsc/100) * tax/200* iif(sflg,1,0) ) as cgst from hsnall into cursor hsnfin ;
 group by hsn
 
 ofnfin = m.co + "hsnfin" + tran(mth,'@l ##')+".csv"
 ofnall = m.co + "thsnall" + tran(mth,'@l ##')+".csv"
 copy to (ofnfin) type deli
 selec hsnall
 copy to (ofnall) type deli
 
 
