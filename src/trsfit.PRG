* Program to copy one form into another  eg  proforma to bill etc
* written by Sunil Gandhi
* as on 13-11-18
* file name trsfit.prg


* Param req  co(c), yr(c),bk_src(c), frmno(c), bk_trg(c)
para co,yr,bk_src,frmno,bk_trg
close data
set excl off
set path to x:\pm\cm,g:\vfpobjbill.git\data,g:\vfpobjbill.git\src
*set step on
Priv fntsrc , fnisrc  && source file to copy from
Priv fnttrg, fnitrg   && target files to copy to
Priv fnttmp, fnitmp    && name of temporary file to store data
Priv rv,ar1,ir1,tr1,tr2,nxfrm,nxdt,totitm,n

* form the src file and target files
m.bssrc = m.co + m.yr + m.bk_src 
m.bstrg = m.co + m.yr + m.bk_trg
m.fntsrc = m.bssrc + 'top.dbf'
m.fnisrc = m.bssrc + 'itm.dbf'
m.fnttrg = m.bstrg + 'top.dbf'
m.fnitrg = m.bstrg + 'itm.dbf'
* check if file exists  exit if not exists
 rv = 0
 rv = rv + iif(file(m.fntsrc),1,0) 
 rv = rv + iif(file(m.fnisrc),2,0)
 rv = rv + iif(file(m.fnttrg),4,0)
 rv = rv + iif(file(m.fnitrg),8,0) 
 
 if rv # 15 && all files  dont exist
    return 
 endif
   
* check if frmno exists exit if not exists

  select f1no from (m.fntsrc) where f1no == m.frmno into array ar1
  if _tally # 1  && form doesn't exist
    return
  endif  
* copy the top rec to fnttmp
  release memo like ar1
  select * from (m.fntsrc) where f1no == m.frmno into array ar1
  

* copy the itm recs to fnitmp
  select * from (m.fnisrc) where bno == m.frmno into array ir1
  totitm = _tally
  
* get the next form no in the file and repla the current frmno with it in tmpfiles
  select max(f1no) from (m.fnttrg) into array tr1
  if _tally = 1
    select f1dt from (m.fnttrg) into array tr2 where f1no == tr1[1]
    nxfrm = iif(_tally = 1, tran(val(tr1[1])+1,'@l ######'),0)
    nxdt = tr2[1]
  
    ar1[1,1] = nxfrm
    ar1[1,2] = nxdt
    ar1[1,3] = nxfrm
    ar1[1,4] = nxdt
    close data
    use (fnttrg) alia tup shared
    append blank
    gather from ar1 memo
   * list memo like ar1
    use
  else
    return err  
  endif  
* next append items to the file
  use (fnitrg) alia itm
  *set step on
  For n = 1 to totitm
    append blank
    ir1[n,1] = nxfrm
    totcol = alen(ir1,2)
    acopy(ir1,ir2, aelement(ir1,n,1), totcol)
    dime ir2[totcol]
    gather from ir2
     *list memo like ir2
    release ir2
  next n
  ? "Copied " + m.frmno + " to " + m.nxfrm + " with items -> " + str(totitm)
  close data
  


