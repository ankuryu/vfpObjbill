* computes the gst of a bill
* 
* written by SVG
* based on co,yr,bk,bno  supplied as para
if pcount() < 0
 text
   Usage  do gst_tax  with co,yr,bk,bno"
    where co = company code (PM/KC)
          yr = Year digit without century eg 17/18 etc
          bk = Book Code => 1: Cmemo / 2: BillBook 3: Challan etc.
          bno = Bill No in 6 digits ####### 
   
 endtext  
 * return
endif
if adir(j3,"\\samba24\pmc\pm\cm\*") # 0
 set path to x:\pm\cm
else 
 
 set path to h:\pm\daily.0\cm
endif 
set excl off

 
obj1 = createobject("cybb")
obj1.co = "PM"
obj1.yr = "17"
obj1.bk = "1"
obj1.gt_fname()

obj1.gen_b2b()
sele b2b
brow


text
 select f1no,f1dt,pname from (obj1.fnt) into cursor jiju where f1dt > {^2017-06-30}
do while .t.
 sele jiju
 brow
  if lastkey() = 27
    exit
  endif

 obj1.bno = jiju.f1no
 obj1.cal_gst()
 *susp
enddo
endtext

clea 
return

 




store 0 to m.ttl, m.amt, m.roff



*select * from 

function jango  
        wait window "Clicking..." nowait
      clear events
      return
return

Define Class cybb  as custom
  m.co = "PM"   && company code
  m.yr = ""     && year withouth century
  m.bk = "2"    && bk code 
  m.bno = ""    && billno
  m.fnt = ""    && filename for primary
  m.fni = ""    && filename for secondary
  m.rdfrm = NULL  && form for reading the data
  
  
  function gen_b2b
  * generates the b2b compliant 
  * gst,bno,bdt,amt,pos,rchrg,invtyp,egst,taxr,taxbl,ces,igst,cgst,sgst
  create cursor b2b ( gst c(15),bno c(6), bdt d, amt n(9.2),pos c(20), rchrg l, invtyp c(5),;
                      egst c(15), taxr n(5,2), taxbl n(9,2),ces n(9,2), ;
                      igst n(9,2), cgst n(9,2), sgst n(9,2) )
   select itm.type,itm.rate,itm.qty,itm.tax,itm.dsc, iif(itm.type $ "INVITM",itm.qty*itm.rate,itm.amt) as am,tup.gst,tup.f1no,tup.f1dt,tup.pname from (this.fni) as itm left join (this.fnt) as tup on itm.bno == tup.f1no where month(tup.f1dt) == 7 order by f1no into cursor jio
  sele jio
  
  scan 
   store "" to m.egst ,m.pos
   store 0 to m.taxbl,m.taxr,m.ces
   store 0 to m.igst, m.sgst,m.cgst     

  m.bno = jio.f1no
   ? recno("jio")
   m.igstflg = (left(jio.gst,2) # "27")
   store 0 to m.amt, m.gttl, m.rttl
   * now check if the tax rate is single
   select jio.tax from jio into array ar5 group by jio.tax
   *set step on
   Do Case
   case type("ar5") = 'U'
     m.taxr = 0
   Case alen(ar5)  = 2
     m.taxr = ar5[1,1] + ar5[2,1]
     m.taxr = iif(tran(m.taxr, '@z ##')$ "05121828",m.taxr,0)
   Case alen(ar5) = 1
     m.taxr = iif(tran(m.taxr, '@z ##')$ "05121828",m.taxr,0)
   Endcase

   select sum(jio.am) from jio where jio.f1no == m.bno  into array ar2
   select sum((0+jio.dsc/100)*jio.am) from jio where jio.f1no == m.bno into array ar3
   if m.igstflg
     select sum( (1+jio.dsc/100)*jio.am*(0+jio.tax/100))from jio where jio.f1no == m.bno  into array ar4
   else
     select sum( (1+jio.dsc/100)*jio.am*(0+jio.tax/200)) from jio where jio.f1no == m.bno  into array ar4
   endif
   m.bno = jio.f1no
   m.bdt = jio.f1dt
   m.pname = jio.pname
   m.gst =  jio.gst
   m.rchrg = .f.
   m.invtyp = "R"
   ? "Total : ....."  + tran(ar2[1],'@z #########.##')
   m.rttl = ar2[1]
   ? "Discount : ...." + tran(ar3[1],'@z ########.##')
   m.rttl = m.rttl + ar3[1]
   ? "Running Total ...>>> " + tran(m.rttl,"@z ########.##")
   m.taxbl = m.rttl
   if igstflg
     ? " IGST Tax : ........." + tran(round(ar4[1],0),'@z ########.##')
     m.rttl = m.rttl + round(ar4[1],0)
     m.igst = round(ar4[1],0)
   else 
     ? " SGST TAX : ........." + tran(round(ar4[1],0),'@z ########.##')
     ? " CGST TAX : ........." + tran(round(ar4[1],0),'@z ########.##')   
     m.cgst = round(ar4[1],0)
     m.sgst = m.cgst
     m.rttl = m.rttl + round(ar4[1],0)*2
   endif
    ? "Running Total ...>>> " + tran(m.rttl,"@z ########.##")   
    m.roff = round(m.rttl,0) - m.rttl
    ? "Rounding of  ........" + tran(m.roff,'@z ##.##')
    ? "Grand Total.........." +tran(round(m.rttl,0),'@z ########.##') 
    m.amt = round(m.rttl,0)
   
    insert into b2b ( gst,bno,bdt,amt,pos,rchrg,invtyp,egst,taxr,taxbl,ces,igst,cgst,sgst ) ;
    values ( m.gst,m.bno,m.bdt,m.amt,m.pos,m.rchrg,m.invtyp,m.egst,m.taxr,m.taxbl,m.ces,m.igst,m.cgst,m.sgst )
   select jio
   do while jio.f1no = m.bno
    skip
   enddo
   skip -1
   wait window
   clea
 endscan
 return   
                     
                      
  
  
  function cal_gst()  && calculates the gst
  ? this.bno
  select itm.type,itm.rate,itm.qty,itm.tax,itm.dsc, iif(itm.type $ "INVITM",itm.qty*itm.rate,itm.amt) as am,tup.gst from (this.fni) as itm left join (this.fnt) as tup on itm.bno == tup.f1no where itm.bno ==this.bno into cursor jio
  brow 
  ? jio.gst
  igstflg = (left(jio.gst,2) # "27")
  store 0 to m.amt, m.gttl, m.rttl
  select sum(am) from jio into array ar2
  select sum((0+dsc/100)*am) from jio into array ar3
  if igstflg
    select sum( (1+dsc/100)*am*(0+tax/100))from jio into array ar4
  else
    select sum( (1+dsc/100)*am*(0+tax/200)) from jio into array ar4
  endif    
  ? "Total : ....."  + tran(ar2[1],'@z #########.##')
  m.rttl = ar2[1]
  ? "Discount : ...." + tran(ar3[1],'@z ########.##')
  m.rttl = m.rttl + ar3[1]
  ? "Running Total ...>>> " + tran(m.rttl,"@z ########.##")
  if igstflg
    ? " IGST Tax : ........." + tran(round(ar4[1],0),'@z ########.##')
    m.rttl = m.rttl + round(ar4[1],0)
  else 
    ? " SGST TAX : ........." + tran(round(ar4[1],0),'@z ########.##')
    ? " CGST TAX : ........." + tran(round(ar4[1],0),'@z ########.##')   
    m.rttl = m.rttl + round(ar4[1],0)*2
  endif
   ? "Running Total ...>>> " + tran(m.rttl,"@z ########.##")   
   m.roff = round(m.rttl,0) - m.rttl
   ? "Rounding of  ........" + tran(m.roff,'@z ##.##')
   ? "Grand Total.........." +tran(round(m.rttl,0),'@z ########.##') 
  wait window
  clea
  
  return
  
  
  
  function gt_fyr(dt)
    dt = iif( type('dt') == 'U',date(),dt)  
    yr_n = year(date())
    mt_n = month(date())
    yr_s = str( yr_n + iif(mt_n <4,1,0))
    this.yr = right(yr_s,2)
  return this.yr
  
  
  
  function gt_fname()
    this.fni = this.co + this.yr + this.bk + 'itm'
    this.fnt = this.co + this.yr + this.bk + 'top'
   return
  

  function chk_it()
   this.co = iif(type("this.co")= 'U' .or. !(this.co $ "PMKC"), 'PM',m.co)
   this.yr = iif(type("this.yr")= 'U', gt_fyr(), yr )
   this.bk = iif(type("this.bk") = 'U' , '2', bk)
   this.bno = iif(type("this.bno") = "U",'00000',bno)
   
   return
   
  function rd_coyrbk()
  
  
  
  
  function rd_coyr()
  
  this.rdfrm = createobject("form") 
  v = this.rdfrm
   with v
   .height = 55
   .width = 100
   .top = 10
   .left = 10
   
   .addobject("cboCo","comboBox")
    with v.cboCo
      .height = 15
      .width =  20
      .left = 2 
      .top = 2
    endwith
    
    
   .addobject("cboYr","comboBox")
    with v.cboYr
      .height = 15
      .width =  20
      .left = 25 
      .top = 2
    endwith
   
   
   .addobject("cboBk","comboBox")
    with v.cboBk
      .height = 15
      .width =  20
      .left = 75 
      .top = 2
    endwith
   
   
   .addobject("cmdSel","commandbutton")
    with v.cmdSel
      .height = 15
      .width =  20
      .left = 100 
      .top = 2
      .caption = "Select"
    endwith
   
   
   .addobject("cmdCan","commandbutton")
   
    with v.cmdCan
      .height = 15
      .width =  20
      .left = 100 
      .top = 20
      .caption = "Cancel"
    endwith
   
   .addobject("cmdExit","commandbutton") 
    with v.cmdExit
      .height = 15
      .width =  20
      .left = 100 
      .top = 40
      .caption = "Exit"      
      .WriteMethod("click","jango()",.t.)
      
    endwith
  endwith
  
  this.rdfrm.show()
  read events
  
  return
  
enddefine  

