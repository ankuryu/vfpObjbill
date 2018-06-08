para m.mfg,m.icode,m.asize
m.fsal = 'pm18inv'
m.frcp = 'pm118irc'
set path to x:\pm\cm,x:\pm\99,x:\pm\18
select mfg,icode,asize,qty,loc,max(dt) as mxdt from x:\pm\18\csm18 ;
   where mfg == m.mfg .and. ; 
   icode == m.icode .and. ;
   asize == m.asize ;
   into array ops
   if _tally # 0
     m.op = ops[4] 
     m.loc = ops[5]
     mxdt = ops[6]
   else
     m.op = 0
     m.loc = ''
     mxdt = ctod('01-04-18')  
   endif
   
  select sum(qty)  from (m.fsal) as isu ;
  where mfg == m.mfg .and. ;
   icode == m.icode .and. ;
   asize == m.asize .and. ;
   isu.dt > mxdt ;
   into array rd
   if _tally > 0
     m.isu = rd[1]
   else
     m.isu = 0
   endif  
   
     select sum(qty)  from (m.frcp) as rcp ;
  where mfg == m.mfg .and. ;
   icode == m.icode .and. ;
   asize == m.asize .and. ;
   rcp.r_dt > mxdt ;
   into array rc
      if _tally > 0
     m.rcp = rc[1]
   else
     m.rcp = 0
   endif  
   
   ? m.mfg,m.icode,m.asize
   ? "Opening : "  + tran(m.op,'@l ####')
   ? "Rcps : "  + tran(m.rcp,'@l ####')
   ? "Isu : "  + tran(m.isu,'@l ####')
   m.bal = m.op + m.rcp - m.isu
   ? "Bal : "  + tran(m.bal,'@l ####')
   

Define Class ostk as custom
 mfg = ""
 icode = ""
 asize = ""
 dt = ctod('')
 ostk = 0
 isu = 0
 rcp = 0
 bal = 0
 loc = ''
 pnd = 0
 ord = 0
 co = 'PM'
 yr = '18'
 fnostk = ""
 fnisu = ""
 fnrcp = ""
 fnpnd = ""
 fnord = ""
 
 function init ()
  this.fnostk = 'csm' + this.yr
  this.fnisu = this.co + this.yr + 'inv'
  this.fnrcp = this.co +'1' + this.yr + 'irc'
  
 return
 
 function get_stk()
 select mfg,icode,asize,qty,loc,max(dt) as mxdt from (this.fnostk) ;
   where mfg == this.mfg .and. ; 
   icode == this.icode .and. ;
   asize == this.asize ;
   into array ops
   recex =  _tally # 0
   this.ostk = iif( recex ,ops[4],0)
   this.loc = this.loc + iif(recex,alltrim(ops[5]),'')
   this.dt = iif(recex,(ops[6]),ctod(''))
* now get the sales figures
  select sum(qty)  from (this.fnisu) as isu ;
  where mfg == this.mfg .and. ;
   icode == this.icode .and. ;
   asize == this.asize .and. ;
   isu.dt > this.dt ;
   into array rd
   recex =  _tally # 0
   this.isu = iif(recex,rd[1],0)
   
 * now get the receipts
 
   select sum(qty)  from (this.fnrcp) as rcp ;
  where mfg == this.mfg .and. ;
   icode == this.icode .and. ;
   asize == this.asize .and. ;
   rcp.r_dt > this.dt ;
   into array rc
  recex = _tally # 0
  this.rcp = iif(recex,rc[1],0)
  this.bal = this.ostk + this.rcp - this.isu 
   ? this.mfg,this.icode,this.asize
   ? "Opening : "  + tran(this.ostk,'@l ####')
   ? "Rcps : "  + tran(this.rcp,'@l ####')
   ? "Isu : "  + tran(this.isu,'@l ####')
   ? "Bal : "  + tran(this.bal,'@l ####')
   

 
 function get_isudtls
 
 function get_rcpdtls
 
 
 function get_pnddtls
 
 
 
 
 

Enddefine


  
  