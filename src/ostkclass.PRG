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
   *set step on
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
   *? this.mfg,this.icode,this.asize
   *? "Opening : "  + tran(this.ostk,'@l ####')
   *? "Rcps : "  + tran(this.rcp,'@l ####')
   *? "Isu : "  + tran(this.isu,'@l ####')
   *? "Bal : "  + tran(this.bal,'@l ####')
   

 
 function get_isudtls
  wait window "Sales"
 
 function get_rcpdtls
  wait window "Receipts"
 
 function get_pnddtls
  wait window "Pending"
Enddefine


  
  