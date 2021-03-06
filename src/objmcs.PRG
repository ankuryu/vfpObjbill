****************************************
* File objmcs.prg
* Used for omcs  object which consists of mfg, icode and asize code
* retrieves the description based on the codes
* Also prices / tax /disc etc on the basis of the code
* will use a master pointer file which will point to the necessary file
* to retrieve the necessary data
* this defines the container and  the object
* linked with objbill.prg  (This file is used there using set procedure to )





Define class kmcs as custom  && container for the omcs object
 omcs = NULL
 dfile = "omcsd.dbf"   && central data file containing all data about various files to refer
 ficode = ""  && file name for icode, description etc
 fmfg = ""    && file name for mfg,description etc
 fasize = ""  && file name for asize description
 fpric = ""   && file for prices 
 omcsd = NULL &&  this object will have all the necessary data based on omcs
 
 function init(  m.pmcs)
   this.omcs = m.pmcs     && mcs object sent as parameter
  this.omcsd = createobject("mcsd",this.omcs) && create the object mcsd
 return
 
 function gt_mname()
  && gets mfg name
  priv ar1,rv
  rv = .f.
  select fname from (this.dfile) where ki = "MFG" .and. scp = "ALL" into array ar1 && get the mfg file
  if _tally = 1 .and. !empty(ar1[1])
    m.fname = ar1[1]
    m.mfg = this.omcs.mfg 
    select name from (m.fname) into array ar1 where mfg = m.mfg
    if _tally = 1
      this.omcsd.mname = ar1[1]
      rv = .t.
    endif
  endif
    
 return rv
 
 function gt_sname()
  && gets the size name
  Priv ar1,rv
  rv = .f.
  select fname from (this.dfile) where ki = "ASIZE" .and. scp = "ALL" into array ar1 && get the size file
  if _tally = 1 .and. !empty(ar1[1])
    m.fname = ar1[1]
    m.asize = this.omcs.asize
    select mmsize,inchsize from (m.fname) into array ar1 where siz_code == m.asize
    if _tally = 1
      this.omcsd.mmsize  = ar1[1]
      this.omcsd.inchsize = ar1[2]
      rv = .t.
    endif
  endif
    
 return rv
  
 

 
 function gt_iname()
 && gets the description
  
  Priv ar1,rv
  rv = .f.
  * set step on
  select fname from (this.dfile) where ki = "ICODE" .and. scp = "ALL" into array ar1 && get the description file
  if _tally = 1 .and. !empty(ar1[1])
    m.fname = ar1[1]
    m.icode = this.omcs.icode
    select name,d_desc from (m.fname) into array ar1 where icode == code
    if _tally = 1
      this.omcsd.idesc  = ar1[1]
      this.omcsd.iddesc = ar1[2]
      rv = .t.
    endif
  endif
    
 return rv
 
 
 function gt_pri()
 && get the prices 
   
  Priv ar1,rv
  rv = .f.
  select fname from (this.dfile) where ki = "PRICE" .and. scp = "LDR" into array ar1 && get the description file
  if _tally = 1 .and. !empty(ar1[1])
    m.fname = ar1[1]
    m.icode = this.omcs.icode
    m.mfg   = this.omcs.mfg
    m.asize = this.omcs.asize
    store 0 to this.omcsd.pri , this.omcsd.mrp
    select pri,mrp from (m.fname) into array ar1 where mfg == m.icode .and. icode == code  .and. asize == m.asize
    if _tally = 1
      this.omcsd.pri = ar1[1]
      this.omcsd.mrp = ar1[2]
      rv = .t.
    endif
  endif
    
 return rv
 
 function mk_desc()
  && makes the total description using all above
  Priv rv
  this.gt_mname()
  this.gt_sname()
  this.gt_iname()
  with this.omcsd
   rv = alltrim(.mname) + " " + alltrim(.idesc) + " " + alltrim(.mmsize) + "mm"
  endwith
  return rv
  
  
  
  Enddefine  && Kmcs
  
  
  Define Class mcsd  as custom
   
   omcs  = NULL    && the mcs object which will help collect the data
   mname = ""      &&  Manufactur's  name
   mmsize = ""     && Size in MM
   inchsize = ""   && Size in Inch
   idesc =  ""      && Short desc of the itme
   iddesc = ""      && Detailed descriptiong of the item
   pri = 0         && Price
   mrp = 0         && Mrp  Price
   hpri = 0        && History price
   opri = 0        && other price
   
   function init(obj)
     this.omcs = obj
   return
     
  Enddefine
  
  
  *************************************************************************
  * removed below definition of the class as its already defined in objbill
  * which is related to other classes in it.  BAD definition albit
  *
*  DEfine class mcs as custom
*  mfg = ""
*  icode = ""
*  asize = ""
  
*  enddefine
