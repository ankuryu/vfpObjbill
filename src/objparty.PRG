*********************************************
* uses the following form files
* "frmobjParty" for  reading the object data
* "frmbrwParty" for displaying the objects in the container.


DEFINE class kPrty  as custom  &&  Container object for oPrty

  oPrty = NULL       &&  Party object
  fname = NULL       &&  File containing the Details of
  frmobj = NULL      &&  Form object used to read the object:- frmObjParty.scx
  frmbrw = NULL      && Form for Browsing / Selecting Data  := frmBrwParty.scx



  FUNCTION init()
  this.oPrty = createobject("oParty")
  DO form frmobjParty name this.frmobj noshow
  this.frmobj.pobj = this.oPrty            &&  Pointer to the object
  this.frmobj.kpobj = this                 &&  Pointer to the container obj
  DO form frmbrwParty name this.frmbrw noshow
  this.frmobj.pobj = this.oPrty
  this.frmbrw.koprty = this
  this.frmbrw.oPrty = this.oPrty

  RETURN

  FUNCTION put_obj()   && puts the object into the file
  PRIV pcode
  T = this.oPrty
  m.pcode = alltrim(t.pcode)
  IF !empt(m.pcode) .and. this.chk_obj()
    UPDATE (this.fname) ;
      set pname = t.pname , ;
      p_addr = t.p_addr, ;
      mvat = t.mvat,;
      pan = t.pan ,;
      gst = t.gst ;
      where pcode == t.pcode
*set pcode = t.pcode ,
  ELSE
    IF empt(m.pcode)
      this.oPrty.pcode = this.gen_new_pcode(this.oPrty.pname)
    ENDIF
    INSERT into (this.fname) (pcode,pname,p_addr,mvat,pan,gst) values(t.pcode,t.pname,t.p_addr,t.mvat,t.pan,t.gst)
  ENDIF

  FUNCTION get_obj()  && gets the object with pcode from the file
  PRIVATE rv,ar1
  rv = .f.
  pcode = this.oPrty.pcode
  SELECT * from (this.fname) where pcode == m.pcode into array ar1
  IF type('ar1') # 'U' .and. alen(ar1,1) = 1
    WITH this.oPrty
      .pname = ar1[2]
      .p_addr = ar1[3]
      .mvat = ar1[4]
      .ptly = ar1[5]
      .pan =  ar1[6]
      .gst =  ar1[7]
    ENDWITH
    rv = .t.
  ENDIF
  RETURN rv

  FUNCTION rd_obj()  && reads the obj details using form objfrm
  SELECT pcode,pname,gst from (this.fname) order by pcode into cursor t1
  this.frmobj.grdParties.recordSourceType = 1
  this.frmobj.grdParties.recordSource = "t1"
  this.frmobj.show()
  this.frmobj.grdParties.refresh()
  READ event
*this.frmobj.hide()
  RETURN


  FUNCTION chk_obj()  && checks if pcode exists in the file
*pcd = iif(!pcd , this.oPrty.pcode,pcd)
  pcd = this.oPrty.pcode
  SELECT pcode from (this.fname) where m.pcd == pcode into array ar1
* susp
  RETURN (type('ar1')# 'U') .and. alen(ar1,1) = 1


  FUNCTION chk_mvat()  &&  Checks for mvat in the file
  m.mvat = this.oPrty.mvat
  SELECT mvat from (this.fname) where m.mvat == mvat into array ar1
* susp
  RETURN (type('ar1')# 'U') .and. alen(ar1,1) = 1

  FUNCTION chk_gst() && Checks for GST in the file
  m.gst = this.oPrty.gst
  SELECT gst from (this.fname) where m.gst == gst into array ar1
  RETURN (_tally # 0)


  FUNCTION brw_obj(ptxt)   &&  brows and gives for selection objects using search crit if any
  ptxt = iif( type("ptxt")="C",ptxt,"")
*set step on
*msg = "SElect * from " + (this.fname) +" Where pname like '"+ ptxt + "%' Noconsole"
*wait window msg nowa

*SElect * from   (this.fname)  Where pname like  (m.ptxt +"%") Noconsole
  this.frmbrw.grdgen.recordsourceType = 4
  this.frmbrw.grdgen.recordsource = "SElect * from " + (this.fname) +" Where pname like '"+ ptxt + "%' into cursor t1"
  this.frmbrw.txtSrch.value = m.ptxt
  this.frmbrw.grdgen.refresh()
  this.frmbrw.show()
  this.frmbrw.txtSrch.setfocus()


  FUNCTION srch_pcode(str) && search and gives array of

  FUNCTION srch_pname(str)

  FUNCTION gen_new_pcode(m.pname)  &&  Generates the new pcode based on the first alphabet of name
&&  and last sequence of letter
  PRIV ar2

  SELECT max(pcode) from (this.fname) where left(pcode,1) = left(m.pname,1) into array ar2
  IF _tally = 0  && this is the first name with this alpha
    m.pcode = left(m.pname,1) + "0001"
  ELSE
    incval = tran(val(subs(ar2[1],2,4))+1,"@l ####")
    m.pcode = left(m.pname,1)+ incval
  ENDIF
  RETURN m.pcode


  FUNCTION Add
* Adds new party into the container
  WAIT window "Adding New...."
  this.rd_obj()
  RETURN

ENDDEFINE


DEFINE Class oParty as custom   && Object of the Party Details

  pcode = ''         && party code
  pname = ''         && Party Name
  p_addr = ''        && Party Address
  mvat = ''          && Mvat Number
  pan = ''           && Pan Number of Party
  ptly = ''          && Ledger name as in Tally
  gst = ''           && GST number
  kPrty = NULL       && points to the container class of the party

  FUNCTION blnkit &&  Blanks the object with initial values
  this.pcode = ''
  this.pname = ''
  this.p_addr = ''
  this.mvat = ''
  this.pan = ''
  this.ptly = ''
  this.gst = ''
  RETURN

ENDDEFINE
