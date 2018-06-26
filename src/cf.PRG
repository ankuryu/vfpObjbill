*co : PM,KC,AB
*yr : {17,18}
*bk : {1:CM, 2:BL, 3:CH, 4:QU 5:PR}
*fls : files
*avl : l
*lck :

=cr_cf()
=cr_files("KC","18","4",'.\data')
=cr_files("KC","18",'5','.\data')

FUNCTION cr_cf()
CREATE cursor cfdtls( co c(2), ;
  yr c(2), ;
  bk c(1), ;
  avl l ,  ;
  lck l,   ;
  fls c(200);
  )
APPEND from cfdtls.csv type deli

RETURN

FUNCTION cr_files(co,yr,bk,pth)
*set step on
if type(m.pth) # 'C'
  release m.pth
  m.pth = './'
endif
debugo m.co+m.yr+m.bk
err = !(isalpha(left(m.co,1)) .and. isalpha(right(m.co,1)) .and. len(m.co) = 2)
         debugout isalph(m.co)
         debugout (isalpha(right(m.co,1)))
         debugout (len(m.co)== 2)
msg = iif(err,"Company Name Error ","")
err = err .and. !(isdigit(m.yr).and. isdigit(right(m.yr,1)) .and. len(m.yr) == 2)
msg = msg + iif(err," YEAR Error ","")
err  = err .and. !(isdigit( m.bk) .and. len(m.bk) == 1)
msg = msg + iif(err," BOOK Error ","")
IF err
  wait window msg nowait
  RETURN .f.
ELSE
  fntop = m.co + m.yr + m.bk + 'top.dbf'
  fnitm = m.co + m.yr + m.bk + 'itm.dbf'
  fninv = m.co + m.yr + 'inv.dbf'

* create file top
  IF !FILE(m.fntop)
    CREATE table (m.pth + m.fntop)( ;
      f1no c(6),;
      f1dt d,;
      f2no c(6),;
      f2dt d,;
      pcode c(10),;
      famt n(12,2),;
      Form c(10),;
      pname c(30),;
      p_addr M ,;
      o_det M ,;
      d_det M ,;
      cr n(3) ,;
      co c(2) ,;
      itxt M ,;
      gst c(15);
      )
      inde on f1no tag bno
      inde on pname tag pname
     msg = 'Created .. ' + m.pth + m.fntop
    wait window msg nowait
  ENDIF
  IF !FILE(m.fnitm)
* create file itm

    CREATE table (m.pth + m.fnitm)( ;
      bno c(6),;
      sr c(2) ,;
      TYPE c(3),;
      qty n(9,2) ,;
      unt c(5) ,;
      rate n(9,2) ,;
      dsc n(6,2) ,;
      tax n(6,2),;
      amt n(10,2),;
      iki c(8) ,;
      DESC M ,;
      hsn c(10) ;
      )
      inde on bno+sr tag bno
     msg = 'Created .. ' + m.pth + m.fnitm
    wait window msg nowait

  ENDIF
  IF !file(m.fninv)
* create inv file
    CREATE table (m.pth + m.fninv)( ;
      b_no c(10) ,;
      b_dt d ,;
      mfg c(3) ,;
      icode c(10) ,;
      asize c(3) ,;
      qty n(4) ,;
      rate n(8,2) ,;
      dsc n(6,2) ,;
      tax n(6,2) ,;
      dlv_no c(8) ,;
      dlv_dt d ,;
      dt d ,;
      iki c(8) ,;
      co c(2) ;
      )
      inde on iki tag iki
      inde on iki tag ki
      inde on icode+asize+mfg tag csm
      inde on mfg + icode + asize tag mcs
     msg = 'Created .. ' + m.pth + m.fninv
    wait window msg nowait

  ENDIF
ENDIF &&  invalid file name
RETURN .t.












