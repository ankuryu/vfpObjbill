* in Item2.scr
tf = thisform
*set step on
qflg =  m.pmbknt.bk = '5'
  with tf
    .txtExpr.enabled = qflg
    .edinar.enabled = qflg
    .spnRnd.enabled = qflg
    .cmdTnc.enabled = qflg
  endwith
  return

* in frmtup.scx
tf = thisform
if m.pmbknt.bk = '5'  && Quotation
  with tf
    .lblOrdDtl.caption = 'Enq No'
     .lblDlvDtl.caption = 'Contact'
     .lblDest.caption = 'Mobile'
   endwith
 else
   with tf
     .lblOrdDtl.caption = 'Order'
     .lblDlvDtl.caption = 'Trspt'
     .lblDest.caption = 'Dest'
   endwith
endif
