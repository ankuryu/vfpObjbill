*Filename cr_frm.prg
* AS on 9-sep-2015 15:02
* By 
* Purpose : creates the classes for forms to read the necessary
* Data 


frm1 = createobject('myform',300,500,200,100,"My Form")
frm1.activate()
frm1.show()

read



define class mybtn as commandbutton

Procedure init
para l,t,c    && Left,Top,Caption
this.height = 30
this.width = 60
this.caption = c
this.left = l
this.top = t
return
enddefine

define class mytext as TextBox


Procedure init
Para w,l,t  && width, left, top


this.height = 20
this.width = w
this.top = t
this.left = l

enddefine


define class myform as form

Procedure init
para h,w,l,t,c  &&  height, width, left, top and caption
this.height = h
this.width = w
this.left = l
this.top = t
this.caption = c

this.addobject("cmdExit","myBtn",(w-10-60),h-10-30  , 'Exit')
this.cmdExit.visible = .t.

return
enddefine