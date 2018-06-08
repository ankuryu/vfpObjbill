* Program to test the ostk (Stock class)
* by sunil gandhi
* fname : 
* as on : 
* ver 1.0.0
* ver log ------

return
set proc to stk1itm.prg
set path to x:\pm\cm,x:\pm\99, x:\pm\18
ostk1 = createobject("ostk")
ostk1.mfg = "LDR"
ostk1.icode = "FLB123"
ostk1.asize = 'F'
clear
ostk1.get_stk()

ostk1 = NULL
return
