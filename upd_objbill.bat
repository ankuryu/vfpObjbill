rar u objBill.rar *
xcopy objBill.rar y:\dropbox\private\dev\vfp\objbill
y:
cd dropbox\private\dev\vfp\objbill
rar e -f objbill.rar
if NOT EXIST f:\objBill goto end
f:
cd \objbill
xcopy c:\objbill\objbill.rar .
rar e -f objbill.rar 
 
:end
