* Program to set one time code
* on basis of the 1st Alphabet of the name and 
* balance 4 numerical seq

close data
use tmpq3
inde on pname to tmp1
go top
m.prvalf = left(pname,1)
seq = 1
m.palf = left(pname,1)   
do while !eof()
    m.pcode = m.palf + tran(m.seq,'@l ####')
   repl pcode with  m.pcode
   ? pcode,pname
   skip
   m.palf = left(pname,1)
   if m.palf == m.prvalf
     seq = seq+1
   else
     seq = 1
     m.prvalf = m.palf
   endif
enddo
