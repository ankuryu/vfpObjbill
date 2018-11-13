* Program to make Eway Bill Using Data from Invoices and getting other  Data
*
* Filename mk_eway.prg
* as on
* by Sunil Gandhi


close data
set date brit
set excl off

Para m.co,m.yr,m.bk,m.bno




Define Class ewaykont as custom
ewy = NULL  && 




Enddefine

Define Class ewy
userGstin ='27'
supplyType = 'O'
subSupplyType = 1
docType = 'INV'
docNo = m.f1no
docDate m.f1dt
fromGstin = userGstin && C(15)
fromTrdName = m.coname
fromAddr1 = m.coaddr1
fromAddr2  = m.coaddr2
fromPlace  = m.coPlace
fromPincode = 400003
fromStateCode = 27
actualFromStateCode =27
toGstin =   &&  C(15)
toTrdName =m.pname && C(15)
toAddr1
toAddr2
toPlace
toPincode
toStateCode
actualToStateCode
totalValue
cgstValue
sgstValue
igstValue
cessValue
TotNonAdvolValue
OthValue
transMode = 1
transDistance = m.distance
transporterName
transperterId
transDocNo
transDocDate
vehicleNo 
vehicleType = "R"
mainHsnCode
itemList = Dime[1]

Enddefine

Define Class ewy_itm as custom
itemNo = 0
productName = ""
producDesc = ""
hsnCode = 00000000
quantity = 000000000000
qtyUnit = "NOS"
taxableAmount = 0
sgstRate = 0
cgstRate = 0
igstRate = 0
cessRate = 0
cessNonAdvol = 0

Enddefine