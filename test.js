
console.log("start");
var bno2 = new Bill('00100');
bno2.rd_frm();
var bkont1 = new BillKont()
	bkont1.init();
	bkont1.fname = "dbase.sq3" ;
console.log("Billno", bkont1.obill.bno);
	bkont1.obill.bno = '000100'
console.log(	bkont1.obill.bdt ); 

console.log(bkont1.fname);
console.log(bkont1.obill.bno);


function BillKont() {
  this.fname = "" ;
  this.maxbno = "" ;
//  this.obill = new Bill("300") ;
  this.init = function(){
	 console.log("Init started");
	 this.obill = new Bill("500");
   console.log(this.obill) 
   
   console.log("Filename :",this.fname);
  }
  this.init()
  console.log("created..",this.obill.bno);
}

function Bill(bno) {
	console.log("jango");
 this.bno = bno ;
 this.bdt = '01/04/2016' ;
 this.pname = "Panalal" ;
 this.pcode = "PMC" ;
 this.bamt = 0 ;
 this.frm = '' ;
 this.cr = 0 ;
 this.itmknt = 0 ;
 this.itmlst = [] ;


 this.rd_frm = function() {console.log("Reading Form") } 
 this.add_itm = function() { console.log("Adding Item")
  this.itmknt = itmknt+1
	 this.itmlst[this.itmknt] = new Itmo(this.bno,this.itmknt) 
 }
 this.edi_itm =  function() { console.log("EditingItem")}
 this.del_itm =  function() { console.log("Deleting Item")}
 this.prn_bil = function() { console.log("Printing Form")}
 this.put_bil = function() { console.log("Puting Bill")}
 this.get_bil = function() { console.log("Getting Bill")}
}

function Itmo(bno,sr) {
	this.bno = bno;
	this.sr = sr ;
	this.typ = "" ;
	this.unt = "" ;
	this.qty = 0 ;
	this.rate = 0 ;
	this.tax = 0 ;
	this.dsc = 0 ;
	this.amt = 0 ;
	this.desc = "" ;

	this.rd_frm = function(){ console.log("") } ;
	this.add_itm =function() { console.log("") } ;
	this.edi_itm =function() { console.log("") } ;
        this.del_itm =function() { console.log("") } ;
	this.put_itm =function() { console.log("") } ;
	this.get_itm =function() { console.log("") } ;
}
