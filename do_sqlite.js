// file name : 
// File to create an object which shall interface with server and sqlite db file
//  uses  the following 
//  php, jquery 

function Sqliobj() {
 this.fname = '' ;  // file name of database to interface
 this.tbl = "" ;    //  Table name of the database
 this.sql = "" ;    //  The sql string  which will do the operation
 this.stat = "" ;   //  Result status of opr
 this.data = "" ;   //  Data as an obj for sending/receiving
 this.php  = "" ;   //  php file which will interacti with the server

this.do_it = function(){ console.log("I am doing it") ;
 $.get
}
