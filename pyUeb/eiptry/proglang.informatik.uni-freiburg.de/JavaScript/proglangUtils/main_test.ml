open Unix
open ExtList	
open ExtSMap	
open ExtSSet	
open ExtString	
open ExtUtils	
open HiddenInts	
open HistoryMap	
open Log	
open Option	
open OUnit	
open OwnMap	
open Path	
open PrioQueues
open String_of	
open ExtSQueue
open Test	

let _ =
  ignore (Test.run_tests None)
