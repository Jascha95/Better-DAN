// 2.1

interface class Event {
   getDuration();
}

class Concert implements Event {
   int start, end;
   getDuration() {
	  return (end-start);
   }	
}

class ConcertSeries implements Event {
   Event[] events;
   getStart() {
	  int start;
	  for (event in events) {
		 if(event instanceof Concert) {
			start = (event.start<start) ? event.start : start;
		 } else if (event instanceof ConcertSeries) {
			start = (event.getStart()<start) ? event.getStart() : start;
		 }
	  }
   }
   getEnd() {
	  int end;
	  for (event in events) {
		 if(event instanceof Concert) {
			end = (event.end>end) ? event.end : end;
		 } else if (event instanceof ConcertSeries) {
			end = (event.getEnd()>end) ? event.getEnd() : end;
		 }
	  }
   }
   getDuration() {
	  return (getEnd()-getStart());
   }
}


// 2.2


interface Visitor {
   visit(Concert);
   visit(ConcertSeries);
}

class DurationVisitor {
   int start, end;
   visit(Concert concert) {
	  start = (concert.start<start) ? concert.start : start;
	  end = (concert.end>end) ? concert.end : end;
   }
   visit(ConcertSeries series) {
	  for (event in series.events) {
		 event.accept(this);
	  }
   }
   getDuration() {
	  return (end-start);
   }
}

interface class Event {
   // [..]
   accept(Visitor);
}

class Concert implements Event {
   // [..]
   accept(Visitor visitor) {
	  visitor.visit(this);
   }
}

class ConcertSeries implements Event {
   // [..]
   accept(Visitor visitor) {
	  visitor.visit(this);
   }
}
