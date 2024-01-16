class Date {
  int day;
  int month;
  int year;
  
  Date(int day, int month, int year) {
    this.day = day;
    this.month = month;
    this.year = year;
  }
}

class Entry {
  Date d;
  double distance;
  int duration;
  String comment;
  
  Entry (Date d, double distance, int duration, String comment) {
    this.d = d;
    this.distance = distance;
    this.duration = duration;
    this.comment = comment;
  }
}

// Beispiele f ̈ur die Klasse Entry
class EntryExample {
  Date d1 = new Date (5,6,2003);
  Entry e1 = new Entry (this.d1, 8.5, 27, "gut");
  Date d2 = new Date (6,6,2003);
  Entry e2 = new Entry (this.d2, 4.5, 24, "m ̈ude");
  
  Date d3 = new Date (23,6,2003);
  Entry e3 = new Entry (this.d3, 42.2, 150, "ersch ̈opft");
  
  EntryExample () {
  }
}

// Lauftagebuch
interface ILog {}
// leeres Tagebuch
class MTLog implements ILog {
  MTLog() {}
}
// Listenglied im Lauftagebuch
class ConsLog implements ILog {
  Entry fst;
  ILog rst;
  
  ConsLog(Entry fst, ILog rst) {
    this.fst = fst;
    this.rst = rst;
  }
}
