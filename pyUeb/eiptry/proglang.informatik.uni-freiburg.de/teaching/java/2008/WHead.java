////begin_preamble
// Interface und Funktionalit?t f?r Suchbaum
class WHead {
  WTree wt;
  private WTree wmt = new WMT ();

  public WHead () {
    this.wt = wmt;
  }

  public Wine find (String name) {
    WTree wtree = this.wt;
    return null;
  }
  public void change (String name, int newprice) {
    WTree wtree = this.wt;
  }
    ////break
  public void add (Wine w) {
    if (this.wt.isEmpty ()) {
      this.wt = new WNode (w, wmt, wmt); return;
    } else {
      String name = w.name;
      WTree wtree = this.wt; // bekannt: !wtree.empty()
      while (!wtree.isEmpty()) {
        Wine e = wtree.getEntry();
        int r = e.compareName (name);
        if (r == 0) { // ?berschreibe vorhandenen Eintrag
          wtree.setEntry(w); return;
        } else {
          if (r > 0) {
            WTree w1 = wtree.getLeft();
            if (w1.isEmpty ()) {
              wtree.setLeft(new WNode (w, wmt, wmt)); return;
            } else {
              wtree = w1;
            }
          } else {
            WTree w1 = wtree.getRight();
            if (w1.isEmpty()) {
              wtree.setRight(new WNode (w, wmt, wmt)); return;
            } else {
              wtree = w1;
            }
          }
        }
      }
      Util.error ("this cannot happen");
    }
  }
}
////end_postamble

interface WTree {
  public boolean isEmpty();
  public Wine getEntry();
  public WTree getLeft();
  public WTree getRight();
  public void setEntry(Wine w);
  public void setLeft(WTree wt);
  public void setRight(WTree wt);
}

class WMT implements WTree {
  public WMT () {}
  
  public boolean isEmpty() { return true; }
  public Wine getEntry() { return null; }
  public WTree getLeft() { return null; }
  public WTree getRight() { return null; }
  public void setEntry(Wine w) { return; }
  public void setLeft(WTree wt) { return; }
  public void setRight(WTree wt) { return; }
}

class WNode implements WTree {
  private Wine entry;
  private WTree left;
  private WTree right;
  public WNode(Wine entry, WTree left, WTree right) {
    this.entry = entry;
    this.left = left;
    this.right = right;
  }
  public boolean isEmpty() { return false; }
  public Wine getEntry() { return this.entry; }
  public WTree getLeft() { return this.left; }
  public WTree getRight() { return this.right; }
  public void setEntry(Wine w) { this.entry = w; return; }
  public void setLeft(WTree wt) { this.left = wt; return; }
  public void setRight(WTree wt) { this.right = wt; return; }
}

class Wine {
  String name;
  int price;
  public Wine (String name, int price) {
    this.name = name;
    this.price = price;
  }
  public int compareName(String name) {
    return this.name.compareTo(name);
  }
}