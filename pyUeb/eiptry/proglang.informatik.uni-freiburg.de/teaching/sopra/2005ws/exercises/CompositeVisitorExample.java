import java.util.*;

interface FileSystemVisitor {
    Object visitFile(File f);
    Object visitDirectory(Directory d);
}

abstract class FileSystemEntry {

    private String name;

    FileSystemEntry(String name) {
        this.name = name;
    }

    String getName() {
        return name;
    }

    abstract Object accept(FileSystemVisitor v);
}

class File extends FileSystemEntry {

    private int fd;

    File(String name, int fd) {
        super(name);
        this.fd = fd;
    }

    Object accept(FileSystemVisitor v) {
        return v.visitFile(this);
    }
}

class Directory extends FileSystemEntry {

    private List children;

    Directory(String name) {
        super(name);
        children = new ArrayList();
    }

    Iterator getChildren() {
        return children.iterator();
    }

    void addChild(FileSystemEntry e) {
        children.add(e);
    }

    Object accept(FileSystemVisitor v) {
        return v.visitDirectory(this);
    }
}

class NamesVisitor implements FileSystemVisitor {

    public Object visitFile(File f) {
        List l = new ArrayList();
        l.add(f.getName());
        return l;
    }

    public Object visitDirectory(Directory d) {
        List l = new ArrayList();
        for (Iterator it = d.getChildren(); it.hasNext(); ) {
            FileSystemEntry child = (FileSystemEntry) it.next();
            List childList = (List) child.accept(this);
            l.addAll(childList);
        }
        // l.add(d.getName());
        return l;
    }

}

public class CompositeVisitorExample {

    public static void main(String[] args) {
        Directory root = new Directory(null);
        Directory home = new Directory("home");
        Directory etc = new Directory("etc");
        Directory stefan = new Directory("stefan");
        Directory helge = new Directory("helge");
        Directory max = new Directory("max");
        File mailStefan = new File("mail", 1);    
        File mailMax = new File("mail", 2);
        File passwd = new File("passwd", 3);
        
        root.addChild(home);
        home.addChild(stefan);
        home.addChild(helge);
        home.addChild(max);
        stefan.addChild(mailStefan);
        max.addChild(mailMax);
        root.addChild(etc);
        etc.addChild(passwd);

        FileSystemVisitor v = new NamesVisitor();
        List allNames = (List) root.accept(v);
        System.out.println(allNames);
    }
}