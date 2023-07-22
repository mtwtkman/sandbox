package newbie;

import newbie.hoge.ChildPackage;

public class App {
  public static void main(String[] args) {
    BoxDemo.main();
    ChildPackage c = new ChildPackage(1);
    c.show();
  }
}

class OuterClass {
  String outerField = "Outer field";
  static String staticOuterField = "Static outer field";

  class InnerClass {
    void accessMembers() {
      System.out.println(outerField);
      System.out.println(staticOuterField);
    }
  }

  public class StaticNestedClass {
    void accessMembers(OuterClass outer) {
      System.out.println(outerField);
      System.out.println(outer.outerField);
      System.out.println(staticOuterField);
    }
  }

  public static void main(String[] args) {
    System.out.println("Inner class:");
    System.out.println("------------");
    OuterClass outerObject = new OuterClass();
    OuterClass.InnerClass innerObject = outerObject.new InnerClass();
    innerObject.accessMembers();

    System.out.println("\nStatic nested class:");
    System.out.println("----------------------");
    StaticNestedClass staticNestedObject = outerObject.new StaticNestedClass();
    staticNestedObject.accessMembers(outerObject);

    System.out.println("\nTop-level class:");
    System.out.println("------------------");
    TopLevelClass topLevelObject = new TopLevelClass();
    topLevelObject.accessMembers(outerObject);
  }
}

class TopLevelClass {
  void accessMembers(OuterClass outer) {
    System.out.println(outer.outerField);
    System.out.println(OuterClass.staticOuterField);
  }
}

class DataStructure {
  private final static int SIZE = 15;
  private int[] arrayOfInts = new int[SIZE];

  public DataStructure() {
    for (int i = 0; i < SIZE; i++) {
      arrayOfInts[i] = i;
    }
  }

  public void printEven() {
    DataStructureIterator iterator = this.new EvenIterator();
    while (iterator.hasNext()) {
      System.out.print(iterator.next() + " ");
    }
    System.out.println();
  }

  interface DataStructureIterator extends java.util.Iterator<Integer> {
  }

  private class EvenIterator implements DataStructureIterator {
    private int nextIndex = 0;

    public boolean hasNext() {
      return (nextIndex <= SIZE - 1);
    }

    public Integer next() {
      Integer retValue = Integer.valueOf(arrayOfInts[nextIndex]);
      nextIndex += 2;
      return retValue;
    }
  }

  public static void main(String[] s) {
    DataStructure ds = new DataStructure();
    ds.printEven();
  }
}


enum E {
  A (1, 2),
  B (3, 4);

  private final int x;
  private final int y;

  E(int x, int y) {
    System.out.println("called" + this);
    this.x = x;
    this.y = y;
  }
}
