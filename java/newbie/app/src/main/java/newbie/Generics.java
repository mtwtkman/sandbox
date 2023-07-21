package newbie;

import java.util.ArrayList;
import java.util.List;

class Box<T> {
  private T t;

  void set(T t) {
    this.t = t;
  }

  T get() {
    return this.t;
  }

  <U extends Number> void inspect(U u) {
    System.out.println("T: " + t.getClass().getName());
    System.out.println("U: " + u.getClass().getName());
  }

  static void main() {
    Box<Integer> box = new Box<>();
    box.set(new Integer(10));
    box.inspect(new Double(3));
  }
}

class NaturalNumber<T extends Integer> {
  private T n;

  NaturalNumber(T n) {
    this.n = n;
  }

  boolean isEven() {
    return n.intValue() % 2 == 0;
  }
}

class BoxDemo {
  static <U> void addBox(U u, java.util.List<Box<U>> boxes) {
    Box<U> box = new Box<>();
    box.set(u);
    boxes.add(box);
  }

  static <U> void outputBoxes(java.util.List<Box<U>> boxes) {
    int counter = 0;
    for (Box<U> box : boxes) {
      U boxContents = box.get();
      System.out.println("Box #" + counter + " contains [" + boxContents.toString() + "]");
      counter++;
    }
  }

  static void main() {
    java.util.ArrayList<Box<Integer>> listOfIntegerBoxes = new java.util.ArrayList<>();
    BoxDemo.<Integer>addBox(Integer.valueOf(10), listOfIntegerBoxes);
    BoxDemo.addBox(Integer.valueOf(20), listOfIntegerBoxes);
    BoxDemo.addBox(Integer.valueOf(30), listOfIntegerBoxes);
    BoxDemo.outputBoxes(listOfIntegerBoxes);
  }
}

class MyClass<X2> {
  <T> MyClass(T t) {
  }

  static void main() {
    MyClass<Integer> x = new MyClass<>("");
  }
}

class NatNum {
  private int i;

  NatNum(int i) {
    this.i = i;
  }
}

class EvenNum extends NatNum {
  EvenNum(int i) {
    super(i);
  }

  static void main() {
    List<NatNum> le = new ArrayList<>();
    List<? super EvenNum> ln = le;
  }
}
