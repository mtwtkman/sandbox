package newbie;

class Superclass {
  void printMethod() {
    System.out.println("Printed in Superclass.");
  }
}

class Subclass extends Superclass {
  void printMethod() {
    super.printMethod();
    System.out.println("Printed in Subclass");
  }

  static void main() {
    Subclass s = new Subclass();
    s.printMethod();
  }
}

class SuperclassC implements Cloneable {
  SuperclassC() {
    super();
  }

  protected SuperclassC clone() throws CloneNotSupportedException {
    return new SuperclassC();
  }

  void hoge() {
    try {
      SuperclassC c = clone();
    }
    finally {
      return ;
    }
  }
}
