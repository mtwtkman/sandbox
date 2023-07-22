package newbie.hoge;

class HogeX {}

public class ChildPackage {
  int x;

  public ChildPackage(int x) {
    this.x = x;
  }

  public void show() {
    System.out.println(this.x);
  }
}
