package newbie;

class Bicycle {
  int gear;
  int cadence;
  int speed;

  public Bicycle(int cadence, int speed, int gear) {
    this.cadence = cadence;
    this.speed = speed;
    this.gear = gear;
  }

  void printDescription() {
    System.out.println("\nBike is " + this.gear + " with a cadence of " + this.cadence
        + " and travelling at a speed of " + this.speed + ".");
  }
}

class MountainBike extends Bicycle {
  private String suspension;

  public MountainBike(
      int startCadence,
      int startSpeed,
      int startGear,
      String suspensionType) {
    super(startCadence, startSpeed, startGear);
    this.setSuspension(suspensionType);
  }

  public String getSuspension() {
    return this.suspension;
  }

  public void setSuspension(String suspensionType) {
    this.suspension = suspensionType;
  }

  public void printDescription() {
    super.printDescription();
    System.out.println("The " + "MountainBike has a" + getSuspension() + " suspension.");
  }
}

class RoadBike extends Bicycle {
  private int tireWidth;

  public RoadBike(
    int startCadence,
    int startSpeed,
    int startGear,
    int newTireWidth
  ) {
    super(startCadence, startSpeed, startGear);
    this.setTireWidth(newTireWidth);
  }

  public int getTireWidth() {
    return this.tireWidth;
  }

  public void setTireWidth(int newTireWidth) {
    this.tireWidth = newTireWidth;
  }

  public void printDescription() {
    super.printDescription();
    System.out.println("The RoadBike has " + getTireWidth() + " MM tires.");
  }
}

class TestBikes {
  public static void main() {
    Bicycle bike01 = new Bicycle(20, 10, 1);
    MountainBike bike02 = new MountainBike(20, 10, 5, "Dual");
    RoadBike bike03 = new RoadBike(40, 20, 8, 23);

    bike01.printDescription();
    bike02.printDescription();
    bike03.printDescription();
  }
}
