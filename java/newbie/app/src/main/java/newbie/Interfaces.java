package newbie;

import java.time.*;
import java.lang.*;
import java.util.*;

interface Relatable {
  public int isLargerThan(Relatable other);
}

class Point {
  private int x;
  private int y;

  Point(int x, int y) {
    this.x = x;
    this.y = y;
  }

  void setX(int x) {
    this.x = x;
  }

  void setY(int y) {
    this.y = y;
  }

}

class RectanglePlus implements Relatable {

  int width = 0;
  int height = 0;
  Point origin;

  public RectanglePlus() {
    origin = new Point(0, 0);
  }

  RectanglePlus(Point p) {
    origin = p;
  }

  RectanglePlus(int w, int h) {
    origin = new Point(0, 0);
    width = w;
    height = h;
  }

  void move(int x, int y) {
    origin.setX(x);
    origin.setY(y);
  }

  int getArea() {
    return width * height;
  }

  public int isLargerThan(Relatable other) {
    RectanglePlus otherRect = (RectanglePlus) other;
    if (this.getArea() < otherRect.getArea())
      return -1;
    else if (this.getArea() > otherRect.getArea())
      return 1;
    else
      return 0;
  }

}

interface TimeClient {
  void setTime(int hour, int minute, int second);

  void setDate(int day, int month, int year);

  void setDateAndTime(int day, int month, int year, int hour, int minute, int second);

  LocalDateTime getLocalDateTime();

  static ZoneId getZoneId(String zoneString) {
    try {
      return ZoneId.of(zoneString);
    } catch (DateTimeException e) {
      System.err.println("Invalid time zone: " + zoneString + "; using default time zone instad.");
      return ZoneId.systemDefault();
    }
  }

  default ZonedDateTime getZonedDateTime(String zoneString) {
    return ZonedDateTime.of(getLocalDateTime(), getZoneId(zoneString));
  }
}

class SimpleTimeClient implements TimeClient {
  private LocalDateTime dateAndTime;

  public SimpleTimeClient() {
    dateAndTime = LocalDateTime.now();
  }

  public void setTime(int hour, int minute, int second) {
    LocalDate currentDate = LocalDate.from(dateAndTime);
    LocalTime timeToSet = LocalTime.of(hour, minute, second);
    dateAndTime = LocalDateTime.of(currentDate, timeToSet);
  }

  public void setDate(int day, int month, int year) {
    LocalDate dateToSet = LocalDate.of(day, month, year);
    LocalTime currentTime = LocalTime.from(dateAndTime);
    dateAndTime = LocalDateTime.of(dateToSet, currentTime);
  }

  public void setDateAndTime(int day, int month, int year, int hour, int minute, int second) {
    LocalTime timeToSet = LocalTime.of(hour, minute, second);
    LocalDate dateToSet = LocalDate.of(day, month, year);
    dateAndTime = LocalDateTime.of(dateToSet, timeToSet);
  }

  public LocalDateTime getLocalDateTime() {
    return dateAndTime;
  }

  public String toString() {
    return dateAndTime.toString();
  }

  public static void main(String... args) {
    TimeClient myTimeClient = new SimpleTimeClient();
    System.out.println(myTimeClient.toString());
  }
}
