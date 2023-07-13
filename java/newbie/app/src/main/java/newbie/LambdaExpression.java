package newbie;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.Comparator;
import java.util.function.Predicate;
import java.lang.Iterable;
import java.time.chrono.IsoChronology;

class Person {
  public enum Sex {
    MALE,
    FEMALE,
  }

  int age;
  String emailAddress;
  Sex gender;

  public Person(int age, Sex gender, String emailAddress) {
    this.age = age;
    this.gender = gender;
    this.emailAddress = emailAddress;
  }

  public Sex getGender() {
    return gender;
  }

  public int getAge() {
    return age;
  }

  public String getEmailAddress() {
    return emailAddress;
  }

  public void printPerson() {
    System.out.print("age: " + age);
  }
}

class RosterTest {
  interface CheckPerson {
    boolean test(Person p);
  }

  public static void printPersonOlderThan(List<Person> roster, int age) {
    for (Person p : roster) {
      if (p.getAge() >= age) {
        p.printPerson();
      }
    }
  }

  public static void printPerson(List<Person> roster, CheckPerson tester) {
    for (Person p : roster) {
      if (tester.test(p)) {
        p.printPerson();
      }
    }
  }

  public static void processPerson(
      List<Person> roster,
      Predicate<Person> tester,
      Consumer<Person> block) {
    for (Person p : roster) {
      if (tester.test(p)) {
        block.accept(p);
      }
    }
  }

  public static void processPersonWithFunction(
      List<Person> roster,
      Predicate<Person> tester,
      Function<Person, String> mapper,
      Consumer<String> block) {
    for (Person p : roster) {
      if (tester.test(p)) {
        String data = mapper.apply(p);
        block.accept(data);
      }
    }
  }

  public static <X, Y> void processElements(
      Iterable<X> source,
      Predicate<X> tester,
      Function<X, Y> mapper,
      Consumer<Y> block) {
    for (X p : source) {
      if (tester.test(p)) {
        Y data = mapper.apply(p);
        block.accept(data);
      }
    }
  }

  void print1(List<Person> roster) {
    printPerson(
        roster,
        p -> p.getGender() == Person.Sex.MALE
            && p.getAge() >= 18
            && p.getAge() <= 25);
  }

  void print2(List<Person> roster) {
    processPerson(
        roster,
        p -> p.getGender() == Person.Sex.MALE && p.getAge() >= 18 && p.getAge() <= 25,
        p -> p.printPerson());
  }

  void print3(List<Person> roster) {
    processPersonWithFunction(
        roster,
        p -> p.getGender() == Person.Sex.MALE && p.getAge() >= 18 && p.getAge() <= 25,
        p -> p.getEmailAddress(),
        email -> System.out.println(email));
  }

  void processEmail(List<Person> roster) {
    processElements(
        roster,
        p -> p.getGender() == Person.Sex.MALE && p.getAge() >= 18 && p.getAge() <= 25,
        p -> p.getEmailAddress(),
        email -> System.out.println(email));
  }

  void useStream(List<Person> roster) {
    roster
        .stream()
        .filter(
            p -> {
            boolean p1 = p.getGender() == Person.Sex.MALE;
            return p1 && p.getAge() >= 18 && p.getAge() <= 25;
        })
        .map(

            p -> p.getEmailAddress())
        .forEach(email -> System.out.println(email));
  }
}

class MethodReferencesExamples {
  public static <T> T mergeThings (T a, T b, BiFunction<T, T, T> merger) {
    return merger.apply(a,b);
  }

  public static String appendStrings(String a, String b) {
    return a + b;
  }

  public String appendStrings2(String a, String b) {
    return a + b;
  }

  public static void main() {
    MethodReferencesExamples myApp = new MethodReferencesExamples();

    System.out.println(MethodReferencesExamples.mergeThings("Hello ", "World!", (a,b) -> a + b));
    System.out.println(MethodReferencesExamples.mergeThings("Hello ", "World!", MethodReferencesExamples::appendStrings));
    System.out.println(MethodReferencesExamples.mergeThings("Hello ", "World!", myApp::appendStrings2));
    System.out.println(MethodReferencesExamples.mergeThings("Hello ", "World!", String::concat));
  }
}
