package hearth.examples.enums;

public enum ExampleJavaEnumWithMethods {
  VALUE1("value1"),
  VALUE2("value2");

  private ExampleJavaEnumWithMethods(String name) {
    this.name = name;
  }

  private String name;

  public String getName() {
    return this.name;
  }
}
