package hearth.examples.classes;

public class ExampleJavaClass {

  public ExampleJavaClass() {
    this.string = "";
    this.i = 0;
    this.b = false;
  }

  private String string;
  public String getString() { return string; }
  public void setString(String string) { this.string = string; }

  private int i;
  public int getInt() { return i; }
  public void setInt(int i) { this.i = i; }

  private boolean b;
  public boolean getBoolean() { return b; }
  public void setBoolean(boolean b) { this.b = b; }

  @Override
  public boolean equals(Object other) {
    if (other == null || !(other instanceof ExampleJavaClass)) {
      return false;
    }
    ExampleJavaClass otherClass = (ExampleJavaClass) other;
    return string.equals(otherClass.getString()) && i == otherClass.getInt() && b == otherClass.getBoolean();
  }

  @Override
  public int hashCode() {
    return scala.Tuple3.apply(string, i, b).hashCode();
  }

  @Override
  public String toString() {
    return "ExampleJavaClass(" + string + ", " + i + ", " + b + ")";
  }
}
