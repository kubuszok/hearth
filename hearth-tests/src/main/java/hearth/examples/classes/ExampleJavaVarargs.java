package hearth.examples.classes;

/** Java varargs example: {@code int... xs} erases to {@code int[]}, unlike Scala's {@code Seq[Int]}. */
public class ExampleJavaVarargs {

  public ExampleJavaVarargs() {}

  public int sum(int... xs) {
    int total = 0;
    for (int x : xs) {
      total += x;
    }
    return total;
  }
}
