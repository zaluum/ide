package graystone.zaluum;

public class TestTest {
  public ConstBox a = new ConstBox();
  public PrintBox b = new PrintBox();
  public void apply() {
    a.apply();
    b.a = a.o;
    b.apply();
  }
}
