package graystone.zaluum

class TestTestScala {
  val a = new ScalaConstBox();
  val b = new ScalaPrintBox();
  def apply() {
    a.apply();
    b.a = a.o;
    b.apply();
  }
}