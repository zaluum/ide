package graystone.zaluum;

import graystone.zaluum.annotations.Box;
import graystone.zaluum.annotations.In;
import graystone.zaluum.annotations.Out;

@Box
public class SumBox {
  @In public double   a = 0; 
  @In public double b = 0;
  @Out public double c = 0;
  public void apply() {
    c=  a + b;
  }
}
