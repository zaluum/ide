package graystone.zaluum;

import graystone.zaluum.annotations.Box;
import graystone.zaluum.annotations.Out;

@Box
public class ConstBox {
  @Out public double o = 1.0;
  public void apply(){
    System.out.println("const" + o);
  }
}
