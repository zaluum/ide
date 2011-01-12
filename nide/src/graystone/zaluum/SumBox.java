package graystone.zaluum;

import graystone.zaluum.annotations.Box;
import graystone.zaluum.annotations.In;
import graystone.zaluum.annotations.Out;

@Box(image="icons/op.png")
public class SumBox {
  @In (x =  0 ,y = 13) public double a = 0; 
  @In (x =  0 ,y = 36) public double b = 0;
  @Out(x = 48 ,y = 21) public double c = 0;
  public void apply() {
    c=  a + b;
  }
}
