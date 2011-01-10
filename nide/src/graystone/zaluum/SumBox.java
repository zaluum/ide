package graystone.zaluum;

import graystone.zaluum.annotations.Box;
import graystone.zaluum.annotations.In;
import graystone.zaluum.annotations.Out;

@Box(image="icons/op.png")
public class SumBox {
  @In (x =  0 ,y =  5) public double a = 0; 
  @In (x =  0 ,y = 20) public double b = 0;
  @Out(x = 32 ,y = 10) public double c = 0;
  public void apply() {
    c=  a + b;
  }
}
