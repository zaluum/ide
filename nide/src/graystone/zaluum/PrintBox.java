package graystone.zaluum;

import graystone.zaluum.annotations.Box;
import graystone.zaluum.annotations.In;

@Box(image="icons/print.png")
public class PrintBox {

  @In(x=0,y=0) public double  a = 0;
  public void apply(){
    System.out.println("printbox "  + a);
  }
}
