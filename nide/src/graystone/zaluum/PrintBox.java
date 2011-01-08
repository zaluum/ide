package graystone.zaluum;

import graystone.zaluum.annotations.Box;
import graystone.zaluum.annotations.In;

@Box
public class PrintBox {

  @In public double  a = 0;
  public void apply(){
    System.out.println("printbox "  + a);
  }
}
