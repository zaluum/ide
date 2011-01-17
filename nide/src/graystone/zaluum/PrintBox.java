package graystone.zaluum;

import org.zaluum.nide.java.Box;
import org.zaluum.nide.java.In;

@Box(image="graystone/zaluum/print.png")
public class PrintBox {

  @In(x=0,y=0) public double  a = 0;
  public void apply(){
    System.out.println("printbox "  + a);
  }
}
