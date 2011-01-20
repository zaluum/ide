package graystone.zaluum;

import org.zaluum.nide.java.Box;
import org.zaluum.nide.java.Out;

@Box
public class ConstBox {
  @Out(x=10,y=10) public double o = 1.0;
  public void apply(){
    System.out.println("const" + o);
  }
}
