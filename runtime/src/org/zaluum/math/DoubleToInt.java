package org.zaluum.math;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;
@Box
public class DoubleToInt {
  @In(x=0,y=12) public double a;
  @Out(x=24,y=12) public int o;
  public void apply(){
    o=(int)a;
  }
}
