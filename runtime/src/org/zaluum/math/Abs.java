package org.zaluum.math;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;
@Box
public class Abs {
  @In(x=0,y=12) public double a;
  @Out(x=24,y=12) public double o;
  public void apply(){
    o=Math.abs(a);
  }
}
