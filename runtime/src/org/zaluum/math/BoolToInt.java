package org.zaluum.math;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;
@Box
public class BoolToInt {
  @In(x=0,y=12) public boolean a;
  @Out(x=24,y=12) public int o;
  public void apply(){
    o=a?1:0;
  }
}
