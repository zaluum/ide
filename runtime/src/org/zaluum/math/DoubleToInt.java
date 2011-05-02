package org.zaluum.math;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;
@Box
public class DoubleToInt {
  @In public double a;
  @Out public int o;
  public void apply(){
    o=(int)a;
  }
}
