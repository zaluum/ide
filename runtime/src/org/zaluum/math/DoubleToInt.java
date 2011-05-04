package org.zaluum.math;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;
import org.zaluum.annotation.Out;
@Box
public class DoubleToInt {
  @In public double a;
  @Out public int o;
  public void apply(){
    o=(int)a;
  }
}
