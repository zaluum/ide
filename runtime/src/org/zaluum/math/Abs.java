package org.zaluum.math;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;
@Box
public class Abs {
  @In public double a;
  @Out public double o;
  public void apply(){
    o=Math.abs(a);
  }
}
