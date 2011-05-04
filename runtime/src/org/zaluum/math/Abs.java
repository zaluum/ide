package org.zaluum.math;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;
import org.zaluum.annotation.Out;
@Box
public class Abs {
  @In public double a;
  @Out public double o;
  public void apply(){
    o=Math.abs(a);
  }
}
