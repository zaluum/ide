package org.zaluum.math;

import org.zaluum.annotation.Box;

@Box
public class Sqrt extends DoubleOp1{
  public void apply(){
    o = Math.sqrt(a);
  }
}
