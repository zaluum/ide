package org.zaluum.math;

import org.zaluum.runtime.Box;

@Box
public class Sqrt extends DoubleOp1{
  public void apply(){
    o = Math.sqrt(a);
  }
}
