package org.zaluum.math;

import org.zaluum.runtime.Box;

@Box
public class Pow extends DoubleOp2{
  public void apply(){
    o = Math.pow(a, b);
  }
}
