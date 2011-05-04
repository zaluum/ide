package org.zaluum.math;

import org.zaluum.annotation.Box;

@Box
public class Pow extends DoubleOp2{
  public void apply(){
    o = Math.pow(a, b);
  }
}
