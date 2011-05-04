package org.zaluum.math;

import org.zaluum.annotation.Box;

@Box
public class Sum extends DoubleOp2{
  public void apply(){
    o = a+b;
  }
}
