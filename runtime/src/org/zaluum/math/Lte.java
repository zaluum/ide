package org.zaluum.math;

import org.zaluum.annotation.Box;

@Box
public class Lte extends DoubleBoolOp2{
  public void apply(){
    o = a<=b;
  }
}
