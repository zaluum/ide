package org.zaluum.math;

import org.zaluum.runtime.Box;

@Box
public class Div extends DoubleOp2{
  public void apply(){
    o = a/b;
  }
}
