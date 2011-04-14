package org.zaluum.math;

import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;

public abstract class DoubleOp2 {
  @In(x=0,y=6) public double a;
  @In(x=0,y=18) public double b;
  @Out(x=24,y=12) public double o;
}
