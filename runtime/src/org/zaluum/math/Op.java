package org.zaluum.math;

import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;

public abstract class Op {
  @In(x=0,y=12) public double a;
  @In(x=0,y=24) public double b;
  @Out(x=48,y=24) public double o;
}
