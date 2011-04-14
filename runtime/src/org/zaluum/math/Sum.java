package org.zaluum.math;

import org.zaluum.runtime.Box;

@Box
public class Sum extends Op{
  public void apply(){
    o = a+b;
  }
}
