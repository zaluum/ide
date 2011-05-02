package org.zaluum.math;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;
@Box
public class BoolToInt {
  @In public boolean a;
  @Out public int o;
  public void apply(){
    o=a?1:0;
  }
}
