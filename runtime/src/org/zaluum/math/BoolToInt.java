package org.zaluum.math;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;
import org.zaluum.annotation.Out;
@Box
public class BoolToInt {
  @In public boolean a;
  @Out public int o;
  public void apply(){
    o=a?1:0;
  }
}
