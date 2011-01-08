package org.zaluum.nide.compiler;

import graystone.zaluum.SumBox;

public class GenerationTemplate {
  public SumBox a = new SumBox();
  public SumBox b = new SumBox();
  public SumBox c = new SumBox();
  
  public void apply() {
    a.apply();
    c.a = a.c;
   /* b.apply();
    c.b = b.c;
    c.apply();*/
  }
   
}
