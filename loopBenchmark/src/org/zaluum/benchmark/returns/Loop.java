package org.zaluum.benchmark.returns;

public abstract class Loop {
  public abstract void contents();
  public abstract boolean cond();
  public void apply() {
    do{
      contents();
    }while(cond());
  }
}
