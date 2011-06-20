package org.zaluum.benchmark.allobjects;

public abstract class Loop {
  public abstract void contents();
  public abstract boolean cond();
  public void apply() {
    do{
      contents();
    }while(cond());
  }
}
