package org.zaluum.runtime;

public abstract class RunnableBox {
  public abstract void contents(); 
  public void apply(){
    contents();
  }
}
