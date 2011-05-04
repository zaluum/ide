package org.zaluum.basic;

public abstract class RunnableBox {
  public abstract void contents(); 
  public void apply(){
    contents();
  }
}
