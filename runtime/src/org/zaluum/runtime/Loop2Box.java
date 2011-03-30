package org.zaluum.runtime;

@Box
public abstract class Loop2Box extends RunnableBox{
  @Out(x=0,y=48) public boolean cond2;
  @Override
  public void apply() {
    do{
      contents();
    }while(cond2);
  }
}
