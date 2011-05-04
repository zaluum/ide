package org.zaluum.basic;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.Out;


@Box
public class TrueBox extends RunnableBox{
	@Out public boolean out = true;
	public void contents(){}
}
