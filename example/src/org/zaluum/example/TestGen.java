package org.zaluum.example;

import java.lang.reflect.InvocationTargetException;

public class TestGen {
	public SliderWidget box1 = new SliderWidget();
	public TextWidget box = new TextWidget();
	
	public void apply() throws InterruptedException, InvocationTargetException{
		box1.apply();
		box.d = box1.out;
		box.apply();
	}
}
