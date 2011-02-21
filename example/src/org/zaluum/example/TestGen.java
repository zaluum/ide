package org.zaluum.example;

public class TestGen {
	public SliderWidget box1 = new SliderWidget();
	public TextWidget box = new TextWidget();
	
	public void apply(){
		box1.apply();
		box.d = box1.out;
		box.apply();
	}
}
