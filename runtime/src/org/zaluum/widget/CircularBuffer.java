package org.zaluum.widget;

public class CircularBuffer {
	private int head = 0, tail = 0;
	private int size = 0;
	private double[] buffer;

	public CircularBuffer(int maxlen) {
		buffer = new double[maxlen];
	}

	private int toCircular(int index) {
		return (index + head) % buffer.length;
	}

	public double get(int i) {
		return buffer[toCircular(i)];
	}

	public void clear() {
		size = 0;
		head = 0;
		tail = 0;
	}

	public void add(double d) {
		buffer[tail] = d;
		tail = (tail + 1) % buffer.length;
		if (size == buffer.length) {
			head = (head + 1) % buffer.length;
		} else {
			size++;
		}
	}

	public double[] orderedCopy() {
		double[] copy = new double[size];
		System.arraycopy(buffer, head, copy, 0, size - head);
		System.arraycopy(buffer, 0, copy, size - head, head);
		return copy;
	}
}
