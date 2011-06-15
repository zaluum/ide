package org.zaluum.example.sound;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.TargetDataLine;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.Out;
@Box
public class Microphone {
	@Out
	short[] o;
	AudioFormat format = new AudioFormat(8000.0f, 16, 1, true, true);
	TargetDataLine microphone;

	public Microphone() throws LineUnavailableException {
		 
		microphone = AudioSystem.getTargetDataLine(format);
		microphone.open(format);
		microphone.start();
	}

	public void apply() {
		byte buff[] = new byte[microphone.getBufferSize() / 5];
		int numRead = microphone.read(buff, 0, buff.length);
		o = new short[numRead/2];
		for (int i = 0; i < numRead; i += 2) {
			o[i / 2] = getSample(buff, i);
		}
	}

	public static short getSample(byte[] buffer, int position) {
		return (short) (((buffer[position + 1] & 0xff) << 8) | (buffer[position] & 0xff));
	}
}
