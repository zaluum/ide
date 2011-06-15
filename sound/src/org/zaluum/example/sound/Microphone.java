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
	double out;
	AudioFormat format = new AudioFormat(44000.0f, 16, 1, true, true);
	TargetDataLine microphone;

	public Microphone() throws LineUnavailableException {
		 
		microphone = AudioSystem.getTargetDataLine(format);
		microphone.open(format);
		microphone.start();
	}

	byte buff[] = new byte[format.getFrameSize()];
	public void apply() {
		microphone.read(buff, 0, buff.length);
		int sample = (short)(((int)buff[0] & 0xff) << 8) + ((int)buff[1] & 0xff);
		out = (sample) / 32767.0;
	}

	/*public static short getSample(byte[] buffer, int position) {
		return (short) (((buffer[position + 1] & 0xff) << 8) | (buffer[position] & 0xff));
	}*/
}
