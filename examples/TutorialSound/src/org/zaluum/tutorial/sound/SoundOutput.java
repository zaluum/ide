package org.zaluum.tutorial.sound;

import javax.sound.sampled.*;
import org.zaluum.annotation.*;

@Box
public class SoundOutput {
	private final SourceDataLine sndOut;
	private final byte[] buffer;
	private int bufferCount = 0;
	public SoundOutput() throws LineUnavailableException {
		sndOut = AudioSystem.getSourceDataLine(WavSoundInput.format);
		int bufferSize = 4096;
		sndOut.open(WavSoundInput.format,bufferSize);
		buffer = new byte[sndOut.getBufferSize()];
		sndOut.start();
	}
	@Apply public void playSamples(double[] in ) {
		for (double d : in) {
			playSample(d);
		}
	}
	private void playSample(double in) {
		int s = (int)(32767.0*Math.min(1.0,Math.max(-1.0, in)));
		byte msb = (byte) (s >>> 8);
		byte lsb = (byte) s;
		buffer[bufferCount++]=lsb;
		buffer[bufferCount++]=msb;
		buffer[bufferCount++]=lsb;
		buffer[bufferCount++]=msb;
		if (bufferCount>=buffer.length) {
			bufferCount=0;
			int offset = 0;
			while (offset<buffer.length){
				offset+=sndOut.write(buffer, offset, buffer.length - offset);
			}
		}
	}
}
