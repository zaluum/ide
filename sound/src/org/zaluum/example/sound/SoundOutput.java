package org.zaluum.example.sound;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.Mixer.Info;
import javax.sound.sampled.SourceDataLine;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class SoundOutput {
	private double deltaTime;
	static final int channels = 2;
	static final int bits = 16;
	static final float sampleRate = 44100.0f;
	AudioFormat format = new AudioFormat(sampleRate, bits, channels, true, true);
	SourceDataLine sndOut;
	byte[] buffer;
	int bufferCount = 0;
	public SoundOutput() throws LineUnavailableException {
		sndOut = AudioSystem.getSourceDataLine(format);
		
		int bufferSize = 4096;//format.getFrameSize() * Math.round(format.getSampleRate()/10);
		sndOut.open(format,bufferSize);
		System.out.println(sndOut.getBufferSize() + " sampleRate: " + format.getSampleRate());
		buffer = new byte[sndOut.getBufferSize()];
		deltaTime = 1/sampleRate;
		sndOut.start();
	}
	@Apply public double deltaTime(double[] in ) {
		for (double d : in) {
			deltaTime(d);
		}
		return deltaTime*in.length;
	}
	@Apply
	public double deltaTime(double in) {
		int s = (int)(32767.0f*Math.min(1.0,Math.max(-1.0, in)));
		byte msb = (byte) (s >>>8);
		byte lsb = (byte) s;
		buffer[bufferCount++]=msb;
		buffer[bufferCount++]=lsb;
		buffer[bufferCount++]=msb;
		buffer[bufferCount++]=lsb;
		if (bufferCount>=buffer.length) {
			bufferCount=0;
			int offset = 0;
			while (offset<buffer.length){
				offset+=sndOut.write(buffer, offset, buffer.length - offset);
			}
		}
		return deltaTime;
	}
    public void test() throws Exception {
    	int samples = 2 << 19;
		float frequency = 440;
		int sampleSizeInBytes = 2;
		byte audioBuffer[] = new byte[samples * channels * sampleSizeInBytes];

		for (int i = 0, j = 0; i < samples; ++i) {
			int wave = (int) (32767.0 * Math.sin(2.0 * Math.PI * frequency * i
					/ sampleRate));
			byte msb = (byte) (wave >>> 8);
			byte lsb = (byte) wave;

			for (int c = 0; c < channels; ++c) {
				audioBuffer[j++] = msb;
				if (sampleSizeInBytes > 1) {
					audioBuffer[j++] = lsb;
				}
			}
		}
		int offset = 0;
		while(true){
			offset += sndOut.write(audioBuffer, offset, audioBuffer.length-offset);
		}
	}
	public static void main(String[] args) throws Exception {
		for(Info i : AudioSystem.getMixerInfo()) {
			System.out.println(i);
			Mixer m= AudioSystem.getMixer(i);
			System.out.println("sources");
			for (javax.sound.sampled.Line.Info linfo : m.getSourceLineInfo()) {
				System.out.println("\t"+linfo);
			}
			System.out.println("targets");
			for (javax.sound.sampled.Line.Info linfo : m.getTargetLineInfo()) {
				System.out.println("\t"+linfo);
			}

		}
		new SoundOutput().test();
	}
}
