package org.zaluum.example.sound;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.Line;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.Mixer.Info;
import javax.sound.sampled.SourceDataLine;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;
import org.zaluum.annotation.Out;

@Box
public class SoundOutput {
	@In double in;
	@Out double deltaTime;
	static final int channels = 2;
	static final int bits = 16;
	//static final int bufferSize = 4096;
	static final float sampleRate = 44000.0f;
	AudioFormat format = new AudioFormat(sampleRate, bits, channels, true, true);
	SourceDataLine sndOut;
	byte[] buffer;
	int bufferCount = 0;
	public SoundOutput() throws LineUnavailableException {
		sndOut = AudioSystem.getSourceDataLine(format);
		int bufferSize = format.getFrameSize() * Math.round(format.getSampleRate()/10);
		sndOut.open(format,bufferSize);
		System.out.println(sndOut.getBufferSize());
		buffer = new byte[sndOut.getBufferSize()];
		deltaTime = 1/sampleRate;
		sndOut.start();
	}
	
	public void apply() {
		int s = (int)(32767.0f*in);
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
