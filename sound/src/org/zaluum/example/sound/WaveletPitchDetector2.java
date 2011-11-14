package org.zaluum.example.sound;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.StaticBox;
/*

Dynamic Wavelet Algorithm Pitch Tracking library
Released under the MIT open source licence
 
Copyright (c) 2010 Antoine Schmitt

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
@StaticBox
public class WaveletPitchDetector2 {
	private static int power2(int i) {
		int res = 1,j;
		for (j=0;j<i;j++) res <<=1;
		return res;
	}
	private static int iabs(int i) {
		if (i<0) return -i;
		else return i;
	}
	@Box
	public static double computeWaveletPitch(double[] samples) {
		double ampThres = 0.2;
		double pitchF = 0.0;
		
		int i, j;
		double si, si1;
		
		// must be a power of 2
		//samplecount = _floor_power2(samplecount);
		
		double[] sam = samples.clone();
		int samplecount = samples.length; 
		int curSamNb = samplecount;
		
		int[] distances = new int[samplecount];
		int[] mins = new int[samplecount];
		int[] maxs = new int[samplecount];
		int nbMins, nbMaxs;
		
		// algorithm parameters
		int maxFLWTlevels = 6;
		double maxF = 3000.;
		int differenceLevelsN = 3;
		double maximaThresholdRatio = 0.75;
		
		double ampltitudeThreshold;  
		double theDC = 0.0;
		
		{ // compute ampltitudeThreshold and theDC
			//first compute the DC and maxAMplitude
			double maxValue = 0.0;
			double minValue = 0.0;
			for (i = 0; i < samplecount;i++) {
				si = sam[i];
				theDC = theDC + si;
				if (si > maxValue) maxValue = si;
				if (si < minValue) minValue = si;
			}
			theDC = theDC/samplecount;
			maxValue = maxValue - theDC;
			minValue = minValue - theDC;
			double amplitudeMax = (maxValue > -minValue ? maxValue : -minValue);
			
			ampltitudeThreshold = Math.max(amplitudeMax*maximaThresholdRatio,ampThres);
			//asLog("dywapitch theDC=%f ampltitudeThreshold=%f\n", theDC, ampltitudeThreshold);
			
		}
		
		// levels, start without downsampling..
		int curLevel = 0;
		double curModeDistance = -1.;
		int delta;
		
		while(true) {
			
			// delta
			delta = (int)(44100.0/(power2(curLevel)*maxF));
			//("dywapitch doing level=%ld delta=%ld\n", curLevel, delta);
			
			if (curSamNb < 2) return pitchF;
			
			// compute the first maximums and minumums after zero-crossing
			// store if greater than the min threshold
			// and if at a greater distance than delta
			double dv, previousDV = -1000;
			nbMins = nbMaxs = 0;   
			int lastMinIndex = -1000000;
			int lastmaxIndex = -1000000;
			int findMax = 0;
			int findMin = 0;
			for (i = 2; i < curSamNb; i++) {
				si = sam[i] - theDC;
				si1 = sam[i-1] - theDC;
				
				if (si1 <= 0 && si > 0) findMax = 1;
				if (si1 >= 0 && si < 0) findMin = 1;
				
				// min or max ?
				dv = si - si1;
				
				if (previousDV > -1000) {
					
					if (findMin!=0 && previousDV < 0 && dv >= 0) { 
						// minimum
						if (Math.abs(si) >= ampltitudeThreshold) {
							if (i > lastMinIndex + delta) {
								mins[nbMins++] = i;
								lastMinIndex = i;
								findMin = 0;
								//if DEBUGG then put "min ok"&&si
								//
							} else {
								//if DEBUGG then put "min too close to previous"&&(i - lastMinIndex)
								//
							}
						} else {
							// if DEBUGG then put "min "&abs(si)&" < thresh = "&ampltitudeThreshold
							//--
						}
					}
					
					if (findMax!=0 && previousDV > 0 && dv <= 0) {
						// maximum
						if (Math.abs(si) >= ampltitudeThreshold) {
							if (i > lastmaxIndex + delta) {
								maxs[nbMaxs++] = i;
								lastmaxIndex = i;
								findMax = 0;
							} else {
								//if DEBUGG then put "max too close to previous"&&(i - lastmaxIndex)
								//--
							}
						} else {
							//if DEBUGG then put "max "&abs(si)&" < thresh = "&ampltitudeThreshold
							//--
						}
					}
				}
				
				previousDV = dv;
			}
			
			if (nbMins == 0 && nbMaxs == 0) {
				// no best distance !
				//asLog("dywapitch no mins nor maxs, exiting\n");
				
				// if DEBUGG then put "no mins nor maxs, exiting"
				return pitchF;
			}
			//if DEBUGG then put count(maxs)&&"maxs &"&&count(mins)&&"mins"
			
			// maxs = [5, 20, 100,...]
			// compute distances
			int d;
			for (i=0;i<distances.length;i++)
				distances[i] = 0;
			for (i = 0 ; i < nbMins ; i++) {
				for (j = 1; j < differenceLevelsN; j++) {
					if (i+j < nbMins) {
						d = iabs(mins[i] - mins[i+j]);
						//asLog("dywapitch i=%ld j=%ld d=%ld\n", i, j, d);
						distances[d] = distances[d] + 1;
					}
				}
			}
			for (i = 0 ; i < nbMaxs ; i++) {
				for (j = 1; j < differenceLevelsN; j++) {
					if (i+j < nbMaxs) {
						d = iabs(maxs[i] - maxs[i+j]);
						//asLog("dywapitch i=%ld j=%ld d=%ld\n", i, j, d);
						distances[d] = distances[d] + 1;
					}
				}
			}
			
			// find best summed distance
			int bestDistance = -1;
			int bestValue = -1;
			for (i = 0; i< curSamNb; i++) {
				int summed = 0;
				for (j = -delta ; j <= delta ; j++) {
					if (i+j >=0 && i+j < curSamNb)
						summed += distances[i+j];
				}
				//asLog("dywapitch i=%ld summed=%ld bestDistance=%ld\n", i, summed, bestDistance);
				if (summed == bestValue) {
					if (i == 2*bestDistance)
						bestDistance = i;
					
				} else if (summed > bestValue) {
					bestValue = summed;
					bestDistance = i;
				}
			}
			//asLog("dywapitch bestDistance=%ld\n", bestDistance);
			
			// averaging
			double distAvg = 0.0;
			double nbDists = 0;
			for (j = -delta ; j <= delta ; j++) {
				if (bestDistance+j >=0 && bestDistance+j < samplecount) {
					int nbDist = distances[bestDistance+j];
					if (nbDist > 0) {
						nbDists += nbDist;
						distAvg += (bestDistance+j)*nbDist;
					}
				}
			}
			// this is our mode distance !
			distAvg /= nbDists;
			//asLog("dywapitch distAvg=%f\n", distAvg);
			
			// continue the levels ?
			if (curModeDistance > -1.) {
				double similarity = Math.abs(distAvg*2 - curModeDistance);
				if (similarity <= 2*delta) {
					//if DEBUGG then put "similarity="&similarity&&"delta="&delta&&"ok"
	 				//asLog("dywapitch similarity=%f OK !\n", similarity);
					// two consecutive similar mode distances : ok !
					pitchF = 44100./(power2(curLevel-1)*curModeDistance);
					return pitchF;
				}
				//if DEBUGG then put "similarity="&similarity&&"delta="&delta&&"not"
			}
			
			// not similar, continue next level
			curModeDistance = distAvg;
			
			curLevel = curLevel + 1;
			if (curLevel >= maxFLWTlevels) {
				// put "max levels reached, exiting"
	 			//asLog("dywapitch max levels reached, exiting\n");
				return pitchF;
			}
			
			// downsample
			if (curSamNb < 2) {
	 			//asLog("dywapitch not enough samples, exiting\n");
				return pitchF;
			}
			for (i = 0; i < curSamNb/2; i++) {
				sam[i] = (sam[2*i] + sam[2*i + 1])/2.;
			}
			curSamNb /= 2;
		}
		///
	}
	
	
	public static void main(String[] args) {
		double[] sin = new double[1024];
		for (int i = 0; i < sin.length; i++) {
			sin[i] = Math.sin(2 * Math.PI * 550 * i / 44100.0);
		}
		System.out.println(computeWaveletPitch(sin));
	}
}
