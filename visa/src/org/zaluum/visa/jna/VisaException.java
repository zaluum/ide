package org.zaluum.visa.jna;
public class VisaException extends Exception {
	private static final long serialVersionUID = 1L;

	public VisaException(long errId) {
		super(Long.toHexString(errId));
	}
}
