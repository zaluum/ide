package org.zaluum.nide.eclipse.integration.model;

import org.eclipse.jdt.internal.compiler.lookup.InvocationSite;
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding;

@SuppressWarnings("restriction")
public class FakeInvocationSite implements InvocationSite {
	public FakeInvocationSite(TypeBinding expectedType) {
	}

	public TypeBinding[] genericTypeArguments() {
		return null;
	}

	public boolean isSuperAccess() {
		return false;
	}

	public boolean isTypeAccess() {
		return false;
	}

	public void setActualReceiverType(ReferenceBinding receiverType) {
	}

	public void setDepth(int depth) {
	}

	public void setFieldIndex(int depth) {
	}

	public int sourceEnd() {
		return 0;
	}

	public int sourceStart() {
		return 0;
	}

	public TypeBinding expectedType() {
		return null;
	}
}
