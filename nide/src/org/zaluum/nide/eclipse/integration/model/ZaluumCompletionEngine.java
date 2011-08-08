package org.zaluum.nide.eclipse.integration.model;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.internal.compiler.lookup.Binding;
import org.eclipse.jdt.internal.compiler.lookup.ClassScope;
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding;
import org.eclipse.jdt.internal.compiler.lookup.InvocationSite;
import org.eclipse.jdt.internal.compiler.lookup.LocalVariableBinding;
import org.eclipse.jdt.internal.compiler.lookup.LookupEnvironment;
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding;
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding;
import org.eclipse.jdt.internal.compiler.lookup.Scope;
import org.eclipse.jdt.internal.compiler.lookup.SourceTypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding;
import org.eclipse.jdt.internal.compiler.util.ObjectVector;

@SuppressWarnings("restriction")
public class ZaluumCompletionEngine {
	LookupEnvironment lookupEnvironment;

	public ZaluumCompletionEngine(LookupEnvironment lookupEnvironment) {
		this.lookupEnvironment = lookupEnvironment;
	}

	public List<MethodBinding> findAllConstructors(ReferenceBinding currentType,
			Scope scope) {
		FakeInvocationSite invocationSite = new FakeInvocationSite(null);
		// No visibility checks can be performed without the scope &
		// invocationSite
		List<MethodBinding> found = new ArrayList<MethodBinding>();
		MethodBinding[] methods = currentType.availableMethods();
		if (methods != null) {
			next: for (int f = methods.length; --f >= 0;) {
				MethodBinding constructor = methods[f];
				if (constructor.isConstructor()) {

					if (constructor.isSynthetic())
						continue next;

					if (constructor.isViewedAsDeprecated()
							&& !scope
									.isDefinedInSameUnit(constructor.declaringClass))
						continue next;

					if (!constructor.canBeSeenBy(invocationSite, scope)) {
						if (!constructor.isProtected())
							continue next;
					}
					found.add(constructor);
				}
			}
		}
		return found;
	}

	public ObjectVector findAllMethods(ReferenceBinding receiverType,
			ClassScope scope, boolean statics) {
		ObjectVector methodsFound = new ObjectVector();
		findMethods(new char[0], // selector
				null, // typeArgTypes
				null, // argTypes
				receiverType, // receiverType
				scope, // scope
				methodsFound, // methodsFound
				statics, // onlystatic
				!statics, // noStaticMethods
				false, // exactmatch
				new FakeInvocationSite(null), // invocationSite
				scope, // invocationScope
				false, // implicitCall
				false, // supercall
				false, // canbePrefixed
				null, // missingelements
				null, // missingElementsStarts
				null, // missingElementsEnds
				false, // missingElementsHaveProblems
				null, // castedReceiver
				-1, // receiverStart
				-1 // receiverEnd
		);
		return methodsFound;
	}

	public ObjectVector findAllFields(ReferenceBinding receiverType,
			ClassScope scope, boolean statics) {
		ObjectVector fieldsFound = new ObjectVector();
		ObjectVector localsFound = new ObjectVector();

		findFields(new char[0], // selector
				receiverType, // receiverType
				scope, // scope
				fieldsFound, // fieldsFound
				localsFound, // localsFound
				statics, // onlystatic
				new FakeInvocationSite(null), // invocationSite
				scope, // invocationScope
				false, // implicitCall
				false, // canbePrefixed
				null, // missingelements
				null, // missingElementsStarts
				null, // missingElementsEnds
				false, // missingElementsHaveProblems
				null, // castedReceiver
				-1, // receiverStart
				-1 // receiverEnd
		);
		return fieldsFound;
	}

	public void findMethods(char[] selector, TypeBinding[] typeArgTypes,
			TypeBinding[] argTypes, ReferenceBinding receiverType, Scope scope,
			ObjectVector methodsFound, boolean onlyStaticMethods,
			boolean noStaticMethods, boolean exactMatch,
			InvocationSite invocationSite, Scope invocationScope,
			boolean implicitCall, boolean superCall, boolean canBePrefixed,
			Binding[] missingElements, int[] missingElementsStarts,
			int[] missingElementsEnds, boolean missingElementsHaveProblems,
			char[] castedReceiver, int receiverStart, int receiverEnd) {

		ReferenceBinding currentType = receiverType;
		if (receiverType.isInterface()) {
			findInterfacesMethods(selector, typeArgTypes, argTypes,
					receiverType, new ReferenceBinding[] { currentType },
					scope, methodsFound, onlyStaticMethods, noStaticMethods,
					exactMatch, invocationSite, invocationScope, implicitCall,
					superCall, canBePrefixed, missingElements,
					missingElementsStarts, missingElementsEnds,
					missingElementsHaveProblems, castedReceiver, receiverStart,
					receiverEnd);

			currentType = scope.getJavaLangObject();
		}
		boolean hasPotentialDefaultAbstractMethods = true;
		while (currentType != null) {

			MethodBinding[] methods = currentType.availableMethods();
			if (methods != null) {
				findLocalMethods(selector, typeArgTypes, argTypes, methods,
						scope, methodsFound, onlyStaticMethods,
						noStaticMethods, exactMatch, receiverType,
						invocationSite, invocationScope, implicitCall,
						superCall, canBePrefixed, missingElements,
						missingElementsStarts, missingElementsEnds,
						missingElementsHaveProblems, castedReceiver,
						receiverStart, receiverEnd);
			}

			if (hasPotentialDefaultAbstractMethods
					&& (currentType.isAbstract()
							|| currentType.isTypeVariable()
							|| currentType.isIntersectionType() || currentType
							.isEnum())) {

				ReferenceBinding[] superInterfaces = currentType
						.superInterfaces();
				if (superInterfaces != null && currentType.isIntersectionType()) {
					for (int i = 0; i < superInterfaces.length; i++) {
						superInterfaces[i] = (ReferenceBinding) superInterfaces[i]
								.capture(invocationScope,
										invocationSite.sourceEnd());
					}
				}

				findInterfacesMethods(selector, typeArgTypes, argTypes,
						receiverType, superInterfaces, scope, methodsFound,
						onlyStaticMethods, noStaticMethods, exactMatch,
						invocationSite, invocationScope, implicitCall,
						superCall, canBePrefixed, missingElements,
						missingElementsStarts, missingElementsEnds,
						missingElementsHaveProblems, castedReceiver,
						receiverStart, receiverEnd);
			} else {
				hasPotentialDefaultAbstractMethods = false;
			}
			currentType = currentType.superclass();
		}
	}

	private void findInterfacesMethods(char[] selector,
			TypeBinding[] typeArgTypes, TypeBinding[] argTypes,
			ReferenceBinding receiverType, ReferenceBinding[] itsInterfaces,
			Scope scope, ObjectVector methodsFound, boolean onlyStaticMethods,
			boolean noStaticMethods, boolean exactMatch,
			InvocationSite invocationSite, Scope invocationScope,
			boolean implicitCall, boolean superCall, boolean canBePrefixed,
			Binding[] missingElements, int[] missingElementssStarts,
			int[] missingElementsEnds, boolean missingElementsHaveProblems,
			char[] castedReceiver, int receiverStart, int receiverEnd) {

		if (selector == null)
			return;

		if (itsInterfaces != Binding.NO_SUPERINTERFACES) {
			ReferenceBinding[] interfacesToVisit = itsInterfaces;
			int nextPosition = interfacesToVisit.length;

			for (int i = 0; i < nextPosition; i++) {
				ReferenceBinding currentType = interfacesToVisit[i];
				MethodBinding[] methods = currentType.availableMethods();
				if (methods != null) {
					findLocalMethods(selector, typeArgTypes, argTypes, methods,
							scope, methodsFound, onlyStaticMethods,
							noStaticMethods, exactMatch, receiverType,
							invocationSite, invocationScope, implicitCall,
							superCall, canBePrefixed, missingElements,
							missingElementssStarts, missingElementsEnds,
							missingElementsHaveProblems, castedReceiver,
							receiverStart, receiverEnd);
				}

				itsInterfaces = currentType.superInterfaces();
				if (itsInterfaces != null
						&& itsInterfaces != Binding.NO_SUPERINTERFACES) {
					int itsLength = itsInterfaces.length;
					if (nextPosition + itsLength >= interfacesToVisit.length)
						System.arraycopy(
								interfacesToVisit,
								0,
								interfacesToVisit = new ReferenceBinding[nextPosition
										+ itsLength + 5], 0, nextPosition);
					nextInterface: for (int a = 0; a < itsLength; a++) {
						ReferenceBinding next = itsInterfaces[a];
						for (int b = 0; b < nextPosition; b++)
							if (next == interfacesToVisit[b])
								continue nextInterface;
						interfacesToVisit[nextPosition++] = next;
					}
				}
			}
		}
	}

	public void findLocalMethods(char[] methodName, TypeBinding[] typeArgTypes,
			TypeBinding[] argTypes, MethodBinding[] methods, Scope scope,
			ObjectVector methodsFound, boolean onlyStaticMethods,
			boolean noStaticMethods, boolean exactMatch,
			ReferenceBinding receiverType, InvocationSite invocationSite,
			Scope invocationScope, boolean implicitCall, boolean superCall,
			boolean canBePrefixed, Binding[] missingElements,
			int[] missingElementsStarts, int[] missingElementsEnds,
			boolean missingElementsHaveProblems, char[] castedReceiver,
			int receiverStart, int receiverEnd) {

		ObjectVector newMethodsFound = new ObjectVector();
		// Inherited methods which are hidden by subclasses are filtered out
		// No visibility checks can be performed without the scope &
		// invocationSite

		int methodLength = methodName.length;
		int minTypeArgLength = typeArgTypes == null ? 0 : typeArgTypes.length;
		int minArgLength = argTypes == null ? 0 : argTypes.length;

		next: for (int f = methods.length; --f >= 0;) {
			MethodBinding method = methods[f];

			if (method.isSynthetic())
				continue next;

			if (method.isDefaultAbstract())
				continue next;

			if (method.isConstructor())
				continue next;

			/*
			 * if ( this.options.checkDeprecation &&
			 * method.isViewedAsDeprecated() &&
			 * !scope.isDefinedInSameUnit(method.declaringClass)) continue next;
			 */
			// TODO (david) perhaps the relevance of a void method must be
			// lesser than other methods
			// if (expectedTypesPtr > -1 && method.returnType ==
			// BaseTypes.VoidBinding) continue next;
			if (noStaticMethods && method.isStatic())
				continue next;

			if (onlyStaticMethods && !method.isStatic())
				continue next;

			if (/* this.options.checkVisibility */true && !method.canBeSeenBy(
					receiverType, invocationSite, scope))
				continue next;

			if (superCall && method.isAbstract()) {
				methodsFound.add(new Object[] { method, receiverType });
				continue next;
			}

			if (exactMatch) {
				if (!CharOperation
						.equals(methodName, method.selector, false /*
																	 * ignore
																	 * case
																	 */)) {
					continue next;
				}
			} else {
				if (methodLength > method.selector.length)
					continue next;
				/*
				 * if (!CharOperation.prefixEquals(methodName, method.selector,
				 * false) //ignore case ) && !( this.options.camelCaseMatch &&
				 * CharOperation .camelCaseMatch(methodName, method.selector)))
				 * { continue next; }
				 */
			}

			if (minTypeArgLength != 0
					&& minTypeArgLength != method.typeVariables.length)
				continue next;

			if (minTypeArgLength != 0) {
				method = scope.environment().createParameterizedGenericMethod(
						method, typeArgTypes);
			}

			if (minArgLength > method.parameters.length)
				continue next;

			for (int a = minArgLength; --a >= 0;) {
				if (argTypes[a] != null) { // can be null if it could not be
											// resolved properly
					if (!argTypes[a].isCompatibleWith(method.parameters[a])) {
						continue next;
					}
				}
			}

			for (int i = methodsFound.size; --i >= 0;) {
				Object[] other = (Object[]) methodsFound.elementAt(i);
				MethodBinding otherMethod = (MethodBinding) other[0];
				ReferenceBinding otherReceiverType = (ReferenceBinding) other[1];
				if (method == otherMethod && receiverType == otherReceiverType)
					continue next;

				if (CharOperation.equals(method.selector, otherMethod.selector,
						true)) {
					if (receiverType == otherReceiverType) {
						if (this.lookupEnvironment.methodVerifier()
								.isMethodSubsignature(otherMethod, method)) {
							if (!superCall
									|| !otherMethod.declaringClass
											.isInterface()) {
								continue next;
							}
						}
					} else {
						if (this.lookupEnvironment.methodVerifier()
								.isMethodSubsignature(otherMethod, method)) {
							if (receiverType.isAnonymousType())
								continue next;

							if (!superCall) {
								if (!canBePrefixed)
									continue next;

							}
						}
					}
				}
			}

			newMethodsFound.add(new Object[] { method, receiverType });

			ReferenceBinding superTypeWithSameErasure = (ReferenceBinding) receiverType
					.findSuperTypeOriginatingFrom(method.declaringClass);
			if (method.declaringClass != superTypeWithSameErasure) {
				MethodBinding[] otherMethods = superTypeWithSameErasure
						.getMethods(method.selector);
				for (int i = 0; i < otherMethods.length; i++) {
					if (otherMethods[i].original() == method.original()) {
						method = otherMethods[i];
					}
				}
			}

		}

		methodsFound.addAll(newMethodsFound);
	}

	/****************************************
	 * 
	 * 
	 */

	// Helper method for findFields(char[], ReferenceBinding, Scope,
	// ObjectVector, boolean)
	private void findFields(char[] fieldName, FieldBinding[] fields,
			Scope scope, ObjectVector fieldsFound, ObjectVector localsFound,
			boolean onlyStaticFields, ReferenceBinding receiverType,
			InvocationSite invocationSite, Scope invocationScope,
			boolean implicitCall, boolean canBePrefixed,
			Binding[] missingElements, int[] missingElementsStarts,
			int[] missingElementsEnds, boolean missingElementsHaveProblems,
			char[] castedReceiver, int receiverStart, int receiverEnd) {

		ObjectVector newFieldsFound = new ObjectVector();
		// Inherited fields which are hidden by subclasses are filtered out
		// No visibility checks can be performed without the scope &
		// invocationSite

		int fieldLength = fieldName.length;
		next: for (int f = fields.length; --f >= 0;) {
			FieldBinding field = fields[f];

			if (field.isSynthetic())
				continue next;

			if (onlyStaticFields && !field.isStatic())
				continue next;

			if (fieldLength > field.name.length)
				continue next;

			if (!CharOperation.prefixEquals(fieldName, field.name, false /*
																		 * ignore
																		 * case
																		 */))
				continue next;

			if (true && !field.canBeSeenBy(receiverType, invocationSite, scope))
				continue next;

			for (int i = fieldsFound.size; --i >= 0;) {
				Object[] other = (Object[]) fieldsFound.elementAt(i);
				FieldBinding otherField = (FieldBinding) other[0];
				ReferenceBinding otherReceiverType = (ReferenceBinding) other[1];
				if (field == otherField && receiverType == otherReceiverType)
					continue next;
				if (CharOperation.equals(field.name, otherField.name, true)) {
					if (field.declaringClass
							.isSuperclassOf(otherField.declaringClass))
						continue next;
					if (otherField.declaringClass.isInterface()) {
						if (field.declaringClass == scope.getJavaLangObject())
							continue next;
						if (field.declaringClass.implementsInterface(
								otherField.declaringClass, true))
							continue next;
					}
					if (field.declaringClass.isInterface())
						if (otherField.declaringClass.implementsInterface(
								field.declaringClass, true))
							continue next;
					if (!canBePrefixed)
						continue next;
				}
			}

			for (int l = localsFound.size; --l >= 0;) {
				LocalVariableBinding local = (LocalVariableBinding) localsFound
						.elementAt(l);

				if (CharOperation.equals(field.name, local.name, true)) {
					SourceTypeBinding declarationType = scope
							.enclosingSourceType();
					if (declarationType.isAnonymousType()
							&& declarationType != invocationScope
									.enclosingSourceType()) {
						continue next;
					}
					if (!canBePrefixed)
						continue next;
					break;
				}
			}

			newFieldsFound.add(new Object[] { field, receiverType });

			char[] completion = field.name;

			if (castedReceiver != null) {
				completion = CharOperation.concat(castedReceiver, completion);
			}

		}
		fieldsFound.addAll(newFieldsFound);
	}

	public void findFields(char[] fieldName, ReferenceBinding receiverType,
			Scope scope, ObjectVector fieldsFound, ObjectVector localsFound,
			boolean onlyStaticFields, InvocationSite invocationSite,
			Scope invocationScope, boolean implicitCall, boolean canBePrefixed,
			Binding[] missingElements, int[] missingElementsStarts,
			int[] missingElementsEnds, boolean missingElementsHaveProblems,
			char[] castedReceiver, int receiverStart, int receiverEnd) {

		if (fieldName == null)
			return;

		ReferenceBinding currentType = receiverType;
		ReferenceBinding[] interfacesToVisit = null;
		int nextPosition = 0;
		do {
			ReferenceBinding[] itsInterfaces = currentType.superInterfaces();
			if (itsInterfaces != Binding.NO_SUPERINTERFACES) {
				if (interfacesToVisit == null) {
					interfacesToVisit = itsInterfaces;
					nextPosition = interfacesToVisit.length;
				} else {
					int itsLength = itsInterfaces.length;
					if (nextPosition + itsLength >= interfacesToVisit.length)
						System.arraycopy(
								interfacesToVisit,
								0,
								interfacesToVisit = new ReferenceBinding[nextPosition
										+ itsLength + 5], 0, nextPosition);
					nextInterface: for (int a = 0; a < itsLength; a++) {
						ReferenceBinding next = itsInterfaces[a];
						for (int b = 0; b < nextPosition; b++)
							if (next == interfacesToVisit[b])
								continue nextInterface;
						interfacesToVisit[nextPosition++] = next;
					}
				}
			}

			FieldBinding[] fields = currentType.availableFields();
			if (fields != null && fields.length > 0) {
				findFields(fieldName, fields, scope, fieldsFound, localsFound,
						onlyStaticFields, receiverType, invocationSite,
						invocationScope, implicitCall, canBePrefixed,
						missingElements, missingElementsStarts,
						missingElementsEnds, missingElementsHaveProblems,
						castedReceiver, receiverStart, receiverEnd);
			}
			currentType = currentType.superclass();
		} while (currentType != null);

		if (interfacesToVisit != null) {
			for (int i = 0; i < nextPosition; i++) {
				ReferenceBinding anInterface = interfacesToVisit[i];
				FieldBinding[] fields = anInterface.availableFields();
				if (fields != null) {
					findFields(fieldName, fields, scope, fieldsFound,
							localsFound, onlyStaticFields, receiverType,
							invocationSite, invocationScope, implicitCall,
							canBePrefixed, missingElements,
							missingElementsStarts, missingElementsEnds,
							missingElementsHaveProblems, castedReceiver,
							receiverStart, receiverEnd);
				}

				ReferenceBinding[] itsInterfaces = anInterface
						.superInterfaces();
				if (itsInterfaces != Binding.NO_SUPERINTERFACES) {
					int itsLength = itsInterfaces.length;
					if (nextPosition + itsLength >= interfacesToVisit.length)
						System.arraycopy(
								interfacesToVisit,
								0,
								interfacesToVisit = new ReferenceBinding[nextPosition
										+ itsLength + 5], 0, nextPosition);
					nextInterface: for (int a = 0; a < itsLength; a++) {
						ReferenceBinding next = itsInterfaces[a];
						for (int b = 0; b < nextPosition; b++)
							if (next == interfacesToVisit[b])
								continue nextInterface;
						interfacesToVisit[nextPosition++] = next;
					}
				}
			}
		}
	}
}
