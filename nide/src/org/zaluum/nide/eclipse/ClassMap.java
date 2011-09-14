package org.zaluum.nide.eclipse;

import java.util.HashMap;
import java.util.Map;

/**
 * {@link Map}-like interface for mapping {@link Class} to value.
 * 
 * @author scheglov_ke
 * @coverage core.util
 */
public final class ClassMap<V> {
	// //////////////////////////////////////////////////////////////////////////
	//
	// Constructor
	//
	// //////////////////////////////////////////////////////////////////////////
	/**
	 * Creates new instance of {@link ClassMap}.
	 */
	public static <V> ClassMap<V> create() {
		return new ClassMap<V>();
	}

	// //////////////////////////////////////////////////////////////////////////
	//
	// Map
	//
	// //////////////////////////////////////////////////////////////////////////
	public void put(Class<?> key, V value) {
		getMap(key).put(key, value);
	}

	public V get(Class<?> key) {
		return getMap(key).get(key);
	}

	public void remove(Class<?> key) {
		getMap(key).remove(key);
	}

	public void clear(ClassLoader classLoader) {
		getMap(classLoader).clear();
	}

	// //////////////////////////////////////////////////////////////////////////
	//
	// Implementation
	//
	// //////////////////////////////////////////////////////////////////////////
	private Map<Class<?>, V> getMap(Class<?> key) {
		ClassLoader classLoader = key.getClassLoader();
		return getMap(classLoader);
	}

	@SuppressWarnings("unchecked")
	private Map<Class<?>, V> getMap(ClassLoader classLoader) {
		Object map = ClassLoaderLocalMap.get(classLoader, this);
		if (map == null) {
			map = new HashMap<Class<?>, V>();
			ClassLoaderLocalMap.put(classLoader, this, map);
		}
		return (Map<Class<?>, V>) map;
	}
}
