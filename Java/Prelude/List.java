import java.util.Iterator;

abstract class List<T> implements Iterable<T> {
	List() {}
	
	public abstract <X> X match(X nil, Function<T,Function<List<T>,X>> cons);
	
	public Iterator<T> iterator() {
		return new Iterator<T>() {
			public boolean hasNext() {
				Function<Boolean,Function<List<T>,Boolean>> const1 = Prelude.constFunction();
				Function<Function<List<T>,Boolean>,Function<T,Function<List<T>,Boolean>>> const2 = Prelude.constFunction();
				
				return match(new Boolean(false), const2.apply(const1.apply(new Boolean(true))));
			}
			
			public T next() {
				Function<T,Function<List<T>,T>> cnst = Prelude.constFunction();
				
				return match(null, cnst);
			}
			
			public void remove() {
				throw new UnsupportedOperationException("Wow, this is stupid");
			}
		};
	}
}