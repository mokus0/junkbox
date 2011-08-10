public abstract class Prelude {
	private Prelude() {}
	
	public static <A> Function<A,A> idFunction() {
		return new Function<A,A>() {
			public A apply(A a) { return a; }
		};
	}
	
	public static <A> A id(A a) {
		return a;
	}
	
	public static <A,B> Function<A,Function<B,A>> constFunction() {
		return new Function<A,Function<B,A>>() {
			public Function<B,A> apply(final A a) {
				return new Function<B,A>() {
					public A apply(B b) {
						return a;
					}
				};
			}
		};
	}
	
	public static <A,B> Function<B,A> const_(A a) {
		Function<A,Function<B,A>> f = constFunction();
		return f.apply(a);
	}
	
	// yow, I can't tell who this one confuses more - me or javac.
	// javac tells me there's a syntax error, but I can't find it.
	public static <A,B> Function<B,Function<Function<A,Function<B,A>>,Function<List<A>,B>>> foldlFunction() {
		return new Function<B,Function<Function<A,Function<B,A>>,Function<List<A>,B>>>() {
			public Function<Function<A,Function<B,A>>,Function<List<A>,B>> apply(final z: B) {
				return new Function<Function<A,Function<B,A>>,Function<List<A>,B>>() {
					public Function<List<A>,B>(final Function<A,Function<B,A>> f) {
						return new Function<List<A>,B>() {
							public B apply(List<A> xs) {
								return (B) null;
							}
						};
					}
				};
			}
		};
	}
}