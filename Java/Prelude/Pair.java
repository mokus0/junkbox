class Pair<A,B> {
	final A fst;
	final B snd;
	
	public Pair(A a, B b) {
		fst = a;
		snd = b;
	}
	
	public <T> T match(Function<A,Function<B,T>> f) {
		return f.apply(fst).apply(snd);
	}
}