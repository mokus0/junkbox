public class Left<A,B> extends Either<A,B> {
	public final A value;
	
	public Left(A value) {
		this.value = value;
	}
	
	public <T> T match(Function<A,T> f, Function<B,T> g) {
		return f.apply(value);
	}
}