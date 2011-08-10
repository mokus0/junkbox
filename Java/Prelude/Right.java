public class Right<A,B> extends Either<A,B> {
	public final B value;
	
	public Right(B value) {
		this.value = value;
	}
	
	public <T> T match(Function<A,T> f, Function<B,T> g) {
		return g.apply(value);
	}
}