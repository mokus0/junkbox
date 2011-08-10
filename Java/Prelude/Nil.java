public class Nil<T> extends List<T> {
	public <X> X match(X nil, Function<T,Function<List<T>,X>> cons) {
		return nil;
	}
}