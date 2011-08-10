public class Cons<T> extends List<T> {
	T head;
	List<T> tail;
	
	public Cons(T hd, List<T> tl) {
		head = hd;
		tail = tl;
	}
	
	public <X> X match(X nil, Function<T,Function<List<T>,X>> cons) {
		return cons.apply(head).apply(tail);
	}
}