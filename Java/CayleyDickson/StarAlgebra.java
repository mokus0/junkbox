public interface StarAlgebra<This, Base> {
    public This negate();
    public This conjugate();
    public Base realPart();
    public Base magnitudeSquared();
    
    public This add(This other);
    public This sub(This other);
    public This mul(This other);
}