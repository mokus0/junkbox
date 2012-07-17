public class Dbl
implements StarAlgebra<Dbl, Double> {
    public final double value;
    
    public Dbl(double value) {
        this.value = value;
    }
    
    @Override
    public String toString() {
        return "new Dbl(" + value + ")";
    }
    
    public Dbl negate() { return new Dbl(-value); }
    public Dbl conjugate() { return this; }
    public Double realPart() { return value; }
    public Double magnitudeSquared() { return value * value; }
    
    public Dbl add(Dbl other) { return new Dbl(value + other.value); }
    public Dbl sub(Dbl other) { return new Dbl(value - other.value); }
    public Dbl mul(Dbl other) { return new Dbl(value * other.value); }
}