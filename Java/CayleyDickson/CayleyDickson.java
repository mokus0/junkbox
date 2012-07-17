public final class CayleyDickson<S, T extends StarAlgebra<T, S>>
implements StarAlgebra<CayleyDickson<S,T>, S> {
    public final T re, im;
    
    public CayleyDickson(T re, T im) {
        this.re = re;
        this.im = im;
    }
    
    public static <S, T extends StarAlgebra<T,S>> CayleyDickson<S,T> Complex(T re, T im) {
        return new CayleyDickson<S,T>(re, im);
    }
    
    public static <S, T extends StarAlgebra<T,S>> CayleyDickson<S,CayleyDickson<S,T>> Quaternion(T re, T i, T j, T k) {
        return new CayleyDickson<S,CayleyDickson<S,T>>(
            Complex(re, i),
            Complex(j, k));
    }
    
    public static <S, T extends StarAlgebra<T,S>> CayleyDickson<S,CayleyDickson<S,CayleyDickson<S,T>>> Octonion(T re, T i, T j, T k, T l, T m, T n, T o) {
        return new CayleyDickson<S,CayleyDickson<S,CayleyDickson<S,T>>>(
            Quaternion(re, i, j, k),
            Quaternion(l, m, n, o));
    }
    
    @Override
    public String toString() {
        return "new CayleyDickson(" + re + ", " + im + ")";
    }
    
    public S realPart() { return re.realPart(); }
    
    public CayleyDickson<S,T> negate() {
        return new CayleyDickson<S,T>(re.negate(), im.negate());
    }
    
    public CayleyDickson<S,T> conjugate() {
        return new CayleyDickson<S,T>(re.conjugate(), im.negate());
    }
    
    public CayleyDickson<S,T> add(CayleyDickson<S,T> other) {
        return new CayleyDickson<S,T>(re.add(other.re), im.add(other.im));
    }
    
    public CayleyDickson<S,T> sub(CayleyDickson<S,T> other) {
        return new CayleyDickson<S,T>(re.sub(other.re), im.sub(other.im));
    }
    
    public CayleyDickson<S,T> mul(CayleyDickson<S,T> other) {
        // (a*c - conj d * b)
        // (d*a + b * conj c)
        return new CayleyDickson<S,T>(
            re.mul(other.re).sub(other.im.conjugate().mul(im)),
            other.im.mul(re).add(im.mul(other.re.conjugate())));
    }
    
    public S magnitudeSquared() {
        // a * conj a + conj b * b
        return re.mul(re.conjugate()).add(im.conjugate().mul(im)).realPart();
    }
}
