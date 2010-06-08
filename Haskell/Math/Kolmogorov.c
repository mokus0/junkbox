#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// C := A * B
void mMultiply(double *A,double *B,double *C,int m) { 
	int i,j,k; double s;
	for(i=0;i<m;i++) 
		for(j=0; j<m; j++) {
			s=0.; 
			for(k=0;k<m;k++) 
				s+=A[i*m+k]*B[k*m+j]; 
			C[i*m+j]=s;
		}
}

// pow(10, *eV) * V := (pow(10, eA) * A) ^ n
// where dim(A) = (m,m)
void mPower(double *A,int eA,double *V,int *eV,int m,int n) { 
	double *B;
	int eB,i;
	
	// Base case (n = 1): copy A to V and eA to eV
	if(n==1) {
		for(i=0;i<m*m;i++) 
			V[i]=A[i];
		
		*eV=eA; 
		return;
	} 
	
	// Recursive step: 
	//    pow(10, *eV) * V := (pow(10,eA) * A) ^ floor(n/2))
	mPower(A,eA,V,eV,m,n/2); 
	B=(double*)malloc((m*m)*sizeof(double)); 
	
	//    pow(10,  eB) * B := (pow(10, *eV) * V) ^ 2
	mMultiply(V,V,B,m); 
	eB=2*(*eV); 
	
	if(n%2==0) {
		// if original N was even, finish by copying B to V and eB to eV
		for(i=0;i<m*m;i++) 
			V[i]=B[i]; 
		
		*eV=eB;
	} else {
		// if original N was odd, finish by multiplying:
		//    pow(10, *eV) * V := (pow(10,  eA) * A) * (pow(10,  eB) * B)
		mMultiply(A,B,V,m);	
		*eV = eA + eB;
	} 
	
	// Finally, if center element of V is too big, move some of its exponent into *eV
	if (V[(m/2)*m+(m/2)] > 1e140) {
		for (i=0; i<m*m; i++) 
			V[i] = V[i] * 1e-140;
		*eV += 140;
	} 
	
	free(B); 
}

double K(int n,double d) { 
	int i,j,g,eH,eQ;
 	
	// Marsaglia's shortcut for p >~ 0.999
	double s = d*d*n; 
	if(s>7.24||(s>3.76&&n>99)) 
		return 1-2*exp(-(2.000071+.331/sqrt(n)+1.409/n)*s);
	
	
	int k=(int)(n*d)+1; 
	int m=2*k-1; 
	double h=k-n*d; 
	
	double *H = (double *)malloc((m * m) * sizeof(double)); 
	double *Q = (double *)malloc((m * m) * sizeof(double)); 
	
	// Fill H with 0s where there will be 0s and 1s everywhere else
	for(i=0;i<m;i++) for(j=0;j<m;j++)
		if(i-j+1<0) H[i*m+j]=0; 
		else H[i*m+j]=1; 
	
	// Fill edge parts of H
	for(i=0;i<m;i++) {
		H[i*m]-=pow(h,i+1); 
		H[(m-1)*m+i]-=pow(h,(m-i));
	} 
	
	// Set bottom left corner
	H[(m-1)*m]+=(2*h-1>0?pow(2*h-1,m):0); 
	
	// Fill lower triangle of 1s with 1/(i-j+1)!
	for(i=0;i<m;i++) for(j=0;j<m;j++)
		if(i-j+1>0) for(g=1;g<=i-j+1;g++) H[i*m+j]/=g; 
	
	// Compute pow(10,eQ)*Q := H^n
	eH=0; 
	mPower(H,eH,Q,&eQ,m,n); 
	
	// Compute scale factor (n!/n^n), avoiding underflow by modifying eQ
	// Computed as product[i/n | i <- [1..n]]
	s=Q[(k-1)*m+k-1]; 
	for(i=1;i<=n;i++) {
		s=s*i/n; 
		if(s < 1e-140) {
			s*=1e140; eQ-=140;
		}
	} 
	
	s*=pow(10.,eQ); 
	
	free(H); 
	free(Q); 
	
	return s;
}

int usage() {
	printf("Usage: k n d\n");
	printf("   where\n");
	printf("        n is the number of trials\n");
	printf("        d is the d-statistic\n");
	
	return -1;
}

int main(int argc, char **argv) {
	if (argc != 3) {
		return usage();
	} else {
		int n = atoi(argv[1]);
		double d = atof(argv[2]);
		printf("%.16g\n", K(n,d));
	}
}
