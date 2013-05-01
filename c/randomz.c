#include <math.h>
#include <time.h>

#define A 16807
#define M 2147483647
#define Q 127773
#define R 2836

//#define EPS_P  1.2e-7 	// nearest float number greater than 1 is 1+1.2e-7
//#define DEPS_P 2.2e-16	// nearest double number greater than 1 is 1+2e-16
#define EPS_N  6e-8	  	// nearest float number less than 1 is 1-6e-8
#define DEPS_N 1.1e-16	// nearest double number less than 1 is 1-1.1e-16
#define RNMAX (1.0-EPS_N)
#define DRNMAX (1.0-DEPS_N)

//#define AM (1.0/M)
#define AM RNMAX*(1.0/M)
#define DAM DRNMAX*(1.0/M)

//double am=1.0/M;
//int z = (int) time(NULL);
static int z=1;
static int ix=245371421, iy=16807;

void randomz_seed( int seed )
{
	int k;
	if ( seed == 0 ){
		z = (int) time(NULL);
	}else{
		z = seed;
	}

	iy = z;
	k  = iy/Q;
	iy = A*(iy-k*Q) - R*k;
	if (iy<0) iy += M;

	ix  = iy;
	ix ^= (ix<<13);
	ix ^= (ix>>17);
	ix ^= (ix<<5);
}

float randomz()
{
//	float temp;
	int k;
	ix ^= (ix<<13);
	ix ^= (ix>>17);
	ix ^= (ix<<5);
	k = iy/Q;
	iy = A*(iy-k*Q) - R*k;
	if ( iy<0 ) iy += M;
//	temp = AM * (1|(M&(ix^iy)));
//	if ( temp>RNMAX ) return RNMAX;
//	else return temp;
	return AM * (1|(M&(ix^iy)));
}

double randomz_dbl()
{
//	double temp;
	int k;
	ix ^= (ix<<13);
	ix ^= (ix>>17);
	ix ^= (ix<<5);
	k = iy/Q;
	iy = A*(iy-k*Q) - R*k;
	if ( iy<0 ) iy += M;
//	temp = DAM * (1|(M&(ix^iy)));
//	if ( temp>DRNMAX ) return DRNMAX;
//	else return temp;
	return DAM * (1|(M&(ix^iy)));
}

/*
#define BIG 10
double gaussrand1( double mean,double sigma )
{
	double x;
	while (1){
		x = randomz(-BIG,BIG);
		if ( randomz(0,1) < exp(-0.5*x*x) )
			return mean+sigma*x;
	}
}
*/

#define PI 3.14159265358979323846
#define TWO_PI PI*2
float normal_rand()
{
	float x,y,z;
	static float a[2];
	static int g=1;

	g++;
	if (g>1) {
		x = randomz();
		y = randomz()*TWO_PI;
//		z = sqrt( -2*log(x) ) * cos(y);
		z = sqrt( -2*log(x) );
		a[0] = z*cos(y);
		a[1] = z*sin(y);
		g=0;
	}
	return a[g];
}

float gaussrand( float mean, float sigma )
{
	float x,y,z;
	static float a[2];
	static int g=1;

	g++;
	if (g>1) {
		x = randomz();
		y = randomz()*TWO_PI;
//		z = sqrt( -2*log(x) ) * cos(y);
		z = sqrt( -2*log(x) );
		a[0] = z*cos(y);
		a[1] = z*sin(y);
		g=0;
	}
	return mean + sigma*a[g];
}

double normal_rand_dbl()
{
	double x,y,z;
	static double a[2];
	static int g=1;

	g++;
	if (g>1) {
		x = randomz_dbl();
		y = randomz_dbl()*TWO_PI;
//		z = sqrt( -2*log(x) ) * cos(y);
		z = sqrt( -2*log(x) );
		a[0] = z*cos(y);
		a[1] = z*sin(y);
		g=0;
	}
	return a[g];
}

double gaussrand_dbl( double mean, double sigma )
{
	double x,y,z;
	static double a[2];
	static int g=1;

	g++;
	if (g>1) {
		x = randomz_dbl();
		y = randomz_dbl()*TWO_PI;
//		z = sqrt( -2*log(x) ) * cos(y);
		z = sqrt( -2*log(x) );
		a[0] = z*cos(y);
		a[1] = z*sin(y);
		g=0;
	}
	return mean + sigma*a[g];
}
