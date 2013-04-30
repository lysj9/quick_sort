#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <omp.h>
#include <sys/time.h>

void quick_sort_seq(double *a, int n);
void quick_sort_omp(double *a, int n);
void randomz_seed(int seed);
double randomz_dbl();

int main()
{
	int n=100000000;
	double *a,*b;
	a=(double*) malloc(n*sizeof(double));
	b=(double*) malloc(n*sizeof(double));
	int i;
	randomz_seed(1);
	for (i=0;i<n;++i){
		a[i] = randomz_dbl();
		b[i] = a[i];
	}
	struct timeval t0,t1;
	long long tns=0LL;
	double t_cost;
//	clock_t tc0,tc1;
//	double tomp0,tomp1;
	int max_depth=10;
	int nmax=10;
	for (nmax=10000;nmax<=n;nmax*=10){
	for (i=0;i<n;++i) a[i]=b[i];
//#ifdef _OPENMP
//	tomp0=omp_get_wtime();
//#else
//	tc0 = clock();
//#endif
	gettimeofday(&t0,NULL);
	quick_sort_seq(a,nmax);
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
//#ifdef _OPENMP
//	tomp1=omp_get_wtime();
//	t_cost=tomp1-tomp0;
//#else
//	tc1 = clock();
//	t_cost = (double) (tc1-tc0)/CLOCKS_PER_SEC;
//#endif
	printf("seq: nmax=%d, use %lld ns, %lf ms...\n",nmax,tns,t_cost);

	for (i=0;i<n;++i) a[i]=b[i];
	gettimeofday(&t0,NULL);
	quick_sort_omp(a,nmax);
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
	printf("omp: nmax=%d, use %lld ns, %lf ms...\n",nmax,tns,t_cost);
	printf("\n");
	}

	for (i=0;i<n-1;++i){
		if (a[i]>a[i+1]){
			printf("i %d %lf %lf\n",i,a[i],a[i+1]);
			exit(-1);
		}
	}
	printf("dui\n");
	return 0;
}
