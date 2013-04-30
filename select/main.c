#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <omp.h>
#include <sys/time.h>

void quick_sort_omp(double *a, int n);
void quick_sort_widx_omp(double *a, int *idx, int n);
double quick_select(double *a, int k, int n);
double quick_select_widx(double *a, int *idx, int k, int n, int *ik);
void randomz_seed(int seed);
double randomz_dbl();

int main()
{
	int n=100000000;
	double *a,*b;
	int *idx;
	a=(double*) malloc(n*sizeof(double));
	b=(double*) malloc(n*sizeof(double));
	idx=(int*) malloc(n*sizeof(int));
	int i;
	int k;
	double ak;
	int ik;
	randomz_seed(1);
	for (i=0;i<n;++i){
		b[i] = randomz_dbl();
	}
	struct timeval t0,t1;
	long long tns=0LL;
	double t_cost;
//	clock_t tc0,tc1;
//	double tomp0,tomp1;

//#ifdef _OPENMP
//	tomp0=omp_get_wtime();
//#else
//	tc0 = clock();
//#endif

//#ifdef _OPENMP
//	tomp1=omp_get_wtime();
//	t_cost=tomp1-tomp0;
//#else
//	tc1 = clock();
//	t_cost = (double) (tc1-tc0)/CLOCKS_PER_SEC;
//#endif
	int nmax=10;
	for (nmax=10000;nmax<=n;nmax*=10){
	k=nmax*0.5;
	/* omp, no index */
	for (i=0;i<nmax;++i){
		a[i]=b[i];
	}
	gettimeofday(&t0,NULL);
	quick_sort_omp(a,nmax);
	ak=a[k];
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
	printf("k=%d, ak=%12.8f\n",k,ak);
	printf("omp, no-idx:   nmax=%d, use %lld ns, %lf ms...\n",nmax,tns,t_cost);
	/* omp, with index */
	for (i=0;i<nmax;++i){
		a[i]=b[i];
		idx[i]=i;
	}
	gettimeofday(&t0,NULL);
	quick_sort_widx_omp(a,idx,nmax);
	ak=a[k];
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
	printf("k=%d, ak=%12.8f, ik=%d\n",k,ak,idx[k]);
	printf("omp, with-idx: nmax=%d, use %lld ns, %lf ms...\n",nmax,tns,t_cost);
	/* quick select, no index */
	for (i=0;i<nmax;++i){
		a[i]=b[i];
	}
	gettimeofday(&t0,NULL);
	ak=quick_select(a,k,nmax);
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
	printf("k=%d, ak=%12.8f\n",k,ak);
	printf("q-s, no-idx:   nmax=%d, use %lld ns, %lf ms...\n",nmax,tns,t_cost);
//	printf("a[k]=%12.8f\n",a[k]);
	/* quick select, with index */
	for (i=0;i<nmax;++i){
		a[i]=b[i];
		idx[i]=i;
	}
	gettimeofday(&t0,NULL);
	ak=quick_select_widx(a,idx,k,nmax,&ik);
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
	printf("k=%d, ak=%12.8f, ik=%d\n",k,ak,ik);
	printf("q-s, no-idx:   nmax=%d, use %lld ns, %lf ms...\n",nmax,tns,t_cost);
//	printf("a[k]=%12.8f\n",a[k]);
	printf("\n");
	}

	free(a);
	free(b);
	free(idx);
	return 0;
}
