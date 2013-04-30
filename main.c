#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <omp.h>
#include <sys/time.h>

void quick_sort_seq(double *a, int n);
void quick_sort_widx_seq(double *a, int *idx, int n);
void quick_sort_omp(double *a, int n);
void quick_sort_widx_omp(double *a, int *idx, int n);
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
	/* seq, no index */
	for (i=0;i<nmax;++i){
		a[i]=b[i];
	}
	gettimeofday(&t0,NULL);
	quick_sort_seq(a,nmax);
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
	printf("seq, no-idx:   nmax=%d, use %lld ns, %lf ms...\n",nmax,tns,t_cost);
	/* seq, with index */
	for (i=0;i<nmax;++i){
		a[i]=b[i];
		idx[i]=i;
	}
	gettimeofday(&t0,NULL);
	quick_sort_widx_seq(a,idx,nmax);
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
	printf("seq, with-idx: nmax=%d, use %lld ns, %lf ms...\n",nmax,tns,t_cost);
	/* omp, no index */
	for (i=0;i<nmax;++i){
		a[i]=b[i];
	}
	gettimeofday(&t0,NULL);
	quick_sort_omp(a,nmax);
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
	printf("omp, no-idx:   nmax=%d, use %lld ns, %lf ms...\n",nmax,tns,t_cost);
	/* omp, with index */
	for (i=0;i<nmax;++i){
		a[i]=b[i];
		idx[i]=i;
	}
	gettimeofday(&t0,NULL);
	quick_sort_widx_omp(a,idx,nmax);
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
	printf("omp, with-idx: nmax=%d, use %lld ns, %lf ms...\n",nmax,tns,t_cost);
	printf("\n");
	}

	for (i=0;i<n-1;++i){
		if (a[i]>a[i+1]){
			printf("i %d %lf %lf\n",i,a[i],a[i+1]);
			exit(-1);
		}
	}
	printf("dui\n");
	for (i=0;i<n-1;++i){
		if (b[idx[i]]>b[idx[i+1]]){
			printf("i %d %d %d\n",i,idx[i],idx[i+1]);
			exit(-1);
		}
	}
	printf("dui\n");
	free(a);
	free(b);
	free(idx);
	return 0;
}
