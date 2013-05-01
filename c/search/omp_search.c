#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <omp.h>
#include <sys/time.h>

void quick_sort_noidx_omp(double *a, int n, int max_depth);
void quick_sort_widx_omp(double *a, int *idx, int n, int max_depth);
void randomz_seed(int seed);
double randomz_dbl();

int main()
{
	int n=10000000;
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
	int max_depth=10;
	int nmax=10;

	for (max_depth=0;max_depth<41;++max_depth){
	/* no index */
	for (i=0;i<n;++i){
		a[i]=b[i];
	}
	gettimeofday(&t0,NULL);
	quick_sort_noidx_omp(a,n,max_depth);
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
	printf("no-idx:   max_depth=%d, use %lld ns, %lf ms...\n",max_depth,tns,t_cost);
	/* with index */
	for (i=0;i<n;++i){
		a[i]=b[i];
		idx[i]=i;
	}
	gettimeofday(&t0,NULL);
	quick_sort_widx_omp(a,idx,n,max_depth);
	gettimeofday(&t1,NULL);
	tns = (t1.tv_sec-t0.tv_sec)*1000000LL + (t1.tv_usec-t0.tv_usec);
	t_cost = tns*1e-3;
	printf("with-idx: max_depth=%d, use %lld ns, %lf ms...\n",max_depth,tns,t_cost);
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
