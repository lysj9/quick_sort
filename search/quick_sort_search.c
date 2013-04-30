#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>

//#define NN 10
//#define MAX_DEPTH 11
int NN=15;
int MAX_DEPTH=11;

#ifdef _SHOW_STACK_LEVEL_
int level=0;
#endif

#define SWAP(a,b,type) \
({\
	type temp;\
	temp=a;\
	a=b;\
	b=temp;\
})

//#define SWAP(a,b,type) do\
{\
	type temp;\
	temp=a;\
	a=b;\
	b=temp;\
} while(0)

/*
 * 采用递归方式，数组大小小于NN时采用冒泡排序
 */
quick_sort_recursive(double *a, int l, int r)
{
	int i,j;
	int k;
	double a0,temp;
	if (r-l<NN){
		for (j=l+1;j<=r;++j){
			a0 = a[j];
			for (i=j-1;i>=l;--i){
				if (a[i]<a0) break;
				a[i+1] = a[i];
			}
			a[i+1] = a0;
		}
		return;
	} else {
		i=(l+r)/2;
		SWAP(a[i],a[l+1],double);
		/*
		i=l;
		j=r+1;
		a0=a[l];
		while (1){
			while (a[++i]<a0);
			while (a[--j]>a0);
			if (i>=j) break;
			SWAP(a[i],a[j],double);
		}
		a[l]=a[j];
		a[j]=a0;
		*/
		/* 
		 * numerical recipe 方法
		 */
		if (a[l]>a[r]){
			SWAP(a[l],a[r],double);
		}
		if (a[l+1]>a[r]){
			SWAP(a[l+1],a[r],double);
		}
		if (a[l]>a[l+1]){
			SWAP(a[l],a[l+1],double);
		}
		i=l+1;
		j=r;
		a0=a[l+1];
		while (1){
			while (a[++i]<a0);
			while (a[--j]>a0);
			if (j<i) break;
			SWAP(a[i],a[j],double);
		}
		a[l+1]=a[j];
		a[j]=a0;
		/*
		 * numerical recipe 方法
		 */

		quick_sort_recursive(a,l,j-1);
		quick_sort_recursive(a,j+1,r);
	}
}

/*
 * 采用循环方式，数组大小小于NN时采用冒泡排序
 */
void quick_sort_loop(double *a, int n)
{
	int i,j,k,l=0,r=n-1;
	double temp,a0;
	int nstack=2*log2(n);
	int istack[nstack],jstack=0;

	while (1){
		if (r-l<NN){
			for (j=l+1;j<=r;++j){
				a0 = a[j];
				for (i=j-1;i>=l;--i){
					if (a[i]<a0) break;
					a[i+1] = a[i];
				}
				a[i+1] = a0;
			}
			if (jstack <= 0) break;
			jstack -= 2;
			l = istack[jstack];
			r = istack[jstack+1];
			continue;
		}
		k = (l+r)/2;
		temp=a[k];
		a[k]=a[l+1];
		a[l+1]=temp;
		if (a[l]>a[r]){
			SWAP(a[l],a[r],double);
//			temp=a[l];
//			a[l]=a[r];
//			a[r]=temp;
		}
		if (a[l+1]>a[r]){
			SWAP(a[l+1],a[r],double);
//			temp=a[l+1];
//			a[l+1]=a[r];
//			a[r]=temp;
		}
		if (a[l]>a[l+1]){
			SWAP(a[l],a[l+1],double);
//			temp=a[l];
//			a[l]=a[l+1];
//			a[l+1]=temp;
		}
		i = l+1;
		j = r;
		a0 = a[l+1];
		while (1){
//			for ( ;(a[++i]<a0); );
//			for ( ;(a[--j]>a0); );
			while (a[++i]<a0);
			while (a[--j]>a0);
			if (j<i) break;
			SWAP(a[i],a[j],double);
//			temp=a[i];
//			a[i]=a[j];
//			a[j]=temp;
		}
		a[l+1] = a[j];
		a[j] = a0;
		if (r-i+1 >= j-l){
			istack[jstack]=i;
			istack[jstack+1]=r;
			r=j-1;
		} else {
			istack[jstack]=l;
			istack[jstack+1]=j-1;
			l=i;
		}
		jstack += 2;
	}
	return;
}

void quick_sort_seq1(double *a, int n, int nmax)
{
	NN = nmax > 2 ? nmax : 2;
	quick_sort_loop(a,n);
}

void quick_sort_seq2(double *a, int n, int nmax)
{
	NN = nmax > 2 ? nmax : 2;
	quick_sort_recursive(a,0,n-1);
}

/* 
 * qs0 即快速排序法的完全递归版本
 * 采用（子）数组的中间元素作为枢
 */
static
qs0(double *a, int l, int r, int depth)
{
	int i,j;
	int k;
	double a0,temp;
	if (l<r){
		i=(l+r)/2;
		SWAP(a[i],a[l+1],double);
		i=l;
		j=r+1;
		a0=a[l];
		while (1){
			while (a[++i]<a0);
			while (a[--j]>a0);
			if (i>=j) break;
			SWAP(a[i],a[j],double);
		}
		a[l]=a[j];
		a[j]=a0;

		depth--;
		if (depth>0){
#pragma omp task
		qs0(a,l,j-1,depth);
#pragma omp task
		qs0(a,j+1,r,depth);
		} else {
		qs0(a,l,j-1,depth);
		qs0(a,j+1,r,depth);
		}
	}
}

/* 
 * qs 在数组较小时采用冒泡排序
 * 采用（子）数组的中间元素作为枢
 * 采用numerical recipe中的方法，首先对l,l+1和r进行排序，然后寻找'j'
 * （必须在数组较小时采用其他方法排序，退出循环采用j<i而非j<=i）
 * 但其中区别暂不明
 */
static
qs(double *a, int l, int r, int depth)
{
	int i,j;
	int k;
	double a0,temp;
	if (r-l<NN){
		for (j=l+1;j<=r;++j){
			a0 = a[j];
			for (i=j-1;i>=l;--i){
				if (a[i]<a0) break;
				a[i+1] = a[i];
			}
			a[i+1] = a0;
		}
		return;
	} else {
		i=(l+r)/2;
		SWAP(a[i],a[l+1],double);
		/*
		i=l;
		j=r+1;
		a0=a[l];
		while (1){
			while (a[++i]<a0);
			while (a[--j]>a0);
			if (i>=j) break;
			SWAP(a[i],a[j],double);
		}
		a[l]=a[j];
		a[j]=a0;
		*/
		/* 
		 * numerical recipe 方法
		 */
		if (a[l]>a[r]){
			SWAP(a[l],a[r],double);
		}
		if (a[l+1]>a[r]){
			SWAP(a[l+1],a[r],double);
		}
		if (a[l]>a[l+1]){
			SWAP(a[l],a[l+1],double);
		}
		i=l+1;
		j=r;
		a0=a[l+1];
		while (1){
			while (a[++i]<a0);
			while (a[--j]>a0);
			if (j<i) break;
			SWAP(a[i],a[j],double);
		}
		a[l+1]=a[j];
		a[j]=a0;
		/*
		 * numerical recipe 方法
		 */

		depth--;
		if (depth>0){
#pragma omp task
		qs(a,l,j-1,depth);
#pragma omp task
		qs(a,j+1,r,depth);
		} else {
		qs(a,l,j-1,depth);
		qs(a,j+1,r,depth);
		}
	}
#ifdef _SHOW_STACK_LEVEL_
	if (level>depth) {
		level=depth;
	}
#endif
}

void quick_sort_omp(double *a, int n, int max_depth)
{
#pragma omp parallel
	{
#pragma omp single nowait
	qs(a,0,n-1,max_depth);
	}
#ifdef _SHOW_STACK_LEVEL_
	printf("level=%d\n",level);
#endif
}
