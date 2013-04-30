/* 
 * qs0 即快速排序法的完全递归版本
 * 采用（子）数组的中间元素作为枢
 */
static
qs0(double *a, int l, int r, int depth)
{
	int i,j;
	double a0;
//	double ftemp;
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
