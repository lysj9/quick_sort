Module mod_sort
	implicit none
	private
	integer,parameter::SP=kind(1.0)
	integer,parameter::DP=kind(1.0d0)
	integer,parameter::PC=SP
	integer,parameter::I4B=selected_int_kind(9)
	integer(I4B),parameter::NN=15,NSTACK=2*8*I4B

	public::quick_sort_noidx,quick_sort_widx
	public::quick_select

	public::quick_sort
	interface quick_sort
		module procedure quick_sort_noidx,quick_sort_widx
	end interface

	contains
	function quick_select(a,k,n)
	implicit none
	real(PC)::quick_select
	integer(I4B),intent(in)::n,k
	real(PC),dimension(n),intent(inout)::a
	real(PC)::a0,ftemp
	integer(I4B)::i,j,l,r

	l=1
	r=n
	do
		if (r-l<=1) then
			if (r-l==1) then
				if (a(l)>a(r)) then
					ftemp=a(l)
					a(l)=a(r)
					a(r)=ftemp
				end if
			end if
			quick_select=a(k)
			exit
		else
			i=(l+r)/2
			ftemp=a(i)
			a(i)=a(l+1)
			a(l+1)=ftemp
			if (a(l)>a(r)) then
				ftemp=a(l)
				a(l)=a(r)
				a(r)=ftemp
			end if
			if (a(l+1)>a(r)) then
				ftemp=a(l+1)
				a(l+1)=a(r)
				a(r)=ftemp
			end if
			if (a(l)>a(l+1)) then
				ftemp=a(l)
				a(l)=a(l+1)
				a(l+1)=ftemp
			end if
			i=l+1
			j=r
			a0=a(l+1)
			do
				do
					i=i+1
					if (a(i)>=a0) exit
				end do
				do
					j=j-1
					if (a(j)<=a0) exit
				end do
				if (j<i) exit
				ftemp=a(i)
				a(i)=a(j)
				a(j)=ftemp
			end do
			a(l+1)=a(j)
			a(j)=a0
			if (j>=k) r=j-1
			if (j<=k) l=i
		end if
	end do
	end function quick_select

end Module
