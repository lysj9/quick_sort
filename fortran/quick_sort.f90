Module mod_sort
	use omp_lib
	implicit none
	private
	integer,parameter::SP=kind(1.0)
	integer,parameter::DP=kind(1.0d0)
	integer,parameter::PC=DP
	integer,parameter::I4B=selected_int_kind(9)
	integer(I4B)::NN=15
	integer(I4B)::MD=10
!	integer(I4B),parameter::NN=15
!	integer(I4B),parameter::MD=10
	integer(I4B),parameter::NSTACK=2*8*I4B

!	public::quick_sort_noidx_loop,quick_sort_noidx_recursive
!	public::quick_sort_widx_loop,quick_sort_widx_recursive
	public::set_param

	public::quick_sort_noidx_seq
	public::quick_sort_widx_seq
	public::quick_sort_noidx_omp
	public::quick_sort_widx_omp

	interface swap
		module procedure swap_i,swap_r
	end interface

	public::quick_sort_noidx,quick_sort_widx
	public::quick_sort
	interface quick_sort
		module procedure quick_sort_noidx,quick_sort_widx
	end interface


	contains
	! set parameters (NN, MD)
	subroutine set_param(nmax,max_depth)
	integer(I4B),intent(in)::nmax,max_depth
	NN=nmax
	if (NN<3) NN=3
	MD=max_depth
	end subroutine

	! swap two value (integer)
	subroutine SWAP_I(ix,iy)
	implicit none
	integer(I4B),intent(inout)::ix,iy
	integer(I4B)::iz
	iz=ix
	ix=iy
	iy=iz
	return
	end subroutine

	! swap two value (real)
	subroutine SWAP_R(rx,ry)
	implicit none
	real(PC),intent(inout)::rx,ry
	real(PC)::rz
	rz=rx
	rx=ry
	ry=rz
	return
	end subroutine

	! quick sort
	subroutine quick_sort_noidx(a,n,p)
	implicit none
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),intent(in)::n
	integer,optional,intent(in)::p
	integer::use_openmp
	integer::num_threads
	if (present(p)) then
		use_openmp=p
	else
		use_openmp=1
	endif

	if (use_openmp==0) then
		call quick_sort_noidx_loop(a,n)
!		call quick_sort_noidx_recursive(a,n,1,n)
	else
!$OMP PARALLEL
!$OMP SINGLE
!$		call qs_noidx(a,n,1,n,MD)
!$OMP END SINGLE NOWAIT
!$OMP END PARALLEL
!$		return

!		call quick_sort_noidx_omp(a,n)

		call quick_sort_noidx_loop(a,n)
!		call quick_sort_noidx_recursive(a,n,1,n)
	end if
	end subroutine

	subroutine quick_sort_widx(a,idx,n,p)
	implicit none
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),dimension(n),intent(inout)::idx
	integer(I4B),intent(in)::n
	integer,optional,intent(in)::p
	integer::use_openmp
	integer::num_threads
	if (present(p)) then
		use_openmp=p
	else
		use_openmp=1
	endif

	if (use_openmp==0) then
		call quick_sort_widx_loop(a,idx,n)
!		call quick_sort_widx_recursive(a,idx,n,1,n)
	else
!$OMP PARALLEL
!$OMP SINGLE
!$		call qs_widx(a,idx,n,1,n,MD)
!$OMP END SINGLE NOWAIT
!$OMP END PARALLEL
!$		return

!		call quick_sort_widx_omp(a,idx,n)

		call quick_sort_widx_loop(a,idx,n)
!		call quick_sort_widx_recursive(a,idx,n,1,n)
	end if
	end subroutine

	subroutine quick_sort_noidx_seq(a,n)
	implicit none
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),intent(in)::n
	call quick_sort_noidx_loop(a,n)
!	call quick_sort_noidx_recursive(a,n,1,n)
	end subroutine

	subroutine quick_sort_widx_seq(a,idx,n)
	implicit none
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),dimension(n),intent(inout)::idx
	integer(I4B),intent(in)::n
	call quick_sort_widx_loop(a,idx,n)
!	call quick_sort_widx_recursive(a,idx,n,1,n)
	end subroutine

	subroutine quick_sort_noidx_omp(a,n)
	implicit none
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),intent(in)::n
!$OMP PARALLEL
!$OMP SINGLE
	call qs_noidx(a,n,1,n,MD)
!$OMP END SINGLE NOWAIT
!$OMP END PARALLEL
	end subroutine

	subroutine quick_sort_widx_omp(a,idx,n)
	implicit none
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),dimension(n),intent(inout)::idx
	integer(I4B),intent(in)::n
!$OMP PARALLEL
!$OMP SINGLE
	call qs_widx(a,idx,n,1,n,MD)
!$OMP END SINGLE NOWAIT
!$OMP END PARALLEL
	end subroutine

	subroutine quick_sort_noidx_loop(a,n)
	implicit none
!	integer(I4B),parameter::NN=15,NSTACK=2*8*I4B
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),intent(in)::n
	integer(I4B)::i,j
	integer(I4B)::l,r
	integer(I4B)::jstack
	real(PC)::a0
!	real(PC)::ftemp
	integer(I4B),dimension(nstack)::istack

	jstack=0
	l=1
	r=n
	do
		if (r-l<NN) then
			do j=l+1,r
				a0=a(j)
				do i=j-1,l,-1
					if (a(i)<=a0) exit
					a(i+1)=a(i)
				end do
				a(i+1)=a0
			end do
			if (jstack==0) exit
			r=istack(jstack)
			l=istack(jstack-1)
			jstack=jstack-2
		else
			i=(l+r)/2
			call SWAP(a(i),a(l+1))
			if (a(l)>a(r)) then
				call SWAP(a(l),a(r))
			end if
			if (a(l+1)>a(r)) then
				call SWAP(a(l+1),a(r))
			end if
			if (a(l)>a(l+1)) then
				call SWAP(a(l),a(l+1))
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
				call SWAP(a(i),a(j))
			end do
			a(l+1)=a(j)
			a(j)=a0
			jstack=jstack+2
			if (r-i+1>=j-l) then
				istack(jstack)=r
				istack(jstack-1)=i
				r=j-1
			else
				istack(jstack)=j-1
				istack(jstack-1)=l
				l=i
			end if
		end if
	end do
	end subroutine quick_sort_noidx_loop

	subroutine quick_sort_widx_loop(a,idx,n)
	implicit none
!	integer(I4B),parameter::NN=15,NSTACK=2*8*I4B
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),dimension(n),intent(inout)::idx
	integer(I4B),intent(in)::n
	integer(I4B)::i,j
	integer(I4B)::l,r
	integer(I4B)::jstack
	real(PC)::a0
!	real(PC)::ftemp
	integer(I4B)::id0
!	integer(I4B)::itemp
	integer(I4B),dimension(nstack)::istack

	jstack=0
	l=1
	r=n
	do
		if (r-l<NN) then
			do j=l+1,r
				a0=a(j)
				id0=idx(j)
				do i=j-1,l,-1
					if (a(i)<=a0) exit
					a(i+1)=a(i)
					idx(i+1)=idx(i)
				end do
				a(i+1)=a0
				idx(i+1)=id0
			end do
			if (jstack==0) exit
			r=istack(jstack)
			l=istack(jstack-1)
			jstack=jstack-2
		else
			i=(l+r)/2
			call SWAP(a(i),a(l+1))
			call SWAP(idx(i),idx(l+1))
			if (a(l)>a(r)) then
				call SWAP(a(l),a(r))
				call SWAP(idx(l),idx(r))
			end if
			if (a(l+1)>a(r)) then
				call SWAP(a(l+1),a(r))
				call SWAP(idx(l+1),idx(r))
			end if
			if (a(l)>a(l+1)) then
				call SWAP(a(l),a(l+1))
				call SWAP(idx(l),idx(l+1))
			end if
			i=l+1
			j=r
			a0=a(l+1)
			id0=idx(l+1)
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
				call SWAP(a(i),a(j))
				call SWAP(idx(i),idx(j))
			end do
			a(l+1)=a(j)
			a(j)=a0
			idx(l+1)=idx(j)
			idx(j)=id0
			jstack=jstack+2
			if (r-i+1>=j-l) then
				istack(jstack)=r
				istack(jstack-1)=i
				r=j-1
			else
				istack(jstack)=j-1
				istack(jstack-1)=l
				l=i
			end if
		end if
	end do
	end subroutine quick_sort_widx_loop

	recursive subroutine quick_sort_noidx_recursive(a,n,l,r)
	implicit none
!	integer(I4B),parameter::NN=15,NSTACK=2*8*I4B
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),intent(in)::n
	integer(I4B),intent(in)::l,r
	integer(I4B)::i,j
	real(PC)::a0
!	real(PC)::ftemp

	if (r-l<NN) then
		do j=l+1,r
			a0=a(j)
			do i=j-1,l,-1
				if (a(i)<=a0) exit
				a(i+1)=a(i)
			end do
			a(i+1)=a0
		end do
		return
	else
		i=(l+r)/2
		call SWAP(a(i),a(l+1))
		if (a(l)>a(r)) then
			call SWAP(a(l),a(r))
		end if
		if (a(l+1)>a(r)) then
			call SWAP(a(l+1),a(r))
		end if
		if (a(l)>a(l+1)) then
			call SWAP(a(l),a(l+1))
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
			call SWAP(a(i),a(j))
		end do
		a(l+1)=a(j)
		a(j)=a0
		call quick_sort_noidx_recursive(a,n,l,j-1)
		call quick_sort_noidx_recursive(a,n,j+1,r)
	end if
	end subroutine quick_sort_noidx_recursive

	recursive subroutine quick_sort_widx_recursive(a,idx,n,l,r)
	implicit none
!	integer(I4B),parameter::NN=15,NSTACK=2*8*I4B
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),dimension(n),intent(inout)::idx
	integer(I4B),intent(in)::n
	integer(I4B),intent(in)::l,r
	integer(I4B)::i,j
	real(PC)::a0
!	real(PC)::ftemp
	integer(I4B)::id0
!	integer(I4B)::itemp

	if (r-l<NN) then
		do j=l+1,r
			a0=a(j)
			id0=idx(j)
			do i=j-1,l,-1
				if (a(i)<=a0) exit
				a(i+1)=a(i)
				idx(i+1)=idx(i)
			end do
			a(i+1)=a0
			idx(i+1)=id0
		end do
		return
	else
		i=(l+r)/2
		call SWAP(a(i),a(l+1))
		call SWAP(idx(i),idx(l+1))
		if (a(l)>a(r)) then
			call SWAP(a(l),a(r))
			call SWAP(idx(l),idx(r))
		end if
		if (a(l+1)>a(r)) then
			call SWAP(a(l+1),a(r))
			call SWAP(idx(l+1),idx(r))
		end if
		if (a(l)>a(l+1)) then
			call SWAP(a(l),a(l+1))
			call SWAP(idx(l),idx(l+1))
		end if
		i=l+1
		j=r
		a0=a(l+1)
		id0=idx(l+1)
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
			call SWAP(a(i),a(j))
			call SWAP(idx(i),idx(j))
		end do
		a(l+1)=a(j)
		a(j)=a0
		idx(l+1)=idx(j)
		idx(j)=id0
		call quick_sort_widx_recursive(a,idx,n,l,j-1)
		call quick_sort_widx_recursive(a,idx,n,j+1,r)
	end if
	end subroutine quick_sort_widx_recursive

	recursive subroutine qs_noidx(a,n,l,r,depth)
	implicit none
!	integer(I4B),parameter::NN=15,NSTACK=2*8*I4B
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),intent(in)::n
	integer(I4B),intent(in)::l,r
	integer(I4B),intent(in)::depth
	integer(I4B)::i,j
	real(PC)::a0
!	real(PC)::ftemp

	if (r-l<NN) then
		do j=l+1,r
			a0=a(j)
			do i=j-1,l,-1
				if (a(i)<=a0) exit
				a(i+1)=a(i)
			end do
			a(i+1)=a0
		end do
		return
	else
		i=(l+r)/2
		call SWAP(a(i),a(l+1))
		if (a(l)>a(r)) then
			call SWAP(a(l),a(r))
		end if
		if (a(l+1)>a(r)) then
			call SWAP(a(l+1),a(r))
		end if
		if (a(l)>a(l+1)) then
			call SWAP(a(l),a(l+1))
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
			call SWAP(a(i),a(j))
		end do
		a(l+1)=a(j)
		a(j)=a0

!		depth=depth-1
		if (depth>0) then
			!$OMP TASK SHARED(a)
			call qs_noidx(a,n,l,j-1,depth-1)
			!$OMP END TASK
			!$OMP TASK SHARED(a)
			call qs_noidx(a,n,j+1,r,depth-1)
			!$OMP END TASK
		else
			call quick_sort_noidx_loop(a(l:j-1),j-l)
			call quick_sort_noidx_loop(a(j+1:r),r-j)
!			call qs_noidx(a,n,l,j-1,depth-1)
!			call qs_noidx(a,n,j+1,r,depth-1)
		end if
	end if
	end subroutine qs_noidx

	recursive subroutine qs_widx(a,idx,n,l,r,depth)
	implicit none
!	integer(I4B),parameter::NN=15,NSTACK=2*8*I4B
	real(PC),dimension(n),intent(inout)::a
	integer(I4B),dimension(n),intent(inout)::idx
	integer(I4B),intent(in)::n
	integer(I4B),intent(in)::l,r
	integer(I4B),intent(in)::depth
	integer(I4B)::i,j
	real(PC)::a0
!	real(PC)::ftemp
	integer(I4B)::id0
!	integer(I4B)::itemp

	if (r-l<NN) then
		do j=l+1,r
			a0=a(j)
			id0=idx(j)
			do i=j-1,l,-1
				if (a(i)<=a0) exit
				a(i+1)=a(i)
				idx(i+1)=idx(i)
			end do
			a(i+1)=a0
			idx(i+1)=id0
		end do
		return
	else
		i=(l+r)/2
		call SWAP(a(i),a(l+1))
		call SWAP(idx(i),idx(l+1))
		if (a(l)>a(r)) then
			call SWAP(a(l),a(r))
			call SWAP(idx(l),idx(r))
		end if
		if (a(l+1)>a(r)) then
			call SWAP(a(l+1),a(r))
			call SWAP(idx(l+1),idx(r))
		end if
		if (a(l)>a(l+1)) then
			call SWAP(a(l),a(l+1))
			call SWAP(idx(l),idx(l+1))
		end if
		i=l+1
		j=r
		a0=a(l+1)
		id0=idx(l+1)
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
			call SWAP(a(i),a(j))
			call SWAP(idx(i),idx(j))
		end do
		a(l+1)=a(j)
		a(j)=a0
		idx(l+1)=idx(j)
		idx(j)=id0

!		depth=depth-1
		if (depth>0) then
			!$OMP TASK SHARED(a,idx)
			call qs_widx(a,idx,n,l,j-1,depth-1)
			!$OMP END TASK
			!$OMP TASK SHARED(a,idx)
			call qs_widx(a,idx,n,j+1,r,depth-1)
			!$OMP END TASK
		else
			call quick_sort_widx_loop(a(l:j-1),idx(l:j-1),j-l)
			call quick_sort_widx_loop(a(j+1:r),idx(j+1:r),r-j)
!			call qs_widx(a,idx,n,l,j-1,depth-1)
!			call qs_widx(a,idx,n,j+1,r,depth-1)
		end if
	end if
	end subroutine qs_widx
end Module
