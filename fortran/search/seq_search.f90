Program main
	use mod_sort
	use mod_randomz
	implicit none
	integer,parameter::SP=kind(1.0)
	integer,parameter::DP=kind(1.0d0)
	integer,parameter::PC=DP
	integer,parameter::I4B=selected_int_kind(9)
	integer,parameter::I8B=selected_int_kind(18)
	integer::n=1e7
	real(PC),dimension(:),allocatable::a,b
	integer,dimension(:),allocatable::idx
	integer::i
	integer::nmax
!	real::t0,t1
!	call cpu_time(t0)
!	call cpu_time(t1)
!	t_cost=t1-t0
	real::t_cost
	integer(I8B)::c0,c1,count_rate
	call system_clock(count_rate=count_rate)
	print *,count_rate
	allocate(a(n))
	allocate(b(n))
	allocate(idx(n))
	call randomz_seed(1)
	do i=1,n
		b(i) = randomz_dbl()
	end do

	do nmax=1,51

	! loop, no index
	call set_param(nmax,10)
	a=b
	call system_clock(c0)
	call quick_sort_noidx_seq1(a,n)
	call system_clock(c1)
	t_cost = dble(c1-c0)/(count_rate)
	write (*,'("loop, no-idx:   nmax=",I3,", use ",F15.6," ms...")') &
		nmax,t_cost*1000
	! loop, with index
	a=b
	do i=1,n
		idx(i)=i
	end do
	call system_clock(c0)
	call quick_sort_widx_seq1(a,idx,n)
	call system_clock(c1)
	t_cost = dble(c1-c0)/(count_rate)
	write (*,'("loop, with-idx: nmax=",I3,", use ",F15.6," ms...")') &
		nmax,t_cost*1000
	! recursive, no index
	a=b
	call system_clock(c0,count_rate)
	call quick_sort_noidx_seq2(a,n)
	call system_clock(c1)
	t_cost = dble(c1-c0)/(count_rate)
	write (*,'("recu, no-idx:   nmax=",I3,", use ",F15.6," ms...")') &
		nmax,t_cost*1000
	! recursive, with index
	a=b
	do i=1,n
		idx(i)=i
	end do
	call system_clock(c0)
	call quick_sort_widx_seq2(a,idx,n)
	call system_clock(c1)
	t_cost = dble(c1-c0)/(count_rate)
	write (*,'("recu, with-idx: nmax=",I3,", use ",F15.6," ms...")') &
		nmax,t_cost*1000
	write (*,*) ' '

	end do

	do i=1,n-1
		if (a(i)>a(i+1)) then
			print *,"wrong!",i,a(i),a(i+1)
			stop
		end if
	end do
	print *,'dui'
	do i=1,n-1
		if (b(idx(i))>b(idx(i+1))) then
			print *,"wrong!",i,idx(i),idx(i+1)
			stop
		end if
	end do
	print *,'dui'
	deallocate(a)
	deallocate(b)
	deallocate(idx)
end
