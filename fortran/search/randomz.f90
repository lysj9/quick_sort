Module mod_randomz
	implicit none
	private
	integer,parameter::k4b=selected_int_kind(9)
	integer,parameter::DP=kind(1.0d0)
	real(DP),parameter::PI=3.14159265358979323846_DP
	real(DP),parameter::TWO_PI=2*PI
	integer(k4b),parameter::A=16807,M=2147483647,Q=127773,R=2836
	integer(k4b)::k
	integer(k4b),save::z=1
	integer(k4b),save::ix=245371421,iy=16807

	public randomz_seed,randomz_seed2
	public randomz,gaussrand
	public::randomz_dbl

	contains
	function randomz()
	implicit none
	real::randomz
	real,parameter::am=nearest(1.0,-1.0)/M
!	real,parameter::am=1.0/M

	ix = ieor(ix,ishft(ix,13))
	ix = ieor(ix,ishft(ix,-17))
	ix = ieor(ix,ishft(ix,5))

	k = iy/Q
	iy = A*(iy-k*Q) - R*k
	if ( iy<0 ) iy = iy+M
!	randomz = x + (y-x)*am*ior(iand(M,ieor(ix,iy)),1)
	randomz = am*ior(iand(M,ieor(ix,iy)),1)

	end function

	function randomz_dbl()
	implicit none
	real::randomz_dbl
	real,parameter::am=nearest(1.0d0,-1.0d0)/M
!	real,parameter::am=1.0/M

	ix = ieor(ix,ishft(ix,13))
	ix = ieor(ix,ishft(ix,-17))
	ix = ieor(ix,ishft(ix,5))

	k = iy/Q
	iy = A*(iy-k*Q) - R*k
	if ( iy<0 ) iy = iy+M
!	randomz = x + (y-x)*am*ior(iand(M,ieor(ix,iy)),1)
	randomz_dbl = am*ior(iand(M,ieor(ix,iy)),1)
	end function

	subroutine randomz_seed(seed)
	implicit none
	integer(k4b),intent(in)::seed
	if ( seed==0 ) then
		call system_clock(z)
	else
		z = abs(seed)
	end if
! numberical recipe:
!	iy = ior(ieor(888889999,abs(z)),1)
!	ix = ieor(777755555,abs(z))

! new strategy:
! use the values after one cycle as initial seed
! ix & iy should be chosen "randomly"
! default iy = 1 -- 16807, ix=16807 -- 245371421
	iy = z
	k = iy/Q
	iy = A*(iy-k*Q) - R*k
	if ( iy<0 ) iy = iy+M

	ix = iy
	ix = ieor(ix,ishft(ix,13))
	ix = ieor(ix,ishft(ix,-17))
	ix = ieor(ix,ishft(ix,5))
	return
	end subroutine

	subroutine randomz_seed2(seed)
	implicit none
	integer(k4b),intent(in)::seed
	integer,dimension(8)::values
	if ( seed==0 ) then
		call date_and_time(VALUES=values)
		z = mod(values(1),100) + 70*( values(2) + 12*&
			&( values(3) + 31*&
			&( values(5) + 23*&
			&( values(6) + 59*values(7) )))) +&
			&values(8)
	else
		z = abs(seed)
	end if
! numberical recipe:
!	iy = ior(ieor(888889999,abs(z)),1)
!	ix = ieor(777755555,abs(z))

! new strategy:
! use the values after one cycle as initial seed
! ix & iy should be chosen "randomly"
! default iy = 1 -- 16807, ix=16807 -- 245371421
	iy = z
	k = iy/Q
	iy = A*(iy-k*Q) - R*k
	if ( iy<0 ) iy = iy+M

	ix = iy
	ix = ieor(ix,ishft(ix,13))
	ix = ieor(ix,ishft(ix,-17))
	ix = ieor(ix,ishft(ix,5))
	return
	end subroutine

	function normal_rand()
	implicit none
	real::normal_rand
	real::x,y,z
	real,save::a(2)
	integer,save::g=2

	g=g+1
	if ( g>2 ) then
		x = randomz()
		y = randomz()*TWO_PI
		z = sqrt( -2*log(x) )
!		z = sqrt( -2*log(x) ) * cos(y)
		a(1) = z*cos(y)
		a(2) = z*sin(y)
		g=1
	end if
	normal_rand = a(g)
	return
	end function

	function gaussrand(mean,sigma)
	implicit none
	real::gaussrand
	real,intent(in)::mean,sigma
	real::x,y,z
	real,save::a(2)
	integer,save::g=2

	g=g+1
	if ( g>2 ) then
		x = randomz()
		y = randomz()*TWO_PI
		z = sqrt( -2*log(x) )
!		z = sqrt( -2*log(x) ) * cos(y)
		a(1) = z*cos(y)
		a(2) = z*sin(y)
		g=1
	end if
	gaussrand = mean + sigma*a(g)
	return
	end function

end Module
