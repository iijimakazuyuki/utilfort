module array
	use bitset_mod
	implicit none
	
	type :: jag_i
		integer, allocatable :: val(:)
		integer, allocatable :: idx(:)
	end type
	
	interface has
		module procedure has_i
	end interface
	
	interface indexof
		module procedure indexof_i, indexof_b
	end interface
	
	interface count
		module procedure count_i
	end interface
contains
	function bitset_to_jag(bs) result(a)
		type(jag_i) :: a
		type(bitset), intent(in) :: bs(:)
		integer :: i, j, k, n, m
		n = size(bs)
		allocate(a%idx(0:n))
		do i=1, n
			a%idx(i) = a%idx(i-1) + count_bitset(bs(i))
		end do
		allocate(a%val(a%idx(n)))
		k = 0
		do i=1, n
			do j=1, bs(i)%n
				if(btest_bitset(bs(i),j)) then
					k = k + 1
					a%val(k) = j
				end if
			end do
		end do
	end function
	
	integer function count_i(a,x) result(count)
		integer, intent(in) :: a(:), x
		integer :: i
		count = 0
		do i=1, size(a)
			if(a(i) == x) count = count + 1
		end do
	end function
	
	logical function has_i(a,x) result(has)
		integer, intent(in) :: a(:), x
		integer :: i
		has = .false.
		do i=1, size(a)
			if(a(i) == x) then
				has = .true.
				exit
			end if
		end do
	end function
	
	integer function indexof_b(a, x) result(idx)
		logical, intent(in) :: a(:), x
		integer :: i
		idx = -1
		do i=1, size(a)
			if(a(i) .eqv. x) then
				idx = i
				exit
			end if
		end do
	end function
	
	integer function indexof_i(a, x) result(idx)
		integer, intent(in) :: a(:), x
		integer :: i
		idx = -1
		do i=1, size(a)
			if(a(i) == x) then
				idx = i
				exit
			end if
		end do
	end function
end module
