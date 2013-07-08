module jag
	use bitset_mod
	implicit none
	
	type :: jag_i
		integer :: n, m
		integer, allocatable :: val(:)
		integer, allocatable :: idx(:)
	end type
contains
	function bitset_to_jag(bs) result(a)
		type(jag_i) :: a
		type(bitset), intent(in) :: bs(:)
		integer :: i, j, k
		a%n = size(bs)
		allocate(a%idx(0:a%n))
		a%idx(0) = 0
		do i=1, a%n
			a%idx(i) = a%idx(i-1) + count_bitset(bs(i))
		end do
		a%m = a%idx(a%n)
		allocate(a%val(a%m))
		k = 0
		do i=1, a%n
			do j=1, bs(i)%n
				if(btest_bitset(bs(i),j)) then
					k = k + 1
					a%val(k) = j
				end if
			end do
		end do
	end function
	
	subroutine print_jag(a)
		type(jag_i), intent(in) :: a
		integer :: i, j, si, ei
		do i=1, a%n
			si = a%idx(i-1) + 1
			ei = a%idx(i)
			if(si <= ei) print *, a%val(si:ei)
		end do
	end subroutine
end module
