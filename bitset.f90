module bitset_mod
	implicit none
	type :: bitset
		integer :: n
		integer, allocatable :: set(:)
	end type
contains
	function new_bitset(n) result(bs)
		type(bitset) :: bs
		integer, intent(in) :: n
		bs%n = n
		allocate(bs%set(1+(n-1)/bit_size(n)))
	end function
	
	subroutine print_bitset(bs)
		type(bitset) :: bs
		character(len=bs%n) :: a
		integer :: i, j
		a = "";
		do i=1, bs%n
			if(btest_bitset(bs,i)) then
				a = trim(a) // "1"
			else
				a = trim(a) // "0"
			end if
		end do
		print *, a
	end subroutine
	
	logical function btest_bitset(bs,idx) result(b)
		type(bitset), intent(in) :: bs
		integer, intent(in) :: idx
		integer :: i, j
		i = idx
		call map_bitset(i,j)
		b = btest(bs%set(j),i)
	end function
	
	subroutine set_bitset(bs,idx)
		type(bitset), intent(inout) :: bs
		integer, intent(in) :: idx
		integer :: i, j
		i = idx
		call map_bitset(i,j)
		print *, i, j
		bs%set(j) = ibset(bs%set(j),i)
	end subroutine
	
	subroutine clr_bitset(bs,idx)
		type(bitset), intent(inout) :: bs
		integer, intent(in) :: idx
		integer :: i, j
		i = idx
		call map_bitset(i,j)
		bs%set(j) = ibclr(bs%set(j),i)
	end subroutine
	
	subroutine map_bitset(i,j)
		integer, intent(inout) :: i, j
		j = i/bit_size(i) + 1
		i = i - (j-1)*bit_size(i)
	end subroutine
	
	integer function count_bitset(bs) result(c)
		type(bitset) :: bs
		integer :: i, j, k
		i = 0
		j = 0
		c = 0
		do k=1, bs%n
			i = i + 1
			if(i > bit_size(i)) then
				j = j + 1
			end if
			if(btest(bs%set(j),i)) then
				c = c + 1
			end if
		end do
	end function
end module
