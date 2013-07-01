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
		bs%set = 0
	end function
	
	function ieor_bitset(a,b) result(c)
		type(bitset) :: c
		type(bitset), intent(in) :: a, b
		integer :: i, n
		c = new_bitset(a%n)
		do i=1, size(a%set)
			c%set(i) = ieor(a%set(i), b%set(i))
		end do
	end function
	
	function iand_bitset(a,b) result(c)
		type(bitset) :: c
		type(bitset), intent(in) :: a, b
		integer :: i, n
		c = new_bitset(a%n)
		do i=1, size(a%set)
			c%set(i) = iand(a%set(i), b%set(i))
		end do
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
		j = (i-1)/bit_size(i) + 1
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
	
	integer function next_bitset(bs, i) result(n)
		!todo binary search improvement
		type(bitset), intent(in) :: bs
		integer, intent(in) :: i
		
		integer :: j, k, sk, nbits, nsets
		logical :: ok
		
		nbits = bit_size(i)
		nsets = size(bs%set)
		n = 0
		do j=i/nbits+1, nsets
			if(bs%set(j) /= 0) then
				ok = .false.
				sk = i-(j-1)*nbits
				if(sk < 1) sk = 1
				do k=sk, nbits
					if(btest(bs%set(j),k)) then
						n = (j-1)*nbits+k
						ok = .true.
						exit
					end if
				end do
				if(ok) exit
			end if
		end do
	end function
end module
