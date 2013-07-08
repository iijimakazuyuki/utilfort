module array
	implicit none
	
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
