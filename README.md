utilfort
========

Utility for Fortran

array
-----

###indexof

###has

###count

bitset_mod
----------

    type(bitset) :: bs
    bs = new_bitset(10)    ! 0000000000
    call set_bitset(bs, 3) ! 0010000000
    call set_bitset(bs, 7) ! 0010001000

###new_bitset

###print_bitset

###ieor_bitset

###iand_bitset

###next_bitset

###btest_bitset

###set_bitset

###clr_bitset

###count_bitset
