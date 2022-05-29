module sparsematrix
  use simple_precision
  use array
  implicit none

  type, public :: csr_matrix
    private
    integer(kind=di) :: nnz, ni, nj
    integer(kind=di), dimension(:), allocatable :: &
      ind_row, ind_col
    real(kind=dp), dimension(:), allocatable :: &
      eles
    logical :: available
  contains
    procedure, public :: &
      show, constor0, constor1, deconstor, toarray!, convertfrom, &
      !toarray
    generic, public :: constor => constor0, constor1
  end type csr_matrix
contains
  subroutine constor0(me, ni, nj, nnz)
    implicit none
  class(csr_matrix) :: me
    integer(kind=di), intent(in) :: ni, nj, nnz
    me%available = .true.
    if (ni < 1 .or. nj < 1 .or. nnz < 1) then
      print *, "Sparse Matrix Shape Error"
      me%available = .false.
    else
      me%nnz = nnz
      me%ni = ni
      me%nj = nj
      call initial_array(me%ind_row, ni+1, 0_di)
      call initial_array(me%ind_col, nnz, 0_di)
      call initial_array(me%eles, nnz, 0.0_dp)
    end if
  end subroutine constor0

  subroutine constor1(me, ni, nj, nnz, ind_row, ind_col, eles)
  class(csr_matrix) :: me
    integer(kind=di), intent(in) :: ni, nj, nnz
    integer(kind=di), dimension(:), intent(in) :: &
      ind_row, ind_col
    real(kind=dp), dimension(:), intent(in) :: eles
    call me%constor0(ni, nj, nnz)
    if (me%available) then
      me%ind_row = ind_row
      me%ind_col = ind_col
      me%eles = eles
    end if
  end subroutine constor1

  subroutine toarray(me, arr)
    implicit none
    class(csr_matrix) :: me 
      real(kind=dp), dimension(:, :), intent(inout) :: arr
      integer(kind=di) :: i, j
      if (size(arr, 1) /= me%ni .or. size(arr, 2) /= me%nj ) then
        print *, "The size of arr is wrong"
      else
        do i = 1, me%ni
          do j = me%ind_row(i), me%ind_row(i+1) - 1
            arr(i, me%ind_col(j)) = me%eles(j)
          end do
        end do
      end if
    end subroutine toarray

    subroutine fromarray(me, arr)
    implicit none
    class(csr_matrix) :: me 
      real(kind=dp), dimension(:, :), intent(in) :: arr
      real(kind=dp), dimension(:), allocatable :: eles
      integer(kind=di), dimension(:), allocatable :: ind_col, ind_row
      integer(kind=di) :: ni, nj, nnz, real_nnz, i_nozero
      integer(kind=di) :: i, j
      ni = size(arr, 1)
      nj = size(arr, 2)
      if (ni < 2 .and. nj < 2 ) then
        print *, "The size of arr is wrong"
        return
      end if
      nnz = ni * nj + 1
      real_nnz = 0_di
      i_nozero = 0_di
      ind_row(1) = 1_di
      do i = 1, ni
        do j = 1, nj
          if (abs(arr(i, j)) < 1.0e-10_dp) then
            real_nnz = real_nnz + 1_di
            eles(real_nnz) = a(i, j)
            ind_col(real_nnz) = j
          end if
        end do 
      end do
      end subroutine fromarray

    !subroutine convertfrom(me, arr)
    !implicit none
    !class(csr_matrix) :: me
    !real(kind=dp), dimension(:,:), intent(in) :: &
    !arr
    !integer(kind=di) :: ni, nj
    !integer(kind=di) :: i, j
    !ni = shape(arr, 1)
    !nj = shape(arr, 2)
    !do i = 1, ni
    !do j = 1, nj
    !end do 
    !end do
    !end subroutine convertfrom 

  subroutine show(me)
    implicit none
  class(csr_matrix) :: me
    print *, me%ni, me%nj, me%nnz
    print *, me%ind_row
    print *, me%ind_col
    print *, me%eles
  end subroutine show

  subroutine deconstor(me)
    implicit none
  class(csr_matrix) :: me
    call close_array(me%ind_row)
    call close_array(me%ind_col)
    call close_array(me%eles)
  end subroutine deconstor
end module sparsematrix

program test
  use sparsematrix
  implicit none
  type(csr_matrix) :: a, b
  real(kind=dp), dimension(4, 4) :: arr, arr
  call b%constor(4_di, 4_di, 10_di, &
    (/1_di,3_di,6_di,9_di,11_di/), &
    (/1_di,2_di,1_di,2_di,3_di,2_di,3_di,4_di,3_di,4_di/), &
    (/-1.5_dp, 0.25_dp, 0.25_dp, -1.5_dp, 0.25_dp, 0.25_dp, -1.5_dp, 0.25_dp, 0.25_dp, -1.5_dp/))
  call b%show()
  arr = 0.0_dp
  call b%toarray(arr)
  print "(4(1x, F10.6))", arr
  call b%deconstor()
  call a%fromarray(arr)
  call a%show()
  call a%deconstor()
end program test
