! empirical.f90
!
! Henri Kauhanen 2016-2017



program empirical
  ! Declare variables
  implicit none
  integer :: i, j ! counters
  character(len=32) :: carg1, carg2, carg3, carg4, carg5, carg6 ! command-line arguments
  integer, dimension(_DIMENSION_,3) :: adjs ! adjacency matrix
  real :: p, q, r ! model parameters
  integer :: iter ! number of iterations
  real :: randno, randno2 ! random numbers
  integer :: x, y ! current site and its neighbour

  ! Read command-line arguments
  call getarg(1, carg1)
  call getarg(2, carg2)
  call getarg(3, carg3)
  call getarg(4, carg4)
  call getarg(5, carg5)
  call getarg(6, carg6)
  read(carg3, *) p
  read(carg4, *) q
  read(carg5, *) r
  read(carg6, *) iter

  ! Initialize RNG seed
  call random_seed()

  ! Read in adjacency matrix
  open(99, file=carg1)
  do i = 1,_DIMENSION_
    read(99,*) adjs(i,:)
  end do
  close(99)

  ! Simulation
  do j = 1,iter
    ! Pick a site at random
    call random_number(randno)
    randno = randno*_DIMENSION_
    x = INT(randno) + 1

    ! If x does not have a neighbour, we treat it as a fixed
    ! boundary condition and do nothing
    if (adjs(x,2) /= 0) then
      ! Which event to execute?
      call random_number(randno)
      if (randno < r) then
        ! Spatial event
        y = adjs(x,2)
        adjs(x,3) = adjs(y,3)
      else
        ! Ingress/egress event
        call random_number(randno2)
        if (adjs(x,3) == 1) then
          if (randno2 < q) then
            adjs(x,3) = -1
          end if
        else
          if (randno2 < p) then
            adjs(x,3) = 1
          end if
        end if
      end if
    end if
  end do

  ! Writeout
  open(98, file=carg2)
  do i = 1,_DIMENSION_
    write(98,*) adjs(i,:)
  end do
  close(98)

end program empirical
