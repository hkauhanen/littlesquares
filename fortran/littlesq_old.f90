! littlesq.f90
!
! Henri Kauhanen 2017
!
! Run some simulations of our "little squares" model on a regular
! lattice with periodic boundary conditions. This code runs such
! simulations for a given value of q (probability of voter event)
! and a given value of tau (temperature), for _RESOLUTION_ values of
! p_i and p_e. Output is in the form of a whitespace separated table.
!
! The program takes three command-line arguments: the first argument is
! tau, the second q, the third the filename for the outfile. Other
! parameters need to be set using preprocessor directives, see directly
! below.



! Some preprocessor directives here. If compiling with gfortran, don't
! forget to use the -cpp flag so that these get processed. In order of
! appearance: length of lattice side; multiplier M, to decide number of
! iterations, which will be N*N*M; length of the measurement phase L:
! rho and sigma will be calculated for the last L iterations; how many
! values of p_i (and p_e) to consider.
#define _N_ 50
#define _ITERMULT_ 100000
#define _MPLENGTH_ 1000
#define _RESOLUTION_ 20


! Subroutine to return a random von Neumann neighbour of a lattice node
subroutine randneigh(sx, sy, sN, snx, sny)
  implicit none
  integer, intent(in) :: sx, sy, sN
  integer, intent(out) :: snx, sny
  real :: randnob

  ! get random snx, sny
  call random_number(randnob)
  if (randnob <= 0.25) then
    ! north
    snx = sx
    sny = sy - 1
  else if (randnob > 0.25 .and. randnob <= 0.50) then
    ! east
    snx = sx + 1
    sny = sy
  else if (randnob > 0.50 .and. randnob <= 0.75) then
    ! south
    snx = sx
    sny = sy + 1
  else
    ! west
    snx = sx - 1
    sny = sy
  end if

  ! take care of lattice boundaries
  if (snx < 1) then
    snx = sN
  else if (snx > sN) then
    snx = 1
  end if
  if (sny < 1) then
    sny = sN
  else if (sny > sN) then
    sny = 1
  end if
end subroutine randneigh


! Subroutine to set the RNG seed from environmental noise; essential if
! this is run parallel on Condor
subroutine seed_from_urandom()
  implicit none
  integer :: i(12)
  open(99, file='/dev/urandom', access='stream', form='UNFORMATTED')
  read(99) i
  close(99)
  call random_seed(put=i)
end subroutine seed_from_urandom


! The program itself
program littlesq
  ! Declare variables
  implicit none
  character(len=32) :: comarg1, comarg2, comarg3 ! command-line arguments
  integer :: N, iter, mp_length, measurement_phase, resotop, nact, ups ! stuff
  integer :: i, j ! counters
  integer :: x, y ! lattice coordinates
  integer :: nx, ny ! lattice coordinates for neighbour
  real :: randno, randno2 ! to hold random reals
  real :: q, rho, sigma, pe, alpha, intended_alpha
  real, dimension(_RESOLUTION_) :: pin
  integer, dimension(_N_, _N_) :: spins
  real, dimension(_N_, _N_) :: rands

  ! Read command-line argument
  call getarg(1, comarg1)
  call getarg(2, comarg2)
  call getarg(3, comarg3)

  ! Initialize variables
  N = _N_
  nx = 1
  ny = 1
  iter = _ITERMULT_*N*N
  mp_length = _MPLENGTH_
  measurement_phase = iter - mp_length
  resotop = _RESOLUTION_
  read(comarg1, *) intended_alpha
  read(comarg2, *) q
  pin = seq(real(0), (q/(1-q))*intended_alpha, _RESOLUTION_)

  ! Open file connection, write csv header
  open(1, file=comarg3)
  write(1,*) ' "pi" "pe" "q" "N" "ups" "nact" "rho" "sigma" "alpha" "ialpha"'

  ! Set random number seed
  !call random_seed()
  call seed_from_urandom()

  ! Loop through pi values, running simulation
  do i = 1,resotop
    ! Initialize lattice
    call random_number(rands)
    do x = 1,N
      do y = 1,N
        if (rands(x,y) <= 0.5) then
          spins(x,y) = -1
        else
          spins(x,y) = 1
        end if
      end do
    end do

    ! Set pe
    pe = (q/(1-q))*intended_alpha - pin(i)

    ! Run simulation
    nact = 0
    ups = 0
    do j = 1,iter
      ! Pick a random node of the lattice
      call random_number(randno)
      randno = randno*N
      x = int(randno) + 1
      call random_number(randno)
      randno = randno*N
      y = int(randno) + 1

      ! Which event to execute?
      call random_number(randno)
      call random_number(randno2)
      if (randno <= q) then
        ! voter event: first, pick one von Neumann neighbour of (x,y)
        call randneigh(x, y, N, nx, ny)
        ! then copy spin from (nx,ny) into (x,y)
        spins(x,y) = spins(nx,ny)
      else
        if (spins(x,y) == 1) then
          if (randno2 <= pe) then
            ! egress event
            spins(x,y) = -1
          end if
        else
          if (randno2 <= pin(i)) then
            ! ingress event
            spins(x,y) = 1
          end if
        end if
      end if

      ! Find the proportion of active surfaces during the latter part
      ! of the run (measurement_phase)
      if (j > measurement_phase) then
        do x = 1,N
          do y = 1,N
            call randneigh(x, y, N, nx, ny)
            if (spins(x,y) /= spins(nx,ny)) then
              nact = nact + 1
            end if
          end do
        end do

        ! Find density of up spin
        do x = 1,N
          do y = 1,N
            if (spins(x,y) == 1) then
              ups = ups + 1
            end if
          end do
        end do
      end if
    end do

    ! Calculate rho, sigma and alpha
    sigma = real(nact)/real(real(mp_length)*real(N*N))
    rho = real(ups)/real(real(mp_length)*real(N*N))
    alpha = ((1-q)*(pin(i) + pe))/q

    ! Writeout
    write(1,*) pin(i), pe, q, N, ups, nact, rho, sigma, alpha, intended_alpha
  end do

  ! Close file connection and goodbye
  close(1)


  ! Helper functions
contains

  ! Returns a sequence of real numbers from 'from' to 'to' of length 'len'
  pure function seq(from, to, len) result(arr)
    implicit none
    real, intent(in) :: from, to
    integer, intent(in) :: len
    real, dimension(len) :: arr
    integer :: i
    arr(1) = from
    do i = 2,len
      arr(i) = arr(i-1) + (to-from)/(len-1)
    end do
  end function seq

  ! Returns a logarithmic sequence from 'from' to 'to' of length 'len'
  pure function logseq(from, to, len) result(arr)
    implicit none
    real, intent(in) :: from, to
    integer, intent(in) :: len
    real, dimension(len) :: arr
    integer :: i
    arr = seq(log(from), log(to), len)
    do i = 1,len
      arr(i) = exp(arr(i))
    end do
  end function logseq



end program littlesq

