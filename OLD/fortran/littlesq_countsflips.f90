! littlesq_countsflips.f90
!
! Henri Kauhanen 2017-2019
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
!
! This version, in contrast to littlesq.f90, also counts
! how many spin-flips occur, during the measurement phase.



! Some preprocessor directives here. If compiling with gfortran, don't
! forget to use the -cpp flag so that these get processed. In order of
! appearance: length of lattice side; multiplier M, to decide number of
! iterations, which will be N*N*M; length of the measurement phase L:
! rho and sigma will be calculated for the last L iterations; how many
! values of p_i (and p_e) to consider.
#define _N_ 100
#define _ITERMULT_ 5
#define _ALPHARESOLUTION_ 35
#define _RHORESOLUTION_ 10


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


! The program itself
program littlesq_countsflips
  ! Declare variables
  implicit none
  character(len=32) :: comarg1, comarg2 ! command-line arguments
  integer :: N, iter, mp_length, measurement_phase, resotop, nact, ups ! stuff
  integer :: no_flips ! number of spin-flips
  integer :: i, j, alphactr ! counters
  integer :: x, y ! lattice coordinates
  integer :: nx, ny ! lattice coordinates for neighbour
  real :: randno, randno2 ! to hold random reals
  real :: q, rho, sigma, pi, pe, alpha, intended_alpha, multip
  real, dimension(_ALPHARESOLUTION_) :: alphaseq
  real, dimension(_RHORESOLUTION_) :: pin
  integer, dimension(_N_, _N_) :: spins
  real, dimension(_N_, _N_) :: rands

  ! Read command-line argument
  call getarg(1, comarg1)
  call getarg(2, comarg2)

  ! Initialize variables
  N = _N_
  nx = 1
  ny = 1
  iter = _ITERMULT_*N*N
  read(comarg1, *) multip
  q = real(2)/real(3)

  ! Open file connection, write csv header
  open(1, file=comarg2)
  write(1,*) ' "pi" "pe" "q" "N" "ialpha" "flips"'

  ! Set random number seed
  call random_seed()
  !call seed_from_urandom()

  ! Loop through alpha (=tau) values
  alphaseq = logseq(real(0.01), 0.5, _ALPHARESOLUTION_)
  do alphactr = 1,_ALPHARESOLUTION_
    intended_alpha = alphaseq(alphactr)
    pin = seq(real(0), intended_alpha, _RHORESOLUTION_)

    ! Loop through pi values, running simulation
    do i = 1,_RHORESOLUTION_
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
      pi = pin(i)
      pe = intended_alpha - pi

      ! Run simulation
      nact = 0
      ups = 0
      no_flips = 0
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
            if (spins(x,y) /= spins(nx,ny)) then
              no_flips = no_flips + 1
            end if
          spins(x,y) = spins(nx,ny)
        else
          if (spins(x,y) == 1) then
            if (randno2 <= pe) then
              ! egress event
              spins(x,y) = -1
                no_flips = no_flips + 1
            end if
          else
            if (randno2 <= pi) then
              ! ingress event
              spins(x,y) = 1
                no_flips = no_flips + 1
            end if
          end if
        end if
      end do

      ! Calculate rho, sigma and alpha
      sigma = real(nact)/real(2*mp_length*N*N)
      rho = real(ups)/real(mp_length*N*N)
      alpha = ((1-q)*(pi + pe))/q

      ! Writeout
      write(1,*) pi, pe, q, N, intended_alpha, no_flips
    end do
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



end program littlesq_countsflips

