module readers
  implicit none  
  private
  public :: readsngl, readcont, lenobsnamebsv
  integer :: lenobsnamebsv
  
contains
  
subroutine readsngl(iin,iout,iprecision,ierr)
  ! Read single observations
  !   real32 specifies 32-bit real = 4 bytes = single precision.
  !   real64 specifies 64-bit real = 8 bytes = double precision.
  use iso_fortran_env, only: real32, real64
  implicit none
  ! -- dummy arguments
  integer, intent(in) :: iin,iout,iprecision
  integer, intent(out) :: ierr
  ! -- local variables
  integer, parameter :: OBSNAMEMAXSIZE=100
  integer :: k, kerr
  character(len=OBSNAMEMAXSIZE) :: obsname
  real(real32) :: val1
  real(real64) :: val2
  logical :: eof
  ! -- formats
10 format(a,',',g14.7)
20 format(a,',',g22.15)   
30 format('Simulated values for ',i0,' single observations were processed.' )  
  !
  ierr = 0
  k = 0
  do
    ! -- Read observation name
    !read(iin,end=1000,err=900)obsname
    call read_observation_name(iin, obsname, eof, kerr)
    if (eof) goto 1000
    if (kerr /= 0) goto 900
    !
    ! -- read simulated value and write observation name and simulated value to iout
    if (iprecision==1) then
      read(iin,end=900,err=900)val1
      write(iout,10)trim(obsname),val1
    elseif (iprecision==2) then
      read(iin,end=900,err=900)val2
      write(iout,20)trim(obsname),val2
    endif
    k = k + 1
  enddo
  !
  ! -- error
900 continue
  ierr = 1
  return
  !
  ! -- normal return
1000 continue
  write(*,30)k
  return
end subroutine readsngl
  
subroutine readcont(iin,iout,iprecision,ierr)
  ! Read continuous observations
  !   real32 specifies 32-bit real = 4 bytes = single precision.
  !   real64 specifies 64-bit real = 8 bytes = double precision.
  use iso_fortran_env, only: int32, real32, real64
  implicit none
  ! -- dummy arguments
  integer, intent(in) :: iin,iout,iprecision
  integer, intent(out) :: ierr
  ! -- local variables
  integer, parameter :: OBSNAMEMAXSIZE = 100
  integer(int32) :: nobs
  integer :: i, k, kerr
  character(len=1) :: comma = ','
  character(len=OBSNAMEMAXSIZE) :: obsname
  character(len=100000) :: header
  real(real32), allocatable, dimension(:) :: vals1
  real(real64), allocatable, dimension(:) :: vals2
  real(real32) :: totimr
  real(real64) :: totimd
  logical :: eof
  !-- formats
10 format(g14.7,200(:',',g14.7))  
20 format(g22.15,200(:',',g22.15))  
30 format('Simulated values for ',i0,' time steps of ', &
          i0,' continuous observations were processed.' )  
  !
  header = ''
  ierr = 0
  !
  ! -- read number of observations (per time step) and allocate arrays
  read(iin,err=900)nobs
  if (iprecision==1) then
    allocate(vals1(nobs))
  elseif (iprecision==2) then
    allocate(vals2(nobs))
  endif
  !
  ! -- build and write the header
  header = 'time'
  ! -- read the observation names
  do i=1,nobs
    !read(iin,end=900,err=900)obsname
    call read_observation_name(iin, obsname, eof, kerr)
    if (eof) goto 900
    if (kerr /= 0) goto 900
    header = trim(header)//comma//obsname
  enddo
  write(iout,'(a)')trim(header)
  !
  ! -- for each time step, read totim and the simulated values, then write them
  k = 0
  if (iprecision==1) then
    do
      read(iin,end=1000,err=900)totimr
      read(iin,end=900,err=900)(vals1(i),i=1,nobs)
      write(iout,10)totimr,(vals1(i),i=1,nobs)
      k = k + 1
    enddo
  elseif (iprecision==2) then
    do
      read(iin,end=1000,err=900)totimd
      read(iin,end=900,err=900)(vals2(i),i=1,nobs)
      write(iout,20)totimd,(vals2(i),i=1,nobs)
      k = k + 1
    enddo
  endif
  !
  ! -- error
900 continue
  ierr = 1
  return
  !
  ! -- normal return
1000 continue
  write(*,30)k,nobs
  return
end subroutine readcont
  
subroutine read_observation_name(iu, obsname, eof, ierr)
  ! dummy
  integer, intent(in) :: iu
  character(len=*), intent(inout) :: obsname
  logical, intent(inout) :: eof
  integer, intent(inout) :: ierr
  ! local
  integer :: i, istat
  character(len=1) :: ch
  !
  eof = .false.
  ierr = 0
  obsname = ''
  do i=1,lenobsnamebsv
    read(iu,end=99,iostat=istat)ch
    if (istat /= 0) then
      ierr = istat
      goto 99
    endif
    obsname(i:i) = ch
  enddo
  !
  return
99 continue
  eof = .true.
  return
end subroutine read_observation_name

end module readers
  