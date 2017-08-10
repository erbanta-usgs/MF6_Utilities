module readers
  use ConstantsModule, only: LENOBSNAME
  implicit none
  private
  public :: readsngl, readcont, read_header, obstype, convert, filname

  integer :: iprecision, lenobsnamebsv, nobs
  character(len=100) :: text1 = '', word
  character(len=200) :: filname = ''
  !character(len=100), allocatable, dimension(:) :: obsnames
  character(len=4) :: obstype, cvar
  logical :: convert

contains

  subroutine read_header(iin, iout)
    ! dummy
    integer, intent(in) :: iin, iout
    ! local
    integer :: i, ierr, istat, k, kerr
    logical :: eof
    real :: val1
    double precision :: val2
    character(len=LENOBSNAME) :: obsname
    ! formats
    10 format(/,'File "',a,'" summary:',/)
    20 format('File contains SINGLE observations')
    21 format('File contains CONTINUOUS observations')
    31 format('Precision is SINGLE')
    32 format('Precision is DOUBLE')
    40 format('Observation names:')
    41 format(2x,a)
    50 format(/,'Number of observations: ',i0)
    !
    k = 0
    nobs = 0
    ! -- read the first 100 bytes and get observation type and precision
    read(iin,err=900,end=900)text1
    !
    ! OBSTYPE (either 'sngl' or 'cont') is in bytes 1-4
    obstype = text1(1:4)
    if (obstype .ne. 'sngl' .and. obstype .ne. 'cont') then
      write(*,*)'Error: invalid observation type: "' // obstype // '"'
      goto 900
    endif
    !
    ! Precision indicator (either 'single' or 'double') is in bytes 6-11
    word = text1(6:11)
    if (word=='single') then
      iprecision = 1
    elseif (word=='double') then
      iprecision = 2
    else
      write(*,*)'Error: invalid precision: "' // trim(word) // '"'
      goto 900
    endif
    !
    ! Get LENOBSNAME from bytes 12-15 (no longer allow field to be blank)
    cvar = text1(12:15)
    if (cvar == '') then
      write(*,*)'Error: LENOBSNAME not found in BSV header.'
    else
      read(cvar,'(i4)',iostat=istat)lenobsnamebsv
      if (istat /= 0) then
        write(*,*)'Error reading LENOBSNAME from header.'
        goto 900
      elseif (lenobsnamebsv < 1) then
        write(*,*)'Error: LENOBSNAME value read from header is invalid.'
        goto 900
      endif
    endif
    !
    if (.not. convert) then
      write(*,10)trim(filname)
      ! Summarize contents, including observation names
      select case (obstype)
      case ('sngl')
        write(*,20)
      case ('cont')
        write(*,21)
      end select
      !
      select case (iprecision)
      case (1)
        write(*,31)
      case (2)
        write(*,32)
      end select
      !
      write(*,40)
      if (obstype=='sngl') then
        ierr = 0
        k = 0
        kerr = 0
        do
          ! -- Read observation name
          call read_observation_name(iin, obsname, eof, kerr)
          if (eof) goto 1000
          if (kerr /= 0) goto 900
          write(*,41)trim(obsname)
          !
          ! -- read simulated value
          if (iprecision==1) then
            read(iin,end=900,err=900)val1
          elseif (iprecision==2) then
            read(iin,end=900,err=900)val2
          endif
          k = k + 1
        enddo
        nobs = k
        !
      elseif (obstype=='cont') then
        ierr = 0
        !
        ! -- read number of observations (per time step) and allocate arrays
        read(iin,err=900)nobs
        !
        ! -- read the observation names
        do i=1,nobs
          call read_observation_name(iin, obsname, eof, kerr)
          if (eof) goto 900
          if (kerr /= 0) goto 900
          write(*,41)trim(obsname)
        enddo
      endif
    endif
    !
    goto 1000
  900 continue
    write(*,*)'Error encountered.'
    write(*,*)''
    stop
  1000 continue
    !close(iin)
    !close(iout)
    if (.not. convert) then
      write(*,50) nobs
    endif
    return
  end subroutine read_header

  subroutine readsngl(iin,iout,ierr)
    ! Read single observations
    !   real32 specifies 32-bit real = 4 bytes = single precision.
    !   real64 specifies 64-bit real = 8 bytes = double precision.
    use iso_fortran_env, only: real32, real64
    implicit none
    ! -- dummy arguments
    integer, intent(in) :: iin,iout
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
    kerr = 0
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

  subroutine readcont(iin,iout,ierr)
    ! Read continuous observations
    !   real32 specifies 32-bit real = 4 bytes = single precision.
    !   real64 specifies 64-bit real = 8 bytes = double precision.
    use iso_fortran_env, only: int32, real32, real64
    implicit none
    ! -- dummy arguments
    integer, intent(in) :: iin,iout
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
