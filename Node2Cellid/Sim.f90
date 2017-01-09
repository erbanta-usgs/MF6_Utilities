module SimModule
  use ConstantsModule, only: MAXCHARLEN,LINELENGTH
  use SimVariablesModule, only: iout
  implicit none
  private
  public :: count_errors, iverbose, record_exchange, record_model,             &
            record_package, record_solution, sim_message, store_error, ustop,  &
            converge_reset, converge_check, final_message, store_warning,      &
            store_note, count_warnings, count_notes, store_error_unit
  integer, parameter :: MAXOBJ = 4
  integer :: iverbose=0 !0: print nothing
                        !1: print first level subroutine information
  character(len=MAXCHARLEN), allocatable, dimension(:) :: sim_errors
  character(len=MAXCHARLEN), allocatable, dimension(:) :: sim_warnings
  character(len=MAXCHARLEN), allocatable, dimension(:) :: sim_notes
  character(len=50), dimension(MAXOBJ) :: lastobjects = ''
  ! lastobjects(1) = last exchange
  ! lastobjects(2) = last model
  ! lastobjects(3) = last package
  ! lastobjects(4) = last solution
  character(len=10), dimension(MAXOBJ) :: types
  data types/'exchange  ','model     ','package   ','solution  '/
  !types(1) = 'exchange  '
  !types(2) = 'model     '
  !types(3) = 'package   '
  !types(4) = 'solution  '
  integer, dimension(MAXOBJ) :: iobjorder = 0

contains

subroutine reorder(id)
  implicit none
  ! -- dummy argument
  integer, intent(in) :: id
  ! -- local variables
  integer :: i, iposid
  ! -- If id is not the first item in the list, reorder the list.
  if (iobjorder(1) .ne. id) then
    ! -- Find position of id in list, which will determine which elements
    !    need to be reordered. Start by assuming it's at the end.
    iposid = MAXOBJ
    findpos: do i=1,MAXOBJ
      if (iobjorder(i) == id) then
        iposid = i
        exit findpos
      endif
    enddo findpos
    ! -- Within the range 1,iposid-1, push down all the items that are not id.
    !    Overwrite an item that is id.
    do i=iposid-1,1,-1
      if (iobjorder(i) .ne. id) then
        iobjorder(i+1) = iobjorder(i)
      endif
    enddo
    iobjorder(1) = id
  endif
  return
end subroutine reorder

subroutine report_objects()
  ! **************************************************************************
  ! Report the last-accessed objects of each type, in order
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- local variables
  integer :: i
  ! -- formats
10 format('Last-accessed ',a,': ',a)
  !
  do i=MAXOBJ,1,-1
    if (iobjorder(i) > 0 .and. iobjorder(i) <= MAXOBJ) then
      write(iout,10)trim(types(iobjorder(i))),trim(lastobjects(iobjorder(i)))
      write(*,10)trim(types(iobjorder(i))),trim(lastobjects(iobjorder(i)))
    endif
  enddo
  return
end subroutine report_objects

subroutine record_exchange(exchangename)
  ! **************************************************************************
  ! Record the name of the last-accessed exchange
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- dummy argument
  character(len=*), intent(in) :: exchangename
  !
  if (exchangename .ne. ' ') then
    lastobjects(1) = exchangename
    call reorder(1)
  endif
  return
end subroutine record_exchange

subroutine record_model(modelname)
  ! **************************************************************************
  ! Record the name of the last-accessed model
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- dummy argument
  character(len=*), intent(in) :: modelname
  !
  if (modelname .ne. ' ') then
    lastobjects(2) = modelname
    call reorder(2)
  endif
  return
end subroutine record_model

subroutine record_package(packagename)
  ! **************************************************************************
  ! Record the name of the last-accessed package
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- dummy argument
  character(len=*), intent(in) :: packagename
  !
  if (packagename .ne. ' ') then
    lastobjects(3) = packagename
    call reorder(3)
  endif
  return
end subroutine record_package

subroutine record_solution(solutionname)
  ! **************************************************************************
  ! Record the name of the last-accessed solution
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- dummy argument
  character(len=*), intent(in) :: solutionname
  !
  if (solutionname .ne. ' ') then
    lastobjects(4) = solutionname
    call reorder(4)
  endif
  return
end subroutine record_solution

integer function count_errors()
  implicit none
  if (allocated(sim_errors)) then
    count_errors = size(sim_errors)
  else
    count_errors = 0
  endif
  return
end function count_errors

integer function count_warnings()
  implicit none
  if (allocated(sim_warnings)) then
    count_warnings = size(sim_warnings)
  else
    count_warnings = 0
  endif
  return
end function count_warnings

integer function count_notes()
  implicit none
  if (allocated(sim_notes)) then
    count_notes = size(sim_notes)
  else
    count_notes = 0
  endif
  return
end function count_notes

subroutine store_error(errmsg)
  ! **************************************************************************
  ! Store an error message for printing at end of simulation
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  use ArrayHandlersModule, only: ExpandArray
  implicit none
  ! -- dummy arguments
  character(len=*), intent(in) :: errmsg
  ! -- local variables
  integer :: i
  !
  call ExpandArray(sim_errors)
  i = size(sim_errors)
  sim_errors(i) = errmsg
  !
  return
end subroutine store_error

subroutine store_error_unit(iunit)
  ! **************************************************************************
  ! Convert iunit to file name and indicate error reading from this file
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- dummy arguments
  integer, intent(in) :: iunit
  ! -- local variables
  character(len=LINELENGTH) :: fname
  !
  inquire(unit=iunit, name=fname)
  call store_error('ERROR OCCURRED WHILE READING FILE: ')
  call store_error(trim(adjustl(fname)))
  !
  return
end subroutine store_error_unit

subroutine store_warning(warnmsg)
  ! **************************************************************************
  ! Store a warning message for printing at end of simulation
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  use ArrayHandlersModule, only: ExpandArray
  implicit none
  ! -- dummy arguments
  character(len=*), intent(in) :: warnmsg
  ! -- local variables
  integer :: i
  !
  call ExpandArray(sim_warnings)
  i = size(sim_warnings)
  sim_warnings(i) = warnmsg
  !
  return
end subroutine store_warning

subroutine store_note(note)
  ! **************************************************************************
  ! Store a note for printing at end of simulation
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  use ArrayHandlersModule, only: ExpandArray
  implicit none
  ! -- dummy arguments
  character(len=*), intent(in) :: note
  ! -- local variables
  integer :: i
  !
  call ExpandArray(sim_notes)
  i = size(sim_notes)
  sim_notes(i) = note
  !
  return
end subroutine store_note

logical function print_errors()
  ! **************************************************************************
  ! Print all error messages that have been stored
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- local variables
  integer :: i, isize
  ! -- formats
10 format(/,'ERROR REPORT:',/)
  !
  print_errors = .false.
  if (allocated(sim_errors)) then
    isize = size(sim_errors)
    if (isize>0) then
      print_errors = .true.
      if (iout /= 0) write(iout,10)
      write(*,10)
      call report_objects()
      do i=1,isize
        call write_message(sim_errors(i))
        if (iout /= 0) then
          call write_message(sim_errors(i),iout)
        endif
      enddo
    endif
  endif
  !
  return
end function print_errors

subroutine print_warnings()
  ! **************************************************************************
  ! Print all warning messages that have been stored
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- local variables
  integer :: i, isize
  ! -- formats
10 format(/,'WARNINGS:',/)
  !
  if (allocated(sim_warnings)) then
    isize = size(sim_warnings)
    if (isize>0) then
      if (iout>0) write(iout,10)
      write(*,10)
      do i=1,isize
        call write_message(sim_warnings(i))
        if (iout>0) call write_message(sim_warnings(i),iout)
      enddo
    endif
    write(*,'()')
    if (iout>0) write(iout,'()')
  endif
  !
  return
end subroutine print_warnings

subroutine print_notes()
  ! **************************************************************************
  ! Print all note that have been stored
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- local variables
  integer :: i, isize
  character(len=MAXCHARLEN+10) :: noteplus
  ! -- formats
10 format(/,'NOTES:')
20 format(i0,'. ',a)
  !
  if (allocated(sim_notes)) then
    isize = size(sim_notes)
    if (isize>0) then
      if (iout>0) write(iout,10)
      write(*,10)
      do i=1,isize
        write(noteplus,20)i,trim(sim_notes(i))
        call write_message(noteplus)
        if (iout>0) call write_message(noteplus,iout)
      enddo
    endif
  endif
  !
  return
end subroutine print_notes

subroutine write_message(message,iunit,error,leadspace,endspace)
  ! -- Subroutine write_message formats and writes a message.
  !
  ! -- Arguments are as follows:
  !       MESSAGE      : message to be written
  !       IUNIT        : the unit number to which the message is written
  !       ERROR        : if true precede message with "Error"
  !       LEADSPACE    : if true precede message with blank line
  !       ENDSPACE     : if true follow message by blank line
  !
  use, intrinsic :: iso_fortran_env, only: output_unit
  implicit none
  ! -- dummy arguments
  character (len=*), intent(in) :: message
  integer,           intent(in), optional :: iunit
  logical,           intent(in), optional :: error
  logical,           intent(in), optional :: leadspace
  logical,           intent(in), optional :: endspace
  ! -- local variables
  integer                   :: jend, i, nblc, junit, leadblank
  integer                   :: itake, j
  character(len=20)         :: ablank
  character(len=MAXCHARLEN) :: amessage
  !
  amessage = message
  if (amessage==' ') return
  if (amessage(1:1).ne.' ') amessage=' ' // trim(amessage)
  ablank=' '
  itake=0
  j=0
  if(present(iunit))then
    junit=iunit
  else
    junit=output_unit
  end if
  if(present(leadspace))then
    if(leadspace) then
      if (junit>0) then
        write(junit,*)
      else
        write(*,*)
      endif
    endif
  endif
  if(present(error))then
    if(error)then
      nblc=len_trim(amessage)
      amessage=adjustr(amessage(1:nblc+8))
      if(nblc+8.lt.len(amessage)) amessage(nblc+9:)=' '
      amessage(1:8)=' Error: '
    end if
  end if
  !
  do i=1,20
    if(amessage(i:i).ne.' ')exit
  end do
  leadblank=i-1
  nblc=len_trim(amessage)
  !
5 continue
  jend=j+78-itake
  if(jend.ge.nblc) go to 100
  do i=jend,j+1,-1
    if(amessage(i:i).eq.' ') then
      if(itake.eq.0) then
        if (junit>0) then
          write(junit,'(a)',err=200) amessage(j+1:i)
        else
          write(*,'(a)',err=200) amessage(j+1:i)
        endif
        itake=2+leadblank
      else
        if (junit>0) then
          write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:i)
        else
          write(*,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:i)
        endif
      end if
      j=i
      go to 5
    end if
  end do
  if(itake.eq.0)then
    if (junit>0) then
      write(junit,'(a)',err=200) amessage(j+1:jend)
    else
      write(*,'(a)',err=200) amessage(j+1:jend)
    endif
    itake=2+leadblank
  else
    if (junit>0) then
      write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
    else
      write(*,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
    endif
  end if
  j=jend
  go to 5
  !
100 continue
  jend=nblc
  if(itake.eq.0)then
    if (junit>0) then
      write(junit,'(a)',err=200) amessage(j+1:jend)
    else
      write(*,'(a)',err=200) amessage(j+1:jend)
    endif
  else
    if (junit>0) then
      write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
    else
      write(*,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
    endif
  end if
  !
  if(present(endspace))then
    if(endspace) then
      if (junit>0) then
        write(junit,*)
      else
        write(*,*)
      endif
    endif
  end if
  return
  !
200 continue
  call ustop()
  !
end subroutine write_message

subroutine sim_message(iv,message)
! -- iv is the verbosity level of this message
! --  (1) means primary subroutine for simulation, exchange, model,
! --      solution, package, etc.
! -- message is a character string message to write
  implicit none
  integer,intent(in) :: iv
  character(len=*),intent(in) :: message
  if(iv<=iverbose) then
    write(iout,'(a)') message
  endif
end subroutine sim_message

subroutine ustop(stopmess,ioutlocal)
  ! **************************************************************************
  ! Stop program, with option to print message before stopping.
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  ! -- dummy arguments
  implicit none
  character, optional, intent(in) :: stopmess*(*)
  integer,   optional, intent(in) :: ioutlocal
  ! -- local variables
  character(len=*), parameter :: fmt = '(1x,a)'
  character(len=*), parameter :: msg = 'Stopping due to error(s)'
  logical :: errorfound
  !---------------------------------------------------------------------------
  call print_notes()
  call print_warnings()
  errorfound = print_errors()
  if (present(stopmess)) then
    if (stopmess.ne.' ') then
      write(*,fmt) stopmess
      if (iout > 0) write(iout,fmt) stopmess
      if (present(ioutlocal)) then
        if (ioutlocal .ne. iout .and. ioutlocal > 0) then
          write(ioutlocal,fmt) trim(stopmess)
          close(ioutlocal)
        endif
      endif
    endif
  endif
  !
  if (errorfound) then
    write(*,fmt) msg
    if (iout > 0) write(iout,fmt) msg
    if (present(ioutlocal)) then
      write(iout,fmt) msg
    endif
  endif
  !
  close(iout)
  stop
end subroutine ustop

  subroutine converge_reset()
! ******************************************************************************
! converge_reset
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SimVariablesModule, only: isimcnvg
! ------------------------------------------------------------------------------
    !
    isimcnvg = 1
    !
    ! -- Return
    return
  end subroutine converge_reset

  subroutine converge_check(exit_tsloop)
! ******************************************************************************
! converge_check
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: isimcnvg, numnoconverge, isimcontinue
    ! -- dummy
    logical, intent(inout) :: exit_tsloop
    ! -- format
    character(len=*), parameter :: fmtfail =                                   &
      "(1x, 'Simulation convergence failure.',                                 &
       ' Simulation will terminate after output and deallocation.')"
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    exit_tsloop = .false.
    !
    ! -- Count number of failures
    if(isimcnvg == 0) numnoconverge = numnoconverge + 1
    !
    ! -- Continue if 'CONTINUE' specified in simulation control file
    if(isimcontinue == 1) then
      if(isimcnvg == 0) then
        isimcnvg = 1
      endif
    endif
    !
    ! --
    if(isimcnvg == 0) then
      write(iout, fmtfail)
      exit_tsloop = .true.
    endif
    !
    ! -- Return
    return
  end subroutine converge_check

  subroutine final_message()
! ******************************************************************************
! converge_message
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: isimcnvg, numnoconverge
    ! -- formats
    character(len=*), parameter :: fmtnocnvg =                                 &
      "(1x, 'Simulation convergence failure occurred ', i0, ' time(s).')"
! ------------------------------------------------------------------------------
    !
    ! -- Write message if any nonconvergence
    if(numnoconverge > 0) then
      write(*, fmtnocnvg) numnoconverge
      write(iout, fmtnocnvg) numnoconverge
    endif
    !
    if(isimcnvg == 0) then
      call ustop('Premature termination of simulation.', iout)
    else
      call ustop('Normal termination of simulation.', iout)
    endif
    !
    ! -- Return
    return
  end subroutine final_message

end module SimModule
