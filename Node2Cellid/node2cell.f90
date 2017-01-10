program node2cell
  use subs, only: findcell, findcellv, urword
  use LineListModule, only: LineListType
  use SimModule, only: store_error, ustop
  implicit none
  integer :: nlay1, nrow1, ncol1, nplay1, nodes1, node1, node2, numlay1, n, &
             numrow1, numcol1, nmodels, m1ndim, m2ndim, ncpl1, ncpl2
  integer :: nlay2, nrow2, ncol2, nplay2, nodes2, numlay2,  &
             numrow2, numcol2, icol, istart, istop, lent, &
             numalphaj, nodej, ij, kj, jj
  integer :: i1, i2, j1, j2, k1, k2, ipos1, nlines, i, iout
  double precision :: rdum
  character(len=1000) :: lineorig, linenew, templine, fname, ermsg
  type(LineListType) :: lineList
  ! formats
50 format(a1000)
60 format(a,2x,i0,1x,i0,1x,i0)
62 format(a,2x,i0,1x,i0)
70 format(a)
80 format(a,1x,i0,a,i0)
100 format(' Model ',i0,' contains ',i5,' cells in each layer',/, &
            ' and ',i6,' cells altogether'/)
  !
  ! Initialize some variables
  m1ndim = 0
  m2ndim = 0
  numalphaj = 0
  call lineList%InitializeLineList()
  !
  ! DEFINE MODEL GRID(S)
  !
  write(*,*)'Enter number of models (1 or 2):'
  read(*,*) nmodels
  if (nmodels < 1 .or. nmodels > 2) stop('Invalid number of models')
  write(*,*)'For model 1 enter number of dimensions in grid (DISV=2, DIS=3):'
  read(*,*) m1ndim
  if (nmodels == 2) then
    write(*,*)'For model 2 enter number of dimensions in grid (DISV=2, DIS=3):'
    read(*,*) m2ndim
  endif
  !
  ! Check ndim input
  if (m1ndim < 2 .or. m1ndim > 3) then
    stop('Invalid # dimensions for model 1')
  endif
  if (nmodels == 2 .and. (m2ndim < 2 .or. m2ndim > 3)) then
    stop('Invalid # dimensions for model 2')
  endif
  !
  select case (m1ndim)
  case (2)
    write(*,*)'For model 1, enter nlay, ncpl:'
    read(*,*)nlay1, ncpl1
    nplay1 = ncpl1
  case (3)
    write(*,*)'For model 1, enter nlay, nrow, ncol:'
    read(*,*)nlay1, nrow1, ncol1
    nplay1 = nrow1 * ncol1
  end select
  nodes1 = nplay1 * nlay1
  write(*,100)1, nplay1, nodes1
  if (nmodels > 1) then
    select case (m2ndim)
    case (2)
      write(*,*)'For model 2, enter nlay, ncpl:'
      read(*,*)nlay2, ncpl2
      nplay2 = ncpl2
    case (3)
      write(*,*)'For model 2, enter  nlay, nrow, ncol:'
      read(*,*)nlay2, nrow2, ncol2
      nplay2 = nrow2 * ncol2
    end select
    nodes2 = nplay2 * nlay2
    write(*,100)2, nplay2, nodes2
  endif
  !
  write(*,*)'For GNC file, enter NUMALPHAJ (for EXG, enter 0):'
  read(*,*)numalphaj
  if (numalphaj < 0) then
    write(*,*)'Error. NUMALPHA must be 0 or more. Stopping.'
    stop
  endif
  if (numalphaj > 0 .and. nmodels == 1) then
    ! GNC input will have a nodem entry even though there's only one model.
    ! In this case, nodem is for the model 1 grid.
    ! To handle this situation, just increment numalphaj by 1.
    numalphaj = numalphaj + 1
  endif
  !
  ! PROCESS NODE-BASED INPUT AND CONVERT NODE NUMBERS TO CELLIDS
  !
  write(*,*)'Enter text line(s). Enter negative number or 0 to end:'
  do
    ! Process one line
    read(*,50,end=300)lineorig
    ! See if first nonblank character is #; if so, just copy to line list and cycle
    templine = adjustl(lineorig)
    if (templine(1:1) == '#') then
      call lineList%AddLine(lineorig)
      cycle
    endif
    icol = 1
    ! Get first node number and write equivalent cellid to linenew
    call urword(lineorig,icol,istart,istop,2,node1,rdum,0,0)
    if (node1 < 1) exit
    if (node1 > nodes1) then
      write(ermsg,80)'Node1 exceeds Nodes1:',node1,' > ',nodes1
      call store_error(ermsg)
      call ustop()
    endif
    linenew = ''
    ! Write cellid of node in model 1
    select case (m1ndim)
    case (2)
      call findcellv(k1,i1,nlay1,ncpl1,node1)
      write(linenew,62)'',k1,i1
    case (3)
      call findcell(k1,i1,j1,nlay1,nrow1,ncol1,node1)
      write(linenew,60)'',k1,i1,j1
    end select
    ! If nmodels = 2, get second node number and concatenate cellid to linenew
    if (nmodels == 2) then
      ! Append cellid of node in model 2
      call urword(lineorig,icol,istart,istop,2,node2,rdum,0,0)
      if (node2 > nodes2) then
        write(ermsg,80)'Node2 exceeds Nodes2:',node2,' > ',nodes2
        call store_error(ermsg)
        call ustop()
      endif
      select case (m2ndim)
      case (2)
        call findcellv(k2,i2,nlay2,ncpl2,node2)
        write(linenew,62)trim(linenew),k2,i2
      case (3)
        call findcell(k2,i2,j2,nlay2,nrow2,ncol2,node2)
        write(linenew,60)trim(linenew),k2,i2,j2
      end select
    endif
    !
    ! Convert all NODESJ nodes to cellids (NUMALPHAJ of them).
    ! Each NODESJ node represents a contributing cell in model 1 (e.g. the 
    ! parent model in an LGR setup), so use model-1 nrow, ncol, nlay in call 
    ! to findcell.
    do i=1,numalphaj
      call urword(lineorig,icol,istart,istop,2,nodej,rdum,0,0)
      if (nodej > nodes1) then
        write(ermsg,80)'Nodej exceeds Nodes1:',nodej,' > ',nodes1
        call store_error(ermsg)
        call ustop()
      endif
      select case (m1ndim)
      case (2)
        call findcellv(kj,ij,nlay1,ncpl1,nodej)
        write(linenew,62)trim(linenew),kj,ij
      case (3)
        call findcell(kj,ij,jj,nlay1,nrow1,ncol1,nodej)
        write(linenew,60)trim(linenew),kj,ij,jj
      end select
    enddo
    !
    ipos1 = istop + 1
    lent = len_trim(lineorig)
    linenew = trim(linenew) // ' ' // lineorig(ipos1:lent)
    call lineList%AddLine(linenew)
  enddo
300 continue   
!
! Write contents of line list
iout =  88
fname = 'node2cell_output.txt'
open(unit=iout,file=fname,status='REPLACE')
nlines = lineList%CountLines()
do i=1,nlines
  templine = ''
  call lineList%GetLine(i,templine)
  write(iout,70)trim(templine)
enddo

close(iout)
write(*,*)'Output written to ', trim(fname)

stop

end program node2cell
