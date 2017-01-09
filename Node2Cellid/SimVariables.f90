module SimVariablesModule
  public
  integer :: iout              ! -- unit number for simulation output
  integer :: isimcnvg          ! -- 1 if all objects have converged, 0 otherwise
  integer :: isimcontinue = 0  ! -- 1 to continue if isimcnvg = 0, 0 to terminate
  integer :: isimcheck = 1     ! -- 1 to check input, 0 to ignore checks
  integer :: numnoconverge = 0 ! -- number of times there were convergence problems
end module SimVariablesModule
