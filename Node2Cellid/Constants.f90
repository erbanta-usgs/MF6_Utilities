module ConstantsModule
  public
  integer, parameter :: IUSERFORMATSTRIP = -99
  integer, parameter :: IUSERFORMATWRAP = 99
  integer, parameter :: LENBIGLINE = 5000
  integer, parameter :: LENHUGELINE = 50000
  integer, parameter :: LENMODELNAME = 20
  integer, parameter :: LENSOLUTIONNAME = 20
  integer, parameter :: LENAUXNAME = 16
  integer, parameter :: LENBOUNDNAME = 40
  integer, parameter :: LENPACKAGENAME = 16
  integer, parameter :: LENPACKAGETYPE = 7
  integer, parameter :: LENORIGIN = LENMODELNAME + LENPACKAGENAME
  integer, parameter :: LENFTYPE = 5
  integer, parameter :: LENOBSNAME = 40
  integer, parameter :: LENOBSTYPE = 20
  integer, parameter :: LENSFRNAME = 16
  integer, parameter :: LENTIMESERIESNAME = 24
  integer, parameter :: LENTIMESERIESTEXT = 12
  integer, parameter :: LINELENGTH = 300
  integer, parameter :: MAXCHARLEN = 1000
  integer, parameter :: MAXOBSTYPES = 100
  integer, parameter :: NAMEDBOUNDFLAG = -9
  integer, parameter :: IZERO = 0

  double precision, parameter :: DZERO = 0.0D0
  double precision, parameter :: DONETHIRD = 1.0D0 / 3.0D0
  double precision, parameter :: DHALF = 0.5D0
  double precision, parameter :: DP6 = 0.6D0
  double precision, parameter :: DTWOTHIRDS = 2.0D0 / 3.0D0
  double precision, parameter :: DP7 = 0.7D0
  double precision, parameter :: DP9 = 0.9D0
  double precision, parameter :: DP99 = 0.99D0
  double precision, parameter :: DP999 = 0.999D0

  double precision, parameter :: DONE = 1.0D0
  double precision, parameter :: D1P1 = 1.1D0
  double precision, parameter :: DFIVETHIRDS = 5.0D0 / 3.0D0
  double precision, parameter :: DTWO = 2.0D0
  double precision, parameter :: DTHREE = 3.0D0
  double precision, parameter :: DFOUR = 4.0D0
  double precision, parameter :: DSIX = 6.0D0
  double precision, parameter :: DEIGHT = 8.0D0
  double precision, parameter :: DTEN = 1.0D1

  double precision, parameter :: DEP6 = 1.0D6
  double precision, parameter :: DEP20 = 1.0D20


  double precision, parameter :: DPREC = EPSILON(1.0D0)
  double precision, parameter :: DEM1  = 1.0D-1
  double precision, parameter :: D5EM2 = 5.0D-2
  double precision, parameter :: DEM2  = 1.0D-2
  double precision, parameter :: DEM3  = 1.0D-3
  double precision, parameter :: DEM4  = 1.0D-4
  double precision, parameter :: DEM5  = 1.0D-5
  double precision, parameter :: DEM6  = 1.0D-6
  double precision, parameter :: DEM7  = 1.0D-7
  double precision, parameter :: DEM8  = 1.0D-8
  double precision, parameter :: DEM9  = 1.0D-9
  double precision, parameter :: DEM14 = 1.0D-14
  double precision, parameter :: DEM15 = 1.0D-15
  double precision, parameter :: DEM20 = 1.0D-20
  double precision, parameter :: DEM30 = 1.0D-30

  double precision, parameter :: DLNLOW = 0.995d0
  double precision, parameter :: DLNHIGH = 1.005d0

  double precision, parameter :: DPI = DFOUR * ATAN(DONE)
  double precision, parameter :: DTWOPI = DTWO * DFOUR * ATAN(DONE)

  double precision, parameter :: DGRAVITY = 9.80665D0
  double precision, parameter :: DNODATA = 3.0d30
  
  character(len=10), dimension(3, 3), parameter :: cidxnames = reshape (       &
    [ '      NODE', '          ', '          ',                                &
      '     LAYER', '    CELL2D', '          ',                                &
      '     LAYER', '       ROW', '       COL'], [3,3])

  ! -- Enumerators used with TimeSeriesType
  ENUM, BIND(C)
    ! Sets UNDEFINED=0, STEPWISE=1, LINEAR=2, LINEAREND=3
    ENUMERATOR :: UNDEFINED, STEPWISE, LINEAR, LINEAREND
  END ENUM

end module ConstantsModule
