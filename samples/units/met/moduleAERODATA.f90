MODULE AERODATA

  INTEGER, PARAMETER :: NASECT = 3  ! total number of size sections 
  ! over the simulated aerosol size range
  ! DEPENDS ON NO. OF SECTIONS 

  ! Maximum particle diameter simulated [micro meter]
  != unit um :: dpup
  REAL, PARAMETER :: DPUP = 10.0

  ! Minimum particle diameter simulated [micro meter]
  != unit um :: dplow
  REAL, PARAMETER :: DPLOW = 0.0215

  REAL VRAT    ! volume ratio of adjacent sections 

  REAL VRLOW   ! = (2.0 / (1.0 + VRAT) )**(1.0/3.0)  

  REAL VRHI    ! = VRLOW * VRAT**(1.0/3.0)   

  REAL DPBINMIN! initial center diameter of the first size section 
  ! [micro meter] 

  ! Particle density [g / cm**3] -- wrong: should be um**3
  ! For comparison between 2-bin and 8-bin, use the value of 1.352,
  ! so that the cut off size for PM2.5 is 2.15 um, consistent with 
  ! the 8-bin cut off size 
  != unit g / um**3 :: densp
  REAL, PARAMETER :: DENSP = 1.352

  ! Particle diameters (microns)
  != unit um :: dpctr
  REAL :: DPCTR( NASECT )

  ! PM Conc (ug/m3)
  != unit ug/m**3 :: pmconc
  REAL :: PMCONC( NASECT )

  ! Particle surface area (um2), volume (um3), mass (ug) and no. conc (#/m3)
  ! in each size section
  != unit um**2 :: surfp
  != unit um**3 :: vol
  != unit ug :: aeroma
  != unit 1 / m**3 :: xnum
  REAL :: SURFP, VOL, AEROMA, XNUM

  != unit m**2/m**3 :: area
  REAL :: AREA( NASECT )    ! Surface area of each section [m**2/m**3] 

END MODULE AERODATA
!.......................................................................
