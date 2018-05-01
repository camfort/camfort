program gravity
  != unit m**3 / s**2 / kg :: G
  real :: G = 6.67408E-11

  != unit kg :: mE, mS
  real :: mE = 5.972E24, mS = 1.989E30

  != unit m :: rSE
  real :: rSE = 1.496E11, rSE2 = rSE + 1e9

  != unit 1 :: pi
  real :: pi = 3.1415926

  != unit s / day :: sec_per_day
  real :: sec_per_day = 86400

  real :: vexh = 210000

  != unit m :: c, c2
  real :: f, v, c, d, deltaV, mfuel, d2, c2, v2

  ! gravitational force between Sun and Earth
  f = G * mE * (mS / sqr(rSE))

  ! velocity of Earth around Sun
  v = sqrt(G * mS / rSE)

  ! circumference of orbit (assuming circular)
  c = 2 * pi * rSE

  ! number of days for modelled orbital circuit
  d = c / v / sec_per_day

  ! total change in velocity required to transfer orbits
  deltaV = hohmann(G * mS, rSE, rSE + 1e9)

  ! amount of rocket fuel required
  mfuel = mE * (1 - exp(-deltaV / vexh))

  ! length of revised year
  c2 = 2 * pi * rSE2
  v2 = sqrt(G * mS / rSE2)
  d2 = c2 / v2 / sec_per_day
  
  print *, f, v, c, d
  print *, deltaV, mfuel, d2

contains
  != unit 'a**2 :: sqr
  real function sqr(x)
    != unit 'a :: x
    real :: x
    sqr = x * x
  end function sqr

  != unit m / s :: hohmann
  real function hohmann(mu, r1, r2)
    != unit m :: r1, r2
    real :: mu, r1, r2, dv1, dv2
    dv1 = sqrt(mu/r1) * (sqrt(2*r2 / (r1 + r2)) - 1)
    dv2 = sqrt(mu/r2) * (1 - sqrt(2*r1 / (r1 + r2)))
    hohmann = dv1 + dv2
  end function hohmann
end program gravity
