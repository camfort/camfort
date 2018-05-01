program gravity
  real :: G = 6.67408E-11

  real :: mE = 5.972E24, mS = 1.989E30

  real :: rSE = 1.496E11

  real :: pi = 3.1415926

  real :: sec_per_day = 86400

  real :: f, v, c, d

  ! gravitational force between Sun and Earth
  f = G * mE * (mS / sqr(rSE))

  ! velocity of Earth around Sun
  v = sqrt(G * mS / rSE)

  ! circumference of orbit (assuming circular)
  c = 2 * pi * rSE

  ! number of days for modelled orbital circuit
  d = c / v / sec_per_day

  print *, f, v, c, d

contains
  real function sqr(x)
    real :: x
    sqr = x * x
  end function sqr
end program gravity
