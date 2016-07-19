      PROGRAM ONE
        IMPLICIT NONE
        REAL  radius, height
        REAL  mass
        REAL  densit, volume, pi, i
        volume = pi * radius**2 * height
        mass = volume * densit

        do 10 i=1,10
           radius = i
 10     continue
      end
      function r(m,t,a)
        integer m,a(10)
        real t

        r = 0.1*t * (m**2 + 14*m + 46)
        if (r .LT. 0) r = 0.0

        return
      end
