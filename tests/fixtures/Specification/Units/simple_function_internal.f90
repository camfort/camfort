module simple_function_internal

  contains

      function print_lightspeed()

          ! prints speed of light in a vacuum
          ! first in m/s, then in km/s

          integer :: print_lightspeed

          integer :: lightspeed
          real :: lightspeed_converted

          lightspeed = 299792458 ! m/s
          lightspeed_converted = lightspeed / 1000  ! km/s

          print *, lightspeed
          print *, lightspeed_converted

          print_lightspeed = 1  ! success

      end

  end