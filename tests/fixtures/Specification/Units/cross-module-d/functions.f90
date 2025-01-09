module functions

  implicit none

contains

  function convert(length_m) result(length_km)
      
      use constants

      real :: length_m
      real :: length_km
      
      ! convert using constant from sub-module
      length_km = length_m * km_per_m

  end
  
end