module simple_function

contains

    function get_lightspeed()

        ! get speed of light in a vacuum

        integer :: get_lightspeed

        integer :: lightspeed = 299792458.0 ! m/s

        get_lightspeed = lightspeed

    end

end
