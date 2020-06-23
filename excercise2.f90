module group_function
    implicit none
    contains
    
real function bod_analytical(distance)
real, intent(in) :: distance
real, parameter :: K_B = 0.5, U = 27500., Initial_bod = 18.18
  bod_analytical = Initial_bod*exp(-K_B/U*distance)
end function bod_analytical 

real function do_analytical(distance)
real, intent(in) :: distance
real, parameter :: K_B = 0.5, K_a = 1.0, U = 27500., DO_S = 10.0, Initial_bod = 18.18, Initial_do = 9.09
  do_analytical = DO_S - K_B/(K_a - K_B)*Initial_bod*(exp(-K_B/U*distance) - exp(-K_a/U*distance)) &
                  - (DO_S - Initial_do) * exp(-K_a/U*distance)
end function do_analytical

end module group_function


program code7
use group_function
    implicit none

    integer :: i, nsteps, j
    real :: h, l, k1B, k2B, k3B, k4B, k1D, k2D, k3D, k4D, error_bod, error_do, mae_bod, mae_do
    real, dimension(0:2001) :: conc_bod, conc_do, conc_bod_a, conc_do_a
    real, parameter :: K_B = 0.5, K_a = 1.0, U = 27500., DO_S = 10.0, Initial_bod = 18.18, initial_do = 9.09
  
    ! step size
    h = 0.1 ![km]
    nsteps =  int(200.0/h)
    l = 0.1*1000 ! unit conversion : km to m
  
    ! Initial conditions
    conc_bod(0) = Initial_bod
    conc_do(0) = initial_do
    conc_bod_a(0) = Initial_bod
    conc_do_a(0) = initial_do

    error_bod = 0.
    error_do = 0.

    do i=0, nsteps
      k1B = -K_B/U*conc_bod(i)
      k2B = -K_B/U*(conc_bod(i)+0.5*l*k1B)
      k3B = -K_B/U*(conc_bod(i)+0.5*l*k2B)
      k4B = -K_B/U*(conc_bod(i)+l*k3B)
  
      k1D = K1B + K_a/U*(DO_S - conc_do(i))
      k2D = k2B + K_a/U*(DO_S - (conc_do(i)+0.5*l*k1D))
      k3D = K3B + K_a/U*(DO_S - (conc_do(i)+0.5*l*k2D))
      k4D = K4B + K_a/U*(DO_S - (conc_do(i)+l*k3D))
      
      conc_bod(i+1) = conc_bod(i)+l/6.0*(k1B+2.0*k2B+2.0*k3B+k4B)
      conc_do(i+1) = conc_do(i)+l/6.0*(k1D+2.0*k2D+2.0*k3D+k4D)
      
      conc_bod_a(i+1) = bod_analytical (l*(i+1))
      conc_do_a(i+1) = do_analytical(l*(i+1))
      
      error_bod = error_bod + abs(conc_bod_a(i+1) - conc_bod(i+1))
      error_do = error_do + abs(conc_do_a(i+1) - conc_do(i+1))

    end do 

    mae_bod = error_bod / nsteps
    mae_do = error_do / nsteps
    print*, 'Mean absolute error for BOD = ', mae_bod
    print*, ' Mean absolute error for DO = ', mae_do
  
    open(unit=10, file="bod.csv")
    open(unit=11, file="do.csv")
  
    do j = 0, nsteps
      if (j == 0) then
        write(10, 100) j*h, conc_bod(0)
        write(11, 100) j*h, conc_do(0)
      else if (mod(j, 10) == 0) then
        write(10, 100) j*h, conc_bod(j)
        write(11, 100) j*h, conc_do(j)
      end if
    end do
  100 format(F6.2, ',', F6.2)
    
  end program code7