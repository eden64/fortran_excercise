program code7
    implicit none
    integer :: i, nsteps, j
    real :: h, l, k1B, k2B, k3B, k4B, k1D, k2D, k3D, k4D
    real, dimension(0:2001) :: conc_bod, conc_do
    real, parameter :: K_B = 0.5, K_a = 1.0, U = 27500., DO_S = 10.0
  
    ! step size
    h = 0.1 ![km]
    nsteps =  int(200.0/h)
    l = 0.1*1000 ! unit conversion : km to m
  
    ! Initial conditions
    conc_bod(0) = 18.18
    conc_do(0) = 9.09
  
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
    end do 
  
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