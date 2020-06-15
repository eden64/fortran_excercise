program excercise1
    implicit none
    real  :: DO_conc,T(degree_c)
    integer::i
     do i= 5,30,5
       T(degree_c)=real(i)
       DO_conc= Dissolved Oxygen(mg/l)
        write(*,30)Dissolved oxygen 
       30 format(F6.0)
     end do
     contains
     real function calculate dissolved oxygen 
     real, intent (in) :: T(degree_c)
    dissolved oxygen(mg/l)=14.652-0.41022*T(degree_c)+0.00799*T^2-0.000077774*T^3
    end function calculate dissolved oxygen


end program excercise1