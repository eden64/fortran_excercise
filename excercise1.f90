program excercise1
    implicit none
    real  :: DO_conc,T !Please avoid using parenthes in declaration. I deleted them.
    integer::i
     do i= 5,30,5
       T=real(i) ! Same as the above comment
       DO_conc= Dissolved_Oxygen(T) ! Two words (i.e., Dissolved and oxygen) must be connected with such as '-' or '_'. Remove parenthes
        write(*,30) DO_conc 
       30 format(F6.0)
     end do
     contains
     real function dissolved_oxygen(temperature) !This name must match with the name above (Dissolved_Oxygen) 
     real, intent (in) :: T  ! Please avoid using parenthes. You can simply set T
    dissolved_oxyge=14.652-0.41022*T+0.00799*T**2-0.000077774*T**3
    end function dissolved_oxygen


end program excercise1
