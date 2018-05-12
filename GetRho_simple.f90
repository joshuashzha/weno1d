subroutine getrho_simple
use definition
implicit none

integer :: j

if(test_model_no == 1) then
  
   do j=1,length_step
      if(j <= length_step*3/10) then
         rho(j) = 1.0_dp
         vel(j) = 0.75_dp
         p(j) = 1.0_dp
         epsilon(j) = p(j) / rho(j) / (gamma-1.0_dp)
      else
         rho(j) = 0.125_dp
         vel(j) = 0.0_dp
         p(j) = 0.1_dp
         epsilon(j) = p(j) / rho(j) / (gamma-1.0_dp)
      endif
   enddo
   call boundary1d(rho,even)
   call boundary1d(vel,even)
   call boundary1d(p,even)
   call boundary1d(epsilon,even)

elseif(test_model_no == 2) then

endif

endsubroutine
