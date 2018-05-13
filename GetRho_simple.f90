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

   do j=1,length_step
      if(j <= length_step*5/10) then
         rho(j) = 1.0_dp
         vel(j) = -2.0_dp
         p(j) = 0.4_dp
         epsilon(j) = p(j) / rho(j) / (gamma-1.0_dp)
      else
         rho(j) = 1.0_dp
         vel(j) = 2.0_dp
         p(j) = 0.4_dp
         epsilon(j) = p(j) / rho(j) / (gamma-1.0_dp)
      endif
   enddo
   call boundary1d(rho,even)
   call boundary1d(vel,even)
   call boundary1d(p,even)
   call boundary1d(epsilon,even)

elseif(test_model_no == 3) then
   
   do j=1,length_step
      if(j <= length_step*4/10) then
         rho(j) = 5.99924_dp
         vel(j) = 19.5975_dp
         p(j) = 460.894_dp
         epsilon(j) = p(j) / rho(j) / (gamma-1.0_dp)
      else
         rho(j) = 5.99242_dp
         vel(j) = -6.19633_dp
         p(j) = 46.0950_dp
         epsilon(j) = p(j) / rho(j) / (gamma-1.0_dp)
      endif
   enddo
   call boundary1d(rho,even)
   call boundary1d(vel,even)
   call boundary1d(p,even)
   call boundary1d(epsilon,even)

elseif(test_model_no == 4) then
   
   do j=1,length_step
      if(j <= length_step*8/10) then
         rho(j) = 1.0_dp
         vel(j) = -19.59745_dp
         p(j) = 1000.0_dp
         epsilon(j) = p(j) / rho(j) / (gamma-1.0_dp)
      else
         rho(j) = 1.0_dp
         vel(j) = -19.59745_dp
         p(j) = 0.01_dp
         epsilon(j) = p(j) / rho(j) / (gamma-1.0_dp)
      endif
   enddo
   call boundary1d(rho,even)
   call boundary1d(vel,even)
   call boundary1d(p,even)
   call boundary1d(epsilon,even)

else
   stop "no such test model"   
endif

endsubroutine
