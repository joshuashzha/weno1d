subroutine FindDt
use definition
implicit none

integer :: j
real(dp) :: lambda_loc, lambda

lambda = 0.0_dp
do j=1, length_step
   lambda_loc = abs(vel(j)) + sqrt(dpdrho(j)+dpdepsilon(j)*p(j)/rho(j)**2)
   lambda = max(lambda,lambda_loc)
enddo
dt = cfl * dx / lambda

end subroutine
