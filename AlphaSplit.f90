SUBROUTINE ALPHASPLIT (alpha)
USE DEFINITION
IMPLICIT NONE

INTEGER :: i, j
REAL (DP), INTENT (OUT) :: alpha
REAL (DP) :: lambda_loc, lambda

lambda = 0.0_dp

do j=1, length_step
   lambda_loc = abs(vel(j)) + sqrt(dpdrho(j)+dpdepsilon(j)*p(j)/rho(j)**2)
   lambda = max(lambda,lambda_loc)
enddo

alpha = lambda

END SUBROUTINE
