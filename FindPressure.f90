SUBROUTINE FINDPRESSURE
USE DEFINITION
IMPLICIT NONE

INTEGER :: j

DO j = -4, length_step + 5
   p (j) = (gamma-1.0_dp) * rho (j) * epsilon(j)
   dpdrho (j) = (gamma-1.0_dp) * epsilon(j)
   dpdepsilon (j) = (gamma-1.0_dp) * rho (j)
END DO

END SUBROUTINE
