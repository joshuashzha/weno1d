SUBROUTINE GETVEL
USE DEFINITION
IMPLICIT NONE

INTEGER :: j

DO j = 1, length_step
   IF(rho(j)>=1.1*rho_a) THEN
      vel (j) = - ini_vel * dx * (DBLE(j)-0.5_dp) 
   ENDIF
END DO

CALL BOUNDARY1D (vel, odd)

END SUBROUTINE
