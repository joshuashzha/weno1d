SUBROUTINE FROMUTORVE (u)
USE DEFINITION
IMPLICIT NONE

REAL (DP), INTENT (IN), DIMENSION (1 : no_of_eq, -4 : length_step + 5) :: u
INTEGER :: j

DO j = -4, length_step + 5
	rho (j) = u (1, j)
	vel (j) = u (2, j) / rho (j)
	epsilon (j) = u (3, j) / rho (j) - 5.0E-1_DP * vel (j) ** 2
END DO

END SUBROUTINE