SUBROUTINE FROMRVETOU (u)
USE DEFINITION
IMPLICIT NONE

REAL (DP), INTENT (OUT), DIMENSION (1 : no_of_eq, -4 : length_step + 5) :: u
INTEGER :: j

DO j = -4, length_step + 5
	u (1, j) = rho (j)
	u (2, j) = rho (j) * vel (j)
	u (3, j) = rho (j) * epsilon (j) + 5.0E-1_DP * rho (j) * vel (j) ** 2
END DO

END SUBROUTINE