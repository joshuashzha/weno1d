SUBROUTINE GETEPSILON
USE DEFINITION
IMPLICIT NONE

INTEGER :: j

DO j = -4, length_step + 5
	epsilon (j) = (k / (gamma - 1.0E0_DP)) * rho (j) ** (gamma - 1.0E0_DP)
END DO

END SUBROUTINE