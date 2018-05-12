SUBROUTINE FINDCENTRALDENSITY
USE DEFINITION
IMPLICIT NONE

centralrho = ((1.5E0_DP * dx) ** 2 * rho (1) - (5.0E-1_DP * dx) ** 2 * rho (2)) &
		/ ((1.5E0_DP * dx) ** 2 - (5.0E-1_DP * dx) ** 2)

END SUBROUTINE