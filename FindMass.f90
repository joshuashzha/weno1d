SUBROUTINE FINDMASS
USE DEFINITION
IMPLICIT NONE

INTEGER :: j

mass = 0.0E0_DP

IF (sp_dim_i == 2) THEN
	mass = (4.0E0_DP / 3.0E0_DP) * pi * ((DBLE (1) - 5.0E-1_DP) * dx) ** 3 * rho (1)
	DO j = 2, length_step
		mass = mass + 4.0E0_DP * pi * dx * ((DBLE (j) - 5.0E-1_DP) * dx) ** 2 * rho (j)
	END DO
END IF

END SUBROUTINE