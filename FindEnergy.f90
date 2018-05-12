SUBROUTINE FINDENERGY
USE DEFINITION
IMPLICIT NONE

INTEGER :: j

energy = 0.0E0_DP

IF (sp_dim_i == 2) THEN
	energy = (4.0E0_DP / 3.0E0_DP) * pi * ((DBLE (1) - 5.0E-1_DP) * dx) ** 3 &
			* (u_old (3, 1) + 5.0D-1 * rho (1) * phi (1))
	DO j = 2, length_step
		energy = energy + 4.0E0_DP * pi * dx * ((DBLE (j) - 5.0E-1_DP) * dx) ** 2 &
				* (u_old (3, j) + 5.0E-1_DP * rho (j) * phi (j))
	END DO
END IF

END SUBROUTINE