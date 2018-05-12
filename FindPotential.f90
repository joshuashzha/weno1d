SUBROUTINE FINDPOTENTIAL
USE DEFINITION
IMPLICIT NONE

INTEGER :: j
REAL (DP), DIMENSION (-4 : length_step + 5) :: temp

IF (w_gravity_i == 1) THEN
	temp (1) = (4.0E0_DP / 3.0E0_DP) * pi * ((DBLE (1) - 5.0E-1_DP) * dx) ** 3 * rho (1)
	temp (2) = 9.0E0_DP * temp (1) + (4.0E0_DP / 3.0E0_DP) * pi * dx * ((DBLE (2) - 5.0E-1_DP) * dx) ** 2 * rho (2)
	DO j = 3, length_step
		temp (j) = temp (j - 2) + 4.0E0_DP * pi * (dx / 3.0E0_DP) &
				* (rho (j - 2) * ((DBLE (j - 2) - 5.0E-1_DP) * dx) ** 2 &
				+ 4.0E0_DP * rho (j - 1) * ((DBLE (j - 1) - 5.0E-1_DP) * dx) ** 2 &
				+ rho (j) * ((DBLE (j) - 5.0E-1_DP) * dx) ** 2)
	END DO

	Do j = 1, length_step
		phip (j) = temp (j) / ((DBLE (j) - 5.0E-1_DP) * dx) ** 2
	END DO
	CALL BOUNDARY1D (phip, odd)

	phi (length_step) = 0.0E0_DP
	DO j = length_step - 1, 1, -1
		phi (j) = phi (j + 1) - (phip (j) + phip (j + 1)) * dx / 2.0E0_DP
	END DO
	CALL BOUNDARY1D (phi, even)

	CALL POTENTIALRELAX
ELSE
	DO j = -4, length_step + 5
		phi (j) = 0.0E0_DP
		phip (j) = 0.0E0_DP
	END DO
END IF

END SUBROUTINE