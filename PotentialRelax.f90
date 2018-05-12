SUBROUTINE POTENTIALRELAX
USE DEFINITION
IMPLICIT NONE

INTEGER :: j, n, exno
REAL (DP), DIMENSION (-4 : length_step + 5) :: phinew, error

DO n = 1, relax_max
	DO j = 1, length_step - 1
		phinew (j) = 5.0E-1_DP * (phi (j + 1) + phi (j - 1)) + 5.0E-1_DP * dx / ((DBLE (j) - 5.0E-1_DP) * dx) &
				* (phi (j + 1) - phi (j - 1)) - 2.0E0_DP * pi * dx ** 2 * rho (j)
	END DO
	phinew (length_step) = phi (length_step)
	CALL BOUNDARY1D (phinew, even)

	DO j = 1, length_step - 1
		error (j) = (phinew (j) - phi (j)) / phi (j)
	END DO
	error (length_step) = 0.0E0_DP
	CALL BOUNDARY1D (error, even)

	DO j = -4, length_step + 5
		phi (j) = phinew (j)
	END DO

	DO j = 1, length_step
		phip (j) = (- phi (j + 2) + 8.0E0_DP * phi (j + 1) - 8.0E0_DP * phi (j - 1) + phi (j - 2)) / (1.2E1_DP * dx)
	END DO
	CALL BOUNDARY1D (phip, odd)

	exno = 1
	DO j = 1, length_step
		IF (ABS (error (j)) > tolerance) THEN
			exno = 0
		END IF
	END DO
	IF (exno == 1) THEN
		EXIT
	END IF
END DO

END SUBROUTINE