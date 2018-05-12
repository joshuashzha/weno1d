SUBROUTINE CHECKRHO
USE DEFINITION
IMPLICIT NONE

INTEGER :: j, found

r_grid = length_step
found = 0

DO j = length_step, 1, -1
	IF (found == 0) THEN
		IF (rho (j) > rho_a) THEN
			r_grid = j
			found = 1
		END IF
	ELSE IF (found == 1) THEN
		IF (rho (j) > 1.1E0_DP * rho_a) THEN
			EXIT
		ELSE IF (rho (j) <= rho_a) THEN
			found = 0
		END IF
	END IF
END DO

DO j = -4, r_grid
	IF (rho (j) < rho_a) THEN
		rho (j) = rho_a
		vel (j) = vel_a
		epsilon (j) = epsilon_a
	END IF
END DO

DO j = r_grid + 1, length_step + 5
	rho (j) = rho_a
	vel (j) = vel_a
	epsilon (j) = epsilon_a
END DO

CALL BOUNDARY1D (rho, even)
CALL BOUNDARY1D (vel, odd)
CALL BOUNDARY1D (epsilon, even)

END SUBROUTINE
