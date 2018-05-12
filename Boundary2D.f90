SUBROUTINE BOUNDARY2D (u)
USE DEFINITION
IMPLICIT NONE

REAL (DP), INTENT (INOUT), DIMENSION (1 : no_of_eq, -4 : length_step + 5) :: u
INTEGER :: i, j

DO i = 1, no_of_eq
	IF (parity2d (i) == even) THEN
		DO j = 1, 5
			u (i, 1 - j) = u (i, 1)
		END DO
	ELSE IF (parity2d (i) == odd) THEN
		DO j = 1, 5
			u (i, 1 - j) = - u (i, j)
		END DO
	END IF

	DO j = 1, 5
		u (i, length_step + j) = u (i, length_step)
	END DO
END DO

END SUBROUTINE
