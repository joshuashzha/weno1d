SUBROUTINE BOUNDARY1D (array, sign)
USE DEFINITION
IMPLICIT NONE

REAL (DP), INTENT (INOUT), DIMENSION (-4 : length_step + 5) :: array
INTEGER, INTENT (IN) :: sign
INTEGER :: j

IF (sign == even) THEN
	DO j = 1, 5
		array (1 - j) = array (1)
	END DO
ELSE IF (sign == odd) THEN
	DO j = 1, 5
		array (1 - j) = - array (j)
	END DO
END IF

DO j = 1, 5
	array (length_step + j) = array (length_step)
END DO

END SUBROUTINE
