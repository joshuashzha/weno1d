SUBROUTINE UPDATE (v, u)
USE DEFINITION
IMPLICIT NONE

REAL (DP), INTENT (INOUT), DIMENSION (1 : no_of_eq, -4 : length_step + 5) :: v
REAL (DP), INTENT (OUT), DIMENSION (1 : no_of_eq, -4 : length_step + 5) :: u
INTEGER :: i, j

DO i = 1, no_of_eq
	DO j = -4, length_step + 5
		u (i, j) = v (i, j)
	END DO
END DO

CALL FINDPRESSURE
CALL FINDMASS
CALL FINDPOTENTIAL
CALL FINDENERGY
CALL FINDCENTRALDENSITY

DO j = -4, length_step + 5
	f (1, j) = u (2, j)
	f (2, j) = u (2, j) * vel (j) + p (j)
	f (3, j) = vel (j) * (u (3, j) + p (j))

	sa (1, j) = f (1, j)
	sa (2, j) = u (2, j) * vel (j)
	sa (3, j) = f (3, j)

	sb (1, j) = 0.0E0_DP
	sb (2, j) = u (1, j) * phip (j)
	sb (3, j) = u (2, j) * phip (j)
END DO

END SUBROUTINE