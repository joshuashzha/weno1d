SUBROUTINE SPATIAL (u)
USE DEFINITION
IMPLICIT NONE

INTEGER :: i, j
REAL (DP), INTENT (IN), DIMENSION (1 : no_of_eq, -4 : length_step + 5) :: u
REAL (DP), DIMENSION (1 : no_of_eq, -4 : length_step + 5) :: f_p, f_m, flux, dfdx
REAL (DP), DIMENSION (-4 : length_step + 5) :: v, v_p, v_m, flux_p, flux_m
REAL (DP) :: alpha

CALL ALPHASPLIT (alpha)

DO i = 1, no_of_eq
	DO j = -4, length_step + 5
		f_p (i, j) = 5.0E-1_DP * (f (i, j) + alpha * u (i, j))
		f_m (i, j) = 5.0E-1_DP * (f (i, j) - alpha * u (i, j))
	END DO
END DO

DO i = 1, no_of_eq
	DO j = -4, length_step + 5
		v (j) = f_p (i, j)
	END DO
	CALL WENO (v, v_p, v_m)
	DO j = -1, length_step + 2
		flux_p (j) = v_m (j)
	END DO

	DO j = -4, length_step + 5
		v (j) = f_m (i, j)
	END DO
	CALL WENO (v, v_p, v_m)
	DO j = -1, length_step + 2
		flux_m (j) = v_p (j + 1)
	END DO

	DO j = 0, length_step + 1
		flux (i, j) = flux_p (j) + flux_m (j)
	END DO
END DO

DO i = 1, no_of_eq
	DO j = 1, length_step
		dfdx (i, j) = (flux (i, j) - flux (i, j - 1)) / dx
	END DO

	DO j = 1, length_step
		l (i, j) = - dfdx (i, j) !- sp_dim / ((DBLE (j) - 5.0E-1_DP) * dx) * sa (i, j) - sb (i, j)
	END DO
END DO

END SUBROUTINE
