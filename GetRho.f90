SUBROUTINE GETRHO
USE DEFINITION
IMPLICIT NONE

INTEGER, PARAMETER :: no_of_eq_ini = 2, more = 10 ** ini_acc
INTEGER, PARAMETER :: length_morestep = length_step * more
REAL (DP), PARAMETER :: dxmore = dx / DBLE (more), k_more = k, gamma_more = gamma
INTEGER :: i, j, r_grid_more
REAL (DP) :: p_c, p_a, x, ini_rho, ini_p
REAL (DP), DIMENSION (1 : no_of_eq_ini + 1) :: y_zero, y_one, y_two, y_three, der
REAL (DP), DIMENSION (1 : no_of_eq_ini + 1, -4 : length_morestep + 5) :: y		! y = (/ mass, pressure, density /)

CALL GETRHO_EOSRTOP (p_c, rho_c, k_more, gamma_more)
CALL GETRHO_EOSRTOP (p_a, rho_a, k_more, gamma_more)

y (1, 0) = 0.0E0_DP
y (2, 0) = p_c
y (3, 0) = rho_c
r_grid_more = length_morestep

DO j = 0, length_morestep - 1
	DO i = 1, no_of_eq_ini + 1
		y_zero (i) = y (i, j)
	END DO

	x = DBLE (j) * dxmore
	CALL INI_DER (der, x, y_zero, no_of_eq_ini)
	IF (j == 0) THEN
		der (2) = 0.0E0_DP
	END IF
	DO i = 1, no_of_eq_ini
		y_one (i) = y_zero (i) + (1.0E0_DP / 2.0E0_DP) * dxmore * der (i)
	END DO
	ini_p = y_one (2)
	IF (ini_p <= p_a) THEN
		r_grid_more = j
		EXIT
	END IF
	CALL GETRHO_EOSPTOR (ini_rho, ini_p, k_more, gamma_more)
	y_one (3) = ini_rho

	x = DBLE (j) * dxmore + (1.0E0_DP / 2.0E0_DP) * dxmore
	CALL INI_DER (der, x, y_one, no_of_eq_ini)
	DO i = 1, no_of_eq_ini
		y_two (i) = y_zero (i) + (1.0E0_DP / 2.0E0_DP) * dxmore * der (i)
	END DO
	ini_p = y_two (2)
	IF (ini_p <= p_a) THEN
		r_grid_more = j
		EXIT
	END IF
	CALL GETRHO_EOSPTOR (ini_rho, ini_p, k_more, gamma_more)
	y_two (3) = ini_rho

	x = DBLE (j) * dxmore + (1.0E0_DP / 2.0E0_DP) * dxmore
	CALL INI_DER (der, x, y_two, no_of_eq_ini)
	DO i = 1, no_of_eq_ini
		y_three (i) = y_zero (i) + dxmore * der (i)
	END DO
	ini_p = y_three (2)
	IF (ini_p <= p_a) THEN
		r_grid_more = j
		EXIT
	END IF
	CALL GETRHO_EOSPTOR (ini_rho, ini_p, k_more, gamma_more)
	y_three (3) = ini_rho

	x = DBLE (j) * dxmore + dxmore
	CALL INI_DER (der, x, y_three, no_of_eq_ini)
	DO i = 1, no_of_eq_ini
		y (i, j + 1) = - (1.0E0_DP / 3.0E0_DP) * y_zero (i) + (1.0E0_DP / 3.0E0_DP) * y_one (i) &
				+ (2.0E0_DP / 3.0E0_DP) * y_two (i) + (1.0E0_DP / 3.0E0_DP) * y_three (i) &
				+ (1.0E0_DP / 6.0E0_DP) * dxmore * der (i)
	END DO
	ini_p = y (2, j + 1)
	IF (ini_p <= p_a) THEN
		r_grid_more = j
		EXIT
	END IF
	CALL GETRHO_EOSPTOR (ini_rho, ini_p, k_more, gamma_more)
	y (3, j + 1) = ini_rho
END DO

DO j = r_grid_more + 1, length_morestep
	y (3, j) = rho_a
END DO

DO j = 1, length_step
	rho (j) = y (3, j * more - more / 2)
END DO

CALL BOUNDARY1D (rho, even)

END SUBROUTINE