SUBROUTINE WENO (v, vplus, vminus)
USE DEFINITION
IMPLICIT NONE

REAL (DP), INTENT (IN), DIMENSION (-4 : length_step + 5) :: v
REAL (DP), INTENT (OUT), DIMENSION (-4 : length_step + 5) :: vplus, vminus
INTEGER :: i, j, r, s
REAL (DP) :: temp
REAL (DP), DIMENSION (-1 : 2, 0 : 2) :: c
REAL (DP), DIMENSION (0 : 2) :: vrhs, vlhs, d, td, beta, alpha, talpha, omega, tomega

CALL CONSTC (c)
CALL CONSTD (d)

DO r = 0, 2
	td (r) = d (2 - r)
END DO

DO i = -2, length_step + 3
	DO r = 0, 2
		vrhs (r) = 0.0E0_DP

		DO j = 0, 2
			vrhs (r) = vrhs (r) + c (r, j) * v (i - r + j)
		END DO
	END DO

	DO r = 0, 2
		vlhs (r) = 0.0E0_DP

		DO j = 0, 2
			vlhs (r) = vlhs (r) + c (r - 1, j) * v (i - r + j)
		END DO
	END DO

	beta (0) = (1.3E1_DP / 1.2E1_DP) * (v (i) - 2 * v (i + 1) + v (i + 2)) ** 2 &
			+ (1.0E0_DP / 4.0E0_DP) * (3 * v (i) - 4 * v (i + 1) + v (i + 2)) ** 2
	beta (1) = (1.3E1_DP / 1.2E1_DP) * (v (i - 1) - 2 * v (i) + v (i + 1)) ** 2 &
			+ (1.0E0_DP / 4.0E0_DP) * (v (i - 1) - v (i + 1)) ** 2
	beta (2) = (1.3E1_DP / 1.2E1_DP) * (v (i - 2) - 2 * v (i - 1) + v (i)) ** 2 &
			+ (1.0E0_DP / 4.0E0_DP) * (v (i - 2) - 4 * v (i - 1) + 3 * v (i)) ** 2

	DO r = 0, 2
		alpha (r) = d (r) / (beta (r) + smallpara) ** 2
	END DO

	temp = 0.0E0_DP

	DO s = 0, 2
		temp = temp + alpha (s)
	END DO

	DO r = 0, 2
		omega (r) = alpha (r) / temp
	END DO

	DO r = 0, 2
		talpha (r) = td (r) / (beta (r) + smallpara) ** 2
	END DO

	temp = 0.0E0_DP

	DO s = 0, 2
		temp = temp + talpha (s)
	END DO

	DO r = 0, 2
		tomega (r) = talpha (r) / temp
	END DO

	vminus (i) = 0.0E0_DP

	DO r = 0, 2
		vminus (i) = vminus (i) + omega (r) * vrhs (r)
	END DO

	vplus (i) = 0.0E0_DP

	DO r = 0, 2
		vplus (i) = vplus (i) + tomega (r) * vlhs (r)
	END DO
END DO

END SUBROUTINE