SUBROUTINE OUTPUT (n)
USE DEFINITION
IMPLICIT NONE

INTEGER, INTENT (IN) :: n
INTEGER :: j

WRITE (11, *) '"Time = ', global_time
DO j = 1, length_step
	WRITE (11, 701) dx * (DBLE (j) - 5.0E-1_DP), rho (j)
END DO
WRITE (11, *)

WRITE (12, *) '"Time = ', global_time
DO j = 1, length_step
	WRITE (12, 701) dx * (DBLE (j) - 5.0E-1_DP), vel (j)
END DO
WRITE (12, *)

WRITE (13, *) '"Time = ', global_time
DO j = 1, length_step
	WRITE (13, 701) dx * (DBLE (j) - 5.0E-1_DP), epsilon (j)
END DO
WRITE (13, *)

WRITE (14, *) '"Time = ', global_time
DO j = 1, length_step
	WRITE (14, 701) dx * (DBLE (j) - 5.0E-1_DP), p (j)
END DO
WRITE (14, *)

WRITE (15, *) '"Time = ', global_time
DO j = 1, length_step
	WRITE (15, 701) dx * (DBLE (j) - 5.0E-1_DP), phi (j)
END DO
WRITE (15, *)

WRITE (16, *) '"Time = ', global_time
DO j = 1, length_step
	WRITE (16, 701) dx * (DBLE (j) - 5.0E-1_DP), phip (j)
END DO
WRITE (16, *)

WRITE (17, *) '"Time = ', global_time
DO j = 1, length_step
	WRITE (17, 701) dx * (DBLE (j) - 5.0E-1_DP), u_old (1, j)
END DO
WRITE (17, *)

WRITE (18, *) '"Time = ', global_time
DO j = 1, length_step
	WRITE (18, 701) dx * (DBLE (j) - 5.0E-1_DP), u_old (2, j)
END DO
WRITE (18, *)

WRITE (19, *) '"Time = ', global_time
DO j = 1, length_step
	WRITE (19, 701) dx * (DBLE (j) - 5.0E-1_DP), u_old (3, j)
END DO
WRITE (19, *)

WRITE (20, 701) global_time, mass
WRITE (21, 701) global_time, energy
WRITE (22, 701) global_time, centralrho

701 FORMAT (2F33.15)

END SUBROUTINE
