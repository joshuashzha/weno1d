SUBROUTINE RUNGEKUTTA
USE DEFINITION
IMPLICIT NONE

INTEGER :: i, j
REAL (DP), DIMENSION (1 : no_of_eq, -4 : length_step + 5) :: u_rubbish

CALL SPATIAL (u_old)
DO i = 1, no_of_eq
	DO j = 1, length_step
		u_one (i, j) = u_old (i, j) + (1.0E0_DP / 7.0E0_DP) * dt * l (i, j)
	END DO
END DO
CALL BOUNDARY2D (u_one)
CALL FROMUTORVE (u_one)
!CALL CHECKRHO
!CALL FROMRVETOU (u_one)
CALL UPDATE (u_one, u_rubbish)

CALL SPATIAL (u_one)
DO i = 1, no_of_eq
	DO j = 1, length_step
		u_two (i, j) = u_old (i, j) + (3.0E0_DP / 1.6E1_DP) * dt * l (i, j)
	END DO
END DO
CALL BOUNDARY2D (u_two)
CALL FROMUTORVE (u_two)
!CALL CHECKRHO
!CALL FROMRVETOU (u_two)
CALL UPDATE (u_two, u_rubbish)

CALL SPATIAL (u_two)
DO i = 1, no_of_eq
	DO j = 1, length_step
		u_three (i, j) = u_old (i, j) + (1.0E0_DP / 3.0E0_DP) * dt * l (i, j)
	END DO
END DO
CALL BOUNDARY2D (u_three)
CALL FROMUTORVE (u_three)
!CALL CHECKRHO
!CALL FROMRVETOU (u_three)
CALL UPDATE (u_three, u_rubbish)

CALL SPATIAL (u_three)
DO i = 1, no_of_eq
	DO j = 1, length_step
		u_four (i, j) = u_old (i, j) + (2.0E0_DP / 3.0E0_DP) * dt * l (i, j)
	END DO
END DO
CALL BOUNDARY2D (u_four)
CALL FROMUTORVE (u_four)
!CALL CHECKRHO
!CALL FROMRVETOU (u_four)
CALL UPDATE (u_four, u_rubbish)

CALL SPATIAL (u_four)
DO i = 1, no_of_eq
	DO j = 1, length_step
		u_new (i, j) = - (3.0E0_DP / 4.0E0_DP) * u_old (i, j) + (7.0E0_DP / 4.0E0_DP) * u_one (i, j) &
				+ (3.0E0_DP / 4.0E0_DP) * dt * l (i, j)
	END DO
END DO
CALL BOUNDARY2D (u_new)
CALL FROMUTORVE (u_new)
if(checkrho_flag) CALL CHECKRHO
CALL FROMRVETOU (u_new)
CALL UPDATE (u_new, u_old)

END SUBROUTINE
