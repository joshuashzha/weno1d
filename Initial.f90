SUBROUTINE INITIAL
USE DEFINITION
IMPLICIT NONE

INTEGER :: j, m

IF(test_model) THEN

        write(*,*) 'Running 1D test model!'
        n_backup = 0
        CALL GETRHO_simple
        CALL FROMRVETOU(u_new)
        CALL UPDATE(u_new, u_old)

ELSEIF(star_model) THEN

        write(*,*) 'Construing a ploytrope'
	n_backup = 0
	CALL GETRHO
	CALL GETVEL
	CALL GETEPSILON
	CALL CHECKRHO
	CALL FROMRVETOU (u_new)
	CALL UPDATE (u_new, u_old)
END IF

END SUBROUTINE
