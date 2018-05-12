SUBROUTINE BACKUP (n)
USE DEFINITION
IMPLICIT NONE

INTEGER :: j, m
INTEGER, INTENT (IN) :: n

OPEN (UNIT = 51, FILE = 'BackUp_d.dat', STATUS = 'REPLACE')
OPEN (UNIT = 52, FILE = 'BackUp_s.dat', STATUS = 'REPLACE')
OPEN (UNIT = 53, FILE = 'BackUp_tau.dat', STATUS = 'REPLACE')
OPEN (UNIT = 54, FILE = 'BackUp_time.dat', STATUS = 'REPLACE')

DO j = 1, length_step
	WRITE (51, *) u_old (1, j)
	WRITE (52, *) u_old (2, j)
	WRITE (53, *) u_old (3, j)
END DO

WRITE (54, *) n
WRITE (54, *) DBLE (n) * dt

DO m = 51, 54
	CLOSE (m)
END DO

END SUBROUTINE