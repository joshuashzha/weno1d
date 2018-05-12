PROGRAM Star_WENO
USE DEFINITION
IMPLICIT NONE

INTEGER :: m, n, fileno_len
CHARACTER (len = 256) :: fileno
real (DP) :: output_time_last

CALL INITIAL

global_time = 0.0_dp
output_time_last = global_time
WRITE (fileno, *) n_backup / time_step + 1
fileno = ADJUSTL (fileno)
fileno_len = LEN_TRIM (fileno)

OPEN (UNIT = 11, FILE = 'Star_WENO_Density_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')
OPEN (UNIT = 12, FILE = 'Star_WENO_Velocity_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')
OPEN (UNIT = 13, FILE = 'Star_WENO_Epsilon_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')
OPEN (UNIT = 14, FILE = 'Star_WENO_Pressure_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')
OPEN (UNIT = 15, FILE = 'Star_WENO_Potential_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')
OPEN (UNIT = 16, FILE = 'Star_WENO_PotentialDerivative_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')
OPEN (UNIT = 17, FILE = 'Star_WENO_d_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')
OPEN (UNIT = 18, FILE = 'Star_WENO_s_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')
OPEN (UNIT = 19, FILE = 'Star_WENO_tau_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')
OPEN (UNIT = 20, FILE = 'Star_WENO_Mass_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')
OPEN (UNIT = 21, FILE = 'Star_WENO_Energy_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')
OPEN (UNIT = 22, FILE = 'Star_WENO_CentralDensity_'//fileno (1 : fileno_len)//'.dat', STATUS = 'REPLACE')

CALL OUTPUT (n_backup)

DO n = 1, time_step
   CALL FindDt
   if(global_time + dt >= total_time) dt=total_time-global_time
   write(*,101) n,global_time, dt
   global_time = global_time + dt
   CALL RUNGEKUTTA

   IF (global_time - output_time_last >= output_time) THEN
      write(*,102) global_time
      CALL OUTPUT (n_backup + n)
      output_time_last = global_time
   END IF
   If(global_time >= total_time) exit
END DO

CALL OUTPUT (n_backup + n)
DO m = 11, 22
	CLOSE (m)
END DO

CALL BACKUP (n_backup + n - 1)

101 format('step:',1XI4,2X't:',1XES10.3,2X'dt:',1XES9.2)
102 format('Output at time:',1XES10.3)
END PROGRAM
