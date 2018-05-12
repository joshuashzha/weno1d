SUBROUTINE GETRHO_EOSRTOP (ini_p, ini_rho, k_more, gamma_more)
USE DEFINITION
IMPLICIT NONE

REAL (DP), INTENT (IN) :: ini_rho, k_more, gamma_more
REAL (DP), INTENT (OUT) :: ini_p

ini_p = k_more * ini_rho ** gamma_more

END SUBROUTINE