SUBROUTINE GETRHO_EOSPTOR (ini_rho, ini_p, k_more, gamma_more)
USE DEFINITION
IMPLICIT NONE

REAL (DP), INTENT (IN) :: ini_p, k_more, gamma_more
REAL (DP), INTENT (OUT) :: ini_rho

ini_rho = (ini_p / k_more) ** (1.0E0_DP / gamma_more)

END SUBROUTINE