SUBROUTINE INI_DER (der, x, y, no_of_eq_ini)
USE DEFINITION
IMPLICIT NONE

INTEGER, INTENT (IN) :: no_of_eq_ini
REAL (DP), INTENT (IN) :: x
REAL (DP), INTENT (IN), DIMENSION (1 : no_of_eq_ini + 1) :: y
REAL (DP), INTENT (OUT), DIMENSION (1 : no_of_eq_ini + 1) :: der

der (1) = 4.0E0_DP * pi * x ** 2 * y (3)
der (2) = - y (1) * y (3) / x ** 2
der (3) = 0.0E0_DP

END SUBROUTINE