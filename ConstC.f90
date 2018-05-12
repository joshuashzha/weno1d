SUBROUTINE CONSTC (c)
USE DEFINITION
IMPLICIT NONE

REAL (8), INTENT (OUT), DIMENSION (-1 : 2, 0 : 2) :: c

c (-1, 0) = 1.1E1_DP / 6.0E0_DP
c (-1, 1) = - 7.0E0_DP / 6.0E0_DP
c (-1, 2) = 1.0E0_DP / 3.0E0_DP

c (0, 0) = 1.0E0_DP / 3.0E0_DP
c (0, 1) = 5.0E0_DP / 6.0E0_DP
c (0, 2) = - 1.0E0_DP / 6.0E0_DP

c (1, 0) = - 1.0E0_DP / 6.0E0_DP
c (1, 1) = 5.0E0_DP / 6.0E0_DP
c (1, 2) = 1.0E0_DP / 3.0E0_DP

c (2, 0) = 1.0E0_DP / 3.0E0_DP
c (2, 1) = - 7.0E0_DP / 6.0E0_DP
c (2, 2) = 1.1E1_DP / 6.0E0_DP

END SUBROUTINE