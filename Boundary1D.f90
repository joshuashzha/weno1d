SUBROUTINE BOUNDARY1D (array, sign)
USE DEFINITION
IMPLICIT NONE

REAL (DP), INTENT (INOUT), DIMENSION (-4 : length_step + 5) :: array
INTEGER, INTENT (IN) :: sign
INTEGER :: j

IF(star_model) THEN

   IF(sign == even) THEN
      DO j = 1,5
         array(1-j) = array(j)
      ENDDO
   ELSEIF(sign == odd) THEN
      DO j = 1,5
         array(1-j) = - array(j)
      ENDDO
   ENDIF

   DO j=1, 5
      array(length_step + j) = array(length_step)
   ENDDO

ELSEIF(test_model) THEN

   DO j = 1, 5
      array (1 - j) = array (1)
      array (length_step + j) = array (length_step)
   END DO

ENDIF

END SUBROUTINE
