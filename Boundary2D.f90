SUBROUTINE BOUNDARY2D (u)
USE DEFINITION
IMPLICIT NONE

REAL (DP), INTENT (INOUT), DIMENSION (1 : no_of_eq, -4 : length_step + 5) :: u
INTEGER :: i, j

IF(star_model) THEN
  
   do j = 1,5
      u(1,1-j) = u(1,j)
      u(2,1-j) = - u(2,j)
      u(3,1-j) = u(3,j)
   enddo

   do j = 1,5
      do i = 1,no_of_eq
         u(i,length_step+j) = u(i,length_step)
      enddo
   enddo

ELSEIF(test_model) THEN

   do j=1,5
      do i=1,no_of_eq
         u(i,1-j) = u(i,1) 
         u(i,length_step+j) = u(i,length_step)
      enddo
   enddo

ENDIF

END SUBROUTINE
