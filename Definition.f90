MODULE DEFINITION
IMPLICIT NONE
SAVE
INCLUDE "Parameter.h"

INTEGER, PARAMETER :: no_of_eq = 3
INTEGER, PARAMETER :: even = 0, odd = 1
INTEGER, DIMENSION (1 : no_of_eq), PARAMETER :: parity2d = (/ even, even, even /)
REAL (DP), PARAMETER :: pi = 3.1415926535897932384626433832795E0_DP

INTEGER, PARAMETER :: length_step = INT (total_length / dx)
REAL (DP) :: dt = cfl * dx
INTEGER, PARAMETER :: time_step = 100000 
REAL (DP) :: global_time

REAL (DP), PARAMETER :: sp_dim = DBLE (sp_dim_i)

INTEGER :: n_backup, r_grid
REAL (DP), DIMENSION (-4 : length_step + 5) :: rho, vel, epsilon, p, dpdrho, dpdepsilon, phi, phip
REAL (DP), DIMENSION (1 : no_of_eq, -4 : length_step + 5) :: u_old, u_one, u_two, u_three, u_four, u_new, f, sa, sb, l
REAL (DP) :: mass, energy, centralrho

END MODULE DEFINITION
