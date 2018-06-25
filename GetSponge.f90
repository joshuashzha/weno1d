!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! This subroutine supply a sponge on the stellar
! surface to avoid spurious oscillation due to
! the imperfect initial model
! Written by Leung Shing Chi in 2016
! based on the Zingale 2002
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine getsponge
use definition
implicit none

! Dummy variables
integer :: i, j

! Position factor defined in the paper
real (DP) :: r_tp, r_md, r_sp, rad_dist
real (DP) :: f_damp

! Threshold density and damping factor
real (DP) :: rho_sponge = 1.0D-13
real (DP) :: kap_sponge = 4.9282D-3

! Give r_tp and r_sp an arbitrarily large number 

rho_sponge = rho(1) * 0.01D0

r_md = 1.0D10
r_sp = 1.0D10
i = 0

! First find r_sp according to the prescription in paper
do j = 1, length_step, 1
   rad_dist = (DBLE(j) -0.5) * dx
   if(rho(j) <= rho_sponge) then
      r_sp = MIN(rad_dist, r_sp)
   endif
   if(rho(j) <= rho_a) then
      r_md = MIN(rad_dist, r_md)
   endif
enddo
r_tp = 2.0D0 * r_md - r_sp

! Then assign a relaxed veloicty according
! to the damping strength
do j = 1, length_step, 1

   rad_dist = (DBLE(j)-0.5)*dx
   if(rad_dist > r_tp) then
      f_damp = 1.0D0
   elseif(rad_dist <= r_tp .and. rad_dist > r_sp) then
      f_damp = 0.5D0 * (1.0D0 - DCOS(pi * (rad_dist - r_sp)/(r_tp - r_sp)))
   else
      f_damp = 0.0D0
   endif

   vel(j) = vel(j) / (1.0D0 + kap_sponge * f_damp * dt)

end do

end subroutine
