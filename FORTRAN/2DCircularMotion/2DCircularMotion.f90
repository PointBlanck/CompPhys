program CircularMotion

  implicit none

  !Declaration of variables:
  real            :: x0,y0,R,x,y,vx,vy,t,t0,tf,dt
  real            :: theta,omega
  real, parameter :: PI = 3.1415927

  !----------------------------------
  ! Ask for input:
  !----------------------------------
  print *,'# Enter omega: '
  read  *,omega
  print *,'# Enter (x0,y0) and R: '
  read  *, x0, y0, R
  print *,'# Enter t0,tf,dt: '
  read  *,t0,tf,dt
  print *,'# omega   = ',omega
  print *,'# x0,y0,R = ',x0,y0,R
  print *,'#t0,tf,dt = ',t0,tf,dt

  !-----------------------------------
  ! Initialize:
  !-----------------------------------
  if (R .le. 0.0) stop 'Illegal value for R'
  if (t0 .lt. 0.0) stop 'Illegal value for t0'
  if (tf .le. t0) stop 'Illegal value for tf (tf < t0)'
  if (omega .le. 0.0) stop 'Illegal value for omega'
  if (dt .le. 0.0) stop 'Illegal value for dt'
  print *,'# T= ', 2.0*PI/omega
  open(unit=11,file='Circle.dat')
  ! Compute:
  t = t0
  do while (t .le. tf)
     t = t + dt
     theta = omega*(t - t0)
     x     = x0 + R*cos(theta)
     y     = y0 + R*sin(theta)
     vx    = -omega*R*sin(theta)
     vy    = omega*R*cos(theta)
     write(11,*) t, x, y, vx, vy
     t     = t + dt
  end do
  close(11)

end program CircularMotion
