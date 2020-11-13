module functions
subroutine scaterate()
40  format(6(1x,e11.5))
!use physconst
use math
use constlib
implicit none
real*8 argplus, argminus, argminussnd, soundrateamp, summrate
	sndrateconst = (me*koefe)**0.5d0*(kberg*temp)**3*(defpot*evtoerg)**2/(2**2.5d0*pi*hperg**4*usl**4*ro)
	nqopt = 1/(exp(fonon/(kbol*temp))-1)
    argplus=4*sqes*(sqes+sqrt(energy))/(kbol*temp)
    argminus=4*sqes*(sqes-sqrt(energy))/(kbol*temp)
    argminussnd=4*sqes*(sqrt(energy)-sqes)/(kbol*temp)
    if (energy > fonon) then
      !optrateemi = optrateconst*(nqopt+1)*sqrt((energy-fonon)*evtoerg)
      optrateemi = optrateconst*(nqopt+1)*(1+2*alp*(energy-fonon))*sqrt((energy-fonon)*evtoerg*(1+alp*(energy-fonon)))
    else
      optrateemi = 0
    end if
    !optrateabs = optrateconst*nqopt*sqrt((energy+fonon)*evtoerg)
    optrateabs = optrateconst*nqopt*(1+2*alp*(energy+fonon))*sqrt((energy+fonon)*evtoerg*(1+alp*(energy+fonon)))
    if (energy.eq.0) then
      energy=1d-15
      print *, 'warning energy = 0'
    end if
    soundrateamp=sndrateconst/sqrt(energy*evtoerg)
    if (energy < esound) then
       soundrateabs = soundrateamp*(fone(argplus)-fone(argminus))
       soundrateemi = 0
    else
       soundrateabs = soundrateamp*fone(argplus)
       soundrateemi = soundrateamp*gone(argminussnd)
    end if
    !summrate=optrateabs+optrateemi+soundrateabs+soundrateemi
    summrate=soundrateabs+soundrateemi
    if (summrate.gt.gamma) then
		print *, 'warning, rates > gamma at energy = ', energy
		write (*, 40) summrate, gamma
		pause
	end if
	!write (*, 40) summrate, gamma
return
end


subroutine scaterateres()
!use physconst
use math
use constlib
implicit none
40  format(6(1x,e11.5))
call scaterate()
!print*,energy,soundrateabs, soundrateemi;pause
optrateabs=0; optrateemi=0
CALL random_number(ranval)
!if (energy > 1d-6) then
	!write (*, 40) energy, esound; pause
!end if
a=ranval*gamma
!a=ranval*(soundrateemi+soundrateabs)
res=1337
   if ((a.le.soundrateemi).and.(a.ge.0)) then
      res = 1
      call  anglesound()
   end if
   if ((a.le.soundrateemi+soundrateabs).and.(a.gt.soundrateemi)) then
      res = 2
      call anglesound()
   end if
   if ((a.le.soundrateemi+soundrateabs+optrateemi).and.(a.gt.soundrateemi+soundrateabs)) then
      res = 3
      energy = energy - fonon
      call angleoptic()
   end if
   if ((a.le.soundrateemi+soundrateabs+optrateemi+optrateabs).and.(a.gt.soundrateemi+soundrateabs+optrateemi)) then
      energy = energy + fonon
      res = 4
      call angleoptic()
   end if
   if (a.gt.soundrateemi+soundrateabs+optrateemi+optrateabs) then
      res = 5
   end if
   If (energy.lt.0) then
      print *, 'energy < 0 !!!'
      energy = 1d-15
   end if
   !write (*,e11.5) soundrateemi+soundrateabs+optrateemi+optrateabs
   !write (*,*) a, res;pause
return
end

subroutine angleoptic()
	!use physconst
	use math
	call random_number(ranval)
	teta = acos(1-2*ranval)
	call random_number(ranval)
	fi = 2*PI*ranval
	return
end

subroutine anglesound()
  use math
  use constlib
  !use physconst
  implicit none
  integer :: i, j, k
  real*8 q, deltae, qend, dq, iq
  real*8 kx, ky, kz, ks, ke, kxe, kye, kze
  real*8 beta, g1, g2, g3, nu
  real*8 kostil
  real*8 qendres,c, qendfix
  integer :: test=0
  q=0
  test=0
  CALL random_number(ranval)
  !write (*, 40) energy, esound; pause
  if (res.eq.1) then
    if (energy.lt.esound) then
       qend=0d0
       q=0d0
       print *, 'emission, energy < esound'
    else
       qend=4*sqes*(sqrt(energy)-sqes)/(kbol*temp)
       q=0d0
    end if
    qendres=fone(qend)
    qendfix=qend
    if (qendres.ne.0d0) then
		do while ((qend-q)>eps)
			dq=(qend-q)/2.0
			iq=q+dq
			if ((fone(q)/fone(qendfix)-ranval)*(fone(iq)/fone(qendfix)-ranval).lt.0  ) then
				qend=iq
			else
				q=iq
			end if
		end do
    else
		q=0.0  
    end if
  end if
  if (res.eq.2) then
     if (energy < esound) then
       qend=4*sqes*(sqes+sqrt(energy))/(kbol*temp)
       q=4*sqes*(sqes-sqrt(energy))/(kbol*temp)
       print *, 'absrob, energy < esound'
    else
       qend=4*sqes*(sqes+sqrt(energy))/(kbol*temp)
       q=0d0
    end if
    qendres=gone(qend)
    qendfix=qend
    if (qend.ne.0) then
		do while ((qend-q)>eps)
			dq=(qend-q)/2.0
			iq=q+dq
			if ((gone(q)/gone(qendfix)-ranval)*(gone(iq)/gone(qendfix)-ranval).lt.0  ) then
				qend=iq
			else
				q=iq
			end if
		end do
    else
    q=0.0
    end if
  end if
  !if (q.ne.0) then
	!print *, q; pause
  !end if
  q=q*temp*kberg/(hperg*usl)
  deltae=hperg*q*usl
  qendfix = qendfix*temp*kberg/(hperg*usl)
  !write (*,40) deltae/evtoerg, q, qend
  !if ((qendfix/q).gt.(1.1d0))   print *, qendfix/q, deltae/evtoerg, q!;pause
  !if (q.eq.0) print *, 'a'
  !if (qendfix.eq.0) print *, 'b'
  !print *, qend/q
  ks=sqrt(energy*evtoerg*2*me*koefe)/hperg
  !write (*,40) q, deltae/evtoerg, energy, qendfix;pause
  
  if (res.eq.1) then
      energy=energy-deltae/evtoerg
      if (energy<0) then 
		print *, 'warning! energy =', energy
		energy=1d-7
	  end if
      ke=sqrt(k*k-(deltae*2*me*koefe)/hperg**2)
    !print *, deltae/evtoerg, '-'!;pause
  end if
  if (res.eq.2) then
    ke=sqrt(k*k+(deltae*2*me*koefe)/hperg**2)
    energy=energy+deltae/evtoerg
    !print *, deltae/evtoerg, '+'!;pause
  end if
  kx=ks*sin(teta)*cos(fi)
  ky=ks*sin(teta)*sin(fi)
  kz=ks*cos(teta)
  !print *, cos(teta), cos(fi);pause
  if (ks.eq.0) then
  ks=1d+2
  end if
  if (ke.eq.0) then
  ke=1d+2
  end if
  kostil=(ks*ks+ke*ke-q*q)/(2*k*ke)
  if (kostil>1) then !еще один костыль
    if (kostil > 1.5) then
       !print *, 'kostil >1', kostil, 'k, kend, q =' 
       !write (*,40) ks, ke, q
    end if
    kostil=1
  end if
  if (kostil<-1) then !еще один костыль
     if (kostil < -1.5) then
       !print *, 'kostil >1', kostil
    end if
     kostil=-1
  end if
  !beta=acos((k*k+ke*ke-q*q)/(2*k*ke))
  beta=acos(kostil)
  !g1=acos(kz/k)
  g1=teta
  g2=acos(ky/ks)
  g3=acos(kx/ks)
  !nu=acos(ky/sqrt(k*k-kz*kz))
  nu=acos(sin(fi))
  kx=ke*(cos(beta)*cos(g1)+sin(beta)*cos(fi)*sin(g1))
  ky=ke*(cos(beta)*cos(g2)-sin(beta)*cos(fi)*cos(g1)*cos(nu)-sin(beta)*sin(fi)*sin(nu))
  kz=ke*(cos(beta)*cos(g3)-sin(beta)*cos(fi)*sin(nu)*cos(g1)+sin(beta)*sin(fi)*cos(nu))
  !write(*,40) kz,ky,kz;pause
  if (kz.eq.0) then
    kz=1d+3
  end if
  if (kx.eq.0) then
    kx=1d+3
  end if
  teta=atan(sqrt(kx*kx+ky*ky)/kz)
	if (energy.gt.1d-2) then
		!print*,kx,ky,atan(ky/kx)
		!pause
	endif
  fi=atan(ky/kx)
  if (kx.lt.0) fi=fi+Pi   
  40  format(6(1x,e11.5))
  return
end
end module functions



  


