module anglefunc
contains
subroutine scaterate2()
40  format(6(1x,e11.5))
use math
use constlib
implicit none
real*8 argplus, argminus, argminussnd, soundrateamp, summrate
    argplus=4*sqes*(sqes+sqrt(energy))/(kbol*temp)
    argminus=4*sqes*(sqes-sqrt(energy))/(kbol*temp)
    argminussnd=4*sqes*(sqrt(energy)-sqes)/(kbol*temp)
    if (energy > fonon) then
      !optrateemi = optrateconst*(nqopt+1)*sqrt((energy-fonon)*evtoerg)
      !here non-parabolic
      optrateemi = optrateconst*(nqopt+1)*(1+2*alp*(energy-fonon))*sqrt((energy-fonon)*evtoerg*(1+alp*(energy-fonon)))
    else
      optrateemi = 0
    end if
    !optrateabs = optrateconst*nqopt*sqrt((energy+fonon)*evtoerg)
    !here non-parabolic
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
    summrate=optrateabs+optrateemi+soundrateabs+soundrateemi
    !summrate=soundrateabs+soundrateemi
    if (summrate.gt.gamma) then
		print *, 'warning, rates > gamma at energy = ', energy
		write (*, 40) summrate, gamma
		!pause
	end if
end

subroutine scaterateres()
use math
use constlib
implicit none
40  format(6(1x,e11.5))
call scaterate()
!optrateabs=0; optrateemi=0
CALL random_number(ranval)
a=ranval*gamma
res='none'
   if ((a.le.soundrateemi).and.(a.ge.0)) then
      res = 'sndemi'
      call  anglesound()
      !call angleoptic()
   end if
   if ((a.le.soundrateemi+soundrateabs).and.(a.gt.soundrateemi)) then
      res = 'sndabs'
      call anglesound()
     !call angleoptic()
   end if
   if ((a.le.soundrateemi+soundrateabs+optrateemi).and.(a.gt.soundrateemi+soundrateabs)) then
      res = 'optemi'
      if (polarInput.eq.1) then
		call anglepolaropt()
	  else
		call angleoptic()
	  end if
     ! energy = energy - fonon
   end if
   if ((a.le.soundrateemi+soundrateabs+optrateemi+optrateabs).and.(a.gt.soundrateemi+soundrateabs+optrateemi)) then
      res = 'optabs'
      if (polarInput.eq.1) then
		call anglepolaropt()
	  else
		call angleoptic()
	  end if
     !energy = energy + fonon
   end if
	ee_help=soundrateemi+soundrateabs+optrateemi+optrateabs
	if (ee_collisions.eq.0) then
		ee_rate=0
	end if
   if ((a.le.(ee_help+ee_rate)).and.(a.gt.ee_help)) then
      res = 'ee'
	  energy=0.5*energy
	  call angleoptic()
   end if
   if (a.gt.soundrateemi+soundrateabs+optrateemi+optrateabs+ee_rate) then
      res = 'selfsc'
   end if
   If (energy.lt.0) then
      print *, 'energy < 0 !!!'
      energy = 1d-15
   end if
end

subroutine angleoptic()
	use math
	use constlib
	call random_number(ranval)
	teta = acos(1-2*ranval)
	call random_number(ranval)
	fi = 2*PI*ranval
	
end

subroutine anglesound()
  40  format(6(1x,e11.5))
  use math
  use constlib
  implicit none
  integer :: i, j, k
  q=0
  CALL random_number(ranval)
  if (res.eq.'sndemi') then
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
  if (res.eq.'sndabs') then
     if (energy < esound) then
       qend=4*sqes*(sqes+sqrt(energy))/(kbol*temp)
       q=4*sqes*(sqes-sqrt(energy))/(kbol*temp)
       !print *, 'absrob, energy < esound'
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
		!
    else
    q=0.0
    end if
  end if
  q=q*temp*kberg/(hperg*usl)
  deltae=hperg*q*usl
  qendfix = qendfix*temp*kberg/(hperg*usl)
  ks=sqrt(energy*evtoerg*2*me*koefe)/hperg
  if (res.eq.'sndemi') then
      energy=energy-deltae/evtoerg
      if (energy<0) then 
		print *, 'warning! energy =', energy
		energy=1d-7
		pause;
	  end if
      ke=sqrt(ks*ks-(deltae*2*me*koefe)/hperg**2)
  end if
  if (res.eq.'sndabs') then
    ke=sqrt(ks*ks+(deltae*2*me*koefe)/hperg**2)
    energy=energy+deltae/evtoerg
  end if
  call changeAngle()
end

subroutine scaterate()
40  format(6(1x,e11.5))
use math
use constlib
implicit none
real*8 argplus, argminus, argminussnd, soundrateamp, summrate
real*8 :: wlo=3d13, x, y, ek, optrateconstpolar
	argplus=4*sqes*(sqes+sqrt(energy))/(kbol*temp)
    argminus=4*sqes*(sqes-sqrt(energy))/(kbol*temp)
    argminussnd=4*sqes*(sqrt(energy)-sqes)/(kbol*temp)
	fonon = hperg*wlo/evtoerg
	nqopt = 1/(exp(fonon/(kbol*temp))-1)
    !optrateconstpolar=sqrt(me*koefe*wlo/2.0)*qe**2/(hperg**1.5d0*dielconstsum)
    !here was main mistake dir by zero
    optrateconstpolar=sqrt(me*koefe*wlo/2.0)*qe**2/(hperg**1.5d0*dielconst)
    
    !!! kostilConst
    
    optrateconstpolar=optrateconstpolar*kostilConstPolar
	if (energy.eq.0) then
		energy=1d-6
		print *, 'warning energy = 0'
    end if
	optRateAbsPolar=optrateconstpolar*nqopt*log(abs((sqrt(energy)+sqrt(energy+fonon))/((sqrt(energy)-sqrt(energy+fonon)))))
	optRateAbsPolar=optRateAbsPolar*sqrt(fonon/energy)
	if (energy.gt.fonon) then
		optRateEmiPolar=optrateconstpolar*(nqopt+1)*log(abs((sqrt(energy)+sqrt(energy-fonon))/((sqrt(energy)-sqrt(energy-fonon)))))
		optRateEmiPolar=optRateEmiPolar*sqrt(fonon/energy)
	else
		optRateEmiPolar=0
	end if
	optrateabs=optRateAbsPolar
	optrateemi=optRateEmiPolar
	!print *, dielconstsum;pause
	!write (*,40) energy, fonon, optrateemi; pause
    soundrateamp=sndrateconst/sqrt(energy*evtoerg)
   ! print *, soundrateamp;pause
    if (energy < esound) then
       soundrateabs = soundrateamp*(fone(argplus)-fone(argminus))
       soundrateemi = 0
    else
       soundrateabs = soundrateamp*fone(argplus)
       soundrateemi = soundrateamp*gone(argminussnd)
    end if
    !print *, soundrateabs,soundrateemi;pause
    summrate=optrateabs+optrateemi+soundrateabs+soundrateemi
    !write(*,40) summrate
    !summrate=soundrateabs+soundrateemi
    if (summrate.gt.gamma) then
		print *, 'warning, rates > gamma at energy = ', energy
		write (*, 40) summrate, gamma
		gamma = summrate * 1.1d0
		print *, 'new gamma = '
		write (*, 40) gamma
		!pause
	end if
end

subroutine anglepolaropt()
  40  format(6(1x,e11.5))
  use math
  use constlib
  implicit none
  integer :: i, j, k
	CALL random_number(ranval)
	if (res.eq.'optemi') then
		if (energy.lt.fonon) then
			qend=0d0
			q=0d0
			stop
		else
			qend=1+sqrt(1-fonon/energy)
			q=1-sqrt(1-fonon/energy)
			q=q*(qend/q)**ranval
		end if
   	end if
	if (res.eq.'optabs') then
		qend=1+sqrt(1+fonon/energy)
		q=-1+sqrt(1+fonon/energy)
		qstart=q
		q=q*(qend/q)**ranval
   	end if
	ks=sqrt(energy*evtoerg*2*me*koefe)/hperg
	q=q*ks
	if (res.eq.'optemi') then
		ke=ks*sqrt(1-fonon/energy)
		energy=energy-fonon
	end if
	if (res.eq.'optabs') then
		ke=ks*sqrt(1+fonon/energy)
		energy=energy+fonon
	end if
	call changeAngle()
end

subroutine changeAngleOriginal()
	40  format(6(1x,e11.5))
	use math
	use constlib
	implicit none
	kx=ks*sin(teta)*cos(fi)
	ky=ks*sin(teta)*sin(fi)
	kz=ks*cos(teta)
	if (ks.eq.0) ks=1d+2
	if (ke.eq.0) ke=1d+2
	trianglecheck=(ks*ks+ke*ke-q*q)/(2*ks*ke) !check triangle
		if (abs(trianglecheck)>1) then !not a triangle
			print *, res
			print *, 'trianglecheck not done, trianglecheck', trianglecheck, 'k, kend, q =' 
			write (*,40) ks, q, ke
			write (*,40) energy, deltae
			write (*,40) kx, ky, kz, ks, ke
			write (*,40) teta, fi
			write (*,40) fonon, sqrt(1+fonon/(energy-fonon))
			write (*,40) energy, energy-fonon
			write (*,40) qend, qstart*ks, ranval
			pause
			trianglecheck=1
		end if
	beta=acos(trianglecheck)
	betatest=betatest+beta
	g1=acos(kz/ks)
	g2=acos(ky/ks)
	g3=acos(kx/ks)
	nu=acos(sin(fi))
	kx=ke*(cos(beta)*cos(g1)+sin(beta)*cos(fi)*sin(g1))
	ky=ke*(cos(beta)*cos(g2)-sin(beta)*cos(fi)*cos(g1)*cos(nu)-sin(beta)*sin(fi)*sin(nu))
	kz=ke*(cos(beta)*cos(g3)-sin(beta)*cos(fi)*sin(nu)*cos(g1)+sin(beta)*sin(fi)*cos(nu))
	if (kz.eq.0)kz=1d+3
	if (kx.eq.0)kx=1d+3
	teta = acos(kz/sqrt(kx*kx + ky*ky + kz*kz))
	fi=atan(ky/kx)
	if (kx.lt.0) fi=fi+Pi
	if (fi.lt.0) fi=fi+2*Pi 
end

subroutine changeAngle()
	40  format(6(1x,e11.5))
	use math
	use constlib
	implicit none
	real*8 be, fiS, kxStart, kyStart, kzStart
	kx=ks*sin(teta)*cos(fi)
	ky=ks*sin(teta)*sin(fi)
	kz=ks*cos(teta)
	kxStart=kx;kyStart=Ky; kzStart=kz
	if (ks.eq.0) ks=1d+2
	if (ke.eq.0) ke=1d+2
	trianglecheck=(ks*ks+ke*ke-q*q)/(2*ks*ke) !check triangle
		if (triangleCheckConfig.eq.1) then
		if (abs(trianglecheck)>triangleCheckNumber) then !not a triangle
			print *, res
			print *, 'trianglecheck not done, trianglecheck', trianglecheck, 'k, kend, q =' 
			print *, "ks, q, ke"
			write (*,40) ks, q, ke
			print *, "energy deltae"
			write (*,40) energy, deltae
			print *, "kx, ky, kz, ks, ke"
			write (*,40) kx, ky, kz, ks, ke
			print *, "teta, fi"
			write (*,40) teta, fi
			print *, "fonon, sqrt(1+fonon/energy-fonon)"
			write (*,40) fonon, sqrt(1+fonon/(energy-fonon))
			print *, "energy, energy-fonon"
			write (*,40) energy, energy-fonon
			print *, "qend, qstart*ks, ranval"
			write (*,40) qend, qstart*ks, ranval
			
			if (triangleCheckPause.eq.1) then
				pause
			end if
			trianglecheck=1
		end if
		end if
	CALL random_number(ranval)
	beta=acos(trianglecheck)
	be=trianglecheck
	betatest=betatest+beta
	g1=kz/ks
	g2=ky/ks
	g3=kx/ks
	!nu=ky/sqrt(ks*ks-kz*kz)
	fiS=fi !fi start
	CALL random_number(ranval)
	fi=2*pi*ranval
	nu=sin(fiS)
	!kx=ke*(be*g1+sqrt(1-be**2)*cos(fi)*sqrt(1-g1**2))
	!ky=ke*(be*g2-sqrt(1-be**2)*cos(fi)*g1*nu-sqrt(1-be**2)*sin(fi)*sqrt(1-nu**2))
	!kz=ke*(be*g3-sqrt(1-be**2)*cos(fi)*sqrt(1-nu**2)*g1+sqrt(1-be**2)*sin(fi)*nu)
	!kx=ke*(be*g1+sqrt(1-be**2)*cos(fiS)*sqrt(1-g1**2))
	!ky=ke*(be*g2-sqrt(1-be**2)*cos(fiS)*g1*nu-sqrt(1-be**2)*sin(fiS)*sqrt(1-nu**2))
	!kz=ke*(be*g3-sqrt(1-be**2)*cos(fiS)*sqrt(1-nu**2)*g1+sqrt(1-be**2)*sin(fiS)*nu)
	kx=ke*(be*cos(fiS)*sin(teta)-sqrt(1-be**2)*sin(fi)*sin(fiS)+sqrt(1-be**2)*cos(fi)*cos(fiS)*cos(teta))
	ky=ke*(sqrt(1-be**2)*cos(fiS)*sin(fi)+cos(beta)*sin(fiS)*sin(teta)+sqrt(1-be**2)*cos(fi)*sin(fiS)*cos(teta))
	kz=ke*(be*cos(teta)-sin(beta)*cos(fi)*sin(teta))
	if (kz.eq.0)kz=1d+3
	if (kx.eq.0)kx=1d+3
	teta = acos(kz/sqrt(kx*kx + ky*ky + kz*kz))
	fi=atan(ky/kx)
    if (kx.lt.0) fi=fi+Pi
    if (fi.lt.0) fi=fi+2*Pi
    
    !ke=sqrt(kx*kx+ky*ky+kz*kz)
    !print *, beta, acos((ks*ks+ke*ke-q*q)/(2*ks*ke)); pause
    
    tetatest2=tetatest2+teta; fitest2=fitest2+fi
end

end module anglefunc
