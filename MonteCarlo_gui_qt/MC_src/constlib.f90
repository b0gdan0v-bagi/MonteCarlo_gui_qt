module constlib
	implicit none
	REAL*8  :: PI = 3.14159265358d0
	REAL*8, PARAMETER  :: hplank = 6.582119d-16 !hplank / 2pi (ev*s)
	REAL*8, PARAMETER  :: hperg =1.053571d-27 !hplank / 2pi (erg)
	REAL*8, PARAMETER  :: evtoerg = 1.60218d-12 !ev to erg
	REAL*8, PARAMETER  :: kbol = 8.6173303d-5 !ev*k-1
	REAL*8, PARAMETER  :: kberg = 1.380648d-16 !erg*k-1
	REAL*8, PARAMETER  :: me = 9.10938d-28 !gramm
	
	REAL*8, PARAMETER  :: qe = 4.9032d-10 !electrone charge
	
	REAL*8  :: fonon = 0.037d0 !energy of fonon
	REAL*8  :: temp !temperature
	REAL*8  :: esound, sqes !energy of sound and sqrt(esound)
	REAL*8  :: optrateconst, sndrateconst, nqopt, smstoev!sm/s to ev (mv2/2) 

	character*30 :: material
	character(100) makedirectory, path
	character(8)  :: date
    character(10) :: time
	REAL*8  :: ro !crystal density
	REAL*8  :: dtk!deformation potential opt and intervalley scat with e (ev/sm)
	REAL*8  :: usl  !longitudinal sound velocity
	REAL*8  :: ust !transverse sound velocity
	REAL*8  :: defpot!electron acoustic-phonon deformation potential (eV)
	REAL*8  :: alp!non parab parametr
	REAL*8  :: eg!energy gap
	REAL*8  :: dielconst !dielecrtic constant
	REAL*8  :: mn !ge elec eff mass
	REAL*8  :: mh  !ge heavy holee eff mass
	REAL*8  :: ncon  !concentration of e
	REAL*8  :: koefe !koef electronemass 

	REAL*8  :: gamma = 1d+12 !max sum scat rate
	real*8  :: ranval
	REAL*8  :: optrateabs, optrateemi, soundrateabs, soundrateemi, optratekoef, dielconstsum, optRateAbsPolar, optRateEmiPolar
	REAL*8  :: ee_rate, ee_help
	REAL*8  energy, a, fi, teta !a for selection scate mechanism  
	REAL*8 :: tetatest, fitest, tetatest2, fitest2
	character*10  :: res
	real*8 limiterForce, fieldInput, fieldsinput(100), polarInput, kostilConstPolar, ee_collisions
	real*8 triangleCheckConfig, triangleCheckNumber, triangleCheckPause
	!REAL*8  testteta, testfi, testenergy
	REAL*8, PARAMETER :: eps = 1d-12
	REAL*8  :: deltaenergy = 1d-3
	integer, PARAMETER :: maxExNumbers = 150
	character*200 :: exouthelp(maxExNumbers),  exOutHelpSum, exOutHelpSumSemi(2), exOutHelpSumForMulti, THname
	REAL*8 :: impion = 0, auger = 0, resconst(maxExNumbers) = 0, et, intconst(maxExNumbers) = 0, nullresconst = 0, nullintconst
	real*8 Nreal
	integer s1, N, Nsave, mashtab, nout,  logcreate, kegl !kegl for good lines 
	real*8 :: outres(50000, maxExNumbers), outreshelp(maxExNumbers)
	real*8 :: outresTH(50000, maxExNumbers, 10), outresTHhelp(maxExNumbers, 10)
	real*8 :: outresSemi(50000, maxExNumbers, 2), outresSemihelp(maxExNumbers, 2)
	real*8 :: outresTHhelpOneFile(50000)
	!real*8 :: outres150(50000, maxExNumbers), outres180(50000, maxExNumbers), outres210(50000, maxExNumbers)
	!real*8 :: outres240(50000, maxExNumbers), outres270(50000, maxExNumbers), outres300(50000, maxExNumbers)
	!real*8 :: outreshelp150(50000), outreshelp180(50000), outreshelp210(50000)
	!real*8 :: outreshelp240(50000), outreshelp270(50000), outreshelp300(50000)
	real*8 :: FreeRun = 0
	integer exstart, exend, exnumbers, exinteger
	real*8 ex, timestart, timeend 
	REAL*8 :: ux, uy, uz, avgspeed=0, t=0, uxsmena=0, tsum, avgspeedY, avgspeedZ, uysmena, uzsmena
	REAL*8 :: energyfound = 0, speed = 0, avgenergy = 0, maxenergy = 0, energysmena = 0
	real*8 :: mashtabout=1d-3
	integer :: act=0
	integer :: global
	
	integer, PARAMETER :: NumberPolarColumnConst = 100 ! const
	integer :: NumberPolarColumn
    real*8 :: PartOfPolarSpeed(50000, maxExNumbers) = 0, avgPolarSpeed, PolarSpeedOut, TetaResOut
    real*8 :: PartOfPolarSpeedhelp(maxExNumbers) = 0
	
	

	character*200 fnamestart,fnameend, fname, fnamerasp, timecheck
	! angle func global parameters
	real*8 q, deltae, qend, dq, iq
	real*8 kx, ky, kz, ks, ke, kxe, kye, kze
	real*8 betatest, beta, g1, g2, g3, nu
	real*8 trianglecheck
	real*8 qendres,c, qendfix,qstart
	integer :: test=0
contains
subroutine inicProcedure()
	implicit none
	PI = 4.D0 * DataN (1.D0)
	CALL RANDOM_SEED()
	!optratekoef=1650
	optratekoef=2000
	select case (material)
		case ('ge')
			call inicGe()
		case ('si')
			call inicSi()
		case ('inas')
			call inicInAs()
		case ('ingaas')
			call inicInGaAs()
		case ('insb')
			call inicInSb()
		case ('cdhgte')
			call iniccdhgte()
			
		case DEFAULT
			print *, 'error material!!'
			pause
	end select
	dtk = sqrt(ro)*1.8d+8 !deformation potential opt and intervalley scat with e (ev/sm)
	!print *, dtk;pause
	esound = me*koefe*usl**2/(2*evtoerg)
	sqes = sqrt(esound)
	optrateconst = (dtk*evtoerg)**2*(me*koefe)**1.5d0/(1.4142*pi*hperg**2*ro*fonon*evtoerg)
	!!!!!!!!
	
	!optrateconst = optrateconst * optratekoef
	
	!!!!!
	
	sndrateconst = (me*koefe)**0.5d0*(kberg*temp)**3*(defpot*evtoerg)**2/(2**2.5d0*pi*hperg**4*usl**4*ro)
	!print *, sndrateconst; pause
	nqopt = 1/(exp(fonon/(kbol*temp))-1)
	smstoev = me*koefe/(2*evtoerg)
	ncon = 5d+14 !concentration of e
	call date_and_time(DATE=date)
    call date_and_time(TIME=time)
	path = 'res_' // trim(material) // '_' // trim(date) // '_' // trim(time)
	makedirectory = 'mkdir ' // trim(path)
	call system(makedirectory)
	!print *, optrateconst,sndrateconst,nqopt
end
subroutine inicGe()
	implicit none
	ro = 5.32 !crystal density
	!dtk = 5.5d+8 !deformation potential opt and intervalley scat with e (ev/sm)
	usl = 9.0d+5 !longitudinal sound velocity
	!ust = 5.3d+5 !transverse sound velocity
	defpot = 9.0 !electron acoustic-phonon deformation potential (eV)
	alp = 0.3d0 !non parab parametr
	eg=0.742d0-(4.8d-4)*temp**2/(temp+235)!energy gap
	dielconst=16.2 !dielecrtic constant
	!mn = 0.02 !ge elec eff mass
	mh = 0.45 !ge heavy holee eff mass
	koefe = 0.2 !koef electronemass 
	fonon = 0.037d0 !energy of fonon
end
subroutine inicSi()
	implicit none
	ro = 2.33 !crystal density
	!dtk = 0.5d+8 !deformation potential opt and intervalley scat with e (ev/sm)
	usl = 9.0d+5 !longitudinal sound velocity
	!ust = 5.3d+5 !transverse sound velocity
	defpot = 9.0 !electron acoustic-phonon deformation potential (eV)
	alp = 0.5d0 !non parab parametr
	eg=1.17d0-(4.73d-4)*temp**2/(temp+636) !energy gap
	dielconst=11.7 !dielecrtic constant
	!mn = 0.08 !ge elec eff mass
	mh = 0.53 !ge heavy holee eff mass
	koefe = 0.19 !koef electronemass
	fonon = 0.063d0 !energy of fonon 
end	
subroutine inicInAs()
	implicit none
	!real*8 :: c1=8.34,c2=4.54,c3=3.95, a=5.2,b=-1.8,d=-3.6
	ro = 5.68 !crystal density
	!dtk = 0.5d+8 !deformation potential opt and intervalley scat with e (ev/sm)
	usl = 3.83d+5 !longitudinal sound velocity
	!ust = 5.3d+5 !transverse sound velocity
	defpot = 4.9 !electron acoustic-phonon deformation potential (eV)
	alp = 2.7d0 !non parab parametr
	!test
	!print *, 'test usl', usl, sqrt((3*c1+2*c2+4*c3)*10d11/(5*ro)); pause
	!print *, 'privet'
	eg=0.415d0-(2.76d-4)*temp**2/(temp+83) !energy gap
	!print *, eg
	dielconst=15.5!dielecrtic constant
	!mn = 0.08 !ge elec eff mass
	mh = 0.41 !ge heavy holee eff mass
	koefe = 0.023 !koef electronemass
	fonon = 0.030d0 !energy of fonon 
end	
subroutine inicInGaAs()
	implicit none
	real x
	print *, 'In(1-x)Ga(x)As. Write x!'
	x=0.47 ;print*, x;!read (*,*) x
	if (x>1.or.x<0) then
		print *, 'Error! wrong x!!!!!!!!!!!!'
		pause
	end if
	ro = 5.68-0.37*x !crystal density
	usl = (3.83+0.90*x)*10d+5 !longitudinal sound velocity
	!ust = 5.3d+5 !transverse sound velocity
	defpot = 7 !electron acoustic-phonon deformation potential (eV)
	 !non parab parametr !взял такой-же
	!eg=0.42d0-(2.76d-4)*temp**2/(temp+83) !energy gap
	eg = 0.42 + 0.625*x-(5.8/(temp+300)-4.19/(temp+271))*10d-4*temp**2*x-4.19*10d-4*temp**2/(temp+271) +0.475*x**2
	print *, 'InGaAs eg = ', eg
	!eg=0.35
	dielconst=15.1-2.87*x+0.67*x**2 !dielecrtic constant
	!mn = 0.08 !ge elec eff mass
	mh = 0.41+0.1*x !ge heavy holee eff mass
	koefe = 0.023+0.037*x+0.003*x**2 !koef electronemass
	alp = (1-koefe)**2/eg
	fonon = 0.030d0 !energy of fonon 
end	
! не сделанно ниже ничего
subroutine inicInSb()
	implicit none
	ro = 5.77 !crystal density
	!dtk = 0.5d+8 !deformation potential opt and intervalley scat with e (ev/sm)
	usl = 4.79d+5 !longitudinal sound velocity
	!ust = 5.3d+5 !transverse sound velocity
	defpot = 6.5 !electron acoustic-phonon deformation potential (eV)
	alp = 5.6d0 !non parab parametr
	eg=0.24d0-(6d-4)*temp**2/(temp+500) !energy gap
	!eg=0.35
	dielconst=16.8 !dielecrtic constant
	!mn = 0.08 !ge elec eff mass
	mh = 0.43 !ge heavy holee eff mass
	koefe = 0.014 !koef electronemass
	fonon = 0.025d0 !energy of fonon 
end	
!fonon? alp!?!??!?
subroutine inicCdHgTe()
	implicit none
	real x
	print *, 'Hg(1-x)Cd(x)Te. Write x!'
	x=0.21 ;print*, x;!read (*,*) x
	if (x>1.or.x<0) then
		print *, 'Error! wrong x!!!!!!!!!!!!'
		pause
	end if
	ro = 5.68 !crystal density
	!dtk = 0.5d+8 !deformation potential opt and intervalley scat with e (ev/sm)
	usl = 3.01d+5 !longitudinal sound velocity
	!ust = 5.3d+5 !transverse sound velocity
	!defpot = 4.9 !electron acoustic-phonon deformation potential (eV)
	defpot = 14
	!alp = 2.27d0 !non parab parametr
	eg=-0.302+1.93*x-0.81*x**2+0.832*x**3+5.35d-4*(1-2*x)*temp !energy gap
	!print *, eg;pause
	!eg=0.35
	dielconst=17.7 !dielecrtic constant
	!mn = 0.08 !ge elec eff mass
	mh = 0.55 !ge heavy holee eff mass
	koefe = 2.3d-5*temp+0.00454 !koef electronemass (line approx)
	alp = (1-koefe)**2/eg
	fonon = 0.030d0 !energy of fonon 
end	

	
end module constlib