module maincount
contains
subroutine mcinfield()
	30  format(6(1x,e11.5))
	use constlib
	use anglefunc
	implicit none
	integer :: i, j, k
	ux=1d+2;uy=1d+2;uz=1d+2 ! чтоб на нуль не делить
	teta=0;fi=0;tsum=0
	impion=0;auger=0;resconst=0   
	avgspeed=0;avgspeedY=0;avgspeedZ=0;uxsmena=ux; uysmena=uy;uzsmena=uz
	tetatest=0;fitest=0; tetatest2=0; fitest2=0
	speed=0;act=0;energy=0;energysmena=0;s1=0
	freerun=0;
	!avgPolarSpeed=0 ! NEW NEW NEW
	
    DO i = 1, N, 1
		if (i.eq.N/2) print *, '*** 50 percent done ***'
		
        CALL random_number(ranval)
	    t=-log(ranval)
	    tsum=tsum+t
		!ux = ux + ex*qe*t/(300*me*koefe*gamma) 
		if (limiterForce.eq.0) then
			uz = uz + ex*qe*t/(300*me*koefe*gamma) 
			!print*, "hello"; pause
		else
			if (energy < limiterForce) then
				uz = uz + ex*qe*t/(300*me*koefe*gamma) 
			else
				energy = 0.0001
			end if
		end if
      	!speed = (i*speed+ux)/(i+1)
      	speed = (i*speed+uz)/(i+1)
        energy = smstoev*(ux*ux + uy*uy + uz*uz)
		if (i.eq.1) energy = 2d-6
		!if (ex.ne.0) then
			teta = acos(uz/sqrt(ux*ux + uy*uy + uz*uz))
			fi=atan(uy/ux)
			if (ux.lt.0) fi=fi+Pi
			if (fi.lt.0) fi=fi+2*Pi
		!end if
        if (teta<0.or.teta>pi) then
			print *, 'warning, teta=', teta; pause
		end if
		if (fi<0.or.fi>2*pi) then
			print *, 'warning, fi=',  fi; pause
		end if         
      	avgspeed = avgspeed + ((ux + uxsmena)*t / 2)
      	avgspeedY = avgspeedY + ((uy + uysmena)*t / 2)
      	avgspeedZ = avgspeedZ + ((uz + uzsmena)*t / 2)
      	
    	avgenergy = avgenergy+(energy+energysmena)/2
    	freerun= freeRun + sqrt(ux*ux+uy*uy+uz*uz)*t/gamma
    	    	  	
	    call scaterateres()
		ux=(sqrt(energy/smstoev))*sin(teta)*cos(fi)
		uy=(sqrt(energy/smstoev))*sin(teta)*sin(fi)
		uz=(sqrt(energy/smstoev))*cos(teta)
		
		! NEW
		!PolarSpeedOut = sqrt(ux**2+uy**2+uz**2)
		!AvgPolarSpeed = AvgPolarSpeed + PolarSpeed
    	!print*, NumberPolarColumnConst; pause
    	!NumberPolarColumn = int(NumberPolarColumnConst*2*pi / teta)
    	!print*, teta, NumberPolarColumnConst*2*pi / teta, NumberPolarColumn; pause
    	!PartOfPolarSpeed(NumberPolarColumn, global) = PartOfPolarSpeed(NumberPolarColumn, global) + 1
		
		
		if (res.ne.'selfsc') act = act + 1
		uxsmena = ux; uysmena=uy; uzsmena=uz
		tetatest=tetatest+teta; fitest=fitest+fi
	    energysmena = energy
	    if (energy > maxenergy) maxenergy = energy !!!!!!!!!!!!!!!!!!!
  	    energyfound = energy/mashtabout
	 ! OLD OLD OLD OLD RASPRED
	    s1=int(energyfound)+1
	    if ((s1 <= nout).and.(s1>0).and.(global<150)) THEN
			!print *, global; pause
	    	outres(s1, global) = outres(s1, global) + t
	    	if (cos(teta)<0) then
	    		outresSemi(s1, global, 1) = outresSemi(s1, global, 1) + t
	    	else 
	    		outresSemi(s1, global, 2) = outresSemi(s1, global, 2) + t
	    	end if
	    	!outres(s1, global) = outres(s1, global) + 1
	    END IF
		!if (energy > 0.12) then
		!	s1=int((cos(teta)+1)*100)+1
			!s1=int(teta*100)+1
			!print *, teta, cos(teta)+1; pause
		!	if ((s1 <= nout).and.(s1>0)) THEN
		!		outres(s1, global) = outres(s1, global) + 1
		!	END IF
		!end if
		if (ux.lt.(sqrt(ux*ux+uy*uy+uz*uz))) then
		do j = 1, 6
			if ((energy > (j*0.05)).and.(energy < ((j+1)*0.05)) ) then
				!s1=int((cos(teta)+1)*100)+1
				s1=int((cos(fi)+1)*100)+1
				if ((s1 <= nout).and.(s1>0)) THEN
					outresTH(s1, global, j) = outresTH(s1, global, j) + t
				END IF
			end if
			!print *, 0.15+j*0.03;pause
		end do
		end if
		
	    
		IF (logcreate == 1) THEN
		    !if (res.ne.'selfsc') write(2,30) res, energy, energy-energysmena, real(i)!ux, uy, uz
		ENDIF
		END DO
end 

subroutine fields(exstart, exend, exnumbers)
	30 format(6(1x,e11.5))
	40 format(40a1)
	use constlib
	use anglefunc
	use logMake
	implicit none
	integer :: i, j, k
	integer :: exstart,exend,exnumbers
	integer :: ios
	timestart = secnds(0.0)
	write(fnamestart,'(I0)') exstart
	write(fnameend,'(I0)') exend
	fname=trim(path) // '/speed_' // trim(fnamestart) //  '_' // trim(fnameend) // '.txt'
	open(3, file= fname, status="unknown", iostat =IOS)
	open(2, file= "log.txt", status="unknown", iostat =IOS)
	fname=trim(path) // '/impion_' // trim(fnamestart) //  '_' // trim(fnameend) // '.txt'
	open(500, file = fname, status="unknown", iostat =IOS)
	fname = trim(path) // '/log.txt'
	open(501, file= fname, status="unknown", iostat =IOS)
	call inputLog()
	if (IOS /=0) then
		print*, "error open file! "
		stop
	end if
	if (exend.ne.0) then
		global = 1
		ex=exend
		Nsave=N
		N=N/10 ! потом надо бы изменить
		!print *, ex, real(N)
		call mcinfield()
		energy=maxenergy
		call scaterate()
		call gammaLog()
		gamma=6*(optrateabs+optrateemi+soundrateabs+soundrateemi) !6 for incrising gamma
		outres=0;outreshelp=0;outresTH=0;outresTHhelp=0;outresSemi=0;outresSemihelp=0;
		N=Nsave
		
	end if
	DO k = 1, exnumbers
		global=k !k is not global var
		if (fieldInput.eq.1) then
			ex=fieldsInput(k)
		else
			if (k.eq.1) then
				ex=0
			else
				ex=(k-1)*(exend - exstart) /((exnumbers-1)*1d0) + exstart
			end if
		end if
		!call exponentVarField() !задает поле в строгую последовательность :0,500,1000,5000,10000
		write(exouthelp(global),'(I0)') int(ex) !for output
		call beforeMainCountLog()
		!write(*, 40) ('_', kegl = 1, 40)
		!write(*, 40) ('*', kegl = 1, 40) !выводим до, чтобы визуально оценить время
		!print *, "ex =", ex
		!main counsts here!
		call mcinfield()
		avgenergy = avgenergy / (N+1)
		avgspeed = avgspeed / tsum
		freeRun = freeRun / (N+1)
		!avgPolarSpeed = avgPolarSpeed / (N+1) ! for future
		
		call afterMainCountLog()
		WRITE (3,'(2x, f11.5, 2(2x, e11.5))') ex,  avgspeedZ
		!if (exnumbers>8) call outputSinglefunk() !тут можно писать в сингл файлы, но зачем?
		!call outputSinglefunk() !тут можно писать в сингл файлы, но зачем?

		!!!! main imp ion calculate
		call impioncalc()
	end do
	!PartOfPolarSpeed=PartOfPolarSpeed/(N+1) ! NEW. Normalize
	call outputallfunk()
	call timeCountLog()
	
	close(3)
	close(2)
	close(500)
	close(501)
end

subroutine impioncalc()
	30 format(6(1x,e11.5))
	40 format(40a1)
	use constlib
	implicit none
	integer :: i, j, k
	!eg=0.1 !!! for test
	et=(2*koefe/mh+1)*eg		
	auger = (2*PI)**2.5d0*hperg**3*avgenergy*evtoerg*qe**4*exp(-2*koefe*eg/(mh*kbol*temp))
	write (*, '(2x, a, e11.5)') 'test, exp ', exp(-2*koefe*eg/(mh*kbol*temp)) !test
	auger = auger / (2*(koefe*me)**0.5d0*dielconst**2*(eg*evtoerg)**2.5d0*(mh*me*kberg*temp)**1.5d0)
	impion = (koefe*me)**2.5d0*qe**4/(4*pi**2*hperg**6*dielconst**2*(eg*evtoerg)**2.5d0)
	intconst(global)=0
	do i=1, nout
		intconst(global)=intconst(global)+outres(i, global)*(mashtabout*evtoerg)
	end do
		!intconst=intconst*evtoerg*sqrt(2d0)*(me*koefe)**1.5d0/(pi**2*hperg**3)
	intconst(global)=ncon/intconst(global)
	write (*, '(2x, a, e11.5)') 'const integreal ', intconst(global)
	resconst(global)=0    
	energy=et
	i=int(et/mashtabout)
	print *, 'i', i
	if (energy>maxenergy) then
		write(*, 40) ('=', kegl = 1, 10)
		print *, 'warning!, et > maxenergy, ionization == 0', et, maxenergy
		write(*, 40) ('=', kegl = 1, 10)
	end if
	do while ((energy<100).and.(i<50000))
		resconst(global)=resconst(global)+evtoerg**3*((energy-et)**3*outres(i, global)+(energy-et+1d-3)**3*outres(i+1, global))
		energy = energy + mashtabout
		i = i + 1
		!if (outres(i, global).ne.0) print*,'->>>>>>>>>>>>>>>>>>',outres(i, global)
	end do
	resconst(global)=resconst(global)*intconst(global)*(mashtabout*evtoerg)/2d0
	resconst(global)=(resconst(global)-resconst(1))/(sqrt(et*evtoerg)*sqrt(2d0)*(me*koefe)**1.5d0/(pi**2*hperg**3))
	if (global.ne.1) then
		print *, 'auger, imp, intregal'
		write (*, 30) auger, impion!, resconst
		write(*, 40) ('_', kegl = 1, 40)
		resconst(global) = impion*resconst(global)/(auger*ncon**2)
		write (*, 30) resconst(global)/ncon
		WRITE (500,'(2x, f11.5, 2x, e11.5)') ex, resconst(global)/ncon 
	end if 
end

subroutine outputallfunk()
	40 format(40a1)
	use constlib
	use gnumaker
	implicit none
	integer, parameter :: tennull = 10
	integer :: i,j,k, ze=0, noutsave, va
	! FOR MULTI 
	!exOutHelpSum= trim(path) // '/distribFunc_' 
	exOutHelpSum= 'distribFunc_' 
	!print*, exOutHelpSum;pause 
	if (exnumbers < 8) then
		do i=1, exnumbers
			exOutHelpSum=trim(exOutHelpSum) // trim(exouthelp(i)) // '_'
			!print*, exOutHelpSum;pause 
		end do
	else
		exOutHelpSum=trim(exOutHelpSum) // 'start=' // trim(exouthelp(1)) // '_end=' // trim(exouthelp(exnumbers)) // '_'
	end if
	!print *, exouthelpsum
	exOutHelpSumSemi(1) = trim(exOutHelpSum) // '_semi_1.txt'
	exOutHelpSumSemi(2) = trim(exOutHelpSum) // '_semi_2.txt'
	exOutHelpSum=trim(exOutHelpSum) // '.txt'
	
	write(*, 40) ('_', kegl = 1, 40) 
	!! cutt null with output
	noutsave=nout
	do i = 1, noutsave
		va = outres(i, exnumbers)
		if (va.eq.0) then
			ze=ze+1
			if (ze>tennull) then
				exit
			else
				cycle
			end if
		end if
	end do
	noutsave=i
	call DistributionFuncData() !for Gnuplot, before data file for sure path
	call DistributionFuncDataSemi()	
	exOutHelpSum = trim(path) // '/' // trim(exOutHelpSum)
	exOutHelpSumSemi(1) = trim(path) // '/' // trim(exOutHelpSumSemi(1))
	exOutHelpSumSemi(2) = trim(path) // '/' // trim(exOutHelpSumSemi(2))
	open(100, file=exOutHelpSum)!, status="unknown")
	open(99, file=exOutHelpSumSemi(1))
	open(98, file=exOutHelpSumSemi(2))
		
	exOutHelpSumForMulti = exOutHelpSum

	do i = 1, noutsave
		energy=i*mashtabout
		do j=1, exnumbers
			!dont forget to call back
			outreshelp(j)=outres(i,j)!/sqrt(energy)
			outresSemihelp(j,1)=outresSemi(i,j,1)
			outresSemihelp(j,2)=outresSemi(i,j,2)
			
		end do 
		write(100, '(2x, f11.5, 9(2x, e11.5))') energy, outreshelp(:exnumbers) ! eto bil default
		write(99, '(2x, f11.5, 9(2x, e11.5))') energy, outresSemihelp(:exnumbers, 1) ! eto bil default
		write(98, '(2x, f11.5, 9(2x, e11.5))') energy, outresSemihelp(:exnumbers, 2) ! eto bil default
	end do
	close(100)
	
	!do i = 1, 314
	do k=1, 6
		write(THname,'(I0)') 50*k
		exOuthelpsum= trim(path) // '/' // '_energy_' // trim(THname) // '.txt'
		open(101, file=exOutHelpSum)
		!exOuthelpsum= trim(path) // '/' // '_energy_50-200_delta_50.txt'
		!open(100, file=exOutHelpSum)
		do i = 1, 200
			tetaresout=i*0.01-1
			!tetaresout=i*0.01
		
			do j=1, exnumbers
				!outreshelp(j)=outres(i,j)!/sqrt(energy)
				outresTHhelp(j, k)=outresTH(i,j,k)
				if (j.eq.exnumbers) then
					outresTHhelpOneFile(k)=outresTH(i,j,k)
					!print*, outresTHhelp(j, k), outresTHhelpOneFile(k);pause
				end if
			end do
			
			write(101, '(2x, f11.5, 9(2x, e11.5))') tetaresout, outresTHhelp(:exnumbers, k)
			!write(100, '(2x, f11.5, 9(2x, e11.5))') tetaresout, outresTHhelpOneFile(:k)
			!write(*, '(2x, f11.5, 9(2x, e11.5))') tetaresout, outresTHhelp(:exnumbers, k)
			!write(*, '(2x, f11.5, 9(2x, e11.5))') tetaresout, outresTHhelpOneFile(:k)
			!pause
			!write(100, '(2x, f11.5, 9(2x, e11.5))') tetaresout, outreshelp(:exnumbers) ! eto bil default
		end do
		close(101)
		!close(100)
	end do
	exOuthelpsum= trim(path) // '/' // '_energy_50-200_delta_50.txt'
	open(100, file=exOutHelpSum)
	do i = 1, 200
		tetaresout=i*0.01-1
		do k=1, 6
			
			outresTHhelpOneFile(k)=outresTH(i,exnumbers,k)
			!print*, outresTH(i,j,k), j, exnumbers; pause
				!write(100, '(2x, f11.5, 9(2x, e11.5))') tetaresout, outreshelp(:exnumbers) ! eto bil default
		end do
		write(100, '(2x, f11.5, 9(2x, e11.5))') tetaresout, outresTHhelpOneFile(:k)
		!pause
		
	end do
		close(100)
	
	
	!do i = 1, NumberPolarColumnConst*10
		!tetaResOut=i*2*pi/NumberPolarColumnConst
		!do j=1, exnumbers
			!PartOfPolarSpeedhelp(j)=PartOfPolarSpeed(i,j)
		!end do 
		!write(100, '(2x, f11.5, 9(2x, e11.5))') tetaResOut, PartOfPolarSpeedhelp(:exnumbers) 
	!end do
	!;close(101);close(102);close(103);close(104);close(105);close(106)
end

subroutine outputSinglefunk()
	use constlib
	implicit none
	integer, parameter :: tennull = 10
	integer :: i,j,k, ze=0, noutsave, va
	fnamerasp=trim(path) // '/' // trim(material) // 'ex_' // trim(exouthelp(global)) // '_' //trim(material) // '.txt'
	open(1, file=fnamerasp, status="unknown")
	!!cut zero
	noutsave=nout
	do i = 1, noutsave
		va = outres(i, global)
		if (va.eq.0) then
			ze=ze+1
			if (ze>tennull) then
				exit
			else
				cycle
			end if
		end if
	end do
	noutsave=i
	do i = 1, noutsave, 1
		energy = i*mashtabout
		WRITE (1,*) energy, outres(i, global), outres(i,global)/sqrt(energy)
	end do
	close(1)
end

subroutine exponentVarField()
	use constlib
	implicit none
		if (global.eq.0) ex=0
		if (global.eq.1) ex=500
		if (global.eq.2) ex=1000
		if (global.eq.3) ex=5000
		if (global.eq.4) ex=10000
end

end module maincount