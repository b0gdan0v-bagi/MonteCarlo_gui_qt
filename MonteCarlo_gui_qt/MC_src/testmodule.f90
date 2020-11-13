module testmodule
contains
subroutine scateratewrite(aaa, bbb)
	30  format(6(1x,e11.5))
	use math
	use constlib
	use anglefunc
	implicit none
	integer aaa, bbb, i, ios
	real*8 testNonPolarAbs, testNonPolarEmi
	fname = trim(path) // '/scateRateData.txt'
	open(4, file= fname, status="unknown", iostat =IOS)
	!deltaenergy = 0.1 * deltaenergy
	if (IOS /=0) then
		print*, "error open file! "
		stop 			
	end if
		do i=aaa, bbb
			energy=i*deltaenergy
			call scaterate2()
			testNonPolarAbs = optrateabs
			testNonPolarEmi = optrateemi
			call scaterate()
			write (4,'(2x, f11.5, 4(2x, e11.5))') energy, optrateabs, optrateemi, testNonPolarAbs, testNonPolarEmi
			!write (4,'(2x, f11.5, 4(2x, e11.5))') energy, optrateabs, optrateemi, soundrateabs, soundrateemi
			!write (*,'(2x, f11.5, 4(2x, e11.5))') energy, optrateabs, optrateemi, soundrateabs, soundrateemi
			!pause
		end do
		!deltaenergy = 10.0 * deltaenergy
	close(4)
end
subroutine testalgoritm()
	30  format(6(1x,e11.5))
	use math
	use constlib
	implicit none
	integer i
	real*8 testqendfix, testq, testiq, testdq, testqend
	real*8 :: testfoneq = 0
	open(4, file= "res/test.txt", status="unknown")
		do i=0, 1000
			energy=i*deltaenergy
			testqendfix = 4*sqes*(sqrt(energy)-sqes)/(kbol*temp)
			testqend = testqendfix
			testq = 0
			CALL random_number(ranval)
			do while ((testqend-testq)>eps)
				testdq=(testqend-testq)/2.0
				testiq=testq+testdq
				if (    (fone(testq)/fone(testqendfix)-ranval)*(fone(testiq)/fone(testqendfix)-ranval).lt.0   ) then
					testqend=testiq
				else
					testq=testiq
				end if
			end do
			testfoneq = testfoneq + fone(testiq)/fone(testqendfix)
			write (4,30) testqendfix, fone(testqendfix), ranval, testiq, fone(testiq), fone(testiq)/fone(testqendfix)
		end do
		write (4,30) testfoneq/1001.0
	close(4)
end
subroutine scateratewritepolar()
	30  format(6(1x,e11.5))
	use math
	use constlib
	use anglefunc
	implicit none
	integer i, ios
	real*8 summa(10, 10)
	open(4, file= "res/sumscateratefromdiel.txt", status="unknown", iostat =IOS)
	if (IOS /=0) then
		print*, "error open file! "
		stop 			
	end if
	energy=0.01
		do i=1, 4
			dielconstsum=0.01*10**i
			if (i.eq.4) dielconstsum=40
			call scaterate()
			summa(1,i)=optRateAbsPolar+optRateEmiPolar+soundrateabs+soundrateemi
		end do
	energy=0.1
		do i=1, 4
			dielconstsum=0.01*10**i
			if (i.eq.4) dielconstsum=40
			call scaterate()
			summa(2,i)=optRateAbsPolar+optRateEmiPolar+soundrateabs+soundrateemi
		end do
	energy=1
		do i=1, 4
			dielconstsum=0.01*10**i
			if (i.eq.4) dielconstsum=40
			call scaterate()
			summa(3,i)=optRateAbsPolar+optRateEmiPolar+soundrateabs+soundrateemi
		end do
		do i=1, 4
			dielconstsum=0.01*10**i
			if (i.eq.4) dielconstsum=40
			write (4,'(2x, f11.5, 4(2x, e11.5))') dielconstsum, summa(1,i), summa(2,i), summa(3,i)
		end do
	close(4)
end

end module testmodule
