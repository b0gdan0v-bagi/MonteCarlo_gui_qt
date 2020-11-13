module logMake
contains
subroutine gammaLog()
	use constlib
	implicit none
		write (*, '(1x, a, e11.5)') 'max gamma was= ', optrateabs+optrateemi+soundrateabs+soundrateemi
		write (*, '(1x, a, e11.5)') "max energy at max field =", maxenergy
		write (*, '(1x, a, e11.5)') "New gamma =", gamma
		write (501, '(1x, a, e11.5)') 'max gamma was= ', optrateabs+optrateemi+soundrateabs+soundrateemi
		write (501, '(1x, a, e11.5)') "max energy at max field =", maxenergy
		write (501, '(1x, a, e11.5)') "New gamma =", gamma
end

subroutine beforeMainCountLog()
	40 format(40a1)
	use constlib
	implicit none
		write(*, 40) ('_', kegl = 1, 40)
		write(*, 40) ('*', kegl = 1, 40) !выводим до, чтобы визуально оценить время
		print *, "ex =", ex
		write(501, 40) ('_', kegl = 1, 40)
		write(501, 40) ('*', kegl = 1, 40) !выводим до, чтобы визуально оценить время
		write(501,*) "ex =", ex
end

subroutine afterMainCountLog()
	40 format(40a1)
	use constlib
	implicit none
		write (*, '(2x, a, e11.5)') "avgenergy =", avgenergy
		write (*, '(2x, a, e11.5)') "maxenergy =", maxenergy
		write (*, '(2x, a, e11.5)') "self-scattering =", 1-1d0*act/N
		write (*, 40) ('=', kegl = 1, 40)
		write (*, '(2x, a, e11.5)') "avgspeedX =", real(avgspeed)
		write (*, '(2x, a, e11.5)') "avgspeedY =", real(avgspeedY/tsum)
		write (*, '(2x, a, e11.5)') "avgspeedZ =", real(avgspeedZ/tsum)
		write (*, '(2x, a, e11.5)') "FreeRun =", real(FreeRun)
		write (*, '(2x, a, e11.5)') "Effective Temperautre =", real(FreeRun*ex*qe)
		write (*, '(2x, a, e11.3, 2x, a, e11.3)') 'teta avg = ', tetatest/(N+1), ' fi avg = ', fitest/(N+1)
		write (*, '(2x, a, e11.3, 2x, a, e11.3)') 'teta scat avg = ',tetatest2/act, 'fi scat avg = ',fitest2/act
		write (*, 40) ('=', kegl = 1, 40)
		write (*, '(2x, a, e11.5)') "phonon-scat times =", real(act)

		write (501, '(2x, a, e11.5)') "avgenergy =", avgenergy
		write (501, '(2x, a, e11.5)') "maxenergy =", maxenergy
		write (501, '(2x, a, e11.5)') "self-scattering =", 1-1d0*act/N
		write (501, 40) ('=', kegl = 1, 40)
		write (501, '(2x, a, e11.5)') "avgspeedX =", real(avgspeed)
		write (501, '(2x, a, e11.5)') "avgspeedY =", real(avgspeedY/tsum)
		write (501, '(2x, a, e11.5)') "avgspeedZ =", real(avgspeedZ/tsum)
		write (501, '(2x, a, e11.5)') "FreeRun =", real(FreeRun)
		write (501, '(2x, a, e11.5)') "Effective Temperautre =", real(FreeRun*ex*qe)
		write (501, '(2x, a, e11.3, 2x, a, e11.3)') 'teta avg = ', tetatest/(N+1), ' fi avg = ', fitest/(N+1)
		write (501, '(2x, a, e11.3, 2x, a, e11.3)') 'teta scat avg = ',tetatest2/act, 'fi scat avg = ',fitest2/act
		write (501, 40) ('=', kegl = 1, 40)
		write (501, '(2x, a, e11.5)') "phonon-scat times =", real(act)
end
		
subroutine timeCountLog()
	40 format(40a1)
	use constlib
	implicit none
	character(100) timeAndDate
		timeend = secnds(timestart)
		write(*, *) "programm time =  ", timeend, " seconds."
		write(*, *) "time per field = ", timeend/exnumbers, " seconds."
		write(*, *) "cycle time =     ", timeend/(N*exnumbers), " seconds."
		write(*, 40) ('_', kegl = 1, 40)
		write(*, 40) ('*', kegl = 1, 40)
		write(501, *) "programm time =  ", timeend, " seconds."
		write(501, *) "time per field = ", timeend/exnumbers, " seconds."
		write(501, *) "cycle time =     ", timeend/(N*exnumbers), " seconds."
		write(501, 40) ('_', kegl = 1, 40)
		write(501, 40) ('*', kegl = 1, 40)
		call date_and_time(DATE=date)
		call date_and_time(TIME=time)
		timeAndDate = "program ended at " // trim(date) // '  ' // trim (time)
		write(*,*) timeAndDate
		write(501,*) timeAndDate
end

subroutine inputLog()
	30  format(6(1x,e11.5))
	use constlib
	implicit none
	write(501,*) 'iterations=  ', Nreal
	write(501,*) 'nout=        ', nout
	write(501,*) 'exstart=     ', exstart
	write(501,*) 'iterations=  ', exnumbers
	write(501,*) 'exend=       ', exend
	write(501,*) 'lof=         ', logcreate
	write(501,*) 'gamma=       ', gamma
	write(501,*) 'temperature= ', temp
	write(501,*) "material =   ", material
end

end module logMake
