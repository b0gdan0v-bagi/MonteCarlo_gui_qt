module textmodule
contains
subroutine readinput()
	use constlib
	implicit none
	integer i
	real batya(5)
	character temptext !for read
	open(1, file='input.txt')
	read(1,*) temptext, Nreal, temptext, nout, temptext, exstart, temptext, exnumbers 
	read(1,*) temptext, exend, temptext, logcreate, temptext, gamma, temptext, temp, temptext, material
	read(1,*) temptext, limiterForce, temptext, fieldInput, temptext, polarInput, temptext, kostilConstPolar
	read(1,*) temptext, triangleCheckConfig, temptext, triangleCheckPause, temptext, triangleCheckNumber
	read(1,*) temptext, ee_collisions, temptext, ee_rate
	close(1)
	if (fieldInput.eq.1) then
		open(1, file='fieldInput.txt')
		read(1,*) exnumbers
		do i=1, exnumbers
		print*, i
			read(1,*) fieldsinput(i)
		end do
		close(1)
	end if
	if (exnumbers>maxexnumbers) print*, 'ERROR!!! exnumbers>maxexnumbers'
	N=int(Nreal)
	!exnumbers=exnumbers+1
end

subroutine textfield()
	30  format(6(1x,e11.5))
	use constlib
	implicit none
	WRITE (*,*) "iterations =", real(N) 
	WRITE (*,*) "nout =", real(nout) 
	WRITE (*,*) "exstart =", exstart 
	WRITE (*,*) "exend =", exend
	WRITE (*,*) "gamma =", real(gamma)
	WRITE (*,*) "temperature = ", temp
	WRITE (*,*) "material = ", material
	WRITE (*,*) "polar scattering = ", polarInput
	write (*, '(2x, a, e11.5)') "eg    =", eg
end

subroutine writeinput()
	30  format(6(1x,e11.5))
	use constlib
	implicit none
	open(1, file='input.txt', status="unknown")
	write(1,*) 'iterations=  ', Nreal
	write(1,*) 'nout=        ', nout
	write(1,*) 'exstart=     ', exstart
	write(1,*) 'iterations=  ', exnumbers
	write(1,*) 'exend=       ', exend
	write(1,*) 'lof=         ', logcreate
	write(1,*) 'gamma=       ', gamma
	write(1,*) 'temperature= ', temp
	write(1,*) "material =   ", material
	close(1)
end
subroutine inictext()
	implicit none
	write(*,'(A)') 'Hello, this program will count electrons trajectories'
	write(*,'(A)') 'Write "man" for availiable commands / help'
	write(*,'(A)') 'Write "end" for end'
	write(*,'(A)') 'Write "mcd" main monte-carlo prog with default parametrs'
end
subroutine manual()
	40 format(40a1)
		implicit none
		integer kegl
		write(*,'(A)') 'Man - manual'
		write(*, 40) ('*', kegl = 1, 40)
		write(*,'(A)') '"mc" main monte-carlo prog with count from exstart to exend with n points'
		write(*,'(A)') '"mczero" main monte-carlo prog with ex=0'
		write(*,'(A)') '"mcd" main monte-carlo prog with default parametrs'
		write(*,'(A)') '"sr" count scate rate from 1 mev to 1 ev with delta 1 mev'
		write(*,'(A)') '"testalgo" count correctness of fone/gone q found algoritms'
		write(*,'(A)') '"ge" will chancge for ge'
		write(*,'(A)') '"si" will chancge for si'
		write(*, 40) ('*', kegl = 1, 40)
		write(*,'(A)') 'You can use real for this:'
		write(*,'(A)') '"n" for change the number of trajectories'
		write(*,'(A)') '"nout" for number of points output distribution function'
		write(*,'(A)') '"t" for new temperature'
		write(*,'(A)') '"g" for new gamma'
		write(*,'(A)') '"exstart"'
		write(*,'(A)') '"exend"'
		write(*,'(A)') '"exnumbers"'
		write(*, 40) ('*', kegl = 1, 40)
end



end module textmodule

