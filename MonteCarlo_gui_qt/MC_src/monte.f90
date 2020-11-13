program monte
30 format(6(1x,e11.5))
40 format(40a1)

use testmodule
use math
use constlib
use anglefunc
use maincount
use textmodule
use gnufor2
use logMake
use gnumaker

implicit none
character(100) :: line
integer :: i, j, k
real*8 key


call inictext()
call readinput() !read input file from main catalog
call inicProcedure() 
call gnuplotcommand()
!print *, esound; pause
call scateratewrite(0, 3000)
call scateRateData()

!call system(makedirectory)

!print*, outres; pause

!!cdhgte . fonon? alp!?!??!?
! optrate * 100
!make polaropt

!main cycle
do while (line.ne.'end')
	!read(*,'(a)') line
	line = 'mcd' !for fast testing
	select case (line)
		case ('sr')   
			write(*,'(A)') 'Count scate rate from 1 mev to 1 ev with delta 1 mev'
			call scateratewrite(1, 1000)
			write(*,'(A)') 'done'
			write(*, 40) ('*', kegl = 1, 40)
		case ('testalgo') 
			call testalgoritm()
		case ('mc')  
			print *, 'Print, exstart, exend, exnumbers'
			read exstart, exend, exnumbers
			call textfield()
			call fields(exstart, exend, exnumbers)
		case ('mczero')  
			call textfield()
			call fields(0, 1, 1)
		case ('mcd')
			!energy = 0.1
			!call scaterate()
			!call scateratewritepolar()
			!write(*,'(2x, a, 2(1x, e11.5))') 'gamma, summrate test', gamma, optrateabs+optrateemi+soundrateabs+soundrateemi  
			dielconstsum=10
			!call scateratewrite(1, 1000) ! for test only !!!!!!!!!!!!!!!!
			call textfield()
			call fields(exstart, exend, exnumbers)
			!gamma=1e+15
			!outres=0
			!material='si'
			!call inicProcedure()
			!call textfield()
			!call fields(exstart, exend, exnumbers)
			line='end' !for fast testing
		case ('n')  
			print *, 'Print new n'
			read *, N
			N=int(N)
			call writeinput()
			print *, 'new N=', N
		case ('nout')  
			print *, 'Print new nout'
			read *, nout
			nout=int(nout)
			call writeinput()
			print *, 'new nout=', nout
		case ('exstart')  
			print *, 'Print new exstart'
			read *, exstart
			exstart=int(exstart)
			call writeinput()
			print *, 'new exstart=', exstart
		case ('exend')  
			print *, 'Print new exend'
			read *, exend
			exend=int(exend)
			call writeinput()
			print *, 'new exend=', exend
		case ('exnumbers')  
			print *, 'Print new exnumbers'
			read *, exnumbers
			exnumbers=int(exnumbers)
			call writeinput()
			print *, 'new exnumbers=', exnumbers
		case ('g')  
			print *, 'Print new gamma'
			read *, gamma
		call writeinput()
			print *, 'new gamma=', gamma
		case ('t')  
			print *, 'Print new temperature'
			read *, temp
			call writeinput()
			print *, 'new temperature=', temp
		case ('ge')
			material='ge'
			call inicProcedure()
		case ('si')
			material='si'
			call inicProcedure()
		case ('man') 
			call manual()
		case DEFAULT
			print *, 'not avaliable command'
	end select
end do
pause  

end program monte
