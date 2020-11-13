module gnumaker
contains
subroutine scateRateData()	
	use constlib
	fname = trim(path) // '/scateRate.plt'
	open(502, file= fname, status="unknown", iostat =IOS)
	write(502,*) ' set xlabel "energy"'
	!write(502,*) ' set yrange [0:2]'
	write(502,*) ' set ylabel "Scaterate"'
	write(502,*) ' m="./scateRateData.txt"'
	write(502,*) ' set nokey'
	write(502,*) ' set grid'
	write(502,*) ' set title "Scate Rate"'
	write(502,*) 'set key right top'
	write(502,*) ' set border linewidth 1.5'
	call LineMaker(4, 1)
	write(502,*) 'plot m using 1:2 with linespoints linestyle 1 title "Optical Rate Absorption"'
	write(502,*) 'replot m using 1:3 with linespoints linestyle 2 title "Optical Rate Emission"'
	write(502,*) 'replot m using 1:4 with linespoints linestyle 3 title "Non polar Optical Rate Absorption"'
	write(502,*) 'replot m using 1:5 with linespoints linestyle 4 title "Non polar Optical Rate Emission"'
	close(502)

	fname = trim(path) // '/scateRateLogScale.plt'
	open(502, file= fname, status="unknown", iostat =IOS)
	write(502,*) ' set xlabel "energy"'
	!write(502,*) ' set yrange [0:2]'
	write(502,*) ' set ylabel "Scaterate"'
	write(502,*) ' m="./scateRateData.txt"'
	write(502,*) ' set nokey'
	write(502,*) ' set grid'
	write(502,*) ' set logscale y'
	write(502,*) ' set title "Scate Rate"'
	write(502,*) 'set key right top'
	write(502,*) ' set border linewidth 1.5'
	call LineMaker(4, 1)
	write(502,*) 'plot m using 1:2 with linespoints linestyle 1 title "Optical Rate Absorption"'
	write(502,*) 'replot m using 1:3 with linespoints linestyle 2 title "Optical Rate Emission"'
	write(502,*) 'replot m using 1:4 with linespoints linestyle 3 title "Non polar Optical Rate Absorption"'
	write(502,*) 'replot m using 1:5 with linespoints linestyle 4 title "Non polar Optical Rate Emission"'
	close(502)

end

subroutine DistributionFuncData()	
	use constlib
	integer i
	character*200 TempData, DataNumber
	fname = trim(path) // '/DistributionFuncData.plt'
	open(502, file= fname, status="unknown", iostat =IOS)
	write(502,*) ' set xlabel "energy"'
	!write(502,*) ' set yrange [0:2]'
	write(502,*) ' set ylabel "Distribution"'
	write(502,*) ' m="./' // trim(exOutHelpSum) // '"'
	write(502,*) ' set nokey'
	write(502,*) ' set grid'
	write(502,*) ' set logscale y'
	write(502,*) ' set title "Scate Rate"'
	write(502,*) 'set key right top'
	write(502,*) ' set border linewidth 1.5'
	call LineMaker(exnumbers, 1)
	write(502,*) 'plot m using 1:2 with linespoints linestyle 1 title "'// trim(exouthelp(1)) // '"'
	if (exnumbers>=2) then
		do i=2, exnumbers
			write (DataNumber, "(I1)") i+1
			TempData = 'replot m using 1:' //trim(DataNumber) // ' with linespoints linestyle '
			write (DataNumber, "(I1)") i
			TempData = trim(TempData) // ' ' // trim(DataNumber)  // ' title "'// trim(exouthelp(i)) // '"'
			write(502,*) TempData
		end do
	end if
	close(502)
end

subroutine DistributionFuncDataSemi()	
	use constlib
	integer i
	character*200 TempData, DataNumber
	fname = trim(path) // '/DistributionFuncDataSemi.plt'
	open(502, file= fname, status="unknown", iostat =IOS)
	write(502,*) ' set xlabel "energy"'
	!write(502,*) ' set yrange [0:2]'
	write(502,*) ' set ylabel "Distribution"'
	write(502,*) ' m="./' // trim(exOutHelpSumSemi(1)) // '"'
	write(502,*) ' n="./' // trim(exOutHelpSumSemi(2)) // '"'
	write(502,*) ' set nokey'
	write(502,*) ' set grid'
	write(502,*) ' set logscale y'
	write(502,*) ' set title "Scate Rate"'
	write(502,*) 'set key right top'
	write(502,*) ' set border linewidth 1.5'
	call LineMaker(exnumbers, 2)
	write(502,*) 'plot m using 1:2 with linespoints linestyle 1 title "'// trim(exouthelp(1)) // ' cos < 0"'
	write(502,*) 'plot n using 1:2 with linespoints linestyle 11 title "'// trim(exouthelp(1)) // ' cos > 0"'
	if (exnumbers>=2) then
		do i=2, exnumbers
			write (DataNumber, "(I1)") i+1
			TempData = 'replot m using 1:' //trim(DataNumber) // ' with linespoints linestyle '
			write (DataNumber, "(I1)") i
			TempData = trim(TempData) // ' ' // trim(DataNumber)  // ' title "'// trim(exouthelp(i)) // ' cos < 0"'
			write(502,*) TempData
		end do
		do i=2, exnumbers
			write (DataNumber, "(I1)") i+1
			TempData = 'replot n using 1:' //trim(DataNumber) // ' with linespoints linestyle '
			write (DataNumber, "(I2)") i+10
			TempData = trim(TempData) // ' ' // trim(DataNumber)  // ' title "'// trim(exouthelp(i)) // ' cos > 0"'
			write(502,*) TempData
		end do
	end if
	close(502)
end

subroutine LineMaker(aaa, bbb) !aaa number of colors, bbb - number of files to plot
	character*200 DataNumber, TempData
	character*200, parameter, dimension(7):: COLORS = (/ "red", "blue", "black" ,"orange", "yellow", "green", "pink"/)
	integer i, j, aaa, bbb
	do j=1, bbb
	do i=1, aaa
		write (DataNumber, "(I1)") i+(j-1)*10
		if (j>1) write (DataNumber, "(I2)") i+(j-1)*10
		write(502,*) 'set style line ' // trim(DataNumber) // '\'
		write(502,*) 'linecolor rgb "' // trim(COLORS(i)) // '"\'
		write (DataNumber, "(I1)") 1+(j-1)*2
		if (j.eq.1) then
			write(502,*) 'linetype ' // trim(DataNumber) // ' linewidth 0.5 \'
		else
			write(502,*) 'linetype ' // trim(DataNumber) // ' linewidth 1.0 \'
		end if
		write(502,*) 'pointtype 7 pointsize 0.2'
	end do
	end do

end

end module gnumaker