module gnufor2
contains
subroutine gnuplotcommand()
	implicit none
	OPEN(UNIT=600,FILE='data_plot.plt')
	write(600, *) 'set xlabel "ev"'
	write(600, *) 'set logscale y'
	!write(600, *) 'set xrange [-1.1:1.1]'
	!write(600, *) 'set yrange [0:0.02]'
	!write(600, *) 'set xlabel "k, pi/a"'
	write(600, *) 'set ylabel "a/lambda"'
	write(600, *) 'm="./res_ingaas/raspred_full_0_250_500_750_1000_.txt"'
	write(600, *) 'set key left bottom'
	write(600, *) 'set grid'
	write(600, *) "set title 'Photonic band structure'"
	write(600, *) 'set border linewidth 1.5'
	write(600, *) 'set style line 1 \'
	write(600, *) "	linecolor rgb '#0060ad' \"
	write(600, *) '	linetype 1 linewidth 0.5 \'
	write(600, *) '	pointtype 7 pointsize 0.2'
	write(600, *) 'plot   m using 1:2 t "0"'
	write(600, *) 'replot m using 1:3 t "250"'
	write(600, *) 'replot m using 1:4 t "500"'
	write(600, *) 'replot m using 1:5 t "750"'
	close(600)
end
end module gnufor2