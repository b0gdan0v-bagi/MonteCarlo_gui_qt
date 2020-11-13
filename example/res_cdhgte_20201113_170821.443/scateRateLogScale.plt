  set xlabel "energy"
  set ylabel "Scaterate"
  m="./scateRateData.txt"
  set nokey
  set grid
  set logscale y
  set title "Scate Rate"
 set key right top
  set border linewidth 1.5
 set style line 1\
 linecolor rgb "red"\
 linetype 1 linewidth 0.5 \
 pointtype 7 pointsize 0.2
 set style line 2\
 linecolor rgb "blue"\
 linetype 1 linewidth 0.5 \
 pointtype 7 pointsize 0.2
 set style line 3\
 linecolor rgb "black"\
 linetype 1 linewidth 0.5 \
 pointtype 7 pointsize 0.2
 set style line 4\
 linecolor rgb "orange"\
 linetype 1 linewidth 0.5 \
 pointtype 7 pointsize 0.2
 plot m using 1:2 with linespoints linestyle 1 title "Optical Rate Absorption"
 replot m using 1:3 with linespoints linestyle 2 title "Optical Rate Emission"
 replot m using 1:4 with linespoints linestyle 3 title "Non polar Optical Rate Absorption"
 replot m using 1:5 with linespoints linestyle 4 title "Non polar Optical Rate Emission"
