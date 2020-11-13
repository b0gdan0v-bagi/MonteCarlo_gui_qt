  set xlabel "energy"
  set ylabel "Distribution"
  m="./distribFunc_0_200_400_600_800_.txt"
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
 set style line 5\
 linecolor rgb "yellow"\
 linetype 1 linewidth 0.5 \
 pointtype 7 pointsize 0.2
 plot m using 1:2 with linespoints linestyle 1 title "0"
 replot m using 1:3 with linespoints linestyle 2 title "200"                                                                                                                                             
 replot m using 1:4 with linespoints linestyle 3 title "400"                                                                                                                                             
 replot m using 1:5 with linespoints linestyle 4 title "600"                                                                                                                                             
 replot m using 1:6 with linespoints linestyle 5 title "800"                                                                                                                                             
