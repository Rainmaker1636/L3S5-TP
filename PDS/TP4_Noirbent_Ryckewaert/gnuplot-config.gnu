set title "Temps d'execution en fonction de la taille du buffer"
set logscale x
set xlabel "Taille du buffer"
set ylabel "Temps d'execution"
set style data linespoints
plot "mcat.dat" using 1:2 title "Temps d'execution real",\
     "mcat.dat" using 1:3 title "Temps d'execution user",\
     "mcat.dat" using 1:4 title "Temps d'execution sys"
pause -1  "Hit return to continue
