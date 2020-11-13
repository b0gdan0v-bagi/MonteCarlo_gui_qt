module math
contains
real*8 function fone(x)
	implicit none
	real*8 :: x, xa=3.5, res !xa=xavg=3.5 - recomended
	if (x.LE.xa) then
		res = x**2/2-x**3/6+x**4/48-x**6/4320+x**8/241920-x**10/1209600+x**12/622702080
	else
		res = xa**2/2-xa**3/6+xa**4/48-xa**6/4320+xa**8/241920-xa**10/1209600+xa**12/622702080
	res = res + exp(-xa)*(xa**2+2*xa+2)-exp(-x)*(x**2+2*x+2)
	endif
	fone = res
	return
end function fone

real*8 function gone(x)
	implicit none
	real*8 :: x, xa=4, res !xa=xavg=4 - recomended
	if (x.LE.xa) then
		res = x**2/2 + x**3/6 + x**6/4320+x**8/241920-x**10/1209600+x**12/622702080
	else
		res = xa**2/2 + xa**3/6 + xa**6/4320+xa**8/241920-xa**10/1209600+xa**12/622702080
		res = res + exp(-xa)*(xa**2+2*xa+2)-xa**3/3-exp(-x)*(x**2+2*x+2)+x**3/3
	endif
	gone = res
	return
end function gone
end module math