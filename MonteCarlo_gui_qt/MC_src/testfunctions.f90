subroutine writeresultscatrate(a,b)
    include 'physconst.f90'
    real*8 a, b
    30  format(4(1x,e11.5))
	open(4, file= "res/resultscatrate.txt", status="unknown")
	do i=int(a), int(b)
		energy=i*1d-3
		call scaterate(energy, optrateabs, optrateemi, soundrateabs, soundrateemi)
		write (4,30) optrateabs, optrateemi, soundrateabs, soundrateemi
	end do
close(4)
end subroutine writeresultscatrate

  res=1
  open(4, file= "res/checksndangelabs.txt", status="unknown")
  do i=1, 1000
    testteta=0
    do while (testteta<pi)
    testfi=0
      do while (testfi<2*pi)
    testenergy=i*1d-4
    energy=testenergy
    testteta=testteta+5*0.0174533d0
    teta=testteta
    testfi=testfi+5*0.0174533d0
    fi=testfi
    !call scaterateres(res, energy, teta, fi)
   ! write(4,50) testenergy, energy, testteta, teta, testfi, fi
    50  format(7(1x,e11.5))
    !write(*,30) energy, teta, fi
    !write(*,*) '======'; pause
  end do
  end do
  !write(*,*) '======';pause
  end do
close(4)
write(*,*) '======';
  res=2
  !open(4, file= "res/checksndangelemi.txt", status="unknown")
  do i=1, 1000
    testteta=0
    do while (testteta<pi)
    testfi=0
      do while (testfi<2*pi)
    testenergy=i*1d-4
    energy=testenergy
    testteta=testteta+5*0.0174533d0
    teta=testteta
    testfi=testfi+5*0.0174533d0
    fi=testfi
    !call scaterateres(res, energy, teta, fi)
    !write(4,50) testenergy, energy, testteta, teta, testfi, fi
    !write(*,30) energy, teta, fi
    !write(*,*) '======'; pause
  end do
  end do
  end do
!close(4)
!scatrate by energy
!open(4, file= "res/resultscatrate.txt", status="unknown")
!do i=1, 1000
 ! energy=i*1d-3
 ! call scaterate(energy, optrateabs, optrateemi, soundrateabs, soundrateemi)
  !write (4,30) optrateabs, optrateemi, soundrateabs, soundrateemi
!end do
!close(4)