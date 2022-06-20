!program to find length scale from structure factor and correlation function of binary AB mixture, chc equation is used

program chc_sf_cf
    implicit none
    real::ran2
    integer::lx=128,ly=128,tot_t=200,idum=-195836,nens=10  !parameters
    real::dx2=1.0, dt=0.01    !step size
    integer::i,j,t,iens        !loop indexes
    real, dimension(128,128)::psi0,d2psi,phi,d2phi,psi
    integer,dimension(128)::f,b     !for boundary condition (f->forward, b->backward)

    open(unit=3,file='chc_sf_cf.dat',status='unknown')

    ! initialize psi randomly at t=0
    ! boundary condition
    do i=1,lx     !lx=ly so we can use any of them
    f(i) = i+1    !forward
    if(i==lx) f(i)=1
    b(i) = i-1   !backward
    if(i==1) b(i)=lx
    enddo

    ensemble_loop:do iens=1,nens
        do i=1,lx
            do j=1,ly
                psi0(i,j) = 1.0*ran2(idum) - 0.3
            end do
        end do

    time_loop:do t=0,tot_t

    !psi calculation at every time step
    do i=1,lx
        do j=1,ly
        d2psi(i,j) = (psi0(f(i),j) + psi0(b(i),j) + psi0(i,f(j)) + psi0(i,b(j)) -4.0*psi0(i,j))/dx2
        phi(i,j) = psi0(i,j)**3.0 - psi0(i,j) - d2psi(i,j)
        d2phi(i,j) = (phi(f(i),j) + phi(b(i),j) + phi(i,f(j)) + phi(i,b(j)) - 4.0*phi(i,j))/dx2
        enddo
    enddo

    ! writing psi to file at every 20 steps
    if(mod(t,20)==0) then
        do i=1,lx
            do j=1,ly
                write(3,*)iens,t*dt,psi0(i,j)
            enddo
        enddo
    endif

    psi = psi0 + d2phi*dt
    psi0 = psi

    enddo time_loop
    enddo ensemble_loop

end program chc_sf_cf

REAL FUNCTION ran2(idum)
              
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      REAL AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1, &
      IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791, &
      NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/, iv/NTAB*0/, iy/0/
      IF (idum.LE.0) THEN
        idum=MAX(-idum,1)
        idum2=idum
        DO 11 j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          IF (idum.LT.0) idum=idum+IM1
          IF (j.LE.NTAB) iv(j)=idum
11      CONTINUE
        iy=iv(1)
      ENDIF
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      IF (idum.LT.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      IF (idum2.LT.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      IF(iy.LT.1)iy=iy+IMM1
      ran2=MIN(AM*iy,RNMX)
      RETURN
      END
















    

