!     *****************************************************
!     A Program for obtaining the evolution of a homogeneous binary
!     mixture which is quenched below the coexistence curve at time t=0.  
!     Patterns are obtained from an Euler-discretized version of the
!     dimensionless "CHC" eq. without thermal fluctuations.
!     Lattice size is 256 * 256.
!     *****************************************************

      PROGRAM evolution_chc
      IMPLICIT NONE
      REAL::ran2,dt,dx
      INTEGER::i,j,idum,t 
      INTEGER, PARAMETER:: L=100
      REAL, DIMENSION(L,L)::m,x,y,z,n      
      INTEGER, DIMENSION(L)::p,q
!     for a 2-D square lattice of size (L-1)*(L-1),
!     total no. of points on the lattice = L*L      
      idum = -195836
      dt = 0.01
      dx = 1.0
!      OPEN(unit=1,file='c1kp.dat',status='unknown')
!      OPEN(unit=2,file='c1kn.dat',status='unknown')
      OPEN(unit=3,file='chcMovie.dat',status='unknown')
      
!     generating random nos. on a square lattice of size L*L
!     random numbers are between -0.1 to +0.1
      DO i=1,L
        DO j=1,L
!          m(i,j) = -0.1 + 0.2* ran2(idum)
          m(i,j) = -0.5 + 0.2* ran2(idum)
        ENDDO 
      ENDDO 
!     *****************************************************

!     applying periodic boundary conditions
      DO i=1,L
        p(i)=i+1
        if(i==L) p(i)=1
        q(i)=i-1
        if(i==1) q(i)=L
      ENDDO
!     *****************************************************

!     Euler-discretization mesh sizes are delta t =0.01 & delta x =1.0
      DO t=0,2000
!       PRINT*,m(1,1)
        DO i=1,L
          DO j=1,L
            x(i,j)=m(p(i),j)+m(q(i),j)+m(i,p(j))+m(i,q(j))-4.0*m(i,j) 
            y(i,j)=m(i,j)**3-m(i,j)-x(i,j)/(dx*dx)
          ENDDO
        ENDDO
        DO i=1,L
          DO j=1,L
            z(i,j)=y(p(i),j)+y(q(i),j)+y(i,p(j))+y(i,q(j))-4.0*y(i,j)
            n(i,j)=m(i,j)+(dt)/(dx*dx)*z(i,j)
          ENDDO
        ENDDO
!       evolution of domain growth (pattern formation)

        IF(MOD(t,20)==0) THEN

        WRITE(3,*)L*L
        WRITE(3,*)""

        DO i=1,L
          DO j=1,L
            IF(m(i,j)>0.0)THEN
              WRITE(3,*)"A",i,j,0
            ELSE
              WRITE(3,*)"B",i,j,0
            ENDIF
          ENDDO
        ENDDO 

        ENDIF

        m = n
        
      ENDDO            !  end of loop t (time steps)

      END PROGRAM evolution_chc

!     *****************************************************
!     *****************************************************

!     a subroutine for generating random numbers
!     DOUBLE PRECISION FUNCTION ran2(idum)
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
      END              !!!!!         end of subroutine ran2
!     !!!!!!!!!!!!!!*******************!!!!!!!!!!!!!!!!!!!!
!     *****************************************************


