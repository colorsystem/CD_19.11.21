C     Last change: KU 30.04.2020 09:50:10
      SUBROUTINE lubksb(a,n,np,indx,b)
      IMPLICIT  REAL*8 (A-H,O-Z)
      INTEGER(KIND=4):: n,np,indx(*)
      REAL(KIND=8),DIMENSION (np,*) :: a
      REAL(KIND=8),DIMENSION (*) :: b

      INTEGER(KIND=4) :: i,ii,j,ll
      REAL(KIND=8) :: su
      ii=0
      do 12 i=1,n
        ll=indx(i)
        su=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            su=su-a(i,j)*b(j)
11        continue
        else if (su.ne.0.) then
          ii=i
        endif
        b(i)=su
12    continue
      do 14 i=n,1,-1
        su=b(i)
        do 13 j=i+1,n
          su=su-a(i,j)*b(j)
13      continue
        b(i)=su/(a(i,i)+SIGN(1.0D-8,a(i,i)))
14    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 5cL&12s(5A2+Y53.
      SUBROUTINE ludcmp(a,n,np,indx,d)
      IMPLICIT  REAL*8 (A-H,O-Z)
      INTEGER(KIND=4):: n,np,indx(*)
      REAL(KIND=8),DIMENSION (np,*) :: a
      REAL(KIND=8)  ::  TIN,d
      INTEGER i,imax,j,k
      REAL*8 aamax,dum,su,vv(N)
      d=1.D0
      TIN=TINY(d)
      DO i=1,n
        indx(i)=i 
      END DO
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.D0) THEN
           d=0.
           RETURN
        ENDIF
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        do 14 i=1,j-1
          su=a(i,j)
          do 13 k=1,i-1
            su=su-a(i,k)*a(k,j)
13        continue
          a(i,j)=su
14      continue
        aamax=0.
        do 16 i=j,n
          su=a(i,j)
          do 15 k=1,j-1
            su=su-a(i,k)*a(k,j)
15        continue
          a(i,j)=su
          dum=vv(i)*abs(su)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.D0)a(j,j)=TIN
        if(j.ne.n)then
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      return
      end
C  (C) Copr. 1986-92 Numerical Recipes Software 5cL&12s(5A2+Y53.

C  (C) Copr. 1986-92 Numerical Recipes Software 5cL&12s(5A2+Y53.
      SUBROUTINE zroots(a,m,roots,polish)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER(KIND=4) m
      REAL(KIND=8) EPS
      COMPLEX(KIND=8) a(m+1),roots(m)
      LOGICAL(KIND=4) polish
      PARAMETER (EPS=1.d-6)
CU    USES laguer
      INTEGER(KIND=4) i,j,jj,its
      COMPLEX(KIND=8) ad(m+1),x,b,c
      do 11 j=1,m+1
        ad(j)=a(j)
11    continue
      do 13 j=m,1,-1
        x=cmplx(0.,0.)
        call laguer(ad,j,x,its)
        if(abs(aimag(x)).le.2.*EPS**2*abs(real(x))) x=cmplx(real(x),0.)
        roots(j)=x
        b=ad(j+1)
        do 12 jj=j,1,-1
          c=ad(jj)
          ad(jj)=b
          b=x*b+c
12      continue
13    continue
      if (polish) then
        do 14 j=1,m
          call laguer(a,m,roots(j),its)
14      continue
      endif
      do 16 j=2,m
        x=roots(j)
        do 15 i=j-1,1,-1
          if(real(roots(i)).le.real(x))goto 10
          roots(i+1)=roots(i)
15      continue
        i=0
10      roots(i+1)=x
16    continue
      return
      END


C  (C) Copr. 1986-92 Numerical Recipes Software 5cL&12s(5A2+Y53.
      SUBROUTINE zrhqr(a,m,rtr,rti)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER(KIND=4) m
      REAL(KIND=8) a(m+1),rtr(m),rti(m)
CU    USES balanc,hqr
      INTEGER(KIND=4) j,k
      REAL(KIND=8) hess(M,M),xr,xi
      IF(M.EQ.0) THEN
        RETURN
      ENDIF
      if (a(m+1).eq.0.D0) THEN
        DO I=1,M
          rtr(i)=-HUGE(1.D0)
          rti(i)=-HUGE(1.D0)
        END DO
       RETURN
      ENDIF
      do 12 k=1,m
        hess(1,k)=-a(m+1-k)/a(m+1)
        do 11 j=2,m
          hess(j,k)=0.
11      continue
        if (k.ne.m) hess(k+1,k)=1.D0
12    continue
      call balanc(hess,m,m)
      call hqr(hess,m,m,rtr,rti)
      do 14 j=2,m
        xr=rtr(j)
        xi=rti(j)
        do 13 k=j-1,1,-1
          if(rtr(k).le.xr)goto 1
          rtr(k+1)=rtr(k)
          rti(k+1)=rti(k)
13      continue
        k=0
1       rtr(k+1)=xr
        rti(k+1)=xi
14    continue
      return
      END
      SUBROUTINE hqr(a,n,np,wr,wi)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER(KIND=4) n,np
      REAL(KIND=8) a(np,np),wi(np),wr(np)
      INTEGER(KIND=4) i,its,j,k,l,m,nn
      REAL(KIND=8) anorm,p,q,r,s,t,u,v,w,x,y,z
      anorm=abs(a(1,1))
      do 12 i=2,n
        do 11 j=i-1,n
          anorm=anorm+abs(a(i,j))
11      continue
12    continue
      nn=n
      t=0.
1     if(nn.ge.1)then
        its=0
2       do 13 l=nn,2,-1
          s=abs(a(l-1,l-1))+abs(a(l,l))
          if(s.eq.0.D0)s=anorm
          if(abs(a(l,l-1))+s.eq.s)goto 3
13      continue
        l=1
3       x=a(nn,nn)
        if(l.eq.nn)then
          wr(nn)=x+t
          wi(nn)=0.
          nn=nn-1
        else
          y=a(nn-1,nn-1)
          w=a(nn,nn-1)*a(nn-1,nn)
          if(l.eq.nn-1)then
            p=0.5*(y-x)
            q=p**2+w
            z=sqrt(abs(q))
            x=x+t
            if(q.ge.0.D0)then
              z=p+sign(z,p)
              wr(nn)=x+z
              wr(nn-1)=wr(nn)
              if(z.ne.0.D0)wr(nn)=x-w/z
              wi(nn)=0.D0
              wi(nn-1)=0.D0
            else
              wr(nn)=x+p
              wr(nn-1)=wr(nn)
              wi(nn)=z
              wi(nn-1)=-z
            endif
            nn=nn-2
          else
            if(its.eq.100) RETURN
            if(its.eq.10.or.its.eq.20)then
              t=t+x
              do 14 i=1,nn
                a(i,i)=a(i,i)-x
14            continue
              s=abs(a(nn,nn-1))+abs(a(nn-1,nn-2))
              x=0.75*s
              y=x
              w=-0.4375*s**2
            endif
            its=its+1
            do 15 m=nn-2,l,-1
              z=a(m,m)
              r=x-z
              s=y-z
              p=(r*s-w)/a(m+1,m)+a(m,m+1)
              q=a(m+1,m+1)-z-r-s
              r=a(m+2,m+1)
              s=abs(p)+abs(q)+abs(r)
              p=p/s
              q=q/s
              r=r/s
              if(m.eq.l)goto 4
              u=abs(a(m,m-1))*(abs(q)+abs(r))
              v=abs(p)*(abs(a(m-1,m-1))+abs(z)+abs(a(m+1,m+1)))
              if(u+v.eq.v)goto 4
15          continue
4           do 16 i=m+2,nn
              a(i,i-2)=0.
              if (i.ne.m+2) a(i,i-3)=0.D0
16          continue
            do 19 k=m,nn-1
              if(k.ne.m)then
                p=a(k,k-1)
                q=a(k+1,k-1)
                r=0.
                if(k.ne.nn-1)r=a(k+2,k-1)
                x=abs(p)+abs(q)+abs(r)
                if(x.ne.0.D0)then
                  p=p/x
                  q=q/x
                  r=r/x
                endif
              endif
              s=sign(sqrt(p**2+q**2+r**2),p)
              if(s.ne.0.D0)then
                if(k.eq.m)then
                  if(l.ne.m)a(k,k-1)=-a(k,k-1)
                else
                  a(k,k-1)=-s*x
                endif
                p=p+s
                x=p/s
                y=q/s
                z=r/s
                q=q/p
                r=r/p
                do 17 j=k,nn
                  p=a(k,j)+q*a(k+1,j)
                  if(k.ne.nn-1)then
                    p=p+r*a(k+2,j)
                    a(k+2,j)=a(k+2,j)-p*z
                  endif
                  a(k+1,j)=a(k+1,j)-p*y
                  a(k,j)=a(k,j)-p*x
17              continue
                do 18 i=l,min(nn,k+3)
                  p=x*a(i,k)+y*a(i,k+1)
                  if(k.ne.nn-1)then
                    p=p+z*a(i,k+2)
                    a(i,k+2)=a(i,k+2)-p*r
                  endif
                  a(i,k+1)=a(i,k+1)-p*q
                  a(i,k)=a(i,k)-p
18              continue
              endif
19          continue
            goto 2
          endif
        endif
      goto 1
      endif
      return
      END
      SUBROUTINE balanc(a,n,np)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER(KIND=4) n,np
      REAL(KIND=8) a(np,np),RADI,SQRDX
      PARAMETER (RADI=2.,SQRDX=RADI**2)
      INTEGER(KIND=4) i,j,last
      REAL(KIND=8) c,f,g,r,s
1     continue
        last=1
        do 14 i=1,n
          c=0.
          r=0.
          do 11 j=1,n
            if(j.ne.i)then
              c=c+abs(a(j,i))
              r=r+abs(a(i,j))
            endif
11        continue
          if(c.ne.0.D0.and.r.ne.0.D0)then
            g=r/RADI
            f=1.
            s=c+r
2           if(c.lt.g)then
              f=f*RADI
              c=c*SQRDX
            goto 2
            endif
            g=r*RADI
3           if(c.gt.g)then
              f=f/RADI
              c=c/SQRDX
            goto 3
            endif
            if((c+r)/f.lt.0.95*s)then
              last=0
              g=1./f
              do 12 j=1,n
                a(i,j)=a(i,j)*g
12            continue
              do 13 j=1,n
                a(j,i)=a(j,i)*f
13            continue
            endif
          endif
14      continue
      if(last.eq.0)goto 1
      return
      END
      SUBROUTINE elmhes(a,n,np)
      IMPLICIT REAL*8(A-H,O-Z)

      INTEGER(KIND=4) n,np
      REAL(KIND=8) a(np,np)
      INTEGER(KIND=4) i,j,m
      REAL(KIND=8) x,y
      do 17 m=2,n-1
        x=0.
        i=m
        do 11 j=m,n
          if(abs(a(j,m-1)).gt.abs(x))then
            x=a(j,m-1)
            i=j
          endif
11      continue
        if(i.ne.m)then
          do 12 j=m-1,n
            y=a(i,j)
            a(i,j)=a(m,j)
            a(m,j)=y
12        continue
          do 13 j=1,n
            y=a(j,i)
            a(j,i)=a(j,m)
            a(j,m)=y
13        continue
        endif
        if(x.ne.0.D0)then
          do 16 i=m+1,n
            y=a(i,m-1)
            if(y.ne.0.D0)then
              y=y/x
              a(i,m-1)=y
              do 14 j=m,n
                a(i,j)=a(i,j)-y*a(m,j)
14            continue
              do 15 j=1,n
                a(j,m)=a(j,m)+y*a(j,i)
15            continue
            endif
16        continue
        endif
17    continue
      return
      END
      REAL(KIND=8) FUNCTION plgndr(l,m,x)
      IMPLICIT REAL*8(A-H,O-Z)
C     x=COS(THETA)
      INTEGER l,m
      REAL(KIND=8) x
      INTEGER i,ll
      REAL(KIND=8) fact,pll,pmm,pmmp1,somx2
      if(m.lt.0.or.m.gt.l.or.abs(x).gt.1.D0) THEN
c        *'bad arguments in plgndr'
        PLGNDR=-999.0D0
        RETURN
      ENDIF
      pmm=1.
      if(m.gt.0) then
        somx2=sqrt((1.-x)*(1.+x))
        fact=1.
        do 11 i=1,m
          pmm=-pmm*fact*somx2
          fact=fact+2.
11      continue
      endif
      if(l.eq.m) then
        plgndr=pmm
      else
        pmmp1=x*(2*m+1)*pmm
        if(l.eq.m+1) then
          plgndr=pmmp1
        else
          do 12 ll=m+2,l
            pll=(x*(2*ll-1)*pmmp1-(ll+m-1)*pmm)/(ll-m)
            pmm=pmmp1
            pmmp1=pll
12        continue
          plgndr=pll
        endif
      endif
      return
      END FUNCTION

C  (C) Copr. 1986-92 Numerical Recipes Software 5cL&12s(5A2+Y53.
      SUBROUTINE laguer(a,m,x,its)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER(KIND=4) m,its,MAXIT,MR,MT
      REAL(KIND=8) EPSS
      COMPLEX(KIND=8) a(m+1),x
      PARAMETER (EPSS=1.D-7,MR=8,MT=10,MAXIT=MT*MR)
      INTEGER(KIND=4) iter,j
      REAL(KIND=8) abx,abp,abm,err,frac(MR)
      COMPLEX(KIND=8) dx,x1,b,d,f,g,h,sq,gp,gm,g2
      SAVE frac
      DATA frac /.5,.25,.75,.13,.38,.62,.88,1./
      do 12 iter=1,MAXIT
        its=iter
        b=a(m+1)
        err=abs(b)
        d=cmplx(0.,0.,8)
        f=cmplx(0.,0.,8)
        abx=abs(x)
        do 11 j=m,1,-1
          f=x*f+d
          d=x*d+b
          b=x*b+a(j)
          err=abs(b)+abx*err
11      continue
        err=EPSS*err
        if(abs(b).le.err) then
          return
        else
          g=d/b
          g2=g*g
          h=g2-2.*f/b
          sq=sqrt((m-1)*(m*h-g2))
          gp=g+sq
          gm=g-sq
          abp=abs(gp)
          abm=abs(gm)
          if(abp.lt.abm) gp=gm
          if (max(abp,abm).gt.0.D0) then
            dx=m/gp
          else
            dx=exp(cmplx(log(1.+abx),real(iter)))
          endif
        endif
        x1=x-dx
        if(x.eq.x1)return
        if (mod(iter,MT).ne.0) then
          x=x1
        else
          x=x-dx*frac(iter/MT)
        endif
12    continue
      x=CMPLX(-HUGE(EPSS),-HUGE(EPSS),8)
C      pause 'too many iterations in laguer'
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 5cL&12s(5A2+Y53.
      SUBROUTINE hpsort(n,ra,num)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER(KIND=4):: n
      REAL(KIND=8),DIMENSION(*):: ra
      INTEGER(KIND=4),DIMENSION(*):: num
      INTEGER(KIND=4) :: i,ir,j,l,nua
      if (n.lt.2) return
      l=n/2+1
      ir=n
10    continue
        if(l.gt.1)then
          l=l-1
          nua=num(l)
        else
          nua=num(ir)
          num(ir)=num(1)
          ir=ir-1
          if(ir.eq.1)then
            num(1)=nua
            return
          endif
        endif
        i=l
        j=l+l
20      if(j.le.ir)then
          if(j.lt.ir)then
            if(ra(num(j)).lt.ra(num(j+1)))j=j+1
          endif
          if(ra(nua).lt.ra(num(j)))then
            num(i)=num(j)
            i=j
            j=j+j
          else
            j=ir+1
          endif
        goto 20
        endif
        num(i)=nua
      goto 10
      END
C
C
C
C
C
C
C
C
C      Unterforsthuber
C
C
C
C
C
C
C
C
      LOGICAL(KIND=4) Function KombOwied(k , n  , KK,  Start )
      IMPLICIT NONE
c
c
c     Kombinationen ohne Wiederholung von k Elementen der Ordnung (des Umfangs) n
c        k    k!
c     A(C  )=--------
c        n   n!(k-n)!
c     k=3 n=2
c     12
c     13
c     23
      LOGICAL(KIND=4) :: Start
      INTEGER(KIND=4) ::I,J,KJ,k,n
      INTEGER(KIND=4),DIMENSION(*) ::KK
      KomboWied =.True.
      IF( .Not. Start) Then
        DO i = 1 , n
          KK(i) = i
        ENDDO
        Start = .True.
        RETURN
      End If

      DO j = 1 , n
        kj = n + 1 - j
        IF( KK(kj) .LT. (k + 1 - j)) Then
           KK(kj) = KK(kj) + 1
           DO i = kj + 1 , n
              KK(i) = KK(i - 1) + 1
           ENDDO
           RETURN
        End If
      ENDDO
      Start = .False.
      KomboWied = .False.
      RETURN
      End Function
C
C
C
C
C
      LOGICAL(KIND=4) Function PermoWied(k, KK,START)
      IMPLICIT NONE

c
c
c
c     R. Sedgewick Permutation Generation Methods (Algorithm 2)
c     http://www.ee.princeton.edu/~rblee/ELE572Papers/p137-sedgewick.pdf
c
c
c     Permutationen ohne Wiederholung von k Elementen
c
c        k
c     A(P  )=k!
c
c     k=3
c
c     123
c     132
c     231
c     213
c     312
c     321
c
C     Beim ersten Aufruf muss START=.FALSE. gesetzt werden
      SAVE i,c
      INTEGER(KIND=4):: k
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) ::C
      LOGICAL(KIND=4):: Start
      INTEGER(KIND=4):: i
      INTEGER(KIND=4):: j,l,kj,IER
      INTEGER(KIND=4),DIMENSION(*) ::KK
      PermoWied =.True.
      If (.Not. Start) Then
         ALLOCATE(C(K),STAT=IER)
         DO j = 1 , k
          KK(j) = j
          c(j) = 1
         ENDDO
         i = 2
         Start = .True.
         RETURN
      End If
c
c
c
      Do While (i .LE. k )
       IF( c(i) .LT. i) Then
         IF(MOD( i, 2).EQ.1) Then
              l = 1
         Else
              l = c(i)
         End If
         kj = KK(i)
         KK(i) = KK(l)
         KK(l) = kj
         c(i) = c(i) + 1
         i = 2
         RETURN
       Else
         c(i) = 1
         i = i + 1
       End If
      ENDDO
      Start = .False.
      DEALLOCATE(C)
      PermoWied = .False.
      RETURN
      END FUNCTION
C
C
C
C
      LOGICAL(KIND=4) Function VarmWied(k  , n , KK,START )
      IMPLICIT NONE


c
c
c      Variationen mit Wiederholung von k Elementen der Ordnung (des Umfangs) n
c         k    n
c      A(V  )=k
c         n
c      k=3 n=2
c      11
c      12
c      13
c      21
c      22
c      23
c      31
c      32
c      33
      LOGICAL(KIND=4):: Start
      INTEGER(KIND=4) ::i,j,k,n
      INTEGER(KIND=4),DIMENSION(*) ::KK

      VarmWied = .True.
      IF( .Not. Start) Then
         do i = 1 , n
           KK(i) = 1
         enddo
         Start = .True.
         return
      End If
      do i = n , 1 , -1
        IF(KK(i) .LT. k) Then
           KK(i) = KK(i) + 1
           do j = i + 1 , n
              KK(j) = 1
           enddo
           return
        Else
           IF( i .EQ. 1 )Then
             VarmWied = .False.
             Start = .False.
             return
           End If
        End If
      ENDDO
      RETURN
      End Function

      SUBROUTINE VEKP(N,AX,WN)

C
C
C
C     Normalenvektor für AX(1,I) I=1..N
C
C
      INTEGER(KIND=4) ::N,I
      REAL(KIND=8),DIMENSION(*) :: WN
      REAL(KIND=8),DIMENSION(N,*) :: AX
      IF(N.EQ.2) THEN
         WN(1)=AX(2,1)
         WN(2)=-AX(1,1)
      ELSE IF(N.EQ.3) THEN
         WN(1)=AX(2,1)*AX(3,2)-AX(3,1)*AX(2,2)
         WN(2)=AX(3,1)*AX(1,2)-AX(1,1)*AX(3,2)
         WN(3)=AX(1,1)*AX(2,2)-AX(2,1)*AX(1,2)
      ELSE IF(N.EQ.4) THEN
         WN(1)=+AX(2,1)*AX(3,2)*AX(4,3)
     *         +AX(2,2)*AX(3,3)*AX(4,1)
     *         +AX(2,3)*AX(3,1)*AX(4,2)
     *         -AX(2,3)*AX(3,2)*AX(4,1)
     *         -AX(2,1)*AX(3,3)*AX(4,2)
     *         -AX(2,2)*AX(3,1)*AX(4,3)
         WN(2)=-AX(1,1)*AX(3,2)*AX(4,3)
     *         -AX(1,2)*AX(3,3)*AX(4,1)
     *         -AX(1,3)*AX(3,1)*AX(4,2)
     *         +AX(1,3)*AX(3,2)*AX(4,1)
     *         +AX(1,1)*AX(3,3)*AX(4,2)
     *         +AX(1,2)*AX(3,1)*AX(4,3)
         WN(3)=+AX(1,1)*AX(2,2)*AX(4,3)
     *         +AX(1,2)*AX(2,3)*AX(4,1)
     *         +AX(1,3)*AX(2,1)*AX(4,2)
     *         -AX(1,3)*AX(2,2)*AX(4,1)
     *         -AX(1,1)*AX(2,3)*AX(4,2)
     *         -AX(1,2)*AX(2,1)*AX(4,3)
         WN(4)=-AX(1,1)*AX(2,2)*AX(3,3)
     *         -AX(1,3)*AX(2,1)*AX(3,2)
     *         -AX(1,2)*AX(2,3)*AX(3,1)
     *         +AX(1,3)*AX(2,2)*AX(3,1)
     *         +AX(1,1)*AX(2,3)*AX(3,2)
     *         +AX(1,2)*AX(2,1)*AX(3,3)
      ELSE
         DO I=1,N
           WN(I)=0.
         END DO
      ENDIF
      RETURN
      END SUBROUTINE

      SUBROUTINE INVERA (N,M,A,EPS,DET,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     A=MATRIX (EINGANG)
C     A=INVERSE MATRIX (AUSGANG)
C     B,AD,IK=HILFSVEKTOREN DER DIMENSION M
C     M=DIMENSION IM RUFENDEN PROGRAMM
C     N=AKTUELLE DIMENSION
C     EPS=TOLERANZ FUER PIVOTELEMENT
C     DET=DETERMINANTE (RELATIV; NORMIERT MIT LAENGE DER SPALTENVEKT.)
C
C
C
      DIMENSION A(M,*), B(N),AD(N),IK(N)
      IER=0
      FA=1.
      DO 1 J=1,N
      Y=0.
      DO 2 I=1,N
      Y=Y+A(I,J)**2
   2  CONTINUE
      FA=FA*Y
    1 CONTINUE
      F=SQRT(FA)
      DET=1.
      DO 10 J=1,N
      IK(J)=J
 10   CONTINUE
      DO  20 I=1,N
      K=I
      Y=A(I,I)
      IP=I+1
      IF(IP.GT.N) GOTO 33
      DO  30 J=IP,N
      W= A(I,J)
      IF(ABS(W).LE.ABS(Y))  GOTO 30
      K=J
      Y=W
 30   CONTINUE
   33 DET=DET*Y
      IF(ABS(Y).LT.EPS) THEN
        IER=-1
        RETURN
      ENDIF
      Y=1./Y
      DO  40 J=1,N
      AD(J)= A(J,K)
      A(J,K)= A(J,I)
      A(J,I)= -AD(J)*Y
      A(I,J)= A(I,J)*Y
      B(J)=A(I,J)
 40   CONTINUE
      A(I,I)=Y
      J=IK(I)
      IK(I)=IK(K)
      IK(K)=J
      DO  50 K=1,N
      DO  50 J=1,N
      IF(J.EQ.I.OR.K.EQ.I) GOTO 50
      A(K,J)= A(K,J)- B(J)*AD(K)
 50   CONTINUE
 20   CONTINUE
      DO  60 I=1,N
 70   K=IK(I)
      IF(K.EQ.I) GOTO  60
      DO  90 J=1,N
      W=A(I,J)
      A(I,J)= A(K,J)
      A(K,J)= W
 90   CONTINUE
      IP=IK(I)
      IK(I)=IK(K)
      IK(K)=IP
      DET=-DET
      GOTO 70
 60   CONTINUE
      DET=DET/F
      RETURN
      END
C
C
C

      SUBROUTINE TRIINV(MDC,MZ,MS,C,MDA,A,IER)
      IMPLICIT NONE
      INTEGER(KIND=4) ::MDA,MDC,MZ,MS,J,I,IER
      REAL(KIND=8),DIMENSION (MDC,*) ::C
      REAL(KIND=8),DIMENSION (MDA,*) ::A
C
C
C      Umspeichern
C
C
       IER=0
       CALL TRIMAT(MDC,MZ,MS,C,IER)

       DO J=1,MS
          DO I=J,MS
             A(J,I)=C(J,I)
          END DO
       END DO


C
C
C
C       T   -1
C     (A *A)   berechnen
C
C
      CALL COVTRA(MDA,MS,A,IER)
C
C     Untere Dreiecksmatrix auffüllen
C
C
      DO J=1,MS
         DO I=J+1,MS
           A(I,J)=A(J,I)
         END DO
      END DO


      RETURN

      END SUBROUTINE
C
C

C
C
C
      SUBROUTINE TRIMAT(MDC,MZ,MS,C,IER)
      IMPLICIT NONE
      INTEGER(KIND=4) ::MDC,MZ,MS,J,JL,I,IER
      REAL(KIND=8)::RMO,RAN
      REAL(KIND=8),DIMENSION (MDC,*) ::C
      DATA RAN/1.D300/

C      OBERE DREIECKSFORM HERSTELLEN
C
       IER=0
       DO J=1,MS
         CALL H12(1,J,J+1,MZ,C(1,J),1,RMO,C(1,J+1),1,MDC,MS-J,RAN)
       ENDDO
       DO J=2,MS+1
          JL=J-1
          DO I=1,JL
              C(J,I)=0.
          ENDDO
       ENDDO
      RETURN
      END SUBROUTINE




      SUBROUTINE COVTRA(MDA,N,A,IER)
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(MDA,*)
      DOUBLE PRECISION S,SI,SJ
      DATA RANG/1.D80/,RANGIN/1.D-300/,SRANGE/1.D300/
C 
C                                         T 
C     BERECHNUNG DER COVARIANZ-MATRIX (A*A )**(-1) AUS DER
C     OBEREN DREIECKSMATRIX A
C     s. C.L. Lawson and R.J.Hanson:"Solving Least Squares Problems" (1974) S.69
C 
C 
      IER=0
      DO 10 I=1,N
      IF(ABS(A(I,I)).LT.RANGIN) THEN
        IER=-1
        RETURN
      ENDIF
      A(I,I)=1./A(I,I)
  10  CONTINUE
      IF(N.EQ.1) GOTO 50
C 
      DO 20 I=1,N-1 
      DO 20 J=I+1,N 
      S=0.
      DO 30 L=I,J-1 
      SI=A(I,L)
      SJ=A(L,J)
      S=S+SI*SJ 
  30  CONTINUE
      IF(ABS(S).GT.SRANGE) S=SRANGE
      A(I,J)=-A(J,J)*S
  20  CONTINUE
C 
C 
  50  DO 60 I=1,N 
      DO 60 J=I,N 
      S=0.
      DO 70 L=J,N 
      SI=A(I,L)
      SJ=A(J,L)
      S=S+SI*SJ         
  70  CONTINUE
      IF(ABS(S).LT.SRANGE) THEN
         A(I,J)=S
      ELSE
         A(I,J)=1.
      ENDIF
  60  CONTINUE
      RETURN
      END SUBROUTINE
C
C
C
C
C
C
C
C
C
C
C
C
C     *******************************************
C     3 dimensionale exponentielle Splines
C     *******************************************

C
C
C
C
C
C
      REAL(KIND=8) FUNCTION SPLVALD3(X1,X2,X3,N,M,L,X1A,X2A,X3A,YA,Y2A,
     *                               XSI,YP1,YPN,YDX)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C
C
C
      INTERFACE
      SUBROUTINE splin3X(n,m,l,x1a,x2a,x3a,ya,y2a,x1,x2,x3,YY,XSI,YDX)
      INTEGER(KIND=4):: n,m,l
      REAL(KIND=8),DIMENSION(*):: x1a,x2a,x3a
      REAL(KIND=8),DIMENSION(n,m,*):: y2a,ya
      REAL(KIND=8),DIMENSION(M,*):: yy
      REAL(KIND=8),OPTIONAL,DIMENSION(M,*) ::YDX
      REAL(KIND=8):: x1,x2,x3,XSI
      END subroutine
      REAL(KIND=8) FUNCTION SPLVALD2(X1,X2,N,M,X1A,X2A,YA,Y2A,
     *                               XSI,YP1,YPN,YDX)
      INTEGER(KIND=4) ::N,M
      REAL(KIND=8),DIMENSION(N,*) ::YA,Y2A
      REAL(KIND=8),DIMENSION(*) :: X1A,X2A
      REAL(KIND=8) ::X1,X2,XSI,YP1,YPN
      REAL(KIND=8),OPTIONAL,DIMENSION(*) ::YDX
      END FUNCTION
      end Interface
C
C
C
C
C
      REAL(KIND=8),DIMENSION(N,M,*) ::YA,Y2A
      REAL(KIND=8),DIMENSION(*) :: X1A,X2A,X3A
      REAL(KIND=8) ::X1,X2,X3
      REAL(KIND=8),DIMENSION(M,L) ::YY,YY2,YYDX
      REAL(KIND=8),DIMENSION(*),OPTIONAL :: YDX

C
C
C     Stützstellen YY für X1=const bestimmen
C
      IF(PRESENT(YDX)) THEN
       Call splin3X(n,m,l,x1a,x2a,x3a,ya,y2a,x1,x2,x3,YY,XSI,YYDX)
      ELSE
       Call splin3X(n,m,l,x1a,x2a,x3a,ya,y2a,x1,x2,x3,YY,XSI)
      ENDIF
C
C
C     2. Ableitungen (yy2) aus X2a,X3a,yy berechnen
C
      Call splie2X(M,L,x2a,x3a,yy,yy2,XSI,YP1,YPN)

C
C     2 dimensionaler Spline für x1=const  berechnen
C
C
      IF(PRESENT(YDX)) THEN
C
C      Funktionswert für X1,X2,X3
       SPLVALD3=SPLVALD2(X2,X3,M,L,X2A,X3A,YY,YY2,
     *                               XSI,YP1,YPN,YDX(2))
C
C      DY/DX1 berechnen
C
C      2. Ableitungen (yy2) aus X2a,X3a,yydx berechnen
C
       Call splie2X(M,L,x2a,x3a,yydx,yy2,XSI,YP1,YPN)
C
       YDX(1)=SPLVALD2(X2,X3,M,L,X2A,X3A,YYdx,YY2,
     *                               XSI,YP1,YPN)

      ELSE
       SPLVALD3=SPLVALD2(X2,X3,M,L,X2A,X3A,YY,YY2,
     *                               XSI,YP1,YPN)
      ENDIF
C
C
C
C
C
C
      RETURN
      END FUNCTION
C
C
C
C
C
C     Splineapproximation für 3-dimensionale Tabellen
C
C
C
C
C
C
      SUBROUTINE splie3X(N,M,L,x1a,x2a,x3a,ya,y2a,XSI,YP1,YPN)
C
C
C     Berechnung der 2.-Ableitungen für die Gitterpunkte
C
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTEGER(KIND=4) n,m,l
      REAL(KIND=8) :: XSI,YP1,YPN
      REAL(KIND=8),DIMENSION(N,M,*):: y2a,ya
      REAL(KIND=8),DIMENSION(*):: x1a,x2a,x3a
C
CU    USES splinex
C
      INTEGER(KIND=4) :: i2,I3
      do i3=1,l
      do i2=1,m
        call splineX(n,x1a,ya(1,I2,i3),y2a(1,i2,i3),xsi,YP1,YPN)
      enddo
      end do
      return
      END
C
C
C
C
      SUBROUTINE splin3X(N,M,L,x1a,x2a,x3a,ya,y2a,x1,x2,x3,YY,XSI,YDX)
C
C
C     Berechnung einer 2-dimensionalen Tabelle (yy) für x1=const
C
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)

      INTERFACE
      SUBROUTINE SPLINTX(N,X,Y,Y2EXP,XS,YS,XSI,YSDX)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8),DIMENSION(*) ::X,Y,Y2EXP
      REAL(KIND=8),OPTIONAL ::YSDX
      INTEGER(KIND=4):: N
      REAL(KIND=8) ::XSI,XS,YS
      END subroutine
      end Interface

      INTEGER(KIND=4):: n,m,l
      REAL(KIND=8),DIMENSION(*):: x1a,x2a,x3a
      REAL(KIND=8),DIMENSION(n,m,*):: y2a,ya
      REAL(KIND=8),DIMENSION(M,*):: yy
      REAL(KIND=8),OPTIONAL,DIMENSION(M,*) ::YDX
      REAL(KIND=8):: x1,x2,x3
C
CU    USES splintx
C
      INTEGER(KIND=4):: i2,i3
      do i3=1,l
      do i2=1,m
        IF(PRESENT(YDX)) THEN
           call splintx(n,x1a,ya(1,i2,i3),y2a(1,i2,i3),
     *                    x1,yy(i2,i3),XSI,YDX(I2,I3))
        ELSE
           call splintx(n,x1a,ya(1,i2,i3),y2a(1,i2,i3),
     *                    x1,yy(i2,i3),XSI)
        ENDIF
      enddo                                                         
      end do
      return
      END
C
c
c
c
c
c
c
c
c
c
C
C     *******************************************
C     2 dimensionale exponentielle Splines
C     *******************************************
C
      REAL(KIND=8) FUNCTION SPLVALD2(X1,X2,N,M,X1A,X2A,YA,Y2A,
     *                               XSI,YP1,YPN,YDX)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C
C
C
C
C

      INTERFACE
      SUBROUTINE SPLINTX(N,X,Y,Y2EXP,XS,YS,XSI,YSDX)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8),DIMENSION(*) ::X,Y,Y2EXP
      REAL(KIND=8),OPTIONAL ::YSDX
      INTEGER(KIND=4):: N
      REAL(KIND=8) ::XSI,XS,YS
      END subroutine
      SUBROUTINE splin2X(N,M,x1a,x2a,ya,y2a,x1,x2,YY,XSI,YDX)
      INTEGER(KIND=4):: n,m
      REAL(KIND=8),DIMENSION(*):: x1a,x2a
      REAL(KIND=8),DIMENSION(n,*):: y2a,ya
      REAL(KIND=8),DIMENSION(*)::  yy
      REAL(KIND=8),DIMENSION(*),OPTIONAL::  ydx
      REAL(KIND=8) :: x1,x2,XSI
      END SUBROUTINE
      end Interface
C
C
C
C
      INTEGER(KIND=4) :: N,M
      REAL(KIND=8),DIMENSION(N,*) ::YA,Y2A
      REAL(KIND=8),DIMENSION(*) :: X1A,X2A
      REAL(KIND=8) ::X1,X2,YS,XSI,YP1,YPN
      REAL(KIND=8),DIMENSION(M) ::YY,YY2,YYDX
      REAL(KIND=8),OPTIONAL,DIMENSION(*) ::YDX

C
C
C     Stützstellen YY für X1=const bestimmen
C

      IF(PRESENT(YDX)) THEN
       call splin2X(n,m,x1a,x2a,ya,y2a,x1,x2,YY,XSI,YYDX)

      ELSE
       call splin2X(n,m,x1a,x2a,ya,y2a,x1,x2,YY,XSI)

      ENDIF

C
C     2. Ableitungen YY2 aus YY bestimmen
C
C
      call splinex(m,x2a,yy,yy2,XSI,YP1,YPN)


C
C     Wert für X2 bestimmen
C
C
      IF(PRESENT(YDX)) THEN
        call splintx(m,x2a,yy,yy2,x2,ys,XSI,YDX(2))
C
C       2. Ableitungen YY2 aus YYDX bestimmen
C
C
        call splinex(m,x2a,yydx,yy2,XSI,YP1,YPN)
C
C
C       DY/DX1
C
        call splintx(m,x2a,yydx,yy2,x2,YDX(1),XSI)


      ELSE
        call splintx(m,x2a,yy,yy2,x2,ys,XSI)
      ENDIF

      SPLVALD2=YS
      RETURN
      END FUNCTION

C
C
C
C     Splineapproximation für 2-dimensionale Tabellen
C
C
      SUBROUTINE splie2X(N,M,x1a,x2a,ya,y2a,XSI,YP1,YPN)
C
C
C     Berechnung der 2.-Ableitungen (Y2a) aus ya für die Gitterpunkte x1a,x2a
C
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTEGER(KIND=4) :: I2
      INTEGER(KIND=4) :: N,M
      REAL(KIND=8) :: XSI,YP1,YPN
      REAL(KIND=8),DIMENSION(*):: x1a,x2a
      REAL(KIND=8),DIMENSION(N,*):: y2a,ya
CU    USES splinex
      DO  I2=1,M
        call splineX(N,x1a,ya(1,i2),y2a(1,i2),XSI,YP1,YPN)
      enddo
      RETURN
      END SUBROUTINE
C
      SUBROUTINE splin2X(N,M,x1a,x2a,ya,y2a,x1,x2,YY,XSI,YDX)
C
C
C     Berechnung einer 1-dimensionalen Tabelle (yy) für x1=const
C
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C
C
      INTERFACE
      SUBROUTINE SPLINTX(N,X,Y,Y2EXP,XS,YS,XSI,YSDX)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8),DIMENSION(*) ::X,Y,Y2EXP
      REAL(KIND=8),OPTIONAL ::YSDX
      INTEGER(KIND=4):: N
      REAL(KIND=8) ::XSI,XS,YS
      END subroutine
      END INTERFACE
C
C
      INTEGER(KIND=4):: n,m
      REAL(KIND=8),DIMENSION(*):: x1a,x2a
      REAL(KIND=8),DIMENSION(n,*):: y2a,ya
      REAL(KIND=8),DIMENSION(*)::  yy
      REAL(KIND=8),DIMENSION(*),OPTIONAL::  ydx

      REAL(KIND=8) :: x1,x2
C
CU    USES splintx
C
      INTEGER(KIND=4):: i2
      do  i2=1,m
      IF(PRESENT(YDX)) THEN
        call splintx(n,x1a,ya(1,i2),y2a(1,i2),x1,yy(i2),XSI,YDX(I2))
      ELSE
        call splintx(n,x1a,ya(1,i2),y2a(1,i2),x1,yy(i2),XSI)
      ENDIF
      enddo
      return
      END
C
C
C
C
C
C
C
C     *******************************************
C     1 dimensionale exponentielle Splines
C     *******************************************
C
C
      SUBROUTINE SPLINTX(N,X,Y,Y2EXP,XS,YS,XSI,YSDX)
C
C
C     Berechnung des Funktionswertes (YS) und optional YSDX (dYS/dXS) aus den N Stützstellen Y und den 2. Ableitungen Y2EXP
C     an der Stelle  XS  für exponentielle Splines (XSI=Daempfung)
C     (s. auch Numercal Recipes S. 109ff)
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8),DIMENSION(*) ::X,Y,Y2EXP
      REAL(KIND=8),OPTIONAL ::YSDX
      INTEGER(KIND=4):: K,KHI,KLO
      REAL(KIND=8) ::H,XSI,T,XS,YS
      KLO=1
      KHI=N
      DO WHILE ((KHI-KLO).GT.1)
        K=(KHI+KLO)/2
        IF(X(K).GT.XS) THEN
           KHI=K
        ELSE
           KLO=K
        ENDIF
      END DO
      H=X(KHI)-X(KLO)
      IF(H.EQ.0.D0) THEN
        YS=HUGE(1.D0)
        RETURN
      ENDIF
      T=(XS-X(KLO))/H
      HP=XSI*H
      HH=H**2
      YS=T*Y(KLO+1)+(1.-T)*Y(KLO)
     *+HH*(Y2EXP(KLO+1)*EEX(HP,T)+Y2EXP(KLO)*EEX(HP,1.-T))
      IF(PRESENT(YSDX)) THEN
         YSDX=(Y(KLO+1)-Y(KLO)
     *      +HH*(Y2EXP(KLO+1)*EEXDT(HP,T)-Y2EXP(KLO)*EEXDT(HP,1.-T)))/H
      ENDIF
      RETURN
      END SUBROUTINE
C

C
C
C
      SUBROUTINE SPLINEX(N,X,Y,Y2EXP,XSI,YP1,YPN)
C
C
C
C     Berechnung der 2. Ableitungen für 1 dimensionalen Exponentielle Splines
C
C     N Anzahl Stuetzstellen
C     X Abszissenwerte der Stützstellen
C     Y Ordinatenwerte der Stützstellen
C     YP1 1.-Ableitung für X(1) (2. Ableitung= 0 falls YP1>99.E30)
C     YPN 1.-Ableitung für X(N) (2. Ableitung= 0 falls YPN>99.E30)
c     XSI Daempfung für exponentiellen Spline
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8),DIMENSION(*) ::X,Y,Y2EXP
      REAL(KIND=8),DIMENSION(N) :: U
      INTEGER(KIND=4):: I,K
      REAL(KIND=8) ::  QN,UN,YP1,YPN,XSI
c
c
      HM=(X(2)-X(1))
      ALPHM=RR(HM*XSI)*HM
      IF(YP1.GT.0.99D30) THEN
        Y2EXP(1)=0.
        U(1)=0.
      ELSE
        Y2EXP(1)=-ALPHM/(QQ(HM*XSI)*HM)
        U(1)=((Y(2)-Y(1))/HM-YP1)/(QQ(HM*XSI)*HM)
      ENDIF
      DO I=2,N-1
        HP=X(I+1)-X(I)
        BET=(Y(I+1)-Y(I))/HP-(Y(I)-Y(I-1))/HM
        ALPH=QQ(HM*XSI)*HM+QQ(HP*XSI)*HP
        ALPHP=RR(HP*XSI)*HP
        Y2EXP(I)=-ALPHP/(Y2EXP(I-1)*ALPHM+ALPH)
        U(I)=(2.*BET/ALPH-2.*ALPHM*U(I-1)/ALPH)/
     *       (2.*Y2EXP(I-1)*ALPHM/ALPH+2.)
        ALPHM=ALPHP
        HM=HP
      END DO
      IF(YPN.GT.0.99D30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=+ALPHP/(QQ(HP*XSI)*HP)
        UN=(YPN-(Y(N)-Y(N-1))/HP)/(QQ(HP*XSI)*HP)
      ENDIF
      Y2EXP(N)=(UN-QN*U(N-1))/(QN*Y2EXP(N-1)+1.)
      DO K=N-1,1,-1
        Y2EXP(k)=Y2EXP(K)*Y2EXP(K+1)+U(K)
      END DO
      RETURN
      END SUBROUTINE
c
c
c
C
C
      SUBROUTINE jacobi(a,n,np,d,v,nrot,ier)
      IMPLICIT REAL(KIND=8)(A-H,O-Z)
      INTEGER n,np,nrot,NMAX
      REAL(KIND=8) a(np,np),d(np),v(np,np)
      INTEGER i,ip,iq,j
      REAL(KIND=8) c,g,h,s,sm,t,tau,theta,tresh,b(np),z(np)
      IER=0
      do 12 ip=1,n
        do 11 iq=1,n
          v(ip,iq)=0.
11      continue
        v(ip,ip)=1.
12    continue
      do 13 ip=1,n
        b(ip)=a(ip,ip)
        d(ip)=b(ip)
        z(ip)=0.
13    continue
      nrot=0
      do 24 i=1,50
        sm=0.
        do 15 ip=1,n-1
          do 14 iq=ip+1,n
            sm=sm+abs(a(ip,iq))
14        continue
15      continue
        if(sm.eq.0.D0)return
        if(i.lt.4)then
          tresh=0.2*sm/n**2
        else
          tresh=0.
        endif
        do 22 ip=1,n-1
          do 21 iq=ip+1,n
            g=100.*abs(a(ip,iq))
            if((i.gt.4).and.(abs(d(ip))+
     &      g.eq.abs(d(ip))).and.(abs(d(iq))+g.eq.abs(d(iq))))then
              a(ip,iq)=0.
            else if(abs(a(ip,iq)).gt.tresh)then
              h=d(iq)-d(ip)
              if(abs(h)+g.eq.abs(h))then
                t=a(ip,iq)/h
              else
                theta=0.5*h/a(ip,iq)
                t=1./(abs(theta)+sqrt(1.+theta**2))
                if(theta.lt.0.D0)t=-t
              endif
              c=1./sqrt(1+t**2)
              s=t*c
              tau=s/(1.+c)
              h=t*a(ip,iq)
              z(ip)=z(ip)-h
              z(iq)=z(iq)+h
              d(ip)=d(ip)-h
              d(iq)=d(iq)+h
              a(ip,iq)=0.
              do 16 j=1,ip-1
                g=a(j,ip)
                h=a(j,iq)
                a(j,ip)=g-s*(h+g*tau)
                a(j,iq)=h+s*(g-h*tau)
16            continue
              do 17 j=ip+1,iq-1
                g=a(ip,j)
                h=a(j,iq)
                a(ip,j)=g-s*(h+g*tau)
                a(j,iq)=h+s*(g-h*tau)
17            continue
              do 18 j=iq+1,n
                g=a(ip,j)
                h=a(iq,j)
                a(ip,j)=g-s*(h+g*tau)
                a(iq,j)=h+s*(g-h*tau)
18            continue
              do 19 j=1,n
                g=v(j,ip)
                h=v(j,iq)
                v(j,ip)=g-s*(h+g*tau)
                v(j,iq)=h+s*(g-h*tau)
19            continue
              nrot=nrot+1
            endif
21        continue
22      continue
        do 23 ip=1,n
          b(ip)=b(ip)+z(ip)
          d(ip)=b(ip)
          z(ip)=0.
23      continue
24    continue
      IER=-10
c      pause 'too many iterations in jacobi'
      return
      END
C
      SUBROUTINE RPOLY(OP, DEGREE, ZEROR, ZEROI,                         RPO  10
     * FAIL)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)                                                           RPO  20
C FINDS THE ZEROS OF A REAL POLYNOMIAL                                  RPO  30
COP - DOUBLE PRECISION VECTOR OF COEFFICIENTS IN                        RPO  40
C      ORDER OF DECREASING POWERS.                                      RPO  50
C DEGREE - INTEGER DEGREE OF POLYNOMIAL.                                RPO  60
C ZEROR, ZERO! - OUTPUT DOUBLE PRECISION VECTORS OF                     RPO  70
C               REAL AND IMAGINARY PARTS OF THE                         RPO   80
C               ZEROS.                                                  RPO  90
C FAIL - OUTPUT LOGICAL PARAMETER, TRUE ONLY IF                         RPO 100
C        LEADING COEFFICIENT IS ZERO OR IF RPOLY                        RPO 110
C        HAS FOUND FEWER THAN DEGREE ZEROS.                             RPO 120
C        IN THE LATTER CASE DEGREE IS RESET TO                          RPO 130
C        THE NUMBER OF ZEROS FOUND.                                     RPO 140
C TO CHANGE THE SIZE OF POLYNOMIALS WHICH CAN BE                        RPO 150
C SOLVED, RESET THE DIMENSIONS OF THE ARRAYS IN THE                     RPO 160
C COMMON AREA AND IN THE FOLLOWING DECLARATIONS.                        RPO 170 
C THE SUBROUTINE USES SINGLE PRECISION CALCULATIONS                     RPO 180 
C FOR SCALING, BOUNDS AND ERROR CALCULATIONS. ALL                       RPO 190
C        OP(1)*X**DEGREE+OP(2)*X**(DEGREE-1).... OP(DEGREE+1)





c    •     M.A. Jenkins

C CALCULATIONS FOR THE ITERATIONS ARE DONE IN DOUBLE                     RPO 200 
C PRECISION.                                                                 RPO 210 
      COMMON /GLOBAL/ P, QP, K, QK, SVK, SR, SI, U,                      RPO 220 
     * V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                       RPO 230
     * H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                       RPO 240 
      DOUBLE PRECISION P(101), QP(101), K(101),                           RPO  250
     * QK (101), SVK(101) , SR, SI, U, V , A, B, C, D,                      RPO  260
     * A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                           RPO  270
     * LZR, LZI                                                             RPO  280 
      REAL(KIND=8) ETA, ARE, MRE                                                   RPO  290
      INTEGER N, NN                                                        RPO  300 
      DOUBLE PRECISION OP(*), TEMP(101),                                 RPO  310
     * ZEROR(*), ZEROI(*), T, AA, BB, CC,                                RPO 320
     * FACTOR                                                               RPO  330
      REAL(KIND=8) PT(101), LO, MAXX, MINX, XX, YY, COSR,                            RPO  340
     * SINR, XXX, X, SC, BND, XM, FF, DF, DX, INFIN,                       RPO  350
     * SMALNO, BASE                                                        RPO 360
      INTEGER DEGREE, CNT, NZ, I, J, JJ, NM1,L                              RPO  370
      LOGICAL FAIL, ZEROK                                                  RPO  380 
C THE FOLLOWING STATEMENTS SET MACHINE CONSTANTS USED                    RPO  390 
C IN VARIOUS PARTS OF THE PROGRAM. THE MEANING OF THE                    RPO  400 
C FOUR CONSTANTS ARE •..                                                    RPO  410 
C ETA     THE MAXIMUM RELATIVE REPRESENTATION ERROR                      RPO  420 
C         WHICH CAN BE DESCRIBED AS THE SMALLEST                          RPO  430 
C         POSITIVE FLOATING POINT NUMBER SUCH THAT                       RPO  440 
C         l.D0+ETA IS GREATER THAN l.                                     RPO  450 
C INFINY THE LARGEST FLOATING-POINT NUMBER.                              RPO  460 
C SMALNO THE SMALLEST POSITIVE FLOATING-POINT NUMBER                    RPO  470 
C         IF THE EXPONENT RANGE DIFFERS IN SINGLE AND                    RPO  480 
C         DOUBLE PRECISION THEN SMALNO AND INFIN                          RPO  490 
C         SHOULD INDICATE THE SMALLER RANGE.                              RPO  500 
C BASE    THE BASE OF THE FLOATING-POINT NUMBER                           RPO  510 
C         SYSTEM USED.                                                     RPO  520 
C THE VALUES BELOW CORRESPOND TO THE BURROUGHS 86700                     RPO  530 
      BASE=2.0D0                                                            RPO  540
C      ETA= .5*BASE**(1-26)
      ETA=TINY(BASE)                                                     RPO  550
      INFIN= 2.2D300                                                     RPO  560
      SMALNO = 1.0D-300                                                      RPO  570
CARE AND MRE REFER TO THE UNIT ERROR IN+ AND*                          RPO  580 
C RESPECTIVELY. THEY ARE ASSUMED TO BE THE SAME AS                       RPO  590 
C ETA.                                                                       RPO  600 
      ARE = ETA                                                             RPO  610
      MRE = ETA                                                             RPO  620 
      LO= SMALNO/ETA                                                       RPO  630 
C INITIALIZATION OF CONSTANTS FOR SHIFT ROTATION                          RPO  640 
      XX= .70710678                                                       RPO  650
      YY = -XX                                                              RPO  660
      COSR = -.069756474D0                                                    RPO  670
      SINR = .99756405D0                                                      RPO  680
      FAIL= .FALSE.                                                       RPO  690 
      N = DEGREE                                                            RPO  700 
      NN = N + 1
C ALGORITHM FAILS IF THE LEADING COEFFICIENT IS ZERO.                    RPO  720
      IF (OP(1).NE.0.D0) GO TO 10                                          RPO  730
      FAIL= .TRUE.                                                         RPO  740 
      DEGREE= 0
      RETURN                                                                RPO  760
C REMOVE THE ZEROS AT THE ORIGIN IF ANY                                   RPO  770 
   10 IF (OP(NN).NE.0.0D0) GO TO 20                                       RPO  780
      J = DEGREE - N + 1                                                  RPO  790 
      ZEROR(J) = 0.D0                                                      RPO  800 
      ZEROI(J) = 0.D0                                                      RPO  810
      NN = NN - 1                                                           RPO  820
      N = N - 1
      GO TO 10                                                              RPO  840
C MAKE A COPY OF THE COEFFICIENTS                                          RPO  850 
   20 DO 30 I=1,NN                                                          RPO  860
        P(I)=OP(I)                                                         RPO  870
   30 CONTINUE
C START THE ALGORITHM FOR ONE ZERO                                        RPO  890
   40 IF (N.GT.2) GO TO 60
                                                                          RPO  900
      IF (N.LT.1) RETURN                                                   RPO  910
C CALCULATE THE FINAL ZERO OR PAIR OF ZEROS                               RPO  920 
      IF (N.EQ.2) GO TO 50                                                 RPO  930 
      ZEROR(DEGREE) =-P(2)/P(l)                                            RPO 940
      ZEROI(DEGREE) = 0.0D0                                                RPO 950
      RETURN                                                                RPO 960 
   50 CALL QUAD(P(1), P(2), P(3), ZEROR(DEGREE-1),                         RPO 970
     * ZEROI(DEGREE-1), ZEROR(DEGREE), ZEROI(DEGREE))
      RETURN                                                                RPO 990
C FIND LARGEST AND SMALLEST MODULI OF COEFFICIENTS.                       RPO 1000 
   60 MAXX= 0.                                                              RPO 1010
      MINX= INFIN                                                           RPO 1020
      DO 70 I=1,NN                                                          RPO 1030
        X= ABS(P(I))                                                       RPO 1040
        IF (X.GT.MAXX) MAXX= X                                             RPO 1050
        IF (X.NE.0.D0.AND.X.LT.MINX) MINX= X                              RPO 1060
   70 CONTINUE                                                              RPO 1070 
C SCALE IF THERE ARE LARGE OR VERY SMALL COEFFICIENTS                     RPO 1080 
C COMPUTES A SCALE FACTOR TO MULTIPLY THE                                 RPO 1090 
C COEFFICIENTS OF îHE POLYNOMIAL. THE SCALING IS DONE                    RPO 1100 
C TO AVOID OVERFLOW AND TO AVOID UNDETECTED UNDERFLOW                     RPO 1110 
C INTERFERING WITH THE CONVERGENCE CRITERION.                             RPO 1120 
C THE FACTOR IS A POWER OF THE BASE                                       RPO 1130 
      SC= LO/MINX                                                           RPO 1140
      IF (SC.GT.1.0D0) GO TO 80                                              RPO 1150
      IF (MAXX.LT.10.D0) GO TO 110                                           RPO 1160
      IF (SC.EQ.0.D0) SC= SMALNO                                            RPO 1170
      GO TO 90                                                              RPO 1180 
   80 IF (INFIN/SC.LT.MAXX) GO TO 110                                      RPO 1190
   90 L = LOG(SC)/LOG(BASE) + .5
      FACTOR= (BASE*1.0D0)**L                                              RPO 1210
      IF (FACTOR.EQ.1.0D0) GO TO 110                                       RPO 1220
      DO 100 I=1,NN                                                         RPO 1230
        P(I) = FACTOR*P(I)                                                  RPO 1240 
  100 CONTINUE
C COMPUTE LOWER BOUND ON MODULI OF ZEROS.                                 RPO 1260
  110 DO 120 I=1,NN                                                         RPO 1270
        PT(I) = ABS(P(I))                                                  RPO 1280
  120 CONTINUE                                                              RPO 1290 
      PT(NN) = -PT(NN)                                                      RPO 1300 
C COMPUTE UPPER ESTIMATE OF BOUND                                          RPO 1310 
      X= EXP((LOG(-PT(NN))-LOG(PT(1)))/FLOAT(N))                        RPO 1320
      IF (PT(N).EQ.0.D0) GO TO 130                                          RPO 1330
C IF NEWTON STEP AT THE ORIGIN IS BETTER, USE IT.                         RPO 1340 
      XM = -PT(NN)/PT(N)                                                    RPO 1350 
      IF (XM.LT.X) X= XM                                                  RPO 1360 
C CHOP THE INTERVAL (0,X} UNTIL FF .LE. 0                                 RPO 1370 
  130 XM = X*0.1                                                             RPO 1380
      FF= PT(1)                                                            RPO 1390
      DO 140 I=2,NN                                                         RPO 1400 
        FF= FF*XM + PT(I)                                                 RPO 1410
  140 CONTINUE                                                              RPO 1420 
      IF (FF.LE.0.D0) GO TO 150                                              RPO 1430
      X= XM                                                                RPO 1440 
      GO TO 130                                                             RPO 1450 
  150 DX= X                                                                RPO 1460 
C DO NEWTON ITERATION UNTIL X CONVERGES TO TWO                            RPO 1470 
C DECIMAL PLACES                                                            RPO 1480 
  160 IF (ABS(DX/X).LE.0.005D0) GO TO 180                                    RPO 1490
      FF= PT(1)                                                            RPO 1500
      DF= FF                                                               RPO 1510
      DO 170 I=2,N                                                          RPO 1520 
        FF= FF*X + PT(I)                                                  RPO 1530 
        DF= DF*X + FF                                                     RPO 1540
  170 CONTINUE                                                              RPO 1550 
      FF= FF*X + PT(NN)                                                   RPO 1560
      DX= FF/DF                                                            RPO 1570
      X= X - DX                                                           RPO 1580
      GO TO 160                                                             RPO 1590 
  180 BND= X                                                               RPO 1600 
C COMPUTE THE DERIVATIVE AS THE INTIAL K POLYNOMIAL                       RPO 1610 
C AND DO 5 STEPS WITH NO SHIFT                                             RPO 1620 
      NM1 = N - 1                                                           RPO 1630
      DO 190 I=2,N                                                          RPO 1640 
        K(I) = FLOAT(NN-I)*P(I)/FLOAT(N)                                   RPO 1650 
  190 CONTINUE                                                              RPO 1660 
      K(1) = P(1)                                                           RPO 1670
      AA= P(NN)                                                            RPO 1680 
      BB= P(N)                                                             RPO 1690 
      ZEROK = K(N) .EQ.0.D0                                                 RPO 1700
      DO 230 JJ=1,5                                                        RPO 1710
        CC=K(N)                                                          RPO 1720
        IF (ZEROK) GO TO 210                                              RPO 1730 
CUSE SCALED FORM OF RECURRENCE IF VALUE OF KAT 0 IS                    RPO 1740 
C NONZERO                                                                   RPO 1750 
        T = -AA/CC                                                         RPO 1760 
        DO 200 I=1,NM1                                                    RPO 1770
          J = NN - I                                                       RPO 1780 
          K(J) = T*K(J-1) + P(J)                                          RPO 1790 
  200 CONTINUE                                                           RPO 1800 
        K(1) = P(1)                                                         RPO 1810
        ZEROK = ABS(K(N)).LE.ABS(BB)*ETA*10.D0                             RPO 1820
        GO TO 230                                                          RPO 1830 
CUSE UNSCALED FORM OF RECURRENCE                                          RPO 1840 
  210 DO 220 I=l,NM1                                                    RPO 1850
          J = NN - I                                                       RPO 1860 
          K(J) = K(J-1)                                                    RPO 1870
  220 CONTINUE                                                           RPO 1880 
        K (1) = 0. D0                                                        RPO 1890 
        ZEROK = K(N).EQ.0.0D0                                              RPO 1900
  230 CONTINUE                                                              RPO 1910 
C SAVE K FOR RESTARTS WITH NEW SHIFTS                                     RPO 1920 
      DO 240 I=1,N                                                         RPO 1930
        TEMP(I) = K(I)                                                     RPO 1940
  240 CONTINUE                                                              RPO 1950 
C LOOP TO SELECT THE QUADRATIC CORRESPONDING TO EACH                    RPO 1960 
C NEW SHIFT                                                                RPO 1970 
      DO 280 CNT=1,50                                                      RPO 1980
C QUADRATIC CORRESPONDS TO A DOUBLE SHIFT TO A                           RPO 1990 
C NON-REAL POINT AND ITS COMPLEX CONJUGATE. THE POINT                    RPO 2000 
CHAS MODULUS BND ANO AMPLITUDE ROTATED BY 94 DEGREES                    RPO 2010 
C FROM THE PREVIOUS SHIFT                                                  RPO 2020 
        XXX= COSR*XX - SINR*YY                                            RPO 2030 
        YY = SINR*XX + COSR*YY                                             RPO 2040
        XX= XXX                                                           RPO 2050
        SR= BND*XX                                                        RPO 2060 
        SI= BND*YY                                                        RPO 2070 
        U= -2.0D0*SR                                                      RPO 2080 
        V= BND                                                            RPO 2090 
C SECOND STAGE CALCULATION, FIXED QUADRATIC
        CALL FXSHFR(20*CNT, NZ)
        IF (NZ.EQ.0) GO TO 260                                            RPO 2120
C THE SECOND STAGE JUMPS DIRECTLY TO ONE OF THE THIRD                    RPO 2130 
C STAGE ITERATIONS AND RETURNS HERE IF SUCCESSFUL.                        RPO 2140 
C DEFLATE THE POLYNOMIAL, STORE THE ZERO OR ZEROS AND                    RPO 2150 
C RETURN TO THE MAIN ALGORITHM.                                            RPO 2160 
        J = DEGREE - N + 1                                                RPO 2170 
        ZEROR(J) = SZR                                                     RPO 2180 
        ZEROI(J) = SZI                                                     RPO 2190 
        NN = NN - NZ                                                       RPO 2200 
        N = NN - 1
        DO 250 I=1,NN                                                      RPO 2220
          P (I) = QP (I)                                                      RPO 2230 
  250   CONTINUE
        IF (NZ.EQ.1) GO TO 40                                             RPO 2250
        ZEROR(J+1) = LZR                                                  RPO 2260
        ZEROI(J+1) = LZI
        GO TO 40                                                           RPO 2280
C IF THE ITERATION IS UNSUCCESSFUL ANOTHER QUADRATIC                      RPO 2290 
CIS CHOSEN AFTER RESTORING K                                             RPO 2300 
  260 DO 270 I=1,N                                                       RPO 2310
          K(I) = TEMP(I)                                                   RPO 2320
  270 CONTINUE                                                            RPO 2330 
  280 CONTINUE                                                              RPO 2340 
C RETURN WITH FAILURE IF NO CONVERGENCE WITH 20                          RPO 2350 
C SHIFTS                                                                    RPO 2360 
      FAIL= .TRUE.
      DEGREE=  DEGREE - N                                                  RPO 2380
      RETURN                                                               RPO 2390 
      END                                                                  RPO 2400 


      SUBROUTINE FXSHFR(L2, NZ)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)                                            FXS   10
C COMPUTES UP TO L2 FIXED SHIFT K-POLYNOMIALS,                          FXS   20 
C TESTING FOR CONVERGENCE IN THE LINEAR OR QUADRATIC                      FXS 30 
C CASE. INITIATES ONE OF THE VARIABLE SHIFT                               FXS 40
C ITERATIONS AND RETURNS WITH THE NUMBER OF ZEROS                        FXS 50 
C FOUND.                                                                    FXS   60 
C L2 - LIMIT OF FIXED SHIFT STEPS                                         FXS 70 
C NZ - NUMBER OF ZEROS FOUND                                              FXS 80 
      COMMON /GLOBAL/ P, QP, K, QK, SVK, SR, SI, U,                      FXS   90 
     * V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                       FXS 100
     * H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                       FXS 110 
      DOUBLE PRECISION P(101), QP(101), K(101),                           FXS 120
     * QK(101), SVK(101), SR, SI, U, V, A, B, C, D,                      FXS 130
     * A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                         FXS 140
     * LZR, LZI                                                            FXS 150 
      REAL(KIND=8) ETA, ARE, MRE                                                   FXS 160
      INTEGER N, NN                                                        FXS 170 
      DOUBLE PRECISION SVU, SVV, UI, VI, S                               FXS 180 
      REAL(KIND=8) BETAS, BETAV, OSS, OVV, SS, VV, TS, TV,                       FXS 190
     * O!S. OTV, TVV, TSS                                                 FXS 200 
      INTEGER L2, NZ, TYPEX, I, J, IFLAG                                   FXS 210
      LOGICAL VPASS, SPASS, VTRY, STRY                                    FXS 220 
      NZ =0                                                               FXS 230
      BETAV = .25D0                                                          FXS 240
      BETAS = .25D0                                                          FXS 250
      OSS = SR                                                             FXS 260 
      OVV= V                                                              FXS 270
C EVALUATE POLYNOMIAL BY SYNTHETIC DIVISION
      CALL QUADSD(NN, U, V, P, QP, A, B)                                  FXS 290
      CALL CALCSC(TYPEX)                                                     FXS 300
      DO 80 J=1,L2                                                         FXS 310
C CALCULATE NEXT K POLYNOMIAL AND ESTIMATE V                             FXS 320 
        CALL NEXTK(TYPEX)                                                   E'XS 330
        CALL CALCSC(TYPEX)                                                  FXS 340
        CALL NEWEST(TYPEX, UI, VI)                                         FXS 350
        VV = VI                                                            FXS 360
C ESTIMATES                                                                 FXS 370 
        SS = 0.D0                                                             FXS 380
        IF (K(N).NE.0.D0) SS= -P(NN)/K(N)                                FXS 390
        TV= 1.D0                                                             FXS 400
        TS= 1.D0                                                             FXS 410
        IF (J.EQ.1 .OR. TYPEX.EQ.3) GO TO 70                              FXS 420
C COMPUTE RELATIVE MEASURES OF CONVERGENCE OF SAND V                    FXS 430 
C SEQUENCES                                                                 FXS 440 
        IF (VV.NE.0.D0) TV= ABS((VV-OVV)/VV)                               FXS 450
        IF (SS.NE.0.D0) TS= ABS((SS-OSS)/SS)                                FXS 460
C IF DECREASING, MULTIPLY TWO MOST RECENT                                 FXS 470 
C CONVERGENCE MEASURES                                                     FXS 480 
        TVV = 1.                                                            FXS 490
        IF (TV.LT.OTV) TVV = TV*OTV                                       FXS 500 
        TSS= 1.                                                            FXS 510
        IF (TS.LT.OTS) TSS= TS*OTS                                       FXS 520 
C COMPARE WITH CONVERGENCE CRITERIA                                       FXS 530 
        VPASS = TVV.LT.BETAV                                               FXS 540 
        SPASS= TSS.LT.BETAS                                               FXS 550 
        IF (.NOT.(SPASS .OR. VPASS)) GO TO 70                            FXS 560
CAT LEAST ONE SEQUENCE HAS PASSED THE CONVERGENCE                        FXS 570 
C TEST. STORE VARIABLES BEFORE ITERATING                                  FXS 580 
        SVU = U                                                           FXS 590
        SVV = V                                                            FXS 600 
        DO 10 I=1,N                                                        FXS 610
          SVK (I) = K (I)                                                     FXS 620 
   10   CONTINUE                                                            FXS  630 
        S = SS                                                             FXS 640 
C CHOOSE ITERATION ACCORDING TO THE FASTEST                               FXS 650 
C CONVERGING SEQUENCE                                                      FXS 660 
        VTRY = .FALSE.                                                     FXS 670 
        STRY = .FALSE.                                                     FXS 680 
        IF (SPASS .AND. ((.NOT.VPASS) .OR.                                F¥S 690
     * TSS.LT.TVV)) GO TO 40                                            FXS 700 
   20 CALL QUADIT(UI, VI, NZ)                                            FXS 710
        IF (NZ.GT.0) RETURN                                                FXS 720 
C QUADRATIC ITERATION HAS FAILED. FLAG THAT IT HAS                       FXS 730 
C BEEN TRIED AND DECREASE THE CONVERGENCE CRITERION.                      FXS 740 
        VTRY = .TRUE.                                                      FXS 750 
        BETAV = BETAV*.25D0                                                  FXS 760
C TRY LINEAR ITERATION IF IT HAS NOT BEEN TRIED AND                      FXS 770 
C THES SEQUENCE IS CONVERGING                                             FXS 780 
        IF (STRY .OR. (.NOT.SPASS)) GO TO 50                              FXS 790 
        DO 30 I=1,N                                                       FXS  800
          K (I) = SVK (I)                                                    FXS 810 
   30 CONTINUE                                                           FXS 820 
   40 CALL REALIT(S, NZ, IFLAG)
        IF (NZ.GT.0) RETURN                                               FXS 840
C LINEAR ITERATION HAS FAILED. FLAG THAT IT HAS BEEN                     FXS 850 
C TRIED AND DECREASE THE CONVERGENCE CRITERION                           FXS 860 
        STRY = .TRUE.                                                     FXS 870 
        BETAS= BETAS*.25                                                  FXS 880 
        IF (IFLAG.EQ.0) GO TO 50                                          FXS 890 
C IF LINEAR ITERATION SIGNALS AN ALMOST DOUBLE REAL                      FXS 900 
C ZERO ATTEMPT QUADRATIC INTERATION                                       FXS 910 
        UI= -(S+S)                                                       FXS 920
        VI= S*S                                                          FXS 930 
        GO TO 20                                                           FXS 940 
C RESTORE VARIABLES                                                        FXS 950 
   50   U= SVU                                                           FXS 960
        V= SVV                                                            FXS 970 
        DO 60 I=1,N                                                       FXS 980
          K (I) = SVK (I)                                                    FXS 990
   60 CONTINUE                                                           FXS 1000 
C TRY QUADRATIC ITERATION IF IT HAS NOT BEEN TRIED                       FXS 1010 
C AND THE V SEQUENCE IS CONVERGING                                        FXS 1020 
        IF (VPASS .AND. (.NOT.VTRY)) GO TO 20                            FXS 1030 
C RECOMPUTE QP AND SCALAR VALUES TO CONTINUE THE                         FXS 1040 
C SECOND STAGE                                                             FXS 1050 
        CALL QUADSD(NN, U, V, P, QP, A, B)                                FXS 1060
        CALL CALCSC(TYPEX)                                                  FXS 1070
   70   OVV=  VV                                                          FXS 1080
        OSS = SS                                                          FXS 1090 
        OTV = TV                                                          FXS 1100 
        OTS = TS                                                          FXS 1110 
   80 CONTINUE                                                             FXS 1120
      RETURN                                                               FXS 1130 
      END                                                                  FXS 1140 


      SUBROUTINE QUADIT(UU, VV, NZ)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)                                                QUA   10
C VARIABLE-SHIFT K-POLYNOMIAL ITERATION FOR A                            QUA   20 
C QUADRATIC FACTOR CONVERGES ONLY IF THE ZEROS ARE                       QUA   30 
C EQUIMODULAR OR NEARLY SO.                                               QUA   40 
C UU,VV - COEFFICIENTS OF STARTING QUADRATIC                              QUA   50 
C NZ - NUMBER OF ZERO FOUND                                               QUA   60 
      COMMON /GLOBAL/ P, QP, K, QK, SVK, SR, SI, U,                      QUA   70 
     * V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                       QUA   80
     * H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                       QUA   90 
      DOUBLE PRECISION P(101), QP(101), K(101),                          QUA  100
     * QK ( 101) , SVK ( 101) , SR, SI , U, V, A, B, C, D,                      QUA  110 
     * A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                         QUA  120
     * LZR, LZI                                                            QUA  130 
      REAL(KIND=8) ETA, ARE, MRE                                                  QUA  140
      INTEGER N, NN                                                       OUA  150 
      DOUBLE PRECISION UI, VI, UU, VV                           QUA  160
      REAL(KIND=8) MS, MP, OMP, EE, RELSTP, T, ZM                                QUA  170
      INTEGER NZ, TYPEX, I, J                                              QUA  180
      LOGICAL TRIED                                                        QUA  190 
      NZ= 0                                                               QUA  200 
      TRIED= .FALSE.                                                     QUA  210 
      U= UU                                                              QUA 220
      V= VV                                                               QUA 230 
      J = 0                                                                QUA 240 
C MAIN LOOP                                                                QUA 250 
   10 CALL QUAD(1.0D0, U, V, SZR, SZI, LZR, LZI)                          QUA 260
C RETURN IF ROOTS OF THE QUADRATIC ARE REAL AND NOT                      QUA 270 
C CLOSE TO MULTIPLE OR NEARLY EQUAL AND OF OPPOSITE                     QUA 280 
C SIGN                                                                     QUA 290 
      IF (ABS(ABS(SZR)-ABS(LZR)) .GT.0.01D0*                            QUA  300
     * ABS(LZR)) RETURN                                                  QUA  310
C EVALUA'I'E POLYNOMIAL BY QUADRATIC SYNTHETIC DIVISION                    QUA  320 
      CALL QUADSD(NN, U, V, P, QP, A, B)
      MP= ABS(A-SZR*B) + ABS(SZI*B)
C COMPUTE A RIGOROUS BOUND ON THE ROUNDING ERROR IN                    QUA  350
C EVALUTING P                                                              QUA  360
      ZM = SQRT(ABS(V))                                                  QUA  370
      EE= 2.*ABS(QP(1))                                                  QUA  380
      T = -SZR*B                                                            QUA 390 
      DO 20 I=2,N                                                          QUA 400 
        EE= EE*ZM + ABS(QP(I))                                          QUA 410
   20 CONTINUE                                                              QUA 420 
      EE= EE*ZM + ABS(A+T)                                              QUA 430
      EE= (5.*MRE+4.*ARE)*EE - (5.*MRE+2.*ARE)*                          QUA 440 
     * (ABS(A+T)+ABS(B)*ZM)   +                                          QUA 450
     * 2.*ARE*ABS(T)                                                       QUA 460 
C ITERATION HAS CONVERGED SUFFICIENTLY IF THE                            QUA 470 
C POLYNOMIAL VALUE IS LESS THAN 20 TIMES THIS BOUND                      QUA 480 
      IF (MP.GT.20.*EE) GO TO 30                                          QUA 490 
      NZ= 2                                                               QUA 500
      RETURN                                                               OUA 510 
   30 J = J + 1                                                            QUA 520 
C STOP ITERATION AFTER 20 STEPS                                           QUA 530 
      IF (J.GT.20) RETURN                                                  QUA 540 
      IF (J.LT.2) GO TO 50                                                QUA 550 
      IF (RELSTP.GT .01D0 .OR. MP.LT.OMP .OR. TRIED)                       QUA 560
     *GOTO 50                                                            QUA 570 
CA CLUSTER APPEARS TO BE STALLING THE CONVERGENCE.                       QUA 580 
C FIVE FIXED SHIFT STEPS ARE TAKEN WITH A U,V CLOSE                      QUA 590 
C TO THE CLUSTER                                                           QUA 600 
      IF (RELSTP.LT.ETA) RELSTP = ETA                                     QUA 610 
      RELSTP = SQRT(RELSTP)                                                QUA 620 
      U= U - U*RELSTP                                                     QUA 630 
      V= V+ V*RELSTP                                                     QUA 640 
      CALL QUADSD(NN, U, V, P, QP, A, B)                                  QUA 650 
      DO 40 I=1,5                                                          QUA 660
        CALL CALCSC(TYPEX)                                                  QUA 670
        CALL NEXTK(TYPEX)                                                   QUA 680
   40 CONTINUE                                                              QUA 690 
      TRIED= .TRUE.                                                       QUA 700 
      J = 0                                                                QUA 710 
   50 OMP = MP                                                             QUA 720 
C CALCULATE NEXT K POLYNOMIAL AND NEW U AND V                            QUA 730 
      CALL CALCSC(TYPEX)                                                    QUA 740
      CALL NEXTK(TYPEX)                                                     QUA 750
      CALL CALCSC(TYPEX)                                                    QUA 760
      CALL NEWEST(TYPEX, UI, VI)                                           QUA 770
C IF VI IS ZERO THE ITERATION IS NOT CONVERGING                           QUA 780 
      IF (VI.EQ.0.D0) RETURN                                              QUA 790 
      RELSTP = ABS((VI-V)/VI)                                             QUA 800
      U= UI                                                               QUA 810 
      V= VI                                                               QUA 820 
      GO TO 10                                                             QUA 830 
      END                                                                  QUA 840 


      SUBROUTINE REALIT(SSS, NZ, IFLAG)                                   REA   10
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C VARIABLE-SHIFT H POLYNOMIAL ITERATION FOR A REAL                       REA   20
C ZERO.                                                                     REA 30 
C SSS - STARTING ITERATE                                                 REA 40 
C NZ    - NUMBER OF ZERO FOUND                                            REA 50 
C !FLAG - FLAG TO INDICATE A PAIR OF ZEROS NEAR REAL                     REA 60 
C         AXIS.                                                            REA   70
      COMMON /GLOBAL/ P, QP, K, QK, SVK, SR, SI, U,                      REA   80 
     * V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                       REA   90
     * H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                       REA  100 
      DOUBLE PRECISION P(101), QP(101), K(101),                           REA 110
     * QK(101), SVK(101), SR, SI, U, V, A, B, C, D,                      REA 120
     * A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                         REA 130
     * LZR, LZI                                                            REA 140 
      REAL(KIND=8) ETA, ARE, MRE                                                   REA  150
      INTEGER N, NN                                                        REA  160 
      DOUBLE PRECISION PV, KV, T, S, SSS                                 REA  170
      REAL(KIND=8) MS, MP, OMP, EE                                                REA  180
      INTEGER NZ, IFLAG, I, J, NM1                                       REA  190
      NM1 = N - 1
      NZ= 0                                                               REA  210 
      S = SSS                                                              REA  220 
      IFLAG = 0                                                            REA  230
      J = 0                                                                 REA  240 
C MAIN LOOP                                                                 REA  250
   10 PV = P(1)                                                             REA  260
C EVALUATE PATS
      QP(1) = PV                                                           REA  280
      DO 20 I=2,NN                                                         REA  290
        PV = PV*S + P(I)                                                   REA  300 
        QP(I) = PV                                                         REA 310 
   20 CONTINUE
      MP= ABS(PV)
C COMPUTE A RIGOROUS BOUND ON THE ERROR IN EVALUATING                     REA  340
C P                                                                         REA  350
      MS= ABS(S)                                                          REA  360
      EE= (MRE/(ARE+MRE))*ABS(QP(1))                                      REA  370
      DO 30 I=2,NN                                                          REA  380
        EE= EE*MS + ABS(QP(I))                                            REA  390
   30 CONTINUE                                                              REA  400 
C ITERATION HAS CONVERGED SUFFICIENTLY IF THE                             REA  410 
C POLYNOMIAL VALUE IS LESS THAN 20 TIMES THIS BOUND                      REA  420 
      IF (MP.GT.20.*((ARE+MRE)*EE-MRE*MP))   GO TO 40                      REA  430
      NZ= 1                                                                REA  440 
      SZR = S                                                               REA  450 
      SZI = 0.D0
      RETURN                                                                REA  470
   40 J = J + 1                                                             REA  480 
C STOP ITERATION AFTER 10 STEPS                                            REA 490 
      IF (J.GT.10) RETURN                                                  REA 500 
      IF (J.LT.2) GO TO 50                                                 REA 510 
      IF (ABS(T).GT..001*ABS(S-T) .OR. MP.LE.OMP)                       REA 520
     *GOTO 50                                                             REA 530 
CA CLUSTER OF ZEROS NEAR THE REAL AXIS HAS BEEN                         REA 540 
C ENCOUNTERED RETURN WITH IFLAG SET TO INITIATE A                        REA 550 
C QUADRATIC ITERATION                                                       REA 560 
      IFLAG= 1                                                             REA 570
      SSS= S                                                               REA 580 
      RETURN                                                                REA 590 
C RETURN IF THE POLYNOMIAL VALUE HAS INCREASED                            REA 600 
C SIGNIFICAN'I'LY                                                              REA 610 
   50 OMP = MP                                                              REA 620 
C COMPUTE T, THE NEXT POLYNOMIAL, AND THE NEW ITERATE                    REA 630 
      KV= K(1)                                                               REA 640
      QK(1) = KV                                                           REA 650
      DO 60 I=2,N                                                            REA 660
        KV= KV*S + K(I)                                                     REA 670 
        QK(I) = KV                                                          REA 680 
   60 CONTINUE
      IF (ABS(KV).LE.ABS(K(N))*10.*ETA)   GO TO 80                       REA 700
CUSE THE SCALED FORM OF THE RECURRENCE IF THE VALUE                     REA 710 
C OF KATS IS NONZERO                                                      REA 720 
      T = -PV/KV                                                            REA 730 
      K ( 1) = QP ( 1)                                                          REA 740
      DO 70 I=2,N                                                           REA 750
        K(I) = T*QK(I-1)  + QP(I)                                            REA 760 
   70 CONTINUE                                                              REA 770 
      GO TO 100                                                             REA 780 
CUSE UNSCALED FORM                                                         REA 790 
   80 K(1) = 0.0D0                                                          REA 800
      DO 90 I=2,N                                                           REA 810 \
        K (I) = QK ( I -1)                                                      REA  820 
   90 CONTINUE                                                              REA  830 
  100 KV= K(1)                                                             REA  840
      DO 110 I=2,N                                                          REA  850
        KV= KV*S + K(I)                                                     REA  860 
  110 CONTINUE                                                              REA  870 
      T = 0.0D0                                                             REA  880
      IF (ABS(KV) .GT.ABS(K(N))*10.*ETA) T = -PV/KV                     REA  890
      S = S + T                                                             REA 900 
      GO TO 10                                                              REA 910 
      END                                                                   REA 920 


      SUBROUTINE CALCSC(TYPEX)                                               CAL   10
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C THIS ROUTINE CALCULATES SCALAR QUANTITIES USED TO                      CAL   20
C COMPUTE THE NEXT K POLYNOMIAL AND NEW ESTIMATES OF                     CAL   30 
C THE QUADRATIC COEFFICIENTS.                                               CAL   40
C TYPE - INTEGER VARIABLE SET HERE INDICATING HOW THE                    CAL   50 
C CALCULATIONS ARE NORMALIZED TO AVOID OVERFLOW                           CAL   60 
      COMMON /GLOBAL/ P, QP, K, QK, SVK, SR, SI, U,                      CAL   70 
     * V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                       CAL   80
     * H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                       CAL   90 
      DOUBLE PRECISION P(101), QP(101), K(101),                           CAL  100
     * QK(101), SVK(101), SR, SI , U, V , A, B, C, D,                      CAL  110
     * A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                         CAL  120
     * LZR, LZI                                                            CAL  130 
      REAL(KIND=8) ETA, ARE, MRE                                                   CAL  140
      INTEGER N, NN                                                        CAL  150 
      INTEGER TYPEX                                                         CAL  170
C SYNTHETIC DIVISION OF K BY THE QUADRATIC l,U,V                         CAL  180 
      CALL QUADSD(N, U, V, K, QK, C, D)                                   CAL 190
      IF (ABS(C).GT.ABS(K(N))*100.*ETA) GO TO 10                         CAL 200
      IF (ABS(D).GT.ABS(K(N-1))*100.*ETA)   GO TO 10                     CAL  210
      TYPEX= 3                                                             CAL  220
C TYPE=3 INDICATES THE QUADRATIC IS ALMOST A FACTOR                      CAL  230 
C OF K                                                                      CAL  240 
      RETURN                                                                CAL  250 
   10 IF (ABS(D).LT.ABS(C)) GO TO 20                                    CAL  260
      TYPEX= 2                                                             CAL  270
C TYPE=2 INDICATES THAT ALL FORMULAS ARE DIVIDED BYD                    CAL  280 
      E = A/D                                                              CAL  290
      F = C/D                                                              CAL  300 
      G = U*B                                                              CAL  310 
      H = V*B                                                              CAL  320 
      A3 = (A+G)*E + H*(B/D)                                               CAL  330 
      A1= B*F - A                                                         CAL  340
      A7 = (F+U)*A + H                                                     CAL  350 
      RETURN                                                                CAL  360 
   20 TYPEX= 1                                                             CAL  370
C TYPE=l I~DICATES THAT ALL FORMULAS ARE DIVIDED BY e                    CAL  380
      E = A/C                                                              CAL  390
      F = D/C                                                              CAL  400 
      G = U*E                                                              CAL  410 
      H = V*B                                                              CAL  420 
      A3 = A*E + (H/C+G)*B                                                 CAL  430 
      A1= B - A*(D/C)                                                     CAL  440
      A7 =A+ G*D + H*F                                                   CAL  450 
      RETURN                                                                CAL  460 
      END                                                                   CAL  470 


      SUBROUTINE NEXTK(TYPEX)                                               NEX   10
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C COMPUTES THE NEXT K POLYNOMIALS USING SCALARS                           NEX   20
C COMPUTED IN CALCSC                                                       NEX   30 
      COMMON /GLOBAL/ P, QP, K, QK, SVK, SR, SI, U,                      NEX   40 
     * V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                       NEX   50
     * H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                       NEX 60 
      DOUBLE PRECISION P(101), QP(101), K(101),                           NEX   70
     * QK ( 101) , SVK ( 101) , SR, SI , U, V, A, B, C , D,                      NEX   80 
     * A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                         NEX   90
     * LZR, LZI                                                            NEX  100 
      REAL(KIND=8) ETA, ARE, MRE                                                   NEX  110
      INTEGER N, NN                                                        NEX  120 
      DOUBLE PRECISION TEMP                                                NEX  130
      INTEGER TYPEX                                                         NEX  140
      IF (TYPEX.EQ.3) GO TO 40                                             NEX  150
      TEMP= A                                                             NEX  160 
      IF (TYPEX.EQ.1) TEMP= B                                             NEX  170
      IF (ABS(A1).GT.ABS(TEMP)*ETA*10.) GO TO 20                           NEX  180
C IF A1 IS NEARLY ZERO THEN USE A SPECIAL FORM OF THE                    NEX  190
C RECURRENCE                                                                NEX  200 
      K(1)=0.0D0                                                          NEX  210
      K(2)=-A7*QP(1)                                                     NEX 220
      DO 10 I=3,N                                                          NEX  230
        K(I) = A3*QK(I-2) - A7*QP(I-1)                                     NEX 240
   10 CONTINUE                                                              NEX  250 
      RETURN                                                                NEX  260 
CUSE SCALED FORM OF THE RECURRENCE                                        NEX 270 
   20 A7 = A7/A1                                                           NEX 280
      A3 = A3/A1                                                            NEX  290
      K(1) = QP (1)                                                            NEX 300
      K(2) = QP (2) - A7*QP(1)                                                NEX 310
      DO 30 I=3,N                                                            NEX  320 
        K (I) = A3*QK(I-2)   - A7*QP(I-1) + QP(I)                              NEX 330
   30 CONTINUE                                                                NEX  340 
      RETURN                                                                  NEX  350 
C USE UNSCALED FORM OF THE RECURRENCE IF TYPE IS 3                        NEX  360
   40 K ( 1) = 0.D0                                                            NEX  370 
      K ( 2) = 0.D0                                                            NEX  380 
      DO 50 I=3,N                                                             NEX  390 
        K (I) = QK (I-2)                                                        NEX  400 
   50 CONTINUE                                                                NEX  410
      RETURN                                                                  NEX  420 
      END                                                                     NEX  430 



      SUBROUTINE NEWEST(TYPEX, UU, VV)                                       NEW   10
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C COMPUTE NEW ESTIMATES OF THE QUADRATIC COEFFICIENTS                      NEW   20
C USING THE SCALARS COMPUTED IN CALCSC.                                      NEW   30 
      COMMON /GLOBAL/ P, QP, K, QK, SVK, SR, SI, U,                       NEW   40 
     * V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                        NEW   50
     * H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                        NEW   60 
      DOUBLE PRECISION P(101),    QP(101),  K(101),                              NEW   70
     * QK(101), SVK(101), SR, SI, U, V, A, B, C, D,                       NEW   80
     * A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                          NEW   90
     * LZR, LZI                                                               NEW  100 
      REAL(KIND=8) ETA, ARE, MRE                                                     NEW  110
      INTEGER N, NN                                                          NEW  120 
      DOUBLE PRECISION A4, A5, Bl, B2, Cl, C2, C3,                          NEW  130
     * C4, TEMP, UU, VV                                                      NEW 140
      INTEGER TYPEX                                                           NEW 150
CUSE FORMULAS APPROPRIATE TO SETTING OF TYPE.                             NEW 160 
      IF (TYPEX.EQ.3) GO TO 30                                               NEW 170
      IF (TYPEX.EQ.2)    GO TO 10                                                NEW 180
      A4 =A+ U*B + H*F                                                     NEW 191t 
      A5= C + (U+V*F)*D                                                     NEW 200
      GO TO 20                                                                NEW 210 
   10 A4 = (A+G)*F + H                                                       NEw 220
      A5 = (F+U)*C + V*D                                                     NEW  230
C EVALUATE NEw QUADRATIC COEFFICIENTS.                                      NEW 240 
   20 Bl = -K(N)/P(NN)                                                         NEW 250
      B2 = -(K(N-1)+Bl*P(N))/P(NN)                                            NEW 260
      Cl = V*B2*A1                                                            NEW 270
      C2 = Bl*A7                                                              NEW 280
      C3 = Bl*Bl*A3                                                           NEW 290
      C4 = Cl - C2 - C3                                                      NEW 300
      TEMP= A5+ Bl*A4 - C4                                                 NEW 310
      IF (TEMP.EQ.0.D0) GO TO 30                                            NEW 320
      UU = U - (U*(C3+C2)+V*(Bl*A1+B2*A7))/TEMP                              NEW 330
      VV = V*(1.+C4/TEMP)                                                    NEW 340
      RETURN                                                                  NEw 350 
C IF TYPE=3 THE QUADRATIC IS ZEROED                                         NEW 360 
   30 UU= 0.D0                                                               NEW 370
      VV = 0.D0                                                               NEW 380
      RETURN                                                                  NEW 390 
      END                                                                     NEW 400 


      SUBROUTINE QUADSD(NN, U, V, P, Q, A, B)                              QUA   10 
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C DIVIDES P BY THE QUADRATIC l,U,V PLACING THE                          QUA 20
C QUOTIENT IN Q AND THE REMAINDER IN A,B                                   QUA   30 
      DOUBLE PRECISION P(NN), Q(NN), U, V, A, B, C                        QUA   40 
      INTEGER I                                                               QUA   50
      B = P(1)                                                                  QUA   60
      Q(1) = B                                                                QUA   70
      A= P(2) - U*B                                                         QUA   80 
      Q(2) = A                                                                QUA   90 
      DO 10 I=3,NN                                                            QUA  100
         C = P(I) - U*A - V*B                                                QUA  110 
         Q(I) = C                                                              QUA  120 
         B = A                                                                 QUA  130 
         A = C                                                                 QUA  140 
   10 CONTINUE                                                                QUA  150 
       RETURN                                                                  OUA  160 
       END                                                                     QUA  170 
      SUBROUTINE QUAD(A, B1, C, SR, SI, LR, LI)                          QUA   10
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C CALCULATE THE ZEROS OF THE QUADRATIC A*Z**2+Bl*Z+C.                    QUA   20
C THE QUADRATIC FORMULA, MODIFIED TO AVOID                               QUA   30 
C OVERFLOW, IS USED TO FIND THE LARGER ZERO IF THE                       QUA   40 
C ZEROS ARE REAL AND BOTH ZEROS ARE COMPLEX.                             QUA   50 
C THE SMALLER REAL ZERO IS FOUND DIRECTLY FROM THE                      QUA   60 
C PRODUCT OF THE ZEROS C/A.                                               QUA   70 
      DOUBLE PRECISION A, B1, C, SR, SI, LR, LI, B,                      QUA 80
     * D, E                                                              QUA 90
      IF (A.NE.0.D0) GO TO 20                                            QUA 100
      SR= 0.D0                                                           QUA 110 
      IF (B1.NE.0.D0) SR= -C/B1                                         QUA 120
      LR= 0.D0                                                           QUA 130
   10 SI= 0.D0                                                           QUA 140
      LI= 0.D0                                                           QUA 150 
      RETURN                                                              QUA 160 
   20 IF (C.NE.0.D0) GO TO 30                                            QUA 170
      SR= 0.D0                                                           QUA 180
      LR= -B1/A                                                          QUA 190
      GO TO 10                                                            QUA  200 
C COMPUTE DISCRIMINANT AVOIDING OVERFLOW                                 QUA  210 
   30 B = B1/2.00D0                                                         QUA 220
      IF (ABS(B) .LT.ABS(C)) GO TO 40                                   QUA 230
      E= 1.D0 - (A/B)*(C/B)                                              QUA 240
      D = SQRT(ABS(E))*ABS(B)                                           QUA 250
      GO TO 50                                                            QUA 260 
   40 E= A                                                                QUA  270
      IF (C.LT.0.D0) E= -A                                              QUA  280
      E= B*(B/ABS(C)) - E                                               QUA  290
      D = SQRT(ABS(E))*SQRT(ABS(C))                                     QUA 300
   50 IF (E.LT.0.D0) GO TO 60                                            QUA 310
CREAL ZEROS                                                              QUA  320 
      IF (B.GE.0.D0) D = -D                                             QUA  330
      LR= (-B+D)/A                                                       QUA 340 
      SR= 0.D0                                                           QUA 350
      IF (LR.NE.0.D0) SR = (C/LR)/A                                       QUA 360
      GO TO 10                                                            QUA 370 
C COMPLEX CONJUGATE ZEROS                                                 QUA 380 
   60 SR = -B/A                                                           QUA 390
      LR= SR                                                             QUA 400 
      SI= ABS(D/A)                                                      QUA 410
      LI= -SI                                                            QUA 420 
      RETURN                                                              QUA 430 
      END                                                                 QUA  440 
      SUBROUTINE SAVKY (X,Y,Y1,NN,NGL,M,NAB,IA,IER)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
CCCCCC
C     X=ABSZISSENWERTE
C     Y=ORDINATENWERTE
C     Y1(1)=F-WERT, Y1(2)=1.ABL., USW (GEGLAETTET AN DER STELLE X(IA))
C     NN=ZAHL DER PUNKTEPAARE
C     NGL=GRAD DER GLEICHUNG
C     M:ZAHL DER STUETZSTELLEN =2*M+1
C     NAB  (BERECHNUNG DER 0-TEN BIS NAB-TEN ABLEITUNG, NAB<=NGL)
C     IA=AKTUELLER INDEX DES ZU GLAETTENDEN PUNKTES
CCCCCC
c      PARAMETER (LNGL=NGL,LM=M)
c      PARAMETER (LINGL=LNGL+1,LNGL21=2*LNGL+1,LMM=2*LM+1,LMLL=LMM*LINGL,
c     & LI212=(LINGL*(LINGL+1))/2)
      DIMENSION Y1(*)
      DIMENSION X(*),Y(*)
C      DIMENSION  SV(LNGL21),Z(LMM),SM(LINGL,LINGL),FM(LINGL,LMM),
C     & FAKN(LINGL),AUX(LNGL)
C      DIMENSION Y1(*)
C      DIMENSION SM1(LI212),FM1(LMLL)
C      DIMENSION SMSCA(LINGL),FMSCA(LMM)
      DIMENSION  SV(2*NGL+1),Z(2*M+1),SM(NGL+1,NGL+1),FM(NGL+1,2*M+1),
     & FAKN(NGL+1),AUX(NGL)
      DIMENSION SM1(((NGL+1)*(NGL+2))/2),FM1((2*M+1)*(NGL+1))
      DIMENSION SMSCA(NGL+1),FMSCA(2*M+1)
      FAKN(1)=1.0
      FAKN(2)=1.0
      DO   I=3,NGL+1
        FAKN(I)=FAKN(I-1)*(I-1)
      ENDDO
      NABB=NAB+1
      INGL=NGL+1
      ISV=NGL*2+1
      MM=M*2+1
      IU=IA-M-1
      IF(IU.LT.0.OR.(IU+MM).GT.NN) GOTO 99
C     BERECHNUNG VON S(R+K)
      DO  J=1,M
        Z(J)=X(IA)-X(IU+J)
      ENDDO
      Z(M+1)=0.0
      DO  J=M+2,MM
        Z(J)=X(IU+J)-X(IA)
      ENDDO
      SV(1)=1.0*MM
      DO  I=2,ISV
       SV(I)=0.
       DO  J=1,M
        SV(I)=SV(I)+(-Z(J))**(I-1)
       ENDDO
       DO J=M+1,MM
        SV(I)=SV(I)+Z(J)**(I-1)
       ENDDO
      ENDDO
C     BERECHNUNG DER MATRIX SM
C
      DO   I=1,INGL
        DO   J=1,INGL
         SM(I,J) =SV(J+I-1)
        ENDDO
      ENDDO
C
C     BERECHNUNG DER MATRIX FM
C
      DO  I=2,INGL
       DO   J=1,M
        FM(I,J)=(-Z(J))**(I-1)
       ENDDO
       DO  J=M+1,MM
        FM(I,J)=Z(J)**(I-1)
       ENDDO
      ENDDO
      DO  J=1,MM
       FM(1,J) =1.0
      ENDDo
C
C     SCALING
C     MULTIPLIKATION D * SM * D
      K = 0
      DO  I=1,INGL
       DO  J=1,I
        K = K+1
        SM1(K) = SV(I+J-1)
       ENDDO
      ENDDO
      K = 0
      DO J=1,MM
       DO  I=1,INGL
        K = K+1
        FM1(K) = FM(I,J)
       ENDDO
      ENDDO
      EPS = 1.0D-7
      CALL GELS(FM1,SM1,INGL,MM,EPS,IER,AUX)
      IF(IER.NE.0) RETURN
C
c      DO I=1,NABB
c        Y1(I)=0.0
c      END DO

      DO I=1,NABB
        SUU=0.0
c         Y1(I)=0.0
       DO J=1,MM
        I1=(J-1)*INGL+I
        J1=IU+J
        FMIJ = FM1(I1)*FAKN(I)
c        Y1(I) = Y1(I)+Y(J1)*FMIJ
         SUU=SUU+Y(J1)*FMIJ
       ENDDO
         Y1(I)=SUU
      ENDDO
      RETURN
   99 IER=-11
      RETURN
      END SUBROUTINE

      SUBROUTINE GELS(R,A,M,N,EPS,IER,AUX)                              GELS
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C                                                                       GELS  10
C     ..................................................................GELS  20
C                                                                       GELS  30
C        SUBROUTINE GELS                                                GELS  40
C                                                                       GELS  50
C        PURPOSE                                                        GELS  60
C           TO SOLVE A SYSTEM OF SIMULTANEOUS LINEAR EQUATIONS WITH     GELS  70
C           SYMMETRIC COEFFICIENT MATRIX UPPER TRIANGULAR PART OF WHICH GELS  80
C           IS ASSUMED TO BE STORED COLUMNWISE.                         GELS  90
C                                                                       GELS 100
C        USAGE                                                          GELS 110
C           CALL GELS(R,A,M,N,EPS,IER,AUX)                              GELS 120
C                                                                       GELS 130
C        DESCRIPTION OF PARAMETERS                                      GELS 140
C           R      - M BY N RIGHT HAND SIDE MATRIX.  (DESTROYED)        GELS 150
C                    ON RETURN R CONTAINS THE SOLUTION OF THE EQUATIONS.GELS 160
C           A      - UPPER TRIANGULAR PART OF THE SYMMETRIC             GELS 170
C                    M BY M COEFFICIENT MATRIX.  (DESTROYED)            GELS 180
C           M      - THE NUMBER OF EQUATIONS IN THE SYSTEM.             GELS 190
C           N      - THE NUMBER OF RIGHT HAND SIDE VECTORS.             GELS 200
C           EPS    - AN INPUT CONSTANT WHICH IS USED AS RELATIVE        GELS 210
C                    TOLERANCE FOR TEST ON LOSS OF SIGNIFICANCE.        GELS 220
C           IER    - RESULTING ERROR PARAMETER CODED AS FOLLOWS         GELS 230
C                    IER=0  - NO ERROR,                                 GELS 240
C                    IER=-1 - NO RESULT BECAUSE OF M LESS THAN 1 OR     GELS 250
C                             PIVOT ELEMENT AT ANY ELIMINATION STEP     GELS 260
C                             EQUAL TO 0,                               GELS 270
C                    IER=K  - WARNING DUE TO POSSIBLE LOSS OF SIGNIFI-  GELS 280
C                             CANCE INDICATED AT ELIMINATION STEP K+1,  GELS 290
C                             WHERE PIVOT ELEMENT WAS LESS THAN OR      GELS 300
C                             EQUAL TO THE INTERNAL TOLERANCE EPS TIMES GELS 310
C                             ABSOLUTELY GREATEST MAIN DIAGONAL         GELS 320
C                             ELEMENT OF MATRIX A.                      GELS 330
C           AUX    - AN AUXILIARY STORAGE ARRAY WITH DIMENSION M-1.     GELS 340
C                                                                       GELS 350
C        REMARKS                                                        GELS 360
C           UPPER TRIANGULAR PART OF MATRIX A IS ASSUMED TO BE STORED   GELS 370
C           COLUMNWISE IN M*(M+1)/2 SUCCESSIVE STORAGE LOCATIONS, RIGHT GELS 380
C           HAND SIDE MATRIX R COLUMNWISE IN N*M SUCCESSIVE STORAGE     GELS 390
C           LOCATIONS. ON RETURN SOLUTION MATRIX R IS STORED COLUMNWISE GELS 400
C           TOO.                                                        GELS 410
C           THE PROCEDURE GIVES RESULTS IF THE NUMBER OF EQUATIONS M IS GELS 420
C           GREATER THAN 0 AND PIVOT ELEMENTS AT ALL ELIMINATION STEPS  GELS 430
C           ARE DIFFERENT FROM 0. HOWEVER WARNING IER=K - IF GIVEN -    GELS 440
C           INDICATES POSSIBLE LOSS OF SIGNIFICANCE. IN CASE OF A WELL  GELS 450
C           SCALED MATRIX A AND APPROPRIATE TOLERANCE EPS, IER=K MAY BE GELS 460
C           INTERPRETED THAT MATRIX A HAS THE RANK K. NO WARNING IS     GELS 470
C           GIVEN IN CASE M=1.                                          GELS 480
C           ERROR PARAMETER IER=-1 DOES NOT NECESSARILY MEAN THAT       GELS 490
C           MATRIX A IS SINGULAR, AS ONLY MAIN DIAGONAL ELEMENTS        GELS 500
C           ARE USED AS PIVOT ELEMENTS. POSSIBLY SUBROUTINE GELG (WHICH GELS 510
C           WORKS WITH TOTAL PIVOTING) WOULD BE ABLE TO FIND A SOLUTION.GELS 520
C                                                                       GELS 530
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  GELS 540
C           NONE                                                        GELS 550
C                                                                       GELS 560
C        METHOD                                                         GELS 570
C           SOLUTION IS DONE BY MEANS OF GAUSS-ELIMINATION WITH         GELS 580
C           PIVOTING IN MAIN DIAGONAL, IN ORDER TO PRESERVE             GELS 590
C           SYMMETRY IN REMAINING COEFFICIENT MATRICES.                 GELS 600
C                                                                       GELS 610
C     ..................................................................GELS 620
C                                                                       GELS 630
C                                                                       GELS 650
C                                                                       GELS 660
      DOUBLE PRECISION PIV,TB,TOL,PIVI
      DIMENSION  A(*),R(*),AUX(*)                                       GELS 670
      IF(M.LE.0)GOTO 24                                                      GELS 680
C                                                                       GELS 690
C     SEARCH FOR GREATEST MAIN DIAGONAL ELEMENT                         GELS 700
      IER=0                                                             GELS 710
      PIV=0.                                                            GELS 720
      L=0
      I=0
      J=0                                                               GELS 730
      DO  K=1,M                                                         GELS 740
       L=L+K                                                             GELS 750
       TB =ABS(A(L))                                                      GELS 760
c      IF((TB-PIV)3,3,2
       IF((TB-PIV).LE.0.0) CYCLE
       PIV=TB                                                            GELS 780
       I=L                                                               GELS 790
       J=K                                                               GELS 800
      ENDDO                                                             GELS 810
      TOL=EPS*PIV                                                       GELS 820
C     MAIN DIAGONAL ELEMENT A(I)=A(J,J) IS FIRST PIVOT ELEMENT.         GELS 830
C     PIV CONTAINS THE ABSOLUTE VALUE OF A(I).                          GELS 840
C                                                                       GELS 850
C                                                                       GELS 860
C     START ELIMINATION LOOP                                            GELS 870
      LST=0                                                             GELS 880
      NM=N*M                                                            GELS 890
      LEND=M-1                                                          GELS 900
      DO  K=1,M                                                         GELS 910
C                                                                       GELS 920
C        TEST ON USEFULNESS OF SYMMETRIC ALGORITHM                         GELS 930
c        IF(PIV)24,24,4                                                    GELS 940
c    4   IF(IER)7,5,7                                                      GELS 950
c    5   IF(PIV-TOL)6,6,7                                                  GELS 960
c    6   IER=K-1                                                           GELS 970
c    7   LT=J-K
         IF(PIV.LE.0.0) GOTO 24                                                    GELS 940
         IF(IER.NE.0) GOTO 7                                                      GELS 950
         IF((PIV-TOL).GT.0.0) GOTO 7                                                  GELS 960
         IER=K-1                                                           GELS 970
   7     LT=J-K                                                               GELS 980
         LST=LST+K                                                         GELS 990
C                                                                       GELS1000
C        PIVOT ROW REDUCTION AND ROW INTERCHANGE IN RIGHT HAND SIDE R      GELS1010
         PIVI=1./A(I)                                                      GELS1020
         DO   L=K,NM,M                                                     GELS1030
          LL=L+LT                                                           GELS1040
          TB=PIVI*R(LL)                                                     GELS1050
          R(LL)=R(L)                                                        GELS1060
          R(L)=TB
         ENDDO                                                          GELS1070
C                                                                       GELS1080
C        IS ELIMINATION TERMINATED                                         GELS1090
         IF((K-M).GE.0) GOTO  19                                                    GELS1100
C                                                                       GELS1110
C        ROW AND COLUMN INTERCHANGE AND PIVOT ROW REDUCTION IN MATRIX A.   GELS1120
C        ELEMENTS OF PIVOT COLUMN ARE SAVED IN AUXILIARY VECTOR AUX.       GELS1130
    9    LR=LST+(LT*(K+J-1))/2                                             GELS1140
         LL=LR                                                             GELS1150
         L=LST                                                             GELS1160
         DO  II=K,LEND                                                   GELS1170
           L=L+II                                                            GELS1180
           LL=LL+1                                                           GELS1190
c           IF(L-LR)12,10,11
           IF((L-LR).EQ.0) THEN
             A(LL)=A(LST)                                                      GELS1210
             TB=A(L)                                                           GELS1220
             GO TO 13
           ELSEIF((L-LR).GT.0) THEN
             LL=L+LT
           ELSE                                                          GELS1240
             TB=A(LL)
           ENDIF                                                          GELS1250
           A(LL)=A(L)                                                        GELS1260
   13      AUX(II)=TB                                                        GELS1270
           A(L)=PIVI*TB
         ENDDO
C                                                                       GELS1290
C        SAVE COLUMN INTERCHANGE INFORMATION                               GELS1300
         A(LST)=LT                                                         GELS1310
C                                                                       GELS1320
C        ELEMENT REDUCTION AND SEARCH FOR NEXT PIVOT                       GELS1330
         PIV=0.                                                            GELS1340
         LLST=LST                                                          GELS1350
         LT=0                                                              GELS1360
         DO  II=K,LEND                                                     GELS1370
           PIVI=-AUX(II)                                                     GELS1380
           LL=LLST                                                           GELS1390
           LT=LT+1                                                           GELS1400
           DO  LLD=II,LEND                                                 GELS1410
             LL=LL+LLD                                                         GELS1420
             L=LL+LT                                                           GELS1430
             A(L)=A(L)+PIVI*A(LL)
           ENDDO                                                             GELS1440
         LLST=LLST+II                                                      GELS1450
         LR=LLST+LT                                                        GELS1460
         TB=ABS(A(LR))                                                     GELS1470
         IF((TB-PIV).LE.0.0) GOTO 17                                                GELS1480
         PIV=TB                                                            GELS1490
         I=LR                                                              GELS1500
         J=II+1                                                            GELS1510
   17    DO   LR=K,NM,M                                                   GELS1520
          LL=LR+LT                                                          GELS1530
          R(LL)=R(LL)+PIVI*R(LR)
         ENDDO
       ENDDO
      ENDDO                                                              GELS1540
C     END OF ELIMINATION LOOP                                           GELS1550
C                                                                       GELS1560
C                                                                       GELS1570
C     BACK SUBSTITUTION AND BACK INTERCHANGE                            GELS1580
c   19 IF(LEND)24,23,20
   19  IF(LEND.GT.0) THEN                                               GELS1590
        II=M                                                              GELS1600
        DO   I=2,M                                                        GELS1610
         LST=LST-II                                                        GELS1620
         II=II-1                                                           GELS1630
         L=A(LST)+.5                                                       GELS1640
         DO   J=II,NM,M                                                   GELS1650
          TB=R(J)                                                           GELS1660
          LL=J                                                              GELS1670
          K=LST                                                             GELS1680
          DO  LT=II,LEND                                                  GELS1690
            LL=LL+1                                                           GELS1700
            K=K+LT                                                            GELS1710
            TB=TB-A(K)*R(LL)
          ENDDO                                                            GELS1720
          K=J+L                                                             GELS1730
          R(J)=R(K)                                                         GELS1740
          R(K)=TB
         ENDDO
        ENDDO
       ELSEIF(LEND.LT.0) THEN
         GOTO 24
       ENDIF
   23 RETURN                                                            GELS1760
C                                                                       GELS1770
C                                                                       GELS1780
C     ERROR RETURN                                                      GELS1790
   24 IER=-1                                                            GELS1800
      RETURN                                                            GELS1810
      END SUBROUTINE                                                              GELS1820



