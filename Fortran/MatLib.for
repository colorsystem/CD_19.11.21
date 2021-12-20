C     Last change: KU 21.08.2019 12:19:02
c
C
C    siehe auch NLSIMPLX.FOR
c
C
C
      SUBROUTINE NLDFIT(MF,M,ME,N,MAXIT,MAXFUN,RESSIZ,XLB,XUB,
     &           X,FVALUE,IFAIL,FUNEXT,MATEXT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      INTEGER(KIND=4) :: MF,M,ME,N,IPRINT,MAXIT,MAXFUN,MAXFUU
      INTEGER(KIND=4) :: LACTIV,LWA,IFAIL,IOUT,MAXI,MAXF
      REAL(KIND=8) :: RES,RESSIZ,RSIZE
      DIMENSION XLB(N),XUB(N),X(N)
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: XB
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: XL
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: XU
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: G
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: DG
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: U
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: WA
      INTEGER,ALLOCATABLE,DIMENSION(:) :: KWA
      LOGICAL,ALLOCATABLE,DIMENSION(:) :: ACTIVE
      EXTERNAL FUNEXT,MATEXT
      DATA EPS/1.0D-7/
      DATA ACC/1.0D-13/
      DATA ACCQP/1.0D-12/
      DATA RHO/1.0D3/
      DATA MAXNM/30/
      DATA EPMAX/1.0D-12/

C
C
      LMMAX=M+MF+1
      LNMAX=MAX0(MF+N+1,2)
      LMNN2=M+2*N+3*MF+2
      LN=N+MF
      LWA=LNMAX*LNMAX+25*LNMAX+7*LMMAX+150
     &   +MAX0(5*LNMAX*LNMAX/2+17*LNMAX+M*LNMAX+3*M+20,(N+1)*MF)
      LKWA=LNMAX+30
      LACTIV=2*LMMAX+10

C
C
      RSIZE=MAX(RESSIZ,1.0D-10)

C
      IER=0
      RES=0.0

C
C
C
C
      ALLOCATE (XB(LNMAX),XL(LNMAX),XU(LNMAX),G(LMMAX),
     &          DG(LMMAX,LNMAX),U(LMNN2),ACTIVE(LACTIV),
     &          WA(LWA),KWA(LKWA),STAT=IER)
      IF(IER.NE.0) THEN
        GOTO 900
      ENDIF
C     
C
c     OPEN(25,FILE='TESTREZ.TXT')
C
C     
      DO I=1,N+MF
         XB(I)=0.0
      END DO
      DO I=1,N
        XB(I)=X(I)
        XL(I)=XLB(I)
        XU(I)=XUB(I)
      END DO
C
C      XB außerhalb der Grenzen XL,XU
C
C
      DO I=1,N
       IF(XL(I).GT.XB(I)) THEN
          XB(I)= XL(I)
       ENDIF
       IF(XU(I).LT.XB(I)) THEN
         XB(I)=XU(I)
       ENDIF

      END DO
C
C
C
      IOUT=0
      IPRINT=0
C      IOUT=27
C      IPRINT=4
c      WRITE(IOUT,*) 'MF,M,ME,N',MF,M,ME,N
C
C********
C
C
C
C
C     BESCHREIBUNG DER LISTENPARAMETER
C
C     N    ANZAHL X-WERTE
C     X    X-WERTE (D.H. ZU BESTIMMENDE GROESSEN; BEIM AUFRUF STEHEN
C          IN X GEEIGNETE STARTWERTE)
C     MF   ANZAHL MINIMIERUNGSBEDINGUNGEN
C     M    ANZAHL GLEICHUNGEN + UNGLEICHUNGEN
C     ME   DAVON ANZAHL GLEICHUNGEN
C     G    1...MF DIE ZU MINIMIERENDEN FUNKTIONEN FÜR X (bzw. XB)
C     G    MF+1...MF+M  ERGEBNIS DER GLEICHUNGEN + UNGLEICHUNGEN FÜR X (bzw. XB)
C     XB,XL,XU sind größer dimensioniert und entsprechen (von 1..N) den X,XLB,XUB-Werten
C     IER  FEHLERCODE
C
C     Bei nichtlinearen Gleichungen empfiehlt es sich, zunächst mit
C     MFneu=MF+ME (Gleichungen werden als Minimierungsbedingungen verwendet)
C     MEneu=0
C     Mneu=M-ME
C     Mit den so erhaltenen X-Werten normal (d.h. mit MF,M,ME) weiterarbeiten
C
C     ITERATION
C
      MAXFUU=MIN(50,MAXFUN)
      IFAIL=0
  10  CONTINUE
      IF((IFAIL.EQ.0).OR.(IFAIL.EQ.-1)) THEN
          CALL FUNEXT(N,XB,XL,XU,MF,M,G,IER)
C          WRITE(IOUT,*)'G',(G(I),I=1,M+MF)
          IF(IER.GT.0) THEN
            IFAIL=IER
            GOTO 900
          ENDIF
      ENDIF
C
C
C
C     Ableitungen
C
C
C
      IF((IFAIL.EQ.0).OR.(IFAIL.EQ.-2)) THEN
          CALL MATEXT(N,XB,XL,XU,LMMAX,MF,M,G,DG,IER)
          IF(IER.GT.0) THEN
            IFAIL=IER
            GOTO 900
          ENDIF
C          DO I=1,M+MF
C             WRITE(IOUT,*) 'DG',(DG(I,J),J=1,N)
C          END DO
          DGMAX=0.0
          DO I=1,MF
             DO J=1,N
               DGG=ABS(DG(I,J))
               IF(DGMAX.LT.DGG) THEN
                  DGMAX=DGG
               ENDIF
             END DO
          END DO
          IF(DGMAX.LT.EPMAX) THEN
             IFAIL=4173
             GOTO 900
          ENDIF
      ENDIF
C
C
C     Optimierung
C
C
C
C
      CALL NLPLSQ (      M,     ME,  LMMAX,     MF,      N,
     /               LNMAX,  LMNN2,     XB,      G,    RES,
     /                  DG,      U,     XL,     XU,    ACC,
     /               ACCQP,  RSIZE, MAXFUU,  MAXIT,  MAXNM,
     /                 RHO, IPRINT,   IOUT,  IFAIL,     WA,
     /                 LWA,    KWA,   LKWA,  ACTIVE,  LACTIV)
c      IF(IFAIL.EQ.10) THEN
C
      IF(IFAIL.LT.0) GOTO 10
C
C
C
C
      DO I=1,N
        X(I)=XB(I)
      ENDDO
C      WRITE(IOUT,*) 'RES,IFAIL',RES,IFAIL
      CALL FUNEXT(N,X,XL,XU,MF,M,G,IER)
C
C
C     Fehlerquadratsumme
C
C
      FVALUE=0.0
      DO I=1,MF
        FVALUE=FVALUE+G(I)**2
      END DO
C      WRITE(IOUT,*) 'FVALUE,RES',FVALUE,RES
C
C
C     Überprüfung der Grenzen
C
C
      IER=0
      DO I=MF+1,MF+ME
        IF(ABS(G(I)).GT.EPS) THEN
          IER=-1
          EXIT
        ENDIF
      END DO
      DO I=MF+ME+1,MF+M
         IF(G(I).LT.-EPS) THEN
           IER=-1
           EXIT
         ENDIF
      END DO
      IF(IER.EQ.-1) THEN
         IFAIL=-4174
      ELSE
         IFAIL=0
      ENDIF
      FVALUE=RES
 900  DEALLOCATE (XB,XL,XU,G,DG,U,ACTIVE,WA,KWA)
C
      RETURN
      END
C
C
C******************************************************************************
C
C
C
      SUBROUTINE NCONS(M,ME,N,XGUESS,XLB,XUB,
     &           IPRINT,MAXIT,X,FVALUE,NLFUNC,NLGRAD,IFAIL,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      EXTERNAL NLFUNC,NLGRAD,QL
      INTEGER IOP
      LOGICAL LQL
      INTEGER M,ME,N,IPRINT,MAXIT,LDC,MAXFUN,MODE
      INTEGER MMAX,M1,MU,LIWK,LACTIV,LWA,IFAIL,IOUT
      DIMENSION XGUESS(*),XLB(*),XUB(*),X(*),F(1,1)
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:):: G
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:):: DF
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:):: DG
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:):: U
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:):: C
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:):: D
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:):: WA
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:):: XX

C
C
C
c      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:):: CL,CU,DX
c      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: IC
      INTEGER,ALLOCATABLE,DIMENSION(:):: KWA
      LOGICAL,ALLOCATABLE,DIMENSION(:)::  ACTIVE
      DATA EPS/1.0D-7/
      DATA ACC/1.0D-14/
      DATA ACCQP/1.0D-15/
      DATA STPMIN/1.0D-10/
      DATA RHO/1.0D3/
      DATA MAXFUN/50/
      DATA MAXNM/30/
      DATA MODE/0/
      DATA LQL/.TRUE./
C
C     M   Anzahl Nebenbedingungen (Gleichungen + Ungleichungen)
C     ME  Anzahl Gleichungen (1..... ME)
C     N   Anzahl Unbekannte
C
      MMAX=MAX0(M,1)
      NMAX=MAX0(N+1,2)
      LMAX=1
      L=1
      MNN2=M+N+N+2
      MNN2X=MMAX+NMAX+NMAX+2

      LWA=23*N+4*M+3*MMAX+LMAX*(N+M+1)+150
     &   +3*NMAX*NMAX/2+10*NMAX+MMAX+M+1

      LKWA=NMAX+25
      LACTIV=2*MMAX+10

C
C
C
C

C
      IER=0
C

C
C
C
C
      ALLOCATE (XX(NMAX),G(MMAX),DF(NMAX),DG(MMAX,NMAX),U(MNN2X),
     &          C(NMAX,NMAX),D(NMAX),ACTIVE(LACTIV),WA(LWA),KWA(LKWA),
     &          STAT=IER)
      IF(IER.NE.0) THEN
        RETURN
      ENDIF
C
C
c      IPRINT=4
      IOUT=0
C
C      XX außerhalb der Grenzen XLB,XUB
C
C
        DO I=1,N
         XX(I)=XGUESS(I)
         IF(XLB(I).GT.XX(I)) THEN
            XX(I)=XLB(I)
         ENDIF
         IF(XUB(I).LT.XX(I)) THEN
            XX(I)=XUB(I)
         ENDIF
        END DO
C
C
C
C

C
C

      IER=0
C
C
C      Mainblock
C
C

C
C
C
C
      IFAIL=0

  10  CONTINUE
      IF((IFAIL.EQ.0).OR.(IFAIL.EQ.-1)) THEN

        CALL NLFUNC(M,ME,N,F0,G,XX)
        F(1,1)=F0
      ENDIF
C
C
C
C     Ableitungen
C
C
C
      IF((IFAIL.EQ.0).OR.(IFAIL.EQ.-2)) THEN
        CALL NLGRAD(M,ME,MMAX,N,F0,G,DF,DG,XX)
      ENDIF
C
C
C     Optimierung
C
C
      CALL NLPQLP (     L,      M,     ME,   MMAX,      N,
     /                     NMAX,   MNN2,      XX,      F,      G,
     /                       DF,     DG,      U,    XLB,    XUB,
     /                        C,      D,    ACC,  ACCQP, STPMIN,
     /                   MAXFUN,  MAXIT,  MAXNM,    RHO, IPRINT,
     /                     MODE,   IOUT,  IFAIL,     WA,    LWA,
     /                      KWA,   LKWA, ACTIVE, LACTIV,    LQL,
     /                   QL)
C
C
      IF(IFAIL.LT.0) GOTO 10
C
      FVALUE=F(1,1)
      DO I=1,N
        X(I)=XX(I)
      END DO
      DEALLOCATE (XX,G,DF,DG,U,C,D,ACTIVE,WA,KWA)
C
      RETURN
      END
C
C
C
      SUBROUTINE NLDFIU(MF,M,ME,N,MAXIT,MAXFUN,RESSIZ,XLB,XUB,
     &           X,FVALUE,IFAIL,FUNEXT,MATEXT)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C
C 
C 
C 
      REAL(KIND=8),DIMENSION(*) ::  X,XLB,XUB
      EXTERNAL FUNEXT,MATEXT
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: A
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: AF
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) ::  F,G
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) ::  RW
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: GRA
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: BL,BU
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: XP
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: XB
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) ::IW
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: IND
      INTEGER(KIND=4),DIMENSION(27) :: IOPT
      REAL(KIND=8) :: TOL,RANGES,PRECIS
      LOGICAL(KIND=4) :: REZOK
C
C
C
C     

      DATA RANGES/1.D80/,PRECIS/5.D-7/,TOL/1.D-5/
      DATA IOPT/3,2,8,99,23*0/
      DATA M0/0/
C 
C
      IFAIL=0
      IER=0
      ISTAT=0
      REZOK=.FALSE.
C 
      MO=MAX(MF,N)+M+N
      N1=N+1
      MM=MAX(MF,N)+M
      MV=MAX(MF,N)+N
      MN=M+N
      ALLOCATE(A(MO,MN+3),AF(MO,N+1),F(MM),G(MM),RW(6*N+5*M),GRA(N),
     &  BL(MN),BU(MN),XP(2*MN+2*N),XB(N),IW(2*MN),IND(MN),STAT=ISTAT)
      ISTAT=IERALC(ISTAT)
      IF(ISTAT.NE.0) THEN
         IFAIL=4000
         RETURN
      ENDIF
C
C

C
C
C
      RANGIN=1./RANGES
      RANGT=SQRT(RANGES)
      RANGS=1./RANGT
      RANGT=SQRT(RANGT)
      PTOL=RESSIZ
      XTOL=PTOL
      P2T=PTOL**2+RANGIN
      X2T=XTOL**2 
      P2R=0.001*P2T
      IER=0 
      IT=0
      FSA=RANGES
      N1=N+1
      P1=0.01
      P=0.
      X6=0.0
      DOWN=0.01 
      UP=1000.
      CHANGE=0.5
      ITM=MAXIT
C    
C
      DO I=1,N
      XP(I)=0.0
       IF(XLB(I).GT.X(I)) THEN
          X(I)= XLB(I)
       ENDIF
       IF(XUB(I).LT.X(I)) THEN
         X(I)=XUB(I)
       ENDIF
      END DO
C
C
C
      IF(M.GT.0) THEN
        CALL FUNEXT(N,X,XLB,XUB,MF,M,F,IER)
        CALL MATEXT(N,X,XLB,XUB,MO,MF,M,F,AF,IER)
        DO I=1,M
          AF(MF+I,N+1)=-F(MF+I)
        END DO
C
C       X-Werte an Grenzen anpassen
C
         DO I=1,N
          BL(I)=XLB(I)-X(I)
          BU(I)=XUB(I)-X(I)
          IND(I)=3
         END DO
         CALL SBOCLS(AF(MF+1,1),MO,M0,M,N,BL,BU,IND,IOPT,XP,RNOC,RNRM,
     &            IER,RW,IW)
      ENDIF
C
C
      DO  I=1,N
         X(I)=X(I)+XP(I)
         XP(I)=X(I)
         XB(I)=X(I)
      ENDDO
C 
C 
C 
C 
C     FEHLERQUADRATSUMME FUER STARTWERTE
C 
C 
C 

C 
 50   CALL FQUSU(IER,N,X,XLB,XUB,MF,M,F,FS,FUNEXT)
      IF(IER.GT.1) GOTO 990
C 
C 
C 
C 
C 
      FSM=FS
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
C
C
C     MINIMIERUNGSGLEICHUNGEN + Grenzen für DELTA-X
C
C

      CALL MATEXT(N,X,XLB,XUB,MO,MF,M,F,AF,IER)
C
C
C
C      GRENZEN
C
C
       DO I=1,N
          BL(I)=XLB(I)-X(I)
          BU(I)=XUB(I)-X(I)
          IND(I)=3
       END DO
       DO I=1,M
          BL(N+I)=-F(MF+I)
          IF(I.LE.ME) THEN
            BU(N+I)=BL(N+I)
            IND(N+I)=3
          ELSE
            BU(N+I)=RANGES
            IND(N+I)=1
          ENDIF
       END DO
C
C
C     Gleichungen und Ungleichungen von den hinteren in die vorderen Zeilen umspeichern
C
C
      CALL UMSTO(MO,MF,M,N,A,AF,F)
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
          GLR=0.
          GL=0.
          DO  J=1,N
             GRA(J)=SNRM2(MF,A(M+1,J),1)
C
C
C
C
C 
C             GRADIENTENLAENGE
  
C 
             SUU= SDOT(MF,A(M+1,J),1,A(M+1,N1),1)
             RW(J)=SUU
             GLR=GLR+GRA(J)
          ENDDO
          GL=SNRM2(N,RW,1)
          IF(ABS(GL).GT.RANGT) GL=SIGN(RANGT,GL)
          SM=-2.*GL*GL  
      IG=1
      P1L=P1
      IF(P1L.LT.PRECIS) P1L=PRECIS
      FSA=FS
      FS=FSM
      FS1=RANGES
      FS2=RANGES
C 
C 
C 
C 
C 
C 
C 
  100 GOTO(110,120,130,140,142,200),IG
  110 P=P1L 
      GOTO 150
  120 IF(FS.LE.FS1) THEN
            P=P1L*UP
      ELSE
            P=P1L*DOWN
      ENDIF 
      GOTO 150
  130 P=P1*CHANGE 
      GOTO 150
  140 X3=1./(P1+RANGS)
      X4=1./(P2+RANGS)
      DET=(X4-X3)*(X3*X4)**2
      AP1=(FS2-FS-SM*X4)*X3**2
      AP2=(FS1-FS-SM*X3)*X4**2
      AI=6.*(AP1-AP2)/(DET+RANGS) 
      BI=2.*(X4*AP2-X3*AP1)/(DET+RANGS) 
      IF(ABS(AI).LT.RANGS.OR.ABS(AI).GT.RANGT) GOTO 145
      IF(ABS(BI).LT.RANGT.OR.ABS(BI).GT.RANGT) GOTO 145
      DET=BI*BI-2.*AI*SM
      IF(DET.LT.0.) GOTO 145
      DET=SQRT(ABS(DET))
      X5=(-BI+DET)/AI 
      X6=(-BI-DET)/AI 
      XX=X5 
      IF(XX.LT.0.) THEN 
             XX=X6
             IG=IG+1
      ENDIF 
      IF(ABS(XX).LE.RANGS) GOTO 145 
      P=1./XX 
      GOTO 150
  142 XX=X6 
      IF(XX.LE.-RANGS) GOTO 145 
      P=1./(XX+RANGS)
      GOTO 150
  145 IF(P.EQ.100.) THEN
         P=10000.
      ELSE
         P=100.
      ENDIF
C 
C 
C 
C 
  150 IG=IG+1 
      IF(P.GT.10000.) P=10000.
C    
C
C     UMSPEICHERN
C
          CALL UMSTO(MO,MF,M,N,A,AF,F)
C
C
C
          DO   I=1,N
            DO   J=1,N1
              A(MM+I,J)=0.
              IF(I.EQ.J) A(MM+I,J)=P*GRA(J)
            ENDDO
          ENDDO



      CALL SBOCLS(A,MO,M,MV,N,BL,BU,IND,IOPT,XP,RNOC,RNRM,
     &            IER,RW,IW)
      IF(IER.NE.0) THEN
         GOTO 990
      ENDIF
C
C
C 
      DO   I=1,N
        XP(I)=X(I)+XP(I)
      ENDDO
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
C 
C 
C
C 
C     FEHLERQUADRATSUMME FUER XP
C 
C 
C 
C 
      CALL FQUSU(IER,N,XP,XLB,XUB,MF,M,G,FSP,FUNEXT)
      IF(IER.GT.1) GOTO 990
C 
C 
C 
C 
C
C 
      IF(FS1.LE.FSP) THEN 
              IF(FS2.LE.FSP) THEN 
                   GOTO 180 
              ELSE
                   P2=P
                   FS2=FSP
                   GOTO 180 
              ENDIF 
      ELSE
              FS2=FS1 
              P2=P1 
              FS1=FSP 
              P1=P
              GOTO 180
      ENDIF
C
C
C     Abfragen
C
 180  DO I=1,N
        IF(XP(I).LT.XLB(I)-TOL.OR.XP(I).GT.XUB(I)+TOL) THEN
          GOTO 100
        ENDIF
      END DO
      DO I=MF+1,MF+ME
         IF(ABS(G(I)).GT.TOL) THEN
            GOTO 100
         ENDIF
      END DO
      DO I=MF+ME+1,MF+M
         IF(G(I).LT.-TOL) THEN
           GOTO 100
         ENDIF
      END DO

C
C     Prüfen, ob Fehlerquadratsumme kleiner ist
C
C
      IF(FSM.GT.FSP) THEN
           REZOK=.TRUE.
           FSM=FSP
           DO   I=1,N
             XB(I)=XP(I)
           ENDDO
      ENDIF 
      GOTO 100
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
C     NEUER X-WERT
C 
C 
C 
C 
C 
C 
C 
  200 XLAE=0. 
      XL=0. 
      DO   I=1,N
        XLAE=XLAE+(X(I)-XB(I))**2
        XL=XL+X(I)**2
        X(I)=XB(I)
      ENDDO
C 
C
C 
C 
C     ABFRAGEN
C 
C 
C 
C 
C 
C     KEINE VERBESSERUNG GEFUNDEN 
C 
C 
      IF(FS.LE.FSM) GOTO 900
C 
C 
C     RELATIVE AENDERUNG DER FEHLERQUADRATSUMME ZU KLEIN
C 
C 
      FDI=ABS(FSM-FSA)/(FSA+P2R)
      IF(IT.GT.0.AND.FDI.LT.P2R) GOTO 900
      IF(IT.GT.0.AND.FSM.LT.P2T) GOTO 900
C 
C 
C 
C     AENDERUNG DER FEHLERQUADRATSUMME UND AENDERUNG LAENGE DES X-VEKTORS 
C     ZU KLEIN  
C 
C 
C 
      FDI=ABS(FS-FSM)/(FS+P2R)      
      ADI=XLAE/(XL+RANGIN)
      IF(FDI.LT.P2R.AND.ADI.LT.X2T) GOTO 900
      IT=IT+1 
C 
C 
C 
C     ITERATIONSZAEHLER ABGELAUFEN
C 
C 
C 
C 
C 
      IF(IT.GT.ITM) GOTO 900
C 
C 
      GOTO 50
C 
C 
C 
C 
C 

  900 CALL FQUSU(IER,N,X,XLB,XUB,MF,M,F,FS,FUNEXT)
C
      IF(.NOT.REZOK) THEN
         IFAIL=-9999
      ENDIF

C
C               
  910 DEALLOCATE(A,AF,F,G,RW,GRA,BL,BU,XP,XB,IW,IND)
      FVALUE=FSM
      RETURN
  990 IFAIL=IER
      GOTO 910
      END





      SUBROUTINE UMSTO(MO,MF,M,N,A,AF,F)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(MO,*),AF(MO,*),F(*)
C
      DO I=1,M
         A(I,N+1)=-F(MF+I)
         DO J=1,N
            A(I,J)=AF(MF+I,J)
         END DO
      END DO
      DO I=1,MF
         A(M+I,N+1)=-F(I)
         DO J=1,N
            A(M+I,J)=AF(I,J)
         END DO
      END DO
C 
C
C 
      RETURN
      END 



C******************************************************************************
C
      SUBROUTINE HFT(A,MDA,M,N,H)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) :: MDA,M,N,I,J
      DIMENSION A(MDA,*),H(*)
      DATA RAN/1.D80/
      DO J=1,N
        NJ=MIN(N,J+1)
        CALL H12(1,J,J+1,M,A(1,J),1,H(J),A(1,NJ),1,MDA,N-J,RAN)
      END DO
C
C
C     Unterhalb der oberen Dreiecksmatrix wird ausgenullt
C
      DO J=1,N
         DO I=J+1,N
            A(I,J)=0.0
         END DO
      END DO
      DO J=1,N
        DO I=N+1,M
           A(I,J)=0.0
        END DO
      END DO
      RETURN
      END SUBROUTINE
      SUBROUTINE FQUSU(IER,N,X,XL,XU,MF,M,F,FS,FUNEXT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*),XL(*),XU(*),F(*)
      EXTERNAL FUNEXT

      IER=0 
      CALL FUNEXT(N,X,XL,XU,MF,M,F,IER)
      FS=SNRM2(MF,F,1)
      RETURN
      END
C
c
      SUBROUTINE HBT(MDA,M,N,A,NDA,Q)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      DIMENSION A(MDA,N),Q(NDA,N),H(MDA),AA(MDA+1,N)
      DIMENSION EE(N,N),QJ(N,N),U(N)
      DATA RANGES/1.D30/
C
C
C     s. "Solving Least Square Problems" S.75
C
C     A Input (MDA,N)
C     A=R Output (MDA,M)
C     A*Q=(R:0)
C
C
      AA(1:MDA,1:N)=A(1:MDA,1:N)
C
C     Zeile ergänzen wegen A(J+1,1) in H12
C
C      

      DO I=1,N
        AA(MDA+1,I)=0.0
      END DO
      MDIM=MDA+1
C
C     HBT
C
      DO I=1,N
        DO J=1,N
         EE(I,J)=0.0
        END DO
        EE(I,I)=1.0
      END DO
      Q(1:N,1:N)=EE(1:N,1:N)
      DO J=1,M
        CALL H12(1,J,J+1,N,AA(J,1),MDIM,H(J),AA(J+1,1),
     &           MDIM,MDIM,M-J,RANGES)
        DO I=1,J
          U(I)=0.0
        END DO
          U(J)=H(J)
        DO I=J+1,N
          U(I)=AA(J,I)
        END DO
        QJ(1:N,1:N)=EE(1:N,1:N)
        SJ=AA(J,J)
        BJ=SJ*H(J)
        QJ(1:N,1:N)=EE(1:N,1:N)
        DO I=1,N
           DO K=1,N
             QJ(I,K)=QJ(I,K)+U(K)*U(I)/BJ
           END DO
        END DO
        Q(1:N,1:N)=MATMUL(Q(1:N,1:N),QJ(1:N,1:N))
      END DO
      A(1:M,1:N)=AA(1:M,1:N)
      DO J=1,M
        DO I=J+1,N
           A(J,I)=0.0
        END DO
      END DO
C
      RETURN
      END SUBROUTINE
c
C
C******************************************************************************

      SUBROUTINE HFTI(A,MDA,M,N,B,MDB,NB,TAU,
     &                KRANK,RNORM,H,G,IP)                                        
C     C.L.LAWSON AND R.J.HANSON. JET PROPULSION LABORATORY. 1973 JUN 12 
C     TO APPEAR IN 'SOLVING LEAST SQUARES PROBLEMS'. PRENTICE-HALL. 1974
C              SOLVE LEAST SQUARES PROBLEM USING ALGORITHM HFTI
C 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(MDA,*),B(MDB,*),H(*),G(*),RNORM(*)
      DIMENSION IP(*)
      INTEGER(KIND=4) :: NJ
C 
      DATA SZERO/0.D0/,FACTOR/1.D-6/,RANGES/1.D80/,RANGIN/1.D-80/
      DATA SRAN/1.D36/
C 
C 
C 
      HMAX=0.0D0
      FACTOR=10*TINY(TAU)
      K=0 
      LDIAG=MIN(M,N)
      IF(LDIAG.LE.0) GOTO 270
      DO 80 J=1,LDIAG
      IF(J.EQ.1) GOTO 20
C
C     UPDATE SQUARED COLUMN LENGTH AND FIND LMAX
C
      LMAX=J
      DO 10 L=J,N
      AAA=A(J-1,L)
      IF(ABS(AAA).GT.SRAN) AAA=SIGN(SRAN,AAA)
      H(L)=H(L)-AAA**2 
      IF(H(L).GT.H(LMAX)) LMAX=L
  10  CONTINUE
      IF(DIFF(HMAX+FACTOR*H(LMAX),HMAX).GT.0) GOTO 50
C
C     COMPUTE SQUARED COLUMN LENGTH AND FIND LMAX
C
  20  LMAX=J
      DO  L=J,N
      H(L)=0.
      DO   I=J,M
         AAA=A(I,L)
         IF(ABS(AAA).GT.SRAN) AAA=SIGN(SRAN,AAA)
         H(L)=H(L)+AAA**2
      ENDDO
      IF(H(L).GT.H(LMAX)) LMAX=L
      ENDDO
      HMAX=H(LMAX)
C
C     LMAX HAS BEEN DETERMINED
C
C
C     DO COLUMN INTERCHANGE IF NEEDED
C
C
  50  CONTINUE
      IP(J)=LMAX
      IF(IP(J).EQ.J) GOTO 70
      DO 60 I=1,M
      TMP=A(I,J)
      A(I,J)=A(I,LMAX)
      A(I,LMAX)=TMP
  60  CONTINUE
      H(LMAX)=H(J)
C
C     COMPUTE THE J-TH TRANSFORMATION AND APPLY IT TO A AND B
C
C 
C     A(1,J+1) wird für J=N in H12 nich benötigt
C
C
  70  NJ=MIN(N,J+1)
      CALL H12(1,J,J+1,M,A(1,J),1,H(J),A(1,NJ),1,MDA,N-J,RANGES)
      CALL H12(2,J,J+1,M,A(1,J),1,H(J),B,1,MDB,NB,RANGES)
   80 CONTINUE
C 
C  DETERMINE THE PSEUDORANK, K ,USING THE TOLERANCE TAU 
C 
           AMAX=ABS(A(1,1))
           DO  J=1,LDIAG
           IF(ABS(A(J,J))/(AMAX+RANGIN).LE.TAU) GOTO 100  
           ENDDO
       K=LDIAG
       GOTO 110 
  100  K=J-1
  110  KP1=K+1
C 
C
C      COMPUTE THE NORM OF THE RESIDUAL VECTORS
C
       IF(NB.LE.0) GOTO 140
       DO  JB=1,NB
       TMP=SZERO
       IF(KP1.GT.M) THEN     
          TMP=SZERO
       ELSE
          TMP=SNRM2(M-KP1+1,B(KP1,JB),1)
       ENDIF
C      DO 120 I=KP1,M
C      TMP=TMP+B(I,JB)**2
C 120  CONTINUE
       RNORM(JB)=TMP
       ENDDO
  140  CONTINUE
C
C
C      SPEZIAL FOR PSEUDORANK=0
C
C
C
C
       IF(K.GT.0) GOTO 160
       IF(NB.LE.0) GOTO 270
       DO  JB=1,NB
       DO  I=1,N
       B(I,JB)=SZERO
       ENDDO
       ENDDO
       GOTO 270
      
   
C
C 
C
C   IF THE PSEUDONORM IS LESS THAN N COMPUTE HOUSEHOLDER
C   DECOMPOSITION OF FIRST K ROWS 
C 
  160  IF(K.EQ.N) GOTO 180
          DO   II=1,K
          I=KP1-II
          CALL H12(1,I,KP1,N,A(I,1),MDA,G(I),A,MDA,1,I-1,RANGES)
          ENDDO
  180  CONTINUE
C
C
C
C 
       IF(NB.LE.0) GOTO 270
C 
       DO 260 JB=1,NB
C
C 
C      SOLVE THE K BY K TRIANGULAR SYSTEM 
C 
       DO  L=1,K
       SM=SZERO 
       I=KP1-L
       IF(I.EQ.K) GOTO 200 
       IP1=I+1
       DO  J=IP1,K
       SM=SM+A(I,J)*B(J,JB)
       ENDDO
  200  SM1=SM
       B(I,JB)=(B(I,JB)-SM1)/A(I,I)
       ENDDO
C 
C 
C 
C      COMPLETE COMPUTATION OF SOLUTION 
C 
C 
       IF(K.EQ.N) GOTO 240
       DO   J=KP1,N
       B(J,JB)=SZERO
       ENDDO
       DO  I=1,K
       CALL H12(2,I,KP1,N,A(I,1),MDA,G(I),B(1,JB),1,MDB,1,RANGES)
       ENDDO
C
C 
C 
C 
C 
C 
C   RE-ORDER THE SOLUTION VECTOR TO COMPENSATE FOR THE
C   COLUMN INTERCHANGE
C 
  240       DO 250 JJ=1,LDIAG 
            J=LDIAG+1-JJ
            IF(IP(J).EQ.J) GOTO 250 
            L=IP(J) 
            TMP=B(L,JB)
            B(L,JB)=B(J,JB) 
            B(J,JB)=TMP
  250       CONTINUE
  260    CONTINUE
C 
C 
C 
C 
C 
C  THE SOLUTION VECTORS X ARE NOW 
C  IN THE FIRST N ROWS OF THE ARRAY B( , )
C
C
C
 270  KRANK=K
      RETURN
      END 
C

C++++++++++++++++ SINGLE PRECISION VERSION 2SP (AUG 1982) ++++++++++++++
C     SUBROUTINE H12 (MODE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV,RANGES)
C     BASED ON C.L.LAWSON AND R.J.HANSON,
C     'SOLVING LEAST SQUARES PROBLEMS', PRENTICE-HALL, 1974
C
C     CONSTRUCTION AND/OR APPLICATION OF A SINGLE
C     HOUSEHOLDER TRANSFORMATION..     Q = I + U*(U**T)/B
C
C     MODE    = 1 OR 2   TO SELECT ALGORITHM  H1  OR  H2 .
C     LPIVOT IS THE INDEX OF THE PIVOT ELEMENT.
C     L1,M   IF L1 .LE. M   THE TRANSFORMATION WILL BE CONSTRUCTED TO
C            ZERO ELEMENTS INDEXED FROM L1 THROUGH M.   IF L1 GT. M
C            THE SUBROUTINE DOES AN IDENTITY TRANSFORMATION.
C     U(),IUE,UP    ON ENTRY TO H1 U() CONTAINS THE PIVOT VECTOR.
C                   IUE IS THE STORAGE INCREMENT BETWEEN ELEMENTS.
C                                       ON EXIT FROM H1 U() AND UP
C                   CONTAIN QUANTITIES DEFINING THE VECTOR U OF THE
C                   HOUSEHOLDER TRANSFORMATION.   ON ENTRY TO H2 U()
C                   AND UP SHOULD CONTAIN QUANTITIES PREVIOUSLY COMPUTED
C                   BY H1.  THESE WILL NOT BE MODIFIED BY H2.
C     C()    ON ENTRY TO H1 OR H2 C() CONTAINS A MATRIX WHICH WILL BE
C            REGARDED AS A SET OF VECTORS TO WHICH THE HOUSEHOLDER
C            TRANSFORMATION IS TO BE APPLIED.  ON EXIT C() CONTAINS THE
C            SET OF TRANSFORMED VECTORS.
C     ICE    STORAGE INCREMENT BETWEEN ELEMENTS OF VECTORS IN C().
C     ICV    STORAGE INCREMENT BETWEEN VECTORS IN C().
C     NCV    NUMBER OF VECTORS IN C() TO BE TRANSFORMED. IF NCV .LE. 0
C            NO OPERATIONS WILL BE DONE ON C().
C  RANGES IS 2 OR 3 ORDERS OF MAGNITUDE SMALLER THAN BIG, WHERE BIG IS
C      THE LARGEST NUMBER THAT DOES NOT OVERFLOW AND 1/BIG DOES NOT
C      UNDERFLOW.  FOR THE DOUBLE PRECISION VERSION, BIG AND RANGES
C      ARE IN DOUBLE PRECISION.  FOR THE SINGLE PRECISION VERSION,
C      THEY ARE IN SINGLE PRECISION (AND THEREFORE RANGES=SRANGE).
      SUBROUTINE H12 (MODE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV,RANGES)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(IUE,*), C(*)
      DIMENSION HIFF(2)
      DOUBLE PRECISION BB,SM1
      INTEGER*4 I2,I3,I4,INCR
C     DOUBLE PRECISION SM,B,BB,BC,BD
C     DOUBLE PRECISION ABS, AMAX1, C, CL, CLINV, DOUBLE, ONE, RANGES,!DP
C    1 RANGIN, SIGN, SM1, SQRT, U, UP!DP
C     ABS(SM)=DABS(SM)!DP
C     AMAX1(SM,ONE)=DMAX1(SM,ONE)!DP
C     DOUBLE(ONE)=DBLE(ONE)
C     DOUBLE(SM)=SM!DP
C     SQRT(SM)=DSQRT(SM)!DP
C     SIGN(SM,ONE)=DSIGN(SM,ONE)!DP
      ONE=1.D0
C     ONE=1.D0!DP
C
      IF (0.GE.LPIVOT.OR.LPIVOT.GE.L1.OR.L1.GT.M) RETURN
      RANGIN=ONE/RANGES
      CL=ABS(U(1,LPIVOT))
      IF (MODE.EQ.2) GO TO 60
C                            ****** CONSTRUCT THE TRANSFORMATION. ******
C         DO 10 J=L1,M
C  10     CL=MAX(ABS(U(1,J)),CL)
C     IF (CL .LE. RANGIN) GO TO 130
C     CLINV=ONE/CL
C     HILF=U(1,LPIVOT)*CLINV
C     SM=(HILF)**2
C         DO 30 J=L1,M
C         HILF=U(1,J)*CLINV
C  30     SM=SM+(HILF)**2
C
C
C         SM=SDOT(M-L1+1,U(1,L1),IUE,U(1,L1),IUE)
C         SM1=(SM+U(1,LPIVOT)**2)*CLINV**2
C     IF(SM.GT.RANGES) SM=RANGES
C     IF(SM.LE.RANGIN) SM=0.
C     CL=-SIGN(CL*SQRT(SM),U(1,LPIVOT))
      HIFF(1)=SNRM2(M-L1+1,U(1,L1),IUE)
      HIFF(2)=U(1,LPIVOT)
      SM=SNRM2(2,HIFF,1)
      CL=-SIGN(SM,U(1,LPIVOT))
      UP=U(1,LPIVOT)-CL
      U(1,LPIVOT)=CL
      GO TO 70
C            ****** APPLY THE TRANSFORMATION  I+U*(U**T)/B  TO C. ******
C
   60 IF (CL .LE. RANGIN) GO TO 130
   70 IF (NCV.LE.0) RETURN
C     WRITE(1,1200) 1
C1200 FORMAT(' H12 ',I5)
C     WRITE(1,1300) 1,UP,U(1,LPIVOT),BB,RANGIN
C1300 FORMAT(' UP U BB RANGIN ',I5,4E14.7)
      BB=UP*U(1,LPIVOT)
C     WRITE(1,1300) 2,UP,U(1,LPIVOT),BB,RANGIN
C                       B  MUST BE NONPOSITIVE HERE.  IF B = 0., RETURN.
C
      IF (BB.GE. -RANGIN) GO TO 130
C     IF(DABS(BB).GT.RANGES) GOTO 130
C     WRITE(1,1300) 3,UP,U(1,LPIVOT),BB,RANGIN
      B=ONE/BB
C     WRITE(1,1200) 15
      I2=1-ICV+ICE*(LPIVOT-1)
      INCR=ICE*(L1-LPIVOT)
      DO 120 J=1,NCV
          I2=I2+ICV
          I3=I2+INCR
          I4=I3
          SM1=C(I2)*UP
          DO  I=L1,M
              SM1=SM1+C(I3)*U(1,I)
              I3=I3+ICE
          ENDDO
C
C
C             SUMM=SDOT(M-L1+1,C(I3),ICE,U(1,L1),IUE)
C             SM=SM+SUMM
          IF (SM1.EQ.0.D0) GOTO 120
  100     SM=SM1*B
          C(I2)=C(I2)+SM*UP
          DO  I=L1,M
              C(I4)=C(I4)+SM*U(1,I)
              I4=I4+ICE
          ENDDO
C
C
C             CALL SAXPY(M-L1+1,SM,U(1,L1),IUE,C(I4),ICE)
  120 CONTINUE
  130 RETURN
      END
C
C
C
      SUBROUTINE SBOCLS(W,MDW,MCON,MROWS,NCOLS,BL,BU,IND,IOPT,X,RNORMC,
     *   RNORM,MODE,RW,IW)
      USE MODSBOC
C***BEGIN PROLOGUE  SBOCLS
C***DATE WRITTEN   821220   (YYMMDD)
C***REVISION DATE  880722   (YYMMDD)
C***CATEGORY NO.  K1A2A,G2E,G2H1,G2H2
C***KEYWORDS  BOUNDS,CONSTRAINTS,INEQUALITY,LEAST SQUARES,LINEAR
C***AUTHOR  HANSON, R. J., SNLA
C***PURPOSE  Solve the bounded and constrained least squares
C            problem consisting of solving the equation 
C                      E*X = F  (in the least squares sense)
C             subject to the linear constraints
C                            C*X = Y.
C***DESCRIPTION
C
C     This subprogram solves the bounded and constrained least squares
C     problem. The problem statement is: 
C
C     Solve E*X = F (least squares sense), subject to constraints
C     C*X=Y.
C
C     In this formulation both X and Y are unknowns, and both may 
C     have bounds on any of their components.  This formulation 
C     of the problem allows the user to have equality and inequality
C     constraints as well as simple bounds on the solution components.
C
C     This constrained linear least squares subprogram solves E*X=F
C     subject to C*X=Y, where E is MROWS by NCOLS, C is MCON by NCOLS.
C
C      The user must have dimension statements of the form
C
C      DIMENSION W(MDW,NCOLS+MCON+1), BL(NCOLS+MCON), BU(NCOLS+MCON), 
C     * X(2*(NCOLS+MCON)+2+NX), RW(6*NCOLS+5*MCON)
C       INTEGER IND(NCOLS+MCON), IOPT(17+NI), IW(2*(NCOLS+MCON))
C
C     (here NX=number of extra locations required for the options; NX=0
C     if no options are in use. Also NI=number of extra locations 
C     for options 1-9.
C
C    INPUT
C    -----
C
C    -------------------------
C    W(MDW,*),MCON,MROWS,NCOLS
C    -------------------------
C     The array W contains the (possibly null) matrix [C:*] followed by
C     [E:F].  This must be placed in W as follows: 
C          [C  :  *]
C     W  = [       ]
C          [E  :  F]
C     The (*) after C indicates that this data can be undefined. The
C     matrix [E:F] has MROWS rows and NCOLS+1 columns. The matrix C is
C     placed in the first MCON rows of W(*,*) while [E:F]
C     follows in rows MCON+1 through MCON+MROWS of W(*,*). The vector F
C     is placed in rows MCON+1 through MCON+MROWS, column NCOLS+1. The 
C     values of MDW and NCOLS must be positive; the value of MCON must 
C     be nonnegative. An exception to this occurs when using option 1 
C     for accumulation of blocks of equations. In that case MROWS is an
C     OUTPUT variable only, and the matrix data for [E:F] is placed in
C     W(*,*), one block of rows at a time. See IOPT(*) contents, option
C     number 1, for further details. The row dimension, MDW, of the 
C     array W(*,*) must satisfy the inequality: 
C
C     If using option 1,
C                     MDW .ge. MCON + max(max. number of
C                     rows accumulated, NCOLS)
C
C     If using option 8, MDW .ge. MCON + MROWS.
C     Else, MDW .ge. MCON + max(MROWS, NCOLS).
C
C     Other values are errors, but this is checked only when using
C     option=2.  The value of MROWS is an output parameter when 
C     using option number 1 for accumulating large blocks of least
C     squares equations before solving the problem. 
C     See IOPT(*) contents for details about option 1.
C
C    ------------------
C    BL(*),BU(*),IND(*)
C    ------------------
C     These arrays contain the information about the bounds that the
C     solution values are to satisfy. The value of IND(J) tells the
C     type of bound and BL(J) and BU(J) give the explicit values for 
C     the respective upper and lower bounds on the unknowns X and Y.
C     The first NVARS entries of IND(*), BL(*) and BU(*) specify
C     bounds on X; the next MCON entries specify bounds on Y.
C
C    1.    For IND(J)=1, require X(J) .ge. BL(J);
C          IF J.gt.NCOLS,        Y(J-NCOLS) .ge. BL(J). 
C          (the value of BU(J) is not used.)
C    2.    For IND(J)=2, require X(J) .le. BU(J);
C          IF J.gt.NCOLS,        Y(J-NCOLS) .le. BU(J). 
C          (the value of BL(J) is not used.)
C    3.    For IND(J)=3, require X(J) .ge. BL(J) and
C                                X(J) .le. BU(J);
C          IF J.gt.NCOLS,        Y(J-NCOLS) .ge. BL(J) and
C                                Y(J-NCOLS) .le. BU(J). 
C          (to impose equality constraints have BL(J)=BU(J)=
C          constraining value.)
C    4.    For IND(J)=4, no bounds on X(J) or Y(J-NCOLS) are required.
C          (the values of BL(J) and BU(J) are not used.)
C
C     Values other than 1,2,3 or 4 for IND(J) are errors. In the case
C     IND(J)=3 (upper and lower bounds) the condition BL(J) .gt. BU(J)
C     is  an  error.   The values BL(J), BU(J), J .gt. NCOLS, will be 
C     changed.  Significant changes mean that the constraints are
C     infeasible.  (Users must make this decision themselves.)
C     The new values for BL(J), BU(J), J .gt. NCOLS, define a
C     region such that the perturbed problem is feasible.  If users
C     know that their problem is feasible, this step can be skipped
C     by using option number 8 described below.
C
C    -------
C    IOPT(*)
C    -------
C     This is the array where the user can specify nonstandard options
C     for SBOCLS( ). Most of the time this feature can be ignored by
C     setting the input value IOPT(1)=99. Occasionally users may have
C     needs that require use of the following subprogram options. For
C     details about how to use the options see below: IOPT(*) CONTENTS. 
C
C     Option Number   Brief Statement of Purpose
C     ------ ------   ----- --------- -- -------
C           1         Return to user for accumulation of blocks
C                     of least squares equations.  The values
C                     of IOPT(*) are changed with this option.
C                     The changes are updates to pointers for
C                     placing the rows of equations into position 
C                     for processing.
C           2         Check lengths of all arrays used in the
C                     subprogram.
C           3         Column scaling of the data matrix, [C].
C                                                        [E]
C           4         User provides column scaling for matrix [C]. 
C                                                             [E]
C           5         Provide option array to the low-level 
C                     subprogram SBOLS( ).
C                     {Provide option array to the low-level
C                     subprogram SBOLSM( ) by imbedding an
C                     option array within the option array to
C                     SBOLS(). Option 6 is now disabled.}
C           6         Provide additional factor for WT=SQRT(SRELPR) (Change Unterforsthuber)
C           7         Move the IOPT(*) processing pointer.
C           8         Do not preprocess the constraints to
C                     resolve infeasibilities.
C           9         Do not pretriangularize the least squares matrix.
C          99         No more options to change.
C
C    ---- 
C    X(*) 
C    ---- 
C     This array is used to pass data associated with options 4,5 and
C     6. Ignore this parameter (on input) if no options are used.
C     Otherwise see below: IOPT(*) CONTENTS.
C
C
C    OUTPUT
C    ------
C
C    -----------------
C    X(*),RNORMC,RNORM
C    -----------------
C     The array X(*) contains a solution (if MODE .ge.0 or .eq.-22) for
C     the constrained least squares problem. The value RNORMC is the
C     minimum residual vector length for the constraints C*X - Y = 0.
C     The value RNORM is the minimum residual vector length for the
C     least squares equations. Normally RNORMC=0, but in the case of
C     inconsistent constraints this value will be nonzero.
C     The values of X are returned in the first NVARS entries of X(*).
C     The values of Y are returned in the last MCON entries of X(*).
C
C    ---- 
C    MODE 
C    ---- 
C     The sign of MODE determines whether the subprogram has completed
C     normally, or encountered an error condition or abnormal status. A
C     value of MODE .ge. 0 signifies that the subprogram has completed
C     normally. The value of mode (.ge. 0) is the number of variables 
C     in an active status: not at a bound nor at the value zero, for
C     the case of free variables. A negative value of MODE will be one
C     of the cases (-57)-(-41), (-37)-(-22), (-19)-(-2). Values .lt. -1
C     correspond to an abnormal completion of the subprogram. These  
C     error messages are in groups for the subprograms SBOCLS(),
C     SBOLSM(), and SBOLS().  An approximate solution will be returned
C     to the user only when max. iterations is reached, MODE=-22.
C
C    -----------
C    RW(*),IW(*)
C    -----------
C     These are working arrays.  (normally the user can ignore the
C     contents of these arrays.)
C
C    IOPT(*) CONTENTS
C    ------- --------
C     The option array allows a user to modify some internal variables
C     in the subprogram without recompiling the source code. A central
C     goal of the initial software design was to do a good job for most
C     people. Thus the use of options will be restricted to a select
C     group of users. The processing of the option array proceeds as
C     follows: a pointer, here called LP, is initially set to the value
C     1. At the pointer position the option number is extracted and
C     used for locating other information that allows for options to be
C     changed. The portion of the array IOPT(*) that is used for each
C     option is fixed; the user and the subprogram both know how many
C     locations are needed for each option. The value of LP is updated 
C     for each option based on the amount of storage in IOPT(*) that is
C     required. A great deal of error checking is done by the
C     subprogram on the contents of the option array. Nevertheless it
C     is still possible to give the subprogram optional input that is
C     meaningless. For example option 4 uses the locations
C     X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1) for passing scaling data.
C     The user must manage the allocation of these locations.
C
C   1
C   -
C     This option allows the user to solve problems with a large number
C     of rows compared to the number of variables. The idea is that the
C     subprogram returns to the user (perhaps many times) and receives
C     new least squares equations from the calling program unit.
C     Eventually the user signals "that's all" and a solution is then
C     computed. The value of MROWS is an output variable when this 
C     option is used. Its value is always in the range 0 .le. MROWS
C     .le. NCOLS+1. It is the number of rows after the
C     triangularization of the entire set of equations. If LP is the
C     processing pointer for IOPT(*), the usage for the sequential
C     processing of blocks of equations is
C
C
C        IOPT(LP)=1 
C         Move block of equations to W(*,*) starting at
C         the first row of W(*,*).
C        IOPT(LP+3)=# of rows in the block; user defined
C
C     The user now calls SBOCLS( ) in a loop. The value of IOPT(LP+1) 
C     directs the user's action. The value of IOPT(LP+2) points to
C     where the subsequent rows are to be placed in W(*,*). Both of 
C     these values are first defined in the subprogram. The user
C     changes the value of IOPT(LP+1) (to 2) as a signal that all of
C     the rows have been processed. 
C
C
C      .<LOOP
C      . CALL SBOCLS( )
C      . IF(IOPT(LP+1) .EQ. 1) THEN
C      .    IOPT(LP+3)=# OF ROWS IN THE NEW BLOCK; USER DEFINED
C      .    PLACE NEW BLOCK OF IOPT(LP+3) ROWS IN 
C      .    W(*,*) STARTING AT ROW MCON + IOPT(LP+2).
C      .
C      .    IF( THIS IS THE LAST BLOCK OF EQUATIONS ) THEN
C      .       IOPT(LP+1)=2
C      .<------CYCLE LOOP
C      .    ELSE IF (IOPT(LP+1) .EQ. 2) THEN
C      <-------EXIT LOOP SOLUTION COMPUTED IF MODE .GE. 0
C      . ELSE
C      . ERROR CONDITION; SHOULD NOT HAPPEN.
C      .<END LOOP
C
C     Use of this option adds 4 to the required length of IOPT(*).
C
C   2
C   -
C     This option is useful for checking the lengths of all arrays used
C     by SBOCLS( ) against their actual requirements for this problem.
C     The idea is simple: the user's program unit passes the declared 
C     dimension information of the arrays. These values are compared
C     against the problem-dependent needs within the subprogram. If any 
C     of the dimensions are too small an error message is printed and a
C     negative value of MODE is returned, -41 to -47. The printed error
C     message tells how long the dimension should be. If LP is the
C     processing pointer for IOPT(*),
C
C        IOPT(LP)=2 
C        IOPT(LP+1)=Row dimension of W(*,*) 
C        IOPT(LP+2)=Col. dimension of W(*,*)
C        IOPT(LP+3)=Dimensions of BL(*),BU(*),IND(*)
C        IOPT(LP+4)=Dimension of X(*)
C        IOPT(LP+5)=Dimension of RW(*)
C        IOPT(LP+6)=Dimension of IW(*)
C        IOPT(LP+7)=Dimension of IOPT(*)
C         .
C        CALL SBOCLS( )
C
C     Use of this option adds 8 to the required length of IOPT(*).
C
C   3
C   -
C     This option can change the type of scaling for the data matrix. 
C     Nominally each nonzero column of the matrix is scaled so that the
C     magnitude of its largest entry is equal to the value ONE. If LP
C     is the processing pointer for IOPT(*), 
C
C        IOPT(LP)=3 
C        IOPT(LP+1)=1,2 or 3
C            1= Nominal scaling as noted;
C            2= Each nonzero column scaled to have length ONE;
C            3= Identity scaling; scaling effectively suppressed. 
C         .
C        CALL SBOCLS( )
C
C     Use of this option adds 2 to the required length of IOPT(*).
C
C   4
C   -
C     This options allows the user to provide arbitrary (positive)
C     column scaling for the matrix. If LP is the processing pointer
C     for IOPT(*),
C
C        IOPT(LP)=4 
C        IOPT(LP+1)=IOFF
C        X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1)
C        = Positive scale factors for cols. of E.
C         .
C        CALL SBOCLS( )
C
C     Use of this option adds 2 to the required length of IOPT(*)
C     and NCOLS to the required length of X(*).
C
C   5
C   -
C     This option allows the user to provide an option array to the
C     low-level subprogram SBOLS( ). If LP is the processing pointer 
C     for IOPT(*),
C
C        IOPT(LP)=5 
C        IOPT(LP+1)= Position in IOPT(*) where option array 
C                    data for SBOLS( ) begins. 
C         .
C        CALL SBOCLS( )
C
C     Use of this option adds 2 to the required length of IOPT(*).
C
C   6
C   -
C     This option is no longer operative.  To pass an option array
C     to the low-level subprogram SBOLSM( ), imbed it within an option
C     array passed to SBOLS() using option 5.
C     Option is now reserved for providing an additional factor for WT (eta in the publication)
C     IOPT(LP+1) contains the exponent of 10. E.g. if IOPT(LP+1)=-3 then
C     WT is multiplied by 10**(-3)=0.001
C
C   7
C   -
C     Move the processing pointer (either forward or backward) to the
C     location IOPT(LP+1). The processing pointer moves to locations
C     LP+2 if option number 7 is used with the value -7.  For 
C     example to skip over locations 3,...,NCOLS+2,
C
C       IOPT(1)=7
C       IOPT(2)=NCOLS+3
C       (IOPT(I), I=3,...,NCOLS+2 are not defined here.)
C       IOPT(NCOLS+3)=99
C       CALL SBOCLS( )
C
C     CAUTION: Misuse of this option can yield some very hard-to-find
C     bugs. Use it with care. It is intended to be used for passing
C     option arrays to other subprograms.
C
C   8
C   -
C     This option allows the user to suppress the algorithmic feature
C     of SBOCLS( ) that processes the constraint equations C*X = Y and
C     resolves infeasibilities. The steps normally done are to solve
C     C*X - Y = 0 in a least squares sense using the stated bounds on
C     both X and Y. Then the "reachable" vector Y = C*X is computed
C     using the solution X obtained. Finally the stated bounds for Y are
C     enlarged to include C*X. To suppress the feature:  
C
C
C       IOPT(LP)=8
C         .
C       CALL SBOCLS( )
C
C     Use of this option adds 1 to the required length of IOPT(*).
C
C   9
C   -
C     This option allows the user to suppress the pretriangularizing
C     step of the least squares matrix that is done within SBOCLS( ).
C     This is primarily a means of enhancing the subprogram efficiency
C     and has little effect on accuracy. To suppress the step, set: 
C
C       IOPT(LP)=9
C         .
C       CALL SBOCLS( )
C
C     Use of this option adds 1 to the required length of IOPT(*).
C
C   99
C   --
C     There are no more options to change.
C
C     Only option numbers -99, -9,-8,...,-1, 1,2,...,9, and 99 are
C     permitted. Other values are errors. Options -99,-1,...,-9 mean
C     that the respective options 99,1,...,9 are left at their default
C     values. An example is the option to suppress the preprocessing of
C     contraints:   
C
C       IOPT(1)=-8 Option is recognized but not changed
C       IOPT(2)=99
C       CALL SBOCLS( )
C
C    Error Messages for SBOCLS()
C    ----- -------- --- --------
C
C WARNING in... 
C SBOCLS(). THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE. THE NUMBER
C OF EFFECTIVE ROWS=(I2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        41
C
C WARNING IN...
C SBOCLS(). THE COLUMN DIMENSION OF W(,)=(I1) MUST BE .GE. NCOLS+
C MCON+1=(I2).
C           IN ABOVE MESSAGE, I1=         2
C           IN ABOVE MESSAGE, I2=         3
C ERROR NUMBER =        42
C
C WARNING IN...
C SBOCLS(). THE DIMENSIONS OF THE ARRAYS BL(),BU(), AND IND()=(I1)
C MUST BE .GE. NCOLS+MCON=(I2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        43
C
C WARNING IN...
C SBOCLS(). THE DIMENSION OF X()=(I1) MUST BE
C .GE. THE REQD.LENGTH=(I2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        44
C
C WARNING IN...
C SBOCLS(). THE .
C SBOCLS() THE DIMENSION OF IW()=(I1) MUST BE .GE. 2*NCOLS+2*MCON=(I2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         4
C ERROR NUMBER =        46
C
C WARNING IN...
C SBOCLS(). THE DIMENSION OF IOPT()=(I1) MUST BE .GE. THE REQD.
C LEN.=(I2).
C           IN ABOVE MESSAGE, I1=        16
C           IN ABOVE MESSAGE, I2=        18
C ERROR NUMBER =        47
C
C WARNING IN...
C SBOCLS(). ISCALE OPTION=(I1) MUST BE 1-3.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =        48
C
C WARNING IN...
C SBOCLS(). OFFSET PAST X(NCOLS) (I1) FOR USER-PROVIDED COLUMN SCALING
C MUST BE POSITIVE. 
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =        49
C
C WARNING IN...
C SBOCLS(). EACH PROVIDED COL. SCALE FACTOR MUST BE POSITIVE.
C  COMPONENT (I1) NOW = (R1). 
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, R1=    0. 
C ERROR NUMBER =        50
C
C WARNING IN...
C SBOCLS(). THE OPTION NUMBER=(I1) IS NOT DEFINED.
C           IN ABOVE MESSAGE, I1=      1001
C ERROR NUMBER =        51
C
C WARNING IN...
C SBOCLS(). NO. OF ROWS=(I1) MUST BE .GE. 0 .AND. .LE. MDW-MCON=(I2). 
C           IN ABOVE MESSAGE, I1=         2
C           IN ABOVE MESSAGE, I2=         1
C ERROR NUMBER =        52
C
C WARNING IN...
C SBOCLS(). MDW=(I1) MUST BE POSITIVE.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =        53
C
C WARNING IN...
C SBOCLS(). MCON=(I1) MUST BE NONNEGATIVE.
C           IN ABOVE MESSAGE, I1=        -1
C ERROR NUMBER =        54
C
C WARNING IN...
C SBOCLS(). NCOLS=(I1) THE NO. OF VARIABLES MUST BE POSITIVE.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =        55
C
C WARNING IN...
C SBOCLS(). FOR J=(I1), IND(J)=(I2) MUST BE 1-4.
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         0
C ERROR NUMBER =        56
C
C WARNING IN...
C SBOCLS(). FOR J=(I1), BOUND BL(J)=(R1) IS .GT. BU(J)=(R2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, R1=     .1000000000E+01
C           IN ABOVE MESSAGE, R2=    0. 
C ERROR NUMBER =        57
C           LINEAR CONSTRAINTS, SNLA REPT. SAND82-1517, AUG., (1982). 
C***REFERENCES  HANSON, R. J. LINEAR LEAST SQUARES WITH BOUNDS AND
C                 LINEAR CONSTRAINTS, SIAM J. SCI. STAT. COMPUT., VOL. 7,
C                 NO. 3, JULY, 1986.
C***ROUTINES CALLED  R1MACH,SASUM,SBOLS,SCOPY,SDOT,SNRM2,SSCAL,XERRWV 
C***MODUL    MODSBOC
C***END PROLOGUE  SBOCLS
C     REVISED 880722-1100
C     REVISED YYMMDD-HHMM
C
C    PURPOSE
C    -------
C     THIS IS THE MAIN SUBPROGRAM THAT SOLVES THE LEAST SQUARES
C     PROBLEM CONSISTING OF LINEAR CONSTRAINTS
C
C              C*X = Y
C
C     AND LEAST SQUARES EQUATIONS
C
C              E*X = F
C
C     IN THIS FORMULATION THE VECTORS X AND Y ARE BOTH UNKNOWNS.
C     FURTHER, X AND Y MAY BOTH HAVE USER-SPECIFIED BOUNDS ON EACH
C     COMPONENT.  THE USER MUST HAVE DIMENSION STATEMENTS OF THE
C     FORM
C
C     DIMENSION W(MDW,NCOLS+MCON+1), BL(NCOLS+MCON),BU(NCOLS+MCON),
C               X(2*(NCOLS+MCON)+2+NX), RW(6*NCOLS+5*MCON)
C
C     INTEGER IND(NCOLS+MCON), IOPT(16+NI), IW(2*(NCOLS+MCON))
C
C     TO CHANGE THIS SUBPROGRAM FROM SINGLE TO DOUBLE PRECISION BEGIN 
C     EDITING AT THE CARD 'C++'.
C     CHANGE THIS SUBPROGRAM TO SBOCLS AND THE STRINGS
C     /SDOT/ TO /DDOT/, /SNRM2/ TO /DNRM2/, /SRELPR/ TO /DRELPR/,
C     /R1MACH/ TO /D1MACH/, /E0/ TO /D0/, /SCOPY/ TO /DCOPY/,
C     /SSCAL/ TO /DSCAL/, /SASUM/ TO /DASUM/, /SBOLS/ TO /DBOLS/,
C     /REAL            / TO /DOUBLE PRECISION/.
C ++
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION W(MDW,*),BL(*),BU(*),X(*),RW(*)
      DOUBLE PRECISION ANORM, CNORM, ONE, RNORM, RNORMC, SRELPR
      DOUBLE PRECISION T, T1, T2, SDOT, SNRM2, WT, ZERO ,WTSPR
      DOUBLE PRECISION SASUM, R1MACH
C     THIS VARIABLE REMAINS TYPED REAL. 
      DOUBLE PRECISION RDUM
      INTEGER IND(*),IOPT(*),IW(*),JOPT(05)
      LOGICAL CHECKL,FILTER,ACCUM,PRETRI
      SAVE IGO,ACCUM,CHECKL
      DATA IGO/0/
C***FIRST EXECUTABLE STATEMENT  SBOCLS
      NERR = 0
      MODE = 0
      LEVEL = 1
C
C     Änderung (Unterforsthuber)
C     WTSPR ist Standardwert für WT (eta in Hanson Publikation)
C     WTSPR=1. (Orginalwert)
      WTSPR=WT1
      IF (IGO.EQ.0) THEN
C     DO(CHECK VALIDITY OF INPUT DATA)
C     PROCEDURE(CHECK VALIDITY OF INPUT DATA)
C
C     SEE THAT MDW IS .GT.0. GROSS CHECK ONLY.
          IF (MDW.LE.0) THEN
              NERR = 53
              NCHAR = 36
              CALL XERRWV('SBOCLS(). MDW=(I1) MUST BE POSITIVE.',NCHAR,
     *                    NERR,LEVEL,1,MDW,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          END IF
C
C     SEE THAT NUMBER OF CONSTRAINTS IS NONNEGATIVE.
          IF (MCON.LT.0) THEN
              NERR = 54
              NCHAR = 40
              CALL XERRWV('SBOCLS(). MCON=(I1) MUST BE NONNEGATIVE.', 
     *                    NCHAR,NERR,LEVEL,1,MCON,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          END IF
C
C     SEE THAT NUMBER OF UNKNOWNS IS POSITIVE.
          IF (NCOLS.LE.0) THEN
              NERR = 55
              NCHAR = 59
              CALL XERRWV(
     *     'SBOCLS(). NCOLS=(I1) THE NO. OF VARIABLES MUST BE POSITIVE.'
     *                    ,NCHAR,NERR,LEVEL,1,NCOLS,IDUM,0,RDUM,RDUM) 
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          END IF
C
C     SEE THAT CONSTRAINT INDICATORS ARE ALL WELL-DEFINED.
          DO 10 J = 1,NCOLS + MCON
              IF (IND(J).LT.1 .OR. IND(J).GT.4) THEN
                  NERR = 56
                  NCHAR = 46
                  CALL XERRWV(
     *                  'SBOCLS(). FOR J=(I1), IND(J)=(I2) MUST BE 1-4.'
     *                        ,NCHAR,NERR,LEVEL,2,J,IND(J),0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
   10     CONTINUE
C
C     SEE THAT BOUNDS ARE CONSISTENT.
          DO 20 J = 1,NCOLS + MCON
              IF (IND(J).EQ.3) THEN
                  IF (BL(J).GT.BU(J)) THEN
                      NERR = 57
                      NCHAR = 58
                      CALL XERRWV(
     *      'SBOCLS(). FOR J=(I1), BOUND BL(J)=(R1) IS .GT. BU(J)=(R2).'
     *                            ,NCHAR,NERR,LEVEL,1,J,IDUM,2,BL(J), 
     *                            BU(J))
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 260
                  END IF
              END IF
   20     CONTINUE
C     END PROCEDURE 
C     DO(PROCESS OPTION ARRAY)
C     PROCEDURE(PROCESS OPTION ARRAY)
          ZERO = 0.D0
          ZERU = TINY(ZERO)
          ONE = 1.D0
C
C
          SRELPR =R1MACH(4)
          CHECKL = .FALSE.
          FILTER = .TRUE.
          LENX = 2* (NCOLS+MCON) + 2
          ISCALE = 1
          IGO = 1
          ACCUM = .FALSE.
          PRETRI = .TRUE.
          LOPT = 0
          LP = 0
          LDS = 0
C     DO FOREVER
   30     CONTINUE
          LP = LP + LDS
          IP = IOPT(LP+1)
          JP = ABS(IP)
C
C     TEST FOR NO MORE OPTIONS TO CHANGE.
          IF (IP.EQ.99) THEN
              IF (LOPT.EQ.0) LOPT = LP+1
C
C     SEND COL. SCALING TO SBOLS().
              IDOPE(4)=1
C
C     NOTE THAT SBOLS() WAS CALLED BY SBOCLS()
              IDOPE(5)=1
C
C     CHANGE PRETRIANGULARIZATION FACTOR IN SBOLSM().
              IDOPE(1) = NCOLS + MCON + 1
C
C     PASS WEIGHT TO SBOLSM() FOR RANK TEST.
              IDOPE(2) = NCOLS + MCON + 2
              IDOPE(3) = MCON 
C     EXIT FOREVER
              GO TO 50
          ELSE IF (JP.EQ.99) THEN
              LDS = 1
C     CYCLE FOREVER 
              GO TO 50
          ELSE IF (JP.EQ.1) THEN
              IF (IP.GT.0) THEN
C
C     SET UP DIRECTION FLAG LOCATION, ROW STACKING POINTER
C     LOCATION, AND LOCATION FOR NUMBER OF NEW ROWS.
                  LOCACC = LP + 2
C
C                  IOPT(LOCACC-1)=OPTION NUMBER FOR SEQ. ACCUMULATION.
C     CONTENTS..   IOPT(LOCACC  )=USER DIRECTION FLAG, 1 OR 2.
C                  IOPT(LOCACC+1)=ROW STACKING POINTER.
C                  IOPT(LOCACC+2)=NUMBER OF NEW ROWS TO PROCESS.
C     USER ACTION WITH THIS OPTION..
C      (SET UP OPTION DATA FOR SEQ. ACCUMULATION IN IOPT(*).)
C      (MOVE BLOCK OF EQUATIONS INTO W(*,*)  STARTING AT FIRST
C       ROW OF W(*,*) BELOW THE ROWS FOR THE CONSTRAINT MATRIX C.
C       SET IOPT(LOCACC+2)=NO. OF LEAST SQUARES EQUATIONS IN BLOCK.
C              LOOP 
C              CALL SBOCLS()
C
C                  IF(IOPT(LOCACC) .EQ. 1) THEN
C                      STACK EQUAS. INTO W(*,*), STARTING AT
C                      ROW IOPT(LOCACC+1).
C                       INTO W(*,*).
C                       SET IOPT(LOCACC+2)=NO. OF EQUAS.
C                      IF LAST BLOCK OF EQUAS., SET IOPT(LOCACC)=2.
C                  ELSE IF IOPT(LOCACC) .EQ. 2) THEN
C                      (PROCESS IS OVER. EXIT LOOP.)
C                  ELSE
C                      (ERROR CONDITION. SHOULD NOT HAPPEN.)
C                  END IF
C              END LOOP
                  IOPT(LOCACC+1) = MCON + 1
                  ACCUM = .TRUE.
                  IOPT(LOCACC) = IGO
              END IF
              LDS = 4
C     CYCLE FOREVER 
              GO TO 30
          ELSE IF (JP.EQ.2) THEN
              IF (IP.GT.0) THEN
C
C     GET ACTUAL LENGTHS OF ARRAYS FOR CHECKING AGAINST NEEDS.
                  LOCDIM = LP + 2
C
C     LMDW.GE.MCON+MAX(MOUT,NCOLS), IF MCON.GT.0 .AND FILTER
C     LMDW.GE.MCON+MOUT, OTHERWISE
C
C     LNDW.GE.NCOLS+MCON+1
C     LLB .GE.NCOLS+MCON
C     LLX .GE.2*(NCOLS+MCON)+2+EXTRA REQD. IN OPTIONS.
C     LLRW.GE.6*NCOLS+5*MCON
C     LLIW.GE.2*(NCOLS+MCON)
C     LIOP.GE. AMOUNT REQD. FOR OPTION ARRAY.
                  LMDW = IOPT(LOCDIM)
                  LNDW = IOPT(LOCDIM+1) 
                  LLB = IOPT(LOCDIM+2)
                  LLX = IOPT(LOCDIM+3)
                  LLRW = IOPT(LOCDIM+4) 
                  LLIW = IOPT(LOCDIM+5) 
                  LIOPT = IOPT(LOCDIM+6)
                  CHECKL = .TRUE.
              END IF
              LDS = 8
C     CYCLE FOREVER 
              GO TO 30
C
C     OPTION TO MODIFY THE COLUMN SCALING.
          ELSE IF (JP.EQ.3) THEN
              IF (IP.GT.0) THEN
                  ISCALE = IOPT(LP+2)
C
C     SEE THAT ISCALE IS 1 THRU 3.
                  IF (ISCALE.LT.1 .OR. ISCALE.GT.3) THEN
                      NERR = 48
                      NCHAR = 41
                      CALL XERRWV(
     *                       'SBOCLS(). ISCALE OPTION=(I1) MUST BE 1-3.'
     *                            ,NCHAR,NERR,LEVEL,1,ISCALE,IDUM,0,
     *                            RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 260
                  END IF
              END IF
              LDS = 2
C     CYCLE FOREVER 
              GO TO 30
C
C     IN THIS OPTION THE USER HAS PROVIDED SCALING.  THE
C     SCALE FACTORS FOR THE COLUMNS BEGIN IN X(NCOLS+IOPT(LP+2)).
          ELSE IF (JP.EQ.4) THEN
              IF (IP.GT.0) THEN
                  ISCALE = 4
                  IF (IOPT(LP+2).LE.0) THEN
                      NERR = 49
                      NCHAR = 86
                      CALL XERRWV(
     *'SBOCLS(). OFFSET PAST X(NCOLS) (I1) FOR USER-PROVIDED COLUMN SCAL
     *ING MUST BE POSITIVE.',
     *    NCHAR,NERR,LEVEL,1,IOPT(LP+2),IDUM,0,RDUM,
     *                            RDUM) 
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 260
                  END IF
                  CALL SCOPY(NCOLS,X(NCOLS+IOPT(LP+2)),1,RW,1)
                  LENX = LENX + NCOLS
                  DO 40 J = 1,NCOLS
                      IF (RW(J).LE.ZERO) THEN
                          NERR = 50
                          NCHAR = 84
                          CALL XERRWV(
     *'SBOCLS(). EACH PROVIDED COL. SCALE FACTOR MUST BE POSITIVE. COMP.
     * (I1)   NOW = (R1).',NCHAR,NERR,LEVEL,1,J,IDUM,1,RW(J),RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                          GO TO 260
                      END IF
   40             CONTINUE
              END IF
              LDS = 2
C     CYCLE FOREVER 
              GO TO 30
C
C     IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO SBOLS().
          ELSE IF (JP.EQ.5) THEN
              IF (IP.GT.0) THEN
                  LOPT = IOPT(LP+2)
              END IF
              LDS = 2
C     CYCLE FOREVER 
              GO TO 30
C
C     IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO SBOLSM().
C     (NO LONGER USED.) OPTION NOW MUST BE PASSED IMBEDDED IN
C     OPTION ARRAY FOR SBOLS().
C          ELSE IF (JP.EQ.6) THEN
C              LDS = 2
C     CYCLE FOREVER 
C              GO TO 30
C     JP=6 (additional factor for WT)
           ELSEIF(JP.EQ.6) THEN
               WTSPR=0.D0
               IF(IP.GT.0) THEN
                 WTSPR=10.**(IOPT(LP+2))
               ENDIF
               LDS=2
               GOTO 30

C
C     THIS OPTION USES THE NEXT LOC OF IOPT(*) AS A
C     POINTER VALUE TO SKIP TO NEXT.
          ELSE IF (JP.EQ.7) THEN
              IF (IP.GT.0) THEN
                  LP = IOPT(LP+2)-1
                  LDS = 0
              ELSE
                  LDS = 2
              END IF
C     CYCLE FOREVER 
              GO TO 30
C
C     THIS OPTION AVOIDS THE CONSTRAINT RESOLVING PHASE FOR 
C     THE LINEAR CONSTRAINTS C*X=Y.
          ELSE IF (JP.EQ.8) THEN
              FILTER = .NOT. (IP.GT.0)
              LDS = 1
C     CYCLE FOREVER 
              GO TO 30
C
C     THIS OPTION SUPPRESSES PRETRIANGULARIZATION OF THE LEAST
C     SQUARES EQATIONS.
          ELSE IF (JP.EQ.9) THEN
              PRETRI = .NOT. (IP.GT.0)
              LDS = 1
C     CYCLE FOREVER 
              GO TO 30
C
C     NO VALID OPTION NUMBER WAS NOTED. THIS IS AN ERROR CONDITION.
          ELSE
              NERR = 51
              NCHAR = 48
              CALL XERRWV(
     *                'SBOCLS(). THE OPTION NUMBER=(I1) IS NOT DEFINED.'
     *                    ,NCHAR,NERR,LEVEL,1,JP,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          END IF
C     END FOREVER
C     END PROCEDURE 
   50     CONTINUE
          IF (CHECKL) THEN
C     DO(CHECK LENGTHS OF ARRAYS)
C     PROCEDURE(CHECK LENGTHS OF ARRAYS)
C
C     THIS FEATURE ALLOWS THE USER TO MAKE SURE THAT THE
C     ARRAYS ARE LONG ENOUGH FOR THE INTENDED PROBLEM SIZE AND USE.
           IF(FILTER .AND. .NOT.ACCUM) THEN
                MDWL=MCON+MAX(MROWS,NCOLS)
           ELSE IF (ACCUM) THEN
                MDWL=MCON+NCOLS+1
           ELSE
                MDWL=MCON+NCOLS
           END IF
              IF (LMDW.LT.MDWL) THEN
                  NERR = 41
                  NCHAR = 88
                  CALL XERRWV(
     *'SBOCLS(). THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE. THE NUMBER
     *OF EFFECTIVE ROWS=(I2).',NCHAR,NERR,LEVEL,2,LMDW,
     *MDWL,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LNDW.LT.NCOLS+MCON+1) THEN
                  NERR = 42
                  NCHAR = 75
                  CALL XERRWV(
     *'SBOCLS(). THE COLUMN DIMENSION OF W(,)=(I1) MUST BE .GE. NCOLS+MC
     *ON+1=(I2).',NCHAR,NERR,LEVEL,2,LNDW,NCOLS+MCON+1,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LLB.LT.NCOLS+MCON) THEN
                  NERR = 43
                  NCHAR = 94
                  CALL XERRWV(
     *'SBOCLS(). THE DIMENSIONS OF THE ARRAYS BL(),BU(), AND IND()=(I1)
     *MUST BE .GE. NCOLS+MCON=(I2).',NCHAR,NERR,LEVEL,2,LLB,NCOLS+MCON,
     *                        0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LLX.LT.LENX) THEN
                  NERR = 44
                  NCHAR = 71
                  CALL XERRWV(
     *'SBOCLS(). THE DIMENSION OF X()=(I1) MUST BE .GE. THE REQD. LENGTH
     *=(I2).',NCHAR,NERR,LEVEL,2,LLX,LENX,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LLRW.LT.6*NCOLS+5*MCON) THEN
                  NERR = 45
                  NCHAR = 70
                  CALL XERRWV(
     *'SBOCLS(). THE DIMENSION OF RW()=(I1) MUST BE .GE. 6*NCOLS+5*MCON=
     *(I2).',NCHAR,NERR,LEVEL,2,LLRW,6*NCOLS+5*MCON,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LLIW.LT.2*NCOLS+2*MCON) THEN
                  NERR = 46
                  NCHAR = 69
                  CALL XERRWV(
     *'SBOCLS() THE DIMENSION OF IW()=(I1) MUST BE .GE. 2*NCOLS+2*MCON=(
     *I2).',NCHAR,NERR,LEVEL,2,LLIW,2*NCOLS+2*MCON,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LIOPT.LT.LP+17) THEN
                  NERR = 47
                  NCHAR = 72
                  CALL XERRWV(
     *'SBOCLS(). THE DIMENSION OF IOPT()=(I1) MUST BE .GE. THE REQD. LEN
     *.=(I2).',NCHAR,NERR,LEVEL,2,LIOPT,LP+17,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
C     END PROCEDURE 
          END IF
      END IF
C
C     OPTIONALLY GO BACK TO THE USER FOR ACCUMULATION OF LEAST SQUARES
C     EQUATIONS AND DIRECTIONS FOR PROCESSING THESE EQUATIONS.
C     DO(ACCUMULATE LEAST SQUARES EQUATIONS)
C     PROCEDURE(ACCUMULATE LEAST SQUARES EQUATIONS)
      IF (ACCUM) THEN
          MROWS = IOPT(LOCACC+1) - 1 - MCON
          INROWS = IOPT(LOCACC+2)
          MNEW = MROWS + INROWS
          IF (MNEW.LT.0 .OR. MNEW+MCON.GT.MDW) THEN
              NERR = 52
              NCHAR = 66
              CALL XERRWV(
     *'SBOCLS(). NO. OF ROWS=(I1) MUST BE .GE. 0 .AND. .LE.MDW-MCON=(I2)
     *',NCHAR,NERR,LEVEL,2,MNEW,MDW-MCON,0,RDUM,RDUM)
C    (RETURN TO USER PROGRAM UNIT)
              GO TO 260
          END IF
      END IF
C
C     USE THE SOFTWARE OF SBOLS( ) FOR THE TRIANGULARIZATION OF THE
C     LEAST SQUARES MATRIX.  THIS MAY INVOLVE A SYSTALTIC INTERCHANGE 
C     OF PROCESSING POINTERS BETWEEN THE CALLING AND CALLED (SBOLS()) 
C     PROGRAM UNITS.
      JOPT(01) = 1
      JOPT(02) = 2
      JOPT(04) = MROWS
      JOPT(05) = 99 
      IRW = NCOLS + 1
      IIW = 1
      IF (ACCUM .OR. PRETRI) THEN
C
C     NOTE THAT SBOLS() WAS CALLED BY SBOCLS()
              IDOPE(5)=0
          CALL SBOLS(W(MCON+1,1),MDW,MOUT,NCOLS,BL,BU,IND,JOPT,X,RNORM,
     *               MODE,RW(IRW),IW(IIW))
      ELSE
          MOUT = MROWS
      END IF
      IF (ACCUM) THEN
          ACCUM = IOPT(LOCACC) .EQ. 1
          IOPT(LOCACC+1) = JOPT(03) + MCON
          MROWS = MIN(NCOLS+1,MNEW)
      END IF
C     END PROCEDURE 
      IF (ACCUM) goto 990
C     DO(SOLVE CONSTRAINED AND BOUNDED LEAST SQUARES PROBLEM)
C     PROCEDURE(SOLVE CONSTRAINED AND BOUNDED LEAST SQUARES PROBLEM)
C
C     MOVE RIGHT HAND SIDE OF LEAST SQUARES EQUATIONS.
      CALL SCOPY(MOUT,W(MCON+1,NCOLS+1),1,W(MCON+1,NCOLS+MCON+1),1)
      IF (MCON.GT.0 .AND. FILTER) THEN
C
C     PROJECT THE LINEAR CONSTRAINTS INTO A REACHABLE SET.
          DO 60 I = 1,MCON
              CALL SCOPY(NCOLS,W(I,1),MDW,W(MCON+1,NCOLS+I),1)
   60     CONTINUE
C
C      PLACE (-)IDENTITY MATRIX AFTER CONSTRAINT DATA.
          DO 70 J = NCOLS + 1,NCOLS + MCON + 1
              W(1,J) = ZERO
              CALL SCOPY(MCON,W(1,J),0,W(1,J),1)
   70     CONTINUE
          W(1,NCOLS+1) = -ONE 
          CALL SCOPY(MCON,W(1,NCOLS+1),0,W(1,NCOLS+1),MDW+1)
C
C     OBTAIN A 'FEASIBLE POINT' FOR THE LINEAR CONSTRAINTS. 
          JOPT(01) = 99
          IRW = NCOLS + 1
          IIW = 1
C
C     NOTE THAT SBOLS() WAS CALLED BY SBOCLS()
              IDOPE(5)=0
          MNCDUM=NCOLS+MCON
          CALL SBOLS(W,MDW,MCON,MNCDUM,BL,BU,IND,JOPT,X,RNORMC,
     *               MODEC,RW(IRW),IW(IIW))

C
C     ENLARGE THE BOUNDS SET, IF REQUIRED, TO INCLUDE POINTS THAT
C     CAN BE REACHED.

          DO 130 J = NCOLS + 1,NCOLS + MCON
              ICASE = IND(J)
              IF (ICASE.LT.4) THEN
                  T = SDOT(NCOLS,W(MCON+1,J),1,X,1)
              END IF
              GO TO (80,90,100,110),ICASE
              GO TO 120
C     CASE 1
   80         BL(J) = MIN(T,BL(J))
              GO TO 120
C     CASE 2
   90         BU(J) = MAX(T,BU(J))
              GO TO 120
C     CASE 3
  100         BL(J) = MIN(T,BL(J))
              BU(J) = MAX(T,BU(J))
              GO TO 120
C     CASE 4
  110         CONTINUE
  120         CONTINUE
  130     CONTINUE

C
C     MOVE CONSTRAINT DATA BACK TO THE ORIGINAL AREA.
          DO 140 J = NCOLS + 1,NCOLS + MCON
              CALL SCOPY(NCOLS,W(MCON+1,J),1,W(J-NCOLS,1),MDW)
  140     CONTINUE
      END IF
      IF (MCON.GT.0) THEN
          DO 150 J = NCOLS + 1,NCOLS + MCON
              W(MCON+1,J) = ZERO
              CALL SCOPY(MOUT,W(MCON+1,J),0,W(MCON+1,J),1)
  150     CONTINUE

C
C     PUT IN (-)IDENTITY MATRIX (POSSIBLY) ONCE AGAIN.
          DO 160 J = NCOLS + 1,NCOLS + MCON + 1
              W(1,J) = ZERO
              CALL SCOPY(MCON,W(1,J),0,W(1,J),1)
  160     CONTINUE

          W(1,NCOLS+1) = -ONE 
          CALL SCOPY(MCON,W(1,NCOLS+1),0,W(1,NCOLS+1),MDW+1)
      END IF
C
C     COMPUTE NOMINAL COLUMN SCALING FOR THE UNWEIGHTED MATRIX.
      CNORM = ZERO
      ANORM = ZERO
      DO 170 J = 1,NCOLS
          T1 = SASUM(MCON,W(1,J),1)
C         T2 = SASUM(MOUT,W(MCON+1,1),1)
C         Änderung Unterforsthuber (27.11.97)
          T2 = SASUM(MOUT,W(MCON+1,J),1)
          T = T1 + T2
          IF (ABS(T).LE.ZERU) T = ONE
          CNORM = MAX(CNORM,T1)
          ANORM = MAX(ANORM,T2)
          X(NCOLS+MCON+J) = ONE/T
  170 CONTINUE

      GO TO (180,190,210,220),ISCALE
      GO TO 230
C     CASE 1
  180 CONTINUE
      GO TO 230
C     CASE 2
C
C     SCALE COLS. (BEFORE WEIGHTING) TO HAVE LENGTH ONE.
  190 DO 200 J = 1,NCOLS
          T = SNRM2(MCON+MOUT,W(1,J),1) 
          IF (ABS(T).LE.ZERU) T = ONE
          X(NCOLS+MCON+J) = ONE/T
  200 CONTINUE
      GO TO 230
C     CASE 3
C
C     SUPPRESS SCALING (USE UNIT MATRIX).
  210 X(NCOLS+MCON+1) = ONE
      CALL SCOPY(NCOLS,X(NCOLS+MCON+1),0,X(NCOLS+MCON+1),1) 
      GO TO 230
C     CASE 4
C
C     THE USER HAS PROVIDED SCALING.
  220 CALL SCOPY(NCOLS,RW,1,X(NCOLS+MCON+1),1)
  230 CONTINUE

      DO 240 J = NCOLS + 1,NCOLS + MCON 
          X(NCOLS+MCON+J) = ONE
  240 CONTINUE
C
C     WEIGHT THE LEAST SQUARES EQUATIONS.
      WT = DSQRT(SRELPR)
      IF(WTSPR.GT.0.D0) THEN
         WT=WTSPR
      ENDIF
C
C       OPEN(21,FILE='PRN')
C      WRITE(21,*) 'WT =', WT
C      CLOSE(21)
      IF (ANORM.GT.ZERO) WT = WT/ANORM
      IF (CNORM.GT.ZERO) WT = WT*CNORM
      DO 250 I = 1,MOUT
          CALL SSCAL(NCOLS,WT,W(I+MCON,1),MDW)
  250 CONTINUE
      CALL SSCAL(MOUT,WT,W(MCON+1,MCON+NCOLS+1),1)
      LRW = 1
      LIW = 1
C
C     SET THE NEW TRIANGULARIZATION FACTOR.
      X(NCOLS+MCON+IDOPE(1))= ZERO
C
C     SET THE WEIGHT TO USE IN COMPONENTS .GT. MCON,
C     WHEN MAKING LINEAR INDEPENDENCE TEST.
      X(NCOLS+MCON+IDOPE(2))= ONE/WT
      IDOPE(5)=1
      MRODUM=MOUT+MCON
      MNCDUM=NCOLS+MCON
      CALL SBOLS(W,MDW,MRODUM,MNCDUM,BL,BU,IND,IOPT(LOPT),X,
     *           RNORM,MODE,RW(LRW),IW(LIW))
C     END PROCEDURE
C     PROCEDURE(RETURN TO USER PROGRAM UNIT)
  260 IF(MODE.GE.0)MODE=-NERR 
      IGO = 0
  990 RETURN
C     END PROGRAM
      END 
      SUBROUTINE SBOLS(W,MDW,MROWS,NCOLS,BL,BU,IND,IOPT,X,RNORM,MODE,
     *   RW,IW)
      USE MODSBOC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SBOLS
C***DATE WRITTEN   821220   (YYMMDD)
C***REVISION DATE  880722   (YYMMDD)
C***CATEGORY NO.  K1A2A,G2E,G2H1,G2H2
C***KEYWORDS  BOUNDS,CONSTRAINTS,INEQUALITY,LEAST SQUARES,LINEAR
C***AUTHOR  HANSON, R. J., SNLA
C***PURPOSE  Solve the problem
C                 E*X = F (in the least  squares  sense)
C            with bounds on selected X values.
C***DESCRIPTION
C
C     The user must have dimension statements of the form: 
C
C       DIMENSION W(MDW,NCOLS+1), BL(NCOLS), BU(NCOLS),
C      * X(NCOLS+NX), RW(5*NCOLS)
C       INTEGER IND(NCOLS), IOPT(1+NI), IW(2*NCOLS)
C
C     (here NX=number of extra locations required for option 4; NX=0
C     for no options; NX=NCOLS if this option is in use. Here NI=number
C     of extra locations required for options 1-6; NI=0 for no
C     options.) 
C
C   INPUT 
C   ----- 
C
C    --------------------
C    W(MDW,*),MROWS,NCOLS
C    --------------------
C     The array W(*,*) contains the matrix [E:F] on entry. The matrix
C     [E:F] has MROWS rows and NCOLS+1 columns. This data is placed in 
C     the array W(*,*) with E occupying the first NCOLS columns and the
C     right side vector F in column NCOLS+1. The row dimension, MDW, of 
C     the array W(*,*) must satisfy the inequality MDW .ge. MROWS.
C     Other values of MDW are errrors. The values of MROWS and NCOLS
C     must be positive. Other values are errors. There is an exception
C     to this when using option 1 for accumulation of blocks of 
C     equations. In that case MROWS is an OUTPUT variable ONLY, and the
C     matrix data for [E:F] is placed in W(*,*), one block of rows at a
C     time.  MROWS contains the number of rows in the matrix after 
C     triangularizing several blocks of equations. This is an OUTPUT
C     parameter ONLY when option 1 is used. See IOPT(*) CONTENTS
C     for details about option 1.
C
C    ------------------
C    BL(*),BU(*),IND(*)
C    ------------------
C     These arrays contain the information about the bounds that the
C     solution values are to satisfy. The value of IND(J) tells the
C     type of bound and BL(J) and BU(J) give the explicit values for 
C     the respective upper and lower bounds.
C
C    1.    For IND(J)=1, require X(J) .ge. BL(J).
C          (the value of BU(J) is not used.)
C    2.    For IND(J)=2, require X(J) .le. BU(J).
C          (the value of BL(J) is not used.)
C    3.    For IND(J)=3, require X(J) .ge. BL(J) and
C                                X(J) .le. BU(J).
C    4.    For IND(J)=4, no bounds on X(J) are required. 
C          (the values of BL(J) and BU(J) are not used.)
C
C     Values other than 1,2,3 or 4 for IND(J) are errors. In the case
C     IND(J)=3 (upper and lower bounds) the condition BL(J) .gt. BU(J)
C     is an error.
C
C    -------
C    IOPT(*)
C    -------
C     This is the array where the user can specify nonstandard options
C     for SBOLSM( ). Most of the time this feature can be ignored by
C     setting the input value IOPT(1)=99. Occasionally users may have
C     needs that require use of the following subprogram options. For
C     details about how to use the options see below: IOPT(*) CONTENTS. 
C
C     Option Number   Brief Statement of Purpose
C     ------ ------   ----- --------- -- -------
C           1         Return to user for accumulation of blocks
C                     of least squares equations.
C           2         Check lengths of all arrays used in the
C                     subprogram.
C           3         Standard scaling of the data matrix, E. 
C           4         User provides column scaling for matrix E.
C           5         Provide option array to the low-level 
C                     subprogram SBOLSM( ).
C           6         Move the IOPT(*) processing pointer.
C           7         User has called SBOLS() directly.
C          99         No more options to change.
C
C    ---- 
C    X(*) 
C    ---- 
C     This array is used to pass data associated with option 4. Ignore 
C     this parameter if this option is not used. Otherwise see below: 
C     IOPT(*) CONTENTS.
C
C    OUTPUT
C    ------
C
C    ----------
C    X(*),RNORM
C    ----------
C     The array X(*) contains a solution (if MODE .ge.0 or .eq.-22) for
C     the constrained least squares problem. The value RNORM is the
C     minimum residual vector length.
C
C    ---- 
C    MODE 
C    ---- 
C     The sign of MODE determines whether the subprogram has completed
C     normally, or encountered an error condition or abnormal status. A
C     value of MODE .ge. 0 signifies that the subprogram has completed
C     normally. The value of MODE (.GE. 0) is the number of variables
C     in an active status: not at a bound nor at the value ZERO, for
C     the case of free variables. A negative value of MODE will be one
C     of the cases -37,-36,...,-22, or -17,...,-2. Values .lt. -1
C     correspond to an abnormal completion of the subprogram. To
C     understand the abnormal completion codes see below: ERROR
C     MESSAGES for SBOLS( ). AN approximate solution will be returned
C     to the user only when max. iterations is reached, MODE=-22.
C     Values for MODE=-37,...,-22 come from the low-level subprogram
C     SBOLSM(). See the section ERROR MESSAGES for SBOLSM() in the
C     documentation for SBOLSM(). 
C
C    -----------
C    RW(*),IW(*)
C    -----------
C     These are working arrays with 5*NCOLS and 2*NCOLS entries.
C     (normally the user can ignore the contents of these arrays,
C     but they must be dimensioned properly.) 
C
C    IOPT(*) CONTENTS
C    ------- --------
C     The option array allows a user to modify internal variables in 
C     the subprogram without recompiling the source code. A central  
C     goal of the initial software design was to do a good job for most
C     people. Thus the use of options will be restricted to a select
C     group of users. The processing of the option array proceeds as
C     follows: a pointer, here called LP, is initially set to the value
C     1. This value is updated as each option is processed. At the
C     pointer position the option number is extracted and used for
C     locating other information that allows for options to be changed.
C     The portion of the array IOPT(*) that is used for each option is
C     fixed; the user and the subprogram both know how many locations
C     are needed for each option. A great deal of error checking is
C     done by the subprogram on the contents of the option array.
C     Nevertheless it is still possible to give the subprogram optional
C     input that is meaningless. For example option 4 uses the
C     locations X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1) for passing
C     scaling data. The user must manage the allocation of these
C     locations.
C
C   1
C   -
C     This option allows the user to solve problems with a large number
C     of rows compared to the number of variables. The idea is that the
C     subprogram returns to the user (perhaps many times) and receives
C     new least squares equations from the calling program unit.
C     Eventually the user signals "that's all" and then computes the
C     solution with one final call to subprogram SBOLS( ). The value of
C     MROWS is an OUTPUT variable when this option is used. Its value
C     is always in the range 0 .le. MROWS .le. NCOLS+1. It is equal to
C     the number of rows after the triangularization of the entire set
C     of equations. If LP is the processing pointer for IOPT(*), the
C     usage for the sequential processing of blocks of equations is 
C
C        IOPT(LP)=1 
C        Move block of equations to W(*,*) starting at
C        the first row of W(*,*). 
C        IOPT(LP+3)=# of rows in the block; user defined
C
C     The user now calls SBOLS( ) in a loop. The value of IOPT(LP+1)
C     directs the user's action. The value of IOPT(LP+2) points to
C     where the subsequent rows are to be placed in W(*,*).
C
C      .<LOOP
C      . CALL SBOLS()
C      . IF(IOPT(LP+1) .EQ. 1) THEN
C      .    IOPT(LP+3)=# OF ROWS IN THE NEW BLOCK; USER DEFINED
C      .    PLACE NEW BLOCK OF IOPT(LP+3) ROWS IN 
C      .    W(*,*) STARTING AT ROW IOPT(LP+2).
C      .
C      .    IF( THIS IS THE LAST BLOCK OF EQUATIONS ) THEN
C      .       IOPT(LP+1)=2
C      .<------CYCLE LOOP
C      .    ELSE IF (IOPT(LP+1) .EQ. 2) THEN
C      <-------EXIT LOOP SOLUTION COMPUTED IF MODE .GE. 0
C      . ELSE
C      . ERROR CONDITION; SHOULD NOT HAPPEN.
C      .<END LOOP
C
C     Use of this option adds 4 to the required length of IOPT(*).
C
C
C   2
C   -
C     This option is useful for checking the lengths of all arrays used
C     by SBOLS() against their actual requirements for this problem.
C     The idea is simple: the user's program unit passes the declared 
C     dimension information of the arrays. These values are compared
C     against the problem-dependent needs within the subprogram. If any 
C     of the dimensions are too small an error message is printed and a
C     negative value of MODE is returned, -11 to -17. The printed error
C     message tells how long the dimension should be. If LP is the
C     processing pointer for IOPT(*),
C
C        IOPT(LP)=2 
C        IOPT(LP+1)=Row dimension of W(*,*) 
C        IOPT(LP+2)=Col. dimension of W(*,*)
C        IOPT(LP+3)=Dimensions of BL(*),BU(*),IND(*)
C        IOPT(LP+4)=Dimension of X(*)
C        IOPT(LP+5)=Dimension of RW(*)
C        IOPT(LP+6)=Dimension of IW(*)
C        IOPT(LP+7)=Dimension of IOPT(*)
C         .
C        CALL SBOLS()
C
C     Use of this option adds 8 to the required length of IOPT(*).
C
C   3
C   -
C     This option changes the type of scaling for the data matrix E.
C     Nominally each nonzero column of E is scaled so that the
C     magnitude of its largest entry is equal to the value ONE. If LP
C     is the processing pointer for IOPT(*), 
C
C        IOPT(LP)=3 
C        IOPT(LP+1)=1,2 or 3
C            1= Nominal scaling as noted;
C            2= Each nonzero column scaled to have length ONE;
C            3= Identity scaling; scaling effectively suppressed. 
C         .
C        CALL SBOLS()
C
C     Use of this option adds 2 to the required length of IOPT(*).
C
C   4
C   -
C     This option allows the user to provide arbitrary (positive)
C     column scaling for the matrix E. If LP is the processing pointer
C     for IOPT(*),
C
C        IOPT(LP)=4 
C        IOPT(LP+1)=IOFF
C        X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1)
C        = Positive scale factors for cols. of E.
C         .
C        CALL SBOLS()
C
C     Use of this option adds 2 to the required length of IOPT(*) and
C     NCOLS to the required length of X(*).
C
C   5
C   -
C     This option allows the user to provide an option array to the
C     low-level subprogram SBOLSM(). If LP is the processing pointer 
C     for IOPT(*),
C
C        IOPT(LP)=5 
C        IOPT(LP+1)= Position in IOPT(*) where option array 
C                    data for SBOLSM() begins. 
C         .
C        CALL SBOLS()
C
C     Use of this option adds 2 to the required length of IOPT(*).
C
C   6
C   -
C     Move the processing pointer (either forward or backward) to the
C     location IOPT(LP+1). The processing point is moved to entry
C     LP+2 of IOPT(*) if the option is left with -6 in IOPT(LP).  For
C     example to skip over locations 3,...,NCOLS+2 of IOPT(*),
C
C       IOPT(1)=6
C       IOPT(2)=NCOLS+3
C       (IOPT(I), I=3,...,NCOLS+2 are not defined here.)
C       IOPT(NCOLS+3)=99
C       CALL SBOLS()
C
C     CAUTION: Misuse of this option can yield some very hard
C     -to-find bugs.  Use it with care.
C
C   7
C   -
C     If the user is calling SBOLS() directly, use this option. 
C     (This is necessary because SBOCLS() uses SBOLS() as a
C     low-level subprogram.  Due to weighting required within
C     SBOCLS(), the two cases must be known.) For example, 
C
C       IOPT(1)=7
C       IOPT(1)=99
C
C   99
C   --
C     There are no more options to change.
C
C     Only option numbers -99, -7,-6,-5,...,-1, 1,2,...,7, and 99 are
C     permitted. Other values are errors. Options -99,-1,...,-7 mean
C     that the repective options 99,1,...,7 are left at their default
C     values. An example is the option to modify the (rank) tolerance: 
C
C       IOPT(1)=-3 Option is recognized but not changed
C       IOPT(2)=2  Scale nonzero cols. to have length ONE
C       IOPT(3)=99
C
C    ERROR MESSAGES for SBOLS()
C    ----- -------- --- -------
C
C WARNING IN...
C SBOLS(). MDW=(I1) MUST BE POSITIVE.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =         2
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS(). NCOLS=(I1) THE NO. OF VARIABLES MUST BE POSITIVE.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =         3
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS(). FOR J=(I1), IND(J)=(I2) MUST BE 1-4.
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         0
C ERROR NUMBER =         4
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS(). FOR J=(I1), BOUND BL(J)=(R1) IS .GT. BU(J)=(R2). 
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, R1=    0. 
C           IN ABOVE MESSAGE, R2=    ABOVE MESSAGE, I1=         0
C ERROR NUMBER =         6
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS(). ISCALE OPTION=(I1) MUST BE 1-3.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =         7
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS(). OFFSET PAST X(NCOLS) (I1) FOR USER-PROVIDED  COLUMN SCALING
C MUST BE POSITIVE. 
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =         8
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS(). EACH PROVIDED COL. SCALE FACTOR MUST BE POSITIVE.
C COMPONENT (I1) NOW = (R1).
C           IN ABOVE MESSAGE, I1=        ND. .LE. MDW=(I2). 
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         0
C ERROR NUMBER =        10
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS().THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE.THE NUMBER OF ROWS=
C (I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         1
C ERROR NUMBER =        11
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS(). THE COLUMN DIMENSION OF W(,)=(I1) MUST BE .GE. NCOLS+1=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        12
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS().THE DIMENSIONS OF THE ARRAYS BL(),BU(), AND IND()=(I1) MUST BE
C .GE. NCOLS=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         1
C ERROR NUMBER =        13
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS(). THE DIMENSION OF X()=(I1) MUST BE .GE. THE REQD. LENGTH=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        14
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS(). THE DIMENSION OF RW()=(I1) MUST BE .GE. 5*NCOLS=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         3
C ERROR NUMBER =        15
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS() THE DIMENSION OF IW()=(I1) MUST BE .GE. 2*NCOLS=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        16
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C
C WARNING IN...
C SBOLS() THE DIMENSION OF IOPT()=(I1) MUST BE .GE. THE REQD. LEN.=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         1
C ERROR NUMBER =        17
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.) 
C***REFERENCES  HANSON, R. J. LINEAR LEAST SQUARES WITH BOUNDS AND
C                 LINEAR CONSTRAINTS, SIAM J. SCI. STAT. COMPUT., VOL. 7,
C                 NO. 3, JULY, 1986.
C***ROUTINES CALLED  ISAMAX,SBOLSM,SCOPY,SNRM2,SROT,SROTG,XERRWV
C***MODUL    MODSBOC
C***END PROLOGUE  SBOLS
C
C     SOLVE LINEAR LEAST SQUARES SYSTEM WITH BOUNDS ON
C     SELECTED VARIABLES.
C     REVISED 880722-1100
C     REVISED YYMMDD-HHMM
C     TO CHANGE THIS SUBPROGRAM FROM SINGLE TO DOUBLE PRECISION BEGIN 
C     EDITING AT THE CARD 'C++'.
C     CHANGE THIS SUBPROGRAM NAME TO DBOLS AND THE STRINGS
C     /SCOPY/ TO /DCOPY/, /SBOL/ TO /DBOL/,
C     /SNRM2/ TO /DNRM2/, /ISAMAX/ TO /IDAMAX/,
C     /SROTG/ TO /DROTG/, /SROT/ TO /DROT/, /E0/ TO /D0/,
C     /REAL            / TO /DOUBLE PRECISION/.
C ++
      DOUBLE PRECISION   W(MDW,*),BL(*),BU(*),X(*),RW(*)
      DOUBLE PRECISION   SC, SS, ONE, SNRM2, RNORM, ZERO
C
C     THIS VARIABLE SHOULD REMAIN TYPE REAL.
      DOUBLE PRECISION RDUM
      INTEGER IND(*),IOPT(*),IW(*)
      LOGICAL CHECKL
      SAVE IGO,LOCACC,LOPT,ISCALE
      DATA IGO/0/
C***FIRST EXECUTABLE STATEMENT  SBOLS
      NERR = 0
      MODE = 0
      LEVEL = 1
      IF (IGO.EQ.0) THEN
C     DO(CHECK VALIDITY OF INPUT DATA)
C     PROCEDURE(CHECK VALIDITY OF INPUT DATA)
C
C     SEE THAT MDW IS .GT.0. GROSS CHECK ONLY.
          IF (MDW.LE.0) THEN
              NERR = 2
              NCHAR = 35
              CALL XERRWV('SBOLS(). MDW=(I1) MUST BE POSITIVE.',NCHAR,
     .                    NERR,LEVEL,1,MDW,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 190
          END IF
C
C     SEE THAT NUMBER OF UNKNOWNS IS POSITIVE.
          IF (NCOLS.LE.0) THEN
              NERR = 3
              NCHAR = 58
              CALL XERRWV(
     .      'SBOLS(). NCOLS=(I1) THE NO. OF VARIABLES MUST BE POSITIVE.'
     .                    ,NCHAR,NERR,LEVEL,1,NCOLS,IDUM,0,RDUM,RDUM) 
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 190
          END IF
C
C     SEE THAT CONSTRAINT INDICATORS ARE ALL WELL-DEFINED.
          DO 10 J = 1,NCOLS
              IF (IND(J).LT.1 .OR. IND(J).GT.4) THEN
                  NERR = 4
                  NCHAR = 45
                  CALL XERRWV(
     .                   'SBOLS(). FOR J=(I1), IND(J)=(I2) MUST BE 1-4.'
     .                        ,NCHAR,NERR,LEVEL,2,J,IND(J),0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
   10     CONTINUE
C
C     SEE THAT BOUNDS ARE CONSISTENT.
          DO 20 J = 1,NCOLS
              IF (IND(J).EQ.3) THEN
                  IF (BL(J).GT.BU(J)) THEN
                      NERR = 5
                      NCHAR = 57
                      CALL XERRWV(
     .       'SBOLS(). FOR J=(I1), BOUND BL(J)=(R1) IS .GT. BU(J)=(R2).'
     .                            ,NCHAR,NERR,LEVEL,1,J,IDUM,2,BL(J), 
     .                            BU(J))
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 190
                  END IF
              END IF
   20     CONTINUE
C     END PROCEDURE 
C     DO(PROCESS OPTION ARRAY)
C     PROCEDURE(PROCESS OPTION ARRAY)
          ZERO = 0.D0
          ZERU=TINY(ZERO)
          ONE = 1.D0
          CHECKL = .FALSE.
          LENX = NCOLS
          ISCALE = IDOPE(4)
          IGO = 2
          LOPT = 0
          LP = 0
          LDS = 0
   30     CONTINUE
          LP = LP + LDS
          IP = IOPT(LP+1)
          JP = ABS(IP)
C
C     TEST FOR NO MORE OPTIONS.
          IF (IP.EQ.99) THEN
              IF (LOPT.EQ.0) LOPT = LP + 1
              GO TO 50
          ELSE IF (JP.EQ.99) THEN
              LDS = 1
              GO TO 30
          ELSE IF (JP.EQ.1) THEN
              IF (IP.GT.0) THEN
C
C     SET UP DIRECTION FLAG, ROW STACKING POINTER 
C     LOCATION, AND LOCATION FOR NUMBER OF NEW ROWS.
                  LOCACC = LP + 2
C
C                  IOPT(LOCACC-1)=OPTION NUMBER FOR SEQ. ACCUMULATION.
C     CONTENTS..   IOPT(LOCACC  )=USER DIRECTION FLAG, 1 OR 2.
C                  IOPT(LOCACC+1)=ROW STACKING POINTER.
C                  IOPT(LOCACC+2)=NUMBER OF NEW ROWS TO PROCESS.
C     USER ACTION WITH THIS OPTION..
C      (SET UP OPTION DATA FOR SEQ. ACCUMULATION IN IOPT(*).
C      MUST ALSO START PROCESS WITH IOPT(LOCACC)=1.)
C      (MOVE BLOCK OF EQUATIONS INTO W(*,*)  STARTING AT FIRST
C       ROW OF W(*,*).  SET IOPT(LOCACC+2)=NO. OF ROWS IN BLOCK.)
C              LOOP 
C              CALL SBOLS()
C
C                  IF(IOPT(LOCACC) .EQ. 1) THEN
C                      STACK EQUAS., STARTING AT ROW IOPT(LOCACC+1),
C                       INTO W(*,*).
C                       SET IOPT(LOCACC+2)=NO. OF EQUAS.
C                      IF LAST BLOCK OF EQUAS., SET IOPT(LOCACC)=2.
C                  ELSE IF IOPT(LOCACC) .EQ. 2) THEN
C                      (PROCESS IS OVER. EXIT LOOP.)
C                  ELSE
C                      (ERROR CONDITION. SHOULD NOT HAPPEN.)
C                  END IF
C              END LOOP
C              SET IOPT(LOCACC-1)=-OPTION NUMBER FOR SEQ. ACCUMULATION.
C              CALL SBOLS( )
                  IOPT(LOCACC+1) = 1
                  IGO = 1
              END IF
              LDS = 4
              GO TO 30
          ELSE IF (JP.EQ.2) THEN
              IF (IP.GT.0) THEN
C
C     GET ACTUAL LENGTHS OF ARRAYS FOR CHECKING AGAINST NEEDS.
                  LOCDIM = LP + 2
C
C     LMDW.GE.MROWS 
C     LNDW.GE.NCOLS+1
C     LLB .GE.NCOLS 
C     LLX .GE.NCOLS+EXTRA REQD. IN OPTIONS.
C     LLRW.GE.5*NCOLS
C     LLIW.GE.2*NCOLS
C     LIOP.GE. AMOUNT REQD. FOR IOPTION ARRAY.
                  LMDW = IOPT(LOCDIM)
                  LNDW = IOPT(LOCDIM+1) 
                  LLB = IOPT(LOCDIM+2)
                  LLX = IOPT(LOCDIM+3)
                  LLRW = IOPT(LOCDIM+4) 
                  LLIW = IOPT(LOCDIM+5) 
                  LIOPT = IOPT(LOCDIM+6)
                  CHECKL = .TRUE.
              END IF
              LDS = 8
              GO TO 30
C
C     OPTION TO MODIFY THE COLUMN SCALING.
          ELSE IF (JP.EQ.3) THEN
              IF (IP.GT.0) THEN
                  ISCALE = IOPT(LP+2)
C
C     SEE THAT ISCALE IS 1 THRU 3.
                  IF (ISCALE.LT.1 .OR. ISCALE.GT.3) THEN
                      NERR = 7
                      NCHAR = 40
                      CALL XERRWV(
     .                        'SBOLS(). ISCALE OPTION=(I1) MUST BE 1-3.'
     .                            ,NCHAR,NERR,LEVEL,1,ISCALE,IDUM,0,
     .                            RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 190
                  END IF
              END IF
              LDS = 2
C     CYCLE FOREVER 
              GO TO 30
C
C     IN THIS OPTION THE USER HAS PROVIDED SCALING.  THE
C     SCALE FACTORS FOR THE COLUMNS BEGIN IN X(NCOLS+IOPT(LP+2)).
          ELSE IF (JP.EQ.4) THEN
              IF (IP.GT.0) THEN
                  ISCALE = 4
                  IF (IOPT(LP+2).LE.0) THEN
                      NERR = 8
                      NCHAR = 85
                      CALL XERRWV(
     .'SBOLS(). OFFSET PAST X(NCOLS) (I1) FOR USER-PROVIDED COLUMN SCALI
     .NG MUST BE POSITIVE.',NCHAR,NERR,LEVEL,1,IOPT(LP+2),IDUM,0,RDUM,
     .                            RDUM) 
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 190
                  END IF
                  CALL SCOPY(NCOLS,X(NCOLS+IOPT(LP+2)),1,RW,1)
                  LENX = LENX + NCOLS
                  DO 40 J = 1,NCOLS
                      IF (RW(J).LE.ZERO) THEN
                          NERR = 9
                          NCHAR = 85
                          CALL XERRWV(
     .'SBOLS(). EACH PROVIDED COL. SCALE FACTOR MUST BE POSITIVE. COMPON
     .ENT (I1) NOW = (R1).',NCHAR,NERR,LEVEL,1,J,IDUM,1,RW(J),RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                          GO TO 190
                      END IF
   40             CONTINUE
              END IF
              LDS = 2
C     CYCLE FOREVER 
              GO TO 30
C
C     IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO SBOLSM().
          ELSE IF (JP.EQ.5) THEN
              IF (IP.GT.0) THEN
                  LOPT = IOPT(LP+2)
              END IF
              LDS = 2
C     CYCLE FOREVER 
              GO TO 30
C
C     THIS OPTION USES THE NEXT LOC OF IOPT(*) AS THE PLACE TO
C     MOVE AT THE NEXT STEP OF PROCESSESING.
          ELSE IF (JP.EQ.6) THEN
              IF (IP.GT.0) THEN
                  LP = IOPT(LP+2)-1
                  LDS = 0
              ELSE
                  LDS = 2
              END IF
C     CYCLE FOREVER 
              GO TO 30
C
C     THIS OPTION PROVIDES INFORMATION ABOUT WHO CALLED SBOLS.
C     IT WAS EITHER SBOCLS() OR THE USER.
          ELSE IF (JP.EQ.7) THEN
              LDS=1 
              IF (IP.GT.0) THEN
                IDOPE(5)=0
                ISCALE=1
              END IF
C     CYCLE FOREVER 
              GO TO 30
C
C     NO VALID OPTION NUMBER WAS NOTED. THIS IS AN ERROR CONDITION.
          ELSE
              NERR = 6
              NCHAR = 47
              CALL XERRWV(
     .                 'SBOLS(). THE OPTION NUMBER=(I1) IS NOT DEFINED.'
     .                    ,NCHAR,NERR,LEVEL,1,JP,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 190
          END IF
   50     CONTINUE
C     END PROCEDURE 
          IF (CHECKL) THEN
C     DO(CHECK LENGTHS OF ARRAYS)
C     PROCEDURE(CHECK LENGTHS OF ARRAYS)
C
C     THIS FEATURE ALLOWS THE USER TO MAKE SURE THAT THE
C     ARRAYS ARE LONG ENOUGH FOR THE INTENDED PROBLEM SIZE AND USE.
              IF (LMDW.LT.MROWS) THEN
                  NERR = 11
                  NCHAR = 76
                  CALL XERRWV(
     .'SBOLS(). THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE.THE NUMBER OF
     . ROWS=(I2).',NCHAR,NERR,LEVEL,2,LMDW,MROWS,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LNDW.LT.NCOLS+1) THEN 
                  NERR = 12
                  NCHAR = 69
                  CALL XERRWV(
     .'SBOLS(). THE COLUMN DIMENSION OF W(,)=(I1) MUST BE .GE. NCOLS+1=(
     .I2).',NCHAR,NERR,LEVEL,2,LNDW,NCOLS+1,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LLB.LT.NCOLS) THEN
                  NERR = 13
                  NCHAR = 88
                  CALL XERRWV(
     .'SBOLS(). THE DIMENSIONS OF THE ARRAYS BL(),BU(), AND IND()=(I1) M
     .UST BE .GE. NCOLS=(I2).',NCHAR,NERR,LEVEL,2,LLB,NCOLS,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LLX.LT.LENX) THEN
                  NERR = 14
                  NCHAR = 70
                  CALL XERRWV(
     .'SBOLS(). THE DIMENSION OF X()=(I1) MUST BE .GE. THE REQD. LENGTH=
     .(I2).',NCHAR,NERR,LEVEL,2,LLX,LENX,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LLRW.LT.5*NCOLS) THEN 
                  NERR = 15
                  NCHAR = 62
                  CALL XERRWV(
     .  'SBOLS(). THE DIMENSION OF RW()=(I1) MUST BE .GE. 5*NCOLS=(I2).'
     .                        ,NCHAR,NERR,LEVEL,2,LLRW,5*NCOLS,0,RDUM,
     .                        RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LLIW.LT.2*NCOLS) THEN 
                  NERR = 16
                  NCHAR = 61
                  CALL XERRWV(
     .   'SBOLS() THE DIMENSION OF IW()=(I1) MUST BE .GE. 2*NCOLS=(I2).'
     .                        ,NCHAR,NERR,LEVEL,2,LLIW,2*NCOLS,0,RDUM,
     .                        RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LIOPT.LT.LP+1) THEN
                  NERR = 17
                  NCHAR = 71
                  CALL XERRWV(
     .'SBOLS(). THE DIMENSION OF IOPT()=(I1) MUST BE .GE. THE REQD. LEN.
     .=(I2).',NCHAR,NERR,LEVEL,2,LIOPT,LP+1,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
C     END PROCEDURE 
          END IF
      END IF
      GO TO (60,90),IGO
      GO TO 180
C
C     GO BACK TO THE USER FOR ACCUMULATION OF LEAST SQUARES 
C     EQUATIONS AND DIRECTIONS TO QUIT PROCESSING.
C     CASE 1
   60 CONTINUE
C     DO(ACCUMULATE LEAST SQUARES EQUATIONS)
C     PROCEDURE(ACCUMULATE LEAST SQUARES EQUATIONS)
      MROWS = IOPT(LOCACC+1) - 1
      INROWS = IOPT(LOCACC+2) 
      MNEW = MROWS + INROWS
      IF (MNEW.LT.0 .OR. MNEW.GT.MDW) THEN
          NERR = 10 
          NCHAR = 61
          CALL XERRWV(
     .   'SBOLS(). NO. OF ROWS=(I1) MUST BE .GE. 0 .AND. .LE. MDW=(I2).'
     .                ,NCHAR,NERR,LEVEL,2,MNEW,MDW,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
          GO TO 190 
      END IF
      DO 80 J = 1,MIN(NCOLS+1,MNEW)
          DO 70 I = MNEW,MAX(MROWS,J) + 1,-1
              IBIG = ISAMAX(I-J,W(J,J),1) + J - 1 
C
C     PIVOT FOR INCREASED STABILITY.
              CALL SROTG(W(IBIG,J),W(I,J),SC,SS)
              CALL SROT(NCOLS+1-J,W(IBIG,J+1),MDW,W(I,J+1),MDW,SC,SS) 
              W(I,J) = ZERO
   70     CONTINUE
   80 CONTINUE
      MROWS = MIN(NCOLS+1,MNEW)
      IOPT(LOCACC+1) = MROWS + 1
      IGO = IOPT(LOCACC)
C     END PROCEDURE 
      IF (IGO.EQ.2) THEN
          IGO = 0
      END IF
      GO TO 180
C     CASE 2
   90 CONTINUE
C     DO(INITIALIZE VARIABLES AND DATA VALUES)
C     PROCEDURE(INITIALIZE VARIABLES AND DATA VALUES)
      DO 150 J = 1,NCOLS
          GO TO (100,110,120,130),ISCALE
          GO TO 140 
  100     CONTINUE
C     CASE 1
C
C     THIS IS THE NOMINAL SCALING. EACH NONZERO
C     COL. HAS MAX. NORM EQUAL TO ONE.
          IBIG = ISAMAX(MROWS,W(1,J),1) 
          RW(J) = ABS(W(IBIG,J))
          IF (ABS(RW(J)).LE.ZERU) THEN
              RW(J) = ONE
          ELSE
              RW(J) = ONE/RW(J)
          END IF
          GO TO 140 
  110     CONTINUE
C     CASE 2
C
C     THIS CHOICE OF SCALING MAKES EACH NONZERO COLUMN
C     HAVE EUCLIDEAN LENGTH EQUAL TO ONE.
          RW(J) = SNRM2(MROWS,W(1,J),1) 
          IF (ABS(RW(J)).LE.ZERU) THEN
              RW(J) = ONE
          ELSE
              RW(J) = ONE/RW(J)
          END IF
          GO TO 140 
  120     CONTINUE
C     CASE 3
C
C     THIS CASE EFFECTIVELY SUPPRESSES SCALING BY SETTING
C     THE SCALING MATRIX TO THE IDENTITY MATRIX.
          RW(1) = ONE
          CALL SCOPY(NCOLS,RW,0,RW,1)
          GO TO 160 
  130     CONTINUE
C     CASE 4
          GO TO 160 
  140     CONTINUE
  150 CONTINUE
  160 CONTINUE
C     END PROCEDURE 
C     DO(SOLVE BOUNDED LEAST SQUARES PROBLEM)
C     PROCEDURE(SOLVE BOUNDED LEAST SQUARES PROBLEM)
C
C     INITIALIZE IBASIS(*), J=1,NCOLS, AND IBB(*), J=1,NCOLS,
C     TO =J,AND =1, FOR USE IN SBOLSM( ).
      DO 170 J = 1,NCOLS
          IW(J) = J 
          IW(J+NCOLS) = 1
          RW(3*NCOLS+J) = BL(J)
          RW(4*NCOLS+J) = BU(J)
  170 CONTINUE
      CALL SBOLSM(W,MDW,MROWS,NCOLS,RW(3*NCOLS+1),RW(4*NCOLS+1),IND,
     .            IOPT(LOPT),X,RNORM,MODE,RW(NCOLS+1),RW(2*NCOLS+1),RW,
     .            IW,IW(NCOLS+1))
C     END PROCEDURE
      IGO = 0
  180 CONTINUE
      RETURN
C     PROCEDURE(RETURN TO USER PROGRAM UNIT)
  190 IF(MODE.GE.0)MODE = -NERR
      IGO = 0
      RETURN
C     END PROCEDURE 
      END 
      SUBROUTINE SBOLSM(W,MDW,MINPUT,NCOLS,BL,BU,IND,IOPT,X,RNORM,MODE,
     .                  RW,WW,SCL,IBASIS,IBB)
      USE MODSBOC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SBOLSM
C***REFER TO  SBOCLS,SBOLS
C***ROUTINES CALLED  IVOUT,R1MACH,SAXPY,SCOPY,SDOT,SMOUT,SNRM2,SROT,
C                    SROTG,SSWAP,SVOUT,XERRWV
C***MODUL    MODSBOC
C***DESCRIPTION
C
C          Solve E*X = F (least squares sense) with bounds on
C            selected X values. 
C     The user must have dimension statements of the form: 
C
C       DIMENSION W(MDW,NCOLS+1), BL(NCOLS), BU(NCOLS),
C      * X(NCOLS+NX), RW(NCOLS), WW(NCOLS), SCL(NCOLS)
C       INTEGER IND(NCOLS), IOPT(1+NI), IBASIS(NCOLS), IBB(NCOLS)
C
C     (here NX=number of extra locations required for options 1,...,7;
C     NX=0 for no options; here NI=number of extra locations possibly
C     required for options 1-7; NI=0 for no options; NI=14 if all the
C     options are simultaneously in use.) 
C
C    INPUT
C    -----
C
C    --------------------
C    W(MDW,*),MROWS,NCOLS
C    --------------------
C     The array w(*,*) contains the matrix [E:F] on entry. The matrix 
C     [E:F] has MROWS rows and NCOLS+1 columns. This data is placed in 
C     the array W(*,*) with E occupying the first NCOLS columns and the
C     right side vector F in column NCOLS+1. The row dimension, MDW, of 
C     the array W(*,*) must satisfy the inequality MDW .ge. MROWS.
C     Other values of MDW are errors. The values of MROWS and NCOLS
C     must be positive. Other values are errors.
C
C    ------------------
C    BL(*),BU(*),IND(*)
C    ------------------
C     These arrays contain the information about the bounds that the
C     solution values are to satisfy. The value of IND(J) tells the
C     type of bound and BL(J) and BU(J) give the explicit values for 
C     the respective upper and lower bounds.
C
C    1.    For IND(J)=1, require X(J) .ge. BL(J).
C    2.    For IND(J)=2, require X(J) .le. BU(J).
C    3.    For IND(J)=3, require X(J) .ge. BL(J) and
C                                X(J) .le. BU(J).
C    4.    For IND(J)=4, no bounds on X(J) are required. 
C     The values of BL(*),BL(*) are modified by the subprogram. Values
C     other than 1,2,3 or 4 for IND(J) are errors. In the case IND(J)=3
C     (upper and lower bounds) the condition BL(J) .gt. BU(J) is an
C     error.
C
C    -------
C    IOPT(*)
C    -------
C     This is the array where the user can specify nonstandard options
C     for SBOLSM( ). Most of the time this feature can be ignored by
C     setting the input value IOPT(1)=99. Occasionally users may have
C     needs that require use of the following subprogram options. For
C     details about how to use the options see below: IOPT(*) CONTENTS. 
C
C     Option Number   Brief Statement of Purpose
C     ----- ------   ----- --------- -- -------
C           1         Move the IOPT(*) processing pointer.
C           2         Change rank determination tolerance. 
C           3         Change blow-up factor that determines the
C                     size of variables being dropped from active 
C                     status.
C           4         Reset the maximum number of iterations to use
C                     in solving the problem.
C           5         The data matrix is triangularized before the
C                     problem is solved whenever (NCOLS/MROWS) .lt.
C                     FAC. Change the value of FAC.
C           6         Redefine the weighting matrix used for
C                     linear independence checking.
C           7         Debug output is desired.
C          99         No more options to change.
C
C    ---- 
C    X(*) 
C    ---- 
C     This array is used to pass data associated with options 1,2,3 and 
C     5. Ignore this input parameter if none of these options are used.
C     Otherwise see below: IOPT(*) CONTENTS.
C
C    ----------------
C    IBASIS(*),IBB(*)
C    ----------------
C     These arrays must be initialized by the user. The values
C         IBASIS(J)=J, J=1,...,NCOLS
C         IBB(J)   =1, J=1,...,NCOLS
C     are appropriate except when using nonstandard features. 
C
C    ------
C    SCL(*)
C    ------
C     This is the array of scaling factors to use on the columns of the
C     matrix E. These values must be defined by the user. To suppress
C     any column scaling set SCL(J)=1.0, J=1,...,NCOLS.
C
C    OUTPUT
C    ------
C
C    ----------
C    X(*),RNORM
C    ----------
C     The array X(*) contains a solution (if MODE .ge.0 or .eq.-22) for
C     the constrained least squares problem. The value RNORM is the
C     minimum residual vector length.
C
C    ---- 
C    MODE 
C    ---- 
C     The sign of mode determines whether the subprogram has completed
C     normally, or encountered an error condition or abnormal status.
C     A value of MODE .ge. 0 signifies that the subprogram has completed 
C     normally. The value of MODE (.ge. 0) is the number of variables
C     in an active status: not at a bound nor at the value ZERO, for
C     the case of free variables. A negative value of MODE will be one
C     of the 20 cases -40,-39,...,-22, or -1. Values .lt. -1 correspond 
C     to an abnormal completion of the subprogram. To understand the 
C     abnormal completion codes see below: ERROR MESSAGES for SBOLSM( )
C     An approximate solution will be returned to the user only when 
C     max. iterations is reached, MODE=-22.
C
C    -----------
C    RW(*),WW(*)
C    -----------
C     These are working arrays each with NCOLS entries. The array RW(*)
C     contains the working (scaled, nonactive) solution values. The
C     array WW(*) contains the working (scaled, active) gradient vector 
C     values. 
C
C    ----------------
C    IBASIS(*),IBB(*)
C    ----------------
C     These arrays contain information about the status of the solution
C     when MODE .ge. 0. The indices IBASIS(K), K=1,...,MODE, show the
C     nonactive variables; indices IBASIS(K), K=MODE+1,..., NCOLS are
C     the active variables. The value (IBB(J)-1) is the number of times
C     variable J was reflected from its upper bound. (normally the user
C     can ignore these parameters.) 
C
C    IOPT(*) CONTENTS
C    ------- --------
C     The option array allows a user to modify internal variables in 
C     the subprogram without recompiling the source code. A central  
C     goal of the initial software design was to do a good job for most
C     people. Thus the use of options will be restricted to a select
C     group of users. The processing of the option array proceeds as
C     follows: a pointer, here called LP, is initially set to the value
C     1. The value is updated as the options are processed.  At the
C     pointer position the option number is extracted and used for
C     locating other information that allows for options to be changed.
C     The portion of the array IOPT(*) that is used for each option is
C     fixed; the user and the subprogram both know how many locations
C     are needed for each option. A great deal of error checking is
C     done by the subprogram on the contents of the option array.
C     Nevertheless it is still possible to give the subprogram optional
C     input that is meaningless. For example some of the options use
C     the location X(NCOLS+IOFF) for passing data. The user must manage
C     the allocation of these locations when more than one piece of  
C     option data is being passed to the subprogram.
C
C   1
C   -
C     Move the processing pointer (either forward or backward) to the
C     location IOPT(LP+1). The processing pointer is moved to location
C     LP+2 of IOPT(*) in case IOPT(LP)=-1.  For example to skip over
C     locations 3,...,NCOLS+2 of IOPT(*),
C
C       IOPT(1)=1
C       IOPT(2)=NCOLS+3
C       (IOPT(I), I=3,...,NCOLS+2 are not defined here.)
C       IOPT(NCOLS+3)=99
C       CALL SBOLSM( )
C
C     CAUTION: Misuse of this option can yield some very hard
C     -to-find bugs.  Use it with care.
C
C   2
C   -
C     The algorithm that solves the bounded least squares problem
C     iteratively drops columns from the active set. This has the
C     effect of joining a new column vector to the QR factorization of
C     the rectangular matrix consisting of the partially triangularized 
C     nonactive columns. After triangularizing this matrix a test is
C     made on the size of the pivot element. The column vector is
C     rejected as dependent if the magnitude of the pivot element is
C     .le. TOL* magnitude of the column in components strictly above
C     the pivot element. Nominally the value of this (rank) tolerance 
C     is TOL = SRELPR, where SRELPR is relative machine
C     precision. To change only the value of TOL, for example,
C
C       X(NCOLS+1)=TOL
C       IOPT(1)=2
C       IOPT(2)=1
C       IOPT(3)=99
C       CALL SBOLSM()
C
C     Generally, if LP is the processing pointer for IOPT(*),
C
C       X(NCOLS+IOFF)=TOL
C       IOPT(LP)=2
C       IOPT(LP+1)=IOFF
C        .
C       CALL SBOLSM()
C
C     The required length of IOPT(*) is increased by 2 if option 2 is
C     used; The required length of X(*) is increased by 1. A value of
C     IOFF .le. 0 is an error. A value of TOL .le. SRELPR gives a
C     warning message; it is not considered an error. 
C     Here SRELPR is the relative machine precision.
C
C   3
C   -
C     A solution component is left active (not used) if, roughly 
C     speaking, it seems too large. Mathematically the new component is
C     left active if the magnitude is .ge.((vector norm of F)/(matrix
C     norm of E))/BLOWUP. Nominally the factor BLOWUP = SQRT(SRELPR)
C     where SRELPR is the relative machine precision. To change only
C     the value of BLOWUP, for example, 
C
C       X(NCOLS+2)=BLOWUP
C       IOPT(1)=3
C       IOPT(2)=2
C       IOPT(3)=99
C       CALL SBOLSM()
C
C     Generally, if LP is the processing pointer for IOPT(*),
C
C       X(NCOLS+IOFF)=BLOWUP
C       IOPT(LP)=3
C       IOPT(LP+1)=IOFF
C        .
C       CALL SBOLSM()
C
C     The required length of IOPT(*) is increased by 2 if option 3 is
C     used; the required length of X(*) is increased by 1. A value of
C     IOFF .le. 0 is an error. A value of BLOWUP .le. 0.0 is an error. 
C
C   4
C   -
C     Normally the algorithm for solving the bounded least squares
C     problem requires between NCOLS/3 and NCOLS drop-add steps to
C     converge. (this remark is based on examining a small number of
C     test cases.) The amount of arithmetic for such problems is
C     typically about twice that required for linear least squares if
C     there are no bounds and if plane rotations are used in the
C     solution method. Convergence of the algorithm, while
C     mathematically certain, can be much slower than indicated. To
C     avoid this potential but unlikely event ITMAX drop-add steps are
C     permitted. Nominally ITMAX=5*(MAX(MROWS,NCOLS)). To change the
C     value of ITMAX, for example,
C
C       IOPT(1)=4
C       IOPT(2)=ITMAX
C       IOPT(3)=99
C       CALL SBOLSM()
C
C     Generally, if LP is the processing pointer for IOPT(*),
C
C       IOPT(LP)=4
C       IOPT(LP+1)=ITMAX
C        .
C       CALL SBOLSM()
C
C     The value of ITMAX must be .gt. 0. Other values are errors. Use
C     of this option increases the required length of IOPT(*) by 2. 
C
C   5
C   -
C     For purposes of increased efficiency the MROWS by NCOLS+1 data
C     matrix [E:F] is triangularized as a first step whenever MROWS 
C     satisfies FAC*MROWS .gt. NCOLS. Nominally FAC=0.  To change the
C     value of FAC,
C
C       X(NCOLS+3)=FAC
C       IOPT(1)=5
C       IOPT(2)=3
C       IOPT(3)=99
C       CALL SBOLSM()
C
C     Generally, if LP is the processing pointer for IOPT(*),
C
C       X(NCOLS+IOFF)=FAC
C       IOPT(LP)=5
C       IOPT(LP+1)=IOFF
C        .
C       CALL SBOLSM()
C
C     The value of FAC must be nonnegative. Other values are errors.
C     Using the value FAC=0.0 suppresses the initial triangularization step. 
C     Use of this option increases the required length of IOPT(*) by 2; 
C     The required length of of X(*) is increased by 1.
C
C   6
C   -
C     The norm used in testing the magnitudes of the pivot element
C     compared to the mass of the column above the pivot line can be
C     changed. The type of change that this option allows is to weight
C     the components with an index larger than MVAL by the parameter
C     WT. Normally MVAL=0 and WT=1. To change both the values MVAL and
C     WT, where LP is the processing pointer for IOPT(*),
C
C       X(NCOLS+IOFF)=WT
C       IOPT(LP)=6
C       IOPT(LP+1)=IOFF
C       IOPT(LP+2)=MVAL
C
C     Use of this option increases the required length of IOPT(*) by 3. 
C     The length of X(*) is increased by 1. Values of MVAL must be
C     nonnegative and not greater than MROWS. Other values are errors.
C     The value of WT must be positive. Any other value is an error. If
C     either error condition is present a message will be printed.
C
C   7
C   -
C     Debug output, showing the detailed add-drop steps for the 
C     constrained least squares problem, is desired. This option is
C     intended to be used to locate suspected bugs.  To print,
C
C       IOPT(LP)=7
C
C   99
C   --
C     There are no more options to change.
C
C     The values for options are 1,...,7,99, and are the only ones
C     permitted. Other values are errors. Options -99,-1,...,-7 mean
C     that the repective options 99,1,...,7 are left at their default
C     values. An example is the option to modify the (rank) tolerance: 
C
C       X(NCOLS+1)=TOL
C       IOPT(1)=-2
C       IOPT(2)=1
C       IOPT(3)=99
C
C    Error Messages for SBOLSM( )
C    ----- -------- --- ---------
C    WARNING IN...
C    SBOLSM(). MORE THAN (I1)=ITMAX ITERATIONS SOLVING BOUNDED LEAST
C    SQUARES PROBLEM.
C              IN ABOVE MESSAGE, I1=         3
C    ERROR NUMBER =        22 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM. THE OPTION NUMBER=(I1) IS NOT DEFINED.
C              IN ABOVE MESSAGE, I1=         0
C    ERROR NUMBER =        23 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE OFFSET=(I1) BEYOND POSTION NCOLS=(I2)
C    MUST BE POSITIVE FOR OPTION NUMBER 2.
C              IN ABOVE MESSAGE, I1=         0
C              IN ABOVE MESSAGE, I2=         1
C    ERROR NUMBER =        24 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE TOLERANCE FOR RANK DETERMINATION=(R1)
C    IS LESS THAN MACHINE PRECISION=(R2).
C              IN ABOVE MESSAGE, R1=    0.
C              IN ABOVE MESSAGE, R2=     .7105427358E-14
C    ERROR NUMBER =        25 
C
C    WARNING IN...
C    SBOLSM(). THE OFFSET=(I1) BEYOND POSITION NCOLS=(I2) MUST
C     BE POSTIVE FOR OPTION NUMBER 3.
C              IN ABOVE MESSAGE, I1=         0
C              IN ABOVE MESSAGE, I2=         1
C    ERROR NUMBER =        26 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE RECIPROCAL OF THE BLOW-UP FACTOR FOR REJECTING
C    VARIABLES MUST BE POSITIVE. NOW=(R1).
C              IN ABOVE MESSAGE, R1=    0.
C    ERROR NUMBER =        27 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE MAXIMUM NUMBER OF ITERATIONS=(I1) MUST BE POSITIVE.
C              IN ABOVE MESSAGE, I1=         0
C    ERROR NUMBER =        28 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE OFFSET=(I1) BEYOND POSITION NCOLS=(I2) MUST BE
C    POSTIVE FOR OPTION NUMBER 5.
C              IN ABOVE MESSAGE, I1=         0
C              IN ABOVE MESSAGE, I2=         1
C    ERROR NUMBER =        29 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE FACTOR (NCOLS/MROWS) WHERE PRETRIANGULARIZING IS
C    PERFORMED MUST BE NONNEGATIVE. NOW=(R1).
C              IN ABOVE MESSAGE, R1=    -.2500000000E+00
C    ERROR NUMBER =        30 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE NUMBER OF ROWS=(I1) MUST BE POSITIVE.
C              IN ABOVE MESSAGE, I1=         0
C    ERROR NUMBER =        31 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE NUMBER OF COLS.=(I1) MUST BE POSTIVE.
C              IN ABOVE MESSAGE, I1=         0
C    ERROR NUMBER =        32 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE. THE
C    NUMBER OF ROWS =(I2).
C              IN ABOVE MESSAGE, I1=         0
C              IN ABOVE MESSAGE, I2=         1
C    ERROR NUMBER =        33 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). FOR J=(I1) THE CONSTRAINT INDICATOR MUST BE 1-4.
C              IN ABOVE MESSAGE, I1=         1
C              IN ABOVE MESSAGE, I2=         0
C    ERROR NUMBER =        34 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). FOR J=(I1) THE LOWER BOUND=(R1) IS .GT. THE UPPER
C     BOUND=(R2).
C              IN ABOVE MESSAGE, I1=         1
C              IN ABOVE MESSAGE, R1=    0.
C              IN ABOVE MESSAGE, R2=    -.1000000000E+01
C    ERROR NUMBER =        35 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE INPUT ORDER OF COLUMNS=(I1) IS NOT BETWEEN 1
C    AND NCOLS=(I2).
C              IN ABOVE MESSAGE, I1=         0
C              IN ABOVE MESSAGE, I2=         1
C    ERROR NUMBER =        36 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE BOUND POLARITY FLAG IN COMPONENT J=(I1) MUST
C    BE POSITIVE.  NOW=(I2).
C              IN ABOVE MESSAGE, I1=         1
C              IN ABOVE MESSAGE, I2=         0
C    ERROR NUMBER =        37 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE ROW SEPARATOR TO APPLY WEIGHTING (I1) MUST LIE
C    BETWEEN 0 AND MROWS (I2). WEIGHT (R1) MUST BE POSITIVE.
C              IN ABOVE MESSAGE, I1=        -1
C              IN ABOVE MESSAGE, I2=         2
C              IN ABOVE MESSAGE, R1=    0.
C    ERROR NUMBER =        38 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE OFFSET (I1) BEYOND POSITION NCOLS=(I2) MUST BE
C    POSITIVE FOR OPTION NUMBER 7.
C              IN ABOVE MESSAGE, I1=        -1
C              IN ABOVE MESSAGE, I2=         2
C    ERROR NUMBER =        39 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    SBOLSM(). THE COLUMN PIVOTING THRESHOLD FACTOR MUST BE 
C    POSITIVE. NOW=(R1).
C              IN ABOVE MESSAGE, R1=    0.
C    ERROR NUMBER =        40 
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C***END PROLOGUE  SBOLSM
C
C     PURPOSE
C     -------
C     THIS IS THE MAIN SUBPROGRAM THAT SOLVES THE BOUNDED
C     LEAST SQUARES PROBLEM.  THE PROBLEM SOLVED HERE IS:   
C
C     SOLVE E*X =  F  (LEAST SQUARES SENSE)
C     WITH BOUNDS ON SELECTED X VALUES. 
C
C     REVISED 880722-1100 
C     REVISED YYMMDD-HHMM
C
C     TO CHANGE THIS SUBPROGRAM FROM SINGLE TO DOUBLE PRECISION BEGIN 
C     EDITING AT THE CARD 'C++'.
C     CHANGE THE SUBPROGRAM NAME TO DBOLSM AND THE STRINGS
C     /SAXPY/ TO /DAXPY/, /SCOPY/ TO /DCOPY/,
C     /SDOT/ TO /DDOT/, /SNRM2/ TO /DNRM2/,
C     /SROT/ TO /DROT/, /SROTG/ TO /DROTG/, /R1MACH/ TO /D1MACH/,
C     /SVOUT/ TO /DVOUT/, /SMOUT/ TO /DMOUT/,
C     /SSWAP/ TO /DSWAP/, /E0/ TO /D0/, 
C     /REAL            / TO /DOUBLE PRECISION/.
C ++
C
C     THIS VARIABLE REMAINS TYPE REAL.
      DOUBLE PRECISION RDUM
      DOUBLE PRECISION W(MDW,*),BL(*),BU(*)
      DOUBLE PRECISION X(*),RW(*),WW(*),SCL(*)
      DOUBLE PRECISION ALPHA,BETA,BOU,COLABV,COLBLO 
      DOUBLE PRECISION CL1,CL2,CL3,ONE,BIG
      DOUBLE PRECISION FAC,RNORM,SC,SS,T,TOLIND,WT
      DOUBLE PRECISION TWO,T1,T2,WLARGE,WLA,WLB,XNEW
      DOUBLE PRECISION ZERO,SDOT,SNRM2
      DOUBLE PRECISION R1MACH,TOLSZE
      INTEGER IBASIS(*),IBB(*),IND(*),IOPT(*)
      LOGICAL FOUND,CONSTR,CNZ
      INEXT(IDUM) = MIN(IDUM+1,MROWS)
C***FIRST EXECUTABLE STATEMENT  SBOLSM
      LEVEL = 1
C
C    VERIFY THAT THE PROBLEM DIMENSIONS ARE DEFINED PROPERLY.
      IF (MINPUT.LE.0) THEN
          NERR = 31 
          NCHAR = 51
          CALL XERRWV(
     .             'SBOLSM(). THE NUMBER OF ROWS=(I1) MUST BE POSITIVE.'
     .                ,NCHAR,NERR,LEVEL,1,MINPUT,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
          GO TO 600 
*
      END IF
*
      IF (NCOLS.LE.0) THEN
          NERR = 32 
          NCHAR = 51
          CALL XERRWV(
     .             'SBOLSM(). THE NUMBER OF COLS.=(I1) MUST BE POSTIVE.'
     .                ,NCHAR,NERR,LEVEL,1,NCOLS,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
          GO TO 600 
*
      END IF
*
      IF (MDW.LT.MINPUT) THEN 
          NERR = 33 
          NCHAR = 78
          CALL XERRWV(
     .'SBOLSM(). THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE. THE NUMBER
     .OF ROWS=(I2).',NCHAR,NERR,LEVEL,2,MDW,MROWS,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
          GO TO 600 
*
      END IF
C
C     VERIFY THAT BOUND INFORMATION IS CORRECT.
      DO 10 J = 1,NCOLS
         IF (IND(J).LT.1 .OR. IND(J).GT.4) THEN
             NERR = 34
             NCHAR = 58
             CALL XERRWV(
     .      'SBOLSM(). FOR J=(I1) THE CONSTRAINT INDICATOR MUST BE 1-4.'
     .                   ,NCHAR,NERR,LEVEL,2,J,IND(J),0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
             GO TO 600
*
         END IF
*
   10 CONTINUE
      DO 20 J = 1,NCOLS
         IF (IND(J).EQ.3) THEN
             IF (BU(J).LT.BL(J)) THEN
                 NERR = 35
                 NCHAR = 71
                 CALL XERRWV( 
     .'SBOLSM(). FOR J=(I1) THE LOWER BOUND=(R1) IS .GT. THE UPPER BOUND
     .=(R2).',NCHAR,NERR,LEVEL,1,J,IDUM,2,BL(J),BU(J))
C     DO(RETURN TO USER PROGRAM UNIT)
                 GO TO 600
*
             END IF 
*
         END IF
*
   20 CONTINUE
C
C     CHECK THAT PERMUTATION AND POLARITY ARRAYS HAVE BEEN SET.
      DO 30 J = 1,NCOLS
         IF (IBASIS(J).LT.1 .OR. IBASIS(J).GT.NCOLS) THEN
             NERR = 36
             NCHAR = 74
             CALL XERRWV(
     .'SBOLSM(). THE INPUT ORDER OF COLUMNS=(I1) IS NOT BETWEEN 1 AND NC
     .OLS=(I2).',NCHAR,NERR,LEVEL,2,IBASIS(J),NCOLS,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
             GO TO 600
*
         END IF
*
         IF (IBB(J).LE.0) THEN
             NERR = 37
             NCHAR = 81
             CALL XERRWV(
     .'SBOLSM(). THE BOUND POLARITY FLAG IN COMPONENT J=(I1) MUST BE POS
     .ITIVE. NOW=(I2).',NCHAR,NERR,LEVEL,2,J,IBB(J),0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
             GO TO 600
*
         END IF
*
   30 CONTINUE
C     DO(PROCESS OPTION ARRAY)
      GO TO 570
*
   40 CONTINUE
C     DO(INITIALIZE VARIABLES AND DATA VALUES)
      GO TO 460
*
   50 CONTINUE
      IF (IPRINT.GT.0) THEN
C         CALL SMOUT(MROWS,NCOLS+1,MDW,W,'('' PRETRI. INPUT MATRIX'')',
C    .               -4)
C         CALL SVOUT(NCOLS,BL,'('' LOWER BOUNDS'')',-4)
C         CALL SVOUT(NCOLS,BU,'('' UPPER BOUNDS'')',-4)
      END IF
*
   60 CONTINUE
      ITER = ITER + 1
      IF (ITER.LE.ITMAX) GO TO 80
      NERR = 22
C     NCHAR = 80
C     CALL XERRWV(
C    .'SBOLSM(). MORE THAN (I1)=ITMAX ITERATIONS SOLVING BOUNDED LEAST S
C    .QUARES PROBLEM.',NCHAR,NERR,LEVEL,1,ITMAX,IDUM,0,RDUM,RDUM)
C     DO(RESCALE AND TRANSLATE VARIABLES)
C     IGOPR = 1
      GO TO 130
*
   70 CONTINUE
C     DO(RETURN TO USER PROGRAM UNIT)
      GO TO 600
*
   80 CONTINUE
C     DO(FIND A VARIABLE TO BECOME NON-ACTIVE)
      GO TO 180
*
   90 CONTINUE
      IF (FOUND) GO TO 110
C     DO(RESCALE AND TRANSLATE VARIABLES)
      IGOPR = 2
      GO TO 130
*
  100 CONTINUE
      MODE = NSETB
      RETURN
*
  110 CONTINUE
C     DO(MAKE MOVE AND UPDATE FACTORIZATION)
      GO TO 280
*
  120 CONTINUE
      GO TO 60
C     PROCEDURE(RESCALE AND TRANSLATE VARIABLES)
  130 CONTINUE
      CALL SCOPY(NSETB,X,1,RW,1)
      X(1) = ZERO
      CALL SCOPY(NCOLS,X,0,X,1)
      DO 140 J = 1,NSETB
         JCOL = ABS(IBASIS(J))
         X(JCOL) = RW(J)*ABS(SCL(JCOL)) 
  140 CONTINUE
      DO 150 J = 1,NCOLS
         IF (MOD(IBB(J),2).EQ.0) X(J) = BU(J) - X(J)
  150 CONTINUE
      DO 160 J = 1,NCOLS
         JCOL = IBASIS(J)
         IF (JCOL.LT.0) X(-JCOL) = BL(-JCOL) + X(-JCOL)
  160 CONTINUE
      DO 170 J = 1,NCOLS
         IF (SCL(J).LT.ZERO) X(J) = -X(J)
  170 CONTINUE
      CALL SSCAL(MROWS-MVAL,WT,W(INEXT(MVAL),NCOLS+1),1)
      RNORM = SNRM2(MROWS-MAX(NSETB,MVAL),
     .        W(INEXT(MAX(NSETB,MVAL)),NCOLS+1),1)
C     END PROCEDURE
      GO TO (70,100),IGOPR
C     PROCEDURE(FIND A VARIABLE TO BECOME NON-ACTIVE)
  180 CONTINUE
C
C     COMPUTE (NEGATIVE) OF GRADIENT VECTOR, W=
C     (TRANSPOSE OF E)*(F-E*X).
      WW(1) = ZERO
      CALL SCOPY(NCOLS,WW,0,WW,1)
      DO 190 J = NSETB + 1,NCOLS
         JCOL = ABS(IBASIS(J))
         WW(J) = SDOT(MROWS-NSETB,W(INEXT(NSETB),J),1,
     .           W(INEXT(NSETB),NCOLS+1),1)*ABS(SCL(JCOL))
  190 CONTINUE
      IF (IPRINT.GT.0) THEN
C         CALL SVOUT(NCOLS,WW,'('' GRADIENT VALUES'')',-4)
C         CALL IVOUT(NCOLS,IBASIS,'('' INTERNAL VARIABLE ORDER'')',-4)
C         CALL IVOUT(NCOLS,IBB,'('' BOUND POLARITY'')',-4)
      END IF
*
  200 CONTINUE
C
C     IF ACTIVE SET = NUMBER OF TOTAL ROWS, QUIT. 
      IF (NSETB.EQ.MROWS) THEN
          FOUND = .FALSE.
C     EXIT PROCEDURE
          GO TO 90
*
      END IF
C
C     CHOOSE AN EXTREMAL COMPONENT OF GRADIENT VECTOR
C     FOR A CANDIDATE TO BECOME NON-ACTIVE.
      WLARGE = -BIG 
      JBIG = 0
      CNZ = .FALSE. 
      DO 210 J = NSETB + 1,NCOLS
         T = WW(J)
C     SKIP LOOKING AT COMPONENTS FLAGGED AS NON-CANDIDATES. 
         IF (ABS(T-BIG).LE.ZERU) GO TO 210
         ITEMP = IBASIS(J)
         JCOL = ABS(ITEMP)
         IF (NSETB.LT.MVAL) THEN
             CL1 = SNRM2(NSETB,W(1,J),1)
             CL2 = SNRM2(MVAL-NSETB,W(INEXT(NSETB),J),1)
             COLABV = CL1
             COLBLO = CL2
*
         ELSE
             CL1 = SNRM2(MVAL,W(1,J),1) 
             CL2 = DABS(WT)*SNRM2(NSETB-MVAL,W(INEXT(MVAL),J),1)
             CL3 = DABS(WT)*SNRM2(MROWS-NSETB,W(INEXT(NSETB),J),1)
             CALL SROTG(CL1,CL2,SC,SS)
             COLABV = DABS(CL1)
             COLBLO = CL3
         END IF
*
         IF (ITEMP.LT.0) THEN 
             IF (MOD(IBB(JCOL),2).EQ.0) T = -T
C     SKIP LOOKING AT COMPONENTS THAT WOULD NOT DECREASE OBJECTIVE.
             IF (T.LT.ZERO) GO TO 210
         END IF
C     THIS IS A COLUMN PIVOTING STEP THAT MAXIMIZES THE RATIO OF
C     COLUMN MASS ON AND BELOW THE PIVOT LINE RELATIVE TO THAT
C     STRICTLY ABOVE THE PIVOT LINE.
         IF (ABS(COLABV).LE.ZERU .AND. .NOT. CNZ) THEN 
             T = COLBLO*DABS(SCL(JCOL))
             IF (WLARGE.LT.T) THEN
                 WLARGE = T
                 JBIG = J
             END IF 
*
         ELSE
             IF ( .NOT. CNZ) THEN
                 WLA = ZERO
                 WLB = ZERO
                 CNZ = .TRUE. 
             END IF 
*
           IF(SQRT(COLBLO)*SQRT(WLA) .GE. SQRT(COLABV)*SQRT(WLB)) THEN
                WLB=COLBLO
                WLA=COLABV
                JBIG=J
           END IF
         END IF
*
  210 CONTINUE
      IF (JBIG.EQ.0) THEN
          FOUND = .FALSE.
          IF (IPRINT.GT.0) THEN
C             CALL IVOUT(0,I,'('' FOUND NO VARIABLE TO ENTER'')',-4)
          END IF
C     EXIT PROCEDURE
          GO TO 90
*
      END IF
C
C     SEE IF THE INCOMING COL. IS SUFFICIENTLY INDEPENDENT. 
C     THIS TEST IS MADE BEFORE AN ELIMINATION IS PERFORMED. 
      IF (IPRINT.GT.0) THEN

C         CALL IVOUT(1,JBIG,'('' TRY TO BRING IN THIS COL.'')',-4)
      END IF
*
      IF (CNZ) THEN 
      IF(WLB.LE.WLA*TOLIND) THEN
          FOUND = .FALSE.

          IF (IPRINT.GT.0) THEN
C             CALL IVOUT(0,I,'('' VARIABLE IS DEPENDENT, NOT USED.'')',
C    .                   -4)
          END IF
*
          WW(JBIG) = BIG
          GO TO 200
*
      END IF
      END IF
C
C     SWAP MATRIX COLS. NSETB+1 AND JBIG, PLUS POINTER INFO., AND
C     GRADIENT VALUES.
      NSETB = NSETB + 1
      IF (NSETB.NE.JBIG) THEN 
          CALL SSWAP(MROWS,W(1,NSETB),1,W(1,JBIG),1)
          CALL SSWAP(1,WW(NSETB),1,WW(JBIG),1)
          ITEMP = IBASIS(NSETB)
          IBASIS(NSETB) = IBASIS(JBIG)
          IBASIS(JBIG) = ITEMP
      END IF
C
C     ELIMINATE ENTRIES BELOW THE PIVOT LINE IN COL. NSETB. 
      IF (MROWS.GT.NSETB) THEN
          DO 220 I = MROWS,NSETB + 1,-1 
             IF (I.EQ.MVAL+1) GO TO 220 
             CALL SROTG(W(I-1,NSETB),W(I,NSETB),SC,SS)
             W(I,NSETB) = ZERO
             CALL SROT(NCOLS-NSETB+1,W(I-1,NSETB+1),MDW,W(I,NSETB+1), 
     .                 MDW,SC,SS)
  220     CONTINUE
          IF ((MVAL.GE.NSETB) .AND. (MVAL.LT.MROWS)) THEN
              T = W(NSETB,NSETB)
              IF (ABS(T).GT.ZERU) THEN
                  T = WT*W(MVAL+1,NSETB)/T
*
              ELSE
                  T = BIG
              END IF
*
              IF (TOLIND*ABS(T).GT.ONE) THEN
                  CALL SSWAP(NCOLS-NSETB+2,W(NSETB,NSETB),MDW,
     .                       W(MVAL+1,NSETB),MDW) 
                  CALL SSCAL(NCOLS-NSETB+2,WT,W(NSETB,NSETB),MDW)
                  CALL SSCAL(NCOLS-NSETB+2,ONE/WT,W(MVAL+1,NSETB),MDW)
              END IF
*
              CALL SROTG(W(NSETB,NSETB),W(MVAL+1,NSETB),SC,SS)
              W(MVAL+1,NSETB) = ZERO
              CALL SROT(NCOLS-NSETB+1,W(NSETB,NSETB+1),MDW, 
     .                  W(MVAL+1,NSETB+1),MDW,SC,SS)
          END IF
*
      END IF
*
      IF (ABS(W(NSETB,NSETB)).LE.ZERU) THEN
          WW(NSETB) = BIG
          NSETB = NSETB - 1
          IF (IPRINT.GT.0) THEN
C             CALL IVOUT(0,I,'('' PIVOT IS ZERO, NOT USED.'')',-4)
          END IF
*
          GO TO 200 
*
      END IF
C
C     CHECK THAT NEW VARIABLE IS MOVING IN THE RIGHT DIRECTION.
      ITEMP = IBASIS(NSETB)
      JCOL = ABS(ITEMP)
      XNEW = (W(NSETB,NCOLS+1)/W(NSETB,NSETB))/ABS(SCL(JCOL))
C CONT: DO BLOCK
C QUIT: DO BLOCK
      IF (ITEMP.LT.0) THEN
C     IF(WW(NSETB).GE.ZERO.AND.XNEW.LE.ZERO) EXIT(QUIT)
C     IF(WW(NSETB).LE.ZERO.AND.XNEW.GE.ZERO) EXIT(QUIT)
          IF (WW(NSETB).GE.ZERO .AND. XNEW.LE.ZERO) GO TO 230
          IF (WW(NSETB).LE.ZERO .AND. XNEW.GE.ZERO) GO TO 230
      END IF
C     EXIT(CONT)
      GO TO 240
C     END BLOCK
  230 CONTINUE
      WW(NSETB) = BIG
      NSETB = NSETB - 1
      IF (IPRINT.GT.0) THEN
C         CALL IVOUT(0,I,'('' VARIABLE HAS BAD DIRECTION, NOT USED.'')',
C    .               -4)
      END IF
*
      GO TO 200
C     END BLOCK
  240 CONTINUE
      FOUND = .TRUE.
C     EXIT PROCEDURE
      GO TO 250
*
  250 CONTINUE
C     END PROCEDURE 
      GO TO 90
C     PROCEDURE(SOLVE THE TRIANGULAR SYSTEM)
  260 CONTINUE
      CALL SCOPY(NSETB,W(1,NCOLS+1),1,RW,1)
      DO 270 J = NSETB,1,-1
         RW(J) = RW(J)/W(J,J) 
         JCOL = ABS(IBASIS(J))
         T = RW(J)
         IF (MOD(IBB(JCOL),2).EQ.0) RW(J) = -RW(J)
         CALL SAXPY(J-1,-T,W(1,J),1,RW,1)
         RW(J) = RW(J)/ABS(SCL(JCOL))
  270 CONTINUE
      IF (IPRINT.GT.0) THEN
C         CALL SVOUT(NSETB,RW,'('' SOLN. VALUES'')',-4)
C         CALL IVOUT(NSETB,IBASIS,'('' COLS. USED'')',-4)
      END IF
C     END PROCEDURE 
      GO TO (290,430),LGOPR
C     PROCEDURE(MAKE MOVE AND UPDATE FACTORIZATION)
  280 CONTINUE
C     DO(SOLVE THE TRIANGULAR SYSTEM)
      LGOPR = 1
      GO TO 260
*
  290 CONTINUE
C
C     SEE IF THE UNCONSTRAINED SOL. (OBTAINED BY SOLVING THE
C     TRIANGULAR SYSTEM) SATISFIES THE PROBLEM BOUNDS.
      ALPHA = TWO
      BETA = TWO
      X(NSETB) = ZERO
      DO 300 J = 1,NSETB
         ITEMP = IBASIS(J)
         JCOL = ABS(ITEMP)
         T1 = TWO
         T2 = TWO
         IF (ITEMP.LT.0) THEN 
             BOU = ZERO
*
         ELSE
             BOU = BL(JCOL)
         END IF
*
         IF (ABS(BOU+BIG).GE.ZERU) BOU = BOU/ABS(SCL(JCOL))
         IF (RW(J).LE.BOU) T1 = (X(J)-BOU)/ (X(J)-RW(J))
         BOU = BU(JCOL)
         IF (ABS(BOU-BIG).GE.ZERU) BOU = BOU/ABS(SCL(JCOL))
         IF (RW(J).GE.BOU) T2 = (BOU-X(J))/ (RW(J)-X(J))
C
C     IF NOT, THEN COMPUTE A STEP LENGTH SO THAT THE
C     VARIABLES REMAIN FEASIBLE.
         IF (T1.LT.ALPHA) THEN
             ALPHA = T1
             JDROP1 = J
         END IF
*
         IF (T2.LT.BETA) THEN 
             BETA = T2
             JDROP2 = J
         END IF
*
  300 CONTINUE
      CONSTR = ALPHA .LT. TWO .OR. BETA .LT. TWO
      IF (CONSTR) GO TO 310
C
C     ACCEPT THE CANDIDATE BECAUSE IT SATISFIES THE STATED BOUNDS
C     ON THE VARIABLES.
      CALL SCOPY(NSETB,RW,1,X,1)
      GO TO 120
*
  310 CONTINUE
C
C     TAKE A STEP THAT IS AS LARGE AS POSSIBLE WITH ALL
C     VARIABLES REMAINING FEASIBLE.
      DO 320 J = 1,NSETB
         X(J) = X(J) + MIN(ALPHA,BETA)* (RW(J)-X(J))
  320 CONTINUE
      IF (ALPHA.LE.BETA) THEN
          JDROP2 = 0
*
      ELSE
          JDROP1 = 0
      END IF
*
  330 IF (JDROP1+JDROP2.GT.0 .AND. NSETB.GT.0) GO TO 340
      GO TO 450
*
  340 JDROP = JDROP1 + JDROP2 
      ITEMP = IBASIS(JDROP)
      JCOL = ABS(ITEMP)
      IF (JDROP2.GT.0) THEN
C
C     VARIABLE IS AT AN UPPER BOUND.  SUBTRACT MULTIPLE OF THIS COL.
C     FROM RIGHT HAND SIDE.
          T = BU(JCOL)
          IF (ITEMP.GT.0) THEN
              BU(JCOL) = T - BL(JCOL)
              BL(JCOL) = -T
              ITEMP = -ITEMP
              SCL(JCOL) = -SCL(JCOL)
              DO 350 I = 1,JDROP
                 W(I,JDROP) = -W(I,JDROP)
  350         CONTINUE
*
          ELSE
              IBB(JCOL) = IBB(JCOL) + 1 
              IF (MOD(IBB(JCOL),2).EQ.0) T = -T
          END IF
C     VARIABLE IS AT A LOWER BOUND.
      ELSE
          IF (ITEMP.LT.ZERO) THEN
              T = ZERO
*
          ELSE
              T = -BL(JCOL)
              BU(JCOL) = BU(JCOL) + T
              ITEMP = -ITEMP
          END IF
*
      END IF
*
      CALL SAXPY(JDROP,T,W(1,JDROP),1,W(1,NCOLS+1),1)
C
C     MOVE CERTAIN COLS. LEFT TO ACHIEVE UPPER HESSENBERG FORM.
      CALL SCOPY(JDROP,W(1,JDROP),1,RW,1)
      DO 360 J = JDROP + 1,NSETB
         IBASIS(J-1) = IBASIS(J)
         X(J-1) = X(J)
         CALL SCOPY(J,W(1,J),1,W(1,J-1),1)
  360 CONTINUE
      IBASIS(NSETB) = ITEMP
      W(1,NSETB) = ZERO
      CALL SCOPY(MROWS-JDROP,W(1,NSETB),0,W(JDROP+1,NSETB),1)
      CALL SCOPY(JDROP,RW,1,W(1,NSETB),1)
C
C     TRANSFORM THE MATRIX FROM UPPER HESSENBERG FORM TO
C     UPPER TRIANGULAR FORM.
      NSETB = NSETB - 1
C SMLL:   
C     *DO BLOCK
C NRML:   
C     *DO BLOCK
      DO 380 I = JDROP,NSETB
C
C     LOOK FOR SMALL PIVOTS AND AVOID MIXING WEIGHTED AND
C     NONWEIGHTED ROWS.
         IF (I.EQ.MVAL) THEN
             T = ZERO
             DO 370 J = I,NSETB
                JCOL = ABS(IBASIS(J))
                T1 = ABS(W(I,J)*SCL(JCOL))
                IF (T1.GT.T) THEN
                    JBIG = J
                    T = T1
                END IF
*
  370        CONTINUE
C     EXIT(NRML)
             GO TO 390
*
         END IF
*
         CALL SROTG(W(I,I),W(I+1,I),SC,SS)
         W(I+1,I) = ZERO
         CALL SROT(NCOLS-I+1,W(I,I+1),MDW,W(I+1,I+1),MDW,SC,SS)
  380 CONTINUE
C     EXIT (SMLL)
      GO TO 420
C     END BLOCK
  390 CONTINUE
C
C     THE TRIANGULARIZATION IS COMPLETED BY GIVING UP
C     THE HESSENBERG FORM AND TRIANGULARIZING A RECTANGULAR MATRIX.
      CALL SSWAP(MROWS,W(1,I),1,W(1,JBIG),1)
      CALL SSWAP(1,WW(I),1,WW(JBIG),1)
      CALL SSWAP(1,X(I),1,X(JBIG),1)
      ITEMP = IBASIS(I)
      IBASIS(I) = IBASIS(JBIG)
      IBASIS(JBIG) = ITEMP
      JBIG = I
      DO 410 J = JBIG,NSETB
         DO 400 I = J + 1,MROWS
            CALL SROTG(W(J,J),W(I,J),SC,SS)
            W(I,J) = ZERO
            CALL SROT(NCOLS-J+1,W(J,J+1),MDW,W(I,J+1),MDW,SC,SS)
  400    CONTINUE
  410 CONTINUE
C     END BLOCK
  420 CONTINUE
C
C     SEE IF THE REMAINING COEFFICIENTS ARE FEASIBLE.  THEY SHOULD
C     BE BECAUSE OF THE WAY MIN(ALPHA,BETA) WAS CHOSEN.  ANY THAT ARE 
C     NOT FEASIBLE WILL BE SET TO THEIR BOUNDS AND
C     APPROPRIATELY TRANSLATED.
      JDROP1 = 0
      JDROP2 = 0
C     DO(SOLVE THE TRIANGULAR SYSTEM)
      LGOPR = 2
      GO TO 260
*
  430 CONTINUE
      CALL SCOPY(NSETB,RW,1,X,1)
      DO 440 J = 1,NSETB
         ITEMP = IBASIS(J)
         JCOL = ABS(ITEMP)
         IF (ITEMP.LT.0) THEN 
             BOU = ZERO
*
         ELSE
             BOU = BL(JCOL)
         END IF
*
         IF (ABS(BOU+BIG).GE.ZERU) BOU = BOU/ABS(SCL(JCOL))
         IF (X(J).LE.BOU) THEN
             JDROP1 = J
             GO TO 330
*
         END IF
*
         BOU = BU(JCOL)
         IF (ABS(BOU-BIG).GE.ZERU) BOU = BOU/ABS(SCL(JCOL))
         IF (X(J).GE.BOU) THEN
             JDROP2 = J
             GO TO 330
*
         END IF
*
  440 CONTINUE
      GO TO 330
*
  450 CONTINUE
C     END PROCEDURE 
      GO TO 120
C     PROCEDURE(INITIALIZE VARIABLES AND DATA VALUES)
  460 CONTINUE
C
C     PRETRIANGULARIZE RECTANGULAR ARRAYS OF CERTAIN SIZES
C     FOR INCREASED EFFICIENCY.
      IF (FAC*MINPUT.GT.NCOLS) THEN
          DO 480 J = 1,NCOLS + 1
             DO 470 I = MINPUT,J + MVAL + 1,-1
                CALL SROTG(W(I-1,J),W(I,J),SC,SS) 
                W(I,J) = ZERO 
                CALL SROT(NCOLS-J+1,W(I-1,J+1),MDW,W(I,J+1),MDW,SC,SS)
  470        CONTINUE
  480     CONTINUE
          MROWS = NCOLS + MVAL + 1
*
      ELSE
          MROWS = MINPUT
      END IF
C
C      SET THE X(*) ARRAY TO ZERO SO ALL COMPONENTS ARE DEFINED.
      X(1) = ZERO
      CALL SCOPY(NCOLS,X,0,X,1)
C
C     THE ARRAYS IBASIS(*), IBB(*) ARE INITIALIZED BY THE CALLING
C     PROGRAM UNIT. 
C     THE COL. SCALING IS DEFINED IN THE CALLING PROGRAM UNIT.
C    'BIG' IS PLUS INFINITY ON THIS MACHINE.
      BIG = R1MACH(2)
      DO 540 J = 1,NCOLS
         ICASE = IND(J)
C     DO CASE(ICASE,4)
         GO TO (490,500,510,520),ICASE
*
         GO TO 530
C     CASE 1
  490    BU(J) = BIG
         GO TO 530
C     CASE 2
  500    BL(J) = -BIG
         GO TO 530
C     CASE 3
  510    GO TO 530
C     CASE 4
  520    BL(J) = -BIG
         BU(J) = BIG
C     END CASE
  530    CONTINUE
  540 CONTINUE
      DO 560 J = 1,NCOLS
         IF ((BL(J).LE.ZERO.AND.ZERO.LE.BU(J).AND.ABS(BU(J)).LT.
     .       ABS(BL(J))) .OR. BU(J).LT.ZERO) THEN 
             T = BU(J)
             BU(J) = -BL(J)
             BL(J) = -T
             SCL(J) = -SCL(J) 
             DO 550 I = 1,MROWS
                W(I,J) = -W(I,J)
  550        CONTINUE
         END IF
C
C     INDICES IN SET T(=TIGHT) ARE DENOTED BY NEGATIVE VALUES
C     OF IBASIS(*). 
         IF (BL(J).GE.ZERO) THEN
             IBASIS(J) = -IBASIS(J)
             T = -BL(J)
             BU(J) = BU(J) + T
             CALL SAXPY(MROWS,T,W(1,J),1,W(1,NCOLS+1),1)
         END IF
*
  560 CONTINUE
      NSETB = 0
      ITER = 0
C     END PROCEDURE 
      GO TO 50
C     PROCEDURE(PROCESS OPTION ARRAY)
  570 CONTINUE
      IF (IDOPE(5).EQ.1) THEN
          FAC = X(NCOLS+IDOPE(1))
          WT = X(NCOLS+IDOPE(2))
          MVAL = IDOPE(3)
*
      ELSE
          FAC = FAC0
          WT = WT2
          MVAL = MVA0
      END IF
*
      ZERO = 0.D0
      ZERU = TINY(ZERO)
      ONE = 1.D0
      TWO = 2.D0
      TOLIND = R1MACH(4)
      TOLSZE = SQRT(R1MACH(4))
      ITMAX =MAX(ITMAS, 5*MAX(MINPUT,NCOLS))
      IPRINT = 0
C
C     CHANGES TO SOME PARAMETERS CAN OCCUR THROUGH THE OPTION
C     ARRAY, IOPT(*).  PROCESS THIS ARRAY LOOKING CAREFULLY 
C     FOR INPUT DATA ERRORS.
      LP = 0
      LDS = 0
  580 CONTINUE
      LP = LP + LDS 
C
C     TEST FOR NO MORE OPTIONS.
      IP = IOPT(LP+1)
      JP = ABS(IP)
      IF (IP.EQ.99) THEN
          GO TO 590 
*
      ELSE IF (JP.EQ.99) THEN 
          LDS = 1
          GO TO 580 
*
      ELSE IF (JP.EQ.1) THEN
C
C     MOVE THE IOPT(*) PROCESSING POINTER.
          IF (IP.GT.0) THEN
              LP = IOPT(LP+2) - 1
              LDS = 0
*
          ELSE
              LDS = 2
          END IF
*
          GO TO 580 
*
      ELSE IF (JP.EQ.2) THEN
C
C     CHANGE TOLERANCE FOR RANK DETERMINATION.
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              IF (IOFF.LE.0) THEN
                  NERR = 24
                  NCHAR = 89
                  CALL XERRWV(
     .'SBOLSM(). THE OFFSET=(I1) BEYOND POSTION NCOLS=(I2) MUST BE POSIT
     .IVE FOR OPTION NUMBER 2.',NCHAR,NERR,LEVEL,2,IOFF,NCOLS,0,RDUM, 
     .                        RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
              TOLIND = X(NCOLS+IOFF)
              IF (TOLIND.LT.R1MACH(4)) THEN
                  NERR = 25
                  NLEVEL = 0
                  NCHAR = 88
                  CALL XERRWV(
     .'SBOLSM(). THE TOLERANCE FOR RANK DETERMINATION=(R1) IS LESS THAN
     .MACHINE PRECISION=(R2).',NCHAR,NERR,NLEVEL,0,IDUM,IDUM,2,TOLIND,
     .                        R1MACH(4))
              END IF
*
          END IF
*
          LDS = 2
          GO TO 580 
*
      ELSE IF (JP.EQ.3) THEN
C
C     CHANGE BLOWUP FACTOR FOR ALLOWING VARIABLES TO BECOME 
C     INACTIVE.
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              IF (IOFF.LE.0) THEN
                  NERR = 26
                  NCHAR = 89
                  CALL XERRWV(
     .'SBOLSM(). THE OFFSET=(I1) BEYOND POSITION NCOLS=(I2) MUST BE POST
     .IVE FOR OPTION NUMBER 3.',NCHAR,NERR,LEVEL,2,IOFF,NCOLS,0,RDUM, 
     .                        RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
              TOLSZE = X(NCOLS+IOFF)
              IF (TOLSZE.LE.ZERO) THEN
                  NERR = 27
                  CALL XERRWV(
     .'SBOLSM(). THE RECIPROCAL OF THE BLOW-UP FACTOR FOR REJECTING VARI
     .ABLES MUST BE POSITIVE. NOW=(R1).',NCHAR,NERR,LEVEL,0,IDUM,IDUM,1,
     .                        TOLSZE,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
          END IF
*
          LDS = 2
          GO TO 580 
*
      ELSE IF (JP.EQ.4) THEN
C
C     CHANGE THE MAX. NO. OF ITERATIONS ALLOWED.
          IF (IP.GT.0) THEN
              ITMAX = IOPT(LP+2)
              IF (ITMAX.LE.0) THEN
                  NERR = 28
                  NCHAR = 65
                  CALL XERRWV(
     .'SBOLSM(). THE MAXIMUM NUMBER OF ITERATIONS=(I1) MUST BE POSITIVE.
     .',NCHAR,NERR,LEVEL,1,ITMAX,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
          END IF
*
          LDS = 2
          GO TO 580 
*
      ELSE IF (JP.EQ.5) THEN
C
C     CHANGE THE FACTOR FOR PRETRIANGULARIZING THE DATA MATRIX.
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              IF (IOFF.LE.0) THEN
                  NERR = 29
                  NCHAR = 89
                  CALL XERRWV(
     .'SBOLSM(). THE OFFSET=(I1) BEYOND POSITION NCOLS=(I2) MUST BE POST
     .IVE FOR OPTION NUMBER 5.',NCHAR,NERR,LEVEL,2,IOFF,NCOLS,0,RDUM, 
     .                        RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
              FAC = X(NCOLS+IOFF)
              IF (FAC.LT.ZERO) THEN
                  NERR = 30
                  NLEVEL = 0
                  NCHAR = 104 
                  CALL XERRWV(
     .'SBOLSM(). THE FACTOR (NCOLS/MROWS) WHERE PRE-TRIANGULARIZING IS P
     .ERFORMED MUST BE NONNEGATIVE. NOW=(R1).',NCHAR,NERR,NLEVEL,0,IDUM,
     .                        IDUM,1,FAC,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
          END IF
*
          LDS = 2
          GO TO 580 
*
      ELSE IF (JP.EQ.6) THEN
C
C     CHANGE THE WEIGHTING FACTOR (FROM ONE) TO APPLY TO COMPONENTS
C     NUMBERED .GT. MVAL (INITIALLY SET TO 1.)  THIS TRICK IS NEEDED
C     FOR APPLICATIONS OF THIS SUBPROGRAM TO THE HEAVILY WEIGHTED
C     LEAST SQUARES PROBLEM THAT COME FROM EQUALITY CONSTRAINTS.
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              MVAL = IOPT(LP+3)
              WT = X(NCOLS+IOFF)
          END IF
*
          IF (MVAL.LT.0 .OR. MVAL.GT.MINPUT .OR. WT.LE.ZERO) THEN
              NERR = 38
              NLEVEL = 0
              NCHAR = 116
              CALL XERRWV(
     .'SBOLSM(). THE ROW SEPARATOR TO APPLY WEIGHTING (I1) MUST LIE BETW
     .EEN 0 AND MROWS (I2). WEIGHT (R1) MUST BE POSITIVE.',NCHAR,NERR,
     .                    NLEVEL,2,MVAL,MINPUT,1,WT,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 600
*
          END IF
*
          LDS = 3
          GO TO 580 
C
C     TURN ON DEBUG OUTPUT.
      ELSE IF (JP.EQ.7) THEN
          IF (IP.GT.0) IPRINT = 1
          LDS = 1
          GO TO 580 
*
      ELSE
          NERR = 23 
          NCHAR = 46
          CALL XERRWV('SBOLSM. THE OPTION NUMBER=(I1) IS NOT DEFINED.', 
     .                NCHAR,NERR,LEVEL,1,IP,IDUM,0,RDUM,RDUM) 
C     DO(RETURN TO USER PROGRAM UNIT) 
          GO TO 600 
* 
      END IF
* 
  590 CONTINUE
C     END PROCEDURE 
      GO TO 40
C     PROCEDURE(RETURN TO USER PROGRAM UNIT)
  600 CONTINUE
      MODE = -NERR
      RETURN
C     END PROCEDURE 
C     END PROGRAM 
      END
C++++++++++++++++ SINGLE PRECISION VERSION 2SP (AUG 1982) ++++++++++++++
C  FUNCTION DIFF. 
C     BASED ON C.L.LAWSON AND R.J.HANSON, 
C     'SOLVING LEAST SQUARES PROBLEMS', PRENTICE-HALL, 1974 
      DOUBLE PRECISION FUNCTION DIFF(X,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     DOUBLE PRECISION FUNCTION DIFF(X,Y)!DP
C     DOUBLE PRECISION X, Y!DP
      DIFF=X-Y
      RETURN
      END 
      INTEGER FUNCTION ISAMAX(N,SX,INCX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  ISAMAX    
C***DATE WRITTEN   791001   (YYMMDD)    
C***REVISION DATE  851111   (YYMMDD)    
C***CATEGORY NO.  D1A2   
C***KEYWORDS  BLAS,LINEAR ALGEBRA,MAXIMUM COMPONENT,VECTOR  
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)    
C           KROGH, F. T., (JPL)    
C***PURPOSE  Find the smallest index of that component af a vector    
C            having the maximum magnitude.   
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C    
C     --Input--
C        N  number of elements in input vector(s) 
C       SX  single precision vector with N elements    
C     INCX  storage spacing between elements of SX
C    
C     --Output--    
C   ISAMAX  smallest index (zero if N .LE. 0)
C    
C     Find smallest index of maximum magnitude of single precision SX.
C     ISAMAX =  first I, I = 1 to N, to minimize  ABS(SX(1-INCX+I*INCX)    
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  ISAMAX 
C    
      DOUBLE PRECISION SX(*),SMAX,XMAG    
C***FIRST EXECUTABLE STATEMENT  ISAMAX  
      ISAMAX = 0    
      IF(N.LE.0) RETURN  
      ISAMAX = 1    
      IF(N.LE.1)RETURN   
      IF(INCX.EQ.1)GOTO 20    
C    
C        CODE FOR INCREMENTS NOT EQUAL TO 1. 
C    
      SMAX = ABS(SX(1))
      NS = N*INCX   
      II = 1   
          DO 10 I=1,NS,INCX
          XMAG = ABS(SX(I))
          IF(XMAG.LE.SMAX) GO TO 5 
          ISAMAX = II    
          SMAX = XMAG    
    5     II = II + 1    
   10     CONTINUE  
      RETURN   
C    
C        CODE FOR INCREMENTS EQUAL TO 1.
C    
   20 SMAX = ABS(SX(1))  
      DO 30 I = 2,N
         XMAG = ABS(SX(I))
         IF(XMAG.LE.SMAX) GO TO 30 
         ISAMAX = I 
         SMAX = XMAG
   30 CONTINUE 
      RETURN   
      END 
      DOUBLE PRECISION FUNCTION SASUM(N,SX,INCX)    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SASUM
C***DATE WRITTEN   791001   (YYMMDD)    
C***REVISION DATE  820801   (YYMMDD)    
C***CATEGORY NO.  D1A3A  
C***KEYWORDS  ADD,BLAS,LINEAR ALGEBRA,MAGNITUDE,SUM,VECTOR  
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)    
C           KROGH, F. T., (JPL)    
C***PURPOSE  Sum of magnitudes of s.p vector components
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C    
C     --Input--
C        N  number of elements in input vector(S) 
C       SX  single precision vector with N elements    
C     INCX  storage spacing between elements of SX
C    
C     --Output--    
C    SASUM  single precision result (zero if N .LE. 0) 
C    
C     Returns sum of magnitudes of single precision SX.
C     SASUM = sum from 0 to N-1 of  ABS(SX(1+I*INCX))  
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  SASUM  
C    
      DOUBLE PRECISION SX(*)
C***FIRST EXECUTABLE STATEMENT  SASUM   
      SASUM = 0.0D0 
      IF(N.LE.0)RETURN   
      IF(INCX.EQ.1)GOTO 20    
C    
C        CODE FOR INCREMENTS NOT EQUAL TO 1. 
C    
      NS = N*INCX
      DO  I=1,NS,INCX
             SASUM = SASUM + ABS(SX(I))
      ENDDO
      RETURN   
C    
C        CODE FOR INCREMENTS EQUAL TO 1.
C    
C    
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6. 
C    
   20 M = MOD(N,6)  
      IF( M .EQ. 0 ) GO TO 40 
      DO   I = 1,M
        SASUM = SASUM + ABS(SX(I))
      ENDDO
      IF( N .LT. 6 ) RETURN   
   40 MP1 = M + 1   
      DO  I = MP1,N,6
        SASUM = SASUM + ABS(SX(I)) + ABS(SX(I + 1)) + ABS(SX(I + 2))
     1  + ABS(SX(I + 3)) + ABS(SX(I + 4)) + ABS(SX(I + 5))  
      ENDDO
      RETURN   
      END 
      SUBROUTINE SAXPY(N,SA,SX,INCX,SY,INCY) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SAXPY
C***DATE WRITTEN   791001   (YYMMDD)    
C***REVISION DATE  820801   (YYMMDD)    
C***CATEGORY NO.  D1A7   
C***KEYWORDS  BLAS,LINEAR ALGEBRA,TRIAD,VECTOR    
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)    
C           KROGH, F. T., (JPL)    
C***PURPOSE  S.P. computation y = a*x + y    
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C    
C     --Input--
C        N  number of elements in input vector(s) 
C       SA  single precision scalar multiplier    
C       SX  single precision vector with N elements    
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements    
C     INCY  storage spacing between elements of SY
C    
C     --Output--    
C       SY  single precision result (unchanged if N .LE. 0) 
C    
C     Overwrite single precision SY with single precision SA*SX +SY.  
C     For I = 0 to N-1, replace  SY(LY+I*INCY) with SA*SX(LX+I*INCX) +
C       SY(LY+I*INCY), where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N    
C       and LY is defined in a similar way using INCY. 
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  SAXPY  
C    
      DOUBLE PRECISION SX(*),SY(*),SA
C***FIRST EXECUTABLE STATEMENT  SAXPY   
      IF(N.LE.0.OR.SA.EQ.0.D0) RETURN   
      IF(INCX.EQ.1.AND.INCY.EQ.1) THEN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1 
C    
C    
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4. 
         M = MOD(N,4)
         IF( M .EQ. 0 ) GO TO 40
         DO   I = 1,M
           SY(I) = SY(I) + SA*SX(I)
         ENDDO
         IF( N .LT. 4 ) RETURN
   40    MP1 = M + 1
         DO  I = MP1,N,4
           SY(I) = SY(I) + SA*SX(I)
           SY(I + 1) = SY(I + 1) + SA*SX(I + 1)
           SY(I + 2) = SY(I + 2) + SA*SX(I + 2)
           SY(I + 3) = SY(I + 3) + SA*SX(I + 3)
         ENDDO
         RETURN
      ENDIF
C
C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS. 
C    
      IF(INCX.EQ.INCY.AND.INCX.GT.1) THEN
         NS = N*INCX
         DO   I=1,NS,INCX
          SY(I) = SA*SX(I) + SY(I)
         ENDDO
         RETURN
      ENDIF
C
C        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS.  
C    
         IX = 1
         IY = 1
         IF(INCX.LT.0)IX = (-N+1)*INCX + 1
         IF(INCY.LT.0)IY = (-N+1)*INCY + 1
         DO  I = 1,N
           SY(IY) = SY(IY) + SA*SX(IX)
           IX = IX + INCX
           IY = IY + INCY
         ENDDO
         RETURN
C
      END
      SUBROUTINE SCOPY(N,SX,INCX,SY,INCY)    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SCOPY
C***DATE WRITTEN   791001   (YYMMDD)    
C***REVISION DATE  820801   (YYMMDD)    
C***CATEGORY NO.  D1A5   
C***KEYWORDS  BLAS,COPY,LINEAR ALGEBRA,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)    
C           KROGH, F. T., (JPL)    
C***PURPOSE  Copy s.p. vector y = x
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C    
C     --Input--
C        N  number of elements in input vector(s) 
C       SX  single precision vector with N elements    
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements    
C     INCY  storage spacing between elements of SY
C    
C     --Output--    
C       SY  copy of vector SX (unchanged if N .LE. 0)  
C    
C     Copy single precision SX to single precision SY. 
C     For I = 0 to N-1, copy  SX(LX+I*INCX) to SY(LY+I*INCY),    
C     where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N, and LY is
C     defined in a similar way using INCY.   
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  SCOPY  
C    
      DOUBLE PRECISION SX(*),SY(*)
C***FIRST EXECUTABLE STATEMENT  SCOPY   
      IF(N.LE.0)RETURN   
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1 
C    
C    
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 7. 
C    
      IF(INCX.EQ.1.AND.INCY.EQ.1) THEN
         M = MOD(N,7)
         IF( M .EQ. 0 ) GO TO 40
         DO   I = 1,M
           SY(I) = SX(I)
         ENDDO
         IF( N .LT. 7 ) RETURN
   40    MP1 = M + 1
         DO   I = MP1,N,7
           SY(I) = SX(I)
           SY(I + 1) = SX(I + 1)
           SY(I + 2) = SX(I + 2)
           SY(I + 3) = SX(I + 3)
           SY(I + 4) = SX(I + 4)
           SY(I + 5) = SX(I + 5)
           SY(I + 6) = SX(I + 6)
         ENDDO
         RETURN
      ENDIF
C
C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS. 
C    
      IF(INCX.EQ.INCY.AND.INCX.GT.1) THEN
         NS = N*INCX
         DO   I=1,NS,INCX
          SY(I) = SX(I)
         ENDDO
         RETURN
      ENDIF
C
C        CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.   
C    
         IX = 1
         IY = 1
         IF(INCX.LT.0)IX = (-N+1)*INCX + 1
         IF(INCY.LT.0)IY = (-N+1)*INCY + 1
         DO   I = 1,N
           SY(IY) = SX(IX)
           IX = IX + INCX
           IY = IY + INCY
         ENDDO
      END
      DOUBLE PRECISION FUNCTION SDOT(N,SX,INCX,SY,INCY)  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SDOT 
C***DATE WRITTEN   791001   (YYMMDD)    
C***REVISION DATE  820801   (YYMMDD)    
C***CATEGORY NO.  D1A4   
C***KEYWORDS  BLAS,INNER PRODUCT,LINEAR ALGEBRA,VECTOR 
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)    
C           KROGH, F. T., (JPL)    
C***PURPOSE  S.P. inner product of s.p. vectors   
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C    
C     --Input--
C        N  number of elements in input vector(s) 
C       SX  single precision vector with N elements    
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements    
C     INCY  storage spacing between elements of SY
C    
C     --Output--    
C     SDOT  single precision dot product (zero if N .LE. 0) 
C    
C     Returns the dot product of single precision SX and SY.
C     SDOT = sum for I = 0 to N-1 of  SX(LX+I*INCX) * SY(LY+I*INCY),  
C     where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N, and LY is
C     defined in a similar way using INCY.   
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  SDOT   
C    
      DOUBLE PRECISION SX(*),SY(*)
C***FIRST EXECUTABLE STATEMENT  SDOT    
      BIG=SQRT(R1MACH(2))
      SMA=SQRT(R1MACH(1))
      NS=N*INCX
      DO   I=1,NS,INCX
      IF(ABS(SX(I)).GT.BIG) SX(I)=SIGN(BIG,SX(I))
      IF(ABS(SX(I)).LT.SMA) SX(I)=0.
      ENDDO
      
      NS=N*INCY
      DO   I=1,NS,INCY
      IF(ABS(SY(I)).GT.BIG) SY(I)=SIGN(BIG,SY(I))
      IF(ABS(SY(I)).LT.SMA) SY(I)=0.       
      ENDDO
      SDOT = 0.0D0  
      IF(N.LE.0)RETURN   
      IF(INCX.EQ.1.AND.INCY.EQ.1) THEN
C    
C        CODE FOR BOTH INCREMENTS EQUAL TO 1 
C    
C    
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5. 
C    
         M = MOD(N,5)
         IF( M .EQ. 0 ) GO TO 40
         DO   I = 1,M
           SDOT = SDOT + SX(I)*SY(I)
         ENDDO
         IF( N .LT. 5 ) RETURN
   40    MP1 = M + 1
         DO  I = MP1,N,5
         SDOT = SDOT + SX(I)*SY(I) + SX(I + 1)*SY(I + 1) +
     &   SX(I + 2)*SY(I + 2) + SX(I + 3)*SY(I + 3) + SX(I + 4)*SY(I + 4)
         ENDDO
         RETURN
      ENDIF
C
C        CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.
C    
      IF(INCX.EQ.INCY.AND.INCX.GT.1) THEN
         NS=N*INCX
         DO  I=1,NS,INCX
           SDOT = SDOT + SX(I)*SY(I)
         ENDDO
         RETURN
      ENDIF

C
C        CODE FOR UNEQUAL INCREMENTS OR NONPOSITIVE INCREMENTS.  
C    
      IX = 1   
      IY = 1   
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1 
      DO  I = 1,N
        SDOT = SDOT + SX(IX)*SY(IY)
        IX = IX + INCX   
        IY = IY + INCY
      ENDDO
      RETURN
      END
      DOUBLE PRECISION FUNCTION SNRM2(N,SX,INCX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SNRM2
C***DATE WRITTEN   791001   (YYMMDD)    
C***REVISION DATE  820801   (YYMMDD)    
C***CATEGORY NO.  D1A3B  
C***KEYWORDS  BLAS,EUCLIDEAN,L2,LENGTH,LINEAR ALGEBRA,NORM,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)    
C***PURPOSE  Euclidean length (L2 norm) of s.p. vector 
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C    
C     --Input--
C        N  number of elements in input vector(s) 
C       SX  single precision vector with N elements    
C     INCX  storage spacing between elements of SX
C    
C     --Output--    
C    SNRM2  single precision result (zero if N .LE. 0) 
C    
C     Euclidean norm of the N-vector stored in SX() with storage 
C     increment INCX .   
C     If N .LE. 0, return with result = 0.   
C     If N .GE. 1, then INCX must be .GE. 1  
C    
C           C. L. Lawson, 1978 Jan 08   
C    
C     Four Phase Method     using two built-in constants that are
C     hopefully applicable to all machines.  
C         CUTLO = maximum of  SQRT(U/EPS)  over all known machines.   
C         CUTHI = minimum of  SQRT(V)      over all known machines.   
C     where    
C         EPS = smallest no. such that EPS + 1. .GT. 1.
C         U   = smallest positive no.   (underflow limit)   
C         V   = largest  no.            (overflow  limit)   
C    
C     Brief Outline of Algorithm.. 
C    
C     Phase 1 scans zero components.    
C     Move to phase 2 when a component is nonzero and .LE. CUTLO 
C     Move to phase 3 when a component is .GT. CUTLO   
C     Move to phase 4 when a component is .GE. CUTHI/M 
C     where M = N for X() real and M = 2*N for complex.
C    
C     Values for CUTLO and CUTHI.. 
C     From the environmental parameters listed in the IMSL converter  
C     document the limiting values are as follows..    
C     CUTLO, S.P.   U/EPS = 2**(-102) for  Honeywell.  Close seconds are   
C                   Univac and DEC at 2**(-103)   
C                   Thus CUTLO = 2**(-51) = 4.44089E-16
C     CUTHI, S.P.   V = 2**127 for Univac, Honeywell, and DEC.   
C                   Thus CUTHI = 2**(63.5) = 1.30438E19
C     CUTLO, D.P.   U/EPS = 2**(-67) for Honeywell and DEC. 
C                   Thus CUTLO = 2**(-33.5) = 8.23181D-11   
C     CUTHI, D.P.   same as S.P.  CUTHI = 1.30438D19   
C     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /  
C     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /  
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  SNRM2  
      DOUBLE PRECISION   SX(*),  CUTLO, CUTHI, HITEST, SUMM, XMAX,
     &                   ZERO, ONE  
      DOUBLE PRECISION R1MACH
      DATA   ZERO, ONE /0.0D0, 1.0D0/   
C    
      CUTHI=SQRT(R1MACH(2))
      CUTLO=SQRT(R1MACH(1))
C***FIRST EXECUTABLE STATEMENT  SNRM2
      IF(N .GT. 0) GO TO 10   
         SNRM2  = ZERO   
         GO TO 300  
C    
   10 ISCH=1
      SUMM = ZERO
      XMAX=ZERO
      NN = N * INCX 
C                                                 BEGIN MAIN LOOP
      I = 1    
C
C     AENDERUNG UNTERFORSTHUBER
C     PRUEFEN, OB WERT UNTERHALB DER MASCHINENGENAUIGKEIT
C
C
   20 IF( SX(I) .GE.-R1MACH(1).AND.SX(I).LE.R1MACH(1)) GO TO 200
      GOTO(30,50,70,110),ISCH
C      IF(ISCH.EQ.1) THEN
C         GOTO 30
C      ELSE IF(ISCH.EQ.2) THEN
C         GOTO 50
C      ELSE IF (ISCH.EQ.3) THEN
C         GOTO 70
C      ELSE
C         GOTO 110
C      ENDIF



   30 IF( ABS(SX(I)) .GT. CUTLO) GO TO 85
      ISCH=2
      XMAX = ZERO   
C    
C                        PHASE 1.  SUM IS ZERO    
C    
   50 IF(DABS(SX(I)) .GT. CUTLO) GO TO 85    
C    
C                                PREPARE FOR PHASE 2.  
      ISCH=3
      GO TO 105
C    
C                                PREPARE FOR PHASE 4.  
C    
  100 I = J    
      ISCH=4
      SUMM = (SUMM / SX(I)) / SX(I)
  105 XMAX = ABS(SX(I))  
      GO TO 115
C    
C                   PHASE 2.  SUM IS SMALL.  
C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.   
C    
   70 IF( ABS(SX(I)) .GT. CUTLO ) GO TO 75   
C    
C                     COMMON CODE FOR PHASES 2 AND 4.  
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.   
C    
  110 IF( ABS(SX(I)) .LE. XMAX ) GO TO 115   
         SUMM = ONE + SUMM * (XMAX / SX(I))**2
         XMAX = DABS(SX(I))    
         GO TO 200  
C    
  115 SUMM = SUMM + (SX(I)/XMAX)**2
      GO TO 200
C    
C    
C                  PREPARE FOR PHASE 3. 
C    
   75 SUMM = (SUMM * XMAX) * XMAX
C    
C    
C     FOR REAL OR D.P. SET HITEST = CUTHI/N  
C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)   
C    
   85 HITEST = CUTHI/FLOAT( N )    
C    
C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
C    
      DO   J =I,NN,INCX
        IF(DABS(SX(J)) .GE. HITEST) GO TO 100
        SUMM = SUMM + SX(J)**2
      ENDDO
      SNRM2 = SQRT( DABS(SUMM) )
      GO TO 300
C
  200 CONTINUE 
      I = I + INCX  
      IF ( I .LE. NN ) GO TO 20    
C    
C              END OF MAIN LOOP.   
C    
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.  
C    
      SNRM2=XMAX*SQRT(DABS(SUMM))
  300 CONTINUE 
      RETURN   
      END 

      SUBROUTINE SROT(N,SX,INCX,SY,INCY,SC,SS)    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SROT 
C***DATE WRITTEN   791001   (YYMMDD)    
C***REVISION DATE  820801   (YYMMDD)    
C***CATEGORY NO.  D1A8   
C***KEYWORDS  BLAS,GIVENS ROTATION,LINEAR ALGEBRA,VECTOR    
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)    
C           KROGH, F. T., (JPL)    
C***PURPOSE  Apply s.p. Givens rotation 
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C    
C     --Input--
C        N  number of elements in input vector(s) 
C       SX  single precision vector with N elements    
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements    
C     INCY  storage spacing between elements of SY
C       SC  element of rotation matrix  
C       SS  element of rotation matrix  
C    
C     --Output--    
C       SX  rotated vector SX (unchanged if N .LE. 0)  
C       SY  rotated vector SY (unchanged if N .LE. 0)  
C    
C     Multiply the 2 x 2 matrix  ( SC SS) times the 2 x N matrix (SX**T)   
C                                (-SS SC)                        (SY**T)   
C     where **T indicates transpose.  The elements of SX are in  
C     SX(LX+I*INCX), I = 0 to N-1, where LX = 1 if INCX .GE. 0, else  
C     LX = (-INCX)*N, and similarly for SY using LY and INCY.    
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  SROT   
C    
      DOUBLE PRECISION   SX,SY,SC,SS,ZERO,ONE,W,Z   
      DIMENSION SX(*),SY(*)
      DATA ZERO,ONE/0.D0,1.D0/
      ZERO=TINY(1.D0)
C***FIRST EXECUTABLE STATEMENT  SROT
      IF(N .LE. 0 .OR. (ABS(SS) .LE. ZERO .AND. ABS(SC-ONE).LE.ZERO))
     &  GO TO 40
      IF(.NOT. (INCX .EQ. INCY .AND. INCX .GT. 0)) GO TO 20 
C    
           NSTEPS=INCX*N 
           DO 10 I=1,NSTEPS,INCX
                W=SX(I)
                Z=SY(I)  
                SX(I)=SC*W+SS*Z    
                SY(I)=-SS*W+SC*Z   
   10           CONTINUE 
           GO TO 40 
C    
   20 CONTINUE 
           KX=1
           KY=1
C    
           IF(INCX .LT. 0) KX=1-(N-1)*INCX   
           IF(INCY .LT. 0) KY=1-(N-1)*INCY   
C    
           DO 30 I=1,N
                W=SX(KX)
                Z=SY(KY)
                SX(KX)=SC*W+SS*Z   
                SY(KY)=-SS*W+SC*Z  
                KX=KX+INCX    
                KY=KY+INCY    
   30           CONTINUE 
   40 CONTINUE 
C    
      RETURN   
      END 
      SUBROUTINE SROTG(SA,SB,SC,SS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SROTG
C***DATE WRITTEN   791001   (YYMMDD)    
C***REVISION DATE  820801   (YYMMDD)    
C***CATEGORY NO.  D1B10  
C***KEYWORDS  BLAS,GIVENS ROTATION,LINEAR ALGEBRA,VECTOR    
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)    
C           KROGH, F. T., (JPL)    
C***PURPOSE  Construct s.p. plane Givens rotation 
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C    
C     --Input--
C       SA  single precision scalar
C       SB  single precision scalar
C    
C     --Output--    
C       SA  single precision result R   
C       SB  single precision result Z   
C       SC  single precision result
C       SS  single precision result
C    
C     Designed by C. L. Lawson, JPL, 1977 Sept 08 
C    
C    
C     Construct the Givens transformation    
C    
C         ( SC  SS )
C     G = (        ) ,    SC**2 + SS**2 = 1 ,
C         (-SS  SC )
C    
C     which zeros the second entry of the 2-vector  (SA,SB)**T.  
C    
C     The quantity R = (+/-)SQRT(SA**2 + SB**2) overwrites SA in 
C     storage.  The value of SB is overwritten by a value Z which
C     allows SC and SS to be recovered by the following algorithm:    
C    
C           If Z=1  set  SC=0.  and  SS=1.   
C           If ABS(Z) .LT. 1  set  SC=SQRT(1-Z**2)  and  SS=Z    
C           If ABS(Z) .GT. 1  set  SC=1/Z  and  SS=SQRT(1-SC**2) 
C    
C     Normally, the subprogram SROT(N,SX,INCX,SY,INCY,SC,SS) will
C     next be called to apply the transformation to a 2 by N matrix.  
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  SROTG  
C    
C***FIRST EXECUTABLE STATEMENT  SROTG   
      DOUBLE PRECISION R1MACH
C     AEDERUNG UNTERFORSTHUBER
      BIG=SQRT(R1MACH(2))
      SMA=SQRT(R1MACH(1))
      IF(ABS(SA).LT.SMA) SA=0.
      IF(ABS(SA).GT.BIG) SA=SIGN(BIG,SA)
      IF(ABS(SB).LT.SMA) SB=0.
      IF(ABS(SB).GT.BIG) SB=SIGN(BIG,SB)
      IF (ABS(SA) .LE. ABS(SB)) GO TO 10
C    
C *** HERE ABS(SA) .GT. ABS(SB) ***
C    
      U = SA + SA   
      V = SB / U    
C    
C     NOTE THAT U AND R HAVE THE SIGN OF SA  
C    
      R = SQRT(.25 + V**2) * U
C    
C     NOTE THAT SC IS POSITIVE
C    
      SC = SA / R   
      SS = V * (SC + SC) 
      SB = SS  
      SA = R   
      RETURN   
C    
C *** HERE ABS(SA) .LE. ABS(SB) ***
C    
   10 IF (SB .GE.-R1MACH(1).AND.SB.LE.R1MACH(1)) GO TO 20
      U = SB + SB   
      V = SA / U    
C    
C     NOTE THAT U AND R HAVE THE SIGN OF SB  
C     (R IS IMMEDIATELY STORED IN SA)   
C    
      SA = SQRT(.25 + V**2) * U    
C    
C     NOTE THAT SS IS POSITIVE
C    
      SS = SB / SA  
      SC = V * (SS + SS) 
      IF (SC .EQ. 0.) GO TO 15
      SB = 1. / SC  
      RETURN   
   15 SB = 1.  
      RETURN   
C    
C *** HERE SA = SB = 0. ***   
C    
   20 SC = 1.  
      SS = 0.  
      RETURN   
C    
      END 
      SUBROUTINE SROTM(N,SX,INCX,SY,INCY,SPARAM)  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SROTM
C***DATE WRITTEN   791001   (YYMMDD)    
C***REVISION DATE  820801   (YYMMDD)    
C***CATEGORY NO.  D1A8   
C***KEYWORDS  BLAS,LINEAR ALGEBRA,MODIFIED GIVENS ROTATION,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)    
C           KROGH, F. T., (JPL)    
C***PURPOSE  Apply s.p. modified Givens transformation 
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C    
C     --Input--
C        N  number of elements in input vector(s) 
C       SX  single precision vector with N elements    
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements    
C     INCY  storage spacing between elements of SY
C   SPARAM  5-element vector. SPARAM(1) is SFLAG described below.
C             Locations 2-5 of SPARAM contain elements of the    
C              transformation matrix H described below.
C    
C     --Output--    
C       SX  rotated vector (unchanged if N .LE. 0)
C       SY  rotated vector (unchanged if N .LE. 0)
C    
C     Apply the modified Givens transformation, H, to the 2 by N matrix    
C    
C     (SX**T) , where **T indicates transpose.  The elements of SX are
C     in (SY**T)    
C    
C     SX(LX+I*INCX), I = 0 to N-1, where LX = 1 if INCX .GE. 0, else  
C     LX = (-INCX)*N, and similarly for SY using using LY and INCY.   
C     With SPARAM(1)=SFLAG, H has one of the following forms..   
C    
C     SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0    
C    
C       (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)  
C     H=(          )    (          )    (          )    (          )  
C       (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0). 
C    
C     See SROTMG for a description of data storage in SPARAM.    
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  SROTM  
C    
      DIMENSION SX(*),SY(*),SPARAM(5)
      DATA ZERO,TWO/0.D0,2.D0/
      ZERO=TINY(1.D0)
C***FIRST EXECUTABLE STATEMENT  SROTM
      SFLAG=SPARAM(1)
      IF(N .LE. 0 .OR.(ABS(SFLAG+TWO).LE.ZERO)) GO TO 140
          IF(.NOT.(INCX.EQ.INCY.AND. INCX .GT.0)) GO TO 70  
C    
               NSTEPS=N*INCX  
               IF(ABS(SFLAG).LE.ZERO) THEN
C               IF(SFLAG) 50,10,30
C   10          CONTINUE
                SH12=SPARAM(4)
                SH21=SPARAM(3)
                    DO 20 I=1,NSTEPS,INCX

                    W=SX(I)   
                    Z=SY(I)   
                    SX(I)=W+Z*SH12 
                    SY(I)=W*SH21+Z 
   20               CONTINUE  
                GO TO 140
               ELSE IF(SFLAG.GT.ZERO) THEN
C   30           CONTINUE
                SH11=SPARAM(2)
                SH22=SPARAM(5)
                    DO 40 I=1,NSTEPS,INCX
                    W=SX(I)
                    Z=SY(I)   
                    SX(I)=W*SH11+Z 
                    SY(I)=-W+SH22*Z
   40               CONTINUE  
               GO TO 140 
             ELSE IF(SFLAG.LT.ZERO) THEN
C   50          CONTINUE
               SH11=SPARAM(2) 
               SH12=SPARAM(4) 
               SH21=SPARAM(3) 
               SH22=SPARAM(5) 
                    DO 60 I=1,NSTEPS,INCX
                    W=SX(I)
                    Z=SY(I)   
                    SX(I)=W*SH11+Z*SH12 
                    SY(I)=W*SH21+Z*SH22 
   60               CONTINUE  
               GO TO 140 
             ENDIF
   70     CONTINUE  
          KX=1 
          KY=1 
          IF(INCX .LT. 0) KX=1+(1-N)*INCX    
          IF(INCY .LT. 0) KY=1+(1-N)*INCY    
C    
          IF(ABS(SFLAG).LE.ZERO) THEN
C          IF(SFLAG)120,80,100
C   80     CONTINUE
           SH12=SPARAM(4)
           SH21=SPARAM(3)
               DO 90 I=1,N
               W=SX(KX)
               Z=SY(KY)  
               SX(KX)=W+Z*SH12
               SY(KY)=W*SH21+Z
               KX=KX+INCX
               KY=KY+INCY
   90          CONTINUE  
          GO TO 140 
          ELSE IF(SFLAG.GT.ZERO) THEN
C  100     CONTINUE
           SH11=SPARAM(2)
           SH22=SPARAM(5)
               DO 110 I=1,N
               W=SX(KX)
               Z=SY(KY)  
               SX(KX)=W*SH11+Z
               SY(KY)=-W+SH22*Z    
               KX=KX+INCX
               KY=KY+INCY
  110          CONTINUE  
          GO TO 140 
          ELSE IF(SFLAG.LT.ZERO) THEN
C  120     CONTINUE
           SH11=SPARAM(2)
           SH12=SPARAM(4)
           SH21=SPARAM(3)
           SH22=SPARAM(5)
               DO 130 I=1,N
               W=SX(KX)  
               Z=SY(KY)  
               SX(KX)=W*SH11+Z*SH12
               SY(KY)=W*SH21+Z*SH22
               KX=KX+INCX
               KY=KY+INCY
  130          CONTINUE  
          ENDIF
  140     CONTINUE  
          RETURN    
      END 
      SUBROUTINE SROTMG(SD1,SD2,SX1,SY1,SPARAM)   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SROTMG    
C***DATE WRITTEN   780301   (YYMMDD)    
C***REVISION DATE  820801   (YYMMDD)    
C***CATEGORY NO.  D1B10  
C***KEYWORDS  BLAS,LINEAR ALGEBRA,MODIFIED GIVENS ROTATION,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)    
C           KROGH, F. T., (JPL)    
C***PURPOSE  Construct s.p. modified Givens transformation  
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C    
C     --Input--
C      SD1  single precision scalar used to define A1 below 
C      SD2  single precision scalar used to define A2 below 
C      SB1  single precision scalar defining A1 below  
C      SB2  single precision scalar defining A2 below  
C   SPARAM  S.P. 5-vector. SPARAM(1)=SFLAG defined below.   
C           Locations 2-5 contain the rotation matrix. 
C    
C     --Output--    
C     SD1  changed to represent the effect of the transformation 
C     SD2  changed to represent the effect of the transformation 
C     SB1  changed to represent the effect of the transformation 
C     SB2  unchanged
C    
C     Construct the modified Givens transformation matrix H which zeros    
C     the second component of the 2-vector  (SQRT(SD1)*SX1,SQRT(SD2)* 
C     SY2)**T. 
C     With SPARAM(1)=SFLAG, H has one of the following forms..   
C    
C     SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0    
C    
C       (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)  
C     H=(          )    (          )    (          )    (          )  
C       (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0).
C    
C     Locations 2-5 of SPARAM contain SH11,SH21,SH12, and SH22   
C     respectively.  (Values of 1.E0, -1.E0, or 0.E0 implied by the   
C     value of SPARAM(1) are not stored in SPARAM.)    
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  SROTMG 
C    
      DIMENSION SPARAM(5)
      INTEGER  IGO
      DATA ZERO,ONE,TWO /0.D0,1.D0,2.D0/
      DATA GAM,GAMSQ,RGAMSQ/4096.D0,1.67772E7,5.96046E-8/   
C***FIRST EXECUTABLE STATEMENT  SROTMG  
      IF(.NOT. SD1 .LT. ZERO) GO TO 10  
C       GO ZERO-H-D-AND-SX1.. 
          GO TO 60  
   10 CONTINUE 
C     CASE-SD1-NONNEGATIVE    
      SP2=SD2*SY1   
      IF(.NOT. ABS(SP2) .LE. TINY(1.D0)) GO TO 20
          SFLAG=-TWO
          GO TO 260 
C     REGULAR-CASE..
   20 CONTINUE 
      SP1=SD1*SX1   
      SQ2=SP2*SY1   
      SQ1=SP1*SX1   
C    
      IF(.NOT. ABS(SQ1) .GT. ABS(SQ2)) GO TO 40   
          SH21=-SY1/SX1  
          SH12=SP2/SP1   
C    
          SU=ONE-SH12*SH21    
C    
          IF(.NOT. SU .LE. ZERO) GO TO 30    
C         GO ZERO-H-D-AND-SX1..    
               GO TO 60  
   30     CONTINUE  
               SFLAG=ZERO
               SD1=SD1/SU
               SD2=SD2/SU
               SX1=SX1*SU
C         GO SCALE-CHECK..    
               GO TO 100 
   40 CONTINUE 
          IF(.NOT. SQ2 .LT. ZERO) GO TO 50   
C         GO ZERO-H-D-AND-SX1..    
               GO TO 60  
   50     CONTINUE  
               SFLAG=ONE 
               SH11=SP1/SP2   
               SH22=SX1/SY1   
               SU=ONE+SH11*SH22    
               STEMP=SD2/SU   
               SD2=SD1/SU
               SD1=STEMP 
               SX1=SY1*SU
C         GO SCALE-CHECK 
               GO TO 100 
C     PROCEDURE..ZERO-H-D-AND-SX1..
   60 CONTINUE 
          SFLAG=-ONE
          SH11=ZERO 
          SH12=ZERO 
          SH21=ZERO 
          SH22=ZERO 
C    
          SD1=ZERO  
          SD2=ZERO  
          SX1=ZERO  
C         RETURN..  
          GO TO 220 
C     PROCEDURE..FIX-H.. 
   70 CONTINUE 
      IF(.NOT. SFLAG .GE. ZERO) GO TO 90
C    
          IF(.NOT. ABS(SFLAG) .LE. TINY(1.D0)) GO TO 80
          SH11=ONE  
          SH22=ONE  
          SFLAG=-ONE
          GO TO 90  
   80     CONTINUE  
          SH21=-ONE 
          SH12=ONE  
          SFLAG=-ONE
   90 CONTINUE 
      IF(IGO.EQ.1) THEN
        GOTO 120
      ELSE IF(IGO.EQ.2) THEN
        GOTO 150
      ELSE IF(IGO.EQ.3) THEN
        GOTO 180
      ELSE IF(IGO.EQ.4) THEN
        GOTO 210
      ENDIF
C      GO TO IGO,(120,150,180,210)
C     PROCEDURE..SCALE-CHECK  
  100 CONTINUE 
  110     CONTINUE  
          IF(.NOT. SD1 .LE. RGAMSQ) GO TO 130
               IF(ABS(SD1) .LE.TINY(1.D0)) GO TO 160
               IGO=1
C              FIX-H..   
               GO TO 70  
  120          CONTINUE  
               SD1=SD1*GAM**2 
               SX1=SX1/GAM    
               SH11=SH11/GAM  
               SH12=SH12/GAM  
          GO TO 110 
  130 CONTINUE 
  140     CONTINUE  
          IF(.NOT. SD1 .GE. GAMSQ) GO TO 160 
               IGO=2
C              FIX-H..   
               GO TO 70  
  150          CONTINUE  
               SD1=SD1/GAM**2 
               SX1=SX1*GAM    
               SH11=SH11*GAM  
               SH12=SH12*GAM  
          GO TO 140 
  160 CONTINUE 
  170     CONTINUE  
          IF(.NOT. ABS(SD2) .LE. RGAMSQ) GO TO 190
               IF(ABS(SD2) .LE. TINY(1.D0)) GO TO 220
               IGO=3
C              FIX-H..   
               GO TO 70  
  180          CONTINUE  
               SD2=SD2*GAM**2 
               SH21=SH21/GAM  
               SH22=SH22/GAM  
          GO TO 170 
  190 CONTINUE 
  200     CONTINUE  
          IF(.NOT. ABS(SD2) .GE. GAMSQ) GO TO 220 
               IGO=4
C              FIX-H..   
               GO TO 70  
  210          CONTINUE  
               SD2=SD2/GAM**2 
               SH21=SH21*GAM  
               SH22=SH22*GAM  
          GO TO 200 
  220 CONTINUE 
C          IF(SFLAG)250,230,240
          IF(SFLAG.EQ.0) THEN
             GOTO 230
          ELSE IF(SFLAG.GT.0) THEN
             GOTO 240
          ELSE
             GOTO 250
          ENDIF
  230     CONTINUE  
               SPARAM(3)=SH21 
               SPARAM(4)=SH12 
               GO TO 260 
  240     CONTINUE  
               SPARAM(2)=SH11 
               SPARAM(5)=SH22 
               GO TO 260 
  250     CONTINUE  
               SPARAM(2)=SH11 
               SPARAM(3)=SH21 
               SPARAM(4)=SH12 
               SPARAM(5)=SH22 
  260 CONTINUE 
          SPARAM(1)=SFLAG
          RETURN    
      END 
      SUBROUTINE SSCAL(N,SA,SX,INCX)    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SSCAL
C***DATE WRITTEN   791001   (YYMMDD)    
C***REVISION DATE  820801   (YYMMDD)    
C***CATEGORY NO.  D1A6   
C***KEYWORDS  BLAS,LINEAR ALGEBRA,SCALE,VECTOR    
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)    
C           KROGH, F. T., (JPL)    
C***PURPOSE  S.P. vector scale x = a*x  
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C    
C     --Input--
C        N  number of elements in input vector(s) 
C       SA  single precision scale factor    
C       SX  single precision vector with N elements    
C     INCX  storage spacing between elements of SX
C    
C     --Output--    
C       SX  single precision result (unchanged if N .LE. 0) 
C    
C     Replace single precision SX by single precision SA*SX.
C     For I = 0 to N-1, replace SX(1+I*INCX) with  SA * SX(1+I*INCX)  
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  SSCAL  
C    
      DOUBLE PRECISION SA,SX(*)
C***FIRST EXECUTABLE STATEMENT  SSCAL   
      IF(N.LE.0)RETURN   
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1. 
C    
      NS = N*INCX   
          DO 10 I = 1,NS,INCX 
          SX(I) = SA*SX(I)    
   10     CONTINUE  
      RETURN   
C    
C        CODE FOR INCREMENTS EQUAL TO 1.
C    
C    
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5. 
C    
   20 M = MOD(N,5)  
      IF( M .EQ. 0 ) GO TO 40 
      DO 30 I = 1,M
        SX(I) = SA*SX(I) 
   30 CONTINUE 
      IF( N .LT. 5 ) RETURN   
   40 MP1 = M + 1   
      DO 50 I = MP1,N,5
        SX(I) = SA*SX(I)
        SX(I + 1) = SA*SX(I + 1)   
        SX(I + 2) = SA*SX(I + 2)   
        SX(I + 3) = SA*SX(I + 3)   
        SX(I + 4) = SA*SX(I + 4)   
   50 CONTINUE 
      RETURN   
      END 
      SUBROUTINE SSWAP(N,SX,INCX,SY,INCY)    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***BEGIN PROLOGUE  SSWAP
C***DATE WRITTEN   791001   (YYMMDD)    
C***REVISION DATE  820801   (YYMMDD)    
C***CATEGORY NO.  D1A5   
C***KEYWORDS  BLAS,INTERCHANGE,LINEAR ALGEBRA,VECTOR   
C***AUTHOR  LAWSON, C. L., (JPL)   
C           HANSON, R. J., (SNLA)  
C           KINCAID, D. R., (U. OF TEXAS)    
C           KROGH, F. T., (JPL)    
C***PURPOSE  Interchange s.p vectors    
C***DESCRIPTION
C    
C                B L A S  Subprogram    
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s) 
C       SX  single precision vector with N elements    
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements    
C     INCY  storage spacing between elements of SY
C    
C     --Output--    
C       SX  input vector SY (unchanged if N .LE. 0)    
C       SY  input vector SX (unchanged if N .LE. 0)    
C    
C     Interchange single precision SX and single precision SY.   
C     For I = 0 to N-1, interchange  SX(LX+I*INCX) and SY(LY+I*INCY), 
C     where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N, and LY is
C     defined in a similar way using INCY.   
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,   
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,    
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323    
C***ROUTINES CALLED  (NONE)   
C***END PROLOGUE  SSWAP  
C    
      DOUBLE PRECISION SX(*),SY(*),STEMP1,STEMP2,STEMP3
C***FIRST EXECUTABLE STATEMENT  SSWAP   
      IF(N.LE.0)RETURN   
      IF(INCX.EQ.1.AND.INCY.EQ.1) THEN
C    
C       CODE FOR BOTH INCREMENTS EQUAL TO 1  
C    
C    
C       CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 3.  
C    
        M = MOD(N,3)
        IF( M .EQ. 0 ) GO TO 40
        DO   I = 1,M
          STEMP1 = SX(I)
          SX(I) = SY(I)
          SY(I) = STEMP1
        ENDDO
        IF( N .LT. 3 ) RETURN
   40   MP1 = M + 1
        DO I = MP1,N,3
          STEMP1 = SX(I)
          STEMP2 = SX(I+1)
          STEMP3 = SX(I+2)
          SX(I) = SY(I)
          SX(I+1) = SY(I+1)
          SX(I+2) = SY(I+2)
          SY(I) = STEMP1
          SY(I+1) = STEMP2
          SY(I+2) = STEMP3
        ENDDO
        RETURN
      ENDIF
C   60 CONTINUE
C     IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
C    5 CONTINUE
C
C     CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.    
      IF(INCX.EQ.INCY.AND.INCX.GT.1) THEN
        NS = N*INCX
        DO   I=1,NS,INCX
          STEMP1 = SX(I)
          SX(I) = SY(I)
          SY(I) = STEMP1
        ENDDO
        RETURN
      ENDIF

C    
C       CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.    
C    
      IX = 1   
      IY = 1   
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1 
      DO   I = 1,N
        STEMP1 = SX(IX)  
        SX(IX) = SY(IY)  
        SY(IY) = STEMP1  
        IX = IX + INCX   
        IY = IY + INCY   
      ENDDO
      RETURN
      END 
      INTEGER FUNCTION IDAMAX(N,DX,INCX)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

*     .. Scalar Arguments ..
      INTEGER INCX,N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION DX(*)
*     ..
*
*  Purpose
*  =======
*
*     IDAMAX finds the index of element having max. absolute value.
*
*  Further Details
*  ===============
*
*     jack dongarra, linpack, 3/11/78.
*     modified 3/93 to return if incx .le. 0.
*     modified 12/3/93, array(1) declarations changed to array(*)
*
*  =====================================================================
*
*     .. Local Scalars ..
      DOUBLE PRECISION DMAX
      INTEGER I,IX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC DABS
*     ..
      IDAMAX = 0
      IF (N.LT.1 .OR. INCX.LE.0) RETURN
      IDAMAX = 1
      IF (N.EQ.1) RETURN
      IF (INCX.EQ.1) THEN
*
*        code for increment equal to 1
*
         DMAX = DABS(DX(1))
         DO I = 2,N
            IF (DABS(DX(I)).GT.DMAX) THEN
               IDAMAX = I
               DMAX = DABS(DX(I))
            END IF
         END DO
      ELSE
*
*        code for increment not equal to 1
*
         IX = 1
         DMAX = DABS(DX(1))
         IX = IX + INCX
         DO I = 2,N
            IF (DABS(DX(IX)).GT.DMAX) THEN
               IDAMAX = I
               DMAX = DABS(DX(IX))
            END IF
            IX = IX + INCX
         END DO
      END IF
      RETURN
      END


*****************************************************************************

C
*****************************************************************************
      DOUBLE PRECISION FUNCTION R1MACH(I) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
c     INTEGER*4 SMALL
c     INTEGER*4 LARGE
c     INTEGER*4 RIGHT
c     INTEGER*4 DIVER
c     INTEGER*4 LOG10
C
c     REAL RMACH(5) 
C
c     EQUIVALENCE (RMACH(1),SMALL)
c     EQUIVALENCE (RMACH(2),LARGE)
c     EQUIVALENCE (RMACH(3),RIGHT)
c     EQUIVALENCE (RMACH(4),DIVER)
c     EQUIVALENCE (RMACH(5),LOG10)
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C     DATA RMACH(1) / Z400800000 /
C     DATA RMACH(2) / Z5FFFFFFFF /
C     DATA RMACH(3) / Z4E9800000 /
C     DATA RMACH(4) / Z4EA800000 /
C     DATA RMACH(5) / Z500E730E8 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700/6700/7700 SYSTEMS.
C
C     DATA RMACH(1) / O1771000000000000 /
C     DATA RMACH(2) / O0777777777777777 /
C     DATA RMACH(3) / O1311000000000000 /
C     DATA RMACH(4) / O1301000000000000 /
C     DATA RMACH(5) / O1157163034761675 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C
C     DATA RMACH(1) / 00564000000000000000B /
C     DATA RMACH(2) / 37767777777777777776B /
C     DATA RMACH(3) / 16414000000000000000B /
C     DATA RMACH(4) / 16424000000000000000B /
C     DATA RMACH(5) / 17164642023241175720B /
C
C     MACHINE CONSTANTS FOR THE CRAY 1
C
C     DATA RMACH(1) / 200034000000000000000B /
C     DATA RMACH(2) / 577767777777777777776B /
C     DATA RMACH(3) / 377224000000000000000B /
C     DATA RMACH(4) / 377234000000000000000B /
C     DATA RMACH(5) / 377774642023241175720B /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
C     STATIC RMACH(5)
C
C     DATA SMALL/20K,0/,LARGE/77777K,177777K/
C     DATA RIGHT/35420K,0/,DIVER/36020K,0/
C     DATA LOG10/40423K,42023K/
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C
C     DATA SMALL(1),SMALL(2) / '20000000, '00000201 /
C     DATA LARGE(1),LARGE(2) / '37777777, '00000177 /
C     DATA RIGHT(1),RIGHT(2) / '20000000, '00000352 /
C     DATA DIVER(1),DIVER(2) / '20000000, '00000353 /
C     DATA LOG10(1),LOG10(2) / '23210115, '00000377 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.
C
C     DATA RMACH(1) / O402400000000 /
C     DATA RMACH(2) / O376777777777 /
C     DATA RMACH(3) / O714400000000 /
C     DATA RMACH(4) / O716400000000 /
C     DATA RMACH(5) / O776464202324 /
C
C     MACHINE CONSTANTS FOR THE HP 2100 
C
C     3 WORD DOUBLE PRECISION WITH FTN4 
C
C     DATA SMALL(1), SMALL(2) / 40000B,       1 / 
C     DATA LARGE(1), LARGE(2) / 77777B, 177776B / 
C     DATA RIGHT(1), RIGHT(2) / 40000B,    325B / 
C     DATA DIVER(1), DIVER(2) / 40000B,    327B / 
C     DATA LOG10(1), LOG10(2) / 46420B,  46777B / 
C
C     MACHINE CONSTANTS FOR THE HP 2100 
C     4 WORD DOUBLE PRECISION WITH FTN4 
C
C     DATA SMALL(1), SMALL(2) / 40000B,       1 / 
C     DATA LARGE91), LARGE(2) / 77777B, 177776B / 
C     DATA RIGHT(1), RIGHT(2) / 40000B,    325B / 
C     DATA DIVER(1), DIVER(2) / 40000B,    327B / 
C     DATA LOG10(1), LOG10(2) / 46420B,  46777B / 
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86  AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA RMACH(1) / Z00100000 /
C     DATA RMACH(2) / Z7FFFFFFF /
C     DATA RMACH(3) / Z3B100000 /
C     DATA RMACH(4) / Z3C100000 /
C     DATA RMACH(5) / Z41134413 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA OR KI PROCESSOR).
C
C     DATA RMACH(1) / "000400000000 /
C     DATA RMACH(2) / "377777777777 /
C     DATA RMACH(3) / "146400000000 /
C     DATA RMACH(4) / "147400000000 /
C     DATA RMACH(5) / "177464202324 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1) /    8388608 /
C     DATA LARGE(1) / 2147483647 /
C     DATA RIGHT(1) /  880803840 /
C     DATA DIVER(1) /  889192448 /
C     DATA LOG10(1) / 1067065499 /
C
C     DATA RMACH(4) / O06500000000 /
C     DATA RMACH(5) / O07746420233 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGERS  (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1),SMALL(2) /   128,     0 /
C     DATA LARGE(1),LARGE(2) / 32767,    -1 /
C     DATA RIGHT(1),RIGHT(2) / 13440,     0 /
C     DATA DIVER(1),DIVER(2) / 13568,     0 /
C     DATA LOG10(1),LOG10(2) / 16282,  8347 /
C
C     DATA SMALL(1),SMALL(2) / O000200, O000000 / 
C     DATA LARGE(1),LARGE(2) / O077777, O177777 / 
C     DATA RIGHT(1),RIGHT(2) / O032200, O000000 / 
C     DATA DIVER(1),DIVER(2) / O032400, O000000 / 
C     DATA LOG10(1),LOG10(2) / O037632, O020233 / 
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C     DATA RMACH(1) / O000400000000 /
C     DATA RMACH(2) / O377777777777 /
C     DATA RMACH(3) / O146400000000 /
C     DATA RMACH(4) / O147400000000 /
C     DATA RMACH(5) / O177464202324 /
C
C     MACHINE CONSTANTS FOR THE VAX 11/780
C    (EXPRESSED IN INTEGER AND HEXADECIMAL)
C  ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSTEMS***
C  *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
C
C     DATA SMALL(1) /       128 /
C     DATA LARGE(1) /    -32769 /
C     DATA RIGHT(1) /     13440 /
C     DATA DIVER(1) /     13568 /
C     DATA LOG10(1) / 547045274 /
C
C     DATA SMALL(1) / Z00000080 /
C     DATA LARGE(1) / ZFFFF7FFF /
C     DATA RIGHT(1) / Z00003480 /
C     DATA DIVER(1) / Z00003500 /
C     DATA LOG10(1) / Z209B3F9A /
C
C     MACHINE CONSTANTS FOR THE ELXSI 6400
C       ASSUMING REAL*4 IS THE DEFAULT REAL
C
C     DATA SMALL(1)/ '00800000'X/
C     DATA LARGE(1)/ '7F7FFFFF'X/
C     DATA RIGHT(1)/ '33800000'X/
C     DATA DIVER(1)/ '34000000'X/
C     DATA LOG10(1)/ '3E9A209B'X/
C
C     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR
C
C     DATA SMALL(1),SMALL(2) /     0,    256/
C     DATA LARGE(1),LARGE(2) /    -1,   -129/
C     DATA RIGHT(1),RIGHT(2) /     0,  26880/
C     DATA DIVER(1),DIVER(2) /     0,  27136/
C     DATA LOG10(1),LOG10(2) /  8347,  32538/
C
C     MACHINE CONSTANTS FOR THE IBM PC - MICROSOFT FORTRAN
C
C     DATA SMALL(1) / #00800000 /
C     DATA LARGE(1) / #7F7FFFFF /
C     DATA RIGHT(1) / #33800000 /
C     DATA DIVER(1) / #34000000 /
C     DATA LOG10(1) / #3E9A209A /
C
C     MACHINE CONSTANTS FOR THE IBM PC - PROFESSIONAL FORTRAN
C                       FOR THE LAHEY F77 COMPILER.

c     DATA SMALL/ #00800000/
c     DATA LARGE/ #7F7FFFFF/
c     DATA RIGHT/ #33800000/
c     DATA DIVER/ #34000000/
c     DATA LOG10/ #3E9A209A/
C
C***FIRST EXECUTABLE STATEMENT  R1MACH
C
      IF(I.EQ.1) THEN
         R1MACH = TINY(1.D0)
      ELSE IF(I.EQ.2) THEN
         R1MACH = HUGE(1.D0)
      ELSE IF(I.EQ.3) THEN
         R1MACH = 0.5*EPSILON(1.D0)
      ELSE IF(I.EQ.4) THEN 
         R1MACH = EPSILON(1.D0)
      ELSE IF(I.EQ.5) THEN 
         R1MACH = LOG10(2.D0)
      ELSE
C        CALL XERRWV('ERROR IN R1MACH',15,158,I,I,I,I,I,RDUM,RDUM)
         R1MACH = TINY(1.D0)
      END IF
 
      RETURN
C
      END 
      INTEGER FUNCTION I1MACH(I)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

      INTEGER I
C
C    I1MACH( 1) = THE STANDARD INPUT UNIT.
C    I1MACH( 2) = THE STANDARD OUTPUT UNIT.
C    I1MACH( 3) = THE STANDARD PUNCH UNIT.
C    I1MACH( 4) = THE STANDARD ERROR MESSAGE UNIT.
C    I1MACH( 5) = THE NUMBER OF BITS PER INTEGER STORAGE UNIT.
C    I1MACH( 6) = THE NUMBER OF CHARACTERS PER CHARACTER STORAGE UNIT.
C    INTEGERS HAVE FORM SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C    I1MACH( 7) = A, THE BASE.
C    I1MACH( 8) = S, THE NUMBER OF BASE-A DIGITS.
C    I1MACH( 9) = A**S - 1, THE LARGEST MAGNITUDE.
C    FLOATS HAVE FORM  SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C               WHERE  EMIN .LE. E .LE. EMAX.
C    I1MACH(10) = B, THE BASE.
C  SINGLE-PRECISION
C    I1MACH(11) = T, THE NUMBER OF BASE-B DIGITS.
C    I1MACH(12) = EMIN, THE SMALLEST EXPONENT E.
C    I1MACH(13) = EMAX, THE LARGEST EXPONENT E.
C  DOUBLE-PRECISION
C    I1MACH(14) = T, THE NUMBER OF BASE-B DIGITS.
C    I1MACH(15) = EMIN, THE SMALLEST EXPONENT E.
C    I1MACH(16) = EMAX, THE LARGEST EXPONENT E.
C
      INTEGER IMACH(16), OUTPUT, SC, SMALL(2)
      SAVE IMACH, SC
      REAL RMACH
      EQUIVALENCE (IMACH(4),OUTPUT), (RMACH,SMALL(1))
      INTEGER I3, J, K, T3E(3)
      DATA T3E(1) / 9777664 /
      DATA T3E(2) / 5323660 /
      DATA T3E(3) / 46980 /
C  THIS VERSION ADAPTS AUTOMATICALLY TO MOST CURRENT MACHINES,
C  INCLUDING AUTO-DOUBLE COMPILERS.
C  TO COMPILE ON OLDER MACHINES, ADD A C IN COLUMN 1
C  ON THE NEXT LINE
      DATA SC/0/
C  AND REMOVE THE C FROM COLUMN 1 IN ONE OF THE SECTIONS BELOW.
C  CONSTANTS FOR EVEN OLDER MACHINES CAN BE OBTAINED BY
C          mail netlib@research.bell-labs.com
C          send old1mach from blas
C  PLEASE SEND CORRECTIONS TO dmg OR ehg@bell-labs.com.
C
C     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /   43 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   63 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /, SC/987/
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     32-BIT INTEGER ARITHMETIC.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C     NOTE THAT THE PUNCH UNIT, I1MACH(3), HAS BEEN SET TO 7
C     WHICH IS APPROPRIATE FOR THE UNIVAC-FOR SYSTEM.
C     IF YOU HAVE THE UNIVAC-FTN SYSTEM, SET IT TO 1.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    6 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   60 /
C      DATA IMACH(15) /-1024 /
C      DATA IMACH(16) / 1023 /, SC/987/
C
      IF (SC .NE. 987) THEN
*        *** CHECK FOR AUTODOUBLE ***
         SMALL(2) = 0
         RMACH = 1E13
         IF (SMALL(2) .NE. 0) THEN
*           *** AUTODOUBLED ***
            IF (      (SMALL(1) .EQ. 1117925532
     *           .AND. SMALL(2) .EQ. -448790528)
     *       .OR.     (SMALL(2) .EQ. 1117925532
     *           .AND. SMALL(1) .EQ. -448790528)) THEN
*               *** IEEE ***
               IMACH(10) = 2
               IMACH(14) = 53
               IMACH(15) = -1021
               IMACH(16) = 1024
            ELSE IF ( SMALL(1) .EQ. -2065213935
     *          .AND. SMALL(2) .EQ. 10752) THEN
*               *** VAX WITH D_FLOATING ***
               IMACH(10) = 2
               IMACH(14) = 56
               IMACH(15) = -127
               IMACH(16) = 127
            ELSE IF ( SMALL(1) .EQ. 1267827943
     *          .AND. SMALL(2) .EQ. 704643072) THEN
*               *** IBM MAINFRAME ***
               IMACH(10) = 16
               IMACH(14) = 14
               IMACH(15) = -64
               IMACH(16) = 63
            ELSE
c               WRITE(*,9010)
               STOP 777
               END IF
            IMACH(11) = IMACH(14)
            IMACH(12) = IMACH(15)
            IMACH(13) = IMACH(16)
         ELSE
            RMACH = 1234567.
            IF (SMALL(1) .EQ. 1234613304) THEN
*               *** IEEE ***
               IMACH(10) = 2
               IMACH(11) = 24
               IMACH(12) = -125
               IMACH(13) = 128
               IMACH(14) = 53
               IMACH(15) = -1021
               IMACH(16) = 1024
               SC = 987
            ELSE IF (SMALL(1) .EQ. -1271379306) THEN
*               *** VAX ***
               IMACH(10) = 2
               IMACH(11) = 24
               IMACH(12) = -127
               IMACH(13) = 127
               IMACH(14) = 56
               IMACH(15) = -127
               IMACH(16) = 127
               SC = 987
            ELSE IF (SMALL(1) .EQ. 1175639687) THEN
*               *** IBM MAINFRAME ***
               IMACH(10) = 16
               IMACH(11) = 6
               IMACH(12) = -64
               IMACH(13) = 63
               IMACH(14) = 14
               IMACH(15) = -64
               IMACH(16) = 63
               SC = 987
            ELSE IF (SMALL(1) .EQ. 1251390520) THEN
*              *** CONVEX C-1 ***
               IMACH(10) = 2
               IMACH(11) = 24
               IMACH(12) = -128
               IMACH(13) = 127
               IMACH(14) = 53
               IMACH(15) = -1024
               IMACH(16) = 1023
            ELSE
               DO 10 I3 = 1, 3
                  J = SMALL(1) / 10000000
                  K = SMALL(1) - 10000000*J
                  IF (K .NE. T3E(I3)) GO TO 20
                  SMALL(1) = J
 10               CONTINUE
*              *** CRAY T3E ***
               IMACH( 1) = 5
               IMACH( 2) = 6
               IMACH( 3) = 6
               IMACH( 4) = 0
               IMACH( 5) = 64
               IMACH( 6) = 8
               IMACH( 7) = 2
               IMACH( 8) = 63
               CALL I1MCR1(IMACH(9), K, 32767, 16777215, 16777215)
               IMACH(10) = 2
               IMACH(11) = 53
               IMACH(12) = -1021
               IMACH(13) = 1024
               IMACH(14) = 53
               IMACH(15) = -1021
               IMACH(16) = 1024
               GO TO 35
 20            CALL I1MCR1(J, K, 16405, 9876536, 0)
               IF (SMALL(1) .NE. J) THEN
c                  WRITE(*,9020)
                  STOP 777
                  END IF
*              *** CRAY 1, XMP, 2, AND 3 ***
               IMACH(1) = 5
               IMACH(2) = 6
               IMACH(3) = 6
               IMACH(4) = 6
               IMACH(5) = 46
               IMACH(6) = 8
               IMACH(7) = 2
               IMACH(8) = 45
               CALL I1MCR1(IMACH(9), K, 0, 4194303, 16777215)
               IMACH(10) = 2
               IMACH(11) = 47
               IMACH(12) = -8188
               IMACH(13) = 8189
               IMACH(14) = 94
               IMACH(15) = -8141
               IMACH(16) = 8189
               GO TO 35
               END IF
            END IF
         IMACH( 1) = 5
         IMACH( 2) = 6
         IMACH( 3) = 6
         IMACH( 4) = 6
         IMACH( 5) = 32
         IMACH( 6) = 4
         IMACH( 7) = 2
         IMACH( 8) = 31
         IMACH( 9) = 2147483647
 35      SC = 987
         END IF
 9010 FORMAT(/' Adjust autodoubled I1MACH by uncommenting data'/
     * ' statements appropriate for your machine and setting'/
     * ' IMACH(I) = IMACH(I+3) for I = 11, 12, and 13.')
 9020 FORMAT(/' Adjust I1MACH by uncommenting data statements'/
     * ' appropriate for your machine.')
      IF (I .LT. 1  .OR.  I .GT. 16) GO TO 40
      I1MACH = IMACH(I)
      RETURN
c 40   WRITE(*,*) 'I1MACH(I): I =',I,' is out of bounds.'
  40  STOP
* /* C source for I1MACH -- remove the * in column 1 */
* /* Note that some values may need changing. */
*#include <stdio.h>
*#include <float.h>
*#include <limits.h>
*#include <math.h>
*
*long i1mach_(long *i)
*{
*	switch(*i){
*	  case 1:  return 5;	/* standard input */
*	  case 2:  return 6;	/* standard output */
*	  case 3:  return 7;	/* standard punch */
*	  case 4:  return 0;	/* standard error */
*	  case 5:  return 32;	/* bits per integer */
*	  case 6:  return sizeof(int);
*	  case 7:  return 2;	/* base for integers */
*	  case 8:  return 31;	/* digits of integer base */
*	  case 9:  return LONG_MAX;
*	  case 10: return FLT_RADIX;
*	  case 11: return FLT_MANT_DIG;
*	  case 12: return FLT_MIN_EXP;
*	  case 13: return FLT_MAX_EXP;
*	  case 14: return DBL_MANT_DIG;
*	  case 15: return DBL_MIN_EXP;
*	  case 16: return DBL_MAX_EXP;
*	  }
*	fprintf(stderr, "invalid argument: i1mach(%ld)\n", *i);
*	exit(1);return 0; /* some compilers demand return values */
*}
      END
      SUBROUTINE I1MCR1(A, A1, B, C, D)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

**** SPECIAL COMPUTATION FOR OLD CRAY MACHINES ****
      INTEGER A, A1, B, C, D
      A1 = 16777216*B + C
      A = 16777216*A1 + D
      END
C
C
C
      SUBROUTINE XERRWV(CHR,NCHAR,NERR,LEVEL,ID1,ID2,ID3,ID4,R1,R2)
********************* 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C*********************  
      CHARACTER*(*) CHR
C      CHARACTER *90 TXT
C      WRITE (TXT,1000) NERR,ID2,ID3,R1,R2
C      TXT(25: )=CHR
      ICOD=101
      CALL FEHER(NERR,ICOD)
C 1000 FORMAT(1X,3I3,2(' ',F5.3))
      RETURN
C      
      END
C++++++++++++++++ SINGLE PRECISION VERSION 2SP (AUG 1982) ++++++++++++++
C     SUBROUTINE SVDRS2 (A,MDA,MM,NN,B,MDB,NB,S,IERROR,RANGES)
C  BASED ON LAWSON-HANSON SVDRS EXCEPT THAT STOP IS REPLACED WITH 
C      IERROR=5 AND RETURN, AND IERROR=6 IF MIN0(MM,NN).LE.0. 
C  ON A NORMAL RETURN, IERROR=1.
C  RANGES IS 2 OR 3 ORDERS OF MAGNITUDE SMALLER THAN BIG, WHERE BIG IS
C      THE LARGEST NUMBER THAT DOES NOT OVERFLOW AND 1/BIG DOES NOT 
C      UNDERFLOW.  FOR THE DOUBLE PRECISION VERSION, BIG AND RANGES
C      ARE IN DOUBLE PRECISION.  FOR THE SINGLE PRECISION VERSION,
C      THEY ARE IN SINGLE PRECISION (AND THEREFORE RANGES=SRANGE).
C-----------------------------------------------------------------------
C  CALLS SUBPROGRAMS - H12, QRBD
C  WHICH IN TURN CALL - G1, G2, DIFF
C-----------------------------------------------------------------------
C         SINGULAR VALUE DECOMPOSITION ALSO TREATING RIGHT SIDE VECTOR. 
C 
C     THE ARRAY S OCCUPIES 3*N CELLS. 
C     A OCCUPIES M*N CELLS
C     B OCCUPIES M*NB CELLS.
C 
C     SPECIAL SINGULAR VALUE DECOMPOSITION SUBROUTINE.
C     WE HAVE THE M X N MATRIX A AND THE SYSTEM A*X=B TO SOLVE. 
C     EITHER M .GE. N  OR  M .LT. N IS PERMITTED. 
C                  THE SINGULAR VALUE DECOMPOSITION 
C     A = U*S*V**(T) IS MADE IN SUCH A WAY THAT ONE GETS
C       (1) THE MATRIX V IN THE FIRST N ROWS AND COLUMNS OF A.
C       (2) THE DIAGONAL MATRIX OF ORDERED SINGULAR VALUES IN 
C       THE FIRST N CELLS OF THE ARRAY S(IP), IP .GE. 3*N.
C       (3) THE MATRIX PRODUCT U**(T)*B=G GETS PLACED BACK IN B.
C       (4) THE USER MUST COMPLETE THE SOLUTION AND DO HIS OWN
C       SINGULAR VALUE ANALYSIS.
C     ******* 
C     GIVE SPECIAL
C     TREATMENT TO ROWS AND COLUMNS WHICH ARE ENTIRELY ZERO.  THIS
C     CAUSES CERTAIN ZERO SING. VALS. TO APPEAR AS EXACT ZEROS RATHER 
C     THAN AS ABOUT ETA TIMES THE LARGEST SING. VAL.   IT SIMILARLY 
C     CLEANS UP THE ASSOCIATED COLUMNS OF U AND V.
C     METHOD..
C     1. EXCHANGE COLS OF A TO PACK NONZERO COLS TO THE LEFT. 
C        SET N = NO. OF NONZERO COLS. 
C        USE LOCATIONS A(1,NN),A(1,NN-1),...,A(1,N+1) TO RECORD THE 
C        COL PERMUTATIONS.
C     2. EXCHANGE ROWS OF A TO PACK NONZERO ROWS TO THE TOP.
C        QUIT PACKING IF FIND N NONZERO ROWS.  MAKE SAME ROW EXCHANGES
C        IN B.  SET M SO THAT ALL NONZERO ROWS OF THE PERMUTED A
C        ARE IN FIRST M ROWS.  IF M .LE. N THEN ALL M ROWS ARE
C        NONZERO.  IF M .GT. N THEN THE FIRST N ROWS ARE KNOWN
C        TO BE NONZERO,AND ROWS N+1 THRU M MAY BE ZERO OR NONZERO.
C     3. APPLY ORIGINAL ALGORITHM TO THE M BY N PROBLEM.
C     4. MOVE PERMUTATION RECORD FROM A(,) TO S(I),I=N+1,...,NN.
C     5. BUILD V UP FROM  N BY N  TO NN BY NN BY PLACING ONES ON
C        THE DIAGONAL AND ZEROS ELSEWHERE.  THIS IS ONLY PARTLY DONE
C        EXPLICITLY.  IT IS COMPLETED DURING STEP 6.
C     6. EXCHANGE ROWS OF V TO COMPENSATE FOR COL EXCHANGES OF STEP 2.
C     7. PLACE ZEROS IN  S(I),I=N+1,NN  TO REPRESENT ZERO SING VALS.
C 
      SUBROUTINE SVDRS2 (A,MDA,MM,NN,B,MDB,NB,S,IERROR,RANGES)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     DOUBLE PRECISION A, B, ONE, RANGES, S, T, ZERO!DP
      DIMENSION A(MDA,*),B(MDB,*),S(NN,3)
      ZERO=0.D0 
C     ZERO=0.D0!DP
      ONE=1.D0
C     ONE=1.D0!DP 
C 
C          BEGIN.. SPECIAL FOR ZERO ROWS AND COLS.
C 
C          PACK THE NONZERO COLS TO THE LEFT
C 
      N=NN
      IERROR=6
      IF (N.LE.0.OR.MM.LE.0) RETURN 
      IERROR=1
      J=N 
   10 CONTINUE
         DO 20 I=1,MM 
         IF (A(I,J)) 50,20,50 
   20    CONTINUE 
C 
C        COL J  IS ZERO. EXCHANGE IT WITH COL N.
C 
      IF (J.EQ.N) GO TO 40
         DO 30 I=1,MM 
   30    A(I,J)=A(I,N)
   40 CONTINUE
      A(1,N)=J
      N=N-1 
   50 CONTINUE
      J=J-1 
      IF (J.GE.1) GO TO 10
C          IF N=0 THEN A IS ENTIRELY ZERO AND SVD 
C          COMPUTATION CAN BE SKIPPED 
      NS=0
      IF (N.EQ.0) GO TO 240 
C          PACK NONZERO ROWS TO THE TOP 
C          QUIT PACKING IF FIND N NONZERO ROWS
      I=1 
      M=MM
   60 IF (I.GT.N.OR.I.GE.M) GO TO 150 
      IF (A(I,I)) 90,70,90
   70    DO 80 J=1,N
         IF (A(I,J)) 90,80,90 
   80    CONTINUE 
      GO TO 100 
   90 I=I+1 
      GO TO 60
C          ROW I IS ZERO
C          EXCHANGE ROWS I AND M
  100 IF(NB.LE.0) GO TO 115 
         DO 110 J=1,NB
         T=B(I,J) 
         B(I,J)=B(M,J)
  110    B(M,J)=T 
  115    DO 120 J=1,N 
  120    A(I,J)=A(M,J)
      IF (M.GT.N) GO TO 140 
         DO 130 J=1,N 
  130    A(M,J)=ZERO
  140 CONTINUE
C          EXCHANGE IS FINISHED 
      M=M-1 
      GO TO 60
C 
  150 CONTINUE
C          END.. SPECIAL FOR ZERO ROWS AND COLUMNS
C          BEGIN.. SVD ALGORITHM
C     METHOD..
C     (1)  REDUCE THE MATRIX TO UPPER BIDIAGONAL FORM WITH
C     HOUSEHOLDER TRANSFORMATIONS.
C         H(N)...H(1)AQ(1)...Q(N-2) = (D**T,0)**T 
C     WHERE D IS UPPER BIDIAGONAL.
C 
C     (2)  APPLY H(N)...H(1) TO B. HERE H(N)...H(1)*B REPLACES B
C     IN STORAGE. 
C 
C     (3)  THE MATRIX PRODUCT W= Q(1)...Q(N-2) OVERWRITES THE FIRST 
C     N ROWS OF A IN STORAGE. 
C 
C     (4)  AN SVD FOR D IS COMPUTED.  HERE K ROTATIONS RI AND PI ARE
C     COMPUTED SO THAT
C         RK...R1*D*P1**(T)...PK**(T) = DIAG(S1,...,SM) 
C     TO WORKING ACCURACY.  THE SI ARE NONNEGATIVE AND NONINCREASING. 
C     HERE RK...R1*B OVERWRITES B IN STORAGE WHILE
C     A*P1**(T)...PK**(T)  OVERWRITES A IN STORAGE. 
C 
C     (5)  IT FOLLOWS THAT,WITH THE PROPER DEFINITIONS, 
C     U**(T)*B OVERWRITES B, WHILE V OVERWRITES THE FIRST N ROW AND 
C     COLUMNS OF A. 
C 
      L=MIN(M,N)
C        THE FOLLOWING LOOP REDUCES A TO UPPER BIDIAGONAL AND 
C        ALSO APPLIES THE PREMULTIPLYING TRANSFORMATIONS TO B.
C 
         DO 170 J=1,L 
         IF (J.GE.M) GO TO 160
         JJ=J
         NJ=MIN(J+1,L)
         CALL H12 (1,JJ,J+1,M,A(1,J),1,T,A(1,NJ),1,MDA,N-J,RANGES)
         CALL H12 (2,JJ,J+1,M,A(1,J),1,T,B,1,MDB,NB,RANGES)
  160    IF (J.GE.N-1) GO TO 170
         CALL H12 (1,J+1,J+2,N,A(J,1),MDA,S(J,3),A(J+1,1),MDA,1,M-J,
     1   RANGES)
  170    CONTINUE 
C 
C     COPY THE BIDIAGONAL MATRIX INTO THE ARRAY S() FOR QRBD. 
C 
      IF (N.EQ.1) GO TO 190 
         DO 180 J=2,N
         S(J,1)=A(J,J)
  180    S(J,2)=A(J-1,J)
  190 S(1,1)=A(1,1) 
C 
      NS=N
      IF (M.GE.N) GO TO 200 
      NS=M+1
      S(NS,1)=ZERO
      S(NS,2)=A(M,M+1)
  200 CONTINUE
C 
C     CONSTRUCT THE EXPLICIT N BY N PRODUCT MATRIX, W=Q1*Q2*...*QL*I
C     IN THE ARRAY A(). 
C 
         DO 230 K=1,N 
         I=N+1-K
         IF(I.GT.MIN(M,N-2)) GO TO 210
         CALL H12 (2,I+1,I+2,N,A(I,1),MDA,S(I,3),A(1,I+1),1,MDA,N-I,
     1   RANGES)
  210    DO 220 J=1,N 
  220    A(I,J)=ZERO
  230    A(I,I)=ONE 
C
C         COMPUTE THE SVD OF THE BIDIAGONAL MATRIX
C 
      CALL QRBD (IPASS,S(1,1),S(1,2),NS,A,MDA,N,B,MDB,NB,RANGES)
C 
      GO TO (240,310), IPASS
  240 CONTINUE
      IF (NS.GE.N) GO TO 260
      NSP1=NS+1 
         DO 250 J=NSP1,N
  250    S(J,1)=ZERO
  260 CONTINUE
      IF (N.EQ.NN) RETURN 
      NP1=N+1 
C               MOVE RECORD OF PERMUTATIONS 
C               AND STORE ZEROS 
         DO 280 J=NP1,NN
         S(J,1)=A(1,J)
         DO 270 I=1,N 
  270    A(I,J)=ZERO
  280    CONTINUE 
C          PERMUTE ROWS AND SET ZERO SINGULAR VALUES. 
         DO 300 K=NP1,NN
         I=S(K,1) 
         S(K,1)=ZERO
         DO 290 J=1,NN
         A(K,J)=A(I,J)
  290    A(I,J)=ZERO
         A(I,K)=ONE 
  300    CONTINUE 
C          END.. SPECIAL FOR ZERO ROWS AND COLUMNS
      RETURN
  310 IERROR=5
      RETURN
      END 
C++++++++++++++++ SINGLE PRECISION VERSION 2SP (AUG 1982) ++++++++++++++
C     SUBROUTINE QRBD (IPASS,Q,E,NN,V,MDV,NRV,C,MDC,NCC,RANGES)
C     BASED ON C.L.LAWSON AND R.J.HANSON, 
C     'SOLVING LEAST SQUARES PROBLEMS', PRENTICE-HALL, 1974 
C          QR ALGORITHM FOR SINGULAR VALUES OF A BIDIAGONAL MATRIX. 
C 
C     THE BIDIAGONAL MATRIX 
C 
C                       (Q1,E2,0...    )
C                       (   Q2,E3,0... )
C                D=     (       .      )
C                       (         .   0)
C                       (           .EN)
C                       (          0,QN)
C 
C                 IS PRE AND POST MULTIPLIED BY 
C                 ELEMENTARY ROTATION MATRICES
C                 RI AND PI SO THAT 
C 
C                 RK...R1*D*P1**(T)...PK**(T) = DIAG(S1,...,SN) 
C 
C                 TO WITHIN WORKING ACCURACY. 
C 
C  1. EI AND QI OCCUPY E(I) AND Q(I) AS INPUT.
C 
C  2. RM...R1*C REPLACES 'C' IN STORAGE AS OUTPUT.
C 
C  3. V*P1**(T)...PM**(T) REPLACES 'V' IN STORAGE AS OUTPUT.
C 
C  4. SI OCCUPIES Q(I) AS OUTPUT. 
C 
C  5. THE SI'S ARE NONINCREASING AND NONNEGATIVE. 
C 
C     THIS CODE IS BASED ON THE PAPER AND 'ALGOL' CODE..
C REF.. 
C  1. REINSCH,C.H. AND GOLUB,G.H. 'SINGULAR VALUE DECOMPOSITION 
C     AND LEAST SQUARES SOLUTIONS' (NUMER. MATH.), VOL. 14,(1970).
C 
C  RANGES IS 2 OR 3 ORDERS OF MAGNITUDE SMALLER THAN BIG, WHERE BIG IS
C      THE LARGEST NUMBER THAT DOES NOT OVERFLOW AND 1/BIG DOES NOT 
C      UNDERFLOW.  FOR THE DOUBLE PRECISION VERSION, BIG AND RANGES
C      ARE IN DOUBLE PRECISION.  FOR THE SINGLE PRECISION VERSION,
C      THEY ARE IN SINGLE PRECISION (AND THEREFORE RANGES=SRANGE).
C-----------------------------------------------------------------------
C  CALLS SUBPROGRAMS - G1, G2, DIFF 
C-----------------------------------------------------------------------
      SUBROUTINE QRBD (IPASS,Q,E,NN,V,MDV,NRV,C,MDC,NCC,RANGES)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     DOUBLE PRECISION ABS, AMAX1, C, CS, DENOM, DIFF, DNORM, E, F,!DP
C    A G, H, ONE, Q, RANGES,!DP
C    1 RNUMER, SN, SQRT, SQRTRG, T, TWO, V, X, Y, Z, ZERO!DP
      LOGICAL WNTV, HAVERS, FAIL, LDUM
      DIMENSION Q(NN),E(NN),V(MDV,*),C(MDC,*)
C     AMAX1(ZERO,ONE)=DMAX1(ZERO,ONE)!DP
C     ABS(ONE)=DABS(ONE)!DP 
C     SQRT(ONE)=DSQRT(ONE)!DP 
      ZERO=0.D0 
C     ZERO=0.D0!DP
      ONE=1.D0
C     ONE=1.D0!DP 
      TWO=2.D0
C     TWO=2.D0!DP 
      SQRTRG=SQRT(RANGES)
C 
      N=NN
      IPASS=1 
      IF (N.LE.0) RETURN
      N10=10*N
      WNTV=NRV.GT.0 
      HAVERS=NCC.GT.0 
      FAIL=.FALSE.
      NQRS=0
      E(1)=ZERO 
      DNORM=ZERO
           DO 10 J=1,N
   10      DNORM=MAX(ABS(Q(J))+ABS(E(J)),DNORM)
           DO 200 KK=1,N
           K=N+1-KK 
C 
C     TEST FOR SPLITTING OR RANK DEFICIENCIES.. 
C         FIRST MAKE TEST FOR LAST DIAGONAL TERM, Q(K), BEING SMALL.
   20       IF(K.EQ.1) GO TO 50 
            IF(DIFF(DNORM+Q(K),DNORM)) 50,25,50 
C 
C     SINCE Q(K) IS SMALL WE WILL MAKE A SPECIAL PASS TO
C     TRANSFORM E(K) TO ZERO. 
C 
   25      CS=ZERO
           SN=-ONE
                DO 40 II=2,K
                I=K+1-II
                F=-SN*E(I+1)
                E(I+1)=CS*E(I+1)
                CALL G1 (Q(I),F,CS,SN,Q(I)) 
C         TRANSFORMATION CONSTRUCTED TO ZERO POSITION (I,K).
C 
                IF (.NOT.WNTV) GO TO 40 
                     DO 30 J=1,NRV
   30                CALL G2 (CS,SN,V(J,I),V(J,K))
C              ACCUMULATE RT. TRANSFORMATIONS IN V. 
C 
   40           CONTINUE
C 
C         THE MATRIX IS NOW BIDIAGONAL, AND OF LOWER ORDER
C         SINCE E(K) .EQ. ZERO..
C 
   50           DO 60 LL=1,K
                L=K+1-LL
                IF(DIFF(DNORM+E(L),DNORM)) 55,100,55
   55           IF(DIFF(DNORM+Q(L-1),DNORM)) 60,70,60 
   60           CONTINUE
C     THIS LOOP CAN'T COMPLETE SINCE E(1) = ZERO. 
C 
           GO TO 100
C 
C         CANCELLATION OF E(L), L.GT.1. 
   70      CS=ZERO
           SN=-ONE
                DO 90 I=L,K 
                F=-SN*E(I)
                E(I)=CS*E(I)
                IF(DIFF(DNORM+F,DNORM)) 75,100,75 
   75           CALL G1 (Q(I),F,CS,SN,Q(I)) 
                IF (.NOT.HAVERS) GO TO 90 
                     DO 80 J=1,NCC
   80                CALL G2 (CS,SN,C(I,J),C(L-1,J))
   90           CONTINUE
C 
C         TEST FOR CONVERGENCE..
  100      Z=Q(K) 
           IF (L.EQ.K) GO TO 170
C 
C         SHIFT FROM BOTTOM 2 BY 2 MINOR OF B**(T)*B. 
           X=Q(L) 
           Y=Q(K-1) 
           G=E(K-1) 
           H=E(K) 
C-----------------------------------------------------------------------
C  TO PREVENT ZERO-DIVIDE, TEST DENOMINATOR.
C-----------------------------------------------------------------------
           RNUMER=(Y-Z)*(Y+Z)+(G-H)*(G+H) 
           DENOM=TWO*H*Y
           LDUM=ABS(DENOM) .LE. ZERO
           IF (LDUM) F=.5*RANGES
           IF (.NOT.LDUM) F=RNUMER/DENOM
C-----------------------------------------------------------------------
C  TO HELP PREVENT OVERFLOW, SET G=ABS(F) FOR VERY LARGE F. 
C-----------------------------------------------------------------------
           G=ABS(F) 
           IF (G .LT. SQRTRG) G=SQRT(ONE+G**2)
           IF (F.LT.ZERO) GO TO 110 
           T=F+G
           GO TO 120
  110      T=F-G
  120      F=((X-Z)*(X+Z)+H*(Y/T-H))/X
C 
C         NEXT QR SWEEP.. 
           CS=ONE 
           SN=ONE 
           LP1=L+1
                DO 160 I=LP1,K
                G=E(I)
                Y=Q(I)
                H=SN*G
                G=CS*G
                CALL G1 (F,H,CS,SN,E(I-1))
                F=X*CS+G*SN 
                G=-X*SN+G*CS
                H=Y*SN
                Y=Y*CS
                IF (.NOT.WNTV) GO TO 140
C 
C              ACCUMULATE ROTATIONS (FROM THE RIGHT) IN 'V' 
                     DO 130 J=1,NRV 
  130                CALL G2 (CS,SN,V(J,I-1),V(J,I))
  140           CALL G1 (F,H,CS,SN,Q(I-1))
                F=CS*G+SN*Y 
                X=-SN*G+CS*Y
                IF (.NOT.HAVERS) GO TO 160
                     DO 150 J=1,NCC 
  150                CALL G2 (CS,SN,C(I-1,J),C(I,J))
C              APPLY ROTATIONS FROM THE LEFT TO 
C              RIGHT HAND SIDES IN 'C'..
C 
  160           CONTINUE
           E(L)=ZERO
           E(K)=F 
           Q(K)=X 
           NQRS=NQRS+1
           IF (NQRS.LE.N10) GO TO 20
C          RETURN TO 'TEST FOR SPLITTING'.
C 
           FAIL=.TRUE.
C     ..
C     CUTOFF FOR CONVERGENCE FAILURE. 'NQRS' WILL BE 2*N USUALLY. 
  170      IF (Z.GE.ZERO) GO TO 190 
           Q(K)=-Z
           IF (.NOT.WNTV) GO TO 190 
                DO 180 J=1,NRV
  180           V(J,K)=-V(J,K)
  190      CONTINUE 
C         CONVERGENCE. Q(K) IS MADE NONNEGATIVE.. 
C 
  200      CONTINUE 
      IF (N.EQ.1) RETURN
           DO 210 I=2,N 
           IF (Q(I).GT.Q(I-1)) GO TO 220
  210      CONTINUE 
      IF (FAIL) IPASS=2 
      RETURN
C     ..
C     EVERY SINGULAR VALUE IS IN ORDER..
  220      DO 270 I=2,N 
           T=Q(I-1) 
           K=I-1
                DO 230 J=I,N
                IF (T.GE.Q(J)) GO TO 230
                T=Q(J)
                K=J 
  230           CONTINUE
           IF (K.EQ.I-1) GO TO 270
           Q(K)=Q(I-1)
           Q(I-1)=T 
           IF (.NOT.HAVERS) GO TO 250 
                DO 240 J=1,NCC
                T=C(I-1,J)
                C(I-1,J)=C(K,J) 
  240           C(K,J)=T
  250      IF (.NOT.WNTV) GO TO 270 
                DO 260 J=1,NRV
                T=V(J,I-1)
                V(J,I-1)=V(J,K) 
  260           V(J,K)=T
  270      CONTINUE 
C         END OF ORDERING ALGORITHM.
C 
      IF (FAIL) IPASS=2 
      RETURN
      END 

C++++++++++++++++ SINGLE PRECISION VERSION 2SP (AUG 1982) ++++++++++++++
C  SUBROUTINE G1. 
      SUBROUTINE G1 (A,B,COS,SIN,SIG) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA RANGEIN/1.D-80/
C     BASED ON C.L.LAWSON AND R.J.HANSON, 
C     'SOLVING LEAST SQUARES PROBLEMS', PRENTICE-HALL, 1974 
C 
C 
C     COMPUTE ORTHOGONAL ROTATION MATRIX..
C     COMPUTE.. MATRIX   (C, S) SO THAT (C, S)(A) = (SQRT(A**2+B**2)) 
C                        (-S,C)         (-S,C)(B)   (   0          )
C     COMPUTE SIG = SQRT(A**2+B**2) 
C        SIG IS COMPUTED LAST TO ALLOW FOR THE POSSIBILITY THAT 
C        SIG MAY BE IN THE SAME LOCATION AS A OR B .
C 
C     DOUBLE PRECISION A, ABS, B, COS, ONE, SIG, SIGN, SIN, SQRT,!DP
C    1 XR, YR, ZERO!DP
C     ABS(A)=DABS(A)!DP 
C     SQRT(A)=DSQRT(A)!DP 
C     SIGN(A,B)=DSIGN(A,B)!DP 
      ZERO=0.D0 
C     ZERO=0.D0!DP
      ONE=1.D0
C     ONE=1.D0!DP 
      IF(ABS(A).LE.RANGEIN.AND.ABS(B).LE.RANGEIN) GOTO 30
      IF (ABS(A).LE.ABS(B)) GO TO 10
      XR=B/A         
      YR=SQRT(ONE+XR**2)           
      COS=SIGN(ONE/YR,A)
      SIN=COS*XR
      SIG=ABS(A)*YR 
      RETURN
   10 IF(ABS(B).LE.RANGEIN) GOTO 30
      XR=A/B         
      YR=SQRT(ONE+XR**2)        
      SIN=SIGN(ONE/YR,B)
      COS=SIN*XR
      SIG=ABS(B)*YR 
      RETURN
   30 SIG=ZERO
      COS=ZERO
      SIN=ONE 
      RETURN
      END 
C++++++++++++++++ SINGLE PRECISION VERSION 2SP (AUG 1982) ++++++++++++++
C  SUBROUTINE G2. 
      SUBROUTINE G2    (COS,SIN,X,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BASED ON C.L.LAWSON AND R.J.HANSON, 
C     'SOLVING LEAST SQUARES PROBLEMS', PRENTICE-HALL, 1974 
C          APPLY THE ROTATION COMPUTED BY G1 TO (X,Y).
C     DOUBLE PRECISION COS, SIN, X, XR, Y!DP
      XR=COS*X+SIN*Y
      Y=-SIN*X+COS*Y
      X=XR
      RETURN
      END 
C
      SUBROUTINE TRICOV(MDA,A,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(MDA,*)
C                               T
C     BERECHNUNG DES PRODUKTES A * A
C     EINER OBEREN DREIECKSMATRIX

      DO I=N,1,-1
         DO J=N,I,-1
           S=0.
           DO L=1,J     
             S=S+A(L,I)*A(L,J)
           ENDDO
           A(I,J)=S
         ENDDO
       ENDDO
       RETURN
       END 
C
      SUBROUTINE COVTRI(MDA,A,N)
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(MDA,*)
      DOUBLE PRECISION S,SI,SJ
      DATA RANGES/1.D80/,RANGIN/1.D-80/,SRANGE/1.D25/
C 
C                                         T 
C     BERECHNUNG DER COVARIANZ-MATRIX (A*A )**(-1) AUS DER
C     OBEREN DREIECKSMATRIX A 
C 
C 
      DO 10 I=1,N 
      A(I,I)=1./(A(I,I)+RANGIN)
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
C
C
C     Unterhalb der Diagonalen wird aufgefüllt
C
      DO I=1,N
         DO J=I+1,N
           A(J,I)=A(I,J)
         END DO
      END DO
      RETURN
      END 
       
*------- AKINT

      subroutine AKINT(PUF,N,X_arr,Y_arr,IER,Merk,M,P,Abw)
      implicit double precision (a-h,o-z)
         dimension PUF(*), X_arr(*), Y_arr(*), Abw(*)

*-------------------------------------------------------------
*
*     PUF   Arbeitsspeicher dim(PUF) = 4*N+5*(N+4)
*     N     Anzahl Stuetzstellen der vorgegebenen x-, y-Werte
*     X_arr Abszissenwerte der vorgegebenen Stuetzwerte (Anzahl N)
*     Y_arr Ordinatenwerte der vorgegebenen Stuetzwerte (Anzahl N)
*     IER   Fehlercode
*     MERK  INTEGER            0 OHNE AUSGLEICH; 1 MIT AUSGLEICH
*     M     INTEGER            1/2 EINFLUSSBREITE DES AUSGLEICHSINTERVALLS
*                              3 < M < 5
*     P     GRAD DER WERTIGKEITSKURVE 0.5 < P < 2.
*     ABW   MAXIMALE ABWEICHUNGEN (ANZAHL = N), FALLS MERK=1
*
*-------------------------------------------------------------



       N1=N+1
       N2=2*N+1
       N3=3*N+1
       N4=N3+N
       N5=N4+N+4
       N6=N5+N+4
       N7=N6+N+4
       N8=N7+N+4
       IER=0
       call AKIMAS(N,X_arr,Y_arr,Merk,M,P,Abw,
     -              PUF(1),PUF(N1),PUF(N2),PUF(N3),
     _              PUF(N4),PUF(N5),PUF(N6),PUF(N7),PUF(N8),IER)


       if (IER.NE.0) then
          return
       endif

      RETURN
      end

*------- Ende AKINT

*------- AKIMAS

      subroutine AKIMAS(NN, X_arr, Y_arr, Merk, M, P, Abw_arr, A_arr,
     -                   B_arr, C_arr, D_arr,y_neu_arr,x_neu_arr,
     _                   abw_neu_arr,t,st,IER)
         implicit double precision (a-h,o-z)
          dimension X_arr(*), Y_arr(*), Abw_arr(*), A_arr(*),
     -              B_arr(*), C_arr(*), D_arr(*), T(*),
     -              St(*), Y_neu_arr(*),x_neu_arr(*),abw_neu_arr(*)
          eps = 0.000001


*------------------------------------------------------------
*             Die Procedur Akimas interpoliert aus Stuetzstellen
*             in der x,y-Ebene Polynome dritten Grades, die zwischen
*             jeweils zwei Punkten definiert werden.
*
*              Eingabeparameter
*              ----------------
*              NN      integer       Anzahl der Stuetzstellen
*              X_arr   real-array    Abszissenwerte
*              XA_arr  real-array    Abszissenwerte zu denen
*                                    Ordinatenwerte gesucht
*              Y_arr   real-array    Ordinatenwerte
*              Merk    integer       0 = ohne Ausgl., 1 = mit Ausgl.
*              M       integer       1/2 Einflussbreite des Ausgl.-
*                                    Intervalls
*              P       real          Grad der Wertigkeitskurve
*              Abw_arr real-array    max. Absolutabweichung, negative
*                                    Werte heben Kontrolle auf
*                      M,P,ABW_ARR   NUR FUER MERK=1 ERFORDERLICH
*
*              Ausgabeparameter
*              ----------------
*              A_arr   real-array    Diese vier Array beinhalten
*              B_arr   real-array    die Polynomkonstanten
*              C_arr   real-array
*              D_arr   real-array
*                      DIESE WERTE SIND NACH AUFRUF DES PROGRAMMS IN PUF  
*                      GESPEICHERT A=PUF(1 - N)   B=PUF(N+1 - 2*N) USW
*------------------------------------------------------------
          N=NN 

*              falls ja, dann ab hier ausgleichen
          if (Merk .EQ. 1) then
             Do i=1,n
                x_neu_arr(i)=x_arr(i)
                t(i)=Y_arr(i)  
             enddo
             call extrap(N, X_neu_arr, t, St,0,IER)
             if (IER.NE.0) then
                return
             endif

             do I = 1,N
                Abw_neu_arr(N+3-I) = Abw_arr(N+1-I)
             enddo

             Abw_neu_arr(1)   = -1
             Abw_neu_arr(2)   = -1
             Abw_neu_arr(N+3) = -1
             Abw_neu_arr(N+4) = -1

             N = N + 4


*----------    normierten X-Abstand definieren

             if ((N-1).EQ.0) then
                 IER = 1
                 return
             endif

             X_norm = (X_neu_arr(N)-X_neu_arr(1))/(N-1)

             if (X_norm.EQ.0) then
                 IER = 1
                 return
             endif

*----------    Schleife ueber alle Stuetzstellen

             do I = 1,N
                Sum1   = 0
                Sum2   = 0
                M_loop = M

10              if (((I-M_loop).LT.1) .OR. ((I+M_loop).GT.N)) then
                   M_loop = M_loop -1
                   goto 10
                endif

*-----------     Summenschleife

                do J = I-M_loop,I+M_loop
                   if (J.EQ.I) then
                      Term1 = 1
                   else
                      Term1 = 1-Power(P,(1.0/M)*(abs(X_neu_arr(I)-
     -                        X_neu_arr(J))/X_norm))
                   endif
                   if (Term1 .LT. 0) then
                      Term1 = 0
*                          da es keine negative Wertigkeit gibt
                   endif
                   Sum1 = Sum1 + Term1 * t(J)
                   Sum2 = Sum2 + Term1
                enddo

                if (Sum2.EQ.0) then
                   IER = 1
                   return
                endif

                Y_neu_arr(I) = Sum1/Sum2
*                        dies ist die korrigierte Stuetzstelle

*-----------      Kontrolle, ob innerhalb des gewaehlten
*-----------      Zuverlaessigkeitsbereich

                if (Abw_neu_arr(i) .GE. 0) then
                   if (Y_neu_arr(I).LT.(t(i)-Abw_neu_arr(I))) then
                       Y_neu_arr(I) = t(I)-Abw_neu_arr(I)
                   endif

                   if (Y_neu_arr(I).GT.(t(I)+Abw_neu_arr(I))) then
                       Y_neu_arr(I) = t(I)+Abw_neu_arr(I)
                   endif
                endif
             enddo

*----------    Rueckindizierung 2 Werte nach unten

             N = N - 4
             do I = 1,N
                X_neu_arr(I)   = X_neu_arr(I+2)
                Y_neu_arr(I)   = Y_neu_arr(I+2)
                Abw_neu_arr(I) = Abw_neu_arr(I+2)
             enddo
          else
             do i=1,n
                x_neu_arr(i)   = x_arr(i)
                y_neu_arr(i)   = y_arr(i)
             enddo
          endif
*----------  Ende der Ausgleichung

*----------  hier beginnt die eigentliche Interpolation

           call Extrap(N, X_neu_arr, Y_neu_arr, St,1,IER)
           if (IER.NE.0) then
              return
           endif

*     Intervall bestimmen, in dem  der vorgegebene x-Wert
*     liegt


**************************************************************
*       Berechnung der Steigung des Intervalls, in dem
*        der vorgegebene x-Wert liegt

*----------   Berechnung aller Polynomsteigungen


           do I = 3,N+2
              amax = MAX(abs(St(I+1)),abs(St(I)),abs(St(I-1)),
     -                abs(St(I-2)))
              if (abs(St(I+1)-St(I))+
     -            abs(St(I-1)-St(I-2)).LT.eps) then
                 T(I) = 0.5*(St(I+1)+St(I))
              else
                 T(I) = (abs(St(I+1) - St(I))   * St(I-1) +
     -                   abs(St(I-1) - St(I-2)) * St(I))  /
     -                  (abs(St(I+1) - St(I))   +
     -                   abs(St(I-1) - St(I-2)))
              endif
           enddo


*--------  Rueckindizierung 2 Werte nach unten

           do I = 1,N
              T(I)     = T(I+2)
              St(I)    = St(I+2)
              y_neu_arr(i)=y_neu_arr(i+2)
           enddo

*---------   Berechnug der Polynomkoeffizienten

           do I = 1,N-1
              A_arr(I) = Y_neu_arr(I)
              B_arr(I) = T(I)

              anenn= X_arr(I+1)-X_arr(I)
              if (anenn.EQ.0.) then
                 IER = 1
                 return
              endif

              C_arr(I) = (3*St(I)-2*T(I)-T(I+1)) / anenn

              anenn=(X_arr(I+1)-X_arr(I))*(X_arr(I+1)-X_arr(I))
              if (anenn.EQ.0.) then
                 IER = 1
                 return
              endif

              D_arr(I) = (T(I)+T(I+1)-2*St(I)) / anenn
           enddo
        return
      end

*-----------  Ende AKIMAS

*-----------  Extrap

          subroutine Extrap(N, X_arr, Y_arr, St,MLIN,IER)
             implicit double precision (a-h,o-z)
             dimension X_arr(*), Y_arr(*), St(*)

*--------------------------------------------------------------
*               Der erste Teil ist die Interpolation.
*               Die Stuetzstellen werden um 2 Indizes nach oben
*               verschoben.
*--------------------------------------------------------------
*              mlin = 1     Endpunkte quadratisch extrapoliert
*              mlin = 0     Endpunkte linear extrapoliert

             do I = 1,N
                X_arr(N+3-I) = X_arr(N+1-I)
                Y_arr(N+3-I) = Y_arr(N+1-I)
             enddo

*--------       Berechnung der extrapolierten X-Werte rechts und links

             X_arr(2) = X_arr(3) + X_arr(4) - X_arr(5)
             X_arr(1) = X_arr(2) + X_arr(3) - X_arr(4)

             X_arr(N+3) = X_arr(N+2) + X_arr(N+1) - X_arr(N)
             X_arr(N+4) = X_arr(N+3) + X_arr(N+2) - X_arr(N+1)

*--------       Berechnung der Steigung der Strecken

             do I = 3,N+1
                hilf = X_arr(I+1)-X_arr(I)
                if (hilf.EQ.0) then
                   IER = 1
                   return
                else
                   St(I) = (Y_arr(I+1)-Y_arr(I)) / hilf
                endif
             enddo



*--------------------------------------------------------
*               Berechnung der extrapolierten Y-Werte und
*               Steigungen rechts und links
*--------------------------------------------------------

             if(mlin.eq.1) then
               Y_arr(2)=(X_arr(3)-X_arr(2))*(St(4)-2*St(3))+Y_arr(3)
             else
               Y_arr(2)=(X_arr(3)-X_arr(2))*(-St(3))+Y_arr(3)
             endif
             hilf = X_arr(3) - X_arr(2)
             if (hilf.EQ.0) then
                 IER = 1
                 return
             else
                 St(2)  = (Y_arr(3)-Y_arr(2))/hilf
             endif
             if(mlin.eq.1) then
               Y_arr(1)=(X_arr(2)-X_arr(1))*(St(3)-2*St(2))+Y_arr(2)
             else
               Y_arr(1)=(X_arr(2)-X_arr(1))*(-St(2))+Y_arr(2)
             endif

             hilf = X_arr(2) - X_arr(1)
             if (hilf.EQ.0) then
                IER = 1
                return
             else
                St(1)   =(Y_arr(2)-Y_arr(1))/hilf
             endif

             if(mlin.eq.1) then 
               Y_arr(N+3)=(2*St(N+1)-St(N))*(X_arr(N+3)-X_arr(N+2))
     -                 +Y_arr(N+2)
             else
               Y_arr(N+3)=(St(N+1))*(X_arr(N+3)-X_arr(N+2))
     -                 +Y_arr(N+2)
             endif

             hilf = X_arr(N+3) - X_arr(N+2)
             if (hilf.EQ.0) then
                IER = 1
                return
             else
                 St(N+2)   =(Y_arr(N+3)-Y_arr(N+2))/ hilf
             endif
             if(mlin.eq.1) then
               Y_arr(N+4)=(2*St(N+2)-St(N+1))*(X_arr(N+4)-X_arr(N+3))
     -                 +Y_arr(N+3)
             else
               Y_arr(N+4)=(St(N+2))*(X_arr(N+4)-X_arr(N+3))
     -                 +Y_arr(N+3)
             endif

             hilf = X_arr(N+4) - X_arr(N+3)
             if (hilf.EQ.0) then
                 IER = 1
                 return
             else
                 St(N+3)   =(Y_arr(N+4)-Y_arr(N+3))/hilf
             endif
          return
          end

*-----------  Ende Extrap


         SUBROUTINE INTAKI(PUF,N,X,M,XA,YA)
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
         DIMENSION PUF(*),X(*),XA(*),YA(*)
C
*-------------------------------------------------------------
*
*     PUF   Arbeitsspeicher dim(PUF) = 4*N+5*(N+4)
*     N     Anzahl Stuetzstellen der vorgegebenen x-, y-Werte
*     X     Abszissenwerte der vorgegebenen Stuetzwerte (Anzahl N)
*     M     Anzahl gesuchte Ordinatenwerte (YA)
*     XA    Stuetzstellen fuer gesuchte YA-Werte
*     YA    Gesuchte Ordinatenwerte
*-------------------------------------------------------------
*

*
*-------------------------------------------------------------




         N1=N
         N2=2*N
         N3=3*N

      IF(X(1).LT.X(N)) THEN
         V=1.
      ELSE
         V=-1.
      ENDIF
      DO K=1,M
        IF(V*XA(K).LE.V*X(1)) THEN
           I=1
           GOTO 100
        ENDIF
        IF(V*XA(K).GT.V*X(N)) THEN
           I=N-1
           GOTO 100
        ENDIF
        do J = 1,N-1
            IF(V*XA(K).GT.V*X(J).AND.V*XA(K).LE.V*X(J+1)) GOTO 90
        ENDDO
        J=N-1
 90     I=J
 100           YA(K) = PUF(I)
     -                 + PUF(N1+I) * Power(1.D0,(XA(k)-X(I)))
     -                 + PUF(N2+I) * Power(2.D0,(XA(k)-X(I)))
     -                 + PUF(N3+I) * Power(3.D0,(XA(k)-X(I)))
      enddo
      RETURN
      END

      double precision function Power(P,X)
      implicit double precision (a-h,o-z)
         integer P_int
         if (X .EQ. 0.) then
            Power = 0
         endif
         if (X .GT. 0.) then
            Power = exp(P*log(X))
         endif
         if (X .LT. 0.) then
            P_int = nint(P)
            P_hil = exp(P_int*log(abs(X)))

            if (mod(P_int,2).NE.0) then
               Power = (-1.) * P_hil
            else
               Power = P_hil
            endif
         endif
      return
      end



      SUBROUTINE SPEXI(PUF,N,XA1,XE1,Y,NA,XA2,XE2,YA,IER) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     INTERPOLATION MIT EXPONENTIELLEM SPLINE 
C 
C     PUF - ARBEITSSPEICHER FUER UP SPEXI; 8*N + NA
C     N - ANZAHL PUNKTE  E I N G A B E
C     XA1 - ABSZISSEN-ANFANGSWERT  E I N G A B E
C     XE1 - ABSZISSEN-ENDWERT  E I N G A B E
C     Y  - ORDINATEN-WERTE (VEKTOR)  E I N G A B E
C     NA - ANZAHL PUNKTE  A U S G A B E 
C     XA2 - ABSZISSEN-ANFANGSWERT  A U S G A B E
C     XE2 - ABSZISSEN-ENDWERT  A U S G A B E
C     YA - ORDINATEN-WERTE (VEKTOR)  A U S G A B E
C 
C 
      DIMENSION Y(*),PUF(*),YA(*) 
C 
C 
      IER=0 
C    
C     AUFBAU X-VEKTOR   PUF(1)...PUF(N)
C
C
      H1=(XE1-XA1)/FLOAT(N-1) 
      PUF(1)=XA1
      DO 6 I=2,N
    6 PUF(I)=PUF(I-1) + H1
C
C     AUFBAU XA-VEKTOR   PUF(N+1)....PUF(N+NA)
      H1=(XE2-XA2)/FLOAT(NA-1) 
      PUF(N+1)=XA2
      DO 16 I=2,NA
   16 PUF(N+I)=PUF(N+I-1) + H1
      NN=N+NA+1
      CALL EXSPLI(PUF(NN),N,PUF(1),Y,NA,PUF(N+1),YA,IER)
      RETURN
      END 
      SUBROUTINE EXSPLI(PUF,N,X,Y,NA,XA,YA,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PUF(*),X(*),Y(*),XA(*),YA(*)
C
C     BERECHNUNG VON EXPONENTIELLEN SPLINES
C
C     PUF   ARBEITSSPEICHER DIM(PUF)=7*N
C     N     ANZAHL STUETZSTELLEN DER VORGEGEBENEN X,Y-WERTE
C     X     ABSZISSENWERTE DER VORGEGEBENEN STUETZWERTE (ANZAHL N)
C     Y     ORDINATENWERTE DER VORGEGEBENEN STUETZWERTE (ANZAHL N)
C     NA    ANZAHL DER GESUCHTEN ORDINATENWERTE
C     XA    ABSZISSENWERTE DER GESUCHTEN YA-WERTE (ANZAHL NA)
C     YA    GESUCHTE ORDINATENWERT (ANZAHL NA)
C     IER   FEHLERCODE
C           1        ANZAHL STUETZSTELLEN KLEINER 3
C           2        X-WERTE NICHT AUFSTEIGEND (X(I-1) > X(I))
C           
C     AUFTEILUNG DES ARBEITSSPEICHERS  P U F
C     --------------------------------------
C        1 -  N     P (TENSION PARAMETER LAMBDA)
C      N+1 - 2N     D (SECOND DERIVATIVES)
C     2N+1 - 3N     DQ (SECOND DIFFERENCE QUOTIENT) 
C     3N+1 - 4N     H (INTERVAL LENGTH) 
C     4N+1 - 5N     HP (PRODUCTS ABS(LAMBDA*H) )
C     5N+1 - 6N     R (ZWISCHENSPEICHER)
C     6N+1 - 7N     Q (ZWISCHENSPEICHER)
C
      ICC=1
      N1=N+1
      N2=2*N + 1
      N3=3*N + 1
      N4=4*N + 1
      N5=5*N + 1
      N6=6*N + 1
      CALL EXSPL(PUF(1),PUF(N1),PUF(N2),PUF(N3),PUF(N4),PUF(N5), 
     &   PUF(N6),N,X,Y,NA,XA,YA,IER,ICC)          
      RETURN
      END
      SUBROUTINE EXSPLA(PUF,N,X,Y,NA,XA,YA,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PUF(*),X(*),Y(*),XA(*),YA(*)
C
C     BERECHNUNG VON EXPONENTIELLEN SPLINES
C
C     PUF   ARBEITSSPEICHER DIM(PUF)=7*N
C     N     ANZAHL STUETZSTELLEN DER VORGEGEBENEN X,Y-WERTE
C     X     ABSZISSENWERTE DER VORGEGEBENEN STUETZWERTE (ANZAHL N)
C     Y     ORDINATENWERTE DER VORGEGEBENEN STUETZWERTE (ANZAHL N)
C     NA    ANZAHL DER GESUCHTEN ORDINATENWERTE
C     XA    ABSZISSENWERTE DER GESUCHTEN YA-WERTE (ANZAHL NA)
C     YA    GESUCHTE ORDINATENWERT (ANZAHL NA)
C     IER   FEHLERCODE
C           1        ANZAHL STUETZSTELLEN KLEINER 3
C           2        X-WERTE NICHT AUFSTEIGEND (X(I-1) > X(I))
C           
C     AUFTEILUNG DES ARBEITSSPEICHERS  P U F
C     --------------------------------------
C        1 -  N     P (TENSION PARAMETER LAMBDA)
C      N+1 - 2N     D (SECOND DERIVATIVES)
C     2N+1 - 3N     DQ (SECOND DIFFERENCE QUOTIENT) 
C     3N+1 - 4N     H (INTERVAL LENGTH) 
C     4N+1 - 5N     HP (PRODUCTS ABS(LAMBDA*H) )
C     5N+1 - 6N     R (ZWISCHENSPEICHER)
C     6N+1 - 7N     Q (ZWISCHENSPEICHER)
C
      ICC=2
      N1=N+1
      N2=2*N + 1
      N3=3*N + 1
      N4=4*N + 1
      N5=5*N + 1
      N6=6*N + 1
      CALL EXSPL(PUF(1),PUF(N1),PUF(N2),PUF(N3),PUF(N4),PUF(N5), 
     &   PUF(N6),N,X,Y,NA,XA,YA,IER,ICC)          
      RETURN
      END
      SUBROUTINE EXSPL(P,D,DQ,H,HP,R,Q,N,X,Y,NA,XA,YA,IER,ICC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P(N),D(N),DQ(N),H(N),HP(N),R(N),Q(N),X(N),Y(N),
     &   XA(NA),YA(NA)
      DOUBLE PRECISION A1
C
C
      ICO=ICC
      IF(ICO.EQ.2) THEN
         DO 5 I=1,N
    5    P(I)=4.
      ELSE
         DO 6 I=1,N
         P(I)=0.
    6    CONTINUE
      ENDIF
C
C
C     COMPUTATION OF THE COEFFICIENTS OF THE EXPONENTIAL SPLINE
C     ---------------------------------------------------------
C
C
C      COMPUTATION OF THE ELEMENTS OF THE TRIDIAGONAL SYSTEM
C
   10 IF (N.GE.4) GOTO 120
      IER=1
      GOTO 500
  120 N1=N - 1
      U=Y(1)
      DO 130 I=2,N
      I1=I-1
      H(I1)=X(I) - X(I1)
      IF (H(I1).GE.0) GOTO 140
      IER=2
      GOTO 500
  140 V=Y(I)
      HP(I1)=ABS(H(I1)*P(I1))
      IF (H(I1).EQ.0.0) THEN
         D(I)=V
      ELSE
         D(I)=(V-U)/H(I1)
         U=V
      ENDIF
      Q(I)=QQ(HP(I1))*H(I1)
      R(I)=RR(HP(I1))*H(I1)
  130 CONTINUE
C
C
C     SOLUTION OF THE TRIDIAGONAL SYSTEM WITH
C     DIAGONAL: Q(I)=Q(I+1) I=1,N1
C     OFF-DIAGONAL: R(I)  I=2,N1
C     RIGHT HAND SIDE: D(I+1)-D(I)  I=1,N1
C     SECOND DIFFERENCE QUOTIENT: DQ(I)=D(I+1)-D(I) I=1,N1
C
      D(1)=0.0
      U=0.0
      DO 200 I=2,N1
      Q(I)=Q(I) + Q(I+1) - U*R(I)
      DQ(I)=D(I+1) - D(I)
      D(I)=DQ(I) - U*D(I-1)
  200 U=R(I+1)/Q(I)
      D(N)=0.
      I=N1
  220 D(I)=(D(I) - R(I+1)*D(I+1))/Q(I)
      IF (I.EQ.2) GOTO 195
      I=I - 1
      GOTO 220
C
C
C
  195 IF (ICO.GE.2) GOTO 300
      ICO=ICO + 1
C
C
C     DETERMINATION OF THE TENSION  P A R A M E T E R S   P
C     -----------------------------------------------------
C
      IF (H(1).LE.0.0) GOTO 250
      Y11=Y(2)
      GOTO 260
  250 Y11=Y(1)
  260 Y1=ABS(Y11)
      D1=D(1)*DQ(1)
      DO 270 I=2,N-2
      I1=I + 1
      D2=D(I1)*DQ(I1)
      Y2=ABS(Y(I1))
      IF (Y2.GT.Y1) Y1=Y2
      IF (D1*D2.LE.0) GOTO 280
      P(I)=0.0
      GOTO 285
  280 IF (Y1.NE.0) GOTO 295
      P(I)=15./H(I)
      GOTO 285
  295 P(I)=(4. + 1./(0.1 + ABS(Y(I1)-Y11)/Y1))/H(I)
  285 Y11=Y(I1)
      Y1=Y2
      D1=D2
  270 CONTINUE
      P(1)=P(2)*H(1)/H(2)
      P(N-1)=P(N-2)*H(N-1)/H(N-2)
      GOTO 10
C
C
C     INTERPOLATION VON ZWISCHENPUNKTEN
C
  300 CONTINUE
      I=1
      DO 320 J=1,NA
      XX=XA(J)
  330 I1=I + 1
      IF (XX.LT.X(I1)) GOTO 350
C     INTERVALLWECHSEL
      IF (I.EQ.N-1) GOTO 350
      I=I1
      GOTO 330
C
C
C     EVALUATION OF THE EXPONENTIAL  S P L I N E  AT THE
C               A B S Z I S S A   XX
C               --------------------
C
  350 IF (I.LE.(N-1)) GOTO 420
      IER=3
C     GOTO 500
  420 T=(XX-X(I))/H(I)
      T1=1.0 - T
      IF (T.LE.2.0.AND.T.GE.-1.) GOTO 430
C     IER=4
C     GOTO 500
  430 HH=H(I)**2
      EX=T*Y(I1)+T1*Y(I)+HH*(D(I1)*EEX(HP(I),T)+D(I)*EEX(HP(I),T1))
      YA(J)=EX
  320 CONTINUE
C
  500 RETURN
      END
      DOUBLE PRECISION FUNCTION PHI(X)
C
C
C     Approximation für SINH
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8) :: PHIDX
      DIMENSION A(4)
      DATA A/0.16666666666D-0,0.83333336379D-2,
     *       0.19840927713D-3,0.27713991169D-5/
      XX=X*X
      PHI=((A(4)*XX+A(3))*XX+A(2))*XX+A(1)
      RETURN
      ENTRY PHIDX(X)
      XX=X*X
      PHIDX=((6.*A(4)*XX+4.*A(3))*XX+2.*A(2))*X
      RETURN
      END
      DOUBLE PRECISION FUNCTION PP(XQ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      X=SQRT(ABS(XQ))
      PP=1.+XQ*PHI(X)
      RETURN
      END
      DOUBLE PRECISION FUNCTION QQ(Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      X=ABS(Y)
      IF(X.GT.0.5) THEN
            EX=0.
            IF(X.LT.30.) EX=EXP(-2.*X)
            Q=1.-EX
            QQ=(1.-Q/X+EX)/(X*Q)
      ELSE
            XQ=(0.5*X)**2
            PPP=PP(XQ)
            PH=PHI(X)
            QQ=(0.5*PPP**2-PH)/(1.+PH*X**2)
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION RR(Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      X=ABS(Y)
      IF(X.GT.0.5) THEN
            EX=0.
            IF(X.LT.30.) EX=EXP(-X)
            Q=1.-EX**2
            RR=(Q/X-2.*EX)/(X*Q)
      ELSE
            RR=PHI(X)/(1.+PHI(X)*X**2)
      ENDIF
      RETURN
      END
      DOUBLE PRECISION  FUNCTION EEX(Y,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     0<=T<=1.
      X=ABS(Y)
      IF(X.GT.0.5) THEN
            EX=0.
            IF(X.LT.30.) EX=EXP(-2.*X)
            EXT=0.
            XT=ABS(X*T)
            IF(XT.LT.30) EXT=EXP(-2.*XT)
            EX1=0.
            X1=ABS(X*(1.-T))
            IF(X1.LT.30) EX1=EXP(-X1)
            EEX=(EX1*(1.-EXT)/(1.-EX)-T)/(X**2)
       ELSE
            XT=ABS(X*T)  
            EEX=T*(PHI(XT)*T**2-PHI(X))/(1.+PHI(X)*X**2)
       ENDIF
      RETURN
      END

      DOUBLE PRECISION  FUNCTION EEXDT(Y,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     0<=T<=1.
      X=ABS(Y)
      IF(X.GT.0.5) THEN
            EX=0.
            IF(X.LT.30.) EX=EXP(-2.*X)
            EXT=0.
            XT=ABS(X*T)
            IF(XT.LT.30) EXT=EXP(-2.*XT)
            EX1=0.
            X1=ABS(X*(1.-T))
            IF(X1.LT.30) EX1=EXP(-X1)
            EEXDT=(X*EX1*(1.+EXT)/(1.-EX)-1)/(X**2)
       ELSE
            XT=ABS(X*T)
            EEXDT=(3.*PHI(XT)*T**2+X*PHIDX(XT)*T**3-PHI(X))
     *            /(1.+PHI(X)*X**2)
       ENDIF
      RETURN
      END


C*********************************************************************
C*********************************************************************
C*********************************************************************
C*********************************************************************
C*********************************************************************
      SUBROUTINE INVTRI(MI,N,R,PRECIS,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(MI,1)
      IER=0
      DO 10 I=1,N
      IF(ABS(R(I,I)).LE.PRECIS) GOTO 900
      R(I,I)=1./R(I,I)
  10  CONTINUE
      DO 20 I=1,N-1
      DO 20 J=I+1,N      
      SUMM=0.
      DO 30 L=I,J-1
      SUMM=SUMM+R(I,L)*R(L,J)
  30  CONTINUE
      R(I,J)=-R(J,J)*SUMM
  20  CONTINUE
      RETURN
  900 IER=7
      RETURN
      END
C*********************************************************************
C*********************************************************************
C*********************************************************************
C*********************************************************************
C*********************************************************************
C*********************************************************************
C*********************************************************************
C*********************************************************************
C*********************************************************************

C
C                           +-------+ 
C                           ! PLIGS ! 
C                           +-------+ 
      SUBROUTINE PLIGS(NDIM,N,M,NG,A,B,TOL,DETR,IER)         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     LOESUNG DES GLEICHUNGSSYSTEMS
C             A*X=B
C
C     NDIM     DIMENSION FUER A UND B IM RUFENDEN PROGRAMM
C     N        AKTUELLE ZEILEN UND SPALTENANZAHL VON A
C     M        ANZAHL DER RECHTEN SPALTENVEKTOREN ABGESPEICHERT IN MATRIX B
C     NG=N
C     A        MATRIX DER GLEICHUNGEN
C     B        MATRIX DER RECHTEN SEITEN B (EINGABE)
C              MATRIX DER LOESUNG X        (AUSGABE)
C     TOL      TOLERANZ FUER PIVOTSUCHE
C     DETR     RELATIVE DETERMINANTE (MASS FUER KONDITIONSZAHL DER MATRIX A)         
C     IER      0        KEIN FEHLER
C              1        FEHLER         
      DIMENSION A(NDIM,2),B(NDIM,1) 
      DOUBLE PRECISION SUU
      DATA RANGES/1.D80/
      SRR=1./SQRT(RANGES)
      DETR=1. 
      DO 5 I=1,N
      SUU=0.D0
      DO 6 J=1,N
      SUU=SUU+A(J,I)**2 
   6  CONTINUE
      DETR=DETR*SUU 
      IF(ABS(DETR).LT.SRR) DETR=0.
   5  CONTINUE
      SUU=1.D0
      IER=0 
      IF(NG.GT.N) GOTO 35 
      NY=NG-1 
      DO 64 J=1,NG
      JY=J+1
      BIGA=0. 
      DO 30 I=J,N 
      DO 30 K=J,N 
      IF(ABS(BIGA)-ABS(A(I,K))) 20,30,30
 20   BIGA=A(I,K) 
      KMAX=K
      IMAX=I
 30   CONTINUE
      SUU=SUU*BIGA
      IF(ABS(BIGA)-TOL) 35,35,40
 40   DO 52 K=1,N 
      SAV =A(K,J)
      A(K,J)=A(K,KMAX)
      A(K,KMAX)=SAV
  52  CONTINUE
      DO 50 K=J,N 
      SAV=A(J,K)
      A(J,K)=A(IMAX,K)
      A(IMAX,K)=SAV
      A(J,K)=A(J,K)/BIGA
 50   CONTINUE
      DO 51  K=1,M
      SAV=B(J,K)
      B(J,K)=B(IMAX,K)
      B(IMAX,K)=SAV
      B(J,K)=B(J,K)/BIGA
 51   CONTINUE
      IF(J-NG) 55,63,55 
 55   DO 65 K=JY,N
      IF(ABS(A(K,J)).LT.SRR) GOTO 65     
      DO 60 I=JY,N
      A(K,I)=A(K,I)-A(J,I)*A(K,J) 
 60   CONTINUE
      DO 61 I=1,M 
      B(K,I)=B(K,I)-B(J,I)*A(K,J) 
 61   CONTINUE
 65   CONTINUE
      IF(J-1) 62,62,63
 62   DO 66 I=1,N 
      A(I,1)=I
 66   CONTINUE
 63   SAV =A(J,1)
      A(J,1)=A(KMAX,1)
      A(KMAX,1)=SAV
 64   CONTINUE
 70   IF(N.EQ.1) GOTO 100 
      IF(NG.EQ.N) GOTO 71 
      NG1=NG+1
      DO 72 I=NG1,N 
      DO 72 K=1,M 
      B(I,K)=0. 
  72  CONTINUE
  71  IF(NY.EQ.0) GOTO 73 
      DO 80 K=1,M 
      DO 80 J=1,NY
      JY=NG-J 
      DO 80 I=1,J 
      IX=NG-I+1 
      B(JY,K)=B(JY,K)-A(JY,IX)*B(IX,K)
 80   CONTINUE
  73  DO 91 I=1,N 
      A(I,2)=A(I,1) 
  91  CONTINUE
      NY=N-1
      IF(NY.EQ.0) GOTO 100
      DO 90 J=1,NY
      JY=J+1
      DO 94 I=JY,N
      JX=A(I,2)+0.5 
      IF(JX-J) 94,96,94 
 96   DO 95 K=1,M 
      SAV=B(J,K)
      B(J,K)=B(I,K) 
      B(I,K)=SAV
 95   CONTINUE
      A(I,2)=A(J,2) 
      GOTO 90 
 94   CONTINUE
 90   CONTINUE
 100  DETR=SUU/SQRT(ABS(DETR)) 
      GOTO 110
  35  IER=1 
      DETR=0. 
 110  RETURN
      END 
      SUBROUTINE INVERS (N,M,A,EPS,DET,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     A=MATRIX (EINGANG)
C     A=INVERSE MATRIX (AUSGANG)
C     B,AD,IK=HILFSVEKTOREN DER DIMENSION M
C     M=DIMENSION IM RUFENDEN PROGRAMM
C     N=AKTUELLE DIMENSION
C     EPS=TOLERANZ FUER PIVOTELEMENT (>0 DET WIRD MIT LAENGE DER SPALTEVEKTOREN NORMIERT)
C     DET=DETERMINANTE (FALLS EPS>0 RELATIV; NORMIERT MIT LAENGE DER SPALTENVEKT.)
C
C
C
      REAL(KIND=8),DIMENSION(M,*) ::A
      REAL(KIND=8),DIMENSION(M) :: B,AD
      INTEGER(KIND=4),DIMENSION(M) :: IK
      IER=0
      EEP=ABS(EPS)
      FA=1.
      DO   J=1,N
        Y=0.
        DO   I=1,N
          Y=Y+A(I,J)**2
        ENDDO
        FA=FA*Y
      ENDDO
C      F=SQRT(FA)
      DET=1.
      DO  J=1,N
        IK(J)=J
      ENDDO
      DO  I=1,N
        K=I
        Y=A(I,I)
C        L=I-1
        IP=I+1
        IF(IP.GT.N) GOTO 33
        DO   J=IP,N
          W= A(I,J)
          IF(ABS(W).LE.ABS(Y))  CYCLE
          K=J
          Y=W
        ENDDO
   33   DET=DET*Y
        IF(ABS(Y).LT.EEP) THEN
          IER=-1
          RETURN
        ENDIF
        Y=1./Y
        DO J=1,N
          AD(J)= A(J,K)
          A(J,K)= A(J,I)
          A(J,I)= -AD(J)*Y
          A(I,J)= A(I,J)*Y
          B(J)=A(I,J)
        ENDDO
        A(I,I)=Y
        J=IK(I)
        IK(I)=IK(K)
        IK(K)=J
        DO  K=1,N
        DO  J=1,N
         IF(J.EQ.I.OR.K.EQ.I) CYCLE
         A(K,J)= A(K,J)- B(J)*AD(K)
        ENDDO
        ENDDO
      ENDDO
      DO I=1,N
 70     K=IK(I)
        IF(K.EQ.I) CYCLE
        DO J=1,N
          W=A(I,J)
          A(I,J)= A(K,J)
          A(K,J)= W
        ENDDO
        IP=IK(I)
        IK(I)=IK(K)
        IK(K)=IP
        DET=-DET
        GOTO 70
      ENDDO
      IF(EPS.GT.0.0) THEN
        DET=DET/FA
      ENDIF
      RETURN
      END
C      
      DOUBLE PRECISION FUNCTION ARSINH(X)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF(X.GE.0.) THEN
          IF(ABS(X).LT.1.E3) THEN
               ARSINH=LOG(X+SQRT(1.D0+X*X))
          ELSE  
               ARSINH=LOG(2.*X)
          ENDIF
      ELSE                   
          IF(ABS(X).LT.1.E3) THEN
               ARSINH=-LOG(-X+SQRT(1.D0+X*X))
          ELSE
               ARSINH=-LOG(-2.*X)
          ENDIF
      ENDIF
      RETURN
      END
c
c                         +-------+
c                         !  erf  !
c                         +-------+
c
      double precision function erf(x)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      double precision ss,t,a1,a2,a3,a4,a5
c
c Zweck: Berechnen der Error-Funktion (2/SQRT(PI)*INT(EXP(-X**2)) durch rationale Approxima-
c        tion mit einem Fehler |eps(x)|=<1.4e-07.
c
      data p/0.3275911/
      data a1/0.254829592/
      data a2/-0.284496736/
      data a3/1.421413741/
      data a4/-1.45312027/
      data a5/1.061405429/
c
      xx=abs(x)
      t=1./(1.d0+p*xx)
c
      ss=a1*t+a2*t**2+a3*t**3+a4*t**4+a5*t**5     
      ss=1.d0-ss*gss(x)
      erf=ss
      if(x.lt.0.d0) then
        erf=-erf
      endif
c
      return
      end
      double precision function gss(x)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      if(abs(x).gt.9.) then
               gss=0.
      else
               gss=exp(-x*x)
      endif
      return
      end
C
C
C
      double precision function brent(ax,bx,cx,f,tol,xmin)
      IMPLICIT double precision (a-h,o-z)
      INTEGER ITMAX
      double precision ax,bx,cx,tol,xmin,f,CGOLD,ZEPS
      EXTERNAL f
      PARAMETER (ITMAX=100,CGOLD=.3819660,ZEPS=1.0d-10)
      INTEGER iter
      double precision a,b,d,e,etemp,fu,fv,fw,fx,p,q,r,
     &                 tol1,tol2,u,v,w,x,xm
      a=min(ax,cx)
      b=max(ax,cx)
      v=bx
      w=v
      x=v
      e=0.
      fx=f(x)
      fv=fx
      fw=fx
      do 11 iter=1,ITMAX
        xm=0.5*(a+b)
        tol1=tol*abs(x)+ZEPS
        tol2=2.*tol1
        if(abs(x-xm).le.(tol2-.5*(b-a))) goto 3
        if(abs(e).gt.tol1) then
          r=(x-w)*(fx-fv)
          q=(x-v)*(fx-fw)
          p=(x-v)*q-(x-w)*r
          q=2.*(q-r)
          if(q.gt.0.) p=-p
          q=abs(q)
          etemp=e
          e=d
          if(abs(p).ge.abs(.5*q*etemp).or.p.le.q*(a-x).or.p.ge.q*(b-x)) 
     *goto 1
          d=p/q
          u=x+d
          if(u-a.lt.tol2 .or. b-u.lt.tol2) d=sign(tol1,xm-x)
          goto 2
        endif
1       if(x.ge.xm) then
          e=a-x
        else
          e=b-x
        endif
        d=CGOLD*e
2       if(abs(d).ge.tol1) then
          u=x+d
        else
          u=x+sign(tol1,d)
        endif
        fu=f(u)
        if(fu.le.fx) then
          if(u.ge.x) then
            a=x
          else
            b=x
          endif
          v=w
          fv=fw
          w=x
          fw=fx
          x=u
          fx=fu
        else
          if(u.lt.x) then
            a=u
          else
            b=u
          endif
          if(fu.le.fw .or. w.eq.x) then
            v=w
            fv=fw
            w=u
            fw=fu
          else if(fu.le.fv .or. v.eq.x .or. v.eq.w) then
            v=u
            fv=fu
          endif
        endif
11    continue
c     pause 'brent exceed maximum iterations'
3     xmin=x
      brent=fx
      return
      END


C  (C) Copr. 1986-92 Numerical Recipes Software 5cL&12s(5A2+Y53.

      DOUBLE PRECISION FUNCTION ANPHI(N,X,XM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION X(*),ENPHI
      DOUBLE PRECISION XM(*)
      ANPHI=ENPHI(N,X,XM)
      RETURN
      END
      DOUBLE PRECISION FUNCTION A0PHI(N,X,XM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     BERECHNUNG DER SUMME(P(I)*EXP(-P(I)/(XM(I)+TINY(1.)))
C
      DOUBLE PRECISION X(*),SUMM,PHH,PI
      DOUBLE PRECISION XM(*)
      SUMM=TINY(1.D0)
      DO I=1,N
         SUMM=SUMM+X(I)
      ENDDO
      PHH=0.D0
      DO I=1,N
         PI=X(I)/SUMM
         PHH=PHH+PI*EXP(-PI/XM(I))              
      ENDDO
      A0PHI=PHH   
      RETURN
      END
      DOUBLE PRECISION FUNCTION A1PHI(N,X,XM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     BERECHNUNG DER -SUMME(P(I)*EXP(P(I)/XM(I))
C
      DOUBLE PRECISION X(*),SUMM,PHH,PI
      DOUBLE PRECISION XM(*)
      SUMM=TINY(1.D0)
      DO I=1,N
         SUMM=SUMM+X(I)
      ENDDO
      PHH=0.D0
      DO I=1,N
         PI=X(I)/SUMM
         PHH=PHH+PI*EXP(PI/XM(I))              
      ENDDO
      A1PHI=-PHH   
      RETURN
      END
      DOUBLE PRECISION FUNCTION QNPHI(N,X,XM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     BERECHNUNG DER QUADRATSUMME
C
      DOUBLE PRECISION X(*),SUMM,PHH
      DOUBLE PRECISION XM(*)
      SUMM=TINY(1.D0)
      DO I=1,N
         SUMM=SUMM+X(I)
      ENDDO
      PHH=0.
      DO I=1,N
         PHH=PHH+(X(I)/(SUMM+TINY(1.D0)))
      ENDDO
      QNPHI=PHH   
      RETURN
      END

      DOUBLE PRECISION FUNCTION ENPHI(N,X,XM)
C
C     BERECHNUNG DER ENTROPIE
C
      DOUBLE PRECISION X(*),SUMM,PHH,TIN,XXX
      DOUBLE PRECISION XM(*)
      TIN=TINY(1.D0)
      SUMM=TIN
      DO I=1,N
         XXX=ABS(X(I))
         SUMM=SUMM+XXX
      ENDDO
      PHH=0.D0
      DO I=1,N
         XXX=ABS(X(I))
         PHH=PHH+XXX*LOG(ABS((XXX+TIN)/(XM(I)*SUMM+TIN)))
      ENDDO
      ENPHI=-PHH/SUMM
      RETURN
      END
      DOUBLE PRECISION FUNCTION ENAPHI(N,K,X,XM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     1. ABLEITUNG DER ENTROPIE NACH X(K)
C
      DOUBLE PRECISION X(*),SUMM,PHH,XXX,TIN
      DOUBLE PRECISION XM(*)
      TIN=TINY(1.D0)
      SUMM=TIN
      DO I=1,N
         XXX=ABS(X(I))
         SUMM=SUMM+XXX
      ENDDO
      PHH=0.D0
      DO I=1,N
         XXX=ABS(X(I))
         PHH=PHH+XXX/SUMM*(LOG(ABS((XXX+TIN)/(XM(I)*SUMM+TIN))))
      ENDDO
      XXX=ABS(X(K))
      ENAPHI=(-LOG(ABS(XXX+TIN)/(XM(K)*SUMM+TIN))+PHH)/SUMM
      RETURN
      END
      DOUBLE PRECISION FUNCTION ENA2PH(N,K,L,X,XM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     2. ABLEITUNG DER ENTROPIE NACH X(K) UND X(L)
C
      DOUBLE PRECISION X(*),SUMM,SU2,PI,A2,XXX,XXY,TIN
      DOUBLE PRECISION XM(*)
      TIN=TINY(1.D0)
      SUMM=TIN
      DO I=1,N
         XXX=ABS(X(I))
         SUMM=SUMM+XXX
      ENDDO
      SU2=0.D0
      DO I=1,N
         XXX=ABS(X(I))
         PII=XXX/SUMM
         SU2=SU2+PII*LOG(PII/XM(I)+TIN)
      ENDDO
      XXX=ABS(X(K))
      XXY=ABS(X(L))
      A2=1.+LOG(XXY/(SUMM*XM(L)+TIN))
     &     +LOG(XXX/(SUMM*XM(K)+TIN))-2.*SU2
      IF(L.EQ.K) A2=A2-SUMM/(XXX+TIN)
      ENA2PH=A2/(SUMM*SUMM)
      RETURN
      END

C******************************************************
C******************************************************
C******************************************************
      SUBROUTINE INTVAL(X,XMI,XMA,FUNC,IER)
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA EPS/5.D-7/
      DATA EPP/1.D-5/
      DATA TAU/1.D-10/
      DATA GOL/0.618034/
C 
C     BERECHNUNG DER NULLSTELLE EINER FUNCTION DURCH INTERVALLSCHACHT.
C 
C 
      XMIN=XMI
      XMAX=XMA
      IER=0
C 
      AMIN=FUNC(XMIN) 
      AMAX=FUNC(XMAX)
      AMA=AMAX
      AMI=AMIN
C 
C
C 
      IF(AMAX*AMIN.GT.0.) GOTO 91 
C 
C 
C 
      IT=0
      X=0.5*(XMIN+XMAX) 
  10  A=FUNC(X) 
      IF(A*AMIN.GT.0.) THEN 
             XMIN=X 
             AMIN=A 
             X=XMIN+GOL*(XMAX-XMIN)
      ELSE
             XMAX=X 
             AMAX=A 
             X=XMAX-GOL*(XMAX-XMIN) 
      ENDIF 
      IT=IT+1 
      IF(IT.GT.100) GOTO 90 
      IF(ABS((XMIN-XMAX)/(ABS(XMA-XMI)+TAU)).LT.EPS) GOTO 90
      IF(ABS(AMAX-AMIN)/(ABS(AMA-AMI)+TAU).LT.EPP) GOTO 90
      GOTO 10 
C 
C 
C 
  90  RETURN
  91  IER=4115
      RETURN
      END 
      SUBROUTINE INTVLL(X,XMI,XMA,FUNC,IER)
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA EPP/1.D-5/
      DATA EPS/5.D-6/ 
      DATA GOL/0.618034/
      DATA TAU/1.D-10/
C 
C     BERECHNUNG DER NULLSTELLE EINER FUNCTION DURCH INTERVALLSCHACHT.
C 
      IER=0
      XMIN=XMI
      XMAX=XMA
C 
C 
      IF(XMIN.LE.0.) XMIN=1.D-60
      AMIN=FUNC(XMIN) 
      AMAX=FUNC(XMAX)
      AMA=AMAX
      AMI=AMIN
C 
C 
C 
      IF(AMAX*AMIN.GT.0.) GOTO 91 
C 
C 
C 
      IT=0
      X=EXP(0.5*(LOG(XMIN)+LOG(XMAX)))
  10  A=FUNC(X) 
      IF(A*AMIN.GT.0.) THEN 
             XMIN=X 
             AMIN=A 
             X=EXP(LOG(XMIN)+GOL*(LOG(XMAX)-LOG(XMIN)))
      ELSE
             XMAX=X 
             AMAX=A 
             X=EXP(LOG(XMAX)-GOL*(LOG(XMAX)-LOG(XMIN)))
      ENDIF 
      IT=IT+1 
      IF(IT.GT.100) GOTO 90 
      IF(ABS((XMIN-XMAX)/(ABS(XMA-XMI)+TAU)).LT.EPS) GOTO 90
      IF(ABS(AMAX-AMIN)/(ABS(AMA-AMI)+TAU).LT.EPP) GOTO 90
      GOTO 10 
C 
C 
C 
  90  RETURN
  91  IER=4115
      RETURN
      END 
C 
C 
      SUBROUTINE REGFAL(X,XMI,XMA,FUNC,IER)
C 
C 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA EPS/1.D-5/
C 
      IER=0
      XMIN=XMI
      XMAX=XMA
      AMIN=FUNC(XMIN) 
      AMAX=FUNC(XMAX) 
C 
C 
C 
      IF(AMAX*AMIN.GT.0.) GOTO 91 
C 
C 
C 
      IT=0
      BB=(XMIN-XMAX)/(AMIN-AMAX)
      X=XMIN-AMIN*BB
  10  A=FUNC(X) 
      IF(A*AMIN.GT.0.) THEN 
             XMIN=X 
             AMIN=A 
      ELSE
             XMAX=X 
             AMAX=A 
      ENDIF 
      IT=IT+1 
      IF(ABS(AMAX-AMIN).LT.EPS) GOTO 90 
      IF(ABS(A).LT.EPS) GOTO 90 
      BB=(XMIN-XMAX)/(AMIN-AMAX)
      X=XMIN-AMIN*BB
      GOTO 10 
  90  RETURN
  91  IER=4115
      RETURN
      END 

