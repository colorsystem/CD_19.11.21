C     Last change: KU 17.10.2021 17:39:36

C     Last change:  UFO  23 Aug 104   10:32 am
C**********************************************************************
C**********************************************************************
C
C           Klaus Unterforsthuber
C           20.06.2004
C
C**********************************************************************
C**********************************************************************
C**********************************************************************
C**********************************************************************
C
C
      SUBROUTINE ASINVR(RU,D,R,N,KW,A,S,A0,S0,TAS,TTL,
     &                  INL,BBL,BBU,PHIA,IER) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      INTERFACE
      REAL(KIND=8) FUNCTION REFAS(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
C
C
C
      DIMENSION RU(*),D(*),R(*),BL(2),BU(2)
      DIMENSION RI(2)
      DIMENSION HILF(6),RW(12),IW(4)
C     REAL PUF[ALLOCATABLE](:,:)
      DIMENSION PUF(6,3)
      DIMENSION IND(2),IOPT(17)
      DIMENSION BLZ(2),BUZ(2)
      DIMENSION INL(*),BBL(*),BBU(*)
      DATA RANG/1.D80/
      DATA TOL/5.D-6/,ALPH0/1.D-10/
C
C 
C 
C     A     ABSORPTION (ZU BEGINN STEHT DER GEWUENSCHTE WERT IN A)
C     S     STREUUNG   (ZU BEGINN STEHT DER GEWUENSCHTE WERT IN S) 
C     RU    EINGABE: R-WERTE DER UNTERGRUENDE GEMESSEN
C           AUSGABE: R-WERTE DER UNTERGRUENDE KORRIGIERT
C     R     R-WERTE DER SCHICHT 
C     D     SCHICHTDICKEN 
C     INL   KENNUNG FUER GRENZE
C     BBL   UNTERE GRENZEN FUER A UND S
C     BBU   OBERE GRENZEN FUER A UND S
C     TAS   GEWICHT FUER ANPASSUNG AN A0 UND S0
C     TTL   STARTWERT FUER ALPHA (DAEMPFUNGSFAKTOR)
C 
C 
C 
      DO 10 I=1,N
      RKORR=RUKORR(RU(I),KW)
      RU(I)=RKORR 
  10  CONTINUE
C 
C 
C 
      ENTRY ASKORR(RU,D,R,N,KW,A,S,A0,S0,TAS,TTL,
     &             INL,BBL,BBU,PHIA,IER)  
C 
C 
      IER=0
      IOPT(1)=99
C 
C 
      DO I=1,N
         RI(I)=RUMESS(RU(I),KW)
         BLZ(I)=BBL(I)
         BUZ(I)=BBU(I)
         IND(I)=INL(I)
      ENDDO
C 
C     RU    EINGABE: R-WERTE DER UNTERGRUENDE KORRIGIERT
C     RU    AUSGABE: R-WERTE DER UNTERGRUENDE KORRIGIERT
C 
      
      IF(R(1).GT.RI(1).AND.R(2).LT.RI(2)) THEN
              RI(2)=R(2)
              RU(2)=RUKORR(RI(2),KW)
      ENDIF
C 
C 
      NN=N+4
      IGG=0
      ALPH=TTL        
      MCON=0
      NCOLS=2
      MROW=N+2
      IF(TAS.NE.0.) MROW=MROW+2
      PHIA=RANG  
C
      IZ=0
C 
  15  DO 20 I=1,N 
      AA=A*D(I) 
      SS=S*D(I) 
      PUF(I,3)=R(I)-REFAS(AA,SS,RU(I),KW,REFAA,REFSS)
      PUF(I,1)=D(I)*REFAA
      PUF(I,2)=D(I)*REFSS
  20  CONTINUE
C 
      DO 40 J=1,2
      DO 45 K=1,2
      PUF(N+J,K)=0.
  45  CONTINUE
      PUF(N+J,J)=ALPH
  40  CONTINUE
      PUF(N+1,3)=0.
      PUF(N+2,3)=0.
      IF(TAS.NE.0.) THEN
          PUF(N+3,3)=TAS*(A0-A) 
          PUF(N+4,3)=TAS*(S0-S)
          DO 46 J=1,2
          DO 47 K=1,2
          PUF(N+2+J,K)=0.
  47      CONTINUE
          PUF(N+2+J,J)=TAS
  46      CONTINUE
      ENDIF
C     GRENZEN
C
      BL(1)=BLZ(1)-A       
      BL(2)=BLZ(2)-S       
      BU(1)=BUZ(1)-A
      BU(2)=BUZ(2)-S
C 
C 
C     WRITE(1,1997) IZ
C1997 FORMAT(' IZ',I5)
C     DO 77 I=1,MROW
C     WRITE(1,1999) (PUF(I,J),J=1,NCOLS+1)
C77   CONTINUE
C     DO 78 I=1,NCOLS
C     WRITE(1,1998) IND(I),BL(I),BU(I)                         
C78   CONTINUE
C1999 FORMAT(1X,7E11.4)
C1998 FORMAT(1X,I1,1X,7E11.4)
      CALL SBOCLS(PUF,NN,MCON,MROW,NCOLS,BL,BU,IND,IOPT,HILF,RNC,RNN,
     &            IER,RW,IW)    
C 
C
C     WRITE(1,3377) RNC,RNN,HILF(1),HILF(2)
C3377 FORMAT(' RNC RNN HILF ',4E10.3)
C  
C 
C 
      AN=A+HILF(1)    
      SN=S+HILF(2)     
C 
C 
C 
      PHI=0.
      DO 50 I=1,N 
      AA=AN*D(I)
      SS=SN*D(I)
      PHI=PHI+(R(I)-REFAS(AA,SS,RU(I),KW))**2
 50   CONTINUE
      PHI=DSQRT(PHI) 
C     WRITE(1,3388) IZ,IGG,AN,SN,PHI,ALPH
C3388 FORMAT(' IZ IGG AN SN PHI ALPH ',2I3,4E11.4)
      IF(PHI.GE.PHIA) THEN
             IGG=IGG+1 
             IF(IGG.GT.5) GOTO 90
             ALPH=ALPH*2. 
             IF(ALPH.GT.1000.) GOTO 90
      ELSE
             ALPH=ALPH*0.618
             IF(ALPH.LT.ALPH0) ALPH=ALPH0
             A=AN
             S=SN
             IZ=IZ+1
             IF(IZ.GT.100) GOTO 90
             IF(IGG.EQ.0) THEN
                 IF(ABS((PHI-PHIA)/(PHIA+TOL)).LT.TOL) GOTO 90 
             ENDIF
             PHIA=PHI
             IF(DSQRT(PHI).LT.TOL) GOTO 90
             IGG=0      
      ENDIF
      GOTO 15 
C 
C 
C 
  90  RETURN
      END 
C******************************************************************************

C
C
C
C
C


      SUBROUTINE ASDEK(CF,CW,CS,NDI,NWE,R,A,S,AW,SW,AS,SS,
     &                 KW,*)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CF(*),CW(*),CS(*),R(NDI,*)
      DIMENSION A(*),S(*),AW(*),SW(*),AS(*),SS(*)
      DIMENSION RR(2),BBL(2),BBU(2),INL(2)
      DIMENSION PUF(99)
C
      INL(1)=1
      INL(2)=1
      BBL(1)=0.
      BBL(2)=0.
      BBU(1)=1.D80
      BBU(2)=1.D80
      N=2
      A0=1.D0
      S0=1.
      TAS=0.
      DO I=1,NWE
      RR(1)=R(I,1)
      RR(2)=R(I,2)
      CALL ASDECK(PUF,RR,CF,CW,CS,N,KW,A(I),S(I),A0,S0,TAS,
     &            AW(I),SW(I),AS(I),SS(I),INL,BBL,BBU,PHIA,IER) 
      ENDDO
      RETURN
      END

      
      SUBROUTINE ASDECK(PUF,R,CF,CW,CS,N,KW,A,S,A0,S0,TAS,
     &                  AW,SW,AS,SS,INL,BBL,BBU,PHIA,IER) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CF(*),CW(*),CS(*),R(*),PUF(6,*),BL(2),BU(2)
      DIMENSION IND(2),IOPT(17),RW(30),IW(4),HILF(6)
      DIMENSION INL(*),BBL(*),BBU(*)
      EXTERNAL REFWTA
      DATA RANG/1.D80/
      DATA TOL/1.D-5/
C     DATA ALPH0/1.D-10/
C  
C 
C     CF    FARBMITTELKONZENTRATION
C     CW    WEISSPIGMENTKONZENTRATION
C     CS    SCHWARZPIGMENTKONZENTRATION
C     AW    ABSORPTION WEISSPIGMENT
C     SW    STREUUNG WEISSPIGMENT
C     AS    ABSORPTION SCHWARZPIGMENT
C     SS    STREUUNG SCHWARZPIGMENT
C     A     ABSORPTION (ZU BEGINN STEHT DER GEWUENSCHTE WERT IN A)
C     S     STREUUNG   (ZU BEGINN STEHT DER GEWUENSCHTE WERT IN S) 
C     R     R-WERTE DER SCHICHT 
C     INL   KENNUNG FUER GRENZE
C     BBL   UNTERE GRENZEN FUER A UND S
C     BBU   OBERE GRENZEN FUER A UND S
C 
C 
C 
C 
C 
      IER=0
      MCON=0
      NCOLS=2
      MROW=N    
C     IF(TAS.NE.0.) MROW=MROW+2
      PHIA=RANG  
C
C
C 
      TAU=RANG
      RD=1.
      DEL=0.001
      DO 20 I=1,N 
      CALL REINV(TAU,ALB,RD,R(I),REFWTA,KW,2,IER)
C     R1=REFWTA(TAU,ALB,RD,GK,KW,CDE)
C     K=20
C     WRITE(K,4444) R(I),R1,ALB         
C4444 FORMAT(' R(I) R1 ALB ',3F10.4)
      FN=(SW+AW)*CW(I)+(SS+AS)*CS(I)              
      RDD=(REFWTA(TAU,ALB-DEL,RD,KW)
     &    -REFWTA(TAU,ALB,RD,KW))/DEL
      IF(RDD.LE.TOL) RDD=1.
      ALC=ALB-1.          
      RRR=-ALB*FN+SW*CW(I)+SS*CS(I)
      GG=RDD/(FN+(A0+S0)*CF(I))
      
      PUF(I,1)=GG*ALB*CF(I)               
      PUF(I,2)=GG*ALC*CF(I)            
      PUF(I,3)=GG*RRR      
  20  CONTINUE
C 
C     GRENZEN
C
      BL(1)=BBL(1)         
      BL(2)=BBL(2)         
      BU(1)=BBU(1)  
      BU(2)=BBU(2)  
      IND(1)=INL(1)
      IND(2)=INL(2)
      IOPT(1)=99
C 
  
C     DO 77 I=1,MROW
C     WRITE(1,1999) (PUF(I,J),J=1,NCOLS+1)
C77   CONTINUE
C     DO 78 I=1,NCOLS
C     WRITE(1,1998) IND(I),BL(I),BU(I)                         
C78   CONTINUE
C1999 FORMAT(1X,7E11.4)
C1998 FORMAT(1X,I1,1X,7E11.4)
      CALL SBOCLS(PUF,6,MCON,MROW,NCOLS,BL,BU,IND,IOPT,HILF,RNC,RNN,
     &            IER,RW,IW)    
C 
C
C     WRITE(1,3377) RNC,RNN,HILF(1),HILF(2)
C3377 FORMAT(' RNC RNN HILF ',4E10.3)
C  
C 
C 
      A=HILF(1)       
      S=HILF(2)        
C 
C 
C 
C 
  90  RETURN
      END 


c
c
C******************************************************
C                           +-------+
C                           ! REINV
C                           +-------+
      SUBROUTINE REINV(TAU,ALB,RD,R,REFWTA,KW,I,IER)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION V(3),REFWTA
      EXTERNAL REFWTA
      DATA GOL/0.618034/,A0/0.0D0/
C 
      IER=0
      V(1)=TAU
      V(2)=ALB
      V(3)=RD 
      RG=R
C
C     Minimalwert berechnen (TAU ~ Unendlich)
C
      RGR=REFWTA(CTT,A0,RD,KW)
      IF (RG.LT.RGR) THEN
          RG=RGR+TOL
      END IF
C
C     Maximalwert berechnen (Tau=0.0)
C
      RGR=REFWTA(A0,A0,RD,KW)
      IF(RG.GT.RGR) THEN
          RG=RGR-TOL
      END IF
C
      IF(I.EQ.2) THEN
         XMIN=1.D0
         XMAX=0.D0
      ENDIF
      IF(I.EQ.3) THEN
         XMIN=0.
         XMAX=RGR
      ENDIF
      IF(I.EQ.1) THEN              
            XMIN=1./EPS
            RD=V(3)
            RR=REFWTA(A0,A0,RD,KW)
            IF(RG.GT.RR) THEN
                XMAX=-1./EPS
            ELSE
                XMAX=0.
            ENDIF 
      ENDIF
C
C 
  8   V(I)=XMIN
      RD=V(3)
      AMIN=REFWTA(V(1),V(2),RD,KW)-RG
      IF(ABS(AMIN).LT.TOL) GOTO 30
      V(I)=XMAX
      RD=V(3)
      AMAX=REFWTA(V(1),V(2),RD,KW)-RG
      IF(ABS(AMAX).LT.TOL) GOTO 30
C
C 
C 
      IF(AMAX*AMIN.GT.0.D0) GOTO 40 
C 
C
C 
      IT=0
      X=V(I)            
      IF(X.LE.XMIN.OR.X.GE.XMAX) THEN
       IF(I.EQ.1) THEN
         YMIN=ARSINH(XMIN)
         YMAX=ARSINH(XMAX)
         Y=0.5*(YMIN+YMAX)
         X=SINH(Y)
       ELSE
         X=0.5*(XMIN+XMAX)
       ENDIF
      ENDIF
  10  V(I)=X     
      RD=V(3)
      A=REFWTA(V(1),V(2),RD,KW)-RG
      IF(A*AMIN.GT.0.) THEN
             XMIN=X 
             AMIN=A 
           IF(I.EQ.1) THEN
             YMIN=ARSINH(XMIN)
             YMAX=ARSINH(XMAX)
             Y=YMIN+GOL*(YMAX-YMIN)
             X=SINH(Y)
           ELSE
             X=XMIN+GOL*(XMAX-XMIN) 
           ENDIF
      ELSE
             XMAX=X 
             AMAX=A 
          IF(I.EQ.1) THEN
             YMIN=ARSINH(XMIN)
             YMAX=ARSINH(XMAX)
             Y=YMIN+GOL*(YMAX-YMIN)
             X=SINH(Y)
           ELSE
             X=XMAX-GOL*(XMAX-XMIN) 
           ENDIF
      ENDIF 
      IT=IT+1 
      IF(IT.GT.100) GOTO 30 
      IF(ABS(AMIN-AMAX).LT.EPS) GOTO 30 
      GOTO 10
C 
C 
C 
  40  IER=2
      V(I)=XMIN
      IF(AMAX.LT.AMIN) V(I)=XMAX
  30  IF(I.EQ.1) TAU=V(I)
      IF(I.EQ.2) ALB=V(I)
      IF(I.EQ.3) RD=V(I)
      RETURN
      END 
C 

      DOUBLE PRECISION FUNCTION REFWTA(TAU,ALB,RU,KW)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RU

      INTERFACE
      REAL(KIND=8) FUNCTION REFAS(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
      IF(ALB.LT.0.) ALB=0.
      IF(ALB.GT.1.) ALB=1.
      A=(1.-ALB)*TAU
      S=ALB*TAU 
      REFWTA=REFAS(A,S,RU,KW)
      RETURN
      END

C
      DOUBLE PRECISION FUNCTION TRFWTA(TAU,ALB,RU,KW)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RU

      INTERFACE
      REAL(KIND=8) FUNCTION TEFAS(A,S,RD,KW,TEFAA,TEFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::TEFAA,TEFSS
      END FUNCTION
      END INTERFACE
      IF(ALB.GT.1.) ALB=1.
      A=(1.-ALB)*TAU
      S=ALB*TAU 
      TRFWTA=TEFAS(A,S,RU,KW)
      RETURN
      END        
C***************************************************************************
C
      DOUBLE PRECISION FUNCTION RGODE(ALP,RLP,SCHI,RU,KW)

      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C 
C 
      AI= GINTE(KW)
      R1 = GLANZ(KW)
      R2 = GK(3,KW)
      GD1 = 1.0-GLINZ(KW)
      RR = GD1*(1.-R2)
C     SF=GK(6)
C     SB=GK(7)
C     ST=0.25*(1.-SF-SB)
      Q=SCHI-(ALP+RLP)
      IF(Q.LT.TOL) Q=TOL
      RQ=RLP*RLP
      AQ=SCHI*SCHI
      CMTR=AQ-Q*Q+RQ
      WURZ=DSQRT(ABS(((SCHI-Q)**2-RQ)*((SCHI+Q)**2-RQ)))
      IF(CMTR.LE.TOL.AND.WURZ.LE.TOL) THEN
         RUN=0.
      ELSE
         RUN=DSQRT(ABS((CMTR-WURZ)/(CMTR+WURZ)))
      ENDIF
      RGODE=AI*(R1+(RR*RUN/(1.-RUN*R2)))
      RETURN
      END
C
C 
C******************************************************
C
      DOUBLE PRECISION FUNCTION RGOTR(ALP,RLP,SCHI,RU,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C 
C 
      AI=GINTE(KW)
      R1 = GLANZ(KW)
      R2 = GK(3,KW)
      GD1 = 1.0-GLINZ(KW)
      RR = GD1*(1.-R2)
      Q=SCHI-(ALP+RLP)
      IF(Q.LT.TOL) Q=TOL
      RQ=RLP*RLP
      AQ=SCHI*SCHI
      QQ=Q*Q
      CMTR=AQ-QQ+RQ
      WURZ=DSQRT(ABS(((SCHI-Q)**2-RQ)*((SCHI+Q)**2-RQ)))
      IF(CMTR.LE.TOL.AND.WURZ.LE.TOL) THEN
         RUN=0.
      ELSE
         RUN=DSQRT(ABS((CMTR-WURZ)/(CMTR+WURZ)))
      ENDIF
      BQ=QQ-RQ 
      RUQ=RUN*RUN
      P12=SCHI*LOG(ABS((AQ+RUQ*BQ+TINY(1.))/(AQ*RUQ+BQ+TINY(1.))))
      IF(RUN.GT..999) THEN
         XX=(AQ*RUQ+QQ-RQ)/((SCHI*(AQ-QQ+RQ)+TINY(1.)))
      ELSE
         XX=(1.-RUQ)/(P12+TINY(1.))
      ENDIF
      XQQ=(RUN-RU)*QEEE(P12)
      RGOTR=AI*(R1+(RR*(RU*XX+XQQ)/((1.-R2*RU)*XX+(RUN-R2)*XQQ)))
      RETURN
      END
C******************************************************
C******************************************************
C******************************************************
C***************************************************************************
C
C
      DOUBLE PRECISION FUNCTION OMEGA0(RUN,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C
C     BERECHNUNG DER ALBEDO AUS DERM REFLEXIONSWERT FÜR DIE UNENDLICH DICKE SCHICHT
C     UND DEN STREUWERTEN FÜR VORWÄRTS- (SF) UND RÜCKWÄRTSSTREUUNG (SB)
C
      SF=GK(6,KW)
      SB=GK(7,KW)
      SP=SF+SB
      SM=SB-SF
      SIGD=(1.D0-RUN)/(1.D0+RUN)
      SIGQ=SIGD**2
      OMA=1.D0-3.*SP-SIGQ*SM*(1.D0+SP)
      OMB=1.D0+3.*SP+SIGQ*(2.*SM-1.D0-SP)
      OMC=2.*(SIGQ-1.D0)
      OMEGA0=(-OMB+SQRT(OMB**2-4.*OMA*OMC))/(2.*OMA)
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION RSAUND(R,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C     BERECHNUNG DER KORRIGIERTEN R-WERTE (RSAUND) AUS GEMESSENEN R-WERTEN(R)
C
C
      R0=GLANZ(KW)
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)
      RHI=R/AI-R0
      RSAUND=RHI/((1.0-R00)*(1.D0-R11)+R11*RHI)
      RETURN
      END
C 
      DOUBLE PRECISION FUNCTION RUSAUN(RK,KW)
      USE MODGKWR

C
C     BERECHNUNG DER GEMESSENEN R-WERTE (RUSAUN) AUS KORRIGIERTEN R-WERTEN (RK)
C
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      R0=GLANZ(KW)
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)
      RUSAUN=AI*(R0+((1.0-R00)*(1.D0-R11)*RK/(1.D0-R11*RK)))
      RETURN
      END
C******************************************************************************
      DOUBLE PRECISION FUNCTION TA0QQ(XST,OM0,KW)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA EPS/1.D-30/
      SIG=SIGMA(OM0,KW)
      IF(SIG.EQ.0.D0) THEN
        TA0QQ=1./EPS
      ELSE
        TA0QQ=-0.5*LOG(ABS(1.D0-XST)+EPS)/SIG
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION SIGSTA(OM0,SF,SB)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SP=SF+SB
      SM=SB-SF
      HI=1.D0 - 0.5*(1.D0+SP)*OM0
      CHZ=(1.D0-SP*OM0)*HI-0.5*(OM0*(1.D0-SP))**2 
      SIGSTA=SQRT(CHZ/((1.D0+SM*OM0)*HI))
      RETURN
      END
      DOUBLE PRECISION FUNCTION SIGMA(OM0,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SF=GK(6,KW)
      SB=GK(7,KW)
      SIGMA=(1.D0+(SB-SF)*OM0)*SIGSTA(OM0,SF,SB)
      RETURN
      END
      DOUBLE PRECISION FUNCTION XSTAR(RW,RUW,RS,RUS,RUN)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA EPS/1.D-30/
      CHN=(1.D0-RUN*RW)*(RUN-RUW)-(1.D0-RUN*RS)*(RUN-RUS)
      IF(ABS(CHN).LT.EPS) THEN
         XSTAR=1.D0
      ELSE
         XSTAR=(1.D0-RUN**2)*((RW-RS)-(RUW-RUS))/CHN
         IF(XSTAR.LT.0.D0) THEN
           XSTAR=0.D0
         ENDIF
         IF(XSTAR.GT.1.D0-EPS) THEN
            XSTAR=1.D0-EPS
         ENDIF
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION RUNEND(RW,RUW,RS,RUS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA TOL/0.00000001/
C
C
C     Berechnung von R-unendlich aus korrigierten R-Werten über weißem und scharzem Untergrund,
C     und korrigierten R-Werten über weißem  und schwarzem Untergrund
C     alle Werte sind mit RSAUND korrigiert
C
      CHHN=(RS-RUS)*(1.D0+RW*RUW)-(RW-RUW)*(1.D0+RS*RUS)
      IF(ABS(CHHN).LT.TOL) THEN
         CH=0.D0
      ELSE
         CH=2.D0*(RS*RUW-RW*RUS)/CHHN
      ENDIF
      IF(CH.GT.1.D0) THEN 
         CH=1.D0
      ENDIF
      CHWU=SQRT(ABS(1.D0-CH**2))
      RUNEND=SQRT(ABS((1.D0-CHWU)/(1.D0+CHWU)))
      RETURN
      END 
C
      DOUBLE PRECISION FUNCTION TUNEND(TW,RUW,TS,RUS,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C
C     Berechnung von R-unendlich aus Transmissionswerten über weiß (gemessen)(TW) und R-Werten für weißem Untergrund (korrigiert) (RUW) und
C     Transmissionswerten über schwarz (gemessen) (TS)  und R-Werten für schwarzem Untergrund (korrigiert) (RUS)
C     FALLS RES=1 oder REW=1 Kein Ergebnis
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)
      QW=TW/AI
      QS=TS/AI
      UW=QW*(1.-RUS)
      US=QS*(1.-RUW)
      V1=UW-US
      V2=UW*RUW-US*RUS
      H=QW*QS*(RUS-RUW)/((1.0-R00))
      CHHN=V1*V1+V2*V2-(1.+R11*R11)*H*H
      IF(ABS(CHHN).LT.TOL) THEN
         CH=0.D0
      ELSE
         CH=2.*(V1*V2-R11*H*H)/CHHN
      ENDIF
      IF(CH.GT.1.D0) THEN 
         CH=1.D0
      ENDIF
      CHWU=SQRT(ABS(1.D0-CH**2))
      TUNEND=SQRT(ABS((1.D0-CHWU)/(1.D0+CHWU)))
      RETURN
      END
C
C     BERECHNUNG VON REFLEXIONSWERTEN (EINFACHVERSIONEN)
C
C
C
      DOUBLE PRECISION FUNCTION RKUMU(AKS,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     REFLEXIONSWERTE KUBELKA-MUNK MIT SAUNDERSON-KORREKTUR
C     AKS= K/S-WERT
C
      RUN=1./(AKS+1.+DSQRT(AKS*(AKS+2.)))
      R0=GLANZ(KW)
      R1=GLINZ(KW)
      AI=GINTE(KW)
      R2=GK(3,KW)
      RR=(1.0-R1)*(1.-R2)
      RKUMU=AI*(R0+(RR*RUN/(1.-R2*RUN)))
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION RLABE(EXT,KW,RU)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     REFLEXIONSWERTE LAMBERT-BEER MIT SAUNDERSON-KORREKTUR
C     EXT EXTINKTION 
C     RU KORRIGIERTE R-WERTE DES UNTERGRUNDES
      REX=RU*EXP(-2.*EXT)
      R0=GLANZ(KW)
      R1=GLINZ(KW)
      AI=GINTE(KW)
      R2=GK(3,KW)
      RR=(1.0-R1)*(1.-R2)
      RLABE=AI*(R0+(RR*REX/(1.-R2*REX)))
      RETURN
      END
C
C
C
C***************************************************************************
C***************************************************************************
      DOUBLE PRECISION FUNCTION R2KUB(A0,S0,A1,S1,RD,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     R-Werte FÜR 2 ÜBEREINANDERLIEGENDE SCHICHTEN
C     SCHICHT 0 (A0,S0)
C     SCHICHT 1 (A1,S1)
C
C     10.3.00
C 
C 
C 
C 
C     Ax     ABSORPTION
C     Sx     STREUUNG
C     RD     REFLEXION DES UNTERGRUNDES (KORRIGIERT)
C 
      DOUBLE PRECISION RD
C
C 
C 
C 
C 
C 
C 
C 
C 
      AI=GINTE(KW)
      R1 = GLANZ(KW)
      R2 = GK(3,KW)
      GD1 = 1.0-GLINZ(KW)
      RR = GD1*(1.D0-R2)
C
C
      SF=GK(6,KW)
      SB=GK(7,KW)
      CALL RUNQWRT(A0,S0,SF,SB,RU0,QW0)
      CALL RUNQWRT(A1,S1,SF,SB,RU1,QW1)
C
C
C     Berechnung der Reflexion einer Doppelschicht
C
C
      ZAE=RD+(RU0-RD)*QW0+(RU1-RD)*QW1*(1.-(1.-RU0*RU1)*QW0)
      ANE=1.-R2*RD+(RU0-R2)*(RU0-RD)*QW0+(RU1-RD)*(RU1-R2)*QW1
     &-(RU1-RD)*(RU0-R2)*(1.-RU0*RU1)*QW0*QW1
      R2KUB=AI*(R1+(RR*ZAE/ANE))
    
C
C 
      RETURN
      END
      DOUBLE PRECISION FUNCTION R2UNE(A0,S0,A1,S1,RD,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     R-Werte FÜR 2 ÜBEREINANDERLIEGENDE SCHICHTEN (UNTERE SCHICHT IST UNENDLICH DICK)
C     SCHICHT 0 (A0,S0)
C     SCHICHT 1 (A1,S1)
C
C     10.3.00
C 
C 
C 
C 
C
C     Ax     ABSORPTION
C     Sx     STREUUNG
C     RD     REFLEXION DES UNTERGRUNDES (KORRIGIERT)
C 
      DOUBLE PRECISION RD
C
C 
C 
C 
C 
C 
C 
C 
C 
      AI=GINTE(KW)
      R1 = GLANZ(KW)
      R2 = GK(3,KW)
      GD1 = 1.0-GLINZ(KW)
      RR = GD1*(1.D0-R2)
C
C
      SF=GK(6,KW)
      SB=GK(7,KW)
      CALL RUNQWRT(A0,S0,SF,SB,RU0,QW0)
      CALL RUNQWRT(A1,S1,SF,SB,RU1,QW1)
C
C
C     Berechnung der Reflexion einer Doppelschicht (Untere Schicht ist unendlich dick)
C
C
      ZAE=RU1+(RU0-RU1)*QW0
      ANE=1.-R2*RU1+(RU0-RU1)*(RU0-R2)*QW0
      R2UNE=AI*(R1+(RR*ZAE/ANE))
    
C
C 
      RETURN
      END
      SUBROUTINE RUNQWRT(A,S,SF,SB,RUN,QW)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA RANN/1.D72/
      DATA EPS/1.D-40/
      ST=0.25*(1.D0-SF-SB)
C
      TAU=S+A
      IF(TAU.GT.RANN) TAU=RANN 
      ALB=0.
      IF(ABS(TAU).GT.EPS) ALB=S/TAU
      HHI=4.*ALB*ST*ST/(1.D0-ALB*(1.D0-2.*ST)+EPS)
      OF=SF+HHI
      OB=SB+HHI
      OLF=OF*ALB
      OLB=OB*ALB
      ONN=1.D0+OLB-OLF
      W=2.*OB/(ONN+TINY(1.D0))
      WW=ALB*W                   
      EXX=1.D0-WW
      IF(EXX.LT.0.D0) EXX=0.D0
C
C     GAMMA ^
C
C
      AQL=DSQRT(DABS(EXX)) 
C
C     R-UNENDLICH 
C 
      RUN=(1.D0-AQL)/(1.D0+AQL)
C     
C
C
C     X*GAMMA
C
      TAU=TAU*ONN
      GAM=TAU*AQL
C
C
C     (GAMMA-aii)*x*Q(2*GAMMA*X)
C
C
C
      QW=2.*TAU*QEEE(2.*GAM)/(1+RUN)**2
      RETURN
      END
C
C
C
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C
C
C
      DOUBLE PRECISION FUNCTION TRTAU(A,S,TAU,RD,KW,RT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER RT
      DOUBLE PRECISION RD
      IF(RT.EQ.1) THEN
        TRTAU=TETAU(A,S,TAU,RD,KW)
      ELSE
        TRTAU=RETAU(A,S,TAU,RD,KW)
      ENDIF
      RETURN
      END

      DOUBLE PRECISION FUNCTION RETAU(A,S,TAU,RD,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SF=GK(6,KW)*S
      SB=GK(7,KW)*S
      ST=0.25*(S-SF-SB)
      RETAU=RWTAU(A,SF,SB,ST,TAU,RD,KW)
      RETURN
      END
C
C***************************************************************************
C***************************************************************************
      DOUBLE PRECISION FUNCTION RWTAU(A,SF,SB,ST,TAUEX,RD,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     MEADOR AND WEAVER FUER NICHTDECKENDE SCHICHTEN
C
C     04.09.94
C 
C 
C 
C 
C
C     A     ABSORPTION
C     S     STREUUNG
C     RD    REFLEXION DES UNTERGRUNDES (KORRIGIERT) 
C 
      DOUBLE PRECISION RD
C
C 
C 
C 
C 
C 
C 
C 
C 
      AI=GINTE(KW)
      R1 = GLANZ(KW)
      R2 = GK(3,KW)
      GD1 = 1.0-GLINZ(KW)
      RR = GD1*(1.-R2)
      S=SF+SB+4.*ST
C
      AA=A
      SS=S
      TAU=SS+AA  
      IF(TAU.GT.RANN) TAU=RANN 
      IF(ST.LE.0.) THEN
         HHI=0.
      ELSE
         HHI=4.*ST*ST/(AA+2.*ST)
      ENDIF
      IF(TAU.LE.EPS) THEN
         OLF=0.
         OLB=0.
      ELSE
         OLF=(SF+HHI)/TAU
         OLB=(SB+HHI)/TAU
      ENDIF
      HIP=1.-OLF+OLB
      HII=OLB/(1.-OLF+EPS)
      AQL=DSQRT(ABS((1.-HII)/(1.+HII)))
C
C     R-UNENDLICH 
C 
C     
      RUN=(1.-AQL)/(1+AQL)                 
      QW=2.*TAUEX*HIP
      X=QW*AQL
      QTW=QEEE(X)
      QW=QW*QTW
C 
C     ALLGEMEINER FALL
C 
   10 RU2=(1.+RUN)**2 
      HILF =(RUN-RD)/RU2            
      HILW=HILF*QW
      RG = R1+RR*(RD+HILW)/((1.-R2*RD)+(RUN-R2)*HILW) 
      IF(RG.LT.R1) RG=R1
      RWTAU=AI*RG
      RETURN
      END
C

      DOUBLE PRECISION FUNCTION TETAU(A,S,TAU,RD,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RD
      SF=GK(6,KW)*S
      SB=GK(7,KW)*S
      ST=0.25*(S-SF-SB)
      TETAU=TWTAU(A,SF,SB,ST,TAU,RD,KW)
      RETURN
      END
C
C***************************************************************************
C***************************************************************************
      DOUBLE PRECISION FUNCTION TWTAU(A,SF,SB,ST,TAUEX,RD,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     MEADOR AND WEAVER FUER NICHTDECKENDE SCHICHTEN
C
C     18.07.2000
C 
C 
C 
C 
C
C     A     ABSORPTION
C     S     STREUUNG
C     RD    REFLEXION DES UNTERGRUNDES (KORRIGIERT) 
C 
      DOUBLE PRECISION RD
C
C 
C 
C 
C 
C 
C 
C 
C 
      AI=GINTE(KW)
      R2 = GK(3,KW)
      GD1 = 1.0-GLINZ(KW)
      RT = GD1*(1.-RD)
      S=SF+SB+4.*ST
C
      AA=A
      SS=S
      TAU=SS+AA  
      IF(TAU.GT.RANN) TAU=RANN 
      IF(ST.LE.0.) THEN
         HHI=0.
      ELSE
         HHI=4.*ST*ST/(AA+2.*ST)
      ENDIF
      IF(TAU.LE.EPS) THEN
         OLF=0.
         OLB=0.
      ELSE
         OLF=(SF+HHI)/TAU
         OLB=(SB+HHI)/TAU
      ENDIF
      HIP=1.-OLF+OLB
      HII=OLB/(1.-OLF+EPS)
      AQL=DSQRT(ABS((1.-HII)/(1.+HII)))
C
C     R-UNENDLICH 
C 
C     
      RUN=(1.-AQL)/(1+AQL)                 
      QW=2.*TAUEX*HIP
      X=QW*AQL
      QTW=QEEE(X)
      QW=QW*QTW
C 
C     ALLGEMEINER FALL
C 
   10 RU2=(1.+RUN)**2 
      HILF =(RUN-RD)/RU2            
      HILW=HILF*QW
      RG =RT*(EXP(-0.5*X))/((1.-R2*RD)+(RUN-R2)*HILW)
      TWTAU=AI*RG
      RETURN
      END

C
C***************************************************************************
C

C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION TRRDI(AS,SS,S1,S2,AD,SD,RB,KW,RT)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER RT
      DOUBLE PRECISION RB
      IF(RT.EQ.1) THEN
        TRRDI=TVRDIN(AS,SS,S1,S2,AD,SD,RB,KW)
      ELSE
        TRRDI=RVRDIN(AS,SS,S1,S2,AD,SD,RB,KW)
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION RVRDE(AS,SS,S1,S2,AD,SD,RB,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RB
C      
C     AS     ABSORPTION GERICHTET
C     SS     STREUUNG GERICHTET-GERICHTET
C     S1     STREUUNG GERICHTET-DIFFUS (VORWAERTS)
C     S2     STREUUNG GERICHTET-DIFFUS (RUECKWAERTS)
C     AD     ABSORPTION DIFFUS
C     SD     STREUUNG DIFFUS-DIFFUS
C
C
C
C     4-Fluss deckende Schicht
C  
C
C
C
      R0=GLANZ(KW)
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)
C
C
C     ALPH0(+)  1.-R00
C     ALPH0(-)  0.
C     ALPH1(+)  0.
C                     +
C     ALPH1(-)  RSD*Y0 (X0)
C     BETA0(+)  R00
C     BETA0(-)  RSS
C     BETA1(+)  R11
C     BETA1(-)  RDD
C
C
C
C     DECKENDE SCHICHT  
C   
C    
C
C     S1  =   VORWAERTSSTREUUNG   GERICHTET ===>  DIFFUS
C     S2  =   RUECKWAERTSSTREUUNG GERICHTET ===>  DIFFUS
C     SS  =   STREUUNG            GERICHTET ===>  GERICHTET
C
C
C   
      SSD=S1+S2
C
C
      TAS=AS+SSD+SS
      TAD=AD+SD
      SID=DSQRT(AD/(TAD+EEP))
      GAD=DSQRT((AS+SSD)/(TAS+EEP))
      GAM=GAD*TAS
      SIG=SID*TAD
C
C
C     GAMMA(*)   SIGMA(*)   h(*)   z(*)   H     Z   
C
C
      Z=(1.D0-GAD)/(1.D0+GAD)
      H=(1.D0-SID)/(1.D0+SID)
      ANZ=1.D0-R00*Z
      Y0= (1.0-R00)*Z/ANZ
      ANH=1.D0-R11*H
      Y2=(1.00-R11)*H/ANH
      Y1=(1.0-R00)*(S2+S1*Z+H*(S1+S2*Z))/(ANZ*ANH*(GAM+SIG)+EEP)
      RGE=R0 +(1.-R00)*Y0
      RDI=(1.D0-R11)*Y1
      RDJ=R11+(1.0-R11)*Y2
      RVRDE=AI*((1.0-GK(12,KW))*(RGE+RDI)+GK(12,KW)*RDJ)
      RETURN
      END

C
      DOUBLE PRECISION FUNCTION RVRDI(AS,SS,S1,S2,AD,SD,RB,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RB
C
C
C  
C
C      
C      
C     AS     ABSORPTION GERICHTET
C     SS     STREUUNG GERICHTET-GERICHTET
C     S1     STREUUNG GERICHTET-DIFFUS (VORWAERTS)
C     S2     STREUUNG GERICHTET-DIFFUS (RUECKWAERTS)
C     AD     ABSORPTION DIFFUS
C     SD     STREUUNG DIFFUS-DIFFUS
C
C
C     RB     REFLEXION DES UNTERGRUNDES (KORRIGIERT)
C  
C
C
C
C
      R0=GLANZ(KW)
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)*(1.0-GK(12,KW))
      AJ=GINTE(KW)*GK(12,KW)
      RSS=GK(4,KW)*RB
      RDD=GK(5,KW)*RB
      RSD=(1.D0-GK(4,KW))*RB
C
C
C     ALPH0(+)  1.-R00
C     ALPH0(-)  0.
C     ALPH1(+)  0.
C                     +
C     ALPH1(-)  RSD*Y0 (X0)
C     BETA0(+)  R00
C     BETA0(-)  RSS
C     BETA1(+)  R11
C     BETA1(-)  RDD
C
C
C
C   
C    
C
C    
C
C     S1  =   VORWAERTSSTREUUNG   GERICHTET ===>  DIFFUS
C     S2  =   RUECKWAERTSSTREUUNG GERICHTET ===>  DIFFUS
C     SS  =   STREUUNG            GERICHTET ===>  GERICHTET
C
C
C   
C
C
      SSD=S1+S2
C
C   
C
C
      TAS=AS+SSD+SS
      TAD=AD+SD
      SIG=DSQRT(AD*TAD)
      GAM=DSQRT((AS+SSD)*TAS)
      IF(TAS.LE.ZERO) THEN
         GAD=1.D0
      ELSE
         GAD=GAM/TAS
      ENDIF
      IF(TAD.LE.ZERO) THEN
         SID=1.D0
      ELSE
         SID=SIG/TAD
      ENDIF
C      
C
C     GAMMA(*)   SIGMA(*)   h(*)   z(*)   H     Z   
C
C
      GAS=GAM+AS+SSD+0.5*SS
      SIS=SIG+AD+0.5*SD
      ZST=1.D0+GAD
      HST=1.D0+SID
      Z=(1.D0-GAD)/(1.D0+GAD)
      H=(1.D0-SID)/(1.D0+SID)
      XSI=GAM
      ETA=SIG
      QX=QEEE(2.*XSI)
      QE=QEEE(2.*ETA)
      ANZ=1.D0-R00*RSS+(R00-Z)*(RSS-Z)*GAS*QX
      ANH=1.D0-R11*RDD+(R11-H)*(RDD-H)*SIS*QE
      HE=2.*SID/HST
      HX=2.*GAD/ZST
      AST=SSD*(2.D0-HE-HX)
      BETA=(1.D0-RSS)*HE
      BXSI=(1.D0-RDD)*HX
      BST=(1.D0-RSS)*(1.D0-RDD)
      FA1=AST*RDD*RSS+S2*( BST+RDD*BETA+RSS*BXSI+RDD*RSS*HE*HX)
      FA2=AST*RSS    +S1*(-BST+    BETA-RSS*BXSI+    RSS*HE*HX)
      FA3=AST*RDD    +S1*(-BST-RDD*BETA+    BXSI+    RDD*HE*HX)   
      FA4=AST        +S2*( BST-    BETA-    BXSI+        HE*HX)
      Q1=SEEE(XSI+ETA,ZERO)
      Q2=SEEE(XSI-ETA,2.*ETA)
      Q3=SEEE(ETA-XSI,2.*XSI)
      Q4=SEEE(ETA+XSI,ETA+XSI)
      U1=UEEE(XSI,ETA,ZERO)
      U2=UEEE(XSI,-ETA,2.*ETA)      
      U3=UEEE(ETA,XSI,ZERO)
      U4=UEEE(ETA,-XSI,2.*XSI)       
      FF=FEEE(XSI,ETA)
C
C
      Y0= (1.0-R00)*(RSS+(Z-RSS)*GAS*QX)/ANZ
      YH= (1.0-R11)*(RDD+(H-RDD)*SIS*QE)/ANH
      ALN=RSD*EXX(-XSI-ETA)
      AL1=FA1*Q1+FA2*Q2+FA3*Q3+FA4*Q4
      AL2=(1.D0-RSS)*AST*(RDD*U1+U2)
      AL3=(1.D0-RDD)*AST*(RSS*U3+U4)
      AL4=BST*AST*FF
      Y1=(1.0-R00)*(ALN+(0.25*ZST*HST*AL1+AL4*SIS*GAS
     &  + 0.5*(HST*GAS*AL2+ZST*SIS*AL3)))/(ANH*ANZ)
      RGE=R0 +(1.-R00)*Y0
      RDI=(1.D0-R11)*Y1
      RVRDI=AI*(RGE+RDI)+AJ*(R0+(1.0-R00)*YH)
      RETURN
      END  


C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION TVRDI(AS,SS,S1,S2,AD,SD,RB,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RB
C
C
C      
C
C  
C
C      
C      
C     AS     ABSORPTION GERICHTET
C     SS     STREUUNG GERICHTET-GERICHTET
C     S1     STREUUNG GERICHTET-DIFFUS (VORWAERTS)
C     S2     STREUUNG GERICHTET-DIFFUS (RUECKWAERTS)
C     AD     ABSORPTION DIFFUS
C     SD     STREUUNG DIFFUS-DIFFUS
C
C
C
C     RB     REFLEXION DES UNTERGRUNDES (KORRIGIERT)
C  
C
C
C
C
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)*(1.0-GK(12,KW))
      AJ=GINTE(KW)*GK(12,KW)
      RSS=GK(4,KW)*RB
      RDD=GK(5,KW)*RB
      RSD=(1.D0-GK(4,KW))*RB
C
C
C     ALPH0(+)  1.-R00
C     ALPH0(-)  0.
C     ALPH1(+)  0.
C                     +
C     ALPH1(-)  RSD*Y0 (X0)
C     BETA0(+)  R00
C     BETA0(-)  RSS
C     BETA1(+)  R11
C     BETA1(-)  RDD
C
C
C
C   
C    
C
C    
C
C     S1  =   VORWAERTSSTREUUNG   GERICHTET ===>  DIFFUS
C     S2  =   RUECKWAERTSSTREUUNG GERICHTET ===>  DIFFUS
C     SS  =   STREUUNG            GERICHTET ===>  GERICHTET
C
C
      SSD=S1+S2
C   
C
C
C
C
C
C
C
      TAS=AS+SSD+SS
      TAD=AD+SD
      SIG=DSQRT(AD*TAD)
      GAM=DSQRT((AS+SSD)*TAS)
      IF(TAS.LE.ZERO) THEN
         GAD=1.D0
      ELSE
         GAD=GAM/TAS
      ENDIF
      IF(TAD.LE.ZERO) THEN
         SID=1.D0
      ELSE
         SID=SIG/TAD
      ENDIF
C      
C
C     GAMMA(*)   SIGMA(*)   h(*)   z(*)   H     Z   
C
C
      GAS=GAM+AS+SSD+0.5*SS
      SIS=SIG+AD+0.5*SD
      ZST=1.D0+GAD
      HST=1.D0+SID
      Z=(1.D0-GAD)/(1.D0+GAD)
      H=(1.D0-SID)/(1.D0+SID)
      XSI=GAM
      ETA=SIG
      QX=QEEE(2.*XSI)
      QE=QEEE(2.*ETA)
      ANZ=1.D0-R00*RSS+(R00-Z)*(RSS-Z)*GAS*QX
      ANH=1.D0-R11*RDD+(R11-H)*(RDD-H)*SIS*QE
      HE=2.*SID/HST
      HX=2.*GAD/ZST
      AST=SSD*(2.-HE-HX)
      BETA=(1.D0-RSS)*HE
      BXSI=(1.D0-R11)*HX
      BST=(1.D0-RSS)*(1.D0-R11)
      GA1=AST*RSS    +S2*(-BST+    BETA-RSS*BXSI+    RSS*HE*HX)
      GA2=AST*RSS*R11+S1*( BST+R11*BETA+RSS*BXSI+R11*RSS*HE*HX)
      GA3=AST        +S1*( BST-    BETA-    BXSI+        HE*HX)   
      GA4=AST*R11    +S2*(-BST-R11*BETA+    BXSI+    R11*HE*HX)
      Q1=SEEE(XSI+ETA,ETA)
      Q2=SEEE(XSI-ETA,ETA)     
      Q3=SEEE(ETA-XSI,2.*XSI+ETA)
      Q4=SEEE(ETA+XSI,XSI)
      U1=UEEE(XSI,-ETA,ETA)             
      U2=UEEE(XSI,ETA,ETA)                  
      U3=UEEE(ETA,-XSI,XSI)                
      U4=UEEE(ETA,XSI,XSI)          
      FF=GEEE(ETA,XSI)             
C
C
      Y0=(1.0-R00)*EXX(-XSI)/ANZ
      ALN=RSD*EXX(-XSI)*(R11+(H-R11)*SIS*QE)
      AL1=GA1*Q1+GA2*Q2+GA3*Q3+GA4*Q4      
      AL2=(1.D0-RSS)*AST*(R11*U1+U2)
      AL3=(1.D0-R11)*AST*(RSS*U3+U4)
      AL4=BST*AST*FF
      Y1=(1.0-R00)*(ALN+(0.25*ZST*HST*AL1+AL4*SIS*GAS
     &  +0.5*(HST*GAS*AL2+ZST*SIS*AL3)))/(ANH*ANZ)
      TGE=(1.D0-RSS-RSD)*Y0
      TDI=(1.D0-RDD)*Y1
      TVRDI=AI*(TGE+TDI)+AJ*(1.0-RDD)*(1.0-R00)*EXP(-ETA)/ANH
C
C
C
      RETURN
      END  

C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION RS1S2(A,S1,S2,SS,RB,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RB
C
C
C      
C
C  
C
C      
C      
C     AS     ABSORPTION GERICHTET
C     SS     STREUUNG GERICHTET-GERICHTET
C     SSD    STREUUNG GERICHTET-DIFFUS 
C     AD     ABSORPTION DIFFUS
C     SD     STREUUNG DIFFUS-DIFFUS
C
C
C
C     RB     REFLEXION DES UNTERGRUNDES (KORRIGIERT)
C  
C
C     TRANSPARENT
C
C
C
      SD=S1+S2
      
      RS1S2=RVRDIN(A,SS,S1,S2,A,SD,RB,KW)
      RETURN
      END
C******************************************************
C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION RSDSS(A,SD,SS,RB,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RB
C
C
C      
C
C  
C
C      
C      
C     AS=A   ABSORPTION GERICHTET
C     SS     STREUUNG GERICHTET-GERICHTET
C     SSD=SD STREUUNG GERICHTET-DIFFUS 
C     AD=A   ABSORPTION DIFFUS
C     SD     STREUUNG DIFFUS-DIFFUS
C
C
C
C     RB     REFLEXION DES UNTERGRUNDES (KORRIGIERT)
C  
C
C
C     TRANSPARENT
C
C
      S1=GK(6,KW)*SD
      S2=GK(7,KW)*SD
      
      RSDSS=RVRDIN(A,SS,S1,S2,A,SD,RB,KW)
      RETURN
      END
C******************************************************
C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION RS1S2D(A,S1,S2,SS,RB,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RB
C
C
C      
C
C  
C
C      
C      
C     AS     ABSORPTION GERICHTET
C     SS     STREUUNG GERICHTET-GERICHTET
C     SSD    STREUUNG GERICHTET-DIFFUS 
C     AD     ABSORPTION DIFFUS
C     SD     STREUUNG DIFFUS-DIFFUS
C
C
C
C     RB     REFLEXION DES UNTERGRUNDES (KORRIGIERT)
C  
C
C     DECKEND
C
C
C
      SD=S1+S2
      
      RS1S2D=RVRDE(A,SS,S1,S2,A,SD,RB,KW)
      RETURN
      END
C******************************************************
C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION RSDSSD(A,SD,SS,RB,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RB
C
C
C      
C
C  
C
C      
C      
C     AS=A   ABSORPTION GERICHTET
C     SS     STREUUNG GERICHTET-GERICHTET
C     SSD=SD STREUUNG GERICHTET-DIFFUS 
C     AD=A   ABSORPTION DIFFUS
C     SD     STREUUNG DIFFUS-DIFFUS
C
C
C
C     RB     REFLEXION DES UNTERGRUNDES (KORRIGIERT)
C  
C
C
C     DECKEND
C
C
      S1=GK(6,KW)*SD
      S2=GK(7,KW)*SD
      
      RSDSSD=RVRDE(A,SS,S1,S2,A,SD,RB,KW)
      RETURN
      END
C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION RSPDI(AS,SS,SSD,AD,SD,RB,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RB
C
C
C      
C
C  
C
C      
C      
C     AS     ABSORPTION GERICHTET
C     SS     STREUUNG GERICHTET-GERICHTET
C     SSD    STREUUNG GERICHTET-DIFFUS 
C     AD     ABSORPTION DIFFUS
C     SD     STREUUNG DIFFUS-DIFFUS
C
C
C
C     RB     REFLEXION DES UNTERGRUNDES (KORRIGIERT)
C  
C
C
C
C
      S1=GK(6,KW)*SSD
      S2=GK(7,KW)*SSD
      RSPDI=RVRDIN(AS,SS,S1,S2,AD,SD,RB,KW)
      RETURN
      END
C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION RSPDE(AS,SS,SSD,AD,SD,RB,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RB
C
C
C      
C
C  
C
C      
C      
C     AS     ABSORPTION GERICHTET
C     SS     STREUUNG GERICHTET-GERICHTET
C     SSD    STREUUNG GERICHTET-DIFFUS 
C     AD     ABSORPTION DIFFUS
C     SD     STREUUNG DIFFUS-DIFFUS
C
C
C
C     RB     REFLEXION DES UNTERGRUNDES (KORRIGIERT)
C  
C
C     DECKEND
C
C
C
      S1=GK(6,KW)*SSD
      S2=GK(7,KW)*SSD
      RSPDE=RVRDE(AS,SS,S1,S2,AD,SD,RB,KW)
      RETURN
      END
C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION TSPDI(AS,SS,SSD,AD,SD,RB,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION RB
C
C
C      
C
C  
C
C      
C      
C     AS     ABSORPTION GERICHTET
C     SS     STREUUNG GERICHTET-GERICHTET
C     SSD    STREUUNG GERICHTET-DIFFUS
C     AD     ABSORPTION DIFFUS
C     SD     STREUUNG DIFFUS-DIFFUS
C
C
C
C     RB     REFLEXION DES UNTERGRUNDES (KORRIGIERT)
C  
C
C
C
C
      S1=GK(6,KW)*SSD
      S2=GK(7,KW)*SSD
      TSPDI=TVRDIN(AS,SS,S1,S2,AD,SD,RB,KW)
      RETURN
      END
C
C
C



C***************************************************************************
C***************************************************************************
c     24.01.2021
C
C***************************************************************************
C***************************************************************************

C
C
      DOUBLE PRECISION FUNCTION REFAS(A,S,RD,KW,
     &                          REFAA,REFSS)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTERFACE
      REAL(KIND=8) FUNCTION RETRTRA(JSTEU,A,SM,ST,RD,KW,
     &                              REFAA,REFSM,REFST)
      INTEGER(KIND=4) ::KW,JSTEU
      REAL(KIND=8) ::A,SM,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSM,REFST
      END FUNCTION
      END INTERFACE

      REAL(KIND=8) ::RD,A,SM,ST
      INTEGER(KIND=4) ::KW
      REAL(KIND=8),OPTIONAL :: REFAA,REFSS
C
C
C
      SF=GK(6,KW)
      SB=GK(7,KW)
      ST=S*ABS(0.25*(1.D0-SF-SB))
      SM=S*ABS(1.D0+SB-SF)
      IF(PRESENT(REFAA).AND.PRESENT(REFSS)) THEN
         REFAS=RETRTRA(0,A,SM,ST,RD,KW,REFAA,REFSM,REFST)
         REFSS=(1.0+SB-SF)*REFSM+0.25*(1.0-SF-SB)*REFST
      ELSE
         REFAS=RETRTRA(0,A,SM,ST,RD,KW)
      ENDIF
      RETURN
      END FUNCTION
C
C
C***************************************************************************
C***************************************************************************
c     24.01.2021
C
C***************************************************************************
C***************************************************************************

C
C
C
      DOUBLE PRECISION FUNCTION TEFAS(A,S,RD,KW,
     &                          TEFAA,TEFSS)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTERFACE
      REAL(KIND=8) FUNCTION RETRTRA(JSTEU,A,SM,ST,RD,KW,
     &                              REFAA,REFSM,REFST)
      INTEGER(KIND=4) ::KW,JSTEU
      REAL(KIND=8) ::A,SM,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSM,REFST
      END FUNCTION
      END INTERFACE

      REAL(KIND=8) ::RD,A,SM,ST
      INTEGER(KIND=4) ::KW
      REAL(KIND=8),OPTIONAL :: TEFAA,TEFSS
C
C
C
      SF=GK(6,KW)
      SB=GK(7,KW)
      ST=S*ABS(0.25*(1.D0-SF-SB))
      SM=S*ABS(1.D0+SB-SF)
      IF(PRESENT(TEFAA).AND.PRESENT(TEFSS)) THEN
         TEFAS=RETRTRA(1,A,SM,ST,RD,KW,TEFAA,TEFSM,TEFST)
         TEFSS=(1.0+SB-SF)*TEFSM+0.25*(1.0-SF-SB)*TEFST
      ELSE
         TEFAS=RETRTRA(1,A,SM,ST,RD,KW)
      ENDIF
      RETURN
      END FUNCTION
C
C***************************************************************************
C***************************************************************************
c     24.01.2021
C
C***************************************************************************
C***************************************************************************

C
C
      DOUBLE PRECISION FUNCTION REWEMET(A,SB,ST,RD,KW,
     &                                  REFAA,REFSB,REFST)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTERFACE
      REAL(KIND=8) FUNCTION RETRTRA(JSTEU,A,SM,ST,RD,KW,
     &                              REFAA,REFSM,REFST)
      INTEGER(KIND=4) ::KW,JSTEU
      REAL(KIND=8) ::A,SM,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSM,REFST
      END FUNCTION
      END INTERFACE

      REAL(KIND=8) ::RD,A,SM,ST
      INTEGER(KIND=4) ::KW
      REAL(KIND=8),OPTIONAL :: REFAA,REFSB,REFST
C
C
C
      SM=2.0*SB+4.0*ST
      IF(PRESENT(REFAA).AND.PRESENT(REFSB)
     &                      .AND.PRESENT(REFST)) THEN
         REWEMET=RETRTRA(0,A,SM,ST,RD,KW,REFAA,REFSM,REFST)
         REFSB=2.0*REFSM
         REFST=4.0*REFSM+REFST
      ELSE
         REWEMET=RETRTRA(0,A,SM,ST,RD,KW)
      ENDIF
      RETURN
      END FUNCTION
C
C***************************************************************************
C***************************************************************************
c     24.01.2021
C
C***************************************************************************
C***************************************************************************

C
      DOUBLE PRECISION FUNCTION TEWEMET(A,SB,ST,RD,KW,
     &                                  REFAA,REFSB,REFST)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTERFACE
      REAL(KIND=8) FUNCTION RETRTRA(JSTEU,A,SM,ST,RD,KW,
     &                              REFAA,REFSM,REFST)
      INTEGER(KIND=4) ::KW,JSTEU
      REAL(KIND=8) ::A,SM,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSM,REFST
      END FUNCTION
      END INTERFACE

      REAL(KIND=8) ::RD,A,SM,ST
      INTEGER(KIND=4) ::KW
      REAL(KIND=8),OPTIONAL :: REFAA,REFSB,REFST
C
C
C
      SM=2.0*SB+4.0*ST
      IF(PRESENT(REFAA).AND.PRESENT(REFSB)
     &                      .AND.PRESENT(REFST)) THEN
         TEWEMET=RETRTRA(1,A,SM,ST,RD,KW,REFAA,REFSM,REFST)
         REFSB=2.0*REFSM
         REFST=4.0*REFSM+REFST
      ELSE
         TEWEMET=RETRTRA(1,A,SM,ST,RD,KW)
      ENDIF
      RETURN
      END FUNCTION
C
C
C***************************************************************************
C***************************************************************************
c     24.01.2021
C
C***************************************************************************
C***************************************************************************

C
C
      DOUBLE PRECISION FUNCTION RETRTRA(JSTEU,A,SM,ST,RD,KW,
     &                                  REFAA,REFSM,REFST)
      USE MODGKWR

C 
C     MISCHFORMEL HOFFMANN (FARBE+LACK 87(1981)15)
C                 SCHMELZER(DEFAZET 9(1977)373)
C     MEADOR+WEAVER (APPL.OPTICS,12(1976)3155)
C 
C 
C
C     18.06.2004
C
C 
C 
C     QV=(1.-J2(TL)/J2(1)*TL*T2L)/A
C     QW=(1.-J1(TL)/J1(1)*T2L)/AL 
C     TL=T**(AL**2) 
C     T2L=T**(2*AL*(1-AL))
C 
C     J1(TL)/J1(1)=TL**1.1355 
C     J2(TL)/J2(1)=TL**4.2710 
C 
C
C 
C     J1 UND J2 NACH HOFFMANN 
C 
C
C 
C 
C
C     A     ABSORPTION
C     SM    STREUUNG =1+SB-ST
C     ST    STREUUNG =0.25*(1-SF-SB)
C     RD    REFLEXION DES UNTERGRUNDES (KORRIGIERT) 
C 
C
C     JSTEU=0 REMISSION  =1 TRANSMISSION
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8) ::RD,A,SM,ST
      INTEGER(KIND=4) ::KW
      REAL(KIND=8),OPTIONAL :: REFAA,REFSM,REFST
C 
C 
C 
C 
C 
C
C 
C 
C
      AI=GINTE(KW)
      R1 = GLANZ(KW)
      R2 = GK(3,KW)
      GD1 = 1.0-GLINZ(KW)
C
C
      EI1=GK(8,KW)
      EI2=GK(9,KW)
      POT=GK(11,KW)
c      SF=GK(6,KW)
c      SB=GK(7,KW)
c      ST=ABS(0.25*(1.D0-SF-SB))
c      SM=ABS(1.D0+SB-SF)
C
      AA=A
      IF(AA.LE.TOL) THEN
         AA=TOL
      ENDIF
      SS=SM
      ASM=SQRT(ABS(SS+AA))
      A6ST=SQRT(ABS(AA*(AA+6.0*ST)))
      A2ST=SQRT(ABS(AA+2.0*ST))
      AQL=A6ST/(A2ST*ASM)
      RUN=(1.0-AQL)/(1.0+AQL)
      HILF=(RUN-RD)/(1.0+RUN)**2
      ASX=0.0
      AS2=0.0
      ASP=0.0
      IF(ASM.GT.0.0) THEN
         AS2=ASM**2
         ASP=ASM**(-2.0*POT)
         ASX=AS2*ASP
      ENDIF
      RO1=(AQL*EI1+2.0)*ASX
      X1=RO1*AQL
C
C
      RO2=(AQL*EI2+2.0)*ASX
      X2=RO2*AQL
      Q2=QEEE(X2)
      QW=RO2*Q2
C
C
      HILW=HILF*QW
C
C
C
      ANX=(1.0-R2*RD)+(RUN-R2)*HILW
C
      IF(JSTEU.EQ.0) THEN
C
C
C        REMISSION
C
C
         Q1=QEEE(X1)
         QV=RO1*Q1
         HILV=HILF*QV
         RR = GD1*(1.D0-R2)
         RG = R1+RR*(RD+HILV)/ANX
         IF(RG.LT.R1) RG=R1
         RETRTRA=AI*RG

      ELSEIF(JSTEU.EQ.1) THEN
C
C
C        TRANSMISSION
C
C
         RT=GD1*(1.0-RD)
         RETRTRA=AI*RT*EXP(-0.5*X1)/ANX
C
C 
C
C 
C 
      ENDIF
      IF(.NOT.PRESENT(REFAA).OR..NOT.PRESENT(REFSM)
     &                      .OR..NOT.PRESENT(REFST)) THEN
         RETURN
      ENDIF
C
C 
C 
C 
C 
C     ABLEITUNGEN
C 
C
C 
C 
C 
C 
      DASMA=0.5/ASM
      DA6STA=(AA+3.0*ST)/A6ST
      DA2STA=0.5/A2ST
C
C
      DASMS=0.5/ASM
      DA6STS=3.0*AA/A6ST
      DA2STS=1.0/A2ST
C
C
      DRUNAQL=-2.0/(1.0+AQL)**2
      DHILFRUN=(1.0-RUN+2.0*RD)/(1.0+RUN)**3
C
C
      AHH=4.0*ASM*A2ST*(POT-1.0)
      DROASM1=-ASP*(A6ST*EI1*(2.0*POT-1.0)+AHH)/A2ST
      DROA2ST1=-ASX*A6ST*EI1/(ASM*A2ST**2)
      DROA6ST1=ASM*ASP*EI1/A2ST
C
C
      DROASM2=-ASP*(A6ST*EI2*(2.0*POT-1.0)+AHH)/A2ST
      DROA2ST2=-ASX*A6ST*EI2/(ASM*A2ST**2)
      DROA6ST2=ASM*ASP*EI2/A2ST
C
C
      DAQLASM=-A6ST/(A2ST*ASM**2)
      DAQLA2ST=-A6ST/(ASM*A2ST**2)
      DAQLA6ST=1.0/(ASM*A2ST)
C
C
C
      DRODA1=DROASM1*DASMA+DROA2ST1*DA2STA+DROA6ST1*DA6STA
      DRODST1=DROA2ST1*DA2STS+DROA6ST1*DA6STS
      DRODSM1=DROASM1*DASMS
C
C
C
C
C
      DRODA2=DROASM2*DASMA+DROA2ST2*DA2STA+DROA6ST2*DA6STA
      DRODST2=DROA2ST2*DA2STS+DROA6ST2*DA6STS
      DRODSM2=DROASM2*DASMS
C
C
C
C
      DAQLA=DAQLASM*DASMA+DAQLA2ST*DA2STA+DAQLA6ST*DA6STA
      DAQLST=DAQLA2ST*DA2STS+DAQLA6ST*DA6STS
      DAQLSM=DAQLASM*DASMS

C
C
C
      DQWA=RO2*QEXX(X2)*(AQL*DRODA2+RO2*DAQLA)+Q2*DRODA2
      DQWST=RO2*QEXX(X2)*(AQL*DRODST2+RO2*DAQLST)+Q2*DRODST2
      DQWSM=RO2*QEXX(X2)*(AQL*DRODSM2+RO2*DAQLSM)+Q2*DRODSM2
C
C
C
C
C

C
C
      DHILWA=QW*DHILFRUN*DRUNAQL*DAQLA+HILF*DQWA
      DHILWST=QW*DHILFRUN*DRUNAQL*DAQLST+HILF*DQWST
      DHILWSM=QW*DHILFRUN*DRUNAQL*DAQLSM+HILF*DQWSM
      
C
C
      DANXA=HILW*DRUNAQL*(DAQLASM*DASMA+DAQLA2ST*DA2STA+DAQLA6ST*DA6STA)
     &      +DHILWA*(RUN-R2)
      DANXST=HILW*DRUNAQL*(DAQLA2ST*DA2STS+DAQLA6ST*DA6STS)
     &      +DHILWST*(RUN-R2)
      DANXSM=HILW*DRUNAQL*(DAQLASM*DASMS)
     &      +DHILWSM*(RUN-R2)
C
C
      IF(JSTEU.EQ.0) THEN
C
         DQVA=RO1*QEXX(X1)*(AQL*DRODA1+RO1*DAQLA)+Q1*DRODA1
         DQVST=RO1*QEXX(X1)*(AQL*DRODST1+RO1*DAQLST)+Q1*DRODST1
         DQVSM=RO1*QEXX(X1)*(AQL*DRODSM1+RO1*DAQLSM)+Q1*DRODSM1
C
C
C
         DHILVA=QV*DHILFRUN*DRUNAQL*DAQLA+HILF*DQVA
         DHILVST=QV*DHILFRUN*DRUNAQL*DAQLST+HILF*DQVST
         DHILVSM=QV*DHILFRUN*DRUNAQL*DAQLSM+HILF*DQVSM
C
C
         REFAA=AI*RR*(ANX*DHILVA-(RD+HILV)*DANXA)/ANX**2
         REFST=AI*RR*(ANX*DHILVST-(RD+HILV)*DANXST)/ANX**2
         REFSM=AI*RR*(ANX*DHILVSM-(RD+HILV)*DANXSM)/ANX**2
      ELSEIF(JSTEU.EQ.1) THEN
         DHILTA=DRODA1*AQL+RO1*DAQLA
         DHILTST=DRODST1*AQL+RO1*DAQLST
         DHILTSM=DRODSM1*AQL+RO1*DAQLSM
         REFAA=RETRTRA*(-0.5*DHILTA-DANXA/ANX)
         REFST=RETRTRA*(-0.5*DHILTST-DANXST/ANX)
         REFSM=RETRTRA*(-0.5*DHILTSM-DANXSM/ANX)
C
C
      ENDIF
C
C
      RETURN
      END
C
C
C     24.01.2021
C 
C       
      REAL(KIND=8) FUNCTION REWEMED(A,SB,ST,RD,KW,REFAA,REFSB,REFST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,SB,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSB,REFST
      INTERFACE
      REAL(KIND=8) FUNCTION REFDEK(A,SM,ST,RD,KW,REFAA,REFSM,REFST)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,SM,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSM,REFST
      END FUNCTION
      END INTERFACE
      SM=2.0*SB+4.0*ST
c      WRITE(*,*) 'SM',SM
      IF(PRESENT(REFAA).AND.PRESENT(REFSB)
     &                      .AND.PRESENT(REFST)) THEN
         REWEMED=REFDEK(A,SM,ST,RD,KW,REFAA,REFSM,REFST)
         REFSB=2.0*REFSM
         REFST=4.0*REFSM+REFST
      ELSE
         REWEMED=REFDEK(A,SM,ST,RD,KW)
      ENDIF
      RETURN
      END
C
C
C
C     24.01.2021
C 
C 
C
C
C
      REAL(KIND=8) FUNCTION REFDE(A,S,RU,KW,REFAA,REFSS)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,SB,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      INTERFACE
      REAL(KIND=8) FUNCTION REFDEK(A,SM,ST,RD,KW,REFAA,REFSM,REFST)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,SM,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSM,REFST
      END FUNCTION
      END INTERFACE
      SF=GK(6,KW)
      SB=GK(7,KW)
      ST=S*ABS(0.25*(1.D0-SF-SB))
      SM=S*ABS(1.D0+SB-SF)
      IF(PRESENT(REFAA).AND.PRESENT(REFSS))THEN
         REFDE=REFDEK(A,SM,ST,RD,KW,REFAA,REFSM,REFST)
         REFSS=(1.0+SB-SF)*REFSM+0.25*(1.0-SF-SB)*REFST
      ELSE
         REFDE=REFDEK(A,SM,ST,RD,KW)
      ENDIF
      RETURN
      END
C
C
C
      DOUBLE PRECISION FUNCTION REFDEK(A,SM,ST,RD,KW,
     &                                 REFAA,REFSM,REFST)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,SM,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSM,REFST
C
C
C 
C     MEADOR AND WEAVER FUER DECKENDE SCHICHTEN
C     REMISSION
C
C
C
C
C     24.01.2021
C 
C 
C     GK(1)  GLANZ
C     GK(2)  INNERE GERICHTETE REFLEXION 
C     GK(3)  INNERE DIFFUSE REFLEXION
C     GK(4)  NICHT VERWENDET
C     GK(5)  NICHT VERWENDET
C     GK(6)  NICHT VERWENDET
C     GK(7)  NICHT VERWENDET
C     GK(8)  NICHT VERWENDET
C     GK(9)  NICHT VERWENDET
C     GK(10) GESAMTINTENSITAET (s. auch GINTE)
C     GK(11) NICHT VERWENDET
C
C
C     SONDERFAELLE
C
C
C 
C     A     ABSORPTION
C     SB    RÜCKWÄRTSSTREUUNG
C     ST    SEITWÄRTSSTREUUNG
C     RD    REFLEXION DES UNTERGRUNDES (KORRIGIERT)NICHT VERWENDET
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
      AI=GINTE(KW)
      R1 = GLANZ(KW)
      R2 = GK(3,KW)
      GD1 = 1.0-GLINZ(KW)
      RR = GD1*(1.-R2)
C
C
C
      AA=A
      IF(AA.LE.TOL) THEN
         AA=TOL
      ENDIF
      SS=SM
      ASM=SQRT(ABS(SS+AA))
      A6ST=SQRT(ABS(AA*(AA+6.0*ST)))
      A2ST=SQRT(ABS(AA+2.0*ST))
      AQL=A6ST/(A2ST*ASM)
C
      ANEN=(1.0-R2)+AQL*(1.0+R2)
      REFDEK=AI*(R1+RR*(1.0-AQL)/ANEN)
C
      IF(.NOT.PRESENT(REFAA).OR..NOT.PRESENT(REFSM)
     &                      .OR..NOT.PRESENT(REFST)) THEN
         RETURN
      ENDIF
C
C
      DASMA=0.5/ASM
      DA6STA=(AA+3.0*ST)/A6ST
      DA2STA=0.5/A2ST
C
C
C
      DASMS=0.5/ASM
      DA6STS=3.0*AA/A6ST
      DA2STS=1.0/A2ST
C
C
C
      DAQLASM=-A6ST/(A2ST*ASM**2)
      DAQLA2ST=-A6ST/(ASM*A2ST**2)
      DAQLA6ST=1.0/(ASM*A2ST)
C
C
      DRUNAQL=-2*AI*RR/ANEN**2
C
C
      REFAA=DAQLASM*DASMA+DAQLA2ST*DA2STA+DAQLA6ST*DA6STA
      REFSM=DAQLASM*DASMS
      REFST=DAQLA2ST*DA2STS+DAQLA6ST*DA6STS
      REFAA=DRUNAQL*REFAA
      REFSM=DRUNAQL*REFSM
      REFST=DRUNAQL*REFST
C
C
C
C 
      RETURN
      END FUNCTION


C
C
C     Berechnung der Reflexions- und Transmissionswerte (2 Fluss)
C
C
C
      DOUBLE PRECISION FUNCTION TRKUB(A,S,RD,KW,RT)
C
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) :: RT
      DOUBLE PRECISION RD
      IF(RT.EQ.1) THEN
        TRKUB=TEKUB(A,S,RD,KW)
      ELSE
        TRKUB=REKUB(A,S,RD,KW)
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION TEKUB(A,S,RD,KW)
C
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTERFACE
      REAL(KIND=8) FUNCTION RETRTRA(JSTEU,A,SM,ST,RD,KW,
     &                              REFAA,REFSM,REFST)
      INTEGER(KIND=4) ::KW,JSTEU
      REAL(KIND=8) ::A,SM,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSM,REFST
      END FUNCTION
      END INTERFACE
      
C     GK(6)  ANTEIL VORWAERTSSTREUUNG   (0.5==> ISOTROP   0.==> KUB.-MUNK)
C     GK(7)  ANTEIL RUECKWAERTSSTREUUNG (0.5==> ISOTROP 1.==> KUBELKA MUNK)
      SF=GK(6,KW)
      SB=GK(7,KW)
      ST=S*ABS(0.25*(1.D0-SF-SB))
      SM=S*ABS(1.D0+SB-SF)
      TEKUB=RETRTRA(1,A,SM,ST,RD,KW)
      RETURN
      END
C
C***************************************************************************
C***************************************************************************
c
      DOUBLE PRECISION FUNCTION REKUB(A,S,RD,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTERFACE
      REAL(KIND=8) FUNCTION RETRTRA(JSTEU,A,SM,ST,RD,KW,
     &                              REFAA,REFSM,REFST)
      INTEGER(KIND=4) ::KW,JSTEU
      REAL(KIND=8) ::A,SM,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSM,REFST
      END FUNCTION
      END INTERFACE
      
C     GK(6)  ANTEIL VORWAERTSSTREUUNG   (0.5==> ISOTROP   0.==> KUB.-MUNK)
C     GK(7)  ANTEIL RUECKWAERTSSTREUUNG (0.5==> ISOTROP 1.==> KUBELKA MUNK)
      SF=GK(6,KW)
      SB=GK(7,KW)
      ST=S*ABS(0.25*(1.D0-SF-SB))
      SM=S*ABS(1.D0+SB-SF)
      REKUB=RETRTRA(0,A,SM,ST,RD,KW)
      RETURN
      END
C
C***************************************************************************
C***************************************************************************
c
c     Reflexion und Transmission für Rezeptberechnung
c
c
c
c
c
      REAL(KIND=8) FUNCTION TRRWR(PAS,RU,KW,RT,KU,ABLEI)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8),DIMENSION(*) :: PAS
      INTEGER(KIND=4) :: RT,KU,KW
      REAL(KIND=8) ::RW,HLF,DPS,DEE,AD,SD,SS,AS,TAU,ST,RU
      REAL(KIND=8),OPTIONAL,DIMENSION(*) :: ABLEI
      INTEGER NPAS
      INTERFACE
      REAL(KIND=8) FUNCTION TRFAS(A,S,RD,KW,RT,TRFAA,TRFSS)
      INTEGER(KIND=4) ::KW,RT
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::TRFAA,TRFSS
      END FUNCTION
C
C
      REAL(KIND=8) FUNCTION TRSTZ(A,S,RD,KW,RT,KU,ABLEI)
      INTEGER(KIND=4) ::KW,RT,KU
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL,DIMENSION(*)  ::ABLEI
      END FUNCTION
C
C
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      REAL(KIND=8) FUNCTION REWEMET(A,SB,ST,RD,KW,REFAA,REFSB,REFST)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,SB,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSB,REFST
      END FUNCTION
      REAL(KIND=8) FUNCTION REWEMED(A,SB,ST,RD,KW,REFAA,REFSB,REFST)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,SB,ST,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSB,REFST
      END FUNCTION
      REAL(KIND=8) FUNCTION TEWEMET(A,SB,ST,RD,KW,TEFAA,TEFSB,TEFST)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,SB,ST,RD
      REAL(KIND=8),OPTIONAL  ::TEFAA,TEFSB,TEFST
      END FUNCTION
c
c
c

      END INTERFACE
      DATA DEE/1.D-4/,DABL/1.D-8/
C
C
C     PAS(1)     ABSORPTION (DIFFUS)                    (AD)
C     PAS(2)     STREUUNG (DIFFUS)                      (SD)
C     PAS(3)     VORWÄRTSSTREUUNG (GERICHTET==> DIFFUS) (S1)
C     PAS(4)     RÜCKWÄRTSSTREUUNG (GERICHTET==> DIFFUS)(S2)
C     PAS(5)     ABSORPTION GERICHTET                   (AS)
C     PAS(6)     STREUUNG (GERICHTET==>GERICHTET)       (SS)
C       .                .
C       .                .
      AD=PAS(1)
      SD=PAS(2)
      NPAS=2
C
C
C
C
C
C
C      Absorption und Streuung für Omega0 - Transmission - Tabelle
C
C






      IF(CDE(1:1).EQ.'T'.OR.CDE(1:1).EQ.'X'.OR.CDE(1:1).EQ.'Y'
     &                  .OR.CDE(1:1).EQ.'U') THEN
C
C
C         TRANSPARENTE SCHICHT 2-FLUSS-THEORIE
C
C
          IF(ICHAR(CDE(2:2)).GE.65.AND.ICHAR(CDE(2:2)).LE.90) THEN
C
C            Programm aus Modul MODSTTZ
C
             IF(PRESENT(ABLEI)) THEN
                TRRWR=TRSTZ(AD,SD,RU,KW,RT,KU,ABLEI)
             ELSE
                TRRWR=TRSTZ(AD,SD,RU,KW,KU,RT)
             ENDIF
          ELSE
C
C
             IF(PRESENT(ABLEI)) THEN
                TRRWR=TRFAS(AD,SD,RU,KW,RT,ABLEI(1),ABLEI(2))
             ELSE
                TRRWR=TRFAS(AD,SD,RU,KW,RT)
             ENDIF
          ENDIF
      ELSE IF(CDE(1:1).EQ.'D'.OR.CDE(1:1).EQ.'H'.OR.CDE(1:1).EQ.'I'
     &                       .OR.CDE(1:1).EQ.'C')THEN
C
C     
C
C         DECKENDE SCHICHT 2-FLUSS-THEORIE
C
C
          IF(PRESENT(ABLEI)) THEN
             TRRWR=REFDE(AD,SD,RU,KW,ABLEI(1),ABLEI(2))
          ELSE
             TRRWR=REFDE(AD,SD,RU,KW)
          ENDIF

C
C
C
C     BERECHNUNG VON REFLEXIONSWERTEN FUER EFFEKTPIGMENTE
C
C
      ELSE IF(CDE(1:1).EQ.'S') THEN
C
C
C         TRANSPARENTE SCHICHT 4-FLUSS-THEORIE
C          
C
          AD=PAS(1)
          SD=PAS(2)
          S1=PAS(3)
          S2=PAS(4)
          AS=PAS(5)
          SS=PAS(6)
          NPAS=6

          TRRWR=TRRDI(AS,SS,S1,S2,AD,SD,RU,KW,RT)
          IF(PRESENT(ABLEI)) THEN
             DPS=DEE*MAX(1.D0,AD)
             HLF=AD+DPS
             RW=TRRDI(AS,SS,S1,S2,HLF,SD,RU,KW,RT)
             ABLEI(1)=(RW-TRRWR)/DPS
             DPS=DEE*MAX(1.D0,SD)
             HLF=SD+DPS
             RW=TRRDI(AS,SS,S1,S2,AD,HLF,RU,KW,RT)
             ABLEI(2)=(RW-TRRWR)/DPS
             DPS=DEE*MAX(1.D0,S1)
             HLF=S1+DPS
             RW=TRRDI(AS,SS,HLF,S2,AD,SD,RU,KW,RT)
             ABLEI(3)=(RW-TRRWR)/DPS
             DPS=DEE*MAX(1.D0,S2)
             HLF=S2+DPS
             RW=TRRDI(AS,SS,S1,HLF,AD,SD,RU,KW,RT)
             ABLEI(4)=(RW-TRRWR)/DPS
             DPS=DEE*MAX(1.D0,AS)
             HLF=AS+DPS
             RW=TRRDI(HLF,SS,S1,S2,AD,SD,RU,KW,RT)
             ABLEI(5)=(RW-TRRWR)/DPS
             DPS=DEE*MAX(1.D0,SS)
             HLF=SS+DPS
             RW=TRRDI(AS,HLF,S1,S2,AD,SD,RU,KW,RT)
             ABLEI(6)=(RW-TRRWR)/DPS

          ENDIF

C
      ELSE IF(CDE(1:1).EQ.'E') THEN
C
C
C         DECKENDE SCHICHT 4-FLUSS-THEORIE
C
C
C
C
          S1=PAS(3)
          S2=PAS(4)
          AS=PAS(5)
          SS=PAS(6)
          NPAS=6
          TRRWR=RVRDE(AS,SS,S1,S2,AD,SD,RU,KW)
          IF(PRESENT(ABLEI)) THEN
             DPS=DEE*MAX(1.D0,AD)
             HLF=AD+DPS
             RW=RVRDE(AS,SS,S1,S2,HLF,SD,RU,KW)
             ABLEI(1)=(RW-TRRWR)/DPS
             DPS=DEE*MAX(1.D0,SD)
             HLF=SD+DPS
             RW=RVRDE(AS,SS,S1,S2,AD,HLF,RU,KW)
             ABLEI(2)=(RW-TRRWR)/DPS

             DPS=DEE*MAX(1.D0,S1)
             HLF=S1+DPS
             RW=RVRDE(AS,SS,HLF,S2,AD,SD,RU,KW)
             ABLEI(3)=(RW-TRRWR)/DPS
             DPS=DEE*MAX(1.D0,S2)
             HLF=S2+DPS
             RW=RVRDE(AS,SS,S1,HLF,AD,SD,RU,KW)
             ABLEI(4)=(RW-TRRWR)/DPS

             DPS=DEE*MAX(1.D0,AS)
             HLF=AS+DPS
             RW=RVRDE(HLF,SS,S1,S2,AD,SD,RU,KW)
             ABLEI(5)=(RW-TRRWR)/DPS
             DPS=DEE*MAX(1.D0,SS)
             HLF=SS+DPS
             RW=RVRDE(AS,HLF,S1,S2,AD,SD,RU,KW)
             ABLEI(6)=(RW-TRRWR)/DPS

          ENDIF


C
C
C
C     BERECHNUNG VON REFLEXIONSWERTEN AUS AD,SD,TAU (Transparent,tranluzent)
C
C
      ELSE IF(CDE(1:1).EQ.'V') THEN
C
C
C         TRANSPARENTE (TRANSLUZENTE) SCHICHT
C          
C
          TAU=PAS(3)
          NPAS=3
          TRRWR=TRTAU(AD,SD,TAU,RU,KW,RT)
          IF(PRESENT(ABLEI)) THEN
             DPS=DEE*MAX(1.D0,AD)
             HLF=AD+DPS
             RW=TRTAU(HLF,SD,TAU,RU,KW,RT)
             ABLEI(1)=(RW-TRRWR)/DPS
             DPS=DEE*MAX(1.D0,SD)
             HLF=SD+DPS
             RW=TRTAU(AD,HLF,TAU,RU,KW,RT)
             ABLEI(2)=(RW-TRRWR)/DPS
             DPS=DEE*MAX(1.D0,TAU)
             HLF=TAU+DPS
             RW=TRTAU(AD,SD,HLF,RU,KW,RT)
             ABLEI(3)=(RW-TRRWR)/DPS
          ENDIF

C
C
C     BERECHNUNG VON REFLEXIONSWERTEN AUS AS,SS,AD,SD (2 Schichten)
C
C
      ELSE IF(CDE(1:1).EQ.'F') THEN
C
C
C         Untere Schicht (AS,SS) ist deckend
C          
C
C          AS=PAS(3)
C          SS=PAS(4)
C          RW= R2UNE(AD,SD,AS,SS,RU,GKQ,KW,CDE)
C
C
C
C
C
C          Optische Daten nach Untergründen getrennt
C
C
C
          IF(KU.EQ.1) THEN
             IF(PRESENT(ABLEI)) THEN
                TRRWR=REFDE(AD,SD,RU,KW,ABLEI(1),ABLEI(2))
             ELSE
                TRRWR=REFDE(AD,SD,RU,KW)
             ENDIF
          ELSEIF(KU.EQ.2) THEN
             AS=PAS(3)
             SS=PAS(4)
             NPAS=4
             IF(PRESENT(ABLEI)) THEN
                TRRWR=REFDE(AS,SS,RU,KW,ABLEI(3),ABLEI(4))
             ELSE
                TRRWR=REFDE(AS,SS,RU,KW)
             ENDIF
          ENDIF
      ELSE IF(CDE(1:1).EQ.'R') THEN
C
C
C         Beide Schichten transparent
C
C
C          AS=PAS(3)
C          SS=PAS(4)
C          RW= R2KUB(AD,SD,AS,SS,RU,GKQ,KW,CDE)
C
C
C
C
C          Optische Daten nach Untergründen getrennt
C
C
C
          IF(KU.EQ.1) THEN
             IF(PRESENT(ABLEI)) THEN
                TRRWR=TRFAS(AD,SD,RU,KW,RT,ABLEI(1),ABLEI(2))
             ELSE
                TRRWR=TRFAS(AD,SD,RU,KW,RT)
             ENDIF
          ELSEIF(KU.EQ.2) THEN
             AS=PAS(3)
             SS=PAS(4)
             NPAS=4
             IF(PRESENT(ABLEI)) THEN
                TRRWR=TRFAS(AS,SS,RU,KW,RT,ABLEI(3),ABLEI(4))
             ELSE
                TRRWR=TRFAS(AS,SS,RU,KW,RT)
             ENDIF
          ENDIF
      ELSE IF(CDE(1:1).EQ.'G') THEN
C
C
C         Transversal (deckend)
C          
C
          ST=PAS(3)
          NPAS=3
          IF(PRESENT(ABLEI)) THEN
            TRRWR=REWEMED(AD,SD,ST,RU,KW,ABLEI(1),ABLEI(2),ABLEI(3))
          ELSE
            TRRWR=REWEMED(AD,SD,ST,RU,KW)
          ENDIF
      ELSE IF(CDE(1:1).EQ.'W') THEN
C
C
C         Transversal (transparent)
C          
C
          ST=PAS(3)
          NPAS=3
          IF(RT.EQ.1) THEN
C
C           Transmission
C
            IF(PRESENT(ABLEI)) THEN
              TRRWR=TEWEMET(AD,SD,ST,RU,KW,ABLEI(1),ABLEI(2),ABLEI(3))
            ELSE
              TRRWR=TEWEMET(AD,SD,ST,RU,KW)
            ENDIF
          ELSE
C
C           Remission
C
            IF(PRESENT(ABLEI)) THEN
              TRRWR=REWEMET(AD,SD,ST,RU,KW,ABLEI(1),ABLEI(2),ABLEI(3))
            ELSE
              TRRWR=REWEMET(AD,SD,ST,RU,KW)
            ENDIF
          ENDIF
                   
C     ELSE IF(CDE(1:1).EQ.'sonst') THEN
C
C
      ENDIF
C
C
C     Absolutbetrag der Ableitungen > DABL setzen
      IF(PRESENT(ABLEI)) THEN
        DO I=1,NPAS
          ABLEI(I)=SIGN(MAX(ABS(ABLEI(I)),DABL),ABLEI(I))
        END DO
      ENDIF
      RETURN
      END
C
C
C     Dummy Programme (mit Berücksichtigung von Stützstellen) s.GRDSTZ
C
      REAL(KIND=8) FUNCTION TRSTZ(A,S,RD,KW,RT,KU,ABLEI)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTEGER(KIND=4) ::KW,RT,KU
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL,DIMENSION(*)  ::ABLEI
      INTERFACE
      REAL(KIND=8) FUNCTION TRFAS(A,S,RD,KW,RT,TRFAA,TRFSS)
      INTEGER(KIND=4) ::KW,RT
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::TRFAA,TRFSS
      END FUNCTION
      END INTERFACE
C
C
      TRSTZ=TRFAS(A,S,RD,KW,RT,ABLEI(1),ABLEI(2))
      RETURN
      END FUNCTION
C
C
      SUBROUTINE STUETZST()
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      RETURN
      END SUBROUTINE
C
C***************************************************************************
C
      DOUBLE PRECISION FUNCTION TRFAS(A,S,RD,KW,RT,TRFAA,TRFSS)
      USE MODGKWR

C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) :: KW,RT
      REAL(KIND=8):: A,S,RD
      REAL(KIND=8),OPTIONAL  ::TRFAA,TRFSS


      INTERFACE
      REAL(KIND=8) FUNCTION REFAS(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      REAL(KIND=8) FUNCTION TEFAS(A,S,RD,KW,TEFAA,TEFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::TEFAA,TEFSS
      END FUNCTION
      END INTERFACE
C
C
C
C
C
C
      IF(PRESENT(TRFAA).NEQV.PRESENT(TRFSS)) THEN
        TRFAS=-999.
        RETURN
      ENDIF
      IF(RT.EQ.1) THEN
        IF(PRESENT(TRFAA).AND.PRESENT(TRFSS)) THEN
          TRFAS=TEFAS(A,S,RD,KW,TRFAA,TRFSS)
        ELSE
          TRFAS=TEFAS(A,S,RD,KW)
        ENDIF
      ELSE
        IF(PRESENT(TRFAA).AND.PRESENT(TRFSS)) THEN
          TRFAS=REFAS(A,S,RD,KW,TRFAA,TRFSS)
        ELSE
          TRFAS=REFAS(A,S,RD,KW)
        ENDIF
      ENDIF
      RETURN
      END

C
C***************************************************************************
C******************************************************
C******************************************************
C
C******************************************************
      DOUBLE PRECISION FUNCTION TRMESS(RB,KW,RT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) :: RT
      IF(RT.EQ.1) THEN
C
C       TRANSMISSION
C
        TRMESS=TUMESS(RB,KW)
      ELSE
C
C       REFLEXION
C
        TRMESS=RUMESS(RB,KW)
      ENDIF
      RETURN
      END
C

      DOUBLE PRECISION FUNCTION RUMESS(RB,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     BERECHNUNG DES GEMESSENEN UNTERGRUNDES AUS DEM KORRIGIRTEN UNT. (RB)
C     UND DEN OBERFLAECHENKORREKTUREN (GK) FÜR REFLEXIONSMESSUNGEN
C
C
C
      R0=GLANZ(KW)
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)
      IF(CDE(1:1).EQ.'S'.OR.CDE(1:1).EQ.'E') THEN
          GS=GK(4,KW)
          GD=GK(5,KW)
      ELSE
          GS=0.D0
          GD=1.0D0
      ENDIF
      RSS=GS*RB
      RDD=GD*RB
      RSD=(1.D0-GS)*RB
      H1=1.D0-R00*RSS
      H2=(1.D0-R11)*RSD/(1.D0-R11*RDD)
      RUMESS=AI*(R0+((1.0-R00)*((1.D0-R00)*RSS+H2)/H1))
      RETURN
      END
C******************************************************
C******************************************************
C
      DOUBLE PRECISION FUNCTION TUMESS(RB,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     BERECHNUNG DES GEMESSENEN UNTERGRUNDES AUS DEM KORRIGIRTEN UNT. (RB=R2 (Buch))
C     UND DEN OBERFLAECHENKORREKTUREN (GK) FÜR TRANSMISSIONSMESSUNGEN
C
C
C
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)
      IF(CDE(1:1).EQ.'S'.OR.CDE(1:1).EQ.'E') THEN
          GS=GK(4,KW)
          GD=GK(5,KW)
      ELSE
          GS=0.D0
          GD=1.0D0
      ENDIF
      RSS=GS*RB
      RDD=GD*RB
      RSD=(1.D0-GS)*RB
      H1=1.D0-R00*RSS
      H2=(1.D0-RDD)*RSD*R11/(1.D0-R11*RDD)
      TUMESS=AI*(((1.0-R00)*((1.D0-RSS-RSD)+H2)/H1))
      RETURN
      END

C
C
C
C     Berechnung der Reflexions- und Transmissionswerte (Untergründe)
C
C
C
C******************************************************
      DOUBLE PRECISION FUNCTION TRKORR(RU,KW,RT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) :: RT
      IF(RT.EQ.1) THEN
C
C       TRANSMISSION
C
        TRKORR=TUKORR(RU,KW)
      ELSE
C
C       REFLEXION
C
        TRKORR=RUKORR(RU,KW)
      ENDIF
      RETURN
      END
C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION RUKORR(RU,KW)
C
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     BERECHNUNG DES KORRIGIERTEN UNTERGRUNDES AUS DEM GEMESSENEN UNT. (RB)
C     UND DEN OBERFLAECHENKORREKTUREN (GK) FUER REFLEXION
C
C
      R0=GLANZ(KW)
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)
      IF(CDE(1:1).EQ.'S'.OR.CDE(1:1).EQ.'E') THEN
          GS=GK(4,KW)
          GG=GK(5,KW)
      ELSE
          GS=0.D0
          GG=1.0D0
      ENDIF
      GD=1.D0-GS
      V=(RU/AI-R0)/((1.0-R00))
      IF(V.GT.1.0) THEN
        V=1.0
      ENDIF
      BB=(1.D0-R11)*GD+V*R00*GS+(1.D0-R00)*GS+V*R11*GG
      SS=DSQRT(DABS(1.D0-4.*V*R11*GS*GG*(V*R00+1.D0-R00)/(BB*BB)))
      RUKORR=2.*V/(BB*(1.D0+SS))
      IF(RUKORR.LT.0.0) THEN
         RUKORR=0.0
      ENDIF
      RETURN
      END
C
C
C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION TUKORR(TU,KW)
C
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     BERECHNUNG DES KORRIGIERTEN UNTERGRUNDES AUS DER GEMESSENEN Transmission TU
C     UND DEN OBERFLAECHENKORREKTUREN (GK) FÜR TRANSMISSION
C     TUKORR = R2 im Buch
C
C
C
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)
      IF(CDE(1:1).EQ.'S'.OR.CDE(1:1).EQ.'E') THEN
          GS=GK(4,KW)
          GG=GK(5,KW)
      ELSE
          GS=0.D0
          GG=1.
      ENDIF
      W=TU/(AI*(1.0-R00))
      IF(W.GT.1.0) THEN
        W=1.0
      ENDIF
      WH=(1.-R11)+(1.-W)*R11*GG+GS*(R11-W*R00)
      WG=R11*GS*GG*(1.-R00*W)*(1.-W)
      TUKORR=2.*(1.-W)/(WH*(1.+DSQRT(DABS(1.-4.*WG/WH**2))))
      RETURN
      END
C
C
C     Ableitungen
c
C******************************************************
      DOUBLE PRECISION FUNCTION DRTRMESS(RB,KW,RT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) :: RT
      IF(RT.EQ.1) THEN
C
C       TRANSMISSION
C
        DRTRMESS=DRTUMESS(RB,KW)
      ELSE
C
C       REFLEXION
C
        DRTRMESS=DRRUMESS(RB,KW)
      ENDIF
      RETURN
      END
C

C
C******************************************************
C******************************************************
C
      DOUBLE PRECISION FUNCTION DRRUMESS(RB,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     ABLEITUNG DRRU/DRRB
C
C
C
      R0=GLANZ(KW)
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)
      IF(CDE(1:1).EQ.'S'.OR.CDE(1:1).EQ.'E') THEN
          GS=GK(4,KW)
          GD=GK(5,KW)
      ELSE
          GS=0.D0
          GD=1.0D0
      ENDIF
      RSS=GS*RB
      RDD=GD*RB
      RSD=(1.D0-GS)*RB
      RU=RUMESS(RB,KW)
      V=(RU/AI-R0)/(1.0-R00)
      HH=R11*(1.0D0-R11)*RDD*RSD/(1.-R11*RDD)**2
      DRRUMESS=AI*(1.0-R00)*(V+HH)/(RB*(1.0D0-R00*RSS))
      RETURN
      END
C******************************************************
C******************************************************
C
      DOUBLE PRECISION FUNCTION DRTUMESS(RB,KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     ABLEITUNG DRTU/DRRB
C
C
C
      R00=GLINZ(KW)
      R11=GK(3,KW)
      AI=GINTE(KW)
      IF(CDE(1:1).EQ.'S'.OR.CDE(1:1).EQ.'E') THEN
          GS=GK(4,KW)
          GD=GK(5,KW)
      ELSE
          GS=0.D0
          GD=1.0D0
      ENDIF
      RSS=GS*RB
      RDD=GD*RB
      RSD=(1.D0-GS)*RB
      TU=TUMESS(RB,KW)
      W=TU/(AI*(1.0-R00))
      HH=R11*RSD*RDD*(1.0D0-R11)/(1.D0-R11*RDD)**2
      DRTUMESS=AI*(1.0-R00)*(W-1.D0-HH)/(RB*(1.D0-R00*RSS))
      RETURN
      END


C
C
C     Spezille Funktionen
C
C
C
C
      DOUBLE PRECISION FUNCTION FTAU(TAU,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION DTAU
      CHARACTER*1 CSCH
      DATA WUPI/1.12837916709551257/
C     IKENN=IFIX(GINTE(KW)+0.5)
C     CSCH=CDE(2:2)
C     DIE TRANSMISSION WIRD DURCH TNEU=T**(TAU**(1.+POT)) ERSETZT
C
C
C     CSCH=0         TAU==>TAU
C     CSCH=1         TAU==>0.5*LOG(1.+TAU+SQRT(TAU*(2.+TAU)))  KUBELKA MUNK
C     CSCH=2         TAU==>ARSINH(TAU)
C     CSCH=3         TAU==>ERF(TAU)  
C     CSCH=4         TAU==>ARCTAN(TAU)

C     NUR CSCH=0 IST ERLAUBT
C
      POT=-GK(11,KW)
      CSCH='0'
      TAV=TAU
      IF(POT.NE.0.D0) THEN
            IF(ABS(TAU).NE.0.) THEN
                   THH=POT*LOG(ABS(TAU))
                   TAV=0.0                    
                   IF(THH.GT.-80.) THEN
                     TAV=TAU*EXP(THH)
                   ELSE IF(THH.GT.80) THEN
                     TAV=1.D80
                   ENDIF 
            ENDIF
            IF(ABS(TAV).LT.RANGIN) TAV=0.
      ENDIF
      IF(CSCH.EQ.'1') THEN 
             SIG=SIGN(1.D0,TAU)
             TAV=ABS(TAV)
             IF(TAV.LT.1.D0) THEN
                    SQ=DSQRT(ABS(TAV*(2.D0+TAV)))
             ELSE
                    SQ=TAV*DSQRT(1.D0+2./TAV)
             ENDIF
             TAV=SIG*0.5*LOG(1.D0+TAV+SQ)
      ENDIF
      IF(CSCH.EQ.'2') THEN
             TAV=ARSINH(TAV)   
      ENDIF
      IF(CSCH.EQ.'3') THEN
             TAV=ERF(TAV)
      ENDIF
      IF(CSCH.EQ.'4') THEN
             TAV=ATAN(TAV)
      ENDIF
      FTAU=TAV
      RETURN
      ENTRY DTAU(TAU,KW)
C     CSCH=CDE(2:2)
C     
C
C     NUR CSCH=0 IST ERLAUBT
C
      POT=-GK(11,KW)
      CSCH='0'
      TAV=TAU
      DTDX=1.
      IF(POT.NE.0.D0) THEN
            IF(ABS(TAU).GE.TOL) THEN
                   TAV=TAU*(ABS(TAU)**POT)
                   DTDX=(1.D0+POT)*TAV/TAU
            ELSE
                   DTDX=(1.D0+POT)*TOL**(POT)
            ENDIF
            IF(ABS(DTDX).LT.RANGIN) DTDX=0.D0
      ENDIF
      IF(CSCH.EQ.'1') THEN
             TAV=ABS(TAV)
             IF(TAV.LT.1.D0) THEN
                    SQ=DSQRT(ABS(TAV*(2.D0+TAV)))
             ELSE
                    SQ=TAV*DSQRT(1.D0+2./TAV)
             ENDIF
              HHHH=SQ                          
              IF(HHHH.LT.TOL) HHHH=TOL
              DTDX=0.5*DTDX/HHHH
      ENDIF
      IF(CSCH.EQ.'2') THEN
              HHHH=1./DSQRT(1.D0+TAV*TAV)
              DTDX=DTDX*HHHH
      ENDIF
      IF(CSCH.EQ.'3') THEN
              HHHH=WUPI*GSS(TAV)
              DTDX=DTDX*HHHH
      ENDIF
      IF(CSCH.EQ.'4') THEN
              HHHH=1./(1.D0+TAV*TAV)
              DTDX=DTDX*HHHH
      ENDIF
      DTAU=DTDX
      RETURN
      END
C
C
C
C
      DOUBLE PRECISION FUNCTION STAINT(C,NST,CST,TST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TST(*),CST(*)
C
C
C     BERECHNUNG VON POLYGONZUEGEN    
C     AUS VORGEGEBENEN ABSZISSENWERTEN CST UND
C     DEN DORT VORGEGEBENEN ABLEITUNGEN TST
C     GEGEBEN CST(I),TST(I) FUER I=1.....NST
C      

      DO I=1,NST
         IF(C.LT.CST(I)) GOTO 20
      ENDDO
      I=NST
  20  J=I 
      IF(J.EQ.1) THEN
          CU=C
      ELSE
          CU=C-CST(J-1)
      ENDIF
      S1=0.
      DO K=1,J-1
         IF(K.EQ.1) THEN
           S1=S1+TST(K)*CST(K)
         ELSE
           S1=S1+TST(K)*(CST(K)-CST(K-1))
         ENDIF
      ENDDO
      STAINT=S1+TST(J)*CU
      RETURN
      END

C
C
C******************************************************************************
C******************************************************************************
C
      DOUBLE PRECISION FUNCTION STAINA(C,NST,CST,TST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TST(*),CST(*)
C
C
C     ABLEITUNG NACH C
C
C
C
      DO I=1,NST
         IF(C.LT.CST(I)) GOTO 20
      ENDDO
      I=NST
  20  J=I 
      STAINA=TST(J)
      RETURN
      END
      DOUBLE PRECISION FUNCTION STATST(C,NST,CST,TST,M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TST(*),CST(*)
      DATA DEL/1.D-4/,DELTA/5.D-6/
C
C     ABLEITUNGEN VON STAINT NACH TST(M)
C
      DDEL=DEL+DELTA*TST(M)
      FU=STAINT(C,NST,CST,TST)
      TST(M)=TST(M)+DDEL
      STATST=(STAINT(C,NST,CST,TST)-FU)/DDEL
      TST(M)=TST(M)-DDEL
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION STUINT(C,NST,CST,TST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION STUINA
      DIMENSION TST(*),CST(*),D(0:NST+1 )
      DATA ALA/0.0/,AM/0.0D0/
      KENN=0
      GOTO 10
      ENTRY STUINA(C,NST,CST,TST)
C     Ableitung nach C
      KENN=1
C
C
C     BERECHNUNG VON EXPONENTIELLEN SPLINES
C     AUS VORGEGEBENEN ABSZISSENWERTEN CST UND
C     DEN DORT VORGEGEBENEN ABLEITUNGEN TST
C     GEGEBEN CST(I),TST(I) FUER I=1.....NST
C
 10   IF(NST.EQ.1) THEN
        IF(KENN.EQ.0) THEN
           STUINT=C*TST(NST)
        ELSE
           STUINA=TST(NST)
        ENDIF
      ELSE
C
C       2. Ableitungen an Rändern werden auf 0 gesetzt.
C
C
C
        D(0)=0.0
        D(NST)=0.0
C
C       Berechnung der 2. Ableitungen
C
C
        DO I=NST-1,1,-1
           H=CST(I+1)-CST(I)
           AMU=H*ALA
           D(I)=-D(I+1)
     &     +(TST(I+1)-TST(I))/(H*(VRQ(AMU,1.D0)-VRQ(AMU,0.D0)))
        END DO
C
C
C
        YI=0.0
        YIP=0.
        DO I=1,NST
         IF(I.EQ.1) THEN
           H=CST(I)
           AMU=H*ALA
C
C          Ableitung für C=0 berechnen
C
           Y00=TST(1)-H*(D(1)+D(0))*(VRQ(AMU,1.D0)-VRQ(AMU,0.D0))
           YIP=YI+H*(Y00+H*D(0)*VRQ(AMU,1.D0)-H*D(1)*VRQ(AMU,0.D0))
         ELSE
           H=CST(I)-CST(I-1)
           AMU=H*ALA
           YIP=YI+
     &     H*(TST(I-1)+H*VRQ(AMU,1.D0)*D(I-1)-H*VRQ(AMU,0.D0)*D(I))
         ENDIF
          IF(C.LE.CST(I).OR.I.EQ.NST) THEN
          IF(I.EQ.1) THEN
            T=C/H
          ELSE
            T=(C-CST(I-1))/H
          ENDIF
          IF(KENN.EQ.0) THEN
             STUINT=YIP*T+YI*(1.D0-T)+(D(I)*VVV(AMU,T)
     &       +D(I-1)*VVV(AMU,1.D0-T))*H**2
          ELSE
             STUINA=(YIP-YI)/H+
     &              H*(D(I)*VRQ(AMU,T)-D(I-1)*VRQ(AMU,1.D0-T))
          ENDIF
          EXIT
         ELSE
          YI=YIP
         ENDIF
        END DO
      ENDIF
      RETURN
      END
      
      DOUBLE PRECISION FUNCTION STUTST(C,NST,CST,TST,M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TST(*),CST(*)
      DATA DEL/1.D-4/,DELTA/5.D-6/
C
C     ABLEITUNGEN VON STUINT NACH TST(M)
C
      DDEL=DEL+DELTA*TST(M)
      FU=STUINT(C,NST,CST,TST)
      TST(M)=TST(M)+DDEL
      STUTST=(STUINT(C,NST,CST,TST)-FU)/DDEL
      TST(M)=TST(M)-DDEL
      RETURN
      END
      DOUBLE PRECISION FUNCTION STUTAT(C,NST,CST,TST,M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TST(*),CST(*)
      DATA DEL/1.D-4/,DELTA/5.D-6/
C
C     ABLEITUNGEN VON STUINA NACH TST(M)
C
      DDEL=DEL+DELTA*TST(M)
      FU=STUINA(C,NST,CST,TST)
      TST(M)=TST(M)+DDEL
      STUTAT=(STUINA(C,NST,CST,TST)-FU)/DDEL
      TST(M)=TST(M)-DDEL
      RETURN
      END
      DOUBLE PRECISION FUNCTION STUCST(C,NST,CST,TST,M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TST(*),CST(*)
      DATA DEL/1.D-4/,DELTA/5.D-6/
C
C     ABLEITUNGEN VON STUINT NACH CST(M)
C
      DDEL=DEL+DELTA*CST(M)
      FU=STUINT(C,NST,CST,TST)
      CST(M)=CST(M)+DDEL
      STUCST=(STUINT(C,NST,CST,TST)-FU)/DDEL
      TST(M)=CST(M)-DDEL
      RETURN
      END
      DOUBLE PRECISION FUNCTION STUCAT(C,NST,CST,TST,M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TST(*),CST(*)
      DATA DEL/1.D-4/,DELTA/5.D-6/
C
C     ABLEITUNGEN VON STUINA NACH CST(M)
C
      DDEL=DEL+DELTA*CST(M)
      FU=STUINA(C,NST,CST,TST)
      CST(M)=CST(M)+DDEL
      STUCAT=(STUINA(C,NST,CST,TST)-FU)/DDEL
      TST(M)=CST(M)-DDEL
      RETURN
      END

      DOUBLE PRECISION FUNCTION STACST(C,NST,CST,TST,M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TST(*),CST(*)
      DATA DEL/1.D-4/,DELTA/5.D-6/
C
C     ABLEITUNGEN VON STAINT NACH CST(M)
C
      DDEL=DEL+DELTA*CST(M)
      FU=STAINT(C,NST,CST,TST)
      CST(M)=CST(M)+DDEL
      STACST=(STAINT(C,NST,CST,TST)-FU)/DDEL
      CST(M)=CST(M)-DDEL
      RETURN
      END

      DOUBLE PRECISION FUNCTION VRQ(AMU,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA FA2/0.5D0/,FA3/1.666666667D-1/
      DATA FA4/4.1666666667D-2/FA5/8.333333333D-3/
C
C     SUMME VRQ(AMU,0)=-R(I+1); VRQ(AMU,0)=Q(I) (NUMERISCHE MATHEMATIK 35(1980) S 81: COMPUTATION OF THE EXPONENTIAL SPLINES)
      IF (AMU.GT.0.001) THEN
        VRQ= COSH(AMU*T)/(AMU*SINH(AMU))-1./AMU**2
      ELSE
        VRQ=(FA2*T*T-FA3+(FA4*T**4-FA5)*AMU**2)/(1.+FA3*AMU**2)
      END IF
      RETURN
      END
      DOUBLE PRECISION FUNCTION VVV(AMU,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA FA3/1.666666667D-1/,FA5/8.333333333D-3/
      IF(AMU.GT.0.001) THEN
        VVV=(SINH(AMU*T)/SINH(AMU)-T)/AMU**2
      ELSE
        VVV=(FA3*T*(T**2-1)+FA5*T*(T**4-1)*AMU**2)/(1.+FA3*AMU**2)
      ENDIF
      END
C******************************************************************************
C******************************************************************************
C
C
C
C
      DOUBLE PRECISION FUNCTION TAUINT(C,CST,TST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION TAUINA
      DIMENSION TST(4),CST(4),H(4),CSA(0:4),TSA(0:4)
      DIMENSION A(3,3),S(0:4)
      KENN=0
      GOTO 10
      ENTRY TAUINA(C,CST,TST)
      KENN=1
 10   TSA(0)=0.D0    
      CSA(0)=0.D0
      DO I=1,4
         TSA(I)=TST(I)
         CSA(I)=CST(I)
         H(I)=CSA(I)-CSA(I-1)
      ENDDO
C
C
C     BERECHNUNG VON SPLINES 
C     AUS VORGEGEBENEN ABSZISSEN CSA UND ORDINATEN TSA-WERTEN
C     GEGEBEN CSA(I),TSA(I) FUER I=0.....4
C     DIE 2. ABLEITUNGEN FUER I=0 UND I=4 SIND = 0.
C      
      AN1=(H(1)+H(2))/3.
      AN2=(H(2)+H(3))/3.
      AN3=(H(3)+H(4))/3.
      AL1=H(2)/6.
      AL2=H(3)/6.
      DET=AN1*AN2*AN3-AL1*AL1*AN3-AL2*AL2*AN1      
      A(1,1)=(AN2*AN3-AL2*AL2)/DET                                   
      A(3,3)=(AN1*AN2-AL1*AL1)/DET                                      
      A(2,2)=AN1*AN3/DET                                
      A(1,2)=-AL1*AN3/DET                           
      A(1,3)=AL1*AL2/DET                 
      A(2,3)=-AL2*AN1/DET                       
      A(2,1)=A(1,2)
      A(3,2)=A(2,3)
      A(3,1)=A(1,3) 
      DO I=1,4
         IF(C.LT.CST(I)) GOTO 20
      ENDDO
      I=4
  20  J=I 
      IF(J.GT.4) J=4
      IF(J.LT.1) J=1
      DO K=J-1,J  
        SUM=0.
        IF(K.EQ.0.OR.K.EQ.4) THEN
           S(K)=0.
        ELSE 
           DO I=1,3
              SUM=SUM+A(K,I)*((TSA(I+1)-TSA(I))/H(I+1)
     &                       -(TSA(I)-TSA(I-1))/H(I))
           ENDDO
           S(K)=SUM
        ENDIF
      ENDDO
      CO=C-CSA(J)
      CU=C-CSA(J-1)
      IF(KENN.EQ.0) THEN
         TAUINT=(-S(J-1)*CO**3+S(J)*CU**3)/(6.*H(J))
     &         +(TSA(J)*CU-TSA(J-1)*CO)/H(J)
     &         -(  S(J)*CU-  S(J-1)*CO)*H(J)/6.           
      ELSE
         TAUINA=(-S(J-1)*CO**2+S(J)*CU**2)/(2.*H(J))
     &         +(TSA(J)-TSA(J-1))/H(J)
     &         -(  S(J)-  S(J-1))*H(J)/6.           
      ENDIF
      RETURN
      END
    
      DOUBLE PRECISION FUNCTION TAUABL(C,CST,TST,M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TST(4),CST(4),H(4),CSA(0:4)
      DIMENSION A(3,3),S(0:4)
C     TSA(0)=0.D0 
      CSA(0)=0.D0
      DO I=1,4
C        TSA(I)=TST(I)
         CSA(I)=CST(I)
         H(I)=CSA(I)-CSA(I-1)
      ENDDO
      AN1=(H(1)+H(2))/3.
      AN2=(H(2)+H(3))/3.
      AN3=(H(3)+H(4))/3.
      AL1=H(2)/6.
      AL2=H(3)/6.
      DET=AN1*AN2*AN3-AL1*AL1*AN3-AL2*AL2*AN1      
      A(1,1)=(AN2*AN3-AL2*AL2)/DET                                   
      A(3,3)=(AN1*AN2-AL1*AL1)/DET                                      
      A(2,2)=AN1*AN3/DET                                
      A(1,2)=-AL1*AN3/DET                           
      A(1,3)=AL1*AL2/DET                 
      A(2,3)=-AL2*AN1/DET                       
      A(2,1)=A(1,2)
      A(3,2)=A(2,3)
      A(3,1)=A(1,3) 
      DO I=1,4
         IF(C.LT.CST(I)) GOTO 20
      ENDDO
      I=4
  20  J=I 
      IF(J.GT.4) J=4
      IF(J.LT.1) J=1
      DO K=J-1,J  
        SUM=0.
        IF(K.EQ.0.OR.K.EQ.4) THEN
           S(K)=0.
        ELSE 
           IF(M.EQ.1) THEN
C             TST(0)=0.
C             S(K)=-A(K,1)*(1./H(2)+1./H(1))+A(K,2)/H(2)
C             TST(0)=TST(1)
              S(K)=-A(K,1)*(1./H(2)        )+A(K,2)/H(2)
           ELSE IF(M.EQ.2) THEN
              S(K)=A(K,1)/H(2)-A(K,2)*(1./H(3)+1./H(2))+A(K,3)/H(3)
           ELSE IF(M.EQ.3) THEN
              S(K)=A(K,2)/H(3)-A(K,3)*(1./H(4)+1./H(3))
           ELSE IF(M.EQ.4) THEN
              S(K)=A(K,3)/H(4)
           ENDIF 
        ENDIF
      ENDDO
      CO=C-CSA(J)
      CU=C-CSA(J-1)
      SUM=(-S(J-1)*CO**3+S(J)*CU**3)/(6.*H(J))
     &      -(S(J)*CU-S(J-1)*CO)*H(J)/6.
      IF(M.EQ.J) SUM=SUM+CU/H(J)
      IF(M.EQ.J-1) SUM=SUM-CO/H(J)      
      TAUABL=SUM
      RETURN
      END
    

C

C
C
C     Hilfsfunktionen
C
c
c
c
c
      DOUBLE PRECISION FUNCTION FEEE(X,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA ZERO/0.D0/
      IF(ABS(Y).GT.ABS(X)) THEN
         X1=X
         Y1=Y
      ELSE
         X1=Y
         Y1=X
      ENDIF
      IF(ABS(Y1).GT.0.00750D0) THEN
         FEEE=(UEEE(X1,Y1,ZERO)    
     &       -UEEE(X1,-Y1,2.*Y1))/(2.*Y1)
      ELSE
         FEEE=0.33333333334*(1.D0-X1)*(1.D0-Y1)+0.2*(X1*X1+Y1*Y1)     
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION GEEE(X,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF(ABS(Y).GT.ABS(X)) THEN
         X1=X
         Y1=Y
      ELSE
         X1=Y
         Y1=X
      ENDIF
      IF(ABS(Y1).GT.0.00750D0) THEN
         GEEE=(UEEE(X1,-Y1,Y1)
     &       -UEEE(X1,Y1,Y1))/(2.*Y1)
      ELSE
         GEEE=0.16666667*(1.D0-X1)*(1.D0-Y1)+0.09166667*(X1*X1+Y1*Y1)             
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION UEEE(X,Y,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF(ABS(X).GT.0.0010D0) THEN
       UEEE=(SEEE(X+Y, Z)-SEEE(Y-X,2.*X+Z))/(2.*X)
      ELSE 
       IF(ABS(Y).GT.0.002D0) THEN
         UEEE=(SEEE(2.*X,Z)-SEEE(Y+X,Z))/(Y-X)
       ELSE
         UEEE=(0.5D0-0.1666666667*(3.*X+Y)+
     &         0.04166666667*(7.*X*X+Y*Y+4.*X*Y))*EXX(-Z)
       ENDIF
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION SEEE(X,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF(ABS(X).LT.0.0001D0) THEN
             SEEE=QEEE(X)*EXX(-Y)
      ELSE
             SEEE=(EXX(-Y)-EXX(-(X+Y)))/X
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION EXX(X) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA RANG/1.D100/,ZERO/0.D0/
      IF(X.GT.250.D0) THEN
           EXX=RANG
      ELSE IF(X.LT.-250.D0) THEN
           EXX=ZERO
      ELSE
           EXX=EXP(X)
      ENDIF
      RETURN
      END
C****************************************************************************
C 


C******************************************************
C******************************************************


      DOUBLE PRECISION FUNCTION QEXX(X)
C
C 
C     BERECHNUNG VON D((1.-EXP(-X))/X)/DX=(EXP(-X)*(X+1.)-1.)/X**2
C 
C 
C     ABRAMOWITZ S.70-71
C 
C 
C 
C 
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION QEEE
      DOUBLE PRECISION X
      DIMENSION B(6)
      DATA B/.0833334,.0166667,.0071429,.0039683,.0025253,.0017483/ 
      DATA B0/0.5/
      DATA RANG/1.D300/
C 
C 
C 
C 
      IF(ABS(X).LT.0.01) THEN
            T2=X*X
            SUU=B(1)*X/(1.D0+B(2)*T2/(1.D0+B(3)*T2/(1.D0+B(4)*T2/
     &                 (1.D0+B(5)*T2/(1.D0+B(6)*T2))))) 
            QEXX=(-B0+SUU)/(1.D0+X*(B0+SUU))
      ELSEIF(X.GT.690.) THEN
            QEXX=-1./X**2
      ELSEIF(X.LT.-690.) THEN
            QEXX=-RANG
      ELSE
            QEXX=(EXP(-X)*(X+1.D0)-1.D0)/X**2
      ENDIF
      RETURN
C
C     BERECHNUNG VON (1-EXP(-X))/X
C
C
      ENTRY QEEE(X)
      IF(ABS(X).LT.0.01) THEN
            T2=X*X
            SUU=B(1)*X/(1.D0+B(2)*T2/(1.D0+B(3)*T2/(1.D0+B(4)*T2/
     &                 (1.D0+B(5)*T2/(1.D0+B(6)*T2))))) 
            QEEE=1./(1.D0+X*(B0+SUU))

      ELSEIF(X.GT.690.) THEN
            QEEE=1./X
      ELSEIF(X.LT.-690.) THEN
            QEEE=-RANG
      ELSE
            QEEE=(1.D0-EXP(-X))/X
      ENDIF
      RETURN

      END

C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
C
C
C
C     Sonderfunktionen
C
C
c

      DOUBLE PRECISION FUNCTION ALLEN(L,G,HS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      H=0.2*(3.*G*G+2.+(1.-G*G)*HS)
      LL=L
      AA=(1.+4.5*LL-2.5*LL*LL)*G**LL
      IF(LL.GE.2) THEN
         AA=AA+1.25*(3.*H-1)*LL*(LL-1.)*G**(LL-2)    
      ENDIF
      ALLEN=AA
      RETURN
      END 
C
C
C
C
C
      DOUBLE PRECISION FUNCTION FRESG(AMU,BRECH)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     AMU=COS(PHI0)            PHI0=EINFALLSWINKEL
C     BRECH                    BRECHUNGSINDEX
C
C
      FRESG=0.5*(FRESP(AMU,BRECH)+FRESS(AMU,BRECH))
      RETURN
      END
      DOUBLE PRECISION FUNCTION FRESP(AMU,BRECH)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      HILF=1.D0-BRECH**2
      IF(AMU**2.LE.HILF) GOTO 50
      HILF=DSQRT(BRECH**2-1.D0+AMU**2)
      AB=AMU*BRECH**2
      FRESP=((AB-HILF)/(AB+HILF))**2
      RETURN
  50  FRESP=1.D0
      RETURN
      END
      DOUBLE PRECISION FUNCTION FRESS(AMU,BRECH)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      HILF=1.-BRECH**2
      IF(AMU**2.LE.HILF) GOTO 50
      HILF=DSQRT(BRECH**2-1.+AMU**2)
      AC=AMU
      FRESS=((HILF-AC)/(HILF+AC))**2
      RETURN
  50  FRESS=1.D0
      RETURN
      END
C
C
C     FUNCTIONEN FUER FARKOR
C
      SUBROUTINE FASTR(XYZT,XYZP,XYZU,CT,CP,AKS,FST,NN,KW,IJ)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C 
      DIMENSION XYZT(*),XYZP(*),XYZU(*)
C 
C 
C     BERECHNUNG DER FARBSTAERKE AUS DEM KLEINSTEN DER X,Y,Z-WERTE oder aus Y-Wert (IJ=2)
C     NACH KUBELKA-MUNK FUER DECKENDE SCHICHTEN 
C     GK(2) = INNERER GERICHTETER GLANZANTEIL (Z.B. 0.04) 
C     GK(3) = INNERER DIFFUSER GLANZANTEIL (Z.B. 0.60)
C 
C 
      XM=1.D80
      IF(IJ.GE.1.AND.IJ.LE.3) THEN
        II=IJ
      ELSE
        DO 10 I=1,3 
          XX=XYZT(I)/FAKT(I,NN) 
          IF(XX.GT.XM) GOTO 10
          II=I
          XM=XX 
  10    CONTINUE
      ENDIF
      XU=XYZU(II)/FAKT(II,NN)
      FU=AKUMU(XU,0)
      XT=XYZT(II)/FAKT(II,NN) 
      XP=XYZP(II)/FAKT(II,NN) 
C     WRITE(1,'('' xT xP xU '',3e10.3)') xT,xp,xu
      FT=AKUMU(XT,0)-FU
      FP=AKUMU(XP,0)-FU
C     write(1,'('' cp ft '',2e10.3)') cp,ft
      FST=(CT*FP)/(CP*FT+TINY(1.))
      AKS=FP
      RETURN
      END 
C                           +-------+ 
C                           ! FATRA ! 
C                           +-------+ 
      SUBROUTINE FATRA(XYZT,XYZP,XYZU,CT,CP,EXT,FST,NN,IRT,KW,IJ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C 
      DIMENSION XYZT(*),XYZP(*),XYZU(*)
C
C 
C     BERECHNUNG DER FARBSTAERKE AUS DEM KLEINSTEN DER X,Y,Z-WERTE  
C     NACH LAMBERT-BEER FUER TRANSPARENTE SCHICHTEN (RU = 100%)
C     GK(2) = INNERER GERICHTETER GLANZANTEIL (Z.B. 0.04) 
C     GK(3) = INNERER DIFFUSER GLANZANTEIL (Z.B. 0.60)
  
C
      XM=1.D80
      IF(IJ.GE.1.AND.IJ.LE.3) THEN
        II=IJ
      ELSE
        DO 10 I=1,3
          XX=XYZT(I)/FAKT(I,NN) 
          IF(XX.GT.XM) GOTO 10
          II=I
          XM=XX 
  10    CONTINUE
      ENDIF
      XT=XYZT(II)/FAKT(II,NN) 
      XP=XYZP(II)/FAKT(II,NN) 
      XU=XYZU(II)/FAKT(II,NN)
      FT=AABEE(XT,0,XU,IRT)
      FP=AABEE(XP,0,XU,IRT)
      FST=(CT*FP)/(CP*FT+TINY(1.))
      EXT=FP
      RETURN
      END 
C                           +-------+ 
C                           ! FATXT ! 
C                           +-------+ 
      SUBROUTINE FATXT(NWE,RT,RM,RU,CT,CP,AKSG,AKSU,FSTG,FSTU,K,KW)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RT(*),RM(*),RU(*),ASK(NWE)
C
C     KUBELKA-MUNK(DECKEND) SUMME UEBER WELLENLAENGEN
C     RU GEMESSENER REFLEXIONSWERT DES UNTERGRUNDES (SUBSTRATS)
C     
      DO 10 I=1,NWE
      ASU=AKUMU(RU(I),KW)
      ASK(I)=AKUMU(RT(I),KW)-ASU
  10  CONTINUE
      FTU=SUM(ASK)
      CALL GEWICH(NWE,ASK,FT,K)
      DO 20 I=1,NWE
      ASU=AKUMU(RU(I),KW)
      ASK(I)=AKUMU(RM(I),KW)-ASU
  20  CONTINUE
      CALL GEWICH(NWE,ASK,FP,K)
      FSTG=(CT*FP)/(CP*FT+TINY(1.))
      AKSG=FP/(FAKT(1,K)+FAKT(2,K)+FAKT(3,K))
      FSTU=SUM(ASK)
      AKSU=FSTU/NWE
      FSTU=FSTU/(FTU+TINY(1.0))
      RETURN
      END
C                           +-------+ 
C                           ! FALMB ! 
C                           +-------+ 
      SUBROUTINE FALMB(NWE,RT,RM,RU,CT,CP,EXTG,EXTU,FSTG,FSTU,K,IRT,KW)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RT(*),RM(*),RU(*),ASK(NWE)
C
C     LAMBERT-BEER(TRANSPARENT) SUMME UEBER WELLENLAENGEN
C     
      DO 10 I=1,NWE
      ASK(I)=AABEE(RT(I),KW,RU(I),IRT)
  10  CONTINUE
      FTU=SUM(ASK)
      CALL GEWICH(NWE,ASK,FT,K)
      DO 20 I=1,NWE
      ASK(I)=AABEE(RM(I),KW,RU(I),IRT)
  20  CONTINUE
      CALL GEWICH(NWE,ASK,FP,K)
      FSTG=(CT*FP)/(CP*FT+TINY(1.))
      EXTG=FP/(FAKT(1,K)+FAKT(2,K)+FAKT(3,K))
      FSTU=SUM(ASK)
      EXTU=FSTU/NWE
      FSTU=FSTU/(FTU+TINY(1.))
      RETURN
      END
C
C
C
      DOUBLE PRECISION FUNCTION AKUMU(R,KW)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

C
C     K/S-WERTE
C
C     KUBELKA-MUNK(DECKEND) MIT SAUNDERSON-KORREKTUR
C
      RH=R-GLANZ(KW)
      IF(RH.LT.EEP) RH=EEP
      R1=GK(2,KW)
      R2=GK(3,KW)
      RR=(GINTE(KW)-R1)*(1.-R2)
      AK=RH/(RH*R2+RR)
      AKUMU=(1.-AK)**2/(2.*AK)
      RETURN
      END
      DOUBLE PRECISION FUNCTION AABEE(R,KW,RU,RT)
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) :: RT
C
C     OPTISCHE DICHTE (NATUERLICHER LOGARITHMUS)
C
C     LAMBERT-BEER MIT SAUNDERSON-KORREKTUR
C     RU KORRIGIERTE R-WERTE DES UNTERGRUNDES
C
C     FOPT=FAKTOR (z.B. =.4342945 zur Umrechnung in dekadischen Logarithmus)
C                          2 zur Beruecksichtigung des halben Lichtdurchgangs bei Transmission
C                          .8685889 beides
C 
C
      R0=GLANZ(KW)
      R1=GK(2,KW)
      R2=GK(3,KW)
      IF(RT.EQ.0) THEN
C       Tau für Remission
        RR=(GINTE(KW)-R1)*(1.-R2)
        RH=R-R0
        IF(RH.LT.0.) RH=0.
        AK=RH/(RH*R2+RR)
        AABEE=-0.5*FOPT*LOG(DABS(AK/RU+EEP))
C
      ELSE
C
C       Tau für Transmission
C
        RR=(GINTE(KW)-R1)*(1.-RU)
        AABEE=-FOPT*LOG(2.*R/(RR+SQRT(RR**2+4.*R*RU*R2)))
      ENDIF
      RETURN
      END    


      DOUBLE PRECISION FUNCTION GLZNOG(KW,IRT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      GLZNOG=(1.-IRT)*GLANZ(KW)
      RETURN
      END

      DOUBLE PRECISION FUNCTION GLANZ(KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      GLANZ=GK(1,KW)
      RETURN
      END
      DOUBLE PRECISION FUNCTION GLINZ(KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        GLINZ=GK(2,KW)
      RETURN
      END
      DOUBLE PRECISION FUNCTION GINTE(KW)
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        GINTE=GK(10,KW)+1.0D-12
      RETURN
      END
C
C
c
      REAL(KIND=8) FUNCTION FRKORRI(AKK,RR2,RR1,RRN,KW)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) :: KW
      REAL(KIND=8) :: RRN,RR1,RR2,AKK
C
C     Berechnung korrigierter R-Werte aus 3 R-werten z.B
C     für Nachstellung über schwarzem Untergrund
C     RR2 Rechnung für schwarzen Untergrund
C     RR1 Rechnung für weißen Untergrund
C     RRN Nachstellung über weißem Unteergrund
C
C
C     oder
C
C
C     RR2=gemessene R-Werte für Hilfskorrektur (Nachstellung) vor Korrektur mit R-Werten der Vorlage
C     RR1=gemessene R-Werte für Hilfskorrektur (Vorlage)
C     RRN=gemessene R-Werte der Vorlage
C
c
c
      RHHH=RSCHL(AKK,RRN,KW)+RSCHL(AKK,RR2,KW)-RSCHL(AKK,RR1,KW)
      FRKORRI=RRUECK(AKK,RHHH,KW)
      RETURN
      END FUNCTION

C
C
C
C
c
c
c
c
c
      REAL(KIND=8) FUNCTION RSCHL(ALPH,R,KW)
      USE MODGKWR

      IMPLICIT NONE
      REAL(KIND=8) :: ALPH,R,HIR,HLFR,R0,R00,AI,GLINZ,GLANZ,GINTE,ASCHL
      INTEGER(KIND=4) ::KW

C
C     korrigierte R-Werte (R-Schlange) aus R-Werten berechnen
C
C
c      R0=GLANZ(KW)
c      R00=GLINZ(KW)
      R0=0.0D0
      R00=0.0D0
      AI=GINTE(KW)
      HIR=(R/AI-R0)/((1.0D0-R00)+TOL)
      RSCHL=ASCHL(ALPH,HIR)
      RETURN
      END
C
C
C
      REAL(KIND=8) FUNCTION RRUECK(ALPH,RSCH,KW)
      USE MODGKWR

      IMPLICIT NONE
      REAL(KIND=8) :: ALPH,RSCH,RRUE,RH,R0,R00,AI,
     &                GLINZ,GLANZ,GINTE,ARUECK
      INTEGER(KIND=4) ::KW
C
C     R-Werte aus R-Schlange berechnen
C
C
c      R0=GLANZ(KW)
c      R00=GLINZ(KW)
      R0=0.0D0
      R00=0.0D0
      AI=GINTE(KW)
      RH=ARUECK(ALPH,RSCH)
      RRUECK=AI*(R0+(RH-TOL)*(1.0D0-R00))
      RETURN
      END
C
C
C
      REAL(KIND=8) FUNCTION DRDRSCH(ALPH,RSCH,KW)
      USE MODGKWR

      IMPLICIT NONE
      REAL(KIND=8)::ALPH,RSCH,R,RRUECK,R0,R00,AI,
     &              GLINZ,GLANZ,GINTE,DAASCHL
      INTEGER(KIND=4) ::KW

C
C     Ableitung dR/dR-Schlange
C
C
c      R0=GLANZ(KW)
c      R00=GLINZ(KW)
      R0=0.0D0
      R00=0.0D0
      AI=GINTE(KW)
      DRDRSCH=AI*(1.0-R00)*DAASCHL(ALPH,RSCH)
c
      RETURN
      END
c
C

c
C
C
C
C
C
C
C
C
C     RVRDIN und TVRDIN können an Stelle von
C     RVRDI bzw. TVRDI  zur Berechnung der Remission bzw. Transmission
C     für die Vierfluss-Theorie verwendet werden
C
C
C
C
C
      REAL(KIND=8) FUNCTION RVRDIN(AS,SS,S1,S2,AD,SD,RU,KW)
      USE MODGKWR
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8) AS,SS,S1,S2,AD,SD,RU
      INTEGER(KIND=4) KW
      REAL(KIND=8) R0G,R0,R1,RS,RD,RG,T0G,T0S,T1G,T1S,XSI,ETA
      REAL(KIND=8) SP,SM,S4P,S4M,S4SP,S4GP,S4SM,S4GM,
     &             XF41,XF42,XF51,XF52,XF6
      REAL(KIND=8) DEL
      REAL(KIND=8) Z,H,TG,TS,NXSI,NETA,QXSI,QETA
      REAL(KIND=8) FRSRG,FRS,FRG,F00
      INTERFACE
      REAL(KIND=8) FUNCTION GLANZ(KW)
      INTEGER(KIND=4) KW
      END FUNCTION
      REAL(KIND=8) FUNCTION GLINZ(KW)
      INTEGER(KIND=4) KW
      END FUNCTION
      REAL(KIND=8) FUNCTION QEEE(X)
      REAL(KIND=8) X
      END FUNCTION
      END INTERFACE
C
C     Grenzflächenparameter(N0=N1=N2=1)
C
      R0G=GLANZ(KW)
      R0=GLINZ(KW)
      R1=GK(3,KW)
      RS=GK(4,KW)*RU
      RD=(1.D0-GK(4,KW))*RU
      RG=GK(5,KW)*RU
      AI=GINTE(KW)*(1.0-GK(12,KW))
      AJ=GINTE(KW)*GK(12,KW)
C
C
C     Hilfsgrößen
C
      T0G=AS+SS+S1+S2
      T1G=-(AS+S1+S2)
      T0S=AD+SD
      T1S=-AD
C
      XSI=-T1G*SQRT(ABS(1.0D0-SS/(T1G+EEP)))
      ETA=-T1S*SQRT(ABS(1.0D0-SD/(T1S+EEP)))
C
C
C     R-unendlich
C

C
      IF(T0G.LT.EEP) THEN
        Z=1.0
      ELSE
        Z=(T0G-XSI)/(T0G+XSI)
      ENDIF
      IF(T0S.LT.EEP) THEN
        H=1.0
      ELSE
        H=(T0S-ETA)/(T0S+ETA)
      ENDIF
C
C
      TG=T0G/(1.0+Z)**2
      TS=T0S/(1.0+H)**2
C
C
      QXSI=2.0*TG*QEEE(2.0*XSI)
      QETA=2.0*TS*QEEE(2.0*ETA)
C
C
C
C
      NXSI=1.0-R0*RS+(Z-R0)*(Z-RS)*QXSI
      NETA=1.0-R1*RG+(H-R1)*(H-RG)*QETA
C
C
C
c
      IF(XSI.GT.CTT.OR.ETA.GT.CTT) THEN
        FRSRG=0.0
        FRS=0.0
        FRG=0.0
        F00=0.0
      ELSE
        XF41=XFO4(XSI,ETA)
        XF42=XFO4(ETA,XSI)
        XF51=XFO5(XSI,ETA)
        XF52=XFO5(ETA,XSI)
        XF6=XFO6(XSI,ETA)
C
C
        SP=S1+S2
        SM=S2-S1

c
c
        S4P=SM*T1S*T1G+SP*T0G*T0S
        S4M=SM*T1S*T1G-SP*T0S*T0G
        S4SP=SM*T1S+SP*T0S
        S4GP=SM*T1G+SP*T0G
        S4SM=SM*T1S-SP*T0S
        S4GM=SM*T1G-SP*T0G
C
C
C
C
C
C
        FRSRG=(S4P*XF6-S4SM*XF51-S4GM*XF52)/8.0+XF41*S2/4.0
        FRS  =(S4M*XF6-S4SP*XF51+S4GP*XF52)/8.0+XF41*S1/4.0
        FRG  =(S4M*XF6+S4SP*XF51-S4GP*XF52)/8.0+XF42*S1/4.0
        F00  =(S4P*XF6+S4SM*XF51+S4GM*XF52)/8.0+XF42*S2/4.0
      ENDIF
C
C
C     RGE gerichtete Remission
C     RDI diffuse Remission
C
      RDF= R0G+(1.0-R0)*(1.0-R1)*(RG+(H-RG)*QETA)/NETA
      RGE=R0G+(1.0-R0)**2*(RS+(Z-RS)*QXSI)/NXSI
      RDI=(1.0-R0)*(1.0-R1)
     &   *((FRSRG*RG+FRS)*RS+FRG*RG+F00+EXP(-(XSI+ETA))*RD)/(NXSI*NETA)
      RVRDIN=AI*(RDI+RGE)+AJ*RDF
      RETURN
      END
      REAL(KIND=8) FUNCTION TVRDIN(AS,SS,S1,S2,AD,SD,RU,KW)
      USE MODGKWR
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8) AS,SS,S1,S2,AD,SD,RU
      INTEGER(KIND=4) KW
      REAL(KIND=8) R0G,R0,R1,RS,RD,RG,T0G,T0S,T1G,T1S,XSI,ETA
      REAL(KIND=8) SP,SM,S1P,S1M,S3P,S3M,XF1,XF2,XF3
      REAL(KIND=8) DEL
      REAL(KIND=8) Z,H,TG,TS,NXSI,NETA,QXSI,QETA
      REAL(KIND=8) AG10,AG13,AG20,AG23
      INTERFACE
      REAL(KIND=8) FUNCTION GLANZ(KW)
      INTEGER(KIND=4) KW
      END FUNCTION
      REAL(KIND=8) FUNCTION GLINZ(KW)
      INTEGER(KIND=4) KW
      END FUNCTION
      REAL(KIND=8) FUNCTION QEEE(X)
      REAL(KIND=8) X
      END FUNCTION
      END INTERFACE
      DATA DEL/1.0D-5/
C
C     Grenzflächenparameter(N0=N1=N2=1)
C
      R0G=GLANZ(KW)
      R0=GLINZ(KW)
      R1=GK(3,KW)
      RS=GK(4,KW)*RU
      RD=(1.D0-GK(4,KW))*RU
      RG=GK(5,KW)*RU
      AI=GINTE(KW)*(1.0-GK(12,KW))
      AJ=GINTE(KW)*GK(12,KW)

C
C
C     Hilfsgrößen
C
      T0G=AS+SS+S1+S2
      T1G=-(AS+S1+S2)
      T0S=AD+SD
      T1S=-AD
C
      XSI=SQRT(ABS(-T0G*T1G))
      ETA=SQRT(ABS(-T0S*T1S))
C
C
C     R-unendlich
C

C
      IF(T0G.LT.DEL) THEN
        Z=1.0
      ELSE
        Z=(T0G-XSI)/(T0G+XSI)
      ENDIF
      IF(T0S.LT.DEL) THEN
        H=1.0
      ELSE
        H=(T0S-ETA)/(T0S+ETA)
      ENDIF
C
C
      TG=T0G/(1.0+Z)**2
      TS=T0S/(1.0+H)**2
C
C
      QXSI=2.0*TG*QEEE(2.0*XSI)
      QETA=2.0*TS*QEEE(2.0*ETA)
C

C
      NXSI=1.0-R0*RS+(Z-R0)*(Z-RS)*QXSI
      NETA=1.0-R1*RG+(H-R1)*(H-RG)*QETA
C
C
C
      SP=S1+S2
      SM=S2-S1
      S1P=(SM*(T1S+T1G)+SP*(T0S+T0G))/4.0
      S1M=(SM*(T1G-T1S)+SP*(T0S-T0G))/4.0
      S3P=(SM*(T1S*T1G)+SP*(T0S*T0G))/2.0
      S3M=(SM*(T1S*T1G)-SP*(T0S*T0G))/2.0
C

C
C
      XF1=XFO1(XSI,ETA)
      XF2=XFO2(XSI,ETA)
      XF3=XFO3(XSI,ETA)
C
C
      AG10=S1P*XF1+S3M*XF3-S1*XF2*0.5
      AG13=S1M*XF1+S3P*XF3-S2*XF2*0.5
      AG20=S1M*XF1-S3P*XF3+S2*XF2*0.5
      AG23=S1P*XF1-S3M*XF3+S1*XF2*0.5
C
C     TGE gerichtete Transmission
C     TDI diffuse Transmission
C
C
      TGE=(1.0-RS-RD)*(1.0-R0)*EXP(-XSI)/NXSI
      TDI=(1.0-RG)*(1.0-R0)*(RD*(R1+(H-R1)*QETA)*EXP(-XSI)
     &     +R1*AG20-AG10+RS*(R1*AG23-AG13))/(NXSI*NETA)
      TVRDIN=AI*(TGE+TDI)+AJ*(1.0-RG)*(1.0-R0)*EXP(-ETA)/NETA
      RETURN
      END
      REAL(KIND=8) FUNCTION XFO1(XSI,ETA)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      DATA EPS/1.0D-3/
C
C     EXP(-XSI-ETA) wird verwendet
C
      X=XSI
      Y=ETA
      IF(XSI.LT.ETA) THEN
        X=ETA
        Y=XSI
      ENDIF
C
C
C
      IF(X.LT.2.0*EPS) THEN
         XFO1=-EXP(-X-Y)*(1.0+(X**2+Y**2)/12.0)
      ELSE IF(ABS(X-Y).LT.EPS) THEN
         XFO1=(EXP(-2*Y-X)-EXP(-Y))*(1.0+(Y-X)/2.0+(Y-X)**3/6.0)/
     &   (Y+X)
      ELSE
         XFO1= (EXP(-Y)+EXP(-2.0*X-Y)-EXP(-X)-EXP(-2.0*Y-X))/
     &   (Y**2-X**2)
      ENDIF
      END FUNCTION

      REAL(KIND=8) FUNCTION XFO2(XSI,ETA)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      DATA EPS/1.0D-3/
C
C     EXP(-XSI-ETA) wird verwendet
C
      X=XSI
      Y=ETA
      IF(XSI.LT.ETA) THEN
        X=ETA
        Y=XSI
      ENDIF
C
C
C
      IF(X.LT.2.0*EPS) THEN
         XFO2=EXP(-X-Y)*(2.0+(X**2+Y**2)/3.0)
      ELSE IF(ABS(X-Y).LT.EPS) THEN
         HLF1=1.0+X*((X-Y)/2.0+(X-Y)**2/6.0)
         HLF2=X*(1.0+(X-Y)**3/24.0)
         XFO2=(EXP(-X)*(HLF1+HLF2)-EXP(-2*Y-X)*(HLF1-HLF2))/(Y+X)
      ELSE
         XFO2=(Y*(EXP(-X)-EXP(-2.0*Y-X))-X*(EXP(-Y)-EXP(-2.0*X-Y)))
     &    /(Y**2-X**2)
      ENDIF
      END FUNCTION

      REAL(KIND=8) FUNCTION XFO3(XSI,ETA)
C
C     EXP(-XSI-ETA) wird verwendet
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      DATA EPS/1.0D-3/
      INTERFACE
      REAL(KIND=8) FUNCTION QEEE(X)
      REAL(KIND=8) X
      END FUNCTION
      END INTERFACE
      X=XSI
      Y=ETA
      IF(XSI.LT.ETA) THEN
        X=ETA
        Y=XSI
      ENDIF
C
C
C
      IF(X.LT.2.0*EPS) THEN
         XFO3=EXP(-X-Y)*(1.0/6.0+(X**2+Y**2)/120.0)
      ELSE IF(ABS(X-Y).LT.EPS) THEN
         HLF1=Y*(1.0+(Y-X)**2/6.0+(Y-X)**4/120.0)
         HLF2=1.0+Y*((Y-X)/2.0+(Y-X)**3/24.0)
         XFO3=(EXP(-X)*(HLF1-HLF2)+EXP(-2.0*Y-X)*(HLF1+HLF2))
     &    /(2.0*X*Y*(Y+X))
      ELSE
         XFO3=(QEEE(2.0*Y)*EXP(-X)-QEEE(2.0*X)*EXP(-Y))/(Y**2-X**2)
      ENDIF
      END FUNCTION
C
C
      REAL(KIND=8) FUNCTION XFO4(XSI,ETA)
C
C     EXP(-XSI-ETA) wird verwendet
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTERFACE
      REAL(KIND=8) FUNCTION QEEE(X)
      REAL(KIND=8) X
      END FUNCTION
      END INTERFACE
      IF(ABS(ETA-XSI).GT.50.0) THEN
         XFO4=2.0*(QEEE(2.0*(ETA+XSI))
     &    +(EXP(-2.0*ETA)-EXP(-2.0*XSI))/(2.0*(XSI-ETA)))
      ELSE
      XFO4=2.0*(QEEE(2.0*(ETA+XSI))+EXP(-2.0*ETA)*QEEE(2.0*(XSI-ETA)))
      ENDIF
      RETURN
      END FUNCTION
C
C
C
      REAL(KIND=8) FUNCTION XFO5(XSI,ETA)
C
C     EXP(-XSI-ETA) wird verwendet
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      DATA EPS/1.0D-3/
      INTERFACE
      REAL(KIND=8) FUNCTION QEEE(X)
      REAL(KIND=8) X
      END FUNCTION
      END INTERFACE
      IF(ABS(XSI+ETA).LT.2.0*EPS) THEN
        XFO5=2.0*(XSI+ETA-XSI*ETA-1.0)-(3.0/2.0)*XSI**2-(7.0/6.0)*ETA**2
      ELSE IF(ABS(XSI-ETA).LT.EPS) THEN
        HLF=2.0*(1.0+(XSI-ETA)*(1.0+XSI)-ETA*XSI**2)
     &      +2.0/3.0*(2.0*XSI**3+ETA**3)
        XFO5=(HLF*EXP(-2.0*XSI)
     &   -(1.0+EXP(-2*(ETA+XSI))))/(ETA*(ETA+XSI))
      Else
         XFO5=((EXP(-ETA)-EXP(-XSI))**2+(1.0-EXP(-(ETA+XSI)))**2
     &   -2*XSI*QEEE(2.0*ETA)*(1.0-EXP(-2.0*XSI)))/(XSI**2-ETA**2)

      ENDIF
      RETURN
      END FUNCTION
C
C
C
C
      REAL(KIND=8) FUNCTION XFO6(XSI,ETA)
C
C     EXP(-XSI-ETA) wird verwendet
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      DATA EPS/1.0D-3/
      INTERFACE
      REAL(KIND=8) FUNCTION QEEE(X)
      REAL(KIND=8) X
      END FUNCTION
      END INTERFACE
      IF(ABS(XSI+ETA).LT.2.0*EPS) THEN
        XFO6=4.0*((1.0+ETA*XSI-XSI-ETA)/3.0+(ETA**2+XSI*2)/5.0)
      ELSE IF(ABS(XSI-ETA).LT.EPS) THEN
        HLF=2.0*EXP(-2.0*ETA)
     &   *(1.0-(XSI-ETA)+(2.0/3.0)*(XSI-ETA)**2-(XSI-ETA)**3/3.0)
        XFO6=(((1.0-EXP(-2.0*(XSI+ETA)))/(ETA+XSI))-HLF)/(ETA*XSI)
      Else
        XFO6=2.0*(QEEE(2.0*ETA)*(1.0+EXP(-2.0*XSI))
     &           -QEEE(2.0*XSI)*(1.0+EXP(-2.0*ETA)))/(XSI**2-ETA**2)
      ENDIF
      RETURN
      END FUNCTION


