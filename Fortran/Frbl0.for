C     Last change: KU 26.01.2020 11:02:43
C******************************************************
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

      INTEGER FUNCTION NPNPS(NPX,NST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) :: CNZDEP
      IF(CNZDEP().EQ.-1.OR.NST.EQ.0) THEN
        NPNPS=NPX
      ELSE IF(MOD(CNZDEP(),2).EQ.1)  THEN
        NPNPS=NST+1
      ELSE IF(MOD(CNZDEP(),2).EQ.0) THEN
        NPNPS=2*NST
      ENDIF
      RETURN
      END
C******************************************************
C**********************************************************************
C**********************************************************************
C
C           Klaus Unterforsthuber
C           09.08.2006
C
C**********************************************************************
C**********************************************************************
C**********************************************************************
C**********************************************************************
C

      INTEGER FUNCTION NOPNPX()
      USE MODGKWR,ONLY:CDE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ANZAHL ABSORPTIONS- UND STREUKOEFFIZIENTEN PRO Farb-/Bindemittel
C
C     4-FLUSS-THEORIE(S,E) ODER KONZ.-ABH.-GRUNDDATEN(H,I,X,Y)
C     ODER VERSCHIEDENE 4-FLUSS-ANSÄTZE (S.RWRWR)
C
C
      IF(CDE(1:1).EQ.'S'.OR.CDE(1:1).EQ.'E') THEN
          NOPNPX=6
      ENDIF
      IF(CDE(1:1).EQ.'T'.OR.CDE(1:1).EQ.'D') THEN
          NOPNPX=2
      ENDIF
      IF(CDE(1:1).EQ.'V') THEN
          NOPNPX=3
      ENDIF
      IF(CDE(1:1).EQ.'X'.OR.CDE(1:1).EQ.'H') THEN
          NOPNPX=2
      ENDIF
      IF(CDE(1:1).EQ.'Y'.OR.CDE(1:1).EQ.'I') THEN
          NOPNPX=2
      ENDIF
      IF(CDE(1:1).EQ.'R'.OR.CDE(1:1).EQ.'F') THEN
          NOPNPX=4
      ENDIF
      IF(CDE(1:1).EQ.'W'.OR.CDE(1:1).EQ.'G') THEN
          NOPNPX=3
      ENDIF
      RETURN
      END

      INTEGER FUNCTION NOPNPP()
      USE MODGKWR,ONLY:CDE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) :: NOPNPX
C
C     ANZAHL ABSORPTIONS- UND STREUKOEFFIZIENTEN ZUR Berechnung der Remission bzw. Transmission
C
C
      NOPNPP=NOPNPX()
      RETURN
      END

      LOGICAL FUNCTION LLOPT()
      USE MODGKWR,ONLY:CDE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     DECKENDE SCHICHTEN
C
      LLOPT=.FALSE.
C
C     PRUEFEN, OB CDE(1:1)='O','P','Q','R','S','T','U','V','W','X','Y','Z'
C     D.H. TRANSPARENTE ODER TRANSLUZENTE SCHICHTEN
C
      II=ICHAR(CDE(1:1))
      IF(II.GE.79.AND.II.LE.90) THEN
C
C         TRANSPARENTE SCHICHTEN
C
          LLOPT=.TRUE.
      ENDIF
      RETURN
      END
C 
      INTEGER FUNCTION CNZDEP()
      USE MODGKWR,ONLY:CDE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER CCC*1
C
C     Nicht konzentrationsabhängig
C
      CNZDEP=-1
C
C     PRUEFEN, OB CDE(1:1)='X','H','Y','I'
C     D.H. KONZENTRATIONSABHÄNGIG
C
      CCC=CDE(1:1)
      IF(CCC.EQ.'X'.OR.CCC.EQ.'H') THEN
        CNZDEP=1
      ELSE IF(CCC.EQ.'Y'.OR.CCC.EQ.'I') THEN
        CNZDEP=2
      ENDIF
      RETURN
      END  
C
C
      LOGICAL FUNCTION RWOK(IKD,CMETH,CART)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      INCLUDE "PRAMR.INC"
C
C
      CHARACTER*1 DOL
      CHARACTER CART*4
      INTEGER*2 HM,IM,JM,LM 
      CHARACTER*(*) CMETH
      LOGICAL RWHR
C
C
      CHARACTER*1 CHM(28),CIM(3),CJM(6),CLM(3)
      DATA CHM/'$','@','A','B','C','D','E','F','G','H','I',
     &         'J','K','L','M','N','O','P','Q','R','S','T',
     &         'U','V','W','X','Y','Z'/
      DATA CIM/'$','T','P'/
      DATA CJM/'$','M','B','R','U','C'/

      DATA CLM/'$','W','S'/
      DATA HM/28/,IM/3/,JM/6/,LM/3/
C
      DATA DOL/'$'/
C
C
C     IKD = 1    '$' IST PLATZHALTER FUER DIE ERSTEN VIER STELLEN VON CMETH   
C     IKD = 0     KEIN PLATZHALTER
C
C
      RWOK=.TRUE.
      IF(CART(1:1).NE.CMETH(1:1)) THEN
        IF(CMETH(1:1).NE.DOL.OR.IKD.EQ.0) THEN
             RWOK=.FALSE.
        ELSE
          DO I=2,HM
           IF(CART(1:1).EQ.CHM(I)) GOTO 5 
          ENDDO
          RWOK=.FALSE.
        ENDIF 
 5    ENDIF
      IF(CART(2:2).NE.CMETH(2:2)) THEN
        IF(CMETH(2:2).NE.DOL.OR.IKD.EQ.0) THEN
             RWOK=.FALSE.
        ELSE
          DO I=2,IM
           IF(CART(2:2).EQ.CIM(I)) GOTO 10
          ENDDO
          RWOK=.FALSE.
        ENDIF 
 10   ENDIF
      IF(CART(3:3).NE.CMETH(3:3)) THEN  
        IF(CMETH(3:3).NE.DOL.OR.IKD.EQ.0) RWOK=.FALSE.
      ENDIF
      IF(CART(4:4).NE.CMETH(4:4)) THEN
        IF(CMETH(4:4).NE.DOL.OR.IKD.EQ.0) RWOK=.FALSE.
      ENDIF
      RETURN
      ENTRY RWHR(CART)
      RWHR=.TRUE.
      DO I=1,HM
         IF(CART(1:1).EQ.CHM(I)) THEN
           GOTO 102 
         ENDIF
      ENDDO
      GOTO 109
 102  DO I=1,IM
         IF(CART(2:2).EQ.CIM(I)) THEN
           GOTO 103 
         ENDIF
      ENDDO
      GOTO 109
 103  DO I=1,JM
         IF(CART(3:3).EQ.CJM(I)) THEN
           GOTO 104 
         ENDIF
      ENDDO
      GOTO 109
 104  DO I=1,LM
         IF(CART(4:4).EQ.CLM(I)) THEN
           GOTO 105 
         ENDIF
      ENDDO
      GOTO 109
  109 RWHR=.FALSE.
  105 RETURN
      END     
C******************************************************
C******************************************************
      DOUBLE PRECISION FUNCTION CHRWRT(TEXT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*8 DTEXT,TEXT
      DOUBLE PRECISION DDDD
      EQUIVALENCE (DTEXT,DDDD)
      DTEXT=TEXT
      CHRWRT=DDDD
      RETURN
      END




C******************************************************
C******************************************************
C******************************************************
C******************************************************
      SUBROUTINE CONMEN(NF,C,ASUM,MDA,NZ,A)
C
C
C     Ableitungen nach Konzentrationen (C) werden in Ableitungen nach Mengen umgerechnet
C     NF Anzahl Konzentrationen
C     C Konzentrationen
C     ASUM Gesamtmenge
C     NZ Anzahl Zeilen in Matrix A
C     A Matrix A (NZ,NF)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      REAL(KIND=8),DIMENSION(MDA,*) :: A
      REAL(KIND=8),DIMENSION(*) :: C
      IF(ABS(ASUM).LT.1.D-80) GOTO 90
      DO J=1,NZ
         SUMN=0.D0
         DO I=1,NF
           SUMN=SUMN+C(I)*A(J,I)
         ENDDO
         DO I=1,NF
           A(J,I)=(A(J,I)-SUMN)/ASUM
         ENDDO
      ENDDO
  90  RETURN
      END
      SUBROUTINE KPRUEF(KENN,*)
C
C
C     Freischaltcode wird geprüft
C
C
      INTEGER*4 KENN,KEN(4),KENH,I1,I2,I3            
      DATA KEN/7,13,17,19/                    
      DO 50 J=1,4
      KENH=KEN(J)
      IF(KENN.LE.0) GOTO 90
      I1=KENN
      I2=0
      IM=7
      IF(J.EQ.1) IM=10
      IF(J.EQ.2) IM=8 
      IF(J.EQ.3) IM=8 
      DO 10 I=1,IM
      I3=MOD(I1,KENH)
      I2=I2+I3
      I1=I1/KENH
 10   CONTINUE      
      IF(MOD(I2,KENH).NE.0) GOTO 90
 50   CONTINUE
      RETURN  
 90   RETURN 1
      END


C******************************************************
C******************************************************
C******************************************************
C******************************************************
C******************************************************
C******************************************************
      SUBROUTINE RMEAN(KA,IANZ,NWE,RS,R)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(*),RS(*),RQ(*),RO(*),RU(*),ROM(*),RUM(*)
      DATA EPS/1.D-40/
      KENN=1
      GOTO 21
      ENTRY RMEAQ(KA,IANZ,NWE,RS,RQ,R)
      KENN=2
 21   IF(IANZ.EQ.0) THEN
        DO J=1,NWE
           RS(J)=0.
           IF(KENN.EQ.2) THEN
             RQ(J)=0.
           ENDIF
        END DO
      ENDIF
      IANZ=IANZ+1
      DO  J=1,NWE
      IF(R(J).LT.0.) R(J)=0.
      GOTO(31,32,33),KA
C
C
C
C     ARITHMETISCHES MITTEL
C
  31  RRR=R(J)
      RS(J)=RS(J)+RRR
      IF(KENN.EQ.1) CYCLE
      RQ(J)=RQ(J)+RRR**2
      CYCLE
C
C
C     GEOMETRISCHES MITTEL
C
C
  32  RRR=LOG(R(J)+EPS)
      RS(J)=RS(J)+RRR                  
      IF(KENN.EQ.1) CYCLE
      RQ(J)=RQ(J)+RRR**2               
      CYCLE
C
C
C
C
C
C     HARMONISCHES MITTEL
C
C
C
C
C
  33  RRR=1./(R(J)+EPS)
      RS(J)=RS(J)+RRR            
      IF(KENN.EQ.1) CYCLE
      RQ(J)=RQ(J)+RRR**2          
      ENDDO
C
      RETURN
C
C
C
C     LETZTE MESSUNG WIRD ELIMINIERT
C
C
C
C
C
C
      ENTRY RMDIF(KA,IANZ,NWE,RS,R)
      KENN=1
      GOTO 97
      ENTRY RMRED(KA,IANZ,NWE,RS,RQ,R)
      KENN=2
  97  IANZ=IANZ-1
      DO  J=1,NWE
      GOTO(71,72,73),KA
C
C
C
C     ARITHMETISCHES MITTEL
C
  71  RRR=R(J)
      RS(J)=RS(J)-RRR
      IF(KENN.EQ.1) CYCLE
      RQ(J)=RQ(J)-RRR**2
      CYCLE
C
C
C     GEOMETRISCHES MITTEL
C
C
  72  RRR=LOG(R(J)+EPS)
      RS(J)=RS(J)-RRR                  
      IF(KENN.EQ.1) CYCLE
      RQ(J)=RQ(J)-RRR**2               
      CYCLE
C
C
C
C
C
C     HARMONISCHES MITTEL
C
C
C
C
C
  73  RRR=1./(R(J)+EPS)
      RS(J)=RS(J)-RRR            
      IF(KENN.EQ.1) CYCLE
      RQ(J)=RQ(J)-RRR**2          
      ENDDO
C
      RETURN
C
      ENTRY RMEAW(KA,IANZ,NWE,RS,R)
C
C
C     MITTELWERTE
C
C
C
      DO  J=1,NWE
      RMIT=0.
      IF(IANZ.GE.1) RMIT=RS(J)/IANZ
      GOTO(41,42,43),KA
C
C
C     ARITHMETISCHES MITTEL
C
C
  41  R(J)=RMIT         
      CYCLE
C
C
C
C     GEOMETRISCHES MITTEL
C
C
  42  R(J)=EXP(RMIT+EPS)       
      CYCLE
C
C
C
C     HARMONISCHES MITTEL
C
C
C
  43  R(J)=1./(RMIT+EPS)
      ENDDO
C
      RETURN
      ENTRY RMEAS(KA,IANZ,NWE,RS,RQ,RO,R,RU,ROM,RUM)
C
C
C     Mittelwert und STANDARDABWEICHUNG
C
C     KA     ART DER MITTELUNG
C     IANZ   ANZAHL DER MESSUNGEN
C     NWE    ANZAHL WELLENLAENGEN
C     RS     SUMME DER MESSWERTE
C     RQ     QUADRATSUMME
C     RO     OBERE GRENZE EINZELMESSUNG
C     R      MITTELWERT
C     RU     UNTERE GRENZE EINZELMESSUNG
C     ROM    OBERE GRENZE MITTELWERT
C     RUM    UNTERE GRENZE MITTELWERT
C
C
C
C
C
      FANZ=FLOAT(IANZ)
      DO J=1,NWE
      RMIT=0.
      IF(IANZ.GE.1) RMIT=RS(J)/FANZ
      IF(IANZ.LE.1) THEN     
           SMIT=0.
      ELSE
           SMIT=RQ(J)-FANZ*RMIT**2
           SMIT=ABS(SMIT/(FANZ-1.))
           SMIT=SQRT(SMIT)
      ENDIF
      FANN=1./SQRT(FANZ)
      GOTO(51,52,53),KA
C
C
C     ARITHMETISCHES MITTEL
C
C
  51  R(J)=RMIT         
      RO(J)=RMIT+SMIT
      RU(J)=RMIT-SMIT
      ROM(J)=RMIT+SMIT*FANN
      RUM(J)=RMIT-SMIT*FANN
      CYCLE
C
C
C
C     GEOMETRISCHES MITTEL
C
C
  52  R(J)=EXP(RMIT+EPS)       
      RO(J)=EXP(RMIT+SMIT+EPS)
      RU(J)=EXP(RMIT-SMIT+EPS)
      ROM(J)=EXP(RMIT+SMIT*FANN+EPS)
      RUM(J)=EXP(RMIT-SMIT*FANN+EPS)
      CYCLE
C
C
C
C     HARMONISCHES MITTEL
C
C
C
  53  R(J)=1./(RMIT+EPS)
      RO(J)=1./(RMIT-SMIT+EPS)
      RU(J)=1./(RMIT+SMIT+EPS)
      ROM(J)=1./(RMIT-SMIT*FANN+EPS)
      RUM(J)=1./(RMIT+SMIT*FANN+EPS)
      ENDDO
C
      RETURN
      ENTRY RMEAB(KA,IANZ,NWE,RS,RQ,RO,R,RU)
C
C
C     Mittelwert obere und untere Schranke
C
C     KA     ART DER MITTELUNG
C     IANZ   ANZAHL DER MESSUNGEN
C     NWE    ANZAHL WELLENLAENGEN
C     RS     SUMME DER MESSWERTE
C     RQ     QUADRATSUMME
C     RO     OBERE GRENZE EINZELMESSUNG
C     R      MITTELWERT
C     RU     UNTERE GRENZE EINZELMESSUNG
C
C
C
C
C
      FANZ=FLOAT(IANZ)
      DO  J=1,NWE
      RMIT=0.
      IF(IANZ.GE.1) RMIT=RS(J)/FANZ
      IF(IANZ.LE.1) THEN     
           SMIT=0.
      ELSE
           SMIT=RQ(J)-FANZ*RMIT**2
           SMIT=ABS(SMIT/(FANZ-1.))
           SMIT=SQRT(SMIT)
      ENDIF
      FANN=1./SQRT(FANZ)
      GOTO(61,62,63),KA
C
C
C     ARITHMETISCHES MITTEL
C
C
  61  R(J)=RMIT         
      RO(J)=RMIT+SMIT
      RU(J)=RMIT-SMIT
      CYCLE
C
C
C
C     GEOMETRISCHES MITTEL
C
C
  62  R(J)=EXP(RMIT)       
      RO(J)=EXP(RMIT+SMIT)
      RU(J)=EXP(RMIT-SMIT)
      CYCLE
C
C
C
C     HARMONISCHES MITTEL
C
C
C
  63  R(J)=1./(RMIT+EPS)
      RO(J)=1./(RMIT-SMIT+EPS)
      RU(J)=1./(RMIT+SMIT+EPS)
      ENDDO
C
      RETURN
      END

C
C
C******************************************************
C
      DOUBLE PRECISION FUNCTION FRFUN(R,KA,TPS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA EPS/1.D-30/
C
C     ARITH.
C
      IF(KA.EQ.1) THEN
            FRFUN=R
C
C
C     GEOM.
C
C
      ELSE IF(KA.EQ.2) THEN
            RHHH=R+TPS
            IF(ABS(RHHH).LT.EPS) RHHH=EPS
            FRFUN=LOG(ABS(RHHH))
C
C
C     HARM
C
C
      ELSE IF(KA.EQ.3) THEN
            RHHH=R+TPS
            IF(ABS(RHHH).LT.EPS) RHHH=EPS
            FRFUN=1./ABS(RHHH)
      ENDIF
      RETURN
      END


C******************************************************
C******************************************************
C******************************************************
C******************************************************
c
c
c
c
c
c     Allgemeiner Mittelwert
C
C
C
C
C
C******************************************************


C****************************************************************************
C****************************************************************************
C****************************************************************************
C****************************************************************************
      DOUBLE PRECISION FUNCTION RDIFF(RW,RN,RM,KA)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
C     BERECHNUNG NEUER R-WERTE NACH VERSCHIEDENEN MITTELUNGEN
C
C
C
C
C
      DATA EPS/1.D-80/
C
      GOTO (10,20,30),KA
C
C
C     ARITHMETISCHES MITTEL
C
C
C
  10  RDIFF=RW+RN-RM
      RETURN
C
C
C
C     GEOMETRISCHES MITTEL
C
C
C
C
  20  RDIFF=(RW*RN)/(RM+EPS)
      RETURN
C
C
C
C
C     HARMONISCHES MITTEL
C
C
C
C
  30  QW=1./(RW+EPS)
      QN=1./(RN+EPS)
      QM=1./(RM+EPS)
      RDIFF=1./(QW+QN-QM)
      RETURN
      END
C******************************************************
C******************************************************
C******************************************************
C******************************************************
      SUBROUTINE DBLSNG(N,RD,RS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     DOUBLE PRECISION IN SINGLE UMSPEICHERN
C
      REAL*4 RS(*)
      DIMENSION RD(*)
      DO I=1,N
         RS(I)=RD(I)
      ENDDO
      RETURN
      END
      SUBROUTINE SNGDBL(N,RD,RS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     SINGLE IN DOUBLE PRECISION UMSPEICHERN
C
      REAL*4 RS(*)
      DIMENSION RD(*)
      DO I=1,N
         RD(I)=RS(I)
      ENDDO
      RETURN
      END
C******************************************************
      SUBROUTINE UPCASE(TEXT)
      CHARACTER*(*) TEXT
      ILEN=LEN(TEXT)
      DO I=1,ILEN
        J=ICHAR(TEXT(I:I))
        IF(J.GT.96.AND.J.LT.123) THEN
           TEXT(I:I)=CHAR(J-32)
        ENDIF
      ENDDO
      RETURN
      END
C
C******************************************************************************
C******************************************************
C 
      FUNCTION IBIN(NF,ICHF)  
      DIMENSION ICHF(*)
C     ERSTES BINDEMITTEL
      ICREF=1
      IBIN=IBCC(NF,ICHF,ICREF) 
      RETURN
      END
      FUNCTION IBWN(NF,ICHF)  
      DIMENSION ICHF(*)
C     ERSTES Weisspigment
      ICREF=2
      IBWN=IBCC(NF,ICHF,ICREF) 
      RETURN
      END
      FUNCTION IBBN(NF,ICHF)  
      DIMENSION ICHF(*)
C     ERSTES Schwarzpigment
      ICREF=3
      IBBN=IBCC(NF,ICHF,ICREF) 
      RETURN
      END
      FUNCTION IBMN(NF,ICHF)
      DIMENSION ICHF(*)
C     ERSTES Metalleffektpigment
      ICREF=4
      IBMN=IBCC(NF,ICHF,ICREF)
      RETURN
      END
      FUNCTION IBCC(NF,ICHF,ICREF)
      DIMENSION ICHF(*)
      IBI=0
      DO 10 L=1,NF
      IF(IBI.EQ.0.AND.ICHF(L).EQ.ICREF) IBI=L
  10  CONTINUE 
      IBCC=IBI
      RETURN
      END
      FUNCTION IBAN(LK,NF,IBI)
      DIMENSION LK(*)
      IBAN=0
      DO I=1,NF
       IF(LK(I).EQ.IBI) THEN
        IBAN=I
        EXIT
       ENDIF
      ENDDO
      RETURN
      END
C******************************************************************************
C******************************************************************************
C******************************************************
      SUBROUTINE RMITAL(KA,KM,NWE,KME,KMN,R,RS,RQ,RM,RSN,RQN,IER)
      USE MODRMEM,ONLY:IPM
      
C      INCLUDE "PRAMR.INC"
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION KME(*),KMN(*),R(*),RS(*),RQ(*),RSN(*),RQN(*),
     &          RM(NWE*KM,*)
C
      IER=0
        DO KW=1,KM
          NAD=(KW-1)*NWE
          IF(KME(KW).EQ.IPM) THEN
             CALL RMRED(KA,KME(KW),NWE,RS(NAD+1),RQ(NAD+1),
     &                                 RM(NAD+1,KME(KW)))
          ENDIF
          CALL RMEAQ(KA,KME(KW),NWE,RS(NAD+1),RQ(NAD+1),R(NAD+1))
          
          KMN(KW)=KME(KW)
          DO I=1,NWE
             RM(NAD+I,KME(KW))=R(NAD+I)
             RSN(NAD+I)=RS(NAD+I)
             RQN(NAD+I)=RQ(NAD+I)
          ENDDO
        ENDDO
C
      RETURN
      END

C
C
      SUBROUTINE RRRMIT(KA,KM,NWE,KMN,RSN,RQN,RO,R,RU,IER)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION KMN(*),RSN(*),RQN(*),RO(*),R(*),RU(*)
C
      IER=0
C 
C
C     MITTELWERT BERECHNEN 
C
      DO KW=1,KM
         NAD=(KW-1)*NWE
C
C        MITTELWERT OBERE UNTERE GRENZE
C
C
         CALL RMEAB(KA,KMN(KW),NWE,RSN(NAD+1),RQN(NAD+1),
     &                             RO(NAD+1),R(NAD+1),RU(NAD+1))
C
      ENDDO
C
      RETURN
      END
C
C
C
C
      SUBROUTINE RMZERO(AKA,NWEW,KMW,Q,IER)
      USE MODRMEM
      USE MODWINK,ONLY:KM
      USE MODILLU,ONLY:NWE
      USE MODGKWR,ONLY:KA
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 

C
      INTEGER*4 NWEW,KMW
      REAL(KIND=4) :: AKA

C
      REAL*4 Q(*)

C
C
C
C
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
C     Übernahme der COMMON-WERTE
C
C
C

      KA=INT(2.5-AKA)
      IF(KA.LE.0) THEN
         KA=1
      ENDIF
      NWE=NWEW
      KM=KMW
      KND=KM*NWE
C
C     Felder von MODRMEM allocieren
C
      IF(ALLOCATED(KME)) THEN
         DEALLOCATE(KME,KMN,DEW,
     &              RS,RQ,RSN,RQN,R,RO,RU,
     &              RH,RM)
      ENDIF
      ALLOCATE(KME(KM),KMN(KM),DEW(KM),
     &         RS(KND),RQ(KND),RSN(KND),RQN(KND),R(KND),RO(KND),RU(KND),
     &         RH(KND,5),RM(KND,IPM),STAT=IER)



C
C
C
C
C     AUSNULLEN VON RS UND RQ 
C

C     
C       CALL SNGDBL(KND,R,Q)
        DO I=1,KM
           KME(I)=0
           KMN(I)=0
        ENDDO
        DO I=1,KND
              RS(I)=0.D0
              RQ(I)=0.D0
              Q(I)=0.
        ENDDO
C
C
C
  900 RETURN
      END
C

C
C
C
C
C
C
C      
      DOUBLE PRECISION FUNCTION GRUBB(N,DECH,ALPP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      REAL*4 G(25)
C
C     SCHWELLENWERTE FUER AUSREISSERZTEST NACH GRUBBS (s.PETERSEN S233)
      DATA G/0.000,0.000,1.155,1.492,1.749,
     &       1.944,2.097,2.221,2.323,2.410,
     &       2.485,2.550,2.607,2.659,2.705,
     &       2.747,2.785,2.821,2.854,2.884,
     &       2.912,2.939,2.963,2.987,3.009/
      IF(N.LE.25) THEN
         GRUBB=G(N)*DECH
      ELSE
         GRUBB=G(25)*DECH
      ENDIF
      RETURN
      END
      SUBROUTINE GETNRM(NWW,ID,FAKT,XYZE,IER)
      USE MODILLU
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=4),DIMENSION(NWW,*)::XYZE
      REAL(KIND=4),DIMENSION(*)::FAKT
      INTEGER*4 ID,NWW

C
C
C      UMSPEICHERN DER NORMSPEKTRALWERTE
C
C
C
C      CONTE
C
       IER=0
       NLZ=1
       NWE=NWW
C
       IF(NWE.LE.0) THEN
         IER=4046
         RETURN
       ENDIF
C
      IF(ALLOCATED(NLA)) THEN
        DEALLOCATE(NLA,FA,WSOL,FTKT,EILLU)
      ENDIF
C
C
      ALLOCATE(NLA(NLZ),FA(NLZ),WSOL(NWE),FTKT(3,NLZ),
     &            EILLU(NWE,3,NLZ),STAT=IER)
      NLA(1)= ID
      FA(1)=1.
      IF(NLA(1).LE.0) THEN
         IER=4026
         RETURN
      ENDIF
C

C
C      MODILLU erstellen
C
C           
          DO J=1,3
             FTKT(J,1)=FAKT(J)
             IF(FTKT(J,1).LE.0.D0) THEN
                IER=4027
                RETURN
             ENDIF
             DO I=1,NWE
                EILLU(I,J,1)=XYZE(I,J)
             ENDDO
          ENDDO
      RETURN
      END
      SUBROUTINE MODGET(AK,NGK,GKW,NWEW,KMW,IER)
      USE MODWINK,ONLY:KM
      USE MODILLU,ONLY:NWE
      USE MODGKWR,ONLY:AKA,GK
C
C
C     GK(1)  GLANZ
C     GK(2)  INNERE GERICHTETE REFLEXION 
C     GK(3)  INNERE DIFFUSE REFLEXION
C     GK(4)  ANTEIL GERICHTET FUER UNTERGRUND (WIRD HIER NICHT VERWENDET)
C     GK(5)  ANTEIL DIFFUS-DIFFUS FUER UNTERGRUND (WIRD HIER NICHT VERWENDET)
C     GK(6)  ANTEIL VORWAERTSSTREUUNG (0.5==> ISOTROP   0.==> KUB.-MUNK)
C     GK(7)  ANTEIL RUECKWAERTSSTREUUNG (0.5==> ISOTROP 1.==> KUBELKA MUNK)
C     GK(8)  SCHMELZER PARAMETER 1  = 0.1355
C     GK(9)  SCHMELZER PARAMETER 2  = 2.2710
C     GK(10) GESAMTINTENSITAET (s. auch GINTE)
C     GK(11) POTENZ FUER TAU
C     GK(12) Anteil diffus für 4-Kanal-Theorie
C     GK(13) POTENZ FUER SQRT(1.0-OMEGA)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KMW,NWEW
      REAL(KIND=4) :: AK
      REAL(KIND=4),DIMENSION(NGK,*)::GKW
C
C     Übernahme in Module
C
      IER=0
      KM=KMW
      IF(KM.LE.0) THEN
         IER=4006
         RETURN
      ENDIF
      NWE=NWEW
      IF(NWE.LE.0) THEN
         IER=4005
         RETURN
      ENDIF
C
      IF(ALLOCATED(GK)) THEN
        DEALLOCATE(GK)
      ENDIF  
      ALLOCATE(GK(NGK,0:KMW))
C
C
C     Für KW=0 werden die Werte von KW=1 genommen
      DO I=1,NGK
        GK(I,0)=GKW(I,1)
      ENDDO
      DO KW=1,KMW
        DO I=1,NGK
         GK(I,KW)=GKW(I,KW)
        ENDDO
        IF(GK(10,KW).EQ.0.D0) THEN
          IER=4023
          RETURN
        ENDIF
        IF((GK(6,KW)+GK(7,KW)).LE.0.D0) THEN
           IER=4024
           RETURN
        ENDIF
      END DO
C
C
      AKA=AK
      RETURN
      END SUBROUTINE
         
      SUBROUTINE NULLRWRT (NWE,KM,NUU,RUNT)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8),DIMENSION(NWE,KM,*) :: RUNT
      DO K=1,NUU
        DO KW=1,KM
          DO I=1,NWE
            RUNT(I,KW,K)=0.
          END DO
        END DO
      ENDDO
      RETURN
      END SUBROUTINE
C
C
C
C
C
C
C
      REAL(KIND=8) FUNCTION ASCHL(ALPH,A)
      IMPLICIT NONE
      REAL(KIND=8) :: ALPH,HLFR,A,SML,TOL,ARUECK,DAASCHL,AS,ANN
      DATA TOL/1.0D-12/,SML/1.0D-2/
C
C
      HLFR=A
      IF(HLFR.LT.TOL) HLFR=TOL
      IF(ABS(ALPH).LT.SML) THEN
        HLFR=LOG(HLFR)
        ASCHL=-HLFR*(1.+0.5*ALPH*HLFR)
      ELSE
        ASCHL=(1.0-HLFR**ALPH)/ALPH
      ENDIF
      RETURN
C
C     Rueckrechnung
C
C
      ENTRY ARUECK(ALPH,AS)
      HLFR=AS
      IF(ABS(ALPH).LT.SML) THEN
        ARUECK=EXP(-HLFR*(1.+0.5*ALPH*HLFR))
      ELSE
        ANN=1.0/ALPH
        IF((1.0-ALPH*HLFR).LE.TOL) THEN
            HLFR=(1.0-TOL)*ANN
        ENDIF
        ARUECK=(1.-ALPH*HLFR)**ANN
      ENDIF
      RETURN
C
C
C     Ableitung dA/dAS=dARUECK/dASCHL
C
C
      ENTRY DAASCHL(ALPH,AS)
      HLFR=AS
      IF(ABS(ALPH).LT.SML) THEN
        DAASCHL=-EXP(-HLFR*(1.+0.5*ALPH*HLFR))*(1.0+ALPH*HLFR)
      ELSE
        ANN=1.0/ALPH
        IF((1.0-ALPH*HLFR).LE.TOL) THEN
            HLFR=(1.0-TOL)*ANN
        ENDIF
        DAASCHL=-(1.0-ALPH*HLFR)**(ANN-1.0)
      ENDIF
      RETURN
      END

