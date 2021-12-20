C     Last change: KU 14.11.2021 15:33:57










      SUBROUTINE REZFIT(KFIT,KF,ITT,MAXFUN,FTT,
     &                 X,FVALUE,IER)
      USE MODGREN
      USE MODFARB
      USE MODMERZ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4)::KFIT,KF,ITT
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: XLB
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: XUB
      EXTERNAL MTRWRT,FTRWRT,FUNREZ,MATREZ,MTMIXT,FTMIXT
      DATA AMAXI/1.D80/
      DIMENSION X(*)
      IER=0
      IF(KF.LE.1) THEN
        IER=4177
        RETURN
      ENDIF
      ALLOCATE(XLB(KF),XUB(KF))
      DO I=1,KF
         XLB(I)=CL(LK(I))
         XUB(I)=CU(LK(I))
         IF(IC(LK(I)).EQ.1) THEN
            XUB(I)=AMAXI
         ENDIF
         IF(IC(LK(I)).EQ.2) THEN
            XLB(I)=0.
         ENDIF
      END DO
      CALL NUMALL(KFIT,KF,MAL,MGAL,MEAL)
      IF(KFIT.EQ.1) THEN
C
C
C        FARBWERTE-ANGLEICH (SCHITTKOWSKI)
C
C
         CALL NLDFIT(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FUNREZ,MATREZ)
         IF(IFAIL.GT.4000) THEN
            IER=IFAIL
            GOTO 900
         ENDIF
         IF(IFAIL.EQ.0.OR.IFAIL.EQ.1.OR.
     &      IFAIL.EQ.2.OR.IFAIL.EQ.7.OR.IFAIL.EQ.4) THEN
           IER=0
         ELSE
           IER=-4138
         ENDIF
      ELSE IF(KFIT.EQ.2) THEN
C
C
C        REFLEXIONSWERTE-ANGLEICH (SCHITTKOWSKI )
C
         CALL NLDFIT(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FTRWRT,MTRWRT)
         IF(IFAIL.GT.4000) THEN
            IER=IFAIL
            GOTO 900
         ENDIF
         IF(IFAIL.EQ.0.OR.IFAIL.EQ.1.OR.
     &      IFAIL.EQ.2.OR.IFAIL.EQ.7.OR.IFAIL.EQ.4) THEN
            IER=0
         ELSE
           IER=-4138
         ENDIF
      ELSE IF(KFIT.EQ.3) THEN
C
C
C        Farbwert + REFLEXIONSWERTE-ANGLEICH (SCHITTKOWSKI )
C
         CALL NLDFIT(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FTMIXT,MTMIXT)
         IF(IFAIL.GT.4000) THEN
            IER=IFAIL
            GOTO 900
         ENDIF
         IF(IFAIL.EQ.0.OR.IFAIL.EQ.1.OR.
     &      IFAIL.EQ.2.OR.IFAIL.EQ.7.OR.IFAIL.EQ.4) THEN
           IER=0
         ELSE
           IER=-4138
         ENDIF
C
C
C
C
C
      ELSE IF(KFIT.EQ.4) THEN
C
C
C        FARBWERTE-ANGLEICH (LEVENBERG-MARQUARD)
C
C
         CALL NLDFIU(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FUNREZ,MATREZ)
         IF(IFAIL.GT.4000) THEN
            IER=IFAIL
            GOTO 900
         ENDIF
         IF(ABS(IFAIL).GT.4000) THEN
           IER=IFAIL
         ENDIF
      ELSE IF(KFIT.EQ.5) THEN
C
C
C        REFLEXIONSWERTE-ANGLEICH (LEVENBERG-MARQUARD)
C
         CALL NLDFIU(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FTRWRT,MTRWRT)
         IF(IFAIL.GT.4000) THEN
            IER=IFAIL
            GOTO 900
         ENDIF
         IF(ABS(IFAIL).GT.4000) THEN
           IER=IFAIL
         ENDIF
      ELSE IF(KFIT.EQ.6) THEN
C
C
C        Farbwert + REFLEXIONSWERTE-ANGLEICH (LEVENBERG-MARQUARD)
C
         CALL NLDFIU(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FTMIXT,MTMIXT)
         IF(IFAIL.GT.4000) THEN
            IER=IFAIL
            GOTO 900
         ENDIF
C
         IF(IFAIL.EQ.0.OR.IFAIL.EQ.1.OR.
     &      IFAIL.EQ.2.OR.IFAIL.EQ.7.OR.IFAIL.EQ.4) THEN
           IER=0
         ELSE
           IER=-4138
         ENDIF
C
      ELSE
         IER=4104
      ENDIF
C
 900  DEALLOCATE(XLB,XUB)
      RETURN
      END
C
C
C
C
C
      SUBROUTINE NUMALL(KFIT,KF,MAL,MGAL,MEAL)
      USE MODGREN
      USE MODFARB
      USE MODMERZ

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     BERECHNUNG DER ANZAHL GLEICHUNGEN/UNGLEICHUNGEN FUER FUNREZ,MATREZ (IPRN=1)
C                                                     BZW. FTRWRT,MTRWRT (IPRN=2)
C
C     MAL  ANZAHL MINIMIERUNGSBEDINGUNGEN
C     MGAL ANZAHL GLEICHUNGEN UND UNGLEICHUNGEN
C     MEAL ANZAHL GLEICHUNGEN
      KM=KMS()
      NWE=NWS()
      MAL=0
      JUR=JUV
      IF(JUR.EQ.0) THEN
         JUR=JUU
      END IF
      IF(KFIT.EQ.1.OR.KFIT.EQ.4) THEN
C
C        FARBANGLEICH
C
         NZU=0
         IF(.NOT.(KGX.EQ.0.OR.JO.LT.2)) THEN
           NZU=NZU+1
         ENDIF
         MAL=3*JN*JUR*KM+NZU+NEM
         MGAL=NUNEQU(NEE,KF)
         MEAL=NUNEQU(NEQ,KF)
      ELSEIF(KFIT.EQ.2.OR.KFIT.EQ.5) THEN
C
C        ANGLEICH REMISSIONSWERTE
C
         NZU=0
         IF(.NOT.(KGX.EQ.0.OR.JO.LT.2)) THEN
            IF(KWO.LE.0.OR.KWO.GT.KM) THEN
              NZU=NWE*KM
            ELSE
              NZU=NWE
            ENDIF
         ENDIF
C
C
         MAL=JUR*KM*NWE+NZU+NEM
         MGAL=NUNEQU(NEE,KF)
         MEAL=NUNEQU(NEQ,KF)
      ELSEIF(KFIT.EQ.3.OR.KFIT.EQ.6) THEN
C
C        FARBANGLEICH
C
         NZU=0
         IF(.NOT.(KGX.EQ.0.OR.JO.LT.2)) THEN
           NZU=NZU+1
         ENDIF
         MAL=3*JN*JUR*KM+NZU+NEM
         MGAL=NUNEQU(NEE,KF)
         MEAL=NUNEQU(NEQ,KF)
C
C        und Angleich Remissionswerte
C
         MAL=MAL+JUR*KM*NWE
      ENDIF
      RETURN
      END SUBROUTINE
C
C

C
      SUBROUTINE MATREZ(NGL,X,XL,XU,MDA,NZZ,MCON,F,A,IER)
      USE MODFARB
      USE MODMERZ

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*),XL(*),XU(*),A(MDA,*),F(*),C(NGL)
C
C      DATA RANGE/1.D80/
C
C     Farbwerteangleich mit Nebenbedingungen
C
C     MINIMIERUNGSBEDINGUNGEN
C
      CSUM=EPL
      DO I=1,NGL
         CSUM=CSUM+X(I)
      ENDDO
      IF(CSUM.LE.2.*EPL) THEN
         CSUM=1.
      ENDIF
      DO I=1,NGL
         C(I)=X(I)/CSUM
      ENDDO


C
C
      CALL MATRA(IER,C,NGL,MDA,NZ,F(1),A(1,1))
C

C     Umrechnung der Ableitungen nach Konzentrationen in solche nach Mengen
C
C

      CALL CONMEN(NGL,C,CSUM,MDA,NZ,A(1,1))
C
C
      NO=NZ
C
C
C     Zusätzliche MINIMIERUNGSBEDINGUNGEN
C
C

      CALL MINMAT(NGL,X,LK,MDA,NA,A(NO+1,1),IGX,
     &            GXG,GGE)
      NO=NO+NA
      IF(NO.NE.NZZ) THEN
         IER=91
         RETURN
      ENDIF

C
C
C
C     GRENZEN
C
C
      CALL MUNEQU(IER,X,NGL,MDA,MCCC,A(NO+1,1))
      IF(MCON.NE.MCCC) THEN
         IER=92
         RETURN
      ENDIF
      RETURN
      END
C 
C******************************************************
C******************************************************
C******************************************************************************
      SUBROUTINE FUNREZ(N,X,XL,XU,NZZ,MCON,F,IER)
      USE MODFARB
      USE MODMERZ

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*),C(N)
      IER=0
C
C
C
C
C     Farbwerteangleich mit Nebenbedingungen
C


      CSUM=EPL
      DO I=1,N
         CSUM=CSUM+X(I)
      ENDDO
      IF(CSUM.LE.2.*EPL) THEN
         CSUM=1.
      ENDIF
      DO I=1,N
         C(I)=X(I)/CSUM
      ENDDO
      CALL FFUNK(IER,C,N,F(1),NO)
C
C
C
C      
C     Zusätzliche Minimierungsbedingungen für die Mengen
C
C
      CALL MINFUN(N,X,LK,NA,F(NO+1),IGX,GXG,GGE)
      NO=NO+NA
      IF(NO.NE.NZZ) THEN
         IER=91
         RETURN
      ENDIF
C
C
C     Grenzen
C
C

      CALL FUNEQU(IER,X,N,MCCC,F(NO+1))
      IF(MCON.NE.MCCC) THEN
         IER=92
         RETURN
      ENDIF
C
      RETURN
      END 
C
C******************************************************
C******************************************************
C 
C
      SUBROUTINE MTMIXT(NGL,X,XL,XU,MDA,NZZ,MCON,F,A,IER)
      USE MODFARB
      USE MODMERZ

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*),XL(*),XU(*),A(MDA,*),F(*),C(NGL)
C
C      DATA RANGE/1.D80/
C
C     Farbwerteangleich + R-Werte Angleich
C
C
C     MINIMIERUNGSBEDINGUNGEN
C
      CSUM=EPL
      DO I=1,NGL
         CSUM=CSUM+X(I)
      ENDDO
      IF(CSUM.LE.2.*EPL) THEN
         CSUM=1.
      ENDIF
      DO I=1,NGL
         C(I)=X(I)/CSUM
      ENDDO


C
C
      CALL MATRA(IER,C,NGL,MDA,NZ,F(1),A(1,1))
C
C
C     Angleich R-Werte
C
C
      CALL MARGEW(IER,C,NGL,MDA,NA,F(NZ+1),A(NZ+1,1))
C     Umrechnung der Ableitungen nach Konzentrationen in solche nach Mengen
C
C
      NA=NZ+NA
      CALL CONMEN(NGL,C,CSUM,MDA,NA,A(1,1))
C
C
      NO=NA
C
C
C     Zusätzliche MINIMIERUNGSBEDINGUNGEN
C
C

      CALL MINMAT(NGL,X,LK,MDA,NA,A(NO+1,1),IGX,
     &            GXG,GGE)
      NO=NO+NA
      IF(NO.NE.NZZ) THEN
         IER=91
         RETURN
      ENDIF

C
C
C
C     GRENZEN
C
C
      CALL MUNEQU(IER,X,NGL,MDA,MCCC,A(NO+1,1))
      IF(MCON.NE.MCCC) THEN
         IER=92
         RETURN
      ENDIF
      RETURN
      END
C 
C******************************************************
C******************************************************
C******************************************************************************
      SUBROUTINE FTMIXT(N,X,XL,XU,NZZ,MCON,F,IER)
      USE MODFARB
      USE MODMERZ

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c      INCLUDE "PRAMR.INC"
c      INCLUDE "COFAR.INC"
c      INCLUDE "COMEN.INC"
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*),C(N)
      IER=0
C
C
C
C
C     Farbwerteangleich + R-Werte Angleich
C


      CSUM=EPL
      DO I=1,N
         CSUM=CSUM+X(I)
      ENDDO
      IF(CSUM.LE.2.*EPL) THEN
         CSUM=1.
      ENDIF
      DO I=1,N
         C(I)=X(I)/CSUM
      ENDDO
      CALL FFUNK(IER,C,N,F(1),NO)
C
C
C     R-Angleich
C
C
      CALL FFRGEW(IER,C,N,F(NO+1),NA)
C
C      
C     Zusätzliche Minimierungsbedingungen für die Mengen
C
C
      NO=NO+NA
      CALL MINFUN(N,X,LK,NA,F(NO+1),IGX,GXG,GGE)
      NO=NO+NA
      IF(NO.NE.NZZ) THEN
         IER=91
         RETURN
      ENDIF
C
C
C     Grenzen
C
C

      CALL FUNEQU(IER,X,N,MCCC,F(NO+1))
      IF(MCON.NE.MCCC) THEN
         IER=92
         RETURN
      ENDIF
C
      RETURN
      END 
C
C******************************************************
C******************************************************
C******************************************************************************
      SUBROUTINE MTRWRT(NGL,X,XL,XU,MDA,NZZ,MCON,F,A,IER)
      USE MODFARB
      USE MODMERZ

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c      INCLUDE "PRAMR.INC"
c      INCLUDE "COMEN.INC"
c      INCLUDE "COFAR.INC"
      DIMENSION X(*),XL(*),XU(*),A(MDA,*),F(*),C(NGL)
C
c      DATA RANGE/1.D80/
C
C     R-Werte Angleich + Nebenbedingungen
C
C     MINIMIERUNGSBEDINGUNGEN
C
      CSUM=EPL
      DO I=1,NGL
         CSUM=CSUM+X(I)
      ENDDO
      IF(CSUM.LE.2.*EPL) THEN
         CSUM=1.
      ENDIF
      DO I=1,NGL
         C(I)=X(I)/CSUM
      ENDDO
C
C
      CALL MARWRT(IER,C,NGL,MDA,NZ,F(1),A(1,1))
C
C     Umrechnung der Ableitungen nach Konzentratinen in solche nach Mengen
C
C
      CALL CONMEN(NGL,C,CSUM,MDA,NZ,A(1,1))
C
C
      NO=NZ
      CALL MINMAT(NGL,X,LK,MDA,NA,A(NO+1,1),IGX,
     &            GXG,GGE)
      NO=NO+NA
      IF(NO.NE.NZZ) THEN
         IER=91
         RETURN
      ENDIF

C
C
C
C     GRENZEN
C
C
      CALL MUNEQU(IER,X,NGL,MDA,MCCC,A(NO+1,1))
      IF(MCON.NE.MCCC) THEN
         IER=92
         RETURN
      ENDIF

      RETURN
      END
C 
C******************************************************
C******************************************************
C
C
C 
C
C 
C******************************************************************************
      SUBROUTINE FTRWRT(N,X,XL,XU,NZZ,MCON,F,IER)
      USE MODFARB
      USE MODMERZ

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c      INCLUDE "PRAMR.INC"
c      INCLUDE "COFAR.INC"
c      INCLUDE "COMEN.INC"
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*),C(N)
      IER=0
C
C
C     R-Werte Angleich + Nebenbedingungen
C

C
C
      CSUM=EPL
      DO I=1,N
         CSUM=CSUM+X(I)
      ENDDO
      IF(CSUM.LE.2.*EPL) THEN
         CSUM=1.
      ENDIF
      DO I=1,N
         C(I)=X(I)/CSUM
      ENDDO
      CALL FFRWRT(IER,C,N,F(1),NO)
C
C      
C     MENGENGLEICHUNGEN
C
C
      CALL MINFUN(N,X,LK,NA,F(NO+1),IGX,GXG,GGE)
      NO=NO+NA
      IF(NO.NE.NZZ) THEN
         IER=91
         RETURN
      ENDIF
C
      CALL FUNEQU(IER,X,N,MCCC,F(NO+1))
      IF(MCON.NE.MCCC) THEN
         IER=92
         RETURN
      ENDIF
C
C
      RETURN
      END 
C

C
C                           +-------+ 
C                           !MARWRT  !
C                           +-------+ 
      SUBROUTINE MARWRT(IER,C,NN,MD,NZ,F,AM)
      USE MODGREN
      USE MODFARB
      USE MODMERZ
      USE MODRWRZ
      USE MODXYRZ
      USE MODWINK,ONLY:FWI
      USE MODILLU,ONLY:FA


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C
C     C     FARB/BINDEMITTELKONZENTRATIONEN (SUMME = 1.)
C     NN    ANZAHL FARB/BINDEMITTEL
C     MD    DIMENSION VON AM IM RUFENDEN PROGRAMM
C     NZ    ANZAHL MINIMIERUNGSBEDINGUNGEN
C     AM    MATRIX MIT MINIMIERUNGSBEDINGUNGEN
C 
C
      SAVE JV
C
      DIMENSION AM(MD,*),C(*),F(*)
C 
C
C
C
C     R-Werte Angleich +
C
C
C     BERECHNUNG DER MENGEN AUS LINEAREM SYSTEM
C 
C 
      KM=KMS()
      NWE=NWS()
      SUF=1.0E-30
      DO KW=1,KM
        SUF=SUF+FWI(KW)
      END DO
      SUF=SUF*NWE

      IER=0
C
C 
C
C
C 
C
  5   JV=JUV

      IF(KGX.EQ.1) THEN
             IF(JV.LT.JO) JV=JUU
      ENDIF
C
C 
      DO K=1,JV
C
C
C     BERECHNUNG DER REFLEXIONSWERTE UND DER ABLEITUNGEN NACH
C     ABSORPTION UND STREUUNG
C
C
C 
C 
C 
         DO KW=1,KM
            CALL MALAB(NN,C,RR(1,KW,K),KW,K)
C

C           BEI ZWEI MESSUNGEN DES UNTERGRUNDES KOENNEN VERBESSERTE
C           REFLEXIONSWERTE BERECHNET WERDEN
C
C           
C
C           
            IF(JUN.EQ.2) THEN
               CALL MAKOR(NN,C,RR(1,KW,K),KW,K)
            ENDIF
C
C
         ENDDO
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
C
C 
C     RR           REFLEXIONSWERTE
C 
C 
C 
C 
C 
C 
C 
C 
  100 NZU=0
C 
C 
      IF(KGX.EQ.0.OR.JO.LT.2) GOTO 150
C
      NA=KM*NWE*JUV
      IF(KWO.LE.0.OR.KWO.GT.KM) THEN
          NZU=NWE*KM
          DO L=1,NN
           DO I=1,NWE*KM
              AM(NA+I,L)=0.0
           ENDDO
          ENDDO
      ELSE
          NZU=NWE
          DO L=1,NN
           DO I=1,NWE
              AM(NA+I,L)=0.
           ENDDO
          ENDDO
      ENDIF
C
C
C
C
C     FÜR REFLEXION
C
C
C
C
      IF(IRETR(1).EQ.IRETR(2).AND.IRETR(1).EQ.0) THEN
        DO K=1,JO,JO-1
         IF(KWO.LE.0.OR.KWO.GT.KM) THEN
          DO KW=1,KM
            NAD=(KW-1)*NWE
            DO L=1,NN
              CALL MALDC(NN,C,RD,L,KW,K)
C
C             BEI ZWEI MESSUNGEN DER NACHSTELLUNG (UEBER WEISS UND SCHWARZ)
C             KOENNEN VERBESSERTE REFLEXIONSWERTE (RR) BERECHNET WERDEN
C
C
C           
              IF(JUN.EQ.2) THEN
                 CALL MALKO(RR(1,KW,K),RD,L,KW,K)
              ENDIF
     	      DO I=1,NWE
                IF(K.EQ.1) THEN
	          AM(NA+NAD+I,L)=FWI(KW)*(AM(NA+NAD+I,L)+RD(I))
                ELSE
	          AM(NA+NAD+I,L)=FWI(KW)*(AM(NA+NAD+I,L)-RD(I))
               ENDIF
              END DO
C
             ENDDO
           ENDDO
         ELSE
          DO L=1,NN
              CALL MALDC(NN,C,RD,L,KWO,K)
C
C             BEI ZWEI MESSUNGEN DER NACHSTELLUNG (UEBER WEISS UND SCHWARZ)
C             KOENNEN VERBESSERTE REFLEXIONSWERTE (RR) BERECHNET WERDEN
C
              IF(JUN.EQ.2) THEN
                 CALL MALKO(RR(1,KWO,K),RD,L,KWO,K)
              ENDIF
     	      DO I=1,NWE
                IF(K.EQ.1) THEN
	          AM(NA+I,L)=AM(NA+I,L)+RD(I)
                ELSE
	          AM(NA+I,L)=AM(NA+I,L)-RD(I)
               ENDIF
              END DO

            ENDDO
         ENDIF
        ENDDO
C
C
C     FÜR TRANSMISSION
C
C
C
      ELSEIF (IRETR(1).EQ.1.OR.IRETR(2).EQ.1) THEN
        IF(IRETR(1).EQ.1) THEN
          K=1
        ELSEIF(IRETR(2).EQ.1) THEN
          K=2
        ENDIF
         IF(KWO.LE.0.OR.KWO.GT.KM) THEN
          DO KW=1,KM
            NAD=(KW-1)*NWE
            DO L=1,NN
              CALL MALDC(NN,C,RD,L,KW,K)
C
C             BEI ZWEI MESSUNGEN DER NACHSTELLUNG (UEBER WEISS UND SCHWARZ)
C             KOENNEN VERBESSERTE REFLEXIONSWERTE (RR) BERECHNET WERDEN
C
              IF(JUN.EQ.2) THEN
                CALL MALKO(RR(1,KW,K),RD,L,KW,K)
              ENDIF
     	      DO I=1,NWE
	          AM(NA+NAD+I,L)=FWI(KW)*(AM(NA+NAD+I,L)+RD(I))
              END DO
C
             ENDDO
           ENDDO
         ELSE
           DO L=1,NN
              CALL MALDC(NN,C,RD,L,KWO,K)
C
C             BEI ZWEI MESSUNGEN DER NACHSTELLUNG (UEBER WEISS UND SCHWARZ)
C             KOENNEN VERBESSERTE REFLEXIONSWERTE (RR) BERECHNET WERDEN
C
              IF(JUN.EQ.2) THEN
                CALL MALKO(RR(1,KWO,K),RD,L,KWO,K)
              ENDIF
     	      DO I=1,NWE
	          AM(NA+I,L)=AM(NA+I,L)+RD(I)
              END DO

            ENDDO
         ENDIF
      ENDIF
C       
      IBA=IBAN(LK,NN,IBI)
      GDV=GDE/SUF
      DO L=1,NN
        DO I=1,NZU
         AM(NA+I,L)=GDV*AM(NA+I,L)
         IF(L.EQ.IBA) THEN
           AM(NA+I,L)=AM(NA+I,L)+0.0001*GDV
         ENDIF
        END DO
      END DO
C 

C
C 
C 
C 
C 
 150  NA=0
C 
C 
      CALL MARGEW(IER,C,NN,MD,NZ,F(1),AM(1,1))
C 
C 
C 
      NZ=NZU+NZ
C
C 
C 
C 
C
C
C
       
      RETURN
      END
C
C
C
C
C
C                           +-------+ 
C                           !FFRWRT  !
C                           +-------+ 
      SUBROUTINE FFRWRT(IER,C,NN,F,NZ)
      USE MODGREN
      USE MODFARB
      USE MODMERZ
      USE MODRWRZ
      USE MODWINK,ONLY:FWI
      USE MODILLU,ONLY:FA

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C
C     R-Werte Angleich +
C
C 
C
      DIMENSION C(*),F(*)
C
C
C
      IER=0
      KM=KMS()
      NWE=NWS()
      SUF=1.0E-30
      DO KW=1,KM
        SUF=SUF+FWI(KW)
      END DO
      SUF=SUF*NWE
C
      NAV=0
C 
  5   JV=JUV
      IF(KGX.EQ.1) THEN
        IF(JV.LT.JO) JV=JUU
      ENDIF

C 
C 
      DO  K=1,JV
C
C
C 
C 
         DO KW=1,KM
C
C
C        BERECHNUNG DER REFLEXIONSWERTE
C
C
C
C
C
            CALL MALRW(NN,C,RR(1,KW,K),KW,K)
C
C
C           BEI ZWEI MESSUNGEN DES UNTERGRUNDES KOENNEN VERBESSERTE
C           REFLEXIONSWERTE BERECHNET WERDEN
C
C           
            IF(JUN.EQ.2) THEN
               CALL MAKOR(NN,C,RR(1,KW,K),KW,K)
            ENDIF
C
C
C
C 
         ENDDO
      ENDDO      
C 
C 
C 
C 
C 
C 
C 
C 
      NZU=0
C 
C 
C 
      IF(KGX.EQ.0.OR.JO.LT.2) GOTO 50
C 
C     UNTERSCHIED REFLEXION ÜBER WEISS - SCHWARZ (zur Berechnung des Weiss-Schwarz-Kontrastes)
C 
C
      NA=KM*NWE*JUV
C
C     FÜR REFLEXION
C
C
C
      IF(IRETR(1).EQ.IRETR(2).AND.IRETR(1).EQ.0) THEN
        IF(KWO.LE.0.OR.KWO.GT.KM) THEN
          NZU=NWE*KM
          DO KW=1,KM
            NAD=(KW-1)*NWE
            DO I=1,NWE
              F(NA+NAD+I)=FWI(KW)*(RR(I,KW,1)-RR(I,KW,2)-REPS)
            END DO
          END DO
        ELSE
          NZU=NWE
          DO I=1,NWE
            F(NA+I)=(RR(I,KWO,1)-RR(I,KWO,2)-REPS)
          END DO
        ENDIF
C
C
C     FÜR TRANSMISSION
C
C
C
      ELSEIF (IRETR(1).EQ.1.OR.IRETR(2).EQ.1) THEN
        IF(IRETR(1).EQ.1) THEN
          K=1
        ELSEIF(IRETR(2).EQ.1) THEN
          K=2
        ENDIF
        IF(KWO.LE.0.OR.KWO.GT.KM) THEN
          NZU=NWE*KM
          DO KW=1,KM
            NAD=(KW-1)*NWE
            DO I=1,NWE
              F(NA+NAD+I)=FWI(KW)*(RR(I,KW,K)-REPS)
            END DO
          END DO
        ELSE
          NZU=NWE
          DO I=1,NWE
            F(NA+I)=(RR(I,KWO,K)-REPS)
          END DO
        ENDIF
      ENDIF
C
      IBA=IBAN(LK,NN,IBI)
      GDV=GDE/SUF
      DO I=1,NZU
         F(NA+I)=GDV*F(NA+I)
         IF(IBA.NE.0) THEN
             F(NA+I)=F(NA+I)+.0001*GDV*(C(IBA)-1.)
         ENDIF
      END DO
C

C
C
C 
C 
  50  NA=0
C
C    
      CALL FFRGEW(IER,C,NN,F(1),NZ)
      NA=NZU+NZ
C
C 
C 
C 
      NZ=NA
C 

C
      RETURN
      END 
C 
CC
C******************************************************************************
C
C

C******C******
C******C******
C
C                           +-------+ 
C                           !MARGEW  !
C                           +-------+ 
      SUBROUTINE MARGEW(IER,C,NN,MD,NZ,F,AM)
      USE MODMERZ
      USE MODRWRZ
      USE MODWINK,ONLY:FWI


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C
C     C     FARB/BINDEMITTELKONZENTRATIONEN (SUMME = 1.)
C     NN    ANZAHL FARB/BINDEMITTEL
C     MD    DIMENSION VON AM IM RUFENDEN PROGRAMM
C     NZ    ANZAHL MINIMIERUNGSBEDINGUNGEN
C     AM    MATRIX MIT MINIMIERUNGSBEDINGUNGEN
C 
C
      SAVE JV
C
      DIMENSION AM(MD,*),C(*),F(*)
C 
C
C

C
C     BERECHNUNG DER MENGEN AUS LINEAREM SYSTEM
C 
C 
      KM=KMS()
      NWE=NWS()

      IER=0
C
C 
      DO K=1,JUV
C
C
C     BERECHNUNG DER REFLEXIONSWERTE UND DER ABLEITUNGEN NACH
C     ABSORPTION UND STREUUNG
C
C
C 
C 
C 
         DO KW=1,KM
            CALL MALAB(NN,C,RR(1,KW,K),KW,K)
C
C
C           
C
C           
            CALL MAKOR(NN,C,RR(1,KW,K),KW,K)
C
C
         ENDDO
      ENDDO    


      DO K=1,JUV
         NAV=(K-1)*KM*NWE
         DO KW=1,KM
            NAD=(KW-1)*NWE
            DO L=1,NN
C
C                    
C               ABLEITUNGEN DR/DC
C                  
C
C
              CALL MALDC(NN,C,RD,L,KW,K)
              CALL MALKO(RR(1,KW,K),RD,L,KW,K)
C
              DO I=1,NWE
                AM(NAV+NAD+I,L)=RGWW(I)*FU(K)*FWI(KW)*RD(I)
              END DO
            ENDDO
         ENDDO
      ENDDO
      NZ=JUV*KM*NWE
      RETURN
      END
C
C
C
C
C                           +-------+ 
C                           !FFRGEW  !
C                           +-------+ 
      SUBROUTINE FFRGEW(IER,C,NN,F,NZ)
      USE MODMERZ
      USE MODRWRZ
      USE MODWINK,ONLY:FWI

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C
      DIMENSION C(*),F(*)
C
C     R-Werte-Angleich
C
      IER=0
      KM=KMS()
      NWE=NWS()
 
      DO  K=1,JUV
         NAV=(K-1)*KM*NWE
         DO  KW=1,KM
          NAD=(KW-1)*NWE
C
C 
C
C         KORREKTUR FUER NACHSTELLUNG BERECHNEN
C
            CALL MALRW(NN,C,RR(1,KW,K),KW,K)
C
C
C           verbesserte Werte für Koprrekturrechnung
C
            CALL MAKOR(NN,C,RR(1,KW,K),KW,K)


C
          DO I=1,NWE
             F(NAV+NAD+I)=RGWW(I)*FU(K)*FWI(KW)*(RR(I,KW,K)-RV(I,KW,K))
          END DO
C 
C
C
         ENDDO
      ENDDO
      NZ=JUV*KM*NWE
C 
      RETURN
      END
C 

C
C                           +-------+
C                           !MATRA  ! 
C                           +-------+ 
      SUBROUTINE MATRA(IER,C,NN,MD,NZ,F,AM)
      USE MODFARB
      USE MODMERZ
      USE MODRWRZ
      USE MODXYRZ
c      USE MODSORT
      USE MODWINK,ONLY:FWI
      USE MODILLU,ONLY:FA
C
C     Farbwerteangleich

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C
C     C     FARB/BINDEMITTELKONZENTRATIONEN (SUMME = 1.)
C     NN    ANZAHL FARB/BINDEMITTEL
C     MD    DIMENSION VON AM IM RUFENDEN PROGRAMM
C     NZ    ANZAHL MINIMIERUNGSBEDINGUNGEN
C     AM    MATRIX MIT MINIMIERUNGSBEDINGUNGEN
C 
C 
      SAVE DQ,JV
C 
      DIMENSION XYZQ(3),AM(MD,*),C(*),F(*),XYZU(3)
      DIMENSION AQH(3,3)
      DATA XYZU/0.D0,0.D0,0.D0/
C 
C 
C

C 
C 
C
C

C
C     BERECHNUNG DER MENGEN AUS LINEAREM SYSTEM
C 
C 
      KM=KMS()

      IER=0
C
C 
C 
C
C 
C 
  5   JV=JUV

      IF(KGX.EQ.1) THEN
        IF(JV.LT.JO) JV=JUU
      ENDIF

C 
C 
      DO K=1,JV
C
C
C     BERECHNUNG DER REFLEXIONSWERTE UND DER ABLEITUNGEN NACH
C     ABSORPTION UND STREUUNG
C
C
C 
C 
C 
         DO KW=1,KM
         CALL MALAB(NN,C,RR(1,KW,K),KW,K)
C

C           BEI ZWEI MESSUNGEN DES UNTERGRUNDES KOENNEN VERBESSERTE
C           REFLEXIONSWERTE BERECHNET WERDEN
C
C           
            IF(JUN.EQ.2) THEN
               CALL MAKOR(NN,C,RR(1,KW,K),KW,K)
            ENDIF
C
C
         ENDDO
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
      NZU=0
C 
C 
      IF(KGX.EQ.0.OR.JO.LT.2) GOTO 50
C 
C 
      NZU=1
C 
C
C 
C     FÜR REFLEXION KONTRAST-DE
C 
C
C

C 
      IF(IRETR(1).EQ.IRETR(2).AND.IRETR(1).EQ.0) THEN

        DO K=1,JV
C
C
C       TRISTIMULUSWERTE NACHSTELLUNG
C 
C 
          IF(KWO.LE.0.OR.KWO.GT.KM) THEN
           DO KW=1,KM
            GKK=GLZNOG(KW,IRETR(K))
            CALL NOGXX(XYZB(1,1,KW,K),1,RR(1,KW,K),GKK)
           ENDDO
          ELSE
           GKK=GLZNOG(KWO,IRETR(K))
           CALL NOGXX(XYZB(1,1,KWO,K),1,RR(1,KWO,K),GKK)
          ENDIF
        ENDDO
C 
C 

C 
C 
C 
C       TRANSLUZENTE SCHICHT
C 
C 
        IF(KWO.LE.0.OR.KWO.GT.KM) THEN
          DQ=0.
          SUF=0.
          DO KW=1,KM
            CALL DELABAL(JABST,XYZB(1,1,KW,1),XYZB(1,1,KW,JO),DEV(KW),
     &               DI(1,KW),DC,DH,DI(2,KW),DI(3,KW),1)   
            DQ=DQ+FWI(KW)*DEV(KW)**2
            SUF=SUF+FWI(KW)
          ENDDO
          DVV=SQRT(DQ/SUF)
        ELSE
          CALL DELABAL(JABST,XYZB(1,1,KWO,1),XYZB(1,1,KWO,JO),DEV(KWO),
     &               DI(1,KWO),DC,DH,DI(2,KWO),DI(3,KWO),1)
          DVV=DEV(KWO)
        ENDIF
C
        DO  K=1,JO,JO-1
         IF(KWO.LE.0.OR.KWO.GT.KM) THEN
           DO KW=1,KM
C
C
C           ABLEITUNGEN DLAB/DXYZ
C           
            CALL DEMATAL(JABST,XYZB(1,1,KW,K),AQH,1)

            DO L=1,3
               SUMM=0.
                 DO I=1,3  
                    SUMM=SUMM+AQH(I,L)*DI(I,KW)
                 ENDDO 
               AD(L,KW,K)= GDE*FWI(KW)*SUMM/(DVV*SUF+EPL)
               IF(K.EQ.1) AD(L,KW,K)=-AD(L,KW,K)
            ENDDO
           ENDDO
         ELSE
            CALL DEMATAL(JABST,XYZB(1,1,KWO,K),AQH,1)
            DO L=1,3
               SUMM=0.
                 DO I=1,3  
                    SUMM=SUMM+AQH(I,L)*DI(I,KWO)
                 ENDDO 
               AD(L,KWO,K)=GDE*SUMM/(DVV+EPL)
               IF(K.EQ.1) AD(L,KWO,K)=-AD(L,KWO,K)
            ENDDO
         ENDIF
        ENDDO
C
C
C
C       FÜR TRANSMISSION FARBABSTAND ZU IDEAL-SCHWARZ
C
C
C
      ELSEIF (IRETR(1).EQ.1.OR.IRETR(2).EQ.1) THEN
        IF(IRETR(1).EQ.1) THEN
          K=1
        ELSEIF(IRETR(2).EQ.1) THEN
          K=2
        ENDIF
C
C
C       TRISTIMULUSWERTE NACHSTELLUNG
C 
C 
        IF(KWO.LE.0.OR.KWO.GT.KM) THEN
         DO KW=1,KM
            GKK=GLZNOG(KW,IRETR(K))
            CALL NOGXX(XYZB(1,1,KW,K),1,RR(1,KW,K),GKK)
         ENDDO
        ELSE
         GKK=GLZNOG(KWO,IRETR(K))
         CALL NOGXX(XYZB(1,1,KWO,K),1,RR(1,KWO,K),GKK)
        ENDIF
C
C 

C 
C 
C 
C       TRANSLUZENTE SCHICHT
C 
C 
        IF(KWO.LE.0.OR.KWO.GT.KM) THEN
          DQ=0.
          SUF=0.
          DO KW=1,KM
            CALL DELABAL(JABST,XYZU,XYZB(1,1,KW,K),DEV(KW),
     &               DI(1,KW),DC,DH,DI(2,KW),DI(3,KW),1)   
            DQ=DQ+ FWI(KW)*DEV(KW)**2
            SUF=SUF+FWI(KW)
          ENDDO
          DVV=SQRT(DQ)/SUF
        ELSE
          CALL DELABAL(JABST,XYZU,XYZB(1,1,KWO,K),DEV(KWO),
     &               DI(1,KWO),DC,DH,DI(2,KWO),DI(3,KWO),1)   
          DVV=DEV(KWO)
        ENDIF

C
        IF(KWO.LE.0.OR.KWO.GT.KM) THEN
         DO KW=1,KM
C
C
C           ABLEITUNGEN DLAB/DXYZ
C           
            CALL DEMATAL(JABST,XYZB(1,1,KW,K),AQH,1)
            DO L=1,3
               SUMM=0.
                 DO I=1,3  
                    SUMM=SUMM+AQH(I,L)*DI(I,KW)
                 ENDDO 
               AD(L,KW,K)= GDE*FWI(KW)*SUMM/(DVV*SUF+EPL)
            ENDDO
         ENDDO
        ELSE
            CALL DEMATAL(JABST,XYZB(1,1,KWO,K),AQH,1)
            DO L=1,3
               SUMM=0.
                 DO I=1,3  
                    SUMM=SUMM+AQH(I,L)*DI(I,KWO)
                 ENDDO 
               AD(L,KWO,K)=GDE*SUMM/(DVV+EPL)
            ENDDO
        ENDIF
      ENDIF

C 

C 
C 
C     FARBABSTAENDE FUER NORMLICHTARTEN (JN) UND UNTERGRUENDE (JUV)
C 
C 
C 
  50  NA=0
      DO  K=1,JUV
         DO  KW=1,KM
C
C 
C
C         KORREKTUR FUER NACHSTELLUNG BERECHNEN, FALLS NOCH NICHT ERFOLGT
C
          IF(JUN.EQ.1) THEN
             CALL MAKOR(NN,C,RR(1,KW,K),KW,K)
          ENDIF
C 

C 
C 
            DO  J=1,JN
C
C
C              TRISTIMULUSWERTE NACHSTELLUNG                       
C 
C 
               GKK=(1.-IRETR(K))*GLANZ(KW)
               CALL NOGXX(XYZB(1,J,KW,K),J,RR(1,KW,K),GKK)
               FAKT=FA(J)*FU(K)*FWI(KW)
               CALL DELABAL(JABST,XYZV(1,J,KW,K),XYZB(1,J,KW,K),DE,
     &                      DL,DC,DH,DA,DB,J)        
C 
C
C
C
C
C                DLAB/DXYZ
C
C
C
C
C
                 CALL DEMATAL(JABST,XYZB(1,J,KW,K),AQH,J)
                 DO  L=1,3
C                  DL/DXYZ
                   AQ(1,L,J,KW,K)=FAKT*WLCH(1)*AQH(1,L)      
C                  DC/DXYZ
                   AQ(2,L,J,KW,K)=FAKT*WLCH(2)*(AQH(2,L)*CPHI(J,KW,K)
     &                                         +AQH(3,L)*SPHI(J,KW,K))
C                  DH/DXYZ
                   AQ(3,L,J,KW,K)=FAKT*WLCH(3)*(-AQH(2,L)*SPHI(J,KW,K)
     &                                          +AQH(3,L)*CPHI(J,KW,K))
                 ENDDO     
            ENDDO
         ENDDO
      ENDDO

C 
      NA=3*JN*JUV*KM+NZU
C
C
C 
      NZ=NA
      IF(NZ.GT.MD) THEN
          CALL FEHER(212,1000*MD+NZ)
          CALL FSTOP(10)
      ENDIF
C 
C 
C 
C     RR           REFLEXIONSWERTE
C 
C 
C 
C 
C 
C 
C 
C 
  100 NZU=0
C 
C 
      IF(KGX.EQ.0.OR.JO.LT.2) GOTO 150 
C 
      NZU=1
C 
      NA=3*JN*JUV*KM+1
C
C
C
C 
C     FÜR REFLEXION KONTRAST-DE
C 
C
C
      DO L=1,NN
         AM(NA,L)=0.
      ENDDO
C
      IF(IRETR(1).EQ.IRETR(2).AND.IRETR(1).EQ.0) THEN

C 
C     TRANSLUZENTE SCHICHT     
C 
C
C 
C 
      DO K=1,JO,JO-1
       IF(KWO.LE.0.OR.KWO.GT.KM) THEN
        DO KW=1,KM
          DO L=1,NN
            CALL MALDC(NN,C,RD,L,KW,K)
C
C           BEI ZWEI MESSUNGEN DER NACHSTELLUNG (UEBER WEISS UND SCHWARZ)
C           KOENNEN VERBESSERTE REFLEXIONSWERTE (RR) BERECHNET WERDEN
C
            IF(JUN.EQ.2) THEN
               CALL MALKO(RR(1,KW,K),RD,L,KW,K)
            ENDIF
C
C
C                
C              
C            DXYZ/DC(K)
C             
C             
C              
             GKK=(1.-IRETR(K))*GLANZ(KW)
             CALL NOGXD(XYZQ,1,RD,GKK)
C              
C
C               
C            DDE/DC(K) 
C             
             DO  LJ=1,3  
                AM(NA,L)=AM(NA,L)+AD(LJ,KW,K)*XYZQ(LJ) 
             ENDDO
          ENDDO
        ENDDO
       ELSE
          DO L=1,NN
            CALL MALDC(NN,C,RD,L,KWO,K)
C
C           BEI ZWEI MESSUNGEN DER NACHSTELLUNG (UEBER WEISS UND SCHWARZ)
C           KOENNEN VERBESSERTE REFLEXIONSWERTE (RR) BERECHNET WERDEN
C
            IF(JUN.EQ.2) THEN
               CALL MALKO(RR(1,KWO,K),RD,L,KWO,K)
            ENDIF
C
C                
C              
C            DXYZ/DC(K)
C             
C             
C              
             GKK=(1.-IRETR(K))*GLANZ(KWO)
             CALL NOGXD(XYZQ,1,RD,GKK)
C
C             
C               
C            DDE/DC(K) 
C             
             DO  LJ=1,3  
                AM(NA,L)=AM(NA,L)+AD(LJ,KWO,K)*XYZQ(LJ) 
             ENDDO
          ENDDO
       ENDIF
      ENDDO
C       
C 
C
C
C
C     FÜR TRANSMISSION FARBABSTAND ZU IDEAL-SCHWARZ
C
C
C
      ELSEIF (IRETR(1).EQ.1.OR.IRETR(2).EQ.1) THEN
        IF(IRETR(1).EQ.1) THEN
          K=1
        ELSEIF(IRETR(2).EQ.1) THEN
          K=2
        ENDIF

C 
C     TRANSLUZENTE SCHICHT     
C 
C
C 
C 
       IF(KWO.LE.0.OR.KWO.GT.KM) THEN
        SUF=1.0E-30
        DO KW=1,KM
          SUF=SUF+ FWI(KW)
        ENDDO
        DO L=1,NN
           AM(NA,L)=0.0
        ENDDO
        DO KW=1,KM
          DO L=1,NN
            CALL MALDC(NN,C,RD,L,KW,K)
C
C           BEI ZWEI MESSUNGEN DER NACHSTELLUNG (UEBER WEISS UND SCHWARZ)
C           KOENNEN VERBESSERTE REFLEXIONSWERTE (RR) BERECHNET WERDEN
C
            IF(JUN.EQ.2) THEN
               CALL MALKO(RR(1,KW,K),RD,L,KW,K)
            ENDIF
C
C
C                
C              
C            DXYZ/DC(K)
C             
C             
C              
             GKK=GLZNOG(KW,IRETR(K))
             CALL NOGXD(XYZQ,1,RD,GKK)
C
C               
C            DDE/DC(K) 
C             
             DO  LJ=1,3  
                AM(NA,L)=AM(NA,L)+AD(LJ,KW,K)*XYZQ(LJ) 
             ENDDO
          ENDDO
        ENDDO
        DO KW=1,KM
          DO L=1,NN
             AM(NA,L)= FWI(KW)*AM(NA,L)/SUF
          ENDDO
        ENDDO
                                                       
       ELSE
          DO L=1,NN
            CALL MALDC(NN,C,RD,L,KWO,K)
C
C           BEI ZWEI MESSUNGEN DER NACHSTELLUNG (UEBER WEISS UND SCHWARZ)
C           KOENNEN VERBESSERTE REFLEXIONSWERTE (RR) BERECHNET WERDEN
C
            IF(JUN.EQ.2) THEN
               CALL MALKO(RR(1,KWO,K),RD,L,KWO,K)
            ENDIF
C
C                
C              
C            DXYZ/DC(K)
C             
C             
C              
             GKK=GLZNOG(KWO,IRETR(K))
             CALL NOGXD(XYZQ,1,RD,GKK)
C              
C             
C               
C            DDE/DC(K) 
C             
             DO  LJ=1,3  
                AM(NA,L)=AM(NA,L)+AD(LJ,KWO,K)*XYZQ(LJ) 
             ENDDO
          ENDDO
       ENDIF

      ENDIF
C
      IBA=IBAN(LK,NN,IBI)
      IF(IBA.NE.0) THEN
         AM(NA,IBA)=AM(NA,IBA)+0.0001*GDE
      ENDIF
C
C
C
C 
C 
C 
C 
C 
 150  NA=0
      DO K=1,JUV
         DO KW=1,KM
            DO L=1,NN
               CALL MALDC(NN,C,RD,L,KW,K)
               IF(JUN.GT.0) THEN
                  CALL MALKO(RR(1,KW,K),RD,L,KW,K)
               ENDIF
                NA=3*JN*((K-1)*KM+KW-1)
C
C                
C                    
C               ABLEITUNGEN DR/DC
C                  
C
C
C
 
                DO  J=1,JN
                    FAKT=FA(J)*FU(K)*FWI(KW)
                       GKK=GLANZ(KW)
                       CALL NOGXD(XYZQ,J,RD,GKK)
C
C
C
C
C
C                      DLAB/DC(L)
C
C
C
C
                       DO IV=1,3
                          NA=NA+1
                          SUMM=0.
                          DO  IW=1,3
                              SUMM=SUMM+XYZQ(IW)*AQ(IV,IW,J,KW,K)
                          ENDDO        
                          AM(NA,L)=SUMM
                       ENDDO
                ENDDO
            ENDDO
         ENDDO
      ENDDO
C 
C 

C 
C 
C 
      NA=NZU+3*JN*JUV*KM
C
C 
C 
C 
      NZ=NA
C
C
C

C
       
      RETURN
      END
C 

C
C
C                           +-------+ 
C                           !FFUNK  !
C                           +-------+ 
      SUBROUTINE FFUNK(IER,C,NN,F,NZ)
      USE MODFARB
      USE MODMERZ
      USE MODRWRZ
      USE MODXYRZ
c      USE MODSORT
      USE MODWINK,ONLY:FWI
      USE MODILLU,ONLY:FA

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     Farbwerteangleich
C 
C 
      DIMENSION C(*),F(*),XYZU(3)
C
      DATA XYZU/0.D0,0.D0,0.D0/

C 
C
      KM=KMS()

      IER=0
C
C 
  5   JV=JUV
      IF(KGX.EQ.1) THEN
             IF(JV.LT.JO) JV=JUU
      ENDIF
C
C 
      DO  K=1,JV
C
C
C 
C 
         DO KW=1,KM
C
C
C        BERECHNUNG DER REFLEXIONSWERTE
C
C
C
C
C
            CALL MALRW(NN,C,RR(1,KW,K),KW,K)
C
C           BEI ZWEI MESSUNGEN DES UNTERGRUNDES KOENNEN VERBESSERTE
C           REFLEXIONSWERTE BERECHNET WERDEN
C
C           
            IF(JUN.EQ.2) THEN
               CALL MAKOR(NN,C,RR(1,KW,K),KW,K)
            ENDIF
C
C
C
C 
         ENDDO
      ENDDO
C 
C 
C 
C 
C 
C 
C 
C 
      NZU=0
C 
C 
C 
      IF(KGX.EQ.0.OR.JO.LT.2) GOTO 50
C 
C
      NZU=1
C 
C 
C     FÜR REFLEXION KONTRAST-DE
C 
C
C
      IF(IRETR(1).EQ.IRETR(2).AND.IRETR(1).EQ.0) THEN

        DO K=1,JO
C 
C 
          IF(KWO.LE.0.OR.KWO.GT.KM) THEN
            DO KW=1,KM
C
C
C
C
C
C
C
C 
C 
C     TRISTIMULUSWERTE NACHSTELLUNG (OHNE KORREKTUR DER R-WERTE)                      
C 
C 
C 
               GKK=GLZNOG(KW,IRETR(K))
               CALL NOGXX(XYZB(1,1,KW,K),1,RR(1,KW,K),GKK)
            ENDDO
          ELSE
            GKK=GLZNOG(KWO,IRETR(K))
            CALL NOGXX(XYZB(1,1,KWO,K),1,RR(1,KWO,K),GKK)
          ENDIF
        ENDDO
C
C 
C
C 
C       TRANSLUZENTE SCHICHT
C 
C 
        IF(KWO.LE.0.OR.KWO.GT.KM) THEN
           DQ=0.
           SUF=0.
           DO KW=1,KM
             CALL DELABAL(JABST,XYZB(1,1,KW,1),XYZB(1,1,KW,JO),DEV(KW),
     &                DI(1,KW),DC,DH,DI(2,KW),DI(3,KW),1)   
             DQ=DQ+FWI(KW)*DEV(KW)**2
             SUF=SUF+FWI(KW)
           ENDDO
           DVV=SQRT(DQ/SUF)
        ELSE
           CALL DELABAL(JABST,XYZB(1,1,KWO,1),XYZB(1,1,KWO,JO),DEV(KWO),
     &                DI(1,KWO),DC,DH,DI(2,KWO),DI(3,KWO),1)   
           DVV=DEV(KWO)
        ENDIF
        NA=3*JUV*JN*KM+1
        F(NA)=GDE*(DVV-FDE)
C
C
C     FÜR TRANSMISSION FARBABSTAND ZU IDEAL-SCHWARZ
C
C
C
      ELSEIF (IRETR(1).EQ.1.OR.IRETR(2).EQ.1) THEN
        IF(IRETR(1).EQ.1) THEN
          K=1
        ELSEIF(IRETR(2).EQ.1) THEN
          K=2
        ENDIF

C
C 
          IF(KWO.LE.0.OR.KWO.GT.KM) THEN
            DO KW=1,KM
C
C
C
C
C
C
C
C 
C 
C     TRISTIMULUSWERTE NACHSTELLUNG (OHNE KORREKTUR DER R-WERTE)                      
C 
C 
C 
               GKK=GLZNOG(KW,IRETR(K))
               CALL NOGXX(XYZB(1,1,KW,K),1,RR(1,KW,K),GKK)
             ENDDO
          ELSE
             GKK=GLZNOG(KWO,IRETR(K))
             CALL NOGXX(XYZB(1,1,KWO,K),1,RR(1,KWO,K),GKK)
          ENDIF
C
C 
C
C 
C       TRANSLUZENTE SCHICHT
C 
C 
        IF(KWO.LE.0.OR.KWO.GT.KM) THEN
           DQ=0.
           SUF=1.E-30
           DO KW=1,KM
             CALL DELABAL(JABST,XYZU,XYZB(1,1,KW,K),DEV(KW),
     &                DI(1,KW),DC,DH,DI(2,KW),DI(3,KW),1)   
             DQ=DQ+FWI(KW)*DEV(KW)**2
             SUF=SUF+FWI(KW)
           ENDDO
           DVV=SQRT(DQ/SUF)
        ELSE
           CALL DELABAL(JABST,XYZU,XYZB(1,1,KWO,K),DEV(KWO),
     &                DI(1,KWO),DC,DH,DI(2,KWO),DI(3,KWO),1)   
           DVV=DEV(KWO)
        ENDIF
        NA=3*JUV*JN*KM+1
        F(NA)=GDE*(DVV-FDE)
      ENDIF
      IBA=IBAN(LK,NN,IBI)
      IF(IBA.NE.0) THEN
         F(NA)=F(NA)+0.0001*GDE*(C(IBA)-1.)
      ENDIF


C
C
C 
C 
C 
C     FARBABSTAENDE FUER NORMLICHTARTEN (JN) UND UNTERGRUENDE (JUV)
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
  50  NA=0
      DO  K=1,JUV
         DO KW=1,KM
C
C         BERECHNUNG DER KORRIGIERTEN REFLEXIONSWERTE
C
C
C****     CALL MALRW(NN,C,NWE,RR(1+NAD,K),KW,K)
C
C         KORREKTUR FUER NACHSTELLUNG BERECHNEN, FALLS NOCH NICHT ERFOLGT
C
          GKK=GLZNOG(KW,IRETR(K))
          IF(JUN.EQ.1) THEN
             CALL MAKOR(NN,C,RR(1,KW,K),KW,K)
          ENDIF
            DO J=1,JN
               CALL NOGXX(XYZB(1,J,KW,K),J,RR(1,KW,K),GKK)
               FAKT=FA(J)*FU(K)*FWI(KW)
                 CALL DELABAL(JABST,XYZV(1,J,KW,K),XYZB(1,J,KW,K),DE,
     &                      DL,DC,DH,DA,DB,J)        
C 
C 
C                HELLIGKEITSDIFFERENZ
C 
C 
                 NA=NA+1 
                 F(NA)=WLCH(1)*FAKT*DL
C 
C 
C 
C 
C 
C                C-KOORDINATE
C 
C 
                 NA=NA+1 
                 F(NA)=WLCH(2)*FAKT*(DA*CPHI(J,KW,K)+
     &                                    DB*SPHI(J,KW,K))
C 
C 
C 
C                H-KOORDINATE
C 
C 
C 
                 NA=NA+1 
                 F(NA)=WLCH(3)*FAKT*(-DA*SPHI(J,KW,K)+
     &                                     DB*CPHI(J,KW,K))     
C 
C
            ENDDO
         ENDDO
      ENDDO     
C 
      NA=NA+NZU
      NZ=NA
C
C

      RETURN
      END 
C******************************************************************************
C
C
C
C
C 
C******************************************************************************
C 
C
      SUBROUTINE MUNEQU(IER,X,NCOLS,MDW,MCON,A)
      USE MODFARB
      USE MODGREN

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*),A(MDW,*)
C
C     GLEICHUNGEN UND UNGLEICHUNGEN WERDEN AUFGEBAUT
C
C
C
C
C 
C     AUFBAU A(K,J) FUER DIE GLEICHUNGEN UND UNGLEICHUNGEN
C 
      K=0
      DO  II=1,NEE
      	DO L=1,NCOLS
          IF(IGR(II,LK(L)).EQ.1) THEN
            K=K+1
     	    DO J=1,NCOLS
              LKK=LK(J)
              IF(LKK.LE.0) THEN
                 GOTO 90
              ENDIF
              BBB=AGR(II,LKK)
              A(K,J)=BBB
      	    ENDDO
C           A(K,NCOLS+1)=AGR(II,NF1)
            EXIT
          ENDIF
      	END DO
      ENDDO
      MCON=K
C
C
C
C 
C 
C 
C
      RETURN
  90  IER=90                 
      END
C******C******

C******************************************************
C******************************************************************************
C******************************************************
C******************************************************


      SUBROUTINE FUNEQU(IER,X,NCOLS,MCON,F)
      USE MODGREN
      USE MODFARB

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*),F(*)
C
C     GLEICHUNGEN UND UNGLEICHUNGEN Für Mengen
C
C
C
C
C
C
C 
C 
C 
C     AUFBAU F(K) FUER DIE GLEICHUNGEN UND UNGLEICHUNGEN
C 
      K=0
      DO  II=1,NEE
      	DO L=1,NCOLS
          IF(IGR(II,LK(L)).EQ.1) THEN
            K=K+1
            SUU=0.
     	    DO J=1,NCOLS
              LKK=LK(J)
              IF(LKK.LE.0) THEN
                 GOTO 90
              ENDIF
              BBB=AGR(II,LKK)
              SUU=SUU+X(J)*BBB
      	    ENDDO
            F(K)=SUU-AGR(II,NF1)
C
C           Für IGX=5 wurde IGR(II,NF1)=1 gesetzt (SUB REZLIMT)
C           Zuwaage kann über ZUWIT verändert werden
C
            IF(IGR(II,NF1).EQ.-1) THEN
                AG5=AGR(II,NF1)
            ENDIF
            IF(IGR(II,NF1).EQ.1) THEN
c
               F(K)=F(K)+(AGR(II,NF1)+AG5)*(1.0-ZUWIT)
            ENDIF
            EXIT
          ENDIF
      	END DO
      ENDDO
      MCON=K
C
      RETURN
  90  IER=90                 
      END
c

C
C******C******
      FUNCTION NUNEQU(NEZ,NCOLS)
      USE MODGREN
      USE MODFARB

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ANZAHL GEMAESS NEZ (FUER GLEICHUNGEN BZW. UNGLEICHUNGEN)
C
C
C
C
      K=0
      DO  II=1,NEZ
      	DO L=1,NCOLS
          IF(IGR(II,LK(L)).EQ.1) THEN
            K=K+1
            EXIT
          ENDIF
      	END DO
      ENDDO
      NUNEQU=K

C 
C 
C
C
      RETURN
      END

C

C 
C
C
c
c

c
c
c
      REAL(KIND=8) FUNCTION FZUWIT(ZUWI)
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODRWRZ,ONLY:RV,RR,IRETR
      USE MODGREN
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DATA MAXFUN/50/,K/1/
C
C
C      Function für Intervallschachtelung zur Begrenzung der Zuwaage
C      bei vorgegebenem Farbabstand DTO
C
      DO I=1,NF
         ACONZ(I)=ACZWI(I)
      END DO

      FZUWIT=0.0
      ZUWIT=ZUWI
C
C     Angleich gemäß KFJT
C
C
      CALL REZFIT(KFJT,NF,ITM,MAXFUN,ATO,
     &                    ACONZ,PHI,IER)
      IF(IFEHL(IER).NE.0) THEN
         GOTO 900
      ENDIF

      CALL FADIFF(JABST,KWE,IRETR(K),NWS(),RV(1,1,K),RR(1,1,K),
     &               DE,DL,DC,DH,DA,DB,AMA,1,1)
      FZUWIT=DTO-DE

 900  RETURN
      END
C 

C
      REAL(KIND=8) FUNCTION FUNGGE(GGEL)
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODRWRZ,ONLY:RV,RR,IRETR
      USE MODGREN
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DATA MAXFUN/50/,K/1/
C
C
C
C
C
C      Function für Intervallschachtelung zur Begrenzung Mengenänderung
C      bei vorgegebenem Farbabstand DTO
C

      FUNGGE=0.0
      DO I=1,NF
         ACONZ(I)=ACZWI(I)
      END DO
      GGE=GGEL
C
C     Angleich R-Werte
C
C
C
C      CALL REZFIT(2,NF,ITM,MAXFUN,ATO,
C    &                    ACONZ,PHI,IER)
C
C     Angleich gemäß KFJT
C
C
      CALL REZFIT(KFJT,NF,ITM,MAXFUN,ATO,
     &                    ACONZ,PHI,IER)
      IER=0
      IF(IFEHL(IER).NE.0) THEN
         GOTO 900
      ENDIF
      CALL FADIFF(JABST,KWE,IRETR(K),NWS(),RV(1,1,K),RR(1,1,K),
     &               DE,DL,DC,DH,DA,DB,AMA,1,1)
      FUNGGE=DE-DTO
 900  RETURN
      END

C 
C
C

C
C 
      SUBROUTINE MINFUN(NN,X,LK,NA,F,IGX,HXG,HGE)
      USE MODGREN
C
C     Minimierungsbedingungen für Mengen
C
C     HXG=GXG (aus MODMERZ)
C      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     HTO GEWICHT  
C
C
      DIMENSION X(*),F(*)
      INTEGER LK(*)
C
C 
C     BEDEUTUNG VON IGX (S. GREMIN (FAKER2))
C
C
      NA=0
      DO I=1,NEM
         NA=NA+1
         SELECT CASE (IGX)
C
C
C            0 NORMAL
C            1 MINIMIERUNG BINDEMITTELMENGE
C            2 MINIMIERUNG FARBMITTELMENGE
C            3 MINIMIERUNG ZUWAAGE
C            4 MINIMIERUNG DIFFERENZ ZU ALTER MENGE
C            5 WIE 3 (VORGEGEBENER FARBABSTAND)
C            6 WIE 4 (VORGEGEBENER FARBABSTAND)
C
             CASE (0,1,2,4,6)
               SUMM=AGR(NEE+I,NF1)
               DO J=1,NN
                 SUMM=SUMM-AGR(NEE+I,LK(J))*X(J)
               ENDDO
               F(NA)=-HGE*SUMM
             CASE (3,5)
               SUMM=AGR(NEE+I,NF1)
               DO J=1,NN
                 SUMM=SUMM-AGR(NEE+I,LK(J))*X(J)
               ENDDO
               F(NA)=-HXG*SUMM
         END SELECT
      ENDDO
C
      RETURN
      END
C



      SUBROUTINE MINMAT(NN,X,LK,MDIM,NA,A,IGX,
     &                  HXG,HGE)
      USE MODGREN

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C 
C
C 
C     BEDEUTUNG VON IGX (S. GREMIN (FAKER2))
C
C
C 
C     HGE GEWICHT ZUR DAEMPFUNG
C
C
      DIMENSION X(*),A(MDIM,*)
      INTEGER LK(*)
C
C
C      
C     MINIMIERUNG
C
      NA=0
C
C
      DO I=1,NEM
         NA=NA+1
         DO J=1,NN
            A(I,J)=0.
         END DO
         DO J=1,NN
         SELECT CASE (IGX)
             CASE (0,1,2,4,6)
                 A(I,J)=HGE*AGR(NEE+I,LK(J))
             CASE (3,5)
                 A(I,J)=HXG*AGR(NEE+I,LK(J))
         END SELECT
         ENDDO
      ENDDO
C      
C
      RETURN
      END
CC
C
C
C

c
c
c
c
C
      SUBROUTINE REDETR(KORR,SENS,N,X)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      REAL(KIND=8),DIMENSION(*) ::X
      REAL(KIND=8) ::KORR,SENS
      REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE  ::A,S
      REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE  ::B
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: GRA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: C
      DATA RANGE/1.D80/,NB/1/,EPL/1.D-70/
      DATA EPS/1.E-6/
C
C
C
C
      IF(N.LE.0) THEN
        KORR=0.
        SENS=0.
        RETURN
      ENDIF
      CALL NUMALL(1,N,MAL,MGAL,MEAL)
      NZ=MAL
      MAL=MAL+1
      MDW=MAX(N,MAL)
      MAL=MDW
      ALLOCATE(A(MAL,N+1),B(MDW,1),GRA(MDW),C(N),S(N,3),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         RETURN
      ENDIF
      CSUM=EPL
      DO I=1,N
         CSUM=CSUM+X(I)
      ENDDO
      IF(CSUM.LE.2.*EPL) THEN
         CSUM=1.
      ENDIF
      DO I=1,N
         C(I)=X(I)/CSUM
      ENDDO
      CALL FFUNK(IER,C,N,GRA,NO)
      CALL MATRA(IER,C,N,MAL,NZ,GRA,A)
C
C     Restdiagonale =0. wegen H12
C

      DO J=NZ+1,N
        DO I=1,N
          A(J,I)=0.
        END DO
      ENDDO
      DO I=1,NO
        B(I,1)=GRA(I)
      END DO
      IF(NZ.GT.MDW) THEN
        KORR=-999.9
        SENS=-999.9
        GOTO 90
      ENDIF
C
C
C
C     BERECHNUNG DER EIGENWERTE
C
C
C

      CALL SVDRS2 (A,MAL,NZ,N,B,MDW,NB,S,IERROR,RANGE)
C
C
C     s. B. Sluban + J. H. Nobbs: Color Research and Application 20(1995) 226
C                    ibid                                        22(1997) 88
C
C
      SENS=S(1,1)
      KORR=0.
      DO J=1,N
         IF(ABS(S(J,1)).LE.EPS*SENS) THEN
            EXIT
         ENDIF
         KORR=S(J,1)
      END DO
C
C
  90  DEALLOCATE(A,B,S,GRA,C)
C
      RETURN
      END

C******************************************************
C******************************************************
C******************************************************
C
C******************************************************
C


      SUBROUTINE MALRW(NN,C,RRR,KW,K)
      USE MODABST
      USE MODFARB
      USE MODMERZ
      USE MODRWRZ

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION RRR(*),C(*)
C
      CALL MIXRW(NN,C,RRR,KW,K)
C
      RETURN
      END
C 
      SUBROUTINE MALAB(NN,C,RRR,KW,K)
c      USE MODABST
c      USE MODFARB
c      USE MODMERZ
c      USE MODRWRZ

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RRR(*),C(*)
      CALL MIXAB(NN,C,RRR,KW,K)

      RETURN
      END
  
C
      SUBROUTINE MALDC(NN,C,RDD,L,KW,K)
c      USE MODABST
c      USE MODFARB
c      USE MODMERZ
c      USE MODRWRZ

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION C(*),RDD(*)
C
C
C
 
      CALL MIXDC(NN,C,RDD,L,KW,K)

      RETURN
      END
 
 
C 
C 
C                           +-------+ 
C                           ! MIXRW ! 
C                           +-------+ 
      SUBROUTINE MIXRW(NN,C,RRR,KW,K)

      USE MODMERZ
      USE MODFARB
      USE MODABST
      USE MODRWRZ
      USE MODGKWR,ONLY:CDE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA DELM/0.0000001/

c
c
c
c
c

      INTERFACE
      REAL(KIND=8) FUNCTION TRRWR(PAS,RU,KW,RT,KU,ABLEI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8),DIMENSION(*) :: PAS
      INTEGER(KIND=4) :: RT,KU,KW
      REAL(KIND=8) ::RU
      REAL(KIND=8),OPTIONAL :: ABLEI(*)
      END FUNCTION
      END INTERFACE


C 
C
C
C     REFLEXIONSWERTE FUER LACKFARBEN,KUNSTSTOFFE,DRUCKFARBEN
C
C 
C 
C
      DIMENSION RRR(*)
      DIMENSION C(*)
      REAL(KIND=8) ::RWW
      INTEGER(KIND=4) :: CNZDEP
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
      KENN=0
      GOTO 10 
      ENTRY MIXAB(NN,C,RRR,KW,K)
      KENN=1
      GOTO 10
C 
C 
C 
C 
C 
C 
C 
C 
C 
  10  NWE=NWS()
      CF=1.D-100
      DO I=1,NN
C
C        reine Farbmittelkonzentration (ohne Bindemittel)
C
C
C         IF(ICHF(LK(I)).NE.1.AND.ICHF(LK(I)).NE.8) THEN
         IF(ICHF(LK(I)).NE.1) THEN
           CF=CF+C(I)
         ENDIF
      END DO
      DO I=1,NWE
C 
C 
C 
        IF(CNZDEP().EQ.1) THEN
C
C         'X' 'H' mit C
C
          ASHIL(1)=0.D0
          ASHIL(2)=0.D0
          DO L=1,NN
           DO J=1,NST(LK(L))+1
             TST(J)=ABSTA(I,KW,J,LK(L))
           ENDDO
           DCL=D(K)*C(L)
           TAU=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
           HI=TST(NST(LK(L))+1)
           ASHIL(1)=ASHIL(1)+HI*TAU
           ASHIL(2)=ASHIL(2)+(1.D0-HI)*TAU
          ENDDO
        ELSEIF(CNZDEP().EQ.2) THEN
C
C         'Y' 'I' mit C
C
          ASHIL(1)=0.D0
          ASHIL(2)=0.D0
          DO L=1,NN
           DO J=1,2*NST(LK(L))
             TST(J)=ABSTA(I,KW,J,LK(L))
           ENDDO
           DCL=D(K)*C(L)
           AA=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
           SS=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST(NST(LK(L))+1))
           ASHIL(1)=ASHIL(1)+AA
           ASHIL(2)=ASHIL(2)+SS
         ENDDO


        ELSEIF(CNZDEP().EQ.3) THEN
C
C         'X' 'H' mit CF
C
          ASHIL(1)=0.D0
          ASHIL(2)=0.D0
          ASHIL(3)=0.D0
          ASHIL(4)=0.D0
          DO L=1,NN
           DO J=1,NST(LK(L))+1
             TST(J)=ABSTA(I,KW,J,LK(L))
           ENDDO
           DCL=D(K)*CF
           TAU=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
           HI=TST(NST(LK(L))+1)
           ASHIL(1)=ASHIL(1)+HI*TAU*C(L)/CF
           ASHIL(2)=ASHIL(2)+(1.D0-HI)*TAU*C(L)/CF
C
C          SUMME TAU-Werte
C
           ASHIL(3)=ASHIL(3)+TAU*C(L)/CF
C
C          SUMME der Ableitungen
C
C
           DAU=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
           ASHIL(4)=ASHIL(4)+DAU*C(L)/CF
          ENDDO
        ELSEIF(CNZDEP().EQ.4) THEN
C
C
C         'Y' 'I' mit CF
C
          ASHIL(1)=0.D0
          ASHIL(2)=0.D0
          ASHIL(3)=0.D0
          ASHIL(4)=0.D0
          DO L=1,NN
           DO J=1,2*NST(LK(L))
             TST(J)=ABSTA(I,KW,J,LK(L))
           ENDDO
           DCL=D(K)*CF
           AA=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
           SS=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST(NST(LK(L))+1))
           ASHIL(1)=ASHIL(1)+AA*C(L)/CF
           ASHIL(2)=ASHIL(2)+SS*C(L)/CF
C
C          SUMME der Ableitungen
C
C
           DAA=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
           ASHIL(3)=ASHIL(3)+DAA*C(L)/CF
           DSS=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST(NST(LK(L))+1))
           ASHIL(4)=ASHIL(4)+DSS*C(L)/CF
         ENDDO
        ELSEIF(CNZDEP().EQ.5) THEN
C
C         'X' 'H' mit CH
C
          ASHIL(1)=0.D0
          ASHIL(2)=0.D0
          ASHIL(3)=0.D0
          ASHIL(4)=0.D0
          DO L=1,NN
           DO J=1,NST(LK(L))+1
             TST(J)=ABSTA(I,KW,J,LK(L))
           ENDDO
C           IF(ICHF(LK(L)).EQ.1.OR.ICHF(LK(L)).EQ.8) THEN
           IF(ICHF(LK(L)).EQ.1) THEN
             CH=1.
           ELSE
             CH=C(L)/(C(L)+1.0-CF+DELM)
           ENDIF
           DCL=D(K)*CH
           TAU=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
           HI=TST(NST(LK(L))+1)
           ASHIL(1)=ASHIL(1)+HI*TAU*C(L)/CH
           ASHIL(2)=ASHIL(2)+(1.D0-HI)*TAU*C(L)/CH
C           IF(ICHF(LK(L)).NE.1.AND.ICHF(LK(L)).NE.8) THEN
           IF(ICHF(LK(L)).NE.1) THEN
C
C            SUMME TAU-Werte
C
             ASHIL(3)=ASHIL(3)+TAU
C
C            SUMME der Ableitungen
C
C
             DAU=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
             ASHIL(4)=ASHIL(4)+DAU*CH
           ENDIF
          ENDDO
        ELSEIF(CNZDEP().EQ.6) THEN
C
C
C         'Y' 'I' mit CH
C
          ASHIL(1)=0.D0
          ASHIL(2)=0.D0
          ASHIL(3)=0.D0
          ASHIL(4)=0.D0
          ASHIL(5)=0.D0
          ASHIL(6)=0.D0
          DO L=1,NN
           DO J=1,2*NST(LK(L))
             TST(J)=ABSTA(I,KW,J,LK(L))
           ENDDO
C           IF(ICHF(LK(L)).EQ.1.OR.ICHF(LK(L)).EQ.8) THEN
           IF(ICHF(LK(L)).EQ.1) THEN
             CH=1.
           ELSE
             CH=C(L)/(C(L)+1.0-CF+DELM)
           ENDIF
           DCL=D(K)*CH
           AA=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
           SS=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST(NST(LK(L))+1))
           ASHIL(1)=ASHIL(1)+AA*C(L)/CH
           ASHIL(2)=ASHIL(2)+SS*C(L)/CH
C           IF(ICHF(LK(L)).NE.1.AND.ICHF(LK(L)).NE.8) THEN
           IF(ICHF(LK(L)).NE.1) THEN
            ASHIL(3)=ASHIL(3)+AA
            ASHIL(4)=ASHIL(4)+SS

C
C           SUMME der Ableitungen
C
C
            DAA=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
            ASHIL(5)=ASHIL(5)+DAA*CH
            DSS=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST(NST(LK(L))+1))
            ASHIL(6)=ASHIL(6)+DSS*CH
           ENDIF
         ENDDO






        ELSE
          DO JP=1,NPX
           ASHIL(JP)=0.D0
           DO  L=1,NN
               ASHIL(JP)=ASHIL(JP)+C(L)*ABSTA(I,KW,JP,LK(L))
           ENDDO
           ASHIL(JP)=D(K)*ASHIL(JP)
           IF(ASHIL(JP).LT.0.D0) ASHIL(JP)=0.D0
          ENDDO
        ENDIF
C 
C 
        DO JP=1,NPX
           IF(ASHIL(JP).LT.0.D0) ASHIL(JP)=0.D0
           ABSSU(I,KW,JP)=ASHIL(JP)
        ENDDO
        IF(CNZDEP().GT.2) THEN
           ABSSU(I,KW,3)=ASHIL(3)
           ABSSU(I,KW,4)=ASHIL(4)
        ENDIF
        RUU=RU(I,KW,K)
        IF(KENN.EQ.1) THEN
          RWW=TRRWR(ASHIL,RUU,KW,IRETR(K),K,ABLEI)
          DO JP=1,NPX
             RAS(I,KW,K,JP)=ABLEI(JP)
          ENDDO
        ELSE
          RWW=TRRWR(ASHIL,RUU,KW,IRETR(K),K)
        ENDIF
        RRR(I)=RWW
C
C
      ENDDO
C
      RETURN
      END 
C 
 
C 
C******************************************************
      SUBROUTINE MIXDC(NN,C,RDD,L,KW,K)

      USE MODMERZ
      USE MODFARB
      USE MODABST
      USE MODRWRZ
      USE MODGKWR,ONLY:CDE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA DELM/0.0000001/

C
C     ABLEITUNG DR/DC FUER LACKFARBEN,KUNSTSTOFFE,DRUCKFARBEN
C 
C
C
      DIMENSION RDD(*),C(*)
      INTEGER(KIND=4) :: CNZDEP
C
C     RAS           ABLEITUNGEN NACH ABSORPTION UND STREUUNG
C 
C
C
C
C
      CF=1.D-100
      DO I=1,NN
C
C        reine Farbmittelkonzentration (ohne Bindemittel und Zusatzstoffe)
C
C
C         IF(ICHF(LK(I)).NE.1.AND.ICHF(LK(I)).NE.8) THEN
         IF(ICHF(LK(I)).NE.1) THEN
           CF=CF+C(I)
         ENDIF
      END DO
      NWE=NWS()
      DO  I=1,NWE  
       IF(CNZDEP().EQ.1) THEN
C
C        'X' 'H' mit C
C
         DO J=1,NST(LK(L))+1
            TST(J)=ABSTA(I,KW,J,LK(L))
         ENDDO
         OM0=TST(NST(LK(L))+1)
         DCL=D(K)*C(L)
         TAA=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
         SUMM=D(K)*TAA*((1.-OM0)*RAS(I,KW,K,2)+OM0*RAS(I,KW,K,1))
       ELSEIF(CNZDEP().EQ.2) THEN
C
C        'Y' 'I' mit C
C
         DO J=1,2*NST(LK(L))
            TST(J)=ABSTA(I,KW,J,LK(L))
         ENDDO
         DCL=D(K)*C(L)
         DAA=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
         DSS=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST(NST(LK(L))+1))
         SUMM=D(K)*(DSS*RAS(I,KW,K,2)+DAA*RAS(I,KW,K,1))

       ELSEIF(CNZDEP().EQ.3) THEN
C
C        'X' 'H' mit CF
C

         DO J=1,NST(LK(L))+1
            TST(J)=ABSTA(I,KW,J,LK(L))
         ENDDO
         OM0=TST(NST(LK(L))+1)
         DCL=D(K)*CF
         TAU=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
C         IF(ICHF(LK(L)).EQ.1.OR.ICHF(LK(L)).EQ.8) THEN
         IF(ICHF(LK(L)).EQ.1) THEN
C
C          BINDEMITTEL
C
C
           TAA=TAU/CF
         ELSE
C
C

           TAA=D(K)*ABSSU(I,KW,4)+TAU/CF-ABSSU(I,KW,3)/CF
         ENDIF
         SUMM=TAA*((1.-OM0)*RAS(I,KW,K,2)+OM0*RAS(I,KW,K,1))
       ELSEIF(CNZDEP().EQ.4) THEN
C
C        'Y' 'I' mit CF
C
         DO J=1,2*NST(LK(L))
            TST(J)=ABSTA(I,KW,J,LK(L))
         ENDDO
         DCL=D(K)*CF
         AA=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
         SS=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST(NST(LK(L))+1))
C         IF(ICHF(LK(L)).EQ.1.OR.ICHF(LK(L)).EQ.8) THEN
         IF(ICHF(LK(L)).EQ.1) THEN
C
C          BINDEMITTEL
C
C
           DAA=AA/CF
           DSS=SS/CF
         ELSE
C
C
           DAA=D(K)*ABSSU(I,KW,3)+AA/CF-ABSSU(I,KW,1)/CF
           DSS=D(K)*ABSSU(I,KW,4)+SS/CF-ABSSU(I,KW,2)/CF
         ENDIF

         SUMM=DSS*RAS(I,KW,K,2)+DAA*RAS(I,KW,K,1)
       ELSEIF(CNZDEP().EQ.5) THEN
C
C        'X' 'H' mit CH
C

         DO J=1,NST(LK(L))+1
            TST(J)=ABSTA(I,KW,J,LK(L))
         ENDDO
         OM0=TST(NST(LK(L))+1)
C         IF(ICHF(LK(L)).EQ.1.OR.ICHF(LK(L)).EQ.8) THEN
         IF(ICHF(LK(L)).EQ.1) THEN
           CH=1.
         ELSE
           CH=C(L)/(C(L)+1.0-CF+DELM)
         ENDIF
         DCL=D(K)*CH
         TAU=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
C         IF(ICHF(LK(L)).EQ.1.OR.ICHF(LK(L)).EQ.8) THEN
         IF(ICHF(LK(L)).EQ.1) THEN
C
C          BINDEMITTEL ZUSATZSTOFF
C
C
           TAA=TAU+ABSSU(I,KW,3)-D(K)*ABSSU(I,KW,4)
         ELSE
C
C
           TAA=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
           DCHDC=(1.0-CH)**2/(1.-CF+DELM)
           TAA=(D(K)*C(L)*TAA*DCHDC+TAU*(1.0-C(L)*DCHDC/CH))/CH
         ENDIF
         SUMM=TAA*((1.-OM0)*RAS(I,KW,K,2)+OM0*RAS(I,KW,K,1))
       ELSEIF(CNZDEP().EQ.6) THEN
C
C        'Y' 'I' mit CH
C
         DO J=1,2*NST(LK(L))
            TST(J)=ABSTA(I,KW,J,LK(L))
         ENDDO
C         IF(ICHF(LK(L)).EQ.1.OR.ICHF(LK(L)).EQ.8) THEN
         IF(ICHF(LK(L)).EQ.1) THEN
           CH=1.
         ELSE
           CH=C(L)/(C(L)+1.0-CF+DELM)
         ENDIF
         DCL=D(K)*CH
         AA=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
         SS=STUINT(DCL,NST(LK(L)),CST(1,LK(L),KW),TST(NST(LK(L))+1))

C         IF(ICHF(LK(L)).EQ.1.OR.ICHF(LK(L)).EQ.8) THEN
         IF(ICHF(LK(L)).EQ.1) THEN

C
C          BINDEMITTEL
C
C
           DAA=AA+ABSSU(I,KW,3)-D(K)*ABSSU(I,KW,5)
           DSS=SS+ABSSU(I,KW,4)-D(K)*ABSSU(I,KW,6)
         ELSE
C
C
           DAA=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST)
           DCHDC=(1.0-CH)**2/(1.-CF+DELM)
           DAA=(D(K)*C(L)*DAA*DCHDC+AA*(1.0-C(L)*DCHDC/CH))/CH
           DSS=STUINA(DCL,NST(LK(L)),CST(1,LK(L),KW),TST(NST(LK(L))+1))
           DSS=(D(K)*C(L)*DSS*DCHDC+SS*(1.0-C(L)*DCHDC/CH))/CH
         ENDIF

         SUMM=DSS*RAS(I,KW,K,2)+DAA*RAS(I,KW,K,1)





       ELSE
         SUMM=0.
         DO JP=1,NPX
            SUMM=SUMM+RAS(I,KW,K,JP)*ABSTA(I,KW,JP,LK(L))
         ENDDO
         SUMM=D(K)*SUMM
       ENDIF
       RDD(I)=SUMM
      ENDDO    
      RETURN
      END
C******************************************************
C******************************************************************************
      SUBROUTINE BEAUSO(KF,LQ,C,KZAL,RZNR,RZKF,RZACONZ,RZPRE,RZLK,RZDEG)
      USE MODMERZ
      USE MODRWRZ
      USE MODFARB
      USE MODSORT


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
C 

C
      INTEGER(KIND=4) :: RZKF,RZNR
      REAL(KIND=8),DIMENSION(*) :: RZACONZ,RZPRE,RZDEG
      INTEGER(KIND=4),DIMENSION(*) :: RZLK
      DIMENSION C(*),LQ(*)
C        
C
      RZKF=KF
C
C
      DO  I=1,KF
        RZACONZ(I)=C(I)
        RZLK(I)=LQ(I)
        RZPRE(I)=PRE(LQ(I))
      ENDDO       
C
C
      DO JE=1,16
         RZDEG(JE)=DEG(JE)
      ENDDO
C
      RZNR=KZAL
C
C
      RETURN
      END

C******************************************************
C
C     
C******************************************************************************
      SUBROUTINE BEASOP(JABST,KF,LQ,C,PRE,NWE,RV,RR,KGX,
     &                  JUV,JUN,JUU,K,IRT,KWI,KORR,SENS,DVV,DEG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
      DIMENSION C(*),LQ(*),PRE(*),DEG(*),XYZV(3),XYZN(3)
      INTEGER(KIND=4) :: JABST,KF,KGX,JUV,JUN,JUU,K,IRT,KWI
      DIMENSION RR(NWE,*),RV(NWE,*)
      REAL(KIND=8) :: KORR,SENS,EPS
      DATA EPS/1.E-30/
C
C     DEG(1)   Metamerie
C     DEG(2)   Preis
C     DEG(3)   Korrigierbarkeit
C     DEG(4)   Kontrast DE*
C     DEG(5)   DE* Normlichtart 1
C     DEG(6)   DE* Normlichtart 2
C     DEG(7)   DE* Normlichtart 3
C     DEG(8)   DE* Normlichtart 4
C     DEG(9)   DE* Normlichtart 5
C     DEG(10)  Summe DE* alle Normlichtarten
C     DEG(11)  Flächendifferenz R-Kurven
C     DEG(12)  Sensibilität
C     DEG(13)  EXP(-Korrigierbarkeit)
C     DEG(14)  Standardabweichung KWI-ter Winkel (Messgeometrie)
C     DEG(15)  Mittelwert Farbabstand erste Lichtart über alle Winkel
C     DEG(16)  Mittelwert (Farbabstand) über alle Winkel und alle Lichtarten
C
C
C
      DO I=1,16
         DEG(I)=-999.
      END DO
C
C        
C

C     KORRIGIERBARKEIT
C
      
      DEG(3)=-LOG(ABS(KORR)+EPS)
  
C
C     PREIS FUER REZEPT
C
C
      DEG(2)=PREGES(KF,LQ,C,PRE)
C
C
      IF(KGX.EQ.1) THEN
C
C     FARBABSTAND DER SCHICHT UEBER WEISSEM UND SCHWARZEM UNTERGRUND
             DEG(4)=DVV
      ELSE

             DEG(4)=-1.
      ENDIF

      IF(K.GT.JUV) THEN
        RETURN
      ENDIF
C
C
C     FARBABSTAENDE
C
C
C     FARBABSTAENDE (ERSTE NORMLICHTART UND ERSTER UNTERGRUND)
C

C
C
C
      ASUM=0.
      DO JE=1,NLS()
C
         CALL FADIFF(JABST,KWI,IRT,NWE,RV,RR,DE,DL,DC,DH,DA,DB,
     &               AMA,1,JE)
         DEG(4+JE)=DE             
C
         ASUM=ASUM+DE**2
C
C        METAMERIE
C
         IF(JE.EQ.2) DEG(1)=AMA
C
      ENDDO
C
      DEG(10)=SQRT(ASUM)/FLOAT(NLS())
C
      DEG(11)=FLDIF(KWI,NWE,RV,RR)
C
C
C     SENSITIVITÄT
C
C
C
      DEG(12)=-LOG(ABS(SENS)+EPS)
      DEG(13)=KORR
C
C
C     Standardabweichung KWI-ter Winkel(Messgeometrie) DEG(14)
C
      SUMM=0.0
      IF(KWI.LE.0.OR.KWI.GT.KMS()) THEN
        DO KW=1,KMS()
          DO I=1,NWE
            SUMM=SUMM+(RV(I,KW)-RR(I,KW))**2
          END DO
        END DO
        SUMM=(NWE-1)*SUMM/(KMS()*NWE-1)
      ELSE
        DO I=1,NWE
          SUMM=SUMM+(RV(I,KWI)-RR(I,KWI))**2
        END DO
      ENDIF
      DEG(14)=SQRT(SUMM/(NWE-1))
C
C
C
C     Mittelwert Farbabstand über alle Winkel bei erster Lichtart
C
C
      SUMMA=0.0
      SUMM=0.0
      DO JE=1,NLS()
        DO KW=1,KMS()
           GKK=GLZNOG(KW,IRT)
           CALL NOGXX(XYZV,JE,RV(1,KW),GKK)
           CALL NOGXX(XYZN,JE,RR(1,KW),GKK)
           CALL DELABAL(JABST,XYZV,XYZN,DES,DLS,DCS,DHS,DAS,DBS,JE)
           IF (JE.EQ.1) THEN
              SUMM=SUMM+DES**2
           END IF
           SUMMA=SUMMA+DES**2
        END DO
      ENDDO
      DEG(15)=SQRT(SUMM/KMS())
      DEG(16)=SQRT(SUMMA/(KMS()*NLS()))
      RETURN
      END

C******************************************************************************
      DOUBLE PRECISION FUNCTION APAFA(KF,DEG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DEG(*)
C
C 
C 
C 
C 
C 

      APAFA=DEG(16)
      RETURN
      END
C
C 
    
C******************************************************
C******************************************************************************
C 

