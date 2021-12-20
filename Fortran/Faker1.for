C     Last change: KU 26.04.2020 09:31:25
C
C******************************************************
C******************************************************************************
C******************************************************************************
C
C******************************************************
C 
C******************************************************************************
C******************************************************************************
      SUBROUTINE REZMSH(MNF,LID,SOMNG,NWEL,KML,RCAL,SOKRIT,IER)
      USE MODRWRZ
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODGREN
      USE MODSORT
      USE MODABST
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
C 
C 
C 
C     BERECHNUNG VON REFLEXIONSWERTEN AUS FARB-BINDEMITTELMENGEN FUER DECKENDE, OPAKE UND
C     TRANSPARENTE FARBMITTELSCHICHTEN
C 
C 
C 
C 
C 
C 
C 
C 
      INTEGER(KIND=4) :: MNF,NWEL,KML
      REAL(KIND=4), DIMENSION(NWEL,KML,*) :: RCAL
      INTEGER(KIND=4),DIMENSION(*)::LID
      REAL(KIND=4),DIMENSION(16,*):: SOKRIT
      REAL(KIND=4),DIMENSION(*)::SOMNG
      DIMENSION C(MNF)
      REAL(KIND=8) :: KORR,SENS

C 
C 
C
      IF(MNF.EQ.0) THEN
        IER=4014
        RETURN
      ENDIF

C 
C 
C 
      IF(JUU.EQ.0.AND.JUV.EQ.0) THEN
        JUU=1
        CALL NULLRWRT (NWEL,KML,JUU,RU)
      ENDIF
C
C
C
C 
C
C
C 
      IER=0
      KM=KMS()
      NWE=NWS()
      NF=MNF
      DO K=1,UBOUND(D,1)
        DO I=1,16
         SOKRIT(I,K)=0.
        ENDDO
      ENDDO
C 
      CSUM=EPL
      DO I=1,NF
         LK(I)=LID(I)
         ACONZ(I)=SOMNG(I)/ANORM
         CSUM=CSUM+ACONZ(I)
      ENDDO
      IF(CSUM.LE.2.*EPL) THEN
          CSUM=1.
      ENDIF
      DO I=1,NF
         C(I)=ACONZ(I)/CSUM
      ENDDO
C
C
      DO J=1,NF
         IF(ABS(C(J)).LT.EPL) THEN
            C(J)=0.
         ENDIF
      ENDDO
      DVV=-1
      DO K=1,MAX(JUU,JUV)
        DO KW=1,KM
           CALL MALRW(NF,C,RR(1,KW,K),KW,K)
           KK=K
           IF(JUU.EQ.2.AND.JUN.EQ.1) THEN
              KK=1
           ENDIF
           CALL MAKOR(NF,C,RR(1,KW,K),KW,KK)
           DO I=1,NWE
              RCAL(I,KW,K)=RR(I,KW,K)
           ENDDO
           GKK=GLZNOG(KW,IRETR(K))
           CALL NOGXX(XYZB(1,1,KW,K),1,RR(1,KW,K),GKK)
         ENDDO
         IF (K.EQ.2) THEN
C
C              Kontrast DE
C
               CALL DELABAL(JABST,XYZB(1,1,1,1),XYZB(1,1,1,2),DVV,
     &                         DL,DC,DH,DA,DB,1)
          END IF
      ENDDO
      DO K=1,MAX(JUU,JUV)
         CALL REDETR(KORR,SENS,NF,C)
         CALL BEASOP(JABST,NF,LK,C,PRE,NWE,RV(1,1,K),RR(1,1,K),KGX,
     &                  JUV,JUN,JUU,K,IRETR(K),KWC,KORR,SENS,DVV,DEG)
C
C
         DO I=1,16
            SOKRIT(I,K)=DEG(I)
         ENDDO
      ENDDO
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
      SUBROUTINE REZGRND(IPRN,MNF,LID,SOMNG,NWEL,KML,NSTN,CSTN,
     &                   GRUGES,IER)
      USE MODRWRZ
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODGREN
      USE MODABST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
C 
C
C 
C     BERECHNUNG VON Grunddaten AUS FARB-BINDEMITTELMENGEN FUER DECKENDE, OPAKE UND
C     TRANSPARENTE FARBMITTELSCHICHTEN
C 
C 
C 
C 
C 
C 
C 
C 
      INTEGER(KIND=4) ::NSTN
      REAL(KIND=4),DIMENSION(KML,*)::CSTN
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: GRUGES
      INTEGER (KIND=4) :: IPRN
      INTEGER(KIND=4) :: CNZDEP
      INTEGER(KIND=4),DIMENSION(*)::LID
      REAL(KIND=4),DIMENSION(*)::SOMNG
      DIMENSION C(MNF),RHILF(NWEL)

C 
C 
C
C 
C 
      IF(IPRN.EQ.0) THEN
        RETURN
      ENDIF
      IF(MNF.EQ.0) THEN
        IER=4014
        RETURN
      ENDIF

C 
C 
C 
      IF(JUU.EQ.0.AND.JUV.EQ.0) THEN
        JUU=1
        CALL NULLRWRT (NWEL,KML,JUU,RU)
      ENDIF
C
C
C
C 
C
C
C 
      IER=0
      KM=KMS()
      NWE=NWS()
      NF=MNF
C
      CSUM=EPL
      DO I=1,NF
         LK(I)=LID(I)
         CSUM=CSUM+SOMNG(I)
      ENDDO
      DO I=1,NF
         C(I)=SOMNG(I)/CSUM
      ENDDO
      CSUM=1.0
C
C
C
C
C     Falls IPRN=2 werden die Farbmittel mit OP='=' nicht zur Berechnung der Grunddaten verwendet.
C     damit können die optischen Konstanten (Absorption und Streuung) auch für Teile einer Rezeptmischung berechnet werden
C     (z.B. falls Restfarben mit Weißpigment gemischt wurden)
C
      IF(IPRN.EQ.2) THEN
        NF=0
        CSUM=EPL
        DO J=1,MNF
          IF(OP(LK(J)).NE.'=') THEN
             NF=NF+1
             LK(NF)=LK(J)
             C(NF)=C(J)
             CSUM=CSUM+C(NF)
          ENDIF
        END DO
      ENDIF
C 
C 
      NPX=NOPNPX()
      NSTN=0
C 
C
C 
C
      DO K=1,MAX(JUU,JUV)
        DO KW=1,KM
           CALL MALRW(NF,C,RHILF,KW,K)
           IF(K.EQ.1) THEN
           IF(MOD(CNZDEP(),2).EQ.1) THEN
C
C                 konz.abhaengige Grunddaten
C
C
C
C                 FÜR EINE STÜTZSTELLE WERDEN DIE GRUNDDATEN BERECHNET
C         
                  NSTN=1
                  CSTN(KW,1)=CCST(1,1,1.D0,0.1D0,1.D0)
                  DO I=1,NWE
                     GRUGES(I,KW,1)=(ABSSU(I,KW,1)+ABSSU(I,KW,2))
     &                /(D(K)*CSUM)
                  ENDDO
C
C
C                 Die letzten Grunddaten sind A/(A+S) = 1.-Albedo
C
                  DO I=1,NWE
                     GRUGES(I,KW,2)=ABSSU(I,KW,1)
     &               /(ABSSU(I,KW,1)+ABSSU(I,KW,2))
                  ENDDO
C
           ELSEIF(MOD(CNZDEP(),2).EQ.0) THEN
C
C                 konz.abhaengige Grunddaten
C
C
C
C                 FÜR EINE STÜTZSTELLE WERDEN DIE GRUNDDATEN BERECHNET
C         
                  NSTN=1
                  CSTN(KW,1)=CCST(1,1,1.D0,0.1D0,1.D0)
                  DO I=1,NWE
                     GRUGES(I,KW,1)=ABSSU(I,KW,1)/(D(K)*CSUM)
                     GRUGES(I,KW,2)=ABSSU(I,KW,2)/(D(K)*CSUM)
                  ENDDO
C
C
C
C
             
             ELSE
                  DO JP=1,NPX
                  DO I=1,NWE
                     GRUGES(I,KW,JP)=ABSSU(I,KW,JP)/(D(K)*CSUM)
                  ENDDO
                  ENDDO
             ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C
C
C


C
      RETURN
      END

C
C
      SUBROUTINE RKDEST(IER)          
      USE MODRWRZ
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODGREN
      USE MODABST

C 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
C
C 
C 
C 
      IBI=IBIN(NF,ICHF)
      KM=KMS()

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
C     PRUEFEN, OB DECKVERMOEGEN BERECHNET WERDEN KANN
C
C
C 
      CALL PRUDEK(NF,IER)

C
C     
C
C
C
C     BERECHNUNG DER R-WERTE (VORLAGE) FUER DIE VORGEGEBENEN KONZENTRATIONEN
C
C 
C 
C 
C     
      DO  K=1,JUV
C 
C 
        DO J=1,JN 
C
C
C
          DO KW=1,KM
C
C
C
C
C
C
C           TRISTIMULUSWERTE DER VORLAGE FUER NORMLICHTARTEN (GEMESSEN)
C
            GKK=GLZNOG(KW,IRETR(K))
            CALL NOGXX(XYZV(1,J,KW,K),J,RV(1,KW,K),GKK)
C
C
C
C           SIN UND COS DES WINKELS ZWISCHEN A* UND VEKTOR DER VORLAGE IN A*,B*-EBENE
C
C
C
            CALL ABWIN(JABST,XYZV(1,J,KW,K),SPHI(J,KW,K),CPHI(J,KW,K),J)
C
          ENDDO
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
      CALL RKSTR(IER) 
C 
C 
C
C 
C 
C 
C 
 900  RETURN
      END

C******************************************************
C

C
      SUBROUTINE RKSTR(IER)          
      USE MODRWRZ
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODABST

C 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
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
C
C     BERECHNUNG DER R-WERTE (NACHSTELLUNG) FUER DIE VORGEGEBENEN KONZENTRATIONEN
C
C 
C 
C
C 
C 
C     
      KM=KMS()

      DO  K=1,JUN
C 
        DO KW=1,KM
          CALL MALRW(NF,CONA,RY(1,KW,K),KW,K)
        ENDDO
C 
        DO J=1,JN
C
C
C
          DO KW=1,KM
C
C
C
C
C
C
C
C           TRISTIMULUSWERTE DER NACHST. FUER NORMLICHTARTEN (GEMESSEN)
C
            GKK=GLZNOG(KW,IRETR(K))
            CALL NOGXX(XYZN(1,J,KW,K),J,RN(1,KW,K),GKK)
C
C
C
C            TRISTIMULUSWERTE DER NACHST. FUER NORMLICHTARTEN (BERECHNET)_
C 
             CALL NOGXX(XYZB(1,J,KW,K),J,RY(1,KW,K),GKK)
C
          ENDDO
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
 900  RETURN
      END

C******************************************************
C******************************************************************************
C******************************************************
C******************************************************
      SUBROUTINE  MINFA(KFIT,IER,DETR)
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODGREN
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8) :: KORR,SENS,DETR,FZUWIT,FUNGGE
      EXTERNAL FZUWIT,FUNGGE
C
C
      DATA MAXFUN/50/
C

      SELECT CASE (IGX)
          CASE (5)
C
C
C           Farbabstand DTO wird durch Änderung der Zuwaage iterativ eingestellt
C           (Maximalmenge s. INO  wird nicht überschritten)
C
            KFJT=KFIT
            ZUMIN=0.0D0
            ZUMAX=1.0D0
            DTOU=FZUWIT(ZUMIN)
            IF(DTOU.GT.0.0) THEN
               GOTO 500
            ENDIF
            DTOO=FZUWIT(ZUMAX)
            IF(DTOO.GT.0.0) THEN
               ZUWI=0.5*(ZUMIN+ZUMAX)
               CALL INTVAL(ZUWI,ZUMIN,ZUMAX,FZUWIT,IER)
            ENDIF
 500        ZUWIT=1.0
          CASE (6)
C
C
C           Farbabstand DTO wird durch Minimierung der Mengenänderungen eingestellt
C           (Maximalmenge s. INO  wird nicht überschritten)
C
            KFJT=KFIT
            GGEMIN=0.0D0
            GGEMAX=GGEM
            DTOU=FUNGGE(GGEMIN)
            IF(DTOU.GT.0.0) THEN
             GGE=GGEMAX
            ELSE
             DTOO=FUNGGE(GGEMAX)
             IF(DTOO.GT.0.0) THEN
               GGEL=0.5*(GGEMIN+GGEMAX)
               CALL INTVAL(GGEL,GGEMIN,GGEMAX,FUNGGE,IER)
             ENDIF
            ENDIF
          CASE DEFAULT
c            KFJT=2
c            CALL REZFIT(KFJT,NF,ITM,MAXFUN,ATO,
c     &                 ACONZ,PHI,IER)
            KFJT=KFIT
            CALL REZFIT(KFJT,NF,ITM,MAXFUN,ATO,
     &                 ACONZ,PHI,IER)
      END SELECT

      IF(IFEHL(IER).NE.0) THEN
         DETR=1.
         GOTO 900
      ENDIF
C
      CALL REDETR(KORR,SENS,NF,ACONZ)


      DETR=KORR
 900  ZUWIT=0.0
      RETURN
      END
c
c

C 
C******************************************************
C******************************************************
C******************************************************
C******************************************************
C******************************************************
C******************************************************
C******************************************************
C******************************************************
C
      SUBROUTINE REZSTR(IER)
      USE MODRWRZ
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODGREN
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
C
C 
C 
C
C
C
C
      IBI=IBIN(NF,ICHF)
      KM=KMS()
C
C
C     PRUEFEN, OB DECKVERMOEGEN BERECHNET WERDEN KANN
C
      CALL PRUDEK(NF,IER)

C 
C 
C
C
C 
C 
C     
      DO 60 K=1,JUV
C 
C 
      DO 65 J=1,JN 
C
C
C
C
C
C     TRISTIMULUSWERTE DER VORLAGE FUER NORMLICHTARTEN (GEMESSEN)
C
C
      DO KW=1,KM
C
C
C
          GKK=GLZNOG(KW,IRETR(K))
          CALL NOGXX(XYZV(1,J,KW,K),J,RV(1,KW,K),GKK)
C
C
C
C         SIN UND COS DES WINKELS ZWISCHEN A* UND VEKTOR DER VORLAGE IN A*,B*-EBENE
C
C
C
          CALL ABWIN(JABST,XYZV(1,J,KW,K),SPHI(J,KW,K),CPHI(J,KW,K),J)
C
C     
C 
C
C     
C 
      ENDDO
C
C
C     
C 
C 
 
  65  CONTINUE
  60  CONTINUE
C 
C
C
C
C 
C 
  900 RETURN
      END
C
C******************************************************************************
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
      SUBROUTINE REZBAS(KFIT,MNF,SOMNG,NWEL,KML,RDAB,SOKRIT,IER)
      USE MODRWRZ
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODGREN
      USE MODSORT
      USE MODABST
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     KFIT=1 Farbwerteangleich (Rezfit)
C     KFIT=2 Reflexionswerteangleich (Rezfit)
C
C 
C 
C 
C     BERECHNUNG VON FARBREZEPTEN FUER DECKENDE, OPAKE UND
C     TRANSPARENTE FARBMITTELSCHICHTEN
C 
C 
C 
C 
C 
C 
C 
C 
      INTEGER*4 NWEL,KML,KFIT,MNF
      REAL(KIND=4),DIMENSION(*):: SOMNG
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAB
      REAL(KIND=4),DIMENSION(16,*) :: SOKRIT
      REAL(KIND=8) ::KORR,SENS
      REAL(KIND=8),DIMENSION(MNF)::C
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
      DATA MAXFUN/50/
C 
C 
C 
      IER=0
      KM=KML
      NWE=NWEL
      NF=MNF
      IKOR=0

C 
C 
C
C
C
C     ACONZ=Effektive Menge /z.B. kann EFF=spez. Gewicht sein, dann wurden die optischen Daten für Volumina berechnet
      DO I=1,NF
         ACONZ(I)=SOMNG(I)/ANORM
         ACZWI(I)=ACONZ(I)
      END DO

C
C
C 
C 
C 
C 
      ITA=15

C
C
C     ELIMINIEREN VON '-'-ZEICHEN
C
C
C    
      II=0
      DO 101 I=1,NF
      IF(KTO(I).NE.'-') THEN
             II=II+1
             LK(II)=LK(I) 
             ACONZ(II)=ACONZ(I)
      ENDIF
 101  CONTINUE
      KF=II
C                                 
      ITT=4*ITA
      CALL REZFIT(KFIT,KF,ITT,MAXFUN,ATO,
     &                 ACONZ,PHI,IER)
C
C
      IF(IFEHL(IER).NE.0) GOTO 900
C


      DO 102 II=KF,1,-1
      ACONZ(LK(II))=ACONZ(II)
 102  CONTINUE     
      DO 103 I=1,NF
      LK(I)=I
      IF(KTO(I).EQ.'-') ACONZ(I)=0.
 103  CONTINUE
      KOMB=0
      SUU=0.
      DO J=1,NF
         IF(ABS(ACONZ(J)).LT.EPL) THEN
            ACONZ(J)=0.
         ENDIF
         AMG=ACONZ(J)*ANORM
         IF(AMG.GT.AMGGRZ) THEN
            IER=-4124
c            GOTO 900
         ENDIF
         SOMNG(J)=AMG
         SUU=SUU+ACONZ(J)
      ENDDO
      DO J=1,NF
        C(J)=ACONZ(J)/SUU
      END DO
      DO K=1,MAX0(JUU,JUV)
        DO KW=1,KM
           DO I=1,NWE
              RDAB(I,KW,K)=RR(I,KW,K)
           ENDDO
        ENDDO
        CALL REDETR(KORR,SENS,NF,C)
        CALL BEASOP(JABST,NF,LK,C,PRE,NWE,RV(1,1,K),RR(1,1,K),KGX,
     &              JUV,JUN,JUU,K,IRETR(K),KWC,KORR,SENS,DVV,DEG)
c
C
C 
        DO I=1,16
            SOKRIT(I,K)=DEG(I)
        ENDDO


      ENDDO
C
 900  RETURN
      END



C
CC
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C
C***************************************************************************

       SUBROUTINE REZKOM(KFIT,MNF,SOMNG,
     &            KZA,KFM,NFR,LKID,REMNG,IER)
      USE MODRWRZ
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODGREN
      USE MODSORT
      USE MODABST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

C
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:) :: RZDEG
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: RZACONZ,RZPRE
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:,:) :: RZLK
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: RZKF,RZNR
C
      INTEGER(KIND=4) :: KFIT,MNF,KZA,KRR,JTOF
      INTEGER(KIND=4),DIMENSION(KZA+1) :: NFR
      REAL(KIND=4),DIMENSION(KFM,KZA+1)::REMNG
      REAL(KIND=4),DIMENSION(MNF)::SOMNG
      INTEGER(KIND=4),DIMENSION(KFM,KZA+1)::LKID
      REAL(KIND=8) :: KORR,SENS,SUU,EPPS,EPS
C 
C 
C 
C 
C 
      DIMENSION LQ(MNF),CQ(MNF),ACOO(MNF)
      DIMENSION LRS(MNF),CLRS(MNF)
      REAL(KIND=8),DIMENSION(16) ::DEGHILF
      LOGICAL LOPT,LUPT,IMSVX,LEPT
      LOGICAL LLRS
      CHARACTER(LEN=1) CIJK
      LOGICAL(KIND=4) :: ENTHAL
C 
C
C 
C
      CHARACTER*1 BLA
C
C
      DATA MAXFUN/50/,EPS/1.D-7/
C 
C 
      DATA LOPT/.TRUE./,LUPT/.FALSE./,LEPT/.FALSE./
C
C
C     LEPT=.TRUE. es werden u.U. schlechte Rezepte aussortirt
C 
C 
C 
      DATA BLA/' '/             
C 
C
C
      IER=0
      EPPS=EPMNG/(ANORM+TINY(1.))
      IF(MNF.EQ.0) THEN
        IER=4014
        RETURN
      ENDIF
C
C
C
C     ACONZ effektive Mengen
C
      NF=MNF
      DO I=1,NF
        ACONZ(I)=SOMNG(I)/ANORM
        ACOO(I)=ACONZ(I)
      ENDDO
C
C
C
C
C 
C     PRUEFEN, OB NICHT VERWENDETE FARBMITTEL ZU GUTEM REZEPT FUEHREN
C
C 
C      WRITE(27,*) 'REZKOM Start'
C 
C 
C 
C 
C 
      ITA=2*ITM
C
C
C
      KQ=NF
      DO I=1,KQ
        LQ(I)=LK(I)
        CQ(I)=ACONZ(I)
      END DO
      II=0
      DO I=1,KQ
        ICC=ICHAR(KTO(LQ(I)))
        IF((ICC.GE.65.AND.ICC.LE.90)
     &    .OR.LQ(I).EQ.IBWN(NF,ICHF).OR.LQ(I).EQ.IBBN(NF,ICHF)
     &    .OR.LQ(I).EQ.IBIN(NF,ICHF).OR.CQ(I).LT.EPS) THEN
                II=II+1
                LK(II)=LQ(I)
                ACONZ(II)=CQ(I)+SMENG
        ENDIF
      ENDDO
      LLRS=.FALSE.
      KF=II
C
c      WRITE(27,*)'Halt1'
C
C                                      
      IF(KF.LE.1) GOTO 107
C                                      
C                                        
C
C                               
      IF(LEPT) THEN
        KFJT=KFIT/5
        CALL REZFIT(KFJT,KF,ITA,MAXFUN,ATO,
     &                 ACONZ,PHI,IER)
        KFJT=MOD(KFIT,5)
        CALL REZFIT(KFJT,KF,ITA,MAXFUN,ATO,
     &                 ACONZ,PHI,IER)
C
C 
C
C                                       
C                                  
        KORR=1.
        SENS=1.
        CALL BEASOP(JABST,KF,LK,ACONZ,PRE,NWS(),RV,RR,KGX,
     &              JUV,JUN,JUU,1,IRETR(1),KWE,KORR,SENS,DVV,DEG)

C
C                                    
C
C                             
C                                      
C       SCHLECHTE FARBMITTEL-KOMBINATION WIRD ZWISCHENGESPEICHERT
C
C
C
C
C
        IF(DEG(5).GT.DEOK) THEN
            LLRS=.TRUE.
            KLRS=0
            DCNEU=APAFA(KF,DEG)
            DO I=1,KF
               ICC=ICHAR(KTO(LK(I)))
               IF(.NOT.((ICC.GE.65.AND.ICC.LE.90)
     &         .OR.LK(I).EQ.IBWN(NF,ICHF).OR.LK(I).EQ.IBBN(NF,ICHF)
     &         .OR.LK(I).EQ.IBI)) THEN
                   KLRS=KLRS+1
                   LRS(KLRS)=LK(I)
                   CLRS(KLRS)=ACONZ(I)
               ENDIF
            ENDDO
        ENDIF
C
C     BASISREZEPT WIRD ZURUECKGESPEICHERT
C
C
C 
      ENDIF
C
C
C
  107 KF=NF
      DO  I=1,NF
          LK(I)=LQ(I)
          ACONZ(I)=CQ(I)
      ENDDO
C
C 
C 
C 
C     MIN. ANZAHL FARBMITTEL PRO REZEPT: KMI
C 
C 
C     MAX. ANZAHL FARBMITTEL PRO REZEPT: KMA
C 
C 
C     
C
C     MAXIMALE ANZAHL ZU BERECHNENDER REZEPTE: KRE
C
C 
C 
      IF(KMA.GT.KFM) KMA=KFM
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
      IF(ALLOCATED(RZKF)) THEN
        DEALLOCATE(RZACONZ,RZPRE,RZLK,RZKF,RZNR,RZDEG)
      ENDIF
      KRR=MAX(KRE+1,KZA+1)
      ALLOCATE(RZACONZ(KFM,KRR),RZPRE(KFM,KRR),RZLK(KFM,KRR),
     &   RZKF(KRR),RZNR(KRR),RZDEG(16,2,KRR),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         RETURN
      ENDIF
C
C
C
C
C
C
      KZAL=0
      JTOF=0
      DO  J=1,NF
         IF(IBI.NE.0.AND.KTO(IBI).NE.'-') JTOF=IBSET(JTOF,0)
C
C        PRUEFEN AUF BUCHSTABEN (A,B,C,D,E,F,G)
C
C
C
         IJ=ICHAR(KTO(J))-64
         IF(IJ.GE.1.AND.IJ.LE.7) THEN
      	   JTOF=IBSET(JTOF,IJ)
         ENDIF
      ENDDO
C 
c      WRITE(27,*)'Halt2'

C
C 
C 
C     BEDEUTUNG VON KTO 
C 
C 
C     KTO  = BLANK  FARB/BINDEMITTEL WIRD IN DER GEGEBENEN KOMBINATION
C                   VERWENDET 
C     KTO  = ZIFFER FARB/BINDEMITTEL WIRD IN DER GEGEBENEN KOMBINATION
C                   VERWENDET, FALLS KEIN ANDERES FARB/BINDEMITTEL
C                   MIT DER GLEICHEN TOPFNUMMER VORHANDEN IST 
C     KTO  = BUCHST (A,B,C,D,E,F,G) GENAU EIN FARB/BINDEMITTEL MIT DIESER
C                   TOPFNUMMER MUSS IM VORGEGEBENEN REZEPT VORKOMMEN
C     KTO  = '-'    FARB/BINDEMITTEL WIRD NICHT VERWENDET
C
C     KTO   FARBMITTEL MIT KTO= MÜSSEN IMMER MIT MINDESTENS EINEM KTO=   KOMBINIERT WERDEN
C                             I                                      J
C                             M                                      N
C                             S                                      T
C                             V                                      W
C                             X                                      Y
C     KTO    FARBMITTEL MIT KTO=  DÜRFEN NICHT MIT EINEM KTO= KOMBINIERT WERDEN
C                              (                            )
C                              [                            ]
C                              {                            }
C                              <                            >
C                              /                            \
C
C
C
C     GROSSE SCHLEIFE 
C 
C 
C 
      KNI=MIN0(NF,KMI)
      KNA=MIN0(NF,KMA)
      KF=KNA
      IF(KF.GT.UBOUND(LKS,1)) THEN
        IER=4129
        GOTO 140
      ENDIF
      IF(KF.LE.0) THEN
        IER=4014
        GOTO 140
      ENDIF
      KOMB=0
C 
C
  112 DO 114 I=1,KF 
  114 LK(I)=I 
      GO TO 118 
  111 DO  J=1,KF
        KJ=KF+1-J
        IF(LK(KJ).LT.(NF+1-J)) GOTO 116
      ENDDO
      KF=KF-1
      IF (KF.GE.KNI) GOTO 112 
      IF(KOMB.EQ.0) THEN
                  KNI=KNI-1
                  IF(KNI.LE.0) GOTO 140
                  KF=KNI
                  GOTO 112
      ENDIF
      GOTO 140
  116 LK(KJ)=LK(KJ)+1 
      IF (KJ.EQ.KF) GO TO 118 
      KJ1=KJ+1
      DO I=KJ1,KF
         LK(I)=LK(I-1)+1
      ENDDO
C
C
  118 DO I=1,KF
        IF(KTO(LK(I)).EQ.'-') GOTO 111
      ENDDO
      MN1=KF-1
      IF(MN1.LE.0) GOTO 121 
      DO I=1,MN1
C
C         Gleichheit für H,I,...Z ist erlaubt
C
         IF(ICHAR(KTO(LK(I))).GE.72.AND.ICHAR(KTO(LK(I))).LE.90) THEN
           CYCLE
         ENDIF
      
         IW=I+1
         DO   K=IW,KF
          IF(KTO(LK(I)).EQ.KTO(LK(K)).AND.KTO(LK(I)).NE.BLA) GOTO 111
         ENDDO
      ENDDO
  121 IF(KF.LT.1) GO TO 111
C 
C     PRUEFEN, OB BINDEMITTEL VORHANDEN 
C
C
      IF(.NOT.BTEST(JTOF,0)) GOTO 124
      DO  J=1,KF
         IF(LK(J).EQ.IBI) GOTO 124
      ENDDO
      GOTO 111
C 
C 
C     BUCHSTABEN (A,B,C,D,E,F,G) ÜBERPRÜFEN
C
C
  124 DO 123 I=1,7
        IF(.NOT.BTEST(JTOF,I)) GOTO 123
        DO 122 J=1,KF
          IJ=ICHAR(KTO(LK(J)))-64
          IF(IJ.EQ.I) GOTO 123
  122   CONTINUE
        GOTO 111
  123 CONTINUE
C 
C 
C
C 
C     BUCHSTABEN (H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) ÜBERPRÜFEN
C
C
      DO K=1,19
       CIJK=CHAR(71+K)
       IMSVX=.TRUE.
       DO I=1,KF
        IF(KTO(LK(I)).EQ.CIJK) THEN
          IMSVX=.FALSE.
          DO J=I+1,KF
            IF(KTO(LK(J)).EQ.CIJK) THEN
                IMSVX=.TRUE.
                EXIT
            END IF
          END DO
          IF(J.LE.KF) THEN
             EXIT
          ENDIF
        ENDIF
       END DO
C
       IF(.NOT.IMSVX) GOTO 111
      END DO
C
C 
C     SONDERZEICHEN ((,) UND [,] UND {,} UND <,> UND /,\) ÜBERPRÜFEN
C
C
      IMSVX=.TRUE.
      DO I=1,KF
        IF(KTO(LK(I)).EQ.'(') THEN
          IMSVX=.TRUE.
          DO J=1,KF
            IF(KTO(LK(J)).EQ.')') THEN
                IMSVX=.FALSE.
                EXIT
            END IF
          END DO
        ENDIF
      END DO
C 
      IF(.NOT.IMSVX) GOTO 111
C 
      DO I=1,KF
        IF(KTO(LK(I)).EQ.'[') THEN
          IMSVX=.TRUE.
          DO J=1,KF
            IF(KTO(LK(J)).EQ.']') THEN
                IMSVX=.FALSE.
                EXIT
            END IF
          END DO
        ENDIF
      END DO
C 
      IF(.NOT.IMSVX) GOTO 111
C
      DO I=1,KF
        IF(KTO(LK(I)).EQ.'{') THEN
          IMSVX=.TRUE.
          DO J=1,KF
            IF (KTO(LK(J)).EQ.'}') THEN
                IMSVX=.FALSE.
                EXIT
            END IF
          END DO
        ENDIF
      END DO
C 
      IF(.NOT.IMSVX) GOTO 111
C
      DO I=1,KF
        IF(KTO(LK(I)).EQ.'<') THEN
          IMSVX=.TRUE.
          DO J=1,KF
            IF (KTO(LK(J)).EQ.'>') THEN
                IMSVX=.FALSE.
                EXIT
            END IF
          END DO
        ENDIF
      END DO
C 
      IF(.NOT.IMSVX) GOTO 111
C
      DO I=1,KF
        IF(KTO(LK(I)).EQ.'/') THEN
          IMSVX=.TRUE.
          DO J=1,KF
            IF (KTO(LK(J)).EQ.'\') THEN
                IMSVX=.FALSE.
                EXIT
            END IF
          END DO
        ENDIF
      END DO
C 
      IF(.NOT.IMSVX) GOTO 111

      DO I=1,KF
        IF(KTO(LK(I)).EQ.'/') THEN
          IMSVX=.TRUE.
          DO J=1,KF
            IF (KTO(LK(J)).EQ.'\') THEN
                IMSVX=.FALSE.
                EXIT
            END IF
          END DO
        ENDIF
      END DO
C 
      IF(.NOT.IMSVX) GOTO 111



C
C
C
C
C     ABFRAGEN OB FARBSTOFFAUSWAHL SCHLECHT
C
C
C
      IF(LLRS.AND.LOPT) THEN
               DO 161 LA=1,KF
               DO 162 LB=1,KLRS
               IF(LK(LA).EQ.LRS(LB)) GOTO 165
  162          CONTINUE
               GOTO 160
  165          CQ(LA)=CLRS(LB)
               LQ(LA)=LK(LA)
  161          CONTINUE
C
C
C      REZEPT IST SCHLECHT
C
C
               KQ=KF
               GOTO 135
      ENDIF
C

C 
C 
 160  IF(LUPT) THEN

C     
C
C
C       ABFRAGEN, OB FARBSTOFFAUSWAHL BEREITS VORHANDEN
C                                    
C                            
C
C

        IF(ENTHAL(MNF,KFM,KF,LK,ACONZ,KZAL,RZKF,RZLK,RZACONZ,EPPS))THEN
          GOTO 135
        ENDIF
          
c        DO 186 I=1,KZAL
c        KG=RZKF(I)
c        DO J=1,KF
c           LTREFF(J)=0
c        ENDDO
c        DO 187 LB=1,KG
c        DO 188 LA=1,KF
c        IF(LK(LA).EQ.RZLK(LB,I)) THEN
c           LTREFF(LA)=1
c           GOTO 187
c        ENDIF
c 188    CONTINUE
C      
C       LK NICHT GEFUNDEN
C
c 187    CONTINUE
C
C       ALLE LK MIT ACONZ UNGLEICH NULL GEFUNDEN
C
C
c       DO J=1,KF
c           IF(LTREFF(J).EQ.0) GOTO 189
c        ENDDO
c        GOTO 135
C
C
C                                               
C
c 186    CONTINUE
      ENDIF
C

C
C 
C     BERECHNUNG DER FARBMITTELMENGEN (NORMIERT) 
C 
C 
C 
C
C

 189  ITZ=2+KF
      MAXF=6
      SUU=0.
      DO I=1,KF
        ACONZ(I)=ACOO(LK(I))
        SUU=SUU+ACONZ(I)
      END DO
      IF(SUU.LT.100*EPS) THEN
           II=IBIND(KF,LK,ICHF)
           IF(II.NE.0) THEN
              ACONZ(II)=1.
           ELSE
             II=IWEIS(KF,LK,ICHF)
             IF(II.NE.0) THEN
                ACONZ(II)=1.
             ELSE
                II=IMETL(KF,LK,ICHF)
                IF(II.NE.0) THEN
                   ACONZ(II)=1.
                ENDIF
              ENDIF
           ENDIF
      ENDIF
      KFJT=KFIT/5
      CALL REZFIT(KFJT,KF,ITZ,MAXF,ATO,
     &                 ACONZ,PHI,IER)
      KFJT=MOD(KFIT,5)
      CALL REZFIT(KFJT,KF,ITZ,MAXF,ATO,
     &                 ACONZ,PHI,IER)
      IF(IER.NE.0) THEN
        GOTO 135
      ENDIF

      KOMB=KOMB+1
C     
C                                        
C                                                    
C
C
C     PRUEFEN AUF NULLMENGE UND ZURUECKSORTIEREN BEI BEREITS ZWISCHEN-
C     GESPEICHERTEN FARB-/BINDEMITTEL
C
C 
      IF(LOPT) THEN
C
C
C
C       ABFRAGEN, OB FARBSTOFFAUSWAHL BEREITS VORHANDEN
C                                    
C                            
C
C
        IF(ENTHAL(MNF,KFM,KF,LK,ACONZ,KZAL,RZKF,RZLK,RZACONZ,EPPS))THEN
          GOTO 135
        ENDIF
          
c
      ENDIF
C 
C 
c 
C 
C 
C 
  151 KORR=1.
      SENS=1.
      CALL BEASOP(JABST,KF,LK,ACONZ,PRE,NWS(),RV,RR,KGX,
     &            JUV,JUN,JUU,1,IRETR(1),KWE,KORR,SENS,DVV,DEG)
C                                                         
      DCNEU=APAFA(KF,DEG)     
C 
C     REZEPT ZWISCHENSPEICHERN
C                                      

C                                
C 
C
      DO  I=1,KZAL
        KFF=RZKF(I)
        DO J=1,16
          DEGHILF(J)=RZDEG(J,1,I)
        ENDDO
        DCMM=APAFA(KFF,DEGHILF)
        IF(DCMM.LT.DCNEU) THEN
          CYCLE
        ENDIF
C
C       UMSPEICHERN
C
        DO J=KZAL,I,-1
          RZNR(J+1)=RZNR(J)
          RZKF(J+1)=RZKF(J)
          DO L=1,RZKF(J)
             RZACONZ(L,J+1)=RZACONZ(L,J)
             RZPRE(L,J+1)=RZPRE(L,J)
             RZLK(L,J+1)=RZLK(L,J)
          END DO
          DO L=1,16
             RZDEG(L,1,J+1)=RZDEG(L,1,J)
          END DO
        ENDDO
        GOTO 134
      ENDDO
      I=KZAL+1
 134  IM=I
      CALL BEAUSO(KF,LK,ACONZ,IM,RZNR(IM),RZKF(IM),
     &            RZACONZ(1,IM),RZPRE(1,IM),RZLK(1,IM),RZDEG(1,1,IM))
C
C
C                                       
C******************************************************************************

      KZAL=MIN0(KZAL+1,KRE)
C
C
C
C                              
C 
  135 GOTO 111
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
 140  KZAA=0
      KZAN=0
      IF(KZAL.LE.0) THEN
         GOTO 900
      ENDIF

C
C
C
C
C
 141  KZAA=KZAA+1
C
C 
C 
C
      KF=RZKF(KZAA)
      DO J=1,KF
        LK(J)=RZLK(J,KZAA)
        ACONZ(J)=RZACONZ(J,KZAA)
      END DO
C
C
C     ABFRAGEN OB FARBSTOFFAUSWAHL BEREITS VORHANDEN
C
C
      IF(LOPT) THEN
        IF(ENTHAL(MNF,KFM,KF,LK,ACONZ,KZAN,RZKF,RZLK,RZACONZ,EPPS))THEN
          GOTO 142
        ENDIF
      ENDIF
C
C
C 
C 
C     BERECHNUNG DER FARBMITTELMENGEN
C 
  147 IF(KF.EQ.0) GOTO 142
      KFJT=KFIT/5
      CALL REZFIT(KFJT,KF,ITM,MAXFUN,ATO,
     &                 ACONZ,PHI,IER)
      KFJT=MOD(KFIT,5)
      CALL REZFIT(KFJT,KF,ITM,MAXFUN,ATO,
     &                 ACONZ,PHI,IER)
C
C
C
C     ELIMINIEREN DER NULLMENGEN
C
C
C
C
C
      II=0
      DO 179 I=1,KF
      IF(ACONZ(I).LE.EPS.AND.LK(I).NE.IBI
     &                  .AND.KTO(LK(I)).EQ.BLA) GOTO 179
      II=II+1
      LQ(II)=LK(I)
      CQ(II)=ACONZ(I)
  179 CONTINUE
      KQ=II
      IF(KQ.EQ.0) GOTO 142
C
C     ABFRAGEN OB FARBSTOFFAUSWAHL BEREITS VORHANDEN
C
      IF(ENTHAL(MNF,KFM,KQ,LQ,CQ,KZAN,RZKF,RZLK,RZACONZ,EPPS))THEN
          GOTO 142
      ENDIF
C
C
C                                       
C
C 
C
C                                                          
C
      CALL REDETR(KORR,SENS,KQ,CQ)

      CALL BEASOP(JABST,KQ,LQ,CQ,PRE,NWS(),RV,RR,KGX,
     &            JUV,JUN,JUU,1,IRETR(1),KWE,KORR,SENS,DVV,DEG)
C
C 
C     REZEPT ZURUECKSPEICHERN
C 
C 
      KZAN=KZAN+1
      CALL BEAUSO(KQ,LQ,CQ,KZAN,RZNR(KZAN),RZKF(KZAN),
     &            RZACONZ(1,KZAN),RZPRE(1,KZAN),RZLK(1,KZAN),
     &            RZDEG(1,1,KZAN))
C
C
C
C
C
C
C
C
C     Rezept nach REMNG USW. umspeichern
C
C
C
      NFR(KZAN)=RZKF(KZAN)
      DO L=1,RZKF(KZAN)
         LKK=RZLK(L,KZAN)
c         IF(ABS(RZACONZ(L,KZAN)).LT.EPS) THEN
c            RZACONZ(L,KZAN)=0.
c         ENDIF
         LKID(L,KZAN)=LKK
         REMNG(L,KZAN)=RZACONZ(L,KZAN)*ANORM
      ENDDO
      IF(KZAN.GE.KZA) GOTO 900
C
C
C 
C
C
C
  142 IF(KZAN.LT.KRE.AND.KZAA.LT.KZAL) GOTO 141
C
 900  DEALLOCATE(RZACONZ,RZPRE,RZLK,RZKF,RZNR,RZDEG,STAT=IER)
      KZA=KZAN
C 
C 
C
C 
C 
      RETURN
C
C
C
C
C 
      END
C
C******************************************************************************
C******************************************************************************

c
C
C     Last change:  UFO  18 Mar 105   10:42 am
C
      SUBROUTINE PRUDEK(NFD,IER)
      USE MODRWRZ
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODABST

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C
C
      DIMENSION COHLF(NFD)
C
C 
C
C
      JO=1
C
C
C
C     PRUEFEN, OB DECKVERMOEGEN BERECHNET WERDEN KANN
C
      IF(KGX.EQ.1) THEN
C
C 
C
         JO=2
         IF(D(JO).EQ.0.) THEN
            D(JO)=D(1)
         ENDIF    
         IF(IBI.NE.0) THEN
            LKS=LK(1)    
            LK(1)=IBI         
            COHLF(1)=1.
           DO K=1,JO,JO-1
             DO KW=1,1
               CALL MALRW(1,COHLF,RR(1,KW,K),KW,K)
               GKK=GLZNOG(KW,IRETR(K))
               CALL NOGXX(XYZB(1,1,KW,K),1,RR(1,KW,K),GKK)
             ENDDO
           ENDDO
           LK(1)=LKS
           CALL DELABAL(JABST,XYZB(1,1,1,1),XYZB(1,1,1,JO),
     &                DEW,DLD,DC,DH,DAD,DBD,1)
           IF(DEW.LT.FDE) THEN
              IER=-4062
              IF(IFEHL(IER).NE.0) GOTO 900
           ENDIF
         ELSE
           SUU=1.D-50
           DO I=1,NF
              SUU=SUU+ACONZ(I)
           ENDDO
           DO I=1,NF
              COHLF(I)=ACONZ(I)/SUU
           ENDDO

           DO K=1,JO,JO-1
             DO KW=1,1
               CALL MALRW(1,COHLF,RR(1,KW,K),KW,K)
               GKK=GLZNOG(KW,IRETR(K))
               CALL NOGXX(XYZB(1,1,KW,K),1,RR(1,KW,K),GKK)
             ENDDO
           ENDDO
           CALL DELABAL(JABST,XYZB(1,1,1,1),XYZB(1,1,1,JO),
     &                DEW,DLD,DC,DH,DAD,DBD,1)
           IF(DEW.LT.FDE) THEN
              IER=-4121
              IF(IFEHL(IER).NE.0) GOTO 900
           ENDIF
         ENDIF

      ENDIF
  900 RETURN
      END
      DOUBLE PRECISION FUNCTION PREGES(KF,LQ,C,PRE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION LQ(*),C(*),PRE(*)
      DATA EPQ/1.D-30/
C
C     C=effektive Konzentrationen, deshalb Korrektur mit EFF erforderlich um auf reine Gewichtsmengen zu kommen
C
      AKOST=0.
      CSUM=EPQ      
      DO 10 I=1,KF
      AKOST=AKOST+C(I)*PRE(LQ(I))
      CSUM=CSUM+C(I)
  10  CONTINUE
C
C
      PREGES=AKOST/CSUM
      RETURN
      END
      DOUBLE PRECISION FUNCTION CCST(J,NST,PM,CU,CO)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      FF=1./NST
      CMIN=CU 
      IF(CMIN.EQ.0.) CMIN=0.1*CO
      IF(CMIN.GT.0.1*CO) CMIN=0.1*CO
      PMAX=PM
      IF(PMAX.EQ.0.) THEN
        PMAX=0.5*(CMIN/CO)**(-1.333333)
      ENDIF
      PMA=ARSINH(PMAX)
      CMAX=CO
      CCST=SINH(FF*J*PMA)*CMAX/PMAX
      RETURN
      END
c
c
      SUBROUTINE CHKDICK(IER)
      USE MODMERZ
      USE MODRWRZ,ONLY:D
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL LLOPT

      IER=0
      IF(JUU.GE.1.AND.LLOPT().AND.D(1).LE.0.0) THEN
          IER=4068
          GOTO 900
      ELSE
          IF(D(1).LE.0.0) THEN
             D(1)=1.0
          ENDIF
      ENDIF
C
      IF(UBOUND(D,1).GT.1) THEN
        IF(JUV.GT.1.AND.LLOPT().AND.D(2).LE.0.0) THEN
         IER=4069
         GOTO 900
        ELSE
         IF(D(2).LE.0.0) THEN
           D(2)=1.0
         ENDIF
        ENDIF
      ENDIF
 900  RETURN
      END SUBROUTINE
C
C
      SUBROUTINE CHKNU(IPRG,NUV,NUN,NUU,IER)
      USE MODMERZ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER(KIND=4) :: NUV,NUN,NUU,IPRG,IER
      LOGICAL(KIND=4) :: LLOPT
C
C     ANZAHL VORLAGEN,NACHSTELLUNGEN UND UNTERGRUENDE
C
C     Untergrund   (BIT0 = 1)
C     Vorlage      (BIT1 =1)
C     Nachstellung (BIT2=1)
C     IPRG=1 MSHREZ,RAUREZ (2**0)
C     IPRG=2 REZEP         (2**1)
C     IPRG=4 MSHKOR        (2**2)
C     IPRG=6 REZKOR         (2**2+2**1)
      JUV= NUV
      JUN= NUN
      JUU= NUU
C
C
      CALL CHKDICK(IER)
      IF(IER.NE.0) THEN
        GOTO 900
      ENDIF
      IF(JUV.GT.1.AND.JUU.LE.1) THEN
           IER=4155
           GOTO 900
      ENDIF
      IF(JUN.GT.1.AND.JUU.LE.1) THEN
           IER=4156
           GOTO 900
      ENDIF
c
c
c
      IF(IPRG.EQ.2.OR.IPRG.EQ.6) THEN
         IF(JUV.EQ.0) THEN
           IER=4051
           GOTO 900
         ENDIF
      ENDIF
      IF(IPRG.LE.3) THEN
         JUN=0
      ENDIF
      IF(IPRG.GT.1) THEN
        IF(LLOPT()) THEN
          IF(JUU.EQ.0) THEN
             IER=4050
             RETURN
          ENDIF
        ENDIF
c        IF(JUN.LT.JUU) THEN
c          JUN=JUU
c        ENDIF
c        IF(JUN.LT.JUV) THEN
c           JUN=JUV
c        ENDIF
      ENDIF
      IF(IPRG.EQ.6) THEN
        IF(JUV.EQ.0) THEN
           IER=4051
           RETURN
        ENDIF
        IF(LLOPT()) THEN
          IF(JUU.LT.JUV) THEN
            IER=4093
            RETURN
          ENDIF 
        ENDIF
      ENDIF
      IF(IPRG.GE.4) THEN
        IF(JUN.EQ.0) THEN
           IER=4052
           RETURN
        ENDIF
        IF(LLOPT()) THEN
          IF(JUU.LT.JUN) THEN
            IER=4094
            RETURN
          ENDIF 
        ENDIF
      ENDIF

      IF(IPRG.EQ.6) THEN
C        IF((IGX.EQ.3.OR.IGX.EQ.5).AND.AZU.EQ.0.) THEN
C          IER=-4057
C          GOTO 900
C        ENDIF
C        IF((IGX.NE.0.AND.IGX.NE.3.AND.IGX.NE.5).AND.GGE.EQ.0.) THEN
C          IER=-4072
C          GOTO 900
C        ENDIF
        IF((IGX.EQ.5.OR.IGX.EQ.6).AND.DTO.EQ.0.) THEN
           IER=4058
           RETURN
        ENDIF
c        IF((IGX.EQ.5.OR.IGX.EQ.6).AND.GTO.EQ.0.) THEN
c           IER=4059
c           RETURN
c        ENDIF
      ENDIF
C
C     ABFRAGEN DECKVERMOEGEN
C
C
C 
      IF(KGX.EQ.1) THEN
        IF(GDE.EQ.0.) THEN
          IER=4055
          GOTO 900
        ENDIF
        IF(JUU.LT.2) THEN
          IER=4056
          GOTO 900
        ENDIF
        IF(.NOT.LLOPT()) THEN
          IER=4060
          GOTO 900
        ENDIF
      ENDIF                  



C
C
      IF(IPRG.LE.3) THEN
         
C        Rezeptberechnung (s.MALKO)
C

         IKOR=0
      ELSE
C
C        Korrekturberechnung
C
         IKOR=1
      ENDIF

 900  RETURN
      END
      FUNCTION IBIND(KF,LK,ICHF)
C
C     Index Bindemittel
C
      INTEGER(KIND=4) :: I
      INTEGER(KIND=4),DIMENSION(*) :: LK,ICHF
        IBIND=0
        DO I=1,KF
           IF(ICHF(LK(I)).EQ.1) THEN
             IBIND=I
             EXIT
           ENDIF
        END DO
      RETURN
      END
      FUNCTION IWEIS(KF,LK,ICHF)
C
C     Index Weißpigment
C
      INTEGER(KIND=4) :: I
      INTEGER(KIND=4),DIMENSION(*) :: LK,ICHF
        IWEIS=0
        DO I=1,KF
           IF(ICHF(LK(I)).EQ.2) THEN
             IWEIS=I
             EXIT
           ENDIF
        END DO
      RETURN
      END
      FUNCTION ISCHW(KF,LK,ICHF)
C
C     Index Schwarzpigment
C
      INTEGER(KIND=4) :: I
      INTEGER(KIND=4),DIMENSION(*) :: LK,ICHF
        ISCHW=0
        DO I=1,KF
           IF(ICHF(LK(I)).EQ.3) THEN
             ISCHW=I
             EXIT
           ENDIF
        END DO
      RETURN
      END
      FUNCTION IMETL(KF,LK,ICHF)
C
C     Index Metalleffektpigment
C
      INTEGER(KIND=4) :: I
      INTEGER(KIND=4),DIMENSION(*) :: LK,ICHF
        IMETL=0
        DO I=1,KF
           IF(ICHF(LK(I)).EQ.4) THEN
             IMETL=I
             EXIT
           ENDIF
        END DO
      RETURN
      END
C
C
C
      LOGICAL(KIND=4) FUNCTION ENTHAL(MNF,KFM,KF,LK,ACONZ,
     &                                KZAN,RZKF,RZLK,RZACONZ,EPS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C
C      PRÜFEN, OB REZEPT LK,ACONZ IN DEN bereits berechneten Rezepten (1 ... KZAN) enthalten ist
C
C
      REAL(KIND=8),DIMENSION(KFM,*) :: RZACONZ
      INTEGER(KIND=4),DIMENSION(KFM,*) :: RZLK
      REAL(KIND=8),DIMENSION(*) :: ACONZ
      INTEGER(KIND=4),DIMENSION(*) ::LK,RZKF
      INTEGER(KIND=4),DIMENSION(MNF) :: LQ,LP
      INTEGER(KIND=4) :: I,J,KQ,KP,LA,LB,KFM,KF,KZAN
      ENTHAL=.FALSE.
       KQ=0
       DO J=1,KF
        IF(ACONZ(J).GT.EPS) THEN
C
C         Farb-/Bindemittel mit Mengen > 0. herausfiltern
C
C
          KQ=KQ+1
          LQ(KQ)=LK(J)
        ENDIF
       END DO
       DO I=1,KZAN
         KG=RZKF(I)
         KP=0
         DO LB=1,KG
           IF(RZACONZ(LB,I).GT.EPS) THEN
C
C            Farb-/Bindemittel mit Mengen > 0. herausfiltern
C
C
             KP=KP+1
             LP(KP)=RZLK(LB,I)
           ENDIF
         END DO
         LA=0
         IF(KQ.EQ.KP) THEN
           DO LA=1,KQ
               DO LB=1,KP
                   IF(LQ(LA).EQ.LP(LB)) THEN
                     EXIT
                   ENDIF
               END DO
               IF(LB.GT.KP) THEN
C
C
C                LQ(LA) nicht gefunden
C
C
                 EXIT
               ENDIF
           END DO
         ENDIF
         IF(LA.GT.KQ) THEN
C
C             ALLE LQ(LA) in LP(LB) enthalten
C
C
              EXIT
C
          ENDIF
       END DO
C
C
C
       IF(I.LE.KZAN) THEN
C
C
C        Rezept LK bereits vorhanden
C
C
         ENTHAL=.TRUE.
       ENDIF
      RETURN
      END


