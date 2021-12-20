C     Last change: KU 24.06.2019 12:13:29
C
C
C
C
C
      SUBROUTINE RwertMit(AK,NGKL,GKW,NANZ,KMW,NW,CM,RM,RU,R,RO,
     &                    FEHL)
      USE MOTFEHL
      USE MODGKWR,ONLY:AKA,GK
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE(TYFEH) :: FEHL
      INTEGER*4 KMW,NANZ,NW,IER
      REAL(KIND=4) :: AK
      REAL(KIND=4),DIMENSION(NGKL,*)::GKW
      REAL(KIND=4),DIMENSION(*) :: CM
      REAL(KIND=4),DIMENSION(NW,*):: R,RO,RU
      REAL(KIND=4),DIMENSION(NW,KMW,*):: RM
      REAL(KIND=8),DIMENSION(NANZ) :: CMIT
      REAL(KIND=8),DIMENSION(NW) :: RS,RQ
C
C     BERECHNUNG VON MITTELWERTEN UND STANDARDABWEICHUNGEN 
C     FUER REFELEXIONSWERTE
C
C
C 
C
C
C
      DLL_EXPORT RwertMit
C
C
      IER=0
      CALL FEHINI()
      CALL MODGET(AK,NGKL,GKW,NW,KMW,IER)
      IF(IFEHL(IER).NE.0)THEN
        GOTO 900
      ENDIF

C
C     CM auf 1.0 normieren
C
      SUU=0.00000001
      DO K=1,NANZ
        SUU=SUU+CM(K)
      END DO
C 
      DO K=1,NANZ
         CMIT(K)=CM(K)/SUU
      END DO
C
C
      DO KW=1,KMW
        DO I=1,NW
          RS(I)=0.0
          RQ(I)=0.0
        END DO
        DO K=1,NANZ
           DO I=1,NW
              RHILF=RM(I,KW,K)
              RRR=RSCHL(AKA,RHILF,KW)
              RS(I)=RS(I)+CMIT(K)*RRR
              RQ(I)=RQ(I)+CMIT(K)*RRR**2
           END DO
        END DO
C
C     Rückrechnung in Reflexionswerte
C
C
        DO I=1,NW
           RMIT=RS(I)
           SI=SQRT(RQ(I)-RMIT**2)
           RU(I,KW)=RRUECK(AKA,RMIT-SI,KW)
           R(I,KW)=RRUECK(AKA,RMIT,KW)
           RO(I,KW)=RRUECK(AKA,RMIT+SI,KW)
        END DO
C
      END DO
C
  900 IF(IFEHL(IER).NE.0) THEN
        CALL GETFEH(FEHL)
      ENDIF
      RETURN
      END SUBROUTINE
C
C
C
C
      SUBROUTINE RwertAKA(AK,NGKL,GKW,KMW,NW,RR,R2,R1,RN,FEHL)
      USE MOTFEHL
      USE MODGKWR,ONLY:AKA,GK
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE(TYFEH) :: FEHL
      INTEGER*4 KMW,NANZ,NW,IER
      REAL(KIND=4) :: AK
      REAL(KIND=4),DIMENSION(NGKL,*)::GKW
      REAL(KIND=4),DIMENSION(NW,*):: RR,R1,R2,RN
C
C     BERECHNUNG NEUER REMISSIONSWERTE
C
C
C 
C
C
C
      DLL_EXPORT RwertAKA
C
C
      IER=0
      CALL FEHINI()
      CALL MODGET(AK,NGKL,GKW,NW,KMW,IER)
      IF(IFEHL(IER).NE.0)THEN
        GOTO 900
      ENDIF


C
      DO KW=1,KMW
        DO I=1,NW
          RR2=R2(I,KW)
          RR1=R1(I,KW)
          RRN=RN(I,KW)
          RR(I,KW)=FRKORRI(AKA,RR2,RR1,RRN,KW)
        END DO
      END DO
C
  900 IF(IFEHL(IER).NE.0) THEN
        CALL GETFEH(FEHL)
      ENDIF
      RETURN
      END SUBROUTINE
C
C
C

