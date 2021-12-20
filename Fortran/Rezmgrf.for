C     Last change: KU 12.04.2020 13:24:16
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  REZMGRF  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
      SUBROUTINE REZMGRF(IPRG,NUU,NUV,NUN,NWEL,KML,RETR,KWB,
     &           RUNT,RVOR,RNAC,MNF,SOMNG,DICKE,FEHL)
      USE MODFEHL
      USE MODFARB
      USE MODRWRZ
      USE MODMERZ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      INTERFACE
      INTEGER(KIND=4) FUNCTION DIMIER(NWEL,KML,NLZL,NPQGL,MNF,NUA)
      INTEGER(KIND=4) :: NWEL
      INTEGER(KIND=4),OPTIONAL :: KML,NLZL,NPQGL,MNF,NUA
      END FUNCTION
      END INTERFACE
C
C
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RVOR,RNAC,RUNT
      INTEGER*4 IPRG,NWEL,KML,NUV,NUN,NUU,MNF,IFEHL,NUMAX
      INTEGER(KIND=4),DIMENSION(*)::RETR,KWB
      REAL(KIND=4),DIMENSION(*)::DICKE
      REAL(KIND=4),DIMENSION(*) :: SOMNG
      TYPE(TYFEH) FEHL
C
      DLL_EXPORT REZMGRF
C
C     Mengen nach MODFARB übernehmen und mit ANORM=MNGREI korrigieren
C

      IER=0
      CALL FEHINI()
      NUMAX=MAX(NUU,NUV,NUN,UBOUND(IRETR,1))

      IF(IFEHL(DIMIER(NWEL,KML=KML,MNF=MNF,NUA=NUMAX)).NE.0)  GOTO 900
C
C
C 
      NWE=NWEL
      KM=KML
      DO K=1,NUMAX
        D(K)=DICKE(K)
        IRETR(K)=RETR(K)
        IF(IRETR(K).LT.0.OR.IRETR(K).GT.1) THEN
            IER=4153
            IF(IFEHL(IER).NE.0)  GOTO 900
        ENDIF
        IKWB(K)=KWB(K)
        IF(IKWB(K).LT.1.OR.IKWB(K).GT.2) THEN
            IER=4077
            IF(IFEHL(IER).NE.0)  GOTO 900
        ENDIF
      END DO
C
C     Anzahl R-Werte überprüfen und übernehmen
C
C
      CALL CHKNU(IPRG,NUV,NUN,NUU,IER)
      IF(IFEHL(IER).NE.0)  GOTO 900
C
C
C
C
C
C     R-Werte Untergrund übernehmen
C
C
C

      DO K=1,UBOUND(RU,3)
         DO KW=1,UBOUND(RU,2)
            DO I=1,UBOUND(RU,1)
               RU(I,KW,K)=0.
            END DO
         END DO
      END DO
      DO K=1,JUU
        CALL GETRRR(NWEL,KML,RUNT(1,1,K),RU(1,1,K),IER)
        IF(IFEHL(IER).NE.0) GOTO 900
      ENDDO
C
C     KORRIGIERTE REFLEXIONSWERTE BERECHNEN
C
C
      DO K=1,JUU
      DO KW=1,KM
         DO I=1,NWE
              RUU=RU(I,KW,K)
              RU(I,KW,K)=TRKORR(RUU,KW,IRETR(K))
         ENDDO
      ENDDO
      ENDDO
C
C
C     R-Werte Vorlage übernehmen
C
C
C 
C
      DO K=1,JUV
        CALL GETRRR(NWEL,KML,RVOR(1,1,K),RV(1,1,K),IER)
        IF(IFEHL(IER).NE.0) GOTO 900
      ENDDO
C
C
C
C     R-Werte Nachstellung übernehmen
C
C
C
C 
      DO K=1,JUN
        CALL GETRRR(NWEL,KML,RNAC(1,1,K),RN(1,1,K),IER)
        IF(IFEHL(IER).NE.0) GOTO 900
      ENDDO
C
C     Mengen übernehmen
C
C
      CALL GETMNG(MNF,SOMNG,IER)
      IF(IFEHL(IER).NE.0)  GOTO 900
C
 900  CALL GETFEH(FEHL)
      RETURN
C
      END SUBROUTINE
C

C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  REZHILF  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
      SUBROUTINE REZHILF(IH,NWEL,KML,
     &           RVOR,RHILF,MNF,REMNG,DICKE,FEHL)
      USE MODFEHL
      USE MODFARB
      USE MODHILF
      USE MODMERZ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) ::RVOR,RHILF
      INTEGER*4  NWEL,KML,MNF,IFEHL
      REAL(KIND=4),DIMENSION(*)::DICKE
      REAL(KIND=4),DIMENSION(*) :: REMNG
      TYPE(TYFEH) FEHL
C
C
      DLL_EXPORT REZHILF

C
C
C
C     REZEPTE FUER HILFKORREKTUREN UEBERNEHMEN
C
C
      IER=0
      CALL FEHINI()
      IHLF=IH
      IF((IH.LE.0).OR.(IH.GT.UBOUND(KFH,1))) THEN
        IER=4162
        GOTO 900
      ENDIF
      CALL GETHLF(IH,DICKE,MNF,REMNG,IER)
      IF(IFEHL(IER).NE.0)  GOTO 900
C

C
C
C
C     R-WERTE VORLAGE FUER HILFSKORREKTUREN UEBERNEHMEN
C
C      
      DO K=1,JUV
        CALL GETRRR(NWEL,KML,RVOR(1,1,K), RZ(1,1,K,IH),IER)
        IF(IFEHL(IER).NE.0) GOTO 900
      ENDDO
C
C    
C
C
C
C     R-WERTE NACHSTELLUNG FUER HILFSKORREKTUREN UEBERNEHMEN
C
      DO K=1,JUN
          CALL GETRRR(NWEL,KML,RHILF(1,1,K),RH(1,1,K,IH),IER)
          IF(IFEHL(IER).NE.0) GOTO 900
      ENDDO
      RETURN
C
  900 IF(IFEHL(IER).NE.0)THEN
        CALL GETFEH(FEHL)
      ENDIF
      RETURN
C
      END SUBROUTINE

c
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
C
