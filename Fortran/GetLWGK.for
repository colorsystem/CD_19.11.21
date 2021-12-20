C     Last change: KU 20.02.2020 15:42:19
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  GETFDM  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************

      SUBROUTINE GETFDM(Kww,AkaL,FoptL,CDEL,
     &                  NLZL,BeIDL,NormIDL,GewNormL,FaktL,NWEL,KML,
     &                  WsolL,EL,IhrmWinL,GewIhrmL,NGKL,GKL,FEHL)
      USE MODILLU
      USE MODWINK
      USE MODGKWR
      USE MOTFEHL

      IMPLICIT NONE
      INTEGER(KIND=4) ::I,IER,ICHK,IFEHL,K,J,KW,IERALC,NGKL
C
C
      TYPE(TYFEH) :: FEHL
      INTEGER(KIND=4) ::NLZL,BeIDL,NWEL,KML,KWW
      INTEGER(KIND=4),DIMENSION(*) :: IHRMWinL,NORMIDL
      REAL(KIND=4) :: FoptL,AKAL
      REAL(KIND=4),DIMENSION(*) :: GewNormL,GewIhrmL,WSOLL
      REAL(KIND=4),DIMENSION(NGKL,*) :: GKL
      REAL(KIND=4),DIMENSION(3,*) :: FAKTL
      REAL(KIND=4),DIMENSION(NWEL,3,*) :: EL
      INTEGER(KIND=1),DIMENSION(2) ::CDEL
C
C
C
      DLL_EXPORT GETFDM
C
C
C
C
C     MODILLU erstellen
C
C
C
      IER=0
      CALL FEHINI()
C

C
C
C
      CALL TERMFDM(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
C
C
      CALL MODGET(AKAL,NGKL,GKL,NWEL,KML,IER)
      IF(IFEHL(IER).NE.0)THEN
        GOTO 900
      ENDIF
C
C
      NLZ=NLZL
      NB=BeIDL
C
C
C
C
C     Art der Mittelwertbildung
C     1 arithmetisch
C     2 geometrisch
C     3 harmonisch
C
      KA=INT(2.5-AKAL)
C
      IF(KA.LE.0) KA=1
C
C
C
C     UMRECHNUNGSFAKTOR FUER OPTISCHE DICHTE
C
      FOPT=FOPTL
C
C
C

      IF(NLZ.LE.0) THEN
         IER=4004
         IF(IFEHL(IER).NE.0)THEN
           GOTO 900
         ENDIF
      ENDIF
      ALLOCATE(NLA(NLZ),FA(NLZ),WSOL(NWEL),FTKT(3,NLZ),
     &          EILLU(NWEL,3,NLZ),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      DO I=1,NLZ
          NLA(I)=normIDL(I)
          FA(I)=GewNormL(I)
          IF(NLA(I).LT.0) THEN
             IER=4026
             IF(IFEHL(IER).NE.0)THEN
               GOTO 900
             ENDIF
          ENDIF
      ENDDO
C
      ICHK=0
      DO I=1,NLZ
          IF(FA(I).GT.0.D0) THEN
             ICHK=1
          ENDIF
      ENDDO
      IF(ICHK.EQ.0) THEN
          IER=4029
          IF(IFEHL(IER).NE.0)THEN
           GOTO 900
          ENDIF
      ENDIF

      DO I=1,NWE
         WSOL(I)=WSOLL(I)
         IF(WSOL(I).LE.0.D0) THEN
            IER=4028
            IF(IFEHL(IER).NE.0) THEN
               GOTO 900
            ENDIF
         ENDIF
      ENDDO
C
C
C
       DO K=1,NLZ
          DO J=1,3
             FTKT(J,K)=FAKTL(J,K)
             IF(FTKT(J,K).LE.0.D0) THEN
                IER=4027
                IF(IFEHL(IER).NE.0) THEN
                 GOTO 900
                ENDIF
             ENDIF
             DO I=1,NWE
                EILLU(I,J,K)=EL(I,J,K)
             ENDDO
          ENDDO
        ENDDO
C
C
C
C     MODWINK
C
C
C
C
C     Winkel
C
C
c

      ALLOCATE(IHRMWIN(KM),FWI(KM),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF

C
C
C
      DO KW=1,KML
         IHRMWIN(KW)=IhrmWinL(KW)
         FWI(KW)= GewIhrmL(KW)
      ENDDO
      ICHK=0
      DO I=1,KML
          IF(FWI(I).GT.0.D0) THEN
             ICHK=1
          ENDIF
      ENDDO
      IF(ICHK.EQ.0) THEN

         IER=4030
         IF(IFEHL(IER).NE.0)THEN
           GOTO 900
         ENDIF
      ENDIF

C
C
C     WINKEL FUER FARBABSTAND (Rezeptrechnung (KWC))
C     Winkel für Auswertung (Qualitätskontrolle)
C

      KWC=KWW
      IF(KWC.GT.KM) THEN
        KWC=1
      ENDIF

C
C
C     MODGKWR
C
C
C
      CDE(1:1)=CHAR(CDEL(1))
      CDE(2:2)=CHAR(CDEL(2))
      IF(ICHAR(CDE(1:1)).LE.64) THEN
         IER=4022
         IF(IFEHL(IER).NE.0)THEN
           GOTO 900
         ENDIF
      ENDIF

      IF(CDE(1:1).NE.'E'.AND.CDE(1:1).NE.'S') THEN
      DO KW=1,KML
        IF(GK(7,KW).LE.1.D-10) THEN
           IER=4139
           IF(IFEHL(IER).NE.0)THEN
             GOTO 900
           ENDIF
        ENDIF
      END DO
      ENDIF



 900  CALL GETFEH(FEHL)
      IF(FEHL%IFEH.NE.0) THEN
         CALL TERMFDM(IER)
      ENDIF

C
C
      RETURN
      END SUBROUTINE
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  GET6164  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************

      SUBROUTINE GET6164(NLZL,NormIDL,FaktL,NWEL,WsolL,EL,FEHL)
      USE MOD6164
      USE MOTFEHL

      IMPLICIT NONE
      INTEGER(KIND=4) ::I,IER,IFEHL,K,J
C
C
      TYPE(TYFEH) :: FEHL
      INTEGER(KIND=4) ::NWEL,NLZL,IERALC
      INTEGER(KIND=4),DIMENSION(*) :: NORMIDL
      REAL(KIND=4),DIMENSION(*) :: WSOLL
      REAL(KIND=4),DIMENSION(3,*) :: FAKTL
      REAL(KIND=4),DIMENSION(NWEL,3,*) :: EL
      REAL(KIND=8) :: EPW
      DATA EPW/1.D-3/
C
C
C
      DLL_EXPORT GET6164
C
C
C
C
C     MODILLU erstellen
C
C
C
      IER=0
      CALL FEHINI()
C
C
C
C
      NW35=NWEL
      IF(NW35.NE.35) THEN
        IER=4000
         IF(IFEHL(IER).NE.0) THEN
           GOTO 900
         ENDIF
      ENDIF
      IF(ABS(WSOLL(1)-380.).GT.EPW) THEN
        IER=4000
        IF(IFEHL(IER).NE.0) THEN
           GOTO 900
        ENDIF
      ENDIF
      IF(ABS(WSOLL(NW35)-720.).GT.EPW) THEN
        IER=4000
        IF(IFEHL(IER).NE.0) THEN
           GOTO 900
        ENDIF
      ENDIF


C
C
C
      IF(NWEL.LE.0) THEN
         IER=4005
         IF(IFEHL(IER).NE.0) THEN
           GOTO 900
         ENDIF
      ENDIF
      ALLOCATE(NL35(NLZL),WSOL35(NW35),F6164(3,NLZL),
     &          E6164(NW35,3,NLZL),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      DO I=1,NLZL
          NL35(I)=normIDL(I)
          IF(NL35(I).LT.0) THEN
             IER=4026
             IF(IFEHL(IER).NE.0) THEN
              GOTO 900
             ENDIF
          ENDIF
      ENDDO

C

      DO I=1,NWEL
         WSOL35(I)=WSOLL(I)
         IF(WSOL35(I).LE.0.D0) THEN
            IER=4028
            IF(IFEHL(IER).NE.0) THEN
              GOTO 900
            ENDIF
         ENDIF
      ENDDO

C
C
C
       DO K=1,NLZL
          DO J=1,3
             F6164(J,K)=FAKTL(J,K)
             IF(F6164(J,K).LE.0.D0) THEN
                IER=4027
                IF(IFEHL(IER).NE.0) THEN
                 GOTO 900
                ENDIF
             ENDIF
             DO I=1,NW35
                E6164(I,J,K)=EL(I,J,K)
             ENDDO
          ENDDO
        ENDDO
C
C

C

  900 CALL GETFEH(FEHL)
      IF(FEHL%IFEH.NE.0) THEN
         CALL TERMFDM(IER)
      ENDIF

C
C
      RETURN
      END SUBROUTINE
C
C
C
C
C
C
      SUBROUTINE TERMFDM(IER)
      USE MODWINK
      USE MODILLU
      USE MOD6164
      USE MODGKWR
      INTEGER(KIND=4) ::IER
      IER=0
      NWE=0
      KM=0
      NLZ=0
      IF(ALLOCATED(WSOL35)) THEN
         DEALLOCATE(NL35,WSOL35,F6164,E6164)
      ENDIF
      IF(ALLOCATED(WSOL)) THEN
         DEALLOCATE(NLA,FA,WSOL,FTKT,EILLU)
      ENDIF
      IF(ALLOCATED(IHRMWIN)) THEN
         DEALLOCATE(IHRMWIN,FWI)
      ENDIF
      IF(ALLOCATED(GK)) THEN
         DEALLOCATE(GK)
      ENDIF

      RETURN
      END SUBROUTINE
C
C
      INTEGER(KIND=4) FUNCTION TESTFDM(NWEL,KML)
      USE MODILLU
      USE MODWINK
      USE MODGKWR
      USE MODFEHL
      IMPLICIT NONE
      INTEGER(KIND=4) ::NWEL,KML
      TESTFDM=0
      FEHH%KENN=0
      IF(.NOT.ALLOCATED(NLA)) THEN
        TESTFDM=4000
        FEHH%KENN=1
        RETURN
      ENDIF
      IF(.NOT.ALLOCATED(IHRMWIN)) THEN
        TESTFDM=4000
        FEHH%KENN=2
        RETURN
      ENDIF
      IF(NWE.NE.NWEL) THEN
        TESTFDM=4000
        FEHH%KENN=3
        RETURN
      ENDIF
      IF(KM.NE.KML) THEN
        TESTFDM=4000
        FEHH%KENN=4
        RETURN
      ENDIF
      RETURN
      END


