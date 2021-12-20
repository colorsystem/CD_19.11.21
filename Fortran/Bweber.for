C     Last change:  UFO  12 Apr 108   12:53 pm
c
c
c
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  BWEBER  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
      INTEGER(KIND=4) FUNCTION BWEBER(KW,NWEL,KML,NQU,
     &                QUREDAM,TYREDAM,RDAM,FEHL)
c
c
c
c
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
C     BERECHNUNG DES GUENSTIGSTEN B-WERTES
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
      INTEGER(KIND=4) ::NWEL,KML,KW,NQU
      TYPE(TYRWERT) TYREDAM(*)
      TYPE(QURWERT) QUREDAM(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM
      TYPE(SRWERT) RWERP
      REAL(KIND=8),TARGET,ALLOCATABLE,DIMENSION(:) ::RZ                                           RUK21,RUK22

      TYPE(TYFEH) FEHL
      INTEGER INBW(8)
C
      DATA NFR/8/
C
C
C
      DLL_EXPORT BWEBER

C
      ALLOCATE(RZ(NWEL),STAT=IER)
      IF(IFEHL(IERALC(IER)).NE.0)THEN
         GOTO 900
      ENDIF
C
C
      RWERP%R=>RZ
C
      CALL SRT000(RWERP)
C
      IER=0
      IF(KW.LE.0.OR.KW.GT.KML) THEN
        IF(IFEHL(4165).NE.0) GOTO 900
      ENDIF
C
      KBWD=KBWT
C
C
C
C     BESTIMMUNG DER "RICHTIGEN" RICHTTYPTIEFE
      
C
C
      IF(KBWD.EQ.0) THEN
        DO I=1,NFR
            INBW(I)=0
        ENDDO
        DO L=1,NQU
          IF(TYREDAM(L)%RETR.EQ.1) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
          ENDIF
            CALL GETKPF(KW,TYREDAM(L),QUREDAM(L),RWERP,IER)
            CALL GETRWRT(NWEL,RDAM(1,KW,L),RWERP%R)
C            CALL GETREF(1,NWE,TYREDAM(L),RDAM(1,KW,L),RWERP,IER)
            IF(IFEHL(IER).NE.0) GOTO 900
            KBW=IBWERT(RWERP%R(1),GLANZ(KW))

            INBW(KBW)=INBW(KBW)+1
        ENDDO
        KMAX=0
        KBWT=1
        DO J=1,NFR
            IF(INBW(J).GT.KMAX) THEN
               KMAX=INBW(J)
               KBWT=J
            ENDIF
        ENDDO
      ELSE
          KBWT=KBWD
      ENDIF
      BWEBER=KBWT
  900 CALL GETFEH(FEHL)
      DEALLOCATE(RZ,STAT=IER)
      RETURN
C
      END 
C

