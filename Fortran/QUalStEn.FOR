C     Last change: KU 12.04.2020 13:24:30
C
C
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  QUABEG  ************************************************************************
C***************************************************************************************************************************
C***************************************  04.11.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************

C
      SUBROUTINE QUABEG(NWE,KM,FEHL)
      USE MOTFEHL
      USE MODQUAL

      IMPLICIT NONE
      INTEGER(KIND=4) :: IFEHL,IER,NWE,KM,I
      TYPE(TYFEH) :: FEHL
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
      DLL_EXPORT QUABEG
C
C
      CALL FEHINI()
      IF(IFEHL(DIMIER(NWE,KML=KM)).NE.0) THEN
         GOTO 900
      ENDIF
C
C
C
      IF(ALLOCATED(RU)) THEN
         DEALLOCATE(GH,GU,GV,G,RH,RCROM,RU,RC,RF,A,S,AW,SW,AS,SS,
     &         RP,RQ,RUN,ROK,ROS,RUR,RUM,XM)
      ENDIF
C
C
C     Felder für MODQUAL
C
C
      ALLOCATE(GH(NWE),GU(NWE),GV(NWE),G(NWE),
     &         RH(NWE),RCROM(NWE),RU(NWE),RC(NWE),RF(NWE),
     &         A(NWE),S(NWE),AW(NWE),SW(NWE),AS(NWE),SS(NWE),
     &         RP(NWE,2),RQ(NWE,2),RUN(NWE,2),ROK(NWE,2),ROS(NWE,2),
     &         RUR(NWE,3),RUM(NWE,3),XM(NWE),STAT=IER)
      DO I=1,NWE
         XM(I)=1.0
      END DO
C
C
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
 900  CALL GETFEH(FEHL)
      RETURN
      END SUBROUTINE
C

C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  QUAEND  ************************************************************************
C***************************************************************************************************************************
C***************************************  04.11.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************

C
      SUBROUTINE QUAEND(FEHL)
      USE MODQUAL
      USE MOTFEHL
      IMPLICIT NONE
      INTEGER(KIND=4) :: IFEHL,IER
      TYPE(TYFEH) :: FEHL
C
C
      DLL_EXPORT QUAEND
C
C
      IF(ALLOCATED(FCA)) THEN
         DEALLOCATE(FCA)
      ENDIF
      DEALLOCATE(GH,GU,GV,G,RH,RCROM,RU,RC,RF,A,S,AW,SW,AS,SS,
     &         RP,RQ,RUN,ROK,ROS,RUR,RUM,XM)

      CALL TERMFDM(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF

 900  CALL GETFEH(FEHL)
      RETURN
      END SUBROUTINE
C
C
C

