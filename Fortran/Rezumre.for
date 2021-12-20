C     Last change: KU 08.08.2021 12:31:35
C
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  RCHREI  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
C
C      INCLUDE "FAKER2.FOR"

C
C 
C
C
C
C
C
C
      SUBROUTINE RCHREI(MNF,IVOLL,INFL,SPLOE,BAMNG,FAMNG,
     &                  PROZ,PROB,EFFL,SPZ,ICHFL,FEHL)
C
C
C
      USE MOTFEHL
C
C
C     BERECHNUNG REINMENGEN
C     AUS NORMIERUNGSMENGEN (INF)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      INTEGER(KIND=4):: MNF,IVOLL,INFL
      REAL(KIND=4)::SPLOE
      REAL(KIND=4),DIMENSION(*)::PROZ,PROB,BAMNG,FAMNG,SPZ,EFFL
      INTEGER(KIND=4),DIMENSION(*)::ICHFL

      TYPE(TYFEH) FEHL
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: EFF
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: VSCH
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: RSCH
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: CBA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: CLA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::SPEZ
      INTEGER(KIND=4),DIMENSION(:),ALLOCATABLE ::ICHF
      DLL_EXPORT RCHREI
C
      CALL FEHINI()
C 
      IER=0
      NF=MNF
      IF(NF.LE.0) THEN
         IER=4043
         IF(IFEHL(IER).NE.0) GOTO 999
      ENDIF
      ALLOCATE(VSCH(NF),CBA(NF),CLA(NF),SPEZ(NF),EFF(NF),
     &         RSCH(NF),ICHF(NF),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
C
C
C
C
C 
C     UMSPEICHERN
C      
C 
      DO I=1,NF
C        K=REMNG(I)%GRUNR
         K=I
         IF(K.LE.0) THEN
            IER=4075
            IF(IFEHL(IER).NE.0) GOTO 900
         ENDIF
         VSCH(I)=BAMNG(I)
         SPEZ(I)=SPZ(I)
         ICHF(I)=ICHFL(I)
         EFF(I)=EFFL(I)
      ENDDO
      SPLE=SPLOE
      INF=INFL
      IVOL=IVOLL
C
      CALL FABIPR(NF,PROZ,PROB,CBA,CLA)
C
C
C
C
C
C     BERECHNUNG DER REINEN FARB-/BINDEMITTELMENGEN RSCH
C 
      CALL CALREI(IVOL,NF,ICHF,EFF,SPEZ,SPLE,
     &                          INF,VSCH,CBA,CLA,RSCH,IER)
C
C
C

C
C
C
C
C
      DO I=1,NF
         FAMNG(I)=RSCH(I)
      ENDDO
C
      IF(IFEHL(IER).NE.0) GOTO 900

C
C
C
 900  CALL GETFEH(FEHL)
      DEALLOCATE(VSCH,CBA,CLA,SPEZ,EFF,
     &         RSCH,ICHF)
      RETURN
c
 999  CALL GETFEH(FEHL)
      RETURN
      END
C 

C
C
C
C
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  RCHMNG  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
C
C
C
C
C
C
C
C
      SUBROUTINE RCHMNG(MNF,IVOLL,INFL,SPLOE,BAMNG,FAMNG,
     &                  PROZ,PROB,EFFL,SPZ,ICHFL,FEHL)
C
C
      USE MOTFEHL

C
C     UMRECHNUNG DER NORMIERUNGSMENGEN aus den REINMENGEN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      INTEGER(KIND=4):: MNF,IVOLL,INFL
      REAL(KIND=4)::SPLOE
      REAL(KIND=4),DIMENSION(*)::PROZ,PROB,BAMNG,FAMNG,SPZ,EFFL
      INTEGER(KIND=4),DIMENSION(*)::ICHFL
      TYPE(TYFEH) FEHL
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: EFF
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: VSCH
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::RSCH
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: CBA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: CLA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: SPEZ
      INTEGER(KIND=4),DIMENSION(:),ALLOCATABLE :: ICHF
      DLL_EXPORT RCHMNG
C
      CALL FEHINI()

C
      IER=0
C
C
      NF=MNF
      IF(NF.LE.0) THEN
         IER=4043
         IF(IFEHL(IER).NE.0) GOTO 999
      ENDIF
      ALLOCATE(VSCH(NF),CBA(NF),CLA(NF),SPEZ(NF),EFF(NF),
     &         RSCH(NF),
     &         ICHF(NF),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 999
      ENDIF

C 
C 
C     UMSPEICHERN
C      
C
      DO I=1,NF
         RSCH(I)=FAMNG(I)
         SPEZ(I)=SPZ(I)
         ICHF(I)=ICHFL(I)
         EFF(I)=EFFL(I)
      ENDDO
      SPLO=SPLOE
C 
C
      CALL FABIPR(NF,PROZ,PROB,CBA,CLA)
C
C
C
      IVOL=IVOLL
      INF=INFL
C
C
C
C
C
C
C
C     BERECHNUNG DER NORMIERUNGSMENGEN VSCH AUS 
C     REINEN FARB-/BINDEMITTELMENGEN RSCH
C
C
C
      CALL CALMNG(IVOL,NF,ICHF,EFF,SPEZ,SPLO,
     &                          INF,VSCH,CBA,CLA,
     &                          RSCH,IER)
C
C
C
C
      DO I=1,NF
         BAMNG(I)=VSCH(I)
      ENDDO
C
      IF(IFEHL(IER).NE.0) GOTO 900

C
C
 900  CALL GETFEH(FEHL)
      DEALLOCATE(VSCH,CBA,CLA,SPEZ,RSCH,EFF,
     &           ICHF)
      RETURN
c
 999  CALL GETFEH(FEHL)
      RETURN
      END
C 
C
C
C
C
C 
C
C
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  RCHAMX  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
C
C
C
C
C
C
C
      SUBROUTINE RCHAMX(MNF,IVOLL,INFL,INOL,INPL,INQL,SPLOE,FAMNG,
     &                 PROZ,PROB,EFFL,SPZ,ICHFL,OPL,
     &                 MNGREI,MNGBA,MNGAKT,MNGZAE,MNGNEN,FEHL)

C
C
C
C
      USE MOTFEHL

C
C     BERECHNUNG DER NORMIERUNGSMENGEN AUS REINMENGEN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      INTEGER(KIND=4):: MNF,IVOLL,INOL,INPL,INQL,INFL
      REAL(KIND=4)::SPLOE,MNGREI,MNGAKT,MNGZAE,MNGNEN,MNGBA
      REAL(KIND=4),DIMENSION(*)::PROZ,PROB,FAMNG,SPZ,EFFL
      INTEGER(KIND=4),DIMENSION(*)::ICHFL
      INTEGER(KIND=1),DIMENSION(*)::OPL

      TYPE(TYFEH) FEHL
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::EFF
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::RSCH
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::VSCH
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: CBA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: CLA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: SPEZ
      INTEGER(KIND=4),DIMENSION(:),ALLOCATABLE :: ICHF
      CHARACTER(LEN=1),DIMENSION(:),ALLOCATABLE ::OP
      DLL_EXPORT RCHAMX
C             
      CALL FEHINI()

C 
      IER=0
      NF=MNF
      IF(NF.LE.0) THEN
         IER=4043
         IF(IFEHL(IER).NE.0) GOTO 999
      ENDIF
      ALLOCATE(CBA(NF),CLA(NF),SPEZ(NF),EFF(NF),
     &         VSCH(NF),RSCH(NF),OP(NF),
     &         ICHF(NF),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 999
      ENDIF

C 
C 
C     UMSPEICHERN
C      
C 
      DO I=1,NF
         K=I
         IF(K.LE.0) THEN
            IER=4075
            IF(IFEHL(IER).NE.0) GOTO 900
         ENDIF
         RSCH(I)=FAMNG(I)
         SPEZ(I)=SPZ(I)
         ICHF(I)=ICHFL(I)
         OP(I)=CHAR(OPL(I))
         EFF(I)=EFFL(I)
      ENDDO
      SPLO=SPLOE
C 
C
      CALL FABIPR(NF,PROZ,PROB,CBA,CLA)
C
C
C
      IVOL=IVOLL
      INO=INOL
      INP=INPL
      INQ=INQL
      INF=INFL
C
C
C
C
C
C
C
C     BERECHNUNG VON
C     AMR:  REINE FARB-/BINDEMITTELMENGE
C     AMM:  NORMIERUNGSMENGE
C     AMZA: ZAEHLER PROZENTIGKEIT
C     AMNE: NENNER PROZENTIGKEIT
C     AUS REINEN FARB-/BINDEMITTELMENGEN RSCH
C
C
C
      CALL CALMNX(IVOL,NF,ICHF,EFF,SPEZ,SPLO,OP,CBA,CLA,RSCH,
     &                          AMR,INO,AMM,
     &                          INP,INQ,AMZA,AMNE,IER)
C
C
C
      IF(IFEHL(IER).NE.0) GOTO 900
C
      MNGREI=AMR
      MNGAKT=AMM
      MNGZAE=AMZA
      MNGNEN=AMNE
C
      CALL CALMNG(IVOL,NF,ICHF,EFF,SPEZ,SPLO,
     &                          INF,VSCH,CBA,CLA,
     &                          RSCH,IER)
      MNGBA=0.
      DO I=1,NF
        MNGBA=MNGBA+VSCH(I)
      END DO
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
 900  CALL GETFEH(FEHL)
      DEALLOCATE(CBA,CLA,SPEZ,RSCH,VSCH,OP,EFF,
     &           ICHF)
      RETURN
c
 999  CALL GETFEH(FEHL)
      RETURN
      END
C 
C
C
C
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  RCHZUW  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
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
      SUBROUTINE RCHZUW(MNF,REMNG,REALT,REZUW,FEHL)
      USE MOTFEHL

C
C     BERECHNUNG DER REINEN ZUWAAGEN AUS DEN REINMENGEN
C      
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(*)::REMNG,REALT,REZUW
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: ZUWA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::RNEU
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::RALT
      DLL_EXPORT RCHZUW
C             
      CALL FEHINI()

C
      IER=0
      NF=MNF
      IF(NF.LE.0) THEN
         IER=4043
         IF(IFEHL(IER).NE.0) GOTO 999
      ENDIF
      ALLOCATE(RNEU(NF),RALT(NF),ZUWA(NF),STAT=IER)
      IF(IFEHL(IERALC(IER)).NE.0) GOTO 999
C
C
C 
C     UMSPEICHERN
C
C 
      DO I=1,NF
         RNEU(I)=REMNG(I)
         RALT(I)=REALT(I)
      ENDDO
C
C
      CALL ZUWAG(NF,RNEU,RALT,ZUWA,IER)
C
C
      DO I=1,NF
         REZUW(I)=ZUWA(I)
      ENDDO
C
C
C
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
 900  CALL GETFEH(FEHL)
      DEALLOCATE(ZUWA,RNEU,RALT)
      RETURN
 999  CALL GETFEH(FEHL)
      RETURN
      END
c
c


