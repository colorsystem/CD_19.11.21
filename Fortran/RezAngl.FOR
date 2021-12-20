C     Last change:  KLA  17 Aug 113    7:33 pm
C
      SUBROUTINE REZANGLN(MNF,IVOLL,INFL,SPLOE,LID,FAMNG,
     &                   PROZ,PROB,EFFL,SPZ,ICHFL,DOSMIN,FEHL)
C
C
      USE MOTFEHL
      USE MODGREN

C
C     Reinmengen für Normierungsmengen als Vielfache von DOSMIN
C     Größte Mengen werden in geeigneter Art und Weise verändert
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      INTEGER(KIND=4):: MNF,IVOLL,INFL,KANZGES,KMNGGES,KANZMAX,IMAX
      REAL(KIND=4)::SPLOE,DOSMIN
      REAL(KIND=4),DIMENSION(*)::PROZ,PROB,FAMNG,SPZ,EFFL
      INTEGER(KIND=4),DIMENSION(*)::ICHFL,LID
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: KANZ,KZWI
      LOGICAL(KIND=4),ALLOCATABLE,DIMENSION(:) :: MNGAEND
      TYPE(TYFEH) FEHL
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: EFF
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: VSCH
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::RSCH
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: CBA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: CLA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: SPEZ
      REAL(KIND=8) :: SCHLUCK,MNGGES,DOSEPS,SUA
      INTEGER(KIND=4) :: IFEHL
      INTEGER(KIND=4),DIMENSION(:),ALLOCATABLE :: ICHF
C
C
C
      DLL_EXPORT REZANGLN
C
C
      IER=0
      IF(DOSMIN.LE.0.) THEN
         RETURN
      ENDIF
      DOSEPS=0.1*DOSMIN
C
C
      NF=MNF
      IF(NF.LE.0) THEN
         IER=4043
         IF(IFEHL(IER).NE.0) GOTO 999
      ENDIF
      ALLOCATE(VSCH(NF),CBA(NF),CLA(NF),SPEZ(NF),EFF(NF),
     &         RSCH(NF),KANZ(NF),KZWI(NF),MNGAEND(NF),
     &         ICHF(NF),STAT=IER)
      IF(IFEHL(IER).NE.0) GOTO 999

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
      IF(IFEHL(IER).NE.0) GOTO 900
C
      SCHLUCK=DOSMIN

      MNGGES=0.D0
      DO I=1,NF
         MNGGES=MNGGES+VSCH(I)
         MNGAEND(I)=.FALSE.
         KANZ(I)=INT((VSCH(I)+0.5*SCHLUCK)/SCHLUCK)
      ENDDO
      KMNGGES=INT((MNGGES+0.5*SCHLUCK)/SCHLUCK)

C
C
C     Größtes KANZ(I) wird erhöht oder erniedrigt
C
C
C
      DO KK=1,NF
        KANZGES=0
        DO I=1,NF
           KANZGES=KANZGES+KANZ(I)
        END DO
        IF(KANZGES.EQ.KMNGGES) THEN
          EXIT
        ENDIF
C
C
C
C
        KANZMAX=0
        IMAX=0
        DO I=1,NF
           KZWI(I)=KANZ(I)
           IF(KANZ(I).GT.KANZMAX .AND..NOT.MNGAEND(I)) THEN
              IMAX=I
              KANZMAX=KANZ(I)
           ENDIF
        END DO
        IF(IMAX.NE.0) THEN
          IF(KANZGES.LT.KMNGGES) THEN
             KZWI(IMAX)=KANZ(IMAX)+1
          ELSEIF(KANZGES.GT.KMNGGES) THEN
             KZWI(IMAX)=KANZ(IMAX)-1
          ENDIF
          MNGAEND(IMAX)=.TRUE.
          DO I=1,NF
           VSCH(I)=SCHLUCK*KZWI(I)
          END DO
          CALL CALREI(IVOL,NF,ICHF,EFF,SPEZ,SPLO,
     &                          INF,VSCH,CBA,CLA,RSCH,IER)

C
C
C
C
C        
C
C            Prüfen, ob Grenzen verletzt sind
C
C
C
             DO I=1,NF
               SELECT CASE (IC(LID(I)))
                   CASE (1)
C
C                  >= CL
C
C
                   IF(RSCH(I).LT.CL(LID(I))-DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE (2)
C
C                  >= CU
C
C
                   IF(RSCH(I).GT.CU(LID(I))+DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE (3)
C
C                  >= CU
C
C
                   IF(RSCH(I).LT.CL(LID(I))-DOSEPS.OR.
     &             RSCH(I).GT.CU(LID(I))+DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE DEFAULT
               END SELECT
             END DO
             IF(I.LE.NF) THEN
                CYCLE
             ENDIF
             DO I=1,NEE
               SUA=0.
               DO K=1,NF
                  SUA=SUA+RSCH(K)*AGR(I,LID(K))
               END DO
               II=NF1-1+I
               SELECT CASE (IC(II))
                   CASE (1)
C
C                  >= CL
C
C
                   IF(SUA.LT.CL(II)-DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE (2)
C
C                  >= CU
C
C
                   IF(SUA.GT.CU(II)+DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE (3)
C
C                  >= CU
C
C
                   IF(SUA.LT.CL(II)-DOSEPS.OR.SUA.GT.CU(II)+DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE DEFAULT
               END SELECT
             END DO
             IF(I.LE.NEE) THEN
                CYCLE
             ENDIF
             DO I=1,NF
               KANZ(I)=KZWI(I)
             END DO
        ENDIF
      ENDDO
C
C
C
C
C     VSCH neu berechnen
C
C
C
      DO I=1,NF
         VSCH(I)=SCHLUCK*KANZ(I)
      END DO
C
C
C
C
C     BERECHNUNG DER REINEN FARB-/BINDEMITTELMENGEN RSCH aus VSCH
C 
      CALL CALREI(IVOL,NF,ICHF,EFF,SPEZ,SPLO,
     &                          INF,VSCH,CBA,CLA,RSCH,IER)

      IF(IFEHL(IER).NE.0) GOTO 900

C
      DO I=1,NF
         FAMNG(I)=RSCH(I)
      END DO
C
C
 900  CALL GETFEH(FEHL)
      DEALLOCATE(VSCH,CBA,CLA,SPEZ,EFF,RSCH,KANZ,KZWI,MNGAEND,
     &           ICHF)
c
 999  CALL GETFEH(FEHL)
      RETURN
      END
C
C     Dosierung unter Berücksichtigung des kleinsten Farbabstandes
C
      SUBROUTINE REZANGLD(MNF,IVOLL,INFL,SPLOE,LID,FAMNG,
     &                   PROZ,PROB,EFFL,SPZ,ICHFL,DOSMIN,FEHL)
C
C
      USE MOTFEHL
      USE MODRWRZ,ONLY:D
      USE MODGREN

C
C
C     Reinmengen für Normierungsmengen als Vielfache von DOSMIN
C     Farbabstand soll möglichst klein sein
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      INTEGER(KIND=4):: MNF,IVOLL,INFL,KMNGGES
      INTEGER(KIND=4):: I,J,K,KX
      LOGICAL(KIND=4) ::START,VarmWied,ALLPOS,RESU
      REAL(KIND=4)::SPLOE,DOSMIN
      REAL(KIND=4),DIMENSION(*)::PROZ,PROB,FAMNG,SPZ,EFFL
      INTEGER(KIND=4),DIMENSION(*)::ICHFL,LID
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: KANZ,KK,KANZOPT
      TYPE(TYFEH) FEHL
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: EFF
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: VSCH
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::RSCH
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: CBA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: CLA
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: SPEZ
      REAL(KIND=4),ALLOCATABLE,DIMENSION(:,:,:) :: RCAL
      REAL(KIND=4),ALLOCATABLE,DIMENSION(:,:) :: SOKRIT
      REAL(KIND=4),ALLOCATABLE,DIMENSION(:) :: REMNG
      REAL(KIND=8) :: SCHLUCK,MNGGES,DEVER,DEALT,DE,SUA,DOSEPS
      INTEGER(KIND=4),DIMENSION(:),ALLOCATABLE :: ICHF
C
C
C
      DLL_EXPORT REZANGLD
C
C
      IER=0
      IF(DOSMIN.EQ.0.) THEN
         RETURN
      ENDIF
C
C
      DOSEPS=0.1*DOSMIN
      NF=MNF
      IF(NF.LE.0) THEN
         IER=4043
         IF(IFEHL(IER).NE.0) GOTO 999
      ENDIF
      ALLOCATE(VSCH(NF),CBA(NF),CLA(NF),SPEZ(NF),EFF(NF),
     &         RSCH(NF),KANZ(NF),KK(NF),KANZOPT(NF),
     &         ICHF(NF),STAT=IER)
      IF(IFEHL(IER).NE.0) GOTO 999

C
C 
C     UMSPEICHERN
C      
C
      DO I=1,NF
         RSCH(I)=FAMNG(I)
         SPEZ(I)=SPZ(LID(I))
         ICHF(I)=ICHFL(LID(I))
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
      NWE=NWS()
      KM=KMS()
      MUU=UBOUND(D,1)
      RESU=.FALSE.
C
C
      ALLOCATE(RCAL(NWE,KM,MUU),REMNG(NF),SOKRIT(16,MUU),STAT=IER)
      IF(IFEHL(IER).NE.0) GOTO 999

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
      IF(IFEHL(IER).NE.0) GOTO 900
C
      SCHLUCK=DOSMIN
      MNGGES=0.
      DO I=1,NF
         MNGGES=MNGGES+VSCH(I)
         KANZOPT(I)=0
      ENDDO
      KMNGGES=INT((MNGGES+0.5*SCHLUCK)/SCHLUCK)
      START=.FALSE.
C
C
C     Variationen über alle möglichen Zusammensetzungen
C
C
C
c      OPEN(27,FILE='TESTOUT.TXT')
c      WRITE(27,*) 'LID',(LID(I),I=1,NF)
c      WRITE(27,*) 'NF1',NF1,'NEE',NEE,'NEQ',NEQ
c      WRITE(27,*) 'IC',(IC(I),I=1,NF1-1+NEE)
c      WRITE(27,*) 'CL',(CL(I),I=1,NF1-1+NEE)
c      WRITE(27,*) 'CU',(CU(I),I=1,NF1-1+NEE)
c      DO J=1,NEE
c         WRITE(27,*) 'AGR',(AGR(J,I),I=1,NF1)
c      END DO
      K=5
      KX=INT(K/2)+1
      DeALT=1.E30
      DO J=1,1
C
C
C       Prozedur wird 5 mal wiederholt
C
C
        DO I=1,NF
           KANZ(I)=INT((VSCH(I)+0.5*SCHLUCK)/SCHLUCK)-KX
        END DO
c        WRITE(27,*) 'KANZ',(KANZ(I),I=1,NF)
        DE=1.E30
        DO WHILE (VarmWied(k,NF,KK,Start))
c           WRITE(27,*) 'KK',(KK(I),I=1,NF)
           KGES=0
           ALLPOS=.TRUE.
           DO I=1,NF
             INTFARB=KANZ(I)+KK(I)
             IF(INTFARB.LT.0) THEN
               ALLPOS=.FALSE.
             ENDIF
             KGES=KGES+INTFARB
           END DO
           IF(KGES.NE.KMNGGES.AND..NOT. ALLPOS) THEN
c             WRITE(27,*) 'KGES,KMNGGES',KGES,KMNGGES,'ALLPOS',ALLPOS
             CYCLE
           ENDIF
           DO I=1,NF
                VSCH(I)=SCHLUCK*(KANZ(I)+KK(I))
           END DO
c           WRITE(27,*) 'VSCH',(VSCH(I),I=1,NF)
           CALL CALREI(IVOL,NF,ICHF,EFF,SPEZ,SPLO,
     &                  INF,VSCH,CBA,CLA,RSCH,IER)
c           WRITE(27,*) 'RSCH',(RSCH(I),I=1,NF),'IER',IER
C
C
C
C            Prüfen, ob Grenzen verletzt sind
C
C
C
             
           DO I=1,NF
               SELECT CASE (IC(LID(I)))
                   CASE (1)
C
C                  >= CL
C
C
                   IF(RSCH(I).LT.CL(LID(I))-DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE (2)
C
C                  >= CU
C
C
                   IF(RSCH(I).GT.CU(LID(I))+DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE (3)
C
C                  >= CU
C
C
                   IF(RSCH(I).LT.CL(LID(I))-DOSEPS.OR.
     &             RSCH(I).GT.CU(LID(I))+DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE DEFAULT
               END SELECT
           END DO
           IF(I.LE.NF) THEN
                CYCLE
           ENDIF
           DO I=1,NEE
               SUA=0.
               DO K=1,NF
                  SUA=SUA+RSCH(K)*AGR(I,LID(K))
               END DO
               II=NF1-1+I
               SELECT CASE (IC(II))
                   CASE (1)
C
C                  >= CL
C
C
                   IF(SUA.LT.CL(II)-DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE (2)
C
C                  >= CU
C
C
                   IF(SUA.GT.CU(II)+DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE (3)
C
C                  >= CU
C
C
                   IF(SUA.LT.CL(II)-DOSEPS.OR.SUA.GT.CU(II)+DOSEPS) THEN
                     EXIT
                   ENDIF
                   CASE DEFAULT
               END SELECT
           END DO
           IF(I.LE.NEE) THEN
                CYCLE
           ENDIF
           DO I=1,NF
               REMNG(I)=RSCH(I)
           END DO
           CALL REZMSH(NF,LID,REMNG,NWE,KM,RCAL,SOKRIT,IER)
           DEVER=SOKRIT(5,1)
           DO I=1,NF
                DEVER=DEVER+ABS(KK(I))*0.000001
           END DO
           IF(DE.GT.DEVER) THEN
                 DE=DEVER
                 DO I=1,NF
                    KanzOPT(I)=KANZ(I)+KK(I)
                 END DO
                 RESU=.TRUE.
            ENDIF
        END DO
        DO I=1,NF
           VSCH(I)=SCHLUCK*KANZOPT(I)
        END DO
        IF(ABS(DE-DEALT)<0.0000001) THEN
          EXIT
        ENDIF
        DEALT=DE
      END DO
C
C
      IF(RESU) THEN
C
C
C        BERECHNUNG DER REINEN FARB-/BINDEMITTELMENGEN RSCH aus VSCH
C 
         CALL CALREI(IVOL,NF,ICHF,EFF,SPEZ,SPLO,
     &                          INF,VSCH,CBA,CLA,RSCH,IER)
C
         IF(IFEHL(IER).NE.0) GOTO 900
         DO I=1,NF
            FAMNG(I)=RSCH(I)
         END DO
      ENDIF
c      WRITE(27,*) 'RESU',RESU
c      CLOSE(27)
C
C
 900  CALL GETFEH(FEHL)
      DEALLOCATE(VSCH,CBA,CLA,EFF,SPEZ,RSCH,KANZ,KK,KANZOPT,
     &           ICHF,RCAL,SOKRIT,REMNG)
      RETURN
c
 999  CALL GETFEH(FEHL)
      RETURN
      END

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
c
c
c
c
c
c

