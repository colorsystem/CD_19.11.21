C     Last change: KU 16.08.2021 14:22:23
C
C
C**********************************************************************
C**********************************************************************
C
C           Klaus Unterforsthuber
C           20.06.2021
C
C**********************************************************************
C**********************************************************************
C**********************************************************************
C**********************************************************************
C
      SUBROUTINE CALLIM(IVOL,NF,ICHF,EFF,SPEZ,SPLO,
     &                       INF,VSCHA,INM,LOGZUW,OP,VLIUA,VLIOA,ALIM,
     &                       CBAA,CLAA,RMENG,
     &                       INO,AML,AMH,
     &                       INP,INQ,PROL,PROH,
     &                       IGX,BEL,
     &                       MDIA,MEI,MEQ,MEM,AGR,IGR,BL,BU,
     &                       IND,IER)
C
C
C
C     IVOL = 0   MENGEN
C            1   VOLUMINA
C     NF         ANZAHL FARB-BINDEMITTEL
C     ICHF       ART DER FARB-/BINDEMITTEL (0=FARBMITTEL; 1= BINDEMITTEL usw.)
C     EFF        UMRECHNUNGSFAKTOR (z.B. IN ANZAHL PIGMENTTEILCHEN)
C     SPEZ       SPEZIFISCHES GEWICHT
C     SPLO       SPEZ. GEWICHT DES LOESEMITTELS
C     OP         * ODER +
C     INF        ART DER MENGEN (VOLUMEN)
C                0   BATCHMENGEN
C                1   REINE FARB-/BINDEMITTEL
C                2   FESTSTOFFMENGEN IM FARBMITTELBATCH
C                3   BINDEMITTELMENGEN IM FARBMITTELBATCH
C                4   LOESEMITTELMENGEN IM FARBMITTELBATCH
C                5   BINDE- + LOESEMITTEL IM FARBMITTELBATCH
C                6   NUR FARBMITTELMENGEN
C                
C      VSCHA     MENGEN GEMAESS NORMIERUNG INF
C      INM       ART DER MENGEN (VOLUMEN) FÜR GRENZEN  (BEDEUTUNG WIE INF)
C      VLIUA     ZUG. UNTERE GRENZEN
C      VLIOA     ZUG. OBERE GRENZEN
C      ALIM      MENGEM (ALT) FUER ZUWAGE-BERECHNUNG (LOGZUW=TRUE)
C      CBAA      BATCHKONZENTRATION
C      CLAA      BINDEMITTELKONZENTRATION
C      INO       NORMIERUNGSART (s.VKOEFF) FUER AML UND AMH
C      INP       NORMIERUNGSART FUER PROZENTIGKEIT (ZÄHLER)
C      INQ       NORMIERUNGSART FUER PROZENTIGKEIT (NENNER)
C      AML       DSGL. FUER UNTERE GRENZE
C      AMH       DSGL. FUER OBERE GRENZE
C      PROL      UNTERE GRENZE FUER PROZENTIGKEIT (GEMAESS INP/INQ)
C      PROH      OBERE GRENZE FUER PROZENTIGKEIT (GEMAESS INP/INQ)
C      RMENG     REINE FARB-/BINDEMITTELMENGEN
C      IGX       ART DER MINIMIERUNG
C      BEL       GEWICHT FUER FARB-/BINDEMITTEL 
C
C      MEI       ANZAHL GLEICHUNGEN+Ungleichungen
C      MEQ       DAVON ANZAHL (VON 1 bis MEQ) GLEICHUNGEN
C      MEM       ANZAHL ZUSÄTZLICHER  MINIMIERUNGSBEDINGUNGEN
C
C      AUSGABE GROESSEN
C
C      AGR,BU,BL,IN ANGABEN ÜBER GRENZEN
C      IGR ANGABE AUF WELCHE FARBMITTEL DIE GRENZEN WIRKEN
C
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      LOGICAL LOGZUW
      CHARACTER*1 OP(*)
      DIMENSION EFF(*),SPEZ(*),AC(NF)
      DIMENSION VSCHA(*),VLIUA(*),VLIOA(*),ALIM(*)
      DIMENSION CBAA(*),CLAA(*),BEL(*)
      DIMENSION IGR(MDIA,*),AGR(MDIA,*),BL(*),BU(*),IND(*)
      DIMENSION RMENG(*)
      INTEGER(KIND=4),DIMENSION(*) ::ICHF
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: SPZ ,SPEM,SPEF
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: CBAM,CBAF

      REAL(KIND=8) :: BIG
      DATA TOL/1.E-7/,BIG/1.D80/
C
      ALLOCATE (SPEM(NF),SPEF(NF),CBAM(NF),CBAF(NF),SPZ(NF),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF
C
C
      IER=0
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
C          SPEZ.GEWICHT IN ABHAENGIGKEIT VON VOLUMEN ODER MENGENNORMIERUNG (IVOL)
C
           CALL VOLSPZ(IVOL,NF,EFF,SPEZ,SPZ,SPLO,SLOE,IER)
           IF(IER.NE.0) GOTO 99
C
C
C     BINDEMITTELPOSITION
C
C
      IBI=IBIN(NF,ICHF)
C
C
C
C
C          GRENZEN GEMAESS OPERATOREN (OP)
C

C
C
C          SPEZIFISCHES GEWICHT UND BATCHKONZ.FÜR INM ZUR BERECHNUNG DER GEWUENSCHTEN
C
C          FARB-/BINDEMITTELMENGE(VOLUMEN) FESTLEGEN CBA,SPEZ
C
c           CALL SONSPZ(NF,IBI,INM,IVOL,CBAA,CBA,
c     &                 EFF,SPEZ,SPE,SPLO,SPLE,IER)
c            IF(IER.NE.0) GOTO 99
           CALL SONSPZ(NF,IBI,INM,IVOL,CBAA,CBAM,
     &                  EFF,SPEZ,SPEM,SPLO,SPLE,IER)
           IF(IER.NE.0) GOTO 99
C
           CALL SONSPZ(NF,IBI,INF,IVOL,CBAA,CBAF,
     &                  EFF,SPEZ,SPEF,SPLO,SPLE,IER)
           IF(IER.NE.0) GOTO 99
C
C


C
C
C          GRENZEN GEMAESS OPERATOREN (OP)

C
C
           CALL GREINM(NF,IVOL,IBI,LOGZUW,OP,VLIUA,VLIOA,
     &                 ALIM,SPEM,CBAM,SPEF,CBAF,CLAA,SPLE,MDIA,MEI,
     &                 AGR(1,1),IGR(1,1),BL,BU,IND,IER)

           IF(IER.NE.0) GOTO 99
C
           NFE=NF+MEI+1
           IF(MEI.GT.MDIA) THEN
             IER=4086
             GOTO 99
           ENDIF
C
C
C
C
C          GRENZEN GEMAESS NORMIERUNGSMENGE(VOLUMEN)
C
           IF(INO.GT.0) THEN
                 IF(AMH.LT.TOL) THEN
                    IER=4079
                    GOTO 99
                 ENDIF
                 IF(AML.LT.TOL) THEN
                    AML=0.0001*AMH
                 ENDIF
                 IF(AML.GT.AMH+TOL) THEN
                    IER=4090
                    GOTO 99
                 ENDIF
                 IF(MEI+1.GT.MDIA) THEN
                   IER=4086
                   GOTO 99
                 ENDIF
                 CALL GREINO(NF,ICHF,CBAA,CLAA,SPZ,SLOE,OP,INO,AML,AMH,
     &               IGX,MDIA,NEH,AGR(MEI+1,1),IGR(MEI+1,1),
     &               BL(NFE),BU(NFE),IND(NFE),IER)
                     IF(IER.NE.0) GOTO 99
                     MEI=MEI+NEH
                     NFE=NF+MEI+1
                     IF(MEI.GT.MDIA) THEN
                       IER=4086
                       GOTO 99
                     ENDIF

          ENDIF
C
C          GRENZEN GEMAESS PROZENTIGKEITEN
C
          IF(INP.GT.0.AND.INQ.GT.0) THEN
                   IF(MEI+1.GT.MDIA) THEN
                     IER=4086
                     GOTO 99
                   ENDIF
                   CALL GREINP(NF,ICHF,CBAA,CLAA,SPZ,SLOE,OP,
     &             INP,INQ,PROL,PROH,
     &             MDIA,NEH,AGR(MEI+1,1),IGR(MEI+1,1),
     &             BL(NFE),BU(NFE),IND(NFE),IER)
                   IF(IER.NE.0) GOTO 99
                   MEI=MEI+NEH
                   IF(MEI.GT.MDIA) THEN
                     IER=4086
                     GOTO 99
                   ENDIF
C
C
                   NFE=NF+MEI+1
           ENDIF
           IF(MEI.EQ.0) THEN
              DO I=1,NF
                 IF(IND(I).EQ.3.AND.ABS(BU(I)-BL(I)).LE.TOL) THEN
                    GOTO 100
                 ENDIF
              END DO
              IER=4089
              GOTO 99
           ENDIF
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

           CALL MADJS(NF,ICHF,RMENG,MDIA,MEI,AGR(1,1),BL,BU,IND,IER)
           IF(IER.NE.0) GOTO 99
C
C
C           GLEICHUNGEN WERDEN "NACH VORNE SORTIERT"
C
            DO I=1,MEI-1
             IF(IND(NF+I).NE.3.OR.ABS(BU(NF+I)-BL(NF+I)).GT.TOL
     &                        .OR.IGR(I,NF+1).NE.0) THEN
             DO J=I+1,MEI   
              IF(IND(NF+J).EQ.3.AND.ABS(BU(NF+J)-BL(NF+J)).LT.TOL
     &                         .AND.IGR(J,NF+1).EQ.0) THEN
C
C               VERTAUSCHEN
C                
                IV=NF+I
                JV=NF+J
                BB=BU(IV)
                BU(IV)=BU(JV)
                BU(JV)=BB   
                BB=BL(IV)
                BL(IV)=BL(JV)
                BL(JV)=BB    
                IK=IND(IV)
                IND(IV)=IND(JV)
                IND(JV)=IK
                DO L=1,NF+1
                   III=IGR(I,L)
                   AAA=AGR(I,L)
                   IGR(I,L)=IGR(J,L)
                   AGR(I,L)=AGR(J,L)
                   IGR(J,L)=III
                   AGR(J,L)=AAA
                ENDDO
                EXIT
              ENDIF
             ENDDO
             ENDIF
            ENDDO
C   
C           MEQ BESTIMMEN
C
            MEQ=0
            DO I=1,MEI
             IF(IND(NF+I).EQ.3.AND.ABS(BU(NF+I)-BL(NF+I)).LT.TOL
     &                        .AND.IGR(I,NF+1).EQ.0) THEN
               MEQ=MEQ+1
               AGR(MEQ,NF+1)=BU(NF+I)
             ENDIF
            ENDDO
C
C
C
C           SUMME(AGR(I)*M(I))>=0. ZULASSEN
C
C
C
            K=0
            DO I=MEQ+1,MEI
               IF(IND(NF+I).EQ.1) THEN
                 AGR(I,NF+1)=BL(NF+I)
                 BU(NF+I)=BIG
               ENDIF
               IF(IND(NF+I).EQ.2) THEN
                 DO J=1,NF
                    AGR(I,J)=-AGR(I,J)
                 END DO
                 AGR(I,NF+1)=-BU(NF+I)
                 IND(NF+I)=1
                 BL(NF+I)=-BU(NF+I)
                ENDIF
               IF(IND(NF+I).EQ.3) THEN
                 K=K+1
                 DO J=1,NF
                    AGR(MEI+K,J)=-AGR(I,J)
                    IGR(MEI+K,J)=IGR(I,J)
                 END DO
                 IGR(MEI+K,NF+1)=ABS(IGR(I,NF+1))
                 AGR(MEI+K,NF+1)=-BU(NF+I)
                 IND(NF+MEI+K)=1
                 BL(NF+MEI+K)=-BU(NF+I)
                 BU(NF+MEI+K)=BIG
C
C
                 IND(NF+I)=1
                 AGR(I,NF+1)=BL(NF+I)
                 BU(NF+I)=BIG
                ENDIF
            END DO
            MEI=MEI+K
C

           CALL MADFEA(NF,AC,MDIA,MEI,AGR,BL,BU,IND,IER)
           IF(IER.NE.0) GOTO 99
C
C
C
C          MINIMIERUNGSBEDINGUNGEN FUER MINFUN IN ABHAENGIGKEIT VON IGX
C    	   FARB-/BINDEMITTELMENGE(VOLUMEN) FESTLEGEN CBA,SPEZ
C
c 100       CALL SONSPZ(NF,IBI,INF,IVOL,CBAA,CBA,
c     &                 EFF,SPEZ,SPE,SPLO,SPLE,IER)
c           IF(IER.NE.0) GOTO 99
C
   
 100       CALL GREMIN(NF,IBI,VSCHA,CBAF,CLAA,SPEF,SPLE,OP,VLIOA,
     &            IGX,BEL,MDIA,NEH,AGR(MEI+1,1),IGR(MEI+1,1),IER)
           IF(IER.NE.0) GOTO 99
           MEM=NEH
C
C
C
C
           DO I=1,NF
              IF(IND(I).EQ.3.AND.ABS(BL(I)-BU(I)).LE.TOL) THEN
                RMENG(I)=BL(I)
              ENDIF
              IF (IND(I).EQ.3.OR.IND(I).EQ.1) THEN
                IF(RMENG(I).LT.BL(I)) THEN
                  RMENG(I)=BL(I)
                ENDIF
              ENDIF
              IF (IND(I).EQ.3.OR.IND(I).EQ.2) THEN
                IF(RMENG(I).GT.BU(I)) THEN
                  RMENG(I)=BU(I)
                ENDIF
              ENDIF
           END DO
C
C
C
C
C
C
 99   DEALLOCATE (CBAM,SPEM,CBAF,SPEF,SPZ)
      RETURN
      END
C
C
C
      SUBROUTINE GREINM(NF,IVOL,IBI,LOGZUW,CHL,ALIU,ALIO,
     &                 ALIM,SPEM,CBAM,SPEF,CBAF,CLAA,SPLE,
     &                 MDIM,NEE,AGR,IGR,CL,CU,IC,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C

      DIMENSION ALIM(*),ALIO(*),ALIU(*)
      DIMENSION SPEM(*),CBAM(*),SPEF(*),CBAF(*)
      DIMENSION CLAA(*)
      DIMENSION AGR(MDIM,*),IGR(MDIM,*),CL(*),CU(*),IC(*)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: AKOE
      CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:):: GRE
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:):: IRE
      
C
C
      LOGICAL LOGZUW
      CHARACTER*(*) CHL(*)
      CHARACTER*1 BLA,STR,STT,STI
      DATA BLA/' '/,STR/'/'/,STI/'\'/
      DATA RANGEX/1.E35/
C
C
C     GRENZEN 
C     GLEICHUNGEN UND UNGLEICHUNGEN FUER REINE FARB/BINDEMITTELMENGEN
C     NORMIERT GEMAESS NORM AUF AMENG    
C
C     ALIM UND ALIU SIND BATCHMENGEN (BZW. VOLUMINA)
C     DURCH ENTSPRECHENDE WAHL VON SPE,CLI,CBA UND SPLE KANN DIE BEDEUTUNG VON
C     ALIM UND ALIU VERAENDERT WERDEN (Z.B. FUER SPE=1. UND SPLE=1. LIEGEN
C     STETS MENGEN VOR).
C     
      ALLOCATE(AKOE(NF),GRE(NF),IRE(NF),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF
C
C
      DO I=1,MDIM
         DO J=1,NF+1
            AGR(I,J)=0.
            IGR(I,J)=0
         ENDDO
      ENDDO
      IER=0
      NEE=0
      JNF=0
      DO I=1,NF
        IF (LOGZUW) THEN
           CL(I)=ALIM(I)
        ELSE
           CL(I)=0.
        END IF
        CU(I)=RANGEX
        IC(I)=1
      END DO
      DO 10 I=1,NF
C
C
C     GRENZEN FUER BINDEMITTELMENGE    
C
C     BINDEMITTELLIMITIERUNG WIRD IMMER ALS GLEICHUNG AUFGEBAUT, DA BINDEMITTEL AUCH IN FARBMITTELBATCHES ENTHALTEN IST
C
C
C
C
C
      IF(I.EQ.IBI) THEN      
             IC(I)=4
             CL(I)=0.
             CU(I)=RANGEX
             NEE=NEE+1
             IF(NEE.GT.MDIM) GOTO 899
             JNF=NEE
             DO J=1,NF
                IF(J.EQ.IBI) THEN
                     AGR(NEE,J)=1.0
                     IGR(NEE,J)=1
                ELSE
                     AGR(NEE,J)=0.0
                     IGR(NEE,J)=0
                ENDIF
             ENDDO
                  IC(NF+NEE)=1
                  CL(NF+NEE)=0.
                  CU(NF+NEE)=RANGEX
             IF(LOGZUW.OR.CHL(I).NE.' ') THEN
               NEE=NEE+1
               DO J=1,NF
                 IF(J.EQ.IBI) THEN
                     AGR(NEE,J)=1.0
                     IGR(NEE,J)=1
                 ELSE
                     AGR(NEE,J)=0.0
                     IGR(NEE,J)=0
                 ENDIF
                 IC(NF+NEE)=1
                 CL(NF+NEE)=0.
                 CU(NF+NEE)=RANGEX
               ENDDO
               IF(LOGZUW) THEN

C
C
C               GRENZEN FUER ZUWAAGE-OPTIMIERUNG
C
C
             	IF(CHL(I).EQ.'=') THEN
                  IC(NF+NEE)=3
                  CL(NF+NEE)=MAX(ALIU(I),ALIM(I))
                  CU(NF+NEE)=MAX(ALIO(I),ALIM(I))
                ELSE IF(CHL(I).EQ.'<') THEN
                  IC(NF+NEE)=3
                  CU(NF+NEE)=MAX(ALIO(I),ALIM(I))
                  CL(NF+NEE)=MAX(ALIU(I),ALIM(I))
                ELSE IF(CHL(I).EQ.'>') THEN
                  IC(NF+NEE)=1
                  CL(NF+NEE)=MAX(ALIU(I),ALIM(I))
                  CU(NF+NEE)=RANGEX
                ELSE
                  IC(NF+NEE)=1
                  CL(NF+NEE)=ALIM(I)
                  CU(NF+NEE)=RANGEX
                ENDIF
             ELSE
             	IF(CHL(I).EQ.'=') THEN
                  IC(NF+NEE)=3
                  CL(NF+NEE)=ALIU(I)
                  CU(NF+NEE)=ALIO(I)
                ELSE IF(CHL(I).EQ.'<') THEN
                  IC(NF+NEE)=3
                  CU(NF+NEE)=ALIO(I)
                  CL(NF+NEE)=ALIU(I)
                ELSE IF(CHL(I).EQ.'>') THEN
                  IC(NF+NEE)=1
                  CL(NF+NEE)=ALIU(I)
                  CU(NF+NEE)=ALIO(I)
                ELSE
                  IC(NF+NEE)=1
                  CL(NF+NEE)=0.
                  CU(NF+NEE)=RANGEX
                ENDIF
             ENDIF
           ENDIF
           GOTO 10
      ENDIF
C
C
C     
C     :   -    ZEICHEN FARBMITTEL:BINDEMITTEL = const
C
C
      IF(CHL(I).EQ.':') THEN
             IF(IBI.EQ.0.OR.ALIO(IBI).EQ.0.) GOTO 900
             NEE=NEE+1
             IF(NEE.GT.MDIM) GOTO 899
             DO  J=1,NF 
                    IF(J.EQ.IBI) THEN
                       AGR(NEE,J)=-ALIO(I)/ALIO(IBI)
                       IGR(NEE,J)=1
                    ELSE IF(J.EQ.I) THEN
                       AGR(NEE,J)=1.
                       IGR(NEE,J)=1
                    ELSE
                       AGR(NEE,J)=0.
                    ENDIF
             ENDDO
             CL(NF+NEE)=0.
             CU(NF+NEE)=0.
             IC(NF+NEE)=3
             GOTO 10
C
C
C
C     
C     =   -  ZEICHEN FARBMITTELMENGE = const
C
C
      ELSE IF(CHL(I).EQ.'=') THEN
             IF(LOGZUW) THEN
             	CL(I)=MAX(ALIU(I),ALIM(I))
                CU(I)=MAX(ALIO(I),ALIM(I))
             	IC(I)=3
             ELSE
             	CL(I)=ALIU(I)
                CU(I)=ALIO(I)
             	IC(I)=3
             ENDIF
C
C     
C     < -    ZEICHEN FARBMITTELMENGE < const
C
C
      ELSE IF(CHL(I).EQ.'<') THEN
             IF(LOGZUW) THEN
             	CU(I)=MAX(ALIO(I),ALIM(I))
                CL(I)=MAX(ALIU(I),ALIM(I))
             	IC(I)=3
             ELSE
                CU(I)=ALIO(I)
                CL(I)=ALIU(I)
                IC(I)=3
             ENDIF
      
C
C     

      
C
C     
C     > -    ZEICHEN FARBMITTELMENGE > KONST.
C
C
      ELSE IF(CHL(I).EQ.'>') THEN
             IF(LOGZUW) THEN
               CL(I)=MAX(ALIU(I),ALIM(I))
               CU(I)=RANGEX
               IC(I)=1
             ELSE
               CL(I)=ALIU(I)
               CU(I)=RANGEX
               IC(I)=1
             ENDIF
      ELSE
             IF(LOGZUW) THEN
               CL(I)=ALIM(I)
               CU(I)=RANGEX
               IC(I)=1
             ELSE
               CL(I)=ALIU(I)
               CU(I)=RANGEX
               IC(I)=1
             ENDIF
      ENDIF
  10  CONTINUE
C     
C
C
C     LIMITIERUNG BEI SCHRAEGSTRICH (VERHAELTNIS WIRD FESTGEHALTEN)
C
      IK=0 
      STT=BLA
C
      DO I=1,NF
C
C        ABFRAGE, OB SCHRAEGSTRICH       
C
         IF(CHL(I).EQ.STR) THEN
            IF(STT.EQ.STR) THEN
            IF(ALIO(IK).EQ.0.) GOTO 900
C
C                 SCHRAEGSTRICH WURDE BEREITS GEFUNDEN
C
                  NEE=NEE+1
                  AGR(NEE,I)=1.
                  AGR(NEE,IK)=-ALIO(I)/ALIO(IK)
                  IGR(NEE,I)=1
                  IGR(NEE,IK)=1
                  CU(NF+NEE)=0.
                  CL(NF+NEE)=0.
                  IC(NF+NEE)=3
            ELSE                 
C
C           SCHRAEGSTRICH ZUM ERSTEN MAL GEFUNDEN
C
C
                  IK=I
                  STT=STR
            ENDIF
         ENDIF
      ENDDO
C     
C
C     
C
C
C     LIMITIERUNG BEI BACKSLASH (\-MENGEN/GESAMTLIMITIERUNGSMENGE>=const
C
      IK=0 
      STT=BLA
C
      SUU=0.
      DO I=1,NF
         SUU=SUU+ALIO(I)
      END DO
      DO I=1,NF
C
C        ABFRAGE, OB BACKSLASH       
C
         IF(CHL(I).EQ.STI) THEN
            IF(SUU.EQ.0.)  GOTO 900
            NEE=NEE+1
            FAKT=ALIO(I)/SUU
            DO J=1,NF
               AGR(NEE,J)=-FAKT
               IGR(NEE,J)=1
            END DO
            AGR(NEE,I)=1.+ AGR(NEE,I)
            IC(NF+NEE)=3
            CL(NF+NEE)=0.
            CU(NF+NEE)=0.
         ENDIF
      ENDDO
C
C
C
C     LIMITIERUNG BEI GLEICHEN BUCHSTABEN
C
      NEG=0
      DO J=1,NF
         GRE(J)=BLA
      ENDDO
C
C
C        ABFRAGE, OB KLEIN-BUCHSTABEN (SUMME aller gleicher KLEIN-BUCHSTEABEN
C         oder aller gleicher Ziffern  wird limitiert)
C

      DO I=1,NF
         ICCC=ICHAR(CHL(I))
         IF((ICCC.GE.97.AND.ICCC.LE.122)
     &       .OR.(ICCC.GE.49.AND.ICCC.LE.57)) THEN


            DO K=1,NEG 
               ICCG=ICHAR(GRE(K))
               IF(ICCG.EQ.ICCC) THEN
C
C                 KLEINBUCHSTABE oder Ziffer WURDE BEREITS GEFUNDEN
C
                  AGR(NEE+K,I)=1.
                  IGR(NEE+K,I)=1
                  GOTO 20
               ENDIF                 
            ENDDO
C
C           KLEIN-BUCHSTABE ODER Ziffern  ZUM ERSTEN MAL GEFUNDEN
C
C
            NEG=NEG+1           
            GRE(NEG)=CHL(I) 
            AGR(NEE+NEG,I)=1.
            IGR(NEE+NEG,I)=1
            CU(NF+NEE+NEG)=0.
            DO L=1,NF
            IF(CHL(L).EQ.GRE(NEG)) THEN
               CU(NF+NEE+NEG)=CU(NF+NEE+NEG)+ALIO(L)
            ENDIF
            ENDDO
            CL(NF+NEE+NEG)=CU(NF+NEE+NEG)
C
C           BUCHSTABE a-h SUMME < LIMITIERUNG
C
            IF(ICCC.GE.97.AND.ICCC.LE.104) THEN
              IC(NF+NEE+NEG)=2
            ENDIF
C
C           BUCHSTABE i-q SUMME = LIMITIERUNG oder Ziffer 1-9 Summe=Limitierung
C
            IF((ICCC.GE.105.AND.ICCC.LE.113)
     &        .OR.(ICCC.GE.49.AND.ICCC.LE.57)) THEN
              IC(NF+NEE+NEG)=3
            ENDIF
C
C           BUCHSTABEN r-z SUMME > LIMITIERUNG
C
            IF(ICCC.GE.114.AND.ICCC.LE.122) THEN
              IC(NF+NEE+NEG)=1
            ENDIF
         ENDIF
  20  ENDDO
C
C        ABFRAGE, OB GROSS-BUCHSTABEN (Verhältnis gleicher GROSS-BUCHSTABEN wird festgehalten)
C
      NEF=0
      DO J=1,NF
         GRE(J)=BLA
         IRE(J)=0
      ENDDO

      DO I=1,NF
         ICCC=ICHAR(CHL(I))
C
C
         IF(ICCC.GE.65.AND.ICCC.LE.90)THEN


            DO K=1,NEF
               ICCG=ICHAR(GRE(K))
               IF(ICCG.EQ.ICCC) THEN
C
C                 BUCHSTABE WURDE BEREITS GEFUNDEN (Verhältnis wird festhalten)
C
                  AGR(NEE+NEG+K,I)=ALIO(IRE(K))
                  AGR(NEE+NEG+K,IRE(K))=-ALIO(I)
                  IGR(NEE+NEG+K,I)=1
                  GOTO 30
               ENDIF                 
            ENDDO
C
C           GROSS-BUCHSTABE  ZUM ERSTEN MAL GEFUNDEN
C
C
            NEF=NEF+1
            GRE(NEF)=CHL(I)
            IRE(NEF)=I
            CU(NF+NEE+NEG+NEF)=0.
            CL(NF+NEE+NEG+NEF)=0.
            IC(NF+NEE+NEG+NEF)=3
         ENDIF
  30  ENDDO

C
C     UMRECHNUNG DER FABMITTELLIMITIERUNGEN FUER BATCHVOLUMINA IN SOLCHE 
C     FUER REINE FARBMITTELMENGEN. Gilt NICHT FÜR BINDEMITTEL, DESHALB
C     LIMITIERUNG FUER BINDEMITTEL DURCH IC(IBI)=4 DEAKTIVIERT UND IN FORM EINER GLEICHUNG (s.u.) ANGEGEBEN.
C
C

      DO I=1,NF
         DO J=1,NF
            AKOE(J)=0.
         ENDDO
         AKOE(I)=1.
         CALL WKOEF(NF,IBI,CBAM,CLAA,SPEM,SPLE,AKOE,IER)
         IF(IER.NE.0) GOTO 99
         CL(I)=CL(I)*AKOE(I)
         CU(I)=CU(I)*AKOE(I)
      ENDDO
      NEE=NEE+NEG+NEF
C
C     DIE FUER BATCHVOLUMINA BZW. LIMITIERUNGEN ('=','>' usw.)GUELTIGEN MATRIXELEMENTE
C     FUER GLEICHUNGEN UND UNGLEICHUNGEN WERDEN IN SOLCHE 
C     FUER REINE FARB-/BINDEMITTELMENGEN UMGERECHNET. DIE GRENZEN (CL UND CU)
C     BLEIBEN UNVERAENDERT.
C     
      DO J=1,NEE
C
         DO I=1,NF
            AKOE(I)=AGR(J,I)
         ENDDO
         IF(J.EQ.JNF) THEN
C          Matrix-Koeffizienten für Batches (Pasten)
           CALL UKOEF(NF,IBI,CBAF,CLAA,SPEF,SPLE,AKOE,IER)
         ELSE
C          Matrixkoeffizienten für Limitierungsmengen (ALIU,ALIO,ALIM)
           CALL UKOEF(NF,IBI,CBAM,CLAA,SPEM,SPLE,AKOE,IER)
         ENDIF
         IF(IER.NE.0) GOTO 99
         DO I=1,NF
            AGR(J,I)=AKOE(I)
         ENDDO
      ENDDO
C
C
  99  DEALLOCATE(AKOE,GRE,IRE)
      RETURN
C
C
C     FEHLERMELDUNGEN
C
C
 899  IER=4086
      GOTO 99 
 900  IER=4087
      GOTO 99
      END
      SUBROUTINE CALREI(IVOL,NF,ICHF,EFF,SPEZ,SPLO,
     &                          INF,VSCHA,CBAA,CLAA,RMENG,IER)
C
C     BERECHNUNG DER REINEN FARB-/BINDEMITTELMENGEN (RMENG) AUS VORGEG.
C     MENGEN (VOLUMEN) VSCHA GEMAESS NORMIERUNG INF
C
C     IVOL = 0   MENGEN
C            1   VOLUMINA
C     NF         ANZAHL FARB-BINDEMITTEL
C     ICHF       ART DER FARB-/BINDEMITTEL (0=FARBMITTEL; 1= BINDEMITTEL usw.)
C     SPEZ       SPEZIFISCHES GEWICHT
C     SPLO       SPEZ. GEWICHT DES LOESEMITTELS
C     OP         * ODER + 
C     INF        ART DER MENGEN (VOLUMEN)
C                0   BATCHMENGEN
C                1   REINE FARB-/BINDEMITTEL
C                2   FESTSTOFFMENGEN IM FARBMITTELBATCH
C                3   BINDEMITTELMENGEN IM FARBMITTELBATCH
C                4   LOESEMITTELMENGEN IM FARBMITTELBATCH
C                5   BINDE- + LOESEMITTEL IM FARBMITTELBATCH
C                6   NUR FARBMITTELMENGEN
C                
C      VSCHA     MENGEN GEMAESS NORMIERUNG INF
C      CBAA      BATCHKONZENTRATION
C      CLAA      BINDEMITTELKONZENTRATION
C
C      AUSGABE GROESSEN
C
C      RMENG     REINE FARB-/BINDEMITTELMENGEN (WERDEN BERECHNET)

C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      DIMENSION SPEZ(*),EFF(*)
      DIMENSION VSCHA(*),CBAA(*),CLAA(*)
      DIMENSION RMENG(*)   
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: SPE
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: CBA
      INTEGER(KIND=4),DIMENSION(*)::ICHF
C
      ALLOCATE (SPE(NF),CBA(NF),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF
      IER=0
C
C     BINDEMITTELPOSITION
C
C
C     Pruefen, ob nur ein Bindemittel vorhanden
C
C
C
C
      IEE=0
      IBI=IBIN(NF,ICHF)
      DO I=1,NF
        IF(I.NE.IBI) THEN
          IF(ICHF(I).EQ.1) THEN
C           Zwei Bindemittel
            IEE=-4161
            EXIT
          ENDIF
        ENDIF
      END DO
C
C
C     SPEZIFISCHES GEWICHT UND BATCHKONZ. ZUR BERECHNUNG DER GEWUENSCHTEN 
C     REINEN FARB-/BINDEMITTELMENGE (RMENG) FESTLEGEN (ALT) CBA,SPE
C
C   
      CALL SONSPZ(NF,IBI,INF,IVOL,CBAA,CBA,EFF,SPEZ,SPE,SPLO,SPLE,IER)
      IF(IER.NE.0) GOTO 99
C
C     REINE FARB-/BINDEMITTEL AUS NORMIERUNGSMENGEN BERECHNEN  
C 
      CALL VOLREI(NF,IBI,RMENG,VSCHA,CBA,CLAA,SPE,SPLE,IER)
      IF(IER.NE.0) GOTO 99
C
C
C           
C
 99   DEALLOCATE (CBA,SPE)
      IF(IEE.NE.0.AND.IER.EQ.0) THEN
        IER=IEE
      ENDIF
      RETURN
      END
C
C
C
      SUBROUTINE CALMNG(IVOL,NF,ICHF,EFF,SPEZ,SPLO,
     &                          INF,VSCHN,CBAA,CLAA,RSCH,IER)
C
C     BERECHNUNG DER NORMIERUNGSMENGEN (VSCHN)) AUS
C     REINEN FARB-/BINDEMITTELMENGEN (RMENG)
C     (GEMAESS NORMIERUNG INF
C
C     IVOL = 0   MENGEN
C            1   VOLUMINA
C     NF         ANZAHL FARB-BINDEMITTEL
C     ICHF       ART DER FARB-/BINDEMITTEL (0=FARBMITTEL; 1= BINDEMITTEL usw.)
C     SPEZ       SPEZIFISCHES GEWICHT
C     SPLO       SPEZ. GEWICHT DES LOESEMITTELS
C     OP         * ODER + 
C     INF        ART DER MENGEN (VOLUMEN)
C                0   BATCHMENGEN
C                1   REINE FARB-/BINDEMITTEL
C                2   FESTSTOFFMENGEN IM FARBMITTELBATCH
C                3   BINDEMITTELMENGEN IM FARBMITTELBATCH
C                4   LOESEMITTELMENGEN IM FARBMITTELBATCH
C                5   BINDE- + LOESEMITTEL IM FARBMITTELBATCH
C                6   NUR FARBMITTELMENGEN
C                
C      VSCHN      MENGEN GEMAESS NORMIERUNG INF (WERDEN BERECHNET)
C      CBAA      BATCHKONZENTRATION
C      CLAA      BINDEMITTELKONZENTRATION
C
C      AUSGABE GROESSEN
C
C      RSCH     REINE FARB-/BINDEMITTELMENGEN

C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      DIMENSION SPEZ(*),EFF(*)
      DIMENSION VSCHN(*),CBAA(*),CLAA(*)
      DIMENSION RSCH(*)
      INTEGER(KIND=4),DIMENSION(*) ::ICHF
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: SPE
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: CBA
C
      ALLOCATE (SPE(NF),CBA(NF),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF
      IER=0
C
C     BINDEMITTELPOSITION
C
C
      IBI=IBIN(NF,ICHF)
C
C
C     SPEZIFISCHES GEWICHT UND BATCHKONZ. ZUR BERECHNUNG DER GEWUENSCHTEN
C     FARB-/BINDEMITTELMENGE(VOLUMEN) FESTLEGEN (NEU) CBA,SPEN
C
      CALL SONSPZ(NF,IBI,INF,IVOL,CBAA,CBA,EFF,SPEZ,SPE,SPLO,SPLE,IER)
      IF(IER.NE.0) GOTO 99
C
C
C
C
C
C     BATCHVOLUMEN (VSCHN) AUS RMENG BERECHNEN
C
      CALL REIVOL(NF,IBI,RSCH,VSCHN,CBA,CLAA,SPE,SPLE,IER)
      IF(IER.NE.0) GOTO 99

C
C
C
C           
C
 99   DEALLOCATE (CBA,SPE)
      RETURN
      END
C

C 
C
C 
      
      SUBROUTINE CALMNX(IVOL,NF,ICHF,EFF,SPEZ,SPLO,OP,
     &                          CBAA,CLAA,RMENG,  
     &                          AMR,INO,AMM,
     &                          INP,INQ,AMZA,AMNE,IER)
C
C     BERECHNUNG DER NORMIERUNGSMENGEN AMR,AMM,AMZA,AMNE
C     AUS REINEN MENGEN
C
C     IVOL = 0   MENGEN
C            1   VOLUMINA
C     NF         ANZAHL FARB-BINDEMITTEL
C     ICHF       ART DER FARB-/BINDEMITTEL (0=FARBMITTEL; 1= BINDEMITTEL usw.)
C     SPEZ       SPEZIFISCHES GEWICHT
C     SPLO       SPEZ. GEWICHT DES LOESEMITTELS
C     OP         * ODER + 
C                
C      CBAA      BATCHKONZENTRATION
C      CLAA      BINDEMITTELKONZENTRATION
C      INO       NORMIERUNGSART (s.VKOEFF) FUER AML UND AMH
C      INP       NORMIERUNGSART FUER PROZENTIGKEIT ZÄHLER
C      INQ       NORMIERUNGSART FUER PROZENTIGKEIT NENNER

C
C      AUSGABE GROESSEN
C
C      AMR       REINE FARB-/BINDEMITTEL (GESAMT)
C      AMM       MENGE (GEMAESS NORMIERUNSART INO
C      AMZA      MENGE (ZAEHLER) ZUR BERECHNUNG DER AKTUELLEN PROZENTIGKEIT
C      AMNE      MENGE (NENNER) ZUR BERECHNUNG DER AKTUELLEN PROZENTIGKEIT
C                AKTUELLE PROZENTIGKEIT=AMZA/AMNE

C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      DIMENSION SPEZ(*),EFF(*)
      DIMENSION CBAA(*),CLAA(*)
      DIMENSION RMENG(*)
      INTEGER(KIND=4),DIMENSION(*) ::ICHF
      CHARACTER*1 OP(*)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: SPZ
C
C
      ALLOCATE (SPZ(NF),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF
      IER=0
C
C     BINDEMITTELPOSITION
C
C
      IBI=IBIN(NF,ICHF)
C
C     SPEZ.GEWICHT IN ABHAENGIGKEIT VON VOLUMEN ODER MENGENNORMIERUNG (IVOL)
C
C
      CALL VOLSPZ(IVOL,NF,EFF,SPEZ,SPZ,SPLO,SLOE,IER)
      IF(IER.NE.0) GOTO 99
C
C
C     REINE FARB-/BINDEMITTEL (GESAMT)
C
C
      AMR=0.
      DO I=1,NF
         AMR=AMR+RMENG(I)
      ENDDO

C
C     BERECHNUNG DER NORMIERUNGSMENGE GEMAESS INO
C
C          
C
      IF(INO.GT.0) THEN
         CALL VGVOL(NF,ICHF,RMENG,CBAA,CLAA,SPZ,SLOE,OP,
     &              AMM,INO,IER)
         IF(IER.NE.0) GOTO 99
      ELSE
         AMM=1.
      ENDIF      
C
C
C
C      BERECHNUNG DER ZUR BERECHNUNG DER PROZENTIGKEIT NOTWENDIGEN
C      MENGEN
C
C
C
C          
C
                 
      IF(INP.GT.0.AND.INQ.GT.0) THEN
         CALL TEINPQ(IBI,INP,INQ,IER)
C        CALL KINP(IBI,INP,INOO,INOU,IER)
         IF(IER.NE.0) GOTO 99
         CALL VGVOL(NF,ICHF,RMENG,CBAA,CLAA,SPZ,SLOE,OP,
     &              AMZA,INP,IER)
         IF(IER.NE.0) GOTO 99
         IER=0 
         CALL VGVOL(NF,ICHF,RMENG,CBAA,CLAA,SPZ,SLOE,OP,
     &              AMNE,INQ,IER)
         IF(IER.NE.0) GOTO 99
      ELSE
         AMZA=0.
         AMNE=1.
      ENDIF
C
C
C
C           
C
 99   DEALLOCATE (SPZ)
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
C
C
C
C
C
      SUBROUTINE GREMIN(NF,IBI,VSCHA,CBA,CLI,SPE,SPLE,
     &                 OP,VLIOA,IGX,BEL,
     &                 MDIM,NEE,AGR,IGR,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      DIMENSION VSCHA(*),VLIOA(*)
      DIMENSION SPE(*),CLI(*),CBA(*),BEL(*)
      CHARACTER*1 OP(*)
      DIMENSION AGR(MDIM,*),IGR(MDIM,*)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: AKOE
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: RMENG
C

C
C     "GLEICHUNGEN" FUER MINIMIERUNGSBEZIEHUNGEN (S. SUBROUTINE MINFUN)
C      BL = LIMITIERUNGSMENGE (VOLUMEN)
C      BU = AKTUELLE MENGE (VOLUMEN)
C         

C 
C      GRENZEN FUER MINIMIERUNG ANHAND VON IGX   
C      DIE MENGEN (BZW. VOLUMENANGABEN) ENTSPRECHEND INF
C
C
      NEE=0
      IF(IGX.LT.0) THEN
         RETURN
      ENDIF
C
      ALLOCATE(AKOE(NF),RMENG(NF),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF     
C             
      IER=0
      CALL VOLREI(NF,IBI,RMENG,VSCHA,CBA,CLI,SPE,SPLE,IER)
      IF(IER.NE.0) GOTO 99
C
C     "GLEICHUNGEN" FUER MINIMIERUNGSBEZIEHUNGEN (S. SUBROUTINE MINFUN)
C         

C 
C      GRENZEN FUER MINIMIERUNG ANHAND VON IGX   
C      DIE MENGEN (BZW. VOLUMENANGABEN) ENTSPRECHEND INF
C
C
      SELECT CASE(IGX)
        CASE(0)
C
C         MINIMIERUNG DES FARBABSTANDES
C
        CASE(1)
C
C         
C
C         MINIMIERUNG DES FARBABSTANDES BEI MINIMALER BINDEMITTELMENGE
C
          IF(IBI.EQ.0) THEN
             IER=4102
             GOTO 99
          ENDIF          
          NEE=NEE+1
          DO I=1,NF
             AGR(NEE,I)=0.
             IGR(NEE,I)=0
          ENDDO
          DO I=1,NF
             IF(I.EQ.IBI) THEN             
                AGR(NEE,I)=BEL(I)
                IGR(NEE,I)=1
             ENDIF
          ENDDO
          AGR(NEE,NF+1)=0.
          IGR(NEE,NF+1)=0
C
C
        CASE(2)
C
C         MINIMIERUNG DES FARBABSTANDES BEI MINIMALER FARBMITTELMENGE
C
          IF(IBI.EQ.0) THEN
             IER=4102
             GOTO 99
          ENDIF          
C
          NEE=NEE+1
          DO I=1,NF
             AGR(NEE,I)=0.
             IGR(NEE,I)=0
          ENDDO
          DO I=1,NF
             IF(I.NE.IBI) THEN             
                AGR(NEE,I)=BEL(I)
                IGR(NEE,I)=1
             ENDIF
          ENDDO
          AGR(NEE,NF+1)=0.
          IGR(NEE,NF+1)=0
                    
C
C
        CASE(3)
C          
C                
C         MINIMIERUNG DES FARBABSTANDES BEI VORGEGEBENER ZUWAAGE
C
C         Darüber hinaus weden die OBEREN UND UNTEREN GRENZEN IN SUBROUTINE GREINO FESTGELEGT
C
C
          NEE=NEE+1
          SUMM=0.D0
          DO J=1,NF
             AGR(NEE,J)=1.
             IGR(NEE,J)=1
             SUMM=SUMM+VSCHA(J)
          END DO
          IGR(NEE,NF+1)=0
          AGR(NEE,NF+1)=SUMM
C
C
C
        CASE(4)
C          
C                
C         MINIMIERUNG DES FARBABSTANDES BEI MINIMALER MENGENABWEICHUNG
C
C 
          NEE=NF
          DO I=1,NEE
             DO J=1,NF
                IF(I.EQ.J) THEN
                   AGR(I,J)=BEL(I)
                   IGR(I,J)=1
                ELSE
                   AGR(I,J)=0.
                   IGR(I,J)=0
                ENDIF
             ENDDO
             AGR(I,NF+1)=BEL(I)*VSCHA(I)
             IGR(I,NF+1)=0
          ENDDO
          
C
        CASE(5)
C          
C                
C         MINIMIERUNG DER ZUWAAGE BEI VORGEG. FARBABSTAND DTO
C
C
C         OBERE UND UNTERE GRENZE WERDEN IN SUBROUTINE GREINO FESTGELEGT
          NEE=NEE+1
          SUMM=0.D0
          DO J=1,NF
             AGR(NEE,J)=1.
             IGR(NEE,J)=1
             SUMM=SUMM+VSCHA(J)
          END DO
          IGR(NEE,NF+1)=0
          AGR(NEE,NF+1)=SUMM
C
C
        CASE(6)
C          
C                
C         MINIMIERUNG DER MENGENABWEICHUNG BEI VORGEG. FARBABSTAND DTO
C
C 
          NEE=NF
          DO I=1,NEE
             DO J=1,NF
                IF(I.EQ.J) THEN
                   AGR(I,J)=BEL(I)
                   IGR(I,J)=1
                ELSE
                   AGR(I,J)=0.
                   IGR(I,J)=0
                ENDIF
             ENDDO
             AGR(I,NF+1)=BEL(I)*VSCHA(I)
             IGR(I,NF+1)=0
          ENDDO
          
C
          	
      END SELECT
C
C
C
C     Minimierungsbedingungen gemäß "~"
C
C
      DO J=1,NF
         IF(OP(J).EQ.'~') THEN
           NEE=NEE+1
           DO I=1,NF
             AGR(NEE,I)=0.
             IGR(NEE,I)=0
           ENDDO
           AGR(NEE,J)=BEL(J)
           IGR(NEE,J)=1
           AGR(NEE,NF+1)=BEL(J)*VLIOA(J)
           IGR(NEE,NF+1)=0
         ENDIF
      END DO
C
C     Umrechnung der Koeffizienten für reine Mengen
C     rechte Seiten bleiben gleich
C
C
      DO I=1,NEE
            DO J=1,NF
              AKOE(J)=AGR(I,J)
            ENDDO
            CALL UKOEF(NF,IBI,CBA,CLI,SPE,SPLE,AKOE,IER)
            IF(IER.NE.0) GOTO 99
            DO J=1,NF
               AGR(I,J)=AKOE(J)
            ENDDO
      ENDDO
C
  99  DEALLOCATE(AKOE,RMENG)
C
      RETURN
C
C
C
C
      END
C
      SUBROUTINE GREINO(NF,ICHF,CBA,CLI,SPE,SPLE,OP,INO,AMEL,AMEH,
     &                 IGX,MDIM,NEE,AGR,IGR,CL,CU,IC,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      DIMENSION SPE(*),CLI(*),CBA(*)
      CHARACTER*1 OP(*)
      DIMENSION AGR(MDIM,*),IGR(MDIM,*),CL(*),CU(*),IC(*)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: AKOE
      INTEGER(KIND=4),DIMENSION(*) ::ICHF
C
C
C            
C
C     LIMITIERUNG GEMAESS NORMIERUNGSART (INO)
C
C        INO
C
C
C           1       Gesamtvolumen
C           2       Volumen Bindemittel+Loesemittel in reinem Bindemittelbatch
C           3       Gesamtbinde+ -loesemittelvolumen
C           4       reines Farbmittelvolumen
C           5       Volumen des Farbmittelbatches
C           6       Volumen des letzten Farb/Bindemittel (rein)
C                   (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C           7       Batchvolumen des letzten Farb/Bindemittels
C           8       Volumen des ersten Farb/Bindemittels (rein)
C                   (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C           9       Batchvolumen des ersten Farb/Bindemittels
C          10       Volumen des reinen Farb/Bindemittels (Festkoerpervolumen)
C          11       Volumen des gesamten Bindemittels (rein)
C          12       Volumen des Bindemittels (rein) im Bindmittelbatch
C          13       Volumen des Gesamtloesemittels
C          14       Volumen des Loesemittels im Bindemittelbatch
C          15       Volumen der *Farb-/Bindemittel (rein)
C          16       Volumen der *Farb-/Bindemittel (Batch)
C          17       Volumen der +Farb-/Bindemittel (rein)
C          18       Volumen der +Farb-/Bindemittel (Batch)
C          19       Volumen aller Weißpigmente (rein)
C          20       Batchvolumen aller Weißpigmente
C          21       Volumen aller Schwarzpigmente (rein)
C          22       Batchvolumen aller Schwarzpigmente
C          23       Volumen aller Farbmittel(ICHF=0) (rein)
C          24       Batchvolumen aller Farbmittel (ICHF=0)
C          25       Volumen aller Farbmittel(ICHF=0) + Schwarzpigment(ICHF=3) (rein)
C          26       Batchvolumen aller Farbmittel (ICHF=0) + Schwarzpigment (ICHF=3)
C          27       Volumen aller Farbmittel(ICHF=0) + Effektpigmente(ICHF=5) (rein)
C          28       Batchvolumen aller Farbmittel (ICHF=0) + Effektpigmente(ICHF=5)
C          29       Volumen aller Farbmittel(ICHF=0) + Effektpigmente(ICHF=5)+ Schwarzpigment (ICHF=3) (rein)
C          30       Batchvolumen aller Farbmittel (ICHF=0) + Effektpigmente(ICHF=5)+Schwarzpigment(ICHF=3)
C
C
C
C
      ALLOCATE(AKOE(NF),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF
C
C
C     MENGENLIMITIERUNG
C
      NEE=1
C
      CU(NEE)=AMEH
      CL(NEE)=AMEL
      IC(NEE)=3
      CALL MKOEFF(NF,ICHF,CBA,CLI,SPE,SPLE,OP,AKOE,INO,IER)
      IF(IER.NE.0) GOTO 99
      DO I=1,NF
         AGR(NEE,I)=AKOE(I)
         IGR(NEE,I)=1
      ENDDO
      AGR(NEE,NF+1)=0.
      IGR(NEE,NF+1)=0
      IF(IGX.EQ.5) THEN
         IGR(NEE,NF+1)=-1
      ENDIF   
C
C
 99   DEALLOCATE(AKOE)
C
C
C
      RETURN
C
C
      END
C
      SUBROUTINE GREINP(NF,ICHF,CBA,CLI,SPE,SPLE,OP,INP,INQ,PROLL,PROHH,
     &                 MDIM,NEE,AGR,IGR,CL,CU,IC,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      DIMENSION SPE(*),CLI(*),CBA(*)
      CHARACTER*1 OP(*)
      DIMENSION AGR(MDIM,*),IGR(MDIM,*),CL(*),CU(*),IC(*)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: AKOU,AKOO
      INTEGER(KIND=4),DIMENSION(*) ::ICHF

      DATA RANGEX/1.E35/,TOL/1.D-4/
C
      ALLOCATE(AKOU(NF),AKOO(NF),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF 
C
C             
C
C
C     PROZENTIGKEITLIMITIERUNG
C
C
      IBI=IBIN(NF,ICHF)
      NEE=0
      IF(INP.GT.0) THEN
         IF(IBI.EQ.0.AND.INP.LE.3) THEN
            IER=4092
            GOTO 99
         ENDIF
C         CALL KINP(IBI,INP,INOO,INOU,IER)
         CALL TEINPQ(IBI,INP,INQ,IER)
         IF(IER.NE.0) GOTO 99
         CALL MKOEFF(NF,ICHF,CBA,CLI,SPE,SPLE,OP,AKOO,INP,IER)
         IF(IER.NE.0) GOTO 99
         CALL MKOEFF(NF,ICHF,CBA,CLI,SPE,SPLE,OP,AKOU,INQ,IER)
         IF(IER.NE.0) GOTO 99
      
         NEE=NEE+1
         CU(NEE)=0.
         CL(NEE)=-RANGEX
         IC(NEE)=2
         PROL=PROLL
         PROH=PROHH
         IF(PROH.GT.1.) THEN
           PROH=1.
         ENDIF
         IF(PROH.LT.0.) THEN
           PROH=0.
         ENDIF
         IF(PROL.LT.0.) THEN
           PROL=0.
         ENDIF
         IF(PROL.GT.PROH) THEN
           PROL=PROH
         ENDIF
C
C        UNTERE PROZENTIGKEIT
C
         DO I=1,NF
            AGR(NEE,I)=PROL*AKOU(I)-AKOO(I)
            IF(ABS(AGR(NEE,I)).GT.TOL) THEN
            	IGR(NEE,I)=1
            ELSE
                IGR(NEE,I)=0
            ENDIF
         ENDDO
         AGR(NEE,NF+1)=0.
         IGR(NEE,NF+1)=0
C
C
C        OBERE PROZENTIGKEIT
C
         IF(ABS(PROH-PROL).LT.TOL) THEN
C
C          AUS UNGLEICHUNG WIRD GLEICHUNG GEMACHT
C
           IC(NEE)=3
           CL(NEE)=0.
         ELSE
           NEE=NEE+1
           CU(NEE)=RANGEX
           CL(NEE)=0.
           IC(NEE)=1
           DO I=1,NF
              AGR(NEE,I)=PROH*AKOU(I)-AKOO(I)
              IF(ABS(AGR(NEE,I)).GT.TOL) THEN
              	IGR(NEE,I)=1
              ELSE
                IGR(NEE,I)=0
              ENDIF
           ENDDO
           AGR(NEE,NF+1)=0.
           IGR(NEE,NF+1)=0
         ENDIF
      ENDIF
C
C
C
C
  99  DEALLOCATE(AKOU,AKOO)
      RETURN
C
C
C
C
      END
C
C
C******************************************************************************
      SUBROUTINE MADJS(NF,ICHF,ACONZ,MDIM,NEE,AGR,CL,CU,IC,IER)
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     Reine Mengen mit Grenzen korrigieren
C 
      DIMENSION ACONZ(*),ICHF(*)   
      DIMENSION AGR(MDIM,*),CL(*),CU(*),IC(*)
      DIMENSION IOPT(25)
      INTEGER,ALLOCATABLE,DIMENSION(:):: IW
      INTEGER,ALLOCATABLE,DIMENSION(:) :: IND
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: BL
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: BU
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:)::  X
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: RW
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:):: W
      DATA IOPT/9,6,-5,8,99,20*0/
C
C      DATA TOL/0.D-7/
      IER=0
      MH=NEE+NF+1
      ALLOCATE (BL(MH),BU(MH),X(2*MH+2),RW(5*MH+NF),
     &          IND(MH),IW(2*MH),W(MH,MH+1),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF
      MCON=NEE
      DO I=1,NF+NEE
           IND(I)=IC(I)
           BL(I)=CL(I)
           BU(I)=CU(I)
      ENDDO
      DO I=1,NEE
         DO J=1,NF+1
            W(I,J)=AGR(I,J)
         ENDDO
      ENDDO
      DO I=1,NF
      	DO J=1,NF+1
      	  W(I+NEE,J)=0.
        END DO
      END DO
      AMMAX=1.D-15
      IMMAX=1
      IF(IBIN(NF,ICHF).GT.0) THEN
        IBWBM=IBIN(NF,ICHF)
        IF (ACONZ(IBWBM).GT.AMMAX) THEN
           AMMAX=ACONZ(IBWBM)
           IMMAX=IBWBM
        END IF
      ELSEIF(IBWN(NF,ICHF).GT.0) THEN
        IBWBM=IBWN(NF,ICHF)
        IF (ACONZ(IBWBM).GT.AMMAX) THEN
           AMMAX=ACONZ(IBWBM)
           IMMAX=IBWBM
        END IF
      ELSEIF(IBMN(NF,ICHF).GT.0) THEN
        IBWBM=IBMN(NF,ICHF)
        IF (ACONZ(IBWBM).GT.AMMAX) THEN
           AMMAX=ACONZ(IBWBM)
           IMMAX=IBWBM
        END IF
      ENDIF
      DO J=1,NF
        IF(ACONZ(J).GT.AMMAX) THEN
           AMMAX=ACONZ(J)
           IMMAX=J
        ENDIF
      END DO
      II=0
      DO J=1,NF
       IF(ACONZ(J).LE.0.) ACONZ(J)=0.
       IF(J.NE.IMMAX) THEN
       	II=II+1
        W(II+NEE,J)=1.
        W(II+NEE,IMMAX)=-ACONZ(J)/AMMAX
       ENDIF
      ENDDO
      NG=NF-1
C
      CALL SBOCLS(W,MH,MCON,NG,NF,BL,BU,IND,IOPT,X,RNORMC,RN,IER,
     &            RW,IW)
      IF(IER.NE.0) THEN
         GOTO 99
      ENDIF
      DO 15 I=1,NF
       ACONZ(I)=X(I)
  15  CONTINUE
  99  DEALLOCATE (BL,BU,X,RW,IND,IW,W)
      RETURN
      END 
            
C******************************************************************************
      SUBROUTINE MADFEA(NF,C,MDIM,NEE,AGR,CL,CU,IC,IER)
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C 
      DIMENSION AGR(MDIM,*),CL(*),CU(*),IC(*)
      DIMENSION IOPT(25)
      INTEGER,ALLOCATABLE,DIMENSION(:):: IW
      INTEGER,ALLOCATABLE,DIMENSION(:) :: IND
      REAL(KIND=8),DIMENSION(*) :: C
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: BL
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: BU
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:)::  X
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: RW
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:):: W
      REAL(KIND=8) :: TOL,RNORMC,RN
      DATA IOPT/99,24*0/
C
      DATA TOL/1.D-3/
      IER=0
      MH=NEE+NF+1
      ALLOCATE (BL(MH),BU(MH),X(2*MH+2),RW(5*MH+NF),
     &          IND(MH),IW(2*MH),W(MH,MH+1),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF
      DO I=1,NF+NEE
           IND(I)=IC(I)
           BL(I)=CL(I)
           BU(I)=CU(I)
      ENDDO
      DO I=1,NEE
         DO J=1,NF+1
            W(I,J)=AGR(I,J)
         ENDDO
      ENDDO
C
      MEE=0
      CALL SBOCLS(W,MH,NEE,MEE,NF,BL,BU,IND,IOPT,X,RNORMC,RN,IER,
     &            RW,IW)
      IF(IER.NE.0) THEN
         GOTO 99
      ENDIF
      DO I=1,NF
        C(I)=X(I)
      END DO
      IF(IER.NE.0) THEN
         IER=4091
         GOTO 99
      ENDIF
      IF(ABS(RNORMC).GT.TOL) THEN
         IER=4150
         GOTO 99
      ENDIF
  99  DEALLOCATE (BL,BU,X,RW,IND,IW,W)
      RETURN
      END 
c
c
c
c
c
c
      SUBROUTINE MKOEFF(NC,ICHF,CBA,CLI,SPE,SPLE,OP,AKOEF,INO,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      DIMENSION CBA(*),CLI(*),SPE(*),AKOEF(*)
      INTEGER(KIND=4),DIMENSION(*) ::ICHF
      CHARACTER*1 OP(*)
C     Berechnung der Koeffizienten fuer die Mengen der
C     reinen Farb-/Bindemittel fuer 
C     verschiedenen Volumennormierungsarten    
C     
C      
C     
C     NC      Anzahl der Farb/Bindemittel
C     CBA     Mengenkonzentration des Farbmittels ( ist fuer Bindemittel =0.)
C     CLI     Mengenkonzentration des Bindemittels
C     SPE     spezifisches Gewicht der Farb/Bindemittel 
C     SPLE    spezifisches Gewicht des Loesemittels
C     OP      * oder +
C     AKOEF   Koeffizienten gemaess Normierung 
C     INO     Kennzeichen fuer Art der Normierung
C           1       Gesamtvolumen
C           2       Volumen Bindemittel+Loesemittel in reinem Bindemittelbatch
C           3       Gesamtbinde+ -loesemittelvolumen
C           4       reines Farbmittelvolumen
C           5       Volumen des Farbmittelbatches
C           6       Volumen des letzten Farb/Bindemittel (rein)
C                   (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C           7       Batchvolumen des letzten Farb/Bindemittels
C           8       Volumen des ersten Farb/Bindemittels (rein)
C                   (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C           9       Batchvolumen des ersten Farb/Bindemittels
C          10       Volumen des reinen Farb/Bindemittels (Festkoerpervolumen)
C          11       Volumen des gesamten Bindemittels (rein)
C          12       Volumen des Bindemittels (rein) im Bindmittelbatch
C          13       Volumen des Gesamtloesemittels
C          14       Volumen des Loesemittels im Bindemittelbatch
C          15       Volumen der *Farb-/Bindemittel (rein)
C          16       Volumen der *Farb-/Bindemittel (Batch)
C          17       Volumen der +Farb-/Bindemittel (rein)
C          18       Volumen der +Farb-/Bindemittel (Batch)
C          19       Volumen aller Weißpigmente (rein)
C          20       Batchvolumen aller Weißpigmente
C          21       Volumen aller Schwarzpigmente (rein)
C          22       Batchvolumen aller Schwarzpigmente
C          23       Volumen aller Farbmittel(ICHF=0) (rein)
C          24       Batchvolumen aller Farbmittel (ICHF=0)
C          25       Volumen aller Farbmittel(ICHF=0) + Schwarzpigment(ICHF=3) (rein)
C          26       Batchvolumen aller Farbmittel (ICHF=0) + Schwarzpigment (ICHF=3)
C          27       Volumen aller Farbmittel(ICHF=0) + Effektpigmente(ICHF=5) (rein)
C          28       Batchvolumen aller Farbmittel (ICHF=0) + Effektpigmente(ICHF=5)
C          29       Volumen aller Farbmittel(ICHF=0) + Effektpigmente(ICHF=5)+ Schwarzpigment (ICHF=3) (rein)
C          30       Batchvolumen aller Farbmittel (ICHF=0) + Effektpigmente(ICHF=5)+Schwarzpigment(ICHF=3)
C     IBI     Positionsnummer des Bindemittels
C     IER     Fehlercode
C
C

      IBI=IBIN(NC,ICHF)
      CALL VKOEFF(NC,ICHF,CBA,CLI,SPE,SPLE,OP,AKOEF,INO,IER)
      IF(IER.NE.0) GOTO 99
      CALL UKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
  99  RETURN
      END
      SUBROUTINE VKOEFF(NC,ICHF,CBA,CLI,SPE,SPLE,OP,AKOEF,INO,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
C
      DIMENSION CBA(*),CLI(*),SPE(*),AKOEF(*)
      INTEGER(KIND=4),DIMENSION(*) ::ICHF
      CHARACTER*1 OP(*)
      CHARACTER*1 STE
      CHARACTER*1 PLU
C
      DATA STE/'*'/,PLU/'+'/
C
C
C     Berechnung der Koeffizienten fuer die Batchvolumennormierung
C     fuer die verschiedenen Volumennormierungsarten    
C     
C      
C     
C     NC      Anzahl der Farb/Bindemittel
C     CBA     Mengenkonzentration des Farbmittels ( ist fuer Bindemittel =0.)
C     CLI     Mengenkonzentration des Bindemittels
C     SPE     spezifisches Gewicht der Farb/Bindemittel 
C     SPLE    spezifisches Gewicht des Loesemittels
C     AKOEF   Koeffizienten gemaess Normierung 
C     INO     Kennzeichen fuer Art der Normierung
C           1       Gesamtvolumen
C           2       Volumen Bindemittel+Loesemittel in reinem Bindemittelbatch
C           3       Gesamtbinde+ -loesemittelvolumen
C           4       reines Farbmittelvolumen
C           5       Volumen des Farbmittelbatches
C           6       Volumen des letzten Farb/Bindemittel (rein)
C                   (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C           7       Batchvolumen des letzten Farb/Bindemittels
C           8       Volumen des ersten Farb/Bindemittels (rein)
C                   (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C           9       Batchvolumen des ersten Farb/Bindemittels
C          10       Volumen des reinen Farb/Bindemittels (Festkoerpervolumen)
C          11       Volumen des gesamten Bindemittels (rein)
C          12       Volumen des Bindemittels (rein) im Bindmittelbatch
C          13       Volumen des Gesamtloesemittels
C          14       Volumen des Loesemittels im Bindemittelbatch
C          15       Volumen der *Farb-/Bindemittel (rein)
C          16       Volumen der *Farb-/Bindemittel (Batch)
C          17       Volumen der +Farb-/Bindemittel (rein)
C          18       Volumen der +Farb-/Bindemittel (Batch)
C          19       Volumen aller Weißpigmente (rein)
C          20       Batchvolumen aller Weißpigmente
C          21       Volumen aller Schwarzpigmente (rein)
C          22       Batchvolumen aller Schwarzpigmente
C          23       Volumen aller Farbmittel(ICHF=0) (rein)
C          24       Batchvolumen aller Farbmittel (ICHF=0)
C          25       Volumen aller Farbmittel(ICHF=0) + Schwarzpigment(ICHF=3) (rein)
C          26       Batchvolumen aller Farbmittel (ICHF=0) + Schwarzpigment (ICHF=3)
C          27       Volumen aller Farbmittel(ICHF=0) + Effektpigmente(ICHF=5) (rein)
C          28       Batchvolumen aller Farbmittel (ICHF=0) + Effektpigmente(ICHF=5)
C          29       Volumen aller Farbmittel(ICHF=0) + Effektpigmente(ICHF=5)+ Schwarzpigment (ICHF=3) (rein)
C          30       Batchvolumen aller Farbmittel (ICHF=0) + Effektpigmente(ICHF=5)+Schwarzpigment(ICHF=3)
C 
C
C


       
C     IBI     Positionsnummer des Bindemittels
C     IER     Fehlercode
C
C
C
C     ABFRAGEN
C
C
C
      IBI=IBIN(NC,ICHF)
      DO I=1,NC
         IF(CBA(I).LE.0.) GOTO 91
         IF(SPE(I).LE.0.) GOTO 92
      ENDDO
      IF(SPLE.LE.0.) GOTO 92              
C
C
C
C
C
C
      IER=0
C
C
      SELECT CASE(INO)
C
C       1       Gesamtvolumen
C
C
        CASE(1)
C 


            DO I=1,NC
               AKOEF(I)=1.
            ENDDO
           
C      
C       2       Volumen Bindemittel+Loesemittel in Bindemittelbatch
C
        CASE(2)
C 
            IF(IBI.EQ.0) GOTO 90
            DO I=1,NC
               AKOEF(I)=0.
            ENDDO
            AKOEF(IBI)=1.
C
C
C       3       Gesamtbinde+ -loesemittelvolumen
C
        CASE(3)
C 
C
C
            DO I=1,NC
               IF(I.NE.IBI) THEN
                  AKOEF(I)=(1.-CBA(I))/(SPLE*CBA(I))
               ENDIF
            ENDDO
            IF(IBI.NE.0) THEN
               DO I=1,NC
                  IF(I.EQ.IBI) THEN
                     AKOEF(I)=1./SPE(IBI)+(1.-CBA(IBI))/(CBA(IBI)*SPLE)
                  ELSE
                     AKOEF(I)=AKOEF(I)*(1.-CLI(I)/CBA(IBI))
                  ENDIF
               ENDDO
            ENDIF
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99

C
C
C
C       4       reines Farbmittelvolumen
C
        CASE(4)
C
C
C
            DO I=1,NC
               IF(I.NE.IBI) THEN
                  AKOEF(I)=1./SPE(I)
               ENDIF
            ENDDO
            IF(IBI.NE.0.) AKOEF(IBI)=0.
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
C 
C
C       5       Volumen des Farbmittelbatches
C
C
C
C
        CASE(5)
C
C
C
            DO I=1,NC
               IF(I.NE.IBI) THEN
                  AKOEF(I)=1.
               ENDIF
            ENDDO
            IF(IBI.NE.0.) AKOEF(IBI)=0.
        
C 
C
C       6       Volumen des letzten Farb/Bindemittel (rein)
C               (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C 
C
C
C
C
C
        CASE(6)
C
C
C
            DO I=1,NC
               AKOEF(I)=0.
            ENDDO
            AKOEF(NC)=1./SPE(NC)
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
        
C 
C
C       7       Batchvolumen des letzten Farb/Bindemittels
C 
C
C
C
C
C
        CASE(7)
C
C
C
            DO I=1,NC
               AKOEF(I)=0.
            ENDDO
            AKOEF(NC)=1.
        
C 
C
C       8       Volumen des ersten Farb/Bindemittels (rein)
C               (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C 
C
C 
C
C
C
C
C
        CASE(8)
C
C
C
            DO I=1,NC
               AKOEF(I)=0.
            ENDDO
            AKOEF(1)=1./SPE(1)
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
        
C       9       Batchvolumen des ersten Farb/Bindemittels
C       
C 
C
C
C
C
C
C
        CASE(9)
C
C
C
            DO I=1,NC
               AKOEF(I)=0.
            ENDDO
            AKOEF(1)=1.
C
C
C       10       Volumen des reinen Farb/Bindemittels (Festkoerpervolumen)
        
C 
C
C
C
C
C
C
        CASE(10)
C
C
C
            DO I=1,NC
               AKOEF(I)=1./SPE(I)
            ENDDO
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
C
C
C
C
C       11       Volumen des gesamten Bindemittels (rein)
           
C      
C
        CASE(11)
C 
            IF(IBI.EQ.0) GOTO 90
            DO I=1,NC
               AKOEF(I)=0.
            ENDDO
            AKOEF(IBI)=1.
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
C
C
C       12       Volumen des Bindemittels (rein) im Bindmittelbatch
C          
C      
C
        CASE(12)
C 
            IF(IBI.EQ.0) GOTO 90
            DO I=1,NC
               AKOEF(I)=0.
            ENDDO
            SPI=1./SPE(IBI)
            AKOEF(IBI)=SPI/(SPI+(1.-CBA(IBI))/(SPLE*CBA(IBI)))
C
C
C       13       Volumen des Gesamtloesemittels
C           
C      
C
        CASE(13)
C 
            DO I=1,NC
               IF(I.NE.IBI) THEN
                  AKOEF(I)=(1.-CBA(I))/(SPLE*CBA(I))
               ENDIF 
            ENDDO
            IF(IBI.NE.0) THEN
               DO I=1,NC
                  IF(I.EQ.IBI) THEN
                     AKOEF(I)=(1.-CBA(I))/(SPLE*CBA(I))
                  ELSE
                     AKOEF(I)=AKOEF(I)*(1.-CLI(I)/CBA(IBI))
                  ENDIF
               ENDDO
            ENDIF
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
C
C
C       14       Volumen des Loesemittels im Bindemittelbatch
C      
C
        CASE(14)
C 
            IF(IBI.EQ.0) GOTO 90
            DO I=1,NC
               AKOEF(I)=0.
            ENDDO
            SPI=1./SPLE
            AKOEF(IBI)=SPI/(SPI+CBA(IBI)/(SPE(IBI)*(1.-CBA(IBI))))
C
C  
        
C 
C
C       15      Volumen der *- Farb/Bindemittel (rein)
C               (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C 
C
C
C
C
C
        CASE(15)
C
C
C
            DO I=1,NC
               IF(OP(I).EQ.STE) THEN
                  AKOEF(I)=1./SPE(I)
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
        
C 
C
C      16       Batchvolumen der * Farb/Bindemittel
C 
C
C
C
C
C
        CASE(16)
C
C
C
            DO I=1,NC
               IF(OP(I).EQ.STE) THEN
                  AKOEF(I)=1.
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
        
C 
        
C 
C
C       17      Volumen der +- Farb/Bindemittel (rein)
C               (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C 
C
C
C
C
C
        CASE(17)
C
C
C
            DO I=1,NC
               IF(OP(I).EQ.PLU) THEN
                  AKOEF(I)=1./SPE(I)
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
        
C 
C
C      18       Batchvolumen der + Farb/Bindemittel
C 
C
C
C
C
C
        CASE(18)
C
C
C
            DO I=1,NC
               IF(OP(I).EQ.PLU) THEN
                  AKOEF(I)=1.
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
        
C 
C
C
C
C       19      Volumen aller Weißpigmente (rein)
C
C
C
C
C
C
        CASE(19)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.2) THEN
                  AKOEF(I)=1./SPE(I)
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
        
C 
C
C      20       Batchvolumen aller Weißpigmente
C 
C
C
C
C
C
        CASE(20)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.2) THEN
                  AKOEF(I)=1.
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
        
C 
C
C
C
C
C       21      Volumen aller Schwarzpigmente (rein)
C
C
C
C
C
C
        CASE(21)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.3) THEN
                  AKOEF(I)=1./SPE(I)
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
        
C 
C
C      22     Batchvolumen aller Schwarzpigmente
C 
C
C
C
C
C
        CASE(22)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.3) THEN
                  AKOEF(I)=1.
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
        
C 
C
C
C
C
C       23      Volumen aller Farbmittel(ICHF=0) (rein)
C
C
C
C
C
        CASE(23)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.0) THEN
                  AKOEF(I)=1./SPE(I)
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
        
C 
C
C      24     Batchvolumen aller Farbmittel (ICHF=0)
C 
C
C
C
C
C
        CASE(24)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.0) THEN
                  AKOEF(I)=1.
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
        
C 
C
C
C
C
C
C       25      Volumen aller Farbmittel(ICHF=0) + Schwarzpigment(ICHF=3) (rein)
C
C
C
C
C
        CASE(25)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.0.OR.ICHF(I).EQ.3) THEN
                  AKOEF(I)=1./SPE(I)
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
        
C 
C
C      26     Batchvolumen aller Farbmittel (ICHF=0) + Schwarzpigment (ICHF=3)
C 
C
C
C
C
C
        CASE(26)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.0.OR.ICHF(I).EQ.3) THEN
                  AKOEF(I)=1.
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
C
C
C
C       27      Volumen aller Farbmittel(ICHF=0) + Effektpigmente(ICHF=5) (rein)
C
C
C
C
C
        CASE(27)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.0.OR.ICHF(I).EQ.5) THEN
                  AKOEF(I)=1./SPE(I)
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
        
C 
C
C      28     Batchvolumen aller Farbmittel (ICHF=0) + Effektpigmente(ICHF=5)
C 
C
C
C
C
C
        CASE(28)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.0.OR.ICHF(I).EQ.5) THEN
                  AKOEF(I)=1.
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
C
C
C
C       29      Volumen aller Farbmittel(ICHF=0) + Effektpigmente(ICHF=5)+ Schwarzpigment (ICHF=3) (rein)
C
C
C
C
C
        CASE(29)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.0.OR.ICHF(I).EQ.5.OR.ICHF(I).EQ.3) THEN
                  AKOEF(I)=1./SPE(I)
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO
            CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
            IF(IER.NE.0) GOTO 99
        
C 
C
C      30     Batchvolumen aller Farbmittel (ICHF=0) + Effektpigmente(ICHF=5)+Schwarzpigment(ICHF=3)
C 
C
C
C
C
C
        CASE(30)
C
C
C
            DO I=1,NC
               IF(ICHF(I).EQ.0.OR.ICHF(I).EQ.5.OR.ICHF(I).EQ.3) THEN
                  AKOEF(I)=1.
               ELSE
                  AKOEF(I)=0.
               ENDIF
            ENDDO

        CASE DEFAULT
            IER=4078
      END SELECT

      DO I=1,NC
         IF(AKOEF(I).GT.1.D-5) THEN
            RETURN
         ENDIF
      END DO
C
      GOTO 93
  99  RETURN
C
C
  90  IER=4080
      GOTO 99
  91  IER=4081
      GOTO 99
  92  IER=4082
      GOTO 99
  93  IER=4100
      GOTO 99
      END
      SUBROUTINE SONSPZ(NF,IBI,INF,IVOL,CBA,CB,
     &                  EFF,SPEZ,SPE,SPLE,SPL,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      DIMENSION SPE(*),EFF(*),SPEZ(*),CB(*),CBA(*)
C
C     SPE NEUES SPEZ.GEWICHT
C     SPEZ ALTES SPEZ. GEWICHT
C     CB  NEUE FARBMITTELKONZENTRATION
C     CBA ALTE BINDEMITTELKONZENTRATION
C     SPL  NEUES SPEZ. GEW. LOESEMITTEL
C     SPLE ALTES SPEZ. GEW. LOESEMITTEL

C
      IER=0
      HU=0.1D0*HUGE(1.D0)
      DO I=1,NF
         IF(CBA(I).EQ.0.) GOTO 91
         CB(I)=CBA(I)
      ENDDO
      CALL VOLSPZ(IVOL,NF,EFF,SPEZ,SPE,SPLE,SPL,IER)
      IF(IER.NE.0) GOTO 99
C
C     ART DER MENGEN/VOLUMEN
C
      SELECT CASE(INF)
        CASE(0)
C
C
C         BATCHMENGEN(VOLUMEN) (FARB- + BINDE- +LOESEMITTEL)
C     
C
        CASE(1)
C
C         REINE FARB-/BINDEMITTELMENGE(VOLUMEN) 
C
          DO I=1,NF
             CB(I)=1.D0
          ENDDO
C
        CASE(2)
C
C
C         FESTSTOFFMENGEN (FARB- + BINDEMITTEL) IM FARB-/BINDEMITTELBATCH
C     
          SPL=HU          
C
        CASE(3)
C
C         BINDEMITTELMENGEN(VOLUMEN) IM FARB-/BINDEMITTELBATCH 
C
          SPL=HU
          DO I=1,NF
             IF(I.NE.IBI) THEN
                  SPE(I)=HU
             ENDIF
          ENDDO
C
        CASE(4)
C
C         LOESEMITTELMENGEN(VOLUMEN) IM FARB-/BINDEMITTELBATCH 
C
          DO I=1,NF
             SPE(I)=HU
          ENDDO
C
        CASE(5)
C
C         BINDE- + LOESEMITTELMENGEN(VOLUMEN) IM FARB-/BINDEMITTELBATCH 
C
          DO I=1,NF
            IF(I.NE.IBI) THEN
                SPE(I)=HU
            ENDIF
          ENDDO
C
        CASE(6)
C
C         REINE FARBMITTELMENGE(VOLUMEN) OHNE BINDEMITTEL
C
          SPL=HU
          DO I=1,NF
             IF(I.EQ.IBI) THEN
                SPE(I)=HU
             ENDIF
          ENDDO
        CASE DEFAULT
          IER=4085
      END SELECT
C        
                    
  99  RETURN
  91  IER=4081
      GOTO 99
      END  
      SUBROUTINE VOLSPZ(IVOL,NF,EFF,SPEZ,SPZ,SPLO,SLOE,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      DIMENSION SPEZ(*),SPZ(*),EFF(*)
C
C     SPEZIFISCHES GEWICHT FUER MENGEN BZW: VOLUMEN FESTLEGEN
C
      IER=0
C
      IF(IVOL.EQ.0) THEN
C        
C        MENGEN
C
           SLOE=1.D0
C        
           DO I=1,NF
C              SPZ(I)=1.D0
             SPZ(I)=EFF(I)
           ENDDO
      ELSE
C
C
C        VOLUMEN
C
C
            SLOE=SPLO
            IF(SLOE.LE.0.D0) GOTO 91
            DO I=1,NF
               IF(SPEZ(I).LE.0.D0) GOTO 91
C               SPZ(I)=SPEZ(I)
               SPZ(I)=EFF(I)*SPEZ(I)
            ENDDO
      ENDIF   
 99   RETURN
 91   IER=4082
      GOTO 99
      END
      SUBROUTINE TEINPQ(IBI,INP,INQ,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      IER=0
      IF(IBI.EQ.0) THEN

C                REINES FARBMITTEL
C           -------------------------------------
C                 FESTSTOFFGEHALT
C

            IF(INP.EQ.4.AND.INQ.EQ.10) THEN
               IER=4131
            ENDIF
C
C                FARBMITTELBATCHES
C           -------------------------------------
C                 GESAMTMENGE
C

            IF(INP.EQ.5.AND.INQ.EQ.1) THEN
               IER=4132
            ENDIF
      ENDIF
      RETURN
      END
C
C
C
C     KINP wird nichtmehr verwendet
C
C
      SUBROUTINE KINP(IBI,INP,INOO,INOU,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
C     INOO UND INOU FUER VGVOL BESTIMMEN
C   
      IER=0
      SELECT CASE(INP)
          CASE(0)
            INOO=-1
            INOU=-1
          CASE(1)
C               BINDEMITTEL IN BINDEMITTELBATCH
C           -------------------------------------
C                 FESTSTOFFGEHALT
C
            INOO=12
            INOU=10
            
   
          CASE(2)
C                BINDEMITTELBATCH
C           -------------------------------------
C                 GESAMTMENGE      
C
            INOO=2
            INOU=1
            

          CASE(3)
C                GESAMTBINDEMITTEL
C           -------------------------------------
C                 FESTSTOFFGEHALT
C
            INOO=11
            INOU=10
            
C
          CASE(4)
C                GESAMT BINDE- + LOESEMITTEL
C           -------------------------------------
C                 GESAMTMENGE
C
            INOO=3
            INOU=1
C
          CASE(5)
C                REINES FARBMITTEL
C           -------------------------------------
C                 FESTSTOFFGEHALT
C
            INOO=4
            INOU=10
            IF(IBI.EQ.0) THEN
               GOTO 91
            ENDIF
C
          CASE(6)
C                FARBMITTELBATCHES
C           -------------------------------------
C                 GESAMTMENGE
C
            INOO=5
            INOU=1
            IF(IBI.EQ.0) THEN
               GOTO 92
            ENDIF
          CASE(7)
C                LETZTES FARBMITTEL(REIN)
C           -------------------------------------
C                 FESTSTOFFGEHALT
C
            INOO=6
            INOU=10
          CASE(8)
C                LETZTES FARBMITTEL (BATCH)
C           -------------------------------------
C                 GESAMTMENGE
C
            INOO=7
            INOU=1
          CASE(9)
C                ERSTES FARBMITTEL(REIN)
C           -------------------------------------
C                 FESTSTOFFGEHALT
C
            INOO=8
            INOU=10
          CASE(10)
C                ERSTES FARBMITTEL (BATCH)
C           -------------------------------------
C                 GESAMTMENGE
C
            INOO=9
            INOU=1
          CASE(11)
C                *-FARB-/BINDEMITTEL (REIN)
C           -------------------------------------
C                 FESTSTOFFGEHALT
C
            INOO=15
            INOU=10
          CASE(12)
C                *-FARB-/BINDEMITTEL (BATCH)
C           -------------------------------------
C                 GESAMTMENGE
C
            INOO=16
            INOU=1
          CASE(13)
C                +-FARB-/BINDEMITTEL (REIN)
C           -------------------------------------
C                 FESTSTOFFGEHALT
C
            INOO=17
            INOU=10
          CASE(14)
C                +-FARB-/BINDEMITTEL (BATCH)
C           -------------------------------------
C                 GESAMTMENGE
C
            INOO=18
            INOU=1
          CASE(15)
C                +-FARB-/BINDEMITTEL (REIN)
C           -------------------------------------
C                 *-FARB-/BINDEMITTEL (REIN)

C
            INOO=17
            INOU=15

          CASE(16)
C                +-FARB-/BINDEMITTEL (BATCH)
C           -------------------------------------
C                 *-FARB-/BINDEMITTEL (BATCH)

C
            INOO=18
            INOU=16
          CASE(17)
C                  GESAMTBINDEMITTEL
C           -------------------------------------
C                 *-FARB-/BINDEMITTEL (REIN)

C
            INOO=11
            INOU=15
          CASE(18)
C                 BINDEMITTELBATCH
C           -------------------------------------
C                 *-FARB-/BINDEMITTELBATCH

C
            INOO=2
            INOU=16

          CASE(19)
C                  LETZTES FARB/BINDEMITTEL(REIN)
C           -------------------------------------
C                 *-FARB-/BINDEMITTEL (REIN)

C
            INOO=6
            INOU=15
          CASE(20)
C                  LETZTES FARB/BINDEMITTEL(BATCH)
C           -------------------------------------
C                 *-FARB-/BINDEMITTELBATCH

C
            INOO=7
            INOU=16
          CASE(21)
C                  ERSTES FARB/BINDEMITTEL(REIN)
C           -------------------------------------
C                 *-FARB-/BINDEMITTEL (REIN)

C
            INOO=8
            INOU=15
          CASE(22)
C                 ERSTES FARB/BINDEMITTEL(BATCH)
C           -------------------------------------
C                 *-FARB-/BINDEMITTELBATCH

C
            INOO=9
            INOU=16
          CASE(23)
C                 Farbmittel (rein)
C           -------------------------------------
C                 Bindemittel (rein)

C
            INOO=23
            INOU=11
          CASE(24)
C                 Farbmittel (BATCH)
C           -------------------------------------
C                 Bindemittel (Batch)

C
            INOO=24
            INOU=12
          CASE(25)
C                 Farbmittel + Schwarzpigment (rein)
C           -------------------------------------
C                 Bindemittel (rein)

C
            INOO=25
            INOU=11
          CASE(26)
C                 Farbmittel + Schwarzpigment(BATCH)
C           -------------------------------------
C                 Bindemittel (Batch)

C
            INOO=26
            INOU=12
          CASE(27)
C                 Farbmittel (rein)
C           -------------------------------------
C                 Gesamtmenge
C
            INOO=23
            INOU=1
          CASE(28)
C                 Farbmittel (BATCH)
C           -------------------------------------
C                Gesamtmenge

C
            INOO=24
            INOU=1
          CASE(29)
C                 Farbmittel + Schwarzpigment (rein)
C           -------------------------------------
C                Gesamtmenge
C
            INOO=25
            INOU=1
          CASE(30)
C                 Farbmittel + Schwarzpigment(BATCH)
C           -------------------------------------
C                 Gesamtmenge

C
            INOO=26
            INOU=1








          CASE DEFAULT
      END SELECT   
      RETURN
  91  IER=4131
      RETURN
  92  IER=4132
      RETURN
      END            
C
C
C
C******************************************************************************
      SUBROUTINE REIVOL(NC,IBI,CC,AC,CBA,CLI,SPE,SPLE,IER)
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      DIMENSION CC(*),AC(*),CBA(*),CLI(*),SPE(*)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: AKOEF
      DATA TOL/-1.D-3/,EPS/1.E-4/
C
C
C     Berechnung der Farb/Bindemittelbatchvolumen aus den
C     reinen Farb/Bindemittelmengen
C      
C     
C     NC      Anzahl der Farb/Bindemittel
C     CC      Mengen der Reinstoffe 
C     AC      Batchvolumina = Volumen Farbmittel+Bindemittel+Loesemittel im
C             Batch (Bei Batchvolumen Bindemittel ist Volumen Farbmittel =0.)
C     CBA     Mengenkonzentration des Farbmittels ( ist fuer Bindemittel =0.)
C     CLI     Mengenkonzentration des Bindemittels
C     SPE     spezifisches Gewicht der Farb/Bindemittel 
C     SPLE    spezifisches Gewicht des Loesemittels
C     IBI     Positionsnummer des Bindemittels
C     IER     Fehlercode
C
C     Anmerkung: Sind alle spezifischen Gewichte=1., so ist die Volumen-
C                berechnung identisch mit der Mengenberechnung 
C
C
C
      IER=0
      ALLOCATE(AKOEF(NC),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF
C
C
C
C
      DO J=1,NC
         DO I=1,NC
            AKOEF(I)=0.D0
         ENDDO
         AKOEF(J)=1.
         CALL UKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
         IF(IER.NE.0) GOTO 99
         SUU=0.
         DO I=1,NC
            SUU=SUU+AKOEF(I)*CC(I)
         ENDDO
         AC(J)=SUU
      ENDDO
      SUU=0.D0
      DO J=1,NC
         SUU=SUU+AC(J)
      ENDDO
      DO J=1,NC
         IF(AC(J).LT.TOL*SUU.AND.SUU.GT.EPS) THEN
            IER=4105
         ENDIF
C        AC(J)=DMAX1(AC(J),0.D0)
      ENDDO
C
C
 99   DEALLOCATE(AKOEF)
      RETURN
      END     
C******************************************************************************
      SUBROUTINE VOLREI(NC,IBI,CC,AC,CBA,CLI,SPE,SPLE,IER)
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      DIMENSION CC(*),AC(*),CBA(*),CLI(*),SPE(*)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: AKOEF
C
C
C     Berechnung der reinen Farb/Bindemittelmengen
C     aus den Farb/Bindemittelbatchvolumen 
C 
C      
C     
C     NC      Anzahl der Farb/Bindemittel
C     CC      Mengen der Reinstoffe 
C     AC      Batchvolumina = Volumen Farbmittel+Bindemittel+Loesemittel im
C             Batch (Bei Batchvolumen Bindemittel ist Volumen Farbmittel =0.)
C     CBA     Mengenkonzentration des Farbmittels ( ist fuer Bindemittel =0.)
C     CLI     Mengenkonzentration des Bindemittels
C     SPE     spezifisches Gewicht der Farb/Bindemittel 
C     SPLE    spezifisches Gewicht des Loesemittels
C     IBI     Positionsnummer des Bindemittels
C     IER     Fehlercode
C
C     Anmerkung: Sind alle spezifischen Gewichte=1., so entsprechen die
C                AC-Werte den Batchmengen
C
C
      IER=0
      ALLOCATE(AKOEF(NC),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF
C
C
C
C
C
      DO J=1,NC
         DO I=1,NC
            AKOEF(I)=0.D0
         ENDDO
         AKOEF(J)=1.
         CALL WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
         IF(IER.NE.0) GOTO 99
         SUU=0.D0
         DO I=1,NC
            SUU=SUU+AKOEF(I)*AC(I)
         ENDDO
         CC(J)=SUU
      ENDDO
C
C
C
 99   DEALLOCATE(AKOEF)
      RETURN
      END     
C******************************************************************************
      SUBROUTINE VGVOL(NC,ICHF,CC,CBA,CLI,SPE,SPLE,OP,VMENG,INO,IER)
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      DIMENSION CC(*),CBA(*),CLI(*),SPE(*)
      CHARACTER*1 OP(*)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: HILF
      INTEGER(KIND=4),DIMENSION(*) ::ICHF
      DATA TOL/1.D-7/
C
C      
C     VOLUMENBERECHNUNG GEMAESS NORMIERUNG AUS reinen FARB-/BINDEMITTELMENGEN
C
C
C     
C     NC      Anzahl der Farb/Bindemittel
C     CC      Mengen der Reinstoffe
C     CBA     Mengenkonzentration des Farbmittels ( ist fuer Bindemittel =0.)
C     CLI     Mengenkonzentration des Bindemittels
C     SPE     spezifisches Gewicht der Farb/Bindemittel 
C     SPLE    spezifisches Gewicht des Loesemittels
C     OP      * oder +
C     VMENG   Istvolumen gemaess Normierung 
C     INO     Kennzeichen fuer Art der Normierung
C           1       Gesamtvolumen
C           2       Volumen Bindemittel+Loesemittel in reinem Bindemittelbatch
C           3       Gesamtbinde+ -loesemittelvolumen
C           4       reines Farbmittelvolumen
C           5       Volumen des Farbmittelbatches
C           6       Volumen des letzten Farb/Bindemittel (rein)
C                   (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C           7       Batchvolumen des letzten Farb/Bindemittels
C           8       Volumen des ersten Farb/Bindemittels (rein)
C                   (bei Bindemittel: Volumen des Gesamtbindemittels(rein))
C           9       Batchvolumen des ersten Farb/Bindemittels
C          10       Volumen des reinen Farb/Bindemittels (Festkoerpervolumen)
C          11       Volumen des gesamten Bindemittels (rein)
C          12       Volumen des Bindemittels (rein) im Bindmittelbatch
C          13       Volumen des Gesamtloesemittels
C          14       Volumen des Loesemittels im Bindemittelbatch
C          15       Volumen der *Farb-/Bindemittel (rein)
C          16       Volumen der *Farb-/Bindemittel (Batch)
C          17       Volumen der +Farb-/Bindemittel (rein)
C          18       Volumen der +Farb-/Bindemittel (Batch)        
C     IBI     Positionsnummer des Bindemittels
C     IER     Fehlercode
C
C     Anmerkung: Sind alle spezifischen Gewichte=1., so ist die Volumen-
C                berechnung identisch mit der Mengenberechnung 
C
C
      ALLOCATE(HILF(NC),STAT=IER)
      IF(IER.NE.0) THEN
         RETURN
      ENDIF
      CALL MKOEFF(NC,ICHF,CBA,CLI,SPE,SPLE,OP,HILF,INO,IER)
      IF(IER.NE.0) GOTO 99
      VMENG=0.D0
      DO I=1,NC
           VMENG=VMENG+HILF(I)*CC(I)
      ENDDO
      IF(VMENG.LT.-TOL) THEN
         IER=4108
      ENDIF
      IF(VMENG.LT.TOL) THEN
         VMENG=TOL
      ENDIF
  99  DEALLOCATE(HILF)
      RETURN
      END
      SUBROUTINE UKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
C
      DIMENSION CBA(*),CLI(*),SPE(*),AKOEF(*)
C
C     Umrechnung der Koeffizienten fuer Batchvolumina in solche fuer die
C     Mengen der Reinstoffe. Durch geeignete Wahl von CBA,CLI,SPE und SPLE 
C     sind auch andere Umrechnungen moeglich.
C    
C     AKOEF[NEU](I)=SUMME(AKOEF[ALT](K)*BETQ(K))
C      
C     
C     NC      Anzahl der Farb/Bindemittel
C     CBA     Mengenkonzentration des Farbmittels ( ist fuer Bindemittel =0.)
C     CLI     Mengenkonzentration des Bindemittels
C     SPE     spezifisches Gewicht der Farb/Bindemittel 
C     SPLE    spezifisches Gewicht des Loesemittels
C     AKOEF   Koeffizienten gemaess Normierung 
C     IBI     Positionsnummer des Bindemittels
C     IER     Fehlercode
C
C
C
C     ABFRAGEN
C
C
C
      IER=0
      BIBI=0.D0
      DO I=1,NC
         IF(CBA(I).LE.0.) GOTO 91
         IF(SPE(I).LE.0.) GOTO 92
         IF(I.EQ.IBI) THEN
              BIBI=AKOEF(I)
         ENDIF
      ENDDO
      IF(SPLE.LE.0.) GOTO 92
C
      DO I=1,NC 
         IF(I.EQ.IBI) THEN
            CBB=(1.-CBA(I))/CBA(I)
            SPI=1./SPE(I)
            AKOEF(I)=AKOEF(I)*(SPI+CBB/SPLE)
         ELSE
            SUU=0.D0
            FAK=1./SPLE
            IF(IBI.NE.0) THEN
               CBL=(1.D0-CBA(IBI))/CBA(IBI)
               FAK=FAK+CLI(I)*(1./SPE(IBI)-1./SPLE)
               SPI=1./SPE(IBI)
               SUU=BIBI*CLI(I)*(SPI+CBL/SPLE)
            ENDIF
            CBB=(1.D0-CBA(I))/CBA(I)
            AKOEF(I)=AKOEF(I)*(1./SPE(I)+CBB*FAK)-CBB*SUU
         ENDIF
      ENDDO
 99   RETURN
C
C
C
C
C
C
  91  IER=4081
      GOTO 99
  92  IER=4082
      GOTO 99
      END

      SUBROUTINE WKOEF(NC,IBI,CBA,CLI,SPE,SPLE,AKOEF,IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
C
      DIMENSION CBA(*),CLI(*),SPE(*),AKOEF(*)
      DATA ENE/1.0E-6/
C
C     Umrechnung der Koeffizienten fuer Reinmengen in solche fuer die
C     Batchvolumina. Durch geeignete Wahl von CBA,CLI,SPE und SPLE sind
C     auch andere Umrechnungen moeglich.
C      
C     AKOEF[NEU](I)=SUMME(AKOEF[ALT](K)*BET(K))
C
C     
C     NC      Anzahl der Farb/Bindemittel
C     CBA     Mengenkonzentration des Farbmittels ( ist fuer Bindemittel =0.)
C     CLI     Mengenkonzentration des Bindemittels
C     SPE     spezifisches Gewicht der Farb/Bindemittel 
C     SPLE    spezifisches Gewicht des Loesemittels
C     AKOEF   Koeffizienten gemaess Normierung 
C     IBI     Positionsnummer des Bindemittels
C     IER     Fehlercode
C
C
C
C     ABFRAGEN
C
C
C
      IER=0
      BIBI=0.D0
      DO I=1,NC
         IF(CBA(I).LE.0.) GOTO 91
         IF(SPE(I).LE.0.) GOTO 92
         IF(I.EQ.IBI) THEN
              BIBI=AKOEF(I)
         ENDIF
      ENDDO
      IF(SPLE.LE.0.) GOTO 92
C
      DO I=1,NC 
         IF(I.EQ.IBI) THEN
            CBB=(1.D0-CBA(I))/CBA(I)
            SPI=1./SPE(I)
            WNEN=SPI+CBB/SPLE
            IF(ABS(WNEN).GT.ENE) THEN
               AKOEF(I)=AKOEF(I)/WNEN
            ELSE
              AKOEF(I)=1.0/ENE
              GOTO 93
            ENDIF
         ELSE
            SUU=0.
            FAK=1./SPLE
            CBB=(1.D0-CBA(I))/CBA(I)
            IF(IBI.NE.0) THEN
               FAK=FAK+CLI(I)*(1./SPE(IBI)-1./SPLE)
               SUU=CLI(I)*CBB
            ENDIF
            SPI=1./SPE(I)
            WNEN=SPI+CBB*FAK
            IF(ABS(WNEN).GT.ENE) THEN
               AKOEF(I)=(AKOEF(I)+BIBI*SUU)/WNEN
            ELSE
              AKOEF(I)=1.0/ENE
              GOTO 93
            ENDIF
         ENDIF
      ENDDO
  99  RETURN
C
C
C
C
C
C
  91  IER=4081
      GOTO 99
  92  IER=4082
      GOTO 99
  93  IER=4178
      GOTO 99
      END
      SUBROUTINE MNGVOL(NC,CC,SPE)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      DIMENSION CC(*),SPE(*)
C
C
C     Mengen in Volumen umrechnen
C
      DO I=1,NC
         CC(I)=CC(I)/SPE(I)
      ENDDO
      RETURN
      ENTRY VOLMNG(NC,CC,SPE)
C
C
C     Volumen in Mengen Umrechnen
C
      DO I=1,NC
         CC(I)=CC(I)*SPE(I)
      ENDDO
      RETURN
      END
C
C 
                  
C
      SUBROUTINE ZUWAG(NF,RNEU,RALT,ZUWA,
     &           IER)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION RNEU(*),RALT(*),ZUWA(*)
      DATA TOL/1.D-6/
C
C******************************************************
C
C     BERECHNUNG DER ZUWAAGE ZUWA (=Reine Farb-/Bindemittelmenge)
C     AUS REINEN MENGEN (RNEU,RALT)
C
C     NF         ANZAHL FARB-BINDEMITTEL
C
C      AUSGABE GROESSEN
C
C      ZUWA      ZUWAAGEBATCHMENGE (REINMENGE)

C************************************************************************
C************************************************************************
C
C
C
C
C
C
        AMNEU=0.D0
        AMALT=0.D0
        DO I=1,NF
           AMNEU=AMNEU+RNEU(I)
           AMALT=AMALT+RALT(I)
        END DO
C

C
C
C 
C 
C       BERECHNUNG DES ZUWAAGE-FAKTORS
C       BEDINGUNG: RNEU(J)-RALT(J) >0. FUER ALLE J 
C       AMFKT = MAX(RALT(J)/RNEU(J),1.))
C 
C
C
      IER=0
      IEE=0
      FFZ=1.
      AMFKT=1.
      DO I=1,NF
           IF(RNEU(I).LE.TOL*AMNEU) THEN
              IF(RALT(I).GT.TOL*AMALT) THEN
                 IEE=-4074
                 EXIT
              ELSE
                 FFZ=0.D0
              ENDIF
           ELSE
              FFZ=RALT(I)/RNEU(I)
           ENDIF
           IF(FFZ.GT.AMFKT) THEN
              AMFKT=FFZ  
           ENDIF
      ENDDO 
C
      DO I=1,NF
        IF(IEE.EQ.0) THEN
          HILF=AMFKT*RNEU(I)
          ZUWA(I)=HILF-RALT(I)
          IF(ZUWA(I).LT.0.D0) THEN
            ZUWA(I)=0.D0
          ENDIF
        ELSE
          ZUWA(I)=-999.99
        ENDIF
      ENDDO
C
C     IER WIRD GESETZT, FALLS ZUWAAGENMINIMIERUNG AKTIV
C
C
      IER=IEE
      RETURN
      END
C
C

