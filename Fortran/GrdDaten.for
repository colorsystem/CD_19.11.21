C     Last change: KU 27.10.2021 10:12:51
C
C
c
c     INCLUDE "GRDREWR.FOR"
      INCLUDE "GETLWGK.FOR"
C      INCLUDE "MATLIB.FOR"
C      INCLUDE "QLalt.FOR"
C      INCLUDE "CNZDEP.FOR"
C      INCLUDE "FRBL0.FOR"
C
C     REFAS,TEFAS mit
C     Potenz für SQRT(1-w*OM0)
C     SQRT(1-w*OM0) ====> SQRT(1-w*OM0)**(1.-AKAB)
C      INCLUDE "TRFASN.FOR"
c     INCLUDE "NLINB.FOR"
C
C

C
      SUBROUTINE GRDBEG(NWEL,KML,NME,NFGL,NPQG,MMX,FEHL)
      USE MOTFEHL
      USE MODGRUN
      USE MODGRME
C
      INTEGER(KIND=4) :: IFEHL

      INTEGER(KIND=4) :: NME,KML,NWEL,NFGL,NPQG,MMX,IER,IERALC
      TYPE(TYFEH) :: FEHL

C
      DLL_EXPORT GRDBEG
C
      IER=0
      CALL FEHINI()
C
C     MODGRME
C
C
      ALLOCATE(FU(NME),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF

C
C
C     MODGRUN
C
C     NFG Anzahl Farb-/Bindemittel
C     MMX Anzahl Rezepte mit R-Werten
C
      NFG=NFGL
      NPQ1=NPQG
      MDIM=KML*NWEL
      NPQ=NPQ1-1
      MX=MMX
      NPDIM=NPQ1*NFG
      IF(ALLOCATED(PH)) THEN
         DEALLOCATE(PH,PDUM,RABL)
      ENDIF

      ALLOCATE(PH(0:NPQ1),PDUM(-2:NPQ1),RABL(NPQ1),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF


      ALLOCATE(RUM(NWEL,KML,NME),
     &         RWB(NWEL,KML,NME),RR(NWEL,KML,MMX),
     &         DR(NPDIM,MDIM),DRA(NPDIM,MDIM),
     &         CQ(0:NFG,MMX),DIK(KML,MMX),DIKALT(MMX),DAE(NFG),
     &         AKOP(NFG),AVAL(NPQ1,NFG),ADAE(NPQ1,NFG),
     &         PMSTZ(NFG),ACQ(NPQ1),
     &         LD(NPQG*NFGL),LCST(NPQG*NFGL),LC(NPQG*NFGL),LDIK(MMX),
     &         STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      ALLOCATE(ICHF(NFG),LX(20),KX(20),IRTU(NME),KUNT(NME),LCQ(NPQ1),
     &         NANZ(NFG),LQ(MDIM),KU(MMX),KQ(MDIM),NRRWRT(MMX),
     &         CCQ(NPQ1,NFG),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      IF(ALLOCATED(NST)) THEN
         DEALLOCATE(NST,CST,CSTA)
      ENDIF
C
      DO I=1,UBOUND(ICHF,1)
         ICHF(I)=0
      END DO
      DO I=1,UBOUND(NANZ,1)
         NANZ(I)=0
      END DO
      DO I=1,UBOUND(LD,1)
         LD(I)=0
      END DO
      DO I=1,UBOUND(LCST,1)
         LCST(I)=0
      END DO
      DO I=1,UBOUND(LC,1)
         LC(I)=0
      END DO
      DO I=1,UBOUND(LX,1)
         LX(I)=0
      END DO
      DO I=1,UBOUND(KX,1)
         KX(I)=0
      END DO
      DO I=1,UBOUND(IRTU,1)
         IRTU(I)=0
      END DO
      DO I=1,UBOUND(KUNT,1)
         KUNT(I)=0
      END DO
      DO I=1,UBOUND(LCQ,1)
         LCQ(I)=0
      END DO
      DO I=1,UBOUND(LQ,1)
         LQ(I)=0
      END DO
      DO I=1,UBOUND(KU,1)
         KU(I)=0
      END DO
      DO I=1,UBOUND(KQ,1)
         KQ(I)=0
      END DO
      DO J=1,UBOUND(CCQ,2)
        DO I=1,UBOUND(CCQ,1)
          CCQ(I,J)=' '
        END DO
      ENDDO

C
C
C
C
C     MODGRME
C
      ALLOCATE(CST(NPQ1,NFG,KML),NST(NFG),CSTA(NPQ1,NFG),
     &         FST(NFG),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      CALL TERMFDM(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      DO I=1,UBOUND(FST,1)
         FST(I)=1.0
      END DO

 900  CALL GETFEH(FEHL)
      RETURN
      END SUBROUTINE
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  GRDEND  **********************************************************************
C***************************************************************************************************************************
C***************************************  24.03.2005  **********************************************************************
C***************************************************************************************************************************

      SUBROUTINE GRDEND(FEHL)
      USE MOTFEHL
      USE MODGRUN
      USE MODGRME
c      USE MODSTTZ
C
      TYPE(TYFEH) :: FEHL
      INTEGER(KIND=4) :: IFEHL,IER,IERALC
C
      DLL_EXPORT GRDEND
C
      IER=0
      CALL FEHINI()
C
C     MODGRME
C
C
      IF(ALLOCATED(FU)) THEN
        DEALLOCATE(FU,STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
          GOTO 900
        ENDIF
      ENDIF
C

      IF(ALLOCATED(RR)) THEN
        DEALLOCATE(RUM, RWB,RR,DR,DRA,CQ,DIK,DIKALT,
     &         DAE,AKOP,AVAL,ADAE,PMSTZ,ACQ,
     &         LD,LCST,LC,LDIK,STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
          GOTO 900
        ENDIF
      ENDIF
      IF(ALLOCATED(PDUM)) THEN
        DEALLOCATE(PDUM,RABL,PH,STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
          GOTO 900
        ENDIF
      ENDIF
C
C
      IF(ALLOCATED(AGGR)) THEN
        DEALLOCATE(AGGR,ADIK,ACST,CU,CL,INC,STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
          GOTO 900
        ENDIF
      ENDIF


      IF(ALLOCATED(ICHF)) THEN
        DEALLOCATE(ICHF,LX,KX,IRTU,KUNT,LCQ,NANZ,LQ,KU,KQ,
     &             NRRWRT,CCQ,STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
          GOTO 900
        ENDIF
      ENDIF
C
C
C     MODGRME
C
C
      IF(ALLOCATED(NST)) THEN
        DEALLOCATE(CST,NST,CSTA,FST,STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
          GOTO 900
        ENDIF
      ENDIF
C
C
      CALL TERMFDM(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF

 900  CALL GETFEH(FEHL)
      RETURN
      END SUBROUTINE
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  GRDMEN  **********************************************************************
C***************************************************************************************************************************
C***************************************  24.03.2005  **********************************************************************
C***************************************************************************************************************************

      SUBROUTINE GRDMEN(FUL,EXPOL,GEWR,FTOL,GEWDIKL,JTML,IGLAEL,FEHL)
      USE MOTFEHL
      USE MODGRME
      IMPLICIT NONE
C
      INTEGER(KIND=4) :: IFEHL,I

      REAL(KIND=4) :: EXPOL,GEWR,FTOL,GEWDIKL
      INTEGER(KIND=4) :: JTML,IGLAEL,IER
      REAL(KIND=4),DIMENSION(*) ::FUL
      TYPE(TYFEH) :: FEHL
      INTEGER(KIND=4) :: NOPNPX
      REAL(KIND=8) ::SFU,TLL
      DATA TLL/1.E-5/
C
      DLL_EXPORT GRDMEN
C
C
      IER=0
      CALL FEHINI()
C
      SFU=0.0
      DO I=1,UBOUND(FU,1 )
         FU(I)=FUL(I)
         SFU=SFU+FU(I)
      END DO
      IF(SFU.LT.TLL) THEN
         IER=4042
         GOTO 900
      ENDIF 

      NPX=NOPNPX()
      EXPO=EXPOL
      GGW=GEWR
      JTM=JTML
      FTO=FTOL
      GEWDIK=GEWDIKL
      IGLAE=IGLAEL
C
C
C     STUTZSTELLEN FÜR CDE=TA usw.
C
C
      CALL STUETZST()
C
C
C

C 
C 
C
 900  IF(IFEHL(IER).NE.0) THEN
          CALL GETFEH(FEHL)
      ENDIF
      RETURN
      END
C     Last change:  UFO  24 Mar 105   10:36 am
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  GRDMGRF  **********************************************************************
C***************************************************************************************************************************
C***************************************  24.03.2005  **********************************************************************
C***************************************************************************************************************************
      SUBROUTINE GRDMGRF(NWEL,KML,NME,KFM,NDGRE,NAZ,
     &                 ICHL,FSTL,AKOWRTL,GRSTRL,GRDAEL,GRCCQL,
     &                 DIKREZ,RMENG,KWBUN,RETRUN,REDUN,
     &                 NRRWRTL,KWBRE,RETRRE,REDAM,FEHL)
      USE MOTFEHL
      USE MODGRUN
      USE MODGRME
      USE MODGKWR,ONLY:CDE

C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE(TYFEH) :: FEHL
      REAL(KIND=4),DIMENSION(0:9) :: AKOWRTL
      REAL(KIND=4),DIMENSION(NDGRE,0:9) :: GRSTRL
      REAL(KIND=4),DIMENSION(NDGRE,0:9) :: GRDAEL
      INTEGER(KIND=1),DIMENSION(NDGRE,0:9) :: GRCCQL

C 
      REAL(KIND=4),DIMENSION(KFM,*):: RMENG
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: REDAM,REDUN
      REAL(KIND=4),DIMENSION(*) :: DIKREZ,FSTL
      INTEGER(KIND=4),DIMENSION(*) :: KWBUN,RETRUN,KWBRE,RETRRE,
     &                                ICHL,NRRWRTL
      INTEGER(KIND=4) :: IFEHL
      LOGICAL(KIND=4):: LLOPT
      INTEGER(KIND=4) :: CNZDEP
C
C
C
C
      DLL_EXPORT GRDMGRF
C
C

      IER=0
      CALL FEHINI()
C
      NPX= NOPNPX()
      KM=KMS()
      NWE=NWS()
      DO J=1,KFM
         ICHF(J)=ICHL(J)
         FST(J)=0.01*FSTL(J)
C
C
C        KOPPLUNG ZWISCHEN VERSCHIEDENEN GRUNDDATEN-TYPEN
C        z.B. (SD und SSD Streuung(diffus-diffus) Streuung(gerichtet-diffus) 
C
C
         AKOP(J)=AKOWRTL(ICHF(J))
C
         IF(MOD(CNZDEP(),2).EQ.1) THEN
C
C              ANZAHL STÜTZSTELLEN für konzentrationsabhängige Grunddaten ('X', 'H')
C
C
               NANZ(J)=INT(GRSTRL(3,ICHF(J))+0.5)
               IF(NANZ(J).LE.0) THEN
                 NANZ(J)=1
               ENDIF
C
C              Abstand der Stützstellen ( PMSTZ<=1.0 ~ linear ; PMSTZ >> 1.0 ~ logarithmisch (s. PLIN bei GK-Werten))
C
C
               PMSTZ(J)=GRSTRL(4,ICHF(J))
C
C
C
C
C        KONZENTRATIONSABHÄNGIG
C
C
C
            DO L=1,NPQ
C
C              ANFANGSWERTE BZW. ANZUSTREBENDE WERTE
C
               CCQ(L,J)= CHAR(GRCCQL(1,ICHF(J)))
C
               AVAL(L,J)= GRSTRL(1,ICHF(J))
C
C              DAEMPFUNG 
C
               ADAE(L,J)= GRDAEL(1,ICHF(J))
C
            ENDDO
C
C
C           für  Albedo
C
            CCQ(NPQ1,J)= CHAR(GRCCQL(2,ICHF(J)))

C
C           ANFANGSWERTE BZW. ANZUSTREBENDE WERTE
C
C
            AVAL(NPQ1,J)= GRSTRL(2,ICHF(J))
C
C           DAEMPFUNG
C
            ADAE(NPQ1,J)= GRDAEL(2,ICHF(J))
C
C
         ELSE IF(MOD(CNZDEP(),2).EQ.0) THEN
C
C            konzentrationsabhängige Grunddaten ('Y' 'I')
C
C              ANZAHL STÜTZSTELLEN für konzentrationsabhängige Grunddaten ('X', 'H')
C
C
               NANZ(J)=INT(GRSTRL(3,ICHF(J))+0.5)
               IF(NANZ(J).LE.0) THEN
                 NANZ(J)=1
               ENDIF
C
C              Abstand der Stützstellen ( PMSTZ<=1.0 ~ linear ; PMSTZ >> 1.0 ~ logarithmisch (s. PLIN bei GK-Werten))
C
C
               PMSTZ(J)=GRSTRL(4,ICHF(J))
C
C
C
C
C        KONZENTRATIONSABHÄNGIG
C
C
C
            NPQ2=NPQ1/2
            DO L=1,NPQ2
C
C              ANFANGSWERTE BZW. ANZUSTREBENDE WERTE FÜR ABSORPTION
C
               CCQ(L,J)= CHAR(GRCCQL(1,ICHF(J)))
C
               AVAL(L,J)= GRSTRL(1,ICHF(J))
C
C              DAEMPFUNG 
C
               ADAE(L,J)= GRDAEL(1,ICHF(J))
C
            ENDDO
C
C
            DO L=1,NPQ2
C
C              ANFANGSWERTE BZW. ANZUSTREBENDE WERTE FÜR STREUUNG
C
               CCQ(L+NPQ2,J)= CHAR(GRCCQL(2,ICHF(J)))
C
               AVAL(L+NPQ2,J)= GRSTRL(2,ICHF(J))
C
C              DAEMPFUNG 
C
               ADAE(L+NPQ2,J)= GRDAEL(2,ICHF(J))
C
            ENDDO
C
         ELSE
C
C
C
C
C           NICHT KONZENTRATIONSABHÄNGIG
C
C
            DO L=1,NPX
               LL=L
               CCQ(L,J)=CHAR(GRCCQL(LL,ICHF(J)))
C
C              ANFANGSWERTE BZW. ANZUSTREBENDE WERTE
C
C
               AVAL(L,J)= GRSTRL(LL,ICHF(J))
C
C              DAEMPFUNG
C
               ADAE(L,J)= GRDAEL(LL,ICHF(J))
            ENDDO
         ENDIF
         IF(ICHF(J).EQ.8) THEN
C
C               ZUSATZSTOFFE
C
C
              DO L=1,NPQ1
                CCQ(L,J)='='
              ENDDO
C
C
         ENDIF

C
      ENDDO
C
C
C     Blank-Zeichen in "=" umwandeln
C
      DO J=1,KFM
        DO L=1,NPQ1
            IF(CCQ(L,J).EQ.' ') THEN
               CCQ(L,J)='='
            ENDIF
        END DO
      END DO


C
C
C
C     UNTERGRUENDE
C
C
      IRTU(1)=0
      IRTU(2)=0
      KUNT(1)=0
      KUNT(2)=0
      DO NU=1,2
         KUJ=KWBUN(NU)
         KUNT(NU)=KWBUN(NU)
         IF(KUJ.GT.0) THEN
           IRT=RETRUN(NU)
           IRTU(NU)=IRT
           DO KW=1,KM
              DO K=1,NWE
                RUM(K,KW,NU)=REDUN(K,KW,NU)
                RWB(K,KW,NU)=TRKORR(RUM(K,KW,NU),KW,IRT)
             ENDDO
           ENDDO
          ENDIF
      ENDDO

C
C
C
C
C     ANZAHL FARB-/BINDEMITTEL
C
      MF=KFM
C
C     ANZAHL REZEPTE
C
C
      M=0           
      DIKMIN=1.D130
C
C
C     REZEPTE ÜBERNEHMEN
C
C
      DO L=1,NAZ

C
         DIKALT(L)=DIKREZ(L)
C
C
C        ART DES UNTERGRUNDES
C
         IF(KWBRE(L).LT.0) THEN
C
C           REZEPTE MIT KWB<0 WERDEN UEBERSPRUNGEN
C
            CYCLE
         ENDIF
         M=M+1
         IF(LLOPT()) THEN
           KU(M)=0
         ELSE
           KU(M)=1
         ENDIF
         IF(KWBRE(L).EQ.KUNT(1)) THEN
           KU(M)=1
         ELSEIF(KWBRE(L).EQ.KUNT(2)) THEN
           KU(M)=2
         ENDIF
         NRRWRT(M)=NRRWRTL(L)
C
         IF(KU(M).EQ.0.OR.KU(M).GT.2) THEN
           IF(LLOPT()) THEN
C
C            Transparent
C
             IER=4077
             GOTO 900
           ELSE
C
C            Deckend (Untergrund spielt keine Rolle)
C
             KU(M)=1
           ENDIF
         ENDIF
         IRT= RETRRE(L)
         IF(IRTU(KU(M)).NE.IRT) THEN
           IF(LLOPT()) THEN
C
C            Transparent
C
            IER=4147
            GOTO 900
           ELSE
C
C           Deckend (nur Reflexion möglich)
C
           ENDIF
         ENDIF
C
C
C
C        SCHICHTDICKE
C
         CQ(0,M)=DIKREZ(L)
         DO I=1,KML
           DIK(I,M)=DIKREZ(L)
         ENDDO
         LDIK(M)=L

C
C        MENGEN
C
         DO I=1,MF
            CQ(I,M)=RMENG(I,L)
         END DO

         SUU=0.
         DO K=1,MF
           SUU=SUU+CQ(K,M)
         END DO

         IF(CQ(0,M).LT.DIKMIN) THEN
             DIKMIN=CQ(0,M)
         ENDIF
C
C        REFLEXIONSWERTE
C
C
         DO KW=1,KM
            DO K=1,NWE
              RR(K,KW,M)=REDAM(K,KW,L)
            ENDDO
         ENDDO
      ENDDO
C
C
C
C
C     OBERE GRENZE FÜR GRUNDDATEN
C
C
C
      IF(LLOPT()) THEN
C
C       SCHICHT IST NICHT DECKEND
C
        IF (DIKMIN.EQ.0.) THEN
            DIKMIN=1.
        END IF
        GRMAX=1.D5/DIKMIN
      ELSE
C
C       SCHICHT IST DECKEND
C
        GRMAX=1.D5
C
      ENDIF
 900  IF(IFEHL(IER).NE.0) THEN
         GOTO 999
      ENDIF
 999  CALL GETFEH(FEHL)
      RETURN
      END SUBROUTINE
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  GRDGRUN  **********************************************************************
C***************************************************************************************************************************
C***************************************  24.03.2005  **********************************************************************
C***************************************************************************************************************************
      SUBROUTINE GRDGRUN(KFU,KFO,NWEL,KML,NPQG,NSTL,CSTL,GRUND,FEHL)
      USE MOTFEHL
      USE MODGRUN
      USE MODGRME,ONLY:DICK
      USE MODGKWR,ONLY:CDE

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      TYPE(TYFEH) :: FEHL
      REAL(KIND=8) :: CSUM,CCST
      INTEGER(KIND=4) :: CNZDEP
      REAL(KIND=4),DIMENSION(NWEL,KML,NPQG,*) :: GRUND
      REAL(KIND=4),DIMENSION(KML,NPQG,*)::CSTL
      REAL(KIND=8),DIMENSION(2,MAX(KFU,KFO)) :: CHILF
      INTEGER(KIND=4),DIMENSION(*) :: NSTL
      INTEGER(KIND=4) :: NOPNPX,NPNPS
C
C
C
C
C
      DLL_EXPORT GRDGRUN
C
C
C
      IER=0
      CALL FEHINI()
C
      IF(KFO.GT.UBOUND(ICHF,1)) THEN
         IER=4039
         GOTO 900
      ENDIF
      KM=KML
      NWE=NWEL
C
C
C     STÜTZSTELLEN für konzentrationsabhängige Grunddaten
C
C
C
C     KOPPLUNG,DAEMPFUNG UND STANDARD-STARTWERTE SETZEN
C


      DO K=KFU,KFO
         NSTL(K)=0
         DO I=1,NPQG
            DO KW=1,KML
               CSTL(KW,I,K)=0.
            ENDDO
         ENDDO
      ENDDO
C
C
C
C
C
      IER=0
      DICK=0.D0
      DO L=1,M
        IF(CQ(0,L).GT.DICK) DICK=CQ(0,L)
      ENDDO
C

C
C
C
C
C
      IF(CNZDEP().GT.0) THEN
C
C
C
C     BERECHNUNG VON STUETZSTELLEN für konzentrationsabhängige Grunddaten
C
C

          DO K=KFU,KFO
            CHILF(1,K)=0.
            CHILF(2,K)=DICK
          ENDDO
          DO L=1,M
             CSUM=0.
             DO K=1,MF
                CSUM=CSUM+CQ(K,L)
             ENDDO
             DO K=KFU,KFO
                   IF(CQ(K,L).GT.0.) THEN
C
C                     STUETZSTELLEN WERDEN MIT HILFE DER GEGEBENEN
C                     SCHICHTDICKE CQ(0,L)
C                     SCHICHTDICKE D(1). DAMIT KÖNNEN UNTERSCHIEDLICHE
C                     SCHICHTDICKEN BEI DER KONZENTRATIONSABHÄNGIGKEIT
C                     BERUECKSICHTIGT WERDEN.
C                     AUSSERDEM WERDEN DIE KONZENTRATIONSABHAENGIGEN OPT.KONST
C                     NUR VON DEN REINEN FARBMITTEL AB. CQ(1,L) wird ENTSPRECHEND
C                     KORRIGIERT
                      CH=CQ(0,L)*CQ(K,L)/CSUM
C
C
                      IF(CH.GT.CHILF(1,K)) CHILF(1,K)=CH
                      IF(CH.LT.CHILF(2,K).AND.CH.GT.0.) CHILF(2,K)=CH
                   ENDIF
             ENDDO
          ENDDO
C
C
C
C
C
C
C
         DO K=KFU,KFO
            IF(CHILF(1,K).EQ.0.) THEN
               CHILF(1,K)=DICK
            ENDIF
            IF(ABS(CHILF(2,K)-DICK).LE.TINY(1.D0)) THEN
               CHILF(2,K)=0.1*DICK
            ENDIF
         ENDDO


         DO K=KFU,KFO
            QUS=CHILF(2,K)
            QOS=CHILF(1,K)
            NSTL(K)=NANZ(K)
            PM=PMSTZ(K)
            DO J=1,NSTL(K)
              DO KW=1,KML
               CSTL(KW,J,K)=CCST(J,NSTL(K),PM,QUS,QOS)
              END DO
            ENDDO
         ENDDO
      ENDIF
 
C


      DO J=KFU,KFO

C
C
C
C
C
         IF(MOD(CNZDEP(),2).EQ.1) THEN
C
C        'X' und 'H'

C
C
C
C
C
C        KONZENTRATIONSABHÄNGIG
C
C
            NST1=NSTL(J)+1
            IF(NST1.GT.NPQG) THEN
               IER=4163
               GOTO 900
            ENDIF
            DO L=1,NSTL(J)
C
C
               DO KW=1,KML
                 DO I=1,NWEL
                   GRUND(I,KW,L,J)=AVAL(L,J)
                 ENDDO
               ENDDO
            ENDDO
C
C
C

            DO KW=1,KM
              DO I=1,NWE
                GRUND(I,KW,NST1,J)=AVAL(NPQG,J)
              ENDDO
            ENDDO
         ELSE IF(MOD(CNZDEP(),2).EQ.0) THEN
C
C        'Y' und 'I'

C
C
C
C
C
C        KONZENTRATIONSABHÄNGIG
C
C
            NPQ2=NPQG/2
            IF(2*NPQ2.NE.NPQG) THEN
               IER=4163
               GOTO 900
            ENDIF
            DO L=1,NSTL(J)
C
C
               DO KW=1,KML
                 DO I=1,NWEL
                   GRUND(I,KW,L,J)=AVAL(L,J)
                   GRUND(I,KW,L+NSTL(J),J)=AVAL(L+NPQ2,J)
                 ENDDO
               ENDDO
            ENDDO
C
C
C
         ELSE
C
C
C
C
C           NICHT KONZENTRATIONSABHÄNGIG
C
C
            DO L=1,NOPNPX()
C
               DO KW=1,KM
                 DO I=1,NWE
                    GRUND(I,KW,L,J)=AVAL(L,J)
                 ENDDO
               ENDDO
            ENDDO
         ENDIF
C
       ENDDO
       DO K=KFU,KFO
         NJJ=0
         IF(CNZDEP().GT.0) THEN
           NJJ=NSTL(K)
         ENDIF
         DO J=NJJ+1,NPQG
           DO KW=1,KML
            CSTL(KW,J,K)=0.0
           END DO
         ENDDO
       END DO


 900  IF(IFEHL(IER).NE.0)THEN
         GOTO 999
      ENDIF
C 
 999  CALL GETFEH(FEHL)
      RETURN
      END SUBROUTINE
C***************************************************************************************************************************

C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  GRDLIM  **********************************************************************
C***************************************************************************************************************************
C***************************************  28.01.2005  **********************************************************************
C***************************************************************************************************************************

      SUBROUTINE GRDLIM(KFI,KFA,KML,NPQG,NSTL,CSTL,FEHL)
      USE MOTFEHL
      USE MODGRUN
      USE MODGRME,ONLY:GRMAX,NPP,NZUS,NPX,PH,IGLAE
      USE MODGKWR,ONLY:CDE

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE(TYFEH) :: FEHL
      REAL(KIND=8) ::GFK
      INTEGER(KIND=4) :: NFGL
      INTEGER(KIND=4) ::KFI,KFA,IFEHL,ISCH,NOPNPP,NOPNPX,NPNPS
      INTEGER(KIND=4),DIMENSION(*) :: NSTL
      REAL(KIND=4),DIMENSION(KML,NPQG,*) :: CSTL
      LOGICAL(KIND=4) :: LLOPT
      INTEGER(KIND=4) :: CNZDEP
      REAL(KIND=8),DIMENSION(NPQG) ::ACQL
      INTEGER(KIND=4),DIMENSION(NPQG) ::LCQL
C
C
C               KOPPLUNGEN BEI VORGEGEBENEN KOPPLUNGSPARAMETERN GRCCQ
C               REIHENFOLGE S.UNTEN
C
C
C                REAL(kind=4),dimension(0:9) :: AKOWRT
C
C               STARTWERTE FUER GRUNDDATEN
C               z.B. GRSTR(1,0) erster zu berechnender Wert fuer Farbmittel
C
C
C
C
C
      DLL_EXPORT GRDLIM
C
c      OPEN(29,FILE='TESTAGGR.TXT')

C
C
      IER=0
      CALL FEHINI()
      DO I=1,UBOUND(LD,1)
         LD(I)=0
      END DO
      DO I=1,UBOUND(LCST,1)
         LCST(I)=0
      END DO
      DO I=1,UBOUND(LC,1)
         LC(I)=0
      END DO
      NFGL=KFI+KFA
      NPX=NOPNPX()
      NPP=NOPNPP()
      DO I=1,NFGL
         NSTH=NSTL(I)
         NSTH=NPNPS(NPX,NSTH)
         IF(NSTH.GT.NPQG) THEN
           IER=4163
           GOTO 900
         ENDIF
      END DO
      NSTH=0
C
C
      NDIK=0
      IF(IGLAE.GT.2.AND.LLOPT().AND.CNZDEP().EQ.-1) THEN
C
C     Dickenoptimierung nur für konzentrationsunabhängigen Daten und transparenten Schgichten
C
         NDIK=M
      ENDIF
      NCST=0
      IF(IGLAE.EQ.2.AND.CNZDEP().GT.0) THEN
        NDIK=0
C
C       Max. Liklihood für die Stützkonzentrationen (CST) bei konzentrationsabhängigen Grunddaten
C       (nur wenn Anzahl Winkel/Messgeometrien (KMS) = 1)
        DO I=1,NFGL
          NCST=NCST+NSTL(I)
        END DO
      ENDIF

      MDIM=10*NFGL
      ALLOCATE(CL(MDIM+NFGL*NPQG),CU(MDIM+NFGL*NPQG),
     &        INC(MDIM+NFGL*NPQG),AGGR(MDIM,NFGL*NPQG+1),
     &        ADIK(MX,NDIK+1),ACST(NCST,NCST+1),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      DO I=1,UBOUND(INC,1)
         INC(I)=0
      END DO
C
C
C
      IF(MF.NE.KFI+KFA) THEN
        IER=4066
        GOTO 900
      ENDIF
C
C     PRÜFEN, OB Eichfärbungen gefunden
C
C
C
      IF(M.EQ.0) THEN
         IER=4044
         GOTO 900
      ENDIF
      IF(MOD(CNZDEP(),2).EQ.1) THEN
C
C       Konzentrationsabhängig  ('X' und 'H')
C
        DO J=1,MF
            NST1=NSTL(J)+1

            CCQ(NST1,J)= CCQ(NPQG,J)
C
C           ANFANGSWERTE BZW. ANZUSTREBENDE WERTE
C
C
            AVAL(NST1,J)= AVAL(NPQG,J)
C           DAEMPFUNG
C
            ADAE(NST1,J)=ADAE(NPQG,J)

        END DO
      ELSEIF(MOD(CNZDEP(),2).EQ.0) THEN
C
C       Konzentrationsabhängig  ('Y' und 'I')
C

        NPQ2=NPQG/2
        DO J=1,MF
          DO K=1,NSTL(J)

            CCQ(K+NSTL(J),J)= CCQ(K+NPQ2,J)
C
C           ANFANGSWERTE BZW. ANZUSTREBENDE WERTE
C
C
            AVAL(K+NSTL(J),J)= AVAL(K+NPQ2,J)
C           DAEMPFUNG
C
            ADAE(K+NSTL(J),J)=ADAE(K+NPQ2,J)
          END DO
        END DO

      ENDIF


C
C     CCQ für konstante Grunddaten CCQ="=" setzen
C
      DO J=1,KFI
C
C
         IF(MOD(CNZDEP(),2).EQ.1) THEN
C
C
C           KONZENTRATIONSABHÄNGIG 'X' und 'H'
C
C
            NST1=NSTL(J)+1
            DO L=1,NST1
C
C

C
C              KENNUNG SETZEN, DASS OPTISCHE DATEN FÜR J<KFU  KONSTANT BLEIBEN
C
               CCQ(L,J)='='
C

C
C
C
C
C
C
            ENDDO
        ELSEIF(MOD(CNZDEP(),2).EQ.0) THEN
C
C
C           KONZENTRATIONSABHÄNGIG 'Y' und 'I'
C
C
            DO L=1,2*NSTL(J)
C
C

C
C              KENNUNG SETZEN, DASS OPTISCHE DATEN FÜR J<KFU  KONSTANT BLEIBEN
C
               CCQ(L,J)='='
C

C
C
C
C
            ENDDO
         ELSE
C
C
C
C
C
C        NICHT KONZENTRATIONSABHÄNGIG
C
C
C
C
            DO L=1,NOPNPX()
C
C              KENNUNG SETZEN, DASS OPTISCHE DATEN FÜR J<KFU  KONSTANT BLEIBEN
C
               CCQ(L,J)='='
C
            ENDDO
         ENDIF
C
      ENDDO

C




      IS=0
      ICST=0
      DO J=1,MF
          NPHLF=NPNPS(NPX,NSTL(J))
          DO L=1,NPHLF

C
C           Anzahl (IS) und Position der variablen Parameter bestimmen
C
C
            IF(CCQ(L,J).NE.'=') THEN
               IS=IS+1                 
               LD(IS)=NPQG*(J-1)+L
               IF(NCST.GT.0.AND.L.LT.NSTL(J)) THEN
                 ICST=ICST+1
C
C                Position für variable CST für max. Likelihood CST
C
C
                 LCST(ICST)=NPQG*(J-1)+L
               ENDIF
            ENDIF
          ENDDO
      ENDDO
C
C
      IX=0
      MT=MF
      NGL=IS
      IF(NGL.LE.0) THEN
         DO J=KFI+1,KFI+KFA
            IF(ICHF(J).EQ.8) THEN
              GOTO 900
            ENDIF
         END DO
         IER=4047
         GOTO 900
      ENDIF
C
C
C       STANDARDGRENZEN
C
      KG=0
      DO  IA=1,IS
          KG=KG+1
          LDI=LD(IA)
          J=(LDI-1)/NPQG+1
          INC(KG)=3
          CL(KG)=0.0
          CU(KG)=GRMAX
          IF(MOD(CNZDEP(),2).EQ.1) THEN
C
C            KONZENTRATIONSABHÄNGIG FÜR ALBEDO ('X' und 'H')
C
C
             ISCH=MOD(LDI-1,NPQG)+1
             IF(ISCH.EQ.NSTL(J)+1) THEN
                INC(KG)=3
                CU(KG)=1.
             ENDIF
          ENDIF
      ENDDO
      DO K=1,IX
          KG=KG+1
          INC(KG)=3
          CL(KG)=0.
          CU(KG)=1.E35
      ENDDO
C               LINEARE NEBENBEDINGUNGEN
C 
      NEG=0
C
C
C       Größer(>),kleiner(<), und Verhältnisse (/,\,:)
C
      IA=0
      DO J=1,MF
C
C
C        Kopplung >,<,/,\,:
C
C
        NPHLF=NPNPS(NPX,NSTL(J))

        DO L=1,NPHLF
            IF(CCQ(L,J).NE.'=') THEN
               IA=IA+1
               LCQL(L)=IA
               ACQL(L)=AVAL(L,J)
            ENDIF
            IF(CCQ(L,J).EQ.'>') THEN
               CL(IA)=AVAL(L,J)
               INC(IA)=1
            ENDIF
            IF(CCQ(L,J).EQ.'<') THEN
               CU(IA)=AVAL(L,J)
               INC(IA)=2
            ENDIF
C
C
C           SPEZIALKOPPLUNG FÜR E0 und S0 (6 Konstanten)
C           SD=S1+S2 oder SS=S1+S2
C
            IF(CDE(1:1).EQ.'E'.OR.CDE(1:1).EQ.'S') THEN
              IF(CCQ(L,J).EQ.'&') THEN
              IF(L.EQ.2.OR.L.EQ.6) THEN
                NEG=NEG+1
                DO I=1,IS+1
                   AGGR(NEG,I)=0.
                END DO
                AGGR(NEG,IA)=1.
                IVV=0
                IF(L.EQ.6) THEN
                  IVV=-4
                ENDIF
                DO I=1,2
                  IF(CCQ(L+I+IVV,J).NE.'=') THEN
                    AGGR(NEG,IA+I+IVV)=-1.
                  ENDIF
                END DO
                CU(NGL+NEG)=0.
                CL(NGL+NEG)=0.
                INC(NGL+NEG)=3
              ENDIF
              ENDIF
            ENDIF
C
            IF(CCQ(L,J).EQ.'/'
     &      .OR.CCQ(L,J).EQ.'\') THEN
             DO L1=1,L-1
                IF(CCQ(L1,J).EQ.CCQ(L,J)) THEN
                   NEG=NEG+1
                   IF(NEG.GT.MDIM) THEN
                      IER=4045
                      GOTO 900
                   ENDIF
                   DO I=1,IS+1
                      AGGR(NEG,I)=0.
                   ENDDO
                   AGGR(NEG,IA)=ACQL(L1)
                   AGGR(NEG,LCQL(L1))=-AVAL(L,J)
                   CU(NGL+NEG)=0.
                   CL(NGL+NEG)=0.
                   INC(NGL+NEG)=3
                   EXIT
                ENDIF
              ENDDO
             ENDIF
C
        ENDDO
      ENDDO
C
      NEE=0
C
C
C       Zusätzliche Minimierungsbedingungen NEG+1     NEG+NEE
C       für konzentrationsabhängige Grunddaten aufbauen
C
C
      IF(CNZDEP().GT.0) THEN
C
C
        DO J=1,MF
C
C       PRUEFEN, OB ERSTE NST(J) KOPPLUNGEN UEBEREINSTIMMEN, FALLS @,A,B,C,D,E,F
C
         IF(ICHAR(CCQ(1,J)).GE.64.AND.ICHAR(CCQ(1,J)).LE.70) THEN
           DO L=2,NSTL(J)
             IF(CCQ(L,J).NE.CCQ(1,J)) THEN
C
C              KOPPLUNG BEI CDE=X,H,Y,I
C              MUSS BEI DEN ERSTEN NST(J) PARAMETERN GLEICH SEIN
C
               IER=4106
               GOTO 900
             ENDIF
             IF(MOD(CNZDEP(),2).EQ.0) THEN
               IF(CCQ(L+NSTL(J),J).NE.CCQ(NSTL(J)+1,J)) THEN
C
C                KOPPLUNG BEI CDE=Y,I
C                MUSS BEI  PARAMETERN NST(J)+1 ...2*NST(J) GLEICH SEIN
C
                 IER=4106
                 GOTO 900
               ENDIF
             ENDIF
           ENDDO
         ENDIF
        ENDDO
      ENDIF


      IA=0
      DO J=1,MF

         IF(CNZDEP().GT.0) THEN

C
C         KONZENTRATIONSABHÄNGIG
C
C         ZUSÄTZLICHE MINIMIERUNGSBEDINGUNGEN AUFBAUEN
C
          NPHLF=NPNPS(NPX,NSTL(J))
          DO L=1,NPHLF
            IF(CCQ(L,J).NE.'=') THEN
               IA=IA+1
            ENDIF
C
C
C           Kopplung @,A,B,....
C
C
C
C
C
            IF(ICHAR(CCQ(L,J)).GE.64.AND.ICHAR(CCQ(L,J)).LE.70) THEN
              GFK=AKOP(J)
C
C              'X' und 'H'und 'Y' und 'I'
C
C
               IF((L.GE.2.AND.L.LE.NSTL(J)-1.AND.GFK.GT.0.D0)
     &         .OR.(MOD(CNZDEP(),2).EQ.0.AND.
     &         (L.GE.NSTL(J)+2.AND.L.LE.2*NSTL(J)-1.AND.GFK.GT.0.D0)))
     &         THEN
                  NEE=NEE+1
                  IF(NEG+NEE.GT.MDIM) THEN
                      IER=4045
                      GOTO 900
                  ENDIF
                  DO I=1,IS+1
                     AGGR(NEG+NEE,I)=0.
                  ENDDO   
                  IF(CCQ(L,J).EQ.'@') THEN
                     AGGR(NEG+NEE,IA-1)=-1.*GFK
                     AGGR(NEG+NEE,IA)=1.*GFK
                     IF(L.EQ.NSTL(J)-1.OR.L.EQ.2.*NSTL(J)-1) THEN
                        NEE=NEE+1
                        DO I=1,IS+1
                          AGGR(NEG+NEE,I)=0.
                        ENDDO
                        AGGR(NEG+NEE,IA)=-1.*GFK
                        AGGR(NEG+NEE,IA+1)=1.*GFK
                     ENDIF                   
                  ELSE
                    LL=L
                    IF(MOD(CNZDEP(),2).EQ.0
     &               .AND.L.GT.NSTL(J)) THEN
                        LL=L-NSTL(J)
                    ENDIF
                    CNEN=CSTL(1,LL+1,J)-CSTL(1,LL-1,J)
                    IF(CNEN.NE.0.) THEN
                     AGGR(NEG+NEE,IA-1)=GFK
     &                    *(CSTL(1,LL,J)-CSTL(1,LL+1,J))/CNEN
                     AGGR(NEG+NEE,IA+1)=GFK
     &                    *(CSTL(1,LL-1,J)-CSTL(1,LL,J))/CNEN
                     AGGR(NEG+NEE,IA)  =GFK
        	    ENDIF
                  ENDIF
               ENDIF
C


            ENDIF
          ENDDO
C
         ELSE
C
C
C         NICHT KONZENTRATIONSABHÄNGIG
C
C
C
C
C
C   
          GFK=AKOP(J)
          NPHLF=NPNPS(NPX,NSTL(J))
          DO L=1,NPHLF
            IF(CCQ(L,J).NE.'=') THEN
               IA=IA+1                 
               LCQL(L)=IA
               ACQL(L)=AVAL(L,J)
            ENDIF
C
C
c
c
C           KOPPLUNG ':' und '%'
c
c
c
c
            IF(CCQ(L,J).EQ.':'
     &      .OR.CCQ(L,J).EQ.'%') THEN
             DO L1=1,L-1
                IF(CCQ(L1,J).EQ.CCQ(L,J)) THEN
                   NEE=NEE+1
                   IF(NEG+NEE.GT.MDIM) THEN
                      IER=4045
                      GOTO 900
                   ENDIF
                   DO I=1,IS+1
                      AGGR(NEG+NEE,I)=0.
                   ENDDO
                   AGGR(NEG+NEE,IA)=GFK*ACQL(L1)
                   AGGR(NEG+NEE,LCQL(L1))=-GFK*AVAL(L,J)
c                   WRITE(29,*) 'IS,J,NEG,NEE,GFK,ACQL(L1),AVAL(L,J)',
c     &                          IS,J,NEG,NEE,GFK,ACQL(L1),AVAL(L,J)
                   EXIT
                ENDIF
              ENDDO
             ENDIF
c
c
c
c
c
c
c
c
C
C           Kopplung @,A,B,....
C
C
            IF(ICHAR(CCQ(L,J)).GE.64.AND.ICHAR(CCQ(L,J)).LE.70) THEN
             IF(CDE(1:1).EQ.'V') THEN
C
C
C
C            KOPPLUNG VON TAU MIT AD+SD (GFK*(TAU-AD-SD))=0.
C
C
C
              IF(L.EQ.3)THEN
                IF(CCQ(L-1,J).NE.'='.AND.CCQ(L-2,J).NE.'=') THEN
                  NEE=NEE+1
                  DO I=1,IS+1
                       AGGR(NEG+NEE,I)=0.
                  ENDDO
                  AGGR(NEG+NEE,IA)= 1.*GFK
                  AGGR(NEG+NEE,IA-1)= -1.*GFK
                  AGGR(NEG+NEE,IA-2)= -1.*GFK
                ELSE IF(CCQ(L-1,J).EQ.'='.AND.CCQ(L-2,J).NE.'=') THEN
                  NEE=NEE+1
                  DO I=1,IS+1
                       AGGR(NEG+NEE,I)=0.
                  ENDDO
                  AGGR(NEG+NEE,IA)= 1.*GFK
                  AGGR(NEG+NEE,IA-1)= -1.*GFK
                  AGGR(NEG+NEE,IS+1)= 1.*GFK*AVAL(L-1,J)
                ELSE IF(CCQ(L-1,J).NE.'='.AND.CCQ(L-2,J).EQ.'=') THEN
                  NEE=NEE+1
                  DO I=1,IS+1
                       AGGR(NEG+NEE,I)=0.
                  ENDDO
                  AGGR(NEG+NEE,IA)= 1.*GFK
                  AGGR(NEG+NEE,IA-1)= -1.*GFK
                  AGGR(NEG+NEE,IS+1)= 1.*GFK*AVAL(L-2,J)
                ENDIF
               ENDIF
             ELSE
               DO L1=1,L-1
                 IF(CCQ(L1,J).EQ.CCQ(L,J).AND.GFK.GT.0.D0) THEN
                   NEE=NEE+1
                   IF(NEG+NEE.GT.MDIM) THEN
                      IER=4045
                      GOTO 900
                   ENDIF
                   DO I=1,IS+1
                      AGGR(NEG+NEE,I)=0.
                   ENDDO
                   AGGR(NEG+NEE,IA)=-1.*GFK*ACQL(L1)
                   AGGR(NEG+NEE,LCQL(L1))=1.*GFK*AVAL(L,J)
                   EXIT
                 ENDIF
               ENDDO
             ENDIF
            ENDIF
C
         ENDDO
        ENDIF
      ENDDO
c      WRITE(29,*) 'AKOP',(AKOP(J),J=1,MF)
c      WRITE(29,*) 'ICHF',(ICHF(J),J=1,MF)
C
C
C
C
C
C
C
C
C
C     DAEMPFUNG, FALLS GRDAE=ADAE VORHANDEN (STANDARD)
C
C       
      IA=0
      NEV=0
      DO J=1,MF
         NPHLF=NPNPS(NPX,NSTL(J))
         DO L=1,NPHLF
           IF(CCQ(L,J).NE.'=') THEN
              IA=IA+1
              IF(ADAE(L,J).NE.0.) THEN
                 NEE=NEE+1
                 DO IB=1,IS+1
                   AGGR(NEG+NEE,IB)=0.
                 ENDDO
                 NEV=NEV+1
                 LC(NEV)=IA
                 AGGR(NEG+NEE,IA)=ADAE(L,J)
C
C                DÄMPFUNGSZIEL X ==> DRA
C
                 AGGR(NEG+NEE,IS+1)=ADAE(L,J)
               ENDIF
           ENDIF
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
      DO II=1,IX
         LX(II)=II
      ENDDO
      DO I=1,NGL
        IF(INC(I).EQ.1.OR.INC(I).EQ.4) THEN
           CU(I)=1.D60
C
C
C
C          VERBESSERTE WAHL DER OBEREN GRENZE FUER TRANSPARENTE SCHICHTEN
C
C
C
        ENDIF
        IF(INC(I).EQ.2.OR.INC(I).EQ.4) THEN
           CL(I)=0.D0
        ENDIF
      END DO
      ME=0
      NEH=0
      DO I =1,NEG
        IF(INC(NGL+I).EQ.3) THEN
          IF(ABS(CU(NGL+I)-CL(NGL+I)).LT.1.E-4) THEN
C
C
C           GLEICHUNG GEFUNDEN
C
            ME=ME+1
            DO J=1,NGL
              SAV=AGGR(I,J)
              AGGR(I,J)=AGGR(ME,J)
              AGGR(ME,J)=SAV
            END DO
            SAV=CU(NGL+I)
            CU(NGL+I)= CU(NGL+ME)
            CU(NGL+ME)=SAV
            SAV=CL(NGL+I)
            CL(NGL+I)= CL(NGL+ME)
            CL(NGL+ME)=SAV
            ISAV=INC(NGL+I)
            INC(NGL+I)= INC(NGL+ME)
            INC(NGL+ME)=ISAV
           ELSE
C
C
C           NEUE UNGLEICHUNG HINZUFÜGEN
C
C
C          VERSCHIEBEN
C
C
           DO K=1,NEE
              DO J=1,NGL+1
                 AGGR(NEG+NEH+NEE-K+2,J)=AGGR(NEG+NEH+NEE-K+1,J)
              END DO
           END DO
           NEH=NEH+1
           DO J=1,NGL
              AGGR(NEG+NEH,J)=-AGGR(I,J)
           END DO
           CL(NGL+NEG+NEH)=-CU(NGL+NEG+NEH)
          ENDIF
        ENDIF
        IF(INC(NGL+I).EQ.2) THEN
           DO J=1,NGL
             AGGR(I,J)=-AGGR(I,J)
           END DO
           CL(NGL+I)=-CU(NGL+I)
        ENDIF
      END DO
      NEG=NEG+NEH
      DO I=1,NEG
         AGGR(I,NGL+1)=CL(NGL+I)
      END DO
C
C
C
C     Gleichungen für max. Likelihood (Dicken)
C
      NED=0
      IF(NDIK.GT.0.AND.IGLAE.EQ.4) THEN
         DO I=1,NDIK-1
            DO J=I+1,NDIK
              IF(NRRWRT(I).EQ.NRRWRT(J)) THEN
                 NED=NED+1
                 DO L=1,NDIK+1
                   ADIK(NED,L)=0.0
                 END DO
C
C                DIK(weiß)/DIK(schwarz)=DIK(weiß[alt]/DIK(schwarz[alt])
C
C
                 ADIK(NED,I)=CQ(0,J)
                 ADIK(NED,J)=-CQ(0,I)
              ENDIF
            END DO
         END DO
      ENDIF
C
C
C     Ungleichungen für Stützstellen CST bei Konzentrationsabhängigkeit der Grunddaten
       
C
      NEC=0
      DO K=1,ICST
        J=(LCST(K)-1)/NPQG+1
        L=LCST(K)-(J-1)*NPQG
        IF(L.GT.1.AND.L.LT.NSTL(J)) THEN
           NEC=NEC+1
           DO I=1,NCST+1
             ACST(NEC,I)=0.0
           END DO
           ACST(NEC,K-1)=-1.0
           ACST(NEC,K)=1.0
           ACST(NEC,ICST+1)=0.001
        ENDIF
      END DO
C
c
 900  IF(IFEHL(IER).NE.0)THEN
         GOTO 999
      ENDIF
C
 999  CALL GETFEH(FEHL)
c      WRITE(29,*) 'NEE,NEG,IS',NEE,NEG,IS
c      DO I=1,NEE+NEG
c        WRITE(29,*) 'AGGR',(AGGR(i,J),J=1,IS+1)
c      END DO
c      CLOSE(29)
      RETURN
      END SUBROUTINE


C
C
C
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  GRDCALC  **********************************************************************
C***************************************************************************************************************************
C***************************************  28.01.2005  **********************************************************************
C***************************************************************************************************************************

      SUBROUTINE GRDCALC(NWEL,KML,NPQG,NSTL,CSTL,DIKNEU,GRUND,GRUGRZ,
     &                   PHHI,FEHL)
      USE MOTFEHL
      USE MODGRUN
      USE MODGRME
      USE MODGKWR,ONLY:CDE,GK
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 




C
      TYPE(TYFEH) FEHL
C
      REAL(KIND=4),DIMENSION(NWEL,KML,NPQG,*) :: GRUND,GRUGRZ
      REAL(KIND=4),DIMENSION(KML,NPQG,*)::CSTL
      REAL(KIND=4),DIMENSION(KML,*) :: DIKNEU
      INTEGER(KIND=4),DIMENSION(*) :: NSTL
      INTEGER(KIND=4) :: NWEL,KML,NPQG,IFEHL,NOPNPP,NOPNPX
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: XLB,XLG
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: XUB
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: X,X0,XX
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: XS
      DOUBLE PRECISION FVALUES(4)
      REAL*4 PHHI
      LOGICAL(KIND=4) :: LLOPT
      INTEGER(KIND=4) :: CNZDEP
      EXTERNAL FUNGRU,MATGRU
C
C
      DLL_EXPORT GRDCALC
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
C     KU(J) = 1  WEISSER UNTERGRUND 
C     KU(J) = 2  SCHWARZER UNTERGRUND 
C
C 
C     CQ(0,J)  =  SCHICHTDICKE
C 
C 
C 
C
C 
C
      IER=0
      CALL FEHINI()
C
C
C
      IF(NPQG.NE.NPQ1) THEN
         IF(IFEHL(4041).NE.0) THEN
           GOTO 900
         ENDIF
      ENDIF
      NPP=NOPNPP()

c
      NWE=NWEL
      KM=KML
C
      CALL GRDGRUA(NWEL,KML,NPQG,NSTL,CSTL,GRUND,GRUGRZ,IER)
      IF(IFEHL(IER).NE.0) THEN
       GOTO 900
      ENDIF
C
C
C
C
c      NINI=0
c      NINB=0

      MAXIT=2*JTM
      MAXFUN=20
      RESSIZ=FTO+1.D-6
      PHHI=0.
      PHIR=0.


C
C     ANFANGSWERTE
C
C
C
C
C
C
C     ANZAHL DER GLEICHZEITIG ZU BERECHNENDEN WELLENLÄNGEN
C     (INNERHALB DIESER "BREITE" IST FÜR NQ>=3 EINE GLÄTTUNG MÖGLICH)
C
C
C
C     Effektive Dicke bestimmen, falls GEWDIK>0.0
C
C
      NQ=1
      IF(IGLAE.GT.0) THEN
         NQ=NWE
      END IF
C
C
C
      NXAL=NQ*IS+IX+NDIK+ICST
      ALLOCATE(X(NXAL),XX(NXAL),X0(NXAL),XS(NXAL,4),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      ALLOCATE(XLB(NXAL),XUB(NXAL),XLG(NXAL),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF

      IF(MOD(NWE,NQ).NE.0) THEN
        IF(IFEHL(4000).NE.0) GOTO 900
      ENDIF
      DO J=1,M
        IF(KU(J).LE.0.OR.KU(J).GT.2) THEN
           IF(IFEHL(4077).NE.0) GOTO 900
        ENDIF
      END DO
C
C

      FVALUE=0.0
      DO KFW=KM,1,-1
C
        I2=0
        DO I=1,4
          FVALUES(I)=1.D60
        END DO
        DO MQ=1,NWE,NQ

           NDA=(KFW-1)*NWE
             KG=0
             DO II=MQ,MQ+NQ-1
              DO IA=1,IS
                KG=KG+1
                X0(KG)=MAX(DR(LD(IA),II+NDA),0.001/DICK)
                XLG(KG)=CL(IA)
                XLB(KG)=CL(IA)+0.01/DICK
                XUB(KG)=CU(IA)
              ENDDO
             ENDDO
             DO  K=1,IX
                KG=KG+1
                X0(KG)=GK(LX(K),KFW)
                XLB(KG)=0.0
                XLG(KG)=0.0
                XUB(KG)=1.0
             ENDDO
C
C            Dicke als Variable einführen (Maximum Likelihood)
C
C
             DO K=1,NDIK
                KG=KG+1
                X0(KG)=DIK(KFW,K)
                XLB(KG)=0.1*DIK(KFW,K)
                XLG(KG)=XLB(KG)
                XUB(KG)=10.0*DIK(KFW,K)
             END DO
C
C
C            Stütz-Konzentrationen bei konzentrationsabhängigen Grunddaten
C
C
             DO K=1,ICST
                J=(LCST(K)-1)/NPQG+1
                L=LCST(K)-(J-1)*NPQG
                KG=KG+1
                X0(KG)=CST(L,J,KFW)
                XLB(KG)=0.0
                XLG(KG)=XLB(KG)
                XUB(KG)=CST(NST(J),J,KFW)
             END DO
             IF(ICST.GT.0) THEN
               DO J=1,MF
                  DO L=1,NST(J)
                   CSTA(L,J)=CST(L,J,KFW)
                  END DO
               END DO
             ENDIF


C
C
           MAL=NQ*M
           MAL=MAL+NQ*NEE
           MAL=MAL+NDIK+ICST
           MGAL=NQ*NEG+NED+NEC
           MEAL=NQ*ME+NED
C
C          erster Versuch mit X-Werten aus vorhergehender Berechnung
C          und korrigieten unteren Grenzen
C
           IF(I2.EQ.1) THEN
             DO K=1,NXAL
               X(K)=XX(K)
             END DO
             CALL NLDFIT(MAL,MGAL,MEAL,NXAL,MAXIT,MAXFUN,RESSIZ,
     &                 XLB,XUB,X,FVALUE,IFAIL,FUNGRU,MATGRU)
             FVALUES(1)=FVALUE
             DO K=1,NXAL
               XS(K,1)=X(K)
             END DO
C
C            zweiter Versuch mit X-Werten aus vorhergehender Berechnung
C            und normalen unteren Grenzen
C
             DO K=1,NXAL
               X(K)=XX(K)
             END DO
             CALL NLDFIT(MAL,MGAL,MEAL,NXAL,MAXIT,MAXFUN,RESSIZ,
     &                 XLG,XUB,X,FVALUE,IFAIL,FUNGRU,MATGRU)
             FVALUES(2)=FVALUE
             DO K=1,NXAL
               XS(K,2)=X(K)
             END DO
           ENDIF
C
C          dritter Versuch mit vorgegebenen X-Werten
C          und korrigieten unteren Grenzen
C

           DO K=1,NXAL
             X(K)=X0(K)
           END DO


           CALL NLDFIT(MAL,MGAL,MEAL,NXAL,MAXIT,MAXFUN,RESSIZ,
     &                 XLB,XUB,X,FVALUE,IFAIL,FUNGRU,MATGRU)
           FVALUES(3)=FVALUE
           DO K=1,NXAL
               XS(K,3)=X(K)
           END DO
C
C            dritter Versuch mit vorgegebenen X-Werten
C            und normalen unteren Grenzen
C

           CALL NLDFIT(MAL,MGAL,MEAL,NXAL,MAXIT,MAXFUN,RESSIZ,
     &                 XLG,XUB,X,FVALUE,IFAIL,FUNGRU,MATGRU)
           FVALUES(4)=FVALUE
             DO K=1,NXAL
               XS(K,4)=X(K)
             END DO
C
C          beste Lösung aussuchen
C
           FVALUE=MIN(FVALUES(1),FVALUES(2),FVALUES(3),FVALUES(4))
           DO I=1,4
             IF (FVALUE.EQ.FVALUES(I)) THEN
                DO J=1,NXAL
                  X(J)=XS(J,I)
                END DO
             END IF
           END DO
C
C
C
C
           PHHI=PHHI+FVALUE**2
           IF(IFEHL(IER).NE.0) THEN
              DEALLOCATE(X,XX,X0,XS,XLB,XUB,XLG,STAT=IER)
              GOTO 900
           ENDIF
           KG=0
           DO II=MQ,MQ+NQ-1
             DO IA=1,IS
                KG=KG+1
                DR(LD(IA),II+NDA)=X(KG)
             ENDDO
           ENDDO
           I2=1
           DO II=1,NXAL
              XX(II)=X(II)
           END DO



           DO  K=1,IX
                KG=KG+1
                GK(LX(K),KFW)=X(KG)
           ENDDO
           DO K=1,NDIK
              KG=KG+1
              DIK(KFW,K)=X(KG)
           END DO
C
C          Stützstellen zurückspüeichern
C
C
           DO K=1,ICST
              J=(LCST(K)-1)/NPQG+1
              L=LCST(K)-(J-1)*NPQG
              KG=KG+1
              CST(L,J,KFW)=X(KG)
           END DO
           
           RMAX=0.
           PHI=0.
           DO  II=MQ,MQ+NQ-1
               DO J=1,M
                 RUU=RWB(II,KFW,KU(J))
                 IRT=IRTU(KU(J))
                 CALL RWLIK(RW,PDUM,MT,ICHF,DIK(KFW,J),CQ(1,J),
     &                      DR(1,II+NDA),KU(J),IRT,RUU,KFW)
                 DIF=RR(II,KFW,J)-RW
                 PHIR=PHIR+DIF**2
                 PHI=PHI+DIF**2
                 IF(ABS(DIF).GT.ABS(RMAX)) THEN
                    RMAX=DIF
                 ENDIF
               ENDDO
           ENDDO
C
C
C
C
         ENDDO
      ENDDO
      PHHI=SQRT(PHHI)
C
C
C
      DEALLOCATE(X,XX,X0,XS,XLB,XUB,XLG,STAT=IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
C
C
C
C
C     GRUNDDATEN UEBERNEHMEN
C
C
  100 DO J=1,MF
         JDR=NPQ1*(J-1)
         NSTL(J)=0
         DO L=1,NPQ1
           DO KW=1,KML
              CSTL(KW,L,J)=0.
           END DO
         END DO
C
C
C
         IF(CNZDEP().GT.0) THEN
           NSTL(J)=NST(J)
           DO L=1,NST(J)
             DO KW=1,KML
              CSTL(KW,L,J)=CST(L,J,KW)
             END DO
           END DO
         ENDIF
C
C

         NPHLF=NPNPS(NPX,NSTL(J))
         DO KW=1,KML
C
C
C
            DO L=1,NPHLF
               DO I=1,NWEL
                  GRUND(I,KW,L,J)=DR(JDR+L,I+(KW-1)*NWE)
                  IF(GRUND(I,KW,L,J).GT.1.0D9) THEN
                    IF(IFEHL(-4176).NE.0) THEN
                    ENDIF
                  ENDIF
               ENDDO
            ENDDO
C
         ENDDO
      ENDDO

      DO I=1,KML
        DO K=1,MX
          DIKNEU(I,K)=DIKALT(K)
        END DO
        DO K=1,M
         DIKNEU(I,LDIK(K))=DIK(I,K)
        END DO
      END DO


C
      IF(ALLOCATED(X)) THEN
          DEALLOCATE(X,XS,XX)
      ENDIF
      IF(ALLOCATED(XLB)) THEN
         DEALLOCATE(XLB,XUB,XLG)
      ENDIF
C 
c
 900  CALL GETFEH(FEHL)
      RETURN
C 
C 
      END
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  GRDREFL  **********************************************************************
C***************************************************************************************************************************
C***************************************  28.01.2005  **********************************************************************
C***************************************************************************************************************************
      SUBROUTINE GRDREFL(NWEL,KML,NPQG,NPXL,NSTL,CSTL,GRUND,
     &                  NAZ,NFGL,KWBRE,
     &                  DIKNEU,RMENG,REDAB,GRNDALL,FEHL)
      USE MOTFEHL
      USE MODGRUN
      USE MODGRME
      USE MODGKWR
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      TYPE(TYFEH) FEHL
C
      REAL(KIND=4),DIMENSION(NWEL,KML,NPQG,*) :: GRUND
      REAL(KIND=4),DIMENSION(NWEL,KML,NPXL,*) :: GRNDALL
      REAL(KIND=4),DIMENSION(KML,NPQG,*)::CSTL
      INTEGER(KIND=4),DIMENSION(*) :: NSTL
      INTEGER(KIND=4) :: NWEL,KML,NPQG,IFEHL,NOPNPP,NOPNPX
      REAL(KIND=4),DIMENSION(NFGL,*):: RMENG
      REAL(KIND=4),DIMENSION(NWEL,KML,*):: REDAB
      REAL(KIND=4),DIMENSION(KML,*) :: DIKNEU
      INTEGER(KIND=4),DIMENSION(*) :: KWBRE
      REAL(KIND=8) :: DIKH
C
C
C
C
C
C
      DLL_EXPORT GRDREFL
C
C
C
C     Reflexionswerte aus Farb-/Bindemittelmengen berechnen
C     (Untergründe müssen vorhanden sein)
C
C
      IER=0
      CALL FEHINI()
      NPP=NOPNPP()
C
C
C
      CALL GRDGRUA(NWEL,KML,NPQG,NSTL,CSTL,GRUND,GRUND,IER)
      IF(IFEHL(IER).NE.0) THEN
       GOTO 900
      ENDIF
C
      DO J=1,NAZ
        KU(J)=1
        IF(ABS(KWBRE(J)).EQ.KUNT(1)) THEN
           KU(J)=1
        ELSEIF(ABS(KWBRE(J)).EQ.KUNT(2)) THEN
           KU(J)=2
        ENDIF
        KUJ=KU(J)
        IF(KUJ.LE.0.OR.KUJ.GT.2) THEN
          IF(IFEHL(4077).NE.0) THEN
            GOTO 900
          ENDIF
        ENDIF
        IRT=IRTU(KU(J))
        CGES=0.
        DO I=1,NFGL
           CQ(I,J)=RMENG(I,J)
           CGES=CGES+CQ(I,J)
        END DO
c
C
C
        DO  KFW=KML,1,-1
          DIKH=DIKNEU(KFW,J)

          NDA=(KFW-1)*NWEL
          DO  I=1,NWEL
            RUU=RWB(I,KFW,KUJ)
            CALL RWLIK(RW,PDUM,NFGL,ICHF,DIKH,CQ(1,J),
     &                 DR(1,I+NDA),KUJ,IRT,RUU,KFW)
C
            REDAB(I,KFW,J)=RW
C
C
C           Gesamtabsorption bzw. Streuung
C
C
            DO L=1,NPXL
              GRNDALL(I,KFW,L,J)=PDUM(L)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C
C
 900  CALL GETFEH(FEHL)
C
C
      RETURN
      END


C
C
C     GRUNDATEN UMSPEICHERN
C
C
C
      SUBROUTINE GRDGRUA(NWEL,KML,
     &                   NPQG,NSTL,CSTL,GRUND,GRUGRZ,IER)
      USE MOTFEHL
      USE MODGRUN
      USE MODGRME
C      USE MODGKWR,ONLY:CDE

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

C
C
C
C
C     Alte Grunddaten für die Farb-/Bindemittel von KFU bis KFO werden übernommen
C
C
C
C
C
C
C
C 
      REAL(KIND=4),DIMENSION(NWEL,KML,NPQG,*) :: GRUND,GRUGRZ
      REAL(KIND=4),DIMENSION(KML,NPQG,*)::CSTL
      INTEGER(KIND=4),DIMENSION(*) :: NSTL
C
C
      INTEGER(KIND=4) :: CNZDEP
C
C
C
C
C
C     Alte Grunddaten übernehmen
C
      IER=0
      KM=KML
      NWE=NWEL

      IF(NFG.GT.UBOUND(ICHF,1)) THEN
         IER=4039
         GOTO 900
      ENDIF
C
C
C
C     OPTISCHE KONSTANTEN FÜR konstante Grunddaten CCQ="=" setzen
C
      DO J=1,NFG
         JDR=NPQG*(J-1)
C
         NPHLF=NPNPS(NPX,NSTL(J))

C
         IF(CNZDEP().GT.0) THEN
C
C
C           KONZENTRATIONSABHÄNGIG
C
C
          IF(ICHF(J).EQ.8) THEN
C
C             Zusatzstoffe
C
C
            NST(J)= 1
            DO I=1,NST(J)
               DO KW=1,KML
                  CST(I,J,KW)= 1.0
               END DO
            ENDDO
            NPHLF=NPNPS(NPX,NST(J))
            NST1=NPHLF
            DO L=1,NST1
C
C
C
C
C
C
C
C
C              GRUNDDATEN FÜR ZUSATZSTOFFE FESTLEGEN
C
C
               DO KW=1,KM
                 DO I=1,NWE
                    DR(JDR+L,I+(KW-1)*NWE)=0.0
                    DRA(JDR+L,I+(KW-1)*NWE)=0.0
                 ENDDO
               ENDDO
            ENDDO
          ELSE
            NST1=NPHLF
            DO L=1,NST1
C
C
C
C
C
C
C
C
C              GRUNDDATEN FÜR NICHT-ZUSATZSTOFFE ÜBERNEHMEN
C
C
               DO KW=1,KM
                 DO I=1,NWE
                    DR(JDR+L,I+(KW-1)*NWE)=GRUND(I,KW,L,J)
                    DRA(JDR+L,I+(KW-1)*NWE)=GRUGRZ(I,KW,L,J)
                 ENDDO
               ENDDO
            ENDDO
            NST(J)= NSTL(J)
            IF(NST(J).EQ.0) THEN
               IER=4120
               GOTO 900
            ENDIF
            DO I=1,NST(J)
               DO KW=1,KML
                  CST(I,J,KW)= CSTL(KW,I,J)
               END DO
            ENDDO
          ENDIF
         ELSE
C
C
C
C
C
C        NICHT KONZENTRATIONSABHÄNGIG
C
C
C
C
            NST(J)=NSTL(J)
            DO I=1,NST(J)
              DO KW=1,KML
               CST(I,J,KW)= CSTL(KW,I,J)
              END DO
            ENDDO

            DO L=1,NPHLF
               DO KW=1,KM
                 DO I=1,NWE
                   IF(ICHF(J).EQ.8) THEN
C                     ZUSATZSTOFFE
                      DR(JDR+L,I+(KW-1)*NWE)=0.0
                      DRA(JDR+L,I+(KW-1)*NWE)=0.0
                   ELSE
C                     NICHT-ZUSATZSTOFFE
                      DR(JDR+L,I+(KW-1)*NWE)=GRUND(I,KW,L,J)
                      DRA(JDR+L,I+(KW-1)*NWE)=GRUGRZ(I,KW,L,J)
                   ENDIF
                 ENDDO
               ENDDO
            ENDDO
         ENDIF
C
      ENDDO
C
C
  900 RETURN
      END SUBROUTINE
C

C
C
C
c
c
C******************************************************************************
      SUBROUTINE MATGRU(NN,X,XL,XU,MDIM,MAL,MM,F,A,IER)
      USE MODGRUN
      USE MODGRME
      USE MODGKWR,ONLY:GK
C
C     
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
C
C 
      DOUBLE PRECISION  X(*),XL(*),XU(*),A(MDIM,*),F(*),SUMA


C
C
C     MQ STARTWELLENLÄNGE
C     NQ SCHRITTWELLENLÄNGE
C
      IER=0
      NWE=NWS()
      KG=NQ*IS
      DO K=1,IX
         GK(LX(K),KFW)=X(KG+K)
      ENDDO
      KG=NQ*IS+IX
      DO K=1,NDIK
         DIK(KFW,K)=X(KG+K)
      END DO
C
C     Stützstellen zurückspeichern
C
C
      KG=NQ*IS+IX+NDIK
      DO K=1,ICST
        J=(LCST(K)-1)/NPQ1+1
        L=LCST(K)-(J-1)*NPQ1
        CST(L,J,KFW)=X(KG+K)
      END DO



      DO IJ=1,MAL+MM
        DO  K=1,NN
         A(IJ,K)=0.
        ENDDO
      ENDDO

      DO IJ=1,MAL
        NDA=(KFW-1)*NWE
C
C
C
        IF(IJ.LE.NQ*M.AND.IJ.GT.0) THEN
          I=(IJ-1)/M+1
          II=I+MQ-1
          J=IJ-M*(I-1)
          KG=(I-1)*IS
C
          DO  IA=1,IS
            DR(LD(IA),II+NDA)=X(KG+IA)
          ENDDO

C 
C
C
          RUU=RWB(II,KFW,KU(J))
          RMU=RUM(II,KFW,KU(J))
C 

          IRT=IRTU(KU(J))
          RMM=RR(II,KFW,J)
          GEW=RGEW(RMM,RMU,GGW*FU(KU(J)),EXPO)
          CALL RWABL(RW,RABL,PDUM,MT,ICHF,DIK(KFW,J),CQ(1,J),
     &         DR(1,II+NDA),KU(J),IRT,RUU,KFW)
C***
C
C
C
C 
          IC=(I-1)*IS
          DO IA=1,IS
            IV=IC+IA
            A(IJ,IV)=GEW*FUND(LD(IA),PDUM,RABL,DIK(KFW,J),CQ(1,J),
     &                  DR(1,II+NDA))
          ENDDO
C 
C 
C         Ableitungen nach der Dicke
C

          IF(NDIK.GT.0) THEN
            IV=NQ*IS+IX+J
            SUMA=0.0
            DO IA=1,NPP
               SUMA=SUMA+RABL(IA)*PDUM(IA)
            END DO
            A(IJ,IV)=GEW*SUMA/DIK(KFW,J)
          ENDIF
C
C
C
C         Ableitungen nach Stützstellen (CST) bei konzentrationsabhängigen Grunddaten
C
C
C
          DO IA=1,ICST
               IV=NQ*IS+IX+NDIK+IA
               A(IJ,IV)=GEW*FUNCST(LCST(IA),PDUM,RABL,DIK(KFW,J),
     &                             CQ(1,J),DR(1,II+NDA))
          END DO
C
C
        ELSE IF(IJ.GT.NQ*M.AND.IJ.LE.NQ*(M+NEE)) THEN

C
          I=IJ-NQ*M
          IL=(I-1)/NEE+1
          IK=I-NEE*(IL-1)
          II=IL+MQ-1
          KG=(IL-1)*IS

          IF(IK.LE.NEE-NEV) THEN
              DO J=1,IS
                A(IJ,KG+J)= AGGR(NEG+IK,J)
              ENDDO
          ELSE
            IF(IGLAE.EQ.0.OR.NQ.LE.3) THEN
C
C             LINEARE NEBENBEDINGUNGEN; ANPASSUNG AN ALTE GRUNDDATEN
C
C
              DO IA=1,IS
                A(IJ,KG+IA)= AGGR(NEG+IK,IA)
              ENDDO
            ELSE
C
C             IGLAE>0
C             Benachbarte Wellenlängen werden mit ADAE gekoppelt
C
C
              IV=IK-NEE+NEV
                IF(IL.EQ.1) THEN
                  IF(II.EQ.1) THEN
                    A(IJ,KG+LC(IV))=- AGGR(NEG+IK,LC(IV))
                    A(IJ,KG+LC(IV)+IS)=AGGR(NEG+IK,LC(IV))
                  ELSE
                    A(IJ,KG+LC(IV))=-2.*AGGR(NEG+IK,LC(IV))
                    A(IJ,KG+LC(IV)+IS)=AGGR(NEG+IK,LC(IV))

                  ENDIF
                ELSE
                  IF((KG+LC(IV)+IS).GT.NN) THEN
                    A(IJ,KG+LC(IV)-IS)=AGGR(NEG+IK,LC(IV))
                    A(IJ,KG+LC(IV))=-1.*AGGR(NEG+IK,LC(IV))
                  ELSE
                    A(IJ,KG+LC(IV)-IS)=AGGR(NEG+IK,LC(IV))
                    A(IJ,KG+LC(IV))=-2.*AGGR(NEG+IK,LC(IV))
                    A(IJ,KG+LC(IV)+IS)=AGGR(NEG+IK,LC(IV))
                  ENDIF
                ENDIF
c              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C
C
C
C
C     Dickenoptimierung (Maximum Likelihood) Nur wenn NQ=NWE
C
C
C
      I=NQ*IS+IX
      IJ=NQ*(M+NEE)
      DO J=1,NDIK
        A(IJ+J,I+J)=GEWDIK
      END DO
C
C
C
C
C     Stützstellenoptimierung (max. Likelihood) bei konzentrationsabhängrgrn Grunddaten
C
      IJ=NQ*(M+NEE)+NDIK
      KG=NQ*IS+IX+NDIK
      DO K=1,ICST
        J=(LCST(K)-1)/NPQ1+1
        L=LCST(K)-(J-1)*NPQ1
        A(IJ+K,KG+K)=GEWDIK
      END DO

C
C
C     Lineare Nebenbedingungen (Gleichungen) für Dicken
C
C
      DO IK=1,NED
         DO J=1,NDIK
           A(MAL+IK,I+J)=ADIK(IK,J)
         END DO
      END DO
C

C
C
      DO IK=1,MM-NED-NEC
C
C     LINEARE NEBENBEDINGUNGEN (GRENZEN)
C
C
         IJ=-IK
         I=(-IJ-1)/NEG+1
         II=-IJ-NEG*(I-1)
         KG=(I-1)*IS
         DO J=1,IS
            A(IK+MAL+NED,KG+J)=AGGR(II,J)
         ENDDO
C

C 
      ENDDO
C
C
C     Lineare Nebenbedingungen Stützstellen (bei Konzentrationsabhängigkeit)
C
      KG=NQ*IS+IX+NDIK
      DO IK=1,NEC
         DO J=1,ICST
           A(MAL+MM-NEC+IK,KG+J)=ACST(IK,J)
         END DO
      END DO

      RETURN
      END SUBROUTINE
C
C
C
C
C
C
      SUBROUTINE FUNGRU(NN,X,XL,XU,MAL,MM,FF,IER)
      USE MODGRUN
      USE MODGRME
      USE MODGKWR,ONLY:GK
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
C 
C 
C
      DIMENSION X(*),XL(*),XU(*),FF(*)
C 
      IER=0
      NWE=NWS()

      NDA=(KFW-1)*NWE
C 
C 
      KG=NQ*IS
      DO K=1,IX
         GK(LX(K),KFW)=X(KG+K)
      ENDDO
      KG=NQ*IS+IX
      DO K=1,NDIK
         DIK(KFW,K)=X(KG+K)
      END DO
C
C     Stützstellen zurückspüeichern
C
C
      KG=NQ*IS+IX+NDIK
      DO K=1,ICST
        J=(LCST(K)-1)/NPQ1+1
        L=LCST(K)-(J-1)*NPQ1
        CST(L,J,KFW)=X(KG+K)
      END DO
      DO IJ=1,MAL
        IF(IJ.LE.NQ*M.AND.IJ.GT.0) THEN
          I=(IJ-1)/M+1
          II=I+MQ-1
          J=IJ-M*(I-1)
          KG=(I-1)*IS
C
          DO  IA=1,IS
            DR(LD(IA),II+NDA)=X(KG+IA)
          ENDDO
C
C
C
C
C 
C 
C
          RUU=RWB(II,KFW,KU(J))
          RMU=RUM(II,KFW,KU(J))
          IRT=IRTU(KU(J))
          CALL RWLIK(RW,PDUM,MT,ICHF,DIK(KFW,J),CQ(1,J),
     &               DR(1,II+NDA),KU(J),IRT,RUU,KFW)
          RMM=RR(II,KFW,J)
          GEW=RGEW(RMM,RMU,GGW*FU(KU(J)),EXPO)
          FF(IJ) =GEW*(RW-RMM)
C
C
        ELSE IF(IJ.GT.NQ*M.AND.IJ.LE.NQ*(M+NEE)) THEN
C
C       LINEARE NEBENBEDINGUNGEN (MINIMIERUNG)
C
C
          I=IJ-NQ*M
          IL=(I-1)/NEE+1
          IK=I-NEE*(IL-1)
          II=IL+MQ-1
          KG=(IL-1)*IS
          IF(IK.LE.NEE-NEV) THEN
              SUU=0.
              DO IA=1,IS
                SUU=SUU+AGGR(NEG+IK,IA)*X(KG+IA)
              ENDDO
              FF(IJ) =SUU-AGGR(NEG+IK,IS+1)
          ELSE
            IV=IK-NEE+NEV
            IF(IGLAE.EQ.0.OR.NQ.LE.3) THEN
C
C             LINEARE NEBENBEDINGUNGEN; ANPASSUNG AN ALTE GRUNDDATEN
C
C
              SUU=0.
              DO IA=1,IS
                SUU=SUU+AGGR(NEG+IK,IA)*X(KG+IA)
              ENDDO
              FF(IJ) =SUU-AGGR(NEG+IK,IS+1)*DRA(LD(LC(IV)),II+NDA)
            ELSE
              IF(IL.EQ.1) THEN
              IF(II.EQ.1) THEN
               FF(IJ)=AGGR(NEG+IK,LC(IV))*
     &          (-1.*X(KG+LC(IV))+X(KG+LC(IV)+IS))
              ELSE
               FF(IJ)=AGGR(NEG+IK,LC(IV))*
     &         (DR(LD(LC(IV)),II-1+NDA)-2.*X(KG+LC(IV))+X(KG+LC(IV)+IS))
              ENDIF
              ELSE
              IF((KG+LC(IV)+IS).GT.NN) THEN
               FF(IJ)=AGGR(NEG+IK,LC(IV))*
     &          (X(KG+LC(IV)-IS)-1.*X(KG+LC(IV)))
              ELSE
               FF(IJ)=AGGR(NEG+IK,LC(IV))*
     &          (X(KG+LC(IV)-IS)-2.*X(KG+LC(IV))+X(KG+LC(IV)+IS))
              ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C
C     Dickenoptimierung (Maximum Likelihood) Nur wenn NQ=NWE und IGLAE>1
C
C
C
      IJ=NQ*(M+NEE)
      DO J=1,NDIK
        FF(IJ+J)=GEWDIK*(DIK(KFW,J)-CQ(0,J))
      END DO
C
C
C     Stützstellenoptimierung (max. Likelihood) bei konzentrationsabhängrgrn Grunddaten
C
      IJ=NQ*(M+NEE)+NDIK
      KG=NQ*IS+IX+NDIK
      DO K=1,ICST
        J=(LCST(K)-1)/NPQ1+1
        L=LCST(K)-(J-1)*NPQ1
        FF(IJ+K)=GEWDIK*(X(KG+K)-CSTA(L,J))
      END DO

C
C
C     Lineare Nebenbedingungen (Gleichungen) für Dicken
C
C
      DO IK=1,NED
         SUU=0.0
         DO J=1,NDIK
           SUU=SUU+DIK(KFW,J)*ADIK(IK,J)
         END DO
         FF(MAL+IK)=SUU
      END DO
C


      DO IK=1,MM-NED-NEC
C
C     LINEARE NEBENBEDINGUNGEN (GRENZEN)
C
C
         IJ=-IK
         I=(-IJ-1)/NEG+1
         II=-IJ-NEG*(I-1)
         KG=(I-1)*IS
         SUU=0.0
         SUL=0.0
         DO J=1,IS
            SUU=SUU+AGGR(II,J)*X(KG+J)
            SUL=SUL+AGGR(II,J)*XL(KG+J)
         ENDDO
         FF(MAL+NED+IK)=SUU-MAX(AGGR(II,IS+1),SUL)
C

C 
      ENDDO
C
C
C     Lineare Nebenbedingungen Stützstellen (bei Konzentrationsabhängigkeit)
C
      KG=NQ*IS+IX+NDIK
      DO IK=1,NEC
         SUU=0.0
         DO J=1,ICST
           SUU=SUU+X(KG+J)*ACST(IK,J)
         END DO
         SUU=SUU-ACST(IK,ICST+1)
         FF(MAL+MM-NEC+IK)=SUU
      END DO
C
C
      RETURN
      END SUBROUTINE
c
c
c
c
c

C******************************************************
C******************************************************
C
C
      DOUBLE PRECISION FUNCTION RGEW(RG,RU,GGW,EXPO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      RGEW=GGW*(ABS(RG)+0.001)**EXPO
      RETURN
      END
c
C 
C
      DOUBLE PRECISION FUNCTION GAM(CM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA DEGAM/1.D8/
      GAM=DEGAM
      IF(CM.NE.0.) GAM=1./CM
      RETURN
      END 
C
C

C
C     RWLIK
C
C
C
      SUBROUTINE RWLIK(RW,P,MT,ICHF,DIK,CQ,DR,
     &                 KU,IRT,RU,KW)
      USE MODGRME
      USE MODGKWR,ONLY:CDE,GK
      USE MODGRUN,ONLY:KFW
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
c
      INTERFACE
      REAL(KIND=8) FUNCTION WINDSCH(A,S,STUE,AM,SM,YDX,YDSTUE)
      REAL(KIND=8) :: A,S,AM,SM
      REAL(KIND=8) :: STUE(*)
      REAL(KIND=8),OPTIONAL,DIMENSION(*) ::YDX,YDSTUE
      END FUNCTION
      REAL(KIND=8) FUNCTION WINDQU(A,S,STUE,AM,SM,YDX,YDSTUE)
      REAL(KIND=8) :: A,S,AM,SM
      REAL(KIND=8) :: STUE(*)
      REAL(KIND=8),OPTIONAL,DIMENSION(*) ::YDX,YDSTUE
      END FUNCTION
c
c
c
      REAL(KIND=8) FUNCTION TRRWR(PAS,RU,KW,RT,KU,ABLEI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8),DIMENSION(*) :: PAS
      INTEGER(KIND=4) :: RT,KU,KW
      REAL(KIND=8) ::RU,DIK
      REAL(KIND=8),OPTIONAL :: ABLEI(*)
      END FUNCTION
      END INTERFACE


C 
C
      
      DIMENSION CQ(1:*),DR(*),P(-2:*),RABL(*)
      DIMENSION ICHF(*)
      INTEGER(KIND=4) :: CNZDEP
C 
C
C 
C 
C     CQ(0)  =  SCHICHTDICKE
C 
C 
      JSTEU=0
      GOTO 5


      ENTRY RWABL(RW,RABL,P,MT,ICHF,DIK,CQ,DR,
     *                          KU,IRT,RU,KW)
C 
      JSTEU=1
C 
C 
C 
  5   CGES=1.D-100
      CFARB=1.D-100
      CWEISS=1.D-100
      DO  IJ=1,MT
          CGES=CGES+CQ(IJ)
C
C         ohne Bindemittel und Zusatzstoffe
C
C
          IF(ICHF(IJ).NE.1.AND.ICHF(IJ).NE.8) THEN
            CFARB=CFARB+CQ(IJ)
          ENDIF
          IF(ICHF(IJ).EQ.2) THEN
             CWEISS=CWEISS+CQ(IJ)
          ENDIF
      ENDDO  
      P(0)=CGES
      P(-1)=CFARB
      P(-2)=CWEISS
C*************
C*************
C*************
      DO KK=1,NPP
         P(KK)=0.D0
      ENDDO
      IF(CNZDEP().GT.0) THEN
C
        DO  IJ=1,MT
          ISA=NPQ1*(IJ-1)+1
          CC=CQ(IJ)*DIK/CGES
          CF=CFARB*DIK/CGES
          IF(ICHF(IJ).NE.1.AND.ICHF(IJ).NE.8) THEN
            CH=CQ(IJ)/(CQ(IJ)+CGES-CFARB+DELM)
          ELSE
            CH=1.0
          ENDIF
          CH=CH*DIK+DELM
          CW=DIK*(CQ(IJ)-CWEISS)/(CGES-CWEISS+DELM)
          IF(ICHF(IJ).EQ.2) THEN
             CW=CC
          ENDIF
          IF(CNZDEP().EQ.1) THEN
C
C            'X' oder 'H' mit CC
C
             TAU=FST(IJ)*STUINT(CC,NST(IJ),CST(1,IJ,KFW),DR(ISA))
             P(1)=P(1)+DR(ISA+NST(IJ))*TAU
             P(2)=P(2)+(1.-DR(ISA+NST(IJ)))*TAU
          ELSEIF(CNZDEP().EQ.2) THEN


C
C            'Y' oder 'I' mit CC
C
             A=FST(IJ)*STUINT(CC,NST(IJ),CST(1,IJ,KFW),DR(ISA))
             S=FST(IJ)*STUINT(CC,NST(IJ),CST(1,IJ,KFW),DR(ISA+NST(IJ)))
             P(1)=P(1)+A
             P(2)=P(2)+S
C
C
C            CNZDEP > 2 OBSOLET
C
C

          ELSEIF(CNZDEP().EQ.3) THEN

C
C            'X' oder 'H' mit CF
C
             TAU=FST(IJ)*STUINT(CF,NST(IJ),CST(1,IJ,KFW),DR(ISA))
             P(1)=P(1)+DR(ISA+NST(IJ))*TAU*CC/(CF+DELM)
             P(2)=P(2)+(1.0-DR(ISA+NST(IJ)))*TAU*CC/(CF+DELM)
          ELSEIF(CNZDEP().EQ.4) THEN


C
C            'Y' oder 'I' mit CF
C
             A=FST(IJ)*STUINT(CF,NST(IJ),CST(1,IJ,KFW),DR(ISA))
             S=FST(IJ)*STUINT(CF,NST(IJ),CST(1,IJ,KFW),DR(ISA+NST(IJ)))
             P(1)=P(1)+A*CC/(CF+DELM)
             P(2)=P(2)+S*CC/(CF+DELM)
          ELSEIF(CNZDEP().EQ.5) THEN

C
C            'X' oder 'H' mit CH
C
             TAU=FST(IJ)*STUINT(CH,NST(IJ),CST(1,IJ,KFW),DR(ISA))
             P(1)=P(1)+DR(ISA+NST(IJ))*TAU*CC/(CH+DELM)
             P(2)=P(2)+(1.-DR(ISA+NST(IJ)))*TAU*CC/(CH+DELM)
          ELSEIF(CNZDEP().EQ.6) THEN


C
C            'Y' oder 'I' mit CH
C
             A=FST(IJ)*STUINT(CH,NST(IJ),CST(1,IJ,KFW),DR(ISA))
             S=FST(IJ)*STUINT(CH,NST(IJ),CST(1,IJ,KFW),DR(ISA+NST(IJ)))
             P(1)=P(1)+A*CC/(CH+DELM)
             P(2)=P(2)+S*CC/(CH+DELM)
          ELSEIF(CNZDEP().EQ.7) THEN

C
C            'X' oder 'H' mit CW
C
             TAU=FST(IJ)*STUINT(CW,NST(IJ),CST(1,IJ,KFW),DR(ISA))
             P(1)=P(1)+DR(ISA+NST(IJ))*TAU*CC/(CW+DELM)
             P(2)=P(2)+(1.-DR(ISA+NST(IJ)))*TAU*CC/(CW+DELM)
          ELSEIF(CNZDEP().EQ.8) THEN


C
C            'Y' oder 'I' mit CW
C
             A=FST(IJ)*STUINT(CW,NST(IJ),CST(1,IJ,KFW),DR(ISA))
             S=FST(IJ)*STUINT(CW,NST(IJ),CST(1,IJ,KFW),DR(ISA+NST(IJ)))
             P(1)=P(1)+A*CC/(CW+DELM)
             P(2)=P(2)+S*CC/(CW+DELM)

          ENDIF

        ENDDO
      ELSE
C
C       nicht Konzentrationabhängig
C

        DO  IJ=1,MT
          ISA=NPQ1*(IJ-1)+1
          DO KK=1,NPP
               P(KK)=P(KK)+FST(IJ)*DR(ISA+KK-1)*CQ(IJ)
          ENDDO
        ENDDO  
C 
        DO KK=1,NPP
           P(KK)=DIK*P(KK)/CGES
           IF(P(KK).LT.0.) P(KK)=0.
        ENDDO
      ENDIF
C 
      DO KK=0,NPP
         PH(KK)=P(KK)
         IF(PH(KK).LE.10.*TINY(1.D0)) PH(KK)=10.*TINY(1.D0)
      ENDDO

C

      IF(JSTEU.EQ.0) THEN
         RW=TRRWR(PH(1),RU,KW,IRT,KU)
      ELSE
         RW=TRRWR(PH(1),RU,KW,IRT,KU,RABL)
      ENDIF

C
C
C
      RETURN
      END 


C
C
C
C

      DOUBLE PRECISION FUNCTION FUND(LDI,
     &       P,RABL,DIK,CQ,DR)
      USE MODGRME
      USE MODGKWR,ONLY:CDE
      USE MODGRUN,ONLY:KFW,ICHF
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C

      DIMENSION CQ(1:*),DR(*),P(-2:*) 
      DIMENSION RABL(*)
      INTEGER(KIND=4) :: CNZDEP
      FUNN=0.0
C
C
C     BERECHNUNG VON d(R)/d(ABSORPTION(LDI) od. STREUUNG(LDI))
C
C
C     Farbmittelnummer
C

C
C     Startposion in DR für Farbmittel IGD
C
      IGD=(LDI-1)/NPQ1+1

C
C
      IDR=(IGD-1)*NPQ1+1
C
C
C

C
C     Nummer der optischen Konstanten (z.B. 1=Absorption,2=Streuung) für Farbmittel IGD
C
      ISCH=MOD(LDI-1,NPQ1)+1
C
      CGES=P(0)
      CFARB=P(-1)
      CWEISS=P(-2)

C 
C
      IF(CNZDEP().GT.0) THEN
C
C        Konzentrationsabhängig 'X' und 'H'
C
C
         CC=CQ(IGD)*DIK/CGES
         CF=CFARB*DIK/CGES
         IF(ICHF(IGD).NE.1.AND.ICHF(IGD).NE.8) THEN
           CH=CQ(IGD)/(CQ(IGD)+CGES-CFARB+DELM)
         ELSE
           CH=1.0
         ENDIF
         CW=(CQ(IGD)-CWEISS)*DIK/(CGES-CWEISS+DELM)
         IF(ICHF(IGD).EQ.2) THEN
             CW=CC
         ENDIF
         CH=CH*DIK+DELM

C
C
C
         IF(CNZDEP().EQ.1) THEN
C
C            'X' oder 'H' mit CC
C

           IF(ISCH.EQ.NST(IGD)+1) THEN
C
C            ABLEITUNG OMEGA0
C
             TAU=FST(IGD)*STUINT(CC,NST(IGD),CST(1,IGD,KFW),DR(IDR))
             FUNN=(RABL(1)-RABL(2))*TAU

           ELSE

C
C            ABLEITUNG nach TAU=DR(IDR)
C
C
             DAU=FST(IGD)*STUTST(CC,NST(IGD),CST(1,IGD,KFW),
     &           DR(IDR),LDI-IDR+1)
             DDD=DR(IDR+NST(IGD))
             FUNN=((1.0-DDD)*RABL(2)+DDD*RABL(1))*DAU

           ENDIF
         ELSEIF(CNZDEP().EQ.2) THEN
C
C          ABLEITUNG A,S=DR(IDR)
C
C
C            'Y' oder 'I' mit CC
C
             IF(ISCH.LE.NST(IGD)) THEN
C
C              Ableitung nach A
C
               DAA=FST(IGD)*STUTST(CC,NST(IGD),CST(1,IGD,KFW),
     &                 DR(IDR),LDI-IDR+1)
               FUNN=RABL(1)*DAA
             ELSE
C
C              Ableitung nach S
C
               IDS=IDR+NST(IGD)
               DSS=FST(IGD)*STUTST(CC,NST(IGD),CST(1,IGD,KFW),DR(IDS),
     &                    LDI-IDS+1)
               FUNN=RABL(2)*DSS 


             ENDIF
C
C
C            CNZDEP > 2 OBSOLET
C
C
         ELSEIF(CNZDEP().EQ.3) THEN
C
C            'X' oder 'H' (mit CF)
C

           IF(ISCH.EQ.NST(IGD)+1) THEN
C
C            ABLEITUNG OMEGA0
C
             TAU=FST(IGD)*STUINT(CF,NST(IGD),CST(1,IGD,KFW),DR(IDR))
             FUNN=(RABL(1)-RABL(2))*TAU*CC/(CF+DELM)

           ELSE

C
C            ABLEITUNG nach TAU=DR(IDR)
C
C
             DAU=FST(IGD)*STUTST(CF,NST(IGD),CST(1,IGD,KFW),
     &               DR(IDR),LDI-IDR+1)
             DDD=DR(IDR+NST(IGD))
             FUNN=((1.-DDD)*RABL(2)+DDD*RABL(1))*DAU*CC/(CF+DELM)

           ENDIF
         ELSEIF(CNZDEP().EQ.4) THEN
C
C          ABLEITUNG A,S=DR(IDR)
C
C
C            'Y' oder 'I' (mit CF)
C
             IF(ISCH.LE.NST(IGD)) THEN
C
C              Ableitung nach A
C
               DAA=FST(IGD)*STUTST(CF,NST(IGD),CST(1,IGD,KFW),
     &             DR(IDR),LDI-IDR+1)
               FUNN=RABL(1)*DAA*CC/(CF+DELM)
             ELSE
C
C              Ableitung nach S
C
               IDS=IDR+NST(IGD)
               DSS=FST(IGD)*STUTST(CF,NST(IGD),CST(1,IGD,KFW),DR(IDS),
     &                    LDI-IDS+1)
               FUNN=RABL(2)*DSS*CC/(CF+DELM)
             ENDIF

         ELSEIF(CNZDEP().EQ.5) THEN
C
C            'X' oder 'H' (mit CH)
C

           IF(ISCH.EQ.NST(IGD)+1) THEN
C
C            ABLEITUNG OMEGA0
C
             TAU=FST(IGD)*STUINT(CH,NST(IGD),CST(1,IGD,KFW),DR(IDR))
             FUNN=(RABL(1)-RABL(2))*TAU*CC/(CH+DELM)

           ELSE

C
C            ABLEITUNG nach TAU=DR(IDR)
C
C
             DAU=FST(IGD)*STUTST(CH,NST(IGD),CST(1,IGD,KFW),
     &           DR(IDR),LDI-IDR+1)
             DDD=DR(IDR+NST(IGD))
             FUNN=((1.-DDD)*RABL(2)+DDD*RABL(1))*DAU*CC/(CH+DELM)

           ENDIF
         ELSEIF(CNZDEP().EQ.6) THEN
C
C          ABLEITUNG A,S=DR(IDR)
C
C
C            'Y' oder 'I' (mit CH)
C
             IF(ISCH.LE.NST(IGD)) THEN
C
C              Ableitung nach A
C
               DAA=FST(IGD)*STUTST(CH,NST(IGD),CST(1,IGD,KFW),
     &             DR(IDR),LDI-IDR+1)
               FUNN=RABL(1)*DAA*CC/(CH+DELM)
             ELSE
C
C              Ableitung nach S
C
               IDS=IDR+NST(IGD)
               DSS=FST(IGD)*STUTST(CH,NST(IGD),CST(1,IGD,KFW),DR(IDS),
     &                    LDI-IDS+1)
               FUNN=RABL(2)*DSS*CC/(CH+DELM)

             ENDIF
         ELSEIF(CNZDEP().EQ.7) THEN
C
C            'X' oder 'H' (mit CW)
C

           IF(ISCH.EQ.NST(IGD)+1) THEN
C
C            ABLEITUNG OMEGA0
C
             TAU=FST(IGD)*STUINT(CW,NST(IGD),CST(1,IGD,KFW),DR(IDR))
             FUNN=(RABL(1)-RABL(2))*TAU*CC/(CW+DELM)

           ELSE

C
C            ABLEITUNG nach TAU=DR(IDR)
C
C
             DAU=FST(IGD)*STUTST(CW,NST(IGD),CST(1,IGD,KFW),
     &           DR(IDR),LDI-IDR+1)
             DDD=DR(IDR+NST(IGD))
             FUNN=((1.-DDD)*RABL(2)+DDD*RABL(1))*DAU*CC/(CW+DELM)

           ENDIF
         ELSEIF(CNZDEP().EQ.8) THEN
C
C          ABLEITUNG A,S=DR(IDR)
C
C
C            'Y' oder 'I' (mit CW)
C
             IF(ISCH.LE.NST(IGD)) THEN
C
C              Ableitung nach A
C
               DAA=FST(IGD)*STUTST(CW,NST(IGD),CST(1,IGD,KFW),
     &             DR(IDR),LDI-IDR+1)
               FUNN=RABL(1)*DAA*CC/(CW+DELM)
             ELSE
C
C              Ableitung nach S
C
               IDS=IDR+NST(IGD)
               DSS=FST(IGD)*STUTST(CW,NST(IGD),CST(1,IGD,KFW),DR(IDS),
     &                    LDI-IDS+1)
               FUNN=RABL(2)*DSS*CC/(CW+DELM)

             ENDIF

         ENDIF
      ELSE
C
C 
C
C        Nicht Konzentrationsabhängig
C
C
C
C
         IF(ISCH.LE.NPP) THEN
            FUNN=FST(IGD)*DIK*CQ(IGD)*RABL(ISCH)/CGES
         ENDIF
      ENDIF
      FUND=FUNN
      RETURN
      END 
  
C
C 
c
c
c
c
      DOUBLE PRECISION FUNCTION FUNCST(LDI,P,RABL,DIK,CQ,DR)
      USE MODGRME
      USE MODGKWR,ONLY:CDE
      USE MODGRUN,ONLY:KFW,ICHF
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION CQ(1:*),DR(*),P(-2:*)
      DIMENSION RABL(*)
      INTEGER(KIND=4) :: CNZDEP
      FUNN=0.0
C
C
C     BERECHNUNG VON d(R)/d(CST)
C
C     Farbmittelnummer
C
      IGD=(LDI-1)/NPQ1+1
C
C     Startposion in DR für Farbmittel IGD
C
      IDR=(IGD-1)*NPQ1+1

C
C     Nummer der optischen Konstanten (z.B. 1=Absorption,2=Streuung) für Farbmittel IGD
C
      ISCH=MOD(LDI-1,NPQ1)+1
C
      CGES=P(0)
      CFARB=P(-1)
      CWEISS=P(-2)
C 
C
      IF(CNZDEP().GT.0) THEN
C
C        Konzentrationsabhängig
C
C
         CC=CQ(IGD)*DIK/CGES
         CF=CFARB*DIK/CGES
         IF(ICHF(IGD).NE.1.AND.ICHF(IGD).NE.8) THEN
           CH=CQ(IGD)/(CQ(IGD)+CGES-CFARB+DELM)
         ELSE
           CH=1.0
         ENDIF
         CW=(CQ(IGD)-CWEISS)*DIK/(CGES-CWEISS+DELM)
         IF(ICHF(IGD).EQ.2) THEN
             CW=CC
         ENDIF
         CH=CH*DIK+DELM
         IF(CNZDEP().EQ.1) THEN
C
C          'X' und 'H'
C
           IF(ISCH.EQ.NST(IGD)+1) THEN
C
C           ABLEITUNG OMEGA0
C
           ELSE
C
C          ABLEITUNG CST 
C

             DAU=FST(IGD)*STUCST(CC,NST(IGD),CST(1,IGD,KFW),
     &           DR(IDR),LDI-IDR+1)
             DDD=FST(IGD)*DR(IDR+NST(IGD))
             FUNN=((1.-DDD)*RABL(2)+DDD*RABL(1))*DAU
           ENDIF
         ELSEIF(CNZDEP().EQ.2) THEN
C
C          ABLEITUNG CST 
C
C
C          'X' und 'H'
C
              DAA=FST(IGD)*STUCST(CC,NST(IGD),CST(1,IGD,KFW),
     &            DR(IDR),LDI-IDR+1)
              LDJ=LDI-IDR+1+NST(IGD)
              DSS=FST(IGD)*STUCST(CC,NST(IGD),CST(1,IGD,KFW),
     &                   DR(IDR+NST(IGD)),LDJ)
              FUNN=DAA*RABL(1)+DSS*RABL(2)
C
C
C            CNZDEP > 2 OBSOLET
C
C

         ELSEIF(CNZDEP().EQ.3) THEN
C
C          'X' und 'H'
C
           IF(ISCH.EQ.NST(IGD)+1) THEN
C
C           ABLEITUNG OMEGA0
C
           ELSE
C
C          ABLEITUNG CST 
C

             DAU=FST(IGD)*STUCST(CF,NST(IGD),CST(1,IGD,KFW),
     &           DR(IDR),LDI-IDR+1)
             DDD=FST(IGD)*DR(IDR+NST(IGD))
             FUNN=((1.-DDD)*RABL(2)+DDD*RABL(1))*DAU*CC/(CF+DELM)
           ENDIF
         ELSEIF(CNZDEP().EQ.4) THEN
C
C          ABLEITUNG CST 
C
C
C          'X' und 'H'
C
              DAA=FST(IGD)*STUCST(CF,NST(IGD),CST(1,IGD,KFW),
     &            DR(IDR),LDI-IDR+1)
              LDJ=LDI-IDR+1+NST(IGD)
              DSS=FST(IGD)*STUCST(CF,NST(IGD),CST(1,IGD,KFW),
     &                   DR(IDR+NST(IGD)),LDJ)
              FUNN=(DAA*RABL(1)+DSS*RABL(2))*CC/(CF+DELM)
         ELSEIF(CNZDEP().EQ.5) THEN
C
C          'X' und 'H'
C
           IF(ISCH.EQ.NST(IGD)+1) THEN
C
C           ABLEITUNG OMEGA0
C
           ELSE
C
C          ABLEITUNG CST 
C

             DAU=FST(IGD)*STUCST(CH,NST(IGD),CST(1,IGD,KFW),
     &           DR(IDR),LDI-IDR+1)
             DDD=FST(IGD)*DR(IDR+NST(IGD))
             FUNN=((1.-DDD)*RABL(2)+DDD*RABL(1))*DAU*CC/(CH+DELM)
           ENDIF
         ELSEIF(CNZDEP().EQ.6) THEN
C
C          ABLEITUNG CST 
C
C
C          'X' und 'H'
C
              DAA=FST(IGD)*STUCST(CH,NST(IGD),CST(1,IGD,KFW),
     &            DR(IDR),LDI-IDR+1)
              LDJ=LDI-IDR+1+NST(IGD)
              DSS=FST(IGD)*STUCST(CH,NST(IGD),CST(1,IGD,KFW),
     &                   DR(IDR+NST(IGD)),LDJ)
              FUNN=(DAA*RABL(1)+DSS*RABL(2))*CC/(CH+DELM)

         ELSEIF(CNZDEP().EQ.7) THEN
C
C          'X' und 'H'
C
           IF(ISCH.EQ.NST(IGD)+1) THEN
C
C           ABLEITUNG OMEGA0
C
           ELSE
C
C          ABLEITUNG CST 
C

             DAU=FST(IGD)*STUCST(CW,NST(IGD),CST(1,IGD,KFW),
     &           DR(IDR),LDI-IDR+1)
             DDD=FST(IGD)*DR(IDR+NST(IGD))
             FUNN=((1.-DDD)*RABL(2)+DDD*RABL(1))*DAU*CC/(CW+DELM)
           ENDIF
         ELSEIF(CNZDEP().EQ.8) THEN
C
C          ABLEITUNG CST 
C
C
C          'X' und 'H'
C
              DAA=FST(IGD)*STUCST(CW,NST(IGD),CST(1,IGD,KFW),
     &            DR(IDR),LDI-IDR+1)
              LDJ=LDI-IDR+1+NST(IGD)
              DSS=FST(IGD)*STUCST(CH,NST(IGD),CST(1,IGD,KFW),
     &                   DR(IDR+NST(IGD)),LDJ)
              FUNN=(DAA*RABL(1)+DSS*RABL(2))*CC/(CW+DELM)


         ENDIF

      ELSE
C
C 
C
C        Nicht konzentrationsabhängig
C
C
C
C
      ENDIF
      FUNCST=FUNN
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
C
C

