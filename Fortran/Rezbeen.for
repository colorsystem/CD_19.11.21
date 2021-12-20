C     Last change: KU 10.04.2021 10:06:45
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  REZBEG  ************************************************************************
C***************************************************************************************************************************
C***************************************  04.11.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************

C
      SUBROUTINE REZBEG(NWEL,KML,NME,NFG,NFV,NPQG,NLQ,NHLF,MEE,MEM,NSPE,
     &                  FEHL)
      USE MODFEHL
      USE MODFARB
      USE MODABST
      USE MODRWRZ
      USE MODXYRZ
      USE MODGREN
      USE MODHILF
      USE MODMERZ,ONLY:KRE,IKOR,JN,JUV,JUN,JUU,FU,DI,DEV
      USE MODSORT
      IMPLICIT NONE
      INTEGER(KIND=4) :: NME,KML,NWEL,NFG,NFV,NPQG,NLQ,NHLF,MEE,MEM,NSPE
      INTEGER(KIND=4) :: IFEHL,IER,IERALC,NFGE,NFGZ,NFG1
      TYPE(TYFEH) :: FEHL
      INTEGER(KIND=4)::I
C
C
      DLL_EXPORT REZBEG
C
      IER=0
      CALL FEHINI()

C
C
      NF=0
      IBA=0
      IBI=0
      NPX=0
      NEQ=0
      NEE=0
      NEM=0
      NF1=0
      IHLF=0
      IKOR=0
      JN=0
      JUV=0
      JUN=0
      JUU=0
      FEHH%IFEH=0
      FEHH%IWARN=0
      NFGE=MEE+MEM
      NFGZ=NFG+MEE
      NFG1=NFG+1
C


C
C     MODMERZ
C
c      WRITE(*,*)'NME,KML',NME,KML
      ALLOCATE(FU(NME),DEV(KML),DI(3,KML),STAT=IER)

      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF

C
C
C     MAXIMALE ANZAHL ZU BERECHNEDER REZEPTE
C
C
      KRE= NSPE+1
C
C
C
C     Prüfen, ob Farb-/Bindemittel vorhanden
C
C

      IF(NFG.EQ.0) THEN
        IER=4043
        IF(IFEHL(IER).NE.0)THEN
           GOTO 900
        ENDIF
      ENDIF
c      OPEN(27,FILE='TESTOUT.TXT')
C
C
C
C     PRÜFEN, ob maximale Anzahl Farb-Bindemittel pro Rezept überschritten
C
C
C
      IF(NFV.GT.NFG) THEN
        IER=4109
        IF(IFEHL(IER).NE.0)THEN
           GOTO 900
        ENDIF
      ENDIF



C
C     MODFARB
C
c      WRITE(27,*) 'NFG',NFG
      ALLOCATE(KTO(NFG),LK(NFG),ICHF(NFG),ACLIM(NFG),ACONZ(NFG+1),
     &         ACONA(NFG),CONA(NFG),ACBEST(NFG),PRE(NFG),
     &         ACZWI(NFG),FASTART(NFG),STAT=IER)

      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF


C
C     MODABST
C
      ALLOCATE(NST(NFG),CST(NPQG,NFG,KML),ABSTA(NWEL,KML,NPQG,NFG),
     &         STAT=IER )
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF

C
C     MODRWRZ
C
c      WRITE(27,*) 'NME,NWEL,KML,NPQG,NFG,NLQ',NME,NWEL,KML,NPQG,NFG,NLQ
      ALLOCATE(D(NME),DALT(NME),R0(NWEL,KML,NME),RV(NWEL,KML,NME),
     &         RD(NWEL),RY(NWEL,KML,NME),RU(NWEL,KML,NME),
     &         ABLEI(NPQG),RN(NWEL,KML,NME),RR(NWEL,KML,NME),
     &         TST(NPQG),AD(3,KML,NME),AQ(3,3,NLQ,KML,NME),DR(NWEL),
     &         ASHIL(MAX(NPQG,6)),ABSSU(NWEL,KML,MAX(NPQG,6)),
     &         RAS(NWEL,KML,NME,NPQG),RSU(NWEL,KML,NME),RGWW(NWEL),
     &         RHLF(NWEL,KML,NFG,NME),IRETR(NME),IKWB(NME),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      DO I=1,NWEL
        RGWW(I)=1.0
      END DO
C
C
C
      DO I=1,NME
         IRETR(I)=0
         IKWB(I)=0
         D(I)=0.
      END DO
C
C     MODXYRZ
C
c      WRITE(27,*) 'NLQ,KML,NME',NLQ,KML,NME
      ALLOCATE(XYZV(3,NLQ,KML,NME),XYZN(3,NLQ,KML,NME),
     &         XYZB(3,NLQ,KML,NME),SPHI(NLQ,KML,NME),CPHI(NLQ,KML,NME),
     &         STAT=IER)

      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
C
C     MODGREN
C
c      WRITE(27,*) 'NFGE,NFG1,NFGZ',NFGE,NFG1,NFGZ
      ALLOCATE(AGR(NFGE,NFG1),IGR(NFGE,NFG1),IC(NFGZ),CL(NFGZ),CU(NFGZ),
     &         OP(NFG),STAT=IER)
      IF(IFEHL(IERALC(IER)).NE.0)THEN
         GOTO 900
      ENDIF


C
C     MODSORT
C
      IF(NSPE.GT.0) THEN
          ALLOCATE(ASOS(8,NSPE),
     &         CS(NFV,NSPE),LKS(NFV,NSPE),KFS(NSPE),KNUS(NSPE),
     &         STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      ENDIF

C
C     MODHILF
C
      IF(NHLF.GT.0) THEN

          ALLOCATE(DIHLF(NME,NHLF),KFH(NHLF),AMHLF(NHLF),ACH(NFV,NHLF),
     &             AMA(NHLF,NFV),CADD(NFV,NHLF),CADI(NFV,NHLF),
     &             PIV(NFV),PIW(NFV),IP(NFV),RZW(MAX(NFV,NHLF),NWEL),
     &             CBH(NFV,NHLF),CBY(NFV,NHLF),RNORM(NWEL),
     &             RH(NWEL,KML,NME,NHLF),RZ(NWEL,KML,NME,NHLF),
     &             STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      ENDIF

C
C
C

 900  CALL GETFEH(FEHL)
c      CLOSE(27)
      RETURN
      END SUBROUTINE
C

C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  REZEND  ************************************************************************
C***************************************************************************************************************************
C***************************************  04.11.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************

C
      SUBROUTINE REZEND(FEHL)
      USE MODFEHL
      USE MODFARB
      USE MODABST
      USE MODRWRZ
      USE MODXYRZ
      USE MODGREN
      USE MODHILF
      USE MODMERZ,ONLY:FU,DI,DEV
      USE MODSORT

      IMPLICIT NONE
      INTEGER(KIND=4) :: IFEHL,IER,IERALC
      TYPE(TYFEH) :: FEHL
C
C
      DLL_EXPORT REZEND
      IER=0
      CALL FEHINI()
C
C     DEALLOCATION
C

C
C     MODMERZ
C
      IF(ALLOCATED(FU)) THEN
        DEALLOCATE(FU,DEV,DI,STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
          GOTO 900
        ENDIF
      ENDIF

C
C     MODFARB
C
      IF(ALLOCATED(KTO)) THEN
        DEALLOCATE(KTO,LK,ICHF,ACLIM,ACONZ,
     &         ACONA,CONA,ACBEST,PRE,
     &         ACZWI,FASTART,STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
         GOTO 900
        ENDIF
      ENDIF
C
C     MODABST
C

      IF(ALLOCATED(NST)) THEN
        DEALLOCATE(NST,CST,ABSTA,
     &         STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
         GOTO 900
        ENDIF
      ENDIF
C
C     MODRWRZ
C
      IF(ALLOCATED(D)) THEN
        DEALLOCATE(D,DALT,R0,RV,RN,RD,DR,
     &         RY,RU,RR,RSU,RGWW,
     &         ASHIL,TST,ABLEI,
     &         AD,AQ,
     &         ABSSU,RAS,RHLF,
     &         IRETR,IKWB,
     &         STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
          GOTO 900
        ENDIF
      ENDIF
C
C     MODXYRZ
C
      IF(ALLOCATED(XYZV)) THEN
        DEALLOCATE(XYZV,XYZN,
     &         XYZB,SPHI,CPHI,
     &         STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
         GOTO 900
        ENDIF
      ENDIF
C
C     MODGREN
C
      IF(ALLOCATED(AGR)) THEN
        DEALLOCATE(AGR,IGR,IC,CL,CU,OP,
     &         STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
         GOTO 900
        ENDIF
      ENDIF

C
C     MODSORT
C
      IF(ALLOCATED(KNUS)) THEN
        DEALLOCATE(ASOS,
     &         CS,LKS,KFS,KNUS,
     &         STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
          GOTO 900
        ENDIF
      ENDIF

C
C     MODHILF
C
      IF(ALLOCATED(KFH)) THEN
          DEALLOCATE(DIHLF,KFH,AMHLF,ACH,
     &             AMA,CADD,CADI,
     &             PIV,PIW,IP,RZW,RNORM,
     &             CBH,CBY,
     &             RH,RZ,
     &             STAT=IER)
         IF(IFEHL(IERALC(IER)).NE.0)THEN
            GOTO 900
         ENDIF
      ENDIF

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

