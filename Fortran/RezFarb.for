C     Last change: KU 11.08.2021 17:47:16

      INCLUDE "GETLWGK.FOR"
      INCLUDE "REZMEGR.FOR"
      INCLUDE "REZANGL.FOR"
      INCLUDE "REZLIMT.FOR"
      INCLUDE "REZMGRF.FOR"
      INCLUDE "REZBEEN.FOR"
C     Entweder (mit KA)
c      INCLUDE "KORRALT.FOR"
C     oder (mit AKA)
      INCLUDE "KORRNEU.FOR"
C      INCLUDE "QLalt.FOR"
C      INCLUDE "MSCHLIB.FOR"
c      INCLUDE "FAKER0.FOR"
c      INCLUDE "FAKER1.FOR"
c      INCLUDE "FAKER2.FOR"
C      INCLUDE "FAKER4.FOR"
c      INCLUDE "FAKER5.FOR"
c      INCLUDE "FRBL0.FOR"
c      INCLUDE "FRBL1.FOR"
C      INCLUDE "NUMLIB.FOR"
C      INCLUDE "FEHLIB.FOR"
C      INCLUDE "MATLIB.FOR"
C
C
c      INCLUDE "REZDRU.FOR"
C
c
C
C
c
C
C
C
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  REZREZ  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
      SUBROUTINE REZREZ(KFIT,MNF,SOMNG,
     &                  KZL,KFM,NFR,LKID,REMNG,RGEW,FEHL)
      USE MOTFEHL
C     USE MODMERZ,ONLY:ANORM
C
C     KFIT=KFIT1+5*KFIT2
C     Damit können z.B. mit KFIT=11 sowohl ein R-Angleich (KFIT2=KFIT/5=2) als auch ein Farbangleich (KFIT1=MOD(KFIT,5)=1)
C     durchgeführt werden
C
C
C     BERECHNUNG VON FARBREZEPTEN FUER DECKENDE UND TRANSPARENTE
C     FARBMITTELSCHICHTEN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4),DIMENSION(*):: NFR
      INTEGER(KIND=4) ::MNF,KFIT
      REAL(KIND=4),DIMENSION(KFM,*)::REMNG
      REAL(KIND=4),DIMENSION(*)::SOMNG
      REAL(KIND=4),DIMENSION(*)::RGEW
      INTEGER(KIND=4),DIMENSION(KFM,*)::LKID
      TYPE(TYFEH) FEHL
C
      DLL_EXPORT REZREZ
C
C
C
C     Farbangleich (KFIT=1)  oder R-Angleich (KFIT=2) beides KFIT=3
C
C
c      OPEN(27, FILE='TESTREZ.TXT')
C
C
c      WRITE(27,*) 'REZREZ Start'
C
C
C 
C 
      IER=0
      CALL FEHINI()
C
C
      NWE=NWS()
      CALL SETGEW(NWE,RGEW)
C
C
C
      CALL REZSTR(IER)
      IF(IFEHL(IER).NE.0) GOTO 900
      KZA=KZL

      CALL REZKOM(KFIT,MNF,SOMNG,
     &            KZA,KFM,NFR,LKID,REMNG,IER)

      IF(IFEHL(IER).NE.0) GOTO 900
      KZL=KZA

C
c
 900  CALL GETFEH(FEHL)

c      close(27)
      RETURN
C 
C
C
      END
C
C
C
C
C     Last change:  UFO  18 Oct 104   12:48 pm
c
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  BASREZ  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
c
c
      SUBROUTINE BASREZ(KFIT,MNF,BAMNG,NWEL,KML,RCAL,SOKRIT,RGEW,FEHL)
      USE MOTFEHL
      USE MODFARB
      USE MODRWRZ
      USE MODMERZ

      USE MODGREN

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=4),DIMENSION(NWEL,KML,2) :: RCAL
      REAL(KIND=4),DIMENSION(*)::RGEW
      REAL(KIND=4),DIMENSION(16,*)::SOKRIT
      INTEGER*4 KFIT,NWEL,KML,MNF
C
      REAL(KIND=4),DIMENSION(MNF) :: BAMNG

      TYPE(TYFEH) FEHL
C
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
C 
      DLL_EXPORT BASREZ
C
C
C 
c      OPEN(27, FILE='TESTBAS.TXT')

C
C
C 
      IER=0
      CALL FEHINI()
C
      CALL SETGEW(NWEL,RGEW)
C
C
      IF(MNF.EQ.0) THEN
        IF(IFEHL(4014).NE.0) GOTO 900
      ENDIF

C
C
      IF(IFEHL(DIMIER(NWEL,KML=KML,MNF=MNF)).NE.0)  GOTO 900
C
C
C
C
C
C
      CALL REZSTR(IER)
      IF(IFEHL(IER).NE.0) GOTO 900
      CALL REZBAS(KFIT,MNF,BAMNG,NWEL,KML,RCAL,SOKRIT,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
C
C
C
C
c
  900 CALL GETFEH(FEHL)
c      CLOSE(27)
      RETURN
C
C
C 
      END
C
C

C
C
c
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  MSHREZ  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
      SUBROUTINE MSHREZ(IPRN,NWEL,KML,RCAL,
     &           KF,LID,REMNG,DICKE,
     &           NSTN,CSTN,GRUGES,SOKRIT,FEHL)
      USE MOTFEHL
      USE MODFARB
      USE MODRWRZ
      USE MODMERZ
C
C
C     Berechnung von Remissins bzw. Transmissionswerten RCAL aus Farb-/Bindemittelmengen REMNG

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER(KIND=4) :: IPRN
      INTEGER(KIND=4) NWEL,KML,NSTN,KF
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: GRUGES
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RCAL
      INTEGER(KIND=4),DIMENSION(*)::LID
      REAL(KIND=4),DIMENSION(*) ::REMNG,DICKE
      REAL(KIND=4),DIMENSION(16,*):: SOKRIT
      REAL(KIND=4),DIMENSION(KML,*) ::CSTN
      TYPE(TYFEH) FEHL
C
C 
      DLL_EXPORT MSHREZ
C
C 
c      OPEN(27,FILE='TESTOUT.TXT')
     

C
C
C
      IER=0
      CALL FEHINI()
C
C
C     Neue Schichtdicke wird übernommen
C
      DO I=1,UBOUND(D,1)
         D(I)=DICKE(I)
      END DO
c
      CALL CHKDICK(IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
      CALL REZSTR(IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C     Mengen übernehmen
C
C
C
C
C
C
      CALL REZMSH(KF,LID,REMNG,NWEL,KML,RCAL,SOKRIT,IER)
      CALL REZGRND(IPRN,KF,LID,REMNG,NWEL,KML,NSTN,CSTN,GRUGES,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
C
C
C
C
c
 900  CALL GETFEH(FEHL)
c      CLOSE(27)
      RETURN
C 
C
C 
      END
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  REZKOR  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
C





      SUBROUTINE REZKOR(KFIT,MNF,REMNG,DICKE,GGEN,RGEW,FEHL)
      USE MOTFEHL
      USE MODRWRZ
      USE MODMERZ

C
C
C     BERECHNUNG VON FARBREZEPTKORREKTUREN FUER DECKENDE UND TRANSPARENTE
C     FARBMITTELSCHICHTEN
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C

C
C     RA0 = R0   RAUX=RHLF
C
C
      INTEGER(KIND=4) :: KFIT,MNF,IER,IFEHL
      REAL(KIND=4),DIMENSION(*)::REMNG,DICKE
      REAL(KIND=4),DIMENSION(*)::RGEW
      REAL(KIND=4) :: GGEN,GETGGE
      TYPE(TYFEH) FEHL
C
C
C 
      DLL_EXPORT REZKOR
C
C 
C 
C
C
C
c      OPEN(27,FILE='TESTREZ.TXT')
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
      NWEL=NWS()
      CALL SETGEW(NWEL,RGEW)

C
C
C     Neue Schichtdicke wird übernommen
C
      DO I=1,UBOUND(D,1)
         D(I)=DICKE(I)
      END DO
C
      CALL CHKDICK(IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
C
      CALL RKDEST(IER)

C
      IF(IFEHL(IER).NE.0) GOTO 900
C
C

      CALL KORREZ(KFIT,MNF,REMNG,IER)
C
C
      GGEN=GETGGE()
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
c
  900 CALL GETFEH(FEHL)
c      CLOSE(27)
      RETURN
C
C
C
      END
C
C
C     Neues GGE (für IGX=6) übernehmen
C
C
      REAL(KIND=4) FUNCTION GETGGE()
      USE MODMERZ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
C
      GETGGE=GGE
      RETURN
      END
C
C
C
C     Gewichte für R-Wert-Angleich neu setzen
C
C
C
      SUBROUTINE SETGEW (NWEL,GEW)
      USE MODRWRZ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4):: I,NWEL
      REAL(KIND=4),DIMENSION(*)::GEW
C
C
C
C
      DO I=1,NWEL
        RGWW(I)=GEW(I)
      END DO
      RETURN
      END


C******************************************************************************
C******************************************************************************
C******************************************************************************
      SUBROUTINE KORREZ(KFIT,MNF,REMNG,IER)
      USE MODRWRZ
      USE MODXYRZ
      USE MODMERZ
      USE MODGREN
      USE MODFARB

C
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
C
C
C 
C 
       REAL(KIND=4),DIMENSION(*)::REMNG
       INTEGER(KIND=4)::MNF,IER,KFIT
C
C
C 
C 
      IF(MNF.EQ.0) THEN
        IER=4014
        RETURN
      ENDIF

C 
      NF=MNF
      DO I=1,NF
         ACONZ(I)=REMNG(I)/ANORM
         ACZWI(I)=ACONZ(I)
      END DO

C 
C
C 
C     BERECHNUNG VON FARBREZEPTKORREKTURENFUER DECKENDE, TRANSLUZENTE UND
C     TRANSPARENTE FARBMITTELSCHICHTEN
C 
C 
C
C 
C
C          MDIM=UBOUND(AGR,1)
C          CALL MADFEA(NF,ACONZ,MDIM,NEE,AGR,CL,CU,IC,IER)


C             
C 
C 
C 
C 
C 
C
      CALL  MINFA(KFIT,IER,DETR)
      IF(IER.NE.0) GOTO 900
C
C

      DO I=1,NF
         IF(ABS(ACONZ(I)).LT.EPL) THEN 
            ACONZ(I)=0.
         ENDIF
         AMG=ACONZ(I)*ANORM
         IF(AMG.GT.AMGGRZ) THEN
            IER=-4124
            GOTO 900
         ENDIF
         REMNG(I)=AMG
      ENDDO
C
 900  RETURN
      END 

C
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  MSHKOR  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
C
      SUBROUTINE MSHKOR(IPRN,NWEL,KML,RCAL,
     &                  KF,LID,REMNG,DICKE,
     &                  NSTN,CSTN,GRUGES,SOKRIT,FEHL)
      USE MOTFEHL
      USE MODFARB
      USE MODRWRZ
      USE MODMERZ
      USE MODSORT

C
C
C     REFLEXIONSWERTE AUS FARB-/BINDEMITTELMISCHUNGEN
C
CC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C
      INTEGER(KIND=4) :: IPRN
      INTEGER(KIND=4) ::NWEL,KML,NSTN
      INTEGER(KIND=4),DIMENSION(*) ::LID
      REAL(KIND=4),DIMENSION(NWEL,KML,*) ::RCAL
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: GRUGES
      REAL(KIND=4),DIMENSION(16,*)::SOKRIT
      REAL(KIND=4),DIMENSION(*):: CSTN(KML,*)
      REAL(KIND=4),DIMENSION(*):: REMNG,DICKE
      TYPE(TYFEH)FEHL
C 

C
      DLL_EXPORT MSHKOR
C
C 
C 
C
C
C
c      OPEN(27,FILE='TESTMSH.TXT')
C 
C
C
C
C 
C 
      IER=0
      CALL FEHINI()
c
c

C
C     Neue Schichtdicke wird übernommen
C
      DO I=1,UBOUND(D,1)
         D(I)=DICKE(I)
      END DO
c
      CALL CHKDICK(IER)
      IF(IFEHL(IER).NE.0) GOTO 900
      CALL RKDEST(IER)
      IF(IFEHL(IER).NE.0) GOTO 900

C
C

C
C
C
C     R-WERTE FUER NEUE FARB-/BINDINDEMITTELMENGEN
C
C

      CALL REZMSH(KF,LID,REMNG,NWEL,KML,RCAL,SOKRIT,IER)
      CALL REZGRND(IPRN,KF,LID,REMNG,NWEL,KML,NSTN,CSTN,GRUGES,IER)

      IF(IFEHL(IER).NE.0) GOTO 900
C
C
c
c      CLOSE(27)
  900 CALL GETFEH(FEHL)
      RETURN
C 
C
C 
      END
C
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  REFKOR  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c

      SUBROUTINE REFKOR(NWEL,KML,RNAC,FEHL)

C

      USE MOTFEHL
      USE MODFARB
      USE MODRWRZ
      USE MODHILF
      USE MODXYRZ
      USE MODMERZ
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) ::RNAC

C
C
C
C     BERECHNUNG VON KORRIGIERTEN R-WERTEN UND U.U. DIFFERENTIALKOEFF.
C     RA0 UND RAUX AUS DEN REZEPTEN UND REFLEXIONSWERTEN VON NACHSTELLUNGEN
C
C
C
C
C
C

C
      INTEGER(KIND=4) :: IER
      TYPE(TYFEH) FEHL
C
C 
C 
      DLL_EXPORT REFKOR
C
C 
C 
c      OPEN(27,FILE='TESTHILF.TXT')
C 
C 
C 
C 
      IER=0
      CALL FEHINI()
C
C
C
C
C
C
C
C     X,Y,Z FUER NACHSTELLUNG(N) UND BERECHNETE NACHSTELLUNG(B) BERECHNEN
C 
      CALL RKSTR(IER)
C
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
C
C
C

C 
C 
C 
C     PRUEFEN, OB ABWEICHUNG MESSUNG-RECHNUNG KLEINER VORGEGEBENEM WERT 
C 
C 
C 
      CALL DELABAL(JABST,XYZN(1,1,1,1),XYZB(1,1,1,1),
     &             DE,DL,DC,DH,DA,DB,1)
      IF(DE.GT.DEOK) THEN
         IER=-4099
         IF(IFEHL(IER).NE.0) GOTO 900
      ENDIF
C 
C 

      CALL MAHLF(IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
C      
      IF(JUU.EQ.2.AND.JUN.EQ.1) THEN
C         
C        R-WERTE NACHSTELLUNG FUER SCHWARZEN UNTERGRUND BERECHNEN 
C        TYREDAN(2).R(I)-TYREDAN(1).R(I)= TYREDAB(2).R(I)-TYREDAB(1).R(I)
C        ES werden die alten Schichtdicken verwendet
C
C         
         DO KW=1,KML
C           TPS=TOLM-GLANZ(KW)
           CALL MALRW(NF,CONA,RR(1,KW,1),KW,1)
           CALL MAKOR(NF,CONA,RR(1,KW,1),KW,1)
           CALL MALRW(NF,CONA,RR(1,KW,2),KW,2)
           CALL MAKOR(NF,CONA,RR(1,KW,2),KW,2)
           DO I=1,NWEL
             RNC=RNAC(I,KW,1)
             RNAC(I,KW,2)=FRKORRI(AKK,RR(I,KW,2),RR(I,KW,1),RNC,KW)

           ENDDO
         ENDDO
      ENDIF
c
C
c
  900 CALL GETFEH(FEHL)
c      CLOSE(27)

      RETURN
C 
C
C
      END

c
c
C
c
C
C
C********************************************************
C********************************************************
C********************************************************
C********************************************************
C********************************************************
C********************************************************
C********************************************************
C
C
C
C
 
