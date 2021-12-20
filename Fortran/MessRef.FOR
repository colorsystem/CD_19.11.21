C     Last change: KU 24.06.2019 12:17:45
C
C
C
      INCLUDE "QRmess.FOR
      INCLUDE "MESSFIT.FOR"
      INCLUDE "XYZLABCH.FOR"
c      INCLUDE "FRBL0.FOR"
C
C
C
C
C
C
C
      SUBROUTINE REFWRT(NWE,AW,AS,AG,MW,MS,MG,WIST,WSOL,MK,RR,IER)
      IMPLICIT REAL(KIND=8)(A-H,O-Z)
      REAL(KIND=8),DIMENSION(NWE) :: AWEI,ASCH,AGRA,MWEI,MSCH,MGRA,
     &                               WSO,WIS,RMESS,RREF
      REAL(KIND=4),DIMENSION(NWE) :: AW,AS,AG,MW,MS,MG,WSOL,WIST,MK,RR

      INTEGER(KIND=4) ::IER
C
C     Mk      Messwert der Probe (bei Istwellenlänge)
C     Mw      Messwert des Weißstandards (bei Istwellenlänge)
C     Ms      Messwert des Schwarzstandards (bei Istwellenlänge)
C     Mg      Messwert des Graustandards (bei Istwellenlänge)
C
C     Aw      Absolutwert des Weißstandards (bei Sollwellenlänge)
C     As      Absolutwert des Schwarzstandards (bei Sollwellenlänge)
C     Ag      Absolutert des Graustandards (bei Sollwellenlänge)
C     WIST    Istwellenlängen
C     Wsol    Sollwellenlängen
C     RR      Reflexions- oder Transmissionswert (berechnet bei Sollwellenlänge)
C
C
      DLL_EXPORT REFWRT
C
C
      IER=0
      NNR=NWE
      DO L=1,NNR
        WIS(L)=WIST(L)
        WSO(L)=WSOL(L)
        AWEI(L)=AW(L)
        ASCH(L)=AS(L)
        AGRA(L)=AG(L)
        MWEI(L)=MW(L)
        MSCH(L)=MS(L)
        MGRA(L)=MG(L)
        RMESS(L)=MK(L)
      ENDDO
C
C
      CALL REFCAL(NWE,AWEI,ASCH,AGRA,MWEI,MSCH,MGRA,WIS,WSO,
     &                      RMESS,RREF,IER)
C
      IF(IER.NE.0) THEN
        GOTO 900
      ENDIF
      DO L=1,NNR
        RR(L)=RREF(L)
      ENDDO
 900  RETURN
      END SUBROUTINE
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
      SUBROUTINE MESSWRT(NWE,AW,AS,AG,MW,MS,MG,WIST,WSOL,RR,MK,IER)
      IMPLICIT REAL(KIND=8)(A-H,O-Z)
      REAL(KIND=8),DIMENSION(NWE) :: AWEI,ASCH,AGRA,MWEI,MSCH,MGRA,
     &                               WSO,WIS,RMESS,RREF
      REAL(KIND=4),DIMENSION(NWE) :: AW,AS,AG,MW,MS,MG,WSOL,WIST,MK,RR

      INTEGER(KIND=4) ::IER
C
C
      DLL_EXPORT MESSWRT
C
C

      IER=0
      NNR=NWE
      DO L=1,NNR
        WIS(L)=WIST(L)
        WSO(L)=WSOL(L)
        AWEI(L)=AW(L)
        ASCH(L)=AS(L)
        AGRA(L)=AG(L)
        MWEI(L)=MW(L)
        MSCH(L)=MS(L)
        MGRA(L)=MG(L)
        RREF(L)=RR(L)
      ENDDO
C
C
      CALL MESSCAL(NWE,AWEI,ASCH,AGRA,MWEI,MSCH,MGRA,WIS,WSO,
     &                      RREF,RMESS,IER)
C
      IF(IER.NE.0) THEN
        GOTO 900
      ENDIF
      DO L=1,NNR
        MK(L)=RMESS(L)
      ENDDO
 900  RETURN
      END SUBROUTINE
C
C
C
C
C

      SUBROUTINE RMEMI(NWEW,KMW,VE,VL,VC,VH,VA,VB,VLPH,DECHK,Q,
     &                 JAMI,VEW,IERR)
      USE MODRMEM
      USE MODWINK,ONLY:KM
      USE MODILLU,ONLY:NWE
      USE MODGKWR,ONLY:AKA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTERFACE
      SUBROUTINE QMEAN(KW,AKA,IANZ,NWE,R,RS,RQ)
      INTEGER(KIND=4) :: IANZ,NWE,KW
      REAL(KIND=8) :: AKA
      REAL(KIND=8),DIMENSION(*) ::  R,RS
      REAL(KIND=8),OPTIONAL,DIMENSION(*) :: RQ
      END SUBROUTINE
      SUBROUTINE QMEAS(KW,AKA,IANZ,NWE,R,RS,RQ,RO,RU,ROM,RUM)
      INTEGER(KIND=4) :: IANZ,NWE,KW
      REAL(KIND=8) :: AKA
      REAL(KIND=8),DIMENSION(*) ::  R,RS
      REAL(KIND=8),OPTIONAL,DIMENSION(*) :: RQ,RO,RU,ROM,RUM
      END SUBROUTINE
      SUBROUTINE QMITAL(AKA,KM,NWE,KME,KMN,R,RS,RQ,RM,RSN,RQN,IER)
      INTEGER(KIND=4) :: KM,NWE,IER
      REAL(KIND=8) :: AKA
      INTEGER(KIND=4),DIMENSION(*) :: KME,KMN
      REAL(KIND=8),DIMENSION(*) ::  R,RS,RQ,RSN,RQN
      REAL(KIND=8),DIMENSION(KM*NWE,*) :: RM
      END SUBROUTINE
      END INTERFACE

C


      INTEGER*4 JAMI,NWEW,KMW,IERR
      REAL*4 VLPH
      REAL*4 DECHK
      DIMENSION XYZ(3),XYZV(3)
      REAL*4 VE(*),VL(*),VC(*),VH(*),VA(*),VB(*),VEW(*)
      REAL*4 Q(*)
C
C     BERECHNUNG VON MITTELWERTEN UND STANDARDABWEICHUNGEN
C     FUER REFELEXIONSWERTE
C 
C
C 
C
      DATA DEB/1.D0/
      DATA ALPP/1.D-2/
C
C
C
      DLL_EXPORT RMEMI
C
C
      DECH=DECHK
      IF(DECH.LT.0.01D0) THEN
        DECH=0.01D0
      ENDIF
C
      IER=0
      ALPH=0.D0
C
      IER=0
      KND=NWEW*KMW
      IF(KND.NE.NWE*KM) THEN
        IER=4009
        GOTO 900
      ENDIF


        DO I=1,5
           DE(I)=0.D0
           DL(I)=0.D0
           DC(I)=0.D0
           DH(I)=0.D0
           DA(I)=0.D0
           DB(I)=0.D0
           VE(I)=0.  
           VL(I)=0.
           VC(I)=0.
           VH(I)=0.
           VA(I)=0.
           VB(I)=0.
        ENDDO
        DO I=1,IPM
             SDE(I)=0.D0
        ENDDO
C
        CALL SNGDBL(KND,R,Q)

C
C
        IF(JAMI.GE.0) THEN
           CALL QMITAL(AKA,KM,NWE,KME,KMN,R,RS,RQ,RM,RSN,RQN,IER)
        ENDIF
        IF(IER.NE.0) GOTO 900
C
C
C
           DO KW=1,KM
           NAD=(KW-1)*NWE
           NA1=NAD+1
C
C
C
             CALL QMEAS(KW,AKA,KME(KW),NWE,RH(NA1,2),RS(NA1),RQ(NA1),
     &       RH(NA1,1),RH(NA1,3),RH(NA1,4),RH(NA1,5))
             GKK=GLANZ(KW)
             CALL NOGXX(XYZV,1,RH(NA1,2),GKK)
             DO I=1,KME(KW)
                  CALL NOGXX(XYZ,1,RM(NA1,I),GKK)
                  CALL DELAB(XYZV,XYZ,DEH,DLH,DCH,DHH,DAH,DBH,1)
                  SDE(I)=SDE(I)+DEH**2
             ENDDO 
           ENDDO
C 
C
C          SORTIEREN 
C
C
           IAMI=KME(1)

           DO I=1,IAMI
             SDE(I)=SQRT(SDE(I)/IAMI)
           END DO
           ILETZT=KME(1)
           DO IJ=1,KME(1)-1
              DO IK=IJ+1,KME(1)
                IF(SDE(IJ).GT.SDE(IK)) THEN
                   SSD=SDE(IJ)
                   SDE(IJ)=SDE(IK)
                   SDE(IK)=SSD
                   IF(IK.EQ.ILETZT) THEN
                      ILETZT=IJ
                   ELSE IF(IJ.EQ.ILETZT) THEN
                      ILETZT=IK
                   ENDIF 
                   DO KW=1,KM 
                      NAD=(KW-1)*NWE 
                      DO I=1,NWE
                         RRR=RM(NAD+I,IJ)
                         RM(NAD+I,IJ)=RM(NAD+I,IK)  
                         RM(NAD+I,IK)=RRR
                      ENDDO 
                   ENDDO
                ENDIF       
              ENDDO
           ENDDO
           DO KW=1,KM
               KMN(KW)=0
               NAD=(KW-1)*NWE 
               DO I=1,NWE
                 RSN(NAD+I)=0.D0
                 RQN(NAD+I)=0.D0
               ENDDO
           ENDDO
C
           IF(IAMI.GE.3) THEN
             GRU=GRUBB(IAMI,DECH,ALPP)
             GRUB=SDE(IAMI)
             IF(GRUB.GT.GRU) THEN
                IER=-4010
             ENDIF
           ENDIF
           DO KK=1,IAMI
              DO KW=1,KM
               NAD=(KW-1)*NWE 
               CALL QMEAN(KW,AKA,KMN(KW),NWE,RM(NAD+1,KK),RSN(NAD+1),
     &                                   RQN(NAD+1))
              ENDDO
           ENDDO
           DO I=1,5
               DE(I)=0.D0
               DL(I)=0.D0
               DC(I)=0.D0
               DH(I)=0.D0
               DA(I)=0.D0
               DB(I)=0.D0
           ENDDO
           DO KW=1,KM
             GKK=GLANZ(KW)
             NAD=(KW-1)*NWE
             NA1=NAD+1
             CALL QMEAS(KW,AKA,KMN(KW),NWE,RH(NA1,2),RSN(NA1),RQN(NA1),
     &        RH(NA1,1),RH(NA1,3),RH(NA1,4),RH(NA1,5))
             CALL NOGXX(XYZV,1,RH(NA1,2),GKK)
             DO I=1,5
              IF(I.EQ.2) THEN
                  IF(JAMI.GT.0) THEN
                     CALL NOGXX(XYZ,1,R(NA1),GKK)
                  ELSE
C
C                    KEINE AKTUELLE MESSUNG VORHANDEN
C
                     DO J=1,3
                       XYZ(J)=-1.
                     END DO
                  endif
              ELSE      
                  CALL NOGXX(XYZ,1,RH(NA1,I),GKK)
              ENDIF
              IF(XYZ(1).LT.0..AND.XYZ(2).LT.0..AND.XYZ(3).LT.0.) THEN
                DEH=0.
                DLH=0.
                DCH=0.
                DHH=0.
                DAH=0.
                DBH=0.
              ELSE
                CALL DELAB(XYZV,XYZ,DEH,DLH,DCH,DHH,DAH,DBH,1)
              ENDIF
              IF(I.EQ.1) THEN
                 DEW(KW)=DEH**2
              ENDIF
              IF(I.EQ.3) THEN
                 DEZ=0.5*(DEW(KW)+DEH**2)
                 DEW(KW)=SQRT(ABS(DEZ))
              ENDIF
              DE(I)=DE(I)+DEH**2
              DL(I)=DL(I)+DLH
              DC(I)=DC(I)+DCH
              DH(I)=DH(I)+DHH
              DA(I)=DA(I)+DAH
              DB(I)=DB(I)+DBH
             ENDDO     
          ENDDO
          DO I=1,5
                DE(I)=SQRT(DE(I)/REAL(KM))
                DL(I)=DL(I)/REAL(KM)
                DC(I)=DC(I)/REAL(KM)
                DH(I)=DH(I)/REAL(KM)
                DA(I)=DA(I)/REAL(KM)
                DB(I)=DB(I)/REAL(KM)
          ENDDO
          IF(IAMI.GT.1) THEN
              PRU=(0.5*(DE(4)+DE(5))/DEB)**2
              ARG=SQRT(ABS(0.5*PRU))
              ALPH=ERF(ARG)
          ENDIF
C
C
C
C
        VLPH=ALPH
        JAMI=IAMI
        DO I=1,KM
           VEW(I)=DEW(I)
        ENDDO
        DO I=1,5
           VE(I)=DE(I)
           VL(I)=DL(I)
           VC(I)=DC(I)
           VH(I)=DH(I)
           VA(I)=DA(I)
           VB(I)=DB(I)
        ENDDO
C
C
  900 IERR=IER
      RETURN
C
      END
C
C
C
C
C
C
C
      SUBROUTINE RMERD(NWEW,KMW,Q,IER)
      USE MODRMEM
      USE MODWINK,ONLY:KM
      USE MODILLU,ONLY:NWE
      USE MODGKWR,ONLY:AKA
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
      INTERFACE
      SUBROUTINE QMRED(KW,AKA,IANZ,NWE,R,RS,RQ)
      INTEGER(KIND=4) :: IANZ,NWE
      REAL(KIND=8) :: AKA
      REAL(KIND=8),DIMENSION(*) ::  R,RS
      REAL(KIND=8),OPTIONAL,DIMENSION(*) :: RQ
      END SUBROUTINE
      END INTERFACE

C
      INTEGER*4 NWEW,KMW
C
      REAL*4 Q(*)
C
C
C
C
      DLL_EXPORT RMERD
C
C
C

C
C     LETZTE MESSUNG LÖSCHEN
C
      IER=0
C
      KND=NWEW*KMW
      IF(KND.NE.NWE*KM) THEN
        IER=4009
        GOTO 900
      ENDIF

C
C     
        CALL SNGDBL(KND,R,Q)
C
C
      IF(KME(1).GT.0) THEN
        DO KW=1,KM
           NAD=(KW-1)*NWE
           CALL QMRED(KW,AKA,KME(KW),NWE,R(NAD+1),RS(NAD+1),RQ(NAD+1))
        ENDDO
        DO K=ILETZT,KME(1)
             DO KW=1,KM 
               NAD=(KW-1)*NWE
               DO I=1,NWE
                  RM(NAD+I,K)=RM(NAD+I,K+1)
               ENDDO
             ENDDO
        ENDDO
        DO KW=1,KM
           KMN(KW)=KMN(KW)-1
           NAD=(KW-1)*NWE
           DO I=1,NWE
              RSN(NAD+I)=RS(NAD+I)
              RQN(NAD+I)=RQ(NAD+I)
           ENDDO
        END DO
      ENDIF
C
 900  RETURN
      END
C
C
C
C
C
C
C
C******************************************************************************
      SUBROUTINE RMEPR(IRT,NWEW,KMW,
     &                 QO,Q,QU,KENN,IERR)
      USE MODRMEM
      USE MODWINK,ONLY:KM
      USE MODILLU,ONLY:NWE
      USE MODGKWR,ONLY:AKA,GK
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C
C
      INTEGER*4 NWEW,KMW,KENN,IERR,IRT
      REAL*4 QO(*),Q(*),QU(*)
      DATA TOL/1.D-4/
C
C
C
C
      DLL_EXPORT RMEPR
C
C
C
C
      IER=0
      NWE=NWEW
      KM=KMW
      KND=KM*NWE
C
C 
C
      CALL QRRMIT(AKA,KM,NWE,KMN,R,RSN,RQN,RO,RU,IER)
C
      IFFE=0
      DO KW=1,KM
         NAD=(KW-1)*NWE
C
C
C     PRUEFEN AUF KLEINER GLANZ (NUR WERTE > 0. BERÜCKSICHTIGEN, DA U.U. UV-FILTER EINGESCHALTET)
C
C
C
         DO NW=1,NWE
           IF(R(NW+NAD).LT.0.) THEN
              R(NW+NAD)=0.
           ENDIF
         ENDDO
         DO NW=1,NWE
           IF(R(NW+NAD).GT.0.) THEN
             IF((R(NW+NAD)-GLANZ(KW)).LE.-TOL.AND.IFFE.EQ.0) THEN
               IFFE=NW
             ENDIF
           ENDIF
         ENDDO 
      ENDDO
C 
C
C
C
C 
  900 IERR=IER
      CALL DBLSNG(KND,R,Q)
      CALL DBLSNG(KND,RO,QO)
      CALL DBLSNG(KND,RU,QU)
      KENN=IFFE
C
C
      IF(ALLOCATED(GK)) THEN
        DEALLOCATE(GK)
      ENDIF
      IF(ALLOCATED(KME)) THEN
         DEALLOCATE(KME,KMN,DEW,
     &              RS,RQ,RSN,RQN,R,RO,RU,
     &              RH,RM)
      ENDIF
      RETURN
 895  IER=-4998
      GOTO 900
      END 
C*********************************************
C
C
      SUBROUTINE RMNUL(AKA,NWEW,KMW,Q,NGKW,GKW,MESID,ID,
     &           FAKT,XYZE,IERR)
      USE MODRMEM
      USE MODWINK,ONLY:KM
      USE MODILLU,ONLY:NWE
      USE MODGKWR,ONLY:GK
      USE MODFUNC,ONLY:JABST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 

C
      INTEGER*4 NWEW,KMW,ID,IERR
      INTEGER*4 MESID
      REAL(KIND=4) :: AKA
      REAL*4 GKW(NGKW,*)

C
      REAL*4 Q(*),XYZE(NWEW,*),FAKT(*)

C
C
C
C
C
      DLL_EXPORT RMNUL
C
C
C

C
C
      IER=0
C
C
C
C
C
C
      JABST=0
      MEID=MESID
      NORID=ID
C
C

C
C
      CALL QMZERO(AKA,NGKW,GKW,NWEW,KMW,Q,IER)
C
C
      ILETZT=0
      CALL GETNRM(NWEW,ID,FAKT,XYZE,IER)
      IF(IER.NE.0) GOTO 900

C
C
C
  900 IERR=IER
      RETURN
      END
C

