C     Last change: KU 14.11.2021 21:48:34
C
C
c      INCLUDE "FARBMOD.FOR"
C
C
      INCLUDE "XYZLABCH.FOR"
      INCLUDE "GETLWGK.FOR"
      INCLUDE "FRBMEN.FOR"

C
c      INCLUDE "FEHLIB.FOR"
c      INCLUDE "FAKER3.FOR"
c      INCLUDE "FAKER4.FOR"
c      INCLUDE "FAKER5.FOR"
c      INCLUDE "FRBL1.FOR"
c      INCLUDE "FRBL0.FOR"
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FARBEG  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
      SUBROUTINE FARBEG(NWE,KM,FEHL)
      USE MOTFEHL
      IMPLICIT NONE
      TYPE(TYFEH) FEHL
      INTEGER(KIND=4) ::NWE,KM,IFEHL
C
C
C
      INTERFACE
      INTEGER(KIND=4) FUNCTION DIMIER(NWEL,KML,NLZL,NPQGL,MNF,NUA)
      INTEGER(KIND=4) :: NWEL
      INTEGER(KIND=4),OPTIONAL :: KML,NLZL,NPQGL,MNF,NUA
      END FUNCTION
      END INTERFACE

      DLL_EXPORT FARBEG
      CALL FEHINI()
      IF(IFEHL(DIMIER(NWE,KML=KM)).NE.0) THEN
         GOTO 900
      ENDIF
 900  CALL GETFEH(FEHL)
      RETURN
      END SUBROUTINE
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FAREND  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
      SUBROUTINE FAREND(FEHL)
      USE MOTFEHL
      IMPLICIT NONE
      INTEGER(KIND=4) ::IER,IFEHL
      TYPE(TYFEH) FEHL
      DLL_EXPORT FAREND
      CALL TERMFDM(IER)
      IF(IFEHL(IER).NE.0) THEN
         CALL GETFEH(FEHL)
      ENDIF
      RETURN
      END SUBROUTINE

c
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FARKOORD  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
C
C
C
C
C
C
C
      SUBROUTINE FARKOORD(NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,
     &                  QUREDUN,TYREDUN,RDUN,
     &                  AUFGART,AUSWANZ,AUSWID,
     &                  NPAM,PARAM,WERT,FEHL)

C      SUBROUTINE FARKOR(IPRI,NQUU,QUREDAM,TYREDAM,QUREDUN,TYREDUN,
C     &                  MENALL,NOR2,AUSWHL,PARMERK,NPAM,PARAM,FEHL)
      USE MOTFEHL
      USE MOTWERT
      USE MOSRWRT
      USE MOTTYRW
      USE MOTQURW
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     BERECHNUNG VON FARBKOORDINATEN UND FARBABSTAENDEN
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C
C
C
      SAVE NPPA,NPB,NWT,NWP,KNO,CMETH,CMATH
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*):: RDAM,RDUN
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      INTEGER(KIND=1),DIMENSION(12) :: AUFGART
      INTEGER(KIND=4),DIMENSION(*):: AUSWID
      INTEGER(KIND=4) :: KWW,NWEL,KML,NQU,NPAM,AUSWANZ,IER,IFEHL
      INTEGER(KIND=4) :: L,J,LITP,NPPA,NPB,NWP,NWT,KNO
      REAL(KIND=8),TARGET,ALLOCATABLE,DIMENSION(:) :: RZ,RWT,RWTW,RWTS,
     &                                                RUN11,RUN12,
     &                                                RUN21,RUN22,
     &                                                RUK11,RUK12,
     &                                                RUK21,RUK22
      TYPE(SRWERT) RWERP,RWERT,REDTW,REDTS,RWERUN(2,2),RWERUK(2,2)
      TYPE(TYFEH) FEHL
      LOGICAL RWOK,RWHR
      CHARACTER*6 BLAN
C
C
C
      CHARACTER*12 CMETH
      CHARACTER*6 CMATH
      DATA BLAN/'      '/
C
      DLL_EXPORT FARKOORD
      IER=0
C
c      OPEN(27,FILE='TESTDE.TXT')
C
C
        ALLOCATE(RZ(NWEL),RWT(NWEL),RWTW(NWEL),RWTS(NWEL),
     &                  RUN11(NWEL),RUN12(NWEL),RUN21(NWEL),RUN22(NWEL),
     &                  RUK11(NWEL),RUK12(NWEL),RUK21(NWEL),RUK22(NWEL),
     &                  STAT=IER)
        IER=IERALC(IER)
        IF(IFEHL(IER).NE.0)THEN
           GOTO 900
        ENDIF
        RWERP%R=>RZ
        RWERT%R=>RWT
        REDTW%R=>RWTW
        REDTS%R=>RWTS
        RWERUN(1,1)%R=>RUN11
        RWERUN(1,2)%R=>RUN12
        RWERUN(2,1)%R=>RUN21
        RWERUN(2,2)%R=>RUN22
        RWERUK(1,1)%R=>RUK11
        RWERUK(1,2)%R=>RUK12
        RWERUK(2,1)%R=>RUK21
        RWERUK(2,2)%R=>RUK22

        CALL SRT000(RWERP)
        DO I=1,2
          DO JJ=1,2
           CALL SRT000(RWERUN(I,JJ))
           CALL SRT000(RWERUK(I,JJ))
          END DO
        END DO

        CALL FAG000
C       CALL OPAUSG(IPRN,IER)
C       IF(IFEHL(IER).NE.0) GOTO 900

c
        DO JJ=1,NPAM
         PARAM(JJ)%ID=-1
         PARAM(JJ)%NR=-1
         PARAM(JJ)%LNR=-1
         PARAM(JJ)%AuswID=-1
         PARAM(JJ)%ITP=0
         PARAM(JJ)%KWB=0
         PARAM(JJ)%CMETH=BLAN
         DO I=1,64
            WERT(I,JJ)=HUGE(1.)
         ENDDO
        ENDDO
        PARAM(NPAM)%ID=-999
c
c      CALL GETMEN(0,MENALL,KWW,IER)
c      IF(IFEHL(IER).NE.0) GOTO 900
c      CALL GETNORM(MENALL,NOR2%NORM,IER)
c      IF(IFEHL(IER).NE.0) GOTO 900
c      CALL GET6164(MENALL,NOR2%NO6164,IER)
c      IF(IFEHL(IER).NE.0) GOTO 900
c
c      Anzahl Anweisungen
C
        NPB=AUSWANZ
C
        DO I=1,12
          CMETH(I:I)=CHAR(AUFGART(I))
        END DO
        CMATH=CMETH(7:12)
        IF(.NOT.RWHR(CMETH(1:4)).OR..NOT.RWHR(CMATH(1:4))) THEN
         IER=4035
         IF(IFEHL(IER).NE.0) GOTO 900
        ENDIF
C
C
C       PRUEFUNG FUER WINKEL
C
C
C
        NWP=ICHAR(CMETH(12:12))-48
        NWT=ICHAR(CMETH(6:6))-48
        IF(NWP.NE.NWT) THEN
           MOX=MOD(NPAM,2)
           IF(MOX.EQ.1) THEN
              IER=4038
              IF(IFEHL(IER).NE.0) GOTO 900
           ENDIF
C
           IF(CMETH(1:4).NE.CMETH(7:10)) THEN
            IER=4002
            IF(IFEHL(IER).NE.0) GOTO 900
           ENDIF
         ENDIF
C
C
C       VERGLEICH BEI GLEICHEN WINKELN
C
        IF(NWT.EQ.NWP) THEN
           NPPA=NQU*NPB
C
C       VERGLEICH BEI VERSCHIEDENEN WINKELN
C
        ELSE
           NPPA=2*NQU*NPB
        ENDIF
C
C
C
        IF(NPAM.NE.NPPA) THEN
          IER=4037
          IF(IFEHL(IER).NE.0) GOTO 900
        ENDIF
C
C       Berechnung von korrigierten Reflexionswerten fuer Untergruende
C
C
C        Erster Winkel und zu bearbeitender Winkel (Typ und Probe)
C
C
        DO NW=1,MIN(2,KMS())
         IF(NW.EQ.1) THEN
            KWW=NWT
         ELSE
            KWW=NWP
         ENDIF
         DO KU=1,2
          DO I=1,NWS()
             RHI=RDUN(I,KWW,KU)
             RWERUN(NW,KU)%R(I)=RHI
             RWERUK(NW,KU)%R(I)=RUKORR(RHI,KWW)
          ENDDO
         ENDDO
        ENDDO
C
C
c
C
C
C
C      PRUEFUNG FUER NORMLICHTART
C
C
C
C

        KNOP=ICHAR(CMETH(11:11))-48
        KNOT=ICHAR(CMETH(5:5))-48
        IF(KNOT.NE.KNOP) THEN
          IER=4001
          IF(IFEHL(IER).NE.0) GOTO 900
        ENDIF
        KNO=KNOP
C
C
C
C
C     Startwert LNRST und Schrittweite LNRDE für LNR
C
C
      LNRST=0
      LNRDE=1
      IF(CMETH(1:1).EQ.CMATH(1:1)) THEN
        DO L=1,NQU
          IF(CMETH(1:1).EQ.QUREDAM(L)%CART(1:1)) THEN
            LNRST=L-1
            DO I=L+1,NQU
             IF(CMETH(1:1).EQ.QUREDAM(I)%CART(1:1)) THEN
               LNRDE=I-L
               EXIT
             ENDIF
            END DO
            EXIT
          ENDIF
        END DO
      ENDIF
      LNR=LNRST-LNRDE
      LITPW=1
      LITPS=0
      LW=0
      LS=0
      J=1-NPB
      DO L=1,NQU
        J=J+NPB
C
C       Gleiche Winkel (PROBE-BEZUG)
C
C
C
        IF(NWT.EQ.NWP) THEN
            CALL GETKPF(NWP,TYREDAM(L),QUREDAM(L),RWERP,IER)
            CALL GETRWRT(NWEL,RDAM(1,NWP,L),RWERP%R)
            IRT=TYREDAM(L)%RETR
            IF(IFEHL(IER).NE.0) GOTO 900
          IF(RWOK(1,CMETH,RWERP%CART(1:4))) THEN
              ITP=1
              LNR=LNR+LNRDE
              IUNT=IUNTGR(RWERP%CART(4:4))
                CALL FAGEN(NWEL,KML,ITP,NWP,KNO,LNR,NPB,AUSWID,
     &          CMETH,RWERP,RWERT,REDTW,REDTS,
     &          RWERUN(1,IUNT),RWERUK(1,IUNT),
     &          PARAM(J),WERT(1,J),IER)
                IF(IFEHL(IER).NE.0) GOTO 900
                IF(RWERP%IVONA.EQ.1) THEN
                 IF(RWERP%CART(4:4).EQ.'W') THEN
                  LITPW=L
                  LITPS=0
                  LW=L
                  LS=0
                 ELSEIF(RWERP%CART(4:4).EQ.'S') THEN
                  LITPS=L
                  LS=L
                 ENDIF
                ENDIF
                CALL FAALL(NWEL,KML,ITP,NWP,KNO,LNR,NPB,AUSWID,CMETH,
     &          L,LITPW,LW,LITPS,LS,QUREDAM,TYREDAM,RDAM,
     &          IUNT,QUREDUN,TYREDUN,RDUN,PARAM(J),WERT(1,J),IER)
C
C
                IF(IFEHL(IER).NE.0) GOTO 900

           ELSEIF (RWOK(1,CMATH,RWERP%CART(1:4)))THEN
              ITP=2
              LNR=LNR+LNRDE
              IUNT=IUNTGR(RWERP%CART(4:4))
                CALL FAGEN(NWEL,KML,ITP,NWP,KNO,LNR,NPB,AUSWID,
     &          CMATH,RWERP,RWERT,REDTW,REDTS,
     &          RWERUN(1,IUNT),RWERUK(1,IUNT),
     &          PARAM(J),WERT(1,J),IER)
                IF(IFEHL(IER).NE.0) GOTO 900
                IF(RWERP%IVONA.EQ.1) THEN
                 IF(RWERP%CART(4:4).EQ.'W') THEN
                  LW=L
                 ELSEIF(RWERP%CART(4:4).EQ.'S') THEN
                  LS=L
                 ENDIF
                ENDIF
                CALL FAALL(NWEL,KML,ITP,NWP,KNO,LNR,NPB,AUSWID,CMATH,
     &          L,LITPW,LW,LITPS,LS,QUREDAM,TYREDAM,RDAM,
     &          IUNT,QUREDUN,TYREDUN,RDUN,PARAM(J),WERT(1,J),IER)
                IF(IFEHL(IER).NE.0) GOTO 900
           ELSE
               CYCLE
           ENDIF
C
           IF(IFEHL(IER).NE.0) GOTO 900
C
C       VERSCHIEDENE WINKEL
C
        ELSE
            CALL GETKPF(NWT,TYREDAM(L),QUREDAM(L),RWERP,IER)
            CALL GETRWRT(NWEL,RDAM(1,NWT,L),RWERP%R)
            IRT=TYREDAM(L)%RETR
            IF(IFEHL(IER).NE.0) GOTO 900
            IF(.NOT.RWOK(1,CMETH,RWERP%CART(1:4))) THEN
              CYCLE
            ENDIF
            LNR=LNR+LNRDE
            IUNT=IUNTGR(RWERP%CART(4:4))
              CALL FAGEN(NWEL,KML,1,NWT,KNO,LNR,NPB,AUSWID,
     &          CMETH,RWERP,RWERT,REDTW,REDTS,
     &          RWERUN(1,IUNT),RWERUK(1,IUNT),
     &          PARAM(J),WERT(1,J),IER)
                IF(IFEHL(IER).NE.0) GOTO 900


C
C
           CALL GETKPF(NWP,TYREDAM(L),QUREDAM(L),RWERP,IER)
           CALL GETRWRT(NWEL,RDAM(1,NWP,L),RWERP%R)
           IRT=TYREDAM(L)%RETR
           IF(IFEHL(IER).NE.0) GOTO 900
           IF (.NOT.RWOK(1,CMATH,RWERP%CART(1:4))) THEN
              CYCLE
           ENDIF
           IUNT=IUNTGR(RWERP%CART(4:4))
c           IF(RWERP%IVONA.EQ.1) THEN
             CALL FAGEN(NWEL,KML,2,NWP,KNO,LNR,NPB,AUSWID,
     &          CMATH,RWERP,RWERT,REDTW,REDTS,
     &          RWERUN(2,IUNT),RWERUK(2,IUNT),
     &          PARAM(J),WERT(1,J),IER)
                IF(IFEHL(IER).NE.0) GOTO 900
c           ENDIF
C
C
        ENDIF
      ENDDO

c
  900 DEALLOCATE(RZ,RWT,RWTW,RWTS,RUN11,RUN12,RUN21,RUN22,
     &              RUK11,RUK12,RUK21,RUK22,STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
       GOTO 999
      ENDIF
 999  CALL GETFEH(FEHL)
c     CLOSE(27)
      RETURN
c
      END


C                         +-------+
C                         ! FAALL !
C                         +-------+
      SUBROUTINE FAALL(NWEL,KML,ITP,KW,KNO,JJ,IANZ,ID,CMETH,
     &           L,LITPW,LW,LITPS,LS,QUREDAM,TYREDAM,RDAM,
     &           IUNT,QUREDUN,TYREDUN,RDUN,PARAM,WERT,IER)
      USE MOTWERT
      USE MOTTYRW
      USE MOTQURW
      USE MODWINK
      USE MODFUNC

      USE MODFAKT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     NWEL = Anzahl Wellenlängen
C     KML = Maximale Anzahl Winkel
C     ITP= 1=Bezug;2=Probe
C     KW = Nummer Winkel
C     KNO= Nummer Lichtart
C     JJ = laufende Rwert-NR
C     IANZ = Anzahl Auswertungen
C     ID= AuswerteID (AUSWID)
C     CMETH = Auswertestring (z.B. @TMW??)
C     L = Nummer aktuelle Messung
C     LITPW= Nummer aktueller Bezug (weiß)
C     LW = Nummer aktuelle Weißmessung
C     LITPS = Nummer aktueller Bezug (schwarz)
C     LS = Nummer aktuelle Schwarzmessung
C     QUREDAM = Auswertestrings
C     TYREDAM = dsgl.
C     RDAM = R-Werte
C     IUNT = Nummer Untergrund
C     QUREDUN Kenungen Untergrund
C     TYREDUN = dsgl.
C     RDUN = R-Werte Untergrund
C     PARAM = Parameter für aktuelle Auswertung (1 .... IANZ)
C     WERT = Werte für zur Auswertung zugehörenden Merkmale
C     IER = Fehlercode
C
C
C
C
C     MERKMALEN, DIE AUS DEN KURVEN MEHRERER WINKEL BERECHNET WERDEN
C
      TYPE(TYPARAM) PARAM(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*)
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*)
      REAL(KIND=8),DIMENSION(64,*):: WERT
      REAL(KIND=8) :: MAXDE,MAXWIN,MDE,DEMIT
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDUN
      CHARACTER(LEN=*) ::  CMETH
      DIMENSION XYZ(3)
      DIMENSION ALAB(3),ALCH(3),AL(3),XYZV(3),XYZN(3)
      DIMENSION R(NWEL),RV(NWEL),RN(NWEL)
      INTEGER*4 ID(*)
      IRT=TYREDAM(L)%RETR
      DO J=1,IANZ
        PARAM(J)%LNR=JJ
        IER=0
C
        SELECT CASE (ID(J))
C
C
C
        CASE (17)
C
C       FLOP-INDEX
C
C
        IF(LW.EQ.L) THEN
C
C        Merkmale für weiße Messungen

C
C
C        Winkel 45 Grad zur Berechnumng spezieller Werte und Mittelwerten über Winkel
C
C
         AL(1)=-1.0
         AL(2)=-1.0
         AL(3)=-1.0
         IF(IHRMWIN(KW).EQ.45) THEN
C
C        Merkmale für weiße Messungen und Winkel 45 Grad
C
C
           DO K=1,KMS()
             KK=0
             IF(IHRMWIN(K).EQ.45) THEN
C
C              45 Grad gefunden
C
C
C
                KK=2
             ELSEIF(IHRMWIN(K).EQ.15) THEN
C
C              15 Grad gefunden
C
C
               KK=1
             ELSEIF(IHRMWIN(K).EQ.110) THEN
C
C              110 Grad gefunden
C
C
               KK=3
             ENDIF
             IF(KK.GT.0) THEN
                DO I=1,NWS()
                 R(I)=RDAM(I,K,L)
                END DO
                GKK=GLZNOG(K,IRT)
                CALL NOGXX(XYZ,KNO,R,GKK)
                CALL LCH(XYZ,ALAB,ALCH,KNO)
                AL(KK)=ALAB(1)
             ENDIF
           ENDDO
           IF(AL(1).GE.0.0.AND.AL(2).GE.0.0.AND.AL(3).GE.0.0) THEN
C
C             FLOP-INDEX
C
C
              DAL=AL(1)-AL(3)
              IF(DAL.GT.0.D0) THEN
           	WERT(2,J)=2.69*DAL**1.11/(AL(2)**0.86+TINY(1.))
              ENDIF
            ENDIF
C
C           mdE und maxdE nach DIN6175-2
C
            IF(ITP.EQ.2) THEN
              MAXWIN=-999
              MDE=0.0
              MAXDE=0.0
              DEMIT=0.0
              SUFWIN=1.E-30
              KMM=0
              DO K=1,KMS()
                IF(IHRMWIN(K).LT.1000) THEN
                 SUFWIN=SUFWIN+FWI(K)
                 KMM=KMM+1
                 CALL SLCHAB(NWEL,K,KNO,RDAM(1,1,LITPW),RDAM(1,1,LW),
     &                       SL,SC,SH,SA,SB)
                 CALL CALDEEFF(NWEL,K,KNO,RDAM(1,1,LITPW),RDAM(1,1,LW),
     &                         SL,SC,SH,SA,SB,DEFF,DE)
                 DEMIT=DEMIT+DE**2
                 MDE=MDE+DEFF
                 IF(DEFF.GT.MAXDE) THEN
                    MAXDE=DEFF
                    MAXWIN=IHRMWIN(K)
                 ENDIF
                ENDIF
              END DO
              WERT(3,J)=MDE/KMM
              WERT(4,J)=MAXDE
              WERT(5,J)=MAXWIN
              WERT(6,J)=SQRT(DEMIT/SUFWIN)
             ENDIF
C
C            Ende 45 Grad weiß
C
         ENDIF
C
C
         IF(ITP.EQ.2.AND.IHRMWIN(KW).LT.1000) THEN
C
C          Differenzen Messungen weiß Probe(weiß) LW Bezug(weiß) LITPW
C
C          dEeff
C
C
           CALL SLCHAB(NWEL,KW,KNO,RDAM(1,1,LITPW),RDAM(1,1,LW),
     &                 SL,SC,SH,SA,SB)
           CALL CALDEEFF(NWEL,KW,KNO,RDAM(1,1,LITPW),RDAM(1,1,LW),
     &                   SL,SC,SH,SA,SB,DEFF,DE)
           WERT(1,J)=DEFF
         ENDIF
C
C
C       Ende Messungen weiß
C
        ELSEIF(LS.EQ.L)THEN
C
C       Vergleich weiß(LW) mit scharz (LS)
C
        IF(IHRMWIN(KW).EQ.45) THEN
C
C         Mittelwert Kontrastfarbabstand über alle Winkel
C
          IF(LS.GT.0.AND.LW.GT.0) THEN
C
C         Mittelwert Kontrast-DE über alle Winkel
C
              KMM=0
              DKDEMIT=0.0
              DO K=1,KMS()
                IF(IHRMWIN(K).LT.1000) THEN
                 DO I=1,NWEL
                   RV(I)=RDAM(I,K,LW)
                   RN(I)=RDAM(I,K,LS)
                 END DO
                 KMM=KMM+1
                 GKK=GLZNOG(K,IRT)
                 CALL NOGXX(XYZV,KNO,RV,GKK)
                 CALL NOGXX(XYZN,KNO,RN,GKK)
                 CALL DELABAL(JABST,XYZV,XYZN,
     &                        DES,DLS,DCS,DHS,DAS,DBS,KNO)
                 DKDEMIT=DKDEMIT+DES**2
                ENDIF
              ENDDO
              IF(KMM.GT.0) THEN
                DKDEMIT=SQRT(DKDEMIT/KMM)
              ENDIF
              WERT(11,J)=DKDEMIT
C
C             Ende Mittelwert Kontrastfarbabstand
C
          ENDIF
C
C         Ende 45 Grad
C
        ENDIF
C
C       Ende Messungen schwarz
C
        ENDIF
C
C
        CASE DEFAULT
C
        END SELECT
      ENDDO
 900  RETURN
      END
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C                         +-------+
C                         ! FAGEN !
C                         +-------+
      SUBROUTINE FAGEN(NWEL,KML,ITP,KW,KNO,JJ,IANZ,ID,CMETH,
     &          RWERP,RWERT,REDTW,REDTS,RWERUN,RWERUK,PARAM,WERT,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODWINK

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE XYZT,XYZTW,XYZTS,XYZPW,XYZPS,XYZU,XYZUW,XYZUS
C
C
      TYPE(SRWERT) RWERP,RWERT,REDTW,REDTS,RWERUN,RWERUK
C
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*):: WERT
      CHARACTER*(*) CMETH
      DIMENSION XYZP(3),XYZT(3),XYZTW(3),XYZTS(3)
      DIMENSION XYZPW(3),XYZPS(3)
      DIMENSION XYZU(3),XYZUW(3),XYZUS(3)
      INTEGER IOP,KWB
      INTEGER*4 ID(*)
C
C     XYZP     AKTUELLER WERT
C     XYZT     ZUGEHÖRIGER TYP
C     XYZPW    AKTUELLER WERT UEBER WEISS
C     XYZPS    AKTUELLER WERT UEBER SCHWARZ
C     XYZTW    TYP UEBER WEISS
C     XYZTS    TYP UEBER SCHWARZ



      DATA XYZPW/3*-1./,XYZTW/3*-1./,XYZPS/3*-1./,XYZTS/3*-1./
      DATA XYZUW/3*-1./,XYZUS/3*-1./
      DATA IOP/0/
C
      IER=0
      IEE=0
C
C
      IF(IOP.EQ.0) THEN
         RWERT%CART='      '
         IOP=1
      ENDIF
C
      IRT=RWERP%RETR
      GKK=GLZNOG(KW,IRT)
      IF(RWERP%CART(4:4).EQ.'W') THEN
        IF(RWERP%CART(2:2).EQ.'T') THEN
          CALL NOGXX(XYZTW,KNO,RWERP%R,GKK)
        ELSEIF(RWERP%CART(2:2).EQ.'P') THEN
          CALL NOGXX(XYZPW,KNO,RWERP%R,GKK)
        ENDIF
      ELSEIF(RWERP%CART(4:4).EQ.'S') THEN
        IF(RWERP%CART(2:2).EQ.'T') THEN
          CALL NOGXX(XYZTS,KNO,RWERP%R,GKK)
        ELSEIF(RWERP%CART(2:2).EQ.'P') THEN
          CALL NOGXX(XYZPS,KNO,RWERP%R,GKK)
        ENDIF
      ENDIF
      KWB=1
      IF(ITP.EQ.1) THEN
c       RWERT=RWERP
        CALL RWCLONE(NWEL,RWERT,RWERP)
        CALL NOGXX(XYZT,KNO,RWERP%R,GKK)
        CALL NOGXX(XYZU,KNO,RWERUN%R,GKK)
        IF(RWERP%CART(4:4).EQ.'S') THEN
           KWB=2
           CALL RWCLONE(NWEL,REDTS,RWERT)
           DO I=1,3
              XYZUS(I)=XYZU(I)
           ENDDO
        ELSE
           CALL RWCLONE(NWEL,REDTW,RWERT)
           DO I=1,3
              XYZUW(I)=XYZU(I)
           ENDDO
        ENDIF
        DO I=1,3
           XYZP(I)=XYZT(I)
        ENDDO
      ENDIF
C
      IF(ITP.EQ.2) THEN
        CALL NOGXX(XYZP,KNO,RWERP%R,GKK)
        CALL NOGXX(XYZU,KNO,RWERUN%R,GKK)
        IF(RWERP%CART(4:4).EQ.'S') THEN
           KWB=2
           DO I=1,3
              XYZUS(I)=XYZU(I)
           ENDDO
        ELSE
           DO I=1,3
              XYZUW(I)=XYZU(I)
           ENDDO
        ENDIF
        IF(CMETH(4:4).EQ.'$') THEN
             IF(RWERP%CART(4:4).EQ.'S') THEN
                 CALL RWCLONE(NWEL,RWERT,REDTS)
                 DO I=1,3
                    XYZT(I)=XYZTS(I)
                 ENDDO
             ELSE
                 CALL RWCLONE(NWEL,RWERT,REDTW)
                 DO I=1,3
                    XYZT(I)=XYZTW(I)
                 ENDDO
             ENDIF
        ENDIF
        IF(RWERP%RETR.NE.RWERT%RETR) THEN
           IER=4151
           RETURN
        ENDIF
      ENDIF
C
C
C
      IF(XYZUW(2).LE.0.D0.AND.RWERP%CART(4:4).EQ.'W') THEN
         IEE=-4119
C         RETURN
      ENDIF
C
C
C
C
C
      IEND=0
C
C
C
C
      DO J=1,IANZ

         IF(IEND.EQ.1) THEN
            IER=4007
            RETURN
         ENDIF
         IF(PARAM(J)%ID.EQ.-999) THEN
            PARAM(J)%ID=-1
            IEND=1
         ENDIF
C
         PARAM(J)%ID=RWERP%ID
         PARAM(J)%NR=RWERP%NR
         PARAM(J)%LNR=JJ
         PARAM(J)%AuswID=ID(J)
         PARAM(J)%ITP=ITP
         PARAM(J)%KWB=KWB
         PARAM(J)%RETR=RWERP%RETR
         PARAM(J)%CMETH=CMETH
         PARAM(J)%CMETH(1:4)=RWERP%CART
C
        IF(RWERP%IAMI.GE.0.AND.RWERP%IVONA.EQ.1) THEN
C
C
        SELECT CASE (ID(J))
C
C
C
        CASE (0)
C
C         KOPF-DATEN
C
          CALL LIKPF(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
        CASE (1)
C
C
C         X Y Z  -  WERTE
C
          CALL LIXYZ(ITP,KNO,XYZT,XYZP,XYZUW,
     &           RWERT,RWERP,RWERUN,RWERUK,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
        CASE (2)
C         C I E L A B   W E R T E
C
          CALL LICIE(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
        CASE (3)
C
C
C
C
C
C        CIE2000
C
          CALL LI2000(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,PARAM(J),WERT(1,J),KW,IER)

C
C
C         TC29 (CIE94)
C
C
          CALL LICTC(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP, PARAM(J),WERT(1,J),KW,IER)

        CASE(4)
C
C
C         CMC(Original)
C
C
          CALL LICMO(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,PARAM(J),WERT(1,J),KW,IER)

        CASE (5)

C        DIN 6167 CIE9-Werte
C
          CALL LICIE9(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C

        CASE (7)


C         C M C NACH CLARKE DONALD RIGG
C
          CALL LICMC(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
        CASE (9)
C
C
C         J P C 7 9
C
          CALL LIJPC(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
        CASE (10)
C
C         DIN 6164
C
          CALL LIDIN(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
        CASE (11)

C         CIELUV  L* U* V*

          CALL LILUV(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
        CASE (12)

C         S P E Z I A L  (WEISSGRAD,GELBWERT,OPAZITAET)
C
C
          CALL LISPZ(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
        CASE (13)
C
C         S P E Z I A L  (FARBSTAERKE) TRANSPARENT
C
C
          CALL LISTAT(ITP,KNO,XYZT,XYZP,XYZUW,
     &           RWERT,RWERP,
     &           RWERUN,RWERUK,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
C
        CASE (14)


C         O P T I S C H E   D I C H T E N
C
C
          CALL LOPTD(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,RWERUN,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
        CASE (15)
C
C
C
C         S P E Z I A L  (FARBSTAERKE) DECKEND
C
C
          CALL LISTAD(ITP,KNO,XYZT,XYZP,XYZUW,
     &           RWERT,RWERP,
     &           RWERUN,RWERUK,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
        CASE (16)
C
C
C
C         S P E Z I A L (1)
C
C
          CALL LISP1(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)

        CASE (17)
C
C
C
C         DIM6175 s. FAALL
C
C
c          CALL L6175M(ITP,KNO,XYZT,XYZP,
c     &           RWERT,RWERP,
c     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
C
C
C
C
        CASE (18)

C         KUNDENWUNSCH
C
C
C         WERT(60...62)=XYZT,XYZP
C
          CALL LIKUN(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
C
        CASE (19)

C         KUNDENWUNSCH
C
C         WERT(60...62)=XYZT,XYZP
C
C
          CALL LIKUN(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
C
C
C
        CASE (20)

C         KUNDENWUNSCH
C
C
C         WERT(60...62)=XYZT,XYZP
C
C
          CALL LIKUN(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
C
        CASE (21)
C
C
C          A N S  (AN40)
C
           CALL LIANS(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
C
C
        CASE (22)


C         F M C 2   F R I E L E    MAC ADAM   CHICKERING
C
          CALL LIFMC(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
        CASE (23)

C          H U N T E R
C
           CALL LIHUN(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
        CASE (24)
C
C
C
C         MAC ADAM-SIMON-GOODWIN
C
          CALL LISIM(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
        CASE (25)
C
C
C
C
C         J U D D - H U N T E R (NBS)
C
          CALL LINBS(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
C
C
C
        CASE (26)

C         KUNDENWUNSCH
C
C         WERT(60...62)=XYZT,XYZP
C
C
C
          CALL LIKUN(ITP,KNO,XYZT,XYZP,
     &           RWERT,RWERP,
     &           PARAM(J),WERT(1,J),KW,IER)
        END SELECT
        ELSE
          IF(RWERT%IAMI.LT.0) THEN
             IER=4179
          ENDIF
        ENDIF
        IF(IER.NE.0) GOTO 900
      ENDDO
 900  IF(IER.EQ.0) THEN
        IER=IEE
      ENDIF
      RETURN
      ENTRY FAG000
      IOP=0
      DO I=1,3
       XYZPW(I)=-1.
       XYZTW(I)=-1.
       XYZPS(I)=-1.
       XYZTS(I)=-1.
       XYZUW(I)=-1.
       XYZUS(I)=-1.
      END DO
      RETURN
      END
      SUBROUTINE RWCLONE(NWE,RTARGET,RSOURCE)
      USE MOSRWRT
      TYPE(SRWERT) RTARGET,RSOURCE
      RTARGET%ID=RSOURCE%ID
      RTARGET%NR=RSOURCE%NR
      RTARGET%CART=RSOURCE%CART
      DO I=0,7
        RTARGET%CAMP(I)=RSOURCE%CAMP(I)
      END DO
      RTARGET%DESTD=RSOURCE%DESTD
      RTARGET%RETR=RSOURCE%RETR
      RTARGET%IAMI=RSOURCE%IAMI
      RTARGET%IVONA=RSOURCE%IVONA
      DO I=1,NWE
        RTARGET%R(I)=RSOURCE%R(I)
      END DO
      RETURN
      END SUBROUTINE
C
C
C
C                                  ----------
C                                  | DEeffVW|
C                                  ----------
C
      SUBROUTINE DEEFFVW(NWEL,KW,KNO,RDAT,RDAP,SL,SC,SH,SA,SB,DEFF,IER)
      USE MODFUNC

C
C     dEeff', mdE', maxdE' für VW
C     (analog zu DIN6175-2, wobei SL,SC,SH,Sa und Sb vorgegeben sind)
C
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=4),DIMENSION(NWEL,*) :: RDAP,RDAT
      REAL(KIND=4) :: SL,SC,SH,SA,SB,DEFF
      INTEGER(KIND=4) :: NWEL,KW,KNO
C
      DLL_EXPORT DEEFFVW
C
      IER=0
      IF(JABST.EQ.-1) THEN
        IER=-1
        RETURN
      ENDIF
      CALL CALDEEFF(NWEL,KW,KNO,RDAT,RDAP,
     &     DBLE(SL),DBLE(SC),DBLE(SH),DBLE(SA),DBLE(SB),DDEFF,DE)
      DEFF=DDEFF
      RETURN
      END SUBROUTINE

C
C

      SUBROUTINE CALDEEFF(NWEL,KW,KNO,RDAT,RDAP,SL,SC,SH,SA,SB,DEFF,DE)
      USE MODFAKT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=4),DIMENSION(NWEL,*) :: RDAP,RDAT
      REAL(KIND=8),DIMENSION(3) :: XYZT,ALABT,ALCHT,XYZP,ALABP,ALCHP
      INTEGER(KIND=4) :: IRT,KW,KNO,NWEL
      DATA IRT/0/
      DATA TOO/1.0D-4/
C
C
C

      CALL COLVALUE(NWEL,IRT,KW,KNO,RDAT(1,KW),XYZT,ALABT,ALCHT)
      CALL COLVALUE(NWEL,IRT,KW,KNO,RDAP(1,KW),XYZP,ALABP,ALCHP)

      CALL DELAB(XYZT,XYZP,DE,DL,DC,DH,DA,DB,KNO)


C
      DEFF=0.0
      DEAB=0.0
      DECH=0.0
      IF(SL.GT.TOO.AND.SA.GT.TOO.AND.SB.GT.TOO) THEN
        DEAB=SQRT(ABS((DL/(GL*SL))**2+(DA/(GA*SA))**2+(DB/(GB*SB))**2))
      ENDIF
      IF(SL.GT.TOO.AND.SC.GT.TOO.AND.SH.GT.TOO) THEN
         DECH=SQRT(ABS((DL/(GL*SL))**2+(DC/(GC*SC))**2+(DH/(GH*SH))**2))
      ENDIF

      IF(DEAB.GT.TOO.AND.DECH.GT.TOO) THEN
C
C
C
C       GEOM. MITTEL L*
C
C
        ALM=SQRT(ABS(ALABT(1)*ALABP(1)))
C
C       GEOM. MITTEL C*
C
C
        ACM=SQRT(ABS(ALCHT(2)*ALCHP(2)))
c
c

C
C
        C0=10.0+8./(1.+EXP(27.0-ALM))
        SIG=1.0/(1.0+EXP(ACM-C0))
C
C
C
        DEFF=SIG*DEAB+(1.-SIG)*DECH
      ELSEIF(DEAB.GT.TOO) THEN
C
C         SC=0.0 OR SH=0.0
C
          DEFF=DEAB
      ELSEIF(DECH.GT.TOO) THEN
C
C         SA=0.0 OR SB=0.0
C
          DEFF=DECH
      ENDIF
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE SLCHAB(NWEL,KW,KNO,RDAT,RDAP,SL,SC,SH,SA,SB)
      USE MODWINK
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=4),DIMENSION(NWEL,*) :: RDAP,RDAT
      REAL(KIND=8),DIMENSION(3) :: XYZT,ALABT,ALCHT,XYZP,ALABP,ALCHP
      INTEGER(KIND=4) :: IRT
      DATA IRT/0/

C
C
C
      CALL COLVALUE(NWEL,IRT,KW,KNO,RDAT(1,KW),XYZT,ALABT,ALCHT)
      CALL COLVALUE(NWEL,IRT,KW,KNO,RDAP(1,KW),XYZP,ALABP,ALCHP)
C
C     GEOM. MITTEL L*
C
C
      ALM=SQRT(ABS(ALABT(1)*ALABP(1)))
C
C     GEOM. MITTEL C*
C
C
      ACM=SQRT(ABS(ALCHT(2)*ALCHP(2)))
c
c
c
      WIN=ABS(IHRMWIN(KW))
      SL=0.15*SQRT(ALM)+31.5/WIN
      SC=MAX(0.7,0.48*SQRT(ABS(ACM))-0.35*SQRT(ABS(ALM))+42./WIN)
      SH=MAX(0.7,0.14*SQRT(ABS(ACM))-0.20*SQRT(ABS(ALM))+21./WIN+0.7)
      SA=0.7
      SB=0.7
c
c
c
      RETURN
      END SUBROUTINE
C
C

