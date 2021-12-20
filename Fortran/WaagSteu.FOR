C     Last change: KU 30.08.2019 18:11:55
c
      INCLUDE "GETLWGK.FOR"
c      INCLUDE "FAKER5.FOR"
c      INCLUDE "MatLib.for"
C
 
C
C     s.  Waagensteuerung.doc
C     ALPH   Parameter zur Berechnung von R-Schlange (s. Buch S. 148)
C     N      Anzahl Mengen W
C     Q      Anzahl Messungen (normalerweise=1)
C     Gw     Gewichte für Messungen (1...Q)
C     W      zu berechnende Mengen
C     Wu     untere Grenze fürMengen
C     Wo     obere Grenze für Mengen
C     Wi     Mengen (Waageneinstellungen) für Nachstellung
C     KML    Anzahl Winkel
C     NWEL   Anzahl Wellenlängen
C     RS     Remissionswerte der Vorlage
C     RI     Remissionswerte der Nachstellung (1...Q)
C     H      Korrekturmatrix DR/DC
C     RR     berechnete Remissionswerte
C     Mm     Anzahl Nebenbedingunge(Gleichungen + Ungleichungen)
C     Me     Anzahl Gleichungen (A(1..Me,*))
C     Mq     Anzahl Minimierungsbedingungen (A(Mm+1..Mm+Mq,*))
C     A      Matrix für Nebenbedingungen
C     FEHL   Fehlercode
C     

C
C
      Subroutine WAAGREF(ALPH,N,Q,Gw,W,Wu,Wo,WI,KML,NWEL,RS,RI,H,RR,
     &                   Mm,Me,Mq,A,FEHL)
C
C     Berechnung neuer Waageneinstellungen (Mengen) mit Hilfe von Reflexionswerten
C
      USE MODWINK,ONLY:FWI
      USE MOTFEHL
      USE MODGKWR

      IMPLICIT NONE
      TYPE(TYFEH) :: FEHL
      REAL(KIND=4) ::ALPH
      INTEGER(KIND=4) :: N,Q,KML,NWEL,Mm,Me,Mq
      REAL(KIND=4),DIMENSION(Mm+Mq,N+1) :: A
      INTEGER(KIND=4) :: J,I,L,K,KW,KH,IER,MDIB,MDIM,MROWS,
     &                   IFEHL,MODE,ITMAX,IT
      REAL(KIND=8) X(2*(N+Mm)+2),RW(6*N+5*Mm)
      INTEGER(KIND=4) IW(2*(N+Mm))
      REAL(KIND=8),DIMENSION(N+Mm) :: CU,CL
      INTEGER(KIND=4),DIMENSION(N+Mm) :: IP
      INTEGER(KIND=4),DIMENSION(25) :: IOPT
      REAL(KIND=4),DIMENSION(NWEL,KML,N) :: H
      REAL(KIND=8),DIMENSION(NWEL,KML,N) :: HH
      REAL(KIND=4),DIMENSION(NWEL,KML,Q) :: RI
      REAL(KIND=4),DIMENSION(NWEL,KML) :: RS,RR
      REAL(KIND=4),DIMENSION(N,*) :: WI
      REAL(KIND=4),DIMENSION(*) :: Gw,W,Wu,Wo
      REAL(KIND=8) :: SUN,SUG,SUX,TOLI,ANORM,ANALT,EPP,
     &                RNORM,RNORMC,DRDRSCH,RSCH,RSCHL,RUECK
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: RH
      REAL(KIND=8),DIMENSION(N) :: C
      REAL(KIND=8),DIMENSION(N,Q) :: CI
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: AM
      DATA TOLI/1.D-7/,ITMAX/100/,EPP/1.D-30/
      DATA IOPT/9,6,-5,8,99,20*0/

      REAL(KIND=8) ::ALP
C
      DLL_EXPORT WAAGREF
C
      IER=0
      ALP=ALPH
      MDIB=NWEL*KML
      MDIM=Q*MDIB+Mm+Mq
      ALLOCATE(AM(MDIM,N+Mm+1),RH(NWEL),STAT=IER)
      IF(IFEHL(IER).NE.0) THEN
        GOTO 900
      ENDIF
C
      SUG=0.
      DO J=1,Q
         SUG=SUG+Gw(J)
      END DO
C
C
C
C
C
C     Konzentrationen für WI
C
C
      DO J=1,Q
        SUN=EPP
        DO I=1,N
          SUN=SUN+WI(I,J)
        END DO
        DO I=1,N
          CI(I,J)=WI(I,J)/SUN
        END DO
      END DO
C
C
C     STARTWERTE FÜR W
C
C
      DO I=1,N
         SUN=EPP
         DO J=1,Q
            SUN=SUN+WI(I,J)
         END DO
         W(I)=MAX(SUN/Q,0.01)
      END DO
C
      ANORM=1.D30
      ANALT=0.
C
C
      DO KW=1,KML
         DO L=1,NWEL
           DO I=1,N
              HH(L,KW,I)=H(L,KW,I)
           END DO
          END DO
      END DO

C
C
C
C      HH mit Ableitungen multiplizieren
C
C

C
      DO  KW=1,KML
       DO L=1,NWEL
        DO I=1,N
         RSCH=RSCHL(ALP,DBLE(RS(L,KW)),KW)
         HH(L,KW,I)=HH(L,KW,I)*DRDRSCH(ALP,RSCH,KW)
        END DO
       END DO
      ENDDO

C
C
C     SCHLEIFE BIS KEINE ÄNDERUNG DER FARBABWEICHUNG FESTSTELLBAR
C
C
C
      DO IT=1,ITMAX
         ANALT=ANORM
         ANORM=0.

C
C
C
C        GRENZEN FÜR W(I)
C
C
         DO I=1,N
            CU(I)=Wo(I)
            CL(I)=Wu(I)
            IP(I)=3
         END DO
C
C
C
C        LINEARE Nebenbedingungen
C
C

         DO K=1,Mm
            DO I=1,N
               AM(K,I)=A(K,I)
            END DO
C
C
C           Ungleichungen
C           SUMME(A(I) < A(N+1)
C
C
            CU(N+K)=A(K,N+1)
            CL(N+K)=0.0
            IP(N+K)=2
C
C
C           Gleichungen
C
C
            IF(K.LE.Me) THEN
              CL(N+K)=CU(N+K)
              IP(N+K)=3
            ENDIF
         END DO
C
C
C
C
C        Minimierungsbedingungen
C
C
         DO K=Mm+1,Mm+Mq
            DO I=1,N+1
               AM(K,I)=A(K,I)
            END DO
         END DO
C      
C
C
C        Konzentrationen aus W(I)
C
C
C
         SUN=EPP
         DO I=1,N
             SUN=SUN+W(I)
         END DO
         DO I=1,N
          C(I)=W(I)/SUN
         END DO

C
C
C
C        Schleife über alle Messungen
C
C
         DO J=1,Q
C
C
C
             DO KW=1,KML
               KH=Mm+Mq+NWEL*(J-1)*KML+(KW-1)*NWEL
               DO L=1,NWEL
                  SUX=0.
                  DO I=1,N
                     SUX=SUX+HH(L,KW,I)*(C(I)-CI(I,J))
                  END DO
                  RH(L)=RI(L,KW,J)+SUX
                   AM(KH+L,N+1)=FWI(KW)*Gw(J)*(RS(L,KW)-RH(L))
                   ANORM=ANORM+AM(KH+L,N+1)**2
                  DO I=1,N
                     AM(KH+L,I)=FWI(KW)*Gw(J)*HH(L,KW,I)
                  END DO
               END DO

C              DAM/DC =====> DAM/DW
C
               CALL CONMEN(N,C,SUN,MDIM,NWEL,AM(KH+1,1))
C
C
C              SUMME(AM(KH+L,I)*W(I)alt)=0. (homogene Funktion 0.ten Grades)
C              deshalb
C              AM*(W(I)neu-W(I)alt)=AM*W(I)neu
C
C
C
C
             END DO
         ENDDO
C
C
C
C        Prüfen, ob Fehlerquadratsumme stabil
C
C
C
         IF(ABS(ANORM-ANALT).LT.TOLI) THEN
            EXIT
         ENDIF
C
C
C
C        Neue Berechnung von W(I)
C
C
C
         MROWS=MDIM-Mm
         CALL SBOCLS(AM,MDIM,Mm,MROWS,N,CL,CU,IP,IOPT,
     &                     X,RNORMC,RNORM,MODE,RW,IW)
         IF(MODE.LT.0) then
           IER=4000
           GOTO 900
         ENDIF
          
         DO I=1,N
            W(I)=X(I)
         ENDDO
      ENDDO
C
C
C
C
C     R-Werte (berechnet)
C
      DO KW=1,KML
         DO L=1,NWEL
            RR(L,KW)=0.
         END DO
      END DO
      SUN=EPP
      DO I=1,N
         SUN=SUN+W(I)
      END DO
      DO I=1,N
        C(I)=W(I)/SUN
      ENDDO
      DO KW=1,KML
        DO L=1,NWEL
          RUECK=0.0
          DO J=1,Q
            SUX=0.
            DO I=1,N
              SUX=SUX+HH(L,KW,I)*(C(I)-CI(I,J))
            END DO
            RUECK=RUECK+GW(J)*(RI(L,KW,J)+SUX)
          ENDDO
          RUECK=RUECK/SUG
          RR(L,KW)=RUECK
        END DO
      END DO
C
C
C
C
C

C
C
C
C
  900 IF(IFEHL(IER).NE.0) THEN
        GOTO 999
      ENDIF
 999  CALL GETFEH(FEHL)
      DEALLOCATE(AM,RH, STAT=IER)
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
      Subroutine WAAGLAB(JABST,ALPH,N,Q,Gw,W,Wu,Wo,WI,KML,NWEL,
     &                   RS,RI,H,RR,Mm,Me,Mq,A,FEHL)
C
C     Berechnung neuer Waageneinstellungen mit Hilfe von Farbkoordinaten
C
      USE MODWINK,ONLY:FWI
      USE MOTFEHL
      USE MODGKWR
      USE MODFAKT

      IMPLICIT NONE
      TYPE(TYFEH) :: FEHL
      REAL(KIND=4) ::ALPH
      INTEGER(KIND=4) :: N,Q,KML,NWEL,Mm,Me,Mq,JABST
      REAL(KIND=4),DIMENSION(Mm+Mq,N+1) :: A
      INTEGER(KIND=4) ::J,I,L,K,KW,KH,IRETR,TESTFDM,IER,MDIB,MDIM,MROWS,
     &                  IFEHL,MODE,ITMAX,IT,NLZ,NLS,NL,KK
      REAL(KIND=8) X(2*(N+Mm)+2),RW(6*N+5*Mm)
      INTEGER(KIND=4) IW(2*(N+Mm))
      REAL(KIND=8),DIMENSION(N+Mm) :: CU,CL
      INTEGER(KIND=4),DIMENSION(N+Mm) :: IP
      INTEGER(KIND=4),DIMENSION(25) :: IOPT
      REAL(KIND=4),DIMENSION(NWEL,KML,N) :: H
      REAL(KIND=8),DIMENSION(NWEL,KML,N) :: HH
      REAL(KIND=4),DIMENSION(NWEL,KML,Q) :: RI
      REAL(KIND=4),DIMENSION(NWEL,KML) :: RS,RR
      REAL(KIND=4),DIMENSION(N,*) :: WI
      REAL(KIND=4),DIMENSION(*) :: Gw,W,Wu,Wo
      REAL(KIND=8) :: SUN,SUG,GK1,GKK,GLZNOG,SUX,TOLI,ANORM,ANALT,EPP,
     &                RNORM,RNORMC,DRDRSCH,RSCH,RSCHL,RUECK
      REAL(KIND=8),DIMENSION(3) :: ALCH,XYZH,XYZ,ALAB
      REAL(KIND=8),DIMENSION(3,3) :: AQH
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)::XYZS,ALABS
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: RH
      REAL(KIND=8),DIMENSION(N) :: C
      REAL(KIND=8),DIMENSION(N,Q) :: CI
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: AM
      DATA TOLI/1.D-7/,ITMAX/100/,EPP/1.D-30/,IRETR/0/,GK1/0.0/
      DATA IOPT/9,6,-5,8,99,20*0/

      REAL(KIND=8) ::ALP
C
      DLL_EXPORT WAAGLAB
C
      IER=0
C
C     PARAMETER FÜR DIN 6176
C
      AKCH=1.0
      AKE=1.0
      ALP=ALPH
      NLZ=NLS()
      MDIB=NWEL*KML
      MDIM=3*Q*NLZ*KML+Mm+Mq
      ALLOCATE(AM(MDIM,N+Mm+1),XYZS(3,NLZ,KML),ALABS(3,NLZ,KML),
     &         RH(NWEL),STAT=IER)
      IF(IFEHL(IER).NE.0) THEN
        GOTO 900
      ENDIF
      IF(IFEHL(IER).NE.0) THEN
        GOTO 900
      ENDIF
      IF(IFEHL(TESTFDM (NWEL,KML)).NE.0) THEN
        GOTO 900
      ENDIF
C
      SUG=0.
      DO J=1,Q
         SUG=SUG+Gw(J)
      END DO
C
C
C     SOLL X,Y,Z (XYZS); SOLL L,a,b (ALABS)
C
C
C
      DO KW=1,KML
         GKK=GLZNOG(KW,IRETR)
         DO L=1,NWEL
            RH(L)=RS(L,KW)
         END DO
         DO NL=1,NLZ
            CALL NOGXX(XYZS(1,NL,KW),NL,RH,GKK)
            CALL LCHAL(JABST,XYZS(1,NL,KW),ALABS(1,NL,KW),ALCH,NL)
         END DO
      END DO
C
C
C
C     Konzentrationen für WI
C
C
C
      DO J=1,Q
        SUN=EPP
        DO I=1,N
          SUN=SUN+WI(I,J)
        END DO
        DO I=1,N
          CI(I,J)=WI(I,J)/SUN
        END DO
      END DO
C
C
C     STARTWERTE FÜR W
C
C
      DO I=1,N
         SUN=EPP
         DO J=1,Q
            SUN=SUN+WI(I,J)
         END DO
         W(I)=MAX(SUN/Q,0.01)
      END DO
C
      ANORM=1.D30
      ANALT=0.
C
C
      DO KW=1,KML
         DO L=1,NWEL
           DO I=1,N
              HH(L,KW,I)=H(L,KW,I)
           END DO
          END DO
      END DO

C
C
C
C      HH mit Ableitungen multiplizieren
C
C

C
      DO  KW=1,KML
       DO L=1,NWEL
        DO I=1,N
         RSCH=RSCHL(ALP,DBLE(RS(L,KW)),KW)
         HH(L,KW,I)=HH(L,KW,I)*DRDRSCH(ALP,RSCH,KW)
        END DO
       END DO
      ENDDO

C
C
C     SCHLEIFE BIS KEINE ÄNDERUNG DER FARBABWEICHUNG FESTSTELLBAR
C
C
C
      DO IT=1,ITMAX
         ANALT=ANORM
         ANORM=0.

C
C
C
C        GRENZEN FÜR W(I)
C
C
         DO I=1,N
            CU(I)=Wo(I)
            CL(I)=Wu(I)
            IP(I)=3
         END DO
C
C
C
C        LINEARE Nebenbedingungen
C
C

         DO K=1,Mm
            DO I=1,N
               AM(K,I)=A(K,I)
            END DO
C
C
C           Ungleichungen
C           SUMME(A(I) < A(N+1)
C
C
            CU(N+K)=A(K,N+1)
            CL(N+K)=0.0
            IP(N+K)=2
C
C
C           Gleichungen
C
C
            IF(K.LE.Me) THEN
              CL(N+K)=CU(N+K)
              IP(N+K)=3
            ENDIF
         END DO
C
C
C
C
C        Minimierungsbedingungen
C
C
         DO K=Mm+1,Mm+Mq
            DO I=1,N+1
               AM(K,I)=A(K,I)
            END DO
         END DO
C      
C
C
C        Konzentrationen aus W(I)
C
C
C
         SUN=EPP
         DO I=1,N
             SUN=SUN+W(I)
         END DO
         DO I=1,N
          C(I)=W(I)/SUN
         END DO

C
C
C
C        Schleife über alle Messungen
C
C
         DO J=1,Q
C
C
C
             DO KW=1,KML
               KH=Mm+Mq+(KW-1)*NWEL
               DO L=1,NWEL
                  SUX=0.
                  DO I=1,N
                     SUX=SUX+HH(L,KW,I)*(C(I)-CI(I,J))
                  END DO
                  RH(L)=RI(L,KW,J)+SUX
               END DO
C
C
C
               DO NL=1,NLZ
C
C
C              TRISTIMULUSWERTE X,Y,Z; und L,a,b für J-te Messung
C
C
                  GKK=GLZNOG(KW,IRETR)
                  KH=Mm+Mq+3*(J-1)*NLZ*KML+3*(KW-1)*NLZ+3*(NL-1)
                  CALL NOGXX(XYZ,NL,RH,GKK)
                  CALL LCHAL(JABST,XYZ,ALAB,ALCH,NL)
                  CALL DEMATAL(JABST,XYZ,AQH,NL)
                  DO K=1,3
                     AM(KH+K,N+1)=FWI(KW)*Gw(J)*(ALABS(K,NL,KW)-ALAB(K))
                     ANORM=ANORM+AM(KH+K,N+1)**2
                  END DO
C
C
C
C
C
C
C
C
                  DO I=1,N
                    CALL NOGXD(XYZH,NL,HH(1,KW,I),GK1)
                    DO K=1,3
                     SUX=0.
                     DO KK=1,3
                        SUX=SUX+AQH(K,KK)*XYZH(KK)
                     END DO
C
C
C
C

C
C                    AM = ABLEITUNGEN DLAB/DC(I)
C
C

                     AM(KH+K,I)=FWI(KW)*Gw(J)*SUX
                    ENDDO
                  END DO
C
C
C                 DAM/DC =====> DAM/DW
C
                  CALL CONMEN(N,C,SUN,MDIM,3,AM(KH+1,1))
C
               ENDDO
             END DO
         ENDDO
C
C

C
C        Prüfen, ob Fehlerquadratsumme stabil
C
C
C
         IF(ABS(ANORM-ANALT).LT.TOLI) THEN
            EXIT
         ENDIF
C
C
C
C        Neue Berechnung von W(I)
C
C
C
         MROWS=MDIM-Mm
         CALL SBOCLS(AM,MDIM,Mm,MROWS,N,CL,CU,IP,IOPT,
     &                     X,RNORMC,RNORM,MODE,RW,IW)
         IF(MODE.LT.0) then
           IER=4000
           GOTO 900
         ENDIF
          
         DO I=1,N
            W(I)=X(I)
         ENDDO
      ENDDO
C
C
C
C
C     R-Werte (berechnet)
C
      DO KW=1,KML
         DO L=1,NWEL
            RR(L,KW)=0.
         END DO
      END DO
      SUN=EPP
      DO I=1,N
         SUN=SUN+W(I)
      END DO
      DO I=1,N
        C(I)=W(I)/SUN
      ENDDO
      DO KW=1,KML
        DO L=1,NWEL
          RUECK=0.0
          DO J=1,Q
            SUX=0.
            DO I=1,N
              SUX=SUX+HH(L,KW,I)*(C(I)-CI(I,J))
            END DO
            RUECK=RUECK+GW(J)*(RI(L,KW,J)+SUX)
          ENDDO
          RUECK=RUECK/SUG
          RR(L,KW)=RUECK
        END DO
      END DO
C
C
C
C
C

C
C
C
C
  900 IF(IFEHL(IER).NE.0) THEN
        GOTO 999
      ENDIF
 999  CALL GETFEH(FEHL)
      DEALLOCATE(AM,RH, STAT=IER)
      RETURN
      END


C
C
C     ALPH   Parameter zur Berechnung von R-Schlange (s. Buch S. 148)
C     Gh     Gewicht für alte H-Matrix
C     N      Anzahl Mengen pro Waageneinstellungen
C     P      Anzahl Waageneinstellungen
C     WV     Mengen für Vorlage (kann für jede Waageneinstellung verschieden sein)
C     WN     Mengen für Nachstellung
C     KML    Anzahl Winkel
C     NWEL   Anzahl Wellenlängen
C     RV     Remissionswerte der Vorlage für Waageneinstellung (kann für jede Waageneinstellung verschieden sein)
C     RN     Remissionswerte der Nachstellung für Waageneinstellung
C     H      DR/Dc Matrix der Ableitungen (Eingabe==> alte Matrix; Ausgabe ==> neu berechnete Matrix)
C     FEHL   Fehlercode
C
C
C
C
      SUBROUTINE WAAGMAT(ALPH,Gh,N,P,WV,WN,KML,NWEL,RV,RN,H,FEHL)
      USE MOTFEHL
      USE MODGKWR

      IMPLICIT NONE
      TYPE(TYFEH) :: FEHL
      REAL(KIND=4) ::ALPH,GH
      INTEGER(KIND=4) :: N,P,M,KML,NWEL,J,I,K,KW,MDIB,KRANK,IFEHL,IER
      REAL(KIND=4),DIMENSION(NWEL,KML,N) :: H
      REAL(KIND=4),DIMENSION(NWEL,KML,P) :: RV,RN
      REAL(KIND=8),DIMENSION(NWEL,KML,P) :: RNSCH,RVSCH
      REAL(KIND=4),DIMENSION(N,P) :: WV,WN
      REAL(KIND=8) :: SUN,SUV,RSCHL,RRUECK,RRR,SUU
      REAL(KIND=8),DIMENSION(P+N,NWEL,KML) :: DR
      REAL(KIND=8),DIMENSION(P+N,N) :: DC
      REAL(KIND=8),DIMENSION (N):: HH,GG
      REAL(KIND=8),DIMENSION (NWEL*KML):: RNORM
      INTEGER(KIND=4),DIMENSION (N):: IP

      REAL(KIND=8) ::ALP,TAU
      DATA TAU/1.D-7/
C
C
C
      DLL_EXPORT WAAGMAT
C
C
C
C
      IER=0
      ALP=ALPH
C
C
C     Berechnunmg der Matrix zur Waageneinstellung
C     
C     KONZENTRATIONSDIFFERENZEN
C
C
C
C
      DO J=1,P
         SUN=TAU*TAU
         SUV=TAU*TAU
         DO I=1,N
            SUN=SUN+WN(I,J)
            SUV=SUV+WV(I,J)
         END DO
         DO I=1,N
            DC(J,I)=WN(I,J)/SUN-WV(I,J)/SUV
         END DO
      END DO
      DO J=1,N
         DO I=1,N
            DC(P+J,I)=0.
            IF(I.EQ.J) THEN
               DC(P+J,I)=GH
            ENDIF
         END DO
      END DO
C
C
C    DIFFERENZEN FÜR REMISSIONSWERTE
C

      DO KW=1,KML
        DO I=1,NWEL
          DO J=1,P
             RNSCH(I,KW,J)=RSCHL(ALP,DBLE(RN(I,KW,J)),KW)
             RVSCH(I,KW,J)=RSCHL(ALP,DBLE(RV(I,KW,J)),KW)
             DR(J,I,KW)=RNSCH(I,KW,J)-RVSCH(I,KW,J)
          END DO
          DO J=1,N
             DR(P+J,I,KW)=GH*H(I,KW,J)
          END DO
        END DO
      END DO
C
C
C
      MDIB=NWEL*KML
      M=P+N
      CALL HFTI(DC,M,M,N,DR,M,MDIB,TAU,KRANK,RNORM,HH,GG,IP)
C
C
C     H-Matrix
C
      DO KW=1,KML
        DO I=1,NWEL
          DO K=1,N
             H(I,KW,K)=DR(K,I,KW)
          END DO
        END DO
      END DO
      IF(IFEHL(IER).NE.0) THEN
         CALL GETFEH(FEHL)
      ENDIF
      RETURN
      END
C
C
c
c
c
c
c
      SUBROUTINE WAAGRUCK(ALPH,N,P,WV,WN,KML,NWEL,RV,H,RR,FEHL)
      USE MOTFEHL
      USE MODGKWR
      IMPLICIT NONE
      TYPE(TYFEH) :: FEHL
      REAL(KIND=4) ::ALPH
      REAL(KIND=8),DIMENSION(P,N) :: DC
      REAL(KIND=4),DIMENSION(N,P) :: WV,WN
      INTEGER(KIND=4) :: N,P,KML,NWEL,J,I,K,KW,IFEHL,IER
      REAL(KIND=4),DIMENSION(NWEL,KML,N) :: H
      REAL(KIND=4),DIMENSION(NWEL,KML,P) :: RR,RV
      REAL(KIND=8) :: SUN,SUV,RSCHL,RRUECK,RRR,SUU

      REAL(KIND=8) ::ALP,TAU
      DATA TAU/1.D-7/
C
C
C
      DLL_EXPORT WAAGRUCK
C
C
C
C
      IER=0
      ALP=ALPH
C
C
C
C
      DO J=1,P
         SUN=TAU*TAU
         SUV=TAU*TAU
         DO I=1,N
            SUN=SUN+WN(I,J)
            SUV=SUV+WV(I,J)
         END DO
         DO I=1,N
            DC(J,I)=WN(I,J)/SUN-WV(I,J)/SUV
         END DO
      END DO
      DO KW=1,KML
        DO I=1,NWEL
          DO J=1,P
             SUU=0.0
             DO K=1,N
               SUU=SUU+H(I,KW,K)*DC(J,K)
             END DO
             RRR=RSCHL(ALP,DBLE(RV(I,KW,J)),KW)+SUU
             RRR=RRUECK(ALP,RRR,KW)
             RR(I,KW,J)=RRR
          END DO
        END DO
      END DO
      IF(IFEHL(IER).NE.0) THEN
         CALL GETFEH(FEHL)
      ENDIF
      RETURN
      END
C
C
C
C
C     Farbwerte
C
C
      SUBROUTINE WAAGFARB(JABST,KW,NWEL,RV,RN,DE,DL,DC,DH,DA,DB,FEHL)
      USE MOTFEHL
      USE MODGKWR
C
C     Für Faktoren AKE und AKCH zur Berechnung der DIN6167 Farbdifferenzen (Jabst=1)
      USE MODFAKT

      IMPLICIT NONE
      TYPE(TYFEH) :: FEHL
      REAL(KIND=4) ::ALPH
      INTEGER(KIND=4)::IRETR,L,NWEL,IFEHL,KW,IER,NL,JABST
      REAL(KIND=8),DIMENSION(3) :: XYZV,XYZN
      REAL(KIND=8),DIMENSION(NWEL) :: RH
      REAL(KIND=4)::DE,DL,DC,DH,DA,DB
      REAL(KIND=8)::DE8,DL8,DC8,DH8,DA8,DB8,GKK,GLZNOG

      REAL(KIND=4),DIMENSION(NWEL) :: RV,RN
      DATA IRETR/0/
C
C
C
      DLL_EXPORT WAAGFARB
C
C
C
C
      IER=0
C
C     PARAMETER FÜR DIN 6176
C
      AKCH=1.0
      AKE=1.0
      NL=1
      GKK=GLZNOG(KW,IRETR)
C
C     Vorlage
C

      DO L=1,NWEL
         RH(L)=RV(L)
      END DO
      CALL NOGXX(XYZV,NL,RH,GKK)
C
C     Nachstellung
C

      DO L=1,NWEL
         RH(L)=RN(L)
      END DO

      CALL NOGXX(XYZN,NL,RH,GKK)
      CALL DELABAL(JABST,XYZV,XYZN,DE8,DL8,DC8,DH8,DA8,DB8,NL)
      DE=DE8
      DL=DL8
      DC=DC8
      DH=DH8
      DA=DA8
      DB=DB8
      IF(IFEHL(IER).NE.0) THEN
         CALL GETFEH(FEHL)
      ENDIF
      RETURN
      END SUBROUTINE


