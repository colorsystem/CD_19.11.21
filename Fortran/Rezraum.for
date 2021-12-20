      INCLUDE "GETLWGK.FOR"
      INCLUDE "REZMEGR.FOR"
      INCLUDE "REZLIMT.FOR"
      INCLUDE "REZMGRF.FOR"
      INCLUDE "REZBEEN.FOR"
c      INCLUDE "MSCHLIB.FOR"
c      INCLUDE "FAKER0.FOR"
c      INCLUDE "FAKER1.FOR"
c      INCLUDE "FAKER4.FOR"
c      INCLUDE "FAKER5.FOR"
c      INCLUDE "FRBL1.FOR"
c      INCLUDE "KORRNEU.FOR"
c      INCLUDE "MATLIB.FOR"
c      INCLUDE "MSCHLIB.FOR
C      INCLUDE "REZDRU.FOR"
c      INCLUDE "QLalt.FOR"
c
c

C
C
c      INCLUDE "REZDRU.FOR"


C     Last change: KU 11.04.2021 09:15:22
      SUBROUTINE REZRAUM(OPTL,KWW,NLICHT,KU,NWEL,KML,NME,MNF,KZL,
     &                   FAMNG,REMNG,RCAL,AL,WIU,WIO,WIS,SOKRIT,FEHL)
      USE MOTFEHL

C
C
C     BERECHNUNG VON Rezepten für maximales CHROMA bei vorgegebener Helligkeit
C     für verschiedenen Wingel WIU bis WIO in Schrittet WIS
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=4),DIMENSION(NWEL,KML,NME,*) ::RCAL
      REAL(KIND=4),DIMENSION(MNF,*)::REMNG
      REAL(KIND=4),DIMENSION(*)::FAMNG

      REAL(KIND=4),DIMENSION(16,*)::SOKRIT
      REAL(KIND=4) :: AL,WIU,WIO,WIS

      INTEGER*4 KZL,KWW,NWEL,KU,MNF,OPTL
      TYPE(TYFEH) FEHL
C             
C 
C 
      DLL_EXPORT REZRAUM
C 
C 
      CALL FEHINI
C
C 
C 
C 
c     OPEN(27,FILE='TEXTOUT.TXT')
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
C
C
C
C     WINKEL
C
      KWA=KWW
      IF(KWA.LE.0.OR.KWA.GT.KML) THEN
        KWA=1
      ENDIF
C
C     Lichtart
C
      NLI=NLICHT
      IF(NLI.LE.0) THEN
        NLI=1
      ENDIF  
      KZAL=KZL
C      
C      
C 
C
      CALL RAUREZ(OPTL,KWA,NLI,KU,NWEL,KML,NME,MNF,KZAL,FAMNG,REMNG,
     &            RCAL,AL,WIU,WIO,WIS,SOKRIT,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
      KZL=KZAL
C
C
c
C
 900  CALL GETFEH(FEHL)
c     CLOSE(27)
      RETURN
      END
C 
C
      SUBROUTINE RAUREZ(OPTL,KWA,NLI,KU,NWEL,KML,NME,MNF,KZAL,FAMNG,
     &            REMNG,RCAL,AL,WIU,WIO,WIS,SOKRIT,IER)

      USE MODRWRZ
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODGREN
      USE MODABST
      USE MODRAUM

C 
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C 
C 
C
C
C
      EXTERNAL NLFUNC,NLGRAD
      REAL(KIND=4),DIMENSION(NWEL,KML,NME,*) ::RCAL
      REAL(KIND=4),DIMENSION(MNF,*)::REMNG
      REAL(KIND=4),DIMENSION(*)::FAMNG
      REAL(KIND=4),DIMENSION(16,*)::SOKRIT
      REAL(KIND=4) :: AL,WIU,WIO,WIS

      INTEGER*4 NWEL,MNF,KWA,NLI,KZAL,IBV,OPTL



C
C
      LOGICAL GUTREZPT,GUT
C
      REAL(KIND=8),DIMENSION(MNF) :: ACWWW,ACGUT,ACOL,ACOU,ACGUE,
     &                               X,X0,X1,X2
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) ::GG
      DIMENSION CONZ(1)
      REAL(KIND=8) ::DEVV

      INTEGER M4,ME4,NF4,IPRINT
C 
      DATA TOL/1.E-5/
      DATA RAKL/1.E-10/
      DATA TOO/.1/
C 
C
C
C
C
C
      GGE=0.
      KWB=KWA
      NLJ=NLI
      KUNT=KU
      OPT=OPTL
C
C
      NWE=NWS()
C
c      OPEN(27,FILE='TESTRAU.TXT')
C
C
C
      NNF=MNF
C
c      WRITE(27,*) 'MNF,NF,NNF,NEE,NEQ',MNF,NF,NNF,NEE,NEQ

C
C
      SUMI=0.0
      DO I=1,MNF
        ACONZ(I)=FAMNG(I)/ANORM
        ACGUE(I)=ACONZ(I)
        ACGUT(I)=ACONZ(I)
        ACZWI(I)=FASTART(I)/ANORM
        SUMI=SUMI+ACZWI(I)
      ENDDO

      DO I=1,NNF
         IF(IC(I).EQ.1.OR.IC(I).EQ.3) THEN
            ACOL(I)=CL(I)
         ELSE
            ACOL(I)=0.
         ENDIF
         IF(IC(I).EQ.2.OR.IC(I).EQ.3) THEN
            ACOU(I)=CU(I)
         ELSE
            ACOU(I)=SUMI
         ENDIF
      ENDDO
C
C
C
C
C
      ME4=NEQ+2
      M4=2+NEE
      IF(KGX.GT.0) THEN
         ME4=ME4+1
         M4=M4+1
      ENDIF
C
C
      NF4=NF
      MAXITN=2*ITM
      IPRINT=0
      MM1=M4
C
      ALLOCATE (GG(MM1),STAT=IER)
      IER=IERALC(IER)
      IF(IER.NE.0) THEN
        RETURN
      ENDIF
C 
C     
C
C  OK
C
C
      IBI=IBIN(NF,ICHF)
      IBV=IBWN(NF,ICHF)
      DO I=1,NF
        ACWWW(I)=0.0
      END DO
      IF(IBI.GT.0) THEN
        ACWWW(IBI)=SUMI
      ELSEIF(IBV.GT.0) THEN
        ACWWW(IBV)=SUMI
      ENDIF

C
C
C     PRUEFEN, OB DECKVERMOEGEN BERECHNET WERDEN KANN
C
C
C 
      JO=1
      IF(KGX.EQ.1) THEN
        JO=2
C     
        IF(D(JO).EQ.0.) THEN
           D(JO)=D(1)
        ENDIF
        IF(IBI.NE.0) THEN
          LKS=LK(1)
          LK(1)=IBI
          CONZ(1)=1.
          DO K=1,JO,JO-1
            CALL MALRW(1,CONZ,RR(1,KWB,K),KWB,K)
            GKK=GLZNOG(KWB,IRETR(K))
            CALL NOGXX(XYZ(1,K),NLJ,RR(1,KWB,K),GKK)
          ENDDO
          LK(1)=LKS
          CALL DELABAL(JABST,XYZ(1,1),XYZ(1,JO),
     &                 DEVV,DLD,DC,DH,DAD,DBD,NLJ)
          IF(DEVV.LE.FDE) THEN
            IER=4062
            GOTO 900
          ENDIF
        ENDIF
      ENDIF
C
      GFU=10.0
C
C     
      ALCH(1)=AL
C
      ALCH(3)=WIU
      ALCH(2)=50.0
C
C
C
C     
      KZAE=0
C
C 
  50  IF(ALCH(3).GT.WIO) GOTO 95
C     
C                                    
C                                    
C
C
C                                       
          DO I=1,NF4
            ACGUE(I)=ACGUT(I)
          ENDDO
C
C
C
C
        CALL NCONS(M4,ME4,NF4,ACGUE,ACOL,ACOU,
     &       IPRINT,MAXITN,X0,FVALUE0,NLFUNC,NLGRAD,IFAIL0,IER)
        IF(GUTREZPT(M4,ME4,NF4,X0,FVALUE0,NLFUNC)) THEN
          FVALUE=FVALUE0
          IFAIL=IFAIL0
          DO I=1,NF4
            X(I)=X0(I)
          END DO
        ELSE
C
C
C         weitere Versuche
C
C
          DO I=1,NF4
             ACGUE(I)=ACZWI(I)
          END DO
          CALL NCONS(M4,ME4,NF4,ACGUE,ACOL,ACOU,
     &       IPRINT,MAXITN,X1,FVALUE1,NLFUNC,NLGRAD,IFAIL1,IER)
          DO I=1,NF4
             ACGUE(I)=ACWWW(I)
          END DO
          CALL NCONS(M4,ME4,NF4,ACGUE,ACOL,ACOU,
     &       IPRINT,MAXITN,X2,FVALUE2,NLFUNC,NLGRAD,IFAIL2,IER)

C          ABFRAGEN
C
C

c          GUT= GUTREZPT(M4,ME4,NF4,X0,FVALUE0,NLFUNC)
c          WRITE(27,*) 'ACGUT',(ACGUT(I),I=1,NF4)
c          WRITE(27,*) '0 IFAIL,FVALUE GUT X',IFAIL0,FVALUE0,GUT,
c     &                                       (X0(I),I=1,NF4)
c          GUT= GUTREZPT(M4,ME4,NF4,X1,FVALUE1,NLFUNC)
c          WRITE(27,*) 'ACZWI',(ACZWI(I),I=1,NF4)
c          WRITE(27,*) '1 IFAIL,FVALUE GUT X',IFAIL1,FVALUE1,GUT,
c     &                                       (X1(I),I=1,NF4)
c          GUT= GUTREZPT(M4,ME4,NF4,X2,FVALUE2,NLFUNC)
c          WRITE(27,*) 'ACWWW',(ACWWW(I),I=1,NF4)
c          WRITE(27,*) '2 IFAIL,FVALUE GUT X',IFAIL2,FVALUE2,GUT,
c     &                                       (X2(I),I=1,NF4)
          IFAIL=-1
          FVALUE=0.0
          DO I=1,NF4
            X(I)=X0(I)
          END DO
          IF(GUTREZPT(M4,ME4,NF4,X0,FVALUE0,NLFUNC)) THEN
             FVALUE=FVALUE0
             IFAIL=IFAIL0
          ENDIF
          IF(GUTREZPT(M4,ME4,NF4,X1,FVALUE1,NLFUNC)) THEN
             IF(ABS(FVALUE1).GT.ABS(FVALUE)) THEN
               DO I=1,NF4
                X(I)=X1(I)
               END DO
               FVALUE=FVALUE1
               IFAIL=IFAIL1
             ENDIF
          ENDIF
          IF(GUTREZPT(M4,ME4,NF4,X2,FVALUE2,NLFUNC)) THEN
             IF(ABS(FVALUE2).GT.ABS(FVALUE)) THEN
               DO I=1,NF4
                X(I)=X2(I)
               END DO
               FVALUE=FVALUE2
               IFAIL=IFAIL2
             ENDIF
          ENDIF
        ENDIF

C        WRITE(27,*) 'IFAILO,IER',IFAIL,IER,'L*',ALCH(1),'h',ALCH(3)
C        WRITE(27,*) 'X',(X(I),I=1,NF)


        IF(IER.NE.0) GOTO 900
C
C                      
C                      
C
C
          DO I=1,NF4
            ACONZ(I)=X(I)
            IF(ACONZ(I).LT.RAKL) ACONZ(I)=0.
          ENDDO
C
        DO I=1,NF4
           ACGUT(I)=ACONZ(I)
        ENDDO
     
c       write(fehl.cfeh,3333) nf4,m4,me4,mm1,
c    &                      (X(j),j=1,3)
c3333 format(' nf4,m4,me4,mm1,x1,x2,x3',
c    &         4i4,12f7.3)
c     return
C 
C
C
C
C       REZEPT UND FARBKOORDINATEN NACH REZPT UND REMNG UMSPEICHERN
C
        KZAE=KZAE+1
C
        IF(KZAE.GT.KZAL) THEN
          KZAE=KZAE-1
           IER=4103
           GOTO 900
        ENDIF
        SOKRIT(1,KZAE)=ALCC(1)
        SOKRIT(2,KZAE)=ALCC(2)
        SOKRIT(3,KZAE)=ALCC(3)
        SOKRIT(4,KZAE)=ALCH(1)
        SOKRIT(5,KZAE)=ALCH(2)
        SOKRIT(6,KZAE)=ALCH(3)
        SOKRIT(7,KZAE)=DVV
        SOKRIT(8,KZAE)=IFAIL
        DO I=1,MNF
          REMNG(I,KZAE)=ACONZ(I)
        ENDDO
        DO K=1,JO
          DO I=1,NWE
            RCAL(I,KWB,K,KZAE)=RR(I,KWB,K)
          END DO
        END DO
C
C
  90    ALCH(3)=ALCH(3)+WIS
        GOTO 50
C
  95  KZAL=KZAE
C
C
C
C     Nachiteration mit Nachfolger
C
C
      ALCH(3)=WIU
      DO J=KZAL,1,-1
C
C
C
C         Suche gute Lösung rechts
C
C
          DO K=1,NF4
            X0(K)=REMNG(K,J)
          END DO
          GUT=GUTREZPT(M4,ME4,NF4,X0,FVALUE0,NLFUNC)
          JMAX=0
          DO I=J+1,KZAL
              DO K=1,NF4
                X(K)=REMNG(K,I)
              END DO
              ALCH(3)=SOKRIT(6,I)
              IF(GUTREZPT(M4,ME4,NF4,X,FVALUE,NLFUNC)) THEN
                JMAX=I
C                WRITE(27,*) 'J JMAX 0',J,JMAX
                EXIT
              ENDIF
          END DO
          IF(I.GT.KZAL) THEN
C
C           weiter suchen
            DO I=1,J-1
              DO K=1,NF4
                X(K)=REMNG(K,I)
              END DO
              ALCH(3)=SOKRIT(6,I)
              IF(GUTREZPT(M4,ME4,NF4,X,FVALUE,NLFUNC)) THEN
                JMAX=I
C                WRITE(27,*) 'J JMAX 1',J,JMAX
                EXIT
              ENDIF
            END DO
          ENDIF
C
C
C         Nachrechnung
C
C
          IF(JMAX.GT.0) THEN
            ALCH(3)=SOKRIT(6,J)
            DO I=1,NF4
             ACGUE(I)= REMNG(I,JMAX)
            END DO
C            WRITE(27,*) 'L*,h',ALCH(1),ALCH(3)
C            WRITE(27,*) 'REMNG v',(REMNG(i,J),I=1,NF)
C            WRITE(27,*) 'ACGUE',(ACGUE(I),I=1,NF)
C            WRITE(27,*) 'ACZWI',(ACZWI(I),I=1,NF)

C
C            Startwerte sind die Werte der nachfolgenden Rechnung (JMAX)
C
C
            CALL NCONS(M4,ME4,NF4,ACGUE,ACOL,ACOU,
     &         IPRINT,MAXITN,X,FVALUE,NLFUNC,NLGRAD,IFAIL,IER)
C            WRITE(27,*) 'ALCC',ALCC(1),ALCC(2),ALCC(3)
C            WRITE(27,*) 'X',(X(I),I=1,NF)
C
            IF(GUTREZPT(M4,ME4,NF4,X,FVALUE,NLFUNC)) THEN
C
C             Umspeichern
C
C
              IF(.NOT.GUT.OR.FVALUE.LT.FVALUE0) THEN
                SOKRIT(1,J)=ALCC(1)
                SOKRIT(2,J)=ALCC(2)
                SOKRIT(3,J)=ALCC(3)
                SOKRIT(7,J)=DVV
                SOKRIT(8,J)=IFAIL
                DO I=1,MNF
                  REMNG(I,J)=X(I)
                ENDDO
                DO K=1,JO
                  DO I=1,NWE
                    RCAL(I,KWB,K,J)=RR(I,KWB,K)
                  END DO
                END DO
              ENDIF
            ENDIF
          ENDIF
      END DO

C
C
C     Nachiteration, falls Farbwerte nicht erreicht wurden
C
      ALCH(3)=WIU
      DO J=KZAL,1,-1
C
C
C       Suche nach guten Lösungen
C
C
        DO K=1,NF4
          X(K)=REMNG(K,J)
        END DO
        JMIN=0
        JMAX=0
        ALCH(3)=SOKRIT(6,J)
        IF(GUTREZPT(M4,ME4,NF4,X,FVALUE,NLFUNC)) THEN
           SOKRIT(9,J)=0
C           WRITE(27,*) 'J gut',J
        ELSE
           SOKRIT(9,J)=-1
C
C         Suche gute Lösung links
C
C
          DO I=J-1,1,-1
            DO K=1,NF4
              X(K)=REMNG(K,I)
            END DO
            ALCH(3)=SOKRIT(6,I)
            IF(GUTREZPT(M4,ME4,NF4,X,FVALUE,NLFUNC)) THEN
              JMIN=I
C              WRITE(27,*) 'J JMIN 0',J,JMIN
              EXIT
            ENDIF
          END DO
          IF(I.LT.1) THEN
C
C           weiter suchen
            DO I=KZAL,1,-1
              DO K=1,NF4
                X(K)=REMNG(K,I)
              END DO
              ALCH(3)=SOKRIT(6,I)
              IF(GUTREZPT(M4,ME4,NF4,X,FVALUE,NLFUNC)) THEN
                JMIN=I
C              WRITE(27,*) 'J JMIN 1',J,JMIN
                EXIT
              ENDIF
            END DO
          ENDIF
C
C         Suche gute Lösung rechts
C
C
          DO I=J+1,KZAL
              DO K=1,NF4
                X(K)=REMNG(K,I)
              END DO
              ALCH(3)=SOKRIT(6,I)
              IF(GUTREZPT(M4,ME4,NF4,X,FVALUE,NLFUNC)) THEN
                JMAX=I
C                WRITE(27,*) 'J JMAX 0',J,JMAX
                EXIT
              ENDIF
          END DO
          IF(I.GT.KZAL) THEN
C
C           weiter suchen
            DO I=1,J-1
              DO K=1,NF4
                X(K)=REMNG(K,I)
              END DO
              ALCH(3)=SOKRIT(6,I)
              IF(GUTREZPT(M4,ME4,NF4,X,FVALUE,NLFUNC)) THEN
                JMAX=I
C                WRITE(27,*) 'J JMAX 1',J,JMAX
                EXIT
              ENDIF
            END DO
          ENDIF
C
C
C         Nachrechnung
C
C
          IF(JMIN.GT.0.AND.JMAX.GT.0) THEN
            ALCH(3)=SOKRIT(6,J)
C            WRITE(27,*) 'J,JMIN,JMAX',J,JMIN,JMAX
            DO I=1,NF4
             ACZWI(I)=0.5*(REMNG(I,JMIN)+REMNG(I,JMAX))
             ACGUE(I)= REMNG(I,JMAX)
            END DO
C            WRITE(27,*) 'L*,h',ALCH(1),ALCH(3)
C            WRITE(27,*) 'REMNG v',(REMNG(i,J),I=1,NF)
C            WRITE(27,*) 'ACGUE',(ACGUE(I),I=1,NF)
C            WRITE(27,*) 'ACZWI',(ACZWI(I),I=1,NF)

C
C            Startwerte sind die Werte der nachfolgenden Rechnung (JMAX)
C
C
            CALL NCONS(M4,ME4,NF4,ACGUE,ACOL,ACOU,
     &         IPRINT,MAXITN,X,FVALUE,NLFUNC,NLGRAD,IFAIL,IER)
            IF(.NOT.GUTREZPT(M4,ME4,NF4,X,FVALUE,NLFUNC)) THEN
C
C             Startwerte sind die Mittelwerte der benachbarten Rechnungen (JMIN JMAX)
C
                CALL NCONS(M4,ME4,NF4,ACZWI,ACOL,ACOU,
     &            IPRINT,MAXITN,X,FVALUE,NLFUNC,NLGRAD,IFAIL,IER)
            ENDIF
C            WRITE(27,*) 'ALCC',ALCC(1),ALCC(2),ALCC(3)
C            WRITE(27,*) 'X',(X(I),I=1,NF)
C
            IF(GUTREZPT(M4,ME4,NF4,X,FVALUE,NLFUNC)) THEN
C
C             Umspeichern
C
C
              SOKRIT(1,J)=ALCC(1)
              SOKRIT(2,J)=ALCC(2)
              SOKRIT(3,J)=ALCC(3)
              SOKRIT(7,J)=DVV
              SOKRIT(8,J)=IFAIL
              DO I=1,MNF
                REMNG(I,J)=X(I)
              ENDDO
              DO K=1,JO
                DO I=1,NWE
                  RCAL(I,KWB,K,J)=RR(I,KWB,K)
                END DO
              END DO
            ENDIF
          ENDIF
        ENDIF
      END DO
C
C
C

C
      DO J=1,KZAL
        SOKRIT(3,J)=MOD(SOKRIT(3,J),360.0)
        IF(J.EQ.KZAL.AND.ABS(SOKRIT(3,J)).LT.TOL) THEN
          SOKRIT(3,J)=360.0
        ENDIF

        SUMI=0.
        AKOST=0.
        DO I=1,MNF
          REMNG(I,J)=ANORM*REMNG(I,J)
          SUMI=SUMI+REMNG(I,J)
          AKOST=AKOST+REMNG(I,J)*PRE(I)
        ENDDO
C
C       Gesamtpreis
        SOKRIT(10,J)=AKOST
C       Gesamtmenge
        SOKRIT(11,J)=SUMI
      ENDDO
C
C
 900  DEALLOCATE(GG)
C      CLOSE(27)
      RETURN
      END                
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C
C
      SUBROUTINE NLGRAD(M4,ME4,MMAX,NN,F,G,DF,DG,X)
      USE MODRWRZ
      USE MODMERZ
      USE MODGREN
      USE MODRAUM

C
C     
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C
      INTEGER M4,ME4,MMAX
      DIMENSION X(*),DF(*),DG(MMAX,*),G(*)
      DIMENSION CIE(3)
      DIMENSION CZW(3)
      DIMENSION DII(3)
C
C
C
C
C
C 
C
      DIMENSION XYZQ(3),C(NN)
      DIMENSION AQH(3,3)
C 
      DATA DEEL/.0005/,WI180/57.29578/
C 
      DATA EPL/1.D-80/

C 
C 
C 

      CSUM=EPL
      DO I=1,NN
         CSUM=CSUM+X(I)
      ENDDO
      IF(CSUM.LE.2.*EPL) THEN
         CSUM=1.
      ENDIF
      DO I=1,NN
         C(I)=X(I)/CSUM
      ENDDO
C
C 
C 
C 
C 
      JV=KUNT
      JW=KUNT
      KW=KWB
      IF(KGX.EQ.1) THEN
             JW=1
             JV=2
             JO=2
      ENDIF

C 
C 
      DO K=JW,JV
C
C
C     BERECHNUNG DER REFLEXIONSWERTE UND DER ABLEITUNGEN NACH
C     ABSORPTION UND STREUUNG
C
C
C 
C 
C 
         CALL MALAB(NN,C,RR(1,KW,K),KW,K)
C 
      ENDDO    
C 
C 
C 
C 
C 
C 
C 
C 
      NAV=0
C 
C 
C 
      IF(KGX.EQ.0.OR.JO.LT.2) GOTO 50
C 
C 
      NAV=1
C 
C 
      DO K=JW,JV
C
C
C     TRISTIMULUSWERTE                        
C 
C 
         GKK=GLZNOG(KW,IRETR(K))
         CALL NOGXX(XYZ(1,K),NLJ,RR(1,KW,K),GKK)
      ENDDO    
C 
C 
C 
C 
C 
C     TRANSLUZENTE SCHICHT     
C 
C 
      CALL DELABAL(JABST,XYZ(1,1),XYZ(1,JO),DVV,
     &               DII(1),DC,DH,DII(2),DII(3),NLJ)
C 
      DO  K=1,JO,JO-1
        CALL DEMATAL(JABST,XYZ(1,K),AQH,NLJ)
            DO L=1,3
               SUMI=0.
                 DO I=1,3  
                    SUMI=SUMI+AQH(I,L)*DII(I)
                 ENDDO 
               AD(L,KW,K)=GDE*SUMI/(DVV+EPL)
               IF(K.EQ.1) AD(L,KW,K)=-AD(L,KW,K)
            ENDDO
      ENDDO                 
C 
C 
C 
C 
C 
C 
C     TRANSLUZENTE SCHICHT     
C 
      DO L=1,NN
         DG(NAV,L)=0.
      ENDDO
C 
C 
C 
      KW=KWB
      DO K=1,JO,JO-1
          DO L=1,NN
            CALL MALDC(NN,C,RD,L,KW,K)
C
C             
C                
C              
C            DXYZ/DC(K)
C             
C             
C              
             CALL NOGXD(XYZQ,NLJ,RD,0.D0)
C              
C             
C               
C            DDE/DC(K) 
C             
             DO  LJ=1,3  
                DG(NAV,L)=DG(NAV,L)+AD(LJ,KW,K)*XYZQ(LJ)
             ENDDO
          ENDDO
      ENDDO
C       
C
C 
C 
C 
C 
  50  NA=0
      K=KUNT
      KW=KWB
C 
C 
C
C 
C 
C
C
C              TRISTIMULUSWERTE NACHSTELLUNG                       
C
C 
               GKK=GLZNOG(KW,IRETR(K))
               CALL NOGXX(XYZ(1,1),NLJ,RR(1,KW,K),GKK)
C
C
C
C
C
C                DLAB/DXYZ
C
C
C
C                  DL/DXYZ = AQH(1,L)
C
C                  Da*/DXYZ = AQH(2,L)
C
C                  Db*/DXYZ = AQH(3,L)
C
C
C
C
C
                 CALL DEMATAL(JABST,XYZ(1,1),AQH,NLJ)
C
C
C 
C
C 
C 
C 
C     RR           REFLEXIONSWERTE
C 
C 
C 
C 
C 
C 
C 
C 
            DO L=1,NN
               CALL MALDC(NN,C,RD,L,KW,K)
               NA=NAV
C                    
C                
C                    
C               ABLEITUNGEN DR/DC
C                  
C
C
C
C
C
C
C
 
                      CALL NOGXD(XYZQ,NLJ,RD,0.D0)
C
C
C
C
C
C                      DLAB/DC(L)
C
C
C
C
                       DO IV=1,3
                          NA=NA+1
                          SUMI=0.
                          DO  IW=1,3
                              SUMI=SUMI+XYZQ(IW)*AQH(IV,IW)
                          ENDDO        
                          DG(NA,L)=SUMI
                       ENDDO
            ENDDO
C
C           Ableitungren nach Mengen
C
C

            CALL CONMEN(NN,C,CSUM,MMAX,NA,DG)
C
C 
C 
      
C
C
C
      CALL LCHAL(JABST,XYZ,CIE,ALCC,NLJ)
      WAB=CIE(2)**2+CIE(3)**2+DEEL
C                              
      DO I=1,NN 
        CZW(1)=DG(1+NAV,I)
        CZW(2)=WI180*(CIE(2)*DG(3+NAV,I)-CIE(3)*DG(2+NAV,I))/WAB
        CF=-2.*(CIE(2)*DG(2+NAV,I)+CIE(3)*DG(3+NAV,I))
        DF(I)=OPT*GFU*CF
        IF(NAV.EQ.1) THEN
           DG(1,I)=100.*DG(1,I)
        ENDIF
        DG(NAV+1,I)=10.*CZW(1)
        DG(NAV+2,I)=10.*CZW(2)
        DO II=1,M4-2-NAV
           DG(2+II+NAV,I)=10.*AGR(II,I)
        ENDDO
      ENDDO
C
C
      RETURN
      END
C******************************************************************************
C


      SUBROUTINE NLFUNC(M4,ME4,NN,F,G,X)
      USE MODRWRZ
      USE MODMERZ
      USE MODGREN
      USE MODRAUM

C
C     
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C
c      COMMON/CHROMA/ KUNT,KWB,NLJ,NAV,DER,ALCC(3),
C     &               XYZ(3,2),DVV,ALCH(3),ISTA,GFU,NEZ
      INTEGER M4,ME4,MMAX
      DIMENSION X(*),G(*)
      DIMENSION CIE(3)
C
C 
      DIMENSION C(NN)
C 
C 
      DATA EPL/1.D-80/
C 
      CSUM=EPL
      DO I=1,NN
         CSUM=CSUM+X(I)
      ENDDO
      IF(CSUM.LE.2.*EPL) THEN
         CSUM=1.
      ENDIF
      DO I=1,NN
         C(I)=X(I)/CSUM
      ENDDO
C 
      KW=KWB
      NAV=0
C 
  5   JV=KUNT
      JW=KUNT
      IF(KGX.EQ.1) THEN
             JW=1
             JV=2
             JO=2
      ENDIF
C 
C 
      DO  K=JW,JV
C
C
C 
C 
C
C
C        BERECHNUNG DER REFLEXIONSWERTE
C
C
C
C
C
            CALL MALRW(NN,C,RR(1,KW,K),KW,K)
C 
C
C 
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
      IF(KGX.EQ.0.OR.JO.LT.2) GOTO 50
C 
C 
C 
C 
C
C
      DO K=1,JO
C 
C 
          GKK=GLZNOG(KW,IRETR(K))
          CALL NOGXX(XYZ(1,K),NLJ,RR(1,KW,K),GKK)
      ENDDO      
C 
C 
C 
C 
C     TRANSLUZENTE SCHICHT    
C 
C 
           CALL DELABAL(JABST,XYZ(1,1),XYZ(1,JO),DVV,
     &                DL,DC,DH,DA,DB,NLJ)   
      NAV=1
C
C     1. GLEICHNUNG, FALLS DECKVERMOEGEN BERECHNET WERDEN SOLL
C
      G(1)=100.*GDE*(DVV-FDE)

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
C 
  50  GKK=GLZNOG(KW,IRETR(KUNT))

      CALL NOGXX(XYZ(1,1),NLJ,RR(1,KW,KUNT),GKK)

      CALL LCHAL(JABST,XYZ,CIE,ALCC,NLJ)
C    
      F=-OPT*GFU*ALCC(2)**2

C
      G(NAV+1)=10.*(ALCC(1)-ALCH(1))
C    
      G(NAV+2)=10.*(WIDIFF(ALCH(3),ALCC(3)))
C     
C
      DO II=1,M4-2-NAV
         SUMI=-AGR(II,NN+1)
         DO I=1,NN
             SUMI=SUMI+AGR(II,I)*X(I)
         ENDDO
         G(2+II+NAV)=10.*SUMI
      ENDDO
C
C 
      RETURN
      END
C******************************************************************************

      LOGICAL FUNCTION GUTREZPT(M,ME,N,X,FVALUE,FUNCFF)

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      REAL(KIND=8),DIMENSION(*) :: X
      REAL(KIND=8),DIMENSION(M) :: GG
      EXTERNAL FUNCFF
      DATA EPS/1.D-3/
C
C
C
      GUTREZPT=.TRUE.
      CALL FUNCFF(M,ME,N,F,GG,X)
c      WRITE(27,*) 'X',(X(I),I=1,N)
c      WRITE(27,*) 'GG',(GG(I),I=1,M)
      FVALUE=F
      DO I=1,ME
         IF(ABS(GG(I)).GT.EPS) THEN
           GUTREZPT=.FALSE.
           EXIT
         ENDIF
      END DO
      DO I=ME+1,M
        IF(GG(I).LT.-EPS) THEN
           GUTREZPT=.FALSE.
           EXIT
        ENDIF
      ENDDO
c      WRITE(27,*)'GUTREZPT',GUTREZPT
C
C
      RETURN
      END FUNCTION
CC
c
c
c
c
c
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
      SUBROUTINE GRZRAUM(FEHL)
      USE MODGREN
      USE MOTFEHL
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      TYPE(TYFEH) FEHL
C
C
C
C     Es muss NEM=0 (keine zusätzlichen Minimierungsbedingen erlaubt) und NEQ=0 (keine Gleichungen) sein
C
C
C
      DLL_EXPORT GRZRAUM
C
C
      CALL FEHINI
C
C
C     keine Minimierungsbedingungen
C
C
      NEM=0
      RETURN
      END SUBROUTINE
C
C
C******************
C
C
C
      SUBROUTINE OPTRAUM(OPTL,NLICHT,KZL,
     &                   AL,WIU,WIO,WIS,SOKRIT,FEHL)
      USE MOTFEHL
      USE MODMERZ
      

C
C
C     BERECHNUNG VON Rezepten für maximales CHROMA bei vorgegebener Helligkeit
C     für verschiedenen Wingel WIU bis WIO in Schrittet WIS
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      REAL(KIND=4),DIMENSION(16,*)::SOKRIT
      REAL(KIND=4) :: AL,WIU,WIO,WIS

      INTEGER*4 KZL,KWW,NWEL,KU,OPTL
      TYPE(TYFEH) FEHL
C             
C 
C 
      DLL_EXPORT OPTRAUM
C 
C 
      CALL FEHINI
C
C 
C 
C 
c      OPEN(27,FILE='TEXTOUT.TXT')
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
C
C
C
C     WINKEL
C
C
C     Lichtart
C
      NLI=NLICHT
      IF(NLI.LE.0) THEN
        NLI=1
      ENDIF  
      KZAL=KZL
C      
C      
C 
C
      CALL RAUOPT(OPTL,NLI,KZAL,
     &            AL,WIU,WIO,WIS,SOKRIT,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
      KZL=KZAL
C
C
c
C
 900  CALL GETFEH(FEHL)
c     CLOSE(27)
      RETURN
      END
C 
C
      SUBROUTINE RAUOPT(OPTL,NLI,KZAL,
     &                  AL,WIU,WIO,WIS,SOKRIT,IER)

      USE MODRAUM
      USE MODMERZ

C 
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C 
C 
C
C
C
      EXTERNAL OPFUNC,OPGRAD
      REAL(KIND=4),DIMENSION(16,*)::SOKRIT
      REAL(KIND=4) :: AL,WIU,WIO,WIS
      INTEGER*4 NLI,KZAL,IBV,OPTL



C
C
      LOGICAL GUTREZPT,GUT
C
      REAL(KIND=8),DIMENSION(3) :: ACOL,ACOU,ACGUE,ACZWI,X0,X,ACGUT
      REAL(KIND=8) ::DEVV,G(2)

      INTEGER M4,ME4,NF4,IPRINT
C 
      DATA TOL/1.E-5/
      DATA RAKL/1.E-10/
      DATA TOO/.1/
C 
C
C
C
C
C
      GGE=0.
      NLJ=NLI
      OPT=OPTL
C
C
C
c      OPEN(27,FILE='TESTRAU.TXT')
C
C
C
C
c      WRITE(27,*) 'MNF,NF,NNF,NEE,NEQ',MNF,NF,NNF,NEE,NEQ

C
C

C
C
C
C
C
      ME4=2
      M4=2

C
      MAXITN=2*ITM
      IPRINT=0
      MM1=M4
C
C
C

C
C
C
      GFU=10.0
C
C     
      ALCH(1)=AL
C
      ALCH(3)=WIU
      ALCH(2)=10.0
C
C
C
C     
      KZAE=0
      NF4=3
c      WRITE(27,*) 'M4,ME4,NF4,OPT',M4,ME4,NF4,OPT
      DO I=1,NF4
            X0(i)=ALCH(I)
            ACOL(I)=0.01
            ACOU(I)=FAKT(I,NLJ)
      ENDDO
C
C
C 
  50  IF(ALCH(3).GT.WIO) GOTO 95
C     
C                                    
C                                    
C
C
C                                       
          DO I=1,NF4
            ACGUT(I)=X0(I)
            ACGUE(I)=ACGUT(I)/4.0
          ENDDO
C
C
C
C
c        WRITE(27,*) 'ACGUE',ACGUE
        CALL NCONS(M4,ME4,NF4,ACGUE,ACOL,ACOU,
     &       IPRINT,MAXITN,X0,FVALUE0,OPFUNC,OPGRAD,IFAIL0,IER)
         IFAIL=IFAIL0
         CALL OPFUNC(M4,ME4,NF4,F,G,X0)
c        WRITE(27,*) 'F,G',F,G(1),G(2)
c         WRITE(27,*) 'X0',X0(1),X0(2),X0(3)
         
c         WRITE(27,*) 'GUT',GUTREZPT(M4,ME4,NF4,X0,FVALUE0,OPFUNC)
        IF(GUTREZPT(M4,ME4,NF4,X0,FVALUE0,OPFUNC)) THEN
          FVALUE=FVALUE0
c          WRITE(27,*) 'FVALUE',FVALUE
c          DO I=1,3
c             WRITE(27,*) 'X,XL,XU',X0(i),ACOL(I),ACOU(I)
c          END DO
          IFAIL=IFAIL0
          DO I=1,NF4
            X(I)=X0(I)
          END DO
        ENDIF
C
       IF(IER.NE.0) GOTO 900
C
C                      
C                      
C
C
C
C
C
C
        KZAE=KZAE+1
C
        IF(KZAE.GT.KZAL) THEN
          KZAE=KZAE-1
           IER=4103
           GOTO 900
        ENDIF
        SOKRIT(1,KZAE)=ALCC(1)
        SOKRIT(2,KZAE)=ALCC(2)
        SOKRIT(3,KZAE)=ALCC(3)
        SOKRIT(4,KZAE)=ALCH(1)
        SOKRIT(5,KZAE)=ALCH(2)
        SOKRIT(6,KZAE)=ALCH(3)
        SOKRIT(7,KZAE)=0.0
        SOKRIT(8,KZAE)=IFAIL
        SOKRIT(9,KZAE)=X0(1)
        SOKRIT(10,KZAE)=X0(2)
        SOKRIT(11,KZAE)=X0(3)
        

C
C
  90    ALCH(3)=ALCH(3)+WIS
        GOTO 50
C
  95  KZAL=KZAE
C
C

C
 900  CONTINUE
c      CLOSE(27)
      RETURN
      END
C
C
      SUBROUTINE OPGRAD(M4,ME4,MMAX,NN,F,G,DF,DG,X)
      USE MODRAUM
      USE MODMERZ
      

C
C     
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C
      INTEGER M4,ME4,MMAX
      DIMENSION X(*),DF(*),DG(MMAX,*),G(*)
      DIMENSION CZW(3),CIE(3)
      DIMENSION DII(3)
C
C
C
C
C
C 
C
      DIMENSION XYZQ(3)
      DIMENSION AQH(3,3)
C 
      DATA DEEL/.0005/,WI180/57.29578/
C 
      DATA EPL/1.D-80/

C 
C 
C
C
C
C
C
C                DLAB/DXYZ
C
C
C
C                  DL/DXYZ = AQH(1,L)
C
C                  Da*/DXYZ = AQH(2,L)
C
C                  Db*/DXYZ = AQH(3,L)
C
C
C
C
C
                 CALL DEMATAL(JABST,X,AQH,NLJ)
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
      CALL LCHAL(JABST,X,CIE,ALCC,NLJ)
      WAB=CIE(2)**2+CIE(3)**2+DEEL
C                              
      DO I=1,NN 
        CZW(1)=AQH(1,I)
        CZW(2)=WI180*(CIE(2)*AQH(3,I)-CIE(3)*AQH(2,I))/WAB
        CF=-2.*(CIE(2)*AQH(2,I)+CIE(3)*AQH(3,I))
        DF(I)=OPT*GFU*CF
        DG(1,I)=10.*CZW(1)
        DG(2,I)=10.*CZW(2)
      ENDDO
C
C
C
      RETURN
      END
C******************************************************************************
C


      SUBROUTINE OPFUNC(M4,ME4,NN,F,G,X)
      USE MODRAUM
      USE MODMERZ

C
C     
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
      INTEGER M4,ME4,MMAX
      DIMENSION X(*),G(*)
      DIMENSION CIE(3)
C
C 
C
C 
      DATA EPL/1.D-80/
C 
C
      CALL LCHAL(JABST,X,CIE,ALCC,NLJ)
C    
      F=-OPT*GFU*ALCC(2)**2

C
      G(1)=10.*(ALCC(1)-ALCH(1))
C    
      G(2)=10.*(WIDIFF(ALCH(3),ALCC(3)))
C
C
C
C 
      RETURN
      END 
