C     Last change: KU 02.07.2021 17:19:36
C
C     Programme zur Berechnung der korrigierten R-Werte für die Rezeptkorrektur
C     mit Hilfe von AKK=alpha (s. R-Schlange Gl. 10.6 und 10.7 in Buch: Theoretische Grundl. .....))
C
C

c   KORRNEU
c
c

C     Last change: U 22.02.2017 17:04:14
C
C     Programme zur Berechnung der korrigierten R-Werte für die Rezeptkorrektur
C     mit Hilfe von AKK=alpha (s. R-Schlange Gl. 10.6 und 10.7 in Buch: Theoretische Grundl. .....))
C
C

C******************************************************
C******************************************************
C******************************************************
      SUBROUTINE MAHLF(IER)
      USE MODRWRZ
      USE MODXYRZ
      USE MODMERZ
      USE MODFARB
      USE MODHILF
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C 
C
C     DEHLF AUS MENVA (COMEN)
C
C
C     AKK =  1   ARITH.
C     AKK =  0   GEOM.
C     AKK = -1  HARM.
C
C
C
C
C     RH       REFLEXIONSWERTE DER HILFSKORREKTUR (GEMESSEN)
C     RN       REFLEXIONSWERTE DER NACHSTELLUNG
C
C
      TAU=1.0D-5
      TOL=1.0D-6
      IER=0
      KM=KMS()
      NWE=NWS()
      NHLF=0
      KAA=KAS()
      NME=UBOUND(D,1)
      DO K=1,NME
         DALT(K)=D(K)
      END DO
      IF(ALLOCATED(AMA)) THEN
         NHLF=UBOUND(AMA,1)
      ENDIF
      IKOR=1
      DO  I=1,NWE
        DO  K=1,NME
          DO  KW=1,KM
             R0(I,KW,K)=0.0
C
C
C
            DO L=1,NF
               RHLF(I,KW,L,K)=0.
            ENDDO           
C
          ENDDO
        ENDDO
      ENDDO
      DO  I=1,NWE
       DO  K=1,JUV
         DO  KW=1,KM
C
C
C
C
C
C         RH=gemessene R-Werte für Hilfskorrektur (Nachstellung) vor Korrektur mit R-Werten der Vorlage
C         RZ=gemessene R-Werte für Hilfskorrektur (Vorlage)
C         RV=gemessene R-Werte der Vorlage
C
C         KORREKTUR DER GEMESSENEN REFLEXIONSWERTE RH DER HILFKORREKTUREN,
C         FALLS VORLAGE FUER HILFKORREKTUR RZ NICHT MIT VORLAGE FUER
C         NACHSTELLUNG RV ÜBEREINSTIMMT

          DO IL=1,IHLF
           RH(I,KW,K,IL)=FRKORRI(AKK,RH(I,KW,K,IL),
     &                           RZ(I,KW,K,IL),RV(I,KW,K),KW)
          ENDDO
C
C         RH=gemessene R-Werte für Hilfskorrektur nach Korrektur mit R-Werten der Vorlage

         ENDDO
       ENDDO
      ENDDO
C
C
      SUA=0.
      DO I=1,NF
         SUA=SUA+ACONA(I)
      END DO
      IF(SUA.LE.TINY(1.)) THEN
        IER=4071
        GOTO 900
      ENDIF
C
      DO I=1,NF
         CONA(I)=ACONA(I)/SUA
      ENDDO

      DO IL=1,IHLF
        SUU=1.E-30
        DO L=1,NF
           SUU=SUU+ACH(L,IL)
        ENDDO
        DO L=1,NF
           CADD(L,IL)=ACH(L,IL)/SUU
        ENDDO
      ENDDO


C
C     BERECHNUNG DER R-WERTE FUER DIE VORGEGEBENEN KONZENTRATIONEN
C
      DO  KW=1,KM

C
C
         DO  K=1,JUN
C
C
C           RY=berechnete R-Werte für Nachstellung
C           RN=gemessene R-Werte für Nachstellung
C           RZ=berechnete R-Werte für Hilfkoorektur
C           RH=gemessene R-Werte für Hilfskorrektur
C
C
            TPS=TOLM-GLZNOG(KW,IRETR(K))
            CALL MALAB(NF,CONA,RY(1,KW,K),KW,K)
C
C
C
C           TEST AUF GUETE DER HILFSKORREKTUREN (RZ=berechnete R-Werte für Hilfskorrektur)
C
C           Schichtdicke der Nachstellung speichern


            DO IL=1,IHLF
             D(K)=DIHLF(K,IL)
             CALL MALRW(NF,CADD(1,IL),RZ(1,KW,K,IL),KW,K)
             DO I=1,NWE

                RR(I,KW,K)=FRKORRI(AKK,RH(I,KW,K,IL),
     &                             RZ(I,KW,K,IL),RY(I,KW,K),KW)
             ENDDO
c
c
c

             GKH=GLZNOG(KW,IRETR(K))
             CALL NOGXX(XYZB(1,1,KW,K),1,RR(1,KW,K),GKH)
             CALL DELABAL(JABST,XYZN(1,1,KW,K),XYZB(1,1,KW,K),
     &                  DE,DL,DC,DH,DA,DB,1)
             IF(DE.GT.DEHLF) THEN
                   IER=-4096
                   IF(IFEHL(IER).NE.0) GOTO 900
             ENDIF
            ENDDO
            D(K)=DALT(K)
C
C
C             ICHI AUS MENUE
C
C             ICHI=0
C             R0 NUR AUS NACHSTELLUNG
C
C
C
C             FUER ICHI=1  
C             MITTELWERT UEBER ALLE DIFFERENZEN
C
C
C
C             FUER ICHI=2
C             LINEARE INTERPOLATION FUER GEWICHT ZUR BERECHNUNG VON DR
C
C
C
C             FUER ICHI=3   
C
C             GLEICHUNGSSYSTEM ZUR BERECHNUNG VON RHLF, SO DASS GILT
C             RHLF*CADD=RZW (MATRIX DER DIFFERENZEN DER R-WERTE) 
C                           FUER JEDE WELLENLAENGE
C
C
C

           DO I=1,NWE
               DR(I)=RSCHL(AKK,RN(I,KW,K),KW)-RSCHL(AKK,RY(I,KW,K),KW)
           ENDDO

           
C
           IF(ICHI.EQ.0.OR.IHLF.EQ.0) GOTO 105
C
C
           IF(ICHI.EQ.3) GOTO 200
C
C
C
C
C
C
             DO I=1,NWE
C
C     
C
              RHA=RSCHL(AKK,RN(I,KW,K),KW)-RSCHL(AKK,RY(I,KW,K),KW)
              SU0=RHA
              RHI=RSCHL(AKK,RN(I,KW,K),KW)-RSCHL(AKK,RV(I,KW,K),KW)
              SU1=RHI
              SU2=RHA*RHI
              SU3=RHI*RHI
              DO IL=1,IHLF
                  RHA=RSCHL(AKK,RH(I,KW,K,IL),KW)
     &               -RSCHL(AKK,RZ(I,KW,K,IL),KW)
C
C
                  RHI=RSCHL(AKK,RH(I,KW,K,IL),KW)
     &               -RSCHL(AKK,RV(I,KW,K),KW)

                 SU0=SU0+RHA                            
                 SU1=SU1+RHI
                 SU2=SU2+RHI*RHA          
                 SU3=SU3+RHI*RHI
              ENDDO
              SDD=(IHLF+1.)*SU3-SU1**2
              IF(ABS(SDD).LT.1.E-4.OR.ICHI.EQ.1) THEN
                 DR(I)=SU0/(IHLF+1.)
              ELSE
                 DR(I)=(SU0*SU3-SU1*SU2)/SDD
              ENDIF
             ENDDO
C
C
C 
C 
C
C
C 
            GOTO 105
C 
C  
C
 200        IF(IHLF.GT.0) THEN
C
C
              MDB=UBOUND(RZW,1)
C
C
C   
              DO IL=1,IHLF
                DO I=1,NWE
                  RZW(IL,I)=RSCHL(AKK,RH(I,KW,K,IL),KW)
     &                     -RSCHL(AKK,RZ(I,KW,K,IL),KW)-DR(I)
                ENDDO
              ENDDO
C 
C
C             Annahme: R(Messung)-R(Rechnung) ist unabhängig von der Schichtdicke
C
C
               DO I=1,NWE
                DO  IL=1,IHLF
                  DO L=1,NF
C                    AMA(IL,L)=(CADD(L,IL)*DIHLF(K,IL)-CONA(L)*DALT(K))
                    AMA(IL,L)=(CADD(L,IL)-CONA(L))
                  ENDDO
                ENDDO
               ENDDO
C
C
C               GLEICHUNGSSYSTEM LOESEN
C
C
                CALL HFTI(AMA,NHLF,IHLF,NF,RZW,MDB,NWE,
     &                   TAU,KRANK,RNORM,PIV,PIW,IP)
C
C
C    
C
C               RHLF = Matrixelemente für zusätzliche Korrektur
C
C
C
               DO I=1,NWE
                 DO L=1,NF
                   RHLF(I,KW,L,K)=RZW(L,I)
                 ENDDO
               ENDDO
C
C
C
C
            ENDIF
C
C 
 105        DO I=1,NWE
                R0(I,KW,K)=DR(I)
            ENDDO
C
C        K
         ENDDO
C
C     KW
      ENDDO     
C
C
C
C
C
 900  RETURN
      END 
C
C 

C
C
C******************************************************
      SUBROUTINE MAKOR(NN,C,RRR,KW,K)
      USE MODRWRZ
      USE MODMERZ
      USE MODFARB
      USE MODGKWR,ONLY:TOL
C
C
C     Neue R-Werte für Farbrezeptkorrektur berechnen, falls IKOR<> 0
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      DIMENSION RRR(*),C(*)
      IF(IKOR.EQ.0) THEN
C
C        Rezeptrechnung ohne R0 und RHLF
C
         RETURN
      ENDIF
      NWE=NWS()
      DO 10 I=1,NWE
      SUU=0.
      DO 20 L=1,NN
C      SUU=SUU+RHLF(I,KW,L,K)*(D(K)*C(L)-DALT(K)*CONA(L))
       SUU=SUU+RHLF(I,KW,L,K)*(C(L)-CONA(L))
  20  CONTINUE
      RSU(I,KW,K)=SUU+R0(I,KW,K)
C
C      R-Schlange(neu)
C
C
C
       RZWI=RSCHL(AKK,RRR(I),KW)
       RHHH=RZWI+RSU(I,KW,K)
C
C
C
C      RRR-neu
C
       RRR(I)=RHHH
C
  10  CONTINUE
      DO I=1,NWE
       RRR(I)=RRUECK(AKK,RRR(I),KW)
      END DO
      RETURN
      END 

      SUBROUTINE MALKO(RRR,RDD,L,KW,K)
      USE MODRWRZ
      USE MODMERZ
      USE MODFARB
C
C     Neue Ableitungen DR/DC für Farbrezeptkorrektur berechnen, falls IKOR<> 0
C
C

C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      DIMENSION RDD(*),RRR(*)
      NWE=NWS()
      IF(IKOR.EQ.0) THEN
C
C        Rezeptrechnung ohne RHLF
C
         RETURN
      ENDIF
      DO 10 I=1,NWE
c
c     R-Schlange(neu)
C
      RHHH=RSCHL(AKK,RRR(I),KW)
C
C     DR-neu/dR-Schlange(neu)
C
      DRN=DRDRSCH(AKK,RHHH,KW)
C
C
C     R-Schlange(alt)
C
      RHHH=RHHH-RSU(I,KW,K)
C
C     DR-alt/DR-Schlange(alt)
C
C
      DRA=DRDRSCH(AKK,RHHH,KW)

c     Ableitung dR-neu/DC
C
C      RDD(I)=DRN*(RDD(I)/DRA+D(K)*RHLF(I,KW,L,K))
      RDD(I)=DRN*(RDD(I)/DRA+RHLF(I,KW,L,K))

  10  CONTINUE  
      RETURN
      END
c
c
c
c
c
C
C
C

