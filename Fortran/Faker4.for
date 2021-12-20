C     Last change: KU 12.11.2021 22:05:44
C
C**********************************************************************
C**********************************************************************
C
C           Klaus Unterforsthuber
C           20.06.2004
C
C**********************************************************************
C**********************************************************************
C**********************************************************************
C**********************************************************************
C
C     Last change:  UFO  18 Jul 104    4:18 pm

C
C
      SUBROUTINE NOGXX(X,NN,R,GK1)
      USE MODILLU

C     23.06.2004
C 
C     BERECHNUNG VON TRISTIMULI-WERTEN X,Y,Z
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(3),R(*)
C
      KENN=1
      GOTO 100
      ENTRY NOGXD(X,NN,R,GK1)
C
C
C     BERECHNUNG FUER ABLEITUNGEN                
C
      KENN=2
C
C
C
C

 100  IF(NLA(NN).LT.0) GOTO 500
C
C 
C     B E R E C H N U N G 
C 
      DO 80 N=1,3 
C 
C 
      SSS= SDOT(NWE,R,1,EILLU(1,N,NN),1)
      IF(KENN.EQ.2) THEN
C
C     ABLEITUNGEN
C
C
             X(N)=SSS
      ELSE
C
C     TRISTIMULUSWERTE
C
C 
             X(N)=SSS - GK1*FTKT(N,NN) 
             IF(X(N).LT.0.D0) X(N)=0.D0
      ENDIF
   80 CONTINUE
      RETURN
C 
C 
  500 DO N=1,3
         X(N)=-100.D0
      ENDDO
      RETURN
C 
      END 
      DOUBLE PRECISION FUNCTION FAKT(I,NN)
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     FAKTOR (X0,Y0,Z0) FUER NORMLICHTART NL(NN) (NL=NUMMER DER NORMLICHTART)
C        I=   1  2  3
C

      IF(NN.LE.NLZ) THEN
         FAKT=FTKT(I,NN)
      ELSE
         FAKT=HUGE(1.D0)
      ENDIF
      RETURN
      END
      INTEGER (KIND=4) FUNCTION NLL(I)
      USE MODILLU
C
C     MOD(NLL,100) NLC,NLA,NLD
C     NLL <100   10 Grad Beobachter
C     NLL >= 100  2 Grad Beobachter
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF(I.LE.NLZ) THEN
         NLL=NLA(I)
      ELSE
         NLL=-1
      ENDIF
      RETURN
      END
      INTEGER (KIND=4) FUNCTION NWS()
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      NWS=NWE
      RETURN
      END
      INTEGER (KIND=4) FUNCTION NLS()
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      NLS=NLZ
      RETURN
      END

C******************************************************************************
C******************************************************
      SUBROUTINE FADIFF(JABST,KWI,IRT,NWEL,RV,RN,DE,DL,DC,DH,DA,DB,
     &                  AMA,N1,N2)
      USE MODILLU
      USE MODGKWR
      USE MODWINK

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RV(NWEL,*),RN(NWEL,*)
      DIMENSION XYZV(3),XYZN(3)                  
      DIMENSION AL(3),AA(3),AB(3),AC(3),AH(3)
      INTEGER(KIND=4) :: JABST,KWI,IRT,NWEL,N1,N2,KWQ
C
      IF(N1.GT.NLZ.OR.N2.GT.NLZ) THEN
         DE=0.
         DL=0.
         DC=0.
         DH=0.
         DA=0.
         DB=0.
         AMA=0.
         RETURN
      ENDIF
C
      KWQ=KWI
C     
C
C
C
C
C
C
C     FARBABSTAENDE FUER  NORMLICHTART N2                     
C

         
      IF(KWQ.LE.0.OR.KWQ.GT.KMS()) THEN
         DO K=1,3
            AL(K)=0.
            AA(K)=0.
            AB(K)=0.
            AC(K)=0.
            AH(K)=0.
         ENDDO
         DEV=0.
         SUU=0.
         AMA=0.  
C
         DO KW=1,KMS()
           GKK=GLZNOG(KW,IRT)
           CALL NOGXX(XYZV,N2,RV(1,KW),GKK)
           CALL NOGXX(XYZN,N2,RN(1,KW),GKK)
           CALL DELABAL(JABST,XYZV,XYZN,DES,DLS,DCS,DHS,DAS,DBS,N2)
C
C
C          METAMERIE
C
C
           CALL METAMAL(JABST,RV(1,KW),RN(1,KW),GKK,AMB,N1,N2)
C
C
           DEW=FWI(KW)*DES
           SUU=SUU+FWI(KW)
C          IF(DEW.GT.DEV) THEN
C              DEV=DEW
C              KWI=KW 
C              DE=DES
C              DL=DLS
C              DC=DCS
C              DH=DHS
C              DA=DAS
C              DB=DBS
C           ENDIF
            AMA=AMA+(FWI(KW)*AMB)**2
            DEV=DEV+(FWI(KW)*DEW)**2
            DLS=FWI(KW)*DLS
            DAS=FWI(KW)*DAS
            DBS=FWI(KW)*DBS
            DCS=FWI(KW)*DCS
            DHS=FWI(KW)*DHS
            AL(1)=AL(1)+DLS**2
            AA(1)=AA(1)+DAS**2
            AB(1)=AB(1)+DBS**2
            AC(1)=AC(1)+DCS**2
            AH(1)=AH(1)+DHS**2
            IF(DLS.GT.0.) THEN
               AL(2)=AL(2)+DLS    
            ELSE
               AL(3)=AL(3)+DLS    
            ENDIF
            IF(DAS.GT.0.) THEN
               AA(2)=AA(2)+DAS    
            ELSE
               AA(3)=AA(3)+DAS    
            ENDIF
            IF(DBS.GT.0.) THEN
               AB(2)=AB(2)+DBS    
            ELSE
               AB(3)=AB(3)+DBS    
            ENDIF
            IF(DCS.GT.0.) THEN
               AC(2)=AC(2)+DCS    
            ELSE
               AC(3)=AC(3)+DCS    
            ENDIF
            IF(DHS.GT.0.) THEN
               AH(2)=AH(2)+DHS    
            ELSE
               AH(3)=AH(3)+DHS    
            ENDIF
         ENDDO

         DE=SQRT(ABS(DEV/SUU))
         AMA=SQRT(AMA/SUU)
         DL=SQRT(AL(1)/SUU)
         IF(ABS(AL(3)).GT.ABS(AL(2))) DL=-DL
         DA=SQRT(AA(1)/SUU)
         IF(ABS(AA(3)).GT.ABS(AA(2))) DA=-DA
         DB=SQRT(ABS(AB(1)/SUU))
         IF(ABS(AB(3)).GT.ABS(AB(2))) DB=-DB
         DC=SQRT(ABS(AC(1)/SUU))
         IF(ABS(AC(3)).GT.ABS(AC(2))) DC=-DC
         DH=SQRT(ABS(AH(1)/SUU))
         IF(ABS(AH(3)).GT.ABS(AH(2))) DH=-DH
      ELSE
           GKK=GLZNOG(KWQ,IRT)
           CALL NOGXX(XYZV,N2,RV(1,KWQ),GKK)
           CALL NOGXX(XYZN,N2,RN(1,KWQ),GKK)
           CALL DELABAL(JABST,XYZV,XYZN,DE,DL,DC,DH,DA,DB,N2)
C
C
C          METAMERIE
C
C
C
           CALL METAMAL(JABST,RV(1,KWI),RN(1,KWI),GKK,AMA,N1,N2)
C
      ENDIF
C
C
C
C
      RETURN
      END   
C
C
C
C
C     FARBDIFFERENZEN
C
      SUBROUTINE DELABAL(JABST,XYZV,XYZN,DE,DL,DC,DH,DA,DB,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZV(*),XYZN(*)

      SELECT CASE (JABST)
        CASE (0)
C
C         DIN 6174
C
          CALL DELAB(XYZV,XYZN,DE,DL,DC,DH,DA,DB,NN)
        CASE (1)
C
C         DIN 6176
C
          CALL DELAB9(XYZV,XYZN,DE,DL,DC,DH,DA,DB,NN)
        CASE (2)
C
C         Alternative CMC-Formel nach Colli and al.
C
          CALL DECMC(XYZV,XYZN,DE,DL,DC,DH,DA,DB,NN)
      END SELECT

      RETURN
      END SUBROUTINE
C
C     MATRIX DLAB/DXYZ
C
      SUBROUTINE DEMATAL(JABST,XYZ,A,J)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8) :: XYZ(*),A(3,3)
      SELECT CASE (JABST)
        CASE (0)
C         DIN 6174
          CALL DEMAT(XYZ,A,J)
        CASE (1)
C         DIN 6176
          CALL DEMAT9(XYZ,A,J)
        CASE(2)
C         CMC Colli
          CALL DEMCMC(XYZ,A,J)
      END SELECT
      RETURN
      END SUBROUTINE
C
C     KOORDINATEN
C
      SUBROUTINE LCHAL(JABST,XYZ,CIE,CIA,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     BERECHNUNG VON L,C,h (CIA)  UND  L,A,B (CIE)  AUS X,Y,Z
  
  
      DIMENSION XYZ(3),CIE(3),CIA(3)
      SELECT CASE (JABST)
        CASE (0)
C         DIN 6174 NACH CIELAB
          CALL LCH(XYZ,CIE,CIA,NN)
        CASE (1)
C         DIN 6176 nach DIN99o
          CALL LCH9(XYZ,CIE,CIA,NN)
        CASE(2)
C         CMC Colli
          CALL CICMC(XYZ,CIE,CIA,NN)
      END SELECT
      RETURN
      END SUBROUTINE
C
C 
C
C     METAMERIE
C
      SUBROUTINE METAMAL(JABST,R,RR,GKK,AMA,N1,N2)
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(*),RR(*)
C
C 
C     METAMERIEINDEX NACH DIN 6172
C 
      SELECT CASE (JABST)
        CASE (0)
C         DIN 6174
          CALL METAM(R,RR,GKK,AMA,N1,N2)
        CASE (1)
C         DIN 6176
          CALL METAM9(R,RR,GKK,AMA,N1,N2)
        CASE (2)
C         CMC Colli
          CALL METCMC(R,RR,GKK,AMA,N1,N2)
      END SELECT
      RETURN
      END SUBROUTINE
C 
C 
C
C 
C 
      SUBROUTINE XYZLCHAL(JABST,XYZ,ALCC,NL,*)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(3),ALCC(3)
      DATA PID/0.0174533/,PI2/1.5707963/
      DATA WUR3/0.20689657/
C
C
C
C     XYZ    GESUCHT X,Y,Z-WERTE
C     ALCC   L*,C*,HGD (IN GRAD) (VORGEG.) 
C     NL     NORMLICHTART
C
C 
C
      SELECT CASE (JABST)
        CASE (0)
C         DIN 6174
          CALL XYZLCH(XYZ,ALCC,NL,*90)
        CASE (1)
C         DIN 6176
          CALL XYZLCH9(XYZ,ALCC,NL,*90)
        CASE(2)
C         nicht verfügbar
          XYZ(1)=-99.0
          XYZ(2)=-99.0
          XYZ(3)=-99.0
      END SELECT
      RETURN
      ENTRY  XYZLABAL(JABST,XYZ,ALCC,NL,*)
C
C
C     XYZ    GESUCHT X,Y,Z-WERTE
C     ALCC   L*,A*,B* (VORGEG.) 
C     NL     NORMLICHTART
C            NON-STANDARD-RETURN FUER NICHT BERECHENBARE WERTE
C
C
C
      SELECT CASE (JABST)
        CASE (0)
C         DIN 6174
          CALL  XYZLAB(XYZ,ALCC,NL,*90)
        CASE (1)
C         DIN 6176
          CALL  XYZLAB9(XYZ,ALCC,NL,*90)
        CASE(2)
C         nicht verfügbar
          XYZ(1)=-99.0
          XYZ(2)=-99.0
          XYZ(3)=-99.0
      END SELECT
      RETURN
  90  RETURN 1
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
C                      +-------+
C                      ! FXYZ  !
C                      +-------+
      SUBROUTINE FXYZ(NR,R,RQ,N,F,GK1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(*),RQ(*),F(4) 
C 
C 
C 
      IF (N.EQ.0) THEN
          CALL NOGXX(F,1,RQ,GK1)
          CALL NOGXX(F(4),2,RQ,GK1) 
      ELSE
          CALL NOGXX(F,1,R,GK1) 
          CALL NOGXX(F(4),2,R,GK1)
      ENDIF 
      RETURN
      END 
 
      SUBROUTINE SIMGOO(XYZT,XYZP,FA,FH,FCC,FCU)
C     BERECHNUNG DES FARBABSTANDES NACH SIMON-GOODWIN 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION  XYZT(3),XYZP(3),T(3,2) 
      DATA RIN/1.D-30/
C 
      SUMT=0. 
      SUMP=0
      DO 10 I=1,3 
      SUMT= SUMT + XYZT(I)
      SUMP= SUMP + XYZP(I)
 10   CONTINUE
      DO 20 I=1,2
      T(I,2)= XYZT(I)/(SUMT+RIN)
      T(I,1)= XYZP(I)/(SUMP+RIN)
 20   CONTINUE
      T(3,2)=XYZT(2)
      T(3,1)=XYZP(2)
      CALL MADAM(T,FA,IER,FCU,FH,FCC)
      IF(IER.NE.0)  GOTO 100
      RETURN
 100  FA=1000.
      FH=1000.
      FCU=1000. 
      FCC=1000. 
      RETURN
      END 

      SUBROUTINE MADAM(T,DA,IER,DCU,DL,DCC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION  T(3,2),  X(19),  Y(27),  FL2(22),  G11(89),  G22(89),
     1  G12(89),  YF(22),  FC(22) 
      INTEGER*4  NR(27,19),NR1(27,10),NR2(27,9)
C 
      EQUIVALENCE (NR,NR1),(NR(1,11),NR2)
      DATA NR1/  7*0,   26,31,2*37, 3*45,         12*58,         89,
     1 1,7,  3*11, 2*22,26,31,2*37, 3*45,         10*58,       3*89,
     2 1,7,12,2*15,2*23,27,32,2*38, 3*46,  3*59,   4*70,  3*58,3*89,
     3 1,7,12,2*15,2*23,27,32,2*38, 3*46,  3*59,   4*70,  3*88,3*85,
     4 2*2,12,2*16,2*24,28,33,2*39, 3*47,  3*60,   4*70,  3*88,3*85,
     5  2*2,3,2*16,2*24,28,33,2*39, 3*47,  3*60,   4*71,  3*88,3*85,
     6        4*3, 3*17,29,34,2*40,2*48,2*54,2*62, 4*71,  3*88,3*85,
     7   4, 8,2*13,3*18,30,35,2*41,2*49,2*55,63,2*66,3*73,3*88,3*85,
     8   4, 8,2*13,3*18,30,35,2*41,2*49,2*55,63,2*66,3*73,2*80,4*82,
     9        4*5,   4*19, 36,2*42,2*50,2*56,64,2*67,3*74,2*80,4*82/
      DATA NR2/4*5,      6*20,    3*43,  2*57,65,2*68, 4*75, 80,4*82,
     2        4*6,      6*20,    3*43,  2*57,65,2*68, 4*75,2*81,3*86, 
     3            10*6,           4*44,   2*61, 2*69,2*76,2*79,2*81,3*86
     4,  0,        9*9,           4*44,   2*61, 2*69,2*76,2*79,2*81,3*86
     5,  0,       11*9,              5*51,      3*72,   2*79,81,4*83, 
     6  0, 5*10,    6*25,            5*51,      3*72,   2*79,81,4*83, 
     7   2*0,4*10,  6*25,            5*51,      3*72,   3*77, 3*83,0, 
     8   2*0,     10*14,               7*52,     4*77,  2*84, 87, 0,
     9    4*0,     8*21,               7*53,      5*78,    2*87,  0 / 
      DATA X / 0.08  ,0.125 ,0.15  ,0.1625,0.175 ,0.1875,0.2125,0.225 , 
     1  0.2375,0.2625,0.275 ,0.2875,0.3125,0.325 ,0.3375,0.35  ,0.375 , 
     2  0.45  ,0.735 /
      DATA Y / 0.0625,0.0875,0.1125,0.125 ,0.1375,0.15  ,0.1625,0.1875, 
     1  0.2125,0.225 ,0.2375,0.25  ,0.2625,0.275 ,0.2875,0.3125,0.325 , 
     2  0.3375,0.35  ,0.3625,0.375 ,0.3875,0.425 ,0.45,0.475,0.65,0.835/
C 
      DATA YF /1.2,1.5,2.,3.,4.,5.,6.,7.,8.,9.,10.,12.,15.,20.,30.,40., 
     150.,60.,70.,80.,90.,102.5/
C 
      DATA FL2/0.,5.1,12.4,23.98,33.04,40.51,47.04,52.8,58.,62.83,67.33,
     1 75.47,86.09,101.07,125.27,145.11,161.83,176.75,190.08,202.16,
     2 213.16,225.56/ 
C 
      DATA FC /.34,.39,.445,.523,.563,.593,.632,.668,.696,.724,.751,.8, 
     1 .872,.975,1.148,1.262,1.39,1.526,1.67,1.8,1.9,1.95/
C 
      DATA G11/529.  ,492.84,365.57,289.  ,282.24,181.71,417.79,315.42, 
     1  135.02, 98.01,139.24,308.35,338.56, 60.06,237.16,282.24,256.  , 
     2  283.59,294.47,240.87, 38.07,111.09,166.41,206.21,100.  , 89.11, 
     3  132.48,160.53,196.  ,256.  , 69.22,104.04,124.99,158.76,190.44, 
     4  223.8 , 59.91, 81.  , 99.2 ,116.64,148.84,184.96,158.51,180.1 , 
     5   45.16, 64.  , 78.15, 97.81,121.  ,152.52, 79.21, 47.75, 28.73, 
     6   70.56, 94.48,114.49,141.61, 30.25, 40.96, 46.65,129.5 , 56.7 , 
     7   68.89, 92.16,109.41, 50.98, 61.78, 77.44, 98.01, 27.35, 37.33, 
     8   57.15, 41.99, 48.58, 58.68, 71.91, 34.46, 38.07, 52.13, 33.06, 
     9   39.88, 27.56, 49.7 , 53.88, 20.98, 32.72, 63.2 , 26.32, 25.  / 
C 
      DATA G22/ 285.61,200.51,60.68,194.88,166.,102.82,153.76,134.56, 
     1 87.61,113.42,137.83,91.01,102.01,67.24,60.84,44.49,35.99,52.13,
     2 121.44,104.45,79.57,89.11,44.09,37.82,73.62,66.26,38.32,32.49, 
     3 31.47,40.7 ,53.58,32.38,27.51,29.27,33.93,62.88,47.61,28.2 ,23.23
     4,23.67,30.14,48.44,71.06,93.7 ,39.69,23.52,20.34,20.07,25.  ,38.56
     5,45.29,46.24,64.8 ,14.82,19.62,31.64,54.76,30.25,17.39,12.96,60.84
     6,12.25,14.21,21.81,40.96,10.3 ,14.36,30.03,54.76,11.66, 9.12,31.81
     7, 8.82,10.63,16.81,30.8 ,23.04,38.44,15.37, 6.97,11.16, 4.97, 8.76
     8,17.39, 4.58, 4.97,13.4 , 6.4 ,10.5 / 
C 
      DATA G12/-174.88,-267.69,-224.82,-443.71,-421.69,-249.74,-114.89, 
     1 -391.64,-185.87,-162.01, 71.71,-81.62,-349.26,-91.27,-40.48, 
     2 -95.82,-126.45,-215.1,-321.61,-294.13,-66.24,71.64,9.86,-74.93,
     3 -140.14,66.89,37.12,-34.94,-92.77,-168.45,60.35,41.79,2.25,-66.09
     4,-116.23,-215.73,61.26,41.9,11.29,-27.2,-83.75,-156.95,-191.27, 
     5 -239.6,54.54,36.67,14.8,-.08,-60.71,-111.08,-88.88,-65.63,-54.31,
     6 4.29,-28.04,-78.97,-138.97,42.56,25.88,13.55,-140.08,4.78,-9.68,
     7 -50.14,-91.31,-1.36,-20.38,-60.57,-94.18,18.93,7.04,-56.95,1.14, 
     8 -11.07,-31.22,-53.72,-37.12,-41.67,-34.07,-3.81,-26.38,-1.35,
     9 -21.99,-33.79,7.64,-11.58,-28.57,7.15,21.21 /
      IER=0
C 
C 
C***********************************************************************
C      DC  UNCORRECTED
C***********************************************************************
      XK1 = T(1,1)
      XK2 = T(1,2)
      YK1 = T(2,1)
      YK2 = T(2,2)
      YG1 = T(3,1)
      YG2 = T(3,2)
      IF(YG2.LE.1.D-6) YG2=1.D-3
      DO 20IS=1,19
      I=IS
      IF (XK2.LE.X(I)) GO TO 30 
   20 CONTINUE
 30   DO 40JS=1,27
      J=JS
      IF (YK2.LE.Y(J)) GO TO 50 
   40 CONTINUE
 50   N = NR(J,I) 
      IF(N.LE.0) THEN
         IER=1
         GOTO 900
      ENDIF
      DXKL = XK1 - XK2
      DYKL = YK1 - YK2
      DCU=100.*SQRT(ABS(DXKL**2*G11(N)+G12(N)*DXKL*DYKL+G22(N)*DYKL**2))
C***********************************************************************
C    DL 
C    DC  CORRECTED
C***********************************************************************
      DO 140IS=2,22 
      I=IS
      IF (YG2.LE.YF(I)) GO TO 160 
  140 CONTINUE
 160  FYC = FC(I-1)*10.**((LOG10(YG2/YF(I-1))*LOG10(FC(I)/FC(I-1))) /
     1 LOG10(YF(I) / YF(I-1)))
      FYL2 = FL2(I-1)+(YG2-YF(I-1))*(FL2(I)-FL2(I-1))/(YF(I)-YF(I-1)) 
      DO 180 IS=2,22
      I=IS
      IF (YG1.LE.YF(I)) GO TO 200 
  180 CONTINUE
 200  FYL1 = FL2(I-1)+(YG1-YF(I-1))*(FL2(I)-FL2(I-1))/(YF(I)-YF(I-1)) 
      DL=-FYL2+FYL1 
      DCC = DCU * FYC 
      DA = SQRT(ABS(DCC**2 + DL**2))
  900 RETURN
      END 
      SUBROUTINE LCH(XYZ,CIE,CIA,NN)  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     BERECHNUNG VON L,C,h (CIA)  UND  L,A,B (CIE)  NACH CIELAB AUS X,Y,Z
  
  
      DIMENSION XYZ(3),CIE(3),CIA(3)
C 
C 
      CALL CILAB(XYZ,CIE,NN)
      CALL CCLCH(CIE,CIA,NN)
      RETURN
      END
      SUBROUTINE CCLCH(CIE,CIA,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     
C     BERECHNUNG VON L,C,HGD AUS L,A,B (CIELAB)
C
      DIMENSION CIE(3),CIA(3)
C      
      CIA(2)=SQRT(ABS(CIE(2)**2 + CIE(3)**2))
      CIA(3)=WNGRD(CIE(2),CIE(3))
C     RLL 
      CIA(1)=CIE(1) 
      RETURN
      END 
  
C
      DOUBLE PRECISION FUNCTION WIDIFFW(WIV,WIN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA W180/180./,W360/360./
      WI=WIN-WIV
      IF(WI.GT.W180) THEN
        WIDIFFW=WI-W360
      ELSEIF(WI.LT.-W180) THEN
        WIDIFFW=W360+WI
      ELSE
        WIDIFFW=WI
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION WIDIFF(WIV,WIN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI180/1.74532925199D-2/
      WIVR=PI180*WIV
      WINR=PI180*WIN
      WIDIFF=WIDIFFR(WIVR,WINR)/PI180
      RETURN
      END
      DOUBLE PRECISION FUNCTION WIDIFFR(WIV,WIN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA WPI/3.14159265359/
      WI=WIN-WIV
      IF(WI.GT.WPI) THEN
        WIDIFFR=WI-2.*WPI
      ELSEIF(WI.LT.-WPI) THEN
        WIDIFFR=2.*WPI+WI
      ELSE
        WIDIFFR=WI
      ENDIF
      RETURN
      END

C 
      DOUBLE PRECISION FUNCTION WIDIFFA(WIV,WIN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA W180/180./,W360/360./
      IF(WIV.GE.0.AND.WIV.LT.W180) THEN
         IF(WIN.GT.WIV+W180.AND.WIN.LE.W360) THEN
            WIDIFFA=WIN-W360-WIV
         ELSE
            WIDIFFA=WIN-WIV
         ENDIF
      ELSE
         IF(WIN.GE.0..AND.WIN.LT.WIV-W180) THEN
            WIDIFFA=WIN+W360-WIV
         ELSE
            WIDIFFA=WIN-WIV
         ENDIF
      ENDIF
      RETURN
      END
      SUBROUTINE STICIE(HGD,CHWERT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*8 CHWERT
      CHARACTER*8 CHH(8)
      DIMENSION GRENZ(8)
      DATA GRENZ/021.801,068.198,111.800,158.199,
     &           201.801,248.198,291.802,338.199/
      DATA CHH/'    R   ','   YR   ','    Y   ','   YG   ',
     &         '    G   ','   BG   ','    B   ','   BR   '/    
      IF(HGD.LE.GRENZ(1).OR.HGD.GT.GRENZ(8)) THEN
             CHWERT=CHH(1) 
      ELSE
         DO I=1,7
            IF(HGD.GT.GRENZ(I).AND.HGD.LE.GRENZ(I+1)) THEN
               CHWERT=CHH(I+1)
               GOTO 90
            ENDIF
         ENDDO
      ENDIF
  90  RETURN
      END
      SUBROUTINE XYZLCH(XYZ,ALCC,NL,*)    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(3),ALCC(3)
      DATA PID/0.0174533/,PI2/1.5707963/
      DATA WUR3/0.20689657/
C
C
C
C     XYZ    GESUCHT X,Y,Z-WERTE
C     ALCC   L*,C*,HGD (IN GRAD) (VORGEG.) 
C     NL     NORMLICHTART
C
      AL=ALCC(1)
      AC=ALCC(2)
      HUE=ALCC(3)
      ALB=HUE*PID
      IF(ALB.GT.2.*PI2) ALB=ALB-4.*PI2
      AS=AC*COS(ALB)
      BS=AC*SIN(ALB)
      GOTO 20
      ENTRY  XYZLAB(XYZ,ALCC,NL,*)    
C
C
C
C     XYZ    GESUCHT X,Y,Z-WERTE
C     ALCC   L*,A*,B* (VORGEG.) 
C     NL     NORMLICHTART
C            NON-STANDARDRETURN FUER NICHT BERECHENBARE WERTE
C
      AL=ALCC(1)
      AS=ALCC(2)
      BS=ALCC(3)
  20  XYZ(2)=(AL+16.)/116.
      XYZ(1)=AS/500.+XYZ(2)
      XYZ(3)=XYZ(2)-BS/200.
      DO 40 I=1,3
      IF(XYZ(I).GT.WUR3) THEN
           XYZ(I)=FAKT(I,NL)*XYZ(I)**3             
      ELSE
           XYZ(I)=FAKT(I,NL)*(XYZ(I)-0.137931)/7.787037
      ENDIF
  40  CONTINUE
      DO 50 I=1,3
C      IF(XYZ(I).GT.FAKT(I,NL)) GOTO 90
      IF(XYZ(I).LT.0.) GOTO 90
  50  CONTINUE
      RETURN
  90  RETURN 1
      END
C 
C******************************************************************************
C 
      SUBROUTINE GELBW(XYZ,GELB,NN) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(*),RXYZ(3)
      DATA TOL/1.D-30/
      CALL URXYZ(XYZ,RXYZ,NN) 
      GELB=100.*(RXYZ(1)-RXYZ(3))/(RXYZ(2)+TOL) 
      RETURN
      END 
      SUBROUTINE YELLOW(XYZ,YELL,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(*)
      DATA TOL/1.D-30/
      YELL=100.*(1.D0-(FAKT(2,NN)*XYZ(3))/(FAKT(3,NN)*XYZ(2)+TOL))
      RETURN
      END 
      SUBROUTINE WEISSG(XYZ,WBE,WST,WBA,WSCIE,WS2,FSTI,NN) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(*),RXYZ(3)
      DATA TOL/1.D-30/
C 
C 
C 
C     RX,RY,RZ BERECHNEN
C 
      CALL URXYZ(XYZ,RXYZ,NN) 
C 
C 
C     WEISSGRAD NACH BERGER 
C 
C 
      WBE=RXYZ(2)+3.*(RXYZ(3)-RXYZ(1))
C 
C 
C 
C     WEISSGRAD NACH STENSBY
C 
      WY=SQRT(ABS(RXYZ(2))) 
      L=10.*WY
      A=17.5*(100.*XYZ(1)/FAKT(1,NN)-XYZ(2))/(WY+TOL) 
      B=7.*(XYZ(2)-100.*XYZ(3)/FAKT(3,NN))/(WY+TOL) 
C 
C 
      WST=L+3.*(A-B)
C 
C 
C 
C     WEISSGRAD BASF (ASTM E 313) Taube Gleichung
C 
      WBA=4.*RXYZ(3)-3.*RXYZ(2) 
C
      CALL CIEWEI(XYZ,WSCIE,FSTI,NN)
      WS2=0.
      RETURN
      END 
      SUBROUTINE CIEWEI(XYZ,WSCIE,FSTI,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(*),XYK(2)
C
C     CIE WEISSGRADFORMEL  (S. BERGER S119,
C         NILS Pauler: Optische Papiereigenschaften)
C
C
      SUU=0.
      DO I=1,3
         SUU=SUU+FAKT(I,NN)
      ENDDO
      X0=FAKT(1,NN)/SUU
      Y0=FAKT(2,NN)/SUU
      CALL XYZKL(XYZ,XYK)          
      WSCIE=XYZ(2)+800.*(X0-XYK(1))+1700.*(Y0-XYK(2))
      FSTI=900.*(X0-XYK(1))-650.*(Y0-XYK(2))
      RETURN
      END
      SUBROUTINE METAM(R,RR,GKK,AMA,N1,N2)
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(*),RR(*)
      DIMENSION XYZV(6),XYZN(6),D1(3),D2(3)
C 
C 
C     METAMERIEINDEX NACH DIN 6172
C 
C 
C 
      IF(N1.GT.NLS().OR.N2.GT.NLS()) THEN
           AMA=0.
           RETURN
      ENDIF
      CALL NOGXX(XYZV,N1,R,GKK)
      CALL NOGXX(XYZN,N1,RR,GKK) 
      CALL DELAB(XYZV,XYZN,DES,D1(1),DCS,DHS,D1(2),D1(3),N1)

      CALL NOGXX(XYZV(4),N2,R,GKK) 
      CALL NOGXX(XYZN(4),N2,RR,GKK)
      CALL DELAB(XYZV(4),XYZN(4),DES,D2(1),DCS,DHS,D2(2),D2(3),N2)
C 
      AMA=SQRT((D1(1)-D2(1))**2+(D1(2)-D2(2))**2+(D1(3)-D2(3))**2)
C 
      RETURN
      END 

      SUBROUTINE METAMA(R,RR,GKK,AMA,N1,N2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(*),RR(*)
      DIMENSION XYZV(6),XYZN(6)       
      DATA TOL/1.D-30/
C 
C 
C
C     METAMERIEINDEX 6172 (ALTE METHODE)
C 
C 
      IF(N1.GT.NLS().OR.N2.GT.NLS()) THEN
           AMA=0.
           RETURN
      ENDIF
C 
C 
      CALL NOGXX(XYZV,N1,R,GKK)
      CALL NOGXX(XYZN,N1,RR,GKK) 
      CALL NOGXX(XYZV(4),N2,R,GKK) 
      CALL NOGXX(XYZN(4),N2,RR,GKK)
C 
C 
      DO 10 I=1,3 
      XYZN(I+3)=XYZN(I+3)*XYZV(I)/(ABS(XYZN(I))+TOL) 
 10   CONTINUE
C 
C 
      CALL DELAB(XYZV(4),XYZN(4),AMA,AAL,AAC,AAH,AAA,AAB,N2)        
      RETURN
      END 
      SUBROUTINE DEANS (XYZT,XYZP,FA,FL,FC,FS,FT,NN)            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DES FARBABSTANDES NACH ADAMS,NICKERSON,STULTZ (AN40) 
C 
      DIMENSION XYZT(1),XYZP(1),VXYZ(3,2),VH(3) 
      CALL VVXYZ(XYZT,VH,NN)
      DO 10 I=1,3 
      VXYZ(I,1)=VH(I) 
 10   CONTINUE
      CALL VVXYZ(XYZP,VH,NN)
      DO 20 I=1,3 
      VXYZ(I,2) = VH(I) 
 20   CONTINUE
      CALL DANS (VXYZ,FA,FL,FC,FS,FT) 
      RETURN
      END 
      SUBROUTINE DANS (V,DE,DEAL,DEAC,DEAS,DEAT)           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION V(3,2)
      A11=40.*(V(1,1)-V(2,1)) 
      A12=40.*(V(1,2)-V(2,2)) 
      A21=16.*(V(2,1)-V(3,1)) 
      A22=16.*(V(2,2)-V(3,2)) 
      DEAL=9.2*(V(2,2)-V(2,1))
      DEAC=SQRT(ABS((A12-A11)**2+(A22-A21)**2)) 
      DEAS=SQRT(ABS(A11**2+A21**2))-SQRT(ABS(A12**2+A22**2))
      DEAT=ABS(DEAC**2-DEAS**2) 
      DEAT=SIGN(SQRT(ABS(DEAT)),A12*A21-A11*A22)
      DE=SQRT(DEAC**2+DEAL**2)
      RETURN
      END 
      SUBROUTINE VVXYZ(XYZ,VXYZ,NN)           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DER VX,VY,VZ-WERTE 
C 
      DIMENSION  XYZ(1),VXYZ(1) 
      DO  10  I=1,3 
      VXYZ(I)= FUV(XYZ(I),I,NN) 
 10   CONTINUE
      RETURN
      END 
      DOUBLE PRECISION FUNCTION FUV (X,I,NN)            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
      DIMENSION  VALT(5)
C 
C 
C 
      IZ = 0
      Y = X 
      A = Y*100./FAKT(I,NN) 
      VALT(1) = SQRT(ABS(Y))
      IF(Y.LT.0.) VALT(1)=0.
 10   IF (IZ.EQ.20) RETURN
      IZ = IZ + 1 
      DO   K = 2,5
      VALT(K) = VALT(K-1) * VALT(1)
      ENDDO
      F=0.0008194 * VALT(5) - 0.020484 * VALT(4) + 0.23352 * VALT(3)
     &-0.22533 * VALT(2) + 1.1913 * VALT(1) - A 
      FS=0.004097 * VALT(4) - 0.081936 * VALT(3) + 0.70056 * VALT(2)
     &-0.45066 * VALT(1) + 1.1913 
      FDFS = F / FS 
      FUV = VALT(1) - FDFS
      IF (ABS(FUV - VALT(1)).LE.0.5D-6) RETURN
      VALT(1) = FUV 
      GO TO 10
      END 
      SUBROUTINE DEFMC(XYZT,XYZP,DE,DL,DCRG,DCYB)            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     **************
C     **************
C 
      DIMENSION  XYZT(1),XYZP(1),PQS(3,2),PQSM(3),DPQS(3),PQSQ(3),YM(4) 
      DIMENSION T(3,2)
      DOUBLE PRECISION  K1,  K2 
      DO   I=1,3
      T(I,1)=XYZT(I)
      T(I,2)=XYZP(I)
      ENDDO
      DO   I=1,2
      PQS(1,I) = 0.724 * T(1,I) + 0.382 * T(2,I) - 0.098 * T(3,I) 
      PQS(2,I) = -0.48 * T(1,I) + 1.37  * T(2,I) + 0.1276* T(3,I) 
      PQS(3,I) = 0.686 * T(3,I)
      ENDDO
      DO  J=1,3
C     Mittelwert P,Q,S
      PQSM(J) = (PQS(J,1) + PQS(J,2)) * 0.5
C     Delta P,Q,S
      DPQS(J) = PQS(J,2) - PQS(J,1) 
      PQSQ(J) = PQSM(J) **2
      ENDDO
C     Mittelwert Y
      YM(1) = (T(2,1) + T(2,2)) * 0.5 
C     Potenzen Y
      DO I=2,4
      YM(I) = YM(I-1) * YM(1)
      ENDDO
C     0.00416**2 =17.3E-6
      AQ = 17.3E-6 * (PQSQ(1) + PQSQ(2)) / (1. + 2.73 * PQSQ(1) * 
     & PQSQ(2) / (PQSQ(1) **2 + PQSQ(2) **2)) 
      BQ = 3.098E-4 * (PQSQ(3) + 0.2015 * YM(2))
C 
C 
C 
C     FMC2
C 
C 
      AF=0. 
      AL=0.279
      K1 = 0.55669 + 0.049434 * YM(1) - 0.82575E-3 * YM(2) + 0.79172E-5 
     & * YM(3) - 0.30087E-7 * YM(4) 
      K2 = 0.17548 + 0.027556 * YM(1) - 0.57262E-3 * YM(2) + 0.63893E-5 
     & * YM(3) - 0.26731E-7 * YM(4) 
C 
C 
C 
C     FMC1
C 
C 
C     K1=1. 
C     K2=1. 
C     AF=0. 
C     AL=0.279
C 
C 
C 
C     FMC-F (FRIELE:"IMPROVMENTS IN COLOUR DIFFERENCE CALCULATIONS" 
C                   J.SOC.DYERS COL.91(1975)S.375 
C 
C 
C 
C 
C     K1=1. 
C     K2=1. 
C     AF=1. 
C     AL=0.08 
C 
C 
C 
C 
C 
      SPQQ = SQRT(ABS(PQSQ(1) + PQSQ(2)))
      DL1 = (PQSM(1) * DPQS(1) + PQSM(2) * DPQS(2)) / SPQQ
      DCYB = PQSM(3) * DL1 / SPQQ - DPQS(3) 
      DCRG = (PQSM(2) * DPQS(1) - PQSM(1) * DPQS(2)) / SPQQ 
      AA=SQRT(ABS(AQ))
      BB=SQRT(ABS(BQ))
      DCRG=K1*DCRG/AA 
      DCYB=K1*DCYB/BB 
      DCQ=(DCRG-AF*DCYB)**2+DCYB**2 
      DL2 = AL * DL1 / AA 
      DL = K2 * DL2 
      DE = SQRT(ABS(DCQ + DL*DL))
      RETURN
      END 
      SUBROUTINE DCIE94(XYZT,XYZP,DE,DL,DC,DH,DA,DB,KNO)
      USE MODFAKT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CIAT(3),CIE(3),CIAP(3)
      DIMENSION XYZT(*),XYZP(*)
C
C         CIE 94
C
         CALL LCH(XYZT,CIE,CIAT,KNO)   
         CALL LCH(XYZP,CIE,CIAP,KNO)   
         CROMA=SQRT(ABS(CIAT(2)*CIAP(2)))
            CALL DELAB(XYZT,XYZP,AMA,ADL,ADC,ADH,ADA,ADB,KNO)     
            SL=1.
            DL=ADL/(AKL94*SL)
            DC=ADC/(AKC94*(1.+0.045*CROMA))
            DH=ADH/(AKH94*(1.+0.015*CROMA))
            DE=SQRT(ABS(DL**2+DC**2+DH**2))
      RETURN
      END  
      SUBROUTINE DULAB(XYZV,XYZN,DE,DL,DC,DH,NN)
      USE MODFAKT
       
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     BERECHNUNG DES FARBABSTANDES UND DER FARBABSSTANDSANTEILE NACH 
C     CMC
C
C
C  
      DIMENSION XYZV(*),XYZN(*),ALAB(3),ALCH(3)  
      EQUIVALENCE (ALCH(1),UL),(ALCH(2),C1),(ALCH(3),H)
      DATA FUM/0.0174533/
      CALL LCH(XYZV,ALAB,ALCH,NN)                
      CALL DELAB(XYZV,XYZN,DEC,DL,DC,DH,DA,DB,NN)
C     
C     XL,XC aus MODUL MODFAKT
C
C     XL Korrektur Helligkeit 
C
C     XC Korrektur Chroma
C
      HPI=H*FUM
      T=TJPC(HPI)
      IF (UL.GE.16.) THEN      
           SL=0.040975*UL/(1.0D0+0.01765*UL) 
      ELSE 
           SL=0.511
      ENDIF
      SC=0.0638*C1/(1.0D0+0.0131*C1)+0.638D0
      F=SQRT(ABS(C1**4/(C1**4+1900.D0)))
      SH=SC*(T*F+1.D0-F)
      DL=DL/(XL*SL) 
      DC=DC/(XC*SC) 
      DH=DH/SH
      DE=SQRT(ABS(DL**2+DC**2+DH**2))
      RETURN
      END 
c
c
c
c
c
c
c
      SUBROUTINE DC2000(XYZT,XYZP,DE,DL,DC,DH,DBL,KNO)
      USE MODFAKT

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CIAT(3),CIAP(3),CIET(3),CIEP(3)
      DIMENSION XYZT(*),XYZP(*)
      DATA PI/3.14159265359/
C
C         CIE 2000
C
         CALL LCH(XYZT,CIET,CIAT,KNO)
         CALL LCH(XYZP,CIEP,CIAP,KNO)
         CHROMA=0.5*(CIAT(2)+CIAP(2))
         HELL=0.5*(CIET(1)+CIEP(1))
         AFAK=(1.+G2000(CHROMA))
         ASTRT=CIET(2)*AFAK
         ASTRP=CIEP(2)*AFAK
C
C        WINKEL IN RADIAN
C
C
         HSTRT=WNKEL(ASTRT,CIET(3))
         HSTRP=WNKEL(ASTRP,CIEP(3))
         CSTRT=SQRT(ASTRT**2+CIET(3)**2)
         CSTRP=SQRT(ASTRP**2+CIEP(3)**2)
         CSTRM=0.5*(CSTRT+CSTRP)
C
C****    DELTA H'
C
         DCSTR=CSTRP-CSTRT
C****
C*****   WINKELDIFFERENZ IN RADIAN
C
C
         DELHSTR=WIDIFFR(HSTRT,HSTRP)
C
C
C
C*****   DELTA H'
C
         DHSTR=2.*SIN(0.5*DELHSTR)*SQRT(CSTRT*CSTRP)
C
C        HELLIGKEITSDIFFERENZ
C
         SL=SL2000(HELL)
         DLSTR=CIEP(1)-CIET(1)
         DL=DLSTR/(AKL2000*SL)
C
C        CHROMADIFFERENZ
C
         SC=1.+0.045*CSTRM
         DC=DCSTR/(AKC2000*SC)
C
C        BUNTTONDIFFERENZ
C
         HM=0.5*(HSTRT+HSTRP)
         SH=1.+0.015*CSTRM*T2000(HM)
         IF(ABS(HSTRT-HSTRP).GT.PI) THEN
            HM=HM+PI
         ENDIF
         DH=DHSTR/(AKH2000*SH)
C
C        BLAU ROTATION
C
         RT=-RC2000(CSTRM)*SIN(2.*DTH2000(HM))
C
C
C        GESAMTFARBABSTAND
C
C
C
         DBL=RT*DC*DH
         DE=SQRT(DL**2+DC**2+DH**2+DBL)

      RETURN
      END  
      DOUBLE PRECISION FUNCTION G2000(CM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      G2000=0.5*(1.-SQRT(CM**7/(25.**7+CM**7)))
      RETURN
      END
      DOUBLE PRECISION FUNCTION SL2000(AL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      AHH=(AL-50.)**2
      SL2000=1.+0.015*(AHH/SQRT(20.+AHH))
      RETURN
      END
      DOUBLE PRECISION FUNCTION T2000(HM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI180/1.74532925199D-2/
C
C
C     HM IN RADIAN
C
C

      HGR=HM/PI180
      W1=PI180*(HGR-30.)
      W2=PI180*(2.*HGR)
      W3=PI180*(3.*HGR+6)
      W4=PI180*(4.*HGR-63)
      T2000=1.-0.17*COS(W1)+0.24*COS(W2)+0.32*COS(W3)-0.2*COS(W4)
      RETURN
      END
      DOUBLE PRECISION FUNCTION DTH2000(HM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI180/1.74532925199D-2/
C      DATA PI/3.14159265359/
C
C     HM UND DTH2000 in RADIAN
C
      HGR=HM/PI180
      DTH2000=PI180*EXP(-((HGR-275.)/25.)**2)*30.
      RETURN
      END
      DOUBLE PRECISION FUNCTION RC2000(CM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      C7=CM**7
      RC2000=2.*SQRT(C7/(C7+25.**7))
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
      SUBROUTINE JPC79(XYZV,XYZN,DE,DL,DC,DH,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZV(1),XYZN(1),A(3,2)
      DIMENSION VV(3) 
C 
C 
C 
C 
C 
      CALL VVXYZ(XYZV,VV,NN)              
      CALL AVXYZ(VV,A(1,1),43.909D0)
      CALL VVXYZ(XYZN,VV,NN)
      CALL AVXYZ(VV,A(1,2),43.909D0)
C 
      DEA=0.
      DO 10 I=1,3 
      DEA=DEA+(A(I,1)-A(I,2))**2
  10  CONTINUE
      DEA=SQRT(ABS(DEA))
C 
C 
C 
C 
      ALB=A(1,1)
      ALP=A(1,2)
      CB=SQRT(ABS(A(2,1)**2+A(3,1)**2))
      CP=SQRT(ABS(A(2,2)**2+A(3,2)**2))
      ALT=0.08195*ALB/(1.+0.01765*ALB)
      CT=0.0638*CB/(1.+0.0131*CB)+0.638 
      PH=WNKEL(A(2,1),A(3,1))
      IF(CB.LT.0.638) THEN           
            T=1.
      ELSE
            T=TJPC(PH)
      ENDIF
      HT=T*CT 
      DC=CP-CB
      DL=ALP-ALB
      DH=ABS(DEA**2-DL**2-DC**2) 
      DH=SQRT(ABS(DH))
      VZ=A(2,2)*A(3,1)-A(2,1)*A(3,2)
       IF(VZ.GT.0.) DH=-DH
      DL=DL/ALT 
      DC=DC/CT
      DH=DH/HT
      DE=SQRT(ABS(DL**2+DC**2+DH**2))
      RETURN
      END
C
C
C
C
      SUBROUTINE METCMC(R,RR,GKK,AMA,N1,N2)
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(*),RR(*)
      DIMENSION XYZV(6),XYZN(6),D1(3),D2(3)
C 
C 
C     METAMERIEINDEX NACH DIN 6172 FÜR CMC-Formel nach COLLI and al.
C 
C 
C 
      IF(N1.GT.NLS().OR.N2.GT.NLS()) THEN
           AMA=0.
           RETURN
      ENDIF
      CALL NOGXX(XYZV,N1,R,GKK)
      CALL NOGXX(XYZN,N1,RR,GKK) 
      CALL DECMC(XYZV,XYZN,DES,D1(1),DCS,DHS,D1(2),D1(3),N1)

      CALL NOGXX(XYZV(4),N2,R,GKK) 
      CALL NOGXX(XYZN(4),N2,RR,GKK)
      CALL DECMC(XYZV(4),XYZN(4),DES,D2(1),DCS,DHS,D2(2),D2(3),N2)

C 
      AMA=SQRT((D1(1)-D2(1))**2+(D1(2)-D2(2))**2+(D1(3)-D2(3))**2)
C 
      RETURN
      END

      SUBROUTINE DECMC(XYZV,XYZN,DE,DL,DC,DH,DA,DB,NN)
      USE MODFAKT

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZV(*),XYZN(*)
C
C     XL,XC aus MODUL MODFAKT
C
C
      CALL DCMC1(XYZV,XYZN,DE,DL,DC,DH,DA,DB,NN,XL,XC)
      RETURN
      END
      SUBROUTINE DCMC1(XYZV,XYZN,DE,DL,DC,DH,DA,DB,NN,XL,XC)  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     BERECHNUNG DES FARBABSTANDES UND DER FARBABSSTANDSANTEILE NACH 
C     COLLI AND AL
C     Aldo Colli, E Gremmo, P.Moniga: Pitture e Vernici April 1989 VOL.: 4 (page 922 ?)
C
C
C  
      DIMENSION XYZV(*),XYZN(*),ALAB(3),ALCH(3)  
      DIMENSION CLAB(3,2)        
      CALL LCH(XYZV,ALAB,ALCH,NN)                
      CALL CMCLAB(ALAB,CLAB(1,1),ALCH,NN,XL,XC)
      CALL LCH(XYZN,ALAB,ALCH,NN)
      CALL CMCLAB(ALAB,CLAB(1,2),ALCH,NN,XL,XC)
      CALL CILCH(CLAB,DE,DL,DC,DH,DA,DB)
      RETURN
      END 
      SUBROUTINE CICMC(XYZ,CLAB,CMC,NN)
      USE MODFAKT
C
C     BERECHNUNG VON L,C,H (CIA)  UND  L,A,B (CIE)  NACH Colli and al. AUS X,Y,Z
C
C
             
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(*),CLAB(*),CMC(*)
      DIMENSION ALAB(3)
C
      CALL LCH(XYZ,ALAB,CLAB,NN)
      CALL CMCLAB(ALAB,CLAB,CMC,NN,XL,XC)
      RETURN
      END
C
C
C
C
C
      SUBROUTINE CMCLAB(ALAB,CLAB,CMC,NN,XL,XC)
      USE MODCMC
C
C
C     Aldo Colli, E Gremmo, P.Moniga: Pitture e Vernici April 1989 VOL.: 4 (page 19 ?)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CMC(*),ALAB(*),ALCH(3),CLAB(*)
      CALL CCLCH(ALAB,ALCH,NN)
      U=LOG(1.0+ALAB(1))
      CMC(1)=AL(1)*ALAB(1)+(AL(2)+(AL(3)+(AL(4)+AL(5)*U)*U)*U)*U
      CMC(1)=CMC(1)/XL
      U=LOG(1.+ALCH(2))
      CMC(2)=AS(1)*ALCH(2)+(AS(2)+(AS(3)+AS(4)*U)*U)*U
      CMC(2)=CMC(2)/XC
C
C     Hue wird in Radiant umgerechnet
C
      U=ALCH(3)*FPI
      SUU=AH+U
      DO I=1,4
         UI=U*I
         SUU=SUU+AA(I)*SIN(UI)+BB(I)*COS(UI)
      ENDDO
      CMC(3)=SUU
      CLAB(1)=CMC(1)
      CLAB(2)=CMC(2)*COS(CMC(3))
      CLAB(3)=CMC(2)*SIN(CMC(3))
      CMC(3)=SUU/FPI
      RETURN
      END

C
C
C
C     CMC nach Colli and al.
C
C
C
C
      SUBROUTINE DEMCMC(XYZ,A,J)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8) :: XYZ(*),ALAB(3),A(3,3),B(3,3)
C
C
C     DLAB/DXYZ
C
      CALL DEMAT(XYZ,B,J)
C
C     L*,a*,b*
C
C
      CALL CILAB(XYZ,ALAB,J)
C
C     DLABCMC/DLAB
C
C
      CALL DEMCOL(ALAB,A)
C
C
      A=MATMUL(A,B)
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE DEMCOL(ALAB,A)
      USE MODFAKT
      USE MODCMC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ALAB(*),A(3,*),ALCH(3)
      CALL CCLCH(ALAB,ALCH,NN)
      UL=LOG(1.+ALAB(1))
      S=ALCH(2)
      US=LOG(1.0+S)
C
C     Hue wird in Radiant umgerechnet
C
      H=ALCH(3)*FPI
      HMG=AH+H
      SUU=1.0
      DO I=1,4
         UI=H*I
         HMG=HMG+AA(I)*SIN(UI)+BB(I)*COS(UI)
         SUU=SUU+1.0*I*(AA(I)*COS(UI)-BB(I)*SIN(UI))
      ENDDO
      DHMGDH=SUU
      SMG=(AS(1)*S+(AS(2)+(AS(3)+AS(4)*US)*US)*US)/XC
      SN=SIN(HMG)
      CS=COS(HMG)
      SSIN=SMG*SN
      SCOS=SMG*CS
      DUSDS=1.0/(1.0+S)
      DSMGDS=(AS(1)+(AS(2)+(2.0*AS(3)+3.0*AS(4)*US)*US)*DUSDS)/XC
      S=S+ECMC

c
C
C     ABLEITUNGEN DLABCMC/DLAB
C     J = NUMMER DER NORMLICHTART
C
C
C
C     DLABCMC/DL*
C 
      DULDL=1.0/(1.0+ALAB(1))
      SUU=AL(1)+(AL(2)+(2.0*AL(3)+(3.0*AL(4)+4.0*AL(5)*UL)*UL)*UL)*DULDL
      A(1,1)=SUU/XL
      A(2,1)=0.0
      A(3,1)=0.0
C
C     DLABCMC/Da*
C

      A(1,2)=0.0
      A(2,2)=+SSIN*DHMGDH*ALAB(3)/S**2+CS*DSMGDS*ALAB(2)/S
      A(3,2)=-SCOS*DHMGDH*ALAB(3)/S**2+SN*DSMGDS*ALAB(2)/S
C
C     DLABCMC/Db*
C
      A(1,3)=0.0
      A(2,3)=-SSIN*DHMGDH*ALAB(2)/S**2+CS*DSMGDS*ALAB(3)/S
      A(3,3)=+SCOS*DHMGDH*ALAB(2)/S**2+SN*DSMGDS*ALAB(3)/S
      RETURN
      END
C

C
C
C
      SUBROUTINE AVXYZ(VV,A,FVI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION VV(3),A(3)
      FK=FVI/40.
      A(1)=FK*9.2*VV(1) 
      A(2)=FK*40.*(VV(1)-VV(2)) 
      A(3)=FK*16.*(VV(2)-VV(3)) 
      RETURN
      END 
      DOUBLE PRECISION FUNCTION TJPC(A)   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     164 GRAD   =     2.86234
C     345 GRAD   =     6.021385
C      35 GRAD   =     0.6108652
C     168 GRAD   =     2.9321531   
C
C
      IF(A.GT.2.86234.AND.A.LT.6.0213859) THEN
                TJPC=0.56+ABS(0.2*COS(A+2.9321531))
      ELSE
                TJPC=0.36+ABS(0.4*COS(A+0.6108652))
      ENDIF
      RETURN
      END
      SUBROUTINE DELUV(XYZV,XYZN,DE,DL,DC,DH,DU,DV,NN)           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZV(1),XYZN(1) 
      DIMENSION CLUV(3,2)
      CALL CILUV(XYZV,CLUV,NN) 
      CALL CILUV(XYZN,CLUV(1,2),NN)
      CALL CDLUV(CLUV,DE,DL,DC,DH,DU,DV) 
      RETURN
      END 
      SUBROUTINE CILUV(XYZ,CLUV,NN)           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DER FARBKOORDINATEN NACH DEM CIELUV-SYSTEM (L*,u*,v*)
C 
C 
      DIMENSION XYZ(*),CLUV(*)
      DATA DDL/116./,DDLL/16./
      DATA DRI/0.3333333/,DTA/0.00885645/,DTB/0.137931/,DTC/7.787037/ 
      DATA RANGIN/1.E-30/
C 
C 
      SUU=FAKT(1,NN)+15.*FAKT(2,NN)+3.*FAKT(3,NN)
      USTRN=4.*FAKT(1,NN)/(SUU+RANGIN)
      VSTRN=9.*FAKT(2,NN)/(SUU+RANGIN)
      SUU=XYZ(1)+15.*XYZ(2)+3.*XYZ(3)
      USTR=4.*XYZ(1)/(SUU+RANGIN)
      VSTR=9.*XYZ(2)/(SUU+RANGIN)
C     HX0=XYZ(1)    /FAKT(1,NN) 
      HY0=XYZ(2)    /FAKT(2,NN) 
C     HZ0=XYZ(3)    /FAKT(3,NN) 
C     HX=DTC*HX0+DTB
      HY=DTC*HY0+DTB
C     HZ=DTC*HZ0+DTB
C     IF(HX0.GT.DTA) HX=HX0**DRI
      IF(HY0.GT.DTA) HY=HY0**DRI
C     IF(HZ0.GT.DTA) HZ=HZ0**DRI
      CLUV(1)=DDL*HY-DDLL
      CLUV(2)=13.*CLUV(1)*(USTR-USTRN)
      CLUV(3)=13.*CLUV(1)*(VSTR-VSTRN)
      RETURN
      END 
      SUBROUTINE CDLUV(CLUV,DECIE,DL,DC,DH,DU,DV)             
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DER FARBABSTAENDE DELUV,DL,DU,DV IM CIELUV-SYSTEM  
      DIMENSION CLUV(3,2)
      DL=CLUV(1,2)-CLUV(1,1)
      DU=CLUV(2,2)-CLUV(2,1)
      DV=CLUV(3,2)-CLUV(3,1)
      DC=SQRT(ABS(CLUV(2,2)**2+CLUV(3,2)**2))
     &  -SQRT(ABS(CLUV(2,1)**2+CLUV(3,1)**2))
      DECIE=SQRT(ABS(DL**2+DU**2+DV**2))
      DH=SQRT(ABS(DECIE**2-DL**2-DC**2))
      VZ=CLUV(2,2)*CLUV(3,1)-CLUV(2,1)*CLUV(3,2)
      IF(VZ.GT.0.) DH=-DH 
      RETURN
      END 
C
C
C     DIN6176
C
      SUBROUTINE DELAB9(XYZV,XYZN,DE,DL,DC,DH,DA,DB,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZV(*),XYZN(*)
      DIMENSION CISTR(3,2)
      CALL CILAB9(XYZV,CISTR,NN)
      CALL CILAB9(XYZN,CISTR(1,2),NN)
      CALL CILCH9(CISTR,DE,DL,DC,DH,DA,DB)
      RETURN
      END


C
C
C
C     Alte Version von 1999
C
C
C
      SUBROUTINE CILAB9alt(XYZ,CISTR,NN)
      USE MODFAKT

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DER FARBKOORDINATEN NACH DEM CIELAB-SYSTEM (L*,a*,b*)
C
C
      DIMENSION XYZ(*),CISTR(*)
      DATA DDL/116.D0/,DDA/500.D0/,DDB/200.D0/,DDLL/16.D0/
      DATA DRI/0.3333333D0/,DTA/0.00885645D0/
      DATA DTB/0.137931D0/,DTC/7.787037D0/
C
C
      HX0=XYZ(1)    /FAKT(1,NN)
      HY0=XYZ(2)    /FAKT(2,NN)
      HZ0=XYZ(3)    /FAKT(3,NN)
      HX=DTC*HX0+DTB
      HY=DTC*HY0+DTB
      HZ=DTC*HZ0+DTB
      IF(HX0.GT.DTA) HX=HX0**DRI
      IF(HY0.GT.DTA) HY=HY0**DRI
      IF(HZ0.GT.DTA) HZ=HZ0**DRI
C
C
C     L*,a*,b* (CIELAB)
C
C
      CISTR(1)    =DDL*HY-DDLL
      CISTR(2)    =DDA*(HX-HY)
      CISTR(3)    =DDB*(HY-HZ)
C
C     Umrechnung in L9,a9,b9 (DIN 6167)
C
C
      CH=CISTR(1)
      CISTR(1)=105.51*LOG(1.D0+0.0158*CH)/(AKE+10.*TINY(1.))
      EH=+0.96130*CISTR(2)+0.2756*CISTR(3)
      FH=-0.19292*CISTR(2)+0.6729*CISTR(3)
      HH=WNKEL(EH,FH)
      CH=SQRT(EH**2+FH**2)
      C9=LOG(1.D0+0.045*CH)/(0.045*AKCH*AKE)
      CISTR(2)=C9*COS(HH)
      CISTR(3)=C9*SIN(HH)
C      WRITE(27,*)'EH,FH,HH,CH,C9,CISTR1,CISTR2,CISTR3',EH,FH,HH,CH,C9,
C     & ( CISTR(I),I=1,3)
      RETURN
      END
C
C
C     DIN6176
C
C
C
C
      SUBROUTINE DEMAT9(XYZ,A,J)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8) :: XYZ(*),ALAB(3),A(3,3),B(3,3)
C
C
C     DLAB/DXYZ
C
      CALL DEMAT(XYZ,B,J)
C
C     L*,a*,b*
C
C
      CALL CILAB(XYZ,ALAB,J)
C
C     DLAB9/DLAB
C
C
      CALL DEMA99(ALAB,A)
C
C
      A=MATMUL(A,B)
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE DEMA99(ALAB,A)
      USE MODFAKT
      USE MODD99
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ALAB(*),A(3,*)
      EO=ALAB(2)*COS26+ALAB(3)*SIN26
      FO=-AA3*SIN26*ALAB(2)+AA3*COS26*ALAB(3)
      GGO=SQRT(EO**2+FO**2)
      DGODA=(COS26*EO-AA3*SIN26*FO)/GGO
      DGODB=(SIN26*EO+AA3*COS26*FO)/GGO
      HILF=1./(AA5*AKCH*AKE*(1.0+AA4*GGO))
      DC9DA=AA4*DGODA*HILF
      DC9DB=AA4*DGODB*HILF
      HH=WNKEL(EO,FO)
      H9=HH+RAD26
      C9=LOG(1.0+AA4*GGO)/(AA5*AKCH*AKE)

      DH9DA=COS26*DWNKEL(1,EO,FO) -AA3*SIN26*DWNKEL(2,EO,FO)
      DH9DB=SIN26*DWNKEL(1,EO,FO) +AA3*COS26*DWNKEL(2,EO,FO)
C
C
C     ABLEITUNGEN DLAB99/DLAB
C     J = NUMMER DER NORMLICHTART
C
C
C
C     DLAB9/DL*
C 
      A(1,1)=AA2/(AKE*(1.0+AA1*ALAB(1)))
      A(2,1)=0.0
      A(3,1)=0.0
C
C     DLAB9/Da*
C

      A(1,2)=0.0
      A(2,2)=DC9DA*COS(H9)-C9*SIN(H9)*DH9DA
      A(3,2)=DC9DA*SIN(H9)+C9*COS(H9)*DH9DA

C
C     DLAB9/Db*
C
      A(1,3)=0.0
      A(2,3)=DC9DB*COS(H9)-C9*SIN(H9)*DH9DB
      A(3,3)=DC9DB*SIN(H9)+C9*COS(H9)*DH9DB
      RETURN
      END
C
C
C
C     neue Version von 2005 s. Witt: FARBE und LACK 3(2005)86
C
C
      SUBROUTINE CILAB9(XYZ,CISTR,NN)
      USE MODFAKT

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     BERECHNUNG DER FARBKOORDINATEN NACH DEM CIELAB-SYSTEM (L*,a*,b*)
C
C
      DIMENSION XYZ(*),CISTR(*),ALAB(3)
C
C
C     CIELAB-Werte berechnen
C
      CALL CILAB(XYZ,ALAB,NN)
C
C     Umrechnung in L9,a9,b9 (DIN 6167)
C
C
      CALL CIL99(ALAB,CISTR)
      RETURN
      END

C
C
      SUBROUTINE XYZLCH9(XYZ,ALCC,NL,*)
      USE MODFAKT
      USE MODD99
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(3),ALCC(3),ALAB(3)
      DATA PID/0.0174533/,PI2/1.5707963/
      DATA WUR3/0.20689657/
C
C
C
C     XYZ    GESUCHT X,Y,Z-WERTE
C     ALCC   L99,C99,HGD99 (IN GRAD) (VORGEG.)
C     NL     NORMLICHTART
C
      AL=ALCC(1)
      AC=ALCC(2)
      HUE=ALCC(3)
      ALB=HUE*PID
      IF(ALB.GT.2.*PI2) ALB=ALB-4.*PI2
      AS=AC*COS(ALB)
      BS=AC*SIN(ALB)
      GOTO 20
      ENTRY  XYZLAB9(XYZ,ALCC,NL,*)
C
C
C
C     XYZ    GESUCHT X,Y,Z-WERTE
C     ALCC   L99,a99,b99 (VORGEG.)
C     NL     NORMLICHTART
C            NON-STANDARDRETURN FUER NICHT BERECHENBARE WERTE
C
      AL=ALCC(1)
      AS=ALCC(2)
      BS=ALCC(3)
C
C     Umrechnung von L99o,a99o,b99o nach L*,a*,b*
C
C
  20  ALAB(1)=(EXP(AL*AKE/AA0)-1.0)/AA1
      C99O=SQRT(AS**2+BS**2)
      H99O=WNKEL(AS,BS)
      HEOFO=H99O-RAD26
      GO=(EXP(AA5*C99O*AKCH*AKE)-1.0)/AA4
      EO=GO*COS(HEOFO)
      FO=GO*SIN(HEOFO)
      ALAB(2)=EO*COS26-(FO/AA3)*SIN26
      ALAB(3)=EO*SIN26+(FO/AA3)*COS26
C
C     Umrechnung von L*,a*,b* in XYZ
C
      CALL XYZLAB(XYZ,ALAB,NL,*90)
      RETURN
  90  RETURN 1
      END



C
C
      SUBROUTINE CIL99(ALAB,CISTR)
      USE MODFAKT
      USE MODD99

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     BERECHNUNG DER FARBKOORDINATEN NACH DEM CIELAB-SYSTEM (L*,a*,b*)
C
C
      DIMENSION ALAB(*),CISTR(*)
C
C

C
C     Umrechnung in L9,a9,b9 (DIN 6167)
C
C
      CH=ALAB(1)
      CISTR(1)=AA0*LOG(1.0+AA1*CH)/(AKE+TINY(1.0))
      EO=COS26*ALAB(2)+SIN26*ALAB(3)
      FO=-AA3*SIN26*ALAB(2)+AA3*COS26*ALAB(3)
      GOO=SQRT(EO**2+FO**2)
C
C     Winkel in Radian
C
      HEOFO=WNKEL(EO,FO)
      H99O=HEOFO+RAD26
      C99O=LOG(1.0+AA4*GOO)/(AA5*AKCH*AKE+TINY(1.0))
      CISTR(2)=C99O*COS(H99O)
      CISTR(3)=C99O*SIN(H99O)
      RETURN
      END


      SUBROUTINE CILCH9(CISTR,DECIE,DL,DC,DH,DA,DB)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DER FARBABSTAENDE DECIE,DL,DC,DH IM DIN6176 SYSTEM
      DIMENSION CISTR(3,*)
      DL=CISTR(1,2)-CISTR(1,1)
      DA=CISTR(2,2)-CISTR(2,1)
      DB=CISTR(3,2)-CISTR(3,1)
      DC=SQRT(ABS(CISTR(2,2)**2+CISTR(3,2)**2))
     &  -SQRT(ABS(CISTR(2,1)**2+CISTR(3,1)**2))
      DECIE=SQRT(ABS(DL**2+DA**2+DB**2))
      DH=SQRT(ABS(DECIE**2-DL**2-DC**2))
      VZ=CISTR(2,2)*CISTR(3,1)-CISTR(2,1)*CISTR(3,2)
      IF(VZ.GT.0.) DH=-DH
      RETURN
      END
      SUBROUTINE METAM9(R,RR,GKK,AMA,N1,N2)
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(*),RR(*)
      DIMENSION XYZV(6),XYZN(6),D1(3),D2(3)
C 
C 
C     METAMERIEINDEX NACH DIN 6172 FÜR DIN6176
C 
C 
C 
      IF(N1.GT.NLS().OR.N2.GT.NLS()) THEN
           AMA=0.
           RETURN
      ENDIF
      CALL NOGXX(XYZV,N1,R,GKK)
      CALL NOGXX(XYZN,N1,RR,GKK) 
      CALL DELAB9(XYZV,XYZN,DES,D1(1),DCS,DHS,D1(2),D1(3),N1)

      CALL NOGXX(XYZV(4),N2,R,GKK) 
      CALL NOGXX(XYZN(4),N2,RR,GKK)
      CALL DELAB9(XYZV(4),XYZN(4),DES,D2(1),DCS,DHS,D2(2),D2(3),N2)
C 
      AMA=SQRT((D1(1)-D2(1))**2+(D1(2)-D2(2))**2+(D1(3)-D2(3))**2)
C 
      RETURN
      END


      SUBROUTINE METAM9A(R,RR,GKK,AMA,N1,N2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(*),RR(*)
      DIMENSION XYZV(6),XYZN(6)
      DATA TOL/1.D-30/
C
C
C     METAMERIEINDEX NACH DIN 6172
C
C
C
      CALL NOGXX(XYZV,N1,R,GKK)
      CALL NOGXX(XYZN,N1,RR,GKK)
      CALL NOGXX(XYZV(4),N2,R,GKK)
      CALL NOGXX(XYZN(4),N2,RR,GKK)
C
C
      DO 10 I=1,3
      XYZN(I+3)=XYZN(I+3)*XYZV(I)/(ABS(XYZN(I))+TOL)
 10   CONTINUE
C
C
      CALL DELAB9(XYZV(4),XYZN(4),AMA,AAL,AAC,AAH,AAA,AAB,N2)
      RETURN
      END
      SUBROUTINE LCH9(XYZ,CIE,CIA,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     BERECHNUNG VON L,C,H (CIA)  UND  L,A,B (CIE)  NACH DIN 6167 AUS X,Y,Z


      DIMENSION XYZ(3),CIE(3),CIA(3)
C
C
      CALL CILAB9(XYZ,CIE,NN)
      CALL CCLCH(CIE,CIA,NN)
      RETURN
      END
c

C
C
C     DIN6174
C
C
C
      SUBROUTINE DELAB(XYZV,XYZN,DE,DL,DC,DH,DA,DB,NN)           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZV(*),XYZN(*) 
      DIMENSION CISTR(3,2)
      CALL CILAB(XYZV,CISTR,NN) 
      CALL CILAB(XYZN,CISTR(1,2),NN)
      CALL CILCH(CISTR,DE,DL,DC,DH,DA,DB) 
      RETURN
      END 
      SUBROUTINE CILAB(XYZ,CISTR,NN)           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DER FARBKOORDINATEN NACH DEM CIELAB-SYSTEM (L*,a*,b*)
C 
C 
      DIMENSION XYZ(*),CISTR(*) 
      DATA DDL/116.D0/,DDA/500.D0/,DDB/200.D0/,DDLL/16.D0/
      DATA DRI/0.3333333D0/,DTA/0.00885645D0/
      DATA DTB/0.137931D0/,DTC/7.787037D0/ 
C 
C 
      HX0=XYZ(1)    /FAKT(1,NN) 
      HY0=XYZ(2)    /FAKT(2,NN) 
      HZ0=XYZ(3)    /FAKT(3,NN) 
      HX=DTC*HX0+DTB
      HY=DTC*HY0+DTB
      HZ=DTC*HZ0+DTB
      IF(HX0.GT.DTA) HX=HX0**DRI
      IF(HY0.GT.DTA) HY=HY0**DRI
      IF(HZ0.GT.DTA) HZ=HZ0**DRI
      CISTR(1)    =DDL*HY-DDLL
      CISTR(2)    =DDA*(HX-HY)
      CISTR(3)    =DDB*(HY-HZ)
      RETURN
      END 
      SUBROUTINE CILCH(CISTR,DECIE,DL,DC,DH,DA,DB)             
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DER FARBABSTAENDE DECIE,DL,DC,DH IM CIELAB-SYSTEM  
      DIMENSION CISTR(3,*)
      DL=CISTR(1,2)-CISTR(1,1)
      DA=CISTR(2,2)-CISTR(2,1)
      DB=CISTR(3,2)-CISTR(3,1)
      DC=SQRT(ABS(CISTR(2,2)**2+CISTR(3,2)**2))
     &  -SQRT(ABS(CISTR(2,1)**2+CISTR(3,1)**2))
      DECIE=SQRT(ABS(DL**2+DA**2+DB**2)) 
      DH=SQRT(ABS(DECIE**2-DL**2-DC**2))
      VZ=CISTR(2,2)*CISTR(3,1)-CISTR(2,1)*CISTR(3,2)
      IF(VZ.GT.0.) DH=-DH 
      RETURN
      END 
C
      SUBROUTINE ABWIN(JABST,XYZV,SPHI,CPHI,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     
C
C
C     SIN UND COS DES WINKELS ZWISCHEN A* UND VEKTOR DER VORLAGE IN A*,B*-EBENE
C
C
C
      DIMENSION XYZV(*),ALAB(3),CMC(3)
C 
      SELECT CASE (JABST)
      CASE (0)
         CALL CILAB(XYZV,ALAB,NN)
      CASE (1)
         CALL CILAB9(XYZV,ALAB,NN)
      CASE (2)
         CALL CICMC(XYZV,ALAB,CMC,NN)
      END SELECT
      PHI=WNKEL(ALAB(2),ALAB(3))
      SPHI=SIN(PHI)
      CPHI=COS(PHI)
      RETURN
      END
      SUBROUTINE DEMAT(XYZ,A,J)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     ABLEITUNGEN DLAB/DXYZ
C     J = NUMMER DER NORMLICHTART
C
C
      DIMENSION XYZ(*),A(3,*)
      DIMENSION XYZQ(3)
      DO I=1,3
         XYZQ(I)=XYZ(I)/FAKT(I,J)
      ENDDO
C
C     DLAB/DX
C 
      A(1,1)=0.
      A(2,1)=500.*DFDRIQ(XYZQ(1))/FAKT(1,J)
      A(3,1)=0.
C
C     DLAB/DY
C
      HILF=DFDRIQ(XYZQ(2))/FAKT(2,J)
      A(1,2)=116.*HILF
      A(2,2)=-500.*HILF
      A(3,2)=200.*HILF
C
C     DLAB/DZ
C
      A(1,3)=0.
      A(2,3)=0.
      A(3,3)=-200.*DFDRIQ(XYZQ(3))/FAKT(3,J)
      RETURN
      END
      DOUBLE PRECISION FUNCTION DFDRIQ(Q)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA GAA/0.008856/,ALP/7.787/,DRI/0.3333334/,DR2/-.6666667/
      IF(Q.GT.GAA) THEN
         DFDRIQ=DRI*Q**DR2
      ELSE
         DFDRIQ=ALP
      ENDIF
      RETURN
      END
   

      SUBROUTINE ECHSAN(XYZV,XYZN,DEF,GS,DNOTE,ANBL,SSRB,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     DIMENSION DEL(9) 

         DIMENSION XYZV(3),XYZN(3)
      DIMENSION CIE(3),CIA(3,2)
      CHARACTER*8 GS,ANBL
      CHARACTER*8 NNOT(9)
      DATA        NNOT/'    1   ',
     &                 '   1-2  ',
     &                 '    2   ',
     &                 '   2-3  ',
     &                 '    3   ',
     &                 '   3-4  ',
     &                 '    4   ',
     &                 '   4-5  ',
     &                 '    5   '/
C
C
      CALL LCH(XYZV,CIE,CIA(1,1),NN)
      CALL LCH(XYZN,CIE,CIA(1,2),NN)
      CALL DELAB(XYZV,XYZN,DE,DL,DC,DH,DA,DB,NN)
C
C
C
C     BESTIMMUNG DER ECHTHEIT NACH ULSHOEFER: ISO 105 A05   
C
C
C
C
      CALL FTAEND(CIA(2,2),CIA(3,2),CIA(2,1),CIA(3,1),DL,DC,DH,
     &            DEF,J,DNOTE) 
C
C
C     GRAUMASSSTABSNOTE  (z.B.  ENTSPRICHT 4.5  DER NOTE 4-5)
C     FUER FARBTONAENDERUNG     ISO 105 A05
C
      
      GS=NNOT(J)               
C
C
C
C     ANBLUTEN NACH ISO 105 A04     (GRENZE 4.75 IST HIER AUF 4.9 ERHOEHT)
C
C
C
      CALL ANBLUT(DE,CIA(3,2),DL,SSRB,J)
C  
C     SSRB      DEZIMALWERT
C
C     NOTE FUER GRAUMASSSTAB     (z.B ENTSPRICHT  4-5  ANBL = 4.5)
C     NACH ISO 105 A04
C
      ANBL=NNOT(J)              
      RETURN
      END
      subroutine ftaend (cp,hp,ct,ht,dl,dc,dh,def,j,dnote)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C*********************************************************
C   Programm zur Berechnung der Farbtonaenderung
C     cp = C (CIELAB) Probe
C     hp = H (CIELAB) Probe 
C     ct = C (CIELAB) Bezug (Typ
C     ht = H (CIELAB) Bezug (Typ)
C     dl = Delta L
C     dc = Delta C
C     dh = Delta H
C     def = Delta EF (ISO 105 A05)
C     j = Nummer der Echtheitsnote
C     dnote = dezimale Echtheitsnote
C*********************************************************   
      dimension cdt(8)
      data cdt/11.60,8.2,5.80,4.1,2.95,2.10,1.25,0.4/
      cm=(cp+ct)/2.
      hm=(hp+ht)/2.
      if(abs(hp-ht).gt.180.and.((hp+ht).lt.360)) hm=hm+180
      if(abs(hp-ht).gt.180.and.((hp+ht).ge.360)) hm=hm-180
      if(abs(hm-280).le.180) xx=((hm-280.)/30.)**2
      if(abs(hm-280).gt.180) xx=((360 - abs(hm-280.))/30.)**2
      dd=dc*cm*exp(-xx)/100.
      dck=dc-dd
      dhk=dh-dd
      dcf=dck/(1+(cm/50)**2)
      dhf=dhk/(1+(cm/100)**2)
      def=sqrt(dl**2+dcf**2+dhf**2)
      if(def.le.3.4) then
        dnote=5-def/1.7
      else
        dnote=5-log(def/0.85D0)/log(2.D0)
      endif
      if(dnote.lt.1) dnote=1
      if(def.gt.cdt(1)) then
        j=0
      else
        do 60 j=1,7
        if(def.le.cdt(j).and.def.gt.cdt(j+1)) goto 70
   60   continue
        j=8
      endif
   70 j=j+1
      return
      end      

      subroutine anblut(de,hp,dl,ssrb,j)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C*********************************************************
C   Programm zur Berechnung der Farbtonaenderung
C     de = Delta E (CIELAB)
C     hp = H (CIELAB) Probe 
C     dl = Delta L
C     srrb = dezimale Echtheitsnote
C     j = Nummer der Echtheitsnote
C*********************************************************   
      dgs=de-.35*(1+cos(hp*3.14159/180.))*(de-abs(dl))
      if(dgs.lt.3.96) then
        ssrb=5-0.22727*dgs
      else
        ssrb=6.08496-1.44268*log(dgs)
      endif
      if(ssrb.lt.1.25) then
        j=1
      else
        do 40 j=2,7
        if(ssrb.ge.j*0.5+0.25.and.ssrb.lt.(j+1)*0.5+0.25) goto 50
   40   continue
        if(ssrb.gt.4.25.and.ssrb.lt.4.9) then
          j=8
          return
        endif
        j=9
      endif
   50 return
      end

C
      

      SUBROUTINE HLAB(XYZ,AHLAB,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DER HUNTER L,A,B-KOORDINATEN 
      DIMENSION XYZ(3),AHLAB(3) 
C 
C 
      DIMENSION AK(3)
      DIMENSION BK(3)
C
C
C     AK(I),BK(I)
C     I=1 Normlichtart c
C     I=2 Normlichtart A
C     I=3 Normlichtart D65 (alle anderen Normlichtarten werdem mit den gleichen AK bzw. BK-Werten berechnet)
C
      DATA AK/175.,185.,172./
      DATA BK/ 70., 38., 67./
      DATA RIN/1.E-30/
      XN=XYZ(1)/FAKT(1,NN)
      YN=XYZ(2)/FAKT(2,NN)
      ZN=XYZ(3)/FAKT(3,NN)
C 
C 
C 
      WY=SQRT(ABS(YN))      
      AHLAB(1)=100.*WY
      NL=MOD(NLL(NN),100)
      IF(NL.LE.0.OR.NL.GT.3) THEN
         NL=3
      ENDIF
      AHLAB(2)=AK(NL)*(XN-YN)/(WY+RIN)
      AHLAB(3)=BK(NL)*(YN-ZN)/(WY+RIN)
      RETURN
      END 
      SUBROUTINE HULAB(XYZV,XYZN,DE,DL,DA,DB,DC,DH,NN)          
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZV(*),XYZN(*)
      DIMENSION AHLAB(3,2)
      CALL HLAB(XYZV,AHLAB,NN)
      CALL HLAB(XYZN,AHLAB(1,2),NN) 
      CALL HUNTE(AHLAB,DE,DL,DA,DB,DC,DH)
      RETURN
      END 
      SUBROUTINE HUNTE(AHLAB,DEHUN,DL,DA,DB,DC,DH)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DES FARBABSTANDES NACH HUNTER AUS DEN L,A,B-KOORDINATEN
C 
      DIMENSION AHLAB(3,2)
      DL=AHLAB(1,2)-AHLAB(1,1)
      DA=AHLAB(2,2)-AHLAB(2,1)
      DB=AHLAB(3,2)-AHLAB(3,1)
      DEHUN=SQRT(ABS(DL*DL+DA*DA+DB*DB))
      DC=SQRT(AHLAB(2,2)**2+AHLAB(3,2)**2)
     &  -SQRT(AHLAB(2,1)**2+AHLAB(3,1)**2)
      DH=SQRT(ABS(DEHUN**2-DL**2-DC**2))
      VZ=AHLAB(2,2)*AHLAB(3,1)-AHLAB(2,1)*AHLAB(3,2)
      IF(VZ.GT.0.) DH=-DH

      RETURN
      END 
      SUBROUTINE URXYZ(XYZ,RXYZ,NN) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     BERECHNUNG DER RX,RY,RZ-WERTE 
C     DIE BA-WERTE AUS "ANLEITUNG FUER RFC3" WERDEN NICHT VERWENDET 
C     XYZ = X,Y,Z 
C     RXYZ= RX,RY,RZ
C 
C 
C 
      DIMENSION BA(3,2)
      DIMENSION XYZ(3),RXYZ(3)
C              NLC10   NLA10   NLD10   NLC02   NLA02   NLD02
      DATA BA/0.25175,0.05133,0.23387,0.25220,0.05155,0.23404/
C 
      NL=NLL(NN)
      NL=MOD(NL,100)
      IF(NL.LE.0.OR.NL.GT.3) THEN
        NL=3
      ENDIF
      NI=1
      IF(NLL(NN).GE.100) THEN
        NI=2
      ENDIF
      BD=BA(NL,NI)
      AX=FAKT(1,NN)/100.
      CX=FAKT(3,NN)/100.
      RXYZ(2)=XYZ(2)
      RXYZ(3)=XYZ(3)/CX 
      RXYZ(1)=XYZ(1)*(1.+BD)/AX-BD*RXYZ(3)  
      RETURN
      END 
      SUBROUTINE XYZFI(IRXYZ,XYZ,NN,GKK)             
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
      DIMENSION FKKT(6,3) 
      DIMENSION IRXYZ(3),XYZ(3) 
C 
C 
C 
C     IRXYZ= RX,RY,RZ-WERTE 
C     XYZ=TRISTIMULUSWERTE
C     FKKT GEWICHTSFAKTOREN A,B,C AUS ZEISS-ANLEITUNG FUER RFC3 
C 
C 
C 
C                NLC10   NLA10   NLD10    NLC02    NLA02    NLD02
      DATA FKKT/0.77718,1.05719,0.768417,0.783185,1.044623,0.770180,
     &          0.19566,0.05427,0.179707,0.197520,0.053849,0.180251,
     &          1.16144,0.35202,1.073241,1.182246,0.355824,1.088814/
C 
C 
      NL=NLL(NN)
      IF(NL.LE.0.OR.NL.GT.3) THEN
         NL=3
      ENDIF
      NI=1
      IF(NLL(NN).GE.100) THEN
        NI=2
      ENDIF
      DO 10 I=1,3 
      XYZ(I)=IRXYZ(I)/100.
  10  CONTINUE
      N=(NI-1)*3+NL
      XYZ(1)=FKKT(N,1)*XYZ(1)+FKKT(N,2)*XYZ(3)
      XYZ(3)=FKKT(N,3)*XYZ(3)
      DO 20 I=1,3 
      XYZ(I)=XYZ(I)-GKK*FAKT(I,NN)
   20 CONTINUE
      RETURN
      END 
      SUBROUTINE XYZKL(XYZ,XYK)          
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(3),XYK(2) 
      SUMM=0.
      DO  I=1,3
      SUMM=SUMM+XYZ(I)
      ENDDO
      IF(SUMM.GT.0.) THEN
          DO   I=1,2
          XYK(I)=XYZ(I)/SUMM
          ENDDO
      ELSE
          DO  I=1,2
          XYK(I)=0.
          ENDDO
      ENDIF     
      RETURN
      END 
      FUNCTION IBWERT(R,GKK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(*)
      DIMENSION XYZ(3)
      CALL NOGXX(XYZ,1,R,GKK)
      BHILF=1.E30
      DO I=1,8
        CALL BWERT(XYZ,I,BA,1)
        IF(ABS(BA).LT.BHILF) THEN
               IBWERT=I
               BHILF=ABS(BA)
        ENDIF
      ENDDO
      RETURN
      END
      SUBROUTINE BWERT(XYZ,ISTA,B,NN)          
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
C 
C     XYZ    X,Y,Z-WERTE ZUR BERECHNUNG VON B 
C     ISTA   STANDARDFARBTIEFE (1/1 USW ) 
C            1     1/1
C            2     1/3
C            3     1/9
C            4     1/25
C            5     1/200
C            6     1/6
C            7     1/12
C            8     2/1    
C
C
C
C     B      B-WERT NACH GALL-RIEDEL
C     NNL     INDEX FUER NORMLICHTART 
C 
C 
C 
C 
      DIMENSION F(8)
      DIMENSION XYZ(3)
      DIMENSION XYK(2)
      DATA F/19.,29.,41.,56.,73.,37.,48.,15./ 
C 
      IF(ISTA.LT.1.OR.ISTA.GT.8) THEN
        BA=HUGE(1.0)
        RETURN
      ENDIF
      YG=XYZ(2) 
      IF (YG.LT.0.) YG=0. 
      SQY=SQRT(ABS(YG))
      CALL XYZKL(XYZ,XYK) 
      XK=XYK(1) 
      YK=XYK(2) 
C 
C 
C 
      SUU=0.
      DO 1 I=1,3
      SUU=SUU+FAKT(I,NN)
 1    CONTINUE  
      X=XK-FAKT(1,NN)/SUU
      Y=YK-FAKT(2,NN)/SUU
      S=10. *SQRT(ABS(X*X + Y*Y))
      PHI2=WNGRD(X,Y) 
      ISTB=ISTA
      IF(NLL(NN).GE.100) THEN
C---
C
C
C
C        2 GRAD BEOBACHTER
C
         IF(ISTB.LE.5) THEN
           AL=POLY2(PHI2,ISTB)
         ELSE IF (ISTB.EQ.6) THEN
C          
C        1/6 RICHTTYPTIEFE
C
           AL=0.685*POLY2(PHI2,2)+0.315*POLY2(PHI2,4)
C
C
C        1/12 RICHTTYPTIEFE
C
C     
         ELSE IF (ISTB.EQ.7) THEN
           AL=0.350*POLY2(PHI2,2)+0.650*POLY2(PHI2,4)
C
C
C
C        2/1 RICHTTYPTIEFE
C
C
         ELSE IF (ISTB.EQ.8) THEN
           AL=1.630*POLY2(PHI2,1)-0.630*POLY2(PHI2,2)
         ENDIF
         GOTO 30

C
C        10 GRAD BEOBACHTER
C
C
      ELSE
C
C
         IF(ISTB.LE.5) THEN
           AL=POL10(PHI2,ISTB)
         ELSE IF (ISTB.EQ.6) THEN
C          
C        1/6 RICHTTYPTIEFE
C
           AL=0.685*POL10(PHI2,2)+0.315*POL10(PHI2,4)
C
C
C        1/12 RICHTTYPTIEFE
C
C     
         ELSE IF (ISTB.EQ.7) THEN
           AL=0.350*POL10(PHI2,2)+0.650*POL10(PHI2,4)
C
C
C
C        2/1 RICHTTYPTIEFE
C
C
         ELSE IF (ISTB.EQ.8) THEN
           AL=1.630*POL10(PHI2,1)-0.630*POL10(PHI2,2)
         ENDIF
      ENDIF
C---
   30 B=F(ISTB) - SQY*(10. - AL*S)  
   70 RETURN
      END 
      DOUBLE PRECISION FUNCTION POLY2(PHI,ST)            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IANZ(5) 
      INTEGER*4 ST
      DIMENSION A(5,14,5),A1(5,14),A2(5,14),A3(5,14), 
     &A4(5,14),A5(5,14) 
      EQUIVALENCE (A(1,1,1),A1),(A(1,1,2),A2),
     &(A(1,1,3),A3),(A(1,1,4),A4),(A(1,1,5),A5) 
      DATA IANZ /9,10,9,11,14/
      DATA A1 / 
     & 0.         , 0.21620E 01, 0.21146E 01, 0.56685E 01,-0.76914E 01, 
     & 0.56000E 02, 0.37730E 01, 0.22982E 01,-0.11750E 02, 0.17685E 02, 
     & 0.88000E 02, 0.38850E 01,-0.14047E 01, 0.86005E 00,-0.54797E 00, 
     & 0.20000E 03, 0.26205E 01,-0.71265E 00,-0.72510E 01,-0.64238E 01, 
     & 0.22800E 03, 0.17110E 01,-0.25648E 01,-0.57746E 02, 0.31603E 03, 
     & 0.24400E 03, 0.11170E 01, 0.36796E 01,-0.41844E 01, 0.15426E 01, 
     & 0.32800E 03, 0.21700E 01,-0.10190E 01, 0.47036E 01,-0.18645E 02, 
     & 0.34400E 03, 0.20510E 01,-0.14617E 01, 0.15468E 02,-0.12263E 02, 
     & 0.36000E 03, 0.10000E 01, 0.10000E 01, 0.10000E 01, 0.10000E 01, 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         / 
      DATA A2 / 
     & 0.         , 0.19710E 01, 0.18854E 01, 0.72139E 01,-0.98081E 01, 
     & 0.52000E 02, 0.35230E 01, 0.56964E 00,-0.97366E 00, 0.38087E 00, 
     & 0.15600E 03, 0.34910E 01,-0.36932E 00,-0.55142E 01, 0.14815E 01, 
     & 0.18800E 03, 0.28560E 01,-0.28126E 01,-0.23760E 01, 0.11254E 02, 
     & 0.21600E 03, 0.21300E 01,-0.27444E 01,-0.25405E 02, 0.62615E 02, 
     & 0.25200E 03, 0.77100E 00,-0.42114E 00, 0.70062E 02,-0.18287E 03, 
     & 0.27600E 03, 0.21770E 01, 0.27983E 01,-0.12018E 02, 0.17020E 02, 
     & 0.30800E 03, 0.24000E 01,-0.49271E 00, 0.31561E 01,-0.87220E 01, 
     & 0.34400E 03, 0.22240E 01,-0.29558E 01,-0.51289E 01, 0.85152E 02, 
     & 0.36000E 03, 0.10000E 01, 0.10000E 01, 0.10000E 01, 0.10000E 01, 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         / 
      DATA A3 / 
     & 0.         , 0.23380E 01,-0.89972E 00, 0.11961E 02,-0.10490E 02, 
     & 0.48000E 02, 0.35020E 01, 0.52484E 01,-0.17432E 02, 0.19165E 02, 
     & 0.92000E 02, 0.40690E 01,-0.36496E 00, 0.64710E 00,-0.14174E 01, 
     & 0.18800E 03, 0.30610E 01,-0.17819E 01,-0.37832E 01,-0.48936E 01, 
     & 0.22400E 03, 0.17010E 01,-0.58311E 01, 0.14461E 02,-0.12941E 02, 
     & 0.24400E 03, 0.10100E 01, 0.46563E 01,-0.21240E 00,-0.57842E 01, 
     & 0.28800E 03, 0.25250E 01, 0.15168E 01,-0.15772E 01,-0.63062E 00, 
     & 0.34400E 03, 0.27690E 01,-0.85358E 00,-0.31714E 02, 0.12577E 03, 
     & 0.36000E 03, 0.10000E 01, 0.10000E 01, 0.10000E 01, 0.10000E 01, 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         / 
      DATA A4 / 
     & 0.         , 0.23990E 01,-0.30667E 01, 0.16380E 02,-0.10999E 02, 
     & 0.24000E 02, 0.24540E 01, 0.60507E 01, 0.19039E 02,-0.87763E 02, 
     & 0.44000E 02, 0.37250E 01, 0.68347E 01,-0.38741E 02, 0.69481E 02, 
     & 0.72000E 02, 0.41260E 01,-0.30389E 00, 0.56079E 01, 0.56553E 01, 
     & 0.10400E 03, 0.47880E 01, 0.35030E 01,-0.17579E 02, 0.20218E 02, 
     & 0.14400E 03, 0.46710E 01,-0.16006E 01,-0.37578E 01, 0.29038E 01, 
     & 0.19600E 03, 0.32310E 01,-0.39918E 01,-0.35840E 01,-0.74141E 01, 
     & 0.22000E 03, 0.19640E 01,-0.86798E 01, 0.27377E 02,-0.17633E 02, 
     & 0.23600E 03, 0.12040E 01, 0.30071E 01, 0.40166E 01,-0.71562E 01, 
     & 0.29600E 03, 0.29080E 01, 0.41689E 00,-0.10629E 01,-0.12985E 01, 
     & 0.36000E 03, 0.10000E 01, 0.10000E 01, 0.10000E 01, 0.10000E 01, 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         / 
      DATA A5 / 
     & 0.         , 0.57810E 01,-0.82288E 01,-0.75625E 01, 0.32188E 02, 
     & 0.20000E 02, 0.40900E 01,-0.62950E 01, 0.27716E 02,-0.28732E 02, 
     & 0.56000E 02, 0.40750E 01, 0.39274E 01, 0.13590E 02,-0.14148E 02, 
     & 0.88000E 02, 0.62600E 01, 0.67537E 01,-0.11422E 02,-0.22258E 02, 
     & 0.10400E 03, 0.69570E 01, 0.23532E 01,-0.12454E 02, 0.14924E 02, 
     & 0.14000E 03, 0.68870E 01,-0.25572E 01, 0.41401E 01,-0.99624E 01, 
     & 0.19600E 03, 0.50030E 01,-0.41350E 01,-0.31582E 02, 0.78570E 02, 
     & 0.21600E 03, 0.35420E 01,-0.41230E 01, 0.16913E 02, 0.24602E 02, 
     & 0.23200E 03, 0.34160E 01, 0.28516E 00, 0.12120E 03,-0.29969E 03, 
     & 0.24800E 03, 0.53360E 01, 0.19078E 02,-0.11038E 03, 0.31144E 03, 
     & 0.26400E 03, 0.68390E 01, 0.43458E 01,-0.12814E 02, 0.15352E 02, 
     & 0.30400E 03, 0.75100E 01,-0.10073E 01, 0.12544E 01,-0.47446E 01, 
     & 0.34400E 03, 0.70040E 01,-0.51011E 01,-0.17707E 02, 0.10250E 02, 
     & 0.36000E 03, 0.10000E 01, 0.10000E 01, 0.10000E 01, 0.10000E 01/ 
C 
C     DIMENSION VON A ALLGEMEIN (L,M,N) 
C     L - ANZAHL DER KONSTANTEN PRO BEREICH = 5 
C     M - MAX. ANZAHL DER WINKELBEREICHE = 14, WENN 1/200 ST MIT ERFASST
C         WERDEN SOLL 
C     N - ANZAHL DER STANDARDFARBTIEFEN = 5 (1/1,1/3,1/9,1/25,1/200 ST) 
C     ST - NR. DER STANDARDFARBTIEFE   1=1/1 ST USW.
C     PHI=WINKEL
C     POLY2=A(PHI) 
C 
      ID=IANZ(ST) 
      DO 5 I=1,ID 
      K=I 
      IF (A(1,I,ST).LE.PHI.AND.PHI.LE.A(1,I+1,ST)) GOTO 20
    5 CONTINUE
   20 W=(PHI-A(1,K,ST))/100.
      POLY2=A(2,K,ST) + A(3,K,ST)*W + A(4,K,ST)*(W*W) + A(5,K,ST) * W
     &**3 
C 
C 
      RETURN
      END 
      DOUBLE PRECISION FUNCTION POL10(PHI,ST)         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IANZ(5) 
      INTEGER*4 ST
      DIMENSION A(5,14,5),A1(5,14),A2(5,14),A3(5,14), 
     &A4(5,14),A5(5,14) 
      EQUIVALENCE (A(1,1,1),A1),(A(1,1,2),A2),
     &(A(1,1,3),A3),(A(1,1,4),A4),(A(1,1,5),A5) 
      DATA IANZ /8,9,8,9,9/ 
      DATA A1 / 
     & 0.         , 0.22500E 01, 0.18503E 01, 0.86812E 01,-0.11633E 02, 
     & 0.60000E 02, 0.39730E 01, 0.15121E 01,-0.62360E 01, 0.38987E 01, 
     & 0.15600E 03, 0.31270E 01,-0.12185E 01, 0.53904E 01,-0.89181E 01, 
     & 0.22400E 03, 0.19870E 01,-0.40070E 01,-0.47812E 02, 0.28402E 03, 
     & 0.24000E 03, 0.12850E 01, 0.74269E 01,-0.17555E 02, 0.89551E 01, 
     & 0.26800E 03, 0.21850E 01, 0.82243E 00, 0.74124E 00,-0.27321E 01, 
     & 0.34000E 03, 0.21420E 01,-0.10320E 01, 0.80563E 01,-0.82812E 00, 
     & 0.36000E 03, 0.10000E 01, 0.10000E 01, 0.10000E 01, 0.10000E 01, 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         / 
      DATA A2 / 
     & 0.         , 0.20400E 01, 0.18016E 01, 0.91563E 01,-0.12687E 02, 
     & 0.52000E 02, 0.36690E 01, 0.14459E 01,-0.35905E 01, 0.20001E 01, 
     & 0.14000E 03, 0.35240E 01, 0.12189E 01,-0.88036E 01, 0.72024E 01, 
     & 0.19600E 03, 0.27100E 01,-0.56220E 00,-0.94526E 01, 0.19922E 01, 
     & 0.23600E 03, 0.11010E 01,-0.41873E 01, 0.16972E 02, 0.11867E 03, 
     & 0.25200E 03, 0.13510E 01, 0.79846E 01,-0.98320E 01,-0.14277E 02, 
     & 0.27600E 03, 0.25040E 01, 0.15494E 01,-0.39106E 01, 0.16213E 01, 
     & 0.34000E 03, 0.23190E 01,-0.25789E 01,-0.40381E 01, 0.50031E 02, 
     & 0.36000E 03, 0.10000E 01, 0.10000E 01, 0.10000E 01, 0.10000E 01, 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         / 
      DATA A3 / 
     & 0.         , 0.23450E 01,-0.63416E 00, 0.11219E 02,-0.78838E 01, 
     & 0.52000E 02, 0.39400E 01, 0.24057E 01,-0.49663E 01, 0.24771E 01, 
     & 0.14800E 03, 0.38640E 01,-0.12891E 01, 0.10054E 01,-0.38938E 01, 
     & 0.22400E 03, 0.17560E 01,-0.49395E 01,-0.20890E 02, 0.18812E 03, 
     & 0.24400E 03, 0.14380E 01, 0.58012E 01,-0.10498E 01,-0.12963E 02, 
     & 0.28000E 03, 0.27850E 01, 0.16785E 01,-0.33505E 01, 0.14638E 01, 
     & 0.33600E 03, 0.29320E 01,-0.15271E 01,-0.15605E 02, 0.49184E 02, 
     & 0.36000E 03, 0.10000E 01, 0.10000E 01, 0.10000E 01, 0.10000E 01, 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         / 
      DATA A4 / 
     & 0.         , 0.23600E 01,-0.30517E 01, 0.11086E 02, 0.30080E 02, 
     & 0.28000E 02, 0.30350E 01, 0.89384E 01,-0.15295E 02,-0.96484E 01, 
     & 0.56000E 02, 0.41270E 01,-0.28082E 00, 0.50835E 01,-0.15742E 01, 
     & 0.96000E 02, 0.47270E 01, 0.25753E 01,-0.66465E 01, 0.26101E 01, 
     & 0.14800E 03, 0.46360E 01,-0.20872E 01, 0.12793E 00,-0.32720E 01, 
     & 0.22400E 03, 0.16870E 01,-0.58010E 01, 0.28726E 02, 0.27294E 02, 
     & 0.24400E 03, 0.18950E 01, 0.60961E 01,-0.83086E 01, 0.23135E 01, 
     & 0.29200E 03, 0.31620E 01, 0.66899E 00,-0.32459E 01, 0.79639E 00, 
     & 0.36000E 03, 0.10000E 01, 0.10000E 01, 0.10000E 01, 0.10000E 01,
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         / 
      DATA A5 / 
     & 0.         , 0.56800E 01,-0.91946E 01,-0.19978E 02, 0.95984E 02, 
     & 0.28000E 02, 0.36460E 01,-0.99084E 00, 0.12063E 02,-0.13496E 01, 
     & 0.80000E 02, 0.62030E 01, 0.14132E 02,-0.49884E 02, 0.56098E 02, 
     & 0.12000E 03, 0.74640E 01,-0.15786E 01, 0.12588E 01,-0.42266E 01, 
     & 0.20800E 03, 0.41700E 01,-0.89904E 01, 0.23009E 02, 0.15734E 02, 
     & 0.23200E 03, 0.35550E 01, 0.42690E 01, 0.21508E 03,-0.72384E 03, 
     & 0.24800E 03, 0.67790E 01, 0.10693E 02,-0.40940E 02, 0.58807E 02, 
     & 0.28000E 03, 0.79350E 01,-0.10953E 00, 0.63794E 00,-0.50045E 01, 
     & 0.36000E 03, 0.10000E 01, 0.10000E 01, 0.10000E 01, 0.10000E 01, 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         , 
     & 0.         , 0.         , 0.         , 0.         , 0.         / 
C 
C     DIMENSION VON A ALLGEMEIN (L,M,N) 
C     L - ANZAHL DER KONSTANTEN PRO BEREICH = 5 
C     M - MAX. ANZAHL DER WINKELBEREICHE = 14, WENN 1/200 ST MIT ERFASST
C         WERDEN SOLL 
C     N - ANZAHL DER STANDARDFARBTIEFEN = 5 (1/1,1/3,1/9,1/25,1/200 ST) 
C     ST - NR. DER STANDARDFARBTIEFE   1=1.1 ST USW.
C     PHI=WINKEL
C     POL10=A(PHI) 
C 
      ID=IANZ(ST) 
      DO 5 I=1,ID 
      K=I 
      IF (A(1,I,ST).LE.PHI.AND.PHI.LE.A(1,I+1,ST)) GOTO 20
    5 CONTINUE
   20 W=(PHI-A(1,K,ST))/100.
      POL10=A(2,K,ST) + A(3,K,ST)*W + A(4,K,ST)*(W*W) + A(5,K,ST) * W
     &**3 
C 
C 
      RETURN
      END 
C
C
C
      SUBROUTINE GEWICH(NWE,GV,SUMM,K)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION GV(*)
      DIMENSION HILF(3)
C
C      NWE     ANZAHL WELLENLAENGEN
C      GV      K/S-WERTE ODER EXTINKTIONSWERTE
C      SUMM    ERGEBNIS
C      K       NUMMER DER NORMLICHTART
C
C
C
      CALL NOGXX(HILF,K,GV,0.D0)
      SUMM=0.
      DO 22 I=1,3
      SUMM=SUMM+HILF(I)
  22  CONTINUE
      RETURN
      END
      SUBROUTINE RWMINI(NTM,JMIN,WMIN,RMIN,NWE,WSOL,RR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RMIN(*),WMIN(*)
      DIMENSION RR(*),WSOL(*),R(NWE)
      INTEGER JMIN(*)
C
C
C     R-Werte für Minima
C
C
      KEN=0
      DO I=1,NWE
         R(I)=RR(I)
      END DO
      GOTO 10
C
C
C     R-Werte für Maxima
C
C
      ENTRY RWMAXI(NTM,JMIN,WMIN,RMIN,NWE,WSOL,RR)
      KEN=1
      DO I=1,NWE
         R(I)=-RR(I)
      END DO
C
C
C     BESTIMMUNG VON NTM REFLEXIONSMINIMA
C
C
 10   DEL=WSOL(2)-WSOL(1)
      DO L=1,NTM
         JMIN(L)=L
         WMIN(L)=HUGE(1.)
         RMIN(L)=HUGE(1.)
      ENDDO
      WZALT=HUGE(1.)
C
C
C     RANDWERTE ALS STARTWERTE VERWENDEN
C
C
C
      IANF=0
      IF(R(1).LT.R(2)) THEN       
               IANF=1
               RMIN(IANF)=R(1)
               WMIN(IANF)=WSOL(1)
               JMIN(IANF)=1
      ENDIF
      IF(R(NWE).LT.R(NWE-1)) THEN
               IANF=IANF+1
               RMIN(IANF)=R(NWE)
               WMIN(IANF)=WSOL(NWE)
               JMIN(IANF)=NWE
      ENDIF
      IF(IANF.EQ.2) THEN
               IF(RMIN(1).GT.RMIN(2)) THEN
                  RRR=RMIN(1)
                  RMIN(1)=RMIN(2)
                  RMIN(2)=RRR
                  RRR=WMIN(1)
                  WMIN(1)=WMIN(2)
                  WMIN(2)=RRR
                  JJ=JMIN(1)
                  JMIN(1)=JMIN(2)                   
                  JMIN(2)=JJ
                ENDIF
      ENDIF
C
C
C     SUCHE NACH MINIMA 
C
C
      DO J=2,NWE-1
          RRD=R(J+1)-R(J-1)
          RRE=R(J+1)-2.*R(J)+R(J-1)
          IF(ABS(RRD).LE.ABS(RRE).AND.RRE.GT.0.) THEN
            WZMIN=WSOL(J)-0.5*DEL*RRD/RRE
            IF(ABS(WZALT-WZMIN).GT.DEL) THEN
             WZALT=WZMIN
C             WWD=(WZMIN-WSOL(J))/DEL
             RZMIN=RWZWIS(WZMIN,NWE,WSOL,R)
C             RZMIN=R(J)+0.5*RRE*WWD**2+0.5*RRD*WWD
             DO L=1,NTM
                IF(RZMIN.LT.RMIN(L)) THEN
C                   
C                     
C               SUCHE, OB WZMIN IN NACHBARSCHAFT VON WMIN(L)
C                
C                  
                DO LL=1,NTM
                   IF(ABS(WZMIN-WMIN(LL)).LT.DEL) THEN
                      IF(RZMIN.LT.RMIN(L)) THEN
                         RMIN(L)=RZMIN
                         WMIN(L)=WZMIN
                         JMIN(L)=J   
                      ENDIF
                      GOTO 50
                   ENDIF  
                ENDDO
C
C
C               VERSCHIEBEN
C
C
C
                DO LL=NTM,L+1,-1
                   RMIN(LL)=RMIN(LL-1)
                   WMIN(LL)=WMIN(LL-1)
                   JMIN(LL)=JMIN(LL-1)
                ENDDO
                RMIN(L)=RZMIN
                WMIN(L)=WZMIN
                JMIN(L)=J
                GOTO 50
               ENDIF
             ENDDO
           ENDIF
          ENDIF
 50   ENDDO
      IF(KEN.EQ.1) THEN
        DO L=1,NTM
          RMIN(L)=-RMIN(L)
        END DO
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION RWZWIS(WZMIN,NWE,WSOL,R)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8),DIMENSION(*) :: WSOL,R
      RWZWIS=RWZWI(WZMIN,NWE,WSOL,R)
      RETURN
      END
      DOUBLE PRECISION FUNCTION RW6164(WZMIN,NW35,WSOL35,R)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8),DIMENSION(*) :: WSOL35,R
      RW6164=RWZWI(WZMIN,NW35,WSOL35,R)
      RETURN
      END

      DOUBLE PRECISION FUNCTION RWZWI(WZMIN,NWE,WSOL,R)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NSTZ=10,NDIM=4*NSTZ+5*(NSTZ+4))
      DIMENSION PUF(NDIM),R(*),ABW(1),XA(1),YA(1),WSOL(*)
      DATA MERK/0/,P/0.D0/,M/0/,ABW/-1.D0/
      DO I=1,NWE-1
        IF(WZMIN.GE.WSOL(I).AND.WZMIN.LT.WSOL(I+1)) THEN
          IF(I.LE.NSTZ/2) THEN
            J=1
          ELSEIF(I.GE.NWE-NSTZ/2+1) THEN
            J=NWE-NSTZ+1
          ELSE
            J=I-NSTZ/2
          ENDIF
          EXIT
        ENDIF
      END DO
      CALL AKINT(PUF,NSTZ,WSOL(J),R(J),IER,Merk,M,P,Abw)
      XA(1)=WZMIN
      CALL INTAKI(PUF,NSTZ,WSOL(J),1,XA,YA)
      RWZWI=YA(1)
      RETURN
      END
      DOUBLE PRECISION FUNCTION FRZMIN(WWD,RL,RM,RR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     BERECHNUNG DES REFLEXIONSWERTES AN DER STELLE WZMIN
C     WWD=(WZMIN-WSOL(J))/(WSOL(J+1)-WSOL(J))
C
      RRD=RR-RL
      RRE=RR-2.*RM+RL
      FRZMIN=RM+0.5*RRE*WWD**2+0.5*RRD*WWD
      RETURN
      END
      REAL(KIND=8) FUNCTION DWNKEL(J,EO,FO)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C
C     Ableitungen Winkel(in rad) nach EO und FO (Abszisse und Ordinate von atan(FO/EO)
C
      GO=EO**2+FO**2
      IF(GO.EQ.0.0) THEN
         DWNKEL=0.0D0
         RETURN
      ENDIF
      SELECT CASE (J)
          CASE (1)
            DWNKEL=-FO/GO
          CASE(2)
            DWNKEL=EO/GO
          CASE DEFAULT
      END SELECT
      RETURN
      END FUNCTION

      DOUBLE PRECISION FUNCTION WNKEL ( X , Y )              
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI/3.14159265359/
      WNKEL=0.D0
      IF(X.EQ.0.AND.Y.EQ.0.) GOTO 50
      WNKEL=ATAN2(Y,X)
      IF(WNKEL.LT.0.) WNKEL=2.*PI+WNKEL 
   50 RETURN
      END 
      DOUBLE PRECISION FUNCTION WNGRD(X,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      ZWI=WNKEL(X,Y)
      WNGRD=ZWI*57.29578  
      RETURN
      END
      SUBROUTINE D6164(DINTSD,DE,DT,DS,DD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     ***************************** 
C     DINTSD(1,I) = DIN-FARBTON     
C     DINTSD(2,I) = DIN-SAETTIGUNG
C     DINTSD(3,I) = DIN-DUNKELSTUFE 
C     ***************************** 
C 
      DIMENSION A(3),DINTSD(3,2)
      DUN=10.-(DINTSD(3,1)+DINTSD(3,2))/2.
      DSS=(DINTSD(2,1)+DINTSD(2,2))/2.
      DT=DINTSD(1,1)-DINTSD(1,2)
      IF(ABS(DT).GT.12.5)DT=ABS(DT)-24. 
      DS=DINTSD(2,1)-DINTSD(2,2)
      DD=DINTSD(3,1)-DINTSD(3,2)
      A(1)=DUN*DSS*DT/54. 
      A(2)=DUN*DS/9.
      A(3) = DD 
      DE1 = 0.
      DO   I = 1,3
      DE1 = DE1 + A(I)**2
      ENDDO
      DE =SQRT(ABS(DE1))
      DT=-A(1) 
      DS=-A(2) 
      DD=-A(3)
      RETURN
      END 
      SUBROUTINE DUNKEL(XYK,D,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYK(3),XYKK(3)
      IER=0
      DO I=1,3
        XYKK(I)=XYK(I)
        IF(ABS(XYKK(I)).LT.TINY(1.0)) THEN
          XYKK(I)=TINY(1.0)
        ENDIF
      END DO
      CALL AOPT(XYKK(1),XYKK(2),A0,IER)
      IF(IER.NE.0) GOTO 10
      D1 = 40.7 * XYK(3)/A0+1.
      IF(ABS(D1).LT.TINY(1.0)) THEN
        D1=TINY(1.0)
      ENDIF
      D = 10. - 6.1723 * LOG10(D1)
 10   RETURN
      END
      SUBROUTINE DINTSD(XYZ,TSD,NN) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG VON TON,SAETTIUNNG UND DUNKELSTUFE NACH DIN6164
C 
      DIMENSION  XYZ(3),TSD(3),XYK(2) 
      CALL XYZKL(XYZ,XYK)
      CALL AUPT(XYK(1),XYK(2),A0,NN,IER)
      IF(IER.NE.0) GOTO 90
      D1 = 40.7 * XYZ(2)/A0 + 1.
      TSD(3)= 10. - 6.1723 * LOG10(D1)
      GOTO 30 
 90   TSD(3) = 0. 
 30   CALL FTS(XYK(1),XYK(2),TSD(1),TSD(2),NN)
      RETURN
      END 
      SUBROUTINE AUPT ( XKL , YKL ,A0,NN,IER)
      USE MOD6164
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     CHARACTER*12 NOGN
c     CHARACTER*10 MADA
c     CHARACTER*128 FENDAT
      DIMENSION  F(41),  G(41)                                              
      DIMENSION  XI(41),  ETA(41),  SIGMA(41),           T(2,2),   B(2) 
      INTEGER*4  JA,  KA,  J,  KE,  K
      DATA EPS/1.D-12/
C     DATA XI/0.,0.004,.023,.108,.437,1.675,4.672,8.647,  
C    1 12.562,15.924,18.196,19.308,19.671,19.723,19.812,20.388,21.911,
C    2 24.696,28.978,34.858,42.180,50.597,59.581,68.530,76.855,83.925,
C    3 89.234,92.927,95.276,96.637,97.345,97.714,97.885,97.967,98.006,
C    4 98.025,98.033,98.037,98.039,98.040,98.041/ 
C     DATA ETA/3*0.,.002,.011,.048,.17,.432,.875,1.569,2.627, 
C    1  4.245, 6.603,10.004,14.837,21.299,29.233,38.382,48.214,58.055,
C    2 67.202,75.194,81.821,87.137,91.313,94.466,96.656,98.099,98.985,
C    3 99.489,99.748,99.882,99.944,99.973,99.987,99.993,99.996,99.998,
C    4 99.999,99.9996,100./ 
C     DATA SIGMA/0.,.024,.132,.623,2.531,9.755,27.502,51.677, 
C    1  76.673,100.028,118.330,130.521,138.516,144.833,151.275,159.025, 
C    2 168.870,180.999,195.199,210.959,227.448,243.873,259.494,273.766, 
C    3 286.269,296.494,303.993,309.129,312.364,314.229,315.196,315.699, 
C    4 315.932,316.043,316.096,316.121,316.132,316.138,316.141,316.143, 
C    5 316.144/ 
c      DATA LU/91/
c      DATA MADA/'#NLA03.FRB'/
       NWA=NW35
c      NL=NLL(NN)
c      IREC=30+4*NWA 
C     OPEN(LU,FILE=FENDAT(MADA),ACCESS='DIRECT',STATUS='OLD',       
C    &         RECL=IREC,ERR=999,IOSTAT=IER) 
c      OPEN(LU,FILE=FENDAT(MADA),ACCESS='DIRECT',STATUS='OLD',       
c     &         RECL=IREC) 
c      JREC=(NL-1)*3 
c      DO  I=1,3 
c      JREC=JREC+1 
c      READ(LU,REC=JREC,IOSTAT=IER,ERR=999) NOGN,FTKT(I),DAE,               
c     &                  KAL,KSL,KEL,(E(K,I),K=1,NWA) 
C 
c      ENDDO     
c      CLOSE(LU,IOSTAT=IER)
      IER=0
      DO K=1,NWA
         XI(K)=E6164(K,1,NN)-E6164(1,1,NN)
         ETA(K)=E6164(K,2,NN)-E6164(1,2,NN)
         SIGMA(K)=E6164(K,3,NN)-E6164(1,3,NN)
      ENDDO
      DO K=2,NWA
         XI(K)=XI(K)+XI(K-1)
         ETA(K)=ETA(K)+ETA(K-1)
         SIGMA(K)=SIGMA(K)+SIGMA(K-1)
      ENDDO
      DXI=(F6164(1,NN)-XI(NWA))/6.
      DET=(F6164(2,NN)-ETA(NWA))/6.
      DES=(F6164(3,NN)-SIGMA(NWA))/6
      DO K=NWA+1,41
         XI(K)=XI(K-1)+DXI   
         ETA(K)=ETA(K-1)+DET
         SIGMA(K)=SIGMA(K-1)+DES
      ENDDO
      DO K=1,41
         SIGMA(K)=SIGMA(K)+XI(K)+ETA(K)
      ENDDO
C     
C     DO I=1,41
C        WRITE(*,2345) I,XI(I),ETA(I),SIGMA(I)
C     ENDDO
C2345 FORMAT(' I XI ETA SIGMA ',I5,3F8.3)      

      JA = 4
      KA = 2
      DO  I = 1,41
      F(I) = SIGMA(I) - 1. / (XKL * XI(I)+TINY(1.0))
      G(I) = SIGMA(I) - 1. / (YKL * ETA(I)+TINY(1.0))
      ENDDO
 20   DO 55 J = JA,41 
      IF (JA.EQ.4) GO TO 23 
      KA = J
      GO TO 25
   23 KE=MIN0(J-2,33) 
 25   DO 50 K =KA,KE
      T(1,1) = F(K) - F(K-1)
      T(1,2) = F(J-1) - F(J)
      T(2,1) = G(K) - G(K-1)
      T(2,2) = G(J-1) - G(J)
      B(1) = F(J-1) - F(K-1)
      B(2) = G(J-1) - G(K-1)
      IF (JA.EQ.4) GO TO 30 
      B(1) = B(1) + F(41) 
      B(2) = B(2) + G(41) 
  30  CALL PLIGS(2,2,1,2,T,B,5.D-4,DETR,IER)     
      IF(IER.NE.0) GOTO 50
C 
      DO  I=1,2
        IF (B(I).LT.0..OR.B(I).GT.1.) GO TO 50
      ENDDO
      GO TO 60
 50   CONTINUE
      IF (JA.EQ.2) KE = 33
 55   CONTINUE
      IF(JA.NE.2)GOTO 5 
      R=1.0 
      X0=F(41)
      Y0=G(41)
      DO   I=1,40
       IF(G(I).LT.Y0.AND.G(I+1).GT.Y0)GOTO 2
       IF(G(I).GT.Y0.AND.G(I+1).LT.Y0)GOTO 2
       IF(ABS(G(I)-Y0).LE.EPS)GOTO 3
      ENDDO
      RETURN
    2 X1=F(I) 
      Y1=G(I) 
      X2=F(I+1) 
      Y2=G(I+1) 
       U=X2-X1
      V=Y2-Y1 
      GX=X1-X0
      GY=Y1-Y0
      S1=(U**2+V**2)*R**2
      S2=(V*GX-U*GY)**2 
      IF(S1.LT.S2) THEN
        IER=99
        RETURN
      ENDIF
      XS=V*((V*GX-U*GY)/(U**2+V**2+TINY(1.0)))
      XS=X0+XS
      YS=-U*((V*GX-U*GY)/(U**2+V**2+TINY(1.0)))
      YS=YS+Y0
      F(41)=XS
      G(41)=YS
      B(1)=(XS-X1)/(X2-X1)
      E1=ETA(I)+B(1)*(ETA(I+1)-ETA(I))
      E2=ETA(41)
      A0=E2-E1
C      AL1=10.*(I+37.+B(1))
C      AL2=780.
      GOTO 999 
    3 F(41)=F(I)
       G(41)=G(I) 
c       AL1=10.*(I+37.)
c      AL2=780.
       E1=ETA(I)
      E2=ETA(41)
      A0=E2-E1
      GOTO 999
    5 JA = 2
      KE=32 
      GO TO 20
 60   E1 = ETA(K-1) +  B(1)  * (ETA(K) - ETA(K-1))
      E2 = ETA(J-1) +  B(2)  * (ETA(J) - ETA(J-1))
      IF (JA.EQ.2) E2 = E2 + ETA(41)
      A0    = E2 - E1 
c      AL1 = 10. * (K + 36. + B(1))
c      AL2 = 10. * (J + 36. + B(2))
C999  WRITE(1,2222) IER
C2222 FORMAT(' IER = ',I5)
 999  RETURN
      END 
      SUBROUTINE FTSBAM(XAN,YAN,DINT,DINS,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     PROGRAMM ZUR UMWANDLUNG VON XAN, YAN IN T, S UND UMGEKEHRT
      DIMENSION FST (3,27), F4ST(3,4), FINT(3)
      DATA U0,V0/.210526,.473684/
      DATA FST/
     1 54.362,61.884,11.511,53.823,55.079,8.776,52.139,47.296,6.879,
     2 50.35,41.713,6.187,47.95,36.071,5.704,45.233,30.811,6.178,
     3 42.556,26.4,7.622,42.272,24.892,12.083,43.243,24.694,17.95,
     4 47.366,25.878,35.669,51.329,27.298,68.187,39.568,21.754,72.73,
     5 30.537,18.126,73.113,25.365,16.471,73.248,21.936,16.116,73.392,
     6 20.433,16.855,73.526,19.296,20.895,73.815,20.106,27.343,74.006,
     7 23.67,39.286,74.111,24.936,46.507,60.059,27.834,51.605,45.819,
     8 32.824,56.452,33.46,38.783,60.27,24.308,48.637,64.712,16.485,
     9 54.362,61.884,11.511,53.823,55.079,8.776,52.139,47.296,6.879/
C      IF (FMZ.EQ.TS) GO TO 50
        XU=FAKT(1,NN)
        YU=FAKT(2,NN)
        ZU=FAKT(3,NN)
      Y=1.
      X=XAN/YAN
      Z=(1.-XAN-YAN)/YAN
      FXU1=Y*ZU-Z*YU
      FXU2=Z*XU-X*ZU
      FXU3=X*YU-Y*XU
      IF (ABS(FXU1)+ABS(FXU2)+ABS(FXU3).LE.0.05) GO TO 70
      ITON=16
    8 CALL F4(ITON,F4ST,FST)
        IF (F4ST(1,3)*FXU1+F4ST(2,3)*FXU2+F4ST(3,3)*FXU3.GT.0.) GOTO 25
   20 ITON=ITON+1
        GO TO 8
   25 IF (F4ST(1,2)*FXU1+F4ST(2,2)*FXU2+F4ST(3,2)*FXU3.LE.0.) GOTO 10
   27 ITON=ITON-1
        CALL F4(ITON,F4ST,FST)
        GO TO 25
   10 IF (ITON.GE.25) ITON=ITON-24
      MMAX=12
      M=0
      X1=0.
      X2=1.
      GO TO 14
   12 X1=TX
      GO TO 14
   13 X2=TX
   14 M=M+1
      IF (M-MMAX.GT.0) GOTO 30
   15 TX=(X1+X2)*0.5
      CALL POLY3(TX,FINT,F4ST)
      IF(FINT(1)*FXU1+FINT(2)*FXU2+FINT(3)*FXU3.GT.0) THEN
         GOTO 13
      ELSE
         GOTO 12
      ENDIF
   30 DINT=FLOAT(ITON)+TX
      XANZ=X/(X+Y*XU/YU+Z*XU/ZU)
      YANZ=Y/(X*YU/XU+Y+Z*YU/ZU)
      SUMFZ=FINT(1)/XU+FINT(2)/YU+FINT(3)/ZU
      XAN6Z=FINT(1)/(XU*SUMFZ)
      YAN6Z=FINT(2)/(YU*SUMFZ)
      UAN6Z=4.*XAN6Z/(-2.*XAN6Z+12.*YAN6Z+3.)-U0
      VAN6Z=9.*YAN6Z/(-2.*XAN6Z+12.*YAN6Z+3.)-V0
      UANZ=4.*XANZ/(-2.*XANZ+12.*YANZ+3.)-U0
      VANZ=9.*YANZ/(-2.*XANZ+12.*YANZ+3.)-V0
      R=SQRT(ABS(UANZ**2+VANZ**2))
      R6=SQRT(ABS(UAN6Z**2+VAN6Z**2))
      DINS=6.*R/R6
        GOTO 80
  70    DINS=0.
        DINT=25.0
  80    RETURN
      END
      SUBROUTINE POLY3(TX,FINT,F4ST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FINT(3),F4ST(3,4)
      A1=(TX*(1.-TX)*(TX-2.))/6.
      A2=((TX+1.)*(TX-1)*(TX-2.))*0.5
      A3=((TX+1.)*TX*(2.-TX))*0.5
      A4=((TX+1.)*TX*(TX-1.))/6
      DO   J=1,3
      FINT(J)=A1*F4ST(J,1)+A2*F4ST(J,2)+A3*F4ST(J,3)+A4*F4ST(J,4)
      ENDDO
      RETURN
      END
      SUBROUTINE F4(ITON,F4ST,FST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F4ST(3,4),FST(3,27)
      IT=ITON
      IF (ITON.LE.1) IT=ITON+24
      IM1=IT-1
      IP2=IT+2
      DO   K=1,3
      L=1
      DO  I=IM1,IP2
       F4ST(K,L)=FST(K,I)
       L=L+1
      ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE FTS(XKL,YKL,T,S,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
      DIMENSION PHI(241),RC6(241),PHI1(130),PHI2(111),RC61(130),
     1RC62(111) 
      EQUIVALENCE(PHI1(1),PHI(1)),(PHI2(1),PHI(131)),(RC61(1),RC6(1)),
     1(RC62(1),RC6(131))
      DATA PHI1 /0.95839,0.93693,0.91865,0.90181,0.88467,0.86738,0.84938
     1,0.83082,0.81162,0.79145,0.77071,0.75051,0.72864,0.70627,0.68376,0
     2.66110,0.63914,0.61991,0.60222,0.58711,0.57147,0.55679,0.54293,0.5
     32852,0.51516,0.50192,0.48813,0.47405,0.46032,0.44650,0.43283,0.418
     487,0.40506,0.39201,0.37806,0.36487,0.35166,0.33817,0.32485,0.31183
     5,0.29883,0.28582,0.27252,0.25906,0.24649,0.23263,0.21994,0.20650,0
     6.19333,0.18017,0.16675,0.15342,0.14074,0.12776,0.11542,0.10223,0.0
     78956,0.07700,0.06450,0.05164,0.03882,0.02560,0.01320,6.28304,6.269
     894,6.25704,6.24294,6.23035,6.21700,6.20282,6.18928,6.17542,6.16172
     9,6.14880,6.13449,6.12027,6.10647,6.09161,6.07640,6.06096,6.04431,6
     A.02661,6.00540,5.98150,5.95525,5.92205,5.88472,5.84100,5.78740,5.7
     B2117,5.63328,5.54644,5.46825,5.39408,5.32550,5.26079,5.19980,5.141
     C61,5.08981,5.03878,4.99253,4.94841,4.90695,4.86800,4.82941,4.79430
     D,4.75996,4.72771,4.69684,4.66790,4.64026,4.61266,4.58685,4.56160,4
     E.53733,4.51418,4.49146,4.46928,4.44816,4.42843,4.41002,4.39286,4.3
     F7735,4.36374,4.35034,4.33764,4.32495,4.31201,4.29952,4.28723/ 
      DATA PHI2   /4.27580,4.26455,4.25299,4.24222,4.23134,4.22119,4.210
     148,4.19986,4.18951,4.18028,4.17063,4.16160,4.15418,4.14757,4.14254
     2,4.13712,4.13183,4.12576,4.11853,4.11025,4.10067,4.08765,4.07298,4
     3.05767,4.04138,4.02620,4.01034,3.99369,3.97706,3.95984,3.94185,3.9
     42377,3.90521,3.88690,3.86842,3.84948,3.83039,3.81123,3.79279,3.772
     566,3.75222,3.73045,3.70622,3.68065,3.65184,3.62226,3.58805,3.55196
     6,3.51175,3.46327,3.41585,3.36129,3.31650,3.26644,3.21929,3.17077,3
     7.12261,3.07427,3.02326,2.97116,2.91893,2.86527,2.81305,2.76174,2.7
     80960,2.65714,2.60589,2.55351,2.50205,2.44941,2.39746,2.34530,2.292
     975,2.24260,2.19367,2.14485,2.09823,2.05435,2.00883,1.96687,1.92759
     A,1.88970,1.85334,1.81756,1.78316,1.75049,1.71757,1.68549,1.65423,1
     B.62357,1.59285,1.56146,1.52809,1.49469,1.45820,1.42250,1.38743,1.3
     C5163,1.31735,1.28256,1.24862,1.21566,1.18355,1.15209,1.12127,1.091
     D74,1.06273,1.03497,1.00816,0.98254,0.95839/ 
      DATA RC61 /0.18034,0.18128,0.18202,0.18263,0.18336,0.18392,0.18467
     1,0.18534,0.18606,0.18692,0.18771,0.18851,0.18948,0.19055,0.19165,0
     2.19278,0.19408,0.19531,0.19642,0.19743,0.19851,0.19938,0.20027,0.2
     30122,0.20210,0.20300,0.20399,0.20497,0.20590,0.20696,0.20805,0.209
     415,0.21028,0.21140,0.21258,0.21374,0.21503,0.21632,0.21765,0.21892
     5,0.22033,0.22156,        0.22279,0.22394,0.22486,0.22585,0.22673,0
     6.22760,0.22832,0.22918,0.22996,0.23078,0.23146,0.23206,0.23241,0.2
     73288,0.23330,0.23356,0.23375,0.23378,0.23364,0.23314,0.23229,0.230
     897,0.22909,0.22694,0.22455,0.22218,0.21975,0.21717,0.21471,0.21230
     9,0.20991,0.20774,0.20543,0.20306,0.20100,0.19870,0.19645,0.19435,0
     A0.19202,0.18968,0.18683,0.18387,0.18080,0.17698,0.17302,0.16880,0.
     B16424,0.15938,0.15408,0.15023,0.14764,0.14595,0.14505,0.14476,0.14
     C491,0.14553,0.14644,0.14773,0.14925,0.15081,0.15241,0.15399,0.1556
     D0,0.15726,0.15911,0.16105,0.16295,0.16490,0.16697,0.16907,0.17118,
     E0.17330,0.17531,0.17741,0.17950,0.18168,0.18392,0.18609,0.18817,0.
     F19016,0.19201,0.19358,0.19519,0.19679,0.19833,0.19995,0.20146,0.20
     G301/
      DATA RC62   /0.20441,0.20561,0.20688,0.20800,0.20906,0.20996,0.210
     184,0.21174,0.21253,0.21333,0.21397,0.21445,0.21473,0.21491,0.21489
     2,0.21493,0.21483,0.21485,0.21469,0.21457,0.21441,0.21394,0.21339,0
     3.21272,0.21187,0.21060,0.20894,0.20705,0.20487,0.20239,0.19990,0.1
     49740,0.19483,0.19232,0.18966,0.18721,0.18476,0.18243,0.18021,0.178
     502,0.17568,0.17308,0.17050,0.16760,0.16448,0.16120,0.15780,0.15424
     6,0.15074,0.14687,0.14339,0.14010,0.13753,0.13519,0.13313,0.13139,0
     7.12996,0.12883,0.12763,0.12657,0.12574,0.12487,0.12417,0.12343,0.1
     82293,0.12253,0.12238,0.12251,0.12293,0.12357,0.12456,0.12545,0.126
     940,0.12746,0.12862,0.12989,0.13120,0.13257,0.13446,0.13643,0.13838
     A,0.14045,0.14252,0.14465,0.14676,0.14846,0.14998,0.15146,0.15280,0
     B.15418,0.15570,0.15707,0.15851,0.16003,0.16169,0.16356,0.16544,0.1
     C6727,0.16896,0.17050,0.17170,0.17275,0.17362,0.17454,0.17528,0.176
     D09,0.17691,0.17778,0.17860,0.17943,0.18034/ 
C 
C 
C 
      X0=1./3. 
      DIV=100.*XKL/FAKT(1,NN)+100.*(1.-XKL-YKL)/FAKT(3,NN)+YKL  
      X=100.*XKL/(DIV*FAKT(1,NN))-X0  
      Y=YKL/DIV-X0
      S=0.
      IF(ABS(X).LE.1.E-8.AND.ABS(Y).LE.1.E-8)X=1. 
      PHIB= WNKEL(X,Y)
      RR=6.*Y-X 
      DO 20 I=2,240 
      I1 = 305 - I
      IF (I.LT.65) I1 = I1 - 240
      IF (PHIB.LE.PHI(I1)) GO TO 30 
 20   CONTINUE
      PHI(64) = 6.283185
      GO TO 40
 30   PHI(64) = 0.
 40   PHIV = (PHIB-PHI(I1)) / (PHI(I1+1)-PHI(I1)) 
      T = 1. + 0.1 * (I1 - 1 + PHIV)
      RC = RC6(I1) + PHIV * (RC6(I1+1) - RC6(I1)) 
      RK=SQRT(ABS(X**2+Y**2))
      RCR=RC*(6.*SIN(PHIB)-COS(PHIB)) 
      S=6.*RK*(19.+6.*RCR)/(RC*(19.+6.*RR)) 
   70 RETURN
      END 
      SUBROUTINE VXVYVZ(XYZ,VXYZ,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DER VX,VY,VZ-WERTE 
C 
      DIMENSION  XYZ(1),VXYZ(1) 
      DO  10  I=1,3 
      VXYZ(I)= FUV(XYZ(I),I,NN) 
 10   CONTINUE
      RETURN
      END 

      SUBROUTINE AOPT ( XKL , YKL ,A0,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE  F,  G,  AL1,  AL2
      DIMENSION F(41),  G(41)
      DIMENSION  XI(41),  ETA(41),  SIGMA(41),           T(2,2),   B(2)
      REAL(KIND=8) :: AL1,AL2
      INTEGER*4  JA,  KA,  J,  KE,  K 
      DATA XI/0.,0.004,.023,.108,.437,1.675,4.672,8.647,  
     1 12.562,15.924,18.196,19.308,19.671,19.723,19.812,20.388,21.911,
     2 24.696,28.978,34.858,42.180,50.597,59.581,68.530,76.855,83.925,
     3 89.234,92.927,95.276,96.637,97.345,97.714,97.885,97.967,98.006,
     4 98.025,98.033,98.037,98.039,98.040,98.041/ 
      DATA ETA/3*0.,.002,.011,.048,.17,.432,.875,1.569,2.627, 
     1  4.245, 6.603,10.004,14.837,21.299,29.233,38.382,48.214,58.055,
     2 67.202,75.194,81.821,87.137,91.313,94.466,96.656,98.099,98.985,
     3 99.489,99.748,99.882,99.944,99.973,99.987,99.993,99.996,99.998,
     4 99.999,99.9996,100./ 
      DATA SIGMA/0.,.024,.132,.623,2.531,9.755,27.502,51.677, 
     1  76.673,100.028,118.330,130.521,138.516,144.833,151.275,159.025, 
     2 168.870,180.999,195.199,210.959,227.448,243.873,259.494,273.766, 
     3 286.269,296.494,303.993,309.129,312.364,314.229,315.196,315.699, 
     4 315.932,316.043,316.096,316.121,316.132,316.138,316.141,316.143, 
     5 316.144/ 
C 
      IER=0
      JA = 4
      KA = 2
      DO   I = 1,41
      F(I) = SIGMA(I) - 1. / XKL * XI(I)
      G(I) = SIGMA(I) - 1. / YKL * ETA(I)
      ENDDO
 20   DO 55 J = JA,41 
      IF (JA.EQ.4) GO TO 23 
      KA = J
      GO TO 25
   23 KE=MIN0(J-2,33) 
 25   DO 50 K =KA,KE
      T(1,1) = F(K) - F(K-1)
      T(1,2) = F(J-1) - F(J)
      T(2,1) = G(K) - G(K-1)
      T(2,2) = G(J-1) - G(J)
      B(1) = F(J-1) - F(K-1)
      B(2) = G(J-1) - G(K-1)
      IF (JA.EQ.4) GO TO 30 
      B(1) = B(1) + F(41) 
      B(2) = B(2) + G(41) 
  30  CALL PLIGS(2,2,1,2,T,B,5.D-4,DETR,IER)
      IF(IER.NE.0) GOTO 50
C 
      DO 45 I=1,2 
      IF (B(I).LT.0..OR.B(I).GT.1.) GO TO 50
 45   CONTINUE
      GO TO 60
 50   CONTINUE
      IF (JA.EQ.2) KE = 33
 55   CONTINUE
      IF(JA.NE.2)GOTO 5 
      R=1.0 
      X0=F(41)
      Y0=G(41)
       DO 1 I=1,40
       IF(G(I).LT.Y0.AND.G(I+1).GT.Y0)GOTO 2
       IF(G(I).GT.Y0.AND.G(I+1).LT.Y0)GOTO 2
       IF(G(I).EQ.Y0)GOTO 3
    1 CONTINUE
      RETURN
    2 X1=F(I) 
      Y1=G(I) 
      X2=F(I+1) 
      Y2=G(I+1) 
       U=X2-X1
      V=Y2-Y1 
      GX=X1-X0
      GY=Y1-Y0
       S1=(U**2+V**2)*R**2
      S2=(V*GX-U*GY)**2 
      IF(S1.LT.S2) THEN
        IER=99
        RETURN
      ENDIF
      XS=V*((V*GX-U*GY)/(U**2+V**2))
      XS=X0+XS
      YS=-U*((V*GX-U*GY)/(U**2+V**2)) 
      YS=YS+Y0
      F(41)=XS
      G(41)=YS
      B(1)=(XS-X1)/(X2-X1)
      E1=ETA(I)+B(1)*(ETA(I+1)-ETA(I))
      E2=ETA(41)
      A0=E2-E1
      AL1=10.*(I+37.+B(1))
      AL2=780.
      GOTO 999 
    3 F(41)=F(I)
       G(41)=G(I) 
       AL1=10.*(I+37.)
      AL2=780.
       E1=ETA(I)
      E2=ETA(41)
      A0=E2-E1
      GOTO 999
    5 JA = 2
      KE=32 
      GO TO 20
 60   E1 = ETA(K-1) +  B(1)  * (ETA(K) - ETA(K-1))
      E2 = ETA(J-1) +  B(2)  * (ETA(J) - ETA(J-1))
      IF (JA.EQ.2) E2 = E2 + ETA(41)
      A0    = E2 - E1 
      AL1 = 10. * (K + 36. + B(1))
      AL2 = 10. * (J + 36. + B(2))
  999 RETURN
      END 
      SUBROUTINE SCHKLW(T,DE) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION T(3,2)
C 
      C=3.+(2.227*T(2,1))/(1.+0.1136*T(2,1))
      S=1.+0.01*C 
      H1=T(1,1)+0.4*T(2,1)-0.2*T(3,1) 
      H2=T(1,2)+0.4*T(2,2)-0.2*T(3,2) 
      A=1000.*LOG10((H2*T(2,1))/(H1*T(2,2)))
      B=1000.*LOG10((T(3,2)*T(2,1))/(T(3,1)*T(2,2)))
      D=1000.*LOG10(T(2,2)/T(2,1))
      C1=S/(1.+(1.2*C)/H1)
      C2=(0.25*S)/(1.+C/T(3,1)) 
      C3=(0.2*S)/(1.+C/T(2,1))
      DE=((C1*A)**2+(C2*B)**2+(C3*D)**2)**0.5 
C 
      RETURN
      END 
      SUBROUTINE CIE(XYK,UCIE,NN) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
C 
      DIMENSION XYK(3),UCIE(3)
C        U0=4.*XNL/(XNL+15.*YNL+3.*ZNL) 
C        V0=6.*YNL/(XNL+15.*YNL+3.*ZNL) 
C        XNL,ZNL KOORDINATEN DER JEWEILIGEN NORMLICHTART
C 
C 
      DIV=FAKT(1,NN)+15.*FAKT(2,NN)+3.*FAKT(3,NN) 
      U0=4.*FAKT(1,NN)/DIV
      V0=6.*FAKT(2,NN)/DIV
      DIV=-2.*XYK(1)    +12.*XYK(2)    +3.
      UK=4.*XYK(1)    /DIV
      VK=6.*XYK(2)    /DIV
      WK=25.*XYK(3)    **(1./3)-17. 
      UCIE(1)    =13.*WK*(UK-U0)        
      UCIE(2)    =13.*WK*(VK-V0)        
      UCIE(3)    =WK
      RETURN
      END 
      SUBROUTINE NEUWE (V,DEMUN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION V(3,2)
      DOUBLE PRECISION K  
      DATA K/16./ 
      SC=V(3,1)+V(3,2)
      SV=V(2,1)+V(2,2)
      SH=V(1,1)+V(1,2)
      DH=V(1,1)-V(1,2)
      DV=V(2,1)-V(2,2)
      DC=V(3,1)-V(3,2)
      VS=5.995
      WS=3.141593/180.
      BL=3.6*WS 
      WIN=COS(DH*BL)
      FH=(4./(3.-WIN    ))**2 
      FSZ=15.+SQRT(ABS((0.5*SC)**2+K*(0.5*SV-VS)**2))
C     Fehler ???????    
      FSN=5.+SQRT(ABS((0.5*SC)**2+K*(0.5*SV-VS)**2))
      FS=FSZ/FSN
      DEMUN=FS*SQRT(ABS(2.*FH*V(3,1)*V(3,2)*(1.-WIN    )+DC**2+K*DV**2))
      RETURN
      END 
      SUBROUTINE CURO(T,DE,NN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     *********** 
C     T = X, Y, Z 
C     *********** 
C 
C 
C 
      DIMENSION T(3,2), RGB(3,2),ABL(3,2) 
C     ****************************
C     ****************************
C 
C 
      DO  I=1,2
      DO  L=1,3
      RGB(L,I)=100.*T(L,I)/FAKT(L,NN) 
      ENDDO
      G = RGB(2,I)**(1./3.) 
      ABL(1,I) = 106.0*(RGB(1,I)**(1./3.)-G)
      ABL(2,I) = 42.34*(G-RGB(3,I)**(1./3.))
      ABL(3,I) = 25.29*G-18.38
      ENDDO
      DE = SQRT(ABS((ABL(1,1)-ABL(1,2))**2+(ABL(2,1)-ABL(2,2))**2
     1+(ABL(3,1)-ABL(3,2))**2))
      RETURN
      END 
      SUBROUTINE GODL (V,DEGOD) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION V(3,2)
      DOUBLE PRECISION K
      DATA K/16./ 
      DH=V(1,1)-V(1,2)
      DV=V(2,1)-V(2,2)
      DC=V(3,1)-V(3,2)
      WS=3.141593/180.
      BL=3.6*WS 
      WIN=COS(DH*BL)
      DEGOD=SQRT(ABS(2*V(3,1)*V(3,2)*(1.- WIN   )+DC**2+K*DV**2))
      RETURN
      END 
      SUBROUTINE JUDHUN(XYZT,XYZP,DE,DL,DC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     BERECHNUNG DES FARBABSTANDES NACH JUDD-HUNTER (NBS) 
      DIMENSION  XYZT(3),XYZP(3),T(3,2),XYK(2)
C 
      CALL XYZKL(XYZT,XYK)
      T(1,2)=XYK(1)
      T(2,2)=XYK(2)
      CALL XYZKL(XYZP,XYK)
      T(1,1)=XYK(1)
      T(2,1)=XYK(2)
C     SUMT=0. 
C     SUMP=0
C     DO 10 I=1,3 
C     SUMT= SUMT + XYZT(I)
C     SUMP= SUMP + XYZP(I)
C10   CONTINUE
C     DO 20 I=1,2 
C     T(I,2)= XYZT(I)/SUMT
C     T(I,1)= XYZP(I)/SUMP
C20   CONTINUE
      T(3,2)=XYZT(2)
      T(3,1)=XYZP(2)
      CALL JUDD(DE,DL,DC,T)
      RETURN
      END 
      SUBROUTINE JUDD (DE,DL,DC,TK)
      USE MODFAKT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     ******************************
C     TK = X KLEIN, Y KLEIN, Y GROSS
C     ******************************
C 
      DIMENSION            TK(3,2),  DIV(2),  AL(2),  BE(2) 
      YS = (TK(3,1) + TK(3,2)) / 2. 
      DY1=0.
      IF(TK(3,1).GT.0.) DY1=SQRT(ABS(TK(3,1)))
      DY2=0.
      IF(TK(3,2).GT.0.) DY2=SQRT(ABS(TK(3,2)))
      DY = DY1-DY2                           
      YP4=0.
      IF(YS.GT.0.) YP4 = SQRT(SQRT(ABS(YS)))
      DO   I=1,2
      DIV (I) = TK(1,I) + 2.2633 * TK(2,I) + 1.1054 
      AL(I) = (2.4266 * TK(1,I) - 1.3631 * TK(2,I) - 0.3214) / DIV(I) 
      BE(I) = (0.571 * TK(1,I) + 1.2447 * TK(2,I) - 0.5708) / DIV(I)
      ENDDO
      DA = AL(1) - AL(2)
      DB = BE(1) - BE(2)
      WERT = SQRT(ABS(DA**2 + DB**2))
      FG=YS/(YS+KGLOSS+1.0D-40)
      DL=FG*kLIGHT*DY
      DC=FG*ABS(221. * YP4 * WERT)
      DE = SQRT(DC**2 + DL**2)
C      DE = SQRT(ABS((221. * YP4 * WERT)**2 + (10. * DY)**2))
C      FG=YS/(YS+2.5)
C      DE=FG*DE
      RETURN
      END 
      SUBROUTINE SAUMIL(T,DE,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     **************
C     T = VX, VY, VZ
C     **************
C 
      DIMENSION  T(3,2),    XI(3,2)
      IER=0
      DO   J=1,2
      VXY = T(1,J)-T(2,J) 
      IF(VXY.EQ.0.) THEN
         IER=1
         RETURN
      ENDIF
      VZY = T(3,J)-T(2,J) 
      VZY1 = VZY * 0.4
      TT = VZY1 / VXY 
      CT=1./SQRT(ABS(1.+TT**2)*SIGN(1.D0,VXY))
      ST = TT * CT
      XI(1,J) = VXY * (9.37 + 0.79 * CT)
      XI(2,J) = 2. * T(2,J) 
      XI(3,J) = VZY * (3.33 + 0.87 * ST)
      ENDDO
      D = 0.
      DO  I=1,3
      D = D + (XI(I,2) - XI(I,1)) **2
      ENDDO
      DE = SQRT(ABS(D))
      RETURN
      END 
      SUBROUTINE UCS(TK,DE) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     *********************** 
C     TK = CIEUCS KOORDINATEN 
C     *********************** 
C 
      DIMENSION TK(3,2) 
C 
      DE=SQRT(ABS((TK(2,2)-TK(2,1))**2+(TK(3,2)-TK(3,1))**2+(TK(1,2)-
     -TK(1,1))**2))
C 
      RETURN
      END 
      SUBROUTINE FRIELE (T,DE,DL,DC,DCRG,DCYB)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C     **************
C      T = X,Y,Z
C     **************
C 
      DIMENSION  T(3,2),  PQS(3,2),  PQSM(3),  DPQS(3),  PQSQ(3),  YM(4)
      DOUBLE PRECISION  K1,  K2 
      DO   I=1,2
      PQS(1,I) = 0.724 * T(1,I) + 0.382 * T(2,I) - 0.098 * T(3,I) 
      PQS(2,I) = -0.48 * T(1,I) + 1.37  * T(2,I) + 0.1276* T(3,I) 
      PQS(3,I) = 0.686 * T(3,I)
      ENDDO
      DO   J=1,3
      PQSM(J) = (PQS(J,1) + PQS(J,2)) * 0.5 
      DPQS(J) = PQS(J,2) - PQS(J,1) 
      PQSQ(J) = PQSM(J) **2
      ENDDO
      YM(1) = (T(2,1) + T(2,2)) * 0.5 
      DO   I=2,4
      YM(I) = YM(I-1) * YM(1)
      ENDDO
      AQ = 17.3E-6 * (PQSQ(1) + PQSQ(2)) / (1. + 2.73 * PQSQ(1) * 
     1 PQSQ(2) / (PQSQ(1) **2 + PQSQ(2) **2)) 
      BQ = 3.098E-4 * (PQSQ(3) + 0.2015 * YM(2))
      K1 = 0.55669 + 0.049434 * YM(1) - 0.82575E-3 * YM(2) + 0.79172E-5 
     1 * YM(3) - 0.30087E-7 * YM(4) 
      K2 = 0.17548 + 0.027556 * YM(1) - 0.57262E-3 * YM(2) + 0.63893E-5 
     1 * YM(3) - 0.26731E-7 * YM(4) 
      SPQQ = SQRT(ABS(PQSQ(1) + PQSQ(2)))
      DL1 = (PQSM(1) * DPQS(1) + PQSM(2) * DPQS(2)) / SPQQ
      DCYB = PQSM(3) * DL1 / SPQQ - DPQS(3) 
      DCRG = (PQSM(2) * DPQS(1) - PQSM(1) * DPQS(2)) / SPQQ 
      DC1 =       SQRT(ABS(DCRG * DCRG / AQ + DCYB * DCYB / BQ))
      AA=SQRT(ABS(AQ))
      DL2 = 0.279 * DL1 / AA
      DC = K1 * DC1 
      DL = K2 * DL2 
      DE = SQRT(ABS(DC*DC + DL*DL))
      DCRG=K1*DCRG/AA 
      DCYB=K1*DCYB/AA 
      RETURN
      END 
C
C
      SUBROUTINE STKOOR(IGG,DFS,DLAB0,WLAB0,DLAB1,WLAB1,DLABS,WLABS,IGQ)    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DLAB0(*),WLAB0(*),DLAB1(*),WLAB1(*),WLABS(*),DLABS(*) 
      DIMENSION A(10),XK(3),XGR(3),B(10)
      DIMENSION BD(10),AD(10),IK(7)
      DIMENSION AM(10,7)           
      DIMENSION RNORM(1)
      DATA EPS/5.E-4/,RANGIN/1.E-35/              
      DATA ITM/100/
      IGR=IGG
      MZ=10
C
C     DLAB0    GRADIENT IM PUNKT WLAB0 AUF DER FLAECHE
C     WLAB0    PUNKT AUF DER FLAECHE
C     DLAB1    RICHTUNGSVEKTOR IM PUNKT WLAB1 SENKRECHT AUF FLAECHE
C     WLAB1    BELIEBIGER PUNKT
C     WLABS    GESUCHTER PUNKT AUF FLAECHE MIT DLAB1 ALS GRADIENT
C              GERADE VON WLABS NACH WLAB1 HAT RICHTUNG DLAB1
C
C 
      GOTO(10,20,200,30),IGR+1


C
C
C     WLAB0  =  WLAB1
C        
C  
  10  DO I=1,3
      BD(I)=WLAB0(I)-WLAB1(I)
      ENDDO    
      SUU=SNRM2(3,BD,1)
      DO 15 I=1,3
      WLABS(I)=WLAB0(I)
  15  CONTINUE
      IGQ=0
      GOTO 900
C
C
C
C
C     DREIECK MIT GLEICHEM WINKEL DER BASISSEITE FUER DLAB0 UND DLAB1
C
C
C
C
C
C
 
  20  DO I=1,3
      BD(I)=WLAB0(I)-WLAB1(I)
      ENDDO    
      SUU=SNRM2(3,BD,1)
      IF(SUU.LT.EPS) GOTO 10
C
C
C
      SU0=SNRM2(3,DLAB0,1)
      SU1=SNRM2(3,DLAB1,1)
      SU2=SDOT(3,DLAB0,1,DLAB1,1)
      V0=SDOT(3,DLAB0,1,WLAB0,1)
      V1=SDOT(3,DLAB0,1,WLAB1,1)
      COWI=SU2/(SU0*SU1+RANGIN)
c      WRITE(1,7399) SU0,SU1,SU2,V0,V1,COWI
c 7399 FORMAT(' SU0,SU1,SU2,V0,V1,COWI ',6E10.3)
      IF(ABS(COWI-1.).LT.1.) THEN
          ALPH=(V0-V1)/SU2          
c          WRITE(1,7376) (V0-V1)/SU2,(V0-V1)/SU0,(V0-V1)/SU1
 7376     FORMAT(' V0 - V1 ',3E15.5)
      ELSE
          V2=SDOT(3,DLAB1,1,WLAB0,1)
          V3=SDOT(3,DLAB1,1,WLAB1,1)
          ALPH=((V3-V2)*SU0+(V0-V1)*SU1)/((SU2-SU0*SU1)*SU1)
c          WRITE(1,7499) V0,V1,V2,V3
      ENDIF
      DO 25 I=1,3
      WLABS(I)=WLAB1(I)+ALPH*DLAB1(I)
  25  CONTINUE
      IGQ=1
c      WRITE(1,7599) ALPH
c 7499 FORMAT(' V0 V1 V2 V3  ',45E14.5)
c 7599 FORMAT(' ALPH ',E14.5)
      GOTO 900
C
C
C     ZYLINDERKOORDINATEN (Z,R)
C
C
C
C     A(1)     AD(1)    A0 
C     A(2)     AD(2)    A1 
C     A(3)              A2
C     A(4)              A3 
C     A(5)     AD(3)    A11
C     A(6)     AD(4)    A22
C     A(7)              A33
C     A(8)              A12
C     A(9)              A13
C     A(10)             A23
C              AD(5)    ALPH1
C
C
 
 200  DO 212 I=1,3
      BD(I)=WLAB0(I)-WLAB1(I)
 212  CONTINUE
      SUU=SNRM2(3,BD,1)
      IF(SUU.LT.EPS) GOTO 10
C
      N=5
      M=5 
      DO 255 I=1,3
      WLABS(I)=WLAB1(I)
 255  CONTINUE
      WLR0=SQRT(ABS(WLAB0(2)**2+WLAB0(3)**2))
      WLR1=SQRT(ABS(WLAB1(2)**2+WLAB1(3)**2))
      PHI0=ATAN2(WLAB0(3),WLAB0(2))
      PHI1=ATAN2(WLAB1(3),WLAB1(2))
      ALR0=DLAB0(2)*COS(PHI0)+DLAB0(3)*SIN(PHI0)
      ALR1=DLAB1(2)*COS(PHI1)+DLAB1(3)*SIN(PHI1)
C     WRITE(*,4388) WLR0,ALR0,WLR1,ALR1,PHI0,PHI1
C4388 FORMAT(' WLR0,ALR0,WLR1,ALR1,PHI0,PHI1 '/1X,6F12.4)
      DO 250 J=1,M
      AD(J)=1.
 250  CONTINUE
C
      IT=0
C
C
C
 260  DO  I=1,MZ
      B(I)=0.
      DO   J=1,N
      AM(I,J)=0.
      ENDDO
      ENDDO
      DO   I=1,3
      BD(I)=WLABS(I)     
      ENDDO
      A(1)=AD(1)
      A(2)=AD(2)
      A(3)=0.
      A(4)=0.
      A(5)=AD(3)
      A(6)=AD(4)
      A(7)=0.   
      A(8)=0.     
      A(9)=0.
      A(10)=0.
      XK(1)=WLAB1(1)
      XK(2)=WLR1     
      XK(3)=0.       
      XGR(1)=DLAB1(1)
      XGR(2)=ALR1    
      XGR(3)=0.       
      CALL PWLABS(A,XK,XGR,XK,*200)
C      SUS=POLY(XK,A)
      WLRS=XK(2)
C     WRITE(*,6129) WLRS,XK(1),SUS     
C6129 FORMAT(' WLRS Z SUS ',4F10.4)
      WLABS(1)=XK(1)           
      WLABS(2)=XK(2)*COS(PHI1)
      WLABS(3)=XK(2)*SIN(PHI1)
C     WRITE(*,4389) (WLABS(I),I=1,3)
C4389 FORMAT(' WLABS  ',3E12.4)
      SUU=0.    
      DO 290 I=1,3
      SUU=SUU+(WLABS(I)-BD(I))**2
 290  CONTINUE
      SUU=SQRT(ABS(SUU))
      IT=IT+1
      IF(IT.GE.ITM.OR.SUU.LT.EPS) THEN
            IGQ=2
            GOTO 900
      ENDIF
C
C
C     AUFBAU DER MATRIX ZUR BESTIMMUNG DER KOEFFIZIENTEN
C
C
C
      AM(1,1)=1.
      AM(1,2)=WLAB0(1)
      AM(1,3)=WLAB0(1)**2
      AM(1,4)=WLR0**2
      AM(2,4)=2.*WLR0      
      AM(3,2)=1.
      AM(3,3)=2.*WLAB0(1)
      AM(4,2)=1.
      AM(4,4)=2.*WLRS     
      AM(4,5)=-ALR1
      AM(5,2)=1.
      AM(5,3)=2.*WLABS(1)
      AM(5,5)=-DLAB1(1)     
      B(2)=ALR0     
      B(3)=DLAB0(1)
C      FAF=SNRM2(4,B,1)
C
C
C
C     DO 300 I=1,M
C     WRITE(*,4444) (AM(I,J),J=1,N),B(I) 
C300  CONTINUE
C4444 FORMAT(1X,7E11.4)
      CALL HFTI(AM,MZ,M,N,B,10,1,EPS,KRANK,RNORM,BD,AD,IK)
C     WRITE(*,7699) KRANK
      IF(KRANK.LT.N) THEN
         IGR=1
         GOTO 20
      ENDIF
C7699 FORMAT(' KRANK ',I7)
      DO 330 I=1,N
      AD(I)=B(I)
 330  CONTINUE
C     WRITE(*,7199) (AD(I),I=1,N)
C7199 FORMAT(' AD ',7E10.3)
C     DO 340 I=1,M
C     WRITE(*,4444) (AM(I,J),J=1,N)       
C340  CONTINUE
      GOTO 260
C
C
C
C
C
C     FLAECHE ZWEITER ORDNUNG
C
C
C
C     A(1)     AD(1)    A0 
C     A(2)              A1 
C     A(3)              A2
C     A(4)     AD(2)    A3 
C     A(5)     AD(3)    A11
C     A(6)     AD(4)    A22
C     A(7)     AD(5)    A33
C     A(8)     AD(6)    A12
C     A(9)              A13
C     A(10)             A23
C              AD(7)    ALPH1
C
 
  30  DO  I=1,3
      BD(I)=WLAB0(I)-WLAB1(I)
      ENDDO    
      SUU=SNRM2(3,BD,1)
      IF(SUU.LT.EPS) GOTO 10
C
      N=7
      M=7 
      DO 50 J=1,M
      AD(J)=1.
  50  CONTINUE
      DO 55 I=1,3
      WLABS(I)=WLAB1(I)
  55  CONTINUE
C
      IT=0
C
C
C
  60  DO 70 I=1,MZ
      B(I)=0.
      DO 70 J=1,N
      AM(I,J)=0.
  70  CONTINUE
      DO 80 I=1,3
      BD(I)=WLABS(I)     
  80  CONTINUE
      A(1)=AD(1)
      A(2)=0.
      A(3)=0.
      A(4)=AD(2)
      A(5)=AD(3)
      A(6)=AD(4)
      A(7)=AD(5)
      A(8)=AD(6)
      A(9)=0.
      A(10)=0.
      XK(1)=WLAB1(2)
      XK(2)=WLAB1(3)
      XK(3)=WLAB1(1)
      XGR(1)=DLAB1(2)
      XGR(2)=DLAB1(3)
      XGR(3)=DLAB1(1)
      CALL PWLABS(A,XK,XGR,XK,*200)
      WLABS(1)=XK(3)
      WLABS(2)=XK(1)
      WLABS(3)=XK(2)
      SUU=0.
      DO 90 I=1,3
      SUU=SUU+(WLABS(I)-BD(I))**2
 90   CONTINUE
      SUU=SQRT(ABS(SUU))
      IT=IT+1
      IF(IT.GE.ITM.OR.SUU.LT.EPS) THEN      
            IGQ=3
            GOTO 900
      ENDIF
C
C
C     AUFBAU DER MATRIX ZUR BESTIMMUNG DER KOEFFIZIENTEN
C
C
C
      AM(1,1)=1.
      AM(1,2)=WLAB0(1)
      AM(1,3)=WLAB0(2)**2
      AM(1,4)=WLAB0(3)**2
      AM(1,5)=WLAB0(1)**2
      AM(1,6)=2.*WLAB0(2)*WLAB0(3)
      AM(2,3)=2.*WLAB0(2)
      AM(2,6)=2.*WLAB0(3)
      AM(3,4)=2.*WLAB0(3)
      AM(3,6)=2.*WLAB0(2)
      AM(4,2)=1.
      AM(4,5)=2.*WLAB0(1)
      AM(5,3)=2.*WLABS(2)
      AM(5,6)=2.*WLABS(3)
      AM(5,7)=-DLAB1(2)
      AM(6,4)=2.*WLABS(3)
      AM(6,6)=2.*WLABS(2)
      AM(6,7)=-DLAB1(3)
      AM(7,2)=1.
      AM(7,5)=2.*WLABS(1)
      AM(7,7)=-DLAB1(1)
      B(2)=DLAB0(2)
      B(3)=DLAB0(3)
      B(4)=DLAB0(1)
C
C
C
C     DO 100 I=1,M
C     WRITE(*,4444) (AM(I,J),J=1,N),B(I) 
C100  CONTINUE
C4444 FORMAT(1X,7E11.4)


      CALL HFTI(AM,MZ,M,N,B,10,1,EPS,KRANK,RNORM,BD,AD,IK)
C     WRITE(*,7699) KRANK
      IF(KRANK.LT.N) THEN
         IGR=2
         GOTO 200
      ENDIF
C7699 FORMAT(' KRANK ',I7)
      DO 130 I=1,N
      AD(I)=B(I)
 130  CONTINUE
C     WRITE(*,7199) (AD(I),I=1,N)
C7199 FORMAT(' AD ',7E10.3)
C     DO 140 I=1,M
C     WRITE(*,4444) (AM(I,J),J=1,N)       
C140  CONTINUE
      GOTO 60
 380  DO 385 I=1,3
      WLABS(I)=WLAB1(I)
 385  CONTINUE
C
 900  DO 400 I=1,3      
      DLABS(I)=WLABS(I)-WLAB1(I)             
 400  CONTINUE         
            WRITE(1,2886) (DLABS(K),K=1,3)
 2886       FORMAT(' DLABS ',3F10.4)
      DF1=SNRM2(3,DLAB1,1)
      DF2=SNRM2(3,DLAB0,1)
      DFF=SDOT(3,DLAB1,1,DLABS,1)
      DFS=0.
      IF(ABS(DF1).GT.RANGIN) DFS=DFF/DF1**2     
            WRITE(1,6599) DF1,DFS,DFF,DFF/(DF1*DF2)          
 6599       FORMAT(' DF1,DFS,DFF,DFS2 ',4E12.4)
      RETURN
      END
      SUBROUTINE PWLABS(A,XK,XG,XS,*)       
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(*),XK(*),XG(*),XS(*)                     
      DATA EPS/5.E-5/
C
C
C
C     A(1)              A0 
C     A(2)              A1 
C     A(3)              A2
C     A(4)              A3 
C     A(5)              A11
C     A(6)              A22
C     A(7)              A33
C     A(8)              A12
C     A(9)              A13
C     A(10)             A23
C                       ALPH1
C
      ET0=A(1)+A(2)*XK(1)+A(3)*XK(2)+A(4)*XK(3)
     &   +A(5)*XK(1)**2+A(6)*XK(2)**2+A(7)*XK(3)**2
     &   +2.*(A(8)*XK(1)*XK(2)+A(9)*XK(1)*XK(3)+A(10)*XK(2)*XK(3))
      ET1=A(2)*XG(1)+A(3)*XG(2)+A(4)*XG(3)
     &   +2.*(A(5)*XK(1)*XG(1)+A(6)*XK(2)*XG(2)+A(7)*XK(3)*XG(3)  
     &   +A(8)*(XG(1)*XK(2)+XG(2)*XK(1))
     &   +A(9)*(XG(1)*XK(3)+XG(3)*XK(1))
     &  +A(10)*(XG(2)*XK(3)+XG(3)*XK(2)))
      ET2=A(5)*XG(1)**2+A(6)*XG(2)**2+A(7)*XG(3)**2
     &   +2.*(A(8)*XG(1)*XG(2)+A(9)*XG(1)*XG(3)+A(10)*XG(2)*XG(3))
C
C
C
C    
      IF(ABS(ET2).GT.EPS) THEN
         WU=SQRT(ABS(ET1**2-4.*ET0*ET2))
         AL1=(-ET1+WU)/(2.*ET2)
         AL2=(-ET1-WU)/(2.*ET2)
         ALPH=AL1
         IF(ABS(ALPH).GT.ABS(AL2)) ALPH=AL2
      ELSE IF(ABS(ET1).GT.EPS) THEN
         ALPH=-ET0/ET1
      ELSE
         RETURN 1
      ENDIF
      DO 10 I=1,3
      XS(I)=XK(I)+ALPH*XG(I)
 10   CONTINUE
C     WRITE(*,6565) (A(I),I=1,10)
C6565 FORMAT(' A (I )'/(1X,5E13.4))     
C     WRITE(*,6566) (XS(I),I=1,3)
C6566 FORMAT(' XS ',3F10.4)
      RETURN
      END
      DOUBLE PRECISION FUNCTION POLY(X,A)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(*),X(*)
      SUU=A(1)+A(2)*X(1)+A(3)*X(2)+A(4)*X(3)
     &   +A(5)*X(1)**2+A(6)*X(2)**2+A(7)*X(3)**2
     &   +2.*(A(8)*X(1)*X(2)+A(9)*X(1)*X(3)+A(10)*X(2)*X(3))
      POLY=SUU
      RETURN
      END
C
      SUBROUTINE FAEAEQ(FAESS,FAET,FP,CH1,CH2,FAE)           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CH1(*),CH2(*),FP(*),FAE(*),FAET(*)
      DATA TOL/1.E-4/
      WRITE(6,6666) FAESS,FAET(1),FAET(2),FP(1),FP(2)
      WRITE(6,6667) (CH1(J),J=1,3)
      WRITE(6,6668) (CH2(J),J=1,3)
 6666 FORMAT(' FAESS FAET FP ',5F12.5)
 6667 FORMAT(' CH1           ',3F12.5)
 6668 FORMAT(' CH2           ',3F12.5)
C
C     FAE(1)      FAERBEAEQUIVALENT WEISSARTIG
C     FAE(2)      FAERBEAEQUIVALENT SCHWARZARTIG
C
C
C     BERECHNUNG DER REL. FARBSTAERKE DER PROBE
C
C
      DET=FP(1)-FP(2)
      IF(ABS(DET).LT.TOL) THEN

C             
C             
C
C
C     FARBSTAERKE (WEISSARTIG)  =  FARBSTAERKE (SCHWARZARTIG)
C
C
C
                 GAM=FAET(1)/FAET(2)
                 DEN=(FP(1)-GAM)*CH1(1)
                 IF(ABS(DEN).GT.0.) THEN
                        DEZ=CH1(2)-FAESS*CH1(3)   
                        FAE(2)=DEZ/DEN
                 ELSE
                        FAE(2)=FAET(2)   
                 ENDIF
                 FAE(1)=GAM*FAE(2)
      ELSE
C
C              
                 DA1=CH1(2)*CH2(1)      -CH2(2)*CH1(1)
                 DA2=CH1(3)*CH2(1)*FP(1)-CH2(3)*CH1(1)*FP(2)
                 DA3=CH1(2)*CH2(1)*FP(2)-CH2(2)*CH1(1)*FP(1)
                 DA4=CH1(3)*CH2(1)      -CH2(3)*CH1(1)
                 DEN=DET*CH1(1)*CH2(1)
                 FAE(1)=(DA1-FAESS*DA2)/DEN
                 FAE(2)=(DA3-FAESS*FP(1)*FP(2)*DA4)/DEN
      ENDIF
      FAE(1)=FAE(1)/FAET(1)
      FAE(2)=FAE(2)/FAET(2)
      RETURN
      END
C
      SUBROUTINE RWAKI(R,NWA,XP,RP)
      USE MODILLU
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::PUF
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::X,ABW
      DIMENSION R(*),XP(*),RP(*)
      ALLOCATE(PUF(9*NWE+20),X(NWE),ABW(NWE),STAT=ISTAT)
      DO I=1,NWE
         ABW(I)=-1.
         X(I)=WSOL(I)
      ENDDO
      MERK=0
      P=1
      M=3
      CALL AKINT(PUF,NWE,X,R,IER,MERK,M,P,ABW)
      CALL INTAKI(PUF,NWE,X,NWA,XP,RP)
      DEALLOCATE(PUF,X,ABW)
      RETURN
      END       
      SUBROUTINE DINNB(RP,DICHTE,DIMAX,KW,RU,IRT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DICHTE(*),RP(*),RU(*)
C
C     GEWICHTSFAKTOREN FUER FILTER ZUR BERECHNUNG DER OPTISCHEN DICHTE
C     NACH DINNB (Fa.Gretag Herrn Dr. Glatz Oktober 1993)
C     WELLENLAENGENBEREICH:  380nm bis 730nm in 10nm Schritten  
      DIMENSION OGEW(36,4),DIUNT(4)
      DATA RUU/1.D0/
      DATA OGEW/
C     b                                                     
     &          0.00000035,0.00000134,0.00000540,0.00001983,
     &          0.00007786,0.00026526,0.00061185,0.00116551,
     &          0.00210278,0.00361523,0.00621615,0.01039468,
     &          0.01792156,0.03079947,0.04771259,0.06322446,
     &          0.07600303,0.08568414,0.09222735,0.09457740,
     &          0.09228556,0.08541422,0.07547279,0.06357111,
     &          0.05071969,0.03705093,0.02562576,0.01636754,
     &          0.00972286,0.00530251,0.00292190,0.00146057,
     &          0.00075382,0.00039618,0.00020415,0.00010417,
C     c                                                     
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          1.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
C     m                                                     
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,1.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
C     y                                                     
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,1.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000/
C
C
C
C
C
C
C
      DO J=1,4
         DICHTE(J)=0.
         DIUNT(J)=0.
         DO I=1,36
            DICHTE(J)=DICHTE(J)+RP(I)*OGEW(I,J)
            DIUNT(J)=DIUNT(J)+RU(I)*OGEW(I,J)
         ENDDO
      ENDDO
      DIMAX=0.
      DO J=1,4
         IF(DIUNT(J).GT.EPSILON(1.D0)) THEN
            DII=DIUNT(J)
            DIUNT(J)=AABEE(DII,KW,RUU,IRT)
         ELSE
            DIUNT(J)=1./EPSILON(1.)
         ENDIF
         IF(DICHTE(J).GT.EPSILON(1.D0)) THEN
            DII=DICHTE(J)
            DICHTE(J)=AABEE(DII,KW,RUU,IRT)
         ELSE
            DICHTE(J)=1./EPSILON(1.)
         ENDIF
         DICHTE(J)=DICHTE(J)-DIUNT(J)
         IF(DICHTE(J).LT.0.D0) DICHTE(J)=0.D0
         IF(DIMAX.LT.DICHTE(J)) DIMAX=DICHTE(J)
      ENDDO
      RETURN
      END
C
C
      SUBROUTINE DINXX(RP,DICHTE,DIMAX,KW,RU,IRT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DICHTE(*),RP(*),RU(*)
C
C     GEWICHTSFAKTOREN FUER FILTER ZUR BERECHNUNG DER OPTISCHEN DICHTE
C     NACH DIN   (Fa.Gretag Herrn Dr. Glatz Oktober 1993)
C     WELLENLAENGENBEREICH:  380nm bis 730nm in 10nm Schritten  
      DIMENSION OGEW(36,4),DIUNT(4)
      DATA RUU/1.D0/
      DATA OGEW/
C     b                                                     
     &          0.00000035,0.00000134,0.00000540,0.00001983,
     &          0.00007786,0.00026526,0.00061185,0.00116551,
     &          0.00210278,0.00361523,0.00621615,0.01039468,
     &          0.01792156,0.03079947,0.04771259,0.06322446,
     &          0.07600303,0.08568414,0.09222735,0.09457740,
     &          0.09228556,0.08541422,0.07547279,0.06357111,
     &          0.05071969,0.03705093,0.02562576,0.01636754,
     &          0.00972286,0.00530251,0.00292190,0.00146057,
     &          0.00075382,0.00039618,0.00020415,0.00010417,
C     c                                                     
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.03437445,0.09716977,0.20534870,
     &          0.27037203,0.22717124,0.11896219,0.04660162,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
C     m                                                     
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.03059733,0.09004386,0.19764395,0.26964768,
     &          0.23431031,0.12664206,0.05111481,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
C     y                                                     
     &          0.00000000,0.00000000,0.02382418,0.07635830,
     &          0.18139414,0.26636866,0.24790533,0.14291267,
     &          0.06123672,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000/
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
      DO J=1,4
         DICHTE(J)=0.
         DIUNT(J)=0.
         DO I=1,36
            DICHTE(J)=DICHTE(J)+RP(I)*OGEW(I,J)
            DIUNT(J)=DIUNT(J)+RU(I)*OGEW(I,J)
         ENDDO
      ENDDO
      DIMAX=0.
      DO J=1,4
         IF(DIUNT(J).GT.EPSILON(1.D0)) THEN
            DII=DIUNT(J)
            DIUNT(J)=AABEE(DII,KW,RUU,IRT)
         ELSE
            DIUNT(J)=1./EPSILON(1.)
         ENDIF
         IF(DICHTE(J).GT.EPSILON(1.D0)) THEN
            DII=DICHTE(J)
            DICHTE(J)=AABEE(DII,KW,RUU,IRT)
         ELSE
            DICHTE(J)=1./EPSILON(1.)
         ENDIF
         DICHTE(J)=DICHTE(J)-DIUNT(J)
         IF(DICHTE(J).LT.0.D0) DICHTE(J)=0.D0
         IF(DIMAX.LT.DICHTE(J)) DIMAX=DICHTE(J)
      ENDDO
      RETURN
      END
C
C
C
      SUBROUTINE ANSIT(RP,DICHTE,DIMAX,KW,RU,IRT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DICHTE(*),RP(*),RU(*)
C
C     GEWICHTSFAKTOREN FUER FILTER ZUR BERECHNUNG DER OPTISCHEN DICHTE
C     NACH ANSIT (Fa.Gretag Herrn Dr. Glatz Oktober 1993)
C     WELLENLAENGENBEREICH:  380nm bis 730nm in 10nm Schritten  
      DIMENSION OGEW(36,4),DIUNT(4)
      DATA RUU/1.D0/
      DATA OGEW/
C     b                                                     
     &          0.00000035,0.00000134,0.00000540,0.00001983,
     &          0.00007786,0.00026526,0.00061185,0.00116551,
     &          0.00210278,0.00361523,0.00621615,0.01039468,
     &          0.01792156,0.03079947,0.04771259,0.06322446,
     &          0.07600303,0.08568414,0.09222735,0.09457740,
     &          0.09228556,0.08541422,0.07547279,0.06357111,
     &          0.05071969,0.03705093,0.02562576,0.01636754,
     &          0.00972286,0.00530251,0.00292190,0.00146057,
     &          0.00075382,0.00039618,0.00020415,0.00010417,
C     c                                                     
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00019168,
     &          0.00143761,0.09584052,0.31946840,0.27154814,
     &          0.17570762,0.07986710,0.03194684,0.01597342,
     &          0.00479203,0.00159734,0.00095841,0.00047920,
     &          0.00015973,0.00003195,0.00000000,0.00000000,
C     m                                                     
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00190006,0.00950029,
     &          0.05700171,0.12920388,0.17480524,0.19000570,
     &          0.16720502,0.12540376,0.07980239,0.04180125,
     &          0.01710051,0.00475014,0.00133004,0.00017101,
     &          0.00001900,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
C     y                                                     
     &          0.00042192,0.00210959,0.00843834,0.02390864,
     &          0.05625563,0.08438344,0.11532403,0.13220072,
     &          0.14063906,0.13641989,0.11954320,0.09141539,
     &          0.05625563,0.02531503,0.00703195,0.00028128,
     &          0.00005626,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000/
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
      DO J=1,4
         DICHTE(J)=0.
         DIUNT(J)=0.
         DO I=1,36
            DICHTE(J)=DICHTE(J)+RP(I)*OGEW(I,J)
            DIUNT(J)=DIUNT(J)+RU(I)*OGEW(I,J)
         ENDDO
      ENDDO
      DIMAX=0.
      DO J=1,4
         IF(DIUNT(J).GT.EPSILON(1.D0)) THEN
            DII=DIUNT(J)
            DIUNT(J)=AABEE(DII,KW,RUU,IRT)
         ELSE
            DIUNT(J)=1./EPSILON(1.)
         ENDIF
         IF(DICHTE(J).GT.EPSILON(1.D0)) THEN
            DII=DICHTE(J)
            DICHTE(J)=AABEE(DII,KW,RUU,IRT)
         ELSE
            DICHTE(J)=1./EPSILON(1.)
         ENDIF
         DICHTE(J)=DICHTE(J)-DIUNT(J)
         IF(DICHTE(J).LT.0.D0) DICHTE(J)=0.D0
         IF(DIMAX.LT.DICHTE(J)) DIMAX=DICHTE(J)
      ENDDO
      RETURN
      END
C
C
C
      SUBROUTINE ANSIA(RP,DICHTE,DIMAX,KW,RU,IRT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DICHTE(*),RP(*),RU(*)
C
C
C     GEWICHTSFAKTOREN FUER FILTER ZUR BERECHNUNG DER OPTISCHEN DICHTE
C     NACH ANSIA (Fa.Gretag Herrn Dr. Glatz oktober 1993)
C     WELLENLAENGENBEREICH:  380nm bis 730nm in 10nm Schritten  
      DIMENSION OGEW(36,4),DIUNT(4)
      DATA RUU/1.D0/
      DATA OGEW/
C     b                                                     
     &          0.00000035,0.00000134,0.00000540,0.00001983,
     &          0.00007786,0.00026526,0.00061185,0.00116551,
     &          0.00210278,0.00361523,0.00621615,0.01039468,
     &          0.01792156,0.03079947,0.04771259,0.06322446,
     &          0.07600303,0.08568414,0.09222735,0.09457740,
     &          0.09228556,0.08541422,0.07547279,0.06357111,
     &          0.05071969,0.03705093,0.02562576,0.01636754,
     &          0.00972286,0.00530251,0.00292190,0.00146057,
     &          0.00075382,0.00039618,0.00020415,0.00010417,
C     c                                                     
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00126881,0.14917082,
     &          0.34292142,0.25479061,0.13785441,0.06618383,
     &          0.02722796,0.01220800,0.00500665,0.00204724,
     &          0.00082987,0.00031892,0.00012345,0.00004801,
C     m                                                     
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00014374,0.02120959,0.19324999,0.31942146,
     &          0.25713428,0.14086487,0.05302396,0.01296851,
     &          0.00186223,0.00012138,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
C     y                                                     
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.01310135,0.21584478,0.32753380,0.26759512,
     &          0.13658160,0.03602872,0.00319345,0.00012119,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000,
     &          0.00000000,0.00000000,0.00000000,0.00000000/
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
      DO J=1,4
         DICHTE(J)=0.
         DIUNT(J)=0.
         DO I=1,36
            DICHTE(J)=DICHTE(J)+RP(I)*OGEW(I,J)
            DIUNT(J)=DIUNT(J)+RU(I)*OGEW(I,J)
         ENDDO
      ENDDO
      DIMAX=0.
      DO J=1,4
         IF(DIUNT(J).GT.EPSILON(1.D0)) THEN
            DII=DIUNT(J)
            DIUNT(J)=AABEE(DII,KW,RUU,IRT)
         ELSE
            DIUNT(J)=1./EPSILON(1.)
         ENDIF
         IF(DICHTE(J).GT.EPSILON(1.D0)) THEN
            DII=DICHTE(J)
            DICHTE(J)=AABEE(DII,KW,RUU,IRT)
         ELSE
            DICHTE(J)=1./EPSILON(1.)
         ENDIF
         DICHTE(J)=DICHTE(J)-DIUNT(J)
         IF(DICHTE(J).LT.0.D0) DICHTE(J)=0.D0
         IF(DIMAX.LT.DICHTE(J)) DIMAX=DICHTE(J)
      ENDDO
      RETURN
      END
      SUBROUTINE XYZNOR(XYZH,RT,RP,K,GKK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RT(*),RP(*)
      DIMENSION XYZP(3),XYZT1(3),XYZP1(3),XYZH(*)
      RIN=10.*TINY(1.D0)
      CALL NOGXX(XYZP,K,RP,GKK)                             
      CALL NOGXX(XYZP1,1,RP,GKK)
      CALL NOGXX(XYZT1,1,RT,GKK)                             
      DO L=1,3                    
         XYZH(L)=XYZP(L)*XYZT1(L)/(XYZP1(L)+RIN)
      ENDDO
      RETURN
      END
 
C
C
C
C******************************************************************************
      SUBROUTINE EMDEDD(JABST,NDI,RF,RC,FDI,DEDW,DEDS,GKK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RF(NDI,*),RC(NDI,*)
      DIMENSION XYZ(3,2),DE(2)
C
C 
C     
C
C
C     BERECHNUNG DER EMPFINDLICHKEIT
      DO J=1,2
         CALL NOGXX(XYZ(1,1),1,RF(1,J),GKK)
         CALL NOGXX(XYZ(1,2),1,RC(1,J),GKK)
         CALL DELABAL(JABST,XYZ(1,1),XYZ(1,2),DE(J),DL,DC,DH,DA,DB,1)
      ENDDO
      DEDW=DE(1)/FDI
      DEDS=DE(2)/FDI
      RETURN
      END

C******************************************************
C
      SUBROUTINE HUEKOR(nmax,iplotanz,icm,h_wert)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension h_wert(nmax,*)
      logical flag(2)
      flag(1)=.false.
      flag(2)=.false.
C     Pruefen, ob HUE-WERTE im 1.(flag(2)=.true.) 
C     und 4.(flag(1)=.true.) Quadranten vorkommen
      do i=1,iplotanz
             do j=1,icm
                  if(h_wert(i,j).GT.180..OR.h_wert(i,j).LT.0.) then
                       flag(1)=.true.
                  end if
                  if(h_wert(i,j).LT.180.) then
                       flag(2)=.true.
                  end if
             end do
      end do
      IF(flag(1).and.flag(2)) then
         do i=1,iplotanz
             do j=1,icm
                  if(h_wert(i,j).GT.180.) then
                       h_wert(i,j)=h_wert(i,j)-360.
                  end if
             end do
        end do
      endif
      return
      end
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
      SUBROUTINE HELMHOLZ(XYK,XYD,ALAMD,PE,PC,NN,IER)
      USE MOD6164
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYKR(2),XYKP(2),DXYK(2),XYKLAM(2),XYD(*),XYK(*)
      DIMENSION XYKH(2),DXYKH(2)
      DIMENSION XYMIN(2),XYMAX(2)
      EXTERNAL FHELM
      DATA EPS/1.D-6/
C
C
C
C     x0,y0 (UNBUNTPUNKT)
C
C
      IER=0
      CALL XYZKL(F6164(1,NN),XY0)
C

C
C     BESTIMMUMG VON ALMIN UNS ALMAX
C
C
C
C     ALAMIN BESTIMMEN
C
C
C
      ND=NN
      CALL HELMLAM(WSOL35(10),XYKR,ND)
      CALL HELMLAM(WSOL35(9),XYKP,ND)
      DXYK(1)=XYKR(1)-XYKP(1)
      DXYK(2)=XYKR(2)-XYKP(2)
      DELTA=SQRT(DXYK(1)**2+DXYK(2)**2)
      DO I=8,1,-1
      XYKR(1)=XYKP(1)
      XYKR(2)=XYKP(2)
      CALL HELMLAM(WSOL35(I),XYKP,ND)
      DXYKH(1)=XYKP(1)-XYKR(1)
      DXYKH(2)=XYKP(2)-XYKR(2)
      DELTH=SQRT(DXYKH(1)**2+DXYKH(2)**2)
C
C     PRÜFUNGEN
C
C
      IF(DELTH.GT.DELTA) THEN
         EXIT
      ENDIF
      IF(DXYKH(1).LT.0.) THEN
         EXIT
      ENDIF
      IF(DXYKH(2).GT.0.) THEN
         EXIT
      ENDIF
      DELTA=DELTH
      END DO
      I=I+1
      ALAMIN=WSOL35(I)
      CALL HELMLAM(ALAMIN,XYMIN,ND)
      DXYKH(1)=XYMIN(1)-XY0(1)
      DXYKH(2)=XYMIN(2)-XY0(1)
      PHMIN=WNGRD(DXYKH(1),DXYKH(2))

C
C
C
C     ALAMAX BESTIMMEN
C
C
C
      CALL HELMLAM(WSOL35(NW35-10),XYKR,ND)
      CALL HELMLAM(WSOL35(NW35-9),XYKP,ND)
      DXYK(1)=XYKR(1)-XYKP(1)
      DXYK(2)=XYKR(2)-XYKP(2)
      DELTA=SQRT(DXYK(1)**2+DXYK(2)**2)
      DO I=NW35-8,NW35
      XYKR(1)=XYKP(1)
      XYKR(2)=XYKP(2)
      CALL HELMLAM(WSOL35(I),XYKP,ND)
      DXYKH(1)=XYKP(1)-XYKR(1)
      DXYKH(2)=XYKP(2)-XYKR(2)
      DELTH=SQRT(DXYKH(1)**2+DXYKH(2)**2)
C
C     PRÜFUNGEN
C
C
      IF(DELTH.GT.DELTA) THEN
         EXIT
      ENDIF
      IF(DXYKH(1).LT.0.) THEN
         EXIT
      ENDIF
      IF(DXYKH(2).GT.0.) THEN
         EXIT
      ENDIF
      DELTA=DELTH
      END DO
      I=I-1
      ALAMAX=WSOL35(I)
      CALL HELMLAM(ALAMAX,XYMAX,ND)
      DXYKH(1)=XYMAX(1)-XY0(1)
      DXYKH(2)=XYMAX(2)-XY0(1)
      PHMAX=WNGRD(DXYKH(1),DXYKH(2))

C
C
C
C     WINKEL
C
C
      XYKR(1)=XYK(1)-XY0(1)
      XYKR(2)=XYK(2)-XY0(2)
      DD=SQRT(XYKR(1)**2+XYKR(2)**2)
      IF(DD.LT.EPS) THEN
        ALAMD=700.
        XYD(1)=XYK(1)
        XYD(2)=XYK(2)
        PE=0.
        PC=0.
        IER=1
        RETURN
      ENDIF
      PHI=WNGRD(XYKR(1),XYKR(2))
C
      PHK=PHI
C
      IF(PHK.GT.PHMAX.AND.PHK.LT.360.) THEN
        PHK=PHK-360.
        PHMAX=PHMAX-360.
      ENDIF
C
C
C
C     GRENZEN NEU SETZEN (WEGEN EINDEUTIGKEIT IN WIDIFF)
C
C
      IF(PHK.LT.90.) THEN
         ALAMIN=500.
      ENDIF
      IF(PHK.GT.160..AND.PHK.LT.PHMIN) THEN
         ALAMAX=500.
      ENDIF

C
C
      ALAMD=0.5*(ALAMAX-ALAMIN)
      CALL INTVAL(ALAMD,ALAMIN,ALAMAX,FHELM,IER)
      IF(IER.NE.0) GOTO 900
C
C     BERECHNUMNG DER x,y KOORDINATEN FÜR DIE FARBTONGLEICHE WELLENLÄNGE
C
C

      IF(PHI.GT.PHMIN.AND.PHI.LT.PHMAX) THEN
         ALAMD=-ALAMD
         CALL PURPURXY(XYK,XYMIN,XYMAX,XY0,XYD,IER)
      ELSE
         CALL HELMLAM(ALAMD,XYD,ND)
      ENDIF
C
C
C
C
C     SPEKTRALER FARBANTEIL
C
      A1=SQRT((XYK(1)-XY0(1))**2+(XYK(2)-XY0(2))**2)
      A2=SQRT((XYD(1)-XY0(1))**2+(XYD(2)-XY0(2))**2)
      PE=A1/(A2+TINY(1.))
C
C
C     SPEKTRALER LEUCHTDICHTEANTEIL
C
      A1=SQRT(XYD(1)**2+XYD(2)**2)
      A2=SQRT(XYK(1)**2+XYK(2)**2)
      PC=PE*A1/(A2+TINY(1.))

C
  900 RETURN
      END
C
C
C
C
C
      SUBROUTINE HELMLAM(ALAM,XYK,NN)
      USE MOD6164

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZLAM(3),XYK(*)
C
C
C     x,y (max) FÜR VORGEGEBENES LAMBDA (ALAM) BERECHNEN (GILT NUR AUSSERHALB DER PURPURGERADEN)
C
C      IX=(NN-1)*3
      DO I=1,3
         XYZLAM(I)=RW6164(ALAM,NW35,WSOL35,E6164(1,I,NN))
      END DO
      CALL XYZKL(XYZLAM,XYK)
      RETURN
      END
      SUBROUTINE PURPURXY(XYK,XYMIN,XYMAX,XY0,XYP,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYMIN(*),XYMAX(*),XY0(*),XYK(*),XYP(*)
C
C
C      BERECHNUNG DER x,y-KOORDINATEN FÜR DIE PURPURGERADE
C
      AZ=(XYMIN(2)-XY0(2))*(XYK(1)-XY0(1))-
     &   (XYMIN(1)-XY0(1))*(XYK(2)-XY0(2))
      AN=(XYMAX(1)-XYMIN(1))*(XYK(2)-XY0(2))-
     &   (XYMAX(2)-XYMIN(2))*(XYK(1)-XY0(1))
      AL=AZ/AN
      XYP(1)=XYMIN(1)+AL*(XYMAX(1)-XYMIN(1))
      XYP(2)=XYMIN(2)+AL*(XYMAX(2)-XYMIN(2))
      RETURN
      END SUBROUTINE

      DOUBLE PRECISION FUNCTION FHELM(AL)
      USE MOD6164

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYH(2),XYD(2)
      AH=AL
      IF(AH.LT.0.) THEN
         AH=-AH
      ENDIF
      CALL HELMLAM(AH,XYD,ND)
      XYH(1)=XYD(1)-XY0(1)
      XYH(2)=XYD(2)-XY0(1)
      PHIH=WNGRD(XYH(1),XYH(2))
      IF(PHK.LT.0.AND.PHIH.GT.(360.-PHMAX)) THEN
         PHIH=PHIH-360.
      ENDIF
      FHELM=WIDIFF(PHK,PHIH)
      RETURN
      END
C
C
C
C
C
C      Farbkonstanz-Index (CMCCAT2000)
C      Stephen  Westland and al.
C      Computational Colour Science S.90
C
C
      SUBROUTINE FKICIE(JABST,KW,KNO,D,XYZB,XYZT,FKI)
      USE MODILLU
C
C     XYZT Tristimuluswerte für Testlichtart (sample)
C     XYZB Tristimuluswerte für Bezugslichtart (reference)
C
      IMPLICIT REAL(KIND=8)(A-H,O-Z)
      REAL(KIND=8),DIMENSION(3) :: XYZB,XYZT,XYZK,LABB,LABK,LCHX
      REAL(KIND=8),DIMENSION(3,3) :: MCAT,MINV
      REAL(KIND=8),DIMENSION(3) :: RGB,RGBWT,RGBWB,RGBK
      DATA MCAT/0.7982,-0.5918,0.0008,
     &          0.3389,1.5512,0.0239,
     &         -0.1371,0.0406,0.9753/
      DATA MINV/1.07645,0.41096,-0.01095,
     &         -0.23766,0.55434,-0.01339,
     &          0.16121,0.03469,1.02434/
C
C     Prüfen von NLA(1) auf NLD65(3 oder 31) und NLA(KNO) auf NLA (2 oder 32) 10 Grad
C
C
      FKI=HUGE(1.0)
      IF((NLA(1).NE.3.AND.NLA(1).NE.31).OR.
     &   (NLA(KNO).NE.2.AND.NLA(KNO).NE.32)) THEN
           RETURN
      ENDIF
C
C
      RGB=MATMUL(MCAT,XYZT)
      RGBWB=MATMUL(MCAT,FTKT(1:3,1))
      RGBWT=MATMUL(MCAT,FTKT(1:3,KNO))
      DO I=1,3
        RGBK(I)=(1.0+D*(RGBWB(I)/RGBWT(I))-D)*RGB(I)
      END DO

      XYZK=MATMUL(MINV,RGBK)
      CALL LCHAL(JABST,XYZB,LABB,LCHX,1)
      CALL LCHAL(JABST,XYZK,LABK,LCHX,1)
C
C     Farbabstand zwischen chromatisch adaptierten und vorgegebenen Werten für Bezugslichtart
      FKI=SQRT((LABK(1)-LABB(1))**2
     &           +(LABK(2)-LABB(2))**2
     &           +(LABK(3)-LABB(3))**2)
  10  RETURN
C
C
C     XYZT so aus XYZB berechnen, dass FKI=0.0
C

      ENTRY FKIINV(KW,KNO,D,XYZB,XYZT)
C
      CALL LCHAL(JABST,XYZB,LABB,LCHX,1)
      CALL XYZLABAL(JABST,XYZK,LABB,2,*10)
      RGBWB=MATMUL(MCAT,FTKT(1:3,1))
      RGBWT=MATMUL(MCAT,FTKT(1:3,KNO))
      RGBK=MATMUL(MCAT,XYZK)
      DO I=1,3
        RGB(I)=RGBK(I)/(1.0+D*(RGBWB(I)/RGBWT(I))-D)
      END DO
      XYZT=MATMUL(MINV,RGB)
      RETURN
      END
C
C
C
      REAL(KIND=8) FUNCTION TSRAMO(NWE,WSOL,R,KV)
      IMPLICIT REAL(KIND=8)(A-H,O-Z)
      REAL(KIND=8),DIMENSION(441) :: WEL,HEMTIRR,DIRCISO
      REAL(KIND=8),DIMENSION(*) :: R,WSOL
      REAL(KIND=8) :: EPS,SUMR,SUME
      INTEGER(KIND=4) :: NANZ,I,J
      DATA EPS/1.0D-3/
      DATA NANZ/441/
C
C     Berechnung der Total Solar Reflectance
C
C     s. ASTM international Designation: G 173 - 03
C     Standard Tables
C     Reference Solar Spectral Irradiances:
C     Direct Normal and
C     Hemispherical on 37° Tilted Surface
C     WSOL Wellenlängen, für die Remissionswerte vorliegen (Anzahl NWE)
C     R    Remissionswerte (Anzahl NWE)
C
C     KV=1 Berechnung mit Hemispherical on 37° Tilted Surface
C     KV=2 Berechnung mit Direct Normal
C
C
C     Wellenlängen für die vorgegebenen Strahlungsintensitäten
C
C
C
      DATA WEL/
     &0300.0,0305.0,0310.0,0315.0,0320.0,0325.0,0330.0,0335.0,0340.0,
     &0345.0,0350.0,0355.0,0360.0,0365.0,0370.0,0375.0,0380.0,0385.0,           
     &0390.0,0395.0,0400.0,0405.0,0410.0,0415.0,0420.0,0425.0,0430.0,           
     &0435.0,0440.0,0445.0,0450.0,0455.0,0460.0,0465.0,0470.0,0475.0,           
     &0480.0,0485.0,0490.0,0495.0,0500.0,0505.0,0510.0,0515.0,0520.0,           
     &0525.0,0530.0,0535.0,0540.0,0545.0,0550.0,0555.0,0560.0,0565.0,           
     &0570.0,0575.0,0580.0,0585.0,0590.0,0595.0,0600.0,0605.0,0610.0,           
     &0615.0,0620.0,0625.0,0630.0,0635.0,0640.0,0645.0,0650.0,0655.0,           
     &0660.0,0665.0,0670.0,0675.0,0680.0,0685.0,0690.0,0695.0,0700.0,           
     &0705.0,0710.0,0715.0,0720.0,0725.0,0730.0,0735.0,0740.0,0745.0,           
     &0750.0,0755.0,0760.0,0765.0,0770.0,0775.0,0780.0,0785.0,0790.0,           
     &0795.0,0800.0,0805.0,0810.0,0815.0,0820.0,0825.0,0830.0,0835.0,           
     &0840.0,0845.0,0850.0,0855.0,0860.0,0865.0,0870.0,0875.0,0880.0,           
     &0885.0,0890.0,0895.0,0900.0,0905.0,0910.0,0915.0,0920.0,0925.0,           
     &0930.0,0935.0,0940.0,0945.0,0950.0,0955.0,0960.0,0965.0,0970.0,           
     &0975.0,0980.0,0985.0,0990.0,0995.0,1000.0,1005.0,1010.0,1015.0,           
     &1020.0,1025.0,1030.0,1035.0,1040.0,1045.0,1050.0,1055.0,1060.0,           
     &1065.0,1070.0,1075.0,1080.0,1085.0,1090.0,1095.0,1100.0,1105.0,           
     &1110.0,1115.0,1120.0,1125.0,1130.0,1135.0,1140.0,1145.0,1150.0,           
     &1155.0,1160.0,1165.0,1170.0,1175.0,1180.0,1185.0,1190.0,1195.0,           
     &1200.0,1205.0,1210.0,1215.0,1220.0,1225.0,1230.0,1235.0,1240.0,           
     &1245.0,1250.0,1255.0,1260.0,1265.0,1270.0,1275.0,1280.0,1285.0,           
     &1290.0,1295.0,1300.0,1305.0,1310.0,1315.0,1320.0,1325.0,1330.0,           
     &1335.0,1340.0,1345.0,1350.0,1355.0,1360.0,1365.0,1370.0,1375.0,           
     &1380.0,1385.0,1390.0,1395.0,1400.0,1405.0,1410.0,1415.0,1420.0,           
     &1425.0,1430.0,1435.0,1440.0,1445.0,1450.0,1455.0,1460.0,1465.0,           
     &1470.0,1475.0,1480.0,1485.0,1490.0,1495.0,1500.0,1505.0,1510.0,           
     &1515.0,1520.0,1525.0,1530.0,1535.0,1540.0,1545.0,1550.0,1555.0,           
     &1560.0,1565.0,1570.0,1575.0,1580.0,1585.0,1590.0,1595.0,1600.0,           
     &1605.0,1610.0,1615.0,1620.0,1625.0,1630.0,1635.0,1640.0,1645.0,           
     &1650.0,1655.0,1660.0,1665.0,1670.0,1675.0,1680.0,1685.0,1690.0,           
     &1695.0,1700.0,1705.0,1710.0,1715.0,1720.0,1725.0,1730.0,1735.0,           
     &1740.0,1745.0,1750.0,1755.0,1760.0,1765.0,1770.0,1775.0,1780.0,           
     &1785.0,1790.0,1795.0,1800.0,1805.0,1810.0,1815.0,1820.0,1825.0,           
     &1830.0,1835.0,1840.0,1845.0,1850.0,1855.0,1860.0,1865.0,1870.0,           
     &1875.0,1880.0,1885.0,1890.0,1895.0,1900.0,1905.0,1910.0,1915.0,           
     &1920.0,1925.0,1930.0,1935.0,1940.0,1945.0,1950.0,1955.0,1960.0,           
     &1965.0,1970.0,1975.0,1980.0,1985.0,1990.0,1995.0,2000.0,2005.0,           
     &2010.0,2015.0,2020.0,2025.0,2030.0,2035.0,2040.0,2045.0,2050.0,           
     &2055.0,2060.0,2065.0,2070.0,2075.0,2080.0,2085.0,2090.0,2095.0,           
     &2100.0,2105.0,2110.0,2115.0,2120.0,2125.0,2130.0,2135.0,2140.0,           
     &2145.0,2150.0,2155.0,2160.0,2165.0,2170.0,2175.0,2180.0,2185.0,           
     &2190.0,2195.0,2200.0,2205.0,2210.0,2215.0,2220.0,2225.0,2230.0,           
     &2235.0,2240.0,2245.0,2250.0,2255.0,2260.0,2265.0,2270.0,2275.0,           
     &2280.0,2285.0,2290.0,2295.0,2300.0,2305.0,2310.0,2315.0,2320.0,           
     &2325.0,2330.0,2335.0,2340.0,2345.0,2350.0,2355.0,2360.0,2365.0,           
     &2370.0,2375.0,2380.0,2385.0,2390.0,2395.0,2400.0,2405.0,2410.0,           
     &2415.0,2420.0,2425.0,2430.0,2435.0,2440.0,2445.0,2450.0,2455.0,           
     &2460.0,2465.0,2470.0,2475.0,2480.0,2485.0,2490.0,2495.0,2500.0/
C
C     Irradiances for Hemispherical on 37° Tilted Surface
C
      DATA HEMTIRR/
     &0.0010,0.0165,0.0509,0.1363,0.2053,0.2789,0.4714,0.4639,0.5018,
     &0.4590,0.5280,0.6114,0.5982,0.6236,0.7551,0.5893,0.7008,0.6736,           
     &0.7970,0.8077,1.1141,1.1511,1.0485,1.2258,1.1232,1.2488,0.8746,           
     &1.2452,1.3499,1.4619,1.5595,1.5224,1.5291,1.5350,1.5077,1.6185,           
     &1.6181,1.5683,1.6224,1.6485,1.5451,1.5635,1.5481,1.5314,1.5236,           
     &1.5781,1.5446,1.5535,1.4825,1.5435,1.5399,1.5634,1.4740,1.5201,           
     &1.4816,1.4777,1.5020,1.5324,1.3709,1.4308,1.4753,1.4895,1.4686,           
     &1.4697,1.4739,1.4026,1.3924,1.4458,1.4340,1.4567,1.3594,1.3499,           
     &1.3992,1.4214,1.4196,1.3958,1.3969,1.3748,1.1821,1.2714,1.2823,           
     &1.3214,1.3175,1.2587,0.9855,1.0380,1.1285,1.2178,1.2195,1.2497,           
     &1.2341,1.2383,0.2660,0.6860,1.1608,1.1771,1.1636,1.1586,1.0910,           
     &1.0932,1.0725,1.0545,1.0559,0.8952,0.8619,0.9694,0.9160,1.0032,           
     &1.0157,1.0165,0.8937,0.9130,0.9882,0.9632,0.9676,0.9269,0.9396,           
     &0.9442,0.9239,0.8136,0.7426,0.8171,0.6247,0.6784,0.7441,0.7111,           
     &0.4321,0.2508,0.4718,0.3682,0.1473,0.3412,0.4207,0.5037,0.6346,           
     &0.5899,0.6047,0.6882,0.7323,0.7518,0.7353,0.6817,0.7191,0.7082,           
     &0.6990,0.6975,0.6906,0.6823,0.6717,0.6645,0.6546,0.6485,0.6359,           
     &0.6291,0.6047,0.5925,0.5972,0.5933,0.5557,0.5207,0.4858,0.5064,           
     &0.4790,0.2499,0.1419,0.1443,0.0706,0.0155,0.2560,0.1460,0.1216,           
     &0.3132,0.2865,0.3886,0.4587,0.4524,0.4407,0.4074,0.4624,0.4470,           
     &0.4483,0.4369,0.4534,0.4278,0.4581,0.4624,0.4600,0.4650,0.4608,           
     &0.4566,0.4571,0.4507,0.4311,0.3957,0.3874,0.4123,0.4220,0.4240,           
     &0.4129,0.4051,0.3531,0.3838,0.3011,0.2858,0.2587,0.3222,0.2292,           
     &0.2312,0.1683,0.1090,0.0160,0.0000,0.0000,0.0000,0.0000,0.0003,           
     &0.0001,0.0000,0.0005,0.0000,0.0000,0.0000,0.0005,0.0002,0.0083,           
     &0.0259,0.0616,0.0214,0.0396,0.0498,0.0274,0.0662,0.0854,0.0934,           
     &0.0497,0.1846,0.0606,0.1250,0.1748,0.1825,0.2506,0.1840,0.2705,           
     &0.2657,0.2645,0.2588,0.2552,0.2669,0.2649,0.2771,0.2699,0.2676,           
     &0.2657,0.2673,0.2418,0.2397,0.2446,0.2587,0.2418,0.2582,0.2381,           
     &0.2368,0.2176,0.2410,0.2345,0.2378,0.2365,0.2337,0.2151,0.2182,           
     &0.2253,0.2223,0.2233,0.2118,0.2217,0.2136,0.2056,0.2132,0.2052,           
     &0.2097,0.1998,0.1978,0.1879,0.1897,0.1870,0.1781,0.1741,0.1615,           
     &0.1682,0.1548,0.1657,0.1530,0.1600,0.1328,0.1417,0.1148,0.1005,           
     &0.0770,0.0889,0.0469,0.0318,0.0148,0.0097,0.0033,0.0010,0.0013,           
     &0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,           
     &0.0000,0.0001,0.0000,0.0002,0.0001,0.0000,0.0000,0.0000,0.0000,           
     &0.0005,0.0009,0.0006,0.0036,0.0033,0.0109,0.0167,0.0100,0.0219,           
     &0.0286,0.0488,0.0679,0.0755,0.0831,0.0856,0.0812,0.0382,0.0150,           
     &0.0397,0.0266,0.0450,0.0740,0.0849,0.0964,0.0898,0.0911,0.0679,           
     &0.0549,0.0692,0.0619,0.0657,0.0774,0.0868,0.0851,0.0891,0.0897,           
     &0.0861,0.0932,0.0897,0.0917,0.0876,0.0886,0.0898,0.0900,0.0908,           
     &0.0895,0.0846,0.0848,0.0842,0.0763,0.0820,0.0804,0.0818,0.0746,           
     &0.0791,0.0790,0.0712,0.0740,0.0793,0.0763,0.0777,0.0755,0.0758,           
     &0.0743,0.0731,0.0708,0.0719,0.0677,0.0669,0.0681,0.0649,0.0640,           
     &0.0663,0.0631,0.0632,0.0613,0.0588,0.0592,0.0639,0.0581,0.0520,           
     &0.0562,0.0568,0.0580,0.0458,0.0514,0.0415,0.0475,0.0502,0.0494,           
     &0.0308,0.0441,0.0426,0.0308,0.0371,0.0406,0.0442,0.0336,0.0338,           
     &0.0273,0.0266,0.0331,0.0451,0.0149,0.0432,0.0208,0.0136,0.0249,           
     &0.0334,0.0241,0.0167,0.0165,0.0080,0.0056,0.0035,0.0029,0.0071/
C
C     Irradiances for Direct Normal (Direct + Circumsolar)
C
      DATA DIRCISO/
     &0.0005,0.0089,0.0278,0.0737,0.1128,0.1550,0.2619,0.2648,0.2966,           
     &0.2785,0.3291,0.3914,0.3924,0.4181,0.5167,0.4109,0.4975,0.4864,           
     &0.5864,0.6010,0.8399,0.8785,0.8091,0.9557,0.8847,0.9931,0.7013,           
     &1.0070,1.0993,1.1992,1.2881,1.2655,1.2791,1.2905,1.2749,1.3755,           
     &1.3825,1.3457,1.3968,1.4238,1.3391,1.3598,1.3497,1.3385,1.3349,           
     &1.3859,1.3598,1.3701,1.3096,1.3657,1.3648,1.3883,1.3118,1.3555,           
     &1.3240,1.3225,1.3455,1.3737,1.2316,1.2870,1.3278,1.3418,1.3237,           
     &1.3254,1.3299,1.2667,1.2589,1.3065,1.2962,1.3170,1.2299,1.2220,           
     &1.2668,1.2871,1.2853,1.2639,1.2650,1.2454,1.0746,1.1538,1.1636,           
     &1.1989,1.1954,1.1428,0.8994,0.9474,1.0294,1.1101,1.1119,1.1404,           
     &1.1273,1.1321,0.2472,0.6338,1.0646,1.0801,1.0687,1.0649,1.0045,           
     &1.0066,0.9886,0.9727,0.9749,0.8293,0.7990,0.8975,0.8493,0.9292,           
     &0.9412,0.9423,0.8290,0.8475,0.9176,0.8949,0.8993,0.8620,0.8743,           
     &0.8791,0.8608,0.7596,0.6943,0.7634,0.5855,0.6355,0.6966,0.6662,           
     &0.4068,0.2369,0.4441,0.3473,0.1394,0.3220,0.3969,0.4747,0.5969,           
     &0.5554,0.5694,0.6473,0.6884,0.7067,0.6916,0.6414,0.6770,0.6668,           
     &0.6584,0.6573,0.6509,0.6435,0.6337,0.6271,0.6180,0.6124,0.6007,           
     &0.5946,0.5718,0.5605,0.5652,0.5616,0.5266,0.4936,0.4611,0.4807,           
     &0.4550,0.2383,0.1356,0.1379,0.0676,0.0148,0.2445,0.1398,0.1165,           
     &0.2990,0.2737,0.3708,0.4373,0.4314,0.4205,0.3891,0.4412,0.4267,           
     &0.4279,0.4171,0.4327,0.4085,0.4372,0.4414,0.4393,0.4441,0.4401,           
     &0.4362,0.4368,0.4309,0.4124,0.3787,0.3709,0.3946,0.4039,0.4058,           
     &0.3952,0.3880,0.3386,0.3678,0.2891,0.2745,0.2486,0.3094,0.2205,           
     &0.2224,0.1622,0.1052,0.0155,0.0000,0.0000,0.0000,0.0000,0.0003,           
     &0.0001,0.0000,0.0005,0.0000,0.0000,0.0000,0.0005,0.0002,0.0080,           
     &0.0251,0.0599,0.0208,0.0385,0.0484,0.0267,0.0644,0.0832,0.0909,           
     &0.0484,0.1795,0.0591,0.1217,0.1699,0.1775,0.2434,0.1789,0.2627,           
     &0.2580,0.2569,0.2514,0.2479,0.2592,0.2574,0.2692,0.2623,0.2601,           
     &0.2582,0.2598,0.2350,0.2329,0.2377,0.2514,0.2349,0.2508,0.2313,           
     &0.2301,0.2115,0.2342,0.2278,0.2311,0.2298,0.2271,0.2091,0.2122,           
     &0.2190,0.2162,0.2172,0.2061,0.2157,0.2079,0.2002,0.2076,0.1999,           
     &0.2043,0.1946,0.1928,0.1832,0.1849,0.1823,0.1737,0.1698,0.1576,           
     &0.1641,0.1511,0.1616,0.1493,0.1561,0.1297,0.1383,0.1121,0.0981,           
     &0.0752,0.0868,0.0459,0.0311,0.0145,0.0095,0.0032,0.0010,0.0012,           
     &0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,           
     &0.0000,0.0001,0.0000,0.0002,0.0001,0.0000,0.0000,0.0000,0.0000,           
     &0.0004,0.0009,0.0005,0.0035,0.0032,0.0107,0.0165,0.0099,0.0216,           
     &0.0281,0.0481,0.0667,0.0742,0.0816,0.0841,0.0798,0.0375,0.0147,           
     &0.0391,0.0262,0.0442,0.0728,0.0835,0.0948,0.0883,0.0896,0.0669,           
     &0.0541,0.0682,0.0610,0.0647,0.0763,0.0855,0.0838,0.0878,0.0884,           
     &0.0849,0.0918,0.0883,0.0903,0.0863,0.0873,0.0884,0.0887,0.0894,           
     &0.0881,0.0834,0.0836,0.0829,0.0752,0.0808,0.0793,0.0806,0.0735,           
     &0.0779,0.0778,0.0702,0.0729,0.0782,0.0752,0.0766,0.0744,0.0747,           
     &0.0733,0.0721,0.0699,0.0710,0.0669,0.0661,0.0674,0.0641,0.0633,           
     &0.0656,0.0624,0.0625,0.0606,0.0582,0.0585,0.0632,0.0575,0.0515,           
     &0.0556,0.0562,0.0574,0.0454,0.0509,0.0411,0.0470,0.0497,0.0489,           
     &0.0305,0.0437,0.0421,0.0305,0.0367,0.0402,0.0437,0.0333,0.0335,           
     &0.0271,0.0264,0.0328,0.0447,0.0148,0.0429,0.0207,0.0135,0.0247,           
     &0.0332,0.0240,0.0166,0.0164,0.0080,0.0056,0.0035,0.0029,0.0070/
      DO J=1,NWE
        IF(WSOL(J).GE.WEL(1)) THEN
          EXIT
        ENDIF
      END DO
      SUMR=0.0
      SUME=0.0
      DO I=1,NANZ
       IF(ABS(WEL(I)-WSOL(J)).LT.EPS) THEN
          SUME=SUME+HEMTIRR(I)
          IF(KV.EQ.1) THEN
            SUMR=SUMR+HEMTIRR(I)*R(J)
          ELSE
            SUMR=SUMR+DIRCISO(I)*R(J)
          ENDIF
          J=J+1
          IF(J.GT.NWE) EXIT
       ENDIF
      END DO
      TSRAMO=100.*SUMR/SUME
      RETURN
      END
C
C     Flächendifferenz
C
C
      DOUBLE PRECISION FUNCTION FLDIF(KWI,NWE,RV,RR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RV(NWE,*),RR(NWE,*)
      IF(KWI.LE.0.OR.KWI.GT.KMS()) THEN
         SUU=0.
         DO KW=1,KMS()
           SUU=SUU+FLDII(RV(1,KW),RR(1,KW))
         ENDDO
         FLDIF=SUU/FLOAT(KMS())
      ELSE
         FLDIF=FLDII(RV(1,KWI),RR(1,KWI))
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION FLDII(RV,RR)
      USE MODILLU
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RV(*),RR(*)
      WDIF=WSOL(2)-WSOL(1)
      SUU=0.
      DO I=1,NWE-1
         DR1=RR(I)-RV(I)
         DR2=RR(I+1)-RV(I+1)
         IF(DR1*DR2.GT.0.) THEN
            SUU=SUU+ABS(DR1)+ABS(DR2)
         ELSE
            R=ABS(DR1)/(ABS(DR2)+TINY(1.))            
            SUU=SUU+(R*ABS(DR1)+ABS(DR2))/(1.+R)
         ENDIF
      ENDDO
      FLDII=0.5*(NWE-1)*WDIF*SUU
      RETURN
      END

