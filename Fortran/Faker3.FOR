C     Last change: KU 21.02.2021 11:57:49
C
C     Last change:  UFO  27 Oct 104    6:11 pm
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

      SUBROUTINE LIFDE(ITP,CONZA,CONZ,
     &                 RWERP,PARAM,WERT)
      USE MOTWERT
      USE MOSRWRT
      USE MODQUAL
      USE MODFUNC
      USE MODILLU
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION CONZT,CCONZ(6),FAE60,CONZTA
      SAVE CONZT,CCONZ,FAE60,CONZTA
      TYPE (SRWERT) RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*)::WERT
C
C
      DATA FAE60/0.D0/
      DATA CONZT/0.D0/
      DATA CCONZ/0.D0,0.D0,0.D0,0.D0,0.D0,0.D0/
      DATA CONZTA/0.D0/
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
C     AUFHELLVERHAELTNIS (AV) RELATIVE FARBSTAERKE (FREL) FAERBEAEQUIV. (FAE)
C
      IF(RWERP%IAMI.LT.0) THEN
         RETURN
      ENDIF
C
C
      AV=1./(CONZ+TINY(1.))
      AVA=1./(CONZA+TINY(1.))
C
C

      IF(RWERP%CART(1:1).EQ.'@') THEN
         ITZ=1
      ELSE IF(RWERP%CART(1:1).EQ.'A') THEN
         ITZ=2
      ELSE IF(RWERP%CART(1:1).EQ.'B') THEN
         ITZ=3
      ELSE IF(RWERP%CART(1:1).EQ.'C') THEN
         ITZ=4
      ELSE IF(RWERP%CART(1:1).EQ.'D') THEN
         ITZ=5
      ELSE IF(RWERP%CART(1:1).EQ.'E') THEN
         ITZ=6
      ENDIF
      IF(RWERP%CART(2:2).EQ.'T') THEN
            CCONZ(ITZ)=CONZ
            FEE=1.
      ELSE 
            FEE=CONZ/(CCONZ(ITZ)+TINY(1.))
      ENDIF
C
      IF(ITP.EQ.1) THEN
            FREL=1.
            FAE=1.
            CONZT=CONZ
            CONZTA=CONZA
      ELSE
            FREL=CONZT/CONZ                  
            FAE=CONZ/CONZT
      ENDIF
C
C     FARBMITTELMENGE FUER NEUE EINWAAGE (GLEICHE FARBSTAERKE WIE TYP(MESSUNG))
C
C   
C             KONZ(TYP-ALT)*FAE*MENGE(WEISS)*PROZ(WEISS)
C     AMNEU=--------------------------------------------------
C                      PROZ(BUNT)
C
      AMNEU=CONZTA*FAE*RWERP%CAMP(1)*RWERP%CAMP(2)/
     &                                 (RWERP%CAMP(4)+TINY(1.))
C
C
C     FARBMITTELMENGE FUER STANDARDFARBTIEFE
C
C     AMNST=MENGE(FARBM.ALT)*CONZ(NEU)/CONZ(ALT)
C     AMNST=MENGE(WEISS ALT)/AV
C
C
C
      AMNST=RWERP%CAMP(3)*CONZ/(CONZA+TINY(1.))

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
C
      WERT(1)=CONZA
      WERT(2)=CONZ
      WERT(3)=AV
      WERT(21)=AVA
C
C
C     FREL,FAE bezogen auf ersten Typ (mit @)
      WERT(4)=FREL
      WERT(5)=FAE
C
C     Reihenfolge altes Programm
C
C     WERT(6)=RWERP%CAMP(1)
C     WERT(7)=RWERP%CAMP(2)
C     WERT(8)=RWERP%CAMP(3)
C     WERT(9)=AMNST
C     WERT(10)=RWERP%CAMP(4)
C
C
C
C
      WERT(6)=RWERP%CAMP(3)
      WERT(7)=AMNST
      WERT(8)=RWERP%CAMP(4)
      WERT(9)=RWERP%CAMP(1)
      WERT(10)=RWERP%CAMP(2)
      IF(RWERP%CART(1:1).EQ.'@') THEN
         FAE60=FAE
      ELSE 
         RUBO=FAE60/(FAE+TINY(1.))-1.   
         WERT(12)=RUBO
         DHA=FAE/(FAE60+TINY(1.))-1.
         WERT(11)=DHA 
      ENDIF
C
C
C     bezogen auf Typ mit gleicher Präparation(gleiches ITZ) (@T mit @P bzw.AT mit AP usw)
C
C
C
      WERT(13)=FEE
      WERT(14)=1./(FEE+TINY(1.))
      WERT(15)=AMNEU
C
C     Färbeäqu.(16) und Farbstärke(17)
C     bezogen auf @-Messung, d.h. Messung der ersten Probe oder des ersten Typs in der Gruppe
C
C
      WERT(16)=FAE/(FAE60+TINY(1.))
      WERT(17)=FAE60/(FAE+TINY(1.))
C
C      CALL RWMINI(1,KMI,WMI,RMI,RWERP%R(1))

C      

      IF(IART.EQ.6) THEN
      	IF(ITP.EQ.1) THEN
      	   WERT(18)=WMIN(1)
      	ENDIF
        WERT(20)=RWZWIS(WMIN(1),NWE,WSOL,RWERP%R)
        WERT(19)=AKUMU( WERT(20),KWS())
      ENDIF
C
C
C
      RETURN
      END

C                            +-------+
C                            ! LIKPF !
C                            +-------+
      SUBROUTINE LIKPF(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODGKWR

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT


      DIMENSION XYZT(3),XYZP(3)
C
C     Anzahl Messungen für Mittelwert
      DUMMY=FLOAT(RWERP%IAMI)
      WERT(1)=DUMMY
C     Farbsabstand (Fehler bei Messungen)
      IF(RWERP%DESTD.LT.100.) THEN
            WERT(2)=RWERP%DESTD
      ENDIF
C
C
C     Flächendifferenz zwischen Kurven
C
C
      IF(ITP.EQ.2) THEN
        WERT(17)=FLDII(RWERT%R,RWERP%R)
      ENDIF
C
C
C     WERT(3-10) IM VB-PROGRAMM
C
C
C      GKK=GLANZ(KW)
C
C
C 
         DO I=21,36
            WERT(I)=GK(I-20,KW)
         ENDDO
         IRT=RWERP%RETR
         WERT(21)= GLZNOG(KW,IRT)
      RETURN
      END
C                            +-------+
C                            ! LIXYZ !
C                            +-------+
      SUBROUTINE LIXYZ(ITP,KNO,XYZT,XYZP,XYZU,
     &                 RWERT,RWERP,RWERUN,RWERUK,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      TYPE (SRWERT) RWERT,RWERP,RWERUN,RWERUK
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT


      DIMENSION XYZT(3),XYZP(3),XYK(2),XYZU(3),XYZ(3)
C
C      GKK=GLANZ(KW)
C
      IF(ITP.EQ.1) THEN
        DO I=1,3
           XYZ(I)=XYZT(I)
        END DO
      ELSEIF(ITP.EQ.2) THEN
        DO I=1,3
           XYZ(I)=XYZP(I)
        END DO
        DO I=1,3
             WERT(8+I)=(XYZP(I)-XYZT(I))
        ENDDO
      ENDIF
      DO I=1,3
         WERT(11+I)=XYZU(I)-XYZ(I)
      ENDDO
         CALL XYZKL(XYZ,XYK)
         WERT(1)=XYZ(1)
         WERT(2)=XYZ(2)
         WERT(3)=XYZ(3)
         WERT(4)=XYK(1)
         WERT(5)=XYK(2)
         DO I=1,3
             WERT(14+I)=XYZU(I)   
         ENDDO
      RETURN
      END
C                             +-------+ 
C                             ! LICIE ! 
C                             +-------+ 
      SUBROUTINE LICIE(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODFAKT

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      TYPE(SRWERT) RWERT,RWERP
      TYPE(TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      SAVE DETW,DETS,CROMT,DEPW
      CHARACTER*8 CHWERT
      DIMENSION XYZT(3),XYZP(3),XYZW(3),XYZS(3)
      DIMENSION XYZPW(3),XYZPS(3),XYZTW(3),XYZTS(3),XYZKW(3),XYZKS(3)
      DIMENSION CIE(3),CIA(3)
      DIMENSION XYZD(3),XYZA(3)
C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)

C
C
C
C 
C     LISTE:  C I E L A B  -  WERTE 
C 
C 
C 
C 
C 
C 
C 
      DO 350 I=1,3
      IF(RWERP%CART(2:2).EQ.'T') THEN
        XYZKW(I)=XYZTW(I)
        XYZKS(I)=XYZTS(I)
      ELSEIF(RWERP%CART(2:2).EQ.'P') THEN
        XYZKW(I)=XYZPW(I)
        XYZKS(I)=XYZPS(I)
      ENDIF
      XYZW(I)=FAKT(I,KNO)
      XYZS(I)=0.
 350  CONTINUE
C
C
C
C      Farbkonstanz-Index
C      (s. Georg A. Klein: Farbenphysik für industrielle Anwendungen (S. 92 und S. 304)
C
      CALL NOGXX(XYZD,1,RWERP%R,GKK)
      CALL NOGXX(XYZA,KNO,RWERP%R,GKK)
C
C     D wird 1 gesetzt, da die Adaptionsfelder (Lat usw. ) nicht bekannt sind
C
C
      D=1.0
      CALL FKICIE(0,KW,KNO,D,XYZD,XYZA,WERT(21))
C
C
C
C
      IF(ITP.EQ.1) THEN
         CALL LCH(XYZT,CIE,CIA,KNO)     
         CALL DELAB(XYZT,XYZW,DEW,DLW,DCW,DHW,DAW,DBW,KNO)         
         CALL DELAB(XYZT,XYZS,DES,DLS,DCS,DHS,DAS,DBS,KNO)         
         WERT(1)=CIA(1)    
         WERT(2)=CIA(2)
         WERT(3)=CIA(3)
         WERT(4)=CIE(2)
         WERT(5)=CIE(3)
         CROMT=CIA(2)
         IF(RWERP%CART(4:4).EQ.'S') THEN
C
C           STREU-DE
C
            WERT(7)=DES
            IF(RWERP%CART(2:2).EQ.'T') THEN
              DETS=DES
            ELSE
              DDE=DES-DETS
              WERT(8)=DDE
            ENDIF

C
            IF(XYZKW(1).GE.0..AND.XYZKW(2).GE.0..AND.XYZKW(3).GE.0.)THEN

C             KONTRAST DELTA-E
              CALL DELAB(XYZKW,XYZKS,DEK,DLK,DCK,DHK,DAK,DBK,KNO)
C
              WERT(20)=DEK
C
C             LASURVERMÖGEN
C
              WERT(19)=DETW/(DES+10.*TINY(1.D0))
            ENDIF
         ELSE
            WERT(6)=DEW
            DETW=DEW
         ENDIF
C
C
         IF(KABS.NE.0) THEN
            WERT(11)=CIA(1)
            WERT(12)=CIA(2)
            WERT(13)=CIA(3)
            WERT(14)=CIE(2)
            WERT(15)=CIE(3)
C
          ENDIF
C
C         FARBTONWINKEL WIRD IN CHARACTER ANGEGEBEN (R Y B G usw.)
C
          IF(CIA(2).GT.0.2) THEN
             CALL STICIE(CIA(3),CHWERT)
          ELSE
             CHWERT='  FREI  '
          ENDIF
C
C
C  
C         Farbtonwinkel
C
          WERT(16)=CHRWRT(CHWERT)
C
C
      ELSE
C 
C 
C 
          CALL LCH(XYZP(1),CIE,CIA,KNO)  
          CALL DELAB(XYZP,XYZW,DEW,DLW,DCW,DHW,DAW,DBW,KNO)         
          CALL DELAB(XYZP,XYZS,DES,DLS,DCS,DHS,DAS,DBS,KNO)         
          WERT(1)=CIA(1)    
          WERT(2)=CIA(2)
          WERT(3)=CIA(3)
          WERT(4)=CIE(2)
          WERT(5)=CIE(3)
          IF(RWERP%CART(4:4).EQ.'S') THEN
C
C           STREU DE
C
C
            WERT(7)=DES
            IF(RWERP%CART(2:2).EQ.'T') THEN
              DETS=DES
            ELSE
              DDE=DES-DETS
              WERT(8)=DDE
            ENDIF
C
            IF(XYZKW(1).GE.0..AND.XYZKW(2).GE.0..AND.XYZKW(3).GE.0.)THEN
C             KONTRAST DELTA-E
C
              CALL DELAB(XYZKW,XYZKS,DEK,DLK,DCK,DHK,DAK,DBK,KNO)
              WERT(20)=DEK
C
C             LASURVERMÖGEN
C
              WERT(19)=DEPW/(DES+10.*TINY(1.D0))
            ENDIF
          ELSE
              WERT(6)=DEW
              DEPW=DEW
          ENDIF
          IF(KNO.NE.1) THEN 
            CALL METAM(RWERT%R(1),RWERP%R(1),
     &      GKK,AMA,1,KNO)
            WERT(9)=AMA
          ENDIF
          CALL DELAB(XYZT,XYZP,DE,DL,DC,DH,DA,DB,KNO)         
          WERT(10)=DE     
          WERT(11)=DL
          WERT(12)=DC
          WERT(13)=DH
          WERT(14)=DA
          WERT(15)=DB
C
C     FARBTONWINKEL WIRD IN CHARACTER ANGEGEBEN (R Y B G usw.)
C
          IF(CIA(2).GT.0.2) THEN
             CALL STICIE(CIA(3),CHWERT)
          ELSE
             CHWERT='  FREI  '
          ENDIF
C 
C         Farbtonwinkel

          WERT(16)= CHRWRT(CHWERT)
C
C
          STREL=SQRT(DA**2+DB**2)
          IF(STREL.LE.0.2) THEN
             CHWERT='  FREI  '
          ELSE IF(CIA(2).LT.10..AND.CROMT.LT.10.) THEN
             HREL=WNGRD(DA,DB)
             CALL STICIE(HREL,CHWERT)
          ELSE
             CHWERT='  >10.  '
          ENDIF
          WERT(17)=STREL
C
C         rel. Farbstich
          WERT(18)=CHRWRT(CHWERT)
C
C
      ENDIF
      RETURN
      END
C                             +-------+ 
C                             ! LICTC ! 
C                             +-------+ 
      SUBROUTINE LICTC(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(3),XYZP(3)
      DIMENSION XYZPW(3),XYZPS(3),XYZTW(3),XYZTS(3)
      DIMENSION XYZH(3)

C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)
C
C
C
C 
C     LISTE:   CIE94  -  WERTE 
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
      IF(ITP.EQ.2) THEN
C
C 
          IF(KNO.NE.1) THEN 
            CALL XYZNOR(XYZH,RWERT%R(1),RWERP%R(1),
     &      KNO,GKK)
            CALL DCIE94(XYZT,XYZH,AMA,ADL,ADC,ADH,ADA,ADB,KNO)
            WERT(9)=AMA
          ENDIF
C 
C 
C 
C 
C
          CALL DCIE94(XYZT,XYZP,DE,DL,DC,DH,DA,DB,KNO)
C         CIE94
C
          WERT(11)=DL
          WERT(12)=DC
          WERT(13)=DH
          WERT(10)=DE
C
      ENDIF
      RETURN
      END
C                             +-------+ 
C                             ! LICMO ! 
C                             +-------+ 
      SUBROUTINE LICMO(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      TYPE (SRWERT) RWERT,RWERP
      REAL(KIND=8),DIMENSION(*):: WERT
      TYPE (TYPARAM) PARAM
      DIMENSION XYZT(3),XYZP(3)
      DIMENSION XYZPW(3),XYZPS(3),XYZTW(3),XYZTS(3)
      DIMENSION XYZH(3)
C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)
C
C
C
C 
C     LISTE:   CMC(XL,XC)      
C 
C
C 
C
C              XL,XC aus COFRB.INC
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
      IF(ITP.EQ.2) THEN
C 
          IF(KNO.NE.1) THEN 
            CALL XYZNOR(XYZH,RWERT%R(1),RWERP%R(1),
     &      KNO,GKK)
            CALL DULAB(XYZT,XYZH,AMA,ADL,ADC,ADH,KNO)
            WERT(9)=AMA
          ENDIF
C 
C 
          CALL DULAB(XYZT,XYZP,DE,DL,DC,DH,KNO)
C
C         CMC(XL,XC)  
C
          WERT(11)=DL
          WERT(12)=DC
          WERT(13)=DH
          WERT(10)=DE
C
      ENDIF
      RETURN
      END
C                             +-------+ 
C                             ! LIJPC ! 
C                             +-------+ 
      SUBROUTINE LIJPC(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      REAL(KIND=8),DIMENSION(*):: WERT
      TYPE (TYPARAM) PARAM
      DIMENSION XYZT(3),XYZP(3),XYZH(3)
C
C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)
C
C 
C     LISTE:  J P C 7 9              
C 
C 
C 
C 
C 
C 
C 
C 
C 
      IF(ITP.EQ.2) THEN
         CALL JPC79(XYZT,XYZP,DE,DL,DC,DH,KNO)               
         IF(KNO.NE.1) THEN 
            CALL XYZNOR(XYZH,RWERT%R(1),RWERP%R(1),
     &      KNO,GKK)
            CALL JPC79(XYZT,XYZH,AMA,ADL,ADC,ADH,KNO)               
            WERT(1)=AMA
         ENDIF
         WERT(2)=DE        
         WERT(3)=DL
         WERT(4)=DC
         WERT(5)=DH
C
      ENDIF
      RETURN
      END
C                             +-------+ 
C                             ! LIDIN ! 
C                             +-------+ 
      SUBROUTINE LIDIN(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      SAVE TSD,TSDM
      DIMENSION XYZT(3),XYZP(3),XYZH(3)
      DIMENSION TSD(3,2),TSDM(3,2),XYK(2),XYD(2)
C
C
C
C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)
C
C     HELMHOLZ-KOORDINATEN
C
C
C
      IF(ITP.EQ.1) THEN
        CALL XYZKL(XYZT,XYK)
      ELSE
        CALL XYZKL(XYZP,XYK)
      ENDIF
      CALL HELMHOLZ(XYK,XYD,ALAMD,PE,PC,KNO,IER)
      IF(IER.EQ.0) THEN
      	  WERT(10)=ALAMD
          WERT(11)=PE
          WERT(12)=PC
          WERT(13)=XYD(1)
          WERT(14)=XYD(2)
      ENDIF


C
C 
C     LISTE:  D I N  6 1 6 4         
C 
C 
      IF(ITP.EQ.1) THEN
          CALL DINTSD(XYZT,TSD(1,1),KNO) 
          WERT(1)=TSD(1,1)  
          WERT(2)=TSD(2,1)
          WERT(3)=TSD(3,1)
          IF(KABS.NE.0) THEN
            WERT(6)=TSD(1,1)
            WERT(7)=TSD(2,1)
            WERT(8)=TSD(3,1)
          ENDIF
C
C 
      ELSE
          CALL DINTSD(XYZP,TSD(1,2),KNO) 
          CALL D6164(TSD,DE,DT,DS,DD)                                 
          IF(KNO.NE.1) THEN 
            CALL XYZNOR(XYZH,RWERT%R(1),RWERP%R(1),
     &      KNO,GKK)
            CALL DINTSD(XYZT,TSDM(1,1),KNO)                         
            CALL DINTSD(XYZH,TSDM(1,2),KNO)
            CALL D6164(TSDM,AMA,AMT,AMS,AMD)
            WERT(4)=AMA
          ENDIF
          WERT(1)=TSD(1,2)  
          WERT(2)=TSD(2,2)
          WERT(3)=TSD(3,2)
          WERT(5)=DE
          WERT(6)=DT
          WERT(7)=DS
          WERT(8)=DD
C
      ENDIF
      RETURN
      END
C 
C                             +-------+ 
C                             ! LILUV! 
C                             +-------+ 
      SUBROUTINE LILUV(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(3),XYZP(3),XYZH(3)
      DIMENSION CLUV(3)  
C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)
C
C
C
C
C 
C     LISTE:  C I E L U V             
C 
C 
C 
C 
C 
C 
C 
      IF(ITP.EQ.1) THEN
          CALL CILUV(XYZT,CLUV,KNO)      
          WERT(1)=CLUV(1)   
          WERT(2)=CLUV(2)
          WERT(3)=CLUV(3)
          IF(KABS.NE.0) THEN
             WERT(6)=CLUV(1)
             WERT(7)=CLUV(2)
             WERT(8)=CLUV(3)
          ENDIF
C 
      ELSE
          CALL CILUV(XYZP,CLUV,KNO)   
          CALL DELUV(XYZT,XYZP,DE,DC,DH,DL,DU,DV,KNO)               
          IF(KNO.NE.1) THEN 
            CALL XYZNOR(XYZH,RWERT%R(1),RWERP%R(1),
     &      KNO,GKK)
            CALL DELUV(XYZT,XYZH,AMA,ADL,ADC,ADH,ADU,ADV,KNO)               
            WERT(4)=AMA
          ENDIF
          WERT(1)=CLUV(1)   
          WERT(2)=CLUV(2)
          WERT(3)=CLUV(3)
          WERT(5)=DE
          WERT(6)=DL
          WERT(7)=DU
          WERT(8)=DV
C
      ENDIF
      RETURN
      END
C 
C                             +-------+ 
C                             ! LISPZ !
C                             +-------+ 
      SUBROUTINE LISPZ(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODFAKT
      USE MODILLU
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      REAL(KIND=8),DIMENSION(*):: WERT
      TYPE (TYPARAM) PARAM
      DIMENSION XYZT(3),XYZP(3)
      DIMENSION XYZD(3),XYZA(3)
      DIMENSION XYZTW(3),XYZTS(3),XYZPW(3),XYZPS(3),XYZKW(3),XYZKS(3)
      CHARACTER*8 GS,ANBL
C
C
C 
C     LISTE:  S P E Z I A L - AUSWERTUNGEN   
C 
C 
C 
C 
C 
C 
C 
      IF(KBWT.LE.0) KBWT=1
      IF(KBWT.GT.8) KBWT=8
C 
      DO I=1,3
      IF(RWERP%CART(2:2).EQ.'T') THEN
        XYZKW(I)=XYZTW(I)
        XYZKS(I)=XYZTS(I)
      ELSEIF(RWERP%CART(2:2).EQ.'P') THEN
        XYZKW(I)=XYZPW(I)
        XYZKS(I)=XYZPS(I)
      ENDIF
      ENDDO
C 
C 
      IF(ITP.EQ.1) THEN
          CALL GELBW(XYZT,GELB,KNO)
          CALL YELLOW(XYZT,YELL,KNO)
               
          CALL WEISSG(XYZT,WBE,WST,WBA,WSCIE,WS2,FSTI,KNO)
          CALL BWERT(XYZT,KBWT,B1,KNO)
C
C
C
C          WBE    WEISSGRAD NACH BERGER
C          WST    WEISSGRAD NACH STENSBY
C          WBA    WEISSGRAD BASF
C          WSCIE  WEISSGRAD NACH CIE (BERGER: S. 119)
C          WS2    FREI
C          FSTI   FARBSTICH NACH CIE (BERGER: S. 119)
C          GELB   GELBWERT 
C          B-WERT 

           WERT(1)=WBE       
           WERT(2)=WST
           WERT(3)=WBA
           WERT(4)=WSCIE
           WERT(5)=FSTI
           WERT(6)=GELB
           WERT(7)=B1
C          GELBWERT (ASTM E 313)
           WERT(15)=YELL
           IF(RWERT%CART(4:4).EQ.'S') THEN
              IF(XYZKW(2).GT.0.) THEN
C                Opazität (DIN 53146 bzw. ISO 2471)
                 OPAZ=XYZKS(2)/(XYZKW(2)+10.*TINY(1.))
                 WERT(13)=100.*OPAZ
              ENDIF
           ENDIF
C
      ELSE 
           CALL GELBW(XYZP,GELB,KNO)
           CALL GELBW(XYZT,GELBT,KNO)
           CALL YELLOW(XYZP,YELL,KNO)
           CALL YELLOW(XYZT,YELLT,KNO)
           CALL WEISSG(XYZP,WBE,WST,WBA,WSCIE,WS2,FSTI,KNO)
           CALL BWERT(XYZP,KBWT,B1,KNO)
C
           CALL ECHSAN(XYZT,XYZP,DEF,GS,DNOTE,ANBL,SSRB,KNO)
           WERT(1)=WBE       
           WERT(2)=WST
           WERT(3)=WBA
           WERT(4)=WSCIE
           WERT(5)=FSTI
           WERT(6)=GELB
           WERT(7)=B1
C
C          GRAUMASSSTAB ISO 105 A05
C
C 
C
           WERT(8)=CHRWRT(GS)
           WERT(9)=DNOTE
C
C          ANBLUTEN ISO 105 A04
C
C 
C
           WERT(10)=CHRWRT(ANBL)
           WERT(11)=SSRB
           WERT(12)=DEF
C
C          OPAZITAET:  TYP (UNENDLICH DICK) PROBE UEBER SCHWARZ
C
           IF(RWERP%CART(4:4).EQ.'S') THEN
              IF (XYZKW(2).GT.0.) THEN
                OPAZ=XYZKS(2)/(XYZKW(2)+10.*TINY(1.))
                WERT(13)=100.*OPAZ
              END IF
           ENDIF
C          DIFFERENZ GELBWERT (DIN 6167 ASTM D 1925)
           WERT(14)=GELB-GELBT
C          GELBWERT (ASTM E 313)
           WERT(15)=YELL
C          DIFFERENZ GELBWERT (ASTM E 313)
           WERT(16)=YELL-YELLT
C
      ENDIF
      WERT(25)=TSRAMO(NWE,WSOL,RWERP%R,1)
      WERT(26)=TSRAMO(NWE,WSOL,RWERP%R,2)
C
C      Farbkonstanz-Index (D=DFKI)
C      (s. Georg A. Klein: Farbenphysik für industrielle Anwendungen (S. 92 und S. 304)
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)

      CALL NOGXX(XYZD,1,RWERP%R,GKK)
      CALL NOGXX(XYZA,KNO,RWERP%R,GKK)
      CALL FKICIE(0,KW,KNO,DFKI,XYZD,XYZA,WERT(27))

      RETURN
      END
C
C                             +--------+ 
C                             ! LISTAT !
C                             +--------+ 
      SUBROUTINE LISTAT(ITP,KNO,XYZT,XYZP,XYZU,
     &           RWERT,RWERP,RWERUN,RWERUK,PARAM,WERT,KW,IER)
C
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP,RWERUN,RWERUK
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      SAVE CT,WELT,RMINT,EXKS1T,EXKS2T,EXKS3T,EXKS4T
      DIMENSION WELT(5),RMINT(5),WELP(5),RMINP(5)
      INTEGER KMIN(5)
      DIMENSION XYZT(3),XYZP(3),XYZU(3)
C
      DATA NTM/5/
C
      IRT=RWERP%RETR
C
C
      CT=1.
      CP=1.
C
C
C 
C     LISTE:  F A R B S T A E R K E  (PRIMITIV) AUSWERTUNG 
C 
C     Transparente Schichten
C 
C
C 
C 
C 
C 
C
C
      IF(ITP.EQ.1) THEN
         CT=1.
         IF(KNO.EQ.1) THEN
              CALL RWMINI(NTM,KMIN,WELT,RMINT,NWE,WSOL,
     &             RWERP%R(1))
              DO I=1,NTM
               IF(RMINT(I).LT.HUGE(1.)) THEN
                 WERT(17+(I-1)*4)=WELT(I)
                 WERT(19+(I-1)*4)=RMINT(I)
                 WERT(18+(I-1)*4)=1.
                 WERT(20+(I-1)*4)=1.
               ENDIF
              ENDDO 
         ENDIF
C
C
C
C
      ELSE
C 
C 
C
C
        CP=1.
        IF(KNO.EQ.1) THEN
             CALL RWMINI(NTM,KMIN,WELP,RMINP,NWE,WSOL,
     &       RWERP%R(1))
             DO I=1,NTM
                IF(WELT(I).LT.HUGE(1.)) THEN
                   DIF=HUGE(1.)
                   JMIN=0
                   DO J=I,NTM
                      DFF=ABS(WELT(I)-WELP(J))
                      IF(DFF.LT.DIF) THEN
                         DIF=DFF
                         JMIN=J
                       ENDIF
                   ENDDO
                   IF(JMIN.GT.0) THEN
                     WWW=WELP(I)
                     WELP(I)=WELP(JMIN)
                     WELP(JMIN)=WWW
                     WWW=RMINP(I)
                     RMINP(I)=RMINP(JMIN)
                     RMINP(JMIN)=WWW
                   ENDIF
                ENDIF
             ENDDO  
             DO I=1,NTM
                IF(RMINP(I).LT.HUGE(1.).AND.
     &          RMINT(I).LT.HUGE(1.)) THEN
                   WERT(17+(I-1)*4)=WELP(I)
                   WERT(19+(I-1)*4)=RMINP(I)
                   CPR=AABEE(RMINP(I),KW,1.D0,IRT)
                   CTR=AABEE(RMINT(I),KW,1.D0,IRT)
                   WERT(18+(I-1)*4)=CPR/(CTR+10.*TINY(1.))
                   WERT(20+(I-1)*4)=CTR/(CPR+10.*TINY(1.))
                ENDIF
             ENDDO 
        ENDIF
      ENDIF
C
C 
C     transparente Schichten   
C
C     FST1    FARBSTAERKE NACH MINIMALEM X,Y,Z 
C     (LAMBERT-BEER TRANSPARENT)
C     FST2    FARBSTAERKE SUMME LOG(R)*(X(LAMBDA+Y(LAMBDA)+Z(LAMBDA)) 
C     FST3    FARBSTAERKE Y-Angleich
C     FST4    FARBSTAERKE SUMME K/S
C
C     BEI DER VERWENDUNG VON XYZU ALS QUSI-REFLEXIONSWERT WIRD NICHT ZWISCHEN 
C     KORRIGIERTEN UND GEMESSENEN REFLEXIONSWERTEN DES UNTERGRUNDES
C     UNTERSCHIEDEN (FASTR BZW.FATRA) 
C
C
C        EXTINKTION
C
C
         IJ=0
C
C        MIN(X,Y,Z)
C
         CALL FATRA(XYZT,XYZP,XYZU,
     &              CT,CP,EXT,FS1,KNO,IRT,KW,IJ)
         FAE1=1./(FS1+TINY(1.))
         EXKS1=EXT
C
C        Gewichtete Summe
C
C
         CALL FALMB(NWS(),
     &        RWERT%R(1),RWERP%R(1),
     &        RWERUK%R(1),
     &              CT,CP,EXKS2,EXKS4,FS2,FS4,KNO,IRT,KW)
         FAE2=1./(FS2+TINY(1.0))
         FAE4=1./(FS4+TINY(1.0))
         IJ=2
C
C        Y-Angleich
C         
         CALL FATRA(XYZT,XYZP,XYZU,
     &              CT,CP,EXT,FS3,KNO,IRT,KW,IJ)
         FAE3=1./(FS3+TINY(1.))
         EXKS3=EXT
         WERT(1)=FS1 
         WERT(2)=FS2
         WERT(3)=FS3
         WERT(4)=FS4
         WERT(5)=FAE1
         WERT(6)=FAE2
         WERT(7)=FAE3
         WERT(8)=FAE4
         WERT(9)=EXKS1
         WERT(10)=EXKS2
         WERT(11)=EXKS3
         WERT(12)=EXKS4
         IF(ITP.EQ.1) THEN
           EXKS1T=EXKS1
           EXKS2T=EXKS2
           EXKS3T=EXKS3
           EXKS4T=EXKS4
         ELSE
           WERT(13)=EXKS1-EXKS1T
           WERT(14)=EXKS2-EXKS2T
           WERT(15)=EXKS3-EXKS3T
           WERT(16)=EXKS4-EXKS4T
         ENDIF
C
C
      RETURN
      END
C
C 
C 
C                             +--------+ 
C                             ! LISTAD !
C                             +--------+ 
      SUBROUTINE LISTAD(ITP,KNO,XYZT,XYZP,XYZU,
     &           RWERT,RWERP,RWERUN,RWERUK,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODILLU

C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP,RWERUN,RWERUK
      REAL(KIND=8),DIMENSION(*):: WERT
      TYPE (TYPARAM) PARAM
      SAVE CT,WELT,RMINT,EXKS1T,EXKS2T,EXKS3T,EXKS4T
      DIMENSION WELT(5),RMINT(5),WELP(5),RMINP(5)
      INTEGER KMIN(5)
      DIMENSION XYZT(3),XYZP(3),XYZU(3)
C
      DATA NTM/5/
C
      IRT=RWERP%RETR
      IF(IRT.EQ.1) THEN
C
C        TRANSPARENTE SCHICHTEN WERDEN HIER NICHT AUSGEWERTET
C
C
         RETURN
      ENDIF
C
C
      CT=1.
      CP=1.
C
C
C 
C     LISTE:  F A R B S T A E R K E  (PRIMITIV) AUSWERTUNG 
C 
C     Deckende Schichten
C 
C 
C 
C 
C 
C 
C 
C 
C
      IF(ITP.EQ.1) THEN
      CT=1.
        IF(KNO.EQ.1) THEN
           CALL RWMINI(NTM,KMIN,WELT,RMINT,NWE,WSOL,RWERP%R(1))
           DO I=1,NTM
              IF(RMINT(I).NE.1.D0*HUGE(1.)) THEN
                 WERT(17+(I-1)*4)=WELT(I)
                 WERT(19+(I-1)*4)=RMINT(I)
                 WERT(18+(I-1)*4)=1.
                 WERT(20+(I-1)*4)=1.
               ENDIF
           ENDDO 
        ENDIF
C
C
C
C
C
C     FARBSTAERKE (WELLENLAENGENABHAENGIG)
C
      ELSE
C
        IF(KNO.EQ.1) THEN
           CALL RWMINI(NTM,KMIN,WELP,RMINP,NWE,WSOL,RWERP%R(1))
           DO I=1,NTM
              IF(WELT(I).NE.1.D0*HUGE(1.)) THEN
                 DIF=HUGE(1.)
                 JMIN=0
                 DO J=I,NTM
                   DFF=ABS(WELT(I)-WELP(J))
                   IF(DFF.LT.DIF) THEN
                      DIF=DFF
                      JMIN=J
                   ENDIF
                 ENDDO
                 IF(JMIN.GT.0) THEN
                   WWW=WELP(I)
                   WELP(I)=WELP(JMIN)
                   WELP(JMIN)=WWW
                   WWW=RMINP(I)
                   RMINP(I)=RMINP(JMIN)
                   RMINP(JMIN)=WWW
                 ENDIF
              ENDIF
           ENDDO  
           DO I=1,NTM
              IF(RMINP(I).NE.1.D0*HUGE(1.).AND.
     &        RMINT(I).NE.1.D0*HUGE(1.)) THEN
                 WERT(17+(I-1)*4)=WELP(I)
                 WERT(19+(I-1)*4)=RMINP(I)
                   CPR=AKUMU(RMINP(I),KW)
                   CTR=AKUMU(RMINT(I),KW)
                   WERT(18+(I-1)*4)=CPR/(CTR+10.*TINY(1.))
                   WERT(20+(I-1)*4)=CTR/(CPR+10.*TINY(1.))
              ENDIF
           ENDDO 
        ENDIF
      ENDIF
C
C 
C     deckende Schichten
C
C     FS1    FARBSTAERKE NACH MINIMALEM X,Y,Z
C     (KUBELKA-MUNK DECKEND)
C     FS2    FARBSTAERKE SUMME K/S*(X(LAMBDA)+Y(LAMBDA)+Z(LAMBDA)) (CAUSW=O)
C     FS3    FARBSTAERKE nach Y-Angleich
C     FS4    FARBSTAERKE SUMME K/S
C     BEI DER VERWENDUNG VON XYZU ALS QUASI-REFLEXIONSWERT WIRD NICHT ZWISCHEN
C     KORRIGIERTEN UND GEMESSENEN REFLEXIONSWERTEN DES UNTERGRUNDES
C     UNTERSCHIEDEN (FASTR BZW.FATRA) 
C
C
C        K-M
C
C        MIN(X,Y,Z)
C
C
         IJ=0
         CALL FASTR(XYZT,XYZP,XYZU,
     &              CT,CP,AKS,FS1,KNO,KW,IJ)
c        WRITE(1,'('' AKS '',f10.4)') AKS
         FAE1=1./(FS1+TINY(1.))
         EXKS1=AKS
C
C        Gewichtete K/S
C
C
         CALL FATXT(NWS(),RWERT%R(1),
     &   RWERP%R(1),RWERUN%R(1),
     &              CT,CP,EXKS2,EXKS4,FS2,FS4,KNO,KW)
         FAE2=1./(FS2+TINY(1.0))
         FAE4=1./(FS4+TINY(1.0))
C
C        Y-Angleich
C
C
C
C
         IJ=2
         CALL FASTR(XYZT,XYZP,XYZU,
     &              CT,CP,AKS,FS3,KNO,KW,IJ)
c        WRITE(1,'('' AKS '',f10.4)') AKS
         FAE3=1./(FS3+TINY(1.))
         EXKS3=AKS
         WERT(1)=FS1 
         WERT(2)=FS2
         WERT(3)=FS3
         WERT(4)=FS4
         WERT(5)=FAE1
         WERT(6)=FAE2
         WERT(7)=FAE3
         WERT(8)=FAE4
         WERT(9)=EXKS1
         WERT(10)=EXKS2
         WERT(11)=EXKS3
         WERT(12)=EXKS4
         IF(ITP.EQ.1) THEN
            EXKS1T=EXKS1
            EXKS2T=EXKS2
            EXKS3T=EXKS3
            EXKS4T=EXKS4
         ELSE
            WERT(13)=EXKS1-EXKS1T
            WERT(14)=EXKS2-EXKS2T
            WERT(15)=EXKS3-EXKS3T
            WERT(16)=EXKS4-EXKS4T
         ENDIF
      RETURN
      END
C
C
C                             +-------+ 
C                             ! LOPTD !
C                             +-------+ 
      SUBROUTINE LOPTD(ITP,KNO,XYZT,XYZP,RWERT,RWERP,RUNT,PARAM,WERT,
     &                 KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP,RUNT
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(*),XYZP(*)
      INTEGER(KIND=4) ::NWA
      PARAMETER (NWA=36)
      DIMENSION RP(NWA),XP(NWA),DICHTE(4)
      DIMENSION RU(NWA)
C
C     RUNT GEMESSENE R-WERTE DES UNTERGRUNDES
C
C
C
C 
C     LISTE:  O P T I S C H E   D I C H T E N               
C 
C 
C 
      IRT=RWERP%RETR
C 
C 
C 
C 
C
C
 110  XP(1)=380.
      DO I=2,NWA
         XP(I)=XP(I-1)+10.
      ENDDO
      CALL RWAKI(RWERP%R(1),NWA,XP,RP)
      CALL RWAKI(RUNT%R(1),NWA,XP,RU)
C
C     OPTISCHE DICHTE NACH DINNB (b  c  m  y   max.)
C
C
      CALL DINNB(RP,DICHTE,DIMAX,KW,RU,IRT)
      WERT(1)=DICHTE(1)
      WERT(2)=DICHTE(2)
      WERT(3)=DICHTE(3)
      WERT(4)=DICHTE(4)
      WERT(5)=DIMAX
C
C
C     OPTISCHE DICHTE NACH DIN  (b  c  m  y   max.)
C
C
      CALL DINXX(RP,DICHTE,DIMAX,KW,RU,IRT)
      WERT(6)=DICHTE(1)
      WERT(7)=DICHTE(2)
      WERT(8)=DICHTE(3)
      WERT(9)=DICHTE(4)
      WERT(10)=DIMAX
C
C
C
C     OPTISCHE DICHTE NACH ANSIT  (b  c  m  y   max.)
C
C
      CALL ANSIT(RP,DICHTE,DIMAX,KW,RU,IRT)
      WERT(11)=DICHTE(1)
      WERT(12)=DICHTE(2)
      WERT(13)=DICHTE(3)
      WERT(14)=DICHTE(4)
      WERT(15)=DIMAX
C

C
C
C
C     OPTISCHE DICHTE NACH ANSIA  (b  c  m  y   max.)
C
C
      CALL ANSIA(RP,DICHTE,DIMAX,KW,RU,IRT)
      WERT(16)=DICHTE(1)
      WERT(17)=DICHTE(2)
      WERT(18)=DICHTE(3)
      WERT(19)=DICHTE(4)
      WERT(20)=DIMAX
C
C
C
      RETURN
      END
C                             +-------+ 
C                             ! LIANS ! 
C                             +-------+ 
      SUBROUTINE LIANS(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(3),XYZP(3)
      DIMENSION XYZH(3)
C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)
C
C
C
C
C 
C     LISTE:  ADAMS,NICKERSON,STULTZ (AN40)
C 
C 
C 
C 
C 
C 
C 
C 
      IF(ITP.EQ.2) THEN
          CALL DEANS(XYZT,XYZP,DE,DL,DC,DS,DT,KNO)     
          IF(KNO.NE.1) THEN 
            CALL XYZNOR(XYZH,RWERT%R(1),RWERP%R(1),
     &      KNO,GKK)
              CALL DEANS(XYZT,XYZH,AMA,ADL,ADC,ADS,ADT,KNO)     
              WERT(1)=AMA
          ENDIF
          WERT(2)=DE        
          WERT(3)=DL
          WERT(4)=DC
          WERT(5)=DS
          WERT(6)=DT
      ENDIF
C
      RETURN
      END
C                             +-------+ 
C                             ! LIFMC ! 
C                             +-------+ 
      SUBROUTINE LIFMC(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(3),XYZP(3),XYZH(3)
C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)
C
C
C
C 
C     LISTE:  F M C 2 FRIELE,MAC ADAM,CHICKERING)
C 
C 
C 
C 
C 
C 
C 
C 
C 
      IF(ITP.EQ.2) THEN
         CALL DEFMC(XYZT,XYZP,DE,DL,DRG,DYB)               
         IF(KNO.NE.1) THEN 
            CALL XYZNOR(XYZH,RWERT%R(1),RWERP%R(1),
     &      KNO,GKK)
            CALL DEFMC(XYZT,XYZH,AMA,ADL,ADRG,ADYB)               
            WERT(1)=AMA
         ENDIF
         WERT(2)=DE        
         WERT(3)=DL
         WERT(4)=DRG
         WERT(5)=DYB
      ENDIF
      RETURN
      END
C 
C                             +-------+ 
C                             ! LIHUN ! 
C                             +-------+ 
      SUBROUTINE LIHUN(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(3),XYZP(3),XYZH(3)
      DIMENSION AHLAB(3)
C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)
C
C
C
C
C 
C     LISTE:  H U N T E R             
C 
C 
      IF(ITP.EQ.1) THEN
         CALL HLAB(XYZT,AHLAB,KNO)      
         WERT(1)=AHLAB(1)  
         WERT(2)=AHLAB(2)
         WERT(3)=AHLAB(3)
         WERT(9)=SQRT(WERT(2)**2+WERT(3)**2)
         IF(KABS.NE.0) THEN
            WERT(6)=AHLAB(1)
            WERT(7)=AHLAB(2)
            WERT(8)=AHLAB(3)
            WERT(10)=WERT(9)
         ENDIF
C 
      ELSE
C 
         CALL HLAB(XYZP,AHLAB,KNO)   
         CALL HULAB(XYZT,XYZP,DE,DL,DA,DB,DC,DH,KNO)               
         IF(KNO.NE.1) THEN 
            CALL XYZNOR(XYZH,RWERT%R(1),RWERP%R(1),
     &      KNO,GKK)
            CALL HULAB(XYZT,XYZH,AMA,ADL,ADA,ADB,ADC,ADH,KNO)               
            WERT(4)=AMA
         ENDIF
         WERT(1)=AHLAB(1)  
         WERT(2)=AHLAB(2)
         WERT(3)=AHLAB(3)
         WERT(5)=DE
         WERT(6)=DL
         WERT(7)=DA
         WERT(8)=DB
         WERT(9)=SQRT(WERT(2)**2+WERT(3)**2)
         WERT(10)=DC
         WERT(11)=DH
      ENDIF
C
      RETURN
      END
C                             +-------+ 
C                             ! LISIM ! 
C                             +-------+ 
      SUBROUTINE LISIM(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(3),XYZP(3),XYZH(3)
C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)
C
C
C
C 
C     LISTE:  MAC ADAM,SIMON,GOODWIN                  
C 
C 
C 
C 
C 
C 
C 
      IF(ITP.EQ.2) THEN
         CALL SIMGOO(XYZT,XYZP,DE,DL,DCC,DCU)                
         IF(KNO.NE.1) THEN 
            CALL XYZNOR(XYZH,RWERT%R(1),RWERP%R(1),
     &      KNO,GKK)
            CALL SIMGOO(XYZT,XYZH,AMA,ADL,ACC,ACU)                 
            WERT(1)=AMA
         ENDIF
         WERT(2)=DE        
         WERT(3)=DL
         WERT(4)=DCC
         WERT(5)=DCU
      ENDIF
      RETURN
      END
C                             +-------+ 
C                             ! LINBS ! 
C                             +-------+ 
      SUBROUTINE LINBS(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(3),XYZP(3),XYZH(3)
C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)
C
C
C
C 
C     LISTE:  J U D D - H U N T E R (NBS)
C 
C 
C 
C 
C 
C 
C 
C 
      IF(ITP.EQ.2) THEN
         CALL JUDHUN(XYZT,XYZP,DE,DL,DC)
         IF(KNO.NE.1) THEN 
            CALL XYZNOR(XYZH,RWERT%R(1),RWERP%R(1),
     &      KNO,GKK)
            CALL JUDHUN(XYZT,XYZH,AMA,DLM,DLC)
            WERT(1)=AMA
         ENDIF
         WERT(2)=DE
         WERT(3)=DL
         WERT(4)=DC
      ENDIF
      RETURN
      END
C
      FUNCTION IUNTGR(CCC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*1 CCC
      IUNTGR=1
      IF(CCC.EQ.'S') THEN
         IUNTGR=2
      ENDIF
      RETURN
      END
      
C

    
C******************************************************************************
C                             +-------+
C                             ! L6175M !
C                             +-------+
      SUBROUTINE L6175M(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODFAKT
      USE MODWINK
C
C     Toleranzen für Automobillack (Mehrwinkel)
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(*),XYZP(*)
      DIMENSION ALCHT(3),ALCHP(3),ALABT(3),ALABP(3)
      DO I=1,KMS()
        IF(IHRMWIN(I).GT.1000) THEN
C
C          KEIN WINKELABHÄNGIGES GERÄT
C
           GOTO 900
        ENDIF
      END DO
C
      IF(ITP.EQ.1) THEN
         RETURN
      ENDIF
      CALL LCH(XYZT,ALABT,ALCHT,KNO)
      CALL LCH(XYZP,ALABP,ALCHP,KNO)

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
      WIN=IHRMWIN(KW)
      SL=0.15*SQRT(ALM)+31.5/WIN
      SC=MAX(0.7,0.48*SQRT(ABS(ACM))-0.35*SQRT(ABS(ALM))+42./WIN)
      SH=MAX(0.7,0.14*SQRT(ABS(ACM))-0.20*SQRT(ABS(ALM))+21./WIN+0.7)
      SA=0.7
      SB=0.7
C

C
C     DEab
C
      CALL DELAB(XYZT,XYZP,DE,DL,DC,DH,DA,DB,KNO)

C
      DEAB=SQRT(ABS((DL/(GL*SL))**2+(DA/(GA*SA))**2+(DB/(GB*SB))**2))
      DECH=SQRT(ABS((DL/(GL*SL))**2+(DC/(GC*SC))**2+(DH/(GH*SH))**2))
C
C
C
C
      C0=10.+8./(1.+EXP(27.-ALM))
      SIG=1./(1.+EXP(ACM-C0))
C
C
C
      WERT(1)=SIG*DEAB+(1.-SIG)*DECH
 900  RETURN
      END
C                             +-------+
C                             ! LISP1 !
C                             +-------+
      SUBROUTINE LISP1(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODFAKT

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(*),XYZP(*),XYZ(3)
      SAVE AMYGSV,AGYGSV
C
C      IRT=RWERP%RETR
C      GKK=GLZNOG(KW,IRT)
C
C
      IF(ITP.EQ.1) THEN
        DO I=1,3
           XYZ(I)=XYZT(I)
        END DO
      ELSEIF(ITP.EQ.2) THEN
        DO I=1,3
           XYZ(I)=XYZP(I)
        END DO
      ENDIF
C
C
C
C        SCHWARZZAHLEN
C
C
C
         AMC=0.
         DO I=1,3
            ALLS=100.*LOG10(FAKT(I,KNO)/(XYZ(I)+10.*TINY(1.)))
            IF(I.EQ.1) THEN
                AMC=AMC+ALLS
            ELSE IF(I.EQ.2) THEN
                AMC=AMC+ALLS
                AMY=ALLS
            ELSE IF(I.EQ.3) THEN
                AMC=AMC-ALLS
            ENDIF
         ENDDO
C
C        SCHWARZWERTE
C
C
         WERT(1)=AMC
         WERT(2)=AMY
         WERT(3)=AMC-AMY
C
C        SCHWARZZAHL (korrigiert) und GRAUZAHL (korrigiert)
C        (korrigiert mit Parametern AMYGS,AMYGSF,AGYGS,AGYGSF aus MODFAKT)
C
C
         IF(ITP.EQ.1) THEN
             AMYR=100.*AMY/AMYGSF
             AMYGSV=AMYR
             AGYR=100.*AMY/AGYGSF
             AGYGSV=AGYR
         ELSEIF(ITP.EQ.2) THEN
             AMYR=100.*AMY/AMYGS
             AGYR=100.*AMY/AGYGS
         ENDIF
C
         WERT(4)=AMYR
         WERT(5)=100.*AMYR/(AMYGSV+10*TINY(1.))
         WERT(6)=AGYR
         WERT(7)=100.*AGYR/(AGYGSV+10*TINY(1.))
      RETURN
      END
c
C                             +-------+
C                             ! LIKUN !
C                             +-------+
      SUBROUTINE LIKUN(ITP,KNO,XYZT,XYZP,RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(*),XYZP(*)
C
      IF(ITP.EQ.1) THEN
        DO I=1,3
           WERT(59+I)=XYZT(I)
        END DO
      ELSEIF(ITP.EQ.2) THEN
        DO I=1,3
           WERT(59+I)=XYZP(I)
        END DO
      ENDIF

      RETURN
      END
C
C                             +-------+
C                             ! LICIE9 !
C                             +-------+
      SUBROUTINE LICIE9(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE(SRWERT) RWERT,RWERP
      TYPE(TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      SAVE DETW,DETS,CROMT,DEPW
      CHARACTER*8 CHWERT
      DIMENSION XYZT(3),XYZP(3),XYZW(3),XYZS(3),XYZD(3),XYZA(3)
      DIMENSION XYZPW(3),XYZPS(3),XYZTW(3),XYZTS(3),XYZKW(3),XYZKS(3)
      DIMENSION CIE(3),CIA(3)
C
C
      IRT=RWERP%RETR
      GKK=GLZNOG(KW,IRT)
C
C
C
C      Farbkonstanz-Index (D=1.)
C      (s. Georg A. Klein: Farbenphysik für industrielle Anwendungen (S. 92 und S. 304)
C
      CALL NOGXX(XYZD,1,RWERP%R,GKK)
      CALL NOGXX(XYZA,KNO,RWERP%R,GKK)
      D=1.0
      CALL FKICIE(1,KW,KNO,D,XYZD,XYZA,WERT(21))
C
C
C     LISTE:  C I E L A B  -  WERTE (DIN6167)
C
C
C
C
C
C
C
      DO  I=1,3
      IF(RWERP%CART(2:2).EQ.'T') THEN
        XYZKW(I)=XYZTW(I)
        XYZKS(I)=XYZTS(I)
      ELSEIF(RWERP%CART(2:2).EQ.'P') THEN
        XYZKW(I)=XYZPW(I)
        XYZKS(I)=XYZPS(I)
      ENDIF
      XYZW(I)=FAKT(I,KNO)
      XYZS(I)=0.
      ENDDO
C
C
C
C
C
C
C
      IF(ITP.EQ.1) THEN
         CALL LCH9(XYZT,CIE,CIA,KNO)
         CALL DELAB9(XYZT,XYZW,DEW,DLW,DCW,DHW,DAW,DBW,KNO)
         CALL DELAB9(XYZT,XYZS,DES,DLS,DCS,DHS,DAS,DBS,KNO)
         WERT(1)=CIA(1)
         WERT(2)=CIA(2)
         WERT(3)=CIA(3)
         WERT(4)=CIE(2)
         WERT(5)=CIE(3)
         CROMT=CIA(2)
         IF(RWERP%CART(4:4).EQ.'S') THEN
C
C           STREU-DE
C
            WERT(7)=DES
            IF(RWERP%CART(2:2).EQ.'T') THEN
              DETS=DES
            ELSE
              DDE=DES-DETS
              WERT(8)=DDE
            ENDIF

C
            IF(XYZKW(1).GE.0..AND.XYZKW(2).GE.0..AND.XYZKW(3).GE.0.)THEN

C             KONTRAST DELTA-E
              CALL DELAB9(XYZKW,XYZKS,DEK,DLK,DCK,DHK,DAK,DBK,KNO)
C
              WERT(20)=DEK
C
C             LASURVERMÖGEN
C
              WERT(19)=DETW/(DES+10.*TINY(1.D0))
            ENDIF
         ELSE
            WERT(6)=DEW
            DETW=DEW
         ENDIF
C
C
C
         IF(KABS.NE.0) THEN
            WERT(11)=CIA(1)
            WERT(12)=CIA(2)
            WERT(13)=CIA(3)
            WERT(14)=CIE(2)
            WERT(15)=CIE(3)
C
          ENDIF
C
C         FARBTONWINKEL WIRD IN CHARACTER ANGEGEBEN (R Y B G usw.)
C
          IF(CIA(2).GT.0.2) THEN
             CALL STICIE(CIA(3),CHWERT)
          ELSE
             CHWERT='  FREI  '
          ENDIF
C
C         Farbtonwinkel
C
C
C
          WERT(16)=CHRWRT(CHWERT)
C
C
      ELSE
C
C
C
          CALL LCH9(XYZP(1),CIE,CIA,KNO)
          CALL DELAB9(XYZP,XYZW,DEW,DLW,DCW,DHW,DAW,DBW,KNO)
          CALL DELAB9(XYZP,XYZS,DES,DLS,DCS,DHS,DAS,DBS,KNO)
          WERT(1)=CIA(1)
          WERT(2)=CIA(2)
          WERT(3)=CIA(3)
          WERT(4)=CIE(2)
          WERT(5)=CIE(3)
          IF(RWERP%CART(4:4).EQ.'S') THEN
C
C           STREU DE
C
C
            WERT(7)=DES
            IF(RWERP%CART(2:2).EQ.'T') THEN
              DETS=DES
            ELSE
              DDE=DES-DETS
              WERT(8)=DDE
            ENDIF
C
            IF(XYZKW(1).GE.0..AND.XYZKW(2).GE.0..AND.XYZKW(3).GE.0.)THEN
C             KONTRAST DELTA-E
C
              CALL DELAB9(XYZKW,XYZKS,DEK,DLK,DCK,DHK,DAK,DBK,KNO)
              WERT(20)=DEK
C
C             LASURVERMÖGEN
C
              WERT(19)=DEPW/(DES+10.*TINY(1.D0))
            ENDIF
          ELSE
              WERT(6)=DEW
              DEPW=DEW
          ENDIF
C
          IF(KNO.NE.1) THEN
            CALL METAM9(RWERT%R(1),RWERP%R(1),
     &      GKK,AMA,1,KNO)
            WERT(9)=AMA
          ENDIF
          CALL DELAB9(XYZT,XYZP,DE,DL,DC,DH,DA,DB,KNO)
          WERT(10)=DE
          WERT(11)=DL
          WERT(12)=DC
          WERT(13)=DH
          WERT(14)=DA
          WERT(15)=DB
C
C     FARBTONWINKEL WIRD IN CHARACTER ANGEGEBEN (R Y B G usw.)
C
          IF(CIA(2).GT.0.2) THEN
             CALL STICIE(CIA(3),CHWERT)
          ELSE
             CHWERT='  FREI  '
          ENDIF
C
C         Farbtonwinkel

          WERT(16)= CHRWRT(CHWERT)
C
C
          STREL=SQRT(DA**2+DB**2)
          IF(STREL.LE.0.2) THEN
             CHWERT='  FREI  '
          ELSE IF(CIA(2).LT.10..AND.CROMT.LT.10.) THEN
             HREL=WNGRD(DA,DB)
             CALL STICIE(HREL,CHWERT)
          ELSE
             CHWERT='  >10.  '
          ENDIF
          WERT(17)=STREL
C
C         rel. Farbstich
          WERT(18)=CHRWRT(CHWERT)
C
C
      ENDIF
      RETURN
      END
C     Last change:  KLA   9 Nov 116    2:02 pm
C
C                             +-------+
C                             ! LICMC !
C                             +-------+
      SUBROUTINE LICMC(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE(SRWERT) RWERT,RWERP
      TYPE(TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      SAVE DETW,DETS,CROMT,DEPW
      CHARACTER*8 CHWERT
      DIMENSION XYZT(3),XYZP(3),XYZW(3),XYZS(3),XYZD(3),XYZA(3)
      DIMENSION XYZPW(3),XYZPS(3),XYZTW(3),XYZTS(3),XYZKW(3),XYZKS(3)
      DIMENSION CIE(3),CIA(3)
C
C
      IRT=RWERP%RETR
      GKK=GLZNOG(KW,IRT)
C
C
C
C      Farbkonstanz-Index (D=1.)
C      (s. Georg A. Klein: Farbenphysik für industrielle Anwendungen (S. 92 und S. 304)
C
      CALL NOGXX(XYZD,1,RWERP%R,GKK)
      CALL NOGXX(XYZA,KNO,RWERP%R,GKK)
      D=1.0
      CALL FKICIE(2,KW,KNO,D,XYZD,XYZA,WERT(21))
C
C
C     LISTE:  CMC modifiziert nach Colli
C
C
C
C
C
C
C
      DO  I=1,3
      IF(RWERP%CART(2:2).EQ.'T') THEN
        XYZKW(I)=XYZTW(I)
        XYZKS(I)=XYZTS(I)
      ELSEIF(RWERP%CART(2:2).EQ.'P') THEN
        XYZKW(I)=XYZPW(I)
        XYZKS(I)=XYZPS(I)
      ENDIF
      XYZW(I)=FAKT(I,KNO)
      XYZS(I)=0.
      ENDDO
C
C
C
C
C
C
C
      IF(ITP.EQ.1) THEN
         CALL CICMC(XYZT,CIE,CIA,KNO)
         CALL DECMC(XYZT,XYZW,DEW,DLW,DCW,DHW,DAW,DBW,KNO)
         CALL DECMC(XYZT,XYZS,DES,DLS,DCS,DHS,DAS,DBS,KNO)
         WERT(1)=CIA(1)
         WERT(2)=CIA(2)
         WERT(3)=CIA(3)
         WERT(4)=CIE(2)
         WERT(5)=CIE(3)
         CROMT=CIA(2)
         IF(RWERP%CART(4:4).EQ.'S') THEN
C
C           STREU-DE
C
            WERT(7)=DES
            IF(RWERP%CART(2:2).EQ.'T') THEN
              DETS=DES
            ELSE
              DDE=DES-DETS
              WERT(8)=DDE
            ENDIF

C
            IF(XYZKW(1).GE.0..AND.XYZKW(2).GE.0..AND.XYZKW(3).GE.0.)THEN

C             KONTRAST DELTA-E
              CALL DECMC(XYZKW,XYZKS,DEK,DLK,DCK,DHK,DAK,DBK,KNO)

C
              WERT(20)=DEK
C
C             LASURVERMÖGEN
C
              WERT(19)=DETW/(DES+10.*TINY(1.D0))
            ENDIF
         ELSE
            WERT(6)=DEW
            DETW=DEW
         ENDIF
C
C
C
         IF(KABS.NE.0) THEN
            WERT(11)=CIA(1)
            WERT(12)=CIA(2)
            WERT(13)=CIA(3)
            WERT(14)=CIE(2)
            WERT(15)=CIE(3)
C
          ENDIF
C
C         FARBTONWINKEL WIRD IN CHARACTER ANGEGEBEN (R Y B G usw.)
C
          IF(CIA(2).GT.0.2) THEN
             CALL STICIE(CIA(3),CHWERT)
          ELSE
             CHWERT='  FREI  '
          ENDIF
C
C
C
C         Farbtonwinkel
C
          WERT(16)=CHRWRT(CHWERT)
C
C
      ELSE
C
C
C
          CALL CICMC(XYZP(1),CIE,CIA,KNO)
          CALL DECMC(XYZP,XYZW,DEW,DLW,DCW,DHW,DAW,DBW,KNO)
          CALL DECMC(XYZP,XYZS,DES,DLS,DCS,DHS,DAS,DBS,KNO)
          WERT(1)=CIA(1)
          WERT(2)=CIA(2)
          WERT(3)=CIA(3)
          WERT(4)=CIE(2)
          WERT(5)=CIE(3)
          IF(RWERP%CART(4:4).EQ.'S') THEN
C
C           STREU DE
C
C
            WERT(7)=DES
            IF(RWERP%CART(2:2).EQ.'T') THEN
              DETS=DES
            ELSE
              DDE=DES-DETS
              WERT(8)=DDE
            ENDIF
C
            IF(XYZKW(1).GE.0..AND.XYZKW(2).GE.0..AND.XYZKW(3).GE.0.)THEN
C             KONTRAST DELTA-E
C
              CALL DECMC(XYZKW,XYZKS,DEK,DLK,DCK,DHK,DAK,DBK,KNO)

              WERT(20)=DEK
C
C             LASURVERMÖGEN
C
              WERT(19)=DEPW/(DES+10.*TINY(1.D0))
            ENDIF
          ELSE
              WERT(6)=DEW
              DEPW=DEW
          ENDIF
C
          IF(KNO.NE.1) THEN
            CALL METCMC(RWERT%R(1),RWERP%R(1),
     &      GKK,AMA,1,KNO)

            WERT(9)=AMA
          ENDIF
          CALL DECMC(XYZT,XYZP,DE,DL,DC,DH,DA,DB,KNO)
          WERT(10)=DE
          WERT(11)=DL
          WERT(12)=DC
          WERT(13)=DH
          WERT(14)=DA
          WERT(15)=DB
C
C     FARBTONWINKEL WIRD IN CHARACTER ANGEGEBEN (R Y B G usw.)
C
          IF(CIA(2).GT.0.2) THEN
             CALL STICIE(CIA(3),CHWERT)
          ELSE
             CHWERT='  FREI  '
          ENDIF
C
C         Farbtonwinkel

          WERT(16)= CHRWRT(CHWERT)
C
C
          STREL=SQRT(DA**2+DB**2)
          IF(STREL.LE.0.2) THEN
             CHWERT='  FREI  '
          ELSE IF(CIA(2).LT.10..AND.CROMT.LT.10.) THEN
             HREL=WNGRD(DA,DB)
             CALL STICIE(HREL,CHWERT)
          ELSE
             CHWERT='  >10.  '
          ENDIF
          WERT(17)=STREL
C
C         rel. Farbstich
          WERT(18)=CHRWRT(CHWERT)
C
C
      ENDIF
      RETURN
      END
c

c
c
C                             +-------+
C                             ! LI2000 !
C                             +-------+ 
      SUBROUTINE LI2000(ITP,KNO,XYZT,XYZTW,XYZTS,XYZP,XYZPW,XYZPS,
     &           RWERT,RWERP,PARAM,WERT,KW,IER)
      USE MOTWERT
      USE MOSRWRT

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE (SRWERT) RWERT,RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DIMENSION XYZT(3),XYZP(3)
      DIMENSION XYZPW(3),XYZPS(3),XYZTW(3),XYZTS(3)
      DIMENSION XYZH(3)

C
C
      IRT=RWERP%RETR
      GKK= GLZNOG(KW,IRT)
C
C
C
C 
C     LISTE:   CIE2000  -  WERTE
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
      IF(ITP.EQ.2) THEN
C
C 
          IF(KNO.NE.1) THEN 
            CALL XYZNOR(XYZH,RWERT%R(1),RWERP%R(1),
     &      KNO,GKK)
            CALL DC2000(XYZT,XYZH,AMA,ADL,ADC,ADH,ADBL,KNO)
            WERT(1)=AMA
          ENDIF
C 
C 
C 
C 
C
          CALL DC2000(XYZT,XYZP,DE,DL,DC,DH,DBL,KNO)
C         CIE2000
C
          WERT(3)=DL
          WERT(4)=DC
          WERT(5)=DH
          WERT(2)=DE
          WERT(6)=DBL
C
      ENDIF
      RETURN
      END



c
      SUBROUTINE LIDEDK(ITP,DIKA,DIKN,DEWESC,
     &                  RWERP,PARAM,WERT)
      USE MOTWERT
      USE MOSRWRT
      USE MODQUAL

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE DIKT,DEDW0T,DEDS0T,DEDST,DEDWT
C
C
      TYPE(SRWERT)  RWERP(*)
      TYPE(TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*)::WERT
      REAL(KIND=8),DIMENSION(*) :: DEWESC

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
C     
C
C
      IF(ITP.EQ.1) THEN
            DREL=1.
            DIKT=DIKN   
            DRABS=1.
            DRSTR=1.
            DRABS0=1.
            DRSTR0=1.
            DEDW0T=DEDW0
            DEDS0T=DEDS0
            DEDWT=DEDW
            DEDST=DEDS
      ELSE
            DREL=DIKN/(DIKT+TINY(1.))                 
            DRABS0=DEDW0T/(DEDW0+TINY(1.))
            DRSTR0=DEDS0T/(DEDS0+TINY(1.))
            DRABS=DEDWT/(DEDW+TINY(1.))
            DRSTR=DEDST/(DEDS+TINY(1.))
      ENDIF
      DECK=1./(DREL+TINY(1.))
C
C     WERTE FUER DICKE=0.
C
      STRE0=1./(DRSTR0+TINY(1.))  
      STAE0=1./(DRABS0+TINY(1.))
C
C     DIW0=DABS0
C     DIS0=DSTR0
C
      DIW0=1./(DEDW0+TINY(1.))
      DIS0=1./(DEDS0+TINY(1.))
      ALALB0=DEDS0/(DEDS0+DEDW0+TINY(1.))
C
C     WERTE FUER DICKE=1.
C
      STRE=1./(DRSTR+TINY(1.))  
      STAE=1./(DRABS+TINY(1.))
C
C     DIW=DABS
C     DIS=DSTR
C
      DIW=1./(DEDW+TINY(1.))
      DIS=1./(DEDS+TINY(1.))
      ALALB =DEDS/(DEDS+DEDW+TINY(1.))
      QLW=0.0
      QLS=0.0
C
C
C
C
C
C
C
C
C
      WERT(1)=DIKN
      WERT(2)=DIKA
      WERT(3)=DREL
      WERT(4)=DECK
      WERT(5)=DEDW0
      WERT(6)=DEDS0
      WERT(7)=DIW0
      WERT(8)=DIS0
      WERT(9)=DRABS0
      WERT(10)=DRSTR0
      WERT(11)=STAE0
      WERT(12)=STRE0
      WERT(13)=ALALB0
      WERT(14)=DEDW
      WERT(15)=DEDS
      WERT(16)=DIW
      WERT(17)=DIS
      WERT(18)=DRABS
      WERT(19)=DRSTR
      WERT(20)=STAE
      WERT(21)=STRE
      WERT(22)=ALALB
      IF(DCROM.NE.HUGE(1.)) THEN
         WERT(23)=DCROM
      ENDIF
      WERT(24)=QLW
      WERT(25)=QLS
      WERT(26)=DIS0*DEDW0
      WERT(27)=DIS*DEDW
      WERT(28)=CF(1)
      WERT(29)=CF(2)
      WERT(30)=DEWESC(1)
      WERT(31)=DEWESC(2)

C
C
C
C
C
C
C
      RETURN
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
C******************************************************************************
C
C                           +-------+ 
C                           ! FDEDK ! 
C                           +-------+ 
      SUBROUTINE FDEDK(NWE,DIK,RWERP,ITP,IER)
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C

C 
C 
C
C
      TYPE(SRWERT) RWERP(2)

      DIMENSION R(NWE,2),RHI(NWE,2),CWS(2),ASNULL(NWE)
      EXTERNAL FDDK
      DATA RAN/1.E8/
      DATA FDI/0.001/,FD0/1.E-4/
C 
      KENN=1
      GOTO 10
      ENTRY FDEDK00(NWE,DIK,RWERP,ITP,IER)
      KENN=0
 10   FDJ=1.+FDI

C 
      IER=0
      KW=KWS()
      IRT=RWERP(1)%RETR
      GKK=GLZNOG(KW,IRT)
C
C
C
      DO I=1,NWE
        ASNULL(I)=0.0
      END DO
      DO J=1,2
         DO I=1,NWE
           R(I,J)=RWERP(J)%R(I)
         ENDDO
      ENDDO
C
C
C     RWERTE FUER KLEINE AENDERUNG
C
      CWS(1)=FDJ*CF(1)
      CWS(2)=FDJ*CF(2)
      CALL RWWRTDE2(CWS,NWE,RP,RUR,A,S,IER)
C
C     RWERTE FUER KLEINE ABWEICHUNG VOM PURWEISS BZW PURSCHWARZ
C
C
      CWS(1)=FD0*CF(1)
      CWS(2)=FD0*CF(2)
      CALL RWWRTDE2(CWS,NWE,RHI,RUR,A,S,IER)
C
C
C
C
C
C     BERECHNUNG DER EMPFINDLICHKEIT DE/D(DICKE) FUER DICKE=MESSUNG
C
C
C
      CALL EMDEDD(JABST,NWE,RP,RQ,FDI,DEDW,DEDS,GKK)
C
C     
C     
C
C
C     BERECHNUNG DER EMPFINDLICHKEIT DE/D(DICKE) FUER DICKE=0.      
C
C
C
      CALL EMDEDD(JABST,NWE,RHI,RUM,FD0,DEDW0,DEDS0,GKK)
C
C
C
C 
C     BERECHNUNG DER SCHICHTDICKE FUER DECKVERMOEGEN
C 
C 
      IF(KENN.EQ.0) THEN
        RETURN
      ENDIF
   48 DMIN=0.01*DIK
      DMAX=10.*DIK+1.E-7
   40 IF (FDDK(DMIN)*FDDK(DMAX).GE.0.) THEN 
              DMAX=DMAX*10. 
              IF(DMAX.GT.RAN) GOTO 900
              GOTO 40 
      ENDIF 
      CALL INTVAL(DIK,DMIN,DMAX,FDDK,IER)
      IF(IER.NE.0) GOTO 900
C
C     REFLEXIONSWERTE FUER NICHT ABSORBIERENDES FARBMITTEL (A(BUNT)=0.) (TA,PA)
C

      CWS(1)=DIK*CF(1)
      CWS(2)=DIK*CF(2)
      CALL RWWRTDE2(CWS,NWE,ROK,RUR,ASNULL,S,IER)
C
C      REFLEXIONSWERTE FUER NICHT STREUENDES FARBMITTEL (S(BUNT)=0.) (TD,PD)
C
      CALL RWWRTDE2(CWS,NWE,ROS,RUR,A,ASNULL,IER)
C
C     REFLEXION DER HOCHPIGMENTIERTEN SCHICHT
C
      TTT=10000.*DIK
C
C     R-WERTE RUECKRECHNUNG
C

      CWS(1)=TTT*CF(1)
      CWS(2)=TTT*CF(2)
      CALL RWWRTDE2(CWS,NWE,RUN,RUR,A,S,IER)






C
C 
      RETURN
 900  CALL DELABAL(JABST,XYZP(1,1),XYZP(1,2),DE,DL,DC,DH,DA,DB,1)
      RETURN
      END 

C******************************************************************************
C
C
C******************************************************

C                            +-------+
C                            ! FDDK  !
C                            +-------+
      DOUBLE PRECISION FUNCTION FDDK(DIK)
      USE MODFUNC
      USE MODQUAL

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CWS(2)
      DATA EPS/1.D-30/
C
C

C 
C 
      KW=KWS()
C 
C 
C     BERECHNUNG DER FARBABSTANDSDIFFERENZ           
C 
C 
      CWS(1)=DIK*CF(1)
      CWS(2)=DIK*CF(2)
      CALL RWWRTDE2(CWS,NWS(),RP,RUR,A,S,IER)
C
C
C
C 
C 
      CALL NOGXX(XYZP(1,1),1,RP(1,1),GLANZ(KW))
      CALL NOGXX(XYZP(1,2),1,RP(1,2),GLANZ(KW))
      CALL DELABAL(JABST,XYZP(1,1),XYZP(1,2),DE,DL,DC,DH,DA,DB,1)
      FDDK=LOG((DE+EPS)/(FDE+EPS))
      RETURN
C
C
C
      END 
C
C
C
C
C
C
C******************************************************************************
C******************************************************************************
C******************************************************************************
C                            +--------+
C                            ! FSTDE !
C                            +--------+

C     Last change:  U    30 Dec 1998    2:20 pm
      SUBROUTINE FSTDE(C,R,ITP,IER)
      USE MODQUAL
      USE MODFUNC
      USE MODILLU
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C 
      DIMENSION R(*)   
      EXTERNAL FBWD
      DATA RANGE/1.D80/
      RANGIN=1./RANGE
C 
      IER=0
C
C     BESTIMMUNG VON RMIN FALLS ANGLEICH IM K/S-MAXIMUM      
C
C
      IF(ITP.EQ.1.AND.IART.EQ.6) THEN
         IF(WELFST.GE.WSOL(1).AND.WELFST.LE.WSOL(NWE)) THEN
            WMIN(1)=WELFST
            KMIN(1)=NWE
            DO I=1,NWE-1
              IF(WMIN(1).GE.WSOL(I).AND.WMIN(1).LT.WSOL(I+1)) THEN
                KMIN(1)=I
              ENDIF
            END DO
            RMIN(1)=RWZWIS(WMIN(1),NWE,WSOL,R)
         ELSE
            CALL RWMINI(1,KMIN,WMIN,RMIN,NWE,WSOL,R)
            RMIN(1)=RWZWIS(WMIN(1),NWE,WSOL,R)
         ENDIF
         IF(KMIN(1).LT.NWE) THEN
             WWD=(WMIN(1)-WSOL(KMIN(1)))/(WSOL(KMIN(1)+1)-WSOL(KMIN(1)))
         ELSE
             WWD=0.
         ENDIF
      ENDIF
C 
C 
C     BERECHNUNG DER G(LAMDA)-WERTE 
C 
      CALL GWRTDEK(C,IER)
      IF(IER.NE.0) GOTO 900
C 
      ITYP=ITP
      IF(ITP.EQ.1) THEN 
C 
C 
C 
C     TYPVERARBEITUNG 
C 
          IF(IART.EQ.3) THEN
C 
C 
C            KONZENTRATION WIRD FUER BTYP=0. BESTIMMT 
  
             BTYP=0.D0
             CMIN=0.01*C
             CMAX=10.*C + 1.D-7 
             FFB=FBWD(CMIN)
   20        IF (FFB*FBWD(CMAX).GE.0.D0) THEN
                 CMAX=CMAX*10.
                 IF(CMAX.GE.RANGE) GOTO 901
                 GOTO 20
             ENDIF
             CALL INTVAL(C,CMIN,CMAX,FBWD,IER)
             IF(IER.NE.0) GOTO 900
C 
          ELSE
C 
C 
C            BTYP WIRD AUS EINGEGEBENER KONZENTRATION BESTIMMT
C
             BTYP=0.
             BTYP=FBWD(C)
             IF(IART.EQ.5) THEN
C
                DO I=1,3
                   XYZT(I)=XYZF(I)
                ENDDO
C 
             ENDIF
          ENDIF 
      ELSE
C 
C 
C     PROBENVERARBEITUNG BPROBE=BTYP
C 
C 
        IF(IART.EQ.5) THEN
C
          CC=C
          CMAX=1.D6
          CMIN=0.0
          DE=BRENT(CMIN,CC,CMAX,FBWD,1.D-5,C)
        ELSE 
          CMIN=0.01*C
          CMAX=10.*C + 1.D-7
   40     IF (FBWD(CMIN)*FBWD(CMAX).GE.0.D0) THEN
              CMAX=CMAX*10. 
              IF(CMAX.GE.RANGE) GOTO 901
              GOTO 40 
          ENDIF 
          CALL INTVAL(C,CMIN,CMAX,FBWD,IER)
          IF(IER.NE.0) GOTO 900
        ENDIF
      ENDIF 
      RETURN
  901 IER=4020
      RETURN
  900 RETURN
      END 
C                            +-------+
C                            ! FBWD  !
C                            +-------+
      DOUBLE PRECISION FUNCTION FBWD(C)
      USE MODQUAL
      USE MODFUNC
      USE MODILLU
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8) ALAB(3),ALCH(3),ALABCC(3)
C
C     BERECHNUNG DER DIFFERENZEN (z.B. BPROBE-BTYP)
C     AUS DER KONZENTRATION C FUER DECKENDE SCHICHTEN
C 
C 
      IER=0
      KW=KWS()

      FBWD=0.0
      CALL RWRTDEK(C,NWE,RU,RF,GV,G,IER)
      IF(IER.NE.0) RETURN

C
C 
C 
C

      CALL NOGXX(XYZF,1,RF,GLANZ(KW))
      GOTO(10,11,12,13,13,14,15,16,17),IART+1
C
C     ANGLEICH Y
C  
  10     FBWD=XYZF(2)-BTYP
         RETURN                
C
C     ANGLEICH AUF MINIMALES X,Y,Z
C
C
C
C
C 
 11   IF(ITYP.EQ.1) THEN
                 XX=1.D80
                 IXYZT=0
                 DO 21 I=1,3
                    XF=XYZF(I)/FAKT(I,1)
                    IF(XF.GT.XX) GOTO 21
                    XX=XF
                    IXYZT=I
  21             CONTINUE
      ENDIF
      FBWD=XYZF(IXYZT)-BTYP
      RETURN
C
C
C
C     ANGLEICH AUF GEWICHTETE SUMME K/S
C
C
  12  CALL GEWICH(NWE,GV,SUMM,1)
      FBWD=SUMM-BTYP
      RETURN
C
C
C     B-WERT ANGLEICH (FIAF)
C
C
C
C
  13  CALL BWERT(XYZF,KBWT,B,1)
      FBWD=B-BTYP
      RETURN
C
C
C
C
C     GEWICHTETE MINIMIERUNG DES FARBABSTANDES
C
C
  14  IF(ITYP.EQ.1) THEN
        FBWD=0.0
      ELSE
        CALL DELABAL(JABST,XYZT,XYZF,DE,DL,DC,DH,DA,DB,1)
        DEN=SQRT((WLCH(1)*DL)**2+(WLCH(2)*DC)**2+(WLCH(3)*DH)**2)
        FBWD=DEN
      ENDIF
      RETURN
C
C     MINIMUM R-WERT
  15  FBWD=RWZWIS(WMIN(1),NWE,WSOL,RF)-RMIN(1)
      RETURN
C
C     SUMME K/S (UNGEWICHTET)
C
  16  FBWD=SUM(GV)-BTYP
      RETURN
C
C     Probe in senkrecht zur Färbequaracteristik(des Bezugs) liegenden Ebene (L,a,b)
C
C
C     L,a,b berechnen
C
C
  17  CALL LCHAL(JABST,XYZF,ALAB,ALCH,1)
      IF(ITYP.EQ.1) THEN
        DO I=1,3
          ALABT(I)=ALAB(I)
        END DO
C
C       Für Ebene senkrecht
C
        CC=1.01*C
        CALL RWRTDEK(CC,NWE,RU,RF,GV,G,IER)
        CALL NOGXX(XYZF,1,RF,GLANZ(KW))
        CALL LCHAL(JABST,XYZF,ALABCC,ALCH,1)

C
C       Normalenvektor
C
        DO I=1,3
           DALABT(I)=ALABCC(I)-ALABT(I)
        END DO
      ENDIF
      SUMM=0.0
      DO I=1,3
         SUMM=SUMM+DALABT(I)*(ALAB(I)-ALABT(I))
      END DO
      FBWD=SUMM
      RETURN
      END 
C
C
C
C
C
C

C
C     Die Unterprogramm FSTTR und FWBT (in Faker3) können später durch nachfolgende Programme ersetzt werden
C
C                           +-------+ 
C                           ! FSTTR ! 
C                           +-------+ 
      SUBROUTINE FSTTR(C,R,ITP,IER)
      USE MODFUNC
      USE MODILLU
      USE MODQUAL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
      DIMENSION R(*)   
      EXTERNAL FBWT
      DATA RANGE/1.D80/
C
C
C 
C
C
C     BESTIMMUNG VON RMIN FALLS ANGLEICH IM EXTINKTIONSMAXIMUM
C
      IER=0
C
      IF(ITP.EQ.1.AND.IART.EQ.6) THEN
         IF(WELFST.GE.WSOL(1).AND.WELFST.LE.WSOL(NWE)) THEN
            WMIN(1)=WELFST
            KMIN(1)=NWE
            DO I=1,NWE-1
              IF(WMIN(1).GE.WSOL(I).AND.WMIN(1).LT.WSOL(I+1)) THEN
                KMIN(1)=I
              ENDIF
            END DO
            RMIN(1)=RWZWIS(WMIN(1),NWE,WSOL,R)
         ELSE
            CALL RWMINI(1,KMIN,WMIN,RMIN,NWE,WSOL,R)
            RMIN(1)=RWZWIS(WMIN(1),NWE,WSOL,R)
         ENDIF
         IF(KMIN(1).LT.NWE) THEN
             WWD=(WMIN(1)-WSOL(KMIN(1)))/(WSOL(KMIN(1)+1)-WSOL(KMIN(1)))
         ELSE
             WWD=0.
         ENDIF
      ENDIF
C
C 
C 
C 
C     BERECHNUNG DER G(LAMDA)-WERTE 
C 
C
C
      CALL GWRTTRA(C,IER)
      IF(IER.NE.0) GOTO 900
C
C
C
C 
C 
      ITYP=ITP
      IF(ITP.EQ.1) THEN 
C 
C 
C 
C     TYPVERARBEITUNG 
C 
C 
          IF(IART.EQ.3) THEN 
C 
C 
C            KONZENTRATION WIRD FUER BTYP=0. BESTIMMT 
  
             BTYP=0.D0
             CMIN=0.01*C
             CMAX=10.*C + 1.D-7 
   20        IF (FBWT(CMIN)*FBWT(CMAX).GE.0.D0) THEN
                 CMAX=CMAX*10.
                 IF(CMAX.GT.RANGE) GOTO 901
                 GOTO 20
             ENDIF
             CALL INTVAL(C,CMIN,CMAX,FBWT,IER)
             IF(IER.NE.0) GOTO 900
C 
          ELSE
C 
C 
C            BTYP WIRD AUS EINGEGEBENER KONZENTRATION BESTIMMT
             BTYP=0.
             BTYP=FBWT(C) 
             IF(IART.EQ.5) THEN
C
                DO I=1,3
                   XYZT(I)=XYZF(I)
                ENDDO
C 
             ENDIF
          ENDIF 
      ELSE
C 
C 
C     PROBENVERARBEITUNG BPROBE=BTYP
C 
C 
        IF(IART.EQ.5) THEN
          CMIN=0.
          CC=C
          CMAX=1.e6
          CMIN=0.0
          DE=BRENT(CMIN,CC,CMAX,FBWT,1.D-4,C)
        ELSE
          CMIN=0.01*C
          CMAX=10.*C + 1.D-7
   40     IF (FBWT(CMIN)*FBWT(CMAX).GE.0.) THEN 
              CMAX=CMAX*10. 
              IF(CMAX.GT.RANGE) GOTO 901
              GOTO 40 
          ENDIF 
          CALL INTVAL(C,CMIN,CMAX,FBWT,IER)  
          IF(IER.NE.0) GOTO 900
        ENDIF
      ENDIF 
      RETURN
  901 IER=4020
      RETURN
  900 RETURN
      END 
C                            +-------+
C                            ! FBWT  !
C                            +-------+
      DOUBLE PRECISION FUNCTION FBWT(C)
      USE MODFUNC
      USE MODQUAL
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8) ALAB(3),ALCH(3),ALABCC(3)
C
C 
C     BERECHNUNG DER B-WERT DIFFERENZ BPROBE-BTYP 
C     AUS DER KONZENTRATION C FUER TRANSPARENTE SCHICHTEN
C 
C 
      IER=0
      FBWT=0.0
      KW=KWS()


      CALL RWRTTRA(C,NWE,RU,RF,GV,G,IER)
      IF(IER.NE.0) RETURN

C
C 
C 
      CALL NOGXX(XYZF,1,RF,GLANZ(KW))
      GOTO(10,11,12,13,13,14,15,16,17),IART+1
C
C     ANGLEICH Y
C  
  10     FBWT=XYZF(2)-BTYP
         RETURN                
C
C
C     ANGLEICH AUF MINIMALES X,Y,Z
C
C
C
C
C 
 11   IF(ITYP.EQ.1) THEN
                 XX=1.D80
                 IXYZT=0
                 DO 21 I=1,3
                 XF=XYZF(I)/FAKT(I,1)
                 IF(XF.GT.XX) GOTO 21
                 XX=XF
                 IXYZT=I
  21             CONTINUE
      ENDIF
      FBWT=XYZF(IXYZT)-BTYP
      RETURN
C
C
C
C     ANGLEICH AUF GEWICHTETE SUMME UEBER EXTINKTION
C
C
C
C
  12  CALL GEWICH(NWE,GV,SUMM,1)
      FBWT=SUMM-BTYP
      RETURN
C
C
C     B-WERT ANGLEICH (FIAF)
C
C
C
C
  13  CALL BWERT(XYZF,KBWT,B,1)
      FBWT=B-BTYP
      RETURN
C
C
C
C
C     GEWICHTETE MINIMIERUNG DES FARBABSTANDES
C
C
  14  IF(ITYP.EQ.1) THEN
        FBWT=0.0
      ELSE
        CALL DELABAL(JABST,XYZT,XYZF,DE,DL,DC,DH,DA,DB,1)
        DEN=SQRT((WLCH(1)*DL)**2+(WLCH(2)*DC)**2+(WLCH(3)*DH)**2)
        FBWT=DEN
      ENDIF
      RETURN
C
C     MINIMUM RWERT
C
C
  15  FBWT=RWZWIS(WMIN(1),NWE,WSOL,RF)-RMIN(1)
      RETURN
C
C     SUMME EXTINKTION (UNGEWICHTET)
C
C
   16 FBWT=SUM(GV)-BTYP
      RETURN
C
C     Probe in senkrecht zur Färbequaracteristik(des Bezugs) liegenden Ebene (L,a,b)
C
C
C     L,a,b berechnen
C
C
  17  CALL LCHAL(JABST,XYZF,ALAB,ALCH,1)
      IF(ITYP.EQ.1) THEN
        DO I=1,3
          ALABT(I)=ALAB(I)
        END DO
C
C       Für Ebene senkrecht
C
        CC=1.01*C
        CALL RWRTTRA(CC,NWE,RU,RF,GV,G,IER)
        CALL NOGXX(XYZF,1,RF,GLANZ(KW))
        CALL LCHAL(JABST,XYZF,ALABCC,ALCH,1)

C
C       Normalenvektor
C
        DO I=1,3
           DALABT(I)=ALABCC(I)-ALABT(I)
        END DO
      ENDIF
      SUMM=0.0
      DO I=1,3
         SUMM=SUMM+DALABT(I)*(ALAB(I)-ALABT(I))
      END DO
      FBWT=SUMM
      RETURN
      END
C
C
C
C
C
C
C
      SUBROUTINE RWRTDEK(C,NWE,RU,R,GV,G,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
C
C 
      REAL(KIND=8),DIMENSION(*) :: RU,R,GV,G
      REAL(KIND=8) :: C
      IER=0
      KW=KWS()
      DO I=1,NWE
      GV(I)=G(I)*C
      R(I)=REFDE(GV(I),1.D0,1.D0,KW)
      ENDDO
      RETURN
      END SUBROUTINE
CC
C
C
C
C
      SUBROUTINE RWRTTRA(C,NWE,RU,R,GV,G,IER)
      USE MODQUAL,ONLY:IRETRA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTERFACE
      REAL(KIND=8) FUNCTION TRFAS(A,S,RD,KW,RT,TRFAA,TRFSS)
      INTEGER(KIND=4) ::KW,RT
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::TRFAA,TRFSS
      END FUNCTION
      END INTERFACE
C
C
      REAL(KIND=8),DIMENSION(*) :: RU,R,GV,G
      REAL(KIND=8) :: C
      IER=0
      KW=KWS()
      ALB=0.
      DO I=1,NWE
        GGG=C*G(I)
        GV(I)=GGG
        TAU=GGG
        R(I)=TRFAS(TAU,0.0D0,RU(I),KW,IRETRA)
      ENDDO
      RETURN
      END SUBROUTINE
C
C
C
C

C
C
      SUBROUTINE RWRTTEX(C,NWE,GU,R,GV,G,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE

C
C
      REAL(KIND=8),DIMENSION(*) :: GU,R,GV,G
      REAL(KIND=8) :: C
      EXTERNAL REFWTA
C
C
      IER=0
C
      TAU=1.D80
      KW=KWS()
      DO I=1,NWE
        GV(I)=G(I)*C
        GGG=GU(I)+GV(I)
        R(I)=REFDE(GGG,1.D0,1.D0,KW)
      ENDDO   
      RETURN
      END SUBROUTINE
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************


c
c
c
c
c
c

C
C
C******************************************************
C******************************************************************************
      SUBROUTINE FLAQLN(IRT,KW)
      USE MODQUAL
      USE MODFUNC,ONLY:JABST

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ALAB(3),CIE(3)
C
C
C     BERECHNUNG DER FARBCHARACTERISTIK UND DER REFLEXIONSWERTE FUER 
C     MAXIMALES CHROMA SOWIE DIE ZUGEHOERIGE SCHICHTDICKE
C
C
c      DI=0.
      QLW=0.
      QLS=0.
c      QMIN=1.E30
      CMAX=0.
      IMIN=0
      IMAX=0
      GKK=GLZNOG(KW,IRT)
      DO I=1,IM
C
C        SCHICHTDICKE
C
         CDI=FCA(I,1)
C
C        Über Weiß
         SELECT CASE(IPRG)
           CASE(97,104)
             CALL RWWRTDE(1,CDI,NWS(),RF,RUR(1,1),A,S,GV,IER)
           CASE(99,105)
             CALL RWWRTTR(IRETR(1),CDI,NWS(),RF,RUR(1,1),A,S,GV,IER)
         END SELECT

         CALL NOGXX(XYZF,1,RF,GKK)
         CALL LCHAL(JABST,XYZF,ALAB,CIE,1)
C
C
C        L,C,HGD,A,B WEISS
C
C
C
         FCA(I,2)=ALAB(1)
         FCA(I,3)=CIE(2) 
         FCA(I,4)=CIE(3)
c        IF(I.GT.1) THEN
c        IF(FCA(I,4).LT.180..AND.FCA(I-1,4).GT.180.) FLAG(1)=.TRUE.
c        IF(FCA(I,4).GT.180..AND.FCA(I-1,4).LT.180.) FLAG(1)=.TRUE.
c        ENDIF
         FCA(I,5)=ALAB(2)
         FCA(I,6)=ALAB(3)
C
C        Über Schwarz
C
C
         SELECT CASE(IPRG)
           CASE(97,104)
             CALL RWWRTDE(2,CDI,NWS(),RF,RUR(1,2),A,S,GV,IER)
           CASE(99,105)
             CALL RWWRTTR(IRETR(2),CDI,NWS(),RF,RUR(1,2),A,S,GV,IER)
         END SELECT
         CALL NOGXX(XYZF,1,RF,GKK)
         CALL LCHAL(JABST,XYZF,ALAB,CIE,1)
C
C
C
C
C        L,C,HGD,A,B SCHWARZ
C
C
C
         FCA(I,7)=ALAB(1)
         FCA(I,8)=CIE(2) 
         FCA(I,9)=CIE(3) 
c        IF(I.GT.1) THEN
c        IF(FCA(I,9).LT.180..AND.FCA(I-1,9).GT.180.) FLAG(2)=.TRUE.
c        IF(FCA(I,9).GT.180..AND.FCA(I-1,9).LT.180.) FLAG(2)=.TRUE.
c        ENDIF
         FCA(I,10)=ALAB(2)
         FCA(I,11)=ALAB(3)
C
C        ALB(I)=ALAB(1)
C        AAB(I)=ALAB(2)
C        ABB(I)=ALAB(3)
c        CLAE(I)=CB
         FCA(I,14)=0.0
C
         HI=SQRT(FCA(I,5)**2+FCA(I,6)**2)
C
C
      ENDDO
C
C
C     KORREKTUR FUER WINKEL
C
      CALL HUEKOR(1,1,IM,FCA(1,4))
      CALL HUEKOR(1,1,IM,FCA(1,9))
C
c     IF(FLAG(1)) THEN
c       DO I=1,IM
c          IF(FCA(I,4).GT.180.) FCA(I,4)=FCA(I,4)-360.
c       ENDDO
c     ENDIF
c     IF(FLAG(2)) THEN
c       DO I=1,IM
c          IF(FCA(I,9).GT.180.) FCA(I,9)=FCA(I,9)-360.
c       ENDDO
c     ENDIF
      IF(IMIN.GT.0) THEN
        FCA(IMIN,14)=-1.0
      ENDIF
      IF(IMAX.GT.0) THEN
         FCA(IMAX,14)=1.0
      ENDIF
c      CLAE(IMIN)=CST
c      CLAE(IMAX)=CMA
C
C     BERECHNUNG DER FARBLAENGE FUER WEISS UND SCHWARZ
C
      QLW=0.
      QLS=0.
      DO I=2,IM
        HHI=(FCA(I,2)-FCA(I-1,2))**2+(FCA(I,5)-FCA(I-1,5))**2+
     &      (FCA(I,6)-FCA(I-1,6))**2
        QLW=QLW+SQRT(ABS(HHI))
        FCA(I,12)=QLW
        HHI=(FCA(I,7)-FCA(I-1,7))**2+(FCA(I,10)-FCA(I-1,10))**2+
     &      (FCA(I,11)-FCA(I-1,11))**2
        QLS=QLS+SQRT(ABS(HHI))
        FCA(I,13)=QLS
      ENDDO
      I=IM
      HHI=(FCA(I,2)-FCA(I,7))**2+(FCA(I,5)-FCA(I,10))**2+
     &    (FCA(I,6)-FCA(I,11))**2
      HHI=SQRT(ABS(HHI))
      FFF=QLW/(QLW+QLS)
      QLS=QLS+HHI*(1.-FFF)
      QLW=QLW+HHI*FFF
      FCA(1,12)=0.    
      FCA(1,13)=0.  
      DO I=1,IM
         FCA(I,12)=QLW-FCA(I,12)
         FCA(I,13)=QLS-FCA(I,13)
      ENDDO
      RETURN
      END

c

      SUBROUTINE FAERBCH(IRT,KW)
      USE MODQUAL
      USE MODFUNC,ONLY:JABST

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ALAB(3),CIE(3)
C
C
C     BERECHNUNG DER FARBCHARACTERISTIK UND DER REFLEXIONSWERTE FUER 
C     MAXIMALES CHROMA SOWIE DIE ZUGEHOERIGE SCHICHTDICKE
C
C
      GKK=GLZNOG(KW,IRT)
      DO I=1,IM
C
C        SCHICHTDICKE
C
         CDI=FCA(I,1)
         DO J=2,IV
           FCA(I,J)=0.0
         END DO
         SELECT CASE(IPRG)
             CASE (100)
C
C              Textil
C
               CALL RWRTTEX(CDI,NWS(),GU,RF,GV,G,IER)
             CASE(99)
C
C              Transparent
C
               CALL RWRTTRA(CDI,NWS(),RU,RF,GV,G,IER)
             CASE(97)
C
C              Deckend
C
               CALL RWRTDEK(CDI,NWS(),RU,RF,GV,G,IER)
         END SELECT
         CALL NOGXX(XYZF,1,RF,GKK)

         CALL LCHAL(JABST,XYZF,ALAB,CIE,1)
C
C
C        L,C,HGD(h),A,B
C
C
C
         FCA(I,2)=ALAB(1)
         FCA(I,3)=CIE(2) 
         FCA(I,4)=CIE(3)
         FCA(I,5)=ALAB(2)
         FCA(I,6)=ALAB(3)
C
C
C
C
C
      ENDDO
C
C
      RETURN
      END
C
C
C
C
C
      SUBROUTINE LITRDK(ITP,DIKA,DIKN,DEWESC,
     &                  RWERP,PARAM,WERT)
      USE MOTWERT
      USE MOSRWRT
      USE MODQUAL

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE DIKT,DEDW0T,DEDS0T,DEDST,DEDWT

C
C
      TYPE(TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*)::WERT
      REAL(KIND=8),DIMENSION(*) :: DEWESC
      TYPE(SRWERT) RWERP(*)
C
C
C
C
C
C
C
      IF(ITP.EQ.1) THEN
            DREL=1.
            DIKT=DIKN   
            DRABS=1.
            DRSTR=1.
            DRABS0=1.
            DRSTR0=1.
            DEDW0T=DEDW0
            DEDS0T=DEDS0
            DEDWT=DEDW
            DEDST=DEDS
      ELSE
            DREL=DIKN/(DIKT+TINY(1.))                 
            DRABS0=DEDW0T/(DEDW0+TINY(1.))
            DRSTR0=DEDS0T/(DEDS0+TINY(1.))
            DRABS=DEDWT/(DEDW+TINY(1.))
            DRSTR=DEDST/(DEDS+TINY(1.))
      ENDIF
      DECK=1./(DREL+TINY(1.))
C
C     WERTE FUER DICKE=0.
C
      STRE0=1./(DRSTR0+TINY(1.))  
      STAE0=1./(DRABS0+TINY(1.))
C
C     DIW0=DABS0
C     DIS0=DSTR0
C
      DIW0=1./(DEDW0+TINY(1.))
      DIS0=1./(DEDS0+TINY(1.))
      ALALB0=DEDS0/(DEDS0+DEDW0+TINY(1.))
C
C     WERTE FUER DICKE=1.
C
      STRE=1./(DRSTR+TINY(1.))  
      STAE=1./(DRABS+TINY(1.))
C
C     DIW=DABS
C     DIS=DSTR
C
      DIW=1./(DEDW+TINY(1.))
      DIS=1./(DEDS+TINY(1.))
      ALALB =DEDS/(DEDS+DEDW+TINY(1.))
      QLW=0.0
      QLS=0.0
C
C
C
C
C
C
C
C
C
      WERT(1)=DIKN
      WERT(2)=DIKA
      WERT(3)=DREL
      WERT(4)=DECK
      WERT(5)=DEDW0
      WERT(6)=DEDS0
      WERT(7)=DIW0
      WERT(8)=DIS0
      WERT(9)=DRABS0
      WERT(10)=DRSTR0
      WERT(11)=STAE0
      WERT(12)=STRE0
      WERT(13)=ALALB0
      WERT(14)=DEDW
      WERT(15)=DEDS
      WERT(16)=DIW
      WERT(17)=DIS
      WERT(18)=DRABS
      WERT(19)=DRSTR
      WERT(20)=STAE
      WERT(21)=STRE
      WERT(22)=ALALB
      IF(DCROM.NE.HUGE(1.D0)) THEN
         WERT(23)=DCROM
      ENDIF
      WERT(24)=QLW
      WERT(25)=QLS
      WERT(26)=DIS0*DEDW0
      WERT(27)=DIS*DEDW
      WERT(28)=DEWESC(1)
      WERT(29)=DEWESC(2)

C
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
C
C
C

C******************************************************************************
C******************************************************************************
C******************************************************************************
C
C                           +-------+ 
C                           ! FDETR !
C                           +-------+ 
      SUBROUTINE FDETR(NWE,DIK,RWERP,ITP,IER)
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      INTERFACE
      REAL(KIND=8) FUNCTION TRFAS(A,S,RD,KW,RT,TRFAA,TRFSS)
      INTEGER(KIND=4) ::KW,RT
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::TRFAA,TRFSS
      END FUNCTION
      END INTERFACE

C 
      SAVE DMAXT         
C 
C
C
      TYPE(SRWERT) RWERP(2)

      DIMENSION RHI(NWE,2),R(NWE,2)
      DIMENSION D(2),DH(2)
      EXTERNAL FDTR
      DATA RANGE/1.D30/,RANGIN/1.D-30/,RAN/1.D14/
      DATA FDI/0.001/
C 
C 
C
C     RWERP(1).CAMP(1)        MENGE BUNTPASTE                      
C     RWERP(1).CAMP(2)        FLAECHE                                 
C     RWERP(1).CAMP(3)        PROZENTIGKEIT DER BUNTPASTE
C 
C
      KENN=1
      GOTO 10
      ENTRY FDETR00(NWE,DIK,RWERP,ITP,IER)
      KENN=0
  10  IER=0
      KW=KWS()
      IRT=RWERP(1)%RETR
      GKK=GLZNOG(KW,IRT)
      FD0=DIK*FDI
C
C 
      DO I=1,NWE
        R(I,1)=RWERP(1)%R(I)
        R(I,2)=RWERP(2)%R(I)
      END DO
C 
C 
C
C     Rückrechnung
C
      D(1)=RWERP(1)%CAMP(0)
      D(2)=RWERP(2)%CAMP(0)
C
      CALL RWWRTTR2(IRETR,D,NWE,RQ,RUR,A,S,IER)

C
C     RP zur Berechnung der Empfindlichkeit (bei D)
C
      DH(1)=D(1)+FD0
      DH(2)=D(2)+FD0
C
C
      CALL RWWRTTR2(IRETR,DH,NWE,RP,RUR,A,S,IER)

C
C     RHI zur Berechnung der Empfindlichkeit (bei D=0.0)
C
      DH(1)=FD0
      DH(2)=FD0
      CALL RWWRTTR2(IRETR,DH,NWE,RHI,RUR,A,S,IER)


C
C
C     BERECHNUNG DER EMPFINDLICHKEIT DE/D(DICKE) FUER DICKE=MESSUNG
C
C
C
      CALL EMDEDD(JABST,NWE,RP,RQ,FD0,DEDW,DEDS,GKK)

C
C     
C     
C
C
C     BERECHNUNG DER EMPFINDLICHKEIT DE/D(DICKE) FUER DICKE=0.
C
C
C
      CALL EMDEDD(JABST,NWE,RHI,RUM,FD0,DEDW0,DEDS0,GKK)
C
C
      IF(KENN.EQ.0) THEN
        RETURN
      ENDIF
C
C 
C     BERECHNUNG DER SCHICHTDICKE FUER DECKVERMOEGEN
C
C 
   48 DMIN=0.01*DIK
      DMAX=10.*DIK+1.E-7
   40 IF (FDTR(DMIN)*FDTR(DMAX).GE.0.) THEN 
              DMAX=DMAX*10. 
              IF(DMAX.GT.RAN) GOTO 900
              GOTO 40 
      ENDIF 
C      IF(ITP.EQ.1) DMAXT=DMAX
c      CALL FLAQLN(IRT,DIK,DMAXT,KW,FDTR)
      CALL INTVAL(DIK,DMIN,DMAX,FDTR,IER)
      IF(IER.NE.0) GOTO 900
C
C
C
C
      DO I=1,NWE
C
C       REFLEXIONSWERTE FUER NICHT ABSORBIERENDES FARBMITTEL (A(BUNT)=0.) (TA,PA)
C
        SSH=DIK*S(I)
        AAH=0.
        DO  J=1,2
          ROK(I,J)=TRFAS(AAH,SSH,RUR(I,J),KW,IRETR(J))
        ENDDO    
C
C       REFLEXIONSWERTE FUER NICHT STREUENDES FARBMITTEL (S(BUNT)=0.) (TD,PD)
C
        SSH=0.        
        AAH=DIK*A(I) 
        DO  J=1,2
          ROS(I,J)=TRFAS(AAH,SSH,RUR(I,J),KW,IRETR(J))
        ENDDO    
      ENDDO
C
C     REFLEXION DER UNENDLICH-DICKEN SCHICHT
C
      TAL=1.D10
      DO   I=1,NWE
        AAH=TAL*A(I)
        SSH=TAL*S(I)
        DO J=1,2
           RUN(I,J)=TRFAS(AAH,SSH,RUR(I,J),KW,IRETR(J))
        END DO
C
C       GEWICHTETES MITTEL
C
C       RFU(I)=(RUN(I,1)**2+RUN(I,2)**2)/(RUN(I,1)+RUN(I,2)+RANGIN)
      ENDDO

C
C     VERHAELTNIS ABSORPTION/STREUUNG AUS Y(GROSS)
C
      RETURN
 900  CALL DELABAL(JABST,XYZP(1,1),XYZP(1,2),DE,DL,DC,DHH,DA,DB,1)
      RETURN
      END
C******************************************************************************
C******************************************************************************
C******************************************************************************

C                            +-------+
C                            ! FDTR  !
C                            +-------+
      FUNCTION FDTR(DIK)
      USE MODFUNC
      USE MODQUAL


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)


      DATA EPS/1.D-30/
C 
C 
      KW=KWS()
      NWE=NWS()
C 
C 
C     BERECHNUNG DER FARBABSTANDSDIFFERENZ           
C 
C 
      CALL RWWRTTR(IRETR(1),DIK,NWE,RP(1,1),RUR(1,1),A,S,GV,IER)
      CALL RWWRTTR(IRETR(2),DIK,NWE,RP(1,2),RUR(1,2),A,S,GV,IER)

C 
C 
C 
      CALL NOGXX(XYZP(1,1),1,RP(1,1),GLANZ(KW))
      CALL NOGXX(XYZP(1,2),1,RP(1,2),GLANZ(KW))
      CALL DELABAL(JABST,XYZP(1,1),XYZP(1,2),DE,DL,DC,DH,DA,DB,1)
      FDTR=LOG((DE+EPS)/(FDE+EPS))
      RETURN
C
C
C
      END 
C
C
C
C
C
C
C
C
      SUBROUTINE RWWRTTR2(IRETR,D,NWE,RQ,RUR,A,S,IER)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C

      DIMENSION RQ(NWE,2),RUR(NWE,2),GV(NWE),IRETR(2)
      DIMENSION D(2),A(*),S(*)
C
C 
      IER=0
C
C     Rückrechnung
C
      DO J=1,2
        CALL RWWRTTR(IRETR(J),D(J),NWE,RQ(1,J),RUR(1,J),A,S,GV,IER)
      ENDDO
C
C
      RETURN
      END
c
      SUBROUTINE RWWRTTR(IRETR,D,NWE,RQ,RUR,A,S,GV,IER)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      INTERFACE
      REAL(KIND=8) FUNCTION TRFAS(A,S,RD,KW,RT,TRFAA,TRFSS)
      INTEGER(KIND=4) ::KW,RT
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::TRFAA,TRFSS
      END FUNCTION
      END INTERFACE

      DIMENSION RQ(*),RUR(*)
      DIMENSION A(*),S(*),GV(*)
C
C 
      IER=0
      KW=KWS()


C 

C
C     Rückrechnung
C
      DO I=1,NWE
C
C       KORRIGIERTE R_WERTE BERECHNEN
C
        UH= RUR(I)

        AAH=D*A(I)
        SSH=D*S(I)
        GV(I)=AAH+SSH
        RQ(I)=TRFAS(AAH,SSH,UH,KW,IRETR)
      ENDDO      

      RETURN
      END
c
c
c
      SUBROUTINE RWWRTDE2(CWS,NWE,RQ,RUK,A,S,IER)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION RQ(NWE,2),CWS(2),A(*),S(*)
C     Werden nicht verwendet (nur zur Kompatibilität)
      DIMENSION RUK(NWE,*),GV(NWE)
C
C
C
      CALL RWWRTDE(1,CWS(1),NWE,RQ(1,1),RUK(1,1),A,S,GV,IER)
      CALL RWWRTDE(2,CWS(2),NWE,RQ(1,2),RUK(1,2),A,S,GV,IER)

      RETURN
      END
C
C
C
C
      SUBROUTINE RWWRTDE(J,C,NWE,R,RUU,AN,SN,GVV,IER)
      USE MODQUAL
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION R(*),AN(*),SN(*),GVV(*)
C     Werden nicht verwendet (nur zur Kompatibilität)
      DIMENSION RUU(*)
C
C
C
      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
C
C
      DATA RD/1.0/
C
C 
C
      IER=0
      KW=KWS()
C
C     Rückrechnung
C
      DO I=1,NWE
        AK=C*AN(I)+CW(J)*AW(I)+CS(J)*AS(I)
        SK=C*SN(I)+CW(J)*SW(I)+CS(J)*SS(I)
C
C       R-WERTE RUECKRECHNUNG
C
C
         GVV(I)=AK/(SK+TINY(1.0))
         R(I)=REFDE(AK,SK,RD,KW)
      ENDDO

      RETURN
      END


C******************************************************************************
C******************************************************************************
C******************************************************************************


      SUBROUTINE LIFDE2(ITP,CONZA,CONZ,DEWESC,
     &                 RWERP,PARAM,WERT)
      USE MOTWERT
      USE MOSRWRT
      USE MODQUAL
      USE MODILLU
      USE MODFUNC


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION CONZT,CCONZ(6),FAE60,CONZTA
      SAVE CONZT,CCONZ,FAE60,CONZTA
      TYPE (SRWERT) RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*)::WERT
      REAL(KIND=8),DIMENSION(*) :: DEWESC
C
C
      DATA FAE60/0.D0/
      DATA CONZT/0.D0/
      DATA CCONZ/0.D0,0.D0,0.D0,0.D0,0.D0,0.D0/
      DATA CONZTA/0.D0/
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
C     AUFHELLVERHAELTNIS (AV) RELATIVE FARBSTAERKE (FREL) FAERBEAEQUIV. (FAE)
C
C
C
      AV=1./(CONZ+TINY(1.))
      AVA=1./(CONZA+TINY(1.))
C
C

      IF(RWERP%CART(1:1).EQ.'@') THEN
         ITZ=1
      ELSE IF(RWERP%CART(1:1).EQ.'A') THEN
         ITZ=2
      ELSE IF(RWERP%CART(1:1).EQ.'B') THEN
         ITZ=3
      ELSE IF(RWERP%CART(1:1).EQ.'C') THEN
         ITZ=4
      ELSE IF(RWERP%CART(1:1).EQ.'D') THEN
         ITZ=5
      ELSE IF(RWERP%CART(1:1).EQ.'E') THEN
         ITZ=6
      ENDIF
      IF(RWERP%CART(2:2).EQ.'T') THEN
            CCONZ(ITZ)=CONZ
            FEE=1.
      ELSE 
            FEE=CONZ/(CCONZ(ITZ)+TINY(1.))
      ENDIF
C
      IF(ITP.EQ.1) THEN
            FREL=1.
            FAE=1.
            CONZT=CONZ
            CONZTA=CONZA
      ELSE
            FREL=CONZT/CONZ                  
            FAE=CONZ/CONZT
      ENDIF
C
C     FARBMITTELMENGE FUER NEUE EINWAAGE (GLEICHE FARBSTAERKE WIE TYP(MESSUNG))
C
C   
C             KONZ(TYP-ALT)*FAE*MENGE(WEISS)*PROZ(WEISS)
C     AMNEU=--------------------------------------------------
C                      PROZ(BUNT)
C
      AMNEU=CONZTA*FAE*RWERP%CAMP(1)*RWERP%CAMP(2)/
     &                                 (RWERP%CAMP(4)+TINY(1.))
C
C
C     FARBMITTELMENGE FUER STANDARDFARBTIEFE
C
C     AMNST=MENGE(FARBM.ALT)*CONZ(NEU)/CONZ(ALT)
C     AMNST=MENGE(WEISS ALT)/AV
C
C
C
      AMNST=RWERP%CAMP(3)*CONZ/(CONZA+TINY(1.))

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
C
      WERT(1)=CONZA
      WERT(2)=CONZ
      WERT(3)=AV
      WERT(21)=AVA
      WERT(4)=FREL
      WERT(5)=FAE
C
C
C
C
C
      WERT(6)=RWERP%CAMP(3)
      WERT(7)=AMNST
      WERT(8)=RWERP%CAMP(4)
      WERT(9)=RWERP%CAMP(1)
      WERT(10)=RWERP%CAMP(2)
      IF(RWERP%CART(1:1).EQ.'@') THEN
         FAE60=FAE
      ELSE 
         RUBO=FAE60/(FAE+TINY(1.))-1.   
         WERT(12)=RUBO
         DHA=FAE/(FAE60+TINY(1.))-1.
         WERT(11)=DHA 
      ENDIF
C
C     bezogen auf Typ mit gleicher Präparation(gleiches ITZ) (@T mit @P bzw.AT mit AP usw)
C
C
C
C
      WERT(13)=FEE
      WERT(14)=1./(FEE+TINY(1.))
      WERT(15)=AMNEU
C
C
C     Färbeäqu.(16) und Farbstärke(17)
C     bezogen auf @-Messung, d.h. Messung der ersten Probe oder des ersten Typs in der Gruppe
C
C
      WERT(16)=FAE/(FAE60+TINY(1.))
      WERT(17)=FAE60/(FAE+TINY(1.))
C

C      

      IF(IART.EQ.6) THEN
      	IF(ITP.EQ.1) THEN
      		WERT(18)=WMIN(1)
      	ENDIF
      	WERT(20)=RWZWIS(WMIN(1),NWE,WSOL,RWERP%R)
     	WERT(19)=AKUMU(WERT(20),KWS())
      ENDIF
      WERT(22)=DEWESC(1)
      WERT(23)=DEWESC(2)
C
C
C
      RETURN
      END
C
C
C
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************

C                           +-------+
C                           ! FSTDE2 !
C                           +-------+
      SUBROUTINE FSTDE2(C,R,ITP,IER)
      USE MODFUNC
      USE MODQUAL
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C 
      DIMENSION R(*)   
      EXTERNAL FBWD2
      DATA RANGE/1.D80/
C
      IER=0
C
C     BESTIMMUNG VON RMIN FALLS ANGLEICH IM K/S-MAXIMUM      
C
C
      IF(ITP.EQ.1.AND.IART.EQ.6) THEN
         IF(WELFST.GE.WSOL(1).AND.WELFST.LE.WSOL(NWE)) THEN
            WMIN(1)=WELFST
            KMIN(1)=NWE
            DO I=1,NWE-1
              IF(WMIN(1).GE.WSOL(I).AND.WMIN(1).LT.WSOL(I+1)) THEN
                KMIN(1)=I
              ENDIF
            END DO
            RMIN(1)=RWZWIS(WMIN(1),NWE,WSOL,R)
         ELSE
            CALL RWMINI(1,KMIN,WMIN,RMIN,NWE,WSOL,R)
            RMIN(1)=RWZWIS(WMIN(1),NWE,WSOL,R)
         ENDIF
      ENDIF
C 
C
C 
      ITYP=ITP
      IF(ITP.EQ.1) THEN 
C 
C 
C 
C     TYPVERARBEITUNG 
C 
          IF(IART.EQ.3) THEN 
C 
C 
C            KONZENTRATION WIRD FUER BTYP=0. BESTIMMT 
  
             BTYP=0.D0
             CMIN=0.01*C
             CMAX=10.*C + 1.D-7 
             FFB=FBWD2(CMIN)
   20        IF (FFB*FBWD2(CMAX).GE.0.D0) THEN
                 CMAX=CMAX*10.
                 IF(CMAX.GE.RANGE) GOTO 901
                 GOTO 20
             ENDIF
             CALL INTVAL(C,CMIN,CMAX,FBWD2,IER)
             IF(IER.NE.0) GOTO 900
C 
          ELSE
C 
C 
C            BTYP WIRD AUS EINGEGEBENER KONZENTRATION BESTIMMT
C
             BTYP=0.
             BTYP=FBWD2(C)
             IF(IART.EQ.5) THEN
C
                DO I=1,3
                   XYZQ(I,1)=XYZH(I)
                ENDDO
C 
             ENDIF
          ENDIF 
      ELSE
C 
C 
C     PROBENVERARBEITUNG BPROBE=BTYP
C 
C 
        IF(IART.EQ.5) THEN
          CMIN=0.
          CC=C
          CMAX=1.D6
          CMIN=0.0
          DE=BRENT(CMIN,CC,CMAX,FBWD2,1.D-5,C)
        ELSE 
          CMIN=0.01*C
          CMAX=10.*C + 1.D-7
   40     IF (FBWD2(CMIN)*FBWD2(CMAX).GE.0.D0) THEN
              CMAX=CMAX*10. 
              IF(CMAX.GE.RANGE) GOTO 901
              GOTO 40 
          ENDIF 
          CALL INTVAL(C,CMIN,CMAX,FBWD2,IER) 
          IF(IER.NE.0) GOTO 900
        ENDIF
      ENDIF 
      RETURN
  901 IER=4020
      RETURN
  900 RETURN
      END 
C                            +-------+
C                            ! FBWD2  !
C                            +-------+
      DOUBLE PRECISION FUNCTION FBWD2(C)
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8) ALAB(3),ALCH(3),ALABCC(3)

C
C
C 
C 
C     BERECHNUNG DER B-WERT DIFFERENZ BPROBE-BTYP 
C     AUS DER KONZENTRATION C FUER DECKENDE SCHICHTEN
C 
C 
      KW=KWS()

      CALL RWWRTDE(1,C,NWS(),RH,RUR(1,1),A,S,GV,IER)
C
C 
C 
C
      CALL NOGXX(XYZH,1,RH,GLANZ(KW))
      GOTO(10,11,12,13,13,14,15,16,17),IART+1
C
C     ANGLEICH Y
C  
  10     FBWD2=XYZH(2)-BTYP
         RETURN                
C
C     ANGLEICH AUF MINIMALES X,Y,Z
C
C
C
C
C 
 11   IF(ITYP.EQ.1) THEN
                 XX=1.D80
                 IXYZT=0
                 DO 21 I=1,3
                 XF=XYZH(I)/FAKT(I,1)
                 IF(XF.GT.XX) GOTO 21
                 XX=XF
                 IXYZT=I
  21             CONTINUE
      ENDIF
      FBWD2=XYZH(IXYZT)-BTYP
      RETURN
C
C
C
C     ANGLEICH AUF GEWICHTETE SUMME K/S
C
C
  12  CALL GEWICH(NWE,GV,SUMM,1)
      FBWD2=SUMM-BTYP
      RETURN
C
C
C     B-WERT ANGLEICH (FIAF)
C
C
C
C
  13  CALL BWERT(XYZH,KBWT,B,1)
      FBWD2=B-BTYP
      RETURN
C
C
C
C
C     GEWICHTETE MINIMIERUNG DES FARBABSTANDES
C
C
  14  IF(ITYP.EQ.1) THEN
        FBWD2=0.0
      ELSE
        CALL DELABAL(JABST,XYZT,XYZH,DE,DL,DC,DH,DA,DB,1)
        DEN=SQRT((WLCH(1)*DL)**2+(WLCH(2)*DC)**2+(WLCH(3)*DH)**2)
        FBWD2=DEN
      ENDIF
      RETURN
  15  FBWD2=RWZWIS(WMIN(1),NWE,WSOL,RH)-RMIN(1)
      RETURN
  16  FBWD2=SUM(GV)-BTYP
      RETURN
C
C     Probe in senkrecht zur Färbequaracteristik(des Bezugs) liegenden Ebene (L,a,b)
C
C
C     L,a,b berechnen
C
C
  17  CALL LCHAL(JABST,XYZF,ALAB,ALCH,1)
      IF(ITYP.EQ.1) THEN
        DO I=1,3
          ALABT(I)=ALAB(I)
        END DO
C
C       Für Ebene senkrecht
C
        CC=1.01*C
        CALL RWWRTDE(1,CC,NWS(),RH,RUR(1,1),A,S,GV,IER)

        CALL NOGXX(XYZH,1,RH,GLANZ(KW))
        CALL LCHAL(JABST,XYZH,ALABCC,ALCH,1)
C
C       Normalenvektor
C
        DO I=1,3
           DALABT(I)=ALABCC(I)-ALABT(I)
        END DO
      ENDIF
      SUMM=0.0
      DO I=1,3
         SUMM=SUMM+DALABT(I)*(ALAB(I)-ALABT(I))
      END DO
      FBWD2=SUMM
      RETURN


      END
C******************************************************************************

C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************


      SUBROUTINE LIFTR(ITP,CONZA,CONZ,
     &                 RWERP,PARAM,WERT)
      USE MOTWERT
      USE MOSRWRT
      USE MODQUAL
      USE MODFUNC
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE CONZT,CONZTA
C
C
      TYPE (SRWERT) RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

      DATA CONZT/0.D0/
      DATA CONZTA/0.D0/
C
C
C
C
C
      IF(RWERP%IAMI.LT.0) THEN
         RETURN
      ENDIF
      IRT=RWERP%RETR

C
C
C
C
C     AUFHELLVERHAELTNIS (AV) RELATIVE FARBSTAERKE (FREL) FAERBEAEQUIV. (FAE)
C
      KW=KWS()
C
C
      AV=1./(CONZ+TINY(1.))
      AVA=1./(CONZA+TINY(1.))
C
C                       FLAECHE
C     REL. FLAECHE =---------------------
C                       PROZ(BUNT)
C
      FLPB=RWERP%CAMP(2)/(0.01*RWERP%CAMP(3)+TINY(1.))
C
C
      IF(ITP.EQ.1) THEN
            FREL=1.
            FAE=1.
            CONZT=CONZ
            CONZTA=CONZA
      ELSE
            FREL=CONZT/CONZ                  
            FAE=CONZ/CONZT
      ENDIF
C
C
C
C
C
C
C     FARBMITTELMENGE FUER NEUE EINWAAGE (GLEICHE FARBSTAERKE WIE TYP(MESSUNG))
C
C
C
C                   KOZ(TYP-ALT)*FAE*FLAECHE      
C           AMNEU=-----------------------------------
C                          PROZENTIGKEIT 

      AMNEU=CONZTA*FAE*RWERP%CAMP(2)/(0.01*RWERP%CAMP(3)+TINY(1.))
C
C
C     FARBMITTELMENGE FUER STANDARDFARBTIEFE
C
C              MENGE(EINWAAGE)*KONZ(NEU)
C     AMNST=---------------------------------
C                     KONZ(ALT)
C
      AMNST=RWERP%CAMP(1)*CONZ/(CONZA+TINY(1.))
C
C
C     FARBKOORDINATEN DER MESSUNG
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
C
C
C
C
      WERT(1)=CONZA
      WERT(2)=CONZ
      WERT(3)=AV
      WERT(21)=AVA
      WERT(4)=FREL
      WERT(5)=FAE
      WERT(6)=RWERP%CAMP(1)
      WERT(7)=AMNST
      WERT(8)=RWERP%CAMP(3)
      WERT(17)=RWERP%CAMP(2)
      WERT(18)=FLPB
C
      WERT(15)=AMNEU
C
C
C      CALL RWMINI(1,KMI,WMI,RMI,RWERP%R(1))

C      

      IF(IART.EQ.6) THEN
      	IF(ITP.EQ.1) THEN
      	  WERT(16)=WMIN(1)
        ENDIF
        WERT(20)=RWZWIS(WMIN(1),NWE,WSOL,RWERP%R)
        WERT(19)=AABEE( WERT(20),KW,1.D0,IRT)
      ENDIF
C
C
C
C
      RETURN
C
C
      END
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************


      SUBROUTINE LIFTX(ITP,CONZA,CONZ,
     &                 RWERP,PARAM,WERT)
      USE MOTWERT
      USE MOSRWRT
      USE MODQUAL
      USE MODFUNC
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION CONZT,CONZTA,AMNT
      SAVE CONZT,CONZTA,AMNT
C
      TYPE (SRWERT) RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*):: WERT

C
C
      DATA CONZT/0.D0/
      DATA CONZTA/0.D0/
      DATA AMNT/0.D0/
C
C
C
C
      KW=KWS()
      IF(RWERP%IAMI.LT.0) THEN
         RETURN
      ENDIF

C
C
C
C
C
C     AUFHELLVERHAELTNIS (AV) RELATIVE FARBSTAERKE (FREL) FAERBEAEQUIV. (FAE)
C
C
C
      AV=1./(CONZ+TINY(1.))
      AVA=1./(CONZA+TINY(1.))
C
C     NEUE EINWAAGE
C
C
C
C
C
      IF(ITP.EQ.1) THEN
            FREL=1.
            FAE=1.
            CONZT=CONZ
            CONZTA=CONZA
            AMNT=RWERP%CAMP(1)
      ELSE
            FREL=CONZT/CONZ                  
            FAE=CONZ/CONZT
      ENDIF
C
C
C
C     FARBMITTELMENGE FUER NEUE EINWAAGE (GLEICHE FARBSTAERKE WIE TYP(MESSUNG))
C
C
C
C
C
C     AMNEU=CONZTA*FAE*AMNT
C
      AMNEU=CONZTA*FAE*AMNT                              
C
C
C     FARBMITTELMENGE FUER STANDARDFARBTIEFE
C
      AMNST=RWERP%CAMP(1)*FAE
C
C
C
C     FARBKOORDINATEN DER MESSUNG
C
C
C
C     CALL NOGXX(XYZA,1,RWERP%R(1),GLANZ(KW))
C     CALL B WERT(XYZA,KBWT,BA,1)
C
C
C
C
C
C
C     FARBKOORDINATEN DER RECHNUNG
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
      WERT(1)=CONZA
      WERT(2)=CONZ
      WERT(3)=AV
      WERT(21)=AVA
      WERT(4)=FREL
      WERT(5)=FAE
      WERT(6)=RWERP%CAMP(1)
      WERT(7)=AMNST
      WERT(15)=AMNEU
C
      IF(IART.EQ.6) THEN
        IF(ITP.EQ.1) THEN
           WERT(16)=WMIN(1)
        ENDIF
        WERT(20)=RWZWIS(WMIN(1),NWE,WSOL,RWERP%R)
        WERT(19)=AKUMU( WERT(20),KW)
      ENDIF
C
C
      RETURN
      END
C




C******************************************************************************
C******************************************************************************
C******************************************************************************


      SUBROUTINE LIFTR2(ITP,CONZA,CONZ,DEWESC,
     &                 RWERP,PARAM,WERT)
      USE MOTWERT
      USE MOSRWRT
      USE MODQUAL
      USE MODILLU
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE CONZT,CONZTA      
C
C
      TYPE (SRWERT) RWERP
      TYPE (TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*)::WERT
      REAL(KIND=8),DIMENSION(*) :: DEWESC
      DATA CONZT/0.D0/
      DATA CONZTA/0.D0/
C
C
C
C
C
C
      IRT=RWERP%RETR

C
C
C
C     AUFHELLVERHAELTNIS (AV) RELATIVE FARBSTAERKE (FREL) FAERBEAEQUIV. (FAE)
C
C
C
      AV=1./(CONZ+TINY(1.))
      AVA=1./(CONZA+TINY(1.))
C
C                       FLAECHE
C     REL. FLAECHE =---------------------
C                       PROZ(BUNT)
C
      FLPB=RWERP%CAMP(2)/(0.01*RWERP%CAMP(3)+TINY(1.))
C
C
      IF(ITP.EQ.1) THEN
            FREL=1.
            FAE=1.
            CONZT=CONZ
            CONZTA=CONZA
      ELSE
            FREL=CONZT/CONZ                  
            FAE=CONZ/CONZT
      ENDIF
C
C
C
C
C
C
C     FARBMITTELMENGE FUER NEUE EINWAAGE (GLEICHE FARBSTAERKE WIE TYP(MESSUNG))
C
C
C
C                   KOZ(TYP-ALT)*FAE*FLAECHE      
C           AMNEU=-----------------------------------
C                          PROZENTIGKEIT 

      AMNEU=CONZTA*FAE*RWERP%CAMP(2)/(0.01*RWERP%CAMP(3)+TINY(1.))
C
C
C     FARBMITTELMENGE FUER STANDARDFARBTIEFE
C
C              MENGE(EINWAAGE)*KONZ(NEU)
C     AMNST=---------------------------------
C                     KONZ(ALT)
C
      AMNST=RWERP%CAMP(1)*CONZ/(CONZA+TINY(1.))
C
C
C     FARBKOORDINATEN DER MESSUNG
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
C
C
C
      WERT(1)=CONZA
      WERT(2)=CONZ
      WERT(3)=AV
      WERT(21)=AVA
      WERT(4)=FREL
      WERT(5)=FAE
      WERT(6)=RWERP%CAMP(1)
      WERT(7)=AMNST
      WERT(8)=RWERP%CAMP(3)
      WERT(17)=RWERP%CAMP(2)
      WERT(18)=FLPB
C
      WERT(15)=AMNEU
C
C

C      

      IF(IART.EQ.6) THEN
      	IF(ITP.EQ.1) THEN
      	  WERT(16)=WMIN(1)
        ENDIF
        WERT(20)=RWZWIS(WMIN(1),NWE,WSOL,RWERP%R)
        WERT(19)=AABEE(WERT(20),KWC,1.D0,IRT)
      ENDIF
      WERT(22)=DEWESC(1)
      WERT(23)=DEWESC(2)

C
C
C
C
      RETURN
C
C
      END
C                           +-------+ 
C                           ! FSTTR2 !
C                           +-------+ 
      SUBROUTINE FSTTR2(C,R,ITP,IER)
      USE MODFUNC
      USE MODQUAL
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION R(*)   
      EXTERNAL FBWT2
      DATA RANGE/1.D80/
C 
C 
C
C 
C
C     BESTIMMUNG VON RMIN FALLS ANGLEICH IM EXTINKTIONSMAXIMUM
C
      IER=0
C
      IF(ITP.EQ.1.AND.IART.EQ.6) THEN
         IF(WELFST.GE.WSOL(1).AND.WELFST.LE.WSOL(NWE)) THEN
            WMIN(1)=WELFST
            KMIN(1)=NWE
            DO I=1,NWE-1
              IF(WMIN(1).GE.WSOL(I).AND.WMIN(1).LT.WSOL(I+1)) THEN
                KMIN(1)=I
              ENDIF
            END DO
            RMIN(1)=RWZWIS(WMIN(1),NWE,WSOL,R)
         ELSE
            CALL RWMINI(1,KMIN,WMIN,RMIN,NWE,WSOL,R)
            RMIN(1)=RWZWIS(WMIN(1),NWE,WSOL,R)
         ENDIF
      ENDIF
C
C 
C
C 
C 
      ITYP=ITP
      IF(ITP.EQ.1) THEN 
C 
C 
C 
C     TYPVERARBEITUNG 
C 
C 
          IF(IART.EQ.3) THEN 
C 
C 
C            KONZENTRATION WIRD FUER BTYP=0. BESTIMMT 
  
             BTYP=0.D0
             CMIN=0.01*C
             CMAX=10.*C + 1.D-7
   20        IF (FBWT2(CMIN)*FBWT2(CMAX).GE.0.D0) THEN
                 CMAX=CMAX*10.
                 IF(CMAX.GT.RANGE) GOTO 901
                 GOTO 20
             ENDIF
             CALL INTVAL(C,CMIN,CMAX,FBWT2,IER)
             IF(IER.NE.0) GOTO 900
C 
          ELSE
C 
C
C            BTYP WIRD AUS EINGEGEBENER KONZENTRATION BESTIMMT
             BTYP=0.
             BTYP=FBWT2(C)
c
C
C
             IF(IART.EQ.5) THEN
C
                DO I=1,3
                   XYZQ(I,1)=XYZH(I)
                ENDDO
C 
             ENDIF
          ENDIF 
      ELSE
C 
C 
C     PROBENVERARBEITUNG BPROBE=BTYP
C 
C 
        IF(IART.EQ.5) THEN
          CMIN=0.
          CC=C
          CMAX=1.E6
          CMIN=0.0
          DE=BRENT(CMIN,CC,CMAX,FBWT2,1.D-4,C)
        ELSE
          CMIN=0.01*C
          CMAX=10.*C + 1.D-7
   40     IF (FBWT2(CMIN)*FBWT2(CMAX).GE.0.) THEN
              CMAX=CMAX*10. 
              IF(CMAX.GT.RANGE) GOTO 901
              GOTO 40 
          ENDIF 
          CALL INTVAL(C,CMIN,CMAX,FBWT2,IER)
          IF(IER.NE.0) GOTO 900
        ENDIF
      ENDIF 
      RETURN
  901 IER=4020
      RETURN
  900 RETURN
      END 
C                            +-------+
C                            ! FBWT2  !
C                            +-------+
      DOUBLE PRECISION FUNCTION FBWT2(C)
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8) ALAB(3),ALCH(3),ALABCC(3)

C
C 
C 
C     BERECHNUNG DER B-WERT DIFFERENZ BPROBE-BTYP 
C     AUS DER KONZENTRATION C FUER TRANSPARENTE SCHICHTEN
C 
C 
C     write(1,'('' C '',e14.7)') C
      KW=KWS()


      CALL RWWRTTR(IRETR(1),C,NWE,RH,RUR(1,1),A,S,GV,IER)
C
C 
C 
      CALL NOGXX(XYZH,1,RH,GLANZ(KW))
      GOTO(10,11,12,13,13,14,15,16,17),IART+1
C
C     ANGLEICH Y
C  
  10     FBWT2=XYZH(2)-BTYP
         RETURN                
C
C
C     ANGLEICH AUF MINIMALES X,Y,Z
C
C
C
C
C
 11   IF(ITYP.EQ.1) THEN
                 XX=1.D80
                 IXYZT=0
                 DO 21 I=1,3
                 XF=XYZH(I)/FAKT(I,1)
                 IF(XF.GT.XX) GOTO 21
                 XX=XF
                 IXYZT=I
  21             CONTINUE
      ENDIF
      FBWT2=XYZH(IXYZT)-BTYP
      RETURN
C
C
C
C     ANGLEICH AUF GEWICHTETE SUMME UEBER EXTINKTION
C
C
C
C
  12  CALL GEWICH(NWE,GV,SUMM,1)
      FBWT2=SUMM-BTYP
      RETURN
C
C
C     B-WERT ANGLEICH (FIAF)
C
C
C
C
  13  CALL BWERT(XYZH,KBWT,B,1)
      FBWT2=B-BTYP
      RETURN
C
C
C
C
C     GEWICHTETE MINIMIERUNG DES FARBABSTANDES
C
C
  14  IF(ITYP.EQ.1) THEN
        FBWT2=0.0
      ELSE
        CALL DELABAL(JABST,XYZT,XYZH,DE,DL,DC,DH,DA,DB,1)
        DEN=SQRT((WLCH(1)*DL)**2+(WLCH(2)*DC)**2+(WLCH(3)*DH)**2)
        FBWT2=DEN
      ENDIF
      RETURN
  15  FBWT2=RWZWIS(WMIN(1),NWE,WSOL,RH)-RMIN(1)
      RETURN
  16  FBWT2=SUM(GV)-BTYP
      RETURN
C
C     Probe in senkrecht zur Färbequaracteristik(des Bezugs) liegenden Ebene (L,a,b)
C
C
C     L,a,b berechnen
C
C
  17  CALL LCHAL(JABST,XYZF,ALAB,ALCH,1)
      IF(ITYP.EQ.1) THEN
        DO I=1,3
          ALABT(I)=ALAB(I)
        END DO
C
C       Für Ebene senkrecht
C
        CC=1.01*C
        CALL RWWRTTR(IRETR(1),CC,NWE,RH,RUR(1,1),A,S,GV,IER)
        CALL NOGXX(XYZH,1,RH,GLANZ(KW))
        CALL LCHAL(JABST,XYZH,ALABCC,ALCH,1)
C
C       Normalenvektor
C
        DO I=1,3
           DALABT(I)=ALABCC(I)-ALABT(I)
        END DO
      ENDIF
      SUMM=0.0
      DO I=1,3
         SUMM=SUMM+DALABT(I)*(ALAB(I)-ALABT(I))
      END DO
      FBWT2=SUMM
      RETURN


      END 
C
C                           +-------+ 
C                           ! FSTTX !
C                           +-------+ 
      SUBROUTINE FSTTX(C,R,ITP,IER)
      USE MODQUAL
      USE MODFUNC
      USE MODQUAL
      USE MODILLU
      USE MODGKWR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c      INCLUDE "PRAMR.INC"
c      INCLUDE "COFIA.INC"
c      INCLUDE "COWIN.INC"
c      INCLUDE "COFRB.INC"
c      INCLUDE "COGKW.INC"
c      INCLUDE "COUNT.INC"
C
      DIMENSION R(*)
      EXTERNAL FBWX
      DATA RANGE/1.D80/
C
C 
C 
C
C     BESTIMMUNG VON RMIN FALLS ANGLEICH IM K/S-MAXIMUM      
C
      IER=0
C      KW=KWS()
C
      IF(ITP.EQ.1.AND.IART.EQ.6) THEN
         IF(WELFST.GE.WSOL(1).AND.WELFST.LE.WSOL(NWE)) THEN
            WMIN(1)=WELFST
            KMIN(1)=NWE
            DO I=1,NWE-1
              IF(WMIN(1).GE.WSOL(I).AND.WMIN(1).LT.WSOL(I+1)) THEN
                KMIN(1)=I
              ENDIF
            END DO
            RMIN(1)=RWZWIS(WMIN(1),NWE,WSOL,R)
         ELSE
            CALL RWMINI(1,KMIN,WMIN,RMIN,NWE,WSOL,R)
            RMIN(1)=RWZWIS(WMIN(1),NWE,WSOL,R)
         ENDIF
         IF(KMIN(1).LT.NWE) THEN
             WWD=(WMIN(1)-WSOL(KMIN(1)))/(WSOL(KMIN(1)+1)-WSOL(KMIN(1)))
         ELSE
             WWD=0.
         ENDIF
      ENDIF
C
C 
C 
C     BERECHNUNG DER G(LAMDA)-WERTE FUER UNTERGRUND UND FAERBUNG
C
C 
      CALL GWRTTEX(C,IER)
C
C 
C 
      ITYP=ITP
      IF(ITP.EQ.1) THEN
C
C 
C 
C     TYPVERARBEITUNG 
C 
C 
          IF(IART.EQ.3) THEN 
C 
C 
C            KONZENTRATION WIRD FUER BTYP=0. BESTIMMT 
  
             BTYP=0.D0
             CMIN=0.01*C
             CMAX=10.*C + 1.D-7 
   20        IF (FBWX(CMIN)*FBWX(CMAX).GE.0.D0) THEN
                 CMAX=CMAX*10.
                 IF(CMAX.GE.RANGE) GOTO 901
                 GOTO 20
             ENDIF
             CALL INTVAL(C,CMIN,CMAX,FBWX,IER)
             IF(IER.NE.0) GOTO 900
C 
          ELSE
C 
C 
C            BTYP WIRD AUS EINGEGEBENER KONZENTRATION BESTIMMT
             BTYP=0.
             BTYP=FBWX(C)
             IF(IART.EQ.5) THEN
C
                DO I=1,3
                   XYZT(I)=XYZF(I)
                ENDDO
C 
             ENDIF
          ENDIF 
      ELSE
C 
C 
C     PROBENVERARBEITUNG BPROBE=BTYP
C 
C 
        IF(IART.EQ.5) THEN
          CMIN=0.
          CC=C
          CMAX=1.D6
          CMIN=0.0
          DE=BRENT(CMIN,CC,CMAX,FBWX,1.D-4,C)
        ELSE 
          CMIN=0.01*C
          CMAX=10.*C + 1.D-7
   40     IF (FBWX(CMIN)*FBWX(CMAX).GE.0.D0) THEN 
              CMAX=CMAX*10. 
              IF(CMAX.GE.RANGE) GOTO 901
              GOTO 40 
          ENDIF 
          CALL INTVAL(C,CMIN,CMAX,FBWX,IER) 
          IF(IFEHL(IER).NE.0) GOTO 900
        ENDIF
      ENDIF 
      RETURN
  901 IER=4020
      RETURN
  900 IER=4019
      RETURN
      END 
C                            +-------+
C                            ! FBWX  !
C                            +-------+
      DOUBLE PRECISION FUNCTION FBWX(C)
      USE MODFUNC
      USE MODQUAL
      USE MODILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=8) ALAB(3),ALCH(3),ALABCC(3)

c      INCLUDE "PRAMR.INC"
c      INCLUDE "COFIA.INC"
c      INCLUDE "COWIN.INC"
c      INCLUDE "COFRB.INC"
c      INCLUDE "COGKW.INC"
c      INCLUDE "COUNT.INC"
C
C 
C 
C 
C     BERECHNUNG DER B-WERT DIFFERENZ BPROBE-BTYP 
C     AUS DER KONZENTRTION C FUER TEXTIL               
C 
C 
      KW=KWS()


      CALL RWRTTEX(C,NWE,GU,RF,GV,G,IER)

C
C 
C 
      CALL NOGXX(XYZF,1,RF,GLANZ(KW))
      GOTO(10,11,12,13,13,14,15,16,17),IART+1
C
C     ANGLEICH Y
C  
  10     FBWX=XYZF(2)-BTYP
         RETURN                

C
C
C     ANGLEICH AUF MINIMALES X,Y,Z
C
C
C
C
C 
 11   IF(ITYP.EQ.1) THEN
                 XX=1.D80
                 IXYZT=0
                 DO 21 I=1,3
                 XF=XYZF(I)/FAKT(I,1)
                 IF(XF.GT.XX) GOTO 21
                 XX=XF
                 IXYZT=I
  21             CONTINUE
      ENDIF
      FBWX=XYZF(IXYZT)-BTYP
      RETURN
C
C
C
C     ANGLEICH AUF GEWICHTETE SUMME K/S
C
C
C
C
C
C
  12  CALL GEWICH(NWE,GV,SUMM,1)
      FBWX=SUMM-BTYP
      RETURN
C
C
C     B-WERT ANGLEICH (FIAF)
C
C
C
C
  13  CALL BWERT(XYZF,KBWT,B,1)
      FBWX=B-BTYP
      RETURN
C
C
C
C
C     GEWICHTETE MINIMIERUNG DES FARBABSTANDES
C
C
  14  IF(ITYP.EQ.1) THEN
        FBWX=0.0
      ELSE
        CALL DELABAL(JABST,XYZT,XYZF,DE,DL,DC,DH,DA,DB,1)
        DEN=SQRT((WLCH(1)*DL)**2+(WLCH(2)*DC)**2+(WLCH(3)*DH)**2)
        FBWX=DEN
      ENDIF
      RETURN
  15  FBWX=RWZWIS(WMIN(1),NWE,WSOL,RF)-RMIN(1)
      RETURN
  16  FBWX=SUM(GV)-BTYP
      RETURN
C
C     Probe in senkrecht zur Färbequaracteristik(des Bezugs) liegenden Ebene (L,a,b)
C
C
C     L,a,b berechnen
C
C
  17  CALL LCHAL(JABST,XYZF,ALAB,ALCH,1)
      IF(ITYP.EQ.1) THEN
        DO I=1,3
          ALABT(I)=ALAB(I)
        END DO
C
C       Für Ebene senkrecht
C
        CC=1.01*C
        CALL RWRTTEX(CC,NWE,GU,RF,GV,G,IER)
        CALL NOGXX(XYZF,1,RF,GLANZ(KW))
        CALL LCHAL(JABST,XYZF,ALABCC,ALCH,1)


C
C       Normalenvektor
C
        DO I=1,3
           DALABT(I)=ALABCC(I)-ALABT(I)
        END DO
      ENDIF
      SUMM=0.0
      DO I=1,3
         SUMM=SUMM+DALABT(I)*(ALAB(I)-ALABT(I))
      END DO
      FBWX=SUMM
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
C
C     RÜCKRECHNUNG
C
      INTEGER (KIND=4) FUNCTION IRUECK(DEWESC)
      USE MODQUAL
      USE MODFUNC
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      DIMENSION DEWESC(*)
C
C         PRUEFEN OB RUECKRECHNUNG ERFOLGREICH
C
C         R-WERTE RUECKRECHNUNG
C
          KW=KWS()
          IRUECK=0
          SELECT CASE(IPRG)
            CASE(97,104)
              CALL RWWRTDE2(CF,NWS(),RQ,RUR,A,S,IER)
            CASE(99,105)
              CALL RWWRTTR2(IRETR,CF,NWS(),RQ,RUR,A,S,IER)
          END SELECT
C
C
C 
           DO J=1,2
             CALL NOGXX(XYZP(1,1),1,RP(1,J),GLANZ(KW))
             CALL NOGXX(XYZP(1,2),1,RQ(1,J),GLANZ(KW))
             CALL DELABAL(JABST,XYZP(1,1),XYZP(1,2),DE,DL,DC,DH,DA,DB,1)
             DEWESC(J)=DE
             IF(DE.GT.DESCHW) THEN
                IRUECK=-4114
             ENDIF
           ENDDO
C 
      RETURN
      END
      INTEGER Function IHRMWINK(IHRMID)
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      SELECT CASE (IHRMID)
          CASE (50)
             IHRMWINK=15
          CASE (51)
             IHRMWINK=20
          CASE (52)
             IHRMWINK=25
          CASE (53)
             IHRMWINK=35
          CASE (54)
             IHRMWINK=45
          CASE (55)
             IHRMWINK=55
          CASE (56)
             IHRMWINK=60
          CASE (57)
             IHRMWINK=65
          CASE (58)
             IHRMWINK=70
          CASE (59)
             IHRMWINK=75
          CASE (60)
             IHRMWINK=80
          CASE (61)
             IHRMWINK=85
          CASE (62)
             IHRMWINK=90
          CASE (63)
             IHRMWINK=100
          CASE (64)
             IHRMWINK=105
          CASE (65)
             IHRMWINK=110
          CASE (66)
             IHRMWINK=115
          CASE DEFAULT
             IHRMWINK=0
      END SELECT
      RETURN
      END FUNCTION
C
C
C
C
C
C
      REAL(KIND=8) FUNCTION FUCHROM(DKONZ)
      USE MODQUAL
      USE MODWINK,ONLY:KWC
      USE MODFUNC,ONLY:JABST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ALAB(3),CIE(3)
      DATA TOL/1.D-12/
C
C
C     BERECHNUNG DES CHROMAWERTES AUS DER PSEUDOSCHICHTDICKE DKONZ (DDKONZ<0 über schwarz DDKONZ>0 über weiß)
C
C
      KW=KWC
      GKK=GLZNOG(KW,IRETRA)
C
C        SCHICHTDICKE
C
      IF(DKONZ.GE.0.0) THEN
        CDI=DKONZ
      ELSE
        CDI=-DKONZ
      ENDIF

      IF(CDI.GE.0.0) THEN
C        Über Weiß
         IF(IPRG.EQ.104.OR.IPRG.EQ.97) THEN
C           Deckend
            CALL RWWRTDE(1,CDI,NWS(),RCROM,RUR(1,1),A,S,GV,IER)
         ELSE
C           Transparent
            CALL RWWRTTR(IRETR(1),CDI,NWS(),RCROM,RUR(1,1),A,S,GV,IER)

         ENDIF
      ELSE
C        Über Schwarz
         IF(IPRG.EQ.104.OR.IPRG.EQ.97) THEN
C           Deckend
            CALL RWWRTDE(2,CDI,NWS(),RCROM,RUR(1,2),A,S,GV,IER)
         ELSE
C           Transparent
            CALL RWWRTTR(IRETR(2),CDI,NWS(),RCROM,RUR(1,2),A,S,GV,IER)
         ENDIF
      ENDIF
      CALL NOGXX(XYZF,1,RCROM,GKK)
      CALL LCHAL(JABST,XYZF,ALAB,CIE,1)
      FUCHROM=-CIE(2)
C
C
      RETURN
      END FUNCTION
C****************************************************************************
C****************************************************************************
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
C     Optische Daten (deckend)
C
C
      SUBROUTINE ABSSTRDE(NWE,A,S,IER)
      USE MODFUNC,ONLY:IDaeArt,GMAX,GMIN
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL FUNDECK,MATDECK
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:)::XLB,XUB,X
      DIMENSION A(*),S(*)
C
C
C
      IER=0
C

C
C     BERECHNUNG DER STREU (S)- UND ABSORPTIONS (A) -KOEFFIZIENTEN AUS
C     DEN GEM. REFLEXIONSWERTEN im Weißverschnitt ((RP(:,1)) und
C     DEN GEM. REFLEXIONSWERTEN im Schwarzverschnitt ((RP(:,2))
C
      ALLOCATE(XLB(2*NWE),XUB(2*NWE),X(2*NWE),STAT=IER)
      IF(IFEHL(IERALC(IER)).NE.0)THEN
         GOTO 900
      ENDIF

      KF=2*NWE
      IF(IDaeArt.EQ.2) THEN
        MAL=4*NWE
      ELSE
        MAL=2*NWE+2*(NWE-2)
      ENDIF
      MGAL=0
      MEAL=0
      MAXFUN=50
      FTT=0.000001
      ITT=100
      DO I=1,NWE
         IF(IDaeArt.EQ.2) THEN
            XLB(I)=GMIN
            XLB(NWE+I)=GMIN
         ELSE
            XLB(I)=GMIN
            XLB(NWE+I)=GMIN
         ENDIF
         XUB(I)=GMAX
         XUB(NWE+I)=GMAX
         X(I)=0.001
         X(NWE+I)=GMIN
      END DO
      CALL NLDFIT(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FUNDECK,MATDECK)

      IF(ABS(IFAIL).GT.4000) THEN
         IER=IFAIL
         GOTO 900
      ENDIF
      DO I=1,NWE
        A(I)=X(I)
        S(I)=X(NWE+I)
      END DO
 900  IF(ALLOCATED(X)) THEN
         DEALLOCATE(XLB,XUB,X)
      ENDIF
      RETURN
      END SUBROUTINE
C
C
C
C******************************************************
C******************************************************
C******************************************************************************
C
C     FUNDECK
C
      SUBROUTINE FUNDECK(N,X,XL,XU,NZZ,MCON,F,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
      IER=0
      RD=1.D0
C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      IF(IDaeArt.EQ.2) THEN
        NZZ=4*NWE
      ELSE
        NZZ=2*NWE+2*(NWE-2)
      ENDIF
      DO J=1,2
        DO I=1,NWE
          NHF=(J-1)*NWE+I
          AAH=CF(J)*X(I)    +CW(J)*AW(I)+CS(J)*AS(I)
          SSH=CF(J)*X(NWE+I)+CW(J)*SW(I)+CS(J)*SS(I)
          F(NHF)=RWGEW*(REFDE(AAH,SSH,RD,KW)-RP(I,J))
        END DO
      END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO J=1,2
           IJ=(J-1)*NWE+1
           DO I=1,NWE
              NHF=2*NWE+(J-1)*NWE+I
              F(NHF)=-RWGEW*ALP*ENAPHI(NWE,I,X(IJ),XM)
           ENDDO
        END DO
      ELSE
C
C       Least Square
C

        DO J=1,2
           DO I=2,NWE-1
            NHF=2*NWE+(J-1)*(NWE-2)+I-1
            IJ=(J-1)*NWE+I
            F(NHF)=RWGEW*ALP*(X(IJ-1)-2.*X(IJ)+X(IJ+1))
           ENDDO
        END DO
      ENDIF
C
      RETURN
      END 
C
C
C     MATDECK
C
C
      SUBROUTINE MATDECK(N,X,XL,XU,MDA,NZZ,MCON,F,W,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      DIMENSION W(MDA,*)
      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
      IER=0
      RD=1.D0
C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      IF(IDaeArt.EQ.2) THEN
        NZZ=4*NWE
      ELSE
        NZZ=2*NWE+2*(NWE-2)
      ENDIF
      DO J=1,2
        DO I=1,NWE
          NHF=(J-1)*NWE+I
          DO K=1,N
             W(NHF,K)=0.0
          END DO
          AAH=CF(J)*X(I)    +CW(J)*AW(I)+CS(J)*AS(I)
          SSH=CF(J)*X(NWE+I)+CW(J)*SW(I)+CS(J)*SS(I)
          FF=REFDE(AAH,SSH,RD,KW,REFAA,REFSS)
          W(NHF,I)=RWGEW*CF(J)*REFAA
          W(NHF,NWE+I)=RWGEW*CF(J)*REFSS
        END DO
      END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO J=1,2
           IJ=(J-1)*NWE+1
           DO I=1,NWE
              NHF=2*NWE+(J-1)*NWE+I
              DO K=1,N
                W(NHF,K)=0.0
              END DO
              DO K=1,NWE
                W(NHF,IJ+K-1)=-RWGEW*ALP*ENA2PH(NWE,I,K,X(IJ),XM)
              END DO
           ENDDO
        END DO
      ELSE
C
C       Least Square
C
        DO J=1,2
           DO I=2,NWE-1
            NHF=2*NWE+(J-1)*(NWE-2)+I-1
            DO K=1,N
               W(NHF,K)=0.0
            END DO
            IJ=(J-1)*NWE+I
            W(NHF,IJ-1)=RWGEW*ALP
            W(NHF,IJ)=-2.*RWGEW*ALP
            W(NHF,IJ+1)=RWGEW*ALP
           ENDDO
        END DO
      ENDIF
      RETURN
      END

C
C     OPTISCHE DATEN (transparent)
C
C
      SUBROUTINE ABSSTRTR(NWE,A,S,IER)
      USE MODFUNC,ONLY:IDaeArt,GMAX,GMIN
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL FUNTRANS,MATTRANS
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:)::XLB,XUB,X
      DIMENSION A(*),S(*)
C
C
C
      IER=0
C

C
C     BERECHNUNG DER STREU (S)- UND ABSORPTIONS (A) -KOEFFIZIENTEN AUS
C     DEN GEM. REFLEXIONSWERTEN ÜBER WEISSEM UNTERGRUND (RP(:,1),
C     DEN GEM. REFLEXIONSWERTEN ÜBER SCHWARZEM UNTERGRUND ((RP(:,2))
C     DEN KORR. REFLEXIONSWERTEN DES WEISSEN UNTERGRUNDES (RUR(:,1)),
C     DEN KORR. REFLEXIONSWERTEN DES SCHWARZEM UNTERGRUNDES (RUR(:,2))
C     (SCHICHTDICKE ÜBER WEISS=SCHICHTDICKE ÜBER SCHWARZ (CF)
C
      ALLOCATE(XLB(2*NWE),XUB(2*NWE),X(2*NWE),STAT=IER)
      IF(IFEHL(IERALC(IER)).NE.0)THEN
         GOTO 900
      ENDIF

      KF=2*NWE
      IF(IDaeArt.EQ.2) THEN
        MAL=4*NWE
      ELSE
        MAL=2*NWE+2*(NWE-2)
      ENDIF
      MGAL=0
      MEAL=0
      MAXFUN=50
      FTT=0.000001
      ITT=100
      DO I=1,NWE
         IF(IDaeArt.EQ.2) THEN
             XLB(I)=GMIN
             XLB(NWE+I)=GMIN
         ELSE
             XLB(I)=GMIN
             XLB(NWE+I)=GMIN
         ENDIF
         XUB(I)=GMAX
         XUB(NWE+I)=GMAX
         X(I)=0.001
         X(NWE+I)=GMIN
      END DO
      CALL NLDFIT(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FUNTRANS,MATTRANS)
      IF(ABS(IFAIL).GT.4000) THEN
         IER=IFAIL
         GOTO 900
      ENDIF
      DO I=1,NWE
        A(I)=X(I)
        S(I)=X(NWE+I)
      END DO
 900  IF(ALLOCATED(X)) THEN
        DEALLOCATE(XLB,XUB,X)
      ENDIF
      RETURN
      END SUBROUTINE
C
C
C
C******************************************************
C******************************************************
C******************************************************************************
C
C     FUNTRANS
C
      SUBROUTINE FUNTRANS(N,X,XL,XU,NZZ,MCON,F,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      INTERFACE
      REAL(KIND=8) FUNCTION TRFAS(A,S,RD,KW,RT,TRFAA,TRFSS)
      INTEGER(KIND=4) ::KW,RT
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::TRFAA,TRFSS
      END FUNCTION
      END INTERFACE
      IER=0
C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      IF(IDaeArt.EQ.2) THEN
        NZZ=4*NWE
      ELSE
        NZZ=2*NWE+2*(NWE-2)
      ENDIF
      DO J=1,2
        DO I=1,NWE
          NHF=(J-1)*NWE+I
          AAH=CF(J)*X(I)
          SSH=CF(J)*X(NWE+I)
          RUU=RUR(I,J)
          F(NHF)=RWGEW*(TRFAS(AAH,SSH,RUU,KW,IRETR(J))-RP(I,J))
        END DO
      END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO J=1,2
           IJ=(J-1)*NWE+1
           DO I=1,NWE
              NHF=2*NWE+(J-1)*NWE+I
              F(NHF)=-RWGEW*ALP*ENAPHI(NWE,I,X(IJ),XM)
           ENDDO
        END DO
      ELSE
C
C       Least Square
C
        DO J=1,2
           DO I=2,NWE-1
            NHF=2*NWE+(J-1)*(NWE-2)+I-1
            IJ=(J-1)*NWE+I
            F(NHF)=RWGEW*ALP*(X(IJ-1)-2.*X(IJ)+X(IJ+1))
           ENDDO
        END DO
      ENDIF
C
      RETURN
      END 
C
C
C     MATTRANS
C
C
      SUBROUTINE MATTRANS(N,X,XL,XU,MDA,NZZ,MCON,F,W,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      DIMENSION W(MDA,*)
      INTERFACE
      REAL(KIND=8) FUNCTION TRFAS(A,S,RD,KW,RT,TRFAA,TRFSS)
      INTEGER(KIND=4) ::KW,RT
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::TRFAA,TRFSS
      END FUNCTION
      END INTERFACE
      IER=0

C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      IF(IDaeArt.EQ.2) THEN
        NZZ=4*NWE
      ELSE
        NZZ=2*NWE+2*(NWE-2)
      ENDIF
      DO J=1,2
        DO I=1,NWE
          NHF=(J-1)*NWE+I
          DO K=1,N
             W(NHF,K)=0.0
          END DO
          AAH=CF(J)*X(I)
          SSH=CF(J)*X(NWE+I)
          RUU=RUR(I,J)
          FF=TRFAS(AAH,SSH,RUU,KW,IRETR(J),REFAA,REFSS)
          W(NHF,I)=RWGEW*CF(J)*REFAA
          W(NHF,NWE+I)=RWGEW*CF(J)*REFSS
        END DO
      END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO J=1,2
           IJ=(J-1)*NWE+1
           DO I=1,NWE
              NHF=2*NWE+(J-1)*NWE+I
              DO K=1,N
                W(NHF,K)=0.0
              END DO
              DO K=1,NWE
                W(NHF,IJ+K-1)=-RWGEW*ALP*ENA2PH(NWE,I,K,X(IJ),XM)
              END DO
           ENDDO
        END DO
      ELSE
C
C       Least Square
C

        DO J=1,2
           DO I=2,NWE-1
            NHF=2*NWE+(J-1)*(NWE-2)+I-1
            DO K=1,N
               W(NHF,K)=0.0
            END DO
            IJ=(J-1)*NWE+I
            W(NHF,IJ-1)=RWGEW*ALP
            W(NHF,IJ)=-2.*RWGEW*ALP
            W(NHF,IJ+1)=RWGEW*ALP
           ENDDO
        END DO
      ENDIF
      RETURN
      END
C 
C
C
C                           +-------+
C                           ! FVODE ! 
C                           +-------+ 
      SUBROUTINE FVODE(KW,NWE,KM,QUREDUN,TYREDUN,RDUN,
     &                           QUASUN,TYASUN,RSUN,IER)
      USE MOTWERT
      USE MOTTYRW
      USE MOTQURW
      USE MODFUNC
      USE MODQUAL


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      TYPE(TYRWERT) TYREDUN(*),TYASUN(*)
      TYPE(QURWERT) QUREDUN(*),QUASUN(*)
      REAL(KIND=4),DIMENSION(NWE,KM,*)::RDUN,RSUN
C
C
C     
      IF(TYASUN(1)%ID.NE.TYREDUN(1)%ID) THEN
      	TYASUN(1)=TYREDUN(1)
      	TYASUN(2)=TYREDUN(1)
      	TYASUN(3)=TYREDUN(2)
      	TYASUN(4)=TYREDUN(2)
      	QUASUN(1)=QUREDUN(1)
      	QUASUN(2)=QUREDUN(1)
      	QUASUN(3)=QUREDUN(2)
      	QUASUN(4)=QUREDUN(2)
        QUASUN(1)%CART(3:4)='AW'
      	QUASUN(2)%CART(3:4)='DW'
      	QUASUN(3)%CART(3:4)='AS'
      	QUASUN(4)%CART(3:4)='DS'
      ENDIF
C
C
C
      DO L=1,3
         DO I=1,NWE
            RUM(I,L)=RDUN(I,KW,L)
         ENDDO
      ENDDO
      CALL ABSSTRSW(NWE,AW,SW,AS,SS,IER)

C
C
C
C     ABSORPTIONS- UND STREUKOEFFIZIENTEN FÜR WEISSPIGMENT UND SCHWARZPIGMENT
C
      DO I=1,NWE
         RSUN(I,KW,1)=AW(I)
         RSUN(I,KW,2)=SW(I)
         RSUN(I,KW,3)=AS(I)
         RSUN(I,KW,4)=SS(I)

      END DO
      RETURN
      END
c
c
c

      SUBROUTINE ABSSTRSW(NWE,AW,SW,AS,SS,IER)
      USE MODFUNC,ONLY:IDaeArt,GMAX,GMIN
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AW(*),SW(*),AS(*),SS(*)
      EXTERNAL FUNWESCH,MATWESCH
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:)::XLB,XUB,X
C
C
C
      IER=0
C

C
C     BERECHNUNG DER STREU (SW,SS)- UND ABSORPTIONS (AW,AS) -KOEFFIZIENTEN AUS
C     R-KURVEN Weißpigment,Schwarzpigment und Mischung Weiß/Schwarz =AMWAMS

C
      ALLOCATE(XLB(3*NWE),XUB(3*NWE),X(3*NWE),STAT=IER)
      IF(IFEHL(IERALC(IER)).NE.0)THEN
         GOTO 900
      ENDIF

      KF=3*NWE
      IF(IDaeArt.EQ.2) THEN
        MAL=6*NWE
      ELSE
        MAL=3*NWE+3*(NWE-2)
      ENDIF
      MGAL=0
      MEAL=0
      MAXFUN=50
      FTT=0.000001
      ITT=100
      NWW=2*NWE
      DO I=1,NWE
C
C        AW (Weißpigment)
C
         IF(IDaeArt.EQ.2) THEN
            XLB(I)=GMIN
         ELSE
            XLB(I)=0.0
         ENDIF
         XUB(I)=GMAX
         X(I)=0.01
C
C        AS,SS (Schwarzpigment)
C
         IF(IDaeArt.EQ.2) THEN
            XLB(NWE+I)=GMIN
            XLB(NWW+I)=GMIN
         ELSE
            XLB(NWE+I)=GMIN
            XLB(NWW+I)=GMIN
         ENDIF
         XUB(NWE+I)=GMAX
         XUB(NWW+I)=GMAX
         X(NWE+I)=1.0
         X(NWW+I)=GMIN

      END DO
      CALL NLDFIT(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FUNWESCH,MATWESCH)

      IF(ABS(IFAIL).GT.4000) THEN
         IER=IFAIL
         GOTO 900
      ENDIF
      DO I=1,NWE
        AW(I)=X(I)
        SW(I)=1.0
        AS(I)=X(NWE+I)
        SS(I)=X(NWW+I)
      END DO
      DEALLOCATE(XLB,XUB,X)
 900  RETURN
      END    
C
C
C     FUNWESCH
C
      SUBROUTINE FUNWESCH(N,X,XL,XU,NZZ,MCON,F,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
      IER=0
      RD=1.D0

C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      NWW=2*NWE
      IF(IDaeArt.EQ.2) THEN
         NZZ=6*NWE
      ELSE
         NZZ=3*NWE+3*(NWE-2)
      ENDIF
      DO J=1,3
        DO I=1,NWE
          NHF=(J-1)*NWE+I
          SELECT CASE (J)
              CASE (1)
C
C               Weißpigment
C
                AAH=X(I)
                SSH=1.0
              CASE(2)
C
C               Schwarzpigment
C
                AAH=X(NWE+I)
                SSH=X(NWW+I)
              CASE(3)
C
C               Mischung Weißpigment/Schwarzpigment (AMWAMS)
C
                AAH=X(I)*AMWAMS+X(NWE+I)
                SSH=AMWAMS+X(NWW+I)
          END SELECT
          F(NHF)=RWGEW*(REFDE(AAH,SSH,RD,KW)-RUM(I,J))
        END DO
      END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO J=1,3
           IJ=(J-1)*NWE+1
           DO I=1,NWE
              NHF=3*NWE+(J-1)*NWE+I
              F(NHF)=-RWGEW*ALP*ENAPHI(NWE,I,X(IJ),XM)
           ENDDO
        END DO
      ELSE
C
C       Least Square
C
        DO J=1,3
           DO I=2,NWE-1
              NHF=3*NWE+(J-1)*(NWE-2)+I-1
              IJ=(J-1)*NWE+I
              F(NHF)=RWGEW*ALP*(X(IJ-1)-2.*X(IJ)+X(IJ+1))
           ENDDO
        END DO
      ENDIF
      RETURN
      END 
C
C
C     MATWESCH
C
C
      SUBROUTINE MATWESCH(N,X,XL,XU,MDA,NZZ,MCON,F,W,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      DIMENSION W(MDA,*)
      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
      IER=0

C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      NWW=2*NWE
      IF(IDaeArt.EQ.2) THEN
         NZZ=6*NWE
      ELSE
         NZZ=3*NWE+3*(NWE-2)
      ENDIF
      DO J=1,3
        DO I=1,NWE
          NHF=(J-1)*NWE+I
          DO K=1,N
             W(NHF,K)=0.0
          END DO
          SELECT CASE (J)
              CASE (1)
C
C               Weißpigment
C
                AAH=X(I)
                SSH=1.0
              CASE(2)
C
C               Schwarzpigment
C
                AAH=X(NWE+I)
                SSH=X(NWW+I)
              CASE(3)
C
C               Mischung Weißpigment/Schwarzpigment (AMWAMS)
C
                AAH=X(I)*AMWAMS+X(NWE+I)
                SSH=AMWAMS+X(NWW+I)

              CASE DEFAULT
          END SELECT
          FF=REFDE(AAH,SSH,RD,KW,REFAA,REFSS)
          SELECT CASE (J)
              CASE (1)
C
C               Weißpigment
C
                W(NHF,I)=RWGEW*REFAA
              CASE(2)
C
C               Schwarzpigment
C
                W(NHF,NWE+I)=RWGEW*REFAA
                W(NHF,NWW+I)=RWGEW*REFSS
              CASE(3)
C
C               Mischung Weißpigment/Schwarzpigment (AMWAMS)
C
                W(NHF,I)=RWGEW*AMWAMS*REFAA
                W(NHF,NWE+I)=RWGEW*REFAA
                W(NHF,NWW+I)=RWGEW*REFSS
              CASE DEFAULT
          END SELECT
        END DO
      END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO J=1,3
           IJ=(J-1)*NWE+1
           DO I=1,NWE
              NHF=3*NWE+(J-1)*NWE+I
              DO K=1,N
                W(NHF,K)=0.0
              END DO
              DO K=1,NWE
                W(NHF,IJ+K-1)=-RWGEW*ALP*ENA2PH(NWE,I,K,X(IJ),XM)
              END DO
           ENDDO
        END DO
      ELSE
C
C       Least Square
C

        DO J=1,3
          DO I=2,NWE-1
            NHF=3*NWE+(J-1)*(NWE-2)+I-1
            DO K=1,N
               W(NHF,K)=0.0
            END DO
            IJ=(J-1)*NWE+I
            W(NHF,IJ-1)=RWGEW*ALP
            W(NHF,IJ)=-2.*RWGEW*ALP
            W(NHF,IJ+1)=RWGEW*ALP
          ENDDO
        END DO
      ENDIF
      RETURN
      END
C
C     Optische Daten für FASDEK
C
      SUBROUTINE GWRTDEK(C,IER)
      USE MODFUNC,ONLY:GMAX,IDAEART,GMIN
      USE MODQUAL




      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL FUNDCK,MATDCK
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:)::XLB,XUB,X
C
C
C
      IER=0
C
C     BERECHNUNG DER K/S-Werte
C
      SELECT CASE (IDAEART)
      CASE (1,2)
        NWE=NWS()
        ALLOCATE(XLB(NWE),XUB(NWE),X(NWE),STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
           GOTO 900
        ENDIF
        KF=NWE
        IF(IDaeArt.EQ.2) THEN
          MAL=2*NWE
        ELSE
          MAL=2*NWE-2
        ENDIF
        MGAL=0
        MEAL=0
        MAXFUN=50
        FTT=0.000001
        ITT=100
        DO I=1,NWE
          IF(IDaeArt.EQ.2) THEN
            XLB(I)=GMIN
          ELSE
            XLB(I)=GMIN
          ENDIF
          XUB(I)=GMAX
          X(I)=0.001
        END DO
        CALL NLDFIT(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FUNDCK,MATDCK)

        IF(ABS(IFAIL).GT.4000) THEN
          IER=IFAIL
          GOTO 901
        ENDIF

        DO I=1,NWE
          G(I)=X(I)/C
        END DO
        CALL RWRTDEK(C,NWE,RU,RC,GV,G,IER)
 901    DEALLOCATE(XLB,XUB,X)
      CASE DEFAULT
        CALL GWDEK(C,IER)
      END SELECT
C

 900  RETURN
      END SUBROUTINE
C******************************************************
C******************************************************
C******************************************************************************
C
C     FUNDCK
C
      SUBROUTINE FUNDCK(N,X,XL,XU,NZZ,MCON,F,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
      IER=0
      DATA SSH/1.0D0/
      DATA RUU/1.0D0/
C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      IF(IDaeArt.EQ.2) THEN
        NZZ=2*NWE
      ELSE
        NZZ=2*NWE-2
      ENDIF
      DO I=1,NWE
          NHF=I
          AAH=X(I)
          F(NHF)=RWGEW*(REFDE(AAH,SSH,RUU,KW)-RH(I))
      END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO I=1,NWE
           NHF=NWE+I
           F(NHF)=-RWGEW*ALP*ENAPHI(NWE,I,X,XM)
        ENDDO
      ELSE
C
C       Least Square
C
        DO I=2,NWE-1
         NHF=NWE+I-1
         F(NHF)=RWGEW*ALP*(X(I-1)-2.*X(I)+X(I+1))
        ENDDO
      ENDIF
C
      RETURN
      END 
C
C
C     MATDCK
C
C
      SUBROUTINE MATDCK(N,X,XL,XU,MDA,NZZ,MCON,F,W,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      DIMENSION W(MDA,*)
      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
      DATA SSH/1.0D0/
      DATA RUU/1.0D0/
      IER=0

C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      IF(IDaeArt.EQ.2) THEN
        NZZ=2*NWE
      ELSE
        NZZ=2*NWE-2
      ENDIF
        DO I=1,NWE
          NHF=I
          DO K=1,N
             W(NHF,K)=0.0
          END DO
          AAH=X(I)
          FF=REFDE(AAH,SSH,RUU,KW,REFAA,REFSS)
          W(NHF,I)=RWGEW*REFAA
        END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO I=1,NWE
           NHF=NWE+I
           DO K=1,N
              W(NHF,K)=0.0
           END DO
           DO K=1,NWE
             W(NHF,K)=-RWGEW*ALP*ENA2PH(NWE,I,K,X,XM)
           END DO
        ENDDO
      ELSE
C
C       Least Square
C

           DO I=2,NWE-1
            NHF=NWE+I-1
            DO K=1,N
               W(NHF,K)=0.0
            END DO
            W(NHF,I-1)=RWGEW*ALP
            W(NHF,I)=-2.*RWGEW*ALP
            W(NHF,I+1)=RWGEW*ALP
           ENDDO
      ENDIF
      RETURN
      END
      SUBROUTINE GWDEK(C,IER)
      USE MODFUNC,ONLY:GMAX,GMIN
      USE MODQUAL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C 
      REAL(KIND=8) :: C
      EXTERNAL REFWTA
      DATA RANGE/1.D80/
      RANGIN=1./RANGE

      KW=KWS()
      NWE=NWS()

      TAU=GMAX
      RD=1.D0
      ALB=0.5D0
      DO  I=1,NWE
        IF(RH(I).GT.GINTE(KW)*RD) THEN
           G(I)=0.0
           CYCLE
        ENDIF
        IF(RH(I).LE.GLANZ(KW)) THEN
           G(I)=GMAX
           CYCLE
        ENDIF
        CALL REINV(TAU,ALB,RD,RH(I),REFWTA,KW,2,IEE)
        IF(IEE.NE.0) THEN
           IER=-4018
           G(I)=0.0
           CYCLE
        ENDIF
        RC(I)=REFWTA(TAU,ALB,RD,KW)
        GHH=1.D0/(ALB+RANGIN)-1.D0
        G(I)=MIN(GHH/C,GMAX)
      ENDDO
C 
      RETURN
      END SUBROUTINE
C
C
C     Optische Daten für FASTRA
C
C

      SUBROUTINE GWRTTRA(C,IER)
      USE MODFUNC,ONLY:GMAX,IDAEART,GMIN
      USE MODQUAL




      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL FUNTRA,MATTRA
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:)::XLB,XUB,X
C
C
C
      IER=0
C
C
C     BERECHNUNG DER Extinktionswerte
C
      SELECT CASE (IDAEART)
      CASE (1,2)
        NWE=NWS()
        ALLOCATE(XLB(NWE),XUB(NWE),X(NWE),STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
          GOTO 900
        ENDIF
        KF=NWE
        IF(IDaeArt.EQ.2) THEN
          MAL=2*NWE
        ELSE
          MAL=2*NWE-2
        ENDIF
        MGAL=0
        MEAL=0
        MAXFUN=50
        FTT=0.000001
        ITT=100
        DO I=1,NWE
          IF(IDaeArt.EQ.2) THEN
            XLB(I)=GMIN
          ELSE
            XLB(I)=GMIN
          ENDIF
          XUB(I)=GMAX
          X(I)=0.001
        END DO
        CALL NLDFIT(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FUNTRA,MATTRA)
        IF(ABS(IFAIL).GT.4000) THEN
          IER=IFAIL
          GOTO 901
        ENDIF

        DO I=1,NWE
          G(I)=X(I)/C
        END DO

        CALL RWRTTRA(C,NWE,RU,RC,GV,G,IER)
 901    DEALLOCATE(XLB,XUB,X)
      CASE DEFAULT
        CALL GWTRA(C,IER)
      END SELECT


 900  RETURN
      END SUBROUTINE
C******************************************************
C******************************************************
C******************************************************************************
C
C     FUNTRA
C
      SUBROUTINE FUNTRA(N,X,XL,XU,NZZ,MCON,F,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      INTERFACE
      REAL(KIND=8) FUNCTION TRFAS(A,S,RD,KW,RT,TRFAA,TRFSS)
      INTEGER(KIND=4) ::KW,RT
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::TRFAA,TRFSS
      END FUNCTION
      END INTERFACE
      IER=0
      DATA SSH/0.0D0/
C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      IF(IDaeArt.EQ.2) THEN
        NZZ=2*NWE
      ELSE
        NZZ=2*NWE-2
      ENDIF
      DO I=1,NWE
          NHF=I
          AAH=X(I)
          F(NHF)=RWGEW*(TRFAS(AAH,SSH,RU(I),KW,IRETRA)-RH(I))
      END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO I=1,NWE
           NHF=NWE+I
           F(NHF)=-RWGEW*ALP*ENAPHI(NWE,I,X,XM)
        ENDDO
      ELSE
C
C       Least Square
C
        DO I=2,NWE-1
         NHF=NWE+I-1
         F(NHF)=RWGEW*ALP*(X(I-1)-2.*X(I)+X(I+1))
        ENDDO
      ENDIF
C
      RETURN
      END 
C
C
C     MATTRA
C
C
      SUBROUTINE MATTRA(N,X,XL,XU,MDA,NZZ,MCON,F,W,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      DIMENSION W(MDA,*)
      INTERFACE
      REAL(KIND=8) FUNCTION TRFAS(A,S,RD,KW,RT,TRFAA,TRFSS)
      INTEGER(KIND=4) ::KW,RT
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::TRFAA,TRFSS
      END FUNCTION
      END INTERFACE
      DATA SSH/0.0D0/
      IER=0
C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      IF(IDaeArt.EQ.2) THEN
        NZZ=2*NWE
      ELSE
        NZZ=2*NWE-2
      ENDIF
        DO I=1,NWE
          NHF=I
          DO K=1,N
             W(NHF,K)=0.0
          END DO
          AAH=X(I)
          FF=TRFAS(AAH,SSH,RU(I),KW,IRETRA,TRFAA,TRFSS)
          W(NHF,I)=RWGEW*TRFAA
        END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO I=1,NWE
           NHF=NWE+I
           DO K=1,N
              W(NHF,K)=0.0
           END DO
           DO K=1,NWE
             W(NHF,K)=-RWGEW*ALP*ENA2PH(NWE,I,K,X,XM)
           END DO
        ENDDO
      ELSE
C
C       Least Square
C

           DO I=2,NWE-1
            NHF=NWE+I-1
            DO K=1,N
               W(NHF,K)=0.0
            END DO
            W(NHF,I-1)=RWGEW*ALP
            W(NHF,I)=-2.*RWGEW*ALP
            W(NHF,I+1)=RWGEW*ALP
           ENDDO
      ENDIF
      RETURN
      END
      SUBROUTINE GWTRA(C,IER)
      USE MODFUNC,ONLY:GMAX,GMIN
      USE MODQUAL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C 
      REAL(KIND=8) :: C
      EXTERNAL REFWTA,TRFWTA

      KW=KWS()
      NWE=NWS()
C
C
C
C

      ALB=0.
      TAU=0.5
      DO  I=1,NWE
        IF(IRETRA.EQ.0) THEN
           RUMM=RUMESS(RU(I),KW)
           R00=RUMESS(0.0D0,KW)
        ELSE
           RUMM=TUMESS(RU(I),KW)
           R00=RUMESS(0.0D0,KW)
        ENDIF
        IF(RH(I).LT.RUMM) THEN
          IF(RH(I).LE.-100.) THEN
             TAU=GMAX
          ELSE
            IF(IRETRA.EQ.0) THEN
              CALL REINV(TAU,ALB,RU(I),RH(I),REFWTA,KW,1,IEE)
            ELSE
              CALL REINV(TAU,ALB,RU(I),RH(I),TRFWTA,KW,1,IEE)
            ENDIF
            IF(IEE.NE.0) THEN
               IER=-4018
               G(I)=0.0
              CYCLE
            ENDIF
          ENDIF
        ELSE
          TAU=0.0
        ENDIF
        G(I)=MIN(TAU/C,GMAX)

        IF(IRETRA.EQ.0) THEN
           RC(I)=REFWTA(TAU,ALB,RU(I),KW)
        ELSE
           RC(I)=TRFWTA(TAU,ALB,RU(I),KW)
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE

C
C
C     Optische Daten für FASTEX
C
C
C
C******************************************************
C******************************************************
C******************************************************************************
      SUBROUTINE GWRTTEX(C,IER)
      USE MODFUNC,ONLY:GMAX,IDAEART,GMIN
      USE MODQUAL




      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL FUNTEX,MATTEX
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:)::XLB,XUB,X
C
C
C
      IER=0
C

C
C     BERECHNUNG DER K/S-Werte
C
      SELECT CASE (IDAEART)
      CASE (1,2)
        NWE=NWS()
        ALLOCATE(XLB(NWE),XUB(NWE),X(NWE),STAT=IER)
        IF(IFEHL(IERALC(IER)).NE.0)THEN
          GOTO 900
        ENDIF
        KF=NWE
        IF(IDaeArt.EQ.2) THEN
          MAL=2*NWE
        ELSE
          MAL=2*NWE-2
        ENDIF
        MGAL=0
        MEAL=0
        MAXFUN=50
        FTT=0.000001
        ITT=100
        DO I=1,NWE
          IF(IDaeArt.EQ.2) THEN
            XLB(I)=GMIN
          ELSE
            XLB(I)=GMIN
          ENDIF
          XUB(I)=GMAX
          X(I)=0.001
        END DO
        CALL NLDFIT(MAL,MGAL,MEAL,KF,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FUNTEX,MATTEX)

        IF(ABS(IFAIL).GT.4000) THEN
          IER=IFAIL
          GOTO 901
        ENDIF

        DO I=1,NWE
          G(I)=X(I)/C
        END DO
        CALL RWRTTEX(C,NWE,RU,RC,GV,G,IER)
 901    DEALLOCATE(XLB,XUB,X)
      CASE DEFAULT
        CALL GWTEX(C,IER)
      END SELECT
 900  RETURN
      END SUBROUTINE
C******************************************************
C******************************************************
C******************************************************************************


C
C     FUNTEX
C
      SUBROUTINE FUNTEX(N,X,XL,XU,NZZ,MCON,F,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
      IER=0
      DATA SSH/1.0D0/
      DATA RUU/1.0D0/
C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      IF(IDaeArt.EQ.2) THEN
        NZZ=2*NWE
      ELSE
        NZZ=2*NWE-2
      ENDIF
      DO I=1,NWE
          NHF=I
          AAH=GU(I)+X(I)
          F(NHF)=RWGEW*(REFDE(AAH,SSH,RUU,KW)-RH(I))
      END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO I=1,NWE
           NHF=NWE+I
           F(NHF)=-RWGEW*ALP*ENAPHI(NWE,I,X,XM)
        ENDDO
      ELSE
C
C       Least Square
C
        DO I=2,NWE-1
         NHF=NWE+I-1
         F(NHF)=RWGEW*ALP*(X(I-1)-2.*X(I)+X(I+1))
        ENDDO
      ENDIF
C
      RETURN
      END 
C
C
C     MATTEX
C
C
      SUBROUTINE MATTEX(N,X,XL,XU,MDA,NZZ,MCON,F,W,IER)
      USE MODQUAL
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*)
      DIMENSION X(*),XL(*),XU(*)
      DIMENSION W(MDA,*)
      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE
      DATA SSH/1.0D0/
      DATA RUU/1.0D0/
      IER=0

C
C
C
      MCON=0
      KW=KWS()
      NWE=NWS()
      IF(IDaeArt.EQ.2) THEN
        NZZ=2*NWE
      ELSE
        NZZ=2*NWE-2
      ENDIF
        DO I=1,NWE
          NHF=I
          DO K=1,N
             W(NHF,K)=0.0
          END DO
          AAH=GU(I)+X(I)
          FF=REFDE(AAH,SSH,RUU,KW,REFAA,REFSS)
          W(NHF,I)=RWGEW*REFAA
        END DO
      IF(IDaeArt.EQ.2) THEN
C
C       Maximale Entropie
C
C
        DO I=1,NWE
           NHF=NWE+I
           DO K=1,N
              W(NHF,K)=0.0
           END DO
           DO K=1,NWE
             W(NHF,K)=-RWGEW*ALP*ENA2PH(NWE,I,K,X,XM)
           END DO
        ENDDO
      ELSE
C
C       Least Square
C

           DO I=2,NWE-1
            NHF=NWE+I-1
            DO K=1,N
               W(NHF,K)=0.0
            END DO
            W(NHF,I-1)=RWGEW*ALP
            W(NHF,I)=-2.*RWGEW*ALP
            W(NHF,I+1)=RWGEW*ALP
           ENDDO
      ENDIF
      RETURN
      END
      SUBROUTINE GWTEX(C,IER)
      USE MODFUNC,ONLY:GMAX,GMIN
      USE MODQUAL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C 
      REAL(KIND=8) :: C
      EXTERNAL REFWTA
      DATA TLL/5.D-5/
      KW=KWS()
      NWE=NWS()
C
C
      TAU=GMAX
      RD=1.D0
      ALB=0.5
      DO  I=1,NWE
       IF(RH(I).GT.GINTE(KW)*RD) THEN
         G(I)=0.
         RC(I)=RH(I)
         CYCLE
       ENDIF
       IF(RH(I).LE.GLANZ(KW)) THEN
           G(I)=GMAX
           CYCLE
       ENDIF
       CALL REINV(TAU,ALB,RD,RH(I),REFWTA,KW,2,IEE)
       IF(IEE.NE.0) THEN
         IER=-4018
       ENDIF
       RC(I)=REFWTA(TAU,ALB,RD,KW)
       GHH=1./(ALB+TINY(1.))-1.
       IF(GU(I).LE.TLL) THEN
           RC(I)=RH(I)
       ELSE
           RC(I)=REFWTA(TAU,ALB,RD,KW)
       ENDIF
       G(I)=MIN((GHH-GU(I))/C,GMAX)
       IF(G(I).LT.0.0) THEN
         G(I)=0.0
       ENDIF
      ENDDO
      RETURN
      END SUBROUTINE
C

