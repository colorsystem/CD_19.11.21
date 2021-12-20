C     Last change: KU 14.02.2021 09:33:36
C
C
C     Modul f¸r SBOCLS
C
C

      MODULE MODSBOC
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTEGER(KIND=4) ::ITMAS,ITER,ITMAX,MVA0
      REAL(KIND=8) :: WT1,WT2,FAC0
      INTEGER(KIND=4),DIMENSION(5) :: IDOPE
      DATA WT1/0.0001/,WT2/1.0/,FAC0/0.0/
      DATA MVA0/0/,ITMAS/10/
C
C     WT1 (ETA in Publication) WT-Wert in SBOCLS
C     WT2                      WT-Wert in SBOLSM (1.0)
C     FAC0                    FAC-Wert in SBOLSM (0.0)
C     ITMAS                   Mindestanzahl Iterationen in SBOLSM
C     ITMAX=MAX(ITMAS,5*(MAX(MROWS,NCOLS))
C     ITER                    AKtuelle Anzahl Iterationen
C
C
      END MODULE

C
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MOSRWRT
C****************************************************************************
C****************************************************************************
C****************************************************************************

      MODULE MOSRWRT
      IMPLICIT REAL(KIND=8) (A-H,O-Z)

      TYPE  SRWERT
                SEQUENCE
C
C
                INTEGER(KIND=4) :: ID
C
C
C
C               BEZEICHNUNG FUER n.TE MESSUNG (CART(1:1)
C
C               ART DES UNTERGRUNDES (CART(4:4))
C                   W    =   WEISSER UNTERGRUND
C                   S    =   SCHWARZER UNTERGRUND   
C
C               ART DER MESSUNG (CART(2:3))
C
C
C                   TM   =   TYP (ODER VORLAGE) GEMESSEN
C                   TR   =   TYP RUECKGERECHNET
C                   TB   =   TYP BERECHNET
C                   TA   =   TYP FUER ABSORTION = 0.
C                   TS   =   TYP FUER STREUUNG  = 0.
C                   TC   =   TYP FUER MAXIMALES CHROMA
C                   TU   =   TYP FUER UNENDLICH DICKE SCHICHT
C
C                   Px   =   DSGL. FUER PROBE (ODER NACHSTELLUNG)
C                  
C                   UM   =   UNTERGRUND GEMESSEN
C                   UR   =   UNTERGRUND RUECKGERECHNET
C                   UK   =   UNTERGRUND KORRIGIERT (Z.B. DURCH GLANZABZUG)
C                    
C                   WM   =   WEISSPIGMENT GEMESSEN
C                   WR   =   WEISSPIGMENT RUECKGERECHNET
C
C                   SM   =   SCHWARZPIGMENT GEMESSEN
C                   SR   =   SCHWARZPIGMENT RUECKGERECHNET
C
C                   GM   =   GRAUPIGMENT GEMESSEN
C                   GR   =   GRAUPIGMENT RUECKGERECHNET
C
C                   RM   =   REFERENZMESSUNG
C                   RR   =   REFERENZMESSUNG RUECKGERECHNET
C
C
                CHARACTER(len=4) :: CART
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
C               SCHICHTDICKE
C
C               CAMP(0)    
C
C 
C
C
C               PARAMETER (MENGEN, KONZENTRATIONEN U. AE. M.)
C              
                REAL(KIND=8),dimension(0:15)::CAMP
C
C
C               STANDARDABWEICHUNG FUER FARBABSTAND (ein Sigma)
C
                REAL(KIND=8):: DESTD
C
C               LAUFENDE NUMMER
C
                INTEGER(KIND=2) :: NR

C
C               REFLEXION(0) ODER TRANSMISSION(1)
C     
                INTEGER (KIND=2):: RETR
C
C
C               ANZAHL MESSUNGEN
C
                INTEGER (KIND=2):: IAMI
C
C               DUMMY
C
C
                INTEGER (KIND=2) IVONA
C
C
C               Remissions- bzw. Transmissionswerte
C
                REAL(KIND=8),POINTER,DIMENSION(:) :: R
C
C
      END TYPE

      END MODULE
C
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MOTTYRW
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
      MODULE MOTTYRW
      TYPE TYRWERT
                SEQUENCE
C               R-WERT ID
C
C
                INTEGER(kind=4):: ID
C               
C
C
C               RWERT_NR
C
                INTEGER(kind=2):: NR
C               
C               Kennung ¸ber weiﬂem(1) oder ¸ber schwarzem (2) Untergrund
C
                INTEGER(KIND=2) :: KWB
C        
C               0= REFLEXION; 1= TRANSMISSION
C     
                INTEGER(KIND=2) :: RETR
C
C               0 nicht vorhanden; 1 vorhanden
C
                INTEGER(KIND=2) :: IVONA
C
      END TYPE
      END MODULE
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MOTQURW
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
      MODULE MOTQURW
      TYPE QURWERT
                SEQUENCE
C               METHODEN ID
C
                INTEGER(KIND=4) :: METHID
C
C               
C               ID FUER UNTERGRUND
C
                INTEGER(KIND=4) :: IUNTID
C
C
C               DICKE,MENGEN usw.
C
                REAL(KIND=4),dimension(0:15) ::  CAMP
C
C
C               TOLERANZEN FARBABSTAENDE
C
C
C
                REAL(KIND=4),dimension(16):: DE
C
C
C
C               ANZAHL MESSUNGEN
C
                INTEGER(KIND=2) :: IAMI
                INTEGER(KIND=2) :: IDUM

C

C
C
C               CHARAKTERISIERUNG DER MESSUNG (z.B. @TMW usw.)
C
C
C
C            
                CHARACTER(LEN=4) :: CART
C    
C           
C
      END TYPE
      END MODULE
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MOTWERT
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
      MODULE MOTWERT
      TYPE  TYPARAM
                SEQUENCE
C               
C               RWERT-ID
C
C
                INTEGER(kind=4) :: ID
C
C               AUSWAHL ID (z.B. DIN6174 u.‰.m)
C
                INTEGER(kind=4) :: AuswID
C
C
C
C               TYP/PROBE
C               
                INTEGER(kind=2) :: ITP
C

C
C               RWERT-NR
C
C
                INTEGER(kind=2) :: NR
C
C
C
C
                INTEGER(KIND=2) :: KWB
C
C               REFLEXION(0) ODER TRANSMISSION(1)
C
C
                INTEGER(KIND=2) :: RETR
C
C
C               KENNUNG (TYP/PROBE-ART,TYP/PROBE,MESSUNG/RECHNUNG usw.,
C                        WEISS/SCHWARZ,NORMLICHTART,WINKEL)
C
                CHARACTER(LEN=8) CMETH
C
C
C
C               Laufende Nummer zusammengehˆriger Auswertungen
C
C
                INTEGER(KIND=4) :: LNR
C
C
C
      END TYPE
      END MODULE
C

C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MOTFEHL
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
      MODULE MOTFEHL
      TYPE TYFEH
                SEQUENCE
C               
C               FEHLER-CODE
C
C
                INTEGER(kind=2) :: IFEH
C
C               
C               FEHLER(0) ODER WARNUNG(1)
C
C
                INTEGER(kind=2) :: IWARN
C
C               FEHLER-KENNUNG (z.B. WINKEL)
C
                INTEGER(kind=4) :: KENN
C               FEHLER-WERT
C
                REAL(kind=4) :: WERT
C              
C               FEHLER-TEXT
C
C
C
                CHARACTER(LEN=8) :: CFEH
C
C
C
      END type
      END MODULE

C     Last change:  UFO  25 Apr 105   11:41 am
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODILLU
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODILLU
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTEGER(KIND=4) ::NB,NWE,NLZ
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) ::NLA
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: FA,WSOL
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: FTKT
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:) :: EILLU
C
C
C
C
      END MODULE
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MOD6164
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MOD6164
      IMPLICIT REAL(KIND=8) (A-H,O-Z)

      INTEGER(KIND=4) ::NW35
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: NL35
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: WSOL35
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: F6164
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:) :: E6164
C
      INTEGER(KIND=4) ::ND
      REAL(KIND=8):: PHK,PHMIN,PHMAX
      REAL(KIND=8),DIMENSION(2):: XY0(2)

C
C
C
C
      END MODULE
C      ************
C****************************************************************************
C     MODULE MODWINK
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
      MODULE MODWINK
C
C     IHRMWIN     Winkel (in Grad) oder IHRM_IHRM aus TBL_IHRM (Datenbank)
C     FWI         Gewicht f¸r Winkel
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) ::IHRMWIN
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: FWI
      INTEGER(KIND=4) :: KM,KWC
      END MODULE
C

C

C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODGKWR
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
C
      MODULE MODGKWR
      IMPLICIT REAL(KIND=8) (A-H,O-Z)

C     GK(1)  GLANZ 
C     GK(2)  INNERE GERICHTETE REFLEXION 
C     GK(3)  INNERE DIFFUSE REFLEXION
C     GK(4)  ANTEIL GERICHTET FUER UNTERGRUND
C     GK(5)  ANTEIL DIFFUS-DIFFUS FUER UNTERGRUND
C     GK(6)  ANTEIL VORWAERTSSTREUUNG (0.5==> ISOTROP   0.==> KUB.-MUNK)
C     GK(7)  ANTEIL RUECKWAERTSSTREUUNG (0.5==> ISOTROP 1.==> KUBELKA MUNK)
C     GK(8)  SCHMELZER PARAMETER 1  = 0.1355
C     GK(9)  SCHMELZER PARAMETER 2  = 2.2710
C     GK(10) GESAMTINTENSITAET 
C     GK(11) POTENZ FUER TAU
C     GK(12) POTENZ FUER (1.-OM0)
C     GK(13) Faktor f¸r CDE='U','C'
C     GK(14) Faktor f¸r CDE='U','C'
C     GK(15) Faktor f¸r CDE='U','C'
C     GK(16) PARAMETER ZUR BERECHNUNG DES MODIFIZIERTEN TAU-WERTES (CDE(2:2)=2,3)
C     SONDERFAELLE
C                         GK(8)=GK(9)=0.
C                         SF=0. SB=1.              KUBELKA-MUNK
C
C                         GK(8)=GK(9)=0.
C                         SF=SB=0.5                2-FLUSS ISOTROP
C
C                         GK(8)=0.1355  GK(9)=2.2710
C                         S=0.                     SCHMELZER-HOFFMANN
C
C     Literatur:
C     MISCHFORMEL HOFFMANN (FARBE+LACK 87(1981)15)
C                 SCHMELZER(DEFAZET 9(1977)373)
C     MEADOR+WEAVER (APPL.OPTICS,12(1976)3155)
C
C
C
C     FOPT=FAKTOR (z.B. =  1. nat¸rlicher Logarithmus
C                          .4342945 zur Umrechnung in dekadischen Logarithmus
C                          2 zur Beruecksichtigung des halben Lichtdurchgangs bei Transmission
C                          .8685889 beides
C
C     FWI         Gewicht f¸r Winkel
C

      INTEGER(KIND=4) :: KA
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: GK
      REAL(KIND=8) ::FOPT,EPS,CTT,TOL,RANG,RANN,RANGIN,EEP,ZERO,SML,
     &               EAB,AKA
      CHARACTER(LEN=2) :: CDE
C
C     DATA
C
C
C
      DATA EPS/1.D-64/
      DATA CTT/5.D+80/
      DATA TOL/1.D-12/
      DATA SML/1.D-2/
      DATA EAB/1.D-4/
      DATA RANG/1.D100/,RANN/1.D140/
      DATA RANGIN/1.D-80/
      DATA EEP/1.D-80/
      DATA ZERO/0.D0/
      DATA FOPT/1.0D0/


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
      END MODULE
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODD99
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODD99
C
C     Konstanten f¸r DIN99
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8) :: AA0,AA1,AA2,AA3,AA4,AA5,COS26,SIN26,RAD26
      DATA AA0/303.67/
      DATA AA1/0.0039/
C     303.67*0.0039
      DATA AA2/1.184313/
      DATA AA3/0.83/
      DATA AA4/0.075/,AA5/0.0435/
C     COS(0.453785605518),SIN(0.453785605518)
      DATA COS26/0.898794046299/,SIN26/0.438371146789/
      DATA RAD26/0.453785605518/

C
      END MODULE
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODCMC
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODCMC
C
C     Konstanten f¸r alternative CMC-Formel von
C     Aldo Colli, E Gremmo, P.Moniga: Pitture e Vernici April 1989 VOL.: 4 (page 19 ?)
C
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      DIMENSION AA(4),BB(4),AL(5),AS(4)
      DATA ECMC/1.E-5/
      DATA AA/ 0.13893,-0.05390,-0.04029,-0.01043/
      DATA BB/-0.09622,-0.06742,-0.02137, 0.00587/
      DATA AL/ 1.171078, 6.7488 ,-10.164, 5.641 ,-.82613/
      DATA AS/ 0.125   , 0.993  , 1.076 , 0.09  /
      DATA AH/ 0.17914/
      DATA FPI/0.0174533/
      END MODULE
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODFAKT
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODFAKT
      IMPLICIT REAL(KIND=8) (A-H,O-Z)

C
C     Parameter zur Berechnung spezieller Merkmale
C
C
C
C
C     XL (Helligkeit) FAKTOR F‹R CMC-FORMEL
C     XC (Chroma) FAKTOR F‹R CMC-FORMEL
C     AKCH CHROMAKORREKTUR F‹R DIN6176
C     AKE FAKTOR GESAMTFARBABSTAND (DIN6176)
C     AKL94    KORREKTURFAKTOR (HELLIGKEIT) F‹R CIE94-Formel
C     AKC94    KORREKTURFAKTOR (CHROMA) F‹R CIE94-Formel
C     AKH94    KORREKTURFAKTOR (BUNTHEIT) F‹R CIE94-Formel
C     AKL2000  KORREKTURFAKTOR (HELLIGKEIT) F‹R CIE94-Formel
C     AKC2000  KORREKTURFAKTOR (CHROMA) F‹R CIE94-Formel
C     AKH2000  KORREKTURFAKTOR (BUNTHEIT) F‹R CIE94-Formel
C     AMYGS    KORREKTURFAKTOR ZUR BERECHNUNG DER SCHWARZZAHL(DEGUSSA)
C     AMYGSF   KORREKTURFAKTOR ZUR BERECHNUNG DER SCHWARZZAHL(DEGUSSA)
C     AGYGS    KORREKTURFAKTOR ZUR BERECHNUNG DER GRAUZAHL(DEGUSSA)
C     AGYGSF   KORREKTURFAKTOR ZUR BERECHNUNG DER GRAUZAHL(DEGUSSA)
C     GL-WERT FUER DIN6176 6175-2
C     GA-WERT FUER DIN6176 6175-2
C     GB-WERT FUER DIN6176 6175-2
C     GC-WERT FUER DIN6176 6175-2
C     GH-WERT FUER DIN6176 6175-2
C     DFKI zur Berechnunf des Farbkonstanz-Indizes CMCCAT200 Georg A. Klein
C     kLIGHT  Korrekturfaktor f¸r Helligkeit (NBS)
C     KGLOSS  Glanzfaktor (NBS)
      REAL(KIND=8) ::XL,XC,AKCH,AKE,AMYGS,AMYGSF,AGYGS,AGYGSF,
     &               GL,GA,GB,GC,GH,
     &               AKL94,AKC94,AKH94,kLIGHT,KGLOSS,
     &               AKL2000,AKC2000,AKH2000,DFKI
      END MODULE
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODQUAL
C****************************************************************************
C****************************************************************************
C****************************************************************************

      MODULE MODQUAL
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTEGER(KIND=4) :: IXYZT,ITYP,IPRG
      REAL(KIND=8) :: BTYP,WWD
      INTEGER(KIND=4),DIMENSION(1) :: KMIN
      REAL(KIND=8) WMIN(1),RMIN(1),XYZT(3),XYZF(3),ALABT(3),DALABT(3)
      INTEGER(KIND=4) :: IM,IV,IRETRA
      INTEGER(KIND=4) IRETR(2)
      REAL(KIND=8) ::DEDW0,DEDS0,DEDW,DEDS,DCROM,QLW,QLS
      REAL(KIND=8) XYZP(3,2),XYZQ(3,2),XYZH(3),
     &             CF(2),CW(2),CS(2)
C
C     (NWE)
C
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:)::RH,GH,RCROM,RU,GU,RC,RF,
     &                                       GV,G,A,S,AW,SW,AS,SS,XM
C
C     (NWE,2)
C
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:)::RP,RQ,RUN,ROK,ROS
C
C     (NWE,3)
C
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:)::RUR,RUM

      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) ::FCA
      DATA IV/14/
      END MODULE
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODFOGR
C****************************************************************************
C****************************************************************************
C****************************************************************************

      MODULE MODFOGR
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
C
C     (NFAB)
C
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: CZWI
C
C     (NWE,NFAB)
C
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: RZWI
      INTEGER(KIND=4) :: JZWI
      END MODULE
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODSPZL (f¸r LSTERNBW)
C****************************************************************************
C****************************************************************************
C****************************************************************************

      MODULE MODSPZL
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8),DIMENSION(3) :: LCHST
      INTEGER (KIND=4) :: NLI,ILCH
      END MODULE

C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODFUNC
C****************************************************************************
C****************************************************************************
C****************************************************************************

      MODULE MODFUNC
      IMPLICIT REAL(KIND=8) (A-H,O-Z)

C     KBWT NUMMER B_WERT (BWERT_ID)
C            1     1/1
C            2     1/3
C            3     1/9
C            4     1/25
C            5     1/200
C            6     1/6
C            7     1/12
C            8     2/1    
C
C     IART ART DER FARBSTAERKEBERECHNUNG (FSTAE_ID)
C     KABS AUSGABE VON ABSOLUTWERTEN FUER TYP
C     FDE  KONTRASTFARBABSTAND
C     AMWAMS WEISS/SCHWARZVERHAELTNIS
C     WELFST Wellenl‰nge, f¸r die eine Farbst‰rke berechnet werden soll
C     DESCHW Schwellenwert f¸r Farbabstand
C     (¸bersteigt der Unterschied Messung-Rechnung z.B. beim Deckvermˆgensprogramm diesen Wert, so erfolgt Fehlermeldung)
C     WLCH GEWICHTE f¸r L*,C*,h
C
      INTEGER(KIND=4) ::JABST,KABS,KBWT,IART,IDaeArt
      REAL(KIND=8) ::FDE,AMWAMS,DESCHW,WELFST,ALP,GMIN,GMAX,RWGEW
      REAL(KIND=8) WLCH(3)
      DATA JABST/-1/,RWGEW/1.0/,GMIN/0.0000001/
      END MODULE
C
C
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODFARB
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODFARB
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTEGER(KIND=4) ::NR,NF,IBA,IBI,IBW,IBB,NF0,KFJT
      REAL(KIND=8) :: EPL,REPS,EPSAMG,SMENG
C
C     Dimension(NF)
C
      CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:) :: KTO
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: LK,ICHF
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: ACLIM,ACONA,CONA,
     &                               ACBEST,PRE,ACZWI,FASTART

C
C     Dimension(NF+1)
C
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) ::ACONZ
      DATA EPL/1.D-70/,REPS/5.D-3/,EPSAMG/1.D-4/,SMENG/1.D-2/
      END MODULE
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODABST
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
C
      MODULE MODABST
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTEGER(KIND=4):: NPX,NPP,NZUS
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: NST
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:) ::CST
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:,:) :: ABSTA
      DATA NZUS/4/
      END MODULE
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODRAUM
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODRAUM
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTEGER(KIND=4) OPT,KUNT,KWB,NLJ,NAV,ISTA,NEZ
      REAL(KIND=8) DER,ALCC(3),XYZ(3,2),DVV,ALCH(3),GFU
      END MODULE
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODRWRZ
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODRWRZ
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: D,ASHIL,TST,ABLEI,RD,DALT
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:) ::
     &             R0,RV,RN,RY,RU,RR,RSU,ABSSU
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:)::DR,RGWW
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:) :: AD
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:,:) :: RAS,RHLF
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:,:,:) :: AQ
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: IRETR,IKWB
      END MODULE
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODXYRZ
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODXYRZ
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:,:) ::
     &             XYZV,XYZN,XYZB
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:) :: SPHI,CPHI
C     SPHI = SINUS DES WINKELS ZWISCHEN A*-ACHSE UND VEKTOR DER VORLAGE IN
C            DER A*,B* - EBENE
C     CPHI = DSGL. FUER COSINUS
      END MODULE
C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODGREN
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODGREN
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      INTEGER(KIND=4) :: NEQ,NEE,NEM,NF1
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:,:) :: IGR
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) ::IC
      REAL(KIND=8) ::  GEWEQU,GEWTOL,ZUWIT,AMGGRZ
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: AGR
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) ::   CL,CU
      CHARACTER(LEN=1),DIMENSION(:),ALLOCATABLE :: OP

C     NEQ ANZAHL GLEICHUNGEN
C     NEE ANZAHL GLEICHUNGEN + UNGLEICHUNGEN
c     NEM ANZAHL "GLEICHUNGEN" FUER MINIMIERUNGSBEZIEHUNGEN
      DATA NF1/0/,ZUWIT/1.0D0/,AMGGRZ/1.0D100/
      END MODULE
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODHILF
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODHILF
      IMPLICIT REAL(KIND=8) (A-H,O-Z)

      INTEGER(KIND=4) :: IHLF
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: KFH
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: DIHLF,ACH,CBH,CBY
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) ::AMA,CADD,CADI

      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: AMHLF,PIV,PIW,RNORM
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:,:)::RH,RZ
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:)::RZW
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:,:,:) :: XYZH
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: IP
      END MODULE


C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODSORT
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODSORT
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8) ::DVV,DET
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) ::ASOS,CS
      REAL(KIND=8),DIMENSION(16) :: DEG
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION (:) ::KFS,KNUS
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:,:) :: LKS
      DATA DVV/9.0/,DEG(1)/0.0/,DET/0.0/
      END MODULE

C
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODMERZ
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C
      MODULE MODMERZ
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: FU
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: DEV
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: DI
      REAL(KIND=8)  GGEM,GGE,GXG,GXD,GTO,DTO,AKK,ATO,FTO,DEL,SPR,
     &              WLCH(3),DEOK,DEHLF,DEGUT,ANORM,
     &              FDE,GDE,TOLM,EPMNG
      INTEGER(KIND=4)JABST,KRE,KMI,KMA,ICHI,KGX,IGX,KWE,KWO,KWM,ITM,
     &                NPXMEN,JN,JUV,JUN,JUU,JO,KOMB,KSOR,ISOR,KBPROZ,
     &                IKOR,GKIDMEN
      DATA IKOR/0/,JABST/0/
      DATA DICK/1.0/
      DATA GKIDMEN/-1/
      DATA TOLM/1.D-3/
      DATA DEL/1.D-4/
      END MODULE
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODFEHL
C****************************************************************************
C****************************************************************************
C****************************************************************************

      MODULE MODFEHL
      USE MOTFEHL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE(TYFEH) FEHH
      DATA FEHH%IFEH/0/
      DATA FEHH%KENN/0/
      DATA FEHH%IWARN/0/
      DATA FEHH%WERT/0.0/
      END MODULE
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODAASS
C****************************************************************************
C****************************************************************************
C****************************************************************************

C
C
C
C     F¸r Subroutine ABSSTRTR
C
      MODULE MODAASS
      REAL(KIND=8) :: ALP,EPSL
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: XSTC,SIGM,XM,OM0
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:)::RZ,RUZ
      DATA EPSL/1.D-80/
      END MODULE
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODRMEM
C****************************************************************************
C****************************************************************************
C****************************************************************************
      MODULE MODRMEM
C
C
C     F¸r Subroutine Rmemi und alle damit zusammenh‰ngenden Unterprogramme
C
      INTEGER(KIND=4) :: ILETZT,MEID,NORID,IPM
      REAL(KIND=8),DIMENSION(25)::SDE
      REAL(KIND=8),DIMENSION(5) ::DE,DL,DH,DC,DA,DB

C
C     KM
C
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: KME,KMN
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: DEW
C
C     KM*NWE
C
C
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:):: RS,RQ,RSN,RQN,R,RO,RU
C
C     KM*NWE,5
C
C
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:):: RH
C
C     KM*NWE,IPM
C
C
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:):: RM
      DATA IPM/25/
      END MODULE
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODFAUM
C****************************************************************************
C****************************************************************************
C****************************************************************************
      MODULE MODFAUM
c
C     F¸r Subroutine FarbUmre Subroutine ENT
c
C     XYZ(3*NLZ), XM(NWE)
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: XYZ,XM,XHIL
      INTEGER (KIND=4) :: KWF,NLN
      REAL(KIND=8) :: ALPN
      END MODULE
C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODGRME
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C     F¸r GRUNDDATEN
C
      MODULE MODGRME
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) ::FU
      REAL(KIND=8) :: DICK,EXPO,GGW,FTO,GEWDIK,DELM
      INTEGER(KIND=4) :: JABST,JTM,NPQ,NPQ1,NFG,IGLAE,NPX,NPP,NZUS
      REAL(KIND=8) :: GRMAX
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) ::NST
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: CSTA
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: PH
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:) :: CST
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: FST



      DATA NZUS/4/,JABST/0/,DELM/0.0000001/
      END MODULE


C****************************************************************************
C****************************************************************************
C****************************************************************************
C     MODULE MODGRUN
C****************************************************************************
C****************************************************************************
C****************************************************************************
C
C     F¸r GRUNDDATEN
C
      MODULE MODGRUN
C
C     MF Anzahl Farb-/Bindemittel
C     M  Anzahl Rezepte, die zur Grunddatenberechnung herangezogen werden kˆˆnen (KWBRE>0))
C
C            ICHF
C               0 = Farbmittel
C               1 = Bindemittel
C               2 = Weiﬂpigment
C               3 = Schwarzpigment
C               4 = Metallic
C               5 = Effektpigment
C               6 = RESTFARBE
C               7 = LABORFARBE
C               8 = Zusatzmittel
C               9 = frei

      INTEGER(KIND=4) :: MF,M,IS,ICST,IX,NQ,MT,MX,MQ,KFW,
     &                   NEE,NEG,NEV,ME,NGL,NDIK,NED,NEC
      INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:) :: LCQ,LDIK,NRRWRT,
     &                ICHF,LD,LCST,LX,KX,IRTU,KUNT,KU,LQ,KQ,NANZ,LC,INC
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: ACQ,DIKALT,
     &                PDUM,RABL,AKOP,PMSTZ,DAE,CU,CL
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) ::
     &                DR,DRA,CQ,AVAL,ADAE,AGGR,DIK,ADIK,ACST
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:) ::RUM,RWB,RR

      CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:,:) :: CCQ

      DATA NDIK/0/,NED/0/,NCST/0/,NEC/0/
      END MODULE



