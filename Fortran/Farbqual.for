C     Last change: KU 23.06.2021 18:03:46
C
C
C
C
      INCLUDE "GETLWGK.FOR"
C
C
C
C
c
c
c
c      INCLUDE "FAKER4.FOR"
c      INCLUDE "FAKER3.FOR"
c      INCLUDE "MATLIB.FOR"
c      INCLUDE "MSCHLIB.FOR"
C
C     BWEBER mit QUABEG und QUAEND
C
      INCLUDE "BWEBER.FOR"
      INCLUDE "QUALSTEN.FOR"
C
      INCLUDE "FRBMEN.FOR"
      INCLUDE "FARBQUZW.FOR"
      INCLUDE "FASFOG.FOR"
C
C
c      INCLUDE "MSCHLIB.FOR"
C      INCLUDE "MATLIB.FOR"
c      INCLUDE "FAKER4.FOR"
c      INCLUDE "FAKER5.FOR"
c      INCLUDE "FRBL0.FOR"
c      INCLUDE "FRBL1.FOR"

C
C
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C***************************************** FASDEKS ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c     deckende Schichten(spezial)
C
      SUBROUTINE FASDEKS(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUKS,TYKS,RKS,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 NQU,NPAM,KW,NWEL,KML,KFIT,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUKS(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYKS(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*)::RDAM,RDAB,RKS,RDUN
      REAL(KIND=8),DIMENSION(64,*)::WERT
      TYPE(TYPARAM) PARAM(*)
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR

C
C
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C 
C    IART
C
C 
C    (0) Y-TYP              ;      DECKEND (Z.B. LACKFARBEN)
C    (1) MINIMALES X,Y,Z-TYP;      DECKEND (Z.B. LACKFARBEN)          
C    (2) GEWICHTETE SUMME K/S;     DECKEND (Z.B. LACKFARBEN)  
C    (3) B-WERT ANGLEICH B(PROBE)=B(TYP)=0.         DECKEND (Z.B.LACK)    
C    (4) B-WERT ANGLEICH B(PROBE)=B(TYP)            DECKEND (Z.B.LACK)  
C    (5) MINIMALER FARBABSTAND                      DECKEND (Z.B.LACK)
C    (6) MAXIMALER K/S-WERT                         DECKEND (Z.B.LACK)
C    (7) UNGEWICHTETE SUMME K/S;     DECKEND (Z.B. LACKFARBEN)  
C    (8) EBENE SENKRECHT ZUR FÄRBECHARACTERISTIK
C
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
c
c
C
C
C
C
      DLL_EXPORT FASDEKS
C
      CALL FASDEKZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUKS,TYKS,RKS,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
c
      RETURN
      END
c
c
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FASTRAS  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
C     Transparente Schichten(spezial)
C
C
      SUBROUTINE FASTRAS(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUEX,TYEX,REX,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NQU,NPAM,KW,NWEL,KML,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUEX(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYEX(*)
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*)::RDAB,RDAM,REX,RDUN
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR

C 
C 
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C 
C    IART
C
C 
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C
C
C 
C    (0) Y-TYP              ;      TRANSPARENT (Z.B. DRUCKFARBEN)    
C    (1) MINIMALES X,Y,Z-TYP;      TRANSPARENT (Z.B. DRUCKFARBEN)    
C    (2) GEWICHTETE SUMME EXTINKT.;TRANSPARENT (Z.B. DRUCKFARBEN)  
C    (3) B-WERT ANGLEICH B(PROBE)=B(TYP)=0. TRANSPARENT (Z.B. DRUCK)         
C    (4) B-WERT ANGLEICH B(PROBE)=B(TYP)    TRANSPARENT (Z.B. DRUCK)               
C    (5) MINIMALER FARBABSTAND              TRANSPARENT (Z.B. DRUCK)
C    (6) MAXIMALER EXTINKTIONSWERT          TRANSPARENT (Z.B. DRUCK)
C    (7) UNGEWICHTETE SUMME EXTINKT.;TRANSPARENT (Z.B. DRUCKFARBEN)  
C    (8) EBENE SENKRECHT ZUR FÄRBECHARACTERISTIK
C 
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
C
C
      DLL_EXPORT FASTRAS
      CALL FASTRAZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUEX,TYEX,REX,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      RETURN
c
c     
      END
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FASTEXS  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
C     Textil(spezial)
C
      SUBROUTINE FASTEXS(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUKS,TYKS,RKS,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NQU,NPAM,KW,NWEL,KML,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUKS(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYKS(*)
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*)::RDAB,RDAM,RKS,RDUN
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
      TYPE(TYFEH) FEHL
      
C 
C 
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C 
C    IART
C
C
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C 
C
C 
C 
C
C 
C    (0) Y-TYP              ;      TEXTIL       
C    (1) MINIMALES X,Y,Z-TYP;      TEXTIL       
C    (2) GEWICHTETE SUMME K/S;     TEXTIL
C    (3) B-WERT ANGLEICH B(PROBE)=B(TYP)=0.        TEXTIL
C    (4) B-WERT ANGLEICH B(PROBE)=B(TYP)           TEXTIL  
C    (5) MINIMALER FARBABSTAND                     TEXTIL            
C    (6) MINIMUM DER REFLEXIONSKURVE
C    (7) UNGEWICHTETE SUMME K/S;     DECKEND (Z.B. LACKFARBEN)
C    (8) EBENE SENKRECHT ZUR FÄRBECHARACTERISTIK
C 
C 
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
      DLL_EXPORT FASTEXS
      CALL FASTEXZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUKS,TYKS,RKS,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
c
      RETURN
      END
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FSTDEKS  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c     deckende Schichten (weiß-scwarz spezial)
C
      SUBROUTINE FSTDEKS(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  QUASUN,TYASUN,RSUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NWEL,KML,NQU,NPAM,KW,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUABSTR(*)
      TYPE(QURWERT) QURUNND(*),QUCRMAX(*),QUASUN(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYABSTR(*)
      TYPE(TYRWERT) TYRUNND(*),TYCRMAX(*),TYASUN(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RDUN,RABSTR
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RSUN,RUNND,RCRMAX
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      TYPE(TYFEH) FEHL
C
C 
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN
C 
C    IART
C
C
C    (0) Y-TYP              ;      DECKEND (Z.B. LACKFARBEN)
C    (1) MINIMALES X,Y,Z-TYP;      DECKEND (Z.B. LACKFARBEN)          
C    (2) GEWICHTETE SUMME K/S;     DECKEND (Z.B. LACKFARBEN)  
C    (3) B-WERT ANGLEICH B(PROBE)=B(TYP)=0.         DECKEND (Z.B.LACK)    
C    (4) B-WERT ANGLEICH B(PROBE)=B(TYP)            DECKEND (Z.B.LACK)  
C    (5) MINIMALER FARBABSTAND                      DECKEND (Z.B.LACK)
C    (6) MAXIMALER K/S-WERT ODER FÜR VORGEG. WELL.  DECKEND (Z.B.LACK)
C    (7) UNGEWICHTETE SUMME K/S;     DECKEND (Z.B. LACKFARBEN)
C    (8) EBENE SENKRECHT ZUR FÄRBECHARACTERISTIK
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
      DLL_EXPORT FSTDEKS
      CALL FSTDEKZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  QUASUN,TYASUN,RSUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      RETURN
c
      END
C
C
C
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FSTTRAS  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c     transparente Schichten(weiß-schwarz spezial)
C
      SUBROUTINE FSTTRAS(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)

c
C     Last change:  U    30 Dec 1998    2:20 pm
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NQU,NPAM,KW,NWEL,KML,IPRG,NFAECHAR,NFAEWRT
C
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUABSTR(*)
      TYPE(QURWERT) QURUNND(*),QUCRMAX(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYABSTR(*)
      TYPE(TYRWERT) TYRUNND(*),TYCRMAX(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RDUN,RABSTR
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RUNND,RCRMAX
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      TYPE(TYFEH) FEHL
C
C 
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN
C 
C    IART
C
C 
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C 
C
C 
C    (0) Y-TYP              ;      TRANSPARENT (Z.B. DRUCKFARBEN)    
C    (1) MINIMALES X,Y,Z-TYP;      TRANSPARENT (Z.B. DRUCKFARBEN)    
C    (2) GEWICHTETE SUMME EXTINKT.;TRANSPARENT (Z.B. DRUCKFARBEN)  
C    (3) B-WERT ANGLEICH B(PROBE)=B(TYP)=0. TRANSPARENT (Z.B. DRUCK)
C    (4) B-WERT ANGLEICH B(PROBE)=B(TYP)    TRANSPARENT (Z.B. DRUCK)               
C    (5) MINIMALER FARBABSTAND              TRANSPARENT (Z.B. DRUCK) 
C    (6) MAXIMALER EXTINKTIONSWERT          TRANSPARENT (Z.B. DRUCK)
C    (7) UNGEWICHTETE SUMME EXTINKT.;TRANSPARENT (Z.B. DRUCKFARBEN)
C    (8) EBENE SENKRECHT ZUR FÄRBECHARACTERISTIK
C
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
C
C
C
      DLL_EXPORT FSTTRAS
C
      CALL FSTTRAZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      RETURN
c
c
      END
C
C
C
C     Deckvermögen
C
C
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  DEKTRAS  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
C     transparente Schichten Deckvermögen (spezial)
C
      SUBROUTINE DEKTRAS(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)

      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NWEL,KML,NQU,NPAM,KW,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),
     &                QUABSTR(*),QURUNND(*),QUCRMAX(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),
     &                TYABSTR(*),TYRUNND(*),TYCRMAX(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RUNND,
     &                                      RCRMAX,RABSTR,RDUN
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*)::WERT
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
C
C 
C 
C     BERECHNUNG DES DECKVERMOEGENS FUER TRANSPARENTE SCHICHTEN                   
C 
C
C 
C 
C 
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
C
C
C
      DLL_EXPORT DEKTRAS
C
      CALL DEKTRAZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      RETURN
c
      END
C

c
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  DEKDEK  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
C     deckende Schichten (Quasideckvermögen spezial)
      SUBROUTINE DEKDEKS(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  QUASUN,TYASUN,RASUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)

      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NWEL,KML,NQU,NPAM,KW,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),
     &                QUABSTR(*),QURUNND(*),QUCRMAX(*),QUASUN(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),
     &                TYABSTR(*),TYRUNND(*),TYCRMAX(*),TYASUN(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RUNND,
     &                                      RCRMAX,RABSTR,RDUN,RASUN
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*)::WERT
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
C
C 
C 
C     BERECHNUNG DES DECKVERMOEGENS FUER TRANSPARENTE SCHICHTEN                   
C 
C
C
C 
C 
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
C
C
C
      DLL_EXPORT DEKDEKS
C
      CALL DEKDEKZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  QUASUN,TYASUN,RASUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      RETURN
c
c     
      END
C******************************************************************************
C******************************************************************************
      SUBROUTINE LCHSTBW(ILCHL,LCHSTL,FEHL)
      USE MODSPZL
      USE MOTFEHL
C
C     zur L*-Berechnung für vorg.a*,b*,B-Wert
C
      IMPLICIT REAL(KIND=8)(A-H,O-Z)
      REAL(KIND=8) :: WRT,WRTMIN,WRTMAX
      REAL(KIND=8) :: FORB,FOMAX,FOMIN,FOAKT,DELBW
      REAL(KIND=4),DIMENSION(3) :: LCHSTL
      INTEGER(KIND=4) :: I,ILCHL
      EXTERNAL FORB
      TYPE(TYFEH) FEHL
      DATA DELBW/1.E-3/
      DLL_EXPORT LCHSTBW
      CALL FEHINI()
      IER=0
      NLI=1
C
C     ILCH=1 L* aus C*,h  berechnen
C     ILCH=2 C* aus L*,h  berechnen
C     ILCH=3 h  aus C*,L* berechnen
C
      ILCH=ILCHL
      DO I=1,3
        LCHST(I)=LCHSTL(I)
      END DO
      SELECT CASE (ILCH)
        CASE (1)
          WRTMIN=0.0
          WRTMAX=100.0
          WRT=50.0
        CASE (2)
          WRTMIN=0.0
          WRTMAX=WRTMIN
          FOMIN=FORB(WRTMIN)
          IF(ABS(FOMIN).LT.DELBW) THEN
            LCHSTL(ILCH)=WRTMIN
            RETURN
          ENDIF
          DO WHILE (FOMIN*FORB(WRTMAX).GT.0.0)
            WRTMAX=WRTMAX+10
          END DO
        CASE (3)
          WRTMIN=0.0
          WRTMAX=360.0
          WRT=180.0
        CASE DEFAULT
      END SELECT
      CALL INTVAL(WRT,WRTMIN,WRTMAX,FORB,IER)
      IF(ABS(FORB(WRT)).LT.DELBW) THEN
        IER=0
      ENDIF
      IF(IFEHL(IER).NE.0) THEN
        GOTO 900
      ENDIF
      LCHSTL(ILCH)=WRT
  900 CALL GETFEH(FEHL)
      RETURN
      END
C
C
C
      REAL(KIND=8) FUNCTION FORB(WRT)
      USE MODFUNC,ONLY:KBWT,JABST
      USE MODSPZL

      IMPLICIT REAL(KIND=8)(A-H,O-Z)
      REAL(KIND=8) :: WRT,B
      REAL(KIND=8),DIMENSION(3) :: XYZ
      LCHST(ILCH)=WRT
C
C     X,Y,Z berechnen
C

      CALL XYZLCHAL(JABST,XYZ,LCHST,NLI,*900)
C
C     B-Wert berechnen für KBWT
C
      CALL BWERT(XYZ,KBWT,B,NLI)
      FORB=B
      RETURN
 900  FORB=1000.0
      RETURN
      END
C
C******************************************************************************
C******************************************************************************
      SUBROUTINE YSWSTBW(ILCHL,LCHSTL,FEHL)
      USE MODSPZL
      USE MOTFEHL
C
C     zur L*-Berechnung für vorg.a*,b*,B-Wert
C
      IMPLICIT REAL(KIND=8)(A-H,O-Z)
      REAL(KIND=8) :: WRT,WRTMIN,WRTMAX
      REAL(KIND=8) :: FORB,FOMAX,FOMIN,FOAKT,DELBW
      REAL(KIND=4),DIMENSION(3) :: LCHSTL
      INTEGER(KIND=4) :: I,ILCHL
      EXTERNAL FORC
      TYPE(TYFEH) FEHL
      DATA DELBW/1.E-3/
      DLL_EXPORT YSWSTBW
      CALL FEHINI()
      IER=0
      NLI=1
C
C     Y (Tristimuluswert)
C     S=sqrt((x-x0)**2+(y-y0)**2) (Helmholzkoordinate)
C     W=Atan(y-y0,x-x0) Helmholzwinkel
C     x0,y0 Unbuntpunkt auf der Schuhsohle
C
C
C     ILCH=1 Y aus S,W  berechnen
C     ILCH=2 S aus Y,W  berechnen
C     ILCH=3 W aus Y,S berechnen
C
      ILCH=ILCHL
      DO I=1,3
        LCHST(I)=LCHSTL(I)
      END DO
      SELECT CASE (ILCH)
        CASE (1)
          WRTMIN=0.0
          WRTMAX=100.0
          WRT=50.0
        CASE (2)
          WRTMIN=0.0
          WRTMAX=WRTMIN
          FOMIN=FORC(WRTMIN)
          IF(ABS(FOMIN).LT.DELBW) THEN
            LCHSTL(ILCH)=WRTMIN
            RETURN
          ENDIF
          DO WHILE (FOMIN*FORC(WRTMAX).GT.0.0)
            WRTMAX=WRTMAX+1
          END DO
        CASE (3)
          WRTMIN=0.0
          WRTMAX=360.0
          WRT=180.0
        CASE DEFAULT
      END SELECT
      CALL INTVAL(WRT,WRTMIN,WRTMAX,FORC,IER)
      IF(ABS(FORB(WRT)).LT.DELBW) THEN
        IER=0
      ENDIF
      IF(IFEHL(IER).NE.0) THEN
        GOTO 900
      ENDIF
      LCHSTL(ILCH)=WRT
  900 CALL GETFEH(FEHL)
      RETURN
      END
C
C
C
      REAL(KIND=8) FUNCTION FORC(WRT)
      USE MODFUNC,ONLY:KBWT,JABST
      USE MODSPZL

      IMPLICIT REAL(KIND=8)(A-H,O-Z)
      REAL(KIND=8) :: WRT,B,SUU,WRAD,PI180
      REAL(KIND=8),DIMENSION(3) :: XYZ
      LCHST(ILCH)=WRT
      DATA PI180/0.0174532925/
      DATA DELL/1.0D-9/
C
C     X,Y,Z berechnen
C
C
C 
      SUU=0.
      DO I=1,3
      SUU=SUU+FAKT(I,NLI)
      ENDDO
      WRAD=LCHST(3)*PI180
      S=LCHST(2)
      X=S*COS(WRAD)
      Y=S*SIN(WRAD)
      XK=X+FAKT(1,NLI)/SUU
      YK=Y+FAKT(2,NLI)/SUU
      XYZ(2)=LCHST(1)
      XYZ(1)=YK*XYZ(2)/(YK+DELL)
      XYZ(3)=(1.0-XK-YK)*XYZ(2)/(YK+DELL)
C
C     B-Wert berechnen für KBWT
C
      CALL BWERT(XYZ,KBWT,B,NLI)
      FORC=B
      RETURN
 900  FORC=1000.0
      RETURN
      END
C
C
C******************************************************************************
C
C
C     Last change:  UFO  27 Jan 105   12:03 pm
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FASDEK  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c

      SUBROUTINE FASDEK(KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUKS,TYKS,RKS,QUREDUN,TYREDUN,RDUN,
     &                  NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NFAECHAR=1,NFAEWRT=1,NRWRT=1)
      INTEGER*4 NQU,NPAM,KW,NWEL,KML,KFIT,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUKS(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYKS(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*)::RDAM,RDAB,RKS,RDUN
      REAL(KIND=8),DIMENSION(64,*)::WERT
      TYPE(TYPARAM) PARAM(*)
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,NRWRT) ::FAECHAR
      DATA KFIT/0/
C
C
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C 
C    IART
C
C 
C    (0) Y-TYP              ;      DECKEND (Z.B. LACKFARBEN)
C    (1) MINIMALES X,Y,Z-TYP;      DECKEND (Z.B. LACKFARBEN)          
C    (2) GEWICHTETE SUMME K/S;     DECKEND (Z.B. LACKFARBEN)  
C    (3) B-WERT ANGLEICH B(PROBE)=B(TYP)=0.         DECKEND (Z.B.LACK)    
C    (4) B-WERT ANGLEICH B(PROBE)=B(TYP)            DECKEND (Z.B.LACK)  
C    (5) MINIMALER FARBABSTAND                      DECKEND (Z.B.LACK)
C    (6) MAXIMALER K/S-WERT                         DECKEND (Z.B.LACK)
C    (7) UNGEWICHTETE SUMME K/S;     DECKEND (Z.B. LACKFARBEN)
C    (8) EBENE SENKRECHT ZUR FÄRBECHARACTERISTIK
C
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
c
c
C
C
C
C
      DLL_EXPORT FASDEK
C
      CALL FASDEKZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUKS,TYKS,RKS,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)

      RETURN
      END
c
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FASTRA  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
      SUBROUTINE FASTRA(KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUEX,TYEX,REX,QUREDUN,TYREDUN,RDUN,
     &                  NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NFAECHAR=1,NFAEWRT=1,NRWRT=1)
      INTEGER*4 KFIT,NQU,NPAM,KW,NWEL,KML,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUEX(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYEX(*)
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*)::RDAB,RDAM,REX,RDUN
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,NRWRT) ::FAECHAR
      DATA KFIT/0/

C 
C 
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C 
C    IART
C
C 
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C
C
C 
C    (0) Y-TYP              ;      TRANSPARENT (Z.B. DRUCKFARBEN)
C    (1) MINIMALES X,Y,Z-TYP;      TRANSPARENT (Z.B. DRUCKFARBEN)    
C    (2) GEWICHTETE SUMME EXTINKT.;TRANSPARENT (Z.B. DRUCKFARBEN)
C    (3) B-WERT ANGLEICH B(PROBE)=B(TYP)=0. TRANSPARENT (Z.B. DRUCK)         
C    (4) B-WERT ANGLEICH B(PROBE)=B(TYP)    TRANSPARENT (Z.B. DRUCK)               
C    (5) MINIMALER FARBABSTAND              TRANSPARENT (Z.B. DRUCK)
C    (6) MAXIMALER EXTINKTIONSWERT          TRANSPARENT (Z.B. DRUCK)
C    (7) UNGEWICHTETE SUMME EXTINKT.;TRANSPARENT (Z.B. DRUCKFARBEN)
C    (8) EBENE SENKRECHT ZUR FÄRBECHARACTERISTIK
C 
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
C
C
      DLL_EXPORT FASTRA

      CALL FASTRAZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUEX,TYEX,REX,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      RETURN
c
c     
      END

C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FASTEX  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
      SUBROUTINE FASTEX(KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUKS,TYKS,RKS,QUREDUN,TYREDUN,RDUN,
     &                  NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NFAECHAR=1,NFAEWRT=1,NRWRT=1)
      INTEGER*4 KFIT,NQU,NPAM,KW,NWEL,KML,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUKS(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYKS(*)
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*)::RDAB,RDAM,RKS,RDUN
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,NRWRT) ::FAECHAR
      DATA KFIT/0/
C 
C 
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C 
C    IART
C
C
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C 
C
C 
C 
C
C 
C    (0) Y-TYP              ;      TEXTIL       
C    (1) MINIMALES X,Y,Z-TYP;      TEXTIL
C    (2) GEWICHTETE SUMME K/S;     TEXTIL
C    (3) B-WERT ANGLEICH B(PROBE)=B(TYP)=0.        TEXTIL
C    (4) B-WERT ANGLEICH B(PROBE)=B(TYP)           TEXTIL  
C    (5) MINIMALER FARBABSTAND                     TEXTIL
C    (6) MINIMUM DER REFLEXIONSKURVE
C    (7) UNGEWICHTETE SUMME K/S;     DECKEND (Z.B. LACKFARBEN)
C    (8) EBENE SENKRECHT ZUR FÄRBECHARACTERISTIK
C 
C 
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
      DLL_EXPORT FASTEX
c      OPEN(27,FILE='OUTTEX.TXT')

      CALL FASTEXZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUKS,TYKS,RKS,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
c      CLOSE(27)
      RETURN
      END
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FSTDEK  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
      SUBROUTINE FSTDEK(KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  QUASUN,TYASUN,RSUN,
     &                  NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NFAECHAR=1,NFAEWRT=1,NRWRT=1)
      INTEGER*4 KFIT,NWEL,KML,NQU,NPAM,KW,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUABSTR(*)
      TYPE(QURWERT) QURUNND(*),QUCRMAX(*),QUASUN(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYABSTR(*)
      TYPE(TYRWERT) TYRUNND(*),TYCRMAX(*),TYASUN(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RDUN,RABSTR
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RSUN,RUNND,RCRMAX
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,NRWRT) ::FAECHAR
      DATA KFIT/0/

C
C 
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN
C 
C    IART
C
C 
C    (0) Y-TYP              ;      DECKEND (Z.B. LACKFARBEN)
C    (1) MINIMALES X,Y,Z-TYP;      DECKEND (Z.B. LACKFARBEN)
C    (2) GEWICHTETE SUMME K/S;     DECKEND (Z.B. LACKFARBEN)  
C    (3) B-WERT ANGLEICH B(PROBE)=B(TYP)=0.         DECKEND (Z.B.LACK)    
C    (4) B-WERT ANGLEICH B(PROBE)=B(TYP)            DECKEND (Z.B.LACK)  
C    (5) MINIMALER FARBABSTAND                      DECKEND (Z.B.LACK)
C    (6) MAXIMALER K/S-WERT ODER FÜR VORGEG. WELL.  DECKEND (Z.B.LACK)
C    (7) UNGEWICHTETE SUMME K/S;     DECKEND (Z.B. LACKFARBEN)
C    (8) EBENE SENKRECHT ZUR FÄRBECHARACTERISTIK
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
      DLL_EXPORT FSTDEK
      CALL FSTDEKZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  QUASUN,TYASUN,RSUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      RETURN
c
      END

C
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FSTTRA  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
      SUBROUTINE FSTTRA(KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  NPAM,PARAM,WERT,FEHL)

c
c
C     Last change:  U    30 Dec 1998    2:20 pm
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NFAECHAR=1,NFAEWRT=1,NRWRT=1)
      INTEGER*4 KFIT,NQU,NPAM,KW,NWEL,KML,IPRG,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUABSTR(*)
      TYPE(QURWERT) QURUNND(*),QUCRMAX(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYABSTR(*)
      TYPE(TYRWERT) TYRUNND(*),TYCRMAX(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RDUN,RABSTR
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RUNND,RCRMAX
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,NRWRT) ::FAECHAR
      DATA KFIT/0/

C
C
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN
C 
C    IART
C
C 
C     BERECHNUNG VON FARBSTAERKEN NACH VERSCHIEDENEN VERFAHREN                   
C 
C
C 
C    (0) Y-TYP              ;      TRANSPARENT (Z.B. DRUCKFARBEN)    
C    (1) MINIMALES X,Y,Z-TYP;      TRANSPARENT (Z.B. DRUCKFARBEN)
C    (2) GEWICHTETE SUMME EXTINKT.;TRANSPARENT (Z.B. DRUCKFARBEN)  
C    (3) B-WERT ANGLEICH B(PROBE)=B(TYP)=0. TRANSPARENT (Z.B. DRUCK)
C    (4) B-WERT ANGLEICH B(PROBE)=B(TYP)    TRANSPARENT (Z.B. DRUCK)
C    (5) MINIMALER FARBABSTAND              TRANSPARENT (Z.B. DRUCK) 
C    (6) MAXIMALER EXTINKTIONSWERT          TRANSPARENT (Z.B. DRUCK)
C    (7) UNGEWICHTETE SUMME EXTINKT.;TRANSPARENT (Z.B. DRUCKFARBEN)  
C    (8) EBENE SENKRECHT ZUR FÄRBECHARACTERISTIK
C
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
C
C
C
      DLL_EXPORT FSTTRA
C
      CALL FSTTRAZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      RETURN
c
c
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
C*****************************************  DEKDEK  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
      SUBROUTINE DEKDEK(KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  QUASUN,TYASUN,RASUN,
     &                  NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NFAECHAR=1,NFAEWRT=1,NRWRT=1)
      INTEGER*4 KFIT,NWEL,KML,NQU,NPAM,KW,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),
     &                QUABSTR(*),QURUNND(*),QUCRMAX(*),QUASUN(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),
     &                TYABSTR(*),TYRUNND(*),TYCRMAX(*),TYASUN(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RUNND,
     &                                      RCRMAX,RABSTR,RDUN,RASUN
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*)::WERT
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,NRWRT) ::FAECHAR
      DATA KFIT/0/
C
C 
C 
C     BERECHNUNG DES DECKVERMOEGENS FUER DECKENDE SCHICHTEN                   
C 
C
C
C 
C
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
C
C
C
      DLL_EXPORT DEKDEK
C
      CALL DEKDEKZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  QUASUN,TYASUN,RASUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      RETURN
c
c     
      END
C
c
C
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  DEKTRA  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
      SUBROUTINE DEKTRA(KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  NPAM,PARAM,WERT,FEHL)


      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NFAECHAR=1,NFAEWRT=1,NRWRT=1)
      INTEGER*4 KFIT,NWEL,KML,NQU,NPAM,KW,NFAECHAR,NFAEWRT
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),
     &                QUABSTR(*),QURUNND(*),QUCRMAX(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),
     &                TYABSTR(*),TYRUNND(*),TYCRMAX(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RUNND,
     &                                      RCRMAX,RABSTR,RDUN
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*)::WERT
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,NRWRT) ::FAECHAR
      DATA KFIT/0/
C
C
C 
C     BERECHNUNG DES DECKVERMOEGENS FUER TRANSPARENTE SCHICHTEN                   
C 
C
C 
C 
C 
C 
C
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
C
C
C
      DLL_EXPORT DEKTRA
C
c      OPEN(27,FILE='TESTDEK.TXT')

      CALL DEKTRAZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
c      CLOSE(27)
      RETURN
c
      END
C

c
C
C
C
C
C
C
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
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  UNENDL  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c

c
c
c
c
C
C
C
C
C
CUNENDL
C
C
      SUBROUTINE UNENDL(KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QURUNND,TYRUNND,RUNND,
     &                  QUREDUN,TYREDUN,RDUN,FEHL)

      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 NQU
C 
C 
C 
C     BERECHNUNG DES DECKVERMOEGENS FUER TRANSPARENTE SCHICHTEN                   
C 
C
C 
C 
C 
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QURUNND(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYRUNND(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDUN,RUNND
      TYPE(SRWERT) RWERP(2)
      REAL(KIND=8),TARGET,ALLOCATABLE,DIMENSION(:) ::RZ1,RZ2
      TYPE(TYFEH) FEHL
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: RU
C
C
C
      DLL_EXPORT UNENDL
C
      ALLOCATE(RZ1(NWEL),RZ2(NWEL),RU(NWEL,2),STAT=IER)
      IF(IFEHL(IER).NE.0) THEN
         GOTO 900
      ENDIf
      RWERP(1)%R=>RZ1
      RWERP(2)%R=>RZ2
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
c
C     Nummer des Winkels
C
      KWC=KW
      NWE=NWS()
C
C
c
c
      DO KU=1,2
          IF(TYREDUN(KU)%RETR.EQ.1) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
          ENDIF
          DO I=1,NWE
             RHI=RDUN(I,KW,KU)
             RU(I,KU)=RUKORR(RHI,KW)
          ENDDO
       ENDDO
c
C
C
c
C
C
C
CC
C
C
cc
c
c
c
      DO L=1,NQU,2
C
C
C
C
C
            IF(TYREDAM(L)%RETR.EQ.1) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
            ENDIF
c            CALL GETRWE(KW,1,NWEL,TYREDAM(L),QUREDAM(L),RDAM(1,KW,L),
c     &                  RWERP(1),IER)
            CALL GETKPF(KW,TYREDAM(L),QUREDAM(L),RWERP(1),IER)
            CALL GETRWRT(NWEL,RDAM(1,KW,L),RWERP(1)%R)

            IF(IFEHL(IER).NE.0) GOTO 900
            IF(RWERP(1)%CART(4:4).NE.'W') THEN
               IER=4113
               IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF
            LL=L+1
            IF(TYREDAM(LL)%RETR.EQ.1) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
            ENDIF
c            CALL GETRWE(KW,1,NWEL,TYREDAM(LL),QUREDAM(LL),RDAM(1,KW,LL),
c     &                  RWERP(2),IER)
            CALL GETKPF(KW,TYREDAM(LL),QUREDAM(LL),RWERP(2),IER)
            CALL GETRWRT(NWEL,RDAM(1,KW,LL),RWERP(2)%R)
            IF(IFEHL(IER).NE.0) GOTO 900
            IF(RWERP(2)%CART(4:4).NE.'S') THEN
               IER=4113
               IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF
            IF(RWERP(1)%CART(1:2).EQ.'@T') THEN
              ITP=1
            ELSEIF (RWERP(1)%CART(1:2).EQ.'@P') THEN
              ITP=2
            ELSE
              CYCLE
            ENDIF
C
C
C
            LJ=L/2+1
            TYRUNND(LJ)=TYREDAM(L)
            QURUNND(LJ)=QUREDAM(L)
            QURUNND(LJ)%CART(3:3)='U'
C
            DO I=1,NWE
             RHW=RSAUND(RWERP(1)%R(I),KW)
             UHW=RSAUND(RU(I,1),KW)
             RHS=RSAUND(RWERP(2)%R(I),KW)
             UHS=RSAUND(RU(I,2),KW)
             RUE=RUNEND(RHW,UHW,RHS,UHS)
             RUNND(I,KW,LJ)=RUSAUN(RUE,KW)
            END DO
C
      ENDDO
c
 900  CONTINUE
      CALL GETFEH(FEHL)
      DEALLOCATE(RU,RZ1,RZ2,STAT=IER)
      RETURN
      END
C
C
C
C
C
C
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  DEKTRA  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
      SUBROUTINE RETRMIS(KU,ITR,KW,NWEL,KML,NQU,QUREDUN,TYREDUN,RDUN,
     &                  QUREDAX,TYREDAX,RDAB,
     &                  QUABSTR,TYABSTR,RABSTR,FEHL)

      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KU,ITR,NWEL,KML,NQU,NPAM,KW

C 
C 
C 
C     BERECHNUNG DES DECKVERMOEGENS FUER TRANSPARENTE SCHICHTEN                   
C 
C
C 
C
      DLL_EXPORT RETRMIS
C

C
C     KU = 1 ungerade Strukturen (z.B. TYREDAM(1),TYREDAM(3) usw)
C     KU = 2   gerade Strukturen (z.B. TYREDAM(2),TYREDAM(4) usw.)
C     ITR=0  Remission;  ITR=1 Transmission
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
      TYPE(QURWERT) QUREDAX(*),QUABSTR(*),QUREDUN(*)
      TYPE(TYRWERT) TYREDAX(*),TYABSTR(*),TYREDUN(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAB,RABSTR,RDUN
      REAL(KIND=8),DIMENSION(NWEL) :: GVV,RUME,RUBE,RPP
      REAL(KIND=8) :: DIK
      REAL(KIND=8) :: DEWESC
      TYPE(SRWERT) RWERP
      REAL(KIND=8),TARGET,ALLOCATABLE,DIMENSION(:) ::RZ1
      TYPE(TYFEH) FEHL
      CHARACTER*6 BLAN
      DATA BLAN/'      '/
      DATA DIK/1.D0/ 
C
      IER=0
      CALL FEHINI()
      IPRG=105
C
C
C
      ALLOCATE(RZ1(NWEL),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      RWERP%R=>RZ1
C
C
      CALL SRT000(RWERP)

C
C
C
C
C
C
C

      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF

C
C
C
C
C
C
      CALL TSTIPRG(IPRG,IER)
      IF(IFEHL(IER).NE.0) GOTO 900

      CALL QUAKOR('T')
c
c
      DEWESC=0.0


c
c
C
C
C
C
C
C     Berechnung von korrigierten Reflexionswerten fuer Untergruende
C
C
C
C
C     Nummer des Winkels
C
      KWC=KW
      IF(KW.LE.0.OR.KW.GT.KML) THEN
        IF(IFEHL(4165).NE.0) GOTO 900
      ENDIF

C
C
C        Transmission
C
       DO I=1,NWS()
          RUME(I)=RDUN(I,KW,KU)
          RUBE(I)=TRKORR(RUME(I),KW,ITR)
       ENDDO
C
C
C
C
cc
c
c
c
c
      LP=-1
      LL=0
      DO L=KU,NQU,2
           LP=LP+2
           LL=LL+1
C
C
C       Gleiche Winkel
C
C
C

            ITP=0
            CALL GETKPF(KW,TYREDAX(L),QUREDAX(L),RWERP,IER)
C
C           Transmission
C
            RWERP%RETR=ITR
C
C
C                 MENGE*PROZ*0.01
C          DICKE=---------------------------_ 
C                 FLAECHE
C
           RWERP%CAMP(0)=RWERP%CAMP(1)*0.01*RWERP%CAMP(3)/
     &          (RWERP%CAMP(2)+TINY(1.))
C
C
C
           DIK=RWERP%CAMP(0)
           IF(DIK.EQ.0.0D0) GOTO 98
C
C
C
C             Remission/Transmission aus optischen Daten berechnen
C
C
C
C
C

             DO I=1,NWS()
              A(I)=RABSTR(I,KW,LP)
              S(I)=RABSTR(I,KW,LP+1)
             ENDDO
             CALL RWWRTTR(ITR,DIK,NWEL,RPP,RUBE,A,S,GV,IER)
             DO I=1,NWS()
                RDAB(I,KW,LL)=RPP(I)
             ENDDO
      ENDDO
C
 900  CONTINUE
      CALL GETFEH(FEHL)
      DEALLOCATE(RZ1,STAT=IER)
      RETURN
  98  IER=4021
      IF(IFEHL(IER).NE.0) GOTO 900
      GOTO 900
c
c     
      END
C
C
C
C
C
C

