C     Last change: KU 01.02.2021 11:37:58
C******************************************************
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  REZMEN  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************

      SUBROUTINE REZMEN(JABSTL,FUL,WLCHL,DEHLFL,DEGUTL,DEOKL,
     &                  FDEL,GDEL,GGEL,FTOL,ATOL,DELL,SPRL,
     &                  GXGL,GXDL, DTOL,GTOL,
     &                  NPXL,EPS,MINREZ,MAXREZ,KWD,ITML,AKKL,
     &                  IGXL,KWEL,KGXL,ICHIL,ISORL,GKIDL,PARMERK,FEHL)
      USE MODILLU,ONLY:NLZ,FA
      USE MODFEHL
      USE MODWINK,ONLY:KM
      USE MODMERZ
      IMPLICIT NONE
C
C
      INTEGER(KIND=4) :: I,IER,IFEHL
      TYPE(TYFEH) :: FEHL
      INTEGER(KIND=4)::MINREZ,MAXREZ,ITML,IGXL,KWEL,
     &                 JABSTL,KGXL,ICHIL,ISORL,KWD,NPXL,GKIDL
      REAL(KIND=4) :: DEHLFL,DEGUTL,DEOKL,FDEL,GDEL,GGEL,AKKL,
     &                FTOL,ATOL,DELL,SPRL,GXGL,GXDL,DTOL,GTOL,
     &                TLL,FUL(*),WLCHL(3),SFU,EPS,SFA
      LOGICAL(KIND=4) ::  LLOPT
      REAL(KIND=4) PARMERK(*)
      DATA TLL/1.E-5/
C 
      DLL_EXPORT REZMEN

C
      IER=0
      CALL FEHINI()
c
C
C
C     Parameter für Merkmale nach MODFAKT speichern
C
      CALL GETPAR(0,PARMERK,IER)
      IF(IFEHL(IER).NE.0) GOTO 900

C
      TOLM=TLL
C
C 
C 
C***************************************
C     MENUE-PARAMETER
C***************************************
C 
C
C      
C
C 
C 
C
C     MESSUNG UEBER WEISSEM UND/ODER SCHWARZEM UNTERGRUND 
C     GEWICHTE
C 
C
C
C 
      SFU=0.
      DO I=1,UBOUND(FU,1)
        FU(I)=FUL(I)
        SFU=SFU+FU(I)
      END DO
      IF(ABS(SFU).LT.TLL) THEN
         IER=4042
         GOTO 900
      ENDIF 
C   
C
C     DIN6174 (0) oder DIN6176(1)
C
      JABST=JABSTL
C
C
C
C     ANZAHL FARB-/BINDEMITTEL PRO REZEPT
C
      KMI= MINREZ
      KMA= MAXREZ
C
C
C     GRENZE FUER FARBABSTAND BEI HILFSKORREKTUREN
C
      DEHLF= DEHLFL
C
C
C
C     Anzahl Grunddaten
C
      NPXMEN=NPXL
C
C 
C
C     GKID
C
      GKIDMEN=GKIDL
C
C     GRENZE FUER FARBABSTAND BEI SORTIERUNG           
C
      DEGUT= DEGUTL
C
C
C
C
C     GRENZE FUER FARBABSTAND MESSUNG-RECHNUNG z.B. BEI FALSCHER EINGABE
C
      DEOK= DEOKL
C
C     ART der Mittelung für Korrekturberechnung
C
C
      AKK=AKKL
C
C     GEWICHTE FUER L* C* H*
C
C
      DO I=1,3
         WLCH(I)= WLCHL(I)
      ENDDO
C
C
C      JN ermitteln
C
C
      JN=NLZ
      SFA=0.0
      DO I=1,NLZ
         SFA=SFA+FA(I)
      ENDDO
      IF(SFA.EQ.0.0) THEN
         IER=4029
         GOTO 900
      ENDIF
C
C    
C
C
C     KONTRASTFARBABSTAND BERUECKSICHTIGEN 0 = NEIN; 1 = JA 
C
      KGX=KGXL
C
      IF(KGX.EQ.1.AND..NOT.LLOPT()) THEN
         IER=4095
         GOTO 900
      ENDIF
C
C
C     WINKEL FUER FARBABSTAND DTO (0 = MITTELWERT UEBER ALLE WINKEL) (Rezeptrechnung (KWe))
      KWE=KWEL
      IF(KWE.GT.KM) THEN
        KWE=1
      ENDIF
      
C
C    
C     WINKEL FUER FARBABSTAND DECKVERMÖGEN
C
      KWO=KWD
      IF(KWO.GT.KM) THEN
        KWO=1
      ENDIF
      
C
C     FARBABSTAND FUER DECKVERMOEGEN
C 
      FDE= FDEL
C
C     GEWICHT FUER DECKVERMOEGEN
C 
      GDE= GDEL
C

C 
C     GEWICHT FUER MINIMALE ZUWAAGE UND ANDERE MINIMIERUNGSFUNKTIONEN
C     S. MINFUN UND MINMAT
C     (s. auch Gewichte FARBM.BEL)
C
C
      GGE= GGEL
      GGEM=GGEL
C
C 
C     ITERATIONSSCHRITTE
C 
      ITM=ITML
C
C     TOL% FUER FARBABSTAND 
C
      FTO= FTOL
C
C     REL. MENGENAENDERUNG
C 
      ATO= ATOL
C
C
C     GENAUIGKEITSPARAMETER
C 
      DEL= DELL
C
C
C     GEWICHT FÜR MINIMIERUNG (FAKTOR FÜR SPR)
C 
      SPR= SPRL
C
C
C     GEWICHT FÜR MINIMIERUNG BEI ZUWAAGEMINIMIERUNG
C
      GXG=GXGL
C
C
C     NICHT VERWENDET
C
C
C
      GXD=GXDL
C
C     ART DER KORREKTURRECHNUNG
C
C
C     BEDEUTUNG VON IGX
C     IGX = 0    BERECHNUNG DER KORREKTUR FUER KLEINST MOEGLICHEN FARBABSTAND
C     IGX = 1    BERECHNUNG DER KORREKTUR FUER KLEINE BINDEMITTELMENGEN
C     IGX = 2    BERECHNUNG DER KORREKTUR FUER KLEINE FARBMITTELMENGEN
C     IGX = 3    BERECHNUNG DER KORREKTUR FUER KLEINST MOEGLICHEN FARBABSTAND
C                ZUWAAGE AUF AZU Prozent LIMITIERT
C     IGX = 4    BERECHNUNG DER KORREKTUR FUER KLEINSTMOEGLICHEN FARBABSTAND
C                DIFFERENZ DER BATCHVOLUMINA MOEGLICHST KLEIN (GEWICHT GGE)
C     IGX = 5    BERECHNUNG DER KORREKTUR SO, DASS FARBABSTAND = DTO
C                UND MOEGLICHST KLEINE GESAMTZUWAAGE BZGL. BATCHVOLUMINA
C     IGX = 6    BERECHNUNG DER KORREKTUR SO, DASS FARBABSTAND = DTO
C                UND MOEGLICHST KLEINE ABWEICHUNG VON ALTEN BATCHVOLUMINA
C                (GEWICHT=GTO)
C
C
      IGX=IGXL
C
C
C     FARBABSTAND FUER KLEINE AENDERUNGEN
C
C
C
      DTO= DTOL
C
C     GEWICHT FUER FARBABSTAND FUER KLEINE AENDERUNGEN
C     
      GTO= GTOL
C
C     WINKEL FUER FARBABSTAND KLEINE AENDERUNGEN (OBSOLET)
C
      KWM= 1
C
C
C
C     AUSNULLEN FUER REZEPTBERECHNUNG
C
C
C     ART DER KORREKTURBERECHNUNG FUER HILFKORREKTUREN
C     0   =   KEINE BESONDERE BERECHNUNG
C     1   =   MITTELWERT DER DIFFERENZEN FUER R0
C     2   =   LINEARE INTERPOLATION
C     3   =   KORREKTURMATRIX RHLF BERECHNEN
C
      ICHI= ICHIL
C
C     Art der Sortierung (z.B. nach Metamerie)
C
      ISOR=ISORL
C
C     OHNE BEDEUTUNG
C
C     KBPROZ=MENALL%BPROZ
C
C
C     Kleinst mögliche Menge
C
      EPMNG=EPS

C
 900  IF(IFEHL(IER).NE.0) THEN
          CALL GETFEH(FEHL)
      ENDIF
      RETURN

      END

c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  REZGRU  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
      SUBROUTINE REZGRU(MNF,MNGREI,OPL,KTL,ICHL,
     &                  PREIS,FST,
     &                  NWE,KM,NPQG,NSTL,CSTL,GRUND,FEHL)
      USE MODFEHL
      USE MODABST
      USE MODMERZ
      USE MODFARB
      USE MOTFEHL
      USE MODRWRZ
      USE MODGREN
      USE MODGKWR,ONLY:CDE
      IMPLICIT NONE
      INTERFACE
      INTEGER(KIND=4) FUNCTION DIMIER(NWEL,KML,NLZL,NPQGL,MNF,NUA)
      INTEGER(KIND=4) :: NWEL
      INTEGER(KIND=4),OPTIONAL :: KML,NLZL,NPQGL,MNF,NUA
      END FUNCTION
      END INTERFACE
C 
C
      TYPE(TYFEH) FEHL
      INTEGER(KIND=4) ::I,J,L,KW,MNF,NWE,KM,IER,
     &                  NOPNPX,NPHLF,NPNPS,IFEHL,NPQG
      REAL(KIND=4),DIMENSION(NWE,KM,NPQG,*) :: GRUND
      INTEGER(KIND=4),DIMENSION(*)::ICHL,NSTL
      REAL(KIND=4),DIMENSION(*) ::PREIS,FST
      REAL(KIND=4),DIMENSION(KM,NPQG,*)::CSTL
      INTEGER(KIND=1),DIMENSION(*) ::KTL
      REAL(KIND=4):: MNGREI
      INTEGER(KIND=1) :: OPL(*)
      INTEGER(KIND=4) :: CNZDEP
C
      DLL_EXPORT REZGRU
C
C
      IER=0
      CALL FEHINI()
C
C
      IER=DIMIER(NWE,KML=KM,NPQGL=NPQG,MNF=MNF)
      IF(IER.NE.0) GOTO 900
      NF=MNF
      NPX=NOPNPX()
      IF(NPXMEN.NE.NPX) THEN
         IER=4107
         GOTO 900
      ENDIF
C
      IF(NF.LE.0) THEN
         IER=4063
         GOTO 900
      ENDIF
C
      IF(IKOR.EQ.0) THEN
        IF(NF.GT.UBOUND(LK,1)) THEN
           IER=4066
           GOTO 900
        ENDIF
      ELSEIF(IKOR.EQ.1) THEN
         IF(NF.GT.UBOUND(LK,1)) THEN
            IER=4109
            GOTO 900
         ENDIF
      ENDIF
c      IF(NF.NE.SOZPT%KF) THEN
c         IER=4070
c         GOTO 900
c      ENDIF


      ANORM=MAX(MNGREI,1.D0)
C
      DO J=1,NF
        LK(J)=J
        PRE(J)=0.
        KTO(J)=' '
        OP(J)=' '
      END DO
C
C

      DO J=1,NF
C
C
C         REINE FARB-/BINDEMITTELMENGEN
C
          PRE(J)= PREIS(J)
          KTO(J)=CHAR(KTL(J))
          OP(J)=CHAR(OPL(J))
C
C         ART DES FARB-/BINDMITTELS
C
          ICHF(J)=ICHL(J)
      ENDDO
C
C     FARBMITTELPROZ UND BINDEMITTELPROZ IN ABHAENGIGKEIT VON KBPROZ
C

C
C
C
      DO J=1,NF
C
C       STUETZSTELLEN BEI KONZENTRATIONABHAENGIGKEIT
C
C
         NST(J)=NSTL(J)
         DO I=1,NST(J)
           DO KW=1,KM
            CST(I,J,KW)=CSTL(KW,I,J)
           ENDDO
         ENDDO
C
C
C
C
C
C
C
C       ABSORPTIONS- UND STREUKOEFFIZIENTEN
C
          DO KW=1,KM
            NPHLF=NPNPS(NPX,NST(J))
            DO L=1,NPHLF
               DO I=1,NWE
                  IF(CNZDEP().EQ.1) THEN
                     IF(L.EQ.NPHLF) THEN
C
C
C                        ALBEDO
C
C
                    	 ABSTA(I,KW,L,J)=GRUND(I,KW,L,J)
                     ELSE
                         ABSTA(I,KW,L,J)
     &                   =0.01* FST(J)*GRUND(I,KW,L,J)
                     ENDIF
                  ELSEIF(CNZDEP().EQ.2) THEN
                     ABSTA(I,KW,L,J)
     &                   =0.01* FST(J)*GRUND(I,KW,L,J)
                  ELSE
                    ABSTA(I,KW,L,J)
     &                 =0.01*FST(J)*GRUND(I,KW,L,J)
                  ENDIF
               ENDDO
            ENDDO
          ENDDO
      ENDDO
C
C
C
C
C
 900  IF(IFEHL(IER).NE.0) THEN
        CALL GETFEH(FEHL)
      ENDIF
      RETURN
      END
C
C

