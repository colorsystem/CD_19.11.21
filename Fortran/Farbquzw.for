C     IPRG=0    FARKOORD,MISCH,UNENDL,FAUMR
C     IPRG=97   FASDEK,FSTDEK
C     IPRG=99   FASTRA,FSTTRA
C     IPRG=100  FSTTEX
C     IPRG=111  FASFOG
C     IPRG=104  DEKDEK
C     IPRG=105  DEKTRA

C     Last change: KU 22.08.2018 20:32:53
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C***************************************** FASDEK ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c

      SUBROUTINE FASDEKZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUKS,TYKS,RKS,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 NQU,NPAM,KW,NWEL,KML,KFIT,NFAECHAR,NFAEWRT
      LOGICAL(KIND=4) :: COTYRWRT
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
C    (7) AUSWERTUNG MIT HILFSMESSUNGEN (SONDERPROG) DECKEND (Z.B.LACK)
C    (8) NICHT AKTIVIERT
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
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUKS(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYKS(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*)::RDAM,RDAB,RKS,RDUN
      REAL(KIND=8),DIMENSION(64,*)::WERT
      TYPE(TYPARAM) PARAM(*)
      TYPE(SRWERT) RWERP,RWERUN(2,2),RWERUK(2,2)
      REAL(KIND=8),TARGET,ALLOCATABLE,DIMENSION(:) ::RZ,RUN11,RUN12,
     &                                                  RUN21,RUN22,
     &                                                  RUK11,RUK12,
     &                                                  RUK21,RUK22
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
      CHARACTER*6 BLAN
      DATA BLAN/'      '/
C
c      OPEN(27,FILE='TXTQUAL.TXT')
C
C
      CALL FEHINI()
      IER=0
      IPRG=97
C
C
      ALLOCATE(RZ(NWEL),RUN11(NWEL),RUN12(NWEL),RUN21(NWEL),RUN22(NWEL),
     &                  RUK11(NWEL),RUK12(NWEL),RUK21(NWEL),RUK22(NWEL),
     &                  STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF

      RWERP%R=>RZ
      RWERUN(1,1)%R=>RUN11
      RWERUN(1,2)%R=>RUN12
      RWERUN(2,1)%R=>RUN21
      RWERUN(2,2)%R=>RUN22
      RWERUK(1,1)%R=>RUK11
      RWERUK(1,2)%R=>RUK12
      RWERUK(2,1)%R=>RUK21
      RWERUK(2,2)%R=>RUK22
C 
C
      CALL SRT000(RWERP)
      DO I=1,2
        DO J=1,2
           CALL SRT000(RWERUN(I,J))
           CALL SRT000(RWERUK(I,J))
        END DO
      END DO

      IER=0
      CALL TSTIPRG(IPRG,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
C     Faerbecharacteristik FCA für MODQUAL
C
      IF(KFIT.EQ.2) THEN
         IM=NFAECHAR
         IV=NFAEWRT
         ALLOCATE(FCA(NFAECHAR,IV),STAT=IER)
      ENDIF
C
C
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      CALL QUAKOR('D')
c
c
      DO J=1,NPAM
         PARAM(J)%ID=-1
         PARAM(J)%NR=-1
         PARAM(J)%LNR=-1
         PARAM(J)%AuswID=-1
         PARAM(J)%CMETH=BLAN
         PARAM(J)%ITP=0
         DO I=1,64
           WERT(I,J)=HUGE(1.)
         ENDDO 
      ENDDO
c
c
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
C      Wird nicht verwendet (nur für Aufruf von GWRTDEK und RWRTDEK)
C
       DO I=1,NWS()
          RU(I)=1.0
       ENDDO
C
       IRETRA=0
       IRT=IRETRA
C
C
C
cc
c
c
c
c
      DO L=1,NQU
C
C
            IF(TYREDAM(L)%RETR.NE.IRETRA) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
            ENDIF

C
C
C
            ITP=0
c            CALL GETRWE(KW,1,NWEL,TYREDAM(L),QUREDAM(L),RDAM(1,KW,L),
c     &                  RWERP,IER)
            CALL GETKPF(KW,TYREDAM(L),QUREDAM(L),RWERP,IER)
            CALL GETRWRT(NWEL,RDAM(1,KW,L),RWERP%R)

            IF(IFEHL(IER).NE.0) GOTO 900
            ICAR=ICHAR(RWERP%CART(1:1))
            IF(ICAR.LT.64.OR.ICAR.GT.69) THEN
                IER=4144
                IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF
            IF(RWERP%CART(1:2).EQ.'@T') THEN
              ITP=1
            ELSEIF (RWERP%CART(2:2).EQ.'P'
     &          .OR.RWERP%CART(2:2).EQ.'T')THEN
              ITP=2 
            ELSE
              CYCLE
            ENDIF
            IF(L.EQ.1.AND.ITP.NE.1) THEN
               IER=4152
               IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF
C
C
C          BERECHNUNG DER STARTKONZENTRATION
C
C
C              MENGE(BUNT)*PROZ(BUNT)*0.01
C          C= ------------------------------------------
C              MENGE(WEISS)*PROZ(WEISS)*0.01
C
C
C
           C=RWERP%CAMP(3)*RWERP%CAMP(4)/(RWERP%CAMP(1)*RWERP%CAMP(2)+
     &                                    TINY(1.D0))
           IF(C.EQ.0.D0) GOTO 98
           CA=C
C
C
C
C
           DO I=1,NWS()
             RH(I)= RWERP%R(I)
           END DO

           IF(KFIT.EQ.2) THEN
C
C             Optische Konstanten und Färbecharakteristik
C
C
              CALL GWRTDEK(C,IER)
              CALL RWRTDEK(C,NWS(),RU,RF,GV,G,IER)

              DO I=1,IM
                 FCA(I,1)= FAECHAR(I,1,L)*RWERP%CAMP(4)
     &                /(RWERP%CAMP(1)*RWERP%CAMP(2)+  TINY(1.D0))
              END DO
              CALL FAERBCH(IRT,KW)
              DO I=1,IM
                DO J=2,IV
                  FAECHAR(I,J,L)=FCA(I,J)
                END DO
              ENDDO
           ELSEIF(KFIT.EQ.1) THEN
C

             IF(QUKS(L)%CART(3:4).NE.'KS') THEN
                IF(IFEHL(4164).NE.0) GOTO 900
             ENDIF
C
C            Neue Konzentration
C
             C=QUKS(L)%CAMP(3)*QUKS(L)%CAMP(4)/
     &        (QUKS(L)%CAMP(1)*QUKS(L)%CAMP(2)+TINY(1.D0))

C
C            Remission für vorgegebene Konzentration
C
C

             DO I=1,NWS()
               G(I)=RKS(I,KW,L)
             ENDDO
             CALL RWRTDEK(C,NWS(),RU,RF,GV,G,IER)
           ELSE
C
C
C          Altes FASDEK (s. FARBQUAL)  (z.B. KFIT=0)
C          Berechnung der Farbstärke durch verschiedenen Angleichverfahren (s. IART)
C
            CALL FSTDE(C,RH,ITP,IER)
           ENDIF
           IF(IFEHL(IER).NE.0) GOTO 900
C 
C 
           IF(.NOT.COTYRWRT(TYREDAB(L),TYREDAM(L))) THEN
            TYREDAB(L)=TYREDAM(L)
            TYKS(L)=TYREDAB(L) 
            QUREDAB(L)=QUREDAM(L)
            QUKS(L)=QUREDAB(L)
            QUREDAB(L)%CART(3:3)='B'
            QUKS(L)%CART(3:4)='KS'
           ENDIF

C 
           DO I=1,NWS()
              RDAB(I,KW,L)=RF(I)
              RKS(I,KW,L)=G(I)
           ENDDO
           PARAM(L)%ITP=ITP
           PARAM(L)%KWB=1
           PARAM(L)%ID=RWERP%ID
           PARAM(L)%NR=RWERP%NR
           PARAM(L)%LNR=L-1
           PARAM(L)%RETR=RWERP%RETR
           PARAM(L)%AuswID=IPRG
           PARAM(L)%CMETH(1:4)=RWERP%CART
           PARAM(L)%CMETH(5:5)=CHAR(KW+48)
           PARAM(L)%CMETH(6:6)=CHAR(49)
           CALL LIFDE(ITP,CA,C,
     &                RWERP,PARAM(L),WERT(1,L))
c
C
C 
      ENDDO
c

 900  CONTINUE
      CALL GETFEH(FEHL)
      DEALLOCATE(RZ,RUN11,RUN12,RUN21,RUN22,
     &              RUK11,RUK12,RUK21,RUK22,STAT=IER)
      IF(ALLOCATED(FCA)) THEN
        DEALLOCATE(FCA,STAT=IER)
      ENDIF
      RETURN
  98  IER=4021
      IF(IFEHL(IER).NE.0) GOTO 900
      GOTO 900
c
      END
c
c
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FASTRA  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
C     Transparente Schichten
C
C
      SUBROUTINE FASTRAZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUEX,TYEX,REX,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODGKWR,ONLY:FOPT
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NQU,NPAM,KW,NWEL,KML,NFAECHAR,NFAEWRT
      LOGICAL(KIND=4) :: COTYRWRT
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
C    (7) AUSWERTUNG MIT HILFSMESSUNGEN (SONDERProgramm) TRANSPARENT (Z.B. DRUCK)       
C    (8) NICHT AKTIVIERT
C 
C 
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUEX(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYEX(*)
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*)::RDAB,RDAM,REX,RDUN
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      TYPE(SRWERT) RWERP,RWERUN(2,2),RWERUK(2,2)
      REAL(KIND=8),TARGET,ALLOCATABLE,DIMENSION(:) ::RZ,RUN11,RUN12,
     &                                                  RUN21,RUN22,
     &                                                  RUK11,RUK12,
     &                                                  RUK21,RUK22
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
      CHARACTER*6 BLAN
      DATA BLAN/'      '/
C
C
C
      CALL FEHINI()
      IER=0
      IPRG=99
C
C
      ALLOCATE(RZ(NWEL),RUN11(NWEL),RUN12(NWEL),RUN21(NWEL),RUN22(NWEL),
     &                  RUK11(NWEL),RUK12(NWEL),RUK21(NWEL),RUK22(NWEL),
     &                  STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      RWERP%R=>RZ
      RWERUN(1,1)%R=>RUN11
      RWERUN(1,2)%R=>RUN12
      RWERUN(2,1)%R=>RUN21
      RWERUN(2,2)%R=>RUN22
      RWERUK(1,1)%R=>RUK11
      RWERUK(1,2)%R=>RUK12
      RWERUK(2,1)%R=>RUK21
      RWERUK(2,2)%R=>RUK22

C 
C
      CALL SRT000(RWERP)
      DO I=1,2
        DO J=1,2
           CALL SRT000(RWERUN(I,J))
           CALL SRT000(RWERUK(I,J))
        END DO
      END DO

C
C
C
C
C
C 
C
      CALL TSTIPRG(IPRG,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
C     Faerbecharacteristik FCA für MODQUAL
C
      IF(KFIT.EQ.2) THEN
         IM=NFAECHAR
         IV=NFAEWRT
         ALLOCATE(FCA(NFAECHAR,IV),STAT=IER)
      ENDIF
C
C
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      CALL QUAKOR('T')


c
      DO J=1,NPAM
         PARAM(J)%ID=-1
         PARAM(J)%NR=-1
         PARAM(J)%LNR=-1
         PARAM(J)%AuswID=-1
         PARAM(J)%ITP=0
         PARAM(J)%CMETH=BLAN
         DO I=1,64
            WERT(I,J)=HUGE(1.)
         ENDDO 
      ENDDO
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
      DO KU=1,1
          IRETRA=TYREDUN(KU)%RETR
          DO I=1,NWS()
             RHI=RDUN(I,KW,KU)
             RWERUN(KW,KU)%R(I)=RHI
             RWERUK(KW,KU)%R(I)=TRKORR(RHI,KW,IRETRA)
          ENDDO
       ENDDO
       DO I=1,NWS()
             RU(I)=RWERUK(KW,1)%R(I)
       ENDDO
       IRT=IRETRA
C
C
C
C
cc
c
c
c
c
      DO L=1,NQU
C
C
C       Gleiche Winkel
C
C
C
            ITP=0
C            CALL GETRWE(KW,1,NWEL,TYREDAM(L),QUREDAM(L),RDAM(1,KW,L),
C     &                  RWERP,IER)
C            CALL GETRWE(KW,TYREDAM(L),QUREDAM(L),1,RWERP,IER)
            CALL GETKPF(KW,TYREDAM(L),QUREDAM(L),RWERP,IER)
            CALL GETRWRT(NWEL,RDAM(1,KW,L),RWERP%R)

            IF(IFEHL(IER).NE.0) GOTO 900
            IF(RWERP%CART(1:2).EQ.'@T') THEN
              ITP=1
            ELSEIF (RWERP%CART(2:2).EQ.'P'
     &          .OR.RWERP%CART(2:2).EQ.'T')THEN
              ITP=2 
            ELSE
              CYCLE
            ENDIF
            IF(L.EQ.1.AND.ITP.NE.1) THEN
               IER=4152
               IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF
C
C
C
C          BERECHNUNG DER STARTKONZENTRATION
C
C              MENGE(BUNT)*PROZ(BUNT)*0.01
C          C= ------------------------------------------
C                    FLAECHE           
C
           C=RWERP%CAMP(1)*0.01*RWERP%CAMP(3)/(RWERP%CAMP(2)+TINY(1.))
           IF(C.EQ.0.D0) GOTO 98
           CA=C

C
C
C
C
           DO I=1,NWS()
             RH(I)= RWERP%R(I)
           END DO
C
C
           IF(KFIT.EQ.2) THEN

C
C             Optische Kobstanten und Färbecharakteristik
C
C
              CALL GWRTTRA(C,IER)
              CALL RWRTTRA(C,NWS(),RU,RF,GV,G,IER)

              DO I=1,IM
                 FCA(I,1)= FAECHAR(I,1,L)*0.01*RWERP%CAMP(3)
     &                     /(RWERP%CAMP(2)+TINY(1.))
              END DO
              CALL FAERBCH(IRT,KW)
              DO I=1,IM
                DO J=2,IV
                  FAECHAR(I,J,L)=FCA(I,J)
                END DO
              ENDDO
           ELSEIF(KFIT.EQ.1) THEN
             IF(QUEX(L)%CART(3:4).NE.'EX') THEN
                IF(IFEHL(4164).NE.0) GOTO 900
             ENDIF
C
C            Neue Konzentrationen
C
             C=QUEX(L)%CAMP(1)*0.01*QUEX(L)%CAMP(3)/
     &        (QUEX(L)%CAMP(2)+TINY(1.))

C
C            Remission für vorgegebene Konzentration
C
C
             DO I=1,NWS()
               G(I)=REX(I,KW,L)
             ENDDO
             CALL RWRTTRA(C,NWS(),RU,RF,GV,G,IER)
           ELSE
C
C
C           Altes FASTRA (s. FARBQUAL)  (z.B. KFIT=0)
C           Berechnung der Farbstärke durch verschiedenen Angleichverfahren (s. IART)
C
            CALL FSTTR(C,RH,ITP,IER)
           ENDIF
           IF(IFEHL(IER).NE.0) GOTO 900
C
C
C
           IF(.NOT.COTYRWRT(TYREDAB(L),TYREDAM(L))) THEN
            TYREDAB(L)=TYREDAM(L)
            TYEX(L)=TYREDAB(L) 
            QUREDAB(L)=QUREDAM(L)
            QUEX(L)=QUREDAB(L)
            QUREDAB(L)%CART(3:3)='B'
            QUEX(L)%CART(3:4)='EX'
           ENDIF
C 




           CALL LIFTR(ITP,CA,C,
     &                RWERP,PARAM(L),WERT(1,L))
           DO I=1,NWS()
              RDAB(I,KW,L)=RF(I)
              REX(I,KW,L)=G(I)*FOPT
           ENDDO
           PARAM(L)%ITP=ITP
           PARAM(L)%KWB=1
           PARAM(L)%RETR=RWERP%RETR
           PARAM(L)%NR=RWERP%NR
           PARAM(L)%LNR=L-1
           PARAM(L)%ID=RWERP%ID
           PARAM(L)%AuswID=IPRG
           PARAM(L)%CMETH(1:4)=RWERP%CART
           PARAM(L)%CMETH(5:5)=CHAR(KW+48)
           PARAM(L)%CMETH(6:6)=CHAR(49)
C
C
C
C    
C
      ENDDO
c
 900  CONTINUE
      CALL GETFEH(FEHL)
      IF(ALLOCATED(FCA)) THEN
        DEALLOCATE(FCA,STAT=IER)
      ENDIF
      DEALLOCATE(RZ,RUN11,RUN12,RUN21,RUN22,
     &              RUK11,RUK12,RUK21,RUK22,STAT=IER)
      RETURN
  98  IER=4021
      IF(IFEHL(IER).NE.0) GOTO 900
      RETURN
c
c     
      END
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FASTEX  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
      SUBROUTINE FASTEXZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUKS,TYKS,RKS,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NQU,NPAM,KW,NWEL,KML,NFAECHAR,NFAEWRT
      LOGICAL(KIND=4) :: COTYRWRT
      
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
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUKS(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYKS(*)
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*)::RDAB,RDAM,RKS,RDUN
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      TYPE(SRWERT) RWERP,RWERUN(2,2),RWERUK(2,2)
      REAL(KIND=8),TARGET,ALLOCATABLE,DIMENSION(:) ::RZ,RUN11,RUN12,
     &                                                  RUN21,RUN22,
     &                                                  RUK11,RUK12,
     &                                                  RUK21,RUK22
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
      TYPE(TYFEH) FEHL
      CHARACTER*6 BLAN
      EXTERNAL REFWTA
      DATA BLAN/'      '/
      DATA TAU/1.D80/,RD/1.D0/
C
C
C
      CALL FEHINI()
      IER=0
      IPRG=100
C
C
      ALLOCATE(RZ(NWEL),RUN11(NWEL),RUN12(NWEL),RUN21(NWEL),RUN22(NWEL),
     &                  RUK11(NWEL),RUK12(NWEL),RUK21(NWEL),RUK22(NWEL),
     &                  STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF

      RWERP%R=>RZ
      RWERUN(1,1)%R=>RUN11
      RWERUN(1,2)%R=>RUN12
      RWERUN(2,1)%R=>RUN21
      RWERUN(2,2)%R=>RUN22
      RWERUK(1,1)%R=>RUK11
      RWERUK(1,2)%R=>RUK12
      RWERUK(2,1)%R=>RUK21
      RWERUK(2,2)%R=>RUK22
C 
C
      CALL SRT000(RWERP)
      DO I=1,2
        DO J=1,2
           CALL SRT000(RWERUN(I,J))
           CALL SRT000(RWERUK(I,J))
        END DO
      END DO

      IER=0
C
C
C
C
C

C 
C
C
      CALL TSTIPRG(IPRG,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
C     Faerbecharacteristik FCA für MODQUAL
C
      IF(KFIT.EQ.2) THEN
         IM=NFAECHAR
         IV=NFAEWRT
         ALLOCATE(FCA(NFAECHAR,IV),STAT=IER)
      ENDIF
C
C
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      CALL QUAKOR( 'D')

c
c
      DO J=1,NPAM
         PARAM(J)%ID=-1
         PARAM(J)%NR=-1
         PARAM(J)%LNR=-1
         PARAM(J)%AuswID=-1
         PARAM(J)%ITP=0
         PARAM(J)%CMETH=BLAN
         DO I=1,64
             WERT(I,J)=HUGE(1.)
         ENDDO 
      ENDDO
c
c
C
C     Berechnung von korrigierten Reflexionswerten fuer Untergruende
C
C
C
C
C     Nummer des Winkels
C
      IRETRA=0
      IRT=IRETRA
      IF(KW.LE.0.OR.KW.GT.KML) THEN
        IF(IFEHL(4165).NE.0) GOTO 900
      ENDIF
      KWC=KW
      DO KU=1,1
          IF(TYREDUN(KU)%RETR.EQ.1) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
          ENDIF
          DO I=1,NWS()
             RHI=RDUN(I,KW,KU)
             RWERUN(1,KU)%R(I)=RHI
             RWERUK(1,KU)%R(I)=TRKORR(RHI,KW,IRETRA)
          ENDDO
      ENDDO
C
C      K/S-WERTE FUER UNTERGRUND (SUBSTRAT)
C
C
       ALU=0.5
       DO I=1,NWS()
         RU(I)=RWERUN(1,1)%R(I)
C
C
C
          CALL REINV(TAU,ALU,RD,RU(I),REFWTA,KW,2,IER)
          GU(I)=1.0/(ALU+TINY(1.))-1.0
       ENDDO
C
cc
c
c
c
      DO L=1,NQU
C
C
C       Gleiche Winkel
C
C
C
            ITP=0
            IF(TYREDAM(L)%RETR.NE.IRETRA) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
            ENDIF
c            CALL GETRWE(KW,1,NWEL,TYREDAM(L),QUREDAM(L),RDAM(1,KW,L),
c     &                  RWERP,IER)
            CALL GETKPF(KW,TYREDAM(L),QUREDAM(L),RWERP,IER)
            CALL GETRWRT(NWEL,RDAM(1,KW,L),RWERP%R)

            IF(IFEHL(IER).NE.0) GOTO 900
            IF(RWERP%CART(1:2).EQ.'@T') THEN
              ITP=1
              AMNT=RWERP%CAMP(1)
            ELSEIF (RWERP%CART(2:2).EQ.'P'
     &          .OR.RWERP%CART(2:2).EQ.'T')THEN
              ITP=2 
            ELSE
              CYCLE
            ENDIF
            IF(L.EQ.1.AND.ITP.NE.1) THEN
               IER=4152
               IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF
C
C
C
C
C          BERECHNUNG DER STARTKONZENTRATION
C
C              MENGE(PROBE)             
C          C= ------------------------------------------
C              MENGE(TYP)              
C
           C=RWERP%CAMP(1)/(AMNT+TINY(1.))
           IF(C.EQ.0.D0) GOTO 98
           CA=C

C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           DO I=1,NWS()
             RH(I)= RWERP%R(I)
           END DO
           IF(KFIT.EQ.2) THEN
C
C             Optische Konstanten und Färbecharakteristik
C
C
              CALL GWRTTEX(C,IER)
              CALL RWRTTEX(C,NWS(),GU,RF,GV,G,IER)

              DO I=1,IM
                 FCA(I,1)= FAECHAR(I,1,L)/(AMNT+TINY(1.))
              END DO
              CALL FAERBCH(IRT,KW)
              DO I=1,IM
                DO J=2,IV
                  FAECHAR(I,J,L)=FCA(I,J)
                END DO
              ENDDO
           ELSEIF(KFIT.EQ.1) THEN
             IF(QUKS(L)%CART(3:4).NE.'KS') THEN
                IF(IFEHL(4164).NE.0) GOTO 900
             ENDIF
C
C            Neue Konzentrationen
C
             C=QUKS(L)%CAMP(1)/(AMNT+TINY(1.))
C
C
C
C            Remission für vorgegebene Konzentration
C
C
             DO I=1,NWS()
               G(I)=RKS(I,KW,L)
             ENDDO
             CALL RWRTTEX(C,NWS(),GU,RF,GV,G,IER)
           ELSE
C
C
C          Altes FASDEK (s. FARBQUAL)  (z.B. KFIT=0)
C          Berechnung der Farbstärke durch verschiedenen Angleichverfahren (s. IART)
C
            CALL FSTTX(C,RH,ITP,IER)
           ENDIF
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
           IF(IFEHL(IER).NE.0) GOTO 900
C
C
C
           IF(.NOT.COTYRWRT(TYREDAB(L),TYREDAM(L))) THEN
            TYREDAB(L)=TYREDAM(L)
            TYKS(L)=TYREDAB(L) 
            QUREDAB(L)=QUREDAM(L)
            QUKS(L)=QUREDAB(L)
            QUREDAB(L)%CART(3:3)='B'
            QUKS(L)%CART(3:4)='KS'
           ENDIF
C 
C

           DO I=1,NWS()
              RDAB(I,KW,L)=RF(I)
              RKS(I,KW,L)=G(I)
           ENDDO
           CALL LIFTX(ITP,CA,C,
     &                RWERP,PARAM(L),WERT(1,L))
           PARAM(L)%ITP=ITP
           PARAM(L)%KWB=1
           PARAM(L)%RETR=RWERP%RETR
           PARAM(L)%NR=RWERP%NR
           PARAM(L)%LNR=L-1
           PARAM(L)%ID=RWERP%ID
           PARAM(L)%AuswID=IPRG
           PARAM(L)%CMETH(1:4)=RWERP%CART
           PARAM(L)%CMETH(5:5)=CHAR(KW+48)
           PARAM(L)%CMETH(6:6)=CHAR(49)
C
      ENDDO
c
 900  CONTINUE
      CALL GETFEH(FEHL)
      DEALLOCATE(RZ,RUN11,RUN12,RUN21,RUN22,
     &              RUK11,RUK12,RUK21,RUK22,STAT=IER)
      IF(ALLOCATED(FCA)) THEN
        DEALLOCATE(FCA,STAT=IER)
      ENDIF
      RETURN
  98  IER=4021
      IF(IFEHL(IER).NE.0) GOTO 900
      GOTO 900
c
c     
      END
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FSTDEK  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
      SUBROUTINE FSTDEKZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  QUASUN,TYASUN,RSUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NWEL,KML,NQU,NPAM,KW,NFAECHAR,NFAEWRT
      LOGICAL(KIND=4) :: COTYRWRT
      REAL(KIND=8),DIMENSION(2) :: DEWESC
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
C    (7) AUSWERTUNG MIT HILFSMESSUNGEN (SONDERPROG) DECKEND (Z.B.LACK)
C    (8) NICHT AKTIVIERT
C 
C 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUABSTR(*)
      TYPE(QURWERT) QURUNND(*),QUCRMAX(*),QUASUN(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYABSTR(*)
      TYPE(TYRWERT) TYRUNND(*),TYCRMAX(*),TYASUN(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RDUN,RABSTR
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RSUN,RUNND,RCRMAX
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      TYPE(SRWERT) RWERP(2)
      REAL(KIND=8),TARGET,ALLOCATABLE,DIMENSION(:) ::RZ1,RZ2
      TYPE(TYFEH) FEHL
      CHARACTER*6 BLAN
      DATA BLAN/'      '/


      INTERFACE
      REAL(KIND=8) FUNCTION REFDE(A,S,RD,KW,REFAA,REFSS)
      INTEGER(KIND=4) ::KW
      REAL(KIND=8) ::A,S,RD
      REAL(KIND=8),OPTIONAL  ::REFAA,REFSS
      END FUNCTION
      END INTERFACE

C
C
C
      CALL FEHINI()
      IER=0
      IPRG=97
C
C
      ALLOCATE(RZ1(NWEL),RZ2(NWEL),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      RWERP(1)%R=>RZ1
      RWERP(2)%R=>RZ2
C
      CALL SRT000(RWERP(1))
      CALL SRT000(RWERP(2))
      IER=0
      CALL TSTIPRG(IPRG,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
C     Faerbecharacteristik FCA für MODQUAL
C
      IF(KFIT.EQ.2) THEN
         IM=NFAECHAR
         IV=NFAEWRT
         ALLOCATE(FCA(NFAECHAR,IV),STAT=IER)
      ENDIF
C
C
      IM=NFAECHAR
      IV=NFAEWRT
      ALLOCATE(FCA(NFAECHAR,IV),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
 
      CALL QUAKOR('D')

c
c
      DO J=1,NPAM
         PARAM(J)%ID=-1
         PARAM(J)%NR=-1
         PARAM(J)%LNR=-1
         PARAM(J)%AuswID=-1
         PARAM(J)%CMETH=BLAN
         PARAM(J)%ITP=0
         DO I=1,64
           WERT(I,J)=HUGE(1.)
         ENDDO 
      ENDDO
c
C
C     Nummer des Winkels
C
      KWC=KW
      IF(KW.LE.0.OR.KW.GT.KML) THEN
        IF(IFEHL(4165).NE.0) GOTO 900
      ENDIF
      IRETRA=0
      IRT=IRETRA
      DEWESC(1)=0.0
      DEWESC(2)=0.0

C
c
C
C
C
C     Berechnung der Streu- und Absorptionswerte für Weißpigment und Schwarzpigment
C     (COMMON COFDK)

      CALL FVODE(KW,NWEL,KML,
     &           QUREDUN,TYREDUN,RDUN,QUASUN,TYASUN,RSUN,IER)
c      C=0.0
c      CF(1)=0.0
c      CF(2)=0.0
c      DO J=1,3
c         WRITE(27,*) 'RUMessung',J,(RUM(I,J),I=1,NWEL)
c         SELECT CASE(J)
c           CASE (1)
c             CW(1)=1.0
c             CS(1)=0.0
c           CASE (2)
c             CW(1)=0.0
c             CS(1)=1.0
c           CASE (3)
c             CW(1)=AMWAMS
c             CS(1)=1.0
c         END SELECT
c         CALL RWWRTDE(1,C,NWEL,RUM(1,1),RUR(1,1),A,S,GV,IER)
c         WRITE(27,*) 'RURechnung',J,(RUM(I,1),I=1,NWEL)
c      END DO
C
C
CC
      DO KU=1,2
       DO I=1,NWS()
         IF(KU.EQ.1) THEN
            RUR(I,KU)=1.0
         ELSE
            RUR(I,KU)=0.0
         ENDIF
         RUM(I,KU)=RUMESS(RUR(I,KU),KW)
C
C
C
       ENDDO
      END DO

C
C
cc
c
c
c
c
      DO L=1,NQU,2
C
C
c           LP=L
            L2=L/2+1

C
C
C
            ITP=0
            IF(TYREDAM(L)%RETR.NE.IRETRA) THEN
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
            ELSEIF (RWERP(1)%CART(2:2).EQ.'P'
     &          .OR.RWERP(1)%CART(2:2).EQ.'T')THEN
              ITP=2 
            ELSE
              CYCLE
            ENDIF
            IF(L.EQ.1.AND.ITP.NE.1) THEN
               IER=4152
               IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF


C
C
C          BERECHNUNG DER STARTKONZENTRATION
C
C
C              MENGE(BUNT)*PROZ(BUNT)*0.01
C          C= ------------------------------------------
C              MENGE(WEISS)*PROZ(WEISS)*0.01
C
C          entsprechend für schwarz
C
           CWW=RWERP(1)%CAMP(3)*RWERP(1)%CAMP(4)/
     &      (RWERP(1)%CAMP(1)*RWERP(1)%CAMP(2)+TINY(1.))
           IF(CWW.EQ.0.D0) GOTO 98
C
           CSS=RWERP(2)%CAMP(3)*RWERP(2)%CAMP(4)/
     &      (RWERP(2)%CAMP(1)*RWERP(2)%CAMP(2)+TINY(1.))
           IF(CSS.EQ.0.D0) GOTO 98
           CF(1)=CWW
           CF(2)=CSS
           CW(1)=1.
           CW(2)=0.
           CS(1)=0.
           CS(2)=1.
           CA=CWW
           C=CWW
           DO I=1,NWS()
              RP(I,1)=RWERP(1)%R(I)
              RP(I,2)=RWERP(2)%R(I)
           END DO

C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

           IF(KFIT.EQ.2) THEN
C
C             Optische Daten berechnen
C
C

C
C
C             BERECHNUNG DER ABSORPTIONS- UND STREUWERTE AUS SCHICHTEN UEBER
C             WEISSEM UND SCHWARZEM UNTERGRUND
C
              CALL ABSSTRDE(NWS(),A,S,IER)

              IF(IFEHL(IER).NE.0) GOTO 900
              IER=IFEHL(IRUECK(DEWESC))

C
C
C             Färbecharakteristik
C
C
              DO I=1,IM
                 FCA(I,1)=FAECHAR(I,1,L2)*0.01*RWERP(1)%CAMP(3)/
     &                   (RWERP(1)%CAMP(2)+TINY(1.))
              END DO
              CALL FLAQLN(IRT,KW)
              DO I=1,IM
                DO J=2,IV
                  FAECHAR(I,J,L2)=FCA(I,J)
                END DO
              ENDDO

           ELSEIF(KFIT.EQ.1) THEN
C
             IF(QUABSTR(L)%CART(3:4).NE.'AF'.OR.
     &          QUABSTR(LL)%CART(3:4).NE.'DF')THEN
                IF(IFEHL(4164).NE.0) GOTO 900
             ENDIF
C
C           Neue Konzentrationen
C
            CF(1)=QUABSTR(L)%CAMP(3)*QUABSTR(L)%CAMP(4)/
     &      (QUABSTR(L)%CAMP(1)*QUABSTR(L)%CAMP(2)+TINY(1.))
            IF(CF(1).EQ.0.D0) GOTO 98
C
            CF(2)=QUABSTR(LL)%CAMP(3)*QUABSTR(LL)%CAMP(4)/
     &      (QUABSTR(LL)%CAMP(1)*QUABSTR(LL)%CAMP(2)+TINY(1.))

C
C            Remission für vorgegebene Dicke und vorgegebene optische Daten
C

             DO I=1,NWS()
              A(I)=RABSTR(I,KW,L)
              S(I)=RABSTR(I,KW,LL)
             ENDDO
             CALL RWWRTDE2(CF,NWS(),RP,RUR,A,S,IER)
           ELSE
C
C
C             Dicke für deckende Schicht
C
C

             CALL ABSSTRDE(NWS(),A,S,IER)
             IF(IFEHL(IER).NE.0) GOTO 900
             IER=IFEHL(IRUECK(DEWESC))

C
C
             CA=CWW
             C=CWW
             CALL FSTDE2(C,RWERP(1)%R,ITP,IER)

           ENDIF
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           IF(IFEHL(IER).NE.0) GOTO 900

C
C
C 

           IF(.NOT.COTYRWRT(TYREDAB(L),TYREDAM(L))) THEN
            TYREDAB(L)=TYREDAM(L)
            TYREDAB(L+1)=TYREDAM(L+1)
            TYABSTR(L)=TYREDAB(L)
            TYABSTR(L+1)=TYREDAB(L+1)
            QUREDAB(L)=QUREDAM(L)
            QUREDAB(L+1)=QUREDAM(L+1)
            QUABSTR(L)=QUREDAB(L)
            QUABSTR(L+1)=QUREDAB(L+1)
            QUREDAB(L)%CART(3:3)='B'
            QUREDAB(L+1)%CART(3:3)='B'
            QUABSTR(L)%CART(3:4)='AF'
            QUABSTR(L+1)%CART(3:4)='DF'
           ENDIF
C
           CSCH=C
           CALL RWWRTDE(1,CSCH,NWS(),RH,RUR(1,1),A,S,GV,IER)
           DO I=1,NWS()
              RDAB(I,KW,L)=RH(I)
              RABSTR(I,KW,L)=A(I)
              RABSTR(I,KW,L+1)=S(I)
           ENDDO
           CSCH=CSS*C/CA
           CALL RWWRTDE(2,CSCH,NWS(),RH,RUR(1,2),A,S,GV,IER)
           DO I=1,NWS()
              RDAB(I,KW,L+1)=RH(I)
           ENDDO
           PARAM(L)%ITP=ITP
           PARAM(L)%KWB=1
           PARAM(L)%ID=RWERP(1)%ID
           PARAM(L)%RETR=RWERP(1)%RETR
           PARAM(L)%NR=RWERP(1)%NR
           PARAM(L)%LNR=L2-1
           PARAM(L)%AuswID=IPRG
           PARAM(L)%CMETH(1:4)=RWERP(1)%CART
           PARAM(L)%CMETH(5:5)=CHAR(KW+48)
           PARAM(L)%CMETH(6:6)=CHAR(49)
C
           CALL LIFDE2(ITP,CA,C,DEWESC,
     &                RWERP(1),PARAM(L),WERT(1,L))
      ENDDO
c
 900  CONTINUE
      CALL GETFEH(FEHL)
      DEALLOCATE(RZ1,RZ2,STAT=IER)
      IF(ALLOCATED(FCA)) THEN
        DEALLOCATE(FCA,STAT=IER)
      ENDIF
      RETURN
  98  IER=4021
      IF(IFEHL(IER).NE.0) GOTO 900
      GOTO 900
c
      END
C
C
C
C
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FSTTRA  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
      SUBROUTINE FSTTRAZW(KFIT,KW,NWEL,KML,NQU,
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
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NQU,NPAM,KW,NWEL,KML,NFAECHAR,NFAEWRT
      LOGICAL(KIND=4) :: COTYRWRT
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
C    (7) AUSWERTUNG MIT HILFSMESSUNGEN (SONDERProgramm) TRANSPARENT (Z.B. DRUCK)       
C    (8) NICHT AKTIVIERT
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
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),QUABSTR(*)
      TYPE(QURWERT) QURUNND(*),QUCRMAX(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),TYABSTR(*)
      TYPE(TYRWERT) TYRUNND(*),TYCRMAX(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RDUN,RABSTR
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RUNND,RCRMAX
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
      REAL(KIND=8),DIMENSION(64,*) :: WERT
      REAL(KIND=8),DIMENSION(2) :: D
      REAL(KIND=8) :: RHI
      REAL(KIND=8),DIMENSION(2) :: DEWESC
      TYPE(SRWERT) RWERP(2)
      REAL(KIND=8),TARGET,ALLOCATABLE,DIMENSION(:) ::RZ1,RZ2
      TYPE(TYFEH) FEHL
      CHARACTER*6 BLAN
      EXTERNAL FUCHROM
      DATA BLAN/'      '/
C
C
C
C
      CALL FEHINI()
      IER=0
      ALLOCATE(RZ1(NWEL),RZ2(NWEL),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
C
      RWERP(1)%R=>RZ1
      RWERP(2)%R=>RZ2
C 
C
      CALL SRT000(RWERP(1))
      CALL SRT000(RWERP(2))
      IER=0
      IPRG=99
C
C
C
C
C
C 
C
      CALL TSTIPRG(IPRG,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
C
C     Faerbecharacteristik FCA für MODQUAL
C
C
C
      IF(KFIT.EQ.2) THEN
         IM=NFAECHAR
         IV=NFAEWRT
         ALLOCATE(FCA(NFAECHAR,IV),STAT=IER)
      ENDIF
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF

      CALL QUAKOR('T')

c
c
      DO J=1,NPAM
         PARAM(J)%ID=-1
         PARAM(J)%NR=-1
         PARAM(J)%LNR=-1
         PARAM(J)%AuswID=-1
         PARAM(J)%ITP=0
         PARAM(J)%CMETH=BLAN
         DO I=1,64
            WERT(I,J)=HUGE(1.)
         ENDDO 
      ENDDO
      DEWESC(1)=0.0
      DEWESC(2)=0.0


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
      DO KU=1,2
          IF(TYREDUN(KU)%RETR.EQ.1) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
          ENDIF
          IRETR(KU)=TYREDUN(KU)%RETR


          DO I=1,NWS()
             RHI=RDUN(I,KW,KU)
             RUM(I,KU)=RHI
             RUR(I,KU)=TRKORR(RHI,KW,IRETR(KU))
          ENDDO
      ENDDO

      IRETRA=IRETR(1)
      IRT=IRETRA
      IF(IRETRA.NE.0) THEN
        IF(IFEHL(4146).NE.0) GOTO 900
      ENDIF
C
      IF(IRETR(1).NE.IRETR(2)) THEN
        IF(IFEHL(4146).NE.0) GOTO 900
      ENDIF
C
C
C
cc
c
c
c
c
      DO L=1,NQU,2
C
C
C
c            LP=L
            L2=L/2+1

C           
C
            ITP=0
            IF(TYREDAM(L)%RETR.NE.IRETRA) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
            ENDIF
            CALL GETKPF(KW,TYREDAM(L),QUREDAM(L),RWERP(1),IER)
            CALL GETRWRT(NWEL,RDAM(1,KW,L),RWERP(1)%R)
            IF(IFEHL(IER).NE.0) GOTO 900
            IF(RWERP(1)%CART(4:4).NE.'W') THEN
                IER=4113
                IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF
            LL=L+1
            IF(TYREDAM(LL)%RETR.NE.IRETRA) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
            ENDIF
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
            IF(L.EQ.1.AND.ITP.NE.1) THEN
               IER=4152
               IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF

C
C
C
C
C
C                 MENGE*PROZ*0.01
C          DICKE=---------------------------_ 
C                 FLAECHE
C
           DO J=1,2
              RWERP(J)%CAMP(0)=RWERP(J)%CAMP(1)*0.01*RWERP(J)%CAMP(3)/
     &          (RWERP(J)%CAMP(2)+TINY(1.))
           ENDDO
C
C
           D(1)=RWERP(1)%CAMP(0)
           D(2)=RWERP(2)%CAMP(0)
           IF(ABS(D(1)-D(2)).GT.1.E-7) THEN
              IER=4142
              GOTO 98
           ENDIF
           CF(1)=D(1)
           CF(2)=D(2)
           C=D(1)
           CA=C
           IF(C.EQ.0.D0) GOTO 98
           DO I=1,NWS()
              RP(I,1)=RWERP(1)%R(I)
              RP(I,2)=RWERP(2)%R(I)
           END DO

C
C
C
C
C          Berechnung der Streu- und Absorptionkoeffizienten
C
C
C
C 
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

           IF(KFIT.EQ.2) THEN
C
C             Optische Daten berechnen
C
C

C
C
C             BERECHNUNG DER ABSORPTIONS- UND STREUWERTE AUS SCHICHTEN UEBER
C             WEISSEM UND SCHWARZEM UNTERGRUND
C 
              CALL ABSSTRTR(NWS(),A,S,IER)
              IF(IFEHL(IER).NE.0) GOTO 900
              IER=IFEHL(IRUECK(DEWESC))
              CALL RWWRTTR2(IRETR,D,NWS(),RP,RUR,A,S,IER)

C
C
C             Färbecharakteristik
C
C
              DO I=1,IM
                 FCA(I,1)=FAECHAR(I,1,L2)*0.01*RWERP(1)%CAMP(3)/
     &                   (RWERP(1)%CAMP(2)+TINY(1.))
              END DO
              CALL FLAQLN(IRT,KW)
              DO I=1,IM
                DO J=2,IV
                  FAECHAR(I,J,L2)=FCA(I,J)
                END DO
              ENDDO
C

           ELSEIF(KFIT.EQ.1) THEN
C
             IF(QUABSTR(L)%CART(3:4).NE.'AF'.OR.
     &          QUABSTR(LL)%CART(3:4).NE.'DF')THEN
                IF(IFEHL(4164).NE.0) GOTO 900
             ENDIF
C
C             Neue Dicke
C

             QUABSTR(L)%CAMP(0)=QUABSTR(L)%CAMP(1)
     &       *0.01*QUABSTR(L)%CAMP(3)/(QUABSTR(L)%CAMP(2)+TINY(1.))
             QUABSTR(LL)%CAMP(0)=QUABSTR(LL)%CAMP(1)
     &       *0.01*QUABSTR(LL)%CAMP(3)/(QUABSTR(LL)%CAMP(2)+TINY(1.))
             D(1)=QUABSTR(L)%CAMP(0)
             D(2)=QUABSTR(LL)%CAMP(0)

C
C            Remission für vorgegebene Dicke und vorgegebene optische Daten
C
             C=D(1)
             DO I=1,NWS()
              A(I)=RABSTR(I,KW,L)
              S(I)=RABSTR(I,KW,LL)
             ENDDO
             CALL RWWRTTR2(IRETR,D,NWS(),RP,RUR,A,S,IER)
           ELSE
C
C
C             Absorption und Streuung
C
C
              CALL ABSSTRTR(NWS(),A,S,IER)
              IF(IFEHL(IER).NE.0) GOTO 900
              IER=IFEHL(IRUECK(DEWESC))

              CALL FSTTR2(C,RWERP(1)%R,ITP,IER)

           ENDIF
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           IF(IFEHL(IER).NE.0) GOTO 900
C
C
C
C
C
C
C
           IF(.NOT.COTYRWRT(TYREDAB(L),TYREDAM(L))) THEN
            TYREDAB(L)=TYREDAM(L)
            TYREDAB(L+1)=TYREDAM(L+1)
            TYABSTR(L)=TYREDAB(L)
            TYABSTR(L+1)=TYREDAB(L+1)
            QUREDAB(L)=QUREDAM(L)
            QUREDAB(L+1)=QUREDAM(L+1)
            QUABSTR(L)=QUREDAB(L)
            QUABSTR(L+1)=QUREDAB(L+1)
            QUREDAB(L)%CART(3:3)='B'
            QUREDAB(L+1)%CART(3:3)='B'
            QUABSTR(L)%CART(3:4)='AF'
            QUABSTR(L+1)%CART(3:4)='DF'
           ENDIF

C
C
C          Weißer Untergrund
C
C
           CSCH=C
           CALL RWWRTTR(IRETR(1),CSCH,NWS(),RH,RUR(1,1),A,S,GV,IER)
           DO I=1,NWS()
              RDAB(I,KW,L)=RH(I)
              RABSTR(I,KW,L)=A(I)
              RABSTR(I,KW,L+1)=S(I)
           ENDDO
C
C
C          Schwarzer Untergrund
C
           CALL RWWRTTR(IRETR(2),CSCH,NWS(),RH,RUR(1,2),A,S,GV,IER)

           DO I=1,NWS()
              RDAB(I,KW,L+1)=RH(I)
           END DO
C
C
C
C
           PARAM(L)%ITP=ITP
           PARAM(L)%KWB=1
           PARAM(L)%NR=RWERP(1)%NR
           PARAM(L)%LNR=L2-1
           PARAM(L)%RETR=RWERP(1)%RETR
           PARAM(L)%ID=RWERP(1)%ID
           PARAM(L)%AuswID=IPRG
           PARAM(L)%CMETH(1:4)=RWERP(1)%CART
           PARAM(L)%CMETH(5:5)=CHAR(KW+48)
           PARAM(L)%CMETH(6:6)=CHAR(49)
C

           CALL LIFTR2(ITP,CA,C,DEWESC,
     &                RWERP(1),PARAM(L),WERT(1,L))
C
C
C    
C
      ENDDO
c
 900  CONTINUE
      CALL GETFEH(FEHL)
      DEALLOCATE(RZ1,RZ2,STAT=IER)
      IF(ALLOCATED(FCA)) THEN
        DEALLOCATE(FCA,STAT=IER)
      ENDIF
      RETURN
  98  IER=4021
      IF(IFEHL(IER).NE.0) GOTO 900
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
C*****************************************  DEKTRA  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
      SUBROUTINE DEKTRAZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)

      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODWINK,ONLY:KWC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NWEL,KML,NQU,NPAM,KW,NFAECHAR,NFAEWRT
      LOGICAL(KIND=4) :: COTYRWRT

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
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),
     &                QUABSTR(*),QURUNND(*),QUCRMAX(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),
     &                TYABSTR(*),TYRUNND(*),TYCRMAX(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RUNND,
     &                                      RCRMAX,RABSTR,RDUN
      REAL(KIND=8),DIMENSION(2) :: D,DH
      REAL(KIND=8) :: DIK,DMAX
      REAL(KIND=8),DIMENSION(2) :: DEWESC
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*)::WERT
      TYPE(SRWERT) RWERP(2)
      REAL(KIND=8),TARGET,ALLOCATABLE,DIMENSION(:) ::RZ1,RZ2
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
      CHARACTER*6 BLAN
      EXTERNAL FUCHROM
      DATA BLAN/'      '/
      DATA DIK/1.D0/ 
C
      IER=0
      CALL FEHINI()
      IPRG=105
C
C
C
      ALLOCATE(RZ1(NWEL),RZ2(NWEL),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      RWERP(1)%R=>RZ1
      RWERP(2)%R=>RZ2
C
C
      CALL SRT000(RWERP(1))
      CALL SRT000(RWERP(2))

C
C
C
C
C
C
C     Faerbecharacteristik FCA für MODQUAL
C
C
C
      IF(KFIT.EQ.2) THEN
         IM=NFAECHAR
         IV=NFAEWRT
         ALLOCATE(FCA(NFAECHAR,IV),STAT=IER)
      ENDIF

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

      DO J=1,NPAM
         PARAM(J)%ID=-1
         PARAM(J)%NR=-1
         PARAM(J)%LNR=-1
         PARAM(J)%AuswID=-1
         PARAM(J)%ITP=0
         PARAM(J)%CMETH=BLAN
         DO I=1,64
            WERT(I,J)=HUGE(1.)
         ENDDO 
      ENDDO
      DEWESC(1)=0.0
      DEWESC(2)=0.0


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
      DO KU=1,2
         IF(TYREDUN(KU)%RETR.EQ.1) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
         ENDIF
         IRETR(KU)=TYREDUN(KU)%RETR
         DO I=1,NWS()
             RUM(I,KU)=RDUN(I,KW,KU)
             RUR(I,KU)=TRKORR(RUM(I,KU),KW,IRETR(KU))
         ENDDO
       ENDDO
       IRETRA=0
       IRT=IRETRA
C
C
C
C
cc
c
c
c
c
      DO L=1,NQU,2
C
C
C       Gleiche Winkel
C
C       LAUFVARIABLE PARAM
         LP=L
         L2=L/2+1

C
C
            ITP=0
            IF(TYREDAM(L)%RETR.NE.IRETRA) THEN
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
            IF(TYREDAM(LL)%RETR.NE.IRETRA) THEN
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
            IF(L.EQ.1.AND.ITP.NE.1) THEN
               IER=4152
               IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF

C
C
C                 MENGE*PROZ*0.01
C          DICKE=---------------------------_ 
C                 FLAECHE
C
           DO J=1,2
              RWERP(J)%CAMP(0)=RWERP(J)%CAMP(1)*0.01*RWERP(J)%CAMP(3)/
     &          (RWERP(J)%CAMP(2)+TINY(1.))
           ENDDO
C
C
C
           D(1)=RWERP(1)%CAMP(0)
           D(2)=RWERP(2)%CAMP(0)
           CF(1)=D(1)
           CF(2)=D(2)
           IF(ABS(D(1)-D(2)).GT.1.E-7) THEN
              IER=4142
              IF(IFEHL(IER).NE.0) GOTO 900
           ENDIF
           DIKA=D(1)
           DIK=DIKA
           IF(DIKA.EQ.0.0D0) GOTO 98
           DO I=1,NWS()
              RP(I,1)=RWERP(1)%R(I)
              RP(I,2)=RWERP(2)%R(I)
           END DO


C
C
           IF(KFIT.EQ.2) THEN
C
C             Optische Daten berechnen
C
C
C
C
C             BERECHNUNG DER ABSORPTIONS- UND STREUWERTE AUS SCHICHTEN UEBER
C             WEISSEM UND SCHWARZEM UNTERGRUND
C
              CALL ABSSTRTR(NWS(),A,S,IER)
              IF(IFEHL(IER).NE.0) GOTO 900
              IER=IFEHL(IRUECK(DEWESC))
              CALL FDETR00(NWS(),DIK,RWERP,ITP,IER)
              DCROM=HUGE(1.D0)
              CALL RWWRTTR2(IRETR,D,NWS(),RP,RUR,A,S,IER)

C
C
C             Färbecharakteristik
C
C

              DO I=1,IM
                 FCA(I,1)=FAECHAR(I,1,L2)*0.01*RWERP(1)%CAMP(3)/
     &                   (RWERP(1)%CAMP(2)+TINY(1.))
              END DO
              CALL FLAQLN(IRT,KW)
              DO I=1,IM
                DO J=2,IV
                  FAECHAR(I,J,L2)=FCA(I,J)
                END DO
              ENDDO
C
C
           ELSEIF(KFIT.EQ.1) THEN
C
             IF(QUABSTR(L)%CART(3:4).NE.'AF'.OR.
     &          QUABSTR(LL)%CART(3:4).NE.'DF')THEN
                IF(IFEHL(4164).NE.0) GOTO 900
             ENDIF
C
C             Neue Dicke
C

             QUABSTR(L)%CAMP(0)=QUABSTR(L)%CAMP(1)
     &       *0.01*QUABSTR(L)%CAMP(3)/(QUABSTR(L)%CAMP(2)+TINY(1.))
             QUABSTR(LL)%CAMP(0)=QUABSTR(LL)%CAMP(1)
     &       *0.01*QUABSTR(LL)%CAMP(3)/(QUABSTR(LL)%CAMP(2)+TINY(1.))
             D(1)=QUABSTR(L)%CAMP(0)
             D(2)=QUABSTR(LL)%CAMP(0)

C
C            Remission für vorgegebene Dicke und vorgegebene optische Daten
C
C
             DIK=D(1)
             DO I=1,NWS()
              A(I)=RABSTR(I,KW,L)
              S(I)=RABSTR(I,KW,LL)
             ENDDO
             CALL FDETR00(NWS(),DIK,RWERP,ITP,IER)
             CALL RWWRTTR2(IRETR,D,NWS(),RP,RUR,A,S,IER)
             DCROM=HUGE(1.D0)
           ELSE
C
C
C
C
C
C
C
C             BERECHNUNG DER ABSORPTIONS- UND STREUWERTE AUS SCHICHTEN UEBER
C             WEISSEM UND SCHWARZEM UNTERGRUND
C
              CALL ABSSTRTR(NWS(),A,S,IER)
              IF(IFEHL(IER).NE.0) GOTO 900

              IER=IFEHL(IRUECK(DEWESC))

              CALL FDETR(NWS(),DIK,RWERP,ITP,IER)
C
C             MAXIMALES CHROMA FÜR MESSUNG ÜBER WEISSEM UNTERGRUND
C
C
C
              DIMIN=0.0
              DIMAX=DIKA*10000.
              WRT= BRENT(DIMIN,DIKA,DIMAX,FUCHROM,1.D-3,DMAXK)
              DCROM=ABS(DMAXK)
              DO I=1,NWS()
                 RCRMAX(I,KW,L2)=RCROM(I)
              END DO
           ENDIF
           IF(IFEHL(IER).NE.0) GOTO 900
C
           DO J=1,2
             LJ=L+J-1
             IF(.NOT.COTYRWRT(TYREDAB(LJ),TYREDAM(LJ))) THEN
               TYREDAB(LJ)=TYREDAM(LJ)
               TYABSTR(LJ)=TYREDAM(LJ)
               QUREDAB(LJ)=QUREDAM(LJ)
               QUABSTR(LJ)=QUREDAM(LJ)
               QUREDAB(LJ)%CART(3:3)='B'
               IF(J.EQ.1) THEN
               	 TYRUNND(L2)=TYREDAM(LJ)
               	 QURUNND(L2)=QUREDAM(LJ)
               	 QURUNND(L2)%CART(3:3)='U'
               	 TYCRMAX(L2)=TYREDAM(LJ)
                 QUCRMAX(L2)=QUREDAM(LJ)
                 QUCRMAX(L2)%CART(3:3)='C'
               ENDIF
               IF(J.EQ.1) THEN
                 QUABSTR(LJ)%CART(3:4)='AF'
               ELSE
                 QUABSTR(LJ)%CART(3:4)='DF'
               ENDIF
             ENDIF
           ENDDO
C
C
C          Unendlich dicke Schicht
C
           DH(1)=D(1)*10000.0
           DH(2)=D(1)*10000.0
           CALL RWWRTTR2(IRETR,DH,NWS(),RUN,RUR,A,S,IER)
C
C
C

           CALL LITRDK(ITP,DIKA,DIK,DEWESC,
     &                RWERP,PARAM(LP),WERT(1,LP))

C
           DO I=1,NWS()
              RDAB(I,KW,L)=RP(I,1)

              RDAB(I,KW,LL)=RP(I,2)

C   
C          REFLEXION UNENDLICH DICK
C
              RUNND(I,KW,L2)=RUN(I,1)
C
C
              RABSTR(I,KW,L)=A(I)
              RABSTR(I,KW,LL)=S(I)
           ENDDO
           PARAM(LP)%ITP=ITP
           PARAM(LP)%KWB=1
           PARAM(LP)%RETR=RWERP(1)%RETR
           PARAM(LP)%NR=RWERP(1)%NR
           PARAM(LP)%LNR=L2-1
           PARAM(LP)%ID=RWERP(1)%ID
           PARAM(LP)%AuswID=IPRG
           PARAM(LP)%CMETH(1:4)=RWERP(1)%CART
           PARAM(LP)%CMETH(5:5)=CHAR(KW+48)
           PARAM(LP)%CMETH(6:6)=CHAR(49)
C
C
C
C
      ENDDO
C
 900  CONTINUE
      CALL GETFEH(FEHL)
      DEALLOCATE(RZ1,RZ2,STAT=IER)
      IF(ALLOCATED(FCA)) THEN
        DEALLOCATE(FCA,STAT=IER)
      ENDIF
      RETURN
  98  IER=4021
      IF(IFEHL(IER).NE.0) GOTO 900
      GOTO 900
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
C*****************************************  DEKDEK  ************************************************************************
C***************************************************************************************************************************
C***************************************  05.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c
c
      SUBROUTINE DEKDEKZW(KFIT,KW,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QURUNND,TYRUNND,RUNND,QUCRMAX,TYCRMAX,RCRMAX,
     &                  QUABSTR,TYABSTR,RABSTR,QUREDUN,TYREDUN,RDUN,
     &                  QUASUN,TYASUN,RASUN,
     &                  NFAECHAR,NFAEWRT,FAECHAR,NPAM,PARAM,WERT,FEHL)

      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KFIT,NWEL,KML,NQU,NPAM,KW,NFAECHAR,NFAEWRT
      LOGICAL(KIND=4) :: COTYRWRT

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
      TYPE(QURWERT) QUREDAM(*),QUREDUN(*),QUREDAB(*),
     &                QUABSTR(*),QURUNND(*),QUCRMAX(*),QUASUN(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDUN(*),TYREDAB(*),
     &                TYABSTR(*),TYRUNND(*),TYCRMAX(*),TYASUN(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAM,RDAB,RUNND,
     &                                      RCRMAX,RABSTR,RDUN,RASUN
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*)::WERT
      REAL(KIND=8),DIMENSION(2) :: DEWESC
      TYPE(SRWERT) RWERP(2)
      REAL(KIND=8),TARGET,ALLOCATABLE,DIMENSION(:) ::RZ1,RZ2
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NFAECHAR,NFAEWRT,*) ::FAECHAR
      CHARACTER*6 BLAN
      EXTERNAL FUCHROM
      DATA BLAN/'      '/
C
C
C
C
      CALL FEHINI()
      IER=0
      IPRG=104
      ALLOCATE(RZ1(NWEL),RZ2(NWEL),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      RWERP(1)%R=>RZ1
      RWERP(2)%R=>RZ2
C 
C
      CALL SRT000(RWERP(1))
      CALL SRT000(RWERP(2))
C
C
C
C     Faerbecharacteristik FCA für MODQUAL
C
C
C
      IF(KFIT.EQ.2) THEN
         IM=NFAECHAR
         IV=NFAEWRT
         ALLOCATE(FCA(NFAECHAR,IV),STAT=IER)
      ENDIF
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
C
C
C
C 
C
      CALL TSTIPRG(IPRG,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
      CALL QUAKOR('D')
c
c
      DO J=1,NPAM
         PARAM(J)%ID=-1
         PARAM(J)%NR=-1
         PARAM(J)%LNR=-1
         PARAM(J)%AuswID=-1
         PARAM(J)%ITP=0
         PARAM(J)%CMETH=BLAN
         DO I=1,64
            WERT(I,J)=HUGE(1.)
         ENDDO 
      ENDDO
      DEWESC(1)=0.0
      DEWESC(2)=0.0


c
c
C
C              ABSORPTION UND STREUUNG FUER WEISS- UND SCHWARZPIGMENT
C
c
C
C     Nummer des Winkels
C
      KWC=KW
      IF(KW.LE.0.OR.KW.GT.KML) THEN
        IF(IFEHL(4165).NE.0) GOTO 900
      ENDIF
      IRETRA=0
      IRT=IRETRA
C
c
C
C
C
C     Berechnung der Streu- und Absorptionswerte für Weißpigment und Schwarzpigment
C     (COMMON COFDK)
       CALL FVODE(KW,NWEL,KML,
     &            QUREDUN,TYREDUN,RDUN,QUASUN,TYASUN,RASUN,IER)
C
C
C
      DO KU=1,2
       DO I=1,NWS()
         IF(KU.EQ.1) THEN
            RUR(I,KU)=1.0
         ELSE
            RUR(I,KU)=0.0
         ENDIF
         RUM(I,KU)=RUMESS(RUR(I,KU),KW)
C
C
C
       ENDDO
      END DO

C
cc
c
c
c
      DO L=1,NQU,2
C
C
C       Gleiche Winkel
C
C       LAUFVARIABLE PARAM
         LP=L
         L2=L/2+1
C
C
            ITP=0
            IF(TYREDAM(L)%RETR.NE.IRETRA) THEN
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
            IF(TYREDAM(LL)%RETR.NE.IRETRA) THEN
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
            IF(L.EQ.1.AND.ITP.NE.1) THEN
               IER=4152
               IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF
C
           DO J=1,2
             LJ=L+J-1
             IF(.NOT.COTYRWRT(TYREDAB(LJ),TYREDAM(LJ))) THEN
               TYREDAB(LJ)=TYREDAM(LJ)
               TYABSTR(LJ)=TYREDAM(LJ)
               QUREDAB(LJ)=QUREDAM(LJ)
               QUABSTR(LJ)=QUREDAM(LJ)
               QUREDAB(LJ)%CART(3:3)='B'
               IF(J.EQ.1) THEN
                 TYRUNND(L2)=TYREDAM(LJ)
                 QURUNND(L2)=QUREDAM(LJ)
                 QURUNND(L2)%CART(3:3)='U'
                 TYCRMAX(L2)=TYREDAM(LJ)
                 QUCRMAX(L2)=QUREDAM(LJ)
                 QUCRMAX(L2)%CART(3:3)='C'
               ENDIF
               IF(J.EQ.1) THEN
                 QUABSTR(LJ)%CART(3:4)='AF'
               ELSE
                 QUABSTR(LJ)%CART(3:4)='DF'
               ENDIF
             ENDIF
           ENDDO
C 
C
C
C
C          BERECHNUNG DER STARTKONZENTRATION
C          RWERP(1).CAMP(1)        MENGE WEISSPASTE IN WEISSVERSCHNITT
C          RWERP(1).CAMP(2)        PROZENTIGKEIT (MAX. =1.) DER WEISSPASTE
C          RWERP(1).CAMP(3)        MENGE DER BUNTPASTE IN WEISSVERSCHNITT
C          RWERP(1).CAMP(4)        PROZENTIGKEIT DER BUNTPASTE
C 
C
C          RWERP(2).CAMP(1)        MENGE SCHWARZPASTE IN SCHWARZVERSCHNITT
C          RWERP(2).CAMP(2)        PROZENTIGKEIT (MAX=1.) DER SCHWARZPASTE
C          RWERP(2).CAMP(3)        MENGE BUNTPASTE IN SCHWARZVERSCHNITT
C          RWERP(2).CAMP(4)        PROZENTIGKEIT DER BUNTPASTE
C
C
C 
C
C
C
C              MENGE(BUNT)*PROZ(BUNT)*0.01
C          C= ------------------------------------------
C              MENGE(WEISS)*PROZ(WEISS)*0.01
C
C          entsprechend für schwarz
C
           CWW=RWERP(1)%CAMP(3)*RWERP(1)%CAMP(4)/
     &      (RWERP(1)%CAMP(1)*RWERP(1)%CAMP(2)+TINY(1.))
           IF(CWW.EQ.0.D0) GOTO 98
C
           CSS=RWERP(2)%CAMP(3)*RWERP(2)%CAMP(4)/
     &      (RWERP(2)%CAMP(1)*RWERP(2)%CAMP(2)+TINY(1.))
           IF(CSS.EQ.0.D0) GOTO 98
C
C
           CF(1)=CWW
           CF(2)=CSS
           CW(1)=1.
           CW(2)=0.
           CS(1)=0.
           CS(2)=1.
           DO I=1,NWS()
              RP(I,1)=RWERP(1)%R(I)
              RP(I,2)=RWERP(2)%R(I)
           END DO



C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

           IF(KFIT.EQ.2) THEN
C
C             Optische Daten berechnen
C
C
C
C
C             BERECHNUNG DER ABSORPTIONS- UND STREUWERTE FÜR JE EINEN WEISS- UND SCHWARZVERSCHNITT
C 
              CALL ABSSTRDE(NWS(),A,S,IER)
              IF(IFEHL(IER).NE.0) GOTO 900
              IER=IFEHL(IRUECK(DEWESC))
              DIK=1.0
              CALL FDEDK00(NWS(),DIK,RWERP,ITP,IER)
              DCROM=HUGE(1.D0)
              CALL RWWRTDE2(CF,NWS(),RP,RUR,A,S,IER)

C
C
C             Färbecharakteristik
C
C
              DO I=1,IM
                 FCA(I,1)=FAECHAR(I,1,L2)*0.01*RWERP(1)%CAMP(3)/
     &                   (RWERP(1)%CAMP(2)+TINY(1.))
              END DO
              CALL FLAQLN(IRT,KW)
              DO I=1,IM
                DO J=2,IV
                  FAECHAR(I,J,L2)=FCA(I,J)
                END DO
              ENDDO

           ELSEIF(KFIT.EQ.1) THEN
C
             IF(QUABSTR(L)%CART(3:4).NE.'AF'.OR.
     &          QUABSTR(LL)%CART(3:4).NE.'DF')THEN
                IF(IFEHL(4164).NE.0) GOTO 900
             ENDIF
C
C            Neue Konzentration
C
C
             CF(1)=QUABSTR(L)%CAMP(3)*QUABSTR(L)%CAMP(4)/
     &        (QUABSTR(L)%CAMP(1)*QUABSTR(L)%CAMP(2)+TINY(1.))
             IF(CWW.EQ.0.D0) GOTO 98
C
             CF(2)=QUABSTR(LL)%CAMP(3)*QUABSTR(LL)%CAMP(4)/
     &        (QUABSTR(LL)%CAMP(1)*QUABSTR(LL)%CAMP(2)+TINY(1.))
             IF(CSS.EQ.0.D0) GOTO 98

C
C            Remission für vorgegebene Dicke und vorgegebene optische Daten
C

             DO I=1,NWS()
              A(I)=RABSTR(I,KW,L)
              S(I)=RABSTR(I,KW,LL)
             ENDDO
             DIK=1.0
             CALL FDEDK00(NWS(),DIK,RWERP,ITP,IER)
             DCROM=HUGE(1.D0)
             CALL RWWRTDE2(CF,NWS(),RP,RUR,A,S,IER)

           ELSE
C
C
C             Dicke für deckende Schicht
C
C
             CALL ABSSTRDE(NWS(),A,S,IER)
             IF(IFEHL(IER).NE.0) GOTO 900
             IER=IFEHL(IRUECK(DEWESC))

             DIKA=1.
             DIK=DIKA
             CALL FDEDK(NWS(),DIK,RWERP,ITP,IER)
             IF(IFEHL(IER).NE.0) GOTO 900

             CMIN=0.0D0
             CMAX=CF(1)*10000.
             CSTART=CF(1)
C
C            MAXIMALES CHROMA FÜR MESSUNG WEISSVERSCHNITT
C
C
             WRT= BRENT(CMIN,CSTART,CMAX,FUCHROM,1.D-3,DMAXK)
             DCROM=ABS(DMAXK)
             DO I=1,NWS()
C
C            REFLEXION MAXIMALES CHROMA
C
              RCRMAX(I,KW,L2)=RCROM(I)

             END DO
           ENDIF
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C
C
C
C          Unendlich dicke Schicht
C
           DUN=CF(1)*1.D6
           CALL RWWRTDE(1,DUN,NWS(),RH,RUR(1,1),A,S,GV,IER)

C
C
C
C 
           CALL LIDEDK(ITP,DIKA,DIK,DEWESC,
     &                RWERP,PARAM(LP),WERT(1,LP))
C
           DO I=1,NWS()
              RDAB(I,KW,L)=RP(I,1)
              RDAB(I,KW,L+1)=RP(I,2)
C   
C          REFLEXION UNENDLICH DICK
C
              RUNND(I,KW,L2)=RH(I)
              RABSTR(I,KW,LP)=A(I)
              RABSTR(I,KW,LP+1)=S(I)
           ENDDO
           PARAM(LP)%ITP=ITP
           PARAM(LP)%KWB=1
           PARAM(LP)%RETR=RWERP(1)%RETR
           PARAM(LP)%NR=RWERP(1)%NR
           PARAM(LP)%LNR=L2-1
           PARAM(LP)%ID=RWERP(1)%ID
           PARAM(LP)%AuswID=IPRG
           PARAM(LP)%CMETH(1:4)=RWERP(1)%CART
           PARAM(LP)%CMETH(5:5)=CHAR(KW+48)
           PARAM(LP)%CMETH(6:6)=CHAR(49)
C
C
      ENDDO
c
 900  CONTINUE
      CALL GETFEH(FEHL)
      DEALLOCATE(RZ1,RZ2,STAT=IER)
      IF(ALLOCATED(FCA)) THEN
        DEALLOCATE(FCA,STAT=IER)
      ENDIF
      RETURN
  98  IER=4021
      IF(IFEHL(IER).NE.0) GOTO 900
      RETURN
c
c     
      END

C
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################
C##############################################################################

