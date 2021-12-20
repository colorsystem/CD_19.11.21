C     Last change:  KLA  27 Nov 114   10:11 am
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FASFOG  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
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
      SUBROUTINE FASFOG(IPRI,KW,NWEL,KML,NQU,KLH,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  NPAM,PARAM,WERT,FEHL)

c
c
c
      USE MODFOGR
      USE MODFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 NQU,NPAM,IPRI,KW,KLH,NWEL,KML
C 
C 
C 
C         
C
C 
C     BERECHNUNG VON FARBSTAERKEN MIT B_WERT-ANGLEICH                           
C 
C
C 
C     IPRI=1    DECKENDE SCHICHT
C     IPRI=2    TRANSPARENTE SCHICHT
C     IPRI=3    TEXTIL
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
      TYPE(QURWERT) QUREDAM(*),QUREDAB(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDAB(*)
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*):: RDAM,RDAB
      REAL(KIND=8),DIMENSION(3) :: XYZ
      REAL(KIND=8),DIMENSION(64,*)::WERT
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: YSO
      TYPE(SRWERT),ALLOCATABLE,DIMENSION(:) ::RWERP
      TYPE(SRWERT) RWERH
      TYPE(TYFEH) FEHL
      CHARACTER*6 BLAN
      DATA BLAN/'      '/
C
C
C
      DLL_EXPORT FASFOG
C
C
C 
C
      IER=0
      IPRG=111
C
C
C
C
      CALL TSTIPRG(IPRG,IER)
      IF(IFEHL(IER).NE.0) GOTO 900

C
C 
C

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
C
C
C
C
C     Nummer des Winkels
C
      KWC=KW
      NFAB=NQU+1
C
      ALLOCATE(CZWI(NFAB),RZWI(NWEL,NFAB),RWERP(NFAB),
     &         YSO(NFAB))
      DO I=1,NFAB
         ALLOCATE(RWERP(I)%R(NWEL))
      END DO
      DO J=1,NFAB
         CALL SRT000(RWERP(J))
      END DO
      ALLOCATE(RWERH%R(NWEL))
c
c
c
      ILH=0
      JZW=0
      NGES=0
c
      DO L=1,NQU+1
C
C
            IF(L.EQ.NQU+1.OR.
     &        (QUREDAM(L)%CART(1:1).EQ.'@'.AND.JZW.GT.0)) THEN
                 IF(JZW.LE.1) THEN
                    IER=4118
                    IF(IFEHL(IER).NE.0) GOTO 900
                 ENDIF
                 ILH=ILH+1
                 IF(ILH.GT.NFAB) THEN
                    IER=4117
                    IF(IFEHL(IER).NE.0) GOTO 900
                 ENDIF 

                 IF(TYREDAB(ILH)%NR.NE.TYREDAM(NGES+1)%NR) THEN
                   QUREDAB(ILH)=QUREDAM(NGES+1)
                   TYREDAB(ILH)=TYREDAM(NGES+1)
                   QUREDAB(ILH)%CART(3:3)='B'
                 ENDIF
C                
C
                 DO J=1,JZW
                    IF(RWERP(J)%CART(2:2).EQ.'T') THEN
                       ITP=1
                    ELSE
                       ITP=2
                    ENDIF
                    PARAM(NGES+J)%ITP=ITP
                    PARAM(NGES+J)%KWB=1
                    PARAM(NGES+J)%RETR=RWERP(J)%RETR
                    PARAM(NGES+J)%NR=RWERP(J)%NR
                    PARAM(NGES+J)%LNR=NGES+J-1
                    PARAM(NGES+J)%ID=RWERP(J)%ID
                    PARAM(NGES+J)%AuswID=IPRG
                    PARAM(NGES+J)%CMETH(1:4)=RWERP(J)%CART
                    PARAM(NGES+J)%CMETH(5:5)=CHAR(KW+48)
                    PARAM(NGES+J)%CMETH(6:6)=CHAR(49)
                    CALL LIFFO(KW,ITP,RWERP(J),
     &                         PARAM(NGES+J),WERT(1,NGES+J))

C
C
                 ENDDO

C
C                SORTIEREN
C
C
C
C 
                 DO K=1,JZW-1
                    DO J=K+1,JZW
                       IF(RWERP(J)%CAMP(0).LT.RWERP(K)%CAMP(0)) THEN
                          RWERH=RWERP(J)
                          RWERP(J)=RWERP(K)
                          RWERP(K)=RWERH
                       ENDIF
                    ENDDO
                 ENDDO

C
C                VERARBEITUNG DER ILH-TEN MESSREIHE
C
C
C
C    
                 JZWI=JZW
                 DO J=1,JZW
C
C
                    CZWI(J)=RWERP(J)%CAMP(0)
                    DO I=1,NWS()
                       RZWI(I,J)=RWERP(J)%R(I)
                    ENDDO
                    CALL NOGXX(XYZ,1,RZWI(1,J),GLANZ(KW))
                    YSO(J)=XYZ(2)
                 ENDDO
                 DO J=1,JZW-1
                    IF(YSO(J).LT.YSO(J+1)) THEN
                      IER=4166
                      IF(IFEHL(IER).NE.0) GOTO 900
                    ENDIF
                 END DO
C
C                 FARBTIEFEANGLEICH DURCH INTERPOLATION
C
C  

                  CONZ=CZWI(2)
                  CALL FBTFO(CONZ,CZWI(1),CZWI(JZWI),IER)
                  IF(IFEHL(IER).NE.0) GOTO 900
            
                  QUREDAB(ILH)%CAMP(0)=CONZ
                  DO I=1,NWS()
                     RDAB(I,KW,ILH)=RF(I)
                  ENDDO
                  NGES=NGES+JZW
                  JZW=0
             ENDIF
             IF(L.EQ.NQU+1) THEN
               EXIT
             ENDIF
C
C
C            MESSUNGEN UEBERNEHMEN
C
C

             ITP=0
             JZW=JZW+1
             IF(TYREDAM(L)%RETR.EQ.1) THEN
              IF(IFEHL(4146).NE.0) GOTO 900
             ENDIF
             CALL GETKPF(KW,TYREDAM(L),QUREDAM(L),RWERP(JZW),IER)
             CALL GETRWRT(NWEL,RDAM(1,KW,L),RWERP(JZW)%R)

             IF(IFEHL(IER).NE.0) GOTO 900
  
C
             IF(IPRI.EQ.1) THEN
C 
C              "DICKE"  (DECKENDE SCHICHTEN) (FAKTOR 0.01 FUER PROZENTIGK.
C                                              WURDE WEGGELASSEN)
C 
               RWERP(JZW)%CAMP(0)=RWERP(JZW)%CAMP(3)*RWERP(JZW)%CAMP(4)/
     &                (RWERP(JZW)%CAMP(1)*RWERP(JZW)%CAMP(2)+TINY(1.))
C
             ELSE IF(IPRI.EQ.2) THEN
C 
C             "DICKE"  (TRANSPARENTE SCHICHTEN)
C 
               RWERP(JZW)%CAMP(0)=RWERP(JZW)%CAMP(1)*0.01
     &                 *RWERP(JZW)%CAMP(3)/(RWERP(JZW)%CAMP(2)+TINY(1.))
C
             ELSE IF(IPRI.EQ.3) THEN
C 
C              "DICKE"  (TEXTIL)
C
               RWERP(JZW)%CAMP(0)=RWERP(JZW)%CAMP(1)
             ENDIF
C
C
      ENDDO
      KLH=ILH
c
 900  CONTINUE
      CALL GETFEH(FEHL)
      DEALLOCATE(RWERH%R)
      DO I=1,NFAB
        DEALLOCATE(RWERP(I)%R)
      END DO
      DEALLOCATE(CZWI,RZWI,RWERP,YSO)
      RETURN
c
c     
      END

C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FASNEU  ************************************************************************
C***************************************************************************************************************************
C***************************************  15.11.2007  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
      SUBROUTINE FASNEU(KW,NWEL,KML,KLH,
     &                  QUREDAB,TYREDAB,RDAB,
     &                  NPAM,PARAM,WERT,FEHL)

      USE MODFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC
      USE MODQUAL
      USE MODWINK,ONLY:KWC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C 
C 
C         
C
C 
C     BERECHNUNG VON FARBSTAERKEN MIT B_WERT-ANGLEICH                           
C     (VERGLEICH DER MESSREIHEN FUER B=0)
C
C 
C     IPRI=1    DECKENDE SCHICHT
C     IPRI=2    TRANSPARENTE SCHICHT
C     IPRI=3    TEXTIL
C 
C 
C 
C 
C
C     KLH ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C 
C 
C
      TYPE(QURWERT) QUREDAB(*)
      TYPE(TYRWERT) TYREDAB(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: RDAB
      INTEGER(KIND=4) KW,NWEL,KML,KLH,NPAM
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*)::WERT
      TYPE(SRWERT) RWERP
      TYPE(TYFEH) FEHL
      CHARACTER*6 BLAN
      DATA BLAN/'      '/
C
C
C
      DLL_EXPORT FASNEU
C
      IER=0
      IPRG=111
C
      ALLOCATE(RWERP%R(NWEL))
C
C
      CALL SRT000(RWERP)
      CALL TSTIPRG(IPRG,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
C
      CALL QUAKOR('T')
      IER=0
C
C
C
C
C
C 
C
c
c
      DO J=1,NPAM
         PARAM(J)%ID=-1
         PARAM(J)%NR=-1
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
c
c
c
      JZW=0
c
      DO L=1,KLH
C
C
C
C
C
C
             ITP=0
             JZW=JZW+1
c             CALL GETRWE(KW,1,NWEL,TYREDAB(L),QUREDAB(L),RDAB(1,KW,L),
c     &                  RWERP,IER)
            CALL GETKPF(KW,TYREDAB(L),QUREDAB(L),RWERP,IER)
            CALL GETRWRT(NWEL,RDAB(1,KW,L),RWERP%R)

             IF(IFEHL(IER).NE.0) GOTO 900
             IF(RWERP%CART(2:2).EQ.'T') THEN
                ITP=1
             ELSE IF(RWERP%CART(2:2).EQ.'P') THEN
                ITP=2
             ENDIF
             PARAM(L)%ITP=ITP
             PARAM(L)%KWB=1
             PARAM(L)%NR=RWERP%NR
             PARAM(L)%RETR=RWERP%RETR
             PARAM(L)%ID=RWERP%ID
             PARAM(L)%AuswID=IPRG
             PARAM(L)%CMETH(1:4)=RWERP%CART
             PARAM(L)%CMETH(5:5)=CHAR(KW+48)
             PARAM(L)%CMETH(6:6)=CHAR(49)
             CALL LIFFO(KW,ITP,RWERP,PARAM(L),WERT(1,L))
C
C
      ENDDO
c
 900  CONTINUE
      CALL GETFEH(FEHL)
      DEALLOCATE(RWERP%R)
      RETURN
c
c
      END

C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************


      SUBROUTINE LIFFO(KW,ITP,
     &                 RWERP,PARAM,WERT)
      USE MOTWERT
      USE MOSRWRT
      USE MODFUNC

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(3),XYZT(3),BA(8)
      DIMENSION DIA(6),CLAB(3),CIA(3)
      SAVE DT,XYZT        
C
C
      TYPE(SRWERT) RWERP
      TYPE(TYPARAM) PARAM
      REAL(KIND=8),DIMENSION(*)::WERT

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
            FREL=1.
            FAE=1.
            DT=RWERP%CAMP(0)
      ELSE
            FREL=DT/(RWERP%CAMP(0)+TINY(1.))
            FAE=RWERP%CAMP(0)/(DT+TINY(1.))
      ENDIF
C
C
C
C
C
C
C
C
C     FARBKOORDINATEN
C
C
C
      CALL NOGXX(XYZ ,1,RWERP%R(1),GLANZ(KW))
      IF(ITP.EQ.1) THEN
         DO I=1,3
            XYZT(I)=XYZ(I)
         ENDDO
      ENDIF
      DO KB=1,8
         CALL BWERT(XYZ ,KB,BA(KB),1)
      ENDDO
C
C
      CALL LCHAL(JABST,XYZ,CLAB,CIA,1)
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
      WERT(1)=RWERP%CAMP(0)
      WERT(2)=FREL
      WERT(3)=FAE
C
      WERT(4)=CIA(1)
      WERT(5)=CIA(2)
      WERT(6)=CIA(3)
      WERT(7)=CLAB(2)
      WERT(8)=CLAB(3)
      DO K=1,8
         WERT(8+K)=BA(K)
      ENDDO
      IF(ITP.EQ.2) THEN
         CALL DELABAL(JABST,XYZT,XYZ,  
     &        DIA(1),DIA(2),DIA(3),DIA(4),DIA(5),DIA(6),1)  
C
         DO K=1,6
            WERT(16+K)=DIA(K)
         ENDDO
      ELSE 
         IF(KABS.NE.0) THEN
            WERT(18)=CIA(1)
            WERT(19)=CIA(2)
            WERT(20)=CIA(3)
            WERT(21)=CLAB(2)
            WERT(22)=CLAB(3)
          ENDIF
      ENDIF  
C
C
C
C
      RETURN
      END
C******************************************************************************
C                           +-------+ 
C                           ! FBTFO ! 
C                           +-------+ 
      SUBROUTINE FBTFO(C,CMI,CMA,IER)
      USE MODQUAL

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C 
C 
      EXTERNAL FFFO 
C
C 
C 
C 
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
C            KONZENTRATION WIRD FUER BTYP=0. BESTIMMT 
           
             BTYP=0.
             NSCH=1000
             DCONZ=CMA/NSCH
             CMIN=CMI
             DO I=1,NSCH+1
                CMAX=CMIN+DCONZ
                IF (FFFO(CMIN)*FFFO(CMAX).LT.0.) THEN
                   EXIT
                ENDIF
                CMIN=CMAX
             END DO
             IF(I.GT.NSCH) THEN
               DCONZ=10.*CMA/NSCH
               CMIN=0.
               DO I=1,NSCH+1
                CMAX=CMIN+DCONZ
                IF (FFFO(CMIN)*FFFO(CMAX).LT.0.) THEN
                   EXIT
                ENDIF
                CMIN=CMAX
               END DO
             ENDIF
             IF(I.GT.NSCH) THEN
               CMIN=0.
               CMAX=10.*CMA
               IF (FFFO(CMIN)*FFFO(CMAX).GE.0.) THEN
                 IER=4143
                 RETURN
               ENDIF
             ENDIF
             CALL INTVAL(C,CMIN,CMAX,FFFO,IER)
             IF(IFEHL(IER).NE.0) GOTO 900
C 
  900 RETURN
      END 
C                            +-------+
C                            ! FFFO  !
C                            +-------+
      DOUBLE PRECISION FUNCTION FFFO(C)
      USE MODFOGR
      USE MODFUNC
      USE MODQUAL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

C 
      KW=KWS()
      KA=KAS()
C 
C
C     BERECHNUNG DER B-WERT DIFFERENZ BPROBE-BTYP 
C     AUS DER KONZENTRATION C 
C
C 
      DO 10 I=1,NWS()
         RF(I)=FFGRA(C,CZWI,NWS(),RZWI,JZWI,I,KA)
  10  CONTINUE  
C 
C 
C
      CALL NOGXX(XYZF,1,RF,GLANZ(KW))
C
C
C     B-WERT ANGLEICH (FIAF)
C
C
C
C
  13  CALL BWERT(XYZF,KBWT,B,1)   
      FFFO=B
      RETURN
      END 
C
C
C
C
      DOUBLE PRECISION FUNCTION FFGRA(C,CZWI,NWE,RZWI,JZWI,I,KA)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RZWI(NWE,*),CZWI(*)
C
C
C
      IF(C.LE.CZWI(1)) THEN
        IF(CZWI(1).GT.0.) THEN
            RO=RZWI(I,1)
            RU=1.
            CU=0.      
            CO=CZWI(1)
            GOTO 10
         ELSE
            RO=RZWI(I,2)
            RU=RZWI(I,1)
            CU=CZWI(1)
            CO=CZWI(2)
            GOTO 10
          ENDIF
      ENDIF
      IF(C.GE.CZWI(JZWI).AND.JZWI.GT.1) THEN
            RU=RZWI(I,JZWI-1)        
            RO=RZWI(I,JZWI)    
            CU=CZWI(JZWI-1)      
            CO=CZWI(JZWI)
            GOTO 10
      ENDIF
      DO K=1,JZWI-1
         IF(C.GE.CZWI(K).AND.C.LT.CZWI(K+1)) THEN
            RU=RZWI(I,K)
            RO=RZWI(I,K+1)
            CU=CZWI(K)
            CO=CZWI(K+1)
            GOTO 10
          ENDIF
      ENDDO
C
C
C
C
  10  IF(KA.EQ.3) THEN
         RU=1./(RU+TINY(1.))
         RO=1./(RO+TINY(1.))
      ELSE IF(KA.EQ.2) THEN
         RU=LOG(RU+TINY(1.))
         RO=LOG(RO+TINY(1.))
      ENDIF
      RHILF=(RO-RU)/(CO-CU+100.*TINY(1.))
      RHILF=RU+RHILF*(C-CU)
      IF(KA.EQ.1) THEN
         IF(RHILF.GT.3.) RHILF=3.
         FFGRA=RHILF
      ELSE IF(KA.EQ.2) THEN
         IF(RHILF.GT.1.1) RHILF=1.1
         FFGRA=EXP(RHILF)
      ELSE IF(KA.EQ.3) THEN
         IF(RHILF.LT.TINY(1.)) RHILF=TINY(1.)
         FFGRA=1./(RHILF+TINY(1.))
      ENDIF
      RETURN
      END
c
c
