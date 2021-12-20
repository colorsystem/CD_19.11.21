      INCLUDE "GETLWGK.FOR"
      INCLUDE "FRBMEN.FOR"
c      INCLUDE "FAKER4.FOR"
C
C
C     Last change: KU 12.04.2020 13:24:37
c
c
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  MISCHEN ************************************************************************
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
      SUBROUTINE MISCHEN(IWESCL,NWEL,KML,NQU,
     &                  QUREDAM,TYREDAM,RDAM,QUREDAB,TYREDAB,RDAB,
     &                  QUREDUN,TYREDUN,RDUN,
     &                  NPAM,PARAM,WERT,FEHL)
      USE MOTFEHL
      USE MOTWERT
      USE MOSRWRT
      USE MOTTYRW
      USE MOTQURW

C
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C    
C     MISCHEN UND NUANCIEREN VON PARTIEN ZUM ANGLEICH AN DEN TYP
C     (Es können mehrere Typen verwendet werden;
C     die dem jeweiligen Typ nachgeordneten Partien werden an diesen Typ angepasst)
C 
C
C
      INTEGER*4 NQU,NPAM,NWEL,KML,IWESCL
C
C 
C     BERECHNUNG VON FARBKOORDINATEN UND FARBABSTAENDEN 
C
C     NQU ANZAHL R-Structuren
C     NPAM maximale Anzahl PARAM-Structuren
C     IWESC=1 nur Messungen für Weiß
C     IWESC=2 Messungen für Weiß und Schwarz
C 
C 
C
C
C      QUREDUN,TYREDUN,RDUN werden nicht verwendet  !!!
C
C
C
C
C 
C 
C 
C 
C
      TYPE(QURWERT) QUREDAM(*),QUREDAB(*),QUREDUN(*)
      TYPE(TYRWERT) TYREDAM(*),TYREDAB(*),TYREDUN(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*)::RDAM,RDAB,RDUN
      TYPE(TYPARAM) PARAM(*)
      REAL(KIND=8),DIMENSION(64,*)::WERT
      TYPE(TYFEH) FEHL
      TYPE(SRWERT),DIMENSION(NQU,IWESCL) :: RWERT
      TYPE(SRWERT),DIMENSION(NQU,IWESCL) :: RWERP
      REAL(KIND=8),DIMENSION(NWEL,KML,NQU,IWESCL)  :: RWEP,RWET
      TYPE(SRWERT) RERPL(2,IWESCL),RWTYP(IWESCL)
      REAL(KIND=8),DIMENSION(NWEL,KML,2,IWESCL) :: RRPL
      REAL(KIND=8),DIMENSION(NWEL,KML,IWESCL) :: RTYP

      INTEGER(KIND=4) :: IPRN,IWESC
      CHARACTER*6 BLAN
      DATA BLAN/'      '/
      DATA IPRN/106/
C
C
C
C
      INTERFACE
      INTEGER(KIND=4) FUNCTION DIMIER(NWEL,KML,NLZL,NPQGL,MNF,NUA)
      INTEGER(KIND=4) :: NWEL
      INTEGER(KIND=4),OPTIONAL :: KML,NLZL,NPQGL,MNF,NUA
      END FUNCTION
      END INTERFACE
C
      DLL_EXPORT MISCHEN
C
      IER=0
C
      IF(IFEHL(DIMIER(NWEL,KML=KML)).NE.0) THEN
         GOTO 900
      ENDIF
C
      CALL FEHINI()
C
C
      CALL TSTIPRG(IPRN,IER)
      IF(IFEHL(IER).NE.0) GOTO 900
      IWESC=IWESCL
      NPPP=NPAM
      NQUU=NQU

C
C



C 
      DO J=1,NPPP
         PARAM(J)%ID=-1
         PARAM(J)%NR=-1
         PARAM(J)%LNR=-1
         PARAM(J)%AuswID=IPRN
         PARAM(J)%ITP=0
         PARAM(J)%CMETH=BLAN
         DO I=1,64
           WERT(I,J)=HUGE(1.D0)
         ENDDO 
      ENDDO
C
C     QUREDAM.CAMP(1)      MENGE(UNTEN)
C     QUREDAM.CAMP(2)      PROZENTIGKEIT (0 - 100)
C     QUREDAM.CAMP(3)      MENGE(OBEN)
C
C
C     PARAM.WERT(1)        MENGE(UNTEN)
C     PARAM.WERT(2)        PROZENTIGKEIT (0 - 100)
C     PARAM.WERT(3)        MENGE(RECHNUNG)
C     PARAM.WERT(4)        MENGE(OBEN)
C
      NPT=0
      NPA=0
      LNR=-1
      DO L=1,NQUU,IWESC
C
C
C
         IF(QUREDAM(L)%CART(1:2).EQ.'@T') THEN
              ITP=1
         ELSEIF (QUREDAM(L)%CART(1:2).EQ.'@P') THEN
              ITP=2 
         ELSE
              CYCLE
         ENDIF
C
         IF(ITP.EQ.1) THEN
C
           DO KW=1,KMS()
            CALL GETKPF(KW,TYREDAM(L),QUREDAM(L),RWTYP(1),IER)
            CALL GETRWRT(NWEL,RDAM(1,KW,L),RTYP(1,KW,1))
           ENDDO

           IF(IFEHL(IER).NE.0) GOTO 900
           IF(RWTYP(1)%CART(4:4).NE.'W') THEN
               IER=4113
               IF(IFEHL(IER).NE.0) GOTO 900
           ENDIF
           IF(IWESC.EQ.2) THEN
            LL=L+1
            DO KW=1,KMS()
            CALL GETKPF(KW,TYREDAM(LL),QUREDAM(LL),RWTYP(2),IER)
            CALL GETRWRT(NWEL,RDAM(1,KW,LL),RTYP(1,KW,2))
            ENDDO
            IF(IFEHL(IER).NE.0) GOTO 900
            IF(RWTYP(2)%CART(4:4).NE.'S') THEN
               IER=4113
               IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF
           ENDIF

         ELSEIF(ITP.EQ.2) THEN
           NPT=NPT+1
           RWERT(NPT,1)=RWTYP(1)
           DO KW=1,KMS()
            DO I=1,NWEL
               RWET(I,KW,NPT,1)=RTYP(I,KW,1)
            END DO
            CALL GETKPF(KW,TYREDAM(L),QUREDAM(L),RWERP(NPT,1),IER)
            CALL GETRWRT(NWEL,RDAM(1,KW,L),RWEP(1,KW,NPT,1))

           ENDDO
           IF(IFEHL(IER).NE.0) GOTO 900
           IF(RWERP(NPT,1)%CART(4:4).NE.'W') THEN
               IER=4113
               IF(IFEHL(IER).NE.0) GOTO 900
           ENDIF
           IF(IWESC.EQ.2) THEN
            RWERT(NPT,2)=RWTYP(2)
            LL=L+1
            DO KW=1,KMS()
            DO I=1,NWEL
               RWET(I,KW,NPT,2)=RTYP(I,KW,2)
            END DO
            CALL GETKPF(KW,TYREDAM(LL),QUREDAM(LL),RWERP(NPT,2),IER)
            CALL GETRWRT(NWEL,RDAM(1,KW,LL),RWEP(1,KW,NPT,2))
            ENDDO
            IF(IFEHL(IER).NE.0) GOTO 900
            IF(RWERP(NPT,2)%CART(4:4).NE.'S') THEN
               IER=4113
               IF(IFEHL(IER).NE.0) GOTO 900
            ENDIF
           ENDIF
         ENDIF
         NPA=NPA+1
         LNR=LNR+1
         PARAM(NPA)%ITP=ITP
         PARAM(NPA)%NR=TYREDAM(L)%NR
         PARAM(NPA)%LNR=LNR
         PARAM(NPA)%RETR=TYREDAM(L)%RETR
         PARAM(NPA)%KWB=TYREDAM(L)%KWB
         PARAM(NPA)%ID=TYREDAM(L)%ID
C
C        UNTERE GRENZE
C
         WERT(1,NPA)=QUREDAM(L)%CAMP(1)
C
C        OBERE GRENZE
C
         WERT(2,NPA)=QUREDAM(L)%CAMP(3)
C
C        PROZENTIGKEIT
C
         WERT(4,NPA)=QUREDAM(L)%CAMP(2)
         PARAM(NPA)%CMETH(1:4)=QUREDAM(L)%CART
         PARAM(NPA)%CMETH(5:5)='$'
         PARAM(NPA)%CMETH(6:6)='$'
         IF(IWESC.EQ.2) THEN
            NPA=NPA+1
            PARAM(NPA)%ITP=ITP
            PARAM(NPA)%NR=TYREDAM(L+1)%NR
            PARAM(NPA)%LNR=LNR
            PARAM(NPA)%RETR=TYREDAM(L+1)%RETR
            PARAM(NPA)%KWB=TYREDAM(L+1)%KWB
            PARAM(NPA)%ID=TYREDAM(L+1)%ID
C
C           UNTERE GRENZE
C
            WERT(1,NPA)=QUREDAM(L+1)%CAMP(1)
C
C           OBERE GRENZE
C
            WERT(2,NPA)=QUREDAM(L+1)%CAMP(3)
C
C           PROZENTIGKEIT
C
            WERT(4,NPA)=QUREDAM(L+1)%CAMP(2)
            PARAM(NPA)%CMETH(1:4)=QUREDAM(L+1)%CART
            PARAM(NPA)%CMETH(5:5)='$'
            PARAM(NPA)%CMETH(6:6)='$'
         ENDIF
      ENDDO
C

C 
C 
C
C
      CALL MIMSH(NWEL,KML,IWESC,NPT,NQUU,RWERT,RWET,RWERP,RWEP,
     &           RERPL,RRPL,IER)
      IF(IFEHL(IER).NE.0) THEN
         GOTO 900
      ENDIF
C

      DO K=1,IWESC
      	DO  I=1,2
           J=(I-1)*IWESC+K
           TYREDAB(J)=TYREDAM(J)
           QUREDAB(J)=QUREDAM(J)
           QUREDAB(J)%CART(3:3)='B'
           DO LK=1,KMS()
             DO LI=1,NWS()
                RDAB(LI,LK,J)= RRPL(LI,LK,I,K)
             END DO
           END DO
      	END DO
      END DO
      DO I=1,NPA,IWESC
           IF(PARAM(I)%ITP.EQ.1) THEN
             DO K=1,NPT
             	    IF(PARAM(I)%NR.EQ.RWERT(K,1)%NR) THEN
C
C                     Rechnung Typ weiß (Gesamtmenge)
C
                      WERT(3,I)=RWERT(1,1)%CAMP(4)
                      EXIT
                    ENDIF
             ENDDO
           ELSEIF(PARAM(I)%ITP.EQ.2) THEN
             DO K=1,NPT
                    IF(PARAM(I)%NR.EQ.RWERP(K,1)%NR) THEN
C
C                     Rechnung Mischung weiß (Menge der NPT-ten Partie)
C
             	      WERT(3,I)=RWERP(K,1)%CAMP(4)
                      EXIT
                    ENDIF
             ENDDO
           ENDIF
           IF (IWESC.EQ.2) THEN
             IF(PARAM(I+1)%ITP.EQ.1) THEN
              DO K=1,NPT
                    IF(PARAM(I+1)%NR.EQ.RWERT(K,2)%NR) THEN
C
C                      Rechnung Typ schwarz (Gesamtmenge=Gesamtmenge (weiß))
C
             	       WERT(3,I+1)=RWERT(1,2)%CAMP(4)
                       EXIT
                    ENDIF
              ENDDO
             ELSEIF(PARAM(I+1)%ITP.EQ.2) THEN
              DO K=1,NPT
                    IF(PARAM(I+1)%NR.EQ.RWERP(K,2)%NR) THEN
C
C                      Rechnung Mischung schwarz (Menge der NPT-ten Partie=Menge der NPT-ten Partie(weiß))
C
             	       WERT(3,I+1)=RWERP(K,2)%CAMP(4)
                       EXIT
                    ENDIF
              ENDDO
             ENDIF
           ENDIF
      ENDDO
      NPAM=NPA
 900  CALL TERMFDM(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 999
      ENDIF
 999  CALL GETFEH(FEHL)
      RETURN
c     
      END
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************
C******************************************************
C******************************************************************************
C******************************************************************************
C
C 
      SUBROUTINE MIMSH(NWEL,KML,IWESC,NPT,NQUU,RWERT,RWET,RWERP,RWEP,
     &                 RERPL,RRPL,IER)
      USE MOSRWRT

C
C 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      EXTERNAL FUNMSH,MATMSH
C
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::X
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::XLB
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::XUB
      DATA MAXFUN/50/,ITT/15/,FTT/1.D-6/
C
C 
C 
C 
C     MISCHEN VON PRODUKTIONSPARTIEN                          
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
      TYPE(SRWERT) RWERT(NQUU,*),RWERP(NQUU,*),RERPL(2,*)
      REAL(KIND=8),DIMENSION(NWEL,KML,NQUU,*) :: RWEP,RWET
      REAL(KIND=8),DIMENSION(NWEL,KML,2,*) :: RRPL
C
C
C
C
      ALLOCATE(X(NPT),XLB(NPT),XUB(NPT),STAT=IER)
      IER=IERALC(IER)
C
C
      AGMIN=RWERT(1,1)%CAMP(1)
      AGMAX=RWERT(1,1)%CAMP(3)
C
C     STARTBERECHNUNGEN
C
C
      CALL STRMSH(NWEL,KML,IWESC,NPT,NQUU,RWERT,RWET,RWERP,RWEP,
     &            AGMIN,AGMAX,IER)
C
C
      MAL=3*NLS()*KMS()*IWESC
      MGAL=2
      MEAL=0
C
C
C     GRENZEN
C
      DO I=1,NPT
         XLB(I)=RWERP(I,1)%CAMP(1)
         XUB(I)=RWERP(I,1)%CAMP(3)
         X(I)=XUB(I)
      END DO
      CALL NLDFIT(MAL,MGAL,MEAL,NPT,ITT,MAXFUN,FTT,XLB,XUB,
     &                 X,FVALUE,IFAIL,FUNMSH,MATMSH)
      IER=IFAIL
      CALL ENDMSH(NWEL,KML,IWESC,NPT,X,NQUU,RWERT,RWET,RWERP,RWEP,
     &            RERPL,RRPL,IER)

 900  DEALLOCATE(X,XLB,XUB,STAT=IER)
      RETURN
      END
C
C
C
C
C
      SUBROUTINE STRMSH(NWEL,KML,IWESC,NPT,NQUU,RWERT,RWET,RWERP,RWEP,
     &                  AGMIN,AGMAX,IER)
      USE MOSRWRT
      USE MODFUNC,ONLY:WLCH,JABST
      USE MODWINK,ONLY:FWI
      USE MODILLU,ONLY:FA

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE C,PROZ,GG,FREF,AMEAX,AMEIN
C
C
C
C
C 
C 
C 
C     MISCHEN VON PRODUKTIONSPARTIEN                          
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
      DIMENSION GHI(3),X(*),XL(*),XU(*),F(*),A(MDA,*)
C
      REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE :: GG
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::C
      REAL(KIND=8),DIMENSION(:),ALLOCATABLE ::PROZ
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:)  :: FREF
      REAL(KIND=8),DIMENSION(NWEL,KML,IWESC)  :: RRU,RTU

C 
C 
      DIMENSION XYZT(3)
C 
C 
C 
C 
C 
C 
C 
      TYPE(SRWERT) RWERT(NQUU,*),RWERP(NQUU,*),RERPL(2,*)
      REAL(KIND=8),DIMENSION(NWEL,KML,NQUU,*) :: RWEP,RWET
      REAL(KIND=8),DIMENSION(NWEL,KML,2,*) :: RRPL

      DATA TOL/1.D-3/,RANGIN/1.D-35/,EPS/1.D-80/
C
C 
      AMEAX=AGMAX
      AMEIN=AGMIN
      TPS=TOL-GLANZ(1)
      NWE=NWS()
      NDIM=3*KMS()*NLS()*IWESC
C
C
C
C
C
C
C
      ALLOCATE(GG(NDIM,NPT),C(NPT),PROZ(NPT),FREF(NWEL,3),STAT=IER)
      IER=IERALC(IER)
      IF(IER.NE.0)THEN
         GOTO 900
      ENDIF
C
C
C
C
C
C     AUFBAU DER MATRIX GG=dFarbwert/dc
      DO  I=1,NPT
         PROZ(I)=0.01*RWERP(I,1)%CAMP(2)+EPS
C
         JJ=0
C
         DO IP=1,IWESC
         DO K=1,NLS()
         DO KW=1,KMS()
           IRT=RWERT(I,IP)%RETR
           GKK=GLZNOG(KW,IRT)
           CALL NOGXX(XYZT,K,RWET(1,KW,I,IP),GKK)
           CALL ABWIN(JABST,XYZT,SPHI,CPHI,K)

           CALL GEWREF(JABST,NWE,KW,IRT,RWET(1,KW,I,IP),NWE,FREF,
     &                 K,AKAS())
             DO KK=1,3
               SUX=0.
               DO J=1,NWE
                 HII= RSCHL(AKAS(),RWEP(J,KW,I,IP),KW)-
     &                RSCHL(AKAS(),RWET(J,KW,I,IP),KW)
                 HII=FREF(J,KK)*HII
                 SUX =SUX +HII
               ENDDO
               GHI(KK)=FA(K)*FWI(KW)*SUX
             ENDDO
             JJ=JJ+1
             GG(JJ,I)=WLCH(1)*GHI(1)
             JJ=JJ+1
             GG(JJ,I)=WLCH(2)*( GHI(2)*CPHI+GHI(3)*SPHI)
             JJ=JJ+1
             GG(JJ,I)=WLCH(3)*(-GHI(2)*SPHI+GHI(3)*CPHI)
         ENDDO
         ENDDO
         ENDDO
C         WRITE(27,*) 'GG',(GG(JJ,I),JJ=1,3)
      ENDDO
      RETURN
C
C
C
C
C
C
      ENTRY FUNMSH(NPT,X,XL,XU,NZZ,MCON,F,IER)

C
C
C     BERECHNUNG DER RECHTEN SEITE FÜR MINIMIERUNG
C
C
C
      CSUM=EPS
      CSU=EPS
      DO J=1,NPT
         CSUM=CSUM+X(J)/PROZ(J)
         CSU=CSU+X(J)
      END DO
      DO J=1,NPT
         C(J)=X(J)/(PROZ(J)*CSUM)
      END DO
      DO I=1,NZZ
         SUU=0.
         DO J=1,NPT
            SUU=SUU+GG(I,J)*C(J)
         END DO
         F(I)=SUU
      END DO
      IF(MCON.NE.2) THEN
        IER=-1
        RETURN
      ENDIF
C
C     BERECHNUNG FÜR RANDBEDINGUNGEN
C
C

      F(NZZ+1)=AMEAX-CSU
      F(NZZ+2)=CSU-AMEIN
      RETURN
C
C
C
C
      ENTRY MATMSH(NPT,X,XL,XU,MDA,NZZ,MCON,F,A,IER)
C
C
C
C     MATRIX FÜR MINIMIERUNGSBEDINGUNGEN
C
C
C
      CSUM=1.D-60
      DO J=1,NPT
         CSUM=CSUM+X(J)/PROZ(J)
      END DO
      DO J=1,NPT
         C(J)=X(J)/(PROZ(J)*CSUM)
      END DO
C
C     UMRECHNUNG IN ABLEITUNGEN NACH X(=MENGE)
C
C
C
      DO I=1,NZZ
         DO J=1,NPT
            A(I,J)=GG(I,J)
         END DO
      END DO
      CALL CONMEN(NPT,C,CSUM,MDA,NZZ,A)
C
C
C     PROZ BERÜCKSICHTIGEN
C
C
      DO I=1,NZZ
         DO J=1,NPT
            A(I,J)=A(I,J)/PROZ(J)
         END DO
      END DO
C
C     MATRIX FÜR RANDBEDINGUNGEN
C
C
      DO I=1,NPT
         A(NZZ+1,I)=-1.
         A(NZZ+2,I)=1.
      END DO
      DO I=1,NZZ
        SUU=0.
       DO J=1,NPT
           SUU=SUU+A(I,J)*X(J)
       END DO
      END DO
      RETURN
C
C
C
C
      ENTRY ENDMSH(NWEL,KML,IWESC,NPT,X,NQUU,RWERT,RWET,RWERP,RWEP,
     &             RERPL,RRPL,IER)
C
      NWE=NWEL
      KM=KML
      TPS=TOL-GLANZ(1)

C
C
C     RUECKRECHNUNG
C
      DO IP=1,IWESC
        DO I=1,NPT
          RWERP(I,IP)%CAMP(4)=X(I)
        ENDDO
      ENDDO
      DO IP=1,IWESC
       DO KW=1,KM
       DO J=1,NWE
        SUP=0.
        SUT=0.
        SUX=0.
        AGG=0.
        DO I=1,NPT
           APROZ=PROZ(I)
           HII=RWERP(I,IP)%CAMP(4)
           SUP=SUP+HII*RSCHL(AKAS(),RWEP(J,KW,I,IP),KW)/APROZ
           SUT=SUT+HII*RSCHL(AKAS(),RWET(J,KW,I,IP),KW)/APROZ
           SUX=SUX+HII/APROZ
           AGG=AGG+HII
        ENDDO   
        HIP=SUP/(SUX+RANGIN)
        HIT=SUT/(SUX+RANGIN)
        RWERT(1,IP)%CAMP(4)=AGG
        RRU(J,KW,IP)=RRUECK(AKAS(),HIP,KW)
        RTU(J,KW,IP)=RRUECK(AKAS(),HIT,KW)

       ENDDO
      ENDDO
      ENDDO
C
      DO IP=1,IWESC
      	RERPL(1,IP)=RWERT(1,IP)
      	RERPL(2,IP)=RWERP(1,IP)

      	DO KW=1,KM
          DO I=1,NWE
             RRPL(I,KW,1,IP)=RTU(I,KW,IP)
             RRPL(I,KW,2,IP)=RRU(I,KW,IP)
          ENDDO
        ENDDO
      ENDDO

  900 IF(ALLOCATED(C)) THEN
        DEALLOCATE(C,PROZ,GG,FREF)
      ENDIF
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE GEWREF(JABST,NWE,KW,IRT,R,NDI,FREF,NN,AKA)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(*),FREF(NDI,3),RH(NWE)
      DIMENSION XYZV(3),XYZH(3)
      DATA DEL/1.E-3/
      GKK=GLZNOG(KW,IRT)
      CALL NOGXX(XYZV,NN,R,GKK)
      DO I=1,NWE
         RH(I)=R(I)
      ENDDO
      DO I=1,NWE
        RH(I)=RH(I)+DEL
        CALL NOGXX(XYZH,NN,RH,GKK)
        CALL DELABAL(JABST,XYZV,XYZH,DE,DL,DC,DH,DA,DB,NN)
        FREF(I,1)=DL/DEL 
        FREF(I,2)=DA/DEL 
        FREF(I,3)=DB/DEL 
        DO J=1,3
           RSCH=RSCHL(AKA,R(I),KW)
           FREF(I,J)=FREF(I,J)*DRDRSCH(AKA,RSCH,KW)
        ENDDO
        RH(I)=RH(I)-DEL       
      ENDDO
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

