C     Last change: KU 12.04.2020 13:47:10
C
C
      INCLUDE "GETLWGK.FOR"
      INCLUDE "FRBMEN.FOR"
c      INCLUDE "MATLIB.FOR"
c      INCLUDE "MSCHLIB.FOR"
C
C
C
c
c
c
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C***************************************** FARBUMRE ************************************************************************
C***************************************************************************************************************************
C***************************************  15.10.2004  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************
c

c
c


      SUBROUTINE FARBUMRE(NWEL,KML,JSCH,JFL,JFW,NLQ,FAKO,
     &           XYZSOLL,XYZIST,TYREINP,REINP,TYREDAT,REDAT,FEHL)
      USE MOTFEHL
      USE MOTTYRW
      USE MOTQURW
      USE MODGKWR,ONLY:GK
      USE MODFUNC,ONLY:ALP,JABST,RWGEW,DESCHW
C      
C 
C     BERECHNUNG VON REFLEXIONSWERTEN AUS FARBKOORDINATEN
C 
C
C 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C 
C
      INTEGER*4 JSCH,JFL,JFW
      TYPE(TYRWERT) TYREDAT,TYREINP(*)
      REAL(KIND=4),DIMENSION(NWEL,KML,*) :: REINP
      REAL(KIND=4),DIMENSION(NWEL,KML) :: REDAT
      REAL(KIND=4),DIMENSION(3,NLQ,KML):: XYZSOLL,XYZIST
      TYPE(TYFEH) FEHL
      REAL*4 FAKO(3*NLQ,*)
      REAL(KIND=8) ::EPP
      REAL(KIND=8) :: DES,DLS,DCS,DHS,DAS,DBS
C
C

C 
C   
      DIMENSION HILF(3*NLQ,KML),XYZ(3*NLQ),ALAB(3*NLQ),ALCH(3*NLQ)
      DIMENSION XYZH(3),ALAP(3)
      DIMENSION RR(NWEL,KML),RH(NWEL,KML),RT(NWEL,KML)
C
C
C
      INTERFACE
      INTEGER(KIND=4) FUNCTION DIMIER(NWEL,KML,NLZL,NPQGL,MNF,NUA)
      INTEGER(KIND=4) :: NWEL
      INTEGER(KIND=4),OPTIONAL :: KML,NLZL,NPQGL,MNF,NUA
      END FUNCTION
      END INTERFACE
      DATA EPP/1.D-3/
C
C 
C 
C
C
C
      DLL_EXPORT FARBUMRE
C
C
C
      CALL FEHINI()
C             
      IF(IFEHL(DIMIER(NWEL,KML=KML)).NE.0) THEN
         GOTO 900
      ENDIF

C
      IER=0
      RWALP=RWGEW*ALP
      ISCH=JSCH
      IFL=JFL
      IFW=JFW
      IF(IFL.NE.1.AND.ISCH.LE.3) THEN
        IER=4141
        IF(IFEHL(IER).NE.0) GOTO 900
      ENDIF
C
c
C     OPEN(27,FILE='OUTTEST.TXT')
      KM=KMS()
      NWE=NWS()
      NLZ=NLS()
      IF(NLZ.LT.NLQ) THEN
        IER=4008
        IF(IFEHL(IER).NE.0) GOTO 900
      ENDIF
      NLZ=NLQ
      DO KW=1,KM
        DO I=1,3*NLZ
            HILF(I,KW)=FAKO(I,KW)
        ENDDO
      END DO
C
C
      DO KW=1,KM
        DO  I=1,NWE
          RT(I,KW)=GLANZ(KW)
          RH(I,KW)=GINTE(KW)+GLANZ(KW)-GLINZ(KW)
          RR(I,KW)=0.0
        ENDDO
      ENDDO
C 
      IF(IFL.EQ.1) THEN
       IF(TYREINP(1)%RETR.EQ.1) THEN
          IF(IFEHL(4146).NE.0) GOTO 900
       ENDIF
       DO KW=1,KM
        GKK=GLANZ(KW)
        DO I=1,NWE
          RR(I,KW)=REINP(I,KW,1)
        ENDDO
        IF(ISCH.LE.3) THEN
           LK=1
           DO  K=1,NLZ
             CALL NOGXX(XYZ(LK),K,RR(1,KW),GKK)
             CALL LCHAL(JABST,XYZ(LK),ALAB(LK),ALCH(LK),K)
             LK=LK+3
           ENDDO     
           IF(ISCH.EQ.1) THEN
C
C             DX,DY,DZ
C
              DO I=1,3*NLZ
                HILF(I,KW)=XYZ(I)+HILF(I,KW)
              ENDDO     
           ELSE IF(ISCH.EQ.2) THEN
C
C             DL*,Da*,Db*/DL9,Da9,Db9
C
              DO I=1,3*NLZ
                HILF(I,KW)=ALAB(I)+HILF(I,KW)
              ENDDO
           ELSE IF(ISCH.EQ.3) THEN
C
C             DL*,DC*,DH*/DL9,DC9,DH9
C
              DO K=1,NLZ
                 KK=3*(K-1)
C
C                UMRECHNUNG nach L*,a*,b*(Probe)/L9,a9.b9 ALAP
C

                 CALL ALAPCLC(ALAB(KK+1),HILF(KK+1,KW),ALAP,IER)
                 DO I=1,3
                    II=KK+I
                    HILF(II,KW)=ALAP(I)
                 ENDDO
              ENDDO          
           ENDIF
        ENDIF
       ENDDO
      ELSE IF (IFL.EQ.2) THEN
         IF(TYREINP(2)%RETR.EQ.1) THEN
          IF(IFEHL(4146).NE.0) GOTO 900
         ENDIF
         IF(TYREINP(3)%RETR.EQ.1) THEN
          IF(IFEHL(4146).NE.0) GOTO 900
         ENDIF
         DO KW=1,KM
          DO I=1,NWE
            RT(I,KW)= REINP(I,KW,2)
            RH(I,KW)=REINP(I,KW,3)
          ENDDO
         ENDDO
C
      ENDIF
C
C
C
C
C
C     X,Y,Z SOLLWERTE BESTIMMEN
C
C
C
C
      DO KW=1,KM
        GKK=GLANZ(KW)
        IF(ISCH.EQ.1.OR.ISCH.EQ.4) THEN
              DO I=1,3*NLZ
                XYZ(I)=HILF(I,KW)
              ENDDO
        ELSE IF(ISCH.EQ.2.OR.ISCH.EQ.5.OR.ISCH.EQ.3) THEN
              DO K=1,NLZ
                KK=(K-1)*3+1
                CALL XYZLABAL(JABST,XYZ(KK),HILF(KK,KW),K,*910)
              ENDDO
        ELSE IF(ISCH.EQ.6) THEN
              DO  K=1,NLZ
                KK=(K-1)*3+1
                CALL XYZLCHAL(JABST,XYZ(KK),HILF(KK,KW),K,*910)
              ENDDO
        ENDIF
        DO K=1,NLZ
          KK=(K-1)*3
          DO I=1,3
            XYZSOLL(I,K,KW)=XYZ(KK+I)
          END DO
        END DO
        IF(IFW.EQ.2) THEN
          MM=3*NLZ
          CALL ENT(MM,XYZ,NWE,RR(1,KW),RT(1,KW),RH(1,KW),NLZ,IFL,KW,
     &                RWALP)
        ELSE
          CALL RXYZ(NWE,RR(1,KW),RT(1,KW),RH(1,KW),XYZ,
     &                   NLZ,IFL,KW,RWALP)
        ENDIF
        DO I=1,NWE
           REDAT(I,KW)=RR(I,KW)
        ENDDO
C
C       Rückrechnung
C
C
        DO K=1,NLZ
          CALL NOGXX(XYZH,K,RR(1,KW),GKK)
          DO L=1,3
           XYZIST(L,K,KW)=XYZH(L)
c           IF(ABS(XYZH(L)-XYZ((K-1)*3+L)).GT.EPP) THEN
c             IER=-4170
c           ENDIF
           CALL DELABAL(JABST,XYZH,XYZ((K-1)*3+1),
     &                  DES,DLS,DCS,DHS,DAS,DBS,K)
           IF(DES.GT.DESCHW) THEN
             IER=-4099
           ENDIF
          END DO
        END DO

      ENDDO
      IF(IFEHL(IER).NE.0) THEN
         GOTO 900
      ENDIF
  900 CALL GETFEH(FEHL)
C      CLOSE(27)
      CALL TERMFDM(IER)
      RETURN

  910 IER=4140
      IF(IFEHL(IER).NE.0) GOTO 900
      RETURN
      END
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
      SUBROUTINE RXYZ(NWE,RR,RT,RH,XYZ,NL,IFL,KW,ALP)
      USE MODILLU,ONLY:FA,EILLU
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(NWE+3*NL,NWE+1)
      DIMENSION RW(6*NWE),IND(NWE),IW(2*NWE),IOPT(17),X(2*(NWE+3)+17)
      DIMENSION RR(*),RT(*),RH(*),XYZ(*),RHLF(NWE)
C      DATA IOP/0/
C
C
C     XYZ   VORGEGEBENE TRISTIMULUSWERTE
C     RR    REFLEXIONSWERTE, DIE MOEGLICHST GUT ERREICHT WERDEN SOLLEN (EING.)
C           ERGEBNIS (AUSG)
C     RT    UNTERE GRENZE FUER REFLEXIONSWERTE
C     RH    OBERE GRENZE FUER REFLEXIONSWERTE
      GKK=GLANZ(KW)
      MDW=NWE+3*NL
      IOPT(1)=8
      IOPT(2)=99
      DO 10 I=1,NWE
      RHLF(I)=RR(I)-GKK
      RT(I)=RT(I)-GKK
      RH(I)=RH(I)-GKK
      IND(I)=3
  10  CONTINUE
C      WRITE(27,*) 'ALP,NL,FA',ALP,NL,FA
      IF(IFL.EQ.1) THEN
C
C
C        MOEGLICHST GUTER ANGLEICH AN GEGEBENE KURVE RR
C
C
C
C
        KQ=3
        NG=0
        MROWS=NWE+3*NL
        DO 60 I=1,NWE
        DO 65 J=1,NWE   
        A(I,J)=0.
  65    CONTINUE
        A(I,I)=ALP
        A(I,NWE+1)=ALP*RHLF(I)
  60    CONTINUE
      ELSE
C
C
C     
C       s. NUMERICAL RECIPES S. 800 (Verwendung von NORMALGLEICHUNGEN)
C
C
C       REGULARISATOR     1   -2    1
C
C
C 
        NG=0           
        MROWS=NWE+3*NL-2      
        KQ=5
        DO 40 I=2,NWE-1
        II=I-1
        DO 45 J=1,NWE+1
        A(II,J)=0.
  45    CONTINUE
        A(II,II)=ALP
        A(II,II+1)=-2.*ALP
        A(II,II+2)=ALP
  40    CONTINUE
      ENDIF
c      DO 15 I=1,NWE
c      RHLF(I)=0.
c  15  CONTINUE
      DO 20 K=1,NL
      II=NWE+3*K-KQ     
      DO 21 L=1,3
      A(II+L,NWE+1)=FA(K)*XYZ((K-1)*3+L)
  21  CONTINUE
      DO 20 I=1,NWE
c     RHLF(I)=1.
c      CALL NOGXX(RW,K,RHLF,0.D0)
      DO 35 L=1,3
c      A(II+L,I)=FA(K)*RW(L)
      A(II+L,I)=FA(K)*EILLU(I,L,K)
  35  CONTINUE
c      RHLF(I)=0.
  20  CONTINUE
      CALL SBOCLS(A,MDW,NG,MROWS,NWE,RT,RH,IND,IOPT,X,RNOC,RNRM,
     &            IER,RW,IW)
      DO 70 I=1,NWE
      RR(I)=X(I)+GKK
      RT(I)=RT(I)+GKK
      RH(I)=RH(I)+GKK
  70  CONTINUE
      RETURN
      END
C     
      SUBROUTINE ENT(MM,AXYZ,NWE,XX,XXB,XXU,NL,IFL,KW,ALP)
      USE MODFAUM
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL NLFUNC,NLGRAD

      DOUBLE PRECISION XG(NWE),XS(NWE),X(NWE),XLB(NWE),XLU(NWE),FVALUE
      DOUBLE PRECISION XX(*),XXB(*),XXU(*),AXYZ(*)
      DATA EPS/1.0D-7/
C
      ALLOCATE(XYZ(3*NL),XHIL(3*NL),XM(NWE))
C
      KWF=KW
      NLN=NL
      ALPN=ALP
      DO K=1,3*NLN
         XYZ(K)=AXYZ(K)
      ENDDO
      M=0
      ME=0
      N=NWE
      DO I=1,N
         XLB(I)=XXB(I)+EPS
         XG(I)=1./N
         XS(I)=1.
         XLU(I)=XXU(I)
         IF(IFL.EQ.1) THEN
            XM(I)=XX(I)
         ELSE
            XM(I)=1.0
         ENDIF
      ENDDO
      SXM=0.
      DO I=1,N
         SXM=SXM+XM(I)
      ENDDO
      DO I=1,N
         XM(I)=XM(I)/(SXM+TINY(1.))
         XM(I)=1.0
      ENDDO  
      IPRINT=0
      MAXITN=100
C
      CALL NCONS(M,ME,N,XG,XLB,XLU,
     &           IPRINT,MAXITN,X,FVALUE,NLFUNC,NLGRAD,IFAIL,IER)
      DO I=1,N
         XX(I)=X(I)
      ENDDO
      DEALLOCATE(XYZ,XHIL,XM)
      RETURN
      END
C******************************************************************************
C******************************************************************************
C******************************************************************************
       
C******************************************************************************
C
C
      SUBROUTINE NLGRAD(M4,ME4,MMAX,NF4,F,G,DF,DG,X)
      USE MODFAUM
      USE MODILLU,ONLY:FA,EILLU

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER  M4,ME4,NF4,MMAX
      DIMENSION R(NF4)
      DOUBLE PRECISION  F
      DOUBLE PRECISION  X(*),DF(*),DG(MMAX,*),G(*)
      DOUBLE PRECISION  ENAPHI
C
C
C
      DO I=1,NF4
         R(I)=X(I)
      ENDDO
C
C
      DO I=1,NF4
         DF(I)=-ALPN*ENAPHI(NF4,I,X,XM)
      ENDDO
C
C
      DO K=1,NLN
      CALL NOGXX(XHIL(1+(K-1)*3),K,R,GLANZ(KWF))
      DO I=1,NF4
         DO L=1,3
             KL=L+(K-1)*3
             DF(I)=DF(I)+2*FA(K)*(XHIL(KL)-XYZ(KL))*EILLU(I,L,K)
         ENDDO
      ENDDO
      ENDDO

      RETURN
      END
C******************************************************************************
C
C
      SUBROUTINE NLFUNC(M4,ME4,NF4,F,G,X)
      USE MODFAUM
      USE MODILLU,ONLY:FA

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER M4,ME4,NF4
      DIMENSION R(NF4)
      DOUBLE PRECISION ENPHI
      DOUBLE PRECISION F
      DOUBLE PRECISION X(*),G(*)
C
C
C
      F=-ALPN*ENPHI(NF4,X,XM)
      DO I=1,NF4
         R(I)=X(I)
      ENDDO
      DO K=1,NLN
        CALL NOGXX(XHIL(1+(K-1)*3),K,R,GLANZ(KWF))
        DO L=1,3
          KL=L+(K-1)*3
           F=F+FA(K)*((XHIL(KL)-XYZ(KL)))**2
        ENDDO
      ENDDO
      RETURN
      END
C******************************************************************************
C
C
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
      SUBROUTINE ALAPCLC(ALAB,DLCH,ALAP,IER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ALAB(*),DLCH(*),ALAP(*)
      DIMENSION A(2,3)
      DIMENSION RW(6*2),IW(2*2),IOPT(17),X(2*(2+3)+17)
      DIMENSION IND(2),CL(2),CU(2),X0(2)
      DATA ALP/5.D-4/,EPS/1.D-10/
C      DATA IOP/0/
C
C
C     XYZ   VORGEGEBENE TRISTIMULUSWERTE
C     RR    REFLEXIONSWERTE, DIE MOEGLICHST GUT ERREICHT WERDEN SOLLEN (EING.)
C           ERGEBNIS (AUSG)
C     RT    UNTERE GRENZE FUER REFLEXIONSWERTE
C     RH    OBERE GRENZE FUER REFLEXIONSWERTE

      IOPT(1)=8
      IOPT(2)=99
      MDW=2
      NG=0
      MROWS=2
      N=2
C
C     L*(PROBE)/L9(PROBE)
C
C
      ALAP(1)=ALAB(1)+DLCH(1)
C
C     a*,b*(Probe)/a9,b9(Probe) wird iterativ berechnet
C
C
      X0(1)=ALAB(2)
      X0(2)=ALAB(3)
      CB=SQRT(ALAB(2)**2+ALAB(3)**2)
      CP=CB+DLCH(2)
      DHABS=SQRT(ABS(DLCH(3)**2+DLCH(2)**2))
C
      IND(1)=3
      IND(2)=3

C
      RNRM=1.D80
      RNOC=1.D80
      X(1)=10.0
      X(2)=10.0
      DO IZ=1,100
C
        CL(1)=ALAB(2)-DHABS-X0(1)
        CL(2)=ALAB(3)-DHABS-X0(2)
        CU(1)=ALAB(2)+DHABS-X0(1)
        CU(2)=ALAB(3)+DHABS-X0(2)
C
        HLF=SQRT(ABS(0.5*(CP*CB+ALAB(2)*X0(1)+ALAB(3)*X0(2))))
        HLN=0.25*DLCH(3)/(HLF+EPS)
        A(1,1)=HLN*ALAB(2)+ALAB(3)
        A(1,2)=HLN*ALAB(3)-ALAB(2)
        A(2,1)=2.0*X0(1)
        A(2,2)=2.0*X0(2)
        A(1,3)=-(DLCH(3)*HLF-ALAB(2)*X0(2)+ALAB(3)*X0(1))
        A(2,3)=-(X0(1)**2+X0(2)**2-CP**2)

        CALL SBOCLS(A,MDW,NG,MROWS,N,CL,CU,IND,IOPT,X,RNOC,RNRM,
     &            IER,RW,IW)
        X0(1)=X0(1)+X(1)
        X0(2)=X0(2)+X(2)
        IF((X(1)**2+X(2)**2).LT.1.D-8) then
           EXIT
        ENDIF
      END DO
C
C

      ALAP(2)=X0(1)
      ALAP(3)=X0(2)
      IER=0
      IF(IZ.GT.100) THEN
        IER=-1
      ENDIF
      RETURN
      END
C
C
C
C
C
C

