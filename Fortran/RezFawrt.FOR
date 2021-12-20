C     Last change: KU 14.11.2021 17:49:41
C
C
C
C
      INCLUDE "GETLWGK.FOR"
      INCLUDE "FRBMEN.FOR"
c      INCLUDE "FAKER4.FOR"
c      INCLUDE "FAKER3.FOR"
c      INCLUDE "FRBL1.FOR"
c      INCLUDE "FRBL0.FOR"
C
C
CDEBUG

      SUBROUTINE REZFAWRT(NUV,NUN,NUU,NWEL,KML,RETR,RVOR,RNACH,
     &                 NLZL,FAWRTV,FAWRTN,FEHL)
      USE MODFEHL
      USE MODFUNC,ONLY:JABST

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     BERECHNUNG VON FARBKOORDINATEN UND FARBABSTAENDEN 
C     (SCHNELLVERSION)
C
C     NQU ANZAHL UNTERGRUENDE
C 
C 
C
      INTEGER(KIND=4) :: NUV,NUN,NUU,NWEL,KML,NLZL
      INTEGER(KIND=4),DIMENSION(*)::RETR
      REAL(KIND=4),DIMENSION(21,NLZL,KML,*) :: FAWRTV,FAWRTN
      TYPE(TYFEH) FEHL
      REAL(KIND=4),DIMENSION(NWEL,KML,*) ::RVOR,RNACH
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:)::RV,RN
      REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)::XYZV
      DIMENSION XYZ(3),ALAB(3),ALCH(3),XYK(2)
      DIMENSION XYZW(3)
      DIMENSION XYZU(3)
      DATA XYZU/0.D0,0.D0,0.D0/
C
      DLL_EXPORT REZFAWRT
C
C
C
c
C
C     VORLAGE
C
C
C
c
      ALLOCATE(RV(NWEL),RN(NWEL),XYZV(3,NLZL,KML),STAT=IER)
      IER=IERALC(IER)
      IF(IFEHL(IER).NE.0)THEN
         GOTO 900
      ENDIF
      NWE=NWS()
      KM=KMS()
      NLZ=NLS()
      IF(NWE.LE.0) THEN
        IER=3519
        IF(IFEHL(IER).NE.0) THEN
          GOTO 900
        ENDIF
      ENDIF
      IF(NUV.GT.2) THEN
        IER=4000
        IF(IFEHL(IER).NE.0) THEN
          GOTO 900
        ENDIF
      ENDIF
      IF(NUN.GT.2) THEN
        IER=4000
        IF(IFEHL(IER).NE.0) THEN
          GOTO 900
        ENDIF
      ENDIF
      IF(NUU.GT.2) THEN
        IER=4000
        IF(IFEHL(IER).NE.0) THEN
          GOTO 900
        ENDIF
      ENDIF

C
C      OPEN(27,FILE='TESTOUT.TXT')
C
      DO K=1,MAX(NUV,NUU,NUN)
       IF(K.LE.NUV) THEN
        DO KW=1,KM
         GKK=GLZNOG(KW, RETR(K))
         DO I=1,NWE
            RV(I)=RVOR(I,KW,K)
         ENDDO
         DO L=1,NLZ
            CALL NOGXX(XYZ,L,RV,GKK)
            DO J=1,3
               FAWRTV(J,L,KW,K)=XYZ(J)
               XYZV(J,L,KW)=XYZ(J)
            ENDDO
               CALL XYZKL(XYZ,XYK)
            DO J=1,2
               FAWRTV(3+J,L,KW,K)=XYK(J)
            END DO
               CALL LCHAL(JABST,XYZ,ALAB,ALCH,L)
            DO J=1,3
               FAWRTV(5+J,L,KW,K)=ALCH(J)
            ENDDO
            DO J=2,3
               FAWRTV(7+J,L,KW,K)=ALAB(J)
            ENDDO
            FAWRTV(17,L,KW,K)=HUGE(1.)
C
C
C           KONTRASTFARBABSTAND
C
C
            IF(K.EQ.1.AND. RETR(1).EQ.0) THEN
                    FAWRTV(18,L,KW,K)=HUGE(1.)
            ELSE
              IF(RETR(K).EQ.0.AND.RETR(1).EQ.0) THEN
                    DO J=1,3
                       XYZW(J)=FAWRTV(J,L,KW,1)
                    END DO
                    CALL DELABAL(JABST,XYZW,XYZ,DEWS,DL,DC,DH,DA,DB,L)
                    FAWRTV(18,L,KW,K)=DEWS
              ELSEIF(RETR(K).EQ.1) THEN
                    CALL DELABAL(JABST,XYZU,XYZ,DEWS,DL,DC,DH,DA,DB,L)
                    FAWRTV(18,L,KW,K)=DEWS
              ENDIF
            ENDIF
         ENDDO        
        ENDDO
       ENDIF
C
C     NACHSTELLUNGEN
C
C
C
c
       IF(K.LE.MAX(NUU,NUN)) THEN
        DO KW=1,KM
         GKK=GLZNOG(KW,RETR(K))
         DO I=1,NWE
            RN(I)=RNACH(I,KW,K)
            RV(I)=RVOR(I,KW,K)
         ENDDO
         DO L=1,NLZ
            CALL NOGXX(XYZ,L,RN,GKK)
            DO J=1,3
               FAWRTN(J,L,KW,K)=XYZ(J)
            ENDDO
               CALL XYZKL(XYZ,XYK)
            DO J=1,2
               FAWRTN(J+3,L,KW,K)=XYk(J)
            END DO
               CALL LCHAL(JABST,XYZ,ALAB,ALCH,L)
            DO J=1,3
               FAWRTN(J+5,L,KW,K)=ALCH(J)
            ENDDO
            DO J=2,3
               FAWRTN(J+7,L,KW,K)= ALAB(J)
            ENDDO
            IF(K.LE.NUV) THEN
               CALL DELABAL(JABST,XYZV(1,L,KW),XYZ,DE,DL,DC,DH,DA,DB,L)
               FAWRTN(11,L,KW,K)=DE
               FAWRTN(12,L,KW,K)=DL
               FAWRTN(13,L,KW,K)=DC
               FAWRTN(14,L,KW,K)=DH
               FAWRTN(15,L,KW,K)=DA
               FAWRTN(16,L,KW,K)=DB

C
C              METAMERIE
C
C
               CALL METAMAL(JABST,RV,RN,GKK,AMB,L,2)
               FAWRTN(17,L,KW,K)=AMB

             ELSE
               FAWRTN(11,L,KW,K)=HUGE(1.)
               FAWRTN(12,L,KW,K)=HUGE(1.)
               FAWRTN(13,L,KW,K)=HUGE(1.)
               FAWRTN(14,L,KW,K)=HUGE(1.)
               FAWRTN(15,L,KW,K)=HUGE(1.)
               FAWRTN(16,L,KW,K)=HUGE(1.)
               FAWRTN(17,L,KW,K)=HUGE(1.)


             ENDIF
C
C
C              KONTRASTFARBABSTAND
C           
               IF(K.EQ.1.AND.RETR(1).EQ.0) THEN
                    FAWRTN(18,L,KW,K)=HUGE(1.)
               ELSE
                 IF(RETR(K).EQ.0.AND.RETR(1).EQ.0) THEN
                    DO J=1,3
                       XYZW(J)=FAWRTN(J,L,KW,1)
                    END DO
                    CALL DELABAL(JABST,XYZW,XYZ,DEWS,DL,DC,DH,DA,DB,L)
                    FAWRTN(18,L,KW,K)=DEWS
                 ELSEIF(RETR(K).EQ.1) THEN
                    CALL DELABAL(JABST,XYZU,XYZ,DEWS,DL,DC,DH,DA,DB,L)
                    FAWRTN(18,L,KW,K)=DEWS
                 ENDIF
               ENDIF

         ENDDO        
        ENDDO
       ENDIF
      ENDDO
      IF(NUV.GT.1) THEN
        IF(RETR(1).EQ.0.AND.RETR(2).EQ.0) THEN
         DO KW=1,KM
            DO L=1,NLZ
               FAWRTV(18,L,KW,1)=FAWRTV(18,L,KW,2)
            ENDDO
         ENDDO
        ENDIF
      ENDIF
      IF(MAX(NUU,NUN).GT.1) THEN
        IF(RETR(1).EQ.0.AND.RETR(2).EQ.0) THEN
         DO KW=1,KM
            DO L=1,NLZ
               FAWRTN(18,L,KW,1)=FAWRTN(18,L,KW,2)
            ENDDO
         ENDDO
        ENDIF
      ENDIF
c
  900 CALL GETFEH(FEHL)
c      CALL TERMFDM(IER)
C       CLOSE(27)
      DEALLOCATE(RV,RN,XYZV)

      RETURN
c     
      END

