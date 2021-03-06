C     Last change: KU 11.02.2020 16:17:10
C
C     Farbwerte
C
C
c
c     fARBKOORDINATEN XYZ,Lab,LCH
c
      SUBROUTINE XYZLABCH(NWEL,IRT,KW,KNO,R,XYZ,ALAB,ALCH,IER)
      USE MODFUNC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=4),DIMENSION(3) :: XYZ,ALAB,ALCH
      REAL(KIND=8),DIMENSION(3) :: XYZ8,ALAB8,ALCH8
      REAL(KIND=4),DIMENSION(NWEL) :: R
      INTEGER(KIND=4) :: NWEL,IRT,KW,KNO

C
      DLL_EXPORT XYZLABCH
C
      IER=0
      IF(JABST.EQ.-1) THEN
        IER=-1
        RETURN
      ENDIF
      CALL COLVALUE(NWEL,IRT,KW,KNO,R,XYZ8,ALAB8,ALCH8)
      DO I=1,3
        XYZ(I)=XYZ8(I)
        ALAB(I)=ALAB8(I)
        ALCH(I)=ALCH8(I)
      END DO
      RETURN
      END SUBROUTINE
C
C
C
C
      SUBROUTINE DIFFREF(NWEL,IRT,KW,KNO,RB,RP,DE,DL,DC,DH,DA,DB,IER)
      USE MODFUNC
C
C
      USE MODILLU
      USE MODWINK
      USE MODGKWR
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=4),DIMENSION(NWEL) :: RB,RP
      INTEGER(KIND=4) :: IRT,KW,KNO,I
      REAL(KIND=4) :: DE,DL,DC,DH,DA,DB
      REAL(KIND=8) :: GKK
      REAL(KIND=8),DIMENSION(3) :: XYZV,XYZN
      REAL(KIND=8),DIMENSION(NWEL) :: RV,RN
C
C
      DLL_EXPORT DIFFREF
C
C
C
      IER=0
      IF(JABST.EQ.-1) THEN
        IER=-1
        RETURN
      ENDIF
      GKK=GLZNOG(KW,IRT)
      DO I=1,NWEL
        RV(I)=RB(I)
        RN(I)=RP(I)
      END DO
      CALL NOGXX(XYZV,KNO,RV,GKK)
      CALL NOGXX(XYZN,KNO,RN,GKK)
      CALL DELABAL(JABST,XYZV,XYZN,DES,DLS,DCS,DHS,DAS,DBS,KNO)
      DE=DES
      DL=DLS
      DC=DCS
      DH=DHS
      DA=DAS
      DB=DBS
      RETURN
      END SUBROUTINE
C
C     FARBDIFFERENZEN
C
C
      SUBROUTINE DIFFXYZ(KNO,XYZSOLL,XYZIST,DE,DL,DC,DH,DA,DB,IER)
      USE MODFUNC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) :: KNO,I
      REAL(KIND=4),DIMENSION(3) :: XYZSOLL,XYZIST
      REAL(KIND=4) :: DE,DL,DC,DH,DA,DB
      REAL(KIND=8) :: GKK
      REAL(KIND=8),DIMENSION(3) :: XYZV,XYZN
C
C
      DLL_EXPORT DIFFXYZ
C
C
C
      IER=0
      IF(JABST.EQ.-1) THEN
        IER=-1
        RETURN
      ENDIF
      DO I=1,3
        XYZV(I)=XYZSOLL(I)
        XYZN(I)=XYZIST(I)
      END DO
      CALL DELABAL(JABST,XYZV,XYZN,DES,DLS,DCS,DHS,DAS,DBS,KNO)
      DE=DES
      DL=DLS
      DC=DCS
      DH=DHS
      DA=DAS
      DB=DBS
      RETURN
      END SUBROUTINE

C
C     Berechnung von Farbwerten
C
C
C
      SUBROUTINE COLVALUE(NWEL,IRT,KW,KNO,RDA,XYZ,ALAB,ALCH)
      USE MODFUNC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(KIND=4),DIMENSION(NWEL) :: RDA

      REAL(KIND=8) :: R(NWEL)
      REAL(KIND=8),DIMENSION(3) :: XYZ,ALAB,ALCH
      INTEGER(KIND=4) :: IRT,KW,KNO
      DO I=1,NWEL
          R(I)=RDA(I)
      END DO
      GKK=GLZNOG(KW,IRT)
      CALL NOGXX(XYZ,KNO,R,GKK)
      CALL LCHAL(JABST,XYZ,ALAB,ALCH,KNO)
      RETURN
      END SUBROUTINE
