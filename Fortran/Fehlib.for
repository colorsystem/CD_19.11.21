C     Last change:  UFO   4 Apr 108    9:21 pm
C******************************************************
C**********************************************************************
C**********************************************************************
C
C           Klaus Unterforsthuber
C           09.08.2006
C
C**********************************************************************
C**********************************************************************
C**********************************************************************
C**********************************************************************
C

      INTEGER FUNCTION IERALC(IER)
      USE MODFEHL
         IERALC=0
         IF (IER.NE.0) THEN
             IERALC=4000
             FEHH%KENN=IER
         END IF
      RETURN
      END
C
C
      SUBROUTINE FEHINI
      USE MODFEHL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      FEHH%IFEH=0
      DO I=1,8
        FEHH%CFEH(I:I)=CHAR(0)
      ENDDO
      FEHH%KENN=0
      FEHH%IWARN=0
      FEHH%WERT=0.
      RETURN
      END 

C
C
C
      INTEGER(KIND=4) FUNCTION IFEHL(IER)
      USE MODFEHL

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(KIND=4) ::IER
      IFEHL=0
      IF(FEHH%IFEH.GT.0) THEN
         IFEHL=FEHH%IFEH
         RETURN
      ENDIF
      IF(IER.GT.0) THEN
         FEHH%IFEH=IER
         IFEHL=IER
         RETURN
      ENDIF
      IF(IER.LT.0) THEN
         FEHH%IWARN=-IER
         RETURN
      ENDIF
      RETURN
      END

C******************************************************
      SUBROUTINE GETFEH(FEHL)
      USE MODFEHL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      TYPE(TYFEH) FEHL
      FEHL=FEHH
      CALL FEHINI
      RETURN
      END
C******************************************************
C******************************************************
C******************************************************
C 
      SUBROUTINE FSTOP(IRET)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      RETURN   
      END
C******************************************************
C
C
C******************************************************
C******************************************************
C******************************************************
C
C     FEHLER-ROUTINE
C 
      SUBROUTINE FEHER(NERR,KENN)
      USE MOTFEHL
      USE MODFEHL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      FEHH%KENN=10000*NERR+KENN
      FEHH%IFEH=4000
      RETURN
      END
C******************************************************
  
C
C******************************************************
      
C******************************************************
C******************************************************
C

