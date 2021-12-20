C     Last change: KU 07.12.2020 09:09:23
C***************************************************************************************************************************
C***************************************************************************************************************************
C********************************** Klaus Unterforsthuber ******************************************************************
C***************************************************************************************************************************
C*****************************************  FRBMEN  ************************************************************************
C***************************************************************************************************************************
C***************************************  27.01.2005  **********************************************************************
C***************************************************************************************************************************
C***************************************************************************************************************************

      SUBROUTINE FRBMEN(IDaeArtL,ALPP,GMAXL,RWGEWL,JABSTL,AbsID,
     &     BwID,FsID,WelFstL,Schwell,WLCHL,KdeWS,VhWS,PARMERK,FEHL)
      USE MODFUNC
      USE MODFEHL

      IMPLICIT NONE
C
C
      INTEGER(KIND=4) :: I,ICHK,IER,IDaeArtL,IFEHL
      TYPE(TYFEH) :: FEHL
      INTEGER(KIND=4) :: AbsID,BwID,FsID,JABSTL
      REAL(KIND=4) :: WelFstL,Schwell,KdeWS,VhWS,WLCHL(3),
     &                ALPP,GMAXL,RWGEWL
      REAL(KIND=4) PARMERK(*)
C
C
C
      DLL_EXPORT FRBMEN
C
C
C     Parameter für Merkmale nach MODFAKT speichern
C
      CALL GETPAR(0,PARMERK,IER)
      IF(IFEHL(IER).NE.0) GOTO 900

C
C     MODFUNC beschreiben
C
C     JABST 0 DIN6174; 1 DIN6176
C
      JABST=JABSTL
C
C
      IER=0
C
C     Kennung z.B. für FSTTRA(für Regularisierung)
C
      IDaeArt=IDaeArtL
C
C     DAEMPFUNG
C
      ALP=ALPP
C
C     Maximalwert für Extinktion bzw. K/S - Werte u.ä.m.
      GMAX=GMAXL
C     Gewicht für Minimierungsbedingung
      RWGEW=RWGEWL
C     Absolutwerte für Bezug bei Differenzwerten
      KABS=ABSID
C     Art des B-wertes
      KBWT=BwID
      IART=FsID
      WELFST=WelFstL
      DESCHW=SCHWELL
      DO I=1,3
         WLCH(I)= WLCHL(I)
      ENDDO
      ICHK=0
      DO I=1,3
         IF(WLCH(I).GT.0.D0) THEN
            ICHK=1
         ENDIF
      ENDDO
      IF(ICHK.EQ.0) THEN
         IER=4024
         GOTO 900
      ENDIF
C
C
C     KONTRASTFARBABSTAND
C
      FDE=KdeWS
C
C
C
C     WEISS/SCHWARZVERHAELTNIS
C
      AMWAMS=VhWS
C
C     
C
 900  IF(IFEHL(IER).NE.0) THEN
          CALL GETFEH(FEHL)
      ENDIF
      RETURN

      END

