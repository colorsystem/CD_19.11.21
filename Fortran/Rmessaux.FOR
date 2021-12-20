C     Last change: KU 19.07.2019 10:04:50
      SUBROUTINE RMAUXIL(MESID,MESGID,IRETR,NGK,GK,NWE,KM,Well,R,
     &           IER)
C
C
C
C     ALLE LISTENPARAMETER BYREF!!!!
C
C
      REAL*4 GK(NGK,*),R(*),Well(*)
      INTEGER*4 MESID,MESGID
      INTEGER*4 NWE,KM,IER,IRETR
      DLL_EXPORT RMAUXIL
      IER=0
C
C     MESID    Nummer des Messgerätes (S.Tabelle TBL_MESSG in BCSWIN.MDB)
C     MESGID   Aktuelle Gruppennummer für Reflexionswerte
C     IRETR    0=Reflexion; 1=Transmission
C     NGK      Anzahl GK-Werte proWinkel 
C     GK(16,KM GK(1)=Glanz (z.B. 0.04)
C              GK(2)=Innere Reflexion(gerichtet) (z.B. 0.04)
C              GK(3) innere Reflexion (diffus) (z.B. 0.6)
C              GK(4)-GK(12) sind hier ohne Bedeutung
C     NWE      ANZAHL Wellenlängen (z.B. 16)
C     KM       Anzahl Winkel bzw. Messgeometrien (normalerweise =1)
C              (>1 für Mehrwinkelmessgeräte oder falls Messungen mit und ohne Glanz durchgeführt werden)
C     Well     Wellenlängen in nm (1 bis NWE)
C     R        Reflexionswerte
C              (R(1) bis  R(NWE)) Messungen für den 1. Winkel bzw. die 1. Messgeometrie)
C              (R(NWE+1) bis  R(2*NWE)) Messungen für den 2. Winkel bzw. die 2. Messgeometrie)
C              (R((KM-1)*NWE+1) bis R(KM*NWE))  Messungen für den KM-ten Winkel bzw. die KM-te Messgeometrie)
C     IER      Fehlercode (0 kein Fehler)
C
      RETURN
      END
