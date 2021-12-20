

Option Strict Off
Option Explicit On
Option Compare Text
Public Module FortranDLL
  Dim res As MessageBoxButtons

  Public NamLogFile As String
  Public umr As RezeptUmrechnung
  Public OptGesamt As OpticalData
  '
  '
  '
  '
  '************************
  '************************
  'ACHTUNG: Diese Typdeklarationen dürfen nur
  'geändert werden, falls die entsprechenden DLL's ebenfalls angepasst werden
  '************************
  '************************
  '************************
  '
  '
  '
  'Zusatzinformation für Rwerte
  '
  '
  Structure TyRwert
    Dim RwertID As Integer 'ID Rwerte
    Dim RwertNr As Short   'Nummer Rwerte
    Dim KWB As Short       'Weiß(1)/Schwarz(2)
    Dim Retr As Short      'Reflex.(0)/Transm.(1)
    Dim IVONA As Short     'Dummy
  End Structure
  '
  '
  '
  'Zusatzinformation Qualitätskontrolle
  '
  '
  '
  Structure TyQual
    Dim MethID As Integer 'ID Methode
    Dim IuntID As Integer 'ID Untergrund
    '                     'transparent    deckend
    Dim Camp00 As Single   'Schichtdicke
    Dim Camp01 As Single   'Menge Bunt     Menge Weiß
    Dim Camp02 As Single   'Fläche         Proz. Weiß 
    Dim Camp03 As Single   'Proz Bunt      Menge Bunt
    Dim Camp04 As Single   '               Proz. Bunt
    Dim Camp05 As Single
    Dim Camp06 As Single
    Dim Camp07 As Single
    Dim Camp08 As Single
    Dim Camp09 As Single
    Dim Camp10 As Single
    Dim Camp11 As Single
    Dim Camp12 As Single
    Dim Camp13 As Single
    Dim Camp14 As Single
    Dim Camp15 As Single

    '                     'Statistischer Farbabstand für
    Dim DE00 As Single    '0. Winkel
    Dim DE01 As Single    '1. Winkel
    Dim DE02 As Single    'u.s.w.
    Dim DE03 As Single
    Dim DE04 As Single
    Dim DE05 As Single
    Dim DE06 As Single
    Dim DE07 As Single
    Dim DE08 As Single
    Dim DE09 As Single
    Dim DE10 As Single
    Dim DE11 As Single
    Dim DE12 As Single
    Dim DE13 As Single
    Dim DE14 As Single
    Dim DE15 As Single
    Dim iami As Short     'Anzahl Messungen
    Dim Idum As Short     'Dummy
    '                     'Art der Messung
    Dim Cart0 As Byte     '@,A,B,.. usw. Gruppe
    Dim Cart1 As Byte     'T,P Typ oder Probe 
    Dim Cart2 As Byte     'M,B Messung,Rechnung
    Dim Cart3 As Byte     'W,S weißer-,schwarzer Untergrund
  End Structure
  '
  '
  '
  'Zusatzinformation Parameter Qualitätskontrolle
  '
  '
  '
  Structure TyWert
    Dim RwertID As Integer 'ID Rwerte
    Dim AuswID As Integer  'ID Auswertung
    Dim Itp As Short       '1 Bezug; 2 Probe 
    Dim RwertNr As Short   'Nummer Rwert
    Dim KWB As Short       'Weiß(1)/Schw(2) 
    Dim Retr As Short      'Reflex.(0)/Transm.(1)
    Dim Cmeth0 As Byte     '@ Messung @,A,B usw.
    Dim Cmeth1 As Byte     'T Bezug oder P Probe
    Dim Cmeth2 As Byte     'Messung(M) oder Rechnung(B)
    Dim Cmeth3 As Byte     'Weiß(W) oder Schwarz(S)
    Dim Cmeth4 As Byte     'Nummer Lichtart (1,2 usw)
    Dim Cmeth5 As Byte     'Nummer Winkel (1,2 usw)
    Dim Cmeth6 As Byte     'frei 
    Dim Cmeth7 As Byte     'frei
    Dim LNR As Integer
  End Structure
  '
  '
  '
  '
  'Fehler für DLL Datenaustausch
  '
  '
  '

  Structure TYFEH
    Dim Ifeh As Short   'Fehlercode     
    Dim Iwarn As Short  'Warncode 
    Dim Kenn As Integer 'Kennung
    Dim Wert As Single  'Zasatzangabe
    Dim cfeh As Long    'Fehlerangabe
  End Structure
  '  '
  '
  '
  '
  '
  '
  'Dimensionierung in Kommentaren für .NET-Aufruf!!!
  '
  '
  '
  '
  '
  'Umrechnung von FarbWerten in Reflexionskurven
  'Isch 1 DX,DY,DZ
  '     2 DL*,Da*,Db*
  '     3 X,Y,Z
  '     4 L*,a*,b*
  '     5 L*,C*,h
  '
  'Ifl  1 Angleich an R-Kurve (Tyreinp(0))
  '     2 Untere (Tyreinp(2)) und obere (TyReinp(3) Grenze für R-Werte
  '
  'Ifw  1 Least Square (d.h. zwischen benachbarten werten soll Differenz
  '                     gemäß Minimierung der Fehlerquadratsumme möglichst klein sein)
  '     2 Maximum Entropie
  '
  '

  '
  'Menueparameter
  '
  '
  Declare Sub FWRMEN Lib "REZFAWRT.DLL" Alias "FRBMEN" _
  (ByRef IDaeArt As Integer, ByRef ALP As Single, ByRef GMAX As Single, ByRef RWGEW As Single, ByRef JABST As Integer, ByRef AbsID As Integer, ByRef BwID As Integer, ByRef FsID As Integer, _
   ByRef WelFst As Single, ByRef Schwell As Single, ByRef WLCH As Single, _
   ByRef KdeWS As Single, ByRef VhWS As Single, ByRef ParamMerk As Single, ByRef FEHL As TYFEH)
  '
  '
  'Übernahme der Werte für Winkel, Normlichtarten und GK-Werte
  '
  '1. FarbWerte XYZ,CIELAB u.ä.m.
  '
  '   dim CDE(0) as byte
  '   dim GK(15) as single
  '   dim NormId(nlz-1) as integer
  '   dim GewNorm(nlz-1) as single
  '   dim Fakt(nlz-1,2) as single
  '   dim WSOL(NWE-1) as single
  '   dim E(NLZ-1,2,nwe-1)
  '   dim IhrmWin(km-1) as integer
  '   dim Iglz(km-1) as integer
  '   Gint(km-1) As Single
  '   dim GewIhrm(KM-1) as single
  '
  Declare Sub GETFDMFWR Lib "REZFAWRT.DLL" Alias "GETFDM" _
  (ByRef KWW As Integer, ByRef Aka As Single, ByRef Fopt As Single, ByRef CDE As Byte, _
   ByRef Nlz As Integer, ByRef BeID As Integer, ByRef NormID As Integer, ByRef GewNorm As Single, ByRef Fakt As Single, _
   ByRef NWE As Integer, ByRef KM As Integer, ByRef Wsol As Single, ByRef E As Single, _
   ByRef IhrmWin As Integer, ByRef GewIhrm As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef FEHL As TYFEH)
  '
  '
  

  'FarbWerte für Rezeptberechnung (s. auch GETFDMFWR)

  '
  '   NUV,NUN,NUU<=2
  '
  '   Dim Retr(0) As Integer
  '   dim Rtyp(1,KM-1,NWE-1) as single
  '   dim RPrb(1,KM-1,NWE-1) as single
  '   FaWrtV(1,KM-1,NLZ-1,20) as single
  '   FaWrtN(1,KM-1,NLZ-1,20) as single
  '
  Declare Sub REZFAWRT Lib "REZFAWRT.DLL" _
  (ByRef NUV As Integer, ByRef NUN As Integer, ByRef NUU As Integer, ByRef NWE As Integer, ByRef KM As Integer, _
  ByRef Retr As Integer, ByRef RTyp As Single, ByRef RPrb As Single, _
  ByRef Nlz As Integer, ByRef FaWrtV As Single, ByRef FaWrtN As Single, ByRef FEHL As TYFEH)

  '
  '2. Grunddaten
  '
  '
  Declare Sub GRDBEG Lib "GRDDATEN.DLL" _
  (ByRef NWE As Integer, ByRef KM As Integer, ByRef NME As Integer, ByRef NFG As Integer, ByRef NPQ1 As Integer, ByRef MMX As Integer, ByRef FEHL As TYFEH)
  '
  '
  'Normlichtarten,Winkel,GK-Werte
  '
  '
  '
  Declare Sub GETFDMGRD Lib "GRDDATEN.DLL" Alias "GETFDM" _
   (ByRef KWW As Integer, ByRef Aka As Single, ByRef Fopt As Single, ByRef CDE As Byte, _
   ByRef Nlz As Integer, ByRef BeID As Integer, ByRef NormID As Integer, ByRef GewNorm As Single, ByRef Fakt As Single, _
   ByRef NWE As Integer, ByRef KM As Integer, ByRef Wsol As Single, ByRef E As Single, _
   ByRef IhrmWin As Integer, ByRef GewIhrm As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  'Menuparameter für Grunddaten
  '
  '
  '
  Declare Sub GRDMEN Lib "GRDDATEN.DLL" _
  (ByRef FU As Single, ByRef EXPO As Single, ByRef GewR As Single, ByRef FTO As Single, ByRef GewDik As Single, ByRef JTM As Integer, ByRef Iglae As Integer, ByRef FEHL As TYFEH)
  '
  '
  '
  'Mengen und Remissionswerte
  '
  '
  '
  Declare Sub GRDMGRF Lib "GRDDATEN.DLL" _
  (ByRef NWE As Integer, ByRef KM As Integer, ByRef NME As Integer, ByRef KFM As Integer, ByRef NDGRE As Integer, ByRef NAZ As Integer, _
   ByRef ICH As Integer, ByRef FST As Single, ByRef AKOWRT As Single, ByRef GRSTR As Single, ByRef GRDAE As Single, ByRef GRCCQ As Byte, _
   ByRef DikAlt As Single, ByRef RMENG As Single, ByRef KWBUN As Integer, ByRef RETRUN As Integer, ByRef REDUN As Single, ByRef NRRWRT As Integer, _
   ByRef KWBRE As Integer, ByRef RETRRE As Integer, ByRef REDAM As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  'Alte Grunddaten übernehmen von Farbmittel KFU bis KFO
  '
  '
  '
  Declare Sub GRDGRUA Lib "GRDDATEN.DLL" _
  (ByRef KM As Integer, ByRef NWE As Integer, ByRef KFU As Integer, ByRef KFO As Integer, _
  ByRef NPQ1 As Integer, ByRef NST As Integer, ByRef Cst As Single, ByRef Grund As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  'Startwerte für die Berechnung von neuen Grunddaten festlegen für Farbmittel KFU bis KFO
  '
  '
  '
  '
  Declare Sub GRDGRUN Lib "GRDDATEN.DLL" (ByRef KFU As Integer, ByRef KFO As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef NPQ1 As Integer, ByRef NST As Integer, ByRef Cst As Single, ByRef Grund As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  'Grenzen für Grunddaten festlegen (Grunddaten für Farbmittel <= KFI sind konstant)
  '(s. auch AKOWRTL,GRSTRL,GRDAEL,GRCCQL in Sub GRDMGRF)
  'Grunddaten von KFI+1 bis KFI+KFA = NAZ sind variabel
  '
  '
  '
  Declare Sub GRDLIM Lib "GRDDATEN.DLL" (ByRef KFI As Integer, ByRef KFA As Integer, ByRef KM As Integer, ByRef NPQ1 As Integer, ByRef NST As Integer, ByRef CST As Single, ByRef FEHL As TYFEH)
  '
  '
  'Neue Grunddaten berechnen
  '
  '
  '
  '
  Declare Sub GRDCALC Lib "GRDDATEN.DLL" (ByRef NWE As Integer, ByRef KM As Integer, _
  ByRef NPQ1 As Integer, ByRef NST As Integer, ByRef Cst As Single, ByRef DikNeu As Single, ByRef Grund As Single, ByRef GruGrz As Single, ByRef PHHI As Single, ByRef FEHL As TYFEH)
  '
  '
  'Remissions- bzw Transmissionswerte berechnen
  '
  '
  '
  '
  '
  '
  Declare Sub GRDREFL Lib "GRDDATEN.DLL" (ByRef NWE As Integer, ByRef KM As Integer, ByRef NPQ1 As Integer, ByRef NPX As Integer, ByRef NST As Integer, ByRef Cst As Single, ByRef Grund As Single, _
  ByRef NAZ As Integer, ByRef NFG As Integer, _
  ByRef KWBRE As Integer, ByRef DikNeu As Single, ByRef RMENG As Single, ByRef REDAB As Single, ByRef GRNDALL As Single, ByRef FEHL As TYFEH)
  '
  '
  'Ende Grunddaten
  '
  '
  '
  '
  '
  Declare Sub GRDEND Lib "GRDDATEN.DLL" (ByRef FEHL As TYFEH)
  '
  '
  '
  'Grunddaten Drucken
  '
  '
  '
  Declare Sub GRDDRU Lib "GRDDATEN.DLL" (ByRef FEHL As TYFEH)

  '
  '
  '
  '
  '
  '
  '
  '3. Rezeptberechnung
  '
  Declare Sub GETFDMRZP Lib "REZFARB.DLL" Alias "GETFDM" _
   (ByRef KWW As Integer, ByRef Aka As Single, ByRef Fopt As Single, ByRef CDE As Byte, _
   ByRef Nlz As Integer, ByRef BeID As Integer, ByRef NormID As Integer, ByRef GewNorm As Single, ByRef Fakt As Single, _
   ByRef NWE As Integer, ByRef KM As Integer, ByRef Wsol As Single, ByRef E As Single, _
   ByRef IhrmWin As Integer, ByRef GewIhrm As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  '
  '
  'Drucken in Rezeptrogramm
  '
  '
  '
  '
  '

  Declare Sub REZDRU Lib "REZFARB.DLL" (ByRef FEHL As TYFEH)
  '
  '
  '
  'Beginn Rezeptrechnung
  '
  '
  Declare Sub REZBEG Lib "REZFARB.DLL" (ByRef NWE As Integer, ByRef KM As Integer, ByRef NME As Integer, _
                     ByRef NFG As Integer, ByRef NFV As Integer, ByRef NPQ1 As Integer, ByRef NLQ As Integer, ByRef NHLF As Integer, _
                     ByRef MEE As Integer, ByRef MEM As Integer, ByRef NSPE As Integer, ByRef FEHL As TYFEH)
  '

  '
  '
  'Menüparameter für Rezeptberechnung
  '
  '
  '
  '
  '
  '   dim FU(0) as single
  '   dim WLCH(2) as single
  '

  Declare Sub REZMEN Lib "REZFARB.DLL" _
  (ByRef JABST As Integer, ByRef FU As Single, ByRef WLCH As Single, _
  ByRef DEHLF As Single, ByRef DEGUT As Single, ByRef DEOK As Single, _
  ByRef FDE As Single, ByRef GDE As Single, ByRef GGE As Single, _
  ByRef FTO As Single, ByRef ATO As Single, ByRef DEL As Single, ByRef SPR As Single, _
  ByRef GXG As Single, ByRef GXD As Single, ByRef DTO As Single, ByRef GTO As Single, _
  ByRef NPX As Integer, ByRef EPMNG As Single, ByRef MINFarb As Integer, _
  ByRef MAXFarb As Integer, ByRef KWD As Integer, ByRef ITM As Integer, ByRef AKA As Single, ByRef IGX As Integer, _
  ByRef KWM As Integer, ByRef KGX As Integer, ByRef ICHI As Integer, ByRef ISOR As Integer, _
  ByRef GKID As Integer, ByRef ParamMerk As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  ' Farbmittel und Grunddaten übernehmen
  '
  '
  '
  '
  '
  '   dim KTO(Kf-1) as byte
  '   dim OP(KF-1) as byte
  '   dim Ichf(kf-1) as integer
  '   dim Preis(kf-1) as single
  '   dim Fst(kf-1) As single
  '   dim eff(kf-1) as single
  '   dim Nst(kf-1) as integer
  '   dim cst(kf-1,NPQ-1 )as single
  '   dim GRUND (kf-1,NPQ,km-1,nwe-1) as single


  Declare Sub REZGRU Lib "REZFARB.DLL" (ByRef NF As Integer, ByRef MngRei As Single, ByRef OP As Byte, _
  ByRef KTO As Byte, ByRef Ichf As Integer, _
  ByRef Preis As Single, ByRef Fst As Single, ByRef NWE As Integer, ByRef KM As Integer, _
  ByRef NPQ1 As Integer, ByRef NST As Integer, ByRef Cst As Single, ByRef Grund As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  'Mengen und Reflexionswerte übernehmen
  '
  '
  '
  '
  '  Dim ReTR(0) as integer
  '  DIM KWB(0) as integer
  '  Dim Dicke(0) as single
  '  Dim Runt(nwe-1,km-1,1) as single
  '  Dim RVor(nwe-1,Km-1,1) as single
  '  Dim RNAC(nwe-1,Km-1,1) as single
  '  dim ReMng(Nf-1) as single

  '

  Declare Sub REZMGRF Lib "REZFARB.DLL" (ByRef IPGR As Integer, ByRef NUU As Integer, ByRef NUV As Integer, ByRef NUN As Integer, _
  ByRef NWE As Integer, ByRef KM As Integer, ByRef Retr As Integer, ByRef KWB As Integer, _
  ByRef Runt As Single, ByRef RVOR As Single, ByRef Rnac As Single, ByRef NF As Integer, ByRef Remng As Single, ByRef DICKE As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  'Zusätzliche Mengen und Reflexionswerte für Hilfskorrektur IH   übernehmen
  '
  '
  '
  '
  '
  Declare Sub REZHILF Lib "REZFARB.DLL" (ByRef IH As Integer, ByRef NWE As Integer, ByRef KM As Integer, _
  ByRef RVOR As Single, ByRef Rnac As Single, ByRef NF As Integer, ByRef Remng As Single, ByRef DICKE As Single, ByRef FEHL As TYFEH)


  '
  '
  '
  'Aus BaMng, BuMng BoMng,Proz,Prob,OP,ICHF usw. und den unteren und oberen Grenzen für die Normierungsmenge (MngMin,MngMax)
  'und die Normierungsprozentigkeiten (ProzMin,ProzMax) werden Gleichungen und Ungleichungen zur Beschränkung der reinen Farb-/Bindemttelmengen hergeleitet
  'Diese stehen FORTRAN-intern den Programmen REZREZ,REZBAS,REZKOR,MSHREZ,MSHKOR,HLFKOR zur Verfügung.
  '
  ' BaMng,FaMng,BuMng,BoMng,BEL,PROZ,PROB,Spz(NF-1) as single
  ' ICHF(NF-1) as integer
  ' OP(NF-1) as Byte
  '
  '
  '
  '
  Declare Sub RCHLIM Lib "REZFARB" (ByRef NF As Integer, ByRef IVOL As Integer, _
  ByRef INF As Integer, ByRef INO As Integer, ByRef INP As Integer, ByRef INQ As Integer, ByRef INM As Integer, ByRef IGX As Integer, _
  ByRef MngMin As Single, ByRef MngMax As Single, ByRef ProzMin As Single, ByRef ProzMax As Single, ByRef Sploe As Single, _
  ByRef BaMng As Single, ByRef FaMng As Single, ByRef Proz As Single, ByRef Prob As Single, ByRef EFF As Single, ByRef Spz As Single, ByRef Ichf As Integer, _
  ByRef OP As Byte, ByRef BuMng As Single, ByRef BoMng As Single, ByRef Bel As Single, ByRef FEHL As TYFEH)


  '
  '
  '
  'Erstrezeptberechnung
  '
  '
  '
  '
  '

  '
  '  dim BaMng(Nf-1) as single
  '  dim ReMng(Nf-1) as single
  '  dim Rcal(nwe-1,km-1,1) as single
  '  dim NFR(KZL-1) as integer
  '  dim LKID(KZL-1,KFM-1 ) as integer
  '  dim ReMng(KZL-1,KFM-1) as single
  '  dim LK(KFM-1) as integer
  '  dim Cst(0) as single
  '  dim Gruges(1,KM-1,NWE-1) as single
  '  dim SoKrit(1,16) as single
  '
  '
  'KFIT=1 Angleich FarbWerte
  'KFIT=2 Angleich Reflexionswerte
  '

  Declare Sub REZREZ Lib "REZFARB.DLL" (ByRef KFIT As Integer, ByRef NF As Integer, ByRef Somng As Single, _
  ByRef KZL As Integer, ByRef KFM As Integer, ByRef NFR As Integer, ByRef LKid As Integer, ByRef Remng As Single, ByRef GEW As Single, ByRef FEHL As TYFEH)

  '
  '

  Declare Sub BASREZ Lib "REZFARB.DLL" (ByRef KFIT As Integer, ByRef NF As Integer, ByRef BaMng As Single, _
  ByRef NWE As Integer, ByRef KM As Integer, ByRef RCal As Single, ByRef SorKrit As Single, ByRef GEW As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  'Dosierung berücksichtigen
  '
  '
  '
  '
  '
  Declare Sub REZANGL Lib "REZFARB.DLL" Alias "REZANGLN" (ByRef NF As Integer, ByRef Ivol As Integer, ByRef Inf As Integer, ByRef Sploe As Single, ByRef LID As Integer, ByRef FaMng As Single, _
                                                   ByRef Proz As Single, ByRef Prob As Single, ByRef EFF As Single, ByRef Spz As Single, ByRef ICH As Integer, ByRef DosMin As Single, ByRef FEHL As TYFEH)
  'Alternativ (berücksichtigt Farbabstand)
  Declare Sub REZANGLD Lib "REZFARB.DLL" Alias "REZANGLD" (ByRef NF As Integer, ByRef Ivol As Integer, ByRef Inf As Integer, ByRef Sploe As Single, ByRef LID As Integer, ByRef FaMng As Single, _
                                                    ByRef Proz As Single, ByRef Prob As Single, ByRef EFF As Single, ByRef Spz As Single, ByRef ICH As Integer, ByRef DosMin As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  'Remissions- bzw. Transmissionswerte aus Rezept berechnen
  '
  '
  '
  '
  Declare Sub MSHREZ Lib "REZFARB.DLL" (ByRef IPRN As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef RCal As Single, _
  ByRef KF As Integer, ByRef LK As Integer, ByRef RezMng As Single, ByRef Dicke As Single, _
  ByRef NST As Integer, ByRef Cst As Single, ByRef Gruges As Single, ByRef SoKrit As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  '
  '
  'Berechnung von Korrekturen für die Reflexionswerte (u.U. unter Berücksichtigung von Hilfkorrekturen
  '
  '
  '

  Declare Sub REFKOR Lib "REZFARB.DLL" (ByRef NWE As Integer, ByRef KM As Integer, ByRef RNAC As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  '
  '
  '
  'Berechnung von Farbrezeptkorrekturen
  '
  '
  '

  '
  '
  Declare Sub REZKOR Lib "REZFARB.DLL" (ByRef KFIT As Integer, ByRef KF As Integer, ByRef Remng As Single, ByRef Dicke As Single, ByRef GGE As Single, ByRef GEW As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  '
  'Remissions- bzw. Transmissionswerte aus Rezept berechnen
  '
  '
  '
  '
  Declare Sub MSHKOR Lib "REZFARB.DLL" (ByRef IPRN As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef RCal As Single, _
  ByRef KF As Integer, ByRef LK As Integer, ByRef Remng As Single, ByRef Dicke As Single, _
  ByRef NST As Integer, ByRef Cst As Single, ByRef Gruges As Single, ByRef SoKrit As Single, ByRef FEHL As TYFEH)

  '
  '
  'Rezeptrogramm beenden (z.B. Deallocieren)
  '
  '
  '
  '
  Declare Sub REZEND Lib "REZFARB" (ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  'Reine Mengen aus Normierungs-Mengen gemäß Normierung INF
  '
  '
  '
  '
  '
  '
  Declare Sub RCHREI Lib "REZUMRE.DLL" (ByRef NF As Integer, ByRef Ivol As Integer, ByRef Inf As Integer, ByRef Sploe As Single, _
  ByRef BaMng As Single, ByRef FaMng As Single, ByRef Proz As Single, ByRef Prob As Single, ByRef EFF As Single, ByRef Spz As Single, _
  ByRef Ichf As Integer, ByRef FEHL As TYFEH)
  '
  '
  '
  'Normierungs-Mengen gemäß Normierung INF aus reinen Mengen berechnen
  '
  '
  Declare Sub RCHMNG Lib "REZUMRE.DLL" (ByRef NF As Integer, ByRef Ivol As Integer, ByRef Inf As Integer, ByRef Sploe As Single, _
  ByRef BaMng As Single, ByRef FaMng As Single, ByRef Proz As Single, ByRef Prob As Single, ByRef EFF As Single, ByRef Spz As Single, _
  ByRef Ichf As Integer, ByRef FEHL As TYFEH)

  '
  'Normierungsmengen aus Reinen Farb-/Bindemittelmengen berechnen
  '
  'MngRei: Reine Gesamtmenge
  'MngBA : Batchmenge gemäß INF
  'MngAkt: Normierungsmenge gemäß INO
  'MngZae: Normierungsmenge für Zähler gemäß INP für Prozentigkeit
  'MngNen: Normierungsmenge für Nenner gemäß INQ für Prozentigkeit
  '
  '
  '
  Declare Sub RCHAMX Lib "REZUMRE.DLL" (ByRef NF As Integer, ByRef Ivol As Integer, ByRef Inf As Integer, ByRef Ino As Integer, ByRef INP As Integer, ByRef INQ As Integer, _
  ByRef Sploe As Single, ByRef FaMng As Single, ByRef Proz As Single, ByRef Prob As Single, ByRef EFF As Single, ByRef Spz As Single, ByRef Ichf As Integer, ByRef OP As Byte, _
  ByRef MngRei As Single, ByRef MngBA As Single, ByRef MngAkt As Single, ByRef MngZae As Single, ByRef MngNen As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  '
  'Berechnung der Zuwaage (für INF=0)
  '
  '
  '
  '    DIM Remng,ReAlt,ReZuw(NF-1) as single
  '
  '
  Declare Sub RCHZUW Lib "REZUMRE.DLL" (ByRef NF As Integer, _
  ByRef Remng As Single, ByRef ReAlt As Single, ByRef ReZuw As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  '4. Qualitätskontrolle
  '
  '
  '
  '
  '
  Declare Sub GETFDMQUA Lib "FARBQUAL.DLL" Alias "GETFDM" _
    (ByRef KWW As Integer, ByRef Aka As Single, ByRef Fopt As Single, ByRef CDE As Byte, _
   ByRef Nlz As Integer, ByRef BeID As Integer, ByRef NormID As Integer, ByRef GewNorm As Single, ByRef Fakt As Single, _
   ByRef NWE As Integer, ByRef KM As Integer, ByRef Wsol As Single, ByRef E As Single, _
   ByRef IhrmWin As Integer, ByRef GewIhrm As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  '
  'Start Qualitätskontrolle
  '
  '
  Declare Sub QUABEG Lib "FARBQUAL.DLL" (ByRef NWE As Integer, ByRef KM As Integer, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  '
  '
  'Menueparameter
  '
  '
  Declare Sub QUAMEN Lib "FARBQUAL.DLL" Alias "FRBMEN" _
  (ByRef IDaeArt As Integer, ByRef ALP As Single, ByRef GMAX As Single, ByRef RWGEW As Single, ByRef JABST As Integer, ByRef AbsID As Integer, ByRef BwID As Integer, ByRef FsID As Integer, _
   ByRef WelFst As Single, ByRef Schwell As Single, ByRef WLCH As Single, _
   ByRef KdeWS As Single, ByRef VhWS As Single, ByRef ParamMerk As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  '
  'BwID aus Messungen ermitteln
  '
  '
  '
  Declare Function BWEBER Lib "FARBQUAL.DLL" _
  (ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, ByRef FEHL As TYFEH) As Integer

  '
  '
  '
  'Farbstärke (deckend)
  '

  '
  '
  Declare Sub FASDEK Lib "FARBQUAL.DLL" _
  (ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QUKS As TyQual, ByRef TYKS As TyRwert, ByRef RKS As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '    

  '
  '
  'Farbstärke (transparent)
  '
  '
  '
  Declare Sub FASTRA Lib "FARBQUAL.DLL" _
  (ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QUEX As TyQual, ByRef TYEX As TyRwert, ByRef REX As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  'Farbstärke (Textil)
  '
  '
  '
  Declare Sub FASTEX Lib "FARBQUAL.DLL" _
  (ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QUEX As TyQual, ByRef TYEX As TyRwert, ByRef REX As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  'Farbstärke (deckend Messung(weiß) + Messung(schwarz))
  'xxxDUN Messungen Weißpigment,Schwarzpigment und Grauabstufung
  'xxxSUN zugehörige Absorptions und Streukoeffizienten
  '
  '
  '
  Declare Sub FSTDEK Lib "FARBQUAL.DLL" _
  (ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QURUNND As TyQual, ByRef TYRUNND As TyRwert, ByRef RUNND As Single, _
  ByRef QUCRMAX As TyQual, ByRef TYCRMAX As TyRwert, ByRef RCRMAX As Single, _
  ByRef QUEX As TyQual, ByRef TYEX As TyRwert, ByRef REX As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef QUASUN As TyQual, ByRef TYASUN As TyRwert, ByRef RSUN As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  'Farbstärke (transparent Messung(weiß) + Messung(schwarz))
  '
  '
  '
  Declare Sub FSTTRA Lib "FARBQUAL.DLL" _
  (ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QURUNND As TyQual, ByRef TYRUNND As TyRwert, ByRef RUNND As Single, _
  ByRef QUCRMAX As TyQual, ByRef TYCRMAX As TyRwert, ByRef RCRMAX As Single, _
  ByRef QUEX As TyQual, ByRef TYEX As TyRwert, ByRef REX As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  '
  'Pseudodeckvermögen aus Messung Weißpigment, Schwarzpigment und Grauabstufung (s. VhWS)
  '
  '
  '
  Declare Sub DEKDEK Lib "FARBQUAL.DLL" _
  (ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QuRunnd As TyQual, ByRef TyRunnd As TyRwert, ByRef Runnd As Single, _
  ByRef QUCRMAX As TyQual, ByRef TYCRMAX As TyRwert, ByRef RCRMAX As Single, _
  ByRef QUABSTR As TyQual, ByRef TYABSTR As TyRwert, ByRef RABSTR As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef QUASUN As TyQual, ByRef TYASUN As TyRwert, ByRef RASUN As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  '
  '
  '
  'Deckvermögen aus Messungen über weißem und schwarzem Untergrund
  '
  '
  '
  Declare Sub DEKTRA Lib "FARBQUAL.DLL" _
  (ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QuRunnd As TyQual, ByRef TyRunnd As TyRwert, ByRef Runnd As Single, _
  ByRef QUCRMAX As TyQual, ByRef TYCRMAX As TyRwert, ByRef RCRMAX As Single, _
  ByRef QUABSTR As TyQual, ByRef TYABSTR As TyRwert, ByRef RABSTR As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  'Remission für unendlich dicke Schicht aus Messungen über weißem und schwarzem Untergrund
  '
  '
  '
  '
  Declare Sub UNENDL Lib "FARBQUAL.DLL" _
  (ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QuRunnd As TyQual, ByRef TyRunnd As TyRwert, ByRef Runnd As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef FEHL As TYFEH)
  '
  '
  '
  'Fasfog
  '
  '
  Declare Sub FASFOG Lib "FARBQUAL.DLL" (ByRef IPRI As Integer, ByRef KW As Integer, ByRef NWEL As Integer, ByRef KML As Integer, ByRef NQU As Integer, ByRef KLH As Integer, _
                  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
                  ByRef NPAM As Integer, ByRef PARAM As TyWert, ByRef WERT As Double, ByRef FEHL As TYFEH)


  '
  '
  Declare Sub FASNEU Lib "FARBQUAL.DLL" (ByRef KW As Integer, ByRef NWEL As Integer, ByRef KML As Integer, ByRef KLH As Integer, _
                  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
                 ByRef NPAM As Integer, ByRef PARAM As TyWert, ByRef WERT As Double, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '5.Spezialprogramme
  '
  '
  '
  '
  '
  '
  '
  '
  '    
  'Remissionskurven aus Konzentrationen berechnen
  'KFIT=2 Berechnung der optischen Konstanten
  'KFIT=1 Remissionswerte für vorgegebene Farbmittelmenge (s.QUREDAB)
  '
  'Deckende Schicht 
  '
  Declare Sub FASDEKS Lib "FARBQUAL.DLL" _
  (ByRef KFIT As Integer, ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QUKS As TyQual, ByRef TYKS As TyRwert, ByRef RKS As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef NFAECHAR As Integer, ByRef NFAEWRT As Integer, ByRef FAECHAR As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  'Transparente Schicht 
  '
  Declare Sub FASTRAS Lib "FARBQUAL.DLL" _
  (ByRef KFIT As Integer, ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QUKS As TyQual, ByRef TYKS As TyRwert, ByRef RKS As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef NFAECHAR As Integer, ByRef NFAEWRT As Integer, ByRef FAECHAR As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  'Berechnung von L*,C*,h-Werten für vorgegebene Standardfarbtiefe (BwID) und vorgegebene  
  '
  '     ILCH=1 L* aus C*,h  berechnen
  '     ILCH=2 C* aus L*,h  berechnen
  '     ILCH=3 h  aus C*,L* berechnen
  '

  '
  '
  Declare Sub LCHSTBW Lib "FARBQUAL.DLL" (ByRef ILCH As Integer, ByRef LCH As Single, ByRef FEHL As TYFEH)
  '
  '     Y (Tristimuluswert)
  '     S=sqrt((x-x0)**2+(y-y0)**2) (Helmholzkoordinate)
  '     W=Atan(y-y0,x-x0) Helmholzwinkel
  '     x0,y0 Unbuntpunkt auf der Schuhsohle
  '
  '
  '     ILCH=1 Y aus S,W  berechnen
  '     ILCH=2 S aus Y,W  berechnen
  '     ILCH=3 W aus Y,S berechnen
  '
  Declare Sub YSWSTBW Lib "FARBQUAL.DLL" (ByRef IYSW As Integer, ByRef YSW As Single, ByRef FEHL As TYFEH)



  'Remissionskurven aus Konzentrationen berechnen
  'KFIT=2 Berechnung der optischen Konstanten
  'KFIT=1 Remissionswerte für vorgegebene Farbmittelmenge (s.QUREDAB)

  Declare Sub FSTTRAS Lib "FARBQUAL.DLL" _
  (ByRef KFIT As Integer, ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QURUNND As TyQual, ByRef TYRUNND As TyRwert, ByRef RUNND As Single, _
  ByRef QUCRMAX As TyQual, ByRef TYCRMAX As TyRwert, ByRef RCRMAX As Single, _
  ByRef QUABSTR As TyQual, ByRef TYABSTR As TyRwert, ByRef ABSTR As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef NFAECHAR As Integer, ByRef NFAEWRT As Integer, ByRef FAECHAR As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)

  '
  '

  Declare Sub FSTDEKS Lib "FARBQUAL.DLL" _
  (ByRef KFIT As Integer, ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QURUNND As TyQual, ByRef TYRUNND As TyRwert, ByRef RUNND As Single, _
  ByRef QUCRMAX As TyQual, ByRef TYCRMAX As TyRwert, ByRef RCRMAX As Single, _
  ByRef QUABSTR As TyQual, ByRef TYABSTR As TyRwert, ByRef ABSTR As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef QUASUN As TyQual, ByRef TYASUN As TyRwert, ByRef RASUN As Single, _
  ByRef NFAECHAR As Integer, ByRef NFAEWRT As Integer, ByRef FAECHAR As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  '    
  'Remissionskurven aus Konzentrationen berechnen
  'KFIT=2 Berechnung der optischen Konstanten
  'KFIT=1 Remissionswerte für vorgegebene Farbmittelmenge (s.QUREDAB)
  '
  '
  Declare Sub DEKTRAS Lib "FARBQUAL.DLL" _
  (ByRef KFIT As Integer, ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QURUNND As TyQual, ByRef TYRUNND As TyRwert, ByRef RUNND As Single, _
  ByRef QUCRMAX As TyQual, ByRef TYCRMAX As TyRwert, ByRef RCRMAX As Single, _
  ByRef QUABSTR As TyQual, ByRef TYABSTR As TyRwert, ByRef ABSTR As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef NFAECHAR As Integer, ByRef NFAEWRT As Integer, ByRef FAECHAR As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  '    
  'Remissionskurven aus Konzentrationen berechnen
  'KFIT=2 Berechnung der optischen Konstanten
  'KFIT=1 Remissionswerte für vorgegebene Farbmittelmenge (s.QUREDAB)
  '
  '
  Declare Sub DEKDEKS Lib "FARBQUAL.DLL" _
  (ByRef KFIT As Integer, ByRef kw As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QURUNND As TyQual, ByRef TYRUNND As TyRwert, ByRef RUNND As Single, _
  ByRef QUCRMAX As TyQual, ByRef TYCRMAX As TyRwert, ByRef RCRMAX As Single, _
  ByRef QUABSTR As TyQual, ByRef TYABSTR As TyRwert, ByRef ABSTR As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef QUASUN As TyQual, ByRef TYASUN As TyRwert, ByRef RASUN As Single, _
  ByRef NFAECHAR As Integer, ByRef NFAEWRT As Integer, ByRef FAECHAR As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  ' Berechnung der Remission/Transmission aus den optischen Daten (Streuung und Absorption)
  '
  '
  Declare Sub RETRMIS Lib "FARBQUAL.DLL" _
     (ByRef KU As Integer, ByRef ITR As Integer, ByRef KW As Integer, ByRef NWEL As Integer, ByRef KML As Integer, ByRef NQU As Integer, _
      ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
      ByRef QUREDAX As TyQual, ByRef TYREDAX As TyRwert, ByRef RDAB As Single, _
      ByRef QUABSTR As TyQual, ByRef TYABSTR As TyRwert, ByRef RABSTR As Single, ByRef FEHL As TYFEH)


  'Ende Qualitätskontrolle
  '
  '
  Declare Sub QUAEND Lib "FARBQUAL.DLL" (ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '6. Mischen Produktionspartie
  '
  '
  '
  'Winkel, Normlichtarten und GK-Werte
  '
  '

  '
  '
  Declare Sub GETFDMMIS Lib "MISCHEN.DLL" Alias "GETFDM" _
    (ByRef KWW As Integer, ByRef Aka As Single, ByRef Fopt As Single, ByRef CDE As Byte, _
   ByRef Nlz As Integer, ByRef BeID As Integer, ByRef NormID As Integer, ByRef GewNorm As Single, ByRef Fakt As Single, _
   ByRef NWE As Integer, ByRef KM As Integer, ByRef Wsol As Single, ByRef E As Single, _
   ByRef IhrmWin As Integer, ByRef GewIhrm As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  '
  'Menueparameter
  '
  '
  Declare Sub MISMEN Lib "MISCHEN.DLL" Alias "FRBMEN" _
  (ByRef IDaeArt As Integer, ByRef ALP As Single, ByRef GMAX As Single, ByRef RWGEW As Single, ByRef JABST As Integer, ByRef AbsID As Integer, ByRef BwID As Integer, ByRef FsID As Integer, _
  ByRef WelFst As Single, ByRef Schwell As Single, ByRef WLCH As Single, _
  ByRef KdeWS As Single, ByRef VhWS As Single, ByRef ParamMerk As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  Declare Sub MISCHEN Lib "MISCHEN.DLL" _
  (ByRef IWESC As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDAB As TyQual, ByRef TYREDAB As TyRwert, ByRef RDAB As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  '
  '
  '
  'Start FarbWerte
  '    '
  '
  '
  '7. FarbWerte für Qualitätskontrolle

  '
  '
  'Start FarbWerte
  '
  '
  Declare Sub FARBEG Lib "FARKOORD.DLL" (ByRef NWE As Integer, ByRef KM As Integer, ByRef FEHL As TYFEH)
  '

  '
  '
  '
  'Winkel, Normlichtarten und GK-Werte
  '
  Declare Sub GETFDMFAR Lib "FARKOORD.DLL" Alias "GETFDM" _
    (ByRef KWW As Integer, ByRef Aka As Single, ByRef Fopt As Single, ByRef CDE As Byte, _
   ByRef Nlz As Integer, ByRef BeID As Integer, ByRef NormID As Integer, ByRef GewNorm As Single, ByRef Fakt As Single, _
   ByRef NWE As Integer, ByRef KM As Integer, ByRef Wsol As Single, ByRef E As Single, _
   ByRef IhrmWin As Integer, ByRef GewIhrm As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef FEHL As TYFEH)

  '
  'Normlichtarten für  380nm-720nm,10nm
  '
  Declare Sub GET6164 Lib "FARKOORD.DLL" (ByRef Nlz As Integer, ByRef NormID As Integer, ByRef Fakt As Single, ByRef NWE As Integer, ByRef Wsol As Single, ByRef E As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  'Menueparameter
  '
  '
  '
  '
  Declare Sub FARMEN Lib "FARKOORD.DLL" Alias "FRBMEN" _
  (ByRef IDaeArt As Integer, ByRef ALP As Single, ByRef GMAX As Single, ByRef RWGEW As Single, ByRef JABST As Integer, ByRef AbsID As Integer, ByRef BwID As Integer, ByRef FsID As Integer, _
   ByRef WelFst As Single, ByRef Schwell As Single, ByRef WLCH As Single, _
   ByRef KdeWS As Single, ByRef VhWS As Single, ByRef ParamMerk As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  Declare Sub FARKOORD Lib "FARKOORD.DLL" _
  (ByRef NWE As Integer, ByRef KM As Integer, ByRef Nqu As Integer, _
  ByRef QUREDAM As TyQual, ByRef TYREDAM As TyRwert, ByRef RDAM As Single, _
  ByRef QUREDUN As TyQual, ByRef TYREDUN As TyRwert, ByRef RDUN As Single, _
  ByRef AufgArt As Byte, ByRef AuswAnz As Integer, ByRef AuswID As Integer, _
  ByRef Npam As Integer, ByRef Param As TyWert, ByRef Wert As Double, ByRef FEHL As TYFEH)
  '
  '
  'Berechnung von XYZ Lab und LCH (nach DIN 6174 oder DIN 6176; s.-JABST in FRBMEN)

  Declare Sub XYZLABCH Lib "FARKOORD.DLL" _
  (ByRef NWE As Integer, ByRef IRT As Integer, ByRef KW As Integer, ByRef KNO As Integer, ByRef R As Single, ByRef XYZ As Single, ByRef ALAB As Single, ByRef ALCH As Single, ByRef IER As Integer)
  '
  'Berechnung von DE,DL,DC,DH,DA,DB (nach DIN 6174 oder DIN 6176; s.-JABST in FRBMEN) aus Reflexionswerten

  Declare Sub DIFFREF Lib "FARKOORD.DLL" _
  (ByRef NWE As Integer, ByRef IRT As Integer, ByRef KW As Integer, ByRef KNO As Integer, ByRef RB As Single, ByRef RP As Single, ByRef DE As Single, ByRef DL As Single, ByRef DC As Single, ByRef DH As Single, ByRef DA As Single, ByRef DB As Single, ByRef IER As Integer)
  '
  'Berechnung von DE,DL,DC,DH,DA,DB (nach DIN 6174 oder DIN 6176; s.-JABST in FRBMEN) aus XYZ-Werten

  Declare Sub DIFFXYZ Lib "FARKOORD.DLL" _
  (ByRef KNO As Integer, ByRef XYZV As Single, ByRef XYZN As Single, ByRef DE As Single, ByRef DL As Single, ByRef DC As Single, ByRef DH As Single, ByRef DA As Single, ByRef DB As Single, ByRef IER As Integer)

  'Spezialprogramm zur Berechnung von dEeff nach VW - Vorgaben
  '
  '
  '
  Declare Sub DEEFFVW Lib "FARKOORD.DLL" _
    (ByRef NWE As Integer, ByRef KW As Integer, ByRef KNO As Integer, _
    ByRef RDAT As Single, ByRef RDAP As Single, ByRef SL As Single, ByRef SC As Single, ByRef SH As Single, ByRef SA As Single, ByRef SB As Single, ByRef DEFF As Single, ByRef IER As Integer)
  '
  'Ende FarbWerte
  '
  '
  Declare Sub FAREND Lib "FARKOORD.DLL" (ByRef FEHL As TYFEH)
  '
  '
  '8. Mittelwert und Standardabweichung
  '
  '
  '
  '
  Declare Sub RwertMit Lib "RWERTMIT.DLL" (ByRef Aka As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef NANZ As Integer, ByRef KM As Integer, ByRef NWE As Integer, ByRef CM As Single, ByRef RMIT As Single, ByRef RU As Single, ByRef R As Single, ByRef RO As Single, ByRef FEHL As TYFEH)
    
  '
  '
  '
  'Neue R-Werte
  '
  '
  Declare Sub RwertAKA Lib "RWERTMIT.DLL" (ByRef Aka As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef KM As Integer, ByRef NWE As Integer, ByRef RR As Single, ByRef R2 As Single, ByRef R1 As Single, ByRef RN As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  '
  '9. Rwerte aus FarbWerten berechnen
  '
  '
  '
  '
  'Winkel, Normlichtarten und GK-Werte
  '
  Declare Sub GETFDMFUM Lib "FARBUMRE.DLL" Alias "GETFDM" _
   (ByRef KWW As Integer, ByRef Aka As Single, ByRef Fopt As Single, ByRef CDE As Byte, _
   ByRef Nlz As Integer, ByRef BeID As Integer, ByRef NormID As Integer, ByRef GewNorm As Single, ByRef Fakt As Single, _
   ByRef NWE As Integer, ByRef KM As Integer, ByRef Wsol As Single, ByRef E As Single, _
   ByRef IhrmWin As Integer, ByRef GewIhrm As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef FEHL As TYFEH)

  '
  'Menueparameter
  '
  '
  '
  '
  Declare Sub FUMMEN Lib "FARBUMRE.DLL" Alias "FRBMEN" _
  (ByRef IDaeArt As Integer, ByRef ALP As Single, ByRef GMAX As Single, ByRef RWGEW As Single, ByRef JABST As Integer, ByRef AbsID As Integer, ByRef BwID As Integer, ByRef FsID As Integer, _
   ByRef WelFst As Single, ByRef Schwell As Single, ByRef WLCH As Single, _
   ByRef KdeWS As Single, ByRef VhWS As Single, ByRef ParamMerk As Single, ByRef FEHL As TYFEH)
  '

  Declare Sub FARBUMRE Lib "FARBUMRE.DLL" _
  (ByRef NWE As Integer, ByRef KM As Integer, ByRef JSCH As Integer, ByRef JFL As Integer, ByRef JFW As Integer, ByRef NLQ As Integer, ByRef FAKO As Single, ByRef XYZS As Single, ByRef XYZI As Single, _
  ByRef TYREINP As TyRwert, ByRef REINP As Single, ByRef TYREDAT As TyRwert, ByRef REDAT As Single, ByRef FEHL As TYFEH)
  '
  '

  '
  '
  '
  '
  '10. Farbraumberechnung
  '

  '


  Declare Sub GETFDMRAU Lib "REZRAUM.DLL" Alias "GETFDM" _
   (ByRef KWW As Integer, ByRef Aka As Single, ByRef Fopt As Single, ByRef CDE As Byte, _
   ByRef Nlz As Integer, ByRef BeID As Integer, ByRef NormID As Integer, ByRef GewNorm As Single, ByRef Fakt As Single, _
   ByRef NWE As Integer, ByRef KM As Integer, ByRef Wsol As Single, ByRef E As Single, _
   ByRef IhrmWin As Integer, ByRef GewIhrm As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  '
  '
  'Drucken in Farbraumrechnung
  '
  '
  '
  '
  '

  Declare Sub REZDRURAU Lib "REZRAUM.DLL" Alias "REZDRU" (ByRef FEHL As TYFEH)
  '
  '
  '
  'Beginn Farbraumrechnung
  '
  '
  Declare Sub REZBEGRAU Lib "REZRAUM.DLL" Alias "REZBEG" (ByRef NWE As Integer, ByRef KM As Integer, ByRef NME As Integer, _
                     ByRef NFG As Integer, ByRef NFV As Integer, ByRef NPQ1 As Integer, ByRef NLQ As Integer, ByRef NHLF As Integer, _
                     ByRef MEE As Integer, ByRef MEM As Integer, ByRef NSPE As Integer, ByRef FEHL As TYFEH)
  '

  '
  '
  'Menüparameter für Farbraumrechnung
  '
  '
  '
  '
  '
  '

  Declare Sub REZMENRAU Lib "REZRAUM.DLL" Alias "REZMEN" _
  (ByRef JABST As Integer, ByRef FU As Single, ByRef WLCH As Single, _
  ByRef DEHLF As Single, ByRef DEGUT As Single, ByRef DEOK As Single, _
  ByRef FDE As Single, ByRef GDE As Single, ByRef GGE As Single, _
  ByRef FTO As Single, ByRef ATO As Single, ByRef DEL As Single, ByRef SPR As Single, _
  ByRef GXG As Single, ByRef GXD As Single, ByRef DTO As Single, ByRef GTO As Single, _
  ByRef NPX As Integer, ByRef EPS As Single, ByRef MINREZ As Integer, _
  ByRef MAXREZ As Integer, ByRef KWH As Integer, ByRef ITM As Integer, ByRef Aka As Single, ByRef IGX As Integer, _
  ByRef KWM As Integer, ByRef KGX As Integer, ByRef ICHI As Integer, ByRef ISOR As Integer, _
  ByRef GKID As Integer, ByRef ParaMerk As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  ' Farbmittel und Grunddaten übernehmen
  '
  '
  '
  '
  '

  Declare Sub REZGRURAU Lib "REZRAUM.DLL" Alias "REZGRU" (ByRef NF As Integer, ByRef MngRei As Single, ByRef OP As Byte, _
  ByRef KTO As Byte, ByRef Ichf As Integer, _
  ByRef Preis As Single, ByRef Fst As Single, ByRef NWE As Integer, ByRef KM As Integer, _
  ByRef NPQ1 As Integer, ByRef NST As Integer, ByRef CST As Single, ByRef Grund As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  'Mengen und Reflexionswerte übernehmen
  '
  '
  '
  '
  '
  Declare Sub REZMGRFRAU Lib "REZRAUM.DLL" Alias "REZMGRF" (ByRef IPGR As Integer, ByRef NUU As Integer, ByRef NUV As Integer, ByRef NUN As Integer, _
  ByRef NWE As Integer, ByRef KM As Integer, ByRef Retr As Integer, ByRef KWB As Integer, _
  ByRef Runt As Single, ByRef RVOR As Single, ByRef RNAC As Single, ByRef NF As Integer, ByRef Remng As Single, ByRef Dicke As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '

  '
  '
  '
  'Aus BaMng, BuMng BoMng,Proz,Prob,OP,ICHF usw. und den unteren und oberen Grenzen für die Normierungsmenge (MngMin,MngMax)
  'und die Normierungsprozentigkeiten (ProzMin,ProzMax) werden Gleichungen und Ungleichungen zur Beschränkung der reinen Farb-/Bindemttelmengen hergeleitet
  'Diese stehen FORTRAN-intern den Programmen REZREZ,REZBAS,REZKOR,MSHREZ,MSHKOR,HLFKOR zur Verfügung.
  '
  '
  '
  '
  '
  Declare Sub RCHLIMRAU Lib "REZRAUM.DLL" Alias "RCHLIM" (ByRef NF As Integer, ByRef Ivol As Integer, _
  ByRef Inf As Integer, ByRef Ino As Integer, ByRef INP As Integer, ByRef INQ As Integer, ByRef INM As Integer, ByRef IGX As Integer, _
  ByRef MngMin As Single, ByRef MngMax As Single, ByRef ProzMin As Single, ByRef ProzMax As Single, ByRef Sploe As Single, _
  ByRef BaMng As Single, ByRef FaMng As Single, ByRef Proz As Single, ByRef Prob As Single, ByRef EFF As Single, ByRef Spz As Single, ByRef Ichf As Integer, _
  ByRef OP As Byte, ByRef BuMng As Single, ByRef BoMng As Single, ByRef Bel As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  'Grenzen für Farbraumrechnung modifizieren
  '
  '
  '
  Declare Sub GRZRAUM Lib "REZRAUM.DLL" (ByRef FEHL As TYFEH)

  '
  '
  '
  'Farbraumberechnung
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  Declare Sub REZRAUM Lib "REZRAUM.DLL" (ByRef OPT As Integer, ByRef KWW As Integer, ByRef NLicht As Integer, ByRef KU As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef NME As Integer, ByRef NF As Integer, ByRef KZL As Integer, _
  ByRef Famng As Single, ByRef Remng As Single, ByRef RCAL As Single, ByRef AL As Single, ByRef WIU As Single, ByRef WIO As Single, ByRef WIS As Single, ByRef SoKrit As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  'Optimalrasumberechnumng
  '
  '
  'mit XYZ<XYZ(max)
  '
  Declare Sub OPTRAUM Lib "REZRAUM.DLL" (ByRef OPT As Integer, ByRef NLICHT As Integer, ByRef KZL As Integer, _
                       ByRef AL As Single, ByRef WIU As Single, ByRef WIO As Single, ByRef WIS As Single, ByRef SOKRIT As Single, ByRef FEHL As TYFEH)
  '
  'mit Optimalfarben
  '
  Declare Sub OPTIRAUM Lib "REZRAUM.DLL" (ByRef OPT As Integer, ByRef NLICHT As Integer, ByRef KZL As Integer, _
                     ByRef AL As Single, ByRef WIU As Single, ByRef WIO As Single, ByRef WIS As Single, ByRef SOKRIT As Single, ByRef FEHL As TYFEH)

  'Ende Raumrechnung
  '
  '
  '
  Declare Sub REZENDRAU Lib "REZRAUM.DLL" Alias "REZEND" (ByRef FEHL As TYFEH)
  '
  '
  '
  '
  'Waagensteuerung
  '
  '

  Declare Sub GETFDMWAG Lib "WaagSteu.DLL" Alias "GETFDM" _
  (ByRef KWW As Integer, ByRef Aka As Single, ByRef Fopt As Single, ByRef CDE As Byte, _
   ByRef Nlz As Integer, ByRef BeID As Integer, ByRef NormID As Integer, ByRef GewNorm As Single, ByRef Fakt As Single, _
   ByRef NWE As Integer, ByRef KM As Integer, ByRef Wsol As Single, ByRef E As Single, _
   ByRef IhrmWin As Integer, ByRef GewIhrm As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef FEHL As TYFEH)


  '
  '
  '
  '
  Declare Sub WAAGMAT Lib "WaagSteu.DLL" (ByRef ALPH As Single, ByRef Gh As Single, ByRef N As Integer, ByRef Mmess As Integer, ByRef WV As Single, ByRef WN As Single, _
                  ByRef KM As Integer, ByRef NWE As Integer, ByRef RV As Single, ByRef RN As Single, ByRef H As Single, ByRef FEHL As TYFEH)
  '
  '
  Declare Sub WAAGRUCK Lib "WaagSteu.DLL" (ByRef ALPH As Single, ByRef N As Integer, ByRef Mmess As Integer, ByRef WV As Single, ByRef WN As Single, _
              ByRef KM As Integer, ByRef NWE As Integer, ByRef RV As Single, ByRef H As Single, ByRef RR As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  Declare Sub WAAGLAB Lib "WaagSteu.DLL" (ByRef JABST As Integer, ByRef ALPH As Single, ByRef N As Integer, ByRef Q As Integer, ByRef G As Single, ByRef W As Single, ByRef Wu As Single, ByRef Wo As Single, ByRef WI As Single, _
                  ByRef KM As Integer, ByRef NWE As Integer, ByRef RS As Single, ByRef RI As Single, ByRef H As Single, ByRef RR As Single, _
                  ByRef Mm As Integer, ByRef Meq As Integer, ByRef Mq As Integer, ByRef A As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  Declare Sub WAAGREF Lib "WaagSteu.DLL" (ByRef ALPH As Single, ByRef N As Integer, ByRef Q As Integer, ByRef G As Single, ByRef W As Single, ByRef Wu As Single, ByRef Wo As Single, ByRef WI As Single, _
                  ByRef KM As Integer, ByRef NWE As Integer, ByRef RS As Single, ByRef RI As Single, ByRef H As Single, ByRef RR As Single, _
                  ByRef Mm As Integer, ByRef Meq As Integer, ByRef Mq As Integer, ByRef A As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  'MehrschichtenRezeptberechnung
  '
  '
  Declare Sub MEHRMHR Lib "GrdDaMSH.DLL" (ByRef P As Integer, ByRef QM As Integer, ByRef N As Integer, ByRef IFANR As Integer, _
                                         ByRef ANTEIL As Single, ByRef NF As Integer, ByRef MENG As Single, ByRef A As Single, ByRef S As Single, ByRef ST As Single, ByRef D As Single, ByRef KW As Integer, _
                                         ByRef RER As Single, ByRef RET As Single, ByRef RG As Single, ByRef TG As Single, ByRef NEUGE As Integer, ByRef IER As Integer)
  '
  '
  Declare Function TRUKORR Lib "GrdDaMSH.DLL" (ByRef RK As Single, ByRef KW As Integer, ByRef RETR As Integer) As Single
  '
  '


  Declare Sub XYZLABCHMHR Lib "GrdDaMSH.DLL" Alias "XYZLABCH" _
  (ByRef NWE As Integer, ByRef IRT As Integer, ByRef KW As Integer, ByRef KNO As Integer, ByRef R As Single, ByRef XYZ As Single, ByRef ALAB As Single, ByRef ALCH As Single, ByRef IER As Integer)

  Declare Sub DIFFREFMHR Lib "GrdDaMSH.DLL" Alias "DIFFREF" _
  (ByRef NWE As Integer, ByRef IRT As Integer, ByRef KW As Integer, ByRef KNO As Integer, ByRef RB As Single, ByRef RP As Single, ByRef DE As Single, ByRef DL As Single, ByRef DC As Single, ByRef DH As Single, ByRef DA As Single, ByRef DB As Single, ByRef IER As Integer)

  Declare Sub SCHICHT Lib "GrdDaMSH.DLL" (ByRef NF As Integer, ByRef ICHF As Integer, ByRef P As Integer, ByRef QM As Integer, ByRef N As Integer, ByRef ICH As Integer, ByRef ANTEIL As Single, ByRef NEUGE As Integer, ByRef IER As Integer)



  Declare Sub GRDBEGMHR Lib "GrdDaMSH.DLL" Alias "GRDBEG" _
  (ByRef NWE As Integer, ByRef KM As Integer, ByRef NME As Integer, ByRef NFG As Integer, ByRef NPQ1 As Integer, ByRef MMX As Integer, ByRef FEHL As TYFEH)
  '
  '
  'Normlichtarten,Winkel,GK-Werte
  '
  '
  '
  Declare Sub GETFDMMHR Lib "GrdDaMSH.DLL" Alias "GETFDM" _
   (ByRef KWW As Integer, ByRef Aka As Single, ByRef Fopt As Single, ByRef CDE As Byte, _
   ByRef Nlz As Integer, ByRef BeID As Integer, ByRef NormID As Integer, ByRef GewNorm As Single, ByRef Fakt As Single, _
   ByRef NWE As Integer, ByRef KM As Integer, ByRef Wsol As Single, ByRef E As Single, _
   ByRef IhrmWin As Integer, ByRef GewIhrm As Single, ByRef NGK As Integer, ByRef GK As Single, ByRef FEHL As TYFEH)

  '
  '
  '
  'Menuparameter für Grunddaten
  '
  '
  '
  Declare Sub GRDMENMHR Lib "GrdDaMSH.DLL" Alias "GRDMEN" _
  (ByRef FU As Single, ByRef EXPO As Single, ByRef GewR As Single, ByRef FTO As Single, ByRef GewDik As Single, ByRef JTM As Integer, ByRef Iglae As Integer, ByRef FEHL As TYFEH)
  '
  '
  '
  'Mengen und Remissionswerte
  '
  '
  '
  Declare Sub GRDMGRFMHR Lib "GrdDaMSH.DLL" Alias "GRDMGRF" _
  (ByRef NWE As Integer, ByRef KM As Integer, ByRef NME As Integer, ByRef KFM As Integer, ByRef NDGRE As Integer, ByRef NAZ As Integer, _
   ByRef ICH As Integer, ByRef AKOWRT As Single, ByRef GRSTR As Single, ByRef GRDAE As Single, ByRef GRCCQ As Byte, _
   ByRef DikAlt As Single, ByRef RMENG As Single, ByRef KWBUN As Integer, ByRef RETRUN As Integer, ByRef REDUN As Single, ByRef NRRWRT As Integer, _
   ByRef KWBRE As Integer, ByRef RETRRE As Integer, ByRef REDAM As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  'Alte Grunddaten übernehmen von Farbmittel KFU bis KFO
  '
  '
  '
  Declare Sub GRDGRUAMHR Lib "GrdDaMSH.DLL" Alias "GRDGRUA" _
  (ByRef KM As Integer, ByRef NWE As Integer, ByRef KFU As Integer, ByRef KFO As Integer, _
  ByRef NPQ1 As Integer, ByRef NST As Integer, ByRef Cst As Single, ByRef Grund As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  '
  'Startwerte für die Berechnung von neuen Grunddaten festlegen für Farbmittel KFU bis KFO
  '
  '
  '
  '
  Declare Sub GRDGRUNMHR Lib "GrdDaMSH.DLL" Alias "GRDGRUN" (ByRef KFU As Integer, ByRef KFO As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef NPQ1 As Integer, ByRef NST As Integer, ByRef Cst As Single, ByRef Grund As Single, ByRef FEHL As TYFEH)
  '
  '
  '
  '
  'Grenzen für Grunddaten festlegen (Grunddaten für Farbmittel <= KFI sind konstant)
  '(s. auch AKOWRTL,GRSTRL,GRDAEL,GRCCQL in Sub GRDMGRF)
  'Grunddaten von KFI+1 bis KFI+KFA = NAZ sind variabel
  '
  '
  '
  Declare Sub GRDLIMMHR Lib "GrdDaMSH.DLL" Alias "GRDLIM" (ByRef KFI As Integer, ByRef KFA As Integer, ByRef KM As Integer, ByRef NPQ1 As Integer, ByRef NST As Integer, ByRef CST As Single, ByRef FEHL As TYFEH)
  '
  '
  'Neue Grunddaten berechnen
  '
  '
  '
  '
  Declare Sub GRDCALCMHR Lib "GrdDaMSH.DLL" Alias "GRDCALC" (ByRef NWE As Integer, ByRef KM As Integer, _
  ByRef NPQ1 As Integer, ByRef NST As Integer, ByRef Cst As Single, ByRef DikNeu As Single, ByRef Grund As Single, ByRef GruGrz As Single, ByRef PHHI As Single, ByRef FEHL As TYFEH)
  '
  '
  'Remissions- bzw Transmissionswerte berechnen
  '
  '
  '
  '
  '
  '
  Declare Sub GRDREFLMHR Lib "GrdDaMSH.DLL" Alias "GRDREFL" (ByRef NWE As Integer, ByRef KM As Integer, ByRef NPQ1 As Integer, ByRef NPX As Integer, ByRef NST As Integer, ByRef Cst As Single, ByRef Grund As Single, _
  ByRef NAZ As Integer, ByRef NFG As Integer, _
  ByRef KWBRE As Integer, ByRef DikNeu As Single, ByRef RMENG As Single, ByRef REDAB As Single, ByRef GRNDALL As Single, ByRef FEHL As TYFEH)
  '
  '
  'Ende Grunddaten
  '
  '
  '
  '
  '
  Declare Sub GRDENDMHR Lib "GrdDaMSH.DLL" Alias "GRDEND" (ByRef FEHL As TYFEH)
  '
  '
  '
  'Grunddaten Drucken
  '
  '
  '
  Declare Sub GRDDRUMHR Lib "GrdDaMSH.DLL" Alias "GRDDRU" (ByRef FEHL As TYFEH)

  '
  'DECLARE-Statements für Splineinterpolation
  '
  '
  '
  Declare Sub SPLINDLL Lib "SPLIDLL.DLL" (ByRef N As Integer, ByRef X As Double, ByRef Y As Double, ByRef Y2 As Double, ByRef XSI As Double, ByRef YP1 As Double, ByRef YPN As Double)
  Declare Sub SPLITDLL Lib "SPLIDLL.DLL" (ByRef N As Integer, ByRef X As Double, ByRef Y As Double, ByRef Y2 As Double, ByRef XS As Double, ByRef YS As Double, ByRef XSI As Double, ByRef YSDX As Double)


  Public NME As Integer
  Public NPQ1 As Integer
  Public FEHL As TYFEH
  Sub FarKordAll(ByRef KNO As Integer, ByRef KW As Integer, ByRef NWE As Integer, ByRef KM As Integer, ByRef Winkel As AngGeos, _
  ByRef Nqu As Integer, ByVal RwertName() As String, ByVal RwertBem() As String, _
  ByRef Qurwert() As TyQual, ByRef TyRwert() As TyRwert, ByRef Rwert(,,) As Single, _
  ByRef QuUnt() As TyQual, ByRef TyUnt() As TyRwert, ByRef Runt(,,) As Single, _
  ByRef AufgArt() As Byte, ByRef AuswAnz As Integer, ByRef AuswID() As Integer, _
  ByRef Npam As Integer, ByRef Param() As TyWert, ByRef Wert(,) As Double, ByRef FEHL As TYFEH)
    '
    '
    '
    '
    '
    Dim SL As Single
    Dim SC As Single
    Dim SH As Single
    Dim Sa As Single
    Dim Sb As Single
    Dim DEff As Single
    Dim mdE As Single
    Dim maxdE As Single
    Dim maxWin As Single
    Dim KMM As Integer
    Dim jt As Integer
    Dim Ier As Integer
    Dim i As Integer
    Dim j As Integer
    Dim l As Integer
    Dim k As Integer
    Dim Ub As Integer
    Dim ChWert As String
    Dim jk As Integer
    Dim kl As Integer
    Dim jb As Integer
    Dim ist As Integer

    '
    '
    '
    '
    '


    Static A(2, 2) As Single
    Static Ia As Boolean = False
    Static HoRGB(2) As Single
    Dim RoGrBl(2) As Integer
    Dim XYZ(2) As Single
    Dim Alab(2) As Single
    Dim Alch(2) As Single

    Try
      Err.Number = 0
      Ub = UBound(TyRwert)
    Catch
      FEHL.Ifeh = 4145
      Exit Sub
    End Try
    ChWert = GetString(AufgArt)
    kl = ChWert.Substring(4, 1) - 1

    If IsNothing(TyRwert) OrElse TyRwert.Count = 0 Then Exit Sub
    Call FARKOORD(NWE, KM, Nqu, Qurwert(0), TyRwert(0), Rwert(0, 0, 0), _
                 QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
                 AufgArt(0), AuswAnz, AuswID(0), _
                 Npam, Param(0), Wert(0, 0), FEHL)
    '

    '
    '
    'AuswAnz Anzahl der Auswertungen
    '
    'Npam AuswAnz*Anzahl R-Werte
    '
    '
    '
    If FEHL.Ifeh <> 0 Then Exit Sub
    jt = 0
    For i = 0 To Npam - 1
      j = Fix(i / AuswAnz)
      If Param(i).Itp = 1 Then
        jt = j
      End If
      'J  Index für zugehörige R-Werte (Probe) bzw. Index für TyRwert
      'jt Index für zugehörige R-Wertte (Bezug) bzw. Index für TyRwert
      '
      'Mit
      'ChWert = TyWertString(Param(i))
      '
      '
      'Nummer des Winkels
      '
      'jk = ChWert.Substring(5, 1) - 1
      'jk = KW 
      '
      'Nummer der Lichtart
      '
      'kl = ChWert.Substring(4, 1) - 1
      'kl = KNO 
      '
      'param(i).itp 
      '=1 Bezug
      '=2 Probe
      'param(i).retr
      '=0 Remission
      '=1 Transmission
      'Param(i).KWB 
      '=1 über weißem Untergrund
      '=2 über schwarzem Untergrund
      'Param(i).RwertID
      'ID für R-Werte
      'Param(i).RwertNr
      'fortlaufende Nummer für R-Werte
      '
      'nwe Anzahl Wellenlängen
      'KM Anzahl Winkel
      'kno Nummer der aktuellen Lichtart
      'kw nummer des aktuellen Winkels
      'RWERT(j,kw,0.....nwe-1) =  R-Werte für j-te Messung und kw-ten Winkel
      '
      '
      Select Case Param(i).AuswID
        Case 0
          '

          '
          'Art des Farbstärkeangleichs
          '
          Wert(i, 2) = MenueParam.Menue.FstaeID
          '
          '
          'R-Werte gemessen oder berechnet usw.
          '
          ChWert = TyWertString(Param(i))
          Wert(i, 3) = GetDouble(GetBytes(Space(4) & ChWert.Substring(3, 1) & Space(3)))
          '
          '
          'Nummer des Winkels
          '
          jk = ChWert.Substring(5, 1) - 1
          ist = 5 - MenueParam.User.Winkel(jk).Chrm.Trim.Length
          Wert(i, 4) = GetDouble(GetBytes(Space(ist) & MenueParam.User.Winkel(jk).Chrm.Trim & Space(3)))
          '
          'Nummer der Lichtart
          '
          kl = ChWert.Substring(4, 1) - 1
          ChWert = TriSp(MenueParam.Normfa(kl).NormKenn)

          Wert(i, 6) = GetDouble(GetBytes(ChWert))
          '
          '
          '
          '
          '
          'Standardfarbtiefe (B-Wert)
          '
          ChWert = "        "
          jb = MenueParam.Menue.BwertID
          If jb >= 1 And jb <= 8 Then
            ChWert = MenueParam.Menue.BwertName(jb)
          End If
          ChWert = ChWert & Space(8 - Len(ChWert))
          Wert(i, 8) = GetDouble(GetBytes(ChWert))
          '
          'Reflexion oder Transmission
          '
          If Param(i).Retr = 0 Then
            ChWert = Space(4) & "R" & Space(3)
          Else
            ChWert = Space(4) & "T" & Space(3)
          End If
          Wert(i, 9) = GetDouble(GetBytes(ChWert))
          '
          '
        Case 17
          '
          '
          '
          '#######MANKISPEZIAL
          '
          '
          '
          '
          If Not MenueParam.VWTable Is Nothing Then
            If MenueParam.VWTable.Rows.Count <> 0 Then
              '
              'dEeff mdE maxdE gemäß VW-Vorgaben
              '
              If jt <> j And ihrum(Winkel(KW).Chrm) = 45 Then
                mdE = 0.0
                maxdE = 0.0
                KMM = 0
                For k = 0 To KM - 1

                  If ihrum(Winkel(k).Chrm) < 1000 Then
                    '
                    'Winkel/Messgeometrie ist nummerisch
                    '
                    '
                    MenueParam.VWDataView.RowFilter = "RWERTNAME='" & AddHkomE(RwertName(jt)) & "' AND GAM=" & ihrum(Winkel(k).Chrm)
                    If MenueParam.VWDataView.Count = 1 Then
                      SL = MenueParam.VWDataView(0)("SL")
                      SC = MenueParam.VWDataView(0)("SC")
                      SH = MenueParam.VWDataView(0)("SH")
                      Sa = MenueParam.VWDataView(0)("SA")
                      Sb = MenueParam.VWDataView(0)("SB")
                      KMM = KMM + 1
                      Call DEEFFVW(NWE, k + 1, KNO + 1, Rwert(jt, 0, 0), Rwert(j, 0, 0), SL, SC, SH, Sa, Sb, DEff, Ier)
                      mdE = mdE + DEff
                      If (DEff > maxdE) Then
                        maxdE = DEff
                        maxWin = ihrum(Winkel(k).Chrm)
                      End If
                    End If
                  End If
                Next k
                If KMM > 0 Then
                  Wert(i, 7) = mdE / KMM
                  Wert(i, 8) = maxdE
                  Wert(i, 9) = maxWin
                End If
              End If
              If jt <> j And ihrum(Winkel(KW).Chrm) < 1000 Then
                MenueParam.VWDataView.RowFilter = "RWERTNAME='" & AddHkomE(RwertName(jt)) & "' AND GAM=" & ihrum(Winkel(KW).Chrm)
                If MenueParam.VWDataView.Count = 1 Then
                  SL = MenueParam.VWDataView(0)("SL")
                  SC = MenueParam.VWDataView(0)("SC")
                  SH = MenueParam.VWDataView(0)("SH")
                  Sa = MenueParam.VWDataView(0)("SA")
                  Sb = MenueParam.VWDataView(0)("SB")
                  Call DEEFFVW(NWE, KW + 1, KNO + 1, Rwert(jt, 0, 0), Rwert(j, 0, 0), SL, SC, SH, Sa, Sb, DEff, Ier)
                  Wert(i, 6) = DEff
                  '
                End If
              End If
              '
            End If
          End If
          '
          '
          '
          '#######MANKISPEZIAL
          '
          '
          '
          '
        Case 18
          'Eintrag für MERK_KEN=RA MERK_ID=1153
          'wert(i,0)=9999.99
          'entsprechend für RB
          'wert(i,1)=9999.99
          'usw.
          'Falls MERK_TYP="T" wird der string CHWERT durch
          'Wert(i, nx) = GetDouble(GetBytes(ChWert))
          'in eine Double-Variable umgewandelt
          'X,Y,Z-Werte (MERK_ID=1214,1215,1216)
          For l = 0 To 2
            XYZ(l) = Wert(i, 59 + l)
          Next l
          '
          'Berechnung von XYZ, Lab und LCH (nach DIN 6174 oder DIN 6176; s.-JABST in FRBMEN)

          'Call XYZLABCH(NWE, Param(i).Retr, KW + 1, KNO + 1, Rwert(j, 0, 0), XYZ(0), Alab(0), Alch(0), Ier)
          '
          'Berechnung von DE,DL,DC,DH,DA,DB (nach DIN 6174 oder DIN 6176; s.-JABST in FRBMEN) aus Reflexionswerten

          'Call DIFFREF(NWE, Param(i).Retr, KW + 1, KNO + 1, Rwert(jt, 0, 0), Rwert(j, 0, 0), DE, DL, DC, DH, DA, DB, Ier)
          '
          'Berechnung von DE,DL,DC,DH,DA,DB (nach DIN 6174 oder DIN 6176; s.-JABST in FRBMEN) aus XYZ-Werten  

          'Call DIFFXYZ(KNO + 1, XYZBezug(0), XYZprobe(0), DE, DL, DC, DH, DA, DB, Ier)
          '
          '
          '
          '
          '
          'Prüfen Lichtart
          'MenueParam.Normfa(KNO).kenn
          '
          '
          'Prüfen auf Winkel
          'MsgBox(Winkel(KW).Chrm)
          '
          '
          'Umwandlung String in Double
          'Wert(i, 0) = GetDouble(GetBytes(ChWert))
          Call UserMerkmale(Param(i).AuswID, KNO, KW, MenueParam.Messg.MessgRwID, Param(i).RwertID, RwertName(j), RwertBem(j), i, Wert)
        Case (19)
          'Eintrag für MERK_KEN=SA MERK_ID=1217
          'wert(i,0)=9999.99
          'entsprechend für SB
          'wert(i,1)=9999.99
          'usw.
          'X,Y,Z-Werte 
          For l = 0 To 2
            XYZ(l) = Wert(i, 59 + l)
          Next l
        Case 20
          'Eintrag für MERK_KEN=TA MERK_ID=1281
          'wert(i,0)=9999.99
          'entsprechend für TB
          'wert(i,1)=9999.99
          'usw.
          'X,Y,Z-Werte 
          For l = 0 To 2
            XYZ(l) = Wert(i, 59 + l)
          Next l
        Case 26
          '
          '
          'Für AUSWID=26 werden RGB-Werte aus XYZ-Werten berechnet
          '
          '
          '
          '
          'RGB - Werte
          '
          If Not Ia Then
            Call ReadAHoRGB(A, HoRGB)
            Ia = True
          End If
          'X,Y,Z-Werte
          For l = 0 To 2
            XYZ(l) = Wert(i, 59 + l)
          Next l
          Call RGBFromXYZ(RoGrBl, XYZ, MenueParam.Normfa(kl).NormFakt, A, HoRGB)
          '
          'j=0 Rot
          'j=1 Grün
          'j=2 Blau
          '
          For l = 0 To 2
            Wert(i, l) = CSng(RoGrBl(l))
          Next l
      End Select
    Next i
  End Sub
  Sub UserMerkmale(AuswID As Integer, KNO As Integer, KW As Integer, MessgrwID As Integer, RwertID As Integer, RwertName As String, RwertBem As String, i As Integer, ByRef Wert(,) As Double)
    'AuswID = AuswahlID z.B. 18 für RA,RB usw.
    'KNO = Nummer der Lichtart in
    'MENUEPARAM.Normfa(KNO).NormKenn steht die Lichtartkennung
    'KW = Nummer des Wenkels in
    'MENUEPARAM.USER.WINKEL(KW),CHRM
    'steht die Kennung des Winkels
    'MessgrwID = ID für Messgerät
    'RwertID = ID für Rwerte
    'RwertName = Name des R-Wertes für RwertID und MessgrwID
    'RwertBem = Bemerkung für RwertID und MessgrwID
    'i = erster Index von Wert
    'Wert(i,ix) = Merkmalswerte für i-ten Merkmalsvektor ix=0.........63
    'z.B. AuswID=18 dann ist WERT(i,0) dem Merkmal RA zugeordnet
    '
    '
    'Strings (z.B. ChWert) der Länge 8 (notfalls mit Blank ergänzen) können mit
    'Wert(i, ix) = GetDouble(GetBytes(ChWert)) ix=0.........63
    'in Double verwandelt werden
    '
    '
    '
  End Sub
  Sub FarbDiff(ByVal Winkel As AngGeos, ByVal IRT As Integer, ByVal KW As Integer, ByVal KNO As Integer, ByVal RB() As Single, ByVal RP() As Single, ByRef DE As Single, ByRef DL As Single, ByRef DC As Single, ByRef DH As Single, ByRef DA As Single, ByRef DB As Single, ByRef IER As Integer)
    Dim Nwe As Integer
    Nwe = Winkel.Wsol.Nwe
    Call GetWinkNormGk("FAR", KW, MenueParam.Messg.Winkel, IER)
    Call GetMenueParam("FAR", IER)

    Call DIFFREF(Nwe, IRT, KW + 1, KNO + 1, RB(0), RP(0), DE, DL, DC, DH, DA, DB, IER)
  End Sub
  Sub LetNamBemDat(ByRef RwerteM As RefValue, ByRef RwerteS As RefValue, ByRef TyRwerb As TyRwert, ByRef Winkel As AngGeos, ByRef Ier As Integer)
    Dim kw As Integer
    RwerteS.DatTim = RwerteM.DatTim
    RwerteS.Name = RwerteM.Name
    RwerteS.Bem = RwerteM.Bem
    RwerteS.Banum = RwerteM.Banum
    RwerteS.Itp = RwerteM.Itp
    RwerteS.IVoNa = RwerteM.IVoNa
    RwerteS.Nr = RwerteM.Nr
    RwerteS.kwb = RwerteM.kwb
    RwerteS.ID = RwerteM.ID
    RwerteS.Gid = RwerteM.Gid
    RwerteS.ReTr = RwerteM.ReTr
    RwerteS.Iplott = RwerteM.Iplott
    RwerteS.Iarch = RwerteM.Iarch
    RwerteS.Cme = RwerteM.Cme
    Call ADDCurves(RwerteS.RefKurv)
    'For kw = 0 To Winkel.Km - 1
    'RwerteS.RefKurv.Add(Winkel(kw).Chrm, New CurveRef(Winkel.Wsol.Nwe))
    'Next kw
  End Sub
  Sub LetTyrwert(ByRef Winkel As AngGeos, ByRef Rwert As RefValue, ByRef TyRwert As TyRwert, ByRef iu As Integer, ByRef Rmit(,,) As Single)
    Rwert.ID = TyRwert.RwertID
    Rwert.Nr = TyRwert.RwertNr
    Rwert.kwb = TyRwert.KWB - 1
    Rwert.ReTr = TyRwert.Retr
    Rwert.IVoNa = CBool(TyRwert.IVONA)
    Call LetRwerte(Winkel, iu, Rmit, Rwert)
  End Sub

  Sub LetWeisungWerte(ByRef k As Integer, ByRef kw As Integer, ByRef Winkel As AngGeos, ByRef Npam As Integer, ByRef Param() As TyWert, ByRef Wert(,) As Double, ByRef ParWeisng As ValuesGrpsAssign, ByRef hug As Double, ByRef Ier As Integer)
    Dim i As Integer
    Dim l As Integer
    Dim AuswID As Integer
    Dim SpaltNr As Integer
    Dim Wrt As Double
    Dim PaNr As Integer
    Dim Ffor As String
    Dim tex As Object
    For l = 0 To ParWeisng.CountMerk - 1
      AuswID = ParWeisng.Merk(l).ID
      AuswID = Fix(AuswID / 64)
      SpaltNr = (ParWeisng.Merk(l).ID Mod 64) - 1
      If SpaltNr >= 0 Then
        For i = 0 To Npam - 1
          If Param(i).AuswID = AuswID Then
            If Param(i).RwertNr >= 0 Then
              If Wert(i, SpaltNr) < hug Then
                If ParWeisng.Merk(l).Typ = "T" Then
                  tex = GetString(GetBytes(Wert(i, SpaltNr)))
                Else
                  Wrt = Wert(i, SpaltNr) * ParWeisng.Merk(l).Fakt
                  Ffor = ParWeisng.Merk(l).Form
                  tex = Format(Wrt, Ffor)
                End If
                PaNr = Param(i).LNR
                ParWeisng(KeyRe(PaNr))(k)(Winkel(kw).Chrm)(l) = tex
              End If
            End If
          End If
        Next i
      End If
    Next l
  End Sub

  Sub GetTyrwert(ByRef Winkel As AngGeos, ByRef Rwert As RefValue, ByRef TyRwert As TyRwert, ByRef iu As Integer, ByRef Rmit(,,) As Single)
    TyRwert.RwertID = Rwert.ID
    TyRwert.RwertNr = Rwert.Nr
    TyRwert.KWB = Rwert.kwb + 1
    TyRwert.Retr = Rwert.ReTr
    TyRwert.IVONA = Abs(CInt(Rwert.IVoNa))
    Call GetRwerte(Winkel, iu, Rmit, Rwert)
  End Sub

  Sub GetNormFarben(ByRef Normfa As NormIlluminats, ByRef Nlz As Integer, ByRef NormID() As Integer, ByRef Gew() As Single, ByRef Fakt(,) As Single, ByRef NWE As Integer, ByRef E(,,) As Single, ByRef Ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Nlz = Normfa.Nlz
    NWE = Normfa(0).Normkurven(0).Nwe
    ReDim NormID(Nlz - 1)
    ReDim Gew(Nlz - 1)
    ReDim Fakt(Nlz - 1, 2)
    ReDim E(Nlz - 1, 2, NWE - 1)
    For i = 0 To Nlz - 1
      NormID(i) = Normfa(i).LichtID
      Gew(i) = Normfa(i).NormGew
      For k = 0 To 2
        Fakt(i, k) = Normfa(i).NormFakt(k)
        For j = 0 To NWE - 1
          E(i, k, j) = Normfa(i).Normkurven(k).R(j)
        Next j
      Next k
    Next i
  End Sub

  Sub GetMenueParam(ByRef Pr As String, ByRef Ier As Integer)
    Dim AbsID As Integer
    Dim BwID As Integer
    Dim FstID As Integer
    Dim WelFst As Single
    Dim Schwell As Single
    Dim WLCH(2) As Single
    Dim KdeWS As Single
    Dim VhWS As Single
    Dim ALP As Single
    Dim GMAX As Single
    Dim RWGEW As Single
    Dim IDaeArt As Integer
    Dim JABST As Integer
    Dim Parmerk(63) As Single
    Ier = 0
    JABST = MenueParam.Menue.JABST
    AbsID = MenueParam.Menue.AbsID
    KdeWS = MenueParam.Menue.KdeWS
    VhWS = MenueParam.Menue.VerWS
    WLCH(0) = MenueParam.Menue.Lgew
    WLCH(1) = MenueParam.Menue.Cgew
    WLCH(2) = MenueParam.Menue.Hgew
    Schwell = MenueParam.Menue.Schwell
    WelFst = MenueParam.Menue.FstaeWel
    BwID = MenueParam.Menue.BwertID
    FstID = MenueParam.Menue.FstaeID
    ALP = MenueParam.Menue.SPR
    GMAX = MenueParam.Menue.GMAX
    RWGEW = 10.0
    'ALP = 0.0
    IDaeArt = MenueParam.Menue.IDaeArt
    For i = 0 To MenueParam.ParaMerks.ParamCount - 1
      Parmerk(MenueParam.ParaMerks(i).ID - 1) = Singl(MenueParam.ParaMerks(KeyRe(MenueParam.ParaMerks(i).ID)).Wert)
    Next
    Select Case Pr
      Case "FAR"
        Call FARMEN(IDaeArt, ALP, GMAX, RWGEW, JABST, AbsID, BwID, FstID, WelFst, Schwell, WLCH(0), KdeWS, VhWS, Parmerk(0), FEHL)
      Case "QUA"
        Call QUAMEN(IDaeArt, ALP, GMAX, RWGEW, JABST, AbsID, BwID, FstID, WelFst, Schwell, WLCH(0), KdeWS, VhWS, Parmerk(0), FEHL)
      Case "SPZ"
        Call QUAMEN(IDaeArt, ALP, GMAX, RWGEW, JABST, AbsID, BwID, FstID, WelFst, Schwell, WLCH(0), KdeWS, VhWS, Parmerk(0), FEHL)
      Case "MIS"
        Call MISMEN(IDaeArt, ALP, GMAX, RWGEW, JABST, AbsID, BwID, FstID, WelFst, Schwell, WLCH(0), KdeWS, VhWS, Parmerk(0), FEHL)
      Case "FUM"
        Call FUMMEN(IDaeArt, ALP, GMAX, RWGEW, JABST, AbsID, BwID, FstID, WelFst, Schwell, WLCH(0), KdeWS, VhWS, Parmerk(0), FEHL)
      Case "FWR"
        Call FWRMEN(IDaeArt, ALP, GMAX, RWGEW, JABST, AbsID, BwID, FstID, WelFst, Schwell, WLCH(0), KdeWS, VhWS, Parmerk(0), FEHL)
    End Select
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If

  End Sub

  '
  '
  '
  '
  '
  '
  '
  '
  '

  '
  Sub StartUnt(ByRef messg As MeasParameters, ByRef Winkel As AngGeos, ByRef TyUnt() As TyRwert, ByRef QuUnt() As TyQual, ByRef Runt(,,) As Single)
    Dim i As Integer
    Dim j As Integer
    Dim Rstd As Single
    Dim R0 As Single
    Dim R1 As Single
    Dim R2 As Single
    Dim kw As Integer
    '
    '
    '
    '
    '
    '
    'Anfangswerte für untergründe
    '
    '
    '
 
    For i = 0 To 2
      TyUnt(i).RwertID = -1
      TyUnt(i).KWB = 1
      If i = 1 Then
        TyUnt(i).KWB = 2
      End If
      Call QuCartBy(QuUnt(i), NstrKen(i))
      For j = 0 To 4
        Select Case j
          Case 0
            QuUnt(i).Camp00 = 0.0#
          Case 1
            QuUnt(i).Camp01 = 0.0#
          Case 2
            QuUnt(i).Camp02 = 0.0#
          Case 3
            QuUnt(i).Camp03 = 0.0#
          Case 4
            QuUnt(i).Camp04 = 0.0#

        End Select
      Next j
      Select Case i
        Case 0
          Rstd = 1.0#
        Case 1
          Rstd = 0.0#
        Case 2
          Rstd = 0.5
      End Select
      For kw = 0 To Winkel.Km - 1
        R1 = messg.Winkel(kw).GK(0)
        R2 = messg.Winkel(kw).GK(2)
        R0 = Winkel(kw).Iglz * messg.Winkel(kw).GK(0)
        For j = 0 To Winkel.Wsol.Nwe - 1
          Runt(i, kw, j) = messg.Winkel(kw).GK(9) * (R0 + (1.0# - R1) * (1.0# - R2) * Rstd / (1.0# - R2 * Rstd))
        Next j
      Next kw
    Next i
  End Sub
  '
  '
  Sub GetWinkNormGk(ByRef Pr As String, ByRef KWW As Integer, ByRef Winkel As AngGeos, ByRef Ier As Integer)
    '
    '
    '
    '
    '
    '
    '
    'Menüparameter für Winkel,Normlichtarten und GK-Werte
    '
    '
    '
    '
    '
    '
    Dim i As Integer
    Dim j As Integer
    Dim Aka As Single
    Dim NWE As Integer
    Dim KM As Integer
    Dim kw As Integer
    Dim CDE(1) As Byte
    Dim Fopt As Single
    Dim BeID As Integer
    Dim Nlz As Integer
    Dim NGK As Integer


    'Tynorm
    '
    'Normspektralwerte
    '
    'E(Anz.Lichtarten-1,2,nwe-1)
    '
    Dim E(,,) As Single
    '
    'Faktoren
    '
    Dim Fakt(,) As Single
    '
    '
    '
    '
    Dim Wsol() As Single
    '
    '
    '
    Dim IhrmWin() As Integer
    Dim GewIhrm() As Single
    Dim GewNorm() As Single
    Dim NormID() As Integer
    Dim GK(,) As Single
    '
    '
    '
    Ier = 0
    '
    '
    NGK = 16
    ReDim GK(Winkel.Km - 1, NGK - 1)
    Aka = 1.0 - MenueParam.Messg.Ka
    Fopt = MenueParam.Menue.Fopt
    CDE(0) = Asc(MenueParam.Messg.CDE.Substring(0, 1))
    CDE(1) = Asc(MenueParam.Messg.CDE.Substring(1, 1))
    'CDE = getBytes(Menueparam.messg.CDE)

    For kw = 0 To Winkel.Km - 1
      For i = 0 To NGK - 1
        GK(kw, i) = Winkel(kw).GK(i)
      Next i
    Next kw
    '
    '
    '
    Call GetNormFarben(MenueParam.Normfa, Nlz, NormID, GewNorm, Fakt, NWE, E, Ier)


    BeID = MenueParam.Messg.BereichID
    ReDim Wsol(NWE - 1)
    For j = 0 To NWE - 1
      Wsol(j) = Winkel.Wsol.R(j)
    Next j
    KM = Winkel.Km
    ReDim IhrmWin(KM - 1)
    ReDim GewIhrm(KM - 1)
    For i = 0 To KM - 1
      IhrmWin(i) = ihrum(Winkel(i).Chrm)
      GewIhrm(i) = Winkel(i).IhrmGew
    Next

    Select Case Pr
      Case "RZP"
        '
        '
        'Erstrezepte
        'Rezeptkorrektur
        'Rwerte aus Rezepten
        'Rwerte aus Rezeptkorrektur
        'BASREZ
        'REZREZ
        'MSHREZ
        'REZKOR
        'MSHKOR
        '
        '
        kw = MenueParam.Menue.Kwj
        Call GETFDMRZP(kw, Aka, Fopt, CDE(0), _
             Nlz, BeID, NormID(0), GewNorm(0), Fakt(0, 0), NWE, KM, Wsol(0), E(0, 0, 0), _
             IhrmWin(0), GewIhrm(0), NGK, GK(0, 0), FEHL)
      Case "FWR"
        '
        'FarbWerte Rezeptberechnung
        'FAWRTREZ
        '
        kw = KWW
        Call GETFDMFWR(kw, Aka, Fopt, CDE(0), _
             Nlz, BeID, NormID(0), GewNorm(0), Fakt(0, 0), NWE, KM, Wsol(0), E(0, 0, 0), _
             IhrmWin(0), GewIhrm(0), NGK, GK(0, 0), FEHL)
      Case "QUA"
        '
        '
        'Qualitätskontrolle
        'FASDEK
        'FASTRA
        'FASTEX
        'FSTDEK
        'FSTTRA
        'DEKDEK
        'DEKTRA
        'UNENDL
        '
        '
        '
        kw = KWW
        Call GETFDMQUA(kw, Aka, Fopt, CDE(0), _
             Nlz, BeID, NormID(0), GewNorm(0), Fakt(0, 0), NWE, KM, Wsol(0), E(0, 0, 0), _
             IhrmWin(0), GewIhrm(0), NGK, GK(0, 0), FEHL)

      Case "FAR"
        '
        'FarbWerte (Farkor) Qualitätskonbtrolle
        kw = KWW
        Call GETFDMFAR(kw, Aka, Fopt, CDE(0), _
             Nlz, BeID, NormID(0), GewNorm(0), Fakt(0, 0), NWE, KM, Wsol(0), E(0, 0, 0), _
             IhrmWin(0), GewIhrm(0), NGK, GK(0, 0), FEHL)
        '
        '
        'DIN 6164
        '
        '
        '
        If MenueParam.NormFaD6164.Nlz <> 0 Then
          Call GetNormFarben(MenueParam.NormFaD6164, Nlz, NormID, GewNorm, Fakt, NWE, E, Ier)
          ReDim Wsol(NWE)
          Wsol(0) = 380.0#
          For j = 1 To NWE - 1
            Wsol(j) = Wsol(j - 1) + 10
          Next j


          Call GET6164(Nlz, NormID(0), Fakt(0, 0), NWE, Wsol(0), E(0, 0, 0), FEHL)
        End If

        '
      Case "MIS"
        '
        'Mischen
        '
        '
        Call GETFDMMIS(kw, Aka, Fopt, CDE(0), _
             Nlz, BeID, NormID(0), GewNorm(0), Fakt(0, 0), NWE, KM, Wsol(0), E(0, 0, 0), _
             IhrmWin(0), GewIhrm(0), NGK, GK(0, 0), FEHL)
      Case "FUM"
        '
        'Farbumrechnung (FARBUMRE)
        '
        Call GETFDMFUM(kw, Aka, Fopt, CDE(0), _
             Nlz, BeID, NormID(0), GewNorm(0), Fakt(0, 0), NWE, KM, Wsol(0), E(0, 0, 0), _
             IhrmWin(0), GewIhrm(0), NGK, GK(0, 0), FEHL)
        '
      Case "SPZ"
        '
        '
        'Spezialprogramme
        '
        '
        kw = KWW
        Call GETFDMQUA(kw, Aka, Fopt, CDE(0), _
             Nlz, BeID, NormID(0), GewNorm(0), Fakt(0, 0), NWE, KM, Wsol(0), E(0, 0, 0), _
             IhrmWin(0), GewIhrm(0), NGK, GK(0, 0), FEHL)

      Case "GRD"
        '
        'Grunddaten
        '
        kw = KWW
        Call GETFDMGRD(kw, Aka, Fopt, CDE(0), _
             Nlz, BeID, NormID(0), GewNorm(0), Fakt(0, 0), NWE, KM, Wsol(0), E(0, 0, 0), _
             IhrmWin(0), GewIhrm(0), NGK, GK(0, 0), FEHL)
      Case "RAU"
        '
        'Raumrechnung
        '
        kw = KWW
        Call GETFDMRAU(kw, Aka, Fopt, CDE(0), _
             Nlz, BeID, NormID(0), GewNorm(0), Fakt(0, 0), NWE, KM, Wsol(0), E(0, 0, 0), _
             IhrmWin(0), GewIhrm(0), NGK, GK(0, 0), FEHL)
      Case "WAG"
        '
        'Waagensteuerung
        '
        kw = KWW
        Call GETFDMWAG(kw, Aka, Fopt, CDE(0), _
             Nlz, BeID, NormID(0), GewNorm(0), Fakt(0, 0), NWE, KM, Wsol(0), E(0, 0, 0), _
             IhrmWin(0), GewIhrm(0), NGK, GK(0, 0), FEHL)
      Case "MHR"
        '
        'Mehrschichtenberechnung
        '
        kw = MenueParam.Menue.Kwj
        Call GETFDMMHR(kw, Aka, Fopt, CDE(0), _
             Nlz, BeID, NormID(0), GewNorm(0), Fakt(0, 0), NWE, KM, Wsol(0), E(0, 0, 0), _
             IhrmWin(0), GewIhrm(0), NGK, GK(0, 0), FEHL)
    End Select
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If


    ''
  End Sub
  Sub RezEnde(ByRef Pr As String, ByRef Ier As Integer)
    '
    '
    '
    '
    '
    '
    '
    'Fortranprogramme beenden
    '
    '
    '
    '
    '
    '


    Select Case Pr
      Case "RZP"
        Call REZEND(FEHL)
      Case "KOR"
        Call REZEND(FEHL)
      Case "RAU"
        Call REZENDRAU(FEHL)
    End Select
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If


    '
  End Sub
  Sub RezBeginn(ByRef Pr As String, ByRef Winkel As AngGeos, ByRef NFG As Integer, ByRef NHLF As Integer, ByRef RzNR As String, ByRef Rezsozpt As RecipesGrp, ByRef Ier As Integer)
    Dim i As Integer
    Dim NPQH As Integer
    Dim KML As Integer
    Dim NWEL As Integer
    Dim NFV As Integer
    Dim NLQ As Integer
    Dim MEE As Integer
    Dim MEM As Integer
    Dim NSPE As Integer
    Dim KyId As String
    NME = 2
    KML = Winkel.Km
    NWEL = Winkel.Wsol.Nwe
    'Falls OptDaten vorhanden, genauere Berechnung mit NST(k) möglich
    NPQ1 = 0
    NPQH = 0
    If Not (NFG = 0 OrElse IsNothing(RzNR) OrElse IsNothing(Rezsozpt)) Then
      For i = 0 To Rezsozpt.Rezepte(RzNR).KF - 1
        KyId = KeyName(Rezsozpt.Rezepte(RzNR)(i).ID)
        If IsNothing(Rezsozpt.Farben(KyId).OptData) Then
          Ier = 3993
          MsgBox(Texxt(Ier) & Space(1) & Rezsozpt.Farben(KyId).Name)
          Exit Sub
        End If
        NPQH = NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, Rezsozpt.Farben(KyId).OptData.Nst)
        NPQ1 = Max(NPQ1, NPQH)
      Next
    End If
    NLQ = MenueParam.Normfa.Nlz
    NSPE = 0
    MEM = NFG
    MEE = 2 * NFG + 5
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    'Fortranprogramme für Rezeptberechnung beginnen
    '
    '
    '
    '
    '
    '
    Call REZEND(FEHL)
    Select Case Pr
      Case "RZP"
        NFV = MenueParam.Misch.MaxFarb
        If NFV > NFG Then
          NFV = NFG
        End If
        '
        's. auch RZP in GetRezeptMenu
        '
        NSPE = 2 * MenueParam.Misch.Rzp
        Call REZBEG(NWEL, KML, NME, NFG, NFV, NPQ1, NLQ, NHLF, MEE, MEM, NSPE, FEHL)
      Case "KOR"
        NFV = NFG
        NSPE = 0
        Call REZBEG(NWEL, KML, NME, NFG, NFV, NPQ1, NLQ, NHLF, MEE, MEM, NSPE, FEHL)
      Case "RAU"
        NFV = NFG
        NSPE = 0
        Call REZBEGRAU(NWEL, KML, NME, NFG, NFV, NPQ1, NLQ, NHLF, MEE, MEM, NSPE, FEHL)
    End Select
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    '
    '
    '
    '
  End Sub
  '
  '
  '
  '
  '
  '
  '
  Sub GetRezeptMenu(ByRef Pr As String, ByRef Ier As Integer)
    '
    '
    '
    '
    'Menüparameter für Rezept-, Rezeptkorrektur, Rezeptraum u.ä.m.
    '
    '
    '
    '
    Dim i As Integer
    Dim FU(1) As Single
    Dim WLCH(2) As Single
    Dim DEHLF As Single
    Dim DEGUT As Single
    Dim DEOK As Single
    Dim FDE As Single
    Dim GDE As Single
    Dim GGE As Single
    Dim FTO As Single
    Dim ATO As Single
    Dim DEL As Single
    Dim SPR As Single
    Dim GXG As Single
    Dim GXD As Single
    Dim DTO As Single
    Dim GTO As Single
    Dim AKA As Single
    Dim NPX As Integer
    Dim EPMNG As Single
    Dim MINFarb As Integer
    Dim MAXFarb As Integer
    Dim KWD As Integer
    Dim ITM As Integer
    Dim IGX As Integer
    Dim KWE As Integer
    Dim KGX As Integer
    Dim ICHI As Integer
    Dim ISOR As Integer
    Dim GKID As Integer
    Dim JABST As Integer
    Dim ParMerk(63) As Single
    '
    '
    '
    Ier = 0
    '
    '
    '
    '
    With MenueParam
      For i = 0 To 1
        FU(i) = .Menue.Fu(i)
      Next
      WLCH(0) = .Menue.Lgew
      WLCH(1) = .Menue.Cgew
      WLCH(2) = .Menue.Hgew

      DEHLF = .Misch.DeHlf
      DEGUT = .Misch.DeGut
      DEOK = .Misch.Deok
      FDE = .Misch.Fde
      GDE = .Misch.Gde
      GGE = .Misch.Gge
      KWD = .Misch.Kwd
      IGX = .Misch.Igx
      FTO = .Menue.Fto
      ATO = .Menue.Ato
      DEL = .Menue.Del
      SPR = .Menue.SPR
      GXG = .Misch.Gxg
      GXD = .Menue.Gxd
      DTO = .Misch.Dto
      GTO = .Misch.Gto
      NPX = .Misch.Npx
      JABST = .Menue.JABST
      EPMNG = 1.0 * .Misch.MinDos + SPR
      MINFarb = .Misch.MinFarb
      MAXFarb = .Misch.MaxFarb
      ITM = .Menue.Itm
      KWE = .Misch.Kwe
      KGX = 0
      If .Misch.Kgx Then
        KGX = 1
      End If
      ICHI = .Misch.Ichi
      ISOR = .Misch.Isor
      GKID = .Misch.GKwrtID
      For i = 0 To .ParaMerks.ParamCount - 1
        ParMerk(MenueParam.ParaMerks(i).ID - 1) = Singl(MenueParam.ParaMerks(KeyRe(MenueParam.ParaMerks(i).ID)).Wert)
      Next
    End With
    AKA = 1.0 - MenueParam.Messg.Ka
    Select Case Pr
      Case "RZP"
        If IGX > 2 Then
          IGX = 0
        End If
        ITM = Max(ITM, 50)
        Call REZMEN(JABST, FU(0), WLCH(0), DEHLF, DEGUT, DEOK, FDE, GDE, GGE, FTO, _
               ATO, DEL, SPR, GXG, GXD, DTO, GTO, _
               NPX, EPMNG, MINFarb, MAXFarb, KWD, ITM, AKA, IGX, KWE, KGX, ICHI, ISOR, GKID, ParMerk(0), FEHL)
      Case "KOR"
        ITM = Max(ITM, 50)
        Call REZMEN(JABST, FU(0), WLCH(0), DEHLF, DEGUT, DEOK, FDE, GDE, GGE, FTO, _
               ATO, DEL, SPR, GXG, GXD, DTO, GTO, _
               NPX, EPMNG, MINFarb, MAXFarb, KWD, ITM, AKA, IGX, KWE, KGX, ICHI, ISOR, GKID, ParMerk(0), FEHL)
      Case "RAU"
        ITM = Max(ITM, 50)
        Call REZMENRAU(JABST, FU(0), WLCH(0), DEHLF, DEGUT, DEOK, FDE, GDE, GGE, FTO, _
       ATO, DEL, SPR, GXG, GXD, DTO, GTO, _
       NPX, EPMNG, MINFarb, MAXFarb, KWD, ITM, AKA, IGX, KWE, KGX, ICHI, ISOR, GKID, ParMerk(0), FEHL)
      Case Else




    End Select
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If

  End Sub
  '
  '
  '
  '
  '
  '
  '
  '

  '
  '
  '
  '
  '
  '

  Sub LetMengen(ByRef KeyGet As String, ByRef KeyLet As String, ByRef Rezsozpt As RecipesGrp, ByRef NR As Integer, ByRef Name As String, ByRef Bem As String, ByRef NF As Integer, ByRef LK() As Integer, ByRef Famng() As Single, ByRef Dicke() As Single, ByRef Ier As Integer)
    '
    'IFaBa=0 reine Mengen
    'IFaBa=1 Batchmengen, Pastenmengen
    '
    '
    Dim i As Integer
    With Rezsozpt
      If KeyGet <> KeyLet Then
        .Rezepte(KeyLet).clear()
      End If
      .Rezepte(KeyLet).ID = .Rezepte(KeyGet).ID
      .Rezepte(KeyLet).Iarch = .Rezepte(KeyGet).Iarch
      .Rezepte(KeyLet).Gid = .Rezepte(KeyGet).Gid
      .Rezepte(KeyLet).GlzGrd = .Rezepte(KeyGet).GlzGrd
      .Rezepte(KeyLet).Nr = NR
      .Rezepte(KeyLet).Name = Name
      .Rezepte(KeyLet).Bem = Bem
      .Rezepte(KeyLet).DatTim = Now
      .Rezepte(KeyLet).UmMng = .Rezepte(KeyGet).UmMng
      .Rezepte(KeyLet).Dicke(0) = Dicke(0)
      .Rezepte(KeyLet).Dicke(1) = Dicke(1)
      For i = 0 To NF - 1
        If KeyLet <> KeyGet Then
          .Rezepte(KeyLet).AddFaNr(KeyRe(i), New ColorAmount)
        End If
        .Rezepte(KeyLet)(i).ID = .Rezepte(KeyGet)(LK(i) - 1).ID
        .Rezepte(KeyLet)(i).Proz = .Rezepte(KeyGet)(LK(i) - 1).Proz
        .Rezepte(KeyLet)(i).Prob = .Rezepte(KeyGet)(LK(i) - 1).Prob
        .Rezepte(KeyLet)(i).FaAmng = Famng(i)

      Next i
    End With

  End Sub






  Sub GetBuBoMng(ByRef Farben As Colorants, ByRef Ummng As Single, ByRef KF As Integer, ByRef ID() As Integer, ByRef BuMng() As Single, ByRef BoMng() As Single, ByRef Bel() As Single, ByRef Ier As Integer)
    Dim KeyD As String
    Dim i As Integer
    Ier = 0
    For i = 0 To KF - 1
      KeyD = KeyName(ID(i))
      If Farben.ContainsFarb(KeyD) Then
        BuMng(i) = Farben(KeyD).BuMng 'Limitierungsmenge (unten)gemäß INM
        BoMng(i) = Farben(KeyD).BoMng 'Limitierungsmenge (oben) gemäß INM
        Bel(i) = Farben(KeyD).Bel
      Else
        Ier = 4049
        MsgBox(Texxt(4049) & ": " & ID(i))
        Exit Sub
      End If
    Next i
  End Sub
  Sub GetOpKto(ByRef Farben As Colorants, ByRef KF As Integer, ByRef ID() As Integer, ByRef OP() As Byte, ByRef KTO() As Byte, ByRef Ier As Integer)
    Dim KeyD As String
    Dim i As Integer
    Dim BytHlf(0) As Byte
    Ier = 0

    For i = 0 To KF - 1
      Try
        KeyD = KeyName(ID(i))
        OP(i) = Asc(Farben(KeyD).OP)
        KTO(i) = Asc(Farben(KeyD).Kto)
      Catch ex As Exception
        Ier = 4049
        MsgBox(Texxt(4049) & ": " & ID(i))
        Exit Sub
      End Try
    Next i
  End Sub

  Sub GetIchfEff(ByRef Farben As Colorants, ByRef KF As Integer, ByRef ID() As Integer, ByRef Ichf() As Integer, ByRef Eff() As Single, ByRef Ier As Integer)
    Dim KeyD As String
    Dim i As Integer
    Ier = 0
    For i = 0 To KF - 1
      Try
        KeyD = KeyName(ID(i))
        'Eff(i) = Farben(KeyD).Eff
        Eff(i) = 1.0
        Ichf(i) = Farben(KeyD).Ichf
      Catch ex As Exception
        Ier = 4049
        MsgBox(Texxt(4049) & ": " & ID(i))
        Exit Sub
      End Try
    Next i
  End Sub
  Sub GetPreisFst(ByRef Farben As Colorants, ByRef KF As Integer, ByRef ID() As Integer, ByRef Preis() As Single, ByRef Fst() As Single, ByRef Ier As Integer)
    Dim KeyD As String
    Dim i As Integer

    For i = 0 To KF - 1
      Try
        KeyD = KeyName(ID(i))
        Preis(i) = Farben(KeyD).Preis 'Preis
        Fst(i) = Farben(KeyD).Fst 'Farbstärke
      Catch ex As Exception
        Ier = 4049
        MsgBox(Texxt(4049) & ": " & ID(i))
        Exit Sub
      End Try
    Next i
  End Sub

  Sub GetProzProbSpz(ByRef RezNam As String, ByRef Rezpte As RecipesGrp, ByRef KF As Integer, ByRef Proz() As Single, ByRef Prob() As Single, ByRef Spz() As Single, ByRef Ier As Integer)
    Dim KeyD As String
    Dim i As Integer
    Ier = 0

    For i = 0 To KF - 1
      Try
        KeyD = KeyName(Rezpte.Rezepte(RezNam)(i).ID)
        Proz(i) = Rezpte.Rezepte(RezNam)(i).Proz 'Farbmittelprozentigkeit
        Prob(i) = Rezpte.Rezepte(RezNam)(i).Prob 'Bindemittelprozentigkeit
        Spz(i) = Rezpte.Farben(KeyD).Spz
      Catch ex As Exception
        Ier = 4049
        MsgBox(Texxt(4049) & ": " & Rezpte.Rezepte(RezNam)(i).ID)
        Exit Sub
      End Try
    Next i
  End Sub
  Sub GetBaMng(ByRef RezNam As String, ByRef Rezpte As RecipesGrp, ByRef KF As Integer, ByRef ID() As Integer, ByRef BaMng() As Single, ByRef Ier As Integer)
    Dim i As Integer
    Ier = 0
    For i = 0 To KF - 1
      Try
        BaMng(i) = Rezpte.Rezepte(RezNam)(i).BaAmng 'Farbmittelmenge gemäß Normierung INF
        ID(i) = Rezpte.Rezepte(RezNam)(i).ID
      Catch ex As Exception
        Ier = 4049
        MsgBox(Texxt(4049) & ": " & Rezpte.Rezepte(RezNam)(i).ID)
        Exit Sub
      End Try
    Next i
  End Sub


  Sub GetFamng(ByRef RezNam As String, ByRef Rezpte As RecipesGrp, ByRef KF As Integer, ByRef ID() As Integer, ByRef FaMng() As Single, ByRef Ier As Integer)
    Dim i As Integer
    For i = 0 To KF - 1
      Try
        FaMng(i) = Rezpte.Rezepte(RezNam)(i).FaAmng 'reine Farbmittelmenge
        ID(i) = Rezpte.Rezepte(RezNam)(i).ID
      Catch ex As Exception
        Ier = 4049
        MsgBox(Texxt(4049) & ": " & Rezpte.Rezepte(RezNam)(i).ID)
        Exit Sub
      End Try
    Next i
  End Sub


  Sub LetQurwert(ByRef Rwert As RefValue, ByRef Qurwert As TyQual)
    Dim i As Integer
    Rwert.Iami = Qurwert.iami
    For i = 0 To Rwert.RefKurv.Count - 1
      Select Case i
        Case 0
          Rwert.De(i) = Qurwert.DE00
        Case 1
          Rwert.De(i) = Qurwert.DE01
        Case 2
          Rwert.De(i) = Qurwert.DE02
        Case 3
          Rwert.De(i) = Qurwert.DE03
        Case 4
          Rwert.De(i) = Qurwert.DE04
        Case 5
          Rwert.De(i) = Qurwert.DE05
        Case 6
          Rwert.De(i) = Qurwert.DE06
        Case 7
          Rwert.De(i) = Qurwert.DE07
        Case 8
          Rwert.De(i) = Qurwert.DE08
        Case 9
          Rwert.De(i) = Qurwert.DE09
        Case 10
          Rwert.De(i) = Qurwert.DE10
        Case 11
          Rwert.De(i) = Qurwert.DE11
        Case 12
          Rwert.De(i) = Qurwert.DE12
        Case 13
          Rwert.De(i) = Qurwert.DE13
        Case 14
          Rwert.De(i) = Qurwert.DE14
        Case 15
          Rwert.De(i) = Qurwert.DE15
      End Select
    Next i
    For i = 0 To 4
      Select Case i
        Case 0
          Rwert.QuControl.Camp(0) = Qurwert.Camp00
        Case 1
          Rwert.QuControl.Camp(1) = Qurwert.Camp01
        Case 2
          Rwert.QuControl.Camp(2) = Qurwert.Camp02
        Case 3
          Rwert.QuControl.Camp(3) = Qurwert.Camp03
        Case 4
          Rwert.QuControl.Camp(4) = Qurwert.Camp04
        Case 5
          Rwert.QuControl.Camp(5) = Qurwert.Camp05
        Case 6
          Rwert.QuControl.Camp(6) = Qurwert.Camp06
        Case 7
          Rwert.QuControl.Camp(7) = Qurwert.Camp07
        Case 8
          Rwert.QuControl.Camp(8) = Qurwert.Camp04
        Case 9
          Rwert.QuControl.Camp(9) = Qurwert.Camp05
        Case 10
          Rwert.QuControl.Camp(10) = Qurwert.Camp06
        Case 11
          Rwert.QuControl.Camp(11) = Qurwert.Camp07
        Case 12
          Rwert.QuControl.Camp(12) = Qurwert.Camp04
        Case 13
          Rwert.QuControl.Camp(13) = Qurwert.Camp05
        Case 14
          Rwert.QuControl.Camp(14) = Qurwert.Camp06
        Case 15
          Rwert.QuControl.Camp(15) = Qurwert.Camp07

      End Select
    Next i
    Rwert.QuControl.Cart = QuCartString(Qurwert)
    Rwert.QuControl.IuntID = Qurwert.IuntID
    Rwert.QuControl.MethID = Qurwert.MethID
  End Sub
  Sub GetQurwert(ByRef Rwert As RefValue, ByRef Qurwert As TyQual, ByRef RwertName As String, ByRef RwertBem As String)
    Dim i As Integer

    Qurwert.iami = Rwert.Iami
    If Rwert.Iami > 0 Then
      For i = 0 To Rwert.RefKurv.Count - 1
        Select Case i
          Case 0
            Qurwert.DE00 = Rwert.De(i)
          Case 1
            Qurwert.DE01 = Rwert.De(i)
          Case 2
            Qurwert.DE02 = Rwert.De(i)
          Case 3
            Qurwert.DE03 = Rwert.De(i)
          Case 4
            Qurwert.DE04 = Rwert.De(i)
          Case 5
            Qurwert.DE05 = Rwert.De(i)
          Case 6
            Qurwert.DE06 = Rwert.De(i)
          Case 7
            Qurwert.DE07 = Rwert.De(i)
          Case 8
            Qurwert.DE08 = Rwert.De(i)
          Case 9
            Qurwert.DE09 = Rwert.De(i)
          Case 10
            Qurwert.DE10 = Rwert.De(i)
          Case 11
            Qurwert.DE11 = Rwert.De(i)
          Case 12
            Qurwert.DE12 = Rwert.De(i)
          Case 13
            Qurwert.DE13 = Rwert.De(i)
          Case 14
            Qurwert.DE14 = Rwert.De(i)
          Case 15
            Qurwert.DE15 = Rwert.De(i)
        End Select
      Next i
    End If
    If Not IsNothing(Rwert.QuControl) Then
      For i = 0 To 7
        Select Case i
          Case 0
            Qurwert.Camp00 = Rwert.QuControl.Camp(i)
          Case 1
            Qurwert.Camp01 = Rwert.QuControl.Camp(i)
          Case 2
            Qurwert.Camp02 = Rwert.QuControl.Camp(i)
          Case 3
            Qurwert.Camp03 = Rwert.QuControl.Camp(i)
          Case 4
            Qurwert.Camp04 = Rwert.QuControl.Camp(i)
          Case 5
            Qurwert.Camp05 = Rwert.QuControl.Camp(i)
          Case 6
            Qurwert.Camp06 = Rwert.QuControl.Camp(i)
          Case 7
            Qurwert.Camp07 = Rwert.QuControl.Camp(i)
          Case 8
            Qurwert.Camp08 = Rwert.QuControl.Camp(i)
          Case 9
            Qurwert.Camp09 = Rwert.QuControl.Camp(i)
          Case 10
            Qurwert.Camp10 = Rwert.QuControl.Camp(i)
          Case 11
            Qurwert.Camp11 = Rwert.QuControl.Camp(i)
          Case 12
            Qurwert.Camp12 = Rwert.QuControl.Camp(i)
          Case 13
            Qurwert.Camp13 = Rwert.QuControl.Camp(i)
          Case 14
            Qurwert.Camp14 = Rwert.QuControl.Camp(i)
          Case 15
            Qurwert.Camp15 = Rwert.QuControl.Camp(i)

        End Select
      Next i
      Call QuCartBy(Qurwert, Rwert.QuControl.Cart)
      'Qurwert.Cart = GetInteger(GetBytes(Rwert.QuControl.Cart))
      Qurwert.IuntID = Rwert.QuControl.IuntID
      Qurwert.MethID = Rwert.QuControl.MethID
      RwertName = Rwert.Name
      RwertBem = Rwert.Bem
    End If
  End Sub


  Sub GetRwerte(ByRef Winkel As AngGeos, ByRef iu As Integer, ByRef Rmit(,,) As Single, ByRef Rwert As RefValue)
    Dim i As Integer
    Dim kw As Integer
    Dim CHRM As String
    Dim KM As Integer
    Dim NWE As Integer
    KM = Winkel.Km
    NWE = Winkel.Wsol.Nwe
    If Rwert.RefKurv.Count = 0 Then Exit Sub
    For kw = 0 To KM - 1
      CHRM = Winkel(kw).Chrm
      For i = 0 To NWE - 1
        Rmit(iu, kw, i) = Rwert.RefKurv(CHRM).R(i)
      Next i
    Next kw
  End Sub
  Sub LetRwerte(ByRef Winkel As AngGeos, ByRef iu As Integer, ByRef Rmit(,,) As Single, ByRef Rwert As RefValue)
    Dim i As Integer
    Dim kw As Integer
    Dim KM As Integer
    Dim NWE As Integer
    Dim CHRM As String
    KM = Winkel.Km
    NWE = Winkel.Wsol.Nwe
    For kw = 0 To Rwert.RefKurv.Count - 1
      For i = 0 To NWE - 1
        Rwert.RefKurv(kw).R(i) = 999
      Next i
    Next
    For kw = 0 To KM - 1
      CHRM = Winkel(kw).Chrm
      For i = 0 To NWE - 1
        Rwert.RefKurv(CHRM).R(i) = Rmit(iu, kw, i)
      Next i
    Next kw
  End Sub






  Function CHMeth(ByRef Pmeth As String, ByRef Ameth As String) As Boolean
    Dim ii As Integer
    CHMeth = True
    'If  Pmeth.substring(3,   1) <> "W" Or  Ameth.substring(3,1)  <> "W" Then
    '   CHMeth = False
    '   Exit Function
    'End If
    If Len(Pmeth) < 6 Or Len(Ameth) < 6 Then
      CHMeth = False
      Exit Function
    End If
    For ii = 0 To 5
      If Ameth.Substring(ii, 1) <> "$" Then
        If Ameth.Substring(ii, 1) <> Pmeth.Substring(ii, 1) Then
          CHMeth = False
          Exit Function
        End If
      End If
    Next ii
  End Function
  Sub LetCalcRwert(ByRef Winkel As AngGeos, ByRef RwertSource As RefValue, ByRef RwertTarget As RefValue, _
  ByRef NR As Integer, ByRef Itp As Boolean, ByRef IVONA As Boolean, ByRef CME As String, _
  ByRef Name As String, ByRef Bem As String, ByRef iu As Integer, ByRef RCal(,,) As Single, ByRef Ier As Integer)
    Dim i As Integer
    Dim kw As Integer
    Dim CHRM As String
    '
    'Umspeichern
    '
    RwertTarget.ID = RwertSource.ID
    RwertTarget.Gid = RwertSource.Gid
    RwertTarget.Itp = Itp
    RwertTarget.Nr = NR
    RwertTarget.Iarch = RwertSource.Iarch
    RwertTarget.IVoNa = IVONA
    RwertTarget.kwb = RwertSource.kwb
    RwertTarget.ReTr = RwertSource.ReTr
    RwertTarget.Iplott = RwertSource.Iplott
    RwertTarget.Name = Name
    RwertTarget.Bem = Bem
    RwertTarget.Cme = CME
    RwertTarget.Banum = RwertSource.Banum
    RwertTarget.DatTim = Now
    RwertTarget.Iplott = RwertSource.Iplott

    '
    '
    '
    'R-Werte für berechnete Kurven löschen
    '
    '
    '
    '
    '
    '
    'R-Werte für berechnete Kurven löschen
    '
    '
    '
    Call ADDCurves(RwertTarget.RefKurv)
    For kw = 0 To RwertSource.RefKurv.Count - 1
      For i = 0 To Winkel.Wsol.Nwe - 1
        RwertTarget.RefKurv(kw).R(i) = 999.0
      Next i
    Next kw
    For kw = 0 To Winkel.Km - 1
      CHRM = Winkel(kw).Chrm
      For i = 0 To Winkel.Wsol.Nwe - 1
        RwertTarget.RefKurv(CHRM).R(i) = RCal(iu, kw, i)
      Next i
    Next kw
  End Sub
  Function FehlDLL(ByRef FEHL As TYFEH, ByRef WarnLog As Integer) As Boolean
    Dim res As DialogResult
    Dim LogF As Boolean
    LogF = True
    If WarnLog > 1 Then
      LogF = False
    End If
    FehlDLL = False
    If FEHL.Ifeh = 4000 Then
      res = MessageOut(LogF, 4000, Texxt(4000) & Space(1) & CStr(FEHL.Kenn), MessageBoxButtons.OK, Texxt(2004), LogFileName)
      FehlDLL = True
      Exit Function
    ElseIf FEHL.Ifeh > 0 Then
      res = MessageOut(LogF, FEHL.Ifeh, Texxt(FEHL.Ifeh), MessageBoxButtons.OK, Texxt(2004), LogFileName)
      FehlDLL = True
      Exit Function
    End If
    If FEHL.Iwarn > 0 And (WarnLog = 1 Or WarnLog = 3) Then
      res = MessageOut(LogF, Abs(FEHL.Iwarn), Texxt(FEHL.Iwarn) & Chr(13) & Texxt(1999), MessageBoxButtons.YesNo, Texxt(2005), LogFileName)
      If res = DialogResult.No Then
        FehlDLL = True
        Exit Function
      End If
    End If
    FEHL.Ifeh = 0
    FEHL.Iwarn = 0
  End Function



  Function NstrCME(ByRef i As Integer) As String
    Static chki As Boolean
    '
    '
    's. auch Texxt(1200 ff)
    '
    '
    '
    Static teex(0 To 5) As String
    If Not chki Then
      teex(0) = "RE"
      teex(1) = "UW"
      teex(2) = "US"
      teex(3) = "WW"
      teex(4) = "SS"
      teex(5) = "GG"
      chki = True
    End If
    NstrCME = teex(i)
  End Function





  Function blonr(ByRef k As Integer, ByRef kw As Integer, ByRef Znr As Integer) As Integer
    blonr = 4096 * k + 256 * kw + Znr
  End Function


  Sub GetGrundDat(ByRef Ifarb As Integer, ByRef Winkel As AngGeos, ByRef NPS As Integer, ByRef Grund(,,,) As Single, _
  ByRef OptKonst As OpticalData)
    Dim i As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim KM As Integer
    Dim NWE As Integer
    Dim CHRM As String
    KM = Winkel.Km
    NWE = Winkel.Wsol.Nwe
    For k = 0 To NPS - 1
      For kw = 0 To KM - 1
        CHRM = Winkel(kw).Chrm
        For i = 0 To NWE - 1
          If OptKonst.Grund.Count = 0 Then
            Grund(Ifarb, k, kw, i) = 0.0
          Else
            Grund(Ifarb, k, kw, i) = OptKonst.Grund(k)(CHRM).R(i)
          End If
        Next i
      Next kw
    Next k
  End Sub
  '

  Sub LetGrundDat(ByRef Ifarb As Integer, ByRef Winkel As AngGeos, ByRef NPS As Integer, ByRef Grund(,,,) As Single, ByRef Optdaten As OpticalData)
    Dim i As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim KM As Integer
    Dim NWE As Integer
    Dim CHRM As String
    KM = Winkel.Km
    NWE = Winkel.Wsol.Nwe
    For k = 0 To NPS - 1
      For kw = 0 To KM - 1
        CHRM = Winkel(kw).Chrm
        For i = 0 To NWE - 1
          Optdaten.Grund(k)(CHRM).R(i) = Grund(Ifarb, k, kw, i)
        Next i
      Next kw
    Next k
  End Sub




  Sub GetGrundDaten(ByRef Pr As String, ByRef Winkel As AngGeos, ByRef Rezsozpt As RecipesGrp, ByRef MngRei As Single, ByRef MNF As Integer, ByRef ID() As Integer, ByRef Ier As Integer)
    Dim k As Integer
    Dim i As Integer
    Dim Iee As Integer
    Dim KyID As String
    Dim KM As Integer
    Dim NWE As Integer
    Dim NPS As Integer
    Dim Preis() As Single
    Dim Fst() As Single
    Dim Eff() As Single
    Dim Ichf() As Integer
    Dim OP() As Byte
    Dim KTO() As Byte
    '
    '
    '
    '
    '
    '
    '
    'Farbmittel und Grunddaten übernehmen
    '
    '
    '
    Dim Grund(,,,) As Single
    '
    Dim Cst(,,) As Single
    Dim NST() As Integer
    KM = Winkel.Km
    NWE = Winkel.Wsol.Nwe
    'Falls OptDaten vorhanden, genauere Berechnung mit NST(k) möglich

    '
    '
    '
    '
    'Grunddaten
    'GRUND(Anz. Farbmittel-1,NST,Anz. Winkel-1,Anz.Wellenlängen-1)
    '
    '


    ReDim Grund(MNF - 1, NPQ1 - 1, KM - 1, NWE - 1)
    '
    'Stützkonzentrationen für Grunddaten (s. auch TyGrund)
    '
    '
    'CST(Anzahl Stützstellen=NST-1,Anzahl Farbmittel-1,)
    '
    ReDim NST(MNF - 1)

    ReDim Cst(MNF - 1, NPQ1 - 1, KM - 1)
    '
    '
    '
    '
    ReDim Preis(MNF - 1)
    ReDim Fst(MNF - 1)
    ReDim Eff(MNF - 1)
    ReDim Ichf(MNF - 1)
    ReDim OP(MNF - 1)
    ReDim KTO(MNF - 1)
    ReDim Eff(MNF - 1)

    '
    '
    '
    'Umspeichern
    '
    '
    '
    For k = 0 To MNF - 1
      KyID = KeyName(ID(k))
      'Nst(k) = Resozpt.Farbm(KyId).OptData.Nst
      NST(k) = Rezsozpt.Farben(KyID).OptData.Nst
      For i = 0 To NST(k) - 1
        Cst(k, i, 0) = Rezsozpt.Farben(KyID).OptData.Cst(i + 1)
      Next i
      'Nps = NPNPS(Resozpt.Farben(KeyId).OptData.NPX, Nst(k))
      NPS = NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, NST(k))
      If NPS > NPQ1 Then
        Iee = 4011
        MsgBox(Texxt(Iee))
        Exit Sub
      End If
      If Not IsNothing(Rezsozpt.Farben(KyID).OptData) AndAlso Rezsozpt.Farben(KyID).OptData.Grund.Count > 0 Then
        Call GetGrundDat(k, Winkel, NPS, Grund, Rezsozpt.Farben(KyID).OptData)
      End If
    Next
    '
    Call GetIchfEff(Rezsozpt.Farben, MNF, ID, Ichf, Eff, Ier)
    Call GetOpKto(Rezsozpt.Farben, MNF, ID, OP, KTO, Ier)
    Call GetPreisFst(Rezsozpt.Farben, MNF, ID, Preis, Fst, Ier)
    Select Case Pr
      Case "RZP"
        Call REZGRU(MNF, MngRei, OP(0), KTO(0), Ichf(0), Preis(0), Fst(0), _
                    NWE, KM, NPQ1, NST(0), Cst(0, 0, 0), Grund(0, 0, 0, 0), FEHL)
      Case "KOR"
        Call REZGRU(MNF, MngRei, OP(0), KTO(0), Ichf(0), Preis(0), Fst(0), _
                    NWE, KM, NPQ1, NST(0), Cst(0, 0, 0), Grund(0, 0, 0, 0), FEHL)
      Case "RAU"
        Call REZGRURAU(MNF, MngRei, OP(0), KTO(0), Ichf(0), Preis(0), Fst(0), _
                    NWE, KM, NPQ1, NST(0), Cst(0, 0, 0), Grund(0, 0, 0, 0), FEHL)
    End Select
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    '
    '
    '
    '

    Erase Grund
    Erase NST
    Erase Cst
    Erase Preis
    Erase Fst
    Erase Eff
    Erase Ichf
    Erase OP
    Erase KTO
    Erase Eff

  End Sub

  Sub GetRezLimit(ByRef Pr As String, ByRef Ummng As Single, ByRef Rezpte As RecipesGrp, _
  ByRef MNF As Integer, ByRef ID() As Integer, ByRef Bamng() As Single, ByRef Famng() As Single, ByRef Proz() As Single, ByRef Prob() As Single, ByRef Spz() As Single, ByRef Ier As Integer)
    Dim Ivol As Integer
    Dim Inf As Integer
    Dim Ino As Integer
    Dim INP As Integer
    Dim INQ As Integer
    Dim INM As Integer
    Dim IGX As Integer
    Dim MngMin As Single
    Dim MngMax As Single
    Dim ProzMin As Single
    Dim ProzMax As Single
    Dim Sploe As Single
    Dim BuMng() As Single
    Dim BoMng() As Single
    Dim Bel() As Single
    Dim Ichf() As Integer
    Dim Eff() As Single
    Dim OP() As Byte
    Dim KTO() As Byte
    '
    '
    '
    '
    '
    '
    '
    '
    IGX = MenueParam.Misch.Igx
    Ivol = Rezpte.IVOL
    Inf = Rezpte.INF
    Ino = Rezpte.INO
    INP = Rezpte.INP
    INQ = Rezpte.INQ
    INM = Rezpte.INM
    MngMin = Rezpte.MngMin
    MngMax = Rezpte.MngMax
    ProzMin = Rezpte.ProzMin
    ProzMax = Rezpte.ProzMax
    Sploe = Rezpte.SpLoe
    '
    '
    '
    '
    '
    '
    '
    ReDim BuMng(MNF - 1)
    ReDim BoMng(MNF - 1)
    ReDim Bel(MNF - 1)
    ReDim Ichf(MNF - 1)
    ReDim OP(MNF - 1)
    ReDim KTO(MNF - 1)
    ReDim Eff(MNF - 1)
    '
    '
    Call GetIchfEff(Rezpte.Farben, MNF, ID, Ichf, Eff, Ier)
    If Ier <> 0 Then
      Exit Sub
    End If
    Call GetOpKto(Rezpte.Farben, MNF, ID, OP, KTO, Ier)
    If Ier <> 0 Then
      Exit Sub
    End If
    Call GetBuBoMng(Rezpte.Farben, Ummng, MNF, ID, BuMng, BoMng, Bel, Ier)
    If Ier <> 0 Then
      Exit Sub
    End If
    Select Case Pr
      Case "RZP"
        If IGX > 2 Then
          IGX = 0
        End If
        Call RCHLIM(MNF, Ivol, Inf, Ino, INP, INQ, INM, IGX, _
         MngMin, MngMax, ProzMin, ProzMax, Sploe, _
        Bamng(0), Famng(0), Proz(0), Prob(0), Eff(0), Spz(0), Ichf(0), OP(0), BuMng(0), BoMng(0), Bel(0), FEHL)
      Case "KOR"
        Call RCHLIM(MNF, Ivol, Inf, Ino, INP, INQ, INM, IGX, _
         MngMin, MngMax, ProzMin, ProzMax, Sploe, _
        Bamng(0), Famng(0), Proz(0), Prob(0), Eff(0), Spz(0), Ichf(0), OP(0), BuMng(0), BoMng(0), Bel(0), FEHL)
      Case "RAU"
        IGX = 0
        Call RCHLIMRAU(MNF, Ivol, Inf, Ino, INP, INQ, INM, IGX, _
         MngMin, MngMax, ProzMin, ProzMax, Sploe, _
        Bamng(0), Famng(0), Proz(0), Prob(0), Eff(0), Spz(0), Ichf(0), OP(0), BuMng(0), BoMng(0), Bel(0), FEHL)
    End Select
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If


    Erase BuMng
    Erase BoMng
    Erase Bel
    Erase Ichf
    Erase OP
    Erase KTO
    Erase Eff
  End Sub







  Sub GetSorkrit(ByRef Rezpt As Recipe, ByRef sorkrit(,) As Single)
    Dim i As Integer
    Rezpt.Sorkrit(0).clear()
    Rezpt.Sorkrit(1).clear()
    Rezpt.Sorkrit(0).Add(KeyRe(0), New Double)
    Rezpt.Sorkrit(0)(KeyRe(0)) = sorkrit(0, 4)
    Rezpt.Sorkrit(1).Add(KeyRe(0), New Double)
    Rezpt.Sorkrit(1)(KeyRe(0)) = sorkrit(1, 4)
    For i = 0 To 15
      Rezpt.Sorkrit(0).Add(KeyRe(i + 1), New Double)
      Rezpt.Sorkrit(0)(KeyRe(i + 1)) = sorkrit(0, i)
      Rezpt.Sorkrit(1).Add(KeyRe(i + 1), New Double)
      Rezpt.Sorkrit(1)(KeyRe(i + 1)) = sorkrit(1, i)
    Next i
  End Sub
  Function Nlaaf(ByRef J2 As Integer, ByRef AufgArt As String) As Integer
    Nlaaf = 1
    If J2 = 2 And AufgArt.Substring(3, 1) = "$" Then
      Nlaaf = 2
    End If
    If J2 = 2 And AufgArt.Substring(3, 1) = "W" And AufgArt.Substring(9, 1) = "S" Then
      Nlaaf = 2
    End If
    If J2 = 2 And AufgArt.Substring(2, 1) <> AufgArt.Substring(8, 1) Then
      Nlaaf = 2
    End If

  End Function
  Sub QuCartBy(ByRef Qucontrol As TyQual, ByVal Cart As String)
    Qucontrol.Cart0 = Asc(Cart.Substring(0, 1))
    Qucontrol.Cart1 = Asc(Cart.Substring(1, 1))
    Qucontrol.Cart2 = Asc(Cart.Substring(2, 1))
    Qucontrol.Cart3 = Asc(Cart.Substring(3, 1))
  End Sub
  Sub TyWertBy(ByRef TyWert As TyWert, ByVal Cmeth As String)
    TyWert.Cmeth0 = Asc(Cmeth.Substring(0, 1))
    TyWert.Cmeth1 = Asc(Cmeth.Substring(1, 1))
    TyWert.Cmeth2 = Asc(Cmeth.Substring(2, 1))
    TyWert.Cmeth3 = Asc(Cmeth.Substring(3, 1))
    TyWert.Cmeth4 = Asc(Cmeth.Substring(4, 1))
    TyWert.Cmeth5 = Asc(Cmeth.Substring(5, 1))
    TyWert.Cmeth6 = Asc(Cmeth.Substring(6, 1))
    TyWert.Cmeth7 = Asc(Cmeth.Substring(7, 1))
  End Sub
  Function QuCartString(ByRef Qucontrol As TyQual) As String
    QuCartString = Chr(Qucontrol.Cart0) & Chr(Qucontrol.Cart1) & Chr(Qucontrol.Cart2) & Chr(Qucontrol.Cart3)
  End Function
  Function TyWertString(ByRef TyWert As TyWert) As String
    TyWertString = Chr(TyWert.Cmeth0) & Chr(TyWert.Cmeth1) & Chr(TyWert.Cmeth2) & Chr(TyWert.Cmeth3) _
    & Chr(TyWert.Cmeth4) & Chr(TyWert.Cmeth5) & Chr(TyWert.Cmeth6) & Chr(TyWert.Cmeth7)
  End Function
  '
  '
  '


End Module




