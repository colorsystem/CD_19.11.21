Option Strict Off
Option Explicit On
Option Compare Text

Public Class QualSpezial
  Implements IDisposable
  Dim quali As QualKontrolle

  Sub WaagenMatrix(Alph As Single, ByRef Varianz As Single, RezAll As RecipesGrp, GrpRwerte As RefValuesGrp, ByRef H(,,) As Single, ByRef RUE(,,) As Single, ByRef ier As Integer)
    Dim GH As Single = 0.0
    Dim SUU As Double

    Dim i As Integer
    Dim j As Integer
    Dim l As Integer
    Dim kw As Integer
    Dim k As Integer
    Dim NF As Integer
    Dim P As Integer
    Dim NWE As Integer
    Dim KM As Integer
    Dim WV(,) As Single
    Dim WN(,) As Single
    Dim RV(,,) As Single
    Dim RN(,,) As Single

    NF = RezAll.Farben.FarbCount
    KM = MenueParam.Messg.Winkel.Km
    NWE = MenueParam.Messg.Winkel.Wsol.Nwe
    P = RezAll.Rezepte.RezCount
    ReDim H(NF - 1, KM - 1, NWE - 1)
    ReDim WV(P - 1, NF - 1)
    ReDim WN(P - 1, NF - 1)

    ReDim RV(P - 1, KM - 1, NWE - 1)
    ReDim RN(P - 1, KM - 1, NWE - 1)
    ReDim RUE(P - 1, KM - 1, NWE - 1)
    If P = 0 Then Exit Sub
    Call GetWinkNormGk("WAG", MenueParam.Menue.Kwj, MenueParam.Messg.Winkel, ier)
    '
    '
    '
    '
    '
    For k = 0 To P - 1
      For kw = 0 To KM - 1
        For i = 0 To NWE - 1
          RV(k, kw, i) = 1.0
          RN(k, kw, i) = GrpRwerte(0)(k).RefKurv(kw).R(i)
        Next i
      Next kw
    Next k
    For k = 0 To P - 1
      For j = 0 To NF - 1
        WV(k, j) = 0.0
        WN(k, j) = 0.0
        For l = 0 To RezAll.Rezepte(k).KF - 1
          If RezAll.Rezepte(k)(l).ID = RezAll.Farben(j).ID Then
            WN(k, j) = RezAll.Rezepte(k)(l).FaAmng
            Exit For
          End If
        Next l
      Next j
    Next k
    '
    '
    For j = 0 To NF - 1
      For kw = 0 To KM - 1
        For i = 0 To NWE - 1
          H(j, kw, i) = 0.0
        Next i
      Next kw
    Next j

    '
    '
    '
    '
    'SUBROUTINE(WAAGMAT(Alph, GH, N, P, WV, WN, KML, NWEL, RV, RN, H, FEHL))

    Call WAAGMAT(Alph, GH, NF, P, WV(0, 0), WN(0, 0), KM, NWE, RV(0, 0, 0), RN(0, 0, 0), H(0, 0, 0), FEHL)
    Call WAAGRUCK(Alph, NF, P, WV(0, 0), WN(0, 0), KM, NWE, RV(0, 0, 0), H(0, 0, 0), RUE(0, 0, 0), FEHL)
    SUU = 0.0
    For k = 0 To P - 1
      For kw = 0 To KM - 1
        For i = 0 To NWE - 1
          SUU = SUU + (RN(k, kw, i) - RUE(k, kw, i)) ^ 2
        Next i
      Next kw
    Next k
    Varianz = Sqrt(SUU / (P * KM * NWE - 1))
  End Sub
  Sub WaagenKorrektur(ALPH As Single, farben As Colorants, H(,,) As Single, Rval As RefValue, ByRef W() As Single)
    Dim i As Integer
    Dim kw As Integer
    Dim k As Integer
    Dim j As Integer
    Dim ier As Integer
    Dim NF As Integer
    Dim Q As Integer = 1
    Dim Gw(0) As Single
    Dim WU() As Single
    Dim WO() As Single
    Dim WI(,) As Single
    Dim RS(,) As Single
    Dim RI(,,) As Single
    Dim RR(,) As Single
    Dim A(,) As Single
    Dim Km As Integer
    Dim NWE As Integer
    Dim MM As Integer
    Dim MEQ As Integer
    Dim MQ As Integer
    Gw(0) = 1.0
    NF = farben.FarbCount
    Km = MenueParam.Messg.Winkel.Km
    NWE = MenueParam.Messg.Winkel.Wsol.Nwe
    ReDim W(NF - 1)
    ReDim WU(NF - 1)
    ReDim WO(NF - 1)
    ReDim WI(Q - 1, NF - 1)
    ReDim W(NF - 1)
    ReDim RS(Km - 1, NWE - 1)
    ReDim RI(Q - 1, Km - 1, NWE - 1)
    ReDim RR(Km - 1, NWE - 1)
    '
    '
    '
    For k = 0 To Q - 1
      For kw = 0 To Km - 1
        For i = 0 To NWE - 1
          RI(k, kw, i) = 1.0
        Next i
      Next kw
    Next k
    For k = 0 To Q - 1
      For j = 0 To NF - 1
        WI(k, j) = 0
      Next j
    Next k
    '
    '

    For kw = 0 To Km - 1
      For i = 0 To NWE - 1
        RS(kw, i) = Rval.RefKurv(kw).R(i)
      Next i
    Next kw
    '
    '
    MM = 1
    MQ = 0
    MEQ = 1
    ReDim A(NF, MM + MQ - 1)
    For j = 0 To NF - 1
      A(j, 0) = 1.0
      WU(j) = 0.0
      WO(j) = 100.0
    Next
    A(NF, 0) = 100
    Call GetWinkNormGk("WAG", MenueParam.Menue.Kwj, MenueParam.Messg.Winkel, ier)
    Call WAAGREF(ALPH, NF, Q, Gw(0), W(0), WU(0), WO(0), WI(0, 0), Km, NWE, RS(0, 0), RI(0, 0, 0), H(0, 0, 0), RR(0, 0), MM, MEQ, MQ, A(0, 0), FEHL)
    If FEHL.Ifeh <> 0 Then
      MsgBox("FEHLER")
    End If
  End Sub
  Sub CalcDeckWerte(ByRef Winkel As AngGeos, ByRef GrpRwerte As RefValuesGrp, ByRef ParamAlle As ValuesGrpsAssigns, ByRef Ier As Integer)
    '
    '
    Dim NWE As Integer
    Dim Iee As Integer
    Dim BoolIer() As Boolean
    '
    'Anzahl R-Kurven (Messung)
    '
    '
    Dim KM As Integer
    Dim Nlz As Integer
    Dim Nu As Integer
    '
    '
    '
    Dim i As Integer
    Dim j As Integer
    Dim l As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim kwf As Integer
    '
    '
    '
    '
    '
    Dim Npam As Integer
    '
    Dim Nqu As Integer
    Dim Nqum As Integer
    Dim Nqub As Integer
    Dim Nlauf As Integer
    '
    '
    '
    '
    Dim AufgArt(11) As Byte
    Dim AAufgart(11) As Byte
    Dim Ianwsg As Integer
    Dim StrRw As String
    Dim NLQ As Integer
    Dim J2 As Integer
    Dim KFIT As Integer
    Dim NFAECHAR As Integer
    Dim NFAEWRT As Integer
    Dim FAECHAR(,,) As Single
    '
    '
    '
    '
    '
    '
    Dim ParMerk(63) As Single      'Parameter zur Berechnung von Merkmalen
    '
    '
    Dim TyRwert() As TyRwert        'R-Werte Messung/Vorlage
    Dim Qurwert() As TyQual        'Zusätze für Qualität
    Dim RwertName() As String
    Dim TyRunnd() As TyRwert        'R-Werte unendlich dick
    Dim QuRunnd() As TyQual        'Zusätze für Qualität
    Dim RunndName() As String
    Dim TyRcrom() As TyRwert        'R-Werte max Chroma
    Dim QuRcrom() As TyQual        'Zusätze für Qualität
    Dim RcromName() As String
    Dim TyRwerb() As TyRwert        'R-Werte Rechnung
    Dim QuRwerb() As TyQual
    Dim TyRwerp() As TyRwert        'R-Werte Rechnung oder Messung (umgespeichert)
    Dim QuRwerp() As TyQual
    Dim TyGrund() As TyRwert        'K/S-Werte;Extinktions-Werte;Abs-Werte;Streuwerte
    Dim QuGrund() As TyQual
    Dim GrundName() As String
    Dim Rwert(,,) As Single
    Dim Runnd(,,) As Single
    Dim Rcrom(,,) As Single
    Dim Rwerb(,,) As Single
    Dim Rwerp(,,) As Single
    Dim Grund(,,) As Single
    Dim TyUnt() As TyRwert
    Dim QuUnt() As TyQual
    Dim Runt(,,) As Single
    Dim Param() As TyWert
    Dim Wert(,) As Double

    '
    'Param-Structure für Messung
    '
    Dim Parab() As TyWert
    Dim Parac() As TyWert
    Dim Wertb(,) As Double
    '
    '

    '
    '
    '
    '
    'Dim AusWhl As TyAuswhl
    '
    '
    'Untergründe
    '
    '
    FEHL.Ifeh = 0
    FEHL.Iwarn = 0
    '

    NLQ = GrpRwerte("W").Count
    If NLQ = 0 Then
      NLQ = GrpRwerte("S").Count
    End If
    If NLQ = 0 Then
      Ier = 2970
      MsgBox(Texxt(Ier))
      Exit Sub
    End If
    '

    '
    'MsgBox ParamAlle(0).AufgArt
    '


    '
    '
    '
    '
    '
    '
    KM = 0
    If GrpRwerte("W").Count > 0 Then
      KM = GrpRwerte("W")(0).RefKurv.Count
    End If
    If KM <> Winkel.Km Then
      MsgBox("ERROR: Km does not agree")
      Exit Sub
    End If
    '
    Nqu = GrpRwerte("W").Count + GrpRwerte("S").Count
    Nlz = Menueparam.Normfa.Nlz
    NWE = Winkel.Wsol.Nwe
    ReDim TyUnt(4)
    ReDim QuUnt(4)
    ReDim Runt(4, KM - 1, NWE - 1)
    Call StartUnt(Menueparam.Messg, Winkel, TyUnt, QuUnt, Runt)
    For i = 0 To Min(2, GrpRwerte.Count) - 1
      Call LetQurwert(GrpRwerte(i).RefUnt, QuUnt(i))
    Next

    '
    J2 = 1
    If Nqu = 2 * NLQ Then
      J2 = 2
    End If
    '
    '
    '
    '
    '
    'Löschen Werte in ParamAlle
    '
    '
    For i = 0 To ParamAlle.Count - 1
      ParamAlle(i).clear()
    Next i
    '
    '
    'Aufbau ParamAlle
    '
    '
    '
    For i = 0 To ParamAlle.Count - 1
      Nlauf = Nlaaf(J2, ParamAlle(i).AufgArt)
      For j = 0 To Nlauf * NLQ - 1
        StrRw = KeyRe(j)
        ParamAlle(i).Add(StrRw, New ValuesGrps)
        For k = 0 To Nlz - 1
          ParamAlle(i)(j).Add(KeyRe(Menueparam.Normfa(k).LichtID), New ValuesGrp)
          For kw = 0 To KM - 1
            ParamAlle(i)(j)(k).Add(Winkel(kw).Chrm, New Values)
            For l = 0 To ParamAlle(i).CountMerk - 1
              ParamAlle(i)(j)(k)(kw).Add(ParamAlle(i).Merk(l).Ken, New Object)
            Next l
          Next kw
        Next k
      Next j
    Next i

    '
    For i = 0 To ParamAlle.Count - 1
      ParamAlle(i).Clearwinkel()
      For j = 0 To Winkel.Km - 1
        ParamAlle(i).AddWinkel(Winkel(j).Chrm, New AnglExist)
        ParamAlle(i).Winkel(Winkel(j).Chrm).Exist = False
      Next
    Next
    '
    'NormFarbWerte,Winkel und GK-Werte übernehmen
    '
    '
    '
    '
    '
    Call GetWinkNormGk("SPZ", MenueParam.Menue.Kwj, Winkel, Iee)
    '

    '
    'Start Qualitätskontrolle
    '
    '
    '
    '
    Call QUABEG(NWE, KM, FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
      Exit Sub
    End If
    '
    '
    '
    'Menüparameter übernehmen
    '
    '
    Call GetMenueParam("SPZ", Iee)
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
    ReDim TyRwert(Nqu - 1)
    ReDim Qurwert(Nqu - 1)
    ReDim RwertName(Nqu - 1)
    ReDim Rwert(Nqu - 1, KM - 1, NWE - 1)
    ReDim BoolIer(KM - 1)  '
    '
    '
    'Gruppe Deckvermögen
    '
    Npam = Nqu
    '
    Nqub = Nqu
    ReDim TyRwerb(Nqu - 1)
    ReDim QuRwerb(Nqu - 1)
    ReDim Rwerb(Nqu - 1, KM - 1, NWE - 1)
    ReDim TyGrund(Nqu - 1)
    ReDim QuGrund(Nqu - 1)
    ReDim GrundName(Nqu - 1)
    ReDim Grund(Nqu - 1, KM - 1, NWE - 1)
    ReDim TyRunnd(Nqu / 2 - 1)
    ReDim QuRunnd(Nqu / 2 - 1)
    ReDim RunndName(Nqu - 1)
    ReDim Runnd(Nqu / 2 - 1, KM - 1, NWE - 1)
    ReDim TyRcrom(Nqu / 2 - 1)
    ReDim QuRcrom(Nqu / 2 - 1)
    ReDim RcromName(Nqu - 1)
    ReDim Rcrom(Nqu / 2 - 1, KM - 1, NWE - 1)

    ReDim Parab(Npam - 1)
    ReDim Wertb(Npam - 1, 63)
    '
    '
    '
    '
    '
    '
    '
    '
    '
    'R-Werte umspeichern nach Tyrwert und Rwert
    '
    '
    j = 0
    For i = 0 To NLQ - 1
      If GrpRwerte("W").Count > 0 Then
        Call GetTyrwert(NWE, KM, GrpRwerte("W")(i), TyRwert(j), j, Rwert)
        j = j + 1
      End If
      If GrpRwerte("S").Count > 0 Then
        Call GetTyrwert(NWE, KM, GrpRwerte("S")(i), TyRwert(j), j, Rwert)
        j = j + 1
      End If
    Next i
    j = 0
    For i = 0 To NLQ - 1
      If GrpRwerte("W").Count > 0 Then
        Call GetQurwert(GrpRwerte("W")(i), Qurwert(j), RwertName(j))
        j = j + 1
      End If
      If GrpRwerte("S").Count > 0 Then
        Call GetQurwert(GrpRwerte("S")(i), Qurwert(j), RwertName(j))
        j = j + 1
      End If
    Next i
    '
    'optische Daten umspeichern
    '
    '
    KFIT = 2
    j = 0
    For i = 0 To NLQ - 1
      If GrpRwerte("A").Count > 0 And GrpRwerte("D").Count > 0 Then
        If GrpRwerte("A")(i).QuControl.Cart <> "" And GrpRwerte("D")(i).QuControl.Cart <> "" Then
          KFIT = 1
          Call GetTyrwert(NWE, KM, GrpRwerte("A")(i), TyGrund(j), j, Grund)
          Call GetQurwert(GrpRwerte("A")(i), QuGrund(j), GrundName(j))
          j = j + 1
          Call GetTyrwert(NWE, KM, GrpRwerte("D")(i), TyGrund(j), j, Grund)
          Call GetQurwert(GrpRwerte("D")(i), QuGrund(j), GrundName(j))
        End If
        j = j + 1
      End If
    Next i
    '
    '
    '
    'Untergründe
    '
    '
    '

    j = 0
    If Not IsNothing(GrpRwerte("W").RefUnt) AndAlso GrpRwerte("W").RefUnt.IVoNa = True Then
      Call GetTyrwert(NWE, KM, GrpRwerte("W").RefUnt, TyUnt(j), j, Runt)
      j = j + 1
    End If
    If Not IsNothing(GrpRwerte("S").RefUnt) AndAlso GrpRwerte("S").RefUnt.IVoNa = True Then
      Call GetTyrwert(NWE, KM, GrpRwerte("S").RefUnt, TyUnt(j), j, Runt)
      j = j + 1
    End If
    Nu = j

    '
    'Felder für Färbecharakteristik
    '
    '
    If KFIT = 2 Then
      NFAECHAR = ParamAlle.Auxiliary(0)(0).Nwe
    Else
      NFAECHAR = 1
    End If
    '
    '
    NFAEWRT = ParamAlle.Auxiliary(0).Count
    '
    '
    '
    ReDim FAECHAR(NLQ - 1, NFAEWRT - 1, NFAECHAR - 1)
    '
    '
    '
    'Merkmale für Deckvermögen usw. berechnen
    '
    '

    Nqum = Nqu
    Nqub = Nqu

    For kw = 0 To KM - 1
      kwf = kw + 1
      '
      '
      '
      '
      'Gruppe Deckvermögen
      '
      '

      '*****************
      '*****************
      '*** DLL-Aufrufe, falls R-Werte berechnet werden **
      '*****************
      '*****************

      '
      '
      'Deckvermögen
      '
      '
      '
      For i = 0 To ParamAlle.AuxCount - 1
        For j = 0 To NFAECHAR - 1

          '
          'Menge(Bunt) übernehmen
          '
          '

          FAECHAR(i, 0, j) = ParamAlle.Auxiliary(GrpRwerte("W").RwKey(i))(0).R(j)
        Next j
      Next i
      Call DEKTRAS(KFIT, kwf, NWE, KM, Nqu, _
            Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
            QuRunnd(0), TyRunnd(0), Runnd(0, 0, 0), QuRcrom(0), TyRcrom(0), Rcrom(0, 0, 0), _
            QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), NFAECHAR, NFAEWRT, FAECHAR(0, 0, 0), _
            Npam, Parab(0), Wertb(0, 0), FEHL)
      If FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
        Exit Sub
      End If
      '
      '
      '
      'Färbecharacteristik umspeichern
      '
      If KFIT = 2 And kw = 0 Then
        For i = 0 To ParamAlle.AuxCount - 1
          For k = 0 To ParamAlle.Auxiliary(i).Count - 1
            For j = 0 To ParamAlle.Auxiliary(i)(k).Nwe - 1
              'k=0 Konzentrationsangabe
              'k=1 Helligkeit (L*)
              'k=2 Chroma
              'k=3 hue
              'k=4 a*
              'k=5 b*
              ParamAlle.Auxiliary(i)(k).R(j) = FAECHAR(i, k, j)
            Next j
          Next k
        Next i
      End If
      '
      Nqum = Nqu
      Nqub = Nqu
      '
      '
      '
      '
      '

      '
      '
      Ier = FEHL.Ifeh
      If Not FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
        For i = 0 To ParamAlle.Count - 1
          ParamAlle(i).Winkel(Winkel(kw).Chrm).Exist = True
        Next        '
        '
        '  Berechnete Werte übernehmen
        '
        '
        '
        For Ianwsg = 0 To ParamAlle.Count - 1
          Call LetWeisungWerte(0, kw, MenueParam.User.Winkel, Npam, Parab, Wertb, ParamAlle(Ianwsg), ParamAlle.HoleNum, Iee)
        Next Ianwsg
      End If
      '
    Next kw
    '
    '
    '
    '
    '
    '  Berechnete R-Werte übernehmen
    '
    GrpRwerte("H").clear()
    GrpRwerte("F").clear()
    GrpRwerte("U").clear()
    GrpRwerte("C").clear()

    '
    '
    '
    'Berechnete R-Werte
    '
    For i = 0 To Nqu - 1 Step J2
      '
      'Weiß
      GrpRwerte("H").Add(KeyRe(TyRwerb(i).RwertNr), New RefValue)
      Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRwerb(i).RwertNr)), GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), Winkel, Iee)
      Call LetTyrwert(NWE, KM, GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), i, Rwerb)
      GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)).QuControl = New QuControls
      Call LetQurwert(GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), QuRwerb(i))
      '
      'Schwarz
      GrpRwerte("F").Add(KeyRe(TyRwerb(i + 1).RwertNr), New RefValue)
      Call LetNamBemDat(GrpRwerte("S")(KeyRe(TyRwerb(i + 1).RwertNr)), GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), TyRwerb(i + 1), Winkel, Iee)
      Call LetTyrwert(NWE, KM, GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), TyRwerb(i + 1), i + 1, Rwerb)
      GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)).QuControl = New QuControls
      Call LetQurwert(GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), QuRwerb(i + 1))
    Next i
    '
    ' R-Werte (unendlich dick)
    '
    For i = 0 To Nqu / 2 - 1
      GrpRwerte("U").Add(KeyRe(TyRunnd(i).RwertNr), New RefValue)
      Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRunnd(i).RwertNr)), GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)), TyRunnd(i), Winkel, Iee)
      Call LetTyrwert(NWE, KM, GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)), TyRunnd(i), i, Runnd)
      GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)).QuControl = New QuControls
      Call LetQurwert(GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)), QuRunnd(i))
    Next i
    '
    ' R-Werte (max Chroma)
    '
    For i = 0 To Nqu / 2 - 1
      GrpRwerte("C").Add(KeyRe(TyRcrom(i).RwertNr), New RefValue)
      Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRcrom(i).RwertNr)), GrpRwerte("C")(KeyRe(TyRcrom(i).RwertNr)), TyRcrom(i), Winkel, Iee)
      Call LetTyrwert(NWE, KM, GrpRwerte("C")(KeyRe(TyRcrom(i).RwertNr)), TyRcrom(i), i, Rcrom)
      GrpRwerte("C")(KeyRe(TyRcrom(i).RwertNr)).QuControl = New QuControls
      Call LetQurwert(GrpRwerte("C")(KeyRe(TyRcrom(i).RwertNr)), QuRcrom(i))
    Next i
    If KFIT = 2 Then
      GrpRwerte("A").clear()
      GrpRwerte("D").clear()

      '
      ' (Absorption)
      '
      For i = 0 To Nqu - 1 Step 2
        GrpRwerte("A").Add(KeyRe(TyGrund(i).RwertNr), New RefValue)
        Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyGrund(i).RwertNr)), GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), Winkel, Iee)
        Call LetTyrwert(NWE, KM, GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), i, Grund)
        GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)).QuControl = New QuControls
        Call LetQurwert(GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)), QuGrund(i))
      Next i
      '
      ' (Streuung)
      '
      For i = 1 To Nqu - 1 Step 2
        GrpRwerte("D").Add(KeyRe(TyGrund(i).RwertNr), New RefValue)
        Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyGrund(i).RwertNr)), GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), Winkel, Iee)
        Call LetTyrwert(NWE, KM, GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), i, Grund)
        GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)).QuControl = New QuControls
        Call LetQurwert(GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)), QuGrund(i))
      Next i
    End If
    '
    '
    '
    '
    '
    '
    '
    '
    '
    'Ende Qualitätskontrolle
    '
    '
    '
    '
    Call QUAEND(FEHL)
    '
    '
    '
    '
    'Untergründe übernehmen
    '
    '
    GrpRwerte("W").RefUnt.RefKurv.clear()
    Call LetNamBemDat(GrpRwerte("W").RefUnt, GrpRwerte("W").RefUnt, TyUnt(0), Winkel, Iee)
    Call LetTyrwert(NWE, KM, GrpRwerte("W").RefUnt, TyUnt(0), 0, Runt)
    Call LetQurwert(GrpRwerte("W").RefUnt, QuUnt(0))
    GrpRwerte("S").RefUnt.RefKurv.clear()
    Call LetNamBemDat(GrpRwerte("S").RefUnt, GrpRwerte("S").RefUnt, TyUnt(1), Winkel, Iee)
    Call LetTyrwert(NWE, KM, GrpRwerte("S").RefUnt, TyUnt(1), 1, Runt)
    Call LetQurwert(GrpRwerte("S").RefUnt, QuUnt(1))

    '
    '
    '
    '
    'Farbwerte für alle Anweisungen
    '
    '
    For Ianwsg = 0 To ParamAlle.Count - 1
      Call quali.FarbwertAnweisung(Winkel, ParamAlle(Ianwsg), GrpRwerte, Ier)
    Next Ianwsg

    '
    '
    '
    '
    '
    '


    '
    '
    Erase Parac
    Erase Parab
    Erase Param
    Erase Wert
    Erase Wert
    Erase Wert
    Erase TyRwert
    Erase Qurwert
    Erase Rwert
    Erase TyRwerb
    Erase QuRwerb
    Erase Rwerb
    Erase TyRwerp
    Erase QuRwerp
    Erase Rwerp
    Erase TyRcrom
    Erase QuRcrom
    Erase Rcrom
    Erase TyRunnd
    Erase QuRunnd
    Erase Runnd
    Erase TyGrund
    Erase QuGrund
    Erase Grund
    '
    ''
  End Sub
  Sub CalcFastWerte(ByRef Winkel As AngGeos, ByRef GrpRwerte As RefValuesGrp, ByRef ParamAlle As ValuesGrpsAssigns, ByRef Ier As Integer)
    '
    '
    Dim NWE As Integer
    Dim Iee As Integer
    '
    'Anzahl R-Kurven (Messung)
    '
    '
    Dim KM As Integer
    Dim Nlz As Integer
    Dim Nu As Integer
    '
    '
    '
    Dim i As Integer
    Dim j As Integer
    Dim l As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim kwf As Integer
    '
    '
    '
    '
    Dim Iprn As Integer
    Dim Npam As Integer
    '
    Dim Nqu As Integer
    Dim Nqum As Integer
    Dim Nqub As Integer
    Dim Nlauf As Integer
    '
    '
    '
    '
    Dim AufgArt(11) As Byte
    Dim AAufgart(11) As Byte
    Dim Ianwsg As Integer
    Dim StrRw As String

    Dim NLQ As Integer
    Dim J2 As Integer
    Dim KFIT As Integer
    Dim NFAECHAR As Integer
    Dim NFAEWRT As Integer = 6
    Dim FAECHAR(,,) As Single
    '

    '
    '
    '
    '
    '
    Dim ParMerk(63) As Single      'Parameter zur Berechnung von Merkmalen
    '
    '
    Dim TyRwert() As TyRwert        'R-Werte Messung/Vorlage
    Dim Qurwert() As TyQual        'Zusätze für Qualität
    Dim RwertName() As String
    Dim TyRunnd() As TyRwert        'R-Werte unendlich dick
    Dim QuRunnd() As TyQual        'Zusätze für Qualität
    Dim TyRcrom() As TyRwert        'R-Werte max Chroma
    Dim QuRcrom() As TyQual        'Zusätze für Qualität
    Dim TyRwerb() As TyRwert        'R-Werte Rechnung
    Dim QuRwerb() As TyQual
    Dim RwerbName() As String
    Dim TyRwerp() As TyRwert        'R-Werte Rechnung oder Messung (umgespeichert)
    Dim QuRwerp() As TyQual
    Dim TyGrund() As TyRwert        'K/S-Werte;Extinktions-Werte;Abs-Werte;Streuwerte
    Dim QuGrund() As TyQual
    Dim GrundName() As String
    Dim Rwert(,,) As Single
    Dim Runnd(,,) As Single
    Dim Rcrom(,,) As Single
    Dim Rwerb(,,) As Single
    Dim Rwerp(,,) As Single
    Dim Grund(,,) As Single
    Dim TyUnt() As TyRwert
    Dim QuUnt() As TyQual
    Dim UntName() As String
    Dim Runt(,,) As Single
    Dim Param() As TyWert
    Dim Wert(,) As Double

    '
    'Param-Structure für Messung
    '
    Dim Parab() As TyWert
    Dim Parac() As TyWert
    Dim Wertb(,) As Double
    Dim Wertc(,) As Double
    '
    '

    '
    '
    '
    '
    'Dim AusWhl As TyAuswhl
    '
    '
    'Untergründe
    '
    '
    FEHL.Ifeh = 0
    FEHL.Iwarn = 0
    '

    NLQ = GrpRwerte("W").Count
    If NLQ = 0 Then
      NLQ = GrpRwerte("S").Count
    End If
    If NLQ = 0 Then
      Ier = 2970
      MsgBox(Texxt(Ier))
      Exit Sub
    End If
    '
    'Parameter zur Berechnung der Merkmale in FARKORall
    '
    '
    '
    'MsgBox ParamAlle(0).AufgArt
    '


    '
    '
    '
    '
    '
    '
    KM = 0
    If GrpRwerte("W").Count > 0 Then
      KM = GrpRwerte("W")(0).RefKurv.Count
    End If
    If KM <> Winkel.Km Then
      MsgBox("ERROR: Km does not agree")
      Exit Sub
    End If
    '
    Nqu = GrpRwerte("W").Count
    Nlz = Menueparam.Normfa.Nlz
    NWE = Winkel.Wsol.Nwe
    ReDim TyUnt(4)
    ReDim QuUnt(4)
    ReDim UntName(4)
    ReDim Runt(4, KM - 1, NWE - 1)
    Call StartUnt(Menueparam.Messg, Winkel, TyUnt, QuUnt, Runt)
    For i = 0 To Min(2, GrpRwerte.Count) - 1
      Call LetQurwert(GrpRwerte(i).RefUnt, QuUnt(i))
    Next
    '
    J2 = 1
    '
    '
    '
    '
    '
    'Löschen Werte in ParamAlle
    '
    '
    For i = 0 To ParamAlle.Count - 1
      ParamAlle(i).clear()
    Next i
    '
    '
    'Aufbau ParamAlle
    '
    '
    '
    For i = 0 To ParamAlle.Count - 1
      Nlauf = Nlaaf(J2, ParamAlle(i).AufgArt)
      For j = 0 To Nlauf * NLQ - 1
        StrRw = KeyRe(j)
        ParamAlle(i).Add(StrRw, New ValuesGrps)
        For k = 0 To Nlz - 1
          ParamAlle(i)(j).Add(KeyRe(Menueparam.Normfa(k).LichtID), New ValuesGrp)
          For kw = 0 To KM - 1
            ParamAlle(i)(j)(k).Add(Winkel(kw).Chrm, New Values)
            For l = 0 To ParamAlle(i).CountMerk - 1
              ParamAlle(i)(j)(k)(kw).Add(ParamAlle(i).Merk(l).Ken, New Object)
            Next l
          Next kw
        Next k
      Next j
    Next i

    For i = 0 To ParamAlle.Count - 1
      ParamAlle(i).Clearwinkel()
      For j = 0 To Winkel.Km - 1
        ParamAlle(i).AddWinkel(Winkel(j).Chrm, New AnglExist)
        ParamAlle(i).Winkel(Winkel(j).Chrm).Exist = False
      Next
    Next
    '
    '
    'NormFarbWerte,Winkel und GK-Werte übernehmen
    '
    '
    '
    '
    '
    Call GetWinkNormGk("SPZ", MenueParam.Menue.Kwj, Winkel, Iee)
    '

    '
    'Start Qualitätskontrolle
    '
    '
    '
    '
    Call QUABEG(NWE, KM, FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
      Exit Sub
    End If
    '
    '
    '
    'Menüparameter übernehmen
    '
    '
    Call GetMenueParam("SPZ", Iee)
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
    ReDim TyRwert(Nqu - 1)
    ReDim Qurwert(Nqu - 1)
    ReDim RwertName(Nqu - 1)
    ReDim Rwert(Nqu - 1, KM - 1, NWE - 1)
    '
    '

    '

    '
    '
    'Gruppe Farbstärke
    '
    '
    Npam = Nqu
    Nqub = Nqu
    ReDim TyRwerb(Nqu - 1)
    ReDim QuRwerb(Nqu - 1)
    ReDim RwerbName(Nqu - 1)
    ReDim Rwerb(Nqu - 1, KM - 1, NWE - 1)
    ReDim TyGrund(Nqu - 1)
    ReDim QuGrund(Nqu - 1)
    ReDim GrundName(Nqu - 1)
    ReDim Grund(Nqu - 1, KM - 1, NWE - 1)
    ReDim Parab(Nqu - 1)
    ReDim Parac(Nqu - 1)
    ReDim Wertb(Nqu - 1, 63)
    ReDim Wertc(Nqu - 1, 63)
    Iprn = 0
    '
    '
    '
    '
    '
    '
    '
    'R-Werte umspeichern nach Tyrwert und Rwert
    '
    '
    '
    '
    Select Case Menueparam.MethID
      Case 4
        KFIT = 2
        j = 0
        For i = 0 To NLQ - 1
          If GrpRwerte("W").Count > 0 Then
            Call GetTyrwert(NWE, KM, GrpRwerte("W")(i), TyRwert(j), j, Rwert)
            If GrpRwerte("K")(i).QuControl.Cart <> "" Then
              KFIT = 1
              Call GetTyrwert(NWE, KM, GrpRwerte("K")(i), TyGrund(j), j, Grund)
            End If
            j = j + 1
          End If
        Next i
        j = 0
        For i = 0 To NLQ - 1
          If GrpRwerte("W").Count > 0 Then
            Call GetQurwert(GrpRwerte("W")(i), Qurwert(j), RwertName(j))
            If GrpRwerte("K")(i).QuControl.Cart <> "" Then
              Call GetQurwert(GrpRwerte("K")(i), QuGrund(j), GrundName(j))
            End If
            j = j + 1
          End If
        Next i
      Case 5
        KFIT = 2
        j = 0
        For i = 0 To NLQ - 1
          If GrpRwerte("W").Count > 0 Then
            Call GetTyrwert(NWE, KM, GrpRwerte("W")(i), TyRwert(j), j, Rwert)
            If GrpRwerte("E")(i).QuControl.Cart <> "" Then
              KFIT = 1
              Call GetTyrwert(NWE, KM, GrpRwerte("E")(i), TyGrund(j), j, Grund)
            End If
            j = j + 1
          End If
        Next i
        j = 0
        For i = 0 To NLQ - 1
          If GrpRwerte("W").Count > 0 Then
            Call GetQurwert(GrpRwerte("W")(i), Qurwert(j), RwertName(j))
            If GrpRwerte("E")(i).QuControl.Cart <> "" Then
              Call GetQurwert(GrpRwerte("E")(i), QuGrund(j), GrundName(j))
            End If
            j = j + 1
          End If
        Next i
    End Select
    '
    '
    '
    '
    '
    'Untergründe
    '
    '
    '

    j = 0
    If Not IsNothing(GrpRwerte("W").RefUnt) AndAlso GrpRwerte("W").RefUnt.IVoNa = True Then
      Call GetTyrwert(NWE, KM, GrpRwerte("W").RefUnt, TyUnt(j), j, Runt)
      j = j + 1
    End If
    If Not IsNothing(GrpRwerte("S").RefUnt) AndAlso GrpRwerte("S").RefUnt.IVoNa = True Then
      Call GetTyrwert(NWE, KM, GrpRwerte("S").RefUnt, TyUnt(j), j, Runt)
      j = j + 1
    End If
    Nu = j

    '
    'Felder für Färbecharakteristik
    '
    '
    '
    'Felder für Färbecharakteristik
    '
    '
    If KFIT = 2 Then
      NFAECHAR = ParamAlle.Auxiliary(0)(0).Nwe
    Else
      NFAECHAR = 1
    End If
    '
    '
    NFAEWRT = ParamAlle.Auxiliary(0).Count
    '
    '
    '
    ReDim FAECHAR(NLQ - 1, NFAEWRT - 1, NFAECHAR - 1)
    '
    '
    '

    '
    '
    For i = 0 To ParamAlle.AuxCount - 1
      For j = 0 To NFAECHAR - 1

        '
        'Menge(Bunt) übernehmen
        '
        '

        FAECHAR(i, 0, j) = ParamAlle.Auxiliary(GrpRwerte("W").RwKey(i))(0).R(j)
      Next j
    Next i
    '
    '
    'Merkmale für Farbstärke  usw. berechnen
    '
    '

    Nqum = Nqu
    Nqub = Nqu

    For kw = 0 To KM - 1
      kwf = kw + 1
      '
      '
      '
      '
      'Gruppe Farbstärken (Bestimmung des richtigen B-Wertes
      '
      '
      '
      'Suche nach "günstigstem" B-Wert
      '
      '
      If Menueparam.Menue.BwertID = 0 Then
        '
        'B-Wert
        '
        Menueparam.Menue.BwertID = BWEBER(kwf, NWE, KM, Nqu, Qurwert(0), TyRwert(0), Rwert(0, 0, 0), FEHL)
        If FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
          Exit Sub
        End If
      End If
      If Menueparam.Menue.BwertID <= 0 Then
        Menueparam.Menue.BwertID = 1
      End If
      '*****************
      '*****************
      '*** DLL-Aufrufe, falls R-Werte berechnet werden **
      '*****************
      '*****************
      Select Case Menueparam.MethID
        '
        '
        '    Farbstärke (deckend)
        '
        '
        '
        Case 4
          If KFIT = 2 Then
            For i = 0 To ParamAlle.AuxCount - 1
              For j = 0 To NFAECHAR - 1

                '
                'Menge(Bunt) übernehmen
                '
                '

                FAECHAR(i, 0, j) = ParamAlle.Auxiliary(GrpRwerte("W").RwKey(i))(0).R(j)
              Next j
            Next i
          End If
          Call FASDEKS(KFIT, kwf, NWE, KM, Nqu, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), NFAECHAR, NFAEWRT, FAECHAR(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)
          If FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
            Exit Sub
          End If
        Case 5
          '
          '
          '    Farbstärke (transparent)
          '
          '

          If KFIT = 2 Then
            For i = 0 To ParamAlle.AuxCount - 1
              For j = 0 To NFAECHAR - 1

                '
                'Menge(Bunt) übernehmen
                '
                '

                FAECHAR(i, 0, j) = ParamAlle.Auxiliary(GrpRwerte("W").RwKey(i))(0).R(j)
              Next j
            Next i
          End If
          Call FASTRAS(KFIT, kwf, NWE, KM, Nqu, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), NFAECHAR, NFAEWRT, FAECHAR(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)
          If FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
            Exit Sub
          End If
      End Select
      '
      '
      '
      'Färbecharacteristik umspeichern
      '
      If KFIT = 2 Then
        For i = 0 To ParamAlle.AuxCount - 1
          For k = 0 To ParamAlle.Auxiliary(i).Count - 1
            For j = 0 To ParamAlle.Auxiliary(i)(k).Nwe - 1
              'k=0 Konzentrationsangabe
              'k=1 Helligkeit (L*)
              'k=2 Chroma
              'k=3 hue
              'k=4 a*
              'k=5 b*
              ParamAlle.Auxiliary(i)(k).R(j) = FAECHAR(i, k, j)
            Next j
          Next k
        Next i
      End If
      '
      Nqum = Nqu
      Nqub = Nqu
      '
      '
      '
      '
      '

      '
      '
      Ier = FEHL.Ifeh
      If Not FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
        For i = 0 To ParamAlle.Count - 1
          ParamAlle(i).Winkel(Winkel(kw).Chrm).Exist = True
        Next
        '
        '
        'Berechnete Werte übernehmen
        '
        '
        '
        For Ianwsg = 0 To ParamAlle.Count - 1
          Call LetWeisungWerte(0, kw, MenueParam.User.Winkel, Npam, Parab, Wertb, ParamAlle(Ianwsg), ParamAlle.HoleNum, Iee)
        Next Ianwsg
      End If
      '
    Next kw
    '
    '
    '
    '
    '
    'Berechnete R-Werte übernehmen
    '
    GrpRwerte("H").clear()
    '
    '
    '
    For i = 0 To Nqub - 1
      GrpRwerte("H").Add(KeyRe(TyRwerb(i).RwertNr), New RefValue)

      Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRwerb(i).RwertNr)), GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), Winkel, Iee)
      Call LetTyrwert(NWE, KM, GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), i, Rwerb)
      GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)).QuControl = New QuControls
      Call LetQurwert(GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), QuRwerb(i))
    Next i
    '
    '
    '
    Select Case Menueparam.MethID
      '
      '
      ' (K/S)
      '
      Case 4
        If KFIT = 2 Then
          GrpRwerte("K").clear()
          For i = 0 To Nqub - 1
            GrpRwerte("K").Add(KeyRe(TyGrund(i).RwertNr), New RefValue)
            Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyGrund(i).RwertNr)), GrpRwerte("K")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), Winkel, Iee)
            Call LetTyrwert(NWE, KM, GrpRwerte("K")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), i, Grund)
            GrpRwerte("K")(KeyRe(TyGrund(i).RwertNr)).QuControl = New QuControls
            Call LetQurwert(GrpRwerte("K")(KeyRe(TyGrund(i).RwertNr)), QuGrund(i))
          Next i
        End If
        '
        '
      Case 5
        '
        'Extinktion
        '
        '
        If KFIT = 2 Then
          GrpRwerte("E").clear()
          For i = 0 To Nqub - 1
            GrpRwerte("E").Add(KeyRe(TyGrund(i).RwertNr), New RefValue)
            Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyGrund(i).RwertNr)), GrpRwerte("E")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), Winkel, Iee)
            Call LetTyrwert(NWE, KM, GrpRwerte("E")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), i, Grund)
            GrpRwerte("E")(KeyRe(TyGrund(i).RwertNr)).QuControl = New QuControls
            Call LetQurwert(GrpRwerte("E")(KeyRe(TyGrund(i).RwertNr)), QuGrund(i))
          Next i
        End If
    End Select
    '
    '
    'Ende Qualitätskontrolle
    '
    '
    '
    '
    '
    Call QUAEND(FEHL)
    '
    '
    '
    'Untergründe übernehmen
    '
    '
    GrpRwerte("W").RefUnt.RefKurv.clear()
    Call LetNamBemDat(GrpRwerte("W").RefUnt, GrpRwerte("W").RefUnt, TyUnt(0), Winkel, Iee)
    Call LetTyrwert(NWE, KM, GrpRwerte("W").RefUnt, TyUnt(0), 0, Runt)
    Call LetQurwert(GrpRwerte("W").RefUnt, QuUnt(0))
    GrpRwerte("S").RefUnt.RefKurv.clear()
    Call LetNamBemDat(GrpRwerte("S").RefUnt, GrpRwerte("S").RefUnt, TyUnt(1), Winkel, Iee)
    Call LetTyrwert(NWE, KM, GrpRwerte("S").RefUnt, TyUnt(1), 1, Runt)
    Call LetQurwert(GrpRwerte("S").RefUnt, QuUnt(1))


    '
    '
    For Ianwsg = 0 To ParamAlle.Count - 1
      Call quali.FarbwertAnweisung(Winkel, ParamAlle(Ianwsg), GrpRwerte, Ier)
    Next Ianwsg
    '
    '
    Erase Parac
    Erase Parab
    Erase Param
    Erase Wert
    Erase Wert
    Erase Wert
    Erase TyRwert
    Erase Qurwert
    Erase Rwert
    Erase TyRwerb
    Erase QuRwerb
    Erase Rwerb
    Erase TyRwerp
    Erase QuRwerp
    Erase Rwerp
    Erase TyRcrom
    Erase QuRcrom
    Erase Rcrom
    Erase TyRunnd
    Erase QuRunnd
    Erase Runnd
    Erase TyGrund
    Erase QuGrund
    Erase Grund
    '
    ''
  End Sub
  Sub CalcLCHBw0(ByRef Winkel As AngGeos, ICHL As Integer, ByRef Lst As List(Of Single()), ByRef Cst As List(Of Single()), ByRef hst As List(Of Single()), ByRef Ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim NWE As Integer
    Dim KM As Integer
    Dim LCH(2) As Single
    '
    '
    Call GetWinkNormGk("SPZ", MenueParam.Menue.Kwj, Winkel, Ier)
    If Ier <> 0 Then Exit Sub
    '
    '
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    Call QUABEG(NWE, KM, FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
      Exit Sub
    End If
    '
    Call GetMenueParam("SPZ", Ier)

    '
    '
    'Berechnung von L*
    '
    For j = 0 To Lst.Count - 1
      For i = 0 To Lst(j).Length - 1
        LCH(0) = Lst(j)(i)
        LCH(1) = Cst(j)(i)
        LCH(2) = hst(j)(i)
        'Call YSWSTBW(ICHL, LCH(0), FEHL)
        Call LCHSTBW(ICHL, LCH(0), FEHL)
        Select Case ICHL
          Case 1
            Lst(j)(i) = LCH(0)
          Case 2
            Cst(j)(i) = LCH(1)
          Case 3
            hst(j)(i) = LCH(2)
        End Select
        Ier = FEHL.Ifeh
        If Ier <> 0 Then
          ReDim Preserve Lst(j)(i - 1)
          Exit For
        End If
      Next i
      k = Cst(j).Count
    Next j
    '
    '
    '
    Call QUAEND(FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
      Exit Sub
    End If

  End Sub
  Sub CalcYSWBw0(ByRef Winkel As AngGeos, ICHL As Integer, ByRef Lst As List(Of Single()), ByRef Cst As List(Of Single()), ByRef hst As List(Of Single()), ByRef Ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim NWE As Integer
    Dim KM As Integer
    Dim LCH(2) As Single
    '
    '
    Call GetWinkNormGk("SPZ", MenueParam.Menue.Kwj, Winkel, Ier)
    If Ier <> 0 Then Exit Sub
    '
    '
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    Call QUABEG(NWE, KM, FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
      Exit Sub
    End If
    '
    Call GetMenueParam("SPZ", Ier)

    '
    '
    'Berechnung von L*
    '
    For j = 0 To Lst.Count - 1
      For i = 0 To Lst(j).Length - 1
        LCH(0) = Lst(j)(i)
        LCH(1) = Cst(j)(i)
        LCH(2) = hst(j)(i)
        'Call YSWSTBW(ICHL, LCH(0), FEHL)
        Call YSWSTBW(ICHL, LCH(0), FEHL)
        Select Case ICHL
          Case 1
            Lst(j)(i) = LCH(0)
          Case 2
            Cst(j)(i) = LCH(1)
          Case 3
            hst(j)(i) = LCH(2)
        End Select
        Ier = FEHL.Ifeh
        If Ier <> 0 Then
          ReDim Preserve Lst(j)(i - 1)
          Exit For
        End If
      Next i
      k = Cst(j).Count
    Next j
    '
    '
    '
    Call QUAEND(FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
      Exit Sub
    End If

  End Sub




  Sub dispose() Implements IDisposable.Dispose
    quali.dispose()
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub


  Public Sub New()
    MyBase.New()
    quali = New QualKontrolle
  End Sub

End Class
