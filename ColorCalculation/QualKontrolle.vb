Option Strict Off
Option Explicit On
Option Compare Text

Public Class QualKontrolle
  Implements IDisposable
  Dim i As Integer
  Dim Ier As Integer
  'Dim Menal As Tymenu
  'Dim Normx As Tynorm2
  Sub RefMit(ByRef Aka As Single, ByRef Winkel As AngGeos, ByRef CM() As Single, ByRef Rwerte As RefValues, ByRef RefWert As RefValue, ByRef Ier As Integer)

    Dim Rmit(,,) As Single
    Dim Ro(,) As Single
    Dim Ru(,) As Single
    Dim R(,,) As Single
    Dim NWE As Integer
    Dim KM As Integer
    Dim Nanz As Integer
    Dim NGK As Integer
    Dim AKaa As Single
    Dim i As Integer
    Dim k As Integer
    Dim GK(,) As Single
    Dim IGLZ() As Integer
    Dim GKINT() As Single
    Nanz = Rwerte.Count
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    NGK = 16
    ReDim IGLZ(KM - 1)
    ReDim GKINT(KM - 1)
    ReDim Rmit(Nanz - 1, KM - 1, NWE - 1)
    ReDim Ro(KM - 1, NWE - 1)
    ReDim Ru(KM - 1, NWE - 1)
    ReDim R(0, KM - 1, NWE - 1)

    ReDim GK(KM - 1, NGK - 1)
    AKaa = Aka
    For kw = 0 To KM - 1
      For i = 0 To NGK - 1
        GK(kw, i) = Winkel(kw).GK(i)
      Next
    Next kw
    If Rwerte.Count = 0 Then Exit Sub
    Ier = FEHL.Ifeh
    For k = 0 To Rwerte.Count - 1
      Call GetRwerte(Winkel, k, Rmit, Rwerte(k))
      'Call RMEMA(NWE, KM, Rmit(0, 0, 0), FEHL)
      Ier = FEHL.Ifeh
    Next k
    Call RwertMit(Aka, NGK, GK(0, 0), Nanz, KM, NWE, CM(0), Rmit(0, 0, 0), Ru(0, 0), R(0, 0, 0), Ro(0, 0), FEHL)


    Ier = FEHL.Ifeh
    Call ADDCurves(RefWert.RefKurv)
    'RefWert.RefKurv.clear()
    'For k = 0 To KM - 1
    'RefWert.RefKurv.Add(Winkel(k).Chrm, New CurveRef(NWE))
    'Next k
    Call LetRwerte(Winkel, 0, R, RefWert)

  End Sub
  Sub RefCorr(ByRef Aka As Single, ByRef Winkel As AngGeos, ByRef FarbRR As RefValue, ByRef FarbR2 As RefValue, ByRef FarbR1 As RefValue, ByRef FarbRN As RefValue, ByRef Ier As Integer)
    Dim NWE As Integer
    Dim KM As Integer
    Dim NGK As Integer
    Dim i, kw, k As Integer
    Dim R2(,,) As Single
    Dim R1(,,) As Single
    Dim RN(,,) As Single
    Dim RR(,,) As Single
    Dim IGLZ() As Integer
    Dim GKINT() As Single
    Dim GK(KM - 1, 15) As Single
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    ngk = 16
    ReDim IGLZ(KM - 1)
    ReDim GKINT(KM - 1)
    '
    '
    For kw = 0 To KM - 1
      For i = 0 To ngk - 1
        GK(kw, i) = Winkel(kw).GK(i)
      Next
    Next kw
    
    '
    '
    ReDim R2(0, KM - 1, NWE - 1)
    ReDim R1(0, KM - 1, NWE - 1)
    ReDim RN(0, KM - 1, NWE - 1)
    ReDim RR(0, KM - 1, NWE - 1)
    Call GetRwerte(Winkel, 0, R2, FarbR2)
    Call GetRwerte(Winkel, 0, R1, FarbR1)
    Call GetRwerte(Winkel, 0, RN, FarbRN)
    '

    Call RwertAKA(Aka, NGK, GK(0, 0), KM, NWE, RR(k, 0, 0), R2(k, 0, 0), R1(k, 0, 0), RN(k, 0, 0), FEHL)
    Call LetRwerte(Winkel, 0, RR, FarbRR)
    '
    '

  End Sub
  Sub GrundCorr(ByRef FarbRR As Colorant, ByRef FarbR2 As Colorant, ByRef FarbR1 As Colorant, ByRef FarbRN As Colorant, ByRef Ier As Integer)
    Dim i, kw, k As Integer
    '
    For k = 0 To FarbRR.OptData.Grund.Count - 1
      For kw = 0 To FarbRR.OptData.Grund(k).Count - 1
        For i = 0 To FarbRR.OptData.Grund(k)(kw).Nwe - 1
          FarbRR.OptData.Grund(k)(kw).R(i) = FarbRN.OptData.Grund(k)(kw).R(i) - FarbR1.OptData.Grund(k)(kw).R(i) + FarbR2.OptData.Grund(k)(kw).R(i)
        Next i
      Next kw
    Next k
    '
    '
    '
  End Sub
  Sub FarbWrtCalc(ByRef Winkel As AngGeos, ByRef GrpRwerte As RefValuesGrp, ByRef FarbWrt As ValuesGrpsAssigns, ByRef Ier As Integer)
    Dim Ianwsg As Integer
    Dim AufgArt As String
    Dim i As Integer
    Dim l As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim RTyp(,,) As Single
    Dim RPrb(,,) As Single
    Dim Retr(1) As Integer
    Dim FaWrtV(,,,) As Single
    Dim FaWrtN(,,,) As Single
    Dim CountAkt As Integer
    Dim NUN As Integer
    Dim NUV As Integer
    Dim NUU As Integer
    Dim KWW As Integer
    Dim NWE As Integer
    Dim KM As Integer
    Dim Nlz As Integer
    Ier = 0
    NUN = 0
    NUV = 0
    KWW = 1
    '
    '
    Ier = 0
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    Nlz = MenueParam.Normfa.Nlz
    Retr(1) = 0
    Retr(0) = GrpRwerte(0)(0).ReTr
    If GrpRwerte.Count > 1 Then
      If GrpRwerte(1).Count > 0 Then
        Retr(1) = GrpRwerte(1)(0).ReTr
      End If
    End If
    '
    ReDim RTyp(1, KM - 1, NWE - 1)
    ReDim FaWrtV(1, KM - 1, Nlz - 1, 20)
    ReDim RPrb(1, KM - 1, NWE - 1)
    ReDim FaWrtN(1, KM - 1, Nlz - 1, 20)
    '
    'ier=1 keine Messung über weiß
    'ier=2 Anzahl Messungen über schwarz<> Anz. über weiß (falls Messungen über schwarz vorhanden)
    'ier=3 erste Messung ist kein Standard
    '
    'FarbWerte für FAWERT (CIELAB)
    '
    'FaWrt(0-2)  XYZ
    'FaWrt(3-4)  xy
    'FaWrt(5-7)  LCh
    'FaWrt(8-9)  a,b
    'FaWrt(10)   DE
    'FaWrt(11)   DL
    'FaWrt(12)   DC
    'FaWrt(13)   DH
    'FaWrt(14)   Da
    'FaWrt(15)   Db
    'FaWrt(16)   META
    'FaWrt(17)   DeWS
    'FaWrt(18)   frei
    'FaWrt(19)   frei
    'FaWrt(20)   frei
    '
    '
    '
    '
    If GrpRwerte.Count = 0 Then
      '
      'keine Messungen über weiß; keine über schwarz
      '
      Ier = 10
      Exit Sub
    End If

    '
    ' NormFarbWerte unf GK-Werte übernehmen
    '
    '
    Call GetWinkNormGk("FWR", KWW, Winkel, Ier)

    If Ier <> 0 Then
      Exit Sub
    End If
    '

    '
    'Menüparameter  übernehmen
    '
    '
    Call GetMenueParam("FWR", Ier)
    If Ier <> 0 Then
      Exit Sub
    End If '
    '
    '
    '
    '
    'Prüfen,ob weiß (allein) oder weiß und schwarz
    '
    '
    '
    '
    If GrpRwerte(0).Count = 0 Then
      '
      'keine Messungen über weiß
      '
      Ier = 1
      Exit Sub
    End If
    If GrpRwerte(0).Count > 0 Then
      '
      'Messung über weiß steht nicht zur Verfügung (IVONA=false)
      '
      If GrpRwerte(0)(0).IVoNa = False Then
        Ier = 2
        Exit Sub
      End If
    End If
    If GrpRwerte.Count = 1 Then
      '
      'Keine Messungen über schwarz
      '
      '
      '
      CountAkt = 1



    ElseIf GrpRwerte.Count > 1 Then
      If GrpRwerte(1).Count = 0 Then
        '
        'Keine Messungen über schwarz
        '
        '
        '
        '
        CountAkt = 1


      ElseIf GrpRwerte(1).Count = GrpRwerte(0).Count Then
        '
        '
        'Anzahl Messungen über schwarz=Anzahl über weiß
        '
        '
        '
        If GrpRwerte(1)(0).IVoNa = True AndAlso GrpRwerte(1)(1).IVoNa = True Then
          '
          '
          '
          '
          'R-Werte für Typ und Probe über schwarz verfügbar
          '
          '
          '
          CountAkt = 2

        Else
          '
          '
          'Nicht verfügbar
          '
          '
          '
          CountAkt = 1


        End If
      Else
        Ier = 2
        Exit Sub
      End If
    End If
    '
    '
    '
    '
    '
    'Anweisungen definieren
    '
    'Schleife über alle R-Werte
    '
    For Ianwsg = 0 To FarbWrt.Count - 1
      FarbWrt(Ianwsg).clear()
    Next
    '
    '
    For i = 0 To GrpRwerte(0).Count - 1

      '
      'Anweisung für weiß bzw.schwarz
      '
      '
      For Ianwsg = 0 To CountAkt - 1
        If i = 0 Then
          AufgArt = FarbWrt(Ianwsg).AufgArt
          If AufgArt.Substring(3, 1) = "$" Then
            If GrpRwerte(Ianwsg)(i).kwb = 0 Then
              Mid(AufgArt, 4, 1) = "W"
            ElseIf GrpRwerte(Ianwsg)(i).kwb = 1 Then
              Mid(AufgArt, 4, 1) = "S"
            End If
          End If
          If AufgArt.Substring(9, 1) = "$" Then
            If GrpRwerte(Ianwsg)(i).kwb = 0 Then
              Mid(AufgArt, 10, 1) = "W"
            ElseIf GrpRwerte(Ianwsg)(i).kwb = 1 Then
              Mid(AufgArt, 10, 1) = "S"
            End If
          End If
          FarbWrt(Ianwsg).AufgArt = AufgArt
        End If
        FarbWrt(Ianwsg).Add(GrpRwerte(Ianwsg).RwKey(i), New ValuesGrps)
        FarbWrt(Ianwsg)(i).Itp = False
        FarbWrt(Ianwsg)(i).Nr = GrpRwerte(Ianwsg)(i).Nr
        FarbWrt(Ianwsg)(i).Name = GrpRwerte(Ianwsg)(i).Name
        FarbWrt(Ianwsg)(i).Dattim = GrpRwerte(Ianwsg)(i).DatTim
        For k = 0 To MenueParam.Normfa.Nlz - 1
          FarbWrt(Ianwsg)(i).Add(KeyName(MenueParam.Normfa(k).LichtID), New ValuesGrp)
          For kw = 0 To Winkel.Km - 1
            FarbWrt(Ianwsg)(i)(k).Add(Winkel(kw).Chrm, New Values)
            For l = 0 To FarbWrt(Ianwsg).CountMerk - 1
              If FarbWrt(Ianwsg).Merk(l).Typ = "T" Then
                FarbWrt(Ianwsg)(i)(k)(kw).Add(FarbWrt(Ianwsg).Merk(l).Ken, New Object)
                FarbWrt(Ianwsg)(i)(k)(kw)(FarbWrt(Ianwsg).Merk(l).Ken) = ""
              Else
                FarbWrt(Ianwsg)(i)(k)(kw).Add(FarbWrt(Ianwsg).Merk(l).Ken, New Object)
                FarbWrt(Ianwsg)(i)(k)(kw)(FarbWrt(Ianwsg).Merk(l).Ken) = Double.NaN
              End If
            Next l
          Next kw
        Next k
      Next Ianwsg
      '
      '
      'R-Werte übernehmen
      '
      '
      '
      If GrpRwerte(0)(i).Itp And CountAkt > 0 And GrpRwerte(0)(i).IVoNa = True Then
        '
        'Bezug über weiß
        '
        '
        ' Sub GetRwerte(ByRef Nwe As Short, ByRef km As Short, ByRef IU As Integer, ByRef Rmit(,,) As Single, ByRef Rwert As RefValue)

        Call GetRwerte(Winkel, 0, RTyp, GrpRwerte(0)(i))
        NUV = 1
        If CountAkt > 1 Then
          If GrpRwerte(1)(i).Itp And GrpRwerte(1)(i).IVoNa = True Then
            '
            'Bezug über schwarz
            '
            '
            Call GetRwerte(Winkel, 1, RTyp, GrpRwerte(1)(i))
            NUV = 2
          End If
        End If
        Call REZFAWRT(NUV, NUN, NUU, NWE, KM, Retr(0), RTyp(0, 0, 0), RPrb(0, 0, 0), _
               Nlz, FaWrtV(0, 0, 0, 0), FaWrtN(0, 0, 0, 0), FEHL)
        If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
          Exit Sub
        End If
      Else
        '
        'Probe über weiß
        '
        '
        If GrpRwerte(0)(i).IVoNa = True Then
          Call GetRwerte(Winkel, 0, RPrb, GrpRwerte(0)(i))
          NUN = 1
          If GrpRwerte.Count > 1 Then
            If GrpRwerte(1).Count > 1 Then
              If Not GrpRwerte(1)(i).Itp AndAlso GrpRwerte(1)(i).IVoNa Then
                '
                'Probe über schwarz
                '
                '
                Call GetRwerte(Winkel, 1, RPrb, GrpRwerte(1)(i))
                NUN = 2
              End If
            End If
          End If
        End If

        Call REZFAWRT(NUV, NUN, NUU, NWE, KM, Retr(0), RTyp(0, 0, 0), RPrb(0, 0, 0), _
               Nlz, FaWrtV(0, 0, 0, 0), FaWrtN(0, 0, 0, 0), FEHL)
        If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
          Exit Sub
        End If
      End If

      '
      'Anweisung für weiß bzw.schwarz
      '
      '
      For Ianwsg = 0 To CountAkt - 1
        For k = 0 To MenueParam.Normfa.Nlz - 1
          For kw = 0 To Winkel.Km - 1
            If GrpRwerte(0)(i).Itp Then
              '
              'Vorlage (Bezug)
              '
              '
              '
              '
              '
              If NUV > 0 Then

                FarbWrt(Ianwsg)(i).Itp = True

                For l = 0 To FarbWrt(Ianwsg)(i)(k)(kw).Count - 1
                  Select Case FarbWrt(Ianwsg).Merk(l).Ken
                    Case "@P"
                      'Preis
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(0.0#, FarbWrt(Ianwsg).Merk(l).Form)
                    Case "@Q"
                      'Flächendifferenz
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(0.0#, FarbWrt(Ianwsg).Merk(l).Form)
                    Case "@R"
                      'Korrigierbarkeit
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(0.0#, FarbWrt(Ianwsg).Merk(l).Form)
                    Case "@S"
                      'Sensitivität
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(0.0#, FarbWrt(Ianwsg).Merk(l).Form)
                    Case "AA"
                      'X
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtV(Ianwsg, kw, k, 0), FarbWrt(Ianwsg).Merk(l).Form)
                    Case "AB"
                      'Y
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtV(Ianwsg, kw, k, 1), FarbWrt(Ianwsg).Merk(l).Form)
                    Case "AC"
                      'Z
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtV(Ianwsg, kw, k, 2), FarbWrt(Ianwsg).Merk(l).Form)
                    Case "AD"
                      'x
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtV(Ianwsg, kw, k, 3), FarbWrt(Ianwsg).Merk(l).Form)
                    Case "AE"
                      'y
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtV(Ianwsg, kw, k, 4), FarbWrt(Ianwsg).Merk(l).Form)
                    Case "BA", "EA"
                      'L*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtV(Ianwsg, kw, k, 5), FarbWrt(Ianwsg).Merk(l).Form)
                    Case "BB", "EB"
                      'C*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtV(Ianwsg, kw, k, 6), FarbWrt(Ianwsg).Merk(l).Form)
                    Case "BC", "EC"
                      'h
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtV(Ianwsg, kw, k, 7), FarbWrt(Ianwsg).Merk(l).Form)
                    Case "BD", "ED"
                      'a*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtV(Ianwsg, kw, k, 8), FarbWrt(Ianwsg).Merk(l).Form)
                    Case "BE", "EE"
                      'b*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtV(Ianwsg, kw, k, 9), FarbWrt(Ianwsg).Merk(l).Form)
                  End Select
                Next l
              Else
                FarbWrt(Ianwsg)(i)(k)(kw).clear()
              End If
            Else
              '
              '
              'Nachstellung (Probe)
              '
              '
              '
              '
              If NUN > 0 Then
                For l = 0 To FarbWrt(Ianwsg)(i)(k)(kw).Count - 1
                  Select Case FarbWrt(Ianwsg).Merk(l).Ken
                    Case MenueParam.Menue.StdMrkKen(0)
                      'Preis
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(0.0#, FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(1)
                      'Flächendifferenz
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(0.0#, FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(2)
                      'Korrigierbarkeit
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(0.0#, FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(3)
                      'Sensitivität
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(0.0#, FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(4)
                      'X
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 0), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(5)
                      'Y
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 1), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(6)
                      'Z
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 2), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(7)
                      'x
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 3), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(8)
                      'y
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 4), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(9)
                      'L*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 5), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(10)
                      'C*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 6), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(11)
                      'h
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 7), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(12)
                      'a*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 8), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(13)
                      'b*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 9), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(14)
                      'META
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 16), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(15)
                      'DE*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 10), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(16)
                      'DL*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 11), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(17)
                      'DC*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 12), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(18)
                      'DH*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 13), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(19)
                      'Da*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 14), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(20)
                      'Db*
                      FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 15), FarbWrt(Ianwsg).Merk(l).Form)
                    Case MenueParam.Menue.StdMrkKen(21)
                      'K-DE*
                      'If Ianwsg = 2 Then
                      'MsgBox TyRwPrb(Ianwsg).St
                      If FaWrtN(Ianwsg, kw, k, 17) < HUGE() Then
                        FarbWrt(Ianwsg)(i)(k)(kw)(l) = Format(FaWrtN(Ianwsg, kw, k, 17), FarbWrt(Ianwsg).Merk(l).Form)
                      Else
                        FarbWrt(Ianwsg)(i)(k)(kw)(l) = "  "
                      End If

                      'End If
                  End Select
                Next l
              Else
                FarbWrt(Ianwsg)(i)(k)(kw).clear()
              End If
            End If
          Next kw
        Next k
      Next Ianwsg
    Next i
    Erase RTyp
    Erase FaWrtV
    Erase RPrb
    Erase FaWrtN
  End Sub


  Sub CalcQualWerte(ByRef Winkel As AngGeos, ByRef GrpRwerte As RefValuesGrp, ByRef ParamAlle As ValuesGrpsAssigns, ByRef Ier As Integer)
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
    Dim lw As Integer
    '
    '

    '
    '
    Dim Iprn As Integer
    Dim Npam As Integer
    Dim IWESC As Integer
    '
    Dim Nqu As Integer
    Dim Nqum As Integer
    Dim Nqub As Integer
    Dim nquh As Integer
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
    '
    '
    '
    '
    '
    '
    '
    Dim TyRwert() As TyRwert        'R-Werte Messung/Vorlage
    Dim Qurwert() As TyQual        'Zusätze für Qualität
    Dim RwertName() As String
    Dim RwertBem() As String
    Dim TyRunnd() As TyRwert        'R-Werte unendlich dick
    Dim QuRunnd() As TyQual        'Zusätze für Qualität
    Dim RunndName() As String
    Dim TyRcrom() As TyRwert        'R-Werte max Chroma
    Dim QuRcrom() As TyQual        'Zusätze für Qualität
    Dim RcromName() As String
    Dim TyRwerb() As TyRwert        'R-Werte Rechnung
    Dim QuRwerb() As TyQual
    Dim RwerbName() As String
    Dim TyRwerp() As TyRwert        'R-Werte Rechnung oder Messung (umgespeichert)
    Dim QuRwerp() As TyQual
    Dim RwerpName() As String
    Dim TyGrund() As TyRwert        'K/S-Werte;Extinktions-Werte;Abs-Werte;Streuwerte
    Dim QuGrund() As TyQual
    Dim GrundName() As String
    Dim TyUntGrd() As TyRwert
    Dim QuUntgrd() As TyQual
    Dim UntgrdName() As String
    Dim Rwert(,,) As Single
    Dim Runnd(,,) As Single
    Dim Rcrom(,,) As Single
    Dim Rwerb(,,) As Single
    Dim Twerb(,,) As Single
    Dim Rwerp(,,) As Single
    Dim Grund(,,) As Single
    Dim UntGrd(,,) As Single
    Dim TyUnt() As TyRwert
    Dim QuUnt() As TyQual
    Dim Runt(,,) As Single
    Dim Tunt(,,) As Single
    Dim tuu As Single
    Dim Param() As TyWert
    Dim Wert(,) As Double
    Dim FaeChar(,,) As Single

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
    'Dim AusWhl As TyAuswhl
    '
    '
    'Untergründe
    '
    '
    FEHL.Ifeh = 0
    FEHL.Iwarn = 0
    '
    '
    '
    '
    'MsgBox ParamAlle(0).AufgArt
    '
    'berechnete R-Werte (weiss)
    GrpRwerte("H").clear()
    'berechnete R-Werte(schwarz)
    GrpRwerte("F").clear()
    'R-Werte (unendlich dick)
    GrpRwerte("U").clear()
    'R-Werte max. Chroma)
    GrpRwerte("C").clear()
    'Extinktionswerte
    GrpRwerte("E").clear()
    'K/S - Werte
    GrpRwerte("K").clear()
    'Absorptionswerte
    GrpRwerte("A").clear()
    'Streuwerte
    GrpRwerte("D").clear()
    'Transmission
    GrpRwerte("T").clear()

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
    '
   
    KM = Winkel.Km
    '
    Nqu = GrpRwerte("W").Count + GrpRwerte("S").Count
    Nlz = MenueParam.Normfa.Nlz
    NWE = Winkel.Wsol.Nwe
    '
    ReDim TyUnt(2)
    ReDim QuUnt(2)
    ReDim Runt(2, KM - 1, NWE - 1)
    ReDim Tunt(2, KM - 1, NWE - 1)
    Call StartUnt(MenueParam.Messg, Winkel, TyUnt, QuUnt, Runt)
    tuu = MenueParam.Messg.Winkel(0).GK(15)
    For i = 0 To 2
      If Not IsNothing(GrpRwerte(i).RefUnt) AndAlso GrpRwerte(i).RefUnt.IVoNa Then
        Call LetQurwert(GrpRwerte(i).RefUnt, QuUnt(i))
      End If
      For kw = 0 To KM - 1
        For j = 0 To NWE - 1
          Tunt(0, kw, j) = tuu
          Tunt(1, kw, j) = 0
        Next
      Next
    Next


    '
    'Untergründe übernehmen
    '
    '
    If Not IsNothing(GrpRwerte("W").RefUnt) AndAlso GrpRwerte("W").RefUnt.ID < 0 Then
      GrpRwerte("W").RefUnt.RefKurv.clear()
      Call LetNamBemDat(GrpRwerte("W").RefUnt, GrpRwerte("W").RefUnt, TyUnt(0), Winkel, Iee)
      Call LetTyrwert(Winkel, GrpRwerte("W").RefUnt, TyUnt(0), 0, Runt)
      Call LetQurwert(GrpRwerte("W").RefUnt, QuUnt(0))
    End If
    If Not IsNothing(GrpRwerte("S").RefUnt) AndAlso GrpRwerte("S").RefUnt.ID < 0 Then
      GrpRwerte("S").RefUnt.RefKurv.clear()
      Call LetNamBemDat(GrpRwerte("S").RefUnt, GrpRwerte("S").RefUnt, TyUnt(1), Winkel, Iee)
      Call LetTyrwert(Winkel, GrpRwerte("S").RefUnt, TyUnt(1), 1, Runt)
      Call LetQurwert(GrpRwerte("S").RefUnt, QuUnt(1))
    End If
    J2 = 1
    If Nqu = 2 * NLQ Then
      J2 = 2
    End If
    '
    '
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
      ParamAlle(i).Clearwinkel()
      For kw = 0 To KM - 1
        ParamAlle(i).AddWinkel(Winkel(kw).Chrm, New AnglExist)
      Next
      Nlauf = Nlaaf(J2, ParamAlle(i).AufgArt)
      For j = 0 To Nlauf * NLQ - 1
        StrRw = KeyRe(j)
        ParamAlle(i).Add(StrRw, New ValuesGrps)
        For k = 0 To Nlz - 1
          ParamAlle(i)(j).Add(KeyRe(MenueParam.Normfa(k).LichtID), New ValuesGrp)
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
    '
    'NormFarbWerte,Winkel und GK-Werte übernehmen
    '
    '
    '
    '
    '
    Call GetWinkNormGk("QUA", MenueParam.Menue.Kwj, Winkel, Iee)
    '
    If Iee <> 0 Then
      Ier = Iee
      Exit Sub
    End If
    '
    'Start Qualitätskontrolle
    '
    '
    '
    '
    Call QUABEG(NWE, KM, FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      Exit Sub
    End If
    '
    '
    '
    'Menüparameter übernehmen
    '
    '
    Call GetMenueParam("QUA", Iee)
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
    ReDim RwertBem(Nqu - 1)
    ReDim Rwert(Nqu - 1, KM - 1, NWE - 1)
    '
    '

    '

    '
    '
    'Programme zur Verarbeitung aufrufen
    '

    Select Case MenueParam.MethID
      Case 1

        '
        'Gruppe Farbabstände
        '
        '
      Case 2, 3, 17, 18, 19, 24, 25, 26, 27
        Npam = Nqu

        '
        'Gruppe Farbstärke
        '
        '
      Case 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 22, 23
        Npam = Nqu
        Nqub = Nqu
        ReDim TyRwerb(Nqu - 1)
        ReDim QuRwerb(Nqu - 1)
        ReDim RwerbName(Nqu - 1)
        ReDim Rwerb(Nqu - 1, KM - 1, NWE - 1)
        ReDim Twerb(Nqu - 1, KM - 1, NWE - 1)
        ReDim TyGrund(Nqu - 1)
        ReDim QuGrund(Nqu - 1)
        ReDim GrundName(Nqu - 1)
        ReDim Grund(Nqu - 1, KM - 1, NWE - 1)
        If MenueParam.MethID = 7 Then
          '
          '
          'Absorption und Streuung für Weiß- und Schwarzpigment
          '
          '
          ReDim TyUntGrd(4)
          ReDim QuUntgrd(4)
          ReDim UntgrdName(4)
          ReDim UntGrd(4, KM - 1, NWE - 1)
        End If
        If MenueParam.MethID = 7 Or MenueParam.MethID = 8 Then
          '
          '
          'R-Werte unendlich dick und R-Werte max. Chroma
          '
          ReDim TyRunnd(Nqu / 2 - 1)
          ReDim QuRunnd(Nqu / 2 - 1)
          ReDim RunndName(Nqu / 2 - 1)
          ReDim Runnd(Nqu / 2 - 1, KM - 1, NWE - 1)
          ReDim TyRcrom(Nqu / 2 - 1)
          ReDim QuRcrom(Nqu / 2 - 1)
          ReDim RcromName(Nqu / 2 - 1)
          ReDim Rcrom(Nqu / 2 - 1, KM - 1, NWE - 1)

        End If
        ReDim Parab(Nqu - 1)
        ReDim Parac(Nqu - 1)
        ReDim Wertb(Nqu - 1, 63)
        ReDim Wertc(Nqu - 1, 63)
        '
        '
        '
        '
        'Gruppe Deckvermögen
        '
        '
      Case 15, 16
        If MenueParam.MethID = 15 Then
          '
          '
          'Absorption und Streuung für Weiß- und Schwarzpigment
          '
          '
          ReDim TyUntGrd(4)
          ReDim QuUntgrd(4)
          ReDim UntgrdName(4)
          ReDim UntGrd(4, KM - 1, NWE - 1)
        End If
        Npam = Nqu
        '
        Nqub = Nqu
        ReDim TyRwerb(Nqu - 1)
        ReDim QuRwerb(Nqu - 1)
        ReDim RwerbName(Nqu - 1)
        ReDim Rwerb(Nqu - 1, KM - 1, NWE - 1)
        ReDim Twerb(Nqu - 1, KM - 1, NWE - 1)
        ReDim TyGrund(Nqu - 1)
        ReDim QuGrund(Nqu - 1)
        ReDim GrundName(Nqu - 1)
        ReDim Grund(Nqu - 1, KM - 1, NWE - 1)
        ReDim TyRunnd(Nqu / 2 - 1)
        ReDim QuRunnd(Nqu / 2 - 1)
        ReDim RunndName(Nqu / 2 - 1)
        ReDim Runnd(Nqu / 2 - 1, KM - 1, NWE - 1)
        ReDim TyRcrom(Nqu / 2 - 1)
        ReDim QuRcrom(Nqu / 2 - 1)
        ReDim RcromName(Nqu / 2 - 1)
        ReDim Rcrom(Nqu / 2 - 1, KM - 1, NWE - 1)

        ReDim Parab(Npam - 1)
        ReDim Wertb(Npam - 1, 63)
      Case 20
        Npam = Nqu
        ReDim TyRunnd(Nqu / 2 - 1)
        ReDim QuRunnd(Nqu / 2 - 1)
        ReDim RunndName(Nqu / 2 - 1)
        ReDim Runnd(Nqu / 2 - 1, KM - 1, NWE - 1)
    End Select
    '
    '
    '
    'R-Werte umspeichern nach Tyrwert
    '
    '
    '
    '
    j = 0
    For i = 0 To NLQ - 1
      If GrpRwerte("W").Count > 0 Then
        Call GetTyrwert(Winkel, GrpRwerte("W")(i), TyRwert(j), j, Rwert)
        j = j + 1
      End If
      If GrpRwerte("S").Count > 0 Then
        Call GetTyrwert(Winkel, GrpRwerte("S")(i), TyRwert(j), j, Rwert)
        j = j + 1
      End If
    Next i
    j = 0
    For i = 0 To NLQ - 1
      If GrpRwerte("W").Count > 0 Then
        Call GetQurwert(GrpRwerte("W")(i), Qurwert(j), RwertName(j), RwertBem(j))
        j = j + 1
      End If
      If GrpRwerte("S").Count > 0 Then
        Call GetQurwert(GrpRwerte("S")(i), Qurwert(j), RwertName(j), RwertBem(j))
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
      Call GetTyrwert(Winkel, GrpRwerte("W").RefUnt, TyUnt(j), j, Runt)
      j = j + 1
    End If
    If Not IsNothing(GrpRwerte("S").RefUnt) AndAlso GrpRwerte("S").RefUnt.IVoNa = True Then
      Call GetTyrwert(Winkel, GrpRwerte("S").RefUnt, TyUnt(j), j, Runt)
      j = j + 1
    End If
    If Not IsNothing(GrpRwerte("G").RefUnt) AndAlso GrpRwerte("G").RefUnt.IVoNa = True Then
      Call GetTyrwert(Winkel, GrpRwerte("G").RefUnt, TyUnt(j), j, Runt)
      j = j + 1
    End If
    Nu = j

    '
    '
    '
    'Merkmale für Farbstärke,Deckvermögen usw. berechnen
    '
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
      Select Case MenueParam.MethID
        Case 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 22, 23
          '
          '
          '
          'Suche nach "günstigstem" B-Wert
          '
          '
          If MenueParam.Menue.BwertID = 0 Then
            '
            'B-Wert
            '
            MenueParam.Menue.BwertID = BWEBER(kwf, NWE, KM, Nqu, Qurwert(0), TyRwert(0), Rwert(0, 0, 0), FEHL)
            If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
              Exit Sub
            End If


          End If
          If MenueParam.Menue.BwertID <= 0 Then
            MenueParam.Menue.BwertID = 1
          End If
      End Select
      '
      '*****************
      '*****************
      '*** DLL-Aufrufe, falls R-Werte berechnet werden **
      '*****************
      '*****************

      Select Case MenueParam.MethID
        '
        '
        '
        Case 4
          '
          '
          '    Farbstärke (deckend)
          '
          '
          '     MsgBox ParamAlle(2).AufgArt
          '
          Call FASDEK(kwf, NWE, KM, Nqu, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)
          '
          Nqum = Nqu
          Nqub = Nqu
          '
          '
          '
          '
          '
        Case 23
          '
          '
          '    Farbstärke (deckend) mit Purton als zweiter Messung
          '
          '
          '
          '
          '
          'Rwerte umspeichern
          '
          '
          Nqum = Nqu
          Nqub = Nqu / 2
          ReDim TyRwerp(Nqub - 1)
          ReDim QuRwerp(Nqub - 1)
          ReDim RwerpName(Nqub - 1)
          ReDim Rwerp(Nqub - 1, KM - 1, NWE - 1)
          For l = 0 To Nqub - 1
            TyRwerp(l) = TyRwert(2 * l)
            QuRwerp(l) = Qurwert(2 * l)
            For lw = 0 To KM - 1
              For i = 0 To NWE - 1
                Rwerp(l, lw, i) = Rwert(2 * l, lw, i)
              Next i
            Next lw
          Next l
          Call FASDEK(kwf, NWE, KM, Nqub, _
          QuRwerp(0), TyRwerp(0), Rwerp(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)
          '
          J2 = 1
          Nqum = Nqu
          '
          '
          '
          '
          '
          '
          '
        Case 5
          '
          '
          '    Farbstärke (transparent)
          '
          '
          '

          Call FASTRA(kwf, NWE, KM, Nqu, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)
          '
          Nqum = Nqu
          Nqub = Nqu
          '
          '
        Case 6
          '
          '
          '    Farbstärke (Textil)
          '
          '
          '
          Call FASTEX(kwf, NWE, KM, Nqu, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)
          '
          '
          Nqum = Nqu
          Nqub = Nqu
          '
          '
        Case 7
          '
          '
          '    Farbstärke (deckend) Weiss-und Schwarzverschnitt
          '
          If Nqu Mod 2 <> 0 Then
            Ier = 2605
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          '
          Call GetMenueParam("QUA", Iee)
          Call FSTDEK(kwf, NWE, KM, Nqu, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuRunnd(0), TyRunnd(0), Runnd(0, 0, 0), QuRcrom(0), TyRcrom(0), Rcrom(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          QuUntgrd(0), TyUntGrd(0), UntGrd(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)
          '
          '
          '
          '
          Nqum = Nqu
          Nqub = Nqu
          '
        Case 8
          '
          '
          '    Farbstärke (transparent) über weißem und schwarzem Untergrund
          '
          If Nqub Mod 2 <> 0 Then
            Ier = 2605
            MsgBox(Texxt(Ier))
            Exit Sub
          End If

          Call GetMenueParam("QUA", Iee)
          Call FSTTRA(kwf, NWE, KM, Nqub, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuRunnd(0), TyRunnd(0), Runnd(0, 0, 0), QuRcrom(0), TyRcrom(0), Rcrom(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)
          '
          Call RETRMIS(1, 1, kwf, NWE, KM, Nqu, QuUnt(0), TyUnt(0), Tunt(0, 0, 0), Qurwert(0), TyRwert(0), Twerb(0, 0, 0), QuGrund(0), TyGrund(0), Grund(0, 0, 0), FEHL)

          '
          '
          '    Nqum = Nqu
          '    Nqub = Nqu
          '
        Case 9
          '
          '    Farbstärke (deckend) und Rub-Out
          '
          '
          If Nqub Mod 2 <> 0 Then
            Ier = 2605
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          '
          '
          '
          Call FASDEK(kwf, NWE, KM, Nqub, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)
          '
          '
          '
          Nqum = Nqu
          Nqub = Nqu
          '
        Case 10
          '
          '    Farbstärke (deckend) und Disp-Härte
          '
          '
          If Nqub Mod 2 <> 0 Then
            Ier = 2605
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          '
          Call FASDEK(kwf, NWE, KM, Nqub, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)

          '
          '
          '
          Nqum = Nqu
          Nqub = Nqu
          '
        Case 11
          '
          '    Farbstärke (deckend) und Rub-Out + Disp.-Härte
          '
          '
          If Nqub Mod 3 <> 0 Then
            Ier = 2605
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          Call FASDEK(kwf, NWE, KM, Nqub, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)

          '
          '
          '
          Nqum = Nqu
          Nqub = Nqu
        Case 12
          '
          '   FOGRA Weißverschnitt
          '
          '
          '
          '   FOGRA-Auswertung
          '
          '
          Iprn = 1
          MsgBox("Method not available")
          Exit Sub
          '    Call FASFOG(IprnL, kwL, nquL, _
          '    Qurwert(0), TyRwert(0), _
          '    nquhL, QuRwerb(0), TyRwerb(0), _
          '    Menal, Normx.norm, NpamL, Parab(0), FEHL)
          If FEHL.Ifeh = 0 Then

            '
            '
            '      Call FASNEU(IprnL, kwL, nquhL, _
            '      QuRwerb(0), TyRwerb(0), _
            '      Menal, Normx.norm, nquhL, Parac(0), FEHL)
            Nqum = Nqu
            Nqub = nquh
          End If
          '
          '
          '
          '
          '
        Case 13
          '
          '   FOGRA Druckfarben
          '
          '
          '   FOGRA-Auswertung
          '
          '
          '
          '
          Iprn = 2
          MsgBox("Method not available")
          Exit Sub
          'Call FASFOG(IprnL, kwL, nquL, _
          'Qurwert(0), TyRwert(0), _
          'nquhL, QuRwerb(0), TyRwerb(0), _
          'Menal, Normx.norm, NpamL, Parab(0), FEHL)
          If FEHL.Ifeh = 0 Then

            '
            '
            '      Call FASNEU(IprnL, kwL, nquhL, _
            '      QuRwerb(0), TyRwerb(0), _
            ' Menal, Normx.norm, nquhL, Parac(0), FEHL)
            ' MsgBox TyRwerb(0).Nr
            Nqum = Nqu
            Nqub = nquh
          End If
        Case 14
          '
          '   Forgra Textil
          '
          '
          '
          '   FOGRA-Auswertung
          '
          '
          Iprn = 3
          MsgBox("Method not available")
          Exit Sub
          '    Call FASFOG(IprnL, kwL, nquL, _
          '    Qurwert(0), TyRwert(0), _
          '    nquhL, QuRwerb(0), TyRwerb(0), _
          '    Menal, Normx.norm, NpamL, Parab(0), FEHL)
          If FEHL.Ifeh = 0 Then

            '
            '
            '      Call FASNEU(IprnL, kwL, nquhL, _
            '      QuRwerb(0), TyRwerb(0), _
            '      Menal, Normx.norm, nquhL, Parac(0), FEHL)
            ' MsgBox TyRwerb(0).Nr
            Nqum = Nqu
            Nqub = nquh
          End If
          '
          '
          '
        Case 15
          '
          '   Deckvermögen (deckend)
          '
          If Nqu Mod 2 <> 0 Then
            Ier = 2605
            MsgBox(Texxt(Ier))
            Exit Sub
          End If


          Call GetMenueParam("QUA", Iee)
          ReDim FaeChar(Nqu - 1, 13, 127)
          Call DEKDEK(kwf, NWE, KM, Nqu, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuRunnd(0), TyRunnd(0), Runnd(0, 0, 0), QuRcrom(0), TyRcrom(0), Rcrom(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          QuUntgrd(0), TyUntGrd(0), UntGrd(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)

          '
          '
        Case 16
          '
          '
          'Deckvermögen (transparent)
          '
          If Nqu Mod 2 <> 0 Then
            Ier = 2605
            MsgBox(Texxt(Ier))
            Exit Sub
          End If

          '
          '
          '
          Call GetMenueParam("QUA", Iee)
          ReDim FaeChar(Nqu - 1, 13, 127)
          Call DEKTRA(kwf, NWE, KM, Nqu, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuRunnd(0), TyRunnd(0), Runnd(0, 0, 0), QuRcrom(0), TyRcrom(0), Rcrom(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)

          '
          '
          'für Transmission
          Call RETRMIS(1, 1, kwf, NWE, KM, Nqu, QuUnt(0), TyUnt(0), Tunt(0, 0, 0), Qurwert(0), TyRwert(0), Twerb(0, 0, 0), QuGrund(0), TyGrund(0), Grund(0, 0, 0), FEHL)
          'für Remission
          'Call RETRMIS(1, 0, kwf, NWE, KM, Nqu, QuUnt(0), TyUnt(0), Runt(0, 0, 0), Qurwert(0), TyRwert(0), Twerb(0, 0, 0), QuGrund(0), TyGrund(0), Grund(0, 0, 0), FEHL)

          '
          '
          Nqum = Nqu
          Nqub = Nqu
          '
          '
        Case 20
          Call UNENDL(kwf, NWE, KM, Nqu, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), _
          QuRunnd(0), TyRunnd(0), Runnd(0, 0, 0), _
          QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          FEHL)
          '     Namen Für Grund
          '
          '
        Case 22

          '
          '
          '    Diepergierung (bis zu 6 Dispergierstufen)
          '
          '
          Call FASDEK(kwf, NWE, KM, Nqub, _
          Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
          QuGrund(0), TyGrund(0), Grund(0, 0, 0), QuUnt(0), TyUnt(0), Runt(0, 0, 0), _
          Npam, Parab(0), Wertb(0, 0), FEHL)
          '
          '
          '
          Nqum = Nqu
          Nqub = Nqu
          '
      End Select
      Ier = FEHL.Ifeh
      If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
        For Ianwsg = 0 To ParamAlle.Count - 1
          ParamAlle(Ianwsg).clear()
        Next
        Exit Sub
      End If
      '
      '
      '  Berechnete Merkmale übernehmen
      '
      '
      '
      Select Case MenueParam.MethID
        Case 4, 5, 6, 7, 8, 9, 10, 11, 15, 16, 22, 23
          For Ianwsg = 0 To ParamAlle.Count - 1
            Call LetWeisungWerte(0, kw, Winkel, Npam, Parab, Wertb, ParamAlle(Ianwsg), ParamAlle.HoleNum, Iee)
          Next Ianwsg
        Case 12, 13, 14
          For Ianwsg = 0 To ParamAlle.Count - 1
            If ParamAlle(Ianwsg).AufgArt.Substring(2, 1) = "B" Then
              Call LetWeisungWerte(0, kw, Winkel, Nqub, Parac, Wertc, ParamAlle(Ianwsg), ParamAlle.HoleNum, Iee)
            Else
              Call LetWeisungWerte(0, kw, Winkel, Npam, Parab, Wertb, ParamAlle(Ianwsg), ParamAlle.HoleNum, Iee)
            End If
          Next Ianwsg
      End Select
      '
    Next kw
    '
    '
    '
    '
    '
    '  Berechnete R-Werte übernehmen
    '
    '
    '
    '
    '
    Select Case MenueParam.MethID
      Case 20
        ' R-Werte (unendlich dick) für Koordinaten Differenzen
        '
        For i = 0 To Nqu / 2 - 1
          GrpRwerte("U").Add(KeyRe(TyRunnd(i).RwertNr), New RefValue)

          Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRunnd(i).RwertNr)), GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)), TyRunnd(i), Winkel, Iee)
          Call LetTyrwert(Winkel, GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)), TyRunnd(i), i, Runnd)
          GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)).QuControl = New QuControls
          Call LetQurwert(GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)), QuRunnd(i))
        Next i
        '

      Case 4, 5, 6, 7, 8, 9, 10, 11, 22, 23 'Farbstärke
        For i = 0 To Nqub - 1 Step J2
          GrpRwerte("H").Add(KeyRe(TyRwerb(i).RwertNr), New RefValue)
          Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRwerb(i).RwertNr)), GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), Winkel, Iee)
          Call LetTyrwert(Winkel, GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), i, Rwerb)
          GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)).QuControl = New QuControls
          Call LetQurwert(GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), QuRwerb(i))
          If J2 = 2 Then
            GrpRwerte("F").Add(KeyRe(TyRwerb(i + 1).RwertNr), New RefValue)
            Call LetNamBemDat(GrpRwerte("S")(KeyRe(TyRwerb(i + 1).RwertNr)), GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), TyRwerb(i + 1), Winkel, Iee)
            Call LetTyrwert(Winkel, GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), TyRwerb(i + 1), i + 1, Rwerb)
            GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)).QuControl = New QuControls
            Call LetQurwert(GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), QuRwerb(i + 1))
          End If
        Next i
        Select Case MenueParam.MethID
          '
          '
          '
          '
          Case 5 'Extinktion für Farbstärke transparent
            '
            ' (Extinktion)
            '
            For i = 0 To Nqu - 1
              GrpRwerte("E").Add(KeyRe(TyGrund(i).RwertNr), New RefValue)
              Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyGrund(i).RwertNr)), GrpRwerte("E")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), Winkel, Iee)
              Call LetTyrwert(Winkel, GrpRwerte("E")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), i, Grund)
              GrpRwerte("E")(KeyRe(TyGrund(i).RwertNr)).QuControl = New QuControls
              Call LetQurwert(GrpRwerte("E")(KeyRe(TyGrund(i).RwertNr)), QuGrund(i))
            Next i
            '
            '
            '
            '
          Case 7, 8   'Absorption Streuung (Farbstärke (weiß/schwarz) deckend (7)
            'Absorption Streuung (Farbstärke weißer und schwarzer Untergrund) (8)
            '
            ' (Absorption)
            '
            For i = 0 To Nqu - 1 Step 2
              GrpRwerte("A").Add(KeyRe(TyGrund(i).RwertNr), New RefValue)
              Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyGrund(i).RwertNr)), GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), Winkel, Iee)
              Call LetTyrwert(Winkel, GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), i, Grund)
              GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)).QuControl = New QuControls
              Call LetQurwert(GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)), QuGrund(i))
            Next i
            '
            ' (Streuung)
            '
            For i = 1 To Nqu - 1 Step 2
              GrpRwerte("D").Add(KeyRe(TyGrund(i).RwertNr), New RefValue)
              Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyGrund(i).RwertNr)), GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), Winkel, Iee)
              Call LetTyrwert(Winkel, GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), i, Grund)
              GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)).QuControl = New QuControls
              Call LetQurwert(GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)), QuGrund(i))
            Next i
            '
            '
            '
            '
            '
            '
            '
            '
          Case Else 'K/S Werte
            '
            ' (K/S)
            '
            For i = 0 To Nqub - 1
              GrpRwerte("K").Add(KeyRe(TyGrund(i).RwertNr), New RefValue)
              Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyGrund(i).RwertNr)), GrpRwerte("K")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), Winkel, Iee)
              Call LetTyrwert(Winkel, GrpRwerte("K")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), i, Grund)
              GrpRwerte("K")(KeyRe(TyGrund(i).RwertNr)).QuControl = New QuControls
              Call LetQurwert(GrpRwerte("K")(KeyRe(TyGrund(i).RwertNr)), QuGrund(i))
            Next i
        End Select
        '

      Case 12, 13, 14       'Fogra

        For i = 0 To nquh - 1
          GrpRwerte("H").Add(KeyRe(TyRwerb(i).RwertNr), New RefValue)
          Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRwerb(i).RwertNr)), GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), Winkel, Iee)
          Call LetTyrwert(Winkel, GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), i, Rwerb)
          GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)).QuControl = New QuControls
          Call LetQurwert(GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), QuRwerb(i))
        Next i
      Case 15, 16     'Deckvermögen
        '
        'Berechnete R-Werte
        '
        For i = 0 To Nqu - 1 Step J2
          '
          'Weiß
          GrpRwerte("H").Add(KeyRe(TyRwerb(i).RwertNr), New RefValue)
          Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRwerb(i).RwertNr)), GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), Winkel, Iee)
          Call LetTyrwert(Winkel, GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), i, Rwerb)
          GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)).QuControl = New QuControls
          Call LetQurwert(GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), QuRwerb(i))
          '
          'Schwarz
          GrpRwerte("F").Add(KeyRe(TyRwerb(i + 1).RwertNr), New RefValue)
          Call LetNamBemDat(GrpRwerte("S")(KeyRe(TyRwerb(i + 1).RwertNr)), GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), TyRwerb(i + 1), Winkel, Iee)
          Call LetTyrwert(Winkel, GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), TyRwerb(i + 1), i + 1, Rwerb)
          GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)).QuControl = New QuControls
          Call LetQurwert(GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), QuRwerb(i + 1))
        Next i
        '
        ' R-Werte (unendlich dick)
        '
        For i = 0 To Nqu / 2 - 1
          GrpRwerte("U").Add(KeyRe(TyRunnd(i).RwertNr), New RefValue)
          Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRunnd(i).RwertNr)), GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)), TyRunnd(i), Winkel, Iee)
          Call LetTyrwert(Winkel, GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)), TyRunnd(i), i, Runnd)
          GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)).QuControl = New QuControls
          Call LetQurwert(GrpRwerte("U")(KeyRe(TyRunnd(i).RwertNr)), QuRunnd(i))
        Next i
        '
        ' R-Werte (max Chroma)
        '
        For i = 0 To Nqu / 2 - 1
          GrpRwerte("C").Add(KeyRe(TyRcrom(i).RwertNr), New RefValue)
          Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRcrom(i).RwertNr)), GrpRwerte("C")(KeyRe(TyRcrom(i).RwertNr)), TyRcrom(i), Winkel, Iee)
          Call LetTyrwert(Winkel, GrpRwerte("C")(KeyRe(TyRcrom(i).RwertNr)), TyRcrom(i), i, Rcrom)
          GrpRwerte("C")(KeyRe(TyRcrom(i).RwertNr)).QuControl = New QuControls
          Call LetQurwert(GrpRwerte("C")(KeyRe(TyRcrom(i).RwertNr)), QuRcrom(i))
        Next i
        '
        ' (Absorption)
        '
        For i = 0 To Nqu - 1 Step 2
          GrpRwerte("A").Add(KeyRe(TyGrund(i).RwertNr), New RefValue)
          Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyGrund(i).RwertNr)), GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), Winkel, Iee)
          Call LetTyrwert(Winkel, GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), i, Grund)
          GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)).QuControl = New QuControls
          Call LetQurwert(GrpRwerte("A")(KeyRe(TyGrund(i).RwertNr)), QuGrund(i))
        Next i
        '
        ' (Streuung)
        '
        For i = 1 To Nqu - 1 Step 2
          GrpRwerte("D").Add(KeyRe(TyGrund(i).RwertNr), New RefValue)
          Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyGrund(i).RwertNr)), GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), Winkel, Iee)
          Call LetTyrwert(Winkel, GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)), TyGrund(i), i, Grund)
          GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)).QuControl = New QuControls
          Call LetQurwert(GrpRwerte("D")(KeyRe(TyGrund(i).RwertNr)), QuGrund(i))
        Next i
    End Select
    '
    '
    'Transmission übernehmen
    '
    '
    If MenueParam.MethID = 8 Or MenueParam.MethID = 16 Then
      '
      'Transmission
      '
      '
      For i = 0 To Nqu - 1 Step 2
        GrpRwerte("T").Add(KeyRe(TyRwerb(i).RwertNr), New RefValue)
        Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRwerb(i).RwertNr)), GrpRwerte("T")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), Winkel, Iee)
        Call LetTyrwert(Winkel, GrpRwerte("T")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), i / 2, Twerb)
        GrpRwerte("T")(KeyRe(TyRwerb(i).RwertNr)).QuControl = New QuControls
        Call LetQurwert(GrpRwerte("T")(KeyRe(TyRwerb(i).RwertNr)), QuRwerb(i))
      Next i
    End If
    '
    '
    '
    'Mischen
    '
    '
    '
    '
    Select Case MenueParam.MethID
      Case 28, 29
        IWESC = 1
        If MenueParam.MethID = 29 Then
          IWESC = 2
        End If
        Nqub = 2 * IWESC
        ReDim TyRwerb(Nqub - 1)
        ReDim QuRwerb(Nqub - 1)
        ReDim Rwerb(Nqub - 1, KM - 1, NWE - 1)
        Npam = Nqu
        ReDim Parab(Npam - 1)
        ReDim Wertb(Npam - 1, 63)

        Call GetWinkNormGk("MIS", MenueParam.Menue.Kwj, Winkel, Iee)
        Call GetMenueParam("MIS", Iee)
        '
        Call MISCHEN(IWESC, NWE, KM, Nqu, Qurwert(0), TyRwert(0), Rwert(0, 0, 0), QuRwerb(0), TyRwerb(0), Rwerb(0, 0, 0), _
        QuUnt(0), TyUnt(0), Runt(0, 0, 0), Npam, Parab(0), Wertb(0, 0), FEHL)
        Ier = FEHL.Ifeh
        If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
          Exit Sub
        End If
        '
        For Ianwsg = 0 To ParamAlle.Count - 1
          Select Case ParamAlle(Ianwsg).AnwsgID
            Case 47, 48
              '

              '
              'Ergebnis Mischen
              '
              '


              'ParamAlle(Ianwsg)(0).Name = parab(0).
              For k = 0 To Nlz - 1
                For kw = 0 To KM - 1
                  Call LetWeisungWerte(k, kw, MenueParam.User.Winkel, Npam, Parab, Wertb, ParamAlle(Ianwsg), ParamAlle.HoleNum, Iee)
                Next kw
              Next k

              If GrpRwerte("H").Count = 0 Then
                '
                '
                'Vergleich Mischung mit gemessenen Kurven
                '
                '
                'Bezug wird übernommen
                '
                '
                '
                '
                'Weiß
                '
                i = J2
                j = 0
                GrpRwerte("H").Add(KeyRe(j), New RefValue)
                Call LetNamBemDat(GrpRwerte("W")(KeyRe(j)), GrpRwerte("H")(KeyRe(j)), TyRwerb(i), Winkel, Iee)
                Call LetTyrwert(Winkel, GrpRwerte("H")(KeyRe(j)), TyRwerb(i), i, Rwerb)
                GrpRwerte("H")(KeyRe(j)).QuControl = New QuControls
                Call LetQurwert(GrpRwerte("H")(KeyRe(j)), QuRwerb(i))
                GrpRwerte("H")(KeyRe(j)).Name = Texxt(1271)
                Mid(GrpRwerte("H")(KeyRe(j)).QuControl.Cart, 2, 1) = "T"
                GrpRwerte("H")(KeyRe(j)).Nr = j
                '
                If J2 = 2 Then
                  '
                  'Schwarz
                  GrpRwerte("F").Add(KeyRe(j), New RefValue)
                  Call LetNamBemDat(GrpRwerte("S")(KeyRe(j)), GrpRwerte("F")(KeyRe(j)), TyRwerb(i + 1), Winkel, Iee)
                  Call LetTyrwert(Winkel, GrpRwerte("F")(KeyRe(j)), TyRwerb(i + 1), i + 1, Rwerb)
                  GrpRwerte("F")(KeyRe(j)).QuControl = New QuControls
                  Call LetQurwert(GrpRwerte("F")(KeyRe(j)), QuRwerb(i + 1))
                  GrpRwerte("F")(KeyRe(j)).Name = Texxt(1271)
                  Mid(GrpRwerte("F")(KeyRe(j)).QuControl.Cart, 2, 1) = "T"
                  GrpRwerte("F")(KeyRe(j)).Nr = j
                End If


                '
                '
                '
                'Messwerte werden übernommen
                '
                For i = J2 To Npam - 1 Step J2
                  '
                  'Weiß
                  '
                  GrpRwerte("H").Add(KeyRe(TyRwert(i).RwertNr), New RefValue)
                  Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRwert(i).RwertNr)), GrpRwerte("H")(KeyRe(TyRwert(i).RwertNr)), TyRwert(i), Winkel, Iee)
                  Call LetTyrwert(Winkel, GrpRwerte("H")(KeyRe(TyRwert(i).RwertNr)), TyRwert(i), i, Rwert)
                  GrpRwerte("H")(KeyRe(TyRwert(i).RwertNr)).QuControl = New QuControls
                  Call LetQurwert(GrpRwerte("H")(KeyRe(TyRwert(i).RwertNr)), Qurwert(i))
                  Mid(GrpRwerte("H")(KeyRe(TyRwert(i).RwertNr)).QuControl.Cart, 3, 1) = "B"
                  If J2 = 2 Then
                    '
                    'Schwarz
                    GrpRwerte("F").Add(KeyRe(TyRwert(i + 1).RwertNr), New RefValue)
                    Call LetNamBemDat(GrpRwerte("S")(KeyRe(TyRwert(i + 1).RwertNr)), GrpRwerte("F")(KeyRe(TyRwert(i + 1).RwertNr)), TyRwert(i + 1), Winkel, Iee)
                    Call LetTyrwert(Winkel, GrpRwerte("F")(KeyRe(TyRwert(i + 1).RwertNr)), TyRwert(i + 1), i + 1, Rwert)
                    GrpRwerte("F")(KeyRe(TyRwert(i + 1).RwertNr)).QuControl = New QuControls

                    Call LetQurwert(GrpRwerte("F")(KeyRe(TyRwert(i + 1).RwertNr)), Qurwert(i + 1))
                    Mid(GrpRwerte("F")(KeyRe(TyRwert(i + 1).RwertNr)).QuControl.Cart, 3, 1) = "B"

                  End If
                Next i
              End If
              Call FarbwertAnweisung(Winkel, ParamAlle(Ianwsg), GrpRwerte, Ier)
          End Select
        Next Ianwsg

        GrpRwerte("H").clear()
        GrpRwerte("F").clear()

        For Ianwsg = 0 To ParamAlle.Count - 1
          If ParamAlle(Ianwsg).AnwsgID <> 47 And ParamAlle(Ianwsg).AnwsgID <> 48 Then
            Nlauf = Nlaaf(J2, ParamAlle(Ianwsg).AufgArt)
            If ParamAlle(Ianwsg).AufgArt.Substring(2, 1) = "B" And ParamAlle(Ianwsg).AufgArt.Substring(8, 1) = "B" Then
              For i = ParamAlle(Ianwsg).Count - 1 To 2 * Nlauf Step -1
                ParamAlle(Ianwsg).Remove(KeyRe(i))
              Next i
            End If
          End If
        Next



        For i = 0 To 2 * IWESC - 1 Step J2
          '
          'Weiß
          '
          GrpRwerte("H").Add(KeyRe(TyRwerb(i).RwertNr), New RefValue)
          Call LetNamBemDat(GrpRwerte("W")(KeyRe(TyRwerb(i).RwertNr)), GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), Winkel, Iee)
          Call LetTyrwert(Winkel, GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), TyRwerb(i), i, Rwerb)
          GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)).QuControl = New QuControls
          Call LetQurwert(GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)), QuRwerb(i))
          If Not GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)).Itp Then
            GrpRwerte("H")(KeyRe(TyRwerb(i).RwertNr)).Name = Texxt(1271)
          End If
          '
          If J2 = 2 Then
            '
            'Schwarz
            GrpRwerte("F").Add(KeyRe(TyRwerb(i + 1).RwertNr), New RefValue)
            Call LetNamBemDat(GrpRwerte("S")(KeyRe(TyRwerb(i + 1).RwertNr)), GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), TyRwerb(i + 1), Winkel, Iee)
            Call LetTyrwert(Winkel, GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), TyRwerb(i + 1), i + 1, Rwerb)
            GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)).QuControl = New QuControls
            Call LetQurwert(GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)), QuRwerb(i + 1))
            If Not GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)).Itp Then
              GrpRwerte("F")(KeyRe(TyRwerb(i + 1).RwertNr)).Name = Texxt(1271)
            End If
          End If
        Next i
    End Select
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
    '
    'Merkmale für alle Anweisungen (außer Mischen)
    '
    '
    '

    '
    For Ianwsg = 0 To ParamAlle.Count - 1
      If ParamAlle(Ianwsg).AnwsgID = 47 Or ParamAlle(Ianwsg).AnwsgID = 48 Then
        Continue For
      End If
      Call FarbwertAnweisung(Winkel, ParamAlle(Ianwsg), GrpRwerte, Ier)
    Next Ianwsg

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
    '
    '
    ''
  End Sub
  Sub CalcFarbWerte(ByRef Winkel As AngGeos, ByRef GrpRwerte As RefValuesGrp, ByRef ParamAlle As ValuesGrpsAssigns, ByRef Ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim l As Integer
    Dim KM As Integer
    Dim Nlq As Integer
    Dim Nlz As Integer
    Dim Nlauf As Integer
    Dim StrRw As String
    Dim J2 As Integer
    '
    'Aufbau ParamAlle
    '
    '
    'Call GetMenueParam("FAR", Ier)

    KM = Winkel.Km
    Nlq = GrpRwerte(0).Count
    If GrpRwerte.Count > 1 AndAlso Not IsNothing(GrpRwerte(1)) AndAlso Nlq = 0 Then
      Nlq = GrpRwerte(1).Count
    End If
    If Nlq = 0 Then
      Ier = 2970
      MsgBox(Texxt(Ier))
      Exit Sub
    End If
    ''
    '
    'Löschen Werte in ParamAlle
    '
    '
    For i = 0 To ParamAlle.Count - 1
      ParamAlle(i).clear()
    Next i
    '
    J2 = 1
    If GrpRwerte.Count > 1 AndAlso GrpRwerte(1).Count > 0 Then
      J2 = 2
    End If
    Nlz = MenueParam.Normfa.Nlz
    For i = 0 To ParamAlle.Count - 1
      Nlauf = Nlaaf(J2, ParamAlle(i).AufgArt)
      For j = 0 To Nlauf * Nlq - 1
        StrRw = KeyRe(j)
        ParamAlle(i).Add(StrRw, New ValuesGrps)
        For k = 0 To Nlz - 1
          ParamAlle(i)(j).Add(KeyRe(MenueParam.Normfa(k).LichtID), New ValuesGrp)
          For kw = 0 To KM - 1
            ParamAlle(i)(j)(k).Add(Winkel(kw).Chrm, New Values)
            For l = 0 To ParamAlle(i).CountMerk - 1
              ParamAlle(i)(j)(k)(kw).Add(ParamAlle(i).Merk(l).Ken, New Object)
            Next l
          Next kw
        Next k
      Next j
      Call FarbWrtAll(Winkel, GrpRwerte, ParamAlle(i), Ier)
    Next i
    '

    ''
  End Sub
  Sub FarbwertAnweisung(ByRef Winkel As AngGeos, ByRef ParamAnweisung As ValuesGrpsAssign, ByVal GrpRwerte As RefValuesGrp, ByRef Ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim AnwsgID As Integer
    Dim AufgArt(11) As Byte
    Dim AAufgart(11) As Byte
    Dim AuswAnz As Integer
    Dim AuswID() As Integer
    Dim Key As String
    Dim GrpRefFarbwrt As RefValuesGrp


    Dim NLQ As Integer
    '
    Dim TBM As Byte
    Dim PBM As Byte
    '
    GrpRefFarbwrt = New RefValuesGrp
    GrpRefFarbwrt.Add("W", New RefValues)
    GrpRefFarbwrt.Add("S", New RefValues)
    '
    '
    '
    'Untergründe
    '
    '
    '
    For i = 0 To GrpRefFarbwrt.Count - 1
      GrpRefFarbwrt(i).RefUnt = GrpRwerte(i).RefUnt
    Next
    '
    '
    '
    NLQ = GrpRwerte(0).Count
    If GrpRwerte.Count > 1 AndAlso Not IsNothing(GrpRwerte(1)) AndAlso NLQ = 0 Then
      NLQ = GrpRwerte(1).Count
    End If
    If NLQ = 0 Then
      Ier = 2970
      MsgBox(Texxt(Ier))
      Exit Sub
    End If
    '
    '
    '
    '
    '
    '
    AnwsgID = ParamAnweisung.AnwsgID
    For i = 0 To 11
      AufgArt(i) = Asc(ParamAnweisung.AufgArt.Substring(i, 1))
    Next i
    AuswAnz = ParamAnweisung.AuswID.Count
    If AuswAnz < 0 Then
      MsgBox(Texxt(3599))
      Ier = -1
      Exit Sub
    End If
    For i = 0 To 11
      AAufgart(i) = AufgArt(i)
    Next i
    ReDim AuswID(AuswAnz)
    For i = 0 To AuswAnz - 1
      AuswID(i) = ParamAnweisung.AuswID(i)
    Next i
    '
    '
    '
    TBM = AAufgart(2)
    PBM = AAufgart(8)
    '
    GrpRefFarbwrt("W").clear()
    GrpRefFarbwrt("S").clear()
    '
    '
    If TBM = Asc("M") And PBM = Asc("M") Then

      '
      'R-Werte (Messung)
      '
      '
      If Not IsNothing(GrpRwerte("W")) AndAlso GrpRwerte("W").Count > 0 Then
        For i = 0 To GrpRwerte("W").Count - 1
          GrpRefFarbwrt("W").Add(GrpRwerte("W").RwKey(i), GrpRwerte("W")(i))
        Next
      End If

      If GrpRwerte("S").Count > 0 Then
        For i = 0 To GrpRwerte("S").Count - 1
          GrpRefFarbwrt("S").Add(GrpRwerte("S").RwKey(i), GrpRwerte("S")(i))
        Next
      End If
      '
      '
      '
    ElseIf TBM = Asc("B") And PBM = Asc("B") Then
      '
      '
      '
      'R-Werte (Rechnung)
      '
      '
      If GrpRwerte.ContainsKey("H") AndAlso GrpRwerte("H").Count > 0 Then
        For i = 0 To GrpRwerte("H").Count - 1
          GrpRefFarbwrt("W").Add(GrpRwerte("H").RwKey(i), GrpRwerte("H")(i))
          GrpRefFarbwrt("W")(i).Banum = ""
        Next
      End If

      If GrpRwerte.ContainsKey("F") AndAlso GrpRwerte("F").Count > 0 Then
        For i = 0 To GrpRwerte("F").Count - 1
          GrpRefFarbwrt("S").Add(GrpRwerte("F").RwKey(i), GrpRwerte("F")(i))
          GrpRefFarbwrt("S")(i).Banum = ""
        Next
      End If
      '
      '
      '
    ElseIf TBM = Asc("U") And PBM = Asc("U") Then
      '
      '
      '
      '
      '
      'R-Werte (unendlich dick)
      '
      '
      If GrpRwerte.ContainsKey("U") AndAlso GrpRwerte("U").Count > 0 Then
        For i = 0 To GrpRwerte("U").Count - 1
          GrpRefFarbwrt("W").Add(GrpRwerte("U").RwKey(i), GrpRwerte("U")(i))
          GrpRefFarbwrt("W")(i).Banum = ""
        Next
      End If
      '
      '
      '
      '
    ElseIf TBM = Asc("C") And PBM = Asc("C") Then
      '
      '
      '
      '
      '
      'R-Werte (max. Chroma)
      '
      '
      '
      '
      '
      If GrpRwerte.ContainsKey("C") AndAlso GrpRwerte("C").Count > 0 Then
        For i = 0 To GrpRwerte("C").Count - 1
          GrpRefFarbwrt("W").Add(GrpRwerte("C").RwKey(i), GrpRwerte("C")(i))
          GrpRefFarbwrt("W")(i).Banum = ""
        Next
      End If
      '
      '
      '
    ElseIf TBM = Asc("M") And PBM = Asc("B") Then
      '
      '
      '
      'Vergleich Messung - Rechnung
      '
      '
      '
      '
      '
      If GrpRwerte.ContainsKey("W") AndAlso GrpRwerte.ContainsKey("H") _
        AndAlso GrpRwerte("W").Count > 0 AndAlso GrpRwerte("H").Count > 0 Then
        j = -1
        For i = 0 To GrpRwerte("W").Count - 1
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("W").Add(Key, GrpRwerte("W")(i))
          GrpRefFarbwrt("W")(Key).Banum = "m"
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("W").Add(Key, GrpRwerte("H")(i))
          GrpRefFarbwrt("W")(Key).Banum = "b"
        Next
      End If

      '
      '
      If GrpRwerte.ContainsKey("S") AndAlso GrpRwerte.ContainsKey("F") _
        AndAlso GrpRwerte("S").Count > 0 AndAlso GrpRwerte("H").Count > 0 Then
        j = -1
        For i = 0 To GrpRwerte("S").Count - 1
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("S").Add(Key, GrpRwerte("S")(i))
          GrpRefFarbwrt("S")(Key).Banum = "m"
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("S").Add(Key, GrpRwerte("F")(i))
          GrpRefFarbwrt("S")(Key).Banum = "b"
        Next
      End If
    ElseIf TBM = Asc("M") And PBM = Asc("U") Then
      '
      '
      '
      'Vergleich Messung - unendlich dick
      '
      '
      '
      '
      '
      '
      '
      '
      '
      If GrpRwerte.ContainsKey("W") AndAlso GrpRwerte.ContainsKey("U") _
        AndAlso GrpRwerte("W").Count > 0 AndAlso GrpRwerte("U").Count > 0 Then
        j = -1
        For i = 0 To GrpRwerte("W").Count - 1
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("W").Add(Key, GrpRwerte("W")(i))
          GrpRefFarbwrt("W")(Key).Banum = "m"
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("W").Add(Key, GrpRwerte("U")(i).clone)
          GrpRefFarbwrt("W")(Key).Banum = "b"
          GrpRefFarbwrt("W")(Key).Name = GrpRefFarbwrt("W")(Key).Name & "(U)"
        Next
      End If
      '
      '
      If GrpRwerte.ContainsKey("S") AndAlso GrpRwerte.ContainsKey("U") _
        AndAlso GrpRwerte("S").Count > 0 AndAlso GrpRwerte("U").Count > 0 Then
        j = -1
        For i = 0 To GrpRwerte("S").Count - 1
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("S").Add(Key, GrpRwerte("S")(i))
          GrpRefFarbwrt("S")(Key).Banum = "m"

          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("S").Add(Key, GrpRwerte("U")(i).clone)
          GrpRefFarbwrt("S")(Key).Banum = "b"
          GrpRefFarbwrt("S")(Key).QuControl.Cart = GrpRefFarbwrt("S")(Key).QuControl.Cart.Substring(0, 3) & "S"
          GrpRefFarbwrt("S")(Key).Name = GrpRefFarbwrt("S")(Key).Name & "(U)"
        Next
      End If

    ElseIf TBM = Asc("B") And PBM = Asc("U") Then
      '
      '
      '
      'Vergleich Rechnung - unendlich dick
      '
      '
      '
      '
      '
      If GrpRwerte.ContainsKey("H") AndAlso GrpRwerte.ContainsKey("U") _
        AndAlso GrpRwerte("H").Count > 0 AndAlso GrpRwerte("U").Count > 0 Then
        j = -1
        For i = 0 To GrpRwerte("W").Count - 1
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("W").Add(Key, GrpRwerte("H")(i))
          GrpRefFarbwrt("W")(Key).Banum = "m"
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("W").Add(Key, GrpRwerte("U")(i).clone)
          GrpRefFarbwrt("W")(Key).Banum = "b"
          GrpRefFarbwrt("W")(Key).Name = GrpRefFarbwrt("W")(Key).Name & "(U)"

        Next
      End If
      '
      '
      If GrpRwerte.ContainsKey("F") AndAlso GrpRwerte.ContainsKey("U") _
        AndAlso GrpRwerte("F").Count > 0 AndAlso GrpRwerte("U").Count > 0 Then
        j = -1
        For i = 0 To GrpRwerte("S").Count - 1
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("S").Add(Key, GrpRwerte("F")(i))
          GrpRefFarbwrt("S")(Key).Banum = "m"
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("S").Add(Key, GrpRwerte("U")(i).clone)
          GrpRefFarbwrt("S")(Key).Banum = "b"
          GrpRefFarbwrt("S")(Key).QuControl.Cart = GrpRefFarbwrt("S")(Key).QuControl.Cart.Substring(0, 3) & "S"
          GrpRefFarbwrt("S")(Key).Name = GrpRefFarbwrt("S")(Key).Name & "(U)"

        Next
      End If
      '

      '
      '
      '

    ElseIf TBM = Asc("M") And PBM = Asc("C") Then
      '
      '
      '
      'Vergleich Messung - max. Chroma
      '
      '
      '
      '
      '

      '
      If GrpRwerte.ContainsKey("W") AndAlso GrpRwerte.ContainsKey("C") _
      AndAlso GrpRwerte("W").Count > 0 AndAlso GrpRwerte("C").Count > 0 Then
        j = -1
        For i = 0 To GrpRwerte("W").Count - 1
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("W").Add(Key, GrpRwerte("W")(i))
          GrpRefFarbwrt("W")(Key).Banum = "m"
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("W").Add(Key, GrpRwerte("C")(i).clone)
          GrpRefFarbwrt("W")(Key).Banum = "b"
          GrpRefFarbwrt("W")(Key).Name = GrpRefFarbwrt("W")(Key).Name & "(C)"

        Next
      End If
      '
      '
      If GrpRwerte.ContainsKey("S") AndAlso GrpRwerte.ContainsKey("C") _
        AndAlso GrpRwerte("S").Count > 0 AndAlso GrpRwerte("C").Count > 0 Then
        j = -1
        For i = 0 To GrpRwerte("S").Count - 1
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("S").Add(Key, GrpRwerte("S")(i))
          GrpRefFarbwrt("S")(Key).Banum = "m"
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("S").Add(Key, GrpRwerte("C")(i).clone)
          GrpRefFarbwrt("S")(Key).Banum = "b"
          GrpRefFarbwrt("S")(Key).QuControl.Cart = GrpRefFarbwrt("S")(Key).QuControl.Cart.Substring(0, 3) & "S"
          GrpRefFarbwrt("S")(Key).Name = GrpRefFarbwrt("S")(Key).Name & "(C)"

        Next
      End If

    ElseIf TBM = Asc("B") And PBM = Asc("C") Then
      '
      '
      '
      'Vergleich Rechnung - max. Chroma
      '
      '
      '
      '
      '
      If GrpRwerte.ContainsKey("H") AndAlso GrpRwerte.ContainsKey("C") _
      AndAlso GrpRwerte("H").Count > 0 AndAlso GrpRwerte("C").Count > 0 Then
        j = -1
        For i = 0 To GrpRwerte("W").Count - 1
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("W").Add(Key, GrpRwerte("H")(i))
          GrpRefFarbwrt("W")(Key).Banum = "m"
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("W").Add(Key, GrpRwerte("C")(i).clone)
          GrpRefFarbwrt("W")(Key).Banum = "b"
          GrpRefFarbwrt("W")(Key).Name = GrpRefFarbwrt("W")(Key).Name & "(C)"

        Next
      End If
      '
      '
      If GrpRwerte.ContainsKey("F") AndAlso GrpRwerte.ContainsKey("C") _
      AndAlso GrpRwerte("F").Count > 0 AndAlso GrpRwerte("C").Count > 0 Then
        j = -1
        For i = 0 To GrpRwerte("S").Count - 1
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("S").Add(Key, GrpRwerte("F")(i))
          GrpRefFarbwrt("S")(Key).Banum = "m"
          j = j + 1
          Key = KeyRe(j)
          GrpRefFarbwrt("S").Add(Key, GrpRwerte("C")(i).clone)
          GrpRefFarbwrt("S")(Key).Banum = "b"
          GrpRefFarbwrt("S")(Key).QuControl.Cart = GrpRefFarbwrt("S")(Key).QuControl.Cart.Substring(0, 3) & "S"
          GrpRefFarbwrt("S")(Key).Name = GrpRefFarbwrt("S")(Key).Name & "(C)"

        Next
      End If

    End If
    '
    '
    'Farbwerte für Anweisung Ianwsg
    '
    '
    Call FarbWrtAll(Winkel, GrpRefFarbwrt, ParamAnweisung, Ier)

    '
    GrpRefFarbwrt.dispose()
  End Sub
  Sub FarbWrtAll(ByRef Winkel As AngGeos, ByRef GrpRwerte As RefValuesGrp, ByRef ParamAnweisung As ValuesGrpsAssign, ByRef Ier As Integer)
    Dim i As Integer
    Dim k As Integer
    Dim j As Integer
    Dim jj As Integer
    Dim jw As Integer
    Dim js As Integer
    Dim J2 As Integer
    Dim kw As Integer
    Dim NWE As Integer
    Dim KM As Integer
    Dim Nu As Integer
    Dim nqu As Integer
    Dim NLW As Integer
    Dim NLS As Integer
    Dim NLQ As Integer
    Dim Npam As Integer
    Dim Rcount As Integer
    Dim AnwsgID As Integer
    Dim AufgArt As String
    Dim AArt As String
    Dim ParMerk(63) As Single      'Parameter zur Berechnung von Merkmalen
    Dim AAufgArt(11) As Byte
    Dim AuswANZ As Integer
    Dim AuswID() As Integer
    Dim RwertName() As String
    Dim RwertBem() As String
    Dim TyRwert() As TyRwert        'R-Werte Messung/Vorlage
    Dim Qurwert() As TyQual        'Zusätze für Qualität
    Dim Rwert(,,) As Single
    Dim TyUnt() As TyRwert
    Dim QuUnt() As TyQual
    Dim UntName() As String
    Dim UntBem() As String
    Dim Runt(,,) As Single
    Dim Param() As TyWert
    Dim Wert(,) As Double
    Dim PaNr As Integer
    Dim Name As String
    Dim Dat As Date
    Dim AdGr As Integer
    Dim AdRw As Integer
    Dim AdGroup() As Integer
    Dim AdRwert() As Integer
    '
    'Merkmalbezeichnung für B-Wert


    '
    '
    '
    For k = 0 To ParamAnweisung.CountMerk - 1
      If ParamAnweisung.Merk(k).ID = 775 And MenueParam.Menue.BwertID <= 8 Then
        ParamAnweisung.Merk(k).Kbez = MenueParam.Menue.BwertName(MenueParam.Menue.BwertID)
      End If
    Next k
    ''
    '
    '
    'NormFarbWerte übernehmen
    '
    '
    '
    '
    '
    Call GetWinkNormGk("FAR", MenueParam.Menue.Kwj, Winkel, Ier)

    If Ier <> 0 Then Exit Sub
    '
    '
    '
    'Start Farbkoordinaten
    '
    '
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    Call FARBEG(NWE, KM, FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      Exit Sub
    End If
    '

    '
    'R-Werte umspeichern nach Rwert,Tyrwert und Qurwrt
    '
    '
    '
    '
    'R-Werte übernehmen
    '
    'Untergründe übernehmen
    '
    '
    'R-Werte Untergrund umspeichert
    '
    '
    Nu = 0
    If Not IsNothing(GrpRwerte(0).RefUnt) AndAlso Not GrpRwerte(0).RefUnt.RefKurv.Count > 0 Then
      Nu = Nu + 1
    End If
    If GrpRwerte.Count > 1 AndAlso Not IsNothing(GrpRwerte(1).RefUnt) AndAlso Not GrpRwerte(1).RefUnt.RefKurv.Count > 0 Then
      Nu = Nu + 1
    End If
    '
    If Nu > 0 Then
      ReDim TyUnt(Nu - 1)
      ReDim QuUnt(Nu - 1)
      ReDim UntName(Nu - 1)
      ReDim UntBem(Nu - 1)
      ReDim Runt(Nu - 1, KM - 1, NWE - 1)
      For i = 0 To Nu - 1
        Call GetTyrwert(Winkel, GrpRwerte(i).RefUnt, TyUnt(i), i, Runt)
        Call GetQurwert(GrpRwerte(i).RefUnt, QuUnt(i), UntName(i), UntBem(i))
      Next i
    Else
      Nu = 3
      ReDim TyUnt(Nu - 1)
      ReDim QuUnt(Nu - 1)
      ReDim Runt(Nu - 1, KM - 1, NWE - 1)
      Call StartUnt(MenueParam.Messg, Winkel, TyUnt, QuUnt, Runt)
    End If
    '
    '

    NLW = GrpRwerte(0).Count
    NLS = 0
    If GrpRwerte.Count > 1 AndAlso GrpRwerte(1).Count > 0 Then
      NLS = GrpRwerte(1).Count
    End If
    nqu = NLW + NLS
    If NLW > 0 And NLS > 0 Then
      If NLW <> NLS Then
        MsgBox("Falsche Anzahl weiß-schwarz")
        Exit Sub
      End If
    End If
    ReDim TyRwert(nqu - 1)
    ReDim Qurwert(nqu - 1)
    ReDim Rwert(nqu - 1, KM - 1, NWE - 1)
    ReDim RwertName(nqu - 1)
    ReDim RwertBem(nqu - 1)
    ReDim AdGroup(nqu - 1)
    ReDim AdRwert(nqu - 1)
    '
    j = -1
    jw = -1
    js = -1
    For i = 0 To Max(NLW, NLS) - 1
      If NLW > 0 Then
        j = j + 1
        jw = jw + 1
        AdGroup(j) = 0
        AdRwert(j) = i
        Call GetTyrwert(Winkel, GrpRwerte(0)(i), TyRwert(j), j, Rwert)
        Call GetQurwert(GrpRwerte(0)(i), Qurwert(j), RwertName(j), RwertBem(j))
      End If
      If NLS > 0 Then
        j = j + 1
        js = js + 1
        AdGroup(j) = 1
        AdRwert(j) = i
        Call GetTyrwert(Winkel, GrpRwerte(1)(i), TyRwert(j), j, Rwert)
        Call GetQurwert(GrpRwerte(1)(i), Qurwert(j), RwertName(j), RwertBem(j))
      End If
    Next
    NLQ = NLW
    If NLQ = 0 Then
      NLQ = NLS
    End If
    '
    J2 = 1
    If nqu = 2 * NLQ Then
      J2 = 2
    End If '
    'Menüparameter übernehmen
    '
    '
    Call GetMenueParam("FAR", Ier)
    AnwsgID = ParamAnweisung.AnwsgID
    AufgArt = ParamAnweisung.AufgArt
    For i = 0 To 11
      AAufgArt(i) = Asc(AufgArt.Substring(i, 1))
    Next i
    AuswANZ = ParamAnweisung.AuswID.Count
    If AuswANZ <= 0 Then
      MsgBox(Texxt(3599))
      Ier = -1
      Exit Sub
    End If
    ReDim AuswID(AuswANZ - 1)
    For i = 0 To AuswANZ - 1
      AuswID(i) = ParamAnweisung.AuswID(i)
    Next i

    '
    For k = 0 To MenueParam.Normfa.Nlz - 1
      If AufgArt.Substring(4, 1) = "?" Then
        AAufgArt(4) = Asc(CStr(k + 1))
        AAufgArt(10) = Asc(CStr(k + 1))
      End If
      If AAufgArt(4) = Asc(CStr(k + 1)) Then
        '
        '
        For kw = 0 To KM - 1
          If AufgArt.Substring(5, 1) = "?" Then
            AAufgArt(5) = Asc(CStr(kw + 1))
          End If
          If AufgArt.Substring(11, 1) = "?" Then
            AAufgArt(11) = Asc(CStr(kw + 1))
          End If
          If (((AufgArt.Substring(5, 1) = AufgArt.Substring(11, 1) Or AufgArt.Substring(5, 1) = "1" And AufgArt.Substring(11, 1) = "?" _
              And AAufgArt(5) = AAufgArt(11) _
              Or AufgArt.Substring(5, 1) <> AufgArt.Substring(11, 1) _
              And AAufgArt(5) <> AAufgArt(11))) _
              And AAufgArt(11) = Asc(CStr(kw + 1))) Then
            If ParamAnweisung.CountWinkel = 0 OrElse ParamAnweisung.Winkel(kw).Exist Then
              '
              Npam = AuswANZ * nqu
              If AAufgArt(5) <> AAufgArt(11) Then
                '
                'Verdoppelung der PARAM Vektoren bei verschiedenen Winkeln
                '
                Npam = 2 * Npam
                'Ndop = 2
              End If
              ReDim Param(Npam - 1)
              ReDim Wert(Npam - 1, 63)
              '
              '
              '
              '
              'Farbkoordinaten für Lichtart(k) und Winkel(kw)
              '
              '
              '*****************
              '*****************
              '*** DLL-Aufruf **
              '*****************
              '*****************
              'MsgBox charby(AAufgart)
              If IsNothing(TyRwert) OrElse TyRwert.Count = 0 Then Exit Sub
              Call FarKordAll(k, kw, NWE, KM, Winkel, nqu, _
              RwertName, RwertBem, Qurwert, TyRwert, Rwert, _
              QuUnt, TyUnt, Runt, _
              AAufgArt, AuswANZ, AuswID, Npam, Param, Wert, FEHL)
              '
              '
              '
              'Fehlerabfrage
              '
              '
              '
              Ier = FEHL.Ifeh
              If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
                Exit Sub
              End If
              '
              '
              '
              'FarbWerte umspeichern nach ParamAnweisung
              '
              '
              Call LetWeisungWerte(k, kw, MenueParam.User.Winkel, Npam, Param, Wert, ParamAnweisung, HUGE, Ier)
              '
              If Ier <> 0 Then Exit Sub
            End If
          End If
        Next kw

      End If
    Next k
    '
    '
    'Namen,Datum usw. den Merkmalswerten zuornen
    '
    '
    AArt = GetString(AAufgArt)
    For i = 0 To Npam - 1 Step ParamAnweisung.AuswID.Count

      PaNr = Param(i).LNR
      jj = Fix(i / ParamAnweisung.AuswID.Count)
      If ParamAnweisung.ContainsRwKey(KeyRe(PaNr)) Then
        ParamAnweisung(KeyRe(PaNr)).Nr = -1
      Else
        Continue For
      End If
      If (CHMeth(TyWertString(Param(i)), AArt.Substring(0, 6)) _
      Or CHMeth(TyWertString(Param(i)), AArt.Substring(6, 6))) And Param(i).Itp > 0 Then
        If Param(i).RwertNr >= 0 Then
          If Param(i).Itp = 1 Then
            ParamAnweisung(KeyRe(PaNr)).Itp = True
          ElseIf Param(i).Itp = 2 Then
            ParamAnweisung(KeyRe(PaNr)).Itp = False
          End If
          AdGr = AdGroup(jj)
          AdRw = AdRwert(jj)
          ParamAnweisung(KeyRe(PaNr)).Nr = GrpRwerte(AdGr)(AdRw).Nr
          Name = GrpRwerte(AdGr)(AdRw).Name
          Dat = GrpRwerte(AdGr)(AdRw).DatTim '
          If ParamAnweisung(KeyRe(PaNr)).Name = "" Then
            ParamAnweisung(KeyRe(PaNr)).Name = Name
            ParamAnweisung(KeyRe(PaNr)).Banum = GrpRwerte(AdGr)(AdRw).Banum
            ParamAnweisung(KeyRe(PaNr)).Dattim = Dat
            ParamAnweisung(KeyRe(PaNr)).Bem = GrpRwerte(AdGr)(AdRw).Bem
          End If
        End If
      End If
    Next i
    Rcount = ParamAnweisung.Count
    For i = Rcount - 1 To 0 Step -1
      If ParamAnweisung(i).Nr = -1 Then
        ParamAnweisung.Remove(i)
      End If
    Next i

    'Ende Farbkoordinaten
    '
    '
    '
    '
    Call FAREND(FEHL)
    Erase TyRwert
    Erase Qurwert
    Erase Rwert
    Erase TyUnt
    Erase QuUnt
    Erase Runt
  End Sub
  Sub FarbDifferenzXYZ(ByVal KW As Integer, ByVal KNO As Integer, ByVal Winkel As AngGeos, ByVal XYZS(,,) As Single, ByVal XYZI(,,) As Single, ByRef DE As Single, ByRef DL As Single, ByRef DC As Single, ByRef DH As Single, ByRef DA As Single, ByRef DB As Single, ByRef IER As Integer)
    Call GetWinkNormGk("FAR", KW, Winkel, IER)
    Call GetMenueParam("FAR", IER)
    Call DIFFXYZ(KNO + 1, XYZS(KW, KNO, 0), XYZI(KW, KNO, 0), DE, DL, DC, DH, DA, DB, IER)
  End Sub
  Sub FarbDifferenzRef(ByVal IRT As Integer, ByVal KW As Integer, ByVal KNO As Integer, ByVal Winkel As AngGeos, ByVal RB() As Single, ByVal RP() As Single, ByRef DE As Single, ByRef DL As Single, ByRef DC As Single, ByRef DH As Single, ByRef DA As Single, ByRef DB As Single, ByRef IER As Integer)
    Call GetWinkNormGk("FAR", KW, Winkel, IER)
    Call GetMenueParam("FAR", IER)
    Call FarbDiff(Winkel, IRT, KW, KNO, RB, RP, DE, DL, DC, DH, DA, DB, IER)
  End Sub
  Function BwertIdent(ByRef Winkel As AngGeos, ByRef Rwerte As RefValues, ByRef Ier As Integer) As Integer
    Dim Nqu As Integer
    Dim i As Integer
    Dim NWE As Integer
    Dim KM As Integer
    Dim Iee As Integer
    Dim Qurwert() As TyQual
    Dim RwertName() As String
    Dim RwertBem() As String
    Dim TyRwert() As TyRwert
    Dim Rmit(,,) As Single
    '
    'Menüparameter übernehmen
    '
    '
    '
    'Call GetMenueParam( Winkel, Menal, ier)
    '
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    Nqu = Rwerte.Count
    '
    '
    '
    '
    Call GetWinkNormGk("QUA", MenueParam.Menue.Kwj, Winkel, Iee)
    '
    '
    '
    'Start Qualitätskontrolle
    '
    '
    '
    '
    Call QUABEG(NWE, KM, FEHL)
    '
    '
    '
    '
    ReDim TyRwert(Nqu - 1)
    ReDim Qurwert(Nqu - 1)
    ReDim RwertName(Nqu - 1)
    ReDim RwertBem(Nqu - 1)
    ReDim Rmit(Nqu - 1, KM - 1, NWE - 1)

    For i = 0 To Nqu - 1
      Call GetTyrwert(Winkel, Rwerte(i), TyRwert(i), i, Rmit)
      Call GetQurwert(Rwerte(i), Qurwert(i), RwertName(i), RwertBem(i))
    Next i
    '
    BwertIdent = BWEBER(MenueParam.Menue.Kwj, NWE, KM, Nqu, Qurwert(0), TyRwert(0), Rmit(0, 0, 0), FEHL)

    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      Exit Function
    End If
  End Function
  Sub FarbWerteXYZLABCH(ByVal winkel As AngGeos, ByRef Rwerte As RefValue, ByRef XYZ() As Single, ByRef ALAB() As Single, ByRef ALCH() As Single, ByRef Ier As Integer)
    Dim Iee As Integer
    Dim NWE As Integer
    Dim KM As Integer
    Dim ReInput() As TyRwert
    Dim R(,,) As Single
    ReDim ReInput(0)
    NWE = winkel.Wsol.Nwe
    KM = winkel.Km
    '
    '
    ReDim R(0, KM - 1, NWE - 1)
    '
    '
    Ier = 0
    '
    '
    '
    '
    '
    '
    'NormFarbWerte,Winkel und GK-Werte übernehmen
    '
    '
    '
    '
    '
    Call GetWinkNormGk("FAR", MenueParam.Menue.Kwj, winkel, Iee)
    '

    '
    '
    '

    '
    'Menüparameter übernehmen
    '
    '
    '
    '
    '
    'Menüparameter übernehmen
    '
    '
    Call GetMenueParam("FAR", Iee)
    If Iee <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    '
    '
    'R-Werte übernehmen
    '
    '
    '
    '
    '
    '
    Call GetTyrwert(winkel, Rwerte, ReInput(0), 0, R)
    '
    '
    '
    '
    'Farbwerte  berechnen
    '
    '
    '
    Call XYZLABCH(NWE, 0, 1, 1, R(0, 0, 0), XYZ(0), ALAB(0), ALCH(0), Ier)
    '

    Erase ReInput
    Erase ReInput
  End Sub
  Sub RefFarbWerte(ByRef Winkel As AngGeos, ByRef Isch As Integer, ByRef Ifl As Integer, ByRef Ifw As Integer, ByRef FAKO(,) As Single, ByRef XYZS(,,) As Single, ByRef XYZI(,,) As Single, ByRef AllRwerte As RefValues, ByRef Ier As Integer)
    Dim i As Integer
    Dim Iee As Integer
    Dim NWE As Integer
    Dim NLZ As Integer
    Dim KM As Integer
    Dim kw As Integer
    Dim ReResult As TyRwert
    Dim ReInput() As TyRwert
    Dim Rmit(,,) As Single
    Dim RResult(,,) As Single
    ReDim ReInput(3)
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    NLZ = MenueParam.Normfa.Nlz
    If UBound(FAKO, 2) / 3 + 1 < NLZ Then
      NLZ = Int((UBound(FAKO, 2) + 1) / 3 + 0.5)
    End If
    If UBound(FAKO, 1) + 1 < KM Then
      KM = UBound(FAKO, 1) + 1
    End If
    '
    '
    ReDim Rmit(2, KM - 1, NWE - 1)
    ReDim RResult(0, KM - 1, NWE - 1)
    '
    '
    Ier = 0
    '
    'R-Werte mit Hilfe von Farbwerten bzw. Farbwertdifferenzen berechnen
    '
    '
    '
    '
    '
    'NormFarbWerte,Winkel und GK-Werte übernehmen
    '
    '
    '
    '
    '

    Call GetWinkNormGk("FUM", MenueParam.Menue.Kwj, Winkel, Iee)
    '

    '
    '
    '

    '
    'Menüparameter übernehmen
    '
    '
    '
    '
    '
    'Menüparameter übernehmen
    '
    '
    Call GetMenueParam("FUM", Iee)
    If Iee <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    '
    '
    'R-Werte übernehmen
    '
    '
    '
    '
    'R-Werte (Allrwerte) für Eingabe und Ausgabe
    '(0)  R-Wert der Rechnung
    '(1)  R-Wert, der möglichst gut erreicht werden soll.
    '(2)  untere Grenze für R-Kurve
    '(3)  obere Grenze für R-Kurve
    '
    '
    If Ifl = 3 Then
      'Untere und obere Grenze (Standard) festlegen
      '
      '
      '
      For kw = 0 To KM - 1
        For i = 0 To Winkel.Wsol.Nwe - 1
          Rmit(1, kw, i) = Winkel(kw).GK(0)
          Rmit(2, kw, i) = Winkel(kw).GK(9)
        Next i
      Next kw
    Else
      For i = 0 To 2
        Call GetTyrwert(Winkel, AllRwerte(KeyRe(i + 1)), ReInput(i), i, Rmit)
      Next i
    End If
    '
    '

    '
    '
    'R-Werte berechnen
    '
    '
    '
    '
    AllRwerte(KeyRe(0)).IVoNa = False
    AllRwerte(KeyRe(0)).kwb = -1
    ReDim XYZS(KM - 1, NLZ - 1, 2)
    ReDim XYZI(KM - 1, NLZ - 1, 2)
    Call FARBUMRE(NWE, KM, Isch, Ifl, Ifw, NLZ, FAKO(0, 0), XYZS(0, 0, 0), XYZI(0, 0, 0), ReInput(0), Rmit(0, 0, 0), ReResult, RResult(0, 0, 0), FEHL)
    Ier = FEHL.Ifeh
    Iee = FEHL.Iwarn
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      Ier = FEHL.Ifeh
      Exit Sub
    End If
    Ier = -Iee
    '
    '
    'R-Werte umspeichern
    '
    '
    'AllRwerte(KeyRe(0)).RefKurv.clear()
    'For kw = 0 To KM - 1
    ' AllRwerte(KeyRe(0)).RefKurv.Add(Winkel(kw).Chrm, New CurveRef(NWE))
    ' Next kw
    Call ADDCurves(AllRwerte(KeyRe(0)).RefKurv)
    '
    Call LetRwerte(Winkel, 0, RResult, AllRwerte(KeyRe(0)))
    AllRwerte(KeyRe(0)).IVoNa = True
    AllRwerte(KeyRe(0)).Iplott = True
    AllRwerte(KeyRe(0)).Name = Texxt(886)
    AllRwerte(KeyRe(0)).kwb = 0
    Erase ReInput
    Erase ReInput
    Erase RResult
  End Sub
  Sub FarbwerteCalc(ByRef Refnr() As String, Winkel As AngGeos, ByRef GrpRwerte As RefValuesGrp, ByRef Farbwerte As ValuesGrpsAssigns, ByRef ier As Integer)
    Dim k As Integer
    Dim i As Integer
    Dim NumRw As Integer
    Dim GrpFaRwerte As New RefValuesGrp
    NumRw = UBound(Refnr) + 1
    GrpFaRwerte.Add("W", New RefValues)
    GrpFaRwerte.Add("S", New RefValues)
    For k = 0 To GrpRwerte.Count - 1
      If IsNothing(GrpRwerte) OrElse GrpRwerte.Count = 0 Then Exit Sub
      If IsNothing(GrpRwerte(k)) OrElse GrpRwerte(k).Count = 0 Then Exit Sub
      GrpFaRwerte(k).clear()
      For i = 0 To NumRw - 1
        'If Not IsNothing(Refnr(i)) Then
        GrpFaRwerte(k).Add(KeyRe(i), GrpRwerte(k)(Refnr(i)))
        GrpFaRwerte(k)(KeyRe(i)).QuControl = New QuControls
        GrpFaRwerte(k)(KeyRe(i)).kwb = k
        GrpFaRwerte(k)(KeyRe(i)).Nr = i
        If i = 0 Then
          GrpFaRwerte(k)(KeyRe(i)).QuControl.Cart = "@T$" & GrpRwerte.RwArt(k)
        Else
          GrpFaRwerte(k)(KeyRe(i)).QuControl.Cart = "@P$" & GrpRwerte.RwArt(k)
        End If
      Next i
    Next k
    '
    '
    Farbwerte.clear()
    AufbauPar.AufbauAnwsgMerk(MenueParam.UserID, MenueParam.MethID, Farbwerte, ier)
    If Farbwerte.Count > 0 Then
      Call CalcFarbWerte(Winkel, GrpFaRwerte, Farbwerte, ier)
    Else
      '
      '
      Farbwerte.clear()
      AufbauPar.AufbauRezeptMerk(Farbwerte, ier)

      Call FarbWrtCalc(Winkel, GrpFaRwerte, Farbwerte, ier)
      
    End If

  End Sub


  Sub dispose() Implements IDisposable.Dispose
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Public Sub New()
    MyBase.New()
  End Sub
End Class