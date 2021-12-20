


<ComClass(MankiewiczRezDruck.ClassId, MankiewiczRezDruck.InterfaceId, MankiewiczRezDruck.EventsId)> _
Public Class MankiewiczRezDruck

#Region "COM-GUIDs"
  ' Diese GUIDs stellen die COM-Identität für diese Klasse 
  ' und ihre COM-Schnittstellen bereit. Wenn Sie sie ändern, können vorhandene 
  ' Clients nicht mehr auf die Klasse zugreifen.
  Public Const ClassId As String = "cd0916b3-2bf2-4e16-a76f-7f333b5e7595"
  Public Const InterfaceId As String = "76e47b68-de71-4fea-8c79-cab997ae9fe9"
  Public Const EventsId As String = "e4a884e6-ba26-45d6-b73f-5dc7944dabbe"
#End Region

  ' Eine erstellbare COM-Klasse muss eine Public Sub New() 
  ' ohne Parameter aufweisen. Andernfalls wird die Klasse 
  ' nicht in der COM-Registrierung registriert und kann nicht 
  ' über CreateObject erstellt werden.



  '
  '

  Dim MnPlDr As HandlePlottDruck
  Dim Zeil As ZeilPrint
  Dim GrLeft As Single = 40
  Dim GrTop As Single = 50
  Dim GrWidth As Single = 750
  Dim GrHeight As Single = 1137
  Dim Strueb As String
  Dim dy As Single
  Dim Faid As Integer
  Dim Keyd As String
  '
  '
  'Druckcharacter
  '
  '01 ==> Anteil(alt) Pigment
  '02 ==> Anteil(neu) Pigment
  '03 ==> Anteil(alt) Paste
  '04 ==> Anteil(neu) Paste
  '05 ==> Menge(alt)  Paste
  '06 ==> Menge(neu)  Paste
  '07 ==> Zuwaagemenge(Paste)
  '08 ==> Neu+Zuwaage (Paste) 
  '09 ==> Prozentigkeit
  '10 ==> Bindemittel-Prozentigkeit
  '11 ==> spez. Gewicht
  '12 ==> Farbstärke
  '13 ==> Anteil
  '14 ==> Menge
  '
  Dim Drubild() As String = {"01", "05"}


  Protected Overrides Sub Finalize()

    MyBase.Finalize()



  End Sub
  Public Sub New()
    MyBase.New()
    Zeil = New ZeilPrint
    Zeil.FontZ = RezFont(2)
    Zeil.BrushZ = New SolidBrush(Color.Black)
    dy = Zeil.FontZ.Height

  End Sub
  '
  '
  'Standards für Graphicobjecte
  '
  '
  '
  '
  Property PlDr As HandlePlottDruck
    Get
      PlDr = MnPlDr
    End Get
    Set(value As HandlePlottDruck)
      MnPlDr = value
    End Set
  End Property



  '
  'Ausdruck des Wiegeschein - Rezeptes (MnKeynam)
  '
  Sub MankDruckWiegeschein(ByVal sender As Object, ByVal ev As PrintPageEventArgs)


    '
    '
    '
    '#######MANKISPEZIAL
    '
    '
    '
    '

    If Zeil.RecPrintCount = 0 Then

      Zeil.Graphbounds = New RectangleF(GrLeft, GrTop, GrWidth, GrHeight)

      Call MankDruWiegRez(MnPlDr.KeyNam, MnPlDr.RefNr, ev.Graphics)
    End If
    Call Zeil.DruckEventRecprint(sender, ev)
  End Sub
  '
  '
  '
  '
  Sub MankDruWiegRez(ByRef RzNr As String, ByRef RefNr() As String, ByVal Graph As Graphics)
    '
    '
    '
    '#######MANKISPEZIAL
    '
    '
    '
    '
    Dim X As Single
    Dim Y As Single
    Dim kwb As Short
    '
    '
    Call MnPlDr.DrRezKopf(X, Y, dy, Zeil, Graph)
    If MnPlDr.ier <> 0 Then Exit Sub
    Y = Y + dy
    '
    '
    'Rezeptname
    '
    '
    Strueb = Texxt(888) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, Strueb, Graph))

    Strueb = Trim(MnPlDr.AllRezepte.Rezepte(RzNr).Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, Strueb, Graph))

    '
    'Vorlage
    '
    '
    Strueb = Texxt(861) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, Strueb, Graph))

    Strueb = Trim(MnPlDr.GrpRwerte(0)("V").Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, Strueb, Graph))

    '
    'Untergrund
    '
    '
    If MnPlDr.GrpRwerte.Count > 0 Then
      Strueb = Texxt(890) & ":"
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, Strueb, Graph))

      Strueb = Trim(MnPlDr.GrpRwerte(0).RefUnt.Name)
      Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, Strueb, Graph))

    End If
    '
    'Rezept Drucken
    '
    '
    '
    kwb = MnPlDr.GrpRwerte(0)(0).kwb
    Call MankDrRezDru(2, X, Y, dy, RzNr, kwb, MnPlDr.AllRezepte, Zeil, Graph)
    '
    MnPlDr.MakeFarbWrt(X, Y, dy, Texxt(875), RzNr, RefNr, MnPlDr.GrpRwerte, MnPlDr.FarbWerte, Zeil, Graph)
    '
    If Y + 7 * dy > Zeil.Height Then
      Zeil.Add(Zeil.NewPage)
      Y = 0
    End If
    Y = Y + 4 * dy
    '
    '
    '
    If MnPlDr.DrPar Then
      Call MnPlDr.DruckParameter(Y, Zeil.Width, dy, Zeil, Graph)
    End If
  End Sub
  '
  '
  '
  '
  Sub MankDrRezDru(ByRef Isch As Short, ByRef X0 As Single, ByRef Y As Single, ByRef dy As Single, ByRef RzNr As String, ByRef kwb As Short, ByRef RezSozpt As RecipesGrp, ByRef Zeil As ZeilPrint, ByRef Graph As Graphics)
    '
    '
    '
    '#######MANKISPEZIAL
    '
    '
    '
    '
    Dim RzKey() As String
    Dim i As Short
    Dim GesMeng As Single
    Dim GesPreis As Single
    Dim X As Single
    Dim Y0 As Single
    Dim Kenn(1) As String
    With RezSozpt
      X = X0
      GesMeng = 0.0#
      GesPreis = 0.0#
      For i = 0 To .Rezepte(RzNr).KF - 1
        Faid = .Rezepte(RzNr)(i).ID
        Keyd = KeyName(Faid)
        GesPreis = GesPreis + .Rezepte(RzNr)(i).FaAmng * .Farben(Keyd).Preis
        GesMeng = GesMeng + .Rezepte(RzNr)(i).FaAmng
      Next i
      If GesMeng = 0.0# Then Exit Sub
      GesMeng = GesMeng / 100.0#
      Strueb = Texxt(388)
      If .Rezepte(RzNr).Sorkrit(0).Count > 0 Then
        Y = Y + dy
        Kenn(0) = MenueParam.Normfa(0).NormKenn
        If MenueParam.Normfa.Nlz = 1 Then
          Kenn(1) = Space(6)
        Else
          Kenn(1) = MenueParam.Normfa(1).NormKenn
        End If


        Strueb = Strueb & Space(2) & Format(.Rezepte(RzNr).Nr, "00")

        Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, Strueb, Graph))
        '
        '
        '
        X = 300
        Y0 = Y
        Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21001), "----------------", Format(.Rezepte(RzNr).Dicke(0), "###0.00")}, Zeil, Graph)
        Y = Y0
        X = X + 70
        Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21000), "----" & Texxt(891 + kwb) & "--------", Format(.Rezepte(RzNr).Sorkrit(0)(KeyRe(1)), "###0.00")}, Zeil, Graph)
        Y = Y0
        X = X + 70
        Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & Kenn(0), "----" & Texxt(891 + kwb) & "--------", Format(.Rezepte(RzNr).Sorkrit(0)(KeyRe(5)), "###0.00")}, Zeil, Graph)
        Y = Y0
        X = X + 70
        Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & Kenn(1), "----" & Texxt(891 + kwb) & "--------", Format(.Rezepte(RzNr).Sorkrit(0)(KeyRe(6)), "###0.00")}, Zeil, Graph)
        Y = Y0
        X = X + 70
        Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21002), "----------------", Format(.Rezepte(RzNr).Sorkrit(0)(KeyRe(3)), "###0.00")}, Zeil, Graph)
        Y = Y0
        X = X + 70
        Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21003), "----------------", Format(GesPreis, "######0.00")}, Zeil, Graph)

        X = 0.0
        Strueb = Sline(255, "-")
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, Strueb, Graph))
      End If
    End With
    Y = Y + dy
    '
    '
    '
    Drubild = GetPrivSettings("MANKIEWICZ", "DRUREZ", "01,02", COLORFileName()).Split(",")
    ReDim RzKey(0)
    RzKey(0) = RzNr
    Call MnPlDr.AnteilMenge(X0, Y, dy, RzKey, RezSozpt, Zeil, Graph, Drubild)

    Y = Y + dy

  End Sub


  '
  'Ausdruck der Rezeptkorrektur - Rezepte (RzNr)
  '
  '
  '
  'Mankiewicz
  '
  '
  Sub MankDruckKorrekturRezept(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    '
    '
    '
    '#######MANKISPEZIAL
    '
    '
    '
    '
    If Zeil.RecPrintCount = 0 Then
      'GraphBounds = New RectangleF(ev.MarginBounds.Left, ev.MarginBounds.Top, ev.MarginBounds.Width, ev.MarginBounds.Height)
      Zeil.Graphbounds = New RectangleF(GrLeft, GrTop, GrWidth, GrHeight)

      Call MankDruKorrRez(MnPlDr.KeyNam, MnPlDr.RzName, MnPlDr.RefNr, MnPlDr.AllRezepte, MnPlDr.GrpRwerte, MnPlDr.ZusatzName, MnPlDr.ZusatzMenge, ev.Graphics)
    End If
    Call Zeil.DruckEventRecprint(sender, ev)
  End Sub
  '
  '


  Sub MankDruKorrRez(ByRef RzNr As String, ByRef MnRzName() As String, ByRef RefNr() As String, ByRef RezSozpt As RecipesGrp, ByRef GrpRwerte As RefValuesGrp, ByRef MnZusatzName As List(Of String), mnZusatzmenge As List(Of Single), ByVal Graph As Graphics)
    '
    '
    '
    '#######MANKISPEZIAL
    '
    '
    '
    '
    Dim X As Single
    Dim Y As Single
    Dim dx As Single
    Dim ReefNr(1) As String
    Dim Ubo As Short
    Dim kwb As Short
    '
    '
    Call MnPlDr.DrRezKopf(X, Y, dy, Zeil, Graph)
    If MnPlDr.ier <> 0 Then Exit Sub
    Y = Y + dy
    '
    '
    'Rezeptname
    '
    '
    Strueb = Texxt(888) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, Strueb, Graph))

    Strueb = Trim(RezSozpt.Rezepte(RzNr).Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, Strueb, Graph))

    '
    '
    'Vorlage
    '
    '
    Strueb = Texxt(861) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, Strueb, Graph))

    Strueb = Trim(GrpRwerte(0)("V").Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, Strueb, Graph))

    '
    'Nachstellung
    '
    '
    Strueb = Texxt(786) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, Strueb, Graph))

    Strueb = Trim(GrpRwerte(0)("N").Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, Strueb, Graph))

    '
    'Untergrund
    '
    '
    If GrpRwerte.Count > 0 Then
      Strueb = Texxt(890) & ":"
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, Strueb, Graph))

      Strueb = Trim(GrpRwerte(0).RefUnt.Name)
      Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, Strueb, Graph))

    End If
    '
    'Rezept Drucken
    '
    '
    '
    kwb = GrpRwerte(0)(0).kwb
    Call MankDrKorrDru(X, Y, dx, dy, kwb, RezSozpt, Graph)



    Try
      Ubo = UBound(RefNr)
      '
      If Ubo = 2 Then
        ReefNr(0) = RefNr(0)
        ReefNr(1) = RefNr(2)
        MnPlDr.MakeFarbWrt(X, Y, dy, Texxt(875) & " (" & Texxt(877) & ")", MnRzName(0), ReefNr, MnPlDr.GrpRwerte, MnPlDr.FarbWerte, Zeil, Graph)
        ReefNr(0) = RefNr(0)
        ReefNr(1) = RefNr(1)
        MnPlDr.MakeFarbWrt(X, Y, dy, Texxt(875) & " (" & Texxt(876) & ")", MnRzName(1), ReefNr, MnPlDr.GrpRwerte, MnPlDr.FarbWerte, Zeil, Graph)
      ElseIf Ubo = 1 Then
        MnPlDr.MakeFarbWrt(X, Y, dy, Texxt(875), MnRzName(0), RefNr, MnPlDr.GrpRwerte, MnPlDr.FarbWerte, Zeil, Graph)
      End If
      '
      '
      '
      '
    Catch

    End Try
    If Y + 7 * dy > Zeil.Height Then
      Zeil.Add(Zeil.NewPage)
      Y = 0
    End If
    Y = Y + 4 * dy
    Call MnPlDr.DruckParameter(Y, dx, dy, Zeil, Graph)
  End Sub
  '
  '
  '
  'Ausdruck aller Rezepte
  '
  '
  Sub MankDruckRezAlle(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    If Zeil.RecPrintCount = 0 Then
      Zeil.Graphbounds = New RectangleF(GrLeft, GrTop, GrWidth, GrHeight)
      Call MankDruRezAll(Zeil, ev.Graphics)
    End If
    Call Zeil.DruckEventRecprint(sender, ev)
  End Sub

  '
  Sub MankDruRezAll(ByRef Zeil As ZeilPrint, ByRef Graph As Graphics)
    Dim strueb As String
    Dim X As Single
    Dim Y As Single
    Dim k As Integer
    Dim ier As Short
    Dim kwb As Short
    Dim Reznam As String
    Dim Refnam() As String
    '
    '
    Call PlDr.DrRezKopf(X, Y, dy, Zeil, Graph)
    If ier <> 0 Then Exit Sub
    '
    'Sortimentname
    '
    '
    strueb = Texxt(850) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

    strueb = PlDr.AllRezepte.Rezepte("SOR").Name
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    '
    '
    '
    'Vorlage
    '
    '
    strueb = Texxt(861) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

    strueb = Trim(PlDr.GrpRwerte(0)("V").Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    '
    'Untergrund
    '
    '
    If PlDr.GrpRwerte.Count > 0 Then
      strueb = Texxt(890) & ":"
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

      strueb = Trim(PlDr.GrpRwerte(0).RefUnt.Name)
      Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    End If

    Y = Y + dy
    Call PlDr.DrSorDru(X, Y, dy, PlDr.AllRezepte, Zeil, Graph)
    Y = Y + dy
    ReDim Refnam(1)
    Refnam(0) = "V"
    For k = 0 To PlDr.Plott.Ksrt.Count - 1
      Reznam = KeyRe(PlDr.Plott.Ksrt(k))
      Refnam(1) = Reznam
      '
      If (Y + 10 * dy + PlDr.Winkel.Km * (MenueParam.Normfa.Nlz + 3) * dy + PlDr.AllRezepte.Rezepte(Reznam).KF * dy) > Zeil.Height Then
        Zeil.Add(Zeil.NewPage)
        Y = 0
      End If
      '
      '
      '
      'Rezept drucken
      '
      '
      '
      '
      '
      Y = Y + 2 * dy
      kwb = PlDr.GrpRwerte(0)(0).kwb
      Call MankDrRezDru(1, X, Y, dy, Reznam, kwb, PlDr.AllRezepte, Zeil, Graph)
      '
      '
      '
      PlDr.MakeFarbWrt(X, Y, dy, Texxt(875), Reznam, Refnam, PlDr.GrpRwerte, PlDr.FarbWerte, Zeil, Graph)

    Next k

  End Sub
  '
  '
  Sub MankDrKorrDru(ByRef X0 As Single, ByRef Y As Single, ByRef dx As Single, ByRef dy As Single, ByRef kwb As Short, ByRef RezSozpt As RecipesGrp, ByVal Graph As Graphics)
    '
    '
    '
    '#######MANKISPEZIAL
    '
    '
    '
    '
    
    Dim i As Short
    Dim j As Short
    Dim Y0 As Single
    Dim index As Integer
    Dim MxMe As Short
    Dim Yzus As Single
    Dim GesMeng() As Single
    Dim GesPreis() As Single
    Dim GesVol() As Single
    Dim GesDichte() As Single
    Dim X As Single
    Dim Kenn(2) As String
    Dim TxtMetam As String
    Dim TxtNl1 As String
    Dim TxtNl2 As String
    'Dim Fontalt As Font
    'Dim FontNeu As Font
    '
    'Fontalt = Zeil.FontZ.Clone

    'FontNeu = New Font(Zeil.FontZ.FontFamily, Zeil.FontZ.Size, FontStyle.Italic)
    X = X0
    Yzus = MnPlDr.ZusatzMenge.Count * dy
    MxMe = UBound(MnPlDr.RzName) + 1
    ReDim GesMeng(MxMe - 1)
    ReDim GesPreis(MxMe - 1)
    ReDim GesVol(MxMe - 1)
    ReDim GesDichte(MxMe - 1)
    For j = 0 To MxMe - 1
      GesMeng(j) = 0.0#
      GesPreis(j) = 0.0
      GesVol(j) = 0.0
      For i = 0 To RezSozpt.Rezepte(MnPlDr.RzName(j)).KF - 1
        Faid = RezSozpt.Rezepte(MnPlDr.RzName(j))(i).ID
        Keyd = KeyName(Faid)
        GesMeng(j) = GesMeng(j) + RezSozpt.Rezepte(MnPlDr.RzName(j))(i).FaAmng
        GesVol(j) = GesVol(j) + RezSozpt.Rezepte(MnPlDr.RzName(j))(i).FaAmng / RezSozpt.Farben(Keyd).Spz
        GesPreis(j) = GesPreis(j) + RezSozpt.Rezepte(MnPlDr.RzName(j))(i).FaAmng * RezSozpt.Farben(Keyd).Preis
      Next i
      GesDichte(j) = GesMeng(j) / (GesVol(j) + 1.0E-20)
      GesMeng(j) = GesMeng(j) / 100.0#
    Next j
    If RezSozpt.Rezepte(MnPlDr.RzName(0)).KF > 0 Then
      If MxMe > 1 Then
        Kenn(2) = TexKt(21000)
        Kenn(0) = MenueParam.Normfa(0).NormKenn
        If MenueParam.Normfa.Nlz = 1 Then
          Kenn(1) = Space(6)
        Else
          Kenn(1) = MenueParam.Normfa(1).NormKenn
        End If
        TxtMetam = Format(RezSozpt.Rezepte(MnPlDr.RzName(1)).Sorkrit(0)(1), "###0.00")
        TxtNl1 = Format(RezSozpt.Rezepte(MnPlDr.RzName(1)).Sorkrit(0)(5), "###0.00")
        TxtNl2 = Format(RezSozpt.Rezepte(MnPlDr.RzName(1)).Sorkrit(0)(6), "###0.00")
        If CSng(TxtMetam) < 0 Then
          TxtMetam = "0,00"
        End If
        If CSng(TxtNl2) < 0 Then
          TxtNl2 = "    "
        End If
        index = 1
      Else
        index = 0
        Kenn(0) = Space(8)
        Kenn(1) = Space(8)
        Kenn(2) = Space(8)
        TxtMetam = Space(7)
        TxtNl1 = Space(7)
        TxtNl2 = Space(7)
      End If



      X = 300
      Y0 = Y
      Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21001), "----------------", Format(RezSozpt.Rezepte(MnPlDr.RzName(index)).Dicke(0), "###0.00")}, Zeil, Graph)
      Y = Y0
      X = X + 70
      Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & Kenn(2), "----" & Texxt(891 + kwb) & "--------", TxtMetam}, Zeil, Graph)
      Y = Y0
      X = X + 70
      Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & Kenn(0), "----" & Texxt(891 + kwb) & "--------", TxtNl1}, Zeil, Graph)
      Y = Y0
      X = X + 70
      Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & Kenn(1), "----" & Texxt(891 + kwb) & "--------", TxtNl2}, Zeil, Graph)
      Y = Y0
      X = X + 70
      Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & TexKt(20999), "----------------", Format(GesDichte(index), "###0.000")}, Zeil, Graph)
      Y = Y0
      X = X + 70
      Call PlDr.BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21003), "----------------", Format(GesPreis(index), "######0.00")}, Zeil, Graph)

      X = 0.0
      Strueb = Sline(255, "-")
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, Strueb, Graph))
    End If

    '
    '
    Y = Y + dy
    Drubild = GetPrivSettings("MANKIEWICZ", "DRUKOR", "01,02", COLORFileName()).Split(",")
    '
    Call MnPlDr.AnteilMenge(X0, Y, dy, MnPlDr.RzName, RezSozpt, Zeil, Graph, Drubild)

    Y = Y + dy
  End Sub

 



End Class


