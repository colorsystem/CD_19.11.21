Option Compare Text
Option Strict Off
Option Explicit On

'
Friend Module modMankiewicz
  '
  '
  '

  '
  '
  Friend Quali As QualKontrolle
  Friend FormMDI As frmMankiewMDI
  Friend MischTable As DataTable
  Friend SortiTable As DataTable
  Dim TestText As String
  '
  '
  '
  Friend ReWrRwert As ReadWriteRwert
  Friend RwWrSortim As ReadWriteSortiment
  Friend RwWrRezept As ReadWriteRezept
  Friend printdoc As PrintDocument
  Friend Measure As MeasureReflex
  Friend Warn As Short = 0
  Friend SortID As Integer
  Friend GetRwerte As HandleRwerte
  Public BtnList As List(Of Button)
  Public BasisProg As clsColorBasis
  Public pd As New PrintDialog
  Public RezAusgabe As Object

  '
  '
  '
  '
 
  
  Sub main()
   
    FormMDI = New frmMankiewMDI
    TestText = Texxt(0)
    RezAusgabe = RezeptAusgabe()
    Application.Run(FormMDI)
    Application.DoEvents()
    Application.Exit()
  End Sub
  Function RezeptAusgabe() As Object
    Dim Objectstring As String = "MankiewiczAusgabe.MankiewiczRezDruck"
    '
    '
    '
    Try
      RezeptAusgabe = CreateObject(Objectstring)
    Catch ex As Exception
      MsgBox(Err.Description & ": " & Objectstring)
      RezeptAusgabe = Nothing
      Exit Function
    End Try

  End Function

  Sub ClearRezepte(ByRef Rezpte As RecipesGrp)
    Dim i As Short
    '
    '
    'Vorhandene Farbmittel in Rezepten Löschen
    '
    '
    For i = 0 To Rezpte.Rezepte.RezCount - 1
      Rezpte.Rezepte(i).clear()
      Rezpte.Rezepte(i).Name = ""
    Next i
    '
    'Vorhandene Farb-/Bindemittel löschen
    '
    '
    Rezpte.Farben.clear()
    ''
  End Sub
  Sub RezeptNewMenge(ByVal Ivol As Integer, ByVal NewMenge As Single, ByVal umr As RezeptUmrechnung, ByVal Rkey() As String, ByVal Rezsozpt As RecipesGrp, ByRef ier As Integer)
    Dim i As Integer
    Dim Rezk As String
    Dim Fakt As Single
    Dim HlfMenge As Single
    Dim OldMenge As Single
    Dim fkkkt As Single
    Dim Eps As Single = 0.0000001
    ier = 0
    '
    '
    '
    '
    'Umrechnung in gravimetrisch aus neuem Gesamtvolumen bzw. neuem Gesamtgewicht 
    '
    '
    '
    '
    '
    '
    'Mengen in Volumenangabe (Ivol=1) bzw. Gewichte (Ivol=0) umrechnen
    '
    '
    Rezsozpt.IVOL = Ivol
    For i = 0 To UBound(Rkey)
      If i = 0 Then
        OldMenge = umr.MngINF(Rkey(i), Rezsozpt, ier)
        If OldMenge < Eps Then
          MsgBox(Texxt(4071))
          ier = 4071
          Exit Sub
        End If
        fkkkt = NewMenge / (OldMenge + Eps)
      End If
      Select Case Rkey(i)
        Case "MNG"
          Rezk = "MNG"
          Call umr.CalcBamng(Rezk, Rezsozpt, ier)
          HlfMenge = umr.MngINF(Rezk, Rezsozpt, ier)
          If HlfMenge < Eps Then
            MsgBox(Texxt(4071))
            ier = 4071
            Exit Sub
          End If
          Fakt = NewMenge / (HlfMenge + Eps)
          Call umr.MNGBamngNorm(Rezk, Rezsozpt, NewMenge, ier)
          Call umr.CalcFamng(Rezk, Rezsozpt, ier)
          '
          '
        Case "KOR"
          Rezk = "KOR"
          HlfMenge = Fakt * umr.MngINF(Rezk, Rezsozpt, ier)
          Call umr.MNGBamngNorm(Rezk, Rezsozpt, HlfMenge, ier)
          Call umr.CalcFamng(Rezk, Rezsozpt, ier)
          '
          '
          '
          '
        Case "MZU"
          Rezk = "MZU"
          HlfMenge = Fakt * umr.MngINF(Rezk, Rezsozpt, ier)
          Call umr.MNGBamngNorm(Rezk, Rezsozpt, HlfMenge, ier)
          Call umr.CalcFamng(Rezk, Rezsozpt, ier)
          '
          '
          '
        Case "ZUW"
          Rezk = "ZUW"
          HlfMenge = Fakt * umr.MngINF(Rezk, Rezsozpt, ier)
          Call umr.MNGBamngNorm(Rezk, Rezsozpt, HlfMenge, ier)
          Call umr.CalcFamng(Rezk, Rezsozpt, ier)
        Case Else
          Rezk = Rkey(i)
          Call umr.CalcBamng(Rezk, Rezsozpt, ier)
          HlfMenge = umr.MngINF(Rezk, Rezsozpt, ier)
          If HlfMenge < Eps Then
            MsgBox(Texxt(4071))
            ier = 4071
            Exit Sub
          End If
          Fakt = NewMenge / HlfMenge
          Call umr.MNGBamngNorm(Rezk, Rezsozpt, NewMenge, ier)
          Call umr.CalcFamng(Rezk, Rezsozpt, ier)

          '
      End Select
    Next i
    Rezsozpt.IVOL = 0
    For i = 0 To UBound(Rkey)
      Select Case Rkey(i)
        Case "MNG"
          Call umr.CalcBamng("MNG", Rezsozpt, ier)
        Case "KOR"
          Call umr.CalcBamng("KOR", Rezsozpt, ier)
        Case "MZU"
          Call umr.CalcBamng("MZU", Rezsozpt, ier)
        Case "ZUW"
          Call umr.CalcBamng("ZUW", Rezsozpt, ier)
      End Select
    Next i

    For i = 0 To Rezsozpt.Farben.FarbCount - 1
      Rezsozpt.Farben(i).BoMng = Rezsozpt.Farben(i).BoMng * fkkkt
      Rezsozpt.Farben(i).BuMng = Rezsozpt.Farben(i).BuMng * fkkkt

    Next (i)
    Rezsozpt.MngMax = Rezsozpt.MngMax * NewMenge / (OldMenge + Eps)
    Rezsozpt.MngMin = Rezsozpt.MngMin * NewMenge / (OldMenge + Eps)
  End Sub
  
  

  Sub RwertGridTable(ByRef MessgName As String, ByVal Winkel As AngGeos, ByRef ReflWerte As RefValues, ByRef RefTabWerte As DataTable, ByRef chkwinkel As List(Of CheckBox), ByRef gridView As DataGridView)
    Dim i As Integer
    Dim k As Integer
    Dim l As Integer
    Dim row As DataRow
    Dim CHRM As String
    '
    '

    '
    '
    RefTabWerte.Rows.Clear()
    RefTabWerte.Columns.Clear()
    RefTabWerte.Columns.Add(Texxt(3659))
    RefTabWerte.Columns.Add(Texxt(824))
    RefTabWerte.Columns.Add(Texxt(916))
    RefTabWerte.Columns.Add(Texxt(375))
    RefTabWerte.Columns.Add(MessgName)

    For i = 0 To Winkel.Wsol.Nwe - 1
      RefTabWerte.Columns.Add(CStr(Winkel.Wsol.R(i)))
    Next i
    '
    'Wellenlängen
    '
    row = RefTabWerte.NewRow
    row(0) = Texxt(3659)
    row(1) = Texxt(824)
    row(2) = Texxt(916)
    row(3) = Texxt(375)
    row(4) = MessgName

    For i = 0 To Winkel.Wsol.Nwe - 1
      row(5 + i) = CStr(Winkel.Wsol.R(i))
    Next
    '
    RefTabWerte.Rows.Add(row)
    '
    '
    l = 0
    For k = 0 To ReflWerte.Count - 1
      If ReflWerte(k).RefKurv.Count > 0 Then
        For kw = 0 To Winkel.Km - 1
          If chkwinkel(kw).Checked Then
            row = RefTabWerte.NewRow
            l = l + 1
            If kw = 0 Then
              row(0) = ReflWerte(k).Tag
              row(1) = ReflWerte(k).Name
              row(2) = ReflWerte(k).Bem
              row(3) = ReflWerte(k).DatTim
            Else
              row(0) = Space(1)
              row(1) = Space(1)
              row(2) = Space(1)
              row(3) = Space(1)
            End If
            CHRM = Winkel(kw).Chrm
            row(4) = CHRM
            For i = 0 To Winkel.Wsol.Nwe - 1
              row(5 + i) = Format(100.0# * ReflWerte(k).RefKurv(CHRM).R(i), "###.000")
            Next i
            RefTabWerte.Rows.Add(row)
          End If
        Next kw
      End If
    Next k
  End Sub
  '
  '
  Sub FarbWertGridTable(ByVal KeyAnwsg As String, ByRef ReflWerte As RefValues, ByRef FarbMerkmale As ValuesGrpsAssigns, ByRef FarbTabWerte As DataTable, ByRef chkLichtart As List(Of CheckBox), ByRef chkWinkel As List(Of CheckBox), ByRef flgFarbwerte As DataGridView)
    Dim j As Integer
    Dim i As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim MeKen As String
    Dim Row As DataRow
    If IsNothing(FarbTabWerte) Then Exit Sub
    FarbTabWerte.Rows.Clear()
    FarbTabWerte.Columns.Clear()
    If IsNothing(KeyAnwsg) Then Exit Sub
    If Not FarbMerkmale.ContainsAnwsg(KeyAnwsg) Then
      Exit Sub
    End If
    If ReflWerte.Count = 0 Then
      Exit Sub
    End If
    FarbTabWerte.Columns.Add("NAME", GetType(String))
    FarbTabWerte.Columns(FarbTabWerte.Columns.Count - 1).Caption = "NAME"
    FarbTabWerte.Columns.Add("BEM", GetType(String))
    FarbTabWerte.Columns(FarbTabWerte.Columns.Count - 1).Caption = "BEM"
    FarbTabWerte.Columns.Add("CME", GetType(String))
    FarbTabWerte.Columns(FarbTabWerte.Columns.Count - 1).Caption = "CME"
    FarbTabWerte.Columns.Add("GID", GetType(String))
    FarbTabWerte.Columns(FarbTabWerte.Columns.Count - 1).Caption = "GID"


    For i = 0 To FarbMerkmale(KeyAnwsg).CountMerk - 1
      FarbTabWerte.Columns.Add(FarbMerkmale(KeyAnwsg).Merk(i).Ken)
      FarbTabWerte.Columns(FarbTabWerte.Columns.Count - 1).Caption = FarbMerkmale(KeyAnwsg).Merk(i).Kbez
    Next
    For i = 0 To MenueParam.Normfa.Nlz - 1
      If chkLichtart(i).Checked Then
        For kw = 0 To MenueParam.User.Winkel.Km - 1
          If chkWinkel(kw).Checked Then
            '
            'Prüfen, ob Werte für Merkmale vorhanden
            '
            '
            For k = 0 To FarbMerkmale(KeyAnwsg).Count - 1
              For j = 0 To FarbMerkmale(KeyAnwsg).CountMerk - 1
                MeKen = FarbMerkmale(KeyAnwsg).Merk(j).Ken
                If Not IsDBNull(FarbMerkmale(KeyAnwsg)(k)(i)(kw)(MeKen)) Then
                  Exit For
                End If
              Next j
              If j < FarbMerkmale(KeyAnwsg).CountMerk Then
                Exit For
              End If
            Next k
            If k >= FarbMerkmale(KeyAnwsg).Count Then
              Continue For
            End If
            '
            'Leerzeile
            '
            Row = FarbTabWerte.NewRow
            For j = 0 To FarbTabWerte.Columns.Count - 1
              If FarbTabWerte.Columns(j).DataType = GetType(String) Then
                Row(j) = " "
              End If
            Next
            FarbTabWerte.Rows.Add(Row)
            '
            'Normlichtart
            '
            '
            Row = FarbTabWerte.NewRow
            For j = 0 To FarbTabWerte.Columns.Count - 1
              If FarbTabWerte.Columns(j).DataType = GetType(String) Then
                Row(j) = " "
              End If
            Next
            Row(0) = MenueParam.Normfa(i).NormNama & " (" & MenueParam.User.Winkel(kw).IhrmBez & ")"
            FarbTabWerte.Rows.Add(Row)
            If flgFarbwerte.Rows.Count = FarbTabWerte.Rows.Count Then
              flgFarbwerte.Rows(FarbTabWerte.Rows.Count - 1).Cells(0).Style.Font = New Font(flgFarbwerte.DefaultCellStyle.Font, FontStyle.Bold)
            End If
            '
            'Merkmale der einzelnen Proben
            '
            For k = 0 To FarbMerkmale(KeyAnwsg).Count - 1
              Row = FarbTabWerte.NewRow
              Row("NAME") = FarbMerkmale(KeyAnwsg)(k).Name
              Row("BEM") = FarbMerkmale(KeyAnwsg)(k).Bem
              Row("CME") = ReflWerte(k).Cme
              Row("GID") = ReflWerte(k).Gid
              For j = 0 To FarbMerkmale(KeyAnwsg).CountMerk - 1
                '
                '
                'Merkmale in Tabelle
                '
                MeKen = FarbMerkmale(KeyAnwsg).Merk(j).Ken
                If Not IsDBNull(FarbMerkmale(KeyAnwsg)(k)(i)(kw)(MeKen)) Then
                  If FarbMerkmale(KeyAnwsg).Merk(j).Typ = "N" Then
                    Row(MeKen) = Format(CSng(FarbMerkmale(KeyAnwsg)(k)(i)(kw)(MeKen)), FarbMerkmale(KeyAnwsg).Merk(j).Form)
                  Else
                    Row(MeKen) = FarbMerkmale(KeyAnwsg)(k)(i)(kw)(MeKen)
                  End If
                Else
                  Row(MeKen) = " "
                End If
              Next j
              FarbTabWerte.Rows.Add(Row)
            Next k
          End If
        Next kw
      End If
    Next i
    '
    If flgFarbwerte.Columns.Count <> FarbTabWerte.Columns.Count Then
      Exit Sub
    End If
    '
    'Gitternetz'
    '
    '
    '
    'Name
    '
    If flgFarbwerte.Columns.Count = 0 Then Exit Sub
    flgFarbwerte.Columns(0).HeaderText = Texxt(2206)
    flgFarbwerte.Columns(0).Width = 175
    flgFarbwerte.Columns(0).ReadOnly = True
    flgFarbwerte.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgFarbwerte.Columns(0).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgFarbwerte.Columns(0).SortMode = DataGridViewColumnSortMode.NotSortable
    '
    flgFarbwerte.Columns(1).Visible = False
    flgFarbwerte.Columns(2).Visible = False
    flgFarbwerte.Columns(3).Visible = False
    flgFarbwerte.Columns(1).Width = 0.0
    flgFarbwerte.Columns(2).Width = 0.0
    flgFarbwerte.Columns(3).Width = 0.0

    '
    '
    For i = 0 To FarbMerkmale(KeyAnwsg).CountMerk - 1
      flgFarbwerte.Columns(i + 4).HeaderText = Trim(FarbMerkmale(KeyAnwsg).Merk(i).Kbez)
      flgFarbwerte.Columns(i + 4).Width = 50
      flgFarbwerte.Columns(i + 4).ReadOnly = True
      flgFarbwerte.Columns(i + 4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      flgFarbwerte.Columns(i + 4).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
      flgFarbwerte.Columns(i + 4).SortMode = DataGridViewColumnSortMode.NotSortable
    Next i



  End Sub



  Sub RwertGridTableOnlyGrid(ByRef MessgName As String, ByRef Winkel As AngGeos, ByRef ReflWerte As RefValues, ByVal chkwinkel As List(Of CheckBox), ByVal gridView As DataGridView)
    Dim i As Integer
    Dim k As Integer
    Dim l As Integer
    Dim CHRM As String
    '
    '

    '
    '
    gridView.Rows.Clear()
    gridView.Columns.Clear()
    For i = 0 To Winkel.Wsol.Nwe + 4
      gridView.Columns.Add(KeyRe(i), "")
    Next i
    gridView.Rows.Add()
    gridView.Rows(0).Cells(0).Value = Texxt(3659)
    gridView.Rows(0).Cells(1).Value = Texxt(824)
    gridView.Rows(0).Cells(2).Value = Texxt(916)
    gridView.Rows(0).Cells(3).Value = Texxt(375)
    gridView.Rows(0).Cells(4).Value = MessgName
    '
    'Wellenlängen
    '
    For i = 0 To Winkel.Wsol.Nwe - 1
      gridView.Rows(0).Cells(5 + i).Value = CStr(Winkel.Wsol.R(i))
    Next
    '
    '
    '
    l = 0
    For k = 0 To ReflWerte.Count - 1
      If ReflWerte(k).RefKurv.Count > 0 Then
        For kw = 0 To Winkel.Km - 1
          If chkwinkel(kw).Checked Then
            gridView.Rows.Add()
            l = l + 1
            If kw = 0 Then
              gridView.Rows(l).Cells(0).Value = ReflWerte(k).Tag
              gridView.Rows(l).Cells(1).Value = ReflWerte(k).Name
              gridView.Rows(l).Cells(2).Value = ReflWerte(k).Bem
              gridView.Rows(l).Cells(3).Value = ReflWerte(k).DatTim
            Else
              gridView.Rows(l).Cells(0).Value = Space(1)
              gridView.Rows(l).Cells(1).Value = Space(1)
              gridView.Rows(l).Cells(2).Value = Space(1)
              gridView.Rows(l).Cells(3).Value = Space(1)
            End If
            CHRM = Winkel(kw).Chrm
            gridView.Rows(l).Cells(4).Value = CHRM

            For i = 0 To Winkel.Wsol.Nwe - 1
              gridView.Rows(l).Cells(5 + i).Value = Format(100.0# * ReflWerte(k).RefKurv(CHRM).R(i), "###.000")
            Next i
          End If
        Next kw
      End If
    Next k
  End Sub
  Sub UserDosieranlage(ByVal KeyNeu As String, ByVal RezSozpt As RecipesGrp)
    Dim AnzFarb As Integer
    Dim i As Integer
    Dim KeyID As String
    Dim ProNummer() As String
    Dim ProAlt() As Single
    Dim ProNeu() As Single
    Dim ProZuw() As Single
    Dim FarbID() As Integer
    Dim FarbName() As String
    AnzFarb = RezSozpt.Rezepte(KeyNeu).KF
    If AnzFarb <= 0 Then Exit Sub
    If Not RezSozpt.Rezepte.ContainsKey(KeyNeu) OrElse RezSozpt.Rezepte(KeyNeu).KF = 0 Then Exit Sub
    Dim Objectstring As String = "MankiewiczAusgabe.AusgabeMankiewicz"
    Dim Ausgabeobject As Object
    '
    '
    '
    Try
      Ausgabeobject = CreateObject(Objectstring)
    Catch ex As Exception
      MsgBox(Err.Description & ": " & Objectstring)
      Ausgabeobject = Nothing
      Exit Sub
    End Try
    '
    '
    ReDim ProNummer(AnzFarb - 1)
    ReDim ProAlt(AnzFarb - 1)
    ReDim ProNeu(AnzFarb - 1)
    ReDim ProZuw(AnzFarb - 1)
    ReDim FarbID(AnzFarb - 1)
    ReDim FarbName(AnzFarb - 1)
    For i = 0 To AnzFarb - 1
      ProNeu(i) = -1.0
      ProAlt(i) = -1.0
      ProZuw(i) = -1.0
      KeyID = KeyName(RezSozpt.Rezepte(KeyNeu)(i).ID)
      ProNummer(i) = RezSozpt.Farben(KeyID).PrNr
      FarbID(i) = RezSozpt.Farben(KeyID).ID
      FarbName(i) = RezSozpt.Farben(KeyID).Name
      If RezSozpt.Rezepte.ContainsKey("MNG") Then
        ProAlt(i) = RezSozpt.Rezepte("MNG")(i).BaAmng
      End If
      If RezSozpt.Rezepte.ContainsKey("ZUW") Then
        If Not (Single.IsNaN(ProZuw(i)) Or Single.IsNegativeInfinity(ProZuw(i)) Or Single.IsPositiveInfinity(ProZuw(i))) Then
          ProZuw(i) = RezSozpt.Rezepte("ZUW")(i).BaAmng
        End If
      End If
      ProNeu(i) = RezSozpt.Rezepte(KeyNeu)(i).BaAmng
    Next i
    '
    '
    If KeyNeu = "MZU" Then
      For i = 0 To AnzFarb - 1
        If ProNeu(i) = -1.0 Then
          Ausgabeobject = Nothing
          Exit Sub
        End If
      Next
    Else
      Erase ProZuw
    End If
    '
    Call Ausgabeobject.Dosieren(FarbID, FarbName, ProNummer, ProAlt, ProNeu, ProZuw)
    '
    '
    '
    Ausgabeobject = Nothing
  End Sub

  Function TestUserSettings(StndID As Integer, UserID As Integer, TabNamen() As String) As Boolean
    '
    'Prüfen, ob Einstellungen von AdmID und UserID übereinstimmen
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim l As Integer
    Dim AdaptUser As OleDbDataAdapter
    Dim TablUser As DataTable
    Dim ViewStandard As DataView
    Dim ViewUser As DataView
    AdaptUser = New OleDbDataAdapter
    AdaptUser.SelectCommand = New OleDbCommand("", Cncol)

    '
    TestUserSettings = False
    '
    '
    '
    For i = 0 To TabNamen.Count - 1
      TablUser = New DataTable
      AdaptUser.SelectCommand.CommandText = "SELECT * FROM " & TabNamen(i) & " WHERE USER_ID=" & UserID & " OR USER_ID=" & StndID
      If Not FillDatset(AdaptUser, TablUser) Then
        Exit Function
      End If
      TablUser.AcceptChanges()
      '
      '
      'Einträge überprüfen
      '
      '
      ViewStandard = New DataView(TablUser, "USER_ID=" & StndID, Nothing, DataViewRowState.CurrentRows)
      ViewUser = New DataView(TablUser, "USER_ID=" & UserID, Nothing, DataViewRowState.CurrentRows)
      '
      'Prüfen, ob Anzahl ViewUser und Anzahl Viewadmin übereinstimmt
      'Reihenfolge kann verschieden sein
      '
      '
      If ViewStandard.Count <> ViewUser.Count Then
        Exit Function
      End If

      '

      For j = 0 To ViewUser.Count - 1
        For k = 0 To TablUser.Columns.Count - 1
          If TablUser.Columns(k).ColumnName <> "USER_ID" Then
            If IsDBNull(ViewStandard(j)(k)) Then
              If TablUser.Columns(k).ColumnName = "MESSG_RETR" Then
                ViewStandard(j)(k) = 0
              End If
            End If
            If IsDBNull(ViewUser(j)(k)) Then
              If TablUser.Columns(k).ColumnName = "MESSG_RETR" Then
                ViewUser(j)(k) = 0
              End If
            End If
            If Not (TabNamen(i) = "TBL_USER" AndAlso TablUser.Columns(k).ColumnName = "USER_NAME") Then
              If (IsDBNull(ViewStandard(j)(k)) And IsDBNull(ViewUser(j)(k))) OrElse ViewStandard(j)(k) <> ViewUser(j)(k) Then
                Exit Function
              End If
            End If
          End If
        Next k
        If k < TablUser.Columns.Count Then
          Exit For
        End If
      Next j
      ViewStandard.Dispose()
      ViewUser.Dispose()
      TablUser.Dispose()
    Next i
    AdaptUser.Dispose()
    TestUserSettings = True
  End Function
  Sub SetStandardSettings(StndID As Integer, UserID As Integer, Tabnamen() As String)
    '
    'Prüfen, ob Einstellungen von AdmID und UserID übereinstimmen
    '
    '
    Dim UserName As String
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim Sqlstmt As String
    Dim AdaptUser As OleDbDataAdapter
    Dim CommandUser As OleDbCommand
    Dim TablUser As DataTable
    CommandUser = New OleDbCommand("", Cncol)
    AdaptUser = New OleDbDataAdapter
    AdaptUser.SelectCommand = New OleDbCommand("", Cncol)
    '
    '
    If StndID = UserID Then
      Exit Sub
    End If
    '
    '
    If ConnOpen(Cncol) Then
      For i = Tabnamen.Count - 1 To 0 Step -1
        '
        '
        'Lösche Einträge für UserID
        '
        '
        If Tabnamen(i) = "TBL_USER" Then
          '
          'USER_NAME zwischenspeichern
          '
          '
          TablUser = New DataTable
          AdaptUser.SelectCommand.CommandText = "SELECT * FROM " & Tabnamen(i) & " WHERE USER_ID=" & UserID
          If Not FillDatset(AdaptUser, TablUser) Then
            Cncol.Close()
            Exit Sub
          End If
          UserName = TablUser.Rows(0)("USER_NAME")
          TablUser.Dispose()
        End If
        Sqlstmt = "DELETE * FROM " & Tabnamen(i) & " WHERE USER_ID=" & UserID
        CommandUser.CommandText = Sqlstmt
        If SQLExeNonQuery(CommandUser, Cncol) <> 0 Then
          Cncol.Close()
          Exit Sub
        End If


      Next i
      '
      '
      'Übertrage die Eintröäge von StndID nach UserID
      '
      For i = 0 To Tabnamen.Count - 1
        TablUser = New DataTable
        AdaptUser.SelectCommand.CommandText = "SELECT * FROM " & Tabnamen(i) & " WHERE USER_ID=" & StndID
        If Not FillDatset(AdaptUser, TablUser) Then
          Cncol.Close()
          Exit Sub
        End If
        TablUser.AcceptChanges()
        '
        '
        '
        '
        'StndID durch UserID ersetzen
        '
        '
        '
        AdaptUser.InsertCommand = OleDBInsertCmd(Tabnamen(i), Cncol)
        For j = 0 To TablUser.Rows.Count - 1
          TablUser.Rows(j)("USER_ID") = UserID
          If Tabnamen(i) = "TBL_USER" Then
            TablUser.Rows(j)("USER_NAME") = UserName
          End If
          For k = 0 To TablUser.Columns.Count - 1
            If IsDBNull(TablUser.Rows(j)(k)) Then
              If TablUser.Columns(k).ColumnName = "MESSG_RETR" Then
                TablUser.Rows(j)(k) = 0
              Else
                MsgBox("Field: " & TablUser.Columns(k).ColumnName & " IS DBNULL")
              End If
            End If
            AdaptUser.InsertCommand.Parameters(k).Value = TablUser.Rows(j)(k)
          Next
          If SQLNonQuery(AdaptUser.InsertCommand, Cncol) Then
            MsgBox("error")
          End If
        Next
        '
        '
        '
        '
        TablUser.Dispose()
      Next i
      AdaptUser.Dispose()
      Cncol.Close()
    End If
  End Sub
 
End Module

