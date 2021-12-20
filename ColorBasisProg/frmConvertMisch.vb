Option Explicit On
Option Compare Text
Option Strict Off

Public Class frmConvertMisch
  Inherits Windows.Forms.Form
  Dim DsetRezpt As DataSet
  Dim OleAdRezpt As OleDbDataAdapter
  Dim OleAdSorti As OleDbDataAdapter
  Dim DatReadRezpt As OleDbDataReader
  Dim CmdRezpt As OleDbCommand
  Dim RwWrSortim As ReadWriteSortiment
  Dim ReWrRwert As ReadWriteRwert
  Dim ReWrFarbe As ReadWriteFarbe
  Dim ReWrGrund As ReadWriteGrund
  Dim CalcRezept As RezeptBerechnung
  Dim Umr As RezeptUmrechnung
  Dim TblMisch As DataTable
  Dim OleFarb As OleDbDataAdapter
  Dim CmdFarb As OleDbCommand
  Dim FarbTab As DataTable


  Dim HandleRezept As HandleRezGeneral
  Dim HandleRwrt As HandleRwerte

  '
  '
  '
  Dim FarbenGloss As Colorants
  Dim RezSozpt As RecipesGrp
  Dim GlossFarb As Colorant
  Dim GrpRwerte As RefValuesGrp
  Dim GrpRwrtGloss As RefValuesGrp
  Dim OldMischid As Integer
  Dim SortiID As Integer
  Dim Keymenge As String
  Dim KeySpei As String
  Dim KeyCalc As String
  Dim UntID(1) As Integer
  Dim TypID(1) As Integer
  Dim SmpID(1) As Integer
  Dim RcmdRef As Integer
  Dim GkIDOld As Integer
  Dim NPXOld As Integer
  Dim CDEOld As String

  Private Sub frmConvertMisch_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    btnMessUnt.Text = Texxt(808)
    btnCalculate.Text = Texxt(360)
    btnSpeichern.Text = Texxt(854)
    lblPrefix.Text = Texxt(3820)
    lblDick.Text = Texxt(832)
    lblMSH.Text = Texxt(422)
    lblRestGew.Text = Texxt(172)
    '
    '
    Me.Text = Texxt(1857) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    AddHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID

    '
    '
    OldMischid = -1
    '
    '
    CalcRezept = New RezeptBerechnung
    Umr = New RezeptUmrechnung
    ReWrRwert = New ReadWriteRwert
    ReWrFarbe = New ReadWriteFarbe
    ReWrGrund = New ReadWriteGrund
    GrpRwerte = New RefValuesGrp
    GrpRwerte.Add("W", New RefValues)
    GrpRwerte("W").RefUnt = New RefValue
    GrpRwerte(0).RefUnt.kwb = 0
    GrpRwrtGloss = New RefValuesGrp
    GrpRwrtGloss.Add("W", New RefValues)
    GrpRwrtGloss.Add("S", New RefValues)
    GrpRwrtGloss("S").RefUnt = Nothing
    GrpRwrtGloss("S").Add("V", Nothing)

    '
    GlossFarb = New Colorant
    FarbenGloss = New Colorants
    RezSozpt = New RecipesGrp
    RwWrSortim = New ReadWriteSortiment
    '
    '
    OleFarb = New OleDbDataAdapter
    CmdFarb = New OleDbCommand
    FarbTab = New DataTable
    Call CreateFarbTab(FarbTab, dbgFarbmittel)

    '
    '
    '
    DsetRezpt = New DataSet
    OleAdRezpt = New OleDbDataAdapter
    OleAdSorti = New OleDbDataAdapter
    CmdRezpt = New OleDbCommand
    OleAdRezpt.SelectCommand = New OleDbCommand("", Cncol)
    OleAdSorti.SelectCommand = New OleDbCommand("", Cndat)


    TblMisch = New DataTable
    HandleRezept = New HandleRezGeneral
    OleAdRezpt.SelectCommand.CommandText = HandleRezept.MischSelectCommand

    OleAdRezpt.SelectCommand.Connection = Cncol()
    If Not FillDatset(OleAdRezpt, DsetRezpt, "TblMisch") Then
      Exit Sub
    End If




    DsetRezpt.Tables.Add("TblSorti")

    cboMsh.DataSource = DsetRezpt.Tables("TblMisch")
    cboMsh.DisplayMember = "MISCH_KBEZ"
    cboMsh.ValueMember = "MISCH_ID"
    cboMsh.SelectedIndex = -1
    cboMsh.SelectedValue = MenueParam.MischID

  End Sub
  Private Sub frmConvertMisch_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Resize
    Call ResizeChild(Me)
  End Sub
  Private Sub frmChargenNuancieren_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    RemoveHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
    AufbauPar.MethID = -1
    RwWrSortim.dispose()
    ReWrRwert.dispose()
    ReWrFarbe.dispose()
    ReWrGrund.dispose()
    CalcRezept.dispose()

  End Sub
  '
  '
  Sub cboMSH_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMsh.SelectedIndexChanged
    Dim i As Short
    Dim SqlStmt As String
    If sender.Selectedindex = -1 Then Exit Sub
    If Not IsNumeric(sender.Selectedvalue) Then Exit Sub
    If sender.SelectedValue = OldMischid Then Exit Sub
    OldMischid = sender.SelectedValue
    AufbauPar.MischID = -1

    AufbauPar.MischID = sender.SelectedValue

    SortiID = -1
    '
    '


    DsetRezpt.Tables("TblSorti").Clear()
    If BitWrt(10, MenueParam.User.Writ) Then
      SqlStmt = "SELECT * FROM " & MenueParam.TableSorti & " WHERE USER_ID=" & MenueParam.UserID _
      & " AND MISCH_ID=" & MenueParam.MischID & " ORDER BY SORTI_NAME"
    Else
      SqlStmt = "SELECT * FROM " & MenueParam.TableSorti & " WHERE MISCH_ID=" & MenueParam.MischID & " ORDER BY SORTI_NAME"
    End If
    lstSOR.DataSource = Nothing
    OleAdSorti.SelectCommand.CommandText = SqlStmt
    OleAdSorti.SelectCommand.Connection = Cndat()
    If Not FillDatset(OleAdSorti, DsetRezpt, "TblSorti") Then
      Exit Sub
    End If
    lstSOR.SelectedIndex = -1
    lstSOR.DataSource = DsetRezpt.Tables("TblSorti")
    lstSOR.DisplayMember = "SORTI_NAME"
    lstSOR.ValueMember = "SORTI_ID"

    '
    '

    lstSOR.SelectedIndex = -1
    If DsetRezpt.Tables("TblSorti").Rows.Count > 0 Then
      SortiID = DsetRezpt.Tables("TblSorti").Rows(0)(0)
      lstSOR.SelectedIndex = -1
    End If

  End Sub

  '

  Private Sub lstSOR_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles lstSOR.SelectedIndexChanged
    Dim ier As Integer
    Dim KeyID As String
    Dim RwNr As String
    Dim FarbRow As DataRow

    If SortiID = -1 Then Exit Sub
    btnSpeichern.Enabled = False
    '
    'Löschen

    RezSozpt.Rezepte.clear()
    RezSozpt.Farben.clear()
    Keymenge = "SOR"
    KeySpei = "COL"
    KeyCalc = "CAL"
    RezSozpt.Rezepte.AddRez(Keymenge, New Recipe)
    RezSozpt.Rezepte.AddRez(KeySpei, New Recipe)
    RezSozpt.Rezepte.AddRez(KeyCalc, New Recipe)

    RezSozpt.Rezepte.kwb(0) = 0
    RezSozpt.Rezepte.kwb(1) = 1
    '
    'Einlesen Sortiment 
    '
    '
    '

    RwWrSortim.ReadSortimFarbGrund(Keymenge, lstSOR.SelectedValue, RezSozpt, UntID, TypID, ier)
    txtDick.Text = RezSozpt.Rezepte(Keymenge).Dicke(0)
    '
    '
    '
    'Felder auffüllen
    '
    '
    'R-Werte Untergrund
    '
    '
    If UntID(0) >= 0 Then
      ReWrRwert.ReadRwert(UntID(0), GrpRwerte(0).RefUnt, ier)
      lblMessUnt.Text = GrpRwerte(0).RefUnt.Name
    End If
    '
    '
    'Tabelle Farbtab füllen
    '
    '
    FarbTab.Rows.Clear()
    GrpRwerte("W").clear()
    For i = 0 To RezSozpt.Rezepte(Keymenge).KF - 1
      FarbRow = FarbTab.NewRow
      FarbRow("FRBID") = RezSozpt.Rezepte(Keymenge)(i).ID
      KeyID = KeyName(FarbRow("FRBID"))
      FarbRow("FRBICH") = RezSozpt.Farben(KeyID).Ichf
      FarbRow("FRBNAM") = RezSozpt.Farben(KeyID).Name
      FarbRow("OP") = RezSozpt.Farben(KeyID).OP
      FarbRow("MENGE") = RezSozpt.Farben(KeyID).BoMng
      FarbRow("PROZ") = RezSozpt.Rezepte(Keymenge)(i).Proz
      FarbRow("DICKE") = RezSozpt.Rezepte(Keymenge).Dicke(0)
      FarbTab.Rows.Add(FarbRow)
      RwNr = KeyRe(i)
      GrpRwerte("W").Add(RwNr, New RefValue)
      GrpRwerte("W")(RwNr).kwb = 0
      GrpRwerte("W")(RwNr).Nr = i
    Next
  End Sub
  '
  '
  '
  '
  '
  '
  '
  Sub CreateFarbTab(ByRef FarbTab As DataTable, ByRef dbgFarbmittel As DataGridView)
    Dim i As Integer
    FarbTab.Columns.Add("FRBID", GetType(Integer))
    FarbTab.Columns.Add("FRBICH", GetType(Integer))
    FarbTab.Columns.Add("FRBNAM", GetType(String))
    FarbTab.Columns.Add("RWRID", GetType(Integer))
    FarbTab.Columns.Add("RWRNAM", GetType(String))
    FarbTab.Columns.Add("OP", GetType(String))
    FarbTab.Columns.Add("MENGE", GetType(Single))
    FarbTab.Columns.Add("PROZ", GetType(Single))
    FarbTab.Columns.Add("DICKE", GetType(Single))
    '
    FarbTab.Columns("FRBID").DefaultValue = -1
    FarbTab.Columns("FRBICH").DefaultValue = 0
    FarbTab.Columns("FRBNAM").DefaultValue = Space(1)
    FarbTab.Columns("RWRID").DefaultValue = -1
    FarbTab.Columns("RWRNAM").DefaultValue = Space(1)
    FarbTab.Columns("OP").DefaultValue = Space(1)
    FarbTab.Columns("MENGE").DefaultValue = -1.0
    FarbTab.Columns("PROZ").DefaultValue = 100
    FarbTab.Columns("DICKE").DefaultValue = 1.0
    '
    For i = 0 To FarbTab.Columns.Count - 1
      FarbTab.Columns(i).AllowDBNull = False
    Next

    '
    '
    '
    dbgFarbmittel.DataSource = FarbTab
    With (dbgFarbmittel.Columns("FRBID"))
      .Name = ""
      .Visible = False
      .ReadOnly = True
    End With
    With dbgFarbmittel.Columns("FRBICH")
      .Name = ""
      .Visible = False
      .ReadOnly = True
    End With
    With dbgFarbmittel.Columns("FRBNAM")
      .HeaderText = Texxt(980)
      .Visible = True
      .Width = 150
      .ReadOnly = True
    End With
    With dbgFarbmittel.Columns("RWRID")
      .Visible = False
      .ReadOnly = True
    End With
    With dbgFarbmittel.Columns("RWRNAM")
      .HeaderText = Texxt(3110)
      .Visible = True
      .Width = 250
      .ReadOnly = True
    End With
    With dbgFarbmittel.Columns("OP")
      .HeaderText = "OP"
      .Visible = True
      .Width = 30
      .ReadOnly = False
      .DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
    End With
    With dbgFarbmittel.Columns("MENGE")
      .HeaderText = Texxt(4630)
      .Visible = True
      .Width = 75
      .ReadOnly = False
      .DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      .DefaultCellStyle.Format = "##.000"
    End With
    With dbgFarbmittel.Columns("PROZ")
      .HeaderText = Texxt(820)
      .Visible = True
      .Width = 75
      .ReadOnly = False
      .DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      .DefaultCellStyle.Format = "##.000"
    End With
    With dbgFarbmittel.Columns("DICKE")
      .HeaderText = Texxt(893)
      .Visible = True
      .Width = 75
      .ReadOnly = False
      .DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      .DefaultCellStyle.Format = "##.000"
    End With
    dbgFarbmittel.AllowUserToAddRows = False
    For i = 0 To dbgFarbmittel.Columns.Count - 1
      dbgFarbmittel.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
    Next
  End Sub
  Sub GetNameID(ByRef ID As Integer, ByRef RefName As String, ByRef Banum As String)
    Dim j As Integer
    Dim ier As Integer
    ier = 0

    If RcmdRef = -1 Then
      lblMessUnt.Text = GrpRwerte("W").RefUnt.Name
    Else
      FarbTab.Rows(RcmdRef)("RWRID") = ID
      FarbTab.Rows(RcmdRef)("RWRNAM") = RefName
      RcmdRef = RcmdRef + 1
      If RcmdRef = FarbTab.Rows.Count Then
        GetPutReflex.InVisible()
        Exit Sub
      End If
      GetPutReflex.Messrefel = GrpRwerte("W")(RcmdRef)
    End If
    '


  End Sub



  Private Sub btnMessUnt_Click(sender As Object, e As System.EventArgs) Handles btnMessUnt.Click

    '
    '
    RcmdRef = -1

    '
    'Messung
    '
    '
    GetPutReflex.Messrefel = GrpRwerte("W").RefUnt

    GetPutReflex.Iarch = 1
    GetPutReflex.Captext = btnMessUnt.Text
    GetPutReflex.Retr = 0
    MenueParam.Messg.MeArtLock = 0
    Me.Enabled = False
    GetPutReflex.ReflexWerte(False)
    Me.Enabled = True
    MenueParam.Messg.MeArtLock = 0

  End Sub

  Private Sub dbgFarbmittel_CellClick(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dbgFarbmittel.CellEnter, dbgFarbmittel.CellClick
    If e.ColumnIndex = -1 Then Exit Sub
    If e.RowIndex = -1 Then Exit Sub
    If FarbTab.Columns(e.ColumnIndex).Caption = "RWRNAM" Then
      RcmdRef = e.RowIndex
      GetPutReflex.Messrefel = GrpRwerte("W")(RcmdRef)
      GetPutReflex.Iarch = 1
      GetPutReflex.Captext = Texxt(496)
      GetPutReflex.Retr = 0
      MenueParam.Messg.MeArtLock = 0
      Me.Enabled = False
      GetPutReflex.ReflexWerte(True)
      Me.Enabled = True
      MenueParam.Messg.MeArtLock = 0
    End If

  End Sub
  Private Sub dbgFarbmittel_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles dbgFarbmittel.DataError
    e.Cancel = False
  End Sub
  Private Sub btnCalculate_Click(sender As Object, e As System.EventArgs) Handles btnCalculate.Click
    Dim i As Integer
    Dim ier As Integer
    Dim KeyID As String
    Dim jcalc As Integer
    MenueParam.Menue.Delta = CSng(txtRestGew.Text)
    GrpRwrtGloss("W").RefUnt = GrpRwerte("W").RefUnt
    RezSozpt.INO = 1
    RezSozpt.MngMax = 100
    RezSozpt.MngMin = 100
    If RezSozpt.Rezepte.RezCount = 0 OrElse IsNothing(RezSozpt.Rezepte(KeyCalc)) Then Exit Sub
    FarbenGloss.clear()
    RezSozpt.Rezepte(KeyCalc).clear()
    RezSozpt.Rezepte(Keymenge).clear()

    For i = 0 To FarbTab.Rows.Count - 1
      RezSozpt.Rezepte(Keymenge).AddFaNr(KeyRe(i), New ColorAmount)
      RezSozpt.Rezepte(Keymenge)(i).ID = FarbTab.Rows(i)("FRBID")
      RezSozpt.Rezepte(Keymenge)(i).FaAmng = FarbTab.Rows(i)("MENGE")
      RezSozpt.Rezepte(Keymenge)(i).Proz = FarbTab.Rows(i)("PROZ")
      KeyID = KeyName(FarbTab.Rows(i)("FRBID"))
      RezSozpt.Farben(KeyID).OP = FarbTab.Rows(i)("OP")
      RezSozpt.Farben(KeyID).BoMng = FarbTab.Rows(i)("MENGE")
      If FarbTab.Rows(i)("OP") = "=" Then
        RezSozpt.Rezepte(KeyCalc).AddFaNr(KeyRe(RezSozpt.Rezepte(KeyCalc).KF), New ColorAmount)
        jcalc = RezSozpt.Rezepte(KeyCalc).KF - 1
        RezSozpt.Rezepte(KeyCalc)(KeyRe(jcalc)).FaAmng = FarbTab.Rows(i)("MENGE")
        RezSozpt.Rezepte(KeyCalc)(KeyRe(jcalc)).Proz = FarbTab.Rows(i)("PROZ")
        RezSozpt.Rezepte(KeyCalc)(KeyRe(jcalc)).ID = FarbTab.Rows(i)("FRBID")
        RezSozpt.Rezepte(KeyCalc)(KeyRe(jcalc)).Prob = RezSozpt.Rezepte(Keymenge)(i).Prob
      End If
    Next i
    Umr.CalcBamng(Keymenge, RezSozpt, ier)
    Umr.CalcBamng(KeyCalc, RezSozpt, ier)

    For i = 0 To FarbTab.Rows.Count - 1
      jcalc = RezSozpt.Rezepte(KeyCalc).KF
      If FarbTab.Rows(i)("OP") = "=" Then
        '
        '
        'Farbmittel mit "=" - Zeichen werden übernommen
        '
        '
        KeyID = KeyName(FarbTab.Rows(i)("FRBID"))
        GlossFarb = RezSozpt.Farben(KeyID).clone
      Else
        '
        'sonst Restfarbenberechnung
        '
        '
        '
        RezSozpt.Rezepte(KeyCalc).Dicke(0) = FarbTab(i)("DICKE")
        RezSozpt.Rezepte(KeyCalc).Nr = 0
        RezSozpt.Rezepte(KeyCalc).AddFaNr(KeyRe(jcalc), New ColorAmount)
        RezSozpt.Rezepte(KeyCalc)(KeyRe(jcalc)).FaAmng = FarbTab.Rows(i)("MENGE")
        RezSozpt.Rezepte(KeyCalc)(KeyRe(jcalc)).Proz = FarbTab.Rows(i)("PROZ")
        RezSozpt.Rezepte(KeyCalc)(KeyRe(jcalc)).ID = FarbTab.Rows(i)("FRBID")
        If IsNothing(GrpRwerte("W")(i)) OrElse GrpRwerte("W")(i).ID = -1 Then
          MsgBox(Texxt(3565) & ": " & FarbTab(i)("FRBNAM"))
          Exit Sub
        End If

        GrpRwrtGloss("W").clear()
        For j = 0 To GrpRwerte("W").Count - 1
          If FarbTab.Rows(i)("RWRID") = GrpRwerte("W")(j).ID Then
            GrpRwrtGloss("W").Add("V", GrpRwerte("W")(j))
            Exit For
          End If
        Next j
        GrpRwrtGloss("W").Add("R", New RefValue)
        GlossFarb = New Colorant
        Umr.CalcBamng(KeyCalc, RezSozpt, ier)
        '
        '

        Call CalcRezept.Restfarbe(5, KeyCalc, KeyCalc, RezSozpt, GrpRwrtGloss, GlossFarb, ier)
        RezSozpt.Rezepte(KeyCalc).RemoveFaNr(KeyRe(jcalc))
        If ier <> 0 Then
          Exit Sub
        End If
        '
        '
        '
      End If

      GlossFarb.Ichf = FarbTab.Rows(i)("FRBICH")
      GlossFarb.Name = txtPrefix.Text & FarbTab.Rows(i)("FRBNAM")
      GlossFarb.OP = " "
      FarbTab.Rows(i)("FRBNAM") = GlossFarb.Name
      KeyID = KeyName(FarbTab.Rows(i)("FRBID"))
      FarbenGloss.AddFarb(KeyID, GlossFarb)
    Next i
    GkIDOld = MenueParam.Misch.GKwrtID
    NPXOld = MenueParam.Misch.Npx
    CDEOld = MenueParam.Messg.CDE
    btnSpeichern.Enabled = True
  End Sub

  Private Sub btnSpeichern_Click(sender As Object, e As System.EventArgs) Handles btnSpeichern.Click
    Dim i As Integer
    Dim j As Integer
    Dim ID As Integer
    Dim ier As Integer
    Dim FaId As Integer
    Dim MitGleich As Boolean
    Dim imsg As DialogResult
    imsg = MessageBox.Show(Texxt(854) & ":" & Space(1) & Texxt(980), Texxt(2000), MessageBoxButtons.YesNo)
    If imsg = Windows.Forms.DialogResult.No Then Exit Sub

    If MenueParam.Misch.GKwrtID = GkIDOld Then
      MitGleich = False
    Else
      MitGleich = True
    End If
    MenueParam.Misch.GKwrtID = GkIDOld
    MenueParam.Misch.Npx = NPXOld
    MenueParam.Messg.CDE = CDEOld
    j = -1
    RezSozpt.Farben.clear()
    Cursor = Cursors.WaitCursor
    For i = 0 To FarbenGloss.FarbCount - 1

      Call ReWrFarbe.ReadFarbmName(FarbenGloss(i).Name, FaId, ier)
      If FaId >= 0 Then
        '
        'Lösche Farbmittel
        '
        '
        FarbenGloss(i).ID = FaId
        ReWrFarbe.FarDel(FarbenGloss(i), True, ier)
      End If
      '
      'Farbmittel speichern
      '
      '
      If FarbTab.Rows(i)("OP") = "=" And Not MitGleich Then
        Continue For
      End If
      FarbenGloss(i).GlzGrdID = -1
      FarbenGloss(i).GlzGrd = 0.0
      ReWrFarbe.FarAdd(FarbenGloss(i), ier)
      '
      '
      'Grunddaten speichern
      '
      '
      '
      ReWrGrund.WriteGrund(FarbenGloss(i).ID, FarbenGloss(i).OptData, MenueParam.Messg.Winkel, ier)
      j = j + 1
      RezSozpt.Rezepte(KeySpei).AddFaNr(KeyRe(j), New ColorAmount)
      RezSozpt.Rezepte(KeySpei)(KeyRe(j)).ID = FarbenGloss(i).ID
      RezSozpt.Rezepte(KeySpei)(KeyRe(j)).FaAmng = 0.001
      RezSozpt.Rezepte(KeySpei)(KeyRe(j)).BaAmng = 0.001
      RezSozpt.Farben.AddFarb(KeyName(FarbenGloss(i).ID), FarbenGloss(i))
    Next i
    Cursor = Cursors.Default

    ' imsg = MessageBox.Show(Texxt(799), Texxt(2000), MessageBoxButtons.YesNo)
    'If imsg = Windows.Forms.DialogResult.No Then Exit Sub
    RezSozpt.Rezepte(KeySpei).Dicke(0) = RezSozpt.Rezepte(Keymenge).Dicke(0)
    RezSozpt.Rezepte(KeySpei).Dicke(1) = RezSozpt.Rezepte(Keymenge).Dicke(1)
    RezSozpt.Rezepte(KeySpei).Name = InputBox(Texxt(824) & ": " & Texxt(850), Texxt(2000), txtPrefix.Text & RezSozpt.Rezepte(Keymenge).Name)
    If RezSozpt.Rezepte(KeySpei).Name = "" Then Exit Sub
    '
    'Sortiment abspeichern
    '
    '
    Call RwWrSortim.AddSortim(KeySpei, ID, RezSozpt, UntID, TypID, ier)
    btnSpeichern.Enabled = False
  End Sub


End Class