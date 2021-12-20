Option Compare Text
Option Strict Off
Option Explicit On
Public Class frmNassTrocken
  Dim GrpRwerte As RefValuesGrp
  Dim quali As QualKontrolle
  Dim RwrtReadWrite As ReadWriteRwert
  Dim WithEvents GetRwerteNass As HandleRwerte
  Dim WithEvents GetRwerteTrocken As HandleRwerte
  Dim Handlerwert As HandleRwerte
  Dim TblRefWerte As DataTable
  Dim TblMessdevice As DataTable
  Dim AdaptMessdevice As OleDbDataAdapter
  Dim ViewNass As DataView
  Dim ViewTrocken As DataView
  Dim MessgTrockenID As Integer
  Dim MessgNassID As Integer
  Dim IanzTrocken As Integer
  Dim IanzNass As Integer

  Dim RefWerteNass As RefValue
  Dim RefWerteTrocken As RefValue

  Private Sub frmMankiewNassTrocken_FormClosing(sender As Object, e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
    AufbauPar.MessgID = MessgNassID
    AufbauPar.MethID = -1
  End Sub

  Private Sub frmMankiewNassTrocken_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    'FormMDI.Text = MenueParam.Messg.Kbez & " - " & MenueParam.Menue.MethBez
    Me.Text = Texxt(1800 + MenueParam.MethID) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"

    lblMessNass.Text = Texxt(3771)
    lblMessTrocken.Text = Texxt(3770)
    btnVorlageTrocken.Text = Texxt(3772)
    btnNachstellungTrocken.Text = Texxt(3773)
    btnVorlageNass.Text = Texxt(3774)
    btnNachstellungNass.Text = Texxt(3775)
    btnDelete.Text = Texxt(3776)
    btnStore.Text = Texxt(3777)
    lblGRP.Text = Texxt(386)
    chkRwerte.Text = Texxt(3110)
    btnKOP.Text = Texxt(389)
    chkDLDADBN.Text = "DL,Da,Db (" & Texxt(3781) & ")"
    GetRwerteTrocken = New HandleRwerte
    GetRwerteNass = New HandleRwerte
    GetRwerteNass.Measure = Measure
    '
    '
    '
    Handlerwert = New HandleRwerte
    Handlerwert.cboGRP = cboGRP
    Handlerwert.lblGRP = lblGRP
    Call Handlerwert.GRoupList()
    cboGRP.SelectedValue = MenueParam.Messg.RwrtGID

    For i = 0 To cboGRP.Items.Count - 1
      If cboGRP.Items(i)("GROUP_ID") = MenueParam.Messg.RwrtGID Then
        cboGRP.SelectedIndex = i
      End If
    Next

    If cboGRP.SelectedIndex = -1 Then
      cboGRP.SelectedIndex = 0
    End If

    quali = New QualKontrolle
    RwrtReadWrite = New ReadWriteRwert

    '
    '
    TblRefWerte = New DataTable
    Call AufbauTblRefWerte()
    dbgNassTrocken.DataSource = TblRefWerte
    Call AufbauGrid()

    '
    '
    '
    '
    RefWerteNass = New RefValue
    RefWerteTrocken = New RefValue

    '
    '
    '
    GrpRwerte = New RefValuesGrp
    GrpRwerte.Add("N", New RefValues)
    GrpRwerte.Add("T", New RefValues)
    GrpRwerte.Add("D", New RefValues)
    GrpRwerte.Add("X", New RefValues) '

    '
    '
    '
    '
    '
    '
    TblMessdevice = New DataTable
    ViewNass = New DataView(TblMessdevice)
    ViewTrocken = New DataView(TblMessdevice)
    AdaptMessdevice = New OleDbDataAdapter
    AdaptMessdevice.SelectCommand = New OleDbCommand("", Cncol)
    AdaptMessdevice.SelectCommand.CommandText = "SELECT TBL_MESSG.MESSG_ID AS MESSG_ID,MESSG_KBEZ FROM (TBL_MESSG INNER JOIN TBL_USER_MESSG ON TBL_MESSG.MESSG_ID=TBL_USER_MESSG.MESSG_ID) WHERE USER_ID=" & MenueParam.UserID
    If Not FillDatset(AdaptMessdevice, TblMessdevice) Then
      Exit Sub
    End If
    cboMessNass.DataSource = ViewNass
    cboMessNass.ValueMember = "MESSG_ID"
    cboMessNass.DisplayMember = "MESSG_KBEZ"
    cboMessTrocken.DataSource = ViewTrocken
    cboMessTrocken.ValueMember = "MESSG_ID"
    cboMessTrocken.DisplayMember = "MESSG_KBEZ"
    cboMessNass.SelectedValue = MenueParam.MessgID
    cboMessTrocken.SelectedValue = MenueParam.MessgID
    MessgNassID = MenueParam.MessgID
    MessgTrockenID = MenueParam.MessgID
  End Sub
  Private Sub frmMankiewFarbwerte_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
    AufbauPar.MethID = -1

  End Sub


  Private Sub frmMankiewRezept_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize
    If Not Me.Visible Then Exit Sub

    Call ResizeChild(Me)
  End Sub


  Private Sub btnDelete_Click(sender As System.Object, e As System.EventArgs) Handles btnDelete.Click
    Call Neustart()
  End Sub


  Private Sub cboMessTrocken_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboMessTrocken.SelectedValueChanged
    If Not IsNumeric(cboMessTrocken.SelectedValue) Then Exit Sub
    MessgTrockenID = cboMessTrocken.SelectedValue
    Call Neustart()
  End Sub

  Private Sub cboMessNass_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboMessNass.SelectedValueChanged
    If Not IsNumeric(cboMessNass.SelectedValue) Then Exit Sub
    MessgNassID = cboMessNass.SelectedValue
  End Sub
  Private Sub GetRwerteNass_HdlGetNameID(ByRef ID As Integer, ByRef Name As String, ByRef Banum As String) Handles GetRwerteNass.HdlGetNameID
    If IanzNass >= IanzTrocken Then
      MsgBox(Texxt(3782))
      Exit Sub
    End If
    TblRefWerte.Rows(IanzNass)("IDN") = ID
    TblRefWerte.Rows(IanzNass)("NAMEN") = Name
    IanzNass = IanzNass + 1

    If IanzNass = IanzTrocken Then
      Me.Enabled = True
      btnVorlageNass.Enabled = True
    Else
      btnVorlageNass.Enabled = False
    End If

  End Sub
  Private Sub GetRwerteTrocken_HdlGetNameID(ByRef ID As Integer, ByRef Name As String, ByRef Banum As String) Handles GetRwerteTrocken.HdlGetNameID
    If TblRefWerte.Rows.Count <= IanzTrocken Then
      TblRefWerte.Rows.Add(TblRefWerte.NewRow)
    End If
    TblRefWerte.Rows(IanzTrocken)("IDT") = ID
    TblRefWerte.Rows(IanzTrocken)("NAMET") = Name
    TblRefWerte.Rows(IanzTrocken)("XXX") = "X"
    IanzTrocken = IanzTrocken + 1
  End Sub
  Private Sub btnTrocken_Click(sender As System.Object, e As System.EventArgs) Handles btnVorlageTrocken.Click, btnNachstellungTrocken.Click
    Dim i As Integer
    If sender.name = "btnVorlageTrocken" Then
      AufbauPar.MessgID = MessgTrockenID
      cboMessTrocken.Enabled = False
    End If
    Me.Enabled = False
    GetRwerteTrocken.Messrefel = RefWerteTrocken
    GetRwerteTrocken.Iarch = 0


    If sender.name = "btnVorlageTrocken" Then
      GetRwerteTrocken.Captext = Texxt(3772)
      Call GetRwerteTrocken.ReflexWerte(False)
      If IanzTrocken = 1 Then
        btnDelete.Enabled = True
        btnNachstellungTrocken.Enabled = True
        btnVorlageTrocken.Enabled = False

      End If
    Else
      GetRwerteTrocken.Captext = Texxt(3773)
      Call GetRwerteTrocken.ReflexWerte(True)
      If IanzTrocken > 1 Then
        btnNachstellungNass.Enabled = True
      End If
    End If
    Me.Enabled = True
  End Sub
  Private Sub btnNachstellungNass_Click(sender As Object, e As System.EventArgs) Handles btnNachstellungNass.Click
    AufbauPar.MessgID = MessgNassID

    Me.Enabled = False
    GetRwerteNass.Messrefel = RefWerteNass
    GetRwerteNass.Iarch = 0
    GetRwerteNass.Captext = Texxt(3775)
    Call GetRwerteNass.ReflexWerte(True)
    btnNachstellungNass.Enabled = True
    Me.Enabled = True
  End Sub
  Private Sub btnVorlageNass_Click(sender As System.Object, e As System.EventArgs) Handles btnVorlageNass.Click
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim ier As Integer
    Dim XSI As Double
    Dim YP1 As Double
    Dim YPN As Double
    Dim YSDX As Double
    Dim EPS As Double
    Dim WSOLTrocken() As Double
    Dim YA2() As Double
    Dim Y() As Double
    Dim XValue As Double
    Dim YValue As Double
    Dim DE As Single
    Dim DL As Single
    Dim DC As Single
    Dim DH As Single
    Dim DA As Single
    Dim DB As Single
    Dim DDE As Single
    Dim Anz As Single
    Dim RMIT() As Single
    Dim FAKO(0, 2) As Single
    Dim XYZS(0, 0, 2) As Single
    Dim XYZI(0, 0, 2) As Single
    XSI = 10.0
    YP1 = 0.0
    YPN = 0.0
    EPS = 1.0E-30
    '
    'Berechnung der R-Werte für die nasse Vorlage
    '
    '
    If IanzNass <> IanzTrocken Then
      MsgBox(Texxt(3783))
      Exit Sub
    End If
    '
    '
    'R-Werte der trockenen Vorlage + Nachstellungen
    '
    '
    If GrpRwerte("T").Count = 0 Or GrpRwerte("N").Count = 0 Or IanzNass <> GrpRwerte("N").Count Or IanzTrocken <> GrpRwerte("T").Count Then


      AufbauPar.MessgID = MessgTrockenID
      ReDim WSOLTrocken(MenueParam.Messg.Winkel.Wsol.Nwe - 1)
      ReDim YA2(MenueParam.Messg.Winkel.Wsol.Nwe - 1)
      ReDim Y(MenueParam.Messg.Winkel.Wsol.Nwe - 1)
      For i = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
        WSOLTrocken(i) = MenueParam.Messg.Winkel.Wsol.R(i)
      Next
      GrpRwerte("T").clear()
      For i = 0 To TblRefWerte.Rows.Count - 1
        GrpRwerte("T").Add(KeyRe(i), New RefValue)
        Call RwrtReadWrite.ReadRwert(TblRefWerte.Rows(i)("IDT"), GrpRwerte("T")(KeyRe(i)), MenueParam.Messg.Winkel, ier)
      Next i
      '
      '
      'Nass
      '
      AufbauPar.MessgID = MessgNassID
      '
      '
      '
      'Umrechnung in Wellenlängen der Nassmessung
      '
      GrpRwerte("D").clear()
      For i = 0 To GrpRwerte("T").Count - 1
        For j = 0 To WSOLTrocken.Count - 1
          Y(j) = GrpRwerte("T")(KeyRe(i)).RefKurv(0).R(j)
        Next j
        Call SPLINDLL(WSOLTrocken.Count, WSOLTrocken(0), Y(0), YA2(0), XSI, YP1, YPN)
        '
        '
        GrpRwerte("D").Add(KeyRe(i), New RefValue)
        GrpRwerte("D")(KeyRe(i)).RefKurv.Add(MenueParam.Messg.Winkel(0).Chrm, New CurveRef(MenueParam.Messg.Winkel.Wsol.Nwe))
        '
        '
        For j = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
          XValue = MenueParam.Messg.Winkel.Wsol.R(j)
          Call SPLITDLL(WSOLTrocken.Count, WSOLTrocken(0), Y(0), YA2(0), XValue, YValue, XSI, YSDX)
          GrpRwerte("D")(KeyRe(i)).RefKurv(0).R(j) = YValue
        Next j
      Next
      '
      '
      '
      '
      'Farbabstände der trockenen Messung
      '
      '
      '
      For i = 1 To TblRefWerte.Rows.Count - 1
        Call quali.FarbDifferenzRef(0, 0, 0, MenueParam.Messg.Winkel, GrpRwerte("D")(KeyRe(0)).RefKurv(0).R, GrpRwerte("D")(KeyRe(i)).RefKurv(0).R, DE, DL, DC, DH, DA, DB, ier)
        TblRefWerte.Rows(i)("DLT") = Format(DL, "##0.000")
        TblRefWerte.Rows(i)("DAT") = Format(DA, "##0.000")
        TblRefWerte.Rows(i)("DBT") = Format(DB, "##0.000")
      Next i
      GrpRwerte("N").clear()
      '
      'Platzhalter für Mittelwert
      '
      GrpRwerte("N").Add(KeyRe(0), New RefValue)
      GrpRwerte("N")(KeyRe(0)).RefKurv.Add(MenueParam.Messg.Winkel(0).Chrm, New CurveRef(MenueParam.Messg.Winkel.Wsol.Nwe))

      '
      'R-Werte der nassen Nachstellungen
      '
      '
      For i = 1 To TblRefWerte.Rows.Count - 1
        GrpRwerte("N").Add(KeyRe(i), New RefValue)
        Call RwrtReadWrite.ReadRwert(TblRefWerte.Rows(i)("IDN"), GrpRwerte("N")(KeyRe(i)), MenueParam.Messg.Winkel, ier)
      Next i
      '
      '
      '
      'R-Werte für DL,DA,DB Angleich
      '
      '


    End If
    '
    '
    '
    'Mittelwert  für  Rnass(Vorlage)=Rtrocken(Vorlage)+Rnass(Nachstellung)- Rtrocken(Nachstellung)
    '
    '
    ReDim RMIT(MenueParam.Messg.Winkel.Wsol.Nwe - 1)
    For j = 0 To GrpRwerte("N")(KeyRe(0)).RefKurv(0).Nwe - 1
      RMIT(j) = 0
    Next j
    Anz = 0.0
    For i = 1 To TblRefWerte.Rows.Count - 1
      If TblRefWerte.Rows(i)("XXX") = "X" Then
        Anz = Anz + 1.0
        If chkRwerte.Checked Then
          For j = 0 To RMIT.Count - 1
            RMIT(j) = RMIT(j) + GrpRwerte("N")(KeyRe(i)).RefKurv(0).R(j) + GrpRwerte("D")(KeyRe(0)).RefKurv(0).R(j) - GrpRwerte("D")(KeyRe(i)).RefKurv(0).R(j)
          Next j
        Else
          Call quali.FarbDifferenzRef(0, 0, 0, MenueParam.Messg.Winkel, GrpRwerte("D")(KeyRe(i)).RefKurv(0).R, GrpRwerte("D")(KeyRe(0)).RefKurv(0).R, DE, DL, DC, DH, DA, DB, ier)
          If ier <> 0 Then
            Exit Sub
          End If
          FAKO(0, 0) = DL
          FAKO(0, 1) = DA
          FAKO(0, 2) = DB
          GrpRwerte("X").clear()
          GrpRwerte("X").Add(KeyRe(0), New RefValue)
          GrpRwerte("X").Add(KeyRe(1), GrpRwerte("N")(KeyRe(i)))
          GrpRwerte("X").Add(KeyRe(2), New RefValue)
          GrpRwerte("X").Add(KeyRe(3), New RefValue)
          Call quali.RefFarbWerte(2, 1, 1, FAKO, XYZS, XYZI, GrpRwerte("X"), ier)
          If ier <> 0 Then
            Exit Sub
          End If
          'Call quali.FarbDifferenzRef(0, 0, 0, MenueParam.Messg.Winkel, GrpRwerte("X")(KeyRe(1)).RefKurv(0).R, GrpRwerte("X")(KeyRe(0)).RefKurv(0).R, DE, DL, DC, DH, DA, DB, ier)
          For j = 0 To RMIT.Count - 1
            RMIT(j) = RMIT(j) + GrpRwerte("X")(KeyRe(0)).RefKurv(0).R(j)
          Next j
        End If
      End If
    Next
    For j = 0 To GrpRwerte("N")(KeyRe(0)).RefKurv(0).Nwe - 1
      If Anz > 0 Then
        GrpRwerte("N")(KeyRe(0)).RefKurv(0).R(j) = RMIT(j) / Anz
      Else
        GrpRwerte("N")(KeyRe(0)).RefKurv(0).R(j) = GrpRwerte("D")(KeyRe(0)).RefKurv(0).R(j)
      End If
    Next j
    TblRefWerte.Rows(0)("NAMEN") = Texxt(3784)
    TblRefWerte.Rows(0)("IDN") = -1
    '
    '
    'Farbwerte für Nassmessung
    '
    For i = 1 To TblRefWerte.Rows.Count - 1
      Call quali.FarbDifferenzRef(0, 0, 0, MenueParam.Messg.Winkel, GrpRwerte("N")(KeyRe(0)).RefKurv(0).R, GrpRwerte("N")(KeyRe(i)).RefKurv(0).R, DE, DL, DC, DH, DA, DB, ier)
      If chkDLDADBN.Checked Then
        '
        'Differenz D(nass)
        '
        '
        TblRefWerte.Rows(i)("DLN") = Format(DL, "##0.000")
        TblRefWerte.Rows(i)("DAN") = Format(DA, "##0.000")
        TblRefWerte.Rows(i)("DBN") = Format(DB, "##0.000")
        DDE = Sqrt((CSng(TblRefWerte.Rows(i)("DLN")) - CSng(TblRefWerte.Rows(i)("DLT"))) ^ 2 _
                 + (CSng(TblRefWerte.Rows(i)("DAN")) - CSng(TblRefWerte.Rows(i)("DAT"))) ^ 2 _
                 + (CSng(TblRefWerte.Rows(i)("DBN")) - CSng(TblRefWerte.Rows(i)("DBT"))) ^ 2)
        TblRefWerte.Rows(i)("DDE") = Format(DDE, "##0.000")
      Else
        '
        'Differenz der Farbabstände (D(nass)-D(trocken))
        '
        '
        TblRefWerte.Rows(i)("DLN") = Format(DL - TblRefWerte.Rows(i)("DLT"), "##0.000")
        TblRefWerte.Rows(i)("DAN") = Format(DA - TblRefWerte.Rows(i)("DAT"), "##0.000")
        TblRefWerte.Rows(i)("DBN") = Format(DB - TblRefWerte.Rows(i)("DBT"), "##0.000")
        DDE = Sqrt(CSng(TblRefWerte.Rows(i)("DLN")) ^ 2 _
                + CSng(TblRefWerte.Rows(i)("DAN")) ^ 2 _
                + CSng(TblRefWerte.Rows(i)("DBN")) ^ 2)
        TblRefWerte.Rows(i)("DDE") = Format(DDE, "##0.000")
      End If
    Next i
    btnStore.Enabled = True
    btnKOP.Enabled = True
  End Sub
  Private Sub btnStore_Click(sender As Object, e As System.EventArgs) Handles btnStore.Click
    Dim ID As Integer
    Dim ier As Integer
    If Not Handlerwert.MeldSpeiAllRwrt(False) Then
      Exit Sub
    End If
    GrpRwerte("N")(KeyRe(0)).Name = Texxt(3784)
    GrpRwerte("N")(KeyRe(0)).Name = InputBox(Texxt(370), Texxt(2000), GrpRwerte("N")(KeyRe(0)).Name)
    If GrpRwerte("N")(KeyRe(0)).Name <> "" Then
      If Len(GrpRwerte("N")(KeyRe(0)).Name) > 40 Then
        GrpRwerte("N")(KeyRe(0)).Name = Mid(GrpRwerte("N")(KeyRe(0)).Name, 1, 40)
      End If
      GrpRwerte("N")(KeyRe(0)).Cme = "$$"
      GrpRwerte("N")(KeyRe(0)).Gid = cboGRP.SelectedValue
      ReWrRwert.WriteRwert(ID, GrpRwerte("N")(KeyRe(0)), ier)
      TblRefWerte.Rows(0)("NAMEN") = GrpRwerte("N")(KeyRe(0)).Name

    End If
  End Sub
  Private Sub btnKOP_Click(sender As Object, e As System.EventArgs) Handles btnKOP.Click
    '
    'Schreiben nach Clipboard
    '
    Clipboard.Clear()
    '
    'Grid in Zwischenablage
    '
    '
    '
    '
    dbgNassTrocken.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
    dbgNassTrocken.SelectAll()

    If dbgNassTrocken.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
      Clipboard.SetDataObject(dbgNassTrocken.GetClipboardContent)
    End If
    dbgNassTrocken.ClearSelection()
  End Sub
  Private Sub chkDLDADBN_Click(sender As Object, e As System.EventArgs) Handles chkDLDADBN.Click
    Call AufbauGrid()
    btnVorlageNass.PerformClick()
  End Sub
  Private Sub dbgNassTrocken_CellClick(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dbgNassTrocken.CellClick
    If e.RowIndex = 0 Then
      dbgNassTrocken.AllowUserToDeleteRows = False
      Exit Sub
    Else
      dbgNassTrocken.AllowUserToDeleteRows = True
    End If
    If e.ColumnIndex <= 0 Then Exit Sub
    If dbgNassTrocken.Columns(e.ColumnIndex).Name = "XXX" Then
      If dbgNassTrocken.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = "X" Then
        dbgNassTrocken.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = " "
      Else
        dbgNassTrocken.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = "X"
      End If
      btnVorlageNass.PerformClick()
    End If

  End Sub
  Private Sub dbgNassTrocken_RowsRemoved(sender As Object, e As System.Windows.Forms.DataGridViewRowsRemovedEventArgs) Handles dbgNassTrocken.RowsRemoved
    If IanzTrocken <= 0 Then Exit Sub
    If IanzNass > e.RowIndex Then IanzNass = IanzNass - 1
    IanzTrocken = IanzTrocken - 1
    If IanzNass = IanzTrocken Then
      btnVorlageNass.Enabled = True
    End If
    GrpRwerte("N").clear()
    GrpRwerte("T").clear()

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
  Sub AufbauTblRefWerte()
    TblRefWerte.Columns.Add("IDT", GetType(Integer))
    TblRefWerte.Columns.Add("NAMET", GetType(String))
    TblRefWerte.Columns.Add("DLT", GetType(String))
    TblRefWerte.Columns.Add("DAT", GetType(String))
    TblRefWerte.Columns.Add("DBT", GetType(String))
    TblRefWerte.Columns.Add("IDN", GetType(Integer))
    TblRefWerte.Columns.Add("NAMEN", GetType(String))
    TblRefWerte.Columns.Add("DLN", GetType(String))
    TblRefWerte.Columns.Add("DAN", GetType(String))
    TblRefWerte.Columns.Add("DBN", GetType(String))
    TblRefWerte.Columns.Add("DDE", GetType(String))
    TblRefWerte.Columns.Add("XXX", GetType(String))
  End Sub
  Sub AufbauGrid()
    Dim i As Integer
    For i = 0 To dbgNassTrocken.Columns.Count - 1
      dbgNassTrocken.Columns(i).ReadOnly = True
      dbgNassTrocken.Columns(i).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
    Next i
    dbgNassTrocken.AllowUserToAddRows = False
    dbgNassTrocken.Columns("IDT").Visible = False
    dbgNassTrocken.Columns("IDN").Visible = False
    dbgNassTrocken.Columns("DLT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DAT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DBT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DLN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DAN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DBN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DDE").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("XXX").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter

    '
    '
    '
    dbgNassTrocken.Columns("NAMET").HeaderText = Texxt(3778)
    dbgNassTrocken.Columns("NAMEN").HeaderText = Texxt(3779)
    dbgNassTrocken.Columns("DLT").HeaderText = "DL" & "(" & Texxt(3780) & ")"
    dbgNassTrocken.Columns("DAT").HeaderText = "Da " & "(" & Texxt(3780) & ")"
    dbgNassTrocken.Columns("DBT").HeaderText = "Db " & "(" & Texxt(3780) & ")"
    If chkDLDADBN.Checked Then
      dbgNassTrocken.Columns("DLN").HeaderText = "DL " & "(" & Texxt(3781) & ")"
      dbgNassTrocken.Columns("DAN").HeaderText = "Da " & "(" & Texxt(3781) & ")"
      dbgNassTrocken.Columns("DBN").HeaderText = "Db " & "(" & Texxt(3781) & ")"
    Else
      dbgNassTrocken.Columns("DLN").HeaderText = "DDL " & "(" & Texxt(3781) & ")"
      dbgNassTrocken.Columns("DAN").HeaderText = "DDa " & "(" & Texxt(3781) & ")"
      dbgNassTrocken.Columns("DBN").HeaderText = "DDb " & "(" & Texxt(3781) & ")"
    End If

    dbgNassTrocken.Columns("DDE").HeaderText = "DDEnt "
    dbgNassTrocken.Columns("NAMET").Width = 200
    dbgNassTrocken.Columns("NAMEN").Width = 200
    dbgNassTrocken.Columns("XXX").Width = 30
    dbgNassTrocken.Columns("DLT").Width = 100
    dbgNassTrocken.Columns("DAT").Width = 100
    dbgNassTrocken.Columns("DBT").Width = 100
    dbgNassTrocken.Columns("DLN").Width = 100
    dbgNassTrocken.Columns("DAN").Width = 100
    dbgNassTrocken.Columns("DBN").Width = 100
    dbgNassTrocken.Columns("DDE").Width = 100

    dbgNassTrocken.Columns("DLT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DAT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DBT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DLN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DAN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DBN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DDE").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    '
    dbgNassTrocken.Columns("DLT").DefaultCellStyle.BackColor = Color.WhiteSmoke
    dbgNassTrocken.Columns("DAT").DefaultCellStyle.BackColor = Color.LightPink
    dbgNassTrocken.Columns("DBT").DefaultCellStyle.BackColor = Color.LightBlue
    dbgNassTrocken.Columns("DLN").DefaultCellStyle.BackColor = Color.WhiteSmoke
    dbgNassTrocken.Columns("DAN").DefaultCellStyle.BackColor = Color.LightPink
    dbgNassTrocken.Columns("DBN").DefaultCellStyle.BackColor = Color.LightBlue

  End Sub
  Sub Neustart()
    IanzNass = 1
    IanzTrocken = 0
    TblRefWerte.Rows.Clear()
    btnNachstellungNass.Enabled = False
    btnNachstellungTrocken.Enabled = False
    btnVorlageNass.Enabled = False
    TblRefWerte.Rows.Clear()
    btnVorlageTrocken.Enabled = True
    btnNachstellungTrocken.Enabled = False
    btnNachstellungNass.Enabled = False
    btnStore.Enabled = False
    btnKOP.Enabled = False
    cboMessTrocken.Enabled = True
    GrpRwerte("T").clear()
    GrpRwerte("D").clear()
    GrpRwerte("N").clear()


  End Sub


End Class