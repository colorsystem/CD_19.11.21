Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmNormlicht

  Dim TblNormlicht As DataTable
  Dim TblBereich As DataTable
  Dim TblSpektren As DataTable
  Dim ViewSpektren As DataView
  Dim ViewNormlicht As DataView
  Dim AdaptNormlicht As OleDbDataAdapter
  Dim AdaptBereich As OleDbDataAdapter
  Dim AdaptSpektren As OleDbDataAdapter
  Dim TextPlott As String
  Dim Namen(2) As String
  Dim DelLam As Single
  Dim Werte() As Single
  '
  Dim SqlStmt As String
  Dim hlf As Object
  Dim i As Integer
  Dim j As Integer
  Dim imsg As Integer
  Dim ier As Integer
  Dim Nwe As Integer
  Dim Nwn As Integer
  Dim r() As Single
  Dim rn() As Single
  Dim Lmb() As Single
  Dim LmbN() As Single
  Dim TableName As String
  Dim TableNameN As String
  Dim Winkel As AngGeos
  Dim Kurvs As CurvesRefGrp
  Dim Plott As GraphicHilfProgramme
  Dim RefPointF() As PointF
  Dim RefValue() As Single
  Dim RefTooltip As ToolTip
  Dim txtSum As List(Of TextBox)
  '
  '
  Dim NewShortName As String
  Dim NewKennName As String
  Dim NewLongName As String
  Function GetRefWert(RefLoc As Point) As Single
    Dim i As Integer
    Dim Value As Single
    Dim AbsPointFMin As Single
    Dim AbstPointF As Single
    GetRefWert = -1.0
    AbsPointFMin = 1.0E+30
    If IsNothing(RefPointF) Then Exit Function
    For i = 0 To RefPointF.Count - 1
      AbstPointF = Sqrt((RefPointF(i).X - CSng(RefLoc.X)) ^ 2 + (RefPointF(i).Y - CSng(RefLoc.Y)) ^ 2)
      If AbsPointFMin > AbstPointF Then
        AbsPointFMin = AbstPointF
        Value = RefValue(i)
      End If
    Next i
    If AbsPointFMin < 10 Then
      GetRefWert = Value
    End If
  End Function
  Private Sub frmNormlicht_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim Ueb As String
    Dim ViewLicht As DataView
    Dim Viewtest As DataView
    Dim TblTest As DataTable
    Dim TestRow As DataRow
    Me.Size = MDiform.Size
    Me.Location = MDiform.Location
    Me.Text = Texxt(312)
    TextPlott = Texxt(312)
    lblBereich.Text = Texxt(3940)
    lblNormlicht.Text = Texxt(3941)
    btnEnd.Text = Texxt(1999)
    btnKopieren.Text = Texxt(389)
    chkBld.Text = Texxt(382)
    btnNewLicht.Text = Texxt(3951)
    btnDelete.Text = Texxt(3073)
    lblSumX.Text = Texxt(3077) & "(X)"
    lblSumY.Text = Texxt(3077) & "(Y)"
    lblSumZ.Text = Texxt(3077) & "(Z)"
    '
    lblHinweis.Text = Texxt(3078)

    '
    RefTooltip = New ToolTip
    RefTooltip.AutoPopDelay = 500
    RefTooltip.ShowAlways = True
    '
    '
    TblNormlicht = New DataTable
    TblBereich = New DataTable
    TblSpektren = New DataTable
    ViewSpektren = New DataView(TblSpektren)
    ViewNormlicht = New DataView(TblNormlicht)
    AdaptNormlicht = New OleDbDataAdapter
    AdaptBereich = New OleDbDataAdapter
    AdaptSpektren = New OleDbDataAdapter
    AdaptNormlicht.SelectCommand = New OleDbCommand("", Cncol)
    AdaptNormlicht.InsertCommand = New OleDbCommand("", Cncol)
    AdaptNormlicht.DeleteCommand = New OleDbCommand("", Cncol)
    AdaptNormlicht.UpdateCommand = New OleDbCommand("", Cncol)
    AdaptBereich.SelectCommand = New OleDbCommand("", Cncol)
    AdaptBereich.InsertCommand = New OleDbCommand("", Cncol)
    AdaptBereich.DeleteCommand = New OleDbCommand("", Cncol)
    AdaptBereich.UpdateCommand = New OleDbCommand("", Cncol)
    AdaptSpektren.SelectCommand = New OleDbCommand("", Cncol)
    AdaptSpektren.InsertCommand = New OleDbCommand("", Cncol)
    AdaptSpektren.DeleteCommand = New OleDbCommand("", Cncol)
    AdaptSpektren.UpdateCommand = New OleDbCommand("", Cncol)
    '
    Plott = New GraphicHilfProgramme
    txtSum = New List(Of TextBox)
    txtSum.Add(Nothing)
    txtSum.Add(txtSumX)
    txtSum.Add(txtSumY)
    txtSum.Add(txtSumZ)

    '
    '
    'Tabellen erstellen
    '
    'TBL_BEREICH
    '
    AdaptBereich.SelectCommand.CommandText = "Select * FROM TBL_BEREICH"
    If Not FillDatset(AdaptBereich, TblBereich) Then
      Exit Sub
    End If
    TblBereich.AcceptChanges()
    '
    'TBL_LICHT
    '
    AdaptNormlicht.SelectCommand.CommandText = "SELECT * FROM TBL_LICHT ORDER BY LICHT_ID"
    If Not FillDatset(AdaptNormlicht, TblNormlicht) Then
      Exit Sub
    End If
    TblNormlicht.AcceptChanges()
    '
    '
    '
    'TBL_LICHT_BEREICH_SPEK
    '
    AdaptSpektren.SelectCommand.CommandText = "SELECT * FROM TBL_LICHT_BEREICH_SPEK"
    If Not FillDatset(AdaptSpektren, TblSpektren) Then
      Exit Sub
    End If
    TblSpektren.AcceptChanges()
    '
    '
    '
    '
    '
    '
    'Wellenlängen ergänzen, falls nicht vorhanden
    '
    '
    '
    'View für all Sätze mit LICHT_ID=0 und XYZ_ID=0
    '
    ViewLicht = New DataView(TblSpektren)
    TblTest = TblSpektren.Clone
    TblTest.Rows.Clear()
    TblTest.AcceptChanges()
    Viewtest = New DataView(TblTest)
    For i = 0 To TblSpektren.Rows.Count - 1
      ViewLicht.RowFilter = "LICHT_ID=" & TblSpektren.Rows(i)("LICHT_ID") & " AND BEREICH_ID=" & TblSpektren.Rows(i)("BEREICH_ID") & " AND XYZ_ID=0"
      If ViewLicht.Count = 0 Then
        '
        'Keine Wellenlängen vorhanden
        '
        '
        Viewtest.RowFilter = "LICHT_ID=" & TblSpektren.Rows(i)("LICHT_ID") & " AND BEREICH_ID=" & TblSpektren.Rows(i)("BEREICH_ID") & " AND XYZ_ID=0"
        '
        If Viewtest.Count = 0 Then
          'auch nicht in TBLTEST
          '
          'Row mit Wellenlängen hinzufügen
          TestRow = TblTest.NewRow
          ViewLicht.RowFilter = "LICHT_ID=0 AND BEREICH_ID=" & TblSpektren.Rows(i)("BEREICH_ID") & " AND XYZ_ID=0"
          TestRow("LICHT_ID") = TblSpektren.Rows(i)("LICHT_ID")
          TestRow("BEREICH_ID") = TblSpektren.Rows(i)("BEREICH_ID")
          TestRow("XYZ_ID") = 0
          TestRow("LICHT_FAKT") = 0.0
          TestRow("LICHT_SPEK") = ViewLicht(0)("LICHT_SPEK")
          TblTest.Rows.Add(TestRow)
        End If

      End If
    Next i
    For i = 0 To TblTest.Rows.Count - 1
      TestRow = TblSpektren.NewRow
      TestRow("LICHT_ID") = TblTest.Rows(i)("LICHT_ID")
      TestRow("BEREICH_ID") = TblTest.Rows(i)("BEREICH_ID")
      TestRow("XYZ_ID") = TblTest.Rows(i)("XYZ_ID")
      TestRow("LICHT_FAKT") = TblTest.Rows(i)("LICHT_FAKT")
      TestRow("LICHT_SPEK") = TblTest.Rows(i)("LICHT_SPEK")
      TblSpektren.Rows.Add(TestRow)
    Next i

    '
    '
    '
    '
    'Grid für Werte
    '
    '
    '
    dbgNormlicht.Columns.Clear()
    For i = 0 To 3
      Ueb = Texxt(3942 + i)
      If i > 0 Then
        Ueb = Ueb & "*H(Lambda)"
        Namen(i - 1) = Ueb
      End If
      dbgNormlicht.Columns.Add(Ueb, Ueb)
    Next i
    dbgNormlicht.Columns(0).Width = 0.15 * (dbgNormlicht.Width - 200)
    dbgNormlicht.Columns(1).Width = 0.35 * (dbgNormlicht.Width - 200)
    dbgNormlicht.Columns(2).Width = 0.35 * (dbgNormlicht.Width - 200)
    dbgNormlicht.Columns(3).Width = 0.35 * (dbgNormlicht.Width - 200)
    '
    '
    '
    dbgNormlicht.Columns(0).DefaultCellStyle.Format = "####"
    dbgNormlicht.Columns(1).DefaultCellStyle.Format = "####.000"
    dbgNormlicht.Columns(2).DefaultCellStyle.Format = "####.000"
    dbgNormlicht.Columns(3).DefaultCellStyle.Format = "####.000"
    '
    '
    '
    dbgNormlicht.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNormlicht.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNormlicht.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNormlicht.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight



    '
    'Winkel = New AngGeos
    'Winkel.Wsol = New Curve(
    'Winkel.Add("XYZ")
    'Winkel("XYZ").Chrm = "XYZ"
    'Winkel("XYZ").Lkm = i
    'Winkel("XYZ").Iglz = 0
    'Winkel("XYZ").IhrmGew = 1
    'Winkel("XYZ").IhrmID = i
    'Kurvs = New KurvenGrp
    'Kurvs.AddNewKurven("X")
    'Kurvs.AddNewKurven("Y")
    'Kurvs.AddNewKurven("Z")
    '
    '
    '
    '
    If UserUfo Then
      btnNewLicht.Visible = True
    End If
    cboBereich.DataSource = TblBereich
    cboBereich.DisplayMember = "BEREICH_NAME"
    cboBereich.ValueMember = "BEREICH_ID"
    '
    cboNormlicht.DataSource = TblNormlicht
    cboNormlicht.DisplayMember = "LICHT_KBEZ"
    cboNormlicht.ValueMember = "LICHT_ID"
    cboBereich.SelectedValue = 1
    '
    '
    'Normlichtart D65
    '
    '
    '
    '
    cboNormlicht.SelectedValue = 3
  End Sub
  Private Sub cboBereichLicht_SelectedValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboBereich.SelectedValueChanged, cboNormlicht.SelectedValueChanged
    Dim i As Integer
    Dim j As Integer
    Dim BereichID As Integer
    Dim LichtID As Integer


    chkBld.Checked = False
    'MsgBox "Hier bin ich"
    'MsgBox datTables.Recordset!NORM_FILE_ID
    If Not IsNumeric(cboBereich.SelectedValue) Or Not IsNumeric(cboNormlicht.SelectedValue) Then Exit Sub
    BereichID = cboBereich.SelectedValue
    LichtID = cboNormlicht.SelectedValue
    If LichtID < 200 Or Not UserUfo Then
      dbgNormlicht.ReadOnly = True
      btnDelete.Visible = False
    Else
      dbgNormlicht.ReadOnly = False
      dbgNormlicht.Columns(0).ReadOnly = True
      btnDelete.Visible = True
    End If
    Nwe = cboBereich.SelectedItem("WELL_NWE")
    Winkel = New AngGeos
    Winkel.Nwp = cboBereich.SelectedItem("WELL_NWP")
    Winkel.Add("XYZ", New AngGeo(0, 0, 0, "", 1, "XYZ"))
    Kurvs = New CurvesRefGrp
    Kurvs.Add("X", New CurvesRef)
    Kurvs.Add("Y", New CurvesRef)
    Kurvs.Add("Z", New CurvesRef)
    '
    '
    'Werte übernehmen
    '
    dbgNormlicht.Rows.Clear()
   
    'Wellenlängen
    '
    '
    DelLam = cboBereich.SelectedItem("WELL_STEP")
    Winkel.Wsol = New CurveRef(Nwe)
    Winkel.Wsol.R(0) = cboBereich.SelectedItem("WELL_START")
    For i = 1 To Nwe - 1
      Winkel.Wsol.R(i) = Winkel.Wsol.R(i - 1) + DelLam
    Next i
    '
    For i = 0 To Nwe - 1
      dbgNormlicht.Rows.Add(Winkel.Wsol.R(i))
    Next i
    '
    ViewSpektren.Sort = "XYZ_ID"
    ViewSpektren.RowFilter = "BEREICH_ID=" & BereichID & " AND LICHT_ID=" & LichtID
    Kurvs(0).clear()
    If ViewSpektren.Count = 0 Then
      dbgNormlicht.Visible = False
      Exit Sub
    Else
      dbgNormlicht.Visible = True
    End If
    For i = 0 To ViewSpektren.Count - 1
      Werte = GetSingles(ViewSpektren(i)("LICHT_SPEK"))

      If i > 0 Then
        Kurvs(i - 1).clear()
        Kurvs(i - 1).Add(Namen(i - 1), New CurveRef(Nwe))
        txtSum(i).Text = Format(ViewSpektren(i)("LICHT_FAKT"), "####.000")
      End If
      For j = 0 To Werte.Length - 1
        If i > 0 Then
          Kurvs(i - 1)(0).R(j) = Werte(j)
        End If
        dbgNormlicht.Rows(j).Cells(i).Value = Werte(j)
      Next j
    Next i

    '
    '
   
    'grafische Darstellung
    '
    '
    '
    '
    'MsgBox Winkel.km
    lblAnz.Text = CStr(Winkel.Nwp)


    picNormlicht.Refresh()

  End Sub

  Private Sub chkBld_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkBld.CheckedChanged
    If chkBld.Checked Then
      dbgNormlicht.Visible = False
      picNormlicht.Visible = True
    Else
      dbgNormlicht.Visible = True
      picNormlicht.Visible = False
    End If
  End Sub

  Private Sub picNormlicht_MouseMove(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles picNormlicht.MouseMove
    Dim Wert As Single
    If IsNothing(sender) Then Exit Sub
    Wert = GetRefWert(e.Location)
    If Wert > -1.0 Then
      RefTooltip.SetToolTip(picNormlicht, Format(Wert, "##0.00"))
    End If
  End Sub


  Private Sub picNormlicht_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles picNormlicht.Paint
    Dim Rmii As Single
    Dim Rmaa As Single
    Rmii = 0.0#
    Rmaa = 3 * DelLam
    Plott.GraphBounds = New RectangleF(0.0, 0.0, picNormlicht.Width, picNormlicht.Height)
    Call Plott.RefPaant(TextPlott, Namen, Rmii, Rmaa, Winkel, Kurvs, 1.0#, 0, 0, e.Graphics, RefPointF, RefValue, ier)

  End Sub

  Private Sub btnKopieren_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnKopieren.Click
    Clipboard.Clear()
    If dbgNormlicht.Visible Then
      '
      'Normspektralwerte in Zwischenablage
      '
      '
      '
      '
      dbgNormlicht.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableWithoutHeaderText
      dbgNormlicht.SelectAll()

      If dbgNormlicht.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
        Clipboard.SetDataObject(dbgNormlicht.GetClipboardContent)
      End If
      dbgNormlicht.ClearSelection()
    End If
  End Sub

  Private Sub btnEnd_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnEnd.Click
    Dim i As Integer
    Dim ComLicht As New OleDbCommand("", Cncol)
    Dim WhereKeyID() As String
    If UserUfo AndAlso AddDelP(2990) Then
      ReDim WhereKeyID(0)
      '
      'TBL_BEREICH
      '
      '
      WhereKeyID(0) = "BEREICH_ID"
      AdaptBereich.UpdateCommand = OleDBUpdateCmd("TBL_BEREICH", WhereKeyID, Cncol)
      AdaptBereich.DeleteCommand = OleDBDeleteCmd("TBL_BEREICH", WhereKeyID, Cncol)
      AdaptBereich.InsertCommand = OleDBInsertCmd("TBL_BEREICH", Cncol)
      '
      '


      'TBL_LICHT
      '
      '
      '
      WhereKeyID(0) = "LICHT_ID"
      AdaptNormlicht.UpdateCommand = OleDBUpdateCmd("TBL_LICHT", WhereKeyID, Cncol)
      AdaptNormlicht.UpdateCommand.CommandText = AdaptNormlicht.UpdateCommand.CommandText & " AND LICHT_ID >= " & 200
      AdaptNormlicht.DeleteCommand = OleDBDeleteCmd("TBL_LICHT", WhereKeyID, Cncol)
      AdaptNormlicht.DeleteCommand.CommandText = AdaptNormlicht.DeleteCommand.CommandText & " AND LICHT_ID >= " & 200
      AdaptNormlicht.InsertCommand = OleDBInsertCmd("TBL_LICHT", Cncol)
      

      '
      '
      'TBL_LICHT_BEREICH_SPEK
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "LICHT_ID"
      WhereKeyID(1) = "BEREICH_ID"
      WhereKeyID(2) = "XYZ_ID"
      AdaptSpektren.UpdateCommand = OleDBUpdateCmd("TBL_LICHT_BEREICH_SPEK", WhereKeyID, Cncol)
      AdaptSpektren.UpdateCommand.CommandText = AdaptSpektren.UpdateCommand.CommandText & " AND LICHT_ID >= " & 200
      AdaptSpektren.InsertCommand = OleDBInsertCmd("TBL_LICHT_BEREICH_SPEK", Cncol)
      '
      'Deletecommand
      '
      '
      '
      ViewNormlicht.RowStateFilter = DataViewRowState.Deleted

      For i = 0 To ViewNormlicht.Count - 1
        ComLicht.CommandText = "DELETE * FROM TBL_USER_METH_LICHT WHERE LICHT_ID=" & ViewNormlicht(i)("LICHT_ID")
        If SQLExeNonQuery(ComLicht) Then
        End If
        ComLicht.CommandText = "DELETE * FROM TBL_LICHT_BEREICH_SPEK WHERE LICHT_ID=" & ViewNormlicht(i)("LICHT_ID")
        If SQLExeNonQuery(ComLicht) Then
        End If
      Next
      'AdaptSpektren.Update(TblSpektren.Select(Nothing, Nothing, DataViewRowState.Deleted))
      'AdaptBereich.Update(TblBereich.Select(Nothing, Nothing, DataViewRowState.Deleted))
      AdaptNormlicht.Update(TblNormlicht.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Updatecommand
      '
      '
      'AdaptBereich.Update(TblBereich.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      AdaptNormlicht.Update(TblNormlicht.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      AdaptSpektren.Update(TblSpektren.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '


      'Insertcommand
      '
      'AdaptBereich.Update(TblBereich.Select(Nothing, Nothing, DataViewRowState.Added))
      AdaptNormlicht.Update(TblNormlicht.Select(Nothing, Nothing, DataViewRowState.Added))
      AdaptSpektren.Update(TblSpektren.Select(Nothing, Nothing, DataViewRowState.Added))

      TblSpektren.AcceptChanges()
      TblBereich.AcceptChanges()
      TblNormlicht.AcceptChanges()

    End If
    Me.DialogResult = DialogResult.OK
  End Sub


  Private Sub SplitAnzeigen_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles SplitAnzeigen.Resize
    picNormlicht.Refresh()
  End Sub

  Private Sub btnNewLicht_Click(sender As Object, e As System.EventArgs) Handles btnNewLicht.Click
    Dim NewLichtID As Integer
    Dim NewLichtRow As DataRow
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    '
    'Namen
    '
    'Kennung
    '
    NewKennName = InputBox(Texxt(3070))
    If NewKennName = "" Then Exit Sub
    '
    '
    'Kurzname
    '
    NewShortName = InputBox(Texxt(3071))
    If NewKennName = "" Then Exit Sub
    '
    '
    '
    'Langname
    '
    '
    NewLongName = InputBox(Texxt(3072))
    If NewLongName = "" Then Exit Sub
    '
    If NewKennName.Length > 6 Then
      NewKennName = NewKennName.Substring(0, 6)
    End If
    If NewShortName.Length > 20 Then
      NewShortName = NewShortName.Substring(0, 20)
    End If
    If NewLongName.Length > 100 Then
      NewLongName = NewLongName.Substring(0, 100)
    End If
    '
    '
    dbgNormlicht.ReadOnly = False
    dbgNormlicht.Columns(0).ReadOnly = True
    '
    '
    '
    If MessageBox.Show(Texxt(3075), Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
      NewLichtID ^= 0
      For i = 0 To TblNormlicht.Rows.Count - 1
        If NewLichtID < TblNormlicht.Rows(i)("LICHT_ID") Then
          NewLichtID = TblNormlicht.Rows(i)("LICHT_ID")
        End If
      Next
      If NewLichtID < 200 Then
        NewLichtID = 200
      Else
        NewLichtID = NewLichtID + 1
      End If
      NewLichtRow = TblNormlicht.NewRow
      NewLichtRow("LICHT_ID") = NewLichtID
      NewLichtRow("LICHT_KENN") = NewKennName
      NewLichtRow("LICHT_KBEZ") = NewShortName
      NewLichtRow("LICHT_LBEZ") = NewLongName

      '
      '
      TblNormlicht.Rows.Add(NewLichtRow)
      '
      '
      '
      'Normspektralwerte*Lichtart
      '
      '
      '
      For j = 0 To TblBereich.Rows.Count - 1
        For i = 0 To 3
          NewLichtRow = TblSpektren.NewRow
          NewLichtRow("LICHT_ID") = NewLichtID
          NewLichtRow("BEREICH_ID") = TblBereich.Rows(j)("BEREICH_ID")
          NewLichtRow("XYZ_ID") = i
          NewLichtRow("LICHT_FAKT") = 0.0
          ReDim Werte(TblBereich.Rows(j)("WELL_NWP") - 1)
          If i = 0 Then
            Werte(0) = TblBereich.Rows(j)("WELL_START")
            For k = 1 To Werte.Count - 1
              Werte(k) = Werte(k - 1) + TblBereich.Rows(j)("WELL_STEP")
            Next
            NewLichtRow("LICHT_SPEK") = GetBytes(Werte)
          Else
            NewLichtRow("LICHT_SPEK") = GetBytes(Werte)
          End If
          TblSpektren.Rows.Add(NewLichtRow)
        Next i
      Next j
      cboNormlicht.SelectedValue = NewLichtID
    End If



  End Sub

  
 
  Private Sub btnDelete_Click(sender As Object, e As System.EventArgs) Handles btnDelete.Click
    Dim Viewrow As DataRowView
    Dim i As Integer
    If cboNormlicht.SelectedValue < 200 Then Exit Sub
    If MessageBox.Show(Texxt(3074), Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then

      For Each Viewrow In ViewSpektren
        Viewrow.Delete()
      Next
      For i = 0 To TblNormlicht.Rows.Count - 1
        If TblNormlicht.Rows(i)("LICHT_ID") = cboNormlicht.SelectedValue Then
          TblNormlicht.Rows(i).Delete()
          Exit For
        End If
      Next
      btnNewLicht.Enabled = False
    End If

  End Sub
  Private Sub dbgNormlicht_CellEndEdit(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dbgNormlicht.CellEndEdit
    Dim wert As Single
    Dim by() As Byte
    Dim i As Integer
    If e.ColumnIndex = 0 Then Exit Sub
    If Not IsNumeric(dbgNormlicht.Rows(e.RowIndex).Cells(e.ColumnIndex).Value) Then
      dbgNormlicht.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = 0.0
    End If
    wert = dbgNormlicht.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
    by = GetBytes(wert)
    For i = 0 To 3
      ViewSpektren(e.ColumnIndex)("LICHT_SPEK")(4 * e.RowIndex + i) = by(i)
    Next i
    wert = 0.0
    For i = 0 To dbgNormlicht.Rows.Count - 1
      wert = wert + dbgNormlicht.Rows(i).Cells(e.ColumnIndex).Value
    Next
    ViewSpektren(e.ColumnIndex)("LICHT_FAKT") = wert
  End Sub

  Private Sub dbgNormlicht_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles dbgNormlicht.DataError
    e.Cancel = True
  End Sub
End Class