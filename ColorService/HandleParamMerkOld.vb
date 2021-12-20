Public Class HandleParamMerk
  Dim i As Integer
  Dim j As Integer
  Dim SqlStmt As String
  Dim hlf As Object
  Dim ier As Integer
  Dim MnUserID As Integer
  Dim MnMethID As Integer
  '
  '
  '
  '
  Dim TblPAR As DataTable
  Dim TblList As DataTable
  Dim TblUserPAR As DataTable
  '
  '
  '
  Dim ViewList As DataView
  Dim ViewUserPAR As DataView
  '
  '
  '
  Dim SCmdPAR As OleDbCommand
  Dim CmdList As OleDbCommand
  Dim CmdUserPAR As OleDbCommand
  '
  '
  Dim AdaptPAR As OleDbDataAdapter
  Dim AdaptList As OleDbDataAdapter
  Dim AdaptUserPAR As OleDbDataAdapter
  '
  '
  Dim WithEvents MnComboWert As ComboBox
  Dim WithEvents ConnPAR As BindingSource
  Dim WithEvents MnflgParameter As DataGridView
  Public Sub New(ByRef flgParameter As DataGridView, ByRef ComboWert As ComboBox)
    '

    TblPAR = New DataTable
    TblList = New DataTable
    TblUserPAR = New DataTable
    '
    '
    '
    ViewList = New DataView(TblList)
    ViewUserPAR = New DataView(TblUserPAR)
    '

    '
    '
    SCmdPAR = New OleDbCommand("", Cncol)
    CmdList = New OleDbCommand("", Cncol)
    CmdUserPAR = New OleDbCommand("", Cncol)
    '
    '
    AdaptPAR = New OleDbDataAdapter
    AdaptList = New OleDbDataAdapter
    AdaptUserPAR = New OleDbDataAdapter
    '
    '
    '
    AdaptPAR.SelectCommand = SCmdPAR
    AdaptList.SelectCommand = CmdList
    AdaptUserPAR.SelectCommand = CmdUserPAR
    '
    '
    'Standardwerte für Parameter
    '
    '
    SqlStmt = "SELECT * FROM TBL_PARAM"
    SCmdPAR.CommandText = SqlStmt
    If Not FillDatset(AdaptPAR, TblPAR) Then
      Exit Sub
    End If
    TblPAR.AcceptChanges()
    '

    '
    'Vorgegebene Auswahl für Parameterwerete
    '
    '
    SqlStmt = "SELECT * FROM TBL_PARAM_LIST"
    CmdList.CommandText = SqlStmt
    If Not FillDatset(AdaptList, TblList) Then
      Exit Sub
    End If
    TblList.AcceptChanges()
    '
    'Userspezifische Parameterwerte
    '
    SqlStmt = "SELECT * FROM TBL_USER_METH_PARAM"
    CmdUserPAR.CommandText = SqlStmt
    If Not FillDatset(AdaptUserPAR, TblUserPAR) Then
      Exit Sub
    End If
    TblUserPAR.AcceptChanges()
    '
    ConnPAR = New BindingSource
    ConnPAR.DataSource = TblPAR
    '
    '
    'Aufbau Grid
    '
    '
    '
    '
    '
    MnflgParameter = flgParameter
    MnflgParameter.DataSource = ConnPAR
    MnComboWert = ComboWert
    MnComboWert.DataSource = ViewList
    MnComboWert.DisplayMember = "PARAM_WERT"

    '
    '
    MnflgParameter.Columns.Clear()
    MnflgParameter.Columns.Add("ID", "")
    MnflgParameter.Columns.Add("LBEZ", Texxt(283))
    MnflgParameter.Columns.Add("KBEZ", Texxt(282))
    MnflgParameter.Columns.Add("WERT", Texxt(243))
    MnflgParameter.Columns.Add("COMM", Texxt(244))
    MnflgParameter.Columns(0).DataPropertyName = "PARAM_ID"
    MnflgParameter.Columns(1).DataPropertyName = "PARAM_LBEZ"
    MnflgParameter.Columns(2).DataPropertyName = "PARAM_KBEZ"
    MnflgParameter.Columns(3).DataPropertyName = "PARAM_WERT"
    MnflgParameter.Columns(4).DataPropertyName = "PARAM_BEZWRT"
    MnflgParameter.AutoGenerateColumns = False
    MnflgParameter.AllowUserToAddRows = False
    MnflgParameter.AllowUserToDeleteRows = False
    MnflgParameter.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
    MnflgParameter.Columns(0).Visible = False
    MnflgParameter.Columns(1).ReadOnly = True
    MnflgParameter.Columns(2).ReadOnly = True
    MnflgParameter.ScrollBars = ScrollBars.Vertical
    MnflgParameter.Columns(0).Width = 0
    MnflgParameter.Columns(1).Width = 250
    MnflgParameter.Columns(2).Width = 100
    MnflgParameter.Columns(3).Width = 50

  End Sub

  Sub FillGrid(ByVal UserID As Long, ByVal MethID As Long)
    Dim i As Integer
    Dim ParRows() As DataRow
    MnUserID = UserID
    MnMethID = MethID
    '
    MnflgParameter.Enabled = False
    TblPAR.RejectChanges()
    ViewUserPAR.RowFilter = "USER_ID=" & MnUserID & " AND METH_ID=" & MnMethID
    For i = 0 To ViewUserPAR.Count - 1
      ParRows = TblPAR.Select("PARAM_ID=" & ViewUserPAR(i)("PARAM_ID"), Nothing, DataRowState.Unchanged)
      If ParRows.Length > 0 Then
        ParRows(0)("PARAM_WERT") = ViewUserPAR(i)("PARAM_WERT")
        ParRows(0)("PARAM_BEZWRT") = ViewUserPAR(i)("PARAM_BEZWRT")
      End If
    Next
    MnflgParameter.Enabled = True

  End Sub
  Sub FillDatabase()
    Dim i As Integer
    Dim WhereKeyID() As String
    If AddDelP(2990) Then
      '
      'TBL_USER_METH_PARAM
      '
      ConnPAR.EndEdit()

      '
      '
      'Insertcommand
      '
      '
      AdaptUserPAR.InsertCommand = OleDBInsertCmd("TBL_USER_METH_PARAM", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "PARAM_ID"
      AdaptUserPAR.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_PARAM", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptUserPAR.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_PARAM", WhereKeyID, Cncol)
      '
      '
      '
      '

      '
      'Delete/Update/Insert TBL_USER_METH_PARAM

      'Delete TBL_USER_METH_PARAM
      '
      AdaptUserPAR.Update(TblUserPAR.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      '
      'Insert TBL_USER_METH_PARAM
      '
      '
      AdaptUserPAR.Update(TblUserPAR.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      '
      'Update TBL_USER_METH_PARAM
      '
      AdaptUserPAR.Update(TblUserPAR.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))

      '
      '
    End If
  End Sub
  Private Sub ConnPAR_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnPAR.CurrentChanged
    If MnflgParameter Is Nothing Then Exit Sub
    ViewList.RowFilter = "PARAM_ID=" & ConnPAR.Current("PARAM_ID")

  End Sub

  Private Sub ConnPAR_CurrentItemChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnPAR.CurrentItemChanged
    Dim RowView As DataRowView
    If Not ConnPAR.Current.row.rowstate = DataViewRowState.ModifiedCurrent Then Exit Sub
    If Not MnflgParameter.Enabled Then Exit Sub
    ViewUserPAR.RowFilter = "USER_ID=" & MnUserID & " AND METH_ID=" & MnMethID & " AND PARAM_ID=" & ConnPAR.Current("PARAM_ID")
    If ViewUserPAR.Count = 0 Then
      'Wert hinzufügen
      RowView = ViewUserPAR.AddNew()
      RowView("USER_ID") = MnUserID
      RowView("METH_ID") = MnMethID
      RowView("PARAM_ID") = ConnPAR.Current("PARAM_ID")
    Else
      'Wert abändern
      RowView = ViewUserPAR(0)
    End If
    RowView("PARAM_WERT") = ConnPAR.Current("PARAM_WERT")
    RowView("PARAM_BEZWRT") = ConnPAR.Current("PARAM_BEZWRT")
    If RowView("PARAM_BEZWRT").length > TblUserPAR.Columns("PARAM_BEZWRT").MaxLength Then
      RowView("PARAM_BEZWRT") = RowView("PARAM_BEZWRT").substring(0, TblUserPAR.Columns("PARAM_BEZWRT").MaxLength)
    End If
    RowView("PARAM_CRNTL_ID") = 0
    RowView.EndEdit()
  End Sub
  Private Sub flgParameter_CellClick(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles MnflgParameter.CellClick
    Dim i As Integer
    MnComboWert.Visible = False
    If MnflgParameter.CurrentCell.ColumnIndex < 3 Then Exit Sub
    If ViewList.Count = 0 Then
      MnflgParameter.Columns("WERT").ReadOnly = False
      MnflgParameter.Columns("COMM").ReadOnly = False
    Else
      MnflgParameter.Columns("WERT").ReadOnly = True
      MnflgParameter.Columns("COMM").ReadOnly = True
    End If
    If MnflgParameter.CurrentCell.ColumnIndex = 3 And ViewList.Count <> 0 Then
      MnComboWert.Visible = True
      Call LocateCombo(MnComboWert, MnflgParameter)
    End If
  End Sub

  Private Sub MnComboWert_DropDownClosed(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnComboWert.DropDownClosed
    Dim i As Integer
    Dim k As Integer
    '
    'Nummer des Parameters
    '
    i = MnflgParameter.CurrentCell.ColumnIndex

    '
    'Startmenge (index=0);Dämpfungsstärke (index=1) ;Kopplungsart(index=2)
    '
    k = MnflgParameter.CurrentCell.RowIndex

    MnflgParameter.CurrentCell.Value = MnComboWert.Items(MnComboWert.SelectedIndex)("PARAM_WERT")
    MnflgParameter.Rows(k).Cells(i + 1).Value = MnComboWert.Items(MnComboWert.SelectedIndex)("PARAM_BEZWRT")
    ConnPAR.ResetCurrentItem()
    MnComboWert.Visible = False
  End Sub

  Private Sub MnflgParameter_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles MnflgParameter.DataError
    e.Cancel = False
  End Sub
  Sub LocateCombo(ByVal cboGKW As ComboBox, ByVal flggkw As DataGridView)
    Dim X As Integer
    Dim Y As Integer
    Dim i As Integer
    X = flggkw.Location.X
    If flggkw.RowHeadersVisible Then
      X = X + flggkw.RowHeadersWidth
    End If
    For i = 0 To flggkw.CurrentCell.ColumnIndex - 1
      If flggkw.Columns(i).Visible Then
        X = X + flggkw.Columns(i).Width
      End If
    Next
    X = X - flggkw.HorizontalScrollingOffset
    cboGKW.Left = X

    Y = flggkw.Location.Y + flggkw.ColumnHeadersHeight
    For i = 0 To flggkw.CurrentCell.RowIndex - 1
      Y = Y + flggkw.Rows(i).Height
    Next
    Y = Y - flggkw.VerticalScrollingOffset
    cboGKW.Top = Y
    cboGKW.Width = flggkw.CurrentCell.Size.Width
  End Sub
  'User/Methoden spezifische Licharten
    DataGridCombo = New DataGridViewComboBoxColumn
    DataGridCombo.DisplayStyle = DataGridViewComboBoxDisplayStyle.Nothing
    DataGridCombo.DisplayStyleForCurrentCellOnly = DataGridViewComboBoxDisplayStyle.ComboBox
    For i = 0 To GewWert.Length - 1
      DataGridCombo.Items.Add(GewWert(i))
    Next i
    DatNOA.Columns.Clear()
    DatNOA.Columns.Add("KBEZ", Texxt(173))
    DataGridCombo.Name = "GEW"
    DataGridCombo.HeaderText = Texxt(172)
    DatNOA.Columns.Add(DataGridCombo)
    DatNOA.Columns.Add("ID", " ")
    DatNOA.DataSource = ViewNOA
    DatNOA.Columns(0).DataPropertyName = "LICHT_KBEZ"
    DatNOA.Columns(1).DataPropertyName = "NORM_GEW"
    DatNOA.Columns(2).DataPropertyName = "LICHT_ID"
    DatNOA.Columns(2).Visible = False
    DatNOA.AutoGenerateColumns = False
    DatNOA.AllowUserToAddRows = False
    DatNOA.AllowUserToDeleteRows = False
    DatNOA.ScrollBars = ScrollBars.Vertical
End Class