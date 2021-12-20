Public Class frmMiscel
  Dim TblPruefm As DataTable
  Dim TblProart As DataTable
  Dim TblParameter As DataTable
  Dim TblParameterList As DataTable
  Dim ViewParameterList As DataView
  Dim WithEvents ConnParameter As BindingSource
  '
  Dim cmdSelParam As OleDbCommand
  '
  Dim cmdSelParamList As OleDbCommand
  Dim cmdUpdParamList As OleDbCommand
  Dim cmdDelParamList As OleDbCommand
  Dim cmdAddParamList As OleDbCommand
  '
  Dim cmdSelPruefm As OleDbCommand
  Dim cmdUpdPruefm As OleDbCommand
  Dim cmdDelPruefm As OleDbCommand
  Dim cmdAddPruefm As OleDbCommand
  Dim cmdSelProart As OleDbCommand
  Dim cmdUpdProart As OleDbCommand
  Dim cmdDelProart As OleDbCommand
  Dim cmdAddProart As OleDbCommand
  Dim AdaptPruefm As OleDbDataAdapter
  Dim AdaptProart As OleDbDataAdapter
  Dim AdaptParam As OleDbDataAdapter
  Dim AdaptParamList As OleDbDataAdapter

  Private Sub frmMiscel_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))

    Me.Text = Texxt(408)
    btnMiscel.Text = Texxt(1999)
    lblPruefm.Text = Texxt(1640)
    lblProart.Text = Texxt(1641)
    lblParameter.Text = Texxt(415)
    lblParameterList.Text = Texxt(419)
    TabMiscel.TabPages(1).Text = Texxt(1640) & " / " & Texxt(1641)
    TabMiscel.TabPages(0).Text = Texxt(419)
    TblPruefm = New DataTable
    TblProart = New DataTable
    TblParameter = New DataTable
    TblParameterList = New DataTable
    ViewParameterList = New DataView(TblParameterList)
    ConnParameter = New BindingSource
    cmdSelParam = New OleDbCommand("", Cncol)
    cmdSelParamList = New OleDbCommand("", Cncol)
    cmdUpdParamList = New OleDbCommand("", Cncol)
    cmdDelParamList = New OleDbCommand("", Cncol)
    cmdAddParamList = New OleDbCommand("", Cncol)


    cmdSelPruefm = New OleDbCommand("", Cncol)
    cmdUpdPruefm = New OleDbCommand("", Cncol)
    cmdDelPruefm = New OleDbCommand("", Cncol)
    cmdAddPruefm = New OleDbCommand("", Cncol)
    cmdSelProart = New OleDbCommand("", Cncol)
    cmdUpdProart = New OleDbCommand("", Cncol)
    cmdDelProart = New OleDbCommand("", Cncol)
    cmdAddProart = New OleDbCommand("", Cncol)
    AdaptPruefm = New OleDbDataAdapter
    AdaptProart = New OleDbDataAdapter
    AdaptPruefm.SelectCommand = cmdSelPruefm
    AdaptPruefm.InsertCommand = cmdAddPruefm
    AdaptPruefm.UpdateCommand = cmdUpdPruefm
    AdaptPruefm.DeleteCommand = cmdDelPruefm
    AdaptProart.SelectCommand = cmdSelProart
    AdaptProart.InsertCommand = cmdAddProart
    AdaptProart.UpdateCommand = cmdUpdProart
    AdaptProart.DeleteCommand = cmdDelProart
    '
    '
    AdaptParam = New OleDbDataAdapter
    AdaptParamList = New OleDbDataAdapter
    AdaptParam.SelectCommand = cmdSelParam
    AdaptParamList.SelectCommand = cmdSelParamList
    AdaptParamList.InsertCommand = cmdAddParamList
    AdaptParamList.UpdateCommand = cmdUpdParamList
    AdaptParamList.DeleteCommand = cmdDelParamList
    '
    '


    '
    'Prüfmittel
    '
    '
    '
    cmdSelPruefm.CommandText = "SELECT * FROM TBL_PRUEFM"
    cmdSelProart.CommandText = "SELECT * FROM TBL_PROART"
    '
    'Parameter
    '
    '
    cmdSelParam.CommandText = "SELECT * FROM TBL_PARAM"
    cmdSelParamList.CommandText = "SELECT * FROM TBL_PARAM_LIST"

    '
    '
    'Tabellen aufbauen
    '
    If Not FillDatset(AdaptPruefm, TblPruefm) Then
      Exit Sub
    End If
    If Not FillDatset(AdaptProart, TblProart) Then
      Exit Sub
    End If
    If Not FillDatset(AdaptParam, TblParameter) Then
      Exit Sub
    End If
    If Not FillDatset(AdaptParamList, TblParameterList) Then
      Exit Sub
    End If
    '
    '
    '
    'Prüfmittel
    '
    TabMiscel.TabPages(1).Show()

    '
    '
    dbgPruefm.DataSource = TblPruefm
    dbgPruefm.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
    dbgPruefm.RowHeadersWidth = 20
    dbgPruefm.Columns("PRUEFM_ID").Visible = False
    dbgPruefm.Columns("PRUEFM_KBEZ").HeaderText = Texxt(282) '
    dbgPruefm.Columns("PRUEFM_LBEZ").HeaderText = Texxt(283)
    dbgPruefm.Columns("PRUEFM_KBEZ").Width = 100
    '
    '
    'Produktart
    '
    '
    '
    dbgProart.DataSource = TblProart
    dbgProart.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
    dbgProart.RowHeadersWidth = 20
    dbgProart.Columns("PROART_ID").Visible = False
    dbgProart.Columns("PROART_KBEZ").HeaderText = Texxt(282) '
    dbgProart.Columns("PROART_LBEZ").HeaderText = Texxt(283)
    dbgProart.Columns("PROART_KBEZ").Width = 100
    '
    '
    '
    TabMiscel.TabPages(0).Show()
    '
    'Parameter
    '
    '
    dbgParameter.Columns.Add("ID", "")
    dbgParameter.Columns.Add("LBEZ", Texxt(283))
    dbgParameter.Columns.Add("KBEZ", Texxt(282))
    dbgParameter.Columns.Add("FORM", "")
    dbgParameter.Columns.Add("WERT", Texxt(243))
    dbgParameter.Columns.Add("BEZWRT", Texxt(244))

    dbgParameter.Columns(0).DataPropertyName = "PARAM_ID"
    dbgParameter.Columns(1).DataPropertyName = "PARAM_LBEZ"
    dbgParameter.Columns(2).DataPropertyName = "PARAM_KBEZ"
    dbgParameter.Columns(3).DataPropertyName = "PARAM_FORM"
    dbgParameter.Columns(4).DataPropertyName = "PARAM_WERT"
    dbgParameter.Columns(5).DataPropertyName = "PARAM_BEZWRT"
    ConnParameter.DataSource = TblParameter
    dbgParameter.DataSource = ConnParameter
    'dbgParameter.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
    dbgParameter.RowHeadersWidth = 20
    dbgParameter.Columns("ID").Visible = False
    dbgParameter.Columns("FORM").Visible = False
    dbgParameter.Columns("WERT").Visible = False
    dbgParameter.Columns("BEZWRT").Visible = False
    dbgParameter.Columns("ID").Width = 0
    dbgParameter.Columns("KBEZ").Width = 200
    dbgParameter.Columns("LBEZ").Width = 250
    dbgParameter.Columns("FORM").Width = 0
    dbgParameter.Columns("WERT").Width = 40
    dbgParameter.Columns("BEZWRT").Width = 200
    'dbgParameter.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
    '
    '
    dbgParameter.AlternatingRowsDefaultCellStyle.BackColor = Color.Beige
    dbgParameter.DefaultCellStyle.BackColor = Color.Bisque
    dbgParameter.AllowUserToAddRows = False
    dbgParameter.ReadOnly = True
    dbgParameter.SelectionMode = DataGridViewSelectionMode.FullRowSelect
    '
    '
    dbgParameter.Columns("WERT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    '
    '
    '
    dbgParameterList.DataSource = ViewParameterList
    dbgParameterList.Columns("PARAM_ID").Visible = False
    dbgParameterList.Columns("PARAM_ID").Width = 10
    dbgParameterList.Columns("PARAM_WERT").Width = 60
    dbgParameterList.Columns("PARAM_BEZWRT").Width = 200

    dbgParameterList.Columns("PARAM_WERT").HeaderText = Texxt(243)
    dbgParameterList.Columns("PARAM_BEZWRT").HeaderText = Texxt(244)
    dbgParameterList.Columns("PARAM_WERT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgParameterList.AllowUserToAddRows = True

  End Sub

  Private Sub btnMiscel_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnMiscel.Click
    Dim WhereKeyID() As String
    If AddDelP(2990) Then
      '
      '
      '
      'TBL_PRUEFM
      '
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptPruefm.InsertCommand = OleDBInsertCmd("TBL_PRUEFM", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "PRUEFM_ID"
      AdaptPruefm.UpdateCommand = OleDBUpdateCmd("TBL_PRUEFM", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptPruefm.DeleteCommand = OleDBDeleteCmd("TBL_PRUEFM", WhereKeyID, Cncol)
      '
      '

      'Delete/Update/Insert TBL_PRUEFM
      '
      'Delete TBL_PRUEFM
      '
      AdaptPruefm.Update(TblPruefm.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Insert TBL_PRUEFM
      '
      '
      AdaptPruefm.Update(TblPruefm.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Update TBL_PRUEFM
      '
      '
      AdaptPruefm.Update(TblPruefm.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      '
      '
      '
      'TBL_PROART
      '
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptProart.InsertCommand = OleDBInsertCmd("TBL_PROART", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "PROART_ID"
      AdaptProart.UpdateCommand = OleDBUpdateCmd("TBL_PROART", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptProart.DeleteCommand = OleDBDeleteCmd("TBL_PROART", WhereKeyID, Cncol)
      '
      '
     
      'Delete/Update/Insert TBL_PROART
      '
      'Delete TBL_PROART
      '
      AdaptProart.Update(TblProart.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Insert TBL_PROART
      '
      '
      AdaptProart.Update(TblProart.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Update TBL_PROART
      '
      '
      AdaptProart.Update(TblProart.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'TBL_PARAM_LIST
      '
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptParamList.InsertCommand = OleDBInsertCmd("TBL_PARAM_LIST", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "PARAM_ID"
      AdaptParamList.UpdateCommand = OleDBUpdateCmd("TBL_PARAM_LIST", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptParamList.DeleteCommand = OleDBDeleteCmd("TBL_PARAM_LIST", WhereKeyID, Cncol)
      '
      '

      'Delete/Update/Insert TBL_PROART
      '
      'Delete TBL_PROART
      '
      AdaptParamList.Update(TblParameterList.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Insert TBL_PROART
      '
      '
      AdaptParamList.Update(TblParameterList.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Update TBL_PROART
      '
      '
      AdaptParamList.Update(TblParameterList.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      TblParameterList.AcceptChanges()
      TblProart.AcceptChanges()
      TblPruefm.AcceptChanges()
    End If
    Me.Close()
    Me.Dispose()
  End Sub

  Private Sub ConnParameter_CurrentChanged(sender As Object, e As System.EventArgs) Handles ConnParameter.CurrentChanged
    If IsNothing(ConnParameter) Then Exit Sub
    If IsNothing(ConnParameter.Current) Then Exit Sub
    ViewParameterList.RowFilter = "PARAM_ID=" & ConnParameter.Current("PARAM_ID")
    TblParameterList.Columns("PARAM_ID").DefaultValue = ConnParameter.Current("PARAM_ID")
    lblParameterList.Text = Texxt(419) & "(" & ConnParameter.Current("PARAM_KBEZ") & ")"
  End Sub
End Class