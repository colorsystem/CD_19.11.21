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
  Dim TblParamAusw As DataTable
  '
  '
  '
  Dim ViewList As DataView
  Dim ViewUserPAR As DataView
  Dim Viewpar As DataView
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
  Dim WithEvents ConnPAR As BindingSource

  Dim WithEvents MnflgParameter As C1TrueDBGrid
  Dim WithEvents MnflgDropParameter As C1TrueDBDropdown
  Public Sub New(ByRef flgParameter As C1TrueDBGrid, ByRef flgDropParameter As C1TrueDBDropdown, ByRef Farbwerte As ValuesGrpsAssigns)
    Dim i As Integer
    Dim Ianwsg As Integer
    Dim kn As Integer
    '
    TblPAR = New DataTable
    TblList = New DataTable
    TblUserPAR = New DataTable
    TblParamAusw = New DataTable
    '
    '
    '
    ViewList = New DataView(TblList)
    ViewUserPAR = New DataView(TblUserPAR)
    Viewpar = New DataView(TblPAR)
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
    Call UpdateLangText(TblPAR, 25000, "PARAM_ID", "PARAM_KBEZ", "PARAM_LBEZ")

    TblPAR.Columns.Add("LIST", GetType(Boolean))
    TblPAR.Columns("LIST").DefaultValue = True
    For i = 0 To TblPAR.Rows.Count - 1
      TblPAR.Rows(i)("LIST") = True
    Next
    TblPAR.AcceptChanges()
    '

    '
    'Vorgegebene Auswahl für Parameterwerete
    '
    '
    SqlStmt = "SELECT *  FROM TBL_PARAM_LIST"
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
    ConnPAR.DataSource = Viewpar
    '
    '
    'Aufbau Grid
    '
    '
    If Not IsNothing(Farbwerte) Then
      SqlStmt = "SELECT * FROM TBL_AUSW_PARAM"
      SCmdPAR.CommandText = SqlStmt
      If Not FillDatset(AdaptPAR, TblParamAusw) Then
        Exit Sub
      End If
      TblParamAusw.AcceptChanges()
      Viewpar.RowFilter = "PARAM_ID=-1"
      For Ianwsg = 0 To Farbwerte.Count - 1
        For kn = 0 To Farbwerte(Ianwsg).AuswID.Count - 1
          For i = 0 To TblParamAusw.Rows.Count - 1
            If Farbwerte(Ianwsg).AuswID(kn) = TblParamAusw.Rows(i)("AUSW_ID") Then
              Viewpar.RowFilter = Viewpar.RowFilter & " OR PARAM_ID=" & TblParamAusw.Rows(i)("PARAM_ID")
            End If
          Next
        Next
      Next
    End If
    '
    '
    '
    MnflgParameter = flgParameter

    MnflgDropParameter = flgDropParameter
    MnflgParameter.Columns.Clear()
    MnflgParameter.SetDataBinding(ConnPAR, "", False)
    MnflgParameter.AlternatingRows = True
    MnflgParameter.EvenRowStyle.BackColor = Color.Beige
    MnflgParameter.OddRowStyle.BackColor = Color.Bisque
    MnflgParameter.Columns(0).Caption = ""
    MnflgParameter.Columns(1).Caption = Texxt(283)
    MnflgParameter.Columns(2).Caption = Texxt(282)
    MnflgParameter.Columns(3).Caption = ""
    MnflgParameter.Columns(4).Caption = Texxt(243)
    MnflgParameter.Columns(5).Caption = Texxt(244)
    MnflgParameter.Columns(6).Caption = "  "
    '
    MnflgParameter.Columns(0).DataField = "PARAM_ID"
    MnflgParameter.Columns(1).DataField = "PARAM_LBEZ"
    MnflgParameter.Columns(2).DataField = "PARAM_KBEZ"
    MnflgParameter.Columns(3).DataField = "PARAM_FORM"
    MnflgParameter.Columns(4).DataField = "PARAM_WERT"
    MnflgParameter.Columns(5).DataField = "PARAM_BEZWRT"
    MnflgParameter.Columns(6).DataField = "LIST"
    MnflgParameter.Columns(6).ValueItems.Presentation = C1.Win.C1TrueDBGrid.PresentationEnum.CheckBox
    MnflgParameter.Columns(6).ValueItems.Translate = False
    MnflgDropParameter.ValueMember = "PARAM_BEZWRT"
    MnflgDropParameter.DisplayMember = "PARAM_BEZWRT"

    '
    '
    '
    MnflgDropParameter.DataSource = ViewList
    MnflgDropParameter.DisplayColumns("PARAM_ID").Visible = False
    MnflgDropParameter.DisplayColumns("PARAM_WERT").Visible = False
    MnflgDropParameter.ExtendRightColumn = True
    MnflgDropParameter.ColumnHeaders = False
    MnflgDropParameter.Height = 100
    '
    MnflgParameter.Splits(0).DisplayColumns(0).Width = 0
    MnflgParameter.Splits(0).DisplayColumns(1).Width = 250
    MnflgParameter.Splits(0).DisplayColumns(2).Width = 100
    MnflgParameter.Splits(0).DisplayColumns(3).Width = 0
    MnflgParameter.Splits(0).DisplayColumns(4).Width = 60
    MnflgParameter.Splits(0).DisplayColumns(5).Width = 300
    MnflgParameter.Splits(0).DisplayColumns(6).Width = 10
    MnflgParameter.Splits(0).DisplayColumns(6).Visible = True
    MnflgParameter.Splits(0).DisplayColumns(6).Button = False
    MnflgParameter.Splits(0).DisplayColumns(6).Locked = False
    '
    MnflgParameter.Splits(0).DisplayColumns(0).Locked = True
    MnflgParameter.Splits(0).DisplayColumns(1).Locked = True
    MnflgParameter.Splits(0).DisplayColumns(2).Locked = True
    MnflgParameter.Splits(0).DisplayColumns(3).Locked = True
    MnflgParameter.Splits(0).DisplayColumns(6).Locked = false
    MnflgParameter.ExtendRightColumn = True
    MnflgParameter.AllowAddNew = False
    MnflgParameter.AllowColMove = False
    MnflgParameter.AllowSort = False
    MnflgDropParameter.Width = MnflgParameter.Splits(0).DisplayColumns(5).Width
    MnflgDropParameter.DisplayColumns(2).Width = MnflgParameter.Splits(0).DisplayColumns(5).Width
    '
    '
    ConnPAR.Position = ConnPAR.Count - 1
    ConnPAR.Position = 0
    '
    '
  End Sub

  Sub FillGrid(ByVal UserID As Long, ByVal MethID As Long)
    Dim i As Integer
    Dim ParRows() As DataRow
    MnUserID = UserID
    MnMethID = MethID
    '
    MnflgParameter.EditActive = False
    ConnPAR.EndEdit()
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
    Dim WhereKeyID() As String
    MnflgParameter.EditActive = False
    ConnPAR.CurrencyManager.EndCurrentEdit()
    If TblUserPAR.Select(Nothing, Nothing, DataViewRowState.Deleted).Count = 0 _
       And TblUserPAR.Select(Nothing, Nothing, DataViewRowState.Added).Count = 0 _
       And TblUserPAR.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent).Count = 0 Then
      Exit Sub
    End If

    If AddModDelP(2990) Then
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
      TblUserPAR.AcceptChanges()
    End If

  End Sub
  Private Sub ConnPAR_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnPAR.CurrentChanged, MnflgParameter.Click
    If IsNothing(ConnPAR) Then Exit Sub
    If IsNothing(ConnPAR.Current) Then Exit Sub
    ConnPAR.CurrencyManager.EndCurrentEdit()
    ViewList.RowFilter = "PARAM_ID=" & ConnPAR.Current("PARAM_ID")
    If MnflgParameter Is Nothing Then Exit Sub
    If ViewList.Count > 0 And ConnPAR.Current("LIST") Then
      MnflgParameter.Columns(5).DropDown = MnflgDropParameter
    Else
      MnflgParameter.Columns(5).DropDown = Nothing
    End If


  End Sub

  Private Sub ConnPAR_CurrentItemChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnPAR.CurrentItemChanged
    Dim RowView As DataRowView
    If IsNothing(ConnPAR) Then Exit Sub
    If IsNothing(ConnPAR.Current) Then Exit Sub
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
    If IsDBNull(RowView("PARAM_BEZWRT")) Then
      RowView("PARAM_BEZWRT") = ""
    End If
    If RowView("PARAM_BEZWRT").length > TblUserPAR.Columns("PARAM_BEZWRT").MaxLength Then
      RowView("PARAM_BEZWRT") = RowView("PARAM_BEZWRT").substring(0, TblUserPAR.Columns("PARAM_BEZWRT").MaxLength)
    End If
    RowView("PARAM_CRNTL_ID") = 0
    RowView.EndEdit()
  End Sub
  Private Sub MnflgDropParameter_RowChange(sender As Object, e As System.EventArgs) Handles MnflgDropParameter.RowChange
    If IsNothing(ConnPAR) Then Exit Sub
    If ViewList.Count = 0 Then Exit Sub
    ConnPAR.Current("PARAM_WERT") = ViewList(MnflgDropParameter.Row)("PARAM_WERT")
    ConnPAR.Current("PARAM_BEZWRT") = ViewList(MnflgDropParameter.Row)("PARAM_BEZWRT")
    ConnPAR.CurrencyManager.EndCurrentEdit()
  End Sub
End Class