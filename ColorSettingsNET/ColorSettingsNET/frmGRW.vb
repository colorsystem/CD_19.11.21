Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmGRW
  Dim imsg As Integer
  Dim MaxID As Integer
  Dim Iopenart As Boolean
  Dim SqlStmt As String
  Dim MnMessgID As Integer
  Dim StrLinMsg As String
  Dim StrLinGrp As String
  Dim ier As Integer
  '
  '
  Dim txtGRP As List(Of TextBox)
  '
  '
  '
  Dim TblMSG As DataTable
  Dim TblGRP As DataTable
  Dim TblUse As DataTable
  Dim TblUseROSelect As DataTable
  Dim TblUseNDSelect As DataTable
  '
  '
  Dim ViewGRP As DataView
  Dim ViewUseRO As DataView
  Dim ViewUseND As DataView
  Dim ViewUseROSelect As DataView
  Dim ViewUseNDSelect As DataView

  '
  '
  Dim AdaptMSG As OleDbDataAdapter
  Dim AdaptGRP As OleDbDataAdapter
  Dim AdaptUse As OleDbDataAdapter
  Dim AdaptUseROSelect As OleDbDataAdapter
  Dim AdaptUseNDSelect As OleDbDataAdapter
  '
  Dim CmdMSG As OleDbCommand
  Dim CmdGRP As OleDbCommand
  Dim cmdUse As OleDbCommand
  Dim cmdUseROSelect As OleDbCommand
  Dim cmdUseNDSelect As OleDbCommand
  '
  '
  Dim WithEvents ConnMSG As BindingSource
  Dim WithEvents ConnGRP As BindingSource
  Private Sub frmGRW_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))

    Me.Text = Texxt(416)
    btnORD.Text = Texxt(1999)
    lblGRP_0.Text = Texxt(386)
    lblGRP_1.Text = Texxt(282)
    lblGRP_2.Text = Texxt(283)
    lblMES.Text = Texxt(420)

    lblUserRO.Text = Texxt(316)
    lblUserND.Text = Texxt(316)
    lblROSelect.Text = Texxt(317)
    lblNDSelect.Text = Texxt(318)
    '
    '
    '
    '
    '
    '
    '
    TblMSG = New DataTable
    TblGRP = New DataTable
    TblUse = New DataTable
    TblUseROSelect = New DataTable
    TblUseNDSelect = New DataTable
    '
    '
    ViewGRP = New DataView(TblGRP)
    ViewUseRO = New DataView(TblUse)
    ViewUseND = New DataView(TblUse)
    ViewUseROSelect = New DataView(TblUseROSelect)
    ViewUseNDSelect = New DataView(TblUseNDSelect)

    '
    '
    AdaptMSG = New OleDbDataAdapter
    AdaptGRP = New OleDbDataAdapter
    AdaptUse = New OleDbDataAdapter
    AdaptUseROSelect = New OleDbDataAdapter
    AdaptUseNDSelect = New OleDbDataAdapter
    '
    CmdMSG = New OleDbCommand("", Cncol)
    CmdGRP = New OleDbCommand("", Cncol)
    cmdUse = New OleDbCommand("", Cncol)
    cmdUseROSelect = New OleDbCommand("", Cncol)
    cmdUseNDSelect = New OleDbCommand("", Cncol)
    '
    '
    ConnMSG = New BindingSource
    ConnGRP = New BindingSource
    '
    '
    BindingMSG.BindingSource = ConnMSG
    BindingGRP.BindingSource = ConnGRP

    '
    '
    AdaptMSG.SelectCommand = CmdMSG
    AdaptGRP.SelectCommand = CmdGRP
    AdaptUse.SelectCommand = cmdUse
    AdaptUseROSelect.SelectCommand = cmdUseROSelect
    AdaptUseNDSelect.SelectCommand = cmdUseNDSelect
    '
    '
    '
    '
    txtGRP = New List(Of TextBox)
    txtGRP.Clear()
    txtGRP.Add(txtGRP_0)
    txtGRP.Add(txtGRP_1)
    txtGRP.Add(txtGRP_2)

    '
    '
    '
    '
    If MnMessgID = -1 Then
      SqlStmt = "SELECT DISTINCT TBL_MESSG.MESSG_ID AS MESSG_ID,MESSG_KBEZ FROM TBL_MESSG INNER JOIN TBL_USER_MESSG ON TBL_MESSG.MESSG_ID=TBL_USER_MESSG.MESSG_ID ORDER BY TBL_MESSG.MESSG_KBEZ"
    Else
      SqlStmt = "SELECT MESSG_ID,MESSG_KBEZ FROM TBL_MESSG WHERE MESSG_ID=" & MnMessgID & " ORDER BY MESSG_KBEZ"
    End If
    If Not Iopenart Then
      BindingMSG.Visible = False
      If Not IsNothing(BindingGRP.DeleteItem) Then
        BindingGRP.DeleteItem.Visible = False
      End If
    End If
    '
    CmdMSG.CommandText = SqlStmt
    If Not FillDatset(AdaptMSG, TblMSG) Then
      Exit Sub
    End If
    TblMSG.AcceptChanges()
    If TblMSG.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_MESSG")
      Exit Sub
    End If
    StrLinMsg = StrLin(TblMSG, "MESSG_ID")
    BindingMSG.Enabled = False
    '
    '
    '
    'Gruppen für R-Werte
    'TBL_MESSG_GROUP
    '
    '
    SqlStmt = "SELECT * FROM TBL_MESSG_GROUP WHERE MESSG_ID IN " & StrLinMsg & " ORDER BY GROUP_ID"
    TblGRP.Clear()
    CmdGRP.CommandText = SqlStmt
    If Not FillDatset(AdaptGRP, TblGRP) Then
      Exit Sub
    End If
    TblGRP.AcceptChanges()
    If TblGRP.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_MESSG_GROUP")
      Exit Sub
    End If
    StrLinGrp = StrLin(TblGRP, "GROUP_ID")
    '
    '  
    '
    SqlStmt = "SELECT TBL_USER.USER_ID AS USER_ID,TBL_USER_MESSG.MESSG_ID AS MESSG_ID,USER_NAME FROM TBL_USER INNER JOIN TBL_USER_MESSG ON TBL_USER.USER_ID=TBL_USER_MESSG.USER_ID WHERE MESSG_ID IN " & StrLinMsg
    cmdUse.CommandText = SqlStmt
    TblUse.Clear()
    If Not FillDatset(AdaptUse, TblUse) Then
      Exit Sub
    End If
    TblUse.AcceptChanges()
    If TblUse.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_USER_MESSG")
      Exit Sub
    End If
    '
    '
    '
    '
    '
    'User für READ-Only
    '
    '
    '
    SqlStmt = "SELECT TBL_USER_MESSG_GROUP_READONLY.MESSG_ID, TBL_USER_MESSG_GROUP_READONLY.GROUP_ID, TBL_USER.USER_ID, TBL_USER.USER_NAME" _
    & " FROM TBL_USER INNER JOIN TBL_USER_MESSG_GROUP_READONLY ON TBL_USER.USER_ID = TBL_USER_MESSG_GROUP_READONLY.USER_ID " _
    & " WHERE MESSG_ID IN " & StrLinMsg & " AND GROUP_ID IN " & StrLinGrp
    '
    cmdUseROSelect.CommandText = SqlStmt
    If Not FillDatset(AdaptUseROSelect, TblUseROSelect) Then
      Exit Sub
    End If
    TblUseROSelect.AcceptChanges()

    '
    '
    'User für "nicht anzeigen"
    '
    '
    '
    SqlStmt = "SELECT TBL_USER_MESSG_GROUP_DONTSHOW.MESSG_ID, TBL_USER_MESSG_GROUP_DONTSHOW.GROUP_ID, TBL_USER.USER_ID, TBL_USER.USER_NAME" _
    & " FROM TBL_USER INNER JOIN TBL_USER_MESSG_GROUP_DONTSHOW ON TBL_USER.USER_ID = TBL_USER_MESSG_GROUP_DONTSHOW.USER_ID " _
    & " WHERE MESSG_ID IN " & StrLinMsg & " AND GROUP_ID IN " & StrLinGrp
    '
    cmdUseNDSelect.CommandText = SqlStmt
    If Not FillDatset(AdaptUseNDSelect, TblUseNDSelect) Then
      Exit Sub
    End If
    TblUseNDSelect.AcceptChanges()




    BindingMSG.Enabled = True
    ConnGRP.DataSource = ViewGRP
    ConnMSG.DataSource = TblMSG
    '
    '
    cboMSG.DataSource = ConnMSG
    cboMSG.DisplayMember = "MESSG_KBEZ"
    cboMSG.ValueMember = "MESSG_ID"
    '
    '
    '
    cboGRP.DataSource = ConnGRP
    cboGRP.DisplayMember = "GROUP_KBEZ"
    cboGRP.ValueMember = "GROUP_ID" '
    '
    '
    lstUseRO.DataSource = ViewUseRO
    lstUseRO.DisplayMember = "USER_NAME"
    lstUseRO.ValueMember = "USER_ID"
    '
    lstUseND.DataSource = ViewUseND
    lstUseND.DisplayMember = "USER_NAME"
    lstUseND.ValueMember = "USER_ID"
    '
    '
    lstUseROSelect.DataSource = ViewUseROSelect
    lstUseROSelect.DisplayMember = "USER_NAME"
    lstUseROSelect.ValueMember = "USER_ID"
    '
    '
    '
    lstUseNDSelect.DataSource = ViewUseNDSelect
    lstUseNDSelect.DisplayMember = "USER_NAME"
    lstUseNDSelect.ValueMember = "USER_ID"


    '
    '
    txtMSG.DataBindings.Add("TEXT", ConnMSG, "MESSG_KBEZ")
    txtGRP_0.DataBindings.Add("TEXT", ConnGRP, "GROUP_ID")
    txtGRP_1.DataBindings.Add("TEXT", ConnGRP, "GROUP_KBEZ")
    txtGRP_2.DataBindings.Add("TEXT", ConnGRP, "GROUP_NAME")
  End Sub


  

  Private Sub ConnMSG_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMSG.CurrentChanged
    If ConnMSG.Current Is Nothing OrElse ConnMSG.Count = 0 Then Exit Sub
    BindingMSG.Enabled = False

    ViewUseRO.RowFilter = "MESSG_ID=" & ConnMSG.Current("MESSG_ID")
    ViewUseND.RowFilter = "MESSG_ID=" & ConnMSG.Current("MESSG_ID")
    '
    ViewGRP.RowFilter = "MESSG_ID=" & ConnMSG.Current("MESSG_ID")
    ConnGRP.CurrencyManager.Refresh()
    ConnGRP.Position = 0
    '
    '
    'Gruppen für R-Werte
    'TBL_MESSG_GROUP
    '
    '
    'datGRP.Recordset = DBas.OpenRecordset(SqlStmt, dbOpenDynaset, dbConsistent, dbReadOnly)
    BindingMSG.Enabled = True

  End Sub



  Private Sub ConnGRP_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnGRP.AddingNew

    For i = 0 To TblGRP.Columns.Count - 1
      TblGRP.Columns(i).DefaultValue = ConnGRP.Current(i)
    Next i
    MaxID = MaxDatTableID(TblGRP, "GROUP_ID", {"MESSG_ID"}, {ConnMSG.Current("MESSG_ID")}) + 1
    TblGRP.Columns("GROUP_ID").DefaultValue = MaxID
    TblGRP.Columns("GROUP_KBEZ").DefaultValue = TblGRP.Columns("GROUP_KBEZ").DefaultValue & " ????"
    If TblGRP.Columns("GROUP_KBEZ").DefaultValue.length > TblGRP.Columns("GROUP_KBEZ").MaxLength Then
      TblGRP.Columns("GROUP_KBEZ").DefaultValue = TblGRP.Columns("GROUP_KBEZ").DefaultValue.substring(0, TblGRP.Columns("GROUP_KBEZ").MaxLength)
    End If
  End Sub





  Private Sub ConnGRP_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnGRP.CurrentChanged
    If ConnMSG.Current Is Nothing OrElse ConnMSG.Count = 0 Then Exit Sub
    If ConnGRP.Current Is Nothing OrElse ConnGRP.Count = 0 Then Exit Sub
    '
    '
    'User für READ-Only
    '
    '
    '
    ViewUseROSelect.RowFilter = "MESSG_ID=" & ConnMSG.Current("MESSG_ID") & " AND GROUP_ID=" & ConnGRP.Current("GROUP_ID")
    '
    '
    '
    'User für "nicht anzeigen"
    '
    '
    '
    ViewUseNDSelect.RowFilter = "MESSG_ID=" & ConnMSG.Current("MESSG_ID") & " AND GROUP_ID=" & ConnGRP.Current("GROUP_ID")
  End Sub

  Private Sub lstUseND_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstUseND.Click
    Dim i As Integer
    Dim RowView As DataRowView
    For i = 0 To ViewUseNDSelect.Count - 1
      If lstUseND.SelectedValue = ViewUseNDSelect(i)("User_id") Then
        Exit Sub
      End If
    Next
    '
    '
    
    '
    '
    '
    RowView = ViewUseNDSelect.AddNew()
    RowView("USER_ID") = lstUseND.SelectedItem("USER_ID")
    RowView("USER_NAME") = lstUseND.SelectedItem("USER_NAME")
    RowView("MESSG_ID") = lstUseND.SelectedItem("MESSG_ID")
    RowView("GROUP_ID") = ConnGRP.Current("GROUP_ID")
    RowView.EndEdit()
  End Sub

  

  Private Sub lstUseNDSelect_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstUseNDSelect.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call lstUseNDSelect_KeyDown(sender, ev)
  End Sub
  Private Sub lstUseNDSelect_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstUseNDSelect.KeyDown
    Dim i As Integer
    For i = 0 To ViewUseNDSelect.Count - 1
      If ViewUseNDSelect(i)("USER_ID") = lstUseNDSelect.SelectedValue Then
        ViewUseNDSelect.Delete(i)
        Exit Sub
      End If
    Next
  End Sub
  

  Private Sub lstUseRO_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstUseRO.Click
    Dim i As Integer
    Dim RowView As DataRowView
    For i = 0 To ViewUseROSelect.Count - 1
      If lstUseRO.SelectedValue = ViewUseROSelect(i)("User_id") Then
        Exit Sub
      End If
    Next
    '
    '

    '
    '
    '
    RowView = ViewUseROSelect.AddNew()
    RowView("USER_ID") = lstUseRO.SelectedItem("USER_ID")
    RowView("USER_NAME") = lstUseRO.SelectedItem("USER_NAME")
    RowView("MESSG_ID") = lstUseRO.SelectedItem("MESSG_ID")
    RowView("GROUP_ID") = ConnGRP.Current("GROUP_ID")
    RowView.EndEdit()
  End Sub
 

  Private Sub lstUseROSelect_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstUseROSelect.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call lstUseROSelect_KeyDown(sender, ev)
  End Sub
  Private Sub lstUseROSelect_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstUseROSelect.KeyDown
    Dim i As Integer
    For i = 0 To ViewUseROSelect.Count - 1
      If ViewUseROSelect(i)("USER_ID") = lstUseROSelect.SelectedValue Then
        ViewUseROSelect.Delete(i)
        Exit Sub
      End If
    Next
  End Sub


  WriteOnly Property Openart() As Boolean
    Set(ByVal value As Boolean)
      Iopenart = value
    End Set
  End Property


  WriteOnly Property MessgID() As Integer
    Set(ByVal value As Integer)
      MnMessgID = value
    End Set
  End Property

  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    Dim WhereKeyID() As String
    If AddDelP(2990) Then
      '
      '
      '
      'TBL_MESSG_GROUP
      '
      ConnGRP.CurrencyManager.EndCurrentEdit()
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptGRP.InsertCommand = OleDBInsertCmd("TBL_MESSG_GROUP", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "MESSG_ID"
      WhereKeyID(1) = "GROUP_ID"
      AdaptGRP.UpdateCommand = OleDBUpdateCmd("TBL_MESSG_GROUP", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptGRP.DeleteCommand = OleDBDeleteCmd("TBL_MESSG_GROUP", WhereKeyID, Cncol)
      '
      '
      '
      '
      '
      '
      'TBL_USER_MESSG_GROUP_READONLY
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptUseROSelect.InsertCommand = OleDBInsertCmd("TBL_USER_MESSG_GROUP_READONLY", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MESSG_ID"
      WhereKeyID(2) = "GROUP_ID"
      AdaptUseROSelect.UpdateCommand = OleDBUpdateCmd("TBL_USER_MESSG_GROUP_READONLY", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptUseROSelect.DeleteCommand = OleDBDeleteCmd("TBL_USER_MESSG_GROUP_READONLY", WhereKeyID, Cncol)
      '
      '
      '
      '
      '
      '
      '
      'TBL_USER_MESSG_GROUP_DONTSHOW
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptUseNDSelect.InsertCommand = OleDBInsertCmd("TBL_USER_MESSG_GROUP_DONTSHOW", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MESSG_ID"
      WhereKeyID(2) = "GROUP_ID"
      AdaptUseNDSelect.UpdateCommand = OleDBUpdateCmd("TBL_USER_MESSG_GROUP_DONTSHOW", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptUseNDSelect.DeleteCommand = OleDBDeleteCmd("TBL_USER_MESSG_GROUP_DONTSHOW", WhereKeyID, Cncol)
      '
      'Delete/Update/Insert TBL_MESSG_GROUP, TBL_USER_MESSG_GROUP_READONLY und TBL_USER_MESSG_GROUP_DONTSHOW
      '
      'Delete TBL_USER_MESSG_GROUP_READONLY
      '
      AdaptUseROSelect.Update(TblUseROSelect.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      'Delete TBL_USER_MESSG_GROUP_DONTSHOW
      '
      AdaptUseNDSelect.Update(TblUseNDSelect.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      'Delete TBL_MESSG_GROUP
      '
      AdaptGRP.Update(TblGRP.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Insert TBL_MESSG_GROUP
      '
      '
      AdaptGRP.Update(TblGRP.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      '
      'Insert TBL_USER_MESSG_GROUP_DONTSHOW
      '
      '
      AdaptUseNDSelect.Update(TblUseNDSelect.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      '
      '
      'Insert TBL_USER_MESSG_GROUP_READONLY
      '
      '
      AdaptUseROSelect.Update(TblUseROSelect.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      '
      '
      '
      'Update TBL_MESSG_GROUP
      '
      '
      AdaptGRP.Update(TblGRP.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      'Update TBL_USER_MESSG_GROUP_DONTSHOW
      '
      '
      AdaptUseNDSelect.Update(TblUseNDSelect.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      '
      'Update TBL_USER_MESSG_GROUP_READONLY
      '
      '
      AdaptUseROSelect.Update(TblUseROSelect.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      '
      '
      '
      '
    End If
    Me.Close()
    Me.Dispose()
  End Sub

  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
    MnMessgID = -1
  End Sub
End Class