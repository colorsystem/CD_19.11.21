Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmGRZ
  Dim imsg As Integer
  Dim MaxID As Integer
  Dim Iopenart As Boolean
  Dim SqlStmt As String
  Dim MnMischID As Integer
  Dim MnMessgID As Integer
  Dim StrLinMSY As String
  Dim StrLinGrp As String
  Dim ier As Integer
  '
  '
  Dim txtGRP As List(Of TextBox)
  '
  '
  '
  Dim TblMSY As DataTable
  Dim TblGRP As DataTable
  Dim TblMES As DataTable
  Dim TblMESGRP As DataTable
  Dim TblMiMeGRP As DataTable
  Dim TblUse As DataTable
  Dim TblUseROSelect As DataTable
  Dim TblUseNDSelect As DataTable
  '
  '
  Dim ViewGRP As DataView
  Dim ViewMES As DataView
  Dim ViewMESGRP As DataView
  Dim ViewMiMeGRP As DataView
  Dim ViewUseRO As DataView
  Dim ViewUseND As DataView
  Dim ViewUseROSelect As DataView
  Dim ViewUseNDSelect As DataView

  '
  '
  Dim AdaptMSY As OleDbDataAdapter
  Dim AdaptGRP As OleDbDataAdapter
  Dim AdaptMES As OleDbDataAdapter
  Dim AdaptMESGRP As OleDbDataAdapter
  Dim AdaptMiMeGRP As OleDbDataAdapter
  Dim AdaptUSE As OleDbDataAdapter
  Dim AdaptUseROSelect As OleDbDataAdapter
  Dim AdaptUseNDSelect As OleDbDataAdapter
  '
  Dim CmdMSY As OleDbCommand
  Dim CmdGRP As OleDbCommand
  Dim CmdMES As OleDbCommand
  Dim cmdUSE As OleDbCommand
  Dim cmdMESGRP As OleDbCommand
  Dim cmdMiMeGRP As OleDbCommand
  Dim cmdUseROSelect As OleDbCommand
  Dim cmdUseNDSelect As OleDbCommand
  '
  '
  Dim WithEvents ConnMSY As BindingSource
  Dim WithEvents ConnMES As BindingSource
  Dim WithEvents ConnGRP As BindingSource
  Private Sub frmGRW_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    Me.Text = Texxt(417)
    btnORD.Text = Texxt(1999)
    lblGRP_0.Text = Texxt(386)
    lblGRP_1.Text = Texxt(282)
    lblGRP_2.Text = Texxt(283)
    lblGRP_3.Text = Texxt(264)
    lblMES.Text = Texxt(420)
    lblMSY.Text = Texxt(422)
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
    TblMSY = New DataTable
    TblMES = New DataTable
    TblMESGRP = New DataTable
    TblMiMeGRP = New DataTable
    TblGRP = New DataTable
    TblUse = New DataTable
    TblUseROSelect = New DataTable
    TblUseNDSelect = New DataTable
    '
    '
    ViewGRP = New DataView(TblGRP)
    ViewMES = New DataView(TblMES)
    ViewMESGRP = New DataView(TblMESGRP)
    ViewMiMeGRP = New DataView(TblMiMeGRP)
    ViewUseRO = New DataView(TblUse)
    ViewUseND = New DataView(TblUse)
    ViewUseROSelect = New DataView(TblUseROSelect)
    ViewUseNDSelect = New DataView(TblUseNDSelect)

    '
    '
    AdaptMSY = New OleDbDataAdapter
    AdaptMES = New OleDbDataAdapter
    AdaptMESGRP = New OleDbDataAdapter
    AdaptMiMeGRP = New OleDbDataAdapter
    AdaptGRP = New OleDbDataAdapter
    AdaptUse = New OleDbDataAdapter
    AdaptUseROSelect = New OleDbDataAdapter
    AdaptUseNDSelect = New OleDbDataAdapter
    '
    CmdMSY = New OleDbCommand("", Cncol)
    CmdGRP = New OleDbCommand("", Cncol)
    cmdUse = New OleDbCommand("", Cncol)
    CmdMES = New OleDbCommand("", Cncol)
    cmdMESGRP = New OleDbCommand("", Cncol)
    cmdMiMeGRP = New OleDbCommand("", Cncol)
    cmdUseROSelect = New OleDbCommand("", Cncol)
    cmdUseNDSelect = New OleDbCommand("", Cncol)
    '
    '
    ConnMSY = New BindingSource
    ConnMES = New BindingSource
    ConnGRP = New BindingSource
    '
    '
    BindingMSY.BindingSource = ConnMSY
    BindingGRP.BindingSource = ConnGRP
    BindingMES.BindingSource = ConnMES

    '
    '
    AdaptMSY.SelectCommand = CmdMSY
    AdaptMES.SelectCommand = CmdMES
    AdaptMESGRP.SelectCommand = cmdMESGRP
    AdaptMiMeGRP.SelectCommand = cmdMiMeGRP
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
    If MnmischID = -1 Then
      SqlStmt = "SELECT DISTINCT TBL_MISCH.MISCH_ID AS MISCH_ID,MISCH_KBEZ FROM TBL_MISCH INNER JOIN TBL_USER_MISCH ON TBL_MISCH.MISCH_ID=TBL_USER_MISCH.MISCH_ID ORDER BY TBL_MISCH.MISCH_KBEZ"
    Else
      SqlStmt = "SELECT MISCH_ID,MISCH_KBEZ FROM TBL_MISCH WHERE MISCH_ID=" & MnMischID & " ORDER BY MISCH_KBEZ"
    End If
    If Not Iopenart Then
      BindingMSY.Visible = False
      If Not IsNothing(BindingGRP.DeleteItem) Then
        BindingGRP.DeleteItem.Visible = False
      End If
    End If
    '
    CmdMSY.CommandText = SqlStmt
    If Not FillDatset(AdaptMSY, TblMSY) Then
      Exit Sub
    End If
    TblMSY.AcceptChanges()
    If TblMSY.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_MISCH")
      Exit Sub
    End If
    StrLinMSY = StrLin(TblMSY, "MISCH_ID")
    BindingMSY.Enabled = False
    '
    '
    '
    'Gruppen für Rezepte
    'TBL_MISCH_GROUP
    '
    '
    SqlStmt = "SELECT * FROM TBL_MISCH_GROUP WHERE MISCH_ID IN " & StrLinMSY & " ORDER BY GROUP_ID"
    TblGRP.Clear()
    CmdGRP.CommandText = SqlStmt
    If Not FillDatset(AdaptGRP, TblGRP) Then
      Exit Sub
    End If
    TblGRP.AcceptChanges()
    If TblGRP.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_MISCH_GROUP")
      Exit Sub
    End If
    StrLinGrp = StrLin(TblGRP, "GROUP_ID")
    '
    '
    '
    '
    'Messgeräte für Mischsysteme
    '
    'TBL_MESSG und TBL_MISCH_MESSG
    '
    If MnMessgID = -1 Then
      SqlStmt = "SELECT MESSG_KBEZ,TBL_MESSG.MESSG_ID AS MESSG_ID,MISCH_ID FROM TBL_MESSG INNER JOIN TBL_MISCH_MESSG ON TBL_MESSG.MESSG_ID=TBL_MISCH_MESSG.MESSG_ID WHERE MISCH_ID IN" & StrLinMSY
    Else
      SqlStmt = "SELECT MESSG_KBEZ,TBL_MESSG.MESSG_ID AS MESSG_ID,MISCH_ID FROM TBL_MESSG INNER JOIN TBL_MISCH_MESSG ON TBL_MESSG.MESSG_ID=TBL_MISCH_MESSG.MESSG_ID" _
      & " WHERE TBL_MESSG.MESSG_ID=" & MnMessgID & " AND MISCH_ID IN " & StrLinMSY
    End If
    TblMES.Clear()
    CmdMES.CommandText = SqlStmt
    If Not FillDatset(AdaptMES, TblMES) Then
      Exit Sub
    End If
    TblMES.AcceptChanges()
    If TblMES.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_MISCH_MESSG")
      Exit Sub
    End If
    If Not Iopenart Then
      BindingMES.Visible = False
    End If
    '
    'TBL_MESSG_GROUP
    '
    SqlStmt = "SELECT * FROM TBL_MESSG_GROUP ORDER BY GROUP_ID"
    TblMESGRP.Clear()
    cmdMESGRP.CommandText = SqlStmt
    If Not FillDatset(AdaptMESGRP, TblMESGRP) Then
      Exit Sub
    End If
    TblMESGRP.AcceptChanges()
    If TblMESGRP.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_MESSG_GROUP")
      Exit Sub
    End If

    '  
    '
    '
    'TBL_MISCH_GROUP_MESSG
    '
    SqlStmt = "SELECT * FROM TBL_MISCH_GROUP_MESSG"
    TblMiMeGRP.Clear()
    cmdMiMeGRP.CommandText = SqlStmt
    If Not FillDatset(AdaptMiMeGRP, TblMiMeGRP) Then
      Exit Sub
    End If
    TblMiMeGRP.AcceptChanges()
    '
    '
    '
    '
    '
    '
    SqlStmt = "SELECT TBL_USER.USER_ID AS USER_ID,TBL_USER_MISCH.MISCH_ID AS MISCH_ID,USER_NAME FROM TBL_USER INNER JOIN TBL_USER_MISCH ON TBL_USER.USER_ID=TBL_USER_MISCH.USER_ID WHERE MISCH_ID IN " & StrLinMSY
    cmdUSE.CommandText = SqlStmt
    TblUse.Clear()
    If Not FillDatset(AdaptUSE, TblUse) Then
      Exit Sub
    End If
    TblUse.AcceptChanges()
    If TblUse.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_USER_MISCH")
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
    SqlStmt = "SELECT TBL_USER_MISCH_GROUP_READONLY.MISCH_ID, TBL_USER_MISCH_GROUP_READONLY.GROUP_ID, TBL_USER.USER_ID, TBL_USER.USER_NAME" _
    & " FROM TBL_USER INNER JOIN TBL_USER_MISCH_GROUP_READONLY ON TBL_USER.USER_ID = TBL_USER_MISCH_GROUP_READONLY.USER_ID " _
    & " WHERE MISCH_ID IN " & StrLinMSY & " AND GROUP_ID IN " & StrLinGrp
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
    SqlStmt = "SELECT TBL_USER_MISCH_GROUP_DONTSHOW.MISCH_ID, TBL_USER_MISCH_GROUP_DONTSHOW.GROUP_ID, TBL_USER.USER_ID, TBL_USER.USER_NAME" _
    & " FROM TBL_USER INNER JOIN TBL_USER_MISCH_GROUP_DONTSHOW ON TBL_USER.USER_ID = TBL_USER_MISCH_GROUP_DONTSHOW.USER_ID " _
    & " WHERE MISCH_ID IN " & StrLinMSY & " AND GROUP_ID IN " & StrLinGrp
    '
    cmdUseNDSelect.CommandText = SqlStmt
    If Not FillDatset(AdaptUseNDSelect, TblUseNDSelect) Then
      Exit Sub
    End If
    TblUseNDSelect.AcceptChanges()




    BindingMSY.Enabled = True

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
    '
    cboMesGRP.DataSource = ViewMESGRP
    cboMesGRP.DisplayMember = "GROUP_KBEZ"
    cboMesGRP.ValueMember = "GROUP_ID"
    ViewMESGRP.Sort = "GROUP_ID"
    '
    '
    '
    ConnGRP.DataSource = ViewGRP
    ConnMES.DataSource = ViewMES
    ConnMSY.DataSource = TblMSY
    '
    '
    '
    cboMSY.DataSource = ConnMSY
    cboMSY.DisplayMember = "MISCH_KBEZ"
    cboMSY.ValueMember = "MISCH_ID" '
    '
    '
    cboMES.DataSource = ConnMES
    cboMES.DisplayMember = "MESSG_KBEZ"
    cboMES.ValueMember = "MESSG_ID"
    '
    '
    '
    cboGRP.DataSource = ConnGRP
    cboGRP.DisplayMember = "GROUP_KBEZ"
    cboGRP.ValueMember = "GROUP_ID" '
    '
    '
    txtMSY.DataBindings.Add("TEXT", ConnMSY, "MISCH_KBEZ")
    txtGRP_0.DataBindings.Add("TEXT", ConnGRP, "GROUP_ID")
    txtGRP_1.DataBindings.Add("TEXT", ConnGRP, "GROUP_KBEZ")
    txtGRP_2.DataBindings.Add("TEXT", ConnGRP, "GROUP_NAME")
    txtMES.DataBindings.Add("TEXT", ConnMES, "MESSG_KBEZ")
  End Sub




  Private Sub ConnMSY_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMSY.CurrentChanged
    If ConnMSY.Current Is Nothing OrElse ConnMSY.Count = 0 Then Exit Sub
    BindingMSY.Enabled = False
    
    '
    ViewUseRO.RowFilter = "MISCH_ID=" & ConnMSY.Current("MISCH_ID")
    ViewUseND.RowFilter = "MISCH_ID=" & ConnMSY.Current("MISCH_ID")
    '
    'Gruppen für R-Werte
    'TBL_MISCH_GROUP
    If Not IsNothing(ConnGRP.Current) Then
      ConnGRP.Current.endedit()
    End If
    ViewGRP.RowFilter = "MISCH_ID=" & ConnMSY.Current("MISCH_ID")
    ConnGRP.CurrencyManager.Refresh()
    ConnGRP.Position = 0
    '
    '

    '
    ''Messgeräte für Mischsystem
    If Not IsNothing(ConnMES.Current) Then
      ConnMES.Current.endedit()
    End If
    ViewMES.RowFilter = "MISCH_ID=" & ConnMSY.Current("MISCH_ID")
    ConnMES.CurrencyManager.Refresh()
    ConnMES.Position = 0
    BindingMSY.Enabled = True

  End Sub

  Private Sub ConnMES_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMES.CurrentChanged
    If Not IsNothing(ConnGRP.Current) Then
      ConnGRP.Current.endedit()
    End If
    ViewMESGRP.RowFilter = "MESSG_ID=" & ConnMES.Current("MESSG_ID")
    ConnGRP.CurrencyManager.Refresh()
    ConnGRP.Position = 0


  End Sub

  Private Sub ConnGRP_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnGRP.AddingNew

    For i = 0 To TblGRP.Columns.Count - 1
      TblGRP.Columns(i).DefaultValue = ConnGRP.Current(i)
    Next i
    MaxID = MaxDatTableID(TblGRP, "GROUP_ID", {"MISCH_ID"}, {ConnMSY.Current("MISCH_ID")}) + 1
    TblGRP.Columns("GROUP_ID").DefaultValue = MaxID
    TblGRP.Columns("GROUP_KBEZ").DefaultValue = TblGRP.Columns("GROUP_KBEZ").DefaultValue & " ????"
    If TblGRP.Columns("GROUP_KBEZ").DefaultValue.length > TblGRP.Columns("GROUP_KBEZ").MaxLength Then
      TblGRP.Columns("GROUP_KBEZ").DefaultValue = TblGRP.Columns("GROUP_KBEZ").DefaultValue.substring(0, TblGRP.Columns("GROUP_KBEZ").MaxLength)
    End If
  End Sub





  Private Sub ConnGRP_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnGRP.CurrentChanged
    If ConnMSY.Current Is Nothing OrElse ConnMSY.Count = 0 Then Exit Sub
    If ConnGRP.Current Is Nothing OrElse ConnGRP.Count = 0 Then Exit Sub
    If ConnMES.Current Is Nothing OrElse ConnMES.Count = 0 Then Exit Sub
    If cboMesGRP.DataSource Is Nothing Then Exit Sub
    ViewMiMeGRP.RowFilter = "MISCH_ID=" & ConnMSY.Current("MISCH_ID") & " AND MESSG_ID=" & ConnMES.Current("MESSG_ID") & " AND GROUP_ID=" & ConnGRP.Current("GROUP_ID")
    '
    '
    '
    If ViewMiMeGRP.Count = 0 Then
      cboMesGRP.SelectedValue = 0
    Else
      cboMesGRP.SelectedValue = ViewMiMeGRP(0)("MESSG_GID")
    End If
    ''
    'User für READ-Only
    '
    '
    '
    ViewUseROSelect.RowFilter = "MISCH_ID=" & ConnMSY.Current("MISCH_ID") & " AND GROUP_ID=" & ConnGRP.Current("GROUP_ID")
    '
    '
    '
    'User für "nicht anzeigen"
    '
    '
    '
    ViewUseNDSelect.RowFilter = "MISCH_ID=" & ConnMSY.Current("MISCH_ID") & " AND GROUP_ID=" & ConnGRP.Current("GROUP_ID")
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
    RowView("MISCH_ID") = lstUseND.SelectedItem("MISCH_ID")
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
    RowView("MISCH_ID") = lstUseRO.SelectedItem("MISCH_ID")
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
  
  Private Sub cboMesGRP_DropDownClosed(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMesGRP.DropDownClosed
    Dim RowView As DataRowView
    ' ViewMESGRP(ViewMiMeGRP())
    If cboMesGRP.SelectedValue = 0 Then
      '
      'Löschen, falls Gruppe GROUP_ID=0 ausgewählt
      '
      If ViewMiMeGRP.Count > 0 Then
        ViewMiMeGRP.Delete(0)
      End If
      Exit Sub
    Else
      '
      If ViewMiMeGRP.Count > 0 Then
        RowView = ViewMiMeGRP(0)
      Else
        '
        'Neuer Eintrag für MESSG_GID
        '
        RowView = ViewMiMeGRP.AddNew()
        RowView("MISCH_ID") = ConnMSY.Current("MISCH_ID")
        RowView("MESSG_ID") = ConnMES.Current("MESSG_ID")
        RowView("GROUP_ID") = ConnGRP.Current("GROUP_ID")
      End If
      RowView("MESSG_GID") = cboMesGRP.SelectedValue
      RowView.EndEdit()
    End If
  End Sub





  WriteOnly Property Openart() As Boolean
    Set(ByVal value As Boolean)
      Iopenart = value
    End Set
  End Property


  WriteOnly Property MischID() As Integer
    Set(ByVal value As Integer)
      MnMischID = value
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
      'TBL_MISCH_GROUP
      '
      ConnGRP.CurrencyManager.EndCurrentEdit()
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptGRP.InsertCommand = OleDBInsertCmd("TBL_MISCH_GROUP", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "MISCH_ID"
      WhereKeyID(1) = "GROUP_ID"
      AdaptGRP.UpdateCommand = OleDBUpdateCmd("TBL_MISCH_GROUP", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptGRP.DeleteCommand = OleDBDeleteCmd("TBL_MISCH_GROUP", WhereKeyID, Cncol)
      '
      '
      '
      '
      '
      '
      'TBL_USER_MISCH_GROUP_READONLY
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptUseROSelect.InsertCommand = OleDBInsertCmd("TBL_USER_MISCH_GROUP_READONLY", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MISCH_ID"
      WhereKeyID(2) = "GROUP_ID"
      AdaptUseROSelect.UpdateCommand = OleDBUpdateCmd("TBL_USER_MISCH_GROUP_READONLY", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptUseROSelect.DeleteCommand = OleDBDeleteCmd("TBL_USER_MISCH_GROUP_READONLY", WhereKeyID, Cncol)
      '
      '
      '
      '
      '
      '
      '
      'TBL_USER_MISCH_GROUP_DONTSHOW
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptUseNDSelect.InsertCommand = OleDBInsertCmd("TBL_USER_MISCH_GROUP_DONTSHOW", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MISCH_ID"
      WhereKeyID(2) = "GROUP_ID"
      AdaptUseNDSelect.UpdateCommand = OleDBUpdateCmd("TBL_USER_MISCH_GROUP_DONTSHOW", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptUseNDSelect.DeleteCommand = OleDBDeleteCmd("TBL_USER_MISCH_GROUP_DONTSHOW", WhereKeyID, Cncol)
      '
      '
      '
      '
      '
      '
      '
      'TBL_MISCH_GROUP_MESSG
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptMiMeGRP.InsertCommand = OleDBInsertCmd("TBL_MISCH_GROUP_MESSG", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "MISCH_ID"
      WhereKeyID(1) = "MESSG_ID"
      WhereKeyID(2) = "GROUP_ID"
      AdaptMiMeGRP.UpdateCommand = OleDBUpdateCmd("TBL_MISCH_GROUP_MESSG", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptMiMeGRP.DeleteCommand = OleDBDeleteCmd("TBL_MISCH_GROUP_MESSG", WhereKeyID, Cncol)
      'Delete/Update/Insert TBL_MISCH_GROUP, TBL_USER_MISCH_GROUP_READONLY,TBL_USER_MISCH_GROUP_DONTSHOW und TBL_MISCH_GROUP_MESSG
      '
      'Delete TBL_MISCH_GROUP_MESSG
      '
      AdaptMiMeGRP.Update(TblMiMeGRP.Select(Nothing, Nothing, DataViewRowState.Deleted))

      'Delete TBL_USER_MISCH_GROUP_READONLY
      '
      AdaptUseROSelect.Update(TblUseROSelect.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      'Delete TBL_USER_MISCH_GROUP_DONTSHOW
      '
      AdaptUseNDSelect.Update(TblUseNDSelect.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      'Delete TBL_MISCH_GROUP
      '
      AdaptGRP.Update(TblGRP.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Insert TBL_MISCH_GROUP
      '
      '
      AdaptGRP.Update(TblGRP.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      '
      'Insert TBL_USER_MISCH_GROUP_DONTSHOW
      '
      '
      AdaptUseNDSelect.Update(TblUseNDSelect.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      '
      '
      'Insert TBL_USER_MISCH_GROUP_READONLY
      '
      '
      AdaptUseROSelect.Update(TblUseROSelect.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      '
      '
      'Insert TBL_MISCH_GROUP_MESSG
      '
      '
      AdaptMiMeGRP.Update(TblMiMeGRP.Select(Nothing, Nothing, DataViewRowState.Added))
      '

      '
      '
      '
      'Update TBL_MISCH_GROUP
      '
      '
      AdaptGRP.Update(TblGRP.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      'Update TBL_USER_MISCH_GROUP_DONTSHOW
      '
      '
      AdaptUseNDSelect.Update(TblUseNDSelect.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      '
      'Update TBL_USER_MISCH_GROUP_READONLY
      '
      '
      AdaptUseROSelect.Update(TblUseROSelect.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      '
      '
      'Update TBL_MISCH_GROUP_MESSG
      '
      '
      AdaptMiMeGRP.Update(TblMiMeGRP.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
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
    MnMischID = -1
    MnMessgID = -1
  End Sub


  
  
End Class


