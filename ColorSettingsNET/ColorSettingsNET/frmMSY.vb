Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmMSY
  REM Laufvariable
  Dim i As Integer
  Dim l As Integer
  Dim k As Integer
  Dim j As Integer
  Dim kw As Integer
  REM Zwischengroessen
  Dim nkw As Integer
  'Message Kennung
  Dim imsg As Integer            'Messagebox (Rückgabewert)
  Dim MaxID As Long             'Maximale ID (Primärschlüssel)
  Dim ier As Integer
  Dim SqlStmt As String
  Dim cboMessArt As List(Of ComboBox)
  Dim lblMessArt As List(Of Label)
  Dim MnIopenArt As Boolean
  Dim MnUserUfo As Boolean

  '
  '
  Dim TblMSY As DataTable
  Dim TblMESSG As DataTable
  Dim TblMESSB As DataTable
  '
  Dim CmdMSY As OleDbCommand
  Dim CmdMESSG As OleDbCommand
  Dim CmdMESSB As OleDbCommand
  '
  '
  '
  '
  '
  Dim AdaptMSY As OleDbDataAdapter
  Dim AdaptMESSG As OleDbDataAdapter
  Dim AdaptMESSB As OleDbDataAdapter
  '
  '
  Dim dsMSY As DataSet
  Dim RelMSY As DataRelation
  '
  '
  Dim ViewMESSG As DataView
  '
  '
  Dim WithEvents ConnMSY As BindingSource
  Dim WithEvents ConnMESSG As BindingSource


  Private Sub frmMSY_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    '
    Me.Text = Texxt(406)
    btnORD.Text = Texxt(1999)
    lblMEC.Text = Texxt(234)
    lblMES.Text = Texxt(233)
    lblMessNam.Text = Texxt(282)
    lblMessArt_0.Text = Texxt(227)
    lblMessArt_1.Text = Texxt(228)
    lblMSY_0.Text = Texxt(282)
    lblMSY_1.Text = Texxt(283)
    lblMSY_2.Text = Texxt(277)
    lblMSY_3.Text = Texxt(278)
    lblMSY_4.Text = Texxt(249)

    '
    TblMSY = New DataTable
    TblMESSG = New DataTable
    TblMESSB = New DataTable
    '
    '
    CmdMSY = New OleDbCommand("", Cncol)
    CmdMESSG = New OleDbCommand("", Cncol)
    CmdMESSB = New OleDbCommand("", Cncol)
    '
    '
    '
    AdaptMSY = New OleDbDataAdapter
    AdaptMESSG = New OleDbDataAdapter
    AdaptMESSB = New OleDbDataAdapter
    '
    '
    '
    ViewMESSG = New DataView(TblMESSG)
    '
    '
    ConnMSY = New BindingSource
    '
    AdaptMSY.SelectCommand = CmdMSY
    AdaptMESSB.SelectCommand = CmdMESSB
    AdaptMESSG.SelectCommand = CmdMESSG
    '
    '
    '
    '
    '
    ConnMSY = New BindingSource
    ConnMESSG = New BindingSource
    '
    '
    '
    lblMessArt = New List(Of Label)
    lblMessArt.Add(lblMessArt_0)
    lblMessArt.Add(lblMessArt_1)
    '
    '
    '
    cboMessArt = New List(Of ComboBox)
    cboMessArt.Add(cboMessArt_0)
    cboMessArt.Add(cboMessArt_1)

    For i = 0 To 1
      cboMessArt(i).Items.Clear()
      cboMessArt(i).Items.Add(Texxt(225))
      cboMessArt(i).Items.Add(Texxt(226))
    Next i
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
    '
    '
    '
    'MISCHSYSTEME
    '
    SqlStmt = "SELECT * FROM TBL_MISCH ORDER BY MISCH_KBEZ"
    CmdMSY.CommandText = SqlStmt
    If Not FillDatset(AdaptMSY, TblMSY) Then
      Exit Sub
    End If
    TblMSY.AcceptChanges()
    '
    '
    '
    '
    'Messgeräte (alle)
    '
    SqlStmt = "SELECT * FROM TBL_MESSG ORDER BY MESSG_KBEZ"
    CmdMESSB.CommandText = SqlStmt
    If Not FillDatset(AdaptMESSB, TblMESSB) Then
      Exit Sub
    End If
    TblMESSB.AcceptChanges()
    '
    '
    '
    '
    '
    '
    'Messgeräte für Mischsysteme
    '
    SqlStmt = "SELECT TBL_MISCH_MESSG.MISCH_ID AS MISCH_ID,TBL_MISCH_MESSG.MESSG_ID AS MESSG_ID,MESSG_KBEZ,MESSG_SET,MESSG_REFTRA,MESSART_ID" _
    & " FROM TBL_MISCH_MESSG INNER JOIN TBL_MESSG ON TBL_MISCH_MESSG.MESSG_ID=TBL_MESSG.MESSG_ID ORDER BY TBL_MISCH_MESSG.MESSG_ID"

    CmdMESSG.CommandText = SqlStmt
    If Not FillDatset(AdaptMESSG, TblMESSG) Then
      Exit Sub
    End If
    TblMESSG.AcceptChanges()
    '
    '
    dsMSY = New DataSet
    dsMSY.Tables.Add(TblMSY)
    dsMSY.Tables.Add(TblMESSG)
    '
    RelMSY = New DataRelation("RELMSY", TblMSY.Columns("MISCH_ID"), TblMESSG.Columns("MISCH_ID"))
    dsMSY.Relations.Add(RelMSY)
    dsMSY.EnforceConstraints = True
    '
    '
    '
    '
    ConnMESSG.DataSource = ViewMESSG
    ConnMSY.DataSource = TblMSY
    '
    '
    '
    txtMSY_0.DataBindings.Add("TEXT", ConnMSY, "MISCH_ID")
    txtMSY_1.DataBindings.Add("TEXT", ConnMSY, "MISCH_KBEZ")
    txtMSY_1.MaxLength = TblMSY.Columns("MISCH_KBEZ").MaxLength
    txtMSY_2.DataBindings.Add("TEXT", ConnMSY, "MISCH_LBEZ")
    txtMSY_2.MaxLength = TblMSY.Columns("MISCH_LBEZ").MaxLength
    txtMSY_3.DataBindings.Add("TEXT", ConnMSY, "MISCH_TBL")
    txtMSY_3.MaxLength = TblMSY.Columns("MISCH_TBL").MaxLength
    txtMSY_4.DataBindings.Add("TEXT", ConnMSY, "MISCH_EINM")
    txtMSY_4.MaxLength = TblMSY.Columns("MISCH_EINM").MaxLength
    txtMSY_5.DataBindings.Add("TEXT", ConnMSY, "MISCH_EIND")
    txtMSY_5.MaxLength = TblMSY.Columns("MISCH_EIND").MaxLength
    txtMSY_6.DataBindings.Add("TEXT", ConnMSY, "MISCH_SET")
    '
    '
    cboMSY.DataSource = ConnMSY
    cboMSY.DisplayMember = "MISCH_KBEZ"
    cboMSY.ValueMember = "MISCH_ID"
    BindingMSY.BindingSource = ConnMSY
    '
    '
    lstMESSB.DataSource = TblMESSB
    lstMESSB.DisplayMember = "MESSG_KBEZ"
    lstMESSB.ValueMember = "MESSG_ID"
    '
    '
    '
    '

    lstMESSG.DataSource = ConnMESSG
    lstMESSG.DisplayMember = "MESSG_KBEZ"
    lstMESSG.ValueMember = "MESSG_ID"
    '
    '
    txtMessArt.DataBindings.Add("TEXT", ConnMESSG, "MESSART_ID")

    '
    '
    '


  End Sub

  Private Sub ConnMSY_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnMSY.AddingNew
    Dim i As Integer
    Dim Leng As Integer
    txtMessArt.Text = ""
    lblMessNam.Text = ""
    cboMessArt(0).SelectedIndex = -1
    cboMessArt(1).SelectedIndex = -1
    If ConnMSY.Count = 0 Then
      TblMSY.Columns("MISCH_KBEZ").DefaultValue = "????"
      TblMSY.Columns("MISCH_LBEZ").DefaultValue = "????"
      TblMSY.Columns("MISCH_SET").DefaultValue = False
      TblMSY.Columns("MISCH_TBL").DefaultValue = "    "
      TblMSY.Columns("MISCH_EINM").DefaultValue = "gr"
      TblMSY.Columns("MISCH_EIND").DefaultValue = "mm"
    Else
      For i = 0 To TblMSY.Columns.Count - 1
        If Not TblMSY.Columns(i).AutoIncrement Then
          TblMSY.Columns(i).DefaultValue = ConnMSY.Current(i)
        End If
      Next i
    End If
    Leng = Min(TblMSY.Columns("MISCH_KBEZ").MaxLength - 4, TblMSY.Columns("MISCH_KBEZ").DefaultValue.length)
    TblMSY.Columns("MISCH_KBEZ").DefaultValue = TblMSY.Columns("MISCH_KBEZ").DefaultValue.substring(0, Leng - 4) & " ????"
    If TblMSY.Columns("MISCH_KBEZ").DefaultValue.length > TblMSY.Columns("MISCH_KBEZ").MaxLength Then
      TblMSY.Columns("MISCH_KBEZ").DefaultValue = TblMSY.Columns("MISCH_KBEZ").DefaultValue.substring(0, TblMSY.Columns("MISCH_KBEZ").MaxLength)
    End If
  End Sub
  '
  '
  '
  '


  Private Sub ConnMSY_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMSY.CurrentChanged
    If ConnMSY.Current Is Nothing OrElse ConnMSY.Count = 0 Then Exit Sub
   

    BindingMSY.Enabled = False
    '
    '
    If ConnMSY.Current.row.rowstate = DataRowState.Detached Then
      ConnMSY.CurrencyManager.EndCurrentEdit()
      '
      Call AddNewViewItem(ConnMSY.Current("MISCH_ID"), "MISCH_ID", ViewMESSG)
      '
    End If

    '
    'Mischsystem abhängige Messgeräte
    '
    If Not IsNothing(ConnMESSG.Current) Then
      ConnMESSG.Current.endedit()
    End If
    ViewMESSG.RowFilter = "MISCH_ID=" & ConnMSY.Current("MISCH_ID")
    ConnMESSG.CurrencyManager.Refresh()
    ConnMESSG.Position = 0
    '
    '
    '
    BindingMSY.Enabled = True
    '
    '
    '
    '
    '
    '
    ''
  End Sub

  Private Sub ConnMESSG_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMESSG.CurrentChanged
    Dim i As Integer
    If ConnMSY.Current Is Nothing OrElse ConnMSY.Count = 0 Then Exit Sub
    If ConnMESSG.Current Is Nothing OrElse ConnMESSG.Count = 0 Then
      txtMessArt.Text = ""
      lblMessNam.Text = ""
      cboMessArt(0).SelectedIndex = -1
      cboMessArt(1).SelectedIndex = -1
      Exit Sub
    End If
    If ConnMESSG.Current.row.rowstate = DataRowState.Detached Then Exit Sub
    lblMessNam.Text = ConnMESSG.Current("Messg_kbez")

    Select Case ConnMESSG.Current("Messg_RefTra")
      Case "A"
        For i = 0 To 1
          cboMessArt(i).Enabled = True
          lblMessArt(i).Visible = True
          cboMessArt(i).Visible = True
          cboMessArt(i).SelectedIndex = BitInt(CShort(i), CShort(i), ConnMESSG.Current("MESSART_ID"))
        Next i
      Case "R"
        For i = 0 To 1
          lblMessArt(i).Visible = True
          cboMessArt(i).Visible = True
          cboMessArt(i).SelectedIndex = 0
          cboMessArt(i).Enabled = False
        Next i
      Case "T"
        For i = 0 To 1
          lblMessArt(i).Visible = True
          cboMessArt(i).Visible = True
          cboMessArt(i).SelectedIndex = 1
          cboMessArt(i).Enabled = False
        Next i
    End Select
    If ConnMESSG.Current("MESSG_SET") And ConnMSY.Current("MISCH_SET") And Not MnUserUfo Then
      cboMessArt(0).Enabled = False
      cboMessArt(1).Enabled = False
    End If
  End Sub

  Private Sub lstMESSB_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstMESSB.Click
    Dim i As Integer
    Dim RowView As DataRowView
    If lstMESSB.SelectedIndex < 0 Then Exit Sub
    For i = 0 To ViewMESSG.Count - 1
      If lstMESSB.SelectedValue = ViewMESSG(i)("MESSG_ID") Then
        Exit Sub
      End If
    Next
    '
    '
    '
    RowView = ViewMESSG.AddNew()
    RowView("MISCH_ID") = ConnMSY.Current("MISCH_ID")
    RowView("MESSG_ID") = lstMESSB.SelectedValue
    RowView("MESSG_KBEZ") = lstMESSB.Text
    RowView("MESSART_ID") = 0
    RowView("MESSG_REFTRA") = lstMESSB.SelectedItem("MESSG_REFTRA")
    RowView("MESSG_SET") = False

    RowView.EndEdit()
    lblMessNam.Text = lstMESSB.Text
    ConnMESSG.Position = ViewMESSG.Count - 1
    ConnMESSG.CurrencyManager.Refresh()
  End Sub


  Private Sub lstMESSG_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstMESSG.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call lstMESSG_KeyDown(sender, ev)
  End Sub

  Private Sub lstMESSG_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstMESSG.KeyDown
    ViewMESSG.Delete(lstMESSG.SelectedIndex)
  End Sub



  Private Sub cboMessArt_Selectedindexchanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMessArt_0.SelectedIndexChanged, cboMessArt_1.SelectedIndexChanged
    txtMessArt.Text = 2 * cboMessArt(1).SelectedIndex + cboMessArt(0).SelectedIndex
  End Sub

  Private Sub txtMessArt_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtMessArt.TextChanged
    If txtMessArt.Text = "" Then Exit Sub
    cboMessArt(1).SelectedIndex = Fix(txtMessArt.Text / 2)
    cboMessArt(0).SelectedIndex = txtMessArt.Text Mod 2
  End Sub


  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    Dim i As Integer
    Dim j As Integer
    Dim MischKbez As String
    Dim MischID As Integer
    Dim MischAdd() As Integer
    Dim cmdHILF As New OleDbCommand
    Dim ViewHilf As New DataView(TblMSY)
    Dim WhereKeyID() As String
    If ConnMSY.Count = 0 Then
      MsgBox(Texxt(3761))
      Me.Close()
      Exit Sub
    End If
    If AddDelP(2990) Then
      '
      '
      ConnMSY.CurrencyManager.EndCurrentEdit()
      ConnMESSG.CurrencyManager.EndCurrentEdit()
      '
      ViewHilf.RowStateFilter = DataViewRowState.Deleted And DataViewRowState.OriginalRows
      If ViewHilf.Count > 0 Then
        MischKbez = Chr(13)
        For i = 0 To ViewHilf.Count - 1
          MischKbez = MischKbez & ViewHilf(i)("MISCH_KBEZ") & Chr(13)
        Next
        If MessageBox.Show(MischKbez & Texxt(2661), Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.No Then
          ViewHilf.Dispose()
          Me.Close()
          Me.Dispose()
          Exit Sub
        End If
      End If
      For i = 0 To ViewHilf.Count - 1
        MischID = ViewHilf(i)("MISCH_ID")
        '
        '
        'TBL_GRUND_FARBM
        '
        SqlStmt = "DELETE * FROM TBL_GRUND_FARBM WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        'TBL_REZEPT_FARBM
        '
        '
        SqlStmt = "DELETE * FROM TBL_REZEPT_FARBM WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'TBL_REZEPT_RWERT
        '
        '
        SqlStmt = "DELETE * FROM TBL_REZEPT_RWERT WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'TBL_REZEPT
        '
        '
        '
        SqlStmt = "DELETE * FROM TBL_REZEPT WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'TBL_SOPRTI_FARBM
        '
        '
        '
        SqlStmt = "DELETE * FROM TBL_SORTI_FARBM WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'TBL_SORTI_RWERT
        '
        '
        '
        SqlStmt = "DELETE * FROM TBL_SORTI_RWERT WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'TBL_SORTI
        '
        '
        '
        SqlStmt = "DELETE * FROM TBL_SORTI WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'TBL_FARBM_PREIS
        '
        '
        '
        SqlStmt = "DELETE * FROM TBL_FARBM_PREIS WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'TBL_FARBM_PROZ
        '
        '
        '
        SqlStmt = "DELETE * FROM TBL_FARBM_PROZ WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'TBL_FARBM_PROB
        '
        '
        '
        SqlStmt = "DELETE * FROM TBL_FARBM_PROB WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'TBL_FARBM
        '
        '
        '
        SqlStmt = "DELETE * FROM TBL_FARBM WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
          Exit Sub
        End If
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
        '
        'TBL_USER_MISCH_GROUP_READONLY
        '
        SqlStmt = "DELETE * FROM TBL_USER_MISCH_GROUP_READONLY WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cncol) <> 0 Then
          Exit Sub
        End If
        '
        'TBL_USER_MISCH_GROUP_DONTSHOW
        '
        SqlStmt = "DELETE * FROM TBL_USER_MISCH_GROUP_DONTSHOW WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cncol) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'TBL_MISCH_GROUP_MESSG
        '
        '
        SqlStmt = "DELETE * FROM TBL_MISCH_GROUP_MESSG WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cncol) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        '
        'TBL_USER_MISCH_MESSG
        '
        '
        SqlStmt = "DELETE * FROM TBL_USER_MISCH_MESSG WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cncol) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        '
        '
        '
        'TBL_USER_METH_MISCH
        '
        SqlStmt = "DELETE * FROM TBL_USER_METH_MISCH WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cncol) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'TBL_USER_MISCH
        '
        '
        SqlStmt = "DELETE * FROM TBL_USER_MISCH WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cncol) <> 0 Then
          Exit Sub
        End If ''
        '
        '
        '
        '
        'TBL_MISCH_GROUP
        '
        SqlStmt = "DELETE * FROM TBL_MISCH_GROUP WHERE MISCH_ID=" & MischID
        cmdHILF.CommandText = SqlStmt
        If SQLExeNonQuery(cmdHILF, Cncol) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        '
      Next i
      '
      ViewHilf.RowStateFilter = DataViewRowState.Added
      ReDim MischAdd(ViewHilf.Count - 1)
      For i = 0 To ViewHilf.Count - 1
        MischAdd(i) = ViewHilf(i)("MISCH_ID")
      Next
      '
      '
      'TBL_MISCH_MESSG
      '
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptMESSG.InsertCommand = OleDBInsertCmd("TBL_MISCH_MESSG", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "MISCH_ID"
      WhereKeyID(1) = "MESSG_ID"
      AdaptMESSG.UpdateCommand = OleDBUpdateCmd("TBL_MISCH_MESSG", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      AdaptMESSG.DeleteCommand = OleDBDeleteCmd("TBL_MISCH_MESSG", WhereKeyID, Cncol)
      '
      '
      '
      '
      'TBL_MISCH
      '
      '
      '
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptMSY.InsertCommand = OleDBInsertCmd("TBL_MISCH", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "MISCH_ID"
      AdaptMSY.UpdateCommand = OleDBUpdateCmd("TBL_MISCH", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptMSY.DeleteCommand = OleDBDeleteCmd("TBL_MISCH", WhereKeyID, Cncol)
      '
      '
      '
      '
      'Delete/Update/Insert TBL_MISCH_MESSG und TBL_MISCH
      '
      'Delete TBL_MISCH_MESSG  
      '
      '      
      ' 
      '
      AdaptMESSG.Update(TblMESSG.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Delete TBL_MISCH
      '
      AdaptMSY.Update(TblMSY.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '

      '
      '
      'Insert TBL_MISCH
      '
      '
      For i = 0 To TblMSY.Select(Nothing, Nothing, DataViewRowState.Added).Count - 1
        TblMSY.Select(Nothing, Nothing, DataViewRowState.Added)(i)("MISCH_SET") = True
      Next i
      AdaptMSY.Update(TblMSY.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Insert TBL_MISCH_MESSG
      '
      '
      AdaptMESSG.Update(TblMESSG.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      'Update TBL_MISCH
      '
      '
      AdaptMSY.Update(TblMSY.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_MISCH_MESSG
      '
      '
      AdaptMESSG.Update(TblMESSG.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '

      '
      For i = 0 To MischAdd.Count - 1
        MischID = MischAdd(i)
        '
        '
        '
        'Insert TBL_MISCH_GROUP 
        '
        '
        For j = 0 To 3
          SqlStmt = "INSERT INTO TBL_MISCH_GROUP (MISCH_ID,GROUP_ID,GROUP_KBEZ,GROUP_NAME,MISCH_RWRTGID)" _
          & " VALUES(" & MischID & "," & j & ",'" & Texxt(3015 + j) & "','" & Texxt(3015 + j) & "',0)"
          cmdHILF.CommandText = SqlStmt
          If SQLExeNonQuery(cmdHILF, Cncol) <> 0 Then
            Exit Sub
          End If
        Next j
        '
        '
      Next i
    End If
    ViewHilf.Dispose()
    Me.Close()
    Me.Dispose()
  End Sub



  




  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
  End Sub




  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub
  '
  '
 
  WriteOnly Property IopenArt() As Boolean
    Set(ByVal value As Boolean)
      MnIopenArt = value
    End Set
  End Property
  WriteOnly Property UserUfo() As Boolean
    Set(ByVal value As Boolean)
      MnUserUfo = value
    End Set
  End Property

  
End Class


