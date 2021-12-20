Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmColorDbCopy
  Dim MischExist As List(Of Integer)
  Dim MischExistGroup As List(Of List(Of Integer))
  Dim MischExistMessg As List(Of List(Of Integer))
  Dim MessgExist As List(Of Integer)
  Dim MessgExistGroup As List(Of List(Of Integer))
  Dim DialogVoidDB As OpenFileDialog
  Dim DBVoidName As String
  Dim DatNewName As String
  Dim TabMessgGroup As DataTable
  Dim TabMischGroupMessg As DataTable
  Dim AdaptMessgMisch As OleDbDataAdapter
  Dim SelCommand As OleDbCommand
  Dim ConnNew As OleDbConnection
  Private Sub frmColorDbCopy_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Me.Text = Texxt(2020)
    lblBIS.Text = Texxt(377)
    lblVON.Text = Texxt(376)
    lblMessg.Text = Texxt(403)
    lblMisch.Text = Texxt(406)
    btnCopyVoid.Text = Texxt(2025)
    btnMARKMessg.Text = Texxt(3670)
    btnMARKMisch.Text = Texxt(3670)
    btnVoid.Text = Texxt(144)
    btnFetchMessg.Text = Texxt(2026)
    btnFetchMisch.Text = Texxt(2027)
    chkDAT.Text = Texxt(2030)

    txtBIS.Text = Format$(DateAdd("d", -1 * 365, Today))
    TabMessgGroup = New DataTable
    TabMischGroupMessg = New DataTable
    AdaptMessgMisch = New OleDbDataAdapter
    AdaptMessgMisch.SelectCommand = New OleDbCommand("", Cncol)

    '
    '
    '
    '
    '

    '
    'Messgeräte-Gruppen einlesen
    '
    '
    AdaptMessgMisch.SelectCommand.CommandText = "SELECT MESSGRW_ID,TBL_MESSG.MESSG_ID AS MESSG_ID,TBL_MESSG.MESSG_KBEZ AS MESSG_KBEZ,GROUP_ID,GROUP_KBEZ FROM TBL_MESSG " _
      & " INNER JOIN TBL_MESSG_GROUP ON TBL_MESSG.MESSG_ID=TBL_MESSG_GROUP.MESSG_ID WHERE TBL_MESSG.MESSG_ID=MESSGRW_ID ORDER BY MESSG_KBEZ"
    If Not FillDatset(AdaptMessgMisch, TabMessgGroup) Then
      Exit Sub
    End If
    '
    '
    dbgMessg.DataSource = TabMessgGroup
    dbgMessg.ReadOnly = True
    dbgMessg.AllowUserToAddRows = False
    dbgMessg.AllowUserToDeleteRows = False
    dbgMessg.AllowUserToOrderColumns = False
    dbgMessg.SelectionMode = DataGridViewSelectionMode.RowHeaderSelect
    dbgMessg.Columns("MESSGRW_ID").Visible = False
    dbgMessg.Columns("MESSG_ID").Visible = False
    dbgMessg.Columns("GROUP_ID").Visible = False
    '
    '
    'Mischsysteme-Gruppen einlesen
    '
    AdaptMessgMisch.SelectCommand.CommandText = "SELECT TBL_MISCH.MISCH_ID AS MISCH_ID,MISCH_KBEZ,GROUP_ID,GROUP_KBEZ,TBL_MESSG.MESSGRW_ID AS MESSGRW_ID,MESSG_KBEZ FROM " _
       & " ((TBL_MISCH INNER JOIN TBL_MISCH_GROUP ON TBL_MISCH.MISCH_ID=TBL_MISCH_GROUP.MISCH_ID) " _
       & " INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID=TBL_MISCH_MESSG.MISCH_ID) " _
       & " INNER JOIN TBL_MESSG ON TBL_MISCH_MESSG.MESSG_ID=TBL_MESSG.MESSG_ID WHERE TBL_MESSG.MESSG_ID=TBL_MESSG.MESSGRW_ID ORDER BY MISCH_KBEZ"
    If Not FillDatset(AdaptMessgMisch, TabMischGroupMessg) Then
      Exit Sub
    End If
    dbgMisch.DataSource = TabMischGroupMessg
    '
    dbgMisch.ReadOnly = True
    dbgMisch.AllowUserToAddRows = False
    dbgMisch.AllowUserToDeleteRows = False
    dbgMisch.AllowUserToOrderColumns = False
    dbgMisch.SelectionMode = DataGridViewSelectionMode.RowHeaderSelect
    dbgMisch.Columns("MISCH_ID").Visible = False
    dbgMisch.Columns("GROUP_ID").Visible = False
    dbgMisch.Columns("MESSGRW_ID").Visible = False
    '
    '
    MischExist = New List(Of Integer)
    MischExistGroup = New List(Of List(Of Integer))
    MischExistMessg = New List(Of List(Of Integer))
    MessgExist = New List(Of Integer)
    MessgExistGroup = New List(Of List(Of Integer))
    '
    '
    'Dataset-Relationen aufbauen
    '
    '
    SelCommand = New OleDbCommand("", Cndat)
  End Sub

 
  Private Sub btnCopyVoid_Click(sender As Object, e As System.EventArgs) Handles btnCopyVoid.Click
    Dim i As Integer
    Dim Sqlstmt As String
    Dim MessgrwID As Integer
    Dim GroupID As Integer
    Dim MischID As Integer
    Dim StrSorti As String
    Dim StrRezept As String
    Dim StrRwert As String
    btnFetchMessg.Enabled = False
    dbgMessg.Enabled = False
    btnFetchMisch.Enabled = False
    dbgMisch.Enabled = False
    Cursor = Cursors.WaitCursor

    '
    '
    'Messgeräte
    '
    '
    '
    If ConnOpen(Cndat) Then
      '
      '
      'Übertrage R-Werte in neue Datenbank
      '
      '
      For i = 0 To MessgExist.Count - 1
        MessgrwID = MessgExist(i)
        For k = 0 To MessgExistGroup(i).Count - 1
          GroupID = MessgExistGroup(i)(k)
          '
          'TBL_RWERT
          '
          Sqlstmt = "INSERT INTO TBL_RWERT IN '" & DatNewName & "' "
          Sqlstmt = Sqlstmt & "SELECT * FROM TBL_RWERT WHERE MESSGRW_ID= " & MessgrwID & " AND RWERT_GID=" & GroupID _
                    & " AND (RWERT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON.Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtBIS.Text))) & ")"
          SelCommand.CommandText = Sqlstmt
          If SQLNonQuery(SelCommand, Cndat) <> 0 Then
            Exit Sub
          End If

        Next k
        '
        'TBL_QUALI
        '
        StrRwert = "(SELECT RWERT_ID FROM TBL_RWERT IN '" & ConnNew.DataSource & "' WHERE MESSGRW_ID=" & MessgrwID & ")"
        Sqlstmt = "INSERT INTO TBL_QUALI IN '" & DatNewName & "' "
        Sqlstmt = Sqlstmt & "SELECT * FROM TBL_QUALI WHERE MESSGRW_ID=" & MessgrwID & " AND QUALI_ID IN " & StrRwert
        SelCommand.CommandText = Sqlstmt
        If SQLNonQuery(SelCommand, Cndat) <> 0 Then
          Exit Sub
        End If
      Next
      '
      '
      '
      'Mischsysteme
      '
      '
      '
      '
      ' 
      '
      '
      For i = 0 To MischExist.Count - 1
        MischID = MischExist(i)

        '
        'TBL_FARBM
        '
        Sqlstmt = "INSERT INTO TBL_FARBM IN '" & DatNewName & "' "
        Sqlstmt = Sqlstmt & "SELECT * FROM TBL_FARBM WHERE MISCH_ID= " & MischID
        SelCommand.CommandText = Sqlstmt
        If SQLNonQuery(SelCommand, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        'TBL_FARBM_PREIS
        '
        '
        Sqlstmt = "INSERT INTO TBL_FARBM_PREIS IN '" & DatNewName & "' "
        Sqlstmt = Sqlstmt & "SELECT * FROM TBL_FARBM_PREIS WHERE MISCH_ID= " & MischID
        SelCommand.CommandText = Sqlstmt
        If SQLNonQuery(SelCommand, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        'TBL_FARBM_PROZ
        '
        '
        Sqlstmt = "INSERT INTO TBL_FARBM_PROZ IN '" & DatNewName & "' "
        Sqlstmt = Sqlstmt & "SELECT * FROM TBL_FARBM_PROZ WHERE MISCH_ID= " & MischID
        SelCommand.CommandText = Sqlstmt
        If SQLNonQuery(SelCommand, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        'TBL_FARBM_PROB
        '
        '
        Sqlstmt = "INSERT INTO TBL_FARBM_PROB IN '" & DatNewName & "' "
        Sqlstmt = Sqlstmt & "SELECT * FROM TBL_FARBM_PROB WHERE MISCH_ID= " & MischID
        SelCommand.CommandText = Sqlstmt
        If SQLNonQuery(SelCommand, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        'TBL_SORTI
        '
        '
        Sqlstmt = "INSERT INTO TBL_SORTI IN '" & DatNewName & "' "
        Sqlstmt = Sqlstmt & "SELECT * FROM TBL_SORTI WHERE MISCH_ID= " & MischID
        SelCommand.CommandText = Sqlstmt
        If SQLNonQuery(SelCommand, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        'TBL_SORTI_FARBM
        '
        '
        StrSorti = "(SELECT SORTI_ID FROM TBL_SORTI IN '" & ConnNew.DataSource & "' WHERE MISCH_ID=" & MischID & ")"
        Sqlstmt = "INSERT INTO TBL_SORTI_FARBM IN '" & DatNewName & "' "
        Sqlstmt = Sqlstmt & "SELECT * FROM TBL_SORTI_FARBM WHERE MISCH_ID= " & MischID & " AND SORTI_ID IN " & StrSorti
        SelCommand.CommandText = Sqlstmt
        If SQLNonQuery(SelCommand, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        For k = 0 To MischExistGroup(i).Count - 1
          GroupID = MischExistGroup(i)(k)
          '
          '

          'TBL_REZEPT
          '
            '
            Sqlstmt = "INSERT INTO TBL_REZEPT IN '" & DatNewName & "' "
            Sqlstmt = Sqlstmt & "SELECT * FROM TBL_REZEPT WHERE MISCH_ID= " & MischID & " AND REZEPT_GID=" & GroupID _
                    & " AND (REZEPT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON.Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtBIS.Text))) & ")"
            SelCommand.CommandText = Sqlstmt
            If SQLNonQuery(SelCommand, Cndat) <> 0 Then
              Exit Sub
            End If
        Next k
        '
        '
        'TBL_REZEPT_FARBM
        '
        '
        '
        '
        StrRezept = "(SELECT REZEPT_ID FROM TBL_REZEPT IN '" & ConnNew.DataSource & "' WHERE MISCH_ID=" & MischID & ")"

        Sqlstmt = "INSERT INTO TBL_REZEPT_FARBM IN '" & DatNewName & "' "
        Sqlstmt = Sqlstmt & "SELECT * FROM TBL_REZEPT_FARBM WHERE MISCH_ID= " & MischID & " AND REZEPT_ID IN " & StrRezept
        SelCommand.CommandText = Sqlstmt
        If SQLNonQuery(SelCommand, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        '
        'abhängige R-Werte übernehmen
        '
        '
        '
        '
        For k = 0 To MischExistMessg(i).Count - 1
          MessgrwID = MischExistMessg(i)(k)
          '
          StrRwert = "(SELECT RWERT_ID FROM TBL_RWERT IN '" & ConnNew.DataSource & "' WHERE MESSGRW_ID=" & MessgrwID & ")"
          '
          'TBL_GRUND_FARBM
          '
          '
          Sqlstmt = "INSERT INTO TBL_GRUND_FARBM IN '" & DatNewName & "' "
          Sqlstmt = Sqlstmt & "SELECT * FROM TBL_GRUND_FARBM WHERE MISCH_ID= " & MischID & " AND MESSGRW_ID=" & MessgrwID
          SelCommand.CommandText = Sqlstmt
          If SQLNonQuery(SelCommand, Cndat) <> 0 Then
            Exit Sub
          End If
          '
          '
          '
          'TBL_SORTI_RWERT
          '
          '

          StrSorti = "(SELECT SORTI_ID FROM TBL_SORTI IN '" & ConnNew.DataSource & "' WHERE MISCH_ID=" & MischID & ")"
          Sqlstmt = "INSERT INTO TBL_SORTI_RWERT IN '" & DatNewName & "' "
          Sqlstmt = Sqlstmt & "SELECT * FROM TBL_SORTI_RWERT WHERE MISCH_ID=" & MischID & " AND MESSGRW_ID= " & MessgrwID _
                    & " AND SORTI_ID IN " & StrSorti & " AND RWERT_ID IN " & StrRwert
          SelCommand.CommandText = Sqlstmt
          If SQLNonQuery(SelCommand, Cndat) <> 0 Then
            Exit Sub
          End If

          '
          '
          '
          'TBL_REZEPT_RWERT
          '
          '
          Sqlstmt = "INSERT INTO TBL_REZEPT_RWERT IN '" & DatNewName & "' "
          Sqlstmt = Sqlstmt & "SELECT * FROM TBL_REZEPT_RWERT WHERE MISCH_ID= " & MischID & " AND MESSGRW_ID=" & MessgrwID _
            & " AND REZEPT_ID IN " & StrRezept & " AND RWERT_ID IN " & StrRwert
          SelCommand.CommandText = Sqlstmt
          If SQLNonQuery(SelCommand, Cndat) <> 0 Then
            Exit Sub
          End If
      
          Next
        Next
    End If
    Cndat.Close()
    Cursor = Cursors.Default
    MessageBox.Show(Texxt(2028) & vbCrLf & DatNewName, Texxt(2000), MessageBoxButtons.OK)
  End Sub
  Private Sub btnVoid_Click(sender As System.Object, e As System.EventArgs) Handles btnVoid.Click
    DBVoidName = GetFileProfile("%PROGRAMFILES%/COLORAPPLPROG/COLORDATAVOID.MDB")
    If Not File.Exists(DBVoidName) Then
      MsgBox(DBVoidName & "  nicht vorhanden")
      Exit Sub
    Else
      DatNewName = NewFileName(Cndat.DataSource, "COLORZWIDATA.MDB")
      DatNewName = InputBox("Neue Datenbank", Texxt(2000), DatNewName)
      If DatNewName = "" Then
        Exit Sub
      End If
    End If
    '
    'Datenbank COLORDATAVOID.MDB kopieren
    '
    '
    If File.Exists(DatNewName) Then
      If MsgBox(Texxt(3050) & ": " & DatNewName & " " & Texxt(3051) & vbCrLf & Texxt(3680) & "?", MsgBoxStyle.YesNo, Texxt(2000)) = MsgBoxResult.Yes Then
        File.Delete(DatNewName)
      Else
        Exit Sub
      End If
    End If
    File.Copy(DBVoidName, DatNewName)
    '
    '
    'Connection
    '
    '
    ConnNew = New OleDbConnection
    ConnNew.ConnectionString = NewConnectionstring(Cndat.ConnectionString, DatNewName)
    btnMARKMessg.Enabled = True
    btnMARKMisch.Enabled = True
    btnCopyVoid.Enabled = True
    btnFetchMessg.Enabled = True
    btnFetchMisch.Enabled = True
    '
    '
  End Sub


  Private Sub chkDAT_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkDAT.CheckedChanged
    If chkDAT.Checked Then
      lblVON.Visible = True
      lblBIS.Visible = True
      txtVON.Visible = True
      txtBIS.Visible = True
    Else
      lblVON.Visible = False
      lblBIS.Visible = False
      txtVON.Visible = False
      txtBIS.Visible = False
    End If
  End Sub

  Private Sub btnMARKMessg_Click(sender As Object, e As System.EventArgs) Handles btnMARKMessg.Click
    dbgMessg.SelectAll()
  End Sub
  Private Sub btnMARKmisch_Click(sender As Object, e As System.EventArgs) Handles btnMARKMisch.Click
    dbgMisch.SelectAll()
  End Sub


  Private Sub btnFetchMessg_Click(sender As Object, e As System.EventArgs) Handles btnFetchMessg.Click
    Dim MessgrwID As Integer
    Dim GroupID As Integer
    Dim MessgIndex As Integer
    If Not IsNothing(TabMessgGroup) AndAlso dbgMessg.SelectedRows.Count > 0 Then
      '
      '
      'Sammeln Messgeräte und zugehörige Gruppen  
      '
      '
      For Each DatRow In dbgMessg.SelectedRows
        MessgrwID = DatRow.Cells("MESSGRW_ID").Value
        GroupID = DatRow.Cells("GROUP_ID").Value
        If Not MessgExist.Contains(MessgrwID) Then
          MessgExist.Add(MessgrwID)
          MessgExistGroup.Add(New List(Of Integer))
        End If
        MessgIndex = MessgExist.IndexOf(MessgrwID)
        '
        '
        'Grupppen übernehmen
        '
        '
        If Not MessgExistGroup(MessgIndex).Contains(GroupID) Then
          MessgExistGroup(MessgIndex).Add(GroupID)
        End If
        dbgMessg.Rows.RemoveAt(DatRow.Index)
      Next
    End If
  End Sub


  Private Sub btnFetchMisch_Click(sender As Object, e As System.EventArgs) Handles btnFetchMisch.Click
    Dim MischIndex As Integer
    Dim MischID As Integer
    Dim MessgrwID As Integer
    Dim GroupID As Integer
    Dim DatRow As DataGridViewRow
  

  

    If Not IsNothing(TabMischGroupMessg) AndAlso dbgMisch.SelectedRows.Count > 0 Then
      '
      '
      ' '
      '
      'Sammeln Mischsysteme und Gruppen sowie Messgeräte
      '
      '
      '
      '
      For Each DatRow In dbgMisch.SelectedRows
        MischID = DatRow.Cells("MISCH_ID").Value
        GroupID = DatRow.Cells("GROUP_ID").Value
        MessgrwID = DatRow.Cells("MESSGRW_ID").Value
        If Not MischExist.Contains(MischID) Then
          '
          '
          MischExist.Add(MischID)
          MischExistGroup.Add(New List(Of Integer))
          MischExistMessg.Add(New List(Of Integer))
        End If
        MischIndex = MischExist.IndexOf(MischID)
        '
        '
        'Grupppen übernehmen
        '
        '
        If Not MischExistGroup(MischIndex).Contains(GroupID) Then
          '
          MischExistGroup(MischIndex).Add(GroupID)
        End If
        '
        '
        'R-Werte übernehmen
        '
        '
        '
        If Not MischExistMessg(MischIndex).Contains(MessgrwID) Then
          '
          MischExistMessg(MischIndex).Add(MessgrwID)
        End If
        dbgMisch.Rows.RemoveAt(DatRow.Index)
      Next
    End If
  End Sub


End Class