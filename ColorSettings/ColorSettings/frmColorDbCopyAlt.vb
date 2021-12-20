Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmColorDbCopy
  Dim DialogVoidDB As OpenFileDialog
  Dim DBVoidName As String
  Dim DatNewName As String
  Dim TabMessgGroup As DataTable
  Dim TabMischGroupMessg As DataTable
  Dim AdaptMessgMisch As OleDbDataAdapter
  Dim AdaptData As OleDbDataAdapter
  Dim SelCommand As OleDbCommand
  Dim InsCommand As OleDbCommand
  Dim DaSet As DataSet
  Dim TabDBName() As String = {"TBL_FARBM", "TBL_FARBM_PREIS", "TBL_FARBM_PROZ", "TBL_FARBM_PROB", "TBL_GRUND_FARBM", "TBL_RWERT", "TBL_QUALI", _
                           "TBL_SORTI", "TBL_SORTI_FARBM", "TBL_SORTI_RWERT", "TBL_REZEPT", "TBL_REZEPT_FARBM", "TBL_REZEPT_RWERT"}
  Dim TblName() As String = {"TblFarbm", "TblFarbmPreis", "TblFarbmProz", "TblFarbmProb", "TblGrundFarbm", "TblRwert", "TblQuali", _
                             "TblSorti", "TblSortiFarbm", "TblSortiRwert", "TblRezept", "TblRezeptFarbm", "TblRezeptRwert"}
  Private Sub frmColorDbCopy_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer

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
    AdaptData = New OleDbDataAdapter
    AdaptData.SelectCommand = New OleDbCommand("", Cndat)
    DaSet = New DataSet
    '
    '
    '
    '
    '
   
    '
    'Messgeräte-Grupen einlesen
    '
    '
    AdaptMessgMisch.SelectCommand.CommandText = "SELECT MESSGRW_ID,TBL_MESSG.MESSG_ID AS MESSG_ID,TBL_MESSG.MESSG_KBEZ AS MESSG_KBEZ,GROUP_ID,GROUP_KBEZ FROM TBL_MESSG " _
      & " INNER JOIN TBL_MESSG_GROUP ON TBL_MESSG.MESSG_ID=TBL_MESSG_GROUP.MESSG_ID WHERE TBL_MESSG.MESSG_ID=MESSGRW_ID"
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
    AdaptMessgMisch.SelectCommand.CommandText = "SELECT TBL_MISCH.MISCH_ID AS MISCH_ID,MISCH_KBEZ,GROUP_ID,GROUP_KBEZ,TBL_MESSG.MESSG_ID AS MESSG_ID,MESSG_KBEZ FROM " _
       & " ((TBL_MISCH INNER JOIN TBL_MISCH_GROUP ON TBL_MISCH.MISCH_ID=TBL_MISCH_GROUP.MISCH_ID) " _
       & " INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID=TBL_MISCH_MESSG.MISCH_ID) " _
       & " INNER JOIN TBL_MESSG ON TBL_MISCH_MESSG.MESSG_ID=TBL_MESSG.MESSG_ID WHERE TBL_MESSG.MESSG_ID=TBL_MESSG.MESSGRW_ID"
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
    dbgMisch.Columns("MESSG_ID").Visible = False
    '
    '
    '
    'Dataset-Tabellen aufbauen
    '
    '
    DaSet = New DataSet

    AdaptData = New OleDbDataAdapter
    SelCommand = New OleDbCommand("", Cndat)
    InsCommand = New OleDbCommand("", Cndat)
    '
    AdaptData.SelectCommand = SelCommand
    For i = 0 To TblName.Count - 1
      DaSet.Tables.Add(TblName(i))
      SelCommand.CommandText = "SELECT * FROM " & TabDBName(i)
      If FillDatSchema(AdaptData, DaSet.Tables(i)) Then

      End If
    Next
    '
    '
    'Dataset-Relationen aufbauen
    '
    '
    Call CreateDatsetRelat("RelFarbmPreis", DaSet, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TblFarbmPreis", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelFarbmProz", DaSet, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TblFarbmProz", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelFarbmProb", DaSet, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TblFarbmProb", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelGrundFarbm", DaSet, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TblGrundFarbm", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelSortiFarbm", DaSet, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TblSortiFarbm", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelMischSorti", DaSet, "TblSorti", {"MISCH_ID", "SORTI_ID"}, "TblSortiFarbm", {"MISCH_ID", "SORTI_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelRezeptFarbm", DaSet, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TblRezeptFarbm", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelMischRezept", DaSet, "TblRezept", {"MISCH_ID", "REZEPT_ID"}, "TblRezeptFarbm", {"MISCH_ID", "REZEPT_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelRwertQuali", DaSet, "TblRwert", {"MESSGRW_ID", "RWERT_ID"}, "TblQuali", {"MESSGRW_ID", "QUALI_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelRwertSorti", DaSet, "TblRwert", {"MESSGRW_ID", "RWERT_ID"}, "TblSortiRwert", {"MESSGRW_ID", "RWERT_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelRwertRezept", DaSet, "TblRwert", {"MESSGRW_ID", "RWERT_ID"}, "TblRezeptRwert", {"MESSGRW_ID", "RWERT_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelSortiSorti", DaSet, "TblSorti", {"MISCH_ID", "SORTI_ID"}, "TblSortiRwert", {"MISCH_ID", "SORTI_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelRezeptRezept", DaSet, "TblRezept", {"MISCH_ID", "REZEPT_ID"}, "TblRezeptRwert", {"MISCH_ID", "REZEPT_ID"}, Rule.Cascade, Rule.Cascade)


  End Sub

  Sub CreateDatsetRelat(ByRef RelName As String, ByRef Datset As DataSet, ParentTabName As String, ParentFields() As String, ChildTabName As String, ChildFields() As String, DeleteRule As Rule, Updaterule As Rule)
    Dim i As Integer
    Dim ParentCount As Integer
    Dim ChildCount As Integer
    Dim ParentColumns() As DataColumn
    Dim ChildColumns() As DataColumn
    '
    'ParentFields
    '
    Erase ParentColumns
    ParentCount = ParentFields.Length
    ReDim ParentColumns(ParentCount - 1)
    For i = 0 To ParentCount - 1
      ParentColumns(i) = Datset.Tables(ParentTabName).Columns(ParentFields(i))
    Next
    '
    'ChildFields
    '
    Erase ChildColumns
    ChildCount = ChildFields.Length
    ReDim ChildColumns(ChildCount - 1)
    For i = 0 To ChildCount - 1
      ChildColumns(i) = Datset.Tables(ChildTabName).Columns(ChildFields(i))
    Next
    '
    'Add Relation
    '
    Datset.Relations.Add(New DataRelation(RelName, ParentColumns, ChildColumns))
    Datset.Relations(RelName).ChildKeyConstraint.DeleteRule = DeleteRule
    Datset.Relations(RelName).ChildKeyConstraint.UpdateRule = Updaterule

  End Sub
  Private Sub btnCopyVoid_Click(sender As Object, e As System.EventArgs) Handles btnCopyVoid.Click
    Dim i As Integer
    '
    '
    '
    For i = 0 To TabDBName.Count - 1
      AdaptData.InsertCommand = OleDBInsertCmd(TabDBName(i), Cndat)
      AdaptData.(DaSet.Tables(TblName(i)).Select(Nothing, Nothing, DataViewRowState.Unchanged))
      DaSet.Tables(TblName(i)).AcceptChanges()
    Next i
    '
    '
    btnFetchMisch.Enabled = False
    btnFetchMessg.Enabled = False
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
    Dim StrMessgRwert As String
    Dim DatRow As DataGridViewRow
    Dim ViewMessg As DataView
    Cursor = Cursors.WaitCursor
    ViewMessg = New DataView(DaSet.Tables("TblRwert"))
    If ConnOpen(Cndat) Then
      If Not IsNothing(TabMessgGroup) AndAlso dbgMessg.SelectedRows.Count > 0 Then
        '
        '
        'Übertrage R-Werte ins Dataset
        '
        '
        For Each DatRow In dbgMessg.SelectedRows
          MessgrwID = DatRow.Cells("MESSGRW_ID").Value
          GroupID = DatRow.Cells("GROUP_ID").Value
          ViewMessg.RowFilter = "MESSGRW_ID=" & MessgrwID
          StrMessgRwert = StrLin(ViewMessg, "RWERT_ID")
          SelCommand.CommandText = "SELECT * FROM TBL_RWERT WHERE MESSGRW_ID= " & MessgrwID & " AND RWERT_GID=" & GroupID & " AND RWERT_ID NOT IN " & StrMessgRwert
          If FillDatset(AdaptData, DaSet.Tables("TblRwert")) Then

          End If
          SelCommand.CommandText = "SELECT * FROM TBL_QUALI WHERE MESSGRW_ID= " & MessgrwID & " AND QUALI_ID NOT IN " & StrMessgRwert
          If FillDatset(AdaptData, DaSet.Tables("TblQuali")) Then

          End If

          dbgMessg.Rows.RemoveAt(DatRow.Index)
        Next
      End If
      Cndat.Close()
    End If
    Cursor = Cursors.Default
  End Sub

  Private Sub btnFetchMisch_Click(sender As Object, e As System.EventArgs) Handles btnFetchMisch.Click
    Dim MischID As Integer
    Dim MessgrwID As Integer
    Dim GroupID As Integer
    Dim StrMessgRwert As String
    Dim StrMischFarbm As String
    Dim DatRow As DataGridViewRow
    Dim ViewMessg As DataView
    Dim ViewMisch As DataView
    btnFetchMessg.Enabled = False
    dbgMessg.Enabled = False
    Cursor = Cursors.WaitCursor
    ViewMessg = New DataView(DaSet.Tables("TblRwert"))
    ViewMisch = New DataView(DaSet.Tables("TblFarbm"))
    If ConnOpen(Cndat) Then
      If Not IsNothing(TabMischGroupMessg) AndAlso dbgMisch.SelectedRows.Count > 0 Then
        '
        '
        'Übertrage Farbmittel usw. ins Dataset
        '
        '
        For Each DatRow In dbgMisch.SelectedRows
          MischID = DatRow.Cells("MISCH_ID").Value
          MessgrwID = DatRow.Cells("MESSGRW_ID").Value
          GroupID = DatRow.Cells("GROUP_ID").Value
          ViewMisch.RowFilter = "MISCH_ID=" & MischID
          StrMischFarbm = StrLin(ViewMessg, "FARBM_ID")
          SelCommand.CommandText = "SELECT * FROM TBL_FARBM WHERE MISCH_ID= " & MischID & " AND RWERT_GID=" & GroupID & " AND RWERT_ID NOT IN " & StrMessgRwert
          If FillDatset(AdaptData, DaSet.Tables("TblRwert")) Then

          End If
          SelCommand.CommandText = "SELECT * FROM TBL_QUALI WHERE MESSGRW_ID= " & MessgrwID & " AND QUALI_ID NOT IN " & StrMessgRwert
          If FillDatset(AdaptData, DaSet.Tables("TblQuali")) Then

          End If

          dbgMessg.Rows.RemoveAt(DatRow.Index)
        Next
      End If
      Cndat.Close()
    End If
    Cursor = Cursors.Default

  End Sub


End Class