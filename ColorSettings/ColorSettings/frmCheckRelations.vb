Public Class frmCheckRelations
  Dim Daset As DataSet
  Dim Adapt As OleDbDataAdapter
  Dim SelCommand As OleDbCommand
  Dim TabDBName() As String = {"TBL_FARBM", "TBL_FARBM_PREIS", "TBL_FARBM_PROZ", "TBL_FARBM_PROB", "TBL_GRUND_FARBM", "TBL_RWERT", "TBL_QUALI", _
                         "TBL_SORTI", "TBL_SORTI_FARBM", "TBL_SORTI_RWERT", "TBL_REZEPT", "TBL_REZEPT_FARBM", "TBL_REZEPT_RWERT"}
  Dim TblDatsetName() As String = {"TblFarbm", "TblFarbmPreis", "TblFarbmProz", "TblFarbmPROB", "TBLGrundFarbm", "TblRwert", "TblQuali", _
                                  "TblSorti", "TblSortiFarbm", "TblSortiRwert", "TblRezept", "TblRezeptFarbm", "TblRezeptRwert"}
  Dim TabMisch As DataTable
  Dim TabMessgrw As DataTable

  Private Sub frmCheckRelations_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Me.Text = Texxt(2008)
    lblDatenbank.Text = Cndat.DataSource
    btnAdjust.Text = Texxt(2009)
    btnIDNumber.Text = Texxt(2040)
    '

    Adapt = New OleDbDataAdapter
    SelCommand = New OleDbCommand("", Cncol)
    Adapt.SelectCommand = SelCommand

    '
    'Messgeräte
    '
    TabMessgrw = New DataTable
    SelCommand.CommandText = "SELECT MESSGRW_ID,MESSG_ID FROM TBL_MESSG WHERE MESSGRW_ID=MESSG_ID"
    If Not FillDatset(Adapt, TabMessgrw) Then
      MsgBox("Error")
    End If
    TabMessgrw.AcceptChanges()
    '
    '
    'Mischsysteme
    '
    TabMisch = New DataTable
    SelCommand.CommandText = "SELECT * FROM TBL_MISCH"
    If Not FillDatset(Adapt, TabMisch) Then
      MsgBox("Error")
    End If
    TabMisch.AcceptChanges()


    Exit Sub
    '
    '
    '
    'Obsolet
    '
    'wird nur verwendet, wenn Id-Nummern über DatSet neu zugeordnet werden
    '
    '
    '
    Daset = New DataSet
    SelCommand.Connection = Cndat
    For i = 0 To TblDatsetName.Count - 1
      SelCommand.CommandText = "SELECT * FROM " & TabDBName(i)
      Daset.Tables.Add(TblDatsetName(i))
      If Not FillDatSchema(Adapt, Daset, TblDatsetName(i)) Then
        MsgBox("Error")
      End If
    Next i
    '
    '
    'Relationen nicht verwendet
    '
    Daset.Relations.Clear()
    Call CreateDatsetRelat("RelFarbPreis", Daset, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TBLFarbmPreis", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelFarbProz", Daset, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TBLFarbmProz", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelFarbProb", Daset, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TBLFarbmProb", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelGrundFarbm", Daset, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TBLGrundFarbm", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelSortiFarbm", Daset, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TBLSortiFarbm", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelRezeptFarbm", Daset, "TblFarbm", {"MISCH_ID", "FARBM_ID"}, "TBLRezeptFarbm", {"MISCH_ID", "FARBM_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelRwertQuali", Daset, "TblRwert", {"MESSGRW_ID", "RWERT_ID"}, "TblQuali", {"MESSGRW_ID", "QUALI_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelSortiRwertMessgrw", Daset, "TblRwert", {"MESSGRW_ID", "RWERT_ID"}, "TBLSortiRwert", {"MESSGRW_ID", "RWERT_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelSortiRwertMisch", Daset, "TblSorti", {"MISCH_ID", "SORTI_ID"}, "TBLSortiRwert", {"MISCH_ID", "SORTI_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelSortiFarbmMisch", Daset, "TblSorti", {"MISCH_ID", "SORTI_ID"}, "TBLSortiFarbm", {"MISCH_ID", "SORTI_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelRezeptRwertMessgrw", Daset, "TblRwert", {"MESSGRW_ID", "RWERT_ID"}, "TBLRezeptRwert", {"MESSGRW_ID", "RWERT_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelRezeptRwertMisch", Daset, "TblRezept", {"MISCH_ID", "REZEPT_ID"}, "TBLRezeptRwert", {"MISCH_ID", "REZEPT_ID"}, Rule.Cascade, Rule.Cascade)
    Call CreateDatsetRelat("RelRezeptFarbmMisch", Daset, "TblRezept", {"MISCH_ID", "REZEPT_ID"}, "TBLRezeptFarbm", {"MISCH_ID", "REZEPT_ID"}, Rule.Cascade, Rule.Cascade)
    '
   
  End Sub
  Private Sub btnAdjust_Click(sender As System.Object, e As System.EventArgs) Handles btnAdjust.Click
    '
    Cursor = Cursors.WaitCursor
    Call TestBeziehungen(Cndat, "TBL_RWERT", {"MESSGRW_ID", "RWERT_ID"}, "TBL_QUALI", {"MESSGRW_ID", "QUALI_ID"}, True)
    Call TestBeziehungen(Cndat, "TBL_FARBM", {"MISCH_ID", "FARBM_ID"}, "TBL_FARBM_PROZ", {"MISCH_ID", "FARBM_ID"}, True)
    Call TestBeziehungen(Cndat, "TBL_FARBM", {"MISCH_ID", "FARBM_ID"}, "TBL_FARBM_PROB", {"MISCH_ID", "FARBM_ID"}, True)
    Call TestBeziehungen(Cndat, "TBL_FARBM", {"MISCH_ID", "FARBM_ID"}, "TBL_FARBM_PREIS", {"MISCH_ID", "FARBM_ID"}, True)
    Call TestBeziehungen(Cndat, "TBL_FARBM", {"MISCH_ID", "FARBM_ID"}, "TBL_GRUND_FARBM", {"MISCH_ID", "FARBM_ID"}, True)
    Call TestBeziehungen(Cndat, "TBL_REZEPT", {"MISCH_ID", "REZEPT_ID"}, "TBL_REZEPT_FARBM", {"MISCH_ID", "REZEPT_ID"}, True)
    Call TestBeziehungen(Cndat, "TBL_RWERT", {"MESSGRW_ID", "RWERT_ID"}, "TBL_REZEPT_RWERT", {"MESSGRW_ID", "RWERT_ID"}, True)
    Call TestBeziehungen(Cndat, "TBL_REZEPT", {"MISCH_ID", "REZEPT_ID"}, "TBL_REZEPT_RWERT", {"MISCH_ID", "REZEPT_ID"}, True)
    Call TestBeziehungen(Cndat, "TBL_SORTI", {"MISCH_ID", "SORTI_ID"}, "TBL_SORTI_FARBM", {"MISCH_ID", "SORTI_ID"}, True)
    Call TestBeziehungen(Cndat, "TBL_RWERT", {"MESSGRW_ID", "RWERT_ID"}, "TBL_SORTI_RWERT", {"MESSGRW_ID", "RWERT_ID"}, True)
    Call TestBeziehungen(Cndat, "TBL_SORTI", {"MISCH_ID", "SORTI_ID"}, "TBL_SORTI_RWERT", {"MISCH_ID", "SORTI_ID"}, True)
    MsgBox(Texxt(2011))
    Cursor = Cursors.Default
  End Sub

 

  Private Sub btnIDNumber_Click(sender As System.Object, e As System.EventArgs) Handles btnIDNumber.Click
    Dim k As Integer
    If MessageBox.Show(Texxt(2042), Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.No Then Exit Sub
    Cursor = Cursors.WaitCursor
    '
    'ID-Nummern für Messgeräte neu zuordnen
    '
    '
    For k = 0 To TabMessgrw.Rows.Count - 1
      Call NewIdNumber(Cndat, "TBL_RWERT", "RWERT_ID", "MESSGRW_ID", TabMessgrw.Rows(k)("MESSGRW_ID"))
    Next
    '
    'ID-Nummern für Mischsysteme neu zuordnen
    '
    '
    For k = 0 To TabMisch.Rows.Count - 1
      '
      '
      '
      Call NewIdNumber(Cndat, "TBL_FARBM", "FARBM_ID", "MISCH_ID", TabMisch.Rows(k)("MISCH_ID"))
      Call NewIdNumber(Cndat, "TBL_SORTI", "SORTI_ID", "MISCH_ID", TabMisch.Rows(k)("MISCH_ID"))
      Call NewIdNumber(Cndat, "TBL_REZEPT", "REZEPT_ID", "MISCH_ID", TabMisch.Rows(k)("MISCH_ID"))
    Next
   
    MessageBox.Show(Texxt(2041), Texxt(2000), MessageBoxButtons.OK)
    Cursor = Cursors.Default
  End Sub
 
  Sub NewIdNumber(cnn As OleDbConnection, TabName As String, IdName As String, WhereName As String, WhereValue As Integer)
    Dim i As Integer
    Dim Datreader As OleDbDataReader
    Dim SelectCommand As OleDbCommand
    Dim UpdateCommand As OleDbCommand
    '
    '
    '
    'neben der FARBM_ID muss in TBL_FARBM auch GLZGRD_ID neu zugeordnet werden
    '
    '
    '
  
    SelectCommand = New OleDbCommand("", cnn)
    UpdateCommand = New OleDbCommand("", cnn)
    SelectCommand.CommandText = "SELECT * FROM " & TabName & " WHERE " & WhereName & "=" & WhereValue & " ORDER BY " & IdName
    Datreader = DataReader(SelectCommand, CommandBehavior.CloseConnection, cnn)
    i = 1
    Do While Datreader.Read
      UpdateCommand.CommandText = "UPDATE " & TabName & " SET " & IdName & "=" & i & " WHERE " & WhereName & "=" & WhereValue & " AND " & IdName & "=" & Datreader(IdName)
      If SQLNonQuery(UpdateCommand, cnn) <> 0 Then
        MsgBox("error")
        Exit Sub
      End If
      '
      If TabName = "TBL_FARBM" Then
        UpdateCommand.CommandText = "UPDATE " & TabName & " SET GLZGRD_ID=" & -i & " WHERE " & WhereName & "=" & WhereValue & " AND GLZGRD_ID=" & Datreader(IdName)
        If SQLNonQuery(UpdateCommand, cnn) <> 0 Then
          MsgBox("error")
          Exit Sub
        End If
      End If
      i = i + 1
    Loop
    Datreader.Close()
    '
    '
    If TabName = "TBL_FARBM" Then
      'GLZGRD_ID neu zuordnen
      '
      '
      '
      'Negative GlzGrdID's in positive verwandeln
      UpdateCommand.CommandText = "UPDATE TBL_FARBM SET GLZGRD_ID=-1*GLZGRD_ID WHERE " & WhereName & "=" & WhereValue & " AND GLZGRD_ID<0"
      If SQLExeNonQuery(UpdateCommand, cnn) <> 0 Then
        Exit Sub
      End If
      '
    End If
    SelectCommand.Dispose()
    UpdateCommand.Dispose()
  End Sub

End Class