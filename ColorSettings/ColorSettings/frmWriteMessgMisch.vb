Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmWriteMessgMisch

  Dim ier As Integer
  Dim ListOLDBCom As List(Of OleDbCommand)
  Dim Sqlstmt As String
  Dim WithID As Boolean = False
  Dim WithOrder As Boolean = False
  Dim btnTree As List(Of Button)
  Dim DialogVoidDB As OpenFileDialog
  Dim DBVoidName As String
  Dim DatNewName As String
  Dim StrMessg As String
  Dim StrMisch As String
  Dim ListID As List(Of Integer)
  Dim TabMessgeräte As DataTable
  Dim ViewMessgeräte As DataView
  Dim ViewMischsysteme As DataView
  Dim TabMischsysteme As DataTable
  Dim TblMessgeräte As DataTable
  Dim AdaptGeneral As OleDbDataAdapter
  Dim CommandNonQuery As OleDbCommand
  Private Sub frmWriteMessgMisch_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim Arrow As String = " ==> "
    Dim ipos As Integer
    Dim Measxxx As String
    Dim MessKlasse As String
    Me.Text = Texxt(143)
    lblDBCopy.Text = Texxt(389) & Space(1) & Texxt(141) & Space(1) & Texxt(144)
    btnVoid.Text = Texxt(144)
    btnMessg.Text = Texxt(403)
    btnMisch.Text = Texxt(406)
    btnMessgSelect.Text = Texxt(3670)
    btnMischSelect.Text = Texxt(3670)
    btnRwerte.Text = Texxt(3110) & Space(1) & Texxt(143)
    btnRezepte.Text = Texxt(730) & Space(1) & Texxt(143)
    lblShowMeasDevices.Text = Texxt(403) & Space(1) & Texxt(142)
    btnVoid.Text = Texxt(144)

    '
    '



    DialogVoidDB = New OpenFileDialog
    AdaptGeneral = New OleDbDataAdapter
    AdaptGeneral.SelectCommand = New OleDbCommand("", Cncol)
    ListOLDBCom = New List(Of OleDbCommand)
    btnTree = New List(Of Button)

    '
    '
    '
    'Messgeräte-Treiber
    '
    '
    dbgMeasure00x.Columns.Add("MEASXXX", "Measxxx")
    dbgMeasure00x.Columns.Add("KLASSE", "COM_Klasse")
    dbgMeasure00x.Columns("MEASXXX").Width = 100
    dbgMeasure00x.Columns("KLASSE").Width = 300
    dbgMeasure00x.AllowUserToAddRows = False

    '
    '
    dbgMeasure00x.Rows.Clear()
    For i = 0 To 99
      Measxxx = "MEAS" & Format(i, "000")
      MessKlasse = GetPrivSettings("MEASURE", Measxxx, "", COLORFileName)
      If MessKlasse <> "" Then
        dbgMeasure00x.Rows.Add(New DataGridViewRow)
        ipos = dbgMeasure00x.Rows.Count - 1
        dbgMeasure00x.Rows(ipos).Cells("MEASXXX").Value = Measxxx
        dbgMeasure00x.Rows(ipos).Cells("KLASSE").Value = MessKlasse
      End If
    Next
    txtMessgKE.Text = GetPrivSettings("STARTUP", "MESSGKE", "", COLORFileName)
    '
    '
    '
    TblMessgeräte = New DataTable
    AdaptGeneral.SelectCommand.CommandText = "SELECT MESSG_ID,MESSGRW_ID,MESSG_KENN,MESSG_DRIVER,BEREICH_NAME,MESSG_WANF,MESSG_WEND,MESSG_REFTRA,MESSG_KBEZ FROM TBL_MESSG" _
      & " INNER JOIN TBL_BEREICH ON TBL_BEREICH.BEREICH_ID=TBL_MESSG.MESSG_NORM_FILE_ID"
    If Not FillDatset(AdaptGeneral, TblMessgeräte) Then
      Exit Sub
    End If
    TblMessgeräte.AcceptChanges()
    dbgMessgeräte.DataSource = TblMessgeräte
    dbgMessgeräte.AllowUserToDeleteRows = False
    dbgMessgeräte.AllowUserToAddRows = False
    dbgMessgeräte.ReadOnly = True
    dbgMessgeräte.Columns(dbgMessgeräte.Columns.Count - 1).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
    dbgMessgeräte.RowHeadersVisible = False
  End Sub

  Sub btnBackColor(btnTree As List(Of Button), btn As Button)
    Dim i As Integer
    For i = 0 To btnTree.Count - 1
      btnTree(i).BackColor = Color.LightGray
    Next
    If IsNothing(btn) Then Exit Sub
    btn.BackColor = Color.Azure
  End Sub

  Private Sub btnVoid_Click(sender As System.Object, e As System.EventArgs) Handles btnVoid.Click
    btnMessg.Enabled = False
    btnMisch.Enabled = False
    btnMischSelect.Enabled = False
    btnRwerte.Enabled = False
    btnMessgSelect.Enabled = False
    btnRezepte.Enabled = False
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
    btnMessg.Enabled = True
    '
    '
  End Sub



  Private Sub btnMessg_Click(sender As Object, e As System.EventArgs) Handles btnMessg.Click
    TabMessgeräte = New DataTable
    AdaptGeneral.SelectCommand.CommandText = "SELECT MESSGRW_ID,MESSG_KBEZ FROM TBL_MESSG WHERE MESSG_ID=MESSGRW_ID ORDER BY MESSG_KBEZ"
    If Not FillDatset(AdaptGeneral, TabMessgeräte) Then
      Exit Sub
    End If
    lstMessg.DataSource = TabMessgeräte
    lstMessg.ValueMember = "MESSGRW_ID"
    lstMessg.DisplayMember = "MESSG_KBEZ"
    btnRwerte.Enabled = True
    btnMessgSelect.Enabled = True
  End Sub

  Private Sub btnRwerte_Click(sender As System.Object, e As System.EventArgs) Handles btnRwerte.Click
    Dim i As Integer
    Dim SqlStmt As String
    Cursor = Cursors.WaitCursor
    ListID = New List(Of Integer)
    For i = 0 To lstMessg.SelectedItems.Count - 1
      ListID.Add(lstMessg.SelectedItems(i)("MESSGRW_ID"))
    Next
    StrMessg = StrLin(ListID)
    SqlStmt = "INSERT INTO TBL_RWERT IN '" & DatNewName & "' SELECT * FROM TBL_RWERT WHERE MESSGRW_ID IN " & StrMessg
    CommandNonQuery = New OleDbCommand(SqlStmt, Cndat)
    If SQLNonQuery(CommandNonQuery, Cndat) Then
      Exit Sub
    End If
    SqlStmt = "INSERT INTO TBL_QUALI IN '" & DatNewName & "' SELECT * FROM TBL_QUALI WHERE MESSGRW_ID IN " & StrMessg
    CommandNonQuery = New OleDbCommand(SqlStmt, Cndat)
    If SQLNonQuery(CommandNonQuery, Cndat) Then
      Exit Sub
    End If
    btnMisch.Enabled = True
    Cursor = Cursors.Default
    MsgBox(Texxt(2034))

  End Sub

  Private Sub btnMisch_Click(sender As Object, e As System.EventArgs) Handles btnMisch.Click
    TabMischsysteme = New DataTable
    AdaptGeneral.SelectCommand.CommandText = "SELECT TBL_MISCH.MISCH_ID,MISCH_KBEZ FROM TBL_MISCH ORDER BY MISCH_KBEZ"
    If Not FillDatset(AdaptGeneral, TabMischsysteme) Then
      Exit Sub
    End If
    lstMisch.DataSource = TabMischsysteme
    lstMisch.ValueMember = "MISCH_ID"
    lstMisch.DisplayMember = "MISCH_KBEZ"
    btnMischSelect.Enabled = True
    btnRezepte.Enabled = True
  End Sub

  Private Sub btnRezepte_Click(sender As Object, e As System.EventArgs) Handles btnRezepte.Click
    Dim i As Integer
    Dim TabName() As String = {"TBL_FARBM", "TBL_FARBM_PREIS", "TBL_FARBM_PROZ", "TBL_FARBM_PROB", _
      "TBL_REZEPT", "TBL_REZEPT_FARBM", "TBL_REZEPT_RWERT", "TBL_SORTI", "TBL_SORTI_FARBM", "TBL_SORTI_RWERT", "TBL_GRUND_FARBM"}
    Dim SqlStmt As String
    ListID = New List(Of Integer)
    For i = 0 To lstMisch.SelectedItems.Count - 1
      ListID.Add(lstMisch.SelectedItems(i)("MISCH_ID"))
    Next
    StrMisch = StrLin(ListID)
    Cursor = Cursors.WaitCursor
    For i = 0 To TabName.Count - 1
      If TabName(i) = "TBL_REZEPT_RWERT" Or TabName(i) = "TBL_SORTI_RWERT" Or TabName(i) = "TBL_GRUND_FARBM" Then
        SqlStmt = "INSERT INTO " & TabName(i) & " IN '" & DatNewName & "' SELECT * FROM " & TabName(i) _
          & " WHERE MISCH_ID IN " & StrMisch & " AND MESSGRW_ID IN " & StrMessg
      Else
        SqlStmt = "INSERT INTO " & TabName(i) & " IN '" & DatNewName & "' SELECT * FROM " & TabName(i) _
          & " WHERE MISCH_ID IN " & StrMisch
      End If
      CommandNonQuery = New OleDbCommand(SqlStmt, Cndat)
      If SQLNonQuery(CommandNonQuery, Cndat) Then
        Exit Sub
      End If
    Next
    Cursor = Cursors.Default
    MsgBox(Texxt(2033))
  End Sub


  Private Sub btnMessgSelect_Click(sender As Object, e As System.EventArgs) Handles btnMessgSelect.Click
    Dim i As Integer
    For i = 0 To lstMessg.Items.Count - 1
      lstMessg.SetSelected(i, True)
    Next i
  End Sub

  Private Sub btnMischSelect_Click(sender As Object, e As System.EventArgs) Handles btnMischSelect.Click
    Dim i As Integer
    For i = 0 To lstMisch.Items.Count - 1
      lstMisch.SetSelected(i, True)
    Next i
  End Sub

End Class