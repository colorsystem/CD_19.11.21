Option Compare Text
Option Explicit On
Option Strict Off

Module modSettings


  '
  '
  'Administrator
  

  'Manager
  '
  '
  Friend GewWert() As Single = {0.0, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1.0, 2.5, 5.0, 7.5, 10.0, 25.0, 50.0, 75.0, 100.0}
  '
  '
  '
  '
  'MDIForm
  '


  '
  'Farben aus .INI-File
  '
  '
  '
  '
  '
  Function FoName(ByVal i As Integer) As String
    Select Case i

      Case 0
        FoName = "MS SANS SERIF"
        FoName = PrivSettings("FONTS", "FONTGENL", FoName, COLORFileName())
      Case 1
        '
        'Symbole
        '
        FoName = "SYMBOL"
        FoName = GetPrivSettings("FONTS", "FONTSYMB", FoName, COLORFileName())
      Case 2
        '
        'Proportionalschrift
        '
        FoName = "ARIAL"
        FoName = GetPrivSettings("FONTS", "FONTPROP", FoName, COLORFileName())
      Case 3
        '
        'Nicht-Proportionalschrift
        '
        '
        FoName = "COURIER NEW"
        FoName = GetPrivSettings("FONTS", "FONTNONP", FoName, COLORFileName())
    End Select
    Try
      For Each ff In System.Drawing.FontFamily.Families
        If ff.Name = FoName Then
          Exit Function
        End If
      Next
    Catch
    End Try
  End Function
  '
  Function GetDirectory(ByVal StrFile As String) As String
    GetDirectory = StrFile
    Dim Laeng As Integer
    Do
      Laeng = Len(GetDirectory)
      If Laeng = 1 Then
        GetDirectory = ""
        Exit Function
      End If
      If Mid(GetDirectory, Laeng, 1) = "\" Then
        GetDirectory = Mid(GetDirectory, 1, Laeng - 1)
        Exit Do
      Else
        GetDirectory = Mid(GetDirectory, 1, Laeng - 1)
      End If
    Loop
  End Function
  '
  Function AutFormat(ByVal wert As Single) As String
    Dim Forr As String
    Dim i As Integer
    Dim ijv As Integer
    Dim ijn As Integer
    ijv = 3
    ijn = 3
    If wert < 0.001 Then
      AutFormat = Format(wert, "0.000")
    ElseIf wert > 1000 Then
      AutFormat = Format(wert, "########.")
    Else
      i = CInt(log10(wert))
      ijv = 1
      ijn = 5
      ijn = ijn - i
      ijv = ijv + i
      Forr = Sline(ijv, "#") & "." & Sline(ijn, "0")
      AutFormat = Format(wert, Forr)
    End If
  End Function
  '
  '
  
  '
  '
  Function AddDelP(ByVal nform As Integer) As Boolean
    Dim imsg As Integer
    imsg = MsgBox(Texxt(nform), 4, Texxt(2000))
    If imsg = 7 Then
      AddDelP = False
    Else
      AddDelP = True
    End If
  End Function


  Function Meld(ByVal Index As Integer) As Integer
    Select Case Index

      '
      '
      '
      'Update
      '
      Case 0
        Meld = 3000
        '
        '
        'Hinzufügen
        '
      Case 1
        Meld = 2999
        '
        '
        'Delete
        '
      Case 2
        '
        Meld = 2998
    End Select
    '
    '
    ''

  End Function


 


  Function MerkNr(ByVal CCC As String) As Integer
    Dim ID As Integer
    ID = Asc(Mid(CCC, 1, 1))
    If ID >= 64 And ID <= 90 Then
      '  Buchstaben(groß)
      MerkNr = ID - 64
    ElseIf ID >= 48 And ID <= 57 Then
      '  Ziffern
      MerkNr = ID - 21
    Else
      'Sonderzeichen
      MerkNr = 64 + ID
    End If
  End Function
  
  Sub AddNewViewItem(ByVal ID As Integer, ByVal WhereKEY As String, ByVal View As DataView)
    Dim i As Integer
    Dim j As Integer
    Dim Count As Integer
    Dim RowView As DataRowView
    Count = View.Count
    For i = 0 To Count - 1
      RowView = View.AddNew()
      For j = 0 To View.Table.Columns.Count - 1
        RowView(j) = View(i)(j)
      Next j
      RowView(WhereKEY) = ID
      RowView.EndEdit()
    Next i
    ''
  End Sub
  Sub GruppeRef(ByVal Messg_id As Integer, ByVal ier As Integer)
    Dim i As Integer
    Dim SqlStmt As String
    Dim SqlEtmt As String
    Dim CmdSet As New OleDbCommand("", Cncol)
    Dim Dyset As OleDbDataReader
    SqlStmt = "SELECT * FROM TBL_MESSG_GROUP WHERE MESSG_ID=" & Messg_id
    CmdSet.CommandText = SqlStmt
    Dyset = DataReader(CmdSet, CommandBehavior.Default & CommandBehavior.CloseConnection)
    If Not Dyset.Read Then
      For i = 0 To 3
        SqlEtmt = "INSERT INTO TBL_MESSG_GROUP (MESSG_ID,GROUP_ID,GROUP_KBEZ,GROUP_NAME) VALUES(" & Messg_id & "," & i & "," & "'" & Texxt(3015 + i) & "',' ')"
        CmdSet.CommandText = SqlEtmt
        If SQLExeNonQuery(CmdSet) <> 0 Then
          Exit Sub
        End If
      Next i
    End If
    Dyset.Close()
  End Sub
  Sub GruppeRez(ByVal MischID As Integer, ByVal ier As Integer)
    Dim i As Integer
    Dim SqlStmt As String
    Dim SqlEtmt As String
    Dim CmdSet As New OleDbCommand("", Cncol)
    Dim Dyset As OleDbDataReader
    SqlStmt = "SELECT * FROM TBL_MISCH_GROUP WHERE MISCH_ID=" & MischID
    CmdSet.CommandText = SqlStmt
    Dyset = DataReader(CmdSet, CommandBehavior.Default & CommandBehavior.CloseConnection)
    If Not Dyset.Read Then
      For i = 0 To 3
        SqlEtmt = "INSERT INTO TBL_MISCH_GROUP (MISCH_ID,GROUP_ID,GROUP_KBEZ,GROUP_NAME) VALUES(" & MischID & "," & i & "," & "'" & Texxt(3015 + i) & "',' ')"
        CmdSet.CommandText = SqlEtmt
        If SQLExeNonQuery(CmdSet) <> 0 Then
          Exit Sub
        End If
      Next i
    End If
    Dyset.Close()
  End Sub
  Sub CalcGKExtra(ByRef flgGKExtra As DataGridView, ByRef TBLEXT As DataTable, ByRef VIEWEXT As DataView)
    Dim i As Integer
    Dim coun As Integer
    Dim Sqlstmt As String
    Dim CHRMBox As New DataGridViewComboBoxColumn
    Dim AdaptEXT As New OleDbDataAdapter
    Dim CmdEXT As New OleDbCommand("", Cncol)
    Dim TblIHR As New DataTable
    Dim AdaptIHR As New OleDbDataAdapter
    Dim CmdIHR As New OleDbCommand
    CmdIHR = New OleDbCommand("", Cncol)
    AdaptIHR.SelectCommand = CmdIHR
    AdaptEXT.SelectCommand = CmdEXT
    TBLEXT = New DataTable
    flgGKExtra.Rows.Clear()
    '
    '
    'TBL_GKWRTEXT einlesen
    '
    '
    Sqlstmt = "SELECT * FROM TBL_GKWRTEXT ORDER BY GKWRT_ID,IHRM_ID "
    CmdEXT.CommandText = Sqlstmt
    TBLEXT.Rows.Clear()
    TBLEXT.Columns.Clear()
    If Not FillDatset(AdaptEXT, TBLEXT) Then
      Exit Sub
    End If
    TBLEXT.Columns("GKWRT_ID").DefaultValue = 0
    TBLEXT.Columns("IHRM_ID").DefaultValue = 0
    For i = 1 To 10
      If Not TBLEXT.Columns.Contains("GK" & Format(i, "00")) Then
        TBLEXT.Columns.Add("GK" & Format(i, "00"))
      End If
    Next
    For i = 2 To TBLEXT.Columns.Count - 1
      TBLEXT.Columns(i).DefaultValue = 0
    Next

    TBLEXT.Columns("GK04").DefaultValue = 0.0
    TBLEXT.Columns("GK05").DefaultValue = 1.0
    TBLEXT.Columns("GK06").DefaultValue = 0.5
    TBLEXT.Columns("GK07").DefaultValue = 0.5
    TBLEXT.Columns("GK10").DefaultValue = 1.0
    TBLEXT.AcceptChanges()
    '
    '
    'TBL_IHRM einlesen
    '
    '
    Sqlstmt = "SELECT * FROM TBL_IHRM ORDER BY IHRM_ID "
    CmdIHR.CommandText = Sqlstmt
    TblIHR.Rows.Clear()
    TblIHR.Columns.Clear()

    If Not FillDatset(AdaptIHR, TblIHR) Then
      Exit Sub
    End If
    TblIHR.Columns.Add("CHRM", GetType(String))
    For i = 0 To TblIHR.Rows.Count - 1
      TblIHR.Rows(i)("CHRM") = Chrum(TblIHR.Rows(i)("IHRM_IHRM"))
    Next
    TblIHR.AcceptChanges()
    '
    '
    '
    '
    CHRMBox.DataSource = TblIHR
    CHRMBox.ValueMember = "IHRM_ID"
    CHRMBox.DisplayMember = "CHRM"
    CHRMBox.DataPropertyName = "IHRM_ID"
    CHRMBox.Name = "IHRM_ID"
    CHRMBox.HeaderText = "CHRM"
    CHRMBox.DisplayStyle = DataGridViewComboBoxDisplayStyle.ComboBox
    flgGKExtra.Columns.Add("GKWRT_ID", "GKID")
    flgGKExtra.Columns.Add(CHRMBox)
    flgGKExtra.Columns("GKWRT_ID").DataPropertyName = "GKWRT_ID"
    VIEWEXT = New DataView(TBLEXT)
    flgGKExtra.DataSource = VIEWEXT
    flgGKExtra.RowHeadersVisible = True
    flgGKExtra.RowHeadersWidth = 50
    For i = 0 To flgGKExtra.Columns.Count - 1
      flgGKExtra.Columns(i).Width = 50
      flgGKExtra.Columns(i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      flgGKExtra.Columns(i).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
    Next

    flgGKExtra.Columns("GKWRT_ID").Width = 0
    flgGKExtra.ReadOnly = True
    flgGKExtra.AllowUserToAddRows = False
    flgGKExtra.SelectionMode = DataGridViewSelectionMode.FullRowSelect

  End Sub
  Sub SaveGK(ByRef GKwrtIDSource As Integer, ByRef GKwrtIDTarget As Integer, ByRef TblGKW As DataTable)
    Dim i As Integer
    Dim j As Integer
    If GKwrtIDSource = GKwrtIDTarget Then Exit Sub
    Dim VIEWTAR As New DataView(TblGKW)
    Dim VIEWSRC As New DataView(TblGKW)
    VIEWTAR.RowFilter = "GKWRT_ID=" & GKwrtIDTarget
    VIEWSRC.RowFilter = "GKWRT_ID=" & GKwrtIDSource
    For i = 0 To VIEWTAR.Table.Columns.Count - 1
      If Not (VIEWTAR.Table.Columns(i).ColumnName = "GKWRT_ID") Then
        VIEWTAR(0)(i) = VIEWSRC(0)(i)
      End If
    Next
  End Sub
  Sub SaveGKExtra(ByRef GKwrtIDSource As Integer, ByRef GKwrtIDTarget As Integer, ByRef TBLEXT As DataTable, ByRef VIEWEXT As DataView)
    Dim i As Integer
    Dim j As Integer
    Dim RowEXT As DataRow
    If GKwrtIDSource = GKwrtIDTarget Then Exit Sub
    Dim VIEWHLF As New DataView(TBLEXT)
    VIEWHLF.RowFilter = "GKWRT_ID=" & GKwrtIDTarget
    For i = VIEWHLF.Count - 1 To 0 Step -1
      VIEWHLF.Delete(i)
    Next i
    VIEWHLF.Dispose()
    For i = 0 To VIEWEXT.Count - 1
      RowEXT = TBLEXT.NewRow
      For j = 0 To TBLEXT.Columns.Count - 1
        RowEXT(j) = VIEWEXT(i)(j)
      Next j
      RowEXT(0) = GKwrtIDTarget
      TBLEXT.Rows.Add(RowEXT)
    Next i

  End Sub
  
End Module
