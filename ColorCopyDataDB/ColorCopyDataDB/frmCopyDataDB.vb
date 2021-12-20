
Public Class frmCopyDataDB
  Dim OpenFile As OpenFileDialog
  Dim MasterFolder As FolderBrowserDialog
  Dim CnColF As OleDbConnection
  Dim TblCol As String = "TBL_COLORFILE"
  Dim ColINI As String = "COLORFILE.INI"
  Dim LangText() As String = {"German", "English", "Spanish", "Portuguese", "Italian", "French"}
  Dim LangTable() As String = {"TBL_TEXTE_GER", "TBL_TEXTE_ENG", "TBL_TEXTE_ESP", "TBL_TEXTE_POR", "TBL_TEXTE_ITL", "TBL_TEXTE_FRE"}
  Dim TabColorfile As DataTable
  Dim dataSprache As DataGridViewComboBoxColumn
  Dim dataUser As DataGridViewComboBoxColumn
  Dim dataMessg As DataGridViewComboBoxColumn
  Dim dataMisch As DataGridViewComboBoxColumn
  Dim AdaptColorfileini As OleDbDataAdapter
  Dim IniSt As Boolean = False


  Private Sub btnDataRepair_Click(sender As Object, e As System.EventArgs) Handles btnDataRepair.Click
    Call ColorDataRepair()
    Cursor = Cursors.Default
  End Sub
  Sub ColorDataRepair()
    'Das Programm kopiert die Datenbank COLORDATA.MDB in eine Zwischendatei COLORZWIDATA.MDB. Folgende Schritte sind erforderlich
    '1. Name (COLORZWIDATA.MDB) der Zieldatei (DATTARGET) festlegen 
    '2. COLORDATAVOID.MDB wird nach Zieldatei (COLORZWIDATA.MDB) kopiert
    '3. Tabelleninhalt von COLORZWIDATA.MDB wird gelöscht
    '4. Tabelleninhalte von COLORDATA.MDB werden nach COLORZWIDATA.MDB geschrieben
    '
    '
    'Damit sind alle Daten der bisherigen COLORDATA.MDB umgespeichert und die Beziehungen zwischen den Tabellen wieder hergestellt
    'Jetzt kann Colordata.MDB in ColordataAlt.Mdb umbenant werden
    'und COLORZWIDATA.MDB wird in Colordata.MDB umbenannt
    '
    '
    'Möglicherweise gibt es Verletzungen der referenziellen Integrität (d.h. fehlerhafte Daten auf Grund ser Beziehungen
    'Mit Hilfe des Unterprogramms TESTBEZIEHUNGEN können die fehlerhaften Sätze der untergeordneten Tabellen gefunden werden
    '
    '
    'Anmerkung: Die Datei TBL_RWERT entspricht nicht der Struktur von TBL_RWERT in COLORDATAVOID.MDB.
    'Desjhalb werden die einzelnen Felder von TBL_RWERT getrennt aufgebaut
    '
    '
    Dim j As Integer
    Dim ier As Integer
    Dim SqlStmt As String
    Dim DatSource As String
    Dim DatTarget As String
    Dim ConnTarget As OleDbConnection
    Dim CMDComm As OleDbCommand
    Dim CMDRwertSource As OleDbCommand
    Dim CMDRwertTarget As OleDbCommand
    Dim RwertAdapt As OleDbDataAdapter
    Dim DatRwertFarbm As OleDbDataReader
    Dim Kenn As String
    Dim Bem As String
    ConnTarget = New OleDbConnection
    CMDComm = New OleDbCommand("", ConnTarget)
    CMDRwertSource = New OleDbCommand("", Cndat)
    CMDRwertTarget = New OleDbCommand("", ConnTarget)
    RwertAdapt = New OleDbDataAdapter
    DatSource = GetFileProfile("%PROGRAMFILES%/COLORAPPLPROG/COLORDATAVOID.MDB")
    If Not File.Exists(DatSource) Then
      MsgBox(DatSource & "  nicht vorhanden")
      Exit Sub
    End If

    DatTarget = NewFileName(Cndat.DataSource, "COLORZWIDATA.MDB")
    '
    '
    '
    ConnTarget.ConnectionString = NewConnectionstring(Cndat.ConnectionString, DatTarget)

    '
    'Datenbank COLORDATAVOID.MDB kopieren
    '
    '
    File.Delete(DatTarget)
    File.Copy(DatSource, DatTarget)
    '
    Cursor = Cursors.WaitCursor

    '
    '
    '
    If ConnOpen(Cndat) Then
      Call TestBeziehungen(Cndat, "TBL_RWERT", {"MESSGRW_ID", "RWERT_ID"}, "TBL_QUALI", {"MESSGRW_ID", "QUALI_ID"}, True)


      '
      'Tabellen von COLORDATA.MDB nach COLORZWIDATA.MDB kopieren
      '
      'TBL_RWERT
      '
      SqlStmt = "INSERT INTO TBL_RWERT (RWERT_ID,MESSGRW_ID,MESSG_ID,USER_ID,RWERT_GID,RWERT_NAME,RWERT_BEM,RWERT_KENN,RWERT_IARCH,RWERT_DATTIM,RWERT_CME,RWERT_RETR,RWERT_IAMI,RWERT_DE,RWERT_RWERT)" _
         & " VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      CMDRwertTarget.CommandText = SqlStmt
      CMDRwertTarget.Parameters.Clear()
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_ID", OleDbType.Integer))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("MESSGRW_ID", OleDbType.Integer))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("MESSG_ID", OleDbType.Integer))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("USER_ID", OleDbType.Integer))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_GID", OleDbType.Integer))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_NAME", OleDbType.Char))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_BEM", OleDbType.Char))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_KENN", OleDbType.Char))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_IARCH", OleDbType.SmallInt))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_DATTIM", OleDbType.Date))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_CME", OleDbType.Char))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_RETR", OleDbType.SmallInt))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_IAMI", OleDbType.SmallInt))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_DE", OleDbType.Binary))
      CMDRwertTarget.Parameters.Add(New OleDbParameter("RWERT_RWERT", OleDbType.Binary))
      CMDRwertTarget.CommandText = SqlStmt

      '
      '
      '
      '

      '
      CMDRwertSource.CommandText = "SELECT * FROM TBL_RWERT"
      DatRwertFarbm = DataRead(CMDRwertSource, System.Data.CommandBehavior.CloseConnection, Cndat)
      ConnTarget.Open()
      Do While DatRwertFarbm.Read
        CMDRwertTarget.Parameters("Rwert_ID").Value = DatRwertFarbm("RWERT_ID")
        CMDRwertTarget.Parameters("messgrw_id").Value = DatRwertFarbm("MESSGRW_ID")
        CMDRwertTarget.Parameters("messg_id").Value = DatRwertFarbm("MESSG_ID")
        CMDRwertTarget.Parameters("user_id").Value = DatRwertFarbm("USER_ID")
        CMDRwertTarget.Parameters("rwert_gid").Value = DatRwertFarbm("RWERT_GID")
        CMDRwertTarget.Parameters("Rwert_name").Value = DatRwertFarbm("RWERT_NAME")
        If IsDBNull(DatRwertFarbm("RWERT_KENN")) Then
          Bem = "  "
        Else
          Bem = DatRwertFarbm("RWERT_KENN")
        End If
        Kenn = DatRwertFarbm("RWERT_CME")
        CMDRwertTarget.Parameters("rwert_kenn").Value = Kenn
        CMDRwertTarget.Parameters("rwert_bem").Value = Bem
        CMDRwertTarget.Parameters("rwert_iarch").Value = DatRwertFarbm("RWERT_IARCH")
        CMDRwertTarget.Parameters("RWERT_DATTIM").Value = DatRwertFarbm("RWERT_DATTIM")
        CMDRwertTarget.Parameters("rwert_cme").Value = DatRwertFarbm("RWERT_CME").SUBSTRING(0, 2)
        CMDRwertTarget.Parameters("rwert_retr").Value = DatRwertFarbm("RWERT_RETR")
        CMDRwertTarget.Parameters("rwert_iami").Value = DatRwertFarbm("RWERT_IAMI")
        CMDRwertTarget.Parameters("rwert_de").Value = DatRwertFarbm("RWERT_DE")
        CMDRwertTarget.Parameters("rwert_rwert").Value = DatRwertFarbm("RWERT_RWERT")
        If SQLExeNonQuery(CMDRwertTarget, ConnTarget) <> 0 Then
          ier = -1
          MsgBox("ERROR R-Werte")
        End If
      Loop

      CMDRwertTarget.Parameters.Clear()
      DatRwertFarbm.Close()
      CMDRwertTarget.Dispose()
      CMDRwertSource.Dispose()


      '
      '

      '
      'TBL_QUALI
      '
      '
      SqlStmt = "INSERT INTO TBL_QUALI SELECT * FROM TBL_QUALI IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERROR1")
        Exit Sub
      End If
      '
      '
      '
      '
      Call TestBeziehungen(Cndat, "TBL_FARBM", {"MISCH_ID", "FARBM_ID"}, "TBL_FARBM_PROZ", {"MISCH_ID", "FARBM_ID"}, True)
      Call TestBeziehungen(Cndat, "TBL_FARBM", {"MISCH_ID", "FARBM_ID"}, "TBL_FARBM_PROB", {"MISCH_ID", "FARBM_ID"}, True)
      Call TestBeziehungen(Cndat, "TBL_FARBM", {"MISCH_ID", "FARBM_ID"}, "TBL_FARBM_PREIS", {"MISCH_ID", "FARBM_ID"}, True)
      Call TestBeziehungen(Cndat, "TBL_FARBM", {"MISCH_ID", "FARBM_ID"}, "TBL_GRUND_FARBM", {"MISCH_ID", "FARBM_ID"}, True)



      '
      '
      '
      '
      '
      'TBL_FARBM
      '
      '
      SqlStmt = "INSERT INTO TBL_FARBM SELECT * FROM TBL_FARBM IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERROR2")
        Exit Sub
      End If
      '
      '
      '
      'GLZGRD_ID prüfen und u.U. setzen
      '
      '
      CMDRwertSource.CommandText = "SELECT * FROM TBL_FARBM"
      DatRwertFarbm = DataRead(CMDRwertSource, System.Data.CommandBehavior.CloseConnection, ConnTarget)
      Do While DatRwertFarbm.Read
        If IsDBNull(DatRwertFarbm("GLZGRD_ID")) OrElse DatRwertFarbm("GLZGRD_ID") = 0 Then
          CMDComm.CommandText = "UPDATE TBL_FARBM SET GLZGRD_ID=" & DatRwertFarbm("FARBM_ID") & " WHERE MISCH_ID=" & DatRwertFarbm("MISCH_ID") & " AND FARBM_ID=" & DatRwertFarbm("FARBM_ID")
          If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
            ier = -1
            MsgBox("ERROR GLZGRD_ID")
          End If
        End If
      Loop
      DatRwertFarbm.Close()
      '
      'TBL_FARBM_PROZ
      '
      '
      SqlStmt = "INSERT INTO TBL_FARBM_PROZ SELECT * FROM TBL_FARBM_PROZ IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERROR3")
        Exit Sub
      End If
      '
      'TBL_FARBM_PROB
      '
      '
      SqlStmt = "INSERT INTO TBL_FARBM_PROB SELECT * FROM TBL_FARBM_PROB IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERROR4")
        Exit Sub
      End If
      '
      'TBL_FARBM_PREIS
      '
      '
      SqlStmt = "INSERT INTO TBL_FARBM_PREIS SELECT * FROM TBL_FARBM_PREIS IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERROR5")
        Exit Sub
      End If
      '
      'TBL_GRUND_FARBM
      '
      '
      SqlStmt = "INSERT INTO TBL_GRUND_FARBM SELECT * FROM TBL_GRUND_FARBM IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERROR6")
        Exit Sub
      End If
      '
      '
      'TBL_REZEPT
      '
      '
      Call TestBeziehungen(Cndat, "TBL_REZEPT", {"MISCH_ID", "REZEPT_ID"}, "TBL_REZEPT_FARBM", {"MISCH_ID", "REZEPT_ID"}, True)
      Call TestBeziehungen(Cndat, "TBL_RWERT", {"MESSGRW_ID", "RWERT_ID"}, "TBL_REZEPT_RWERT", {"MESSGRW_ID", "RWERT_ID"}, True)
      Call TestBeziehungen(Cndat, "TBL_REZEPT", {"MISCH_ID", "REZEPT_ID"}, "TBL_REZEPT_RWERT", {"MISCH_ID", "REZEPT_ID"}, True)
      '
      '
      '
      '
      SqlStmt = "INSERT INTO TBL_REZEPT SELECT * FROM TBL_REZEPT IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERROR7")
        Exit Sub
      End If
      '
      'TBL_REZEPT_FARBM
      '
      '
      SqlStmt = "INSERT INTO TBL_REZEPT_FARBM SELECT * FROM TBL_REZEPT_FARBM IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERROR8")
        Exit Sub
      End If
      '
      'TBL_REZEPT_RWERT
      '
      '
      SqlStmt = "INSERT INTO TBL_REZEPT_RWERT SELECT * FROM TBL_REZEPT_RWERT IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERROR9")
        Exit Sub
      End If
      '
      Call TestBeziehungen(Cndat, "TBL_SORTI", {"MISCH_ID", "SORTI_ID"}, "TBL_SORTI_FARBM", {"MISCH_ID", "SORTI_ID"}, True)
      Call TestBeziehungen(Cndat, "TBL_RWERT", {"MESSGRW_ID", "RWERT_ID"}, "TBL_SORTI_RWERT", {"MESSGRW_ID", "RWERT_ID"}, True)
      Call TestBeziehungen(Cndat, "TBL_SORTI", {"MISCH_ID", "SORTI_ID"}, "TBL_SORTI_RWERT", {"MISCH_ID", "SORTI_ID"}, True)
      '
      '
      '
      'TBL_SORTI
      '
      '
      SqlStmt = "INSERT INTO TBL_SORTI SELECT * FROM TBL_SORTI IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERRORA")
        Exit Sub
      End If
      '
      'TBL_SORTI_FARBM
      '
      '
      SqlStmt = "INSERT INTO TBL_SORTI_FARBM SELECT * FROM TBL_SORTI_FARBM IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERRORB")
        Exit Sub
      End If
      '
      'TBL_SORTI_RWERT
      '
      '
      '
      'Call StoreSingleTarget(Cndat, "TBL_SORTI_RWERT", ConnTarget, "TBL_SORTI_RWERT")

      '
      SqlStmt = "INSERT INTO TBL_SORTI_RWERT SELECT * FROM TBL_SORTI_RWERT IN '" & Cndat.DataSource & "'"
      CMDComm.CommandText = SqlStmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        MsgBox("ERRORC")
        Exit Sub
      End If
      Cndat.Close()
    End If
    Cursor = Cursors.Default
    MsgBox("Datenbank COLORDATA.MDB nach COLORZWIDATA.MDB kopiert")


  End Sub

  Sub StoreSingleTarget(CnSource As OleDbConnection, TabSource As String, CnTarget As OleDbConnection, TabTarget As String)
    Dim j As Integer
    Dim DatReadSource As OleDbDataReader
    Dim CmdSource As OleDbCommand
    Dim CmdTarget As OleDbCommand
    Dim SqlStmt As String
    Dim StrError As String
    CmdSource = New OleDbCommand("", CnSource)
    CmdTarget = New OleDbCommand("", CnTarget)
    CmdSource.CommandText = "SELECT * FROM " & TabSource
    DatReadSource = DataRead(CmdSource, System.Data.CommandBehavior.CloseConnection, CnSource)
    '
    '
    'Speichern nach TabTarget
    '
    '
    SqlStmt = "INSERT INTO " & TabTarget & " ("
    For j = 0 To DatReadSource.FieldCount - 1
      SqlStmt = SqlStmt & DatReadSource.GetName(j)
      If j = DatReadSource.FieldCount - 1 Then
        SqlStmt = SqlStmt & ") VALUES("
      Else
        SqlStmt = SqlStmt & ","
      End If
    Next
    For j = 0 To DatReadSource.FieldCount - 1
      If j = DatReadSource.FieldCount - 1 Then
        SqlStmt = SqlStmt & "?)"
      Else
        SqlStmt = SqlStmt & "?,"
      End If
    Next
    CmdTarget.CommandText = SqlStmt
    CmdTarget.Parameters.Clear()
    For j = 0 To DatReadSource.FieldCount - 1
      CmdTarget.Parameters.Add(DatReadSource.GetName(j), GetOleDbType(DatReadSource.GetFieldType(j)))
    Next
    Do While DatReadSource.Read
      For j = 0 To DatReadSource.FieldCount - 1
        CmdTarget.Parameters(DatReadSource.GetName(j)).Value = DatReadSource.GetValue(j)
      Next
      If SQLExeNonQuery(CmdTarget, CnTarget) <> 0 Then
        StrError = ""
        For j = 0 To DatReadSource.FieldCount - 1
          StrError = StrError & DatReadSource.GetName(j) & "=" & DatReadSource.GetValue(j) & " "
        Next
        MsgBox("Error:  " & StrError)
        Continue Do
      End If
    Loop
  End Sub
  Public Sub New()
    
    ' Dieser Aufruf ist für den Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.

  End Sub
 
  Private Sub btnFileRepair_Click(sender As System.Object, e As System.EventArgs) Handles btnFileRepair.Click
    Call ColorFileRepair()
    Cursor = Cursors.Default
  End Sub
  Sub ColorFileRepair()
    'Das Programm kopiert die Datenbank COLORFILE.MDB in eine Zwischendatei COLORZWIFILE.MDB. Folgende Schritte sind erforderlich
    'In Tabname sind die Namen der erforderlichen Tabelen aufgeführt 
    '1. Name (COLORZWIFILE.MDB) der Zieldatei (DATTARGET) festlegen 
    '2. COLORSYST.MDB wird nach Zieldatei (COLORZWIFILE.MDB) kopiert
    '3. Prüfungen für Tabellen durchführen
    '4. Tabelleninhalt von COLORZWIFILE.MDB wird gelöscht
    '5. Tabelleninhalte von COLORFILE.MDB werden nach COLORZWIFILE.MDB geschrieben
    '
    '
    'Damit sind alle Daten der bisherigen COLORFILE.MDB umgespeichert und die Beziehungen zwischen den Tabellen wieder hergestellt
    'Jetzt kann Colorfile.MDB in ColorfileAlt.Mdb umbenant werden
    'und COLORZWIFILE.MDB wird in Colorfile.MDB umbenannt
    Dim j As Integer
    Dim i As Integer
    Dim TableList As List(Of String)
    Dim Sqlstmt As String
    Dim DatSource As String
    Dim DatTarget As String
    Dim ConnTarget As OleDbConnection
    Dim CMDComm As OleDbCommand
    ConnTarget = New OleDbConnection
    Dim Tabname() As String = _
      {"TBL_AUFG", "TBL_ANWSG", "TBL_AUSW", "TBL_MERK", "TBL_ANWSG_MERK", "TBL_BEREICH", "TBL_BWERT", "TBL_FSTAE", "TBL_IHRM", "TBL_LABOR", "TBL_LCHG", _
       "TBL_LICHT", "TBL_LICHT_BEREICH_SPEK", "TBL_MATPA", "TBL_MENU", "TBL_GKWRT", "TBL_GKWRTEXT", "TBL_GRUSTART", _
       "TBL_MESSG", "TBL_MESSG_IHRM", "TBL_MESSG_GROUP", "TBL_METH", "TBL_METH_ANWSG", "TBL_PROART", "TBL_PRUEFM", "TBL_REZMN", "TBL_PARAM", "TBL_PARAM_LIST", "TBL_AUSW_PARAM", _
       "TBL_MISCH", "TBL_MISCH_GROUP", "TBL_MISCH_MESSG", "TBL_MISCH_GROUP_MESSG", _
       "TBL_USER", "TBL_USER_METH", "TBL_USER_MISCH", "TBL_USER_MESSG", "TBL_USER_METH_ANWSG", "TBL_USER_METH_ANWSG_MERK", "TBL_USER_METH_LICHT", "TBL_USER_METH_MESSG", "TBL_USER_METH_MESSG_IHRM", _
       "TBL_USER_METH_MISCH", "TBL_USER_METH_PARAM", "TBL_USER_MISCH_GROUP_DONTSHOW", "TBL_USER_MISCH_GROUP_READONLY", _
       "TBL_USER_MESSG_GROUP_DONTSHOW", "TBL_USER_MESSG_GROUP_READONLY", "TBL_USER_MISCH_MESSG", _
       "TBL_TEXTE_GER", "TBL_TEXTE_ENG", "TBL_TEXTE_ESP", "TBL_TEXTE_FRE", "TBL_TEXTE_POR", "TBL_TEXTE_ITL", "TBL_TEXTE_USE"}
    CMDComm = New OleDbCommand("", ConnTarget)
    DatSource = GetFileProfile("%PROGRAMFILES%/COLORAPPLPROG/COLORSYST.MDB")
    If Not File.Exists(DatSource) Then
      MsgBox(DatSource & "  nicht vorhanden")
      Exit Sub
    End If

    DatTarget = NewFileName(Cncol.DataSource, "COLORZWIFILE.MDB")
    '
    '
    '
    ConnTarget.ConnectionString = NewConnectionstring(Cncol.ConnectionString, DatTarget)
    '
    'Datenbank COLORSYST.MDB kopieren
    '
    '
    File.Delete(DatTarget)
    File.Copy(DatSource, DatTarget)
    '
    '
    '
    'Prüfen auf doppelten Eintrag
    '
    '
    '
    For i = 0 To Tabname.Count - 2
      For j = i + 1 To Tabname.Count - 1
        If Tabname(j) = Tabname(i) Then
          MsgBox(" Doppelter Eintrag  " & Tabname(j))
        End If
      Next
    Next

    TableList = New List(Of String)
    Call GetTabledefs(TableList, ConnTarget)
    '
    '
    'Prüfen, ob alle Tabellen in Tabname
    '
    '
    For i = 0 To TableList.Count - 1
      If TableList(i).Substring(0, 3) = "TBL" Then
        For j = 0 To Tabname.Count - 1
          If TableList(i) = Tabname(j) Then
            Exit For
          End If
        Next
        If j = Tabname.Count Then
          MsgBox(TableList(i) & " nicht vorhanden")
        End If
      End If
    Next

    Cursor = Cursors.WaitCursor
    '
    '
    '
    '
    'Tabellen in COLORZWFILE.MDB löschen
    '
    '
    '
    For j = Tabname.Count - 1 To 0 Step -1
      Sqlstmt = "DELETE * FROM " & Tabname(j)
      CMDComm.CommandText = Sqlstmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        Exit Sub
      End If
    Next
    '
    'Tabellen von Colorfile.mdb nach  COLORZWIFILE.MDB kopieren
    '
    '
    Call TestBeziehungen(Cncol, "TBL_MESSG_GROUP", {"MESSG_ID", "GROUP_ID"}, "TBL_USER_MESSG_GROUP_DONTSHOW", {"MESSG_ID", "GROUP_ID"}, True)
    Call TestBeziehungen(Cncol, "TBL_MESSG_GROUP", {"MESSG_ID", "GROUP_ID"}, "TBL_USER_MESSG_GROUP_READONLY", {"MESSG_ID", "GROUP_ID"}, True)

    '
    '
    For j = 0 To Tabname.Count - 1
      Sqlstmt = "INSERT INTO " & Tabname(j) & " SELECT * FROM " & Tabname(j) & " IN '" & Cncol.DataSource & "'"
      CMDComm.CommandText = Sqlstmt
      If SQLExeNonQuery(CMDComm, ConnTarget) <> 0 Then
        Exit Sub
      End If
    Next j
    '
    '
    '
    '
    Cursor = Cursors.Default
    MsgBox("Datenbank COLORFILE.MDB nach COLORZWIFILE.MDB kopiert")
  End Sub


  

  



  
  Private Sub btnColorfile_Click(sender As System.Object, e As System.EventArgs) Handles btnColorfileDisplay.Click
    Dim i As Integer
    Dim ColIniFile As String
    Dim TblUser As DataTable
    Dim TblMessg As DataTable
    Dim TblMisch As DataTable
    Dim AdaptUser As OleDbDataAdapter
    Dim AdaptMessg As OleDbDataAdapter
    Dim AdaptMisch As OleDbDataAdapter
   
    '
    '
    IniSt = True
    '
    '
    MasterFolder = New FolderBrowserDialog
    MasterFolder.ShowNewFolderButton = False
    'MasterFolder.RootFolder = Environment.SpecialFolder.UserProfile


    MasterFolder.SelectedPath = GetFileProfile("%USERPROFILE%")
    MasterFolder.Description = "Ordner auswählen"
    If MasterFolder.ShowDialog() = Windows.Forms.DialogResult.Cancel Then
      Exit Sub
    End If
    MsgBox(MasterFolder.SelectedPath)
    My.Computer.FileSystem.CurrentDirectory = MasterFolder.SelectedPath
    '
    '
    '
    'Suche nach COLORFILE.INI
    '
    ColIniFile = MasterFolder.SelectedPath & "\" & ColINI
    If Not File.Exists(ColIniFile) Then
      MsgBox(ColINI & " nicht gefunden")
      Exit Sub
    End If
    '
    AdaptUser = New OleDbDataAdapter
    AdaptMessg = New OleDbDataAdapter
    AdaptMisch = New OleDbDataAdapter
    AdaptUser.SelectCommand = New OleDbCommand("", Cncol)
    AdaptMessg.SelectCommand = New OleDbCommand("", Cncol)
    AdaptMisch.SelectCommand = New OleDbCommand("", Cncol)
    AdaptUser.SelectCommand.CommandText = "SELECT USER_ID,USER_NAME FROM TBL_USER ORDER BY USER_NAME"
    TblUser = New DataTable
    If Not FillDatset(AdaptUser, TblUser) Then
      Exit Sub
    End If
    TblUser.AcceptChanges()
    '
    AdaptMessg.SelectCommand.CommandText = "SELECT MESSG_ID,MESSG_KBEZ FROM TBL_MESSG ORDER BY MESSG_KBEZ"
    TblMessg = New DataTable
    If Not FillDatset(AdaptMessg, TblMessg) Then
      Exit Sub
    End If
    TblMessg.AcceptChanges()
    '
    AdaptMisch.SelectCommand.CommandText = "SELECT MISCH_ID,MISCH_KBEZ FROM TBL_MISCH ORDER BY MISCH_KBEZ"
    TblMisch = New DataTable
    If Not FillDatset(AdaptMisch, TblMisch) Then
      Exit Sub
    End If
    TblMisch.AcceptChanges()
    '
    CnColF = New OleDbConnection
    CnColF.ConnectionString = NewConnectionstring(Cncol.ConnectionString, My.Computer.FileSystem.CurrentDirectory & "\ConfigMankiewicz.mdb")
    '


    '
    '
    '
    TabColorfile = New DataTable
    dbgColorfile.Columns.Clear()
    AdaptColorfileini = New OleDbDataAdapter
    AdaptColorfileini.SelectCommand = New OleDbCommand("", CnColF)
    AdaptColorfileini.SelectCommand.CommandText = "SELECT * FROM " & TblCol
    dbgColorfile.Columns.Add("ID", "ID")
    dbgColorfile.Columns.Add("SubDir", "SubDir")
    '
    '
    dataUser = New DataGridViewComboBoxColumn
    dataUser.DataSource = TblUser
    dataUser.Name = "USERID"
    dataUser.HeaderText = "USER"
    dbgColorfile.Columns.Add(dataUser)
    '
    '
    dbgColorfile.Columns.Add("STDNID", "STDNID")
    '
    '
    dataMessg = New DataGridViewComboBoxColumn
    dataMessg.DataSource = TblMessg
    dataMessg.Name = "MESSGID"
    dataMessg.HeaderText = "MESSG"
    dbgColorfile.Columns.Add(dataMessg)
    '
    '
    dataMisch = New DataGridViewComboBoxColumn
    dataMisch.DataSource = TblMisch
    dataMisch.Name = "MISCHID"
    dataMisch.HeaderText = "MISCH"
    dbgColorfile.Columns.Add(dataMisch)
    '
    '
    dbgColorfile.Columns.Add("MESSGKE", "MESSGKE")
    dbgColorfile.Columns.Add("PASSW", "PASSW")
    dbgColorfile.Columns.Add("GLZGRD", "GLZGRD")
    '
    '
    '*******************Columns.Add
    '
    '
    '
    '
    dbgColorfile.Columns("ID").Width = 20
    dbgColorfile.Columns("SubDir").Width = 60
    dbgColorfile.Columns("USERID").Width = 100
    dbgColorfile.Columns("STDNID").Width = 60
    dbgColorfile.Columns("MESSGID").Width = 100
    dbgColorfile.Columns("MISCHID").Width = 100
    dbgColorfile.Columns("PASSW").Width = 100
    dbgColorfile.Columns("MESSGKE").Width = 60
    '
    '
    dataSprache = New DataGridViewComboBoxColumn
    For i = 0 To LangText.Count - 1
      dataSprache.Items.Add(New ListTextID(i, LangText(i)))
    Next i
    dataSprache.HeaderText = "SPRACHE"
    dataSprache.Name = "SPRACHE"
    dbgColorfile.Columns.Add(dataSprache)
    dbgColorfile.Columns("SPRACHE").Width = 80
    '
    'für [MANKIEWICZ]
    '
    '
    '
    dbgColorfile.Columns.Add("METHID", "METHID")
    dbgColorfile.Columns.Add("DRUREZ", "DRUREZ")
    dbgColorfile.Columns.Add("DRUKOR", "DRUKOR")
    dbgColorfile.Columns.Add("PRODNR", "PRODNR")
    dbgColorfile.Columns.Add("ZUSINF", "ZUSINF")
    dbgColorfile.Columns.Add("FOLDER", "FOLDER")
    dbgColorfile.Columns.Add("VERWEIS", "VERWEIS")
    dbgColorfile.Columns.Add("FIRMA", "FIRMA")
    dbgColorfile.Columns.Add("STRASSE", "STRASSE")
    dbgColorfile.Columns.Add("ORT", "ORT")
    dbgColorfile.Columns.Add("TEL", "TEL")
    '
    '
    dbgColorfile.Columns("PRODNR").Width = 60
    '
    '
    '**************Columns.Width
    '
    '

    For i = 0 To dbgColorfile.Columns.Count - 1
      dbgColorfile.Columns(i).DataPropertyName = dbgColorfile.Columns(i).Name
    Next
    If Not FillDatset(AdaptColorfileini, TabColorfile) Then
      Exit Sub
    End If
    TabColorfile.AcceptChanges()
    dataSprache.ValueMember = "ID"
    dataSprache.DisplayMember = "TEXT"
    '
    dataUser.DisplayMember = "USER_NAME"
    dataUser.ValueMember = "USER_ID"
    '
    '
    dataMessg.DisplayMember = "MESSG_KBEZ"
    dataMessg.ValueMember = "MESSG_ID"
    '
    dataMisch.DisplayMember = "MISCH_KBEZ"
    dataMisch.ValueMember = "MISCH_ID"

    '
    dbgColorfile.DataSource = TabColorfile
    '
    '
    'Default-Werte
    '
    '
    TabColorfile.Columns("ID").DefaultValue = MaxCopyId(TabColorfile) + 1
    TabColorfile.Columns("SubDir").DefaultValue = "??????"
    TabColorfile.Columns("USERID").DefaultValue = GetPrivSettings("IDNUMBERS", "USERID", "-1", ColIniFile)
    TabColorfile.Columns("STDNID").DefaultValue = GetPrivSettings("IDNUMBERS", "STDNID", "-1", ColIniFile)
    TabColorfile.Columns("MESSGID").DefaultValue = GetPrivSettings("IDNUMBERS", "MESSGID", "-1", ColIniFile)
    TabColorfile.Columns("MISCHID").DefaultValue = GetPrivSettings("IDNUMBERS", "MISCHID", "-1", ColIniFile)
    TabColorfile.Columns("MESSGKE").DefaultValue = GetPrivSettings("STARTUP", "MESSGKE", "  ", ColIniFile)
    TabColorfile.Columns("SPRACHE").DefaultValue = 0
    '
    '[MANKIEWICZ]
    '
    TabColorfile.Columns("PASSW").DefaultValue = GetPrivSettings("MANKIEWICZ", "PASSW", "", ColIniFile)
    TabColorfile.Columns("METHID").DefaultValue = GetPrivSettings("MANKIEWICZ", "METHID", " ", ColIniFile)
    TabColorfile.Columns("DRUREZ").DefaultValue = GetPrivSettings("MANKIEWICZ", "DRUREZ", " ", ColIniFile)
    TabColorfile.Columns("DRUKOR").DefaultValue = GetPrivSettings("MANKIEWICZ", "DRUKOR", " ", ColIniFile)
    TabColorfile.Columns("PRODNR").DefaultValue = GetPrivSettings("MANKIEWICZ", "PRODNR", " ", ColIniFile)
    TabColorfile.Columns("GLZGRD").DefaultValue = GetPrivSettings("MANKIEWICZ", "GLZGRD", 0, ColIniFile)
    TabColorfile.Columns("ZUSINF").DefaultValue = GetPrivSettings("MANKIEWICZ", "ZUSINF", " ", ColIniFile)
    TabColorfile.Columns("FOLDER").DefaultValue = GetPrivSettings("MANKIEWICZ", "FOLDER", " ", ColIniFile)
    TabColorfile.Columns("VERWEIS").DefaultValue = GetPrivSettings("MANKIEWICZ", "VERWEIS", " ", ColIniFile)
    TabColorfile.Columns("FIRMA").DefaultValue = GetPrivSettings("MANKIEWICZ", "FIRMA", " ", ColIniFile)
    TabColorfile.Columns("STRASSE").DefaultValue = GetPrivSettings("MANKIEWICZ", "STRASSE", " ", ColIniFile)
    TabColorfile.Columns("ORT").DefaultValue = GetPrivSettings("MANKIEWICZ", "ORT", " ", ColIniFile)
    TabColorfile.Columns("TEL").DefaultValue = GetPrivSettings("MANKIEWICZ", "TEL", " ", ColIniFile)
    '
    '
    '
    '*************Columns.Defaultvalue=GetPrivSetting (von z.B. MasterColorFile-Verzeichnis)
    '
    btnColorfileSpeichern.Enabled = True
  End Sub

  Private Sub Schreiben()
    '
    Dim SubFile As String
    Dim SubDir As String
    Dim i As Integer
    Dim j As Integer
    dbgColorfile.EndEdit()

    '
    '
    'TBL_COLORFILE
    '
    '
    AdaptColorfileini.InsertCommand = OleDBInsertCmd(TblCol, CnColF)
    AdaptColorfileini.DeleteCommand = OleDBDeleteCmd(TblCol, {"ID"}, CnColF)
    AdaptColorfileini.UpdateCommand = OleDBUpdateCmd(TblCol, {"ID"}, CnColF)
    '
    '
    '
    'Delete
    '
    '
    AdaptColorfileini.Update(TabColorfile.Select(Nothing, Nothing, DataViewRowState.Deleted))
    '
    '
    'Insert
    '
    '
    AdaptColorfileini.Update(TabColorfile.Select(Nothing, Nothing, DataViewRowState.Added))
    '
    '
    'Update
    '
    '
    AdaptColorfileini.Update(TabColorfile.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
    '
    '
    '
    '
    '
    TabColorfile.AcceptChanges()
    '
    'COLORFILE.INI anlegen und aufbauen
    '
    For i = 0 To TabColorfile.Rows.Count - 1
      SubDir = MasterFolder.SelectedPath & "\" & TabColorfile.Rows(i)("SubDir")
      If Not Directory.Exists(SubDir) Then
        '
        'Subdirectory anlegen
        '
        '
        Directory.CreateDirectory(SubDir)
        '
      End If
      '
      SubFile = SubDir & "\" & ColINI
      If Not File.Exists(SubFile) Then
        '
        '
        'COLORFILE.INI kopieren
        '
        '
        File.Copy(MasterFolder.SelectedPath & "\" & ColINI, SubFile)
      End If
      '
      '
      'Update Subfile
      '
      '

      LetPrivSettings("IDNUMBERS", "USERID", SubFile) = TabColorfile.Rows(i)("USERID")
      LetPrivSettings("IDNUMBERS", "STDNID", SubFile) = TabColorfile.Rows(i)("STDNID")
      LetPrivSettings("IDNUMBERS", "MESSGID", SubFile) = TabColorfile.Rows(i)("MESSGID")
      LetPrivSettings("IDNUMBERS", "MISCHID", SubFile) = TabColorfile.Rows(i)("MISCHID")
      If IsDBNull(TabColorfile.Rows(i)("PASSW")) OrElse TabColorfile.Rows(i)("PASSW") = "" Then
        LetPrivSettings("MANKIEWICZ", "PASSW", SubFile) = ""
      Else
        LetPrivSettings("MANKIEWICZ", "PASSW", SubFile) = TabColorfile.Rows(i)("PASSW")
      End If
      If IsDBNull(TabColorfile.Rows(i)("MESSGKE")) OrElse TabColorfile.Rows(i)("MESSGKE") = "" Then
        LetPrivSettings("STARTUP", "MESSGKE", SubFile) = "  "
      Else
        LetPrivSettings("STARTUP", "MESSGKE", SubFile) = TabColorfile.Rows(i)("MESSGKE")
      End If
      LetPrivSettings("LANGUAGE", "INDEX", SubFile) = TabColorfile.Rows(i)("SPRACHE") + 1
      LetPrivSettings("LANGUAGE", "TABLE", SubFile) = LangTable(TabColorfile.Rows(i)("SPRACHE"))
      '
      '[MANKIEWICZ]
      '
      LetPrivSettings("MANKIEWICZ", "METHID", SubFile) = TabColorfile.Rows(i)("METHID")
      LetPrivSettings("MANKIEWICZ", "DRUREZ", SubFile) = TabColorfile.Rows(i)("DRUREZ")
      LetPrivSettings("MANKIEWICZ", "DRUKOR", SubFile) = TabColorfile.Rows(i)("DRUKOR")

      '
      If IsDBNull(TabColorfile.Rows(i)("PRODNR")) OrElse TabColorfile.Rows(i)("PRODNR") = "" Then
        LetPrivSettings("MANKIEWICZ", "PRODNR", SubFile) = vbNullString
      Else
        LetPrivSettings("MANKIEWICZ", "PRODNR", SubFile) = TabColorfile.Rows(i)("PRODNR")
      End If
      '
      '
      If IsDBNull(TabColorfile.Rows(i)("GLZGRD")) Then
        LetPrivSettings("MANKIEWICZ", "GLZGRD", SubFile) = 0
      Else
        LetPrivSettings("MANKIEWICZ", "GLZGRD", SubFile) = TabColorfile.Rows(i)("GLZGRD")
      End If
      '
      If IsDBNull(TabColorfile.Rows(i)("ZUSINF")) OrElse TabColorfile.Rows(i)("ZUSINF") = "" Then
        LetPrivSettings("MANKIEWICZ", "ZUSINF", SubFile) = vbNullString
      Else
        LetPrivSettings("MANKIEWICZ", "ZUSINF", SubFile) = TabColorfile.Rows(i)("ZUSINF")
      End If
      LetPrivSettings("MANKIEWICZ", "FOLDER", SubFile) = TabColorfile.Rows(i)("FOLDER")

      If IsDBNull(TabColorfile.Rows(i)("VERWEIS")) OrElse TabColorfile.Rows(i)("VERWEIS") = "" Then
        LetPrivSettings("MANKIEWICZ", "VERWEIS", SubFile) = vbNullString
      Else
        LetPrivSettings("MANKIEWICZ", "VERWEIS", SubFile) = TabColorfile.Rows(i)("VERWEIS")
      End If
      '
      If IsDBNull(TabColorfile.Rows(i)("FIRMA")) OrElse TabColorfile.Rows(i)("FIRMA") = "" Then
        LetPrivSettings("MANKIEWICZ", "FIRMA", SubFile) = vbNullString
      Else
        LetPrivSettings("MANKIEWICZ", "FIRMA", SubFile) = TabColorfile.Rows(i)("FIRMA")
      End If
      '
      If IsDBNull(TabColorfile.Rows(i)("STRASSE")) OrElse TabColorfile.Rows(i)("STRASSE") = "" Then
        LetPrivSettings("MANKIEWICZ", "STRASSE", SubFile) = vbNullString
      Else
        LetPrivSettings("MANKIEWICZ", "STRASSE", SubFile) = TabColorfile.Rows(i)("STRASSE")
      End If
      '
      If IsDBNull(TabColorfile.Rows(i)("ORT")) OrElse TabColorfile.Rows(i)("ORT") = "" Then
        LetPrivSettings("MANKIEWICZ", "ORT", SubFile) = vbNullString
      Else
        LetPrivSettings("MANKIEWICZ", "ORT", SubFile) = TabColorfile.Rows(i)("ORT")
      End If
      '
      If IsDBNull(TabColorfile.Rows(i)("TEL")) OrElse TabColorfile.Rows(i)("TEL") = "" Then
        LetPrivSettings("MANKIEWICZ", "TEL", SubFile) = vbNullString
      Else
        LetPrivSettings("MANKIEWICZ", "TEL", SubFile) = TabColorfile.Rows(i)("TEL")
      End If
      '
      '
      '
      '**********LetPrivSetting
      '
      '
    Next i
  End Sub

 

  Private Sub dbgColorfile_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles dbgColorfile.DataError
    'MsgBox(e.Exception.Message)
    e.Cancel = True
  End Sub

  Private Sub frmCopyDataDB_FormClosing(sender As Object, e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
    If IniSt Then
      btnColorfileSpeichern.PerformClick()
    End If

  End Sub

  Private Sub frmCopyDataDB_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Me.Text = "ColorCopyDataDB"
  End Sub

  Private Sub btnColorfileSpeichern_Click(sender As System.Object, e As System.EventArgs) Handles btnColorfileSpeichern.Click
    If MessageBox.Show("Wollen Sie die Änderungen abspeichern?", "Meldung", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.No Then
      Exit Sub
    End If
    Call Schreiben()
  End Sub
  Function MaxCopyId(ByRef TabColorFile As DataTable) As Integer
    Dim i As Integer
    MaxCopyId = 0
    TabColorFile.AcceptChanges()
    For i = 0 To TabColorFile.Rows.Count - 1
      If MaxCopyId < TabColorFile.Rows(i)("ID") Then
        MaxCopyId = TabColorFile.Rows(i)("ID")
      End If
    Next
  End Function
  '
  '
  '
  'Kopieren, Einfügen, Ausschneiden
  '
  '
  '
  '


  'Copy Code
  Private Sub dbgColorfile_CellMouseClick(sender As Object, e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles dbgColorfile.CellMouseClick



    If dbgColorfile.SelectedCells.Count > 0 Then
      'dbgColorfile.ContextMenuStrip = ContextMenuStrip1
    End If
  End Sub
  Private Sub Cut_Click(sender As Object, e As System.EventArgs)
    Dim dgvcell As DataGridViewCell
    'Copy to clipboard
    CopyToClipboard()

    'Clear selected cells
    For Each dgvcell In dbgColorfile.SelectedCells
      dgvcell.Value = String.Empty
    Next
  End Sub

  'private void copyToolStripMenuItem_Click(object sender, EventArgs e)
  Private Sub Copy_Click(sender As Object, e As System.EventArgs)
    'Perform copy Operation
    CopyToClipboard()

  End Sub
  'private void pasteToolStripMenuItem_Click(object sender, EventArgs e)
  Private Sub Paste_Click(sender As Object, e As System.EventArgs)

    'Perform paste Operation
    PasteClipboardValue()
  End Sub
  'Copy/Paste routines follow...

  'The clipboard values are saved in a Dictionary, so the user can validate or manipulate the values before assigning them into gridviewcell.

  'VB
  'Shrink ▲   Copy Code
  Sub CopyToClipboard()
    Dim DataObj As DataObject
    dbgColorfile.ClipboardCopyMode = _
         DataGridViewClipboardCopyMode.EnableWithoutHeaderText
    If dbgColorfile.GetCellCount( _
        DataGridViewElementStates.Selected) > 0 Then
      'Copy to clipboard
      DataObj = dbgColorfile.GetClipboardContent
      If Not IsDBNull(DataObj) Then
        Clipboard.SetDataObject(DataObj)
      End If
    End If
  End Sub

  Sub PasteClipboardValue()
    Dim rowindex As Integer
    Dim colindex As Integer
    Dim cell As DataGridViewCell
    Dim startcell As DataGridViewCell
    Dim cbvalue As Dictionary(Of Integer, Dictionary(Of Integer, String))
    'Show Error if no cell is selected
    If (dbgColorfile.SelectedCells.Count = 0) Then

      MessageBox.Show("Please select a cell", "Paste", MessageBoxButtons.OK, MessageBoxIcon.Warning)
    End If


    'Get the starting Cell
    'DataGridViewCell startCell = GetStartCell(dataGridView1);
    startcell = GetStartCell(dbgColorfile)
    'Get the clipboard value in a dictionary
    'Dictionary<int, Dictionary<int, string>> cbValue = 
    '	ClipBoardValues(Clipboard.GetText());
    cbvalue = New Dictionary(Of Integer, Dictionary(Of Integer, String))
    cbvalue = clipboardvalues(Clipboard.GetText())
    rowindex = startcell.RowIndex
    For Each rowKey In cbvalue.Keys

      colindex = startcell.ColumnIndex
      For Each cellKey In cbvalue(rowKey).Keys

        'Check if the index is within the limit
        If colindex <= dbgColorfile.Columns.Count - 1 _
            And rowindex <= dbgColorfile.Rows.Count - 1 Then

          cell = dbgColorfile(colindex, rowindex)

          'Copy to selected cells if 'chkPasteToSelectedCells' is checked
          ' if ((chkPasteToSelectedCells.Checked && cell.Selected) ||
          '(!chkPasteToSelectedCells.Checked))
          cell.Value = cbvalue(rowKey)(cellKey)

          colindex = colindex + 1
        End If
      Next

      rowindex = rowindex + 1

    Next
  End Sub

  Private Function GetStartCell(dbgColorfile As DataGridView) As DataGridViewCell
    Dim rowindex As Integer
    Dim colindex As Integer
    Dim dgvcell As DataGridViewCell
    'get the smallest row,column index
    If (dbgColorfile.SelectedCells.Count = 0) Then
      GetStartCell = Nothing
    End If


    rowindex = dbgColorfile.Rows.Count - 1
    colindex = dbgColorfile.Columns.Count - 1

    For Each dgvcell In dbgColorfile.SelectedCells

      If (dgvcell.RowIndex < rowindex) Then
        rowindex = dgvcell.RowIndex
        If (dgvcell.ColumnIndex < colindex) Then
          colindex = dgvcell.ColumnIndex
        End If
      End If

    Next

    'return dgView[colIndex, rowIndex];
    GetStartCell = dbgColorfile(colindex, rowindex)
  End Function

  'private Dictionary<int, Dictionary<int, string>> ClipBoardValues(string clipboardValue)
  'Private Function Dict(ikey As Integer, StrKey As String) As  
  'Private Function clipboardvalue(Clipboardvalues As Clipboard) As Dictionary(Of Integer, String)
  Private Function clipboardvalues(clipboardvalue As String) As Dictionary(Of Integer, Dictionary(Of Integer, String))
    Dim copyvalues As Dictionary(Of Integer, Dictionary(Of Integer, String))
    Dim lines() As String
    Dim i As Integer
    Dim j As Integer
    Dim linecontent() As String
    'Dictionary<int, Dictionary<int, string>>
    'copyValues = new Dictionary<int, Dictionary<int, string>>();
    copyvalues = New Dictionary(Of Integer, Dictionary(Of Integer, String))
    lines = clipboardvalue.Split("\n")

    For i = 0 To lines.Length - 1

      'copyvalues(i, lines(i)) = New Dictionary(Of Integer, String)
      copyvalues(i) = New Dictionary(Of Integer, String)
      linecontent = lines(i).Split("\t")

      'if an empty cell value copied, then set the dictionary with an empty string
      'else Set value to dictionary
      If linecontent.Length = 0 Then
        copyvalues(i)(0) = String.Empty

      Else

        For j = 0 To linecontent.Length - 1
          copyvalues(i)(j) = linecontent(j)
        Next j
      End If
    Next i
    clipboardvalues = copyvalues

  End Function

  Private Sub dbgColorfile_RowHeaderMouseClick(sender As Object, e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles dbgColorfile.RowHeaderMouseClick
    Dim i As Integer
    If IsNothing(TabColorfile) Then Exit Sub
    TabColorfile.AcceptChanges()
    If TabColorfile.Rows.Count = 0 Then Exit Sub
    If e.RowIndex < TabColorfile.Rows.Count Then
      TabColorfile.Columns("ID").DefaultValue = MaxCopyId(TabColorfile) + 1
      TabColorfile.Columns("SubDir").DefaultValue = "??????"
      For i = 2 To TabColorfile.Columns.Count - 1
        TabColorfile.Columns(i).DefaultValue = TabColorfile.Rows(e.RowIndex)(i)
      Next
    Else
      TabColorfile.Columns("ID").DefaultValue = MaxCopyId(TabColorfile) + 2
    End If
  End Sub

 



End Class