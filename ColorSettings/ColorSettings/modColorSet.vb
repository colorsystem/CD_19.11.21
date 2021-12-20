Option Compare Text
Option Explicit On
Option Strict Off
Module modColorSet
  Dim FormColorSet As New frmColorSet
  Public Langindex As Integer           'Index für Sprache 0=Deutsch




  'Public TabLangName() As String = {"TBL_TEXTE_USE", "TBL_TEXTE_GER", "TBL_TEXTE_ENG", "TBL_TEXTE_ESP", "TBL_TEXTE_POR", "TBL_TEXTE_ITL", "TBL_TEXTE_FRE", "TBL_TEXTE_TOOLTIP"}
  'Public NameLangName() As String = {"", "German", "English", "Spanish", "Portuguese", "Italian", "French", ""}
  Public TabLangName() As String = {"TBL_TEXTE_USE", "TBL_TEXTE_GER", "TBL_TEXTE_ENG", "TBL_TEXTE_ESP", "TBL_TEXTE_POR", "TBL_TEXTE_ITL", "TBL_TEXTE_FRE"}
  Public NameLangName() As String = {"", "German", "English", "Spanish", "Portuguese", "Italian", "French"}




  '
  '
  'Administrator
  Public Const ManagerUser = "COLORSYS"
  Public Const ManagerPassw = "CHUZPE"
  Public Const ManagerPassa = "CHUZPA"

  'Manager
  '
  '
  Public UserMan As Boolean
  Public UserUfo As Boolean
  Public Password As String
  '
  '
  '
  '
  'MDIForm
  '
  Public MDiform As Object
  Private _cnnopen As Boolean

  Private Property cnnopen(oleDbConnection As OleDbConnection) As Boolean
    Get
      Return _cnnopen
    End Get
    Set(value As Boolean)
      _cnnopen = value
    End Set
  End Property


  '
  'Farben aus .INI-File
  '
  '
  '
  '
  '
  '
  '
  '
  Sub Main()
    If Not File.Exists(Cncol.DataSource) Then
      MsgBox("File" & " (CNNCOL): " & Cncol.DataSource & " " & "does not exist")
      Exit Sub
    End If
    If Not File.Exists(Cndat.DataSource) Then
      MsgBox("File" & " (CNNDAT): " & Cndat.DataSource & " " & "does not exist")
      Exit Sub
    End If
    If Not File.Exists(Cnkal.DataSource) Then
      LetPrivSettings("STARTUP", "CNNKAL", COLORFileName) = Cndat.ConnectionString
    End If
    If Not File.Exists(Cnsys.DataSource) Then
      MsgBox("File" & " (CNNSYS): " & Cnsys.DataSource & " " & "does not exist")
      Exit Sub
    End If
    Langindex = CLng(GetPrivSettings("LANGUAGE", "INDEX", CStr(1), COLORFileName()))
    '
    '
    '
    '
    '
    '
    '
    '
    Application.Run(New frmColorSet)

    Exit Sub


  End Sub

  Function DbIName(ByVal Index As Integer) As String
    Select Case Index
      Case 0
        DbIName = Cnsys.DataSource
      Case 1
        DbIName = Cncol.DataSource
      Case 2
        DbIName = Cndat.DataSource
      Case 3
        DbIName = Cnkal.DataSource
      Case 4
        DbIName = CnTmp.DataSource
      Case 5
        DbIName = GridFileName()
      Case 6
        DbIName = LogFileName()
    End Select

  End Function
  Sub CopyDaBase()
    Dim ier As Integer
    Dim i As Short
    Dim SqlStmt As String
    Dim CmdFarb As OleDbCommand
    Dim CmdUpdateFarb As OleDbCommand
    Dim DatReadFarb As OleDbDataReader
    Dim CmdCol As OleDbCommand
    Dim CmdAllg As OleDbCommand
    Dim CmdGen As OleDbCommand
    Dim AdaptGen As OleDbDataAdapter
    Dim ReadAllg As OleDbDataReader
    Dim TblGen As DataTable
    Dim ViewGen As DataView
    Dim RowGen As DataRow
    Dim RowUseMeth As DataRow
    Dim CmdUse As OleDbCommand
    Dim Cmdupd As OleDbCommand
    Dim AdaptUse As OleDbDataAdapter
    Dim TblUseMeth As DataTable
    Dim ViewUseMeth As DataView
    Dim TblMethAnwsg As DataTable
    Dim ViewMethAnwsg As DataView
    Dim TblUseMethAnwsg As DataTable
    Dim ViewUseMethAnwsg As DataView
    If Cnsys.ConnectionString <> Cncol.ConnectionString Then
      '
      Try



        '
        'Kopiere Kurzbezeichnungen und Bezeichnungen von Methoden nach BCSWIN
        '
        CmdAllg = New OleDbCommand("", Cncol)
        CmdGen = New OleDbCommand("", Cncol)
        AdaptGen = New OleDbDataAdapter
        AdaptGen.SelectCommand = CmdGen
        CmdUse = New OleDbCommand("", Cncol)
        AdaptUse = New OleDbDataAdapter
        AdaptUse.SelectCommand = CmdUse
        TblGen = New DataTable
        ViewGen = New DataView(TblGen)
        CmdCol = New OleDbCommand("", Cncol)

        '
        'METH_BEZ und METH_KBEZ erweitern
        '
        If ConnOpen(Cncol) Then
          If StringLength("TBL_METH", "METH_BEZ", Cncol) < 64 Then
            CmdCol.CommandText = "ALTER TABLE TBL_METH ALTER COLUMN [METH_BEZ] TEXT(64)"
            If SQLNonQuery(CmdCol, Cncol) <> 0 Then
              Exit Sub
            End If
          End If
          If StringLength("TBL_METH", "METH_KBEZ", Cncol) < 16 Then
            CmdCol.CommandText = "ALTER TABLE TBL_METH ALTER COLUMN [METH_KBEZ] TEXT(16)"
            If SQLNonQuery(CmdCol, Cncol) <> 0 Then
              Exit Sub
            End If
          End If

          '
          CmdGen.CommandText = "SELECT * FROM TBL_METH IN '" & Cnsys.DataSource & "'"
          TblGen.Constraints.Clear()
          TblGen.Rows.Clear()
          TblGen.Columns.Clear()
          If Not FillDatset(AdaptGen, TblGen) Then
            Exit Sub
          End If

          For Each RowGen In TblGen.Rows
            If IsDBNull(RowGen("METH_IGRID")) Then
              RowGen("METH_IGRID") = 0
            End If
            If IsDBNull(RowGen("METH_IEING")) Then
              RowGen("METH_IEING") = 0
            End If
            CmdCol.CommandText = "UPDATE TBL_METH SET [METH_KBEZ]='" & RowGen("METH_KBEZ") & "',[METH_BEZ]='" & RowGen("METH_BEZ") & "',METH_IGRID=" & RowGen("METH_IGRID") & ",METH_IEING=" & RowGen("METH_IEING") & " WHERE METH_ID=" & RowGen("METH_ID")
            If SQLNonQuery(CmdCol, Cncol) <> 0 Then
              Exit Sub
            End If
          Next
          Cncol.Close()
        End If
        '
        '      
        '
        '
        'Texte Methoden übernehmen
        '
        '
        '
        '
        If ConnOpen(Cncol) Then
          CmdGen.CommandText = "SELECT * FROM TBL_METH IN '" & Cnsys.DataSource & "'"
          TblGen.Constraints.Clear()
          TblGen.Rows.Clear()
          TblGen.Columns.Clear()
          If Not FillDatset(AdaptGen, TblGen) Then
            Exit Sub
          End If
          For Each RowGen In TblGen.Rows
            CmdAllg.CommandText = "SELECT * FROM TBL_METH WHERE METH_ID=" & RowGen("METH_ID")
            ReadAllg = DataRead(CmdAllg, CommandBehavior.Default, Cncol)
            If ReadAllg.Read Then
              CmdCol.CommandText = "UPDATE TBL_METH SET " _
             & "[METH_KBEZ]='" & AddHkomE(TexKt(1800 + RowGen("METH_ID"))) & "',[METH_BEZ]='" & AddHkomE(Texxt(1800 + RowGen("METH_ID"))) & "'" _
             & " WHERE METH_ID=" & RowGen("METH_ID")
            Else
              CmdCol.CommandText = "INSERT INTO TBL_METH (METH_ID,METH_KBEZ,METH_BEZ) VALUES (" & RowGen("METH_ID") _
               & ",'" & AddHkomE(TexKt(1800 + RowGen("METH_ID"))) & "','" & AddHkomE(Texxt(1800 + RowGen("METH_ID"))) & "')"
            End If
            If SQLNonQuery(CmdCol, Cncol) <> 0 Then
              Exit Sub
            End If
            ReadAllg.Close()
          Next
          '
          '
          '
          '
          '   
          '
          '
          'Feld JABST für DIN6164 (0) bzw. DIN 6167 (1) und COLLI (2)
          '
          '

          If Not FieldExists("TBL_MENU", "JABST", Cncol) Then
            Call CreateField("TBL_MENU", "JABST", "LONG", "0", ier, Cncol)
          End If
          '
          '
          '
          'USER_ID für Rezept-Datei
          '
          '
          '
          If Not FieldExists("TBL_REZEPT", "USER_ID", Cndat) Then
            Call CreateField("TBL_REZEPT", "USER_ID", "LONG", "0", ier, Cndat)
          End If

          '
          If Not FieldExists("TBL_FARBM", "GLZGRD_ID", Cndat) Then
            Call CreateField("TBL_FARBM", "GLZGRD_ID", "LONG", "-1", ier, Cndat)
            Call CreateField("TBL_FARBM", "FARBM_GLZGRD", "SINGLE", "0", ier, Cndat)
            CmdFarb = New OleDbCommand("", Cndat)
            CmdFarb.CommandText = "SELECT * FROM TBL_FARBM"
            CmdUpdateFarb = New OleDbCommand("", Cndat)
            DatReadFarb = DataReader(CmdFarb, CommandBehavior.CloseConnection, Cndat)

            If ConnOpen(Cndat) Then
              Do While DatReadFarb.Read
                CmdUpdateFarb.CommandText = "UPDATE TBL_FARBM SET GLZGRD_ID=" & DatReadFarb("FARBM_ID") _
                  & " WHERE FARBM_ID=" & DatReadFarb("FARBM_ID") & " AND MISCH_ID=" & DatReadFarb("MISCH_ID")
                If SQLNonQuery(CmdUpdateFarb, Cndat) <> 0 Then
                  MsgBox("ERROR")
                End If
              Loop
              DatReadFarb.Close()
              CmdFarb.Dispose()
              CmdUpdateFarb.Dispose()
              Cndat.Close()
            End If
          End If
          '
          '
          If Not FieldExists("TBL_REZEPT", "REZEPT_GLZGRD", Cndat) Then
            Call CreateField("TBL_REZEPT", "REZEPT_GLZGRD", "SINGLE", "0", ier, Cndat)
          End If
          '
          If Not FieldExists("TBL_SORTI", "SORTI_GLZGRD", Cndat) Then
            Call CreateField("TBL_SORTI", "SORTI_GLZGRD", "SINGLE", "0", ier, Cndat)
          End If
          If Not FieldExists("TBL_SORTI", "SORTI_REZMIN", Cndat) Then
            Call CreateField("TBL_SORTI", "SORTI_REZMIN", "SHORT", "3", ier, Cndat)
          End If
          If Not FieldExists("TBL_SORTI", "SORTI_REZMAX", Cndat) Then
            Call CreateField("TBL_SORTI", "SORTI_REZMAX", "SHORT", "4", ier, Cndat)
          End If
          '

          '
        
          '
          '
          SqlStmt = "LICHT_ID,LICHT_KENN,LICHT_KBEZ,LICHT_LBEZ"
          CmdCol.CommandText = "INSERT INTO TBL_LICHT (" & SqlStmt & ") SELECT " & SqlStmt & " FROM TBL_LICHT IN '" & Cnsys.DataSource & "' WHERE LICHT_ID NOT IN " _
          & "(SELECT LICHT_ID FROM TBL_LICHT)"
          If SQLNonQuery(CmdCol, Cncol) <> 0 Then
            Exit Sub
          End If
          '
          '
          'Neue Lichtart übernehmen
          '
          '
          '
          '
          '
          SqlStmt = "LICHT_ID,LICHT_KENN,LICHT_KBEZ,LICHT_LBEZ"
          CmdCol.CommandText = "INSERT INTO TBL_LICHT (" & SqlStmt & ") SELECT " & SqlStmt & " FROM TBL_LICHT IN '" & Cnsys.DataSource & "' WHERE LICHT_ID NOT IN " _
          & "(SELECT LICHT_ID FROM TBL_LICHT)"
          If SQLNonQuery(CmdCol, Cncol) <> 0 Then
            Exit Sub
          End If
          '
          '
          '
          '
          '
          '
          '
          SqlStmt = "LICHT_ID,BEREICH_ID,XYZ_ID,LICHT_FAKT,LICHT_SPEK"
          CmdCol.CommandText = "INSERT INTO TBL_LICHT_BEREICH_SPEK (" & SqlStmt & ") SELECT " & SqlStmt & " FROM TBL_LICHT_BEREICH_SPEK IN '" & Cnsys.DataSource & "' WHERE LICHT_ID NOT IN " _
          & "(SELECT LICHT_ID FROM TBL_LICHT_BEREICH_SPEK)"
          If SQLNonQuery(CmdCol, Cncol) <> 0 Then
            Exit Sub
          End If
          '
          '
          'Tabelle TBL_GKWRTEXT erstelen
          '
          If Not TableExists("TBL_GKWRTEXT", Cncol) Then
            Call CreateTable("TBL_GKWRTEXT", {"GKWRT_ID", "IHRM_ID", "GK01", "GK02", "GK03", "GK04", "GK05", "GK06", "GK07", "GK08", "GK09", "GK10", "GK11", "GK12", "GK13", "GK14", "GK15", "GK16"}, _
                                             {"LONG", "LONG", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE"}, ier, Cncol)
            '
            'Primary-Key
            '
            '
            Call CreatePrKey("GKPRKEY", "TBL_GKWRTEXT", {"GKWRT_ID", "IHRM_ID"}, ier, Cncol)
            '
            'Relations
            '
            Call CreateRelat("IHRMRL", "TBL_IHRM", "TBL_GKWRTEXT", {"IHRM_ID"}, {"IHRM_ID"}, "NO ACTION", "NO ACTION", ier, Cncol)
            Call CreateRelat("GKWRTRL", "TBL_GKWRT", "TBL_GKWRTEXT", {"GKWRT_ID"}, {"GKWRT_ID"}, "NO ACTION", "NO ACTION", ier, Cncol)
          End If

          '
          '
          'IHRM_BEZ erweitern
          '
          If StringLength("TBL_IHRM", "IHRM_BEZ", Cncol) < 32 Then
            CmdCol.CommandText = "ALTER TABLE TBL_IHRM ALTER COLUMN [IHRM_BEZ] TEXT(32)"
            If SQLNonQuery(CmdCol, Cncol) <> 0 Then
              Exit Sub
            End If
          End If
          '
          'Kopiere IHRM nach COLORWIN
          '
          '
          '
          SqlStmt = "IHRM_ID,IHRM_IHRM,IHRM_BEZ,IHRM_GLANZ"
          CmdCol.CommandText = "INSERT INTO TBL_IHRM (" & SqlStmt & ") SELECT " & SqlStmt & " FROM TBL_IHRM IN '" & Cnsys.DataSource & "' WHERE IHRM_ID NOT IN " _
          & "(SELECT IHRM_ID FROM TBL_IHRM)"
          If SQLNonQuery(CmdCol, Cncol) <> 0 Then
            Exit Sub
          End If
          '
          '
          'Kopiere Aufgaben nach COLORWIN
          '
          '
          '
          '
          SqlStmt = "AUFG_ID,AUFG_ART,AUFG_KBEZ,AUFG_BEZ"
          CmdCol.CommandText = "INSERT INTO TBL_AUFG (" & SqlStmt & ") SELECT " & SqlStmt & " FROM TBL_AUFG IN '" & Cnsys.DataSource & "' WHERE AUFG_ID NOT IN " _
          & "(SELECT AUFG_ID FROM TBL_AUFG)"
          If SQLNonQuery(CmdCol, Cncol) <> 0 Then
            Exit Sub
          End If


          '
          'Kopiere Auswahl nach COLORWIN
          '
          '
          SqlStmt = "AUSW_ID,AUSW_KEN,AUSW_KBEZ,AUSW_LBEZ"
          CmdCol.CommandText = "INSERT INTO TBL_AUSW (" & SqlStmt & ") SELECT " & SqlStmt & " FROM TBL_AUSW IN '" & Cnsys.DataSource & "' WHERE AUSW_ID NOT IN " _
          & "(SELECT AUSW_ID FROM TBL_AUSW)"
          If SQLNonQuery(CmdCol, Cncol) <> 0 Then
            Exit Sub
          End If
          '
          '
          'MERK_LBEZ erweitern
          '
          If StringLength("TBL_MERK", "MERK_LBEZ", Cncol) < 255 Then
            CmdCol.CommandText = "ALTER TABLE TBL_MERK ALTER COLUMN [MERK_LBEZ] TEXT(255)"
            If SQLNonQuery(CmdCol, Cncol) <> 0 Then
              Exit Sub
            End If
          End If
          '
          '
          '
          'Kopiere Merkmale nach COLORWIN
          '
          '
          '
          SqlStmt = "MERK_ID,AUSW_ID,MERK_KEN,MERK_KBEZ,MERK_LBEZ,MERK_TYP,MERK_FORM,MERK_FAKT"
          CmdCol.CommandText = "INSERT INTO TBL_MERK (" & SqlStmt & ") SELECT " & SqlStmt & " FROM TBL_MERK IN '" & Cnsys.DataSource & "' WHERE MERK_ID NOT IN " _
          & "(SELECT MERK_ID FROM TBL_MERK)"
          If SQLNonQuery(CmdCol, Cncol) <> 0 Then
            Exit Sub
          End If
          '
          '
          '
          'Merkmale übernehmen
          '
          '
          '
          '
          CmdGen.CommandText = "SELECT * FROM TBL_MERK IN '" & Cnsys.DataSource & "'"
          TblGen.Constraints.Clear()
          TblGen.Rows.Clear()
          TblGen.Columns.Clear()
          If Not FillDatset(AdaptGen, TblGen) Then
            Exit Sub
          End If
          '
          For Each RowGen In TblGen.Rows
            CmdCol.CommandText = "UPDATE TBL_MERK SET [MERK_TYP]='" & RowGen("MERK_TYP") _
           & "',[MERK_FORM]='" & RowGen("MERK_FORM") & "',[MERK_FAKT]=" & SQLpunkt(RowGen("MERK_FAKT")) _
           & " ,[MERK_KBEZ]='" & AddHkom(TexKt(10000 + RowGen("MERK_ID")), 15) & "',[MERK_LBEZ]='" & AddHkom(Texxt(10000 + RowGen("MERK_ID")), 255) & "'" _
           & " WHERE MERK_ID=" & RowGen("MERK_ID")
            If SQLNonQuery(CmdCol, Cncol) <> 0 Then
              Exit Sub
            End If
          Next
          '
          '
          '
          '
          'Kopiere Anweisungen nach COLORWIN
          '
          '
          '
          SqlStmt = "ANWSG_ID,AUFG_ID,ANWSG_KBEZ,ANWSG_lBEZ"
          CmdCol.CommandText = "INSERT INTO TBL_ANWSG (" & SqlStmt & ") SELECT " & SqlStmt & " FROM TBL_ANWSG IN '" & Cnsys.DataSource & "' WHERE ANWSG_ID NOT IN " _
          & "(SELECT ANWSG_ID FROM TBL_ANWSG)"
          If SQLNonQuery(CmdCol, Cncol) <> 0 Then
            Exit Sub
          End If
          '

          '
          'Texte Anweisungen übernehmen
          '
          '
          '
          '
          CmdGen.CommandText = "SELECT * FROM TBL_ANWSG IN '" & Cnsys.DataSource & "'"
          TblGen.Constraints.Clear()
          TblGen.Rows.Clear()
          TblGen.Columns.Clear()
          If Not FillDatset(AdaptGen, TblGen) Then
            Exit Sub
          End If
          For Each RowGen In TblGen.Rows
            CmdCol.CommandText = "UPDATE TBL_ANWSG SET [AUFG_ID]=" & RowGen("AUFG_ID") _
           & " ,[ANWSG_KBEZ]='" & AddHkomE(TexKt(30000 + RowGen("ANWSG_ID"))) & "',[ANWSG_LBEZ]='" & AddHkomE(Texxt(30000 + RowGen("ANWSG_ID"))) & "'" _
           & " WHERE ANWSG_ID=" & RowGen("ANWSG_ID")
            If SQLNonQuery(CmdCol, Cncol) <> 0 Then
              Exit Sub
            End If
          Next
          '
          '
          'Kopiere Merkmale für Anweisungen nach COLORWIN
          '
          '
          '
          CmdGen.CommandText = "SELECT * FROM TBL_ANWSG_MERK"
          TblGen.Constraints.Clear()
          TblGen.Rows.Clear()
          ViewGen.RowFilter = ""
          TblGen.Columns.Clear()
          If Not FillDatset(AdaptGen, TblGen) Then
            Exit Sub
          End If
          CmdAllg.CommandText = "SELECT * FROM TBL_ANWSG_MERK IN '" & Cnsys.DataSource & "'"
          ReadAllg = DataRead(CmdAllg, CommandBehavior.Default, Cncol)
          Do While ReadAllg.Read
            ViewGen.RowFilter = "ANWSG_ID=" & ReadAllg("ANWSG_ID") & " AND MERK_ID=" & ReadAllg("MERK_ID")
            If ViewGen.Count = 0 Then
              CmdCol.CommandText = "INSERT INTO TBL_ANWSG_MERK (ANWSG_ID,MERK_ID) " _
              & " VALUES(" & ReadAllg("ANWSG_ID") & "," & ReadAllg("MERK_ID") & ")"
              If SQLNonQuery(CmdCol, Cncol) <> 0 Then
                Exit Sub
              End If
            End If
          Loop
          ReadAllg.Close()
          '
          '
          'Kopiere Anweisungen für Methoden nach COLORWIN
          '
          '
          '
          CmdGen.CommandText = "SELECT * FROM TBL_METH_ANWSG"
          TblGen.Constraints.Clear()
          TblGen.Rows.Clear()
          TblGen.Columns.Clear()
          ViewGen.RowFilter = ""
          If Not FillDatset(AdaptGen, TblGen) Then
            Exit Sub
          End If
          CmdAllg.CommandText = "SELECT * FROM TBL_METH_ANWSG IN '" & Cnsys.DataSource & "'"
          ReadAllg = DataRead(CmdAllg, CommandBehavior.Default, Cncol)
          Do While ReadAllg.Read
            ViewGen.RowFilter = "METH_ID=" & ReadAllg("METH_ID") & " AND ANWSG_ID=" & ReadAllg("ANWSG_ID")
            If ViewGen.Count = 0 Then
              CmdCol.CommandText = "INSERT INTO TBL_METH_ANWSG (METH_ID,ANWSG_ID) " _
              & " VALUES(" & ReadAllg("METH_ID") & "," & ReadAllg("ANWSG_ID") & ")"
              If SQLNonQuery(CmdCol, Cncol) <> 0 Then
                Exit Sub
              End If
            End If
          Loop
          ReadAllg.Close()
          '
          '
          'TBL_USER_METH_ANWSG ergänzen
          '
          CmdGen.CommandText = "SELECT * FROM TBL_USER_METH"
          TblUseMeth = New DataTable
          ViewUseMeth = New DataView(TblUseMeth)
          If Not FillDatset(AdaptGen, TblUseMeth) Then
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
          CmdGen.CommandText = "SELECT * FROM TBL_METH_ANWSG"
          TblMethAnwsg = New DataTable
          ViewMethAnwsg = New DataView(TblMethAnwsg)
          If Not FillDatset(AdaptGen, TblMethAnwsg) Then
            Exit Sub
          End If

          '
          '
          CmdGen.CommandText = "SELECT * FROM TBL_USER_METH_ANWSG"
          TblUseMethAnwsg = New DataTable
          ViewUseMethAnwsg = New DataView(TblUseMethAnwsg)
          If Not FillDatset(AdaptGen, TblUseMethAnwsg) Then
            Exit Sub
          End If
          '
          '
          '
          'Prüfen, ob Anweisung bei User vorhanden
          '
          '
          '
          '
          '
          For Each RowUseMeth In TblUseMeth.Rows
            ViewMethAnwsg.RowFilter = "METH_ID=" & RowUseMeth("METH_ID")
            For i = 0 To ViewMethAnwsg.Count - 1
              ViewUseMethAnwsg.RowFilter = "USER_ID=" & RowUseMeth("USER_ID") & " AND METH_ID=" & RowUseMeth("METH_ID") & " AND ANWSG_ID=" & ViewMethAnwsg(i)("ANWSG_ID")
              '
              'Prüfen, ob Anweisung bei User vorhanden
              '
              '
              '
              If ViewUseMethAnwsg.Count = 0 Then
                '
                'Anweisung fehlt
                '
                '
                'Anweisung hinzufügen
                'MsgBox("HIER  " & ViewMethAnwsg.Count & Space(2) & ViewUseMethAnwsg.Count & Space(2) & RowUse("USER_ID") & Space(2) & RowMeth("METH_ID"))
                CmdGen.CommandText = "INSERT INTO TBL_USER_METH_ANWSG (USER_ID,METH_ID,ANWSG_ID,ZEIL_NR) " _
                & " VALUES(" & RowUseMeth("USER_ID") & "," & RowUseMeth("METH_ID") & "," & ViewMethAnwsg(i)("ANWSG_ID") & "," & i + 1 & ")"
                If SQLNonQuery(CmdGen, Cncol) <> 0 Then
                  Exit Sub
                End If
              End If
            Next i
          Next
        End If
        '
        '
        '
        '
        '
        '
        'Tabelle TBL_PARAM für Parameter übernehmen
        '
        '
        '
        '
        CmdCol = New OleDbCommand("", Cncol)
        SqlStmt = "PARAM_ID,PARAM_KBEZ,PARAM_LBEZ,PARAM_FORM,PARAM_WERT,PARAM_BEZWRT"
        CmdCol.CommandText = "INSERT INTO TBL_PARAM (" & SqlStmt & ") SELECT " & SqlStmt & " FROM TBL_PARAM IN '" & Cnsys.DataSource & "' WHERE PARAM_ID NOT IN " _
        & "(SELECT PARAM_ID FROM TBL_PARAM)"
        If SQLNonQuery(CmdCol, Cncol) <> 0 Then
          Exit Sub
        End If '
        '
        '
        '
        '
        '
        '
        'Tabelle TBL_FSTAE übernehmen
        '
        '
        If StringLength("TBL_FSTAE", "FSTAE_BEZ", Cncol) < 100 Then
          CmdCol.CommandText = "ALTER TABLE TBL_FSTAE ALTER COLUMN [FSTAE_BEZ] TEXT(100)"
          If SQLNonQuery(CmdCol, Cncol) <> 0 Then
            Exit Sub
          End If
        End If
        '
        '
        CmdCol = New OleDbCommand("", Cncol)
        SqlStmt = "FSTAE_ID,FSTAE_BEZ"
        CmdCol.CommandText = "INSERT INTO TBL_FSTAE (" & SqlStmt & ") SELECT " & SqlStmt & " FROM TBL_FSTAE IN '" & Cnsys.DataSource & "' WHERE FSTAE_ID NOT IN " _
        & "(SELECT FSTAE_ID FROM TBL_FSTAE)"
        If SQLNonQuery(CmdCol, Cncol) <> 0 Then
          Exit Sub
        End If '
        '
        '
        'Texte aus Textdatei
        '
        '
        For i = 0 To 8
          SqlStmt = "UPDATE TBL_FSTAE SET FSTAE_BEZ='" & Texxt(450 + i) & " ' WHERE FSTAE_ID=" & i
          CmdCol.CommandText = SqlStmt
          If SQLNonQuery(CmdCol, Cncol) <> 0 Then
            Exit Sub
          End If
        Next
        '      
        '
        '
        'Parameter TBL_PARAM übernehmen
        '
        '
        CmdGen.CommandText = "SELECT * FROM TBL_PARAM"
        TblGen = New DataTable
        ViewGen = New DataView(TblGen)
        If Not FillDatset(AdaptGen, TblGen) Then
          Exit Sub
        End If
        For Each RowGen In TblGen.Rows
          CmdCol.CommandText = "UPDATE TBL_PARAM SET [PARAM_KBEZ]='" & TexKt(25000 + RowGen("PARAM_ID")) _
           & "',[PARAM_LBEZ]='" & Texxt(25000 + RowGen("PARAM_ID")) _
           & "',[PARAM_FORM]='" & RowGen("PARAM_FORM") & "',[PARAM_WERT]=" & SQLpunkt(RowGen("PARAM_WERT")) _
           & ",[PARAM_BEZWRT]='" & RowGen("PARAM_BEZWRT") & "' WHERE PARAM_ID=" & RowGen("PARAM_ID")
          If SQLNonQuery(CmdCol, Cncol) <> 0 Then
            Exit Sub
          End If
        Next '
        '
        '
        '
        '
        '
        '
        '
        'Parameter-Abhängigkeiten TBL_AUSW_PARAM übernehmen
        '
        '
        'LÖschen
        '
        CmdCol.CommandText = "DELETE * FROM TBL_AUSW_PARAM"
        If SQLNonQuery(CmdCol, Cncol) <> 0 Then
          Exit Sub
        End If '
        '
        'Einfügen
        '
        CmdCol = New OleDbCommand("", Cncol)
        SqlStmt = "AUSW_ID,PARAM_ID"
        CmdCol.CommandText = "INSERT INTO TBL_AUSW_PARAM (" & SqlStmt & ") SELECT " & SqlStmt & " FROM TBL_AUSW_PARAM IN '" & Cnsys.DataSource & "'"
        If SQLNonQuery(CmdCol, Cncol) <> 0 Then
          Exit Sub
        End If '
        '
        '
        '
        '
        '
        If StringLength("TBL_FARBM", "FARBM_NAME", Cndat) < 128 Then
          Cmdupd = New OleDbCommand
          Cmdupd.Connection = Cndat()
          Cmdupd.CommandText = "ALTER TABLE TBL_FARBM ALTER COLUMN [FARBM_NAME] TEXT(128)"
          If SQLExeNonQuery(Cmdupd, Cndat) <> 0 Then
            Exit Sub
          End If
          Cmdupd.Dispose()
        End If
        '
        '
        '
        '
        '
        '
        '
      Catch ex As Exception
        MsgBox(ex.Message)
      End Try
    End If
    Cncol.Close()
  End Sub

  Sub CopyDaText()
    Dim cmdcol As OleDbCommand
    Dim i As Integer
    Dim j As Integer
    Dim ier As Short
    Dim SqlStmt As String
    Dim Felder() As String
    '



    '
    If Cnsys.ConnectionString <> Cncol.ConnectionString Then
      Try
        cmdcol = New OleDbCommand("", Cncol)
        For i = 1 To TabLangName.Count - 1
          Felder = {"TEXT_ID", "TEXT_KBEZ", "TEXT_LBEZ"}
          '
          'Texte übernehmen (außer TBL_TEXTE_USE)
          '
          'falls TOOLTIPS erwünscht 
          ' If TabLangName(i) = "TBL_TEXTE_TOOLTIP" Then
          ' Felder = {"FORM", "CONTROL", "TEXT_TEXT"}
          ' If Not TableExists(TabLangName(i), Cncol) Then
          ' Call CreateTable(TabLangName(i), {"FORM", "CONTROL", "TEXT_TEXT"}, {"TEXT(32)", "TEXT(32)", "MEMO"}, ier, Cncol)
          ' Call CreatePrKey("TEXT_ID", TabLangName(i), {"FORM", "CONTROL"}, ier, Cncol)
          ' End If
          ' End If
          '
          'LÖschen
          '
          cmdcol.CommandText = "DELETE * FROM " & TabLangName(i) & " WHERE TEXT_ID IN (SELECT TEXT_ID FROM " & TabLangName(i) & " IN '" & Cnsys.DataSource & "')"
          If SQLNonQuery(cmdcol, Cncol) <> 0 Then
            Exit Sub
          End If '
          '
          'Einfügen
          '
          cmdcol = New OleDbCommand("", Cncol)
          SqlStmt = ""
          For j = 0 To Felder.Count - 1
            SqlStmt = SqlStmt & Felder(j)
            If j <> Felder.Count - 1 Then
              SqlStmt = SqlStmt & ","
            End If
          Next j
          cmdcol.CommandText = "INSERT INTO " & TabLangName(i) & "(" & SqlStmt & ") SELECT " & SqlStmt & " FROM " & TabLangName(i) & " IN '" & Cnsys.DataSource & "'" 
          If SQLNonQuery(cmdcol, Cncol) <> 0 Then
            Exit Sub
          End If '
          '
        Next
      Catch ex As Exception

      End Try
    End If
  End Sub




  Function IchfArt(ByVal CHF As String) As Integer
    If CHF = "F" Then
      IchfArt = 0
    ElseIf CHF = "B" Then
      IchfArt = 1
    ElseIf CHF = "W" Then
      IchfArt = 2
    ElseIf CHF = "S" Then
      IchfArt = 3
    ElseIf CHF = "M" Then
      IchfArt = 4
    ElseIf CHF = "E" Then
      IchfArt = 5
    ElseIf CHF = "Z" Then
      IchfArt = 6
    ElseIf CHF = "R" Then
      IchfArt = 7
    End If

  End Function








  Sub umstneu()
    REM Call newrwert
    'Call Newcolth
    REM Call neweich
  End Sub

  Sub umstold()
    REM Call oldrwert
    REM Call oldrwunt
    REM Call oldcolth
    REM Call oldeich
    REM Call oldfarbe
    'Call oldkorr
  End Sub

  Function FoName(ByVal i As Integer) As String
    Dim j As Integer
    Select Case i

      Case 0
        FoName = "MS SANS SERIF"
        FoName = GetPrivSettings("FONTS", "FONTGENL", FoName, COLORFileName())
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
    On Error Resume Next
    'For j = 0 To Screen.FontCount - 1
    ' If FoName = Screen.Fonts(j) Then
    ' Exit Function
    ' End If
    ' Next j
    ' If Screen.FontCount > 0 Then
    ' FoName = Screen.Fonts(0)
    ' End If
    On Error GoTo 0
  End Function


  Public Function strUnQuoteString(ByVal strQuotedString As String)
    '
    ' Diese Routine überprüft, ob strQuotedString
    ' Anführungszeichen enthält, und entfernt diese,
    ' wenn dem so ist.
    '
    strQuotedString = Trim$(strQuotedString)

    If Mid$(strQuotedString, 1, 1) = """" Then
      If Right$(strQuotedString, 1) = """" Then
        '
        ' Sie steht in Anführungszeichen. Diese entfernen.
        '
        strQuotedString = Mid$(strQuotedString, 2, Len(strQuotedString) - 2)
      End If
    End If
    strUnQuoteString = strQuotedString
  End Function

  '
  Function GetDirectory(ByVal StrFile As String) As String
    Dim Splitvek() As String
    Dim i As Integer
    Splitvek = StrFile.Split("\")
    GetDirectory = ""
    For i = 0 To Splitvek.Count - 2
      GetDirectory = GetDirectory & Splitvek(i)
      If i < Splitvek.Count - 2 Then
        GetDirectory = GetDirectory & "\"
      End If
    Next i
  End Function

  Function GetFilename(ByVal StrFile As String) As String
    Dim Splitvek() As String
    Splitvek = StrFile.Split("\")
    GetFilename = Splitvek(Splitvek.Count - 1)
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
      i = CInt(Log10(wert))
      ijv = 1
      ijn = 5
      ijn = ijn - i
      ijv = ijv + i
      Forr = Sline(ijv, "#") & "." & Sline(ijn, "0")
      AutFormat = Format(wert, Forr)
    End If
  End Function



  '
  Function strfak10(ByVal wert As Single) As String
    Dim ivtw As Integer
    Dim i As Integer
    Dim vtt As Single
    ivtw = 36
    Dim vtw() As Single
    ReDim vtw(ivtw)
    vtw(1) = 0.0#
    vtw(2) = 0.00001
    vtw(3) = 0.00002
    vtw(4) = 0.00005
    vtw(5) = 0.0001
    vtw(6) = 0.0002
    vtw(7) = 0.0005
    vtw(8) = 0.001
    vtw(9) = 0.002
    vtw(10) = 0.005
    vtw(11) = 0.01
    vtw(12) = 0.02
    vtw(13) = 0.05
    vtw(14) = 0.1
    vtw(15) = 0.2
    vtw(16) = 0.5
    vtw(17) = 0.7
    vtw(18) = 0.8
    vtw(19) = 0.9
    vtw(20) = 0.95
    vtw(21) = 1.0#
    vtw(22) = 2.0#
    vtw(23) = 5.0#
    vtw(24) = 10.0#
    vtw(25) = 20.0#
    vtw(26) = 50.0#
    vtw(27) = 100.0#
    vtw(28) = 200.0#
    vtw(29) = 500.0#
    vtw(30) = 1000.0#
    vtw(31) = 2000.0#
    vtw(32) = 5000.0#
    vtw(33) = 10000.0#
    vtw(34) = 100000.0#
    vtw(35) = 1000000.0#
    vtw(36) = 10000000.0#

    vtt = wert
    If vtt > vtw(ivtw) Then
      vtt = vtw(1)
    Else
      For i = 1 To ivtw
        If vtt <= vtw(i) Then
          If i = ivtw Then
            vtt = vtw(1)
          Else
            vtt = vtw(i + 1)
          End If
          Exit For
        End If
      Next i
    End If
    strfak10 = Format(vtt, "####0.00######;;00000.00")
  End Function






  '

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




  Function TestTabName(ByVal Text) As Boolean
    Dim i As Integer
    Dim Jasc As Integer
    'MsgBox Asc("9")
    TestTabName = True
    For i = 1 To Len(Text)
      Jasc = Asc(UCase(Mid(Text, i, 1)))
      If Not (Jasc > 64 And Jasc < 91) And Not (Jasc > 47 And Jasc < 58) Then
        TestTabName = False
        Exit Function
      End If
    Next i
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






  '
  '-----------------------------------------------------------
  ' FUNKTION: FileExists
  ' Stellt fest, ob die angegeben Datei existiert.
  '
  ' Eingabe: [strPathName] - Zu überprüfende Datei
  '
  ' Rückgabe: "True", falls Datei existiert, andernfalls "False".
  '-----------------------------------------------------------
  '

  '-----------------------------------------------------------
  ' FUNKTION: DirExists
  '
  ' Stellt fest, ob der angegebene Verzeichnisname existiert.
  ' Mit dieser Funktion kann z. B. festgestellt werden, ob
  ' sich die Installationsdiskette im Laufwerk befindet, indem
  ' das Laufwerk mit einem Befehl wie 'A:\' aufgerufen wird.
  '
  ' Eingabe: [strDirName] - Name des zu prüfenden Verzeichnisses
  '
  ' Rückgabe: "True", falls Verzeichnis existiert, sonst "False".
  '-----------------------------------------------------------
  '
  Public Function DirExists(ByVal strDirName As String) As Boolean
    On Error Resume Next

    DirExists = (GetAttr(strDirName) And vbDirectory) = vbDirectory

    Err.Clear()
  End Function


  Function StringWhere(Tabname As String, datField As String, DatSearchFields() As String, DatValue() As Integer, cnDat As OleDbConnection) As String
    Dim DatReader As OleDbDataReader
    Dim SqlWhere As String
    Dim CmdCommand As New OleDbCommand("", cnDat)
    If DatSearchFields.Count = 0 Then
      StringWhere = "(-999)"
    Else
      SqlWhere = DatSearchFields(0) & "=" & DatValue(0)
      For i = 1 To DatSearchFields.Count - 1
        SqlWhere = SqlWhere & " AND " & DatSearchFields(i) & "=" & DatValue(i)
      Next
      CmdCommand.CommandText = "SELECT " & datField & " FROM " & Tabname & " WHERE " & SqlWhere
      DatReader = DataRead(CmdCommand, CommandBehavior.CloseConnection, cnDat)

      StringWhere = "("
      Do While DatReader.Read
        StringWhere = StringWhere & DatReader(datField) & ","
      Loop
      If StringWhere = "(" Then
        StringWhere = "(-999)"
      Else
        StringWhere = StringWhere.Substring(0, StringWhere.Length - 1) & ")"
      End If
      DatReader.Close()
    End If
  End Function
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
End Module
