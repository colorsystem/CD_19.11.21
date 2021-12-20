Option Compare Text
Option Strict Off
Option Explicit On
Imports GeneralLibrary.Auxiliary
Public Class DataBasisAndTables
  Private Shared DrAllg As OleDbDataReader
  Private Shared CmdAllg As OleDbCommand
  Shared Function CreateDatabase(ByVal cncole As OleDbConnection) As Boolean
    Try
      Dim Cat As Catalog = New Catalog
      Cat.Create(cncole.ConnectionString)
      Cat = Nothing
      CreateDatabase = True
      Cat = Nothing
    Catch
      CreateDatabase = False
    End Try
  End Function
  Shared Sub GetTabledefs(ByRef DBTables As List(Of String), ByVal cncole As OleDbConnection)
    Dim i As Integer
    Dim cnn As ADODB.Connection
    Dim cat As ADOX.Catalog
    cnn = New ADODB.Connection
    cat = New ADOX.Catalog
    cnn.Open(cncole.ConnectionString)
    cat.ActiveConnection = cnn
    DBTables.Clear()
    For i = 0 To cat.Tables.Count - 1
      DBTables.Add(cat.Tables(i).Name)
    Next i
    cnn.Close()
  End Sub
  Shared Function NewConnectionstring(ByVal ConnectionString As String, ByVal Datasource As String) As String
    Dim InFirst As Integer
    Dim InLast As Integer
    InFirst = InStr(ConnectionString, "DATA SOURCE=") + 11
    InLast = InFirst + InStr(ConnectionString.Substring(InFirst), ";") - 1
    NewConnectionstring = ConnectionString.Substring(0, InFirst) & Datasource & ConnectionString.Substring(InLast)
  End Function
  Shared Function StringLength(ByRef TABLENAME As String, ByRef Fieldname As String, ByRef CnDBASE As OleDbConnection)
    Dim i As Integer
    Dim Connbehav As CommandBehavior
    Connbehav = CommandBehavior.KeyInfo Or CommandBehavior.SchemaOnly
    If CnDBASE.State = 0 Then
      Connbehav = Connbehav Or CommandBehavior.CloseConnection
    End If
    StringLength = -1
    Dim Commandsys As New OleDbCommand
    Dim Dyset As OleDbDataReader
    Commandsys.CommandText = "SELECT * FROM " & TABLENAME
    Dyset = DataRead(Commandsys, Connbehav, CnDBASE)
    i = Dyset.GetOrdinal(Fieldname)
    Dyset.GetFieldType(i)
    If Dyset.GetFieldType(i) Is GetType(String) Then
      StringLength = Dyset.GetSchemaTable.Rows(i)(2)
    End If
    Dyset.Close()
    Commandsys.Dispose()
  End Function

  Overloads Shared Function StrLin(ByRef DatTable As DataTable, ByRef FieldName As String) As String
    Dim i As Integer
    StrLin = "("
    For i = 0 To DatTable.Rows.Count - 1
      If IsNumeric(DatTable.Rows(i)(FieldName)) Then
        StrLin = StrLin & DatTable.Rows(i)(FieldName) & ","
      End If
    Next i
    StrLin = StrLin.Substring(0, StrLin.Count - 1) & ")"
    If StrLin = ")" Then
      StrLin = "(-999)"
    End If
  End Function
  Overloads Shared Function StrLin(ByRef ViewData As DataView, ByRef FieldName As String) As String
    Dim i As Integer
    StrLin = "("
    For i = 0 To ViewData.Count - 1
      If IsNumeric(ViewData(i)(FieldName)) Then
        StrLin = StrLin & ViewData(i)(FieldName) & ","
      End If
    Next i
    StrLin = StrLin.Substring(0, StrLin.Count - 1) & ")"
    If StrLin = ")" Then
      StrLin = "(-999)"
    End If
  End Function
  Overloads Shared Function StrLin(ByRef ID As List(Of Integer)) As String
    Dim i As Integer
    StrLin = "("
    For i = 0 To ID.Count - 1
      StrLin = StrLin & ID(i) & ","
    Next i
    StrLin = StrLin.Substring(0, StrLin.Count - 1) & ")"
    If StrLin = ")" Then
      StrLin = "(-999)"
    End If
  End Function

  Shared Function GetFileDir(ByVal OldfileName As String) As String
    Dim i As Integer
    GetFileDir = ""
    For i = Len(OldfileName) To 1 Step -1
      If Mid(OldfileName, i, 1) = "\" Then
        GetFileDir = Mid(OldfileName, 1, i - 1)
        Exit For
      End If
    Next i
  End Function
  Shared Function NewFileName(ByVal OldFileName As String, ByVal Newfile As String) As String
    NewFileName = GetFileDir(OldFileName) & "\" & Newfile
  End Function
  Overloads Shared Function FillDatset(ByRef OleDbAdapt As OleDbDataAdapter, ByRef DaSet As DataSet, ByRef TabNam As String) As Boolean
    Dim MustClose As Boolean
    MustClose = False
    Dim i As Integer
    FillDatset = False
    OleDbAdapt.MissingSchemaAction = MissingSchemaAction.AddWithKey
    Try
      If Not IsNothing(DaSet.Tables(TabNam)) Then
        DaSet.Tables(TabNam).Clear()
      End If
      If OleDbAdapt.SelectCommand.Connection.State = 0 Then
        MustClose = True
        OleDbAdapt.SelectCommand.Connection.Open()
      End If
      OleDbAdapt.Fill(DaSet, TabNam)
      FillDatset = True
    Catch ex As OleDbException
      Call DbExcept(OleDbAdapt, ex)
    Catch ex As Exception
      Call Except(OleDbAdapt, ex)
    End Try
    If MustClose Then
      OleDbAdapt.SelectCommand.Connection.Close()
    End If
    '
    'MaxLength wird um 10000000 erhöht um nicht abfangbare Fehler zu vermeiden; vor dem Abspeichern in die Datenbank muss mi
    'Table.Columns(i).MaxLength mod 10000000 die korrekte Länge berechnet werden und eventuelle Längenüberschreitungen müssen korrigiert werden
    '
    For i = 0 To DaSet.Tables(TabNam).Columns.Count - 1
      If DaSet.Tables(TabNam).Columns(i).MaxLength > 0 Then
        DaSet.Tables(TabNam).Columns(i).MaxLength = 10000000 + DaSet.Tables(TabNam).Columns(i).MaxLength
      End If
    Next
  End Function
  Overloads Shared Function FillDatset(ByRef OleDbAdapt As OleDbDataAdapter, ByRef Table As DataTable) As Boolean
    Dim Mustclose As Boolean
    FillDatset = False
    Mustclose = False
    OleDbAdapt.MissingSchemaAction = MissingSchemaAction.AddWithKey
    Try
      If OleDbAdapt.SelectCommand.Connection.State = 0 Then
        Mustclose = True
        If Not ConnOpen(OleDbAdapt.SelectCommand.Connection) Then
          Exit Function
        End If
      End If
      OleDbAdapt.Fill(Table)

      FillDatset = True
    Catch ex As OleDbException
      Call DbExcept(OleDbAdapt, ex)
    Catch ex As Exception
      Call Except(OleDbAdapt, ex)
    End Try
    If Mustclose Then
      OleDbAdapt.SelectCommand.Connection.Close()
    End If
    ''
  End Function

  Overloads Shared Function FillDatSchema(ByRef OleDbAdapt As OleDbDataAdapter, ByRef DaSet As DataSet, ByRef TabNam As String) As Boolean
    Dim MustClose As Boolean
    MustClose = False
    FillDatSchema = False
    Try
      If Not IsNothing(DaSet.Tables(TabNam)) Then
        DaSet.Tables(TabNam).Clear()
      End If
      If OleDbAdapt.SelectCommand.Connection.State = 0 Then
        MustClose = True
        If Not ConnOpen(OleDbAdapt.SelectCommand.Connection) Then
          Exit Function
          'OleDbAdapt.SelectCommand.Connection.Open()
        End If
      End If
      OleDbAdapt.FillSchema(DaSet, SchemaType.Source, TabNam)
      FillDatSchema = True
    Catch ex As OleDbException
      Call DbExcept(OleDbAdapt, ex)
    Catch ex As Exception
      Call Except(OleDbAdapt, ex)
    End Try
    If MustClose Then
      OleDbAdapt.SelectCommand.Connection.Close()
    End If
  End Function
  Overloads Shared Function FillDatSchema(ByRef OleDbAdapt As OleDbDataAdapter, ByRef Table As DataTable) As Boolean
    Dim MustClose As Boolean
    MustClose = False
    FillDatSchema = False
    Try
      If OleDbAdapt.SelectCommand.Connection.State = 0 Then
        MustClose = True
        If Not ConnOpen(OleDbAdapt.SelectCommand.Connection) Then
          Exit Function
          'OleDbAdapt.SelectCommand.Connection.Open()
        End If
      End If
      OleDbAdapt.FillSchema(Table, SchemaType.Source)
      FillDatSchema = True
    Catch ex As OleDbException
      Call DbExcept(OleDbAdapt, ex)
    Catch ex As Exception
      Call Except(OleDbAdapt, ex)
    End Try
    If MustClose Then
      OleDbAdapt.SelectCommand.Connection.Close()
    End If
  End Function
  Private Shared Sub DbExcept(ByVal oledbadapt As OleDbDataAdapter, ByVal ex As OleDbException)
    MsgBox("OleDbErrorDescription: " & ex.ToString)
    MsgBox("Command String: " & oledbadapt.SelectCommand.CommandText)
    MsgBox("OleDbErrorCode: " & CStr(ex.ErrorCode))
    MsgBox("OleDbErrorSource: " & ex.Source)
    MsgBox("OleDbErrorMessage: " & ex.Message)
    MsgBox("OleDbErrorNative: " & CStr(ex.ErrorCode))
  End Sub
  Private Shared Sub Except(ByVal oledbadapt As OleDbDataAdapter, ByVal ex As Exception)
    MsgBox("ErrorDescription: " & ex.ToString)
    MsgBox("Command String: " & oledbadapt.SelectCommand.CommandText)
    MsgBox("ErrorSource: " & ex.Source)
    MsgBox("ErrorMessage: " & ex.Message)
  End Sub
 
  Shared Sub CreatePrKey(ByRef KeyName As String, ByRef TabName As String, ByRef TabFields() As String, ByRef ier As Integer, ByRef Conn As OleDbConnection)
    Dim i As Short
    Dim SqlStmt As String
    Dim TabKey As String
    CmdAllg = New OleDbCommand
    ier = 0
    TabKey = TabFields(0)
    For i = 1 To UBound(TabFields)
      TabKey = TabKey & "," & TabFields(i)
    Next
    SqlStmt = "ALTER TABLE " & TabName & " ADD CONSTRAINT " & KeyName & " PRIMARY KEY (" & TabKey & ")"
    CmdAllg.CommandText = SqlStmt
    If SQLNonQuery(CmdAllg, Conn) <> 0 Then
      ier = -1
    End If
    CmdAllg.Dispose()
  End Sub
  Shared Sub CreateIndex(ByRef IndexName As String, ByRef TabName As String, ByRef TabFields() As String, ByRef ier As Integer, ByVal Uniqu As Boolean, ByRef Conn As OleDbConnection)
    Dim i As Short
    Dim SqlStmt As String
    Dim TabKey As String
    CmdAllg = New OleDbCommand
    ier = 0
    TabKey = TabFields(0)
    For i = 1 To UBound(TabFields)
      TabKey = TabKey & "," & TabFields(i)
    Next
    If Uniqu Then
      SqlStmt = "CREATE UNIQUE INDEX "
    Else
      SqlStmt = "CREATE INDEX "
    End If
    SqlStmt = SqlStmt & IndexName & " ON " & TabName & " (" & TabKey & ")"
    CmdAllg.CommandText = SqlStmt
    If SQLNonQuery(CmdAllg, Conn) <> 0 Then
      ier = -1
    End If
    CmdAllg.Dispose()

  End Sub
  Shared Sub CreateRelat(ByRef RelName As String, ByRef TabName As String, ByRef ForName As String, ByRef TabFields() As String, ByRef ForFields() As String, ByRef OnUpdate As String, ByRef OnDelete As String, ByRef ier As Integer, ByRef Conn As OleDbConnection)
    Dim i As Short
    Dim SqlStmt As String
    Dim ForKey As String
    Dim TabKey As String
    '
    'Tabname hat eindeutigen Schlüssel (oder Primärschlüssel)
    '
    '
    CmdAllg = New OleDbCommand
    ier = 0
    ForKey = ForFields(0)
    For i = 1 To UBound(ForFields)
      ForKey = ForKey & "," & ForFields(i)
    Next
    TabKey = TabFields(0)
    For i = 1 To UBound(TabFields)
      TabKey = TabKey & "," & TabFields(i)
    Next
    '
    '
    '
    'ON UPDATE bzw. ON DELETE [ RESTRICT | CASCADE | SET NULL | NO ACTION ]
    '
    '
    '
    SqlStmt = "ALTER TABLE " & ForName _
    & " ADD CONSTRAINT " & RelName & " FOREIGN KEY (" & ForKey & ")" _
    & " REFERENCES " & TabName & "(" & TabKey & ") ON DELETE " & OnDelete & " ON UPDATE " & OnUpdate
    CmdAllg.CommandText = SqlStmt
    If SQLNonQuery(CmdAllg, Conn) <> 0 Then
      ier = -1
    End If
    CmdAllg.Dispose()

  End Sub
  Shared Sub CreateField(ByRef TabName As String, ByRef Field As String, ByRef FieldType As String, ByRef OptValue As String, ByRef ier As Integer, ByRef Conn As OleDbConnection)
    CmdAllg = New OleDbCommand
    ier = 0
    CmdAllg.CommandText = "ALTER TABLE " & TabName & " ADD COLUMN [" & Field & "] " & FieldType
    If SQLNonQuery(CmdAllg, Conn) <> 0 Then
      Exit Sub
    End If
    CmdAllg.CommandText = "UPDATE " & TabName & " SET [" & Field & "]=" & OptValue
    If SQLNonQuery(CmdAllg, Conn) <> 0 Then
      Exit Sub
    End If
    CmdAllg.Dispose()
  End Sub
  Shared Sub DeleteField(ByRef TabName As String, ByRef Field As String, ByRef ier As Integer, ByRef Conn As OleDbConnection)
    CmdAllg = New OleDbCommand
    ier = 0
    CmdAllg.CommandText = "ALTER TABLE " & TabName & " DROP COLUMN " & Field
    If SQLNonQuery(CmdAllg, Conn) <> 0 Then
      Exit Sub
    End If
    CmdAllg.Dispose()
  End Sub
  Shared Sub CreateTable(ByRef TabName As String, ByRef TabFields() As String, ByRef TabTypes() As String, ByRef ier As Integer, ByRef Conn As OleDbConnection)
    Dim i As Short
    Dim SqlStmt As String
    Dim TabKey As String
    '
    'Datatypes
    '
    'TEXT(5)
    'LONG
    'SHORT
    'SINGLE
    'DOUBLE
    'MEMO
    'OLEOBJECT
    '
    '
    CmdAllg = New OleDbCommand
    ier = 0
    TabKey = "[" & TabFields(0) & "]" & Space(1) & TabTypes(0)
    For i = 1 To UBound(TabFields)
      TabKey = TabKey & ",[" & TabFields(i) & "]" & Space(1) & TabTypes(i)
    Next

    SqlStmt = "CREATE TABLE " & TabName & " (" & TabKey & ")"
    CmdAllg.CommandText = SqlStmt
    If SQLNonQuery(CmdAllg, Conn) <> 0 Then
      ier = -1
      Exit Sub
    End If
    CmdAllg.Dispose()

  End Sub
  Shared Sub DeletePrKey(ByRef KeyName As String, ByRef TabName As String, ByRef ier As Integer, ByRef Conn As OleDbConnection)
    Dim SqlStmt As String
    CmdAllg = New OleDbCommand
    SqlStmt = "ALTER TABLE " & TabName & " DROP CONSTRAINT " & KeyName
    CmdAllg.CommandText = SqlStmt
    If SQLNonQuery(CmdAllg, Conn) <> 0 Then
      ier = -1
    End If
    CmdAllg.Dispose()
  End Sub
  Shared Sub DeleteIndex(ByRef IndexName As String, ByRef TabName As String, ByRef ier As Integer, ByRef Conn As OleDbConnection)
    Dim SqlStmt As String
    CmdAllg = New OleDbCommand
    ier = 0
    SqlStmt = "DROP INDEX " & IndexName & " ON " & TabName
    CmdAllg.CommandText = SqlStmt
    If SQLNonQuery(CmdAllg, Conn) <> 0 Then
      ier = -1
    End If
    CmdAllg.Dispose()
  End Sub
  Shared Sub DeleteRelat(ByRef RelName As String, ByRef TabName As String, ByRef ier As Integer, ByRef Conn As OleDbConnection)
    Dim SqlStmt As String
    CmdAllg = New OleDbCommand
    ier = 0
    SqlStmt = "ALTER TABLE " & TabName & " DROP CONSTRAINT " & RelName
    CmdAllg.CommandText = SqlStmt
    If SQLNonQuery(CmdAllg, Conn) <> 0 Then
      ier = -1
    End If
    CmdAllg.Dispose()
  End Sub

  Shared Sub DeleteTable(ByRef TabName As String, ByRef ier As Integer, ByRef Conn As OleDbConnection)
    Dim SqlStmt As String
    CmdAllg = New OleDbCommand
    ier = 0
    SqlStmt = "DROP TABLE " & TabName
    CmdAllg.CommandText = SqlStmt
    If SQLNonQuery(CmdAllg, Conn) <> 0 Then
      ier = -1
    End If
    CmdAllg.Dispose()
  End Sub
  Shared Function TableExists(ByVal TableName As String, ByRef Conn As OleDbConnection) As Boolean
    Dim i As Integer
    Dim MustClose As Boolean
    MustClose = False
    Dim SchemaTable As DataTable
    TableExists = False
    If Conn.State = 0 Then
      MustClose = True
      If Not ConnOpen(Conn) Then
        Exit Function
      End If
    End If
    'SchemaTable = Conn.GetOleDbSchemaTable(OleDbSchemaGuid.Tables, _
    '                                                New Object() {Nothing, Nothing, Nothing, "TABLE"})
    SchemaTable = Conn.GetOleDbSchemaTable(OleDbSchemaGuid.Tables, _
                                                New Object() {Nothing, Nothing, Nothing, Nothing})

    For i = 0 To SchemaTable.Rows.Count - 1
      If SchemaTable.Rows(i)(2) = TableName Then
        TableExists = True
        Exit For
      End If
    Next i
    If MustClose Then
      Conn.Close()
    End If
    SchemaTable.Dispose()
  End Function
  Shared Function FieldExists(ByVal TableName As String, ByVal ColumnName As String, ByRef Conn As OleDbConnection) As Boolean
    Dim i As Integer
    Dim SchemaTable As DataTable
    Dim Cmd As New OleDbCommand
    Dim dr As OleDbDataReader
    Dim ConnBehav As CommandBehavior
    ConnBehav = CommandBehavior.SchemaOnly
    If Conn.State = 0 Then
      ConnBehav = ConnBehav Or CommandBehavior.CloseConnection
    End If
    '

    FieldExists = False
    If Conn.State = 0 Then
      If Not ConnOpen(Conn) Then
        Exit Function
      End If
    End If
    'SchemaTable = Conn.GetOleDbSchemaTable(OleDbSchemaGuid.Tables, _
    '                                                New Object() {Nothing, Nothing, Nothing, "TABLE"})
    SchemaTable = Conn.GetOleDbSchemaTable(OleDbSchemaGuid.Tables, _
                                                New Object() {Nothing, Nothing, Nothing, Nothing})

    For i = 0 To SchemaTable.Rows.Count - 1
      If SchemaTable.Rows(i)(2) = TableName Then
        Exit For
      End If
    Next i
    If i >= SchemaTable.Rows.Count Then
      Exit Function
    End If
    Cmd.CommandText = "SELECT * FROM " & TableName
    dr = DataRead(Cmd, ConnBehav, Conn)
    For i = 0 To dr.FieldCount - 1
      If dr.GetName(i) = ColumnName Then
        FieldExists = True
        Exit For
      End If
    Next
    dr.Close()
    'SchemaTable.Dispose()
    'Cmd.Dispose()
  End Function
  Public Shared Function DataRead(ByRef cmd As OleDbCommand, ByRef Behavior As System.Data.CommandBehavior, ByRef AcConn As OleDbConnection) As OleDbDataReader
    Dim msg As String
    Dim i As Integer
    DataRead = Nothing
    If AcConn.State = 0 Then
      If Not ConnOpen(AcConn) Then
        Exit Function
      End If
    End If
    cmd.Connection = AcConn
    Try
      DataRead = cmd.ExecuteReader(Behavior)
    Catch ex As OleDbException
      msg = ""
      For i = 0 To ex.Errors.Count - 1
        msg = msg & "Message = oledberr.message & controlchars.crlf"
        msg = msg & "Source = oledberr.source & controlchars.crlf"
        msg = msg & "NativeError = oledberr.NativeError & controlchars.crlf"
        msg = msg & "SQLState = oledberr.SQLState & controlchars.crlf"
      Next i
      MessageBox.Show(cmd.CommandText)
      MessageBox.Show(ex.Message)
      MessageBox.Show(msg)
    Catch ex As Exception
      MsgBox("ERROR: " & cmd.CommandText)
      MessageBox.Show(ex.Message)
    End Try
  End Function

  Shared Function SQLNonQuery(ByVal cmd As OleDbCommand, ByRef acConn As OleDbConnection) As Integer
    Dim Rec As Integer
    Dim i As Integer
    Dim msg As String
    Dim MustClose As Boolean
    MustClose = False
    SQLNonQuery = 0
    If acConn.State = 0 Then
      MustClose = True
      If Not ConnOpen(acConn) Then
        SQLNonQuery = -3
        Exit Function
      End If
    End If
    cmd.Connection = acConn
    Try
      Rec = cmd.ExecuteNonQuery
    Catch ex As OleDbException
      msg = ""
      For i = 0 To ex.Errors.Count - 1
        msg = msg & "Message = oledberr.message & controlchars.crlf"
        msg = msg & "Source = oledberr.source & controlchars.crlf"
        msg = msg & "NativeError = oledberr.NativeError & controlchars.crlf"
        msg = msg & "SQLState = oledberr.SQLState & controlchars.crlf"
      Next i
      MessageBox.Show(cmd.CommandText)
      MessageBox.Show(ex.Message)
      MessageBox.Show(msg)
      SQLNonQuery = -2
    Catch ex As Exception
      MessageBox.Show("ERROR: " & cmd.CommandText)
      MessageBox.Show(ex.Message)
      SQLNonQuery = -1
    End Try
    If MustClose Then
      acConn.Close()
    End If
  End Function
  Public Shared Function ConnOpen(ByVal Conn As OleDbConnection) As Boolean
    Dim j As Integer
    Dim i As Integer
    Dim jmax As Integer = 10
    ConnOpen = True
    If Conn.State = 0 Then
      ConnOpen = False
      For j = 0 To jmax - 1
        Try
          Conn.Open()
          ConnOpen = True
        Catch myoledbexception As OleDbException
          If j = jmax - 1 Then
            For i = 0 To myoledbexception.Errors.Count - 1
              MessageBox.Show("ERROR" & ControlChars.Tab & i.ToString & _
              ControlChars.Cr & "Message" & ControlChars.Tab & _
              myoledbexception.Errors(i).Message & ControlChars.Cr & _
              "Native" & ControlChars.Tab & _
              myoledbexception.Errors(i).NativeError.ToString & _
              ControlChars.Cr & "Source" & ControlChars.Tab & _
              myoledbexception.Errors(i).Source & ControlChars.Cr & _
              "SQL" & ControlChars.Tab & _
              myoledbexception.Errors(i).SQLState & ControlChars.Cr & _
              "Oledb Exception")
            Next i
          End If
          Conn.Close()
        Catch ex As Exception
          If j = jmax - 1 Then
            MessageBox.Show(ex.ToString, "Other Exception")
          End If
          Conn.Close()
        Finally
        End Try
        If ConnOpen Then
          Exit For
        Else
          Call Wart(100)
        End If
      Next j
    End If
  End Function
  '
  '
  '
  Shared Function MaxDatTableID(OrgTable As DataTable, Feldname As String, ByVal BedingungFeldname() As String, ByVal BedingungID() As Integer)
    Dim i As Integer
    Dim MaxView As DataView
    Dim RowFilter As String
    RowFilter = ""
    For i = 0 To BedingungFeldname.Count - 1
      If BedingungFeldname(i) = "" Then
        Exit For
      End If
      RowFilter = RowFilter & BedingungFeldname(i) & "=" & BedingungID(i)
      If i < BedingungFeldname.Count - 1 Then
        RowFilter = RowFilter & " AND "
      End If
    Next i
    MaxView = New DataView(OrgTable, RowFilter, Feldname, DataViewRowState.CurrentRows)
    MaxDatTableID = MaxView(MaxView.Count - 1)(Feldname)
  End Function
  '
  '
  '
  '
  Shared Function MaxDBTableID(ByRef TabName As String, ByRef FeldName As String, ByVal BedingungFeldname() As String, ByVal BedingungID() As Integer, ByRef Conn As OleDbConnection) As Integer
    Dim i As Integer
    Dim SqlStmt As String
    Dim ConnBehav As CommandBehavior
    ConnBehav = CommandBehavior.SingleRow
    If Conn.State = 0 Then
      ConnBehav = ConnBehav Or CommandBehavior.CloseConnection
    End If
    '
    '
    CmdAllg = New OleDbCommand
    If BedingungFeldname.Count = 0 OrElse BedingungFeldname(0) = "" Then
      SqlStmt = "SELECT MAX([" & FeldName & "]) AS  [MAX_ID] FROM " & TabName
    Else
      SqlStmt = "SELECT MAX([" & FeldName & "]) AS  [MAX_ID] FROM " & TabName & " WHERE "
      For i = 0 To BedingungFeldname.Count - 1
        SqlStmt = SqlStmt & BedingungFeldname(i) & "=" & BedingungID(i)
        If i < BedingungFeldname.Count - 1 Then
          SqlStmt = SqlStmt & " AND "
        End If
      Next i
    End If
    CmdAllg.CommandText = SqlStmt
    DrAllg = DataRead(CmdAllg, ConnBehav, Conn)
    If DrAllg.Read AndAlso Not IsDBNull(DrAllg("max_id")) Then
      MaxDBTableID = DrAllg("max_id")
    Else
      MaxDBTableID = 0
    End If
    DrAllg.Close()
    CmdAllg.Dispose()
    Application.DoEvents()
  End Function
  Shared Sub RowAddDatabase(ByRef TABLENAME As String, ByRef Row As DataRow, ByVal CnDBASE As OleDbConnection)
    Dim Sqlstmt As String
    Dim sqlValues As String
    Dim NameLength As Integer
    Dim RowValue As String
    Dim AcConn As OleDbConnection
    AcConn = CnDBASE
    Dim CmdCommand As New OleDbCommand("", AcConn)
    Dim j As Integer
    '
    '
    'Der zu Row gehörige Datensatz wird zur Datenbanktabelle TABLENAME hinzugefügt
    '
    '
    '
    '
    CmdCommand.Parameters.Clear()
    Sqlstmt = "INSERT INTO " & TABLENAME & " ("
    sqlValues = ") VALUES("
    For j = 0 To Row.Table.Columns.Count - 1
      Sqlstmt = Sqlstmt & Row.Table.Columns(j).ColumnName
      sqlValues = sqlValues & "?"
      '
      'Prüfen, ob Feldlänge in Datenbank (Namelength=-1 ==> kein String)
      NameLength = Row.Table.Columns(j).MaxLength
      If NameLength = -1 AndAlso Row.Table.Columns(j).DataType Is GetType(String) Then
        NameLength = StringLength(TABLENAME, Row.Table.Columns(j).ColumnName, CnDBASE)
      End If
      If NameLength >= 0 Then
        RowValue = Row(j)
        If Len(RowValue) > NameLength Then
          RowValue = RowValue.Substring(0, NameLength)
        End If
        CmdCommand.Parameters.AddWithValue(Row.Table.Columns(j).ColumnName, RowValue)
      Else
        CmdCommand.Parameters.AddWithValue(Row.Table.Columns(j).ColumnName, Row(j))
      End If
      If j < Row.Table.Columns.Count - 1 Then
        Sqlstmt = Sqlstmt & ","
        sqlValues = sqlValues & ","
      End If
    Next
    sqlValues = sqlValues & ")"
    Sqlstmt = Sqlstmt & sqlValues
    CmdCommand.CommandText = Sqlstmt
    If SQLNonQuery(CmdCommand, AcConn) <> 0 Then
      Exit Sub
    End If
    CmdCommand.Dispose()
  End Sub


  Shared Sub RowReadDatabase(ByRef TABLENAME As String, ByRef Row As DataRow, ByVal WhereKeyID() As String, ByVal WhereID() As Integer, ByVal CnDBASE As OleDbConnection)
    Call TableReadDatabase(TABLENAME, Row.Table, WhereKeyID, WhereID, CnDBASE)
    If Row.Table.Rows.Count > 0 Then
      Row = Row.Table.Rows(0)
    End If
  End Sub
  Shared Sub TableReadDatabase(ByRef TABLENAME As String, ByRef Table As DataTable, ByVal WhereKeyID() As String, ByVal WhereID() As Integer, ByVal CnDBASE As OleDbConnection)
    Dim Sqlstmt As String
    Dim SqlWheres As String
    Dim AcConn As OleDbConnection
    Dim AdaptCommand As New OleDbDataAdapter
    AcConn = CnDBASE
    Dim CmdCommand As New OleDbCommand("", AcConn)
    Dim j As Integer
    Dim k As Integer
    Table.Clear()
    Table.Columns.Clear()
    '
    '
    'Select
    '
    Sqlstmt = "SELECT * FROM " & TABLENAME
    '
    'Where-Parameter
    '
    '
    SqlWheres = " WHERE "
    SqlWheres = SqlWheres & WhereKeyID(0) & "=?"
    For j = 1 To UBound(WhereKeyID)
      SqlWheres = SqlWheres & " AND " & WhereKeyID(j) & "=?"
    Next j

    Sqlstmt = Sqlstmt & SqlWheres
    CmdCommand.CommandText = Sqlstmt
    For k = 0 To UBound(WhereKeyID)
      CmdCommand.Parameters.AddWithValue(WhereKeyID(k), WhereID(k))
    Next
    AdaptCommand.SelectCommand = CmdCommand
    If Not FillDatset(AdaptCommand, Table) Then
      MsgBox("Error TableReadDatabase")
    End If

    CmdCommand.Dispose()
    AdaptCommand.Dispose()
  End Sub
  Shared Sub TableColumnsSchema(ByRef TABLENAME As String, ByRef Table As DataTable, ByVal CnDBASE As OleDbConnection)
    Dim Dyset As OleDbDataReader
    Dim AcConn As OleDbConnection
    Dim ConnBehav As CommandBehavior
    ConnBehav = CommandBehavior.KeyInfo Or CommandBehavior.SchemaOnly
    AcConn = CnDBASE
    If AcConn.State = 0 Then
      ConnBehav = ConnBehav Or CommandBehavior.CloseConnection
    End If
    Dim CmdCommand As New OleDbCommand("", AcConn)
    CmdCommand.CommandText = "SELECT * FROM " & TABLENAME
    Dyset = DataRead(CmdCommand, ConnBehav, CmdCommand.Connection)
    Table = Dyset.GetSchemaTable
    Dyset.Close()
    CmdCommand.Dispose()
  End Sub
  Shared Sub RowUpdateDatabase(ByRef TABLENAME As String, ByRef Row As DataRow, ByVal WhereKeyID() As String, ByVal CnDBASE As OleDbConnection)
    Dim Sqlstmt As String
    Dim SqlWheres As String
    Dim RowValue As String
    Dim NameLength As Integer
    Dim AcConn As OleDbConnection
    AcConn = CnDBASE
    Dim CmdCommand As New OleDbCommand("", AcConn)
    Dim j As Integer
    Dim k As Integer
    '
    '
    '
    '
    'Datensätze werden gemäß Row in der Datenbanktabelle TABLENAME für die Feldnamen WhereKeyID geändert
    '
    '
    '
    '     
    '     
    CmdCommand.Parameters.Clear()
    '
    'SET-Parameter
    '
    '
    Sqlstmt = "UPDATE " & TABLENAME & " SET "
    For j = 0 To Row.Table.Columns.Count - 1
      For k = 0 To UBound(WhereKeyID)
        If Row.Table.Columns(j).ColumnName = WhereKeyID(k) Then
          Exit For
        End If
      Next
      If k > UBound(WhereKeyID) Then
        Sqlstmt = Sqlstmt & Row.Table.Columns(j).ColumnName & "=?"
        '
        'Prüfen, ob Feldlänge in Datenbank (Namelength=-1 ==> kein String)
        '
        NameLength = Row.Table.Columns(j).MaxLength
        If NameLength = -1 AndAlso Row.Table.Columns(j).DataType Is GetType(String) Then
          NameLength = StringLength(TABLENAME, Row.Table.Columns(j).ColumnName, AcConn)
        End If
        If NameLength >= 0 Then
          RowValue = Row(j)
          If Len(RowValue) > NameLength Then
            RowValue = RowValue.Substring(0, NameLength)
          End If
          CmdCommand.Parameters.AddWithValue(Row.Table.Columns(j).ColumnName, RowValue)
        Else
          CmdCommand.Parameters.AddWithValue(Row.Table.Columns(j).ColumnName, Row(j))
        End If
        If j < Row.Table.Columns.Count - 1 Then
          Sqlstmt = Sqlstmt & ","
        End If
      End If
    Next
    '
    'Where-Parameter
    '
    '
    SqlWheres = " WHERE "
    SqlWheres = SqlWheres & WhereKeyID(0) & "=?"
    For j = 1 To UBound(WhereKeyID)
      SqlWheres = SqlWheres & " AND " & WhereKeyID(j) & "=?"
    Next j

    Sqlstmt = Sqlstmt & SqlWheres
    CmdCommand.CommandText = Sqlstmt
    For k = 0 To UBound(WhereKeyID)
      CmdCommand.Parameters.AddWithValue(WhereKeyID(k), Row(WhereKeyID(k)))
    Next
    If SQLNonQuery(CmdCommand, AcConn) <> 0 Then
      Exit Sub
    End If
    CmdCommand.Dispose()
  End Sub
  

  Shared Sub RowDeleteDatabase(ByRef TABLENAME As String, ByRef Row As DataRow, ByRef WhereKeyID() As String, ByVal CnDBASE As OleDbConnection)
    Dim Sqlstmt As String
    Dim SqlWheres As String
    Dim AcConn As OleDbConnection
    AcConn = CnDBASE
    Dim CmdCommand As New OleDbCommand("", AcConn)
    Dim j As Integer
    Dim k As Integer
    '
    '
    '
    '
    'Datensätze werden in der Datenbanktabelle TABLENAME für die Feldnamen WhereKeyID gelöscht
    '
    '
    '    '
    CmdCommand.Parameters.Clear()
    SqlWheres = " WHERE "
    SqlWheres = SqlWheres & WhereKeyID(0) & "=?"
    For j = 1 To UBound(WhereKeyID)
      SqlWheres = SqlWheres & " AND " & WhereKeyID(j) & "=?"
    Next j
    Sqlstmt = "DELETE * FROM " & TABLENAME
    CmdCommand.CommandText = Sqlstmt & SqlWheres
    For k = 0 To UBound(WhereKeyID)
      CmdCommand.Parameters.AddWithValue(WhereKeyID(k), Row(WhereKeyID(k)))
    Next
    If SQLNonQuery(CmdCommand, AcConn) <> 0 Then
      Exit Sub
    End If
    CmdCommand.Dispose()
  End Sub
  Shared Function RowExists(ByRef TABLENAME As String, ByRef Row As DataRow, ByRef WhereKeyID() As String, ByVal CnDBASE As OleDbConnection) As Boolean
    Dim Sqlstmt As String
    Dim SqlWheres As String
    Dim AcConn As OleDbConnection
    Dim DaRead As OleDbDataReader
    Dim ConnBehav As CommandBehavior
    ConnBehav = CommandBehavior.SingleRow
    AcConn = CnDBASE
    If AcConn.State = 0 Then
      ConnBehav = ConnBehav Or CommandBehavior.CloseConnection
    End If
    Dim CmdCommand As New OleDbCommand("", AcConn)
    Dim j As Integer
    Dim k As Integer
    '
    '
    '
    'Prüfen, ob Row existiert
    '
    '
    '    '
    RowExists = False
    CmdCommand.Parameters.Clear()
    SqlWheres = " WHERE "
    SqlWheres = SqlWheres & WhereKeyID(0) & "=?"
    For j = 1 To UBound(WhereKeyID)
      SqlWheres = SqlWheres & " AND " & WhereKeyID(j) & "=?"
    Next j
    Sqlstmt = "SELECT * FROM " & TABLENAME
    Sqlstmt = Sqlstmt & SqlWheres
    CmdCommand.CommandText = Sqlstmt
    For k = 0 To UBound(WhereKeyID)
      CmdCommand.Parameters.AddWithValue(WhereKeyID(k), Row(WhereKeyID(k)))
    Next
    '
    DaRead = DataRead(CmdCommand, ConnBehav, AcConn)
    If DaRead.Read Then
      RowExists = True
    End If
    DaRead.Close()
    CmdCommand.Dispose()
  End Function
  Shared Sub RowsAddDatabase(ByRef TABLENAME As String, ByRef DatRows() As DataRow, ByVal CnDBASE As OleDbConnection)
    Dim i As Integer
    For i = 0 To DatRows.Count - 1
      Call RowAddDatabase(TABLENAME, DatRows(i), CnDBASE)
    Next i
  End Sub
  Shared Sub RowsUpdateDatabase(ByRef TABLENAME As String, ByRef DatRows() As DataRow, ByVal WhereKeyID() As String, ByVal CnDBASE As OleDbConnection)
    Dim i As Integer
    For i = 0 To DatRows.Count - 1
      Call RowUpdateDatabase(TABLENAME, DatRows(i), WhereKeyID, CnDBASE)
    Next i
  End Sub
  Shared Sub RowsDeleteDatabase(ByRef TABLENAME As String, ByRef DatRows() As DataRow, ByRef WhereKeyID() As String, ByVal CnDBASE As OleDbConnection)
    Dim i As Integer
    For i = 0 To DatRows.Count - 1
      Call RowDeleteDatabase(TABLENAME, DatRows(i), WhereKeyID, CnDBASE)
    Next i
  End Sub
  Shared Sub RowsAddUpdateDatabase(ByRef TABLENAME As String, ByRef DatRows() As DataRow, ByRef WhereKeyID() As String, ByVal CnDBASE As OleDbConnection)
    Dim i As Integer
    For i = 0 To DatRows.Count - 1
      If RowExists(TABLENAME, DatRows(i), WhereKeyID, CnDBASE) Then
        Call RowUpdateDatabase(TABLENAME, DatRows(i), WhereKeyID, CnDBASE)
      Else
        Call RowAddDatabase(TABLENAME, DatRows(i), CnDBASE)
      End If
    Next i
  End Sub
  Shared Function OleDBInsertCmd(ByVal TABLENAME As String, ByVal CnDBASE As OleDbConnection) As OleDbCommand
    Dim j As Integer
    Dim sqlstmt As String
    Dim sqlValues As String
    Dim AcConn As OleDbConnection
    Dim dr As OleDbDataReader
    Dim ConnBehav As CommandBehavior
    ConnBehav = CommandBehavior.SchemaOnly
    AcConn = CnDBASE
    If AcConn.State = 0 Then
      ConnBehav = ConnBehav Or CommandBehavior.CloseConnection
    End If


    Dim OleDBCmd As New OleDbCommand("", AcConn)
    OleDBCmd.CommandText = "SELECT * FROM " & TABLENAME
    dr = DataRead(OleDBCmd, ConnBehav, AcConn)
    OleDBInsertCmd = New OleDbCommand("", AcConn)
    OleDBInsertCmd.Parameters.Clear()
    sqlstmt = "INSERT INTO " & TABLENAME & " ("
    sqlValues = ") VALUES("
    For j = 0 To dr.FieldCount - 1
      sqlstmt = sqlstmt & dr.GetName(j)
      sqlValues = sqlValues & "?"
      '
      ' NameLength = dr.
      ' If NameLength = -1 AndAlso Columns(j).DataType Is GetType(String) Then
      ' NameLength = StringLength(TABLENAME, Columns(j).ColumnName, AcConn)
      ' End If

      With OleDBInsertCmd.Parameters.Add(dr.GetName(j), GetOleDbType(dr.GetFieldType(j)))
        .SourceColumn = dr.GetName(j)
        .SourceVersion = DataRowVersion.Current
      End With
      If j < dr.FieldCount - 1 Then
        sqlstmt = sqlstmt & ","
        sqlValues = sqlValues & ","
      End If
    Next
    dr.Close()
    sqlValues = sqlValues & ")"
    sqlstmt = sqlstmt & sqlValues
    OleDBInsertCmd.CommandText = sqlstmt
  End Function
  Shared Function OleDBUpdateCmd(ByVal TABLENAME As String, ByVal WhereKeyID() As String, ByVal CnDBASE As OleDbConnection) As OleDbCommand
    Dim Sqlstmt As String
    Dim SqlWheres As String
    'Dim NameLength As Integer
    Dim j As Integer
    Dim k As Integer
    'Dim i As Integer
    Dim dr As OleDbDataReader
    Dim AcConn As OleDbConnection
    Dim ConnBehav As CommandBehavior
    ConnBehav = CommandBehavior.SchemaOnly
    AcConn = CnDBASE
    If AcConn.State = 0 Then
      ConnBehav = ConnBehav Or CommandBehavior.CloseConnection
    End If
    Dim OleDBCmd As New OleDbCommand("", AcConn)
    OleDBCmd.CommandText = "SELECT * FROM " & TABLENAME
    dr = DataRead(OleDBCmd, ConnBehav, AcConn)
    OleDBUpdateCmd = New OleDbCommand("", AcConn)
    OleDBUpdateCmd.Parameters.Clear()
    '
    '
    '
    '
    'SET-Parameter
    '
    '
    Sqlstmt = "UPDATE " & TABLENAME & " SET "
    For j = 0 To dr.FieldCount - 1
      If Not IsNothing(WhereKeyID) Then
        For k = 0 To UBound(WhereKeyID)
          If dr.GetName(j) = WhereKeyID(k) Then
            Exit For
          End If
        Next
      End If
      If IsNothing(WhereKeyID) OrElse k > UBound(WhereKeyID) Then
        'If k > UBound(WhereKeyID) Then

        Sqlstmt = Sqlstmt & dr.GetName(j) & "=?"
        '
        With OleDBUpdateCmd.Parameters.Add(dr.GetName(j), GetOleDbType(dr.GetFieldType(j)))
          .SourceColumn = dr.GetName(j)
          .SourceVersion = DataRowVersion.Current
        End With
        If j < dr.FieldCount - 1 Then
          Sqlstmt = Sqlstmt & ","
        End If
      End If
    Next
    dr.Close()
    '
    'Where-Parameter
    '
    '
    If IsNothing(WhereKeyID) Then
      SqlWheres = ""
    Else
      SqlWheres = " WHERE "
      SqlWheres = SqlWheres & WhereKeyID(0) & "=?"
      For j = 1 To UBound(WhereKeyID)
        SqlWheres = SqlWheres & " AND " & WhereKeyID(j) & "=?"
      Next j
    End If

    Sqlstmt = Sqlstmt & SqlWheres
    OleDBUpdateCmd.CommandText = Sqlstmt
    If Not IsNothing(WhereKeyID) Then
      For k = 0 To UBound(WhereKeyID)
        With OleDBUpdateCmd.Parameters.Add(WhereKeyID(k), OleDbType.Integer)
          .SourceColumn = WhereKeyID(k)
          .SourceVersion = DataRowVersion.Original
        End With
      Next
    End If
  End Function
  Shared Function OleDBDeleteCmd(ByVal TABLENAME As String, ByVal WhereKeyID() As String, ByVal CnDBASE As OleDbConnection) As OleDbCommand
    Dim Sqlstmt As String
    Dim SqlWheres As String
    Dim AcConn As OleDbConnection
    Dim j As Integer
    Dim k As Integer
    AcConn = CnDBASE
    OleDBDeleteCmd = New OleDbCommand("", AcConn)
    OleDBDeleteCmd.Parameters.Clear()
    '
    '
    '
    '
    'Datensätze werden in der Datenbanktabelle TABLENAME für die Feldnamen WhereKeyID gelöscht
    '
    '
    '    '
    OleDBDeleteCmd.Parameters.Clear()
    If IsNothing(WhereKeyID) Then
      SqlWheres = ""
    Else
      SqlWheres = " WHERE "
      SqlWheres = SqlWheres & WhereKeyID(0) & "=?"
      For j = 1 To UBound(WhereKeyID)
        SqlWheres = SqlWheres & " AND " & WhereKeyID(j) & "=?"
      Next j
    End If
    Sqlstmt = "DELETE * FROM " & TABLENAME
    OleDBDeleteCmd.CommandText = Sqlstmt & SqlWheres
    If Not IsNothing(WhereKeyID) Then
      For k = 0 To UBound(WhereKeyID)
        With OleDBDeleteCmd.Parameters.Add(WhereKeyID(k), OleDbType.Integer)
          .SourceColumn = WhereKeyID(k)
          .SourceVersion = DataRowVersion.Original
        End With
      Next
    End If
  End Function

  Shared Function DbTypeToOleDbType(ByVal DbType As DbType) As OleDbType
    Select Case DbType
      Case DbType.AnsiString
        Return OleDbType.VarChar
      Case DbType.AnsiStringFixedLength
        Return OleDbType.Char
      Case DbType.Binary
        Return OleDbType.Binary
      Case DbType.Boolean
        Return OleDbType.Boolean
      Case DbType.Byte
        Return OleDbType.UnsignedTinyInt
      Case DbType.Currency
        Return OleDbType.Currency
      Case DbType.Date
        Return OleDbType.Date
      Case DbType.DateTime
        Throw New NotImplementedException()
      Case DbType.Decimal
        Return OleDbType.Decimal
      Case DbType.Double
        Return OleDbType.Double
      Case DbType.Guid
        Return OleDbType.Guid
      Case DbType.Int16
        Return OleDbType.SmallInt
      Case DbType.Int32
        Return OleDbType.Integer
      Case DbType.Int64
        Return OleDbType.BigInt
      Case DbType.Object
        Return OleDbType.Variant
      Case DbType.SByte
        Return OleDbType.TinyInt
      Case DbType.Single
        Return OleDbType.Single
      Case DbType.String
        Return OleDbType.WChar
      Case DbType.StringFixedLength
        Return OleDbType.VarWChar
      Case DbType.Time
        Throw New NotImplementedException()
      Case DbType.UInt16
        Return OleDbType.UnsignedSmallInt
      Case DbType.UInt32
        Return OleDbType.UnsignedInt
      Case DbType.UInt64
        Return OleDbType.UnsignedBigInt
      Case DbType.VarNumeric
        Return OleDbType.VarNumeric
      Case Else
        Return OleDbType.Variant
    End Select


  End Function
  Shared Sub CheckRelation(ByVal TabParent As DataTable, ByVal FieldParent() As DataColumn, ByVal TabChild As DataTable, ByVal FieldChild() As DataColumn)
    Dim ViewParent As DataView
    Dim RowChild As DataRow
    Dim j As Integer
    Dim FieldString As String
    ViewParent = New DataView(TabParent)
    For Each RowChild In TabChild.Rows
      If RowChild.RowState = DataRowState.Deleted Then
        Continue For
      End If
      FieldString = ""
      For j = 0 To FieldChild.Count - 1
        FieldString = FieldString & FieldParent(j).ColumnName & "=" & RowChild(FieldChild(j).ColumnName)
        If j < FieldParent.Count - 1 Then
          FieldString = FieldString & " AND "
        End If
      Next j
      ViewParent.RowFilter = FieldString
      If ViewParent.Count = 0 Then
        RowChild.Delete()
      End If
    Next
    ViewParent.Dispose()
    TabChild.AcceptChanges()
  End Sub
  Shared Function GetOleDbType(ByVal value As Type) As OleDbType

    If value Is GetType(Boolean) Then
      Return OleDbType.Boolean
    ElseIf value Is GetType(Byte()) Then
      Return OleDbType.Binary
    ElseIf value Is GetType(Byte) Then
      Return OleDbType.UnsignedTinyInt
    ElseIf value Is GetType(Char) Then
      Return OleDbType.Char
    ElseIf value Is GetType(DateTime) Then
      Return OleDbType.Date
    ElseIf value Is GetType(DBNull) Then
      Return OleDbType.Empty
    ElseIf value Is GetType(Decimal) Then
      Return OleDbType.Decimal
    ElseIf value Is GetType(Double) Then
      Return OleDbType.Double
      'ElseIf value Is GetType(Empty) Then
      ' Return OleDbType.Empty
    ElseIf value Is GetType(Int16) Then
      Return OleDbType.SmallInt
    ElseIf value Is GetType(Int32) Then
      Return OleDbType.Integer
    ElseIf value Is GetType(Int64) Then
      Return OleDbType.BigInt
    ElseIf value Is GetType(SByte) Then
      Return OleDbType.TinyInt
    ElseIf value Is GetType(String) Then
      Return OleDbType.VarChar
    ElseIf value Is GetType(Single) Then
      Return OleDbType.Single
    ElseIf value Is GetType(UInt64) Then
      Return OleDbType.UnsignedBigInt
    ElseIf value Is GetType(UInt32) Then
      Return OleDbType.UnsignedInt
    ElseIf value Is GetType(UInt16) Then
      Return OleDbType.UnsignedSmallInt
    ElseIf value Is GetType(Object) Then
      Return OleDbType.Variant
    Else
      Return OleDbType.IUnknown
    End If
  End Function

End Class
