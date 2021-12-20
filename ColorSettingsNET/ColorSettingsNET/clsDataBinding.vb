Option Strict Off
Option Explicit On
Option Compare Text
Public Class clsDataBinding

  Inherits System.Windows.Forms.BindingSource
  Implements IDisposable
  Dim WithEvents MnBindingNavig As BindingNavigator
  Dim WithEvents MnDatatable As DataTable
  Dim CnDaten As OleDbConnection
  Dim Adapter As OleDbDataAdapter
  Dim SelectCommand As OleDbCommand
  '
  Sub New(ByVal BindindNavig As BindingNavigator, ByRef SelectString As String, ByRef NumParameter As Integer)
    Dim i As Integer
    MnBindingNavig = BindindNavig
    CnDaten = New OleDbConnection
    CnDaten.ConnectionString = "Provider=Microsoft.Jet.OLEDB.4.0;Password="""";User ID=Admin;Data Source=D:\Temp\COLHILF.MDB;Mode=Share Deny None;Extended Properties="""";Jet OLEDB:System database="""";Jet OLEDB:Registry Path="""";Jet OLEDB:Database Password="""";Jet OLEDB:Engine Type=5;Jet OLEDB:Database Locking Mode=0;Jet OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:Global Bulk Transactions=1;Jet OLEDB:New Database Password="""";Jet OLEDB:Create System Database=False;Jet OLEDB:Encrypt Database=False;Jet OLEDB:Don't Copy Locale on Compact=False;Jet OLEDB:Compact Without Replica Repair=False;Jet OLEDB:SFP=False"
    Adapter = New OleDbDataAdapter
    SelectCommand = New OleDbCommand("", CnDaten)
    Adapter.SelectCommand = SelectCommand
    MnDatatable = New DataTable
    SelectCommand.CommandText = SelectString
    SelectCommand.Parameters.Clear()
    For i = 0 To NumParameter - 1
      SelectCommand.Parameters.Add("@" & CStr(i), OleDbType.Integer)
    Next i

    Me.DataSource = MnDatatable
    MnBindingNavig.BindingSource = Me

  End Sub
  Sub CreateTable(ByRef WhereID() As Integer)
    Dim i As Integer
    For i = 0 To SelectCommand.Parameters.Count - 1
      SelectCommand.Parameters(i).Value = WhereID(i)
    Next
    MnDatatable.Clear()
    If Not FillDatset(Adapter, MnDatatable) Then
      Exit Sub
    End If
  End Sub
  Sub Update(ByRef TABLENAME As String, ByVal WhereKeyID() As String)
    Dim i As Integer
    Dim AllRows() As DataRow
    Dim UpdTable As New DataTable
    Dim UpdAdapter As New OleDbDataAdapter
    Dim ColumnName As String
    Dim UpdCommand As New OleDbCommand("SELECT * FROM " & TABLENAME, SelectCommand.Connection)
    UpdAdapter.SelectCommand = UpdCommand
    If Not FillDatSchema(UpdAdapter, UpdTable) Then
      Exit Sub
    End If
    For i = 0 To UpdTable.Columns.Count - 1
      ColumnName = UpdTable.Columns(i).ColumnName
      If Not MnDatatable.Columns.Contains(ColumnName) Then
        Exit For
      End If
    Next
    If i < UpdTable.Columns.Count Then
      MsgBox("Nicht alle Felder von " & TABLENAME & " sind in Tabelle enthalten")
      Exit Sub
    End If
    '

    'Datatable zum Beschreiben von TABLENAME aufbauen
    '
    '
    'Deleted Rows
    '
    UpdTable.Rows.Clear()
    AllRows = MnDatatable.Select(Nothing, Nothing, DataViewRowState.Deleted)
    For i = 0 To AllRows.Count - 1
      UpdTable.Rows.Add(UpdTable.NewRow)
      AllRows(i).RejectChanges()
      For j = 0 To UpdTable.Columns.Count - 1
        ColumnName = UpdTable.Columns(j).ColumnName

        UpdTable.Rows(i)(j) = AllRows(i)(ColumnName)
      Next j
      AllRows(i).Delete()
    Next i
    Call RowsDeleteDatabase(TABLENAME, UpdTable.Select, WhereKeyID, SelectCommand.Connection)
    '
    '
    'Modified Rows
    '
    UpdTable.Rows.Clear()
    AllRows = MnDatatable.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent)
    For i = 0 To AllRows.Count - 1
      UpdTable.Rows.Add(UpdTable.NewRow)
      For j = 0 To UpdTable.Columns.Count - 1
        ColumnName = UpdTable.Columns(j).ColumnName
        UpdTable.Rows(i)(j) = AllRows(i)(ColumnName)
      Next j
    Next i
    'Längenprüfung
    '
    Call TableColumnsSchema(TABLENAME, UpdTable, SelectCommand.Connection)

    Call RowsUpdateDatabase(TABLENAME, UpdTable.Select, WhereKeyID, SelectCommand.Connection)
    '
    '
    '
    'Added Rows
    '
    UpdTable.Rows.Clear()
    AllRows = MnDatatable.Select(Nothing, Nothing, DataViewRowState.Added)
    For i = 0 To AllRows.Count - 1
      UpdTable.Rows.Add(UpdTable.NewRow)
      For j = 0 To UpdTable.Columns.Count - 1
        ColumnName = UpdTable.Columns(j).ColumnName
        UpdTable.Rows(i)(j) = AllRows(i)(ColumnName)
      Next j
    Next i
    'Längenprüfung
    '
    Call TableColumnsSchema(TABLENAME, UpdTable, SelectCommand.Connection)

    Call RowsAddDatabase(TABLENAME, UpdTable.Select, SelectCommand.Connection)
    '
    MnDatatable.AcceptChanges()
    UpdTable.Dispose()
    UpdCommand.Dispose()
    UpdAdapter.Dispose()
  End Sub



  Protected Overrides Sub Finalize()

    MyBase.Finalize()
    Dispose(True)
  End Sub



  Protected Overrides Sub Dispose(ByVal disposing As Boolean)
    Exit Sub
    Try
      If disposing Then
        MyBase.Dispose()
      End If
    Finally
      MyBase.Dispose(disposing)
    End Try
  End Sub
End Class
