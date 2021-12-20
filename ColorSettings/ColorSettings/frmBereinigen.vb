Option Compare Text
Option Explicit On
Public Class frmBereinigen
  '
  '
  '
  '   


  '
  Dim DatTableMasterRwert As DataTable
  Dim AdaptMasterRwert As OleDbDataAdapter
  Dim DatTableChildRwertQuali As DataTable
  Dim AdaptChildRwertQuali As OleDbDataAdapter
  '
  '
  Dim DatTableMasterRezept As DataTable
  Dim AdaptMasterRezept As OleDbDataAdapter
  Dim DatTableChildRezeptRwert As DataTable
  Dim AdaptChildRezeptRwert As OleDbDataAdapter
  '
  '
  Dim DatTableMasterSorti As DataTable
  Dim AdaptMasterSorti As OleDbDataAdapter
  Dim DatTableChildSortiRwert As DataTable
  Dim AdaptChildSortiRwert As OleDbDataAdapter

  '
  '
  '
  Dim DatTableMasterFarbm As DataTable
  Dim AdaptMasterFarbm As OleDbDataAdapter
  '
  Dim DatTableChildFarbmPreis As DataTable
  Dim AdaptChildFarbmPreis As OleDbDataAdapter
  Dim DatTableChildFarbmProz As DataTable
  Dim AdaptChildFarbmProz As OleDbDataAdapter
  Dim DatTableChildFarbmProb As DataTable
  Dim AdaptChildFarbmProb As OleDbDataAdapter
  Dim DatTableChildFarbmGrund As DataTable
  Dim AdaptChildFarbmGrund As OleDbDataAdapter
  Dim DatTableChildSortiFarbm As DataTable
  Dim AdaptChildSortiFarbm As OleDbDataAdapter
  Dim DatTableChildRezeptFarbm As DataTable
  Dim AdaptChildRezeptFarbm As OleDbDataAdapter

  Dim AdaptChildFarbm As List(Of OleDbDataAdapter)
  Dim DatTableChildFarbm As List(Of DataTable)
  Dim AdaptChildRwert As List(Of OleDbDataAdapter)
  Dim DatTableChildRwert As List(Of DataTable)
  Dim AdaptChildRezept As List(Of OleDbDataAdapter)
  Dim DatTableChildRezept As List(Of DataTable)
  Dim AdaptChildSorti As List(Of OleDbDataAdapter)
  Dim DatTableChildSorti As List(Of DataTable)

  Dim ViewMasterFarbm As DataView
  Dim ViewMasterRwert As DataView
  Dim ViewMasterRezept As DataView
  Dim ViewMasterSorti As DataView

  Dim WhereKeyID() As String
  Private Sub frmBereinigen_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    '
    '
    Me.Text = Texxt(2008)
    '
    '
    Me.Size = MDiform.Size
    Me.Location = MDiform.Location
    btnAdjust.Text = Texxt(2008) & vbCrLf & Texxt(2009)
    btnUpdate.Text = Texxt(387)
    chkAdjust_0.Text = Texxt(4685)
    chkAdjust_1.Text = Texxt(980)
    chkAdjust_2.Text = Texxt(730)
    chkAdjust_3.Text = Texxt(800)
    btnEnd.Text = Texxt(1999)
    lblPanFarbmittel.Text = Texxt(3970)
    radFarbmittel_0.Text = Texxt(3971)
    radFarbmittel_1.Text = Texxt(3972)
    radFarbmittel_2.Text = Texxt(3973)

    '
    '
    'TBL_RWERT
    '
    '
    DatTableMasterRwert = New DataTable
    AdaptMasterRwert = New OleDbDataAdapter
    AdaptMasterRwert.SelectCommand = New OleDbCommand("", Cndat)
    AdaptMasterRwert.DeleteCommand = New OleDbCommand("", Cndat)
    '
    '
    'TBL_QUALI
    '
    '
    '
    DatTableChildRwertQuali = New DataTable
    AdaptChildRwertQuali = New OleDbDataAdapter
    AdaptChildRwertQuali.SelectCommand = New OleDbCommand("", Cndat)
    AdaptChildRwertQuali.DeleteCommand = New OleDbCommand("", Cndat)
    '
    '
    '
    '
    '
    'TBL_REZEPT
    '
    '
    DatTableMasterRezept = New DataTable
    AdaptMasterRezept = New OleDbDataAdapter
    AdaptMasterRezept = New OleDbDataAdapter
    AdaptMasterRezept.SelectCommand = New OleDbCommand("", Cndat)
    AdaptMasterRezept.DeleteCommand = New OleDbCommand("", Cndat)
    DatTableChildRezeptRwert = New DataTable
    AdaptChildRezeptRwert = New OleDbDataAdapter
    AdaptChildRezeptRwert.SelectCommand = New OleDbCommand("", Cndat)
    AdaptChildRezeptRwert.DeleteCommand = New OleDbCommand("", Cndat)

    '
    '
    '
    '
    '
    'TBL_SORTI
    '
    '
    '
    DatTableMasterSorti = New DataTable
    AdaptMasterSorti = New OleDbDataAdapter
    AdaptMasterSorti = New OleDbDataAdapter
    AdaptMasterSorti.SelectCommand = New OleDbCommand("", Cndat)
    AdaptMasterSorti.DeleteCommand = New OleDbCommand("", Cndat)
    DatTableChildSortiRwert = New DataTable
    AdaptChildSortiRwert = New OleDbDataAdapter
    AdaptChildSortiRwert.SelectCommand = New OleDbCommand("", Cndat)
    AdaptChildSortiRwert.DeleteCommand = New OleDbCommand("", Cndat)

    '
    '
    '
    '
    '
    'TBL_FARBM
    '
    '
    '
    AdaptMasterFarbm = New OleDbDataAdapter
    AdaptMasterFarbm.SelectCommand = New OleDbCommand("", Cndat)
    DatTableMasterFarbm = New DataTable
    AdaptChildFarbmPreis = New OleDbDataAdapter
    AdaptChildFarbmProz = New OleDbDataAdapter
    AdaptChildFarbmProb = New OleDbDataAdapter
    AdaptChildFarbmGrund = New OleDbDataAdapter
    DatTableChildSortiFarbm = New DataTable
    AdaptChildSortiFarbm = New OleDbDataAdapter
    DatTableChildRezeptFarbm = New DataTable
    AdaptChildRezeptFarbm = New OleDbDataAdapter

    '
    '  
    DatTableChildFarbmPreis = New DataTable
    DatTableChildFarbmProz = New DataTable
    DatTableChildFarbmProb = New DataTable
    DatTableChildFarbmGrund = New DataTable

    '
    '
    '
    'Selectcommands
    '
    '
    '
    AdaptChildFarbmPreis.SelectCommand = New OleDbCommand("", Cndat)
    AdaptChildFarbmProz.SelectCommand = New OleDbCommand("", Cndat)
    AdaptChildFarbmProb.SelectCommand = New OleDbCommand("", Cndat)
    AdaptChildFarbmGrund.SelectCommand = New OleDbCommand("", Cndat)
    AdaptChildRezeptFarbm.SelectCommand = New OleDbCommand("", Cndat)
    AdaptChildSortiFarbm.SelectCommand = New OleDbCommand("", Cndat)

    '
    '
    AdaptMasterFarbm.SelectCommand.CommandText = "SELECT * FROM TBL_FARBM"
    AdaptChildFarbmPreis.SelectCommand.CommandText = "SELECT * FROM TBL_FARBM_PREIS"
    AdaptChildFarbmProz.SelectCommand.CommandText = "SELECT * FROM TBL_FARBM_PROZ"
    AdaptChildFarbmProb.SelectCommand.CommandText = "SELECT * FROM TBL_FARBM_PROB"
    AdaptChildFarbmGrund.SelectCommand.CommandText = "SELECT * FROM TBL_GRUND_FARBM"
    AdaptChildRezeptFarbm.SelectCommand.CommandText = "SELECT * FROM TBL_REZEPT_FARBM"
    AdaptChildSortiFarbm.SelectCommand.CommandText = "SELECT * FROM TBL_SORTI_FARBM"

    '
    '
    AdaptMasterRwert.SelectCommand.CommandText = "SELECT * FROM TBL_RWERT"


    AdaptChildRwertQuali.SelectCommand.CommandText = "SELECT * FROM TBL_QUALI"

    '
    '
    '
    '
    'TBL_REZEPT
    '
    '
    AdaptMasterRezept.SelectCommand.CommandText = "SELECT * FROM TBL_REZEPT"
    AdaptChildRezeptRwert.SelectCommand.CommandText = "SELECT * FROM TBL_REZEPT_RWERT"
    '
    '
    '
    '
    '
    'TBL_SORTI
    '
    '
    '
    AdaptMasterSorti.SelectCommand.CommandText = "SELECT * FROM TBL_SORTI"
    AdaptChildSortiRwert.SelectCommand.CommandText = "SELECT * FROM TBL_SORTI_RWERT"

    '
    '
    '
    'Deletecommands
    '
    '
    ReDim WhereKeyID(2)
    WhereKeyID(0) = "MISCH_ID"
    WhereKeyID(1) = "FARBM_ID"
    '
    '
    AdaptChildFarbmPreis.DeleteCommand = New OleDbCommand("", Cndat)
    AdaptChildFarbmProz.DeleteCommand = New OleDbCommand("", Cndat)
    AdaptChildFarbmProb.DeleteCommand = New OleDbCommand("", Cndat)
    AdaptChildFarbmGrund.DeleteCommand = New OleDbCommand("", Cndat)
    AdaptChildSortiFarbm.DeleteCommand = New OleDbCommand("", Cndat)
    AdaptChildRezeptFarbm.DeleteCommand = New OleDbCommand("", Cndat)

    '
    '
    WhereKeyID(2) = "FARBM_IRPA"
    AdaptChildFarbmPreis.DeleteCommand = OleDBDeleteCmd("TBL_FARBM_PREIS", WhereKeyID, Cndat)
    WhereKeyID(2) = "FARBM_IRFA"
    AdaptChildFarbmProz.DeleteCommand = OleDBDeleteCmd("TBL_FARBM_PROZ", WhereKeyID, Cndat)
    WhereKeyID(2) = "FARBM_IRBA"
    AdaptChildFarbmProb.DeleteCommand = OleDBDeleteCmd("TBL_FARBM_PROB", WhereKeyID, Cndat)
    ReDim WhereKeyID(4)
    WhereKeyID(0) = "MISCH_ID"
    WhereKeyID(1) = "FARBM_ID"
    WhereKeyID(2) = "MESSGRW_ID"
    WhereKeyID(3) = "MESSG_ID"
    WhereKeyID(4) = "GKWRT_ID"
    AdaptChildFarbmGrund.DeleteCommand = OleDBDeleteCmd("TBL_GRUND_FARBM", WhereKeyID, Cndat)
    ReDim WhereKeyID(2)
    WhereKeyID(0) = "MISCH_ID"
    WhereKeyID(1) = "SORTI_ID"
    WhereKeyID(2) = "FARBM_ID"
    AdaptChildSortiFarbm.DeleteCommand = OleDBDeleteCmd("TBL_SORTI_FARBM", WhereKeyID, Cndat)
    ReDim WhereKeyID(2)
    WhereKeyID(0) = "MISCH_ID"
    WhereKeyID(1) = "REZEPT_ID"
    WhereKeyID(2) = "FARBM_ID"
    AdaptChildRezeptFarbm.DeleteCommand = OleDBDeleteCmd("TBL_REZEPT_FARBM", WhereKeyID, Cndat)

    '
    '

    ReDim WhereKeyID(2)
    WhereKeyID(0) = "MESSGRW_ID"
    WhereKeyID(1) = "QUALI_ID"
    WhereKeyID(2) = "METH_ID"
    AdaptChildRwertQuali.DeleteCommand = OleDBDeleteCmd("TBL_QUALI", WhereKeyID, Cndat)
    '
    '
    '
    '
    '
    'TBL_REZEPT_RWERT
    '
    '
    ReDim WhereKeyID(5)
    WhereKeyID(0) = "MISCH_ID"
    WhereKeyID(1) = "REZEPT_ID"
    WhereKeyID(2) = "MESSGRW_ID"
    WhereKeyID(3) = "REZEPT_ID"
    WhereKeyID(4) = "RWERT_ID"
    WhereKeyID(5) = "RWERT_KWB"

    AdaptChildRezeptRwert.DeleteCommand = OleDBDeleteCmd("TBL_REZEPT_RWERT", WhereKeyID, Cndat)

    '
    '
    '
    '
    '
    'TBL_SORTI_RWERT
    '
    '
    '
    '
    ReDim WhereKeyID(5)
    WhereKeyID(0) = "MISCH_ID"
    WhereKeyID(1) = "SORTI_ID"
    WhereKeyID(2) = "MESSGRW_ID"
    WhereKeyID(3) = "REZEPT_ID"
    WhereKeyID(4) = "RWERT_ID"
    WhereKeyID(5) = "RWERT_KWB"

    AdaptChildSortiRwert.DeleteCommand = OleDBDeleteCmd("TBL_SORTI_RWERT", WhereKeyID, Cndat)



    '
    '
    '
    'List
    '
    '
    'Farbmittel
    '
    '
    DatTableChildFarbm = New List(Of DataTable)
    DatTableChildFarbm.Clear()
    '
    DatTableChildFarbm.Add(DatTableChildFarbmPreis)
    DatTableChildFarbm.Add(DatTableChildFarbmProz)
    DatTableChildFarbm.Add(DatTableChildFarbmProb)
    DatTableChildFarbm.Add(DatTableChildFarbmGrund)
    DatTableChildFarbm.Add(DatTableChildRezeptFarbm)
    DatTableChildFarbm.Add(DatTableChildSortiFarbm)

    '
    AdaptChildFarbm = New List(Of OleDbDataAdapter)
    AdaptChildFarbm.Clear()
    '
    AdaptChildFarbm.Add(AdaptChildFarbmPreis)
    AdaptChildFarbm.Add(AdaptChildFarbmProz)
    AdaptChildFarbm.Add(AdaptChildFarbmProb)
    AdaptChildFarbm.Add(AdaptChildFarbmGrund)
    AdaptChildFarbm.Add(AdaptChildRezeptFarbm)
    AdaptChildFarbm.Add(AdaptChildSortiFarbm)
    '
    '
    ViewMasterFarbm = New DataView(DatTableMasterFarbm)

    '
    '
    'R-Werte
    '
    ' 
    DatTableChildRwert = New List(Of DataTable)
    DatTableChildRwert.Clear()
    '
    DatTableChildRwert.Add(DatTableChildRwertQuali)
    DatTableChildRwert.Add(DatTableChildRezeptRwert)
    DatTableChildRwert.Add(DatTableChildSortiRwert)

    '
    AdaptChildRwert = New List(Of OleDbDataAdapter)
    AdaptChildRwert.Clear()
    '
    AdaptChildRwert.Add(AdaptChildRwertQuali)
    AdaptChildRwert.Add(AdaptChildRezeptRwert)
    AdaptChildRwert.Add(AdaptChildSortiRwert)

    '
    '
    ViewMasterRwert = New DataView(DatTableMasterRwert)

    '
    '
    '
    '
    '
    'Rezepte
    '
    ' 
    DatTableChildRezept = New List(Of DataTable)
    DatTableChildRezept.Clear()
    '
    DatTableChildRezept.Add(DatTableChildRezeptFarbm)
    DatTableChildRezept.Add(DatTableChildRezeptRwert)

    '
    AdaptChildRezept = New List(Of OleDbDataAdapter)
    AdaptChildRezept.Clear()
    '
    AdaptChildRezept.Add(AdaptChildRezeptFarbm)
    AdaptChildRezept.Add(AdaptChildRezeptRwert)

    '
    '
    ViewMasterRezept = New DataView(DatTableMasterRezept)

    '
    '
    'Sortimente
    '
    ' 
    DatTableChildSorti = New List(Of DataTable)
    DatTableChildSorti.Clear()
    '
    DatTableChildSorti.Add(DatTableChildSortiFarbm)
    DatTableChildSorti.Add(DatTableChildSortiRwert)

    '
    AdaptChildSorti = New List(Of OleDbDataAdapter)
    AdaptChildSorti.Clear()
    '
    AdaptChildSorti.Add(AdaptChildSortiFarbm)
    AdaptChildSorti.Add(AdaptChildSortiRwert)

    '
    '
    ViewMasterSorti = New DataView(DatTableMasterSorti)

    '
    '

    '

    '
    lblDatenbank.Text = Cndat.DataSource

  End Sub

  Private Sub btnAdjust_Click(sender As Object, e As System.EventArgs) Handles btnAdjust.Click
    Dim i As Integer
    Dim j As Integer
    Dim index As Integer
    '
    '
    Cursor = Cursors.WaitCursor
    '
    'Tabellen einlesen(R-Werte)
    '
    '
    '
    '
    '
    '
    If Not FillDatset(AdaptMasterRwert, DatTableMasterRwert) Then
      MsgBox("Error reading Table: " & DatTableMasterRwert.TableName)
      Me.DialogResult = Windows.Forms.DialogResult.Abort
      Exit Sub
    End If
    DatTableMasterRwert.AcceptChanges()
    For i = 0 To AdaptChildRwert.Count - 1
      If Not FillDatset(AdaptChildRwert(i), DatTableChildRwert(i)) Then
        MsgBox("Error reading Table: " & DatTableChildRwert(i).TableName)
        Me.DialogResult = Windows.Forms.DialogResult.Abort
        Exit Sub
      End If
      DatTableChildFarbm(i).AcceptChanges()
    Next
    '
    '
    'Tabellen einlesen(Farbmittel)
    '
    '
    '
    If Not FillDatset(AdaptMasterFarbm, DatTableMasterFarbm) Then
      MsgBox("Error reading Table: " & DatTableMasterFarbm.TableName)
      Me.DialogResult = Windows.Forms.DialogResult.Abort
      Exit Sub
    End If
    DatTableMasterFarbm.AcceptChanges()
    For i = 0 To AdaptChildFarbm.Count - 1
      If Not FillDatset(AdaptChildFarbm(i), DatTableChildFarbm(i)) Then
        MsgBox("Error reading Table: " & DatTableChildFarbm(i).TableName)
        Me.DialogResult = Windows.Forms.DialogResult.Abort
        Exit Sub
      End If
      DatTableChildFarbm(i).AcceptChanges()
    Next
    '
    '
    'Tabellen einlesen(Rezepte)
    '
    '
    '
    If Not FillDatset(AdaptMasterRezept, DatTableMasterRezept) Then
      MsgBox("Error reading Table: " & DatTableMasterRezept.TableName)
      Me.DialogResult = Windows.Forms.DialogResult.Abort
      Exit Sub
    End If
    DatTableMasterRezept.AcceptChanges()
    For i = 0 To AdaptChildRezept.Count - 1
      If Not FillDatset(AdaptChildRezept(i), DatTableChildRezept(i)) Then
        MsgBox("Error reading Table: " & DatTableChildRezept(i).TableName)
        Me.DialogResult = Windows.Forms.DialogResult.Abort
        Exit Sub
      End If
      DatTableChildRezept(i).AcceptChanges()
    Next
    '
    'Tabellen einlesen(Sortimente)
    '
    '
    '
    If Not FillDatset(AdaptMasterSorti, DatTableMasterSorti) Then
      MsgBox("Error reading Table: " & DatTableMasterSorti.TableName)
      Me.DialogResult = Windows.Forms.DialogResult.Abort
      Exit Sub
    End If
    DatTableMasterSorti.AcceptChanges()
    For i = 0 To AdaptChildSorti.Count - 1
      If Not FillDatset(AdaptChildSorti(i), DatTableChildSorti(i)) Then
        MsgBox("Error reading Table: " & DatTableChildSorti(i).TableName)
        Me.DialogResult = Windows.Forms.DialogResult.Abort
        Exit Sub
      End If
      DatTableChildSorti(i).AcceptChanges()
    Next
    '
    '
    'R-Werte
    '
    If chkAdjust_0.Checked Then
      '
      '
      '
      'Bereinigen
      '
      '
      '

      For i = 0 To DatTableChildRwert.Count - 1
        For j = DatTableChildRwert(i).Rows.Count - 1 To 0 Step -1
          If Not DatTableChildRwert(i).Rows(j).RowState = DataRowState.Deleted Then
            If i = 0 Then
              ViewMasterRwert.RowFilter = "MESSGRW_ID=" & DatTableChildRwert(i).Rows(j)("MESSGRW_ID") & " AND RWERT_ID=" & DatTableChildRwert(i).Rows(j)("QUALI_ID")
            Else
              ViewMasterRwert.RowFilter = "MESSGRW_ID=" & DatTableChildRwert(i).Rows(j)("MESSGRW_ID") & " AND RWERT_ID=" & DatTableChildRwert(i).Rows(j)("RWERT_ID")
            End If
            If ViewMasterRwert.Count = 0 Then
              DatTableChildRwert(i).Rows(j).Delete()
              'MsgBox("treffer")
            End If
          End If
        Next j
      Next i

    End If
    '
    '
    '
    '
    '
    'FARBMITTEL
    '
    If chkAdjust_1.Checked Then
      '

      '
      'Bereinigen
      '
      '
      '
      For i = 0 To DatTableChildFarbm.Count - 1
        For j = DatTableChildFarbm(i).Rows.Count - 1 To 0 Step -1
          If Not DatTableChildFarbm(i).Rows(j).RowState = DataRowState.Deleted Then
            ViewMasterFarbm.RowFilter = "MISCH_ID=" & DatTableChildFarbm(i).Rows(j)("MISCH_ID") & " AND FARBM_ID=" & DatTableChildFarbm(i).Rows(j)("FARBM_ID")
            If ViewMasterFarbm.Count = 0 Then
              DatTableChildFarbm(i).Rows(j).Delete()
              'MsgBox("treffer")
            End If
          End If
        Next j
      Next i
    End If
    '
    '
    'Rezepte
    '
    If chkAdjust_2.Checked Then
      '

      '
      '
      'Bereinigen
      '
      '
      '
      For i = 0 To DatTableChildRezept.Count - 1
        For j = DatTableChildRezept(i).Rows.Count - 1 To 0 Step -1
          If Not DatTableChildRezept(i).Rows(j).RowState = DataRowState.Deleted Then
            ViewMasterRezept.RowFilter = "MISCH_ID=" & DatTableChildRezept(i).Rows(j)("MISCH_ID") & " AND REZEPT_ID=" & DatTableChildRezept(i).Rows(j)("REZEPT_ID")
            If ViewMasterRezept.Count = 0 Then
              DatTableChildRezept(i).Rows(j).Delete()
              'MsgBox("treffer")
            End If
          End If
        Next j
      Next i
    End If
    '
    'Sortimente
    '
    If chkAdjust_3.Checked Then
      '

      '
      '
      'Bereinigen
      '
      '
      '
      For i = 0 To DatTableChildSorti.Count - 1
        For j = DatTableChildSorti(i).Rows.Count - 1 To 0 Step -1
          If Not DatTableChildSorti(i).Rows(j).RowState = DataRowState.Deleted Then
            ViewMasterSorti.RowFilter = "MISCH_ID=" & DatTableChildSorti(i).Rows(j)("MISCH_ID") & " AND SORTI_ID=" & DatTableChildSorti(i).Rows(j)("SORTI_ID")
            If ViewMasterSorti.Count = 0 Then
              DatTableChildSorti(i).Rows(j).Delete()
              'MsgBox("treffer")
            End If
          End If
        Next j
      Next i
    End If
    Cursor = Cursors.Default
    btnUpdate.Enabled = True
    btnAdjust.Enabled = False
  End Sub

  Private Sub btnUpdate_Click(sender As Object, e As System.EventArgs) Handles btnUpdate.Click
    '
    Dim i As Integer
    Dim RezeptRows() As DataRow
    Dim AuxCommand As OleDbCommand
    AuxCommand = New OleDbCommand("", Cndat)
    Cursor = Cursors.WaitCursor
    '
    'Löschen
    '
    'R-Werte
    '
    '
    If ConnOpen(Cndat) Then
      For i = 0 To AdaptChildRwert.Count - 1
        AdaptChildRwert(i).Update(DatTableChildRwert(i).Select(Nothing, Nothing, DataViewRowState.Deleted))
      Next
      '
      'Farbmittel
      '
      '
      RezeptRows = DatTableChildFarbm(4).Select(Nothing, Nothing, DataViewRowState.Deleted).Clone
      '
      '
      '

      For i = 0 To AdaptChildFarbm.Count - 1
        If radFarbmittel_0.Checked Or i <> 4 Then
          'Nur für Nicht-Rezepttabellen verwenden, falls radFarbmittel_0.Checked =false
          AdaptChildFarbm(i).Update(DatTableChildFarbm(i).Select(Nothing, Nothing, DataViewRowState.Deleted))
        End If
      Next

      ' 
      If Not IsNothing(RezeptRows) AndAlso RezeptRows.Count > 0 Then
        '
        'Löschen alle Farbmittel für Rezuepte mit mindestens einem fehlenden Farbmittel
        '
        '
        If radFarbmittel_1.Checked Or radFarbmittel_2.Checked Then
          For i = 0 To RezeptRows.Count - 1
            AuxCommand.CommandText = "DELETE * FROM TBL_REZEPT_FARBM WHERE MISCH_ID=" & RezeptRows(i)("MISCH_ID", DataRowVersion.Original) _
                                                       & " AND REZEPT_ID=" & RezeptRows(i)("REZEPT_ID", DataRowVersion.Original)
            If SQLNonQuery(AuxCommand, Cndat) <> 0 Then
              MsgBox("error")
            End If
          Next i
        End If
        '
        '
        '
        '
        '
        'Lösche alle Rezepte mit mindestens einem fehlenden Farbmittel
        '
        If radFarbmittel_2.Checked Then
          For i = 0 To RezeptRows.Count - 1
            AuxCommand.CommandText = "DELETE * FROM TBL_REZEPT_RWERT WHERE MISCH_ID=" & RezeptRows(i)("MISCH_ID", DataRowVersion.Original) _
                                                       & " AND REZEPT_ID=" & RezeptRows(i)("REZEPT_ID", DataRowVersion.Original)
            If SQLNonQuery(AuxCommand, Cndat) <> 0 Then
              MsgBox("error")
            End If
          Next i


          '
          For i = 0 To RezeptRows.Count - 1
            AuxCommand.CommandText = "DELETE * FROM TBL_REZEPT WHERE MISCH_ID=" & RezeptRows(i)("MISCH_ID", DataRowVersion.Original) _
                                                       & " AND REZEPT_ID=" & RezeptRows(i)("REZEPT_ID", DataRowVersion.Original)
            If SQLNonQuery(AuxCommand, Cndat) <> 0 Then
              MsgBox("error")
            End If
          Next i
        End If
      End If
      '
      '
      '
      '
      'Löschen von DBNULL-Einträgen in Mastertabelle
      '
      '
      '
      AuxCommand.CommandText = "DELETE * FROM TBL_REZEPT WHERE MISCH_ID IS NULL OR REZEPT_ID IS NULL"
      If SQLNonQuery(AuxCommand, Cndat) <> 0 Then
        MsgBox("error")
      End If
      '
      '
      '
      AuxCommand.CommandText = "DELETE * FROM TBL_SORTI WHERE MISCH_ID IS NULL OR SORTI_ID IS NULL"
      If SQLNonQuery(AuxCommand, Cndat) <> 0 Then
        MsgBox("error")
      End If
      '
      '
      '
      AuxCommand.CommandText = "DELETE * FROM TBL_RWERT WHERE MESSGRW_ID IS NULL OR RWERT_ID IS NULL"
      If SQLNonQuery(AuxCommand, Cndat) <> 0 Then
        MsgBox("error")
      End If
      '
      '
      '
      AuxCommand.CommandText = "DELETE * FROM TBL_FARBM WHERE MISCH_ID IS NULL OR FARBM_ID IS NULL"
      If SQLNonQuery(AuxCommand, Cndat) <> 0 Then
        MsgBox("error")
      End If




    End If
    Cndat.Close()
    '
    '
    For i = 0 To AdaptChildRwert.Count - 1
      AdaptChildRwert(i).Dispose()
      DatTableChildRwert(i).Dispose()
    Next
    For i = 0 To AdaptChildFarbm.Count - 1
      AdaptChildFarbm(i).Dispose()
      DatTableChildFarbm(i).Dispose()
    Next
    AuxCommand.Dispose()
    Erase RezeptRows

    '
    Cursor = Cursors.Default
    DialogResult = Windows.Forms.DialogResult.OK
    '
  End Sub

  Private Sub btnEnd_Click(sender As Object, e As System.EventArgs) Handles btnEnd.Click
    If Me.Modal Then
      Me.DialogResult = Windows.Forms.DialogResult.OK
    Else
      Me.Close()
    End If
  End Sub
End Class