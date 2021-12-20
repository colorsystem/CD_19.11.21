Option Compare Text
Option Strict Off
Option Explicit On
Public Class frmMankiewSortiment
  Inherits System.Windows.Forms.Form
  Dim CnHilf As OleDbConnection
  Dim WithEvents OpenColorfile As OpenFileDialog
  Dim WithEvents FarbTabelle As DataTable
  Dim WithEvents FarbAlleTabelle As DataTable

  Dim dataadapter As OleDbDataAdapter
  Dim UpdateCommand As OleDbCommand
  Dim InsertCommand As OleDbCommand
  Dim AllgCommand As OleDbCommand
  Dim SortiID As Integer
  Dim MischID As Integer
  Dim DataBaseName As String
  Dim FarbmName As List(Of String)

  Friend WithEvents cboSortiment As ComboBox


  Public Sub New()

    ' This call is required by the Windows Form Designer.
    InitializeComponent()

    ' Add any initialization after the InitializeComponent() call.

    FarbTabelle = New DataTable
    FarbAlleTabelle = New DataTable
    '
    '
    '
    '
    '
    dataadapter = New OleDbDataAdapter
    dataadapter.SelectCommand = New OleDbCommand
    dataadapter.SelectCommand.Connection = Cndat()
    UpdateCommand = New OleDbCommand
    UpdateCommand.Connection = Cndat()
    InsertCommand = New OleDbCommand
    FarbmName = New List(Of String)
    FarbmName.Add("TBL_SORTI_FARBM")
    FarbmName.Add("TBL_REZEPT_FARBM")
    FarbmName.Add("TBL_GRUND_FARBM")
    FarbmName.Add("TBL_FARBM_PREIS")
    FarbmName.Add("TBL_FARBM_PROZ")
    FarbmName.Add("TBL_FARBM_PROB")
    FarbmName.Add("TBL_FARBM")

    '
    ' 
    'flgKAL.TableStyles(0).GridColumnStyles.Add(New DataGridBoolColumn)

    '
  End Sub
  Sub TabChange()
    Dim Roo As DataRow
    Dim imsg As Integer
    Dim ModDel As Boolean
    If IsNothing(FarbTabelle) Then Exit Sub
    If FarbTabelle.Rows.Count = 0 Then Exit Sub
    ModDel = False
    For Each Roo In FarbTabelle.Rows
      If Roo.RowState <> DataRowState.Unchanged Then
        imsg = MessageBox.Show(Texxt(4638), Texxt(2000), MessageBoxButtons.YesNo)
        If imsg = 6 Then
          ModDel = True
        End If
        Exit For
      End If
    Next
    If ModDel Then
      btnChangesUpdate.PerformClick()
    End If
  End Sub

  Private Sub frmMankiewSortiment_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
    Call TabChange()
    AufbauPar.MethID = -1
  End Sub
  Private Sub frmMankiewDaten_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    btnChangesUpdate.Text = Texxt(4610)


 
    cboSortiment = FormMDI.cboSortiment
    '
    'Tabelle für Farb-/Bindemittel
    '
    '
    flgFarbmittel.AllowUserToOrderColumns = False
    flgFarbmittel.AllowUserToAddRows = False
    flgFarbmittel.AllowUserToDeleteRows = True

    flgFarbmittel.AllowUserToOrderColumns = False
    flgFarbmittel.RowHeadersVisible = True
    flgFarbmittel.SelectionMode = DataGridViewSelectionMode.RowHeaderSelect
    flgFarbmittel.AutoResizeColumns(DataGridViewAutoSizeColumnMode.ColumnHeader)
    '

    AufbauPar.MethID = 53

   
    OpenColorfile = New OpenFileDialog
    '
    '
    Call TabChange()
    '
    '
    '
  End Sub

  Private Sub cboSortiment_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboSortiment.SelectedIndexChanged
    Call TabChange()
    flgFarbmittel.DataSource = FarbTabelle
    lstFarbmittel.DataSource = FarbAlleTabelle
    MischID = MenueParam.MischID
    dataadapter.SelectCommand.CommandText = "SELECT TBL_FARBM.FARBM_ID, TBL_FARBM.FARBM_NAME " _
    & "FROM TBL_FARBM INNER JOIN TBL_GRUND_FARBM ON (TBL_FARBM.FARBM_ID = TBL_GRUND_FARBM.FARBM_ID) AND (TBL_FARBM.MISCH_ID = TBL_GRUND_FARBM.MISCH_ID)" _
    & "WHERE (((TBL_GRUND_FARBM.GKWRT_ID)=" & MenueParam.Misch.GKwrtID & ") AND ((TBL_FARBM.MISCH_ID)=" & MenueParam.MischID & "))ORDER BY FARBM_NAME;"
    FarbAlleTabelle.Clear()
    If Not FillDatset(dataadapter, FarbAlleTabelle) Then
      Exit Sub
    End If
    lstFarbmittel.DisplayMember = "FARBM_NAME"
    lstFarbmittel.ValueMember = "FARBM_ID"

    FarbTabelle.Clear()
    'flgFarbmittel.Rows.Clear()
    If cboSortiment.SelectedIndex = -1 Then Exit Sub
    If SortiTable.Rows.Count = 0 Then Exit Sub
    SortiID = SortiTable.Rows(cboSortiment.SelectedIndex)("SORTI_ID")
    dataadapter.SelectCommand.CommandText = "SELECT TBL_SORTI_FARBM.FARBM_ID AS FARBM_ID,TBL_FARBM.FARBM_NAME AS FARBM_NAME,FARBM_MENGE," _
    & " FARBM_PREIS,FARBM_PROZ,FARBM_PROB,FARBM_TOPF,FARBM_OPERAT,FARBM_LIMMNG,FARBM_ICHF FROM TBL_SORTI_FARBM " _
    & " INNER JOIN TBL_FARBM ON (TBL_FARBM.MISCH_ID=TBL_SORTI_FARBM.MISCH_ID AND TBL_FARBM.FARBM_ID=TBL_SORTI_FARBM.FARBM_ID) " _
    & " WHERE TBL_SORTI_FARBM.MISCH_ID=" & MenueParam.MischID & " AND SORTI_ID=" & SortiID
    If Not FillDatset(dataadapter, FarbTabelle) Then
      Exit Sub
    End If
    '
    FarbTabelle.AcceptChanges()
    '
    flgFarbmittel.Columns(0).HeaderText = "ID"
    flgFarbmittel.Columns(0).Visible = False
    flgFarbmittel.Columns(0).Width = 0
    flgFarbmittel.Columns(0).ReadOnly = True
    '
    'Name
    '
    flgFarbmittel.Columns(1).HeaderText = Texxt(394)
    flgFarbmittel.Columns(1).Width = flgFarbmittel.Width - 600
    flgFarbmittel.Columns(1).Visible = True
    flgFarbmittel.Columns(1).ReadOnly = True
    flgFarbmittel.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    '
    '
    'Menge
    '

    flgFarbmittel.Columns(2).HeaderText = Texxt(4630)
    flgFarbmittel.Columns(2).Width = 75
    flgFarbmittel.Columns(2).ReadOnly = False
    flgFarbmittel.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(2).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(2).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(2).HeaderCell.SortGlyphDirection = SortOrder.None
    '
    '
    'PROZ
    '
    '
    flgFarbmittel.Columns(3).HeaderText = Texxt(820)
    flgFarbmittel.Columns(3).Width = 75
    flgFarbmittel.Columns(3).ReadOnly = False
    flgFarbmittel.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(3).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(3).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(3).HeaderCell.SortGlyphDirection = SortOrder.None 'Menge
    '
    '
    '
    'PROB
    '
    '
    flgFarbmittel.Columns(4).HeaderText = Texxt(821)
    flgFarbmittel.Columns(4).Width = 1
    flgFarbmittel.Columns(4).Visible = False
    flgFarbmittel.Columns(4).ReadOnly = False
    flgFarbmittel.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(4).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(4).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(4).HeaderCell.SortGlyphDirection = SortOrder.None
    '
    'Preis
    ' 
    flgFarbmittel.Columns(5).HeaderText = Texxt(822)
    flgFarbmittel.Columns(5).Width = 75
    flgFarbmittel.Columns(5).ReadOnly = False
    flgFarbmittel.Columns(5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(5).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(5).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(5).HeaderCell.SortGlyphDirection = SortOrder.None
    '
    '
    '
    '
    '
   


    

    flgFarbmittel.Columns(6).HeaderText = "Topf"
    flgFarbmittel.Columns(6).Width = 60
    flgFarbmittel.Columns(6).ReadOnly = False
    flgFarbmittel.Columns(6).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(6).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(6).HeaderCell.SortGlyphDirection = SortOrder.None
    '
    '
    'Operator
    '
    '
    '
  
    flgFarbmittel.Columns(7).HeaderText = "OP"
    flgFarbmittel.Columns(7).Width = 60
    flgFarbmittel.Columns(7).ReadOnly = False
    flgFarbmittel.Columns(7).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(7).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(7).HeaderCell.SortGlyphDirection = SortOrder.None
    '
    'Limitierungsmenge
    '
    flgFarbmittel.Columns(8).HeaderText = Texxt(4637)
    flgFarbmittel.Columns(8).Width = 90
    flgFarbmittel.Columns(8).ReadOnly = False
    flgFarbmittel.Columns(8).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(8).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(8).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(8).HeaderCell.SortGlyphDirection = SortOrder.None
    '
    '
    'ICHF
    '
    '
    flgFarbmittel.Columns(9).HeaderText = "ICHF"
    flgFarbmittel.Columns(9).Visible = False
    flgFarbmittel.Columns(9).Width = 0
    flgFarbmittel.Columns(9).ReadOnly = True
    '



    '
    '
    '
    





  End Sub

  Private Sub frmMankiewDaten_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize
    Call ResizeChild(Me)

  End Sub

  

  Private Sub frmMankiewFarben_VisibleChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.VisibleChanged
    If Me.Visible Then
      cboSortiment_SelectedIndexChanged(sender, e)
    Else
    End If
  End Sub

  Private Sub btnChangesUpdate_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnChangesUpdate.Click
    Dim UpdateString As String
    Dim DeleteString As String
    Dim Roo As DataRow
    Dim FAID As Integer
    Dim i As Integer
    UpdateCommand.Parameters.Clear()
    UpdateCommand.Parameters.Add(New OleDbParameter("FARBM_MENGE", OleDbType.Single))
    UpdateCommand.Parameters.Add(New OleDbParameter("FARBM_PREIS", OleDbType.Single))
    UpdateCommand.Parameters.Add(New OleDbParameter("FARBM_TOPF", OleDbType.Char))
    UpdateCommand.Parameters.Add(New OleDbParameter("FARBM_OPERAT", OleDbType.Char))
    UpdateCommand.Parameters.Add(New OleDbParameter("FARBM_LIMMNG", OleDbType.Single))
    For Each Roo In FarbTabelle.Rows
      If Roo.RowState = DataRowState.Modified Then
        UpdateString = "UPDATE TBL_SORTI_FARBM SET FARBM_MENGE=?,FARBM_PREIS=?,FARBM_TOPF=?,FARBM_OPERAT=?,FARBM_LIMMNG=? WHERE SORTI_ID=" & SortiID & " AND MISCH_ID=" & MischID _
        & " AND FARBM_ID=" & Roo("FARBM_ID")
        UpdateCommand.CommandText = UpdateString
        UpdateCommand.Parameters("FARBM_MENGE").Value = Roo("FARBM_MENGE")
        UpdateCommand.Parameters("FARBM_PREIS").Value = Roo("FARBM_PREIS")
        UpdateCommand.Parameters("FARBM_TOPF").Value = Roo("FARBM_TOPF")
        UpdateCommand.Parameters("FARBM_OPERAT").Value = Roo("FARBM_OPERAT")
        UpdateCommand.Parameters("FARBM_LIMMNG").Value = Roo("FARBM_LIMMNG")
        If SQLExeNonQuery(UpdateCommand, Cndat) <> 0 Then
          Exit Sub
        End If
      ElseIf Roo.RowState = DataRowState.Deleted Then
        Roo.RejectChanges()
        FAID = Roo("FARBM_ID")
        Roo.Delete()
        For i = 0 To FarbmName.Count - 1
          DeleteString = "DELETE * FROM " & FarbmName(i) & " WHERE MISCH_ID=" & MischID & " AND FARBM_ID=" & FAID
          UpdateCommand.CommandText = DeleteString
          If SQLExeNonQuery(UpdateCommand, Cndat) <> 0 Then
            Exit Sub
          End If
        Next i
      End If
    Next
    FarbTabelle.AcceptChanges()
  End Sub



  Protected Overrides Sub Finalize()
    MyBase.Finalize()

  End Sub



  Private Sub flgFarbmittel_CellBeginEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellCancelEventArgs) Handles flgFarbmittel.CellBeginEdit
    Exit Sub
    If e.ColumnIndex = 2 Then Exit Sub
    If FarbTabelle.Rows(e.RowIndex)("FARBM_ICHF") <> 6 Then
      e.Cancel = True
      FarbTabelle.Rows(e.RowIndex).RejectChanges()
    End If
  End Sub



  Private Sub FarbTabelle_RowDeleted(ByVal sender As Object, ByVal e As System.Data.DataRowChangeEventArgs) Handles FarbTabelle.RowDeleted
    '
    '
    'Abfragen, ob "DELETE" rückgängig gemacht werden soll 
    '
    '
    If e.Row.RowError = "NO" Then
      e.Row.RejectChanges()
      e.Row.RowError = ""
    End If
  End Sub

  Private Sub FarbTabelle_RowDeleting(ByVal sender As Object, ByVal e As System.Data.DataRowChangeEventArgs) Handles FarbTabelle.RowDeleting
    If e.Row("FARBM_ICHF") <> 6 Then
      '
      '
      'Error für Event "ROWDELETED" setzen, falls kein Delete durchgeführt werden soll
      '
      '
      '
      e.Row.RowError = "NO"
    End If
  End Sub


  
 
  
  
  Private Sub flgFarbmittel_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles flgFarbmittel.DataError
    e.Cancel = True
  End Sub

End Class