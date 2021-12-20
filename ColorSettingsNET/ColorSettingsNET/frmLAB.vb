Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmLAB
  
  Dim MaxID As Long             'Maximale ID (Primärschlüssel)
  Dim Sqlstmt As String
  '
  '
  Dim TblLAB As DataTable
  Dim TblMET As DataTable
  '
  Dim CmdLAB As OleDbCommand
  '
  '
  '
  '
  '
  Dim AdaptLAB As OleDbDataAdapter
  '
  '
  '
  '
  '
  '
  Dim WithEvents ConnLAB As BindingSource


  Private Sub frmLAB_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    '
    Me.Text = Texxt(407)
    lblLAB_00.Text = Texxt(282)
    lblLAB_01.Text = Texxt(283)
    lblLAB_02.Text = Texxt(701)
    lblLAB_03.Text = Texxt(702)
    lblLAB_04.Text = Texxt(703)
    lblLAB_05.Text = Texxt(704)
    lblLAB_06.Text = Texxt(705)
    lblLAB_07.Text = Texxt(706)
    lblLAB_08.Text = Texxt(707)
    btnORD.Text = Texxt(1999)
    '
    TblLAB = New DataTable
    '
    '
    CmdLAB = New OleDbCommand("", Cncol)
    '
    '
    '
    AdaptLAB = New OleDbDataAdapter
    '
    ConnLAB = New BindingSource

    '
    BindingLAB.BindingSource = ConnLAB
    '
    '
    '
    '
    AdaptLAB.SelectCommand = CmdLAB
    '
    '
    '
    '

    '
    Sqlstmt = "SELECT * FROM TBL_LABOR"
    '
    CmdLAB.CommandText = Sqlstmt
    If Not FillDatset(AdaptLAB, TblLAB) Then
      Exit Sub
    End If
    TblLAB.AcceptChanges()
    '
    '
    '
    '
    '
    '
    ConnLAB.DataSource = TblLAB

    txtLAB_00.DataBindings.Add("TEXT", ConnLAB, "LABOR_ID")
    txtLAB_01.DataBindings.Add("TEXT", ConnLAB, "LABOR_KBEZ")
    txtLAB_02.DataBindings.Add("TEXT", ConnLAB, "LABOR_LBEZ")
    txtLAB_03.DataBindings.Add("TEXT", ConnLAB, "LABOR_ABTC")
    txtLAB_04.DataBindings.Add("TEXT", ConnLAB, "LABOR_BAU")
    txtLAB_05.DataBindings.Add("TEXT", ConnLAB, "LABOR_RAUM")
    txtLAB_06.DataBindings.Add("TEXT", ConnLAB, "LABOR_KST")
    txtLAB_07.DataBindings.Add("TEXT", ConnLAB, "LABOR_TEXT1")
    txtLAB_08.DataBindings.Add("TEXT", ConnLAB, "LABOR_TEXT2")
    txtLAB_09.DataBindings.Add("TEXT", ConnLAB, "LABOR_TEXT3")

    txtLAB_01.MaxLength = TblLAB.Columns("LABOR_KBEZ").MaxLength
    txtLAB_02.MaxLength = TblLAB.Columns("LABOR_LBEZ").MaxLength
    txtLAB_03.MaxLength = TblLAB.Columns("LABOR_ABTC").MaxLength
    txtLAB_04.MaxLength = TblLAB.Columns("LABOR_BAU").MaxLength
    txtLAB_05.MaxLength = TblLAB.Columns("LABOR_RAUM").MaxLength
    txtLAB_07.MaxLength = TblLAB.Columns("LABOR_TEXT1").MaxLength
    txtLAB_08.MaxLength = TblLAB.Columns("LABOR_TEXT2").MaxLength
    txtLAB_09.MaxLength = TblLAB.Columns("LABOR_TEXT3").MaxLength

  End Sub

  Private Sub ConnLAB_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnLAB.AddingNew
    Dim i As Integer
    '
    'Defaultstartwerte setzen
    '
    '
    For i = 0 To TblLAB.Columns.Count - 1
      If Not TblLAB.Columns(i).AutoIncrement Then
        TblLAB.Columns(i).DefaultValue = ConnLAB.Current(i)
      End If
    Next i
    '
    MaxID = MaxDatTableID(TblLAB, "LABOR_ID", {""}, {-1}) + 1
    TblLAB.Columns("LABOR_KBEZ").DefaultValue = TblLAB.Columns("LABOR_KBEZ").DefaultValue & " ????"
    If TblLAB.Columns("LABOR_KBEZ").DefaultValue.length > TblLAB.Columns("LABOR_KBEZ").MaxLength Then
      TblLAB.Columns("LABOR_KBEZ").DefaultValue = TblLAB.Columns("LABOR_KBEZ").DefaultValue.substring(0, TblLAB.Columns("LABOR_KBEZ").MaxLength)
    End If
    '
    '
  End Sub
  '
  '
  '
  '
  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    Dim WhereKeyID() As String
    If AddDelP(2990) Then
      '
      '
      '
      'TBL_LABOR
      '
      ConnLAB.CurrencyManager.EndCurrentEdit()
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptLAB.InsertCommand = OleDBInsertCmd("TBL_LABOR", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "LABOR_ID"
      AdaptLAB.UpdateCommand = OleDBUpdateCmd("TBL_LABOR", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptLAB.DeleteCommand = OleDBDeleteCmd("TBL_LABOR", WhereKeyID, Cncol)
      '
      '

      'Delete/Update/Insert TBL_LABOR
      '
      'Delete TBL_LABOR
      '
      AdaptLAB.Update(TblLAB.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Insert TBL_LABOR
      '
      '
      AdaptLAB.Update(TblLAB.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Update TBL_LABOR
      '
      '
      AdaptLAB.Update(TblLAB.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      '
    End If
    Me.Close()
    Me.Dispose()
  End Sub




  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.

  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub

End Class


