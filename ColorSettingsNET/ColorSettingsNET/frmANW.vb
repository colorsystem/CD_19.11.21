Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmANW
  REM Laufvariable
  Dim i As Integer
  Dim l As Integer
  Dim k As Integer
  Dim j As Integer
  Dim kw As Integer
  REM Zwischengroessen
  Dim nkw As Integer
  'Message Kennung
  Dim imsg As Integer            'Messagebox (Rückgabewert)
  Dim MaxID As Long             'Maximale ID (Primärschlüssel)
  Dim ier As Integer
  Dim SqlStmt As String
  Dim StrLinANW As String
  Dim txtANW As List(Of TextBox)
  Dim MnIopenArt As Boolean
  Dim MnUserUfo As Boolean
  '
  '
  Dim TblANW As DataTable
  Dim TblMRK As DataTable
  Dim TblAFG As DataTable
  Dim TblAMR As DataTable
  '
  Dim CmdANW As OleDbCommand
  Dim CmdMRK As OleDbCommand
  Dim CmdAFG As OleDbCommand
  Dim CmdAMR As OleDbCommand
  '
  '
  '
  '
  '
  Dim AdaptANW As OleDbDataAdapter
  Dim AdaptMRK As OleDbDataAdapter
  Dim AdaptAFG As OleDbDataAdapter
  Dim AdaptAMR As OleDbDataAdapter
  '
  '
  '
  '
  Dim ViewAMR As DataView
  '
  '
  Dim WithEvents ConnANW As BindingSource


  Private Sub frmANW_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    '
    Me.Text = Texxt(500)
    btnORD.Text = Texxt(1999)
    lblMRK.Text = Texxt(506)
    lblAMR.Text = Texxt(507)
    lblANW_0.Text = Texxt(502)
    lblANW_1.Text = Texxt(503)
    lblANW_2.Text = Texxt(501)
    '
    TblANW = New DataTable
    TblMRK = New DataTable
    TblAFG = New DataTable
    TblAMR = New DataTable
    '
    CmdANW = New OleDbCommand("", Cncol)
    CmdMRK = New OleDbCommand("", Cncol)
    CmdAFG = New OleDbCommand("", Cncol)
    CmdAMR = New OleDbCommand("", Cncol)
    '
    '
    '
    '
    '
    AdaptANW = New OleDbDataAdapter
    AdaptMRK = New OleDbDataAdapter
    AdaptAFG = New OleDbDataAdapter
    AdaptAMR = New OleDbDataAdapter
    '
    AdaptANW.SelectCommand = CmdANW
    AdaptMRK.SelectCommand = CmdMRK
    AdaptAFG.SelectCommand = CmdAFG
    AdaptAMR.SelectCommand = CmdAMR
    '
    '
    ViewAMR = New DataView(TblAMR)
    '
    '
    ConnANW = New BindingSource
    BindingANW.BindingSource = ConnANW
    '
    '
    txtANW = New List(Of TextBox)
    txtANW.Add(txtANW_0)
    txtANW.Add(txtANW_1)
    txtANW.Add(txtANW_2)
    txtANW.Add(txtANW_3)

    '
    '
    '
    If mnUserUfo Then
      cboAFG.Enabled = True
      BindingANW.Visible = True
      txtANW(2).ReadOnly = False
      txtANW(3).ReadOnly = False
    End If

    '
    'Namen und ID's für Aufgaben (z,B. @TMW11@PMW11)
    'TBL_AUFG
    '
    '
    SqlStmt = "SELECT * FROM TBL_AUFG"
    CmdAFG.CommandText = SqlStmt
    If Not FillDatset(AdaptAFG, TblAFG) Then
      Exit Sub
    End If
    TblAFG.AcceptChanges()

    'datAFG.Recordset = DBas.OpenRecordset("TBL_AUFG", dbOpenDynaset, dbConsistent, dbReadOnly)
    '


    '
    'Merkmale
    'TBL_MERK
    '
    '
    SqlStmt = "SELECT * FROM TBL_MERK ORDER BY MERK_ID"
    CmdMRK.CommandText = SqlStmt
    If Not FillDatset(AdaptMRK, TblMRK) Then
      Exit Sub
    End If
    TblMRK.AcceptChanges()
    'datMRK.Recordset = DBas.OpenRecordset("TBL_MERK", dbOpenDynaset, dbConsistent, dbReadOnly)
    '

    '
    '
    '
    'Anweisungen
    'TBL_ANWSG
    '
    '
    'datANW.DatabaseName = DbName
    'datANW.Refresh
    '
    SqlStmt = "SELECT * FROM TBL_ANWSG"
    CmdANW.CommandText = SqlStmt
    If Not FillDatset(AdaptANW, TblANW) Then
      Exit Sub
    End If
    TblANW.AcceptChanges()
    StrLinANW = StrLin(TblANW, "ANWSG_ID")
    'datANW.Recordset = DBas.OpenRecordset("TBL_ANWSG", dbOpenDynaset, dbConsistent, dbReadOnly)
    '
    'Maximum der Anweisungs-ID
    'datANW.Recordset.MoveLast()
    'MaxID = datANW.Recordset!Anwsg_ID
    'datANW.Recordset.MoveFirst()
    SqlStmt = "SELECT ANWSG_ID,TBL_ANWSG_MERK.MERK_ID AS MERK_ID,TBL_MERK.MERK_KBEZ AS MERK_KBEZ FROM TBL_ANWSG_MERK INNER JOIN TBL_MERK" _
      & " ON TBL_ANWSG_MERK.MERK_ID=TBL_MERK.MERK_ID" _
     & " WHERE [ANWSG_ID] IN " & StrLinANW & " ORDER BY TBL_MERK.MERK_ID"
    'SqlStmt = "SELECT * FROM TBL_ANWSG_MERK WHERE ANWSG_ID=" & datANW.Recordset!Anwsg_ID
    'MsgBox SqlStmt
    CmdAMR.CommandText = SqlStmt
    If Not FillDatset(AdaptAMR, TblAMR) Then
      Exit Sub
    End If
    TblAMR.AcceptChanges()
    'datAMR.Recordset = DBas.OpenRecordset(SqlStmt, dbOpenDynaset)
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
    '
    '
    lstAMR.DataSource = ViewAMR
    lstAMR.DisplayMember = "MERK_KBEZ"
    lstAMR.ValueMember = "MERK_ID"
    '
    lstMRK.DataSource = TblMRK
    lstMRK.DisplayMember = "MERK_KBEZ"
    lstMRK.ValueMember = "MERK_ID"
    '
    '
    '
    cboAFG.DataSource = TblAFG
    cboAFG.DisplayMember = "AUFG_ART"
    cboAFG.ValueMember = "AUFG_ID"
    '
    '
    '
    '
    ConnANW.DataSource = TblANW
    '
    '
    '
    txtANW_0.DataBindings.Add("TEXT", ConnANW, "ANWSG_ID")
    txtANW_2.DataBindings.Add("TEXT", ConnANW, "ANWSG_KBEZ")
    txtANW_3.DataBindings.Add("TEXT", ConnANW, "ANWSG_LBEZ")
    txtANW_1.DataBindings.Add("TEXT", ConnANW, "AUFG_ID")
    cboAFG.DataBindings.Add("SELECTEDVALUE", ConnANW, "AUFG_ID")

  End Sub
  '
  '
  '
  '
  Private Sub ConnANW_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnANW.CurrentChanged
    If ConnANW.Current Is Nothing OrElse ConnANW.Count = 0 Then Exit Sub
    '
    '
    'Neuen Recordset für neue Anweisung
    'TBL_ANWSG_MERK
    '

    ViewAMR.RowFilter = "ANWSG_ID=" & ConnANW.Current("ANWSG_ID")
    lstAMR.SelectedIndex = 0
    lblKEN.Text = "**"
  End Sub


  Private Sub lstMRK_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstMRK.Click
    Dim i As Integer
    Dim RowView As DataRowView
    If lstMRK.SelectedIndex < 0 Then Exit Sub
    'If IsEmpty(dblWIN.SelectedItem) Or LenB(dblWIN.SelectedItem) = 0 Then
    ' Exit Sub
    ' End If

    For i = 0 To ViewAMR.Count - 1
      If lstMRK.SelectedValue = ViewAMR(i)("MERK_ID") Then
        Exit Sub
      End If
    Next
    '
    '
    '
    RowView = ViewAMR.AddNew()
    RowView("ANWSG_ID") = ConnANW.Current("ANWSG_ID")
    RowView("MERK_ID") = lstMRK.SelectedValue
    RowView("MERK_KBEZ") = lstMRK.SelectedItem("MERK_KBEZ")
    RowView.EndEdit()
  End Sub
  Private Sub lstMRK_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles lstMRK.MouseMove
    Dim i As Integer
    i = e.Y / lstMRK.ItemHeight + lstMRK.TopIndex
    If i >= lstMRK.Items.Count Then
      Exit Sub
    End If
    lstMRK.SetSelected(i, True)
    lblKEN.Text = lstMRK.SelectedItem("MERK_KEN")
    lblLBEZ.Text = lstMRK.SelectedItem("MERK_LBEZ")
  End Sub

  Private Sub lstAMR_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstAMR.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call lstAMR_KeyDown(sender, ev)
  End Sub



  Private Sub lstAMR_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstAMR.KeyDown
    ViewAMR.Delete(lstAMR.SelectedIndex)
     
  End Sub
  Private Sub lstAMR_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles lstAMR.MouseMove
    Dim i As Integer
    i = e.Y / lstAMR.ItemHeight + lstAMR.TopIndex
    If i >= lstAMR.Items.Count Then
      Exit Sub
    End If
    lstAMR.SetSelected(i, True)
    lstMRK.SelectedValue = lstAMR.SelectedValue
    lblKEN.Text = lstMRK.SelectedItem("MERK_KEN")
    lblLBEZ.Text = lstMRK.SelectedItem("MERK_LBEZ")
  End Sub
  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    
    Dim WhereKeyID() As String
    If AddDelP(2990) Then
      '
      '
      '
      '
      TblANW.EndInit()
      '
      '
      '
      '
      '
      'Updatecommand
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "ANWSG_ID"
      AdaptANW.UpdateCommand = OleDBUpdateCmd("TBL_ANWSG", WhereKeyID, Cncol)
      AdaptANW.Update(TblANW.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      TblAMR.EndInit()
      '

      '
      'TBL_ANWSG_MERK
      '
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptAMR.InsertCommand = OleDBInsertCmd("TBL_ANWSG_MERK", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "ANWSG_ID"
      WhereKeyID(1) = "MERK_ID"
      AdaptAMR.UpdateCommand = OleDBUpdateCmd("TBL_ANWSG_MERK", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      AdaptAMR.DeleteCommand = OleDBDeleteCmd("TBL_ANWSG_MERK", WhereKeyID, Cncol)
      '
      '
      '
     
      '
      '
      'Delete/Update/Insert TBL_ANWSG_MERK
      '
      'Delete TBL_ANWSG_MERK 
      '
      '      
      ' 
      '
      AdaptAMR.Update(TblAMR.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      '
      '
      'Insert TBL_ANWSG_MERK
      '
      '
      AdaptAMR.Update(TblAMR.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      'Update TBL_ANWSG_MERK
      '
      '
      AdaptAMR.Update(TblAMR.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
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
    Iopenart = True
  End Sub




  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub

  
  WriteOnly Property IopenArt() As Boolean
    Set(ByVal value As Boolean)
      MnIopenArt = value
    End Set
  End Property
  WriteOnly Property UserUfo() As Boolean
    Set(ByVal value As Boolean)
      MnUserUfo = value
    End Set
  End Property
 
 
  
   
End Class


