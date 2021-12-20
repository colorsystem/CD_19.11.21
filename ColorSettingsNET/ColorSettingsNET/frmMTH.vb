Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmMTH

  Dim MaxID As Long             'Maximale ID (Primärschlüssel)
  Dim Sqlstmt As String
  Dim StrLinMTH As String
  Dim i As Integer
  Dim j As Integer
  Dim imsg As Integer
  Dim ier As Integer '
  Dim MnIopenArt As Boolean
  Dim MnUserUfo As Boolean
  '
  Dim TblMTH As DataTable
  Dim TblANWSG As DataTable
  Dim TblANWSGSelect As DataTable
  Dim TblANWSGUser As DataTable
  Dim TblANWSGMetUser As DataTable
  Dim TblANWSGMetUserMerk As DataTable
  '
  Dim CmdMTH As OleDbCommand
  Dim CmdANWSG As OleDbCommand
  Dim CmdANWSGSelect As OleDbCommand
  Dim CmdANWSGUser As OleDbCommand
  Dim CmdANWSGMetUser As OleDbCommand
  Dim CmdANWSGMetUserMerk As OleDbCommand
  '
  '
  Dim ViewANWSGSelect As DataView
  Dim ViewANWSGuser As DataView
  Dim ViewANWSGMetUser As DataView
  Dim ViewANWSGMetUserMerk As DataView
  '
  '
  '
  Dim AdaptMTH As OleDbDataAdapter
  Dim AdaptANWSG As OleDbDataAdapter
  Dim AdaptANWSGSelect As OleDbDataAdapter
  Dim AdaptANWSGuser As OleDbDataAdapter
  Dim AdaptANWSGMetUser As OleDbDataAdapter
  Dim AdaptANWSGMetUserMerk As OleDbDataAdapter
  '
  '
  '
  '
  '
  '
  Dim WithEvents ConnMTH As BindingSource


  Private Sub frmMTH_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    '
    Me.Text = Texxt(415)
    btnORD.Text = Texxt(1999)
    lblMET.Text = Texxt(421)
    lblANW.Text = Texxt(504)
    lblANM.Text = Texxt(505)

    '
    TblMTH = New DataTable
    TblANWSG = New DataTable
    TblANWSGSelect = New DataTable
    TblANWSGUser = New DataTable
    TblANWSGMetUser = New DataTable
    TblANWSGMetUserMerk = New DataTable
    '
    CmdMTH = New OleDbCommand("", Cncol)
    CmdANWSG = New OleDbCommand("", Cncol)
    CmdANWSGSelect = New OleDbCommand("", Cncol)
    CmdANWSGUser = New OleDbCommand("", Cncol)
    CmdANWSGMetUser = New OleDbCommand("", Cncol)
    CmdANWSGMetUserMerk = New OleDbCommand("", Cncol)
    '
    '
    ViewANWSGSelect = New DataView(TblANWSGSelect)
    ViewANWSGuser = New DataView(TblANWSGUser)
    ViewANWSGMetUser = New DataView(TblANWSGMetUser)
    ViewANWSGMetUserMerk = New DataView(TblANWSGMetUserMerk)
    '
    '
    '
    AdaptMTH = New OleDbDataAdapter
    AdaptANWSG = New OleDbDataAdapter
    AdaptANWSGSelect = New OleDbDataAdapter
    AdaptANWSGuser = New OleDbDataAdapter
    AdaptANWSGMetUser = New OleDbDataAdapter
    AdaptANWSGMetUserMerk = New OleDbDataAdapter
    '
    '
    '
    AdaptMTH.SelectCommand = CmdMTH
    AdaptANWSG.SelectCommand = CmdANWSG
    AdaptANWSGSelect.SelectCommand = CmdANWSGSelect
    AdaptANWSGuser.SelectCommand = CmdANWSGUser
    AdaptANWSGMetUser.SelectCommand = CmdANWSGMetUser
    AdaptANWSGMetUserMerk.SelectCommand = CmdANWSGMetUserMerk
    '
    '
    ConnMTH = New BindingSource
     
    '
    '
    '
    '
    '
    '
    '
    BindingMTH.BindingSource = ConnMTH
     '
    '
    '
    '
    '
    '
    'Methoden aus
    'TBL_METH
    '
    Sqlstmt = "SELECT * FROM TBL_METH WHERE METH_BEZ<>" & "'""'" & " AND METH_ID > 0  ORDER BY METH_ID"
    'datMTH.Recordset = DBas.OpenRecordset(Sqlstmt, dbOpenDynaset, dbConsistent, dbReadOnly)
    '

    CmdMTH.CommandText = Sqlstmt
    If Not FillDatset(AdaptMTH, TblMTH) Then
      Exit Sub
    End If
    For i = 0 To TblMTH.Rows.Count - 1
      If Trim(TblMTH.Rows(i)("METH_BEZ")) = "" Then
        TblMTH.Rows(i).Delete()
      End If
    Next
    TblMTH.AcceptChanges()
    StrLinMTH = StrLin(TblMTH, "METH_ID")
    '
    '
    '
    '
    '
    'Anweisungen aus
    'TBL_ANWSG
    '
    Sqlstmt = "SELECT * FROM TBL_ANWSG ORDER BY ANWSG_ID"
    'datANW.Recordset = DBas.OpenRecordset(Sqlstmt, dbOpenDynaset, dbConsistent, dbReadOnly)
    CmdANWSG.CommandText = Sqlstmt
    If Not FillDatset(AdaptANWSG, TblANWSG) Then
      Exit Sub
    End If
    TblANWSG.AcceptChanges()
    '
    '
    '
    'TBL_METH_ANWSG
    '
    '
    '
    '
    Sqlstmt = "SELECT DISTINCT TBL_METH_ANWSG.METH_ID AS METH_ID,TBL_METH_ANWSG.ANWSG_ID AS ANWSG_ID,ANWSG_KBEZ" _
    & " FROM TBL_METH_ANWSG INNER JOIN TBL_ANWSG ON TBL_METH_ANWSG.ANWSG_ID=TBL_ANWSG.ANWSG_ID"
    'datANM.Recordset = DBas.OpenRecordset(Sqlstmt, dbOpenDynaset, dbConsistent, dbReadOnly)
    CmdANWSGSelect.CommandText = Sqlstmt
    If Not FillDatset(AdaptANWSGSelect, TblANWSGSelect) Then
      Exit Sub
    End If
    TblANWSGSelect.AcceptChanges()
    ViewANWSGSelect.Sort = "ANWSG_ID"
    '
    '
    'TBL_USER_METH 
    '
    '
    '
    Sqlstmt = "SELECT DISTINCT TBL_USER_METH.METH_ID AS METH_ID,TBL_USER_METH.USER_ID AS USER_ID" _
    & " FROM TBL_USER_METH"
    'datANM.Recordset = DBas.OpenRecordset(Sqlstmt, dbOpenDynaset, dbConsistent, dbReadOnly)
    CmdANWSGUser.CommandText = Sqlstmt
    If Not FillDatset(AdaptANWSGuser, TblANWSGUser) Then
      Exit Sub
    End If
    TblANWSGUser.AcceptChanges() ' 
    '
    '
    'TBL_USER_METH_ANWSG und TBL_USER_METH
    '
    Sqlstmt = "SELECT TBL_USER_METH_ANWSG.USER_ID AS USER_ID,TBL_USER_METH_ANWSG.METH_ID AS METH_ID,TBL_USER_METH_ANWSG.ANWSG_ID AS ANWSG_ID,ZEIL_NR " _
    & " FROM TBL_USER_METH_ANWSG INNER JOIN TBL_USER_METH ON TBL_USER_METH_ANWSG.USER_ID=TBL_USER_METH.USER_ID AND TBL_USER_METH_ANWSG.METH_ID=TBL_USER_METH.METH_ID"
    'datANM.Recordset = DBas.OpenRecordset(Sqlstmt, dbOpenDynaset, dbConsistent, dbReadOnly)

    CmdANWSGMetUser.CommandText = Sqlstmt
    If Not FillDatset(AdaptANWSGMetUser, TblANWSGMetUser) Then
      Exit Sub
    End If
    TblANWSGMetUser.AcceptChanges()
    ViewANWSGMetUser.Sort = "ZEIL_NR"
    '
    '
    '
    'TBL_USER_METH_ANWSG_MERK
    '
    Sqlstmt = "SELECT * FROM TBL_USER_METH_ANWSG_MERK"
    'datANM.Recordset = DBas.OpenRecordset(Sqlstmt, dbOpenDynaset, dbConsistent, dbReadOnly)

    CmdANWSGMetUserMerk.CommandText = Sqlstmt
    If Not FillDatset(AdaptANWSGMetUserMerk, TblANWSGMetUserMerk) Then
      Exit Sub
    End If
    TblANWSGMetUserMerk.AcceptChanges()
    '
    '
    '
    '
    '
    ViewANWSGMetUser.Sort = "ZEIL_NR"
    lstANW.DataSource = TblANWSG
    lstANW.DisplayMember = "ANWSG_KBEZ"
    lstANW.ValueMember = "ANWSG_ID"
    lstANM.DataSource = ViewANWSGSelect
    lstANM.DisplayMember = "ANWSG_KBEZ"
    lstANM.ValueMember = "ANWSG_ID"
    ConnMTH.DataSource = TblMTH
    '
    cboMTH.DataSource = ConnMTH
    cboMTH.DisplayMember = "METH_BEZ"
    cboMTH.ValueMember = "METH_ID"
    '
    lblMTH.DataBindings.Add("TEXT", ConnMTH, "METH_BEZ")

  End Sub

 
  '
  '
  '
  '
  




  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.

  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub
   
 
  Private Sub ConnMTH_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMTH.CurrentChanged
    If ConnMTH.Current Is Nothing OrElse ConnMTH.Count = 0 Then Exit Sub
    '
    'Dataview  Methoden-abhängigen Anweisungen
    '
    '
    '
    '
    ViewANWSGSelect.RowFilter = "METH_ID=" & ConnMTH.Current("METH_ID")
    ViewANWSGuser.RowFilter = "METH_ID=" & ConnMTH.Current("METH_ID")
    ViewANWSGMetUser.RowFilter = "METH_ID=" & ConnMTH.Current("METH_ID")
    ViewANWSGMetUserMerk.RowFilter = "METH_ID=" & ConnMTH.Current("METH_ID")
    'Sqlstmt = "SELECT *,TBL_ANWSG.ANWSG_ID AS ANWSG_ID FROM TBL_METH_ANWSG,TBL_ANWSG" _
    '& " WHERE METH_ID=" & datMTH.Recordset!METH_ID _
    '& " AND TBL_ANWSG.ANWSG_ID=TBL_METH_ANWSG.ANWSG_ID"
    'datANM.Recordset = DBas.OpenRecordset(Sqlstmt, dbOpenDynaset, dbConsistent, dbReadOnly)

  End Sub
  Private Sub lstANW_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstANW.Click
    Dim i As Integer
    Dim RowView As DataRowView
    If ConnMTH.Current Is Nothing OrElse ConnMTH.Count = 0 Then Exit Sub
    If IsNothing(lstANW.SelectedValue) OrElse Not IsNumeric(lstANW.SelectedValue) Then Exit Sub
    '
    'Prüfen, ob Anweisung bereits vorhanden
    '
    '
    '
    For i = 0 To ViewANWSGSelect.Count - 1
      If ViewANWSGSelect(i)("ANWSG_ID") = lstANW.SelectedValue Then
        Exit Sub
      End If
    Next
    '
    '
    'Anweisung nach TblANWSGSelect übernehmen
    '
    '
    '
    RowView = ViewANWSGSelect.AddNew()
    RowView("METH_ID") = ConnMTH.Current("METH_ID")
    RowView("ANWSG_ID") = lstANW.SelectedItem("ANWSG_ID")
    RowView("ANWSG_KBEZ") = lstANW.SelectedItem("ANWSG_KBEZ")
    RowView.EndEdit()
    '
    '
    'Anweisung nach TblANWSGMetUser übernehmen
    '
    '
    '
    For i = 0 To ViewANWSGuser.Count - 1
      RowView = ViewANWSGMetUser.AddNew()
      RowView("USER_ID") = ViewANWSGuser(i)("USER_ID")
      RowView("METH_ID") = ConnMTH.Current("METH_ID")
      RowView("ANWSG_ID") = lstANW.SelectedItem("ANWSG_ID")
      RowView("ZEIL_NR") = i + 1
      RowView.EndEdit()
    Next i
  End Sub




  Private Sub lstANM_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstANM.DoubleClick
    Dim IndexDel As Integer
    Dim RowView As DataRowView
    If ConnMTH.Current Is Nothing OrElse ConnMTH.Count = 0 Then Exit Sub
    If IsNothing(lstANM.SelectedValue) OrElse Not IsNumeric(lstANM.SelectedValue) Then Exit Sub
    '
    '
    '
    'TBL_USER_METH_ANWSG_MERK
    '
    '
    For Each RowView In ViewANWSGMetUserMerk
      If RowView("ANWSG_ID") = lstANM.SelectedValue Then
        RowView.Delete()
      End If
    Next
    '
    '
    '
    '
    'TBL_USER_METH_ANWSG 
    '
    '
    '
    '
    For Each RowView In ViewANWSGMetUser
      If RowView("ANWSG_ID") = lstANM.SelectedValue Then
        RowView.Delete()
      End If
    Next
    '
    '
    '

    IndexDel = ViewANWSGSelect.Find(lstANM.SelectedValue)
    If IndexDel > -1 Then
      ViewANWSGSelect(IndexDel).Delete()
    End If

    '
    '
    '
    '
  End Sub

  


 

  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    Dim WhereKeyID() As String
    If AddDelP(2990) Then
      '
      '
      '
      'TBL_METH_ANWSG
      '
      ViewANWSGSelect.EndInit()
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptANWSGSelect.InsertCommand = OleDBInsertCmd("TBL_METH_ANWSG", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "METH_ID"
      WhereKeyID(1) = "ANWSG_ID"
      AdaptANWSGSelect.UpdateCommand = OleDBUpdateCmd("TBL_METH_ANWSG", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptANWSGSelect.DeleteCommand = OleDBDeleteCmd("TBL_METH_ANWSG", WhereKeyID, Cncol)
      '
      '
      '
      '
      'TBL_USER_METH_ANWSG
      '
      ViewANWSGMetUser.EndInit()
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptANWSGMetUser.InsertCommand = OleDBInsertCmd("TBL_USER_METH_ANWSG", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "METH_ID"
      WhereKeyID(1) = "ANWSG_ID"
      WhereKeyID(2) = "USER_ID"
      AdaptANWSGMetUser.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_ANWSG", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptANWSGMetUser.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_ANWSG", WhereKeyID, Cncol)
      '
      'TBL_USER_METH_ANWSG_MERK
      '
      'Deletecommand
      '
      '
      ViewANWSGMetUserMerk.EndInit()
      '
      ReDim WhereKeyID(3)
      WhereKeyID(0) = "METH_ID"
      WhereKeyID(1) = "ANWSG_ID"
      WhereKeyID(2) = "USER_ID"
      WhereKeyID(3) = "MERK_ID"
      AdaptANWSGMetUserMerk.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_ANWSG_MERK", WhereKeyID, Cncol)
      '
      '
      '
      'Delete/Update/Insert TBL_USER_METH_ANWSG und TBL_METH_ANWSG
      '
      'Delete TBL_USER_METH_ANWSG_MERK (muss vor TBL_USER_METH_ANWSG gelöscht werden)
      '
      '
      ' 
      '
      AdaptANWSGMetUserMerk.Update(TblANWSGMetUserMerk.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      '
      '
      'Delete TBL_USER_METH_ANWSG
      '
      AdaptANWSGMetUser.Update(TblANWSGMetUser.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      'Delete TBL_METH_ANWSG
      '
      AdaptANWSGSelect.Update(TblANWSGSelect.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      'Insert TBL_METH_ANWSG
      '
      '
      AdaptANWSGSelect.Update(TblANWSGSelect.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Insert TBL_USER_METH_ANWSG
      '
      '
      AdaptANWSGMetUser.Update(TblANWSGMetUser.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      'Update TBL_METH_ANWSG
      '
      '
      AdaptANWSGSelect.Update(TblANWSGSelect.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_USER_METH_ANWSG
      '
      '
      AdaptANWSGMetUser.Update(TblANWSGMetUser.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
    End If
    Me.Close()
    Me.Dispose()
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


