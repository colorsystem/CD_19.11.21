Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmColorDBStructure
  Dim ier As Integer
  Dim ListOLDBCom As List(Of OleDbCommand)
  Dim Sqlstmt As String
  Dim WithID As Boolean = False
  Dim WithOrder As Boolean = False
  Dim btnTree As List(Of Button)
  Dim DialogVoidDB As OpenFileDialog
  Dim DBVoidName As String
  Dim DatNewName As String
  Dim StrMessg As String
  Dim StrMisch As String
  Dim ListID As List(Of Integer)
  Dim TabMessgeräte As DataTable
  Dim ViewMessgeräte As DataView
  Dim ViewMischsysteme As DataView
  Dim TabMischsysteme As DataTable
  Dim TblMessgeräte As DataTable
  Dim AdaptGeneral As OleDbDataAdapter
  Dim CommandNonQuery As OleDbCommand
  Private Sub frmColorDBStructure_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim Arrow As String = " ==> "
    Dim ipos As Integer
    Dim Measxxx As String
    Dim MessKlasse As String
    Me.Text = Texxt(418)
    lblDBCopy.Text = Texxt(389) & Space(1) & Texxt(141) & Space(1) & Texxt(144)
    btnVoid.Text = Texxt(144)
    btnMessg.Text = Texxt(403)
    btnMisch.Text = Texxt(406)
    btnMessgSelect.Text = Texxt(3670)
    btnMischSelect.Text = Texxt(3670)
    btnRwerte.Text = Texxt(3110) & Space(1) & Texxt(143)
    btnRezepte.Text = Texxt(730) & Space(1) & Texxt(143)
    lblShowMeasDevices.Text = Texxt(403) & Space(1) & Texxt(142)
    btnDataBase.Text = Texxt(146)
    btnVoid.Text = Texxt(144)
    btnTreeView.Text = Texxt(147)
    '
    '
    chkWithID.Text = Texxt(148)
    chkOrder.Text = Texxt(855)
    btnMessgGroupDontShow.Text = Texxt(403) & Arrow & Texxt(386) & Arrow & Texxt(14) & Space(1) & Texxt(142)
    btnMessgGroupReadonly.Text = Texxt(403) & Arrow & Texxt(386) & Arrow & Texxt(139) & Space(1) & Texxt(138)
    btnMessgMasterSlave.Text = Texxt(403) & Arrow & Texxt(156) & Arrow & Texxt(157)
    btnMessgMisch.Text = Texxt(403) & Arrow & Texxt(406)
    btnMessgWinkel.Text = Texxt(403) & Arrow & Texxt(173)
    btnMischGroupDontShow.Text = Texxt(406) & Arrow & Texxt(386) & Arrow & Texxt(14) & Space(1) & Texxt(142)
    btnMischGroupReadonly.Text = Texxt(406) & Arrow & Texxt(386) & Arrow & Texxt(139) & Space(1) & Texxt(138)
    btnMischMessg.Text = Texxt(406) & Arrow & Texxt(403)
    btnUserMessg.Text = Texxt(402) & Arrow & Texxt(403) & Arrow & Texxt(407)
    btnUserMethAnwsgMerk.Text = Texxt(402) & Arrow & Texxt(405) & Arrow & Texxt(404) & Arrow & Texxt(414)
    btnUserMethLicht.Text = Texxt(402) & Arrow & Texxt(405) & Arrow & Texxt(149)
    btnUserMethMessgWinkel.Text = Texxt(402) & Arrow & Texxt(405) & Arrow & Texxt(403) & Arrow & Texxt(173)
    btnUserMethodeMessgerätGkwert.Text = Texxt(402) & Arrow & Texxt(405) & Arrow & Texxt(403) & Arrow & Texxt(151)
    btnUserMisch.Text = Texxt(402) & Arrow & Texxt(406)
    btnUserMischMessgGKWerte.Text = Texxt(402) & Arrow & Texxt(406) & Arrow & Texxt(403) & Arrow & Texxt(151)




    DialogVoidDB = New OpenFileDialog
    AdaptGeneral = New OleDbDataAdapter
    AdaptGeneral.SelectCommand = New OleDbCommand("", Cncol)
    ListOLDBCom = New List(Of OleDbCommand)
    btnTree = New List(Of Button)
    btnTree.Add(btnMessgWinkel)
    btnTree.Add(btnUserMethMessgWinkel)
    btnTree.Add(btnUserMethAnwsgMerk)
    btnTree.Add(btnUserMethodeMessgerätGkwert)
    btnTree.Add(btnUserMethLicht)
    btnTree.Add(btnMessgMasterSlave)
    btnTree.Add(btnUserMisch)
    btnTree.Add(btnUserMischMessgGKWerte)
    btnTree.Add(btnUserMessg)
    btnTree.Add(btnMessgMisch)
    btnTree.Add(btnMischMessg)
    btnTree.Add(btnMessgGroupDontShow)
    btnTree.Add(btnMessgGroupReadonly)
    btnTree.Add(btnMischGroupDontShow)
    btnTree.Add(btnMischGroupReadonly)
    Call btnBackColor(btnTree, Nothing)
    'TreeColorStruct.CanSelect = True
    TreeColorStruct.ShowNodeToolTips = True
    '
    '
    '
    'Messgeräte-Treiber
    '
    '
    dbgMeasure00x.Columns.Add("MEASXXX", "Measxxx")
    dbgMeasure00x.Columns.Add("KLASSE", "COM_Klasse")
    dbgMeasure00x.Columns("MEASXXX").Width = 100
    dbgMeasure00x.Columns("KLASSE").Width = 300
    dbgMeasure00x.AllowUserToAddRows = False

    '
    '
    dbgMeasure00x.Rows.Clear()
    For i = 0 To 99
      Measxxx = "MEAS" & Format(i, "000")
      MessKlasse = GetPrivSettings("MEASURE", Measxxx, "", COLORFileName)
      If MessKlasse <> "" Then
        dbgMeasure00x.Rows.Add(New DataGridViewRow)
        ipos = dbgMeasure00x.Rows.Count - 1
        dbgMeasure00x.Rows(ipos).Cells("MEASXXX").Value = Measxxx
        dbgMeasure00x.Rows(ipos).Cells("KLASSE").Value = MessKlasse
      End If
    Next
    txtMessgKE.Text = GetPrivSettings("STARTUP", "MESSGKE", "", COLORFileName)
    '
    '
    '
    TblMessgeräte = New DataTable
    AdaptGeneral.SelectCommand.CommandText = "SELECT MESSG_ID,MESSGRW_ID,MESSG_KENN,MESSG_DRIVER,BEREICH_NAME,MESSG_WANF,MESSG_WEND,MESSG_REFTRA,MESSG_KBEZ FROM TBL_MESSG" _
      & " INNER JOIN TBL_BEREICH ON TBL_BEREICH.BEREICH_ID=TBL_MESSG.MESSG_NORM_FILE_ID"
    If Not FillDatset(AdaptGeneral, TblMessgeräte) Then
      Exit Sub
    End If
    TblMessgeräte.AcceptChanges()
    dbgMessgeräte.DataSource = TblMessgeräte
    dbgMessgeräte.AllowUserToDeleteRows = False
    dbgMessgeräte.AllowUserToAddRows = False
    dbgMessgeräte.ReadOnly = True
    dbgMessgeräte.Columns(dbgMessgeräte.Columns.Count - 1).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
    dbgMessgeräte.RowHeadersVisible = False
  End Sub

  Sub btnBackColor(btnTree As List(Of Button), btn As Button)
    Dim i As Integer
    For i = 0 To btnTree.Count - 1
      btnTree(i).BackColor = Color.LightGray
    Next
    If IsNothing(btn) Then Exit Sub
    btn.BackColor = Color.Azure
  End Sub
  Sub CreateTree(ByVal ListOLDBCom As List(Of OleDbCommand), ByVal Knoten As TreeNodeCollection, WithID As Boolean, ByRef ier As Integer)
    Dim i As Integer
    Dim iee As Integer
    Dim Fieldcount As Integer
    Dim KnotName As String
    Dim Datread As OleDbDataReader
    Dim ListSlave As List(Of OleDbCommand)
    If ListOLDBCom.Count = 0 Then Exit Sub
    ListSlave = New List(Of OleDbCommand)
    For i = 1 To ListOLDBCom.Count - 1
      ListSlave.Add(ListOLDBCom(i))
    Next
    Datread = DataRead(ListOLDBCom(0), CommandBehavior.Default, Cncol)
    Fieldcount = Datread.FieldCount
    Do While Datread.Read
      If IsDBNull(Datread(0)) Then
        KnotName = ""
      Else
        KnotName = Datread(0)
      End If
      If WithID Then
        KnotName = "(" & Datread(Fieldcount - 1) & ") " & KnotName
      End If
      Knoten.Add(KnotName)
      Knoten(Knoten.Count - 1).ToolTipText = Datread.GetName(0)
      If ListSlave.Count = 0 Then Continue Do
      For i = 0 To ListSlave(0).Parameters.Count - 1
        ListSlave(0).Parameters(i).Value = Datread(i + 1)
      Next

      Call CreateTree(ListSlave, Knoten(Knoten.Count - 1).Nodes, WithID, iee)
    Loop
    Datread.Close()
  End Sub
  Private Sub chkWithID_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkWithID.CheckedChanged
    WithID = chkWithID.Checked
  End Sub
  Private Sub chkOrder_Click(sender As Object, e As System.EventArgs) Handles chkOrder.Click
    WithOrder = chkOrder.Checked
  End Sub
  Function NewSqlstmt(ByVal SqlStmt As String, StrName As String, withorder As Boolean)
    NewSqlstmt = "SELECT " & StrName & "," & SqlStmt
    If withorder Then
      NewSqlstmt = NewSqlstmt & " ORDER BY " & StrName
    End If
  End Function
  Private Sub btnMessgWinkel_Click(sender As Object, e As System.EventArgs) Handles btnMessgWinkel.Click
    Call btnBackColor(btnTree, sender)

    ListOLDBCom.Clear()
    Sqlstmt = "MESSG_ID  FROM TBL_MESSG"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MESSG_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    '
    '
    Sqlstmt = "TBL_MESSG_IHRM.MESSG_ID,TBL_IHRM.IHRM_ID FROM TBL_IHRM INNER JOIN TBL_MESSG_IHRM ON TBL_IHRM.IHRM_ID=TBL_MESSG_IHRM.IHRM_ID WHERE TBL_MESSG_IHRM.MESSG_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "IHRM_BEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("MESSG_ID", OleDbType.Integer)
    '
    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)

  End Sub
  Private Sub btnUserMethMessgWinkel_Click(sender As System.Object, e As System.EventArgs) Handles btnUserMethMessgWinkel.Click
    '
    'User Methode Messgeräte Lichtart

    Call btnBackColor(btnTree, sender)

    ListOLDBCom.Clear()
    Sqlstmt = "USER_ID  FROM TBL_USER"
    Sqlstmt = NewSqlstmt(Sqlstmt, "USER_NAME", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    'Methoden
    '
    Sqlstmt = "TBL_USER_METH.USER_ID,TBL_METH.METH_ID FROM TBL_METH INNER JOIN TBL_USER_METH ON TBL_METH.METH_ID=TBL_USER_METH.METH_ID WHERE TBL_USER_METH.USER_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "METH_BEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("USER_ID", OleDbType.Integer)
    '
    '
    'Messgeräte
    '

    Sqlstmt = "TBL_USER_METH_MESSG.USER_ID,TBL_USER_METH_MESSG.METH_ID,TBL_USER_METH_MESSG.MESSG_ID FROM " _
   & " (TBL_MESSG INNER JOIN TBL_USER_METH_MESSG ON TBL_USER_METH_MESSG.MESSG_ID=TBL_MESSG.MESSG_ID)" _
   & " WHERE TBL_USER_METH_MESSG.USER_ID=? AND TBL_USER_METH_MESSG.METH_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MESSG_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(2).Parameters.Add("USER_ID", OleDbType.Integer)
    ListOLDBCom(2).Parameters.Add("METH_ID", OleDbType.Integer)
    '
    '
    '
    '
    '
    'Winkel/Messgeometrien
    '


    Sqlstmt = "TBL_USER_METH_MESSG_IHRM.USER_ID,TBL_USER_METH_MESSG_IHRM.METH_ID,TBL_USER_METH_MESSG_IHRM.MESSG_ID,TBL_USER_METH_MESSG_IHRM.IHRM_ID  FROM " _
     & " (TBL_IHRM INNER JOIN TBL_USER_METH_MESSG_IHRM ON TBL_USER_METH_MESSG_IHRM.IHRM_ID=TBL_IHRM.IHRM_ID)" _
     & " WHERE TBL_USER_METH_MESSG_IHRM.USER_ID=? AND TBL_USER_METH_MESSG_IHRM.METH_ID=? AND TBL_USER_METH_MESSG_IHRM.MESSG_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "IHRM_BEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(3).Parameters.Add("USER_ID", OleDbType.Integer)
    ListOLDBCom(3).Parameters.Add("METH_ID", OleDbType.Integer)
    ListOLDBCom(3).Parameters.Add("MESSG_ID", OleDbType.Integer)
    '

    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub
  Private Sub btnUserMethodeMessgerätGkwert_Click(sender As System.Object, e As System.EventArgs) Handles btnUserMethodeMessgerätGkwert.Click
    '
    'User Methode Messgeräte Lichtart

    Call btnBackColor(btnTree, sender)

    ListOLDBCom.Clear()
    Sqlstmt = "USER_ID  FROM TBL_USER"
    Sqlstmt = NewSqlstmt(Sqlstmt, "USER_NAME", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    'Methoden
    '
    Sqlstmt = "TBL_USER_METH.USER_ID,TBL_METH.METH_ID FROM TBL_METH INNER JOIN TBL_USER_METH ON TBL_METH.METH_ID=TBL_USER_METH.METH_ID WHERE TBL_USER_METH.USER_ID=? AND TBL_METH.METH_ID<50"
    Sqlstmt = NewSqlstmt(Sqlstmt, "METH_BEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("USER_ID", OleDbType.Integer)
    '
    '
    'Messgeräte
    '

    Sqlstmt = "TBL_USER_METH_MESSG.USER_ID,TBL_USER_METH_MESSG.METH_ID AS METH_ID,TBL_USER_METH_MESSG.MESSG_ID FROM " _
   & " (TBL_MESSG INNER JOIN TBL_USER_METH_MESSG ON TBL_USER_METH_MESSG.MESSG_ID=TBL_MESSG.MESSG_ID)" _
   & " WHERE TBL_USER_METH_MESSG.USER_ID=? AND TBL_USER_METH_MESSG.METH_ID=? AND TBL_USER_METH_MESSG.METH_ID<50"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MESSG_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(2).Parameters.Add("USER_ID", OleDbType.Integer)
    ListOLDBCom(2).Parameters.Add("METH_ID", OleDbType.Integer)
    '
    '
    '
    '
    '
    'GKWerte
    '


    Sqlstmt = "TBL_USER_METH_MESSG.USER_ID,TBL_USER_METH_MESSG.METH_ID AS METH_ID,TBL_USER_METH_MESSG.MESSG_ID,TBL_USER_METH_MESSG.GKWRT_ID FROM " _
     & " (TBL_GKWRT INNER JOIN TBL_USER_METH_MESSG ON TBL_USER_METH_MESSG.GKWRT_ID=TBL_GKWRT.GKWRT_ID)" _
     & " WHERE TBL_USER_METH_MESSG.USER_ID=? AND TBL_USER_METH_MESSG.METH_ID=? AND TBL_USER_METH_MESSG.MESSG_ID=? AND TBL_USER_METH_MESSG.METH_ID<50"
    Sqlstmt = NewSqlstmt(Sqlstmt, "GKWRT_BEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(3).Parameters.Add("USER_ID", OleDbType.Integer)
    ListOLDBCom(3).Parameters.Add("METH_ID", OleDbType.Integer)
    ListOLDBCom(3).Parameters.Add("MESSG_ID", OleDbType.Integer)
    '

    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)

  End Sub
  Private Sub btnUserMethAnwsgMerk_Click(sender As Object, e As System.EventArgs) Handles btnUserMethAnwsgMerk.Click
    '
    'User Methode Messgeräte Lichtart

    Call btnBackColor(btnTree, sender)

    ListOLDBCom.Clear()
    Sqlstmt = "USER_ID  FROM TBL_USER"
    Sqlstmt = NewSqlstmt(Sqlstmt, "USER_NAME", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    'Methoden
    '
    Sqlstmt = "TBL_USER_METH.USER_ID,TBL_METH.METH_ID FROM TBL_METH INNER JOIN TBL_USER_METH ON TBL_METH.METH_ID=TBL_USER_METH.METH_ID WHERE TBL_USER_METH.USER_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "METH_BEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("USER_ID", OleDbType.Integer)
    '
    '
    'Anweisuingen
    '
    Sqlstmt = "TBL_USER_METH_ANWSG.USER_ID,TBL_USER_METH_ANWSG.METH_ID,TBL_USER_METH_ANWSG.ANWSG_ID FROM " _
   & " (TBL_ANWSG INNER JOIN TBL_USER_METH_ANWSG ON TBL_USER_METH_ANWSG.ANWSG_ID=TBL_ANWSG.ANWSG_ID)" _
   & " WHERE TBL_USER_METH_ANWSG.USER_ID=? AND TBL_USER_METH_ANWSG.METH_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "ANWSG_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(2).Parameters.Add("USER_ID", OleDbType.Integer)
    ListOLDBCom(2).Parameters.Add("METH_ID", OleDbType.Integer)
    '
    '
    '
    '
    '
    'Merkmale
    '
    Sqlstmt = "TBL_USER_METH_ANWSG_MERK.USER_ID,TBL_USER_METH_ANWSG_MERK.METH_ID,TBL_USER_METH_ANWSG_MERK.ANWSG_ID,TBL_USER_METH_ANWSG_MERK.MERK_ID FROM " _
     & " (TBL_MERK INNER JOIN TBL_USER_METH_ANWSG_MERK ON TBL_USER_METH_ANWSG_MERK.MERK_ID=TBL_MERK.MERK_ID)" _
     & " WHERE TBL_USER_METH_ANWSG_MERK.USER_ID=? AND TBL_USER_METH_ANWSG_MERK.METH_ID=? AND TBL_USER_METH_ANWSG_MERK.ANWSG_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "TBL_MERK.MERK_KBEZ AS MERK_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(3).Parameters.Add("USER_ID", OleDbType.Integer)
    ListOLDBCom(3).Parameters.Add("METH_ID", OleDbType.Integer)
    ListOLDBCom(3).Parameters.Add("ANWSG_ID", OleDbType.Integer)
    '

    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub
  Private Sub btnUserMethLicht_Click(sender As Object, e As System.EventArgs) Handles btnUserMethLicht.Click
    '
    'User Methode Messgeräte Lichtart

    Call btnBackColor(btnTree, sender)

    ListOLDBCom.Clear()
    Sqlstmt = "USER_ID  FROM TBL_USER"
    Sqlstmt = NewSqlstmt(Sqlstmt, "USER_NAME", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    'Methoden
    '
    Sqlstmt = "TBL_USER_METH.USER_ID,TBL_METH.METH_ID FROM TBL_METH INNER JOIN TBL_USER_METH ON TBL_METH.METH_ID=TBL_USER_METH.METH_ID WHERE TBL_USER_METH.USER_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "METH_BEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("USER_ID", OleDbType.Integer)
    '
    '

    '
    '
    '
    '
    'Lichtart

    Sqlstmt = "TBL_USER_METH_LICHT.USER_ID,TBL_USER_METH_LICHT.METH_ID,TBL_USER_METH_LICHT.LICHT_ID FROM " _
     & " (TBL_LICHT INNER JOIN TBL_USER_METH_LICHT ON TBL_USER_METH_LICHT.LICHT_ID=TBL_LICHT.LICHT_ID)" _
     & " WHERE TBL_USER_METH_LICHT.USER_ID=? AND TBL_USER_METH_LICHT.METH_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "LICHT_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(2).Parameters.Add("USER_ID", OleDbType.Integer)
    ListOLDBCom(2).Parameters.Add("METH_ID", OleDbType.Integer)

    '

    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub
  Private Sub btnMessgMasterSlave_Click(sender As Object, e As System.EventArgs) Handles btnMessgMasterSlave.Click
    Call btnBackColor(btnTree, sender)
    ListOLDBCom.Clear()

    '
    '
    Sqlstmt = "MESSGRW_ID FROM TBL_MESSG WHERE MESSG_ID=MESSGRW_ID "
    Sqlstmt = NewSqlstmt(Sqlstmt, "MESSG_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    '
    Sqlstmt = "MESSGRW_ID,MESSG_ID FROM TBL_MESSG WHERE (NOT MESSG_ID=MESSGRW_ID) AND MESSGRW_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MESSG_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("MESSGRW_ID", OleDbType.Integer)

    '
    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub

  Private Sub btnUserMisch_Click(sender As System.Object, e As System.EventArgs) Handles btnUserMisch.Click
    Call btnBackColor(btnTree, sender)
    ListOLDBCom.Clear()
    Sqlstmt = "USER_ID  FROM TBL_USER"
    Sqlstmt = NewSqlstmt(Sqlstmt, "USER_NAME", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    '
    '
    Sqlstmt = "TBL_USER_MISCH.USER_ID,TBL_MISCH.MISCH_ID FROM TBL_MISCH INNER JOIN TBL_USER_MISCH ON TBL_MISCH.MISCH_ID=TBL_USER_MISCH.MISCH_ID WHERE TBL_USER_MISCH.USER_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MISCH_KBEZ", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("USER_ID", OleDbType.Integer)
    '
    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub

  Private Sub btnUserMessg_Click(sender As System.Object, e As System.EventArgs) Handles btnUserMessg.Click
    Call btnBackColor(btnTree, sender)
    ListOLDBCom.Clear()
    Sqlstmt = "USER_ID  FROM TBL_USER"
    Sqlstmt = NewSqlstmt(Sqlstmt, "USER_NAME", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    '
    '
    '
    Sqlstmt = "TBL_USER_MESSG.USER_ID,TBL_MESSG.MESSG_ID FROM TBL_MESSG INNER JOIN TBL_USER_MESSG ON TBL_MESSG.MESSG_ID=TBL_USER_MESSG.MESSG_ID WHERE TBL_USER_MESSG.USER_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MESSG_KBEZ", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("USER_ID", OleDbType.Integer)
    '
    '
    '
    '
    '
    Sqlstmt = "TBL_USER_MESSG.USER_ID,TBL_USER_MESSG.MESSG_ID,TBL_USER_MESSG.MESSG_LABOR_ID FROM " _
      & " (TBL_LABOR INNER JOIN TBL_USER_MESSG ON TBL_LABOR.LABOR_ID=TBL_USER_MESSG.MESSG_LABOR_ID)" _
      & " WHERE TBL_USER_MESSG.USER_ID=? AND TBL_USER_MESSG.MESSG_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "LABOR_KBEZ", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(2).Parameters.Add("USER_ID", OleDbType.Integer)
    ListOLDBCom(2).Parameters.Add("MESSG_ID", OleDbType.Integer)
    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub
  Private Sub btnMischMessg_Click(sender As System.Object, e As System.EventArgs) Handles btnMischMessg.Click
    Call btnBackColor(btnTree, sender)
    ListOLDBCom.Clear()
    '
    '
    Sqlstmt = "MISCH_ID FROM TBL_MISCH"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MISCH_KBEZ", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    '
    Sqlstmt = "TBL_MISCH_MESSG.MISCH_ID,TBL_MESSG.MESSGRW_ID FROM TBL_MESSG INNER JOIN TBL_MISCH_MESSG ON TBL_MESSG.MESSG_ID=TBL_MISCH_MESSG.MESSG_ID " _
      & "WHERE TBL_MISCH_MESSG.MISCH_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MESSG_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("MISCH_ID", OleDbType.Integer)
    '
    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub

  Private Sub btnMessgMisch_Click(sender As System.Object, e As System.EventArgs) Handles btnMessgMisch.Click
    Call btnBackColor(btnTree, sender)
    ListOLDBCom.Clear()
    Sqlstmt = "MESSG_ID FROM TBL_MESSG"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MESSG_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    '
    '
    Sqlstmt = "TBL_MISCH_MESSG.MESSG_ID,TBL_MISCH_MESSG.MISCH_ID FROM TBL_MISCH INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID=TBL_MISCH_MESSG.MISCH_ID WHERE TBL_MISCH_MESSG.MESSG_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MISCH_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("MESSG_ID", OleDbType.Integer)
    '
    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub



  Private Sub btnMessgGroupDontShow_Click(sender As System.Object, e As System.EventArgs) Handles btnMessgGroupDontShow.Click
    Call btnBackColor(btnTree, sender)
    ListOLDBCom.Clear()
    Sqlstmt = "MESSG_ID FROM TBL_MESSG"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MESSG_KBEZ", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    '
    '
    Sqlstmt = "TBL_MESSG_GROUP.MESSG_ID,TBL_MESSG_GROUP.GROUP_ID FROM TBL_MESSG INNER JOIN TBL_MESSG_GROUP ON TBL_MESSG.MESSG_ID=TBL_MESSG_GROUP.MESSG_ID WHERE TBL_MESSG_GROUP.MESSG_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "GROUP_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("MESSG_ID", OleDbType.Integer)
    '
    '
    Sqlstmt = "TBL_MESSG_GROUP.MESSG_ID,TBL_MESSG_GROUP.GROUP_ID,TBL_USER_MESSG_GROUP_DONTSHOW.USER_ID FROM " _
     & "(TBL_MESSG_GROUP INNER JOIN TBL_USER_MESSG_GROUP_DONTSHOW ON " _
     & "(TBL_MESSG_GROUP.MESSG_ID=TBL_USER_MESSG_GROUP_DONTSHOW.MESSG_ID AND TBL_MESSG_GROUP.GROUP_ID=TBL_USER_MESSG_GROUP_DONTSHOW.GROUP_ID)) " _
     & "INNER JOIN TBL_USER ON TBL_USER_MESSG_GROUP_DONTSHOW.USER_ID=TBL_USER.USER_ID " _
     & "WHERE TBL_USER_MESSG_GROUP_DONTSHOW.MESSG_ID=? AND TBL_USER_MESSG_GROUP_DONTSHOW.GROUP_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "USER_NAME", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(2).Parameters.Add("MESSG_ID", OleDbType.Integer)
    ListOLDBCom(2).Parameters.Add("GROUP_ID", OleDbType.Integer)
    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub

  Private Sub btnMessgGroupReadonly_Click(sender As Object, e As System.EventArgs) Handles btnMessgGroupReadonly.Click
    Call btnBackColor(btnTree, sender)
    ListOLDBCom.Clear()
    Sqlstmt = "MESSG_ID FROM TBL_MESSG"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MESSG_KBEZ", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    '
    '
    Sqlstmt = "TBL_MESSG_GROUP.MESSG_ID,TBL_MESSG_GROUP.GROUP_ID FROM TBL_MESSG INNER JOIN TBL_MESSG_GROUP ON TBL_MESSG.MESSG_ID=TBL_MESSG_GROUP.MESSG_ID WHERE TBL_MESSG_GROUP.MESSG_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "GROUP_KBEZ", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("MESSG_ID", OleDbType.Integer)
    '
    '
    Sqlstmt = "TBL_MESSG_GROUP.MESSG_ID,TBL_MESSG_GROUP.GROUP_ID,TBL_USER_MESSG_GROUP_READONLY.USER_ID FROM " _
     & "(TBL_MESSG_GROUP INNER JOIN TBL_USER_MESSG_GROUP_READONLY ON " _
     & "(TBL_MESSG_GROUP.MESSG_ID=TBL_USER_MESSG_GROUP_READONLY.MESSG_ID AND TBL_MESSG_GROUP.GROUP_ID=TBL_USER_MESSG_GROUP_READONLY.GROUP_ID)) " _
     & "INNER JOIN TBL_USER ON TBL_USER_MESSG_GROUP_READONLY.USER_ID=TBL_USER.USER_ID " _
     & "WHERE TBL_USER_MESSG_GROUP_READONLY.MESSG_ID=? AND TBL_USER_MESSG_GROUP_READONLY.GROUP_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "USER_NAME", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(2).Parameters.Add("MESSG_ID", OleDbType.Integer)
    ListOLDBCom(2).Parameters.Add("GROUP_ID", OleDbType.Integer)
    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub



  Private Sub btnMischGroupDontShow_Click(sender As System.Object, e As System.EventArgs) Handles btnMischGroupDontShow.Click
    Call btnBackColor(btnTree, sender)
    ListOLDBCom.Clear()
    Sqlstmt = "MISCH_ID FROM TBL_MISCH"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MISCH_KBEZ", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    '
    '
    Sqlstmt = "TBL_MISCH_GROUP.MISCH_ID,TBL_MISCH_GROUP.GROUP_ID FROM TBL_MISCH INNER JOIN TBL_MISCH_GROUP ON TBL_MISCH.MISCH_ID=TBL_MISCH_GROUP.MISCH_ID WHERE TBL_MISCH_GROUP.MISCH_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "GROUP_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("MISCH_ID", OleDbType.Integer)
    '
    '
    Sqlstmt = "TBL_USER_MISCH_GROUP_DONTSHOW.MISCH_ID,TBL_USER_MISCH_GROUP_DONTSHOW.GROUP_ID,TBL_USER_MISCH_GROUP_DONTSHOW.USER_ID FROM " _
    & " TBL_USER INNER JOIN TBL_USER_MISCH_GROUP_DONTSHOW ON  TBL_USER.USER_ID=TBL_USER_MISCH_GROUP_DONTSHOW.USER_ID" _
    & " WHERE TBL_USER_MISCH_GROUP_DONTSHOW.MISCH_ID=? AND TBL_USER_MISCH_GROUP_DONTSHOW.GROUP_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "USER_NAME", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(2).Parameters.Add("MISCH_ID", OleDbType.Integer)
    ListOLDBCom(2).Parameters.Add("GROUP_ID", OleDbType.Integer)
    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub

  Private Sub btnMischGroupReadonly_Click(sender As Object, e As System.EventArgs) Handles btnMischGroupReadonly.Click
    Call btnBackColor(btnTree, sender)
    ListOLDBCom.Clear()
    Sqlstmt = "MISCH_ID FROM TBL_MISCH"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MISCH_KBEZ", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    '
    '
    Sqlstmt = "TBL_MISCH_GROUP.MISCH_ID,TBL_MISCH_GROUP.GROUP_ID FROM TBL_MISCH INNER JOIN TBL_MISCH_GROUP ON TBL_MISCH.MISCH_ID=TBL_MISCH_GROUP.MISCH_ID WHERE TBL_MISCH_GROUP.MISCH_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "GROUP_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("MISCH_ID", OleDbType.Integer)
    '
    '
    Sqlstmt = "TBL_USER_MISCH_GROUP_READONLY.MISCH_ID,TBL_USER_MISCH_GROUP_READONLY.GROUP_ID,TBL_USER_MISCH_GROUP_READONLY.USER_ID FROM " _
    & " TBL_USER INNER JOIN TBL_USER_MISCH_GROUP_READONLY ON  TBL_USER.USER_ID=TBL_USER_MISCH_GROUP_READONLY.USER_ID" _
    & " WHERE TBL_USER_MISCH_GROUP_READONLY.MISCH_ID=? AND TBL_USER_MISCH_GROUP_READONLY.GROUP_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "USER_NAME", WithOrder)

    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(2).Parameters.Add("MISCH_ID", OleDbType.Integer)
    ListOLDBCom(2).Parameters.Add("GROUP_ID", OleDbType.Integer)
    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub

  Private Sub btnUserMischMessgGKWerte_Click(sender As Object, e As System.EventArgs) Handles btnUserMischMessgGKWerte.Click
    '
    'User Methode Messgeräte Lichtart

    Call btnBackColor(btnTree, sender)

    ListOLDBCom.Clear()
    Sqlstmt = "USER_ID  FROM TBL_USER"
    Sqlstmt = NewSqlstmt(Sqlstmt, "USER_NAME", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    '
    '
    'Mischsysteme
    '
    Sqlstmt = "TBL_USER_MISCH.USER_ID,TBL_MISCH.MISCH_ID FROM TBL_MISCH INNER JOIN TBL_USER_MISCH ON TBL_MISCH.MISCH_ID=TBL_USER_MISCH.MISCH_ID WHERE TBL_USER_MISCH.USER_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MISCH_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(1).Parameters.Add("USER_ID", OleDbType.Integer)
    '
    '
    'Messgeräte
    '

    Sqlstmt = "TBL_USER_MISCH_MESSG.USER_ID,TBL_USER_MISCH_MESSG.MISCH_ID,TBL_USER_MISCH_MESSG.MESSG_ID FROM " _
   & " (TBL_MESSG INNER JOIN TBL_USER_MISCH_MESSG ON TBL_USER_MISCH_MESSG.MESSG_ID=TBL_MESSG.MESSG_ID)" _
   & " WHERE TBL_USER_MISCH_MESSG.USER_ID=? AND TBL_USER_MISCH_MESSG.MISCH_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "MESSG_KBEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(2).Parameters.Add("USER_ID", OleDbType.Integer)
    ListOLDBCom(2).Parameters.Add("MISCH_ID", OleDbType.Integer)
    '
    '
    '
    '
    '
    'GKWerte
    '


    Sqlstmt = "TBL_USER_MISCH_MESSG.USER_ID,TBL_USER_MISCH_MESSG.MISCH_ID,TBL_USER_MISCH_MESSG.MESSG_ID,TBL_USER_MISCH_MESSG.GKWRT_ID  FROM " _
     & " (TBL_GKWRT INNER JOIN TBL_USER_MISCH_MESSG ON TBL_USER_MISCH_MESSG.GKWRT_ID=TBL_GKWRT.GKWRT_ID)" _
     & " WHERE TBL_USER_MISCH_MESSG.USER_ID=? AND TBL_USER_MISCH_MESSG.MISCH_ID=? AND TBL_USER_MISCH_MESSG.MESSG_ID=?"
    Sqlstmt = NewSqlstmt(Sqlstmt, "GKWRT_BEZ", WithOrder)
    ListOLDBCom.Add(New OleDbCommand(Sqlstmt, Cncol))
    ListOLDBCom(3).Parameters.Add("USER_ID", OleDbType.Integer)
    ListOLDBCom(3).Parameters.Add("MISCH_ID", OleDbType.Integer)
    ListOLDBCom(3).Parameters.Add("MESSG_ID", OleDbType.Integer)
    '

    '
    '
    TreeColorStruct.Nodes.Clear()
    Call CreateTree(ListOLDBCom, TreeColorStruct.Nodes, WithID, ier)
  End Sub


  Private Sub btnTreeView_Click(sender As System.Object, e As System.EventArgs) Handles btnTreeView.Click
    splColorTreeStruct.Visible = True
    splColorDataBase.Visible = False
  End Sub

  Private Sub btnDataBase_Click(sender As Object, e As System.EventArgs) Handles btnDataBase.Click
    splColorTreeStruct.Visible = False
    splColorDataBase.Visible = True
  End Sub

  Private Sub btnVoid_Click(sender As System.Object, e As System.EventArgs) Handles btnVoid.Click
    btnMessg.Enabled = False
    btnMisch.Enabled = False
    btnMischSelect.Enabled = False
    btnRwerte.Enabled = False
    btnMessgSelect.Enabled = False
    btnRezepte.Enabled = False
    DBVoidName = GetFileProfile("%PROGRAMFILES%/COLORAPPLPROG/COLORDATAVOID.MDB")
    If Not File.Exists(DBVoidName) Then
      MsgBox(DBVoidName & "  nicht vorhanden")
      Exit Sub
    Else
      DatNewName = NewFileName(Cndat.DataSource, "COLORZWIDATA.MDB")
      DatNewName = InputBox("Neue Datenbank", Texxt(2000), DatNewName)
      If DatNewName = "" Then
        Exit Sub
      End If
    End If
    '
    'Datenbank COLORDATAVOID.MDB kopieren
    '
    '
    If File.Exists(DatNewName) Then
      If MsgBox(Texxt(3050) & ": " & DatNewName & " " & Texxt(3051) & vbCrLf & Texxt(3680) & "?", MsgBoxStyle.YesNo, Texxt(2000)) = MsgBoxResult.Yes Then
        File.Delete(DatNewName)
      Else
        Exit Sub
      End If
    End If
    File.Copy(DBVoidName, DatNewName)
    '
    '
    btnMessg.Enabled = True
    '
    '
  End Sub



  Private Sub btnMessg_Click(sender As Object, e As System.EventArgs) Handles btnMessg.Click
    TabMessgeräte = New DataTable
    AdaptGeneral.SelectCommand.CommandText = "SELECT MESSGRW_ID,MESSG_KBEZ FROM TBL_MESSG WHERE MESSG_ID=MESSGRW_ID ORDER BY MESSG_KBEZ"
    If Not FillDatset(AdaptGeneral, TabMessgeräte) Then
      Exit Sub
    End If
    lstMessg.DataSource = TabMessgeräte
    lstMessg.ValueMember = "MESSGRW_ID"
    lstMessg.DisplayMember = "MESSG_KBEZ"
    btnRwerte.Enabled = True
    btnMessgSelect.Enabled = True
  End Sub

  Private Sub btnRwerte_Click(sender As System.Object, e As System.EventArgs) Handles btnRwerte.Click
    Dim i As Integer
    Dim SqlStmt As String
    Cursor = Cursors.WaitCursor
    ListID = New List(Of Integer)
    For i = 0 To lstMessg.SelectedItems.Count - 1
      ListID.Add(lstMessg.SelectedItems(i)("MESSGRW_ID"))
    Next
    StrMessg = StrLin(ListID)
    SqlStmt = "INSERT INTO TBL_RWERT IN '" & DatNewName & "' SELECT * FROM TBL_RWERT WHERE MESSGRW_ID IN " & StrMessg
    CommandNonQuery = New OleDbCommand(SqlStmt, Cndat)
    If SQLNonQuery(CommandNonQuery, Cndat) Then
      Exit Sub
    End If
    SqlStmt = "INSERT INTO TBL_QUALI IN '" & DatNewName & "' SELECT * FROM TBL_QUALI WHERE MESSGRW_ID IN " & StrMessg
    CommandNonQuery = New OleDbCommand(SqlStmt, Cndat)
    If SQLNonQuery(CommandNonQuery, Cndat) Then
      Exit Sub
    End If
    btnMisch.Enabled = True
    Cursor = Cursors.Default
    MsgBox("R-Werte sind übertragen")

  End Sub

  Private Sub btnMisch_Click(sender As Object, e As System.EventArgs) Handles btnMisch.Click
    TabMischsysteme = New DataTable
    AdaptGeneral.SelectCommand.CommandText = "SELECT TBL_MISCH.MISCH_ID,MISCH_KBEZ FROM TBL_MISCH ORDER BY MISCH_KBEZ"
    If Not FillDatset(AdaptGeneral, TabMischsysteme) Then
      Exit Sub
    End If
    lstMisch.DataSource = TabMischsysteme
    lstMisch.ValueMember = "MISCH_ID"
    lstMisch.DisplayMember = "MISCH_KBEZ"
    btnMischSelect.Enabled = True
    btnRezepte.Enabled = True
  End Sub

  Private Sub btnRezepte_Click(sender As Object, e As System.EventArgs) Handles btnRezepte.Click
    Dim i As Integer
    Dim TabName() As String = {"TBL_FARBM", "TBL_FARBM_PREIS", "TBL_FARBM_PROZ", "TBL_FARBM_PROB", _
      "TBL_REZEPT", "TBL_REZEPT_FARBM", "TBL_REZEPT_RWERT", "TBL_SORTI", "TBL_SORTI_FARBM", "TBL_SORTI_RWERT", "TBL_GRUND_FARBM"}
    Dim SqlStmt As String
    ListID = New List(Of Integer)
    For i = 0 To lstMisch.SelectedItems.Count - 1
      ListID.Add(lstMisch.SelectedItems(i)("MISCH_ID"))
    Next
    StrMisch = StrLin(ListID)
    Cursor = Cursors.WaitCursor
    For i = 0 To TabName.Count - 1
      If TabName(i) = "TBL_REZEPT_RWERT" Or TabName(i) = "TBL_SORTI_RWERT" Or TabName(i) = "TBL_GRUND_FARBM" Then
        SqlStmt = "INSERT INTO " & TabName(i) & " IN '" & DatNewName & "' SELECT * FROM " & TabName(i) _
          & " WHERE MISCH_ID IN " & StrMisch & " AND MESSGRW_ID IN " & StrMessg
      Else
        SqlStmt = "INSERT INTO " & TabName(i) & " IN '" & DatNewName & "' SELECT * FROM " & TabName(i) _
          & " WHERE MISCH_ID IN " & StrMisch
      End If
      CommandNonQuery = New OleDbCommand(SqlStmt, Cndat)
      If SQLNonQuery(CommandNonQuery, Cndat) Then
        Exit Sub
      End If
    Next
    Cursor = Cursors.Default
    MsgBox("Rezepte und Sortimente sind übertragen")
  End Sub


  Private Sub btnMessgSelect_Click(sender As Object, e As System.EventArgs) Handles btnMessgSelect.Click
    Dim i As Integer
    For i = 0 To lstMessg.Items.Count - 1
      lstMessg.SetSelected(i, True)
    Next i
  End Sub

  Private Sub btnMischSelect_Click(sender As Object, e As System.EventArgs) Handles btnMischSelect.Click
    Dim i As Integer
    For i = 0 To lstMisch.Items.Count - 1
      lstMisch.SetSelected(i, True)
    Next i
  End Sub


End Class