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
    Me.Text = Texxt(418)
    lblTreeView.Text = Texxt(147) & Space(1) & Texxt(145)
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


 
  Sub btnBackColor(btnTree As List(Of Button), btn As Button)
    Dim i As Integer
    For i = 0 To btnTree.Count - 1
      btnTree(i).BackColor = Color.LightGray
    Next
    If IsNothing(btn) Then Exit Sub
    btn.BackColor = Color.Azure
  End Sub



End Class