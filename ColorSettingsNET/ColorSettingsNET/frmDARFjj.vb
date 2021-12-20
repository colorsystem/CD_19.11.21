Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmDARF
  Dim imsg As Integer
  Dim ier As Integer
  Dim Datn As String
  Dim i As Integer
  Dim j As Integer
  Dim k As Integer
  Dim kw As Integer
  Dim nkw As Integer
  Dim Kanz As Integer
  Dim DbName As String
  Dim AdaptMess As OleDbDataAdapter
  Dim CmdMess As OleDbCommand
  Dim TabMess As DataTable
  Dim AdaptGroup As OleDbDataAdapter
  Dim Cmdgroup As OleDbCommand
  Dim TabGroup As DataTable
  Dim AdaptRwert As OleDbDataAdapter
  Dim CmdRwert As OleDbCommand
  Dim TabRwert As DataTable
  Dim AdaptWerte_2 As OleDbDataAdapter
  Dim CmdWerte_2 As OleDbCommand
  Dim TabWerte_2 As DataTable
  Dim AdaptWerte_3 As OleDbDataAdapter
  Dim CmdWerte_3 As OleDbCommand
  Dim TabWerte_3 As DataTable
  '
  Dim TabWerte As List(Of DataTable)


  Dim Strgid As String
  Dim SqlStmt As String
  Dim SqlEtmt As String
  'Dim Relat As Relation
  Dim ForeignTabNam() As String
  Dim ForTabCount As Integer
  Dim Garray() As Object
  'Dim TabRwert As Recordset
  Dim RowWrt As Object
  Dim MaxClipBoard As Long
  Dim FilNaa As String
  Dim TblRwertList As String
  Dim TblRwertListNo As String
  '
  '
  '
  Dim cbogrp As List(Of ComboBox)
  Dim lblGRP As List(Of Label)
  Dim chkARC As List(Of CheckBox)
  Dim btnMARK As List(Of Button)
  Dim btnZEIG As List(Of Button)
  Dim btnRUN As List(Of Button)
  Dim btnCLIP As List(Of Button)
  Dim dbgREF As List(Of DataGridView)
  Dim grdWRT As List(Of DataGridView)
  Dim lblANZ As List(Of Label)
  Dim txtANZ As List(Of TextBox)
  Dim chkDAT As List(Of CheckBox)
  Dim lblVON As List(Of Label)
  Dim txtVON As List(Of TextBox)
  Dim lblBIS As List(Of Label)
  Dim txtbis As List(Of TextBox)
  Dim lblSQL As List(Of Label)
  Dim txtSQL As List(Of TextBox)
  Dim lblKENN As List(Of Label)
  Dim txtKENN As List(Of TextBox)
  '
  '
  '
  Dim GrpRwerte As RefValuesGrp
  Dim ReWrRwert As ReadWriteRwert
  Dim RezSozpt As Recipes
  Dim RwWrRezept As ReadWriteRezept
  Dim MischGroup As HandleRezGroup
  Dim HandleRwrt As HandleRwerte
  Dim VKwb(2) As Integer
  Dim RefWert As RefValue
  Dim RefID As Long


  Private Sub frmDARF_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    '
    '
    '
    cbogrp = New List(Of ComboBox)
    cbogrp.Add(cboGRP_0)
    cbogrp.Add(cboGRP_1)
    cbogrp.Add(cboGRP_2)
    cbogrp.Add(cboGRP_3)
    cbogrp.Add(cboGRP_4)
    '
    '
    '
    lblGRP = New List(Of Label)
    lblGRP.Add(lblGRP_0)
    lblGRP.Add(lblGRP_1)
    lblGRP.Add(lblGRP_2)
    lblGRP.Add(lblGRP_3)
    lblGRP.Add(lblGRP_4)
    '
    '
    '
    chkARC = New List(Of CheckBox)
    chkARC.Add(Nothing)
    chkARC.Add(chkARC_1)
    chkARC.Add(chkARC_2)
    chkARC.Add(chkARC_3)
    chkARC.Add(chkARC_4)
    '
    '
    '
    '
    btnMARK = New List(Of Button)
    btnMARK.Add(btnMARK_0)
    btnMARK.Add(btnMARK_1)
    btnMARK.Add(btnMARK_2)
    btnMARK.Add(Nothing)
    btnMARK.Add(btnMARK_4)
    '
    '
    '

    btnZEIG = New List(Of Button)
    btnZEIG.Add(btnZEIG_0)
    btnZEIG.Add(btnZEIG_1)
    btnZEIG.Add(btnZEIG_2)
    btnZEIG.Add(Nothing)
    btnZEIG.Add(btnZEIG_4)
    '
    '
    '
    btnRUN = New List(Of Button)
    btnRUN.Add(btnRUN_0)
    btnRUN.Add(btnRUN_1)
    btnRUN.Add(btnrun_2)
    btnRUN.Add(btnRUN_3)
    btnRUN.Add(btnRUN_4)
    '
    '
    '
    btnCLIP = New List(Of Button)
    btnCLIP.Add(Nothing)
    btnCLIP.Add(Nothing)
    btnCLIP.Add(btnCLIP_2)
    btnCLIP.Add(btnCLIP_3)
    btnCLIP.Add(Nothing)


    '

    '
    '
    dbgREF = New List(Of DataGridView)
    dbgREF.Add(dbgREF_0)
    dbgREF.Add(dbgREF_1)
    dbgREF.Add(dbgREF_2)
    dbgREF.Add(Nothing)
    dbgREF.Add(dbgREF_4)
    '
    '
    '
    grdWRT = New List(Of DataGridView)
    grdWRT.Add(Nothing)
    grdWRT.Add(Nothing)
    grdWRT.Add(grdWRT_2)
    grdWRT.Add(grdWRT_3)
    grdWRT.Add(Nothing)
    '
    '
    '
    '
    lblANZ = New List(Of Label)
    lblANZ.Add(lblANZ_0)
    lblANZ.Add(lblANZ_1)
    lblANZ.Add(lblANZ_2)
    lblANZ.Add(Nothing)
    lblANZ.Add(lblANZ_4)
    '
    '
    '
    '
    txtANZ = New List(Of TextBox)
    txtANZ.Add(txtANZ_0)
    txtANZ.Add(txtANZ_1)
    txtANZ.Add(txtANZ_2)
    txtANZ.Add(Nothing)
    txtANZ.Add(txtANZ_4)
    '
    '
    '
    '
    '
    chkDAT = New List(Of CheckBox)
    chkDAT.Add(chkDAT_0)
    chkDAT.Add(chkDAT_1)
    chkDAT.Add(chkDAT_2)
    chkDAT.Add(Nothing)
    chkDAT.Add(chkDAT_4)
    '
    '
    '
    '
    lblVON = New List(Of Label)
    lblVON.Add(lblVON_0)
    lblVON.Add(lblVON_1)
    lblVON.Add(lblVON_2)
    lblVON.Add(Nothing)
    lblVON.Add(lblVON_4)
    '
    '
    '
    '
    txtVON = New List(Of TextBox)
    txtVON.Add(txtvon_0)
    txtVON.Add(txtVON_1)
    txtVON.Add(txtVON_2)
    txtVON.Add(Nothing)
    txtVON.Add(txtVON_4)
    '
    '
    '
    lblBIS = New List(Of Label)
    lblBIS.Add(lblBIS_0)
    lblBIS.Add(lblBIS_1)
    lblBIS.Add(lblBIS_2)
    lblBIS.Add(Nothing)
    lblBIS.Add(lblBIS_4)
    '
    '
    '
    '
    txtbis = New List(Of TextBox)
    txtbis.Add(txtBIS_0)
    txtbis.Add(txtBIS_1)
    txtbis.Add(txtBIS_2)
    txtbis.Add(Nothing)
    txtbis.Add(txtBIS_4)
    '
    '
    '
    '
    '
    lblSQL = New List(Of Label)
    lblSQL.Add(lblSQL_0)
    lblSQL.Add(lblSQL_1)
    lblSQL.Add(lblSQL_2)
    lblSQL.Add(Nothing)
    lblSQL.Add(lblSQL_4)
    '
    '
    '
    '
    txtSQL = New List(Of TextBox)
    txtSQL.Add(txtSQL_0)
    txtSQL.Add(txtSQL_1)
    txtSQL.Add(txtSQL_2)
    txtSQL.Add(Nothing)
    txtSQL.Add(txtSQL_4)
    '
    '
    '
    '
    lblKENN = New List(Of Label)
    lblKENN.Add(Nothing)
    lblKENN.Add(lblKENN_1)
    lblKENN.Add(Nothing)
    lblKENN.Add(lblKENN_3)
    lblKENN.Add(Nothing)
    '
    '
    '
    txtKENN = New List(Of TextBox)
    txtKENN.Add(Nothing)
    txtKENN.Add(txtKENN_1)
    txtKENN.Add(Nothing)
    txtKENN.Add(txtKENN_3)
    txtKENN.Add(Nothing)
    '
    '
    '
    '
    '
    '
    '
    '
    For i = 0 To lblGRP.Count - 1
      lblGRP(i).Text = Texxt(0)
    Next
    For i = 0 To chkARC.Count - 1
      If Not IsNothing(chkARC(i)) Then
        chkARC(i).Text = Texxt(0)
      End If
    Next
    '
    For i = 0 To btnMARK.Count - 1
      If Not IsNothing(btnMARK(i)) Then
        btnMARK(i).Text = Texxt(0)
      End If
    Next
    '
    '
    For i = 0 To btnZEIG.Count - 1
      If Not IsNothing(btnZEIG(i)) Then
        btnZEIG(i).Text = Texxt(0)
      End If
    Next
    '
    '
    '
    For i = 0 To btnRUN.Count - 1
      If Not IsNothing(btnRUN(i)) Then
        btnRUN(i).Text = Texxt(0)
      End If
    Next
    '
   
    '
    '
    For i = 0 To btnCLIP.Count - 1
      If Not IsNothing(btnCLIP(i)) Then
        btnCLIP(i).Text = Texxt(0)
      End If
    Next
    '
    '
    '
    '
    '
    '
    '
    '
    For i = 0 To lblANZ.Count - 1
      If Not IsNothing(lblANZ(i)) Then
        lblANZ(i).Text = Texxt(0)
      End If
    Next
    '
    '
    '
    '
    For i = 0 To chkDAT.Count - 1
      If Not IsNothing(chkDAT(i)) Then
        chkDAT(i).Text = Texxt(0)
      End If
    Next
    '
    '
    '
    For i = 0 To lblVON.Count - 1
      If Not IsNothing(lblVON(i)) Then
        lblVON(i).Text = Texxt(0)
      End If
    Next
    '
    '
    '
    '
    For i = 0 To lblBIS.Count - 1
      If Not IsNothing(lblBIS(i)) Then
        lblBIS(i).Text = Texxt(0)
      End If
    Next
    '
    '
    '
    '
    '
    For i = 0 To lblSQL.Count - 1
      If Not IsNothing(lblSQL(i)) Then
        lblSQL(i).Text = Texxt(0)
      End If
    Next
    '
    '
    '
    For i = 0 To lblKENN.Count - 1
      If Not IsNothing(lblKENN(i)) Then
        lblKENN(i).Text = Texxt(0)
      End If
    Next
    '
    '
    chkDUP.Text = Texxt(0)
    lblDIK.Text = Texxt(0)
    lblGRPR.Text = Texxt(0)
    chkARCN.Text = Texxt(0)
    chkANEU.Text = Texxt(0)
    lblMSH.Text = Texxt(0)
    lblMESS.Text = Texxt(0)
    '
    '
    '
    TabDARF.TabPages(0).Text = Texxt(3680)
    TabDARF.TabPages(1).Text = Texxt(3681)
    TabDARF.TabPages(2).Text = Texxt(3682)
    TabDARF.TabPages(3).Text = Texxt(3683)
    TabDARF.TabPages(4).Text = Texxt(3685)
    '
    '
    '
    '
    If BitWrt(4, MenueParam.User.Writ) = 1 Then
      '
      'Datenbank exclusiv öffnen
      '
      '
      TabDARF.SelectedIndex = 0
    Else
      TabDARF.TabPages(0).Enabled = False
      TabDARF.SelectedIndex = 1
    End If
    If MenueParam.MischID = -1 Then
      TabDARF.TabPages(4).Enabled = False
    End If


    GrpRwerte = New RefValuesGrp
    ReWrRwert = New ReadWriteRwert
    RefWert = New RefValue
    '
    RezSozpt = New Recipes
    RwWrRezept = New ReadWriteRezept
    MischGroup = New HandleRezGroup
    HandleRwrt = New HandleRwerte
    MischGroup.cboGRP = cboGRPR
    'MischGroup.MischAufbau()
    If MenueParam.MischID <> -1 Then
      txtDIK.Text = MenueParam.Misch.Dicke
    End If
    '
    '
    '
    '
    '
    '
    TabMess = New DataTable
    AdaptMess = New OleDbDataAdapter
    CmdMess = New OleDbCommand("", Cncol)
    AdaptMess.SelectCommand = CmdMess
    AdaptGroup = New OleDbDataAdapter
    Cmdgroup = New OleDbCommand("", Cncol)
    AdaptGroup.SelectCommand = Cmdgroup
    TabGroup = New DataTable
    AdaptRwert = New OleDbDataAdapter
    CmdRwert = New OleDbCommand("", Cndat)
    TabRwert = New DataTable
    dbgREF_0.DataSource = TabRwert
    dbgREF_1.DataSource = TabRwert
    dbgREF_2.DataSource = TabRwert
    dbgREF_4.DataSource = TabRwert
    '
    '
    TabWerte = New List(Of DataTable)
    TabWerte_2 = New DataTable
    AdaptWerte_2 = New OleDbDataAdapter
    CmdWerte_2 = New OleDbCommand("", Cncol)
    TabWerte_3 = New DataTable
    AdaptWerte_3 = New OleDbDataAdapter
    CmdWerte_3 = New OleDbCommand("", Cncol)
    TabWerte.Add(Nothing)
    TabWerte.Add(Nothing)
    TabWerte.Add(TabWerte_2)
    TabWerte.Add(TabWerte_3)
    grdWRT(2).DataSource = TabWerte(2)
    grdWRT(3).DataSource = TabWerte(3)


    '
    '
    '
    'Messgeräte
    '
    '
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_MESSG WHERE MESSG_ID=ORDER BY MESSG_KBEZ"
    CmdMess.CommandText = SqlStmt
    If Not FillDatset(AdaptMess, TabMess) Then
      Exit Sub
    End If
    cboMESS.DataSource = TabMess
    cboMESS.DisplayMember = "MESSG_KBEZ"
    cboMESS.ValueMember = "MESSG_ID"
    cboMESS.SelectedIndex = 0
    'Menue-Parameter uebernehmen
    '
    '

    '
    txtbis(0).Text = Format$(DateAdd("d", -1 * 365, GetType(Date)))
    txtVON(0).Text = Format$(DateAdd("d", -2 * 365, GetType(Date)))
    For j = 1 To 2
      txtbis(j).Text = Format$(DateAdd("d", 0, GetType(Date)))
      txtVON(j).Text = Format$(DateAdd("d", -1 * 365, GetType(Date)))
    Next j
    txtbis(4).Text = Format$(DateAdd("d", 0, GetType(Date)))
    txtVON(4).Text = Format$(DateAdd("d", -1 * 365, GetType(Date)))


    '
    ' Dynset.MoveFirst()
    ' cboGRPN.Clear()
    ' Do While Not Dynset.EOF
    ' cboGRPN.AddItem(Dynset!GROUP_KBEZ)
    ' cboGRPN.ItemData(cboGRPN.NewIndex) = Dynset!Group_id
    ' Dynset.MoveNext()
    ' Loop
    ' For i = 0 To cboGRPN.ListCount - 1
    ' If cboGRPN.ItemData(i) = MenueParam.Messg.UserRefGID Then
    ' cboGRPN.ListIndex = i
    ' Exit For
    ' End If
    ' Next i



    'Dynset.Close()

    Exit Sub
    'DbSavErr:
    'MsgBox Texxt(3610) & Error
    '   Unload(Me)
  End Sub

  Private Sub frmDARF_Resize(sender As Object, e As System.EventArgs) Handles Me.Resize
    TabDARF.ItemSize = New Size((TabDARF.Width - TabDARF.TabPages.Count) / TabDARF.TabPages.Count - 3, TabDARF.ItemSize.Height)
  End Sub
  '
  '
  '
  '
  '
  Private Sub cboMESS_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles _
   cboMESS.SelectedIndexChanged
    Dim index As Integer
    index = CInt(sender.name.substring(8, 1))
    If IsNothing(cboMESS.SelectedValue) OrElse Not IsNumeric(cboMESS.SelectedValue) Then Exit Sub
    '
    '
    '
    'Clear Tables
    '
    '
    TabWerte(2).Clear()
    TabWerte(3).Clear()
    TabRwert.Clear()
    '


    '
    'Combobox mit Gruppen-Id's für R-Werte
    '
    SqlStmt = "SELECT * FROM TBL_MESSG_GROUP WHERE MESSG_ID=" & cboMESS.SelectedValue & " ORDER BY GROUP_ID DESC"
    '
    Cmdgroup.CommandText = SqlStmt
    If Not FillDatset(AdaptGroup, TabGroup) Then
      Exit Sub
    End If
    For i = 0 To cbogrp.Count - 1
      cbogrp(i).DataSource = TabGroup
      cbogrp(i).DisplayMember = "GROUP_KBEZ"
      cbogrp(i).ValueMember = "GROUP_ID"
      cbogrp(i).SelectedIndex = 0
    Next
    AufbauPar.MessgID = cboMESS.SelectedValue

    '
  End Sub
  '


  Private Sub cboGRP_Click(sender As Object, e As System.EventArgs) Handles _
   cboGRP_0.Click, cboGRP_1.Click, cboGRP_2.Click, cboGRP_3.Click, cboGRP_4.Click
    Dim index As Integer
    Index = CInt(sender.name.substring(7, 1))
    If IsNothing(cbogrp(index).SelectedValue) OrElse Not IsNumeric(cbogrp(index).SelectedValue) Then Exit Sub

    MenueParam.Messg.RwrtGID = cbogrp(index).SelectedValue

  End Sub
  '
  '
  '
  Private Sub btnCLIP_Click(sender As Object, e As System.EventArgs) Handles btnCLIP_2.Click, btnCLIP_3.Click
    Dim index As Integer
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim TextSplit() As String
    Dim RowCount As Integer
    Dim RowStart As Integer
    Dim ColumnCount As Integer
    Dim NRow As DataRow
    index = CInt(sender.name.substring(8, 1))
    Select Case index
      Case 2
        '
        '
        'Schreiben nach Clipboard
        '
        Clipboard.Clear()
        '
        'Grid in Zwischenablage
        '
        '
        '
        '
        grdWRT(index).ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableWithoutHeaderText
        grdWRT(index).SelectAll()

        If grdWRT(index).GetCellCount(DataGridViewElementStates.Selected) > 0 Then
          Clipboard.SetDataObject(grdWRT(index).GetClipboardContent)
        End If
        grdWRT(index).ClearSelection()
      Case 3
        

        '
        '
        'Zwischenablage übernehmen
        '
        '
        ColumnCount = grdWRT(index).Columns.Count
        TextSplit = Clipboard.GetDataObject.GetData(DataFormats.Text, True).Split(Chr(9), Chr(13) & Chr(10))
        RowCount = CInt(TextSplit.Length / ColumnCount)
        RowStart = grdWRT(index).CurrentRow.Index
        If Asc(TextSplit(TextSplit.Length - 1)) = 10 Or Asc(TextSplit(TextSplit.Length - 1)) = 9 Or Asc(TextSplit(TextSplit.Length - 1)) = 13 Then
          '
          '
          'Letztes Element weglassen
          '
          '
          '
          ReDim Preserve TextSplit(TextSplit.Length - 2)
        End If
        If RowCount <> 0 AndAlso RowCount * ColumnCount = TextSplit.Length Then
          k = 0
          For j = 0 To RowCount - 1
            If IsDBNull(TextSplit(k)) OrElse TextSplit(k) = "" Then
              Exit For
            End If
            'NRow = TblGrundStoffe.NewRow
            For i = 0 To ColumnCount - 1
              TextSplit(k) = TextSplit(k).Replace(Chr(13), "")
              TextSplit(k) = TextSplit(k).Replace(Chr(10), "")
              TextSplit(k) = TextSplit(k).Replace(Chr(9), "")
              If grdWRT(index).Columns(i).Name = "EQUIV" Then
                If Not IsNumeric(TextSplit(k)) Then
                  TextSplit(k) = 0.0
                End If
                NRow("EQUIV") = TextSplit(k)
              Else
                NRow(grdWRT(index).Columns(i).Name) = TextSplit(k)
              End If
              'End Select
              k = k + 1
            Next i
            If NRow("BEZEICHNUNG") <> "" Then
              TabWerte(index).Rows.Add(NRow)
            End If
          Next j
          TabWerte(index).AcceptChanges()
        Else
          MsgBox("Fehler")
        End If

    End Select
  End Sub
  '
  '
  '
  '
  '
  Private Sub btnMARK_Click(sender As Object, e As System.EventArgs) Handles _
   btnMARK_0.Click, btnMARK_1.Click, btnMARK_2.Click, btnMARK_4.Click
    Dim index As Integer
    index = CInt(sender.name.substring(8, 1))
    btnMARK(index).Enabled = False
    btnRUN(index).Enabled = False
    dbgREF(index).SelectAll()
    'If datREF(Index).Recordset.BOF Then Exit Sub
    'datREF(Index).Recordset.MoveFirst()
    
    'MousePointer = 11
    'Do While dbgREF(Index).SelBookmarks.Count > 0
    ' dbgREF(index).SelBookmarks.Remove(0)
    ' Loop
    ' Do While Not datREF(index).Recordset.EOF
    ' dbgREF(index).SelBookmarks.Add(datREF(index).Recordset.BookMark)
    ' datREF(index).Recordset.MoveNext()
    ' Loop
    ' MousePointer = 1
    btnMARK(index).Enabled = True
    btnRUN(index).Enabled = True
  End Sub
  '
  '
  '
  '
  '
  '
  Private Sub chkDUP_Click(sender As Object, e As System.EventArgs) Handles chkDUP.Click
    If chkDUP.Checked Then
      lblSQL(1).Visible = False
      txtSQL(1).Visible = False
      lblGRP(1).Visible = False
      cbogrp(1).Visible = False
      chkDAT(1).Visible = False
      lblVON(1).Visible = False
      lblBIS(1).Visible = False
      txtVON(1).Visible = False
      txtbis(1).Visible = False
      chkARC(1).Visible = False
    Else
      lblSQL(1).Visible = True
      txtSQL(1).Visible = True
      lblGRP(1).Visible = True
      cbogrp(1).Visible = True
      chkDAT(1).Visible = True
      lblVON(1).Visible = True
      lblBIS(1).Visible = True
      txtVON(1).Visible = True
      txtbis(1).Visible = True
      chkARC(1).Visible = True
    End If
  End Sub
  '
  '
  '
  '
  '
  '
  Private Sub btnRUN_Click(sender As Object, e As System.EventArgs) Handles _
   btnRUN_0.Click, btnRUN_1.Click, btnRUN_2.Click, btnRUN_3.Click, btnRUN_4.Click
    Dim index As Integer
    Dim Kloe As Long
    Dim CharLen As Long
    Dim KeyMeng As String
    Dim RezID As Long
    Dim TypID(2) As Long
    Dim SmpID(2) As Long
    Dim UntID(2) As Long
    Dim WelStr As String
    Cursor = Cursors.WaitCursor
    index = CInt(sender.name.substring(7, 1))
    Select Case Index
      Case 0
        '
        '
        'Löschen
        '
        '
        '
        '

        '
        If dbgREF(index).Rows.Count = 0 Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        imsg = MsgBox(Texxt(2929), 4, Texxt(2000))
        If imsg = 7 Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        imsg = MsgBox(Texxt(2930), 4, Texxt(2000))
        If imsg = 7 Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        btnZEIG(index).Enabled = False

        '
        '
        '
        '
        'ausgewählte Sätze löschen
        '
        '
        Try

          For i = 0 To dbgREF(index).SelectedRows.Count - 1
            RefID = dbgREF(index).SelectedRows(i).Cells("RWERT_ID").Value

            ReWrRwert.DelRwert(RefID, ier)
          Next i
          
          btnZEIG(index).Enabled = True
          imsg = MsgBox(Texxt(2928), 4, Texxt(2000))
         
          Cursor = Cursors.Default
        Catch ex As Exception

        End Try

      Case 1
        '
        '
        'Korrigieren
        '
        If dbgREF(index).Rows.Count = 0 Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        imsg = MsgBox(Texxt(2927), 4, Texxt(2000))
        If imsg = 7 Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        '
        If chkGRPN.Checked AndAlso (Not IsNothing(cboGRPN.SelectedValue) OrElse IsNumeric(cboGRPN.SelectedValue)) Then
          If Not HandleRwrt.MeldSpeiAllRwrt(cboGRPN.SelectedValue, Texxt(3015)) Then
            Cursor = Cursors.Default
            Exit Sub
          End If
        End If
        btnZEIG(index).Enabled = False
        Strgid = " SET RWERT_CME='" & Mid(txtKENN(index).Text, 1, 2) & "'"
        If chkARCN.Checked Then
          Strgid = Strgid & ", RWERT_IARCH=" & chkANEU.CheckState
        End If
        If chkGRPN.Checked Then
          Strgid = Strgid & ", RWERT_GID=" & cboGRPN.SelectedValue
        End If
        '
        '
        'Ausgewählte Sätze korrigieren
        '
        '
        For i = 0 To dbgREF(index).SelectedRows.Count - 1
          RefID = dbgREF(index).SelectedRows(i).Cells("RWERT_ID").Value


          'datREF(index).Recordset.BookMark = dbgREF(index).SelBookmarks(i)
          SqlStmt = "UPDATE TBL_RWERT " & Strgid & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID _
          & " AND [RWERT_ID]=" & RefID
          CmdRwert.CommandText = SqlStmt
          If SQLExeNonQuery(CmdRwert, Cndat) Then
            Continue For
          End If
        Next i
        ' Worksp.CommitTrans()
        ' datREF(index).Refresh()
        ' dbgREF(index).Columns("RWERT_ID").Visible = False
        ' If Not datREF(index).Recordset.BOF Then
        ' datREF(index).Recordset.MoveFirst()
        ' End If
        Cursor = Cursors.Default
        btnZEIG(index).Enabled = True

      Case 2
        '
        '
        '
        'Tabelle (Grid) aufbauen aus ACCESS
        '
        '
        '
        'Tabelle TabMessREch aufbauen
        '
        '
        TabWerte(index).Columns.Add(Texxt(3659))
        TabWerte(index).Columns.Add(Texxt(824))
        TabWerte(index).Columns.Add(Texxt(916) & "/" & Texxt(602))
        TabWerte(index).Columns.Add(Texxt(375))
        TabWerte(index).Columns.Add(MenueParam.Messg.Kbez & "/" & Texxt(893))
        For i = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
          TabWerte(index).Columns.Add(CStr(MenueParam.Messg.Winkel.Wsol.R(i)))
        Next i
        '
        '
       
        '


        '
        For i = 0 To dbgREF(index).SelectedRows.Count - 1
          RefID = dbgREF(index).SelectedRows(i).Cells("RWERT_ID").Value

          '
          '
          'Tabelle aufbauen
          '
          '
          ReWrRwert.ReadRwert(RefID, RefWert, MenueParam.Messg.Winkel, ier)

          j = System.Math.Abs(RwertHilf(StrRw)(KeyNr).kwb) + 1
          Call HandleRwrt.TabRwertRecord(j, MenueParam.Messg.Winkel, RwertHilf(StrRw)(KeyNr), TabMessRech, ier)
          If ier > 0 Then Exit Sub



          StrRw = RwKey(l).Substring(0, 1)
          KeyNr = RwKey(l).Substring(1, 3)
          '
          RowWrt = TabMessRech.NewRow
          '
          '
          RowWrt(0) = "0"
          Nanz = RwertHilf(StrRw)(KeyNr).Nr
          RowWrt(1) = RezSozpt.Rezepte(KeyNr).Name
          RowWrt(2) = RezSozpt.Rezepte(KeyNr).Bem
          RowWrt(3) = RezSozpt.Rezepte(KeyNr).DatTim
          RowWrt(4) = RezSozpt.Rezepte(KeyNr).Dicke(RwertHilf(StrRw)(KeyNr).kwb)
          TabMessRech.Rows.Add(RowWrt)

          For i = 0 To RezSozpt.Rezepte(KeyNr).KF - 1
            RowWrt = TabMessRech.NewRow
            FaId = RezSozpt.Rezepte(KeyNr)(i).ID
            RowWrt(0) = "#" & CStr(FaId)
            RowWrt(1) = RezSozpt.Farben(KeyName(FaId)).Name
            RowWrt(2) = Format(RezSozpt.Rezepte(KeyNr)(i).FaAmng, RezSozpt.Farben(KeyName(FaId)).Form)
            TabMessRech.Rows.Add(RowWrt)
          Next i
          '
          '
          'Messung/Rechnung
          '
          '
          '
          j = System.Math.Abs(RwertHilf(StrRw)(KeyNr).kwb) + 1
          Call handlerezeptTabRwertRecord(j, MenueParam.Messg.Winkel, RwertHilf(StrRw)(KeyNr), TabMessRech, ier)
          If ier > 0 Then Exit Sub
          '
          '
          '
        Next l
        '
        'Gridparameter
        '
        '
        '
        'flgMessRech.Cols.Fixed = 0
        flgMessRech.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
        flgMessRech.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
        flgMessRech.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
        flgMessRech.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
        flgMessRech.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
        flgMessRech.Columns(0).Width = 50
        flgMessRech.Columns(1).Width = 150
        flgMessRech.Columns(2).Width = 125
        flgMessRech.Columns(3).Width = 125
        flgMessRech.Columns(4).Width = 100
        For i = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
          flgMessRech.Columns(i + 5).Width = 50
          flgMessRech.Columns(i + 5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
        Next i





        '
        '
        If dbgREF(index).Rows.Count = 0 Then
          Cursor = Cursors.Default
          Exit Sub
        End If

        grdWRT(index).Visible = True
        dbgREF(index).Visible = False
        Application.DoEvents()
        '
        '
        'Header
        '
        '
        '
        '
        grdWRT(index).AlternatingRowsDefaultCellStyle.BackColor = Color.Azure
        grdWRT(index).DefaultCellStyle.BackColor = Color.Beige
        grdWRT(index).AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
        '
        '
        grdWRT(index).ColumnHeadersDefaultCellStyle.Font = New Font(grdWRT(index).ColumnHeadersDefaultCellStyle.Font, FontStyle.Bold)

        grdWRT(index).AutoGenerateColumns = False
        grdWRT(index).Columns.Add("ID", Texxt(3659))
        grdWRT(index).Columns.Add("NAME", Texxt(824))
        grdWRT(index).Columns.Add("BEM", Texxt(916))
        grdWRT(index).Columns.Add("DATE", Texxt(375))
        grdWRT(index).Columns.Add("MESSG", MenueParam.Messg.Kbez)

       
        '
        '
        grdWRT(index).Columns("ID").Width = 70
        grdWRT(index).Columns("NAME").Width = 280
        grdWRT(index).Columns("BEM").Width = 200
        grdWRT(index).Columns("DATE").Width = 100
        grdWRT(index).Columns("MESSG").Width = 200
        '
        '
        grdWRT(index).Columns("ID").HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        grdWRT(index).Columns("NAME").HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        grdWRT(index).Columns("BEM").HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        grdWRT(index).Columns("DATE").HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        grdWRT(index).Columns("MESSG").HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        For i = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
          WelStr = CStr(MenueParam.Messg.Winkel.Wsol.R(i))
          grdWRT(index).Columns.Add(WelStr, WelStr)
        Next i

        For i = 0 To dbgREF(index).Columns.Count - 1
          grdWRT(index).Columns(i).DataPropertyName = dbgREF(index).Columns(i).Name
        Next


        RefWert.ID = -1
        '
        '
        '
        '
        '
        'Ausgewählte Sätze für Tabelle
        '
        '
        For i = 0 To dbgREF(index).SelectedRows.Count - 1
          RefID = dbgREF(index).SelectedRows(i).Cells("RWERT_ID").Value
          '
          '
          'Rwerte einlesen
          '
          '
          '

          ReWrRwert.ReadRwert(RefID, RefWert, MenueParam.Messg.Winkel, ier)
          If ier = 0 Then
            '
            '
            '
            'Grid aufbauen
            '
            Call GridRwertRecord(CharLen, 1, MenueParam.Messg.Winkel, RefWert, grdWRT(index), ier)
            If ier > 0 Then Exit For
            MaxClipBoard = MaxClipBoard + CharLen
            '
            '
          End If
        Next i
        '
        '
        DBEngine.Idle(dbFreeLocks)
        If grdWRT(index).Rows > 1 Then
          grdWRT(index).FixedRows = 1
        End If
        cmdCLIP(index).Enabled = True

        datREF(index).Refresh()
        datREF(index).Recordset.MoveFirst()
        Me.MousePointer = 1

        '
      Case 3
        '
        '

        'Tabelle nach ACCESS umspeichern
        '
        '
        If Not HandleRwrt.MeldSpeiAllRwrt(cbogrp(index).selectedvalue, Texxt(3015)) Then
          MousePointer = 1
          Exit Sub
        End If
        '
        '
        '
        'Prüfen, ob richtige Wellenlängen verfügbar
        grdWRT(index).row = 0

        For j = 1 To MenueParam.Messg.Winkel.Wsol.Nwe
          grdWRT(index).col = j + 4
          If CSng(grdWRT(index).Text) <> MenueParam.Messg.Winkel.Wsol.R(j) Then
            MsgBox(Texxt(3023))
            MousePointer = 1
            Exit Sub
          End If
        Next j
        '
        '
        '
        '
        RefWert.Cme = txtKENN(index).Text
        RefWert.Iarch = chkARC(index).Value
        RefWert.Gid = cbogrp(index).ItemData(cbogrp(index).ListIndex)
        RefWert.DatTim = Now
        RefWert.St = 0
        RefWert.Banum = Space(1)
        RefWert.Iami = 1
        For j = 1 To MenueParam.Messg.Winkel.Km
          RefWert.De(j) = 0
        Next j
        Do While RefWert.ReflKurven.Count > 0
          RefWert.ReflKurven.RemKurv(1)
        Loop
        For kw = 1 To MenueParam.Messg.Winkel.Km
          RefWert.ReflKurven.Addnew(MenueParam.Messg.Winkel.Wsol.Nwe, MenueParam.Messg.Winkel(kw).Chrm)
        Next kw
        Worksp.BeginTrans()
        Try


          For j = 1 To grdWRT(index).Rows - 1 Step MenueParam.Messg.Winkel.Km
            grdWRT(index).row = j
            grdWRT(index).col = 0
            If Asc(Trim(grdWRT(index).Text)) < 97 And Asc(Trim(grdWRT(index).Text)) > 122 Then
              If Asc(Trim(grdWRT(index).Text)) <> 49 Then
                MsgBox(Texxt(2925))
                GoTo ErrorRun3
              End If
            End If
            grdWRT(index).col = 1
            RefWert.Name = grdWRT(index).Text
            grdWRT(index).col = 2
            RefWert.Bem = grdWRT(index).Text
            grdWRT(index).col = 3
            RefWert.DatTim = grdWRT(index).Text

            For kw = 1 To MenueParam.Messg.Winkel.Km
              grdWRT(index).row = (j - 1) + kw
              For i = 1 To MenueParam.Messg.Winkel.Wsol.Nwe
                grdWRT(index).col = i + 4
                RefWert.ReflKurven(kw).r(i) = 0.01 * Singl(grdWRT(index).Text)
              Next i
            Next kw
            '
            '
            'Tabelle umspeichern
            '
            '
            ReWrRwert.WriteRwert(Ddat, RefWert.ID, RefWert, MenueParam.Messg.Winkel, ier)
          Next j

          imsg = MsgBox(Texxt(2926), 4, Texxt(2000))
          If imsg = 7 Then
            Worksp.Rollback()
          Else
            Worksp.CommitTrans()
          End If
          MousePointer = 1
          Exit Sub

        Catch ex As Exception
          MsgBox(ex.message)
          GoTo errorrun3
        End Try
ErrorRun3:
        MousePointer = 1
        Worksp.Rollback()
        Exit Sub

      Case 4
        '
        '
        '
        'ASCII-File übernehmen
        '
        If Not HandleRwrt.MeldSpeiAllRwrt(cbogrp(index).selectedvalue, Texxt(3015)) Then
          MousePointer = 1
          Exit Sub
        End If

        MousePointer = 11
        DoEvents()
        Worksp.BeginTrans()
        Try

          For i = 1 To MenueParam.Messg.Winkel.Km
            RefWert.De(i) = 0.0#
          Next i
          RefWert.Iami = 1
          RefWert.Bem = ""
          RefWert.Banum = ""
          RefWert.St = 0
          RefWert.Iarch = chkARC(index).Value
          RefWert.Cme = Mid(txtKENN(index).Text, 1, 2)
          RefWert.Gid = cbogrp(index).ItemData(cbogrp(index).ListIndex)
          '
          '
          Call GetFilRwert(FilNaa, Kanz, RefWert, MenueParam.Messg.Winkel, grdWRT(index), ier)
          '
          '
          If ier > 0 Then
            MousePointer = 1
            Worksp.Rollback()
            Exit Sub
          End If
          txtANZ(index).Text = Kanz
          imsg = MsgBox(Texxt(2924), 4, Texxt(2000))
          If imsg = 7 Then
            Worksp.Rollback()
          Else
            Worksp.CommitTrans()
            cmdRUN(index).Enabled = False
          End If

          MousePointer = 1
          Exit Sub

        Catch ex As Exception
          MsgBox(ex.message)
          GoTo errorrun4
        End Try
ErrorRun4:

        MousePointer = 1
        Worksp.Rollback()
        Exit Sub

        '
        '
      Case 5
        '
        '
        'Sätze in Colorthek speichern
        '
        If datREF(index).Recordset.BOF Then
          MousePointer = 1
          Exit Sub
        End If
        imsg = MsgBox(Texxt(2946), 4, Texxt(2000))
        If imsg = 7 Then
          Me.MousePointer = 1
          Exit Sub
        End If
        KeyMeng = "MNG"
        Do While RezSozpt.RezMeng.RezCount > 0
          RezSozpt.RezMeng.RemoveRez(1)
        Loop
        RezSozpt.RezMeng.AddnewRez(KeyMeng)
        RezSozpt.RezMeng(KeyMeng).Dicke(VKwb(1)) = txtDIK.Text
        RezSozpt.RezMeng(KeyMeng).GID = cboGRPR.ItemData(cboGRPR.ListIndex)
        RezSozpt.RezMeng(KeyMeng).dattim = Now
        RezSozpt.RezMeng(KeyMeng).Iarch = chkARCR.Value
        For i = 1 To 2
          TypID(i) = -1
          SmpID(i) = -1
          UntID(i) = -1
        Next i

        '
        '
        Worksp.BeginTrans()
        '
        '
        'Ausgewählte Sätze in Colorthek speichern
        '
        '
        For i = 0 To dbgREF(index).SelBookmarks.Count - 1
          datREF(index).Recordset.BookMark = dbgREF(index).SelBookmarks(i)
          RezSozpt.RezMeng(KeyMeng).Name = datREF(index).Recordset!Rwert_Name
          SmpID(VKwb(1)) = datREF(index).Recordset!Rwert_ID
          RwWrRezept.AddRezept(Ddat, KeyMeng, RezID, RezSozpt, UntID, TypID, SmpID, ier)
        Next i
        imsg = MsgBox(Texxt(2947), 4, Texxt(2000))
        If imsg = 7 Then
          Worksp.Rollback()
        Else
          Worksp.CommitTrans()
        End If
        'datREF(Index).Refresh
        dbgREF(index).Columns("RWERT_ID").Visible = False
        If Not datREF(index).Recordset.BOF Then
          datREF(index).Recordset.MoveFirst()
        End If

        Me.MousePointer = 1
        cmdZEIG(index).Enabled = True

    End Select
    Me.MousePointer = 1
  End Sub




  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************


  Private Sub cmdZEIG_Click(Index As Integer)
    If Not IsDate(txtVON(Index).Text) Then
      MsgBox(Texxt(2500))
      Exit Sub
    End If
    If Not IsDate(txtbis(Index).Text) Then
      MsgBox(Texxt(2500))
      Exit Sub
    End If

    Me.MousePointer = 11
    Try


      Select Case Index
        Case 0
          cmdRUN(Index).Enabled = False
          cmdMARK(Index).Enabled = False
          DoEvents()
          dbgREF(Index).Visible = True
          If Index = 2 Then
            grdWRT(Index).Visible = False
            cmdCLIP(Index).Enabled = False
          End If
          txtANZ(Index).Visible = False
          lblANZ(Index).Visible = False

          '
          '
          

          Strgid = "("
          If cbogrp(Index).ItemData(cbogrp(Index).ListIndex) > 0 Then
            Strgid = "RWERT_GID= " & cbogrp(Index).ItemData(cbogrp(Index).ListIndex) & " AND " & Strgid
          End If
          Strgid = Strgid & " RWERT_IARCH=2 OR "
          Strgid = Strgid & " RWERT_IARCH=0)"
          Strgid = " WHERE " & Strgid & " AND RWERT_DATTIM BETWEEN " & sqldat(txtVON(Index).Text) & " AND " & sqldat(DateAdd("d", 1, txtbis(Index).Text))
          If Trim(txtSQL(Index).Text) <> "" Then
            Strgid = Strgid & " AND " & StrSelct("RWERT_NAME", txtSQL(Index).Text)
          End If
          '
          SqlStmt = "DELETE * FROM " & TblRwertListNo

          Call SQLExeLock(0, Dtmp, SqlStmt, ier)
          If ier <> 0 Then Exit Sub

          '
          '
          '
          '
          SqlStmt = "DELETE * FROM " & TblRwertList

          Call SQLExeLock(0, Dtmp, SqlStmt, ier)
          If ier <> 0 Then Exit Sub
          '
          'R-Werte bei Sortimenten werden umgespeichert
          '
          '
          'For j = 1 To ForTabCount
          SqlStmt = "INSERT INTO " & TblRwertListNo & " IN '" & Dtmp.Name & "'" _
          & " SELECT DISTINCT TBL_RWERT.RWERT_ID FROM TBL_RWERT" _
          & " INNER JOIN TBL_SORTI_RWERT ON (TBL_RWERT.MESSGRW_ID=TBL_SORTI_RWERT.MESSGRW_ID) " _
          & " AND (TBL_RWERT.RWERT_ID=TBL_SORTI_RWERT.RWERT_ID) " _
          & Strgid & " AND TBL_SORTI_RWERT.MESSGRW_ID=" & MenueParam.MessgRwID _
          & " AND  TBL_RWERT.RWERT_ID NOT IN (SELECT RWERT_ID FROM " & TblRwertListNo & " IN '" & Dtmp.Name & "')"
          'MsgBox SqlStmt
          Call SQLExeLock(1, Ddat, SqlStmt, ier)
          If ier <> 0 Then Exit Sub
          'Next
          'R-Werte bei Rezepten  werden umgespeichert
          SqlStmt = "INSERT INTO " & TblRwertListNo & " IN '" & Dtmp.Name & "'" _
          & " SELECT DISTINCT TBL_RWERT.RWERT_ID FROM TBL_RWERT" _
          & " INNER JOIN TBL_REZEPT_RWERT ON (TBL_RWERT.MESSGRW_ID=TBL_REZEPT_RWERT.MESSGRW_ID) " _
          & " AND (TBL_RWERT.RWERT_ID=TBL_REZEPT_RWERT.RWERT_ID) " _
          & Strgid & " AND TBL_REZEPT_RWERT.MESSGRW_ID=" & MenueParam.MessgRwID _
          & " AND TBL_RWERT.RWERT_ID NOT IN (SELECT RWERT_ID FROM " & TblRwertListNo & " IN '" & Dtmp.Name & "')"
          Call SQLExeLock(1, Ddat, SqlStmt, ier)
          If ier <> 0 Then Exit Sub

          SqlStmt = "INSERT INTO " & TblRwertList & " IN '" & Dtmp.Name & "'" _
          & " SELECT RWERT_ID FROM TBL_RWERT" & Strgid _
          & " AND MESSGRW_ID=" & MenueParam.MessgRwID & " AND RWERT_ID NOT IN (SELECT DISTINCT RWERT_ID FROM " & TblRwertListNo & " IN '" & Dtmp.Name & "')"
          'MsgBox SqlStmt
          Call SQLExeLock(1, Ddat, SqlStmt, ier)
          '
          '
          '
          '
          '
          'SqlStmt = "SELECT RWERT_DATE,RWERT_NAME,RWERT_ID FROM " & Menueparam.TblRwert _
          '& " WHERE RWERT_ID IN (SELECT DISTINCT RWERT_ID FROM " & TblRwertList & ")"
          '
          SqlStmt = "SELECT TBL_RWERT.RWERT_ID AS RWERT_ID,RWERT_DATTIM,RWERT_NAME FROM TBL_RWERT" _
                & " WHERE MESSGRW_ID=" & MenueParam.MessgRwID _
                & " AND RWERT_ID IN (SELECT RWERT_ID FROM " & TblRwertList & " IN '" & Dtmp.Name & "')"

          SqlStmt = SqlStmt & " ORDER BY RWERT_NAME"
          'MsgBox SqlStmt
          datREF(Index).Recordset = Ddat.OpenRecordset(SqlStmt, dbOpenDynaset, dbReadOnly)
          If datREF(Index).Recordset.BOF Then
            MsgBox(Texxt(2959))
          Else
            txtANZ(Index).Visible = True
            lblANZ(Index).Visible = True
            datREF(Index).Recordset.MoveLast()
            txtANZ(Index).Text = CInt(datREF(Index).Recordset.RecordCount)
            datREF(Index).Recordset.MoveFirst()

          End If
          '
          '
          '
          '
          cmdRUN(Index).Enabled = True
          cmdMARK(Index).Enabled = True
          Me.MousePointer = 1

        Case 1, 2
          '
          '
          '
          '
          'Aufbau der dbgGRID-Tabelle zum Korrigieren bzw. Umspeichern
          '
          '
          '
          '

          cmdRUN(Index).Enabled = False
          cmdMARK(Index).Enabled = False
          DoEvents()
          dbgREF(Index).Visible = True
          If Index = 2 Then
            grdWRT(Index).Visible = False
            cmdCLIP(Index).Enabled = False
            cmdCLIP(Index).Enabled = False

          End If
          txtANZ(Index).Visible = False
          lblANZ(Index).Visible = False
          If chkDUP.Value = 1 And Index = 1 Then
            SqlStmt = "SELECT RWERT_ID,RWERT_DATTIM,RWERT_NAME FROM TBL_RWERT" _
            & " WHERE MESSGRW_ID=" & MenueParam.MessgRwID & " AND RWERT_NAME IN (SELECT RWERT_NAME FROM TBL_RWERT" _
            & " AS TMP_NAME WHERE MESSGRW_ID=" & MenueParam.MessgRwID & " GROUP BY RWERT_NAME HAVING COUNT(*)>1)"
          Else

            Strgid = ""
            If cbogrp(Index).ItemData(cbogrp(Index).ListIndex) > 0 Then
              Strgid = " AND RWERT_GID= " & cbogrp(Index).ItemData(cbogrp(Index).ListIndex)
            End If
            If chkARC(Index).Value < 2 Then
              Strgid = Strgid & " AND RWERT_IARCH=" & chkARC(Index).Value
            End If
            SqlStmt = "SELECT RWERT_ID,RWERT_DATTIM,RWERT_NAME FROM TBL_RWERT" _
            & " WHERE MESSGRW_ID=" & MenueParam.MessgRwID _
            & " AND (RWERT_DATTIM BETWEEN " & sqldat(txtVON(Index).Text) & " AND " & sqldat(DateAdd("d", 1, txtbis(Index).Text)) & ")" & Strgid
            If Trim(txtSQL(Index).Text) <> "" Then
              SqlStmt = SqlStmt & " AND " & StrSelct("RWERT_NAME", txtSQL(Index).Text)
            End If
          End If
          '
          '
          SqlStmt = SqlStmt & " ORDER BY RWERT_NAME"
          'MsgBox SqlStmt

          datREF(Index).Recordset = Ddat.OpenRecordset(SqlStmt, dbOpenDynaset, dbReadOnly)
          If datREF(Index).Recordset.BOF Then
            MsgBox(Texxt(2959))
          Else
            txtANZ(Index).Visible = True
            lblANZ(Index).Visible = True
            datREF(Index).Recordset.MoveLast()
            txtANZ(Index).Text = CInt(datREF(Index).Recordset.RecordCount)
            datREF(Index).Recordset.MoveFirst()

          End If
          '
          '
          '
          '
          cmdRUN(Index).Enabled = True
          cmdMARK(Index).Enabled = True
          Me.MousePointer = 1

        Case 3
        Case 4
        Case 5
          '
          '
          '
          '
          'Aufbau der dbgGRID-Tabelle zum Speichern der R-Werte in Colorthek
          '
          '
          '
          '

          cmdRUN(Index).Enabled = False
          cmdMARK(Index).Enabled = False
          DoEvents()
          dbgREF(Index).Visible = True
          txtANZ(Index).Visible = False
          lblANZ(Index).Visible = False

          Strgid = ""
          If cbogrp(Index).ItemData(cbogrp(Index).ListIndex) > 0 Then
            Strgid = " AND RWERT_GID= " & cbogrp(Index).ItemData(cbogrp(Index).ListIndex)
          End If
          If chkARC(Index).Value < 2 Then
            Strgid = Strgid & " AND RWERT_IARCH=" & chkARC(Index).Value
          End If
          SqlStmt = "SELECT RWERT_ID,RWERT_DATTIM,RWERT_NAME FROM TBL_RWERT" _
          & " WHERE MESSGRW_ID=" & MenueParam.MessgRwID & " AND (RWERT_DATTIM BETWEEN " & sqldat(txtVON(Index).Text) & " AND " & sqldat(DateAdd("d", 1, txtbis(Index).Text)) & ")" & Strgid
          If Trim(txtSQL(Index).Text) <> "" Then
            SqlStmt = SqlStmt & " AND " & StrSelct("RWERT_NAME", txtSQL(Index).Text)
          End If
          SqlStmt = SqlStmt & " AND RWERT_ID NOT IN (SELECT RWERT_ID FROM TBL_REZEPT_RWERT WHERE MESSGRW_ID=" & MenueParam.MessgRwID & " AND RWERT_KWB=" & VKwb(1) & ")"
          'MsgBox SqlStmt
          '
          '
          SqlStmt = SqlStmt & " ORDER BY RWERT_NAME"
          datREF(Index).Recordset = Ddat.OpenRecordset(SqlStmt, dbOpenDynaset, dbReadOnly)
          If datREF(Index).Recordset.BOF Then
            MsgBox(Texxt(2959))
          Else
            txtANZ(Index).Visible = True
            lblANZ(Index).Visible = True
            datREF(Index).Recordset.MoveLast()
            txtANZ(Index).Text = CInt(datREF(Index).Recordset.RecordCount)
            datREF(Index).Recordset.MoveFirst()

          End If
          '
          '
          '
          '
          cmdRUN(Index).Enabled = True
          cmdMARK(Index).Enabled = True
          Me.MousePointer = 1

      End Select
      If Not dbgREF(Index).BOF Then
        dbgREF(Index).Columns(0).Visible = False
        'dbgref(Index).Columns(1).Caption = Texxt(2205)
        'dbgref(Index).Columns(2).Caption = Texxt(370)
      End If
      dbgREF(Index).ColumnHeaders = False
      Exit Sub

    Catch ex As Exception
      MsgBox(ex.message)
      GoTo fehlzeig
    End Try
FehlZeig:

    txtANZ(Index).Visible = False
    lblANZ(Index).Visible = False
    MousePointer = 1
  End Sub


  Private Sub datMSH_Reposition()
    MischGroup.GroupAufbau()
    VKwb(1) = 1
    VKwb(2) = 2
    If MenueParam.Misch.Vert = 1 Then
      VKwb(1) = 2
      VKwb(2) = 1
    End If

  End Sub

  '*** APEX Migration Utility Code Change ***
  'Private Sub dbgREF_UnboundAddData(Index As Integer, ByVal RowBuf As RowBuffer, NewRowBookmark As Variant)



  Private Sub dbgREF_UnboundAddData(Index As Integer, ByVal RowBuf As TrueDBGrid80.RowBuffer, NewRowBookmark As Object)
    NewRowBookmark = dbgREF(Index).row
    For i = 0 To RowBuf.ColumnCount - 1
      MsgBox(RowBuf.Value(0, i))
    Next i
    RowBuf.RowCount = 1
  End Sub


  '*** APEX Migration Utility Code Change ***
  'Private Sub dbgREF_UnboundReadDataEx(Index As Integer, ByVal RowBuf As RowBuffer, StartLocation As Variant, ByVal offset As Long, ApproximatePosition As Long)
  Private Sub dbgREF_UnboundReadDataEx(Index As Integer, ByVal RowBuf As TrueDBGrid80.RowBuffer, StartLocation As Object, ByVal offset As Long, ApproximatePosition As Long)
    ' MsgBox "readDataEx"
    ' MsgBox RowBuf.ColumnCount
    'RowBuf.RowCount = 1
    'MsgBox RowBuf.Value(0, 0)
    ' MsgBox RowBuf.RowCount
    'StartLocation = "XXX"

    'MsgBox StartLocation
    'MsgBox RowBuf.Value(0, 0)
  End Sub


  Private Sub form_Activate()
    Call EinstAct(AufbauPar.ier, Me)

  End Sub

  Private Sub Form_Initialize()
    TblRwertListNo = "TBL_RWERT_LIST_NO"
    If TablExist(Dtmp, TblRwertListNo) Then
      SqlEtmt = "DROP INDEX [RWERT_ID] ON " & TblRwertListNo
      Call SQLExeLock(0, Dtmp, SqlEtmt, ier)

      SqlEtmt = "DROP TABLE " & TblRwertListNo
      Call SQLExeLock(0, Dtmp, SqlEtmt, ier)
      If ier <> 0 Then Exit Sub
      Dtmp.TableDefs.Refresh()
      DoEvents()
    End If

    SqlEtmt = "CREATE TABLE " & TblRwertListNo & " ([RWERT_ID] LONG CONSTRAINT [RWERT_ID] PRIMARY KEY)"
    Call SQLExeLock(0, Dtmp, SqlEtmt, ier)
    If ier <> 0 Then Exit Sub

    '

    TblRwertList = "TBL_RWERT_LIST_YES"
    If TablExist(Dtmp, TblRwertList) Then
      SqlEtmt = "DROP INDEX [RWERT_ID] ON " & TblRwertList
      Call SQLExeLock(0, Dtmp, SqlEtmt, ier)
      SqlEtmt = "DROP TABLE " & TblRwertList
      Call SQLExeLock(0, Dtmp, SqlEtmt, ier)
      If ier <> 0 Then Exit Sub
      Dtmp.TableDefs.Refresh()
      DoEvents()
    End If

    SqlEtmt = "CREATE TABLE " & TblRwertList & " ([RWERT_ID] LONG CONSTRAINT [RWERT_ID] PRIMARY KEY)"
    Call SQLExeLock(0, Dtmp, SqlEtmt, ier)
    If ier <> 0 Then Exit Sub
    Dtmp.TableDefs.Refresh()
    '

  End Sub

  Private Sub form_Load()

    '
  End Sub


  Private Sub Form_Terminate()
    Exit Sub
    If TablExist(Dtmp, TblRwertList) Then
      SqlEtmt = "DROP TABLE " & TblRwertList
      Call SQLExeLock(0, Dtmp, SqlEtmt, ier)
    End If
    If TablExist(Dtmp, TblRwertListNo) Then
      SqlEtmt = "DROP TABLE " & TblRwertListNo
      Call SQLExeLock(0, Dtmp, SqlEtmt, ier)
    End If

  End Sub

  Private Sub Form_Unload(Cancel As Integer)
    GrpRwerte = Nothing
    ReWrRwert = Nothing
    RefWert = Nothing
    RezSozpt = Nothing
    RwWrRezept = Nothing
    MischGroup = Nothing
    '


    Try

      datREF(0).Recordset.Close()
      datREF(1).Recordset.Close()
      datREF(2).Recordset.Close()
      datREF(5).Recordset.Close()

    Catch ex As Exception

    End Try
    If BitWrt(4, MenueParam.User.Writ) = 1 Then
      'DbName = Ddat.Name
      'CloseDdat Ddat
      'OpenDdat Ddat, False, False, DbErr
    End If
    Ddat.TableDefs.Refresh()
    Exit Sub
DbRucErr:
    'MsgBox Texxt(3610) & Error
    MsgBox(Texxt(3610))
  End Sub





  Private Sub sstDATB_Click(PreviousTab As Integer)
    Dim indes As Integer
    indes = sstDATB.Tab
    Select Case indes
      Case 1, 3, 4
        txtKENN(indes) = MenueParam.Messg.Kenn
    End Select
  End Sub

  '**************************************************


 
 
 
End Class