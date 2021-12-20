Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmDARF
  Dim MnIopenArt As Boolean
  Dim MnUserID As Integer
  Dim MnMischID As Integer
  Dim MnMessgID As Integer
  Dim MnMethID As Integer
  Dim OldMischID As Integer
  Dim OldMessgID As Integer
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
  Dim AdaptMisch As OleDbDataAdapter
  Dim CmdMisch As OleDbCommand
  Dim TabMisch As DataTable
  Dim AdaptRwert As OleDbDataAdapter
  Dim CmdRwert As OleDbCommand
  Dim TabRwert As List(Of DataTable)
  Dim AdaptUser As OleDbDataAdapter
  Dim CmdUser As OleDbCommand
  Dim TabUser As DataTable

  Dim AdaptWerte_2 As OleDbDataAdapter
  Dim CmdWerte_2 As OleDbCommand
  Dim TabWerte_2 As DataTable
  Dim AdaptWerte_3 As OleDbDataAdapter
  Dim CmdWerte_3 As OleDbCommand
  Dim TabWerte_3 As DataTable
  '
  Dim TabWerte As List(Of DataTable)

  Dim Winkel As AngGeos
  Dim Strgid As String
  Dim SqlStmt As String
  Dim SqlEtmt As String
  Dim ForeignTabNam() As String
  Dim ForTabCount As Integer
  Dim RowWrt As Object
  Dim TblRwertList As DataTable
  Dim AdaptList As OleDbDataAdapter
  Dim CmdList As OleDbCommand
  Dim TblRwertListNo As DataTable
  Dim AdaptListNo As OleDbDataAdapter
  Dim CmdListNo As OleDbCommand

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
  Dim RezSozpt As RecipesGrp
  Dim ReWrRezept As ReadWriteRezept
  Dim HandleRwrtn As HandleRwerte
  Dim HandleRwert As List(Of HandleRwerte)
  Dim HandleRezept As HandleRezGeneral
  Dim VKwb(1) As Integer
  Dim RefWert As RefValue
  Dim RefID As Integer
  Dim GroupID As Integer
  Dim Ende As Boolean

  Private Sub frmDARF_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    Dim i As Integer
    Try
      For i = 0 To HandleRwert.Count - 1
        HandleRwert(i) = Nothing
      Next
      HandleRwert = Nothing
      HandleRezept = Nothing
      HandleRwrtn = Nothing
    Catch
    End Try
  End Sub

  Private Sub frmDARF_FormClosing(sender As Object, e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing

  End Sub

  Private Sub frmDARF_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim j As Integer
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    '
    '
    Winkel = MenueParam.User.Winkel
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
    btnRUN.Add(btnRUN_2)
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
    TabRwert = New List(Of DataTable)
    TabRwert.Add(New DataTable)
    TabRwert.Add(New DataTable)
    TabRwert.Add(New DataTable)
    TabRwert.Add(Nothing)
    TabRwert.Add(New DataTable)

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
    txtVON.Add(txtVON_0)
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

    If MnIopenArt Then
      OldMischID = -1
      OldMessgID = -1
      '
      'Datenbank exclusiv öffnen
      '
      Cursor = Cursors.WaitCursor
      Call CreateCopyDatabase(Ende)
      Cursor = Cursors.Default

      If Ende Then
        Me.Close()
        Exit Sub
      End If
      cboMESS.Enabled = True
      cboUSER.Enabled = True
      TabDARF.SelectedIndex = 0
    Else
      TabDARF.TabPages(0).Enabled = False
      TabDARF.SelectedIndex = 1
      cboMESS.Enabled = False
      cboUSER.Enabled = False
      OldMischID = MenueParam.MischID
      OldMessgID = MenueParam.MessgID
    End If
    '
    '
    '
    '
    '
    For i = 0 To lblGRP.Count - 1
      lblGRP(i).Text = Texxt(386)
    Next
    For i = 0 To chkARC.Count - 1
      If Not IsNothing(chkARC(i)) Then
        chkARC(i).Text = Texxt(374)
        chkARC(i).CheckState = CheckState.Unchecked
      End If
    Next
    '
    For i = 0 To btnMARK.Count - 1
      If Not IsNothing(btnMARK(i)) Then
        btnMARK(i).Text = Texxt(3670)
      End If
    Next
    '
    '
    For i = 0 To btnZEIG.Count - 1
      If Not IsNothing(btnZEIG(i)) Then
        btnZEIG(i).Text = Texxt(3690)
      End If
    Next
    '
    Me.Text = Texxt(1908) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    '
    '
    btnRUN(0).Text = Texxt(3691)
    btnRUN(1).Text = Texxt(3692)
    btnRUN(2).Text = Texxt(3682)
    btnRUN(3).Text = Texxt(3683)
    btnRUN(4).Text = Texxt(3693)
    btnEnd.Text = Texxt(490)

    '
    '
    btnCLIP(2).Text = Texxt(3662)
    btnCLIP(3).Text = Texxt(3663)
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
        lblANZ(i).Text = Texxt(3689)
      End If
    Next
    '
    '
    '
    '
    For i = 0 To chkDAT.Count - 1
      If Not IsNothing(chkDAT(i)) Then
        chkDAT(i).Text = Texxt(375)
      End If
    Next
    '
    '
    '
    For i = 0 To lblVON.Count - 1
      If Not IsNothing(lblVON(i)) Then
        lblVON(i).Text = Texxt(376)
      End If
    Next
    '
    '
    '
    '
    For i = 0 To lblBIS.Count - 1
      If Not IsNothing(lblBIS(i)) Then
        lblBIS(i).Text = Texxt(377)
      End If
    Next
    '
    '
    '
    '
    '
    For i = 0 To lblSQL.Count - 1
      If Not IsNothing(lblSQL(i)) Then
        lblSQL(i).Text = Texxt(369)
      End If
    Next
    '
    '
    '
    For i = 0 To lblKENN.Count - 1
      If Not IsNothing(lblKENN(i)) Then
        lblKENN(i).Text = Texxt(281)
      End If
    Next
    '
    '
    chkDUP.Text = Texxt(3652)
    lblDIK.Text = Texxt(832)
    lblGRPR.Text = Texxt(3669)
    chkARCN.Text = Texxt(3660)
    chkANEU.Text = Texxt(374)
    lblMSH.Text = Texxt(96)
    lblMESS.Text = Texxt(204)
    lblUSE.Text = Texxt(201)
    chkGRPN.Text = Texxt(3661)
    chkARCR.Text = Texxt(2425)
    lblHinweis.Text = Texxt(3566)
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

    If MenueParam.MischID = -1 Then
      TabDARF.TabPages(4).Enabled = False
    End If


    GrpRwerte = New RefValuesGrp
    ReWrRwert = New ReadWriteRwert
    RefWert = New RefValue
    '
    RezSozpt = New RecipesGrp
    ReWrRezept = New ReadWriteRezept
    HandleRezept = New HandleRezGeneral
    HandleRezept.cboGRP = cboGRPR
    HandleRezept.lblGRP = lblGRPR
    HandleRwrtn = New HandleRwerte
    HandleRwert = New List(Of HandleRwerte)
    For i = 0 To cbogrp.Count - 1
      HandleRwert.Add(New HandleRwerte)
    Next
    If MenueParam.MischID <> -1 Then
      txtDIK.Text = MenueParam.Misch.Dicke
    End If
    '
    '
    '
    TabUser = New DataTable
    AdaptUser = New OleDbDataAdapter
    CmdUser = New OleDbCommand("", Cncol)
    AdaptUser.SelectCommand = CmdUser
    '
    '
    '
    TabMess = New DataTable
    AdaptMess = New OleDbDataAdapter
    CmdMess = New OleDbCommand("", Cncol)
    AdaptMess.SelectCommand = CmdMess
    TabMisch = New DataTable
    AdaptMisch = New OleDbDataAdapter
    CmdMisch = New OleDbCommand("", Cncol)
    AdaptMisch.SelectCommand = CmdMisch
    AdaptRwert = New OleDbDataAdapter
    CmdRwert = New OleDbCommand("", Cndat)
    AdaptRwert.SelectCommand = CmdRwert

    '
    For i = 0 To dbgREF.Count - 1
      If Not IsNothing(dbgREF(i)) Then
        dbgREF(i).DataSource = TabRwert(i)
        dbgREF(i).AlternatingRowsDefaultCellStyle.BackColor = Color.Azure
        dbgREF(i).DefaultCellStyle.BackColor = Color.Beige
        dbgREF(i).AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
        dbgREF(i).AllowUserToAddRows = False
        dbgREF(i).AllowUserToDeleteRows = False
        dbgREF(i).ReadOnly = True
        dbgREF(i).Columns.Add("RWERT_ID", "ID")
        dbgREF(i).Columns(0).Visible = False
        dbgREF(i).Columns.Add("RWERT_DATTIM", Texxt(2205))
        dbgREF(i).Columns.Add("RWERT_NAME", Texxt(370))
        dbgREF(i).Columns(1).Width = 80
        dbgREF(i).Columns(0).DataPropertyName = dbgREF(i).Columns(0).Name
        dbgREF(i).Columns(1).DataPropertyName = dbgREF(i).Columns(1).Name
        dbgREF(i).Columns(2).DataPropertyName = dbgREF(i).Columns(2).Name
      End If
    Next
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
    For i = 0 To TabWerte.Count - 1
      If Not IsNothing(grdWRT(i)) Then
        grdWRT(i).DataSource = TabWerte(i)
        grdWRT(i).AllowUserToAddRows = False
        grdWRT(i).AllowUserToDeleteRows = False
        grdWRT(i).AlternatingRowsDefaultCellStyle.BackColor = Color.AntiqueWhite
        grdWRT(i).RowHeadersVisible = False
        grdWRT(i).SelectionMode = DataGridViewSelectionMode.RowHeaderSelect
      End If
    Next i
    '
    '
    'Benutzer
    '
    '


    '
    '
    SqlStmt = "SELECT * FROM TBL_USER ORDER BY USER_NAME"
    CmdUser.CommandText = SqlStmt
    If Not FillDatset(AdaptUser, TabUser) Then
      Exit Sub
    End If

    cboMESS.Enabled = False
    cboUSER.Enabled = False
    cboUSER.DataSource = TabUser
    cboUSER.DisplayMember = "USER_NAME"
    cboUSER.ValueMember = "USER_ID"
    cboUSER.SelectedIndex = -1
    cboUSER.Enabled = True
    If MenueParam.UserID = -1 Then
      cboUSER.SelectedValue = TabUser.Rows(0)("USER_ID")
    Else
      cboUSER.SelectedValue = MenueParam.UserID
    End If

    'cboMESS.Enabled = True
    If Not MnIopenArt Then
      cboUSER.Enabled = False
      cboMESS.Enabled = False
    End If

    'Menue-Parameter uebernehmen
    '
    '

    '
    txtbis(0).Text = Format$(DateAdd("d", -1 * 365, Today))
    txtVON(0).Text = Format$(DateAdd("d", -2 * 365, Today))
    For j = 1 To 2
      txtbis(j).Text = Format$(DateAdd("d", 0, Today))
      txtVON(j).Text = Format$(DateAdd("d", -1 * 365, Today))
    Next j
    txtbis(4).Text = Format$(DateAdd("d", 0, Today))
    txtVON(4).Text = Format$(DateAdd("d", -1 * 365, Today))
    '
  End Sub

  Private Sub frmDARF_Paint(sender As Object, e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint
    TabDARF.ItemSize = New Size((TabDARF.Width - TabDARF.TabPages.Count) / TabDARF.TabPages.Count - 3, TabDARF.ItemSize.Height)
  End Sub

  Private Sub frmDARF_Resize(sender As Object, e As System.EventArgs) Handles Me.Resize
  End Sub

  Private Sub cboUSER_Changed(sender As Object, e As System.EventArgs) Handles cboUSER.SelectedValueChanged, cboUSER.EnabledChanged
    If Not IsNumeric(cboUSER.SelectedValue) Then Exit Sub
    If Not cboUSER.Enabled Then Exit Sub
    If MenueParam.UserID <> cboUSER.SelectedValue Then
      AufbauPar.UserID = cboUSER.SelectedValue

    End If
    '
    'Messgeräte
    '
    '
    cboMESS.DataSource = Nothing
    '
    'INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID=TBL_MISCH_MESSG.MISCH_ID
    '
    TabMess.Rows.Clear()
    SqlStmt = "SELECT *,TBL_MESSG.MESSG_ID AS MESSG_ID FROM TBL_MESSG INNER JOIN TBL_USER_MESSG ON TBL_MESSG.MESSG_ID=TBL_USER_MESSG.MESSG_ID WHERE USER_ID=" & cboUSER.SelectedValue & " ORDER BY MESSG_KBEZ"
    CmdMess.CommandText = SqlStmt
    If Not FillDatset(AdaptMess, TabMess) Then
      Exit Sub
    End If

    cboMESS.DataSource = TabMess

    cboMESS.Enabled = False
    cboMESS.DisplayMember = "MESSG_KBEZ"
    cboMESS.ValueMember = "MESSG_ID"
    cboMESS.SelectedIndex = -1
    cboMESS.Enabled = True
    If OldMessgID = -1 Then
      cboMESS.SelectedValue = TabMess.Rows(0)("MESSG_ID")
    Else
      cboMESS.SelectedValue = MenueParam.MessgID
    End If

    cboMESS.Enabled = True
  End Sub
  Private Sub cboMESS_Changed(sender As Object, e As System.EventArgs) Handles cboMESS.EnabledChanged, cboMESS.SelectedValueChanged
    Dim i As Integer
    If IsNothing(cboMESS.DataSource) Then Exit Sub
    If Not cboMESS.Enabled Then Exit Sub
    'If MenueParam.UserID = -1 Then Exit Sub
    If IsNothing(cboMESS.SelectedValue) OrElse Not IsNumeric(cboMESS.SelectedValue) Then Exit Sub
    If cboMESS.SelectedIndex = -1 Then Exit Sub
    '
    '
    '
    'Clear Tables
    '
    '
    TabWerte(2).Clear()
    TabWerte(3).Clear()
    For i = 0 To TabRwert.Count - 1
      If Not IsNothing(TabRwert(i)) Then
        TabRwert(i).Clear()
      End If
    Next
    '


    '
    
    '
    '
    '
    'Mischsysteme
    '
    '
    '
    '
    SqlStmt = "SELECT TBL_MISCH.MISCH_ID AS MISCH_ID,MISCH_KBEZ FROM TBL_MISCH INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID=TBL_MISCH_MESSG.MISCH_ID  WHERE MESSG_ID=" & cboMESS.SelectedValue & " ORDER BY MISCH_KBEZ"
    '
    CmdMisch.CommandText = SqlStmt
    If Not FillDatset(AdaptMisch, TabMisch) Then
      Exit Sub
    End If
    '
    '

    cboMSH.Enabled = False
    AufbauPar.MischID = -1
    AufbauPar.MessgID = -1
    cboMSH.DisplayMember = "MISCH_KBEZ"
    cboMSH.ValueMember = "MISCH_ID"
    cboMSH.DataSource = TabMisch
    cboMSH.Enabled = True

    If TabMisch.Rows.Count = 0 Then
      cboMSH.SelectedIndex = -1
    Else
      If OldMischID = -1 Then
        cboMSH.SelectedValue = TabMisch.Rows(0)("MISCH_ID")
      Else
        cboMSH.SelectedValue = OldMischID
      End If
    End If




    If IsNumeric(cboMSH.SelectedValue) Then
      AufbauPar.MischID = cboMSH.SelectedValue
    Else
      AufbauPar.MischID = -1
      TabPage4.Enabled = False
    End If
    AufbauPar.MessgID = cboMESS.SelectedValue
    AufbauPar.UserID = cboUSER.SelectedValue

    For i = 0 To HandleRwert.Count - 1
      HandleRwert(i).cboGRP = cbogrp(i)
      HandleRwert(i).lblGRP = lblGRP(i)
      HandleRwert(i).GRoupList()
      If BitWrt(1, MenueParam.User.Enabl) Then
        If Not IsNothing(chkARC(i)) Then
          chkARC(i).Enabled = True
        End If
      Else
        If Not IsNothing(chkARC(i)) Then
          chkARC(i).Enabled = False
        End If
      End If
      If BitWrt(1, MenueParam.User.Visbl) Then
        If Not IsNothing(chkARC(i)) Then
          chkARC(i).Visible = True
        End If
      Else
        If Not IsNothing(chkARC(i)) Then
          chkARC(i).Visible = False
        End If
      End If
    Next

    '
    '
    HandleRwrtn.cboGRP = cboGRPN
    HandleRwrtn.lblGRP = lblGRPN
    HandleRwrtn.GRoupList()
    cboGRPN.SelectedIndex = 0
    lblGRPN.Visible = False
    cboGRPN.Visible = False

    '
  End Sub
  '
  '
  '
  '
  '
  '
  Private Sub cboGRP_Click(sender As Object, e As System.EventArgs) Handles _
   cboGRP_0.Click, cboGRP_1.Click, cboGRP_2.Click, cboGRP_3.Click, cboGRP_4.Click
    Dim index As Integer
    index = CInt(sender.name.substring(7, 1))
    If IsNothing(cbogrp(index).SelectedValue) OrElse Not IsNumeric(cbogrp(index).SelectedValue) Then Exit Sub

    MenueParam.Messg.RwrtGID = cbogrp(index).SelectedValue

  End Sub
  '
  '
  '
  Private Sub btnCLIP_Click(sender As Object, e As System.EventArgs) Handles btnCLIP_2.Click, btnCLIP_3.Click
    Dim index As Integer
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
        grdWRT(index).ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
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
        Call HandleRwert(index).MakeTABRefwerte(False, MenueParam.Messg.Kbez, Winkel, TabWerte(index))

        Call ClipboardToTable(True, TabWerte(index), ";", ier)
        If ier <> 0 Then
          MsgBox("Error: " & CStr(ier))
          Cursor = Cursors.Default
          Exit Sub
        End If
        Call HandleRwert(index).MakeGRIDRefwerte(Winkel, grdWRT(index))
        grdWRT(index).Visible = True
        btnRUN(index).Enabled = True
    End Select

    Exit Sub

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
      chkARC(1).Visible = True
      If chkDAT(1).Checked Then
        lblVON(1).Visible = True
        lblBIS(1).Visible = True
        txtVON(1).Visible = True
        txtbis(1).Visible = True
      End If
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
    Dim i As Integer
    Dim j As Integer
    Dim kw As Integer
    Dim KeyMeng As String
    Dim RezID As Integer
    Dim TypID(1) As Integer
    Dim SmpID(1) As Integer
    Dim UntID(1) As Integer
    Dim CHRM As String
    Cursor = Cursors.WaitCursor
    Application.DoEvents()
    index = CInt(sender.name.substring(7, 1))
    Select Case index
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
        If Not HandleRwert(index).MeldSpeiAllRwrt(True) Then
          Cursor = Cursors.Default
          Exit Sub
        End If

        btnZEIG(index).Enabled = False
        '
        '
        '

        '
        '
        '
        '
        'ausgewählte Sätze löschen
        '
        '
        Try
          TabDARF.TabPages(index).Enabled = False

          If ConnOpen(Cndat) Then
            For i = 0 To dbgREF(index).SelectedRows.Count - 1
              RefID = dbgREF(index).SelectedRows(i).Cells("RWERT_ID").Value
              GroupID = cbogrp(index).SelectedValue
              Call ReWrRwert.DelRwert(RefID, GroupID, ier)
            Next i
            Cndat.Close()
          End If
          TabDARF.TabPages(index).Enabled = True
          btnZEIG(index).Enabled = True
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
          If Not HandleRwrtn.MeldSpeiAllRwrt(False) Then
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
        TabDARF.TabPages(index).Enabled = False
        If ConnOpen(Cndat) Then
          For i = 0 To dbgREF(index).SelectedRows.Count - 1
            RefID = dbgREF(index).SelectedRows(i).Cells("RWERT_ID").Value


            'datREF(index).Recordset.BookMark = dbgREF(index).SelBookmarks(i)
            SqlStmt = "UPDATE TBL_RWERT " & Strgid & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID _
            & " AND [RWERT_ID]=" & RefID
            CmdRwert.CommandText = SqlStmt
            If SQLExeNonQuery(CmdRwert, Cndat) = 0 Then

            End If
          Next i
          Cndat.Close()
        End If
        TabDARF.TabPages(index).Enabled = True
        Cursor = Cursors.Default
        btnZEIG(index).Enabled = True

      Case 2
        '
        '
        If dbgREF(index).Rows.Count = 0 Then
          Cursor = Cursors.Default
          Exit Sub
        End If '
        'Tabelle (Grid) aufbauen aus ACCESS
        '
        '
        '
        'Tabelle Tabwerte aufbauen
        '
        '

        '
        '
        Call HandleRwert(index).MakeTABRefwerte(False, MenueParam.Messg.Kbez, Winkel, TabWerte(index))
        '
        Call HandleRwert(index).MakeGRIDRefwerte(Winkel, grdWRT(index))


        '
        TabDARF.TabPages(index).Enabled = False
        If ConnOpen(Cndat) Then
          For i = 0 To dbgREF(index).SelectedRows.Count - 1
            RefID = dbgREF(index).SelectedRows(i).Cells("RWERT_ID").Value

            '
            '
            'Tabelle aufbauen
            '
            '
            ReWrRwert.ReadRwert(RefID, RefWert, ier)

            j = Abs(RefWert.kwb) + 1
            Call HandleRwert(index).TabRefRecord(j, Winkel, 100.0, RefWert, TabWerte(index), ier)
            If ier > 0 Then Exit Sub

          Next i
          Cndat.Close()
        End If
        TabDARF.TabPages(index).Enabled = True
        '
        '
        'Gridparameter
        '
        '
        '
        '
        '
        If dbgREF(index).Rows.Count = 0 Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If dbgREF(index).SelectedRows.Count = 0 Then
          MsgBox(Texxt(3565))
          dbgREF(index).Visible = True
          grdWRT(index).Visible = False
        Else
          grdWRT(index).Visible = True
          dbgREF(index).Visible = False
          btnCLIP(index).Enabled = True
          btnMARK(index).Enabled = False
        End If

        Application.DoEvents()
        '
        '

        Cursor = Cursors.Default

        '
      Case 3
        '
        '

        'Tabelle nach ACCESS umspeichern
        '
        '
        If Not HandleRwert(index).MeldSpeiAllRwrt(False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If


        '
        '
        'Prüfen, ob richtige Wellenlängen verfügbar
        'grdWRT(index).row = 0

        For j = 0 To Winkel.Wsol.Nwe - 1
          If CSng(grdWRT(index).Columns(j + 5).HeaderText) <> Winkel.Wsol.R(j) Then
            MsgBox(Texxt(3023))
            Cursor = Cursors.Default
            Exit Sub
          End If
        Next j
        '
        '
        '
        '
        RefWert.Cme = txtKENN(index).Text
        RefWert.Iarch = chkARC(index).CheckState
        RefWert.Gid = cbogrp(index).SelectedValue
        RefWert.DatTim = Today
        RefWert.ReTr = 0
        RefWert.Banum = Space(1)
        RefWert.Iami = 1
        RefWert.MessgID = MenueParam.MessgID
        For j = 0 To Winkel.Km - 1
          RefWert.De(j) = 0
        Next j
        RefWert.RefKurv.clear()
        Call ADDCurves(RefWert.RefKurv)
        TabDARF.TabPages(index).Enabled = False
        If ConnOpen(Cndat) Then
          For j = 0 To grdWRT(index).Rows.Count - 1 Step Winkel.Km
            If Asc(Trim(grdWRT(index).Rows(j).Cells(0).Value)) < 97 And Asc(Trim(grdWRT(index).Rows(j).Cells(0).Value)) > 122 Then
              If Asc(Trim(grdWRT(index).Text)) <> 49 Then
                MsgBox(Texxt(2925))
                Cursor = Cursors.Default
                Exit Sub
              End If
            End If
            RefWert.Name = grdWRT(index).Rows(j).Cells(1).Value
            RefWert.Bem = grdWRT(index).Rows(j).Cells(2).Value
            RefWert.DatTim = grdWRT(index).Rows(j).Cells(3).Value
            For kw = 0 To Winkel.Km - 1
              CHRM = Winkel(kw).Chrm
              If grdWRT(index).Rows(j + kw).Cells(4).Value <> CHRM Then
                MsgBox(Texxt(3606) & Space(2) & grdWRT(index).Rows(j + kw).Cells(4).Value & "  <<====>>  " & CHRM)
                Cursor = Cursors.Default
                Exit Sub
              End If
              If grdWRT(index).Rows(j + kw).Cells.Count <> Winkel.Wsol.Nwe + 5 Then
                MsgBox(Texxt(3054))
                Cursor = Cursors.Default
                Exit Sub
              End If
              For i = 0 To Winkel.Wsol.Nwe - 1
                RefWert.RefKurv(CHRM).R(i) = 0.01 * Singl(grdWRT(index).Rows(j + kw).Cells(i + 5).Value)
              Next i
            Next kw
            '
            '
            'Tabelle umspeichern
            '
            '
            ReWrRwert.WriteRwert(RefWert.ID, RefWert, ier)
          Next j
          Cndat.Close()
        End If
        TabDARF.TabPages(index).Enabled = True



        '
        '
      Case 4
        '
        '
        'Sätze in Colorthek speichern
        '
        If dbgREF(index).Rows.Count = 0 Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        imsg = MsgBox(Texxt(2946), 4, Texxt(2000))
        If imsg = 7 Then
          Cursor = Cursors.Default
          Exit Sub
        End If


        KeyMeng = "MNG"
        RezSozpt.Rezepte.clear()
        RezSozpt.Rezepte.AddRez(KeyMeng, New Recipe)
        RezSozpt.Rezepte.kwb(0) = VKwb(0) - 1
        RezSozpt.Rezepte.kwb(1) = VKwb(1) - 1
        RezSozpt.Rezepte(KeyMeng).Dicke(VKwb(0) - 1) = txtDIK.Text
        RezSozpt.Rezepte(KeyMeng).Dicke(VKwb(1) - 1) = 0.0
        RezSozpt.Rezepte(KeyMeng).Gid = cboGRPR.SelectedValue
        RezSozpt.Rezepte(KeyMeng).DatTim = Now
        RezSozpt.Rezepte(KeyMeng).Iarch = chkARCR.CheckState
        For i = 0 To 1
          TypID(i) = -1
          SmpID(i) = -1
          UntID(i) = -1
        Next i

        '
        '
        '
        '
        'Ausgewählte Sätze in Colorthek speichern
        '
        '
        If ConnOpen(Cndat) Then
          For i = 0 To dbgREF(index).SelectedRows.Count - 1
            RefID = dbgREF(index).SelectedRows(i).Cells("RWERT_ID").Value

            RezSozpt.Rezepte(KeyMeng).Name = dbgREF(index).SelectedRows(i).Cells("RWERT_NAME").Value
            SmpID(VKwb(0) - 1) = RefID
            ReWrRezept.AddRezept(KeyMeng, RezID, RezSozpt, UntID, TypID, SmpID, ier)
          Next i
          Cndat.Close()
        End If
        dbgREF(index).Columns("RWERT_ID").Visible = False


        Cursor = Cursors.Default
        btnZEIG(index).Enabled = True

    End Select
    Cursor = Cursors.Default
  End Sub


  Private Sub btnZEIG_Click(sender As Object, e As System.EventArgs) Handles _
   btnZEIG_0.Click, btnZEIG_1.Click, btnZEIG_2.Click, btnZEIG_4.Click
    Dim index As Integer
    Dim StrRwert As String
    Dim StrGid As String
    Dim SqlStmt As String
    index = CInt(sender.name.substring(8, 1))
    If Not IsDate(txtVON(index).Text) Then
      MsgBox(Texxt(2500))
      Exit Sub
    End If
    If Not IsDate(txtbis(index).Text) Then
      MsgBox(Texxt(2500))
      Exit Sub
    End If
   
   
    Cursor = Cursors.WaitCursor

    Select Case index
      Case 0
        btnRUN(index).Enabled = False
        btnMARK(index).Enabled = False
        Application.DoEvents()
        dbgREF(index).Visible = True

        txtANZ(index).Visible = False
        lblANZ(index).Visible = False

        '
        '
        '
        'Aufbau der dbgGRID-Tabelle zum Löschen
        '
        '
        '
        '

        StrGid = "("
        If cbogrp(index).SelectedValue > 0 Then
          StrGid = "RWERT_GID= " & cbogrp(index).SelectedValue & " AND " & StrGid
        End If
        StrGid = StrGid & " RWERT_IARCH=2 OR "
        StrGid = StrGid & " RWERT_IARCH=0)"
        StrGid = " WHERE " & StrGid
        If chkDAT(index).Checked Then
          StrGid = StrGid & " AND RWERT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON(index).Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtbis(index).Text)))
        End If
        If Trim(txtSQL(index).Text) <> "" Then
          StrGid = StrGid & " AND " & StrSelct("RWERT_NAME", txtSQL(index).Text)
        End If
        '
        AdaptList = New OleDbDataAdapter
        CmdList = New OleDbCommand("", Cndat)
        AdaptList.SelectCommand = CmdList
        TblRwertList = New DataTable
        TblRwertList.Clear()
        '
        '
        AdaptListNo = New OleDbDataAdapter
        CmdListNo = New OleDbCommand("", Cndat)
        AdaptListNo.SelectCommand = CmdListNo
        TblRwertListNo = New DataTable
        TblRwertListNo.Clear()
        '
        'R-Werte bei Sortimenten werden umgespeichert
        '
        '
        '
        '
        SqlStmt = "SELECT DISTINCT TBL_RWERT.RWERT_ID FROM TBL_RWERT" _
        & " INNER JOIN TBL_SORTI_RWERT ON (TBL_RWERT.MESSGRW_ID=TBL_SORTI_RWERT.MESSGRW_ID) " _
        & " AND (TBL_RWERT.RWERT_ID=TBL_SORTI_RWERT.RWERT_ID) " _
        & StrGid & " AND TBL_SORTI_RWERT.MESSGRW_ID=" & MenueParam.Messg.MessgRwID
        '
        '
        '
        CmdListNo.CommandText = SqlStmt
        If Not FillDatset(AdaptListNo, TblRwertListNo) Then
          Exit Sub
        End If
        '
        'dsgl Rezepte 
        '
        SqlStmt = "SELECT DISTINCT TBL_RWERT.RWERT_ID FROM TBL_RWERT" _
        & " INNER JOIN TBL_REZEPT_RWERT ON (TBL_RWERT.MESSGRW_ID=TBL_REZEPT_RWERT.MESSGRW_ID) " _
        & " AND (TBL_RWERT.RWERT_ID=TBL_REZEPT_RWERT.RWERT_ID) " _
        & StrGid & " AND TBL_REZEPT_RWERT.MESSGRW_ID=" & MenueParam.Messg.MessgRwID
        '
        '
        '
        CmdListNo.CommandText = SqlStmt
        If Not FillDatset(AdaptListNo, TblRwertListNo) Then
          Exit Sub
        End If
        '
        '
        StrRwert = StrLin(TblRwertListNo, "RWERT_ID")
        '
        SqlStmt = "SELECT RWERT_ID FROM TBL_RWERT" & StrGid _
         & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID NOT IN " & StrRwert
        '
        '
        CmdList.CommandText = SqlStmt
        If Not FillDatset(AdaptList, TblRwertList) Then
          Exit Sub
        End If
        '
        '
        StrRwert = StrLin(TblRwertList, "RWERT_ID")
        '

       
        '
        '
        SqlStmt = "SELECT TBL_RWERT.RWERT_ID AS RWERT_ID,RWERT_DATTIM,RWERT_NAME FROM TBL_RWERT" _
             & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID _
             & " AND RWERT_ID IN " & StrRwert

        SqlStmt = SqlStmt & " ORDER BY RWERT_NAME"
        CmdRwert.CommandText = SqlStmt
        TabRwert(index).Clear()
        If Not FillDatset(AdaptRwert, TabRwert(index)) Then
          Exit Sub
        End If
        If dbgREF(index).Rows.Count = 0 Then
          MsgBox(Texxt(2959))
        Else
          txtANZ(index).Visible = True
          lblANZ(index).Visible = True
          txtANZ(index).Text = CInt(TabRwert(index).Rows.Count)
          '
          btnRUN(index).Enabled = True
          btnMARK(index).Enabled = True
        End If
        '
        '
        '

        Cursor = Cursors.Default

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

        btnRUN(index).Enabled = False
        btnMARK(index).Enabled = False
        Application.DoEvents()
        dbgREF(index).Visible = True
        If index = 2 Then
          grdWRT(index).Visible = False
          btnCLIP(index).Enabled = False
        End If
        txtANZ(index).Visible = False
        lblANZ(index).Visible = False
       
        If chkDUP.Checked And index = 1 Then
          SqlStmt = "SELECT RWERT_ID,RWERT_DATTIM,RWERT_NAME FROM TBL_RWERT" _
          & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_NAME IN (SELECT RWERT_NAME FROM TBL_RWERT" _
          & " AS TMP_NAME WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " GROUP BY RWERT_NAME HAVING COUNT(*)>1) ORDER BY RWERT_NAME"
        Else

          StrGid = ""
          If cbogrp(index).SelectedValue > 0 Then
            StrGid = " AND RWERT_GID= " & cbogrp(index).SelectedValue
          End If
          If chkARC(index).CheckState < 2 Then
            StrGid = StrGid & " AND RWERT_IARCH=" & chkARC(index).CheckState
          End If
          SqlStmt = "SELECT RWERT_ID,RWERT_DATTIM,RWERT_NAME FROM TBL_RWERT" _
          & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID
          If chkDAT(index).Checked Then
            SqlStmt = SqlStmt & " AND (RWERT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON(index).Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtbis(index).Text))) & ")" & StrGid
          Else
            SqlStmt = SqlStmt & StrGid
          End If
          If Trim(txtSQL(index).Text) <> "" Then
            SqlStmt = SqlStmt & " AND " & StrSelct("RWERT_NAME", txtSQL(index).Text)
          End If
        End If
        '
        '
        CmdRwert.CommandText = SqlStmt
        TabRwert(index).Clear()
        CmdRwert.CommandText = SqlStmt
        If Not FillDatset(AdaptRwert, TabRwert(index)) Then
          Exit Sub
        End If
        If dbgREF(index).Rows.Count = 0 Then
          MsgBox(Texxt(2959))
        Else
          txtANZ(index).Visible = True
          lblANZ(index).Visible = True
          txtANZ(index).Text = CInt(dbgREF(index).Rows.Count)
          btnRUN(index).Enabled = True
          btnMARK(index).Enabled = True
        End If
        '
        '
        '
        '
        Cursor = Cursors.Default

      Case 3
      Case 4
        '
        '
        '
        '
        'Aufbau der dbgGRID-Tabelle zum Speichern der R-Werte in Colorthek
        '
        '
        '
        AufbauPar.MischID = cboMSH.SelectedValue
        '
        '
        '
        btnRUN(index).Enabled = False
        btnMARK(index).Enabled = False
        Application.DoEvents()
        dbgREF(index).Visible = True
        txtANZ(index).Visible = False
        lblANZ(index).Visible = False
        '
        '
        '
        '
        AdaptList = New OleDbDataAdapter
        CmdList = New OleDbCommand("", Cndat)
        AdaptList.SelectCommand = CmdList
        TblRwertList = New DataTable
        TblRwertList.Clear()
        SqlStmt = "SELECT RWERT_ID FROM TBL_REZEPT_RWERT WHERE MISCH_ID= " & MenueParam.MischID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_KWB=" & VKwb(0)

        CmdList.CommandText = SqlStmt
        If Not FillDatset(AdaptList, TblRwertList) Then
          Exit Sub
        End If
        '
        '
        StrRwert = StrLin(TblRwertList, "RWERT_ID")

        StrGid = ""
        If cbogrp(index).SelectedValue > 0 Then
          StrGid = " AND RWERT_GID= " & cbogrp(index).SelectedValue
        End If
        If chkARC(index).CheckState < 2 Then
          StrGid = StrGid & " AND RWERT_IARCH=" & chkARC(index).CheckState
        End If
        SqlStmt = "SELECT RWERT_ID,RWERT_DATTIM,RWERT_NAME FROM TBL_RWERT" _
        & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND (RWERT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON(index).Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtbis(index).Text))) & ")" & StrGid
        If Trim(txtSQL(index).Text) <> "" Then
          SqlStmt = SqlStmt & " AND " & StrSelct("RWERT_NAME", txtSQL(index).Text)
        End If
        SqlStmt = SqlStmt & " AND RWERT_ID NOT IN  " & StrRwert
        '
        '
        SqlStmt = SqlStmt & " ORDER BY RWERT_NAME"
        '
        '
        CmdRwert.CommandText = SqlStmt
        TabRwert(index).Clear()
        If Not FillDatset(AdaptRwert, TabRwert(index)) Then
          Exit Sub
        End If
        If dbgREF(index).Rows.Count = 0 Then
          MsgBox(Texxt(2959))
        Else
          txtANZ(index).Visible = True
          lblANZ(index).Visible = True
          txtANZ(index).Text = CInt(dbgREF(index).Rows.Count)
          btnRUN(index).Enabled = True
          btnMARK(index).Enabled = True
          cboMSH.Enabled = False
        End If
        '
        '
        '
        '

        Cursor = Cursors.Default

    End Select
    Exit Sub
FehlZeig:
    txtANZ(index).Visible = False
    lblANZ(index).Visible = False
    Cursor = Cursors.Default
  End Sub
  '
  '
  '
  '
  Private Sub cboMSH_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboMSH.SelectedIndexChanged
    If Not cboMSH.Enabled Then Exit Sub
    If Not IsNothing(cboMSH.SelectedValue) AndAlso IsNumeric(cboMSH.SelectedValue) Then
      AufbauPar.UserID = -1
      AufbauPar.MessgID = -1

      AufbauPar.MischID = cboMSH.SelectedValue
      AufbauPar.MessgID = cboMESS.SelectedValue
      AufbauPar.UserID = cboUSER.SelectedValue
      TabDARF.TabPages(4).Enabled = True
    Else
      TabDARF.TabPages(4).Enabled = False
      Exit Sub
    End If
    HandleRezept.GroupList()
    HandleRezept.cboGRP.Enabled = True
    HandleRezept.cboGRP.Visible = True
    HandleRezept.lblGRP.Enabled = True
    HandleRezept.lblGRP.Visible = True

    VKwb(0) = 1
    VKwb(1) = 2
    If MenueParam.Misch.Vert = 1 Then
      VKwb(0) = 2
      VKwb(1) = 1
    End If

  End Sub
  '
  '
  '
  Private Sub TabDARF_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles TabDARF.SelectedIndexChanged
    Dim index As Integer
    Dim i As Integer

    index = sender.selectedindex
    Select Case index
      Case 1, 3
        txtKENN(index).Text = MenueParam.Messg.Kenn
      Case 4
        If IsNumeric(cboMSH.SelectedValue) Then
          TabDARF.TabPages(index).Enabled = True
          cboMSH.Enabled = True
        Else
          TabDARF.SelectTab(1)
          TabDARF.SelectedIndex = 1
        End If
    End Select
    If Not IsNothing(TabRwert) AndAlso TabRwert.Count > 0 Then
      For i = 0 To TabRwert.Count - 1
        If Not IsNothing(TabRwert(i)) Then
          TabRwert(i).Clear()
        End If
      Next
    End If
  End Sub


  '
  Private Sub chkARCN_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkARCN.CheckedChanged
    If chkARCN.Checked Then
      chkANEU.Visible = True
    Else
      chkANEU.Visible = False
    End If
  End Sub
  '
  '

  Private Sub chkGRPN_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkGRPN.CheckedChanged
    If chkGRPN.Checked Then
      cboGRPN.Visible = True
    Else
      cboGRPN.Visible = False
    End If
  End Sub
  Private Sub chkDAT_CheckedChanged(sender As Object, e As System.EventArgs) Handles _
    chkDAT_0.CheckedChanged, chkDAT_1.CheckedChanged, chkDAT_2.CheckedChanged, chkDAT_4.CheckedChanged
    Dim index As Integer
    index = CInt(sender.name.substring(7, 1))
    If chkDAT(index).Checked Then
      lblVON(index).Visible = True
      lblBIS(index).Visible = True
      txtVON(index).Visible = True
      txtbis(index).Visible = True
    Else
      lblVON(index).Visible = False
      lblBIS(index).Visible = False
      txtVON(index).Visible = False
      txtbis(index).Visible = False

    End If


  End Sub
  WriteOnly Property IopenArt() As Boolean
    Set(ByVal value As Boolean)
      MnIopenArt = value
    End Set
  End Property
  WriteOnly Property UserID As Integer
    Set(ByVal value As Integer)
      MnUserID = value
    End Set
  End Property
  WriteOnly Property MischID As Integer
    Set(ByVal value As Integer)
      MnMischID = value
    End Set
  End Property
  WriteOnly Property MethID As Integer
    Set(ByVal value As Integer)
      MnMethID = value
    End Set
  End Property
  WriteOnly Property MessgID As Integer
    Set(ByVal value As Integer)
      MnMessgID = value
    End Set
  End Property



  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************
  '**************************************************















  Private Sub Form_Unload(Cancel As Integer)
    GrpRwerte = Nothing
    ReWrRwert = Nothing
    RefWert = Nothing
    RezSozpt = Nothing
    ReWrRezept = Nothing
    HandleRezept = Nothing
    '
    Exit Sub
DbRucErr:
    MsgBox(Texxt(3610))

  End Sub





  Private Sub sstDATB_Click(PreviousTab As Integer)

  End Sub

  '**************************************************








  Public Sub New()

    ' Dieser Aufruf ist für den Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
    MnIopenArt = False
    MnUserID = -1
    MnMischID = -1
    MnMessgID = -1
    MnMethID = -1

  End Sub

  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtDIK.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub

  Private Sub txtDat_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtBIS_0.Validating, txtVON_0.Validating, txtBIS_1.Validating, txtVON_1.Validating, txtBIS_2.Validating, txtVON_2.Validating, txtBIS_4.Validating, txtVON_4.Validating
    If Not IsDate(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(2500))
    End If
  End Sub
  Private Sub btnEnd_Click(sender As System.Object, e As System.EventArgs) Handles btnEnd.Click
    Me.Close()
  End Sub
  
End Class