Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmDARZ
  Dim MnIopenArt As Boolean
  Dim MnUserID As Integer
  Dim MnMischID As Integer
  Dim MnMessgID As Integer
  Dim MnMethID As Integer
  Dim OldMischID As Integer
  Dim OldMessgID As Integer
  Dim imsg As Integer
  Dim RcmdRef As Integer
  Dim ier As Integer
  Dim Datn As String
  Dim i As Integer
  Dim j As Integer
  Dim k As Integer
  Dim kw As Integer
  Dim nkw As Integer
  Dim Kanz As Integer
  Dim DbName As String
  Dim RzNr As String
  Dim AdaptMisch As OleDbDataAdapter
  Dim CmdMisch As OleDbCommand
  Dim TabMisch As DataTable
  Dim Adaptuser As OleDbDataAdapter
  Dim Cmduser As OleDbCommand
  Dim TabUser As DataTable
  Dim AdaptMess As OleDbDataAdapter
  Dim CmdMess As OleDbCommand
  Dim TabMess As DataTable
  Dim Winkel As AngGeos
  Dim TabGroup As DataTable
  Dim AdaptRezept As OleDbDataAdapter
  Dim CmdRezept As OleDbCommand
  Dim TabRezept As List(Of DataTable)
  Dim AdaptRezeptRwert As OleDbDataAdapter
  Dim CmdRezeptRwert As OleDbCommand
  Dim TabRezeptRwert As DataTable
  Dim AdaptSortiRwert As OleDbDataAdapter
  Dim CmdSortiRwert As OleDbCommand
  Dim TabSortiRwert As DataTable

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
  Dim ForeignTabNam() As String
  Dim ForTabCount As Integer
  Dim Garray() As Object
  Dim RowWrt As Object
  Dim MaxClipBoard As Long
  Dim FilNaa As String
 

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
  Dim dbgREZ As List(Of DataGridView)
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
  Dim chkTYP As List(Of CheckBox)
  Dim chkSMP As List(Of CheckBox)
  Dim chkUUU As List(Of CheckBox)
  Dim chkUNTW As List(Of CheckBox)
  Dim chkUNTS As List(Of CheckBox)
  Dim lblUNTW As List(Of Label)
  Dim lblUNTS As List(Of Label)
  Dim chkDickW As List(Of CheckBox)
  Dim chkDickS As List(Of CheckBox)
  Dim txtDickW As List(Of TextBox)
  Dim txtDickS As List(Of TextBox)

  Dim btnUNTW As List(Of Button)
  Dim btnUNTS As List(Of Button)

  '
  '
  '
  Dim GrpRwerte As RefValuesGrp
  Dim ReWrRwert As ReadWriteRwert
  Dim RezSozpt As RecipesGrp
  Dim ReWrRezept As ReadWriteRezept
  Dim ReWrFarbm As ReadWriteFarbe
  Dim HandleRezept As List(Of HandleRezGeneral)
  Dim HandleRezeptN As HandleRezGeneral
  Dim VKwb(1) As Integer
  Dim RefWert As RefValue
  Dim RezID As Integer
  Dim RefWerteUNT As List(Of RefValue)
  Dim RefWerteTYP As List(Of RefValue)
  Dim RefWerteSMP As List(Of RefValue)

  Dim HandleRwrt As HandleRwerte
  Dim Ende As Boolean

  Private Sub frmDARZ_FormClosing(sender As Object, e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
  End Sub

  Private Sub frmDARZ_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim j As Integer
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    '
    Winkel = MenueParam.User.Winkel

    '
    '
    '
    cbogrp = New List(Of ComboBox)
    cbogrp.Add(cboGRP_0)
    cbogrp.Add(cboGRP_1)
    cbogrp.Add(cboGRP_2)
    cbogrp.Add(cboGRP_3)
    '
    '
    '
    lblGRP = New List(Of Label)
    lblGRP.Add(lblGRP_0)
    lblGRP.Add(lblGRP_1)
    lblGRP.Add(lblGRP_2)
    lblGRP.Add(lblGRP_3)
    '
    '
    '
    chkARC = New List(Of CheckBox)
    chkARC.Add(Nothing)
    chkARC.Add(chkARC_1)
    chkARC.Add(chkARC_2)
    chkARC.Add(chkARC_3)
    '
    '
    '
    '
    btnMARK = New List(Of Button)
    btnMARK.Add(btnMARK_0)
    btnMARK.Add(btnMARK_1)
    btnMARK.Add(btnMARK_2)
    btnMARK.Add(Nothing)
    '
    '
    '

    btnZEIG = New List(Of Button)
    btnZEIG.Add(btnZEIG_0)
    btnZEIG.Add(btnZEIG_1)
    btnZEIG.Add(btnZEIG_2)
    btnZEIG.Add(Nothing)
    '
    '
    '
    btnRUN = New List(Of Button)
    btnRUN.Add(btnRUN_0)
    btnRUN.Add(btnRUN_1)
    btnRUN.Add(btnRUN_2)
    btnRUN.Add(btnRUN_3)
    '
    '
    '
    btnCLIP = New List(Of Button)
    btnCLIP.Add(Nothing)
    btnCLIP.Add(Nothing)
    btnCLIP.Add(btnCLIP_2)
    btnCLIP.Add(btnCLIP_3)


    '

    '
    '
    dbgREZ = New List(Of DataGridView)
    dbgREZ.Add(dbgREZ_0)
    dbgREZ.Add(dbgREZ_1)
    dbgREZ.Add(dbgREZ_2)
    dbgREZ.Add(Nothing)
    '
    TabRezept = New List(Of DataTable)
    TabRezept.Add(New DataTable)
    TabRezept.Add(New DataTable)
    TabRezept.Add(New DataTable)
    TabRezept.Add(Nothing)
    TabRezept.Add(New DataTable)

    '
    '
    grdWRT = New List(Of DataGridView)
    grdWRT.Add(Nothing)
    grdWRT.Add(Nothing)
    grdWRT.Add(grdWRT_2)
    grdWRT.Add(grdWRT_3)
    '
    '
    '
    '
    lblANZ = New List(Of Label)
    lblANZ.Add(lblANZ_0)
    lblANZ.Add(lblANZ_1)
    lblANZ.Add(lblANZ_2)
    lblANZ.Add(Nothing)
    '
    '
    '
    '
    txtANZ = New List(Of TextBox)
    txtANZ.Add(txtANZ_0)
    txtANZ.Add(txtANZ_1)
    txtANZ.Add(txtANZ_2)
    txtANZ.Add(Nothing)
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
    '
    '
    '
    '
    lblVON = New List(Of Label)
    lblVON.Add(lblVON_0)
    lblVON.Add(lblVON_1)
    lblVON.Add(lblVON_2)
    lblVON.Add(Nothing)
    '
    '
    '
    '
    txtVON = New List(Of TextBox)
    txtVON.Add(txtVON_0)
    txtVON.Add(txtVON_1)
    txtVON.Add(txtVON_2)
    txtVON.Add(Nothing)
    '
    '
    '
    lblBIS = New List(Of Label)
    lblBIS.Add(lblBIS_0)
    lblBIS.Add(lblBIS_1)
    lblBIS.Add(lblBIS_2)
    lblBIS.Add(Nothing)
    '
    '
    '
    '
    txtbis = New List(Of TextBox)
    txtbis.Add(txtBIS_0)
    txtbis.Add(txtBIS_1)
    txtbis.Add(txtBIS_2)
    txtbis.Add(Nothing)
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
    '
    '
    '
    '
    txtSQL = New List(Of TextBox)
    txtSQL.Add(txtSQL_0)
    txtSQL.Add(txtSQL_1)
    txtSQL.Add(txtSQL_2)
    txtSQL.Add(Nothing)
    '
    '
    '
   
    If MnIopenArt Then
      '
      OldMischID = -1
      OldMessgID = -1
      '
      'Sätze können gelöscht werden
      '
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
      cboMISCH.Enabled = True
      TabDARZ.SelectedIndex = 0
    Else
      cboMESS.Enabled = False
      cboUSER.Enabled = False
      cboMISCH.Enabled = False
      TabDARZ.TabPages(0).Enabled = False
      TabDARZ.SelectedIndex = 1
      '
      OldMischID = MenueParam.MischID
      OldMessgID = MenueParam.MessgID
    End If


    '
    '
    '
   
    '
    RefWerteUNT = New List(Of RefValue)
    RefWerteTYP = New List(Of RefValue)
    RefWerteSMP = New List(Of RefValue)

    '
    '
    Me.Text = Texxt(1909) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
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
    '
    '
    btnRUN(0).Text = Texxt(3691)
    btnRUN(1).Text = Texxt(3692)
    btnRUN(2).Text = Texxt(3682)
    btnRUN(3).Text = Texxt(3683)
    btnEnd.Text = Texxt(490)
    '
    '
    btnCLIP(2).Text = Texxt(3662)
    btnCLIP(3).Text = Texxt(3663)
    '
    '
    chkTYP = New List(Of CheckBox)
    chkTYP.Add(chkTYP_0)
    chkTYP.Add(Nothing)
    chkTYP.Add(chkTYP_2)
    '
    '
    chkSMP = New List(Of CheckBox)
    chkSMP.Add(chkSMP_0)
    chkSMP.Add(Nothing)
    chkSMP.Add(chkSMP_2)
    '
    '
    '
    '
    chkUUU = New List(Of CheckBox)
    chkUUU.Add(chkUUU_0)
    chkUUU.Add(Nothing)
    chkUUU.Add(chkUUU_2)
    '
    '

    chkUNTW = New List(Of CheckBox)
    chkUNTW.Add(Nothing)
    chkUNTW.Add(chkUntW_1)
    chkUNTW.Add(Nothing)
    chkUNTW.Add(chkUntW_3)
    '
    chkUNTS = New List(Of CheckBox)
    chkUNTS.Add(Nothing)
    chkUNTS.Add(chkUntS_1)
    chkUNTS.Add(Nothing)
    chkUNTS.Add(chkuntS_3)
    '
    lblUNTW = New List(Of Label)
    lblUNTW.Add(Nothing)
    lblUNTW.Add(lblUntW_1)
    lblUNTW.Add(Nothing)
    lblUNTW.Add(lblUntW_3)
    '
    lblUNTS = New List(Of Label)
    lblUNTS.Add(Nothing)
    lblUNTS.Add(lblUntS_1)
    lblUNTS.Add(Nothing)
    lblUNTS.Add(lblUNTS_3)
    '
    '
    '

    '
    btnUNTW = New List(Of Button)
    btnUNTW.Add(Nothing)
    btnUNTW.Add(btnUntW_1)
    btnUNTW.Add(Nothing)
    btnUNTW.Add(btnUntW_3)
    '
    '
    '
    '
    btnUNTS = New List(Of Button)
    btnUNTS.Add(Nothing)
    btnUNTS.Add(btnUntS_1)
    btnUNTS.Add(Nothing)
    btnUNTS.Add(btnUntS_3)
    '
    '
    '
    chkDickW = New List(Of CheckBox)
    chkDickW.Add(Nothing)
    chkDickW.Add(chkDickW_1)
    chkDickW.Add(Nothing)
    chkDickW.Add(chkDickW_3)
    '
    chkDickS = New List(Of CheckBox)
    chkDickS.Add(Nothing)
    chkDickS.Add(chkDickS_1)
    chkDickS.Add(Nothing)
    chkDickS.Add(chkDickS_3)
    '
    '
    txtDickW = New List(Of TextBox)
    txtDickW.Add(Nothing)
    txtDickW.Add(txtDickW_1)
    txtDickW.Add(Nothing)
    txtDickW.Add(txtDickW_3)
    '
    txtDickS = New List(Of TextBox)
    txtDickS.Add(Nothing)
    txtDickS.Add(txtDickS_1)
    txtDickS.Add(Nothing)
    txtDickS.Add(txtDickS_3)
    '
    '
    For i = 0 To chkTYP.Count - 1
      If Not IsNothing(chkTYP(i)) Then
        chkTYP(i).Text = Texxt(3656)
      End If
    Next
    '
    '
    '
    '
    For i = 0 To chkSMP.Count - 1
      If Not IsNothing(chkSMP(i)) Then
        chkSMP(i).Text = Texxt(3657)
      End If
    Next
    '
    '
    '
    '
    For i = 0 To chkUUU.Count - 1
      If Not IsNothing(chkUUU(i)) Then
        chkUUU(i).Text = Texxt(3658)
      End If
    Next
    '
    For i = 0 To btnUNTW.Count - 1
      If Not IsNothing(btnUNTW(i)) Then
        btnUNTW(i).Text = Texxt(808)
      End If
    Next
    '
    '
    For i = 0 To btnUNTS.Count - 1
      If Not IsNothing(btnUNTS(i)) Then
        btnUNTS(i).Text = Texxt(809)
      End If
    Next '
    '
    For i = 0 To chkDickW.Count - 1
      If Not IsNothing(chkDickW(i)) Then
        chkDickW(i).Text = Texxt(810) & "(" & Texxt(878) & ")"
      End If
    Next
    '
    '
    For i = 0 To chkDickS.Count - 1
      If Not IsNothing(chkDickS(i)) Then
        chkDickS(i).Text = Texxt(811) & "(" & Texxt(878) & ")"
      End If
    Next
    '
    '
    '
    '
    '
    For i = 0 To txtDickW.Count - 1
      If Not IsNothing(txtDickW(i)) Then
        txtDickW(i).Text = MenueParam.Misch.Dicke
      End If
    Next
    '
    '
    For i = 0 To txtDickS.Count - 1
      If Not IsNothing(txtDickS(i)) Then
        txtDickS(i).Text = MenueParam.Misch.Dicke
      End If
    Next
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

    lblKENN.Text = Texxt(281)

    '
    chkDUP.Text = Texxt(3652)
    chkARCN.Text = Texxt(3660)
    chkANEU.Text = Texxt(374)
    lblMISCH.Text = Texxt(406)
    chkUNT.Text = Texxt(3655)
    chkUntergrName.Text = Texxt(3673)
    chkFarbmName.Text = Texxt(3626) '
    lblMESS.Text = Texxt(204)
    lblUSE.Text = Texxt(201)
    lblGRR.Text = Texxt(386)
    lblREF.Text = Texxt(3010)
    lblMSH.Text = Texxt(837)
    lblGRPN.Text = Texxt(3661)
    '
    '
    TabDARZ.TabPages(0).Text = Texxt(3680)
    TabDARZ.TabPages(1).Text = Texxt(3681)
    TabDARZ.TabPages(2).Text = Texxt(3682)
    TabDARZ.TabPages(3).Text = Texxt(3683)
    '
    '
    '
    '



    GrpRwerte = New RefValuesGrp
    ReWrRwert = New ReadWriteRwert
    RefWert = New RefValue
    '
    RezSozpt = New RecipesGrp
    ReWrRezept = New ReadWriteRezept
    ReWrFarbm = New ReadWriteFarbe
    HandleRezept = New List(Of HandleRezGeneral)
    For i = 0 To 3
      HandleRezept.Add(New HandleRezGeneral)
      HandleRezept(i).cboGRP = cbogrp(i)
      HandleRezept(i).lblGRP = lblGRP(i)
    Next i
    HandleRezeptN = New HandleRezGeneral
    HandleRezeptN.cboGRP = cboGRPN
    HandleRezeptN.lblGRP = lblGRPN
    HandleRwrt = New HandleRwerte
    TabGroup = New DataTable
    '
    '
    '
    '
    '
    TabMisch = New DataTable
    AdaptMisch = New OleDbDataAdapter
    CmdMisch = New OleDbCommand("", Cncol)
    AdaptMisch.SelectCommand = CmdMisch
    TabUser = New DataTable
    Adaptuser = New OleDbDataAdapter
    Cmduser = New OleDbCommand("", Cncol)
    Adaptuser.SelectCommand = Cmduser
    TabMess = New DataTable
    AdaptMess = New OleDbDataAdapter
    CmdMess = New OleDbCommand("", Cncol)
    AdaptMess.SelectCommand = CmdMess
    AdaptRezept = New OleDbDataAdapter
    CmdRezept = New OleDbCommand("", Cndat)
    AdaptRezept.SelectCommand = CmdRezept
    AdaptRezeptRwert = New OleDbDataAdapter
    CmdRezeptRwert = New OleDbCommand("", Cndat)
    AdaptRezeptRwert.SelectCommand = CmdRezeptRwert
    TabRezeptRwert = New DataTable
    AdaptSortiRwert = New OleDbDataAdapter
    CmdSortiRwert = New OleDbCommand("", Cndat)
    AdaptSortiRwert.SelectCommand = CmdSortiRwert
    TabSortiRwert = New DataTable
    '


    For i = 0 To dbgREZ.Count - 1
      If Not IsNothing(dbgREZ(i)) Then
        dbgREZ(i).DataSource = TabRezept(i)
        dbgREZ(i).AlternatingRowsDefaultCellStyle.BackColor = Color.Azure
        dbgREZ(i).DefaultCellStyle.BackColor = Color.Beige
        dbgREZ(i).AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
        dbgREZ(i).AllowUserToAddRows = False
        dbgREZ(i).AllowUserToDeleteRows = False
        dbgREZ(i).ReadOnly = True
        dbgREZ(i).Columns.Add("REZEPT_ID", "ID")
        dbgREZ(i).Columns(0).Visible = False
        dbgREZ(i).Columns.Add("REZEPT_DATTIM", Texxt(2205))
        dbgREZ(i).Columns.Add("REZEPT_NAME", Texxt(931))
        dbgREZ(i).Columns(1).Width = 80
        dbgREZ(i).Columns(0).DataPropertyName = dbgREZ(i).Columns(0).Name
        dbgREZ(i).Columns(1).DataPropertyName = dbgREZ(i).Columns(1).Name
        dbgREZ(i).Columns(2).DataPropertyName = dbgREZ(i).Columns(2).Name
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
    '

    '
    RzNr = "MNG"
    RezSozpt.Rezepte.AddRez(RzNr, New Recipe)
    '
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
    cboMISCH.Enabled = False
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
      cboMISCH.Enabled = False
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
    For i = 0 To 1
      GrpRwerte.Add(KeyRe(i), New RefValues)
      GrpRwerte(KeyRe(i)).RefUnt = New RefValue
    Next i
    '
    If Not IsNothing(GetPutReflex) Then
      panUNT_1.Visible = True
      panUNT_3.Visible = True
      AddHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
    End If
    '


  End Sub

  Private Sub frmDARZ_Paint(sender As Object, e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint
    TabDARZ.ItemSize = New Size((TabDARZ.Width - TabDARZ.TabPages.Count) / TabDARZ.TabPages.Count - 3, TabDARZ.ItemSize.Height)
  End Sub
  Private Sub cboUSER_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboUSER.SelectedValueChanged
    If Not IsNumeric(cboUSER.SelectedValue) Then Exit Sub
    If Not cboUSER.Enabled Then Exit Sub
    If MenueParam.UserID <> cboUSER.SelectedValue Then
      AufbauPar.UserID = cboUSER.SelectedValue
    End If
    'Mischsystem
    '
    TabMisch.Rows.Clear()


    SqlStmt = "SELECT *,TBL_MISCH.MISCH_ID AS MISCH_ID FROM TBL_MISCH INNER JOIN TBL_USER_MISCH ON TBL_MISCH.MISCH_ID=TBL_USER_MISCH.MISCH_ID WHERE USER_ID=" & cboUSER.SelectedValue & " ORDER BY MISCH_KBEZ"
    CmdMisch.CommandText = SqlStmt
    If Not FillDatset(AdaptMisch, TabMisch) Then
      Exit Sub
    End If

    cboMISCH.Enabled = False
    cboMISCH.DataSource = TabMisch
    cboMISCH.DisplayMember = "MISCH_KBEZ"
    cboMISCH.ValueMember = "MISCH_ID"
    cboMISCH.Enabled = True
    If TabMisch.Rows.Count > 0 Then
      If OldMischID = -1 Then
        cboMISCH.SelectedValue = TabMisch.Rows(0)("MISCH_ID")
      Else
        cboMISCH.SelectedValue = OldMischID
      End If
    Else
      cboMISCH.SelectedIndex = -1
    End If

    '
    '

  End Sub

  '
  '
  '
  '
  '
  Private Sub cboMISCH_Changed(sender As Object, e As System.EventArgs) Handles cboMISCH.EnabledChanged, cboMISCH.SelectedValueChanged

    Dim i As Integer
    If Not cboMISCH.Enabled Then Exit Sub
    If IsNothing(cboMISCH.SelectedValue) OrElse Not IsNumeric(cboMISCH.SelectedValue) Then Exit Sub
    If cboMISCH.SelectedIndex = -1 Then Exit Sub
    '
    '
    '
    'Clear Tables
    '
    '
    TabWerte(2).Clear()
    TabWerte(3).Clear()
    For i = 0 To TabRezept.Count - 1
      If Not IsNothing(TabRezept(i)) Then
        TabRezept(i).Clear()
      End If
    Next
    '
    '
    AufbauPar.MischID = -1
    AufbauPar.MischID = cboMISCH.SelectedValue
    AufbauPar.UserID = cboUSER.SelectedValue
    For i = 0 To HandleRezept.Count - 1
      HandleRezept(i).GroupList()
      '
      If BitWrt(25, MenueParam.User.Enabl) Then
        If Not IsNothing(chkARC(i)) Then
          chkARC(i).Enabled = True
        End If
      Else
        If Not IsNothing(chkARC(i)) Then
          chkARC(i).Enabled = False
        End If
      End If
      If BitWrt(25, MenueParam.User.Visbl) Then
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
    HandleRezeptN.GroupList()
    HandleRezeptN.lblGRP.Visible = True
    HandleRezeptN.cboGRP.Visible = False
    '
    'Combobox mit Gruppen-Id's für R-Werte

    '
    '
    'Messgerät
    '
    cboMESS.Enabled = False
    TabMess.Clear()
    SqlStmt = "SELECT TBL_MESSG.MESSG_ID AS MESSG_ID,MESSG_KBEZ FROM TBL_MESSG INNER JOIN TBL_MISCH_MESSG ON TBL_MESSG.MESSG_ID=TBL_MISCH_MESSG.MESSG_ID WHERE MISCH_ID=" & cboMISCH.SelectedValue & " ORDER BY MESSG_KBEZ"
    CmdMess.CommandText = SqlStmt
    If Not FillDatset(AdaptMess, TabMess) Then
      Exit Sub
    End If
    cboMESS.DataSource = TabMess
    cboMESS.DisplayMember = "MESSG_KBEZ"
    cboMESS.ValueMember = "MESSG_ID"

    If TabMess.Rows.Count > 0 And MnIopenArt Then
      cboMESS.SelectedValue = TabMess.Rows(0)("MESSG_ID")
    Else
      cboMESS.SelectedValue = MenueParam.MessgID
    End If
    If MnIopenArt Then
      cboMESS.Enabled = True
    End If
    If MenueParam.UserID > -1 Then
      cboMESS.Enabled = False
    End If
    If IsNumeric(cboMESS.SelectedValue) Then
      AufbauPar.MessgID = cboMESS.SelectedValue
    End If
    '
    VKwb(0) = 0
    VKwb(1) = 1
    If MenueParam.Misch.Vert = 1 Then
      VKwb(0) = 1
      VKwb(1) = 0
    End If
    For i = 0 To 1
      RezSozpt.Rezepte.kwb(i) = VKwb(i)
    Next i
    '
    '
    If MnIopenArt Then
      cboMESS.Enabled = True
    Else
      cboMESS.Enabled = True
      cboMESS.Enabled = False
    End If
  End Sub
  '


  Private Sub cboGRP_Click(sender As Object, e As System.EventArgs) Handles _
   cboGRP_0.Click, cboGRP_1.Click, cboGRP_2.Click, cboGRP_3.Click
    Dim index As Integer
    index = CInt(sender.name.substring(7, 1))


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
        Call HandleRwrt.MakeTABRefwerte(True, MenueParam.Messg.Kbez, Winkel, TabWerte(index))

        Call ClipboardToTable(True, TabWerte(index), ";", ier)
        If ier <> 0 Then
          MsgBox("Error: " & CStr(ier))
          Cursor = Cursors.Default
          Exit Sub
        End If
        Call HandleRwrt.MakeGRIDRefwerte(Winkel, grdWRT(index))
        grdWRT(index).Visible = True
        btnRUN(index).Enabled = True
    End Select


  End Sub
  '
  '
  '
  '
  '
  Private Sub btnMARK_Click(sender As Object, e As System.EventArgs) Handles _
   btnMARK_0.Click, btnMARK_1.Click, btnMARK_2.Click
    Dim index As Integer
    index = CInt(sender.name.substring(8, 1))
    btnMARK(index).Enabled = False
    btnRUN(index).Enabled = False
    dbgREZ(index).SelectAll()
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
   btnRUN_0.Click, btnRUN_1.Click, btnRUN_2.Click, btnRUN_3.Click
    Dim index As Integer
    Dim i As Integer
    Dim j As Integer
    Dim kw As Integer
    Dim RezID As Integer
    Dim RefID As Integer
    Dim FaID As Integer
    Dim StrRezept As String
    Dim TypID(1) As Integer
    Dim SmpID(1) As Integer
    Dim UntID(1) As Integer
    Dim StrUnt As String
    Dim StrTyp As String
    Dim StrSmp As String
    Dim HlfTxt As String
    Dim NF As Integer
    Dim MngAkt As Single = 100.0
    Dim ArbFarbe As New Colorant
    Dim InChar() As String
    Dim kwb As Integer

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
        If dbgREZ(index).Rows.Count = 0 Then
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
        If Not HandleRezept(index).MeldSpeiAllRzp(True) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        btnZEIG(index).Enabled = False

        '


        'Kloe = 0

        TabDARZ.TabPages(index).Enabled = False
        StrRezept = ""
        For i = 0 To dbgREZ(index).SelectedRows.Count - 1
          RezID = dbgREZ(index).SelectedRows(i).Cells("REZEPT_ID").Value
          StrRezept = StrRezept & CStr(RezID) & ","
        Next i '
        StrRezept = "(" & StrRezept & "-999)"

        If chkUNT.Checked Then
          '
          '
          '
          'Nur Verweise auf Untergründe werden für alle Messgeräte gelöscht
          '
          '
          '
          '
          SqlStmt = "DELETE * FROM TBL_REZEPT_RWERT" _
          & " WHERE MISCH_ID=" & cboMISCH.SelectedValue & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND [REZEPT_ID] IN " & StrRezept & " AND RWERT_KWB<0"
          CmdRezept.CommandText = SqlStmt
          If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
            Exit Sub
          End If
        Else
          '
          '
          'Rezepte und ggf. R-Werte werden gelöscht
          '
          '
          'zu löschende R-wert für Untergründe
          '
          Call DelRezRwert({-1, -2}, StrRezept, StrUnt)
          '
          'zu löschende R-wert für Vorlagen
          '
          Call DelRezRwert({11, 12}, StrRezept, StrTyp)
          '
          'zu löschende R-wert für Nachstellungen
          '
          Call DelRezRwert({1, 2}, StrRezept, StrSmp)
          '
          'Einträge in TBL_REZEPT_RWERT löschen
          '
          'Alle ausgewählten Rezepte werden gelöscht
          '
          SqlStmt = "DELETE * FROM TBL_REZEPT_RWERT WHERE MISCH_ID=" & cboMISCH.SelectedValue & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND REZEPT_ID IN " & StrRezept
          CmdRezept.CommandText = SqlStmt
          If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
            Exit Sub
          End If
          '
          '
          '
          If chkTYP(0).Checked And imsg <> 7 Then
            '
            '
            'Lösche Reflexionswerte für Vorlagen in TBL_QUALI
            '
            '
            '
            '
            '
            SqlStmt = "DELETE * FROM TBL_QUALI WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND QUALI_ID IN " & StrTyp
            CmdRezept.CommandText = SqlStmt
            If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
              Exit Sub
            End If            '
            'Lösche Reflexionswerte für Vorlagen in TBL_RWERT
            '
            '
            '
            '
            SqlStmt = "DELETE * FROM TBL_RWERT WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID IN " & StrTyp
            CmdRezept.CommandText = SqlStmt
            If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
              Exit Sub
            End If      '
          End If
          If chkSMP(0).Checked And imsg <> 7 Then
            '
            'Lösche Reflexionswerte für Nachstellung in TBL_QUALI
            '
            '
            '
            '
            '
            SqlStmt = "DELETE * FROM TBL_QUALI WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND QUALI_ID IN " & StrSmp
            CmdRezept.CommandText = SqlStmt
            If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
              Exit Sub
            End If                     ''
            'Lösche Reflexionswerte für Nachstellungen
            '
            '
            '
            '
            SqlStmt = "DELETE * FROM TBL_RWERT WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID IN " & StrSmp
            CmdRezept.CommandText = SqlStmt
            If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
              Exit Sub
            End If
          End If
          If chkUUU(0).Checked And imsg <> 7 Then
            '
            'Lösche Reflexionswerte für Untergrund in TBL_QUALI
            '
            '
            '
            '
            '
            '
            SqlStmt = "DELETE * FROM TBL_QUALI WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND QUALI_ID IN " & StrUnt
            CmdRezept.CommandText = SqlStmt
            If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
              Exit Sub
            End If                     ' '
            'Lösche Reflexionswerte für Untergründe
            '
            '
            SqlStmt = "DELETE * FROM TBL_RWERT WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID IN " & StrUnt
            CmdRezept.CommandText = SqlStmt
            If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
              Exit Sub
            End If
          End If
          '
          '
          '
          '
          '
          'TBL_REZEPT_FARBM löschen
          '
          '

          SqlStmt = "DELETE * FROM TBL_REZEPT_FARBM" _
          & " WHERE TBL_REZEPT_FARBM.MISCH_ID=" & MenueParam.MischID & " AND [REZEPT_ID] IN " & StrRezept

          CmdRezept.CommandText = SqlStmt
          If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
            Exit Sub
          End If '
          '
          'TBL_REZEPT löschen
          '
          '
          SqlStmt = "DELETE * FROM TBL_REZEPT" _
          & " WHERE MISCH_ID=" & MenueParam.MischID & " AND  [REZEPT_ID] IN " & StrRezept
          CmdRezept.CommandText = SqlStmt
          If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
            Exit Sub
          End If '
        End If
        TabDARZ.TabPages(index).Enabled = True
        btnZEIG(index).Enabled = True
        Cursor = Cursors.Default


      Case 1
        '
        '
        'Korrigieren
        '
        Strgid = ""
        If chkARCN.Checked Then
          Strgid = Strgid & " REZEPT_IARCH=" & chkANEU.CheckState
        End If
        If chkGRPN.Checked Then
          If Strgid <> "" Then
            Strgid = Strgid & ","
          End If
          Strgid = Strgid & " REZEPT_GID=" & cboGRPN.SelectedValue
        End If
        '
        '
        '
        '
        If chkDickW(index).Checked Then
          If Strgid <> "" Then
            Strgid = Strgid & ","
          End If
          Strgid = Strgid & " REZEPT_DIK1=" & SQLpunkt(CStr(txtDickW(index).Text))
        End If
        If chkDickS(index).Checked Then
          If Strgid <> "" Then
            Strgid = Strgid & ","
          End If
          Strgid = Strgid & " REZEPT_DIK2=" & SQLpunkt(CStr(txtDickS(index).Text))
        End If
        '
        '
        If dbgREZ(index).Rows.Count = 0 Then Exit Sub
        If Strgid = "" And chkUNTW(index).CheckState = 0 And chkUNTS(index).CheckState = 0 Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        imsg = MsgBox(Texxt(2927), 4, Texxt(2000))
        If imsg = 7 Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If chkGRPN.Checked Then
          If Not HandleRezeptN.MeldSpeiAllRzp(False) Then
            Cursor = Cursors.Default
            Exit Sub
          End If
        End If
        btnMARK(index).Enabled = False
        '
        '
        'Ausgewählte Sätze korrigieren
        '
        '

        StrRezept = ""
        For i = 0 To dbgREZ(index).SelectedRows.Count - 1
          RezID = dbgREZ(index).SelectedRows(i).Cells("REZEPT_ID").Value
          StrRezept = StrRezept & CStr(RezID) & ","
        Next i '
        StrRezept = "(" & StrRezept & "-999)"
        '
        For j = 0 To 1
          UntID(j) = -1
          If GrpRwerte(KeyRe(j)).RefUnt.IVoNa And GrpRwerte(KeyRe(j)).RefUnt.ID > -1 Then
            UntID(j) = GrpRwerte(KeyRe(j)).RefUnt.ID
          End If
        Next j
        '
        '

        If Strgid <> "" Then
          SqlStmt = "UPDATE TBL_REZEPT" _
          & " SET " & Strgid & " WHERE MISCH_ID=" & MenueParam.MischID & " AND [REZEPT_ID] IN " & StrRezept
          CmdRezept.CommandText = SqlStmt
          If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
            Exit Sub
          End If
        End If
        If chkUNTW(index).Checked And GrpRwerte(KeyRe(0)).RefUnt.IVoNa Then
          '
          '
          'weißer Untergrund für ausgewählte Rezepte wird gelöscht
          '
          SqlStmt = "DELETE * FROM TBL_REZEPT_RWERT" _
          & " WHERE MISCH_ID=" & cboMISCH.SelectedValue & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID _
          & " AND [REZEPT_ID] IN " & StrRezept _
          & " AND RWERT_KWB=-1"
          CmdRezept.CommandText = SqlStmt
          If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
            Exit Sub
          End If
          '
          '
          'Weißer Untergrund für ausgewählte Rezepte wird hinzugefügt
          '
          '
          '
          For i = 0 To dbgREZ(index).SelectedRows.Count - 1
            RezID = dbgREZ(index).SelectedRows(i).Cells("REZEPT_ID").Value
            SqlStmt = "INSERT INTO TBL_REZEPT_RWERT" _
            & "(MISCH_ID,REZEPT_ID,MESSGRW_ID,RWERT_ID,RWERT_KWB) VALUES(" & cboMISCH.SelectedValue _
            & "," & RezID & "," & MenueParam.Messg.MessgRwID & "," & UntID(0) & "," & -1 & ")"
            CmdRezept.CommandText = SqlStmt
            If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
              Exit Sub
            End If
           
          Next i
        End If
        If chkUNTS(index).Checked And GrpRwerte(KeyRe(1)).RefUnt.IVoNa Then
          '
          '
          'schwarzer Untergrund für ausgewählte Rezepte wird gelöscht
          '
          SqlStmt = "DELETE * FROM TBL_REZEPT_RWERT" _
          & " WHERE MISCH_ID=" & cboMISCH.SelectedValue & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID _
          & " AND [REZEPT_ID] IN" & StrRezept _
          & " AND RWERT_KWB=-2"
          CmdRezept.CommandText = SqlStmt
          If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
            Exit Sub
          End If
          '
          '
          'schwarzer Untergrund für ausgewählte Rezepte wird hinzugefügt
          '
          '
          '
          For i = 0 To dbgREZ(index).SelectedRows.Count - 1
            RezID = dbgREZ(index).SelectedRows(i).Cells("REZEPT_ID").Value
            SqlStmt = "INSERT INTO TBL_REZEPT_RWERT" _
            & "(MISCH_ID,REZEPT_ID,MESSGRW_ID,RWERT_ID,RWERT_KWB) VALUES(" & cboMISCH.SelectedValue _
            & "," & RezID & "," & MenueParam.Messg.MessgRwID & "," & UntID(1) & "," & -2 & ")"
            CmdRezept.CommandText = SqlStmt
            If SQLExeNonQuery(CmdRezept, Cndat) <> 0 Then
              Exit Sub
            End If
          Next i
        End If



        Cursor = Cursors.Default
        dbgREZ(index).Columns(0).Visible = False







      Case 2
        '
        '
        '
        'Tabelle (Grid) aufbauen aus ACCESS für ausgewählte Sätze
        '
        '
        '
        '
        If dbgREZ(index).Rows.Count = 0 Then Exit Sub
        btnMARK(index).Enabled = False

        grdWRT(index).Visible = True
        dbgREZ(index).Visible = False
        '
        '
        Call HandleRwrt.MakeTABRefwerte(True, MenueParam.Messg.Kbez, Winkel, TabWerte(index))
        Call HandleRwrt.MakeGRIDRefwerte(Winkel, grdWRT(index))
        '
        '
        '
        'Ausgewählte Sätze in Grid
        '
        If ConnOpen(Cndat) Then
          For i = 0 To dbgREZ(index).SelectedRows.Count - 1
            ' For i = 0 To dbgREZ(index).SelBookmarks.Count - 1
            RezID = dbgREZ(index).SelectedRows(i).Cells("REZEPT_ID").Value

            ReWrRezept.ReadRezeptFarbGrund(RzNr, RezID, RezSozpt, UntID, TypID, SmpID, ier)
            RefWerteUNT.Clear()
            RefWerteTYP.Clear()
            RefWerteSMP.Clear()
            For j = 0 To 1
              If UntID(j) >= 0 And chkUUU(index).Checked Then
                RefWerteUNT.Add(New RefValue)
                ReWrRwert.ReadRwert(UntID(j), RefWerteUNT(j), ier)
              Else
                RefWerteUNT.Add(Nothing)
              End If
              If TypID(j) >= 0 And chkTYP(index).Checked Then
                RefWerteTYP.Add(New RefValue)
                ReWrRwert.ReadRwert(TypID(j), RefWerteTYP(j), ier)
              Else
                RefWerteTYP.Add(Nothing)
              End If
              If SmpID(j) >= 0 And chkSMP(index).Checked Then
                RefWerteSMP.Add(New RefValue)
                ReWrRwert.ReadRwert(SmpID(j), RefWerteSMP(j), ier)
              Else
                RefWerteSMP.Add(Nothing)
              End If
            Next j
            Call HandleRezept(index).TabRezRecord(Winkel, RzNr, RezSozpt, RefWerteUNT, RefWerteTYP, RefWerteSMP, TabWerte(index), ier)
          Next i
          Cndat.Close()
        End If
        '
        '

        btnCLIP(index).Enabled = True

        Cursor = Cursors.Default

        '
      Case 3
        '
        '

        '
        'Tabelle nach ACCESS übernehmen
        '
        '
        If Not HandleRezept(index).MeldSpeiAllRzp(False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        TabDARZ.TabPages(index).Enabled = False
        RefWert.Cme = MenueParam.Messg.Kenn
        RefWert.Iarch = chkARC(index).CheckState
        RefWert.Gid = cboGRR.SelectedValue
        RefWert.DatTim = Now
        RefWert.ReTr = 0
        RefWert.Banum = Space(1)
        RefWert.Iami = 1
        For j = 0 To Winkel.Km - 1
          RefWert.De(j) = 0
        Next j
        For j = 0 To 1
          UntID(j) = -1
          If GrpRwerte(KeyRe(j)).RefUnt.IVoNa Then
            UntID(j) = GrpRwerte(KeyRe(j)).RefUnt.ID
          End If
        Next j
        RefWert.RefKurv.clear()
        Call ADDCurves(RefWert.RefKurv)
      
        '
        '
        '
        '
        '
        '
        RezSozpt.Rezepte(RzNr).DatTim = Now
        RezSozpt.Rezepte(RzNr).Gid = cbogrp(index).SelectedValue
        RezSozpt.Rezepte(RzNr).Iarch = chkARC(index).CheckState
        RezSozpt.IVOL = 0
        RezSozpt.INF = 0
        RezSozpt.INM = 0
        RezSozpt.INO = 1
        RezSozpt.INQ = 0
        RezSozpt.ProzMin = 0
        RezSozpt.ProzMax = 0
        For i = 0 To Winkel.Wsol.Nwe - 1
          'grdWRT(index).col = i + 4
          If CSng(grdWRT(index).Columns(i + 5).HeaderText) <> Winkel.Wsol.R(i) Then
            MsgBox(Texxt(3054), 0, Texxt(2000))
            Exit Select
          End If
        Next i

        RezSozpt.Rezepte(RzNr).clear()
        RezSozpt.Farben.clear()
        RezSozpt.Rezepte(RzNr).Nr = -1
        NF = 0
        j = -1
        If ConnOpen(Cndat) Then
          Do
            If j = grdWRT(index).Rows.Count - 1 Then
              '
              'Letzte Zeile
              '
              HlfTxt = "0"
            Else
              j = j + 1
              HlfTxt = grdWRT(index).Rows(j).Cells(0).Value
            End If
            Select Case HlfTxt
              Case "0"
                If RezSozpt.Rezepte(RzNr).KF <> 0 And RezSozpt.Rezepte(RzNr).Nr >= 0 Then
                  '
                  '
                  '
                  'Prüfen, ob Farbmittelname übereinstimmt
                  '
                  '
                  '
                  For i = 0 To RezSozpt.Farben.FarbCount - 1
                    ArbFarbe.ID = -1
                    ArbFarbe.Name = ""
                    Call ReWrFarbm.FarRea(RezSozpt.Farben(i).ID, ArbFarbe, False, ier)
                    If ier <> 0 Then
                      MsgBox(Texxt(ier))
                      Cursor = Cursors.Default
                      Exit Sub
                    End If
                    If chkFarbmName.Checked And Trim(ArbFarbe.Name) <> RezSozpt.Farben(i).Name Then
                      MsgBox(Texxt(3053) & ": " & ArbFarbe.Name & Space(2) & RezSozpt.Farben(i).Name, 0, Texxt(2000))
                      Exit Do
                    End If
                  Next i
                  RezSozpt.MngMax = MngAkt
                  RezSozpt.MngMin = MngAkt
                  ReWrRezept.AddRezept(RzNr, RezID, RezSozpt, UntID, TypID, SmpID, ier)
                  If ier > 0 Then
                    Cndat.Close()
                    Exit Do
                  End If
                End If
                If j = grdWRT(index).Rows.Count - 1 Then
                  Exit Do
                End If
                RezSozpt.Rezepte(RzNr).clear()
                RezSozpt.Farben.clear()
                NF = 0
                RezSozpt.Rezepte(RzNr).Nr = -1
                TypID(0) = -1
                TypID(1) = -1
                SmpID(0) = -1
                SmpID(1) = -1
                '
                'Kopfzeile Rezept
                '
                '
                RezSozpt.Rezepte(RzNr).Name = grdWRT(index).Rows(j).Cells(1).Value
                RezSozpt.Rezepte(RzNr).Bem = grdWRT(index).Rows(j).Cells(2).Value
                RezSozpt.Rezepte(RzNr).DatTim = grdWRT(index).Rows(j).Cells(3).Value
                InChar = grdWRT(index).Rows(j).Cells(4).Value.split("|")
                RezSozpt.Rezepte(RzNr).Dicke(0) = 0.0
                RezSozpt.Rezepte(RzNr).Dicke(1) = 0.0
                If Not IsNothing(InChar) Then
                  If InChar.Length >= 1 Then
                    RezSozpt.Rezepte(RzNr).Dicke(0) = Singl(InChar(0))
                  End If
                  If InChar.Length >= 2 Then
                    RezSozpt.Rezepte(RzNr).Dicke(1) = Singl(InChar(1))
                  End If
                End If
                If chkDickW(index).Checked Then
                  If IsNumeric(txtDickW(index).Text) Then
                    RezSozpt.Rezepte(RzNr).Dicke(0) = CSng(txtDickW(index).Text)
                  End If
                End If
                If chkDickS(index).Checked Then
                  If IsNumeric(txtDickS(index).Text) Then
                    RezSozpt.Rezepte(RzNr).Dicke(1) = CSng(txtDickS(index).Text)
                  End If
                End If
                RezSozpt.Rezepte(RzNr).Nr = 1
              Case "1", "2", "11", "12", "-1", "-2"
                '
                '
                'Reflexionswerte
                '
                '
                '
                kwb = CInt(grdWRT(index).Rows(j).Cells(0).Value)
                'grdWRT(index).col = 1
                RefWert.Name = grdWRT(index).Rows(j).Cells(1).Value
                'grdWRT(index).col = 2
                RefWert.Bem = grdWRT(index).Rows(j).Cells(2).Value
                'grdWRT(index).col = 3
                RefWert.DatTim = grdWRT(index).Rows(j).Cells(3).Value
                RefWert.Cme = txtKENN.Text
                j = j - 1
                For kw = 0 To Winkel.Km - 1
                  j = j + 1
                  If grdWRT(index).Rows(j).Cells(4).Value <> Winkel(kw).Chrm Then
                    MsgBox(Texxt(3606) & Space(2) & grdWRT(index).Rows(j).Cells(4).Value & "  <<====>>  " & Winkel(kw).Chrm)
                    Cursor = Cursors.Default
                    Exit Sub
                  End If
                  If grdWRT(index).Rows(j).Cells.Count <> Winkel.Wsol.Nwe + 5 Then
                    MsgBox(Texxt(3054))
                    Cursor = Cursors.Default
                    Exit Sub
                  End If
                  For i = 0 To Winkel.Wsol.Nwe - 1
                    RefWert.RefKurv(kw).R(i) = 0.01 * Singl(grdWRT(index).Rows(j).Cells(i + 5).Value)
                  Next i
                Next kw

                Select Case kwb
                  Case 1
                    ReWrRwert.WriteRwert(RefID, RefWert, ier)
                    SmpID(0) = RefID
                  Case 2
                    ReWrRwert.WriteRwert(RefID, RefWert, ier)
                    SmpID(1) = RefID
                  Case 11
                    ReWrRwert.WriteRwert(RefID, RefWert, ier)
                    TypID(0) = RefID
                  Case 12
                    ReWrRwert.WriteRwert(RefID, RefWert, ier)
                    TypID(1) = RefID
                  Case -1
                    If Not chkUNTW(index).Checked Or Not GrpRwerte(KeyRe(0)).RefUnt.IVoNa Then
                      If Not GrpRwerte(KeyRe(0)).RefUnt.IVoNa Or GrpRwerte(KeyRe(0)).RefUnt.ID = -1 Or _
                      GrpRwerte(KeyRe(0)).RefUnt.Name <> RefWert.Name Or Not chkUntergrName.CheckState Then
                        ReWrRwert.WriteRwert(RefID, RefWert, ier)
                        UntID(0) = RefID
                        If chkUntergrName.Checked Then
                          GrpRwerte(KeyRe(0)).RefUnt.IVoNa = True
                          GrpRwerte(KeyRe(0)).RefUnt.ID = RefID
                          GrpRwerte(KeyRe(0)).RefUnt.Name = RefWert.Name
                        End If
                      End If
                    End If
                  Case -2
                    If Not chkUNTS(index).Checked Or Not GrpRwerte(KeyRe(1)).RefUnt.IVoNa Then
                      If Not GrpRwerte(KeyRe(1)).RefUnt.IVoNa Or GrpRwerte(KeyRe(1)).RefUnt.ID = -1 Or _
                      GrpRwerte(KeyRe(1)).RefUnt.Name <> RefWert.Name Or Not chkUntergrName.Checked Then
                        ReWrRwert.WriteRwert(RefID, RefWert, ier)
                        UntID(1) = RefID
                        If chkUntergrName.Checked Then
                          GrpRwerte(KeyRe(1)).RefUnt.IVoNa = True
                          GrpRwerte(KeyRe(1)).RefUnt.ID = RefID
                          GrpRwerte(KeyRe(1)).RefUnt.Name = RefWert.Name
                        End If
                      End If
                    End If
                End Select
                TabDARZ.TabPages(index).Enabled = True


              Case Else
                '
                '
                '
                'Farbmittel (ID,Namen,Mengen)
                '
                '
                '
                If grdWRT(index).Rows(j).Cells(0).Value.substring(0, 1) = "#" Then
                  NF = NF + 1
                  RezSozpt.Rezepte(RzNr).AddFaNr(KeyRe(NF), New ColorAmount)
                  FaID = CInt(grdWRT(index).Rows(j).Cells(0).Value.substring(1))
                  RezSozpt.Rezepte(RzNr)(KeyRe(NF)).ID = FaID
                  RezSozpt.Farben.AddFarb(KeyName(FaID), New Colorant)
                  RezSozpt.Farben(KeyName(FaID)).ID = FaID
                  RezSozpt.Farben(KeyName(FaID)).Name = grdWRT(index).Rows(j).Cells(1).Value
                  RezSozpt.Rezepte(RzNr)(KeyRe(NF)).FaAmng = CSng(grdWRT(index).Rows(j).Cells(2).Value)
                  RezSozpt.Rezepte(RzNr)(KeyRe(NF)).Proz = 100
                  RezSozpt.Rezepte(RzNr)(KeyRe(NF)).Prob = 100
                  RezSozpt.Farben(KeyName(FaID)).Preis = 0
                  RezSozpt.Farben(KeyName(FaID)).OP = Space(1)
                  RezSozpt.Farben(KeyName(FaID)).Kto = Space(1)
                  RezSozpt.Farben(KeyName(FaID)).BoMng = 100
                End If
                '
                '
            End Select
          Loop
          Cndat.Close()
        End If
        Cursor = Cursors.Default
    End Select
TransError:
    Cursor = Cursors.Default
  End Sub


  Private Sub btnZEIG_Click(sender As Object, e As System.EventArgs) Handles _
   btnZEIG_0.Click, btnZEIG_1.Click, btnZEIG_2.Click
    Dim index As Integer
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
        dbgREZ(index).Visible = True

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
          StrGid = "REZEPT_GID= " & cbogrp(index).SelectedValue & " AND " & StrGid
        End If
        StrGid = StrGid & " REZEPT_IARCH=2 OR "
        StrGid = StrGid & " REZEPT_IARCH=0)"
        StrGid = " WHERE MISCH_ID=" & MenueParam.MischID & " AND " & StrGid
        If chkDAT(index).Checked Then
          StrGid = StrGid & " AND (REZEPT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON(index).Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtbis(index).Text))) & ")"
        End If
        If Trim(txtSQL(index).Text) <> "" Then
          StrGid = StrGid & " AND " & StrSelct("REZEPT_NAME", txtSQL(index).Text)
        End If
        '

        SqlStmt = "SELECT REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME FROM TBL_REZEPT" & StrGid
        '
        '
        SqlStmt = SqlStmt & " ORDER BY REZEPT_NAME"

        'SqlStmt = SqlStmt & " ORDER BY RWERT_NAME"
        CmdRezept.CommandText = SqlStmt
        TabRezept(index).Clear()
        If Not FillDatset(AdaptRezept, TabRezept(index)) Then
          Exit Sub
        End If
        If dbgREZ(index).Rows.Count = 0 Then
          MsgBox(Texxt(2953))
        Else
          txtANZ(index).Visible = True
          lblANZ(index).Visible = True
          txtANZ(index).Text = CInt(TabRezept(index).Rows.Count)
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
        dbgREZ(index).Visible = True
        If index = 2 Then
          grdWRT(index).Visible = False
          btnCLIP(index).Enabled = False
        End If
        txtANZ(index).Visible = False
        lblANZ(index).Visible = False
        If chkDUP.Checked And index = 1 Then
          SqlStmt = "SELECT REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME FROM TBL_REZEPT" _
          & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_NAME IN (SELECT REZEPT_NAME FROM TBL_REZEPT" _
          & " AS TMP_NAME GROUP BY REZEPT_NAME HAVING COUNT(*)>1)"
        Else



          StrGid = ""
          If cbogrp(index).SelectedValue > 0 Then
            StrGid = " AND REZEPT_GID= " & cbogrp(index).SelectedValue
          End If
          If chkARC(index).CheckState < 2 Then
            StrGid = StrGid & " AND REZEPT_IARCH=" & chkARC(index).CheckState
          End If
          SqlStmt = "SELECT REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME FROM TBL_REZEPT" _
          & " WHERE MISCH_ID=" & MenueParam.MischID & StrGid
          If chkDAT(index).Checked Then
            SqlStmt = SqlStmt & " AND (REZEPT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON(index).Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtbis(index).Text))) & ")" & StrGid
          End If
          If Trim(txtSQL(index).Text) <> "" Then
            SqlStmt = SqlStmt & " AND " & StrSelct("REZEPT_NAME", txtSQL(index).Text)
          End If
        End If
        '
        '
        SqlStmt = SqlStmt & " ORDER BY REZEPT_NAME"
        '
        '
        '
        CmdRezept.CommandText = SqlStmt
        TabRezept(index).Clear()
        CmdRezept.CommandText = SqlStmt
        If Not FillDatset(AdaptRezept, TabRezept(index)) Then
          Exit Sub
        End If
        If dbgREZ(index).Rows.Count = 0 Then
          MsgBox(Texxt(2953))
        Else
          txtANZ(index).Visible = True
          lblANZ(index).Visible = True
          txtANZ(index).Text = CInt(dbgREZ(index).Rows.Count)
          btnRUN(index).Enabled = True
          btnMARK(index).Enabled = True
        End If
        '
        '
        '
        '
        Cursor = Cursors.Default

      Case 3


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

  '
  '
  Private Sub TabDARF_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles TabDARZ.SelectedIndexChanged
    Dim index As Integer
    Dim i As Integer
    index = sender.selectedindex
    Select Case index
      Case 1, 3
        txtKENN.Text = MenueParam.Messg.Kenn
    End Select
    If Not IsNothing(TabRezept) AndAlso TabRezept.Count > 0 Then
      For i = 0 To TabRezept.Count - 1
        If Not IsNothing(TabRezept(i)) Then
          TabRezept(i).Clear()
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
    chkDAT_0.CheckedChanged, chkDAT_1.CheckedChanged, chkDAT_2.CheckedChanged
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
  Sub DelRezRwert(kwb() As Integer, StrRezept As String, ByRef StrRwert As String)
    Dim j As Integer
    Dim i As Integer
    '
    '
    'Rwert_iD's, die gelöscht werden sollen werden in Abhängigkeit von kwb nach StrRwert gespeichert
    Dim StrSorti As String
    Dim StrNoRwert As String

    Dim IntRez() As String
    Dim IntSor() As String
    '
    'Tabelle der zu löschenden Sätze aus TBL_REZEPT_RWERT 
    '
    '
    SqlStmt = "SELECT MISCH_ID,MESSGRW_ID,REZEPT_ID,RWERT_ID,RWERT_KWB FROM TBL_REZEPT_RWERT" _
    & " WHERE MISCH_ID=" & cboMISCH.SelectedValue & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID _
    & " AND (RWERT_KWB=" & kwb(0) & " OR RWERT_KWB=" & kwb(1) & ") AND REZEPT_ID IN " & StrRezept

    CmdRezeptRwert.CommandText = SqlStmt
    If Not FillDatset(AdaptRezeptRwert, TabRezeptRwert) Then
      Exit Sub
    End If
    StrRwert = StrLin(TabRezeptRwert, "RWERT_ID")
    '
    '
    'Tabelle der nicht zu löschenden Sätze aus TBL_REZEPT_RWERT 
    '
    '
    SqlStmt = "SELECT MISCH_ID,MESSGRW_ID,REZEPT_ID,RWERT_ID,RWERT_KWB FROM TBL_REZEPT_RWERT" _
    & " WHERE MISCH_ID=" & cboMISCH.SelectedValue & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID _
    & " AND REZEPT_ID NOT IN " & StrRezept

    CmdRezeptRwert.CommandText = SqlStmt
    If Not FillDatset(AdaptRezeptRwert, TabRezeptRwert) Then
      Exit Sub
    End If
    StrNoRwert = StrLin(TabRezeptRwert, "RWERT_ID")
    '
    'Identische R-Werte in TBL_SORTI_RWERT suchen
    '
    '
    '
    SqlStmt = "SELECT MISCH_ID,MESSGRW_ID,SORTI_ID,RWERT_ID,RWERT_KWB FROM TBL_SORTI_RWERT" _
   & " WHERE MISCH_ID=" & cboMISCH.SelectedValue & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID IN " & StrRwert

    CmdSortiRwert.CommandText = SqlStmt
    If Not FillDatset(AdaptSortiRwert, TabSortiRwert) Then
      Exit Sub
    End If
    StrSorti = StrLin(TabSortiRwert, "RWERT_ID")
    StrSorti = StrSorti.Substring(0, StrSorti.Length - 1) & "," & StrNoRwert.Substring(1, StrNoRwert.Length - 1)
    '
    '
    'Lösche RWERT_ID in TabRezeptRwert, die in TBL_SORTI_RWERT bzw. Sätze, deren RezeptID nicht in StrRezept enthalten ist  
    '
    '
    IntRez = StrRwert.Substring(1, StrRwert.Length - 2).Split(",")
    IntSor = StrSorti.Substring(1, StrSorti.Length - 2).Split(",")
    For i = 0 To IntRez.Count - 1
      For j = 0 To IntSor.Count - 1
        If IntRez(i) <> "" And IntRez(i) <> "-999" And IntRez(i) = IntSor(j) Then
          IntRez(i) = ""
        End If
      Next j
    Next i
    StrRwert = "(-999,"
    For i = 0 To IntRez.Count - 1
      If IntRez(i) <> "" Then
        StrRwert = StrRwert & IntRez(i) & ","
      End If
    Next i
    StrRwert = StrRwert.Substring(0, StrRwert.Length - 1) & ")"
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




  Private Sub chkDickW_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkDickW_1.CheckedChanged, chkDickW_3.CheckedChanged
    Dim index As Integer
    index = CInt(sender.name.substring(9, 1))
    txtDickW(index).Visible = sender.checked
  End Sub



  Private Sub chkDickS_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkDickS_1.CheckedChanged, chkDickS_3.CheckedChanged
    Dim index As Integer
    index = CInt(sender.name.substring(9, 1))
    txtDickS(index).Visible = sender.checked
  End Sub






  Private Sub chkUntW_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkUntW_1.CheckedChanged, chkUNTW_3.CheckedChanged
    Dim index As Integer
    index = CInt(sender.name.substring(8, 1))
    If Not chkUNTW(index).Checked Then
      GrpRwerte(0).RefUnt.IVoNa = False
      lblUNTW(index).Visible = False
      btnUNTW(index).Enabled = False
    Else
      GrpRwerte(0).RefUnt.IVoNa = True
      lblUNTW(index).Visible = True
      btnUNTW(index).Enabled = True
    End If
  End Sub


  Private Sub chkUntS_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkUntS_1.CheckedChanged, chkuntS_3.CheckedChanged
    Dim index As Integer
    index = CInt(sender.name.substring(8, 1))
    If Not chkUNTS(index).Checked Then
      GrpRwerte(1).RefUnt.IVoNa = False
      lblUNTS(index).Visible = False
      btnUNTS(index).Enabled = False
    Else
      GrpRwerte(1).RefUnt.IVoNa = True
      lblUNTS(index).Visible = True
      btnUNTS(index).Enabled = True
    End If
  End Sub

  Private Sub chkUntergrName_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkUntergrName.CheckedChanged
    Dim i As Integer
    If Not chkUntergrName.Checked Then
      For i = 0 To btnUNTW.Count - 1
        If Not IsNothing(btnUNTW(i)) Then
          btnUNTW(i).Enabled = True
          btnUNTS(i).Enabled = True
        End If
      Next

    Else
      For i = 0 To btnUNTW.Count - 1
        If Not IsNothing(btnUNTW(i)) Then
          btnUNTW(i).Enabled = False
          btnUNTS(i).Enabled = False
          chkUNTW(i).Checked = False
          chkUNTS(i).Checked = False
        End If
      Next
      GrpRwerte(KeyRe(0)).RefUnt.IVoNa = False
      GrpRwerte(KeyRe(1)).RefUnt.IVoNa = False
      GrpRwerte(KeyRe(0)).RefUnt.Name = ""
      GrpRwerte(KeyRe(1)).RefUnt.Name = ""
      GrpRwerte(KeyRe(0)).RefUnt.ID = -1
      GrpRwerte(KeyRe(1)).RefUnt.ID = -1
    End If
  End Sub

  Private Sub chkUNT_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkUNT.CheckedChanged
    If chkUNT.Checked Then
      chkTYP(0).Checked = False
      chkSMP(0).Checked = False
      chkUUU(0).Checked = False
      chkTYP(0).Enabled = False
      chkSMP(0).Enabled = False
      chkUUU(0).Enabled = False
    Else
      chkTYP(0).Enabled = True
      chkSMP(0).Enabled = True
      chkUUU(0).Enabled = True
    End If
  End Sub
  '
  '
  '
  Private Sub btnUNTWS_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
              btnUntW_1.Click, btnUntS_1.Click, btnUNTW_3.Click, btnUNTS_3.Click
    Dim Index As Integer
    Index = 0
    If sender.name.substring(6, 1) = "S" Then
      Index = 1
    End If
    '
    '
    RcmdRef = Index
    GetPutReflex.Messrefel = GrpRwerte(Index).RefUnt
    GetPutReflex.Iarch = 0
    GetPutReflex.Captext = sender.Text
    GetPutReflex.Retr = BitInt(CShort(VKwb(Index)), CShort(VKwb(Index)), MenueParam.Messg.MeArtID)
    GetPutReflex.ReflexWerte(False)
    MenueParam.Messg.MeArtLock = BitInt(CShort(VKwb(Index)), CShort(VKwb(Index)), MenueParam.Messg.MeArtID)


  End Sub
  Private Sub GetNameID(ByRef ID As Integer, ByRef Name As String, ByRef Banum As String)
    Dim j As Integer
    Dim ier As Integer
    If Name <> "" Then
      '
      If GetPutReflex.HideSofort Then
        GetPutReflex.InVisible()
      End If

      '


      j = RcmdRef
      ReWrRwert.ReadRwert(ID, GrpRwerte(KeyRe(j)).RefUnt, ier)
      GrpRwerte(KeyRe(j)).RefUnt.IVoNa = True
      If j = 0 Then
        lblUNTW(TabDARZ.SelectedIndex).Text = Name
      Else
        lblUNTS(TabDARZ.SelectedIndex).Text = Name
      End If
    End If
  End Sub
  Private Sub frmColorthekEing_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    Dim i As Integer
    Try
      For i = 0 To HandleRezept.Count - 1
        HandleRezept(i) = Nothing
      Next
      HandleRwrt = Nothing
      HandleRezeptN = Nothing
      HandleRezept = Nothing
    Catch
    End Try

    If Not IsNothing(GetPutReflex) Then
      RemoveHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
    End If

  End Sub

  Private Sub cboMESS_Changed(sender As Object, e As System.EventArgs) Handles cboMESS.EnabledChanged, cboMESS.SelectedValueChanged
    If Not cboMESS.Enabled Then Exit Sub
    If Not IsNumeric(cboMESS.SelectedValue) Then Exit Sub
    AufbauPar.MessgID = cboMESS.SelectedValue
    HandleRwrt.cboGRP = cboGRR
    HandleRwrt.lblGRP = lblGRR
    HandleRwrt.GRoupList()


    txtKENN.Text = MenueParam.Messg.Kenn
  End Sub

  

  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtDickW_1.Validating, txtDickW_3.Validating, txtDickS_1.Validating, txtDickS_3.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub

  Private Sub txtDat_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtBIS_0.Validating, txtVON_0.Validating, txtBIS_1.Validating, txtVON_1.Validating, txtBIS_2.Validating, txtVON_2.Validating
    If Not IsDate(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(2500))
    End If
  End Sub

  Private Sub btnEnd_Click(sender As System.Object, e As System.EventArgs) Handles btnEnd.Click
    Me.Close()
  End Sub
End Class