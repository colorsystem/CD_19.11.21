Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmMessAdjust
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
  Dim MessgMAS As MeasParameters
  Dim MessgSLA As MeasParameters
  Dim TabRefSelected As DataTable
  Dim AdaptMessg As OleDbDataAdapter
  Dim CmdMessg As OleDbCommand
  Dim TabMessg As DataTable
  Dim ViewMessg As List(Of DataView)

  Dim AdaptGroup As OleDbDataAdapter
  Dim CmdGroup As OleDbCommand
  Dim TabGroup As DataTable
  Dim ViewGroup As List(Of DataView)

  Dim AdaptRwert As OleDbDataAdapter
  Dim CmdRwert As OleDbCommand
  Dim TabRwert As List(Of DataTable)
  Dim AdaptWerte As OleDbDataAdapter
  Dim CmdWerte As OleDbCommand
  Dim TabWerte As DataTable

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
  Dim cboMessG As List(Of ComboBox)
  Dim lblMESSG As List(Of Label)
  '
  '
  '
  Dim GrpRwerte As RefValuesGrp
  Dim ReWrRwert As ReadWriteRwert
  Dim HandleRwert As HandleRwerte
  Dim VKwb(1) As Integer
  Dim RefWert As RefValue
  Dim RefID As Integer
  Dim Ende As Boolean
  Dim Check As HandleHilfLib
  Dim AdjustMeasure As MessAdjust

  

  Private Sub frmMessAdjust_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    'Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    'Me.Location = New Point(0.5 * (MDform.Size.Width - Me.Size.Width), 0.5 * (0.5 * MDform.Size.Height - Me.Size.Height))
    '
    Me.Size = MDiform.size
    Me.Location = MDiform.Location
    Me.Text = Texxt(313)


    '
    AufbauPar.UserID = -1

    cbogrp = New List(Of ComboBox)
    cbogrp.Add(cboGRP_0)
    cbogrp.Add(cboGRP_1)
    cbogrp.Add(cboGRP_2)

    '
    '
    lblGRP = New List(Of Label)
    lblGRP.Add(lblGRP_0)
    lblGRP.Add(lblGRP_1)
    lblGRP.Add(lblGRP_2)

    '
    '
    '
    '
    '
    '
    '
    btnMARK = New List(Of Button)
    btnMARK.Add(Nothing)
    btnMARK.Add(btnMARK_1)

    '
    '

    btnZEIG = New List(Of Button)
    btnZEIG.Add(btnZEIG_0)
    btnZEIG.Add(btnZEIG_1)
    btnZEIG.Add(btnZEIG_0)

    '
    '
    '
    btnRUN = New List(Of Button)
    btnRUN.Add(btnRUN_0)
    btnRUN.Add(btnRUN_1)
    btnRUN.Add(btnRUN_0)

    '
    '
    btnCLIP = New List(Of Button)
    btnCLIP.Add(Nothing)
    btnCLIP.Add(btnCLIP_1)
    '
    '
    '
    dbgREF = New List(Of DataGridView)
    dbgREF.Add(dbgREF_0)
    dbgREF.Add(dbgREF_1)
    dbgREF.Add(dbgREF_2)


    '
    TabRwert = New List(Of DataTable)
    TabRwert.Clear()
    TabRwert.Add(New DataTable)
    TabRwert.Add(New DataTable)
    TabRwert.Add(New DataTable)

    '
    '
    lblKennAlt_0.Text = Texxt(3402) & "(" & Texxt(880) & ")"
    lblKennAlt_1.Text = Texxt(3402) & "(" & Texxt(880) & ")"
    lblKennNeu_0.Text = Texxt(3402) & "(" & Texxt(878) & ")"
    lblKennNeu_1.Text = Texxt(3402) & "(" & Texxt(878) & ")"

    '
    '
    '
    '
    lblANZ = New List(Of Label)
    lblANZ.Add(lblANZ_0)
    lblANZ.Add(lblANZ_1)
    lblANZ.Add(lblANZ_2)

    '
    '
    '
    '
    txtANZ = New List(Of TextBox)
    txtANZ.Add(txtANZ_0)
    txtANZ.Add(txtANZ_1)
    txtANZ.Add(txtANZ_2)

    '
    '
    '
    '
    '
    chkDAT = New List(Of CheckBox)
    chkDAT.Add(chkDAT_0)
    chkDAT.Add(chkDAT_1)
    chkDAT.Add(chkDAT_0)

    '
    '
    '
    lblVON = New List(Of Label)
    lblVON.Add(lblVON_0)
    lblVON.Add(lblVON_1)
    lblVON.Add(lblVON_0)

    '
    '
    '
    '
    txtVON = New List(Of TextBox)
    txtVON.Add(txtVON_0)
    txtVON.Add(txtVON_1)
    txtVON.Add(txtVON_0)

    '
    lblBIS = New List(Of Label)
    lblBIS.Add(lblBIS_0)
    lblBIS.Add(lblBIS_1)
    lblBIS.Add(lblBIS_0)

    '
    '
    '
    '
    txtbis = New List(Of TextBox)
    txtbis.Add(txtBIS_0)
    txtbis.Add(txtBIS_1)
    txtbis.Add(txtBIS_0)

    '
    '
    '
    '
    '
    lblSQL = New List(Of Label)
    lblSQL.Add(lblSQL_0)
    lblSQL.Add(lblSQL_1)
    lblSQL.Add(lblSQL_0)

    '
    '
    '
    txtSQL = New List(Of TextBox)
    txtSQL.Add(txtSQL_0)
    txtSQL.Add(txtSQL_1)
    txtSQL.Add(txtSQL_0)

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
    '
    For i = 0 To lblGRP.Count - 1
      lblGRP(i).Text = Texxt(386)
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
    Me.Text = Texxt(313)
    '
    '
    btnRUN(0).Text = Texxt(3694)
    btnRUN(1).Text = Texxt(3694)

    '
    '
    btnCLIP(1).Text = Texxt(3662)
    radSlave.Text = Texxt(204) & "(" & Texxt(3400) & ")"
    radMaster.Text = Texxt(204) & "(" & Texxt(3401) & ")"
    lblDBGref.Text = radSlave.Text

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
    lblGewichtung.Text = Texxt(172)
    '
    '
    lblMESSG_0.Text = Texxt(204)
    lblMESSG_1.Text = Texxt(204)
    lblMESSG_2.Text = Texxt(204)

    lblMESSG = New List(Of Label)
    lblMESSG.Clear()
    lblMESSG.Add(lblMESSG_0)
    lblMESSG.Add(lblMESSG_1)
    lblMESSG.Add(lblMESSG_2)


    '
    '
    TabMessAdjust.TabPages(0).Text = Texxt(3403)
    TabMessAdjust.TabPages(1).Text = Texxt(3404)
    TabMessAdjust.TabPages(2).Text = Texxt(1999)

    '
    '
    GrpRwerte = New RefValuesGrp
    GrpRwerte.Add("M", New RefValues)
    GrpRwerte.Add("S", New RefValues)
    GrpRwerte.Add("R", New RefValues)

    ReWrRwert = New ReadWriteRwert
    RefWert = New RefValue
    Check = New HandleHilfLib
    AdjustMeasure = New MessAdjust
    '
    '
    '
    '
    TabMessg = New DataTable
    AdaptMessg = New OleDbDataAdapter
    CmdMessg = New OleDbCommand("", Cncol)
    AdaptMessg.SelectCommand = CmdMessg


    AdaptRwert = New OleDbDataAdapter
    CmdRwert = New OleDbCommand("", Cndat)
    AdaptRwert.SelectCommand = CmdRwert
    '
    '
    '
    '
    '
    ' dbgRefSelected aufbauen

    dbgRefSelected.AlternatingRowsDefaultCellStyle.BackColor = Color.Azure
    dbgRefSelected.DefaultCellStyle.BackColor = Color.Beige
    'dbgRefSelected.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
    dbgRefSelected.AllowUserToAddRows = False
    dbgRefSelected.AllowUserToDeleteRows = True
    dbgRefSelected.ReadOnly = True
    dbgRefSelected.Columns.Add("RWERTSLA_ID", "IDSLA")
    dbgRefSelected.Columns(0).Visible = False
    dbgRefSelected.Columns.Add("RWERTSLA_NAME", radSlave.Text)
    dbgRefSelected.Columns.Add("RWERTMAS_ID", "IDMAS")
    dbgRefSelected.Columns(2).Visible = False
    dbgRefSelected.Columns.Add("RWERTMAS_NAME", radMaster.Text)
    dbgRefSelected.Columns.Add("DEALT", "DE(" & Texxt(880) & ")")
    dbgRefSelected.Columns.Add("DENEU", "DE(" & Texxt(878) & ")")



    dbgRefSelected.Columns(1).Width = 0.35 * dbgRefSelected.Width
    dbgRefSelected.Columns(3).Width = 0.35 * dbgRefSelected.Width
    dbgRefSelected.Columns(4).Width = 0.1 * dbgRefSelected.Width
    dbgRefSelected.Columns(5).Width = 0.1 * dbgRefSelected.Width

    'dbgRefSelected.Columns(0).DataPropertyName = dbgREF(i).Columns(0).Name
    'dbgRefSelected.Columns(1).DataPropertyName = dbgREF(i).Columns(1).Name
    'dbgRefSelected.Columns(2).DataPropertyName = dbgREF(i).Columns(2).Name
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
    TabWerte = New DataTable
    AdaptWerte = New OleDbDataAdapter
    CmdWerte = New OleDbCommand("", Cncol)
    '
    '
    HandleRwert = New HandleRwerte

    grdWERT.DataSource = TabWerte
    grdWERT.AllowUserToAddRows = False
    grdWERT.AllowUserToDeleteRows = False
    grdWERT.AlternatingRowsDefaultCellStyle.BackColor = Color.AntiqueWhite
    grdWERT.RowHeadersVisible = False
    grdWERT.SelectionMode = DataGridViewSelectionMode.RowHeaderSelect

    '
    'Menue-Parameter uebernehmen
    '
    '

    '
    txtbis(0).Text = Format$(DateAdd("d", -1 * 365, Today))
    txtVON(0).Text = Format$(DateAdd("d", -2 * 365, Today))
    For j = 0 To 2
      txtbis(j).Text = Format$(DateAdd("d", 0, Today))
      txtVON(j).Text = Format$(DateAdd("d", -1 * 365, Today))
    Next j
    '
    '
    '
    '
    '
    'Gruppen
    '
    TabGroup = New DataTable
    AdaptGroup = New OleDbDataAdapter
    CmdGroup = New OleDbCommand("", Cncol)
    AdaptGroup.SelectCommand = CmdGroup
    '
    ViewGroup = New List(Of DataView)
    ViewGroup.Clear()
    ViewGroup.Add(New DataView(TabGroup))
    ViewGroup.Add(New DataView(TabGroup))
    ViewGroup.Add(New DataView(TabGroup))

    '
    'INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID=TBL_MISCH_MESSG.MISCH_ID
    '
    TabGroup.Rows.Clear()
    SqlStmt = "SELECT * FROM TBL_MESSG_GROUP ORDER BY GROUP_KBEZ"
    CmdGroup.CommandText = SqlStmt
    If Not FillDatset(AdaptGroup, TabGroup) Then
      Exit Sub
    End If
    For i = 0 To cbogrp.Count - 1
      cbogrp(i).DataSource = ViewGroup(i)

      cbogrp(i).Enabled = False
      cbogrp(i).DisplayMember = "GROUP_KBEZ"
      cbogrp(i).ValueMember = "GROUP_ID"
      cbogrp(i).SelectedIndex = -1
      cbogrp(i).Enabled = True
      cbogrp(i).SelectedValue = TabGroup.Rows(0)("GROUP_ID")
      cbogrp(i).Enabled = True
    Next i
    '
    '
    '
    'Messgeräte
    '
    '
    cboMessG = New List(Of ComboBox)
    cboMessG.Clear()
    cboMessG.Add(cboMESSG_0)
    cboMessG.Add(cboMESSG_1)
    cboMessG.Add(cboMESSG_2)

    For i = 0 To cboMessG.Count - 1
      cboMessG(i).DataSource = Nothing
    Next i

    '
    '
    ViewMessg = New List(Of DataView)
    ViewMessg.Clear()
    ViewMessg.Add(New DataView(TabMessg))
    ViewMessg.Add(New DataView(TabMessg))
    ViewMessg.Add(New DataView(TabMessg))

    '
    '
    TabMessg.Rows.Clear()
    SqlStmt = "SELECT *,TBL_MESSG.MESSG_ID AS MESSG_ID FROM TBL_MESSG ORDER BY MESSG_KBEZ"
    CmdMessg.CommandText = SqlStmt
    If Not FillDatset(AdaptMessg, TabMessg) Then
      Exit Sub
    End If
    For i = 0 To cboMessG.Count - 1
      cboMessG(i).DataSource = ViewMessg(i)

      cboMessG(i).Enabled = False
      cboMessG(i).DisplayMember = "MESSG_KBEZ"
      cboMessG(i).ValueMember = "MESSG_ID"
      cboMessG(i).SelectedIndex = -1
      cboMessG(i).Enabled = True
      cboMessG(i).SelectedValue = TabMessg.Rows(0)("MESSG_ID")
    Next i
    '
    'TabMessAdjust.ItemSize = New Size((TabMessAdjust.Width - TabMessAdjust.TabPages.Count) / TabMessAdjust.TabPages.Count - 3, TabMessAdjust.ItemSize.Height)
    TabMessAdjust.ItemSize = New Size((TabMessAdjust.Width - 20) / (1.0 * TabMessAdjust.TabPages.Count), TabMessAdjust.ItemSize.Height)

    '
  End Sub

  Private Sub frmTabMessAdjust_Paint(sender As Object, e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint
    
  End Sub

  

  '
  '
  '
  Private Sub btnCLIP_Click(sender As Object, e As System.EventArgs) Handles btnCLIP_1.Click
    Dim index As Integer
    Dim i As Integer
    Dim j As Integer
    index = CInt(sender.name.substring(8, 1))
    Select Case index
      Case 1
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
        grdWERT.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
        grdWERT.SelectAll()

        If grdWERT.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
          Clipboard.SetDataObject(grdWERT.GetClipboardContent)
        End If
        grdWERT.ClearSelection()
      Case 3

    End Select


  End Sub
  '
  '
  '
  '
  '
  Private Sub btnMARK_Click(sender As Object, e As System.EventArgs) Handles btnMARK_1.Click
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
   
  '
  '
  '
  '
  '
  Private Sub btnRUN_Click(sender As Object, e As System.EventArgs) Handles btnRUN_0.Click, btnRUN_1.Click
    Dim index As Integer
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim ier As Integer
    Dim Rindex As Integer
    Dim GWI As Single
    Cursor = Cursors.WaitCursor
    Application.DoEvents()
    index = CInt(sender.name.substring(7, 1))
    Select Case index
      Case 0
        '
        '
        '
        If cboMessG(0).SelectedValue = cboMessG(2).SelectedValue And txtKennAlt_0.Text = txtKennNeu_0.Text Then
          MsgBox("Messgeräte identisch")
          GoTo endrun
        End If
        '
        AufbauPar.MessgID = cboMessG(0).SelectedValue
        MessgSLA = MenueParam.Messg.clone

        AufbauPar.MessgID = cboMessG(2).SelectedValue
        MessgMAS = MenueParam.Messg.clone
        '
        'Prüfen, ob Messgeräte kompatibel
        '
        '
        '
        Check.CheckMeasureDev(Cncol, Cncol, cboMessG(0).SelectedValue, cboMessG(2).SelectedValue, ier)
        If ier <> 0 Then
          GoTo endrun
        End If
        '
        '
        'Messgerät anpassen
        '
        '
        If dbgRefSelected.Rows.Count <= 0 Then
          MsgBox(Texxt(496) & Space(1) & Texxt(2984))
          GoTo endrun
        End If
        '
        '
        'Einlesen der R-Werte
        '
        '
        If dbgRefSelected.Rows(dbgRefSelected.Rows.Count - 1).Cells("RWERTSLA_ID").Value = -1 Or _
          dbgRefSelected.Rows(dbgRefSelected.Rows.Count - 1).Cells("RWERTMAS_ID").Value = -1 Then
          MsgBox(Texxt(496) & Space(1) & Texxt(2984))
          GoTo EndRun
        End If
        '
        '
        'R-Werte für Master-Gerät
        '
        AufbauPar.MessgID = cboMessG(2).SelectedValue

        GrpRwerte("M").clear()
        For i = 0 To dbgRefSelected.Rows.Count - 1
          Rindex = dbgRefSelected.Rows(i).Cells("RWERTMAS_ID").Value
          GrpRwerte("M").Add(KeyRe(i), New RefValue)
          ReWrRwert.ReadRwert(Rindex, GrpRwerte("M")(KeyRe(i)), ier)
          If ier <> 0 Then
            GoTo endrun
          End If
        Next
        '
        '
        '
        'R-Werte für Slave-Gerät
        '
        '
        '
        AufbauPar.MessgID = cboMessG(0).SelectedValue

        GrpRwerte("S").clear()
        For i = 0 To dbgRefSelected.Rows.Count - 1
          Rindex = dbgRefSelected.Rows(i).Cells("RWERTSLA_ID").Value
          GrpRwerte("S").Add(KeyRe(i), New RefValue)
          ReWrRwert.ReadRwert(Rindex, GrpRwerte("S")(KeyRe(i)), ier)
          If ier <> 0 Then
            Exit Sub
          End If
        Next

        '
        '
        'Rechnung durchführen
        '
        '
        '
        GWI = CSng(txtGewichtung.Text)
        AdjustMeasure.CalculateNewAbsValues(GWI, MessgSLA, MenueParam.Normfa(0), txtKennNeu_0.Text, GrpRwerte, ier)
        If ier <> 0 Then
          GoTo endrun
        End If
        '
        '
        '
        '
        '
        'Farbwerte übernehmen
        '
        '
        '
        For k = 0 To GrpRwerte("R").Count - 1
          dbgRefSelected.Rows(k).Cells("DEALT").Value = GrpRwerte("S")(k).De(0)
          dbgRefSelected.Rows(k).Cells("DENEU").Value = GrpRwerte("R")(k).De(0)

        Next k
        '


        GoTo endrun
        '
        '
        '
        '
        '
        '
        '
        '
        '
      Case 1
        '
        '
        'R-Werte umrechnen
        '
        '
        '
        If dbgREF(index).Rows.Count = 0 Then
          GoTo endrun
        End If
        '
        '
        '
        'R-Werte einlesen
        '
        '
        GrpRwerte("S").clear()
        If ConnOpen(Cndat) Then
          For i = 0 To dbgREF(index).SelectedRows.Count - 1
            RefID = dbgREF(index).SelectedRows(i).Cells("RWERT_ID").Value

            '
            '
            'Tabelle aufbauen
            '
            '
            '
            '
            '
            '
            GrpRwerte("S").Add(KeyRe(i), New RefValue)
            ReWrRwert.ReadRwert(RefID, GrpRwerte("S")(i), ier)
            If ier > 0 Then Exit Sub
          Next i
          Cndat.Close()
        End If
        '
        '
        '
        '
        'R-Werte umrechnen
        '
        AufbauPar.MessgID = cboMessG(1).SelectedValue
        MessgSLA = MenueParam.Messg.clone
        '
        '
        Call AdjustMeasure.CalculateNewRefValues(MessgSLA, MenueParam.Normfa(0), txtKennNeu_1.Text, GrpRwerte, ier)
        If ier <> 0 Then
          GoTo endrun
        End If
        '
        ' 
        '
        '
        '
        'Tabelle Tabwerte aufbauen
        '
        '

        '
        '
        Call HandleRwert.MakeTABRefwerte(False, MenueParam.Messg.Kbez, MenueParam.Messg.Winkel, TabWerte)
        '
        Call HandleRwert.MakeGRIDRefwerte(MenueParam.Messg.Winkel, grdWERT)


        '
        TabMessAdjust.TabPages(index).Enabled = False

        For i = 0 To GrpRwerte("R").Count - 1


          '
          '
          'Tabelle aufbauen
          '
          '
          '
          '
          '
          '



          j = Abs(GrpRwerte("R")(i).kwb) + 1
          Call HandleRwert.TabRefRecord(j, MenueParam.Messg.Winkel, 100.0, GrpRwerte("R")(i), TabWerte, ier)
          If ier > 0 Then Exit Sub

        Next i


        TabMessAdjust.TabPages(index).Enabled = True
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
          grdWERT.Visible = False
        Else
          grdWERT.Visible = True
          dbgREF(index).Visible = False
          btnCLIP(index).Enabled = True
          btnMARK(index).Enabled = False
        End If

        Application.DoEvents()
        '
        '
endrun:
        Cursor = Cursors.Default

        '


        '
        '
        '


    End Select
    Cursor = Cursors.Default
  End Sub


  Private Sub btnZEIG_Click(sender As Object, e As System.EventArgs) Handles btnZEIG_0.Click, btnZEIG_1.Click
    Dim index As Integer
    Dim i As Integer
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
    dbgRefSelected.Rows.Clear()
    Cursor = Cursors.WaitCursor
    Select Case index
      Case 0
      Case 1
        btnMARK(index).Enabled = True
        dbgREF(index).Visible = True
        grdWERT.Visible = False
        btnCLIP(index).Enabled = False
    End Select
    For i = 0 To 2
      '
      If i = index Or (i = 2 And index = 0) Then
        '
        '
        'Aufbau der dbgGRID-Tabelle zum Korrigieren bzw. Umspeichern
        '
        '
        '
        '
        btnRUN(i).Enabled = False
        Application.DoEvents()
        txtANZ(i).Visible = False
        lblANZ(i).Visible = False
        StrGid = ""
        If cbogrp(i).SelectedValue > 0 Then
          StrGid = " AND RWERT_GID= " & cbogrp(i).SelectedValue
        End If
        'If chkARC(i).CheckState < 2 Then
        'StrGid = StrGid & " AND RWERT_IARCH=" & chkARC(i).CheckState
        'End If
        SqlStmt = "SELECT RWERT_ID,RWERT_DATTIM,RWERT_NAME FROM TBL_RWERT" _
        & " WHERE MESSG_ID=" & cboMessG(i).SelectedValue
        If chkDAT(i).Checked Then
          SqlStmt = SqlStmt & " AND (RWERT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON(i).Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtbis(i).Text))) & ")" & StrGid
        Else
          SqlStmt = SqlStmt & StrGid
        End If
        If Trim(txtSQL(i).Text) <> "" Then
          SqlStmt = SqlStmt & " AND " & StrSelct("RWERT_NAME", txtSQL(i).Text)
        End If

        '
        '
        'SqlStmt = SqlStmt & " AND NOT RWERT_NAME IS NULL ORDER BY RWERT_NAME"
        'MsgBox SqlStmt
        '
        '
        CmdRwert.CommandText = SqlStmt
        TabRwert(i).Clear()
        CmdRwert.CommandText = SqlStmt
        If Not FillDatset(AdaptRwert, TabRwert(i)) Then
          Exit Sub
        End If
        'datREF(i).Recordset = Ddat.OpenRecordset(SqlStmt, dbOpenDynaset, dbReadOnly)
        If dbgREF(i).Rows.Count = 0 Then
          If i = 0 Then
            MsgBox(Texxt(2959) & Space(1) & radSlave.Text)
          Else
            MsgBox(Texxt(2959) & Space(1) & radMaster.Text)

          End If
        Else
          txtANZ(i).Visible = True
          lblANZ(i).Visible = True
          txtANZ(i).Text = CInt(dbgREF(i).Rows.Count)
          btnRUN(i).Enabled = True
          'btnMARK(i).Enabled = True
        End If
        End If
      '
    Next i
    '
    '
    '
    Cursor = Cursors.Default



    Exit Sub
FehlZeig:
    MsgBox(Err.Description)
    On Error GoTo 0
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
  '
  Private Sub TabDARF_SelectedIndexChanged(sender As Object, e As System.EventArgs)
    Dim index As Integer
    Dim i As Integer

    'index = sender.selectedindex
    'If Not IsNothing(TabRwert) AndAlso TabRwert.Count > 0 Then
    ' For i = 0 To TabRwert.Count - 1
    ' If Not IsNothing(TabRwert(i)) Then
    ' TabRwert(i).Clear()
    ' End If
    ' Next
    ' End If
  End Sub


  '
  
  '

  
  Private Sub chkDAT_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkDAT_0.CheckedChanged, chkDAT_1.CheckedChanged
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
    
    Exit Sub
DbRucErr:
    'MsgBox Texxt(3610) & Error
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


 
 
 

  Private Sub cboMESSG_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboMESSG_0.SelectedValueChanged, cboMESSG_1.SelectedValueChanged, cboMESSG_2.SelectedValueChanged
    Dim index As Integer
    If Not IsNumeric(sender.selectedvalue) Then Exit Sub
    If Not sender.enabled Then Exit Sub
    index = CInt(sender.name.substring(9, 1))
    ViewGroup(index).RowFilter = "MESSG_ID=" & sender.selectedvalue
    '
    '
    '
    AufbauPar.MessgID = sender.selectedvalue
    If index = 0 Then
      MessgSLA = MenueParam.Messg.clone

    End If
    txtKennAlt_0.Text = MenueParam.Messg.Kenn
    txtKennAlt_1.Text = MenueParam.Messg.Kenn

  End Sub

  Private Sub radMasterSlave_CheckedChanged(sender As Object, e As System.EventArgs) Handles radMaster.CheckedChanged, radSlave.CheckedChanged
    Dim indexVis As Integer
    Dim IndexNvi As Integer

    If cbogrp Is Nothing Then Exit Sub
    If Not sender.checked Then Exit Sub
    If sender.name = "radslave" Then
      indexVis = 0
      IndexNvi = 2
      lblKennAlt_0.Visible = True
      lblKennNeu_0.Visible = True
      txtKennAlt_0.Visible = True
      txtKennNeu_0.Visible = True
    Else
      indexVis = 2
      IndexNvi = 0
      lblKennAlt_0.Visible = False
      lblKennNeu_0.Visible = False
      txtKennAlt_0.Visible = False
      txtKennNeu_0.Visible = False
      btnZEIG_0.Enabled = True
    End If
    lblDBGref.Text = sender.text
    '
    'sichtbar
    '
    '
    cbogrp(indexVis).Visible = True
    lblGRP(indexVis).Visible = True
    cboMessG(indexVis).Visible = True
    lblMESSG(indexVis).Visible = True
    dbgREF(indexVis).Visible = True
    lblANZ(indexVis).Visible = True
    txtANZ(indexVis).Visible = True
    '
    'nicht sichtbar
    '
    '
    cbogrp(IndexNvi).Visible = False
    lblGRP(IndexNvi).Visible = False
    cboMessG(IndexNvi).Visible = False
    lblMESSG(IndexNvi).Visible = False
    dbgREF(IndexNvi).Visible = False
    lblANZ(IndexNvi).Visible = False
    txtANZ(IndexNvi).Visible = False

  End Sub

  Private Sub dbgREF_CellClick(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dbgREF_0.CellClick, dbgREF_2.CellClick
    Dim index As Integer
    Dim Rindex As Integer
    index = CInt(sender.name.substring(7, 1))
    '
    '
    '
    'Abfragen
    '
    Rindex = dbgRefSelected.Rows.Count - 1
    If dbgRefSelected.Rows.Count = 0 OrElse (dbgRefSelected.Rows(Rindex).Cells("RWERTSLA_ID").Value <> -1 And dbgRefSelected.Rows(Rindex).Cells("RWERTMAS_ID").Value <> -1) Then
      '
      'neuer eintrag
      '
      '

      dbgRefSelected.Rows.Add(New DataGridViewRow)
      Rindex = dbgRefSelected.Rows.Count - 1
      dbgRefSelected.Rows(Rindex).Cells("RWERTSLA_ID").Value = -1
      dbgRefSelected.Rows(Rindex).Cells("RWERTMAS_ID").Value = -1
      dbgRefSelected.Rows(Rindex).Cells("RWERTSLA_NAME").Value = ""
      dbgRefSelected.Rows(Rindex).Cells("RWERTMAS_NAME").Value = ""
      If index = 0 Then
        dbgRefSelected.Rows(Rindex).Cells("RWERTSLA_ID").Value = TabRwert(index).Rows(e.RowIndex)("RWERT_ID")
        dbgRefSelected.Rows(Rindex).Cells("RWERTSLA_NAME").Value = TabRwert(index).Rows(e.RowIndex)("RWERT_NAME")
      ElseIf index = 2 Then
        dbgRefSelected.Rows(Rindex).Cells("RWERTMAS_ID").Value = TabRwert(index).Rows(e.RowIndex)("RWERT_ID")
        dbgRefSelected.Rows(Rindex).Cells("RWERTMAS_NAME").Value = TabRwert(index).Rows(e.RowIndex)("RWERT_NAME")
      End If
    Else
      If index = 0 Then
        dbgRefSelected.Rows(Rindex).Cells("RWERTSLA_ID").Value = TabRwert(index).Rows(e.RowIndex)("RWERT_ID")
        dbgRefSelected.Rows(Rindex).Cells("RWERTSLA_NAME").Value = TabRwert(index).Rows(e.RowIndex)("RWERT_NAME")
      ElseIf index = 2 Then
        dbgRefSelected.Rows(Rindex).Cells("RWERTMAS_ID").Value = TabRwert(index).Rows(e.RowIndex)("RWERT_ID")
        dbgRefSelected.Rows(Rindex).Cells("RWERTMAS_NAME").Value = TabRwert(index).Rows(e.RowIndex)("RWERT_NAME")
      End If
    End If
  End Sub
  '
  '
  '
  '

  Private Sub TabMessAdjust_Click(sender As Object, e As System.EventArgs) Handles TabMessAdjust.Click
    If TabMessAdjust.SelectedIndex = 2 Then
      Me.DialogResult = DialogResult.OK
    End If
  End Sub

End Class