Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmNWI
  Dim MnUserid As Integer
  Dim MnMessgID As Integer
  Dim MnMethID As Integer
  Dim MnMischid As Integer
  Dim MnIopenArt As Boolean
  Dim MnUserUfo As Boolean

  Dim MaxGkwID As Integer
  'Anzahl Reflexionskurven (+Zusatzinformation) QU-Kontrolle
  '
  Dim ier As Integer
  Dim Iprn As Integer
  '
  '
  Dim SqlStmt As String
  Dim SqlEtmt As String
  '
  Dim menuID As Integer            'Menü ID
  Dim RezmnID As Integer           'ID für Menü für Rezeptrechnung
  Dim GkwrtID As Integer           'ID für GK_werte
  Dim GkaltID As Integer           'ID für GK_werte (alt)
  Dim GkSperr As Integer           'Kennung, ob Gk-Werte zur Berechnung von Grunddaten verwendet wurden
  Dim lchgID As Integer            'ID Gewichte DL,DC und DH
  Dim matpaID As Integer           'ID für mathematische Parameter
  Dim StrLinUse As String
  Dim StrLinMsg As String
  Dim StrLinMet As String
  '

  Dim MnAform As Form
  '
  '

  REM Laufvariable
  Dim i As Integer
  Dim l As Integer
  Dim k As Integer
  Dim j As Integer
  Dim kw As Integer
  REM Zwischengroessen
  'Message Kennung
  Dim imsg As Integer            'Messagebox (Rückgabewert)
  Dim MessgNwe As Integer
  Dim MessgNwp As Integer
  Dim WellStart As Integer
  Dim WellEnde As Integer
  Dim WellStep As Integer
  Dim MessgTop As Integer
  Dim MessgTdiff As Integer
  Dim MessgTwait As Integer
  Dim MessgStell As Integer
  Dim MessgImes As Integer
  Dim MessgIkal As Integer
  Dim MessgDE As Single
  Dim MessgLaborID As Integer
  Dim MessgNormFileID As Integer
  Dim TabName As String
  Dim Whilf() As Single
  '
  '
  '
  Dim txtZUS As List(Of TextBox)
  Dim cboSTD As List(Of ComboBox)
  Dim txtGKW As List(Of TextBox)
  Dim lblGKW As List(Of Label)
  '
  Dim CmdUSE As OleDbCommand
  Dim CmdMSG As OleDbCommand
  Dim CmdMET As OleDbCommand
  Dim CmdGRP As OleDbCommand
  Dim CmdMTG As OleDbCommand
  Dim CmdGKW As OleDbCommand
  Dim CmdLAB As OleDbCommand
  Dim CmdSET As OleDbCommand '
  Dim CmdHLF As OleDbCommand '
  Dim CmdWIN As OleDbCommand '
  Dim CmdWIA As OleDbCommand
  Dim CmdEXT As OleDbCommand
  '
  '
  '
  Dim AdaptUSE As OleDbDataAdapter
  Dim AdaptMSG As OleDbDataAdapter
  Dim AdaptMTG As OleDbDataAdapter
  Dim AdaptMET As OleDbDataAdapter
  Dim AdaptGRP As OleDbDataAdapter
  Dim AdaptGKW As OleDbDataAdapter
  Dim AdaptLAB As OleDbDataAdapter
  Dim AdaptWIN As OleDbDataAdapter
  Dim AdaptWIA As OleDbDataAdapter
  Dim AdaptEXT As OleDbDataAdapter
  '
  '
  Dim TblUSE As DataTable
  Dim TblMTG As DataTable
  Dim TblMSG As DataTable
  Dim TblMET As DataTable
  Dim TblGRP As DataTable
  Dim TblGKW As DataTable
  Dim TblLAB As DataTable
  Dim TblWIN As DataTable
  Dim TblWIA As DataTable
  Dim TblEXT As DataTable
  '
  '
  '
  Dim ViewUse As DataView
  Dim ViewMTG As DataView
  Dim ViewMSG As DataView
  Dim ViewMET As DataView
  Dim ViewWIN As DataView
  Dim ViewWIA As DataView
  Dim ViewGRP As DataView
  Dim ViewEXT As DataView
  '
  '
  '
  Dim WithEvents ConnUSE As BindingSource
  Dim WithEvents ConnMTG As BindingSource
  Dim WithEvents ConnMSG As BindingSource
  Dim WithEvents ConnMET As BindingSource
  Dim WithEvents ConnGKW As BindingSource

  '
  '
  '
  Dim DatHLF As OleDbDataReader
  '
  Private Sub frmNWI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer

    Dim DataGridCombo As DataGridViewComboBoxColumn
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    Me.Text = Texxt(411)
    btnORD.Text = Texxt(1999)
    lblGRP.Text = Texxt(386)
    lblMEI.Text = Texxt(295)
    lblMES.Text = Texxt(420)
    lblMIT.Text = Texxt(175)
    lblMTH.Text = Texxt(421)
    lblNAU.Text = Texxt(201)
    lblRETR.Text = Texxt(224)
    lblWIA.Text = Texxt(230)
    lblWIN.Text = Texxt(229)

    lblZUS_0.Text = Texxt(2300)
    lblZUS_1.Text = Texxt(2301)
    lblZUS_2.Text = Texxt(2302)
    lblZUS_3.Text = Texxt(2303)
    lblZUS_4.Text = Texxt(2304)
    lblZUS_5.Text = Texxt(2305)
    lblZUS_6.Text = Texxt(2306)
    lblZUS_7.Text = Texxt(2307)
    lblZUS_8.Text = Texxt(2308)
    btnGKSpei0.Text = Texxt(517)
    chkGKExtra.Text = Texxt(518)

    lblGKW_00.Text = Texxt(250)
    lblGKW_01.Text = Texxt(251)
    lblGKW_02.Text = Texxt(252)
    lblGKW_03.Text = Texxt(253)
    lblGKW_04.Text = Texxt(254)
    lblGKW_05.Text = Texxt(255)
    lblGKW_06.Text = Texxt(256)
    lblGKW_07.Text = Texxt(257)
    lblGKW_08.Text = Texxt(258)
    lblGKW_09.Text = Texxt(259)
    lblGKW_10.Text = Texxt(260)
    lblGKW_11.Text = Texxt(687)
    lblGKW_12.Text = Texxt(265)
    lblGKW_13.Text = Texxt(266)
    '
    '
    TabNWI.TabPages(0).Text = Texxt(421)
    TabNWI.TabPages(1).Text = Texxt(151)
    '
    '
    '
    '
    '
    '
    '
    TblUSE = New DataTable
    TblMTG = New DataTable
    TblMSG = New DataTable
    TblMET = New DataTable
    TblGRP = New DataTable
    TblGKW = New DataTable
    TblLAB = New DataTable
    TblWIN = New DataTable
    TblWIA = New DataTable
    TblEXT = New DataTable
    '
    '
    '
    ViewUse = New DataView(TblUSE)
    ViewMTG = New DataView(TblMTG)
    ViewMSG = New DataView(TblMSG)
    ViewMET = New DataView(TblMET)
    ViewWIN = New DataView(TblWIN)
    ViewWIA = New DataView(TblWIA)

    '
    '
    CmdUSE = New OleDbCommand("", Cncol)
    CmdMTG = New OleDbCommand("", Cncol)
    CmdMSG = New OleDbCommand("", Cncol)
    CmdMET = New OleDbCommand("", Cncol)
    CmdGRP = New OleDbCommand("", Cncol)
    CmdGKW = New OleDbCommand("", Cncol)
    CmdLAB = New OleDbCommand("", Cncol)
    CmdSET = New OleDbCommand("", Cncol)
    CmdHLF = New OleDbCommand("", Cncol)
    CmdWIN = New OleDbCommand("", Cncol)
    CmdWIA = New OleDbCommand("", Cncol)
    CmdEXT = New OleDbCommand("", Cncol)
    '
    '
    '
    AdaptUSE = New OleDbDataAdapter
    AdaptMTG = New OleDbDataAdapter
    AdaptMSG = New OleDbDataAdapter
    AdaptMET = New OleDbDataAdapter
    AdaptGRP = New OleDbDataAdapter
    AdaptGKW = New OleDbDataAdapter
    AdaptLAB = New OleDbDataAdapter
    AdaptWIN = New OleDbDataAdapter
    AdaptWIA = New OleDbDataAdapter
    AdaptEXT = New OleDbDataAdapter
    '
    AdaptUSE.SelectCommand = CmdUSE
    AdaptMTG.SelectCommand = CmdMTG
    AdaptMSG.SelectCommand = CmdMSG
    AdaptMET.SelectCommand = CmdMET
    AdaptGRP.SelectCommand = CmdGRP
    AdaptGKW.SelectCommand = CmdGKW
    AdaptLAB.SelectCommand = CmdLAB
    AdaptWIN.SelectCommand = CmdWIN
    AdaptWIA.SelectCommand = CmdWIA
    AdaptEXT.SelectCommand = CmdEXT
    '
    '
    ConnUSE = New BindingSource
    ConnMTG = New BindingSource
    ConnMSG = New BindingSource
    ConnMET = New BindingSource
    ConnGKW = New BindingSource
    '
    '
    '
    '
    '
    '
    '
    BindingUSE.BindingSource = ConnUSE
    BindingMSG.BindingSource = ConnMSG
    BindingMET.BindingSource = ConnMET
    BindingGKW.BindingSource = ConnGKW



    txtGKW = New List(Of TextBox)
    lblGKW = New List(Of Label)
    txtGKW.Add(txtGKW_00)
    txtGKW.Add(txtGKW_01)
    txtGKW.Add(txtGKW_02)
    txtGKW.Add(txtGKW_03)
    txtGKW.Add(txtGKW_04)
    txtGKW.Add(txtGKW_05)
    txtGKW.Add(txtGKW_06)
    txtGKW.Add(txtGKW_07)
    txtGKW.Add(txtGKW_08)
    txtGKW.Add(txtGKW_09)
    txtGKW.Add(txtGKW_10)
    txtGKW.Add(txtGKW_11)
    txtGKW.Add(txtGKW_12)
    txtGKW.Add(txtGKW_13)
    txtGKW.Add(txtGKW_14)
    '
    '
    '
    lblGKW.Add(lblGKW_00)
    lblGKW.Add(lblGKW_01)
    lblGKW.Add(lblGKW_02)
    lblGKW.Add(lblGKW_03)
    lblGKW.Add(lblGKW_04)
    lblGKW.Add(lblGKW_05)
    lblGKW.Add(lblGKW_06)
    lblGKW.Add(lblGKW_07)
    lblGKW.Add(lblGKW_08)
    lblGKW.Add(lblGKW_09)
    lblGKW.Add(lblGKW_10)
    lblGKW.Add(lblGKW_11)
    lblGKW.Add(lblGKW_12)
    lblGKW.Add(lblGKW_13)
    '
    '
    txtZUS = New List(Of TextBox)
    txtZUS.Add(txtZUS_0)
    txtZUS.Add(txtZUS_1)
    txtZUS.Add(txtZUS_2)
    txtZUS.Add(txtZUS_3)
    txtZUS.Add(txtZUS_4)
    txtZUS.Add(txtZUS_5)
    txtZUS.Add(txtZUS_6)
    txtZUS.Add(txtZUS_7)
    txtZUS.Add(txtZUS_8)
    txtZUS.Add(txtZUS_9)
    '
    '
    If BitWrt(28, MenueParam.User.Drum) Or MnIopenArt Then
      flgGKExtra.Visible = True

    End If
    '
    '
    'Benutzer
    '
    '
    'Art der Mittelung
    '
    '
    'Auswahl arith. geom. harm.
    '
    '
    cboMIT.Items.Clear()

    For i = 0 To 2
      cboMIT.Items.Add(New ListTextID(i + 1, Texxt(176 + i)))
    Next i
    '
    '
    'Ja/Nein für Initialisieren oder Kalibrieren
    '
    '
    '
    '
    '
    cboSTD = New List(Of ComboBox)
    cboSTD.Clear()
    cboSTD.Add(cboSTD_0)
    cboSTD.Add(cboSTD_1)
    '
    '
    For i = 0 To 1
      cboSTD(i).Items.Clear()
      For j = 0 To 1
        cboSTD(i).Items.Add(New ListTextID(j, Texxt(3 + j)))
      Next j
    Next i
    '
    '
    'Auswahl Reflexion/Transmission
    '
    '
    cboRETR.Items.Clear()

    For i = 0 To 1
      cboRETR.Items.Add(New ListTextID(i, Texxt(225 + i)))
    Next i
    '
    'Art der Rechnung
    '
    '
    cboRART.Items.Clear()
    For i = 0 To 5
      cboRART.Items.Add(New ListTextID(Asc(ArtCME(i)), Texxt(1000 + i)))
    Next i
    '
    '
    'bei methodenabhängigen GK-Werten (Meth_id >49 und Meth_id<=100)
    'sind nicht alle GK-Werte erforderlich
    '
    '
    '
    'Liste für Labors
    '
    '
    CmdLAB.CommandText = "SELECT * FROM TBL_LABOR"
    'datGKW.Recordset = DBas.OpenRecordset(SqlStmt, dbOpenDynaset, dbConsistent, dbReadOnly)

    If Not FillDatset(AdaptLAB, TblLAB) Then
      Exit Sub
    End If
    TblLAB.AcceptChanges()
    If TblLAB.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_LABOR")
    End If ''
    cboLAB.DataSource = TblLAB
    cboLAB.DisplayMember = "LABOR_KBEZ"
    cboLAB.ValueMember = "LABOR_ID"
    '


    'datLAB.Recordset = DBas.OpenRecordset("TBL_LABOR", dbOpenDynaset, dbConsistent, dbReadOnly)

    '
    txtGKW(13).Visible = False
    lblGKW(12).Visible = False
    txtGKW(8).Visible = True
    lblGKW(7).Visible = True
    txtGKW(9).Visible = True
    lblGKW(8).Visible = True
    txtGKW(11).Visible = True
    lblGKW(10).Visible = True
    txtGKW(12).Visible = False
    lblGKW(11).Visible = False
    cboRART.Visible = False
    '
    '
    '
    '
    'TBL_GKWRT GK_Werte und Berechnungsart für Reflexion (Transmission)
    '
    '
    '
    '
    GkwrtID = -1
    SqlStmt = "SELECT * FROM TBL_GKWRT ORDER BY GKWRT_ID"
    CmdGKW.CommandText = SqlStmt
    If Not FillDatset(AdaptGKW, TblGKW) Then
      Exit Sub
    End If
    TblGKW.AcceptChanges()
    If TblGKW.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_GKWRT")
      Exit Sub
    End If '
    '
    GkwrtID = 0
    '
    'TBL_GKWRTEX
    '
    Call CalcGKExtra(flgGKExtra, TblEXT, ViewEXT)
    '
    '
    'Alle Winkel/Messgeometrien 
    '
    SqlStmt = "SELECT TBL_MESSG_IHRM.MESSG_ID,TBL_MESSG_IHRM.IHRM_ID,POS_ID,IHRM_BEZ " _
    & " FROM TBL_MESSG_IHRM,TBL_IHRM WHERE TBL_MESSG_IHRM.IHRM_ID=TBL_IHRM.IHRM_ID" _
    & " ORDER BY MESSG_ID,POS_ID"
    CmdWIN.CommandText = SqlStmt

    If Not FillDatset(AdaptWIN, TblWIN) Then
      Exit Sub
    End If
    TblWIN.AcceptChanges()
    If TblWIN.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_MESSG_IHRM")
      Exit Sub
    End If
    lstWIN.DataSource = ViewWIN
    lstWIN.DisplayMember = "IHRM_BEZ"
    lstWIN.ValueMember = "IHRM_ID"
    If MnMessgID > -1 Then
      ViewWIN.RowFilter = "MESSG_ID=" & MnMessgID
    End If
    ViewWIN.Sort = "POS_ID"

    '
    '
    'User/Methoden spezifische Winkel und Messgeometrien
    '
    '
    DataGridCombo = New DataGridViewComboBoxColumn
    DataGridCombo.DisplayStyle = DataGridViewComboBoxDisplayStyle.Nothing
    DataGridCombo.DisplayStyleForCurrentCellOnly = DataGridViewComboBoxDisplayStyle.ComboBox
    For i = 0 To GewWert.Length - 1
      DataGridCombo.Items.Add(GewWert(i))
    Next i
    DatWIA.Columns.Clear()
    DatWIA.Columns.Add("BEZ", Texxt(173))
    DataGridCombo.Name = "GEW"
    DataGridCombo.HeaderText = Texxt(172)
    DatWIA.Columns.Add(DataGridCombo)
    DatWIA.Columns.Add("ID", " ")
    DatWIA.DataSource = ViewWIA
    DatWIA.Columns(0).DataPropertyName = "IHRM_BEZ"
    DatWIA.Columns(1).DataPropertyName = "IHRM_GEW"
    DatWIA.Columns(2).DataPropertyName = "IHRM_ID"
    DatWIA.Columns(2).Visible = False
    DatWIA.AutoGenerateColumns = False
    DatWIA.AllowUserToAddRows = False
    DatWIA.AllowUserToDeleteRows = False
    DatWIA.ScrollBars = ScrollBars.Vertical
    '
    '
    '
    SqlStmt = "SELECT TBL_USER_METH_MESSG_IHRM.USER_ID,TBL_USER_METH_MESSG_IHRM.IHRM_ID,TBL_USER_METH_MESSG_IHRM.MESSG_ID,TBL_USER_METH_MESSG_IHRM.METH_ID,POS_ID,IHRM_BEZ,IHRM_GEW" _
        & " FROM TBL_USER_METH_MESSG_IHRM,TBL_IHRM WHERE TBL_USER_METH_MESSG_IHRM.IHRM_ID=TBL_IHRM.IHRM_ID" _
        & " ORDER BY MESSG_ID,POS_ID"

    CmdWIA.CommandText = SqlStmt

    If Not FillDatset(AdaptWIA, TblWIA) Then
      Exit Sub
    End If
    TblWIA.AcceptChanges()
    If TblWIA.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_USER_METH_MESSG_IHRM")
      Exit Sub
    End If
    If MnMessgID > -1 And MnMethID > -1 And MnUserid > -1 Then
      ViewWIA.RowFilter = "MESSG_ID=" & MnMessgID & " AND METH_ID=" & MnMethID & " AND USER_ID=" & MnUserid
    End If

    ViewWIA.Sort = "POS_ID"

    '
    '
    '
    '
    '
    '
    'USER

    If Not MnIopenArt Then

      BindingGKW.AddNewItem.Visible = False

      For i = 0 To txtGKW.Count - 1
        txtGKW(i).Enabled = False
      Next i
      BindingUSE.Visible = False
      BindingMET.Visible = False
      BindingMSG.Visible = False
    End If

    '
    If MnUserid = -1 Then
      SqlStmt = "SELECT DISTINCTROW TBL_USER.USER_ID AS USER_ID,USER_NAME FROM TBL_USER INNER JOIN TBL_USER_MESSG ON TBL_USER.USER_ID=TBL_USER_MESSG.USER_ID ORDER BY USER_NAME"

      'SqlStmt = "SELECT * FROM TBL_USER"
    Else
      SqlStmt = "SELECT * FROM TBL_USER WHERE [USER_ID]=" & MnUserid
    End If
    CmdUSE.CommandText = SqlStmt
    If Not FillDatset(AdaptUSE, TblUSE) Then
      Exit Sub
    End If
    TblUSE.AcceptChanges()
    If TblUSE.Rows.Count = 0 Then
      MsgBox(Texxt(2993))
      Exit Sub
    Else
      StrLinUse = StrLin(TblUSE, "USER_ID")
    End If
    '
    '
    '
    ''
    '
    '
    '
    'Messgeräte
    '
    '
    '
    If MnMessgID = -1 Then
      SqlStmt = "SELECT TBL_USER_MESSG.USER_ID,TBL_USER_MESSG.MESSG_ID,MESSG_KBEZ,MESSG_LABOR_ID," _
      & "MESSG_TOP,MESSG_TDIFF,MESSG_TWAIT,MESSG_IMES,MESSG_IKAL,MESSG_STELL,MESSG_DE,TBL_USER_MESSG.MESSG_SOND," _
      & "MESSG_TBL,MESSG_REFTRA,MESSG_NORM_FILE_ID,USER_NAME,MESSG_LABOR_ID,TBL_USER_MESSG.MESSG_INI,TBL_USER_MESSG.MESSG_KAL" _
      & " FROM TBL_USER_MESSG,TBL_MESSG,TBL_USER WHERE TBL_MESSG.MESSG_ID=TBL_USER_MESSG.MESSG_ID" _
      & " AND TBL_USER_MESSG.USER_ID=TBL_USER.USER_ID AND TBL_USER_MESSG.USER_ID IN" & StrLinUse _
      & " ORDER BY TBL_USER_MESSG.MESSG_ID"
    Else
      SqlStmt = "SELECT TBL_USER_MESSG.USER_ID,TBL_USER_MESSG.MESSG_ID,MESSG_KBEZ,MESSG_LABOR_ID," _
      & "MESSG_TOP,MESSG_TDIFF,MESSG_TWAIT,MESSG_IMES,MESSG_IKAL,MESSG_STELL,MESSG_DE,TBL_USER_MESSG.MESSG_SOND," _
      & "MESSG_TBL,MESSG_REFTRA,MESSG_NORM_FILE_ID,USER_NAME,MESSG_LABOR_ID,TBL_USER_MESSG.MESSG_INI,TBL_USER_MESSG.MESSG_KAL" _
      & " FROM TBL_USER_MESSG,TBL_MESSG,TBL_USER WHERE TBL_MESSG.MESSG_ID=TBL_USER_MESSG.MESSG_ID" _
      & " AND TBL_USER_MESSG.USER_ID=TBL_USER.USER_ID AND TBL_USER_MESSG.USER_ID IN" & StrLinUse _
      & " AND TBL_USER_MESSG.MESSG_ID=" & MnMessgID _
      & " ORDER BY TBL_USER_MESSG.MESSG_ID"
    End If
    CmdMSG.CommandText = SqlStmt
    If Not FillDatset(AdaptMSG, TblMSG) Then
      Exit Sub
    End If
    StrLinMsg = ""
    TblMSG.AcceptChanges()
    If TblMSG.Rows.Count = 0 Then
      MsgBox(Texxt(2989), 0, Texxt(2000))
      Me.Close()
      Exit Sub
    End If
    StrLinMsg = StrLin(TblMSG, "MESSG_ID")
    '
    '
    'Prüfen, ob MESSG_LABOR_ID in TBL_LABOR vorhanden
    '
    '
    For j = 0 To TblMSG.Rows.Count - 1
      For i = 0 To TblLAB.Rows.Count - 1
        If TblMSG.Rows(j)("MESSG_LABOR_ID") = TblLAB.Rows(i)("LABOR_ID") Then
          Exit For
        End If
      Next i
      If i = TblLAB.Rows.Count Then
        TblLAB.Rows.Add(TblLAB.NewRow)
        k = TblLAB.Rows.Count - 1
        TblLAB.Rows(k)("LABOR_ID") = TblMSG.Rows(j)("MESSG_LABOR_ID")
        TblLAB.Rows(k)("LABOR_KBEZ") = "LABOR" & CStr(TblMSG.Rows(j)("MESSG_LABOR_ID"))
        TblLAB.Rows(k)("LABOR_LBEZ") = TblLAB.Rows(k)("LABOR_KBEZ")
        TblLAB.Rows(k)("LABOR_ABTC") = " "
        TblLAB.Rows(k)("LABOR_BAU") = " "
        TblLAB.Rows(k)("LABOR_RAUM") = " "
        TblLAB.Rows(k)("LABOR_KST") = 0
        TblLAB.Rows(k)("LABOR_TEXT1") = " "
        TblLAB.Rows(k)("LABOR_TEXT1") = " "
        TblLAB.Rows(k)("LABOR_TEXT1") = " "

      End If
    Next j
    '

    'Gruppen
    '


    SqlStmt = "SELECT * FROM TBL_MESSG_GROUP"

    CmdGRP.CommandText = SqlStmt
    TblGRP.Clear()
    If Not FillDatset(AdaptGRP, TblGRP) Then
      Exit Sub
    End If
    TblGRP.AcceptChanges()
    ViewGRP = New DataView(TblGRP)

    cboGRP.DataSource = ViewGRP
    cboGRP.DisplayMember = "GROUP_KBEZ"
    cboGRP.ValueMember = "GROUP_ID"
    '
    '
    '
    'Userabhängige Methode auswählen
    '
    If MnMethID = -1 Then
      SqlStmt = "SELECT USER_ID,METH_BEZ,TBL_USER_METH.METH_ID AS METH_ID FROM TBL_USER_METH,TBL_METH WHERE [USER_ID] IN" & StrLinUse & _
      " AND TBL_USER_METH.METH_ID=TBL_METH.METH_ID ORDER BY TBL_USER_METH.METH_ID"
    Else
      SqlStmt = "SELECT USER_ID,METH_BEZ,TBL_USER_METH.METH_ID AS METH_ID FROM TBL_USER_METH,TBL_METH WHERE [USER_ID] IN" & StrLinUse & _
      " AND TBL_USER_METH.METH_ID=TBL_METH.METH_ID AND TBL_USER_METH.METH_ID=" & MnMethID & _
      " ORDER BY TBL_USER_METH.METH_ID"
    End If
    CmdMET.CommandText = SqlStmt
    If Not FillDatset(AdaptMET, TblMET) Then
      Exit Sub
    End If
    Call UpdateLangText(TblMET, 1800, "METH_ID", "", "METH_BEZ")

    TblMET.AcceptChanges()
    StrLinMet = ""
    If TblMET.Rows.Count = 0 Then
      MsgBox(Texxt(3600), 0, Texxt(2000))
      Exit Sub
    Else
      StrLinMet = StrLin(TblMET, "METH_ID")
    End If
    '
    '
    '
    '
    CmdHLF.Connection = Cncol()
    CmdSET.Connection = Cncol()
    Cncol.Close()
    Cncol.Open()
    For k = 0 To TblUSE.Rows.Count - 1
      ViewMSG.RowFilter = "USER_ID=" & TblUSE.Rows(k)("User_id")
      ViewMET.RowFilter = "USER_ID=" & TblUSE.Rows(k)("User_id")
      For i = 0 To ViewMSG.Count - 1
        For j = 0 To ViewMET.Count - 1
          SqlStmt = "SELECT * FROM TBL_USER_METH_MESSG WHERE USER_ID=" & TblUSE.Rows(k)("User_id") _
          & " AND METH_ID=" & ViewMET(j)("METH_ID") & " AND MESSG_ID=" & ViewMSG(i)("MESSG_ID")
          CmdHLF.CommandText = SqlStmt
          DatHLF = DataReader(CmdHLF, CommandBehavior.SingleResult)
          If Not DatHLF.Read Then
            SqlEtmt = "INSERT INTO TBL_USER_METH_MESSG " _
             & " (USER_ID,METH_ID,MESSG_ID,USER_REF_GID,MESSG_KA,GKWRT_ID,MESSG_RETR)" _
             & " VALUES(" & TblUSE.Rows(k)("User_id") & "," & ViewMET(j)("METH_ID") & "," & ViewMSG(i)("MESSG_ID") _
             & ",0,0,0,0)"
            CmdSET.CommandText = SqlEtmt
            If SQLExeNonQuery(CmdSET, Cncol) Then
              Exit Sub
            End If
          End If
          DatHLF.Close()
        Next j
      Next i
    Next k
    Cncol.Close()

    '
    '
    '
    '


    'Aufbau TblMTG für TBL_USER_METH_MESSG 



    '
    SqlStmt = "SELECT * FROM TBL_USER_METH_MESSG WHERE USER_ID IN" & StrLinUse _
    & " AND METH_ID IN" & StrLinMet & " AND MESSG_ID IN" & StrLinMsg
    CmdMTG.CommandText = SqlStmt
    If Not FillDatset(AdaptMTG, TblMTG) Then

    End If
    TblMTG.AcceptChanges()
    '
    '
    '
    '
    ViewMSG.RowFilter = "USER_ID=-1"
    '
    '
    ConnGKW.DataSource = TblGKW
    ConnMTG.DataSource = ViewMTG
    ConnMET.DataSource = ViewMET
    ConnMSG.DataSource = ViewMSG
    ConnUSE.DataSource = ViewUse
    '
    cboUSE.DataSource = ConnUSE
    cboUSE.DisplayMember = "USER_NAME"
    cboUSE.ValueMember = "USER_ID"
    '
    cboMSG.DataSource = ConnMSG
    cboMSG.DisplayMember = "MESSG_KBEZ"
    cboMSG.ValueMember = "MESSG_ID"
    '
    cboMET.DataSource = ConnMET
    cboMET.DisplayMember = "METH_BEZ"
    cboMET.ValueMember = "METH_ID"
    '
    lblUSE.DataBindings.Add("TEXT", ConnUSE, "USER_NAME")
    lblMSG.DataBindings.Add("TEXT", ConnMSG, "MESSG_KBEZ")
    lblMET.DataBindings.Add("TEXT", ConnMET, "METH_BEZ")
    txtZUS_0.DataBindings.Add("TEXT", ConnMSG, "MESSG_TOP")
    txtZUS_1.DataBindings.Add("TEXT", ConnMSG, "MESSG_TDIFF")
    txtZUS_2.DataBindings.Add("TEXT", ConnMSG, "MESSG_TWAIT")
    txtZUS_3.DataBindings.Add("TEXT", ConnMSG, "MESSG_STELL")
    txtZUS_4.DataBindings.Add("TEXT", ConnMSG, "MESSG_IMES")
    txtZUS_5.DataBindings.Add("TEXT", ConnMSG, "MESSG_IKAL")
    txtZUS_6.DataBindings.Add("TEXT", ConnMSG, "MESSG_DE")
    txtZUS_7.DataBindings.Add("TEXT", ConnMSG, "MESSG_LABOR_ID")
    txtZUS_8.DataBindings.Add("TEXT", ConnMSG, "MESSG_INI")
    txtZUS_9.DataBindings.Add("TEXT", ConnMSG, "MESSG_KAL")
    '
    '
    '
    '
    txtGKW_00.DataBindings.Add("TEXT", ConnGKW, "GKWRT_ID")
    txtGKW_14.DataBindings.Add("TEXT", ConnGKW, "GKWRT_BEZ")
    txtGKW_14.MaxLength = TblGKW.Columns("GKWRT_BEZ").MaxLength
    txtGKW_13.DataBindings.Add("TEXT", ConnGKW, "CDE")
    txtGKW_01.DataBindings.Add("TEXT", ConnGKW, "GK01")
    txtGKW_02.DataBindings.Add("TEXT", ConnGKW, "GK02")
    txtGKW_03.DataBindings.Add("TEXT", ConnGKW, "GK03")
    txtGKW_04.DataBindings.Add("TEXT", ConnGKW, "GK04")
    txtGKW_05.DataBindings.Add("TEXT", ConnGKW, "GK05")
    txtGKW_06.DataBindings.Add("TEXT", ConnGKW, "GK06")
    txtGKW_07.DataBindings.Add("TEXT", ConnGKW, "GK07")
    txtGKW_08.DataBindings.Add("TEXT", ConnGKW, "GK08")
    txtGKW_09.DataBindings.Add("TEXT", ConnGKW, "GK09")
    txtGKW_10.DataBindings.Add("TEXT", ConnGKW, "GK10")
    txtGKW_11.DataBindings.Add("TEXT", ConnGKW, "GK11")
    txtGKW_12.DataBindings.Add("TEXT", ConnGKW, "GK12")
    '
    cboGRP.DataBindings.Add("SELECTEDVALUE", ConnMTG, "USER_REF_GID")
    cboSTD_0.DataBindings.Add("SELECTEDINDEX", ConnMSG, "MESSG_INI")
    cboSTD_1.DataBindings.Add("SELECTEDINDEX", ConnMSG, "MESSG_KAL")
    cboMIT.DataBindings.Add("SELECTEDINDEX", ConnMTG, "MESSG_KA")
    cboLAB.DataBindings.Add("SELECTEDVALUE", ConnMSG, "MESSG_LABOR_ID")
    '
    ''
    If BitWrt(28, MenueParam.User.Drum) Or MnIopenArt Then
      chkGKExtra.Visible = True
    End If
  End Sub

  Private Sub frmNWI_Paint(sender As Object, e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint
    TabNWI.ItemSize = New Size(0.5 * TabNWI.Width - 4, TabNWI.ItemSize.Height)
  End Sub
 
  Private Sub ConnUSE_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnUSE.CurrentChanged
    '
    If ConnUSE.Count = 0 Then Exit Sub
    BindingUSE.Enabled = False
    '
    '
    '
    '
    '
    'Userabhängige Messgeräte auswählen
    '

    If Not IsNothing(ConnMSG.Current) Then
      ConnMSG.Current.endedit()
    End If
    ViewMSG.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ConnMSG.CurrencyManager.EndCurrentEdit()

    '
    If ViewMSG.Count = 0 Then
      imsg = MsgBox(Texxt(2989), 0, Texxt(2000))
    End If

    BindingUSE.Enabled = True

  End Sub

  Private Sub ConnMSG_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMSG.CurrentChanged
    If ConnUSE.Current Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    If ConnMSG.Current Is Nothing OrElse ConnMSG.Count = 0 Then Exit Sub
    '
    '
    BindingMSG.Enabled = False
    '
    'Userabhängige Methode auswählen
    '
    If Not IsNothing(ConnMET.Current) Then
      ConnMET.Current.endedit()
    End If
    ViewMET.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ConnMET.CurrencyManager.Refresh()
    ConnMET.Position = 0
    If ViewMET.Count = 0 Then
      MsgBox(Texxt(3600), 0, Texxt(2000))
    End If
    If ConnMSG.Current("Messg_RefTra") = "A" Then
      cboRETR.Visible = True
      lblRETR.Visible = True
    Else
      cboRETR.Visible = False
      lblRETR.Visible = False
    End If

    BindingMSG.Enabled = True



  End Sub

  Private Sub ConnMET_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMET.CurrentChanged
    '
    If ConnUSE.Current Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    If ConnMSG.Current Is Nothing OrElse ConnMSG.Count = 0 Then Exit Sub
    If ViewMSG.Count = 0 Then Exit Sub
    If ViewMET.Count = 0 Then Exit Sub
    If ConnMET.Current("Meth_id") > 49 And ConnMET.Current("Meth_id") <= 100 Then
      TabNWI.TabPages(1).Enabled = False
      TabNWI.SelectTab(0)
    Else
      TabNWI.TabPages(1).Enabled = True
    End If
    BindingMET.Enabled = False
    '
    '
    'Aufbau ViewMTG für TBL_USER_METH_MESSG 
    '
    '
    If Not IsNothing(ConnMTG.Current) Then
      ConnMTG.Current.endedit()
    End If
    ViewMTG.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID") & " AND METH_ID=" & ConnMET.Current("METH_ID") & " AND MESSG_ID=" & ConnMSG.Current("MESSG_ID")
    If ViewMTG.Count = 0 Then Exit Sub
    'ConnMTG.Position = 0


    GkaltID = ConnMTG.Current("gkwrt_id")



    BindingMET.Enabled = True
  End Sub

  Private Sub ConnMTG_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMTG.CurrentChanged
    Dim ier As Integer
    Dim i As Integer
    Dim RefTra As String
    If ConnMSG.Current Is Nothing OrElse ConnMSG.Count = 0 Then Exit Sub
    If ConnMTG.Current Is Nothing OrElse ConnMTG.Count = 0 Then Exit Sub
    RefTra = ConnMSG.Current("Messg_RefTra")
    '

    ViewGRP.RowFilter = "MESSG_ID=" & ConnMSG.Current("messg_id")
    If ViewGRP.Count = 0 Then
      MsgBox(Texxt(1664) & Space(1) & Texxt(420) & Space(1) & ConnMSG.Current("MESSG_KBEZ"))
      Me.Close()
    End If
    If IsDBNull(ConnMTG.Current("MESSG_RETR")) Then
      ConnMTG.Current("MESSG_RETR") = 0
    End If
    If IsDBNull(ConnMTG.Current("Messg_ka")) Or IsDBNull(ConnMTG.Current("user_ref_gid")) Or IsDBNull(ConnMTG.Current("gkwrt_id")) Then
      Call UpdMesMet(0, 0, 0, 0, ReflTra(RefTra))
      cboMIT.SelectedIndex = 0
      cboRETR.SelectedIndex = ReflTra(RefTra)
      cboGRP.SelectedIndex = 0
    Else
      cboMIT.SelectedIndex = ConnMTG.Current("Messg_ka")
      If RefTra = "A" Then
        cboRETR.SelectedIndex = ConnMTG.Current("MESSG_RETR")
      Else
        cboRETR.SelectedIndex = ReflTra(RefTra)
      End If
      'cboGRP.SelectedValue = ConnMTG.Current("user_ref_gid")
      ConnGKW.Position = ConnGKW.Find("GKWRT_ID", ConnMTG.Current("gkwrt_id"))




      If ConnMTG.Current("Meth_id") > 49 And ConnMTG.Current("Meth_id") <= 100 Then
        TabNWI.TabPages(1).Enabled = False
        TabNWI.SelectTab(0)
      Else
        TabNWI.TabPages(1).Enabled = True
      End If
    End If

    ViewWIN.RowFilter = "MESSG_ID=" & ConnMTG.Current("MESSG_ID")

    ViewWIA.RowFilter = "MESSG_ID=" & ConnMTG.Current("MESSG_ID") & " AND METH_ID=" & ConnMTG.Current("METH_ID") & " AND USER_ID=" & ConnMTG.Current("USER_ID")


    MessgNormFileID = ConnMSG.Current("messg_norm_file_ID")
    Call GETNwe(MessgNormFileID, MessgNwe, MessgNwp, WellStart, WellEnde, WellStep, ier)

    Call GETWsol(MessgNormFileID, WellStart, WellEnde, WellStep, Whilf, ier)

    If ier <> 0 Then
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    lstNWI.Items.Clear()
    For i = 0 To MessgNwe - 1
      lstNWI.Items.Add(Whilf(i))
    Next i
    '
    ''
  End Sub

  Sub UpdMesMet(ByVal GkwrtID As Integer, ByVal GkaltID As Integer, ByVal MessgKA As Integer, ByVal GroupID As Integer, ByVal Retr As Integer)
    Dim GKWrt As Long
    GKWrt = GkwrtID
    If GkwrtID <> GkaltID Then
      imsg = MsgBox(Texxt(2920), 4, Texxt(2000))
      If imsg = 7 Then
        GKWrt = GkaltID
      End If
    End If
    '
    ConnMTG.Current("GKWRT_ID") = GKWrt
    ConnMTG.Current("MESSG_KA") = MessgKA
    ConnMTG.Current("USER_REF_GID") = GroupID
    ConnMTG.Current("MESSG_RETR") = Retr

    ''
  End Sub

  

  WriteOnly Property UserID() As Integer
    Set(ByVal value As Integer)
      MnUserid = value
    End Set
  End Property
  WriteOnly Property MessgID() As Integer
    Set(ByVal value As Integer)
      MnMessgID = value
    End Set
  End Property

  WriteOnly Property MethID() As Integer
    Set(ByVal value As Integer)
      MnMethID = value
    End Set
  End Property
  WriteOnly Property MischID() As Integer
    Set(ByVal value As Integer)
      MnMischid = value
    End Set
  End Property

  WriteOnly Property Aform() As Form
    Set(ByVal value As Form)
      MnAform = value
    End Set
  End Property


 

  Private Sub ConnGKW_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnGKW.AddingNew
    Dim i As Integer
    '
    'Defaultstartwerte setzen
    '
    '
    For i = 0 To TblGKW.Columns.Count - 1
      TblGKW.Columns(i).DefaultValue = ConnGKW.Current(i)
    Next i
    MaxGkwID = MaxDatTableID(TblGKW, "GKWRT_ID", {""}, {-1}) + 1
    TblGKW.Columns("GKWRT_ID").DefaultValue = MaxGkwID
    '
    TblGKW.Columns("GKWRT_BEZ").DefaultValue = TblGKW.Columns("GKWRT_BEZ").DefaultValue & " ????"
    If TblGKW.Columns("GKWRT_BEZ").DefaultValue.length > TblGKW.Columns("GKWRT_BEZ").MaxLength Then
      TblGKW.Columns("GKWRT_BEZ").DefaultValue = TblGKW.Columns("GKWRT_BEZ").DefaultValue.substring(0, TblGKW.Columns("GKWRT_BEZ").MaxLength)
    End If
    '
    '
    'TBLEXT umspeichern
    '
    '
    Call SaveGKExtra(0, MaxGkwID, TblEXT, ViewEXT)
    '
    '
    ''
  End Sub

  Private Sub ConnGKW_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnGKW.CurrentChanged


    Dim i As Integer
    '
    '
    '
    If ConnGKW.Current Is Nothing OrElse ConnGKW.Count = 0 Then Exit Sub

    '
    For i = 0 To ViewEXT.Count - 1
      If ViewEXT(i).IsNew Then
        Try
          flgGKExtra.EndEdit()
          ViewEXT(i).EndEdit()
        Catch ex As Exception
          MsgBox(Texxt(516))
          ViewEXT(i).CancelEdit()
          Exit For
        End Try
      End If
    Next
    '
    '
    '
    '
    '
    '
    If Not IsNothing(ViewEXT) Then
      ViewEXT.RowFilter = "GKWRT_ID=" & ConnGKW.Current("GKWRT_ID")
    End If
    '

    For i = 0 To cboRART.Items.Count - 1
      If DirectCast(cboRART.Items(i), ListTextID).ID = Asc(Mid(ConnGKW.Current("CDE"), 1, 1)) Then
        cboRART.SelectedIndex = i
        Exit For
      End If
    Next i
    

    If i >= cboRART.Items.Count Then
      cboRART.SelectedIndex = 0
      txtGKW(13).Text = ConnGKW.Current("CDE")
    End If
    If ConnGKW.Current("gkwrt_id") = 0 Or MnUserUfo Then
      For i = 0 To txtGKW.Count - 1
        txtGKW(i).Enabled = True
      Next i
      txtGKW(13).Enabled = False
      txtGKW(0).Enabled = False
      BindingGKW.AddNewItem.Visible = True
      cboRART.Enabled = True
      flgGKExtra.ReadOnly = False
      flgGKExtra.AllowUserToAddRows = True
      flgGKExtra.AllowUserToDeleteRows = True

    Else
      For i = 0 To txtGKW.Count - 1
        txtGKW(i).Enabled = False
      Next i
      BindingGKW.AddNewItem.Visible = False
      BindingGKW.DeleteItem.Visible = False

      cboRART.Enabled = False

      If MnIopenArt Then
        txtGKW(14).Enabled = True
      End If
      flgGKExtra.ReadOnly = True
      flgGKExtra.AllowUserToAddRows = False
      flgGKExtra.AllowUserToDeleteRows = False

    End If
   
    If Not ConnMTG.Current Is Nothing AndAlso ConnMTG.Count <> 0 Then
      ConnMTG.Current("GKWRT_ID") = ConnGKW.Current("GKWRT_ID")
    End If

   

   
  End Sub


  Function ReflTra(ByVal RefTra As String) As Integer
    ReflTra = 0
    If RefTra = "T" Then
      ReflTra = 1
    End If
  End Function

  
  Private Sub cboSTD_DropDownClosed(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboSTD_0.DropDownClosed, cboSTD_1.DropDownClosed
    Dim index As Integer
    index = CInt(sender.name.substring(7, 1))
    'txtZUS(8 + index).Text = DirectCast(cboSTD(index).SelectedItem, ListTextID).ID
  End Sub



  Private Sub lstWIN_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstWIN.Click
    Dim i As Integer
    Dim RowView As DataRowView
    If lstWIN.SelectedIndex < 0 Then Exit Sub
    If ViewWIA.Count >= 8 Then
      MessageBox.Show(Texxt(3999), Texxt(2000), MessageBoxButtons.OK)
      Exit Sub
    End If
    For i = 0 To ViewWIA.Count - 1
      If lstWIN.SelectedValue = ViewWIA(i)("IHRM_ID") Then
        Exit Sub
      End If
    Next
    '
    '
    '
    RowView = ViewWIA.AddNew()
    RowView("USER_ID") = ConnUSE.Current("USER_ID")
    RowView("METH_ID") = ConnMET.Current("METH_ID")
    RowView("MESSG_ID") = ConnMSG.Current("MESSG_ID")
    RowView("IHRM_ID") = lstWIN.SelectedValue
    RowView("IHRM_GEW") = 1.0
    RowView("IHRM_BEZ") = lstWIN.SelectedItem("IHRM_BEZ")
    RowView("POS_ID") = ViewWIA.Count - 1
    RowView.EndEdit()
  End Sub

  Private Sub DatWIA_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles DatWIA.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call DatWIA_KeyDown(sender, ev)
  End Sub
  Private Sub DatWIA_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles DatWIA.KeyDown
    Dim i As Integer
    If DatWIA.RowCount <= 1 Then Exit Sub
    For i = DatWIA.CurrentCell.RowIndex + 1 To DatWIA.RowCount - 1
      ViewWIA(i)("POS_ID") = ViewWIA(i)("POS_ID") - 1
    Next
    ViewWIA.Delete(DatWIA.CurrentCell.RowIndex)
  End Sub

  Private Sub Form_Initialize()
    MnIopenArt = True
    MnUserid = -1
    MnMessgID = -1
    MnMethID = -1
    MnMischid = -1
    WellStart = 0
    WellEnde = 0
    WellStep = 0
  End Sub


  Private Sub TabNWI_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TabNWI.SelectedIndexChanged
    If TabNWI.SelectedIndex = 1 And Not TabNWI.TabPages(1).Enabled Then
      TabNWI.SelectTab(0)
      TabNWI.SelectedIndex = 0
    End If
    flgGKExtra.Visible = chkGKExtra.Checked
  End Sub

  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    Dim WhereKeyID() As String
    If AddDelP(2990) Then
      '

      ConnUSE.CurrencyManager.EndCurrentEdit()
      ConnMTG.CurrencyManager.EndCurrentEdit()
      ConnMSG.CurrencyManager.EndCurrentEdit()
      ConnMET.CurrencyManager.EndCurrentEdit()
      ConnGKW.CurrencyManager.EndCurrentEdit()
      flgGKExtra.EndEdit()
      ViewEXT.EndInit()


      'Fehlende Labors hinzufügen
      'TBL_LABOR
      '
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptLAB.InsertCommand = OleDBInsertCmd("TBL_LABOR", Cncol)
      AdaptLAB.Update(TblLAB.Select(Nothing, Nothing, DataViewRowState.Added)) '

      '
      '
      '
      'TBL_USER_METH_MESSG_IHRM
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptWIA.InsertCommand = OleDBInsertCmd("TBL_USER_METH_MESSG_IHRM", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(3)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "MESSG_ID"
      WhereKeyID(3) = "IHRM_ID"
      AdaptWIA.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_MESSG_IHRM", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptWIA.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_MESSG_IHRM", WhereKeyID, Cncol)
      '
      '
      '
      '
      '
      '
      'TBL_GKWRT
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptGKW.InsertCommand = OleDBInsertCmd("TBL_GKWRT", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "GKWRT_ID"
      AdaptGKW.UpdateCommand = OleDBUpdateCmd("TBL_GKWRT", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptGKW.DeleteCommand = OleDBDeleteCmd("TBL_GKWRT", WhereKeyID, Cncol)
      '
      '
      '
      '
      '
      'TBL_GKWRTEXT
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptEXT.InsertCommand = OleDBInsertCmd("TBL_GKWRTEXT", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "GKWRT_ID"
      WhereKeyID(1) = "IHRM_ID"
      AdaptEXT.UpdateCommand = OleDBUpdateCmd("TBL_GKWRTEXT", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptEXT.DeleteCommand = OleDBDeleteCmd("TBL_GKWRTEXT", WhereKeyID, Cncol)
      ''
      '

      '
      '
      'TBL_USER_METH_MESSG
      '
      '
      '
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptMTG.InsertCommand = OleDBInsertCmd("TBL_USER_METH_MESSG", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "MESSG_ID"
      AdaptMTG.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_MESSG", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptMTG.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_MESSG", WhereKeyID, Cncol)
      '
      '
      '
      'TBL_USER_MESSG
      '
      '
      '
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptMSG.InsertCommand = OleDBInsertCmd("TBL_USER_MESSG", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MESSG_ID"
      AdaptMSG.UpdateCommand = OleDBUpdateCmd("TBL_USER_MESSG", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptMSG.DeleteCommand = OleDBDeleteCmd("TBL_USER_MESSG", WhereKeyID, Cncol) '
      'Delete/Update/Insert TBL_GKWRT und TBL_USER_METH_MESSG
      '
      'Delete TBL_USER_METH_MESSG_IHRM
      '
      AdaptWIA.Update(TblWIA.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Delete TBL_GKWRTEXT
      '
      AdaptEXT.Update(TblEXT.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      '
      'Delete TBL_GKWRT
      '
      AdaptGKW.Update(TblGKW.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      'Delete TBL_USER_METH_MESSG
      '
      AdaptMTG.Update(TblMTG.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Delete TBL_USER_MESSG
      '
      AdaptMSG.Update(TblMSG.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Insert TBL_USER_MESSG
      '
      '
      AdaptMSG.Update(TblMSG.Select(Nothing, Nothing, DataViewRowState.Added)) '
      ''
      'Insert TBL_USER_METH_MESSG
      '
      '
      AdaptMTG.Update(TblMTG.Select(Nothing, Nothing, DataViewRowState.Added)) '
      '
      '
      'Insert TBL_USER_METH_MESSG_IHRM
      '
      '
      AdaptWIA.Update(TblWIA.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Insert TBL_GKWRT
      '
      '
      AdaptGKW.Update(TblGKW.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      '
      'Insert TBL_GKWRTEXT
      '
      '
      AdaptEXT.Update(TblEXT.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      '
      '
      'Update TBL_USER_METH_MESSG_IHRM
      '
      AdaptWIA.Update(TblWIA.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_GKWRT
      '
      AdaptGKW.Update(TblGKW.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_GKWRTEXT
      '
      AdaptEXT.Update(TblEXT.Select(Nothing, "IHRM_ID", DataViewRowState.ModifiedCurrent))
      '
      '
      '
      'Update TBL_USER_METH_MESSG
      '
      AdaptMTG.Update(TblMTG.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_USER_MESSG
      '
      AdaptMSG.Update(TblMSG.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      ''
      '
      '
      '
     
      '
      '
     
    
      '
      '
      '
      '
    End If
    Me.Close()
    If ConnGKW.Current("GKWRT_ID") = 0 And (MenueParam.MethID < 50 Or MenueParam.MethID >= 100) Then
      MsgBox(Texxt(512), MsgBoxStyle.OkOnly, Texxt(2010))
    End If
    Me.Dispose()
  End Sub

  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
    MnIopenArt = True
    MnUserid = -1
    MnMessgID = -1
    MnMethID = -1
    MnMischid = -1
    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.

  End Sub


  Private Sub cboLAB_DropDownClosed(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboLAB.DropDownClosed
    'txtZUS_7.Text = cboLAB.SelectedValue
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

  Private Sub cboGRP_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboGRP.SelectedValueChanged
    If IsNothing(ConnMTG.Current) Then Exit Sub
    If Not IsNumeric(cboGRP.SelectedValue) Then Exit Sub
    ' MsgBox(ConnMTG.Current("user_ref_gid") & "  " & ConnMTG.Current("MESSG_KA"))
    ConnMTG.Current("user_ref_gid") = cboGRP.SelectedValue
  End Sub

  Private Sub cboMIT_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboMIT.SelectedIndexChanged
    If IsNothing(ConnMTG.Current) Then Exit Sub
    ConnMTG.Current("MESSG_KA") = cboMIT.SelectedIndex
  End Sub

  Private Sub cboLAB_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboLAB.SelectedValueChanged
    If IsNothing(ConnMSG.Current) Then Exit Sub
    ConnMSG.Current("MESSG_LABOR_ID") = cboLAB.SelectedValue
  End Sub
  Private Sub cboSTD_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboSTD_0.SelectedIndexChanged, cboSTD_1.SelectedIndexChanged
    Dim index As Integer
    index = CInt(sender.name.substring(7, 1))
    Select Case index
      Case 0
        ConnMSG.Current("MESSG_INI") = cboSTD(index).SelectedIndex
      Case 1
        ConnMSG.Current("MESSG_KAL") = cboSTD(index).SelectedIndex
    End Select
  End Sub
  Private Sub chkGKExtra_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkGKExtra.CheckedChanged
    flgGKExtra.Visible = chkGKExtra.Checked
  End Sub
  Private Sub flgGKExtra_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles flgGKExtra.DataError
    e.Cancel = False
    MsgBox(e.Exception.Message)
  End Sub
  Private Sub btnGKSpei0_Click(sender As Object, e As System.EventArgs) Handles btnGKSpei0.Click
    Call SaveGK(ConnGKW.Current("GKWRT_ID"), 0, TblGKW)
    Call SaveGKExtra(ConnGKW.Current("GKWRT_ID"), 0, TblEXT, ViewEXT)
  End Sub


   
End Class