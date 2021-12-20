Option Compare Text
Option Explicit On
Option Strict Off
Imports System.ComponentModel
Public Class frmNWU






  Dim GkaltID As Integer
  Dim wert As Single
  Dim MaxGkwID As Integer
  Dim MaxRzpID As Integer
  'Anzahl Reflexionskurven (+Zusatzinformation) QU-Kontrolle
  '
  Dim MnIopenArt As Boolean
  Dim MnUserUfo As Boolean
  Dim ier As Integer
  Dim Count As Integer
  Dim Iprn As Integer
  Dim inde As Integer
  Dim Ichf As Integer
  Dim SqlStmt As String
  Dim SqlEtmt As String
  Dim IcbArt As Integer
  Dim AnzIart As Integer = 9
  Dim NPS As Integer
  Dim Koppwrt As Single
  Dim Start(5) As Object
  Dim DaWrt(5) As Object
  Dim Kopp(5) As String

  Dim menuID As Integer            'Menü ID
  Dim RezmnID As Integer           'ID für Menü für Rezeptrechnung
  Dim GkwrtID As Integer         'ID für GK_Werte
  Dim GkSperr As Integer        'Kennung, ob Gk-Werte zur Berechnung von Grunddaten verwendet wurden
  Dim lchgID As Integer            'ID Gewichte DL,DC und DH
  Dim matpaID As Integer          'ID für mathematische Parameter

  '
  Dim MnUserid As Integer
  Dim MnMessgID As Integer
  Dim MnMessgRwID As Integer
  Dim MnMethID As Integer
  Dim MnMischid As Integer
  Dim MnAform As Form
  Dim MnTblMisch As String
  'Dim Dyset As Recordset
  'Dim DnSet As Recordset
  '
  '
  Dim RowsGRD() As DataRow
  '
  '
  '
  '
  Dim StrLinUse As String
  Dim StrLinMSY As String
  Dim StrLinMet As String

  '
  Dim txtGKW As List(Of TextBox)
  Dim lblGKW As List(Of Label)
  Dim txtRZP As List(Of TextBox)
  Dim lblREZ As List(Of Label)
  Dim cboGKW As List(Of ComboBox)
  Dim cboWIN As List(Of ComboBox)
  Dim txtUSCH As List(Of TextBox)
  '
  Dim DatHLF As OleDbDataReader
  Dim DatSET As OleDbDataReader

  '
  Dim CmdUSE As OleDbCommand
  Dim CmdMSY As OleDbCommand
  Dim CmdMSG As OleDbCommand
  Dim CmdMET As OleDbCommand
  Dim CmdGRP As OleDbCommand
  Dim CmdRZP As OleDbCommand
  Dim CmdGKW As OleDbCommand
  Dim CmdHLF As OleDbCommand
  Dim CmdSET As OleDbCommand
  Dim CmdGRD As OleDbCommand
  Dim CmdEXT As OleDbCommand

  '
  '
  '
  Dim AdaptUSE As OleDbDataAdapter
  Dim AdaptMSY As OleDbDataAdapter
  Dim AdaptMSG As OleDbDataAdapter
  Dim AdaptMET As OleDbDataAdapter
  Dim AdaptGRP As OleDbDataAdapter
  Dim AdaptRZP As OleDbDataAdapter
  Dim AdaptGKW As OleDbDataAdapter
  Dim AdaptGRD As OleDbDataAdapter
  Dim AdaptEXT As OleDbDataAdapter
 

  '
  '
  Dim TblUSE As DataTable
  Dim TblMSY As DataTable
  Dim TblMSG As DataTable
  Dim TblMET As DataTable
  Dim TblGRP As DataTable
  Dim TblRZP As DataTable
  Dim TblGKW As DataTable
  Dim TblGRD As DataTable
  Dim TblEXT As DataTable
  
  '
  '
  '
  Dim ViewUse As DataView
  Dim ViewMSY As DataView
  Dim ViewMSG As DataView
  Dim ViewMET As DataView
  Dim ViewGRP As DataView
  Dim ViewEXT As DataView
  
  '
  '
  '
  Dim WithEvents ConnUSE As BindingSource
  Dim WithEvents ConnMSY As BindingSource
  Dim WithEvents ConnMSG As BindingSource
  Dim WithEvents ConnMET As BindingSource
  Dim WithEvents ConnRZP As BindingSource
  Dim WithEvents ConnGKW As BindingSource
  '
  '
  '
  '
  REM Laufvariable
  Dim i As Integer
  Dim l As Integer
  Dim k As Integer
  Dim j As Integer
  Dim kw As Integer
  REM Zwischengroessen
  Dim nkw As Integer
  'Message Kennung
  Dim imsg As Integer

  Private Sub frmNWU_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed

  End Sub
  'Messagebox (Rückgabewert)
  Private Sub frmNWU_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim Icb(0 To 12) As Integer
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Cursor = Cursors.WaitCursor

    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    '
    KopierenToolStripMenuItem.Text = Texxt(3922)
    EinfügenToolStripMenuItem.Text = Texxt(3923)
    Me.Text = Texxt(413)
    btnORD.Text = Texxt(1999)
    lblGRP.Text = Texxt(386)
    lblUSE.Text = Texxt(999)
    lblMET.Text = Texxt(999)
    lblMIS.Text = Texxt(422)
    lblMES.Text = Texxt(420)
    lblMSY.Text = Texxt(999)
    lblMTH.Text = Texxt(421)
    lblNAU.Text = Texxt(201)
    lblOPTDAT.Text = Texxt(263)
    btnGKSpei0.Text = Texxt(517)
    chkGKExtra.Text = Texxt(518)
    TabNWU.TabPages(0).Text = Texxt(421)
    TabNWU.TabPages(1).Text = Texxt(151)

    lblUSCH_0.Text = Texxt(2320)
    lblUSCH_1.Text = Texxt(2321)
    lblREZ_00.Text = Texxt(550)
    lblREZ_01.Text = Texxt(551)
    lblREZ_02.Text = Texxt(552)
    lblREZ_03.Text = Texxt(553)
    lblREZ_04.Text = Texxt(554)
    lblREZ_05.Text = Texxt(555)
    lblREZ_06.Text = Texxt(556)
    lblREZ_07.Text = Texxt(557)
    lblREZ_08.Text = Texxt(558)
    lblREZ_09.Text = Texxt(559)
    lblREZ_10.Text = Texxt(560)
    lblREZ_11.Text = Texxt(561)
    lblREZ_12.Text = Texxt(562)
    lblREZ_13.Text = Texxt(563)
    lblREZ_14.Text = Texxt(564)
    lblREZ_15.Text = Texxt(565)
    lblREZ_16.Text = Texxt(566)
    lblREZ_17.Text = Texxt(567)
    lblREZ_18.Text = Texxt(568)
    lblREZ_19.Text = Texxt(569)
    lblREZ_20.Text = Texxt(570)
    lblREZ_21.Text = Texxt(571)
    lblREZ_22.Text = Texxt(535)
    lblREZ_23.Text = Texxt(536)
    lblREZ_24.Text = Texxt(537)
    lblREZ_25.Text = Texxt(545)
    '
    '
    '
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
    lblGKW_14.Text = Texxt(688)
    lblGKW_15.Text = Texxt(689)
    lblGKW_16.Text = Texxt(690)
    lblGKW_17.Text = Texxt(691)
    '
    '
    '
    txtUSCH = New List(Of TextBox)
    txtGKW = New List(Of TextBox)
    lblGKW = New List(Of Label)
    txtRZP = New List(Of TextBox)
    lblREZ = New List(Of Label)
    cboGKW = New List(Of ComboBox)
    cboWIN = New List(Of ComboBox)
    '
    '
    '
    TblUSE = New DataTable
    TblMSY = New DataTable
    TblMSG = New DataTable
    TblMET = New DataTable
    TblGRP = New DataTable
    TblRZP = New DataTable
    TblGKW = New DataTable
    TblGRD = New DataTable
    TblEXT = New DataTable
    '
    '
    '
    ViewUse = New DataView(TblUSE)
    ViewMSY = New DataView(TblMSY)
    ViewMSG = New DataView(TblMSG)
    ViewMET = New DataView(TblMET)

    '
    '
    CmdUSE = New OleDbCommand("", Cncol)
    CmdMSY = New OleDbCommand("", Cncol)
    CmdMSG = New OleDbCommand("", Cncol)
    CmdMET = New OleDbCommand("", Cncol)
    CmdGRP = New OleDbCommand("", Cncol)
    CmdRZP = New OleDbCommand("", Cncol)
    CmdGKW = New OleDbCommand("", Cncol)
    CmdHLF = New OleDbCommand("", Cncol)
    CmdSET = New OleDbCommand("", Cncol)
    CmdGRD = New OleDbCommand("", Cncol)
    CmdEXT = New OleDbCommand("", Cncol)

    '
    '
    '
    AdaptUSE = New OleDbDataAdapter
    AdaptMSY = New OleDbDataAdapter
    AdaptMSG = New OleDbDataAdapter
    AdaptMET = New OleDbDataAdapter
    AdaptGRP = New OleDbDataAdapter
    AdaptRZP = New OleDbDataAdapter
    AdaptGKW = New OleDbDataAdapter
    AdaptGRD = New OleDbDataAdapter
    AdaptEXT = New OleDbDataAdapter

    '
    AdaptUSE.SelectCommand = CmdUSE
    AdaptMSY.SelectCommand = CmdMSY
    AdaptMSG.SelectCommand = CmdMSG
    AdaptMET.SelectCommand = CmdMET
    AdaptGRP.SelectCommand = CmdGRP
    AdaptRZP.SelectCommand = CmdRZP
    AdaptGKW.SelectCommand = CmdGKW
    AdaptGRD.SelectCommand = CmdGRD
    AdaptEXT.SelectCommand = CmdEXT

    '
    '
    ConnUSE = New BindingSource
    ConnMSY = New BindingSource
    ConnMSG = New BindingSource
    ConnMET = New BindingSource
    ConnRZP = New BindingSource
    ConnGKW = New BindingSource
    '
    '
    '
    '
    '
    '
    '
    BindingUSE.BindingSource = ConnUSE
    BindingMSY.BindingSource = ConnMSY
    BindingMSG.BindingSource = ConnMSG
    BindingMET.BindingSource = ConnMET
    BindingRZP.BindingSource = ConnRZP
    BindingGKW.BindingSource = ConnGKW
    '
    '
    '
    '
    
    '
    '
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
    txtGKW.Add(txtGKW_15)
    txtGKW.Add(txtGKW_16)
    txtGKW.Add(txtGKW_17)
    txtGKW.Add(txtGKW_18)
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
    lblGKW.Add(lblGKW_14)
    lblGKW.Add(lblGKW_15)
    lblGKW.Add(lblGKW_16)
    lblGKW.Add(lblGKW_17)
    '
    '
    '
    txtRZP.Add(txtRZP_00)
    txtRZP.Add(txtRZP_01)
    txtRZP.Add(txtRZP_02)
    txtRZP.Add(txtRZP_03)
    txtRZP.Add(txtRZP_04)
    txtRZP.Add(txtRZP_05)
    txtRZP.Add(txtRZP_06)
    txtRZP.Add(txtRZP_07)
    txtRZP.Add(txtRZP_08)
    txtRZP.Add(txtRZP_09)
    txtRZP.Add(txtRZP_10)
    txtRZP.Add(txtRZP_11)
    txtRZP.Add(txtRZP_12)
    txtRZP.Add(txtRZP_13)
    txtRZP.Add(txtRZP_14)
    txtRZP.Add(txtRZP_15)
    txtRZP.Add(txtRZP_16)
    txtRZP.Add(txtRZP_17)
    txtRZP.Add(txtRZP_18)
    txtRZP.Add(txtRZP_19)
    txtRZP.Add(txtRZP_20)
    txtRZP.Add(txtRZP_21)
    '
    '
    '
    lblREZ.Add(lblREZ_00)
    lblREZ.Add(lblREZ_01)
    lblREZ.Add(lblREZ_02)
    lblREZ.Add(lblREZ_03)
    lblREZ.Add(lblREZ_04)
    lblREZ.Add(lblREZ_05)
    lblREZ.Add(lblREZ_06)
    lblREZ.Add(lblREZ_07)
    lblREZ.Add(lblREZ_08)
    lblREZ.Add(lblREZ_09)
    lblREZ.Add(lblREZ_10)
    lblREZ.Add(lblREZ_11)
    lblREZ.Add(lblREZ_12)
    lblREZ.Add(lblREZ_13)
    lblREZ.Add(lblREZ_14)
    lblREZ.Add(lblREZ_15)
    lblREZ.Add(lblREZ_16)
    lblREZ.Add(lblREZ_17)
    lblREZ.Add(lblREZ_18)
    lblREZ.Add(lblREZ_19)
    lblREZ.Add(lblREZ_20)
    lblREZ.Add(lblREZ_21)
    lblREZ.Add(lblREZ_22)
    lblREZ.Add(lblREZ_23)
    lblREZ.Add(lblREZ_24)
    lblREZ.Add(lblREZ_25)
    '
    '
    cboWIN.Add(cboWIN_0)
    cboWIN.Add(cboWIN_1)
    '
    '
    cboGKW.Add(cboGKW_0)
    cboGKW.Add(cboGKW_1)
    cboGKW.Add(cboGKW_2)
    cboGKW.Add(cboGKW_3)
    '
    txtUSCH.Add(txtUSCH_0)
    txtUSCH.Add(txtUSCH_1)
    '
    '
    IcbArt = 12
    For i = 0 To IcbArt
      Icb(i) = i
    Next i
    '
    'Combobox für TA,TB..DA,DB usw. (kann nur über ColEinst geändert werden
    '
    '
    If MnIopenArt And MnUserUfo Then
      cboZUS.Visible = True
    End If
    '
    If BitWrt(28, MenueParam.User.Drum) Or MnIopenArt Then
      chkGKExtra.Visible = True
    End If
    '
    GkaltID = -1
    '
    'Comboboxen für Anfangswerte und Daempfung Grunddaten
    '
    '
    For j = 0 To 1
      cboGKW(j).Items.Clear()
      wert = 1.0E+35
      For i = 0 To 35
        wert = CSng(StrFak10(i))
        cboGKW(j).Items.Add(CStr(wert))
        'MsgBox I & "  " & wert
      Next i
    Next j
    '
    '
    'Combobox für Kopplungsparameter
    cboGKW(2).Items.Clear()
    cboGKW(2).DisplayMember = "TEXT"
    cboGKW(2).ValueMember = "ID"

    For i = 0 To 13
      Select Case i
        Case 0
          '
          'konstant
          '
          cboGKW(2).Items.Add(New ListTextID(Asc("="), Texxt(1030 + i)))
        Case 1
          '
          'variabel
          '
          cboGKW(2).Items.Add(New ListTextID(Asc("#"), Texxt(1030 + i)))

        Case 2
          '
          'größer
          '
          'cboGKW(2).AddItem(Texxt(1030 + i))
          'cboGKW(2).ItemData(cboGKW(2).NewIndex) = Asc(">")
          cboGKW(2).Items.Add(New ListTextID(Asc(">"), Texxt(1030 + i)))
        Case 3
          '
          'kleiner
          '
          cboGKW(2).Items.Add(New ListTextID(Asc("<"), Texxt(1030 + i)))

        Case 4
          '
          'Verhältnis /
          '
          cboGKW(2).Items.Add(New ListTextID(Asc("/"), Texxt(1030 + i)))
        Case 5
          '
          'Verhältnis \
          '
          'cboGKW(2).AddItem(Texxt(1030 + i))
          'cboGKW(2).ItemData(cboGKW(2).NewIndex) = Asc("\")
          cboGKW(2).Items.Add(New ListTextID(Asc("\"), Texxt(1030 + i)))
        Case 6
          '
          'Verhältnis :
          '
          cboGKW(2).Items.Add(New ListTextID(Asc(":"), Texxt(1030 + i)))
          '
        Case 7
          '
          'Verhältnis :
          '
          cboGKW(2).Items.Add(New ListTextID(Asc("%"), Texxt(1030 + i)))
          '
        Case 8
          '
          'Summe S(vor)+S(rück)&
          '
          cboGKW(2).Items.Add(New ListTextID(Asc("&"), Texxt(1030 + i)))
          '
        Case 9
          '
          'Kopplung 0
          '
          cboGKW(2).Items.Add(New ListTextID(Asc("@"), Texxt(1030 + i)))
        Case Else
          '
          'weitere Kopplungen (A,B,C,D)
          '
          cboGKW(2).Items.Add(New ListTextID(55 + i, Texxt(1030 + i)))

      End Select
    Next i
    '

    cboGKW(3).Items.Clear()

    For i = 0 To 5
      cboGKW(3).Items.Add(CStr(i + 1))
    Next i
    '
    '
    'Art der Rechnung
    '
    '
    cboRART.Items.Clear()
    cboRART.DisplayMember = "TEXT"
    cboRART.ValueMember = "ID"
    For i = 0 To IcbArt
      cboRART.Items.Add(New ListTextID(Asc(ArtCME(Icb(i)).Substring(0, 1)), Texxt(1000 + Icb(i))))
    Next i
    cboRART.SelectedIndex = 0
    '
    'Zusatz für Art der Rechnung
    '
    '
    cboZUS.Items.Clear()
    cboZUS.Items.Add("0")
    For i = 1 To 26
      cboZUS.Items.Add(Chr(64 + i))
    Next i
    cboZUS.SelectedIndex = 0
    '
    'Einlesen Gruppe
    '
    '
    '
    If ConnOpen(Cncol) Then
      SqlStmt = "SELECT * FROM TBL_MISCH_GROUP"

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

      'Einlesen TBL_GRUSTART (Startwerte,Dämpfung,Kopplung)
      '
      SqlStmt = "SELECT * FROM TBL_GRUSTART ORDER BY GKWRT_ID,FRART_NR"
      CmdGRD.CommandText = SqlStmt
      'datGKW.Recordset = DBas.OpenRecordset(SqlStmt, dbOpenDynaset, dbConsistent, dbReadOnly)

      If Not FillDatset(AdaptGRD, TblGRD) Then
        Exit Sub
      End If
      TblGRD.AcceptChanges()
      If TblGRD.Rows.Count = 0 Then
        MsgBox(Texxt(3506) & ": " & "TBL_GRUSTART")
        Exit Sub
      End If ''
      '
      '
      '
      'TBL_GKWRTEXT
      Call CalcGKExtra(flgGKExtra, TblEXT, ViewEXT)
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

      '
      '
      '
      '
      'Liste für Reflexionswerte (Colorthek) für Vorlage
      '
      cboVOR.Items.Clear()
      cboVOR.DisplayMember = "TEXT"
      cboVOR.ValueMember = "ID"
      cboVOR.Items.Add(New ListTextID(0, Texxt(3)))
      cboVOR.Items.Add(New ListTextID(1, Texxt(4)))
      cboVOR.SelectedIndex = -1
      '
      '
      '
      '
      '
      '
      '
      'Liste für mit oder ohne Kontrastfarbabstand
      '
      cboKDE.Items.Clear()
      cboKDE.DisplayMember = "TEXT"
      cboKDE.ValueMember = "ID"
      cboKDE.Items.Add(New ListTextID(0, Texxt(3)))
      cboKDE.Items.Add(New ListTextID(1, Texxt(4)))
      cboKDE.SelectedIndex = -1
      '
      '
      '
      '
      'Liste für mit oder ohne Messungen über schwarzem Untergrund
      '
      cboSCHW.Items.Clear()
      cboSCHW.DisplayMember = "TEXT"
      cboSCHW.ValueMember = "ID"
      cboSCHW.Items.Add(New ListTextID(0, Texxt(3)))
      cboSCHW.Items.Add(New ListTextID(1, Texxt(4)))
      cboSCHW.SelectedIndex = -1
      '
      '
      '
      '
      '
      '
      'Liste für mit oder ohne Vertauschen von erster mit zweiter Messung
      '
      cboVERT.Items.Clear()
      cboVERT.DisplayMember = "TEXT"
      cboVERT.ValueMember = "ID"
      cboVERT.Items.Add(New ListTextID(0, Texxt(3)))
      cboVERT.Items.Add(New ListTextID(1, Texxt(4)))
      cboVERT.SelectedIndex = -1
      '
      ''
      '
      '
      'Liste Bindemittelprozentigkeit von Bindemittel übernehmen
      '
      cboBPR.Items.Clear()
      cboBPR.DisplayMember = "TEXT"
      cboBPR.ValueMember = "ID"
      cboBPR.Items.Add(New ListTextID(0, Texxt(3)))
      cboBPR.Items.Add(New ListTextID(1, Texxt(4)))
      cboBPR.SelectedIndex = -1
      '
      '
      '
      '
      '
      'Startrezept mit Gewicht (IGX=1,2,3)
      '
      cboMGGE.Items.Clear()
      cboMGGE.DisplayMember = "TEXT"
      cboMGGE.ValueMember = "ID"
      cboMGGE.Items.Add(New ListTextID(0, Texxt(3)))
      cboMGGE.Items.Add(New ListTextID(1, Texxt(4)))
      cboMGGE.SelectedIndex = -1
      '
      '
      '
      '
      'Liste Sortierung (Standard)
      '
      cboSOR.Items.Clear()
      cboSOR.DisplayMember = "TEXT"
      cboSOR.ValueMember = "ID"
      For i = 0 To 7
        cboSOR.Items.Add(New ListTextID(i, Texxt(841 + i)))
      Next i
      cboSOR.Items.Add(New ListTextID(i, Texxt(949)))
      cboSOR.SelectedIndex = -1

      '
      '
      '
      '
      'Art der Rezept bzw. Korrekturrechnung(muß auf 6 erweitert werden
      'falls Farbabstand DTO vorgegeben werden soll)
      '
      cboKOR.Items.Clear()
      cboKOR.ValueMember = "ID"
      cboKOR.DisplayMember = "TEXT"
      For i = 0 To 7
        cboKOR.Items.Add(New ListTextID(i, Texxt(572 + i)))
      Next i
      cboKOR.SelectedIndex = -1
      '
      ' 
      '
      '
      'Art der Berechnung mit Hilfe von Hilfskorrekturen
      '
      cboHLF.Items.Clear()
      cboHLF.DisplayMember = "TEXT"
      cboHLF.ValueMember = "ID"
      For i = 0 To 3
        cboHLF.Items.Add(New ListTextID(i, Texxt(546 + i)))
      Next i
      cboHLF.SelectedIndex = -1
      '
      '
      '
      '
      '
      '
      'Nummer für Winkel
      '
      For j = 0 To 1
        cboWIN(j).Items.Clear()
        For i = 0 To 8
          cboWIN(j).Items.Add(i)
        Next i
      Next j
      '
      '
      '
      '
      '
      '
      'TBL_REZMN Menuparameter für Rezeptberechnung
      '
      '
      '
      '

      SqlStmt = "SELECT * FROM TBL_REZMN ORDER BY REZMN_ID"
      CmdRZP.CommandText = SqlStmt

      If Not FillDatset(AdaptRZP, TblRZP) Then
        Exit Sub
      End If
      TblRZP.AcceptChanges()
      If TblRZP.Rows.Count = 0 Then
        MsgBox(Texxt(3506) & ": " & "TBL_REZMN")
        Exit Sub
      End If '
      '
      '
      '
      '

      '
      If Not MnIopenArt Then
        BindingRZP.AddNewItem.Visible = False
        BindingGKW.DeleteItem.Visible = False
        For i = 0 To txtGKW.Count - 1
          txtGKW(i).Enabled = False
        Next i
        For i = 0 To txtRZP.Count - 1
          txtRZP(i).Enabled = False
        Next i
        'MINDOS
        txtRZP(14).Enabled = True

        BindingUSE.Visible = False
        BindingMSY.Visible = False
        BindingMSG.Visible = False
        BindingMET.Visible = False
        cboMSG.Enabled = False
      End If
      'USER
      '
      If MnUserid = -1 Then
        SqlStmt = "SELECT DISTINCTROW TBL_USER.USER_ID AS USER_ID,USER_NAME FROM TBL_USER INNER JOIN TBL_USER_MISCH ON TBL_USER.USER_ID=TBL_USER_MISCH.USER_ID ORDER BY USER_NAME"
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
      '
      'Mischsysteme
      '
      '
      '
      SqlStmt = "SELECT DISTINCTROW TBL_USER_MISCH.USER_ID, TBL_USER_MISCH.MISCH_ID, TBL_MISCH.MISCH_KBEZ," _
      & "TBL_USER_MISCH.MISCH_TOP, TBL_USER_MISCH.MISCH_TDIFF, TBL_USER_MISCH.MISCH_SOND" _
      & " FROM TBL_MISCH INNER JOIN TBL_USER_MISCH ON TBL_MISCH.MISCH_ID = TBL_USER_MISCH.MISCH_ID" _
      & " WHERE TBL_USER_MISCH.USER_ID IN " & StrLinUse

      If MnMischid >= 0 Then
        '
        SqlStmt = SqlStmt & " AND TBL_USER_MISCH.MISCH_ID=" & MnMischid
        '
      End If
      SqlStmt = SqlStmt & " ORDER BY TBL_USER_MISCH.MISCH_ID"
      CmdMSY.CommandText = SqlStmt
      If Not FillDatset(AdaptMSY, TblMSY) Then
        Exit Sub
      End If
      TblMSY.AcceptChanges()
      If TblMSY.Rows.Count = 0 Then
        MsgBox(Texxt(3506) & ": " & "TBL_USER_MISCH")
        Exit Sub
      Else
        StrLinMSY = StrLin(TblMSY, "MISCH_ID")
      End If
      '
      '
      'Tabelle der User-Mischsystem-abhängige Methoden
      '
      '
      SqlStmt = "SELECT DISTINCTROW TBL_USER_METH_MISCH.USER_ID, TBL_USER_METH_MISCH.MISCH_ID," _
       & "TBL_USER_METH_MISCH.METH_ID, TBL_METH.METH_BEZ, TBL_USER_METH_MISCH.USER_RZP_GID," _
       & "TBL_USER_METH_MISCH.REZMN_ID,ICHI,BPROB,KGX,IGX,ISOR,KWE,KWD,REZVOR,SCHWRZ,MME,VERT,MINDOS" _
       & " FROM TBL_USER_METH_MISCH INNER JOIN TBL_METH ON TBL_USER_METH_MISCH.METH_ID = TBL_METH.METH_ID" _
       & " WHERE TBL_USER_METH_MISCH.USER_ID IN " & StrLinUse & " AND TBL_USER_METH_MISCH.MISCH_ID IN " & StrLinMSY
      '
      '
      '
      'MethodenID's, die mit Mischsystemen arbeiten, werden nach StrLinMet gespeichert
      '
      StrLinMet = "("
      For i = 50 To 99
        StrLinMet = StrLinMet & CStr(i) & ","
      Next
      StrLinMet = StrLinMet & "106,109,110,111)"
      If MnMethID = -1 Then
        SqlStmt = SqlStmt & " AND TBL_USER_METH_MISCH.METH_ID IN " & StrLinMet
      Else
        SqlStmt = SqlStmt & " AND TBL_USER_METH_MISCH.METH_ID=" & MnMethID
      End If
      SqlStmt = SqlStmt & " ORDER BY TBL_USER_METH_MISCH.METH_ID"
      CmdMET.CommandText = SqlStmt
      If Not FillDatset(AdaptMET, TblMET) Then
        Exit Sub
      End If
      Call UpdateLangText(TblMET, 1800, "METH_ID", "", "METH_BEZ")
      TblMET.AcceptChanges()
      If TblMET.Rows.Count = 0 Then
        MsgBox(Texxt(3506) & ": " & "TBL_USER_METH_MISCH")
        Me.DialogResult = Windows.Forms.DialogResult.Abort
        Exit Sub
      End If


      '
      'Tabelle der User-Mischsystem-abhängigen Meßgeräte
      '
      '
      '
      '
      '
      SqlStmt = "SELECT TBL_USER_MISCH_MESSG.USER_ID, TBL_USER_MISCH_MESSG.MISCH_ID, TBL_USER_MISCH_MESSG.MESSG_ID, TBL_MESSG.MESSG_KBEZ, TBL_USER_MISCH_MESSG.GKWRT_ID" _
      & " FROM TBL_MESSG INNER JOIN (TBL_MISCH_MESSG INNER JOIN (TBL_USER_MESSG INNER JOIN TBL_USER_MISCH_MESSG ON" _
      & " (TBL_USER_MESSG.MESSG_ID = TBL_USER_MISCH_MESSG.MESSG_ID) AND (TBL_USER_MESSG.USER_ID = TBL_USER_MISCH_MESSG.USER_ID)" _
      & " AND (TBL_USER_MESSG.MESSG_ID = TBL_USER_MISCH_MESSG.MESSG_ID) AND (TBL_USER_MESSG.USER_ID = TBL_USER_MISCH_MESSG.USER_ID))" _
      & " ON (TBL_MISCH_MESSG.MESSG_ID = TBL_USER_MISCH_MESSG.MESSG_ID) AND (TBL_MISCH_MESSG.MISCH_ID = TBL_USER_MISCH_MESSG.MISCH_ID))" _
      & " ON (TBL_MESSG.MESSG_ID = TBL_USER_MISCH_MESSG.MESSG_ID) AND (TBL_MESSG.MESSG_ID = TBL_MISCH_MESSG.MESSG_ID)" _
      & " AND (TBL_MESSG.MESSG_ID = TBL_USER_MESSG.MESSG_ID)" _
      & " WHERE TBL_USER_MISCH_MESSG.USER_ID IN " & StrLinUse & " AND TBL_USER_MISCH_MESSG.MISCH_ID IN " & StrLinMSY
      If MnMessgID >= 0 Then
        SqlStmt = SqlStmt & " AND TBL_USER_MISCH_MESSG.MESSG_ID=" & MnMessgID
      End If
      SqlStmt = SqlStmt & " ORDER BY TBL_USER_MISCH_MESSG.MESSG_ID"
      CmdMSG.CommandText = SqlStmt
      TblMSG.Clear()
      If Not FillDatset(AdaptMSG, TblMSG) Then
        Exit Sub
      End If
      TblMSG.AcceptChanges()
      If TblMSG.Rows.Count = 0 Then
        MsgBox(Texxt(3506) & ": " & "TBL_USER_MISCH_MESSG")
        Exit Sub
      End If
      '

      '
      'Prüfen, ob Messgeräte in TBL_MISCH_MESSG aber nicht in TBL_USER_MISCH_MESSG
      'Fehlende Messgeräte in Tabelle TblMSG einfügen
      '
      '
      '
      CmdHLF.Connection = Cncol()
      CmdSET.Connection = Cncol()
      For k = 0 To TblUSE.Rows.Count - 1
        ViewMSY.RowFilter = "USER_ID=" & TblUSE.Rows(k)("User_id")
        For i = 0 To ViewMSY.Count - 1
          SqlStmt = "SELECT DISTINCTROW TBL_MISCH_MESSG.MESSG_ID" _
                   & " FROM TBL_USER_MESSG,TBL_USER_MISCH,TBL_MISCH_MESSG WHERE " _
                   & "TBL_MISCH_MESSG.MESSG_ID = TBL_USER_MESSG.MESSG_ID AND " _
                   & "(((TBL_USER_MESSG.User_id) = " & TblUSE.Rows(k)("User_id") & ") AND " _
                   & "((TBL_MISCH_MESSG.MISCH_ID) = " & ViewMSY(i)("misch_id") & " ))"

          CmdHLF.CommandText = SqlStmt
          DatHLF = DataReader(CmdHLF, CommandBehavior.Default, Cncol)
          Do While DatHLF.Read
            SqlStmt = "SELECT * FROM TBL_USER_MISCH_MESSG WHERE USER_ID=" & TblUSE.Rows(k)("User_id") _
            & " AND MISCH_ID=" & ViewMSY(i)("misch_id") _
            & " AND MESSG_ID=" & DatHLF("MESSG_ID")
            CmdSET.CommandText = SqlStmt
            DatSET = DataReader(CmdSET, CommandBehavior.Default, Cncol)

            If Not DatSET.Read Then
              Count = TblMSG.Rows.Count
              TblMSG.Rows.Add(TblMSG.NewRow)
              TblMSG.Rows(Count)("USER_ID") = TblUSE.Rows(k)("User_id")
              TblMSG.Rows(Count)("MISCH_ID") = ViewMSY(i)("misch_id")
              TblMSG.Rows(Count)("MESSG_ID") = DatHLF("MESSG_ID")
              TblMSG.Rows(Count)("GKWRT_ID") = 0
            End If
            DatSET.Close()
          Loop
          DatHLF.Close()
        Next i
      Next k
      Cncol.Close()
      '
      '
      '
      '
      ConnGKW.DataSource = TblGKW
      ConnRZP.DataSource = TblRZP
      ConnMET.DataSource = ViewMET
      ConnMSG.DataSource = ViewMSG
      ConnMSY.DataSource = ViewMSY
      ConnUSE.DataSource = ViewUse
      '
      '
      cboUSE.DataSource = ConnUSE
      cboUSE.DisplayMember = "USER_NAME"
      cboUSE.ValueMember = "USER_ID"
      '
      '
      cboMSY.DataSource = ConnMSY
      cboMSY.DisplayMember = "MISCH_KBEZ"
      cboMSY.ValueMember = "MISCH_ID" '
      '
      cboMSG.DataSource = ConnMSG
      cboMSG.DisplayMember = "MESSG_KBEZ"
      cboMSG.ValueMember = "MESSG_ID"
      '

      '
      lblUSE.DataBindings.Add("TEXT", ConnUSE, "USER_NAME")
      lblMSY.DataBindings.Add("TEXT", ConnMSY, "MISCH_KBEZ")
      txtUSCH_0.DataBindings.Add("TEXT", ConnMSY, "MISCH_TOP")
      txtUSCH_1.DataBindings.Add("TEXT", ConnMSY, "MISCH_TDIFF")
      lblMET.DataBindings.Add("TEXT", ConnMET, "METH_BEZ")
      '
      '
      txtRZP_00.DataBindings.Add("TEXT", ConnRZP, "REZMN_ID")
      txtRZP_01.DataBindings.Add("TEXT", ConnRZP, "REZMN_BEZ")
      txtRZP_01.MaxLength = TblRZP.Columns("REZMN_BEZ").MaxLength
      txtRZP_19.DataBindings.Add("TEXT", ConnRZP, "REZMN_GGE")
      txtRZP_12.DataBindings.Add("TEXT", ConnRZP, "REZMN_DTO")
      txtRZP_13.DataBindings.Add("TEXT", ConnRZP, "REZMN_GTO")
      txtRZP_10.DataBindings.Add("TEXT", ConnRZP, "REZMN_AZU")
      txtRZP_09.DataBindings.Add("TEXT", ConnRZP, "REZMN_DEGUT")
      txtRZP_14.DataBindings.Add("TEXT", ConnMET, "MINDOS")
      txtRZP_03.DataBindings.Add("TEXT", ConnRZP, "REZMN_DENHLF")
      txtRZP_16.DataBindings.Add("TEXT", ConnRZP, "REZMN_FDE")
      txtRZP_17.DataBindings.Add("TEXT", ConnRZP, "REZMN_GDE")
      txtRZP_04.DataBindings.Add("TEXT", ConnRZP, "REZMN_DICKE")
      txtRZP_05.DataBindings.Add("TEXT", ConnRZP, "REZMN_RZP")
      txtRZP_06.DataBindings.Add("TEXT", ConnRZP, "REZMN_MIN")
      txtRZP_07.DataBindings.Add("TEXT", ConnRZP, "REZMN_MAX")
      txtRZP_08.DataBindings.Add("TEXT", ConnRZP, "REZMN_DEOK")
      cboWIN_0.DataBindings.Add("TEXT", ConnMET, "KWE")
      cboWIN_1.DataBindings.Add("TEXT", ConnMET, "KWD")
      cboKDE.DataBindings.Add("SELECTEDINDEX", ConnMET, "KGX")
      cboMGGE.DataBindings.Add("SELECTEDINDEX", ConnMET, "MME")
      cboBPR.DataBindings.Add("SELECTEDINDEX", ConnMET, "BPROB")
      cboVOR.DataBindings.Add("SELECTEDINDEX", ConnMET, "REZVOR")
      cboSCHW.DataBindings.Add("SELECTEDINDEX", ConnMET, "SCHWRZ")
      cboVERT.DataBindings.Add("SELECTEDINDEX", ConnMET, "VERT")
      cboSOR.DataBindings.Add("SELECTEDINDEX", ConnMET, "ISOR")
      cboHLF.DataBindings.Add("SELECTEDINDEX", ConnMET, "ICHI")
      cboKOR.DataBindings.Add("SELECTEDINDEX", ConnMET, "IGX")
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
      txtGKW_15.DataBindings.Add("TEXT", ConnGKW, "GK13")
      txtGKW_16.DataBindings.Add("TEXT", ConnGKW, "GK14")
      txtGKW_17.DataBindings.Add("TEXT", ConnGKW, "GK15")
      txtGKW_18.DataBindings.Add("TEXT", ConnGKW, "GK16")

      cboGRP.DataBindings.Add("SELECTEDVALUE", ConnMET, "USER_RZP_GID")
      '
      '
      '
      '
      '
      Cursor = Cursors.Default
    End If
    '
    '
    '
    '
  End Sub

  Private Sub frmNWU_Paint(sender As Object, e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint
    TabNWU.ItemSize = New Size(0.5 * TabNWU.Width - 4, TabNWU.ItemSize.Height)
  End Sub

  
 
  Sub GridStart(ByVal Ichf As Integer, ByVal NPS As Integer, ByVal CDE As String, ByRef Koppwrt As Single, ByRef Start() As Object, ByRef DaWrt() As Object, ByRef Kopp() As String)
    Dim i As Integer
    Koppwrt = 0
    For i = 0 To 5
      Start(i) = 0
      DaWrt(i) = 0
      If Ichf > 5 Then
        Kopp(i) = "="
      Else
        Kopp(i) = "#"
      End If
    Next i
    If CnzDep(CDE) <> 0 Then
      Start(2) = 4
      Start(3) = 1
    End If
    Select Case Ichf
      Case 0
        '
        'Farbmittel
        '
        '
        If CDE.Substring(0, 1) = "S" Or CDE.Substring(0, 1) = "E" Then
          Koppwrt = 0.0001
          Start(0) = 1
          DaWrt(0) = 0
          Kopp(0) = "%"
          Start(1) = 1
          DaWrt(1) = 0
          Kopp(1) = ":"
          Start(2) = 0.5
          DaWrt(2) = 0
          Kopp(2) = ":"
          Start(3) = 0.5
          DaWrt(3) = 0
          Kopp(3) = ":"
          Start(4) = 1
          DaWrt(4) = 0
          Kopp(4) = "%"
          Start(5) = 0
          DaWrt(5) = 0
          Kopp(5) = "="
        End If

      Case 1
        '
        'Bindemittel
        '
        '
        Koppwrt = 0
        For i = 0 To NPS - 1
          Start(i) = 0
          DaWrt(i) = 0
          Kopp(i) = "="
        Next i
        If CnzDep(CDE) <> 0 Then
          Start(2) = 1
          Start(3) = 1
        End If
        If CDE.Substring(0, 1) = "S" Or CDE.Substring(0, 1) = "E" Then
          Koppwrt = 0.0001
          Start(0) = 0
          DaWrt(0) = 0
          Kopp(0) = "="
          Start(1) = 0
          DaWrt(1) = 0
          Kopp(1) = "="
          Start(2) = 0
          DaWrt(2) = 0
          Kopp(2) = "="
          Start(3) = 0
          DaWrt(3) = 0
          Kopp(3) = "="
          Start(4) = 0
          DaWrt(4) = 0
          Kopp(4) = "="
          Start(5) = 0
          DaWrt(5) = 0
          Kopp(5) = "="
        End If

      Case 2
        '
        'Weißpigment
        '
        Koppwrt = 0
        For i = 0 To NPS - 1
          Start(i) = 0
          DaWrt(i) = 0
          Kopp(i) = "#"
        Next i
        If CnzDep(CDE) <> 0 Then
          Start(2) = 1
          Start(3) = 1
        End If
        If Not Transp(CDE) Then
          If CnzDep(CDE) = 0 Then
            Start(1) = 1
            Kopp(1) = "="
          Else
            Start(0) = 1
            Kopp(0) = "="
          End If
        End If
        If CDE.Substring(0, 1) = "S" Or CDE.Substring(0, 1) = "E" Then
          Koppwrt = 0.0001
          Start(0) = 1
          DaWrt(0) = 0
          Kopp(0) = "%"
          Start(1) = 1
          DaWrt(1) = 0
          Kopp(1) = ":"
          Start(2) = 0.5
          DaWrt(2) = 0
          Kopp(2) = ":"
          Start(3) = 0.5
          DaWrt(3) = 0
          Kopp(3) = ":"
          Start(4) = 1
          DaWrt(4) = 0
          Kopp(4) = "%"
          Start(5) = 0
          DaWrt(5) = 0
          Kopp(5) = "="
          If Mid(CDE, 1, 1) = "E" Then
            Kopp(1) = "="
            Kopp(2) = "="
            Kopp(3) = "="
          End If
        End If
        '
      Case 3
        '
        'Schwarzpigment
        '
        If CDE.Substring(0, 1) = "S" Or CDE.Substring(0, 1) = "E" Then
          Koppwrt = 0.0001
          Start(0) = 1
          DaWrt(0) = 0
          Kopp(0) = "%"
          Start(1) = 1
          DaWrt(1) = 0
          Kopp(1) = ":"
          Start(2) = 0.5
          DaWrt(2) = 0
          Kopp(2) = ":"
          Start(3) = 0.5
          DaWrt(3) = 0
          Kopp(3) = ":"
          Start(4) = 1
          DaWrt(4) = 0
          Kopp(4) = "%"
          Start(5) = 0
          DaWrt(5) = 0
          Kopp(5) = "="
        End If
      Case 4
        '
        'Metallic
        '
        '
        If CDE.Substring(0, 1) = "S" Or CDE.Substring(0, 1) = "E" Then
          Koppwrt = 0.0001
          Start(0) = 1
          DaWrt(0) = 0
          Kopp(0) = "%"
          Start(1) = 1
          DaWrt(1) = 0
          Kopp(1) = ":"
          Start(2) = 0.5
          DaWrt(2) = 0
          Kopp(2) = ":"
          Start(3) = 0.5
          DaWrt(3) = 0
          Kopp(3) = ":"
          Start(4) = 1
          DaWrt(4) = 0
          Kopp(4) = "%"
          Start(5) = 0
          DaWrt(5) = 0
          Kopp(5) = "="
        End If
      Case 5
        '
        'Effektpigment
        '
        '
        '
        If CDE.Substring(0, 1) = "S" Or CDE.Substring(0, 1) = "E" Then
          Koppwrt = 0.0001
          Start(0) = 1
          DaWrt(0) = 0
          Kopp(0) = "%"
          Start(1) = 1
          DaWrt(1) = 0
          Kopp(1) = ":"
          Start(2) = 0.5
          DaWrt(2) = 0
          Kopp(2) = ":"
          Start(3) = 0.5
          DaWrt(3) = 0
          Kopp(3) = ":"
          Start(4) = 1
          DaWrt(4) = 0
          Kopp(4) = "%"
          Start(5) = 0
          DaWrt(5) = 0
          Kopp(5) = "="
        End If
      Case 6, 7, 8
        '
        'Restfarbe
        'Laborfarbe
        'Zusatzmittel
        '
        Koppwrt = 0.0
        For i = 0 To NPS - 1
          Start(i) = 0
          DaWrt(i) = 0
          Kopp(i) = "="
        Next i
        If CnzDep(CDE) <> 0 Then
          Start(2) = 1
          Start(3) = 1
        End If
        '
        '
    End Select
  End Sub

  '
  '
  Sub UpdMsyMet(ByVal Rezmn_ID As Integer, ByVal Group_id As Integer, ByVal ICHI As Integer, ByVal Igx As Integer, ByVal Kgx As Boolean, ByVal Bproz As Boolean, ByVal Isor As Integer, ByVal kwe As Integer, ByVal kwd As Integer, ByVal Mme As Boolean, ByVal REZV As Boolean, ByVal Schwrz As Boolean, ByVal Vert As Boolean, ByVal Mindos As Single)
    '
    '
    ConnMET.Current("REZMN_ID") = Rezmn_ID
    '
    ConnMET.Current("SCHWRZ") = Schwrz
    ConnMET.Current("VERT") = Vert
    ConnMET.Current("REZVOR") = REZV
    ConnMET.Current("MME") = Mme
    ConnMET.Current("KWE") = kwe
    ConnMET.Current("ISOR") = Isor
    ConnMET.Current("KGX") = Kgx
    ConnMET.Current("IGX") = Igx
    ConnMET.Current("BPROB") = Bproz
    ConnMET.Current("ICHI") = ICHI
    ConnMET.Current("MINDOS") = SQLpunkt(CSng(Mindos))
    ConnMET.Current("USER_RZP_GID") = Group_id
    '
    '



  End Sub

  Sub UpdMesMys(ByVal GkwrtID As Integer, ByVal GkaltID As Integer)
    Exit Sub
    If GkwrtID = GkaltID Or GkaltID = -1 Then Exit Sub
    imsg = MsgBox(Texxt(2920) & Chr(13) & Texxt(2921), 4, Texxt(2000))
    If imsg = 7 Then
      Exit Sub
    End If
    ConnMSG.Current("GKWRT_ID") = GkwrtID
  End Sub

  Private Sub cboGKW_DropDownClosed(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboGKW_0.DropDownClosed, cboGKW_1.DropDownClosed, cboGKW_2.DropDownClosed, cboGKW_3.DropDownClosed
    Dim i As Integer
    Dim k As Integer
    Dim index As Integer
    index = CInt(sender.name.substring(7, 1))
    '
    'Nummer des Parameters
    '
    i = flgGKW.CurrentCell.ColumnIndex

    '
    'Startmenge (index=0);Dämpfungsstärke (index=1) ;Kopplungsart(index=2)
    '
    k = (flgGKW.CurrentCell.RowIndex) Mod 3
    If i = 0 And index > 0 Then
      cboGKW(index).Visible = False
      Exit Sub
    End If
    If index = 2 Then
      flgGKW.CurrentCell.Value = Chr(DirectCast(cboGKW(index).SelectedItem, ListTextID).ID)
    Else
      flgGKW.CurrentCell.Value = cboGKW(index).SelectedItem
    End If
    cboGKW(inde).Visible = False
    Call GridSave()

  End Sub
  Private Sub cboRART_DropDownClosed(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboRART.DropDownClosed, cboZUS.DropDownClosed
    If cboRART.SelectedIndex = -1 Then Exit Sub
    If cboZUS.SelectedIndex = -1 Then Exit Sub
    If cboRART.SelectedItem Is Nothing Then Exit Sub
    txtGKW(13).Text = Chr(DirectCast(cboRART.SelectedItem, ListTextID).ID) & cboZUS.Text
    Call GridCrea(Trim(txtGKW(13).Text), ConnGKW.Current("gkwrt_id"), True)
  End Sub



  Private Sub ConnGKW_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnGKW.AddingNew
    Dim i As Integer
    Dim k As Integer
    Dim NewGrd As DataRow
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
    TblGKW.Columns("GKWRT_BEZ").DefaultValue = TblGKW.Columns("GKWRT_BEZ").DefaultValue
    If TblGKW.Columns("GKWRT_BEZ").DefaultValue.length > TblGKW.Columns("GKWRT_BEZ").MaxLength Then
      TblGKW.Columns("GKWRT_BEZ").DefaultValue = TblGKW.Columns("GKWRT_BEZ").DefaultValue.substring(0, TblGKW.Columns("GKWRT_BEZ").MaxLength)
    End If
    '
    '
    '
    Call SaveGRD(0, MaxGkwID)
    Call SaveGKExtra(0, MaxGkwID, TblEXT, ViewEXT)
    '
    ''
  End Sub
  Sub SaveGRD(ByRef GKwrtIDSource As Integer, ByRef GKwrtIDTarget As Integer)
    Dim k As Integer
    Dim i As Integer
    Dim NewGrd As DataRow
    Dim VIEWHLF As New DataView(TblGRD)
    If GKwrtIDSource = GKwrtIDTarget Then Exit Sub
    VIEWHLF.RowFilter = "GKWRT_ID=" & GKwrtIDTarget
    For k = VIEWHLF.Count - 1 To 0 Step -1
      VIEWHLF.Delete(k)
    Next
    For k = 0 To AnzIart - 1
      NewGrd = TblGRD.NewRow
      For i = 0 To TblGRD.Columns.Count - 1
        NewGrd(i) = RowsGRD(k)(i)
      Next
      NewGrd("GKWRT_ID") = GKwrtIDTarget
      TblGRD.Rows.Add(NewGrd)
    Next k
  End Sub
  Private Sub ConnGKW_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnGKW.CurrentChanged


    Dim i As Integer
    Dim j As Integer
    '
    '
    lblOPTDAT.Visible = False
    '
    If ConnGKW.Current Is Nothing OrElse ConnGKW.Count = 0 Then Exit Sub

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
    For i = 0 To cboGKW.Count - 1
      cboGKW(i).Visible = False
    Next i
    For i = 0 To cboRART.Items.Count - 1
      If DirectCast(cboRART.Items(i), ListTextID).ID = Asc(Mid(ConnGKW.Current("CDE"), 1, 1)) Then
        cboRART.SelectedIndex = i
        Exit For
      End If
    Next i
    For i = 0 To cboZUS.Items.Count - 1
      If Asc(cboZUS.Items(i)) = Asc(Mid(ConnGKW.Current("CDE"), 2, 1)) Then
        cboZUS.SelectedIndex = i
        Exit For
      End If
    Next i

    If i >= cboRART.Items.Count Then
      cboRART.SelectedIndex = 0
      txtGKW(13).Text = ConnGKW.Current("CDE")
    End If
    '
    '

    '
    '
    '
    If ConnGKW.Current("gkwrt_id") = 0 Or MnUserUfo Then
      For i = 0 To txtGKW.Count - 1
        txtGKW(i).Enabled = True
      Next i
      txtGKW(13).Enabled = False
      txtGKW(0).Enabled = False
      BindingGKW.AddNewItem.Visible = True
      cboRART.Enabled = True
      cboZUS.Enabled = True
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
      cboZUS.Enabled = False


      txtGKW(14).Enabled = True
      flgGKExtra.ReadOnly = True
      flgGKExtra.AllowUserToAddRows = False
      flgGKExtra.AllowUserToDeleteRows = False
    End If
    If Not RowsGRD Is Nothing AndAlso RowsGRD.Count > 0 Then
      Call GridSave()
    End If


    If cboRART.SelectedIndex > -1 Then
      Call GridCrea(ConnGKW.Current("CDE"), ConnGKW.Current("gkwrt_id"), False)
    End If
    If Not ConnMSG.Current Is Nothing AndAlso ConnMSG.Count <> 0 Then
      ConnMSG.Current("GKWRT_ID") = ConnGKW.Current("GKWRT_ID")
    End If

    If Not MnIopenArt Then
      '
      '
      '
      'Falls das aktuelle Mischsystem auf GK-Sätze zurückgreift, wird eine Labelbox angezeigt!!!
      '
      '

      SqlStmt = "SELECT * FROM TBL_GRUND_FARBM WHERE MISCH_ID=" & MnMischid & " AND GKWRT_ID= " & ConnGKW.Current("gkwrt_id") & " AND MESSGRW_ID=" & MnMessgRwID
      CmdHLF.CommandText = SqlStmt
      CmdHLF.Connection = Cndat()
      DatHLF = DataReader(CmdHLF, CommandBehavior.SingleRow & CommandBehavior.CloseConnection, Cndat)
      If DatHLF.Read Then
        lblOPTDAT.Visible = True
      Else
        lblOPTDAT.Visible = False
      End If
      DatHLF.Close()
    End If
  End Sub

  
  Private Sub ConnMET_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMET.CurrentChanged
    If ConnMET.Current Is Nothing OrElse ConnMET.Count = 0 Then Exit Sub
    '
    BindingMET.Enabled = False
    '
    ViewGRP.RowFilter = "MISCH_ID=" & ConnMET.Current("misch_id")
    If Not IsNothing(ConnMSY.Current) AndAlso ViewGRP.Count = 0 Then
      MsgBox(Texxt(1664) & Space(1) & Texxt(422) & Space(1) & ConnMSY.Current("MISCH_KBEZ"))
      Me.Close()
    End If
    If IsDBNull(ConnMET.Current("rezmn_ID")) OrElse IsDBNull(ConnMET.Current("user_rzp_gid")) Then
      Call UpdMsyMet(0, 0, 0, 0, False, False, 0, 0, 1, False, False, False, False, 0.0#)
      cboGRP.SelectedIndex = 0
    Else
      cboGRP.SelectedValue = ConnMET.Current("user_rzp_gid")
      '
      '
    End If
    If ConnRZP.Count > 0 Then
      'ConnRZP.CurrencyManager.Refresh()
      ConnRZP.Position = ConnRZP.Find("REZMN_ID", ConnMET.Current("rezmn_ID"))
    End If
    BindingMET.Enabled = True
  End Sub
  

  Private Sub ConnMSG_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMSG.CurrentChanged
    If ConnGKW Is Nothing OrElse ConnGKW.Count = 0 Then Exit Sub
    If ConnMSG.Count = 0 Then Exit Sub
    BindingMSG.Enabled = False
    cboMSG.Enabled = False

    'ConnGKW.CurrencyManager.Refresh()
    ConnGKW.Position = ConnGKW.Find("GKWRT_ID", ConnMSG.Current("gkwrt_id"))

    BindingMSG.Enabled = True
    cboMSG.Enabled = True

    ''
  End Sub
 

  Private Sub ConnMSY_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMSY.CurrentChanged
    If ConnMSY.Current Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    If ConnMSY.Count = 0 Then
      TabNWU.Visible = False
      Exit Sub
    End If
    TabNWU.Visible = True

     BindingMSY.Enabled = False
    '
    'Tabelle der User-Mischsystem-abhängige Methoden
    '
    '
    If Not IsNothing(ConnMET.Current) Then
      ConnMET.Current.endedit()
    End If
    ViewMET.RowFilter = "USER_ID=" & ConnUSE.Current("User_id") & " AND MISCH_ID=" & ConnMSY.Current("misch_id")

    If ConnMET.Count = 0 Then
      TabNWU.TabIndex = 1
      TabNWU.TabPages(0).Enabled = False
      Exit Sub
    Else
      TabNWU.TabPages(0).Enabled = True
      ConnMET.CurrencyManager.Refresh()
      ConnMET.Position = 0
    End If
    '
    'Tabelle der User-Mischsystem-abhängigen Meßgeräte
    '
    '
    '
    '
    If Not IsNothing(ConnMSG.Current) Then
      ConnMSG.Current.endedit()
    End If
    ViewMSG.RowFilter = "USER_ID=" & ConnUSE.Current("User_id") & " AND MISCH_ID=" & ConnMSY.Current("misch_id")

    If ConnMSG.Count = 0 Then
      GkaltID = -1
    Else
      '
      GkaltID = ConnMSG.Current("gkwrt_id")
      ConnMSG.CurrencyManager.Refresh()
      ConnMSG.Position = 0
    End If
    BindingMSY.Enabled = True

  End Sub

 

  Private Sub ConnUSE_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnUSE.CurrentChanged


    '
    If ConnUSE.Current Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    BindingUSE.Enabled = False
    '
    ' Userabhängige Mischsysteme auswählen
    '
    If Not IsNothing(ConnMSY.Current) Then
      ConnMSY.Current.endedit()
    End If
    ViewMSY.RowFilter = "USER_ID=" & ConnUSE.Current("User_id")

    If ConnMSY.Count = 0 Then
      imsg = MsgBox(Texxt(2982), 0, Texxt(2000))
    Else
      ConnMSY.CurrencyManager.Refresh()
      ConnMSY.Position = 0
      BindingUSE.Enabled = True
    End If
    '


    ''
  End Sub
  Private Sub flgGKW_CellClick(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles flgGKW.CellClick

    Dim i As Integer
    If flgGKW.CurrentCell.ColumnIndex < 0 Then Exit Sub
    For i = 0 To 3
      cboGKW(i).Visible = False
    Next i
    inde = (flgGKW.CurrentCell.RowIndex) Mod 3
    Ichf = Fix((flgGKW.CurrentCell.RowIndex) / 3)
    If Ichf > 5 Then
      Exit Sub
    End If
    If CnzDep(CStr(txtGKW(13).Text)) <> 0 And (flgGKW.CurrentCell.ColumnIndex = 3 Or flgGKW.CurrentCell.ColumnIndex = 4) Then
      If inde = 0 Then
        If flgGKW.CurrentCell.ColumnIndex = 3 Then
          inde = 3
          If flgGKW.CurrentCell.Value < cboGKW(inde).Items(0) Or flgGKW.CurrentCell.Value > cboGKW(inde).Items(cboGKW(inde).Items.Count - 1) Then
            flgGKW.CurrentCell.Value = cboGKW(inde).Items(3)
          End If
        ElseIf flgGKW.CurrentCell.ColumnIndex = 4 Then
          inde = 0
        End If
      Else
        flgGKW.CurrentCell.Value = " "
        Exit Sub
      End If
    End If
    If flgGKW.CurrentCell.ColumnIndex = 0 And inde > 0 Then
      flgGKW.CurrentCell.Value = " "
      Exit Sub
    End If

    For i = 0 To cboGKW(inde).Items.Count - 1
      If inde = 3 Then
        If flgGKW.CurrentCell.Value = cboGKW(inde).Items(i) Then
          cboGKW(inde).SelectedIndex = i
          Exit For
        End If
      ElseIf inde = 2 Then
        If flgGKW.CurrentCell.Value = Chr(DirectCast(cboGKW(inde).Items(i), ListTextID).ID) Then
          cboGKW(inde).SelectedIndex = i
          Exit For
        End If
      Else
        If flgGKW.CurrentCell.Value = cboGKW(inde).Items(i) Then
          cboGKW(inde).SelectedIndex = i
          Exit For
        End If
      End If
    Next i
    cboGKW(inde).Visible = False

    cboGKW(inde).Visible = True
    Call LocateCombo(cboGKW(inde), flgGKW)


  End Sub
  Sub LocateCombo(ByVal cboGKW As ComboBox, ByVal flggkw As DataGridView)
    Dim X As Integer
    Dim Y As Integer
    Dim i As Integer
    X = flggkw.Location.X
    If flggkw.RowHeadersVisible Then
      X = X + flggkw.RowHeadersWidth
    End If
    For i = 0 To flggkw.CurrentCell.ColumnIndex - 1
      If flggkw.Columns(i).Visible Then
        X = X + flggkw.Columns(i).Width
      End If
    Next
    X = X - flggkw.HorizontalScrollingOffset
    cboGKW.Left = X

    Y = flggkw.Location.Y + flggkw.ColumnHeadersHeight
    For i = 0 To flggkw.CurrentCell.RowIndex - 1
      Y = Y + flggkw.Rows(i).Height
    Next
    Y = Y - flggkw.VerticalScrollingOffset
    cboGKW.Top = Y
    cboGKW.Width = flggkw.CurrentCell.Size.Width
  End Sub

  WriteOnly Property Aform() As Form
    Set(ByVal value As Form)
      MnAform = value
    End Set
  End Property

  WriteOnly Property TblMisch() As String
    Set(ByVal value As String)
      MnTblMisch = value
    End Set
  End Property

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
  WriteOnly Property MessgrwID() As Integer
    Set(ByVal value As Integer)
      MnMessgRwID = value
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


  

  

  

  Sub GridCrea(ByVal CDE As String, ByVal GkwrtID As Integer, NewCDE As Boolean)
    Dim Itext() As Integer
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim ColWidth As Integer = 60
    Dim Count As Integer
    Dim ExtraRow As DataRow
    Call ItextGrund(CDE, Itext)
    NPS = UBound(Itext) + 1

    '
    '
    '
    '
    flgGKW.Rows.Clear()
    flgGKW.Columns.Clear()
    flgGKW.RowHeadersVisible = True
    flgGKW.RowHeadersDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgGKW.TopLeftHeaderCell.Style = flgGKW.RowHeadersDefaultCellStyle
    flgGKW.TopLeftHeaderCell.Value = "(" & CDE & ") " & cboRART.Text

    flgGKW.Columns.Add(Texxt(955), Texxt(955))
    flgGKW.Columns(0).Width = ColWidth

    For i = 1 To NPS
      flgGKW.Columns.Add(Texxt(Itext(i - 1)), Texxt(Itext(i - 1)))
      flgGKW.Columns(i).Width = ColWidth
    Next i


    For i = 0 To AnzIart * 3 - 1
      j = Int(i / 3)
      k = i Mod 3
      flgGKW.Rows.Add()
      flgGKW.Rows(i).HeaderCell.Value = Texxt(980 + j) & "  " & Texxt(952 + k)
      For j = 1 To NPS
        flgGKW.Rows(i).Cells(j).Value = ""
      Next j
    Next i
    '
    '
    '
    '
    '
    RowsGRD = TblGRD.Select("GKWRT_ID=" & GkwrtID, "FRART_NR")
    Count = RowsGRD.Count
    For k = 0 To AnzIart - 1
      For j = 0 To Count - 1
        If RowsGRD(j)("FRART_NR") = k Then
          Exit For
        End If
      Next
      If j = Count Then
        Call GridStart(k, NPS, CDE, Koppwrt, Start, DaWrt, Kopp)
        ExtraRow = TblGRD.NewRow
        ExtraRow("GKWRT_ID") = GkwrtID
        ExtraRow("FRART_NR") = k
        ExtraRow("Koppwrt") = Koppwrt
        ExtraRow("start1") = Start(0)
        ExtraRow("start2") = Start(1)
        ExtraRow("start3") = Start(2)
        ExtraRow("start4") = Start(3)
        ExtraRow("start5") = Start(4)
        ExtraRow("start6") = Start(5)
        ExtraRow("dawrt1") = DaWrt(0)
        ExtraRow("dawrt2") = DaWrt(1)
        ExtraRow("dawrt3") = DaWrt(2)
        ExtraRow("dawrt4") = DaWrt(3)
        ExtraRow("dawrt5") = DaWrt(4)
        ExtraRow("dawrt6") = DaWrt(5)
        ExtraRow("kopp1") = Kopp(0)
        ExtraRow("kopp2") = Kopp(1)
        ExtraRow("kopp3") = Kopp(2)
        ExtraRow("kopp4") = Kopp(3)
        ExtraRow("kopp5") = Kopp(4)
        ExtraRow("kopp6") = Kopp(5)
        TblGRD.Rows.Add(ExtraRow)
      End If
    Next k
    RowsGRD = TblGRD.Select("GKWRT_ID=" & GkwrtID, "FRART_NR")

    For k = 0 To AnzIart - 1
      '
      '
      '
      If NewCDE Then
        Call GridStart(k, NPS, CDE, Koppwrt, Start, DaWrt, Kopp)
        RowsGRD(k)("Koppwrt") = Koppwrt
        RowsGRD(k)("start1") = Start(0)
        RowsGRD(k)("start2") = Start(1)
        RowsGRD(k)("start3") = Start(2)
        RowsGRD(k)("start4") = Start(3)
        RowsGRD(k)("start5") = Start(4)
        RowsGRD(k)("start6") = Start(5)
        RowsGRD(k)("dawrt1") = DaWrt(0)
        RowsGRD(k)("dawrt2") = DaWrt(1)
        RowsGRD(k)("dawrt3") = DaWrt(2)
        RowsGRD(k)("dawrt4") = DaWrt(3)
        RowsGRD(k)("dawrt5") = DaWrt(4)
        RowsGRD(k)("dawrt6") = DaWrt(5)
        RowsGRD(k)("kopp1") = Kopp(0)
        RowsGRD(k)("kopp2") = Kopp(1)
        RowsGRD(k)("kopp3") = Kopp(2)
        RowsGRD(k)("kopp4") = Kopp(3)
        RowsGRD(k)("kopp5") = Kopp(4)
        RowsGRD(k)("kopp6") = Kopp(5)

      Else
        Koppwrt = RowsGRD(k)("Koppwrt")
        Start(0) = RowsGRD(k)("start1")
        Start(1) = RowsGRD(k)("start2")
        Start(2) = RowsGRD(k)("start3")
        Start(3) = RowsGRD(k)("start4")
        Start(4) = RowsGRD(k)("start5")
        Start(5) = RowsGRD(k)("start6")
        DaWrt(0) = RowsGRD(k)("dawrt1")
        DaWrt(1) = RowsGRD(k)("dawrt2")
        DaWrt(2) = RowsGRD(k)("dawrt3")
        DaWrt(3) = RowsGRD(k)("dawrt4")
        DaWrt(4) = RowsGRD(k)("dawrt5")
        DaWrt(5) = RowsGRD(k)("dawrt6")
        Kopp(0) = HandleNullString(RowsGRD(k)("kopp1"))
        Kopp(1) = HandleNullString(RowsGRD(k)("kopp2"))
        Kopp(2) = HandleNullString(RowsGRD(k)("kopp3"))
        Kopp(3) = HandleNullString(RowsGRD(k)("kopp4"))
        Kopp(4) = HandleNullString(RowsGRD(k)("kopp5"))
        Kopp(5) = HandleNullString(RowsGRD(k)("kopp6"))
      End If
      '
      If CnzDep(CDE) <> 0 Then
        DaWrt(2) = " "
        Kopp(2) = " "
        DaWrt(3) = " "
        Kopp(3) = " "
      End If
      '
      '
      'Startwerte
      '
      flgGKW.Rows(3 * k).Cells(0).Value = Koppwrt

      For i = 1 To NPS
        flgGKW.Rows(3 * k).Cells(i).Value = CStr(Start(i - 1))
      Next i
      '
      '
      'Dämpfung
      '
      For i = 1 To NPS
        flgGKW.Rows(3 * k + 1).Cells(i).Value = CStr(DaWrt(i - 1))
      Next i
      '
      'Kopplungsart
      '
      For i = 1 To NPS
        flgGKW.Rows(3 * k + 2).Cells(i).Value = Kopp(i - 1)
      Next i

    Next k
    Application.DoEvents()
  End Sub
  Sub GridSave()
    Dim i As Integer
    Dim k As Integer

    If RowsGRD Is Nothing Then Exit Sub
    If RowsGRD.Count = 0 Then Exit Sub
    If flgGKW.Rows.Count = 0 Then Exit Sub

    NPS = flgGKW.Columns.Count - 1
    '
    '
    '
    '



    For k = 0 To AnzIart - 1
      '
      Koppwrt = CSng(flgGKW.Rows(3 * k).Cells(0).Value)

      For i = 1 To NPS
        Start(i - 1) = CSng(flgGKW.Rows(3 * k).Cells(i).Value)
      Next i
      '
      '
      'Dämpfung
      '
      For i = 1 To NPS
        If IsNumeric(flgGKW.Rows(3 * k + 1).Cells(i).Value) Then
          DaWrt(i - 1) = CSng(flgGKW.Rows(3 * k + 1).Cells(i).Value)
        Else
          DaWrt(i - 1) = 0
        End If
      Next i
      '
      'Kopplungsart
      '
      For i = 1 To NPS
        Kopp(i - 1) = flgGKW.Rows(3 * k + 2).Cells(i).Value
      Next i

      '

      '
      '
      RowsGRD(k)("Koppwrt") = Koppwrt
      RowsGRD(k)("start1") = Start(0)
      RowsGRD(k)("start2") = Start(1)
      RowsGRD(k)("start3") = Start(2)
      RowsGRD(k)("start4") = Start(3)
      RowsGRD(k)("start5") = Start(4)
      RowsGRD(k)("start6") = Start(5)
      RowsGRD(k)("dawrt1") = DaWrt(0)
      RowsGRD(k)("dawrt2") = DaWrt(1)
      RowsGRD(k)("dawrt3") = DaWrt(2)
      RowsGRD(k)("dawrt4") = DaWrt(3)
      RowsGRD(k)("dawrt5") = DaWrt(4)
      RowsGRD(k)("dawrt6") = DaWrt(5)
      RowsGRD(k)("kopp1") = Kopp(0)
      RowsGRD(k)("kopp2") = Kopp(1)
      RowsGRD(k)("kopp3") = Kopp(2)
      RowsGRD(k)("kopp4") = Kopp(3)
      RowsGRD(k)("kopp5") = Kopp(4)
      RowsGRD(k)("kopp6") = Kopp(5)
      '

      '
      '
      'Startwerte
      '


    Next k

  End Sub
  
  

  Private Sub ConnRZP_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnRZP.CurrentChanged
    If ConnMET.Current Is Nothing OrElse ConnMET.Count = 0 Then Exit Sub
    If ConnRZP.Current Is Nothing OrElse ConnRZP.Count = 0 Then Exit Sub
    ConnMET.Current("REZMN_ID") = ConnRZP.Current("REZMN_ID")
  End Sub

  Private Sub ConnRZP_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.BindingManagerDataErrorEventArgs) Handles ConnRZP.DataError
    MsgBox("CONNRZPERROR")
  End Sub

  Private Sub ConnRZP_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnRZP.AddingNew
    Dim i As Integer
    For i = 0 To TblRZP.Columns.Count - 1
      TblRZP.Columns(i).DefaultValue = ConnRZP.Current(i)
    Next i
    MaxRzpID = MaxDatTableID(TblRZP, "REZMN_ID", {""}, {-1}) + 1
    TblRZP.Columns("REZMN_ID").DefaultValue = MaxRzpID
    TblRZP.Columns("REZMN_BEZ").DefaultValue = TblRZP.Columns("REZMN_BEZ").DefaultValue & " ?????"
    If TblRZP.Columns("REZMN_BEZ").DefaultValue.length > TblRZP.Columns("REZMN_BEZ").MaxLength Then
      TblRZP.Columns("REZMN_BEZ").DefaultValue = TblRZP.Columns("REZMN_BEZ").DefaultValue.substring(0, TblRZP.Columns("REZMN_BEZ").MaxLength)
    End If

  End Sub

  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    Dim i As Integer
    Dim OrgRows() As DataRow
    Dim CurRows() As DataRow
    Dim WhereKeyID() As String
    If AddDelP(2990) Then
      '
      'TBL_REZMN
      '
      ConnRZP.CurrencyManager.EndCurrentEdit()
      ConnMET.CurrencyManager.EndCurrentEdit()
      ConnMSG.CurrencyManager.EndCurrentEdit()
      ConnMSY.CurrencyManager.EndCurrentEdit()
      ConnGKW.CurrencyManager.EndCurrentEdit()
      flgGKExtra.EndEdit()
      ViewEXT.EndInit()
      '
      'Insertcommand

      '
      '
      AdaptRZP.InsertCommand = OleDBInsertCmd("TBL_REZMN", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "REZMN_ID"
      AdaptRZP.UpdateCommand = OleDBUpdateCmd("TBL_REZMN", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptRZP.DeleteCommand = OleDBDeleteCmd("TBL_REZMN", WhereKeyID, Cncol)
      '
      '
      '
      'TBL_USER_MISCH 
      '
      '
      '
      '

      '
      '
      'Insertcommand

      '
      '
      AdaptMSY.InsertCommand = OleDBInsertCmd("TBL_USER_MISCH", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MISCH_ID"
      AdaptMSY.UpdateCommand = OleDBUpdateCmd("TBL_USER_MISCH", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptMSY.DeleteCommand = OleDBDeleteCmd("TBL_USER_MISCH", WhereKeyID, Cncol)
      '




      '
      '
      '
      'TBL_USER_METH_MISCH 
      '
      '
      '
      '

      '
      '
      'Insertcommand

      '
      '
      AdaptMET.InsertCommand = OleDBInsertCmd("TBL_USER_METH_MISCH", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "MISCH_ID"
      AdaptMET.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_MISCH", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptMET.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_MISCH", WhereKeyID, Cncol)
      '
      '
      'Delete/Update/Insert TBL_REZMN und TBL_USER_METH_MISCH
      '

      'Delete TBL_USER_MISCH
      '
      AdaptMSY.Update(TblMSY.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '

      'Delete TBL_USER_METH_MISCH
      '
      AdaptMET.Update(TblMET.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Delete TBL_REZMN
      '
      AdaptRZP.Update(TblRZP.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Insert TBL_REZMN
      '
      '
      AdaptRZP.Update(TblRZP.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Insert TBL_USER_MISCH
      '
      '
      AdaptMSY.Update(TblMSY.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Insert TBL_USER_METH_MISCH
      '
      '
      AdaptMET.Update(TblMET.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      'Update TBL_REZMN
      '
      AdaptRZP.Update(TblRZP.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_USER_MISCH
      '
      AdaptMSY.Update(TblMSY.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_USER_METH_MISCH
      '
      AdaptMET.Update(TblMET.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
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
      'TBL_GRUSTART
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptGRD.InsertCommand = OleDBInsertCmd("TBL_GRUSTART", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "GKWRT_ID"
      WhereKeyID(1) = "FRART_NR"
      AdaptGRD.UpdateCommand = OleDBUpdateCmd("TBL_GRUSTART", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptGRD.DeleteCommand = OleDBDeleteCmd("TBL_GRUSTART", WhereKeyID, Cncol)
      ''
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
      'TBL_USER_MISCH_MESSG
      '
      '
      '
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptMSG.InsertCommand = OleDBInsertCmd("TBL_USER_MISCH_MESSG", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MESSG_ID"
      WhereKeyID(2) = "MISCH_ID"
      AdaptMSG.UpdateCommand = OleDBUpdateCmd("TBL_USER_MISCH_MESSG", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptMSG.DeleteCommand = OleDBDeleteCmd("TBL_USER_MISCH_MESSG", WhereKeyID, Cncol)
      '
      '
      'Delete/Update/Insert TBL_GKWRT, TBL_GRUSTART und TBL_USER_MISCH_MESSG

      'Delete TBL_USER_MISCH_MESSG
      '
      AdaptMSG.Update(TblMSG.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Delete TBL_GRUSTART
      '
      AdaptGRD.Update(TblGRD.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      'Delete TBL_GKWRTEXT
      '
      AdaptEXT.Update(TblEXT.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Delete TBL_GKWRT
      '
      AdaptGKW.Update(TblGKW.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      'Insert TBL_GKWRT
      '
      '
      AdaptGKW.Update(TblGKW.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Insert TBL_GRUSTART
      '
      '
      AdaptGRD.Update(TblGRD.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Insert TBL_GKWRTEXT
      '
      '
      AdaptEXT.Update(TblEXT.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      'Insert TBL_USER_MISCH_MESSG
      '
      '
      AdaptMSG.Update(TblMSG.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      'Update TBL_GKWRT
      '
      AdaptGKW.Update(TblGKW.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      'Update TBL_GRUSTART
      '
      AdaptGRD.Update(TblGRD.Select(Nothing, "FRART_NR", DataViewRowState.ModifiedCurrent))
      '
      'Update TBL_GKWRTEXT
      '
      AdaptEXT.Update(TblEXT.Select(Nothing, "IHRM_ID", DataViewRowState.ModifiedCurrent))
      '
      'Update TBL_USER_MISCH_MESSG
      '
      '
      'Prüfen, ob GKWRT_ID geändert wurde
      '
      '
      OrgRows = TblMSG.Select(Nothing, Nothing, DataViewRowState.OriginalRows)
      CurRows = TblMSG.Select(Nothing, Nothing, DataViewRowState.CurrentRows)
      For i = 0 To OrgRows.Count - 1
        If OrgRows(i)("GKWRT_ID") <> CurRows(i)("GKWRT_ID") Then
          imsg = MsgBox(Texxt(2920) & Chr(13) & Texxt(2921), 4, Texxt(2000))
          If imsg = 7 Then
            Exit Sub
          End If
        End If
      Next
      AdaptMSG.Update(TblMSG.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      '
      '
      '
    End If
    Me.Close()
    If ConnGKW.Current("GKWRT_ID") = 0 Then
      MsgBox(Texxt(512), MsgBoxStyle.OkOnly, Texxt(2010))
    End If
  End Sub


  Private Sub KopierenToolStripMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles KopierenToolStripMenuItem.Click
    Clipboard.Clear()
    If flgGKW.Visible Then
      '
      'Tabellen für R-Werte oder Grunddaten in Zwischenablage
      '
      '
      '

      flgGKW.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
      flgGKW.SelectAll()

      If flgGKW.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
        Clipboard.SetDataObject(flgGKW.GetClipboardContent)
      End If
      flgGKW.ClearSelection()
    End If
  End Sub


  Private Sub EinfügenToolStripMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles EinfügenToolStripMenuItem.Click
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim TextSplit() As String
    Dim IanzFlg As Integer
    If flgGKW.Visible Then
      '
      'Tabellen für R-Werte oder Grunddaten in Zwischenablage
      '
      '
      '
      IanzFlg = (flgGKW.Rows.Count + 1) * (flgGKW.Columns.Count + 1)
      TextSplit = Clipboard.GetDataObject.GetData(DataFormats.Text, True).Split(Chr(9), Chr(13) & Chr(10))
      If TextSplit.Length = IanzFlg Then
        flgGKW.TopLeftHeaderCell.Value = TextSplit(0)
        k = 0
        For i = 0 To flgGKW.Columns.Count - 1
          k = k + 1
          flgGKW.Columns(i).HeaderText = TextSplit(k)
        Next
        For j = 0 To flgGKW.Rows.Count - 1
          k = k + 1
          flgGKW.Rows(j).HeaderCell.Value = TextSplit(k).Replace(Chr(10), "")
          For i = 0 To flgGKW.Columns.Count - 1
            k = k + 1
            flgGKW.Rows(j).Cells(i).Value = TextSplit(k)
          Next i
        Next j
      Else
        MsgBox(Texxt(16))
      End If
    End If
  End Sub

  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
    Iopenart = True
    MnUserid = -1
    MnMessgID = -1
    MnMethID = -1
    MnMischid = -1
    MnTblMisch = ""
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
 
  Private Sub flgGKW_Scroll(sender As Object, e As System.Windows.Forms.ScrollEventArgs) Handles flgGKW.Scroll
    Dim i As Integer
    For i = 0 To cboGKW.Count - 1
      cboGKW(i).Visible = False
    Next i
  End Sub
  Private Sub chkGKExtra_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkGKExtra.CheckedChanged
    flgGKW.Visible = Not chkGKExtra.Checked
    flgGKExtra.Visible = chkGKExtra.Checked
  End Sub
  Private Sub flgGKExtra_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles flgGKExtra.DataError
    e.Cancel = False
    MsgBox(e.Exception.Message)
  End Sub

  Private Sub btnGKSpei0_Click(sender As Object, e As System.EventArgs) Handles btnGKSpei0.Click
    Call SaveGK(ConnGKW.Current("GKWRT_ID"), 0, TblGKW)

    Call SaveGRD(ConnGKW.Current("GKWRT_ID"), 0)
    Call SaveGKExtra(ConnGKW.Current("GKWRT_ID"), 0, TblEXT, ViewEXT)
  End Sub

  
 
End Class
