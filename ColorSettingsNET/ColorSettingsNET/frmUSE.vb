Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmUSE
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
  Dim StrLinMet As String
  Dim WhereKeyID() As String
  Dim MnIopenArt As Boolean
  Dim MnUserUfo As Boolean
  '
  '
  Dim dsUSE As DataSet
  Dim RelMETH As DataRelation
  Dim RelMESG As DataRelation
  Dim RelMISG As DataRelation
  Dim RelUseMetLi As DataRelation
  Dim RelUseMetMesIh As DataRelation
  Dim RelUseMetAn As DataRelation
  Dim RelUseMetPa As DataRelation
  Dim RelUseMetMes As DataRelation
  Dim RelUseMetMis As DataRelation
  Dim RelUseMetAnMrk As DataRelation
  Dim RelUseMisMes As DataRelation
  Dim RelUseMesGrpRe As DataRelation
  Dim RelUseMesGrpDo As DataRelation
  Dim RelUseMisGrpRe As DataRelation
  Dim RelUseMisGrpDo As DataRelation
  Dim ParentColumns() As DataColumn
  Dim ChildColumns() As DataColumn


  Dim TblUSE As DataTable
  Dim TblMETB As DataTable
  Dim TblMETH As DataTable
  Dim TblMESB As DataTable
  Dim TblMESG As DataTable
  Dim TblMISB As DataTable
  Dim TblMISG As DataTable
  Dim TblMetAn As DataTable
  Dim TblMesIh As DataTable
  Dim TblUseMetLi As DataTable
  Dim TblUseMetMesIh As DataTable
  Dim TblUseMetAn As DataTable
  Dim TblUseMetPa As DataTable
  Dim TblUseMetMes As DataTable
  Dim TblUseMetMis As DataTable
  Dim TblUseMetAnMrk As DataTable
  Dim TblUseMisMes As DataTable
  Dim TblUseMesGrpRe As DataTable
  Dim TblUseMesGrpDo As DataTable
  Dim TblUseMisGrpRe As DataTable
  Dim TblUseMisGrpDo As DataTable
  '
  Dim CmdUSE As OleDbCommand
  Dim CmdMETB As OleDbCommand
  Dim CmdMETH As OleDbCommand
  Dim CmdMESB As OleDbCommand
  Dim CmdMESG As OleDbCommand
  Dim CmdMISB As OleDbCommand
  Dim CmdMISG As OleDbCommand
  Dim CmdMetAn As OleDbCommand
  Dim CmdMesIh As OleDbCommand
  Dim CmdUseMetLi As OleDbCommand
  Dim CmdUseMetMesIh As OleDbCommand
  Dim CmdUseMetAn As OleDbCommand
  Dim CmdUseMetPa As OleDbCommand
  Dim CmdUseMetMes As OleDbCommand
  Dim CmdUseMetMis As OleDbCommand
  Dim CmdUseMetAnMrk As OleDbCommand
  Dim CmdUseMisMes As OleDbCommand
  Dim CmdUseMesGrpRe As OleDbCommand
  Dim CmdUseMesGrpDo As OleDbCommand
  Dim CmdUseMisGrpRe As OleDbCommand
  Dim CmdUseMisGrpDo As OleDbCommand
  '
  '
  '
  '
  '
  Dim AdaptUSE As OleDbDataAdapter
  Dim AdaptMETB As OleDbDataAdapter
  Dim AdaptMETH As OleDbDataAdapter
  Dim AdaptMESB As OleDbDataAdapter
  Dim AdaptMESG As OleDbDataAdapter
  Dim AdaptMISB As OleDbDataAdapter
  Dim AdaptMISG As OleDbDataAdapter
  Dim AdaptMetAn As OleDbDataAdapter
  Dim AdaptMesIh As OleDbDataAdapter
  Dim AdaptUseMetLi As OleDbDataAdapter
  Dim AdaptUseMetMesIh As OleDbDataAdapter
  Dim AdaptUseMetAn As OleDbDataAdapter
  Dim AdaptUseMetPa As OleDbDataAdapter
  Dim AdaptUseMetMes As OleDbDataAdapter
  Dim AdaptUseMetMis As OleDbDataAdapter
  Dim AdaptUseMetAnMrk As OleDbDataAdapter
  Dim AdaptUseMisMes As OleDbDataAdapter
  Dim AdaptUseMesGrpRe As OleDbDataAdapter
  Dim AdaptUseMesGrpDo As OleDbDataAdapter
  Dim AdaptUseMisGrpRe As OleDbDataAdapter
  Dim AdaptUseMisGrpDo As OleDbDataAdapter
  '
  Dim Adapt As List(Of OleDbDataAdapter)
  '
  '
  Dim ViewHilf As DataView
  '
  Dim ViewMETH As DataView
  Dim ViewMESG As DataView
  Dim ViewMISG As DataView
  Dim ViewMetAn As DataView
  Dim ViewMesIh As DataView
  Dim ViewUseMetLi As DataView
  Dim ViewUseMetMesIh As DataView
  Dim ViewUseMetAn As DataView
  Dim ViewUseMetPa As DataView
  Dim ViewUseMetMes As DataView
  Dim ViewUseMetMis As DataView
  Dim ViewUseMetAnMrk As DataView
  Dim ViewUseMisMes As DataView
  Dim ViewUseMesGrpRe As DataView
  Dim ViewUseMesGrpDo As DataView
  Dim ViewUseMisGrpRe As DataView
  Dim ViewUseMisGrpDo As DataView
  '
  '
  Dim WithEvents ConnUSE As BindingSource
  Dim WithEvents ConnMETH As BindingSource
  Dim WithEvents ConnMESG As BindingSource
  Dim WithEvents ConnMISG As BindingSource
  Dim chkvis As List(Of CheckBox)

  Dim chkEnb As List(Of CheckBox)
  Dim chkWri As List(Of CheckBox)
  Dim chkDRM As List(Of CheckBox)
  Dim chkSon As List(Of CheckBox)
  Dim optDRU As List(Of RadioButton)
  Dim optDRR As List(Of RadioButton)


  Private Sub frmUSE_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    '
    'Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    'Me.WindowState = FormWindowState.Maximized
    '
    TblUSE = New DataTable("TblUSE")
    TblMETB = New DataTable("TblMETB")
    TblMETH = New DataTable("TblMETH")
    TblMESB = New DataTable("TblMESB")
    TblMESG = New DataTable("TblMESG")
    TblMISB = New DataTable("TblMISB")
    TblMISG = New DataTable("TblMISG")
    TblMetAn = New DataTable("TblMetAn")
    TblMesih = New DataTable("TblMesIh")
    TblUseMetLi = New DataTable("TblUseMetLi")
    TblUseMetMesIh = New DataTable("TblUseMetMesIh")
    TblUseMetAn = New DataTable("TblUseMetAn")
    TblUseMetPa = New DataTable("TblUseMetPa")
    TblUseMetMes = New DataTable("TblUseMetMes")
    TblUseMetMis = New DataTable("TblUseMetMis")
    TblUseMetAnMrk = New DataTable("TblUseMetAnMrk")
    TblUseMisMes = New DataTable("TblUseMisMes")
    TblUseMesGrpRe = New DataTable("TblUseMesGrpRe")
    TblUseMesGrpDo = New DataTable("TblUseMesGrpDo")
    TblUseMisGrpRe = New DataTable("TblUseMisGrpRe")
    TblUseMisGrpDo = New DataTable("TblUseMisGrpDo")
    '
    '
    '
    CmdUSE = New OleDbCommand("", Cncol)
    CmdMETB = New OleDbCommand("", Cncol)
    CmdMETH = New OleDbCommand("", Cncol)
    CmdMESB = New OleDbCommand("", Cncol)
    CmdMESG = New OleDbCommand("", Cncol)
    CmdMISB = New OleDbCommand("", Cncol)
    CmdMISG = New OleDbCommand("", Cncol)
    CmdMetAn = New OleDbCommand("", Cncol)
    CmdMesIh = New OleDbCommand("", Cncol)
    CmdUseMetLi = New OleDbCommand("", Cncol)
    CmdUseMetMesIh = New OleDbCommand("", Cncol)
    CmdUseMetAn = New OleDbCommand("", Cncol)
    CmdUseMetPa = New OleDbCommand("", Cncol)
    CmdUseMetMes = New OleDbCommand("", Cncol)
    CmdUseMetMis = New OleDbCommand("", Cncol)
    CmdUseMetAnMrk = New OleDbCommand("", Cncol)
    CmdUseMisMes = New OleDbCommand("", Cncol)
    CmdUseMesGrpRe = New OleDbCommand("", Cncol)
    CmdUseMesGrpDo = New OleDbCommand("", Cncol)
    CmdUseMisGrpRe = New OleDbCommand("", Cncol)
    CmdUseMisGrpDo = New OleDbCommand("", Cncol)
    '
    '
   
    AdaptUSE = New OleDbDataAdapter
    AdaptMETB = New OleDbDataAdapter
    AdaptMETH = New OleDbDataAdapter
    AdaptMESB = New OleDbDataAdapter
    AdaptMESG = New OleDbDataAdapter
    AdaptMISB = New OleDbDataAdapter
    AdaptMISG = New OleDbDataAdapter
    AdaptMetAn = New OleDbDataAdapter
    AdaptMesIh = New OleDbDataAdapter
    AdaptUseMetLi = New OleDbDataAdapter
    AdaptUseMetMesIh = New OleDbDataAdapter
    AdaptUseMetAn = New OleDbDataAdapter
    AdaptUseMetPa = New OleDbDataAdapter
    AdaptUseMetMes = New OleDbDataAdapter
    AdaptUseMetMis = New OleDbDataAdapter
    AdaptUseMetAnMrk = New OleDbDataAdapter
    AdaptUseMisMes = New OleDbDataAdapter
    AdaptUseMesGrpRe = New OleDbDataAdapter
    AdaptUseMesGrpDo = New OleDbDataAdapter '
    AdaptUseMisGrpRe = New OleDbDataAdapter
    AdaptUseMisGrpDo = New OleDbDataAdapter '
    '
    AdaptUSE.SelectCommand = CmdUSE
    AdaptMETB.SelectCommand = CmdMETB
    AdaptMETH.SelectCommand = CmdMETH
    AdaptMESB.SelectCommand = CmdMESB
    AdaptMESG.SelectCommand = CmdMESG
    AdaptMISB.SelectCommand = CmdMISB
    AdaptMISG.SelectCommand = CmdMISG
    AdaptMetAn.SelectCommand = CmdMetAn
    AdaptMesIh.SelectCommand = CmdMesIh
    AdaptUseMetLi.SelectCommand = CmdUseMetLi
    AdaptUseMetMesIh.SelectCommand = CmdUseMetMesIh
    AdaptUseMetAn.SelectCommand = CmdUseMetAn
    AdaptUseMetPa.SelectCommand = CmdUseMetPa
    AdaptUseMetMes.SelectCommand = CmdUseMetMes
    AdaptUseMetMis.SelectCommand = CmdUseMetMis
    AdaptUseMetAnMrk.SelectCommand = CmdUseMetAnMrk
    AdaptUseMisMes.SelectCommand = CmdUseMisMes
    AdaptUseMesGrpRe.SelectCommand = CmdUseMesGrpRe
    AdaptUseMesGrpDo.SelectCommand = CmdUseMesGrpDo
    AdaptUseMisGrpRe.SelectCommand = CmdUseMisGrpRe
    AdaptUseMisGrpDo.SelectCommand = CmdUseMisGrpDo
    '
    Adapt = New List(Of OleDbDataAdapter)
    Adapt.Clear()
    '
    Adapt.Add(AdaptUSE)
    Adapt.Add(AdaptMETH)
    Adapt.Add(AdaptMESG)
    Adapt.Add(AdaptMISG)
    Adapt.Add(AdaptUseMetMis)
    Adapt.Add(AdaptUseMetMes)
    Adapt.Add(AdaptUseMisMes)
    Adapt.Add(AdaptUseMetLi)
    Adapt.Add(AdaptUseMetMesIh)
    Adapt.Add(AdaptUseMetAn)
    Adapt.Add(AdaptUseMetPa)
    Adapt.Add(AdaptUseMetAnMrk)
    Adapt.Add(AdaptUseMesGrpRe)
    Adapt.Add(AdaptUseMesGrpDo)
    Adapt.Add(AdaptUseMisGrpRe)
    Adapt.Add(AdaptUseMisGrpDo)
    '
    dsUSE = New DataSet
    dsUSE.Clear()
    '
    dsUSE.Tables.Add(TblUSE)
    dsUSE.Tables.Add(TblMETH)
    dsUSE.Tables.Add(TblMESG)
    dsUSE.Tables.Add(TblMISG)
    dsUSE.Tables.Add(TblUseMetMis)
    dsUSE.Tables.Add(TblUseMetMes)
    dsUSE.Tables.Add(TblUseMisMes)
    dsUSE.Tables.Add(TblUseMetLi)
    dsUSE.Tables.Add(TblUseMetMesIh)
    dsUSE.Tables.Add(TblUseMetAn)
    dsUSE.Tables.Add(TblUseMetPa)
    dsUSE.Tables.Add(TblUseMetAnMrk)
    dsUSE.Tables.Add(TblUseMesGrpRe)
    dsUSE.Tables.Add(TblUseMesGrpDo)
    dsUSE.Tables.Add(TblUseMisGrpRe)
    dsUSE.Tables.Add(TblUseMisGrpDo) '
    '
    ViewMETH = New DataView(TblMETH)
    ViewMESG = New DataView(TblMESG)
    ViewMISG = New DataView(TblMISG)
    ViewMetAn = New DataView(TblMetAn)
    ViewMesIh = New DataView(TblMesIh)
    ViewUseMetLi = New DataView(TblUseMetLi)
    ViewUseMetMesIh = New DataView(TblUseMetMesIh)
    ViewUseMetAn = New DataView(TblUseMetAn)
    ViewUseMetPa = New DataView(TblUseMetPa)
    ViewUseMetMes = New DataView(TblUseMetMes)
    ViewUseMetMis = New DataView(TblUseMetMis)
    ViewUseMetAnMrk = New DataView(TblUseMetAnMrk)
    ViewUseMisMes = New DataView(TblUseMisMes)
    ViewUseMesGrpRe = New DataView(TblUseMesGrpRe)
    ViewUseMesGrpDo = New DataView(TblUseMesGrpDo)
    ViewUseMisGrpRe = New DataView(TblUseMisGrpRe)
    ViewUseMisGrpDo = New DataView(TblUseMisGrpDo)
    '
    '
    ConnUSE = New BindingSource
    ConnMETH = New BindingSource
    ConnMESG = New BindingSource
    ConnMISG = New BindingSource
    '
    BindingUSE.BindingSource = ConnUSE
    '
    chkEnb = New List(Of CheckBox)
    chkEnb.Clear()
    chkEnb.Add(chkENB_00)
    chkEnb.Add(chkENB_01)
    chkEnb.Add(chkENB_02)
    chkEnb.Add(chkENB_03)
    chkEnb.Add(chkENB_04)
    chkEnb.Add(chkENB_05)
    chkEnb.Add(chkENB_06)
    chkEnb.Add(chkENB_07)
    chkEnb.Add(chkENB_08)
    chkEnb.Add(chkENB_09)
    chkEnb.Add(chkENB_10)
    chkEnb.Add(chkENB_11)
    chkEnb.Add(chkENB_12)
    chkEnb.Add(chkENB_13)
    chkEnb.Add(chkENB_14)
    chkEnb.Add(chkENB_15)
    chkEnb.Add(chkENB_16)
    chkEnb.Add(chkENB_17)
    chkEnb.Add(chkENB_18)
    chkEnb.Add(chkENB_19)
    chkEnb.Add(chkENB_20)
    chkEnb.Add(chkENB_21)
    chkEnb.Add(chkENB_22)
    chkEnb.Add(chkENB_23)
    chkEnb.Add(chkENB_24)
    chkEnb.Add(chkENB_25)
    chkEnb.Add(chkENB_26)
    chkEnb.Add(chkENB_27)
    chkEnb.Add(chkENB_28)
    chkEnb.Add(chkENB_29)
    chkEnb.Add(chkENB_30)
    For i = 0 To chkEnb.Count - 1
      chkEnb(i).Text = Texxt(CInt(chkEnb(i).Text))
    Next

    '
    chkvis = New List(Of CheckBox)
    chkvis.Clear()
    chkvis.Add(chkVIS_00)
    chkvis.Add(chkVIS_01)
    chkvis.Add(chkVIS_02)
    chkvis.Add(chkVIS_03)
    chkvis.Add(chkVIS_04)
    chkvis.Add(chkVIS_05)
    chkvis.Add(chkVIS_06)
    chkvis.Add(chkVIS_07)
    chkvis.Add(chkVIS_08)
    chkvis.Add(chkVIS_09)
    chkvis.Add(chkVIS_10)
    chkvis.Add(chkVIS_11)
    chkvis.Add(chkVIS_12)
    chkvis.Add(chkVIS_13)
    chkvis.Add(chkVIS_14)
    chkvis.Add(chkVIS_15)
    chkvis.Add(chkVIS_16)
    chkvis.Add(chkVIS_17)
    chkvis.Add(chkVIS_18)
    chkvis.Add(chkVIS_19)
    chkvis.Add(chkVIS_20)
    chkvis.Add(chkVIS_21)
    chkvis.Add(chkVIS_22)
    chkvis.Add(chkVIS_23)
    chkvis.Add(chkVIS_24)
    chkvis.Add(chkVIS_25)
    chkvis.Add(chkVIS_26)
    chkvis.Add(chkVIS_27)
    chkvis.Add(chkVIS_28)
    chkvis.Add(chkVIS_29)
    chkvis.Add(chkVIS_30)
    '
    '
    chkWri = New List(Of CheckBox)
    chkWri.Clear()
    chkWri.Add(chkWRI_00)
    chkWri.Add(chkWRI_01)
    chkWri.Add(chkWRI_02)
    chkWri.Add(chkWRI_03)
    chkWri.Add(chkWRI_04)
    chkWri.Add(chkWRI_05)
    chkWri.Add(chkWRI_06)
    chkWri.Add(chkWRI_07)
    chkWri.Add(chkWRI_08)
    chkWri.Add(chkWRI_09)
    chkWri.Add(chkWRI_10)
    chkWri.Add(chkWRI_11)
    chkWri.Add(chkWRI_12)
    chkWri.Add(chkWRI_13)
    chkWri.Add(chkWRI_14)
    chkWri.Add(chkWRI_15)
    chkWri.Add(chkWRI_16)
    chkWri.Add(chkWRI_17)
    chkWri.Add(chkWRI_18)
    chkWri.Add(chkWRI_19)
    chkWri.Add(chkWRI_20)
    chkWri.Add(chkWRI_21)
    chkWri.Add(chkWRI_22)
    chkWri.Add(chkWRI_23)
    chkWri.Add(chkWRI_24)
    chkWri.Add(chkWRI_25)
    chkWri.Add(chkWRI_26)
    chkWri.Add(chkWRI_27)
    chkWri.Add(chkWRI_28)
    chkWri.Add(chkWRI_29)
    chkWri.Add(chkWRI_30)

    For i = 0 To chkWri.Count - 1
      chkWri(i).Text = Texxt(CInt(chkWri(i).Text))
    Next
    '
    chkSon = New List(Of CheckBox)
    chkSon.Clear()
    chkSon.Add(chkSON_00)
    chkSon.Add(chkSON_01)
    chkSon.Add(chkSON_02)
    chkSon.Add(chkSON_03)
    chkSon.Add(chkSON_04)
    chkSon.Add(chkSON_05)
    chkSon.Add(chkSON_06)
    chkSon.Add(chkSON_07)
    chkSon.Add(chkSON_08)
    chkSon.Add(chkSON_09)
    chkSon.Add(chkSON_10)
    chkSon.Add(chkSON_11)
    chkSon.Add(chkSON_12)
    chkSon.Add(chkSON_13)
    chkSon.Add(chkSON_14)
    chkSon.Add(chkSON_15)
    chkSon.Add(chkSON_16)
    chkSon.Add(chkSON_17)
    chkSon.Add(chkSON_18)
    chkSon.Add(chkSON_19)
    chkSon.Add(chkSON_20)
    chkSon.Add(chkSON_21)
    chkSon.Add(chkSON_22)
    chkSon.Add(chkSON_23)
    chkSon.Add(chkSON_24)
    chkSon.Add(chkSON_25)
    For i = 0 To chkSon.Count - 1
      chkSon(i).Text = Texxt(CInt(chkSon(i).Text))
    Next

    chkDRM = New List(Of CheckBox)
    chkDRM.Clear()
    chkDRM.Add(chkDRM_00)
    chkDRM.Add(chkDRM_01)
    chkDRM.Add(chkDRM_02)
    chkDRM.Add(chkDRM_03)
    chkDRM.Add(chkDRM_04)
    chkDRM.Add(chkDRM_05)
    chkDRM.Add(chkDRM_06)
    chkDRM.Add(chkDRM_07)
    chkDRM.Add(chkDRM_08)
    chkDRM.Add(chkDRM_09)
    chkDRM.Add(chkDRM_10)
    chkDRM.Add(chkDRM_11)
    chkDRM.Add(chkDRM_12)
    chkDRM.Add(chkDRM_13)
    chkDRM.Add(chkDRM_14)
    chkDRM.Add(chkDRM_15)
    chkDRM.Add(chkDRM_16)
    chkDRM.Add(chkDRM_17)
    chkDRM.Add(chkDRM_18)
    chkDRM.Add(chkDRM_19)
    chkDRM.Add(chkDRM_20)
    chkDRM.Add(chkDRM_21)
    chkDRM.Add(chkDRM_22)
    chkDRM.Add(chkDRM_23)
    chkDRM.Add(chkDRM_24)
    chkDRM.Add(chkDRM_25)
    chkDRM.Add(chkDRM_26)
    chkDRM.Add(chkDRM_27)
    chkDRM.Add(chkDRM_28)
    chkDRM.Add(chkDRM_29)
    For i = 0 To chkDRM.Count - 1
      chkDRM(i).Text = Texxt(CInt(chkDRM(i).Text))
    Next '
    '
    '
    '
    optDRU = New List(Of RadioButton)
    optDRU.Clear()
    optDRU.Add(optDRU_0)
    optDRU.Add(optDRU_1)
    optDRU.Add(optDRU_2)
    optDRU.Add(optDRU_3)
    '
    '
    '
    '
    '
    optDRR = New List(Of RadioButton)
    optDRR.Clear()
    optDRR.Add(optDRR_0)
    optDRR.Add(optDRR_1)

    Me.Text = Texxt(279)
    btnORD.Text = Texxt(1999)
    lblUSI_0.Text = Texxt(269)
    lblUSI_1.Text = Texxt(270)
    lblUSI_2.Text = Texxt(271)
    lblVIS_0.Text = Texxt(2497)
    lblVIS_1.Text = Texxt(2495)
    lblVIS_2.Text = Texxt(2496)
    lblVIS_3.Text = Texxt(2497)
    lblVIS_4.Text = Texxt(2495)
    lblVIS_5.Text = Texxt(2496)
    lblDRU.Text = Texxt(2560)
    lblDRR.Text = Texxt(2550)
    lblMEC.Text = Texxt(234)
    lblMEE.Text = Texxt(238)
    lblMET.Text = Texxt(237)
    lblMES.Text = Texxt(233)
    lblMIS.Text = Texxt(235)
    lblMIC.Text = Texxt(236)
    TabUSE.TabPages(0).Text = Texxt(2553)
    TabUSE.TabPages(1).Text = Texxt(2554)
    TabUSE.TabPages(2).Text = Texxt(2555)
    TabUSE.TabPages(3).Text = Texxt(2556)

    optDRU_0.Text = Texxt(2561)
    optDRU_1.Text = Texxt(2562)
    optDRU_2.Text = Texxt(2563)
    optDRU_3.Text = Texxt(2564)

    optDRR_0.Text = Texxt(2551)
    optDRR_1.Text = Texxt(2552)
    '
    '
    If ConnOpen(Cncol) Then
      '
      '
      'TBL_USER
      '
      SqlStmt = "SELECT * FROM TBL_USER ORDER BY USER_NAME"
      '
      CmdUSE.CommandText = SqlStmt
      If Not FillDatset(AdaptUSE, TblUSE) Then
        Exit Sub
      End If
      TblUSE.AcceptChanges()
      'Insertcommand
      AdaptUSE.InsertCommand = OleDBInsertCmd("TBL_USER", Cncol)
      'Updatecommand
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "USER_ID"
      AdaptUSE.UpdateCommand = OleDBUpdateCmd("TBL_USER", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUSE.DeleteCommand = OleDBDeleteCmd("TBL_USER", WhereKeyID, Cncol)
      If TblUSE.Rows.Count = 0 Then
        For i = 0 To TblUSE.Columns.Count - 1
          Select Case TblUSE.Columns(i).Caption
            Case "USER_ID"
            Case "USER_NAME"
              TblUSE.Columns(i).DefaultValue = "????"
            Case "USER_PASSW"
              TblUSE.Columns(i).DefaultValue = "????"
            Case Else
              TblUSE.Columns(i).DefaultValue = 0
          End Select
        Next i
      End If
      '
      '
      'Methoden
      '
      '
      '
      
      If MnIopenArt And MnUserUfo Then
        SqlStmt = "SELECT * FROM TBL_METH WHERE METH_ID IN " _
          & "(1,2,3,4,5,6,7,8,9,10,11,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,40,47,48,49,50,51,52,53,54,55,56,57,70,71,72,73,74,75," _
          & "103,106,108,109,110,111) ORDER BY METH_ID"
      Else
        SqlStmt = "SELECT * FROM TBL_METH WHERE METH_ID IN " _
          & "(1,2,3,4,5,6,7,8,9,10,11,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,40,47,48,49,50,51,52,53,54,55,56,57," _
          & "103,106,108,109,110,111) ORDER BY METH_ID"
      End If
      CmdMETB.CommandText = SqlStmt
      If Not FillDatset(AdaptMETB, TblMETB) Then
        Exit Sub
      End If
      TblMETB.AcceptChanges()
      StrLinMet = StrLin(TblMETB, "METH_ID")
      '
      '
      'MESSGERÄTE
      '
      SqlStmt = "SELECT * FROM TBL_MESSG ORDER BY MESSG_KBEZ"
      '
      CmdMESB.CommandText = SqlStmt
      If Not FillDatset(AdaptMESB, TblMESB) Then
        Exit Sub
      End If
      TblMESB.AcceptChanges()
      '
      '
      '
      'MESSGERÄTE IHRM
      '
      SqlStmt = "SELECT * FROM TBL_MESSG_IHRM ORDER BY POS_ID"
      '
      CmdMesIh.CommandText = SqlStmt
      If Not FillDatset(AdaptMesIh, TblMesIh) Then
        Exit Sub
      End If
      TblMesIh.AcceptChanges()
      '
      '
      '
      '
      'MISCHSYSTEME
      '
      SqlStmt = "SELECT * FROM TBL_MISCH ORDER BY MISCH_KBEZ"
      '
      CmdMISB.CommandText = SqlStmt
      If Not FillDatset(AdaptMISB, TblMISB) Then
        Exit Sub
      End If
      TblMISB.AcceptChanges()
      '
      '
      'Userabhängige Methode auswählen
      '
      '    
      '
      'TBL_USER_METH 

      '
      '
      SqlStmt = "SELECT DISTINCT *,TBL_USER_METH.METH_ID AS METH_ID,METH_BEZ FROM TBL_USER_METH INNER JOIN TBL_METH ON TBL_USER_METH.METH_ID=TBL_METH.METH_ID" _
      & " WHERE TBL_USER_METH.METH_ID IN " & StrLinMet

      CmdMETH.CommandText = SqlStmt
      If Not FillDatset(AdaptMETH, TblMETH) Then
        Exit Sub
      End If
      TblMETH.AcceptChanges()
      '

      '
      '
      'Insertcommand
      AdaptMETH.InsertCommand = OleDBInsertCmd("TBL_USER_METH", Cncol)
      'Updatecommand
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      AdaptMETH.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH", WhereKeyID, Cncol)
      'Deletecommand
      AdaptMETH.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH", WhereKeyID, Cncol)
      '
      '    
      'Anweisungen
      'TBL_METH_ANWSG 
      '
      '
      '
      SqlStmt = "SELECT * FROM TBL_METH_ANWSG"

      CmdMetAn.CommandText = SqlStmt
      If Not FillDatset(AdaptMetAn, TblMetAn) Then
        Exit Sub
      End If
      TblMetAn.AcceptChanges() '
      'Userabhängige Messgeräte
      '
      ' 
      '
      'TBL_USER_MESSG 
      '
      '
      SqlStmt = "SELECT *,TBL_USER_MESSG.MESSG_ID AS MESSG_ID,TBL_USER_MESSG.MESSG_SOND AS MESSG_SOND,TBL_USER_MESSG.MESSG_INI AS MESSG_INI,TBL_USER_MESSG.MESSG_KAL AS MESSG_KAL,MESSG_KBEZ FROM TBL_USER_MESSG INNER JOIN TBL_MESSG ON TBL_MESSG.MESSG_ID=TBL_USER_MESSG.MESSG_ID"
      CmdMESG.CommandText = SqlStmt
      If Not FillDatset(AdaptMESG, TblMESG) Then
        Exit Sub
      End If
      TblMESG.AcceptChanges()
      'Insertcommand
      AdaptMESG.InsertCommand = OleDBInsertCmd("TBL_USER_MESSG", Cncol)
      'Updatecommand
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MESSG_ID"
      AdaptMESG.UpdateCommand = OleDBUpdateCmd("TBL_USER_MESSG", WhereKeyID, Cncol)
      'Deletecommand
      AdaptMESG.DeleteCommand = OleDBDeleteCmd("TBL_USER_MESSG", WhereKeyID, Cncol)
      '
      ''
      '
      'Userabhängige Mischsysteme
      '
      '
      '
      '
      'TBL_USER_MISCH
      '
      '
      SqlStmt = "SELECT *,TBL_USER_MISCH.MISCH_ID AS MISCH_ID,MISCH_KBEZ FROM TBL_USER_MISCH INNER JOIN TBL_MISCH ON TBL_MISCH.MISCH_ID=TBL_USER_MISCH.MISCH_ID"
      CmdMISG.CommandText = SqlStmt
      If Not FillDatset(AdaptMISG, TblMISG) Then
        Exit Sub
      End If
      TblMISG.AcceptChanges()
      'Insertcommand
      AdaptMISG.InsertCommand = OleDBInsertCmd("TBL_USER_MISCH", Cncol)
      'Updatecommand
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MISCH_ID"
      AdaptMISG.UpdateCommand = OleDBUpdateCmd("TBL_USER_MISCH", WhereKeyID, Cncol)
      'Deletecommand
      AdaptMISG.DeleteCommand = OleDBDeleteCmd("TBL_USER_MISCH", WhereKeyID, Cncol) '
      '
      '
      '
      '
      '
      'TBL_USER_METH_LICHT
      '
      '
      SqlStmt = "SELECT *,TBL_USER_METH_LICHT.USER_ID AS USER_ID,TBL_USER_METH_LICHT.METH_ID AS METH_ID FROM TBL_USER_METH_LICHT WHERE METH_ID IN " & StrLinMet

      CmdUseMetLi.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMetLi, TblUseMetLi) Then
        Exit Sub
      End If
      TblUseMetLi.AcceptChanges()
      'Insertcommand
      AdaptUseMetLi.InsertCommand = OleDBInsertCmd("TBL_USER_METH_LICHT", Cncol)
      'Updatecommand
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "LICHT_ID"
      AdaptUseMetLi.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_LICHT", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMetLi.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_LICHT", WhereKeyID, Cncol)
      ''
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"

      Call CheckRowsRelation(WhereKeyID, WhereKeyID, TblMETH.Select, TblUseMetLi.Select)
      '
      '
      'TBL_USER_METH_MESSG_IHRM
      '
      '
      '
      SqlStmt = "SELECT * FROM TBL_USER_METH_MESSG_IHRM WHERE METH_ID IN " & StrLinMet
      CmdUseMetMesIh.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMetMesIh, TblUseMetMesIh) Then
        Exit Sub
      End If
      TblUseMetMesIh.AcceptChanges()
      'Insertcommand
      AdaptUseMetMesIh.InsertCommand = OleDBInsertCmd("TBL_USER_METH_MESSG_IHRM", Cncol)
      'Updatecommand
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "MESSG_ID"
      AdaptUseMetMesIh.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_MESSG_IHRM", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMetMesIh.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_MESSG_IHRM", WhereKeyID, Cncol) '
      AdaptUseMetMesIh.DeleteCommand.CommandText = "DELETE * FROM TBL_USER_METH_MESSG_IHRM WHERE USER_ID=? AND (METH_ID=? OR MESSG_ID=?)"
      '
      'TBL_USER_METH_ANWSG
      '
      '
      '
      SqlStmt = "SELECT * FROM TBL_USER_METH_ANWSG WHERE METH_ID IN " & StrLinMet
      CmdUseMetAn.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMetAn, TblUseMetAn) Then
        Exit Sub
      End If
      TblUseMetAn.AcceptChanges()
      'Insertcommand
      AdaptUseMetAn.InsertCommand = OleDBInsertCmd("TBL_USER_METH_ANWSG", Cncol)
      'Updatecommand
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "ANWSG_ID"
      AdaptUseMetAn.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_ANWSG", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMetAn.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_ANWSG", WhereKeyID, Cncol) '
      '
      'TBL_USER_METH_PARAM
      '
      '
      SqlStmt = "SELECT * FROM TBL_USER_METH_PARAM WHERE METH_ID IN " & StrLinMet
      CmdUseMetPa.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMetPa, TblUseMetPa) Then
        Exit Sub
      End If
      TblUseMetPa.AcceptChanges()
      'Insertcommand
      AdaptUseMetPa.InsertCommand = OleDBInsertCmd("TBL_USER_METH_PARAM", Cncol)
      'Updatecommand
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "PARAM_ID"
      AdaptUseMetPa.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_PARAM", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMetPa.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_PARAM", WhereKeyID, Cncol) '
      '
      '
      '
      'TBL_USER_METH_MESSG
      '
      '
      SqlStmt = "SELECT * FROM TBL_USER_METH_MESSG WHERE METH_ID IN " & StrLinMet
      CmdUseMetMes.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMetMes, TblUseMetMes) Then
        Exit Sub
      End If
      TblUseMetMes.AcceptChanges()

      'Insertcommand
      AdaptUseMetMes.InsertCommand = OleDBInsertCmd("TBL_USER_METH_MESSG", Cncol)
      'Updatecommand
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "MESSG_ID"
      AdaptUseMetMes.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_MESSG", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMetMes.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_MESSG", WhereKeyID, Cncol) '
      AdaptUseMetMes.DeleteCommand.CommandText = "DELETE * FROM TBL_USER_METH_MESSG WHERE USER_ID=? AND (METH_ID=? OR MESSG_ID=?)"
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
      SqlStmt = "SELECT * FROM TBL_USER_METH_MISCH WHERE METH_ID IN " & StrLinMet
      CmdUseMetMis.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMetMis, TblUseMetMis) Then
        Exit Sub
      End If
      TblUseMetMis.AcceptChanges()
      'Insertcommand
      AdaptUseMetMis.InsertCommand = OleDBInsertCmd("TBL_USER_METH_MISCH", Cncol)
      'Updatecommand
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "MISCH_ID"
      AdaptUseMetMis.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_MISCH", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMetMis.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_MISCH", WhereKeyID, Cncol) '
      AdaptUseMetMis.DeleteCommand.CommandText = "DELETE * FROM TBL_USER_METH_MISCH WHERE USER_ID=? AND (METH_ID=? OR MISCH_ID=?)"
      '
      'TBL_USER_METH_ANWSG_MERK
      '
      '
      SqlStmt = "SELECT * FROM TBL_USER_METH_ANWSG_MERK WHERE METH_ID IN " & StrLinMet
      CmdUseMetAnMrk.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMetAnMrk, TblUseMetAnMrk) Then
        Exit Sub
      End If
      TblUseMetAnMrk.AcceptChanges()
      'Insertcommand
      AdaptUseMetAnMrk.InsertCommand = OleDBInsertCmd("TBL_USER_METH_ANWSG_MERK", Cncol)
      'Updatecommand
      ReDim WhereKeyID(3)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "ANWSG_ID"
      WhereKeyID(3) = "MERK_ID"
      AdaptUseMetAnMrk.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_ANWSG_MERK", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMetAnMrk.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_ANWSG_MERK", WhereKeyID, Cncol) '
      '
      '
      '
      'TBL_USER_MISCH_MESSG
      '
      '
      '
      '
      '
      SqlStmt = "SELECT * FROM TBL_USER_MISCH_MESSG"
      CmdUseMisMes.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMisMes, TblUseMisMes) Then
        Exit Sub
      End If
      TblUseMisMes.AcceptChanges()
      'Insertcommand
      AdaptUseMisMes.InsertCommand = OleDBInsertCmd("TBL_USER_MISCH_MESSG", Cncol)
      'Updatecommand
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MISCH_ID"
      WhereKeyID(2) = "MESSG_ID"
      AdaptUseMisMes.UpdateCommand = OleDBUpdateCmd("TBL_USER_MISCH_MESSG", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMisMes.DeleteCommand = OleDBDeleteCmd("TBL_USER_MISCH_MESSG", WhereKeyID, Cncol) '
      AdaptUseMisMes.DeleteCommand.CommandText = "DELETE * FROM TBL_USER_MISCH_MESSG WHERE USER_ID=? AND (MISCH_ID=? OR MESSG_ID=?)"
      '
      '
      '
      'TBL_USER_MESSG_GROUP_READONLY
      '
      '
      '
      SqlStmt = "SELECT * FROM TBL_USER_MESSG_GROUP_READONLY"
      CmdUseMesGrpRe.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMesGrpRe, TblUseMesGrpRe) Then
        Exit Sub
      End If
      TblUseMesGrpRe.AcceptChanges()
      'Insertcommand
      AdaptUseMesGrpRe.InsertCommand = OleDBInsertCmd("TBL_USER_MESSG_GROUP_READONLY", Cncol)
      'Updatecommand
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MESSG_ID"
      WhereKeyID(2) = "GROUP_ID"
      AdaptUseMesGrpRe.UpdateCommand = OleDBUpdateCmd("TBL_USER_MESSG_GROUP_READONLY", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMesGrpRe.DeleteCommand = OleDBDeleteCmd("TBL_USER_MESSG_GROUP_READONLY", WhereKeyID, Cncol) '
      '
      '
      '
      '
      '
      'TBL_USER_MESSG_GROUP_DONTSHOW
      '
      '
      ''
      '
      SqlStmt = "SELECT * FROM TBL_USER_MESSG_GROUP_DONTSHOW"
      CmdUseMesGrpDo.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMesGrpDo, TblUseMesGrpDo) Then
        Exit Sub
      End If
      TblUseMesGrpDo.AcceptChanges()
      'Insertcommand
      AdaptUseMesGrpDo.InsertCommand = OleDBInsertCmd("TBL_USER_MESSG_GROUP_DONTSHOW", Cncol)
      'Updatecommand
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MESSG_ID"
      WhereKeyID(2) = "GROUP_ID"
      AdaptUseMesGrpDo.UpdateCommand = OleDBUpdateCmd("TBL_USER_MESSG_GROUP_DONTSHOW", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMesGrpDo.DeleteCommand = OleDBDeleteCmd("TBL_USER_MESSG_GROUP_DONTSHOW", WhereKeyID, Cncol) '
      ' 
      '
      '
      '
      '
      '
      '
      '
      'TBL_USER_MISCH_GROUP_READONLY
      '
      '
      '
      SqlStmt = "SELECT * FROM TBL_USER_MISCH_GROUP_READONLY"
      CmdUseMisGrpRe.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMisGrpRe, TblUseMisGrpRe) Then
        Exit Sub
      End If
      TblUseMisGrpRe.AcceptChanges()
      'Insertcommand
      AdaptUseMisGrpRe.InsertCommand = OleDBInsertCmd("TBL_USER_MISCH_GROUP_READONLY", Cncol)
      'Updatecommand
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MISCH_ID"
      WhereKeyID(2) = "GROUP_ID"
      AdaptUseMisGrpRe.UpdateCommand = OleDBUpdateCmd("TBL_USER_MISCH_GROUP_READONLY", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMisGrpRe.DeleteCommand = OleDBDeleteCmd("TBL_USER_MISCH_GROUP_READONLY", WhereKeyID, Cncol) '
      '
      '
      '
      '
      '
      'TBL_USER_MISCH_GROUP_DONTSHOW
      '
      '
      '
      '
      '
      SqlStmt = "SELECT * FROM TBL_USER_MISCH_GROUP_DONTSHOW"
      CmdUseMisGrpDo.CommandText = SqlStmt
      If Not FillDatset(AdaptUseMisGrpDo, TblUseMisGrpDo) Then
        Exit Sub
      End If
      TblUseMisGrpDo.AcceptChanges()
      'Insertcommand
      AdaptUseMisGrpDo.InsertCommand = OleDBInsertCmd("TBL_USER_MISCH_GROUP_DONTSHOW", Cncol)
      'Updatecommand
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "MISCH_ID"
      WhereKeyID(2) = "GROUP_ID"
      AdaptUseMisGrpDo.UpdateCommand = OleDBUpdateCmd("TBL_USER_MISCH_GROUP_DONTSHOW", WhereKeyID, Cncol)
      'Deletecommand
      AdaptUseMisGrpDo.DeleteCommand = OleDBDeleteCmd("TBL_USER_MISCH_GROUP_DONTSHOW", WhereKeyID, Cncol) '


      '
      dsUSE.Relations.Clear()
      '
      RelMETH = New DataRelation("RELMETH", TblUSE.Columns("USER_ID"), TblMETH.Columns("USER_ID"))
      dsUSE.Relations.Add(RelMETH)
      RelMESG = New DataRelation("RELMESG", TblUSE.Columns("USER_ID"), TblMESG.Columns("USER_ID"))
      dsUSE.Relations.Add(RelMESG)
      RelMISG = New DataRelation("RELMISG", TblUSE.Columns("USER_ID"), TblMISG.Columns("USER_ID"))
      dsUSE.Relations.Add(RelMISG)
      RelUseMetLi = New DataRelation("RELUseMetLi", TblUSE.Columns("USER_ID"), TblUseMetLi.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMetLi)
      RelUseMetMesIh = New DataRelation("RELUseMetMesIh", TblUSE.Columns("USER_ID"), TblUseMetMesIh.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMetMesIh)
      RelUseMetAn = New DataRelation("RELUseMetAn", TblUSE.Columns("USER_ID"), TblUseMetAn.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMetAn)
      RelUseMetPa = New DataRelation("RELUseMetPa", TblUSE.Columns("USER_ID"), TblUseMetPa.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMetPa)
      RelUseMetMes = New DataRelation("RELUseMetMes", TblUSE.Columns("USER_ID"), TblUseMetMes.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMetMes)
      RelUseMetMis = New DataRelation("RELUseMetMis", TblUSE.Columns("USER_ID"), TblUseMetMis.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMetMis)
      RelUseMetAnMrk = New DataRelation("RELUseMetAnMrk", TblUSE.Columns("USER_ID"), TblUseMetAnMrk.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMetAnMrk)
      RelUseMisMes = New DataRelation("RELUseMisMes", TblUSE.Columns("USER_ID"), TblUseMisMes.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMisMes)
      RelUseMesGrpRe = New DataRelation("RELUseMesGrpRe", TblUSE.Columns("USER_ID"), TblUseMesGrpRe.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMesGrpRe)
      RelUseMesGrpDo = New DataRelation("RELUseMesGrpDo", TblUSE.Columns("USER_ID"), TblUseMesGrpDo.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMesGrpDo)
      RelUseMisGrpRe = New DataRelation("RELUseMisGrpRe", TblUSE.Columns("USER_ID"), TblUseMisGrpRe.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMisGrpRe)
      RelUseMisGrpDo = New DataRelation("RELUseMisGrpDo", TblUSE.Columns("USER_ID"), TblUseMisGrpDo.Columns("USER_ID"))
      dsUSE.Relations.Add(RelUseMisGrpDo)
      '
      ReDim ParentColumns(1)
      ParentColumns(0) = TblMETH.Columns("USER_ID")
      ParentColumns(1) = TblMETH.Columns("METH_ID")
      ReDim ChildColumns(1)
      ChildColumns(0) = TblUseMetMis.Columns("USER_ID")
      ChildColumns(1) = TblUseMetMis.Columns("METH_ID")
      Call CheckRelation(TblMETH, ParentColumns, TblUseMetMis, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMetMis", ParentColumns, ChildColumns))
      '
      ChildColumns(0) = TblUseMetMes.Columns("USER_ID")
      ChildColumns(1) = TblUseMetMes.Columns("METH_ID")
      Call CheckRelation(TblMETH, ParentColumns, TblUseMetMes, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMetMes", ParentColumns, ChildColumns))
      '
      ChildColumns(0) = TblUseMetLi.Columns("USER_ID")
      ChildColumns(1) = TblUseMetLi.Columns("METH_ID")
      Call CheckRelation(TblMETH, ParentColumns, TblUseMetLi, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMetLi", ParentColumns, ChildColumns))
      '
      '
      ChildColumns(0) = TblUseMetMesIh.Columns("USER_ID")
      ChildColumns(1) = TblUseMetMesIh.Columns("METH_ID")
      Call CheckRelation(TblMETH, ParentColumns, TblUseMetMesIh, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMetMesih", ParentColumns, ChildColumns))

      ChildColumns(0) = TblUseMetAn.Columns("USER_ID")
      ChildColumns(1) = TblUseMetAn.Columns("METH_ID")
      Call CheckRelation(TblMETH, ParentColumns, TblUseMetAn, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMetAn", ParentColumns, ChildColumns))

      ChildColumns(0) = TblUseMetPa.Columns("USER_ID")
      ChildColumns(1) = TblUseMetPa.Columns("METH_ID")
      Call CheckRelation(TblMETH, ParentColumns, TblUseMetPa, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMetPa", ParentColumns, ChildColumns))


      ChildColumns(0) = TblUseMetAnMrk.Columns("USER_ID")
      ChildColumns(1) = TblUseMetAnMrk.Columns("METH_ID")
      Call CheckRelation(TblMETH, ParentColumns, TblUseMetAnMrk, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMetAnMrk", ParentColumns, ChildColumns))
      '
      '
      '
      '
      ParentColumns(0) = TblMESG.Columns("USER_ID")
      ParentColumns(1) = TblMESG.Columns("MESSG_ID")
      ChildColumns(0) = TblUseMisMes.Columns("USER_ID")
      ChildColumns(1) = TblUseMisMes.Columns("MESSG_ID")
      Call CheckRelation(TblMESG, ParentColumns, TblUseMisMes, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMesMis", ParentColumns, ChildColumns))
      '
      '
      '
      ChildColumns(0) = TblUseMetMes.Columns("USER_ID")
      ChildColumns(1) = TblUseMetMes.Columns("MESSG_ID")
      Call CheckRelation(TblMESG, ParentColumns, TblUseMetMes, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMesMet", ParentColumns, ChildColumns))
      '
      '
      '
      ChildColumns(0) = TblUseMetMesIh.Columns("USER_ID")
      ChildColumns(1) = TblUseMetMesIh.Columns("MESSG_ID")
      Call CheckRelation(TblMESG, ParentColumns, TblUseMetMesIh, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMesMeth", ParentColumns, ChildColumns))
      '
      '
      '
      ChildColumns(0) = TblUseMesGrpRe.Columns("USER_ID")
      ChildColumns(1) = TblUseMesGrpRe.Columns("MESSG_ID")
      Call CheckRelation(TblMESG, ParentColumns, TblUseMesGrpRe, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMesGrpRe", ParentColumns, ChildColumns))
      '
      '
      '
      ChildColumns(0) = TblUseMesGrpDo.Columns("USER_ID")
      ChildColumns(1) = TblUseMesGrpDo.Columns("MESSG_ID")
      Call CheckRelation(TblMESG, ParentColumns, TblUseMesGrpDo, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMesGrpDo", ParentColumns, ChildColumns))
      '
      '
      '
      '
      ParentColumns(0) = TblMISG.Columns("USER_ID")
      ParentColumns(1) = TblMISG.Columns("MISCH_ID")
      ChildColumns(0) = TblUseMisMes.Columns("USER_ID")
      ChildColumns(1) = TblUseMisMes.Columns("MISCH_ID")
      Call CheckRelation(TblMISG, ParentColumns, TblUseMisMes, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMisMes", ParentColumns, ChildColumns))
      '
      '
      '
      '
      ChildColumns(0) = TblUseMetMis.Columns("USER_ID")
      ChildColumns(1) = TblUseMetMis.Columns("MISCH_ID")
      Call CheckRelation(TblMISG, ParentColumns, TblUseMetMis, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMisMet", ParentColumns, ChildColumns))
      '
      '
      '
      '
      '
      '
      ChildColumns(0) = TblUseMisGrpRe.Columns("USER_ID")
      ChildColumns(1) = TblUseMisGrpRe.Columns("MISCH_ID")
      Call CheckRelation(TblMISG, ParentColumns, TblUseMisGrpRe, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMisGrpRe", ParentColumns, ChildColumns))
      '
      '
      '
      ChildColumns(0) = TblUseMisGrpDo.Columns("USER_ID")
      ChildColumns(1) = TblUseMisGrpDo.Columns("MISCH_ID")
      Call CheckRelation(TblMISG, ParentColumns, TblUseMisGrpDo, ChildColumns)
      dsUSE.Relations.Add(New DataRelation("RELMisGrpDo", ParentColumns, ChildColumns))


      dsUSE.EnforceConstraints = True
      '
      '
      '
      '
      lstMESB.DataSource = TblMESB
      lstMESB.DisplayMember = "MESSG_KBEZ"
      lstMESB.ValueMember = "MESSG_ID"
      '
      '
      '
      lstMESG.DataSource = ViewMESG
      lstMESG.DisplayMember = "MESSG_KBEZ"
      lstMESG.ValueMember = "MESSG_ID"
      '
      '
      '
      lstMISB.DataSource = TblMISB
      lstMISB.DisplayMember = "MISCH_KBEZ"
      lstMISB.ValueMember = "MISCH_ID"
      '
      '
      '
      lstMISG.DataSource = ViewMISG
      lstMISG.DisplayMember = "MISCH_KBEZ"
      lstMISG.ValueMember = "MISCH_ID"
      '
      '
      '
      lstMETB.DataSource = TblMETB
      lstMETB.DisplayMember = "METH_BEZ"
      lstMETB.ValueMember = "METH_ID"
      '
      '
      '
      lstMETH.DataSource = ViewMETH
      lstMETH.DisplayMember = "METH_BEZ"
      lstMETH.ValueMember = "METH_ID"
      '
      '


      '
      '
      '
      '
      '
      '
      ConnMETH.DataSource = ViewMETH
      ConnMESG.DataSource = ViewMESG
      ConnMISG.DataSource = ViewMISG
      ConnUSE.DataSource = TblUSE
      '
      '
      cboUSE.DataSource = ConnUSE
      cboUSE.DisplayMember = "USER_NAME"
      cboUSE.ValueMember = "USER_ID" '
      '
      '
      txtUSI_0.DataBindings.Add("TEXT", ConnUSE, "USER_ID")
      txtUSI_1.DataBindings.Add("TEXT", ConnUSE, "USER_NAME")
      txtUSI_1.MaxLength = TblUSE.Columns("USER_NAME").MaxLength
      txtUSI_2.DataBindings.Add("TEXT", ConnUSE, "USER_PASSW")
      txtUSI_2.MaxLength = TblUSE.Columns("USER_PASSW").MaxLength
      txtENB.DataBindings.Add("TEXT", ConnUSE, "USER_ENABL")
      txtVIS.DataBindings.Add("TEXT", ConnUSE, "USER_VISBL")
      txtWRI.DataBindings.Add("TEXT", ConnUSE, "USER_WRITE")
      txtSON.DataBindings.Add("TEXT", ConnUSE, "USER_SONST")
      txtDRM.DataBindings.Add("TEXT", ConnUSE, "USER_DRUM")
      txtDRU.DataBindings.Add("TEXT", ConnUSE, "USER_DRUQ")
    End If
    Cncol.Close()
  End Sub

  '
  '
  '
  '
  Private Sub frmUSE_Paint(sender As Object, e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint
    TabUSE.ItemSize = New Size((TabUSE.Width - TabUSE.TabPages.Count) / TabUSE.TabPages.Count - 3, TabUSE.ItemSize.Height)
  End Sub
  Private Sub ConnUSE_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnUSE.AddingNew
    Dim i As Integer
    Dim Leng As Integer
    If Not ConnUSE.Current Is Nothing Then
      For i = 0 To TblUSE.Columns.Count - 1
        If Not TblUSE.Columns(i).AutoIncrement Then
          TblUSE.Columns(i).DefaultValue = ConnUSE.Current(i)
        End If
      Next i
    End If
    Leng = Min(TblUSE.Columns("USER_NAME").MaxLength - 4, TblUSE.Columns("USER_NAME").DefaultValue.length)
    TblUSE.Columns("USER_NAME").DefaultValue = TblUSE.Columns("USER_NAME").DefaultValue.substring(0, Leng) & " ????"
    If TblUSE.Columns("USER_NAME").DefaultValue.length > TblUSE.Columns("USER_NAME").MaxLength Then
      TblUSE.Columns("USER_NAME").DefaultValue = TblUSE.Columns("USER_NAME").DefaultValue.substring(0, TblUSE.Columns("USER_NAME").MaxLength)
    End If

  End Sub




  Private Sub ConnUSE_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnUSE.CurrentChanged
    '
    If ConnUSE.Current Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    '
    '
    'Methoden, Messgeräte und Mischsysteme werden von vorherigem User übernommen
    '
    '
    '
    If ConnUSE.Current.row.rowstate = DataRowState.Detached Then
      ConnUSE.CurrencyManager.EndCurrentEdit()
      '
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewMETH)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewMESG)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewMISG)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMetLi)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMetMesIh)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMetAn)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMetPa)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMetMes)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMetMis)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMetAnMrk)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMisMes)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMesGrpRe)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMesGrpDo)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMisGrpRe)
      Call AddNewViewItem(ConnUSE.Current("USER_ID"), "USER_ID", ViewUseMisGrpDo)
      '
      '
    End If
    '
    'Userabhängige Methode auswählen
    '

    If Not IsNothing(ConnMETH.Current) Then
      ConnMETH.Current.endedit()
    End If
    ViewMETH.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ConnMETH.CurrencyManager.Refresh()
    ConnMETH.Position = 0
    '
    'Userabhängige Messgeräte auswählen'
    '
    If Not IsNothing(ConnMESG.Current) Then
      ConnMESG.Current.endedit()
    End If
    ViewMESG.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ConnMESG.CurrencyManager.Refresh()
    ConnMESG.Position = 0
    '
    'Userabhängige Mischsysteme auswählen'
    '
    If Not IsNothing(ConnMISG.Current) Then
      ConnMISG.Current.endedit()
    End If
    ViewMISG.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ConnMISG.CurrencyManager.Refresh()
    ConnMISG.Position = 0
    '
    '
    '
    '
    '
    '
    ViewUseMetLi.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ViewUseMetMesIh.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ViewUseMetAn.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ViewUseMetPa.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ViewUseMetMes.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ViewUseMetMis.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ViewUseMetAnMrk.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ViewUseMisMes.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ViewUseMesGrpRe.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ViewUseMesGrpDo.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ViewUseMisGrpRe.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    ViewUseMisGrpDo.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")

  End Sub
  '
  '
  'Methoden
  '
  '
  '
  Private Sub lstMETB_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstMETB.Click
    Dim i As Integer
    Dim RowView As DataRowView
    If lstMETB.SelectedIndex < 0 Then Exit Sub

    For i = 0 To ViewMETH.Count - 1
      If lstMETB.SelectedValue = ViewMETH(i)("METH_ID") Then
        Exit Sub
      End If
    Next
    '
    'TBL_USER_METH
    '
    '
    RowView = ViewMETH.AddNew()
    RowView("USER_ID") = ConnUSE.Current("USER_ID")
    RowView("METH_ID") = lstMETB.SelectedValue
    RowView("METH_BEZ") = lstMETB.Text
    RowView("MENU_ID") = 0
    RowView("SOND_ID") = 0
    RowView("LCHG_ID") = 0
    RowView("KURV_ID") = 0
    RowView("MATPA_ID") = 0
    RowView("AUSG_ID") = 0
    RowView("BW_ID") = 0
    RowView("FS_ID") = 0
    RowView("ABS_ID") = 0
    RowView.EndEdit()
    '
    '
    '   
    '
    'TBL_USER_METH_ANWSG
    '
    '
    ViewMetAn.RowFilter = "METH_ID=" & lstMETB.SelectedValue
    For i = 0 To ViewMetAn.Count - 1
      RowView = ViewUseMetAn.AddNew
      RowView("USER_ID") = ConnUSE.Current("USER_ID")
      RowView("METH_ID") = lstMETB.SelectedValue
      RowView("ANWSG_ID") = ViewMetAn(i)("ANWSG_ID")
      RowView("ZEIL_NR") = i + 1
      RowView.EndEdit()
    Next i '
    '
    '
    '   
    '
    'TBL_USER_METH_LICHT
    '
    '
    RowView = ViewUseMetLi.AddNew
    RowView("USER_ID") = ConnUSE.Current("USER_ID")
    RowView("METH_ID") = lstMETB.SelectedValue
    RowView("LICHT_ID") = 3
    RowView("POS_ID") = 0
    RowView("NORM_GEW") = 1.0
    RowView.EndEdit()
    '
    '
    '
    '
    'TBL_USER_METH_MESSG
    '
    '
    For i = 0 To ViewMESG.Count - 1
      RowView = ViewUseMetMes.AddNew()
      RowView("USER_ID") = ConnUSE.Current("USER_ID")
      RowView("METH_ID") = lstMETB.SelectedValue
      RowView("MESSG_ID") = ViewMESG(i)("MESSG_ID")
      RowView("GKWRT_ID") = 0
      RowView("USER_REF_GID") = 0
      RowView("MESSG_KA") = 0
      RowView("MESSG_RETR") = 0
      If ViewMESG(i)("MESSG_REFTRA") = "T" Then
        RowView("MESSG_RETR") = 1
      End If
      RowView.EndEdit()
    Next i

    '
    '
    'TBL_USER_METH_MESSG_IHRM
    '
    '
    For i = 0 To ViewMESG.Count - 1
      ViewMesIh.RowFilter = "MESSG_ID=" & ViewMESG(i)("MESSG_ID") & " AND POS_ID=0"
      If ViewMesIh.Count <> 0 Then
        RowView = ViewUseMetMesIh.AddNew
        RowView("USER_ID") = ConnUSE.Current("USER_ID")
        RowView("METH_ID") = lstMETB.SelectedValue
        RowView("MESSG_ID") = ViewMESG(i)("MESSG_ID")
        RowView("IHRM_ID") = ViewMesIh(0)("IHRM_ID")
        RowView("POS_ID") = 0
        RowView("IHRM_GEW") = 1.0
        RowView.EndEdit()
      End If
    Next i

    '
    '
    '
    'TBL_USER_METH_MISCH
    '
    '
    '
    For i = 0 To ViewMISG.Count - 1
      RowView = ViewUseMetMis.AddNew()
      RowView("USER_ID") = ConnUSE.Current("USER_ID")
      RowView("METH_ID") = lstMETB.SelectedValue
      RowView("MISCH_ID") = ViewMISG(i)("MISCH_ID")
      RowView("USER_RZP_GID") = 0
      RowView("REZMN_ID") = 0
      RowView("ICHI") = 0
      RowView("BPROB") = 0
      RowView("KGX") = 0
      RowView("IGX") = 0
      RowView("ISOR") = 0
      RowView("KWE") = 0
      RowView("KWD") = 0
      RowView("REZVOR") = 0
      RowView("SCHWRZ") = 0
      RowView("MME") = 0
      RowView("VERT") = 0
      RowView("MINDOS") = 0
      RowView.EndEdit()
    Next i
    '

    ''
  End Sub


  Private Sub lstMETH_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstMETH.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call lstMETH_KeyDown(sender, ev)
  End Sub

  Private Sub lstMETH_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstMETH.KeyDown
    ViewMETH.Delete(lstMETH.SelectedIndex)
  End Sub
  '
  'Messgeräte
  '
  Private Sub lstMESB_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstMESB.Click
    Dim i As Integer
    Dim RowView As DataRowView
    If lstMESB.SelectedIndex < 0 Then Exit Sub

    For i = 0 To ViewMESG.Count - 1
      If lstMESB.SelectedValue = ViewMESG(i)("MESSG_ID") Then
        Exit Sub
      End If
    Next
    '
    '
    '
    'Messgerät hinzufügen
    '
    'TBL_USER_MESSG
    '
    '
    '
    RowView = ViewMESG.AddNew()
    RowView("USER_ID") = ConnUSE.Current("USER_ID")
    RowView("MESSG_ID") = lstMESB.SelectedValue
    RowView("MESSG_KBEZ") = lstMESB.Text
    RowView("MESSG_TDIFF") = 30
    RowView("MESSG_TWAIT") = 10
    RowView("MESSG_IMES") = 1
    RowView("MESSG_IKAL") = 1
    RowView("MESSG_DE") = 1
    RowView("MESSG_STELL") = 5
    RowView("MESSG_TOP") = 200
    RowView("MESSG_SOND") = 0
    RowView("MESSG_LABOR_ID") = 1
    RowView("MESSG_INI") = 1
    RowView("MESSG_KAL") = 1
    RowView.EndEdit()
    '
    '
    '
    
    '
    '
    'TBL_USER_METH_MESSG
    '
    '
    '
    For i = 0 To ViewMETH.Count - 1
      RowView = ViewUseMetMes.AddNew
      RowView("USER_ID") = ConnUSE.Current("USER_ID")
      RowView("METH_ID") = ViewMETH(i)("METH_ID")
      RowView("MESSG_ID") = lstMESB.SelectedValue
      RowView("GKWRT_ID") = 0
      RowView("USER_REF_GID") = 0
      RowView("MESSG_KA") = 0
      RowView("MESSG_RETR") = 0
      If TblMESB.Rows(lstMESB.SelectedIndex)("MESSG_REFTRA") = "T" Then
        RowView("MESSG_RETR") = 1
      End If
      RowView.EndEdit()
    Next i
    '
    '
    'TBL_USER_METH_MESSG_IHRM
    '
    '
    ViewMesIh.RowFilter = "MESSG_ID=" & lstMESB.SelectedValue & " AND POS_ID=0"
    If ViewMesIh.Count <> 0 Then
      For i = 0 To ViewMETH.Count - 1
        RowView = ViewUseMetMesIh.AddNew
        RowView("USER_ID") = ConnUSE.Current("USER_ID")
        RowView("METH_ID") = ViewMETH(i)("METH_ID")
        RowView("MESSG_ID") = lstMESB.SelectedValue
        RowView("IHRM_ID") = ViewMesIh(0)("IHRM_ID")
        RowView("POS_ID") = 0
        RowView("IHRM_GEW") = 1.0
        RowView.EndEdit()
      Next i
    End If
    '
    'TBL_USER_MISCH_MESSG
    '
    '
    For i = 0 To ViewMISG.Count - 1
      RowView = ViewUseMisMes.AddNew
      RowView("USER_ID") = ConnUSE.Current("USER_ID")
      RowView("MISCH_ID") = ViewMISG(i)("MISCH_ID")
      RowView("MESSG_ID") = lstMESB.SelectedValue
      RowView("GKWRT_ID") = 0
      RowView.EndEdit()
    Next i
    ''

  End Sub


  Private Sub lstMESG_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstMESG.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call lstMESG_KeyDown(sender, ev)
  End Sub

  Private Sub lstMESG_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstMESG.KeyDown
    ViewMESG.Delete(lstMESG.SelectedIndex)
  End Sub
  '
  'Mischsysteme
  '
  Private Sub lstMISB_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstMISB.Click
    Dim i As Integer
    Dim RowView As DataRowView
    If lstMISB.SelectedIndex < 0 Then Exit Sub
    For i = 0 To ViewMISG.Count - 1
      If lstMISB.SelectedValue = ViewMISG(i)("MISCH_ID") Then
        Exit Sub
      End If
    Next
    '
    'TBL_USER_MISCH
    '
    RowView = ViewMISG.AddNew()
    RowView("USER_ID") = ConnUSE.Current("USER_ID")
    RowView("MISCH_ID") = lstMISB.SelectedValue
    RowView("MISCH_KBEZ") = lstMISB.Text
    RowView("MISCH_TDIFF") = 30
    RowView("MISCH_TOP") = 200
    RowView("MISCH_SOND") = 0
    RowView.EndEdit()
    '
    '
    '
    'TBL_USER_METH_MISCH
    '
    '
    '
    '
    '
    For i = 0 To ViewMETH.Count - 1
      RowView = ViewUseMetMis.AddNew()
      RowView("USER_ID") = ConnUSE.Current("USER_ID")
      RowView("METH_ID") = ViewMETH(i)("METH_ID")
      RowView("MISCH_ID") = lstMISB.SelectedValue
      RowView("USER_RZP_GID") = 0
      RowView("REZMN_ID") = 0
      RowView("ICHI") = 0
      RowView("BPROB") = 0
      RowView("KGX") = 0
      RowView("IGX") = 0
      RowView("ISOR") = 0
      RowView("KWE") = 0
      RowView("KWD") = 0
      RowView("REZVOR") = 0
      RowView("SCHWRZ") = 0
      RowView("MME") = 0
      RowView("VERT") = 0
      RowView("MINDOS") = 0
      RowView.EndEdit()
    Next i
    '
    '
    '
    '
    'TBL_USER_MISCH_MESSG
    '
    '
    For i = 0 To ViewMESG.Count - 1
      RowView = ViewUseMisMes.AddNew
      RowView("USER_ID") = ConnUSE.Current("USER_ID")
      RowView("MISCH_ID") = lstMISB.SelectedValue
      RowView("MESSG_ID") = ViewMESG(i)("MESSG_ID")
      RowView("GKWRT_ID") = 0
      RowView.EndEdit()
    Next i
    '
    '
    '
    '
    '
   

  End Sub


  Private Sub lstMISG_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstMISG.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call lstMISG_KeyDown(sender, ev)
  End Sub

  Private Sub lstMISG_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstMISG.KeyDown
    ViewMISG.Delete(lstMISG.SelectedIndex)
  End Sub
  '
  '
  '
  '
  '
  '
  '
  '
  Private Sub txtENB_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtENB.TextChanged
    Dim i As Short
    For i = 0 To chkEnb.Count - 1
      chkEnb(i).Checked = BitWrt(i, txtENB.Text)
    Next
  End Sub

  Private Sub chkENB_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
    chkENB_00.CheckedChanged, chkENB_01.CheckedChanged, chkENB_02.CheckedChanged, chkENB_03.CheckedChanged, chkENB_04.CheckedChanged, _
    chkENB_05.CheckedChanged, chkENB_06.CheckedChanged, chkENB_07.CheckedChanged, chkENB_08.CheckedChanged, chkENB_09.CheckedChanged, _
    chkENB_10.CheckedChanged, chkENB_11.CheckedChanged, chkENB_12.CheckedChanged, chkENB_13.CheckedChanged, chkENB_14.CheckedChanged, _
    chkENB_15.CheckedChanged, chkENB_16.CheckedChanged, chkENB_17.CheckedChanged, chkENB_18.CheckedChanged, chkENB_19.CheckedChanged, _
    chkENB_20.CheckedChanged, chkENB_21.CheckedChanged, chkENB_22.CheckedChanged, chkENB_23.CheckedChanged, chkENB_24.CheckedChanged, _
    chkENB_25.CheckedChanged, chkENB_26.CheckedChanged, chkENB_27.CheckedChanged, chkENB_28.CheckedChanged, chkENB_29.CheckedChanged, _
    chkENB_30.CheckedChanged, chkENB_31.CheckedChanged, chkENB_32.CheckedChanged, chkENB_33.CheckedChanged, chkENB_34.CheckedChanged, _
    chkENB_35.CheckedChanged, chkENB_36.CheckedChanged, chkENB_37.CheckedChanged, chkENB_38.CheckedChanged, chkENB_39.CheckedChanged, _
    chkENB_40.CheckedChanged, chkENB_41.CheckedChanged, chkENB_42.CheckedChanged, chkENB_43.CheckedChanged
    Dim Index As Short
    Index = CInt(sender.name.substring(7, 2))
    txtENB.Text = BitWrtID(sender.checked, Index, txtENB.Text)
  End Sub
  '
  '
  '
  '
  '
  '
  '
  Private Sub txtVIS_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtVIS.TextChanged
    Dim i As Short
    For i = 0 To chkvis.Count - 1
      chkvis(i).Checked = BitWrt(i, txtVIS.Text)
    Next

  End Sub

  Private Sub chkVIS_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
    chkVIS_00.CheckedChanged, chkVIS_01.CheckedChanged, chkVIS_02.CheckedChanged, chkVIS_03.CheckedChanged, chkVIS_04.CheckedChanged, _
    chkVIS_05.CheckedChanged, chkVIS_06.CheckedChanged, chkVIS_07.CheckedChanged, chkVIS_08.CheckedChanged, chkVIS_09.CheckedChanged, _
    chkVIS_10.CheckedChanged, chkVIS_11.CheckedChanged, chkVIS_12.CheckedChanged, chkVIS_13.CheckedChanged, chkVIS_14.CheckedChanged, _
    chkVIS_15.CheckedChanged, chkVIS_16.CheckedChanged, chkVIS_17.CheckedChanged, chkVIS_18.CheckedChanged, chkVIS_19.CheckedChanged, _
    chkVIS_20.CheckedChanged, chkVIS_21.CheckedChanged, chkVIS_22.CheckedChanged, chkVIS_23.CheckedChanged, chkVIS_24.CheckedChanged, _
    chkVIS_25.CheckedChanged, chkVIS_26.CheckedChanged, chkVIS_27.CheckedChanged, chkVIS_28.CheckedChanged, chkVIS_29.CheckedChanged, _
    chkVIS_30.CheckedChanged, chkVIS_31.CheckedChanged, chkVIS_32.CheckedChanged, chkVIS_33.CheckedChanged, chkVIS_34.CheckedChanged, _
    chkVIS_35.CheckedChanged, chkVIS_36.CheckedChanged, chkVIS_37.CheckedChanged, chkVIS_38.CheckedChanged, chkVIS_39.CheckedChanged, _
    chkVIS_40.CheckedChanged, chkVIS_41.CheckedChanged, chkVIS_42.CheckedChanged, chkENB_43.CheckedChanged
    Dim Index As Short
    Index = CInt(sender.name.substring(7, 2))
    txtVIS.Text = BitWrtID(sender.checked, Index, txtVIS.Text)
  End Sub
  '
  '
  '
  '
  Private Sub txtWRI_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtWRI.TextChanged
    Dim i As Short
    For i = 0 To chkWri.Count - 1
      chkWri(i).Checked = BitWrt(i, txtWRI.Text)
    Next i

  End Sub

  Private Sub chkwri_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
    chkWRI_00.CheckedChanged, chkWRI_01.CheckedChanged, chkWRI_02.CheckedChanged, chkWRI_03.CheckedChanged, chkWRI_04.CheckedChanged, _
    chkWRI_05.CheckedChanged, chkWRI_06.CheckedChanged, chkWRI_07.CheckedChanged, chkWRI_08.CheckedChanged, chkWRI_09.CheckedChanged, _
    chkWRI_10.CheckedChanged, chkWRI_11.CheckedChanged, chkWRI_12.CheckedChanged, chkWRI_13.CheckedChanged, chkWRI_14.CheckedChanged, _
    chkWRI_15.CheckedChanged, chkWRI_16.CheckedChanged, chkWRI_17.CheckedChanged, chkWRI_18.CheckedChanged, chkWRI_19.CheckedChanged, _
    chkWRI_20.CheckedChanged, chkWRI_21.CheckedChanged, chkWRI_22.CheckedChanged, chkWRI_23.CheckedChanged, chkWRI_24.CheckedChanged, _
    chkWRI_25.CheckedChanged, chkWRI_26.CheckedChanged, chkWRI_27.CheckedChanged, chkWRI_28.CheckedChanged, chkWRI_29.CheckedChanged, _
    chkWRI_30.CheckedChanged
    Dim Index As Short
    Index = CInt(sender.name.substring(7, 2))
    txtWRI.Text = BitWrtID(sender.checked, Index, txtWRI.Text)
  End Sub
  '
  '
  '
  '
  Private Sub txtSON_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtSON.TextChanged
    Dim i As Short
    For i = 0 To chkSon.Count - 1
      chkSon(i).Checked = BitWrt(i, txtSON.Text)
    Next

  End Sub

  Private Sub chkSON_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
    chkSON_00.CheckedChanged, chkSON_01.CheckedChanged, chkSON_02.CheckedChanged, chkSON_03.CheckedChanged, chkSON_04.CheckedChanged, _
    chkSON_05.CheckedChanged, chkSON_06.CheckedChanged, chkSON_07.CheckedChanged, chkSON_08.CheckedChanged, chkSON_09.CheckedChanged, _
    chkSON_10.CheckedChanged, chkSON_11.CheckedChanged, chkSON_12.CheckedChanged, chkSON_13.CheckedChanged, chkSON_14.CheckedChanged, _
    chkSON_15.CheckedChanged, chkSON_16.CheckedChanged, chkSON_17.CheckedChanged, chkSON_18.CheckedChanged, chkSON_19.CheckedChanged, _
    chkSON_20.CheckedChanged, chkSON_21.CheckedChanged, chkSON_22.CheckedChanged, chkSON_23.CheckedChanged, chkSON_24.CheckedChanged, _
    chkSON_24.CheckedChanged, chkSON_25.CheckedChanged, chkSON_26.CheckedChanged, chkSON_27.CheckedChanged, chkSON_28.CheckedChanged, _
    chkSON_29.CheckedChanged, chkSON_30.CheckedChanged, chkSON_31.CheckedChanged, chkSON_32.CheckedChanged, chkSON_33.CheckedChanged
    Dim Index As Short
    Index = CInt(sender.name.substring(7, 2))
    txtSON.Text = BitWrtID(sender.checked, Index, txtSON.Text)
  End Sub '
  '
  '
  '
  '
  '
  Private Sub txtDRM_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtDRM.TextChanged
    Dim i As Short
    For i = 0 To chkDRM.Count - 1
      chkDRM(i).Checked = BitWrt(i, txtDRM.Text)
    Next

  End Sub

  Private Sub chkDRM_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
    chkDRM_00.CheckedChanged, chkDRM_01.CheckedChanged, chkDRM_02.CheckedChanged, chkDRM_03.CheckedChanged, chkDRM_04.CheckedChanged, _
    chkDRM_05.CheckedChanged, chkDRM_06.CheckedChanged, chkDRM_07.CheckedChanged, chkDRM_08.CheckedChanged, chkDRM_09.CheckedChanged, _
    chkDRM_10.CheckedChanged, chkDRM_11.CheckedChanged, chkDRM_12.CheckedChanged, chkDRM_13.CheckedChanged, chkDRM_14.CheckedChanged, _
    chkDRM_15.CheckedChanged, chkDRM_16.CheckedChanged, chkDRM_17.CheckedChanged, chkDRM_18.CheckedChanged, chkDRM_19.CheckedChanged, _
    chkDRM_20.CheckedChanged, chkDRM_21.CheckedChanged, chkDRM_22.CheckedChanged, chkDRM_23.CheckedChanged, chkDRM_24.CheckedChanged, _
    chkDRM_25.CheckedChanged, chkDRM_26.CheckedChanged, chkDRM_27.CheckedChanged, chkDRM_28.CheckedChanged, chkDRM_29.CheckedChanged, _
    chkDRM_30.CheckedChanged, chkDRM_31.CheckedChanged
    Dim Index As Short
    Index = CInt(sender.name.substring(7, 2))
    txtDRM.Text = BitWrtID(sender.checked, Index, txtDRM.Text)
  End Sub
  '
  Private Sub txtDRU_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtDRU.TextChanged
    Dim i As Short
    For i = 0 To optDRU.Count - 1
      If txtDRU.Text Mod 64 = i Then
        optDRU(i).Checked = True
      Else
        optDRU(i).Checked = False
      End If
    Next i
    For i = 0 To optDRR.Count - 1
      If Fix(txtDRU.Text / 64) = i Then
        optDRR(i).Checked = True
      Else
        optDRR(i).Checked = False
      End If
    Next i
    '
    '
    ''
  End Sub
  Private Sub optDR_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  optDRR_0.Click, optDRR_1.Click, optDRU_0.Click, optDRU_1.Click, optDRU_2.Click, optDRU_3.Click
    Dim i As Integer
    Dim HlfID As Integer
    HlfID = 0
    For i = 0 To optDRU.Count - 1
      If optDRU(i).Checked Then
        HlfID = HlfID + i
        Exit For
      End If
    Next i
    For i = 0 To optDRR.Count - 1
      If optDRR(i).Checked Then
        HlfID = HlfID + 64 * i
        Exit For
      End If
    Next i
    txtDRU.Text = HlfID
  End Sub
  '
  
   
  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    Dim i As Integer
    Dim j As Integer
    Dim ViewHilf As DataView
    If ConnUSE.Count = 0 Then
      MsgBox(Texxt(3761))
      Me.Close()
      Exit Sub
    End If
    If AddDelP(2990) Then
      '
      '
      ConnUSE.CurrencyManager.EndCurrentEdit()
      ConnMETH.CurrencyManager.EndCurrentEdit()
      ConnMESG.CurrencyManager.EndCurrentEdit()
      ConnMISG.CurrencyManager.EndCurrentEdit()
      ViewUseMetLi.EndInit()
      ViewUseMetMesIh.EndInit()
      ViewUseMetAn.EndInit()
      ViewUseMetPa.EndInit()
      ViewUseMetMes.EndInit()
      ViewUseMetMis.EndInit()
      ViewUseMetAnMrk.EndInit()
      ViewUseMisMes.EndInit()
      ViewUseMesGrpRe.EndInit()
      ViewUseMesGrpDo.EndInit()
      ViewUseMisGrpRe.EndInit()
      ViewUseMisGrpDo.EndInit()
      '
      '
      'Delete
      '
      '
      '
      '
      For i = Adapt.Count - 1 To 0 Step -1
        Select Case dsUSE.Tables(i).TableName
          Case "TblUseMetMes", "TblUseMetMesih"
            ViewHilf = New DataView(TblMESG)
            ViewHilf.RowStateFilter = DataViewRowState.Deleted And DataViewRowState.OriginalRows
            For j = 0 To ViewHilf.Count - 1
              Adapt(i).DeleteCommand.Parameters(0).Value = ViewHilf(j)("USER_ID")
              Adapt(i).DeleteCommand.Parameters(1).Value = 0
              Adapt(i).DeleteCommand.Parameters(2).Value = ViewHilf(j)("MESSG_ID")
              If SQLExeNonQuery(Adapt(i).DeleteCommand, Cncol) Then
                Exit Sub
              End If
            Next j
            ViewHilf = New DataView(TblMETH)
            ViewHilf.RowStateFilter = DataViewRowState.Deleted And DataViewRowState.OriginalRows
            For j = 0 To ViewHilf.Count - 1
              Adapt(i).DeleteCommand.Parameters(0).Value = ViewHilf(j)("USER_ID")
              Adapt(i).DeleteCommand.Parameters(1).Value = ViewHilf(j)("METH_ID")
              Adapt(i).DeleteCommand.Parameters(2).Value = 0
              If SQLExeNonQuery(Adapt(i).DeleteCommand, Cncol) Then
                Exit Sub
              End If
            Next j
            ViewHilf.Dispose()
          Case "TblUseMetMis"
            ViewHilf = New DataView(TblMETH)
            ViewHilf.RowStateFilter = DataViewRowState.Deleted And DataViewRowState.OriginalRows
            For j = 0 To ViewHilf.Count - 1
              Adapt(i).DeleteCommand.Parameters(0).Value = ViewHilf(j)("USER_ID")
              Adapt(i).DeleteCommand.Parameters(1).Value = ViewHilf(j)("METH_ID")
              Adapt(i).DeleteCommand.Parameters(2).Value = 0
              If SQLExeNonQuery(Adapt(i).DeleteCommand, Cncol) Then
                Exit Sub
              End If
            Next j
            ViewHilf = New DataView(TblMISG)
            ViewHilf.RowStateFilter = DataViewRowState.Deleted And DataViewRowState.OriginalRows
            For j = 0 To ViewHilf.Count - 1
              Adapt(i).DeleteCommand.Parameters(0).Value = ViewHilf(j)("USER_ID")
              Adapt(i).DeleteCommand.Parameters(1).Value = 0
              Adapt(i).DeleteCommand.Parameters(2).Value = ViewHilf(j)("MISCH_ID")
              If SQLExeNonQuery(Adapt(i).DeleteCommand, Cncol) Then
                Exit Sub
              End If
            Next j
            ViewHilf.Dispose()
          Case "TblUseMisMes"
            ViewHilf = New DataView(TblMISG)
            ViewHilf.RowStateFilter = DataViewRowState.Deleted And DataViewRowState.OriginalRows
            For j = 0 To ViewHilf.Count - 1
              Adapt(i).DeleteCommand.Parameters(0).Value = ViewHilf(j)("USER_ID")
              Adapt(i).DeleteCommand.Parameters(1).Value = ViewHilf(j)("MISCH_ID")
              Adapt(i).DeleteCommand.Parameters(2).Value = 0
              If SQLExeNonQuery(Adapt(i).DeleteCommand, Cncol) Then
                Exit Sub
              End If
            Next j
            ViewHilf = New DataView(TblMESG)
            ViewHilf.RowStateFilter = DataViewRowState.Deleted And DataViewRowState.OriginalRows
            For j = 0 To ViewHilf.Count - 1
              Adapt(i).DeleteCommand.Parameters(0).Value = ViewHilf(j)("USER_ID")
              Adapt(i).DeleteCommand.Parameters(1).Value = 0
              Adapt(i).DeleteCommand.Parameters(2).Value = ViewHilf(j)("MESSG_ID")
              If SQLExeNonQuery(Adapt(i).DeleteCommand, Cncol) Then
                Exit Sub
              End If
            Next j
            ViewHilf.Dispose()
          Case "TblMETH"
            Adapt(i).Update(dsUSE.Tables(i).Select(Nothing, Nothing, DataViewRowState.Deleted))
            ReDim WhereKeyID(0)
            WhereKeyID(0) = "USER_ID"
            Adapt(i).DeleteCommand = OleDBDeleteCmd("TBL_USER", WhereKeyID, Cncol)
            Adapt(i).DeleteCommand.CommandText = "DELETE * FROM TBL_USER_METH WHERE USER_ID=?"
            ViewHilf = New DataView(TblUSE)
            ViewHilf.RowStateFilter = DataViewRowState.Deleted And DataViewRowState.OriginalRows
            For j = 0 To ViewHilf.Count - 1
              Adapt(i).DeleteCommand.Parameters(0).Value = ViewHilf(j)("USER_ID")
              If SQLExeNonQuery(Adapt(i).DeleteCommand, Cncol) Then
                Exit Sub
              End If
            Next j

          Case Else

            Adapt(i).Update(dsUSE.Tables(i).Select(Nothing, Nothing, DataViewRowState.Deleted))
        End Select
      Next i

      '
      '
      'Insert
      '
      '
      For i = 0 To Adapt.Count - 1
        Adapt(i).Update(dsUSE.Tables(i).Select(Nothing, Nothing, DataViewRowState.Added))
      Next i
      '
      '
      'Update
      '
      '
      For i = 0 To Adapt.Count - 1
        Adapt(i).Update(dsUSE.Tables(i).Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      Next i
      '
      '
    End If
    Try
      Me.Close()
      Me.Dispose()
    Catch ex As Exception

    End Try
   
  End Sub
 




  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
  End Sub




  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub


  Sub CheckRowsRelation(ByVal ParentFields() As String, ByVal ChildFields() As String, ByVal ParentRows As DataRow(), ByVal ChildRows As DataRow())
    '
    'Prüfen, ob Felder in Childtable nicht in Parenttable enthalten
    '
    '
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim MSG As String
    For i = 0 To ChildRows.Count - 1
      For j = 0 To ParentRows.Count - 1
        For k = 0 To ParentFields.Count - 1
          If ChildRows(i)(ChildFields(k)) <> ParentRows(j)(ParentFields(k)) Then
            Exit For
          End If
        Next k
        If k = ParentFields.Count Then
          Exit For
        End If
      Next j
      If j = ParentRows.Count Then
        MSG = ""
        For k = 0 To ChildFields.Count - 1
          MSG = MSG & ChildFields(k) & Space(2) & CStr(ChildRows(i)(ChildFields(k)))
        Next
        ' MsgBox(MSG)
        ChildRows(i).Delete()
      End If
    Next i
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


