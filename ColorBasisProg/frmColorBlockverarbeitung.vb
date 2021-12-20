Option Explicit On
Option Compare Text
Option Strict Off
Public Class frmColorBlockverarbeitung
  Inherits Windows.Forms.Form
  Dim DsetSortRezptRwrt As DataSet
  Dim OleAdMisch As OleDbDataAdapter
  Dim OleAdSorti As OleDbDataAdapter
  Dim OleAdRezept As OleDbDataAdapter
  Dim OleAdRwert As OleDbDataAdapter
  Dim OleAdRezeptRwert As OleDbDataAdapter
  Dim OleAdRezeptFarbm As OleDbDataAdapter
  Dim CmdGroup As OleDbCommand
  Dim ReadGroup As OleDbDataReader
  Dim CmdRwerte As OleDbCommand
  Dim WithEvents CnnRwerte As BindingSource
  Dim TblRwerte As DataTable
  Dim TblAll As DataTable
  Dim RowRwerte As DataRow
  Dim DbErr As Integer
  Dim Kdru As Integer
  Dim KdrAll As Integer
  Dim CreateGRPRwrt As HandleRwerte
  Dim TabGridWerte As DataTable
  Dim TabGridBase As DataTable
  Dim RefWerteUNT As List(Of RefValue)
  Dim RefWerteTYP As List(Of RefValue)
  Dim RefWerteSMP As List(Of RefValue)
  Dim Winkel As AngGeos
  '
  '
  '
  '
  '
  Dim WithEvents PrintPreview As New System.Windows.Forms.PrintPreviewDialog
  '
  '
  '
  '
  '
  '
  '
  Dim SqlStmt As String
  Dim OldTabIndex As Integer
  Dim OldMischID As Integer
  Dim ier As Integer
  ' Dim Create As BCSWINReadWriteDaten.CreateTables
  '
  '
  '
  Dim RezptGrid As New List(Of HandleRezC1Screen)
  Dim RezGrid As HandleRezC1Screen

  Dim ChkUNT As New List(Of CheckBox)
  Dim chkMessUnt As New List(Of CheckBox)
  Dim btnMessUnt As New List(Of Button)
  Dim lblMessNch As New List(Of Windows.Forms.Label)
  Dim lblMessUnt As New List(Of Windows.Forms.Label)
  Dim lblDickKor As New List(Of Windows.Forms.Label)
  Dim txtDickKor As New List(Of TextBox)
  Dim lblDickNch As New List(Of Windows.Forms.Label)
  Dim txtDickNch As New List(Of TextBox)
  Dim cboVor As List(Of ComboBox)
  Dim SplitContErstRezepte As List(Of SplitContainer)
  Dim cbomim As New List(Of ComboBox)
  Dim txtMng As New List(Of TextBox)
  Dim txtPro As New List(Of TextBox)
  Dim lblmng As New List(Of Windows.Forms.Label)
  Dim cboDRU As New List(Of ComboBox)
  Dim btnDRU As New List(Of Button)
  Dim btnRezepte As New List(Of Button)
  Dim ToolTipRezepte As ToolTip
  Dim Arr(6) As Single

  '
  '
  '
  '
  '
  '
  '
  '
  Dim i100 As Short
  Dim ReUn As String
  Dim KeyMenge As String
  Dim KeySor As String
  Dim TypID() As Integer = {-1, -1}
  Dim SmpID() As Integer = {-1, -1}
  Dim UntID() As Integer = {-1, -1}
  Dim RezSozpt As RecipesGrp
  Dim GroupRezepte As List(Of RecipesGrp)
  Dim GroupRwerte As List(Of RefValuesGrp)
  Dim BlackRwrt As List(Of RefValues)
  Dim Picauf As HandlePictures
  Dim RezDruck As HandlePlottDruck
  Dim GrpRwerte As RefValuesGrp
  Dim RwWrSortim As ReadWriteSortiment
  Dim RwWrRezept As ReadWriteRezept
  Dim ReWrRwert As ReadWriteRwert
  Dim CalcRezept As RezeptBerechnung
  Dim Quali As QualKontrolle
  Dim PicAufBau As New List(Of HandlePictures)
  Dim RezGraphics As HandleRezGrafik
  Dim RezCheckRad As HandleCheckRad
  Dim DatenFarb As HandleRezDsetFarb
  Dim FillGrid As HandleFillGrid
  Dim HandleRezept As HandleRezGeneral
  '
  '
  '
  '
  '
  '
  Dim KeyD As String
  Dim FaId As Integer
  Dim RcmdRef As Integer

  '
  '
  '
  '
  '
  '
  Dim Ipgm As Integer
  Dim Kzal As Integer
  Dim Kzl As Integer
  Dim DiHsc(1) As Single
  Dim Isor As Integer
  Dim Ksor() As Integer
  '
  Dim Iprn As Integer

  '
  '
  '

  ' Laufvariable
  Dim i As Integer
  Dim l As Integer
  Dim k As Integer
  Dim j As Integer
  Dim kw As Integer
  ' Zwischengroessen
  Dim nkw As Integer
  'Message Kennung
  Dim Igx As Integer
  Dim WiFawrt As Integer

  Private Property TblRefGew As Object



  Private Sub frmColorBlockverarbeitung_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    RemoveHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
    AufbauPar.MethID = -1
  End Sub

  Private Sub frmColorBlockverarbeitung_FormClosing(sender As Object, e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
    Call SpeiAll(e.Cancel)
  End Sub


  Private Sub frmColorBlockverarbeitung_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
    Dim i As Integer
    Dim j As Integer
    '
    '
    Winkel = MenueParam.User.Winkel
    '
    '
    ConFlgKopieren.Text = Texxt(389)

    DsetSortRezptRwrt = New DataSet
    OleAdMisch = New OleDbDataAdapter
    OleAdMisch.SelectCommand = New OleDbCommand("", Cncol)
    OleAdSorti = New OleDbDataAdapter
    OleAdSorti.SelectCommand = New OleDbCommand("", Cndat)
    OleAdRezept = New OleDbDataAdapter
    OleAdRwert = New OleDbDataAdapter
    OleAdRezeptRwert = New OleDbDataAdapter
    OleAdRezeptFarbm = New OleDbDataAdapter
    CmdGroup = New OleDbCommand
    CmdRwerte = New OleDbCommand

    '
    '
    Me.Text = Texxt(1871)
    OldTabIndex = -1
    OldMischID = -1
    AddHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID

    '
    '
    '
    '
    '
    '
    'lblARB
    '
    Me.lblARB.Text = Texxt(2002)
    '
    'chkFAR

    Me.chkFAR.Text = Texxt(857)

    '


    '
    'lblSorBez
    '

    Me.lblSorBez.Text = Texxt(850)
    '
    'lblSSS
    '

    Me.lblSSS.Text = Texxt(800)
    '
    '
    'lblMSH
    '
    Me.lblMSH.Text = Texxt(422)
    '
    'lblGRP
    '
    lblGRP.Text = Texxt(417)
    lblRezN.Text = Texxt(861)
    lblGRPRwrt.Text = Texxt(416)
    'chkMitRwrt.Text = Texxt(849)
    btnAlleRezepte.Text = Texxt(3690)
    'btnAbbruch.Text = Texxt(379)
    '
    '
    'lblGLZ
    '
    Me.lblGLZ.Text = Texxt(358)
    '

    '
    'chkARCH
    '

    Me.chkARCH.Text = Texxt(374)
    '
    'chkVOL
    '

    Me.chkVOL.Text = Texxt(830)
    '

    '

    '
    'chkUUU_1
    '

    Me.chkUUU_1.Text = Texxt(805)
    '
    'chkUUU_0
    '

    Me.chkUUU_0.Text = Texxt(804)
    '

    '

    '
    'chkUNT_1
    '

    Me.chkUNT_1.Text = Texxt(939)
    '
    'chkUNT_0
    '

    Me.chkUNT_0.Text = Texxt(938)
    '

    '
    '
    'chkFawrt;chkRwertAngleich
    '
    Me.chkFawrt.Text = Texxt(607)
    chkRwertAngleich.Text = Texxt(4708)
    'chkRefGew.Text = Texxt(3952)
    lblNANZ.Text = Texxt(734)
    btnSpeichern.Text = Texxt(2447)
    btnGrid.Text = Texxt(3682)
    btnCLIP.Text = Texxt(3662)
    btnDelete.Text = Texxt(381)
    '
    '
    '
    'lblGGE
    '
    '
    ' Me.lblGGE.Text = Texxt(172)
    ' Me.lblGGEMax.Text = Texxt(172) & "(" & Texxt(814) & ")"

    '
    '
    '
    '
    '
    'TabSortiment/btnSortiment
    '

    Me.btnSortiment.Text = Texxt(850)
    '

    '
    '
    'dbgREZ_0
    '
    '
    '
    'btnStoreSorti
    '

    Me.btnStoreSorti.Text = Texxt(799)
    '
    'lblSOR
    '

    Me.lblSOR.Text = Texxt(800)
    '

    '

    '

    '

    '
    'lblDickKor_1
    '

    Me.lblDickKor_1.Text = Texxt(798)
    '
    'lblDickKor_0
    '

    Me.lblDickKor_0.Text = Texxt(797)
    '
    '
    '
    'btnMessVor_0
    '

    Me.btnMessVor.Text = Texxt(806)

    '
    '
    '
    'btnMessUnt_0
    '

    Me.btnMessUnt_0.Text = Texxt(808)
    '
    'btnMessUnt_1
    '

    Me.btnMessUnt_1.Text = Texxt(809)
    '
    'splLimits
    '
    '
    '
    '
    'lblMNG_1
    '
    Me.lblMNG_1.Text = Texxt(814)
    '
    'lblMNG_0
    '
    Me.lblMNG_0.Text = Texxt(813)
    '
    '
    '
    'lblANZ
    '

    Me.lblANZ.Text = Texxt(812)

    '
    'chkNAS
    '
    'wird in diesem Programm nicht verwendet (s. Korrekturprogramm)
    Me.chkNAS.Text = Texxt(834)
    '
    'chkKDE
    '

    Me.chkKDE.Text = Texxt(831)

    '
   
   

    '
    'TabErstrezept/btnErstrezept
    '

    Me.btnErstrezept.Text = Texxt(730)


    '

    
    '
    '
    '
    '

    '
    '
    '
    'lblSRT
    '
    Me.lblSRT.Text = Texxt(855)
    '
    'lblSrMax
    '
    'Me.lblSrMax.Text = Texxt(2427)

    '
    'lblNUM
    '

    'Me.lblNUM.Text = Texxt(883)
    '
    '
    'lblCOL
    '
    Me.lblVLG_1.Text = Texxt(837)
    Me.lblVLG_2.Text = Texxt(837)
    '

    '
    'btnKOP
    '

    Me.btnKOP.Text = Texxt(389)
    '
    '
    btnWEI.Text = Texxt(1999)

    '
    'Mischsystem Enabled / Visible
    '
    '
    If BitWrt(23, MenueParam.User.Enabl) Then
      cboMsh.Enabled = True
    Else
      cboMsh.Enabled = False
    End If
    If BitWrt(23, MenueParam.User.Visbl) Then
      cboMsh.Visible = True
      lblMSH.Visible = True
    Else
      cboMsh.Visible = False
      lblMSH.Visible = False
    End If
    '
    'Gruppe Enabled / Visible
    '
    '
    If BitWrt(24, MenueParam.User.Enabl) Then
      cboGRP.Enabled = True
    Else
      cboGRP.Enabled = False
    End If
    If BitWrt(24, MenueParam.User.Visbl) Then
      cboGRP.Visible = True
    Else
      cboGRP.Visible = False
      lblGRP.Visible = False
    End If
   
    '
    '
    'Archiv Enabled / Visible
    '
    '
    If BitWrt(25, MenueParam.User.Enabl) Then
      chkARCH.Enabled = True
    Else
      chkARCH.Enabled = False
    End If
    If BitWrt(25, MenueParam.User.Visbl) Then
      chkARCH.Visible = True
    Else
      chkARCH.Visible = False
    End If
    '

    '
    '
    ToolTipRezepte = New ToolTip
    '
    HandleRezept = New HandleRezGeneral
    '
    '
    '
    '
    'Werte für Glanzabzug
    '
    cboGLZ.Items.Clear()
    For i = 0 To GetPutReflex.GlanzWrt.Count - 1
      cboGLZ.Items.Add(GetPutReflex.GlanzWrt(i))
    Next
    cboGLZ.Text = Winkel(0).GK(0)
    '
    '
    SplitContErstRezepte = New List(Of SplitContainer)
    SplitContErstRezepte.Add(SplitContSortiment)
    SplitContErstRezepte.Add(SplitContEinzelrezept)
    SplitContErstRezepte.Add(SplitContAlleRezepte)
    SplitContErstRezepte.Add(SplitContainGrid)


    '

    ChkUNT.Add(chkUNT_0)
    ChkUNT.Add(chkUNT_1)
    chkMessUnt.Add(chkMessUnt_0)
    chkMessUnt.Add(chkMessUnt_1)


    btnMessUnt.Add(btnMessUnt_0)
    btnMessUnt.Add(btnMessUnt_1)

    lblMessUnt.Add(lblMessUnt_0)
    lblMessUnt.Add(lblMessUnt_1)
    lblDickKor.Add(lblDickKor_0)
    lblDickKor.Add(lblDickKor_1)
    txtDickKor.Add(txtDickKor_0)
    txtDickKor.Add(txtDickKor_1)
    txtMng.Add(txtMNG_0)
    txtMng.Add(txtMNG_1)
    txtPro.Add(txtPRO_0)
    txtPro.Add(txtPRO_1)
    lblmng.Add(lblMNG_0)
    lblmng.Add(lblMNG_1)
    btnRezepte.Add(btnSortiment)
    btnRezepte.Add(btnErstrezept)
    btnRezepte.Add(btnAlleRezepte)

    '
    '
    '
    '
    DatenFarb = New HandleRezDsetFarb
    RezDruck = New HandlePlottDruck
    RezCheckRad = New HandleCheckRad
    RezGraphics = New HandleRezGrafik
    RezCheckRad.FillGrid = FillGrid
    RezCheckRad.PicGraphic = RezGraphics
    'RezCheckRad.cboWIN = cboWin
    RezCheckRad.cboSKAL = cboSkal
    RezGraphics.cboSKAL = cboSkal
    RezGraphics.TextKDS = Nothing
    RezCheckRad.cboSKAL.Enabled = False

    '
    '
    RezCheckRad.AddchkWIN = chkWIN_0
    RezCheckRad.AddchkWIN = chkWIN_1
    RezCheckRad.AddchkWIN = chkWIN_2
    RezCheckRad.AddchkWIN = chkWIN_3
    RezCheckRad.AddchkWIN = chkWIN_4
    RezCheckRad.AddchkWIN = chkWIN_5
    RezCheckRad.AddchkWIN = chkWIN_6
    RezCheckRad.AddchkWIN = chkWIN_7
    RezCheckRad.AddchkWIN = chkWIN_8
    '
    '
    RezCheckRad.AddradWIN = radWIN_0
    RezCheckRad.AddradWIN = radWIN_1
    RezCheckRad.AddradWIN = radWIN_2
    RezCheckRad.AddradWIN = radWIN_3
    RezCheckRad.AddradWIN = radWIN_4
    RezCheckRad.AddradWIN = radWIN_5
    RezCheckRad.AddradWIN = radWIN_6
    RezCheckRad.AddradWIN = radWIN_7
    RezCheckRad.AddradWIN = radWIN_8
    '
    '
    '
    RezCheckRad.AddradNLA = radNLA_0
    RezCheckRad.AddradNLA = radNLA_1
    RezCheckRad.AddradNLA = radNLA_2
    RezCheckRad.AddradNLA = radNLA_3
    RezCheckRad.AddradNLA = radNLA_4
    RezCheckRad.AddradNLA = radNLA_5

    '

    '
    '
    '
    '
    '
    RezCheckRad.AddchkUUU = chkUUU_0
    RezCheckRad.AddchkUUU = chkUUU_1


    '
    '
    '
    RezCheckRad.AddradUUU = radUUU_0
    RezCheckRad.AddradUUU = radUUU_1




    chkVOL.CheckState = CheckState.Indeterminate

    '
    '
    '
    '
    '
    '

    '
    'Gruppen für Rezepte
    '
    '
    HandleRezept = New HandleRezGeneral
    HandleRezept.cboGRP = cboGRP
    HandleRezept.lblGRP = lblGRP
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
    'Spezial
    '
    '
    '
    'Anzahl Farbmittel pro Rezept
    '
    cbomim.Add(cboMim_0)
    cbomim.Add(cboMim_1)

    '
    For j = 0 To 1
      cbomim(j).Items.Clear()
      For i = 0 To 13
        cbomim(j).Items.Add(CStr(i + 1))
      Next i
    Next j
    '

    ''
    '
    '
    'Sortierung
    '
    cboSRT.Items.Clear()
    For i = 0 To 7
      cboSRT.Items.Add(Texxt(841 + i))
    Next i
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
    cboDRU.Add(Nothing)
    cboDRU.Add(cboDru_1)
    cboDRU.Add(cboDRU_2)
    btnDRU.Add(Nothing)
    btnDRU.Add(btnDru_1)
    btnDRU.Add(btnDRU_2)


    RezSozpt = New RecipesGrp
    GroupRezepte = New List(Of RecipesGrp)
    '
    '
    '
    GrpRwerte = New RefValuesGrp
    GroupRwerte = New List(Of RefValuesGrp)
    '
    '
    '
    RwWrSortim = New ReadWriteSortiment
    RwWrRezept = New ReadWriteRezept
    ReWrRwert = New ReadWriteRwert
    CalcRezept = New RezeptBerechnung
    Quali = New QualKontrolle
    'OptGesamt = New BCSwinParamStruct.GrundDaten
    RezGraphics.AllRezepte = RezSozpt
    RezGraphics.GrpRwerte = GrpRwerte
    '
    '
    '
    '
    '
    RezDruck.Plott = RezGraphics
    RezDruck.AllRezepte = RezSozpt
    RezDruck.GrpRwerte = GrpRwerte
    RezDruck.Winkel = Winkel
    '
    '
    FillGrid = New HandleFillGrid
    FillGrid.flgREZ = flgREZ
    FillGrid.chkFawrt = chkFawrt

    FillGrid.RezGraphic = RezGraphics
   
    '

    '
    '
    'Auswahl Drucken
    '

    If Not BitWrt(19, MenueParam.User.Drum) Then
      Kdru = MenueParam.User.Druq / 64
    Else
      Kdru = 2
    End If
    For i = 1 To 2
      cboDRU(i).Items.Clear()
      For j = 0 To 2
        cboDRU(i).Items.Add(Texxt(940 + j))
      Next j
      If BitWrt(22, MenueParam.User.Drum) Then
        cboDRU(i).Visible = True
        btnDRU(i).Visible = False
      Else
        cboDRU(i).Visible = False
        btnDRU(i).Visible = True
      End If
      cboDRU(i).Text = Texxt(941)
    Next i
    btnDru_1.Text = cboDru_1.Items(Kdru)
    btnDRU_2.Text = cboDRU_2.Items(Kdru)
    'btnDRU_3.Text = cboDRU_3.Items(Kdru)
    '
    '
    cboDruAll.Items.Clear()
    cboDruAll.Text = Texxt(945)
    For j = 1 To 2
      cboDruAll.Items.Add(Texxt(944 + j))
    Next j
    If Not BitWrt(18, MenueParam.User.Drum) Then
      KdrAll = 0
    Else
      KdrAll = 1
    End If
    btnDruAll.Text = Texxt(945 + KdrAll)
    If BitWrt(21, MenueParam.User.Drum) Then
      cboDruAll.Visible = True
      btnDruAll.Visible = False
    Else
      cboDruAll.Visible = False
      btnDruAll.Visible = True
    End If
    If PrinterSettings.InstalledPrinters.Count = 0 Then
      For i = 1 To 2
        cboDRU(i).Visible = False
        btnDRU(i).Visible = False
      Next i
      cboDruAll.Visible = False
      btnDruAll.Visible = False
    End If
    ''
    '
    '
    '
    ''
    For i = 0 To 2
      Picauf = New HandlePictures
      PicAufBau.Add(Picauf)
    Next i
    '
    '
    For i = 0 To 1
      RezGrid = New HandleRezC1Screen
      RezptGrid.Add(RezGrid)
      RezptGrid(i).AllRezepte = RezSozpt
      RezptGrid(i).DatenFarb = DatenFarb
      RezptGrid(i).CalcRezept = CalcRezept
    Next i
    PicAufBau(1).Add("REF", CType(picRwert_1, PictureBox))
    PicAufBau(1).Add("LAB", CType(picFarbLAB_1, PictureBox))
    PicAufBau(1).Add("XYZ", CType(picFarbXYZ_1, PictureBox))
    PicAufBau(1).Add("FRB", CType(picRezFarb_1, PictureBox))
    PicAufBau(1).Add("REZ", CType(picRezRezept_1, PictureBox))
    '
    PicAufBau(2).Add("XYZ", CType(picFarbXYZ_2, PictureBox))
    '
    PicAufBau(1).PicGraphic = RezGraphics
    PicAufBau(2).PicGraphic = RezGraphics

    RezptGrid(0).TDBFar = TDBFar_0
    RezptGrid(0).TDBDropFar = TDBDropFar_0
    RezptGrid(0).TDBDropPre = TDBDropPre_0
    RezptGrid(0).TDBDropPrb = TDBDropPrb_0
    RezptGrid(0).TDBDropPro = TDBDropPro_0
    RezptGrid(0).txtFooter = txtSum_0
    RezptGrid(0).chkVOL = chkVOL
    RezptGrid(0).cboMNG = cboMng
    RezptGrid(0).cboPRO_0 = cboPro_0
    RezptGrid(0).cboPRO_1 = cboPro_1
    RezptGrid(0).lstFAR = lstFAR





    '
    '
    '
    '
    '
    '
    '
    '
    '
    RezptGrid(1).TDBFar = TDBFar_1
    RezptGrid(1).TDBDropFar = TDBDropFar_1
    RezptGrid(1).TDBDropPre = TDBDropPre_1
    RezptGrid(1).TDBDropPrb = TDBDropPrb_1
    RezptGrid(1).TDBDropPro = TDBDropPro_1
    RezptGrid(1).txtFooter = txtSUM_1

    For i = 0 To 1
      RezptGrid(i).txtMNG_0 = txtMNG_0
      RezptGrid(i).txtMNG_1 = txtMNG_1
      RezptGrid(i).txtPRO_0 = txtPRO_0
      RezptGrid(i).txtPRO_1 = txtPRO_1
    Next i




    RezptGrid(0).Picauf = Nothing
    RezptGrid(1).Picauf = PicAufBau(1)

    '
    '
    'Datenstrukturen    '
    DatenFarb.DsetFarbCreate()
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
    '
    '
    '

    '

    '
    For i = 0 To 1
      RezptGrid(i).GrpRwerte = GrpRwerte
      RezptGrid(i).PicGraphic = RezGraphics
    Next i
    '
    '
    '
    '
    '
    CnnRwerte = New BindingSource
    TblRwerte = New DataTable
    TblRwerte.Columns.Add("RwertID", GetType(Integer))
    TblRwerte.Columns.Add("RwertName", GetType(String))
    TblRwerte.Columns.Add("RwertDattim", GetType(Date))

    CnnRwerte.DataSource = TblRwerte

    '
    cboVor = New List(Of ComboBox)
    cboVor.Add(cboVor_0)
    cboVor.Add(cboVor_1)
    cboVor.Add(CboVor_2)
    cboVor_0.DataSource = CnnRwerte
    cboVor_0.DisplayMember = "RwertName"
    cboVor_0.ValueMember = "RwertID"
    cboVor_1.DataSource = CnnRwerte
    cboVor_1.DisplayMember = "RwertName"
    cboVor_1.ValueMember = "RwertID"
    CboVor_2.DataSource = CnnRwerte
    CboVor_2.DisplayMember = "RwertName"
    CboVor_2.ValueMember = "RwertID"

    '

    '
    '
    '
    '
    'Tabelle für Sortimente, Rezepte und R-Werte
    '
    '
    '
    '
    '
    '
    DsetSortRezptRwrt.Tables.Add("TblSorti")
    DsetSortRezptRwrt.Tables.Add("TblRezeptFarbmRwert")
  
    ''
  End Sub
  Private Sub frmColorBlockverarbeitung_Shown(sender As Object, e As System.EventArgs) Handles Me.Shown
    '
    'Tabelle aufbauen
    '
    '
    '

    '
    'Tabelle Mischsysteme
    '
    '
    '
    '
    '
    '
    '
    '
    '
    If MenueParam.MischID <> -1 Then
      OleAdMisch.SelectCommand.CommandText = HandleRezept.MischSelectCommand()
      OleAdMisch.SelectCommand.Connection = Cncol()
      If Not FillDatset(OleAdMisch, DsetSortRezptRwrt, "TblMisch") Then
        Exit Sub
      End If

      cboMsh.DataSource = DsetSortRezptRwrt.Tables("TblMisch")
      cboMsh.DisplayMember = "MISCH_KBEZ"
      cboMsh.ValueMember = "MISCH_ID"
      AddHandler cboMsh.SelectedIndexChanged, AddressOf cboMSH_SelectedIndexChanged

      cboMsh.SelectedIndex = -1
      cboMsh.SelectedValue = MenueParam.MischID
    Else
      MsgBox(Texxt(3601))
      SplitContErstRezepte(2).Visible = False
    End If
    '
  End Sub

  Private Sub Change(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMessUnt_0.TextChanged, lblMessUnt_1.TextChanged, txtDickKor_0.TextChanged, txtDickKor_1.TextChanged

    '
    '
    '
    '
    'Daten haben sich geändert
    '
    '
    '
    '
    '
    If RezptGrid.Count = 0 Then Exit Sub
    RezGraphics.DataChanged = True
    '
    '
    '
    '

    If sender.name = "txtDICKKor_0" Then
      If IsNumeric(sender.text) Then
        RezSozpt.Rezepte(KeyMenge).Dicke(0) = CSng(sender.text)
      End If
    End If
    If sender.name = "txtDICKKor_1" Then
      If IsNumeric(sender.text) Then
        RezSozpt.Rezepte(KeyMenge).Dicke(1) = CSng(sender.text)
      End If
    End If
  End Sub

  Sub cboMSH_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMsh.SelectedIndexChanged
    Dim i As Short
    Dim SqlStmt As String
    If sender.Selectedindex = -1 Then Exit Sub
    If Not IsNumeric(sender.Selectedvalue) Then Exit Sub
    If sender.SelectedValue = OldMischID Then Exit Sub
    OldMischID = sender.SelectedValue
    AufbauPar.MischID = -1
    AufbauPar.MischID = sender.SelectedValue
    chkKDE.Checked = MenueParam.Misch.Kgx
    txtKDE.Text = MenueParam.Misch.Fde
    '
    '
    '
    '
    '
    'Grid für Sortimente ("SOR")
    '
    RezptGrid(0).TDBFarGridRezept(ier)
    '
    '
    '
    '
    'Grid für aktuelles Rezept("RZP")
    '
    '
    'RezptGrid(1).imgButton = imgButton

    RezptGrid(1).TDBFarGridRezept(ier)


    DatenFarb.DsetFarbFill(1, False)

    RezptGrid(0).NewKeynam("SOR")
    RezptGrid(1).NewKeynam("RZP")

    '
    '
    '
    '
    '
    '
    cbomim(0).SelectedIndex = MenueParam.Misch.MinFarb - 1
    cbomim(1).SelectedIndex = MenueParam.Misch.MaxFarb - 1
    '
    '
    '
    'Tabelle für Gruppe R-Werte
    '
  
    '
    '
    '
    cboGRPRwrt.DataSource = Nothing
    CreateGRPRwrt = New HandleRwerte
    CreateGRPRwrt.cboGRP = cboGRPRwrt
    CreateGRPRwrt.lblGRP = Nothing
    Call CreateGRPRwrt.GRoupList()
    '
    'Combobox cboGRPR für Gruppen R-Werte
    '
    '
    '
    '
    '
    '
    cboGRPRwrt.SelectedValue = MenueParam.Messg.UserRefGID

    '
    '
    '
    '

    HandleRezept.GroupList()
    '
    If Not btnSpeichern.Visible Then
      cboGRP.Visible = False
      cboGRPRwrt.Visible = False
      lblGRP.Visible = False
      lblGRPRwrt.Visible = False
    End If
    ChkUNT(0).CheckState = CheckState.Indeterminate
    ChkUNT(1).CheckState = CheckState.Unchecked

    '
    '
    TabGridWerte = New DataTable
    TabGridBase = New DataTable
    RefWerteUNT = New List(Of RefValue)
    RefWerteTYP = New List(Of RefValue)
    RefWerteSMP = New List(Of RefValue)
    If Not IsNothing(grdWRT) Then
      grdWRT.DataSource = TabGridWerte
      grdWRT.AllowUserToAddRows = False
      grdWRT.AllowUserToDeleteRows = False
      grdWRT.AlternatingRowsDefaultCellStyle.BackColor = Color.AntiqueWhite
      grdWRT.RowHeadersVisible = False
      grdWRT.SelectionMode = DataGridViewSelectionMode.RowHeaderSelect
    End If
    '
    '
    If Not IsNothing(flgGrid) Then
      flgGrid.DataSource = TabGridBase
      flgGrid.AllowUserToAddRows = False
      flgGrid.AllowUserToDeleteRows = False
      flgGrid.AlternatingRowsDefaultCellStyle.BackColor = Color.LightGreen
      flgGrid.RowHeadersVisible = False
      flgGrid.SelectionMode = DataGridViewSelectionMode.RowHeaderSelect
      flgGrid.RowHeadersVisible = True
    End If
    '
    '
    '
    '
    RezSozpt.Rezepte.clear()
    RezSozpt.Farben.clear()
    RezSozpt.Rezepte.AddRez("SOR", New Recipe)
    RezSozpt.Rezepte.AddRez("BAS", New Recipe)


    '
    '
    '
    GrpRwerte.clear()
    '
    If MenueParam.Misch.Vert = 0 Then
      GrpRwerte.Add("W", New RefValues)
      GrpRwerte.Add("S", New RefValues)
    Else
      GrpRwerte.Add("S", New RefValues)
      GrpRwerte.Add("W", New RefValues)
    End If
    GrpRwerte(0).Add("V", New RefValue)
    GrpRwerte(0).Add("R", New RefValue)
    GrpRwerte(1).Add("V", New RefValue)
    GrpRwerte(1).Add("R", New RefValue)
    GrpRwerte(0).RefUnt = New RefValue
    GrpRwerte(1).RefUnt = New RefValue

    If GrpRwerte.RwArt(0) = "W" Then
      RezSozpt.Rezepte.kwb(0) = 0
      RezSozpt.Rezepte.kwb(1) = 1
      GrpRwerte(0)("V").kwb = 0
      GrpRwerte(1)("V").kwb = 1
      GrpRwerte(0).RefUnt.kwb = 0
      GrpRwerte(1).RefUnt.kwb = 1
      GrpRwerte(0)("R").kwb = 0
      GrpRwerte(1)("R").kwb = 1
      RezGraphics.Vkwb(0) = 0
      RezGraphics.Vkwb(1) = 1
    Else
      RezSozpt.Rezepte.kwb(0) = 1
      RezSozpt.Rezepte.kwb(1) = 0
      GrpRwerte(0)("V").kwb = 1
      GrpRwerte(1)("V").kwb = 0
      GrpRwerte(0).RefUnt.kwb = 1
      GrpRwerte(1).RefUnt.kwb = 0
      GrpRwerte(0)("R").kwb = 1
      GrpRwerte(1)("R").kwb = 0
      RezGraphics.Vkwb(0) = 1
      RezGraphics.Vkwb(1) = 0
    End If
    GrpRwerte(0)("V").Itp = True
    GrpRwerte(1)("V").Itp = True

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
    'Winkel und Lichtart einstellen
    '
    RezGraphics.Winkel = Winkel
    '
    '
    '
    '
    '
    RezCheckRad.Picauf = Nothing
    RezGraphics.Ksrt.Clear()
    For i = 0 To RezCheckRad.chkWIN.Count - 1
      RezCheckRad.chkWIN(i).Visible = False
      RezCheckRad.chkWIN(i).checkstate = CheckState.Unchecked
      RezCheckRad.radWIN(i).Visible = False
      RezCheckRad.radWIN(i).checked = CheckState.Unchecked
    Next i
    For i = 0 To RezCheckRad.radNLA.Count - 1
      RezCheckRad.radNLA(i).Visible = False
      RezCheckRad.radNLA(i).checked = False
    Next i
    For i = 0 To MenueParam.Normfa.Nlz - 1
      RezCheckRad.radNLA(i).Visible = True
      RezCheckRad.radNLA(i).enabled = False
      RezCheckRad.radNLA(i).Text = MenueParam.Normfa(i).NormKenn
    Next i
    '
    RezCheckRad.radNLA(0).checked = True
    '
    '
    '
    For i = 0 To RezCheckRad.chkUUU.Count - 1
      RezCheckRad.chkUUU(i).Text = Texxt(804 + i)
      RezCheckRad.chkUUU(i).Enabled = False
      RezCheckRad.chkUUU(i).Visible = False
      RezCheckRad.chkUUU(i).CheckState = CheckState.Unchecked
      RezCheckRad.radUUU(i).Visible = False
      RezCheckRad.radUUU(i).checked = False
      RezCheckRad.radUUU(i).enabled = False
    Next i
    RezCheckRad.chkUUU(0).Visible = True
    RezCheckRad.chkUUU(0).Enabled = False

    For i = 0 To Winkel.Km - 1
      RezCheckRad.chkWIN(i).Visible = True
      RezCheckRad.chkWIN(i).Enabled = False
      RezCheckRad.radWIN(i).Visible = True
      RezCheckRad.radWIN(i).Enabled = False
      RezCheckRad.chkWIN(i).Text = LTrim(Winkel(i).Chrm)
    Next i
    '
    RezCheckRad.chkWIN(0).checkstate = CheckState.Checked
    RezCheckRad.radWIN(0).checked = CheckState.Checked
    '

    For i = 0 To RezCheckRad.chkUUU.Count - 1
      RezCheckRad.chkUUU(i).Visible = True
      RezCheckRad.chkUUU(i).Text = Texxt(804 + i)
    Next i
    '
    '
    '
    '
    '
    '
    '
    Cursor = Cursors.WaitCursor
    'txtGGEMax.Text = -1.0
    'txtGGE.Text = 0.0
    RezGraphics.DataChanged = False
    Cursor = Cursors.Arrow
    '
    '
    '

    '
    '
    'Sortimente einlesen
    '
    '
    '
    '
    If MenueParam.Misch.Igx > 2 And MenueParam.Misch.Igx < 7 Then MenueParam.Misch.Igx = 0
    '
    Igx = MenueParam.Misch.Igx
    cboSRT.SelectedIndex = MenueParam.Misch.Isor
    '
    '
    '
    '

    '
    'Messungen mit weißem Untergrund oder deckend
    '
    ChkUNT(0).CheckState = CheckState.Checked
    '
    '
    'Messungen mit schwarzem Untergrund
    '
    '
    If MenueParam.Misch.Transp Then
      chkKDE.Checked = MenueParam.Misch.Kgx
      ChkUNT(1).Checked = MenueParam.Misch.Schwrz
    Else
      chkKDE.Checked = False
      ChkUNT(1).Checked = False
    End If
    '
    '
    '
    '
    '
    '
    '
    'Sortimente
    '
    '
    RemoveHandler lstSOR.SelectedIndexChanged, AddressOf lstSOR_SelectedIndexChanged

    If BitWrt(10, MenueParam.User.Writ) Then
      SqlStmt = "SELECT * FROM " & MenueParam.TableSorti & " WHERE USER_ID=" & MenueParam.UserID _
      & " AND MISCH_ID=" & MenueParam.MischID & " ORDER BY SORTI_NAME"
    Else
      SqlStmt = "SELECT * FROM " & MenueParam.TableSorti & " WHERE MISCH_ID=" & MenueParam.MischID & " ORDER BY SORTI_NAME"
    End If
    lstSOR.DataSource = Nothing
    OleAdSorti.SelectCommand.CommandText = SqlStmt
    OleAdSorti.SelectCommand.Connection = Cndat()
    If Not FillDatset(OleAdSorti, DsetSortRezptRwrt, "TblSorti") Then
      Exit Sub
    End If

    lstSOR.DataSource = DsetSortRezptRwrt.Tables("TblSorti")
    lstSOR.DisplayMember = "SORTI_NAME"
    lstSOR.ValueMember = "SORTI_ID"
    AddHandler lstSOR.SelectedIndexChanged, AddressOf lstSOR_SelectedIndexChanged

    btnMessVor.Text = Texxt(806 + RezGraphics.Vkwb(0)) & Space(1)
    If BitWrt(RezGraphics.Vkwb(0), MenueParam.Messg.MeArtID) Then
      btnMessVor.Text = btnMessVor.Text & Space(1) & Texxt(3913)

    End If
    For i = 0 To 1
      btnMessUnt(i).Text = Texxt(808 + RezGraphics.Vkwb(i)) & Space(1)
      RezCheckRad.chkUUU(i).Text = Texxt(804 + RezGraphics.Vkwb(i))
      lblDickKor(i).Text = Texxt(810 + RezGraphics.Vkwb(i))
      ToolTipRezepte.SetToolTip(RezCheckRad.chkUUU(i), Texxt(927 + RezGraphics.Vkwb(i)))
      If BitWrt(RezGraphics.Vkwb(i), MenueParam.Messg.MeArtID) Then
        btnMessUnt(i).Text = btnMessUnt(i).Text & Space(1) & Texxt(3913)
      End If
    Next i
    '
    '
    '
    'panErstrezepte für Sortimente
    ' AddHandler TabRezeptControl.SelectedIndexChanged, AddressOf TabRezeptControl_SelectedIndexChanged
    btnSortiment.PerformClick()
    lstSOR.SelectedIndex = -1
    If DsetSortRezptRwrt.Tables("TblSorti").Rows.Count > 0 Then
      lstSOR.SelectedIndex = 0
    End If
    '
    '
    '
    chkRwertAngleich.Checked = Not chkRwertAngleich.Checked
    chkRwertAngleich.Checked = Not chkRwertAngleich.Checked

  End Sub






  Sub lstSOR_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lstSOR.SelectedIndexChanged
    Dim i As Short
    GroupRezepte.Clear()
    GroupRwerte.Clear()

    If KeyMenge Is Nothing Then Exit Sub
    If lstSOR.SelectedIndex = -1 Then
      RezSozpt.Rezepte(KeyMenge).Dicke(0) = MenueParam.Misch.Dicke
      RezSozpt.Rezepte(KeyMenge).Dicke(1) = MenueParam.Misch.Dicke
      For i = 0 To 1
        TypID(i) = -1
        UntID(i) = -1
        SmpID(i) = -1
      Next i
      Exit Sub
    End If
    If KeyMenge <> "SOR" Then Exit Sub
    If IsNothing(lstSOR.SelectedValue) Then Exit Sub
    Cursor = Cursors.WaitCursor
    RezGrid.chkVOL.CheckState = CheckState.Indeterminate
    SplitContErstRezepte(1).Enabled = False
    SplitContErstRezepte(2).Enabled = False
    Call HandleRezept.SorSpei(RezGraphics.DataChanged, lstSOR, KeyMenge, RezSozpt, OleAdSorti, DsetSortRezptRwrt.Tables("tblsorti"), RwWrSortim, UntID, TypID, ier)
    RezGraphics.DataChanged = False

    HandleRezept.ClearRezepte(RezSozpt)
    RezGraphics.DataChanged = False
    For i = 0 To 1
      lblMessUnt(i).Text = ""
      GrpRwerte(i).RefUnt.Name = ""
      GrpRwerte(0)(i).Name = ""
      GrpRwerte(1)(i).Name = ""
    Next i
    For i = 0 To RezGrid.AllRezepte.Rezepte.RezCount - 1
      RezGrid.AllRezepte.Rezepte(i).clear()
    Next i
    '
    '
    '
    '
    'Einlesen Sortiment 
    '
    '
    '
    RwWrSortim.ReadSortimFarbGrund(KeyMenge, lstSOR.SelectedValue, RezSozpt, UntID, TypID, ier)
    lblSSS.Text = RezSozpt.Rezepte(KeyMenge).Name
    '
    GrpRwerte(0).RefUnt.ID = UntID(RezGraphics.Vkwb(0))
    GrpRwerte(1).RefUnt.ID = UntID(RezGraphics.Vkwb(1))
    GrpRwerte(0)("V").ID = TypID(RezGraphics.Vkwb(0))
    GrpRwerte(1)("V").ID = TypID(RezGraphics.Vkwb(1))
    GrpRwerte(0).RefUnt.Nr = -1
    GrpRwerte(1).RefUnt.Nr = -1
    GrpRwerte(0)("V").Nr = -1
    GrpRwerte(1)("V").Nr = -1

    '
    '
    '
    'Einlesen R-Werte
    '
    '
    '

    For i = 0 To GrpRwerte.Count - 1
      GrpRwerte(i).RefUnt.Name = ""
      GrpRwerte(i).RefUnt.Bem = ""
      GrpRwerte(i).RefUnt.IVoNa = False
      GrpRwerte(i).RefUnt.Iplott = False
      If GrpRwerte(i).RefUnt.ID >= 0 Then
        ReWrRwert.ReadRwert(GrpRwerte(i).RefUnt.ID, GrpRwerte(i).RefUnt, ier)
        GrpRwerte(i).RefUnt.Nr = i
      End If
      lblMessUnt(i).Text = GrpRwerte(i).RefUnt.Name
    Next i

    For i = 0 To GrpRwerte.Count - 1
      GrpRwerte(i)("V").Name = ""
      GrpRwerte(i)("V").Bem = ""
      GrpRwerte(i)("V").IVoNa = False
      GrpRwerte(i)("V").Iplott = False
      If GrpRwerte(i)("V").ID >= 0 Then
        ReWrRwert.ReadRwert(GrpRwerte(i)("V").ID, GrpRwerte(i)("V"), ier)
        GrpRwerte(i)("V").Nr = i
      End If
    Next i

    RezGraphics.DataChanged = False

    '
    '
    '
    'If txtGGEMax.Text < 0.0# Then
    ' txtGGEMax.Text = MenueParam.Misch.Gge
    ' End If
    '
    Call VisGGERezept(Igx, KeyMenge)
    '
    '
    '
    RezGrid.AllRezepte = RezSozpt
    RezGrid.GrpRwerte = GrpRwerte
    RezGrid.TDBFarRezStart(ier)
    RezGrid.chkVOL.CheckState = RezSozpt.IVOL

    'Application.DoEvents()
    '
    '
    'Grid füllen
    '
    '
    '


    If RezSozpt.MngMin < 0.0# Then RezSozpt.MngMin = 0
    If RezSozpt.MngMax < RezSozpt.MngMin Then RezSozpt.MngMax = RezSozpt.MngMin
    If RezSozpt.ProzMin < 0 Then RezSozpt.ProzMin = 0
    If RezSozpt.ProzMax < RezSozpt.ProzMin Then RezSozpt.ProzMax = RezSozpt.ProzMin
    If RezSozpt.Rezepte(KeyMenge).Dicke(0) < 0 Then RezSozpt.Rezepte(KeyMenge).Dicke(0) = 0
    If RezSozpt.Rezepte(KeyMenge).Dicke(1) < 0 Then RezSozpt.Rezepte(KeyMenge).Dicke(1) = 0
    If RezSozpt.Rezepte(KeyMenge).Dicke(0) <> RezSozpt.Rezepte(KeyMenge).Dicke(1) Then
      RezSozpt.Rezepte(KeyMenge).Dicke(1) = RezSozpt.Rezepte(KeyMenge).Dicke(0)
    End If
    txtMng(0).Text = HandleRezept.AutFormat(RezSozpt.MngMin)
    txtMng(1).Text = HandleRezept.AutFormat(RezSozpt.MngMax)
    txtPro(0).Text = HandleRezept.AutFormat(100 * RezSozpt.ProzMin)
    txtPro(1).Text = HandleRezept.AutFormat(100 * RezSozpt.ProzMax)
    txtDickKor(0).Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).Dicke(0))
    txtDickKor(1).Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).Dicke(1))
    If RezSozpt.Rezepte(0).Iarch < 0 Or RezSozpt.Rezepte(0).Iarch > 2 Then
      chkARCH.CheckState = CheckState.Unchecked
    Else
      chkARCH.CheckState = RezSozpt.Rezepte(KeyMenge).Iarch
    End If
    '
    SplitContErstRezepte(1).Enabled = True
    SplitContErstRezepte(2).Enabled = True

    Cursor = Cursors.Arrow
    'Application.DoEvents()
    RezGraphics.DataChanged = False
  End Sub
  Sub VisVisRezept()
    Dim i As Short
    If IsNothing(ChkUNT) Then Exit Sub
    If ChkUNT.Count = 0 Then Exit Sub
    If ChkUNT(0).CheckState = CheckState.Indeterminate Then Exit Sub
    If ChkUNT(1).CheckState = CheckState.Indeterminate Then Exit Sub
    RezCheckRad.chkUUU(0).Checked = True
    RezCheckRad.radUUU(0).Checked = True
    RezCheckRad.radUUU(1).visible = False
    RezCheckRad.chkUUU(1).Visible = False
    If ChkUNT(0).Checked Then
      '
      chkMessUnt(0).Checked = True
      chkMessVor.Checked = True
      '
      lblDickKor(0).Visible = False
      txtDickKor(0).Visible = False
      RezCheckRad.chkUUU(0).Visible = True
      RezCheckRad.radUUU(0).Visible = True
    End If
    If Not MenueParam.Misch.Transp Then
      RezCheckRad.chkUUU(0).Visible = False
      RezCheckRad.radUUU(0).Visible = False
      ChkUNT(0).Visible = False
      ChkUNT(1).Checked = False
      chkKDE.Checked = False
      ChkUNT(1).Visible = False
      chkKDE.Visible = False
      '
      chkMessUnt(0).Checked = False
      chkMessUnt(1).Checked = False
      '
      lblDickKor(1).Visible = False
      txtDickKor(1).Visible = False
    Else
      RezCheckRad.chkUUU(0).Visible = True
      RezCheckRad.radUUU(0).Visible = True
      ChkUNT(0).Visible = True
      ChkUNT(1).Visible = True
      chkKDE.Visible = True
      '
      chkMessUnt(0).Checked = True
      chkMessUnt(1).Checked = True
      '
      lblDickKor(0).Visible = True
      txtDickKor(0).Visible = True
      lblDickKor(1).Visible = True
      txtDickKor(1).Visible = True
    End If
    '
    chkMessUnt_1.Checked = False
    '
    lblDickKor(1).Visible = False
    txtDickKor(1).Visible = False
    RezCheckRad.chkUUU(1).Visible = False
    RezCheckRad.chkUUU(1).Checked = False
    txtKDE.Visible = False
    If ChkUNT(1).Checked Then
      '
      chkMessUnt_1.Checked = True
      '
      lblDickKor(1).Visible = True
      txtDickKor(1).Visible = True
      RezCheckRad.chkUUU(1).Visible = True
      RezCheckRad.radUUU(1).Visible = True
    Else
      RezCheckRad.chkUUU(0).Checked = True
      RezCheckRad.radUUU(0).Checked = True
      RezCheckRad.radUUU(0).Visible = False
      RezCheckRad.radUUU(1).Visible = False
    End If
    If chkKDE.Checked Then
      RezCheckRad.chkUUU(1).Visible = True
      '
      chkMessUnt_1.Checked = True
      '
      lblDickKor(1).Visible = True
      txtDickKor(1).Visible = True
      txtKDE.Visible = True
    End If
    btnMessVor.Visible = chkMessVor.Checked
    cboVor_0.Visible = chkMessVor.Checked
    If GrpRwerte(i)("V").ID >= 0 Then
      GrpRwerte(i)("V").IVoNa = chkMessVor.Checked
    End If
    For i = 0 To 1

      btnMessUnt(i).Visible = chkMessUnt(i).Checked
      lblMessUnt(i).Visible = chkMessUnt(i).Checked

      If GrpRwerte(i).RefUnt.ID >= 0 Then
        GrpRwerte(i).RefUnt.IVoNa = chkMessUnt(i).Checked
      End If
    Next i
    If Not BitWrt(15, MenueParam.User.Visbl) Then
      cboMng.Visible = False
      For i = 0 To txtMng.Count - 1
        txtMng(i).Visible = False
      Next i
    End If
    If Not BitWrt(15, MenueParam.User.Enabl) Then
      cboMng.Enabled = False
      For i = 0 To txtMng.Count - 1
        txtMng(i).Enabled = False
      Next i
    End If
    If Not BitWrt(16, MenueParam.User.Visbl) Then
      cboPro_0.Visible = False
      cboPro_1.Visible = False
      lblPerc.Visible = False
      lblProVerh.Visible = False
      For i = 0 To txtPro.Count - 1
        txtPro(i).Visible = False
      Next i
    End If
    If Not BitWrt(16, MenueParam.User.Enabl) Then
      cboPro_0.Enabled = False
      cboPro_1.Enabled = False
      For i = 0 To txtPro.Count - 1
        txtPro(i).Enabled = False
      Next i
    End If
    If Not BitWrt(15, MenueParam.User.Visbl) And Not BitWrt(16, MenueParam.User.Visbl) And Not BitWrt(21, MenueParam.User.Visbl) Then
      lblmng(0).Visible = False
      lblmng(1).Visible = False
    End If
    If Not BitWrt(17, MenueParam.User.Visbl) Then
      ChkUNT(1).Visible = False
    End If
    If Not BitWrt(17, MenueParam.User.Enabl) Then
      ChkUNT(1).Enabled = False
    End If
    If Not BitWrt(18, MenueParam.User.Visbl) Then
      chkVOL.Visible = False
    End If
    If Not BitWrt(18, MenueParam.User.Enabl) Then
      chkVOL.Enabled = False
    End If
    If Not BitWrt(19, MenueParam.User.Visbl) Then
      chkKDE.Visible = False
      txtKDE.Visible = False
    End If
    If Not BitWrt(19, MenueParam.User.Enabl) Then
      chkKDE.Enabled = False
      txtKDE.Enabled = False
    End If
    If Not BitWrt(21, MenueParam.User.Visbl) Then
      cbomim(0).Visible = False
      cbomim(1).Visible = False
      lblANZ.Visible = False
    End If
    If Not BitWrt(21, MenueParam.User.Enabl) Then
      cbomim(0).Enabled = False
      cbomim(1).Enabled = False
    End If
    If Not BitWrt(22, MenueParam.User.Visbl) Then
      For i = 0 To txtDickKor.Count - 1
        txtDickKor(i).Visible = False
        lblDickKor(i).Visible = False
      Next i
    End If
    If Not BitWrt(22, MenueParam.User.Enabl) Then
      For i = 0 To txtDickKor.Count - 1
        txtDickKor(i).Enabled = False
      Next i
    End If
    If Not BitWrt(26, MenueParam.User.Visbl) Then
      cboGLZ.Visible = False
      lblGLZ.Visible = False
    End If
    If Not BitWrt(26, MenueParam.User.Enabl) Then
      cboGLZ.Enabled = False
    End If
    If Not BitWrt(27, MenueParam.User.Visbl) Then
      'cboSrMax.Visible = False
      'lblSrMax.Visible = False
    End If
    If Not BitWrt(27, MenueParam.User.Enabl) Then
      'cboSrMax.Enabled = False
    End If

    If Not BitWrt(7, MenueParam.User.Writ) Then
      btnStoreSorti.Visible = False
    End If
    If Not BitWrt(10, MenueParam.User.Sonst) Then
      cboGRPRwrt.Visible = False
    End If
    If Not BitWrt(8, MenueParam.User.Sonst) Then
      SplitContErstRezepte(1).Visible = False
    End If


    'If Not WinUSeqMe(MenueParam.User.Winkel, MenueParam.Messg.Winkel) Then
    ' cboGRPRwrt.Visible = False
    ' End If
    '
    '
    ''

  End Sub
 
  Sub VisGGERezept(ByRef Igx As Short, ByRef KeyMenge As String)
    Dim i As Integer
    Dim Bind As Boolean
    If Igx > 2 And Igx < 7 Then
      Igx = 0
      MenueParam.Misch.Igx = 0
    End If
    Bind = False
    If Igx = 2 Or Igx = 1 Then
      For i = 0 To RezSozpt.Rezepte(KeyMenge).KF - 1
        FaId = RezSozpt.Rezepte(KeyMenge)(i).ID
        KeyD = KeyName(FaId)
        If RezSozpt.Farben(KeyD).Ichf = 1 Then
          '
          '
          'Bindemittel gefunden
          '
          '
          '
          '
          Bind = True

          Exit For
        End If
      Next i
    End If
    If Bind Or Igx = 7 Then
      'panGGE.Visible = True
      'txtGGE.Visible = True
      'lblGGE.Visible = True
      'txtGGEMax.Visible = True
      'lblGGEMax.Visible = True
      'hscGGE.Visible = True
    Else
      'panGGE.Visible = False
      'txtGGE.Visible = False
      'lblGGE.Visible = False
      'txtGGEMax.Visible = False
      'lblGGEMax.Visible = False
      'hscGGE.Visible = False
    End If

    lblMINI.Visible = BitWrt(14, MenueParam.User.Visbl)
    '
    lblMINI.Text = Texxt(572 + MenueParam.Misch.Igx) & " (" & Texxt(306 + MenueParam.Menue.JABST) & ") "
  End Sub

  '
  '
  '
  '
  'Wiegeschein drucken
  '
  '
  '
  '
  Sub Wiege(ByVal index As Short, ByVal Kdru As Short)
    Dim k As Integer
    Dim LandSc As Boolean
    Dim DruAusgabe As AusgabeCreateObj
    Dim FormShow As New frmDRR
    Dim FarbWerte As ValuesGrpsAssigns
    Dim Quali As QualKontrolle
    Dim OldINF As Short
    OldINF = -1
    Dim StrName As String
    StrName = InputBox(Texxt(2104), Texxt(2000), RezSozpt.Rezepte(KeyMenge).Name)
    If StrName <> "" Then
      RezSozpt.Rezepte(KeyMenge).Name = StrName
    Else
      Exit Sub
    End If
    '
    '
    '
    Dim Printdoc As New PrintDocument
    Printdoc.PrinterSettings = MnPrintSet
    PrintPreview.Document = Printdoc

    '
    '
    '
    '
    '
    '
    '
    '
    OldINF = RezSozpt.INF
    '
    '
    'Bamng für Gesamtmenge (wg. Batchkonzentrationen
    '
    RezSozpt.INF = 0
    If index = 1 Then
      Call RezDruck.MngUmrech("BAS")
    Else
      Call RezDruck.MngUmrech("")
    End If

    '
    '
    Select Case Kdru
      Case 0
        FormShow.ShowDialog()
        RezDruck.Text0 = FormShow.txtEIN_0.Text
        RezDruck.Text1 = FormShow.txtEIN_1.Text
        RezDruck.Text2 = FormShow.txtEIN_2.Text
        LandSc = Printdoc.DefaultPageSettings.Landscape
        Printdoc.DefaultPageSettings.Landscape = True
        AddHandler Printdoc.PrintPage, AddressOf RezDruck.DruckPictureboxes
        PrintPreview.WindowState = FormWindowState.Maximized
        'Preview anzeigen 
        PrintPreview.ShowDialog()
        RemoveHandler Printdoc.PrintPage, AddressOf RezDruck.DruckPicturebox
        Printdoc.Dispose()



      Case 1
        '
        '

        '
        '
        'Wiegeschein
        '
        '
        '
        ReDim RezDruck.RefNr(1)
        RezDruck.RefNr(0) = "V"
        RezDruck.RefNr(1) = "R"
        If index = 2 Or index = 3 Then
          RezDruck.RefNr(1) = KeyRe(RezSozpt.Rezepte("RZP").Nr)
        End If
        '
        '
        '
        AddHandler Printdoc.PrintPage, AddressOf RezDruck.DruckWiegeschein

        PrintPreview.WindowState = FormWindowState.Maximized
        'Preview anzeigen    
        Printdoc.DefaultPageSettings.Landscape = False

        PrintPreview.ShowDialog()
        RemoveHandler Printdoc.PrintPage, AddressOf RezDruck.DruckWiegeschein
        Printdoc.Dispose()


      Case 2
        '
        FarbWerte = New ValuesGrpsAssigns
        Quali = New QualKontrolle

        RezGrid.AllRezepte.INF = 0
        RezGrid.TDBFARTxtAllSum()
        If index = 1 Then
          '
          'Basisrezept
          '
          RezGrid.Umr.CalcBamng("BAS", RezSozpt, ier)
        ElseIf index = 2 Or index = 3 Then
          '
          'Einzelrezepte
          '
          For k = 0 To Kzl - 1
            RezGrid.Umr.CalcBamng(KeyRe(k), RezSozpt, ier)
          Next k
        End If
        '
        '
        '
        '
        '
        '
        '
        Try
          'Wiegeschein (User-Programm)
          '
          '
          '
          Cursor = System.Windows.Forms.Cursors.WaitCursor

          DruAusgabe = New AusgabeCreateObj
          GrpRwerte(0)("V").Itp = True
          GrpRwerte(1)("V").Itp = True


          '
          '
          'Wiegeschein (User-Programm)
          '
          '
          '
          '
          '
          '
          '
          FarbWerte.clear()
          DruAusgabe.AusgabeWIEGESC(RezSozpt, FarbWerte, GrpRwerte)
          DruAusgabe = Nothing
          FarbWerte = Nothing
          Quali = Nothing
          Cursor = System.Windows.Forms.Cursors.Arrow
          RezGrid.TDBFarFill(ier)
          RezGrid.Picauf.Refresh()
        Catch
          Cursor = System.Windows.Forms.Cursors.Arrow
        End Try

        '
        '
        '
        '
        '

    End Select
    '
    '
    'Bamng für ursprüngliche Mengenart (wg. Batchkonzentrationen)
    '
    RezSozpt.INF = OldINF
    If index = 1 Then
      Call RezDruck.MngUmrech("BAS")
    Else
      Call RezDruck.MngUmrech("")
    End If
    FarbWerte = Nothing
    Quali = Nothing
    FormShow.Dispose()
  End Sub
  '
  '
  'Alle Rezepte drucken
  '
  '
  Sub RezAlle(ByVal KDrAll As Short)
    Dim DruAusgabe As AusgabeCreateObj
    Dim ier As Integer
    Dim FarbWrt As ValuesGrpsAssigns
    Dim quali As QualKontrolle
    If Not cboDruAll.Enabled Then Exit Sub
    Dim OldINF As Short
    Select Case KDrAll
      Case 0
        OldINF = -1
        '
        '
        '
        '
        '
        OldINF = RezSozpt.INF
        '
        '
        'Bamng für Gesamtmenge (wg. Batchkonzentrationen
        '
        RezSozpt.INF = 0
        Call RezDruck.MngUmrech("")
        '
        Dim Printdoc As New PrintDocument
        Printdoc.DefaultPageSettings.Landscape = False
        Printdoc.PrinterSettings = MnPrintSet
        PrintPreview.Document = Printdoc
        '
        '
        '
        Try
          AddHandler Printdoc.PrintPage, AddressOf RezDruck.DruckRezAlle
          PrintPreview.WindowState = FormWindowState.Maximized
          'Preview anzeigen    
          PrintPreview.ShowDialog()
          RemoveHandler Printdoc.PrintPage, AddressOf RezDruck.DruckRezAlle
          Printdoc.Dispose()
        Catch
        End Try
        '
        '
        '
        RezSozpt.INF = OldINF
        Call RezDruck.MngUmrech("")
      Case 1
        'Benutzerausgabe (Rezepte (alle)
        '
        '
        '
        Cursor = System.Windows.Forms.Cursors.WaitCursor

        DruAusgabe = New AusgabeCreateObj
        FarbWrt = New ValuesGrpsAssigns
        quali = New QualKontrolle
        GrpRwerte(0)("V").Itp = True
        GrpRwerte(1)("V").Itp = True
        FarbWrt.clear()
        AufbauPar.AufbauRezeptMerk(FarbWrt, ier)

        Call quali.FarbWrtCalc(Winkel, GrpRwerte, FarbWrt, ier)
        '
        '
        DruAusgabe.AusgabeRZPALLE(RezSozpt, FarbWrt, GrpRwerte)
        DruAusgabe = Nothing
        quali = Nothing
        FarbWrt = Nothing
        Cursor = System.Windows.Forms.Cursors.Arrow
    End Select


    ''
  End Sub
  Sub SpeiAll(ByRef Canc As Boolean)
    Dim jrez As Integer
    Dim i As Integer
    Dim k As Integer
    Dim kzz As Integer
    Dim RezID As Integer
    If Not btnSpeichern.Enabled Or Not btnSpeichern.Visible Then Exit Sub
    If GroupRezepte Is Nothing Then Exit Sub
    If GroupRezepte.Count = 0 Then Exit Sub
    If MessageBox.Show(Texxt(1659) & Space(1) & cboGRP.Text, Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.No Then
      Canc = True
      Exit Sub
    End If
    'RwOnly = MenueParam.Messg.RwrtOnly
    'RzOnly = MenueParam.Misch.RzpOnly
    If Not cboGRPRwrt.Text.Substring(0, 1) = "*" Then
      'MenueParam.Messg.RwrtOnly = False
    End If
    If Not cboGRP.Text.Substring(0, 1) = "*" Then
      'MenueParam.Misch.RzpOnly = False
    End If
    If RezSozpt.Rezepte.ContainsKey("RZP") Then
      RezSozpt.Rezepte.RemoveRez("RZP")
    End If
    Cndat.Open()
    For jrez = 0 To GroupRezepte.Count - 1
      TypID(0) = -1
      TypID(1) = -1
      SmpID(0) = -1
      SmpID(1) = -1
      UntID(0) = -1
      UntID(1) = -1

      For i = 0 To 1
        If GroupRwerte(jrez)(i)("V").IVoNa Then
          TypID(i) = GroupRwerte(jrez)(i)("V").ID
        End If
        If GroupRwerte(jrez)(i).RefUnt.IVoNa Then
          UntID(i) = GroupRwerte(jrez)(i).RefUnt.ID
        End If
      Next i
      kzz = GroupRezepte(jrez).Rezepte.RezCount - 2
      For k = 0 To kzz - 1
        For i = 0 To 1
          If GroupRwerte(jrez)(i)(KeyRe(k)).IVoNa Then
            SmpID(i) = GroupRwerte(jrez)(i)(KeyRe(k)).ID
            '
            'R-Werte abspeichern
            '
            '
            Call ReWrRwert.WriteRwert(SmpID(i), GroupRwerte(jrez)(i)(KeyRe(k)), ier)
          End If
        Next i
        '
        '
        'Rezept abnspeichern
        '
        '
        Call RwWrRezept.AddRezept(KeyRe(k), RezID, GroupRezepte(jrez), UntID, TypID, SmpID, ier)
      Next k
    Next jrez
    Cndat.Close()
    btnSpeichern.Enabled = False
  End Sub


  Private Sub chkUNT_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkUNT_0.CheckStateChanged, chkUNT_1.CheckStateChanged, chkKDE.CheckStateChanged
    If IsNothing(MenueParam) Then Exit Sub
    MenueParam.Misch.Kgx = chkKDE.Checked
    Call VisVisRezept()
  End Sub



  Private Sub frmColorBlockverarbeitung_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Resize
    Call ResizeChild(Me)
  End Sub

  Private Sub btnDRU_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles btnDRU_1.Click, btnDRU_2.Click
    Dim index As Short
    index = CInt(eventSender.name.Substring(7, 1))
    Call Wiege(index, Kdru)
  End Sub



  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    Dispose()
  End Sub


  Private Sub txtDickKor_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtDickKor_0.Leave, txtDickKor_1.Leave
    Dim dik As Single
    dik = Singl(sender.Text)
    RezSozpt.Rezepte(KeyMenge).Dicke(0) = dik
    RezSozpt.Rezepte(KeyMenge).Dicke(1) = dik
    txtDickKor(0).Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).Dicke(0))
    txtDickKor(1).Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).Dicke(1))
  End Sub

  Private Sub cboSRT_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cboSRT.SelectedIndexChanged
    If cboSRT.SelectedIndex < 0 Then Exit Sub
    If Kzal = 0 Then Exit Sub
    cboSRT.Enabled = False
    Ipgm = cboSRT.SelectedIndex
    MenueParam.Misch.Isor = Ipgm




    '

  End Sub



  Private Sub cboMIM_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMim_0.SelectedIndexChanged, cboMim_1.SelectedIndexChanged
    Dim index As Short
    index = CInt(sender.name.ToString.Substring(7, 1))

    If index = 0 Then
      MenueParam.Misch.MinFarb = cbomim(index).SelectedIndex + 1
    Else
      MenueParam.Misch.MaxFarb = cbomim(index).SelectedIndex + 1
    End If
    'cboSrMax.Items.Clear()
    If cbomim(1).SelectedIndex = -1 Or cbomim(0).SelectedIndex = -1 Then Exit Sub
    'cboSrMax.Items.Clear()
    'For i = 1 To CShort(cbomim(1).Text) - CShort(cbomim(0).Text) + 1
    'cboSrMax.Items.Add(CStr(CDbl(cbomim(0).Text) + i - 1))
    'Next i
    'cboSrMax.SelectedIndex = cboSrMax.Items.Count - 1
  End Sub






  Private Sub hscREZ_Scroll(ByVal sender As Object, ByVal e As System.Windows.Forms.ScrollEventArgs) Handles hscREZ.Scroll
    Dim i As Integer
    Dim j As Integer
    Dim RzNr As String
    Dim irez As Integer
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    If sender.value < 0 Then Exit Sub
    If RezSozpt.Rezepte.ContainsKey("RZP") Then
      RezSozpt.Rezepte.RemoveRez("RZP")
    End If
    If sender.value > RezSozpt.Rezepte.RezCount - 2 Then Exit Sub
    If IsNothing(RezGrid) Then Exit Sub
    If IsNothing(RezSozpt) Then Exit Sub
    If IsNothing(RezGraphics) Then Exit Sub
    Cursor = System.Windows.Forms.Cursors.WaitCursor
    irez = sender.value
    RzNr = KeyRe(irez)

    '
    '

    '
    'lblNRR.Text = CStr(Isor)
    '
    '
    'Umhängen Rezepte
    '
    '
    '
   
    '
    RezSozpt.Rezepte.AddRez("RZP", RezSozpt.Rezepte(RzNr))


    '
    'Umhängen R-Werte
    '
    '
    '
    RezGraphics.PlotRwerte.clear()
    j = -1
    For i = 0 To 1
      If GrpRwerte(i)("V").IVoNa Then
        j = j + 1
        RezGraphics.PlotRwerte.Add(KeyRe(j), GrpRwerte(i)("V"))
      End If
    Next i
    For i = 0 To 1
      If GrpRwerte(i)(RzNr).IVoNa Then
        j = j + 1
        RezGraphics.PlotRwerte.Add(KeyRe(j), GrpRwerte(i)(RzNr))
      End If
    Next i
    RezGraphics.FawrtRwerte(0).clear()
    RezGraphics.FawrtRwerte(1).clear()
    RezGraphics.FawrtRwerte(0).Add(KeyRe(0), GrpRwerte(0)("V"))
    RezGraphics.FawrtRwerte(0).Add(KeyRe(1), GrpRwerte(0)(RzNr))
    RezGraphics.FawrtRwerte(1).Add(KeyRe(0), GrpRwerte(1)("V"))
    RezGraphics.FawrtRwerte(1).Add(KeyRe(1), GrpRwerte(1)(RzNr))
    RezGraphics.RzName(0) = "RZP"
    RezGraphics.Text = GrpRwerte(0)("V").Name
    RezGraphics.Rmax = -1
    RezGraphics.Rmin = -1
    RezGrid.TDBFarRezStart(ier)
    RezCheckRad.Picauf.Refresh()
    lblRname.Text = GrpRwerte(0)(RzNr).Name
    Application.DoEvents()
    Cursor = System.Windows.Forms.Cursors.Arrow
  End Sub

  Sub GetNameID(ByRef ID As Integer, ByRef RefName As String, ByRef Banum As String)
    Dim j As Short
    Dim ier As Short
    ier = 0
    If RefName <> "" Then
      '
      If RcmdRef = 0 Then Exit Sub
      'If GetPutReflex.HideSofort Then
      ' GetPutReflex.InVisible()
      'End If
      '
      If RcmdRef > 0 Then
        ReUn = "V"
        j = RcmdRef - 11
        ReWrRwert.ReadRwert(ID, GrpRwerte(j)(ReUn), ier)
      ElseIf RcmdRef < 0 Then
        j = -RcmdRef - 1
        ReUn = "V"
        ReWrRwert.ReadRwert(ID, GrpRwerte(j).RefUnt, ier)
      End If
      If ier = 0 Then

        Select Case RcmdRef
          Case 11, 12
            RowRwerte = TblRwerte.NewRow
            RowRwerte("RwertID") = GrpRwerte(j)(ReUn).ID
            RowRwerte("RwertName") = GrpRwerte(j)(ReUn).Name
            RowRwerte("RwertDattim") = GrpRwerte(j)(ReUn).DatTim
            TblRwerte.Rows.Add(RowRwerte)
            'lblMessVor(j).Text = GrpRwerte(j)(ReUn).Name
            'TypID(RezGraphics.Vkwb(j)) = ID
            'GrpRwerte(j)(ReUn).IVoNa = True
          Case -1, -2
            lblMessUnt(-RcmdRef - 1).Text = GrpRwerte(j).RefUnt.Name
            UntID(RezGraphics.Vkwb(j)) = ID
            GrpRwerte(j).RefUnt.IVoNa = True
        End Select
      End If

      'RefName = ""
    End If

  End Sub
  Sub RezActiv(i As Integer)
    Dim j As Short
    For j = 0 To RezptGrid.Count - 1
      If Not IsNothing(RezptGrid(j)) Then
        RezptGrid(j).chkVOL = Nothing
      End If
    Next
    RezptGrid(i).chkVOL = chkVOL
  End Sub
  Private Sub btnMessVor_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnMessVor.Click
    Dim Index As Short

    '
    '
    TblRwerte.Rows.Clear()
    Index = 0
    RcmdRef = 11
    'GetPutReflex.Kenn = MenueParam.Messg.Kenn
    GetPutReflex.Messrefel = GrpRwerte(Index)("V")
    GetPutReflex.Iarch = 0
    GetPutReflex.Captext = btnMessVor.Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(Index), RezGraphics.Vkwb(Index), MenueParam.Messg.MeArtID)
    Me.Enabled = False
    GetPutReflex.ReflexWerte(True)
    Me.Enabled = True
    'MenueParam.Messg.MeArtLock = BitInt(RezGraphics.Vkwb(Index), RezGraphics.Vkwb(Index), MenueParam.Messg.MeArtID)


    '
    '
    ''
  End Sub

  Private Sub btnMessUnt_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnMessUnt_0.Click, btnMessUnt_1.Click
    Dim Index As Short
    Index = CInt(sender.name.substring(11, 1))
    '
    '
    '
    RcmdRef = -Index - 1

    '
    'Messung
    '
    '
    GetPutReflex.Messrefel = GrpRwerte(Index).RefUnt

    'GetPutReflex.Kenn = MenueParam.Messg.Kenn
    GetPutReflex.Iarch = 1
    GetPutReflex.Captext = btnMessUnt(Index).Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(Index), RezGraphics.Vkwb(Index), MenueParam.Messg.MeArtID)
    MenueParam.Messg.MeArtLock = BitInt(RezGraphics.Vkwb(Index), RezGraphics.Vkwb(Index), MenueParam.Messg.MeArtID)
    Me.Enabled = False
    GetPutReflex.ReflexWerte(False)
    Me.Enabled = True
    MenueParam.Messg.MeArtLock = BitInt(RezGraphics.Vkwb(Index), RezGraphics.Vkwb(Index), MenueParam.Messg.MeArtID)

    ''
  End Sub





  Private Sub btnStoreSorti_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnStoreSorti.Click
    RezGraphics.DataChanged = True
    Call HandleRezept.SorSpei(RezGraphics.DataChanged, lstSOR, KeyMenge, RezSozpt, OleAdSorti, DsetSortRezptRwrt.Tables("tblsorti"), RwWrSortim, UntID, TypID, ier)
    RezGraphics.DataChanged = False
  End Sub




  Private Sub cboGLZ_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboGLZ.Click
    Winkel(0).GK(0) = CSng(cboGLZ.Text)
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
  '
  '





  Private Sub btnRezepte_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSortiment.Click, btnErstrezept.Click, btnAlleRezepte.Click
    Dim i As Integer
    Dim k As Integer
    Dim Jrez As Integer
    Dim Isor As Integer
    Dim irmax As Integer
    Dim Krez As String
    Dim Kzus As String
    Dim Kzz As Integer
    Dim Count As Integer
    Dim index As Short
    Dim ier As Integer
    Dim DG0 As Single
    Dim dial As DialogResult = Windows.Forms.DialogResult.None
    ier = 0
    Select Case sender.name
      Case "btnSortiment"
        index = 0
      Case "btnErstrezept"
        index = 1
      Case "btnAllerezepte"
        index = 2
    End Select
    Select Case index
      Case 0
        '
        'Sortiment
        '
        '
        'Winkel/Licharten
        '
        '
        '
        For i = 0 To RezCheckRad.chkUUU.Count - 1
          RezCheckRad.chkUUU(i).Enabled = False
          RezCheckRad.radUUU(i).enabled = False
        Next i
        For i = 0 To Winkel.Km - 1
          RezCheckRad.chkWIN(i).enabled = False
          RezCheckRad.radWIN(i).enabled = False
        Next i
        For i = 0 To MenueParam.Normfa.Nlz - 1
          RezCheckRad.radNLA(i).enabled = False
        Next i
        cboMsh.Enabled = True


        '
        '
        '
        '
        '
        '
        KeyMenge = "SOR"
        Call RezActiv(0)
        RezGrid = RezptGrid(0)
        RezCheckRad.cboSKAL.Enabled = False

        '
        '
        '
        '
        '
        '
        '
        RezSozpt.Rezepte("BAS").clear()
        btnAlleRezepte.Enabled = False
        btnSpeichern.Enabled = False
        btnGrid.Enabled = False
        SplitContVisible = 0
        cboSRT.Enabled = True
        '
      Case 1
        '

        '
        '
        ''Prüfen, ob richtige Gruppe für Rezepte ausgewählt
        'Wird nur geprüft, falls btnspeichern=visible (z.Zt. ausgeschaltet!!)
        '
        '
        '
        '

        If btnSpeichern.Visible Then
          If cboGRP.SelectedValue = 0 Then
            dial = MessageBox.Show(Texxt(3936) & Space(2) & cboGRP.Text & Chr(13) & Texxt(3937), Texxt(2000), MessageBoxButtons.YesNo)
            If dial = Windows.Forms.DialogResult.No Then
              SplitContVisible = 0
              Exit Sub
            ElseIf dial = Windows.Forms.DialogResult.Yes Then
              btnSpeichern.Visible = False
            End If
          Else
            '
            '
            'Prüfen, ob Rezept-Gruppe leer
            '
            '
            '
            SqlStmt = "SELECT * FROM TBL_REZEPT WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_GID=" & cboGRP.SelectedValue
            CmdGroup.CommandText = SqlStmt
            CmdGroup.Connection = Cndat()
            '
            '
            '
            ReadGroup = DataReader(CmdGroup, CommandBehavior.SingleResult, Cndat)
            If ReadGroup.Read Then
              dial = MessageBox.Show(Texxt(3933) & Space(2) & cboGRP.Text & Chr(13) & Texxt(3935), Texxt(2000), MessageBoxButtons.YesNoCancel)
            End If
            ReadGroup.Close()
            If dial = Windows.Forms.DialogResult.No Then
              btnSpeichern.Visible = False
            ElseIf dial = Windows.Forms.DialogResult.Cancel Then
              SplitContVisible = 0
              Exit Sub
            End If
          End If


          '
          'Prüfen, ob richtige Gruppe für R-Werte ausgewählt
          '
          '


          If cboGRPRwrt.SelectedValue = 0 Then
            dial = MessageBox.Show(Texxt(3936) & Space(2) & cboGRPRwrt.Text & Chr(13) & Texxt(3937), Texxt(2000), MessageBoxButtons.YesNo)
            If dial = Windows.Forms.DialogResult.No Then
              SplitContVisible = 0
              Exit Sub
            ElseIf dial = Windows.Forms.DialogResult.Yes Then
              btnSpeichern.Visible = False
            End If
          Else
            SqlStmt = "SELECT * FROM TBL_RWERT WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_GID=" & cboGRPRwrt.SelectedValue
            CmdGroup.CommandText = SqlStmt
            CmdGroup.Connection = Cndat()
            '
            '
            '
            ReadGroup = DataReader(CmdGroup, CommandBehavior.SingleResult, Cndat)

            If ReadGroup.Read Then
              dial = MessageBox.Show(Texxt(3934) & Space(2) & cboGRPRwrt.Text & Chr(13) & Texxt(3935), Texxt(2000), MessageBoxButtons.YesNoCancel)
            End If
            ReadGroup.Close()
            If dial = Windows.Forms.DialogResult.No Then
              btnSpeichern.Visible = False
            ElseIf dial = Windows.Forms.DialogResult.Cancel Then
              SplitContVisible = 0
              Exit Sub
            End If
          End If
        End If

        '
        '
        'Prüfen, ob Reflexionswerte vorhanden
        '
        '




        If cboVor(0).Items.Count = 0 Then
          MsgBox(Texxt(4051))
          Exit Sub
        End If
        '
        For i = 0 To 1
          If chkMessUnt(i).Checked Then
            If Not GrpRwerte(i).RefUnt.IVoNa Then
              MsgBox(Texxt(2964 + i))
              GoTo rezeptefeh
            End If
          End If
        Next i
        '
        '
        '
        '
        'Sortiment abspeichern
        '
        '
        '
        '
        '
        '

        SplitContainErstrezepte.Visible = False
        'cboWin.Enabled = True
        If KeyMenge = "SOR" Then
          RezGraphics.DataChanged = True
          Call HandleRezept.SorSpei(RezGraphics.DataChanged, lstSOR, KeyMenge, RezSozpt, OleAdSorti, DsetSortRezptRwrt.Tables("tblsorti"), RwWrSortim, UntID, TypID, ier)
          RezGraphics.DataChanged = False

        End If
        If ier <> 0 Then
          GoTo RezepteFeh
        Else
          Cursor = Cursors.WaitCursor
        End If

        '
        '
        'Winkel/Licharten
        '
        '
        '
        'cboWin.Enabled = True
        For i = 0 To Winkel.Km - 1
          RezCheckRad.chkWIN(i).Enabled = True
          RezCheckRad.radWIN(i).Enabled = True
        Next i
        '
        For i = 0 To MenueParam.Normfa.Nlz - 1
          RezCheckRad.radNLA(i).Enabled = True
        Next i
        '
        For i = 0 To RezCheckRad.chkUUU.Count - 1
          RezCheckRad.chkUUU(i).Enabled = True
          RezCheckRad.radUUU(i).enabled = True
        Next i
        '
        '
        chkFAR.Enabled = True
        cboMsh.Enabled = False
        '
        '


        If Not HandleRezept.IVorNa("Z", GrpRwerte, ChkUNT, chkKDE, Nothing) Then
          SplitContVisible = 0
          Exit Sub
        End If
        If Not HandleRezept.IVorNa("V", GrpRwerte, ChkUNT, chkKDE, Nothing) Then
          SplitContVisible = 0
          Exit Sub
        End If

        SplitContVisible = index
        lblARB.Visible = True
        RezCheckRad.chkUUU(0).CheckState = CheckState.Indeterminate
        RezCheckRad.chkUUU(1).CheckState = CheckState.Indeterminate
        Cursor = Cursors.WaitCursor
        If RezSozpt.Rezepte.ContainsKey("RZP") Then
          RezSozpt.Rezepte.RemoveRez("RZP")
        End If


        '
        '

        '
        '
        '
        'Erstrezept
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
        '
        btnErstrezept.Enabled = False
        btnAlleRezepte.Enabled = False
        btnSortiment.Enabled = False
        Me.Enabled = False
        RezptGrid(1).TDBFar.Visible = False
        For i = 0 To Picauf.Count - 1
          Picauf(i).Visible = False
        Next i
        lblARB.SetBounds(Me.Location.X, Me.Location.Y, _
        Me.Size.Width, Me.Size.Height)
        lblARB.Visible = True
        Application.DoEvents()
        '
        Picauf = PicAufBau(1)
        RezCheckRad.Picauf = Picauf
        '
        '
        '
        '
        '
        '
        '****************************
        'Berechnung der Erstrezepte
        '****************************
        '
        '
        '
        '
        Isor = cboSRT.SelectedIndex
        DG0 = MenueParam.Misch.DeGut

        KeyMenge = "BAS"
        KeySor = "SOR"
        SplitContainErstrezepte.Visible = False
        '
        '
        '
        '
        '
        'Blockverarbeitung
        '
        '
        GrpRwerte(1)("V").IVoNa = False
        TypID(1) = -1
        '
        '
        '
        'Schichtdicken
        '
        '
        For i = 0 To 1
          RezSozpt.Rezepte(KeySor).Dicke(i) = CSng(txtDickKor(i).Text)
        Next i
        GrpRwerte(0)("V").IVoNa = True

        If RezSozpt.Rezepte.ContainsKey("RZP") Then
          RezSozpt.Rezepte.RemoveRez("RZP")
        End If
        GroupRezepte.Clear()
        GroupRwerte.Clear()
        '
        '
        'Schleife über alle Vorlagen
        '
        lblARB.Cursor = Cursors.WaitCursor
        Application.DoEvents()

        'Abbruch = False
        '
        '
        'Löschen aller Rezeptgruppen und R-Wertgruppen
        '
        GroupRezepte.Clear()
        GroupRwerte.Clear()
        '
        'Sortierung
        '
        '
        Ipgm = cboSRT.SelectedIndex

        '
        For Jrez = 0 To TblRwerte.Rows.Count - 1
          '
          'numerische Sclüssel für Rezepte löschen 
          '
          '
          '
          Count = RezSozpt.Rezepte.RezCount
          For k = Count - 1 To 0 Step -1
            If IsNumeric(RezSozpt.Rezepte.RezKey(k)) Then
              RezSozpt.Rezepte.RemoveRez(RezSozpt.Rezepte.RezKey(k))
            End If
          Next
          '
          'numerische Sclüssel für Rwerte löschen 
          '
          '
          '
          For i = 0 To 1
            Count = GrpRwerte(i).Count
            For k = Count - 1 To 0 Step -1
              If IsNumeric(GrpRwerte(i).RwKey(k)) Then
                GrpRwerte(i).Remove(GrpRwerte(i).RwKey(k))
              End If
            Next k
          Next i
          GrpRwerte(0)("V").ID = TblRwerte.Rows(Jrez)("RwertID")
          ReWrRwert.ReadRwert(GrpRwerte(0)("V").ID, GrpRwerte(0)("V"), ier)
          lblVRR.Text = GrpRwerte(0)("V").Name
          TypID(0) = GrpRwerte(0)("V").ID
          '
          '

          '



          CalcRezept.BasisRezept(RezGraphics.KFIT + 8, Winkel, KeySor, KeyMenge, RezSozpt, GrpRwerte, ier)
          If ier <> 0 Then
            GoTo rezepteFeh
          End If
          CalcRezept.ErstRezept(RezGraphics.KFIT + 8, Winkel, KeyMenge, KeyMenge, RezSozpt, GrpRwerte, ier)
          If ier <> 0 Then
            GoTo rezepteFeh
          End If
          '
          '
          '
          '
          '
          'Sortieren
          '
          Kzal = RezSozpt.Rezepte.RezCount - 2
          Kzz = Kzal
          irmax = CInt(cbomim(1).Text)
          Kzz = CInt(txtNANZ.Text)


          If Kzal = 0 Then
            GoTo rezeptefeh
          Else
            CalcRezept.SorRez(Ipgm, irmax, Kzl, Ksor, DG0, RezSozpt, ier)
            If Kzl = 0 Then
              GoTo rezeptefeh
            End If
          End If


          '
          '
          '
          '
          '
          '
          '
          '
          'R-Werte und Rezepte abspeichern
          '
          '
          '
          If Kzz > Kzl Then
            Kzz = Kzl
          End If
          For i = 0 To Kzz - 1
            '
            '
            '
            'Rezepte übernehmen
            '
            '
            '



            SmpID(0) = -1
            SmpID(1) = -1
            Krez = KeyRe(Ksor(i))
            Kzus = KeyRe(i)
            '  
            '
            '
            'R-Werte übernehmen
            '
            '
            '
            'Vorlage
            '
            '

            '
            '
            'R-werte
            '
            '
            GrpRwerte(0)(Krez).Gid = cboGRPRwrt.SelectedValue
            GrpRwerte(0)(Krez).DatTim = Now
            GrpRwerte(0)(Krez).Iarch = 2
            GrpRwerte(0)(Krez).Name = Trim(GrpRwerte(0)("V").Name) & Space(2) & Kzus
            '

            If GrpRwerte(1)(Krez).IVoNa Then
              GrpRwerte(1)(Krez).Name = Trim(GrpRwerte(1)("V").Name) & Space(2) & Kzus & "(S)"
              GrpRwerte(1)(Krez).Gid = cboGRPRwrt.SelectedValue
              GrpRwerte(1)(Krez).DatTim = Now
              GrpRwerte(1)(Krez).Iarch = 2
            End If
            '
            '
            '
            'R-Werte übernehmen
            '

            RezSozpt.Rezepte(Krez).DatTim = GrpRwerte(0)(Krez).DatTim
            RezSozpt.Rezepte(Krez).Gid = cboGRP.SelectedValue
            RezSozpt.Rezepte(Krez).Name = GrpRwerte(0)(Krez).Name
          Next i
          '
          '
          '
          '
          GroupRwerte.Add(GrpRwerte.clone)
          GroupRezepte.Add(RezSozpt.clone)
          '
          '
          '
          'nummerische Schlüssel für Rezepte löschen
          '
          '
          Count = GroupRezepte(Jrez).Rezepte.RezCount
          For k = Count - 1 To 0 Step -1
            If IsNumeric(GroupRezepte(Jrez).Rezepte.RezKey(k)) Then
              GroupRezepte(Jrez).Rezepte.RemoveRez(GroupRezepte(Jrez).Rezepte.RezKey(k))
            End If
          Next k
          '
          'numerische Sclüssel für Rwerte löschen 
          '
          '
          '
          For i = 0 To 1
            Count = GroupRwerte(Jrez)(i).Count
            For k = Count - 1 To 0 Step -1
              If IsNumeric(GroupRwerte(Jrez)(i).RwKey(k)) Then
                GroupRwerte(Jrez)(i).Remove(GroupRwerte(Jrez)(i).RwKey(k))
              End If
            Next k
          Next i
          '
          '
          'Schlüssel gemäß Sortierung vergeben
          '
          '
          For k = 0 To Kzz - 1
            GroupRezepte(Jrez).Rezepte.AddRez(KeyRe(k), RezSozpt.Rezepte(KeyRe(Ksor(k)).Clone))
          Next k
          For i = 0 To 1
            For k = 0 To Kzz - 1
              GroupRwerte(Jrez)(i).Add(KeyRe(k), GrpRwerte(i)(KeyRe(Ksor(k)).Clone))
            Next k
          Next i
          '
        Next Jrez
        Erase Ksor
        '
        SplitContainErstrezepte.Visible = True

        Application.DoEvents()
        If ier <> 0 Then
          MessageBox.Show(Texxt(ier))
          GoTo rezeptefeh
        End If '
        '
        '
        '
        '
        '
        'btnAbbruch.Enabled = False
        btnErstrezept.Enabled = True
        btnSortiment.Enabled = True
        Me.Enabled = True
        cboSRT.Enabled = False


        RezptGrid(1).TDBFar.Visible = True
        For i = 0 To Picauf.Count - 1
          Picauf(i).Visible = True
        Next i
        Picauf.PicRezRezept.Visible = Not chkFAR.Checked
        Picauf.PicRezFarb.Visible = chkFAR.Checked
        FillGrid.TableAufbau()
        FillGrid.GridAufbau()
        lblARB.Visible = False
        'Application.DoEvents()

        '
        '
        '


        lblARB.Cursor = Cursors.Default

        ' 
        '
        '
        '
        '
        '
        '

        Call RezActiv(1)
        RezGrid = RezptGrid(1)
        RezCheckRad.Picauf = Picauf
        RezCheckRad.FillGrid = FillGrid
        RezCheckRad.Picauf.Refresh()
        '
        '
        '
        '
        RezDruck.KeyNam = "RZP"

        '
        '
        '
        '
        '
        '
        '
        '
        '

        btnAlleRezepte.Enabled = True
        btnSpeichern.Enabled = True
        btnGrid.Enabled = True


        '
        '

        '
        '
        '
        '
        '
        '
        '
        RezCheckRad.cboSKAL.Enabled = True
        '
        '

        '
        '
        Call MakeTabGRIDRezWerte(TabGridBase, flgGrid)

        Call FillGridBase(RezGraphics.Kwop)

        '
        'btnAllerezepte.Enabled = True
        '
        CnnRwerte.ResetBindings(False)
       


        '
        '
      Case 2
        '
        '
        Picauf = PicAufBau(2)
        For i = 0 To Picauf.Count - 1
          Picauf(i).Visible = True
        Next i
        RezCheckRad.Picauf = Picauf
        SplitContVisible = 2
        hscREZ.Value = flgREZ.CurrentRow.Index
        Call hscREZ_Scroll(hscREZ, New ScrollEventArgs(ScrollEventType.EndScroll, flgREZ.CurrentRow.Index))

        '
        '
        '
    End Select
    RezCheckRad.chkUUU(0).Checked = True
    RezCheckRad.chkUUU(1).Checked = False

    lblARB.Visible = False
    Cursor = Cursors.Arrow
    Application.DoEvents()
    'Fehler
    '
    Exit Sub
RezepteFeh:
    Me.Enabled = True
    SplitContainErstrezepte.Visible = True
    SplitContVisible = 0
    btnSortiment.Enabled = True
    btnErstrezept.Enabled = True
    Cursor = Cursors.Default
  End Sub





  Private Sub cboDru_DropDownClosed(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboDRU_1.DropDownClosed, cboDRU_2.DropDownClosed
    Dim index As Short
    Dim Kdru As Short
    index = CInt(sender.name.Substring(7, 1))
    Kdru = sender.selectedindex
    Call Wiege(index, Kdru)
  End Sub



  Private Sub lstSOR_KeyUp(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstSOR.KeyUp
    If e.KeyCode = Keys.Delete Then
      Call lstSOR_DoubleClick(sender, e)
    End If
  End Sub
  Private Sub lstSOR_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstSOR.DoubleClick
    Dim imsg As DialogResult
    '
    '
    'Sortiment löschen
    '
    '
    '
    If DsetSortRezptRwrt.Tables("TblSorti").Rows.Count = 0 Then Exit Sub
    If BitWrt(6, MenueParam.User.Writ) Then
      If DsetSortRezptRwrt.Tables("TblSorti").Rows(lstSOR.SelectedIndex)("USER_ID") <> MenueParam.UserID Then
        imsg = MessageBox.Show(Texxt(3608) & Space(2) & CStr(DsetSortRezptRwrt.Tables("TblSorti").Rows(lstSOR.SelectedIndex)("USER_ID")), Texxt(2000), MessageBoxButtons.OK)
        Exit Sub
      End If
      imsg = MessageBox.Show(Texxt(3025) & DsetSortRezptRwrt.Tables("TblSorti").Rows(lstSOR.SelectedIndex)("SORTI_NAME") & Space(1) & Texxt(3026), Texxt(2000), MessageBoxButtons.YesNo)
      If imsg = Windows.Forms.DialogResult.No Then
        Exit Sub
      End If
      RwWrSortim.DelSortim(DsetSortRezptRwrt.Tables("TblSorti").Rows(lstSOR.SelectedIndex)("SORTI_ID"), ier)
      If Not FillDatset(OleAdSorti, DsetSortRezptRwrt, "TblSorti") Then
        Exit Sub
      End If
      RezGraphics.DataChanged = False
    End If
  End Sub




  'Private Sub btnSPE_Click(ByVal sender As Object, ByVal e As System.EventArgs)
  ' Dim Iarch As Integer
  ' Dim RzNr As String
  '   If chkARCH.Checked Then
  '     Iarch = 1
  '   Else
  '     Iarch = 0
  '   End If
  '   RzNr = "RZP"
  '   Call HandleRezept.RezSpei(Iarch, RzNr, RezSozpt, UntID, TypID, SmpID, ier)
  ' End Sub

  WriteOnly Property SplitContVisible As Integer
    Set(value As Integer)
      Dim i As Integer
      For i = 0 To SplitContErstRezepte.Count - 1
        SplitContErstRezepte(i).Visible = False
      Next i
      SplitContErstRezepte(value).Visible = True
    End Set
  End Property

  Private Sub btnWEI_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnWEI.Click
    Picauf = PicAufBau(1)
    '
    RezCheckRad.Picauf = Picauf
    RezCheckRad.Picauf.Refresh()
    SplitContVisible = 1
  End Sub








  Private Sub chkFAR_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkFAR.CheckedChanged
    If chkFAR.Checked Then
      picRezRezept_1.Visible = False
      picRezRezept_1.Visible = False
      picRezFarb_1.Visible = True
      picRezFarb_1.Visible = True
    Else
      picRezRezept_1.Visible = True
      picRezRezept_1.Visible = True
      picRezFarb_1.Visible = False
      picRezFarb_1.Visible = False
    End If
  End Sub


  Private Sub btnDruAll_Click(sender As Object, e As System.EventArgs) Handles btnDruAll.Click
    Call RezAlle(KdrAll)
  End Sub


  Private Sub cboDruAll_DropDownClosed(sender As Object, e As System.EventArgs) Handles cboDruAll.DropDownClosed
    Call RezAlle(cboDruAll.SelectedIndex)
  End Sub

  Private Sub chkRwertAngleich_CheckStateChanged(sender As Object, e As System.EventArgs) Handles chkRwertAngleich.CheckStateChanged
    If chkRwertAngleich.Checked Then
      RezGraphics.KFIT = 2
      'chkRefGew.Checked = False
      'panGGE.Enabled = False
      'chkRefGew.Enabled = False
    Else
      'panGGE.Enabled = True
      'chkRefGew.Enabled = True

      RezGraphics.KFIT = 1
      'chkRefGew.Visible = False
      'chkRefGew.Checked = False

    End If
  End Sub

  Private Sub btnKOP_Click(sender As System.Object, e As System.EventArgs) Handles btnKOP.Click
    If flgREZ.Visible Then
      '
      'Tabellen in Zwischenablage
      '
      '
      flgREZ.MultiSelect = True
      flgREZ.ClearSelection()
      flgREZ.SelectionMode = DataGridViewSelectionMode.ColumnHeaderSelect
      '
      Clipboard.Clear()
      flgREZ.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableWithAutoHeaderText
      flgREZ.SelectAll()

      If flgREZ.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
        Clipboard.SetDataObject(flgREZ.GetClipboardContent)
      End If
      flgREZ.ClearSelection()
      flgREZ.SelectionMode = DataGridViewSelectionMode.RowHeaderSelect
      flgREZ.MultiSelect = False

    End If
  End Sub

  Private Sub flgREZ_CellClick(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles flgREZ.CellClick
    hscREZ.Value = flgREZ.CurrentRow.Index
    Call hscREZ_Scroll(hscREZ, New ScrollEventArgs(ScrollEventType.EndScroll, flgREZ.CurrentRow.Index))
  End Sub

  Private Sub flgREZ_RowHeaderMouseClick(sender As Object, e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles flgREZ.RowHeaderMouseClick
    '
    '
    Picauf = PicAufBau(1)
    '
    RezCheckRad.Picauf = Picauf
    SplitContVisible = 1
    hscREZ.Value = flgREZ.CurrentRow.Index
    Call hscREZ_Scroll(hscREZ, New ScrollEventArgs(ScrollEventType.EndScroll, flgREZ.CurrentRow.Index))
  End Sub








  Private Sub CnnRwerte_CurrentChanged(sender As Object, e As System.EventArgs) Handles CnnRwerte.CurrentChanged
    Dim i As Integer
    If IsNothing(CnnRwerte) Then Exit Sub
    If CnnRwerte.Position = -1 Then Exit Sub
    If GroupRezepte.Count = 0 Then Exit Sub
    If GroupRwerte.Count = 0 Then Exit Sub
    RezSozpt = GroupRezepte(CnnRwerte.Position)
    GrpRwerte = GroupRwerte(CnnRwerte.Position)
    '
    '
    '
    RezGraphics.AllRezepte = RezSozpt
    RezGraphics.GrpRwerte = GrpRwerte
    '
    '
    '
    '
    '
    RezDruck.AllRezepte = RezSozpt
    RezDruck.GrpRwerte = GrpRwerte
    '
    '
    RezGrid.AllRezepte = RezSozpt
    RezGrid.GrpRwerte = GrpRwerte

    '
    If RezSozpt.Rezepte.ContainsKey("RZP") Then
      RezSozpt.Rezepte.RemoveRez("RZP")
    End If
    '
    '
    Kzl = RezSozpt.Rezepte.RezCount - 2
    If Kzl = 0 Then
      MsgBox(Texxt(2958))
      SplitContErstRezepte(0).Visible = True
    ElseIf Kzl = 1 Then
      hscREZ.Maximum = (Kzl + hscREZ.LargeChange - 1)
      hscREZ.Minimum = 0
      hscREZ.Value = 1
      hscREZ.Value = 0
      hscREZ.Maximum = (0 + hscREZ.LargeChange - 1)
    Else
      hscREZ.Maximum = (Kzl - 1 + hscREZ.LargeChange - 1)
      hscREZ.Minimum = 0
      hscREZ.Value = Kzl - 1
      hscREZ.Value = 0
    End If
    Call hscREZ_Scroll(hscREZ, New ScrollEventArgs(ScrollEventType.EndScroll, hscREZ.Value))

    '
    '
    '
    '
    '
    'Grid füllen
    '
    '
    '
    RezGraphics.Ksrt.Clear()
    For i = 0 To Kzl - 1
      RezGraphics.Ksrt.Add(i)
    Next i

    FillGrid.FillUeGrid()
   
    '
    '
    'Rezept auswählen
    '
    '
    '
  End Sub

  
  Sub MakeTabGRIDRezWerte(ByRef Tabwerte As DataTable, ByRef flgGrid As DataGridView)

    Tabwerte.Rows.Clear()
    Tabwerte.Columns.Clear()
    Tabwerte.Columns.Add("V", GetType(Integer))
    Tabwerte.Columns.Add("N", GetType(String))
    Tabwerte.Columns.Add(Texxt(824), GetType(String))
    Tabwerte.Columns.Add("DE", GetType(Single))
    Tabwerte.Columns.Add("DL", GetType(Single))
    Tabwerte.Columns.Add("DC", GetType(Single))
    Tabwerte.Columns.Add("DH", GetType(Single))
    Tabwerte.Columns.Add("Da", GetType(Single))
    Tabwerte.Columns.Add("Db", GetType(Single))

    flgGrid.Columns(0).Visible = False
    flgGrid.Columns(1).Visible = False

    flgGrid.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.NotSet
    flgGrid.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.NotSet
    flgGrid.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgGrid.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgGrid.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgGrid.Columns(5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgGrid.Columns(6).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgGrid.Columns(7).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgGrid.Columns(8).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    '
    flgGrid.Columns(3).DefaultCellStyle.Format = "###.00"
    flgGrid.Columns(4).DefaultCellStyle.Format = "###.00"
    flgGrid.Columns(5).DefaultCellStyle.Format = "###.00"
    flgGrid.Columns(6).DefaultCellStyle.Format = "###.00"
    flgGrid.Columns(7).DefaultCellStyle.Format = "###.00"
    flgGrid.Columns(8).DefaultCellStyle.Format = "###.00"

    '
    '
    flgGrid.Columns(0).SortMode = DataGridViewColumnSortMode.NotSortable
    flgGrid.Columns(1).SortMode = DataGridViewColumnSortMode.NotSortable
    flgGrid.Columns(2).SortMode = DataGridViewColumnSortMode.NotSortable
    flgGrid.Columns(3).SortMode = DataGridViewColumnSortMode.NotSortable
    flgGrid.Columns(4).SortMode = DataGridViewColumnSortMode.NotSortable
    flgGrid.Columns(5).SortMode = DataGridViewColumnSortMode.NotSortable
    flgGrid.Columns(6).SortMode = DataGridViewColumnSortMode.NotSortable
    flgGrid.Columns(7).SortMode = DataGridViewColumnSortMode.NotSortable
    flgGrid.Columns(8).SortMode = DataGridViewColumnSortMode.NotSortable



    flgGrid.Columns(0).Width = 0
    flgGrid.Columns(1).Width = 0
    flgGrid.Columns(2).Width = 150
    flgGrid.Columns(3).Width = 50
    flgGrid.Columns(4).Width = 50
    flgGrid.Columns(5).Width = 50
    flgGrid.Columns(6).Width = 50
    flgGrid.Columns(7).Width = 50
    flgGrid.Columns(8).Width = 50

  End Sub
  Sub FillGridBase(ByVal Kw As Integer)
    Dim RzNr As String
    Dim i As Integer
    Dim k As Integer
    Dim kk As Integer
    Dim l As Integer
    Dim ier As Integer
    Dim Nwe As Integer
    Dim GridRow As DataRow
    Dim RV() As Single
    Dim RN() As Single
    Dim DE As Single
    Dim DL As Single
    Dim DC As Single
    Dim DH As Single
    Dim Da As Single
    Dim Db As Single
    '
    Nwe = Winkel.Wsol.Nwe
    ReDim RV(Nwe - 1)
    ReDim RN(Nwe - 1)
    TabGridBase.Rows.Clear()
    '
    'Ausgewählte Sätze in Grid
    '
    For i = 0 To CnnRwerte.Count - 1
      RezSozpt = GroupRezepte(i)
      GrpRwerte = GroupRwerte(i)
      k = -1
      For kk = 0 To RezSozpt.Rezepte.RezCount - 1
        k = k + 1
        RzNr = RezSozpt.Rezepte.RezKey(k)
        If Not IsNumeric(RzNr) Then
          Continue For
        End If
        '
        '
        'Farbwerte berechnen
        '
        '
        For l = 0 To Nwe - 1
          RV(l) = GrpRwerte(0)("V").RefKurv(Winkel(Kw).Chrm).R(l)
          RN(l) = GrpRwerte(0)(RzNr).RefKurv(Winkel(Kw).Chrm).R(l)
        Next l
        '
        '
        Quali.FarbDifferenzRef(0, Kw, 0, Winkel, RV, RN, DE, DL, DC, DH, Da, Db, ier)
        '
        '
        GridRow = TabGridBase.NewRow
        GridRow("V") = i
        GridRow("N") = RzNr
        GridRow(2) = RezSozpt.Rezepte(RzNr).Name
        GridRow("DE") = DE
        GridRow("DL") = DL
        GridRow("DC") = DC
        GridRow("DH") = DH
        GridRow("Da") = Da
        GridRow("Db") = Db
        TabGridBase.Rows.Add(GridRow)

      Next kk
    Next i
    TabGridBase.AcceptChanges()
    flgGrid.Refresh()
    '
  End Sub

  Private Sub btnSpeichern_Click(sender As Object, e As System.EventArgs) Handles btnSpeichern.Click
    Dim canc As Boolean
    Call SpeiAll(canc)
  End Sub

  Private Sub btnGrid_Click(sender As Object, e As System.EventArgs) Handles btnGrid.Click
    Dim RezId As Integer
    Dim RzNr As String
    Dim igroup As Integer
    Dim i As Integer
    Dim j As Integer
    Dim ier As Integer
    Dim Nwe As Integer

    

    'If dbgREZ(index).Rows.Count = 0 Then Exit Sub
    'btnMARK(index).Enabled = False
    grdWRT.Visible = True
    '
    SplitContVisible = 3
    Cursor = Cursors.WaitCursor
    Nwe = Winkel.Wsol.Nwe
  

    '
    Call CreateGRPRwrt.MakeTABRefwerte(True, MenueParam.Messg.Kbez, Winkel, TabGridWerte)
    Call CreateGRPRwrt.MakeGRIDRefwerte(Winkel, grdWRT)
    '


    '
    'Ausgewählte Sätze in Grid
    '
    For i = 0 To TabGridBase.Rows.Count - 1
      igroup = TabGridBase.Rows(i)("V")
      RezSozpt = GroupRezepte(igroup)
      GrpRwerte = GroupRwerte(igroup)
      RzNr = TabGridBase.Rows(i)("N")
      If Not IsNumeric(RzNr) Then
        Continue For
      End If
      RezId = RezSozpt.Rezepte(RzNr).ID
      RefWerteUNT.Clear()
      RefWerteTYP.Clear()
      RefWerteSMP.Clear()
      For j = 0 To 1
        If GrpRwerte(j).RefUnt.IVoNa Then
          RefWerteUNT.Add(GrpRwerte(j).RefUnt)
        Else
          RefWerteUNT.Add(Nothing)
        End If
        If GrpRwerte(j)("V").IVoNa Then
          RefWerteTYP.Add(GrpRwerte(j)("V"))
        Else
          RefWerteTYP.Add(Nothing)
        End If
        If GrpRwerte(j)(RzNr).IVoNa Then
          RefWerteSMP.Add(GrpRwerte(j)(RzNr))
        Else
          RefWerteSMP.Add(Nothing)
        End If
      Next j
      Call HandleRezept.TabRezRecord(Winkel, RzNr, RezSozpt, RefWerteUNT, RefWerteTYP, RefWerteSMP, TabGridWerte, ier)
      '
      '
      '
      '


    Next i
    '
    grdWRT.Refresh()
    btnDelete.Enabled = True
    btnCLIP.Enabled = True
    Cursor = Cursors.Default
  End Sub

  Private Sub btnCLIP_Click(sender As Object, e As System.EventArgs) Handles btnCLIP.Click
    '
    'Schreiben nach Clipboard
    '
    Cursor = Cursors.WaitCursor
    Clipboard.Clear()
    '
    'Grid in Zwischenablage
    '
    '
    '
    '
    grdWRT.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
    grdWRT.SelectAll()

    If grdWRT.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
      Clipboard.SetDataObject(grdWRT.GetClipboardContent)
    End If
    grdWRT.ClearSelection()
    Cursor = Cursors.Default
  End Sub

  Private Sub btnDelete_Click(sender As Object, e As System.EventArgs) Handles btnDelete.Click
    Dim Vrow As DataGridViewRow
    If MessageBox.Show(Texxt(3032), Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.No Then
      Exit Sub
    End If
    For Each Vrow In flgGrid.SelectedRows
      TabGridBase.Rows(Vrow.Index).Delete()
    Next

    TabGridBase.AcceptChanges()
    btnGrid.PerformClick()
  End Sub
  Private Sub txtKDE_Leave(sender As Object, e As System.EventArgs) Handles txtKDE.Leave
    If IsNumeric(txtKDE.Text) Then
      MenueParam.Misch.Fde = CSng(txtKDE.Text)
    End If
  End Sub
  Private Sub ConFlgKopieren_Click(sender As Object, e As System.EventArgs) Handles ConFlgKopieren.Click
    If Not flgGrid.Visible Then
      Exit Sub
    End If
    Cursor = Cursors.WaitCursor
    Clipboard.Clear()
    '
    'Grid in Zwischenablage
    '
    '
    '
    '
    flgGrid.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
    flgGrid.SelectAll()

    If flgGrid.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
      Clipboard.SetDataObject(flgGrid.GetClipboardContent)
    End If
    flgGrid.ClearSelection()
    Cursor = Cursors.Default
  End Sub
  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtMNG_0.Validating, txtMNG_1.Validating, txtPRO_0.Validating, txtPRO_1.Validating, txtSum_0.Validating, txtSUM_1.Validating, _
    txtDickKor_0.Validating, txtDickKor_1.Validating, txtKDE.Validating, txtNANZ.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub

  Private Sub radWIN_CheckedChanged(sender As Object, e As System.EventArgs) Handles radWIN_0.CheckedChanged, radWIN_1.CheckedChanged, radWIN_2.CheckedChanged, _
    radWIN_3.CheckedChanged, radWIN_4.CheckedChanged, radWIN_5.CheckedChanged, radWIN_6.CheckedChanged, radWIN_7.CheckedChanged, radWIN_8.CheckedChanged
    Dim index As Integer
    Dim i As Integer
    If IsNothing(RezGraphics) Then Exit Sub
    For i = 0 To Winkel.Km - 1
      If RezCheckRad.radWIN(i).checked Then
        index = i
      End If
    Next
    Call FillGridBase(index)
  End Sub
End Class