Option Explicit On
Option Compare Text
Option Strict Off
Public Class frmColorRezepte

  Inherits Windows.Forms.Form
  Dim DsetRezpt As New DataSet
  Dim OleAdRezpt As New OleDbDataAdapter
  Dim OleAdSorti As New OleDbDataAdapter
  Dim DatReadRezpt As OleDbDataReader
  Dim CmdRezpt As New OleDbCommand
  Dim DbErr As Integer
  Dim Kdru As Integer
  Dim KdrAll As Integer
  Dim SortiID As Integer = -1
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
  Dim chkMessVor As New List(Of CheckBox)
  Dim btnMessVor As New List(Of Button)
  Dim btnMessUnt As New List(Of Button)
  Dim lblMessVor As New List(Of Label)
  Dim lblMessNch As New List(Of Label)
  Dim lblMessUnt As New List(Of Label)
  Dim lblDickKor As New List(Of Label)
  Dim txtDickKor As New List(Of TextBox)
  Dim lblDickNch As New List(Of Label)
  Dim txtDickNch As New List(Of TextBox)
  Dim radRestFarb As New List(Of RadioButton)
  Dim SplitContErstRezepte As List(Of SplitContainer)
  Dim cbomim As New List(Of ComboBox)
  Dim txtMng As New List(Of TextBox)
  Dim txtPro As New List(Of TextBox)
  Dim lblVOR As New List(Of Label)
  Dim lblmng As New List(Of Label)
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
  Dim Picauf As HandlePictures
  Dim RezDruck As HandlePlottDruck
  Dim GrpRwerte As RefValuesGrp
  Dim RwWrSortim As ReadWriteSortiment
  Dim RwWrRezept As ReadWriteRezept
  Dim ReWrRwert As ReadWriteRwert
  Dim CalcRezept As RezeptBerechnung
  Dim Umr As RezeptUmrechnung
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
  Dim TblRefGew As DataTable
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
  Dim imsg As Integer   'Messagebox (Rückgabewert)
  'Dim Igx As Integer
  Dim WiFawrt As Integer

  Private Sub frmColorRezepte_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    RemoveHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
    AufbauPar.MethID = -1
  End Sub


  Private Sub frmColorRezepte_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
    Dim i As Integer
    Dim j As Integer
    '
    '
    '
    '
    '
    '
    '
    Me.Text = Texxt(1853) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    '
    '
    OldTabIndex = -1
    OldMischID = -1
    OleAdRezpt.SelectCommand = New OleDbCommand("", Cncol)
    OleAdSorti.SelectCommand = New OleDbCommand("", Cndat)
    AddHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
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
    Me.lblGRP.Text = Texxt(3669)
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
    Me.chkAltName.Text = Texxt(902)
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
    chkRefGew.Text = Texxt(3952)
    '
    '
    '
    'lblGGE
    '
    '
    Me.lblGGE.Text = Texxt(172)
    Me.lblGGEMax.Text = Texxt(172) & "(" & Texxt(814) & ")"

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
    'Glanzgrad
    '
    '
    Me.lblGlzGrd.Text = Texxt(870)
    '
    '
    '
    'btnMessVor_0
    '

    Me.btnMessVor_0.Text = Texxt(806)
    '
    'btnMessVor_1
    '

    Me.btnMessVor_1.Text = Texxt(807)
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
    'TabBasisrezept/btnBasisrezept
    '
    '
    '
    '
    Me.btnBasisrezept.Text = Texxt(851)

    '
    'panPicRechts_1
    '
    '

    '

    '
    'lblDIS
    '

    Me.lblDIS.Text = Texxt(832)
    Me.lblDisMax.Text = Texxt(832) & "(" & Texxt(814) & ")"
    '
    'lblKDS
    '

    Me.lblKDS.Text = Texxt(831)
    '
    'btnSPRwrt
    '
    Me.btnSPRwrt.Text = Texxt(849)
    '
    '
    'btnRestFarb
    '
    Me.btnRestFarb.Text = Texxt(853)
    Me.lblRestGew.Text = Texxt(172)
    '
    '
    radRestFarb_0.Text = Texxt(3975)
    radRestFarb_1.Text = Texxt(3976)

    '

    '
    'TabErstrezept/btnErstrezept
    '

    Me.btnErstrezept.Text = Texxt(852)

    '
    'TabAllerezepte/btnAllerezepte
    '

    Me.btnAllerezepte.Text = Texxt(859)

    '

    '
    'btnSPE
    '

    Me.btnSPE.Text = Texxt(854)
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
    Me.lblSrMax.Text = Texxt(2427)

    '
    'lblNUM
    '

    Me.lblNUM.Text = Texxt(883)
    '
    '
    'lblCOL
    '
    Me.lblCOL.Text = Texxt(837)
    '

    '
    'btnKOP
    '

    Me.btnKOP.Text = Texxt(389)
    '
    '
    '
    'Mischsystem Enabled / Visible
    '
    '
    If BitWrt(29, MenueParam.User.Drum) Then
      chkAltName.Visible = True
    End If
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
    If BitWrt(22, MenueParam.User.Sonst) Then
      lblGlzGrd.Visible = True
      txtGlzGrd.Visible = True
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
   
    '
    'Werte für Glanzabzug
    '
    cboGLZ.Items.Clear()
    For i = 0 To GetPutReflex.GlanzWrt.Count - 1
      cboGLZ.Items.Add(GetPutReflex.GlanzWrt(i))
    Next
    cboGLZ.Text = MenueParam.User.Winkel(0).GK(0)
    '
    '
    SplitContErstRezepte = New List(Of SplitContainer)
    SplitContErstRezepte.Add(SplitContSortiment)
    SplitContErstRezepte.Add(SplitContBasisRezept)
    SplitContErstRezepte.Add(SplitContEinzelrezept)
    SplitContErstRezepte.Add(SplitContAlleRezepte)


    '
    '
    radRestFarb.Add(radRestFarb_0)
    radRestFarb.Add(radRestFarb_1)

    '
    '
    ChkUNT.Add(chkUNT_0)
    ChkUNT.Add(chkUNT_1)
    chkMessVor.Add(chkMessVor_0)
    chkMessVor.Add(chkMessVor_1)
    chkMessUnt.Add(chkMessUnt_0)
    chkMessUnt.Add(chkMessUnt_1)

    btnMessVor.Add(btnMessVor_0)
    btnMessVor.Add(btnMessVor_1)
    btnMessUnt.Add(btnMessUnt_0)
    btnMessUnt.Add(btnMessUnt_1)
    lblMessVor.Add(lblMessVor_0)
    lblMessVor.Add(lblMessVor_1)
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
    lblVOR.Add(lblVOR_0)
    lblVOR.Add(lblVOR_1)
    lblmng.Add(lblMNG_0)
    lblmng.Add(lblMNG_1)
    btnRezepte.Add(btnSortiment)
    btnRezepte.Add(btnBasisrezept)
    btnRezepte.Add(btnErstrezept)
    btnRezepte.Add(btnAllerezepte)
    '
    '
    '
    '
    DatenFarb = New HandleRezDsetFarb
    RezDruck = New HandlePlottDruck
    RezDruck.printset = MnPrintSet

    RezCheckRad = New HandleCheckRad
    RezGraphics = New HandleRezGrafik
    RezCheckRad.FillGrid = FillGrid
    RezCheckRad.PicGraphic = RezGraphics
    'RezCheckRad.cboWIN = cboWin
    RezCheckRad.cboSKAL = cboSkal
    RezGraphics.cboSKAL = cboSkal
    RezGraphics.TextKDS = txtKDS
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
    cboSRT.Items.Add(Texxt(949))
    '
    '

    '
    'Tabelle für Gewichte (R-Werte)
    '
    '
    '
    '
    TblRefGew = New DataTable
    '
    TblRefGew.Columns.Add("LAM", GetType(Single))
    TblRefGew.Columns.Add("GEW", GetType(Single))

    For i = 0 To MenueParam.Messg.RefGew.Nwe - 1
      TblRefGew.Rows.Add(TblRefGew.NewRow)
      TblRefGew.Rows(i)("LAM") = MenueParam.Messg.Winkel.Wsol.R(i)
      TblRefGew.Rows(i)("GEW") = MenueParam.Messg.RefGew.R(i)
    Next
    TblRefGew.AcceptChanges()
    '
    '
    dbgRefGew.DataSource = TblRefGew
    dbgRefGew.AllowUserToAddRows = False
    dbgRefGew.Columns("LAM").ReadOnly = True
    dbgRefGew.RowTemplate.Height = 16
    dbgRefGew.Columns(0).HeaderText = Texxt(3942)
    dbgRefGew.Columns(1).HeaderText = Texxt(602)
    dbgRefGew.Columns(0).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgRefGew.Columns(1).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgRefGew.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgRefGew.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgRefGew.Columns(0).Width = 70
    dbgRefGew.Columns(1).Width = 70
    '
    '
    'Gewichte übernehmen
    '
    For i = 0 To TblRefGew.Rows.Count - 1
      MenueParam.Messg.RefGew.R(i) = TblRefGew.Rows(i)("GEW") * MenueParam.Misch.Gge
    Next
    '

    'Tabelle für Sortimente
    '
    '
    '
    '
    '
    '
    DsetRezpt.Tables.Add("TblSorti")

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
    cboDRU.Add(cboDRU_3)
    btnDRU.Add(Nothing)
    btnDRU.Add(btnDru_1)
    btnDRU.Add(btnDRU_2)
    btnDRU.Add(btnDRU_3)


    RezSozpt = New RecipesGrp
    GrpRwerte = New RefValuesGrp
    RwWrSortim = New ReadWriteSortiment
    RwWrRezept = New ReadWriteRezept
    ReWrRwert = New ReadWriteRwert
    CalcRezept = New RezeptBerechnung
    Umr = New RezeptUmrechnung

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
    RezDruck.Winkel = MenueParam.User.Winkel
    '
    btnWEI.Text = Texxt(1999)
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
    If cboDruAll.Items.Count > 0 Then
      cboDruAll.SelectedIndex = 0
    End If
    ''
    '
    '
    '
    ''
    '
    '
    For i = 0 To 3
      Picauf = New HandlePictures
      PicAufBau.Add(Picauf)
      RezptGrid.Add(New HandleRezC1Screen)
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
    PicAufBau(2).Add("REF", CType(picRwert_2, PictureBox))
    PicAufBau(2).Add("LAB", CType(picFarbLAB_2, PictureBox))
    PicAufBau(2).Add("XYZ", CType(picFarbXYZ_2, PictureBox))
    PicAufBau(2).Add("FRB", CType(picRezFarb_2, PictureBox))
    PicAufBau(2).Add("REZ", CType(picRezRezept_2, PictureBox))
    '
    PicAufBau(3).Add("XYZ", CType(picFarbXYZ_3, PictureBox))
    PicAufBau(1).PicGraphic = RezGraphics
    PicAufBau(2).PicGraphic = RezGraphics
    PicAufBau(3).PicGraphic = RezGraphics

    RezptGrid(0).TDBFar = TDBFar_0
    RezptGrid(0).TDBDropFar = TDBDropFar_0
    RezptGrid(0).TDBDropPre = TDBDropPre_0
    RezptGrid(0).TDBDropPrb = TDBDropPrb_0
    RezptGrid(0).TDBDropPro = TDBDropPro_0
    RezptGrid(0).txtFooter = txtSUM_0
    RezptGrid(0).chkVOL = chkVOL
    RezptGrid(0).cboMNG = cboMng
    RezptGrid(0).cboPRO_0 = cboPro_0
    RezptGrid(0).cboPRO_1 = cboPro_1
    RezptGrid(0).lstFAR = lstFAR
    RezptGrid(0).txtMNG_0 = txtMNG_0
    RezptGrid(0).txtMNG_1 = txtMNG_1
    RezptGrid(0).txtPRO_0 = txtPRO_0
    RezptGrid(0).txtPRO_1 = txtPRO_1




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


    RezptGrid(2).TDBFar = TDBFar_2
    RezptGrid(2).TDBDropFar = TDBDropFar_2
    RezptGrid(2).TDBDropPre = TDBDropPre_2
    RezptGrid(2).TDBDropPrb = TDBDropPrb_2
    RezptGrid(2).TDBDropPro = TDBDropPro_2
    RezptGrid(2).txtFooter = txtSUM_2





    RezptGrid(0).Picauf = Nothing
    RezptGrid(1).Picauf = PicAufBau(1)
    RezptGrid(2).Picauf = PicAufBau(2)

    '
    '
    'Datenstrukturen
    '
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
    txtRestGew.Text = MenueParam.Menue.Del

    '

    '
    For i = 0 To 2
      RezptGrid(i).GrpRwerte = GrpRwerte
      RezptGrid(i).PicGraphic = RezGraphics
    Next i
    '
    '
    '
    cboMini.SelectedIndex = -1
    cboMini.Items.Clear()
    For i = 0 To 7
      cboMini.Items.Add(New ListTextID(i, Texxt(572 + i) & " (" & Texxt(306 + MenueParam.Menue.JABST) & ") "))
    Next i
    cboMini.ValueMember = "ID"
    cboMini.DisplayMember = "TEXT"
    cboMini.SelectedIndex = MenueParam.Misch.Igx
    '
    '

    cboMini.Visible = BitWrt(14, MenueParam.User.Visbl)
    cboMini.Enabled = BitWrt(14, MenueParam.User.Enabl)
    ''
    'Tabelle der Mischsysteme aufbauen
    'Tabelle Mischsysteme    '
    '
    '
    '
    If MenueParam.MischID <> -1 Then
      OleAdRezpt.SelectCommand.CommandText = HandleRezept.MischSelectCommand()
      OleAdRezpt.SelectCommand.Connection = Cncol()
      If Not FillDatset(OleAdRezpt, DsetRezpt, "TblMisch") Then
        Exit Sub
      End If
      cboMsh.DataSource = DsetRezpt.Tables("TblMisch")
      cboMsh.DisplayMember = "MISCH_KBEZ"
      cboMsh.ValueMember = "MISCH_ID"
      'AddHandler cboMsh.SelectedIndexChanged, AddressOf cboMSH_SelectedIndexChanged

      cboMsh.SelectedIndex = -1
      cboMsh.SelectedValue = MenueParam.MischID
    Else
      MsgBox(Texxt(3601))
      SplitContErstRezepte(2).Visible = False
    End If

    '
  End Sub


  Private Sub Change(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMessVor_0.TextChanged, lblMessVor_1.TextChanged, lblMessUnt_0.TextChanged, lblMessUnt_1.TextChanged, txtDickKor_0.TextChanged, txtDickKor_1.TextChanged, txtGlzGrd.TextChanged

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
    If sender.name = "txtGlzGrd" Then
      If IsNumeric(sender.text) Then
        RezSozpt.Rezepte(KeyMenge).GlzGrd = CSng(sender.text)
      End If
    End If
  End Sub

  'Private Sub frmColorRezepte_Shown(sender As Object, e As System.EventArgs) Handles Me.Shown
  '
  '
  '
  Sub TabelleMisch()


  End Sub
  Sub cboMSH_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMsh.SelectedIndexChanged
    Dim i As Short
    Dim j As Short
    Dim SqlStmt As String
    If sender.Selectedindex = -1 Then Exit Sub
    If Not IsNumeric(sender.Selectedvalue) Then Exit Sub
    If sender.SelectedValue = OldMischID Then Exit Sub
    OldMischID = sender.SelectedValue
    AufbauPar.MischID = -1

    AufbauPar.MischID = sender.SelectedValue
    chkKDE.Checked = MenueParam.Misch.Kgx
    txtKDE.Text = MenueParam.Misch.Fde
    SortiID = -1
    '
    '
    DatenFarb.DsetFarbFill(1, chkAltName.Checked)
    If DatenFarb.TblFarb.Rows.Count = 0 Then
      'MsgBox(Texxt(2922))
      Exit Sub
    End If

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
    'Grid für Basisrezept ("BAS")
    '
    '
    'RezptGrid(1).imgButton = imgButton

    RezptGrid(1).TDBFarGridRezept(ier)

    '
    '
    '
    '
    '
    'Grid für Einzelrezepte ("RZP")
    '
    RezptGrid(2).TDBFarGridRezept(ier)

    '

    RezptGrid(0).NewKeynam("SOR")
    RezptGrid(1).NewKeynam("BAS")
    RezptGrid(2).NewKeynam("RZP")
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
    '
    '
    cboGRPRwrt.DataSource = Nothing
    GetPutReflex.cboGRP = cboGRPRwrt
    GetPutReflex.lblGRP = Nothing
    Call GetPutReflex.GRoupList()
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
    ChkUNT(0).CheckState = CheckState.Indeterminate
    ChkUNT(1).CheckState = CheckState.Unchecked

    '

    '
    '
    '
    '
    '
    RezSozpt.Rezepte.clear()
    RezSozpt.Farben.clear()
    RezSozpt.Rezepte.AddRez("SOR", New Recipe)
    RezSozpt.Rezepte.AddRez("BAS", New Recipe)

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
    RezGraphics.Winkel = MenueParam.User.Winkel
    '
    '
    '
    '
    '
    RezCheckRad.Picauf = Nothing
    RezGraphics.Ksrt.Clear()
    RezSozpt.Rezepte.RemoveRez("RZP")
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

    For i = 0 To MenueParam.User.Winkel.Km - 1
      RezCheckRad.chkWIN(i).Visible = True
      RezCheckRad.chkWIN(i).Enabled = False
      RezCheckRad.radWIN(i).Visible = True
      RezCheckRad.radWIN(i).Enabled = False
      RezCheckRad.chkWIN(i).Text = LTrim(MenueParam.User.Winkel(i).Chrm)
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
    'Winkel/Messgeometrie für z.B. Lab-Berechnung
    '
    'cboWin.Items.Clear()
    'For i = 0 To MenueParam.User.Winkel.Km - 1
    ' cboWin.Items.Add(MenueParam.User.Winkel(i).Chrm)
    ' Next i
    'cboWin.Enabled = False
    'cboWin.Text = cboWin.Items(0)

    '
    '
    Cursor = Cursors.WaitCursor
    txtGGEMax.Text = -1.0
    txtGGE.Text = 0.0
    RezGraphics.DataChanged = False
    Cursor = Cursors.Arrow
    '
    '
    '
    '
    '
    'Sortimente einlesen
    '
    btnSortiment.PerformClick()

    '
    '
    '
    If MenueParam.Misch.Igx > 2 And MenueParam.Misch.Igx < 7 Then MenueParam.Misch.Igx = 0
    '
    cboMini.SelectedIndex = MenueParam.Misch.Igx

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
    DsetRezpt.Tables("TblSorti").Clear()
    If BitWrt(10, MenueParam.User.Writ) Then
      SqlStmt = "SELECT * FROM " & MenueParam.TableSorti & " WHERE USER_ID=" & MenueParam.UserID _
      & " AND MISCH_ID=" & MenueParam.MischID & " ORDER BY SORTI_NAME"
    Else
      SqlStmt = "SELECT * FROM " & MenueParam.TableSorti & " WHERE MISCH_ID=" & MenueParam.MischID & " ORDER BY SORTI_NAME"
    End If
    lstSOR.DataSource = Nothing
    OleAdSorti.SelectCommand.CommandText = SqlStmt
    OleAdSorti.SelectCommand.Connection = Cndat()
    If Not FillDatset(OleAdSorti, DsetRezpt, "TblSorti") Then
      Exit Sub
    End If

    lstSOR.DataSource = DsetRezpt.Tables("TblSorti")
    lstSOR.DisplayMember = "SORTI_NAME"
    lstSOR.ValueMember = "SORTI_ID"
    AddHandler lstSOR.SelectedIndexChanged, AddressOf lstSOR_SelectedIndexChanged
    For i = 0 To 1
      btnMessVor(i).Text = Texxt(806 + RezGraphics.Vkwb(i)) & Space(1)
      btnMessUnt(i).Text = Texxt(808 + RezGraphics.Vkwb(i)) & Space(1)
      RezCheckRad.chkUUU(i).Text = Texxt(804 + RezGraphics.Vkwb(i))
      lblDickKor(i).Text = Texxt(810 + RezGraphics.Vkwb(i))
      ToolTipRezepte.SetToolTip(RezCheckRad.chkUUU(i), Texxt(927 + RezGraphics.Vkwb(i)))
      j = BitInt(RezGraphics.Vkwb(i), RezGraphics.Vkwb(i), MenueParam.Messg.MeArtID)
      btnMessVor(i).Text = btnMessVor(i).Text & Space(1) & Texxt(3918 + j)
      btnMessUnt(i).Text = btnMessUnt(i).Text & Space(1) & Texxt(3918 + j)
    Next i
    '
    '
    '

    lstSOR.SelectedIndex = -1
    If lstSOR.Items.Count > 0 Then
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
    If lstSOR.SelectedValue = SortiID Then Exit Sub
    SortiID = lstSOR.SelectedValue
    Cursor = Cursors.WaitCursor
    SplitContErstRezepte(1).Enabled = False
    SplitContErstRezepte(2).Enabled = False
    Call HandleRezept.SorSpei(RezGraphics.DataChanged, lstSOR, KeyMenge, RezSozpt, OleAdSorti, DsetRezpt.Tables("tblsorti"), RwWrSortim, UntID, TypID, ier)
    HandleRezept.ClearRezepte(RezSozpt)
    RezGraphics.DataChanged = False
    For i = 0 To 1
      lblMessUnt(i).Text = ""
      lblMessVor(i).Text = ""
      lblVOR(i).Text = ""
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
    'RezSozpt.Rezepte(KeyMenge).RezMin = Max(RezSozpt.Rezepte(KeyMenge).RezMin, MenueParam.Misch.MinFarb)
    'RezSozpt.Rezepte(KeyMenge).RezMax = Max(RezSozpt.Rezepte(KeyMenge).RezMax, MenueParam.Misch.MaxFarb)
    '
    RezGrid.TDBFarRezStart(ier)
    If ier <> 0 Then
      Cursor = Cursors.Default
      Exit Sub
    End If
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
      lblMessVor(i).Text = GrpRwerte(i)("V").Name
      lblVOR(i).Text = GrpRwerte(i)("V").Name
    Next i

    RezGraphics.DataChanged = False
    RezGraphics.Ksrt.Clear()
    RezSozpt.Rezepte.RemoveRez("RZP")

    '
    '
    '
    If txtGGEMax.Text < 0.0# Then
      txtGGEMax.Text = MenueParam.Misch.Gge
    End If
    '
    Call VisGGERezept(MenueParam.Misch.Igx, KeyMenge)
    '
    '
    '
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
    If RezSozpt.Rezepte(KeyMenge).RezMin = 0 Then RezSozpt.Rezepte(KeyMenge).RezMin = 3
    If RezSozpt.Rezepte(KeyMenge).RezMax = 0 Then RezSozpt.Rezepte(KeyMenge).RezMax = 4
    cbomim(0).SelectedIndex = RezSozpt.Rezepte(KeyMenge).RezMin - 1
    cbomim(1).SelectedIndex = RezSozpt.Rezepte(KeyMenge).RezMax - 1
    txtMng(0).Text = HandleRezept.AutFormat(RezSozpt.MngMin)
    txtMng(1).Text = HandleRezept.AutFormat(RezSozpt.MngMax)
    txtPro(0).Text = HandleRezept.AutFormat(100 * RezSozpt.ProzMin)
    txtPro(1).Text = HandleRezept.AutFormat(100 * RezSozpt.ProzMax)
    txtDickKor(0).Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).Dicke(0))
    txtDickKor(1).Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).Dicke(1))
    txtGlzGrd.Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).GlzGrd)
    cboMng.SelectedIndex = RezSozpt.INO
    cboPro_0.SelectedIndex = RezSozpt.INP
    cboPro_1.SelectedIndex = RezSozpt.INQ
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
      chkMessVor(0).Checked = True
      '
      lblVOR(0).AutoSize = False
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
    chkMessVor_1.Checked = False
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
      chkMessVor_1.Checked = True
      '
      lblVOR(0).AutoSize = False
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

    For i = 0 To 1
      btnMessVor(i).Visible = chkMessVor(i).Checked
      lblMessVor(i).Visible = chkMessVor(i).Checked
      lblVOR(i).Visible = chkMessVor(i).Checked
      btnMessUnt(i).Visible = chkMessUnt(i).Checked
      lblMessUnt(i).Visible = chkMessUnt(i).Checked
      If GrpRwerte(i)("V").ID >= 0 Then
        GrpRwerte(i)("V").IVoNa = chkMessVor(i).Checked
      End If
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
      cboSrMax.Visible = False
      lblSrMax.Visible = False
    End If
    If Not BitWrt(27, MenueParam.User.Enabl) Then
      cboSrMax.Enabled = False
    End If
    If Not BitWrt(2, MenueParam.User.Sonst) Then
      panDICKE.Visible = False
      hscDIS.Visible = False
      lblKDS.Visible = False
      txtKDS.Visible = False
      lblDIS.Visible = False
      txtDIS.Visible = False
      lblDisMax.Visible = False
      txtDisMax.Visible = False
    End If
    If Not BitWrt(7, MenueParam.User.Writ) Then
      btnStoreSorti.Visible = False
    End If
    If Not BitWrt(10, MenueParam.User.Sonst) Then
      btnSPRwrt.Visible = False
      cboGRPRwrt.Visible = False
    End If
    If Not BitWrt(8, MenueParam.User.Sonst) Then
      SplitContErstRezepte(1).Visible = False
    End If

    If Not BitWrt(9, MenueParam.User.Sonst) Then
      btnRestFarb.Visible = False
      panRestFarbe.Visible = False
    End If
    If Not WinUSeqMe(MenueParam.User.Winkel, MenueParam.Messg.Winkel) Then
      'btnRestFarb.Visible = False
      'panRestFarbe.Visible = False
      'btnSPRwrt.Visible = False
      'cboGRPRwrt.Visible = False

    End If
    '
    '
    ''

  End Sub
  Sub VisGGERezept(ByRef Igx As Short, ByRef KeyMenge As String)
    Dim i As Integer
    Dim Bind As Boolean
    If IsNothing(KeyMenge) OrElse IsNothing(RezSozpt.Rezepte) Then Exit Sub
    If Igx > 2 And Igx < 7 Then
      Igx = 0
      Igx = 0
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
      panGGE.Visible = True
      txtGGE.Visible = True
      lblGGE.Visible = True
      txtGGEMax.Visible = True
      lblGGEMax.Visible = True
      hscGGE.Visible = True
      chkRefGew.Visible = True
    Else
      panGGE.Visible = False
      txtGGE.Visible = False
      lblGGE.Visible = False
      txtGGEMax.Visible = False
      lblGGEMax.Visible = False
      hscGGE.Visible = False
      chkRefGew.Visible = False
    End If
    If cboMini.SelectedIndex <> Igx Then
      cboMini.SelectedIndex = Igx
    End If
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
    Dim i As Integer
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
      RezSozpt.Rezepte(RezDruck.KeyNam).Name = StrName
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
    'Bamng für Gesamtmenge (wg. Batchkonzentrationen)
    '
    RezSozpt.INF = 0
    If index = 1 Then
      Call RezDruck.MngUmrech("BAS")
    Else
      Call RezDruck.MngUmrech("")
    End If
    '
    ReDim RezDruck.RefNr(1)
    RezDruck.RefNr(0) = "V"
    RezDruck.RefNr(1) = "R"
    If index = 2 Or index = 3 Then
      RezDruck.RefNr(1) = KeyRe(RezSozpt.Rezepte("RZP").Nr)
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
          ' AufbauPar.AufbauRezeptMerk(FarbWerte, ier)

          'Call Quali.FarbwerteCalc(RezDruck.RefNr, MenueParam.User.Winkel, GrpRwerte, FarbWerte, ier)

          AufbauPar.AufbauAnwsgMerk(MenueParam.UserID, MenueParam.MethID, FarbWerte, ier)

          Call Quali.FarbwerteCalc(RezDruck.RefNr, MenueParam.User.Winkel, GrpRwerte, FarbWerte, ier)

          If ier <> 0 Then
            Cursor = Cursors.Default
            Exit Sub
          End If

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
    Dim i As Integer
    Dim k As Integer
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
        'AufbauPar.AufbauRezeptMerk(FarbWrt, ier)

        'Call quali.FarbWrtCalc(MenueParam.User.Winkel, GrpRwerte, FarbWrt, ier)
        '
        Call SetStdQucontrol(GrpRwerte)
        AufbauPar.AufbauAnwsgMerk(MenueParam.UserID, MenueParam.MethID, FarbWrt, ier)

        Call quali.CalcFarbWerte(MenueParam.User.Winkel, GrpRwerte, FarbWrt, ier)

        DruAusgabe.AusgabeRZPALLE(RezSozpt, FarbWrt, GrpRwerte)
        DruAusgabe = Nothing
        quali = Nothing
        FarbWrt = Nothing
        Cursor = System.Windows.Forms.Cursors.Arrow
    End Select


    ''
  End Sub



  Private Sub chkUNT_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkUNT_0.CheckStateChanged, chkUNT_1.CheckStateChanged, chkKDE.CheckStateChanged
    If IsNothing(MenueParam) Then Exit Sub
    MenueParam.Misch.Kgx = chkKDE.Checked
    Call VisVisRezept()
  End Sub



  Private Sub frmColorRezepte_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Resize
    Call ResizeChild(Me)
  End Sub

  Private Sub btnDRU_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles btnDru_1.Click, btnDRU_2.Click, btnDRU_3.Click
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
  Private Sub txtglzgrd_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtGlzGrd.Leave
    RezSozpt.Rezepte(KeyMenge).GlzGrd = Singl(txtGlzGrd.Text)
    txtGlzGrd.Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).GlzGrd)
  End Sub
  Private Sub cboSRMAX_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cboSrMax.SelectedIndexChanged
    Dim SelInd As Integer
    SelInd = cboSRT.SelectedIndex
    cboSRT.SelectedIndex = -1
    cboSRT.SelectedIndex = SelInd
  End Sub

  Private Sub cboSRT_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cboSRT.SelectedIndexChanged
    Dim i As Integer
    Dim DG0 As Single
    Dim Imax As Short
    If cboSRT.SelectedIndex < 0 Then Exit Sub
    If Kzal = 0 Then Exit Sub
    If RezSozpt.Rezepte.RezCount <= 2 Then Exit Sub
    cboSRT.Enabled = False
    Ipgm = cboSRT.SelectedIndex
    MenueParam.Misch.Isor = Ipgm
    DG0 = MenueParam.Misch.DeGut
    Imax = Singl(cboSrMax.Text)
    CalcRezept.SorRez(Ipgm, Imax, Kzl, Ksor, DG0, RezSozpt, ier)
    Application.DoEvents()
    '
    '
    '
    'Umspeichern der Rezepte nach Rezpte
    '
    '
    '
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
    'Sortierung übernehmen
    RezGraphics.Ksrt.Clear()
    For i = 0 To Kzl - 1
      RezGraphics.Ksrt.Add(Ksor(i))
    Next
    '
    '
    '
    'Grid füllen
    '
    '
    '
    '
    FillGrid.FillUeGrid()
    cboSRT.Enabled = True
  End Sub



  Private Sub cboMIM_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMim_0.SelectedIndexChanged, cboMim_1.SelectedIndexChanged
    Dim index As Short
    Dim i As Integer
    index = CInt(sender.name.ToString.Substring(7, 1))
    If Not IsNothing(KeyMenge) Then
      If index = 0 Then
        RezSozpt.Rezepte(KeyMenge).RezMin = cbomim(index).SelectedIndex + 1
      Else
        RezSozpt.Rezepte(KeyMenge).RezMax = cbomim(index).SelectedIndex + 1
      End If
    End If
    cboSrMax.Items.Clear()
    If cbomim(1).SelectedIndex = -1 Or cbomim(0).SelectedIndex = -1 Then Exit Sub
    If Not IsNumeric(cbomim(1).Text) Or Not IsNumeric(cbomim(0).Text) Then Exit Sub
    cboSrMax.Items.Clear()
    For i = 1 To CShort(cbomim(1).Text) - CShort(cbomim(0).Text) + 1
      cboSrMax.Items.Add(CStr(CDbl(cbomim(0).Text) + i - 1))
    Next i
    cboSrMax.SelectedIndex = cboSrMax.Items.Count - 1
  End Sub
  Private Sub lblMessVor_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMessVor_0.DoubleClick, lblMessVor_1.DoubleClick
    Dim Index As Short
    Index = CInt(sender.name.ToString.Substring(11, 1))
    TypID(Index) = -1
    lblMessVor(Index).Text = ""
    GrpRwerte(Index)("V").Name = ""
    GrpRwerte(Index)("V").IVoNa = False
  End Sub

  Private Sub lblMessUnt_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMessUnt_0.DoubleClick, lblMessUnt_1.DoubleClick
    Dim Index As Short
    Index = CInt(sender.name.ToString.Substring(11, 1))
    UntID(Index) = -1
    lblMessUnt(Index).Text = ""
    GrpRwerte(Index).RefUnt.Name = ""
    GrpRwerte(Index).RefUnt.IVoNa = False
  End Sub



  Private Sub hscREZ_Scroll(ByVal sender As Object, ByVal e As System.Windows.Forms.ScrollEventArgs) Handles hscREZ.Scroll
    Dim i As Integer
    Dim j As Integer
    Dim RzNr As String
    Dim RwNr As String
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    If sender.value < 0 Then Exit Sub
    If sender.value >= Kzl Then Exit Sub
    If IsNothing(RezGrid) Then Exit Sub
    If IsNothing(RezSozpt) Then Exit Sub
    If IsNothing(RezGraphics) Then Exit Sub
    Cursor = System.Windows.Forms.Cursors.WaitCursor
    Isor = Ksor(sender.value)
    RzNr = KeyRe(Isor)
    '
    '
    If Not RezSozpt.Rezepte.ContainsKey(RzNr) Then Exit Sub
    '
    lblNRR.Text = CStr(Isor)
    '
    '
    'Umhängen Rezepte
    '
    '
    '
    If RezSozpt.Rezepte.ContainsKey("RZP") Then
      RezSozpt.Rezepte.RemoveRez("RZP")
    End If
    '
    RezSozpt.Rezepte.AddRez("RZP", RezSozpt.Rezepte(RzNr))
    RzNr = "RZP"
    RwNr = KeyRe(RezSozpt.Rezepte(RzNr).Nr)
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
      If GrpRwerte(i)(RwNr).IVoNa Then
        j = j + 1
        RezGraphics.PlotRwerte.Add(KeyRe(j), GrpRwerte(i)(RwNr))
      End If
    Next i
    RezGraphics.FawrtRwerte(0).clear()
    RezGraphics.FawrtRwerte(1).clear()
    RezGraphics.FawrtRwerte(0).Add(KeyRe(0), GrpRwerte(0)("V"))
    RezGraphics.FawrtRwerte(0).Add(KeyRe(1), GrpRwerte(0)(RwNr))
    RezGraphics.FawrtRwerte(1).Add(KeyRe(0), GrpRwerte(1)("V"))
    RezGraphics.FawrtRwerte(1).Add(KeyRe(1), GrpRwerte(1)(RwNr))
    RezGraphics.RzName(0) = "RZP"
    RezGraphics.Text = GrpRwerte(0)("V").Name
    RezGraphics.Rmax = -1
    RezGraphics.Rmin = -1
    RezGrid.TDBFarRezStart(ier)
    RezCheckRad.Picauf.Refresh()
    Application.DoEvents()
    Cursor = System.Windows.Forms.Cursors.Arrow
  End Sub

  Sub GetNameID(ByRef ID As Integer, ByRef RefName As String, ByRef Banum As String)
    Dim j As Short
    If RefName <> "" Then
      '
      If RcmdRef = 0 Then Exit Sub
      If GetPutReflex.HideSofort Then
        GetPutReflex.InVisible()
      End If
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
            lblMessVor(j).Text = GrpRwerte(j)(ReUn).Name
            lblVOR(j).Text = GrpRwerte(j)(ReUn).Name
            TypID(RezGraphics.Vkwb(j)) = ID
            GrpRwerte(j)(ReUn).IVoNa = True
          Case -1, -2
            lblMessUnt(-RcmdRef - 1).Text = GrpRwerte(j).RefUnt.Name
            UntID(RezGraphics.Vkwb(j)) = ID
            GrpRwerte(j).RefUnt.IVoNa = True
        End Select
      End If

      'RefName = ""
    End If

  End Sub
  Sub RezActiv(i)
    Dim j As Short
    For j = 0 To RezptGrid.Count - 1
      If Not IsNothing(RezptGrid(j)) Then
        RezptGrid(j).chkVOL = Nothing
      End If
    Next
    RezptGrid(i).chkVOL = chkVOL
  End Sub
  Private Sub btnMessVor_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnMessVor_0.Click, btnMessVor_1.Click
    Dim Index As Short
    Index = CInt(sender.name.substring(11, 1))
    '
    '
    RcmdRef = Index + 11
    'GetPutReflex.Kenn = MenueParam.Messg.Kenn
    GetPutReflex.Messrefel = GrpRwerte(Index)("V")
    GetPutReflex.Iarch = 0
    GetPutReflex.Captext = btnMessVor(Index).Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(Index), RezGraphics.Vkwb(Index), MenueParam.Messg.MeArtID)
    Me.Enabled = False
    GetPutReflex.ReflexWerte(False)
    Me.Enabled = True
    MenueParam.Messg.MeArtLock = BitInt(RezGraphics.Vkwb(Index), RezGraphics.Vkwb(Index), MenueParam.Messg.MeArtID)


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

  Private Sub hscDIS_Scroll(ByVal sender As Object, ByVal e As System.Windows.Forms.ScrollEventArgs) Handles hscDIS.Scroll
    Dim dick As Single
    Dim Optgesamt As New OpticalData
    If Not hscDIS.Visible Then Exit Sub
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    Cursor = Cursors.WaitCursor
    'Application.DoEvents()
    dick = RezSozpt.Rezepte("BAS").Dicke(0)
    RezSozpt.Rezepte("BAS").Dicke(0) = DiHsc(0) + (CSng(hscDIS.Value) - CSng(hscDIS.Minimum)) * (DiHsc(1) - DiHsc(0)) / (CSng((hscDIS.Maximum - hscDIS.LargeChange + 1)) - CSng(hscDIS.Minimum))
    RezSozpt.Rezepte("BAS").Dicke(1) = RezSozpt.Rezepte("BAS").Dicke(0) * RezSozpt.Rezepte("BAS").Dicke(1) / dick
    txtDIS.Text = Format(RezSozpt.Rezepte("BAS").Dicke(0), "##0.00")
    CalcRezept.MischRezept(0, MenueParam.User.Winkel, "BAS", RezSozpt, GrpRwerte, Optgesamt, ier)
    RezCheckRad.Picauf.Refresh()
    Cursor = Cursors.Arrow
    Optgesamt.dispose()
  End Sub

  Private Sub hscGGE_Scroll(ByVal sender As Object, ByVal e As System.Windows.Forms.ScrollEventArgs) Handles hscGGE.Scroll
    Dim i As Integer
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    If KeyMenge <> "BAS" Then Exit Sub
    If Not hscGGE.Visible Then Exit Sub
    dbgRefGew.EndEdit()
    Cursor = Cursors.WaitCursor
    'Application.DoEvents()
    MenueParam.Misch.Gge = hscGGE.Value * txtGGEMax.Text / (hscGGE.Maximum - hscGGE.LargeChange + 1)
    txtGGE.Text = Format(MenueParam.Misch.Gge, "###0.000")
    If MenueParam.Misch.Igx = 7 Then
      For i = 0 To TblRefGew.Rows.Count - 1
        MenueParam.Messg.RefGew.R(i) = TblRefGew.Rows(i)("GEW") * MenueParam.Misch.Gge
      Next
    End If
    For i = 0 To RezSozpt.Rezepte(KeyMenge).KF - 1
      RezSozpt.Rezepte(KeyMenge)(i).FaAmng = RezSozpt.Rezepte("SOR")(i).FaAmng
    Next
    CalcRezept.BasisRezept(RezGraphics.KFIT, MenueParam.User.Winkel, KeyMenge, "BAS", RezSozpt, GrpRwerte, ier)
    RezCheckRad.Picauf.Refresh()

    If ier > 0 Then
      Cursor = Cursors.Arrow
      Exit Sub
    End If
    RezGrid.TDBFarRezStart(ier)
    RezCheckRad.Picauf.Refresh()
    If ier > 0 Then
      Cursor = Cursors.Arrow
      Exit Sub
    End If
    Cursor = Cursors.Arrow
  End Sub





  Private Sub btnRestFarb_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRestFarb.Click
    Dim i As Integer
    Dim RwWrFarbe As ReadWriteFarbe
    Dim RestFarbe As Colorant
    Dim RwWrGrund As ReadWriteGrund
    Dim irest As Integer
    Dim iprn As Integer
    '
    '
    '
    'Prüfen, ob Winkeleinstellung USER = Winkeleinstellung Messgerät
    '
    '
    '
    'If Not WinUSeqMe(MenueParam.User.Winkel, MenueParam.Messg.Winkel) Then Exit Sub
    '
    '
    '
    '
    '
    '
    RestFarbe = New Colorant
    RwWrFarbe = New ReadWriteFarbe
    RwWrGrund = New ReadWriteGrund
    '
    '
    'Restfarben
    '
    '
    '



    ier = 0
    Cursor = System.Windows.Forms.Cursors.WaitCursor
    '
    'Kennung für Restfarbe
    '
    '
    '
    irest = 0
    If radRestFarb(1).Checked Then
      irest = 1
    End If
    iprn = RezGraphics.KFIT + 4 * irest
    If txtRestGew.Visible Then
      MenueParam.Menue.Delta = CSng(txtRestGew.Text)
    End If
    CalcRezept.Restfarbe(iprn, "BAS", "BAS", RezSozpt, GrpRwerte, RestFarbe, ier)
    Cursor = Cursors.Arrow
    If ier = 0 Then
      RestFarbe.Name = InputBox(Texxt(815), Texxt(853), "@" & GrpRwerte(0)("V").Name)
      If Trim(RestFarbe.Name) <> "" Then
        '
        '
        'Speicher Restfarbe
        '
        '
        '
        RestFarbe.GlzGrdID = -1
        RestFarbe.GlzGrd = 0.0
        RwWrFarbe.FarAdd(RestFarbe, ier)
        '
        'Speichern Grunddaten der Restfarbe
        '
        '
        '
        RwWrGrund.WriteGrund(RestFarbe.ID, RestFarbe.OptData, MenueParam.Messg.Winkel, ier)




        DatenFarb.AddNewFarb(RestFarbe)
      End If
    End If

    RestFarbe.dispose()
    RwWrFarbe.dispose()
    RwWrGrund.dispose()
    Cursor = System.Windows.Forms.Cursors.Arrow

  End Sub

  Private Sub btnSPRwrt_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSPRwrt.Click
    Call SpeiRwertCalc(ReWrRwert, GrpRwerte, cboGRPRwrt.SelectedValue, ier)
  End Sub

  Private Sub btnStoreSorti_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnStoreSorti.Click
    RezGraphics.DataChanged = True
    Call HandleRezept.SorSpei(RezGraphics.DataChanged, lstSOR, KeyMenge, RezSozpt, OleAdSorti, DsetRezpt.Tables("tblsorti"), RwWrSortim, UntID, TypID, ier)
    RezGraphics.DataChanged = False

  End Sub




  Private Sub cboGLZ_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboGLZ.Click
    MenueParam.User.Winkel(0).GK(0) = CSng(cboGLZ.Text) * MenueParam.User.Winkel(0).Iglz
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
  Private Sub btnRezepte_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSortiment.Click, btnBasisrezept.Click, btnErstrezept.Click, btnAllerezepte.Click
    Dim i As Integer
    Dim j As Integer
    Dim index As Short
    Dim ier As Integer


    ier = 0
    Call VisGGERezept(MenueParam.Misch.Igx, KeyMenge)
    cboMini.Enabled = False
    Select Case sender.name
      Case "btnSortiment"
        index = 0
        cboMini.Enabled = BitWrt(14, MenueParam.User.Enabl)
      Case "btnBasisrezept"
        index = 1
      Case "btnErstrezept"
        index = 2
      Case "btnAllerezepte"
        index = 3
    End Select
    If index = 0 Or Not SplitContErstRezepte(index).Visible Then
      SplitContVisible = index
      Select Case index
        Case 0
          '
          '
          For i = RezSozpt.Rezepte.RezCount - 1 To 0 Step -1
            If IsNumeric(RezSozpt.Rezepte.RezKey(i)) Then
              RezSozpt.Rezepte.RemoveRez(i)
            End If
          Next
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
          For i = 0 To MenueParam.Messg.Winkel.Km - 1
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
          '
          '
          KeyMenge = "SOR"

          Call RezActiv(0)
          RezGrid = RezptGrid(0)
          RezGrid.TDBFarRezStart(ier)
          RezCheckRad.cboSKAL.Enabled = False

          '
          '
        Case 1, 2
          '
          '
          Umr.CalcFamng(KeyMenge, RezSozpt, ier)
          '
          '
          'Prüfen, ob Reflexionswerte vorhanden
          '
          '
          For i = 0 To 1
            If chkMessVor(i).Checked Then
              If Not GrpRwerte(i)("V").IVoNa Then
                MsgBox(Texxt(2960 + i))
                GoTo RezepteFeh
              End If
            End If
          Next i
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
            RezSozpt.Rezepte("SOR").Dicke(0) = CSng(txtDickKor_0.Text)
            RezSozpt.Rezepte("SOR").Dicke(1) = CSng(txtDickKor_1.Text)
            RezSozpt.Rezepte("SOR").RezMin = cboMim_0.SelectedIndex + 1
            RezSozpt.Rezepte("SOR").RezMax = cboMim_1.SelectedIndex + 1
            RezSozpt.Rezepte("SOR").GlzGrd = CSng(txtGlzGrd.Text)
            RezSozpt.Rezepte("SOR").RezMin = cbomim(0).Text
            RezSozpt.Rezepte("SOR").RezMax = cbomim(1).Text
            Call HandleRezept.SorSpei(RezGraphics.DataChanged, lstSOR, KeyMenge, RezSozpt, OleAdSorti, DsetRezpt.Tables("tblsorti"), RwWrSortim, UntID, TypID, ier)
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
          For i = 0 To MenueParam.User.Winkel.Km - 1
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
          'Grunddaten umrechnen gemäß Glanzgrad (GlzGrd)
          '
          If BitWrt(22, MenueParam.User.Sonst) Then
            Cursor = Cursors.WaitCursor
            For i = 0 To RezSozpt.Farben.FarbCount - 1
              Call HandleRezept.GrundGlzGrd(MenueParam.MischID, RezSozpt.Farben(i).ID, RezSozpt.Farben(i), RezSozpt.Rezepte(KeyMenge).GlzGrd, ier)
            Next i
            Cursor = Cursors.Default
          End If
        Case 3
          cboMsh.Enabled = False


      End Select
    End If


    Select Case index
      Case 0
        '
        '
        '
        '
        '
        'Sortiment
        '
        '
        '
        RezSozpt.Rezepte("BAS").clear()
        btnAllerezepte.Enabled = False
        '
      Case 1
        '

        '						'
        '
        'Basisrezept
        '
        '
        '

        If KeyMenge = "SOR" Then
          'sstVis(sstRezept, False)
          SplitContainErstrezepte.Visible = False
        Else
          'sstRezept.MousePointer = 11
          Cursor = Cursors.WaitCursor
          Application.DoEvents()
          txtDIS.Text = Format(RezSozpt.Rezepte(KeyMenge).Dicke(RezGraphics.Vkwb(0)), "##0.00")
        End If
        btnSortiment.Enabled = False
        Me.Enabled = False
        Application.DoEvents()
        '
        '
        'Kontrastfarbabstand
        ' 
        '
        '
        '
        '

        If chkKDE.Checked AndAlso (GrpRwerte(0).RefUnt.IVoNa And GrpRwerte(1).RefUnt.IVoNa) Then
          DiHsc(0) = 0.001 * RezSozpt.Rezepte("SOR").Dicke(0)
          DiHsc(1) = 2.0# * RezSozpt.Rezepte("SOR").Dicke(0)
          hscDIS.Maximum = (30000 + hscDIS.LargeChange - 1)
          hscDIS.Minimum = 0
          hscDIS.Value = (hscDIS.Maximum - hscDIS.LargeChange + 1)
          hscDIS.Enabled = True
          'hscDIS.Value = 0.5 * ((hscDIS.Maximum - hscDIS.LargeChange + 1) - hscDIS.Minimum)
          hscDIS.Value = hscDIS.Minimum + (RezSozpt.Rezepte("SOR").Dicke(RezGraphics.Vkwb(0)) - DiHsc(0)) / (DiHsc(1) - DiHsc(0)) * (hscDIS.Maximum - hscDIS.Minimum)
          panDICKE.Visible = True
          lblDIS.Visible = True
          txtDIS.Visible = True
          lblDisMax.Visible = True
          txtDisMax.Visible = True
          hscDIS.Visible = True
          txtDIS.Text = Format(RezSozpt.Rezepte(KeyMenge).Dicke(RezGraphics.Vkwb(0)), "##0.00")
          txtDisMax.Text = DiHsc(1)
          If BitWrt(0, MenueParam.Messg.MeArtID) = BitWrt(1, MenueParam.Messg.MeArtID) Then
            lblKDS.Visible = True
            txtKDS.Visible = True
          End If
        Else
          panDICKE.Visible = False
          lblDIS.Visible = False
          txtDIS.Visible = False
          hscDIS.Visible = False
          lblKDS.Visible = False
          txtKDS.Visible = False
          hscDIS.Enabled = False
          lblDisMax.Visible = False
          txtDisMax.Visible = False
        End If
        '
        '
        '
        btnAllerezepte.Enabled = False
        btnSortiment.Enabled = False
        Me.Enabled = False
        'Application.DoEvents()


        '
        '
        '
        '
        btnErstrezept.Enabled = False
        btnBasisrezept.Enabled = False
        Picauf = PicAufBau(1)
        If KeyMenge = "SOR" Then
          RezptGrid(1).TDBFar.Visible = False
          For i = 0 To Picauf.Count - 1
            Picauf(i).Visible = False
          Next i
          RezSozpt.Rezepte("BAS").clear()
        End If
        lblARB.SetBounds(Me.Location.X, Me.Location.Y, _
        Me.Size.Width, Me.Size.Height)
        lblARB.Visible = True
        '

        '
        '****************************
        'Berechnung des Basisrezeptes
        '****************************
        '
        '
        '
        '
        'Basisrezept berechnen
        '
        '
        '
        Iprn = 1
        SplitContainErstrezepte.Visible = False
        Application.DoEvents()
        If MenueParam.Misch.Igx > 0 Then
          MenueParam.Misch.Gge = CSng(txtGGE.Text)
        End If
        '
        '
        'Anfangswerte berechnen
        '
        '
        '
        '
        Call CalcRezept.DelRezRef({"SOR", "BAS"}, RezSozpt, {"V", "R"}, GrpRwerte)

        CalcRezept.BasisRezept(RezGraphics.KFIT + 8, MenueParam.User.Winkel, KeyMenge, "BAS", RezSozpt, GrpRwerte, ier)
        If ier <> 0 Then
          GoTo RezepteFeh
        End If

        SplitContainErstrezepte.Visible = True
        Application.DoEvents()
        '
        btnErstrezept.Enabled = True
        btnBasisrezept.Enabled = True
        btnSortiment.Enabled = True
        Me.Enabled = True


        For i = 0 To Picauf.Count - 1
          Picauf(i).Visible = True
        Next i
        If chkRefGew.Checked And MenueParam.Misch.Igx = 7 Then
          picFarbXYZ_1.Visible = False
          dbgRefGew.Visible = True
        Else
          picFarbXYZ_1.Visible = True
          dbgRefGew.Visible = False
        End If
        Picauf.PicRezRezept.Visible = Not chkFAR.Checked
        Picauf.PicRezFarb.Visible = chkFAR.Checked
        lblARB.Visible = False
        RezptGrid(1).TDBFar.Visible = True

        '
        Cursor = Cursors.Arrow
        '
        If ier <> 0 Then
          MessageBox.Show(Texxt(ier))
          Exit Sub
        End If
        '
        '
        '
        KeyMenge = "BAS"
        Call RezActiv(1)
        RezGrid = RezptGrid(1)
        RezGrid.TDBFarRezStart(ier)
        RezCheckRad.Picauf = Picauf
        RezGraphics.PlotRwerte.clear()
        j = -1
        For i = 0 To 1
          If GrpRwerte(i)("V").Iplott Then
            j = j + 1
            RezGraphics.PlotRwerte.Add(KeyRe(j), GrpRwerte(i)("V"))
          End If
        Next i
        For i = 0 To 1
          If GrpRwerte(i)("R").Iplott Then
            j = j + 1
            RezGraphics.PlotRwerte.Add(KeyRe(j), GrpRwerte(i)("R"))
          End If
        Next i
        RezGraphics.FawrtRwerte(0).clear()
        RezGraphics.FawrtRwerte(1).clear()
        RezGraphics.FawrtRwerte(0).Add(KeyRe(0), GrpRwerte(0)("V"))
        RezGraphics.FawrtRwerte(0).Add(KeyRe(1), GrpRwerte(0)("R"))
        RezGraphics.FawrtRwerte(1).Add(KeyRe(0), GrpRwerte(1)("V"))
        RezGraphics.FawrtRwerte(1).Add(KeyRe(1), GrpRwerte(1)("R"))
        RezGraphics.RzName(0) = KeyMenge
        RezGraphics.Text = GrpRwerte(0)("V").Name
        RezGraphics.Rmax = -1.0#
        RezGraphics.Rmin = -1.0#
        RezGrid.Picauf.Refresh()
        '
        '
        '
        '
        RezDruck.KeyNam = KeyMenge
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

      Case 2
        '
        '
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
        FillGrid = Nothing
        btnBasisrezept.Enabled = False
        btnErstrezept.Enabled = False
        btnAllerezepte.Enabled = False
        btnSortiment.Enabled = False
        Me.Enabled = False
        Picauf = PicAufBau(2)
        RezptGrid(2).TDBFar.Visible = False
        For i = 0 To Picauf.Count - 1
          Picauf(i).Visible = False
        Next i
        lblARB.SetBounds(Me.Location.X, Me.Location.Y, _
        Me.Size.Width, Me.Size.Height)
        lblARB.Visible = True

        'Application.DoEvents()
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


        KeyMenge = "BAS"
        KeySor = "SOR"
        If RezSozpt.Rezepte(KeyMenge).KF > 0 Then
          KeySor = "BAS"
        End If
        SplitContainErstrezepte.Visible = False
        Application.DoEvents()
        lblARB.Cursor = Cursors.WaitCursor
        MenueParam.Misch.MinFarb = RezSozpt.Rezepte("SOR").RezMin
        MenueParam.Misch.MaxFarb = RezSozpt.Rezepte("SOR").RezMax
        CalcRezept.ErstRezept(RezGraphics.KFIT + 8, MenueParam.User.Winkel, KeySor, KeyMenge, RezSozpt, GrpRwerte, ier)
        SplitContainErstrezepte.Visible = True
        lblARB.Cursor = Cursors.Default
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
        btnBasisrezept.Enabled = True
        btnErstrezept.Enabled = True
        btnSortiment.Enabled = True
        Me.Enabled = True



        RezptGrid(2).TDBFar.Visible = True
        For i = 0 To Picauf.Count - 1
          Picauf(i).Visible = True
        Next i
        Picauf.PicRezRezept.Visible = Not chkFAR.Checked
        Picauf.PicRezFarb.Visible = chkFAR.Checked
        lblARB.Visible = False
        'Application.DoEvents()

        '
        '
        '
        Cursor = System.Windows.Forms.Cursors.Arrow
        Cursor = Cursors.Arrow


        ' 
        '
        '
        '
        '
        '
        '

        FillGrid = New HandleFillGrid
        FillGrid.flgREZ = flgREZ
        FillGrid.chkFawrt = chkFawrt

        FillGrid.RezGraphic = RezGraphics
        FillGrid.TableAufbau()
        FillGrid.GridAufbau()
        Call RezActiv(2)
        RezGrid = RezptGrid(2)
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
        '
        'Sortieren
        '
        If RezSozpt.Rezepte.ContainsKey("RZP") Then
          RezSozpt.Rezepte.RemoveRez("RZP")
        End If
        Kzal = RezSozpt.Rezepte.RezCount - 2
        If Kzal = 0 Then
          MsgBox(Texxt(2953), MsgBoxStyle.OkOnly, Texxt(2000))
          GoTo rezeptefeh
        Else
          For i = 0 To Kzal - 1
            RezSozpt.Rezepte(KeyRe(i)).Name = GrpRwerte(0)("V").Name
          Next i
          cboSRT.SelectedIndex = -1
          cboSRT.SelectedIndex = MenueParam.Misch.Isor
          If Kzl = 0 Then
            MsgBox(Texxt(2953), MsgBoxStyle.OkOnly, Texxt(2000))
            GoTo rezeptefeh
          End If
        End If
        Application.DoEvents()
        btnAllerezepte.Enabled = True

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
        '
        '
        '
      Case 3
        '
        '
        '
        If flgREZ.Rows.Count = 0 Then
          MsgBox(Texxt(2953), MsgBoxStyle.OkOnly, Texxt(2000))
          GoTo rezeptefeh
        End If
        Picauf = PicAufBau(3)
        RezCheckRad.Picauf = Picauf
        hscREZ.Value = flgREZ.CurrentRow.Index
        FillGrid.GridAufbau()
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
    KeyMenge = "SOR"
    SplitContainErstrezepte.Visible = True
    Application.DoEvents()
    '
    btnErstrezept.Enabled = True
    btnBasisrezept.Enabled = True
    btnSortiment.Enabled = True
    Me.Enabled = True

    Cursor = Cursors.Default
    SplitContVisible = 0
  End Sub





  Private Sub cboDru_DropDownClosed(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboDru_1.DropDownClosed, cboDRU_2.DropDownClosed, cboDRU_3.DropDownClosed
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
    If DsetRezpt.Tables("TblSorti").Rows.Count = 0 Then Exit Sub
    If BitWrt(6, MenueParam.User.Writ) Then
      If DsetRezpt.Tables("TblSorti").Rows(lstSOR.SelectedIndex)("USER_ID") <> MenueParam.UserID Then
        imsg = MessageBox.Show(Texxt(3608) & Space(2) & CStr(DsetRezpt.Tables("TblSorti").Rows(lstSOR.SelectedIndex)("USER_ID")), Texxt(2000), MessageBoxButtons.OK)
        Exit Sub
      End If
      imsg = MessageBox.Show(Texxt(3025) & DsetRezpt.Tables("TblSorti").Rows(lstSOR.SelectedIndex)("SORTI_NAME") & Space(1) & Texxt(3026), Texxt(2000), MessageBoxButtons.YesNo)
      If imsg = Windows.Forms.DialogResult.No Then
        Exit Sub
      End If
      RwWrSortim.DelSortim(DsetRezpt.Tables("TblSorti").Rows(lstSOR.SelectedIndex)("SORTI_ID"), ier)
      If Not FillDatset(OleAdSorti, DsetRezpt, "TblSorti") Then
        Exit Sub
      End If
      RezGraphics.DataChanged = False
    End If
  End Sub








  Private Sub btnSPE_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSPE.Click
    Dim Iarch As Integer
    Dim RzNr As String
    Dim DatChange As Boolean
    If chkARCH.Checked Then
      Iarch = 1
    Else
      Iarch = 0
    End If
    RzNr = "RZP"
    DatChange = RezGraphics.DataChanged
    RezGraphics.DataChanged = True
    Call HandleRezept.KorSpei(RzNr, RezSozpt.Rezepte(RzNr).ID, MenueParam.UserID, Iarch, cboGRP.SelectedValue, RwWrRezept, RezGraphics, RezSozpt, UntID, TypID, SmpID, ier)
    RezGraphics.DataChanged = DatChange

  End Sub

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
    Picauf = PicAufBau(2)
    '
    RezCheckRad.Picauf = Picauf
    RezCheckRad.Picauf.Refresh()
    SplitContVisible = 2
  End Sub





  Private Sub txtDisMax_TextChanged(sender As Object, e As System.EventArgs) Handles txtDisMax.TextChanged
    If IsNumeric(txtDisMax.Text) Then
      DiHsc(1) = CSng(txtDisMax.Text)
    End If
  End Sub


  Private Sub chkFAR_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkFAR.CheckedChanged
    If chkFAR.Checked Then
      picRezRezept_1.Visible = False
      picRezRezept_2.Visible = False
      picRezFarb_1.Visible = True
      picRezFarb_2.Visible = True
    Else
      picRezRezept_1.Visible = True
      picRezRezept_2.Visible = True
      picRezFarb_1.Visible = False
      picRezFarb_2.Visible = False
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
      chkRefGew.Checked = False
      panGGE.Enabled = False
      chkRefGew.Enabled = False
    Else
      panGGE.Enabled = True
      chkRefGew.Enabled = True
      If MenueParam.Misch.Igx = 7 Then
        RezGraphics.KFIT = 3
        chkRefGew.Visible = True
      Else
        RezGraphics.KFIT = 1
        chkRefGew.Visible = False
        chkRefGew.Checked = False
      End If
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
    Picauf = PicAufBau(2)
    '
    RezCheckRad.Picauf = Picauf
    SplitContVisible = 2
    hscREZ.Value = flgREZ.CurrentRow.Index
    Call hscREZ_Scroll(hscREZ, New ScrollEventArgs(ScrollEventType.EndScroll, flgREZ.CurrentRow.Index))
  End Sub

  Private Sub chkRefGew_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkRefGew.CheckedChanged
    If chkRefGew.Checked And MenueParam.Misch.Igx = 7 Then
      picFarbXYZ_1.Visible = False
      dbgRefGew.Visible = True
    Else
      picFarbXYZ_1.Visible = True
      dbgRefGew.Visible = False
    End If
  End Sub

  Private Sub dbgRefGew_CellValidating(sender As Object, e As System.Windows.Forms.DataGridViewCellValidatingEventArgs) Handles dbgRefGew.CellValidating
    If e.ColumnIndex = 1 And Not IsNumeric(e.FormattedValue) Then
      e.Cancel = True
      MsgBox(Texxt(16))
    End If
  End Sub

  Private Sub txt_Enter(sender As Object, e As System.EventArgs) Handles txtGGE.Enter, txtGGEMax.Enter, txtKDE.Enter, txtDIS.Enter, txtKDS.Enter, txtDisMax.Enter, txtDickKor_0.Enter, txtDickKor_1.Enter, txtRestGew.Enter, txtGlzGrd.Enter
    sender.tag = sender.text
  End Sub

  Private Sub txt_TextChanged(sender As Object, e As System.EventArgs) Handles txtGGE.TextChanged, txtGGEMax.TextChanged, txtKDE.TextChanged, txtDIS.TextChanged, txtKDS.TextChanged, txtDisMax.TextChanged, txtDickKor_0.TextChanged, txtDickKor_1.TextChanged, txtRestGew.TextChanged, txtGlzGrd.TextChanged
    If Not IsNumeric(sender.text) Then
      sender.text = sender.tag
    End If
  End Sub
  Private Sub txtKDE_Leave(sender As Object, e As System.EventArgs) Handles txtKDE.Leave
    If IsNumeric(txtKDE.Text) Then
      MenueParam.Misch.Fde = CSng(txtKDE.Text)
    End If
  End Sub
  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtMNG_0.Validating, txtMNG_1.Validating, txtPRO_0.Validating, txtPRO_1.Validating, _
    txtDickKor_0.Validating, txtDickKor_1.Validating, txtSUM_0.Validated, txtSUM_1.Validating, txtSUM_2.Validating, txtGlzGrd.Validating, _
    txtDIS.Validating, txtDisMax.Validating, txtGGE.Validating, txtGGEMax.Validating, txtKDE.Validating, txtKDS.Validating, txtRestGew.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub

  Private Sub cboMini_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboMini.SelectedIndexChanged
    MenueParam.Misch.Igx = cboMini.SelectedIndex

    Call VisGGERezept(MenueParam.Misch.Igx, KeyMenge)
  End Sub

  Private Sub chkAltName_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkAltName.CheckedChanged
    Dim i As Integer
    If IsNothing(DatenFarb) Then Exit Sub
    'MsgBox(TDBFar_0.Columns(0).CellValue(0))
    'MsgBox(TDBFar_0.Columns(0).CellText(0))
    'MsgBox(TDBFar_0.Columns(0).Text)
    'MsgBox(TDBFar_0.Columns(0).Value)
    DatenFarb.DsetFarbFill(1, chkAltName.Checked)
    'RezGrid.TDBFar.Columns(0).DropDown.Rebind(True)
    'MsgBox(RezGrid.TDBFar.Columns(0).CellValue(0))

    '
    '
    For i = 0 To RezptGrid.Count - 1
      If Not IsNothing(RezptGrid(i)) AndAlso Not IsNothing(RezptGrid(i).TDBFar) Then
        RezptGrid(i).TDBFarGridRezept(ier)
        RezptGrid(i).NewKeynam(RezptGrid(i).Name)
        If RezptGrid(i).AllRezepte.Rezepte.ContainsKey(RezptGrid(i).Name) Then
          RezptGrid(i).TDBFarRezStart(ier)
        End If
      End If
    Next
    'MsgBox(TDBFar_0.Columns(0).CellText(0))
    'RezGrid.TDBFar.DataSource.datasource.rows(0)("FID") = 228

    'RezGrid.TDBFar.Columns(0).CellValue(0)

    'MsgBox(RezGrid.TDBFar.Columns(0).DropDown.DataSource.rows(0)("FARBM_NAME"))
    'TDBDropFar_0.Columns(0).Refresh()
    'TDBFar_0.Splits(0).DisplayColumns(0).Locked = False
    'TDBFar_0.Splits(0).DisplayColumns(0).AutoDropDown = True
    'RezGrid.TDBFar.Columns(0).RefreshCell()
    'RezGrid.TDBFar.Columns(0).RefetchCell()
    'RezGrid.TDBFar.Columns(0).Refetch()
    'RezGrid.TDBFar.Columns(0).Refresh()
    'RezGrid.TDBFar.Columns(0).DropDown.Update()
    'RezGrid.TDBFar.Columns(0).DropDown.Rebind(True)
    'RezGrid.TDBFar.Columns(0).DropDown.Update()
    'RezGrid.TDBFar.Columns(0).DropDown.Refresh()
    'RezGrid.TDBFar.Columns(0).DropDown.DisplayMember = "FARBM_NAME"
    'RezGrid.TDBFar.Columns(0).FilterDropdown = True
    'RezGrid.TDBFar.Columns(0).DropDown.ValueTranslate = False
    'RezGrid.TDBFar.Columns(0).DropDown.ValueTranslate = True
    'RezGrid.TDBFar.Columns(0).DropDown.DisplayColumns("FARBM_ID").Locked = False
    'RezGrid.TDBFar.Columns(0).DropDown.Rebind(True)
    'RezGrid.TDBFar.Columns(0).Refetch()
  End Sub

  Private Sub frmColorRezepte_ResizeEnd(sender As Object, e As System.EventArgs) Handles Me.ResizeEnd

  End Sub
End Class