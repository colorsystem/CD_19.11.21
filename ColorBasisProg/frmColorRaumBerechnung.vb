Option Explicit On
Option Compare Text
Option Strict Off
Public Class frmColorRaumBerechnung
  Inherits Windows.Forms.Form
  Dim DsetRezpt As New DataSet
  Dim OleAdRezpt As New OleDbDataAdapter
  Dim OleAdSorti As New OleDbDataAdapter
  Dim DatReadRezpt As OleDbDataReader
  Dim CmdRezpt As New OleDbCommand
  Dim DbErr As Integer
  Dim Kdru As Integer
  Dim KdrAll As Integer
  Dim Sortindex As Integer
  Dim IanzHell As Integer
  Dim IanzWink As Integer
  Dim Iprn As Integer
  Dim IanzSort As Integer
  Dim ISort() As Integer
  Dim IHell() As Integer
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
  Dim RezGrid As HandleRezC1Screen
  Dim RezptGrid As List(Of HandleRezC1Screen)
  Dim ChkUNT As New List(Of CheckBox)
  Dim chkMessUnt As New List(Of CheckBox)
  Dim btnMessUnt As New List(Of Button)
  Dim lblMessUnt As New List(Of Windows.Forms.Label)
  Dim lblDickKor As New List(Of Windows.Forms.Label)
  Dim txtDickKor As New List(Of TextBox)
  Dim SplitContRaumBerechnung As List(Of SplitContainer)
  Dim cbomim As New List(Of ComboBox)
  Dim txtMng As New List(Of TextBox)
  Dim txtPro As New List(Of TextBox)
  Dim lblmng As New List(Of Windows.Forms.Label)
  Dim lblSOO As List(Of Windows.Forms.Label)
  Dim lblUEB As List(Of Windows.Forms.Label)
  Dim lblHTO As List(Of Windows.Forms.Label)
  Dim txtTON As List(Of TextBox)
  Dim txtHEL As List(Of TextBox)
  Dim radSOO As List(Of RadioButton)
  'Dim radWIN As List(Of RadioButton)
  'Dim radNLA As List(Of RadioButton)
  Dim chkSOR As List(Of CheckBox)
  Dim radSOR As List(Of RadioButton)
  Dim chkLST As List(Of CheckBox)
  Dim TabREZ As List(Of DataTable)
  Dim TDBFar As List(Of C1TrueDBGrid)

  Dim TDBDropFar As List(Of C1TrueDBDropdown)
  Dim TDBDropPre As List(Of C1TrueDBDropdown)
  Dim TDBDropPro As List(Of C1TrueDBDropdown)
  Dim TDBDropPrb As List(Of C1TrueDBDropdown)
  Dim txtSUM As List(Of TextBox)
  Dim lstFAR As List(Of ListBox)

  Dim UntID As List(Of Integer())
  Dim TypID As List(Of Integer())

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
  Dim RezSozptAll As List(Of RecipesGrp)
  Dim Picauf As HandlePictures
  Dim GrpRwerteAll As List(Of RefValuesGrp)
  Dim RwWrSortim As ReadWriteSortiment
  Dim RwWrRezept As ReadWriteRezept
  Dim ReWrRwert As ReadWriteRwert
  Dim CalcRezept As RezeptBerechnung
  Dim RezGraphics As HandleRezGrafik
  Dim RezCheckRad As HandleCheckRad
  Dim DatenFarb As List(Of HandleRezDsetFarb)
  Dim FillGrid As HandleFillGrid
  Dim HandleRezept As HandleRezGeneral  '
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
  Dim Igx As Integer
  Dim WiFawrt As Integer
  Private Sub frmColorRaumBerechnung_Shown(sender As Object, e As System.EventArgs) Handles Me.Shown
    If MenueParam.MischID <> -1 Then
      OleAdRezpt.SelectCommand.CommandText = HandleRezept.MischSelectCommand()
      OleAdRezpt.SelectCommand.Connection = Cncol()
      If Not FillDatset(OleAdRezpt, DsetRezpt, "TblMisch") Then
        Exit Sub
      End If

      cboMsh.DataSource = DsetRezpt.Tables("TblMisch")
      cboMsh.DisplayMember = "MISCH_KBEZ"
      cboMsh.ValueMember = "MISCH_ID"
      cboMsh.SelectedIndex = -1
      cboMsh.SelectedValue = MenueParam.MischID
    Else
      MsgBox(Texxt(3601))
      SplitContRaumBerechnung(2).Visible = False
    End If
    Application.DoEvents()
  End Sub
   


  Private Sub frmColorRaumBerechnung_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    RemoveHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
    AufbauPar.MethID = -1
  End Sub


  Private Sub frmColorRaumBerechnung_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
    Dim i As Integer
    Dim k As Integer
    Dim kw As Integer
    '
    '
    '
    '
    '
    '
    '
   
    KeyMenge = "SOR"
    Me.Text = Texxt(1870) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
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
    '
    'lblARB
    '
    Me.lblARB.Text = Texxt(2002)
    '
    'chkFAR

    Me.chkFAR.Text = Texxt(857)

    '


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
    UntID = New List(Of Integer())
    UntID.Add({-1, -1})
    UntID.Add({-1, -1})
    UntID.Add({-1, -1})
    UntID.Add({-1, -1})
    '
    '
    TypID = New List(Of Integer())
    TypID.Add({-1, -1})
    TypID.Add({-1, -1})
    TypID.Add({-1, -1})
    TypID.Add({-1, -1})

    '
    btnRaumRechnung.Text = Texxt(1610)
    btnKopieren.Text = Texxt(389)
    btnBild.Text = Texxt(1611)
    btnDrucken.Text = Texxt(397)
    btnGRID.Text = Texxt(1612)
    '
    '
    lblUEB_0.Text = Texxt(1603)
    lblUEB_1.Text = Texxt(1604)
    lblUEB_2.Text = Texxt(1605)
    '
    '
    radSOO_0.Text = Texxt(1606)
    radSOO_1.Text = Texxt(1607)
    radSOO_2.Text = Texxt(1608)
    radSOO_3.Text = Texxt(1609)
    '
    '
    radSOR_0.Text = Texxt(1606)
    radSOR_1.Text = Texxt(1607)
    radSOR_2.Text = Texxt(1608)
    radSOR_3.Text = Texxt(1609)

    '
    '
    chkSOR_0.Text = Texxt(1606)
    chkSOR_1.Text = Texxt(1607)
    chkSOR_2.Text = Texxt(1608)
    chkSOR_3.Text = Texxt(1609)
    '
    '
    lblHTO_0.Text = Texxt(1601)
    lblHTO_1.Text = Texxt(1602)
    lblLST.Text = TexKt(10129)
    '
    '
    '
    'chkFawrt;chkRwertAngleich
    '
    chkRwertAngleich.Text = Texxt(4708)
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
  
    'chkKDE
    '

    Me.chkKDE.Text = Texxt(831)
    'TabBasisrezept/btnBasisrezept
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
      'chkARCH.Visible = True
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
    cboGLZ.Text = MenueParam.User.Winkel(0).GK(0)
    '
    '
    SplitContRaumBerechnung = New List(Of SplitContainer)
    SplitContRaumBerechnung.Add(SplitContSortiment)
    SplitContRaumBerechnung.Add(SplitContChart)
    SplitContRaumBerechnung.Add(SplitContTabelle)
    SplitContRaumBerechnung.Add(SplitContDrawing3D)

    '
    '
    '
    radSOR = New List(Of RadioButton)
    radSOR.Add(radSOR_0)
    radSOR.Add(radSOR_1)
    radSOR.Add(radSOR_2)
    radSOR.Add(radSOR_3)

    '
    '
    chkSOR = New List(Of CheckBox)
    chkSOR.Add(chkSOR_0)
    chkSOR.Add(chkSOR_1)
    chkSOR.Add(chkSOR_2)
    chkSOR.Add(chkSOR_3)


    '
    radSOO = New List(Of RadioButton)
    radSOO.Add(radSOO_0)
    radSOO.Add(radSOO_1)
    radSOO.Add(radSOO_2)
    radSOO.Add(radSOO_3)

    '
    '
    lblSOO = New List(Of Windows.Forms.Label)
    lblSOO.Add(lblSOO_0)
    lblSOO.Add(lblSOO_1)
    lblSOO.Add(lblSOO_2)
    lblSOO.Add(lblSOO_3)
    '
    '
    lblUEB = New List(Of Windows.Forms.Label)
    lblUEB.Add(lblUEB_0)
    lblUEB.Add(lblUEB_1)
    lblUEB.Add(lblUEB_2)
    '
    '
    lblHTO = New List(Of Windows.Forms.Label)
    lblHTO.Add(lblHTO_0)
    lblHTO.Add(lblHTO_1)
    '
    '
    txtHEL = New List(Of TextBox)
    txtHEL.Add(txtHEL_0)
    txtHEL.Add(txtHEL_1)
    txtHEL.Add(txtHEL_2)
    '
    '
    txtTON = New List(Of TextBox)
    txtTON.Add(txtTON_0)
    txtTON.Add(txtTON_1)
    txtTON.Add(txtTON_2)
    '
    '
    '
    '
    RezCheckRad = New HandleCheckRad
    RezGraphics = New HandleRezGrafik
    RezCheckRad.AddradWIN = radWIN_0
    RezCheckRad.AddradWIN = radWIN_1
    RezCheckRad.AddradWIN = radWIN_2
    RezCheckRad.AddradWIN = radWIN_3
    RezCheckRad.AddradWIN = radWIN_4
    RezCheckRad.AddradWIN = radWIN_5
    RezCheckRad.AddradWIN = radWIN_6
    RezCheckRad.AddradWIN = radWIN_7
    RezCheckRad.AddradWIN = radWIN_8

   
    For kw = 0 To RezCheckRad.radWIN.Count - 1
      RezCheckRad.radWIN(kw).Visible = False
    Next
    For kw = 0 To MenueParam.User.Winkel.Km - 1
      RezCheckRad.radWIN(kw).Text = MenueParam.User.Winkel(kw).Chrm
    Next

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
    chkLST = New List(Of CheckBox)
    chkLST.Add(chkLST_00)
    chkLST.Add(chkLST_01)
    chkLST.Add(chkLST_02)
    chkLST.Add(chkLST_03)
    chkLST.Add(chkLST_04)
    chkLST.Add(chkLST_05)
    chkLST.Add(chkLST_06)
    chkLST.Add(chkLST_07)
    chkLST.Add(chkLST_08)
    chkLST.Add(chkLST_09)
    chkLST.Add(chkLST_10)
    chkLST.Add(chkLST_11)
    chkLST.Add(chkLST_12)
    chkLST.Add(chkLST_13)
    chkLST.Add(chkLST_14)
    chkLST.Add(chkLST_15)
    chkLST.Add(chkLST_16)
    chkLST.Add(chkLST_17)
    chkLST.Add(chkLST_18)
    chkLST.Add(chkLST_19)
    chkLST.Add(chkLST_20)
    chkLST.Add(chkLST_21)
    chkLST.Add(chkLST_22)
    chkLST.Add(chkLST_23)
    chkLST.Add(chkLST_24)
    '  
    '
    TabREZ = New List(Of DataTable)
    TabREZ.Add(New DataTable)
    TabREZ.Add(New DataTable)
    TabREZ.Add(New DataTable)
    TabREZ.Add(New DataTable)
    '


    '
    '
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
    btnRezepte.Add(btnBild)
    btnRezepte.Add(btnKopieren)
    btnRezepte.Add(btnDrucken)
    '
    '
    '
    '
    
    RezCheckRad.FillGrid = FillGrid
    RezCheckRad.PicGraphic = RezGraphics
    'RezCheckRad.cboWIN = cboWin
    RezCheckRad.cboSKAL = cboSkal
    RezGraphics.cboSKAL = cboSkal
    RezCheckRad.cboSKAL.Enabled = False

    
    '
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
    RezSozptAll = New List(Of RecipesGrp)
    RezSozptAll.Add(New RecipesGrp)
    RezSozptAll.Add(New RecipesGrp)
    RezSozptAll.Add(New RecipesGrp)
    RezSozptAll.Add(New RecipesGrp)


    GrpRwerteAll = New List(Of RefValuesGrp)
    GrpRwerteAll.Add(New RefValuesGrp)
    GrpRwerteAll.Add(New RefValuesGrp)
    GrpRwerteAll.Add(New RefValuesGrp)
    GrpRwerteAll.Add(New RefValuesGrp)
    '
    '
    RwWrSortim = New ReadWriteSortiment
    RwWrRezept = New ReadWriteRezept
    ReWrRwert = New ReadWriteRwert
    CalcRezept = New RezeptBerechnung
    'OptGesamt = New BCSwinParamStruct.GrundDaten

    '
    '
    '
    TDBFar = New List(Of C1TrueDBGrid)
    TDBFar.Add(TDBFar_0)
    TDBFar.Add(TDBFar_1)
    TDBFar.Add(TDBFar_2)
    TDBFar.Add(TDBFar_3)
    '
    '
    TDBDropFar = New List(Of C1TrueDBDropdown)
    TDBDropFar.Add(TDBDropFar_0)
    TDBDropFar.Add(TDBDropFar_1)
    TDBDropFar.Add(TDBDropFar_2)
    TDBDropFar.Add(TDBDropFar_3)
    '
    '
    '
    TDBDropPre = New List(Of C1TrueDBDropdown)
    TDBDropPre.Add(TDBDropPre_0)
    TDBDropPre.Add(TDBDropPre_1)
    TDBDropPre.Add(TDBDropPre_2)
    TDBDropPre.Add(TDBDropPre_3)
    '
    '
    TDBDropPro = New List(Of C1TrueDBDropdown)
    TDBDropPro.Add(TDBDropPro_0)
    TDBDropPro.Add(TDBDropPro_1)
    TDBDropPro.Add(TDBDropPro_2)
    TDBDropPro.Add(TDBDropPro_3)
    '
    '
    '
    TDBDropPrb = New List(Of C1TrueDBDropdown)
    TDBDropPrb.Add(TDBDropPrb_0)
    TDBDropPrb.Add(TDBDropPrb_1)
    TDBDropPrb.Add(TDBDropPrb_2)
    TDBDropPrb.Add(TDBDropPrb_3)
    '
    '
    '
    '
    txtSUM = New List(Of TextBox)
    txtSUM.Add(txtSum_0)
    txtSUM.Add(txtSUM_1)
    txtSUM.Add(txtSum_2)
    txtSUM.Add(txtSum_3)
    '
    '
    '
    lstFAR = New List(Of ListBox)
    lstFAR.Add(lstFAR_0)
    lstFAR.Add(lstFAR_1)
    lstFAR.Add(lstFAR_2)
    lstFAR.Add(lstFAR_3)
    '
    '
    DatenFarb = New List(Of HandleRezDsetFarb)
    DatenFarb.Add(New HandleRezDsetFarb)
    DatenFarb.Add(New HandleRezDsetFarb)
    DatenFarb.Add(New HandleRezDsetFarb)
    DatenFarb.Add(New HandleRezDsetFarb)


    '
    '
    RezptGrid = New List(Of HandleRezC1Screen)
    RezptGrid.Add(New HandleRezC1Screen)
    RezptGrid.Add(New HandleRezC1Screen)
    RezptGrid.Add(New HandleRezC1Screen)
    RezptGrid.Add(New HandleRezC1Screen)

    For k = 0 To RezptGrid.Count - 1
      DatenFarb(k).DsetFarbCreate()
      RezptGrid(k).TDBFar = TDBFar(k)
      RezptGrid(k).TDBDropFar = TDBDropFar(k)
      RezptGrid(k).TDBDropPre = TDBDropPre(k)
      RezptGrid(k).TDBDropPrb = TDBDropPrb(k)
      RezptGrid(k).TDBDropPro = TDBDropPro(k)
      RezptGrid(k).txtFooter = txtSUM(k)
      RezptGrid(k).chkVOL = chkVOL
      RezptGrid(k).cboMNG = cboMng
      RezptGrid(k).cboPRO_0 = cboPro_0
      RezptGrid(k).cboPRO_1 = cboPro_1
      RezptGrid(k).lstFAR = lstFAR(k)
      RezptGrid(k).txtMNG_0 = txtMNG_0
      RezptGrid(k).txtMNG_1 = txtMNG_1
      RezptGrid(k).txtPRO_0 = txtPRO_0
      RezptGrid(k).txtPRO_1 = txtPRO_1
      RezptGrid(k).PicGraphic = RezGraphics
      RezptGrid(k).DatenFarb = DatenFarb(k)
      RezptGrid(k).CalcRezept = CalcRezept
      RezptGrid(k).AllRezepte = RezSozptAll(k)
      RezptGrid(k).GrpRwerte = GrpRwerteAll(k)
    Next k
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

    '
    ''
  End Sub


  Private Sub Change(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMessUnt_0.TextChanged, lblMessUnt_1.TextChanged, txtDickKor_0.TextChanged, txtDickKor_1.TextChanged
    If IsNothing(RezGraphics) Then Exit Sub
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

    RezGraphics.DataChanged = True
    '
    '
    '
    '

    If sender.name = "txtDICKKor_0" Then
      If IsNumeric(sender.text) Then
        RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(0) = CSng(sender.text)
      End If
    End If
    If sender.name = "txtDICKKor_1" Then
      If IsNumeric(sender.text) Then
        RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(1) = CSng(sender.text)
      End If
    End If
  End Sub

  Sub cboMSH_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMsh.SelectedIndexChanged
    Dim i As Short
    Dim k As Integer
    Dim kw As Integer
    Dim SqlStmt As String

    If sender.Selectedindex = -1 Then Exit Sub
    If Not IsNumeric(sender.Selectedvalue) Then Exit Sub
    If sender.SelectedValue = OldMischID Then Exit Sub
    OldMischID = sender.SelectedValue
    AufbauPar.MischID = sender.SelectedValue
    chkKDE.Checked = MenueParam.Misch.Kgx
    txtKDE.Text = MenueParam.Misch.Fde
    '
    '
    '
    '
    For k = 0 To RezptGrid.Count - 1
      RezptGrid(k).TDBFarGridRezept(ier)
    Next k


    For k = 0 To RezptGrid.Count - 1
      DatenFarb(k).DsetFarbFill(0, False)
      RezptGrid(k).NewKeynam("SOR")
    Next k
    '
    '
    '
    For i = 0 To radSOO.Count - 1
      radSOO(i).Checked = False
    Next
    '
    '
    '
    '

    '
    '
    HandleRezept.GroupList()
    '


    '
    '
    '
    '
    '
    '
    For k = 0 To GrpRwerteAll.Count - 1
      GrpRwerteAll(k).clear()
      '
      If MenueParam.Misch.Vert = 0 Then
        GrpRwerteAll(k).Add("W", New RefValues)
        GrpRwerteAll(k).Add("S", New RefValues)
      Else
        GrpRwerteAll(k).Add("S", New RefValues)
        GrpRwerteAll(k).Add("W", New RefValues)
      End If
      GrpRwerteAll(k)(0).RefUnt = New RefValue
      GrpRwerteAll(k)(1).RefUnt = New RefValue

      If GrpRwerteAll(k).RwArt(0) = "W" Then
        GrpRwerteAll(k)(0).RefUnt.kwb = 0
        GrpRwerteAll(k)(1).RefUnt.kwb = 1
        RezGraphics.Vkwb(0) = 0
        RezGraphics.Vkwb(1) = 1
      Else
        GrpRwerteAll(k)(0).RefUnt.kwb = 1
        GrpRwerteAll(k)(1).RefUnt.kwb = 0

      End If
    Next k
    '
    If GrpRwerteAll(0).RwArt(0) = "W" Then
      RezGraphics.Vkwb(0) = 0
      RezGraphics.Vkwb(1) = 1
    Else
      RezGraphics.Vkwb(0) = 1
      RezGraphics.Vkwb(1) = 0
    End If
    '
    '
    For k = 0 To RezSozptAll.Count - 1
      RezSozptAll(k).Rezepte.clear()
      RezSozptAll(k).Farben.clear()
      If GrpRwerteAll(k).RwArt(0) = "W" Then
        RezSozptAll(k).Rezepte.kwb(0) = 0
        RezSozptAll(k).Rezepte.kwb(1) = 1
      Else
        RezSozptAll(k).Rezepte.kwb(0) = 1
        RezSozptAll(k).Rezepte.kwb(1) = 0
      End If

      RezSozptAll(k).Rezepte.AddRez(KeyMenge, New Recipe)
      RezSozptAll(k).Rezepte(KeyMenge).clear()

      lblSOO(k).Text = ""
    Next k
    lblMessUnt(0).Text = ""
    lblMessUnt(1).Text = ""

    '
    '
    '
    '
    '
    '
    '
    '
    '
    For i = 0 To RezSozptAll.Count - 1
      RezSozptAll(i).Rezepte.clear()
      RezSozptAll(i).Farben.clear()
      RezSozptAll(i).Rezepte.AddRez("SOR", New Recipe)
    Next

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
    For i = 0 To RezCheckRad.chkWIN.Count - 1
      RezCheckRad.chkWIN(i).Visible = False
      RezCheckRad.chkWIN(i).checkstate = CheckState.Unchecked
    Next i
    For kw = 0 To RezCheckRad.radWIN.Count - 1
      RezCheckRad.radWIN(kw).Visible = False
      RezCheckRad.radWIN(kw).Checked = False
    Next kw
    For kw = 0 To MenueParam.User.Winkel.Km - 1
      RezCheckRad.radWIN(kw).Visible = True
    Next kw
    RezCheckRad.radWIN(0).Checked = True
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


    'RezCheckRad.chkWIN(0).checkstate = CheckState.Checked
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
    Cursor = Cursors.WaitCursor
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
    '
    '
    '
    '

    '
    'Messungen mit weißem Untergrund oder deckend
    '
    'ChkUNT(0).CheckState = CheckState.Checked
    '
    '
    'Messungen mit schwarzem Untergrund
    '
    '
    If MenueParam.Misch.Transp Then
      'ChkUNT(1).CheckState = MenueParam.Misch.Schwrz
    End If
    '
    '
    '
    radSOO(0).Checked = True
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
    If Not FillDatset(OleAdSorti, DsetRezpt, "TblSorti") Then
      Exit Sub
    End If

    lstSOR.DataSource = DsetRezpt.Tables("TblSorti")
    lstSOR.DisplayMember = "SORTI_NAME"
    lstSOR.ValueMember = "SORTI_ID"
    AddHandler lstSOR.SelectedIndexChanged, AddressOf lstSOR_SelectedIndexChanged
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

    If DsetRezpt.Tables("TblSorti").Rows.Count > 0 Then
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
      RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(0) = MenueParam.Misch.Dicke
      RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(1) = MenueParam.Misch.Dicke
      For i = 0 To 1
        TypID(Sortindex)(i) = -1
        UntID(Sortindex)(i) = -1
      Next i
      Exit Sub
    End If
    If KeyMenge <> "SOR" Then Exit Sub
    If IsNothing(lstSOR.SelectedValue) Then Exit Sub
    Cursor = Cursors.WaitCursor
    RezGrid = RezptGrid(Sortindex)
    RezGrid.chkVOL.CheckState = CheckState.Indeterminate
    SplitContRaumBerechnung(1).Enabled = False
    SplitContRaumBerechnung(2).Enabled = False
    HandleRezept.ClearRezepte(RezSozptAll(Sortindex))
    RezGraphics.DataChanged = False
    For i = 0 To 1
      lblMessUnt(i).Text = ""
      GrpRwerteAll(Sortindex)(i).RefUnt.Name = ""
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
    RwWrSortim.ReadSortimFarbGrund(KeyMenge, lstSOR.SelectedValue, RezSozptAll(Sortindex), UntID(Sortindex), TypID(Sortindex), ier)
    '

    GrpRwerteAll(Sortindex)(0).RefUnt.ID = UntID(Sortindex)(RezGraphics.Vkwb(0))
    GrpRwerteAll(Sortindex)(1).RefUnt.ID = UntID(Sortindex)(RezGraphics.Vkwb(1))
    GrpRwerteAll(Sortindex)(0).RefUnt.Nr = -1
    GrpRwerteAll(Sortindex)(1).RefUnt.Nr = -1

    '
    '
    '
    'Einlesen R-Werte
    '
    '
    '

    For i = 0 To GrpRwerteAll(Sortindex).Count - 1
      GrpRwerteAll(Sortindex)(i).RefUnt.Name = ""
      GrpRwerteAll(Sortindex)(i).RefUnt.Bem = ""
      GrpRwerteAll(Sortindex)(i).RefUnt.IVoNa = False
      GrpRwerteAll(Sortindex)(i).RefUnt.Iplott = False
      If GrpRwerteAll(Sortindex)(i).RefUnt.ID >= 0 And i = 0 Then
        ReWrRwert.ReadRwert(GrpRwerteAll(Sortindex)(i).RefUnt.ID, GrpRwerteAll(Sortindex)(i).RefUnt, ier)
        GrpRwerteAll(Sortindex)(i).RefUnt.Nr = i
        chkMessUnt(i).Checked = True
      End If
      lblMessUnt(i).Text = GrpRwerteAll(Sortindex)(i).RefUnt.Name

    Next i

    Call VisRau()


    RezGraphics.DataChanged = False
    '
    '
    '
    '
    '
    '
    '

    RezGrid.TDBFarRezStart(ier)
    lblSOO(Sortindex).Text = RezSozptAll(Sortindex).Rezepte(KeyMenge).Name

    'Application.DoEvents()
    '
    '
    'Grid füllen
    '
    '
    '


    If RezSozptAll(Sortindex).MngMin < 0.0# Then RezSozptAll(Sortindex).MngMin = 0
    If RezSozptAll(Sortindex).MngMax < RezSozptAll(Sortindex).MngMin Then RezSozptAll(Sortindex).MngMax = RezSozptAll(Sortindex).MngMin
    If RezSozptAll(Sortindex).ProzMin < 0 Then RezSozptAll(Sortindex).ProzMin = 0
    If RezSozptAll(Sortindex).ProzMax < RezSozptAll(Sortindex).ProzMin Then RezSozptAll(Sortindex).ProzMax = RezSozptAll(Sortindex).ProzMin
    If RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(0) < 0 Then RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(0) = 0
    If RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(1) < 0 Then RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(1) = 0
    If RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(0) <> RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(1) Then
      RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(1) = RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(0)
    End If
    txtMng(0).Text = HandleRezept.AutFormat(RezSozptAll(Sortindex).MngMin)
    txtMng(1).Text = HandleRezept.AutFormat(RezSozptAll(Sortindex).MngMax)
    txtPro(0).Text = HandleRezept.AutFormat(100 * RezSozptAll(Sortindex).ProzMin)
    txtPro(1).Text = HandleRezept.AutFormat(100 * RezSozptAll(Sortindex).ProzMax)
    txtDickKor(0).Text = HandleRezept.AutFormat(RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(0))
    txtDickKor(1).Text = HandleRezept.AutFormat(RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(1))
    If RezSozptAll(Sortindex).Rezepte(0).Iarch < 0 Or RezSozptAll(Sortindex).Rezepte(0).Iarch > 2 Then
      chkARCH.CheckState = CheckState.Unchecked
    Else
      chkARCH.CheckState = RezSozptAll(Sortindex).Rezepte(KeyMenge).Iarch
    End If
    '
    RezGrid.AllRezepte = RezSozptAll(Sortindex)
    RezGrid.chkVOL.CheckState = RezSozptAll(Sortindex).IVOL

    SplitContRaumBerechnung(1).Enabled = True
    SplitContRaumBerechnung(2).Enabled = True

    Cursor = Cursors.Arrow
    Application.DoEvents()
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

    For i = 0 To 1

      btnMessUnt(i).Visible = chkMessUnt(i).Checked
      lblMessUnt(i).Visible = chkMessUnt(i).Checked
      
      If GrpRwerteAll(Sortindex)(i).RefUnt.ID >= 0 Then
        GrpRwerteAll(Sortindex)(i).RefUnt.IVoNa = chkMessUnt(i).Checked
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
     
    
    
    If Not BitWrt(7, MenueParam.User.Writ) Then
      btnStoreSorti.Visible = False
    End If
     
    If Not BitWrt(8, MenueParam.User.Sonst) Then
      SplitContRaumBerechnung(1).Visible = False
    End If
 
    
    '
    ''

  End Sub
  Sub VisRau()
    Dim i As Integer
    If Not Menueparam.Misch.Transp Then
      chkKDE.Checked = False
      chkKDE.Visible = False
      lblMessUnt(0).Visible = False
      lblMessUnt(1).Visible = False
      btnMessUnt(0).Visible = False
      btnMessUnt(1).Visible = False

      lblDickKor(1).Visible = False
      txtDickKor(1).Visible = False
    Else
      chkKDE.Checked = MenueParam.Misch.Kgx

      chkKDE.Visible = True
      lblMessUnt(0).Visible = True
      lblMessUnt(1).Visible = True
      btnMessUnt(0).Visible = True
      btnMessUnt(1).Visible = True
      lblDickKor(0).Visible = True
      txtDickKor(0).Visible = True
      lblDickKor(1).Visible = True
      txtDickKor(1).Visible = True

    End If
    btnMessUnt(1).Visible = False
    lblMessUnt(1).Visible = False
    lblDickKor(1).Visible = False
    txtDickKor(1).Visible = False
    txtKDE.Visible = False
    If chkKDE.Checked Then
      btnMessUnt(1).Visible = True
      lblMessUnt(1).Visible = True
      lblDickKor(1).Visible = True
      txtDickKor(1).Visible = True
      txtKDE.Visible = True
      txtKDE.Text = CStr(MenueParam.Misch.Fde)
    End If
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
    If Not BitWrt(18, MenueParam.User.Visbl) Then
      chkVOL.Visible = False
    End If
    If Not BitWrt(18, MenueParam.User.Enabl) Then
      chkVOL.Enabled = False
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
    If Not BitWrt(5, MenueParam.User.Visbl) Then
      chkKDE.Visible = False
      txtKDE.Visible = False
    End If
    If Not BitWrt(5, MenueParam.User.Enabl) Then
      chkKDE.Enabled = False
      txtKDE.Enabled = False
    End If
    If Not BitWrt(6, MenueParam.User.Visbl) Then
      cboGLZ.Visible = False
      lblGLZ.Visible = False
    End If
    If Not BitWrt(6, MenueParam.User.Enabl) Then
      cboGLZ.Enabled = False
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
 
  '
  '




  Private Sub chkUNT_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkKDE.CheckStateChanged
    If IsNothing(MenueParam) Then Exit Sub
    MenueParam.Misch.Kgx = chkKDE.Checked
    Call VisVisRezept()
  End Sub



  Private Sub frmColorRaumBerechnung_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Resize
    Call ResizeChild(Me)
  End Sub

  


  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    Dispose()
  End Sub


  Private Sub txtDickKor_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtDickKor_0.Leave, txtDickKor_1.Leave
    Dim dik As Single
    dik = Singl(sender.Text)
    RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(0) = dik
    RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(1) = dik
    txtDickKor(0).Text = HandleRezept.AutFormat(RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(0))
    txtDickKor(1).Text = HandleRezept.AutFormat(RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(1))
  End Sub
  

  



  
  

  Private Sub lblMessUnt_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMessUnt_0.DoubleClick, lblMessUnt_1.DoubleClick
    Dim Index As Short
    Index = CInt(sender.name.ToString.Substring(11, 1))
    UntID(Sortindex)(Index) = -1
    lblMessUnt(Index).Text = ""
    GrpRwerteAll(Sortindex)(Index).RefUnt.Name = ""
    GrpRwerteAll(Sortindex)(Index).RefUnt.IVoNa = False
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
        ReWrRwert.ReadRwert(ID, GrpRwerteAll(Sortindex)(j)(ReUn), ier)
      ElseIf RcmdRef < 0 Then
        j = -RcmdRef - 1
        ReUn = "V"
        ReWrRwert.ReadRwert(ID, GrpRwerteAll(Sortindex)(j).RefUnt, ier)
      End If
      If ier = 0 Then

        Select Case RcmdRef
           
          Case -1, -2
            lblMessUnt(-RcmdRef - 1).Text = GrpRwerteAll(Sortindex)(j).RefUnt.Name
            UntID(Sortindex)(RezGraphics.Vkwb(j)) = ID
            GrpRwerteAll(Sortindex)(j).RefUnt.IVoNa = True
        End Select
      End If

    End If

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
    GetPutReflex.Messrefel = GrpRwerteAll(Sortindex)(Index).RefUnt

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

  

  





  

  


  Private Sub btnsortiment_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSortiment.Click
    Dim i As Integer
    SplitContVisible = 0
    
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
    For i = 0 To MenueParam.User.Winkel.Km - 1
      'RezCheckRad.chkWIN(i).enabled = False
    Next i
        For i = 0 To MenueParam.Normfa.Nlz - 1
          RezCheckRad.radNLA(i).enabled = False
        Next i
        chkVOL.Enabled = True
        cboMsh.Enabled = True


        '
        '
        '
        '
        '
        '

    RezCheckRad.cboSKAL.Enabled = False
    btnKopieren.Visible = False
    btnDrucken.Visible = False
    btnGRID.Enabled = False
    btnBild.Enabled = False
    btnRaumRechnung.Visible = True
    btnRaumRechnung.Enabled = True
    btnBild.Visible = False
    For i = 0 To MenueParam.User.Winkel.Km - 1
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




   

  WriteOnly Property SplitContVisible As Integer
    Set(value As Integer)
      Dim i As Integer
      For i = 0 To SplitContRaumBerechnung.Count - 1
        SplitContRaumBerechnung(i).Visible = False
      Next i
      SplitContRaumBerechnung(value).Visible = True
    End Set
  End Property

  Private Sub btnKopieren_Click(sender As Object, e As System.EventArgs) Handles btnKopieren.Click
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

  

  

 

  
  
  Private Sub radSOO_CheckedChanged(sender As Object, e As System.EventArgs) Handles radSOO_0.CheckedChanged, radSOO_1.CheckedChanged, radSOO_2.CheckedChanged, radSOO_3.CheckedChanged
    Dim k As Integer
    Dim i As Integer
    Sortindex = CInt(sender.name.substring(7, 1))
    For i = 0 To RezptGrid.Count - 1
      RezptGrid(i).chkVOL = Nothing
      RezptGrid(i).txtMNG_0 = Nothing
      RezptGrid(i).txtMNG_1 = Nothing
      RezptGrid(i).txtPRO_0 = Nothing
      RezptGrid(i).txtPRO_1 = Nothing
      RezptGrid(i).cboMNG = Nothing
      RezptGrid(i).cboPRO_0 = Nothing
      RezptGrid(i).cboPRO_1 = Nothing
    Next
    RezptGrid(Sortindex).chkVOL = chkVOL
    RezptGrid(Sortindex).txtMNG_0 = txtMNG_0
    RezptGrid(Sortindex).txtMNG_1 = txtMNG_1
    RezptGrid(Sortindex).txtPRO_0 = txtPRO_0
    RezptGrid(Sortindex).txtPRO_1 = txtPRO_1
    RezptGrid(Sortindex).cboMNG = cboMng
    RezptGrid(Sortindex).cboPRO_0 = cboPro_0
    RezptGrid(Sortindex).cboPRO_1 = cboPro_1

    '
    '
    '
    'Umhängen
    '
    '
    '
   
    If radSOO(Sortindex).Checked Then
      For k = 0 To TDBFar.Count - 1
        TDBFar(k).Visible = False
        lstFAR(k).Visible = False
      Next
      TDBFar(Sortindex).Visible = True
      lstFAR(Sortindex).Visible = True
      RezGraphics.AllRezepte = RezSozptAll(Sortindex)
      RezGraphics.GrpRwerte = GrpRwerteAll(Sortindex)
      '
      RezGrid = RezptGrid(Sortindex)
      '
      lblMessUnt(0).Text = GrpRwerteAll(Sortindex)(0).RefUnt.Name
      lblMessUnt(1).Text = GrpRwerteAll(Sortindex)(1).RefUnt.Name
      txtDickKor(0).Text = RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(0)
      txtDickKor(1).Text = RezSozptAll(Sortindex).Rezepte(KeyMenge).Dicke(1)
      For i = 0 To 1
        chkMessUnt(i).Checked = GrpRwerteAll(Sortindex)(0).RefUnt.IVoNa
      Next
    End If
  End Sub



  Private Sub btnRaumRechnung_Click(sender As Object, e As System.EventArgs) Handles btnRaumRechnung.Click
    Dim i As Integer
    Dim k As Integer
    Dim j As Integer
    Dim l As Integer
    Dim kw As Integer
    Dim RzNr As String
    Dim FaKeyid As String
    Dim RowResult As DataRow
    RezGrid.TDBFar.Update()
    For kw = 0 To MenueParam.User.Winkel.Km - 1
      RezCheckRad.radWIN(kw).Enabled = False
    Next kw
    For i = 0 To MenueParam.Normfa.Nlz - 1
      RezCheckRad.radNLA(i).Enabled = False
    Next i
    btnSortiment.Enabled = False
    Cursor = Cursors.WaitCursor
    SplitContainRaumRechnung.Visible = False
    lblARB.SetBounds(Me.Location.X, Me.Location.Y, _
     Me.Size.Width, Me.Size.Height)
    lblARB.Visible = True
    For j = 0 To chkLST.Count - 1
      chkLST(j).Visible = False
      chkLST(j).Checked = False
    Next
    For j = 0 To chkSOR.Count - 1
      chkSOR(j).Visible = False
      chkSOR(j).Checked = False
    Next
    For j = 0 To radSOR.Count - 1
      radSOR(j).Visible = False
      radSOR(j).Checked = False
    Next

    '
    '
    '
    '
    '
    '
    '
    'Menüvariable für Winkel übernehmen(4 low;5 height; 6 diff)
    '
    MenueParam.Menue.Pallg(4) = Singl(txtTON(0).Text)
    MenueParam.Menue.Pallg(5) = Singl(txtTON(1).Text)
    MenueParam.Menue.Pallg(6) = Singl(txtTON(2).Text)
    '
    'Menüvariable für Helligkeit übernehmen(1 low;2 height; 3 diff)
    '
    MenueParam.Menue.Pallg(1) = Singl(txtHEL(0).Text)
    MenueParam.Menue.Pallg(2) = Singl(txtHEL(1).Text)
    MenueParam.Menue.Pallg(3) = Singl(txtHEL(2).Text)
    '
    '

  
    '
    '
    FillGrid = Nothing
    btnBild.Enabled = False
    btnKopieren.Enabled = False
    btnDrucken.Enabled = False
    btnSortiment.Enabled = False
    '
    '
    'Sortiment abspeichern
    '
    '
    '
    '
    '
    '
    'cboWin.Enabled = True
    If KeyMenge = "SOR" Then
      ' Call SorSpei(KeyMenge, RezSozpt, UntID, TypID, ier)
    End If
    If ier <> 0 Then
      ' GoTo RezepteFeh
    Else
      Cursor = Cursors.WaitCursor
    End If

    '
    '
    'Winkel/Licharten
    '
    '
    '
   
    '
    '
    chkFAR.Enabled = True
    chkVOL.Enabled = False
    cboMsh.Enabled = False
    '
    '





    SplitContVisible = 1
    lblARB.Visible = True
    RezCheckRad.chkUUU(0).CheckState = CheckState.Indeterminate
    RezCheckRad.chkUUU(1).CheckState = CheckState.Indeterminate
    Cursor = Cursors.WaitCursor
    MenueParam.Misch.Kwh = RezGraphics.Kwop
    MenueParam.Menue.NLausw = RezGraphics.Knlz

    '


    'Raumberechnung
    '
    '
    If RezSozptAll(k).Rezepte(KeyMenge).KF = 0 Then
      MsgBox(Texxt(4014) & ": " & RezSozptAll(0).Rezepte(KeyMenge).Name, MsgBoxStyle.OkOnly, Texxt(2000))
      GoTo raufeh
    End If
    '
    '
    '
    Iprn = 0
    For k = 0 To 3
      

      '



      '

      Application.DoEvents()
      '
      If RezSozptAll(k).Farben.FarbCount > 0 And RezSozptAll(k).Rezepte.RezCount > 0 And RezSozptAll(k).Rezepte(KeyMenge).KF > 0 Then
        For i = 0 To 1
          If chkMessUnt(i).Checked And i = 0 Then
            If Not GrpRwerteAll(k)(i).RefUnt.IVoNa Then
              MsgBox(Texxt(2964 + i))
              GoTo raufeh
            End If
          End If
        Next i
        If Not HandleRezept.IVorNa("Z", GrpRwerteAll(k), ChkUNT, chkKDE, Nothing) Then
          SplitContainRaumRechnung.Visible = True

          SplitContVisible = 0
          Exit Sub
        End If

        Call CalcRezept.RaumRezept(Iprn, IanzHell, IanzWink, MenueParam.User.Winkel, KeyMenge, RezSozptAll(k), GrpRwerteAll(k), ier)
        If ier <> 0 Then GoTo RauFeh
        chkSOR(k).Text = RezSozptAll(k).Rezepte(KeyMenge).Name

      End If
      '
    Next k

    SplitContainRaumRechnung.Visible = True
    SplitContVisible = 1

    ReDim ISort(3)
    ReDim IHell(IanzHell - 1)
    For j = 0 To IanzHell - 1
      RzNr = KeyRe(j) + KeyRe(0)
      chkLST(j).Visible = True
      chkLST(j).Text = Format(RezSozptAll(0).Rezepte(RzNr).Sorkrit(0)(3), "###.0")
      IHell(j) = -1
    Next j
    IHell(0) = 1
    IanzSort = 0
    For k = 0 To 3
      ISort(k) = -1
      If RezSozptAll(k).Farben.FarbCount > 0 And RezSozptAll(k).Rezepte.RezCount > 0 And RezSozptAll(k).Rezepte(KeyMenge).KF > 0 Then
        chkSOR(k).Visible = True
        chkSOR(k).Checked = False
        radSOR(k).Visible = True
        IanzSort = IanzSort + 1
      End If
    Next k
    ISort(0) = 1
    chkSOR(0).Checked = True
    btnSortiment.Enabled = True
    '
    '
    '
    '
    '
    '
    'Tabellen aufbauen
    '
    For k = 0 To IanzSort - 1
      TabREZ(k).Columns.Clear()
      TabREZ(k).Rows.Clear()
      TabREZ(k).Columns.Add("KennSo", GetType(String))
      TabREZ(k).Columns.Add("KennL*", GetType(String))
      TabREZ(k).Columns.Add("Kennh", GetType(String))
      TabREZ(k).Columns.Add("L*(ist)", GetType(Single))
      TabREZ(k).Columns.Add("C*(ist)", GetType(Single))
      TabREZ(k).Columns.Add("h(ist)", GetType(Single))
      TabREZ(k).Columns.Add("L*(soll)", GetType(Single))
      TabREZ(k).Columns.Add("C*(soll)", GetType(Single))
      TabREZ(k).Columns.Add("h(soll)", GetType(Single))
      For l = 0 To RezSozptAll(k).Rezepte(KeyMenge).KF - 1
        FaKeyid = KeyName(RezSozptAll(k).Rezepte(KeyMenge)(l).ID)
        TabREZ(k).Columns.Add(Trim(RezSozptAll(k).Farben(FaKeyid).Name), GetType(Single))
      Next l
      '
      '
      '
      '
      For j = 0 To IanzHell - 1
        For i = 0 To IanzWink - 1
          RzNr = KeyRe(j) & KeyRe(i)
          If RezSozptAll(k).Rezepte.ContainsKey(RzNr) Then
            RowResult = TabREZ(k).NewRow
            RowResult("KennSo") = KeyRe(k)
            RowResult("KennL*") = KeyRe(j)
            RowResult("Kennh") = KeyRe(i)
            For l = 3 To 8
              RowResult(TabREZ(k).Columns(l).ColumnName) = Format(RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(l - 3), "###.00")
            Next l
            For l = 0 To RezSozptAll(k).Rezepte(RzNr).KF - 1
              FaKeyid = KeyName(RezSozptAll(k).Rezepte(RzNr)(l).ID)
              RowResult(Trim(RezSozptAll(k).Farben(FaKeyid).Name)) = Format(RezSozptAll(k).Rezepte(RzNr)(l).FaAmng, RezSozptAll(k).Farben(KeyName(FaKeyid)).Form)
            Next l
            TabREZ(k).Rows.Add(RowResult)
          End If
        Next i
      Next j
    Next k
    Cursor = Cursors.Default
    btnRaumRechnung.Visible = False
    btnBild.Visible = True
    btnBild.Enabled = True
    btnGRID.Enabled = True
    btnGRID.Visible = True
    btnDrucken.Visible = True
    btnDrucken.Enabled = True
    btnSortiment.Enabled = True
    SplitContainRaumRechnung.Visible = True
    SplitContainRaumRechnung.Enabled = True
    SplitContChart.Enabled = True
    chkSOR(0).Checked = True
    chkLST(0).Checked = True
    radSOR(0).Checked = True
    Exit Sub
    '
    '
    '
    'Fehler
    '
RauFeh:
    btnSortiment.Enabled = True
    SplitContVisible = 0
    SplitContainRaumRechnung.Visible = True
    Cursor = Cursors.Default
    '
    '###########################
    '
    'Prüfen, ob Reflexionswerte vorhanden
    '
    '

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
    '
    '
    '
    RezCheckRad.cboSKAL.Enabled = True
    '
    '

    '
    '
  End Sub

  Private Function RezDruck() As Object
    Throw New NotImplementedException
  End Function


  Private Sub btnStoreSorti_Click(sender As Object, e As System.EventArgs) Handles btnStoreSorti.Click
    RezGraphics.DataChanged = True
    If KeyMenge = "SOR" Then
      Call HandleRezept.SorSpei(RezGraphics.DataChanged, lstSOR, KeyMenge, RezSozptAll(Sortindex), OleAdSorti, DsetRezpt.Tables("tblsorti"), RwWrSortim, UntID(Sortindex), TypID(Sortindex), ier)

      'Call SorSpei(KeyMenge, RezSozptAll(Sortindex), UntID(Sortindex), TypID(Sortindex), ier)
    End If
  End Sub

  Private Sub chkLST_CheckedChanged(sender As Object, e As System.EventArgs) Handles _
   chkLST_00.CheckedChanged, chkLST_01.CheckedChanged, chkLST_02.CheckedChanged, chkLST_03.CheckedChanged, chkLST_04.CheckedChanged, chkLST_05.CheckedChanged, chkLST_06.CheckedChanged, chkLST_07.CheckedChanged, _
   chkLST_08.CheckedChanged, chkLST_09.CheckedChanged, chkLST_10.CheckedChanged, chkLST_11.CheckedChanged, chkLST_12.CheckedChanged, chkLST_13.CheckedChanged, chkLST_14.CheckedChanged, chkLST_15.CheckedChanged, _
   chkLST_16.CheckedChanged, chkLST_17.CheckedChanged, chkLST_18.CheckedChanged, chkLST_19.CheckedChanged, chkLST_20.CheckedChanged, chkLST_21.CheckedChanged, chkLST_22.CheckedChanged, chkLST_23.CheckedChanged, _
   chkLST_24.CheckedChanged
    Dim index As Integer
    index = CInt(sender.name.substring(7, 2))
    IHell(index) = chkLST(index).CheckState
    Call GraphRAU(2, ISort, IHell, IanzWink, MenueParam.Normfa(RezGraphics.Knlz).NormNama, MenueParam.User.Winkel(RezGraphics.Kwop).Chrm, graRAUMB)

  End Sub
  Private Sub chkSOR_CheckedChanged(sender As Object, e As System.EventArgs) Handles _
  chkSOR_0.CheckedChanged, chkSOR_1.CheckedChanged, chkSOR_2.CheckedChanged, chkSOR_3.CheckedChanged
    Dim index As Integer
    index = CInt(sender.name.substring(7, 1))
    ISort(index) = chkSOR(index).CheckState
    Call GraphRAU(2, ISort, IHell, IanzWink, MenueParam.Normfa(RezGraphics.Knlz).NormNama, MenueParam.User.Winkel(RezGraphics.Kwop).Chrm, graRAUMB)

  End Sub

  Private Sub btnBild_Click(sender As Object, e As System.EventArgs) Handles btnBild.Click
    btnKopieren.Enabled = False
    btnRaumRechnung.Visible = False
    btnDrucken.Visible = True
    btnGRID.Visible = True
    btnKopieren.Visible = False
    SplitContVisible = 1
  End Sub

  Private Sub btnGRID_Click(sender As Object, e As System.EventArgs) Handles btnGRID.Click
    btnKopieren.Visible = True
    btnBild.Visible = True
    btnDrucken.Visible = False
    btnGRID.Visible = False
    btnKopieren.Enabled = True
    btnDrucken.Enabled = True
    'sstRezept.Visible = True

    SplitContVisible = 2
  End Sub

 
  '
  '
  '
  '
  '
  '
  Sub GraphRAU(DraMode As Integer, ISort() As Integer, IHell() As Integer, Kzl As Integer, NormName As String, WinName As String, GraRAU As C1.Win.C1Chart.C1Chart)
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim l As Integer

    Dim NRAUSet As Integer
    Dim RzNr As String
    Dim Igut() As Integer
    Dim s As C1.Win.C1Chart.ChartDataSeries
    '
    '
    '
    Cursor = Cursors.Default
    For i = 0 To chkSOR.Count - 1
      chkSOR(i).Enabled = False
    Next i
    For i = 0 To chkLST.Count - 1
      chkLST(i).Enabled = False
    Next i
    NRAUSet = 0
    For k = 0 To ISort.Count - 1
      If ISort(k) = 1 Then
        For j = 0 To IHell.Count - 1
          If IHell(j) = 1 Then
            NRAUSet = NRAUSet + 1
          End If
        Next j
      End If
    Next k
    If Kzl < 2 Or NRAUSet < 1 Then
      GraRAU.Visible = False
      For i = 0 To chkSOR.Count - 1
        chkSOR(i).Enabled = True
      Next i
      For i = 0 To chkLST.Count - 1
        chkLST(i).Enabled = True
      Next i
      Exit Sub
    End If
    'h = 7#
    ReDim Igut(NRAUSet)
    l = 0
    For k = 0 To ISort.Count - 1
      If ISort(k) = 1 Then
        For j = 0 To IHell.Count - 1
          If IHell(j) = 1 Then
            For i = 0 To Kzl - 1
              RzNr = KeyRe(j) & KeyRe(i)
              '              If Abs(RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(0) - RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(3)) < 2 And _
              '              Abs(RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(2) - RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(5)) < 2 Then
              '
              'Gute Rezepte Sorkrit(0)(8)=0.0 kennzueichnen
              '
              '
              '
              If RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(8) = 0.0 Then
                Igut(l) = i
                Exit For
              End If
            Next i
            l = l + 1
          End If
        Next j
      End If
    Next k
    GraRAU.Visible = False
    GraRAU.Header.Text = NormName & Space(4) & WinName
    'GraRAU.FontUse = 3
    'GraRAU.FontSize = 50
    l = 0
    ' GraRAU.ChartGroups(1).SeriesLabels.RemoveAll()
    GraRAU.Reset()
    GraRAU.ChartGroups.Group0.ChartData.SeriesList.Clear()
    GraRAU.ChartGroups.Group0.ChartType = C1.Win.C1Chart.Chart2DTypeEnum.Polar
    '
    GraRAU.BackColor = Color.White
    GraRAU.ChartArea.AxisX.AutoMax = False
    GraRAU.ChartArea.AxisX.AutoMin = False
    GraRAU.ChartArea.AxisX.Max = 360
    GraRAU.ChartArea.AxisX.Min = 0
    GraRAU.ChartArea.AxisY.AutoMax = True
    GraRAU.ChartArea.AxisY.AutoMin = False
    GraRAU.ChartArea.AxisY.Min = 0.0
    GraRAU.ChartGroups.Group0.ChartData.SeriesList.Clear()
    GraRAU.ChartArea.PlotArea.BackColor = Color.WhiteSmoke
    GraRAU.ChartArea.Style.BackColor = Color.White
    'GraRAU.Footer.Text = txtFarbmittel.Text & "/" & cboBindWeiss.Text
    GraRAU.Legend.Visible = True
    GraRAU.Legend.Style.BackColor = GraRAU.ChartArea.PlotArea.BackColor
    GraRAU.Legend.Compass = C1.Win.C1Chart.CompassEnum.East
    GraRAU.ChartGroups.Group0.Polar.Degrees = True
    GraRAU.ChartGroups.Group0.ChartData.SeriesList.Clear()
    '
    '
    '
    '
    '
    For k = 0 To ISort.Count - 1
      If ISort(k) = 1 Then
        For j = 0 To IHell.Count - 1
          If IHell(j) = 1 Then

            RzNr = KeyRe(j) & KeyRe(1)
            s = New C1.Win.C1Chart.ChartDataSeries
            s.PointData.Clear()
            s.SymbolStyle.Shape = C1.Win.C1Chart.SymbolShapeEnum.None
            s.LineStyle.Thickness = 2
            s.LineStyle.Color = s.SymbolStyle.Color
            s.LineStyle.Color = farbini(l)
            s.Label = "L*=" & Format(RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(3), "###.0") & Space(2) & Trim(RezSozptAll(k).Rezepte("SOR").Name)
            s.LegendEntry = True
            For i = 0 To Kzl - 1
              
              RzNr = KeyRe(j) & KeyRe(i)
              s.X.Add(RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(5))
              If Abs(RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(0) - RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(3)) < 2 And _
                Abs(RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(2) - RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(5)) < 2 Then
                s.Y.Add(RezSozptAll(k).Rezepte(RzNr).Sorkrit(0)(1))
                Igut(l) = i
              Else
                If Igut(l) > 0 Then
                  RzNr = KeyRe(j) & KeyRe(Igut(l))
                End If
                s.Y.Add(GraRAU.ChartGroups.Group0.ChartData.Hole)
              End If
            Next i
            GraRAU.ChartGroups.Group0.ChartData.SeriesList.Add(s)
            s.Display = C1.Win.C1Chart.SeriesDisplayEnum.Show
            l = l + 1
          End If
        Next j
      End If
    Next k







    GraRAU.Visible = True

    For i = 0 To chkSOR.Count - 1
      chkSOR(i).Enabled = True
    Next i
    For i = 0 To chkLST.Count - 1
      chkLST(i).Enabled = True
    Next i
    Cursor = Cursors.Default

    Exit Sub

  End Sub



 
 
  Private Sub radSOR_CheckedChanged(sender As Object, e As System.EventArgs) Handles _
   radSOR_0.CheckedChanged, radSOR_1.CheckedChanged, radSOR_2.CheckedChanged, radSOR_3.CheckedChanged
    Dim index As Integer
    Dim i As Integer
    index = CInt(sender.name.substring(7, 1))
    flgREZ.DataSource = TabREZ(index)
    flgREZ.AlternatingRowsDefaultCellStyle.BackColor = Color.WhiteSmoke
    For i = 0 To flgREZ.ColumnCount - 1
      flgREZ.Columns(i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      flgREZ.Columns(i).DefaultCellStyle.Format = "####0.000"
      flgREZ.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
      '
      '
      flgREZ.Columns(i).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
      If i = 0 Then
        flgREZ.Columns(0).HeaderText = RezSozptAll(index).Rezepte(KeyMenge).Name & Space(2) & "(" & MenueParam.Normfa(RezGraphics.Knlz).NormNama & ")" & Space(2) & "(" & MenueParam.User.Winkel(RezGraphics.Kwop).Chrm & ")"
        flgREZ.Columns(0).Frozen = True
      ElseIf i = 1 Or i = 2 Then
        flgREZ.Columns(i).Frozen = True
      End If
    Next i
  End Sub

  Private Sub btnDrucken_Click(sender As Object, e As System.EventArgs) Handles btnDrucken.Click
    Dim ps As New System.Drawing.Printing.PageSettings
    ps.Landscape = True
    graRAUMB.PrintChart(C1.Win.C1Chart.PrintScaleEnum.ScaleToWidth, Nothing, ps)

  End Sub
  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtMNG_0.Validating, txtMNG_1.Validating, txtPRO_0.Validating, txtPRO_1.Validating, txtSum_0.Validating, txtSUM_1.Validating, txtSum_2.Validating, txtSum_3.Validating, _
    txtDickKor_0.Validating, txtDickKor_1.Validating, txtHEL_0.Validating, txtHEL_1.Validating, txtHEL_2.Validating, txtTON_0.Validating, txtTON_1.Validating, txtTON_2.Validating, _
    txtKDE.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub

  
End Class