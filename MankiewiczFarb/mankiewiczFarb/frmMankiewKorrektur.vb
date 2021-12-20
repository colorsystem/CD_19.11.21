Option Compare Text
Option Strict Off
Option Explicit On
Public Class frmMankiewKorrektur
  Inherits System.Windows.Forms.Form
  Friend WithEvents cboMisch As ComboBox
  Dim ier As Integer
  Dim RezKor As String
  Dim RezAlt As String
  Dim RezKey() As String
  Dim RzNr As String
  Dim TypID(1) As Integer
  Dim UntID(1) As Integer
  Dim SmpID(1) As Integer
  Dim OlduserID As Integer
  Dim RzId As Integer
  Dim Preis As Single
  Dim KeyId As String
  Dim ZuMax As Single
  Dim ZuProz As Single
  Dim GGEMax As Single
  Dim RezName As String
  Dim KFIT As Integer
  '
  '
  '
  Dim GrpRwerte As RefValuesGrp
  Dim GrpRwrtRest As RefValuesGrp
  Dim GrpRwrtUmrech As RefValuesGrp
  Dim RestFarb As Colorant
  Dim RezSozpt As RecipesGrp
  Dim OptGesamt As OpticalData
  Dim CalcRezept As RezeptBerechnung
  Dim HandleRezept As HandleRezGeneral
  Dim Umr As RezeptUmrechnung
  Dim WithEvents RezTab As DataTable
  Dim FarbTabelle As DataTable
  Dim RezepteTab As DataTable
  Dim PicGraphic As HandleRezGrafik
  Dim picAuf As HandlePictures
  Dim RezDruck As HandlePlottDruck
  Dim ReWrRezept As ReadWriteRezept
  Dim ReWrFarbe As ReadWriteFarbe
  Dim ReWrGrund As ReadWriteGrund
  Dim CmdRezepte As OleDbCommand
  Dim OleRezepte As OleDbDataAdapter
  Dim CmdFarbmittel As OleDbCommand
  Dim OleFarbmittel As OleDbDataAdapter
  Dim FarbmittelTable As DataTable
  Dim FarbAlleTabelle As DataTable
  Dim DataAdapter As OleDbDataAdapter
  Dim SqlStFarb As String
  Dim GetRwerte As HandleRwerte '
  Dim HscValue As Integer
  Dim MngMaxAlt As Single
  Dim MngMinAlt As Single
  Dim ZuPrz As Single
  Dim DickeNeu As String
  Dim RezIDalt As Integer
  Dim NormGewAlt As Single
  '
  Dim lblGewNL As List(Of Label)
  Dim txtGewNL As List(Of TextBox)
  Dim radNachst As List(Of RadioButton)
  Dim lblLABCH As List(Of Label)
  Dim txtLAB As List(Of TextBox)
  Dim txtLCH As List(Of TextBox)


  Dim ArbFarb As Colorant
  '
  Dim FarbWrtStd As ValuesGrpsAssigns
  '
  '
  '
  '
  '
  '
  Dim radwinkel As List(Of RadioButton)
  Dim chkwinkel As List(Of CheckBox)
  Dim lblwinkel As List(Of Label)
  Dim panwinkel As Panel
  Dim radLichtart As List(Of RadioButton)
  Dim chkLichtart As List(Of CheckBox)
  Dim lblLichtart As List(Of Label)
  Dim panLichtart As Panel
  Dim txtSUM As List(Of TextBox)
  '
  '
  Dim ColumnInfor As DataGridViewComboBoxColumn
  '
  '
  ''
  Dim ppv As PrintPreviewDialog
  Dim ZusInf() As String



  Private Sub frmMankiewKorrektur_Activated(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Activated
    Call ShowMe()
  End Sub


  Private Sub frmMankiewKorrektur_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
    Call HideMe()
  End Sub







  Private Sub frmMankiewKorrektur_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    FormMDI.Icon = My.Resources.korrektur
    FormMDI.Text = MenueParam.User.Name & " - " & MenueParam.Messg.Kbez & " - " & MenueParam.Menue.MethBez
    RezIDalt = -1
    NormGewAlt = MenueParam.Normfa(0).NormGew
    DickeNeu = ""

    radwinkel = FormMDI.radWinkel
    chkwinkel = FormMDI.chkWinkel
    lblwinkel = FormMDI.lblWinkel
    panwinkel = FormMDI.panWinkel
    radLichtart = FormMDI.radLichtart
    chkLichtart = FormMDI.chkLichtart
    lblLichtart = FormMDI.lblLichtart
    panLichtart = FormMDI.panLichtart
    FarbWrtStd = New ValuesGrpsAssigns
    FarbWrtStd.clear()
    AufbauPar.AufbauRezeptMerk(FarbWrtStd, ier)
    Call LichtArtText()
    GetRwerte = New HandleRwerte
    GetRwerte.Measure = Measure
    cboMisch = FormMDI.cboMISCH

    btnDrucken.Text = Texxt(4608)
    btnDruckenZus.Text = Texxt(4608)
    btnUserProg.Text = Texxt(392)
    btnFarbmetrik.Text = Texxt(4615)
    btnKorrektur.Text = Texxt(731)
    btnKorrSpezial.Text = Texxt(737)
    btnPictureDrucken.Text = Texxt(856)
    btnPictureZurück.Text = Texxt(4617)
    btnPrevZurück.Text = Texxt(4617)
    btnRezepte.Text = Texxt(4607)
    btnRwerteNachstellung.Text = Texxt(4613)
    btnRwerteVorlage.Text = Texxt(4612)
    btnRwerteUntergrund.Text = Texxt(4619)
    btnSpeicher.Text = Texxt(4614)
    btnRezVerw.Text = Texxt(4647)
    btnRezeptZurück.Text = Texxt(4617)
    lblGewFakt.Text = Texxt(446)
    chkGEWICHT.Text = Texxt(446)
    chkDatum.Text = Texxt(140)
    chkVol.Text = Texxt(830)
    lblBIS.Text = Texxt(377)
    chkWithTempl.Text = Texxt(4702)
    radNachst_0.Text = Texxt(3110) & " (" & Texxt(786) & ")"
    radNachst_1.Text = Texxt(1242)
    radNachst_2.Text = Texxt(1243)
    lblGlzGrd.Text = Texxt(870)
    '
    '
    radNachst = New List(Of RadioButton)
    radNachst.Add(radNachst_0)
    radNachst.Add(radNachst_1)
    radNachst.Add(radNachst_2)

    lblLABCH = New List(Of Label)
    lblLABCH.Add(lblLABCH_0)
    lblLABCH.Add(lblLABCH_1)
    lblLABCH.Add(lblLABCH_2)
    '
    txtLAB = New List(Of TextBox)
    txtLAB.Add(txtLAB_0)
    txtLAB.Add(txtLAB_1)
    txtLAB.Add(txtLAB_2)
    '
    txtLCH = New List(Of TextBox)
    txtLCH.Add(txtLCH_0)
    txtLCH.Add(txtLCH_1)
    txtLCH.Add(txtLCH_2)
    '


    '
    txtSUM = New List(Of TextBox)
    txtSUM.Add(txtSUM_0)
    txtSUM.Add(txtSUM_1)
    txtSUM.Add(txtSUM_2)
    txtSUM.Add(txtSUM_3)
    txtSUM.Add(txtSUM_4)


    Call LichtArtText()
    lblMeta.Text = Texxt(841)
    lblPreis.Text = Texxt(4633)
    lblMenge.Text = Texxt(602)
    lblDosis.Text = Texxt(4611)
    lblVOR.Text = Texxt(4645)
    lblNach.Text = Texxt(4646)

    lblPicKorrektur.Text = Texxt(731)
    lblPicNachstellung.Text = Texxt(786)
    lblPicVorlage.Text = Texxt(861)
    lblRezept.Text = Texxt(863)
    lblSuchName.Text = Texxt(4606)
    lblZuwaage.Text = Texxt(4616)
    lblMinMeng.Text = Texxt(4648)
    lblMIN.Text = Texxt(4663)
    lblMAX.Text = Texxt(4665)
    lblACT.Text = Texxt(4664)
    lblProzBis.Text = Texxt(377)
    lblDickeAlt.Text = Texxt(832) & "(" & Texxt(880) & ")"
    lblDickeNeu.Text = Texxt(832) & "(" & Texxt(878) & ")"
    lblDTO.Text = Texxt(4682)
    chkRwertAngleich.Text = Texxt(4708)
    Application.DoEvents()
    GrpRwerte = New RefValuesGrp
    GrpRwrtRest = New RefValuesGrp
    GrpRwrtUmrech = New RefValuesGrp
    RezSozpt = New RecipesGrp
    CalcRezept = New RezeptBerechnung
    HandleRezept = New HandleRezGeneral

    ReWrRezept = New ReadWriteRezept
    OptGesamt = New OpticalData
    ReWrFarbe = New ReadWriteFarbe
    ReWrGrund = New ReadWriteGrund
    ArbFarb = New Colorant
    RezKor = ""
    RezAlt = ""
    Umr = New RezeptUmrechnung
    RezAlt = "MNG"
    RezKor = "KOR"
    RezSozpt.Rezepte.AddRez(RezAlt, New Recipe)
    RezSozpt.Rezepte.AddRez(RezKor, New Recipe)
    RezSozpt.Rezepte.AddRez("ZUW", New Recipe)
    RezSozpt.Rezepte.AddRez("MZU", New Recipe)
    '
    '
    '
    FarbTabelle = New DataTable
    '
    '
    'Tabelle für Einzelrezepte
    '
    '
    RezTab = New DataTable
    RezTab.Columns.Add("FAID", GetType(Integer))
    RezTab.Columns.Add("FNAME", GetType(String))
    RezTab.Columns.Add("GEWICHT", GetType(Single))
    RezTab.Columns.Add("KORREK", GetType(Single))
    RezTab.Columns.Add("ZUWAAG", GetType(Single))
    RezTab.Columns.Add("FST", GetType(Single))
    RezTab.Columns.Add("PROZ", GetType(Single))
    RezTab.Columns.Add("PROB", GetType(Single))
    '
    '
    '
    RezTab.Columns("FAID").DefaultValue = -1
    RezTab.Columns("FNAME").DefaultValue = Space(1)
    RezTab.Columns("GEWICHT").DefaultValue = -1
    RezTab.Columns("KORREK").DefaultValue = -1
    RezTab.Columns("ZUWAAG").DefaultValue = -1
    RezTab.Columns("FST").DefaultValue = 100
    RezTab.Columns("PROZ").DefaultValue = 100
    RezTab.Columns("PROB").DefaultValue = 100
    For i = 0 To RezTab.Columns.Count - 1
      RezTab.Columns(i).AllowDBNull = False
    Next
    TDBRezept.DataSource = RezTab
    '
    '
    '
    SplitContainRezManag.Hide()
    SplitContainPreview.Hide()
    SplitContainPicture.Hide()
    SplitContainRezSuch.Show()
    If MenueParam.Messg.Winkel.Km <> MenueParam.User.Winkel.Km Then
      panFarbLabCH.Enabled = False
    End If
    '
    '
    '
    '
    '
    PicGraphic = New HandleRezGrafik
    picAuf = New HandlePictures
    picAuf.Add("REF", PictureBoxKurve)
    picAuf.Add("LAB", PictureBoxLab)
    picAuf.Add("REZ", PictureBoxRezept)
    picAuf.Add("FRB", PictureBoxfarbwerte)
    picAuf.Add("XYZ", pictureBoxFarbe)
    picAuf.PicGraphic = PicGraphic
    RezDruck = New HandlePlottDruck
    RezDruck.printset = pd.PrinterSettings

    'RezDruck.JProz = False
    'RezDruck.JProb = False
    'RezDruck.JStae = False
    'RezDruck.JSpez = False

    RezDruck.AllRezepte = RezSozpt
    RezDruck.GrpRwerte = GrpRwerte
    RezDruck.Winkel = MenueParam.User.Winkel
    RezDruck.Plott = PicGraphic
    ppv = New PrintPreviewDialog
    PicGraphic.GrpRwerte = GrpRwerte
    PicGraphic.AllRezepte = RezSozpt
    '
    '
    '
    '
    '
    'Tabelle für alle Rezepte (gemäß Suchbegriff)
    '
    '
    '
    '
    '
    CmdRezepte = New OleDbCommand
    OleRezepte = New OleDbDataAdapter
    CmdRezepte.Connection = Cndat()
    FarbAlleTabelle = New DataTable
    DataAdapter = New OleDbDataAdapter
    DataAdapter.SelectCommand = New OleDbCommand
    DataAdapter.SelectCommand.Connection = Cndat()
    '
    RezepteTab = New DataTable
    '
    '
    'Tabelle für Farbmittel
    '
    '
    '
    FarbmittelTable = New DataTable
    '
    '
    '
    'Art der Normierungen
    '
    '
    cboMNG.Items.Clear()
    For i = 0 To 18
      cboMNG.Items.Add(RepTexxt(Texxt(580 + i), Texxt(602)))
    Next i
    '
    lblGewNL = New List(Of Label)
    txtGewNL = New List(Of TextBox)
    lblGewNL.Clear()
    txtGewNL.Clear()
    For i = 0 To 4
      Select Case i
        Case 0
          lblGewNL.Add(lblGewNL0)
          txtGewNL.Add(txtGewNL0)
        Case 1
          lblGewNL.Add(lblGewNL1)
          txtGewNL.Add(txtGewNL1)
        Case 2
          lblGewNL.Add(lblGewNL2)
          txtGewNL.Add(txtGewNL2)
        Case 3
          lblGewNL.Add(lblGewNL3)
          txtGewNL.Add(txtGewNL3)
        Case 4
          lblGewNL.Add(lblGewNL4)
          txtGewNL.Add(txtGewNL4)
        Case 5
          lblGewNL.Add(lblGewNL5)
          txtGewNL.Add(txtGewNL5)
      End Select
    Next i
    '
    '
    '


    '
    '
    'Art der Mengenverhältnisse
    '
    '
    cboPROZZae.Items.Clear()
    cboPROZNen.Items.Clear()
    For i = 0 To 18
      cboPROZZae.Items.Add(RepTexxt(Texxt(580 + i), Texxt(602)))
      cboPROZNen.Items.Add(RepTexxt(Texxt(580 + i), Texxt(602)))
    Next i
    '
    ''
    'Grid Rezeptverwaltung
    '
    '
    '
    FarbTabelle.Clear()
   

    flgFarbmittel.Columns(0).HeaderText = "ID"
    flgFarbmittel.Columns(0).Visible = False
    flgFarbmittel.Columns(0).Width = 0
    flgFarbmittel.Columns(0).ReadOnly = True
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(0).DataPropertyName, GetType(Integer))
    FarbTabelle.Columns(0).DefaultValue = -1
    FarbTabelle.Columns(0).AllowDBNull = False
    '
    'Name
    '
    flgFarbmittel.Columns(1).HeaderText = Texxt(394)
    flgFarbmittel.Columns(1).Width = 200
    flgFarbmittel.Columns(1).Visible = True
    flgFarbmittel.Columns(1).ReadOnly = True
    flgFarbmittel.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(1).DataPropertyName, GetType(String))
    FarbTabelle.Columns(1).DefaultValue = Space(1)
    FarbTabelle.Columns(1).AllowDBNull = False

    '
    '
    'Menge
    '

    flgFarbmittel.Columns(2).HeaderText = Texxt(4630)
    flgFarbmittel.Columns(2).Width = 75
    flgFarbmittel.Columns(2).ReadOnly = False
    flgFarbmittel.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(2).DefaultCellStyle.Format = "###0.000"
    flgFarbmittel.Columns(2).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(2).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(2).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(2).DefaultValue = -1
    FarbTabelle.Columns(2).AllowDBNull = False

    '
    '
    'Farbstärke
    ' 
    flgFarbmittel.Columns(3).HeaderText = TexKt(21014)
    flgFarbmittel.Columns(3).Width = 75
    flgFarbmittel.Columns(3).ReadOnly = True
    flgFarbmittel.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(3).DefaultCellStyle.Format = "###0.000"
    flgFarbmittel.Columns(3).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(3).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(3).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(3).DefaultValue = 100
    FarbTabelle.Columns(3).AllowDBNull = False

    '
    '


    'Prozentigkeit
    ' 
    flgFarbmittel.Columns(4).HeaderText = Texxt(820)
    flgFarbmittel.Columns(4).Width = 75
    flgFarbmittel.Columns(4).ReadOnly = False
    flgFarbmittel.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(4).DefaultCellStyle.Format = "###0.000"
    flgFarbmittel.Columns(4).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(4).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(4).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(4).DefaultValue = 100
    FarbTabelle.Columns(4).AllowDBNull = False

    '
    '
    'Bindemittel-Prozentigkeit
    ' 
    flgFarbmittel.Columns(5).HeaderText = Texxt(821)
    flgFarbmittel.Columns(5).Width = 75
    flgFarbmittel.Columns(5).ReadOnly = True
    flgFarbmittel.Columns(5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(5).DefaultCellStyle.Format = "###0.000"
    flgFarbmittel.Columns(5).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(5).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(5).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(5).DefaultValue = 100
    FarbTabelle.Columns(5).AllowDBNull = False

    '
    '
    'Preis
    ' 

    flgFarbmittel.Columns(6).HeaderText = Texxt(822)
    flgFarbmittel.Columns(6).Width = 75
    flgFarbmittel.Columns(6).ReadOnly = False
    flgFarbmittel.Columns(6).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(6).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(6).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(6).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(6).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(6).DefaultValue = 0
    FarbTabelle.Columns(6).AllowDBNull = False

    '
    
    '
    'Operator
    '
    '
    '

    flgFarbmittel.Columns(7).HeaderText = "OP"
    flgFarbmittel.Columns(7).Width = 60
    flgFarbmittel.Columns(7).ReadOnly = False
    flgFarbmittel.Columns(7).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(7).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(7).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(7).DataPropertyName, GetType(String))
    FarbTabelle.Columns(7).DefaultValue = Space(1)
    FarbTabelle.Columns(7).AllowDBNull = False

    '
    'Limitierungsmenge
    '
    flgFarbmittel.Columns(8).HeaderText = Texxt(4637)
    flgFarbmittel.Columns(8).Width = 90
    flgFarbmittel.Columns(8).ReadOnly = False
    flgFarbmittel.Columns(8).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(8).DefaultCellStyle.Format = "###0.000"
    flgFarbmittel.Columns(8).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(8).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(8).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(8).DefaultValue = 100
    FarbTabelle.Columns(8).AllowDBNull = False

    '
    '
    'ICHF
    '
    '
    flgFarbmittel.Columns(9).HeaderText = "ICHF"
    flgFarbmittel.Columns(9).Visible = False
    flgFarbmittel.Columns(9).Width = 0
    flgFarbmittel.Columns(9).ReadOnly = True
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(9).DataPropertyName, GetType(Integer))
    FarbTabelle.Columns(9).DefaultValue = 0
    FarbTabelle.Columns(9).AllowDBNull = False

    '
    '
    '
    'FARBID
    '
    '
    flgFarbmittel.Columns(10).HeaderText = "FARBID"
    flgFarbmittel.Columns(10).Visible = False
    flgFarbmittel.Columns(10).Width = 0
    flgFarbmittel.Columns(10).ReadOnly = True
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(10).DataPropertyName, GetType(Integer))
    FarbTabelle.Columns(10).DefaultValue = -1
    FarbTabelle.Columns(10).AllowDBNull = False

    '
    '
    '
    '
    '
    flgFarbmittel.DataSource = FarbTabelle
    lstFarbmittel.DataSource = FarbAlleTabelle
    If BitWrt(30, MenueParam.User.Writ) Then
      For i = 0 To FarbTabelle.Rows.Count - 1
        flgFarbmittel.Rows(i).DefaultCellStyle.BackColor = Color.FromArgb(FarbTabelle.Rows(i)("FARBM_FARBID"))
      Next i
    End If

    '
    '
    '
    '
    '
    '
    ''
    'Grid Zusatzstoffe
    '
    '
    '
    flgZusatzstoffe.Columns.Add("ID", "ID")
    flgZusatzstoffe.Columns(0).Visible = False
    flgZusatzstoffe.Columns(0).Width = 0
    flgZusatzstoffe.Columns(0).ReadOnly = True
    '
    'Name
    '
    flgZusatzstoffe.Columns.Add("NAME", Texxt(3415))
    flgZusatzstoffe.Columns(1).Width = 200
    flgZusatzstoffe.Columns(1).Visible = True
    flgZusatzstoffe.Columns(1).ReadOnly = False
    flgZusatzstoffe.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    '
    '
    'Menge
    '
    flgZusatzstoffe.Columns.Add("MENGE", Texxt(3416))
    flgZusatzstoffe.Columns(2).Width = 75
    flgZusatzstoffe.Columns(2).ReadOnly = False
    flgZusatzstoffe.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgZusatzstoffe.Columns(2).DefaultCellStyle.Format = "###0.000"
    flgZusatzstoffe.Columns(2).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgZusatzstoffe.Columns(2).HeaderCell.SortGlyphDirection = SortOrder.None
    '
    '
    '
    'Grid Informationen
    '
    '
    '
    ColumnInfor = New DataGridViewComboBoxColumn
    ColumnInfor.HeaderText = Texxt(3417)
    ColumnInfor.Name = "NAME"
    ZusInf = GetPrivSettings("MANKIEWICZ", "ZUSINF", "", COLORFileName).Split(",")
    For i = 0 To ZusInf.Count - 1
      ColumnInfor.Items.Add(ZusInf(i))
    Next
    flgInformationen.Columns.Add(ColumnInfor)
    flgInformationen.Columns.Add("BEZ", Texxt(3418))
    flgInformationen.Columns(0).Width = 0.6 * flgInformationen.Width
    flgInformationen.Columns(1).Width = 0.3 * flgInformationen.Width
    flgInformationen.Columns(1).DefaultCellStyle.NullValue = ""
    flgInformationen.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None
    flgInformationen.AllowUserToAddRows = True
    '
    '
    '
    '
    If Not BitWrt(7, MenueParam.User.Writ) Then
      btnSpeicher.Enabled = False
    End If
    If BitWrt(22, MenueParam.User.Sonst) Then
      lblGlzGrd.Visible = True
      txtGlzGrd.Visible = True
    End If




    Call ShowMe()
  End Sub
  Sub ShowMe()
    Dim i As Integer
    If Not Me.Visible Then Exit Sub
    If IsNothing(RezSozpt) Then Exit Sub

    Cursor = Cursors.WaitCursor
    AufbauPar.MethID = 54
    'Measure.Umspeich(MenueParam.Messg, MenueParam.Normfa(0))
    GGEMax = MenueParam.Misch.Gge
    '
    'Art der Rezept bzw. Korrekturrechnung(muß auf 6 erweitert werden
    'falls Farbabstand DTO vorgegeben werden soll)
    '
    cboMethIGX.Items.Clear()
    cboMethIGX.ValueMember = "ID"
    cboMethIGX.DisplayMember = "TEXT"
    For i = 0 To 6
      cboMethIGX.Items.Add(New ListTextID(i, Texxt(572 + i)))
    Next i
    If MenueParam.Misch.Igx = 7 Then
      MenueParam.Misch.Igx = 0
    End If
    cboMethIGX.SelectedIndex = MenueParam.Misch.Igx
    '
    '

    cboMethIGX.Visible = BitWrt(14, MenueParam.User.Visbl)
    cboMethIGX.Enabled = BitWrt(14, MenueParam.User.Enabl)


    btnRezVerw.Enabled = False
    '
    txtDTO.Text = MenueParam.Misch.Dto
    'Unvisible
    '
    '
    pictureBoxFarbe.Visible = False
    lblPicVorlage.Visible = False
    lblPicNachstellung.Visible = False
    lblPicKorrektur.Visible = False
    panTDBRezept.Visible = False
    btnDrucken.Visible = False
    btnUserProg.Visible = False
    btnFarbmetrik.Visible = False
    btnSpeicher.Visible = False
    lblDEStern.Visible = False
    txtDEv.Visible = False
    txtDEn.Visible = False
    lblDLStern.Visible = False
    txtDLv.Visible = False
    txtDLn.Visible = False
    lblDaStern.Visible = False
    txtDav.Visible = False
    txtDan.Visible = False
    lblDbStern.Visible = False
    txtDbv.Visible = False
    txtDbn.Visible = False
    lblDCStern.Visible = False
    txtDCv.Visible = False
    txtDCn.Visible = False
    lblDHStern.Visible = False
    txtDHv.Visible = False
    txtDHn.Visible = False

    lblMeta.Visible = False
    txtMETAv.Visible = False
    lblPreis.Visible = False
    txtPreisv.Visible = False
    lblZuwaage.Visible = False
    txtZuwaag.Visible = False
    hscZuwaag.Visible = False
    txtMinMeng.Visible = False
    lblMinMeng.Visible = False
    hscMinMeng.Visible = False
    txtDTO.Visible = False
    lblDTO.Visible = False

    '
    '
    'Unenabled
    '
    '
    txtMenge.Enabled = False
    chkVol.Enabled = True
    RezTab.Rows.Clear()
    '
    '
    '
    '

    GrpRwerte.clear()
    GrpRwrtRest.clear()
    '
    If MenueParam.Misch.Vert = 0 Then
      GrpRwerte.Add("W", New RefValues)
      GrpRwerte.Add("S", New RefValues)
      GrpRwrtRest.Add("W", New RefValues)
      GrpRwrtRest.Add("S", New RefValues)
    Else
      GrpRwerte.Add("S", New RefValues)
      GrpRwerte.Add("W", New RefValues)
      GrpRwrtRest.Add("S", New RefValues)
      GrpRwrtRest.Add("W", New RefValues)
    End If
    GrpRwerte(0).Add("V", New RefValue)
    GrpRwerte(0).Add("N", New RefValue)
    GrpRwerte(0).Add("R", New RefValue)
    GrpRwerte(1).Add("V", New RefValue)
    GrpRwerte(1).Add("N", New RefValue)
    GrpRwerte(1).Add("R", New RefValue)
    GrpRwerte(0).RefUnt = New RefValue
    GrpRwerte(1).RefUnt = New RefValue
    GrpRwerte(0)("V").Itp = True
    GrpRwerte(1)("V").Itp = True
    '
    '
    '
    '
    'für Korrekturberechnung mit Restfarbe
    '
    '
    '
    For i = 0 To 1
      GrpRwrtRest(i).Add("V", GrpRwerte(i)("N"))
      GrpRwrtRest(i).Add("R", GrpRwerte(i)("R"))
      GrpRwrtRest(i).RefUnt = GrpRwerte(i).RefUnt
    Next i
    '
    'für R-Werte aus Farbdifferenzen
    '

    GrpRwrtUmrech.clear()

    GrpRwrtUmrech.Add("X", New RefValues)   'Messung und Rechnung
    GrpRwrtUmrech("X").Add(KeyRe(0), GrpRwerte(0)("N"))
    GrpRwrtUmrech("X").Add(KeyRe(1), GrpRwerte(0)("V"))
    GrpRwrtUmrech("X").Add(KeyRe(2), New RefValue)
    GrpRwrtUmrech("X").Add(KeyRe(3), New RefValue)
    '
    '
    '
    txtBIS.Text = Date.Today.ToLocalTime
    txtVon.Text = Date.Today.AddDays(-MenueParam.Misch.Tdiff).ToLocalTime
    '
    '
    '
    '
    SplitContainRezManag.Hide()
    SplitContainPreview.Hide()
    SplitContainPicture.Hide()
    SplitContainRezSuch.Show()
    '
    'btnlist
    '
    '
    FarbmittelTable.Clear()
    cboFarbmittel.DataSource = Nothing
    SqlStFarb = "SELECT FARBM_ID,FARBM_NAME FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " ORDER BY FARBM_NAME"
    CmdFarbmittel = New OleDbCommand(SqlStFarb, Cndat)
    OleFarbmittel = New OleDbDataAdapter
    OleFarbmittel.SelectCommand = CmdFarbmittel
    If Not FillDatset(OleFarbmittel, FarbmittelTable) Then
      Exit Sub
    End If
    FarbmittelTable.AcceptChanges()
    cboFarbmittel.DataSource = FarbmittelTable
    FarbmittelTable.AcceptChanges()
    cboFarbmittel.ValueMember = "FARBM_ID"
    cboFarbmittel.DisplayMember = "FARBM_NAME"
    'TDBFarDropDown.Width = Weite(1)
    CmdFarbmittel.Dispose()
    OleFarbmittel.Dispose()
    GrpRwerte(0)("V").IVoNa = False
    GrpRwerte(0)("N").IVoNa = False
    GrpRwerte(0)("R").IVoNa = False
    btnRwerteVorlage.Visible = False
    btnRwerteNachstellung.Visible = False
    btnRwerteUntergrund.Visible = False
    lblRwerteVorlage.Visible = False
    lblRwerteNachstellung.Visible = False
    lblRwerteUntergrund.Visible = False
    Call MakeVisible()
    '
    'txtDosis.Text = CStr(MenueParam.Misch.MinDos)
    '
    '
    '
    'Gewichtfaktoren
    '
    lblGewDL.Text = "DL"
    lblGewDC.Text = "DC"
    lblGewDH.Text = "DH"
    '
    '
    txtGewDL.Text = Format(MenueParam.Menue.Lgew, "###.00")
    txtGewDC.Text = Format(MenueParam.Menue.Cgew, "###.00")
    txtGewDH.Text = Format(MenueParam.Menue.Hgew, "###.00")
    For i = 0 To lblGewNL.Count - 1
      lblGewNL(i).Visible = False
      txtGewNL(i).Visible = False
    Next
    For i = 0 To Min(MenueParam.Normfa.Nlz - 1, lblGewNL.Count - 1)
      lblGewNL(i).Text = MenueParam.Normfa(i).NormKenn
      txtGewNL(i).Text = Format(MenueParam.Normfa(i).NormGew, "###.00")
      lblGewNL(i).Visible = True
      txtGewNL(i).Visible = True
    Next
    '
    '
    btnRezepte.PerformClick()
    Cursor = Cursors.Arrow
    'Application.DoEvents()
    '
    '
    'Winkel
    '
    '
    For i = 0 To chkwinkel.Count - 1
      RemoveHandler chkwinkel(i).Click, AddressOf LichtartWinkel_click
      chkwinkel(i).Visible = False
      radwinkel(i).Visible = False
      lblwinkel(i).Visible = False
    Next
    '
    '
    '
    panwinkel.Visible = True
    panLichtart.Visible = True
    For i = 0 To MenueParam.User.Winkel.Km - 1
      chkwinkel(i).Visible = True
      radwinkel(i).Visible = True
      lblwinkel(i).Visible = True
      lblwinkel(i).Text = MenueParam.User.Winkel(i).IhrmBez
      AddHandler chkwinkel(i).Click, AddressOf LichtartWinkel_click
      AddHandler radwinkel(i).Click, AddressOf LichtartWinkel_click

    Next
    '
    '
    'Lichtarten
    '
    '
    For i = 0 To radLichtart.Count - 1
      RemoveHandler radLichtart(i).Click, AddressOf LichtartWinkel_click
      radLichtart(i).Visible = False
      chkLichtart(i).Visible = False
      lblLichtart(i).Visible = False

    Next
    '
    '
    '
    panLichtart.Visible = True
    For i = 0 To MenueParam.Normfa.Nlz - 1
      radLichtart(i).Visible = True
      'chkLichtart(i).Visible = True
      lblLichtart(i).Visible = True
      lblLichtart(i).Text = MenueParam.Normfa(i).NormNama
      AddHandler radLichtart(i).Click, AddressOf LichtartWinkel_click

    Next
    '''
  End Sub
  Sub HideMe()
    AufbauPar.MethID = -1
  End Sub
  Private Sub txtDosis_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtDosis.TextChanged
    'MenueParam.Misch.MinDos = Singl(txtDosis.Text)
  End Sub


  Private Sub btnRezepte_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRezepte.Click
    Dim SQLDat As String
    Dim SQLGid As String
    Dim SqlWith As String
    Dim SqlUser As String
    Dim WithMessg As String
    '
    If MenueParam.MischID = -1 Then
      MsgBox(Texxt(3601))
      Exit Sub
    End If

    SqlWith = ""
    If chkWithTempl.Checked Then
      SqlWith = " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_KWB = 11"
    End If

    SQLDat = ""
    If chkDatum.CheckState = CheckState.Checked Then

      If Not IsDate(txtVon.Text) Then Exit Sub
      '
      '
      SQLDat = " AND (REZEPT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVon.Text)) & " AND " & Sqldati(Date.Parse(txtBIS.Text).AddDays(1.0)) & ")"
    End If
    '
    '
    SqlUser = ""
    If BitWrt(8, MenueParam.User.Writ) Then
      SqlUser = "USER_ID=" & MenueParam.UserID & " AND "
    End If
    '
    '
    If MenueParam.Misch.UserRzpGID = 0 Then
      SQLGid = "REZEPT_IARCH<2 AND "
    Else
      SQLGid = "(REZEPT_IARCH<2 AND REZEPT_GID = " & MenueParam.Misch.UserRzpGID & ") AND "
    End If
    '
    '

    '
    '
    '
    'Suche alle Rezepte für MISCHID, bei denen für alle Farb-/Bindemittel Grunddaten bereitstehen
    '1. Schritt: Liste aller Farb-/Bindemittel für Grunddaten
    '2. Schritt: Alle Rezepte suchen, die Farb-/Bindemittel aus Liste (1.Schritt) enthalten
    '3. Schritt: Alle Rezepte, die nicht in Liste (2. Schritt) enthalten sind und R-Werte(Vorlage kwb=11) enthalten usw. 
    '
    '
    '
    '
    'If BitWrt(27, MenueParam.User.Writ) Then
    WithMessg = "TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    'Else
    'WithMessg = "TBL_GRUND_FARBM.MESSG_ID=" & MenueParam.Messg.MessgID
    'End If
    '
    CmdRezepte.CommandText = "SELECT DISTINCTROW TOP " & MenueParam.Misch.Top & " TBL_REZEPT.REZEPT_ID AS REZEPT_ID,REZEPT_GID,REZEPT_NAME,USER_ID FROM TBL_REZEPT " _
    & " LEFT JOIN TBL_REZEPT_RWERT ON (TBL_REZEPT.REZEPT_ID=TBL_REZEPT_RWERT.REZEPT_ID AND TBL_REZEPT.MISCH_ID=TBL_REZEPT_RWERT.MISCH_ID)" _
    & " WHERE " & SqlUser & SQLGid & " TBL_REZEPT.MISCH_ID=" & MenueParam.MischID & SqlWith & SQLDat & " AND TBL_REZEPT.REZEPT_ID NOT IN (SELECT DISTINCT REZEPT_ID FROM TBL_REZEPT_FARBM " _
    & " WHERE " & SQLGid & " MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID NOT IN " _
    & " (SELECT DISTINCT FARBM_ID FROM TBL_GRUND_FARBM WHERE MISCH_ID=" & MenueParam.MischID _
    & " AND TBL_GRUND_FARBM.GKWRT_ID=" & MenueParam.Misch.GKwrtID & " AND " & WithMessg & "))" & SQLDat
    '

    '
    '
    If txtSuchName.Text <> "" Then
      CmdRezepte.CommandText = CmdRezepte.CommandText & " AND " & StrSelct("REZEPT_NAME", AddHkomE(txtSuchName.Text))
    End If
    CmdRezepte.CommandText = CmdRezepte.CommandText & " ORDER BY REZEPT_DATTIM DESC"
    '
    RezepteTab.Clear()
    Application.DoEvents()
    OleRezepte.SelectCommand = CmdRezepte
    Cursor = Cursors.WaitCursor
    If Not FillDatset(OleRezepte, RezepteTab) Then
      Exit Sub
    End If
    RezepteTab.AcceptChanges()
    Cursor = Cursors.Default
    If RezepteTab.Rows.Count = 0 Then
      MsgBox(Texxt(2953))
      Exit Sub
    End If
    RezepteTab.AcceptChanges()
    lstRezepte.DataSource = RezepteTab
    lstRezepte.DisplayMember = "REZEPT_NAME"
    lstRezepte.ValueMember = "REZEPT_ID"
  End Sub
  '
   
  Private Sub lstRezepte_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstRezepte.Click
    Dim i As Integer
    Dim ier As Integer
    cboFarbmittel.Visible = False
    If RezepteTab.Rows.Count = 0 Then Exit Sub
    Cursor = Cursors.WaitCursor
    chkGEWICHT.Checked = True
    lblZuwaage.Visible = False
    txtZuwaag.Visible = False
    hscZuwaag.Visible = False
    hscZuwaag.Enabled = False
    lblMinMeng.Visible = False
    txtMinMeng.Visible = False
    hscMinMeng.Visible = False
    hscMinMeng.Enabled = False
    btnRezVerw.Enabled = True
    If BitWrt(26, MenueParam.User.Drum) Then
      hscGewNormlicht.Visible = True
    End If
    '
    chkVol.Enabled = False
    If GrpRwerte.RwArt(0) = "W" Then
      GrpRwerte(0)("V").kwb = 0
      GrpRwerte(1)("V").kwb = 1
      GrpRwerte(0)("N").kwb = 0
      GrpRwerte(1)("N").kwb = 1
      GrpRwerte(0)("R").kwb = 0
      GrpRwerte(1)("R").kwb = 1
      GrpRwerte(0).RefUnt.kwb = 0
      GrpRwerte(1).RefUnt.kwb = 1
      RezSozpt.Rezepte.kwb(0) = 0
      RezSozpt.Rezepte.kwb(1) = 1
    Else
      GrpRwerte(0)("V").kwb = 1
      GrpRwerte(1)("V").kwb = 0
      GrpRwerte(0)("N").kwb = 1
      GrpRwerte(1)("N").kwb = 0
      GrpRwerte(0)("R").kwb = 1
      GrpRwerte(1)("R").kwb = 0
      GrpRwerte(0).RefUnt.kwb = 1
      GrpRwerte(1).RefUnt.kwb = 0
      RezSozpt.Rezepte.kwb(0) = 1
      RezSozpt.Rezepte.kwb(1) = 0
    End If
    For i = 0 To 1
      GrpRwerte(i).RefUnt.Name = ""
      GrpRwerte(0)(i).Name = ""
      GrpRwerte(1)(i).Name = ""
    Next i
    PicGraphic.Winkel = MenueParam.User.Winkel
    Call SetPicgraphics()
    PicGraphic.Rmin = 0
    PicGraphic.Rmax = 100
    '
    '
    'Sortiment einlesen
    '
    '
    '
    '
    RzId = RezepteTab.Rows(lstRezepte.SelectedIndex)("REZEPT_ID")
    For i = 0 To 1
      TypID(i) = -1
      UntID(i) = -1
      SmpID(i) = -1
    Next i
    For i = 0 To GrpRwerte.Count - 1
      GrpRwerte(i)("V").IVoNa = False
      GrpRwerte(i)("N").IVoNa = False
      GrpRwerte(i)("R").IVoNa = False
    Next i
    '
    '
    '
    'Einlesen Rezept
    '
    '
    ClearRezepte(RezSozpt)
    RezSozpt.Rezepte(RezAlt).Dicke(0) = MenueParam.Misch.Dicke
    RezSozpt.Rezepte(RezAlt).Dicke(1) = MenueParam.Misch.Dicke
    '
    RwWrRezept.ReadRezeptFarbGrund(RezAlt, RzId, RezSozpt, UntID, TypID, SmpID, ier)
    '
    '
    RezName = RezSozpt.Rezepte(RezAlt).Name
    ReDim RezKey(0)
    RezKey(0) = RezAlt

    Umr.CalcBamng(RezAlt, RezSozpt, ier)
    lblRezept.Visible = True
    lblRezept.Text = RezSozpt.Rezepte(RezAlt).Name
    If RezSozpt.IVOL = 1 Then
      chkVol.CheckState = CheckState.Checked
    Else
      chkVol.CheckState = CheckState.Unchecked
    End If

    GrpRwerte(0).RefUnt.ID = UntID(0)
    GrpRwerte(1).RefUnt.ID = UntID(1)
    GrpRwerte(0)("V").ID = TypID(0)
    GrpRwerte(1)("V").ID = TypID(1)
    GrpRwerte(0)("N").ID = SmpID(0)
    GrpRwerte(1)("N").ID = SmpID(1)
    '
    '
    '
    'Einlesen R-Werte (Untergrund)
    '
    '
    '
    For i = 0 To GrpRwerte.Count - 1
      GrpRwerte(i).RefUnt.IVoNa = False
      If GrpRwerte(i).RefUnt.ID >= 0 Then
        ReWrRwert.ReadRwert(GrpRwerte(i).RefUnt.ID, GrpRwerte(i).RefUnt, ier)
      End If
    Next i
    GrpRwerte(1).RefUnt.IVoNa = False

    '
    '
    '
    'Einlesen R-Werte (Vorlage)
    '
    '
    '
    For i = 0 To GrpRwerte.Count - 1
      GrpRwerte(i)("V").IVoNa = False
      If GrpRwerte(i)("V").ID >= 0 Then
        ReWrRwert.ReadRwert(GrpRwerte(i)("V").ID, GrpRwerte(i)("V"), ier)
      End If
    Next i
    GrpRwerte(1)("V").IVoNa = False

    '
    '
    '
    'Einlesen R-Werte (Nachstellung)
    '
    '
    '
    For i = 0 To GrpRwerte.Count - 1
      GrpRwerte(i)("N").IVoNa = False
      If GrpRwerte(i)("N").ID >= 0 Then
        ReWrRwert.ReadRwert(GrpRwerte(i)("N").ID, GrpRwerte(i)("N"), ier)
      End If
    Next i
    GrpRwerte(1)("N").IVoNa = False
    '
    '
    '
    '
    '
    lblRwerteVorlage.Text = Trim(GrpRwerte(0)("V").Name)
    lblRwerteNachstellung.Text = Trim(GrpRwerte(0)("N").Name)
    lblRwerteUntergrund.Text = Trim(GrpRwerte(0).RefUnt.Name)

    '
    '


    'Application.DoEvents()
    '
    '
    'Plausibilitätsprüfungen für Rezepte
    '
    '
    '
    cboMethIGX.Enabled = BitWrt(14, MenueParam.User.Enabl)
    If MenueParam.Misch.Igx = 3 Or MenueParam.Misch.Igx = 5 Then
      RezSozpt.INO = 1
      RezSozpt.INF = 0
    End If


    If RezSozpt.MngMin < 0.0# Then RezSozpt.MngMin = 0
    If RezSozpt.MngMax < RezSozpt.MngMin Then RezSozpt.MngMax = RezSozpt.MngMin
    If RezSozpt.ProzMin < 0 Then RezSozpt.ProzMin = 0
    If RezSozpt.ProzMax < RezSozpt.ProzMin Then RezSozpt.ProzMax = RezSozpt.ProzMin
    If RezSozpt.Rezepte(RezAlt).Dicke(0) < 0 Then RezSozpt.Rezepte(RezAlt).Dicke(0) = 0
    If RezSozpt.Rezepte(RezAlt).Dicke(1) < 0 Then RezSozpt.Rezepte(RezAlt).Dicke(1) = 0
    If RezSozpt.Rezepte(RezAlt).Dicke(0) <> RezSozpt.Rezepte(RezAlt).Dicke(1) Then
      RezSozpt.Rezepte(RezAlt).Dicke(1) = RezSozpt.Rezepte(RezAlt).Dicke(0)
    End If
    ZuPrz = MenueParam.Misch.Zuwag + 0.0001
    txtMenge.Text = Umr.MngINF("MNG", RezSozpt, ier)
    txtACT.Text = txtMenge.Text
    If RezSozpt.INO = 1 Then
      lblACT.Visible = True
      txtACT.Visible = True
    Else
      lblACT.Visible = False
      txtACT.Visible = False
    End If
    '
    txtMngMin.Text = RezSozpt.MngMin
    txtMngMax.Text = RezSozpt.MngMax
    txtProzMin.Text = RezSozpt.ProzMin
    txtProzMax.Text = RezSozpt.ProzMax
    cboMNG.SelectedIndex = RezSozpt.INO
    cboPROZZae.SelectedIndex = RezSozpt.INP
    cboPROZNen.SelectedIndex = RezSozpt.INQ
    '
    '
    'Schichtdicke
    txtDickeAlt.Text = CStr(RezSozpt.Rezepte(RezAlt).Dicke(0))
    If RzId = RezIDalt Then
      If IsNumeric(DickeNeu) Then
        txtDickeNeu.Text = CSng(DickeNeu)
      Else
        txtDickeNeu.Text = CStr(RezSozpt.Rezepte(RezAlt).Dicke(0))
      End If
    Else
      txtDickeNeu.Text = CStr(RezSozpt.Rezepte(RezAlt).Dicke(0))
      DickeNeu = txtDickeNeu.Text
      RezIDalt = RzId
    End If
    '
    '
    'Glanzgrad
    '
    txtGlzGrd.Text = RezSozpt.Rezepte(RezAlt).GlzGrd
    '
    '
    'Aktuelles Rezept in Grid
    '
    '
    panTDBRezept.Visible = True

    Call GridKorrektur(RezSozpt)
    TDBRezept.AllowUserToAddRows = True
    TDBRezept.ReadOnly = False


    'MsgBox(hscRezepte.Maximum)
    '
    '
    'Picgraphic mit aktuellen Daten versorgen
    '
    '
    '
    '
    'R-Werte zur grafischen Darstellung
    '
    '
    '
    '
    PicGraphic.Text = GrpRwerte(0)("V").Name
    PicGraphic.PlotRwerte.clear()
    PicGraphic.PlotRwerte.Add("V", GrpRwerte(0)("V"))
    PicGraphic.PlotRwerte.Add("N", GrpRwerte(0)("N"))
    PicGraphic.PlotRwerte.Add("R", GrpRwerte(0)("R"))
    '
    '
    '
    'R-Werte zur Berechnung von FarbWerten
    '
    '
    '
    '
    '
    '
    'Rezepte
    '
    '
    '
    '


    'Visible
    '
    '
    btnRwerteVorlage.Visible = True
    btnRwerteNachstellung.Visible = True
    btnRwerteUntergrund.Visible = True
    lblRwerteVorlage.Visible = True
    lblRwerteNachstellung.Visible = True
    lblRwerteUntergrund.Visible = True
    '
    '
    '
    '
    '
    Call MakeVisible()
    radNachst_0.Checked = True

    'If Not GrpRwerte(0)("V").IVoNa Then
    'btnRwerteVorlage.Enabled = True
    'End If
    '
    '
    'Rezeptverwaltung
    '
    '
    '
    '
    Call RezToTab("MNG", RezSozpt, FarbTabelle)
    If BitWrt(30, MenueParam.User.Writ) Then
      For i = 0 To FarbTabelle.Rows.Count - 1
        flgFarbmittel.Rows(i).DefaultCellStyle.BackColor = Color.FromArgb(FarbTabelle.Rows(i)("FARBM_FARBID"))
      Next i
    End If
    '
    '
    '
    '
    '
    OlduserID = RezepteTab.Rows(lstRezepte.SelectedIndex)("USER_ID")
    Cursor = Cursors.Arrow
  End Sub
  Sub MakeVisible()
    Dim i As Integer
    Dim Knlz As Short
    Dim Kwop As Short
    Kwop = PicGraphic.Kwop
    Knlz = PicGraphic.Knlz
    'btnRwerteVorlage.Enabled = False
    lblPicVorlage.Visible = False
    lblPicNachstellung.Visible = False
    lblPicKorrektur.Visible = False
    pictureBoxFarbe.Visible = False
    lblVOR.Visible = False
    lblNach.Visible = False
    lblDEStern.Visible = False
    lblDLStern.Visible = False
    lblDaStern.Visible = False
    lblDbStern.Visible = False
    lblDCStern.Visible = False
    lblDHStern.Visible = False
    txtDTO.Visible = False
    lblDTO.Visible = False

    txtDEv.Visible = False
    txtDLv.Visible = False
    txtDav.Visible = False
    txtDbv.Visible = False
    txtDCv.Visible = False
    txtDHv.Visible = False

    txtDEn.Visible = False
    txtDLn.Visible = False
    txtDan.Visible = False
    txtDbn.Visible = False
    txtDCn.Visible = False
    txtDHn.Visible = False

    lblMeta.Visible = False
    txtMETAv.Visible = False
    txtMETAn.Visible = False

    lblPreis.Visible = False
    txtPreisv.Visible = False
    txtPreisn.Visible = False

    btnKorrektur.Visible = False
    btnKorrSpezial.Visible = False
    btnSpeicher.Visible = False
    btnFarbmetrik.Visible = False
    If GrpRwerte(0)("V").IVoNa Then
      btnDrucken.Visible = True
    Else
      btnDrucken.Visible = False
    End If
    btnUserProg.Visible = False
    If GrpRwerte(0)("V").IVoNa And GrpRwerte(0)("N").IVoNa Then
      btnKorrektur.Visible = True
      btnKorrSpezial.Visible = True
      btnSpeicher.Visible = True
      btnFarbmetrik.Visible = True
      btnDrucken.Visible = True
      If BitWrt(20, MenueParam.User.Drum) Then
        btnUserProg.Visible = True
      End If
      PicGraphic.RzName(0) = "MNG"
      Preis = 0.0
      For i = 0 To RezSozpt.Rezepte(RezAlt).KF - 1
        KeyId = KeyName(RezSozpt.Rezepte(RezAlt)(i).ID)
        Preis = Preis + RezSozpt.Rezepte(RezAlt)(i).BaAmng * RezSozpt.Farben(KeyId).Preis
      Next
      txtPreisv.Text = Format(Preis, "#######.00")

      '
      '
      '
      'R-Werte zur Berechnung von FarbWerten
      '
      '
      '
      PicGraphic.FawrtRwerte(0).clear()
      PicGraphic.FawrtRwerte(1).clear()
      PicGraphic.FawrtRwerte(0).Add("V", GrpRwerte(0)("V"))
      PicGraphic.FawrtRwerte(0).Add("N", GrpRwerte(0)("N"))
      If GrpRwerte(0)("R").IVoNa Then
        PicGraphic.FawrtRwerte(0).Add("R", GrpRwerte(0)("R"))
        PicGraphic.RzName(0) = "KOR"
        PicGraphic.RzName(1) = "MNG"
        Preis = 0.0
        For i = 0 To Min(RezSozpt.Rezepte("KOR").KF, RezSozpt.Rezepte(RezAlt).KF) - 1
          If RezSozpt.Rezepte("KOR")(i).ID = RezSozpt.Rezepte(RezAlt)(i).ID Then
            KeyId = KeyName(RezSozpt.Rezepte("KOR")(i).ID)
            Preis = Preis + RezSozpt.Rezepte("KOR")(i).BaAmng * RezSozpt.Farben(KeyId).Preis
          End If
        Next
        txtPreisn.Text = Format(Preis, "#######.00")
        lblPicKorrektur.Visible = True
      End If


      '
      '
      '
      '
      'Farbmetrik 
      '
      '
      '
      lblDEStern.Visible = True
      lblDLStern.Visible = True
      lblDaStern.Visible = True
      lblDbStern.Visible = True
      lblDCStern.Visible = True
      lblDHStern.Visible = True

      txtDEv.Visible = True
      txtDLv.Visible = True
      txtDav.Visible = True
      txtDbv.Visible = True
      txtDCv.Visible = True
      txtDHv.Visible = True


      lblMeta.Visible = True
      txtMETAv.Visible = True

      lblPreis.Visible = True
      txtPreisv.Visible = True
      '
      '
      '
      If MenueParam.Misch.Igx = 5 Or MenueParam.Misch.Igx = 6 Then
        txtDTO.Visible = True
        lblDTO.Visible = True
      Else
      End If
      pictureBoxFarbe.Visible = True
      lblPicVorlage.Visible = True
      lblPicNachstellung.Visible = True
      PicGraphic.CalcFarbWrt()
      pictureBoxFarbe.Refresh()
      If PicGraphic.Farbwerte.Count > 0 AndAlso PicGraphic.Farbwerte(0).Count > 0 Then
        lblVOR.Visible = True
        txtDEv.Text = PicGraphic.Farbwerte(0)(1)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(15))
        txtDLv.Text = PicGraphic.Farbwerte(0)(1)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(16))
        txtDav.Text = PicGraphic.Farbwerte(0)(1)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(19))
        txtDbv.Text = PicGraphic.Farbwerte(0)(1)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(20))
        txtDCv.Text = PicGraphic.Farbwerte(0)(1)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(17))
        txtDHv.Text = PicGraphic.Farbwerte(0)(1)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(18))
        txtMETAv.Text = PicGraphic.Farbwerte(0)(1)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(14))
        If PicGraphic.Farbwerte(0).Count = 3 AndAlso PicGraphic.FawrtRwerte(0)("R").IVoNa Then
          lblNach.Visible = True
          txtDEn.Visible = True
          txtDLn.Visible = True
          txtDan.Visible = True
          txtDbn.Visible = True
          txtDCn.Visible = True
          txtDHn.Visible = True
          txtMETAn.Visible = True
          txtPreisn.Visible = True
          txtDEn.Text = PicGraphic.Farbwerte(0)(2)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(15))
          txtDLn.Text = PicGraphic.Farbwerte(0)(2)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(16))
          txtDan.Text = PicGraphic.Farbwerte(0)(2)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(19))
          txtDbn.Text = PicGraphic.Farbwerte(0)(2)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(20))
          txtDCn.Text = PicGraphic.Farbwerte(0)(2)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(17))
          txtDHn.Text = PicGraphic.Farbwerte(0)(2)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(18))
          txtMETAn.Text = PicGraphic.Farbwerte(0)(2)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(14))

        End If
      End If

    End If
  End Sub
  Sub LichtArtText()
    Dim i As Integer
    Dim Ilicht As Integer
    For i = 0 To radLichtart.Count - 1
      If radLichtart(i).Checked Then
        Ilicht = i
        Exit For
      End If
    Next
    'lblDEStern.Text = "DE* (" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    'lblDLStern.Text = "DL* (" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    'lblDaStern.Text = "Da* (" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    'lblDbStern.Text = "Db* (" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    'lblDCStern.Text = "DC* (" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    'lblDHStern.Text = "DH* (" & MenueParam.Normfa(Ilicht).NormKenn & ")"


    lblDEStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(15)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    lblDLStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(16)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    lblDaStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(19)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    lblDbStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(20)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    lblDCStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(17)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    lblDHStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(18)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"

  End Sub
  Sub GridKorrektur(ByRef RezSozpt As RecipesGrp)
    Dim j As Integer
    Dim SumA As Single
    Dim SumN As Single
    Dim SumZ As Single
    Dim KeyId As String
    Dim Height As Integer
    Height = 20

    RezTab.Rows.Clear()


    SumA = 0.0
    SumN = 0.0
    SumZ = 0.0
    For j = 0 To RezSozpt.Rezepte("MNG").KF - 1
      KeyId = KeyName(RezSozpt.Rezepte("MNG")(j).ID)
      RezTab.Rows.Add(RezTab.NewRow)
      RezTab.Rows(j)(0) = RezSozpt.Farben(KeyId).ID
      RezTab.Rows(j)(1) = Trim(RezSozpt.Farben(KeyId).Name)
      RezTab.Rows(j)(2) = Format(RezSozpt.Rezepte("MNG")(j).BaAmng, RezSozpt.Farben(KeyId).Form)
      SumA = SumA + RezSozpt.Rezepte("MNG")(j).BaAmng
      RezTab.Rows(j)(5) = Format(RezSozpt.Farben(KeyId).Fst, "##0.00")
      RezTab.Rows(j)(6) = Format(RezSozpt.Rezepte("MNG")(j).Proz, "##0.00")
      RezTab.Rows(j)(7) = Format(RezSozpt.Rezepte("MNG")(j).Prob, "##0.00")
      If j < RezSozpt.Rezepte("KOR").KF Then
        RezTab.Rows(j)(3) = Format(RezSozpt.Rezepte("KOR")(j).BaAmng, RezSozpt.Farben(KeyId).Form)
        RezTab.Rows(j)(4) = Format(RezSozpt.Rezepte("ZUW")(j).BaAmng, RezSozpt.Farben(KeyId).Form)
        SumN = SumN + RezSozpt.Rezepte("KOR")(j).BaAmng
        SumZ = SumZ + RezSozpt.Rezepte("ZUW")(j).BaAmng
      End If
    Next
   
    txtSUM(1).Text = Texxt(825)
    txtSUM(2).Text = Format(SumA, "#####.00")
    '
    '
    If RezSozpt.Rezepte("KOR").KF > 0 Then
      txtSUM(3).Text = Format(SumN, "#####.00")
      txtSUM(4).Text = Format(SumZ, "#####.00")
    Else
      txtSUM(3).Visible = False
      txtSUM(4).Visible = False
    End If
    RezTab.AcceptChanges()
    '
    '
    '
    TDBRezept.Enabled = False
    For j = 0 To RezSozpt.Rezepte("MNG").KF - 1
      TDBRezept.Rows(j).Height = Height
    Next j
    If RezSozpt.Rezepte("KOR").KF > 0 Then
      TDBRezept.Columns(3).Visible = True
      TDBRezept.Columns(4).Visible = True
      txtSUM(3).Visible = True
      txtSUM(4).Visible = True
      TDBRezept.AllowUserToDeleteRows = False
    Else
      TDBRezept.Columns(3).Visible = False
      TDBRezept.Columns(4).Visible = False
    End If
    TDBRezept.Columns(1).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill

    '
    '
    'TDBRezept.AllowUserToAddRows = False
    TDBRezept.AllowUserToDeleteRows = True
    TDBRezept.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells
    TDBRezept.ScrollBars = ScrollBars.Vertical
    TDBRezept.BorderStyle = BorderStyle.None
    TDBRezept.RowHeadersVisible = True
    TDBRezept.RowHeadersWidth = 20
    TDBRezept.Columns(0).Width = 0
    TDBRezept.Columns(0).Visible = False
    TDBRezept.Columns(1).HeaderText = Texxt(394)
    TDBRezept.Columns(2).HeaderText = Texxt(4642)
    TDBRezept.Columns(3).HeaderText = Texxt(4643)
    TDBRezept.Columns(4).HeaderText = Texxt(827)
    TDBRezept.Columns(5).HeaderText = TexKt(21014)
    TDBRezept.Columns(6).HeaderText = Texxt(820)
    TDBRezept.Columns(7).HeaderText = Texxt(821)


    TDBRezept.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(6).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(7).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight

    TDBRezept.Columns(1).HeaderCell.Style.Alignment = TDBRezept.Columns(1).DefaultCellStyle.Alignment
    TDBRezept.Columns(2).HeaderCell.Style.Alignment = TDBRezept.Columns(2).DefaultCellStyle.Alignment
    TDBRezept.Columns(3).HeaderCell.Style.Alignment = TDBRezept.Columns(3).DefaultCellStyle.Alignment
    TDBRezept.Columns(4).HeaderCell.Style.Alignment = TDBRezept.Columns(4).DefaultCellStyle.Alignment
    TDBRezept.Columns(5).HeaderCell.Style.Alignment = TDBRezept.Columns(4).DefaultCellStyle.Alignment
    TDBRezept.Columns(6).HeaderCell.Style.Alignment = TDBRezept.Columns(4).DefaultCellStyle.Alignment
    TDBRezept.Columns(7).HeaderCell.Style.Alignment = TDBRezept.Columns(4).DefaultCellStyle.Alignment

    TDBRezept.Columns(1).SortMode = SortOrder.None
    TDBRezept.Columns(2).SortMode = SortOrder.None
    TDBRezept.Columns(3).SortMode = SortOrder.None
    TDBRezept.Columns(4).SortMode = SortOrder.None
    TDBRezept.Columns(5).SortMode = SortOrder.None
    TDBRezept.Columns(6).SortMode = SortOrder.None
    TDBRezept.Columns(7).SortMode = SortOrder.None
    TDBRezept.Columns(1).ReadOnly = True
    TDBRezept.Columns(6).ReadOnly = True
    TDBRezept.Columns(5).ReadOnly = True
    TDBRezept.Columns(7).ReadOnly = True

    TDBRezept.Columns(7).Visible = False
    TDBRezept.Columns(5).DefaultCellStyle.Format = "##0.000"
    TDBRezept.Columns(6).DefaultCellStyle.Format = "##0.000"
    TDBRezept.Columns(7).DefaultCellStyle.Format = "##0.000"

    '
    '
    '
    Call NewColumnsSize()
    TDBRezept.Enabled = True
    TDBRezept.FirstDisplayedScrollingRowIndex = 0
    Cncol.Close()

    If BitWrt(30, MenueParam.User.Writ) Then
      For j = 0 To RezSozpt.Rezepte("MNG").KF - 1
        KeyId = KeyName(RezSozpt.Rezepte("MNG")(j).ID)
        TDBRezept.Rows(j).DefaultCellStyle.BackColor = Color.FromArgb(RezSozpt.Farben(KeyId).FarbID)
      Next j
    End If
  End Sub

  Private Sub NewColumnsSize()
   
    If IsNothing(RezSozpt) Then Exit Sub
    If RezSozpt.Rezepte("KOR").KF > 0 Then
      TDBRezept.Columns(1).Width = TDBRezept.Width * 0.55
      TDBRezept.Columns(2).Width = TDBRezept.Width * 0.125
      TDBRezept.Columns(3).Width = TDBRezept.Width * 0.125
      TDBRezept.Columns(4).Width = TDBRezept.Width * 0.125
    Else
      TDBRezept.Columns(1).Width = TDBRezept.Width * 0.55
      TDBRezept.Columns(2).Width = TDBRezept.Width * 0.2
    End If
    TDBRezept.Columns(5).Width = TDBRezept.Width * 0.125
    TDBRezept.Columns(6).Width = TDBRezept.Width * 0.125
    TDBRezept.Columns(7).Width = TDBRezept.Width * 0.0
    

  End Sub
  Sub CalcKorrRezept()
    Dim ier As Integer
    Dim i As Integer
    cboFarbmittel.Visible = False
    '
    'Gewichtsfaktoren
    '
    MenueParam.Menue.Lgew = CSng(txtGewDL.Text)
    MenueParam.Menue.Cgew = CSng(txtGewDC.Text)
    MenueParam.Menue.Hgew = CSng(txtGewDH.Text)
    For i = 0 To Min(MenueParam.Normfa.Nlz - 1, lblGewNL.Count - 1)
      MenueParam.Normfa(i).NormGew = CSng(txtGewNL(i).Text)
    Next
    '
    '
    '
    '
    cboMethIGX.Enabled = False
    If MenueParam.Misch.Igx = 3 Or MenueParam.Misch.Igx = 5 Then
      MngMinAlt = RezSozpt.MngMin
      MngMaxAlt = RezSozpt.MngMax
      RezSozpt.MngMin = Umr.MngINF("MNG", RezSozpt, ier)
      RezSozpt.MngMax = (1.0 + ZuPrz) * RezSozpt.MngMin
    End If
    If MenueParam.Misch.Igx = 4 Or MenueParam.Misch.Igx = 6 Then
      If MenueParam.Misch.MGGE = 0 Then
        MenueParam.Misch.Gge = 0
      Else
        MenueParam.Misch.Gge = GGEMax
      End If
    End If
    If MenueParam.Misch.Igx = 5 Or MenueParam.Misch.Igx = 6 Then
      MenueParam.Misch.Dto = Singl(txtDTO.Text)
    End If
    RezSozpt.Rezepte(RezKor).Dicke(0) = Singl(txtDickeNeu.Text)
    RezSozpt.Rezepte(RezKor).Dicke(1) = RezSozpt.Rezepte(RezKor).Dicke(0)
    '
    'Warnungsmeldung zurücksetzen
    '
    MenueParam.User.Writ = BitIntID(Warn, 28, 29, MenueParam.User.Writ)
    KFIT = 1
    If chkRwertAngleich.Checked Then
      KFIT = 2
    End If
    CalcRezept.KorrekturRezept(KFIT + 8, MenueParam.User.Winkel, RezAlt, RezKor, RezSozpt, GrpRwerte, ier)
    '
    'Warnungen unterdrücken
    '
    MenueParam.User.Writ = BitIntID(0, 28, 29, MenueParam.User.Writ)
    If ier <> 0 Then
      Cursor = Cursors.Arrow
      Exit Sub
    End If
    '
    '
    '
    '
    'Aktuelles Rezept in Grid
    '
    Call GridKorrektur(RezSozpt)
    TDBRezept.AllowUserToAddRows = False
    TDBRezept.ReadOnly = True
    '
    '
    '
    '
    'R-Werte zur Berechnung von FarbWerten
    '
    '
    '
    '
    PicGraphic.Rmax = -1.0
    PicGraphic.Rmin = -1.0

    Call MakeVisible()
    Call NewColumnsSize()
    pictureBoxFarbe.Refresh()
    ReDim RezKey(3)
    RezKey(0) = "MNG"
    RezKey(1) = "KOR"
    RezKey(2) = "MZU"
    RezKey(3) = "ZUW"

    '
    '
    'Zuwaagenminimierung
    '
    '
    '
    '
    '

    If MenueParam.Misch.Igx = 3 Or MenueParam.Misch.Igx = 5 Then
      hscZuwaag.Maximum = 30000
      hscZuwaag.Minimum = 0

      HscValue = hscZuwaag.Maximum * Umr.MngINF("ZUW", RezSozpt, ier) / (RezSozpt.MngMax - RezSozpt.MngMin)
      If HscValue >= hscZuwaag.Minimum And HscValue <= hscZuwaag.Maximum Then
        hscZuwaag.Value = HscValue
      Else
        hscZuwaag.Value = hscZuwaag.Maximum
      End If
      txtZuwaag.Visible = True
      txtZuwaag.Text = Format(ZuPrz * 100.0, "####.00")
      hscZuwaag.Visible = True
      hscZuwaag.Enabled = True
      lblZuwaage.Visible = True
    End If

    '
    '
    '
    If MenueParam.Misch.Igx = 3 Or MenueParam.Misch.Igx = 5 Then
      ZuMax = Umr.MngINF("ZUW", RezSozpt, ier)
      'If ZuMax > ZuPrz * RezSozpt.MngMin - 0.001 - MenueParam.Misch.MinDos Then
      ' ZuPrz = 2 * ZuPrz
      'End If
      If ZuMax < 0.01 * RezSozpt.MngMin Then
        ZuMax = 0.01 * RezSozpt.MngMin
      End If
    End If
    btnRezVerw.Enabled = False
    '
    '
    'Mengenminimierung
    '
    '
    '
    '
    '
    If MenueParam.Misch.Igx = 1 Or MenueParam.Misch.Igx = 2 Or MenueParam.Misch.Igx = 4 Or MenueParam.Misch.Igx = 6 Then
      hscMinMeng.Maximum = 30000
      hscMinMeng.Minimum = 0


      If MenueParam.Misch.MGGE = 0 Then
        HscValue = hscMinMeng.Minimum
      Else
        HscValue = hscMinMeng.Maximum
      End If



      If HscValue >= hscMinMeng.Minimum And HscValue <= hscMinMeng.Maximum Then
        hscMinMeng.Value = HscValue
      Else
        hscMinMeng.Value = hscMinMeng.Maximum
      End If
      txtMinMeng.Visible = True
      txtMinMeng.Text = Format(GGEMax, "####.00")
      hscMinMeng.Visible = True
      hscMinMeng.Enabled = True
      lblMinMeng.Visible = True
    End If
    txtMenge.Enabled = True
    chkGEWICHT.Checked = False
    If BitWrt(20, MenueParam.User.Drum) Then
      btnUserProg.Visible = True
    End If
  End Sub
  Private Sub PictureBoxfarbe_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles pictureBoxFarbe.Paint
    If IsNothing(PicGraphic) Then Exit Sub
    Try
      e.Graphics.Clear(Color.White)
      PicGraphic.CalcFarbWrt()
      PicGraphic.GraphBounds = New RectangleF(0.0 * sender.width, 0.0 * sender.height, 1.0 * sender.width, 1.0 * sender.height)
      PicGraphic.PicXYZGraph(sender, e.Graphics)
    Finally
    End Try
  End Sub



  Private Sub btnKorrektur_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnKorrektur.Click
    RezSozpt.Rezepte(RezKor).GlzGrd = txtGlzGrd.Text
    If BitWrt(22, MenueParam.User.Sonst) Then
      Cursor = Cursors.WaitCursor
      For i = 0 To RezSozpt.Farben.FarbCount - 1
        Call HandleRezept.GrundGlzGrd(MenueParam.MischID, RezSozpt.Farben(i).ID, RezSozpt.Farben(i), RezSozpt.Rezepte(RezKor).GlzGrd, ier)
      Next i
      Cursor = Cursors.Default
    End If

    hscGewNormlicht.Value = hscGewNormlicht.Maximum
    Call hscGewNormlicht_Scroll(hscGewNormlicht, New System.Windows.Forms.ScrollEventArgs(ScrollEventType.EndScroll, hscGewNormlicht.Maximum))
  End Sub

  Private Sub hscZuwaag_Scroll(ByVal sender As Object, ByVal e As System.Windows.Forms.ScrollEventArgs) Handles hscZuwaag.Scroll
    Dim i As Integer
    Dim ZuAkt As Single
    Dim ZuMngMax As Single
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    If hscZuwaag.Value < 0 Then Exit Sub
    If Not hscZuwaag.Enabled Then Exit Sub

    If hscZuwaag.Value = HscValue Then Exit Sub
    If hscZuwaag.Value > HscValue And MenueParam.Misch.Igx = 3 Then
      hscZuwaag.Value = HscValue
    End If
    txtZuwaag.Enabled = False
    Cursor = Cursors.WaitCursor
    ZuMngMax = RezSozpt.MngMax
    If MenueParam.Misch.Igx = 3 Or MenueParam.Misch.Igx = 5 Then
      ZuAkt = (hscZuwaag.Value - hscZuwaag.Minimum) / (hscZuwaag.Maximum - hscZuwaag.Minimum) * ZuPrz * RezSozpt.MngMin
      RezSozpt.MngMax = RezSozpt.MngMin + ZuAkt
    End If
    KFIT = 1
    If chkRwertAngleich.Checked Then
      KFIT = 2
    End If
    CalcRezept.KorrekturRezept(KFIT, MenueParam.User.Winkel, "MNG", "KOR", RezSozpt, GrpRwerte, ier)
    txtZuwaag.Enabled = True
    If ier <> 0 Then
      Cursor = Cursors.Arrow
      Exit Sub
    End If
    RezSozpt.MngMax = ZuMngMax
    '
    Call MakeVisible()
    '
    '
    Call GridKorrektur(RezSozpt)

    pictureBoxFarbe.Refresh()

    Cursor = Cursors.Arrow
  End Sub





  Private Sub btnFarbmetrik_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnFarbmetrik.Click
    SplitContainRezManag.Hide()
    SplitContainPreview.Hide()
    SplitContainRezSuch.Hide()
    picAuf.Refresh()

    SplitContainPicture.Show()
  End Sub

  '
  '
  '
  '
  Private Sub SplitContainPicture_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles SplitContainPicture.Resize
    sender.refresh()
    Exit Sub
  End Sub
  Private Sub btnPictureDrucken_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnPictureDrucken.Click
    'FormShow.ShowDialog()
    'RezDruck.Text0 = FormShow.txtEIN_0.Text
    'RezDruck.Text1 = FormShow.txtEIN_1.Text
    'RezDruck.Text2 = FormShow.txtEIN_2.Text
    RezDruck.KeyNam = RezAlt
    RezDruck.RzName(0) = RezSozpt.Rezepte(RezAlt).Name
    printdoc = New PrintDocument
    printdoc.PrinterSettings = pd.PrinterSettings
    ppv.Document = printdoc
    AddHandler printdoc.PrintPage, AddressOf RezDruck.DruckPictureboxes
    printdoc.DefaultPageSettings.Landscape = True
    ppv.WindowState = FormWindowState.Maximized
    'Preview anzeigen 
    ppv.ShowDialog()

    RemoveHandler printdoc.PrintPage, AddressOf RezDruck.DruckPictureboxes
    printdoc.Dispose()
  End Sub

  Private Sub btnPictureZurück_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnPictureZurück.Click
    SplitContainRezManag.Hide()
    SplitContainPreview.Hide()
    SplitContainPicture.Hide()
    SplitContainRezSuch.Show()
  End Sub





  Private Sub btnRwerteVorlage_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRwerteVorlage.Click
    Dim Dialo As DialogResult
    Dim i As Integer
    '
    'Vorlage
    '
    '
    GrpRwerte(0)("V").IVoNa = False
    GetRwerte.Messrefel = GrpRwerte(0)("V")
    Me.Enabled = False
    GetRwerte.Iarch = 0
    GetRwerte.Captext = Texxt(861)
    Call GetRwerte.ReflexWerte(Dialo)

    Me.Enabled = True
    If Dialo = DialogResult.OK Then
      lblRwerteVorlage.Text = GrpRwerte(0)("V").Name
      Call MakeVisible()
      For i = 0 To 1
        TypID(i) = GrpRwerte(i)("V").ID
        UntID(i) = GrpRwerte(i).RefUnt.ID
        SmpID(i) = GrpRwerte(i)("N").ID
      Next
      RzId = RezSozpt.Rezepte("MNG").ID
      Call ReWrRezept.UpdateRezept("MNG", RzId, RezSozpt, UntID, TypID, SmpID, ier)
    End If
    '
  End Sub
  Private Sub btnNachstellung_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRwerteNachstellung.Click
    Dim Dialo As DialogResult
    Dim i As Integer
    '
    'Naschstellung
    '
    '
    GrpRwerte(0)("N").IVoNa = False
    GetRwerte.Messrefel = GrpRwerte(0)("N")
    Me.Enabled = False
    GetRwerte.Iarch = 0
    GetRwerte.Captext = Texxt(786)
    Call GetRwerte.ReflexWerte(Dialo)

    Me.Enabled = True
    If Dialo = DialogResult.OK Then
      lblRwerteNachstellung.Text = GrpRwerte(0)("N").Name
      GrpRwerte(0)("R").IVoNa = False
      lblPicKorrektur.Visible = False
      Call MakeVisible()
      For i = 0 To 1
        TypID(i) = GrpRwerte(i)("V").ID
        UntID(i) = GrpRwerte(i).RefUnt.ID
        SmpID(i) = GrpRwerte(i)("N").ID
      Next
      RzId = RezSozpt.Rezepte("MNG").ID
      Call ReWrRezept.UpdateRezept("MNG", RzId, RezSozpt, UntID, TypID, SmpID, ier)

    End If
    pictureBoxFarbe.Refresh()

  End Sub
  Private Sub btnSpeicher_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSpeicher.Click
    Dim i As Integer
    If RezSozpt.Rezepte("KOR").KF > 0 Then
      RzNr = "KOR"
    Else
      RzNr = "MNG"
    End If
    RezName = RezSozpt.Rezepte(RzNr).Name
    If RezName = "" Then
      RezName = RezSozpt.Rezepte("MNG").Name
    End If
    
    Call RezAbsp(RzNr, OlduserID, ier)
    btnRezepte.PerformClick()
  End Sub







  Private Sub chkDatum_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkDatum.CheckStateChanged
    If chkDatum.CheckState = CheckState.Checked Then
      txtVon.Visible = True
      txtBIS.Visible = True
      lblBIS.Visible = True
    Else
      txtVon.Visible = False
      txtBIS.Visible = False
      lblBIS.Visible = False
    End If
  End Sub

  Private Sub TDBRezept_ColumnWidthChanged(sender As Object, e As System.Windows.Forms.DataGridViewColumnEventArgs) Handles TDBRezept.ColumnWidthChanged
    Dim i As Integer
    Dim WholeWidth As Integer
    WholeWidth = TDBRezept.RowHeadersWidth
    For i = 1 To txtSUM.Count - 1
      txtSUM(i).Width = TDBRezept.Columns(i).Width
      txtSUM(i).Left = WholeWidth
      WholeWidth = WholeWidth + TDBRezept.Columns(i).Width
    Next
  End Sub


  Private Sub TDBRezept_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles TDBRezept.Resize
    Call NewColumnsSize()

  End Sub


  Public Sub New()

    ' This call is required by the Windows Form Designer.
    InitializeComponent()

    ' Add any initialization after the InitializeComponent() call.

  End Sub
  Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
    If disposing Then
      If Not (components Is Nothing) Then
        components.Dispose()
      End If
    End If

    MyBase.Dispose(disposing)

  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()

  End Sub

  Private Sub btnDrucken_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnDrucken.Click, btnDruckenZus.Click
    RezName = InputBox("", Texxt(4635), RezName)
    If RezName <> "" Then
      RezSozpt.Rezepte("MNG").Name = RezName
      SplitContainRezManag.Hide()
      SplitContainRezSuch.Hide()
      SplitContainPicture.Hide()





      PrintPrev.Show()



      Dim Printdoc As New PrintDocument
      Printdoc.DocumentName = RezName
      Printdoc.PrinterSettings = pd.PrinterSettings
      ppv.Document = Printdoc
      '
      '
      '
      '
      '
      '
      '
      'Wiegeschein
      '
      RezDruck.Clear()
      RezDruck.KeyNam = "MNG"
      Erase RezDruck.RzName
      ReDim RezDruck.RzName(0)
      RezDruck.RzName(0) = "MNG"
      If RezSozpt.Rezepte("KOR").KF > 0 Then
        ReDim Preserve RezDruck.RzName(3)
        RezDruck.RzName(1) = "KOR"
        RezDruck.RzName(2) = "MZU"
        RezDruck.RzName(3) = "ZUW"
      End If
      'If RezSozpt.Rezepte("KOR").Name = "" Then
      ' RezDruck.RzName(1) = GrpRwerte(0)("N").Name
      ' Else
      ' RezDruck.RzName(1) = RezSozpt.Rezepte("KOR").Name
      ' End If
      '
      '
      Erase RezDruck.RefNr
      ReDim RezDruck.RefNr(0)
      RezDruck.RefNr(0) = "V"
      If GrpRwerte(0)("N").IVoNa Then
        ReDim Preserve RezDruck.RefNr(1)
        RezDruck.RefNr(1) = "N"
      End If

      RezDruck.ZusatzName.Clear()
      RezDruck.ZusatzMenge.Clear()
      RezDruck.InformationsName.Clear()
      RezDruck.InformationsBez.Clear()

      If sender.name = "btnDruckenZus" Then
        For i = 0 To flgZusatzstoffe.Rows.Count - 2
          RezDruck.ZusatzName.Add(flgZusatzstoffe.Rows(i).Cells("NAME").Value)
          RezDruck.ZusatzMenge.Add(flgZusatzstoffe.Rows(i).Cells("MENGE").Value)
        Next i
        For i = 0 To flgInformationen.Rows.Count - 2
          RezDruck.InformationsName.Add(flgInformationen.Rows(i).Cells("NAME").Value)
          RezDruck.InformationsBez.Add(flgInformationen.Rows(i).Cells("BEZ").Value)
        Next i
      Else
        If GrpRwerte(0)("R").IVoNa Then
          ReDim Preserve RezDruck.RefNr(2)
          RezDruck.RefNr(2) = "R"
        End If
      End If
      '
      '
      '
      RezAusgabe.pldr = RezDruck
      AddHandler Printdoc.PrintPage, AddressOf RezAusgabe.MankDruckKorrekturRezept
      Printdoc.DefaultPageSettings.Landscape = False
      ppv.WindowState = FormWindowState.Maximized
      'Preview anzeigen
      ppv.ShowDialog()

      RemoveHandler Printdoc.PrintPage, AddressOf RezAusgabe.MankDruckKorrekturRezept
      Printdoc.Dispose()
      SplitContainRezSuch.Show()
    End If
  End Sub
  Private Sub txtMenge_Keypress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles txtMenge.KeyPress
    Dim ivol As Integer
    Dim NewMenge As Single
    If Asc(e.KeyChar) = 13 Then
      ivol = 0
      If chkVol.CheckState = CheckState.Checked Then
        ivol = 1
      End If
      NewMenge = Singl(txtMenge.Text)
      Call RezeptNewMenge(ivol, NewMenge, Umr, RezKey, RezSozpt, ier)
      Call GridKorrektur(RezSozpt)
    End If
  End Sub


  Private Sub txtMenge_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtMenge.Leave
    Dim ee As New System.Windows.Forms.KeyPressEventArgs(Chr(13))
    Call txtMenge_Keypress(sender, ee)
  End Sub



  Private Sub cboFarbmittel_MouseLeave(sender As Object, e As System.EventArgs) Handles cboFarbmittel.MouseLeave
    cboFarbmittel.Visible = False

  End Sub




  Private Sub cboFarbmittel_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboFarbmittel.SelectedIndexChanged
    Dim FarbID As Integer
    Dim i As Integer
    Dim ier As Integer
    If cboFarbmittel.Enabled = False Then Exit Sub
    If IsNothing(TDBRezept.CurrentCell) Then Exit Sub
    FarbID = cboFarbmittel.SelectedValue
    For i = 0 To RezSozpt.Farben.FarbCount - 1
      If FarbID = RezSozpt.Farben(i).ID Then Exit For
    Next
    If i >= RezSozpt.Farben.FarbCount Then
      RezSozpt.Farben.AddFarb(KeyName(FarbID), New Colorant)
      Call ReWrFarbe.FarReaGrund(FarbID, RezSozpt.Farben(RezSozpt.Farben.FarbCount - 1), False, ier)
    End If
    For i = 0 To RezSozpt.Rezepte("MNG").KF - 1
      If FarbID = RezSozpt.Rezepte("MNG")(i).ID Then Exit For
    Next
    If i >= RezSozpt.Rezepte("MNG").KF Then
      RezSozpt.Rezepte("MNG").AddFaNr(KeyRe(RezSozpt.Rezepte("MNG").KF), New ColorAmount)
      RezSozpt.Rezepte("MNG")(RezSozpt.Rezepte("MNG").KF - 1).ID = FarbID
      RezSozpt.Rezepte("MNG")(RezSozpt.Rezepte("MNG").KF - 1).BaAmng = 0.0
      RezSozpt.Rezepte("MNG")(RezSozpt.Rezepte("MNG").KF - 1).FaAmng = 0.0
      RezSozpt.Rezepte("MNG")(RezSozpt.Rezepte("MNG").KF - 1).Proz = RezSozpt.Farben(KeyName(FarbID)).Prf(0)
      RezSozpt.Rezepte("MNG")(RezSozpt.Rezepte("MNG").KF - 1).Prob = RezSozpt.Farben(KeyName(FarbID)).Prb(0)
      RezSozpt.Farben(KeyName(FarbID)).Preis = RezSozpt.Farben(KeyName(FarbID)).Pre(0)
      Call GridKorrektur(RezSozpt)
    End If
    cboFarbmittel.Enabled = False
    cboFarbmittel.Visible = False

  End Sub



  Private Sub TDBRezept_CellEndEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles TDBRezept.CellEndEdit
    Dim i As Integer
    Dim NewMenge As Single
    Dim OldMenge As Single
    Dim Fakt As Single
    Dim ivol As Integer
    ivol = RezSozpt.IVOL
    If IsDBNull(RezTab.Rows(e.RowIndex)(e.ColumnIndex)) Then
      RezTab.Rows(e.RowIndex)(e.ColumnIndex) = 0.0
      Exit Sub
    End If


    If e.ColumnIndex = 2 And RezTab.Rows.Count > 0 Then
      If e.RowIndex = RezSozpt.Rezepte("MNG").KF Then

      ElseIf e.RowIndex >= 0 And e.RowIndex < RezSozpt.Rezepte("MNG").KF Then
        OldMenge = Umr.MngINO("MNG", RezSozpt, ier)
        RezSozpt.Rezepte("MNG")(e.RowIndex).BaAmng = RezTab.Rows(e.RowIndex)("GEWICHT")
        Call Umr.CalcFamng("MNG", RezSozpt, ier)
        NewMenge = Umr.MngINO("MNG", RezSozpt, ier)
        Fakt = NewMenge / OldMenge
        RezSozpt.MngMax = Fakt * RezSozpt.MngMax
        RezSozpt.MngMin = Fakt * RezSozpt.MngMin
        For i = 0 To FarbTabelle.Rows.Count - 1
          RezSozpt.Farben(KeyName(FarbTabelle.Rows(i)("FARBM_ID"))).BoMng = Fakt * RezSozpt.Farben(KeyName(FarbTabelle.Rows(i)("FARBM_ID"))).BoMng
        Next i
      End If
      txtMenge.Text = Umr.MngINF("MNG", RezSozpt, ier)
      txtMngMax.Text = CStr(RezSozpt.MngMax)
      txtMngMin.Text = CStr(RezSozpt.MngMin)
      For i = 0 To FarbTabelle.Rows.Count - 1
        FarbTabelle.Rows(i)("FARBM_LIMMNG") = RezSozpt.Farben(KeyName(FarbTabelle.Rows(i)("FARBM_ID"))).BoMng
      Next i

      Call GridKorrektur(RezSozpt)
    End If

  End Sub






  Private Sub TDBRezept_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles TDBRezept.Click
    Dim X As Integer
    Dim Y As Integer
    Dim Wi As Integer
    If RezSozpt.Rezepte("KOR").KF <> 0 Then Exit Sub
    If IsNothing(TDBRezept.CurrentCell) Then Exit Sub
    If TDBRezept.CurrentCell.RowIndex = RezSozpt.Rezepte("MNG").KF And TDBRezept.CurrentCell.ColumnIndex = 1 Then
      cboFarbmittel.Visible = True
      cboFarbmittel.Enabled = True
      'X = TDBRezept.Location.X + TDBRezept.RowHeadersWidth
      'Y = TDBRezept.Location.Y
      'Y = Y + TDBRezept.Height - TDBRezept.Rows(RezSozpt.Rezepte("MNG").KF).Height
      ''TDBRezept.LeftCol = 0
      'cboFarbmittel.SetBounds(X, Y, TDBRezept.Columns(1).Width, TDBRezept.Rows(RezSozpt.Rezepte("MNG").KF).Height)
      Call LocateCombo(cboFarbmittel, TDBRezept)
    Else
      cboFarbmittel.Visible = False
      cboFarbmittel.Enabled = False
    End If
  End Sub





  Private Sub RezTab_RowDeleted(ByVal sender As Object, ByVal e As System.Data.DataRowChangeEventArgs) Handles RezTab.RowDeleted

    Call GridKorrektur(RezSozpt)

  End Sub




  Private Sub RezTab_RowDeleting(ByVal sender As Object, ByVal e As System.Data.DataRowChangeEventArgs) Handles RezTab.RowDeleting
    Dim i As Integer
    If RezSozpt.Rezepte("KOR").KF = 0 AndAlso CSng(e.Row("GEWICHT")) = 0.0 Then
      For i = 0 To RezSozpt.Rezepte("MNG").KF - 1
        If RezSozpt.Rezepte("MNG")(i).ID = e.Row("FAID") Then
          RezSozpt.Rezepte("MNG").RemoveFaNr(i)
          Exit For
        End If
      Next
    Else
      e.Row.RejectChanges()
    End If
  End Sub

  Sub LichtartWinkel_click(ByVal sender As Object, ByVal e As System.EventArgs)
    If Not Me.Visible Then Exit Sub
    Call SetPicgraphics()
    Call LichtArtText()
    Call MakeVisible()

    PictureBoxLab.Refresh()
    PictureBoxfarbwerte.Refresh()
    PictureBoxRezept.Refresh()
    PictureBoxKurve.Refresh()
    Call pictureBoxFarbe.Refresh()
  End Sub
  Sub SetPicgraphics()
    PicGraphic.WeSc(0) = True
    PicGraphic.WeSc(1) = False
    For i = 0 To radLichtart.Count - 1
      If radLichtart(i).Checked Then
        PicGraphic.Knlz = i
      End If
    Next i
    For i = 0 To radwinkel.Count - 1
      If radwinkel(i).Checked Then
        PicGraphic.Kwop = i
      End If
    Next i
    For i = 0 To chkwinkel.Count - 1
      PicGraphic.kwopt(i) = False
      If chkwinkel(i).Checked Then
        PicGraphic.kwopt(i) = True
      End If
    Next i
  End Sub
  Private Sub TDBRezept_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles TDBRezept.DataError
    e.Cancel = False
  End Sub
  Private Sub TDBRezept_CellFormatting(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellFormattingEventArgs) Handles TDBRezept.CellFormatting
    If (e.RowIndex >= 0 And e.RowIndex < RezSozpt.Rezepte("MNG").KF) AndAlso (e.ColumnIndex > 1 And e.ColumnIndex < 5) Then
      If Not IsDBNull(e.Value) Then
        e.Value = Format(e.Value, RezSozpt.Farben(KeyName(RezSozpt.Rezepte("MNG")(e.RowIndex).ID)).Form)
      End If
    End If
  End Sub


  Private Sub frmMankiewKorrektur_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize
    If Not Me.Visible Then Exit Sub
    Call ResizeChild(Me)

  End Sub



  Private Sub btnRezVerw_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRezVerw.Click
    Dim OneGlzGrd As String
    Dim WithMessg As String
    OneGlzGrd = " AND TBL_FARBM.FARBM_ID=GLZGRD_ID"
    WithMessg = "TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    'Liste der auswählbaren Farb-/Bindemittel
    '
    FarbAlleTabelle.Clear()
    lstFarbmittel.DataSource = FarbAlleTabelle
    DataAdapter.SelectCommand.CommandText = "SELECT TBL_FARBM.FARBM_ID, TBL_FARBM.FARBM_NAME,FARBM_ICHF,FARBM_FST,FARBM_SMENGE,FARBM_FARBID " _
    & "FROM TBL_FARBM INNER JOIN TBL_GRUND_FARBM ON (TBL_FARBM.FARBM_ID = TBL_GRUND_FARBM.FARBM_ID) AND (TBL_FARBM.MISCH_ID = TBL_GRUND_FARBM.MISCH_ID)" _
    & "WHERE (((TBL_GRUND_FARBM.GKWRT_ID)=" & MenueParam.Misch.GKwrtID & ") AND ((TBL_FARBM.MISCH_ID)=" & MenueParam.MischID & "))" & " AND " & WithMessg & OneGlzGrd & " ORDER BY FARBM_NAME;"
    FarbAlleTabelle.Clear()
    If Not FillDatset(DataAdapter, FarbAlleTabelle) Then
      Exit Sub
    End If
    FarbAlleTabelle.AcceptChanges()
    lstFarbmittel.DisplayMember = "FARBM_NAME"
    lstFarbmittel.ValueMember = "FARBM_ID"
    FarbAlleTabelle.AcceptChanges()
    Call RezToTab("MNG", RezSozpt, FarbTabelle)
    Umr.CalcFamng("MNG", RezSozpt, ier)
    If BitWrt(30, MenueParam.User.Writ) Then
      For i = 0 To FarbTabelle.Rows.Count - 1
        flgFarbmittel.Rows(i).DefaultCellStyle.BackColor = Color.FromArgb(FarbTabelle.Rows(i)("FARBM_FARBID"))
      Next i
    End If
    SplitContainPreview.Hide()
    SplitContainPicture.Hide()
    SplitContainRezSuch.Hide()
    SplitContainRezManag.Show()
    txtACT.Text = TabSum(2, flgFarbmittel)
  End Sub

  Private Sub btnRezeptZurück_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRezeptZurück.Click
    Dim ivol As Integer
    Dim Newmenge As Single
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim ier As Integer
    Dim MiID As Integer
    Dim FaID As Integer
    Dim HilfID As Integer
    Dim Arbfarb As Colorant
    '
    '
    btnDruckenZus.Enabled = True
    MiID = MenueParam.MischID
    For i = 0 To FarbTabelle.Rows.Count - 1
      If Not FarbTabelle.Rows(i).RowState = DataRowState.Deleted AndAlso IsDBNull(FarbTabelle.Rows(i)("FARBM_ID")) Then
        Exit For
      End If
      If FarbTabelle.Rows(i).RowState = DataRowState.Added Then
        '
        'Prüfen, ob Farb-/Bindemittel bereits vorhanden
        '
        '
        HilfID = FarbTabelle.Rows(i)("FARBM_ID")
        If Not RezSozpt.Farben.ContainsFarb(KeyName(HilfID)) Then
          Arbfarb = New Colorant
          FaID = HilfID
          ReWrFarbe.FarReaGrund(FaID, Arbfarb, True, ier)
          Arbfarb.OptData.OptID = FaID
          RezSozpt.Farben.AddFarb(KeyName(HilfID), Arbfarb)
        End If
      ElseIf FarbTabelle.Rows(i).RowState = DataRowState.Deleted Then
        FarbTabelle.Rows(i).RejectChanges()
        HilfID = FarbTabelle.Rows(i)("FARBM_ID")
        RezSozpt.Farben.RemoveFarb(KeyName(HilfID))
        FarbTabelle.Rows(i).Delete()
      End If
    Next
    Cndat.Close()
    Call TabToRez("MNG", RezSozpt, FarbTabelle)
    Umr.CalcFamng("MNG", RezSozpt, ier)
    RezSozpt.Rezepte("KOR").clear()
    RezSozpt.Rezepte("MZU").clear()
    RezSozpt.Rezepte("ZUW").clear()

    Call MakeVisible()
    If BitWrt(30, MenueParam.User.Writ) Then
      For i = 0 To FarbTabelle.Rows.Count - 1
        flgFarbmittel.Rows(i).DefaultCellStyle.BackColor = Color.FromArgb(FarbTabelle.Rows(i)("FARBM_FARBID"))
      Next i
    End If
    '
    '
    '
    RezSozpt.MngMin = Singl(txtMngMin.Text)
    RezSozpt.MngMax = Singl(txtMngMax.Text)
    RezSozpt.ProzMin = Singl(txtProzMin.Text)
    RezSozpt.ProzMax = Singl(txtProzMax.Text)
    '
    Newmenge = Umr.MngINF("MNG", RezSozpt, ier)
    If Newmenge < RezSozpt.MngMin Then
      Newmenge = RezSozpt.MngMin
    End If
    If Newmenge > RezSozpt.MngMax Then
      Newmenge = RezSozpt.MngMax
    End If
    txtACT.Text = Newmenge
    Call RezeptNewMenge(ivol, Newmenge, Umr, RezKey, RezSozpt, ier)
    If ier <> 0 Then
      Exit Sub
    End If
    Call Umr.CalcFamng("MNG", RezSozpt, ier)
    Call GridKorrektur(RezSozpt)
    '

    Call RezAbsp("MNG", OlduserID, ier)
    If ier <> 0 Then
      Exit Sub
    End If

    '
    '
    '
    '
    '
    SplitContainPreview.Hide()
    SplitContainPicture.Hide()
    SplitContainRezManag.Hide()
    SplitContainRezSuch.Show()
    btnRezepte.PerformClick()
  End Sub
  Sub RezAbsp(RzNr As String, OlduserID As Integer, ByRef ier As Integer)
    Dim Rezname As String
    Dim RzID As Integer
    'Rezept abspeichern
    '
    '
    '
    ier = 0
    For i = 0 To 1
      TypID(i) = GrpRwerte(i)("V").ID
      UntID(i) = GrpRwerte(i).RefUnt.ID
      If RzNr = "KOR" Then
        SmpID(i) = -1
      Else
        SmpID(i) = GrpRwerte(i)("N").ID
      End If
    Next
    If Not BitWrt(7, MenueParam.User.Writ) Then Exit Sub
    Rezname = RezSozpt.Rezepte(RzNr).Name
    If Rezname.Trim = "" Then
      Rezname = RezSozpt.Rezepte("MNG").Name
    End If
    Rezname = InputBox("", Texxt(4635), Rezname)
    If Rezname <> "" Then

      Call ReWrRezept.ReadRezeptName(Rezname, RzID, ier)
      RezSozpt.Rezepte(RzNr).Name = Rezname
      RezSozpt.Rezepte(RzNr).Gid = MenueParam.Misch.UserRzpGID
      If RzID = -1 Then
        '
        'Name alt <> Name neu
        '
        If MessageBox.Show(Texxt(4614) & "?", Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
          Call ReWrRezept.AddRezept(RzNr, RzID, RezSozpt, UntID, TypID, SmpID, ier)
          MsgBox(Texxt(888) & ": " & Rezname & " " & Texxt(13), MsgBoxStyle.OkOnly, Texxt(2000))
          Rezname = RezSozpt.Rezepte("MNG").Name
          lblRezept.Text = Rezname
          Exit Sub
        End If
      Else
        If Not BitWrt(21, MenueParam.User.Sonst) Then
          '
          'Rezept bereits vorhanden
          '
          '
          '
          MsgBox(Texxt(2955), MsgBoxStyle.OkOnly, Texxt(2000))
          Exit Sub
        Else
          '
          '
          'Userabhängig
          '
          '
          If BitWrt(9, MenueParam.User.Writ) Then
            '
            '
            If MenueParam.UserID <> OlduserID Then
              'User verschieden
              MsgBox(Texxt(3103), MsgBoxStyle.OkOnly, Texxt(2000))
              Exit Sub
            End If
          End If
          '
          'Überschreiben
          '
          If MessageBox.Show(Texxt(2955) & vbCrLf & Texxt(3034), Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
            Call ReWrRezept.UpdateRezept(RzNr, RzID, RezSozpt, UntID, TypID, SmpID, ier)
            MsgBox(Texxt(888) & ": " & Rezname & " " & Texxt(13), MsgBoxStyle.OkOnly, Texxt(2000))
          End If
        End If
      End If
    End If
  End Sub
  Private Sub lstFarbmittel_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstFarbmittel.Click
    Dim i As Integer
    Dim row As DataRow
    Dim FaID As Integer
    FaID = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_ID")
    '
    '
    '
    'Prüfen, ob Farbmittel bereits vorhanden
    '
    '
    For i = 0 To FarbTabelle.Rows.Count - 1
      If Not FarbTabelle.Rows(i).RowState = DataRowState.Deleted Then
        If CInt(FarbTabelle.Rows(i)("FARBM_ID")) = FaID Then
          Exit Sub
        End If
      End If
    Next

    row = FarbTabelle.NewRow
    ReWrFarbe.FarRea(FaID, ArbFarb, True, ier)
    row("FARBM_ID") = FaID
    row("FARBM_NAME") = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_NAME")
    row("FARBM_MENGE") = 0.0
    row("FARBM_PROZ") = 100.0
    row("FARBM_PROB") = 100.0
    row("FARBM_PREIS") = ArbFarb.Pre(0)
    row("FARBM_FST") = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_FST")
    row("FARBM_OPERAT") = " "
    row("FARBM_LIMMNG") = 0.0
    row("FARBM_ICHF") = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_ICHF")
    row("FARBM_FARBID") = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_FARBID")

    FarbTabelle.Rows.Add(row)
    If BitWrt(30, MenueParam.User.Writ) Then
      For i = 0 To FarbTabelle.Rows.Count - 1
        flgFarbmittel.Rows(i).DefaultCellStyle.BackColor = Color.FromArgb(FarbTabelle.Rows(i)("FARBM_FARBID"))
      Next i
    End If
  End Sub

  Private Sub hscMinMeng_Scroll(ByVal sender As Object, ByVal e As System.Windows.Forms.ScrollEventArgs) Handles hscMinMeng.Scroll
    If Not IsNumeric(txtMinMeng.Text) Then Exit Sub
    If hscMinMeng.Value < 0 Then Exit Sub
    If Not hscMinMeng.Enabled Then Exit Sub
    'If hscMinMeng.Value = HscValue Then Exit Sub
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    Cursor = Cursors.WaitCursor
    txtMinMeng.Enabled = False
    MenueParam.Misch.Gge = GGEMax * hscMinMeng.Value / hscMinMeng.Maximum
    If MenueParam.Misch.Igx = 6 Then
      MenueParam.Misch.Dto = Singl(txtDTO.Text)
    End If
    KFIT = 1
    If chkRwertAngleich.Checked Then
      KFIT = 2
    End If
    CalcRezept.KorrekturRezept(KFIT, MenueParam.User.Winkel, "MNG", "KOR", RezSozpt, GrpRwerte, ier)
    txtMinMeng.Enabled = True
    If ier <> 0 Then
      Cursor = Cursors.Arrow
      Exit Sub
    End If
    '
    Call MakeVisible()
    '
    '
    '
    Call GridKorrektur(RezSozpt)

    pictureBoxFarbe.Refresh()
    If MenueParam.Misch.Igx = 6 Then
      e.NewValue = (hscMinMeng.Maximum - hscMinMeng.LargeChange + 1) * MenueParam.Misch.Gge / CSng(txtMinMeng.Text)
    End If
    Cursor = Cursors.Arrow

  End Sub


  Private Sub cboMNG_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMNG.SelectedIndexChanged
    RezSozpt.INO = cboMNG.SelectedIndex
    If RezSozpt.INO = 1 Then
      lblACT.Visible = True
      txtACT.Visible = True
    Else
      lblACT.Visible = False
      txtACT.Visible = False
    End If
  End Sub

  Private Sub cboPROZNen_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboPROZNen.SelectedIndexChanged
    RezSozpt.INQ = cboPROZNen.SelectedIndex
  End Sub

  Private Sub cboPROZZae_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboPROZZae.SelectedIndexChanged
    RezSozpt.INP = cboPROZZae.SelectedIndex
  End Sub



  Private Sub txtMinMeng_Validated(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtMinMeng.Validated
    GGEMax = CSng(txtMinMeng.Text)

  End Sub



  Private Sub txtZuwaag_Validated(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtZuwaag.Validated
    ZuPrz = 0.01 * CSng(txtZuwaag.Text)
  End Sub



  Private Sub lstRezepte_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstRezepte.DoubleClick
    Dim Ret As DialogResult
    If IsNumeric(lstRezepte.SelectedValue) Then
      Ret = MessageBox.Show(Texxt(3030) & ": " & lstRezepte.Text & Space(1) & Texxt(3031), Texxt(2000), MessageBoxButtons.YesNo)
      If Ret = DialogResult.Yes Then
        ReWrRezept.DelRezept(lstRezepte.SelectedValue, lstRezepte.SelectedItem("REZEPT_GID"), ier)
        Call ShowMe()
      End If
    End If

  End Sub



  Private Sub chkGEWICHT_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkGEWICHT.CheckedChanged
    If chkGEWICHT.Checked Then
      panGewichte.Visible = True
      panFarbwerte.Visible = False
    Else
      panGewichte.Visible = False
      panFarbwerte.Visible = True
    End If
  End Sub
  Sub GetNameID(ByRef ID As Integer, ByRef Name As String, ByRef Banum As String)
    '

    '
    '
    '
    pictureBoxFarbe.Refresh()
  End Sub
  Function TabSum(ByVal Icolumn As Integer, ByVal flgFarbmittel As DataGridView) As Single
    Dim i As Integer
    Dim Sum As Single
    Sum = 0.0
    For i = 0 To flgFarbmittel.Rows.Count - 1
      Sum = Sum + Singl(CStr(flgFarbmittel.Rows(i).Cells(Icolumn).Value))
    Next
    TabSum = Sum
  End Function
  Private Sub flgFarbmittel_CellBeginEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellCancelEventArgs) Handles flgFarbmittel.CellBeginEdit
    If FarbTabelle.Rows.Count <= e.RowIndex Then
      e.Cancel = True
    End If
    Exit Sub
  End Sub


  Private Sub flgFarbmittel_CellEndEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles flgFarbmittel.CellEndEdit
    txtACT.Text = TabSum(2, flgFarbmittel)
    If e.ColumnIndex = 2 Then
      btnDruckenZus.Enabled = False
    End If
  End Sub



  Private Sub flgFarbmittel_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles flgFarbmittel.DataError
    e.Cancel = False
  End Sub

 

  Private Sub flgFarbmittel_RowHeaderMouseDoubleClick(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles flgFarbmittel.RowHeaderMouseDoubleClick
    flgFarbmittel.Rows.RemoveAt(e.RowIndex)
  End Sub

  Private Sub flgFarbmittel_RowsRemoved(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewRowsRemovedEventArgs) Handles flgFarbmittel.RowsRemoved
    txtACT.Text = TabSum(2, flgFarbmittel)

  End Sub

  Private Sub txtDTO_CursorChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtDTO.CursorChanged
    If MenueParam.Misch.Dto <> Singl(txtDTO.Text) And txtDTO.Text <> "" Then
      MenueParam.Misch.Dto = Singl(txtDTO.Text)
      If MenueParam.Misch.Igx = 5 Then
        hscZuwaag.Value = hscZuwaag.Maximum
      End If
      If MenueParam.Misch.Igx = 6 Then
        hscMinMeng.Value = hscMinMeng.Maximum
      End If
    End If
  End Sub

  Private Sub cboMisch_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMisch.SelectedIndexChanged
    Call ShowMe()
    'btnRezepte.PerformClick()
  End Sub




  Private Sub btnUserProg_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnUserProg.Click
    Call UserDosieranlage("MZU", RezSozpt)
  End Sub




  Private Sub btnRwerteUntergrund_Click(sender As System.Object, e As System.EventArgs) Handles btnRwerteUntergrund.Click

    GrpRwerte(0).RefUnt.IVoNa = False
    GetRwerte.Messrefel = GrpRwerte(0).RefUnt
    Me.Enabled = False
    GetRwerte.Iarch = 0
    GetRwerte.Captext = Texxt(890)
    Call GetRwerte.ReflexWerte(False)

    Me.Enabled = True


    If GrpRwerte(0).RefUnt.IVoNa Then
      lblRwerteUntergrund.Text = GrpRwerte(0).RefUnt.Name.Trim
    End If
  End Sub

  Private Sub txtDickealt_Leave(sender As Object, e As System.EventArgs) Handles txtDickeAlt.Leave
    If IsNothing(RezAlt) Then Exit Sub
    If RezSozpt.Rezepte(RezAlt).KF = 0 Then
      Exit Sub
    End If
    RezSozpt.Rezepte(RezAlt).Dicke(0) = Singl(txtDickeAlt.Text)
    '
    '
    '
    '
    Call RwWrRezept.UpdateRezept(RezAlt, RezSozpt.Rezepte(RezAlt).ID, RezSozpt, UntID, TypID, SmpID, ier)
  End Sub
  Private Sub txtDickeNeu_Leave(sender As Object, e As System.EventArgs) Handles txtDickeNeu.Leave
    If IsNumeric(txtDickeNeu.Text) Then
      DickeNeu = txtDickeNeu.Text
    End If
  End Sub

  Private Sub txt_Enter(sender As Object, e As System.EventArgs) Handles txtGewDL.Enter, txtGewDC.Enter, txtGewDH.Enter, txtMngMax.Enter, txtMngMin.Enter, txtProzMax.Enter, txtProzMin.Enter, txtDTO.Enter, txtGewNL1.Enter, txtGewNL2.Enter, txtGewNL3.Enter, txtGewNL4.Enter, txtGewNL5.Enter, txtMenge.Enter, txtDosis.Enter, txtDickeAlt.Enter, txtDickeNeu.Enter, txtACT.Enter, txtZuwaag.Enter, txtMinMeng.Enter
    sender.tag = sender.text
  End Sub

  Private Sub txt_TextChanged(sender As Object, e As System.EventArgs) Handles txtGewDL.TextChanged, txtGewDC.TextChanged, txtGewDH.TextChanged, txtMngMax.TextChanged, txtMngMin.TextChanged, txtProzMax.TextChanged, txtProzMin.TextChanged, txtDTO.TextChanged, txtGewNL1.TextChanged, txtGewNL2.TextChanged, txtGewNL3.TextChanged, txtGewNL4.TextChanged, txtGewNL5.TextChanged, txtMenge.TextChanged, txtDosis.TextChanged, txtDickeAlt.TextChanged, txtDickeNeu.TextChanged, txtACT.TextChanged, txtZuwaag.TextChanged, txtMinMeng.TextChanged, txtGlzGrd.TextChanged
    If Not IsNumeric(sender.text) Then
      sender.text = sender.tag
    End If
  End Sub

  Private Sub hscGewNormlicht_Scroll(sender As System.Object, e As System.Windows.Forms.ScrollEventArgs) Handles hscGewNormlicht.Scroll
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    txtGewNL0.Text = Format(NormGewAlt * hscGewNormlicht.Value / hscGewNormlicht.Maximum, "###.00")
    Call CalcKorrRezept()
  End Sub

  Private Sub txtSUM_2_KeyPress(sender As Object, e As System.Windows.Forms.KeyPressEventArgs) Handles txtSUM_2.KeyPress
    If e.KeyChar = vbCr Then
      sender.enabled = False
      sender.enabled = True
    End If
  End Sub
  Private Sub txtSUM_2_Leave(sender As Object, e As System.EventArgs) Handles txtSUM_2.Leave
    Dim i As Integer
    Dim NewMenge As Single
    Dim ivol As Integer
    ivol = RezSozpt.IVOL
    If Not IsNumeric(txtSUM_2.Text) Then Exit Sub
    NewMenge = CSng(txtSUM(2).Text)
    Call RezeptNewMenge(ivol, NewMenge, Umr, RezKey, RezSozpt, ier)
    Call Umr.CalcFamng("MNG", RezSozpt, ier)
    Call GridKorrektur(RezSozpt)

    txtMngMin.Text = RezSozpt.MngMin
    txtMngMax.Text = RezSozpt.MngMax
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


  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtMenge.Validating, txtProzMax.Validating, txtProzMin.Validating, txtPreisn.Validating, txtPreisv.Validating, _
   txtSUM_2.Validating, txtSUM_3.Validating, txtSUM_4.Validating, txtDickeAlt.Validating, txtDickeNeu.Validating,
  txtDosis.Validating, txtDTO.Validating, txtGewDC.Validating, txtGewDH.Validating, txtGewDL.Validating, txtMETAn.Validating, txtMETAv.Validating, txtZuwaag.Validating, _
  txtDan.Validating, txtDav.Validating, txtDbn.Validating, txtDbv.Validating, txtDCn.Validating, txtDCv.Validating, txtDCn.Validating, txtDCv.Validating, txtDEn.Validating, txtDEv.Validating, txtDHn.Validating, txtDHv.Validating, txtDLn.Validating, txtDLv.Validating, _
  txtGewDC.Validating, txtGewDH.Validating, txtGewDL.Validating, txtGewNL0.Validating, txtGewNL1.Validating, txtGewNL2.Validating, txtGewNL3.Validating, txtGewNL4.Validating, txtGewNL5.Validating, _
  txtLAB_0.Validating, txtLAB_1.Validating, txtLAB_2.Validating, txtLCH_0.Validating, txtLCH_1.Validating, txtLCH_2.Validating

    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub

  Private Sub txtDat_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtBIS.Validating, txtVon.Validating
    If Not IsDate(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub

  Private Sub btnKorrSpezial_Click(sender As Object, e As System.EventArgs) Handles btnKorrSpezial.Click
    Dim irest As Integer
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim KF As Integer
    Dim INO As Integer
    Dim iprn As Integer
    Dim Fako(,) As Single
    Dim Isch As Integer
    Dim Ifl As Integer
    Dim Ifw As Integer
    Dim Index As Integer
    Dim XYZS(0, 0, 2) As Single
    Dim XYZI(0, 0, 2) As Single
    Dim ier As Integer
    '
    '
    '
    '
    'Prüfen, ob Rechnung über Farbkoordinaten erfolgen soll
    Index = -1
    For i = 0 To radNachst.Count - 1
      If radNachst(i).Checked Then
        Index = i
        Exit For
      End If
    Next
    '
    '
    '
    If Index > 0 Then
      '
      'Berechnung der Remissionskurve für die Nachstellung mit Hilfe von Farbkoordinaten (Delta-Werte)
      '
      '
      '
      '
      '
      '
      'R-Werte berechnen
      '
      '
      '
      '

      ReDim Fako(0, 2)
      Ifl = 1
      Ifw = 1
      Select Case Index
        Case 1
          '
          'DL,Da,Db
          '
          For i = 0 To 2
            Fako(0, i) = txtLAB(i).Text
          Next
          Isch = 2
        Case 2
          '
          'DL,DC,DH
          '
          For i = 0 To 2
            Fako(0, i) = txtLCH(i).Text
          Next
          Isch = 3

      End Select
      Call Quali.RefFarbWerte(MenueParam.Messg.Winkel, Isch, Ifl, Ifw, Fako, XYZS, XYZI, GrpRwrtUmrech("X"), ier)
      If ier <> 0 Then
        Cursor = Cursors.Default
        Exit Sub
      End If
      Select Case Index
        Case 1
          GrpRwrtUmrech("X")(0).Name = GrpRwrtUmrech("X")(1).Name & "DLDaDb"
        Case 2
          GrpRwrtUmrech("X")(0).Name = GrpRwrtUmrech("X")(1).Name & "DLDCDH"
      End Select
      lblRwerteNachstellung.Text = Trim(GrpRwerte(0)("N").Name)
      '
      '
      'Kurve speichern
      '
      '
      GrpRwerte(0)("N").Gid = MenueParam.Messg.RwrtGID
      ReWrRwert.WriteRwert(GrpRwerte(0)("N").ID, GrpRwerte(0)("N"), ier)

      If ier <> 0 Then
        Cursor = Cursors.Default
        Exit Sub
      End If
      SmpID(0) = GrpRwerte(0)("N").ID
      SmpID(1) = -1
    End If
    '
    '
    '
    '
    '
    'irest = 1
    'If radRestFarb(1).Checked Then
    ' irest = 1
    ' End If
    'iprn = KFIT + 4 * irest
    iprn = 1
    RestFarb = New Colorant
    INO = RezSozpt.INO
    RezSozpt.INO = 1
    RezSozpt.MngMin = Umr.MngINO("MNG", RezSozpt, ier)
    RezSozpt.MngMax = RezSozpt.MngMin
    Call CalcRezept.Restfarbe(iprn, RezAlt, RezAlt, RezSozpt, GrpRwrtRest, RestFarb, ier)
    If ier <> 0 Then
      Exit Sub
    End If

    RestFarb.Name = InputBox(Texxt(815), Texxt(853), "@" & GrpRwrtRest(0)("V").Name)
    If Trim(RestFarb.Name) <> "" Then
      RestFarb.BoMng = 100.0
      RestFarb.OP = "="



      ReWrFarbe = New ReadWriteFarbe
      ReWrGrund = New ReadWriteGrund
      '
      '
      'Speicher Restfarbe
      '
      '
      '
      RestFarb.GlzGrdID = -1
      RestFarb.GlzGrd = 0.0
      ReWrFarbe.FarAdd(RestFarb, ier)
      '
      'Speichern Grunddaten der Restfarbe
      '
      '
      '
      ReWrGrund.WriteGrund(RestFarb.ID, RestFarb.OptData, MenueParam.Messg.Winkel, ier)



      ' DatenFarb.AddNewFarb(RestFarb)
      ReWrFarbe.dispose()
      ReWrGrund.dispose()
      RezSozpt.Farben.AddFarb(KeyName(RestFarb.ID), RestFarb)
      '
      '
      'Bindemittel entfernen
      '
      '

      For i = RezSozpt.Farben.FarbCount - 1 To 0 Step -1
        If RezSozpt.Farben(i).Ichf = 1 Or RezSozpt.Farben(i).Ichf = 8 Then
          For j = RezSozpt.Rezepte(RezAlt).KF - 1 To 0 Step -1
            If RezSozpt.Rezepte(RezAlt)(j).ID = RezSozpt.Farben(i).ID Then
              RezSozpt.Rezepte(RezAlt).RemoveFaNr(KeyRe(j))
              '
              '
              'Farbmittel in Rezept verschieben
              '
              '
              KF = RezSozpt.Rezepte(RezAlt).KF
              For k = j To KF - 1
                RezSozpt.Rezepte(RezAlt).AddFaNr(KeyRe(k), RezSozpt.Rezepte(RezAlt)(KeyRe(k + 1)))
                RezSozpt.Rezepte(RezAlt).RemoveFaNr(KeyRe(k + 1))
              Next
              RezSozpt.Farben.RemoveFarb(KeyName(RezSozpt.Farben(i).ID))
            End If
          Next j
        End If
      Next i
    Else
      Exit Sub
    End If
    For i = 0 To RezSozpt.Rezepte(RezAlt).KF - 1
      RezSozpt.Rezepte(RezAlt)(i).FaAmng = 0.0
    Next
    RezSozpt.Rezepte(RezAlt).AddFaNr(KeyRe(RezSozpt.Rezepte(RezAlt).KF), New ColorAmount)
    RezSozpt.Rezepte(RezAlt)(KeyRe(RezSozpt.Rezepte(RezAlt).KF - 1)).ID = RestFarb.ID
    RezSozpt.Rezepte(RezAlt)(KeyRe(RezSozpt.Rezepte(RezAlt).KF - 1)).FaAmng = 100.0
    Umr.CalcBamng(RezAlt, RezSozpt, ier)
    PicGraphic.DataChanged = True
    RezSozpt.MngMax = 1000
    RezSozpt.MngMin = 100
    RezSozpt.INO = INO
    Call GridKorrektur(RezSozpt)

    Call RezAbsp("MNG", OlduserID, ier)
   
    If ier <> 0 Then
      Exit Sub
    End If

    Application.DoEvents()
    btnKorrektur.Visible = True
    btnKorrektur.PerformClick()
    RezSozpt.Rezepte("KOR").Name = RezSozpt.Rezepte("MNG").Name

  End Sub

  Private Sub radNachst_CheckedChanged(sender As Object, e As System.EventArgs) Handles radNachst_0.CheckedChanged, radNachst_1.CheckedChanged, radNachst_2.CheckedChanged
    Dim index As Integer
    Dim i As Integer
    If IsNothing(sender.name) Or sender.name = "" Then Exit Sub
    index = CInt(sender.name.substring(10, 1))
    For i = 0 To 2
      txtLAB(i).Visible = False
      txtLCH(i).Visible = False
      lblLABCH(i).Visible = False
    Next
    '
    '
    '
    Select Case index
      Case 0
        If GrpRwerte(0)("V").IVoNa And GrpRwerte(0)("N").IVoNa Then
          btnKorrSpezial.Visible = True
        Else
          btnKorrSpezial.Visible = False
        End If
      Case 1
        '
        'DL,Da,Db
        '
        lblLABCH(0).Text = TexKt(10139)
        lblLABCH(1).Text = TexKt(10142)
        lblLABCH(2).Text = TexKt(10143)
        For i = 0 To 2
          lblLABCH(i).Visible = True
          txtLAB(i).Visible = True
        Next
        btnKorrSpezial.Visible = True
      Case 2
        '
        'DL,DC,DH
        '
        lblLABCH(0).Text = TexKt(10139)
        lblLABCH(1).Text = TexKt(10140)
        lblLABCH(2).Text = TexKt(10141)
        For i = 0 To 2
          lblLABCH(i).Visible = True
          txtLCH(i).Visible = True
        Next
        btnKorrSpezial.Visible = True


    End Select
  End Sub

 
  Private Sub cboMethIGX_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboMethIGX.SelectedIndexChanged
    If cboMethIGX.SelectedIndex > -1 Then
      MenueParam.Misch.Igx = cboMethIGX.SelectedIndex
    End If
  End Sub
End Class


