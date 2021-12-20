Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmQualControl
  Dim sectionTable As DataTable
  '
  Dim LayoutChanged As Boolean
  '
  Dim ppv As PrintPreviewDialog
  Dim AusgabeUser As AusgabeCreateObj

  Dim Teee As String
  Dim RefRow As DataRow
  Dim TabGlz As DataTable
  Dim TabInr As DataTable
  Dim TabWel As DataTable
  Dim TabMerkmale As DataTable
  Dim TabRwerte As DataTable
  Dim RwertTab As DataTable
  Dim RwertView As DataView
  Dim CmdRwert As OleDbCommand
  Dim OleRwert As OleDbDataAdapter
  Dim quali As QualKontrolle
  Dim HandleParameter As HandleParamMerk
  Dim Handlerwert As HandleRwerte
  Dim MerkDruck As HandlePlottDruck
  Dim picAufbau As HandlePictures
  Dim FarbReport As HandleReports
  Dim Ausgabe As AusgabeCreateObj
  Dim cboMISCEL As List(Of ComboBox)
  Dim lblMISCEL As List(Of Label)
  Dim TabMiscel As List(Of DataTable)
  Dim btnREF As List(Of Button)
  Dim lblREF As List(Of Label)
  Dim radNLA As List(Of RadioButton)
  Dim chkNLA As List(Of CheckBox)
  Dim radWIN As List(Of RadioButton)
  Dim chkWIN As List(Of CheckBox)
  Dim chkNextWIN As List(Of CheckBox)
  Dim radDRQ As List(Of RadioButton)
  Dim radGPL As List(Of RadioButton)
  Dim AdaptMiscel As OleDbDataAdapter
  Dim GrpRwerteCalc As RefValuesGrp
  Dim Winkel As AngGeos
  '
  Dim ReadWrite As ReadWriteRwert
  Dim GrpRwerte As RefValuesGrp
  Dim FarbWerte As ValuesGrpsAssigns
  Dim RefWert As RefValue
  Dim GrdArray As DataTable
  Dim GrdView As DataView
  Dim GrdRows() As DataRow
  Dim TabQuali As DataTable
  Dim ViewQuali As DataView
  Dim AdaptQuali As OleDbDataAdapter
  Dim CmdQuery As OleDbCommand
  Dim TabFst As DataTable
  Dim AdaptFst As OleDbDataAdapter
  Dim cdr(11) As String
  '
  Dim RezGraphics As HandleRezGrafik
  'Dim Tooltp As ToolTip
  '
  Dim KeyR As String
  Dim DragValue As Object
  Dim Xvalue As Single
  Dim Yvalue As Single
  Dim ZZZ As String = "X"
  Dim WhereKeyID() As String
  Dim NWE As Integer
  '
  '

  'Konstanten zur Dimensionierung von Feldern

  Dim ngrd As Integer = 30
  Dim nrei As Integer = 30
  Dim Nstr As Integer = 50

  'Menueparameter

  Dim ier As Integer
  Dim Captext As String
  Dim HlfTex() As String
  Dim PicInd As Integer
  Dim ipogrd(ngrd) As Integer
  Dim Kpogrd(ngrd) As Integer
  Dim rowa As Integer
  Dim cola As Integer
  Dim rowm As Integer
  Dim colm As Integer
  Dim icore As Integer
  Dim ID As Integer
  Dim gridwid As Integer
  Dim gridwidth As Integer
  Dim Rheight As Integer
  Dim irei As Integer
  Dim nreiwied As Integer
  Dim nreiwmax As Integer
  Dim cartf As String
  Dim zcol As Integer
  Dim zrow As Integer
  Dim startrows As Integer
  Dim RcmdRef As Integer
  Dim change As Boolean

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
  Dim imsg As Integer            'Messagebox (Rückgabewert)
  Dim MaxID As Integer             'Maximale ID (Primärschlüssel)
  '
  '
  'Anzahl Reflexionskurven (+Zusatzinformation) QU-Kontrolle
  '
  Dim Nqu As Integer
  '
  Dim KopfString As String
  Dim LBwert As Integer
  Dim WKDE As Single
  Dim SqlStmt As String

  'Ueberschriften fuer Qualitaetskontrolle

  'Dim UebGrd(0 To 3, 0 To ngrd) As String * 12

  'Anzahl Auswahlmoeglichkeiten fuer Positionierungen

  Dim IanGrd(ngrd)

  'Namen fuer frmREF - CAPTION = Bezeichnung fuer Messwerte

  Dim NstrRef(Nstr) As String

  'Reihenfolge fuer nstrref
  '
  Dim ireimeth(nrei) As Integer
  Dim ireiwied As Integer
  'Dim nreimeth As Integer
  Dim icamgrd(4, 1) As Integer            'Feldnr füer CAMP-Feld
  Dim mrkadr As DictionaryInd(Of String, GridMerkAdr)  'Adresse für Farbmerkmale in Grid 
  Dim KeyMrkAdr As String
  Private _farbMerkmale As Integer

  '
  '
  '
  '
  '
  Public Sub New()

    ' Dieser Aufruf ist für den Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
    LayoutChanged = False

  End Sub

  Private Property FarbMerkmale(j As Short) As Integer
    Get
      Return _farbMerkmale
    End Get
    Set(value As Integer)
      _farbMerkmale = value
    End Set
  End Property

  Private Sub frmQualControl_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed

    RemoveHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID

    Try
      GrpRwerte = Nothing
      quali = Nothing
      FarbWerte = Nothing
      GrdArray = Nothing
      Ausgabe = Nothing
      ReadWrite = Nothing


    Catch ex As Exception

    End Try
    cboFST.DataSource = Nothing
    cboBWE.Items.Clear()
    cboFST.Items.Clear()
    cboANZ.Items.Clear()
  End Sub


  '
  '
  '





  Private Sub frmQualControl_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    '
    'Vergleich von Farbwerten nur für einen Winkel
    '
    If MenueParam.MethID = 40 And MenueParam.User.Winkel.Km > 1 Then
      MsgBox(Texxt(3569))
      Exit Sub
    End If
    If MenueParam.MethID = 49 Then
      '
      'Mittelwert
      '
      Winkel = MenueParam.Messg.Winkel
    Else
      Winkel = MenueParam.User.Winkel
    End If
    panStandard.Visible = True
    panAuxiliary.Visible = False
    AusgabeUser = New AusgabeCreateObj
    Me.WindowState = FormWindowState.Maximized
    'MsgBox(Tooltp.GetToolTip(TDBGrid))
    '
    If PrinterSettings.InstalledPrinters.Count = 0 Then
      btnDRU.Visible = False
      btnAUS.Visible = False
    End If
    'ReftoolTip = New ToolTip
    'ReftoolTip.AutoPopDelay = 500
    'ReftoolTip.ShowAlways = True
    AddHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
    '
    TDBGrid.Columns.Clear()
    TDBParameter.Columns.Clear()
    change = True
    '
    GrpRwerte = New RefValuesGrp
    GrpRwerteCalc = New RefValuesGrp
    FarbWerte = New ValuesGrpsAssigns
    ReadWrite = New ReadWriteRwert
    TabMerkmale = New DataTable

    '

    quali = New QualKontrolle
    FarbReport = New HandleReports
    FarbReport.Farbwerte = FarbWerte
    FarbReport.GrpRwerte = GrpRwerte
    FarbReport.Farbtable = TabMerkmale


    MerkDruck = New HandlePlottDruck
    MerkDruck.printset = MnPrintSet
    MerkDruck.FarbWerte = FarbWerte
    MerkDruck.GrpRwerte = GrpRwerte
    Ausgabe = New AusgabeCreateObj
    GrdArray = New DataTable
    GrdView = New DataView(GrdArray)
    TDBGrid.DataSource = GrdArray

    '
    '
    'Aufbau RwerteGrp
    '
    cdr(0) = "W"
    cdr(1) = "S"
    cdr(2) = "G"
    cdr(3) = "H"
    cdr(4) = "F"
    cdr(5) = "U"
    cdr(6) = "C"
    cdr(7) = "K"
    cdr(8) = "E"
    cdr(9) = "A"
    cdr(10) = "D"
    cdr(11) = "T"

    GrpRwerteCalc.Add("X", New RefValues)
    For i = 0 To 3
      GrpRwerteCalc("X").Add(KeyRe(i), New RefValue)
    Next i
    Call ADDCurves(GrpRwerteCalc("X")(KeyRe(2)).RefKurv)
    Call ADDCurves(GrpRwerteCalc("X")(KeyRe(3)).RefKurv)

    NWE = Winkel.Wsol.Nwe
    '
    '
    GrpRwerte.Add("W", New RefValues)   'Messung (weiß)
    GrpRwerte.Add("S", New RefValues)   'Messung (schwarz)
    GrpRwerte.Add("G", New RefValues)   'Mischung Weiß/Schwarz (nur für Untergrund, falls METH=7 oder 15, Farbst. deckend, Deckvermögen deckend) 
    GrpRwerte.Add("H", New RefValues)   'Rechnung (weiß)
    GrpRwerte.Add("F", New RefValues)   'Rechnung (schwarz)
    GrpRwerte.Add("U", New RefValues)   'Unendlich dick
    GrpRwerte.Add("C", New RefValues)   'Max Chroma
    GrpRwerte.Add("K", New RefValues)   'K/S-Werte
    GrpRwerte.Add("E", New RefValues)   'Extinktion
    GrpRwerte.Add("A", New RefValues)   'Absorption
    GrpRwerte.Add("D", New RefValues)   'Streuung
    GrpRwerte.Add("T", New RefValues)   'Streuung

    '
    'Untergründe bzw. Messungen Weißpigment, Schwarzpigment und Weiß/Schwarzverschnitt
    '
    '
    GrpRwerte("W").RefUnt = New RefValue
    GrpRwerte("S").RefUnt = New RefValue
    GrpRwerte("G").RefUnt = New RefValue
    GrpRwerte("W").RefUnt.QuControl = New QuControls
    GrpRwerte("S").RefUnt.QuControl = New QuControls
    GrpRwerte("G").RefUnt.QuControl = New QuControls
    '
    '
    RefWert = New RefValue
    '
    '
    btnABR.Text = Texxt(379)
    btnBWE.Text = Texxt(185)
    btnDRU.Text = Texxt(397)
    btnAUS.Text = Texxt(395)
    btnGRF.Text = Texxt(393)
    btnKOP.Text = Texxt(396)
    btnLOE.Text = Texxt(381)
    btnUser.Text = Texxt(392)
    btnVER.Text = Texxt(360)
    btnVERAux.Text = Texxt(360)
    btnVOR.Text = Texxt(391)
    lblMiscel_0.Text = Texxt(2455)
    lblMiscel_1.Text = Texxt(2456)
    lblANZ.Text = Texxt(190)
    lblANZSuch.Text = Texxt(3689)
    lblGLZ.Text = Texxt(358)
    lblTRA.Text = Texxt(513)
    lblGLZAux.Text = Texxt(358)
    lblGRA.Text = Texxt(357)
    lblINR.Text = Texxt(359)
    lblIND.Text = Texxt(368)
    lblINRAux.Text = Texxt(359)
    lblINDAux.Text = Texxt(368)

    lblGRP.Text = Texxt(386)
    lblKDE.Text = Texxt(362)
    lblKOM.Text = Texxt(384)
    lblWEL.Text = Texxt(192)
    btnREF_0.Text = Texxt(351)
    btnREF_1.Text = Texxt(355)
    btnREF_2.Text = Texxt(356)
    btnREF_3.Text = Texxt(352)
    btnREF_4.Text = Texxt(353)
    btnREF_5.Text = Texxt(354)
    lblQCE.Text = Texxt(2002)
    chkPARAM.Text = Texxt(415)
    chkABS.Text = Texxt(363)
    chkLinear.Text = Texxt(532)
    radGPL_0.Text = Texxt(997)
    radGPL_1.Text = Texxt(527)
    radGPL_2.Text = Texxt(528)
    radGPL_3.Text = Texxt(529)
    radGPL_4.Text = Texxt(526)
    radDRQ_00.Text = Texxt(660)
    radDRQ_01.Text = Texxt(661)
    radDRQ_02.Text = Texxt(670)
    radDRQ_03.Text = Texxt(662)
    radDRQ_04.Text = Texxt(663)
    radDRQ_05.Text = Texxt(664)
    radDRQ_06.Text = Texxt(665)
    radDRQ_07.Text = Texxt(666)
    radDRQ_08.Text = Texxt(667)
    radDRQ_09.Text = Texxt(668)
    radDRQ_10.Text = Texxt(669)
    radDRQ_11.Text = Texxt(671)
    radANGL_0.Text = Texxt(1247)
    radANGL_1.Text = Texxt(1248)
    btnCalcRwerte.Text = Texxt(3110)
    chkLAB.Text = Trim(TexKt(17108)) & Space(1) & Trim(TexKt(17111)) & Space(1) & Trim(TexKt(17112))

    mnuCopy.Text = Texxt(3922)
    mnuPaste.Text = Texxt(3923)
    mnuDelete.Text = Texxt(3924)
    '
    '
    'Winkel für nächste R-Werte
    '
    chkNextWIN = New List(Of CheckBox)
    chkNextWIN.Clear()
    chkNextWIN.Add(chkNextWIN_00)
    chkNextWIN.Add(chkNextWIN_01)
    chkNextWIN.Add(chkNextWIN_02)
    chkNextWIN.Add(chkNextWIN_03)
    chkNextWIN.Add(chkNextWIN_04)
    chkNextWIN.Add(chkNextWIN_05)
    chkNextWIN.Add(chkNextWIN_06)
    chkNextWIN.Add(chkNextWIN_07)
    chkNextWIN.Add(chkNextWIN_08)
    '
    '
    radNLA = New List(Of RadioButton)
    radNLA.Clear()
    radNLA.Add(radNLA_0)
    radNLA.Add(radNLA_1)
    radNLA.Add(radNLA_2)
    radNLA.Add(radNLA_3)
    radNLA.Add(radNLA_4)
    '
    chkNLA = New List(Of CheckBox)
    chkNLA.Clear()
    chkNLA.Add(chkNLA_0)
    chkNLA.Add(chkNLA_1)
    chkNLA.Add(chkNLA_2)
    chkNLA.Add(chkNLA_3)
    chkNLA.Add(chkNLA_4)

    '
    '
    radWIN = New List(Of RadioButton)
    radWIN.Clear()
    radWIN.Add(radWIN_00)
    radWIN.Add(radWIN_01)
    radWIN.Add(radWIN_02)
    radWIN.Add(radWIN_03)
    radWIN.Add(radWIN_04)
    radWIN.Add(radWIN_05)
    radWIN.Add(radWIN_06)
    radWIN.Add(radWIN_07)
    radWIN.Add(radWIN_08)
    '
    '
    '
    '
    chkWIN = New List(Of CheckBox)
    chkWIN.Clear()
    chkWIN.Add(chkWIN_00)
    chkWIN.Add(chkWIN_01)
    chkWIN.Add(chkWIN_02)
    chkWIN.Add(chkWIN_03)
    chkWIN.Add(chkWIN_04)
    chkWIN.Add(chkWIN_05)
    chkWIN.Add(chkWIN_06)
    chkWIN.Add(chkWIN_07)
    chkWIN.Add(chkWIN_08)
    '
    '
    '
    '
    radDRQ = New List(Of RadioButton)
    radDRQ.Clear()
    radDRQ.Add(radDRQ_00)
    radDRQ.Add(radDRQ_01)
    radDRQ.Add(radDRQ_02)
    radDRQ.Add(radDRQ_03)
    radDRQ.Add(radDRQ_04)
    radDRQ.Add(radDRQ_05)
    radDRQ.Add(radDRQ_06)
    radDRQ.Add(radDRQ_07)
    radDRQ.Add(radDRQ_08)
    radDRQ.Add(radDRQ_09)
    radDRQ.Add(radDRQ_10)
    radDRQ.Add(radDRQ_11)

    '
    '
    '
    '
    radGPL = New List(Of RadioButton)
    radGPL.Clear()
    radGPL.Add(radGPL_0)
    radGPL.Add(radGPL_1)
    radGPL.Add(radGPL_2)
    radGPL.Add(radGPL_3)
    radGPL.Add(radGPL_4)
    '
    '
    '
    '
    btnREF = New List(Of Button)
    btnREF.Clear()
    btnREF.Add(btnREF_0)
    btnREF.Add(btnREF_1)
    btnREF.Add(btnREF_2)
    btnREF.Add(btnREF_3)
    btnREF.Add(btnREF_4)
    btnREF.Add(btnREF_5)
    '
    '
    lblREF = New List(Of Label)
    lblREF.Clear()
    lblREF.Add(Nothing)
    lblREF.Add(lblREF_1)
    lblREF.Add(lblREF_2)
    lblREF.Add(lblREF_3)
    lblREF.Add(lblREF_4)
    lblREF.Add(lblREF_5)


    For i = 0 To radNLA.Count - 1
      radNLA(i).Visible = False
    Next i
    For i = 0 To chkNLA.Count - 1
      chkNLA(i).Visible = False
    Next i
    For i = 0 To MenueParam.Normfa.Nlz - 1
      radNLA(i).Text = MenueParam.Normfa(i).NormKenn
      radNLA(i).Visible = True
      chkNLA(i).Text = MenueParam.Normfa(i).NormKenn
      chkNLA(i).Visible = True
      chkNLA(i).Checked = True
    Next i
    For i = 0 To radDRQ.Count - 1
      radDRQ(i).Visible = False
    Next i
    For i = 0 To chkWIN.Count - 1
      chkWIN(i).Visible = False
    Next i
    For i = 0 To radWIN.Count - 1
      radWIN(i).Visible = False
    Next i
    For i = 0 To Winkel.Km - 1
      chkWIN(i).Visible = True
      chkWIN(i).Checked = True
      chkWIN(i).Text = Winkel(i).Chrm
      chkNextWIN(i).Text = Winkel(i).Chrm
    Next i

    For i = 0 To Winkel.Km - 1
      radWIN(i).Visible = True
      radWIN(i).Text = Winkel(i).Chrm
    Next i
    '
    '
    '
    radGPL(0).Checked = True
    btnDRU.Enabled = True
    '
    '
    '
    'Werte für Glanzabzug
    '
    cboGLZ.Items.Clear()
    For i = 0 To GetPutReflex.GlanzWrt.Count - 1
      cboGLZ.Items.Add(GetPutReflex.GlanzWrt(i))
    Next
    cboGLZ.Enabled = True
    cboGLZ.Text = Winkel(0).GK(0)
    '
    cboGLZAux.Items.Clear()
    For i = 0 To GetPutReflex.GlanzWrt.Count - 1
      cboGLZAux.Items.Add(GetPutReflex.GlanzWrt(i))
    Next
    cboGLZAux.Enabled = True
    cboGLZAux.Text = Winkel(0).GK(0)

    '
    'Werte für innere Reflexion(ger)
    '
    '
    cboINR.Items.Clear()
    For i = 0 To GetPutReflex.GlanzWrt.Count - 1
      cboINR.Items.Add(GetPutReflex.GlanzWrt(i))
    Next
    cboINR.Enabled = True
    cboINR.Text = Winkel(0).GK(1)
    '
    cboINRAux.Items.Clear()
    For i = 0 To GetPutReflex.GlanzWrt.Count - 1
      cboINRAux.Items.Add(GetPutReflex.GlanzWrt(i))
    Next
    cboINRAux.Enabled = True
    cboINRAux.Text = Winkel(0).GK(1)

    '
    '
    'Werte für innere Reflexion(dif)
    '
    '
    cboIND.Items.Clear()
    For i = 0 To 19
      cboIND.Items.Add(Format(0.05 * i, "0.00"))
    Next
    cboIND.Enabled = True
    cboIND.Text = Winkel(0).GK(2) '
    '
    cboINDAux.Items.Clear()
    For i = 0 To 19
      cboINDAux.Items.Add(Format(0.05 * i, "0.00"))
    Next
    cboINDAux.Enabled = True
    cboINDAux.Text = Winkel(0).GK(2) '

    'Wellenlängen für Farbstärke
    '
    '
    '
    cboWEL.Items.Clear()
    cboWEL.Items.Add(Space(1))
    For i = 0 To Winkel.Wsol.Nwe - 1
      cboWEL.Items.Add(Winkel.Wsol.R(i))
    Next i
    cboWEL.Enabled = True
    cboWEL.SelectedIndex = 0
    '
    '
    TabMiscel = New List(Of DataTable)
    TabMiscel.Clear()
    TabMiscel.Add(New DataTable)
    TabMiscel.Add(New DataTable)
    cboMISCEL = New List(Of ComboBox)
    cboMISCEL.Add(cboMiscel_0)
    cboMISCEL.Add(cboMiscel_1)
    lblMISCEL = New List(Of Label)
    lblMISCEL.Add(lblMiscel_0)
    lblMISCEL.Add(lblMiscel_1)

    '
    '
    'Prüfmittel
    '
    AdaptMiscel = New OleDbDataAdapter
    AdaptMiscel.SelectCommand = New OleDbCommand("", Cncol)

    SqlStmt = "SELECT * FROM TBL_PRUEFM"
    AdaptMiscel.SelectCommand.CommandText = SqlStmt
    If Not FillDatset(AdaptMiscel, TabMiscel(0)) Then
      Exit Sub
    End If
    If TabMiscel(0).Rows.Count = 0 Or Not BitWrt(15, MenueParam.User.Writ) Then
      cboMISCEL(0).Visible = False
      lblMISCEL(0).Visible = False
    End If
    cboMISCEL(0).DataSource = TabMiscel(0)
    cboMISCEL(0).DisplayMember = "PRUEFM_KBEZ"
    cboMISCEL(0).ValueMember = "PRUEFM_ID" '
    cboMISCEL(0).Enabled = True
    '
    '
    'Produktart
    '
    SqlStmt = "SELECT * FROM TBL_PROART"
    AdaptMiscel.SelectCommand.CommandText = SqlStmt
    If Not FillDatset(AdaptMiscel, TabMiscel(1)) Then
      Exit Sub
    End If
    If TabMiscel(1).Rows.Count = 0 Or Not BitWrt(16, MenueParam.User.Writ) Then
      cboMISCEL(1).Visible = False
      lblMISCEL(1).Visible = False
    End If
    cboMISCEL(1).DataSource = TabMiscel(1)
    cboMISCEL(1).DisplayMember = "PROART_KBEZ"
    cboMISCEL(1).ValueMember = "PROART_ID" '
    cboMISCEL(1).Enabled = True
    '
    mrkadr = New DictionaryInd(Of String, GridMerkAdr)
    '
    '
    '
    Call StartMeth(ier)
    If ier <> 0 Then
      Me.Visible = False
      Exit Sub
    End If


    btnVER.Enabled = False
    '
    Call GridMeth(ier)
    If ier <> 0 Then
      Me.Visible = False
      Exit Sub
    End If
    '
    '
    '
    '
    Call GridStart(ier)
    If ier <> 0 Then
      Me.Visible = False
      Exit Sub
    End If
    rowa = 0
    cola = 1
    '
    '
    '
    '
    '
    'Zeichen für Bezug
    '
    '
    lblTYP.Text = ZZZ
    '
    '
    '
    'Fehlerabfragen
    '
    WKDE = MenueParam.Menue.KdeWS
    LBwert = MenueParam.Menue.BwertID
    '

    'methodenabhängige Anweisungen und Auswertungen
    '
    Call AufbauPar.AufbauAnwsgMerk(MenueParam.UserID, MenueParam.MethID, FarbWerte, ier)
    '
    '
    For ianwsg = 0 To FarbWerte.Count - 1
      ReDim FarbWerte(ianwsg).PrintNLAWIN(MenueParam.Normfa.Nlz - 1, Winkel.Km - 1)
    Next ianwsg
    '
    If ier = 3013 Then
      btnVER.Enabled = False
    Else
      btnVER.Enabled = True
    End If
    '
    '
    '
    'Qualitätsmerkmale
    '
    '
    TabQuali = New DataTable
    ViewQuali = New DataView(TabQuali)
    AdaptQuali = New OleDbDataAdapter
    AdaptQuali.SelectCommand = New OleDbCommand("", Cndat)
    AdaptQuali.SelectCommand.CommandText = "SELECT * FROM TBL_QUALI WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    If Not FillDatset(AdaptQuali, TabQuali) Then
      Exit Sub
    End If
    TabQuali.AcceptChanges()
    '
    '
    'Update Command
    '
    '
    ReDim WhereKeyID(2)
    WhereKeyID(0) = "MESSGRW_ID"
    WhereKeyID(1) = "QUALI_ID"
    WhereKeyID(2) = "METH_ID"
    AdaptQuali.UpdateCommand = OleDBUpdateCmd("TBL_QUALI", WhereKeyID, Cndat)
    '
    '
    'Insert Command
    '
    '
    AdaptQuali.InsertCommand = OleDBInsertCmd("TBL_QUALI", Cndat)
    '
    CmdQuery = New OleDbCommand("", Cndat)
    '
    '
    '
    PanQual.Visible = True
    lblQCE.Visible = False
    '
    '
    'Grid für METHID=40 (Vergleich von Farbwerte)
    '
    '
    Call MakeFarbwerte(dbgFarbwerte)
    '



    'Merkmale
    '
    '
    flgMerk.DataSource = TabMerkmale
    Call MakeMerkGrid(FarbWerte, TabMerkmale, flgMerk)
    '
    '
    '
    'R-Werte
    '
    '
    TabRwerte = New DataTable
    Call GetPutReflex.MakeTABRefwerte(False, MenueParam.Messg.Kbez, Winkel, TabRwerte)
    '
    flgRwert.DataSource = TabRwerte
    Call GetPutReflex.MakeGRIDRefwerte(Winkel, flgRwert)

    '
    PanQual.Visible = True
    panResult.Visible = False
    '
    '
    'Grafische Ausgabe
    '
    RezGraphics = New HandleRezGrafik
    RezGraphics.Knlz = 0
    RezGraphics.Kwop = 0
    RezGraphics.ipl = 0
    cboSKAL.Enabled = True
    RezGraphics.cboSKAL = cboSKAL

    RezGraphics.FawrtRwerte.Add("Z", New RefValues)

    '


    '
    '
    '
    '
    '


    '
    RezGraphics.Winkel = Winkel

    '

    picAufbau = New HandlePictures
    picAufbau.Add("REF", CType(picDRQ, PictureBox))
    picAufbau.Add("LAB", CType(picLAB, PictureBox))
    picAufbau.Add("XYZ", CType(picXYZ, PictureBox))
    picAufbau.PicGraphic = RezGraphics
    MerkDruck.Plott = RezGraphics

    '
    '
    '
    '
    'RezCheckRad.AddchkUUU = chkUUU_0
    'RezCheckRad.AddchkUUU = chkUUU_1
    'Grid für Parameter aufbauen
    '
    '
    '
    HandleParameter = New HandleParamMerk(TDBParameter, TDBDropParameter, FarbWerte)

    If MenueParam.ParaMerks.ParamCount > 0 Then
      HandleParameter.FillGrid(MenueParam.UserID, MenueParam.MethID)
    End If
    '
    '
    If BitWrt(25, MenueParam.User.Writ) And MenueParam.ParaMerks.ParamCount > 0 And TDBParameter.DataSource.Count > 0 Then
      chkPARAM.Visible = True
    End If


    '
    '
    '
    'RezCheckRad.AddradUUU = radUUU_0
    'RezCheckRad.AddradUUU = radUUU_1
    cboWEL.Enabled = True
    chkLAB.Checked = False
  End Sub





  Function UebStr() As List(Of String)
    Static Teex As List(Of String)
    If IsNothing(Teex) Then
      Teex = New List(Of String)




      '
      '
      's. auch Texxt(1000 ff)
      '
      '
      '
      '
      'Startwerte
      '
      '


      Teex.Add("        ")   'Reflexionswerte ueber weissem Untergrund oder fuer Weissverchnitt
      '
      Teex.Add("        ")   'Reflexionswerte ueber schwarzem Untergrund oder fuer Schwarzverchnitt
      '
      '
      '
      Teex.Add("1       ")   'Menge Weisspaste
      'uebstr(1, 2) = "1.000000"  'Menge Weisspigment (rein)
      'uebstr(2, 2) = "1.000000"  'Menge Bindemittel in Weisspaste
      'uebstr(3, 2) = "1.000000"  'Menge Loesemittel in Weisspigment

      Teex.Add("1       ")   'Volumen Weisspaste
      'uebstr(1, 3) = "1.000000"  'Volumen Weisspigment (rein)
      'uebstr(2, 3) = "1.000000"  'Volumen Bindemittel in Weisspaste
      'uebstr(3, 3) = "1.000000"  'Volumen Loesemittel in Weisspaste

      Teex.Add("100     ")   'Pigmentgewichtskonzentration Weisspigment      (ohne Loesemittel)
      'uebstr(1, 4) = "100.0000"  'Pigmentkonzentration Weisspigment              (mit Loesemittel)
      'uebstr(2, 4) = "100.0000"  'Pigmentvolumenkonzentration Weisspigment       (ohne Loesemittel)
      'uebstr(3, 4) = "100.0000"  'Pigmentkonzentration (Volumen) Weisspigment    (mit Loesemittel)

      Teex.Add("100     ")   'Bindemittelkonzentration in Weisspaste
      '
      Teex.Add("1       ")   'spez. Gewicht Weisspigment
      '
      '
      '
      '
      Teex.Add("1       ")   'Menge Schwarzpaste
      'uebstr(1, 7) = "1.000000"  'Menge Schwarzpigment (rein)
      'uebstr(2, 7) = "1.000000"  'Menge Bindemittel in Schwarzpaste
      'uebstr(3, 7) = "1.000000"  'Menge Loesemittel in Schwarzpigment

      Teex.Add("1       ")   'Volumen Schwarzpaste
      'uebstr(1, 8) = "1.000000"  'Volumen Schwarzpigment (rein)
      'uebstr(2, 8) = "1.000000"  'Volumen Bindemittel in Schwarzpaste
      'uebstr(3, 8) = "1.000000"  'Volumen Loesemittel in Schwarzpaste

      Teex.Add("100     ")   'Pigmentgewichtskonzentration Schwarzpigment    (ohne Loesemittel)
      'uebstr(1, 9) = "100.0000"  'Pigmentkonzentration Schwarzpigment            (mit Loesemittel)
      'uebstr(2, 9) = "100.0000"  'Pigmentvolumenkonzentration Schwarzpigment     (ohne Loesemittel)
      'uebstr(3, 9) = "100.0000"  'Pigmentkonzentration (Volumen) Schwarzpigment  (mit Loesemittel)

      Teex.Add("100     ")  'Bindemittelkonzentration in Schwarzpaste
      '
      Teex.Add("1       ")  'spez. Gewicht Schwarzpigment
      '
      '
      '
      '
      Teex.Add("1       ")  'Menge Farbpaste
      'uebstr(1, 12) = "1.000000"  'Menge Schwarzpigment (rein)
      'uebstr(2, 12) = "1.000000"  'Menge Bindemittel in Schwarzpaste
      'uebstr(3, 12) = "1.000000"  'Menge Loesemittel in Schwarzpigment

      Teex.Add("1       ")  'Volumen Farbpaste
      'uebstr(1, 13) = "1.000000"  'Volumen Farbpigment (rein)
      'uebstr(2, 13) = "1.000000"  'Volumen Bindemittel in Farbpaste
      'uebstr(3, 13) = "1.000000"  'Volumen Loesemittel in Farbpaste

      Teex.Add("100     ")  'Pigmentgewichtskonzentration Farbpigment    (ohne Loesemittel)
      'uebstr(1, 14) = "100.0000"  'Pigmentkonzentration Farbpigment            (mit Loesemittel)
      'uebstr(2, 14) = "100.0000"  'Pigmentvolumenkonzentration Farbpigment     (ohne Loesemittel)
      'uebstr(3, 14) = "100.0000"  'Pigmentkonzentration (Volumen) Farbpigment  (mit Loesemittel)

      Teex.Add("100     ")  'Bindemittelkonzentration in Farbpaste
      '
      Teex.Add("1       ")  'spez. Gewicht Farbpigment
      '
      '
      '
      '
      Teex.Add("1       ")  'Menge Bindemittel (zusaetzlich)
      'uebstr(1, 17) = "1.000000"  'Menge Bindemittel (gesamt) d.h. Menge(B)zus + Summe(B-Menge(i))
      'uebstr(2, 17) = "1.000000"  'Menge Loesemittel (zusaetzlich)
      'uebstr(3, 17) = "1.000000"  'Menge Loesemittel (gesamt) d.h. Menge(B)zus + Summe(B-Menge(i))

      Teex.Add("1       ")  'Volumen Bindemittel (zusaetzlich)
      'uebstr(1, 18) = "1.000000"  'Volumen Bindemittel (gesamt)d.h. Volumen(B)zus + Summe(B-Volum(i))(gesamt)
      'uebstr(2, 18) = "1.000000"  'Volumen Loesmittel (zusaetzlich)
      'uebstr(3, 18) = "1.000000"  'Volumen Loesemittel (gesamt)d.h. Volumen(B)zus + Summe(B-Volum(i))(gesamt)

      Teex.Add("100     ")  'Bindemittelkonzentration in Farbpaste
      '
      Teex.Add("1       ")  'spez. Gewicht Farbpigment
      '

      Teex.Add("1       ")  'spez. Gewicht Loesemittel
      '
      '
      '
      Teex.Add("1       ")   'Flaeche
      'uebstr(1, 22) = "1.000000"   'Schichtdicke
      '
      '
      '
      Teex.Add("100     ")   'gesamte Gewichtskonzentration (fest) PGK
      'uebstr(1, 23) = "100.0000"   'gesamte Gewichtskonzentration (Einwaage) PK(G)
      'uebstr(2, 23) = "100.0000"   'gesamte Volumenkonzentration (fest) PVK
      'uebstr(3, 23) = "100.0000"   'gesamte Volumenkonzentration (Einwaage) PK(V)
      '
      '
      '
      Teex.Add("1       ")   'Einwaagemenge (gesamt)
      'uebstr(1, 24) = "1.000000"   'Pigmentmenge (gesamt)
      'uebstr(2, 24) = "1.000000"   'Feststoffmenge (gesamt)
      '
      '
      '
      Teex.Add("1       ")   'Einwaagevolumen (gesamt)
      'uebstr(1, 25) = "1.000000"   'Pigmentvolumen (gesamt)
      'uebstr(2, 25) = "1.000000"   'Feststoffvolumen (gesamt)
      Teex.Add("0       ")
      Teex.Add(Space(1))
      Teex.Add(Space(1))
    End If
    UebStr = Teex
  End Function


  '
  Private Sub AnzMess(ByRef MethID As Integer, ByRef nreiwmax As Integer)
    Dim i As Integer
    If MethID = 49 Or MethID = 48 Then
      'Mittelwertbildung
      Exit Sub
    End If
    lblANZ.Visible = True
    cboANZ.Visible = True
    cboANZ.Items.Clear()
    If MethID = 22 Then
      For i = 0 To 5
        cboANZ.Items.Add(CStr(i + 1))
      Next i
    Else
      For i = 0 To 19
        cboANZ.Items.Add(CStr(i + 1))
      Next i
    End If
    cboANZ.SelectedIndex = nreiwmax - 1
  End Sub

  Sub EnblRef()
    Dim i As Integer
    '
    'cmdREF(0)  Reflexionswerte aktivieren/deakt.
    'je nachdem, ob bereits alle anderen R-Werte (Untergr. usw) vorhanden
    '
    '
    btnREF(0).Enabled = True
    For i = 1 To btnREF.Count - 1
      If lblREF(i).Visible Then
        If Trim(lblREF(i).Text) = "" Then
          btnREF(0).Enabled = False
        End If
      End If
    Next i
    '
  End Sub

  Sub GetGridRwert(ByRef Ikenn As Integer, ByRef MethID As Integer, ByRef RwrRwerte As RefValuesGrp, ByRef ier As Integer)
    Dim Rrow As Integer
    Dim Ccol As Integer
    Dim ID As Integer
    Dim jw As Integer
    Dim js As Integer
    Dim KeyRGrp As String
    Dim i As Integer
    '
    '
    'Ikenn=0 alle Spalten
    'Ikenn=1 1. Spalte
    'Ikenn=2 2. Spalte
    ier = 0
    RwrRwerte("W").clear()
    RwrRwerte("S").clear()

    '
    'Reflexionswerte von Tbl_rwert
    'Mengen, cart usw. von Grid
    '

    jw = 0
    js = 0
    Rrow = 0
    Ccol = 0
    Do
      If Rrow = GrdArray.Rows.Count Then
        Exit Do
      End If
      If IsDBNull(GrdArray.Rows(Rrow)(Ccol)) OrElse Not IsNumeric(GrdArray.Rows(Rrow)(Ccol)) Then
        ier = 2984
        MessageBox.Show(Texxt(921 + Ccol) & Space(1) & Texxt(2984), Texxt(2004))
        Exit Do
      End If
      k = Ccol
      If Ikenn = 0 Or k = Ikenn - 1 Then
        If IsNumeric(GrdArray.Rows(Rrow)(Ccol)) Then
          ID = GrdArray.Rows(Rrow)(Ccol)
          If Ccol = 0 And GrdArray.Rows(Rrow)(colm).substring(3, 1) = "W" Then
            jw = jw + 1
            KeyRGrp = "W"
            i = jw - 1
          Else
            js = js + 1
            KeyRGrp = "S"
            i = js - 1
          End If
          RwrRwerte(KeyRGrp).Add(KeyRe(i), New RefValue)
          ReadWrite.ReadRwert(ID, RwrRwerte(KeyRGrp)(i), ier)
          RwrRwerte(KeyRGrp)(i).Nr = i
        End If
      End If
      If icore = 1 Then
        Rrow = Rrow + 1
        Ccol = 0
      ElseIf icore = 2 Then
        If k = 0 Then
          Ccol = 1
        Else
          Ccol = 0
          Rrow = Rrow + 1
        End If
      End If

    Loop

    If RwrRwerte("W").Count + RwrRwerte("S").Count = 0 Then
      MsgBox(Texxt(2970), 0)
      ier = 2970
      Exit Sub
    End If
    '
  End Sub

  Sub GridStart(ByRef ier As Integer)
    Dim i As Integer
    Dim Grirwe As Integer
    Dim Griwrt As Integer
    Dim GriBan As Integer
    Dim IndBanum(1) As String
    ier = 0
    IndBanum(0) = ""
    IndBanum(1) = ""
    change = False
    Grirwe = 200
    GriBan = 100
    Griwrt = 100
    '
    GrdArray.Clear()
    GrdArray.Columns.Clear()
    '
    For i = 0 To colm

      If i < icore Then
        '
        'ID-Nummern
        '
        '
        '
        GrdArray.Columns.Add(KeyRe(i), GetType(Integer))
        With TDBGrid.Columns(KeyRe(i))
          .ReadOnly = True
          .Visible = False
          .Visible = False

        End With
        '
        '
        'R-Wert-Namen
        '
      ElseIf i >= icore And i <= 2 * icore - 1 Then
        GrdArray.Columns.Add(KeyRe(i), GetType(String))
        With TDBGrid.Columns(KeyRe(i))
          .Width = Grirwe
          .Visible = True
          .ReadOnly = True
          .HeaderText = TexKt(20000 + Kpogrd(i))
        End With
        '
        '
      ElseIf i = 2 * icore Then
        '
        'TYP/PROBE ("X" oder " ")
        '
        GrdArray.Columns.Add(KeyRe(i), GetType(String))
        With TDBGrid.Columns(KeyRe(i))
          .Visible = True
          '.Font.Size = 10
          .Width = 15
          .HeaderText = ""
          .ReadOnly = True
          If MenueParam.MethID = 28 Or MenueParam.MethID = 29 Or MenueParam.MethID = 48 Or MenueParam.MethID = 49 Then
            '.Frozen = True
          End If
        End With

        '
        '
      ElseIf i >= 2 * icore + 1 And i <= 3 * icore Then
        '
        '
        '
        'BA-Nummer
        '
        '
        '
        GrdArray.Columns.Add(KeyRe(i), GetType(String))
        With TDBGrid.Columns(KeyRe(i))
          .Width = GriBan
          If i = 2 * icore + 1 Then IndBanum(0) = KeyRe(i)
          If i = 3 * icore Then IndBanum(1) = KeyRe(i)
          .HeaderText = TexKt(20000 + Kpogrd(i))
          .Visible = False
        End With
        '
        '
        'CAMP-Felder
        '
      ElseIf i >= 3 * icore + 1 And i <= colm - 1 Then
        GrdArray.Columns.Add(KeyRe(i), GetType(Single))
        With TDBGrid.Columns(KeyRe(i))
          .Width = Griwrt
          .Visible = True
          .HeaderText = TexKt(20000 + Kpogrd(i))
          .DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
          .DefaultCellStyle.Format = "####0.00"
        End With

        '
      ElseIf i = colm Then
        '
        'CART-FELD
        '
        GrdArray.Columns.Add(KeyRe(i), GetType(String))
        With TDBGrid.Columns(KeyRe(i))
          .ReadOnly = True
          .Visible = False
        End With
      End If

    Next i
    '
    '
    '
    '

    '
    '
    '
    'Standard-Zeile
    '
    '
    ReDim HlfTex(0 To colm - 1)
    '
    For i = 0 To 3 * icore
      HlfTex(i) = ""
    Next i
    For i = 3 * icore + 1 To colm - 1
      HlfTex(i) = UebStr(ipogrd(i))
    Next i
    '
    '
    'Breite des Gitternetzes berechnen
    '
    '
    '

    gridwidth = 30
    For i = 0 To TDBGrid.Columns.Count - 1
      If TDBGrid.Columns(i).Visible Then
        gridwidth = gridwidth + TDBGrid.Columns(i).Width
      End If
      TDBGrid.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
    Next i
    TDBGrid.Width = gridwidth
    TDBGrid.AllowUserToAddRows = False
    TDBGrid.AllowUserToAddRows = False
    TDBGrid.AllowUserToOrderColumns = False
    TDBGrid.SelectionMode = DataGridViewSelectionMode.RowHeaderSelect

    TDBGrid.AlternatingRowsDefaultCellStyle.BackColor = Color.Azure
    TDBGrid.DefaultCellStyle.BackColor = Color.Beige
    TDBGrid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None
    '
    'Öffnen des Layoutfiles
    '
    '
    '
    Try


      'TDBGrid.LayoutFileName = GridFileName()
      'TDBTab.LayoutName = "QUALKONTROLLE" & KeyRe(CInt(MenueParam.MethID))
      If BitWrt(1, MenueParam.User.Drum) Then
        '
        'Layout mit gespeichertem Layout überschreiben
        '
        '

        Try
          'TDBTab.LoadLayout()
        Catch ex As Exception




        End Try
      End If
    Catch ex As Exception

    End Try
    '
    '
    'Visible für BA-Nummer
    '
    '
    For i = 0 To 1
      If IndBanum(i) <> "" Then
        If Not BitWrt(17, MenueParam.User.Writ) Then
          TDBGrid.Columns(IndBanum(i)).Visible = False
        Else
          TDBGrid.Columns(IndBanum(i)).Visible = True
        End If
      End If
    Next i
    '
    '
    'Eingabe für CAMP-Felder gesperrt/nicht gesperrt
    '
    '
    '
    For i = 3 * icore + 1 To colm - 1
      With TDBGrid.Columns(i)
        If BitWrt(12, MenueParam.User.Writ) Then
          .ReadOnly = False
        Else
          .ReadOnly = True
        End If
      End With
    Next i
    TDBGrid.Columns(colm).ReadOnly = True
    change = True
  End Sub


  Sub StartMeth(ier)
    Dim i As Integer
    Me.Text = Texxt(1800 + MenueParam.MethID) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"

    ier = 0
    '
    'Anzahl Moeglichkeiten fuer Ueberschriften
    '
    IanGrd(0) = 1                'Reflexionswerte (weiss)
    IanGrd(1) = 1                'Reflexionswerte (schwarz)
    IanGrd(2) = 4                'Mengenarten Weisspigment
    IanGrd(3) = 4                'Volumenarten Weisspigment
    IanGrd(4) = 4                'Konzentrationsarten Weisspigment (PGK,PK(G),PVK,PK(V))
    IanGrd(5) = 1                'Konzentration des Bindemittels in Weisspaste
    IanGrd(6) = 1                'spez.Gewicht Weisspigment

    IanGrd(7) = 4                'Mengenarten Schwarzpigment
    IanGrd(8) = 4                'Volumenarten Schwarzpigment
    IanGrd(9) = 4                'Konzentrationsarten Schwarzpigment (PGK,PK(G),PVK,PK(V))
    IanGrd(10) = 1                'Konzentration des Bindemittels in Schwarzpaste
    IanGrd(11) = 1                'spez.Gewicht Schwarzpigment

    IanGrd(12) = 4                'Mengenarten Farbpigment
    IanGrd(13) = 4                'Volumenarten Farbpigment
    IanGrd(14) = 4                'Konzentrationsarten Farbpigment (PGK,PK(G),PVK,PK(V))
    IanGrd(15) = 1                'Konzentration des Bindemittels in Farbpaste
    IanGrd(16) = 1                'spez.Gewicht Farbpigment

    IanGrd(17) = 4                'Mengenarten Binde-/Loesemittel
    IanGrd(18) = 4                'Volumenarten Binde-/Loesemittel
    IanGrd(19) = 1                'Bindemittelkonzentration fuer zusaetzliches Bindemittel
    IanGrd(20) = 1                'spezifisches Gewicht Bindemittel
    IanGrd(21) = 1                'spez.Gewicht Loesemittel

    IanGrd(22) = 2                'Flaeche bzw. Schichtdicke

    IanGrd(23) = 4                'Gesamtgewichts(-volumen)konzentrationen

    IanGrd(24) = 3                'Gesamtmenge bzw.  Feststoffmenge

    IanGrd(25) = 3                'Gesamtvolumen bzw.  Feststoffvolumen

    IanGrd(26) = 1
    'For i = 0 To ngrd
    '    For j = 0 To 3
    '        k = 4 * i + j
    '        UebGrd(j, i) = texKt(20000 + k)
    '    Next j
    'Next i
    For i = 0 To Nstr
      NstrRef(i) = Texxt(1200 + i)
    Next i
  End Sub

  Private Sub cboANZ_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboANZ.SelectedIndexChanged
    ireiwied = cboANZ.SelectedIndex + 1
  End Sub


  Private Sub cboBWE_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboBWE.SelectedIndexChanged
    If Not cboBWE.Enabled Then Exit Sub
    MenueParam.Menue.BwertID = cboBWE.SelectedIndex
  End Sub

  Private Sub cboFST_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboFST.SelectedIndexChanged
    If Not cboFST.Enabled Then Exit Sub
    MenueParam.Menue.FstaeID = cboFST.SelectedIndex
    If cboFST.SelectedIndex = 6 Then
      lblWEL.Visible = True
      cboWEL.Visible = True
    Else
      lblWEL.Visible = False
      cboWEL.Visible = False
    End If
  End Sub

  Private Sub cboGLZ_Enter(sender As Object, e As System.EventArgs) Handles cboGLZ.Enter, cboINR.Enter, cboIND.Enter
    sender.tag = sender.text
  End Sub

  Private Sub cboGLZ_Textchanged(sender As Object, e As System.EventArgs) Handles cboGLZ.TextChanged
    If Not IsNumeric(cboGLZ.Text) Then
      cboGLZ.Text = cboGLZ.Tag
    End If
    If IsNumeric(cboGLZ.Text) Then
      For i = 0 To Winkel.Km - 1
        Winkel(i).GK(0) = Singl(cboGLZ.Text) * Winkel(i).Iglz
      Next
      If MenueParam.Messg.GlIn Then
        cboINR.Tag = cboGLZ.Text
        cboINR.Text = cboGLZ.Text
      End If
    End If
  End Sub

  Private Sub cboINR_Textchanged(sender As Object, e As System.EventArgs) Handles cboINR.TextChanged
    If Not IsNumeric(cboINR.Text) Then
      cboINR.Text = cboINR.Tag
    End If
    If IsNumeric(cboINR.Text) Then
      For i = 0 To Winkel.Km - 1
        Winkel(i).GK(1) = Singl(cboINR.Text)
      Next
    End If
  End Sub


  Private Sub cboIND_Textchanged(sender As Object, e As System.EventArgs) Handles cboIND.TextChanged
    If Not IsNumeric(cboIND.Text) Then
      cboIND.Text = cboIND.Tag
    End If
    If IsNumeric(cboIND.Text) Then
      For i = 0 To Winkel.Km - 1
        Winkel(i).GK(2) = Singl(cboIND.Text)
      Next
    End If
  End Sub

  Private Sub cboKDE_Textchanged(sender As Object, e As System.EventArgs) Handles txtKDE.TextChanged
    MenueParam.Menue.KdeWS = Singl(txtKDE.Text)
  End Sub

  Private Sub chkABS_Selectedindexchanged(sender As Object, e As System.EventArgs) Handles chkABS.CheckStateChanged
    MenueParam.Menue.AbsID = chkABS.CheckState
  End Sub

  Private Sub chkPARAM_Click(sender As Object, e As System.EventArgs) Handles chkPARAM.Click
    Dim i As Integer
    Dim j As Integer
    Select Case chkPARAM.Checked
      Case 0
        TDBGrid.Visible = True
        TDBParameter.Visible = False
        splQual.Panel1.Visible = True
        splParameter.Panel1.Visible = True
        For i = 0 To MenueParam.ParaMerks.ParamCount - 1
          For j = 0 To TDBParameter.DataSource.Count - 1
            If MenueParam.ParaMerks(i).ID = TDBParameter.DataSource.item(j)("PARAM_ID") Then
              MenueParam.ParaMerks(i).Lbez = TDBParameter.DataSource.item(j)("PARAM_LBEZ")
              MenueParam.ParaMerks(i).Kbez = TDBParameter.DataSource.item(j)("PARAM_KBEZ")
              MenueParam.ParaMerks(i).Wert = TDBParameter.DataSource.item(j)("PARAM_WERT")
              MenueParam.ParaMerks(i).BezWrt = TDBParameter.DataSource.item(j)("PARAM_BEZWRT")
            End If
          Next
        Next i
      Case 1
        TDBGrid.Visible = False
        TDBParameter.Visible = True
        splQual.Panel1.Visible = False
        splParameter.Panel1.Visible = False

    End Select
  End Sub

  Private Sub btnBWE_Click(sender As Object, e As System.EventArgs) Handles btnBWE.Click

    '
    ier = 0
    GrpRwerte("W").clear()
    GrpRwerte("S").clear()

    Call GetGridRwert(1, MenueParam.MethID, GrpRwerte, ier)
    If ier <> 0 Then
      Exit Sub
    End If
    '

    MenueParam.Menue.BwertID = quali.BwertIdent(Winkel, GrpRwerte("W"), ier)
    If ier <> 0 Then
      Exit Sub
    End If
    cboBWE.SelectedIndex = MenueParam.Menue.BwertID
    '
    '
    'B-Wert
    '
    '
  End Sub


  Private Sub btnLOE_Click(sender As Object, e As System.EventArgs) Handles btnLOE.Click
    rowa = 0
    cola = 1
    If IsNothing(GrdArray) Then Exit Sub
    GrdArray.Clear()
    irei = 0
    nreiwied = 0
    nreiwmax = 0
    Call NameRwe()
    Call EnblRef()
    MenueParam.Menue.KdeWS = WKDE
    MenueParam.Menue.BwertID = LBwert
    txtKDE.Text = MenueParam.Menue.KdeWS
    cboBWE.Text = cboBWE.Items(LBwert)
    txtKOM.Text = ""
  End Sub

  Private Sub btnREF_Click(sender As Object, e As System.EventArgs) Handles _
    btnREF_0.Click, btnREF_1.Click, btnREF_2.Click, btnREF_3.Click, btnREF_4.Click, btnREF_5.Click
    Dim Hilf As String
    Dim index As Integer
    If IsNothing(GrdArray) Then Exit Sub
    index = CInt(sender.name.substring(7, 1))
    chkPARAM.CheckState = 0
    RcmdRef = index
    If index = 0 Then
      '
      'normale Messung
      '
      '
      GetPutReflex.Iarch = 0
      rowa = GrdArray.Rows.Count
      If rowa < 0 Then Exit Sub
      cola = 1
      If icore = 2 Then
        If rowa > 1 Then
          Hilf = GrdArray.Rows(rowa - 1)(colm)
          If Len(Hilf) < 8 Then
            rowa = rowa - 1
            cola = 2
          End If
        End If
      End If
      If ireimeth(2 * icore) < 0 Then
        If GrdArray.Rows.Count = rowa Then
          If rowa = 0 Then
            irei = 0
          Else
            irei = icore
          End If
        Else
          If rowa = 0 Then
            irei = 1
          Else
            irei = icore + 1
          End If
        End If
        nreiwied = 0
        Call NameRwe()
      End If
      '

    Else
      '
      'Untergründe;Weißpigment;Schwarzpigment;Graupigment
      '
      '
      '
      GetPutReflex.Iarch = 1
      GetPutReflex.Captext = Texxt(1199 + index)

    End If
    GetPutReflex.Messrefel = RefWert

    GetPutReflex.Retr = -1
    GetPutReflex.ReflexWerte(True)
  End Sub
  Private Sub btnVER_Click(sender As Object, e As System.EventArgs) Handles btnVER.Click
    Dim IuntID(0 To 2) As Integer
    Dim Nqu As Integer
    Dim Ianwsg As Integer
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim Aka As Single
    Dim CM() As Single
    Dim iee As Integer
    Dim ErrCode As Integer
    Dim NoMerks As Boolean
    chkPARAM.CheckState = 1
    chkPARAM.CheckState = 0
    NoMerks = True
    GrdArray.AcceptChanges()
    '
    '
    FarbWerte.Kommentar = txtKOM.Text
    If Not BitWrt(15, MenueParam.User.Writ) Or IsNothing(cboMISCEL(0).SelectedItem) Then
      FarbWerte.PruefMittel = ""
      FarbWerte.PruefID = 0
    Else
      FarbWerte.PruefMittel = cboMISCEL(0).SelectedItem.row(2)
      If IsNumeric(cboMISCEL(0).SelectedValue) Then
        FarbWerte.PruefID = CInt(cboMISCEL(0).SelectedValue)
      Else
        FarbWerte.PruefID = 0
      End If
    End If
    If Not BitWrt(16, MenueParam.User.Writ) Or IsNothing(cboMISCEL(1).SelectedItem) Then
      FarbWerte.ProduktArt = ""
      FarbWerte.ProArtID = 0
    Else
      FarbWerte.ProduktArt = cboMISCEL(1).SelectedItem.row(2)
      If IsNumeric(cboMISCEL(1).SelectedValue) Then
        FarbWerte.ProArtID = CInt(cboMISCEL(1).SelectedValue)
      Else
        FarbWerte.ProArtID = 0
      End If
    End If
    '
    '
    MenueParam.Menue.FstaeWel = Singl(cboWEL.Text)
    For i = 0 To FarbWerte.Count - 1
      If FarbWerte(i).CountMerk > 0 Then
        NoMerks = False
        Exit For
      End If
    Next i
    If NoMerks And Not (MenueParam.MethID = 1 Or MenueParam.MethID = 49) Then
      MsgBox(Texxt(4123))
      Exit Sub
    End If

    btnVER.Enabled = False
    '

    Cursor = Cursors.WaitCursor
    panResult.Visible = False
    PanQual.Visible = False
    lblQCE.Visible = True
    Application.DoEvents()

    '
    'Lösche R-Werte
    '

    For i = 0 To 2
      IuntID(i) = -1
    Next i
    GrpRwerte("W").clear()
    GrpRwerte("S").clear()
    If MenueParam.MethID = 49 Then
      '
      'Als Winkel wird Menueparam.Messg.winkel verwendet
      '
      '
      'Mittelwert
      '
      '
      Call GetGridRwert(0, MenueParam.MethID, GrpRwerte, ier)
      If GrpRwerte("W").Count = 0 Then GoTo QceWeiter
      If GrpRwerte("W")(0).RefKurv.Count = 0 Then GoTo QceWeiter
      Aka = 1.0# - MenueParam.Messg.Ka
      Nqu = GrpRwerte("W").Count
      If Nqu = 0 Then Exit Sub
      ReDim CM(Nqu - 1)
      For i = 0 To Nqu - 1
        CM(i) = 1.0 / (1.0 * Nqu)
      Next
      Call quali.RefMit(Aka, Winkel, CM, GrpRwerte("W"), RefWert, ier)
      If ier <> 0 Then GoTo QceWeiter
      RefWert.Name = CStr(ReadWrite.RefMax(MenueParam.TableRwert, MenueParam.Messg.MessgRwID) + 1)
      RefWert.Bem = ""
      RefWert.Banum = ""
      RefWert.Iami = 1
      For i = 0 To Winkel.Km - 1
        RefWert.De(i) = 0.0#
      Next i
      RefWert.DatTim = Now
      RefWert.Cme = MenueParam.Messg.Kenn
      RefWert.Iarch = 0
      RefWert.ReTr = 0
      RefWert.Gid = MenueParam.Messg.RwrtGID
      RefWert.MessgID = MenueParam.MessgID
      RefWert.Name = InputBox(Texxt(370), Texxt(2000), RefWert.Name)
      If RefWert.Name <> "" Then
        If Handlerwert.MeldSpeiAllRwrt(False) Then
          RefWert.Gid = Handlerwert.cboGRP.SelectedValue
          Call ReadWrite.WriteRwert(ID, RefWert, ier)
        End If
      End If
      Cursor = Cursors.Default
      PanQual.Visible = True
      panResult.Visible = False
      btnVER.Enabled = True
      lblQCE.Visible = False

      Exit Sub
    End If
    '
    '
    'Lösche Parameter
    '
    '
    '

    For i = 0 To FarbWerte.Count - 1
      FarbWerte(i).clear()
    Next i
    '
    '
    '
    'Untergründe in Ordnung ?
    For i = 0 To 2
      KeyR = GrpRwerte.RwArt(i)
      IuntID(i) = GrpRwerte(KeyR).RefUnt.ID
    Next i
    For i = 0 To 2
      If Not GrpRwerte(i).RefUnt.IVoNa Then
        If i < 2 AndAlso (lblREF(i + 1).Visible And IuntID(i) < 0) Then
          MsgBox(Texxt(355 + i) & Space(1) & Texxt(2984), 0)
          Exit Sub
        End If
        If lblREF(i + 3).Visible And IuntID(i) < 0 Then
          MsgBox(Texxt(351 + i + 1) & Space(1) & Texxt(2984), 0)
          Exit Sub
        End If
      End If
    Next i

    '
    '
    '
    '
    Call GetGridRwert(0, MenueParam.MethID, GrpRwerte, ier)
    '
    If ier > 0 Then GoTo QceWeiter
    '


    Call UpdateQu(MenueParam.MethID, IuntID, GrpRwerte, ier)

    If ier > 0 Then GoTo QceWeiter
    Nqu = GrpRwerte("W").Count
    If Nqu = 0 Then Nqu = GrpRwerte("S").Count
    If icore = 2 And GrpRwerte("W").Count <> GrpRwerte("S").Count Then
      MsgBox(Texxt(2983), 0)
      PanQual.Visible = True
      panResult.Visible = False
      Cursor = Cursors.Default
      btnVER.Enabled = True
      Exit Sub
    End If
    Select Case MenueParam.MethID
      Case 9, 10
        '
        '
        'Prüfen auf richtige Reihenfolge für Rub-Out oder Disp.-Härte
        '
        If (Nqu Mod 2) = 1 Then
          ErrCode = 2975
          GoTo ErrReihen
        End If
        i = -1
        Do While i <= GrpRwerte(0).Count
          i = i + 1
          If i >= GrpRwerte(0).Count Then
            Exit Do
          End If
          If Mid(GrpRwerte(0)(i).QuControl.Cart, 1, 2) = "@T" Then
            If i = GrpRwerte(0).Count - 1 Then
              ErrCode = 2952
              GoTo ErrReihen
            Else
              i = i + 1
              If i >= GrpRwerte(0).Count Then
                ErrCode = 2952
                GoTo ErrReihen
              End If
              If Mid(GrpRwerte(0)(i).QuControl.Cart, 1, 2) <> "AT" Then
                ErrCode = 2952
                GoTo ErrReihen
              End If
            End If
          ElseIf Mid(GrpRwerte(0)(i).QuControl.Cart, 1, 2) = "@P" Then
            If i = GrpRwerte(0).Count - 1 Then
              ErrCode = 2952
              GoTo ErrReihen
            Else
              i = i + 1
              If i >= GrpRwerte(0).Count Then
                ErrCode = 2952
                GoTo ErrReihen
              End If
              If Mid(GrpRwerte(0)(i).QuControl.Cart, 1, 2) <> "AP" Then
                ErrCode = 2952
                GoTo ErrReihen
              End If
            End If
          End If
        Loop
      Case 11
        '
        '
        'Prüfen auf richtige Reihenfolge für Rub-Out und Disp.-Härte
        '
        If (Nqu Mod 3) = 1 Then
          ErrCode = 2975
          GoTo ErrReihen
        End If
        If (Nqu Mod 3) = 2 Then
          ErrCode = 2976
          GoTo ErrReihen
        End If
        i = -1
        Do While i <= GrpRwerte(0).Count
          i = i + 1
          If i >= GrpRwerte(0).Count Then
            Exit Do
          End If
          If Mid(GrpRwerte(0)(i).QuControl.Cart, 1, 2) = "@T" Then
            If i = GrpRwerte(0).Count - 1 Then
              ErrCode = 2952
              GoTo ErrReihen
            Else
              For j = 1 To 2
                i = i + 1
                If i > GrpRwerte(0).Count - 1 Then
                  ErrCode = 2952
                  GoTo ErrReihen
                End If
                If Mid(GrpRwerte(0)(i).QuControl.Cart, 1, 2) <> Chr(64 + j) & "T" Then
                  ErrCode = 2952
                  GoTo ErrReihen
                End If
              Next j
            End If
          ElseIf Mid(GrpRwerte(0)(i).QuControl.Cart, 1, 2) = "@P" Then
            If i = GrpRwerte(0).Count - 1 Then
              ErrCode = 2952
              GoTo ErrReihen
            Else
              For j = 1 To 2
                i = i + 1
                If i > GrpRwerte(0).Count - 1 Then
                  ErrCode = 2952
                  GoTo ErrReihen
                End If
                If Mid(GrpRwerte(0)(i).QuControl.Cart, 1, 2) <> Chr(64 + j) & "P" Then
                  ErrCode = 2952
                  GoTo ErrReihen
                End If
              Next j
            End If
          End If
        Loop
    End Select
    '
    '
    'Bwert und Art der Farbstärkeberechnung bestimmen;
    '
    '
    '
    '
    'Gruppe Sonstiges
    '
    '
    Select Case MenueParam.MethID
      Case 1
        FarbWerte.Kommentar = txtKOM.Text
        '
        '
        '
        'Ausgabe (Benutzer (Bit=1) oder Normal)
        '
        '
        '
        '
        If BitWrt(4, MenueParam.User.Drum) Then
          Cursor = Cursors.WaitCursor
          Ausgabe.AusgabeQUALITA(FarbWerte, GrpRwerte)
          Cursor = Cursors.Default
        Else
          Ausgabe.Ausgabemerkmale(FarbWerte, GrpRwerte)
        End If
        btnVER.Enabled = True
        Cursor = Cursors.Default
        PanQual.Visible = False
        panResult.Visible = True
        panWinChk.Visible = True
        panNlaChk.Visible = True
        panWinRad.Visible = False
        panNlaRad.Visible = False
        radGPL(1).Checked = True
        'panResult.Visible = False
        btnVOR.Enabled = False
        btnDRU.Enabled = False
        btnGRF.PerformClick()
        Exit Sub
    End Select
    '

    '
    '


    '
    '
    Cursor = Cursors.WaitCursor
    PanQual.Visible = False
    panResult.Visible = False
    lblQCE.Visible = True
    '
    '
    'Parameter für Transmissionberechnung
    '
    '
    If MenueParam.MethID = 16 Or MenueParam.MethID = 8 Then
      Winkel(0).GK(15) = Singl(txtTra.Text)
    End If
    Call quali.CalcQualWerte(Winkel, GrpRwerte, FarbWerte, iee)
    If iee <> 0 Then
      GoTo QceWeiter
    End If

    lblQCE.Visible = False
    ier = iee
    Cursor = Cursors.Default
    If ier <> 0 Then
      GoTo QceWeiter
    End If
    cboBWE.SelectedIndex = MenueParam.Menue.BwertID
    '
    '
    '
    'Prüfen, ob Merkale berechnet wurden
    '
    '
    '
    NoMerks = True
    For Ianwsg = 0 To FarbWerte.Count - 1
      'Anweisungen
      For i = 0 To FarbWerte(Ianwsg).Count - 1
        'Kurven
        For k = 0 To FarbWerte(Ianwsg)(i).Count - 1
          'Lichtarten
          For kw = 0 To FarbWerte(Ianwsg)(i)(k).Count - 1
            'Winkel/Messgeometrien
            For j = 0 To FarbWerte(Ianwsg)(i)(k)(kw).Count - 1
              'Merkmale
              If Not IsDBNull(FarbWerte(Ianwsg)(i)(k)(kw)(j)) AndAlso FarbWerte(Ianwsg)(i)(k)(kw)(j) <> "" Then
                NoMerks = False
                Exit For
              End If
            Next j
          Next kw
        Next k
      Next i
    Next Ianwsg
    If NoMerks Then
      MsgBox(Texxt(4154))
      PanQual.Visible = True
      panResult.Visible = False
      btnVER.Enabled = True
      Exit Sub
    End If
    If ier <> 0 Then GoTo QceWeiter
    '

    ''
    '
    '
    '
    '
    'Ausgabe (Benutzer (Bit=1) oder Normal)
    '
    If BitWrt(4, MenueParam.User.Drum) Then
      Cursor = Cursors.WaitCursor
      Ausgabe.AusgabeQUALITA(FarbWerte, GrpRwerte)
      Cursor = Cursors.Default
    Else
      Ausgabe.Ausgabemerkmale(FarbWerte, GrpRwerte)
    End If
    '
    '
    '
    '**************************************
    '**************************************
    '
    PanQual.Visible = False
    panResult.Visible = True
    panWinChk.Visible = True
    panNlaChk.Visible = True
    panWinRad.Visible = False
    panNlaRad.Visible = False
    '
    radGPL(1).Checked = True
    '

    panResult.Visible = True
    btnVOR.Enabled = True
    btnDRU.Enabled = True
    btnVOR.PerformClick()
    '
    btnUser.Visible = True
    Exit Sub
QceWeiter:
    If ier = -91 Then
      MsgBox("Too much parameters for the calculation of signs")
      MsgBox("COLwinFORTRAN.Calcwerte")
    End If
    If ier = -92 Then
      MsgBox("Error in the parameter class (Wert=Null or ID=-1)")
    End If
    Cursor = Cursors.Default
    PanQual.Visible = True
    panResult.Visible = False
    btnVER.Enabled = True
    lblQCE.Visible = False
    Exit Sub
ErrReihen:
    MsgBox(Texxt(ErrCode), 0)
    PanQual.Visible = True
    panResult.Visible = False
    Cursor = Cursors.Default
    btnVER.Enabled = True


  End Sub





  Sub UpdateQu(ByRef MethID As Integer, ByRef IuntID() As Integer, ByRef RwrRwerte As RefValuesGrp, ByRef ier As Integer)
    Dim camp(0 To 5) As Single
    Dim hlf() As Byte
    Dim ID As Integer
    Dim Rrow As Integer
    Dim Ccol As Integer
    Dim tex As String
    Dim Cart As String
    Dim j As Integer
    Dim KeyRGrp As String
    Dim jw As Integer
    Dim js As Integer
    Dim i As Integer
    Dim QuaRow As DataRow
    ier = 0
    '
    'UPDATE TBL_Quali
    '
    '
    '
    'Abspeichern von Mengen (z.B. Menge Weißpigment u.ä.m.)
    '
    '
    '
    '
    'Worksp.BeginTrans
    '
    '
    '
    '
    '
    '
    'Mengen, cart usw. von Grid
    '

    jw = 0
    js = 0
    Rrow = 0
    Ccol = 0
    Do
      If Rrow = GrdArray.Rows.Count Then
        Exit Do
      End If
      If IsDBNull(GrdArray.Rows(Rrow)(Ccol)) OrElse Not IsNumeric(GrdArray.Rows(Rrow)(Ccol)) Then
        ier = 2984
        MessageBox.Show(Texxt(921 + Ccol) & Space(1) & Texxt(2984), Texxt(2004))
        Exit Do
      End If
      k = Ccol
      If Ccol = 0 And GrdArray.Rows(Rrow)(colm).substring(3, 1) = "W" Then
        jw = jw + 1
        KeyRGrp = "W"
        i = jw - 1
        RwrRwerte(KeyRGrp)(i).kwb = 0
      Else
        js = js + 1
        KeyRGrp = "S"
        i = js - 1
        RwrRwerte(KeyRGrp)(i).kwb = 1
      End If
      RwrRwerte(KeyRGrp)(i).IVoNa = True
      ID = CInt(GrdArray.Rows(Rrow)(Ccol))
      If ID <> RwrRwerte(KeyRGrp)(i).ID Then
        MsgBox("Error R-Wert-ID")
        ier = 1
        Exit Do
      End If
      RwrRwerte(KeyRGrp)(i).QuControl = New QuControls
      Ccol = 2 * icore
      tex = LTrim(GrdArray.Rows(Rrow)(colm))
      Cart = Mid(tex, k * 4 + 1, 4)
      For j = 0 To 4
        camp(j) = 0
        If icamgrd(j, k) > 0 Then
          Ccol = icamgrd(j, k)
          camp(j) = Singl(GrdArray.Rows(Rrow)(Ccol))
        End If
        RwrRwerte(KeyRGrp)(i).QuControl.Camp(j) = camp(j)
      Next j
      RwrRwerte(KeyRGrp)(i).QuControl.MethID = MethID
      RwrRwerte(KeyRGrp)(i).QuControl.IuntID = IuntID(k)
      RwrRwerte(KeyRGrp)(i).QuControl.Cart = Cart
      RwrRwerte(KeyRGrp)(i).Itp = False
      If Mid(RwrRwerte(KeyRGrp)(i).QuControl.Cart, 2, 1) = "T" Then
        RwrRwerte(KeyRGrp)(i).Itp = True
      End If
      Select Case MethID
        Case 1, 2, 3, 17, 18, 19, 20, 24, 25, 26, 27, 48, 49
        Case Else

LblQua:
          hlf = GetBytes(camp)
          ViewQuali.RowFilter = "METH_ID=" & MethID & " AND QUALI_ID=" & ID
          '
          '
          'IuntID = -1 für alle Methoden (15 und mit Weißpigment, Schwarzpigment und Mischung Weiß/Schwarz)
          '
          '
          hlf = GetBytes(camp)
          If ViewQuali.Count > 0 Then
            ViewQuali(0).Item("QUALI_CAMP") = hlf
            ViewQuali(0).Item("QUALI_IUNT") = IuntID(k)
            ViewQuali(0).Item("QUALI_MEDNR") = -1
          Else
            QuaRow = TabQuali.NewRow
            QuaRow("QUALI_ID") = ID
            QuaRow("MESSGRW_ID") = MenueParam.Messg.MessgRwID
            QuaRow("METH_ID") = MethID
            QuaRow("QUALI_CART") = Cart
            QuaRow("QUALI_DATTIM") = Now
            QuaRow("QUALI_CAMP") = hlf
            QuaRow("QUALI_IUNT") = IuntID(k)
            QuaRow("QUALI_MEDNR") = -1
            TabQuali.Rows.Add(QuaRow)
          End If
ErrQua:
          If ier = 1 Then
            'TblQua.Requery()
            GoTo LblQua
          ElseIf ier = -1 Then
            'TblQua.Close()
            Exit Sub
          End If
      End Select

      '
      If icore = 1 Then
        Rrow = Rrow + 1
        Ccol = 0
      ElseIf icore = 2 Then
        If k = 0 Then
          Ccol = 1
        Else
          Ccol = 0
          Rrow = Rrow + 1
        End If
      End If

    Loop
    '
    '
    '
    'Delete TBL_QUALI
    '
    '
    AdaptQuali.Update(TabQuali.Select(Nothing, Nothing, DataViewRowState.Deleted))
    '
    '
    'Insert TBL_QUALI
    '
    '
    AdaptQuali.Update(TabQuali.Select(Nothing, Nothing, DataViewRowState.Added))
    '
    '
    '
    'Update TBL_QUALI
    '
    AdaptQuali.Update(TabQuali.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
    '
    '
    '
    '
    TabQuali.AcceptChanges()
  End Sub


  Private Sub GridMeth(ByRef Ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim kw As Integer
    '
    Ier = 0
    '
    '
    'Positionierungen Qualitaetskontrolle
    '

    '
    ' Spaltenbeschriftung und Spaltenbreite gemaess Pruefmethode
    '
    '
    'Alle controls werden unvisible gesetzt
    '
    '
    btnLOE.Visible = True
    btnLOE.Enabled = True
    txtKOM.Visible = True
    lblKOM.Visible = True
    lblSTB.Visible = True
    lblTYP.Visible = True
    '

    btnREF(0).Visible = True
    btnREF(0).Enabled = False
    btnVER.Visible = True
    btnVER.Enabled = True
    lblSTB.Text = Texxt(1205)
    chkABS.CheckState = MenueParam.Menue.AbsID
    '
    '
    nreiwied = 0
    irei = 0
    nreiwmax = 0
    For i = 0 To nrei
      ireimeth(i) = 0
    Next i
    '
    '
    '
    'ipogrd Adreese in Uebstr für Standardwerte
    '
    '
    '
    'Kpogrd Adresse in Texte-Datei ab TEXT_ID=1200
    '
    '
    '
    '
    For i = 0 To ngrd
      ipogrd(i) = 0
      Kpogrd(i) = 0
    Next i
    'rnreimeth
    For j = 0 To 4
      icamgrd(j, 0) = 0
      icamgrd(j, 1) = 0
    Next j
    MenueParam.Messg.MeArtLock = -1
    '
    Call InBwert()
    '
    '
    MenueParam.Menue.IDaeArt = 0

    Select Case MenueParam.MethID

      'Methoden abpruefen;Ueberschriften festlegen;
      'Controls aktivieren/deaktivieren
      'Spaltenbreite festlegen
      Case 1, 2, 17, 18, 24, 25, 26, 27
        '
        'Farbabstaende Farbkoordinaten, R-Werte messen/lesen

        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        '
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 5
        ireimeth(1) = 6
        ireimeth(2) = -1
        '
        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 1
        colm = 4
        '
        '
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        '
        cboGLZ.Text = Winkel(0).GK(0)
        '
        'Absolutwerte Bezug
        '
        chkABS.Visible = True

        '
      Case 3
        '
        'Farbabstaende Farbkoordinaten mit weissem und schwarzem Untergrund


        ipogrd(0) = 30
        ipogrd(1) = 30
        ipogrd(2) = 0
        ipogrd(3) = 1
        ipogrd(4) = 30
        Kpogrd(0) = 120
        Kpogrd(1) = 120
        Kpogrd(2) = 0
        Kpogrd(3) = 4
        Kpogrd(4) = 120
        '
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 7
        ireimeth(1) = 9
        ireimeth(2) = 8
        ireimeth(3) = 10
        ireimeth(4) = -2

        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 2
        colm = 7
        '
        '
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        '
        chkABS.Visible = True
        '
      Case 21
        'Farbabstaende Farbkoordinaten, nur Messungen über schwarz

        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        Kpogrd(0) = 120
        Kpogrd(1) = 4
        Kpogrd(2) = 120
        '
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 9
        ireimeth(1) = 10
        ireimeth(2) = -1
        '
        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 1
        colm = 4
        '
        '
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        '
        cboGLZ.Text = Winkel(0).GK(0)
        '
        'Absolutwerte Bezug
        '
        chkABS.Visible = True


      Case 4
        '
        'Farbstaerke (deckend)
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        ipogrd(4) = 2
        ipogrd(5) = 4
        ipogrd(6) = 12
        ipogrd(7) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        Kpogrd(4) = 8
        Kpogrd(5) = 16
        Kpogrd(6) = 48
        Kpogrd(7) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 11
        ireimeth(1) = 12
        ireimeth(2) = -1

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 4
        icamgrd(2, 0) = 5
        icamgrd(3, 0) = 6
        icamgrd(4, 0) = 7

        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 1
        colm = 8

        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True
        MenueParam.Messg.MeArtLock = 0

        '
      Case 23
        '
        'Farbstaerke (deckend) mit Purtonmessung als zweite Messung
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 30
        ipogrd(2) = 0
        ipogrd(3) = 1
        ipogrd(4) = 30
        ipogrd(7) = 2
        ipogrd(8) = 4
        ipogrd(9) = 12
        ipogrd(10) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 120
        Kpogrd(2) = 0
        Kpogrd(3) = 1
        Kpogrd(4) = 120
        Kpogrd(7) = 8
        Kpogrd(8) = 16
        Kpogrd(9) = 48
        Kpogrd(10) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 11
        ireimeth(1) = 39
        ireimeth(2) = 12
        ireimeth(3) = 40
        ireimeth(4) = -2
        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 7
        icamgrd(2, 0) = 8
        icamgrd(3, 0) = 9
        icamgrd(4, 0) = 10
        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 2
        colm = 11

        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True
        MenueParam.Messg.MeArtLock = 0

        '

      Case 5

        '
        'Farbstaerke (transparent)
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        ipogrd(4) = 12
        ipogrd(5) = 22
        ipogrd(6) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        Kpogrd(4) = 48
        Kpogrd(5) = 88
        Kpogrd(6) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 11
        ireimeth(1) = 12
        ireimeth(2) = -1

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 4
        icamgrd(2, 0) = 5
        icamgrd(3, 0) = 6

        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 1
        colm = 7
        '
        '
        '
        'Festlegen der sichtbaren controls
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True
        btnREF(1).Visible = True
        btnREF(1).Enabled = True
        lblREF(1).Visible = True
        '
      Case 6
        '
        'Farbstaerke (Textil)
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        ipogrd(4) = 24
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        Kpogrd(4) = 96
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 11
        ireimeth(1) = 12
        ireimeth(2) = -1

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 4

        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 1
        colm = 5
        '
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True
        btnREF(1).Visible = True
        btnREF(1).Enabled = True
        lblREF(1).Visible = True
        MenueParam.Messg.MeArtLock = 0


        '
      Case 7
        '
        'Farbstaerke (deckend) Weissverschnitt+Schwarzverschnitt
        '   Call inbwert '
        '
        MenueParam.Menue.IDaeArt = 1
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 30
        ipogrd(2) = 0
        ipogrd(3) = 1
        ipogrd(4) = 30
        ipogrd(7) = 2
        ipogrd(8) = 4
        ipogrd(9) = 12
        ipogrd(10) = 14
        ipogrd(11) = 7
        ipogrd(12) = 9
        ipogrd(13) = 12
        ipogrd(14) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 120
        Kpogrd(2) = 0
        Kpogrd(3) = 4
        Kpogrd(4) = 120
        Kpogrd(7) = 8
        Kpogrd(8) = 16
        Kpogrd(9) = 48
        Kpogrd(10) = 56
        Kpogrd(11) = 28
        Kpogrd(12) = 36
        Kpogrd(13) = 48
        Kpogrd(14) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 7
        ireimeth(1) = 9
        ireimeth(2) = 8
        ireimeth(3) = 10
        ireimeth(4) = -2

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        '
        '
        icamgrd(1, 0) = 7
        icamgrd(2, 0) = 8
        icamgrd(3, 0) = 9
        icamgrd(4, 0) = 10
        icamgrd(1, 1) = 11
        icamgrd(2, 1) = 12
        icamgrd(3, 1) = 13
        icamgrd(4, 1) = 14



        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 2
        colm = 15
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True

        lblGRA.Visible = True
        txtGRA.Visible = True
        txtGRA.Text = CStr(MenueParam.Menue.VerWS)

        '
        'Weißpigment
        '
        btnREF(3).Visible = True
        btnREF(3).Enabled = True
        lblREF(3).Visible = True
        '
        'Schwarzpigment
        '
        btnREF(4).Visible = True
        btnREF(4).Enabled = True
        lblREF(4).Visible = True
        '
        'Graupigment
        '
        btnREF(5).Visible = True
        btnREF(5).Enabled = True
        lblREF(5).Visible = True
        MenueParam.Messg.MeArtLock = 0


      Case 8
        '
        MenueParam.Menue.IDaeArt = 1
        '
        'Farbstaerke (transparent) ueber Untergrund weiss + ~ schwarz
        '   Call inbwert '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 30
        ipogrd(2) = 0
        ipogrd(3) = 1
        ipogrd(4) = 30
        ipogrd(7) = 12
        ipogrd(8) = 22
        ipogrd(9) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 120
        Kpogrd(2) = 0
        Kpogrd(3) = 4
        Kpogrd(4) = 120
        Kpogrd(7) = 48
        Kpogrd(8) = 88
        Kpogrd(9) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 7
        ireimeth(1) = 9
        ireimeth(2) = 8
        ireimeth(3) = 10
        ireimeth(4) = -2

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 7
        icamgrd(2, 0) = 8
        icamgrd(3, 0) = 9
        icamgrd(1, 1) = 7
        icamgrd(2, 1) = 8
        icamgrd(3, 1) = 9



        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 2
        colm = 10
        lblTRA.Visible = True
        txtTra.Visible = True
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True

        '
        'weißer Untergrund
        '
        btnREF(1).Visible = True
        btnREF(1).Enabled = True
        lblREF(1).Visible = True
        '
        'schwarzer Untergrund
        '
        btnREF(2).Visible = True
        btnREF(2).Enabled = True
        lblREF(2).Visible = True


      Case 9
        '
        'Farbstärke deckend (Rub-Out)

        '
        '
        '
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        ipogrd(4) = 2
        ipogrd(5) = 4
        ipogrd(6) = 12
        ipogrd(7) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        Kpogrd(4) = 8
        Kpogrd(5) = 16
        Kpogrd(6) = 48
        Kpogrd(7) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 11
        ireimeth(1) = 16
        ireimeth(2) = 12
        ireimeth(3) = 22
        ireimeth(4) = -2

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 4
        icamgrd(2, 0) = 5
        icamgrd(3, 0) = 6
        icamgrd(4, 0) = 7


        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 1
        colm = 8
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True
        MenueParam.Messg.MeArtLock = 0


      Case 10
        '
        'Farbstärke deckend (Disp.-Härte)
        '
        '
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        ipogrd(4) = 2
        ipogrd(5) = 4
        ipogrd(6) = 12
        ipogrd(7) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        Kpogrd(4) = 8
        Kpogrd(5) = 16
        Kpogrd(6) = 48
        Kpogrd(7) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 15
        ireimeth(1) = 18
        ireimeth(2) = 21
        ireimeth(3) = 24

        ireimeth(4) = -2

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 4
        icamgrd(2, 0) = 5
        icamgrd(3, 0) = 6
        icamgrd(4, 0) = 7


        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 1
        colm = 8
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True
        MenueParam.Messg.MeArtLock = 0


        '
      Case 11
        '
        'Farbstärke deckend (Rub-Out + Disp.-Härte)
        '

        '
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        ipogrd(4) = 2
        ipogrd(5) = 4
        ipogrd(6) = 12
        ipogrd(7) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        Kpogrd(4) = 8
        Kpogrd(5) = 16
        Kpogrd(6) = 48
        Kpogrd(7) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 15
        ireimeth(1) = 16
        ireimeth(2) = 19
        ireimeth(3) = 21
        ireimeth(4) = 22
        ireimeth(5) = 25

        ireimeth(6) = -3

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 4
        icamgrd(2, 0) = 5
        icamgrd(3, 0) = 6
        icamgrd(4, 0) = 7


        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 1
        colm = 8
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True
        MenueParam.Messg.MeArtLock = 0

        '
        '
      Case 12
        '
        'Fograauswertung (Weißverschnitt)
        '
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        ipogrd(4) = 2
        ipogrd(5) = 4
        ipogrd(6) = 12
        ipogrd(7) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        Kpogrd(4) = 8
        Kpogrd(5) = 16
        Kpogrd(6) = 48
        Kpogrd(7) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 11
        ireimeth(1) = 12
        ireimeth(2) = -1

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 4
        icamgrd(2, 0) = 5
        icamgrd(3, 0) = 6
        icamgrd(4, 0) = 7


        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 8



        icore = 1
        colm = 8

        '
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True
        MenueParam.Messg.MeArtLock = 0

        '
        '
      Case 13
        '
        'Fograauswertung (Druckfarben)
        '
        '
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        ipogrd(4) = 12
        ipogrd(5) = 22
        ipogrd(6) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        Kpogrd(4) = 48
        Kpogrd(5) = 88
        Kpogrd(6) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 5
        ireimeth(1) = 6
        ireimeth(2) = -1

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 4
        icamgrd(2, 0) = 5
        icamgrd(3, 0) = 6


        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 8



        icore = 1
        colm = 7

        '
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True
        'cmdREF(1).Visible = True
        'cmdREF(1).Enabled = True
        'lblREF(1).Visible = True
        '

      Case 14
        '
        'Fograauswertung (Textil)
        '
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        ipogrd(4) = 24
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        Kpogrd(4) = 96
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 11
        ireimeth(1) = 12
        ireimeth(2) = -1

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 4


        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 8



        icore = 1
        colm = 5

        '
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True
        'cmdREF(1).Visible = True
        'cmdREF(1).Enabled = True
        'lblREF(1).Visible = True
        MenueParam.Messg.MeArtLock = 0
        '
      Case 15
        '
        MenueParam.Menue.IDaeArt = 1

        '
        'Deckvermögen (deckend)
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 30
        ipogrd(2) = 0
        ipogrd(3) = 1
        ipogrd(4) = 30
        ipogrd(7) = 2
        ipogrd(8) = 4
        ipogrd(9) = 12
        ipogrd(10) = 14
        ipogrd(11) = 7
        ipogrd(12) = 9
        ipogrd(13) = 12
        ipogrd(14) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 120
        ipogrd(2) = 0
        Kpogrd(3) = 4
        Kpogrd(4) = 120
        Kpogrd(7) = 8
        Kpogrd(8) = 16
        Kpogrd(9) = 48
        Kpogrd(10) = 56
        Kpogrd(11) = 28
        Kpogrd(12) = 36
        Kpogrd(13) = 48
        Kpogrd(14) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 7
        ireimeth(1) = 9
        ireimeth(2) = 8
        ireimeth(3) = 10
        ireimeth(4) = -2

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        '
        '
        icamgrd(1, 0) = 7
        icamgrd(2, 0) = 8
        icamgrd(3, 0) = 9
        icamgrd(4, 0) = 10
        icamgrd(1, 1) = 11
        icamgrd(2, 1) = 12
        icamgrd(3, 1) = 13
        icamgrd(4, 1) = 14



        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 2
        colm = 15
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        chkABS.Visible = True

        lblGRA.Visible = True
        txtGRA.Visible = True
        txtGRA.Text = CStr(MenueParam.Menue.VerWS)

        '
        'Weißpigment
        '
        btnREF(3).Visible = True
        btnREF(3).Enabled = True
        lblREF(3).Visible = True
        '
        'Schwarzpigment
        '
        btnREF(4).Visible = True
        btnREF(4).Enabled = True
        lblREF(4).Visible = True
        '
        'Graupigment
        '
        btnREF(5).Visible = True
        btnREF(5).Enabled = True
        lblREF(5).Visible = True
        'Nummern der Texte (uebgrd) für Überschriften

        lblKDE.Visible = True
        txtKDE.Visible = True
        txtKDE.Enabled = True
        txtKDE.Text = MenueParam.Menue.KdeWS
        lblGRA.Visible = True
        txtGRA.Visible = True
        txtGRA.Text = CStr(MenueParam.Menue.VerWS)
        MenueParam.Messg.MeArtLock = 0


      Case 16
        '
        MenueParam.Menue.IDaeArt = 1

        'Deckvermögen (transparent)
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 30
        ipogrd(2) = 0
        ipogrd(3) = 1
        ipogrd(4) = 30
        ipogrd(7) = 12
        ipogrd(8) = 22
        ipogrd(9) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 120
        Kpogrd(2) = 0
        Kpogrd(3) = 4
        Kpogrd(4) = 120
        Kpogrd(7) = 48
        Kpogrd(8) = 88
        Kpogrd(9) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 7
        ireimeth(1) = 9
        ireimeth(2) = 8
        ireimeth(3) = 10
        ireimeth(4) = -2

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 7
        icamgrd(2, 0) = 8
        icamgrd(3, 0) = 9
        icamgrd(1, 1) = 7
        icamgrd(2, 1) = 8
        icamgrd(3, 1) = 9



        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 2
        colm = 10
        lblTRA.Visible = True
        txtTra.Visible = True
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        chkABS.Visible = True

        lblKDE.Visible = True
        txtKDE.Visible = True
        txtKDE.Enabled = True
        txtKDE.Text = MenueParam.Menue.KdeWS
        '
        'weißer Untergrund
        '
        btnREF(1).Visible = True
        btnREF(1).Enabled = True
        lblREF(1).Visible = True
        '
        'schwarzer Untergrund
        '
        btnREF(2).Visible = True
        btnREF(2).Enabled = True
        lblREF(2).Visible = True
        MenueParam.Messg.MeArtLock = 0

      Case 19
        '
        'Farbabstaende Farbkoordinaten (mit weißem Untergrund)

        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 5
        ireimeth(1) = 6
        ireimeth(2) = -1
        '
        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 1
        colm = 4
        '
        '
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        '
        'Absolutwerte Bezug
        '
        chkABS.Visible = True
        '
        'weißer Untergrund
        '
        '
        btnREF(1).Visible = True
        btnREF(1).Enabled = True
        lblREF(1).Visible = True
        '
        '
      Case 20
        '
        'Farbabstaende Farbkoordinaten mit weissem und schwarzem Untergrund


        ipogrd(0) = 30
        ipogrd(1) = 30
        ipogrd(2) = 0
        ipogrd(3) = 1
        ipogrd(4) = 30
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 120
        Kpogrd(2) = 0
        Kpogrd(3) = 4
        Kpogrd(4) = 120
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 7
        ireimeth(1) = 9
        ireimeth(2) = 8
        ireimeth(3) = 10
        ireimeth(4) = -2

        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 2
        colm = 7
        '
        '
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        '
        'Absolutwerte Bezug
        '
        chkABS.Visible = True
        '
        '
        'weißer Untergrund
        '
        '
        btnREF(1).Visible = True
        btnREF(1).Enabled = True
        lblREF(1).Visible = True
        '
        '
        '
        'schwarzer Untergrund
        '
        '
        btnREF(2).Visible = True
        btnREF(2).Enabled = True
        lblREF(2).Visible = True
        '
        '
      Case 22
        '
        'Dispergierung mit bis zu 5 Dispergierstufen
        '

        '
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        ipogrd(4) = 2
        ipogrd(5) = 4
        ipogrd(6) = 12
        ipogrd(7) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        Kpogrd(4) = 8
        Kpogrd(5) = 16
        Kpogrd(6) = 48
        Kpogrd(7) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 11
        ireimeth(1) = 12
        ireimeth(2) = -1


        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 4
        icamgrd(2, 0) = 5
        icamgrd(3, 0) = 6
        icamgrd(4, 0) = 7


        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 5



        icore = 1
        colm = 8
        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        lblGLZ.Visible = True
        cboGLZ.Visible = True
        cboGLZ.Text = Winkel(0).GK(0)
        lblINR.Visible = True
        cboINR.Visible = True
        cboINR.Text = Winkel(0).GK(1)
        lblIND.Visible = True
        cboIND.Visible = True
        cboIND.Text = Winkel(0).GK(2)
        btnBWE.Visible = True
        cboBWE.Visible = True
        chkABS.Visible = True
        MenueParam.Messg.MeArtLock = 0

      Case 28
        '
        'Mischen (weiß)
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        ipogrd(4) = 26
        ipogrd(5) = 4
        ipogrd(6) = 4
        '    ipogrd(7) = 14
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        Kpogrd(4) = 104
        Kpogrd(5) = 105
        Kpogrd(6) = 106
        '    Kpogrd(7) = 56
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 5
        ireimeth(1) = 6
        ireimeth(2) = -1

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 4
        icamgrd(2, 0) = 5
        icamgrd(3, 0) = 6
        '    icamgrd(4, 0) = 7

        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 1
        colm = 7

        '
        'Festlegen der sichtbaren controls
        '
        '
        '
        '
        chkABS.Visible = True

      Case 29
        '
        'Mischen (weiß/schwarz)
        '
        'Nummern der Texte (uebgrd) für Überschriften
        ipogrd(0) = 30
        ipogrd(1) = 30
        ipogrd(2) = 0
        ipogrd(3) = 1
        ipogrd(4) = 30
        ipogrd(7) = 26
        ipogrd(8) = 4
        ipogrd(9) = 4
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 120
        Kpogrd(2) = 0
        Kpogrd(3) = 4
        Kpogrd(4) = 120
        Kpogrd(7) = 104
        Kpogrd(8) = 105
        Kpogrd(9) = 106
        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 7
        ireimeth(1) = 9
        ireimeth(2) = 8
        ireimeth(3) = 10
        ireimeth(4) = -2

        '
        '
        'Feldnummern für CAMP-Feld
        '
        '
        '
        icamgrd(1, 0) = 7
        icamgrd(2, 0) = 8
        icamgrd(3, 0) = 9
        icamgrd(1, 1) = 7
        icamgrd(2, 1) = 8
        icamgrd(3, 1) = 9



        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 2
        colm = 10

      Case 40
        '
        '
        'Vergleich von Farbwerten
        '
        '
        '
        Call GridMethAux(Ier)
        '
      Case 48
        '
        '
        '
        'Mittelwert R-Wert, nächste R-Werte
        '
        '
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        '
        '
        '
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        '
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        '
        '

        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 27
        ireimeth(1) = 28
        ireimeth(2) = -1

        '
        '


        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 0



        icore = 1
        colm = 4

        '
        '
        'Festlegen der sichtbaren controls
        '
        '
        lblANZSuch.Visible = True
        txtANZSuch.Visible = True
        panNextWIN.Visible = True
        For kw = 0 To Winkel.Km - 1
          chkNextWIN(kw).Visible = True
          chkNextWIN(kw).Checked = True
        Next
        OleRwert = New OleDbDataAdapter
        CmdRwert = New OleDbCommand("", Cndat)
        RwertTab = New DataTable
        RwertTab.Columns.Add("DEMOD", GetType(Single))
        RwertView = New DataView(RwertTab)
        '
        '
      Case 49
        '
        '
        '
        'Mittelwert R-Wert, nächste R-Werte
        '
        '
        '
        '
        'Nummern der Texte (uebgrd) für Überschriften
        '
        '
        '
        ipogrd(0) = 30
        ipogrd(1) = 0
        ipogrd(2) = 30
        '
        '
        Kpogrd(0) = 120
        Kpogrd(1) = 0
        Kpogrd(2) = 120
        '
        '

        '   Caption-Nummern fuer bcsref.frm
        '
        ireimeth(0) = 27
        ireimeth(1) = 28
        ireimeth(2) = -1

        '
        '


        '
        '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
        '
        ireiwied = 100



        icore = 1
        colm = 4

        '



        '
      Case Else
        MsgBox(Texxt(3559))
        Ier = 3559
        btnVER.Visible = False
        Exit Sub
    End Select
    '
    '
    '
    '
    '
    '
    '
    If MenueParam.MethID = 48 Or MenueParam.MethID = 49 Then

      lblGRP.Visible = True
      cboGRP.Visible = True
      '
      Handlerwert = New HandleRwerte
      Handlerwert.cboGRP = cboGRP
      Handlerwert.lblGRP = lblGRP
      Call Handlerwert.GRoupList()
      cboGRP.SelectedValue = MenueParam.Messg.RwrtGID

      For i = 0 To cboGRP.Items.Count - 1
        If cboGRP.Items(i)("GROUP_ID") = MenueParam.Messg.RwrtGID Then
          cboGRP.SelectedIndex = i
        End If
      Next

      If cboGRP.SelectedIndex = -1 Then
        cboGRP.SelectedIndex = 0
      End If
    End If
    '
    For i = 2 * icore + 1 To 3 * icore
      ipogrd(i) = 28 + i - 2 * icore - 1
      Kpogrd(i) = 4 * ipogrd(i)
    Next i
    '
    '
    'B-Wert festlegen
    '
    If btnBWE.Visible Then
      cboBWE.SelectedIndex = MenueParam.Menue.BwertID
    End If
    '
    '
    '
    'Art des Farbstärkeangleiches festlegen (FSTAE_ID)
    '
    '
    If MenueParam.MethID > 3 And MenueParam.MethID < 15 Or MenueParam.MethID = 22 Or MenueParam.MethID = 23 Then
      SqlStmt = "SELECT * FROM TBL_FSTAE"
      AdaptFst = New OleDbDataAdapter
      AdaptFst.SelectCommand = New OleDbCommand("", Cncol)
      AdaptFst.SelectCommand.CommandText = SqlStmt
      TabFst = New DataTable
      TabFst.Clear()
      TabFst.Columns.Clear()
      If Not FillDatset(AdaptFst, TabFst) Then
        Exit Sub
      End If
      TabFst.Columns.Add("FSTAE_NAME")
      For i = 0 To TabFst.Rows.Count - 1
        TabFst.Rows(i)("FSTAE_NAME") = Texxt(450 + TabFst.Rows(i)("FSTAE_ID"))
      Next
      cboFST.Enabled = False
      cboFST.Items.Clear()
      cboFST.DataSource = TabFst
      cboFST.DisplayMember = "FSTAE_NAME"
      cboFST.ValueMember = "FSTAE_ID" '
      'DySn = DBas.OpenRecordset(SqlStmt, dbOpenDynaset, dbConsistent, dbReadOnly)
      cboFST.Visible = True
      'cboFST.Clear()
      'Do While Not DySn.EOF
      '      cboFST.AddItem DySn!Fstae_bez
      'cboFST.AddItem(Texxt(450 + DySn!fstae_ID))
      'cboFST.ItemData(cboFST.NewIndex) = DySn!fstae_ID
      'DySn.MoveNext()
      'Loop
      cboFST.Enabled = True

      cboFST.SelectedIndex = MenueParam.Menue.FstaeID
      'DySn.Close()
    End If
    '
    '
    'Eingabetextfeld aktivieren
    '
    '
    '
    If colm > 3 * icore + 1 Then
      If BitWrt(20, MenueParam.User.Writ) Then
        'TDBTab.AllowUpdate = True
      End If
      '
    End If
    txtKOM.Enabled = True
    '
    '
    'Festlegen der unsichtbaren bzw. inaktivierten controls
    '
    '
    '
    '
    If Not BitWrt(2, MenueParam.User.Visbl) Then
      chkABS.Visible = False
    End If
    If Not BitWrt(2, MenueParam.User.Enabl) Then
      chkABS.Enabled = False
    End If
    '
    If Not BitWrt(3, MenueParam.User.Visbl) Then
      btnBWE.Visible = False
      cboBWE.Visible = False
    End If
    If Not BitWrt(3, MenueParam.User.Enabl) Then
      cboBWE.Enabled = False
    End If
    If Not BitWrt(4, MenueParam.User.Visbl) Then
      cboFST.Visible = False
    End If
    If Not BitWrt(4, MenueParam.User.Enabl) Then
      cboFST.Enabled = False
    End If
    If Not BitWrt(5, MenueParam.User.Visbl) Then
      lblKDE.Visible = False
      txtKDE.Visible = False
    End If
    If Not BitWrt(5, MenueParam.User.Enabl) Then
      txtKDE.Enabled = False
    End If
    If Not BitWrt(6, MenueParam.User.Visbl) Then
      lblGLZ.Visible = False
      cboGLZ.Visible = False
    End If
    If Not BitWrt(6, MenueParam.User.Enabl) Then
      cboGLZ.Enabled = False
    End If
    If Not BitWrt(7, MenueParam.User.Visbl) Then
      lblINR.Visible = False
      cboINR.Visible = False
    End If
    If Not BitWrt(7, MenueParam.User.Enabl) Or MenueParam.Messg.GlIn Then
      cboINR.Enabled = False
    End If
    If Not BitWrt(8, MenueParam.User.Visbl) Then
      lblIND.Visible = False
      cboIND.Visible = False
    End If
    If Not BitWrt(8, MenueParam.User.Enabl) Then
      cboIND.Enabled = False
    End If
    '
    '
    Call EnblRef()
    '
    'aktivieren bzw. deaktivieren der R-Werteingabe
    '
    '
    Call NameRwe()
  End Sub
  Private Sub InBwert()
    Dim i As Integer

    '
    '
    'B-WERT Texte
    '
    '
    cboBWE.Items.Clear()

    '

    For i = 0 To 8
      cboBWE.Items.Add(MenueParam.Menue.BwertName(i))
      'cboBWE.ItemData(cboBWE.NewIndex) = i
    Next i
    cboBWE.Enabled = True
    cboBWE.SelectedIndex = MenueParam.Menue.BwertID
  End Sub

  Private Sub NameRwe()
    Captext = NstrRef(ireimeth(irei))
    If nreiwied = 0 And ireiwied > 1 Then
      nreiwmax = ireiwied
      If MenueParam.MethID <> 49 Then
        Call AnzMess(MenueParam.MethID, nreiwmax)
      End If
    End If
    cartf = NstrKen(ireimeth(irei))

    If nreiwmax > 1 And nreiwied < nreiwmax Then
      Captext = CStr(nreiwied + 1) & Texxt(998) & Captext
      Mid(cartf, 1, 1) = Chr(64 + nreiwied)
      nreiwied = nreiwied + 1
    End If
    '
    'neue Nummer fuer CAPTION
    's.COLWIN.FRM mnuQQ
    's. COLWIN.BAS START
    '
    '
    If nreiwied >= nreiwmax Then
      irei = irei + 1
      If ireimeth(irei) < 0 Then
        irei = irei + ireimeth(irei)
      End If
      nreiwied = 0
    End If

    GetPutReflex.Captext = Captext


  End Sub

  Private Sub RowCol()
    Dim HlfStr As String
    Dim i As Integer
    change = False
    cola = (cola Mod icore) + 1
    HlfStr = HlfTex(0)
    For i = 1 To colm - 1
      HlfStr = HlfStr & Chr(9) & HlfTex(i)
    Next i
    If cola = 1 Then
      rowa = rowa + 1
      For i = 1 To 3 * icore
        HlfTex(i) = ""
      Next i
    End If
    change = True
  End Sub





  Public Sub GetNameID(ByRef ID As Integer, ByRef Name As String, ByRef banum As String)
    'Dim DynRef As Recordset
    'Dim DynQua As Recordset
    Dim DESum As Single
    Dim SuFwin As Single
    Dim RBList As List(Of Single())
    Dim RP() As Single
    Dim RHILF() As Single
    Dim RhBytes() As Byte
    Dim NWE As Integer
    Dim kwb As Integer
    Dim KM As Integer
    Dim DE As Single
    Dim DL As Single
    Dim DC As Single
    Dim DH As Single
    Dim DA As Single
    Dim DB As Single
    Dim HlfText As String
    Dim FirstRow As Integer
    Dim kw As Integer
    Dim i As Integer
    Dim j As Integer
    Dim l As Integer
    Dim camp() As Single
    change = False
    Select Case RcmdRef
      Case 0
        '
        'Reflexionswerte
        '
        '
        If MenueParam.MethID = 48 Then
          For i = 0 To chkNextWIN.Count - 1
            If chkNextWIN(i).Checked Then
              Exit For
            End If
          Next
          If i = chkNextWIN.Count Then
            MsgBox(Texxt(2995), MsgBoxStyle.OkOnly, Texxt(2000))
            Exit Sub
          End If
          GrdArray.Rows.Clear()
          rowa = 0
          cola = 1
          Call EnblRef()
          GetPutReflex.InVisible()
          irei = 0
          Call NameRwe()
        End If
        zrow = rowa
        zcol = cola
        '
        '
        '
        If cola = 1 Then
          RefRow = GrdArray.NewRow
          GrdArray.Rows.Add(RefRow)
        ElseIf cola = 0 Then
          Exit Sub
        End If
        '
        'R-Wert ID
        '
        '
        '
        '

        RefRow(zcol - 1) = ID
        '
        'Name R-Werte
        '
        '
        '
        RefRow(zcol + icore - 1) = Name
        '
        'BA-Nummer





        If zcol = 1 Then
          RefRow(colm) = cartf
          If banum <> "" Then
            HlfText = banum
            RefRow(2 * icore + 1) = HlfText
          Else
            RefRow(2 * icore + 1) = ""
          End If
        ElseIf zcol = 2 Then
          RefRow(colm) = RefRow(colm) & cartf
          If banum <> "" Then
            HlfText = banum
            RefRow(3 * icore) = HlfText
          Else
            RefRow(3 * icore) = ""
          End If
        End If
        '
        'Mengen usw.
        '
        '
        '
        ViewQuali.RowFilter = "QUALI_ID=" & ID & " AND METH_ID=" & MenueParam.MethID
        '
        '
        If ViewQuali.Count > 0 Then
          camp = GetSingles(ViewQuali(0).Item("QUALI_CAMP"))
        Else
          ReDim camp(4)
          For j = 0 To 4
            If icamgrd(j, zcol - 1) > 0 Then
              If Trim(HlfTex(icamgrd(j, zcol - 1))) = "" Then
                camp(j) = 0.0#
              Else
                camp(j) = Singl(HlfTex(icamgrd(j, zcol - 1)))
              End If
            End If
          Next j
        End If
        For j = 0 To 4
          If icamgrd(j, zcol - 1) > 0 Then
            GrdArray.Rows(zrow)(icamgrd(j, zcol - 1)) = CStr(camp(j))
          End If
        Next j
        If Mid(cartf, 2, 1) = "T" Then
          GrdArray.Rows(zrow)(2 * icore) = ZZZ
        Else
          GrdArray.Rows(zrow)(2 * icore) = " "
        End If
        Call RowCol()
        Call NameRwe()
        FirstRow = TDBGrid.Rows.Count - TDBGrid.DisplayedRowCount(False)
        TDBGrid.FirstDisplayedScrollingRowIndex = FirstRow
        '
        '
        '
        '
        '
        '
        '
        If MenueParam.MethID = 48 Then
          KM = Winkel.Km
          NWE = Winkel.Wsol.Nwe
          '
          'R-Werte einlesen
          '
          '
          Application.DoEvents()
          GrpRwerte("W").clear()
          Call GetGridRwert(0, MenueParam.MethID, GrpRwerte, ier)
          'GrpRwerte("W")(0).ReTr = kwb
          GrpRwerte("W")(0).Nr = RcmdRef

          GrpRwerte("W")(0).IVoNa = True
          Call EnblRef()
          GetPutReflex.InVisible()


          OleRwert.SelectCommand = CmdRwert
          If cboGRP.SelectedValue = 0 Then
            CmdRwert.CommandText = "SELECT * FROM TBL_RWERT WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID
          Else
            CmdRwert.CommandText = "SELECT * FROM TBL_RWERT WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_GID=" & cboGRP.SelectedValue
          End If
          Cursor = Cursors.WaitCursor
          RwertTab.Rows.Clear()
          If Not FillDatset(OleRwert, RwertTab) Then
            Exit Sub
          End If
          '
          '
          For i = 0 To RwertTab.Rows.Count - 1
            'Reihenfolge an Menueparam.user.winkel anpassen
            '
            ReDim RhBytes(4 * KM * NWE - 1)

            For j = 0 To KM - 1
              For l = 0 To 4 * NWE - 1
                RhBytes(4 * j * NWE + l) = RwertTab.Rows(i)("RWERT_RWERT")(4 * Winkel(j).Lkm * NWE + l)
              Next l
            Next j
            RwertTab.Rows(i)("RWERT_RWERT") = RhBytes
          Next i
          '
          '
          '
          '
          'Spalten hinzufügen
          '
          '

          '
          'Farbabstände berechnen
          '
          '
          Cursor = Cursors.WaitCursor
          NWE = Winkel.Wsol.Nwe
          RBList = New List(Of Single())
          For kw = 0 To Winkel.Km - 1
            RBList.Add(GrpRwerte(0)(0).RefKurv(Winkel(kw).Chrm).R)
          Next
          ReDim RP(NWE - 1)
          Call GetWinkNormGk("FAR", 1, Winkel, ier)
          Call GetMenueParam("FAR", ier)

          For i = 0 To RwertTab.Rows.Count - 1
            RHILF = GetSingles(RwertTab.Rows(i)("RWERT_RWERT"))
            DESum = 0.0
            SuFwin = 0.000000001
            For kw = 0 To Winkel.Km - 1
              If chkNextWIN(kw).Checked Then
                Array.Copy(RHILF, kw * NWE, RP, 0, NWE) '
                Call quali.FarbDifferenzRef(0, kw, 0, Winkel, RBList(kw), RP, DE, DL, DC, DH, DA, DB, ier)
                DESum = DESum + (Winkel(kw).IhrmGew * DE) ^ 2
                SuFwin = SuFwin + Winkel(kw).IhrmGew
              End If
            Next kw
            RwertTab.Rows(i)("DEMOD") = Sqrt(DESum / SuFwin)
          Next i
          RwertTab.AcceptChanges()
          RwertView.Sort = "DEMOD"
          RwertView.RowFilter = ""
          cartf = "@PMW"
          Cursor = Cursors.Default
          '
          '
          'Übernahme in Grid
          '
          '
          '
          For i = 0 To Min(CInt(txtANZSuch.Text), RwertTab.Rows.Count - 1)
            If RwertView(i)("RWERT_ID") <> GrdArray.Rows(0)(0) Then
              RefRow = GrdArray.NewRow
              RefRow(0) = RwertView(i)("RWERT_ID")
              RefRow(1) = RwertView(i)("RWERT_NAME")
              RefRow(2) = " "
              RefRow(3) = " "
              RefRow(4) = cartf
              GrdArray.Rows.Add(RefRow)
            End If
          Next
          Cursor = Cursors.Default


        End If
      Case 1, 2, 3, 4, 5
        '
        'weißer Untergrund (W)
        'schwarzer Untergrund (S)
        'Weißpigment (W)
        'Schwarzpigment (S)
        'Graupigment (G)
        '
        '
        lblREF(RcmdRef).Text = Name
        Select Case RcmdRef
          Case 1, 3
            KeyR = "W"
            kwb = 0
          Case 2, 4
            KeyR = "S"
            kwb = 1
          Case 5
            KeyR = "G"
            kwb = 0
        End Select


        GrpRwerte(KeyR).RefUnt.ID = ID
        GrpRwerte(KeyR).RefUnt.kwb = kwb
        GrpRwerte(KeyR).RefUnt.Nr = RcmdRef - 1
        ReadWrite.ReadRwert(ID, GrpRwerte(KeyR).RefUnt, ier)

        GrpRwerte(KeyR).RefUnt.IVoNa = True
        Call EnblRef()
        GetPutReflex.InVisible()

    End Select

    change = True

  End Sub



  Private Sub TDBGrid_CellEndEdit(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles TDBGrid.CellEndEdit
    Dim ID As Integer
    Dim SqlStmt As String
    Dim BaNum As String

    ID = -1
    BaNum = ""
    If icore = 1 And e.ColumnIndex = 3 Then
      ID = CInt(TDBGrid.Rows(e.RowIndex).Cells(0).Value)
      If IsDBNull(TDBGrid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value) Then
        BaNum = ""
      Else
        BaNum = Trim(TDBGrid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value)
      End If
    End If
    If icore = 2 Then
      If e.ColumnIndex = 2 * icore + 1 Then
        ID = CInt(TDBGrid.Rows(e.RowIndex).Cells(0).Value)
        If IsDBNull(TDBGrid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value) Then
          BaNum = ""
        Else
          BaNum = Trim(TDBGrid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value)
        End If
      End If
      If e.ColumnIndex = 3 * icore Then
        ID = CInt(TDBGrid.Rows(e.RowIndex).Cells(0).Value)
        If IsDBNull(TDBGrid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value) Then
          BaNum = ""
        Else
          BaNum = Trim(TDBGrid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value)
        End If
      End If
    End If
    If ID >= 0 Then
      SqlStmt = "UPDATE TBL_RWERT SET RWERT_KENN='" & BaNum & "'" _
      & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID=" & ID
      CmdQuery.CommandText = SqlStmt
      If SQLNonQuery(CmdQuery, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    DragValue = Nothing
  End Sub

  Private Sub TDBGrid_CellClick(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles TDBGrid.CellClick
    Dim STB As String
    Dim ZEI As String
    If MenueParam.MethID = 48 Or MenueParam.MethID = 49 Then Exit Sub
    If MenueParam.MethID = 28 Or MenueParam.MethID = 29 Then Exit Sub

    '
    '
    'Click
    '
    '
    '
    If e.RowIndex < 0 Then Exit Sub
    If e.ColumnIndex = 2 * icore Then
      Teee = GrdArray.Rows(e.RowIndex)(colm)
      If Trim(Teee) = "" Then
        Exit Sub
      End If
      STB = Teee.Substring(1, 1)
      If STB = "P" Or e.RowIndex = 0 Then
        Mid(Teee, 2, 1) = "T"
        If icore = 2 And Len(Teee) > 4 Then
          Mid(Teee, 6, 1) = "T"
        End If
        ZEI = ZZZ
      Else
        Mid(Teee, 2, 1) = "P"
        If icore = 2 And Len(Teee) > 4 Then
          Mid(Teee, 6, 1) = "P"
        End If
        ZEI = " "
      End If
      GrdArray.Rows(e.RowIndex)(colm) = Teee
      GrdArray.Rows(e.RowIndex)(e.ColumnIndex) = ZEI
    End If

    'End If
  End Sub

  Private Sub TDBGrid_RowsRemoved(sender As Object, e As System.Windows.Forms.DataGridViewRowsRemovedEventArgs) Handles TDBGrid.RowsRemoved
    Dim i As Integer
    Dim ireimin As Integer '
    '

    'TDBTab.Delete()
    ''
    ''
    If TDBGrid.Rows.Count = 0 Then Exit Sub
    If e.RowIndex = 0 Then
      Teee = TDBGrid.Rows(e.RowIndex).Cells(colm).Value
      If Teee = "" Then
        Exit Sub
      End If
      Mid(Teee, 2, 1) = "T"
      If icore = 2 And Len(Teee) > 4 Then
        Mid(Teee, 6, 1) = "T"
      End If
      TDBGrid.Rows(e.RowIndex).Cells(colm).Value = Teee
      TDBGrid.Rows(e.RowIndex).Cells(2 * icore).Value = ZZZ

    End If
    rowa = TDBGrid.Rows.Count
    '
    '
    '
    '
    If ireimeth(2 * icore) > 0 Then
      For i = 0 To nrei
        If ireimeth(i) < 0 Then
          ireimin = ireimeth(i)
          Exit For
        End If
      Next i
      If rowa < Abs(ireimin) Then
        irei = 0
        cartf = NstrKen(ireimeth(irei))
      Else
        irei = -ireimin
        cartf = NstrKen(ireimeth(irei))
      End If
      irei = irei + 1
    End If

  End Sub

  Private Sub TDBGrid_UserDeletingRow(sender As Object, e As System.Windows.Forms.DataGridViewRowCancelEventArgs) Handles TDBGrid.UserDeletingRow
    ' 
    If MenueParam.MethID = 48 Then Exit Sub
    '
    '
    'Doubleclick oder keypress chr(46) Entf
    '
    '
    '
    '
    If Not BitWrt(13, MenueParam.User.Writ) Then
      e.Cancel = True
      Exit Sub
    End If

    '

  End Sub
  Private Sub TDBGrid_RowHeaderMouseDoubleClick(sender As Object, e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles TDBGrid.RowHeaderMouseDoubleClick
    If Not BitWrt(13, MenueParam.User.Writ) Then
      Exit Sub
    End If
    GrdArray.Rows.RemoveAt(e.RowIndex)
  End Sub
  Private Sub TDBGrid_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles TDBGrid.DataError
    e.Cancel = False
  End Sub


  Private Sub TDBGrid_ColumnWidthChanged(sender As Object, e As System.Windows.Forms.DataGridViewColumnEventArgs) Handles TDBGrid.ColumnWidthChanged
    LayoutChanged = True
  End Sub


  Private Sub TDBGrid_CellMouseDown(sender As Object, e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles TDBGrid.CellMouseDown
    If e.RowIndex < 0 Then Exit Sub
    If e.ColumnIndex > 2 * icore And e.ColumnIndex <> colm Then
      DragValue = GrdArray.Rows(e.RowIndex)(e.ColumnIndex)
    End If
  End Sub



  Private Sub TDBGrid_CellMouseUp(sender As Object, e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles TDBGrid.CellMouseUp
    DragValue = Nothing
  End Sub

  Private Sub TDBGrid_CellMouseMove(sender As Object, e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles TDBGrid.CellMouseMove
    If Not e.Button = Forms.MouseButtons.Left Then Exit Sub
    If GrdArray.Rows.Count = 0 Then Exit Sub
    If e.RowIndex < 0 Then Exit Sub
    If IsNothing(DragValue) Then Exit Sub
    If e.ColumnIndex > 2 * icore Then
      '
      If e.ColumnIndex <= 2 * icore Or e.ColumnIndex = colm Then Exit Sub
      If TDBGrid.CurrentCell.IsInEditMode Then Exit Sub
      GrdArray.Rows(e.RowIndex)(e.ColumnIndex) = DragValue
    End If
  End Sub

  Private Sub mnuCopy_Click(sender As Object, e As System.EventArgs) Handles mnuCopy.Click
    Dim i As Integer
    '
    '
    'Ausgewählte Zeilen umspeichern
    '
    '
    '

    ReDim GrdRows(TDBGrid.SelectedRows.Count - 1)

    For i = 0 To TDBGrid.SelectedRows.Count - 1
      GrdRows(i) = GrdArray.Rows(TDBGrid.SelectedRows(i).Index)
    Next
  End Sub

  Private Sub mnuPaste_Click(sender As Object, e As System.EventArgs) Handles mnuPaste.Click
    Dim i As Integer
    Dim j As Integer
    Dim Darow As DataRow
    Dim ipos As Integer
    If IsNothing(GrdRows) Then Exit Sub
    If TDBGrid.SelectedRows.Count = 0 Then Exit Sub
    ipos = TDBGrid.SelectedRows(0).Index
    For i = 0 To GrdRows.Count - 1
      Darow = GrdArray.NewRow
      For j = 0 To GrdArray.Columns.Count - 1
        Darow(j) = GrdRows(i).Item(j)
      Next
      GrdArray.Rows.InsertAt(Darow, ipos)
      If ipos = 0 Then
        Teee = TDBGrid.Rows(ipos).Cells(colm).Value
        If Teee = "" Then
          Exit Sub
        End If
        Mid(Teee, 2, 1) = "T"
        If icore = 2 And Len(Teee) > 4 Then
          Mid(Teee, 6, 1) = "T"
        End If
        TDBGrid.Rows(ipos).Cells(colm).Value = Teee
        TDBGrid.Rows(ipos).Cells(2 * icore).Value = ZZZ
      End If
      ipos = ipos + 1
    Next

    ReDim GrdRows(-1)

  End Sub

  Private Sub mnuDelete_Click(sender As Object, e As System.EventArgs) Handles mnuDelete.Click
    Dim i As Integer
    For i = TDBGrid.SelectedRows.Count - 1 To 0 Step -1
      GrdArray.Rows(TDBGrid.SelectedRows(i).Index).Delete()
    Next
  End Sub





  Private Sub ContextTDBGrid_Opening(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles ContextTDBGrid.Opening
    If TDBGrid.SelectedRows.Count = 0 Then
      mnuCopy.Enabled = False
      mnuPaste.Enabled = False
      mnuDelete.Enabled = False
    Else
      mnuCopy.Enabled = True
      mnuPaste.Enabled = True
      mnuDelete.Enabled = True
    End If
    If IsNothing(GrdRows) OrElse GrdRows.Length = 0 Then
      mnuPaste.Enabled = False
    Else
      mnuPaste.Enabled = True
    End If

  End Sub
  '
  '
  Sub MakeMerkGrid(ByRef FarbMerkmale As ValuesGrpsAssigns, ByRef FarbTabWerte As DataTable, ByRef flgFarbwerte As DataGridView)
    Dim MaxMerk As Integer
    Dim Dadru As Integer
    Dim i As Integer
    Dim Ianwsg As Integer
    If IsNothing(FarbTabWerte) Then Exit Sub
    flgFarbwerte.ColumnHeadersVisible = False
    flgFarbwerte.AlternatingRowsDefaultCellStyle.BackColor = Color.WhiteSmoke

    FarbTabWerte.Rows.Clear()
    FarbTabWerte.Columns.Clear()
    FarbTabWerte.Columns.Add("CHR", GetType(String))
    FarbTabWerte.Columns.Add("DATE", GetType(Date))
    FarbTabWerte.Columns.Add("NAME", GetType(String))
    FarbTabWerte.Columns.Add("BEM", GetType(String))
    FarbTabWerte.Columns.Add("BANUM", GetType(String))

    Dadru = 0
    If (MenueParam.User.Druq Mod 32) > 1 Then
      Dadru = 1
    End If

    'flgFarbwerte.Columns(0).HeaderCell
    MaxMerk = 0
    For Ianwsg = 0 To FarbMerkmale.Count - 1
      If FarbMerkmale(Ianwsg).CountMerk > MaxMerk Then
        MaxMerk = FarbMerkmale(Ianwsg).CountMerk
      End If
    Next Ianwsg

    For i = 0 To MaxMerk - 1
      FarbTabWerte.Columns.Add(KeyRe(i))
    Next
    '
    '
    '
    'Gitternetz
    '
    '
    '
    flgFarbwerte.RowHeadersWidth = 25
    '
    'Name
    '
    If flgFarbwerte.Columns.Count = 0 Then Exit Sub
    '
    'Character CHR
    '
    flgFarbwerte.Columns(0).HeaderText = ""
    flgFarbwerte.Columns(0).Width = 15
    flgFarbwerte.Columns(0).ReadOnly = True
    flgFarbwerte.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
    flgFarbwerte.Columns(0).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgFarbwerte.Columns(0).SortMode = DataGridViewColumnSortMode.NotSortable
    '
    'Datum  DATE
    '
    flgFarbwerte.Columns(1).HeaderText = Texxt(140)
    flgFarbwerte.Columns(1).Width = 40
    flgFarbwerte.Columns(1).ReadOnly = True
    flgFarbwerte.Columns(1).Visible = False
    flgFarbwerte.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgFarbwerte.Columns(1).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgFarbwerte.Columns(1).SortMode = DataGridViewColumnSortMode.NotSortable
    '
    '
    If Dadru = 1 Then
      flgFarbwerte.Columns(1).Visible = True
    Else
      flgFarbwerte.Columns(1).Visible = False
    End If
    '
    '
    'Name NAME
    '
    '
    '
    flgFarbwerte.Columns(2).HeaderText = Texxt(2206)
    flgFarbwerte.Columns(2).Width = 300
    flgFarbwerte.Columns(2).ReadOnly = True
    flgFarbwerte.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgFarbwerte.Columns(2).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgFarbwerte.Columns(2).SortMode = DataGridViewColumnSortMode.NotSortable
    '
    'Bemerkung BEM
    '
    flgFarbwerte.Columns(3).HeaderText = Texxt(375)
    flgFarbwerte.Columns(3).Width = 200
    flgFarbwerte.Columns(3).ReadOnly = True
    flgFarbwerte.Columns(3).Visible = False
    flgFarbwerte.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgFarbwerte.Columns(3).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgFarbwerte.Columns(3).SortMode = DataGridViewColumnSortMode.NotSortable '
    '
    '
    'Batchnummer Banum
    '
    flgFarbwerte.Columns(4).HeaderText = Texxt(375)
    flgFarbwerte.Columns(4).Width = 40
    flgFarbwerte.Columns(4).ReadOnly = True
    flgFarbwerte.Columns(4).Visible = False
    flgFarbwerte.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgFarbwerte.Columns(4).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgFarbwerte.Columns(4).SortMode = DataGridViewColumnSortMode.NotSortable '
    '
    For i = 0 To MaxMerk - 1
      ' flgFarbwerte.Columns(i + 3).HeaderText = Trim(FarbMerkmale(KeyAnwsg).Merk(i).Kbez)
      flgFarbwerte.Columns(KeyRe(i)).Width = 50
      flgFarbwerte.Columns(KeyRe(i)).ReadOnly = True
      flgFarbwerte.Columns(KeyRe(i)).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      flgFarbwerte.Columns(KeyRe(i)).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
      flgFarbwerte.Columns(KeyRe(i)).SortMode = DataGridViewColumnSortMode.NotSortable
    Next i
  End Sub

  '
  '######################################
  '#####################################
  '
  '
  Sub GridTableMerk(ByRef FarbMerkmale As ValuesGrpsAssigns, ByRef FarbTabWerte As DataTable, ByRef chkLichtart As List(Of CheckBox), ByRef chkWinkel As List(Of CheckBox), ByRef flgFarbwerte As DataGridView, ByRef Mrkadr As DictionaryInd(Of String, GridMerkAdr))
    Dim Ianwsg As Integer
    Dim ColIndex As Integer
    Dim j As Integer
    Dim i As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim Row As DataRow
    Dim iv As Integer
    Dim kn As Integer
    Dim Jleer As Boolean
    Dim KNull As Integer
    Dim AnwsgName As String
    Dim llen As Integer
    Dim ScWi As Single
    Dim ScHe As Single
    If IsNothing(FarbTabWerte) Then Exit Sub


    '
    '
    ScWi = 0.0#
    ScHe = 0



    llen = 0
    '
    'Initialisierung der Druckausgabe

    '

    Ianwsg = FarbMerkmale.Count
    If Ianwsg = 0 Then
      Exit Sub
    End If
    If IsNothing(mrkadr) Then Exit Sub
    mrkadr.Clear()
    '
    'Ausdruck für Winkel und Lichtart (ja/nein)
    '

    For Ianwsg = 0 To FarbMerkmale.Count - 1
      For kn = 0 To MenueParam.Normfa.Nlz - 1
        For kw = 0 To Winkel.Km - 1
          FarbMerkmale(Ianwsg).PrintNLAWIN(kn, kw) = False
        Next kw
      Next kn
    Next Ianwsg

    'Winkel/Meßgeometrien
    '
    '
    '
    '
    'Gitternetz
    '
    '
    '
    FarbTabWerte.Rows.Clear()
    'Anweisungen
    '
    'Lichtarten
    '
    '

    For kn = 0 To MenueParam.Normfa.Nlz - 1
      'If chkNLA(kn).Checked Then
      'FarbMerkmale.PrintNLA(kn) = True
      '
      '
      '
      '
      '
      'Winkel/Meßgeometrien
      '
      '
      For kw = 0 To Winkel.Km - 1
        If chkNLA(kn).Checked And chkWIN(kw).Checked Then
          '

          For Ianwsg = 0 To FarbMerkmale.Count - 1
            '
            '
            '
            'Prüfen, ob Werte für Merkmale vorhanden
            '
            For i = 0 To FarbMerkmale(Ianwsg).Count - 1
              For j = 0 To FarbMerkmale(Ianwsg).CountMerk - 1
                If Not IsDBNull(FarbMerkmale(Ianwsg)(i)(kn)(kw)(j)) Or FarbMerkmale(Ianwsg).Merk(j).Typ = "M" Then
                  Exit For
                End If
              Next j
              If j < FarbMerkmale(Ianwsg).CountMerk Then
                Exit For
              End If
            Next i
            If i >= FarbMerkmale(Ianwsg).Count Then
              Continue For
            End If
            FarbMerkmale(Ianwsg).PrintNLAWIN(kn, kw) = True


            '
            '
            'Leerzeile
            '
            '
            Row = FarbTabWerte.NewRow
            FarbTabWerte.Rows.Add(Row)
            Row(0) = " "
            '
            '
            '
            'Schreibe Zeile für Überschrift
            '
            'Name für Lichtart, Winkel und Anweisung
            '
            '
            Row = FarbTabWerte.NewRow
            FarbTabWerte.Rows.Add(Row)
            Row(0) = "+"
            AnwsgName = FarbMerkmale(Ianwsg).AnwsgName
            Row(2) = MenueParam.Normfa(kn).NormNama & Chr(0) & Winkel(kw).Chrm & "(" & Format(100 * Winkel(kw).Iglz * Winkel(kw).GK(0), "0.00") & ")" & Chr(0) & AnwsgName
            '
            flgFarbwerte.Rows(FarbTabWerte.Rows.Count - 1).Cells(2).Style.Font = New Font(flgFarbwerte.DefaultCellStyle.Font, FontStyle.Bold)
            '
            '
            '
            'Name R-Werte und Merkmale
            '
            '
            Row = FarbTabWerte.NewRow
            FarbTabWerte.Rows.Add(Row)
            Row("CHR") = ""
            Row("NAME") = Texxt(600)
            Row("BEM") = Texxt(916)
            Row("BANUM") = Texxt(918)
            For j = 0 To FarbMerkmale(Ianwsg).CountMerk - 1
              Row(KeyRe(j)) = Trim(FarbMerkmale(Ianwsg).Merk(j).Kbez) & Space(1)
            Next j
            '
            '

            '
            '
            '
            KNull = 0
            iv = 0
            For i = 0 To FarbMerkmale(Ianwsg).Count - 1
              Jleer = False
              If FarbMerkmale(Ianwsg)(i).Itp Or i = 0 Then
                Jleer = True
              End If
              If i > 0 Then
                If FarbMerkmale(Ianwsg)(i).Nr = FarbMerkmale(Ianwsg)(i - 1).Nr Then
                  Jleer = False
                End If
              End If
              If Jleer Then
                '
                'Leerzeile
                '
                '
                Row = FarbTabWerte.NewRow
                FarbTabWerte.Rows.Add(Row)
                Row(0) = " "
                '
              End If
              '
              '
              'Merkmale übernehmen
              '
              '
              Row = FarbTabWerte.NewRow
              FarbTabWerte.Rows.Add(Row)
              Row("CHR") = Chr(96 + ((FarbMerkmale(Ianwsg)(i).Nr) Mod 26) + 1)
              ' Rnr(iv) = MnFarbWerte(Ianwsg)(i).Nr
              Row("DATE") = FarbMerkmale(Ianwsg)(i).Dattim
              Row("NAME") = FarbMerkmale(Ianwsg)(i).Name
              Row("BEM") = FarbMerkmale(Ianwsg)(i).Bem
              Row("BANUM") = FarbMerkmale(Ianwsg)(i).Banum
              KNull = 0
              For j = 0 To FarbMerkmale(Ianwsg).CountMerk - 1
                If FarbMerkmale(Ianwsg).Merk(j).Typ = "M" Then
                  ColIndex = -1
                  For k = 0 To FarbTabWerte.Columns.Count - 1
                    If FarbTabWerte.Columns(k).ColumnName = KeyRe(j) Then
                      ColIndex = k
                      Exit For
                    End If
                  Next
                  mrkadr.Add(KeyName(FarbTabWerte.Rows.Count - 1) & KeyRe(ColIndex), New GridMerkAdr(Ianwsg, i, kn, kw, j))
                End If
                Row(KeyRe(j)) = FarbMerkmale(Ianwsg)(i)(kn)(kw)(j)
                If IsDBNull(Row(KeyRe(j))) Then
                  Row(KeyRe(j)) = ""
                End If
                If Trim(Row(KeyRe(j))) = "" Then
                  KNull = KNull + 1
                End If
              Next j
              If KNull = FarbMerkmale(Ianwsg).CountMerk Then
                'Row.Delete()
              End If
            Next i
          Next Ianwsg
        End If
      Next kw
    Next kn
    '


  End Sub

  Sub PlottScreen(Index As Integer, Rmax As Single, GrpRwerte As RefValuesGrp)
    Dim nk As Integer
    Dim ipl As Integer
    Dim Knlz As Integer
    Dim i As Integer
    Dim iee As Long
    Dim Iprn As Integer
    Dim Rwerten As New RefValuesGrp
    Dim Rmii As Single
    Dim Rmaa As Single
    Dim VKwb(2) As Integer
    Rwerten.Add(cdr(Index), GrpRwerte(cdr(Index)))
    '
    'L,a,b-Grafik ausschalten
    '
    '
    If Index > 3 Then
      radGPL(2).Enabled = False
      radGPL(4).Enabled = False
    Else
      radGPL(2).Enabled = True
      radGPL(4).Enabled = True
    End If
    nk = GrpRwerte(cdr(Index)).Count
    If nk < 2 Then
      radGPL(2).Enabled = False
      radGPL(4).Enabled = False
    End If
    Text = radDRQ(Index).Text & "("
    For i = 0 To chkWIN.Count - 1
      If chkWIN(i).Checked Then
        Text = Text & chkWIN(i).Text & " "
      End If
    Next i
    Text = Text & ")"
    Select Case Index
      Case 0, 1, 2, 3, 4, 5, 10
        ipl = 0
      Case 6, 7, 8, 9
        ipl = 2
        If chkLinear.Checked And radGPL(1).Checked Then
          ipl = -1
        End If
    End Select
    If radGPL(1).Checked Then
      chkLinear.Visible = False
      Select Case Index
        Case 6, 7, 8, 9
          chkLinear.Visible = True
      End Select

      picDRQ.Visible = True
      'flgDRQ.Visible = False
      picLAB.Visible = False
      Rmii = -1
      Rmaa = Rmax
      For i = 0 To cboSKAL.SelectedIndex - 1
        If CLng(Rmaa) = CInt(cboSKAL.Items(i).text) Then
          cboSKAL.SelectedIndex = i
          Exit For
        End If
      Next i
    ElseIf radGPL(2).Checked Or radGPL(4).Checked Then
      picDRQ.Visible = False
      picLAB.Visible = True
      'flgDRQ.Visible = False
      For i = 0 To radNLA.Count - 1
        If radNLA(i).Checked Then
          Knlz = i + 1
          Exit For
        End If
      Next i
      Iprn = 0
      If Rwerten(1).Count > 0 Then
        For i = 1 To Rwerten.Count
          VKwb(i) = Rwerten(i)(1).kwb
        Next i
      End If
      ier = iee
      If ier <> 0 Then
        Exit Sub
      End If
    End If
    Rwerten = Nothing
  End Sub

  '
  '
  '
  '
  '




  Private Sub btnVOR_Click(sender As Object, e As System.EventArgs) Handles btnVOR.Click
    splGPL.Visible = False
    splDRQ.Panel1.Visible = False
    panWinChk.Visible = True
    panWinRad.Visible = False
    panNlaChk.Visible = True
    panNlaRad.Visible = False
    flgMerk.Visible = True
    flgRwert.Visible = False
    picDRQ.Visible = False
    picLAB.Visible = False
    picXYZ.Visible = False
    btnDRU.Enabled = True

    Call GridTableMerk(FarbWerte, TabMerkmale, chkNLA, chkWIN, flgMerk, mrkadr)

  End Sub

  Private Sub btnABR_Click(sender As Object, e As System.EventArgs) Handles btnABR.Click
    PanQual.Visible = True
    panResult.Visible = False
    btnVER.Enabled = True
  End Sub

  Private Sub chkWINNLA_CheckedChanged(sender As Object, e As System.EventArgs) Handles _
    chkWIN_00.CheckedChanged, chkWIN_01.CheckedChanged, chkWIN_02.CheckedChanged, chkWIN_03.CheckedChanged, chkWIN_04.CheckedChanged, chkWIN_05.CheckedChanged, chkWIN_06.CheckedChanged, chkWIN_07.CheckedChanged, chkWIN_08.CheckedChanged, _
    chkNLA_0.CheckedChanged, chkNLA_1.CheckedChanged, chkNLA_2.CheckedChanged, chkNLA_3.CheckedChanged, chkNLA_4.CheckedChanged
    Dim i As Integer
    If flgMerk.Visible Then
      btnVOR.PerformClick()
    Else
      For i = 0 To chkWIN.Count - 1
        RezGraphics.kwopt(i) = chkWIN(i).Checked
      Next
      picDRQ.Refresh()
    End If
  End Sub

  Private Sub flgMerk_CellClick(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles flgMerk.CellClick

    If e.ColumnIndex = 0 Then
      If flgMerk.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = "+" Then
        flgMerk.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = "-"
        Exit Sub
      End If
      If flgMerk.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = "-" Then
        flgMerk.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = "+"
        Exit Sub
      End If
    End If
    flgMerk.Rows(e.RowIndex).Cells(e.ColumnIndex).ReadOnly = True
    KeyMrkAdr = KeyName(e.RowIndex) & KeyRe(e.ColumnIndex)
    If mrkadr.Keys.Contains(KeyMrkAdr) Then
      flgMerk.Rows(e.RowIndex).Cells(e.ColumnIndex).ReadOnly = False
    End If
  End Sub

  Private Sub flgMerk_CellEndEdit(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles flgMerk.CellEndEdit
    KeyMrkAdr = KeyName(e.RowIndex) & KeyRe(e.ColumnIndex)
    FarbWerte(mrkadr(KeyMrkAdr).Ianwsg)(mrkadr(KeyMrkAdr).i)(mrkadr(KeyMrkAdr).kn)(mrkadr(KeyMrkAdr).kw)(mrkadr(KeyMrkAdr).j) = flgMerk.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
  End Sub
  Private Sub btnGRF_Click(sender As Object, e As System.EventArgs) Handles btnGRF.Click
    Dim i As Integer
    '
    splDRQ.Panel1.Visible = True

    splDRQ.Visible = True
    splGPL.Visible = True
    panWinChk.Visible = False
    panWinRad.Visible = True
    panNlaChk.Visible = False
    panNlaRad.Visible = True

    flgMerk.Visible = False
    flgRwert.Visible = False
    picDRQ.Visible = False
    picLAB.Visible = False
    picXYZ.Visible = False
    For i = 0 To radDRQ.Count - 1
      radDRQ(i).Visible = False
      radDRQ(i).Enabled = True
    Next i
    For i = 0 To radDRQ.Count - 1
      If Not IsNothing(GrpRwerte(i)) AndAlso GrpRwerte(i).Count > 0 Then
        radDRQ(i).Visible = True
      End If
    Next i
    '
    'Untergrund
    '
    If Not IsNothing(GrpRwerte(0).RefUnt) AndAlso GrpRwerte(0).RefUnt.IVoNa Then
      radDRQ(2).Visible = True
    End If

    For i = 0 To radDRQ.Count - 1
      If radDRQ(i).Visible Then
        radDRQ(i).Checked = True
        radDRQ(i).PerformClick()
        Exit For
      End If
    Next i


  End Sub

  Private Sub radDRQ_Click(sender As Object, e As System.EventArgs) Handles _
   radDRQ_00.Click, radDRQ_01.Click, radDRQ_02.Click, radDRQ_03.Click, radDRQ_04.Click, radDRQ_05.Click, radDRQ_06.Click, radDRQ_07.Click, radDRQ_08.Click, radDRQ_09.Click, radDRQ_10.Click, radDRQ_11.Click
    Dim i As Integer
    Dim IGPL As Integer
    Dim index As Integer
    Dim Fakt As Single
    If IsNothing(GrpRwerte) Then Exit Sub
    radGPL(0).Visible = False
    For i = 0 To radGPL.Count - 1
      radGPL(i).Enabled = False
    Next
    '
    '
    'gewünschte Werte bereitstellen
    '
    index = CInt(sender.name.substring(7, 2))
    If index > 7 And index <> 11 Then
      chkLinear.Visible = True
    Else
      chkLinear.Visible = False
    End If
    RezGraphics.WeSc(0) = True
    RezGraphics.WeSc(1) = False
    'RezGraphics.
    RezGraphics.ipl = 0
    Fakt = 100.0
    Select Case index
      Case 0, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11
        RezGraphics.PlotRwerte.clear()
        For i = 0 To GrpRwerte(index).Count - 1
          RezGraphics.PlotRwerte.Add(KeyRe(i), GrpRwerte(index)(i))
        Next i
        RezGraphics.Text = radDRQ(index).Text
      Case 2
        RezGraphics.PlotRwerte.clear()
        For i = 0 To 2
          If Not IsNothing(GrpRwerte(i).RefUnt) AndAlso GrpRwerte(i).RefUnt.IVoNa Then
            RezGraphics.PlotRwerte.Add(KeyRe(i), GrpRwerte(i).RefUnt)
            If i < 2 Then
              RezGraphics.Vkwb(i) = i
              RezGraphics.WeSc(i) = True
            Else
              RezGraphics.Vkwb(0) = 0
            End If
            radGPL(1).Enabled = True
            radGPL(3).Enabled = True
          End If
        Next i
    End Select
    '
    '
    Select Case index
      '
      Case 0, 1, 3, 4, 5, 6
        '
        'R-Werte weiß und schwarz
        '
        For i = 0 To radGPL.Count - 1
          radGPL(i).Enabled = True
        Next
        '
        'Farbwerte
        '
        radGPL(2).Enabled = True
        RezGraphics.FawrtRwerte(0).clear()
        For i = 0 To GrpRwerte(index).Count - 1
          RezGraphics.FawrtRwerte(0).Add(KeyRe(i), GrpRwerte(index)(i))
          If i > 0 And GrpRwerte(index)(i).Itp Then
            radGPL(2).Enabled = False
          End If
        Next
        RezGraphics.CalcFarbWrt()
        RezGraphics.Vkwb(0) = 0
        If index = 1 Or index = 4 Then
          RezGraphics.Vkwb(0) = 1
        End If
        '


      Case 7, 8, 9, 10, 11
        '
        'Nicht R-Werte
        '
        '
        RezGraphics.Vkwb(0) = 0
        If index = 10 Then
          RezGraphics.Vkwb(0) = 1
        End If
        radGPL(1).Enabled = True
        radGPL(3).Enabled = True
        If index <> 11 Then
          RezGraphics.ipl = 2
          Fakt = 1.0
        End If

    End Select
    '
    '
    '
    '

    '
    '
    'Tabelle
    '
    '
    TabRwerte.Rows.Clear()
    For i = 0 To RezGraphics.PlotRwerte.Count - 1
      Call GetPutReflex.TabRefRecord(1, Winkel, Fakt, RezGraphics.PlotRwerte(i), TabRwerte, ier)
    Next
    '
    '

    IGPL = 0
    For i = 0 To radGPL.Count - 1
      If radGPL(i).Checked Then
        IGPL = i
      End If
    Next i
    radGPL(IGPL).PerformClick()
    Application.DoEvents()
  End Sub
  Private Sub radGPL_Click(sender As Object, e As System.EventArgs) Handles _
  radGPL_0.Click, radGPL_1.Click, radGPL_2.Click, radGPL_3.Click, radGPL_4.Click
    Dim i As Integer
    Dim kw As Integer
    Dim index As Integer
    index = CInt(sender.name.substring(7, 1))
    flgMerk.Visible = False
    flgRwert.Visible = False
    picDRQ.Visible = False
    picLAB.Visible = False
    picXYZ.Visible = False
    panNlaRad.Enabled = False
    panWinRad.Enabled = False
    radDRQ(2).Enabled = True
    For i = 7 To 10
      radDRQ(i).Enabled = True
      chkLinear.Checked = False
    Next
    If index = 1 Then
      cboSKAL.Visible = True
      For i = 7 To radDRQ.Count - 2
        If radDRQ(i).Checked Then
          chkLinear.Visible = True
        End If
      Next
      panWinChk.Visible = True
      panWinRad.Visible = False
    Else
      cboSKAL.Visible = False
      chkLinear.Visible = False
      panWinChk.Visible = False
      panWinRad.Visible = True

    End If
    '
    '
    '
    btnDRU.Enabled = False
    For kw = 0 To Winkel.Km - 1
      If chkWIN(kw).Checked Then
        RezGraphics.kwopt(kw) = True
      Else
        RezGraphics.kwopt(kw) = False
      End If
    Next

    '
    'Art der Darstellung
    '
    Select Case index
      Case 0
        '
        'Merkmale
        '
        If PrinterSettings.InstalledPrinters.Count > 0 Then
          btnUser.Visible = True
          btnAUS.Visible = False
        End If
        '
      Case 1
        '
        '
        '
        'Plotten R-splWerte, Absorption usw.
        '
        If PrinterSettings.InstalledPrinters.Count > 0 Then
          btnUser.Visible = False
          btnAUS.Visible = True
        End If
        '
        '
        panWinRad.Enabled = True
        picDRQ.Visible = True
        picDRQ.Refresh()
        Application.DoEvents()
      Case 2
        '
        '
        'L,a,b-Graphic
        '
        '
        If PrinterSettings.InstalledPrinters.Count > 0 Then
          btnUser.Visible = False
          btnAUS.Visible = True
        End If
        panWinRad.Enabled = True
        panNlaRad.Enabled = True

        picLAB.Visible = True
        picLAB.Refresh()
        radDRQ(2).Enabled = False
        For i = 7 To 10
          radDRQ(i).Enabled = False
        Next
      Case 3
        '
        'Tabellen
        '
        flgRwert.Visible = True
        If PrinterSettings.InstalledPrinters.Count > 0 Then
          btnUser.Visible = True
          btnAUS.Visible = False
        End If
        btnDRU.Enabled = True

      Case 4
        '
        '
        'Farbe
        '
        '
        If PrinterSettings.InstalledPrinters.Count > 0 Then
          btnUser.Visible = False
          btnAUS.Visible = True
        End If
        panWinRad.Enabled = True
        panNlaRad.Enabled = True
        picXYZ.Visible = True
        picXYZ.Refresh()
        radDRQ(2).Enabled = False
        For i = 7 To 10
          radDRQ(i).Enabled = False
        Next
    End Select

    '
    '

  End Sub
  Private Sub picDRQ_Paint(sender As Object, e As System.Windows.Forms.PaintEventArgs) Handles picDRQ.Paint
    If IsNothing(RezGraphics) Then Exit Sub
    RezGraphics.Rmin = -1
    RezGraphics.Rmax = CSng(cboSKAL.Text)
    RezGraphics.Skalier()
    Try


      RezGraphics.GraphBounds = New RectangleF(0.0 * sender.width, 0.0 * sender.height, 1.0 * sender.width, 1.0 * sender.height)
      RezGraphics.PicREFGraph(sender, e.Graphics)
      Application.DoEvents()
    Finally
    End Try
  End Sub

  Private Sub picLAB_Paint(sender As Object, e As System.Windows.Forms.PaintEventArgs) Handles picLAB.Paint
    If IsNothing(RezGraphics) Then Exit Sub
    RezGraphics.GraphBounds = New RectangleF(0.0 * sender.width, 0.0 * sender.height, 1.0 * sender.width, 1.0 * sender.height)

    RezGraphics.PicLABGraph(sender, e.Graphics)

  End Sub

  Private Sub picXYZ_Paint(sender As Object, e As System.Windows.Forms.PaintEventArgs) Handles picXYZ.Paint
    If IsNothing(RezGraphics) Then Exit Sub
    RezGraphics.GraphBounds = New RectangleF(0.0 * sender.width, 0.0 * sender.height, 1.0 * sender.width, 1.0 * sender.height)
    RezGraphics.PicXYZGraph(sender, e.Graphics)
  End Sub

  Private Sub radNLA_CheckedChanged(sender As Object, e As System.EventArgs) Handles _
   radNLA_0.CheckedChanged, radNLA_1.CheckedChanged, radNLA_2.CheckedChanged, radNLA_3.CheckedChanged, radNLA_4.CheckedChanged
    Dim index As Integer
    If sender.name = "" Then Exit Sub
    index = CInt(sender.Name.SUBSTRING(7, 1))
    RezGraphics.Knlz = index
    picDRQ.Refresh()
    picLAB.Refresh()
    picXYZ.Refresh()
  End Sub

  Private Sub radWIN_CheckedChanged(sender As Object, e As System.EventArgs) Handles _
   radWIN_00.CheckedChanged, radWIN_01.CheckedChanged, radWIN_02.CheckedChanged, radWIN_03.CheckedChanged, radWIN_04.CheckedChanged, radWIN_05.CheckedChanged, radWIN_06.CheckedChanged, radWIN_07.CheckedChanged, radWIN_08.CheckedChanged
    Dim index As Integer
    If sender.name = "" Then Exit Sub
    index = CInt(sender.Name.SUBSTRING(7, 2))
    RezGraphics.Kwop = index
    picDRQ.Refresh()
    picLAB.Refresh()
    picXYZ.Refresh()
  End Sub

  Private Sub btnKOP_Click(sender As System.Object, e As System.EventArgs) Handles btnKOP.Click
    Dim bmp As Bitmap
    Clipboard.Clear()
    If flgMerk.Visible Then
      '
      '
      '
      '
      '
      'Schreiben flgMERK nach Clipboard
      '
      '
      '
      '
      flgMerk.SelectionMode = DataGridViewSelectionMode.ColumnHeaderSelect
      flgMerk.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableWithAutoHeaderText
      flgMerk.SelectAll()

      If flgMerk.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
        Clipboard.SetDataObject(flgMerk.GetClipboardContent)
      End If
      flgMerk.ClearSelection()
    ElseIf picDRQ.Visible Then
      bmp = New Bitmap(picDRQ.Width, picDRQ.Height)
      Dim gr As Graphics = Graphics.FromImage(bmp)
      gr.Clear(picDRQ.BackColor)
      RezGraphics.PicREFGraph(picDRQ, gr)
      Clipboard.SetDataObject(bmp, True)
    ElseIf picLAB.Visible Then
      bmp = New Bitmap(picLAB.Width, picLAB.Height)
      Dim gr As Graphics = Graphics.FromImage(bmp)
      gr.Clear(picLAB.BackColor)
      RezGraphics.PicLABGraph(picLAB, gr)
      Clipboard.SetDataObject(bmp, True)
    ElseIf flgRwert.Visible Then
      '
      '
      '
      '
      '
      'Schreiben flgMERK nach Clipboard
      '
      '
      '
      '
      flgRwert.SelectionMode = DataGridViewSelectionMode.ColumnHeaderSelect
      flgRwert.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableWithAutoHeaderText
      flgRwert.SelectAll()

      If flgRwert.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
        Clipboard.SetDataObject(flgRwert.GetClipboardContent)
      End If
      flgRwert.ClearSelection()
    ElseIf picXYZ.Visible Then
      bmp = New Bitmap(picXYZ.Width, picXYZ.Height)
      Dim gr As Graphics = Graphics.FromImage(bmp)
      gr.Clear(picXYZ.BackColor)
      RezGraphics.PicXYZGraph(picXYZ, gr)
      Clipboard.SetDataObject(bmp, True)
    End If
  End Sub



  Private Sub btnDRU_Click(sender As Object, e As System.EventArgs) Handles btnDRU.Click
    Dim i As Integer
    If flgMerk.Visible Or radGPL(0).Checked Then
      If chkDru.Checked Then
        Call FarbReport.SubDrucken()
      Else
        Call MerkDruck.DruckenAllgemein(0)
      End If

      '
      '
      '
      '
      '

    ElseIf radGPL(3).Checked Then
      For i = 0 To radDRQ.Count - 1
        If radDRQ(i).Checked Then
          MerkDruck.Ueberschrift = radDRQ(i).Text
          Exit For
        End If
      Next i
      If (i = 10 Or i <= 5) And GrpRwerte(0)(0).ReTr = 1 Then
        Mid(MerkDruck.Ueberschrift, 1, 1) = "T"
      End If
      MerkDruck.Ueberschrift = InputBox(Texxt(4658), Texxt(4659), MerkDruck.Ueberschrift)
      If MerkDruck.Ueberschrift = "" Then
        Exit Sub
      End If
      MerkDruck.Kommentar = txtKOM.Text
      MerkDruck.DataGrid = flgRwert
      Call MerkDruck.DruckenAllgemein(1)

    End If
  End Sub





  Private Sub chkLinear_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkLinear.CheckedChanged
    Dim i As Integer
    If Not radGPL(1).Checked Then Exit Sub
    RezGraphics.ipl = 0
    If chkLinear.Checked Then
      RezGraphics.ipl = 0
    Else
      For i = 7 To radDRQ.Count - 1
        If radDRQ(i).Checked Then
          RezGraphics.ipl = 2
        End If
      Next
    End If
    picDRQ.Refresh()
  End Sub

  Private Sub cboSKAL_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboSKAL.SelectedIndexChanged
    If Not radGPL(1).Checked Then Exit Sub
    RezGraphics.Rmax = CSng(cboSKAL.Text)
    picDRQ.Refresh()
  End Sub

  Private Sub picLAB_Resize(sender As Object, e As System.EventArgs) Handles picLAB.Resize
    picLAB.Refresh()
  End Sub

  Private Sub picDRQ_Resize(sender As Object, e As System.EventArgs) Handles picDRQ.Resize
    picDRQ.Refresh()
  End Sub

  Private Sub picXYZ_Resize(sender As Object, e As System.EventArgs) Handles picXYZ.Resize
    picXYZ.Refresh()
  End Sub

  Private Sub btnAUS_Click(sender As Object, e As System.EventArgs) Handles btnAUS.Click
    Dim printdoc As PrintDocument
    Dim ppv As PrintPreviewDialog
    Dim i As Integer
    '
    '
    For i = 0 To radGPL.Count
      If radGPL(i).Checked Then
        MerkDruck.PicNr = i
        Exit For
      End If
    Next
    ppv = New PrintPreviewDialog
    printdoc = New PrintDocument
    printdoc.PrinterSettings = MnPrintSet
    ppv.Document = printdoc
    AddHandler printdoc.PrintPage, AddressOf MerkDruck.DruckPicturebox
    printdoc.DefaultPageSettings.Landscape = True
    ppv.WindowState = FormWindowState.Maximized
    'Preview anzeigen 
    ppv.ShowDialog()
    RemoveHandler printdoc.PrintPage, AddressOf MerkDruck.DruckPicturebox
    printdoc.Dispose()



  End Sub


  Private Sub txt_Enter(sender As Object, e As System.EventArgs) Handles txtANZSuch.Enter, txtGRA.Enter, txtKDE.Enter
    sender.tag = sender.text
  End Sub

  Private Sub txt_TextChanged(sender As Object, e As System.EventArgs) Handles txtANZSuch.TextChanged, txtGRA.TextChanged, txtKDE.TextChanged, txtTra.TextChanged
    If Not IsNumeric(sender.text) Then
      sender.text = sender.tag
    End If
  End Sub



  Private Sub btnUser_Click(sender As Object, e As System.EventArgs) Handles btnUser.Click
    Cursor = Cursors.WaitCursor
    AusgabeUser.AusgabeQUALITA(FarbWerte, GrpRwerte)
    Cursor = Cursors.Default
  End Sub
  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtANZSuch.Validating, txtGRA.Validating, txtKDE.Validating, txtTra.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub
  '
  '
  '
  '
  'Subroutinen für Vergleich von Farbwerten
  '
  '
  '
  Private Sub chkLAB_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkLAB.CheckedChanged
    If IsNothing(dbgFarbwerte) OrElse dbgFarbwerte.Columns.Count = 0 Then Exit Sub
    If chkLAB.Checked Then
      dbgFarbwerte.Columns("X").HeaderText = Trim(TexKt(17108)) & Space(1) & MenueParam.Normfa(0).NormKenn
      dbgFarbwerte.Columns("Y").HeaderText = Trim(TexKt(17111)) & Space(1) & MenueParam.Normfa(0).NormKenn
      dbgFarbwerte.Columns("Z").HeaderText = Trim(TexKt(17112)) & Space(1) & MenueParam.Normfa(0).NormKenn
    Else
      dbgFarbwerte.Columns("X").HeaderText = Trim(TexKt(10065)) & Space(1) & MenueParam.Normfa(0).NormKenn
      dbgFarbwerte.Columns("Y").HeaderText = Trim(TexKt(10066)) & Space(1) & MenueParam.Normfa(0).NormKenn
      dbgFarbwerte.Columns("Z").HeaderText = Trim(TexKt(10067)) & Space(1) & MenueParam.Normfa(0).NormKenn
    End If
  End Sub


  Sub MakeFarbwerte(ByRef dbgFarbWerte As DataGridView)
    Dim i As Integer

    dbgFarbWerte.Columns.Add("X", Trim(TexKt(10065)) & Space(1) & MenueParam.Normfa(0).NormKenn)
    dbgFarbWerte.Columns.Add("Y", Trim(TexKt(10066)) & Space(1) & MenueParam.Normfa(0).NormKenn)
    dbgFarbWerte.Columns.Add("Z", Trim(TexKt(10067)) & Space(1) & MenueParam.Normfa(0).NormKenn)
    dbgFarbWerte.Columns(0).HeaderText = Trim(TexKt(10065)) & Space(1) & MenueParam.Normfa(0).NormKenn
    dbgFarbWerte.Columns(1).HeaderText = Trim(TexKt(10066)) & Space(1) & MenueParam.Normfa(0).NormKenn
    dbgFarbWerte.Columns(2).HeaderText = Trim(TexKt(10067)) & Space(1) & MenueParam.Normfa(0).NormKenn
    For i = 0 To dbgFarbWerte.Columns.Count - 1
      dbgFarbWerte.Columns(i).Width = 120
      dbgFarbWerte.Columns(i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      dbgFarbWerte.Columns(i).DefaultCellStyle.Format = "###0.000"
    Next
    dbgFarbWerte.Rows.Add(New DataGridViewRow)
    dbgFarbWerte.Rows.Add(New DataGridViewRow)
    dbgFarbWerte.Rows.Add(New DataGridViewRow)
    dbgFarbWerte.Rows.Add(New DataGridViewRow)
    dbgFarbWerte.Rows(0).HeaderCell.Value = "min"
    dbgFarbWerte.Rows(1).HeaderCell.Value = "max"
    dbgFarbWerte.Rows(2).HeaderCell.Value = "Diff."
    dbgFarbWerte.Rows(3).HeaderCell.Value = "step"
    dbgFarbWerte.EnableHeadersVisualStyles = False
    For i = 0 To dbgFarbWerte.Rows.Count - 1
      dbgFarbWerte.RowHeadersWidth = 55
    Next
    'dbgFarbWerte.EnableHeadersVisualStyles = False
    For i = 0 To dbgFarbWerte.Columns.Count - 1
      dbgFarbWerte.Rows(0).Cells(i).Value = 0.0
      dbgFarbWerte.Rows(1).Cells(i).Value = MenueParam.Normfa(0).NormFakt(i)
      dbgFarbWerte.Rows(2).Cells(i).Value = 10.0
      dbgFarbWerte.Rows(3).Cells(i).Value = 1.0
      dbgFarbWerte.Columns(i).ValueType = GetType(Single)
      dbgFarbWerte.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
    Next

    dbgFarbWerte.ColumnHeadersDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
    dbgFarbWerte.AllowUserToOrderColumns = False
    dbgFarbWerte.AllowUserToAddRows = False
    dbgFarbWerte.AllowUserToDeleteRows = False
    dbgFarbWerte.SelectionMode = DataGridViewSelectionMode.CellSelect
    dbgFarbWerte.RowHeadersVisible = True

    dbgFarbWerte.AllowUserToOrderColumns = False
    dbgFarbWerte.AllowUserToAddRows = False
    dbgFarbWerte.AllowUserToResizeColumns = False
    dbgFarbWerte.AllowDrop =
      dbgFarbWerte.EnableHeadersVisualStyles = False

    dbgFarbWerte.ColumnHeadersDefaultCellStyle.NullValue = False


  End Sub

  Private Sub GridMethAux(ByRef Ier As Integer)
    Dim i As Integer
    Dim j As Integer
    '
    Ier = 0
    panStandard.Visible = False
    panAuxiliary.Visible = True
    '
    '
    'Positionierungen Qualitaetskontrolle
    '

    '
    ' Spaltenbeschriftung und Spaltenbreite gemaess Pruefmethode
    '
    '
    'Alle controls werden unvisible gesetzt
    '
    '
    btnLOE.Visible = True
    btnLOE.Enabled = True
    txtKOM.Visible = True
    lblKOM.Visible = True
    lblSTB.Visible = True
    lblTYP.Visible = True
    '

    lblSTB.Text = Texxt(1205)
    chkABS.CheckState = MenueParam.Menue.AbsID
    '
    '
    nreiwied = 0
    irei = 0
    nreiwmax = 0
    For i = 0 To nrei
      ireimeth(i) = 0
    Next i
    '
    '
    '
    'ipogrd Adreese in Uebstr für Standardwerte
    '
    '
    '
    'Kpogrd Adresse in Texte-Datei ab TEXT_ID=1200
    '
    '
    '
    '
    For i = 0 To ngrd
      ipogrd(i) = 0
      Kpogrd(i) = 0
    Next i
    'rnreimeth
    For j = 0 To 4
      icamgrd(j, 0) = 0
      icamgrd(j, 1) = 0
    Next j
    MenueParam.Messg.MeArtLock = -1
    '
    '
    '
    MenueParam.Menue.IDaeArt = 0


    'Methoden abpruefen;Ueberschriften festlegen;
    'Controls aktivieren/deaktivieren
    'Spaltenbreite festlegen

    '
    'Farbabstaende Farbkoordinaten 

    ipogrd(0) = 30
    ipogrd(1) = 0
    ipogrd(2) = 30
    Kpogrd(0) = 120
    Kpogrd(1) = 0
    Kpogrd(2) = 120
    '
    '   Caption-Nummern fuer bcsref.frm
    '
    ireimeth(0) = 5
    ireimeth(1) = 6
    ireimeth(2) = -1
    '
    '
    '
    'Feldnummern für CAMP-Feld
    '
    '
    '
    '   Anzahl Wiederholungen fuer Caption-Nummern (mit "x.Messung")
    '
    ireiwied = 0



    icore = 1
    colm = 4
    '
    '
    '
    'Festlegen der sichtbaren controls
    '
    '
    '
    lblGLZAux.Visible = True
    cboGLZAux.Visible = True
    '
    cboGLZAux.Text = Winkel(0).GK(0)
    '
    'Absolutwerte Bezug
    '
    chkABS.Visible = True
    '
    '
    '
    '
    For i = 2 * icore + 1 To 3 * icore
      ipogrd(i) = 28 + i - 2 * icore - 1
      Kpogrd(i) = 4 * ipogrd(i)
    Next i
    '
    '
    '
    'Eingabetextfeld aktivieren
    '
    '
    '
    If colm > 3 * icore + 1 Then
      If BitWrt(20, MenueParam.User.Writ) Then
        'TDBTab.AllowUpdate = True
      End If
      '
    End If
    txtKOM.Enabled = True
    '
    '
    'Festlegen der unsichtbaren bzw. inaktivierten controls
    '
    '
    '
    '
    If Not BitWrt(2, MenueParam.User.Visbl) Then
      chkABS.Visible = False
    End If
    If Not BitWrt(2, MenueParam.User.Enabl) Then
      chkABS.Enabled = False
    End If
    '
    If Not BitWrt(6, MenueParam.User.Visbl) Then
      lblGLZAux.Visible = False
      cboGLZAux.Visible = False
    End If
    If Not BitWrt(6, MenueParam.User.Enabl) Then
      cboGLZAux.Enabled = False
    End If
    If Not BitWrt(7, MenueParam.User.Visbl) Then
      lblINRAux.Visible = False
      cboINRAux.Visible = False
    End If
    If Not BitWrt(7, MenueParam.User.Enabl) Or MenueParam.Messg.GlIn = 0 Then
      cboINRAux.Enabled = False
    End If
    If Not BitWrt(8, MenueParam.User.Visbl) Then
      lblINDAux.Visible = False
      cboINDAux.Visible = False
    End If
    If Not BitWrt(8, MenueParam.User.Enabl) Then
      cboINDAux.Enabled = False
    End If
    '
    '
    '
    'aktivieren bzw. deaktivieren der R-Werteingabe
    '
    '
    Call NameRwe()
  End Sub
  '
  Private Sub btnCalcRwerte_Click(sender As Object, e As System.EventArgs) Handles btnCalcRwerte.Click
    Dim ier As Integer
    Dim iee As Integer
    Dim EPS As Single = 0.01
    Dim i As Integer
    Dim kw As Integer
    Dim NWE As Integer
    Dim ifw As Integer
    Dim ID As Integer
    Dim AuxText As String
    Dim FAKO(0, 2) As Single
    Dim FAKM(0, 2) As Single
    Dim FADI(0, 2) As Single
    Dim Diff(2) As Single
    Dim stpp(0, 2) As Single
    Dim XYZS(0, 0, 2) As Single
    Dim XYZI(0, 0, 2) As Single
    Dim DE, DL, DC, DH, DA, DB As Single
    Dim UseBit As Boolean
    If radANGL_0.Checked Then
      '
      'min. Abweichung
      '
      ifw = 1
    Else
      '
      'max. Entropie
      '
      ifw = 2
    End If
    chkPARAM.CheckState = 0
    NWE = Winkel.Wsol.Nwe
    Dim Refval As RefValue
    '
    'R-Werte aus X,Y,Z berechnen
    '
    '
    Cursor = Cursors.WaitCursor
    GrpRwerteCalc("X")(KeyRe(0)).ID = 0
    GrpRwerteCalc("X")(KeyRe(0)).Nr = 0
    GrpRwerteCalc("X")(KeyRe(0)).kwb = 0
    GrpRwerteCalc("X")(KeyRe(0)).IVoNa = True
    For i = 0 To 2
      '
      'X,Y,Z min
      '
      FAKO(0, i) = dbgFarbwerte.Rows(0).Cells(i).Value
      '
      'X,Y,Z max
      '
      FAKM(0, i) = dbgFarbwerte.Rows(1).Cells(i).Value
      '
      'X,Y,Z diff
      '
      Diff(i) = dbgFarbwerte.Rows(2).Cells(i).Value
      'X,Y,Z step
      '
      stpp(0, i) = dbgFarbwerte.Rows(3).Cells(i).Value
    Next
    For kw = 0 To Winkel.Km - 1
      For i = 0 To Winkel.Wsol.Nwe - 1
        GrpRwerteCalc("X")(KeyRe(2)).RefKurv(Winkel(kw).Chrm).R(i) = Winkel(kw).GK(0)
        GrpRwerteCalc("X")(KeyRe(3)).RefKurv(Winkel(kw).Chrm).R(i) = Winkel(kw).GK(9)
      Next i
    Next kw
    ID = 0
    GrpRwerte("W").clear()
    GrdArray.Rows.Clear()
    '
    'Warnungen werden nicht angezeigt
    '
    UseBit = BitWrt(28, MenueParam.User.Writ)
    MenueParam.User.Writ = BitWrtID(False, 28, MenueParam.User.Writ)

    Do
      Do
        Do

          If chkLAB.Checked Then
            '
            'FAKO=L*,a*,b*
            '
            AuxText = "Lab"
            Call quali.RefFarbWerte(Winkel, 5, 3, ifw, FAKO, XYZS, XYZI, GrpRwerteCalc("X"), ier)
            '
            'prüfen, ob Ausgangswert erreicht
            '

            Call quali.FarbDifferenzXYZ(0, 0, Winkel, XYZS, XYZI, DE, DL, DC, DH, DA, DB, iee)
            '
            '
            If DE > EPS Then
              ier = -1
            End If
          Else
            '
            '
            'FAKO=X,Y,Z
            '
            AuxText = "XYZ"
            Call quali.RefFarbWerte(Winkel, 4, 3, ifw, FAKO, XYZS, XYZI, GrpRwerteCalc("X"), ier)

            '
            'prüfen, ob Ausgangswert erreicht
            '

            Call quali.FarbDifferenzXYZ(0, 0, Winkel, XYZS, XYZI, DE, DL, DC, DH, DA, DB, iee)
            '
            '
            If DE > EPS Then
              ier = -1
            End If
          End If
          '

          If ier = 0 And iee = 0 Then
            RefRow = GrdArray.NewRow
            GrdArray.Rows.Add(RefRow)
            RefRow(0) = ID
            RefRow(1) = "T" & AuxText & Format(FAKO(0, 0), "0000") & Format(FAKO(0, 1), "0000") & Format(FAKO(0, 2), "0000")
            RefRow(2) = "X"
            RefRow(3) = " "
            RefRow(4) = "@TMW"
            '
            '
            '
            Refval = GrpRwerteCalc("X")(KeyRe(0)).clone
            Refval.kwb = 0
            Refval.IVoNa = True
            Refval.Itp = True
            Refval.Name = RefRow(1)
            Refval.QuControl = New QuControls
            Refval.QuControl.Cart = RefRow(4)
            GrpRwerte("W").Add(KeyName(ID), Refval)
            '

            GrpRwerteCalc("X")(KeyRe(1)).RefKurv.clear()
            ID = ID + 1
            '
            'berechnete Kurven werden als Referenzkurven für Probe verwendet
            '
            Call ADDCurves(GrpRwerteCalc("X")(KeyRe(1)).RefKurv)
            For kw = 0 To Winkel.Km - 1
              For i = 0 To Winkel.Wsol.Nwe - 1
                GrpRwerteCalc("X")(KeyRe(1)).RefKurv(Winkel(kw).Chrm).R(i) = GrpRwerteCalc("X")(KeyRe(0)).RefKurv(Winkel(kw).Chrm).R(i)
              Next i
            Next kw
            If chkLAB.Checked Then
              'FAKO=DL*,Da*,Db*
              '
              Call quali.RefFarbWerte(Winkel, 2, 1, ifw, stpp, XYZS, XYZI, GrpRwerteCalc("X"), ier)
            Else
              '
              'FAKO=DX,DY,DZ
              '
              Call quali.RefFarbWerte(Winkel, 1, 1, ifw, stpp, XYZS, XYZI, GrpRwerteCalc("X"), ier)
              '
            End If
            If ier = 0 Then
              RefRow = GrdArray.NewRow
              GrdArray.Rows.Add(RefRow)
              RefRow(0) = ID
              RefRow(1) = "P" & AuxText & Format(FAKO(0, 0), "0000") & Format(FAKO(0, 1), "0000") & Format(FAKO(0, 2), "0000")
              RefRow(2) = " "
              RefRow(3) = " "
              RefRow(4) = "@PMW"
              '
              '
              '
              Refval = GrpRwerteCalc("X")(KeyRe(0)).clone
              Refval.kwb = 0
              Refval.IVoNa = True
              Refval.Itp = False
              Refval.Name = RefRow(1)
              Refval.QuControl = New QuControls
              Refval.QuControl.Cart = RefRow(4)
              GrpRwerte("W").Add(KeyName(ID), Refval)
            End If
            '
            '
            '
            '
          End If
          FAKO(0, 0) = FAKO(0, 0) + Diff(0)
          ID = ID + 1

        Loop Until (FAKO(0, 0) > FAKM(0, 0))
        FAKO(0, 0) = dbgFarbwerte.Rows(0).Cells(0).Value
        FAKO(0, 1) = FAKO(0, 1) + Diff(1)
      Loop Until (FAKO(0, 1) > FAKM(0, 1))
      FAKO(0, 1) = dbgFarbwerte.Rows(0).Cells(1).Value
      FAKO(0, 2) = FAKO(0, 2) + Diff(2)
    Loop Until (FAKO(0, 2) > FAKM(0, 2))
    MenueParam.User.Writ = BitWrtID(UseBit, 28, MenueParam.User.Writ)
    btnVERAux.Visible = True
    Cursor = Cursors.Default
    '
    '
  End Sub
  Private Sub btnVERAux_Click(sender As Object, e As System.EventArgs) Handles btnVERAux.Click
    Dim IuntID(0 To 2) As Integer
    Dim Nqu As Integer
    Dim Ianwsg As Integer
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim iee As Integer
    Dim NoMerks As Boolean
    chkPARAM.CheckState = 1
    chkPARAM.CheckState = 0
    NoMerks = True
    GrdArray.AcceptChanges()
    '
    '
    FarbWerte.Kommentar = txtKOM.Text

    '
    For i = 0 To FarbWerte.Count - 1
      If FarbWerte(i).CountMerk > 0 Then
        NoMerks = False
        Exit For
      End If
    Next i
    If NoMerks And Not (MenueParam.MethID = 1 Or MenueParam.MethID = 49) Then
      MsgBox(Texxt(4123))
      Exit Sub
    End If

    '

    Cursor = Cursors.WaitCursor
    panResult.Visible = False
    PanQual.Visible = False
    lblQCE.Visible = True
    Application.DoEvents()
    '
    '
    'Lösche Parameter
    '
    '
    '
    For i = 0 To FarbWerte.Count - 1
      FarbWerte(i).clear()
    Next i
    '
    '

    Nqu = GrpRwerte("W").Count
    '
    '
    Cursor = Cursors.WaitCursor
    PanQual.Visible = False
    panResult.Visible = False
    lblQCE.Visible = True
    Call quali.CalcQualWerte(Winkel, GrpRwerte, FarbWerte, iee)
    If iee <> 0 Then GoTo QceWeiter
    lblQCE.Visible = False
    ier = iee
    Cursor = Cursors.Default
    If ier <> 0 Then
      GoTo QceWeiter
    End If
    '
    '
    '
    'Prüfen, ob Merkale berechnet wurden
    '
    '
    '
    NoMerks = True
    For Ianwsg = 0 To FarbWerte.Count - 1
      'Anweisungen
      For i = 0 To FarbWerte(Ianwsg).Count - 1
        'Kurven
        For k = 0 To FarbWerte(Ianwsg)(i).Count - 1
          'Lichtarten
          For kw = 0 To FarbWerte(Ianwsg)(i)(k).Count - 1
            'Winkel/Messgeometrien
            For j = 0 To FarbWerte(Ianwsg)(i)(k)(kw).Count - 1
              'Merkmale
              If Not IsDBNull(FarbWerte(Ianwsg)(i)(k)(kw)(j)) AndAlso FarbWerte(Ianwsg)(i)(k)(kw)(j) <> "" Then
                NoMerks = False
                Exit For
              End If
            Next j
          Next kw
        Next k
      Next i
    Next Ianwsg
    If NoMerks Then
      MsgBox(Texxt(4154))
      PanQual.Visible = True
      panResult.Visible = False
      Exit Sub
    End If
    If ier <> 0 Then GoTo QceWeiter
    '

    ''
    '
    '
    '
    '
    'Ausgabe (Benutzer (Bit=1) oder Normal)
    '
    If BitWrt(4, MenueParam.User.Drum) Then
      Cursor = Cursors.WaitCursor
      Ausgabe.AusgabeQUALITA(FarbWerte, GrpRwerte)
      Cursor = Cursors.Default
    Else
      Ausgabe.Ausgabemerkmale(FarbWerte, GrpRwerte)
    End If
    '
    '
    '
    '**************************************
    '**************************************
    '
    PanQual.Visible = False
    panResult.Visible = True
    panWinChk.Visible = True
    panNlaChk.Visible = True
    panWinRad.Visible = False
    panNlaRad.Visible = False
    '
    radGPL(3).Checked = True
    '

    panResult.Visible = True
    btnVOR.Enabled = True
    btnDRU.Enabled = True
    btnVOR.PerformClick()
    '
    btnUser.Visible = True
    Exit Sub
QceWeiter:
    If ier = -91 Then
      MsgBox("Too much parameters for the calculation of signs")
      MsgBox("COLwinFORTRAN.Calcwerte")
    End If
    If ier = -92 Then
      MsgBox("Error in the parameter class (Wert=Null or ID=-1)")
    End If
    Cursor = Cursors.Default
    PanQual.Visible = True
    panResult.Visible = False
    lblQCE.Visible = False
    'Exit Sub
    'ErrReihen:
    '    MsgBox(Texxt(ErrCode), 0)
    '    PanQual.Visible = True
    '    panResult.Visible = False
    '    Cursor = Cursors.Default

  End Sub




End Class
Public Class GridMerkAdr
  Dim MnIanwsg As Integer
  Dim Mni As Integer
  Dim MnKn As Integer
  Dim Mnkw As Integer
  Dim Mnj As Integer
  Public Sub New(Ianwsg As Integer, i As Integer, kn As Integer, kw As Integer, j As Integer)
    MnIanwsg = Ianwsg
    Mni = i
    MnKn = kn
    Mnkw = kw
    Mnj = j
  End Sub
  ReadOnly Property Ianwsg As Integer
    Get
      Ianwsg = MnIanwsg
    End Get
  End Property
  ReadOnly Property i As Integer
    Get
      i = Mni
    End Get
  End Property
  ReadOnly Property kn As Integer
    Get
      kn = MnKn
    End Get
  End Property
  ReadOnly Property kw As Integer
    Get
      kw = Mnkw
    End Get
  End Property
  ReadOnly Property j As Integer
    Get
      j = Mnj
    End Get
  End Property
End Class