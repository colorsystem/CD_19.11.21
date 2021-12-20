Option Explicit On
Option Compare Text
Option Strict Off
Public Class frmColorSuchKorr

  Inherits Windows.Forms.Form


  Dim iee As Integer

  Dim Kdru As Integer
  Dim HlfMeng As Single
  Dim Dygkwrt As DataTable
  Dim SqlStmt As String
  Dim SqlEtmt As String
  Dim ReUn As String
  Dim KeyMenge As String
  Dim i100 As Integer
  '
  'Temporäre Tabellen
  '
  '
  '
  Dim RezptGrid As List(Of HandleRezC1Screen)
  Dim RezGrid As HandleRezC1Screen

  '
  Dim Colorthek As Boolean
  Dim Umr As RezeptUmrechnung
  Dim RezSozpt As RecipesGrp
  Dim GrpRwerte As RefValuesGrp
  Dim GrpRwrtRest As RefValuesGrp
  Dim RestFarb As Colorant
  Dim RwWrFarbe As ReadWriteFarbe
  Dim RwWrGrund As ReadWriteGrund
  Dim OptGesamt As OpticalData
  Dim RwWrRezept As ReadWriteRezept
  Dim ReWrRwert As ReadWriteRwert
  Dim FarbWrt As ValuesGrps
  Dim WinHilf As AngGeos
  Dim CalcRezept As RezeptBerechnung
  Dim PicAufbau As List(Of HandlePictures)
  Dim Picauf As HandlePictures
  Dim claDRU As HandlePlottDruck
  Dim quali As QualKontrolle
  Dim HandleRezept As HandleRezGeneral
  Dim HandleRezeptN As HandleRezGeneral
  Dim HandleRwrt As HandleRwerte
  Dim RezHLFSelecID As List(Of Integer)
  Dim RezHlfAvailID As List(Of Integer)
  '
  '
  Dim KeyD As String
  Dim FaId As Integer
  Dim RezID As Integer
  Dim RcmdRef As Integer
  Dim ier As Integer

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

  'Anzahl Reflexionskurven (+Zusatzinformation) QU-Kontrolle
  '
  Dim Iprn As Integer
  '
  '
  '

  Dim i As Integer
  Dim l As Integer
  Dim k As Integer
  Dim j As Integer
  Dim kw As Integer
  Dim nkw As Integer
  '
  '
  '
  'Message Kennung
  '
  '
  '
  Dim imsg As Integer            'Messagebox (Rückgabewert)
  ' Dim Igx As Integer
  Dim HscMax As Integer
  Dim InoMeng As Single
  Dim kwb As Integer
  Dim DatenFarb As HandleRezDsetFarb
  Dim DsetRezpt As New DataSet
  Dim OleAdRezpt As New OleDbDataAdapter
  Dim DatReadRezpt As OleDbDataReader
  Dim CmdRezpt As New OleDbCommand
  Dim ViewHlfAvail As DataView
  Dim ViewHlfSelec As DataView
  Dim ViewRezept As DataView
  Dim WithEvents ConnRezept As BindingSource
  Dim TblRezept As DataTable
  Dim TblRefGew As DataTable
  Dim dbgREZ As List(Of C1TrueDBGrid)
  Dim DbErr As Integer
  Dim KdrAll As Integer
  '
  '
  'Farb-/Bindemittel für Colortheksuche
  '
  Dim OleFarb As OleDbDataAdapter
  Dim CmdFarb As OleDbCommand
  Dim FarbTab As DataTable

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
  Dim OldTabIndex As Integer
  Dim OldMischID As Integer
  ' Dim Create As BCSWINReadWriteDaten.CreateTables
  '
  '
  '
  Dim ChkUNT As New List(Of CheckBox)
  Dim chkMessUnt As New List(Of CheckBox)
  Dim chkMessVor As New List(Of CheckBox)
  Dim chkMessNch As New List(Of CheckBox)
  Dim btnMessVor As New List(Of Button)
  Dim btnMessNch As New List(Of Button)
  Dim btnMessUnt As New List(Of Button)
  Dim lblMessVor As New List(Of Label)
  Dim lblMessNch As New List(Of Label)
  Dim lblMessUnt As New List(Of Label)
  Dim lblDickKor As New List(Of Label)
  Dim txtDickKor As New List(Of TextBox)
  Dim lblDickNch As New List(Of Label)
  Dim txtDickNch As New List(Of TextBox)
  Dim btnMessVOO As New List(Of Button)
  Dim lblMessVOO As New List(Of Label)
  Dim SplitContKorrRezepte As New List(Of SplitContainer)
  Dim RezeptGrid As RecipesGrp
  Dim RefWerteUNT As List(Of RefValue)
  Dim RefWertetyp As List(Of RefValue)
  Dim RefWertesmp As List(Of RefValue)
  Dim cbomim As New List(Of ComboBox)
  Dim txtMng As New List(Of TextBox)
  Dim txtPro As New List(Of TextBox)
  Dim txtSum As New List(Of TextBox)
  Dim lblVOR As New List(Of Label)
  Dim lblmng As New List(Of Label)
  Dim cboDRU As New List(Of ComboBox)
  Dim btnDRU As New List(Of Button)
  Dim txtBIS As New List(Of TextBox)
  Dim txtVON As New List(Of TextBox)
  Dim chkDatum As New List(Of CheckBox)
  Dim txtSUC As New List(Of TextBox)
  Dim lblBIS As New List(Of Label)
  Dim radRwerte As New List(Of RadioButton)
  Dim chkNextWIN As New List(Of CheckBox)

  Dim Arr(6) As Single
  Dim ToolTipSuchKorr As ToolTip
  Dim ListFarbid() As Integer




  '
  '
  '
  '
  '
  '
  '
  '
  Dim KeySor As String
  Dim TypID() As Integer = {-1, -1}
  Dim SmpID() As Integer = {-1, -1}
  Dim UntID() As Integer = {-1, -1}
  Dim DumID() As Integer = {-1, -1}

  Dim RezDruck As HandlePlottDruck
  Dim RezGraphics As HandleRezGrafik
  Dim RezCheckRad As HandleCheckRad
  Dim WiFawrt As Integer
  Dim parform As Form
  Dim VKwb As Object
  Dim RzNr As String
  Dim OleAdRezN As Object
  '

  Public Sub New(ByVal value As Boolean)

    ' Dieser Aufruf ist für den Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
    Colorthek = value
  End Sub '







  Overloads Sub DbgRezCustomize(dbgrez As C1TrueDBGrid)
    dbgrez.AllowSort = False
    dbgrez.AllowUpdate = False
    dbgrez.AllowAddNew = False
    dbgrez.AlternatingRows = True
    dbgrez.EvenRowStyle.BackColor = Color.FloralWhite
    dbgrez.OddRowStyle.BackColor = Color.Lavender
    dbgrez.Splits(0).DisplayColumns(1).Width = 70
    dbgrez.Splits(0).DisplayColumns(2).Width = 150
    dbgrez.Splits(0).DisplayColumns(3).Width = 100
    dbgrez.Splits(0).DisplayColumns(0).Visible = False
    dbgrez.Splits(0).DisplayColumns(1).Visible = True
    dbgrez.Splits(0).DisplayColumns(2).Visible = True
    dbgrez.Splits(0).DisplayColumns(3).Visible = True
    dbgrez.Splits(0).DisplayColumns(1).Style.HorizontalAlignment = AlignHorzEnum.Near
    dbgrez.Splits(0).DisplayColumns(1).HeadingStyle.HorizontalAlignment = AlignHorzEnum.Near
    dbgrez.Splits(0).DisplayColumns(2).Visible = True
    dbgrez.Splits(0).DisplayColumns(3).Visible = True
    dbgrez.Splits(0).DisplayColumns(4).Visible = False
    dbgrez.Splits(0).DisplayColumns(5).Visible = False
    dbgrez.Splits(0).DisplayColumns(6).Visible = False
    dbgrez.Splits(0).DisplayColumns(7).Visible = False
    dbgrez.Columns(0).Caption = ""
    dbgrez.Columns(1).Caption = Texxt(2205)
    dbgrez.Columns(2).Caption = Texxt(2206)
    dbgrez.Columns(3).Caption = Texxt(2207)
    dbgrez.Columns(4).Caption = ""
    dbgrez.Columns(5).Caption = ""
    dbgrez.Splits(0).ExtendRightColumn = True
    'dbgrez.HoldFields()
  End Sub
  Overloads Sub DbgRezCustomize(dbgrez As DataGridView)
    dbgrez.Columns(0).SortMode = DataGridViewColumnSortMode.NotSortable
    dbgrez.AllowUserToAddRows = False
    dbgrez.ReadOnly = True
    dbgrez.DefaultCellStyle.BackColor = Color.FloralWhite
    dbgrez.AlternatingRowsDefaultCellStyle.BackColor = Color.Lavender
    dbgrez.RowHeadersVisible = False
    dbgrez.AutoSize = False
    dbgrez.RowTemplate.Height = 16
    dbgrez.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.None

    dbgrez.Columns(1).DefaultCellStyle.Format = "dd/MM/yyyy"
    dbgrez.Columns(1).Width = 70
    dbgrez.Columns(2).Width = 150
    dbgrez.Columns(3).Width = 100
    dbgrez.Columns(0).Visible = False
    dbgrez.Columns(1).Visible = True
    dbgrez.Columns(2).Visible = True
    dbgrez.Columns(3).Visible = True
    dbgrez.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    'dbgrez.Splits(0).DisplayColumns(1).HeadingStyle.HorizontalAlignment = AlignHorzEnum.Near
    dbgrez.Columns(2).Visible = True
    dbgrez.Columns(3).Visible = True
    dbgrez.Columns(4).Visible = False
    dbgrez.Columns(5).Visible = False
    dbgrez.Columns(6).Visible = False
    dbgrez.Columns(7).Visible = False
    dbgrez.Columns(0).HeaderText = ""
    dbgrez.Columns(1).HeaderText = Texxt(2205)
    dbgrez.Columns(2).HeaderText = Texxt(2206)
    dbgrez.Columns(3).HeaderText = Texxt(2207)
    dbgrez.Columns(4).HeaderText = ""
    dbgrez.Columns(5).HeaderText = ""
    ' dbgrez.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
    'dbgrez.HoldFields()
    '
    '
    '
    '
    '
    '

  End Sub



  Sub ClearRwerte(Colorthek As Boolean, ByRef GrpRwerte As RefValuesGrp)
    Dim i As Integer
    For i = 0 To 1
      '
      'Untergurnd
      '
      '
      GrpRwerte(i).RefUnt.IVoNa = False
      GrpRwerte(i).RefUnt.Iplott = False
      GrpRwerte(i).RefUnt.Name = ""
      GrpRwerte(i).RefUnt.Bem = ""
      GrpRwerte(i).RefUnt.Nr = -1
      '
      'Vorlage
      '
      '
      If Not Colorthek Then
        GrpRwerte(i)("V").IVoNa = False
        GrpRwerte(i)("V").Iplott = False
        GrpRwerte(i)("V").Name = ""
        GrpRwerte(i)("V").Bem = ""
        GrpRwerte(i)("V").Nr = -1
        GrpRwerte(i)("V").ID = -1
      End If
      '
      'Nachstellung
      '
      '
      GrpRwerte(i)("N").IVoNa = False
      GrpRwerte(i)("N").Iplott = False
      GrpRwerte(i)("N").Name = ""
      GrpRwerte(i)("N").Bem = ""
      GrpRwerte(i)("N").Nr = -1
      GrpRwerte(i)("N").ID = -1
      '
      '
      'Rechnung
      '
      '
      GrpRwerte(i)("R").IVoNa = False
      GrpRwerte(i)("R").Iplott = False
      GrpRwerte(i)("R").Name = ""
      GrpRwerte(i)("R").ID = -1
      GrpRwerte(i)("R").Nr = -1
    Next i
  End Sub
  Function ZuwAkt(MaxZuw As Single) As Single
    ZuwAkt = (hscZUW.Value - hscZUW.Minimum) / (hscZUW.Maximum - hscZUW.Minimum) * MaxZuw * RezSozpt.MngMin * 0.01
  End Function
  Function HscVal(Zuakt As Single, MaxZuw As Single) As Integer
    HscVal = hscZUW.Minimum + Zuakt * (hscZUW.Maximum - hscZUW.Minimum) / (MaxZuw * RezSozpt.MngMin * 0.01)
  End Function
  '
  Sub ZuwaagGGEKorr(igx As Integer, MaxZuw As Single, MaxGGE As Single, DTO As Single)
    If igx = 3 Or igx = 5 Then
      '
      '
      'Bei Zuwaageminimierung müssen als Grenzen Gesamtmengen angegeben werden
      '
      '
      '
      If RezSozpt.INO = 1 Then
        HlfMeng = RezGrid.Umr.MngINO("MEN", RezSozpt, iee)
        If HlfMeng > RezSozpt.MngMin Then
          RezSozpt.MngMin = HlfMeng
          If RezSozpt.MngMax < RezSozpt.MngMin Then
            RezSozpt.MngMax = HlfMeng
          End If
        End If
        HlfMeng = (1.0# + 0.01 * MaxZuw) * RezSozpt.MngMin
        If HlfMeng > RezSozpt.MngMax Then
          RezSozpt.MngMax = HlfMeng
        End If
      Else
        RezSozpt.INO = 1
        RezSozpt.MngMin = RezGrid.Umr.MngINO("MEN", RezSozpt, iee)
        RezSozpt.MngMax = (1.0# + +0.01 * MaxZuw) * RezSozpt.MngMin
      End If
      cboMng.SelectedIndex = RezSozpt.INO
      txtMNG_0.Text = RezSozpt.MngMin
      txtMNG_1.Text = RezSozpt.MngMax
    Else
      '
      '
      'Werte für GGE festlegen
      '
      '
      hscGGE.Minimum = 0
      hscGGE.Maximum = 1000
      If MenueParam.Misch.MGGE = 1 Then
        MenueParam.Misch.Gge = Singl(txtGGEMax.Text)
        hscGGE.Value = hscGGE.Maximum
      Else
        MenueParam.Misch.Gge = 0.0#
        hscGGE.Value = hscGGE.Minimum
      End If
    End If
    If igx = 4 Or igx = 6 Then
      MenueParam.Misch.Dto = DTO
    End If
    Call RwertCheck()
  End Sub

  Function NewValHscZuw(InoMeng As Single) As Integer
    Dim HscVal As Single
    HscVal = (hscZUW.Maximum - hscZUW.Minimum) * (InoMeng - RezSozpt.MngMin) / (RezSozpt.MngMax - RezSozpt.MngMin)
    If HscVal > hscZUW.Maximum Then
      NewValHscZuw = hscZUW.Maximum
    ElseIf HscVal < hscZUW.Minimum Then
      NewValHscZuw = hscZUW.Minimum
    Else
      NewValHscZuw = HscVal
    End If
  End Function



  Sub VisGGEZUW(Igx As Integer, KeyMenge As String)
    Dim i As Integer
    '
    '
    '
    'Igx=0   Minimierung Farbabstand
    'Igx=1   zusätzl. (zu 0) Bindemittelminimierung
    'Igx=2   zusätzl. (zu 0) Farbmittelminimierung
    'Igx=3   zusätzl. (zu 0) Zuwaageminimierung
    'Igx=4   zusätzl. (zu 0) Minimierung der Änderungen zu altem Rezept
    'Igx=5   wie 3 + Erreichung eines vorgegebenen Farbabstandes (DTO)
    'Igx=6   wie 4 + Erreichung eines vorgegebenen Farbabstandes (DTO)
    'Igx=7   wie 1 + gewichteter Angleich von R-Werten
    '
    '
    '
    panGGE.Visible = False
    txtGGEMax.Visible = False
    lblGGEMax.Visible = False
    hscGGE.Visible = False
    panZUW.Visible = False
    txtZUW.Visible = False
    lblZUW.Visible = False
    txtZUWMax.Visible = False
    lblZUWMax.Visible = False
    hscZUW.Visible = False
    'lblProz.Visible = False
    Select Case Igx
      Case 1, 2
        '
        '
        'Minimierung Bindemittel,Farbmittel
        '
        '
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

            If BitWrt(4, MenueParam.User.Sonst) Then
              panGGE.Visible = True
              txtGGEMax.Visible = True
              lblGGEMax.Visible = True
              txtGGE.Visible = True
              lblGGE.Visible = True
              hscGGE.Visible = True
            End If
            GoTo EndGGE
          End If
        Next i
        MenueParam.Misch.Igx = 0
        panGGE.Visible = False
        txtGGEMax.Visible = False
        lblGGEMax.Visible = False
        txtGGE.Visible = False
        lblGGE.Visible = False
        hscGGE.Visible = False

      Case 3, 5
        '
        '
        'Zuwaageminimierung
        '
        '
        '
        'Menueparam.Misch.Gxg = 0.0005
        panZUW.Visible = True
        txtZUW.Visible = True
        lblZUW.Visible = True
        txtZUWMax.Visible = True
        lblZUWMax.Visible = True
        hscZUW.Visible = True
        'lblProz.Visible = True
      Case 4, 6
        '
        '
        'kleine Mengenänderung
        '
        '
        '
        panGGE.Visible = True
        txtGGEMax.Visible = True
        lblGGEMax.Visible = True
        txtGGE.Visible = True
        lblGGE.Visible = True
        hscGGE.Visible = True
      Case 7
        '
        '
        'mit Gewichten für R-Werte
        '
        '
        panGGE.Visible = True
        txtGGEMax.Visible = True
        lblGGEMax.Visible = True
        txtGGE.Visible = True
        lblGGE.Visible = True
        hscGGE.Visible = True

    End Select
    If Igx = 5 Or Igx = 6 Then
      lblDTO.Visible = True
      txtDTO.Visible = True
    Else
      lblDTO.Visible = False
      txtDTO.Visible = False
    End If

EndGGE:
    If Not BitWrt(3, MenueParam.User.Sonst) Then
      panZUW.Visible = False
    End If
    If Not BitWrt(4, MenueParam.User.Sonst) Then
      panGGE.Visible = False
    End If


  End Sub
  Sub RwertCheck()
    If chkRwertAngleich.Checked Then
      RezGraphics.KFIT = 2
      chkRefGew.Checked = False
      panGGE.Enabled = False
      chkRefGew.Enabled = False
    Else
      panGGE.Enabled = True
      chkRefGew.Enabled = True
      If MenueParam.Misch.Igx = 7 Then
        chkRefGew.Visible = True
        RezGraphics.KFIT = 3
      Else
        RezGraphics.KFIT = 1
        chkRefGew.Visible = False
        chkRefGew.Checked = False
      End If
    End If
  End Sub
  Sub VisVisKorrektur()
    Dim i As Integer
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
      chkMessVor(0).Checked = True
      chkMessNch(0).Checked = True
      chkMessUnt(0).Checked = True
      '
      lblDickNch(0).Visible = False
      txtDickNch(0).Visible = False
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
      ChkUNT(1).CheckState = 0
      chkKDE.Checked = False
      chkNAS.Checked = False
      ChkUNT(1).Visible = False
      chkKDE.Visible = False
      chkNAS.Visible = False
      '
      chkMessUnt(0).Checked = False
      chkMessUnt(1).Checked = False
      '
      lblDickNch(1).Visible = False
      txtDickNch(1).Visible = False
      lblDickKor(1).Visible = False
      txtDickKor(1).Visible = False
    Else
      RezCheckRad.chkUUU(0).Visible = True
      RezCheckRad.radUUU(0).Visible = True
      ChkUNT(0).Visible = True
      chkKDE.Checked = MenueParam.Misch.Kgx
      ChkUNT(1).Visible = True
      chkKDE.Visible = True
      chkNAS.Visible = True
      '
      '
      chkMessUnt(0).Checked = True
      chkMessUnt(1).Checked = True
      '
      txtDickNch(1).Visible = True
      lblDickNch(0).Visible = True
      txtDickNch(0).Visible = True
      lblDickNch(1).Visible = True
      txtDickNch(1).Visible = True
      txtDickKor(1).Visible = True
      lblDickKor(0).Visible = True
      txtDickKor(0).Visible = True
      lblDickKor(1).Visible = True
      txtDickKor(1).Visible = True
    End If
    '
    chkMessUnt(1).Checked = False
    chkMessVor(1).Checked = False
    chkMessNch(1).Checked = False
    '
    lblDickNch(1).Visible = False
    txtDickNch(1).Visible = False
    lblDickKor(1).Visible = False
    txtDickKor(1).Visible = False
    RezCheckRad.chkUUU(1).Visible = False
    RezCheckRad.chkUUU(1).Checked = False
    RezCheckRad.radUUU(1).Visible = False
    RezCheckRad.radUUU(1).checked = False
    txtKDE.Visible = False
    If ChkUNT(1).Checked Then
      '
      chkMessUnt(1).Checked = True
      chkMessVor(1).Checked = True
      chkMessNch(1).Checked = True
      '
      lblDickNch(1).Visible = True
      txtDickNch(1).Visible = True
      lblDickKor(1).Visible = True
      txtDickKor(1).Visible = True
      RezCheckRad.chkUUU(1).Visible = True
      RezCheckRad.radUUU(1).Visible = True
    Else
      RezCheckRad.chkUUU(0).Checked = True
      RezCheckRad.radUUU(0).checked = True
      RezCheckRad.radUUU(0).Visible = False
      RezCheckRad.radUUU(1).Visible = False
    End If
    If chkNAS.Checked Then
      RezCheckRad.chkUUU(1).Visible = True
      ChkUNT(1).Visible = True
      '
      chkMessUnt(1).Checked = True
      chkMessNch(1).Checked = True
      '
      lblDickNch(1).Visible = True
      txtDickNch(1).Visible = True
      lblDickKor(1).Visible = True
      txtDickKor(1).Visible = True
      chkKDE.Checked = True
    End If
    If chkKDE.Checked Then
      RezCheckRad.chkUUU(1).Visible = True
      chkMessUnt(1).Checked = True

      'lblUDI(1).Visible = True
      'txtDIK(1).Visible = True
      lblDickKor(1).Visible = True
      txtDickKor(1).Visible = True
      txtKDE.Visible = True
      txtKDE.Text = CStr(MenueParam.Misch.Fde)
    End If

    For i = 0 To 1
      btnMessVor(i).Visible = chkMessVor(i).Checked
      lblMessVor(i).Visible = chkMessVor(i).Checked
      lblVOR(i).Visible = chkMessVor(i).Checked
      btnMessVOO(i).Visible = chkMessVor(i).Checked And chkRWRT.Checked
      lblMessVOO(i).Visible = chkMessVor(i).Checked And chkRWRT.Checked
      lblMessUnt(i).Visible = chkMessUnt(i).Checked
      btnMessUnt(i).Visible = chkMessUnt(i).Checked
      lblMessNch(i).Visible = chkMessNch(i).Checked
      btnMessNch(i).Visible = chkMessNch(i).Checked
      If GrpRwerte(i)("V").ID >= 0 Then
        GrpRwerte(i)("V").IVoNa = chkMessVor(i).Checked
      End If
      If GrpRwerte(i)("N").ID >= 0 Then
        GrpRwerte(i)("N").IVoNa = chkMessNch(i).Checked
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
    If Not BitWrt(15, MenueParam.User.Visbl) And Not BitWrt(16, MenueParam.User.Visbl) Then
      lblmng(0).Visible = False
      lblmng(1).Visible = False
    End If
    If Not BitWrt(16, MenueParam.User.Enabl) Then
      cboPro_0.Enabled = False
      cboPro_1.Enabled = False
      For i = 0 To txtPro.Count - 1
        txtPro(i).Enabled = False
      Next i
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
    If Not BitWrt(20, MenueParam.User.Visbl) Then
      chkNAS.Visible = False
    End If
    If Not BitWrt(20, MenueParam.User.Enabl) Then
      chkNAS.Enabled = False
    End If
    If Not BitWrt(22, MenueParam.User.Visbl) Then
      For i = 0 To txtDickNch.Count - 1
        txtDickNch(i).Visible = False
        lblDickNch(i).Visible = False
      Next i
      For i = 0 To txtDickKor.Count - 1
        txtDickKor(i).Visible = False
        lblDickKor(i).Visible = False
      Next i
    End If
    If Not BitWrt(22, MenueParam.User.Enabl) Then
      For i = 0 To txtDickNch.Count - 1
        txtDickNch(i).Enabled = False
      Next i
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
    If Not BitWrt(1, MenueParam.User.Sonst) Then
      'hscMNG.Visible = False
    End If
    If Not BitWrt(2, MenueParam.User.Sonst) Then
      hscDIS.Visible = False
      lblKDS.Visible = False
      panDicke.Visible = False
      lblDIS.Visible = False
      'lblDIQ.Visible = False
    End If
    If Not BitWrt(7, MenueParam.User.Writ) Then
      'btnSPE.Visible = False
    End If
    If Not BitWrt(6, MenueParam.User.Sonst) Then
      'chkUMR.Visible = False
    End If
    '
    If Not BitWrt(7, MenueParam.User.Sonst) Then
      'chkRZAEN.Visible = False
    End If
    '
    '


  End Sub
  WriteOnly Property SplitVisible As Integer
    Set(value As Integer)
      Dim i As Integer
      For i = 0 To SplitContKorrRezepte.Count - 1
        SplitContKorrRezepte(i).Visible = False
      Next i
      SplitContKorrRezepte(value).Visible = True
    End Set
  End Property


  Sub GetNameID(ByRef ID As Integer, ByRef RefName As String, ByRef Banum As String)
    Dim j As Integer
    Dim ier As Integer
    ier = 0
    If RefName <> "" Then
      '
      If RcmdRef = 0 Then Exit Sub
      If GetPutReflex.HideSofort Then
        GetPutReflex.InVisible()
      End If
      '
      If RcmdRef >= 0 Or RcmdRef = -11 Or RcmdRef = -12 Then
        If Abs(RcmdRef) > 10 Then
          ReUn = "V"
          j = Abs(RcmdRef) - 11
        ElseIf RcmdRef > 0 Then
          ReUn = "N"
          j = RcmdRef - 1
        ElseIf RcmdRef < 0 Then
          j = -RcmdRef - 1
        End If
      ElseIf RcmdRef < 0 Then
        j = -RcmdRef - 1
      End If
      If ier = 0 Then
        Select Case RcmdRef
          Case 1, 2
            lblMessNch(j).Text = GrpRwerte(j)(ReUn).Name
            SmpID(RezGraphics.Vkwb(j)) = ID
          Case 11, 12
            lblMessVor(j).Text = GrpRwerte(j)(ReUn).Name
            lblVOR(j).Text = GrpRwerte(j)(ReUn).Name
            TypID(RezGraphics.Vkwb(j)) = ID
          Case -11, -12
            lblMessVOO(j).Text = GrpRwerte(j)(ReUn).Name
            lblVOR(j).Text = GrpRwerte(j)(ReUn).Name
            TypID(RezGraphics.Vkwb(j)) = ID
            GrpRwerte(j)(ReUn).Nr = j
          Case -1, -2
            lblMessUnt(-RcmdRef - 1).Text = GrpRwerte(j).RefUnt.Name
            UntID(RezGraphics.Vkwb(j)) = ID
        End Select
      End If
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
  Sub GraphicPlotRwert(Refkenn() As String, RezGrid As HandleRezC1Screen)
    Dim i As Integer
    Dim j As Integer
    Dim Boo As Boolean
    If IsNothing(RezGrid.Picauf) Then Exit Sub
    RezGraphics.FawrtRwerte(0).clear()
    RezGraphics.FawrtRwerte(1).clear()
    RezGraphics.PlotRwerte.clear()
    Boo = HandleRezept.IVorNa("Z", GrpRwerte, ChkUNT, chkKDE, chkNAS)

    j = -1
    For i = 0 To Refkenn.Count - 1
      Boo = HandleRezept.IVorNa(Refkenn(i), GrpRwerte, ChkUNT, chkKDE, chkNAS)
      RezGraphics.FawrtRwerte(0).Add(KeyRe(i), GrpRwerte(0)(Refkenn(i)))
      RezGraphics.FawrtRwerte(1).Add(KeyRe(i), GrpRwerte(1)(Refkenn(i)))
      If GrpRwerte(0)(Refkenn(i)).IVoNa AndAlso GrpRwerte(0)(Refkenn(i)).Iplott Then
        j = j + 1
        RezGraphics.PlotRwerte.Add(KeyRe(j), GrpRwerte(0)(Refkenn(i)))
      End If
      If GrpRwerte(1)(Refkenn(i)).IVoNa AndAlso GrpRwerte(1)(Refkenn(i)).Iplott Then
        j = j + 1
        RezGraphics.PlotRwerte.Add(KeyRe(j), GrpRwerte(1)(Refkenn(i)))
      End If
    Next i
    '
    '
    '
    If Refkenn.Count > 0 Then

      RezGrid.Picauf.PicRezRezept.Visible = Not chkFAR.Checked
      RezGrid.Picauf.PicRezFarb.Visible = chkFAR.Checked
      RezGraphics.RzName(0) = RezGrid.Name
      RezGraphics.Text = RezGrid.AllRezepte.Rezepte(RezGrid.Name).Name
    End If
    RezGraphics.Rmax = -1.0#
    RezGraphics.Rmin = -1.0#
    Application.DoEvents()
    RezGrid.Picauf.Refresh()

  End Sub
  Sub DruckKorrekt(Index As Integer, kdru As Integer)
    Dim DruAusgabe As AusgabeCreateObj
    Dim FarbWrt As ValuesGrpsAssigns
    Dim quali As QualKontrolle
    Dim FormShow As New frmDRR
    Dim StrName As String
    Dim RzUm As String
    Dim iee As Long
    Dim RzNr As String
    If Not cboDRU(Index).Enabled Then Exit Sub
    RzNr = RezDruck.KeyNam
    Dim Printdoc As New PrintDocument
    Printdoc.PrinterSettings = MnPrintSet
    PrintPreview.Document = Printdoc
    '
    'Kommentar
    RezSozpt.Comment = "   "
    If chkGrundKorr.Checked Then
      RezSozpt.Comment = RezSozpt.Comment & chkGrundKorr.Text
    End If
    StrName = InputBox(Texxt(2104), Texxt(2000), RezSozpt.Rezepte(RzNr).Name)
    If StrName <> "" Then
      RezSozpt.Rezepte(RzNr).Name = StrName
      If RzNr = "KOR" Then
        RezSozpt.Rezepte("MZU").Name = StrName
        RezSozpt.Rezepte("ZUW").Name = StrName
      End If
    Else
      Exit Sub
    End If


    If kdru = 0 Then
      ' claDRU.Picauf = Picauf
      'claDRU.RezDru(RezSozpt.RezMeng(RzNr).Name)

      FormShow.ShowDialog()
      RezDruck.Text0 = FormShow.txtEIN_0.Text
      RezDruck.Text1 = FormShow.txtEIN_1.Text
      RezDruck.Text2 = FormShow.txtEIN_2.Text
      Printdoc.DefaultPageSettings.Landscape = True
      AddHandler Printdoc.PrintPage, AddressOf RezDruck.DruckPictureboxes
      PrintPreview.WindowState = FormWindowState.Maximized
      'Preview anzeigen 
      PrintPreview.ShowDialog()
      RemoveHandler Printdoc.PrintPage, AddressOf RezDruck.DruckPicturebox
      Printdoc.Dispose()


    ElseIf kdru = 1 Then
      '
      '
      '
      '
      'Wiegeschein für Korrektur
      '
      '
      '

      i = -1
      Erase RezDruck.RefNr
      If GrpRwerte(0)("V").IVoNa Then
        i = i + 1
        ReDim Preserve RezDruck.RefNr(i)
        RezDruck.RefNr(i) = "V"
      End If
      If GrpRwerte(0)("N").IVoNa Then
        i = i + 1
        ReDim Preserve RezDruck.RefNr(i)
        RezDruck.RefNr(i) = "N"
      End If
      If GrpRwerte(0)("R").IVoNa Then
        i = i + 1
        ReDim Preserve RezDruck.RefNr(i)
        RezDruck.RefNr(i) = "R"
      End If
      i = -1
      Erase RezDruck.RzName
      If RezSozpt.Rezepte("ALT").KF > 0 Then
        i = i + 1
        ReDim Preserve RezDruck.RzName(i)
        RezDruck.RzName(i) = "ALT"
      End If
      If (MenueParam.Misch.Igx = 3 Or MenueParam.Misch.Igx = 5) And RezSozpt.Rezepte("MZU").KF > 0 Then
        i = i + 1
        ReDim Preserve RezDruck.RzName(i)
        RezDruck.RzName(i) = "MZU"
      ElseIf RezSozpt.Rezepte("KOR").KF > 0 Then
        i = i + 1
        ReDim Preserve RezDruck.RzName(i)
        RezDruck.RzName(i) = "KOR"
      End If
      If RezSozpt.Rezepte("ZUW").KF > 0 Then
        i = i + 1
        ReDim Preserve RezDruck.RzName(i)
        RezDruck.RzName(i) = "ZUW"
      End If

      '
      '
      AddHandler Printdoc.PrintPage, AddressOf RezDruck.DruckKorrekturRezept

      PrintPreview.WindowState = FormWindowState.Maximized
      'Preview anzeigen    
      Printdoc.DefaultPageSettings.Landscape = False

      PrintPreview.ShowDialog()
      RemoveHandler Printdoc.PrintPage, AddressOf RezDruck.DruckKorrekturRezept
      Printdoc.Dispose()

    ElseIf kdru = 2 Then
      '
      '
      'Ausgabe Rezeptkorrektur (User-Programm)
      '
      '
      '
      Cursor = Cursors.WaitCursor
      DruAusgabe = New AusgabeCreateObj
      FarbWrt = New ValuesGrpsAssigns
      quali = New QualKontrolle
      GrpRwerte("W")("V").Itp = True
      GrpRwerte("S")("V").Itp = True
      '
      '
      '
      '
      '
      '
      '
      '

      FarbWrt.clear()

      'AufbauPar.AufbauRezeptMerk(FarbWrt, ier)

      'Call quali.FarbWrtCalc(MenueParam.User.Winkel, GrpRwerte, FarbWrt, iee)
      Call SetStdQucontrol(GrpRwerte)
      AufbauPar.AufbauAnwsgMerk(MenueParam.UserID, MenueParam.MethID, FarbWrt, ier)

      Call quali.CalcFarbWerte(MenueParam.User.Winkel, GrpRwerte, FarbWrt, iee)
      ier = iee
      '
      RzUm = ""
      If Index = 2 Then
        RzUm = "MEN"
      ElseIf Index = 3 Then
        RzUm = "KOR"
      End If
      DruAusgabe.AusgabeKORREKT(RezSozpt, FarbWrt, GrpRwerte)
      DruAusgabe = Nothing
      quali = Nothing
      FarbWrt = Nothing
      Cursor = Cursors.Default
    End If


  End Sub



  '
  '
  '
  '*******************************************
  '*******************************************
  '*******************************************
  '*******************************************
  '*******************************************
  '*******************************************
  '*******************************************
  '*******************************************
  '*******************************************
  '*******************************************
  '
  '
  '
  Private Sub frmColorSuchKorr_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim j As Integer
    parform = Me

    '
    'Texte und Änderungen für Form
    '
    '
    'Call AendMDChild()
    txtGGEMax.Text = -1.0#
    txtZUWMax.Text = -1.0#
    If Colorthek Then
      Me.Text = Texxt(1855) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
      btnColorthek.Visible = True
      btnRezept.Visible = False
      btnEingabe.Visible = True
      btnColorthek.Visible = True
      btnDarstellung.Visible = True
      btnKorrektur.Enabled = False
      btnKorrSpezial.Enabled = False
      btnDarstellung.Enabled = False
      btnEingabe.Enabled = False
      chkDatum_1.Visible = False
      btnSUC_1.Visible = False
      txtVON_1.Visible = False
      txtBIS_1.Visible = False
      lblBIS_1.Visible = False
      lblSUC_1.Visible = False
      txtSUC_1.Visible = False
      chkWithTempl.Visible = False
      chkWithTempl.Checked = False
      chkWithTempl.Enabled = False
    Else
      Me.Text = Texxt(1854) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
      btnColorthek.Visible = False
      btnRezept.Visible = True
      btnEingabe.Visible = False
      btnDarstellung.Visible = True
      btnEingabe.Visible = False
      btnRezept.Visible = True
      btnKorrektur.Enabled = False
      btnKorrSpezial.Enabled = False
      btnDarstellung.Enabled = False
    End If
    btnColorthek.Text = Texxt(733)
    btnDarstellung.Text = Texxt(736)
    btnEingabe.Text = Texxt(735)
    btnRezept.Text = Texxt(730)
    btnKorrektur.Text = Texxt(731)
    btnKorrSpezial.Text = Texxt(737)
    btnSUC_0.Text = Texxt(835)
    btnSUC_1.Text = Texxt(835)
    btnStoreAlt.Text = Texxt(1995)
    btnStoreNeu.Text = Texxt(854)
    btnMessVOO_0.Text = Texxt(806)
    btnMessVOO_1.Text = Texxt(807)
    chkPicVorlage.Text = Texxt(861)
    chkPicNachstellung.Text = Texxt(786)
    chkPicKorrektur.Text = Texxt(731)
    btnMessVor_0.Text = Texxt(806)
    btnMessVor_1.Text = Texxt(807)
    btnMessNch_0.Text = Texxt(828)
    btnMessNch_1.Text = Texxt(829)
    btnMessUnt_0.Text = Texxt(808)
    btnMessUnt_1.Text = Texxt(809)
    lblDickKor_0.Text = Texxt(797)
    lblDickKor_1.Text = Texxt(798)
    lblDickNch_0.Text = Texxt(795)
    lblDickNch_1.Text = Texxt(796)
    lblKDS.Text = Texxt(831)
    lblDIS.Text = Texxt(832)
    lblDisMax.Text = Texxt(832) & "(" & Texxt(814) & ")"
    lblFAR.Text = Texxt(738)
    lblFAQ.Text = Texxt(802)
    lblBIS_0.Text = Texxt(377)
    lblBIS_1.Text = Texxt(377)
    lblMNG_0.Text = Texxt(813)
    lblMNG_1.Text = Texxt(814)
    lblDTO.Text = Texxt(4682)
    '
    '
    chkHLFVorlage.Text = Texxt(728)
    chkKDE.Text = Texxt(831)
    chkNAS.Text = Texxt(834)
    chkUUU_0.Text = Texxt(804)
    chkUUU_1.Text = Texxt(805)
    chkDatum_0.Text = Texxt(140)
    chkDatum_1.Text = Texxt(140)
    chkUMR.Text = Texxt(445)
    lblSUC_0.Text = Texxt(4606)
    lblSUC_1.Text = Texxt(4606)
    btnGRPN.Text = Texxt(872)
    chkRwertAngleich.Text = Texxt(4708)
    chkWithTempl.Text = Texxt(4702)
    chkGrundKorr.Text = Texxt(3954)
    chkRefGew.Text = Texxt(3952)
    chkRZAen.Text = Texxt(873)

    '
    dbgREZ_1.Caption = Texxt(837)
    dbgREZ_2.Caption = Texxt(837)
    '
    '
    lblHLFAvail.Text = Texxt(726)
    lblHLFSelec.Text = Texxt(727)

    lblRezName.Text = Texxt(931)

    'lblARB
    '
    Me.lblARB.Text = Texxt(2002)
    '
    'chkFAR

    Me.chkFAR.Text = Texxt(857)
    '
    '

    'btnSPRwrt
    '
    Me.btnSPRwrt.Text = Texxt(849)

    'lblMSH
    '
    Me.lblMSH.Text = Texxt(422)
    '
    'lblGRP
    '
    Me.lblGRP.Text = Texxt(3669)
    Me.lblGRPN.Text = Texxt(3661)
    '
    '
    'lblGLZ
    '
    Me.lblGLZ.Text = Texxt(358)
    '
    '
    'Glanzgrad
    '
    '
    Me.lblGlzGrd.Text = Texxt(870)
    '
    'chkARCH
    '

    Me.chkARCH.Text = Texxt(374)
    Me.chkARCN.Text = Texxt(3660)
    '
    'chkVOL
    '

    Me.chkVOL.Text = Texxt(830)
    '

    '
    Me.chkAltName.Text = Texxt(902)
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
    'lblZUW
    '
    Me.lblZUW.Text = Texxt(790)
    Me.lblZUWMax.Text = Texxt(790) & "(" & Texxt(814) & ")"
    '
    'lblGGE
    '
    '
    Me.lblGGE.Text = Texxt(172)
    Me.lblGGEMax.Text = Texxt(172) & "(" & Texxt(814) & ")"
    '

    '
    'chkUNT_1
    '

    Me.chkUNT_1.Text = Texxt(939)
    '
    'chkUNT_0
    '

    Me.chkUNT_0.Text = Texxt(938)
    chkRWRT.Text = Texxt(879)
    chkWithOptData.Text = Texxt(3648)
    radRwerte_0.Text = Texxt(1989)
    radRwerte_1.Text = Texxt(4702)
    radRwerte_2.Text = Texxt(4707)
    'Gewichtfaktoren
    '
    lblGewFakt.Text = Texxt(446)
    lblGewDL.Text = "DL"
    lblGewDC.Text = "DC"
    lblGewDH.Text = "DH"
    '
    '
    'Aufbau für für Art der Rezeptsuche für Farbmittel in Rezepten
    '
    '
    cboFAR.Items.Clear()
    For i = 0 To 6
      cboFAR.Items.Add(Texxt(750 + i))
    Next i
    cboFAR.SelectedIndex = 0
    '
    '
    ' Create the ToolTip and associate with the Form container.
    ToolTipSuchKorr = New ToolTip


    ToolTipSuchKorr.ShowAlways = True

    ' Set up the ToolTip text for the Button and Checkbox.



    chkVOL.CheckState = CheckState.Indeterminate
    '
    OldTabIndex = -1
    OldMischID = -1
    OleAdRezpt.SelectCommand = New OleDbCommand("", Cncol)
    AddHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID

    '
    '
    '
    '
    '
    kwb = 0
    '
    '
    '
    Umr = New RezeptUmrechnung
    '
    '
    '
    RezSozpt = New RecipesGrp
    GrpRwerte = New RefValuesGrp
    GrpRwrtRest = New RefValuesGrp
    FarbWrt = New ValuesGrps
    RwWrRezept = New ReadWriteRezept
    ReWrRwert = New ReadWriteRwert
    CalcRezept = New RezeptBerechnung
    OptGesamt = New OpticalData
    claDRU = New HandlePlottDruck
    quali = New QualKontrolle
    DatenFarb = New HandleRezDsetFarb
    RezDruck = New HandlePlottDruck
    RezCheckRad = New HandleCheckRad
    RezGraphics = New HandleRezGrafik
    txtSum = New List(Of TextBox)
    claDRU.GrpRwerte = GrpRwerte
    RezGraphics.AllRezepte = RezSozpt
    RezDruck.Plott = RezGraphics
    RezDruck.AllRezepte = RezSozpt
    RezDruck.GrpRwerte = GrpRwerte
    RezDruck.Winkel = MenueParam.User.Winkel
    '
    '
    'DummyID
    '
    '
    PicAufbau = New List(Of HandlePictures)
    PicAufbau.Add(Nothing)
    PicAufbau.Add(Nothing)
    PicAufbau.Add(New HandlePictures)
    PicAufbau.Add(New HandlePictures)
    PicAufbau(2).Add("REF", CType(picRwert_2, PictureBox))
    PicAufbau(2).Add("LAB", CType(picFarbLAB_2, PictureBox))
    PicAufbau(2).Add("XYZ", CType(picFarbXYZ_2, PictureBox))
    PicAufbau(2).Add("FRB", CType(picRezFarb_2, PictureBox))
    PicAufbau(2).Add("REZ", CType(picRezRezept_2, PictureBox))
    PicAufbau(2).PicGraphic = RezGraphics
    '
    PicAufbau(3).Add("REF", CType(picRwert_3, PictureBox))
    PicAufbau(3).Add("LAB", CType(picFarbLAB_3, PictureBox))
    PicAufbau(3).Add("XYZ", CType(picFarbXYZ_3, PictureBox))
    PicAufbau(3).Add("FRB", CType(picRezFarb_3, PictureBox))
    PicAufbau(3).Add("REZ", CType(picRezRezept_3, PictureBox))
    PicAufbau(3).PicGraphic = RezGraphics


    RezptGrid = New List(Of HandleRezC1Screen)
    RezptGrid.Add(Nothing)
    RezptGrid.Add(New HandleRezC1Screen)
    RezptGrid.Add(New HandleRezC1Screen)
    RezptGrid.Add(New HandleRezC1Screen)
    txtSum.Add(Nothing)
    txtSum.Add(txtSum_1)
    txtSum.Add(txtSUM_2)
    txtSum.Add(txtSUM_3)

    '
    '
    '
    RezptGrid(1).TDBFar = TDBFar_1
    RezptGrid(1).TDBDropFar = TDBDropFar_1
    RezptGrid(1).TDBDropPre = TDBDropPre_1
    RezptGrid(1).TDBDropPro = TDBDropPro_1
    RezptGrid(1).TDBDropPrb = TDBDropPrb_1
    '
    '
    '
    RezptGrid(2).TDBFar = TDBFar_2
    RezptGrid(2).TDBDropFar = TDBDropFar_2
    RezptGrid(2).TDBDropPre = TDBDropPre_2
    RezptGrid(2).TDBDropPro = TDBDropPro_2
    RezptGrid(2).TDBDropPrb = TDBDropPrb_2
    '
    '
    '
    RezptGrid(3).TDBFar = TDBFar_3
    RezptGrid(3).TDBDropFar = TDBDropFar_3
    RezptGrid(3).TDBDropPre = TDBDropPre_3
    RezptGrid(3).TDBDropPro = TDBDropPro_3
    RezptGrid(3).TDBDropPrb = TDBDropPrb_3
    DatenFarb.DsetFarbCreate()

    For i = 1 To 3
      RezptGrid(i).AllRezepte = RezSozpt
      RezptGrid(i).DatenFarb = DatenFarb
      RezptGrid(i).CalcRezept = CalcRezept
      RezptGrid(i).GrpRwerte = GrpRwerte
      RezptGrid(i).Picauf = PicAufbau(i)
      RezptGrid(i).chkVOL = chkVOL
      RezptGrid(i).cboMNG = Nothing
      RezptGrid(i).cboPRO_0 = Nothing
      RezptGrid(i).cboPRO_1 = Nothing
      RezptGrid(i).txtFooter = txtSum(i)

      RezptGrid(i).AllRezepte = RezSozpt
      RezptGrid(i).TDBFar.AllowAddNew = True
      RezptGrid(i).TDBFar.AllowUpdate = True
      RezptGrid(i).TDBFar.AllowDelete = True
    Next i
    '
    '
    '

    RezptGrid(1).cboMNG = cboMng
    RezptGrid(1).cboPRO_0 = cboPro_0
    RezptGrid(1).cboPRO_1 = cboPro_1
    For i = 1 To 3
      RezptGrid(i).txtMNG_0 = txtMNG_0
      RezptGrid(i).txtMNG_1 = txtMNG_1
      RezptGrid(i).txtPRO_0 = txtPRO_0
      RezptGrid(i).txtPRO_1 = txtPRO_1
    Next i
    '
    '
    chkVOL.CheckState = False

    '
    For i = 1 To 3
      RezptGrid(i).GrpRwerte = GrpRwerte
      RezptGrid(i).PicGraphic = RezGraphics
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

    Iprn = 0
    '
    '
    '

    '
    'Gruppen für Rezepte
    '
    '
    '
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
      lblGRP.Visible = True
    Else
      cboGRP.Visible = False
      lblGRP.Visible = False
    End If
    '
    '
    '
    '
    cboGRP.Enabled = True
    cboGRPN.Enabled = True
    HandleRezept = New HandleRezGeneral
    HandleRezeptN = New HandleRezGeneral
    RezeptGrid = New RecipesGrp
    RezeptGrid.Rezepte.kwb(0) = 0
    RezeptGrid.Rezepte.kwb(1) = 1

    HandleRwrt = New HandleRwerte
    RefWerteUNT = New List(Of RefValue)
    RefWertetyp = New List(Of RefValue)
    RefWertesmp = New List(Of RefValue)
    HandleRezept.lblGRP = lblGRP
    HandleRezept.cboGRP = cboGRP
    HandleRezeptN.lblGRP = lblGRPN
    HandleRezeptN.cboGRP = cboGRPN
    'MischGroup(i).datMSH = datMSH
    'MischGroup(i).dbcMSH = dbcMSH(i)
    'MischGroup(i).cboGRP = cboGRP(i)

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
    If BitWrt(22, MenueParam.User.Sonst) Then
      lblGlzGrd.Visible = True
      txtGlzGrd.Visible = True
    End If
    '
    '
    '
    '
    '
    '
    If Not BitWrt(7, MenueParam.User.Sonst) Then
      'RezptGrid(3).TDBFar.Top = RezptGrid(3).TDBFar.Top + chkRZAEN.Height
      'RezptGrid(3).TDBFar.Height = RezptGrid(3).TDBFar.Height + chkRZAEN.Height
    End If
    '
    '
    '
    '
    'Auswahl Drucken
    '

    If Not BitWrt(20, MenueParam.User.Drum) Then
      Kdru = MenueParam.User.Druq / 64
    Else
      Kdru = 2
    End If


    cboDRU.Add(Nothing)
    cboDRU.Add(Nothing)
    cboDRU.Add(cboDRU_2)
    cboDRU.Add(cboDru_3)
    btnDRU.Add(Nothing)
    btnDRU.Add(Nothing)
    btnDRU.Add(btnDRU_2)
    btnDRU.Add(btnDru_3)
    For i = 2 To 3
      cboDRU(i).Items.Clear()
      For j = 0 To 2
        cboDRU(i).Items.Add(Texxt(940 + j))
        'cboDRU(i).ItemData(cboDRU(i).NewIndex) = j
      Next j
      cboDRU(i).Enabled = False
      cboDRU(i).SelectedIndex = Kdru
      btnDRU(i).Text = cboDRU(i).Text
      cboDRU(i).Enabled = True
      If BitWrt(23, MenueParam.User.Drum) Then
        cboDRU(i).Visible = True
        btnDRU(i).Visible = False
      Else
        cboDRU(i).Visible = False
        btnDRU(i).Visible = True
      End If
    Next i
    btnDRU_2.Text = cboDRU_2.Items(Kdru)
    btnDru_3.Text = cboDru_3.Items(Kdru)


    '
    If PrinterSettings.InstalledPrinters.Count = 0 Then
      For i = 2 To 3
        cboDRU(i).Visible = False
        btnDRU(i).Visible = False
      Next i
    End If

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
    cboGRPRwrt.SelectedValue = MenueParam.Messg.UserRefGID '
    '
    '
    If Not BitWrt(10, MenueParam.User.Sonst) Then
      btnSPRwrt.Visible = False
      cboGRPRwrt.Visible = False
    End If
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
    '
    '
    '

    '
    '
    '

    ChkUNT.Add(chkUNT_0)
    ChkUNT.Add(chkUNT_1)
    chkMessVor.Add(chkMessVor_0)
    chkMessVor.Add(chkMessVor_1)
    chkMessNch.Add(chkMessNch_0)
    chkMessNch.Add(chkMessNch_1)
    chkMessUnt.Add(chkMessUnt_0)
    chkMessUnt.Add(chkMessUnt_1)
    btnMessVor.Add(btnMessVor_0)
    btnMessVor.Add(btnMessVor_1)
    btnMessNch.Add(btnMessNch_0)
    btnMessNch.Add(btnMessNch_1)
    btnMessUnt.Add(btnMessUnt_0)
    btnMessUnt.Add(btnMessUnt_1)
    lblMessVor.Add(lblMessVor_0)
    lblMessVor.Add(lblMessVor_1)
    lblMessNch.Add(lblMessNch_0)
    lblMessNch.Add(lblMessNch_1)
    lblMessUnt.Add(lblMessUnt_0)
    lblMessUnt.Add(lblMessUnt_1)
    lblDickKor.Add(lblDickKor_0)
    lblDickKor.Add(lblDickKor_1)
    txtDickKor.Add(txtDickKor_0)
    txtDickKor.Add(txtDickKor_1)
    lblDickNch.Add(lblDickNch_0)
    lblDickNch.Add(lblDickNch_1)
    txtDickNch.Add(txtDickNch_0)
    txtDickNch.Add(txtDickNch_1)

    lblMessVOO.Add(lblMessVOO_0)
    lblMessVOO.Add(lblMessVOO_1)
    btnMessVOO.Add(btnMessVOO_0)
    btnMessVOO.Add(btnMessVOO_1)
    txtMng.Add(txtMNG_0)
    txtMng.Add(txtMNG_1)
    txtPro.Add(txtPRO_0)
    txtPro.Add(txtPRO_1)
    lblVOR.Add(lblVOR_0)
    lblVOR.Add(lblVOR_1)
    lblmng.Add(lblMNG_0)
    lblmng.Add(lblMNG_1)
    '
    SplitContKorrRezepte.Add(SplitContColorthek)
    SplitContKorrRezepte.Add(SplitContEingabe)
    SplitContKorrRezepte.Add(SplitContDarstellung)
    SplitContKorrRezepte.Add(SplitContKorrektur)

    '
    lblBIS.Add(lblBIS_0)
    lblBIS.Add(lblBIS_1)
    '
    txtBIS.Add(txtBIS_0)
    txtBIS.Add(txtBIS_1)
    '
    txtVON.Add(txtVON_0)
    txtVON.Add(txtVON_1)
    '
    chkDatum.Add(chkDatum_0)
    chkDatum.Add(chkDatum_1)
    '
    txtSUC.Add(txtSuc_0)
    txtSUC.Add(txtSUC_1)
    '
    '
    radRwerte.Add(radRwerte_0)
    radRwerte.Add(radRwerte_1)
    radRwerte.Add(radRwerte_2)
    '
    '
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
    RezCheckRad.PicGraphic = RezGraphics
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

    '

    '
    '
    TblRezept = New DataTable
    TblRezept.Columns.Add("REZEPT_ID", GetType(Integer))
    TblRezept.Columns.Add("REZEPT_DATTIM", GetType(Date))
    TblRezept.Columns.Add("REZEPT_NAME", GetType(String))
    TblRezept.Columns.Add("REZEPT_BEM", GetType(String))
    If Colorthek Then
      TblRezept.Columns.Add("DEMOD", GetType(Single))
      TblRezept.Columns.Add("OPTDA", GetType(Boolean))
      TblRezept.Columns("DEMOD").DefaultValue = -1.0
      TblRezept.Columns("OPTDA").DefaultValue = False
    Else
      '
      'mit Readonly Werten, falls Korrekturprogramm
      '
      '
      TblRezept.Columns.Add("DEMOD", GetType(Single), -1.0)
      TblRezept.Columns.Add("OPTDA", GetType(Boolean), True)
    End If
    TblRezept.Columns.Add("RWERT_ID", GetType(Integer))
    TblRezept.Columns.Add("USER_ID", GetType(String))
    '
    '
    ViewRezept = New DataView(TblRezept)
    ConnRezept = New BindingSource
    ConnRezept.DataSource = ViewRezept



    dbgREZ = New List(Of C1TrueDBGrid)
    dbgREZ.Add(Nothing)
    dbgREZ.Add(dbgREZ_1)
    dbgREZ.Add(dbgREZ_2)
    dbgREZ(1).SetDataBinding(ConnRezept, "", False)
    dbgREZ(2).SetDataBinding(ConnRezept, "", False)
    Call DbgRezCustomize(dbgREZ(1))
    Call DbgRezCustomize(dbgREZ(2))


    ViewHlfAvail = New DataView(TblRezept)
    ViewHlfSelec = New DataView(TblRezept)
    dbgHLFAvail.DataSource = ViewHlfAvail
    dbgHLFSelec.DataSource = ViewHlfSelec
    Call DbgRezCustomize(dbgHLFAvail)
    Call DbgRezCustomize(dbgHLFSelec)
    RezHLFSelecID = New List(Of Integer)
    RezHlfAvailID = New List(Of Integer)
    '
    'Tabelle Farbmittel
    '
    '
    OleFarb = New OleDbDataAdapter
    CmdFarb = New OleDbCommand("", Cndat)
    OleFarb.SelectCommand = CmdFarb
    '
    FarbTab = New DataTable
    CmdFarb.CommandText = "SELECT * FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " ORDER BY FARBM_NAME"
    If Not FillDatset(OleFarb, FarbTab) Then
      Exit Sub
    End If

    '
    '
    'Mit farbmitteln
    '
    '
    lstFAQ.DataSource = FarbTab
    lstFAQ.ValueMember = "FARBM_ID"
    lstFAQ.DisplayMember = "FARBM_NAME"
    '
    lstFAR.ValueMember = "ID"
    lstFAR.DisplayMember = "TEXT"
    '
    'Mischsysteme für Korrektur und Colortheksuche
    '
    '
    '
    ''
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
    'Gewichte übernehmen
    '
    For i = 0 To TblRefGew.Rows.Count - 1
      MenueParam.Messg.RefGew.R(i) = TblRefGew.Rows(i)("GEW") * MenueParam.Misch.Gge
    Next
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


  End Sub

  Private Sub frmColorSuchKorr_Shown(sender As Object, e As System.EventArgs) Handles Me.Shown
    If Colorthek Then
      '
      'Colortheksuche
      '
      Call RezActiv(2)
      RezGrid = RezptGrid(2)
      SplitVisible = 0
      '
    Else
      '
      '
      'Korrekturberechnung
      '
      '
      '
      Call RezActiv(1)
      RezGrid = RezptGrid(1)
      '
      SplitVisible = 1
      '


    End If
    '
    '
    '
    Application.DoEvents()
    If MenueParam.MischID = -1 Then
      MsgBox(Texxt(3601))
      If Colorthek Then
        SplitVisible = 0
      Else
        SplitVisible = 1
      End If
      Exit Sub
    End If
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
  End Sub

  Private Sub frmColorRezepte_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    RemoveHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
    AufbauPar.MethID = -1

  End Sub

  Sub cboMSH_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMsh.SelectedIndexChanged
    Dim i As Short
    Dim j As Integer
    If sender.Selectedindex = -1 Then Exit Sub
    If Not IsNumeric(sender.Selectedvalue) Then Exit Sub
    If sender.SelectedValue = OldMischID Then Exit Sub
    OldMischID = sender.SelectedValue
    AufbauPar.MischID = -1
    AufbauPar.MischID = sender.SelectedValue
    chkKDE.Checked = MenueParam.Misch.Kgx
    txtKDE.Text = MenueParam.Misch.Fde
    cboGLZ.Text = MenueParam.User.Winkel(0).GK(0)
    '
    '
    'Keine Hilfskorrekturen
    '
    If MenueParam.Misch.Ichi = 0 Then
      SplitContHLFAvail.Visible = False
      SplitContHLFSelec.Visible = False
      chkHLFVorlage.Visible = False
    Else
      SplitContHLFAvail.Visible = True
      SplitContHLFSelec.Visible = True
      chkHLFVorlage.Visible = True
    End If
    '
    '
    '
    TblRezept.Rows.Clear()
    For i = 1 To RezptGrid.Count - 1
      RezptGrid(i).TDBFarGridRezept(ier)
    Next i
    '
    '
    '
    'Igx = MenueParam.Misch.Igx
    '
    '
    '
    '
    '
    'Art der Rezeptberechnung
    '
    '
    '
    'lblMINI.Visible = BitWrt(14, MenueParam.User.Visbl)
    'lblMINI.Text = Texxt(572 + MenueParam.Misch.Igx) & " (" & Texxt(306 + MenueParam.Menue.JABST) & ") "
    '
    '
    '
    '
    '
    If Colorthek Then
      DatenFarb.DsetFarbFill(0, chkAltName.Checked)
    Else
      DatenFarb.DsetFarbFill(1, chkAltName.Checked)
    End If

    RezptGrid(1).NewKeynam("MEN")
    RezptGrid(2).NewKeynam("COL")
    RezptGrid(3).NewKeynam("KOR")

    '
    '
    txtGewDL.Text = Format(MenueParam.Menue.Lgew, "###.00")
    txtGewDC.Text = Format(MenueParam.Menue.Cgew, "###.00")
    txtGewDH.Text = Format(MenueParam.Menue.Hgew, "###.00") '
    '
    '
    '
    If Colorthek Then
      KeyMenge = "COL"
    Else
      KeyMenge = "MEN"
    End If
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

    '
    '
    '


    '
    '
    '
    '

    '
    ChkUNT(0).CheckState = CheckState.Indeterminate
    ChkUNT(1).CheckState = CheckState.Unchecked

    '

    '
    For j = 0 To 1
      UntID(j) = -1
      TypID(j) = -1
      SmpID(j) = -1
    Next j

    '
    '
    '
    RezSozpt.Rezepte.clear()
    RezSozpt.Farben.clear()
    RezSozpt.Rezepte.AddRez("MEN", New Recipe)
    RezSozpt.Rezepte.AddRez("COL", RezSozpt.Rezepte("MEN"))
    RezSozpt.Rezepte.AddRez("KOR", New Recipe)
    RezSozpt.Rezepte.AddRez("MZU", New Recipe)
    RezSozpt.Rezepte.AddRez("ZUW", New Recipe)
    RezSozpt.Rezepte.AddRez("NEU", RezSozpt.Rezepte("MEN"))
    RezSozpt.Rezepte.AddRez("ALT", RezSozpt.Rezepte("MEN"))
    RezSozpt.Rezepte.AddRez("KOO", RezSozpt.Rezepte("KOR"))

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

    If GrpRwerte.RwArt(0) = "W" Then
      RezSozpt.Rezepte.kwb(0) = 0
      RezSozpt.Rezepte.kwb(1) = 1
      GrpRwerte(0)("V").kwb = 0
      GrpRwerte(1)("V").kwb = 1
      GrpRwerte(0)("N").kwb = 0
      GrpRwerte(1)("N").kwb = 1
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
      GrpRwerte(0)("N").kwb = 1
      GrpRwerte(1)("N").kwb = 0
      GrpRwerte(0).RefUnt.kwb = 1
      GrpRwerte(1).RefUnt.kwb = 0
      GrpRwerte(0)("R").kwb = 1
      GrpRwerte(1)("R").kwb = 0
      RezGraphics.Vkwb(0) = 1
      RezGraphics.Vkwb(1) = 0
    End If
    GrpRwerte(0)("V").Itp = True
    GrpRwerte(1)("V").Itp = True
    GrpRwerte(0)("N").Itp = False
    GrpRwerte(1)("N").Itp = False
    GrpRwerte(0)("R").Itp = False
    GrpRwerte(1)("R").Itp = False
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
    RezGrid.TDBFarRezStart(ier)
    RezGraphics.GrpRwerte = GrpRwerte

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
      txtBIS(i).Text = Date.Today.ToLocalTime
      txtVON(i).Text = Date.Today.AddDays(-MenueParam.Misch.Tdiff).ToLocalTime
    Next i

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
      RezCheckRad.radWIN(i).Visible = True
      RezCheckRad.radWIN(i).Enabled = False
      RezCheckRad.chkWIN(i).Visible = True
      RezCheckRad.chkWIN(i).Enabled = False
      RezCheckRad.chkWIN(i).Text = LTrim(MenueParam.User.Winkel(i).Chrm)
      chkNextWIN(i).Visible = True
      chkNextWIN(i).Checked = True
      chkNextWIN(i).Text = MenueParam.User.Winkel(i).Chrm
    Next i
    '
    '
    '
 
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
    ' cboWin.Text = cboWin.Items(0)

    '
    '
    Cursor = Cursors.WaitCursor
    txtGGEMax.Text = Format(MenueParam.Misch.Gge, "###0.000")
    txtZUWMax.Text = Format(100.0 * MenueParam.Misch.Zuwag, "###0.00")
    txtDTO.Text = Format(MenueParam.Misch.Dto, "###0.000")
    RezGraphics.DataChanged = False
    Cursor = Cursors.Arrow
    '
    '
    '

    '
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
    '
    If MenueParam.Misch.Transp And Not Colorthek Then
      chkKDE.Checked = MenueParam.Misch.Kgx
      ChkUNT(1).Checked = MenueParam.Misch.Schwrz
    Else
      chkKDE.Checked = False
      ChkUNT(1).Checked = False
    End If
    '
    '
    For i = 2 To 3
      ' PicAufbau(i).WeSc(VKwb(1)) = True   'chkUUU(0)
      'NewConnect(PicAufbau(i), RefKenn)

    Next i

    For i = 0 To 3
      ' MischGroup(i).GroupAufbau()
    Next i
    Call VisVisKorrektur()
    '
    '
    'Messungen mit schwarzem Untergrund
    '
    '
    '
    ' 
    For i = 0 To 1
      btnMessVor(i).Text = Texxt(806 + RezGraphics.Vkwb(i)) & Space(1)
      btnMessNch(i).Text = Texxt(828 + RezGraphics.Vkwb(i)) & Space(1)
      btnMessUnt(i).Text = Texxt(808 + RezGraphics.Vkwb(i)) & Space(1)
      btnMessVOO(i).Text = Texxt(806 + RezGraphics.Vkwb(i)) & Space(1)
      RezCheckRad.chkUUU(i).Text = Texxt(804 + RezGraphics.Vkwb(i))
      lblMessNch(i).Text = Texxt(794 + RezGraphics.Vkwb(i))
      lblMessUnt(i).Text = Texxt(796 + RezGraphics.Vkwb(i))
      'RezCheckRad.chkUUU(i).ToolTipText = Texxt(927 + VKwb(i + 1))
      j = BitInt(RezGraphics.Vkwb(i), RezGraphics.Vkwb(i), MenueParam.Messg.MeArtID)
      btnMessVor(i).Text = btnMessVor(i).Text & Space(1) & Texxt(3918 + j)
      btnMessNch(i).Text = btnMessNch(i).Text & Space(1) & Texxt(3918 + j)
      btnMessUnt(i).Text = btnMessUnt(i).Text & Space(1) & Texxt(3918 + j)
      btnMessVOO(i).Text = btnMessVOO(i).Text & Space(1) & Texxt(3918 + j)
    Next i
    '
    '
    '
    For i = 0 To 1
      txtDickKor(i).Text = MenueParam.Misch.Dicke
      txtDickNch(i).Text = MenueParam.Misch.Dicke
      lblMessUnt(i).Text = ""
      lblMessNch(i).Text = ""
      lblMessVor(i).Text = ""
      GrpRwerte(i).RefUnt.Name = ""
      GrpRwerte(i)("N").Name = ""
      GrpRwerte(i)("V").Name = ""
      GrpRwerte(i)("R").Name = ""
      lblVOR(i).Text = ""
      lblMessVOO(i).Text = ""
    Next i
    For i = 1 To 3
      'lblREE(i).text = ""
      'lblBEM(i).text = ""
    Next
    '
    '
    '
    'Defaultwerte
    '
    '
    RezSozpt.Rezepte(KeyMenge).Dicke(0) = MenueParam.Misch.Dicke
    RezSozpt.Rezepte(KeyMenge).Dicke(1) = MenueParam.Misch.Dicke
    For i = 0 To 1
      TypID(i) = -1
      UntID(i) = -1
      SmpID(i) = -1
    Next i
    '
    '
    'Rezepte einlesen
    '
    '

    If Colorthek Then
      '
      '
      '
      '
      'Colortheksuche
      '
      '
      '
      'cboWin.Enabled = True
      '
      '

      '

      '
    Else
      '
      '
      'cboWin.Enabled = False
      '
      '
      '
      '
      btnSUC_1.PerformClick()
      Application.DoEvents()
    End If
    '
    '
    '
    '
    HandleRezept.GroupList()
    HandleRezeptN.GroupList()
    Call RwertCheck()
  End Sub
  Private Sub chkUNT_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkUNT_0.CheckStateChanged, chkUNT_1.CheckStateChanged, chkKDE.CheckStateChanged, chkNAS.CheckStateChanged
    If IsNothing(MenueParam) Then Exit Sub
    MenueParam.Misch.Kgx = chkKDE.Checked
    Call VisVisKorrektur()
  End Sub


  Private Sub btnSUC_Click(sender As Object, e As System.EventArgs) Handles btnSUC_0.Click, btnSUC_1.Click
    Dim index As Integer
    Dim Coldiff As Boolean
    Dim i As Integer




    index = CInt(sender.name.substring(7, 1))
    Call ClearRwerte(Colorthek, GrpRwerte)
    TblRezept.Rows.Clear()
    RezHLFSelecID.Clear()
    RezHlfAvailID.Clear()
    Application.DoEvents()
    Select Case index



      Case 0
        '
        '
        'Colorthek
        '
        '
        btnKorrektur.Enabled = False
        btnKorrSpezial.Enabled = False
        '
        'Call NewRezeptTab(cboFAR.SelectedIndex, ListID, RezepteTab, FarbRezeptTab)
        RezGraphics.DataChanged = False
        Cursor = Cursors.WaitCursor
        Call RezActiv(2)
        RezGrid = RezptGrid(2)
        If chkRWRT.Checked And GrpRwerte(0)("V").IVoNa Then
          TblRezept.Columns("RWERT_ID").DefaultValue = GrpRwerte(0)("V").ID
        Else
          TblRezept.Columns("RWERT_ID").DefaultValue = -1
        End If
        ConnRezept.SuspendBinding()
        ReDim ListFarbid(lstFAR.Items.Count - 1)
        For i = 0 To lstFAR.Items.Count - 1
          ListFarbid(i) = lstFAR.Items(i).id
        Next

        If Not HandleRezept.SqlRezeptColth(cboFAR.SelectedIndex, cboGRP.SelectedValue, chkDatum(index).Checked, Date.Parse(txtVON(index).Text), Date.Parse(txtBIS(index).Text).AddDays(1.0), radRwerte(1).Checked, radRwerte(2).Checked, txtSUC(index).Text, ListFarbid, RezHlfAvailID, TblRezept) Then
          Cursor = Cursors.Default
          ConnRezept.ResumeBinding()
          Exit Sub
        End If
        chkWithTempl.Checked = False
        chkWithTempl.Enabled = False
        chkHLFVorlage.Checked = False
        chkHLFVorlage.Enabled = False
        For i = 0 To chkNextWIN.Count - 1
          If chkNextWIN(i).Checked Then
            Exit For
          End If
        Next
        If i = chkNextWIN.Count Then
          MsgBox(Texxt(2995), MsgBoxStyle.OkOnly, Texxt(2000))
        Else
          Call NextRefValues(cboGRP.SelectedValue, chkDatum(index).Checked, Date.Parse(txtVON(index).Text), Date.Parse(txtBIS(index).Text).AddDays(1.0), txtSUC(index).Text, Coldiff, TblRezept)
        End If

        Cursor = Cursors.Default
        RezGraphics.DataChanged = False

        If ViewRezept.Count = 0 Then
          MessageBox.Show(Texxt(2953))
          RezGrid.TDBFar.Visible = False
          RezGrid.txtFooter.Visible = False
          ConnRezept.ResumeBinding()
          Exit Sub
        Else

          If Coldiff Then
            ViewRezept.Sort = "DEMOD"
            ViewHlfAvail.Sort = "DEMOD"
            btnEingabe.Enabled = True
          Else
            ViewRezept.RowFilter = ""
            ViewHlfAvail.Sort = ""
            btnEingabe.Enabled = False
          End If
          RezGraphics.DataChanged = False
          dbgREZ(2).Visible = True
          KeyMenge = "COL"

          btnDarstellung.Enabled = True
          Application.DoEvents()

          'dbgREZ(2).Row = 0
          'Call ForceColChange(2)
          cboMsh.Enabled = False
          cboGRP.Enabled = False
          btnDarstellung.PerformClick()
          ConnRezept.ResumeBinding()
        End If
        '

        '
        '

        '

      Case 1
        '
        '
        'Korrekturrezepte
        '
        '
        Call RezActiv(1)
        RezGrid = RezptGrid(1)
        '
        RezGraphics.DataChanged = False
        Cursor = Cursors.WaitCursor
        RezSozpt.Rezepte(KeyMenge).clear()
        RezGrid.TDBFarRezStart(ier)
        ConnRezept.SuspendBinding()
        Try
          If Not HandleRezept.SqlRezeptKorr(cboGRP.SelectedValue, chkDatum(index).Checked, chkWithTempl.Checked, Date.Parse(txtVON(1).Text), Date.Parse(txtBIS(1).Text).AddDays(1.0), txtSUC(index).Text, RezHlfAvailID, TblRezept) Then
            Cursor = Cursors.Default
            ConnRezept.ResumeBinding()
            Exit Sub
          End If
        Catch
          Cursor = Cursors.Default
          Exit Sub
        End Try
        If IsNothing(ConnRezept) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If IsNothing(ConnRezept.Current) Then
          Cursor = Cursors.Default
          Exit Sub
        End If

        If IsNumeric(ConnRezept.Current("RWERT_ID")) Then
          chkHLFVorlage.Checked = chkWithTempl.Checked
          chkHLFVorlage.Enabled = chkWithTempl.Checked
        Else
          chkHLFVorlage.Checked = False
          chkHLFVorlage.Enabled = False
        End If


        '
        Application.DoEvents()
        Cursor = Cursors.Default
        RezGraphics.DataChanged = False
        ConnRezept.ResumeBinding()
        If ViewRezept.Count = 0 Then
          MessageBox.Show(Texxt(2953))
          dbgREZ(1).Visible = False
          ConnRezept.ResumeBinding()
          For i = 0 To 1
            lblMessUnt(i).Text = GrpRwerte(i).RefUnt.Name
            lblMessVor(i).Text = GrpRwerte(i)("V").Name
            lblMessNch(i).Text = GrpRwerte(i)("N").Name

          Next
          Exit Sub
        Else
          dbgREZ(1).Visible = True
          Application.DoEvents()
          'dbgREZ(1).Row = 0
          'Call ForceColChange(1)
          btnKorrektur.Enabled = True
          btnKorrSpezial.Enabled = True
          btnDarstellung.Enabled = True
        End If

        '
        '
    End Select


    RezGrid.TDBFar.Visible = True
    chkUMR.Checked = False
    '
    '


  End Sub
  Sub NextRefValues(GID As Integer, DatExist As Boolean, DatVon As Date, DatBis As Date, SuString As String, ByRef Coldiff As Boolean, TableRez As DataTable)
    Dim i As Integer
    Dim Rezeptview As DataView
    Dim TblRwertRezept As New DataTable
    Dim TabRez As DataTable
    Dim AdaptRwertrezept As New OleDbDataAdapter
    Dim CmdRwertRezept As New OleDbCommand("", Cndat)
    Dim Strrezepte As String
    Dim StrRezNoGrund As String
    Dim kw As Integer
    Dim DESUM As Single
    Dim RBList As List(Of Single())
    Dim DE As Single
    Dim DL As Single
    Dim DC As Single
    Dim DH As Single
    Dim DA As Single
    Dim DB As Single
    Dim RP() As Single
    Dim Rhilf() As Single
    Dim NWE As Integer
    Dim CHRM As String
    Coldiff = False
    '
    '
    TabRez = New DataTable
    AdaptRwertrezept.SelectCommand = CmdRwertRezept
    If Not HandleRezept.SqlRezeptNoGrund(GID, DatExist, DatVon, DatBis, SuString, TabRez) Then
      Exit Sub
    End If

    StrRezNoGrund = StrLin(TabRez, "REZEPT_ID")
    TabRez.Rows.Clear()
    ' '
    Rezeptview = New DataView(TableRez) '
    '
    If radRwerte(2).Checked And chkRWRT.Checked And GrpRwerte(0)("V").IVoNa Then
      '
      '
      'R-Werte einlesen
      '
      '
      TblRwertRezept.Rows.Clear()
      Strrezepte = StrLin(TableRez, "REZEPT_ID")
      CmdRwertRezept.CommandText = "SELECT * FROM TBL_RWERT INNER JOIN TBL_REZEPT_RWERT ON TBL_RWERT.RWERT_ID=TBL_REZEPT_RWERT.RWERT_ID" _
      & " WHERE MISCH_ID=" & MenueParam.MischID & " AND TBL_RWERT.MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_KWB=1 AND REZEPT_ID IN " & Strrezepte
      If Not FillDatset(AdaptRwertrezept, TblRwertRezept) Then
        Exit Sub
      End If
      '
      '
      '
      '
      'Mit oder ohne Grunddaten
      '
      '
      For i = TableRez.Rows.Count - 1 To 0 Step -1
        Rezeptview.RowFilter = "REZEPT_ID=" & TableRez.Rows(i)("REZEPT_ID") & " AND REZEPT_ID IN " & StrRezNoGrund
        If Rezeptview.Count = 0 Then
          '
          'Grunddaten vorhanden
          '
          TableRez.Rows(i)("OPTDA") = True
        Else
          '
          'Grunddaten nicht vorhanden
          '
          TableRez.Rows(i)("OPTDA") = False
          If chkWithOptData.Checked Then
            '
            'Rezept entfernen
            '
            '
            TableRez.Rows.RemoveAt(i)
          End If
          '
        End If
      Next

      '
      '
      '
      'Farbabstände berechnen
      '
      '
      '
      '
      '
      Cursor = Cursors.WaitCursor
      NWE = MenueParam.User.Winkel.Wsol.Nwe
      RBList = New List(Of Single())
      For kw = 0 To MenueParam.User.Winkel.Km - 1
        CHRM = MenueParam.User.Winkel(kw).Chrm
        RBList.Add(GrpRwerte(0)(0).RefKurv(CHRM).R)
      Next
      ReDim RP(NWE - 1)
      Call GetWinkNormGk("FAR", 1, MenueParam.User.Winkel, ier)
      Call GetMenueParam("FAR", ier)
      For i = 0 To TblRwertRezept.Rows.Count - 1
        Rhilf = GetSingles(TblRwertRezept.Rows(i)("RWERT_RWERT"))
        DESUM = 0.0
        For kw = 0 To MenueParam.User.Winkel.Km - 1
          If chkNextWIN(kw).Checked Then
            Array.Copy(Rhilf, kw * NWE, RP, 0, NWE)
            Call quali.FarbDifferenzRef(0, kw, RezGraphics.Knlz, MenueParam.User.Winkel, RBList(kw), RP, DE, DL, DC, DH, DA, DB, ier)
            DESUM = DESUM + MenueParam.User.Winkel(kw).IhrmGew ^ 2 * ((txtGewDL.Text * DL) ^ 2 + (txtGewDC.Text * DC) ^ 2 + (txtGewDH.Text * DH) ^ 2)
          End If
        Next kw
        Rezeptview.RowFilter = "REZEPT_ID=" & TblRwertRezept.Rows(i)("REZEPT_ID")
        If Rezeptview.Count > 0 Then
          Rezeptview(0)("DEMOD") = Sqrt(DESUM)
        End If
      Next i
      '
      '
      '
      '
      Coldiff = True
    End If

    TableRez.AcceptChanges()
    '
    Cursor = Cursors.Default
    '
    '
    If radRwerte(2).Checked Then
      '
      '
      'alle Rezepte haben eine Nachstellung
      'Rezepte nach RezHlfAvail speichern
      '
      '
      For i = 0 To TableRez.Rows.Count - 1
        RezHlfAvailID.Add(TableRez.Rows(i)("REZEPT_ID"))
      Next
    End If
    Rezeptview.Dispose()
    TblRwertRezept.Dispose()
    AdaptRwertrezept.Dispose()
    CmdRwertRezept.Dispose()
    TabRez.Dispose()
  End Sub
  Private Sub frmColorRezepte_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Resize
    Call ResizeChild(Me)
  End Sub
  '
  '
  '
  '
  '


  Private Sub cboGLZ_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboGLZ.Click
    MenueParam.User.Winkel(0).GK(0) = CSng(cboGLZ.Text) * MenueParam.User.Winkel(0).Iglz
  End Sub


  Private Sub chkDatum_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkDatum_0.CheckedChanged, chkDatum_1.CheckedChanged
    Dim index As Integer
    If sender.name = "" Then Exit Sub
    index = CInt(sender.name.substring(9, 1))
    If sender.checked Then
      lblBIS(index).Visible = True
      txtBIS(index).Visible = True
      txtVON(index).Visible = True
    Else
      lblBIS(index).Visible = False
      txtBIS(index).Visible = False
      txtVON(index).Visible = False

    End If
  End Sub


  Private Sub chkUMR_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkUMR.CheckedChanged
    Dim i As Integer

    If Not chkUMR.Checked Then
      KeyMenge = "MEN"
      RezGrid.txtFooter.Visible = True
      cboMng.Enabled = False
      cboPro_0.Enabled = False
      cboPro_1.Enabled = False
      txtMng(0).Enabled = False
      txtMng(1).Enabled = False
      txtPro(0).Enabled = False
      txtPro(1).Enabled = False
      txtDickNch(0).Enabled = False
      txtDickNch(1).Enabled = False
      txtDickKor(0).Enabled = False
      txtDickKor(1).Enabled = False
      btnMessVor(0).Enabled = False
      btnMessVor(1).Enabled = False
      btnMessUnt(0).Enabled = False
      btnMessUnt(1).Enabled = False
      lblMessVor(0).Enabled = False
      lblMessVor(1).Enabled = False
      lblMessUnt(0).Enabled = False
      lblMessUnt(1).Enabled = False
      btnStoreAlt.Enabled = False
    Else
      KeyMenge = "NEU"
      'RezGrid.TDBFar.Columns(CInt(RezGrid.TDBFar.Tag)).DataField = "NEU"
      RezGrid.txtFooter.Visible = False
      'RezGrid.lblSUM.Visible = False
      ' RezGrid.TDBFar.Columns(CInt(RezGrid.TDBFar.Tag)).Locked = False
      If BitWrt(22, MenueParam.User.Enabl) Then
        For i = 0 To txtDickNch.Count - 1
          txtDickNch(i).Enabled = True
        Next i
      End If
      If BitWrt(15, MenueParam.User.Enabl) Then
        cboMng.Enabled = True
        txtMng(0).Enabled = True
        txtMng(1).Enabled = True
      End If
      If BitWrt(16, MenueParam.User.Enabl) Then
        cboPro_0.Enabled = True
        cboPro_1.Enabled = True
        txtPro(0).Enabled = True
        txtPro(1).Enabled = True
      End If
      If BitWrt(22, MenueParam.User.Enabl) Then
        txtDickNch(0).Enabled = True
        txtDickNch(1).Enabled = True
        txtDickKor(0).Enabled = True
        txtDickKor(1).Enabled = True
      End If
      btnMessUnt(0).Enabled = True
      btnMessUnt(1).Enabled = True
      lblMessUnt(0).Enabled = True
      lblMessUnt(1).Enabled = True
      If Not Colorthek Then
        btnMessVor(0).Enabled = True
        btnMessVor(1).Enabled = True
        lblMessVor(0).Enabled = True
        lblMessVor(1).Enabled = True
      End If
      btnStoreAlt.Enabled = True
    End If
    RezptGrid(1).NewKeynam(KeyMenge)
    RezGrid.TDBFarRezStart(ier)
  End Sub



  '
  '
  '
  '
  '
  'Events für Colortheksuche
  '
  '
  '
  '
  '
  '
  '
  Private Sub chkRWRT_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkRWRT.CheckedChanged
    If chkRWRT.Checked Then
      btnMessVOO_0.Visible = True
      lblMessVOO_0.Visible = True
      panGewichte.Visible = True
      panNextWIN.Visible = True
      If GrpRwerte(0)("V").ID > -1 Then
        GrpRwerte(0)("V").IVoNa = True
      End If
    Else
      btnMessVOO_0.Visible = False
      lblMessVOO_0.Visible = False
      panGewichte.Visible = False
      GrpRwerte(0)("V").ID = -1
      GrpRwerte(0)("V").IVoNa = False
      lblMessVOO_0.Text = ""
      panNextWIN.Visible = False
    End If


  End Sub
  Private Sub radRwerte_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles radRwerte_0.CheckedChanged, radRwerte_1.CheckedChanged, radRwerte_2.CheckedChanged
    Dim index As Integer
    Dim i As Integer
    If sender.name = "" Then Exit Sub
    ' TDBRezept.Visible = False
    If radRwerte Is Nothing OrElse radRwerte.Count = 0 Then Exit Sub
    For i = 0 To radRwerte.Count - 1
      If radRwerte(i).Checked Then
        index = i
        Exit For
      End If
    Next
    If index = 2 Then
      chkRWRT.Visible = True
      chkWithOptData.Visible = True
      panGewichte.Visible = True
    Else
      chkRWRT.Checked = False
      chkWithOptData.Visible = False
      chkRWRT.Visible = False
      panGewichte.Visible = False
    End If
  End Sub
  Private Sub lstFAQ_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstFAQ.Click
    Dim i As Integer
    For i = 0 To lstFAR.Items.Count - 1
      If lstFAQ.SelectedItem("FARBM_ID") = lstFAR.Items(i).id Then
        Exit Sub
      End If
    Next i
    lstFAR.Items.Add(New ListTextID(lstFAQ.SelectedItem("FARBM_ID"), lstFAQ.SelectedItem("FARBM_NAME")))
  End Sub



  Private Sub lstFAR_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstFAR.DoubleClick
    Dim i As Integer
    If lstFAR.SelectedItem Is Nothing Then Exit Sub
    For i = 0 To lstFAR.Items.Count - 1
      If lstFAR.Items(i).id = lstFAR.SelectedItem.id Then
        lstFAR.Items.RemoveAt(i)
        Exit For
      End If
    Next
  End Sub

  Private Sub lblVOR_TextChanged(sender As Object, e As System.EventArgs) Handles lblVOR_0.TextChanged, lblVOR_1.TextChanged
    Dim index As Integer
    index = CInt(sender.name.substring(7, 1))
    ToolTipSuchKorr.SetToolTip(sender, GrpRwerte("W")(index).Bem)
  End Sub

  Private Sub btnColorthek_Click(sender As Object, e As System.EventArgs) Handles btnColorthek.Click
    'cboWin.Enabled = True
    cboMini.Enabled = BitWrt(14, MenueParam.User.Enabl)
    cboSkal.Enabled = False
    btnKorrektur.Enabled = False
    btnKorrSpezial.Enabled = False
    btnEingabe.Enabled = False
    btnDarstellung.Enabled = False
    cboMsh.Enabled = True
    cboGRP.Enabled = True
    SplitVisible = 0
    KeyMenge = "COL"

    'NewGridPic(0)
  End Sub
  Private Sub btnKorrektur_Click(sender As Object, e As System.EventArgs) Handles btnKorrektur.Click

    Dim i As Integer
    Dim k As Integer
    Dim kk As Integer
    Dim UntHlf(1) As Integer
    Dim TypHlf(1) As Integer
    Dim SmpHlf(1) As Integer
    Dim RezHlfID As Integer
    Dim KeyHlf As String
    Dim KFI As Integer
    ier = 0
    '
    '
    '
    cboMini.Enabled = False
    Call RezActiv(1)
    RezGrid = RezptGrid(1)
    'Call RefreshCurrent()
    RezGrid.TDBFarRezStart(ier)
    chkUMR.CheckState = CheckState.Indeterminate
    chkUMR.Checked = False
    '
    'Winkel/Licharten
    '
    '

    '
    '
    For i = 0 To RezCheckRad.chkUUU.Count - 1
      RezCheckRad.chkUUU(i).Enabled = True
      RezCheckRad.radUUU(i).enabled = True
    Next i
    For i = 0 To MenueParam.Messg.Winkel.Km - 1
      RezCheckRad.chkWIN(i).enabled = True
      RezCheckRad.radWIN(i).enabled = True
    Next i
    For i = 0 To MenueParam.Normfa.Nlz - 1
      RezCheckRad.radNLA(i).enabled = True
    Next i
    'cboWin.Enabled = True
    chkFAR.Enabled = True
    cboMsh.Enabled = False
    chkGrundKorr.Enabled = False
    chkPicNachstellung.Checked = True
    chkPicKorrektur.Checked = True
    Call ZuwaagGGEKorr(MenueParam.Misch.Igx, CSng(txtZUWMax.Text), CSng(txtGGEMax.Text), CSng(txtDTO.Text))
    '
    '
    btnEingabe.Enabled = False
    btnDarstellung.Enabled = False
    '
    '
    '

    RezID = RezSozpt.Rezepte(KeyMenge).ID
    RezSozpt.MngMin = txtMNG_0.Text
    RezSozpt.MngMax = txtMNG_1.Text
    RezSozpt.ProzMin = txtPRO_0.Text
    RezSozpt.ProzMax = txtPRO_1.Text
    Call HandleRezept.KorSpei(KeyMenge, RezID, ConnRezept.Current("User_ID"), chkARCH.CheckState, cboGRP.SelectedValue, RwWrRezept, RezGraphics, RezSozpt, UntID, TypID, SmpID, ier)
    If ier <> 0 Then
      GoTo ErgFeh
    End If

    If KeyMenge = "MEN" Or KeyMenge = "NEU" Then
      SplitContainSuchKorr.Visible = False
    Else
      GoTo ergfeh
    End If
    Cursor = Cursors.WaitCursor
    btnColorthek.Enabled = False
    btnRezept.Enabled = False
    btnKorrektur.Enabled = False
    btnKorrSpezial.Enabled = False
    txtDisMax.Text = Format(RezSozpt.Rezepte(KeyMenge).Dicke(RezGraphics.Vkwb(0)), "##0.00")
    If KeyMenge = "MEN" Then
      'RezptGrid(1).TDBFar.Visible = False
      'For i = 0 To PicAufbau(1).Count - 1
      ' PicAufbau(1)(i).Visible = False
      ' Next i
    End If
    lblARB.SetBounds(Me.Location.X, Me.Location.Y, _
    Me.Size.Width, Me.Size.Height)
    lblARB.Visible = True
    '
    Application.DoEvents()
    '
    '
    '
    'Hilfskorekturen übernehmen
    '==============================
    '
    'Löschen
    '
    ' 
    For k = RezSozpt.Rezepte.RezCount - 1 To 0 Step -1
      If IsNumeric(RezSozpt.Rezepte.RezKey(k)) Then
        GrpRwerte(0).Remove("V" & RezSozpt.Rezepte.RezKey(k))
        GrpRwerte(1).Remove("V" & RezSozpt.Rezepte.RezKey(k))
        GrpRwerte(0).Remove("N" & RezSozpt.Rezepte.RezKey(k))
        GrpRwerte(1).Remove("N" & RezSozpt.Rezepte.RezKey(k))
        RezSozpt.Rezepte.RemoveRez(k)
      End If
    Next
    '

    'Übernehmen 
    '
    '
    '

    kk = 0
    For k = 0 To ViewHlfSelec.Count - 1
      kk = kk + 1
      KeyHlf = KeyRe(kk)
      RezSozpt.Rezepte.AddRez(KeyHlf, New Recipe)
      RezHlfID = ViewHlfSelec(k)("REZEPT_ID")
      Call RwWrRezept.ReadRezeptFarbGrund(KeyHlf, RezHlfID, RezSozpt, UntHlf, TypHlf, SmpHlf, ier)
      Umr.CalcBamng(KeyHlf, RezSozpt, ier)
      For i = 0 To 1


        '
        'Untergrund
        '
        '

        If MenueParam.Misch.Transp AndAlso UntHlf(RezGraphics.Vkwb(i)) <> UntID(RezGraphics.Vkwb(i)) Then
          If MessageBox.Show(Texxt(3953) & ": " & RezSozpt.Rezepte(KeyHlf).Name & Chr(13) & Texxt(159) & "?", Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.No Then
            GoTo ergfeh
          End If
          RezSozpt.Rezepte.RemoveRez(KeyHlf)
          For j = 0 To 1
            GrpRwerte(j).Remove("V" & KeyHlf)
            GrpRwerte(j).Remove("N" & KeyHlf)
          Next j
          kk = kk - 1
          Exit For
        End If
        '
        'Vorlage für Hilfkorrektur lesen
        '
        '
        GrpRwerte(i).Add("V" & KeyHlf, GrpRwerte(i)("V"))
        '
        'Nachstellung für Hilfkorrektur lesen
        '
        '
        GrpRwerte(i).Add("N" & KeyHlf, New RefValue)
        If SmpHlf(RezGraphics.Vkwb(i)) > 0 Then
          GrpRwerte(i)("N" & KeyHlf).ID = SmpHlf(RezGraphics.Vkwb(i))
          GrpRwerte(i)("N" & KeyHlf).kwb = RezGraphics.Vkwb(i)
          GrpRwerte(i)("N" & KeyHlf).IVoNa = False
          GrpRwerte(i)("N" & KeyHlf).Itp = False
          '
          Call ReWrRwert.ReadRwert(GrpRwerte(i)("N" & KeyHlf).ID, GrpRwerte(i)("N" & KeyHlf), ier)
          If ier = 0 Then
            If chkMessNch(i).Checked Then
              GrpRwerte(i)("N" & KeyHlf).IVoNa = True
              GrpRwerte(i)("N" & KeyHlf).Nr = k + 1
            End If
          End If

        End If
      Next i
    Next k
    '
    '
    '
    '
    RezSozpt.MngMin = txtMng(0).Text
    RezSozpt.MngMax = txtMng(1).Text
    RezSozpt.ProzMin = txtPro(0).Text
    RezSozpt.ProzMax = txtPro(1).Text
    RezSozpt.INO = cboMng.SelectedIndex
    RezSozpt.INP = cboPro_0.SelectedIndex
    RezSozpt.INQ = cboPro_1.SelectedIndex
    RezSozpt.IVOL = chkVOL.CheckState

    '
    '
    '
    '==============================
    '
    '****************************
    'Berechnung des Korrekturrezeptes
    '****************************
    '
    '
    '
    RezSozpt.Rezepte("ZUW").clear()
    RezSozpt.Rezepte("MZU").clear()
    '
    '
    '
    '
    RezSozpt.Rezepte(KeyMenge).Dicke(RezGraphics.Vkwb(0)) = Singl(txtDickNch(0).Text)
    RezSozpt.Rezepte(KeyMenge).Dicke(RezGraphics.Vkwb(1)) = Singl(txtDickNch(1).Text)
    RezSozpt.Rezepte("KOR").Dicke(RezGraphics.Vkwb(0)) = Singl(txtDickKor(0).Text)
    RezSozpt.Rezepte("KOR").Dicke(RezGraphics.Vkwb(1)) = Singl(txtDickKor(1).Text)


    For i = 0 To RezptGrid.Count - 1
      If Not IsNothing(RezptGrid(i)) Then
        RezptGrid(i).ChkGrundKorr = chkGrundKorr.CheckState
      End If
    Next i
    '
    '
    If BitWrt(22, MenueParam.User.Sonst) Then
      Cursor = Cursors.WaitCursor
      For i = 0 To RezSozpt.Farben.FarbCount - 1
        Call HandleRezept.GrundGlzGrd(MenueParam.MischID, RezSozpt.Farben(i).ID, RezSozpt.Farben(i), RezSozpt.Rezepte(KeyMenge).GlzGrd, ier)
      Next i
      Cursor = Cursors.Default
    End If

    '
    KFI = RezGraphics.KFIT + 8 + 16 * chkGrundKorr.CheckState
    CalcRezept.KorrekturRezept(KFI, MenueParam.User.Winkel, KeyMenge, "KOR", RezSozpt, GrpRwerte, ier)

    '
    '
    If ier > 0 Then
      GoTo ErgFeh
    End If

    RezDruck.KeyNam = "KOR"
    Call RezActiv(3)
    RezGrid = RezptGrid(3)
    RezCheckRad.Picauf = PicAufbau(3)
    If chkRZAen.Checked Then
      RezGrid.NewKeynam("KOO")
    Else
      RezGrid.NewKeynam("KOR")
    End If
    RezGrid.TDBFarRezStart(ier)
    Call GraphicPlotRwert(New String() {"V", "N", "R"}, RezGrid)

    RezCheckRad.Picauf.Refresh()
    '
    If chkRefGew.Checked And MenueParam.Misch.Igx = 7 Then
      RezGrid.Picauf.PicFarbXYZ.Visible = False
      dbgRefGew.Visible = True
    Else
      RezGrid.Picauf.PicFarbXYZ.Visible = True
      dbgRefGew.Visible = False
    End If
    lblARB.Visible = False
    RezGrid.TDBFar.Visible = True

    '
    Cursor = Cursors.Default
    '
    SplitContainSuchKorr.Visible = True

    SplitVisible = 3

    '

    '
    '
    'RezDruck.KeyNam = rznr
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
    If GrpRwerte(0).RefUnt.IVoNa And GrpRwerte(1).RefUnt.IVoNa Then
      DiHsc(0) = 0.001 * RezSozpt.Rezepte("KOR").Dicke(RezGraphics.Vkwb(0))
      DiHsc(1) = 2.0# * RezSozpt.Rezepte("KOR").Dicke(RezGraphics.Vkwb(0))
      hscDIS.Maximum = 30000
      hscDIS.Minimum = 0
      hscDIS.Value = hscDIS.Maximum
      hscDIS.Enabled = True
      'hscDIS.Value = 0.5 * (hscDIS.Maximum - hscDIS.Minimum)
      hscDIS.Value = hscDIS.Minimum + (RezSozpt.Rezepte("KOR").Dicke(RezGraphics.Vkwb(0)) - DiHsc(0)) / (DiHsc(1) - DiHsc(0)) * (hscDIS.Maximum - hscDIS.Minimum)
      lblDIS.Visible = True
      panDicke.Visible = True
      txtDIS.Visible = True
      lblDisMax.Visible = True
      txtDisMax.Visible = True
      hscDIS.Visible = True
      txtDisMax.Text = DiHsc(1)
      txtDIS.Text = Format(RezSozpt.Rezepte(KeyMenge).Dicke(RezGraphics.Vkwb(0)), "##0.00")
      If BitWrt(0, MenueParam.Messg.MeArtID) = BitWrt(1, MenueParam.Messg.MeArtID) Then
        lblKDS.Visible = True
        txtKDS.Visible = True
      End If
      hscDIS.Visible = True
      Call hscDIS_Scroll(hscDIS, New ScrollEventArgs(ScrollEventType.EndScroll, hscDIS.Value))
    Else
      lblDIS.Visible = False
      txtDIS.Visible = False
      panDicke.Visible = False
      lblDisMax.Visible = False
      txtDisMax.Visible = False
      hscDIS.Visible = False
      lblKDS.Visible = False
      txtKDS.Visible = False
      hscDIS.Enabled = False

    End If
    '

    ' hscMNG.Visible = False
    '
    'Zuwaageberechnung
    '
    If MenueParam.Misch.Igx = 3 Or MenueParam.Misch.Igx = 5 Then
      'RezSozpt.MngMin = Umr.MngINO
      If Abs(RezSozpt.MngMax - RezSozpt.MngMin) > 0.0001 Then
        hscZUW.Maximum = 1000
        hscZUW.Minimum = 0.0#
        InoMeng = RezGrid.Umr.MngINO("MZU", RezSozpt, iee)
        HscMax = NewValHscZuw(InoMeng)
        hscZUW.Value = HscMax
        txtZUW.Text = Format(100.0 * ZuwAkt(CSng(txtZUWMax.Text)) / RezSozpt.MngMin, "###0.00")
      End If
    End If
    '
    '
    'hscMNG.Enabled = False
    btnKorrektur.Enabled = True
    btnKorrSpezial.Enabled = True
    'cboWin.Enabled = True
    If Colorthek Then
      btnColorthek.Enabled = True
    Else
      btnRezept.Enabled = True
    End If
    Cursor = Cursors.Default
    btnEingabe.Enabled = True
    btnDarstellung.Enabled = True
    Exit Sub
    '
    '
    '
    'Fehler
    '
ErgFeh:

    If Colorthek Then
      SplitVisible = 1
      btnColorthek.Enabled = True
    Else
      SplitVisible = 1
      btnRezept.Enabled = True
      btnEingabe.Enabled = True

    End If
    btnKorrektur.Enabled = True
    btnKorrSpezial.Enabled = True
    SplitContainSuchKorr.Visible = True
    Cursor = Cursors.Default
    btnEingabe.Enabled = True
    btnDarstellung.Enabled = True

  End Sub



  Private Sub btnRezept_Click(sender As Object, e As System.EventArgs) Handles btnRezept.Click
    'cboWin.Enabled = False
    cboMini.Enabled = BitWrt(14, MenueParam.User.Enabl)
    chkGrundKorr.Enabled = True
    Call RezActiv(1)
    RezGrid = RezptGrid(1)
    'Call RefreshCurrent()
    RezGrid.TDBFarRezStart(ier)
    cboMsh.Enabled = True
    SplitVisible = 1
    chkUMR.Checked = True
    chkUMR.Checked = False
    btnDarstellung.Enabled = True
    btnKorrektur.Enabled = True
    btnKorrSpezial.Enabled = True
    RezGraphics.DataChanged = False
    btnSUC_1.PerformClick()
  End Sub
  Private Sub btnEingabe_Click(sender As Object, e As System.EventArgs) Handles btnEingabe.Click
    cboMini.Enabled = BitWrt(14, MenueParam.User.Enabl)
    btnKorrektur.Enabled = False
    btnKorrSpezial.Enabled = False
    chkUMR.CheckState = CheckState.Indeterminate
    chkUMR.Checked = False

    Call RezActiv(1)
    RezGrid = RezptGrid(1)
    Call RefreshCurrent()
    RezGrid.TDBFarRezStart(ier)
    If Colorthek Then
      btnDarstellung.Enabled = True
      btnEingabe.Enabled = True
      If ViewRezept.Count > 0 Then
        btnKorrektur.Enabled = ConnRezept.Current("OPTDA") And GrpRwerte(0)("V").IVoNa
        btnKorrSpezial.Enabled = btnKorrektur.Enabled
        dbgREZ(1).Visible = True
        RezGrid.TDBFar.Visible = True
      End If
    Else
      btnEingabe.Enabled = True
      btnDarstellung.Enabled = False

      btnKorrektur.Enabled = True
      btnKorrSpezial.Enabled = True
      'cboWin.Enabled = False
      cboSkal.Enabled = False

    End If
    Application.DoEvents()

    SplitVisible = 1
  End Sub
  '
  '
  Private Sub btnDarstellung_Click(sender As Object, e As System.EventArgs) Handles btnDarstellung.Click

    Dim i As Integer
    cboMini.Enabled = False
    RezCheckRad.Picauf = PicAufbau(2)
    Call RezActiv(2)
    RezGrid = RezptGrid(2)
    RezGrid.TDBFarRezStart(ier)
    For i = 0 To RezCheckRad.chkUUU.Count - 1
      RezCheckRad.chkUUU(i).Enabled = True
      RezCheckRad.radUUU(i).enabled = True
    Next i
    For i = 0 To MenueParam.Messg.Winkel.Km - 1
      RezCheckRad.chkWIN(i).enabled = True
      RezCheckRad.radWIN(i).enabled = True
    Next i
    For i = 0 To MenueParam.Normfa.Nlz - 1
      RezCheckRad.radNLA(i).enabled = True
    Next i
    'cboWin.Enabled = True
    chkFAR.Enabled = True
    cboSkal.Enabled = True
    If Colorthek And GrpRwerte(0)("V").ID < 0 Then
      btnKorrektur.Enabled = False
      btnKorrSpezial.Enabled = False
    Else
      btnKorrektur.Enabled = True
      btnKorrSpezial.Enabled = True

    End If

    SplitVisible = 2
  End Sub
  '
  '
  '
  '
  Private Sub btnMessVor_Click(sender As Object, e As System.EventArgs) Handles btnMessVor_0.Click, btnMessVor_1.Click
    Dim index As Integer
    index = CInt(sender.name.substring(11, 1))
    '
    '
    RcmdRef = index + 11
    GetPutReflex.Messrefel = GrpRwerte(index)("V")

    GetPutReflex.Iarch = 0
    GetPutReflex.Captext = btnMessVor(index).Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(index), RezGraphics.Vkwb(index), MenueParam.Messg.MeArtID)
    MenueParam.Messg.MeArtLock = BitWrt(RezGraphics.Vkwb(index), MenueParam.Messg.MeArtID)
    Me.Enabled = False
    GetPutReflex.ReflexWerte(False)
    Me.Enabled = True
    '
    '
  End Sub

  Private Sub btnMessNch_Click(sender As Object, e As System.EventArgs) Handles btnMessNch_0.Click, btnMessNch_1.Click
    Dim index As Integer
    index = CInt(sender.name.substring(11, 1))

    '
    RcmdRef = index + 1
    'GetPutReflex.kenn = MenueParam.Messg.Kenn
    GetPutReflex.Messrefel = GrpRwerte(index)("N")
    GetPutReflex.Iarch = 0
    GetPutReflex.Captext = btnMessNch(index).Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(index), RezGraphics.Vkwb(index), MenueParam.Messg.MeArtID)
    MenueParam.Messg.MeArtLock = BitWrt(RezGraphics.Vkwb(index), MenueParam.Messg.MeArtID)
    Me.Enabled = False
    GetPutReflex.ReflexWerte(False)
    Me.Enabled = True



    '
    '

  End Sub

  Private Sub btnMessUnt_Click(sender As Object, e As System.EventArgs) Handles btnMessUnt_0.Click, btnMessUnt_1.Click
    Dim index As Integer
    index = CInt(sender.name.substring(11, 1))
    RcmdRef = -index - 1

    '
    'Messung
    '
    '

    GetPutReflex.Iarch = 1
    GetPutReflex.Messrefel = GrpRwerte(index).RefUnt

    GetPutReflex.Captext = btnMessUnt(index).Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(index), RezGraphics.Vkwb(index), MenueParam.Messg.MeArtID)
    MenueParam.Messg.MeArtLock = BitWrt(RezGraphics.Vkwb(index), MenueParam.Messg.MeArtID)
    Me.Enabled = False
    GetPutReflex.ReflexWerte(False)
    Me.Enabled = True

    '

  End Sub
  Private Sub btnVOO_Click(sender As Object, e As System.EventArgs) Handles btnMessVOO_0.Click, btnMessVOO_1.Click
    Dim index As Integer
    index = CInt(sender.name.substring(11, 1))

    '
    RcmdRef = -(index + 11)
    GetPutReflex.Messrefel = GrpRwerte(index)("V")

    'GetPutReflex.kenn = MenueParam.Messg.Kenn
    GetPutReflex.Iarch = 0
    GetPutReflex.Captext = btnMessVOO(index).Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(index), RezGraphics.Vkwb(index), MenueParam.Messg.MeArtID)
    Me.Enabled = False
    GetPutReflex.ReflexWerte(False)
    Me.Enabled = True

  End Sub

  Private Sub lblMessVor_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMessVor_0.DoubleClick, lblMessVor_1.DoubleClick
    Dim Index As Short
    Index = CInt(sender.name.ToString.Substring(11, 1))
    TypID(Index) = -1
    lblMessVor(Index).Text = ""
    GrpRwerte(Index)("V").Name = ""
    GrpRwerte(Index)("V").IVoNa = False
  End Sub
  Private Sub lblMessNch_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMessNch_0.DoubleClick, lblMessNch_1.DoubleClick
    Dim Index As Short
    Index = CInt(sender.name.ToString.Substring(11, 1))
    SmpID(Index) = -1
    lblMessNch(Index).Text = ""
    GrpRwerte(Index)("N").Name = ""
    GrpRwerte(Index)("N").IVoNa = False
  End Sub

  Private Sub lblMessUnt_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMessUnt_0.DoubleClick, lblMessUnt_1.DoubleClick
    Dim Index As Short
    Index = CInt(sender.name.ToString.Substring(11, 1))
    UntID(Index) = -1
    lblMessUnt(Index).Text = ""
    GrpRwerte(Index).RefUnt.Name = ""
    GrpRwerte(Index).RefUnt.IVoNa = False
  End Sub



  Private Sub Change(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMessVor_0.TextChanged, lblMessVor_1.TextChanged, lblMessNch_0.TextChanged, lblMessNch_1.TextChanged, lblMessUnt_0.TextChanged, lblMessUnt_1.TextChanged, _
                                                                                  txtDickKor_0.TextChanged, txtDickKor_1.TextChanged, txtDickNch_0.TextChanged, txtDickNch_1.TextChanged, txtGlzGrd.TextChanged

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
    If IsNothing(RezptGrid) Then Exit Sub
    If RezptGrid.Count = 0 Then Exit Sub
    RezGraphics.DataChanged = True
    '
    '
    '
    '

    If sender.name = "txtDICKNch_0" Then
      If IsNumeric(sender.text) Then
        RezSozpt.Rezepte(KeyMenge).Dicke(0) = CSng(sender.text)
      End If
    End If
    If sender.name = "txtDICKNch_1" Then
      If IsNumeric(sender.text) Then
        RezSozpt.Rezepte(KeyMenge).Dicke(1) = CSng(sender.text)
      End If
    End If
    If sender.name = "txtDICKKor_0" Then
      If IsNumeric(sender.text) Then
        RezSozpt.Rezepte("KOR").Dicke(0) = CSng(sender.text)
      End If
    End If
    If sender.name = "txtDICKKor_1" Then
      If IsNumeric(sender.text) Then
        RezSozpt.Rezepte("KOR").Dicke(1) = CSng(sender.text)
      End If
    End If
    If sender.name = "txtGlzGrd" Then
      If IsNumeric(sender.text) Then
        RezSozpt.Rezepte("KOR").GlzGrd = CSng(sender.text)
        RezSozpt.Rezepte(KeyMenge).GlzGrd = CSng(sender.text)
      End If
    End If
  End Sub

  Private Sub chkFAR_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkFAR.CheckedChanged
    If chkFAR.Checked Then
      picRezRezept_2.Visible = False
      picRezRezept_3.Visible = False
      picRezFarb_2.Visible = True
      picRezFarb_3.Visible = True
    Else
      picRezRezept_2.Visible = True
      picRezRezept_3.Visible = True
      picRezFarb_2.Visible = False
      picRezFarb_3.Visible = False
    End If
  End Sub
  Private Sub chkRwertAngleich_CheckStateChanged(sender As Object, e As System.EventArgs) Handles chkRwertAngleich.CheckStateChanged
    Call RwertCheck()
  End Sub

  Private Sub hscDIS_Scroll(sender As Object, e As System.Windows.Forms.ScrollEventArgs) Handles hscDIS.Scroll
    Dim ier As Integer
    Dim dick As Single
    Dim Optgesamt As New OpticalData
    If Not hscDIS.Visible Then Exit Sub
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    Cursor = Cursors.WaitCursor
    'Application.DoEvents()
    dick = RezSozpt.Rezepte("KOR").Dicke(0)
    RezSozpt.Rezepte("KOR").Dicke(0) = DiHsc(0) + (CSng(hscDIS.Value) - CSng(hscDIS.Minimum)) * (DiHsc(1) - DiHsc(0)) / (CSng((hscDIS.Maximum - hscDIS.LargeChange + 1)) - CSng(hscDIS.Minimum))
    RezSozpt.Rezepte("KOR").Dicke(1) = RezSozpt.Rezepte("KOR").Dicke(0) * RezSozpt.Rezepte("KOR").Dicke(1) / dick
    txtDIS.Text = Format(RezSozpt.Rezepte("KOR").Dicke(0), "##0.00")
    CalcRezept.MischKorrektur(0, MenueParam.User.Winkel, "ALT", "KOR", RezSozpt, GrpRwerte, Optgesamt, ier)
    RezCheckRad.Picauf.Refresh()
    Cursor = Cursors.Arrow
    Optgesamt.dispose()
  End Sub
  Private Sub hscGGE_Scroll(ByVal sender As Object, ByVal e As System.Windows.Forms.ScrollEventArgs) Handles hscGGE.Scroll
    Dim i As Integer
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    If KeyMenge <> "MEN" Then Exit Sub
    If Not hscGGE.Visible Then Exit Sub
    Cursor = Cursors.WaitCursor
    'Application.DoEvents()
    dbgRefGew.EndEdit()

    If MenueParam.Misch.Igx = 3 Or MenueParam.Misch.Igx = 5 Then Exit Sub
    If MenueParam.Misch.Igx = 6 Then
      MenueParam.Misch.Dto = CSng(txtDTO.Text)
    End If
    MenueParam.Misch.Gge = hscGGE.Value * CSng(txtGGEMax.Text) / (hscGGE.Maximum - hscGGE.LargeChange + 1)
    If MenueParam.Misch.Igx = 7 Then
      For i = 0 To TblRefGew.Rows.Count - 1
        MenueParam.Messg.RefGew.R(i) = TblRefGew.Rows(i)("GEW") * MenueParam.Misch.Gge
      Next
    End If
    CalcRezept.KorrekturRezept(RezGraphics.KFIT + 16 * chkGrundKorr.CheckState, MenueParam.User.Winkel, KeyMenge, "KOR", RezSozpt, GrpRwerte, ier)
    If ier > 0 Then
      Cursor = Cursors.Arrow
      Exit Sub
    End If
    RezCheckRad.Picauf.Refresh()
    RezGrid.TDBFarRezStart(ier)
    RezCheckRad.Picauf.Refresh()
    If MenueParam.Misch.Igx = 6 Then
      e.NewValue = (hscGGE.Maximum - hscGGE.LargeChange + 1) * MenueParam.Misch.Gge / CSng(txtGGEMax.Text)
    End If
    txtGGE.Text = Format(MenueParam.Misch.Gge, "###0.000")
    Application.DoEvents()
    Cursor = Cursors.Arrow
  End Sub
  Private Sub hscZUW_Scroll(sender As Object, e As System.Windows.Forms.ScrollEventArgs) Handles hscZUW.Scroll
    Dim ZuAkt As Single
    Dim ZuMngMax As Single
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    If RezSozpt.Rezepte("MEN").KF = 0 Then Exit Sub
    If Not hscZUW.Visible Then Exit Sub
    If hscZUW.Value < 0 Then Exit Sub
    If Not hscZUW.Enabled Then Exit Sub
    If MenueParam.Misch.Igx <> 3 And MenueParam.Misch.Igx <> 5 Then Exit Sub
    'If hscZUW.Value = HscValue Then Exit Sub
    txtZUWMax.Enabled = False
    Cursor = Cursors.WaitCursor
    ZuMngMax = RezSozpt.MngMax
    If MenueParam.Misch.Igx = 5 Then
      MenueParam.Misch.Dto = CSng(txtDTO.Text)
    End If
    If MenueParam.Misch.Igx = 3 Or MenueParam.Misch.Igx = 5 Then
      ZuAkt = ZuwAkt(Singl(txtZUWMax.Text))
      txtZUW.Text = Format(100 * ZuAkt / RezSozpt.MngMin, "####0.00")
      RezSozpt.MngMax = RezSozpt.MngMin + ZuAkt
    End If
    CalcRezept.KorrekturRezept(RezGraphics.KFIT + 16 * chkGrundKorr.CheckState, MenueParam.User.Winkel, "MEN", "KOR", RezSozpt, GrpRwerte, ier)
    txtZUWMax.Enabled = True
    If ier > 0 Then
      Cursor = Cursors.Arrow
      Exit Sub
    End If
    RezCheckRad.Picauf.Refresh()
    RezGrid.TDBFarRezStart(ier)
    RezCheckRad.Picauf.Refresh()
    ZuAkt = RezGrid.Umr.MngINO("KOR", RezSozpt, iee) - RezSozpt.MngMin
    If MenueParam.Misch.Igx = 5 Then
      e.NewValue = HscVal(ZuAkt, Singl(txtZUWMax.Text))
    End If
    txtZUW.Text = Format(100.0 * ZuAkt / RezSozpt.MngMin, "###0.000")
    Cursor = Cursors.Arrow
  End Sub


  Private Sub txtDisMax_TextChanged(sender As Object, e As System.EventArgs) Handles txtDisMax.TextChanged
    If IsNumeric(txtDisMax.Text) Then
      DiHsc(1) = CSng(txtDisMax.Text)
    End If
  End Sub

  Private Sub btnGRPN_Click(sender As System.Object, e As System.EventArgs) Handles btnGRPN.Click
    Dim StrRez As String
    Dim CmdRezptN As New OleDbCommand("", Cndat)
    If ViewRezept.Count = 0 Then Exit Sub
    If Not HandleRezeptN.MeldSpeiAllRzp(False) Then Exit Sub
    If MessageBox.Show(Texxt(1659) & Space(1) & cboGRPN.Text, Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.No Then
      Exit Sub
    End If
    StrRez = StrLin(ViewRezept, "REZEPT_ID")

    CmdRezptN.CommandText = "UPDATE TBL_REZEPT SET REZEPT_GID=" & cboGRPN.SelectedValue & ",REZEPT_IARCH=" & chkARCN.CheckState & " WHERE MISCH_ID=" & MenueParam.MischID & " AND [REZEPT_ID] in " & StrRez
    '
    If SQLExeNonQuery(CmdRezptN, Cndat) = 0 Then

    End If
    '
    CmdRezptN.Dispose()
  End Sub


  Private Sub chkPic_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkPicNachstellung.CheckedChanged, chkPicKorrektur.CheckedChanged
    Dim GrString() As String
    If IsNothing(RezGrid) Then Exit Sub
    If RezGrid.Name = "KOR" Then
      ReDim GrString(0)
      GrString(0) = "V"
      If chkPicNachstellung.Checked Then
        ReDim Preserve GrString(1)
        GrString(1) = "N"
      End If
      If chkPicKorrektur.Checked Then
        ReDim Preserve GrString(GrString.Count)
        GrString(GrString.Count - 1) = "R"
      End If
      Call GraphicPlotRwert(GrString, RezGrid)
    End If
  End Sub


  Private Sub btnStore_Click(sender As Object, e As System.EventArgs) Handles btnStoreAlt.Click, btnStoreNeu.Click
    Dim Iarch As Integer
    Dim RzNr As String
    Dim NachID(1) As Integer
    If sender.name = "btnStoreAlt" Then
      RzNr = "MEN"
      NachID(0) = SmpID(0)
      NachID(1) = SmpID(1)
    ElseIf sender.name = "btnStoreNeu" Then
      RzNr = "KOR"
      NachID(0) = -1
      NachID(1) = -1
    Else
      Exit Sub
    End If
    If chkARCH.Checked Then
      Iarch = 1
    Else
      Iarch = 0
    End If
    RezGraphics.DataChanged = True
    Call HandleRezept.KorSpei(RzNr, RezSozpt.Rezepte(RzNr).ID, ConnRezept.Current("User_ID"), chkARCH.CheckState, cboGRP.SelectedValue, RwWrRezept, RezGraphics, RezSozpt, UntID, TypID, NachID, ier)
    RezGraphics.DataChanged = False

  End Sub

  Private Sub btnDRU_Click(sender As Object, e As System.EventArgs) Handles btnDRU_2.Click, btnDru_3.Click
    Dim index As Short
    index = CInt(sender.name.Substring(7, 1))
    Call DruckKorrekt(index, Kdru)
  End Sub

  Private Sub cboDRU_DropDownClosed(sender As Object, e As System.EventArgs) Handles cboDRU_2.DropDownClosed, cboDru_3.DropDownClosed
    Dim index As Short
    Dim Kdru As Short
    index = CInt(sender.name.Substring(7, 1))
    Kdru = sender.selectedindex
    Call DruckKorrekt(index, Kdru)
  End Sub

  Private Sub ConnRezept_CurrentChanged(sender As Object, e As System.EventArgs) Handles ConnRezept.CurrentChanged

    Call RefreshCurrent()
  End Sub

  Sub RefreshCurrent()
    Dim i As Integer
    If IsNothing(RezGrid) Then Exit Sub
    If IsNothing(ConnRezept.Current) Then Exit Sub
    If ConnRezept.IsBindingSuspended Then Exit Sub
    '
    RezHLFSelecID.Clear()
    If Not Colorthek Then

      '
      '
      'Rezeptkorrektur
      '
      '
      '
      Application.DoEvents()
      Call HandleRezept.KorSpei(KeyMenge, RezID, ConnRezept.Current("User_ID"), chkARCH.CheckState, cboGRP.SelectedValue, RwWrRezept, RezGraphics, RezSozpt, UntID, TypID, SmpID, ier)

      '
    End If
    chkVOL.CheckState = CheckState.Indeterminate
    '
    '

    For i = 0 To RezGrid.AllRezepte.Rezepte.RezCount - 1
      RezSozpt.Rezepte(i).clear()
    Next i
    '
    'Lösche Hilfskorrekturen
    '
    '
    For i = RezGrid.AllRezepte.Rezepte.RezCount - 1 To 0 Step -1
      If RezSozpt.Rezepte.ContainsKey(KeyName(i)) Then
        RezSozpt.Rezepte.RemoveRez(KeyName(i))
      End If
    Next
    If KeyMenge = "" Then Exit Sub
    If ViewRezept.Count = 0 Then Exit Sub
    '




    '
    'Rezept einlesen
    '
    RezID = ConnRezept.Current("REZEPT_ID")
    Cursor = Cursors.WaitCursor
    RezSozpt.Farben.clear()
    chkGrundKorr.Enabled = True
    '
    '
    Call RwWrRezept.ReadRezeptFarbGrund(KeyMenge, RezID, RezSozpt, UntID, TypID, SmpID, ier)
    lblRezeptName.Text = RezSozpt.Rezepte(KeyMenge).Name
    RezGraphics.DataChanged = False
    '
    '
    '
    Call ClearRwerte(Colorthek, GrpRwerte)

    For i = 0 To 1
      '
      'Untergrund
      '
      '

      GrpRwerte(i).RefUnt.ID = UntID(RezGraphics.Vkwb(i))



      If GrpRwerte(i).RefUnt.ID >= 0 Then
        Call ReWrRwert.ReadRwert(GrpRwerte(i).RefUnt.ID, GrpRwerte(i).RefUnt, ier)
        If ier = 0 Then
          If chkMessUnt(i).Checked Then
            GrpRwerte(i).RefUnt.IVoNa = True
            GrpRwerte(i).RefUnt.Nr = i
          End If
        End If
      End If
      lblMessUnt(i).Text = GrpRwerte(i).RefUnt.Name

      '
      'Vorlage
      '
      '

      If Not Colorthek Then
        GrpRwerte(i)("V").ID = TypID(RezGraphics.Vkwb(i))
        GrpRwerte(i)("V").IVoNa = False
        If GrpRwerte(i)("V").ID >= 0 Then
          Call ReWrRwert.ReadRwert(GrpRwerte(i)("V").ID, GrpRwerte(i)("V"), ier)
          If ier = 0 Then
            If chkMessVor(i).Checked Then
              GrpRwerte(i)("V").IVoNa = True
              GrpRwerte(i)("V").Nr = i
            End If
          End If
        End If
      End If
      If GrpRwerte(i)("V").IVoNa Then
        lblMessVor(i).Text = GrpRwerte(i)("V").Name
        lblVOR(i).Text = GrpRwerte(i)("V").Name
        lblMessVOO(i).Text = GrpRwerte(i)("V").Name
      Else
        lblMessVor(i).Text = ""
      End If
      '
      'Nachstellung
      '
      '

      GrpRwerte(i)("N").ID = SmpID(RezGraphics.Vkwb(i))
      GrpRwerte(i)("N").IVoNa = False
      If GrpRwerte(i)("N").ID >= 0 Then
        Call ReWrRwert.ReadRwert(GrpRwerte(i)("N").ID, GrpRwerte(i)("N"), ier)
        If ier = 0 Then
          If chkMessNch(i).Checked Then
            GrpRwerte(i)("N").IVoNa = True
            GrpRwerte(i)("N").Nr = i
          End If
        End If
      End If
      lblMessNch(i).Text = GrpRwerte(i)("N").Name
      '


    Next i
    '

    chkVOL.CheckState = RezSozpt.IVOL '
    Call VisVisKorrektur()
    RezGrid.TDBFarRezStart(ier)
    If Colorthek Then
      RezDruck.KeyNam = "COL"
      If GrpRwerte(0)("V").ID > -1 Then
        If chkRWRT.Checked Then
          GrpRwerte(0)("V").IVoNa = True
        Else
          GrpRwerte(0)("V").IVoNa = False
        End If
      End If
    Else
      RezDruck.KeyNam = "MEN"
    End If
    If GrpRwerte(0)("V").IVoNa And GrpRwerte(0)("N").IVoNa Then
      Call GraphicPlotRwert(New String() {"V", "N"}, RezptGrid(2))
    ElseIf GrpRwerte(0)("N").IVoNa Then
      Call GraphicPlotRwert(New String() {"N"}, RezptGrid(2))
    ElseIf GrpRwerte(0)("V").IVoNa Then
      Call GraphicPlotRwert(New String() {"V"}, RezptGrid(2))
    Else
      Call GraphicPlotRwert(New String() {}, RezptGrid(2))
    End If
    Call VisGGEZUW(MenueParam.Misch.Igx, KeyMenge)


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
    If cboMng.Items.Count > 0 Then
      cboMng.SelectedIndex = RezSozpt.INO
    End If
    If cboPro_0.Items.Count > 0 Then
      cboPro_0.SelectedIndex = RezSozpt.INP
    End If
    If cboPro_1.Items.Count > 0 Then
      cboPro_1.SelectedIndex = RezSozpt.INQ
    End If
    txtMng(0).Text = HandleRezept.AutFormat(RezSozpt.MngMin)
    txtMng(1).Text = HandleRezept.AutFormat(RezSozpt.MngMax)
    txtPro(0).Text = HandleRezept.AutFormat(100 * RezSozpt.ProzMin)
    txtPro(1).Text = HandleRezept.AutFormat(100 * RezSozpt.ProzMax)
    txtDickKor(0).Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).Dicke(0))
    txtDickKor(1).Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).Dicke(1))
    txtGlzGrd.Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).GlzGrd)
    txtDickNch(0).Text = txtDickKor(0).Text
    txtDickNch(1).Text = txtDickKor(1).Text
    If RezSozpt.Rezepte(0).Iarch < 0 Or RezSozpt.Rezepte(0).Iarch > 2 Then
      chkARCH.CheckState = CheckState.Unchecked
    Else
      chkARCH.CheckState = RezSozpt.Rezepte(KeyMenge).Iarch
    End If
    RezSozpt.Rezepte("KOR").Name = RezSozpt.Rezepte(KeyMenge).Name

    '
    '
    If Colorthek Then
      If ConnRezept.Current("OPTDA") And GrpRwerte(0)("V").IVoNa And GrpRwerte(0)("N").IVoNa Then
        btnKorrektur.Enabled = True
        btnKorrSpezial.Enabled = True
      Else
        btnKorrektur.Enabled = False
        btnKorrSpezial.Enabled = False
      End If
    Else
      btnKorrektur.Enabled = True
      btnKorrSpezial.Enabled = True
    End If
    '
    Cursor = Cursors.Default
    '
    '
    '

    RezGraphics.DataChanged = False

    Application.DoEvents()
    '
    '
    Call chkHLFVorlage_CheckedChanged(chkHLFVorlage, New System.EventArgs)
  End Sub
  Private Sub dbgHLFAvail_CellClick(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dbgHLFAvail.CellClick
    RezHLFSelecID.Add(ViewHlfAvail(e.RowIndex)("REZEPT_ID"))
    ViewHlfSelec.RowFilter = "REZEPT_ID IN " & StrLin(RezHLFSelecID)
  End Sub

  Private Sub dbgHLFSelec_CellDoubleClick(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dbgHLFSelec.CellDoubleClick
    Dim ev As New System.Windows.Forms.KeyPressEventArgs(Chr(9))
    Call dbgHLFSelec_KeyPress(sender, ev)
  End Sub

  Private Sub dbgHLFSelec_KeyPress(sender As Object, e As System.Windows.Forms.KeyPressEventArgs) Handles dbgHLFSelec.KeyPress
    Dim i As Integer
    If e.KeyChar = Chr(9) Then
      '
      '
      '
      'Beforedelete und Afterdelete werden nicht automatisch ausgelöst
      '
      '
      '

      For i = 0 To RezHLFSelecID.Count - 1
        If RezHLFSelecID(i) = ViewHlfSelec(dbgHLFSelec.CurrentRow.Index)("REZEPT_ID") Then
          RezHLFSelecID.RemoveAt(i)
          Exit For
        End If
      Next
      ViewHlfSelec.RowFilter = "REZEPT_ID IN " & StrLin(RezHLFSelecID)
    End If
  End Sub

  Private Sub chkHLFVorlage_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkHLFVorlage.CheckedChanged
    If IsNothing(ConnRezept) Then Exit Sub
    If IsNothing(ConnRezept.Current) Then Exit Sub
    If chkHLFVorlage.Checked And IsNumeric(ConnRezept.Current("RWERT_ID")) Then
      ViewHlfAvail.RowFilter = "RWERT_ID=" & ConnRezept.Current("RWERT_ID") & " AND REZEPT_ID <> " & ConnRezept.Current("REZEPT_ID")
    Else
      ViewHlfAvail.RowFilter = "REZEPT_ID <> " & ConnRezept.Current("REZEPT_ID")
    End If
    ViewHlfAvail.RowFilter = ViewHlfAvail.RowFilter & " AND REZEPT_ID IN " & StrLin(RezHlfAvailID)
    RezHLFSelecID.Clear()
    ViewHlfSelec.RowFilter = "REZEPT_ID IN " & StrLin(RezHLFSelecID)
  End Sub
  Private Sub chkRefGew_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkRefGew.CheckedChanged
    If chkRefGew.Checked And MenueParam.Misch.Igx = 7 Then
      picFarbXYZ_3.Visible = False
      dbgRefGew.Visible = True
    Else
      picFarbXYZ_3.Visible = True
      dbgRefGew.Visible = False
    End If
  End Sub

  Private Sub chkRZAen_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkRZAen.CheckedChanged
    If chkRZAen.Checked Then
      RezGrid.NewKeynam("KOO")
    Else
      RezGrid.NewKeynam("KOR")
    End If
  End Sub

  Private Sub cboGRP_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboGRP.SelectedValueChanged
    If Not IsNumeric(cboGRP.SelectedValue) Then Exit Sub
    If Not cboGRP.Enabled Then Exit Sub
    If Colorthek Then
      'btnSUC_0.PerformClick()
    Else
      btnSUC_1.PerformClick()
    End If
  End Sub

  Private Sub dbgREZ_DoubleClick(sender As Object, e As System.EventArgs) Handles dbgREZ_1.DoubleClick, dbgREZ_2.DoubleClick
    '
    '
    ' Doubleclick Entf(ASCII = 46)
    '
    '
    Dim DiaRes As DialogResult
    If ConnRezept.Count = 0 Then Exit Sub
    If sender.SelectedRows.Count = 0 Then Exit Sub
    If BitWrt(14, MenueParam.User.Writ) Then
      DiaRes = MessageBox.Show(Texxt(3030) & Space(1) & ConnRezept.Current("REZEPT_NAME") & Space(1) & Texxt(3031), Texxt(2000), MessageBoxButtons.YesNo)
      If DiaRes = Forms.DialogResult.No Then
        Exit Sub
      Else
        RwWrRezept.DelRezept(ConnRezept.Current("REZEPT_ID"), cboGRP.SelectedValue, ier)
        If ier = 0 Then
          ConnRezept.Current.delete()
        End If
      End If
    End If

  End Sub


  Private Sub dbgREZ_KeyDown(sender As Object, e As System.Windows.Forms.KeyEventArgs) Handles dbgREZ_1.KeyDown, dbgREZ_2.KeyDown
    If e.KeyCode = Keys.Delete Then
      dbgREZ_DoubleClick(sender, e)
    End If
  End Sub
  Private Sub dbgRefGew_CellValidating(sender As Object, e As System.Windows.Forms.DataGridViewCellValidatingEventArgs) Handles dbgRefGew.CellValidating
    If e.ColumnIndex = 1 And Not IsNumeric(e.FormattedValue) Then
      e.Cancel = True
      MsgBox(Texxt(16))
    End If
  End Sub
  Private Sub txt_Enter(sender As Object, e As System.EventArgs) Handles txtGGE.Enter, txtGGEMax.Enter, txtKDE.Enter, txtDIS.Enter, txtKDS.Enter, txtDisMax.Enter, txtDickKor_0.Enter, txtDickKor_1.Enter, txtDickNch_0.Enter, txtDickNch_1.Enter, txtDisMax.Enter, txtDTO.Enter, txtZUW.Enter, txtZUWMax.Enter, txtGewDC.Enter, txtGewDH.Enter, txtGewDL.Enter
    sender.tag = sender.text
  End Sub
  Private Sub txt_TextChanged(sender As Object, e As System.EventArgs) Handles txtGGE.TextChanged, txtGGEMax.TextChanged, txtKDE.TextChanged, txtDIS.TextChanged, txtKDS.TextChanged, txtDisMax.TextChanged, txtDickKor_0.TextChanged, txtDickKor_1.TextChanged, txtDickNch_0.TextChanged, txtDickNch_1.TextChanged, txtDisMax.TextChanged, txtDTO.TextChanged, txtZUW.TextChanged, txtZUWMax.TextChanged, txtGewDC.TextChanged, txtGewDH.TextChanged, txtGewDL.TextChanged, txtGlzGrd.TextChanged
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
     txtDickNch_0.Validating, txtDickNch_1.Validating, txtDickKor_0.Validating, txtDickKor_1.Validating, txtSum_1.Validating, txtSUM_2.Validating, txtSUM_3.Validating, txtGlzGrd.Validating, _
     txtDIS.Validating, txtDisMax.Validating, txtDTO.Validating, txtGewDC.Validating, txtGewDH.Validating, txtGewDL.Validating, txtGGE.Validating, txtGGEMax.Validating, txtKDE.Validating, txtKDS.Validating, _
    txtZUW.Validating, txtZUWMax.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub

  Private Sub txtDat_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtBIS_0.Validating, txtVON_0.Validating, txtBIS_1.Validating, txtVON_1.Validating
    If Not IsDate(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(2500))
    End If
  End Sub

  Private Sub btnKorrSpezial_Click(sender As System.Object, e As System.EventArgs) Handles btnKorrSpezial.Click
    Dim irest As Integer
    Dim i As Integer
    Dim INO As Integer
    '
    '
    'keine Hilfskorrekturen
    '
    '
    RezHLFSelecID.Clear()
    ViewHlfSelec.RowFilter = "REZEPT_ID IN " & StrLin(RezHLFSelecID)
    'irest = 1
    'If radRestFarb(1).Checked Then
    ' irest = 1
    ' End If
    'Iprn = RezGraphics.KFIT + 4 * irest
    Iprn = 1
    RestFarb = New Colorant
    INO = RezSozpt.INO
    RezSozpt.INO = 1
    RezSozpt.MngMin = Umr.MngINO(KeyMenge, RezSozpt, ier)
    RezSozpt.MngMax = RezSozpt.MngMin

    Call CalcRezept.Restfarbe(Iprn, KeyMenge, KeyMenge, RezSozpt, GrpRwrtRest, RestFarb, ier)
    If ier <> 0 Then
      Exit Sub
    End If

    RestFarb.Name = InputBox(Texxt(815), Texxt(853), "@" & GrpRwrtRest(0)("V").Name)
    If Trim(RestFarb.Name) <> "" Then
      RestFarb.BoMng = 100.0
      RestFarb.OP = "="



      RwWrFarbe = New ReadWriteFarbe
      RwWrGrund = New ReadWriteGrund
      '
      '
      'Speicher Restfarbe
      '
      '
      '
      RestFarb.GlzGrdID = -1
      RestFarb.GlzGrd = 0.0
      RwWrFarbe.FarAdd(RestFarb, ier)
      '
      'Speichern Grunddaten der Restfarbe
      '
      '
      '
      RwWrGrund.WriteGrund(RestFarb.ID, RestFarb.OptData, MenueParam.Messg.Winkel, ier)



      DatenFarb.AddNewFarb(RestFarb)
      RwWrFarbe.dispose()
      RwWrGrund.dispose()
      RezSozpt.Farben.AddFarb(KeyName(RestFarb.ID), RestFarb)

    Else
      Exit Sub
    End If
    For i = 0 To RezSozpt.Rezepte(KeyMenge).KF - 1
      RezSozpt.Rezepte(KeyMenge)(i).FaAmng = 0.0
    Next
    RezSozpt.Rezepte(KeyMenge).AddFaNr(KeyRe(RezSozpt.Rezepte(KeyMenge).KF), New ColorAmount)
    RezSozpt.Rezepte(KeyMenge)(KeyRe(RezSozpt.Rezepte(KeyMenge).KF - 1)).ID = RestFarb.ID
    RezSozpt.Rezepte(KeyMenge)(KeyRe(RezSozpt.Rezepte(KeyMenge).KF - 1)).FaAmng = 100.0
    Umr.CalcBamng(KeyMenge, RezSozpt, ier)
    RezGraphics.DataChanged = True
    RezSozpt.INO = INO
    RezSozpt.MngMin = 100.0
    RezSozpt.MngMax = 1000.0
    txtMNG_0.Text = RezSozpt.MngMin
    txtMNG_1.Text = RezSozpt.MngMax
    RezGrid.TDBFarRezStart(ier)
    Application.DoEvents()
    btnKorrektur.PerformClick()
  End Sub
  Private Sub cboMini_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboMini.SelectedIndexChanged
    MenueParam.Misch.Igx = cboMini.SelectedIndex

    Call VisGGEZUW(MenueParam.Misch.Igx, KeyMenge)
  End Sub

  Private Sub chkWithTempl_Click(sender As Object, e As System.EventArgs) Handles chkWithTempl.Click
    chkHLFVorlage.Checked = chkWithTempl.Checked
  End Sub

  Private Sub chkAltName_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkAltName.CheckedChanged
    Dim i As Integer
    If IsNothing(DatenFarb) Then Exit Sub
    'MsgBox(TDBFar_0.Columns(0).CellValue(0))
    'MsgBox(TDBFar_0.Columns(0).CellText(0))
    'MsgBox(TDBFar_0.Columns(0).Text)
    'MsgBox(TDBFar_0.Columns(0).Value)

    DatenFarb.DsetFarbFill(1, chkAltName.Checked)
    For i = 0 To RezptGrid.Count - 1
      If Not IsNothing(RezptGrid(i)) AndAlso Not IsNothing(RezptGrid(i).TDBFar) Then
        RezptGrid(i).TDBFarGridRezept(ier)
        RezptGrid(i).NewKeynam(RezptGrid(i).Name)
        If RezptGrid(i).AllRezepte.Rezepte.ContainsKey(RezptGrid(i).Name) Then
          RezptGrid(i).TDBFarRezStart(ier)
        End If
      End If
    Next
  End Sub

  Private Sub btnSPRwrt_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSPRwrt.Click
    Call SpeiRwertCalc(ReWrRwert, GrpRwerte, cboGRPRwrt.SelectedValue, ier)
  End Sub
  
End Class