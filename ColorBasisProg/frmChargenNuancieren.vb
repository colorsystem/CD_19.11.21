Option Explicit On
Option Compare Text
Option Strict Off
Public Class frmChargenNuancieren

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
  Dim OptGesamt As OpticalData
  '
  Dim Umr As RezeptUmrechnung
  Dim RezSozpt As RecipesGrp
  Dim RezChaBez As RecipesGrp

  Dim GrpRwerte As RefValuesGrp
  Dim GrpRwertChaBez As RefValuesGrp
  Dim ChargenFarb As Colorant
  Dim BezugsFarb As Colorant
  Dim RwWrRezept As ReadWriteRezept
  Dim ReWrRwert As ReadWriteRwert
  Dim ReWrFarb As ReadWriteFarbe
  Dim ReWrGrund As ReadWriteGrund

  Dim FarbWrt As ValuesGrps
  Dim WinHilf As AngGeos
  Dim CalcRezept As RezeptBerechnung
  Dim PicAufbau As List(Of HandlePictures)
  Dim Picauf As HandlePictures
  Dim claDRU As HandlePlottDruck
  Dim quali As QualKontrolle
  Dim HandleRezept As HandleRezGeneral
  Dim HandleRwrt As HandleRwerte

  '
  Dim KeyD As String
  Dim FaId As Integer
  Dim RezID As Integer
  Dim FaIDFarb As Integer
  Dim FaIDWeiss As Integer
  Dim FaIDBinde As Integer
  Dim FaIDSchwarz As Integer
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
  Dim Igx As Integer
  Dim HscMax As Integer
  Dim InoMeng As Single
  Dim kwb As Integer
  Dim DatenFarb As HandleRezDsetFarb
  Dim DsetRezpt As New DataSet
  Dim OleAdRezpt As New OleDbDataAdapter
  Dim DatReadRezpt As OleDbDataReader
  Dim CmdRezpt As New OleDbCommand
  Dim ViewRezept As DataView
  Dim WithEvents ConnRezept As BindingSource
  Dim TblRezept As DataTable
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

  Dim SplitContKorrRezepte As New List(Of SplitContainer)
  Dim RezeptGrid As RecipesGrp
  Dim RefWerteUNT As List(Of RefValue)
  Dim RefWertetyp As List(Of RefValue)
  Dim RefWertesmp As List(Of RefValue)
  Dim cbomim As New List(Of ComboBox)
  Dim txtMng As New List(Of TextBox)
  Dim txtPro As New List(Of TextBox)
  Dim txtSum As New List(Of TextBox)
  Dim lblmng As New List(Of Label)
  Dim cboDRU As New List(Of ComboBox)
  Dim btnDRU As New List(Of Button)
  Dim radRwerte As New List(Of RadioButton)
  Dim lblSchichtdicke As New List(Of Label)
  Dim txtSchichtdicke As New List(Of TextBox)
  Dim txtFarbmenge As New List(Of TextBox)
  Dim txtWeissmenge As New List(Of TextBox)
  Dim txtBindemenge As New List(Of TextBox)
  Dim txtSchwarzmenge As New List(Of TextBox)
  Dim txtFarbproz As New List(Of TextBox)
  Dim txtWeissproz As New List(Of TextBox)
  Dim txtBindeproz As New List(Of TextBox)
  Dim txtSchwarzproz As New List(Of TextBox)
  Dim btnMess As New List(Of Button)
  Dim lblMess As New List(Of Label)
  Dim radSelectRwert As New List(Of RadioButton)
  Dim chkWeissBindeSchwarz As New List(Of CheckBox)
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
  '
  '
  '

  Public Sub New()

    ' Dieser Aufruf ist für den Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
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
    dbgrez.Columns(0).Caption = ""
    dbgrez.Columns(1).Caption = Texxt(2205)
    dbgrez.Columns(2).Caption = Texxt(2206)
    dbgrez.Columns(3).Caption = Texxt(2207)
    dbgrez.Columns(4).Caption = ""
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
    dbgrez.Columns(0).HeaderText = ""
    dbgrez.Columns(1).HeaderText = Texxt(2205)
    dbgrez.Columns(2).HeaderText = Texxt(2206)
    dbgrez.Columns(3).HeaderText = Texxt(2207)
    dbgrez.Columns(4).HeaderText = ""
    ' dbgrez.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
    'dbgrez.HoldFields()
    '
    '
    '
    '
    '
    '

  End Sub



  Sub SetRwerte(ByRef GrpRwerte As RefValuesGrp)
    Dim i As Integer
    Dim j As Integer
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
      'Vorlage (Rechnung)
      '
      '

      GrpRwerte(i)("V").IVoNa = False
      GrpRwerte(i)("V").Iplott = False
      GrpRwerte(i)("V").Name = ""
      GrpRwerte(i)("V").Bem = ""
      GrpRwerte(i)("V").Nr = -1
      GrpRwerte(i)("V").ID = -1
      GrpRwerte(i)("V").Name = Texxt(806 + i)
      Call ADDCurves(GrpRwerte(i)("V").RefKurv)
      'GrpRwerte(i)("V").RefKurv.clear()
      ' For j = 0 To MenueParam.User.Winkel.Km - 1
      ' GrpRwerte(i)("V").RefKurv.Add(MenueParam.User.Winkel(j).Chrm, New CurveRef(MenueParam.User.Winkel.Wsol.Nwe))
      'Next

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
      GrpRwerte(i)("N").Name = Texxt(828 + i)
      Call ADDCurves(GrpRwerte(i)("N").RefKurv)
      'GrpRwerte(i)("N").RefKurv.clear()
      ' For j = 0 To MenueParam.User.Winkel.Km - 1
      ' GrpRwerte(i)("N").RefKurv.Add(MenueParam.User.Winkel(j).Chrm, New CurveRef(MenueParam.User.Winkel.Wsol.Nwe))
      'Next

      '
      'Nachstellung (Zwischenspeicher)
      '
      '
      GrpRwerte(i)("M").IVoNa = False
      GrpRwerte(i)("M").Iplott = False
      GrpRwerte(i)("M").Name = ""
      GrpRwerte(i)("M").Bem = ""
      GrpRwerte(i)("M").Nr = -1
      GrpRwerte(i)("M").ID = -1
      Call ADDCurves(GrpRwerte(i)("M").RefKurv)
      ' GrpRwerte(i)("M").RefKurv.clear()
      ' For j = 0 To MenueParam.User.Winkel.Km - 1
      ' GrpRwerte(i)("M").RefKurv.Add(MenueParam.User.Winkel(j).Chrm, New CurveRef(MenueParam.User.Winkel.Wsol.Nwe))
      'Next

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
    '
    '
    'Charge
    '
    '
    GrpRwerte(0)("C").IVoNa = False
    GrpRwerte(0)("C").Iplott = False
    GrpRwerte(0)("C").Itp = True
    GrpRwerte(0)("C").Name = ""
    GrpRwerte(0)("C").ID = -1
    GrpRwerte(0)("C").Nr = -1
    '
    '
    'Bezug
    '
    '
    GrpRwerte(0)("B").IVoNa = False
    GrpRwerte(0)("B").Iplott = False
    GrpRwerte(0)("B").Name = ""
    GrpRwerte(0)("B").ID = -1
    GrpRwerte(0)("B").Nr = -1
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
      '
      RezCheckRad.chkUUU(0).Visible = True
      RezCheckRad.radUUU(0).Visible = True
    End If
    If Not MenueParam.Misch.Transp Then
      RezCheckRad.chkUUU(0).Visible = False
      RezCheckRad.radUUU(0).Visible = False
      ChkUNT(0).Visible = False
      ChkUNT(1).CheckState = 0
      ChkUNT(1).Visible = False
      '
      lblDicke.Visible = False
      txtDicke.Visible = False
      For i = 0 To 1
        lblSchichtdicke(i).Visible = False
        txtSchichtdicke(i).Visible = False
      Next i
      '
    Else
      RezCheckRad.chkUUU(0).Visible = True
      RezCheckRad.radUUU(0).Visible = True
      ChkUNT(0).Visible = True
      ChkUNT(1).Visible = False
      '
      '
      '
      lblDicke.Visible = True
      txtDicke.Visible = True
      For i = 0 To 1
        lblSchichtdicke(i).Visible = True
        txtSchichtdicke(i).Visible = True
      Next i

    End If
    '
    RezCheckRad.chkUUU(1).Visible = False
    RezCheckRad.chkUUU(1).Checked = False
    RezCheckRad.radUUU(1).Visible = False
    RezCheckRad.radUUU(1).checked = False
    If ChkUNT(1).Checked Then
      '
      RezCheckRad.chkUUU(1).Visible = True
      RezCheckRad.radUUU(1).Visible = True
    Else
      RezCheckRad.chkUUU(0).Checked = True
      RezCheckRad.radUUU(0).checked = True
      RezCheckRad.radUUU(0).Visible = False
      RezCheckRad.radUUU(1).Visible = False
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
    If Not BitWrt(22, MenueParam.User.Visbl) Then
      txtDicke.Visible = False
      lblDicke.Visible = False
      For i = 0 To txtSchichtdicke.Count - 1
        txtSchichtdicke(i).Visible = False
        lblSchichtdicke(i).Visible = False
      Next i
    End If
    If Not BitWrt(22, MenueParam.User.Enabl) Then
      txtDicke.Enabled = False
      lblDicke.Enabled = False
      For i = 0 To txtSchichtdicke.Count - 1
        txtSchichtdicke(i).Enabled = False
        lblSchichtdicke(i).Enabled = False
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
    If IsNothing(RezGrid.Picauf) Then Exit Sub
    RezGraphics.FawrtRwerte(0).clear()
    RezGraphics.FawrtRwerte(1).clear()
    RezGraphics.PlotRwerte.clear()
    ' Boo = HandleRezept.IVorNa("Z", GrpRwerte, ChkUNT, chkKDE, chkNAS)

    j = -1
    For i = 0 To Refkenn.Count - 1
      'Boo = HandleRezept.IVorNa(Refkenn(i), GrpRwerte, ChkUNT, chkKDE, chkNAS)
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
    StrName = InputBox(Texxt(2104), Texxt(2000), RezSozpt.Rezepte(RzNr).Name)
    If StrName <> "" Then
      RezSozpt.Rezepte(RzNr).Name = StrName
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
      If RezSozpt.Rezepte("KOA").KF > 0 Then
        i = i + 1
        ReDim Preserve RezDruck.RzName(i)
        RezDruck.RzName(i) = "KOA"
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

      AufbauPar.AufbauRezeptMerk(FarbWrt, ier)

      Call quali.FarbWrtCalc(MenueParam.Messg.Winkel, GrpRwerte, FarbWrt, iee)
      ier = iee
      '
      RzUm = ""
      If Index = 2 Then
        RzUm = "MEN"
      ElseIf Index = 3 Then
        RzUm = "KOA"
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
  Private Sub frmChargenNuancieren_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim j As Integer
    parform = Me

    '
    'Texte und Änderungen für Form
    '
    '
    'Call AendMDChild()

    Me.Text = Texxt(1856) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    btnRezept.Visible = True
    btnRezept.Visible = True
    btnAustausch.Enabled = False
    btnRezept.Text = Texxt(730)
    btnKorrektur.Text = Texxt(731)
    btnAustausch.Text = Texxt(3803)
    btnSUC.Text = Texxt(835)
    btnStoreAlt.Text = Texxt(1995)
    btnStoreNeu.Text = Texxt(854)
    'btnMessVOO_0.Text = Texxt(806)
    'btnMessVOO_1.Text = Texxt(807)

    btnMess_0.Text = Texxt(3800)
    btnMess_1.Text = Texxt(3801)
    lblFarb_0.Text = Texxt(3800)
    lblFarb_1.Text = Texxt(3801)
    radSelectRwert_0.Text = Texxt(3805)
    radSelectRwert_1.Text = Texxt(3806)
    radSelectRwert_2.Text = Texxt(3807)
    radSelectRwert_3.Text = Texxt(3808)
    '
    chkWeissPigment.Text = Texxt(3810)
    chkBindemittel.Text = Texxt(3811)
    chkSchwarzPigment.Text = Texxt(3812)

    btnMessUnt.Text = Texxt(808)
    btnMessNch.Text = Texxt(828)
    chkRwert.Text = Texxt(3110)
    'lblFAR.Text = Texxt(738)
    'lblFAQ.Text = Texxt(802)
    lblBIS.Text = Texxt(377)
    lblMNG_0.Text = Texxt(813)
    lblMNG_1.Text = Texxt(814)
    '
    '
    'chkHLFVorlage.Text = Texxt(728)
    chkUUU_0.Text = Texxt(804)
    chkUUU_1.Text = Texxt(805)
    chkDatum.Text = Texxt(140)
    chkDatum.Text = Texxt(140)
    chkUMR.Text = Texxt(445)
    'lblSUC_0.Text = Texxt(4606)
    lblSUC.Text = Texxt(4606)
    chkRwertAngleich.Text = Texxt(4708)
    lblRestGew.Text = Texxt(172)

    chkRZAen.Text = Texxt(873)
    '
    dbgREZ_0.Caption = Texxt(837)
    '
    '
    lblRezName.Text = Texxt(931)

    'lblARB
    '
    Me.lblARB.Text = Texxt(2002)
    '
    'chkFAR

    Me.chkFAR.Text = Texxt(857)



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
    '
    '
    'chkUNT_1
    '

    Me.chkUNT_1.Text = Texxt(356)
    '
    'chkUNT_0
    '

    Me.chkUNT_0.Text = Texxt(355)
    '
    '
    '
    '
    lblRwert.Text = Texxt(4685)
    lblSchichtdicke_0.Text = Texxt(2203)
    lblSchichtdicke_1.Text = Texxt(2203)
    lblFarbMenge_0.Text = Texxt(4630)
    lblFarbMenge_1.Text = Texxt(4630)
    lblFarbProz_0.Text = Texxt(3802)
    lblFarbProz_1.Text = Texxt(3802)
    lblWeissMenge_0.Text = Texxt(4630)
    lblWeissMenge_1.Text = Texxt(4630)
    lblWeissProz_0.Text = Texxt(3802)
    lblWeissProz_1.Text = Texxt(3802)
    lblBindeMenge_0.Text = Texxt(4630)
    lblBindeMenge_1.Text = Texxt(4630)
    lblBindeProz_0.Text = Texxt(3802)
    lblBindeProz_1.Text = Texxt(3802)
    lblSchwarzMenge_0.Text = Texxt(4630)
    lblSchwarzMenge_1.Text = Texxt(4630)
    lblSchwarzProz_0.Text = Texxt(3802)
    lblSchwarzProz_1.Text = Texxt(3802)

    lblDicke.Text = Texxt(2203)
    '



    OptGesamt = New OpticalData

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
    RezChaBez = New RecipesGrp
    GrpRwerte = New RefValuesGrp
    GrpRwertChaBez = New RefValuesGrp
    ChargenFarb = New Colorant
    BezugsFarb = New Colorant
    FarbWrt = New ValuesGrps
    RwWrRezept = New ReadWriteRezept
    ReWrRwert = New ReadWriteRwert
    ReWrFarb = New ReadWriteFarbe
    ReWrGrund = New ReadWriteGrund
    CalcRezept = New RezeptBerechnung
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
    RezDruck.Winkel = MenueParam.Messg.Winkel
    '
    '
    'DummyID
    '
    '
    PicAufbau = New List(Of HandlePictures)
    PicAufbau.Add(New HandlePictures)
    PicAufbau.Add(New HandlePictures)
    PicAufbau(0).Add("REF", CType(picRwert_0, PictureBox))
    PicAufbau(0).Add("XYZ", CType(picFarbXYZ_0, PictureBox))
    PicAufbau(0).PicGraphic = RezGraphics
    '
    PicAufbau(1).Add("REF", CType(picRwert_1, PictureBox))
    PicAufbau(1).Add("LAB", CType(picFarbLAB_1, PictureBox))
    PicAufbau(1).Add("XYZ", CType(picFarbXYZ_1, PictureBox))
    PicAufbau(1).Add("FRB", CType(picRezFarb_1, PictureBox))
    PicAufbau(1).Add("REZ", CType(picRezRezept_1, PictureBox))
    PicAufbau(1).PicGraphic = RezGraphics


    RezptGrid = New List(Of HandleRezC1Screen)
    RezptGrid.Add(New HandleRezC1Screen)
    RezptGrid.Add(New HandleRezC1Screen)
    txtSum.Add(txtSum_0)
    txtSum.Add(txtSUM_1)

    '
    '
    '
    RezptGrid(0).TDBFar = TDBFar_0
    RezptGrid(0).TDBDropFar = TDBDropFar_0
    RezptGrid(0).TDBDropPre = TDBDropPre_0
    RezptGrid(0).TDBDropPro = TDBDropPro_0
    RezptGrid(0).TDBDropPrb = TDBDropPrb_0
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
    DatenFarb.DsetFarbCreate()

    For i = 0 To 1
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

    RezptGrid(0).cboMNG = cboMng
    RezptGrid(0).cboPRO_0 = cboPro_0
    RezptGrid(0).cboPRO_1 = cboPro_1
    For i = 0 To 1
      RezptGrid(i).txtMNG_0 = txtMNG_0
      RezptGrid(i).txtMNG_1 = txtMNG_1
      RezptGrid(i).txtPRO_0 = txtPRO_0
      RezptGrid(i).txtPRO_1 = txtPRO_1
    Next i
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
    HandleRezept = New HandleRezGeneral
    RezeptGrid = New RecipesGrp
    RezeptGrid.Rezepte.kwb(0) = 0
    RezeptGrid.Rezepte.kwb(1) = 1

    HandleRwrt = New HandleRwerte
    RefWerteUNT = New List(Of RefValue)
    RefWertetyp = New List(Of RefValue)
    RefWertesmp = New List(Of RefValue)
    HandleRezept.lblGRP = lblGRP
    HandleRezept.cboGRP = cboGRP

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
    '
    '
    '
    If Not BitWrt(7, MenueParam.User.Sonst) Then
      RezptGrid(1).TDBFar.Top = RezptGrid(1).TDBFar.Top + chkRZAen.Height
      RezptGrid(1).TDBFar.Height = RezptGrid(1).TDBFar.Height + chkRZAen.Height
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
    cboDRU.Add(cboDru_1)
    cboDRU.Add(cboDru_1)
    btnDRU.Add(Nothing)
    btnDRU.Add(btnDru_1)
    btnDRU.Add(btnDru_1)
    For i = 1 To 2
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
    btnDru_1.Text = cboDru_1.Items(Kdru)
    btnDru_1.Text = cboDru_1.Items(Kdru)


    '
    If PrinterSettings.InstalledPrinters.Count = 0 Then
      For i = 1 To 2
        cboDRU(i).Visible = False
        btnDRU(i).Visible = False
      Next i
    End If

    '
    '
    '
    '
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
    cboGLZ.Text = MenueParam.Messg.Winkel(0).GK(0)
    '
    '
    '
    '
    '
    lblSchichtdicke.Add(lblSchichtdicke_0)
    lblSchichtdicke.Add(lblSchichtdicke_1)
    txtSchichtdicke.Add(txtSchichtdicke_0)
    txtSchichtdicke.Add(txtSchichtdicke_1)
    txtFarbmenge.Add(txtFarbMenge_0)
    txtFarbmenge.Add(txtFarbMenge_1)
    txtWeissmenge.Add(txtWeissMenge_0)
    txtWeissmenge.Add(txtWeissMenge_1)
    txtBindemenge.Add(txtBindeMenge_0)
    txtBindemenge.Add(txtBindeMenge_1)
    txtSchwarzmenge.Add(txtSchwarzMenge_0)
    txtSchwarzmenge.Add(txtSchwarzMenge_1)

    '
    '
    txtFarbproz.Add(txtFarbProz_0)
    txtFarbproz.Add(txtFarbProz_1)
    txtWeissproz.Add(txtWeissProz_0)
    txtWeissproz.Add(txtWeissProz_1)
    txtBindeproz.Add(txtBindeProz_0)
    txtBindeproz.Add(txtBindeProz_1)
    txtSchwarzproz.Add(txtSchwarzProz_0)
    txtSchwarzproz.Add(txtSchwarzProz_1)

    '
    btnMess.Add(btnMess_0)
    btnMess.Add(btnMess_1)
    lblMess.Add(lblMess_0)
    lblMess.Add(lblMess_1)
    '
    '
    radSelectRwert.Add(radSelectRwert_0)
    radSelectRwert.Add(radSelectRwert_1)
    radSelectRwert.Add(radSelectRwert_2)
    radSelectRwert.Add(radSelectRwert_3)

    '
    chkWeissBindeSchwarz.Add(chkWeissPigment)
    chkWeissBindeSchwarz.Add(chkBindemittel)
    chkWeissBindeSchwarz.Add(chkSchwarzPigment)
    '
    '
    ChkUNT.Add(chkUNT_0)
    ChkUNT.Add(chkUNT_1)
    txtMng.Add(txtMNG_0)
    txtMng.Add(txtMNG_1)
    txtPro.Add(txtPRO_0)
    txtPro.Add(txtPRO_1)
    lblmng.Add(lblMNG_0)
    lblmng.Add(lblMNG_1)

    '
    SplitContKorrRezepte.Add(SplitContEingabe)
    SplitContKorrRezepte.Add(SplitContKorrektur)

    '

    '
    '
    '
    '
    '
    '
    '
    RezCheckRad.PicGraphic = RezGraphics
    'RezCheckRad.cboWIN = cboWin
    RezCheckRad.cboSKAL = cboSkal
    RezGraphics.cboSKAL = cboSkal
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
    txtRestGew.Text = MenueParam.Menue.Del

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

    '
    'mit Readonly Werten, falls Korrekturprogramm
    '
    '
    TblRezept.Columns.Add("DEMOD", GetType(Single), -1.0)
    TblRezept.Columns.Add("OPTDA", GetType(Boolean), True)
    TblRezept.Columns.Add("RWERT_ID", GetType(Integer))

    '
    '
    ViewRezept = New DataView(TblRezept)
    ConnRezept = New BindingSource
    ConnRezept.DataSource = ViewRezept



    dbgREZ = New List(Of C1TrueDBGrid)
    dbgREZ.Add(dbgREZ_0)
    dbgREZ(0).SetDataBinding(ConnRezept, "", False)
    Call DbgRezCustomize(dbgREZ(0))
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
  End Sub

  Private Sub frmChargenNuancieren_Shown(sender As Object, e As System.EventArgs) Handles Me.Shown

    '
    '
    'Korrekturberechnung
    '
    '
    '
    Call RezActiv(0)
    RezGrid = RezptGrid(0)
    '
    SplitVisible = 0
    '


    '
    '
    '
    Application.DoEvents()
    If MenueParam.MischID = -1 Then
      MsgBox(Texxt(3601))
      SplitVisible = 0
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

  Private Sub frmChargenNuancieren_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
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
    cboGLZ.Text = MenueParam.Messg.Winkel(0).GK(0)
    '
    '
    '
    TblRezept.Rows.Clear()
    For i = 0 To RezptGrid.Count - 1
      RezptGrid(i).TDBFarGridRezept(ier)
    Next i
    '
    '
    '
    Igx = MenueParam.Misch.Igx
    '
    '
    '
    '
    '
    'Art der Rezeptberechnung
    '
    '
    '
    lblMINI.Visible = BitWrt(14, MenueParam.User.Visbl)
    lblMINI.Text = Texxt(572 + MenueParam.Misch.Igx) & " (" & Texxt(306 + MenueParam.Menue.JABST) & ") "
    '
    '
    '
    '
    '
    DatenFarb.DsetFarbFill(1, False)

    RezptGrid(0).NewKeynam("MEN")
    RezptGrid(1).NewKeynam("KOA")

    '
    '

    '
    '
    '
    KeyMenge = "MEN"
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
    Next j
    For j = 0 To 1
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
    RezSozpt.Rezepte.AddRez("KOA", New Recipe)
    RezSozpt.Rezepte.AddRez("NEU", RezSozpt.Rezepte("MEN"))
    RezSozpt.Rezepte.AddRez("ALT", RezSozpt.Rezepte("MEN"))
    RezSozpt.Rezepte.AddRez("KOO", RezSozpt.Rezepte("KOA"))
    RezChaBez.Rezepte.AddRez("CHA", New Recipe)
    RezChaBez.Rezepte.AddRez("BEZ", New Recipe)

    '
    '
    '
    GrpRwerte.clear()
    GrpRwertChaBez.clear()
    '
    GrpRwerte.Add("W", New RefValues)
    GrpRwerte.Add("S", New RefValues)
    GrpRwertChaBez.Add("W", New RefValues)
    GrpRwertChaBez.Add("S", New RefValues)
    GrpRwerte(0).Add("V", New RefValue)
    GrpRwerte(0).Add("N", New RefValue)
    GrpRwerte(0).Add("R", New RefValue)
    GrpRwerte(0).Add("M", New RefValue)
    GrpRwerte(1).Add("V", New RefValue)
    GrpRwerte(1).Add("N", New RefValue)
    GrpRwerte(1).Add("R", New RefValue)
    GrpRwerte(1).Add("M", New RefValue)
    GrpRwerte(0).Add("C", New RefValue)
    GrpRwerte(0).Add("B", New RefValue)

    GrpRwerte(0)("C").QuControl = New QuControls
    GrpRwerte(0)("B").QuControl = New QuControls

    GrpRwerte(0).RefUnt = New RefValue
    GrpRwerte(1).RefUnt = New RefValue

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
    GrpRwerte(0)("M").kwb = 0
    GrpRwerte(1)("M").kwb = 1
    GrpRwerte(0)("C").kwb = 0
    GrpRwerte(0)("B").kwb = 0
    RezGraphics.Vkwb(0) = 0
    RezGraphics.Vkwb(1) = 1
    GrpRwerte(0)("V").Itp = True
    GrpRwerte(1)("V").Itp = True
    GrpRwerte(0)("N").Itp = False
    GrpRwerte(1)("N").Itp = False
    GrpRwerte(0)("R").Itp = False
    GrpRwerte(1)("R").Itp = False
    GrpRwerte(0)("M").Itp = False
    GrpRwerte(1)("M").Itp = False
    GrpRwerte(0)("C").Itp = False
    GrpRwerte(0)("B").Itp = True

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

    txtBIS.Text = Date.Today.ToLocalTime
    txtVON.Text = Date.Today.AddDays(-MenueParam.Misch.Tdiff).ToLocalTime


    '
    '
    'Winkel und Lichtart einstellen
    '
    RezGraphics.Winkel = MenueParam.Messg.Winkel
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

    For i = 0 To MenueParam.Messg.Winkel.Km - 1
      RezCheckRad.radWIN(i).Visible = True
      RezCheckRad.radWIN(i).Enabled = False
      RezCheckRad.chkWIN(i).Visible = True
      RezCheckRad.chkWIN(i).Enabled = False
      RezCheckRad.chkWIN(i).Text = LTrim(MenueParam.Messg.Winkel(i).Chrm)
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
    If MenueParam.Misch.Transp Then
      ChkUNT(1).Checked = MenueParam.Misch.Schwrz
    Else
      ChkUNT(1).Checked = False
    End If
    '
    '
    Call VisVisKorrektur()
    '
    '
    'Messungen mit schwarzem Untergrund
    '
    '
    '
    '
    For i = 0 To 1
      ' btnMessVor(i).Text = Texxt(806 + RezGraphics.Vkwb(i)) & Space(1)
      'btnMessNch(i).Text = Texxt(828 + RezGraphics.Vkwb(i)) & Space(1)
      'btnMessUnt(i).Text = Texxt(808 + RezGraphics.Vkwb(i)) & Space(1)
      'btnMessVOO(i).Text = Texxt(806 + RezGraphics.Vkwb(i)) & Space(1)
      RezCheckRad.chkUUU(i).Text = Texxt(804 + RezGraphics.Vkwb(i))
      'lblMessNch(i).Text = Texxt(794 + RezGraphics.Vkwb(i))
      'lblMessUnt(i).Text = Texxt(796 + RezGraphics.Vkwb(i))
      'RezCheckRad.chkUUU(i).ToolTipText = Texxt(927 + VKwb(i + 1))
      If BitWrt(RezGraphics.Vkwb(i), MenueParam.Messg.MeArtID) Then
        'btnMessVor(i).Text = btnMessVor(i).Text & Space(1) & Texxt(3913)
        'btnMessNch(i).Text = btnMessNch(i).Text & Space(1) & Texxt(3913)
        'btnMessUnt(i).Text = btnMessUnt(i).Text & Space(1) & Texxt(3913)
        'btnMessVOO(i).Text = btnMessVOO(i).Text & Space(1) & Texxt(3913)
      End If
    Next i
    '
    '
    '
    For i = 0 To 1
      'txtDickKor(i).Text = MenueParam.Misch.Dicke
      'txtDickNch(i).Text = MenueParam.Misch.Dicke
      'lblMessUnt(i).Text = ""
      'lblMessNch(i).Text = ""
      'lblMessVor(i).Text = ""
      GrpRwerte(i).RefUnt.Name = ""
      GrpRwerte(i)("N").Name = ""
      GrpRwerte(i)("V").Name = ""
      GrpRwerte(i)("R").Name = ""
      GrpRwerte(i)("M").Name = ""
      'lblVOR(i).Text = ""
      'lblMessVOO(i).Text = ""
    Next i
    '
    '
    'Defaultwerte
    '
    '
    RezSozpt.Rezepte(KeyMenge).Dicke(0) = MenueParam.Misch.Dicke
    RezSozpt.Rezepte(KeyMenge).Dicke(1) = MenueParam.Misch.Dicke
    For i = 0 To 1
      UntID(i) = -1
    Next i
    For i = 0 To 1
      TypID(i) = -1
      SmpID(i) = -1
    Next i
    '
    '
    'Rezepte einlesen
    '
    '
    '
    '
    '
    '
    '
    btnSUC.PerformClick()
    Application.DoEvents()

    '
    '
    '
    '
    HandleRezept.GroupList()
    chkRwertAngleich.Checked = Not chkRwertAngleich.Checked

  End Sub
  Private Sub chkUNT_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkUNT_0.CheckStateChanged, chkUNT_1.CheckStateChanged
    If IsNothing(MenueParam) Then Exit Sub
    Call VisVisKorrektur()
  End Sub


  Private Sub btnSUC_Click(sender As Object, e As System.EventArgs) Handles btnSUC.Click
    Dim i As Integer




    Call SetRwerte(GrpRwerte)
    TblRezept.Rows.Clear()

    TblRezept.Rows.Clear()
    Application.DoEvents()
    '
    '
    '
    '
    '
    '
    'Korrekturrezepte
    '
    '
    Call RezActiv(0)
    RezGrid = RezptGrid(0)
    '
    RezGraphics.DataChanged = False
    Cursor = Cursors.WaitCursor
    RezSozpt.Rezepte(KeyMenge).clear()
    RezGrid.TDBFarRezStart(ier)
    ConnRezept.SuspendBinding()
    If Not HandleRezept.SqlRezeptKorr(cboGRP.SelectedValue, chkDatum.Checked, False, Date.Parse(txtVON.Text), Date.Parse(txtBIS.Text).AddDays(1.0), txtSUC.Text, Nothing, TblRezept) Then
      Cursor = Cursors.Default
      ConnRezept.ResumeBinding()
      Exit Sub
    End If

    '
    Application.DoEvents()
    Cursor = Cursors.Default
    RezGraphics.DataChanged = False

    If ViewRezept.Count = 0 Then
      MessageBox.Show(Texxt(2953))
      dbgREZ(0).Visible = False
      ConnRezept.ResumeBinding()
      lblMessUnt.Text = GrpRwerte(0).RefUnt.Name
      lblMessNch.Text = GrpRwerte(0)("M").Name
      chkUMR.Checked = True
      chkUMR.Visible = True
      lblMessNch.Visible = True
      lblMessUnt.Visible = True
      Exit Sub
    Else
      '
      'Rezept
      chkUMR.Visible = True
      dbgREZ(0).Visible = True
      Application.DoEvents()
      btnAustausch.Enabled = True
    End If
    ConnRezept.ResumeBinding()
    '
    '
    RezGrid.TDBFar.Visible = True
    chkUMR.Checked = False
    '
    '
  End Sub
  Private Sub frmChargenNuancieren_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Resize
    Call ResizeChild(Me)
  End Sub
  '
  '
  '
  '
  '


  Private Sub cboGLZ_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboGLZ.Click
    MenueParam.Messg.Winkel(0).GK(0) = CSng(cboGLZ.Text)
  End Sub


  Private Sub chkDatum_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkDatum.CheckedChanged
    If sender.name = "" Then Exit Sub
    If sender.checked Then
      lblBIS.Visible = True
      txtBIS.Visible = True
      txtVON.Visible = True
    Else
      lblBIS.Visible = False
      txtBIS.Visible = False
      txtVON.Visible = False

    End If
  End Sub


  Private Sub chkUMR_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkUMR.CheckedChanged
    Dim i As Integer
    Dim k As Integer
    Dim j As Integer
    btnMessNch.Enabled = False
    btnMessNch.Enabled = radSelectRwert(3).Checked And chkUMR.Checked
    btnMessUnt.Enabled = chkUMR.Checked

    btnMess(0).Enabled = Not chkUMR.Checked
    btnMess(1).Enabled = Not chkUMR.Checked
    Call CreateFarbWeissBindeSchwarz()
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
      lblMessUnt.Enabled = False
      lblMessNch.Enabled = False

      btnStoreAlt.Visible = False
      txtDicke.Enabled = False


    Else
      KeyMenge = "NEU"
      'RezGrid.TDBFar.Columns(CInt(RezGrid.TDBFar.Tag)).DataField = "NEU"
      RezGrid.txtFooter.Visible = False
      'RezGrid.lblSUM.Visible = False
      ' RezGrid.TDBFar.Columns(CInt(RezGrid.TDBFar.Tag)).Locked = False
      If BitWrt(22, MenueParam.User.Enabl) Then
        'For i = 0 To txtDickNch.Count - 1
        'txtDickNch(i).Enabled = True
        ' Next i
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
        txtDicke.Enabled = True
      End If
      lblMessUnt.Enabled = True
      lblMessNch.Enabled = True
      txtDicke.Enabled = True
      btnStoreAlt.Visible = True
    End If
    RezptGrid(0).NewKeynam(KeyMenge)
    RezGrid.TDBFarRezStart(ier)
  End Sub
  '
  '
  '
  Private Sub btnKorrektur_Click(sender As Object, e As System.EventArgs) Handles btnKorrektur.Click

    Dim i As Integer
    Dim k As Integer
    Dim KFI As Integer
    ier = 0
    '
    '
    '

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

    '
    '
    '
    '
    RezSozpt.Rezepte(KeyMenge).Dicke(RezGraphics.Vkwb(0)) = Singl(txtDicke.Text)

    RezID = RezSozpt.Rezepte(KeyMenge).ID
    RezSozpt.MngMin = txtMNG_0.Text
    RezSozpt.MngMax = txtMNG_1.Text
    RezSozpt.ProzMin = txtPRO_0.Text
    RezSozpt.ProzMax = txtPRO_1.Text
    Call HandleRezept.KorSpei(KeyMenge, RezID, MenueParam.UserID, chkARCH.CheckState, cboGRP.SelectedValue, RwWrRezept, RezGraphics, RezSozpt, UntID, TypID, SmpID, ier)
    If ier <> 0 Then
      GoTo ErgFeh
    End If

    If KeyMenge = "MEN" Or KeyMenge = "NEU" Then
      SplitContainSuchKorr.Visible = False
    Else
      GoTo ergfeh
    End If
    Cursor = Cursors.WaitCursor
    btnRezept.Enabled = False
    btnAustausch.Enabled = False
    lblARB.SetBounds(Me.Location.X, Me.Location.Y, _
    Me.Size.Width, Me.Size.Height)
    lblARB.Visible = True
    '
    Application.DoEvents()
    '
    '
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
    RezSozpt.Rezepte("KOA").Dicke(0) = CSng(txtDicke.Text)
    RezSozpt.Rezepte("KOA").Dicke(1) = CSng(txtDicke.Text)

    '
    '

    '
    For i = 0 To 1
      GrpRwerte(i)("V").IVoNa = False
      GrpRwerte(i)("N").IVoNa = False
      GrpRwerte(i)("V").Itp = True
      GrpRwerte(i)("N").Itp = False
    Next
    '
    '
    '
    '
    '
    '
    'R-Werte für Rezept("MEN") als Vorlage und Nachstellung berechnen
    '
    '
    '
    '
    '
    '
    CalcRezept.MischRezept(0, MenueParam.Messg.Winkel, KeyMenge, RezSozpt, GrpRwerte, OptGesamt, ier)

    For i = 0 To 1
      If GrpRwerte(i)("R").IVoNa Then
        GrpRwerte(i)("V").IVoNa = True
        GrpRwerte(i)("N").IVoNa = True
        GrpRwerte(i)("V").Iplott = True
        GrpRwerte(i)("N").Iplott = True
        For j = 0 To GrpRwerte(i)("R").RefKurv.Count - 1
          If radSelectRwert(1).Checked Then
            '
            '
            'Als Vorlage werden die gemessenen  R-Werte der Nachstellung verwendet
            '
            '
            '
            For k = 0 To GrpRwerte(i)("M").RefKurv(j).Nwe - 1
              GrpRwerte(i)("V").RefKurv(j).R(k) = GrpRwerte(i)("M").RefKurv(j).R(k)
            Next k
            GrpRwerte(i)("V").Name = GrpRwerte(i)("M").Name
          ElseIf radSelectRwert(2).Checked Then
            '
            '
            'Als Vorlage werden die gemessenen  R-Werte der Vorlage verwendet
            '
            '
            '
            For k = 0 To GrpRwerte(i)("V").RefKurv(j).Nwe - 1
              GrpRwerte(i)("V").RefKurv(j).R(k) = GrpRwerte(i)("V").RefKurv(j).R(k)
            Next k
            GrpRwerte(i)("V").Name = GrpRwerte(i)("V").Name

          ElseIf radSelectRwert(0).Checked Then
            '
            'Als Vorlage werden die aus den Farb-/Bindemeittelmengen berechneten  R-Werte verwendet
            '
            '
            '
            For k = 0 To GrpRwerte(i)("R").RefKurv(j).Nwe - 1
              GrpRwerte(i)("V").RefKurv(j).R(k) = GrpRwerte(i)("R").RefKurv(j).R(k)
            Next k
            GrpRwerte(i)("V").Name = RezSozpt.Rezepte("MEN").Name
          End If
          '
          'Als Nachstellung werden die aus den Farb-/Bindemeittelmengen berechneten  R-Werte verwendet
          'Anmerkung: Das Korrekturprogramm berechnet dann die gleichen Farb-/Bindemittelmengen wie das normale Erstrezeptprogramm,
          'da dann die berechneten R-Werte mit den R-Werte der Nachstellung übereinstimmen. 
          '
          '
          For k = 0 To GrpRwerte(i)("R").RefKurv(j).Nwe - 1
            GrpRwerte(i)("N").RefKurv(j).R(k) = GrpRwerte(i)("R").RefKurv(j).R(k)
          Next k
        Next j
      End If
    Next i
    '
    '
    'R-Werte für Farbschicht über zweitem Untergrunde werden nicht berücksichtigt
    '
    '
    GrpRwerte(1)("V").IVoNa = False

    '
    '
    '********************************
    'Berechnung des Korrekturrezeptes
    'Vorlage in  GrpRwerte(0)("V")
    'Berechnung eines Rezeptes, bei dem altes Farbmittel durch neues Farbmitel (Charge) ersetzt wird
    '********************************
    '
    KFI = RezGraphics.KFIT + 8
    CalcRezept.KorrekturRezept(KFI, MenueParam.Messg.Winkel, KeyMenge, "KOA", RezSozpt, GrpRwerte, ier)
    RezSozpt.Rezepte("KOA").Name = RezSozpt.Rezepte("MEN").Name
    '
    '
    '
    If ier > 0 Then
      GoTo ErgFeh
    End If

    RezDruck.KeyNam = "KOA"
    Call RezActiv(1)
    RezGrid = RezptGrid(1)
    RezCheckRad.Picauf = PicAufbau(1)
    If chkRZAen.Checked Then
      RezGrid.NewKeynam("KOO")
    Else
      RezGrid.NewKeynam("KOA")
    End If
    RezGrid.TDBFarRezStart(ier)

    Call GraphicPlotRwert(New String() {"V", "R"}, RezGrid)

    RezCheckRad.Picauf.Refresh()
    '

    lblARB.Visible = False
    RezGrid.TDBFar.Visible = True

    '
    Cursor = Cursors.Default
    '
    SplitContainSuchKorr.Visible = True

    SplitVisible = 1

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
    '
    '
    'hscMNG.Enabled = False
    btnAustausch.Enabled = False
    'cboWin.Enabled = True
    btnRezept.Enabled = True
    Cursor = Cursors.Default
    Exit Sub
    '
    '
    '
    'Fehler
    '
ErgFeh:


    SplitVisible = 0
    btnRezept.Enabled = True


    btnAustausch.Enabled = True
    SplitContainSuchKorr.Visible = True
    Cursor = Cursors.Default
    Call RezActiv(0)
    RezGrid = RezptGrid(0)
    Call RefreshCurrent()
    RezGrid.TDBFarRezStart(ier)
  End Sub



  Private Sub btnRezept_Click(sender As Object, e As System.EventArgs) Handles btnRezept.Click
    'cboWin.Enabled = False
    Call RezActiv(0)
    RezGrid = RezptGrid(0)
    RezCheckRad.Picauf = PicAufbau(0)
    Call RefreshCurrent()
    RezGrid.TDBFarRezStart(ier)
    cboMsh.Enabled = True
    SplitVisible = 0
    chkUMR.Checked = True
    chkUMR.Checked = False
    btnAustausch.Enabled = True
    RezGraphics.DataChanged = False
  End Sub
  '
  '
  '
  '
  Private Sub btnMess_Click(sender As Object, e As System.EventArgs) Handles btnMess_0.Click, btnMess_1.Click
    Dim index As Integer
    index = CInt(sender.name.substring(8, 1))
    '
    '
    If index = 0 Then
      RcmdRef = index + 31
      GetPutReflex.Messrefel = GrpRwerte(0)("C")
    ElseIf index = 1 Then
      RcmdRef = index + 20
      GetPutReflex.Messrefel = GrpRwerte(0)("B")
    End If

    GetPutReflex.Iarch = 0
    'GetPutReflex.Captext = btnMessVor(index).Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(0), RezGraphics.Vkwb(0), MenueParam.Messg.MeArtID)
    MenueParam.Messg.MeArtLock = BitWrt(RezGraphics.Vkwb(0), MenueParam.Messg.MeArtID)
    Me.Enabled = False
    GetPutReflex.ReflexWerte(False)
    Me.Enabled = True
    '
    '


  End Sub


  Private Sub lblMess_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMess_0.DoubleClick, lblMess_1.DoubleClick

    Dim Index As Short
    Index = CInt(sender.name.ToString.Substring(8, 1))
    If Index = 1 Then
      TypID(2) = -1
    Else
      SmpID(2) = -1
    End If
    lblMess(Index).Text = ""
    GrpRwerte(Index)("V").Name = ""
    GrpRwerte(Index)("V").IVoNa = False
  End Sub


  Private Sub lblMessUnt_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMessUnt.DoubleClick
    Dim Index As Short
    Index = CInt(sender.name.ToString.Substring(11, 1))
    UntID(Index) = -1
    lblMessUnt.Text = ""
    GrpRwerte(Index).RefUnt.Name = ""
    GrpRwerte(Index).RefUnt.IVoNa = False
  End Sub



  Private Sub Change(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblMess_0.TextChanged, lblMess_1.TextChanged

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
        RezSozpt.Rezepte("KOA").Dicke(0) = CSng(sender.text)
      End If
    End If
    If sender.name = "txtDICKKor_1" Then
      If IsNumeric(sender.text) Then
        RezSozpt.Rezepte("KOA").Dicke(1) = CSng(sender.text)
      End If
    End If

  End Sub

  Private Sub chkFAR_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkFAR.CheckedChanged
    If chkFAR.Checked Then
      picRezRezept_1.Visible = False
      picRezFarb_1.Visible = True
    Else
      picRezRezept_1.Visible = True
      picRezFarb_1.Visible = False
    End If
  End Sub
  Private Sub chkRwertAngleich_CheckStateChanged(sender As Object, e As System.EventArgs) Handles chkRwertAngleich.CheckStateChanged
    If chkRwertAngleich.Checked Then
      RezGraphics.KFIT = 2
    Else
      If MenueParam.Misch.Igx = 7 Then
        RezGraphics.KFIT = 3
      Else
        RezGraphics.KFIT = 1
      End If
    End If
  End Sub




  Private Sub btnStore_Click(sender As Object, e As System.EventArgs) Handles btnStoreAlt.Click, btnStoreNeu.Click
    Dim Iarch As Integer
    Dim RzNr As String
    Dim NachID(3) As Integer
    If sender.name = "btnStoreAlt" Then
      RzNr = "MEN"
      NachID(0) = SmpID(0)
      NachID(1) = SmpID(1)
    ElseIf sender.name = "btnStoreNeu" Then
      RzNr = "KOA"
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
    Call HandleRezept.KorSpei(RzNr, RezSozpt.Rezepte(RzNr).ID, MenueParam.UserID, chkARCH.CheckState, cboGRP.SelectedValue, RwWrRezept, RezGraphics, RezSozpt, UntID, TypID, NachID, ier)
    RezGraphics.DataChanged = False

  End Sub

  Private Sub btnDRU_Click(sender As Object, e As System.EventArgs) Handles btnDru_1.Click
    Dim index As Short
    index = CInt(sender.name.Substring(7, 1))
    Call DruckKorrekt(index, Kdru)
  End Sub

  Private Sub cboDRU_DropDownClosed(sender As Object, e As System.EventArgs) Handles cboDru_1.DropDownClosed
    Dim index As Short
    Dim Kdru As Short
    index = CInt(sender.name.Substring(7, 1))
    Kdru = sender.selectedindex
    Call DruckKorrekt(index, Kdru)
  End Sub

  Private Sub ConnRezept_CurrentChanged(sender As Object, e As System.EventArgs) Handles ConnRezept.CurrentChanged
    Dim i As Integer
    Call RefreshCurrent()
    For i = 0 To 1
      txtSchichtdicke(i).Enabled = False
      txtFarbmenge(i).Enabled = False
      txtWeissmenge(i).Enabled = False
      txtBindemenge(i).Enabled = False
      txtSchwarzmenge(i).Enabled = False
      txtFarbproz(i).Enabled = False
      txtWeissproz(i).Enabled = False
      txtBindeproz(i).Enabled = False
      txtSchwarzproz(i).Enabled = False
    Next
  End Sub

  Sub RefreshCurrent()
    Dim i As Integer
    Dim j As Integer
    Dim KeyFarb As String
    If IsNothing(RezGrid) Then Exit Sub
    If IsNothing(ConnRezept.Current) Then Exit Sub
    If ConnRezept.IsBindingSuspended Then Exit Sub

    chkVOL.CheckState = CheckState.Indeterminate
    '
    '

    For i = 0 To RezGrid.AllRezepte.Rezepte.RezCount - 1
      RezSozpt.Rezepte(i).clear()
    Next i
    '
    'Löschen 
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
    '
    '
    Call RwWrRezept.ReadRezeptFarbGrund(KeyMenge, RezID, RezSozpt, UntID, TypID, SmpID, ier)
    lblRezeptName.Text = RezSozpt.Rezepte(KeyMenge).Name
    RezGraphics.DataChanged = False
    '
    '
    ' cboFarbmittel
    '
    '
    Call CreateFarbWeissBindeSchwarz()
    chkVOL.CheckState = RezSozpt.IVOL
    '
    '
    '
    '
    '
    Call SetRwerte(GrpRwerte)

    '
    'Untergrund
    '
    '

    GrpRwerte(0).RefUnt.ID = UntID(RezGraphics.Vkwb(0))



    lblMessUnt.Visible = False

    If GrpRwerte(0).RefUnt.ID >= 0 Then
      Call ReWrRwert.ReadRwert(GrpRwerte(0).RefUnt.ID, GrpRwerte(0).RefUnt, ier)
      If ier = 0 Then
        GrpRwerte(0).RefUnt.IVoNa = True
        GrpRwerte(0).RefUnt.Nr = 0
        lblMessUnt.Visible = True
      End If
    End If
    lblMessUnt.Text = GrpRwerte(0).RefUnt.Name

    '
    'Vorlage
    '
    '

    GrpRwerte(0)("V").ID = TypID(RezGraphics.Vkwb(0))
    GrpRwerte(0)("V").IVoNa = False
    If GrpRwerte(0)("V").ID >= 0 Then
      Call ReWrRwert.ReadRwert(GrpRwerte(0)("V").ID, GrpRwerte(0)("V"), ier)
      If ier = 0 Then
        GrpRwerte(0)("V").IVoNa = True
        GrpRwerte(0)("V").Nr = 0
      End If
    End If
    '
    'Nachstellung
    '
    '
    lblMessNch.Visible = False
    GrpRwerte(0)("M").ID = SmpID(RezGraphics.Vkwb(0))
    GrpRwerte(0)("M").IVoNa = False
    If GrpRwerte(0)("M").ID >= 0 Then
      Call ReWrRwert.ReadRwert(GrpRwerte(0)("M").ID, GrpRwerte(0)("M"), ier)
      If ier = 0 Then
        GrpRwerte(0)("M").IVoNa = True
        GrpRwerte(0)("M").Nr = 0
        lblMessNch.Visible = True
      End If
    End If
    lblMessNch.Text = GrpRwerte(0)("M").Name
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
    'Charge
    '
    '
    GrpRwerte(0)("C").ID = -1
    GrpRwerte(0)("C").IVoNa = False
    If GrpRwerte(0)("C").ID >= 0 Then
      Call ReWrRwert.ReadRwert(GrpRwerte(0)("C").ID, GrpRwerte(0)("C"), ier)
      If ier = 0 Then
        GrpRwerte(0)("C").IVoNa = True
        GrpRwerte(0)("C").Nr = 0
      End If
    End If
    If GrpRwerte(0)("C").IVoNa Then
      lblMess(0).Text = GrpRwerte(0)("C").Name
    Else
      lblMess(0).Text = ""
    End If
    '
    '
    '
    Call ReadQual(0, GrpRwerte(0)("C"), ier)

    '
    'Bezug
    '
    '
    GrpRwerte(0)("B").ID = -1
    GrpRwerte(0)("B").IVoNa = False
    If GrpRwerte(0)("B").ID >= 0 Then
      Call ReWrRwert.ReadRwert(GrpRwerte(0)("B").ID, GrpRwerte(0)("B"), ier)
      If ier = 0 Then
        GrpRwerte(0)("B").IVoNa = True
        GrpRwerte(0)("B").Nr = 0
      End If
    End If
    If GrpRwerte(0)("B").IVoNa Then
      lblMess(1).Text = GrpRwerte(0)("B").Name
    Else
      lblMess(1).Text = ""
    End If
    '
    '
    Call ReadQual(1, GrpRwerte(0)("B"), ier)
    '
    '
    '
    '
    '
    Call RezActiv(0)
    RezGrid = RezptGrid(0)
    RezCheckRad.Picauf = PicAufbau(0)
    'Call GraphicPlotRwert(New String() {"C", "B"}, RezGrid)
    Call PlottXYZ(RezGraphics)

    '
    '
    chkVOL.CheckState = RezSozpt.IVOL '
    Call VisVisKorrektur()
    RezGrid.TDBFarRezStart(ier)
    RezDruck.KeyNam = "MEN"
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
    txtDicke.Text = HandleRezept.AutFormat(RezSozpt.Rezepte(KeyMenge).Dicke(0))
    If RezSozpt.Rezepte(0).Iarch < 0 Or RezSozpt.Rezepte(0).Iarch > 2 Then
      chkARCH.CheckState = CheckState.Unchecked
    Else
      chkARCH.CheckState = RezSozpt.Rezepte(KeyMenge).Iarch
    End If
    RezSozpt.Rezepte("KOA").Name = RezSozpt.Rezepte(KeyMenge).Name

    '
    '

    btnAustausch.Enabled = True

    '
    Cursor = Cursors.Default
    '
    '
    '

    RezGraphics.DataChanged = False

    Application.DoEvents()
    '
    '
  End Sub

  Sub CreateFarbWeissBindeSchwarz()
    Dim Keyfarb As String
    Dim i As Integer
    Dim j As Integer
    If IsNothing(RezSozpt) Then Exit Sub
    If RezSozpt.Rezepte(KeyMenge).KF = 0 Then Exit Sub
    cboFarbmittel.Items.Clear()


    '
    'Weißpigment
    '
    '
    lblWeisspigment.Text = ""
    FaIDWeiss = -1
    For i = 0 To RezSozpt.Rezepte(KeyMenge).KF - 1
      Keyfarb = KeyName(RezSozpt.Rezepte(KeyMenge)(i).ID)
      For j = 0 To RezSozpt.Farben.FarbCount - 1
        If chkWeissPigment.Checked And RezSozpt.Farben(j).Ichf = 2 AndAlso Keyfarb = RezSozpt.Farben.FaKey(j) Then
          lblWeisspigment.Text = RezSozpt.Farben(j).Name
          FaIDWeiss = RezSozpt.Rezepte(KeyMenge)(i).ID
        End If
      Next
    Next
    '
    '
    'Bindemittel
    '
    '

    lblBindemittel.Text = ""
    FaIDBinde = -1
    For i = 0 To RezSozpt.Rezepte(KeyMenge).KF - 1
      Keyfarb = KeyName(RezSozpt.Rezepte(KeyMenge)(i).ID)
      For j = 0 To RezSozpt.Farben.FarbCount - 1
        If chkBindemittel.Checked And RezSozpt.Farben(j).Ichf = 1 AndAlso Keyfarb = RezSozpt.Farben.FaKey(j) Then
          lblBindemittel.Text = RezSozpt.Farben(j).Name
          FaIDBinde = RezSozpt.Rezepte(KeyMenge)(i).ID
        End If
      Next
    Next
    '
    '
    'Schwarzpigment
    '
    '

    lblSchwarzpigment.Text = ""
    FaIDSchwarz = -1
    For i = 0 To RezSozpt.Rezepte(KeyMenge).KF - 1
      Keyfarb = KeyName(RezSozpt.Rezepte(KeyMenge)(i).ID)
      For j = 0 To RezSozpt.Farben.FarbCount - 1
        If chkSchwarzPigment.Checked And RezSozpt.Farben(j).Ichf = 3 AndAlso Keyfarb = RezSozpt.Farben.FaKey(j) Then
          lblSchwarzpigment.Text = RezSozpt.Farben(j).Name
          FaIDSchwarz = RezSozpt.Rezepte(KeyMenge)(i).ID
        End If
      Next
    Next
    '
    '
    '
    'Farbmittel
    '
    '

    For i = 0 To RezSozpt.Rezepte(KeyMenge).KF - 1
      Keyfarb = KeyName(RezSozpt.Rezepte(KeyMenge)(i).ID)
      For j = 0 To RezSozpt.Farben.FarbCount - 1
        If RezSozpt.Farben(j).ID <> FaIDBinde And RezSozpt.Farben(j).ID <> FaIDWeiss And RezSozpt.Farben(j).ID <> FaIDSchwarz AndAlso Keyfarb = RezSozpt.Farben.FaKey(j) Then
          cboFarbmittel.Items.Add(New ListTextID(RezSozpt.Farben(j).ID, RezSozpt.Farben(j).Name))
        End If
      Next
    Next
    FaIDFarb = -1
    cboFarbmittel.DisplayMember = "TEXT"
    cboFarbmittel.ValueMember = "ID"
    If cboFarbmittel.Items.Count > 0 Then
      cboFarbmittel.SelectedIndex = 0
    End If
    cboFarbmittel.Enabled = True
    If GrpRwerte(0)("C").QuControl.MedNr > -1 Then
      For i = 0 To cboFarbmittel.Items.Count - 1
        If cboFarbmittel.Items(i).id = GrpRwerte(0)("C").QuControl.MedNr Then
          cboFarbmittel.SelectedIndex = i
          Exit For
        End If
      Next
    End If
    '
  End Sub

  Private Sub chkRZAen_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkRZAen.CheckedChanged
    If chkRZAen.Checked Then
      RezGrid.NewKeynam("KOO")
    Else
      RezGrid.NewKeynam("KOA")
    End If
  End Sub

  ' 
  '

  Private Sub cboGRP_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboGRP.SelectedValueChanged
    If Not IsNumeric(cboGRP.SelectedValue) Then Exit Sub
    If Not cboGRP.Enabled Then Exit Sub
    btnSUC.PerformClick()
  End Sub

  Private Sub dbgREZ_DoubleClick(sender As Object, e As System.EventArgs) Handles dbgREZ_0.DoubleClick
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


  Private Sub dbgREZ_KeyDown(sender As Object, e As System.Windows.Forms.KeyEventArgs) Handles dbgREZ_0.KeyDown
    If e.KeyCode = Keys.Delete Then
      dbgREZ_DoubleClick(sender, e)
    End If
  End Sub

  Private Sub txt_TextChanged(sender As Object, e As System.EventArgs) Handles txtSchichtdicke_0.TextChanged, txtSchichtdicke_1.TextChanged
    If Not IsNumeric(sender.text) Then
      sender.text = sender.tag
    End If

  End Sub


  '
  '
  '
  '
  Sub WriteQual(index As Integer, ByRef RefWerte As RefValue, ByRef ier As Integer)
    If RefWerte.ID < 0 Then Exit Sub
    RefWerte.QuControl.MethID = MenueParam.MethID
    '
    'Ausgewählte FarbID
    '
    '
    RefWerte.QuControl.MedNr = FaIDFarb
    RefWerte.QuControl.IuntID = -1
    RefWerte.QuControl.Camp(0) = CSng(txtSchichtdicke(index).Text)
    RefWerte.QuControl.Camp(1) = CSng(txtFarbmenge(index).Text)
    RefWerte.QuControl.Camp(2) = CSng(txtWeissmenge(index).Text)
    RefWerte.QuControl.Camp(3) = CSng(txtBindemenge(index).Text)
    RefWerte.QuControl.Camp(4) = CSng(txtSchwarzmenge(index).Text)
    RefWerte.QuControl.Camp(5) = CSng(txtFarbproz(index).Text)
    RefWerte.QuControl.Camp(6) = CSng(txtWeissproz(index).Text)
    RefWerte.QuControl.Camp(7) = CSng(txtBindeproz(index).Text)
    RefWerte.QuControl.Camp(8) = CSng(txtSchwarzproz(index).Text)

    Call ReWrRwert.WriteQuali(RefWerte.ID, RefWerte, ier)

  End Sub
  Sub ReadQual(index As Integer, ByRef RefWerte As RefValue, ByRef ier As Integer)
    If RefWerte.ID > -1 Then
      Call ReWrRwert.ReadQuali(RefWerte.ID, RefWerte, ier)
      If RefWerte.QuControl.Camp(0) < 0.0 Then
        RefWerte.QuControl.Camp(0) = 1.0
      End If
      If RefWerte.QuControl.Camp(1) < 0.0 Then
        RefWerte.QuControl.Camp(1) = 1.0
      End If
      If RefWerte.QuControl.Camp(2) < 0.0 Or Not chkWeissPigment.Checked Then
        RefWerte.QuControl.Camp(2) = 0.0
      End If
      If RefWerte.QuControl.Camp(3) < 0.0 Or Not chkBindemittel.Checked Then
        RefWerte.QuControl.Camp(3) = 0.0
      End If
      If RefWerte.QuControl.Camp(4) < 0.0 Or Not chkSchwarzPigment.Checked Then
        RefWerte.QuControl.Camp(4) = 0.0
      End If
      If RefWerte.QuControl.Camp(5) < 0.0 Then
        RefWerte.QuControl.Camp(5) = 100.0
      End If
      If RefWerte.QuControl.Camp(6) < 0.0 Then
        RefWerte.QuControl.Camp(6) = 100.0
      End If
      If RefWerte.QuControl.Camp(7) < 0.0 Then
        RefWerte.QuControl.Camp(7) = 100.0
      End If
      If RefWerte.QuControl.Camp(8) < 0.0 Then
        RefWerte.QuControl.Camp(8) = 100.0
      End If

      txtSchichtdicke(index).Text = Format(RefWerte.QuControl.Camp(0), "###0.000")
      txtFarbmenge(index).Text = Format(RefWerte.QuControl.Camp(1), "###0.000")
      txtWeissmenge(index).Text = Format(RefWerte.QuControl.Camp(2), "###0.000")
      txtBindemenge(index).Text = Format(RefWerte.QuControl.Camp(3), "###0.000")
      txtSchwarzmenge(index).Text = Format(RefWerte.QuControl.Camp(4), "###0.000")
      txtFarbproz(index).Text = Format(RefWerte.QuControl.Camp(5), "###0.000")
      txtWeissproz(index).Text = Format(RefWerte.QuControl.Camp(6), "###0.000")
      txtBindeproz(index).Text = Format(RefWerte.QuControl.Camp(7), "###0.000")
      txtSchwarzproz(index).Text = Format(RefWerte.QuControl.Camp(8), "###0.000")
    Else
      RefWerte.QuControl.MedNr = -1
      txtSchichtdicke(index).Text = Format(1.0, "###0.000")
      txtFarbmenge(index).Text = Format(100, "###0.000")
      txtWeissmenge(index).Text = Format(0, "###0.000")
      txtBindemenge(index).Text = Format(0, "###0.000")
      txtSchwarzmenge(index).Text = Format(0, "###0.000")
      txtFarbproz(index).Text = Format(100, "###0.000")
      txtWeissproz(index).Text = Format(100, "###0.000")
      txtBindeproz(index).Text = Format(100, "###0.000")
      txtSchwarzproz(index).Text = Format(100, "###0.000")
      lblMess(index).Text = ""
    End If
  End Sub
  Sub FillRezept(ByRef RefWerte As RefValue, ByRef Rezept As Recipe, ByRef Farben As Colorants, ByRef ier As Integer)
    Dim i As Integer
    Dim j As Integer
    ier = 0
    If RefWerte.ID < 0 Then
      ier = -1
      Exit Sub
    End If

    Rezept.clear()
    '
    'Farbmittel
    '
    If FaIDFarb > -1 Then
      i = Rezept.KF
      Rezept.AddFaNr(KeyRe(i), New ColorAmount)
      Rezept(i).BaAmng = RefWerte.QuControl.Camp(1)
      Rezept(i).Proz = RefWerte.QuControl.Camp(5)
      Rezept(i).ID = FaIDFarb
    End If
    '
    'Weisspigment
    '
    If FaIDWeiss > -1 Then
      i = Rezept.KF
      Rezept.AddFaNr(KeyRe(i), New ColorAmount)
      Rezept(i).BaAmng = RefWerte.QuControl.Camp(2)
      Rezept(i).Proz = RefWerte.QuControl.Camp(6)
      Rezept(i).ID = FaIDWeiss
      Farben(KeyName(FaIDWeiss)).BoMng = Rezept(i).BaAmng
    End If

    '
    'Bindemittel
    '
    If FaIDBinde > -1 Then
      i = Rezept.KF
      Rezept.AddFaNr(KeyRe(i), New ColorAmount)
      Rezept(i).BaAmng = RefWerte.QuControl.Camp(3)
      Rezept(i).Proz = RefWerte.QuControl.Camp(7)
      Rezept(i).ID = FaIDBinde
      Farben(KeyName(FaIDBinde)).BoMng = Rezept(i).BaAmng
    End If

    '
    'Schwarzpigment
    '
    If FaIDSchwarz > -1 Then
      i = Rezept.KF
      Rezept.AddFaNr(KeyRe(i), New ColorAmount)
      Rezept(i).BaAmng = RefWerte.QuControl.Camp(4)
      Rezept(i).Proz = RefWerte.QuControl.Camp(8)
      Rezept(i).ID = FaIDSchwarz
      Farben(KeyName(FaIDSchwarz)).BoMng = Rezept(i).BaAmng
    End If

    '
    '
    '
    Rezept.Dicke(0) = RefWerte.QuControl.Camp(0)
  End Sub
  Sub PlottXYZ(ByRef RezGraphics As HandleRezGrafik)
    RezGraphics.FawrtRwerte(0).clear()
    RezGraphics.FawrtRwerte(1).clear()
    RezGraphics.PlotRwerte.clear()

    If lblMess(0).Text = "" Then
      GrpRwerte(0)("C").Iplott = False
    Else
      GrpRwerte(0)("C").Iplott = True
      RezGraphics.FawrtRwerte(0).Add(KeyRe(0), GrpRwerte(0)("C"))
      RezGraphics.PlotRwerte.Add(KeyRe(0), GrpRwerte(0)("C"))
    End If
    If lblMess(1).Text = "" Then
      GrpRwerte(0)("B").Iplott = False
    Else
      GrpRwerte(0)("B").Iplott = True
      RezGraphics.FawrtRwerte(0).Add(KeyRe(1), GrpRwerte(0)("B"))
      RezGraphics.PlotRwerte.Add(KeyRe(1), GrpRwerte(0)("B"))
    End If
    RezGrid.Picauf.Refresh()
  End Sub

  Private Sub chkRwert_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkRwert.CheckedChanged
    splPicXYZ_0.Visible = Not chkRwert.Checked
    picFarbXYZ_0.Visible = Not chkRwert.Checked
    picRwert_0.Visible = chkRwert.Checked

  End Sub

  Private Sub btnAustausch_Click(sender As Object, e As System.EventArgs) Handles btnAustausch.Click
    Dim Farbcount As Integer
    Dim ier As Integer
    Dim i, j, k As Integer
    If GrpRwerte(0)("C").ID < 0 Or Not GrpRwerte(0)("C").IVoNa Then
      MsgBox(Texxt(3804))
      Exit Sub
    End If
    FaIDFarb = cboFarbmittel.SelectedItem.id
    Call WriteQual(0, GrpRwerte(0)("C"), ier)


    If ier <> 0 Then
      Exit Sub
    End If
    '
    If txtRestGew.Visible Then
      MenueParam.Menue.Delta = CSng(txtRestGew.Text)
    End If
    RezChaBez.Farben = RezSozpt.Farben.clone
    RezChaBez.Farben(KeyName(FaIDFarb)).OP = " "
    If FaIDBinde >= 0 Then
      RezChaBez.Farben(KeyName(FaIDBinde)).OP = "="
    End If
    If FaIDWeiss >= 0 Then
      RezChaBez.Farben(KeyName(FaIDWeiss)).OP = "="
    End If
    If FaIDSchwarz >= 0 Then
      RezChaBez.Farben(KeyName(FaIDSchwarz)).OP = "="
    End If

    GrpRwertChaBez("W").RefUnt = GrpRwerte("W").RefUnt
    '
    '
    '
    'Farben, die nicht im Rezept (NEU) vorkommen, werden eliminiert
    '
    '
    '
    Farbcount = RezChaBez.Farben.FarbCount
    For j = Farbcount - 1 To 0 Step -1
      For i = 0 To RezSozpt.Rezepte("NEU").KF - 1
        If RezChaBez.Farben(j).ID = RezSozpt.Rezepte("NEU")(i).ID Then
          Exit For
        End If
      Next
      If i = RezSozpt.Rezepte("NEU").KF Then
        RezChaBez.Farben.RemoveFarb(KeyName(RezChaBez.Farben(j).ID))
      End If
    Next

    '
    '
    '****************************
    'Grunddaten für Chargenrezept 
    '****************************
    '
    Call FillRezept(GrpRwerte("W")("C"), RezChaBez.Rezepte("CHA"), RezChaBez.Farben, ier)
    If ier = 0 Then
      Call Umr.CalcFamng("CHA", RezChaBez, ier)
    End If
    GrpRwertChaBez("W").clear()
    GrpRwertChaBez("W").Add("V", GrpRwerte("W")("C"))
    GrpRwertChaBez("W").Add("R", GrpRwerte(0)("R"))

    Call CalcRezept.Restfarbe(5, "CHA", "CHA", RezChaBez, GrpRwertChaBez, ChargenFarb, ier)
    If ier <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    '
    '
    '
    If RezChaBez.Rezepte.ContainsKey("BEZ") AndAlso GrpRwerte("W")("B").IVoNa Then
      Call WriteQual(1, GrpRwerte(0)("B"), ier)

      '
      '****************************
      'Grunddaten für Bezugsrezept
      '****************************
      '
      '
      Call FillRezept(GrpRwerte("W")("B"), RezChaBez.Rezepte("BEZ"), RezChaBez.Farben, ier)
      If ier = 0 Then
        Call Umr.CalcFamng("BEZ", RezChaBez, ier)
      End If
      GrpRwertChaBez("W").clear()
      GrpRwertChaBez("W").Add("V", GrpRwerte("W")("B"))
      GrpRwertChaBez("W").Add("R", GrpRwerte(0)("R"))
      Call CalcRezept.Restfarbe(5, "BEZ", "BEZ", RezChaBez, GrpRwertChaBez, BezugsFarb, ier)
      If ier <> 0 Then
        Exit Sub
      End If
      '
      '
      '
      '
      '
      'Chargendaten korrigieren
      '
      '
      '
      Call quali.GrundCorr(ChargenFarb, ChargenFarb, BezugsFarb, RezChaBez.Farben(KeyName(FaIDFarb)), ier)
    End If
    '
    '
    '
    'Chargendaten abspeichern
    '
    'If Not WinUSeqMe(MenueParam.User.Winkel, MenueParam.Messg.Winkel) Then Exit Sub

    '
    '
    If ier = 0 Then
      ChargenFarb.Name = InputBox(Texxt(815), Texxt(853), "@" & cboFarbmittel.Text)
      If Trim(ChargenFarb.Name) <> "" Then
        '
        '
        'Speicher Restfarbe
        '
        '
        '
        ChargenFarb.GlzGrdID = -1
        ChargenFarb.GlzGrd = 0.0
        ReWrFarb.FarAdd(ChargenFarb, ier)
        '
        'Speichern Grunddaten der Restfarbe
        '
        '
        '
        ReWrGrund.WriteGrund(ChargenFarb.ID, ChargenFarb.OptData, MenueParam.Messg.Winkel, ier)
        '
        '
        'Chargenfarb zu Rezsozpt.Farben hinzufügen
        '
        RezSozpt.Farben.AddFarb(KeyName(ChargenFarb.ID), ChargenFarb)
        '
        '
        '
        'Chargenfarb zu Rezsozpt.Rezepte hinzufügen
        '

        '
        i = RezSozpt.Rezepte("MEN").KF
        RezSozpt.Rezepte("MEN").AddFaNr(KeyRe(i), New ColorAmount)
        RezSozpt.Rezepte("MEN")(i).ID = ChargenFarb.ID
        RezSozpt.Rezepte("MEN")(i).FaAmng = 0.0
        RezSozpt.Rezepte("MEN")(i).BaAmng = 0.0
        '
        'Limitierung von FaIDFarb=0.0
        '
        RezSozpt.Farben(KeyName(FaIDFarb)).BoMng = 0.0
        RezSozpt.Farben(KeyName(FaIDFarb)).OP = "="
        '
        RezGrid.DatenFarb.AddNewFarb(ChargenFarb)
        RezGrid.TDBFarRezStart(ier)
        '
        btnKorrektur.Enabled = True
      End If
    End If

    '
  End Sub





  Private Sub cboFarbmittel_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboFarbmittel.SelectedIndexChanged
    GrpRwerte(0)("C").ID = -1
    GrpRwerte(0)("C").IVoNa = False
    Call ReadQual(0, GrpRwerte(0)("C"), ier)
    GrpRwerte(0)("B").ID = -1
    GrpRwerte(0)("B").IVoNa = False
    Call ReadQual(1, GrpRwerte(0)("B"), ier)
    btnKorrektur.Enabled = True
  End Sub




  Private Sub btnMessNch_Click(sender As Object, e As System.EventArgs) Handles btnMessNch.Click


    '
    RcmdRef = 1
    GetPutReflex.Messrefel = GrpRwerte(0)("M")
    GetPutReflex.Iarch = 0
    GetPutReflex.Captext = btnMessNch.Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(0), RezGraphics.Vkwb(0), MenueParam.Messg.MeArtID)
    MenueParam.Messg.MeArtLock = BitWrt(RezGraphics.Vkwb(0), MenueParam.Messg.MeArtID)
    Me.Enabled = False
    GetPutReflex.ReflexWerte(False)
    Me.Enabled = True


    '
    '

  End Sub
  Private Sub btnMessUnt_Click(sender As Object, e As System.EventArgs) Handles btnMessUnt.Click
    RcmdRef = -1

    '
    'Messung
    '
    '

    GetPutReflex.Iarch = 1
    GetPutReflex.Messrefel = GrpRwerte(0).RefUnt

    GetPutReflex.Captext = btnMessUnt.Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(0), RezGraphics.Vkwb(0), MenueParam.Messg.MeArtID)
    MenueParam.Messg.MeArtLock = BitWrt(RezGraphics.Vkwb(0), MenueParam.Messg.MeArtID)
    Me.Enabled = False
    GetPutReflex.ReflexWerte(False)
    Me.Enabled = True

    '

  End Sub
  Sub GetNameID(ByRef ID As Integer, ByRef RefName As String, ByRef Banum As String)
    Dim j As Integer
    Dim index As Integer
    Dim ier As Integer
    ier = 0

    If RefName <> "" Then
      '
      If RcmdRef = 0 Then Exit Sub

      If GetPutReflex.HideSofort Then
        GetPutReflex.InVisible()
      End If
      '
      If RcmdRef >= -1 Then
        If Abs(RcmdRef) = 21 Then
          ReUn = "B"
          j = 0
        ElseIf RcmdRef = 31 Then
          ReUn = "C"
          j = 0
        ElseIf RcmdRef = -1 Then
          j = 0
        ElseIf RcmdRef = 1 Then
          ReUn = "M"
          j = 0
        End If
      End If
      If ier = 0 Then
        Select Case RcmdRef
          Case 31
            index = 0
            lblMess(0).Text = GrpRwerte(j)(ReUn).Name
          Case 21
            index = 1
            lblMess(1).Text = GrpRwerte(j)(ReUn).Name
          Case 1
            lblMessNch.Text = GrpRwerte(j)(ReUn).Name
            SmpID(RezGraphics.Vkwb(j)) = ID
            Exit Sub
          Case -1
            lblMessUnt.Text = GrpRwerte(j).RefUnt.Name
            UntID(RezGraphics.Vkwb(j)) = ID
            Exit Sub
        End Select
      End If
    End If
    If index >= 0 Then
      txtSchichtdicke(index).Enabled = True
      txtFarbmenge(index).Enabled = True
      txtWeissmenge(index).Enabled = True
      txtBindemenge(index).Enabled = True
      txtSchwarzmenge(index).Enabled = True

      txtFarbproz(index).Enabled = True
      txtWeissproz(index).Enabled = True
      txtBindeproz(index).Enabled = True
      txtSchwarzproz(index).Enabled = True

    End If
    Call ReadQual(index, GrpRwerte(j)(ReUn), ier)
    Call PlottXYZ(RezGraphics)


  End Sub



  Private Sub txtDicke_Leave(sender As Object, e As System.EventArgs) Handles txtDicke.Leave
    RezSozpt.Rezepte(KeyMenge).Dicke(0) = Singl(txtDicke.Text)
  End Sub

  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtMNG_0.Validating, txtMNG_1.Validating, txtPRO_0.Validating, txtPRO_1.Validating, _
    txtDicke.Validating, txtSum_0.Validating, txtSUM_1.Validating, txtBindeProz_0.Validating, txtFarbProz_1.Validating, txtBindeMenge_0.Validating, txtBindeMenge_1.Validating, _
    txtBindeProz_0.Validating, txtBindeProz_1.Validating, txtWeissMenge_0.Validating, txtWeissMenge_1.Validating, txtWeissProz_0.Validating, txtWeissProz_1.Validating, _
     txtSchwarzMenge_0.Validating, txtSchwarzMenge_1.Validating, txtSchwarzProz_0.Validating, txtSchwarzProz_1.Validating, _
    txtFarbMenge_0.Validating, txtFarbMenge_1.Validating, txtFarbProz_0.Validating, txtFarbProz_1.Validating, txtSchichtdicke_0.Validating, txtSchichtdicke_1.Validating, _
    txtRestGew.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub

  Private Sub txtDat_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtBIS.Validating, txtVON.Validating
    If Not IsDate(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(2500))
    End If
  End Sub

  Private Sub radSelectRwert_Click(sender As Object, e As System.EventArgs) Handles radSelectRwert_0.Click, radSelectRwert_1.Click, radSelectRwert_2.Click, radSelectRwert_3.Click
    Dim index As Integer
    index = CInt(sender.name.substring(15, 1))

    radSelectRwert(3).Visible = True
    btnMessNch.Visible = False
    lblMessNch.Visible = False

    Select Case index
      Case 3
        btnMessNch.Enabled = False
        btnMessNch.Enabled = radSelectRwert(3).Checked And chkUMR.Checked
        If radSelectRwert(3).Checked Then
          radSelectRwert(3).Visible = False
          btnMessNch.Visible = True
          lblMessNch.Visible = True
        End If
    End Select
  End Sub

  Private Sub chkWeissBindeSchwarz_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkWeissPigment.CheckedChanged, chkBindemittel.CheckedChanged, chkSchwarzPigment.CheckedChanged
    Call CreateFarbWeissBindeSchwarz()
  End Sub
End Class