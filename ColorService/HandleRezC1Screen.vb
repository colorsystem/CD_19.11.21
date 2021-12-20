Option Strict Off
Option Explicit On 
Option Compare Text
Public Class HandleRezC1Screen
  Implements IDisposable


  Dim MnChkGrundKorr As Short
  Dim ColumnsOPR As C1DataColumn
  Dim ColumnsKTO As C1DataColumn
  Dim disposed As Boolean
  Dim MnDatGrid As DataGrid
  Dim AfterEd As Boolean = False
  Dim JNF As Short
  Dim JNM As Short
  Dim MnIer As Short
  Dim i As Short
  Dim j As Short
  Dim k As Short
  Dim Tol As Single
  Dim Amenge As Single
  Dim Fakt As Single
  Dim WithSmng As Boolean
  Dim WrtAlt As Single
  Dim WrtNeu As Single
  Dim WrtProz As Single
  Dim OldRow As Integer
  Dim OldFaID As Integer
  Dim FaID As Integer
  Dim KeyIndex As String
  Dim KeyNam As String
  Dim Ihilf As Short
  Dim KeyHilf(2) As String
  Dim KeId As String
  Dim FaNr As String
  Dim KeyNr As String
  Dim FirstRow As Integer
  Dim MaxRow As Integer
  Dim MaxCol As Short
  Dim MnPicAuf As HandlePictures
  Dim MnPicGraphic As HandleRezGrafik
  Dim ToolTipC1Screen As ToolTip

  Dim MnCalcRezept As RezeptBerechnung
  Dim WithEvents MnTDBFar As C1TrueDBGrid
  Dim WithEvents MnTDBDropFar As C1TrueDBDropdown
  Dim WithEvents MnTDBDropPre As C1TrueDBDropdown
  Dim WithEvents MnTDBDropPro As C1TrueDBDropdown
  Dim WithEvents MnTDBDropPrb As C1TrueDBDropdown
  Dim DataCheckPrg As ValueItems
  Dim DataComboOP As ValueItems
  Dim DataComboKto As ValueItems
  Dim ConnRezept As BindingSource
  '
  '
  '
  '
  Dim MnImgButton As PictureBox  'Image für Rezeptberechnung
  Dim PrintAllow As Boolean  'Kennung, ob Drucken mit F4 erlaubt
  Dim WithEvents MnlstFAR As ListBox
  Dim WithEvents MnChkVOL As CheckBox
  Dim WithEvents MncboMNG As ComboBox
  Dim WithEvents MncboPRO_0 As ComboBox
  Dim WithEvents MncboPRO_1 As ComboBox
  Dim WithEvents MnTxtMNG_0 As TextBox
  Dim WithEvents MnTxtMNG_1 As TextBox
  Dim WithEvents MnTxtPRO_0 As TextBox
  Dim WithEvents MnTxtPRO_1 As TextBox
  Dim WithEvents MncboMIM_0 As ComboBox
  Dim WithEvents MncboMIM_1 As ComboBox
  Dim WithEvents MnTxtFooter As TextBox

  Dim WithEvents ComboFarb As ComboBox
  Dim Reb As Boolean
  Dim MngSUM As Single
  Dim MngHlf As Single
  Dim KbProb As Short  '0 Prob ist auf Binde- und Lösemittel bezogen; 1  ~ auf Gesamtmenge bezogen
  Dim MnAllRezepte As RecipesGrp
  Dim MnGrpRwerte As RefValuesGrp
  Dim GrundDat As OpticalData
  Dim ReWrFarbe As ReadWriteFarbe
  Dim MnUmr As RezeptUmrechnung
  Dim MnDatenFarb As HandleRezDsetFarb
  Dim TblRezept As DataTable
  Dim DbErr As Short
  Dim HeaderText(6) As String 'Überschrift für Art der Menge gemäß (INF)
  Dim HeadLiText(6) As String  'Überschrift für Art der Menge gemäß (INM)
  Dim LayoutChanged As Boolean
  Dim imsg As Short
  Dim TdbFarBook As String
  Dim Weite() As Single = {20.0, 150.0, 50.0, 30.0, 60.0}
 
  '
  '
  '
  '
  '
  '
  Public Sub New()
    MnUmr = New RezeptUmrechnung
    ReWrFarbe = New ReadWriteFarbe
    GrundDat = New OpticalData
    ColumnsOPR = New C1DataColumn("", GetType(String))
    ColumnsKTO = New C1DataColumn("", GetType(String))
    TblRezept = New DataTable
    ConnRezept = New BindingSource
    ConnRezept.DataSource = TblRezept

    'OnAdd = False
    OldFaID = -1
    KbProb = 0
    Tol = 0.0000001
    Ihilf = 0
    KeyIndex = ""

    LayoutChanged = False
    PrintAllow = False
    FirstRow = -1
    If PrinterSettings.InstalledPrinters.Count > 0 Then
      PrintAllow = True
    End If
    disposed = False
    KeyNam = ""
    ToolTipC1Screen = New ToolTip
    MnChkGrundKorr = 0
  End Sub




  Protected Overrides Sub Finalize()


    MyBase.Finalize()

    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    Dim i As Integer
    If disposed Then Exit Sub
    Try
      If BitWrt(16, MenueParam.User.Drum) Then
        If LayoutChanged Then
          For i = 0 To MnTDBFar.Columns.Count - 1
            If Not MnTDBFar.Splits(0).DisplayColumns(i).Visible Then
            End If
          Next i
          'MnTDBFar.SaveLayout(GridFileName())
        End If
      End If
      LayoutChanged = False
    Catch
    End Try
    MnUmr.dispose()
    ReWrFarbe.dispose()
    GrundDat.dispose()

    disposed = True
    'GC.SuppressFinalize(Me)
  End Sub


  ReadOnly Property ier() As Short
    Get
      ier = MnIer
    End Get
  End Property

  Property ChkGrundKorr() As Short
    Get
      ChkGrundKorr = MnChkGrundKorr
    End Get
    Set(ByVal value As Short)
      MnChkGrundKorr = value
    End Set
  End Property

  Property DatenFarb() As HandleRezDsetFarb
    Get
      DatenFarb = MnDatenFarb
    End Get
    Set(ByVal AcDatenFarb As HandleRezDsetFarb)
      MnDatenFarb = AcDatenFarb
    End Set
  End Property

  '
  Property GrpRwerte() As RefValuesGrp
    Get     'R-Werte für Berechnung ("R")
      GrpRwerte = MnGrpRwerte
    End Get
    Set(ByVal Value As RefValuesGrp)
      MnGrpRwerte = Value
    End Set
  End Property

  '

  '
  Property TDBFar() As C1TrueDBGrid
    Get
      TDBFar = MnTDBFar
    End Get
    Set(ByVal Value As C1TrueDBGrid)
      MnTDBFar = Value
    End Set
  End Property
  '
  Property TDBDropFar() As C1TrueDBDropdown
    Get
      TDBDropFar = MnTDBDropFar
    End Get
    Set(ByVal Value As C1TrueDBDropdown)
      MnTDBDropFar = Value
    End Set
  End Property '
  '
  '
  '
  Property TDBDropPre() As C1TrueDBDropdown
    Get
      TDBDropPre = MnTDBDropPre
    End Get
    Set(ByVal Value As C1TrueDBDropdown)
      MnTDBDropPre = Value
    End Set
  End Property
  '
  '
  '
  '
  '
  Property TDBDropPrb() As C1TrueDBDropdown
    Get
      TDBDropPrb = MnTDBDropPrb
    End Get
    Set(ByVal Value As C1TrueDBDropdown)
      MnTDBDropPrb = Value
    End Set
  End Property

  '
  '
  '
  '
  '
  Property TDBDropPro() As C1TrueDBDropdown
    Get
      TDBDropPro = MnTDBDropPro
    End Get
    Set(ByVal Value As C1TrueDBDropdown)
      MnTDBDropPro = Value
    End Set
  End Property
  '
  '
  '
  '
  Property Picauf() As HandlePictures
    Get
      Picauf = MnPicAuf
    End Get
    Set(ByVal Value As HandlePictures)
      MnPicAuf = Value
    End Set
  End Property
  '
  '
  '
  '
  '
  Property CalcRezept() As RezeptBerechnung
    Get
      CalcRezept = MnCalcRezept
    End Get
    Set(ByVal Value As RezeptBerechnung)
      MnCalcRezept = Value
    End Set
  End Property
  '
  '
  '
  '
  '
  Property chkVOL() As CheckBox
    Get
      chkVOL = MnChkVOL
    End Get
    Set(ByVal Value As CheckBox)
      MnChkVOL = Value
    End Set
  End Property
  '
  '
  '
  '
  '
  '
  '
  '
  Property lstFAR() As ListBox
    Get
      lstFAR = MnlstFAR
    End Get
    Set(ByVal Value As ListBox)
      MnlstFAR = Value
    End Set
  End Property



  Property cboMNG() As ComboBox
    Get
      cboMNG = MncboMNG
    End Get
    Set(ByVal Value As ComboBox)
      MncboMNG = Value
    End Set
  End Property
  Property cboPRO_0() As ComboBox
    Get
      cboPRO_0 = MncboPRO_0
    End Get
    Set(ByVal Value As ComboBox)
      MncboPRO_0 = Value
    End Set
  End Property
  Property cboPRO_1() As ComboBox
    Get
      cboPRO_1 = MncboPRO_1
    End Get
    Set(ByVal Value As ComboBox)
      MncboPRO_1 = Value
    End Set
  End Property    '
  '
  Property txtMNG_0() As TextBox
    Get
      txtMNG_0 = MnTxtMNG_0
    End Get
    Set(ByVal Value As TextBox)
      MnTxtMNG_0 = Value
    End Set
  End Property    '
  Property txtMNG_1() As TextBox
    Get
      txtMNG_1 = MnTxtMNG_1
    End Get
    Set(ByVal Value As TextBox)
      MnTxtMNG_1 = Value
    End Set
  End Property

  Property txtPRO_0() As TextBox
    Get
      txtPRO_0 = MnTxtPRO_0
    End Get
    Set(ByVal Value As TextBox)
      MnTxtPRO_0 = Value
    End Set
  End Property
  Property txtPRO_1() As TextBox
    Get
      txtPRO_1 = MnTxtPRO_1
    End Get
    Set(ByVal Value As TextBox)
      MnTxtPRO_1 = Value
    End Set
  End Property
  '' 
  '
  '
  '
  '
  '
  '
  '
  '
  Property txtFooter() As TextBox
    Get
      txtFooter = MnTxtFooter
    End Get
    Set(ByVal Value As TextBox)
      MnTxtFooter = Value
    End Set
  End Property
  '
  '
 
  '
  '
  '
  '
  '
  '
  ReadOnly Property Umr() As RezeptUmrechnung
    Get
      Umr = MnUmr
    End Get
  End Property
  '
  '
  Property PicGraphic() As HandleRezGrafik
    Get
      PicGraphic = MnPicGraphic
    End Get
    Set(ByVal Value As HandleRezGrafik)
      MnPicGraphic = Value
    End Set
  End Property
  '
  '
  '
  Property AllRezepte() As RecipesGrp
    Get
      AllRezepte = MnAllRezepte

    End Get
    Set(ByVal Value As RecipesGrp)
      MnAllRezepte = Value
      'MnUmr.Rezpte = MnAllRezepte
    End Set
  End Property    '
  '
  '
  '
  '


  Private Function CharHilf(ByRef wert As Single) As String
    If Abs(wert + 999.99) > 0.001 Then
      CharHilf = Format(wert, "########.000")
    Else
      CharHilf = "******"
    End If

  End Function


  '




  '
  Sub TDBFarGridRezept(ByRef ier As Short)
    Dim i As Integer
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
    MnUmr.Logwarn = BitInt(28, 29, MenueParam.User.Writ)
    KbProb = MenueParam.Misch.Bprob
    MnTDBFar.AllowAddNew = True
    MnTDBFar.AllowDelete = True
    MnTDBFar.AllowRowSelect = False
    MnTDBFar.AllowSort = False
    MnTxtFooter.Visible = True
    MnTDBFar.Splits(0).ColumnCaptionHeight = 34
    MnTDBFar.Splits(0).ExtendRightColumn = True
    MnTDBFar.Splits(0).FooterStyle.BackColor = Color.LightGray
    MnTDBFar.AllowColMove = False
    MnTDBFar.AllowColSelect = False
    ' MnTDBFar.Splits(0).DisplayColumns(0).
    ' MnTDBFar.Columns(0).
    '
    '
    '
    '
    ier = 0
    MaxCol = TblRezept.Columns.Count
    MaxRow = 0
    '
    '
    '
    '
    '
    '

    '
    '
    '
    If Not IsNothing(MnlstFAR) Then
      MnlstFAR.DataSource = DatenFarb.TblFarb
      MnlstFAR.DisplayMember = "FARBM_NAME"
      MnlstFAR.ValueMember = "FARBM_ID"
    End If

    TblRezept.Constraints.Clear()
    TblRezept.Columns.Clear()
    TblRezept.Rows.Clear()
    MnTDBFar.ClearFields()
    TblRezept.AcceptChanges()
    '

    '
    '
    '
    '
    'Tabelle TblRezept aufbauen
    '
    '
    '
    'Farbmittel-ID
    '
    TblRezept.Columns.Add("FID", GetType(Integer))
    TblRezept.Columns("FID").Unique = True
    TblRezept.PrimaryKey = New DataColumn() {TblRezept.Columns("FID")}
    TblRezept.Columns("FID").DefaultValue = -1 'MnTDBFar.Columns.Add(New C1DataColumn("", "FID", GetType(Integer)))

    '
    '
    '
    '
    '
    '
    'Programmstart (PRG)(1)
    '
    '
    TblRezept.Columns.Add("PRG", GetType(Boolean))
    TblRezept.Columns("PRG").DefaultValue = False
    '
    '
    '
    '
    '
    'Farbmittelmenge(2)
    '
    '
    '
    TblRezept.Columns.Add("MEN", GetType(Single))
    TblRezept.Columns("MEN").AllowDBNull = False
    TblRezept.Columns("MEN").DefaultValue = 0
    '

    '
    'Alte Farbmittelmenge(3)
    '
    '
    '
    TblRezept.Columns.Add("ALT", GetType(Single))
    TblRezept.Columns("ALT").AllowDBNull = False
    TblRezept.Columns("ALT").DefaultValue = 0



    '
    '
    '
    'Zuwaage(4)
    '
    '
    '
    TblRezept.Columns.Add("ZUW", GetType(Single))
    TblRezept.Columns("ZUW").AllowDBNull = False
    TblRezept.Columns("ZUW").DefaultValue = 0
    '
    '
    '
    'Zuwaage + alte Menge(5)
    '
    '
    '
    TblRezept.Columns.Add("MZU", GetType(Single))
    TblRezept.Columns("MZU").AllowDBNull = False
    TblRezept.Columns("MZU").DefaultValue = 0
    '
    '
    '
    '
    '
    '
    '
    'Farbstärke(6)
    '
    '
    '
    '
    '
   
    '
    '
    TblRezept.Columns.Add("FST", GetType(Single))
    TblRezept.Columns("FST").AllowDBNull = False
    TblRezept.Columns("FST").DefaultValue = 100
    '
    '
    '
    '
    '
    'Farbmittelpreise(7)
    '
    '
    '
    '
    '
    MnTDBDropPre.Size = New Size(50, 100)
    MnTDBDropPre.Columns.Clear()
    MnTDBDropPre.Columns.Add(New C1DataColumn("", "FARBM_PREIS", GetType(Single)))
    MnTDBDropPre.SetDataBinding(DatenFarb.DvPre, "", False)
    MnTDBDropPre.DisplayColumns("FARBM_ID").Width = 0
    MnTDBDropPre.DisplayColumns("FARBM_IRPA").Width = 0
    MnTDBDropPre.DisplayColumns("FARBM_PREIS").Width = 50
    '
    '
    MnTDBDropPre.ValueMember = "FARBM_PREIS"
    MnTDBDropPre.DisplayMember = "FARBM_PREIS"
    MnTDBDropPre.ValueTranslate = False
    MnTDBDropPre.ColumnHeaders = False
    MnTDBDropPre.ColumnFooters = False
    MnTDBDropPre.ValueTranslate = True
    MnTDBDropPre.DisplayColumns("FARBM_ID").Visible = False
    MnTDBDropPre.DisplayColumns("FARBM_IRPA").Visible = False
    MnTDBDropPre.DisplayColumns("FARBM_PREIS").Visible = True
    MnTDBDropPre.Columns("FARBM_ID").Caption = ""
    MnTDBDropPre.Columns("FARBM_IRPA").Caption = ""
    MnTDBDropPre.Columns("FARBM_PREIS").Caption = ""
    MnTDBDropPre.DisplayColumns("FARBM_PREIS").Style.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Center
    MnTDBDropPre.HScrollBar.Style = ScrollBarStyleEnum.None
    MnTDBDropPre.VScrollBar.Style = ScrollBarStyleEnum.None
    MnTDBDropPre.Caption = ""
    MnTDBDropPre.ExtendRightColumn = True
    '
    '
    TblRezept.Columns.Add("PRE", GetType(Single))
    TblRezept.Columns("PRE").AllowDBNull = False
    TblRezept.Columns("PRE").DefaultValue = 0
    '
    '
    '
    '
    'Prozentigkeiten(8)
    '
    '
    '
    '
    MnTDBDropPro.Size = New Size(50, 100)
    MnTDBDropPro.Columns.Clear()
    MnTDBDropPro.Columns.Add(New C1DataColumn("", "FARBM_PROZ", GetType(Single)))
    MnTDBDropPro.SetDataBinding(DatenFarb.DvPro, "", False)
    MnTDBDropPro.DisplayColumns("FARBM_ID").Width = 0
    MnTDBDropPro.DisplayColumns("FARBM_IRFA").Width = 0
    MnTDBDropPro.DisplayColumns("FARBM_PROZ").Width = 50
    '
    '
    MnTDBDropPro.ValueMember = "FARBM_PROZ"
    MnTDBDropPro.DisplayMember = "FARBM_PROZ"
    MnTDBDropPro.ValueTranslate = False
    MnTDBDropPro.ColumnHeaders = False
    MnTDBDropPro.ColumnFooters = False
    MnTDBDropPro.ValueTranslate = True
    MnTDBDropPro.DisplayColumns("FARBM_ID").Visible = False
    MnTDBDropPro.DisplayColumns("FARBM_IRFA").Visible = False
    MnTDBDropPro.DisplayColumns("FARBM_PROZ").Visible = True
    MnTDBDropPro.Columns("FARBM_ID").Caption = ""
    MnTDBDropPro.Columns("FARBM_IRFA").Caption = ""
    MnTDBDropPro.Columns("FARBM_PROZ").Caption = ""
    MnTDBDropPro.DisplayColumns("FARBM_PROZ").Style.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Center
    MnTDBDropPro.HScrollBar.Style = ScrollBarStyleEnum.None
    MnTDBDropPro.VScrollBar.Style = ScrollBarStyleEnum.None
    MnTDBDropPro.Caption = ""
    MnTDBDropPro.ExtendRightColumn = True
    '
    '
    '
    TblRezept.Columns.Add("PRO", GetType(Single))
    TblRezept.Columns("PRO").AllowDBNull = False
    TblRezept.Columns("PRO").DefaultValue = 100
    '
    '
    '
    '
    'Bindemittel-Prozentigkeiten(9)
    '
    '
    '
    '
    MnTDBDropPrb.Size = New Size(50, 100)
    MnTDBDropPrb.Columns.Clear()
    MnTDBDropPrb.Columns.Add(New C1DataColumn("", "FARBM_PROB", GetType(Single)))
    MnTDBDropPrb.SetDataBinding(DatenFarb.DvPrb, "", False)
    MnTDBDropPrb.DisplayColumns("FARBM_ID").Width = 0
    MnTDBDropPrb.DisplayColumns("FARBM_IRBA").Width = 0
    MnTDBDropPrb.DisplayColumns("FARBM_PROB").Width = 50
    '
    '
    MnTDBDropPrb.ValueMember = "FARBM_PROB"
    MnTDBDropPrb.DisplayMember = "FARBM_PROB"
    MnTDBDropPrb.ValueTranslate = False
    MnTDBDropPrb.ColumnHeaders = False
    MnTDBDropPrb.ColumnFooters = False
    MnTDBDropPrb.ValueTranslate = True
    MnTDBDropPrb.DisplayColumns("FARBM_ID").Visible = False
    MnTDBDropPrb.DisplayColumns("FARBM_IRBA").Visible = False
    MnTDBDropPrb.DisplayColumns("FARBM_PROB").Visible = True
    MnTDBDropPrb.Columns("FARBM_ID").Caption = ""
    MnTDBDropPrb.Columns("FARBM_IRBA").Caption = ""
    MnTDBDropPrb.Columns("FARBM_PROB").Caption = ""
    MnTDBDropPrb.DisplayColumns("FARBM_PROB").Style.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Center
    MnTDBDropPrb.HScrollBar.Style = ScrollBarStyleEnum.None
    MnTDBDropPrb.VScrollBar.Style = ScrollBarStyleEnum.None
    MnTDBDropPrb.Caption = ""
    MnTDBDropPrb.ExtendRightColumn = True
    '
    '
    '
    '
    TblRezept.Columns.Add("PRB", GetType(Single))
    TblRezept.Columns("PRB").AllowDBNull = False
    TblRezept.Columns("PRB").DefaultValue = 100
    '
    '
    '
    '
    '
    '
    'Töpfe(10)
    '
    '
    '
    '
    '
    '
    '
    '
    With ColumnsKTO.ValueItems
      .Values.Clear()
      .MaxComboItems = 5
      .Presentation = C1.Win.C1TrueDBGrid.PresentationEnum.ComboBox
      .Translate = False
      .Validate = True
      .Values.Add(New ValueItem(" ", " "))
      .Values.Add(New ValueItem(" ", "-"))
      .Values.Add(New ValueItem(" ", "1"))
      .Values.Add(New ValueItem(" ", "2"))
      .Values.Add(New ValueItem(" ", "3"))
      .Values.Add(New ValueItem(" ", "4"))
      .Values.Add(New ValueItem(" ", "5"))
      .Values.Add(New ValueItem(" ", "6"))
      .Values.Add(New ValueItem(" ", "7"))
      .Values.Add(New ValueItem(" ", "8"))
      .Values.Add(New ValueItem(" ", "9"))
      .Values.Add(New ValueItem(" ", "A"))
      .Values.Add(New ValueItem(" ", "B"))
      .Values.Add(New ValueItem(" ", "C"))
      .Values.Add(New ValueItem(" ", "D"))
      .Values.Add(New ValueItem(" ", "E"))
      .Values.Add(New ValueItem(" ", "F"))
      .Values.Add(New ValueItem(" ", "G"))
      .Values.Add(New ValueItem(" ", "H"))
      .Values.Add(New ValueItem(" ", "I"))
      .Values.Add(New ValueItem(" ", "J"))
      .Values.Add(New ValueItem(" ", "K"))
      .Values.Add(New ValueItem(" ", "L"))
      .Values.Add(New ValueItem(" ", "M"))
      .Values.Add(New ValueItem(" ", "N"))
      .Values.Add(New ValueItem(" ", "O"))
      .Values.Add(New ValueItem(" ", "P"))
      .Values.Add(New ValueItem(" ", "Q"))
      .Values.Add(New ValueItem(" ", "R"))
      .Values.Add(New ValueItem(" ", "S"))
      .Values.Add(New ValueItem(" ", "T"))
      .Values.Add(New ValueItem(" ", "U"))
      .Values.Add(New ValueItem(" ", "V"))
      .Values.Add(New ValueItem(" ", "W"))
      .Values.Add(New ValueItem(" ", "X"))
      .Values.Add(New ValueItem(" ", "Y"))
      .Values.Add(New ValueItem(" ", "Z"))
      .Values.Add(New ValueItem(" ", "("))
      .Values.Add(New ValueItem(" ", ")"))
      .Values.Add(New ValueItem(" ", "["))
      .Values.Add(New ValueItem(" ", "]"))
      .Values.Add(New ValueItem(" ", "{"))
      .Values.Add(New ValueItem(" ", "}"))
      .Values.Add(New ValueItem(" ", "<"))
      .Values.Add(New ValueItem(" ", ">"))
      .DefaultItem = 0
    End With
    For Each ValItem In ColumnsKTO.ValueItems.Values
      ValItem.Value = ValItem.DisplayValue
    Next

    TblRezept.Columns.Add("KTO", GetType(String))
    TblRezept.Columns("KTO").AllowDBNull = False
    TblRezept.Columns("KTO").DefaultValue = " "
    ''
    '
    'Operatoren(11)
    '
    With ColumnsOPR.ValueItems
      .Values.Clear()
      .MaxComboItems = 5
      .Presentation = C1.Win.C1TrueDBGrid.PresentationEnum.ComboBox
      .Translate = False
      .Validate = True
      .Values.Add(New ValueItem(" ", " "))
      .Values.Add(New ValueItem(" ", "="))
      .Values.Add(New ValueItem(" ", "<"))
      .Values.Add(New ValueItem(" ", ">"))
      .Values.Add(New ValueItem(" ", "*"))
      .Values.Add(New ValueItem(" ", "+"))
      .Values.Add(New ValueItem(" ", "/"))
      .Values.Add(New ValueItem(" ", ":"))
      .Values.Add(New ValueItem(" ", "\"))
      .Values.Add(New ValueItem(" ", "~"))
      .Values.Add(New ValueItem(" ", "1"))
      .Values.Add(New ValueItem(" ", "2"))
      .Values.Add(New ValueItem(" ", "3"))
      .Values.Add(New ValueItem(" ", "4"))
      .Values.Add(New ValueItem(" ", "5"))
      .Values.Add(New ValueItem(" ", "6"))
      .Values.Add(New ValueItem(" ", "7"))
      .Values.Add(New ValueItem(" ", "8"))
      .Values.Add(New ValueItem(" ", "9"))
      .Values.Add(New ValueItem(" ", "A"))
      .Values.Add(New ValueItem(" ", "B"))
      .Values.Add(New ValueItem(" ", "C"))
      .Values.Add(New ValueItem(" ", "D"))
      .Values.Add(New ValueItem(" ", "E"))
      .Values.Add(New ValueItem(" ", "F"))
      .Values.Add(New ValueItem(" ", "G"))
      .Values.Add(New ValueItem(" ", "H"))
      .Values.Add(New ValueItem(" ", "I"))
      .Values.Add(New ValueItem(" ", "J"))
      .Values.Add(New ValueItem(" ", "K"))
      .Values.Add(New ValueItem(" ", "L"))
      .Values.Add(New ValueItem(" ", "M"))
      .Values.Add(New ValueItem(" ", "N"))
      .Values.Add(New ValueItem(" ", "O"))
      .Values.Add(New ValueItem(" ", "P"))
      .Values.Add(New ValueItem(" ", "Q"))
      .Values.Add(New ValueItem(" ", "R"))
      .Values.Add(New ValueItem(" ", "S"))
      .Values.Add(New ValueItem(" ", "T"))
      .Values.Add(New ValueItem(" ", "U"))
      .Values.Add(New ValueItem(" ", "V"))
      .Values.Add(New ValueItem(" ", "W"))
      .Values.Add(New ValueItem(" ", "X"))
      .Values.Add(New ValueItem(" ", "Y"))
      .Values.Add(New ValueItem(" ", "Z"))
      .DefaultItem = 0
    End With
    For Each ValItem In ColumnsOPR.ValueItems.Values
      ValItem.Value = ValItem.DisplayValue
    Next
    TblRezept.Columns.Add("OPR", GetType(String))
    TblRezept.Columns("OPR").AllowDBNull = False
    TblRezept.Columns("OPR").DefaultValue = " "

    '
    '
    '
    '
    'Limitierungsmengen(12)
    '
    '
    '
    TblRezept.Columns.Add("LIM", GetType(Single))
    TblRezept.Columns("LIM").AllowDBNull = False
    TblRezept.Columns("LIM").DefaultValue = 0
    '
    '
    '
    '
    '
    '
    '
    '
    'Standardfestlegungen
    '
    '
    MnTDBFar.SetDataBinding(ConnRezept, "", False)
    '

    '
    '
    '
    '
    MnTDBFar.Columns("PRG").ValueItems.Presentation = C1.Win.C1TrueDBGrid.PresentationEnum.CheckBox
    MnTDBFar.Columns("PRG").ValueItems.Translate = False
    MnTDBFar.Columns("KTO").ValueItems = ColumnsKTO.ValueItems
    MnTDBFar.Columns("OPR").ValueItems = ColumnsOPR.ValueItems
    '
    '
    MnTDBFar.AllowSort = False
    If BitWrt(30, MenueParam.User.Writ) Then
      MnTDBFar.FetchRowStyles = True
    Else
      MnTDBFar.FetchRowStyles = False
    End If
    For i = 0 To MnTDBFar.Columns.Count - 1
      MnTDBFar.Columns(i).SortDirection = SortDirEnum.None
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
    'Öffnen des Layoutfiles
    '
    '
    '
    '

    Try
      If BitWrt(17, MenueParam.User.Drum) Then
        '
        'Layout mit gespeichertem Layout überschreiben
        '
        '
        'MnTDBFar.LoadLayout(GridFileName)
        'MnTDBFar.ClearFields()
      End If
      LayoutChanged = False

    Catch
    End Try

  End Sub
 
  ReadOnly Property Name As String
    Get
      Name = KeyNam
    End Get
  End Property
  Sub NewKeynam(ByVal Value As String)
    Dim i As Short
    KeyNam = Value
    '
    MnTDBFar.ColumnFooters = False
    MnTDBFar.AllowSort = False
    MnTDBFar.Splits(0).AlternatingRowStyle = True
    MnTDBFar.EvenRowStyle.BackColor = Color.White
    MnTDBFar.OddRowStyle.BackColor = Color.WhiteSmoke

    '
    MnTDBFar.AllowAddNew = True
    MnTDBFar.AllowUpdate = True
    MnTDBFar.AllowDelete = True
    MnTDBFar.AllowRowSelect = True
    Select Case KeyNam
      Case "MEN"
        MnTDBFar.AllowAddNew = False
        MnTDBFar.AllowDelete = False
        MnTDBFar.AllowRowSelect = False
        MnTDBFar.ColumnFooters = True
      Case "MNG"
      Case "SOR"
        MnTDBFar.ColumnFooters = False
      Case "BAS"
        MnTDBFar.ColumnFooters = True
      Case "KOR"
        MnTDBFar.AllowAddNew = False
        MnTDBFar.AllowUpdate = False
        MnTDBFar.AllowDelete = False
        MnTDBFar.ColumnFooters = True
        MnTDBFar.AllowRowSelect = False
      Case "KOO", "KOA"
        MnTDBFar.ColumnFooters = False
        MnTDBFar.AllowDelete = False
      Case "UMR"
        MnTDBFar.AllowAddNew = False
        MnTDBFar.AllowDelete = False
        MnTDBFar.ColumnFooters = True
        MnTDBFar.AllowRowSelect = False
      Case "NEU"
        MnTDBFar.ColumnFooters = False
      Case "COL"
        MnTDBFar.AllowAddNew = False
        MnTDBFar.AllowUpdate = False
        MnTDBFar.AllowDelete = False
        MnTDBFar.ColumnFooters = True
        MnTDBFar.AllowRowSelect = False
      Case "RZP"
        MnTDBFar.AllowAddNew = False
        MnTDBFar.AllowUpdate = False
        MnTDBFar.AllowDelete = False
        MnTDBFar.AllowRowSelect = False
        MnTDBFar.ColumnFooters = True
        MnTxtFooter.ReadOnly = True

      Case "HLF"
      Case "ZEI"
        MnTDBFar.AllowAddNew = False
        MnTDBFar.AllowUpdate = False
        MnTDBFar.AllowDelete = False
        MnTDBFar.AllowRowSelect = False
    End Select
    txtFooter.Visible = MnTDBFar.ColumnFooters
    txtFooter.BackColor = MnTDBFar.Splits(0).FooterStyle.BackColor
   
    MnTDBDropFar.Columns.Clear()
    MnTDBDropFar.Columns.Add(New C1DataColumn("", "FARBM_ID", GetType(Integer)))
    MnTDBDropFar.Columns.Add(New C1DataColumn("", "FARBM_NAME", GetType(String)))
    MnTDBDropFar.SetDataBinding(MnDatenFarb.TblFarb, "", True)
    MnTDBDropFar.Columns(0).DataField = "FARBM_ID"
    MnTDBDropFar.ValueMember = "FARBM_ID"
    MnTDBDropFar.DisplayMember = "FARBM_NAME"
    MnTDBDropFar.DisplayColumns("FARBM_ID").Visible = False
    MnTDBDropFar.DisplayColumns("FARBM_NAME").Visible = True
    MnTDBDropFar.ValueTranslate = True
    MnTDBDropFar.DisplayColumns("FARBM_ID").Locked = True
    MnTDBDropFar.DisplayColumns("FARBM_ID").Width = 0
    MnTDBDropFar.ColumnHeaders = False
    MnTDBDropFar.HScrollBar.Style = ScrollBarStyleEnum.None
    MnTDBDropFar.DisplayColumns("FARBM_NAME").Style.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Near
    MnTDBDropFar.VScrollBar.Style = ScrollBarStyleEnum.Automatic
    MnTDBDropFar.Size = New Size(30, 100)
    '
    '
    '
    '
    MnTDBFar.Columns("FID").DropDown = MnTDBDropFar
    For i = 0 To MnTDBFar.Columns.Count - 1
      KeyIndex = MnTDBFar.Columns(i).DataField
      With MnTDBFar.Splits(0).DisplayColumns(KeyIndex)
        Select Case KeyIndex

          Case "FID"
            '
            '
            '
            'Farbmittel-ID/Name
            '

            .Visible = True
            .DataColumn.Caption = Texxt(815)
            .Width = Weite(1)

            .Button = True
            .Style.HorizontalAlignment = AlignHorzEnum.Near
            Select Case KeyNam
              Case "NEU", "SOR", "BAS", "KOR", "KOO", "COL", "HLF", "KOA"
                .Button = True
                .Locked = False
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
                .DataColumn.FooterText = Texxt(825)
              Case "RZP", "MEN"
                .Button = False
                .Locked = True
                .DataColumn.FooterText = Texxt(825)
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case "ZEI"
                .Button = False
                .Locked = True
              Case "MNG"
                .Button = True
                .Locked = False
              Case "UMR"
                .Button = False
                .Locked = True
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
                .DataColumn.FooterText = Texxt(825)
              Case Else
            End Select
            '
            '
            '
            '
          Case "PRG"
            'Programmstart 
            .Width = Weite(3)
            .Visible = False
            .DataColumn.Caption = "PRG"
            Select Case KeyNam
              Case "ZEI", "UMR", "NEU", "MNG", "MEN", "SOR", "RZP", "COL", "HLF", "KOR", "KOA"
                .Visible = False
                .Button = False
                .Locked = True
              Case "BAS", "KOO"
                .Visible = True
                .Button = False
                .Locked = False
              Case Else
            End Select
          Case "MEN"
            '
            'Menge
            '
            .Width = Weite(4)
            '
            '
            Select Case KeyNam
              Case "ZEI", "MNG", "NEU", "SOR", "KOR", "KOO", "BAS", "COL", "HLF", "KOA"
                .Visible = True
                .Button = False
                .Locked = False
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case "RZP", "UMR", "MEN"
                .Visible = True
                .Button = False
                .Locked = True
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case Else
            End Select
          Case "ALT"
            '
            'Menge(alt)
            '
            'Menge alt
            KeyHilf(0) = KeyIndex
            .Width = Weite(4)
            .Locked = True
            .DataColumn.Caption = Texxt(826)
            .Visible = False
            Select Case KeyNam
              Case "ZEI", "UMR", "NEU", "MNG", "MEN", "SOR", "BAS", "RZP", "COL", "HLF"
                .Visible = False
              Case "KOR", "KOO", "KOA"
                .Visible = True
                .Button = False
                .Locked = True
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case Else
            End Select
          Case "ZUW"
            '
            'Zuwaage
            '
            KeyHilf(2) = KeyIndex

            .Width = Weite(4)
            .Locked = True
            .DataColumn.Caption = Texxt(827)
            .Visible = False
            Select Case KeyNam
              Case "ZEI", "UMR", "NEU", "MNG", "MEN", "SOR", "BAS", "RZP", "COL", "HLF", "KOA"
                .Visible = False
              Case "KOR", "KOO"
                .Visible = True
                .Button = False
                .Locked = True
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case Else
            End Select
          Case "MZU"
            '
            'Zuwaage + Menge(alt)
            '
            KeyHilf(1) = KeyIndex

            .Width = Weite(4)
            .Locked = True
            .DataColumn.Caption = Texxt(836)
            .Visible = False
            Select Case KeyNam
              Case "ZEI", "UMR", "NEU", "MNG", "MEN", "SOR", "BAS", "RZP", "COL", "HLF", "KOA"
                .Visible = False
              Case "KOR", "KOO"
                .Visible = True
                .Button = False
                .Locked = True
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case Else
            End Select
          Case "FST"
            '
            '
            'Farbstärke
            '
            '
            .Locked = True
            .Width = Weite(2)
            .DataColumn.Caption = TexKt(21014)
            .Visible = False
            .DataColumn.NumberFormat = "###.00"
            Select Case KeyNam
              Case "ZEI"
                .Visible = False
              Case "MEN"
                .Visible = True
                .Button = False
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case "MNG"
                .Visible = False
                .Button = False
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case "UMR", "NEU", "SOR", "KOR", "BAS", "RZP", "KOO", "COL", "HLF", "KOA"
                .Visible = True
                .Button = False
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case Else
            End Select
          Case "PRE"
            '
            '
            'FabmittelPreis
            '
            'Preis
            .Locked = False
            .Width = Weite(2)
            .DataColumn.Caption = Texxt(822)
            .Visible = False
            .DataColumn.DropDown = MnTDBDropPre
            .DataColumn.NumberFormat = "###.00"

            Select Case KeyNam
              Case "ZEI"
                .Visible = False
              Case "MEN"
                .Visible = True
                .Locked = False
                .Button = True
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case "MNG"
                .Visible = True
                .Locked = False
                .Button = True
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case "UMR", "NEU", "SOR", "KOR", "BAS", "RZP", "KOO", "COL", "HLF", "KOA"
                .Visible = True
                .Button = True
                .Locked = False
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case Else
            End Select

          Case "PRO"
            '
            'FabmittelProzentigkeit
            '
            'Prozentigkeit
            .Locked = False
            .Width = Weite(2)
            .DataColumn.Caption = Texxt(820)
            .Visible = False
            .DataColumn.DropDown = MnTDBDropPro
            .DataColumn.NumberFormat = "###.00"
            Select Case KeyNam
              Case "ZEI"
                .Visible = False
              Case "MNG", "MEN"
                .Visible = True
                .Locked = False
                .Button = True
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case "UMR", "NEU", "SOR", "KOR", "BAS", "RZP", "KOO", "COL", "HLF", "KOA"
                .Visible = True
                .Button = True
                .Locked = False
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case Else
            End Select

          Case "PRB"
            '
            '
            'FabmittelBindemittelProzentigkeit
            '
            'Bindemittel-Prozentigkeit
            .Locked = False
            .Width = Weite(2)
            .DataColumn.Caption = Texxt(821)
            .Visible = False
            .DataColumn.DropDown = MnTDBDropPrb
            .DataColumn.NumberFormat = "###.00"

            Select Case KeyNam
              Case "ZEI"
                .Visible = False
              Case "MNG", "MEN"
                .Visible = True
                .Locked = False
                .Button = True
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case "UMR", "NEU", "SOR", "BAS", "RZP", "KOR", "KOO", "COL", "HLF", "KOA"
                .Visible = True
                .Button = True
                .Locked = False
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case Else
            End Select

          Case "KTO"
            '
            'Töpfe
            '
            '
            'Topfangabe
            .Locked = False
            .Width = Weite(3)
            .DataColumn.Caption = Texxt(819)
            .Visible = False
            Select Case KeyNam
              Case "SOR", "BAS"
                .Visible = True
                .Locked = False
                .Button = True
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case "UMR", "NEU", "RZP", "KOR", "KOO", "ZEI", "MNG", "MEN", "COL", "HLF", "KOA"
                .Visible = False
              Case Else
            End Select

          Case "OPR"
            '
            'Operator
            '
            'Operator
            .Locked = False
            .Width = Weite(3)
            .DataColumn.Caption = Texxt(817)
            .Visible = False
            Select Case KeyNam
              Case "SOR", "KOR", "KOO", "BAS", "NEU", "MEN", "KOA"
                .Visible = True
                .Locked = False
                .Button = True
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case "UMR", "RZP", "MNG", "ZEI", "COL", "HLF"
                .Visible = False
              Case Else
            End Select
          Case "LIM"
            '
            '
            'Limitierungsmenge
            '
            '
            'Limitierungsmenge
            .Width = Weite(4)
            .Visible = False
            Select Case KeyNam
              Case "SOR", "BAS", "KOR", "KOO", "NEU", "MEN", "KOA"
                .Visible = True
                .Locked = False
                .Button = False
                .FooterStyle.HorizontalAlignment = C1.Win.C1TrueDBGrid.AlignHorzEnum.Far
              Case "UMR", "RZP", "MNG", "ZEI", "COL", "HLF"
                .Visible = False
              Case Else
            End Select
          Case "FST"
          Case "BEL"
          Case "FRM"
        End Select
      End With
    Next i

    Call DropWidth()

    Call FooterBounds()










  End Sub

  Sub TDBFarTextColumn(ByVal JNF As Integer, ByVal JNM As Integer)
    If IsNothing(MnTDBFar) OrElse MnTDBFar.Columns.Count = 0 Then Exit Sub
    MnTDBFar.Columns("MEN").Caption = HeaderText(JNF)
    If MnTDBFar.Splits(0).DisplayColumns("LIM").Visible Then
      MnTDBFar.Columns("LIM").Caption = HeadLiText(JNM)
    End If
  End Sub
  '
  '
  '
  Public Sub TDBFarFill(ByRef ier As Integer)
    Dim j As Integer
    Dim KF As Integer
    Dim Irow As Short
    Dim Icol As Short
    Dim ivol As Short
    Dim KeyIndex As String
    Dim RezRow As DataRow
    ier = 0
    If MnDatenFarb.TblFarb.Rows.Count = 0 Then
      ier = 2922
      Exit Sub
    End If
    ivol = MnAllRezepte.IVOL
    TDBFARTxtAllSum()
    For j = 0 To 6
      HeaderText(j) = RepTexxt(Texxt(640 + j), Texxt(602 + ivol))
      HeadLiText(j) = RepTexxt(Texxt(640 + j), Texxt(605 + ivol))
    Next j

    Call TDBFarTextColumn(MnAllRezepte.INF, MnAllRezepte.INM)


    'Speichern von MnAllRezepte.Rezepte(KeyNam) nach Gitternetz
    '
    '
    '

    TblRezept.Rows.Clear()
    TblRezept.AcceptChanges()
    'TDBFar.ClearFields()
    'MsgBox(MnTDBFar.Columns(0).CellText(0))
    'MsgBox(MnTDBFar.Columns(0).CellValue(0))

    If KeyNam = "" OrElse MnAllRezepte.Rezepte(KeyNam).KF = 0 Then Exit Sub
    '
    '
    ' 
    KeyIndex = TblRezept.Columns(0).Caption
    For Irow = 0 To MnAllRezepte.Rezepte(KeyNam).KF - 1
      RezRow = TblRezept.NewRow
      RezRow(KeyIndex) = MnAllRezepte.Rezepte(KeyNam)(KeyRe(Irow)).ID
      TblRezept.Rows.Add(RezRow)
    Next
    '
    '
    For Irow = MnAllRezepte.Rezepte(KeyNam).KF - 1 To 0 Step -1

      FaNr = KeyRe(Irow)
      If Not MnAllRezepte.Rezepte(KeyNam).ContainsKey(FaNr) Then
        Continue For
      End If
      KeId = KeyName(MnAllRezepte.Rezepte(KeyNam)(FaNr).ID)
      MnTDBFar.Columns("MEN").NumberFormat = MnAllRezepte.Farben(KeId).Form
      MnTDBFar.Columns("ALT").NumberFormat = MnAllRezepte.Farben(KeId).Form
      MnTDBFar.Columns("ZUW").NumberFormat = MnAllRezepte.Farben(KeId).Form
      MnTDBFar.Columns("MZU").NumberFormat = MnAllRezepte.Farben(KeId).Form
      MnTDBFar.Columns("LIM").NumberFormat = MnAllRezepte.Farben(KeId).Form
      'TblRezept.Rows.Add(TblRezept.NewRow)
      RezRow = TblRezept.Rows(Irow)
      For Icol = 0 To TblRezept.Columns.Count - 1
        KeyIndex = TblRezept.Columns(Icol).Caption
        Select Case KeyIndex
          '
          '
          Case "FID"               'Farbmittel-ID
            '
            '
            For j = 0 To MnDatenFarb.TblFarb.Rows.Count - 1
              If MnAllRezepte.Rezepte(KeyNam)(FaNr).ID = MnDatenFarb.TblFarb.Rows(j)("FARBM_ID") Then
                Exit For
              End If
            Next j
            If j >= MnDatenFarb.TblFarb.Rows.Count Then
              MsgBox(Texxt(801) & ": " & MnAllRezepte.Rezepte(KeyNam)(FaNr).ID & " " & Texxt(2984))
              'ier = 2984

              MnAllRezepte.Rezepte(KeyNam).RemoveFaNr(FaNr)
              KF = MnAllRezepte.Rezepte(KeyNam).KF
              For j = Irow To KF - 1
                MnAllRezepte.Rezepte(KeyNam).AddFaNr(KeyRe(j), MnAllRezepte.Rezepte(KeyNam)(KeyRe(j + 1)))
                MnAllRezepte.Rezepte(KeyNam).RemoveFaNr(KeyRe(j + 1))
              Next j
              Exit For
            End If
            'TblRezept.Rows(Irow)(KeyIndex) = MnAllRezepte.Rezepte(KeyNam)(FaNr).ID
            RezRow(KeyIndex) = MnAllRezepte.Rezepte(KeyNam)(FaNr).ID
            Call CreateView(MnAllRezepte.Rezepte(KeyNam)(FaNr).ID)
            '
            '
          Case "PRG"
            RezRow(KeyIndex) = False
            '
            '
            '
          Case "MEN"
            RezRow(KeyIndex) = MnAllRezepte.Rezepte(KeyNam)(FaNr).BaAmng
          Case "ALT"
            If (KeyNam = "KOR" Or KeyNam = "KOO" Or KeyNam = "KOA") AndAlso MnAllRezepte.Rezepte("ALT").KF > 0 Then
              RezRow(KeyIndex) = MnAllRezepte.Rezepte("ALT")(FaNr).BaAmng
            Else
              RezRow(KeyIndex) = 0.0
            End If
          Case "ZUW"                   'Zuwaage
            If MnAllRezepte.Rezepte.ContainsKey("ZUW") And (KeyNam = "KOR" Or KeyNam = "KOO" Or KeyNam = "KOA") AndAlso MnAllRezepte.Rezepte("ZUW").KF > 0 Then
              If Abs(MnAllRezepte.Rezepte("ZUW")(FaNr).BaAmng + 999.99) > 0.001 Then
                RezRow(KeyIndex) = MnAllRezepte.Rezepte("ZUW")(FaNr).BaAmng
              Else
                RezRow(KeyIndex) = -99999999
              End If
            Else
              RezRow(KeyIndex) = 0.0#
            End If
          Case "MZU"                   'Menge(alt)+Zuwaage
            If MnAllRezepte.Rezepte.ContainsKey("MZU") And (KeyNam = "KOR" Or KeyNam = "KOO" Or KeyNam = "KOA") AndAlso MnAllRezepte.Rezepte("MZU").KF > 0 Then
              If Abs(MnAllRezepte.Rezepte("MZU")(FaNr).BaAmng + 999.99) > 0.001 Then
                RezRow(KeyIndex) = MnAllRezepte.Rezepte("MZU")(FaNr).BaAmng
              Else
                RezRow(KeyIndex) = -99999999
              End If
            Else
              RezRow(KeyIndex) = 0.0#
            End If
          Case "OPR"                   'Operator
            RezRow(KeyIndex) = MnAllRezepte.Farben(KeId).OP
          Case "LIM"                   'Limitierungsmenge
            RezRow(KeyIndex) = MnAllRezepte.Farben(KeId).BoMng
          Case "KTO"                   'Topfangabe
            RezRow(KeyIndex) = MnAllRezepte.Farben(KeId).Kto
          Case "PRE"                   'Preis
            RezRow(KeyIndex) = MnAllRezepte.Farben(KeId).Preis
          Case "PRO"                   'Prozentigkeit
            RezRow(KeyIndex) = MnAllRezepte.Rezepte(KeyNam)(FaNr).Proz
          Case "PRB"                   'Bindemittel-Prozentigkeit
            If KbProb = 1 Then
              RezRow(KeyIndex) = MnAllRezepte.Rezepte(KeyNam)(FaNr).Prob * (1.0# - 0.01 * MnAllRezepte.Rezepte(KeyIndex)(FaNr).Proz)
            Else
              RezRow(KeyIndex) = MnAllRezepte.Rezepte(KeyNam)(FaNr).Prob
            End If
          Case "FST"                   'Farbstärke
            RezRow(KeyIndex) = MnAllRezepte.Farben(KeId).Fst
          Case "BEL"                   'Gewicht für Minimierung
            RezRow(KeyIndex) = MnAllRezepte.Farben(KeId).Bel
          Case "FRM"                   'Ausgabeformat
            RezRow(KeyIndex) = MnAllRezepte.Farben(KeId).Form
            '
        End Select
      Next Icol
      If Icol <> TblRezept.Columns.Count Then
        TblRezept.Rows.RemoveAt(Irow)
      End If
    Next Irow
TDBENDE:
    TblRezept.AcceptChanges()
  End Sub

  Sub TDBFarRezStart(ByRef ier As Integer)
    JNF = MnAllRezepte.INF
    If JNF < 0 Or JNF > 6 Then JNF = 0
    JNM = MnAllRezepte.INM
    If JNM < 0 Or JNM > 2 Then JNM = 0
    Select Case KeyNam
      Case "UMR", "NEU", "SOR"
        If MnAllRezepte.INF > 2 Then
          MnAllRezepte.INF = 0
        End If
    End Select
    FirstRow = MnTDBFar.FirstRow
    '
    '
    '
    '

    Call TDBFarFill(ier)
    If ier <> 0 Then
      Exit Sub
    End If
    If MnAllRezepte.Rezepte.ContainsKey("ZUW") And (KeyNam = "KOR" Or KeyNam = "KOO" Or KeyNam = "KOA") Then
      If MnAllRezepte.Rezepte("ZUW").KF = 0 OrElse Single.IsNaN(MnAllRezepte.Rezepte("ZUW")(0).BaAmng) Then
        MnTDBFar.Splits(0).DisplayColumns("ZUW").Visible = False
        MnTDBFar.Splits(0).DisplayColumns("MZU").Visible = False
      Else
        MnTDBFar.Splits(0).DisplayColumns("ZUW").Visible = True
        MnTDBFar.Splits(0).DisplayColumns("MZU").Visible = True

      End If
    End If

    MaxRow = MnAllRezepte.Rezepte(KeyNam).KF
    Application.DoEvents()
    If TblRezept.Rows.Count > 0 Then
      'MnTDBFar.RefetchRow(0)
    End If
    If FirstRow > 0 Then
      'MnTDBFar.FirstDisplayedScrollingRowIndex = FirstRow
    End If
  End Sub
  Public Sub TDBFARTxtAllSum()
    MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
    MnTxtFooter.Text = CharHilf(Umr.MngINF(KeyNam, MnAllRezepte, ier))
    If MnTDBFar.Columns.Count > 1 Then
      MnTDBFar.Columns(2).FooterText = MnTxtFooter.Text
    End If
    If KeyNam = "KOR" Or KeyNam = "KOO" Or KeyNam = "KOA" Then
      Call TDBFARRezUmrKOR()
    End If
  End Sub
  '
  '
  '
  Sub FarbmNeu(ByVal FaID As Integer, ByVal RowNeu As Integer, ByRef ifeh As Integer)
    Dim i As Short
    Dim k As Short
    Dim ier As Integer
    Dim inf As Integer
    ifeh = 0
    KeId = KeyName(FaID)
    FaNr = KeyRe(RowNeu)
    If RowNeu > MnAllRezepte.Rezepte(KeyNam).KF Then Exit Sub
    For k = 0 To MnAllRezepte.Rezepte(KeyNam).KF - 1
      If FaID = MnAllRezepte.Rezepte(KeyNam)(k).ID Then
        If k = RowNeu Then
          Exit Sub
        Else
          MessageBox.Show(Texxt(2954))
          ifeh = 2954
          Exit Sub
        End If
      End If
    Next k
    If RowNeu = MnAllRezepte.Rezepte(KeyNam).KF Then
      '
      '
      'Neues Farbmittel für Rezept hinzufügen
      '
      '
      MnAllRezepte.Rezepte(KeyNam).AddFaNr(FaNr, New ColorAmount)
      '
      '
      '
      '
      '
      If KeyNam = "KOR" Or KeyNam = "KOO" Or KeyNam = "KOA" Then
        '
        'zusätzliches Farb-/Bindemittel auch für "ALT","ZUW" und "MZU"
        '
        '
        For i = 0 To 2
          MnAllRezepte.Rezepte(KeyHilf(i)).AddFaNr(FaNr, New ColorAmount)
        Next
      End If
    End If
    '
    '
    '

    'Prüfen, auf neue Farb-/Bindemittel in Farben-Liste
    '
    '
    '
    '
    '
    If RowNeu < 0 Or RowNeu > MnAllRezepte.Rezepte(KeyNam).KF Then Exit Sub
    If FaID <> MnAllRezepte.Rezepte(KeyNam)(FaNr).ID Then
      '
      '
      '
      'Prüfen, ob neues Farb-/Bindemittel in Farbmittelliste
      '
      '
      '
      For i = 0 To MnAllRezepte.Farben.FarbCount - 1
        If FaID = MnAllRezepte.Farben(i).ID Then
          Exit For
        End If
      Next
      If i >= MnAllRezepte.Farben.FarbCount Then
        MnAllRezepte.Farben.AddFarb(KeId, New Colorant)
        WithSmng = False
        If KeyNam = "SOR" Then
          WithSmng = True
        End If
        ReWrFarbe.FarReaGrund(FaID, MnAllRezepte.Farben(KeId), WithSmng, MnIer)
        If MnAllRezepte.Farben(KeId).PreCount > 0 Then
          MnAllRezepte.Farben(KeId).Preis = MnAllRezepte.Farben(KeId).Pre(0)
        Else
          MnAllRezepte.Farben(KeId).Preis = 0.0
        End If
      Else
        '
        '
        'Prüfen, ob Farb-/Bindemittel im aktuellen Rezept
        '
        For i = 0 To MnAllRezepte.Rezepte(KeyNam).KF - 1
          If FaID = MnAllRezepte.Rezepte(KeyNam)(i).ID Then
            Exit For
          End If
        Next
        If i < MnAllRezepte.Rezepte(KeyNam).KF Then
          MessageBox.Show(Texxt(2954))
          ifeh = 99
          Exit Sub
        End If
      End If


      '
      '
      '
      '
      '
      '
      'Neues Farb-/Bindemittel übernehmen
      '
      '
      '
      '
      MnAllRezepte.Rezepte(KeyNam)(FaNr).ID = FaID
      MnAllRezepte.Rezepte(KeyNam)(FaNr).BaAmng = MnAllRezepte.Farben(KeId).Smenge
      If MnAllRezepte.Farben(KeId).PrfCount > 0 Then
        MnAllRezepte.Rezepte(KeyNam)(FaNr).Proz = MnAllRezepte.Farben(KeId).Prf(0)
      Else
        MnAllRezepte.Rezepte(KeyNam)(FaNr).Proz = 100
      End If
      If MnAllRezepte.Farben(KeId).PrbCount > 0 Then
        MnAllRezepte.Rezepte(KeyNam)(FaNr).Prob = MnAllRezepte.Farben(KeId).Prb(0)
      Else
        MnAllRezepte.Rezepte(KeyNam)(FaNr).Prob = 100
      End If

      MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
      inf = MnAllRezepte.INF
      '
      'Batchmengen werden berechnet, umzu prüfen, ob negative Werte (z.B. für das Bindemittel) vorkommen
      '
      MnAllRezepte.INF = 0
      MnUmr.MngINQ(KeyNam, MnAllRezepte, ier)
      MnAllRezepte.INF = inf
      If ier <> 0 Then
        '
        'Neues Farbmittel löschen, da z.B. negative Mengen berechnet wurden!!
        '
        '
        If MnAllRezepte.Rezepte(KeyNam).ContainsKey(FaNr) Then
          MnAllRezepte.Rezepte(KeyNam).RemoveFaNr(FaNr)
        End If
        '
        '
        '
        '
        '
        If KeyNam = "KOR" Or KeyNam = "KOO" Or KeyNam = "KOA" Then
          '
          'zusätzliches Farb-/Bindemittel auch für "ALT","ZUW" und "MZU"
          '
          '
          For i = 0 To 2
            If MnAllRezepte.Rezepte(KeyHilf(i)).ContainsKey(FaNr) Then
              MnAllRezepte.Rezepte(KeyHilf(i)).RemoveFaNr(FaNr)
            End If
          Next
        End If
        Exit Sub
      End If
      '
      '

      Select Case KeyNam

        Case "KOR", "KOO", "KOA"
          '
          'zusätzliches Farb-/Bindemittel auch für "ALT","ZUW" und "MZU"
          '
          '
          For i = 0 To 2
            ' MnAllRezepte.Rezepte(KeyHilf(i)).AddFaNr
            MnAllRezepte.Rezepte(KeyHilf(i))(FaNr).ID = FaID
            MnAllRezepte.Rezepte(KeyHilf(i))(FaNr).FaAmng = MnAllRezepte.Farben(KeId).Smenge
            MnAllRezepte.Rezepte(KeyHilf(i))(FaNr).Proz = MnAllRezepte.Farben(KeId).Prf(0)
            MnAllRezepte.Rezepte(KeyHilf(i))(FaNr).Prob = MnAllRezepte.Farben(KeId).Prb(0)
            MnUmr.CalcBamng(KeyHilf(i), MnAllRezepte, ier)
          Next
          'Neue Reflexionswerte für Korrekturrezept berechnen
          '
          '
          CalcRezept.MischKorrektur(16 * MnChkGrundKorr, MenueParam.User.Winkel, "ALT", KeyNam, MnAllRezepte, GrpRwerte, GrundDat, MnIer)
          Picauf.Refresh()
        Case "BAS"

          'Neue Reflexionswerte für Rezept berechnen
          '
          '
          CalcRezept.MischRezept(0, MenueParam.User.Winkel, KeyNam, MnAllRezepte, GrpRwerte, GrundDat, MnIer)
          Picauf.Refresh()
      End Select
    End If

  End Sub
  '
  '
  '
  '

  Private Sub TDBFARRezUmrKOR()
    Dim i As Short
    For i = 0 To 2
      If MnAllRezepte.Rezepte.ContainsKey(KeyHilf(i)) AndAlso MnAllRezepte.Rezepte(KeyHilf(i)).KF > 0 Then
        If KeyHilf(j) <> "" Then
          MnUmr.CalcBamng(KeyHilf(i), MnAllRezepte, ier)
          MnTDBFar.Columns(KeyHilf(i)).FooterText = CharHilf(Umr.MngINF(KeyHilf(i), MnAllRezepte, ier))
        End If
      End If
    Next i
  End Sub

  Sub NormBamng(ByVal Rzkey As String, ByVal Rezpte As RecipesGrp, ByVal NewNormMng As Single, ByRef Ier As Integer)
    '
    '
    '
    '
    'Umrechnen der Normierungsmengen mit Hilfe der neuen Gesamtmenge NewNormMng
    '
    '
    Dim Fakt As Double
    Dim i As Integer
    Dim j As Integer
    Dim MngHlf As Single
    MngHlf = MnUmr.MngINO(Rzkey, Rezpte, Ier)
    If Ier > 0 Then Exit Sub
    If MngHlf < Single.Epsilon Then Exit Sub
    Fakt = NewNormMng / MngHlf
    For i = 0 To Rezpte.Rezepte(Rzkey).KF - 1
      Rezpte.Rezepte(Rzkey)(i).BaAmng = Fakt * Rezpte.Rezepte(Rzkey)(i).BaAmng
    Next i
    Rezpte.Rezepte(Rzkey).UmMng = Fakt * Rezpte.Rezepte(Rzkey).UmMng
    Call MnUmr.CalcFamng(Rzkey, Rezpte, Ier)
    If Rzkey = "KOR" Or Rzkey = "KOO" Or Rzkey = "KOA" Then
      For j = 0 To 2
        If Rezpte.Rezepte(KeyHilf(j)).KF > 0 Then
          For i = 0 To Rezpte.Rezepte(KeyHilf(j)).KF - 1
            If Abs(Rezpte.Rezepte(KeyHilf(j))(i).BaAmng + 999.99) > 0.001 Then
              Rezpte.Rezepte(KeyHilf(j))(i).BaAmng = Fakt * Rezpte.Rezepte(KeyHilf(j))(i).BaAmng
            End If
          Next i
          Rezpte.Rezepte(KeyHilf(j)).UmMng = Fakt * Rezpte.Rezepte(KeyHilf(j)).UmMng
          MnUmr.CalcFamng(KeyHilf(j), Rezpte, Ier)
        End If
      Next j
    End If

  End Sub
  Sub NormFamng(ByVal Rzkey As String, ByVal Rezpte As RecipesGrp, ByVal NewReiMng As Single, ByRef Ier As Integer)
    '
    '
    '
    '
    'Umrechnen der reinen Mengen mit Hilfe der neuen Gesamtmenge NewReiMng
    '
    '
    '
    Dim Fakt As Single
    Dim i As Integer
    Dim MngHlf As Single
    If NewReiMng < Single.Epsilon Then Exit Sub
    If Rezpte.INF > 2 Then Exit Sub
    MngHlf = MnUmr.MngINR(Rzkey, Rezpte, Ier)
    If Ier > 0 Then Exit Sub
    If MngHlf < Single.Epsilon Then Exit Sub
    Fakt = NewReiMng / MngHlf
    Fakt = NewReiMng / MngHlf
    For i = 0 To Rezpte.Rezepte(Rzkey).KF - 1
      Rezpte.Rezepte(Rzkey)(i).FaAmng = Fakt * Rezpte.Rezepte(Rzkey)(i).FaAmng
    Next i
  End Sub




  Sub TDBFarPrint()
    'MnTDBFar.PrintInfo.PageHeader = MnAllRezepte.Rezepte((MnTDBFar.Columns(CShort(MnTDBFar.Tag)).DataField)).Name
    'MnTDBFar.PrintInfo.Print()
  End Sub

  '
  '
  '

  Private Sub ComboFarb_SelectionChangeCommitted(ByVal sender As Object, ByVal e As System.EventArgs) Handles ComboFarb.SelectionChangeCommitted
    Dim FaID As Integer
    Dim Rowindex As Integer
    Dim ifeh As Integer
    If IsNothing(sender.selectedvalue) OrElse Not IsNumeric(sender.selectedvalue) Then Exit Sub
    Rowindex = MnTDBFar.Row
    FaID = sender.selectedvalue
    Call FarbmNeu(FaID, Rowindex, ifeh)
    If ifeh <> 0 Then
      'ret = MnTDBFar.CancelEdit()
      Exit Sub
    End If
    Call TDBFarFill(ier)
    MnPicGraphic.DataChanged = True
  End Sub
  Sub CreateView(ByVal FaID As Integer)
    '
    '
    '
    'Aufbau der DataViews
    '
    '
    '
    '
    DatenFarb.DvPre.RowFilter = "FARBM_ID =" & FaID
    DatenFarb.DvPro.RowFilter = "FARBM_ID =" & FaID
    DatenFarb.DvPrb.RowFilter = "FARBM_ID =" & FaID
  End Sub













  '
  '
  '
  '



  Private Sub MncboMNG_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles MncboMNG.SelectedIndexChanged
    MnAllRezepte.INO = MncboMNG.SelectedIndex
  End Sub

  Private Sub MncboPRO_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles MncboPRO_0.SelectedIndexChanged, MncboPRO_1.SelectedIndexChanged

    If sender.name = MncboPRO_0.Name Then
      MnAllRezepte.INP = MncboPRO_0.SelectedIndex
    End If
    If sender.name = MncboPRO_1.Name Then
      MnAllRezepte.INQ = MncboPRO_1.SelectedIndex
    End If


  End Sub



  Private Sub MnChkVOL_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnChkVOL.CheckStateChanged
    Dim i As Short
    Dim ier As Integer
    Dim Ivol As Short
    Dim Fakt As Single
    Dim AmngAlt As Single
    Dim ProNen As Single
    Dim ProZae As Single
    Dim KeyNor As String
    If MnChkVOL.CheckState = CheckState.Indeterminate Then Exit Sub
    '
    '
    '
    KeyNor = KeyNam
    If KeyNor = "KOR" Or KeyNor = "KOO" Or KeyNor = "KOA" Then
      KeyNor = "MEN"
    End If
    AmngAlt = MnUmr.MngINO(KeyNor, MnAllRezepte, ier)
    ProNen = MnUmr.MngINQ(KeyNor, MnAllRezepte, ier)
    ProZae = MnUmr.MngINP(KeyNor, MnAllRezepte, ier)

    '
    '
    Ivol = CInt(MnChkVOL.CheckState)
    MnAllRezepte.IVOL = Ivol
    '
    '
    '
    '
    '
    '
    '
    '
    'In Datenbank die ID's 610 ff. verwenden und 580 ff. löschen
    '
    '
    'Art der Normierungen
    '
    '

    If Not IsNothing(MncboMNG) Then
      MncboMNG.Items.Clear()
      For i = 0 To 18
        MncboMNG.Items.Add(RepTexxt(Texxt(580 + i), Texxt(602 + Ivol)))
      Next i
      MncboMNG.SelectedIndex = MnAllRezepte.INO
      ToolTipC1Screen.SetToolTip(MncboMNG, Texxt(650))
    End If
    If Not IsNothing(MnTxtMNG_0) Then
      ToolTipC1Screen.SetToolTip(MnTxtMNG_0, Texxt(652) & Space(1) & Texxt(602 + Ivol))
    End If
    If Not IsNothing(MnTxtMNG_1) Then
      ToolTipC1Screen.SetToolTip(MnTxtMNG_1, Texxt(651) & Space(1) & Texxt(602 + Ivol))
    End If
    '
    '
    'Art der Mengen-/Volumenverhältnisse
    '
    '

    If Not IsNothing(MncboPRO_0) And Not IsNothing(MncboPRO_1) Then
      MncboPRO_0.Items.Clear()
      MncboPRO_1.Items.Clear()
      For i = 0 To 18
        MncboPRO_0.Items.Add(RepTexxt(Texxt(580 + i), Texxt(602 + Ivol)))
        MncboPRO_1.Items.Add(RepTexxt(Texxt(580 + i), Texxt(602 + Ivol)))
      Next i
      '
      MncboPRO_0.SelectedIndex = MnAllRezepte.INP
      MncboPRO_1.SelectedIndex = MnAllRezepte.INQ
      ToolTipC1Screen.SetToolTip(MncboPRO_0, Texxt(653))
      ToolTipC1Screen.SetToolTip(MncboPRO_1, Texxt(654)) '

    End If
    If Not IsNothing(MnTxtPRO_0) Then
      ToolTipC1Screen.SetToolTip(MnTxtPRO_0, Texxt(656))
    End If
    If Not IsNothing(MnTxtPRO_1) Then
      ToolTipC1Screen.SetToolTip(MnTxtPRO_1, Texxt(655)) '
    End If
    '
    'Normierungsmenge
    '
    '
    Fakt = MnUmr.MngINO(KeyNor, MnAllRezepte, ier) / AmngAlt
    If Not IsNothing(MnTxtMNG_0) And KeyNor <> "SOR" Then
      MnAllRezepte.MngMin = Fakt * MnAllRezepte.MngMin
      MnTxtMNG_0.Text = Format(MnAllRezepte.MngMin, "###0.000")
    End If
    If Not IsNothing(MnTxtMNG_1) And KeyNor <> "SOR" Then
      MnAllRezepte.MngMax = Fakt * MnAllRezepte.MngMax
      MnTxtMNG_1.Text = Format(MnAllRezepte.MngMax, "###0.000")
    End If
    If MnAllRezepte.INP <> 0 And MnAllRezepte.INQ <> 0 Then
      Fakt = MnUmr.MngINP(KeyNor, MnAllRezepte, ier) / ProZae
      Fakt = Fakt * ProNen / MnUmr.MngINQ(KeyNor, MnAllRezepte, ier)
      If Not IsNothing(MnTxtPRO_0) And KeyNor <> "SOR" Then
        MnAllRezepte.ProzMin = Fakt * MnAllRezepte.ProzMin
        MnTxtPRO_0.Text = Format(MnAllRezepte.ProzMin, "###0.000")
      End If
      If Not IsNothing(MnTxtPRO_1) And KeyNor <> "SOR" Then
        MnAllRezepte.ProzMax = Fakt * MnAllRezepte.ProzMax
        MnTxtPRO_1.Text = Format(MnAllRezepte.ProzMax, "###0.000")
      End If
    End If
    '
    '
    'Tabelle rückspeichern
    '
    '
    '
    Call TDBFarFill(ier)
    '
    '
    '
    '

    '
  End Sub

  Private Sub Change(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  MncboMNG.TextChanged, MncboPRO_0.TextChanged, MncboPRO_1.TextChanged, MnTxtMNG_0.TextChanged, MnTxtMNG_1.TextChanged, MnTxtPRO_0.TextChanged, MnTxtPRO_1.TextChanged
    '
    If Reb Then
      Reb = False
      Call TDBFarFill(ier)
    End If

    '
    '
    '
    'Daten haben sich geändert
    '
    '
    '
    '
    '
    If TblRezept.Rows.Count > 0 Then
      'MnTDBFar.RefetchRow(TblRezept.Rows.Count - 1)
    End If
    If Not IsNothing(MnPicGraphic) Then
      MnPicGraphic.DataChanged = True
    End If
    '
    '
    '
    '
    If sender.name = "txtMNG_0" Then
      If IsNumeric(sender.text) Then
        MnAllRezepte.MngMin = CSng(sender.text)
      End If
    End If
    If sender.name = "txtMNG_1" Then
      If IsNumeric(sender.text) Then
        MnAllRezepte.MngMax = CSng(sender.text)
      End If
    End If
    If sender.name = "txtPRO_0" Then
      If IsNumeric(sender.text) Then
        MnAllRezepte.ProzMin = 0.01 * CSng(sender.text)
      End If
    End If
    If sender.name = "txtPRO_1" Then
      If IsNumeric(sender.text) Then
        MnAllRezepte.ProzMax = 0.01 * CSng(sender.text)
      End If
    End If
  End Sub

  Private Sub MnTxt_TextChanged(sender As Object, e As System.EventArgs) Handles MnTxtFooter.TextChanged, MnTxtMNG_0.TextChanged, MnTxtMNG_1.TextChanged, MnTxtPRO_0.TextChanged, MnTxtPRO_1.TextChanged
    If Not IsNumeric(sender.text) Then
      sender.text = sender.tag
    End If
  End Sub

  Private Sub MnTxt_Enter(sender As Object, e As System.EventArgs) Handles MnTxtFooter.Enter, MnTxtMNG_0.Enter, MnTxtMNG_1.Enter, MnTxtPRO_0.Enter, MnTxtPRO_1.Enter
    sender.tag = sender.text
  End Sub



  Private Sub MnTxtFooter_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles MnTxtFooter.KeyPress
    If KeyNam = "" Then Exit Sub
    If Asc(e.KeyChar) = Keys.Return And IsNumeric(MnTxtFooter.Text) Then
      Call NormBamng(KeyNam, MnAllRezepte, Singl(MnTxtFooter.Text), ier)
      Call TDBFarFill(ier)
    End If
  End Sub

  Private Sub MnTxtFooter_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnTxtFooter.Leave
    Call MnTxtFooter_KeyPress(sender, New KeyPressEventArgs(Chr(Keys.Return)))
    MnTDBFar.Columns(2).FooterText = MnTxtFooter.Text

  End Sub

  '
  '
  'Ereignisse für Gitternetz
  '
  '
  '
  Private Sub MnTDBFar_BeforeColEdit(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.BeforeColEditEventArgs) Handles MnTDBFar.BeforeColEdit
    Dim Rowindex As Integer
    Dim FaId As Integer
    If KeyNam = "" Then Exit Sub
    If IsNothing(ConnRezept) Then Exit Sub
    If IsNothing(ConnRezept.Current) Then Exit Sub
    If e.ColIndex = 6 Or e.ColIndex = 7 Or e.ColIndex = 8 Then
      FaId = ConnRezept.Current("FID")
      Call CreateView(FaId)
    End If


    If e.ColIndex > 0 And Rowindex >= MnAllRezepte.Rezepte(KeyNam).KF Then
      e.Cancel = True
      Exit Sub
    End If

  End Sub




  Private Sub MnTDBFar_AfterColEdit(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.ColEventArgs) Handles MnTDBFar.AfterColEdit
    Call AfterEdit(e.ColIndex, MnTDBFar.Row)
  End Sub
  Sub AfterEdit(Colindex As Integer, Rowindex As Integer)
    Dim OpAlt As String
    Dim Limalt As Single
    Dim KeID As String
    If AfterEd Then Exit Sub
    AfterEd = True
    If Colindex = 1 AndAlso MnTDBFar.Splits(0).DisplayColumns("PRG").Visible Then
      Select Case KeyNam
        Case "BAS", "KOR", "KOO", "KOA"
          If ConnRezept(Rowindex)("FID") < 0 Then Exit Sub
          KeID = KeyName(ConnRezept(Rowindex)("FID"))
          OpAlt = MnAllRezepte.Farben(KeID).OP
          Limalt = MnAllRezepte.Farben(KeID).BoMng
          MnAllRezepte.Farben(KeID).OP = "="
          MnAllRezepte.Farben(KeID).BoMng = ConnRezept(MnTDBFar.Row)("MEN")
          Select Case KeyNam
            Case "BAS"
              CalcRezept.BasisRezept(Picauf.PicGraphic.KFIT, MenueParam.User.Winkel, KeyNam, KeyNam, MnAllRezepte, MnGrpRwerte, MnIer)
            Case "KOR", "KOO", "KOA"
              CalcRezept.KorrekturRezept(Picauf.PicGraphic.KFIT + 16 * MnChkGrundKorr, MenueParam.User.Winkel, "ALT", KeyNam, MnAllRezepte, MnGrpRwerte, MnIer)
          End Select
          MnAllRezepte.Farben(KeID).OP = OpAlt
          MnAllRezepte.Farben(KeID).BoMng = Limalt
          Call TDBFarFill(ier)
          Picauf.Refresh()
          ConnRezept(Rowindex)("PRG") = True
      End Select
    Else

      Call FillRezept(Colindex, Rowindex)
    End If
    If Not IsNothing(MnPicGraphic) Then
      MnPicGraphic.DataChanged = True
    End If
    AfterEd = False
  End Sub
  Sub FillRezept(ByVal Colindex As Integer, ByVal Rowindex As Integer)

    '
    Dim UserVal As Object
    Dim i As Integer
    Dim j As Integer
    Dim ier As Short
    Dim Uier As Boolean
    Reb = False
    Dim KeyNr As String
    Dim FaID As Integer
    Dim KeId As String

    If IsNothing(ConnRezept) Then Exit Sub
    If IsNothing(ConnRezept.Current) Then Exit Sub
    If Colindex = 1 Then Exit Sub
    KeyIndex = TblRezept.Columns(Colindex).Caption

    If BitWrt(17, MenueParam.User.Drum) Then
      LayoutChanged = True
    End If


    ' FaID = MnTDBFar.Rows(RowIndex).Cells("FID").Value



    MnIer = 0

    '
    '
    KeyNr = KeyRe(Rowindex)
    If Not MnAllRezepte.Rezepte(KeyNam).ContainsKey(KeyNr) Then Exit Sub
    If IsDBNull(ConnRezept.Current(KeyIndex)) Then Exit Sub
    ConnRezept.CurrencyManager.EndCurrentEdit()
    UserVal = ConnRezept.Current(KeyIndex)
    If KeyIndex <> "FID" Then
      FaID = ConnRezept.Current(0)
      KeId = KeyName(FaID)
    End If

    '
    'Prüfen,ob Eingabe in Ordnung
    '
    Select Case KeyIndex

      '
      Case "FID"       'Farbmittel-ID
        '
        '
        '
        '
        'Menge
      Case "MEN"
        If Not IsNumeric(ConnRezept.Current(Colindex)) Then
          MessageBox.Show(Texxt(16))
          Exit Sub
        End If
        If ConnRezept.Current(Colindex) < 0 Then
          ConnRezept.Current(Colindex) = 0.0
          MessageBox.Show(Texxt(4105))
          Exit Sub
        End If
        '
        '
        '
        '
        '
        '
        '
        Select Case KeyNam
          Case "MNG"                  'Menge
            MnAllRezepte.Rezepte(KeyNam)(KeyNr).BaAmng = Singl(UserVal)
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
          Case "MEN"                  'Menge
            MnAllRezepte.Rezepte(KeyNam)(KeyNr).BaAmng = Singl(UserVal)
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
          Case "SOR"                  'Menge
            MnAllRezepte.Rezepte(KeyNam)(KeyNr).BaAmng = Singl(UserVal)
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
          Case "BAS"                  'Menge
            MnAllRezepte.Rezepte(KeyNam)(KeyNr).BaAmng = Singl(UserVal)
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
            '
            '
            'Neue Reflexionswerte berechnen
            '
            '
            'CalcRezept.MischRezept(0,  MenueParam.User.Winkel, KeyNam, MnAllRezepte, MnGrpRwerte, GrundDat, MnIer)
            Picauf.Refresh()

          Case "KOR", "KOO", "KOA"                  'Menge
            WrtAlt = MnAllRezepte.Rezepte(KeyNam)(KeyNr).BaAmng
            MnAllRezepte.Rezepte(KeyIndex)(KeyNr).BaAmng = Singl(UserVal)
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
            '
            'Neue Reflexionswerte berechnen
            '
            '
            CalcRezept.MischKorrektur(16 * MnChkGrundKorr, MenueParam.User.Winkel, "ALT", KeyNam, MnAllRezepte, MnGrpRwerte, GrundDat, MnIer)
            Picauf.Refresh()
            'Call TDBFarFill()



          Case "UMR", "NEU"                  'Menge
            MnAllRezepte.Rezepte(KeyNam)(KeyNr).BaAmng = Singl(UserVal)
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
          Case "COL"                  'Menge
            MnAllRezepte.Rezepte(KeyNam)(KeyNr).BaAmng = Singl(UserVal)
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
          Case "RZP"                  'Menge
          Case "ZEI"                  'Menge
            MnAllRezepte.Rezepte(KeyNam)(KeyNr).BaAmng = Singl(UserVal)
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
          Case "ALT"                  'Menge alt
        End Select

      Case "ZUW"           'Zuwaage

      Case "MZU"           'Menge(alt)+Zuwaage

      Case "OPR"           'Operator
        '
        '
        '
        '
        If MnAllRezepte.Farben.ContainsFarb(KeId) Then
          MnAllRezepte.Farben(KeId).OP = UserVal
        End If

        '
      Case "LIM"           'Limitierungsmenge
        If Not IsNumeric(ConnRezept.Current(Colindex)) Then
          MessageBox.Show(Texxt(16))
          Exit Sub
        End If
        If ConnRezept.Current(Colindex) < 0 Then
          ConnRezept.Current(Colindex)(Colindex) = 0.0
          MessageBox.Show(Texxt(4105))
          Exit Sub
        End If
        '
        '
        '
        '
        If MnAllRezepte.Farben.ContainsFarb(KeId) Then
          MnAllRezepte.Farben(KeId).BoMng = Singl(UserVal) / MnAllRezepte.Rezepte(KeyNam).UmMng
        End If
      Case "KTO"           'Topfangabe
        '
        '
        If MnAllRezepte.Farben.ContainsFarb(KeId) Then
          MnAllRezepte.Farben(KeId).Kto = UserVal
        End If

      Case "PRE"           'Preis
        If MnAllRezepte.Farben.ContainsFarb(KeId) Then
          MnAllRezepte.Farben(KeId).Preis = Singl(UserVal)
        End If
        Select Case KeyNam
          Case "KOR", "RZP", "BAS", "KOO", "KOA"
            Picauf.Refresh()
        End Select
        '
        '
        '
        '
        '
        '
        '
      Case "PRO"           'Prozentigkeit
        '
        '
        '
        '
        '
        WrtAlt = MnAllRezepte.Rezepte(KeyNam)(KeyNr).Proz
        If ConnRezept.Current(Colindex) <= 0 Or ConnRezept.Current(Colindex) > 100 Then
          ConnRezept.Current(Colindex) = WrtAlt
          Exit Sub
        End If
        WrtNeu = Singl(UserVal)
        MnAllRezepte.Rezepte(KeyNam)(KeyNr).Proz = WrtNeu
        If MnAllRezepte.Farben.ContainsFarb(KeId) Then
          If MnAllRezepte.Farben(KeId).Ichf = 1 Then
            '
            '
            'Für Bindemittel wird PROZ=PROB gesetzt
            '
            '
            MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob = WrtNeu
          End If
        End If
        Select Case KeyNam
          Case "SOR", "MNG", "NEU"
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
          Case "KOR", "KOO", "KOA"
            For i = 0 To 2
              If MnAllRezepte.Rezepte(KeyHilf(i)).KF > 0 Then
                MnAllRezepte.Rezepte(KeyHilf(i))(KeyNr).Proz = WrtNeu
                If MnAllRezepte.Farben.ContainsFarb(KeId) Then
                  If MnAllRezepte.Farben(KeId).Ichf = 1 Then
                    '
                    '
                    'Für Bindemittel wird PROZ=PROB gesetzt
                    '
                    '
                    MnAllRezepte.Rezepte(KeyHilf(i))(KeyNr).Prob = WrtNeu
                  End If
                End If
              End If
            Next i
            Uier = True
            MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
            If ier <> 0 Then
              Uier = False
            End If
            If Uier = True Then
              For i = 0 To 2
                MnUmr.CalcBamng(KeyHilf(i), MnAllRezepte, ier)
                If ier <> 0 Then
                  Uier = False
                End If
              Next i
            End If
            If Uier = False Then
              '
              '
              '
              'Fehler
              '
              '
              '
              MnAllRezepte.Rezepte(KeyNam)(KeyNr).Proz = WrtAlt
              If MnAllRezepte.Farben.ContainsFarb(KeId) Then
                If MnAllRezepte.Farben(KeId).Ichf = 1 Then
                  MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob = WrtAlt
                End If
              End If
              MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
              For i = 0 To 2
                MnAllRezepte.Rezepte(KeyHilf(i))(KeyNr).Proz = WrtAlt
                If MnAllRezepte.Farben.ContainsFarb(KeId) Then
                  If MnAllRezepte.Farben(KeId).Ichf = 1 Then
                    MnAllRezepte.Rezepte(KeyHilf(i))(KeyNr).Prob = WrtAlt
                  End If
                End If
                MnUmr.CalcBamng(KeyHilf(i), MnAllRezepte, ier)
              Next i
              '
              '
              Exit Sub
            End If
            Reb = True
            Call TDBFarFill(ier)
          Case Else
            MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
            If ier <> 0 Then
              MnAllRezepte.Rezepte(KeyNam)(KeyNr).Proz = WrtAlt
              If MnAllRezepte.Farben.ContainsFarb(KeId) Then
                If MnAllRezepte.Farben(KeId).Ichf = 1 Then
                  MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob = WrtAlt
                End If
              End If
              Exit Sub
            Else
              For j = 0 To MnAllRezepte.Rezepte(KeyNam).KF - 1

              Next
            End If
            Reb = True
            Call TDBFarFill(ier)
        End Select
        '
        '
        '
        '
        '
      Case "PRB"           'Bindemittel-Prozentigkeit
        '
        If MnAllRezepte.Farben.ContainsFarb(KeId) Then
          If MnAllRezepte.Farben(KeId).Ichf = 1 Then
            Exit Sub
          End If
        End If

        '
        '
        '

        WrtAlt = MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob
        If ConnRezept.Current(Colindex) <= 0 Or ConnRezept.Current(Colindex) > 100 Then
          ConnRezept.Current(Colindex) = WrtAlt
          MessageBox.Show(Texxt(3990))
          Exit Sub
        End If

        WrtNeu = Singl(UserVal)
        WrtProz = MnAllRezepte.Rezepte(KeyNam)(KeyNr).Proz
        If KbProb = 1 Then
          '
          '
          'Bindemittelprozentigkeit ist auf Gesamtbatchmenge bezogen
          '
          '
          If WrtNeu <> 0 Then
            If WrtProz = 100 Then
              ConnRezept.Current(Colindex) = WrtAlt
              MsgBox(Texxt(3991))
              Exit Sub
            End If
            WrtNeu = WrtNeu / (1.0# - 0.01 * WrtProz)
            If WrtNeu > 100 Or WrtNeu < 0 Then
              ConnRezept.Current(Colindex) = WrtAlt
              MsgBox(Texxt(3991))
              Exit Sub
            End If
          Else
            WrtNeu = 0.0#
          End If
        End If
        MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob = WrtNeu
        Select Case KeyNam
          Case "SOR", "UMR", "NEU"
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
          Case "KOR", "KOO", "KOA"
            For i = 0 To 2
              If MnAllRezepte.Rezepte(KeyHilf(i)).KF > 0 Then
                MnAllRezepte.Rezepte(KeyHilf(i))(KeyNr).Prob = WrtNeu
              End If
            Next i
            MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
            If ier <> 0 Then
              Uier = False
            End If
            If Uier = True Then
              For i = 0 To 2
                MnUmr.CalcBamng(KeyHilf(i), MnAllRezepte, ier)
                If ier <> 0 Then
                  Uier = False
                End If
              Next i
            End If
            If Uier = False Then

              '
              '
              '
              'Fehler
              '
              '
              MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob = WrtAlt
              MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
              For i = 0 To 2
                MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob = WrtAlt
                MnUmr.CalcBamng(KeyHilf(i), MnAllRezepte, ier)
              Next i
              '
              '
              Exit Sub
            End If
            Reb = True

          Case Else
            MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
            If ier <> 0 Then
              '
              '
              '
              'Fehler
              '
              '
              MnAllRezepte.Rezepte(KeyIndex)(KeyNr).Prob = WrtAlt
              '
              '
              Exit Sub
            End If
            Reb = True

        End Select
        '

      Case "FST"           'Farbstärke
        MnAllRezepte.Farben(KeId).Fst = Singl(UserVal)
      Case "BEL"           'Gewicht für Minimierung
        MnAllRezepte.Farben(KeId).Bel = Singl(UserVal)
      Case "FRM"           'Ausgabeformat
        MnAllRezepte.Farben(KeId).Form = UserVal

        '
    End Select
    If Not IsNothing(MnPicGraphic) Then
      MnPicGraphic.DataChanged = True
    End If
  End Sub



  Private Sub MnTDBFar_Scroll(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.CancelEventArgs) Handles MnTDBFar.Scroll
    Call FooterBounds()
  End Sub
  Private Sub MnTDBFar_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnTDBFar.Resize
    Call DropWidth()

    Call FooterBounds()

  End Sub
  Private Sub MnTDBFar_ColResize(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.ColResizeEventArgs) Handles MnTDBFar.ColResize
    Call DropWidth()
    Call FooterBounds()
    If Not e.Column.Visible Then
      e.Column.Width = 0
    End If

  End Sub
  Private Sub MnTDBFar_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles MnTDBFar.Paint
    Call DropWidth()
    Call FooterBounds()
  End Sub
  Sub DropWidth()
    If MnTDBFar.Columns.Count = 0 Then Exit Sub
    MnTDBDropFar.Width = MnTDBFar.Splits(0).DisplayColumns("FID").Width
    MnTDBDropFar.DisplayColumns("FARBM_NAME").Width = MnTDBDropFar.Width - 24
    '
    '

    MnTDBDropPre.Width = MnTDBFar.Splits(0).DisplayColumns("PRE").Width
    MnTDBDropPre.DisplayColumns("FARBM_PREIS").Width = MnTDBDropPre.Width - 24
    '
    '
    MnTDBDropPro.Width = MnTDBFar.Splits(0).DisplayColumns("PRO").Width
    MnTDBDropPro.DisplayColumns("FARBM_PROZ").Width = MnTDBDropPro.Width - 24
    '
    '
    MnTDBDropPrb.Width = MnTDBFar.Splits(0).DisplayColumns("PRB").Width
    MnTDBDropPrb.DisplayColumns("FARBM_PROB").Width = MnTDBDropPrb.Width - 24
  End Sub
  Sub FooterBounds()
    Dim FootLeft As Integer
    Dim FootTop As Integer
    Dim FootWidth As Integer
    Dim FootHeight As Integer
    If MnTDBFar.Columns.Count = 0 Then Exit Sub
    FootLeft = MnTDBFar.Location.X + MnTDBFar.Splits(0).DisplayColumns("FID").Width - MnTDBFar.Splits(0).HorizontalOffset
    If MnTDBFar.Splits(0).DisplayColumns("PRG").Visible Then
      FootLeft = FootLeft + MnTDBFar.Splits(0).DisplayColumns("PRG").Width
    End If
    If MnTDBFar.Splits(0).RecordSelectors Then
      FootLeft = FootLeft + MnTDBFar.RecordSelectorWidth
    End If
    FootTop = MnTDBFar.Location.Y + MnTDBFar.Size.Height - MnTDBFar.Splits(0).ColumnFooterHeight
    If MnTDBFar.HScrollBar.Visible Then
      FootTop = FootTop - MnTDBFar.HScrollBar.Height
    End If
    MnTxtFooter.Location = New Point(FootLeft, FootTop)
    FootWidth = MnTDBFar.Splits(0).DisplayColumns("MEN").Width
    FootHeight = MnTDBFar.Splits(0).ColumnFooterHeight
    MnTxtFooter.Size = New Size(FootWidth, FootHeight)
  End Sub


  Private Sub MnTDBFar_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnTDBFar.DoubleClick
    Dim ev As New System.Windows.Forms.KeyEventArgs(Keys.Delete)
    Call MnTDBFar_KeyDown(sender, ev)
  End Sub
  Private Sub MnTDBFar_KeyDown(sender As Object, e As System.Windows.Forms.KeyEventArgs) Handles MnTDBFar.KeyDown
    Dim i As Integer
    '
    '
    'Drucken
    '
    If e.KeyCode = Keys.F4 Then
      MnTDBFar.PrintInfo.PrintPreview()
      Exit Sub
    End If
    If Not MnTDBFar.AllowRowSelect Or MnTDBFar.SelectedRows.Count = 0 Then Exit Sub
    If e.KeyCode = Keys.Delete Then
      If sender.columns(sender.Col).datafield = "FID" Or MnTDBFar.SelectedRows(0) = MnTDBFar.Row Then
        '
        '
        '
        'Beforedelete und Afterdelete werden nicht automatisch ausgelöst
        'Allowrowselect muss false sein, sonst werden 2 Zeilen gelöscht
        '
        '
        Call RemoveRow(MnTDBFar.Row)
      End If
      Exit Sub
    End If
    If KeyNam = "BAS" Then

      '
      '
      'Lösche ausgewählte Farbmittel
      '
      If e.KeyCode = Keys.F5 Then
        For i = MnTDBFar.SelectedRows.Count - 1 To 0 Step -1
          Call RemoveRow(MnTDBFar.SelectedRows(i))
        Next
        MnTDBFar.SelectedRows.Clear()
      End If
      '
      'Lösche Farbmitel mit Menge=0
      '
      If e.KeyCode = Keys.F6 Then
        For i = TblRezept.Rows.Count - 1 To 0 Step -1
          If TblRezept.Rows(i)("MEN") < 0.0001 Then
            Call RemoveRow(i)
          End If
        Next
        MnTDBFar.SelectedRows.Clear()

      End If

    End If
  End Sub
  Private Sub MnTDBFar_Error(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.ErrorEventArgs) Handles MnTDBFar.Error



    'Private Sub MnTDBFar_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles MnTDBFar.DataError
    e.Handled = True
    e.Continue = True
  End Sub
  Private Sub MnTDBFar_ValueItemError(sender As Object, e As C1.Win.C1TrueDBGrid.ColEventArgs) Handles MnTDBFar.ValueItemError
    If MnTDBFar.Columns(e.ColIndex).DataField = "KTO" Or MnTDBFar.Columns(e.ColIndex).DataField = "OPR" Then
      ' MnTDBFar.Rows(MnTDBFar.Row).
      ConnRezept.Item(MnTDBFar.Row)(MnTDBFar.Columns(e.ColIndex).DataField) = " "
      ConnRezept.EndEdit()
    End If
  End Sub
  Sub RemoveRow(ByVal rowindex As Integer)
    Dim k As Short
    Dim i As Short
    If KeyNam = "" Then Exit Sub
    If Not MnTDBFar.AllowDelete Then Exit Sub
    If rowindex < 0 Or rowindex > MnAllRezepte.Rezepte(KeyNam).KF - 1 Then Exit Sub
    Amenge = MnUmr.MngINF(KeyNam, MnAllRezepte, ier)
    For k = rowindex To MnAllRezepte.Rezepte(KeyNam).KF - 2
      MnAllRezepte.Rezepte(KeyNam)(k).ID = MnAllRezepte.Rezepte(KeyNam)(k + 1).ID
      MnAllRezepte.Rezepte(KeyNam)(k).FaAmng = MnAllRezepte.Rezepte(KeyNam)(k + 1).FaAmng
      MnAllRezepte.Rezepte(KeyNam)(k).BaAmng = MnAllRezepte.Rezepte(KeyNam)(k + 1).BaAmng
      MnAllRezepte.Rezepte(KeyNam)(k).Prob = MnAllRezepte.Rezepte(KeyNam)(k + 1).Prob
      MnAllRezepte.Rezepte(KeyNam)(k).Proz = MnAllRezepte.Rezepte(KeyNam)(k + 1).Proz
      If KeyNam = "KOR" Or KeyNam = "KOO" Or KeyNam = "KOA" Then
        For i = 0 To 2
          MnAllRezepte.Rezepte(KeyHilf(i))(k).ID = MnAllRezepte.Rezepte(KeyHilf(i))(k + 1).ID
          MnAllRezepte.Rezepte(KeyHilf(i))(k).FaAmng = MnAllRezepte.Rezepte(KeyHilf(i))(k + 1).FaAmng
          MnAllRezepte.Rezepte(KeyHilf(i))(k).BaAmng = MnAllRezepte.Rezepte(KeyHilf(i))(k + 1).BaAmng
          MnAllRezepte.Rezepte(KeyHilf(i))(k).Prob = MnAllRezepte.Rezepte(KeyHilf(i))(k + 1).Prob
          MnAllRezepte.Rezepte(KeyHilf(i))(k).Proz = MnAllRezepte.Rezepte(KeyHilf(i))(k + 1).Proz
        Next i
      End If
    Next k
    MnAllRezepte.Rezepte(KeyNam).RemoveFaNr(KeyRe(MnAllRezepte.Rezepte(KeyNam).KF - 1))
    If KeyNam = "KOR" Or KeyNam = "KOO" Or KeyNam = "KOA" Then
      For i = 0 To 2
        MnAllRezepte.Rezepte(KeyHilf(i)).RemoveFaNr(KeyRe(MnAllRezepte.Rezepte(KeyHilf(i)).KF - 1))
      Next i
    End If
    Select Case KeyNam
      Case "KOR", "KOO", "KOA"
      Case "UMR", "NEU", "SOR", "ZEI", "BAS"
      Case Else
        '
        'Menge umrechnen
        Call NormBamng(KeyNam, MnAllRezepte, Amenge, ier)

        '
    End Select
    MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
    Call TDBFARTxtAllSum()
    Select Case KeyNam
      Case "BAS"           'Menge
        Call NormBamng(KeyNam, MnAllRezepte, Amenge, ier)
        '
        '
        'Neue Reflexionswerte berechnen
        '
        '
        CalcRezept.MischRezept(0, MenueParam.User.Winkel, KeyNam, MnAllRezepte, MnGrpRwerte, GrundDat, MnIer)
        Picauf.Refresh()

      Case "KOR", "KOO", "KOA"           'Menge
        Call NormBamng(KeyNam, MnAllRezepte, Amenge, ier)
        '
        'Neue Reflexionswerte berechnen
        '
        '
        CalcRezept.MischKorrektur(16 * MnChkGrundKorr, MenueParam.User.Winkel, "ALT", KeyNam, MnAllRezepte, MnGrpRwerte, GrundDat, MnIer)
        Picauf.Refresh()
    End Select

    Call TDBFarFill(ier)
  End Sub


  Private Sub MnlstFAR_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnlstFAR.Click
    Dim Rowneu As Integer
    Dim ifeh As Integer
    Dim FaID As Integer
    Rowneu = MnAllRezepte.Rezepte(KeyNam).KF
    FaID = sender.SelectedValue
    Call FarbmNeu(FaID, Rowneu, ifeh)
    If ifeh <> 0 Then
      MnTDBFar.EndInit()
      Exit Sub
    End If
    Call TDBFarFill(ier)
    MnPicGraphic.DataChanged = True
  End Sub




  Private Sub MnTDBFar_HeadClick(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.ColEventArgs) Handles MnTDBFar.HeadClick
    Dim Colindex As Integer
    Dim Keyindex As String
    Dim KNF As Short
    Dim KNM As Short
    Dim ivol As Short
    MnTDBFar.EditActive = False
    Application.DoEvents()

    ConnRezept.CurrencyManager.EndCurrentEdit()
    Call AfterEdit(MnTDBFar.Col, MnTDBFar.Row)

    '

    Colindex = e.ColIndex
    If Colindex < 0 Then Exit Sub
    ivol = MnAllRezepte.IVOL
    KNF = MnAllRezepte.INF
    KNM = MnAllRezepte.INM
    If Not IsNothing(MnPicGraphic) Then
      MnPicGraphic.DataChanged = True
    End If
    Keyindex = MnTDBFar.Columns(Colindex).DataField
    JNF = 0
    Select Case Keyindex
      Case "FID"
        '
        '
        ' MENGE zurücksetzen
        '
        '
        JNF = 0
        MnTDBFar.Columns("MEN").Caption = HeaderText(JNF)
        MnAllRezepte.INF = JNF
        '
        '
        'Lim.-Menge zurücksetzen
        '
        '
        If MnTDBFar.Splits(0).DisplayColumns("LIM").Visible Then
          JNM = 0
          MnAllRezepte.INM = JNM
          MnTDBFar.Columns("LIM").Caption = HeadLiText(JNM)
        End If
      Case "LIM"
        If BitWrt(12, MenueParam.User.Sonst) Then
          JNM = MnAllRezepte.INM + 1
          If JNM > 2 Then JNM = 0
          MnAllRezepte.INM = JNM
          MnTDBFar.Columns(Keyindex).Caption = HeadLiText(JNM)
        End If
      Case "MEN"
        Select Case KeyNam
          Case "UMR", "NEU", "SOR", "BAS", "MNG"
            If BitWrt(11, MenueParam.User.Sonst) Then
              JNF = MnAllRezepte.INF + 1
              If JNF > 2 Then JNF = 0
              MnTDBFar.Columns(Keyindex).Caption = HeaderText(JNF)
              MnAllRezepte.INF = JNF
            End If
          Case "RZP", "KOR", "MEN", "HLF", "COL", "ZEI", "KOO", "KOA"
            If BitWrt(11, MenueParam.User.Sonst) Then
              JNF = MnAllRezepte.INF + 1
              If JNF > 6 Then JNF = 0
              MnTDBFar.Columns(Keyindex).Caption = HeaderText(JNF)
              MnAllRezepte.INF = JNF
            End If

        End Select
    End Select
    '
    '
    'Umrechnen
    '
    '
    Select Case KeyNam
      Case "BAS", "RZP", "KOR", "MNG", "MEN", "HLF", "COL", "ZEI", "KOO", "KOA"
        MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
        If ier <> 0 Then
          MnAllRezepte.INF = KNF
          MnAllRezepte.INM = KNM
          MnTDBFar.Columns("MEN").Caption = HeaderText(KNF)
          If MnTDBFar.Splits(0).DisplayColumns("LIM").Visible Then
            MnTDBFar.Columns("LIM").Caption = HeadLiText(KNM)
          End If
          Call TDBFarFill(ier)
          Exit Sub
        End If
        Call TDBFARTxtAllSum()
    End Select
    TDBFarFill(ier)

  End Sub

  Private Sub MnTDBDropFar_SelChange(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.CancelEventArgs) Handles MnTDBDropFar.SelChange
    Dim i As Integer
    Dim FaID As Integer
    Dim Rowindex As Integer
    Dim ifeh As Integer
    '
    '
    'Prüfen, ob bereits vorhanden
    '
    '
    '
    If MnAllRezepte.Rezepte.ContainsKey(KeyNam) Then
      For i = 0 To MnAllRezepte.Rezepte(KeyNam).KF - 1
        If MnTDBDropFar.DataSource.Rows(MnTDBDropFar.Row)("FARBM_ID") = MnAllRezepte.Rezepte(KeyNam)(i).ID Then
          e.Cancel = True
          Call TDBFarFill(ier)
          Exit Sub
        End If
      Next


      If IsNothing(sender.columns("FARBM_ID").value) Then
        e.Cancel = True
        Exit Sub
      End If
      Rowindex = MnTDBFar.Row
      FaID = sender.columns("FARBM_ID").value


      Call FarbmNeu(FaID, Rowindex, ifeh)
      If ifeh <> 0 Then
        Exit Sub
      End If
      Call TDBFarFill(ier)
      MnPicGraphic.DataChanged = True


      MnTDBFar.EditActive = False
    End If
    ''
  End Sub

  Private Sub MnTDBFar_VisibleChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnTDBFar.VisibleChanged
    MnTDBFar.EditActive = False
  End Sub

  Private Sub MnTDBFar_Validating(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MnTDBFar.Validating
    MnTDBFar.EditActive = False
  End Sub

  Private Sub MnTDBFar_FetchRowStyle(sender As Object, e As C1.Win.C1TrueDBGrid.FetchRowStyleEventArgs) Handles MnTDBFar.FetchRowStyle
    If AllRezepte.Rezepte(KeyNam).KF = 0 Then Exit Sub
    If e.Row >= AllRezepte.Rezepte(KeyNam).KF Then Exit Sub
    e.CellStyle.BackColor = Color.FromArgb(AllRezepte.Farben(KeyName(AllRezepte.Rezepte(KeyNam)(e.Row).ID)).FarbID)
  End Sub

End Class
'
'
'
'
'
'
'Dataset für Farbmittel
'
'
'
'
'
'
'
'
'
Public Class HandleRezDsetFarb

  Implements IDisposable
  Dim disposed As Boolean
  Dim DsetFarb As New DataSet
  Dim MnDvPre As New DataView
  Dim MnDvPro As New DataView
  Dim MnDvPrb As New DataView
  Dim OleAdFarb As New OleDbDataAdapter

  Public Sub New()

    disposed = False
  End Sub




  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    '########

    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If disposed Then Exit Sub
    DsetFarb.Dispose()
    MnDvPre.Dispose()
    MnDvPro.Dispose()
    MnDvPrb.Dispose()
    OleAdFarb.Dispose()
    disposed = True
    'GC.SuppressFinalize(Me)
  End Sub
  '
  '
  '
  '
  '
  '
  '
  'Properties
  '
  '
  '
  '
  '
  '
  '
  '
  ReadOnly Property TblFarb() As DataTable
    Get
      TblFarb = DsetFarb.Tables("TblFarb")
    End Get
  End Property
  '
  '
  '
  'DataView Preise
  '
  '
  ReadOnly Property DvPre() As DataView
    Get
      DvPre = MnDvPre
    End Get
  End Property

  '
  '
  'DataView Prozentigkeiten
  '
  '
  ReadOnly Property DvPro() As DataView
    Get
      DvPro = MnDvPro
    End Get
  End Property

  '
  '
  'DataView Bindemtte-Prozentigkeiten
  '
  '
  ReadOnly Property DvPrb() As DataView
    Get
      DvPrb = MnDvPrb
    End Get
  End Property
  '
  '
  '
  'Methoden
  '
  '
  '
  '
  '
  Public Sub DsetFarbCreate()
    '
    DsetFarb = New DataSet
    '
    If DsetFarb.Tables.Count > 0 Then Exit Sub

    '

    '
    'Tabelle Farb-/Bindemittel
    '
    '
    DsetFarb.Tables.Add("TblFarb")
    DsetFarb.Tables("TblFarb").Columns.Add("FARBM_ID", GetType(Integer))
    DsetFarb.Tables("TblFarb").Columns.Add("FARBM_NAME", GetType(String))
    DsetFarb.Tables("TblFarb").Columns("FARBM_ID").Unique = True
    DsetFarb.Tables("TblFarb").PrimaryKey = New DataColumn() {DsetFarb.Tables("TblFarb").Columns("FARBM_ID")}

    '
    'Tabelle Preise
    '
    DsetFarb.Tables.Add("TblPre")
    DsetFarb.Tables("TblPre").Columns.Add("FARBM_ID", GetType(Integer))
    DsetFarb.Tables("TblPre").Columns.Add("FARBM_IRPA", GetType(Integer))
    DsetFarb.Tables("TblPre").Columns.Add("FARBM_PREIS", GetType(Single))
    MnDvPre.Table = DsetFarb.Tables("TblPre")

    '
    '
    'Tabelle Prozentigkeiten
    '
    '
    '

    DsetFarb.Tables.Add("TblPro")
    DsetFarb.Tables("TblPro").Columns.Add("FARBM_ID", GetType(Integer))
    DsetFarb.Tables("TblPro").Columns.Add("FARBM_IRFA", GetType(Integer))
    DsetFarb.Tables("TblPro").Columns.Add("FARBM_PROZ", GetType(Single))
    MnDvPro.Table = DsetFarb.Tables("TblPro")

    '
    '
    'Tabelle Bindemittel-Prozentigkeiten
    '
    '
    '
    DsetFarb.Tables.Add("TblPrb")
    DsetFarb.Tables("TblPrb").Columns.Add("FARBM_ID", GetType(Integer))
    DsetFarb.Tables("TblPrb").Columns.Add("FARBM_IRBA", GetType(Integer))
    DsetFarb.Tables("TblPrb").Columns.Add("FARBM_PROB", GetType(Single))
    MnDvPrb.Table = DsetFarb.Tables("TblPrb")

    '
    '
    'Relation Farbmittel Preis
    '
    '
    '
    DsetFarb.Relations.Add(New DataRelation("RelFarbPre", DsetFarb.Tables("TblFarb").Columns("FARBM_ID"), DsetFarb.Tables("TblPre").Columns("FARBM_ID")))

    '
    '
    '
    'Relation Farbmittel Prozentigkeit
    '
    '
    DsetFarb.Relations.Add(New DataRelation("RelFarbPro", DsetFarb.Tables("TblFarb").Columns("FARBM_ID"), DsetFarb.Tables("TblPro").Columns("FARBM_ID")))
    '
    '
    '
    'Relation Farbmittel Bindemittel-Prozentigkeit
    '
    '
    '
    DsetFarb.Relations.Add(New DataRelation("RelFarbPrb", DsetFarb.Tables("TblFarb").Columns("FARBM_ID"), DsetFarb.Tables("TblPrb").Columns("FARBM_ID")))


  End Sub
  Public Sub DsetFarbFill(ByVal Allfarb As Short, ByVal AltName As Boolean)
    Dim i As Integer
    Dim WithMessg As String
    Dim OneGlzGrd As String
    Dim StrFarb As String
    Dim FarbmName As String
    DsetFarb.Clear()
    DsetFarb.Tables("TblPre").Rows.Clear()
    DsetFarb.Tables("TblPro").Rows.Clear()
    DsetFarb.Tables("TblPrb").Rows.Clear()
    DsetFarb.Tables("TblFarb").Rows.Clear()
    DsetFarb.Tables("TblPre").AcceptChanges()
    DsetFarb.Tables("TblPro").AcceptChanges()
    DsetFarb.Tables("TblPrb").AcceptChanges()
    DsetFarb.Tables("TblFarb").AcceptChanges()
    DsetFarb.AcceptChanges()
    OleAdFarb.SelectCommand = New OleDbCommand("", Cndat)
    '
    'Dataset füllen
    '
    '
    'allfarb=false ==> nur Farbmittel mit vorhandenen Grunddaten
    '
    '
    'Fill Farbmittel
    '

    'If BitWrt(27, MenueParam.User.Writ) Then
    WithMessg = "TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    'Else
    'WithMessg = "TBL_GRUND_FARBM.MESSG_ID=" & MenueParam.Messg.MessgID
    'End If
    '
    OneGlzGrd = " AND TBL_FARBM.FARBM_ID=GLZGRD_ID"
    '
    '
    If AltName Then
      FarbmName = "TBL_FARBM.FARBM_ANAME AS FARBM_NAME"
    Else
      FarbmName = "TBL_FARBM.FARBM_NAME AS FARBM_NAME"
    End If
    If Allfarb = 0 Then
      '
      'alle Farbmittel eines Mischsystems werden gelesen
      '
      OleAdFarb.SelectCommand.CommandText = "SELECT TBL_FARBM.FARBM_ID," & FarbmName _
      & " FROM TBL_FARBM " _
      & "WHERE TBL_FARBM.MISCH_ID=" & MenueParam.MischID & OneGlzGrd & " ORDER BY FARBM_NAME;"
    Else
      '
      '
      OleAdFarb.SelectCommand.CommandText = "SELECT TBL_FARBM.FARBM_ID," & FarbmName _
     & " FROM TBL_FARBM INNER JOIN TBL_GRUND_FARBM ON (TBL_FARBM.FARBM_ID = TBL_GRUND_FARBM.FARBM_ID) AND (TBL_FARBM.MISCH_ID = TBL_GRUND_FARBM.MISCH_ID)" _
     & "WHERE (((TBL_GRUND_FARBM.GKWRT_ID)=" & MenueParam.Misch.GKwrtID & ") AND ((TBL_FARBM.MISCH_ID)=" & MenueParam.MischID & "))" _
     & " AND " & WithMessg & OneGlzGrd & " ORDER BY FARBM_NAME;"
    End If
    DsetFarb.Tables("TblFarb").Rows.Clear()
    OleAdFarb.SelectCommand.Connection = Cndat()
    If Not FillDatset(OleAdFarb, DsetFarb, "TblFarb") Then
      Exit Sub
    End If
    DsetFarb.Tables("TblFarb").AcceptChanges()
    For i = 0 To DsetFarb.Tables("TblFarb").Rows.Count - 1
      DsetFarb.Tables("TblFarb").Rows(i)("FARBM_NAME") = Trim(DsetFarb.Tables("TblFarb").Rows(i)("FARBM_NAME"))
    Next
    StrFarb = StrLin(DsetFarb.Tables("TblFarb"), "FARBM_ID")
    '
    '
    '
    'Fill Preis
    '
    '
    '

    OleAdFarb.SelectCommand.CommandText = "SELECT TBL_FARBM_PREIS.FARBM_ID,TBL_FARBM_PREIS.FARBM_IRPA,TBL_FARBM_PREIS.FARBM_PREIS " _
  & "FROM TBL_FARBM_PREIS " _
  & " WHERE TBL_FARBM_PREIS.MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID IN " & StrFarb & " ORDER BY TBL_FARBM_PREIS.FARBM_ID,FARBM_IRPA;"
    OleAdFarb.SelectCommand.Connection = Cndat()
    If Not FillDatset(OleAdFarb, DsetFarb, "TblPre") Then
      Exit Sub
    End If
    '
    '
    '
    'Fill Prozentigkeit
    '
    '
    '
    OleAdFarb.SelectCommand.CommandText = "SELECT TBL_FARBM_PROZ.FARBM_ID,TBL_FARBM_PROZ.FARBM_IRFA,TBL_FARBM_PROZ.FARBM_PROZ " _
  & "FROM TBL_FARBM_PROZ " _
  & "WHERE  TBL_FARBM_PROZ.MISCH_ID =" & MenueParam.MischID & " AND FARBM_ID IN " & StrFarb & " ORDER BY TBL_FARBM_PROZ.FARBM_ID,FARBM_IRFA;"
    OleAdFarb.SelectCommand.Connection = Cndat()
    If Not FillDatset(OleAdFarb, DsetFarb, "TblPro") Then
      Exit Sub
    End If
    '
    '
    '
    'Fill Bindemittel-Prozentigkeit
    '
    '
    '
    OleAdFarb.SelectCommand.CommandText = "SELECT TBL_FARBM_PROB.FARBM_ID,TBL_FARBM_PROB.FARBM_IRBA,TBL_FARBM_PROB.FARBM_PROB " _
    & "FROM TBL_FARBM_PROB " _
    & "WHERE TBL_FARBM_PROB.MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID IN " & StrFarb & " ORDER BY TBL_FARBM_PROB.FARBM_ID,FARBM_IRBA;"
    OleAdFarb.SelectCommand.Connection = Cndat()
    If Not FillDatset(OleAdFarb, DsetFarb, "TblPrb") Then
      Exit Sub
    End If

  End Sub
  Sub AddNewFarb(ByVal Farbe As Colorant)
    Dim i As Short
    Dim RowFarb As DataRow
    With DsetFarb.Tables("TBLFARB")
      RowFarb = .NewRow
      RowFarb("FARBM_ID") = Farbe.ID
      RowFarb("FARBM_NAME") = Farbe.Name
      .Rows.Add(RowFarb)
    End With
    For i = 0 To Farbe.PreCount - 1
      With DsetFarb.Tables("TBLPRE")
        RowFarb = .NewRow
        RowFarb("FARBM_ID") = Farbe.ID
        RowFarb("FARBM_IRPA") = i
        RowFarb("FARBM_PREIS") = Farbe.Pre(i)
        .Rows.Add(RowFarb)
      End With
    Next i
    For i = 0 To Farbe.PrfCount - 1
      With DsetFarb.Tables("TBLPRO")
        RowFarb = .NewRow
        RowFarb("FARBM_ID") = Farbe.ID
        RowFarb("FARBM_IRFA") = i
        RowFarb("FARBM_PROZ") = Farbe.Prf(i)
        .Rows.Add(RowFarb)
      End With
    Next i
    For i = 0 To Farbe.PrbCount - 1
      With DsetFarb.Tables("TBLPRB")
        RowFarb = .NewRow
        RowFarb("FARBM_ID") = Farbe.ID
        RowFarb("FARBM_IRBA") = i
        RowFarb("FARBM_PROB") = Farbe.Prb(i)
        .Rows.Add(RowFarb)
      End With
    Next i
  End Sub
End Class

