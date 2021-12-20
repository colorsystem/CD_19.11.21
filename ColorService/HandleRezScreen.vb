Option Strict Off
Option Explicit On 
Option Compare Text
Friend Class HandleRezptScreen

  Implements IDisposable





  Dim disposed As Boolean
  Dim MnDatGrid As DataGrid

  Dim TDBCol As C1DataColumn
  Dim JNF As Short
  Dim JNM As Short
  Dim MnIer As Short
  Dim MnFullAend As Boolean 'True bei MNG volle Rezeptänderung möglich;False nur beschränkt änderbar
  Dim i As Short
  Dim j As Short
  Dim k As Short
  Dim Tol As Single
  Dim Amenge As Single
  Dim Fakt As Single
  Dim WithSmng As Boolean
  Dim BoLim As Boolean
  Dim ButtPre As Boolean
  Dim ButtPro As Boolean
  Dim ButtPrb As Boolean
  Dim ButtKto As Boolean
  Dim ButtOpr As Boolean
  Dim BoOPR As Boolean
  Dim BoKTO As Boolean
  Dim WrtAlt As Single
  Dim WrtNeu As Single
  Dim WrtProz As Single
  Dim OldRow As Integer
  Dim OldFaID As Integer
  Dim FaID As Integer
  Dim KeyIndex As String
  Dim RowIndex As Integer
  Dim KeyNam As String
  Dim MnDataChanged As Boolean
  Dim Ihilf As Short
  Dim KeyHilf(2) As String
  Dim KeId As String
  Dim FaNr As String
  Dim KeyNr As String
  Dim FirstRow As Object
  Dim MaxRow As Integer
  Dim MaxCol As Short
  Dim MnPicAuf As HandlePictures

  Dim MnMenueParam As AllParameters
  Dim MnCalcRezept As RezeptBerechnung
  Dim WithEvents MnTDBFar As C1TrueDBGrid
  Dim WithEvents MnTDBDropFar As C1TrueDBDropdown 'Dropdown für Farbmittelnamen

  Dim ValItem As C1.Win.C1TrueDBGrid.ValueItem()
  Dim MnImgButton As PictureBox  'Image für Rezeptberechnung
  Dim PrintAllow As Boolean  'Kennung, ob Drucken mit F4 erlaubt
  Dim WithEvents MnlstFAR As ListBox
  Dim WithEvents MnChkVOL As CheckBox
  Dim WithEvents MncboMNG As ComboBox
  Dim WithEvents MncboPRO_0 As ComboBox
  Dim WithEvents MncboPRO_1 As ComboBox
  Dim WithEvents MncboMIM_0 As ComboBox
  Dim WithEvents MncboMIM_1 As ComboBox
  Dim WithEvents MnTxtSUM As TextBox

  Dim Reb As Boolean
  Dim MngSUM As Single
  Dim MngHlf As Single
  Dim KbProb As Short  '0 Prob ist auf Binde- und Lösemittel bezogen; 1  ~ auf Gesamtmenge bezogen
  Dim MnAllRezepte As RecipesGrp
  Dim MnGrpRwerte As RefValuesGrp
  'Dim MnGrundDat As BCSwinParamStruct.GrundDaten
  Dim ReWrFarbe As ReadWriteFarbe
  Dim MnUmr As RezeptUmrechnung
  Dim MnDatenFarb As HandleDsetFarb
  Dim MnTDBDropPre As C1TrueDBDropdown  'Dropdown für Preise
  Dim MnTDBDropPro As C1TrueDBDropdown  'Dropdown für Farbm.-Prozentigkeiten
  Dim MnTDBDropPrb As C1TrueDBDropdown  'Dropdown für Bindem.-Prozentigkeiten
  Dim TblRezept As DataTable
  'Dim Ddat As DAO.Database
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

    'Für Support von Windows.Forms-Klassenkompositions-Designer
    MnUmr = New RezeptUmrechnung
    ReWrFarbe = New ReadWriteFarbe
    TblRezept = New DataTable
    'OnAdd = False
    OldFaID = -1
    KbProb = 0
    Tol = 0.0000001
    Ihilf = 0
    BoLim = False
    BoOPR = False
    BoKTO = False
    KeyIndex = ""
    RowIndex = -1
    MnDataChanged = False
    LayoutChanged = False
    TblRezept.Clear()
    PrintAllow = False
    If PrinterSettings.InstalledPrinters.Count > 0 Then
      PrintAllow = True
    End If
    disposed = False
  End Sub




  Protected Overrides Sub Finalize()


    MyBase.Finalize()

    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If disposed Then Exit Sub
    Try
      If BitInt(16, 16, MnMenueParam.User.Drum) = 1 Then
        If LayoutChanged Then
          imsg = MsgBox(Texxt(2949), 4, Texxt(2000))
          If imsg <> 7 Then
            For i = 0 To MnTDBFar.Splits(0, 0).DisplayColumns.Count - 1
              If Not MnTDBFar.Splits(0, 0).DisplayColumns(i).Visible Then
                MnTDBFar.Splits(0, 0).DisplayColumns(i).Width = 0
              End If
            Next i
            'MnTDBFar.SaveLayout(GridFileName())
          End If
        End If
      End If
      LayoutChanged = False
    Catch
    End Try
    MnUmr.dispose()
    ReWrFarbe.dispose()
    MnTDBDropFar.Dispose()
    MnTDBDropPre.Dispose()
    MnTDBDropPro.Dispose()
    MnTDBDropPrb.Dispose()

    TblRezept.Dispose()
    disposed = True
    'GC.SuppressFinalize(Me)
  End Sub


  ReadOnly Property ier() As Short
    Get
      ier = MnIer
    End Get
  End Property

  Property DataChanged() As Boolean
    Get
      DataChanged = MnDataChanged
    End Get
    Set(ByVal Value As Boolean)
      MnDataChanged = Value
    End Set
  End Property


  Property FullAend() As Boolean
    Get
      FullAend = MnFullAend
    End Get
    Set(ByVal Value As Boolean)
      MnFullAend = Value
    End Set
  End Property
  Property DatenFarb() As HandleDsetFarb
    Get
      DatenFarb = MnDatenFarb
    End Get
    Set(ByVal AcDatenFarb As HandleDsetFarb)
      MnDatenFarb = AcDatenFarb
    End Set
  End Property
  Property MenueParam() As AllParameters
    Get
      MenueParam = MnMenueParam
    End Get
    Set(ByVal AcMenueParam As AllParameters)
      MnMenueParam = AcMenueParam
      KbProb = MenueParam.Misch.Bprob
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
  'Property GrundDat() As BCSwinParamStruct.GrundDaten
  '	Get		'Optische Konstanten (Summe)
  '		GrundDat = MnGrundDat
  '	End Get
  '	Set(ByVal Value As BCSwinParamStruct.GrundDaten)
  '		MnGrundDat = Value
  '	End Set
  'End Property
  '
  Property TDBFar() As C1TrueDBGrid
    Get
      TDBFar = MnTDBFar
    End Get
    Set(ByVal Value As C1TrueDBGrid)
      MnTDBFar = Value
      MnTDBFar.AllowSort = False
      MnTDBFar.FetchRowStyles = True
      MnTDBFar.Splits(0, 0).ColumnCaptionHeight = 2 * MnTDBFar.Splits(0, 0).ColumnCaptionHeight
    End Set
  End Property
  '
  '
  '
  '
  Property TDBDropFAR() As C1TrueDBDropdown
    Get
      TDBDropFAR = MnTDBDropFar
    End Get
    Set(ByVal Value As C1TrueDBDropdown)
      MnTDBDropFar = Value
      MnTDBDropFar.Size = New Size(30, 100)
    End Set
  End Property
  '
  '
  '
  '
  Property TDBDropPre() As C1TrueDBDropdown
    Get
      TDBDropPre = MnTDBDropPre
    End Get
    Set(ByVal Value As C1TrueDBDropdown)
      MnTDBDropPre = Value
      MnTDBDropPre.ColumnHeaders = False
      MnTDBDropPre.HScrollBar.Style = ScrollBarStyleEnum.None
    End Set
  End Property
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
      MnTDBDropPro.ColumnHeaders = False
      MnTDBDropPro.HScrollBar.Style = ScrollBarStyleEnum.None
    End Set
  End Property
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
      MnTDBDropPrb.ColumnHeaders = False
      MnTDBDropPrb.HScrollBar.Style = ScrollBarStyleEnum.None
    End Set
  End Property
  '
  '
  '

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
  ' 
  '
  '
  '
  '
  '
  '
  '
  '
  Property txtSUM() As TextBox
    Get
      txtSUM = MnTxtSUM
    End Get
    Set(ByVal Value As TextBox)
      MnTxtSUM = Value
      MnTxtSUM.TextAlign = HorizontalAlignment.Right
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
  '
  ReadOnly Property Umr() As RezeptUmrechnung
    Get
      Umr = MnUmr
    End Get
  End Property
  '
  '

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

  Sub TDBFarClear()
    If Not IsNothing(MnlstFAR) Then
      MnlstFAR.DataSource = Nothing
    End If
    MnTDBFar.DataSource = Nothing
    MnTDBDropFar.DataSource = Nothing
    MnTDBDropPre.DataSource = Nothing
    MnTDBDropPro.DataSource = Nothing
    MnTDBDropPrb.DataSource = Nothing
  End Sub


  '
  Sub TDBFarGridRezept(ByRef ier As Short)
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
    MnUmr.Logwarn = BitInt(28, 29, MnMenueParam.User.Writ)
    KbProb = MnMenueParam.Misch.Bprob

    '



    '
    '
    '
    ier = 0
    MaxCol = TblRezept.Columns.Count
    MaxRow = 0
    '
    '
    'Spezialangaben für die verschiedenen Mengenangaben
    '
    '
    '
    '

    '
    '
    '
    MnTDBFar.DataSource = TblRezept
    If Not IsNothing(MnlstFAR) Then
      MnlstFAR.DataSource = DatenFarb.TblFarb
      MnlstFAR.DisplayMember = "FARBM_NAME"
      MnlstFAR.ValueMember = "FARBM_ID"

      'MnlstFAR.Update()
    End If

    '
    Select Case KeyNam
      Case "MNG", "NEU", "SOR", "BAS", "COL", "KOR", "HLF", "ZEI", "KOO", "RZP"
        MnTDBFar.Splits(0, 0).DisplayColumns(KeyNam).Width = Weite(2)

        '
        'Farbmittelname
        '
        'Call TDBFarResize()
        If Not IsNothing(TblRezept.Columns("FID")) Then
          With MnTDBFar.Columns("FID")


            MnTDBDropFar.DataSource = DatenFarb.TblFarb
            MnTDBDropFar.Update()
            MnTDBDropFar.Columns.Add(New C1DataColumn)

            MnTDBDropFar.ValueMember = "FARBM_ID"
            MnTDBDropFar.DisplayMember = "FARBM_NAME"
            MnTDBDropFar.ValueTranslate = True
            MnTDBDropFar.Width = Weite(1)
            MnTDBDropFar.DisplayColumns("FARBM_NAME").Width = Weite(1) - 24
            MnTDBDropFar.DisplayColumns("FARBM_ID").Locked = True
            MnTDBDropFar.DisplayColumns("FARBM_ID").Visible = False
            MnTDBDropFar.DisplayColumns("FARBM_ID").Width = 0
            MnTDBDropFar.ColumnHeaders = False
            .DropDown = MnTDBDropFar
          End With


        End If
        If Not IsNothing(TblRezept.Columns("PRE")) Then
          With MnTDBFar.Columns("PRE")
            .DropDown = MnTDBDropPre
            .DropDown.DataSource = DatenFarb.DvPre
            .DropDown.ValueMember = "FARBM_PREIS"
            .DropDown.DisplayMember = "FARBM_ID"
            .DropDown.DisplayColumns("FARBM_ID").Width = 0
            .DropDown.DisplayColumns("FARBM_ID").Locked = True
            .DropDown.DisplayColumns("FARBM_PREIS").Width = Weite(2) - 24
            .DropDown.Width = Weite(2)
            .DropDown.ColumnHeaders = False
          End With


        End If
        If Not IsNothing(TblRezept.Columns("PRO")) Then
          With MnTDBFar.Columns("PRO")
            .DropDown = MnTDBDropPro
            .DropDown.DataSource = DatenFarb.DvPro
            .DropDown.ValueMember = "FARBM_PROZ"
            .DropDown.DisplayMember = "FARBM_ID"
            .DropDown.DisplayColumns("FARBM_ID").Width = 0
            .DropDown.DisplayColumns("FARBM_ID").Locked = True
            .DropDown.DisplayColumns("FARBM_PROZ").Width = Weite(2) - 24
            .DropDown.Width = Weite(2)
            .DropDown.ColumnHeaders = False
          End With


        End If
        If Not IsNothing(TblRezept.Columns("PRB")) Then
          With MnTDBFar.Columns("PRB")
            .DropDown = MnTDBDropPrb
            .DropDown.DataSource = DatenFarb.DvPrb
            .DropDown.ValueMember = "FARBM_PROB"
            .DropDown.DisplayMember = "FARBM_ID"
            .DropDown.DisplayColumns("FARBM_ID").Width = 0
            .DropDown.DisplayColumns("FARBM_ID").Locked = True
            .DropDown.DisplayColumns("FARBM_PROB").Width = Weite(2) - 24
            .DropDown.Width = Weite(2)
            .DropDown.ColumnHeaders = False
          End With


        End If
    End Select

    'MnTDBFar.ApproxCount = 0
    'MnTDBFar.HeadLines = 2
    '
    Select Case KeyNam
      Case "MNG", "NEU", "BAS", "SOR", "COL"
        If BoOPR Then
          With MnTDBFar.Columns("OPR").ValueItems
            .Values.Clear()
            .MaxComboItems = 5
            .Presentation = 2
            .Translate = False
            .Validate = True
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem(" ", " "))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("=", "="))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("<", "<"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem(">", ">"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("*", "*"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("+", "+"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("/", "/"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem(":", ":"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("\", "\"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("A", "A"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("B", "B"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("C", "C"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("1", "1"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("2", "2"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("3", "3"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("X", "X"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("Y", "Y"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("Z", "Z"))
            .DefaultItem = 0
            'MnTDBFar.Splits(0, 0).DisplayColumns("OPR").Style.BackColor = Color.LightGray
          End With
        End If

        If BoKTO Then
          With MnTDBFar.Columns("KTO").ValueItems
            .Values.Clear()
            .MaxComboItems = 5
            .Presentation = 2
            .Translate = False
            .Validate = True
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem(" ", " "))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("1", "1"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("2", "2"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("3", "3"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("4", "4"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("5", "5"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("6", "6"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("7", "7"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("8", "8"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("9", "9"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("A", "A"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("B", "B"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("C", "C"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("D", "D"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("E", "E"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("F", "F"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("G", "G"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("I", "I"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("J", "J"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("M", "M"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("N", "N"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("S", "S"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("T", "T"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("V", "V"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("W", "W"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("X", "X"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("Y", "Y"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("(", "("))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem(")", ")"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("[", "["))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("]", "]"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("{", "{"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("}", "}"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem("<", "<"))
            .Values.Add(New C1.Win.C1TrueDBGrid.ValueItem(">", ">"))
            .DefaultItem = 0
          End With
        End If
    End Select
    '
    '
    '
    '
    MnTDBFar.AllowColSelect = False
    MnTDBFar.Splits(0, 0).Style.VerticalAlignment = AlignVertEnum.Center

    '
    For i = 0 To TblRezept.Columns.Count - 1
      KeyIndex = TblRezept.Columns(i).ColumnName
      MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Style.VerticalAlignment = AlignVertEnum.Center
      MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Style.HorizontalAlignment = AlignHorzEnum.Far
      MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).HeadingStyle.HorizontalAlignment = AlignHorzEnum.Far
      With MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex)
        .AllowSizing = True
        .AllowFocus = True
        .ButtonHeader = False

        Select Case KeyIndex
          '
          '
          '
          '
          Case "FID"               'Farbmittel-ID
            '
            .Locked = False
            '.Button = False
            .Visible = True
            .Width = Weite(1)
            .AutoComplete = True
            .AutoDropDown = True
            .Style.HorizontalAlignment = AlignHorzEnum.Far

            '
            '
            '
          Case "ALT"                   'Menge alt
            .Style.HorizontalAlignment = AlignHorzEnum.Far
            .Button = False
            .Width = Weite(2)
            .Locked = True
          Case "ZUW"                   'Zuwaage
            .Style.HorizontalAlignment = AlignHorzEnum.Far
            .Button = False
            .Width = Weite(2)
            .Locked = True
          Case "MZU"                   'Menge(alt)+Zuwaage
            .Style.HorizontalAlignment = AlignHorzEnum.Far
            .Button = False
            .Width = Weite(2)
            .Locked = True
          Case "PRG"                   'Prgramm starten
            MnTDBFar.Columns(KeyIndex).ValueItems.Presentation = PresentationEnum.RadioButton
            .Button = True
            .Locked = False
            .Width = Weite(0)
            .ButtonAlways = True
          Case "OPR"                   'Operator
            .Button = True
            .Locked = False
            .Width = Weite(3)
          Case "LIM"                   'Limitierungsmenge
            .Style.HorizontalAlignment = AlignHorzEnum.Far
            .Button = False
            .Locked = False
            .Width = Weite(4)
          Case "KTO"                   'Topfangabe
            .Locked = False
            .Button = True
            .Width = Weite(3)
          Case "PRE"                   'Preis
            .Style.HorizontalAlignment = AlignHorzEnum.Far
            .Locked = False
            .Button = True
            .Width = Weite(2)
          Case "PRO"                   'Prozentigkeit
            .Style.HorizontalAlignment = AlignHorzEnum.Far
            .Locked = False
            .Button = True
            .Width = Weite(2)
          Case "PRB"                   'Bindemittel-Prozentigkeit
            .Style.HorizontalAlignment = AlignHorzEnum.Far
            .Locked = False
            .Button = True
            .Width = Weite(2)
          Case "FST"
          Case "BEL"
          Case "FRM"
            '
        End Select
      End With
    Next i
    'MnTDBFar.ExtendRightColumn = True
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
    'Öffnen des Layoutfiles
    '
    '
    '
    '

    Try
      If BitInt(17, 17, MnMenueParam.User.Drum) = 1 Then
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

  '






  Public Sub TblRezeptRebind()
    Dim Irow As Short
    Dim Icol As Short
    Dim ivol As Short
    Dim KeyIndex As String
    Dim RowRezpt As DataRow
    ivol = MnAllRezepte.IVOL
    TDBFARTxtAllSum()

    For j = 0 To 6
      HeaderText(j) = RepTexxt(Texxt(640 + j), Texxt(602 + ivol))
      HeadLiText(j) = RepTexxt(Texxt(640 + j), Texxt(605 + ivol))
    Next j

    MnTDBFar.Columns(KeyNam).Caption = HeaderText(MnAllRezepte.INF)

    If BoLim Then
      MnTDBFar.Columns("LIM").Caption = HeadLiText(MnAllRezepte.INM)
    End If
    '
    'Speichern von MnAllRezepte.Rezepte(KeyNam) nach Tabxxx  
    '
    TblRezept.Rows.Clear()
    '
    '
    For Irow = 0 To MnAllRezepte.Rezepte(KeyNam).KF - 1
      If TblRezept.Rows.Count <= Irow Then
        RowRezpt = TblRezept.NewRow
      Else
        RowRezpt = TblRezept.Rows(Irow)
      End If
      FaNr = KeyRe(Irow)
      KeId = KeyName(MnAllRezepte.Rezepte(KeyNam)(FaNr).ID)
      For Icol = 0 To TblRezept.Columns.Count - 1
        KeyIndex = TblRezept.Columns(Icol).ColumnName

        Select Case KeyIndex
          '
          '
          Case "FID"               'Farbmittel-ID
            '
            '
            RowRezpt("FID") = MnAllRezepte.Rezepte(KeyNam)(FaNr).ID
            '
            '
            '
            '
            '
          Case "MNG", "SOR", "BAS", "KOR", "KOO", "NEU", "HLF", "RZP", "COL", "ZEI", "ALT"                     'Menge
            RowRezpt(KeyIndex) = Format(MnAllRezepte.Rezepte(KeyIndex)(FaNr).BaAmng, MnAllRezepte.Farben(KeId).Form)
          Case "ZUW"                   'Zuwaage
            If MnAllRezepte.Rezepte("ZUW").KF > 0 Then
              If Abs(MnAllRezepte.Rezepte("ZUW")(KeyNr).BaAmng + 999.99) > 0.001 Then
                RowRezpt(KeyIndex) = Format(MnAllRezepte.Rezepte("ZUW")(FaNr).BaAmng, MnAllRezepte.Farben(KeId).Form)
              Else
                RowRezpt(KeyIndex) = -99999999
              End If
            Else
              RowRezpt(KeyIndex) = 0.0#
            End If
          Case "MZU"                   'Menge(alt)+Zuwaage
            If MnAllRezepte.Rezepte("MZU").KF > 0 Then
              If Abs(MnAllRezepte.Rezepte("MZU")(KeId).BaAmng + 999.99) > 0.001 Then
                RowRezpt(KeyIndex) = Format(MnAllRezepte.Rezepte("MZU")(FaNr).BaAmng, MnAllRezepte.Farben(KeId).Form)
              Else
                RowRezpt(KeyIndex) = -99999999
              End If
            Else
              RowRezpt(KeyIndex) = Format(0.0#, MnAllRezepte.Farben(KeId).Form)
            End If
          Case "OPR"                   'Operator
            RowRezpt(KeyIndex) = MnAllRezepte.Farben(KeId).OP
          Case "LIM"                   'Limitierungsmenge
            RowRezpt(KeyIndex) = Format(MnAllRezepte.Rezepte(KeyNam).UmMng * MnAllRezepte.Farben(KeId).BoMng, MnAllRezepte.Farben(KeId).Form)
          Case "KTO"                   'Topfangabe
            RowRezpt(KeyIndex) = MnAllRezepte.Farben(KeId).Kto
          Case "PRE"                   'Preis
            RowRezpt(KeyIndex) = MnAllRezepte.Farben(KeId).Preis
          Case "PRO"                   'Prozentigkeit
            RowRezpt(KeyIndex) = Format(MnAllRezepte.Rezepte(KeyNam)(FaNr).Proz, "###.00")
          Case "PRB"                   'Bindemittel-Prozentigkeit
            If KbProb = 1 Then
              RowRezpt(KeyIndex) = Format(MnAllRezepte.Rezepte(KeyNam)(FaNr).Prob * (1.0# - 0.01 * MnAllRezepte.Rezepte(KeyIndex)(FaNr).Proz), "###.00")
            Else
              RowRezpt(KeyIndex) = Format(MnAllRezepte.Rezepte(KeyNam)(FaNr).Prob, "###.00")
            End If
          Case "FST"                   'Farbstärke
            RowRezpt(KeyIndex) = MnAllRezepte.Farben(KeId).Fst
          Case "BEL"                   'Gewicht für Minimierung
            RowRezpt(KeyIndex) = MnAllRezepte.Farben(KeId).Bel
          Case "FRM"                   'Ausgabeformat
            RowRezpt(KeyIndex) = MnAllRezepte.Farben(KeId).Form
            '
        End Select

      Next Icol
      If TblRezept.Rows.Count <= Irow Then
        TblRezept.Rows.Add(RowRezpt)
        MnTDBFar.Bookmark = TblRezept.Rows.Count - 1
      End If

    Next Irow
  End Sub



  Sub NewKeynam(ByVal Value As String)

    Dim i As Short
    KeyNam = Value
    MnTDBFar.AllowAddNew = True
    MnTDBFar.AllowUpdate = True
    MnTDBFar.AllowDelete = True
    MnTxtSUM.Visible = False
    MnTDBFar.ColumnFooters = True
    MnTDBFar.Splits(0, 0).DisplayColumns(KeyNam).FooterStyle.HorizontalAlignment = AlignHorzEnum.Far



    With MnTDBFar.Splits(0, 0).DisplayColumns(KeyNam)
      .Style.HorizontalAlignment = AlignHorzEnum.Far

      .Button = False

      Select Case KeyNam


        Case "MNG"               'Menge
          .Locked = True
          MnTDBFar.AllowAddNew = False
          MnTDBFar.AllowUpdate = False
          MnTDBFar.AllowDelete = False
        Case "SOR"               'Menge
          'MnTDBFar.ColumnFooters = False
          .Locked = False
          MnTDBFar.ColumnFooters = False
        Case "BAS"               'Menge
          .Locked = False
          MnTDBFar.ColumnFooters = True
        Case "KOR"               'Menge
          MnTDBFar.AllowAddNew = False
          MnTDBFar.AllowUpdate = False
          MnTDBFar.AllowDelete = False
          .Locked = True
        Case "NEU"               'Menge
          .Locked = False
        Case "COL"               'Menge
          MnTDBFar.AllowAddNew = False
          MnTDBFar.AllowUpdate = False
          MnTDBFar.AllowDelete = False
          .Locked = True
        Case "RZP"               'Menge
          .Locked = True
          MnTDBFar.AllowAddNew = False
          MnTDBFar.AllowUpdate = False
          MnTDBFar.AllowDelete = False
        Case "HLF"               'Menge
          .Button = False
          .Locked = False
        Case "ZEI"               'Menge
          .Button = False
          .Locked = True
          MnTDBFar.AllowAddNew = False
          MnTDBFar.AllowUpdate = False
          MnTDBFar.AllowDelete = False
      End Select
    End With
    '
    Select Case KeyNam
      Case "MNG", "RZP", "ZEI", "COL"

        '
        'FabmittelID (Name)
        '
        '
        '
        For i = 0 To MnTDBFar.Columns.Count - 1
          KeyIndex = MnTDBFar.Columns(i).DataField
          With MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex)

            Select Case KeyIndex

              Case "FID"
                .Button = False
                .Locked = True
                .FooterStyle.HorizontalAlignment = AlignHorzEnum.Far
                MnTDBFar.Columns("FID").FooterText = Texxt(825)
                '
                '
              Case "PRE"
                'FabmittelPreis
                '
                '
                '
                ButtPre = False
                .Button = ButtPre
                .Locked = True

                '
              Case "PRO"
                '
                'FabmittelProzentigkeit
                '
                '
                '
                ButtPro = False
                .Button = ButtPro
                .Locked = True

                '
              Case "PRB"
                '
                'FabmittelBindemittelProzentigkeit
                '
                '
                '
                ButtPrb = False
                .Button = ButtPrb
                .Locked = True

                '
              Case "KTO"

                '
                'Töpfe
                '
                '
                '
                ButtKto = False
                .Button = ButtKto
                .Locked = True

                '
                '
              Case "OPR"
                'Operator
                '
                '
                '
                ButtOpr = False
                .Button = ButtOpr
                .Locked = True
            End Select
          End With
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
      Case "SOR", "BAS", "KOR", "KOO", "NEU", "HLF"
        For i = 0 To MnTDBFar.Columns.Count - 1
          KeyIndex = MnTDBFar.Columns(i).DataField
          With MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex)

            Select Case KeyIndex
              '
              'FabmittelID (Name)
              '
              '
              '
              Case "FID"
                .Button = True
                .Locked = False
                .FooterStyle.HorizontalAlignment = AlignHorzEnum.Far
                MnTDBFar.Columns("FID").FooterText = Texxt(825)
                '
                '
                'FabmittelPreis
                '
                '
                '
              Case "PRE"
                ButtPre = True
                .Button = ButtPre
                .Locked = False

                '
                '
                'FabmittelProzentigkeit
                '
                '
                '
              Case "PRO"
                ButtPro = True
                .Button = ButtPro
                .Locked = False

                '
                '
                'FabmittelBindemittelProzentigkeit
                '
                '
                '
              Case "PRB"
                ButtPrb = True
                .Button = ButtPrb
                .Locked = False

                '
                '
                'Töpfe
                '
                '
                '
              Case "KTO"
                ButtKto = True
                .Button = ButtKto
                .Locked = False

                '
                '
                'Operator
                '
                '
                '
              Case "OPR"
                ButtOpr = True
                .Button = ButtOpr
                .Locked = False
            End Select
          End With
        Next i
    End Select
  End Sub
  Sub TblRezeptAddColumn(ByRef Colnam As String)

    Select Case Colnam
      '
      '
      '
      '
      Case "FID"      'Farbmittel-ID
        '
        TblRezept.Columns.Add("FID", GetType(Integer))
        TblRezept.Columns("FID").Unique = True
        TblRezept.PrimaryKey = New DataColumn() {TblRezept.Columns("FID")}

        '
        '
        '
        '
        '
        '
        '
      Case "MNG"          'Menge
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0
        KeyNam = Colnam
        MnAllRezepte.Rezepte.AddRez(Colnam, New Recipe)
        MnAllRezepte.Rezepte.AddRez("NEU", MnAllRezepte.Rezepte(Colnam))
      Case "SOR"          'Menge
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0
        KeyNam = Colnam
        MnAllRezepte.Rezepte.AddRez(Colnam, New Recipe)
      Case "BAS"          'Menge
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0
        KeyNam = Colnam
        MnAllRezepte.Rezepte.AddRez(Colnam, New Recipe)
      Case "KOR"          'Menge
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0
        KeyNam = Colnam
        MnAllRezepte.Rezepte.AddRez(Colnam, New Recipe)
        MnAllRezepte.Rezepte.AddRez("KOO", MnAllRezepte.Rezepte(Colnam))
        MnAllRezepte.Rezepte.AddRez("ALT", MnAllRezepte.Rezepte("MNG"))
        MnAllRezepte.Rezepte.AddRez("COL", MnAllRezepte.Rezepte("MNG"))
        MnAllRezepte.Rezepte.AddRez("ZUW", New Recipe)
        MnAllRezepte.Rezepte.AddRez("MZU", New Recipe)
      Case "NEU"          'Menge
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0
        KeyNam = Colnam
        MnAllRezepte.Rezepte.AddRez(Colnam, New Recipe)
        MnAllRezepte.Rezepte.AddRez("MNG", MnAllRezepte.Rezepte(Colnam))
      Case "COL"          'Menge
        TblRezept.Columns.Add(Colnam, GetType(Single))
        TblRezept.Columns(Colnam).DefaultValue = 0
        KeyNam = Colnam
      Case "RZP"          'Menge
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0
        KeyNam = Colnam
        MnAllRezepte.Rezepte.AddRez(Colnam, New Recipe)
      Case "HLF"          'Menge
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0
        KeyNam = Colnam
        MnAllRezepte.Rezepte.AddRez(Colnam, New Recipe)
      Case "ZEI"          'Menge
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0
        KeyNam = Colnam
        MnAllRezepte.Rezepte.AddRez(Colnam, New Recipe)
      Case "ALT"          'Menge alt
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0
        KeyNam = Colnam
        KeyHilf(Ihilf) = "ALT"
        Ihilf = Ihilf + 1
      Case "ZUW"          'Zuwaage
        KeyHilf(Ihilf) = "ZUW"
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0
        Ihilf = Ihilf + 1
      Case "MZU"          'Menge(alt)+Zuwaage
        KeyHilf(Ihilf) = "MZU"
        TblRezept.Columns.Add(Colnam, GetType(String))
        Ihilf = Ihilf + 1
      Case "PRG"          'Programm starten
        If BitInt(1, 1, MnMenueParam.User.Sonst) = 1 Then
          TblRezept.Columns.Add(Colnam, GetType(String))
        End If
      Case "OPR"          'Operator
        BoOPR = True
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = " "
        '
      Case "LIM"          'Limitierungsmenge
        BoLim = True
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0.0

      Case "KTO"          'Topfangabe
        BoKTO = True
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = " "

      Case "PRE"          'Preis
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 0.0

      Case "PRO"          'Prozentigkeit
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 100.0

      Case "PRB"          'Bindemittel-Prozentigkeit
        TblRezept.Columns.Add(Colnam, GetType(String))
        TblRezept.Columns(Colnam).DefaultValue = 100.0
      Case "FST"
      Case "BEL"
      Case "FRM"
        '
    End Select


  End Sub

  Sub TDBFarCapColumn()
    Dim i As Short
    For i = 0 To MnTDBFar.Columns.Count - 1
      TDBCol = MnTDBFar.Columns(i)
      KeyIndex = TblRezept.Columns(i).ColumnName
      Select Case KeyIndex
        '
        '
        '
        '
        Case "FID"          'Farbmittel-ID
          '
          With TDBCol
            .Caption = Texxt(815)
          End With

          '
          '
          '
          '
        Case "MNG"              'Menge
          With TDBCol
            .Caption = HeaderText(JNF)
          End With


        Case "SOR"              'Menge
          With TDBCol
            .Caption = HeaderText(JNF)
          End With
        Case "BAS"              'Menge
          With TDBCol
            .Caption = HeaderText(JNF)
          End With
        Case "KOR"              'Menge
          With TDBCol
            .Caption = HeaderText(JNF)
          End With
        Case "NEU"              'Menge
          With TDBCol
            .Caption = HeaderText(JNF)
          End With
        Case "COL"              'Menge
          With TDBCol
            .Caption = HeaderText(JNF)
          End With
        Case "RZP"              'Menge
          With TDBCol
            .Caption = HeaderText(JNF)
          End With
        Case "HLF"              'Menge
          With TDBCol
            .Caption = HeaderText(JNF)
          End With
        Case "ZEI"              'Menge
          With TDBCol
            .Caption = HeaderText(JNF)
          End With
        Case "ALT"              'Menge alt
          With TDBCol
            .Caption = Texxt(826)
          End With
        Case "ZUW"              'Zuwaage
          With TDBCol
            .Caption = Texxt(827)
          End With
        Case "MZU"              'Menge(alt)+Zuwaage
          With TDBCol
            .Caption = Texxt(836)
          End With
        Case "OPR"              'Operator
          With TDBCol
            .Caption = Texxt(817)
          End With
        Case "PRG"              'Operator
          With TDBCol
            .Caption = Space(1)
          End With
          '
        Case "LIM"              'Limitierungsmenge
          With TDBCol
            .Caption = HeadLiText(JNM)
          End With
        Case "KTO"              'Topfangabe
          With TDBCol
            .Caption = Texxt(819)
          End With
        Case "PRE"              'Preis
          With TDBCol
            .Caption = Texxt(822)
          End With

        Case "PRO"              'Prozentigkeit
          With TDBCol
            .Caption = Texxt(820)
            '.NumberFormat = "FormatText Event"
            '.NumberFormat = "General Number"
            '.NumberFormat = "Fixed"

          End With

        Case "PRB"              'Bindemittel-Prozentigkeit
          With TDBCol
            .Caption = Texxt(821)
          End With
        Case "FST"
        Case "BEL"
        Case "FRM"
          '
      End Select
    Next i

  End Sub
  '
  '
  '


  Sub TDBFarRezStart()
    Dim j As Short
    Dim ivol As Short
    JNF = MnAllRezepte.INF
    If JNF < 0 Or JNF > 6 Then JNF = 0
    JNM = MnAllRezepte.INM
    If JNM < 0 Or JNM > 2 Then JNM = 0
    TblRezept.Rows.Clear()
    Select Case KeyNam
      Case "NEU", "SOR"
        If MnAllRezepte.INF > 2 Then
          MnAllRezepte.INF = 0
        End If
    End Select
    FirstRow = MnTDBFar.FirstRow
    Call TDBFarCapColumn()
    '
    '
    '
    '
    Application.DoEvents()

    Call TblRezeptRebind()

    MaxRow = MnAllRezepte.Rezepte(KeyNam).KF
    Application.DoEvents()
    If TblRezept.Rows.Count > 0 Then
      MnTDBFar.RefetchRow(0)
    End If
    If Not IsDBNull(FirstRow) And FirstRow > 0 Then
      MnTDBFar.FirstRow = 1
      MnTDBFar.FirstRow = FirstRow
    End If
  End Sub
  Public Sub TDBFARTxtAllSum()
    MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
    MnTDBFar.Columns(KeyNam).FooterText = CharHilf(Umr.MngINF(KeyNam, MnAllRezepte, ier))
    If KeyNam = "KOR" Or KeyNam = "KOO" Then
      Call TDBFARRezUmrKOR()
    End If
  End Sub
  '
  '
  '
  Sub FarbmNeu(ByVal FaID As Integer, ByVal RowNeu As Integer)
    Dim i As Short
    Dim k As Short
    MnIer = 0
    KeId = KeyName(FaID)
    FaNr = KeyRe(RowNeu)
    If RowNeu > MnAllRezepte.Rezepte(KeyNam).KF - 1 Then
      For k = 0 To MnAllRezepte.Rezepte(KeyNam).KF - 1
        If FaID = MnAllRezepte.Rezepte(KeyNam)(k).ID Then
          MessageBox.Show(Texxt(2954))
          MnIer = 2954
          Exit Sub
        End If
      Next k
      MnAllRezepte.Rezepte(KeyNam).AddFaNr(FaNr, New ColorAmount)
      '
      '
      '
      '
      '
      If KeyNam = "KOR" Or KeyNam = "KOO" Then
        '
        'zusätzliches Farb-/Bindemittel auch fü "ALT","ZUW" und "MZU"
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

    'Prüfen, auf neue Farb-/Bindemittel
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
        ReWrFarbe.FarRea(MnMenueParam, FaID, MnAllRezepte.Farben(KeId), WithSmng, MnIer)
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
          MnIer = 99
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
      MnAllRezepte.Rezepte(KeyNam)(FaNr).FaAmng = MnAllRezepte.Farben(KeId).Smenge
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

      MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
      '
      '
      '
      '

      Select Case KeyNam

        Case "KOR", "KOO"
          '
          'zusätzliches Farb-/Bindemittel auch fü "ALT","ZUW" und "MZU"
          '
          '
          For i = 0 To 2
            MnAllRezepte.Rezepte(KeyHilf(i))(KeId).ID = FaID
            MnAllRezepte.Rezepte(KeyHilf(i))(KeId).FaAmng = MnAllRezepte.Farben(KeId).Smenge
            MnAllRezepte.Rezepte(KeyHilf(i))(KeId).Proz = MnAllRezepte.Farben(KeId).Prf(0)
            MnAllRezepte.Rezepte(KeyHilf(i))(KeId).Prob = MnAllRezepte.Farben(KeId).Prb(0)
            MnUmr.CalcFamng(KeyHilf(i), MnAllRezepte, ier)
          Next
          'Neue Reflexionswerte für Korrekturrezept berechnen
          '
          '
          'CalcRezept.CalcMischKorrektur(0, Ddat, MenueParam, MenueParam.User.Winkel, "ALT", KeyNam, MnAllRezepte, GrpRwerte, GrundDat, MnIer)
          Picauf.Refresh()
        Case "BAS"

          'Neue Reflexionswerte für Korrekturrezept berechnen
          '
          '
          'CalcRezept.CalcMischRezept(0, Ddat, MenueParam, MenueParam.User.Winkel, KeyNam, MnAllRezepte, GrpRwerte, GrundDat, MnIer)
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
      If MnAllRezepte.Rezepte(KeyHilf(i)).KF > 0 Then
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
    Dim Fakt As Single
    Dim i As Integer
    Dim MngHlf As Single
    MngHlf = MnUmr.MngINO(Rzkey, Rezpte, Ier)
    If Ier > 0 Then Exit Sub
    If MngHlf < MngHlf.Epsilon Then Exit Sub
    Fakt = NewNormMng / MngHlf
    For i = 0 To Rezpte.Rezepte(Rzkey).KF - 1
      Rezpte.Rezepte(Rzkey)(i).BaAmng = Fakt * Rezpte.Rezepte(Rzkey)(i).BaAmng
    Next i
    Rezpte.Rezepte(Rzkey).UmMng = Fakt * Rezpte.Rezepte(Rzkey).UmMng
    Call MnUmr.CalcFamng(Rzkey, Rezpte, Ier)
    If Rzkey = "KOR" Or Rzkey = "KOO" Then
      For j = 0 To 2
        If Rezpte.Rezepte(KeyHilf(j)).KF > 0 Then
          For i = 0 To Rezpte.Rezepte(KeyHilf(j)).KF - 1
            If Abs(Rezpte.Rezepte(KeyHilf(j))(i).BaAmng + 999.99) > 0.001 Then
              Rezpte.Rezepte(KeyHilf(j))(i).BaAmng = Fakt * Rezpte.Rezepte(KeyHilf(j))(i).BaAmng
            End If
          Next i
          MnAllRezepte.Rezepte(KeyHilf(j)).UmMng = Fakt * MnAllRezepte.Rezepte(KeyHilf(j)).UmMng
          MnUmr.CalcFamng(KeyHilf(j), MnAllRezepte, Ier)
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
    If NewReiMng < NewReiMng.Epsilon Then Exit Sub
    If Rezpte.INF > 2 Then Exit Sub
    MngHlf = MnUmr.MngINR(Rzkey, Rezpte, Ier)
    If Ier > 0 Then Exit Sub
    If MngHlf < MngHlf.Epsilon Then Exit Sub
    Fakt = NewReiMng / MngHlf
    Fakt = NewReiMng / MngHlf
    For i = 0 To Rezpte.Rezepte(Rzkey).KF - 1
      Rezpte.Rezepte(Rzkey)(i).FaAmng = Fakt * Rezpte.Rezepte(Rzkey)(i).FaAmng
    Next i
  End Sub




  Sub TDBFarPrint()
    MnTDBFar.PrintInfo.PageHeader = MnAllRezepte.Rezepte((MnTDBFar.Columns(CShort(MnTDBFar.Tag)).DataField)).Name
    MnTDBFar.PrintInfo.Print()
  End Sub
  '
  '
  '
  '
  '
  'Ereignisse für Tabelle
  '
  '


  '








  Private Sub MnTDBFar_ColResize(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.ColResizeEventArgs) Handles MnTDBFar.ColResize
    MnTxtSUM.Visible = False
    KeyIndex = MnTDBFar.Columns(e.ColIndex).DataField
    If BitInt(17, 17, MnMenueParam.User.Drum) = 1 Then
      LayoutChanged = True
    End If
    Select Case KeyIndex
      Case "FID"
        MnTDBDropFar.Width = MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Width
        MnTDBDropFar.DisplayColumns("FARBM_NAME").Width = MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Width - 24
      Case "PRE"
        MnTDBDropPre.Width = MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Width
        MnTDBDropPre.DisplayColumns("FARBM_PREIS").Width = MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Width - 24
      Case "PRO"
        MnTDBDropPro.Width = MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Width
        MnTDBDropPro.DisplayColumns("FARBM_PROZ").Width = MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Width - 24
      Case "PRB"
        MnTDBDropPrb.Width = MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Width
        MnTDBDropPrb.DisplayColumns("FARBM_PROB").Width = MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Width - 24
      Case "KTO"
      Case "OPR"
    End Select
  End Sub










  Private Sub MnTDBFar_ButtonClick(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.ColEventArgs) Handles MnTDBFar.ButtonClick
    Dim OpAlt As String
    Dim Limalt As Single
    Dim KeyJndex As String


    KeyJndex = TblRezept.Columns(e.ColIndex).ColumnName
    If KeyJndex <> "PRG" Then Exit Sub
    Select Case KeyNam
      Case "BAS", "KOR", "KOO"
        FaID = TblRezept.Rows(MnTDBFar.Bookmark)("FID")
        KeId = KeyName(FaID)
        OpAlt = MnAllRezepte.Farben(KeId).OP
        Limalt = MnAllRezepte.Farben(KeId).BoMng
        MnAllRezepte.Farben(KeId).OP = "="
        TblRezept.Rows(MnTDBFar.Bookmark)(KeyNam) = MnTDBFar.Columns(KeyNam).Value
        MnAllRezepte.Farben(KeId).BoMng = MnTDBFar.Columns(KeyNam).Value
        Select Case KeyNam
          Case "BAS"
            'CalcRezept.CalcBasisRezept(0, Ddat, MnMenueParam, MnMenueParam.User.Winkel, KeyNam, KeyNam, MnAllRezepte, MnGrpRwerte, MnIer)
          Case "KOR", "KOO"
            'CalcRezept.CalcKorrekturRezept(0, Ddat, MnMenueParam, MnMenueParam.User.Winkel, "ALT", KeyNam, MnAllRezepte, MnGrpRwerte, MnGrundDat, MnIer)
        End Select
        MnAllRezepte.Farben(KeId).OP = OpAlt
        MnAllRezepte.Farben(KeId).BoMng = Limalt
        Call TblRezeptRebind()
        Picauf.Refresh()
    End Select
  End Sub












  Private Sub MnTDBFar_BeforeColUpdate(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.BeforeColUpdateEventArgs) Handles MnTDBFar.BeforeColUpdate
    '
    Dim UserVal As Object
    Dim k As Integer
    Dim ier As Short
    Dim Uier As Boolean
    Reb = False
    Dim val As Single
    MnIer = 0
    KeyIndex = TblRezept.Columns(e.ColIndex).ColumnName
    RowIndex = MnTDBFar.Bookmark
    If RowIndex > TblRezept.Rows.Count And KeyIndex <> "FID" Then
      e.Cancel = True
      MnTDBFar.DataChanged = False
      Exit Sub
    End If


    If KeyIndex <> TblRezept.Columns(e.ColIndex).ColumnName Then
      e.Cancel = True
      MnTDBFar.DataChanged = False
      Exit Sub
    End If
    If RowIndex > MnAllRezepte.Rezepte(KeyNam).KF Then
      e.Cancel = True
      MnTDBFar.DataChanged = False
      Exit Sub
    End If

    '
    '
    KeyNr = KeyName(RowIndex + 1)
    UserVal = MnTDBFar.Columns(KeyIndex).Value
    If KeyIndex <> "FID" Then
      FaID = TblRezept.Rows(RowIndex)("FID")
      KeId = KeyName(FaID)
    Else
      If Not IsNumeric(MnTDBDropFar.Columns("FARBM_ID").Value) Then
        e.Cancel = True
        MnTDBFar.DataChanged = False
        Exit Sub
      End If
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
        'Prüfen, ob zusätzliches  Farb-/Bindemittel
        '
        '
        k = RowIndex
        FaID = CInt(MnTDBDropFar.Columns("FARBM_ID").Value)

        '
        Call FarbmNeu(FaID, k)
        If MnIer <> 0 Then
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        '

        '
        'Menge
      Case "MNG", "SOR", "BAS", "KOR", "KOO", "NEU", "COL", "RZP", "ZEI", "HLF", "ALT"
        If Not IsNumeric(MnTDBFar.Columns(e.ColIndex).Value) Then
          MessageBox.Show(Texxt(16))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        If CInt(MnTDBFar.Columns(e.ColIndex).Value) < 0 Then
          MessageBox.Show(Texxt(4105))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        '
        '
        '
        '
        '
        '
        '
        Select Case KeyIndex
          Case "MNG"                  'Menge
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
            'CalcRezept.CalcMischRezept(0, Ddat, MnMenueParam, MnMenueParam.User.Winkel, KeyNam, MnAllRezepte, MnGrpRwerte, MnGrundDat, MnIer)
            Picauf.Refresh()

          Case "KOR", "KOO"                   'Menge
            WrtAlt = MnAllRezepte.Rezepte(KeyNam)(KeyNr).BaAmng
            MnAllRezepte.Rezepte(KeyIndex)(KeyNr).BaAmng = Singl(UserVal)
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
            '
            'Neue Reflexionswerte berechnen
            '
            '
            'CalcRezept.CalcMischKorrektur(0, Ddat, MnMenueParam, MnMenueParam.User.Winkel, "ALT", KeyNam, MnAllRezepte, MnGrpRwerte, MnGrundDat, MnIer)
            Picauf.Refresh()



          Case "NEU"                  'Menge
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
        For i = 0 To MnTDBFar.Columns(KeyIndex).ValueItems.Values.Count - 1
          If MnTDBFar.Columns(e.ColIndex).Value = MnTDBFar.Columns(KeyIndex).ValueItems.Values(i).Value Then
            Exit For
          End If
        Next
        If i = MnTDBFar.Columns(KeyIndex).ValueItems.Values.Count Then
          MessageBox.Show(Texxt(16))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        '
        '
        '
        '
        '
        MnAllRezepte.Farben(KeId).OP = UserVal

        '
      Case "LIM"           'Limitierungsmenge
        If Not IsNumeric(MnTDBFar.Columns(e.ColIndex).Value) Then
          MessageBox.Show(Texxt(16))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        If CInt(MnTDBFar.Columns(e.ColIndex).Value) < 0 Then
          MessageBox.Show(Texxt(4105))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        '
        '
        '
        '
        MnAllRezepte.Farben(KeId).BoMng = Singl(UserVal) / MnAllRezepte.Rezepte(KeyNam).UmMng

      Case "KTO"           'Topfangabe
        For i = 0 To MnTDBFar.Columns(KeyIndex).ValueItems.Values.Count - 1
          If MnTDBFar.Columns(e.ColIndex).Value = MnTDBFar.Columns(KeyIndex).ValueItems.Values(i).Value Then
            Exit For
          End If
        Next
        If i = MnTDBFar.Columns(KeyIndex).ValueItems.Values.Count Then
          MessageBox.Show(Texxt(16))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        '
        '
        '
        MnAllRezepte.Farben(KeId).Kto = UserVal

      Case "PRE"           'Preis
        If Not IsNumeric(MnTDBFar.Columns(e.ColIndex).Value) Then
          MessageBox.Show(Texxt(16))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        If CSng(MnTDBFar.Columns(e.ColIndex).Value) < 0 Then
          MessageBox.Show(Texxt(3989))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        '
        '
        '
        '
        '
        '
        val = MnTDBFar.Columns(e.ColIndex).Value
        MnTDBFar.Columns(e.ColIndex).Value = Format(val, "##0.00")
        MnAllRezepte.Farben(KeId).Preis = Singl(UserVal)
        Select Case KeyNam
          Case "KOR", "RZP", "BAS", "KOO"
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
        If Not IsNumeric(MnTDBFar.Columns(e.ColIndex).Value) Then
          MessageBox.Show(Texxt(16))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        If CInt(MnTDBFar.Columns(e.ColIndex).Value) <= 0 Or CInt(MnTDBFar.Columns(e.ColIndex).Value) > 100 Then
          MessageBox.Show(Texxt(3990))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        '
        '
        '
        '
        '
        '
        WrtAlt = MnAllRezepte.Rezepte(KeyNam)(KeyNr).Proz
        WrtNeu = Singl(UserVal)
        MnAllRezepte.Rezepte(KeyNam)(KeyNr).Proz = WrtNeu
        If MnAllRezepte.Farben(KeId).Ichf = 1 Then
          '
          '
          'Für Bindemittel wird PROZ=PROB gesetzt
          '
          '
          MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob = WrtNeu
        End If
        Select Case KeyNam
          Case "SOR", "NEU"
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
          Case "KOR", "KOO"
            For i = 0 To 2
              If MnAllRezepte.Rezepte(KeyHilf(i)).KF > 0 Then
                MnAllRezepte.Rezepte(KeyHilf(i))(KeyNr).Proz = WrtNeu
                If MnAllRezepte.Farben(KeId).Ichf = 1 Then
                  '
                  '
                  'Für Bindemittel wird PROZ=PROB gesetzt
                  '
                  '
                  MnAllRezepte.Rezepte(KeyHilf(i))(KeyNr).Prob = WrtNeu
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
            If Uier = True Then
              For i = 0 To 2
                MnTDBFar.Columns(KeyNam).FooterText = CharHilf(Umr.MngINF(KeyHilf(i), MnAllRezepte, ier))
              Next i
            Else
              '
              '
              '
              'Fehler
              '
              '
              '
              MnAllRezepte.Rezepte(KeyNam)(KeyNr).Proz = WrtAlt
              If MnAllRezepte.Farben(KeId).Ichf = 1 Then
                MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob = WrtAlt
              End If
              MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
              For i = 0 To 2
                MnAllRezepte.Rezepte(KeyHilf(i))(KeyNr).Proz = WrtAlt
                If MnAllRezepte.Farben(KeId).Ichf = 1 Then
                  MnAllRezepte.Rezepte(KeyHilf(i))(KeyNr).Prob = WrtAlt
                End If
                MnUmr.CalcBamng(KeyHilf(i), MnAllRezepte, ier)
              Next i
              '
              '
              e.Cancel = True
              MnTDBFar.DataChanged = False
              Exit Sub
            End If
            Reb = True

          Case Else
            MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
            If ier <> 0 Then
              MnAllRezepte.Rezepte(KeyNam)(KeyNr).Proz = WrtAlt
              If MnAllRezepte.Farben(KeId).Ichf = 1 Then
                MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob = WrtAlt
              End If
              e.Cancel = True
              MnTDBFar.DataChanged = False
              Exit Sub

            End If
            Reb = True

        End Select
        '
        '

        val = MnTDBFar.Columns(e.ColIndex).Value
        MnTDBFar.Columns(e.ColIndex).Value = Format(val, "##0.00")
        '
        '
        '
        '
        '
      Case "PRB"           'Bindemittel-Prozentigkeit
        If Not IsNumeric(MnTDBFar.Columns(e.ColIndex).Value) Then
          MessageBox.Show(Texxt(16))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        If CSng(MnTDBFar.Columns(e.ColIndex).Value) <= 0 Or CSng(MnTDBFar.Columns(e.ColIndex).Value) > 100 Then
          MessageBox.Show(Texxt(3990))
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If
        '
        '
        If MnAllRezepte.Farben(KeId).Ichf = 1 Then
          e.Cancel = True
          MnTDBFar.DataChanged = False
          Exit Sub
        End If

        '
        '
        '

        WrtAlt = MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob
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
              MsgBox(Texxt(3991))
              e.Cancel = True
              MnTDBFar.DataChanged = False
              Exit Sub
            End If
            WrtNeu = WrtNeu / (1.0# - 0.01 * WrtProz)
            If WrtNeu > 100 Or WrtNeu < 0 Then
              MsgBox(Texxt(3991))
              e.Cancel = True
              MnTDBFar.DataChanged = False
              Exit Sub
            End If
          Else
            WrtNeu = 0.0#
          End If
        End If
        MnAllRezepte.Rezepte(KeyNam)(KeyNr).Prob = WrtNeu
        Select Case KeyNam
          Case "SOR", "NEU"
            MnUmr.CalcFamng(KeyNam, MnAllRezepte, ier)
          Case "KOR"
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
            If Uier = True Then
              For i = 0 To 2
                MnTDBFar.Columns(KeyNam).FooterText = CharHilf(Umr.MngINF(KeyHilf(i), MnAllRezepte, ier))
              Next i
            Else
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
              e.Cancel = True
              MnTDBFar.DataChanged = False
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
              e.Cancel = True
              MnTDBFar.DataChanged = False
              Exit Sub
            End If
            Reb = True

        End Select
        '
        '
        '

        val = MnTDBFar.Columns(e.ColIndex).Value
        MnTDBFar.Columns(e.ColIndex).Value = Format(val, "##0.00")
      Case "FST"           'Farbstärke
        MnAllRezepte.Farben(KeId).Fst = Singl(UserVal)
      Case "BEL"           'Gewicht für Minimierung
        MnAllRezepte.Farben(KeId).Bel = Singl(UserVal)
      Case "FRM"           'Ausgabeformat
        MnAllRezepte.Farben(KeId).Form = UserVal

        '
    End Select
    MnDataChanged = True

  End Sub















  Private Sub MnTDBFar_RowColChange(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.RowColChangeEventArgs) Handles MnTDBFar.RowColChange
    KeyIndex = TblRezept.Columns(MnTDBFar.Col).ColumnName

    '
    'Prüfen ob Zeile vorhanden
    '
    If MnTDBFar.Bookmark > TblRezept.Rows.Count - 1 And KeyIndex <> "FID" Then
      Select Case KeyIndex
        Case "PRE", "PRO", "PRB", "KTO", "OPR"
          MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Button = False
      End Select
      Exit Sub
    Else
      Select Case KeyIndex
        Case "PRE"
          MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Button = ButtPre
        Case "PRO"
          MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Button = ButtPro
        Case "PRB"
          MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Button = ButtPrb
        Case "KTO"
          MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Button = ButtKto
        Case "OPR"
          MnTDBFar.Splits(0, 0).DisplayColumns(KeyIndex).Button = ButtOpr
      End Select
    End If
    '
    '
    '
    '
    '
    '
    '
    '
    'Aufbau der DataViews
    '
    '
    '
    '
    If Not IsDBNull(MnTDBFar.Columns("FID").Value) And IsNumeric(MnTDBFar.Columns("FID").Value) Then
      Select Case KeyIndex
        Case "PRE"
          DatenFarb.DvPre.RowFilter = "FARBM_ID =" & MnTDBFar.Columns("FID").Value
        Case "PRO"
          DatenFarb.DvPro.RowFilter = "FARBM_ID =" & MnTDBFar.Columns("FID").Value
        Case "PRB"
          DatenFarb.DvPrb.RowFilter = "FARBM_ID =" & MnTDBFar.Columns("FID").Value
        Case "KTO", "OPR"
      End Select
    End If
  End Sub







  Private Sub MnTDBFar_AfterDelete(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnTDBFar.AfterDelete
    Dim k As Short
    Dim i As Short
    If RowIndex < 0 Or RowIndex > MnAllRezepte.Rezepte(KeyNam).KF - 1 Then Exit Sub
    Amenge = MnUmr.MngINR(KeyNam, MnAllRezepte, ier)
    For k = RowIndex To MnAllRezepte.Rezepte(KeyNam).KF - 2
      MnAllRezepte.Rezepte(KeyNam)(k).ID = MnAllRezepte.Rezepte(KeyNam)(k + 1).ID
      MnAllRezepte.Rezepte(KeyNam)(k).FaAmng = MnAllRezepte.Rezepte(KeyNam)(k + 1).FaAmng
      MnAllRezepte.Rezepte(KeyNam)(k).BaAmng = MnAllRezepte.Rezepte(KeyNam)(k + 1).BaAmng
      MnAllRezepte.Rezepte(KeyNam)(k).Prob = MnAllRezepte.Rezepte(KeyNam)(k + 1).Prob
      MnAllRezepte.Rezepte(KeyNam)(k).Proz = MnAllRezepte.Rezepte(KeyNam)(k + 1).Proz
    Next k
    MnAllRezepte.Rezepte(KeyNam).RemoveFaNr(KeyRe(MnAllRezepte.Rezepte(KeyNam).KF - 1))
    Select Case KeyNam
      Case "KOR", "KOO"
        For i = 0 To 2

          For k = RowIndex To MnAllRezepte.Rezepte(KeyNam).KF - 2
            MnAllRezepte.Rezepte(KeyHilf(i))(k).ID = MnAllRezepte.Rezepte(KeyHilf(i))(k + 1).ID
            MnAllRezepte.Rezepte(KeyHilf(i))(k).FaAmng = MnAllRezepte.Rezepte(KeyHilf(i))(k + 1).FaAmng
            MnAllRezepte.Rezepte(KeyHilf(i))(k).BaAmng = MnAllRezepte.Rezepte(KeyHilf(i))(k + 1).BaAmng
            MnAllRezepte.Rezepte(KeyHilf(i))(k).Prob = MnAllRezepte.Rezepte(KeyHilf(i))(k + 1).Prob
            MnAllRezepte.Rezepte(KeyHilf(i))(k).Proz = MnAllRezepte.Rezepte(KeyHilf(i))(k + 1).Proz
          Next k
          MnAllRezepte.Rezepte(KeyHilf(i)).RemoveFaNr(KeyRe(MnAllRezepte.Rezepte(KeyNam).KF - 1))

        Next i
      Case "NEU", "SOR", "ZEI", "BAS"
      Case Else
        '
        'Menge umrechnen
        Call NormFamng(KeyNam, MnAllRezepte, Amenge, ier)

        '
    End Select
    MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
    Call TDBFARTxtAllSum()
    Select Case KeyNam
      Case "BAS"           'Menge
        Call NormFamng(KeyNam, MnAllRezepte, Amenge, ier)
        '
        '
        'Neue Reflexionswerte berechnen
        '
        '
        'CalcRezept.CalcMischRezept(0, Ddat, MnMenueParam, MnMenueParam.User.Winkel, KeyNam, MnAllRezepte, MnGrpRwerte, MnGrundDat, MnIer)
        Picauf.Refresh()

      Case "KOR", "KOO"            'Menge
        Call NormFamng(KeyNam, MnAllRezepte, Amenge, ier)
        '
        'Neue Reflexionswerte berechnen
        '
        '
        'CalcRezept.CalcMischKorrektur(0, Ddat, MnMenueParam, MnMenueParam.User.Winkel, "ALT", KeyNam, MnAllRezepte, MnGrpRwerte, MnGrundDat, MnIer)
        Picauf.Refresh()
    End Select



    Call Change(sender, e)
  End Sub


  Private Sub MnTDBFar_BeforeDelete(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.CancelEventArgs) Handles MnTDBFar.BeforeDelete
    RowIndex = MnTDBFar.Bookmark
    If RowIndex < 0 Or RowIndex > MnAllRezepte.Rezepte(KeyNam).KF - 1 Then
      e.Cancel = True
      Exit Sub
    End If
    KeyIndex = TblRezept.Columns(MnTDBFar.Col).ColumnName
  End Sub
  '
  '
  '
  '



  Private Sub MncboMNG_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles MncboMNG.SelectedIndexChanged
    MnAllRezepte.INO = MncboMNG.SelectedIndex
  End Sub

  Private Sub MncboPRO_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles MncboPRO_0.SelectedIndexChanged, MncboPRO_1.SelectedIndexChanged
    MnAllRezepte.INP = MncboPRO_0.SelectedIndex
    MnAllRezepte.INQ = MncboPRO_1.SelectedIndex

  End Sub



  Private Sub MnChkVOL_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnChkVOL.CheckStateChanged
    Dim MnIVOL As Short     'Volumen (1) Menge (0)
    Dim i As Short
    Dim Ivol As Short
    If MnChkVOL.CheckState = CheckState.Indeterminate Then Exit Sub

    '
    '
    '
    '
    '
    '
    '
    '
    '
    Ivol = CInt(MnChkVOL.CheckState)

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
    End If
    '
    '
    'Art der Mengen-/Volumenverhältnisse
    '
    '
    If Not IsNothing(MncboPRO_0) And Not IsNothing(MncboPRO_1) Then
      MncboPRO_0.Items.Clear()
      MncboPRO_1.Items.Clear()
      For i = 0 To 22
        MncboPRO_0.Items.Add(RepTexxt(Texxt(580 + i), Texxt(602 + Ivol)))
        MncboPRO_1.Items.Add(RepTexxt(Texxt(580 + i), Texxt(602 + Ivol)))
      Next i
      '
      MncboPRO_0.SelectedIndex = MnAllRezepte.INP
      MncboPRO_1.SelectedIndex = MnAllRezepte.INP
    End If
    If Ivol = MnAllRezepte.IVOL Then Exit Sub
    MnAllRezepte.IVOL = Ivol
    '
    '
    'Tabelle rückspeichern
    '
    '
    '
    TblRezeptRebind()
    '
    '
    '
    '
    '
  End Sub

  Private Sub MnTDBFar_HeadClick(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.ColEventArgs) Handles MnTDBFar.HeadClick
    Dim KNF As Short
    Dim KNM As Short
    Dim ivol As Short
    ivol = MnAllRezepte.IVOL
    KNF = MnAllRezepte.INF
    KNM = MnAllRezepte.INM
    MnDataChanged = True
    KeyIndex = MnTDBFar.Columns(e.ColIndex).DataField
    JNF = 0
    Select Case KeyIndex
      Case "FID"
        '
        '
        ' MENGE zurücksetzen
        '
        '
        JNF = 0
        MnTDBFar.Columns(KeyNam).Caption = HeaderText(JNF)
        MnAllRezepte.INF = JNF
        '
        '
        'Lim.-Menge zurücksetzen
        '
        '
        If BoLim Then
          JNM = 0
          MnAllRezepte.INM = JNM
          MnTDBFar.Columns("LIM").Caption = HeadLiText(JNM)
        End If
      Case "LIM"
        If BitInt(12, 12, MnMenueParam.User.Sonst) = 1 Then
          JNM = MnAllRezepte.INM + 1
          If JNM > 2 Then JNM = 0
          MnAllRezepte.INM = JNM
          MnTDBFar.Columns(KeyIndex).Caption = HeadLiText(JNM)
        End If
      Case "NEU", "SOR", "BAS", "RZP", "KOR", "MNG", "HLF", "COL", "ZEI", "KOO"
        Select Case KeyNam
          Case "NEU", "SOR", "BAS"
            If BitInt(11, 11, MnMenueParam.User.Sonst) = 1 Then
              JNF = MnAllRezepte.INF + 1
              If JNF > 2 Then JNF = 0
              MnTDBFar.Columns(KeyNam).Caption = HeaderText(JNF)
              MnAllRezepte.INF = JNF
            End If
          Case "RZP", "KOR", "MNG", "HLF", "COL", "ZEI", "KOO"
            If BitInt(11, 11, MnMenueParam.User.Sonst) = 1 Then
              JNF = MnAllRezepte.INF + 1
              If JNF > 6 Then JNF = 0
              MnTDBFar.Columns(KeyNam).Caption = HeaderText(JNF)
              MnAllRezepte.INF = JNF
            End If

        End Select
    End Select
    '
    '
    'Umrechnen
    '
    '
    '
    Select Case KeyNam
      Case "BAS", "RZP", "KOR", "MNG", "HLF", "COL", "ZEI", "KOO"
        MnUmr.CalcBamng(KeyNam, MnAllRezepte, ier)
        If ier <> 0 Then
          MnAllRezepte.INF = KNF
          MnAllRezepte.INM = KNM
          MnTDBFar.Columns(KeyNam).Caption = HeaderText(KNF)
          If BoLim Then
            MnTDBFar.Columns("LIM").Caption = HeadLiText(KNM)
          End If
          TblRezeptRebind()
          Exit Sub
        End If
        Call TDBFARTxtAllSum()
    End Select
    'MnAllerezepte====>Tabelle 
    TblRezeptRebind()

  End Sub
  Private Sub Change(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  MncboMNG.TextChanged, MncboPRO_0.TextChanged, MncboPRO_1.TextChanged, _
   MnTDBFar.AfterUpdate, _
   MnTDBFar.Change
    '
    If Reb Then
      Reb = False
      Call TblRezeptRebind()
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
      MnTDBFar.RefetchRow(TblRezept.Rows.Count - 1)
    End If
    MnDataChanged = True
  End Sub




  '

  '
  '
  '
  '
  '
  '

  Private Sub MnTDBFar_FootClick(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.ColEventArgs) Handles MnTDBFar.FootClick
    Dim X As Single
    Dim Y As Single
    Dim Wi As Single
    MnTxtSUM.Visible = True
    MnTxtSUM.Text = MnTDBFar.Columns(KeyNam).FooterText
    MnTxtSUM.Focus()
    X = MnTDBFar.Location.X
    Y = MnTDBFar.Location.Y
    Wi = MnTDBFar.Splits(0, 0).RecordSelectorWidth
    For i = 0 To MnTDBFar.Columns.Count - 1
      If TblRezept.Columns(i).ColumnName = KeyNam Then
        Exit For
      End If
      Wi = Wi + MnTDBFar.Splits(0, 0).DisplayColumns(i).Width
    Next
    X = X + Wi + 3
    Y = Y - 4 + MnTDBFar.Height - MnTDBFar.Splits(0, 0).ColumnFooterHeight
    If MnTDBFar.Splits(0, 0).HScrollBar.Visible Then
      Y = Y - MnTDBFar.Splits(0, 0).HScrollBar.Height
    End If
    MnTDBFar.LeftCol = 0
    MnTxtSUM.SetBounds(X, Y, MnTDBFar.Splits(0, 0).DisplayColumns(KeyNam).Width, MnTDBFar.Splits(0, 0).ColumnFooterHeight)
  End Sub

  Private Sub MnTDBFar_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnTDBFar.Resize
    MnTxtSUM.Visible = False
  End Sub

  Private Sub MnTxtSUM_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles MnTxtSUM.KeyPress
    Dim KeyAscii As Short
    KeyAscii = Asc(e.KeyChar)
    If KeyAscii = 13 Then
      Call NormBamng(MnTDBFar.Columns(KeyNam).DataField, MnAllRezepte, Singl(MnTxtSUM.Text), ier)
      TblRezeptRebind()
      MnTDBFar.LeftCol = 0
    End If
  End Sub

  Private Sub MnTxtSUM_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnTxtSUM.Leave
    Call MnTxtSUM_KeyPress(sender, New KeyPressEventArgs(Chr(13)))
    MnTxtSUM.Visible = False
  End Sub


  Private Sub MnTDBFar_BeforeColEdit(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.BeforeColEditEventArgs) Handles MnTDBFar.BeforeColEdit
    Try
      If TblRezept.Rows.Count = MnTDBFar.Bookmark Then
        MnTDBFar.FirstRow = MnTDBFar.FirstRow + 1
      End If
    Finally
    End Try
  End Sub




  Private Sub MnTDBFar_FetchRowStyle(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.FetchRowStyleEventArgs) Handles MnTDBFar.FetchRowStyle
    If e.Row < TblRezept.Rows.Count Then
      If TblRezept.Rows(e.Row).RowState = DataRowState.Detached Or TblRezept.Rows(e.Row).RowState = DataRowState.Deleted Then Exit Sub
      e.CellStyle.BackColor = Color.FromArgb(MnAllRezepte.Farben(KeyName(TblRezept.Rows(e.Row)("FID"))).FarbID)
      e.CellStyle.BackColor = Color.Beige
    End If
  End Sub


  Private Sub MnTDBFar_FormatText(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.FormatTextEventArgs) Handles MnTDBFar.FormatText
    'If e.Column.DataField = "PRO" And IsNumeric(e.Value) Then
    'e.Column.Text = FormatNumber(e.Value, 2, TriState.UseDefault)
    'End If
  End Sub



  Private Sub MnTDBFar_Scroll(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.CancelEventArgs) Handles MnTDBFar.Scroll
    MnTxtSUM.Visible = False
  End Sub




  Private Sub MnTDBFar_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnTDBFar.DoubleClick
    '
    '
    '
    'Beforedelete und Afterdelete werden nicht automatisch ausgelöst
    '
    '
    '
    ' Exit Sub
    If TblRezept.Rows.Count = 0 Then Exit Sub
    If MnTDBFar.AllowDelete = False Then Exit Sub
    RowIndex = MnTDBFar.Bookmark
    If RowIndex >= TblRezept.Rows.Count Then Exit Sub
    KeyIndex = TblRezept.Columns(MnTDBFar.Col).ColumnName
    Call MnTDBFar_AfterDelete(sender, e)
    MnTDBFar.Delete()
  End Sub








  Private Sub MnlstFAR_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnlstFAR.Click, MnTDBDropFar.Click
    Dim Rowneu As Integer
    If sender.name = "LSTFAR" Then
      Rowneu = MnAllRezepte.Rezepte(KeyNam).KF
    Else
      Rowneu = MnTDBFar.Row
    End If
    Call FarbmNeu(sender.SelectedValue, Rowneu)
    If MnIer <> 0 Then Exit Sub
    Call TblRezeptRebind()
    If TblRezept.Rows.Count > 0 Then
      MnTDBFar.RefetchRow(TblRezept.Rows.Count - 1)
    End If
    MnTDBFar.FirstRow = MnTDBFar.FirstRow + 1
    MnDataChanged = True
  End Sub

  Private Sub MncboMIM_0_ControlAdded(ByVal sender As Object, ByVal e As System.Windows.Forms.ControlEventArgs) Handles MncboMIM_0.ControlAdded

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
Friend Class HandleDsetFarb

	Implements IDisposable
	Dim disposed As Boolean
	Dim DsetFarb As New DataSet
	Dim MnDvPre As New DataView
	Dim MnDvPro As New DataView
	Dim MnDvPrb As New DataView
	Dim OleAdFarb As New OleDbDataAdapter
    Dim MnMenueparam As AllParameters

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
    Property MenueParam() As AllParameters
        Get
            MenueParam = MnMenueparam
        End Get
        Set(ByVal Value As AllParameters)
            MnMenueparam = Value
        End Set
    End Property
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
	Public Sub DsetFarbFill()
		DsetFarb.Tables("TblPre").Clear()
		DsetFarb.Tables("TblPro").Clear()
		DsetFarb.Tables("TblPrb").Clear()
		DsetFarb.Tables("TblFarb").Clear()
        OleAdFarb.SelectCommand = New OleDbCommand("", Cndat)
		'
		'Dataset füllen
		'
		'
		'
		'
		'Fill Farbmittel
		'
		'
        OleAdFarb.SelectCommand.CommandText = "SELECT TBL_FARBM.FARBM_ID, TBL_FARBM.FARBM_NAME " _
       & "FROM TBL_FARBM INNER JOIN TBL_GRUND_FARBM ON (TBL_FARBM.FARBM_ID = TBL_GRUND_FARBM.FARBM_ID) AND (TBL_FARBM.MISCH_ID = TBL_GRUND_FARBM.MISCH_ID)" _
       & "WHERE (((TBL_GRUND_FARBM.GKWRT_ID)=" & MnMenueparam.Misch.GKwrtID & ") AND ((TBL_FARBM.MISCH_ID)=" & MnMenueparam.MischID & "))ORDER BY FARBM_NAME;"
        OleAdFarb.SelectCommand.Connection = Cndat()

        If Not FillDatset(OleAdFarb, DsetFarb, "TblFarb") Then
            Exit Sub
        End If
        '
        '
        '
        'Fill Preis
        '
        '
        '
        OleAdFarb.SelectCommand.CommandText = "SELECT TBL_FARBM_PREIS.FARBM_ID,TBL_FARBM_PREIS.FARBM_PREIS " _
        & "FROM TBL_FARBM_PREIS INNER JOIN TBL_GRUND_FARBM ON (TBL_FARBM_PREIS.FARBM_ID = TBL_GRUND_FARBM.FARBM_ID) AND (TBL_FARBM_PREIS.MISCH_ID = TBL_GRUND_FARBM.MISCH_ID)" _
        & "WHERE (((TBL_FARBM_PREIS.MISCH_ID)=" & MnMenueparam.MischID & ") AND ((TBL_GRUND_FARBM.GKWRT_ID)=" & MnMenueparam.Misch.GKwrtID & ")) ORDER BY TBL_FARBM_PREIS.FARBM_ID,FARBM_IRPA;"

        OleAdFarb.SelectCommand.Connection = Cndat()
        If Not FillDatset(OleAdFarb, DsetFarb, "TblPre") Then
            Exit Sub
        End If


        OleAdFarb.SelectCommand.CommandText = "SELECT TBL_FARBM_PROZ.FARBM_ID,TBL_FARBM_PROZ.FARBM_PROZ " _
        & "FROM TBL_FARBM_PROZ INNER JOIN TBL_GRUND_FARBM ON (TBL_FARBM_PROZ.FARBM_ID = TBL_GRUND_FARBM.FARBM_ID) AND (TBL_FARBM_PROZ.MISCH_ID = TBL_GRUND_FARBM.MISCH_ID)" _
        & "WHERE (((TBL_FARBM_PROZ.MISCH_ID)=" & MnMenueparam.MischID & ") AND ((TBL_GRUND_FARBM.GKWRT_ID)=" & MnMenueparam.Misch.GKwrtID & ")) ORDER BY TBL_FARBM_PROZ.FARBM_ID,FARBM_IRFA;"

        OleAdFarb.SelectCommand.Connection = Cndat()
        If Not FillDatset(OleAdFarb, DsetFarb, "TblPro") Then
            Exit Sub
        End If


        OleAdFarb.SelectCommand.CommandText = "SELECT TBL_FARBM_PROB.FARBM_ID,TBL_FARBM_PROB.FARBM_PROB " _
        & "FROM TBL_FARBM_PROB INNER JOIN TBL_GRUND_FARBM ON (TBL_FARBM_PROB.FARBM_ID = TBL_GRUND_FARBM.FARBM_ID) AND (TBL_FARBM_PROB.MISCH_ID = TBL_GRUND_FARBM.MISCH_ID)" _
        & "WHERE (((TBL_FARBM_PROB.MISCH_ID)=" & MnMenueparam.MischID & ") AND ((TBL_GRUND_FARBM.GKWRT_ID)=" & MnMenueparam.Misch.GKwrtID & ")) ORDER BY TBL_FARBM_PROB.FARBM_ID,FARBM_IRBA;"

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
                RowFarb("FARBM_PREIS") = Farbe.Pre(i)
                .Rows.Add(RowFarb)
            End With
        Next i
        For i = 0 To Farbe.PrfCount - 1
            With DsetFarb.Tables("TBLPRO")
                RowFarb = .NewRow
                RowFarb("FARBM_ID") = Farbe.ID
                RowFarb("FARBM_PROZ") = Farbe.Prf(i)
                .Rows.Add(RowFarb)
            End With
        Next i
        For i = 0 To Farbe.PrbCount - 1
            With DsetFarb.Tables("TBLPRB")
                RowFarb = .NewRow
                RowFarb("FARBM_ID") = Farbe.ID
                RowFarb("FARBM_PROB") = Farbe.Prb(i)
                .Rows.Add(RowFarb)
            End With
        Next i
    End Sub
End Class

