<ComClass(AusgabeMankiewicz.ClassId, AusgabeMankiewicz.InterfaceId, AusgabeMankiewicz.EventsId)> _
Public Class AusgabeMankiewicz

#Region "COM-GUIDs"
  ' Diese GUIDs stellen die COM-Identität für diese Klasse 
  ' und ihre COM-Schnittstellen bereit. Wenn Sie sie ändern, können vorhandene 
  ' Clients nicht mehr auf die Klasse zugreifen.
  Public Const ClassId As String = "affcf40a-4e27-4d8f-970d-e41972c0d472"
  Public Const InterfaceId As String = "5c2cb240-aec2-46fc-8a37-6e2bc399606a"
  Public Const EventsId As String = "d23d436b-86bc-4cf1-82cd-44335652e33f"
#End Region

  ' Eine erstellbare COM-Klasse muss eine Public Sub New() 
  ' ohne Parameter aufweisen. Andernfalls wird die Klasse 
  ' nicht in der COM-Registrierung registriert und kann nicht 
  ' über CreateObject erstellt werden.
  Public Sub New()
    MyBase.New()
  End Sub
  Dim DruckSelected As Integer
  Dim Verweis As String
  Dim Firma As String
  Dim Strasse As String
  Dim Ort As String
  Dim Tel As String
  Dim MessGeraet As String
  Dim MessGeometrie As String
  Dim MessProg As String
  Dim Bemerkung As String
  Dim ReportEtiketten As c1report
  Dim ReportFarbMaster As C1Report
  Dim ReportFarbDetails As C1Report
  Dim SectionTable As DataTable
  Dim ViewSubTable As DataView
  Dim ViewSecTable As DataView
  Dim SubReportTable As DataTable
  Dim ppv As New PrintPreviewDialog
  Dim Pd As New PrintDialog
  Dim WithEvents Printdoc As PrintDocument
  Dim EtikUeber() As String
  Dim EtikVorlage As String
  Dim EtikNachstellung() As String
  Dim EtikValue As List(Of String())
  Dim Currentpage As Integer
  Dim FontEti As Font
  Dim FontSymb As Font
  Dim Brush As SolidBrush
  Dim Fontu As Font
  Dim Fontv As Font
  Dim Fontw As Font
  Dim Fontx As Font
  Dim Fonty As Font
  Dim Fontz As Font
  Dim Brusheti As SolidBrush
  Dim Barcode As C1BarCode
  Dim DHeight As Integer = 0
  Dim DWidth As Integer = 0

  Private Property EtikNormWin As String

  '
  '
  '
  '
  '
  'Ausdruck Farbwerte Mankiewicz
  'Unterforsthuber 10.10.2013)
  '
  '
  '
  '
  '
  Sub SubDruck(ByRef FaObs As Integer, ByRef UserID As Integer, ByRef MethID As Integer, ByRef Messg As MeasParameters, ByRef FarbTable As DataTable)
    Dim ColCount As Integer
    Dim Isec As Integer
    Dim Fawrt As Integer
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim FrmMankiewicz As New frmMankiewiczAusgabe
    Dim VorValue(3) As String
    Dim IsecBool As Boolean
    Dim AllgRow As DataRow
    'Die Datentabelle entspricht der in MankiewiczFarb(Farbwerte)im Gitternetz dargestellten Tabelle'
    'alle Größen haben den Datentyp = String; Die erste Zeile ist Header-Zeile und entspricht den .Columns(i).Caption-Werten der Datentabelle
    '
    '
    'Hier muss von Mankiewicz die Druckerausgabe programmiert werden
    '
    '
    '
    MessGeraet = Messg.Kbez
    MessGeometrie = Messg.Lbez
    MessProg = Messg.Prog
    '
    'Report anzeigen
    '
    '
    '
    FrmMankiewicz.ShowDialog()
    DruckSelected = FrmMankiewicz.DruckSelected
    '
    '
    '
    '
    ' 
    'Tabellen für Master und Detail - Eintragungen aufbauen
    '
    '
    SectionTable = FarbTable.Clone
    SubReportTable = FarbTable.Clone
    ColCount = FarbTable.Columns.Count
    SectionTable.Columns.Add("ISEC", GetType(Integer))
    SubReportTable.Columns.Add("ISEC", GetType(Integer))
    IsecBool = False
    Isec = -1
    For Each AllgRow In FarbTable.Rows
      If AllgRow(0).trim = "" Then
        '
        '
        'Leerzeile gefunden
        '
        '
        IsecBool = True
      Else
        If IsecBool Then
          '
          '
          'Überschrift gefunden
          '
          '
          Isec = Isec + 1
          SectionTable.Rows.Add(SectionTable.NewRow)
          For j = 0 To FarbTable.Columns.Count - 1
            If FarbTable.Columns(j).DataType = GetType(String) And (IsNothing(AllgRow(j)) OrElse IsDBNull(AllgRow(j))) Then AllgRow(j) = ""
            SectionTable.Rows(SectionTable.Rows.Count - 1)(j) = AllgRow(j)
          Next j
          SectionTable.Rows(SectionTable.Rows.Count - 1)("ISEC") = Isec
          IsecBool = False
        Else
          SubReportTable.Rows.Add(SubReportTable.NewRow)
          For j = 0 To FarbTable.Columns.Count - 1
            If IsNothing(AllgRow(j)) OrElse IsDBNull(AllgRow(j)) Then AllgRow(j) = ""
            SubReportTable.Rows(SubReportTable.Rows.Count - 1)(j) = AllgRow(j)
          Next j
          SubReportTable.Rows(SubReportTable.Rows.Count - 1)("ISEC") = Isec
        End If
      End If
    Next
    If SectionTable.Columns.Contains("CME") Then
      SectionTable.Columns.Remove("CME")
    End If
    If SectionTable.Columns.Contains("GID") Then
      SectionTable.Columns.Remove("GID")
    End If
    If SubReportTable.Columns.Contains("CME") Then
      SubReportTable.Columns.Remove("CME")
    End If
    If SubReportTable.Columns.Contains("GID") Then
      SubReportTable.Columns.Remove("GID")
    End If
    '
    Bemerkung = SubReportTable.Rows(0)("BEM")
    '
    '
    'Bemerkung löschen
    '
    '

    If SectionTable.Columns.Contains("BEM") Then
      SectionTable.Columns.Remove("BEM")
    End If
    If SubReportTable.Columns.Contains("BEM") Then
      SubReportTable.Columns.Remove("BEM")
    End If
    ViewSubTable = New DataView(SubReportTable)
    ViewSecTable = New DataView(SectionTable)

    '
    '
    Printdoc = New PrintDocument

    '
    '
    '
    '
    Fawrt = 0
    Verweis = ""
    Select Case DruckSelected
      Case 1  'Freigabeprotokoll Standard

        Fawrt = 0
        Verweis = GetPrivSettings("MANKIEWICZ", "VERWEIS", Texxt(29010), COLORFileName())

      Case 2  'NLD65,MI:F11+LED
        '
        Call ColumnsNotDelete({"NAME", "ISEC", "BK", "BN", "BO", "BJ", "BI", "CME", "GID"})
        For i = 1 To SectionTable.Rows.Count - 1
          Call ColumnsBlank(i, {"BK", "BN", "BO", "BJ"})
          Call ColumnsBlank(i, {"BK", "BN", "BO", "BJ"})
        Next i
      Case 3  'DE(NLD65)
        Fawrt = 0
        Call ColumnsNotDelete({"NAME", "ISEC", "BJ", "CME", "GID"})
        ViewSecTable.RowFilter = "ISEC=0"

      Case 4  'F11
        Call ColumnsNotDelete({"NAME", "ISEC", "BK", "BN", "BO", "BJ", "CME", "GID"})
        ViewSecTable.RowFilter = "ISEC=1"


      Case 5  'Metallic
        Call ColumnsNotDelete({"NAME", "ISEC", "BK", "BN", "BO", "CME", "GID"})
        ViewSecTable.RowFilter = "ISEC=0"


      Case 6  'Etiketten
        Fawrt = 2
      Case 7 'Ende
        Exit Sub
    End Select

    SectionTable.AcceptChanges()
    SubReportTable.AcceptChanges()
    '

    If Fawrt = 0 Then
      '
      '
      Firma = GetPrivSettings("MANKIEWICZ", "FIRMA", "Mankiewicz Gebr. & Co. (Gmbh & Co. KG)", COLORFileName())
      Strasse = GetPrivSettings("MANKIEWICZ", "STRASSE", "Georg-Wilhelm-Strasse 189", COLORFileName())
      Ort = GetPrivSettings("MANKIEWICZ", "ORT", "D-21107 Hamburg", COLORFileName())
      Tel = GetPrivSettings("MANKIEWICZ", "TEL", " ", COLORFileName())

      '
      'Reports generieren

      '
      Call RenderFarb()
      '
      'Priview anzeigen
      '
      '
      '
      ppv.Document = ReportFarbMaster.Document
      Pd.Document = ReportFarbMaster.Document

      If Pd.ShowDialog = DialogResult.OK Then
        ppv.WindowState = FormWindowState.Maximized
        '
        'Priview anzeigen
        '
        '
        ppv.ShowDialog()
      End If
    ElseIf Fawrt = 1 Then
      '
      '************************
      'wird nicht verwendet
      '************************
      '
      'Fontx = New Font("MICROSOFT SANS SERIF", 10, FontStyle.Bold)
      'Brushx = New SolidBrush(Color.Black)
      'Fonty = New Font("MICROSOFT SANS SERIF", 10, FontStyle.Regular)
      'Brushy = New SolidBrush(Color.Black)
      'Fontz = New Font("MICROSOFT SANS SERIF", 10, FontStyle.Regular)
      'Brushz = New SolidBrush(Color.Black)
      '
      '
      '
      '
      Brush = New SolidBrush(Color.Black)
      Fontu = New Font("TIMES NEW ROMAN", 12, FontStyle.Bold)
      Fontv = New Font("TIMES NEW ROMAN", 10, FontStyle.Regular)
      Fontw = New Font("TIMES NEW ROMAN", 10, FontStyle.Bold)
      Fontx = New Font("TIMES NEW ROMAN", 10, FontStyle.Bold)
      Fonty = New Font("TIMES NEW ROMAN", 10, FontStyle.Regular)
      Fontz = New Font("TIMES NEW ROMAN", 16, FontStyle.Underline)
      '
      '
      ViewSubTable = New DataView(SubReportTable)
      ViewSecTable = New DataView(SectionTable)




      Pd.Document = Printdoc
      ppv.Document = Printdoc
      Printdoc.DefaultPageSettings.Landscape = False

      'If Pd.ShowDialog = DialogResult.OK Then

      '
      If Pd.ShowDialog = DialogResult.OK Then
        ppv.WindowState = FormWindowState.Maximized
        '
        'Priview anzeigen
        '
        '
        ppv.ShowDialog()
      End If
    ElseIf Fawrt = 2 Then
      '
      '
      'Etiketten
      '
      '
      '
      EtikValue = New List(Of String())
      Erase EtikNachstellung

      If Not (SubReportTable.Columns.Contains("BK") And SubReportTable.Columns.Contains("BN") And SubReportTable.Columns.Contains("BO") And SubReportTable.Columns.Contains("BJ")) Then
        Exit Sub
      End If
      '
      '
      '
      FontEti = New Font("Arial", 8, FontStyle.Regular)
      FontSymb = New Font("Symbol", 8, FontStyle.Regular)
      Brusheti = New SolidBrush(Color.Black)
      Barcode = New C1BarCode
      Barcode.CodeType = CodeTypeEnum.Code128
      'BK   DL*
      'BN   Da*
      'BO   Db*
      'BL   DC*
      'BM   DH*
      'BJ   DE*

      '
      '
      'EtikUeber = (FarbTable.Rows(1)(0) & Space(50)).substring(0, 32) & SubReportTable.Columns("BK").Caption.Trim & Space(3) & SubReportTable.Columns("BN").Caption.Trim & Space(3) & SubReportTable.Columns("BO").Caption.Trim & Space(3) & SubReportTable.Columns("BJ").Caption.Trim
      EtikValue.Clear()
      ReDim EtikUeber(4)
      EtikUeber(0) = (FarbTable.Rows(1)(0) & Space(50)).substring(0, 32)
      EtikUeber(1) = SubReportTable.Columns("BK").Caption.Trim
      EtikUeber(2) = SubReportTable.Columns("BN").Caption.Trim
      EtikUeber(3) = SubReportTable.Columns("BO").Caption.Trim
      EtikUeber(4) = SubReportTable.Columns("BJ").Caption.Trim

      EtikVorlage = SubReportTable.Rows(0)(0)
      j = -1
      For i = 1 To SubReportTable.Rows.Count - 1
        If IsNumeric(SubReportTable.Rows(i)("BK")) And IsNumeric(SubReportTable.Rows(i)("BN")) And IsNumeric(SubReportTable.Rows(i)("BO")) And IsNumeric(SubReportTable.Rows(i)("BJ")) Then
          j = j + 1
          ReDim Preserve EtikNachstellung(j)
          EtikNachstellung(j) = (SubReportTable.Rows(i)(0) & Space(50)).substring(0, 32)
          VorValue(0) = SubReportTable.Rows(i)("BK")
          VorValue(1) = SubReportTable.Rows(i)("BN")
          VorValue(2) = SubReportTable.Rows(i)("BO")
          VorValue(3) = SubReportTable.Rows(i)("BJ")
          EtikValue.Add(VorValue.Clone)
        Else
          Exit For
        End If
      Next i
      If EtikValue Is Nothing OrElse EtikValue.Count = 0 Then
        Exit Sub
      End If
      Currentpage = 0
      '
      '
      Pd.Document = Printdoc
      ppv.Document = Printdoc
      Printdoc.DefaultPageSettings.Landscape = False

      If Pd.ShowDialog = DialogResult.OK Then
        '
        'Priview anzeigen
        '
        '
        '
        ppv.WindowState = FormWindowState.Maximized

        ppv.ShowDialog()
      End If
    End If
  End Sub
  Sub RenderFarb()
    Dim f As Field
    Dim i As Integer
    ReportFarbMaster = New C1Report
    ReportFarbDetails = New C1Report
    Dim grp As Group
    Dim Margin As Double = 770
    Dim FHeight As Double = 330
    Dim MerkWidth As Double = 770
    Dim NamWidth As Double = 2700
    Dim MerkHeight As Double = 215
    Dim WholeWidth As Double
    Dim Fwidth As Double
    Dim Fleft As Double
    '
    '
    '
    '
    '
    '
    '
    'DetailReport
    '
    '
    'prevent reentrant calls
    '
    If ReportFarbDetails.IsBusy Then Beep() : Exit Sub
    '
    '
    '
    '
    With ReportFarbDetails
      .Clear()
      '.Font.Name = "Microsoft Sans Serif"
      '.Font.Size = 8.25
      .ReportName = "FarbDetails"
      .DataSource.Recordset = ViewSubTable
    End With
    '
    '
    With ReportFarbDetails.Layout

      '.Orientation = OrientationEnum.Auto
      '.Width = 7.0 * 1440    '8.5 - margins
    End With
    '
    '
    'create detail section
    '
    '
    '
    With ReportFarbDetails.Sections(SectionTypeEnum.Detail)
      .Height = MerkHeight
      .Visible = True
      Fwidth = NamWidth
      Fleft = 0
      For i = 0 To SubReportTable.Columns.Count - 2
        f = .Fields.Add(SubReportTable.Columns(i).ColumnName, SubReportTable.Columns(i).ColumnName, Fleft, 0.5 * MerkHeight, 0.95 * Fwidth, MerkHeight)
        f.Calculated = True
        f.Visible = True
        f.Font.Size = 8.25
        Fleft = Fleft + Fwidth
        Fwidth = MerkWidth
        If i = 0 Then
          f.Align = FieldAlignEnum.LeftMiddle
        Else
          f.Align = FieldAlignEnum.RightMiddle
        End If
        '
        '
        '
        'Linie (vertikal)
        '
        f = .Fields.Add("LINIEV0", "", Fleft, 0, 0, 1.5 * MerkHeight)
        f.LineSlant = LineSlantEnum.Down
        f.LineWidth = 5
        f.BorderStyle = BorderStyleEnum.Solid
        f.BorderColor = Drawing.Color.Black
      Next
    End With
    '
    '
    'MasterReport
    '
    '
    'prevent reentrant calls
    '
    If ReportFarbMaster.IsBusy Then Beep() : Exit Sub
    '
    'Initialize layout (Master)
    '
    '


    '
    '
    'initialize control
    '
    '
    With ReportFarbMaster
      .Clear()
      .Font.Name = "Microsoft Sans Serif"
      .Font.Size = 8.25
      .ReportName = "FarbMaster"
      .DataSource.Recordset = ViewSecTable
    End With
    '
    '
    '
    '
    '
    With ReportFarbMaster.Layout
      '
      '
      '1 Inch=1440 twips
      '
      '
      .Orientation = OrientationEnum.Portrait
      '.Width = 7 * 1440 * ReportFarbMaster.Document.PrinterSettings.DefaultPageSettings.Bounds.Width / ReportFarbMaster.Document.PrinterSettings.DefaultPageSettings.Bounds.Height
      '.Width = 7 * 1440
      '.Width = ReportFarbMaster.Document.DefaultPageSettings.Bounds.Width
      .MarginLeft = Margin
      .MarginTop = Margin
      .MarginRight = Margin
      .MarginBottom = Margin
      .Width = ReportFarbMaster.C1Document.PageLayout.PageSettings.Width.ConvertUnit(C1.C1Preview.UnitTypeEnum.Twip) - .MarginLeft - .MarginRight
      WholeWidth = .Width
    End With

    '
    '
    '

    'Create a reportheader
    '
    '
    '
    With ReportFarbMaster.Sections(SectionTypeEnum.Header)
      .Height = 288
      .Visible = True
      'f = .Fields.Add("fldTitle", "MANKIEWICZ COLOR SYSTEM (" & MessGeraet & "[" & MessKenn & "])", 0.15 * WholeWidth, MerkHeight, ReportFarbMaster.Layout.Width, 1000)
      'f.Visible = True
      'f.Font.Size = 14
      'f.Font.Bold = True
      'f.BackColor = Drawing.Color.White
      'f.Align = FieldAlignEnum.LeftMiddle

      'f = .Fields.Add("fldMess", MessGeraet, 1000, 1500, 8000, 300)
      'f.Visible = True
      'f.Font.Size = 10
      'f.Font.Bold = True
      'f.BackColor = Drawing.Color.LightGray
      'f.Align = FieldAlignEnum.CenterMiddle



      f = .Fields.Add("picture", "Picture", WholeWidth - 0.4 * WholeWidth, MerkHeight, 0.4 * WholeWidth, 1440)
      f.Picture = My.Resources.MG_Logo
      f.Align = FieldAlignEnum.RightMiddle
      '
      '
      f = .Fields.Add("Man000", Verweis, 0.5 * Margin, 1440 - 3.6 * MerkHeight, 0.5 * WholeWidth, 1.8 * MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 16
      f.Font.Bold = True
      f.BackColor = Drawing.Color.White
      '
      '
      f = .Fields.Add("Man001", Firma, 0.5 * Margin, 1440, WholeWidth - 0.5 * Margin, 1.3 * MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 10
      f.Font.Bold = True
      f.BackColor = Drawing.Color.White
      '
      '
      f = .Fields.Add("Man002", Strasse, 0.5 * Margin, 1440 + 1.3 * MerkHeight, WholeWidth - 0.5 * Margin, MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 8.25
      f.Font.Bold = False
      f.BackColor = Drawing.Color.White
      '
      '

      '
      '
      f = .Fields.Add("Man003", Ort, 0.5 * Margin, 1440 + 2.3 * MerkHeight, WholeWidth - 0.5 * Margin, MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 8.25
      f.Font.Bold = False
      f.BackColor = Drawing.Color.White
      '
      '
      f = .Fields.Add("Man004", Tel, 0.5 * Margin, 1440 + 3.3 * MerkHeight, WholeWidth - 0.5 * Margin, MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 8.25
      f.Font.Bold = False
      f.BackColor = Drawing.Color.White

      '
      '
      '
      f = .Fields.Add("Man005", Texxt(29011), 0, 1440 + 4.3 * MerkHeight, WholeWidth, 2 * MerkHeight)
      f.Align = FieldAlignEnum.CenterMiddle
      f.Visible = True
      f.Font.Size = 16
      f.Font.Bold = False
      f.Font.Underline = True
      f.BackColor = Drawing.Color.White
      '
      '
      '
      '
      '
      '
      f = .Fields.Add("Man006", Texxt(29012), 0.5 * Margin, 1440 + 6.3 * MerkHeight, WholeWidth, 1.5 * MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 12
      f.Font.Bold = True
      f.Font.Underline = False
      f.BackColor = Drawing.Color.White
      '
      '
      '
      '
      '
      f = .Fields.Add("Man007", Texxt(29013), 0.5 * Margin, 1440 + 8 * MerkHeight, 0.5 * WholeWidth, MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 8.25
      f.Font.Bold = False
      f.Font.Underline = False
      f.BackColor = Drawing.Color.White
      '
      '
      '
      '
      f = .Fields.Add("Man008", MessGeraet & " (" & MessProg & ")", 0.5 * Margin + 2000, 1440 + 8 * MerkHeight, 0.5 * WholeWidth, MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 8.25
      f.Font.Bold = False
      f.Font.Underline = False
      f.BackColor = Drawing.Color.White
      '
      '
      '
      '
      f = .Fields.Add("Man009", Texxt(29014), 0.5 * Margin, 1440 + 9 * MerkHeight, 0.5 * WholeWidth, MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 8.25
      f.Font.Bold = False
      f.Font.Underline = False
      f.BackColor = Drawing.Color.White
      '
      '
      '
      '
      f = .Fields.Add("Man010", MessGeometrie, 0.5 * Margin + 2000, 1440 + 9 * MerkHeight, 0.5 * WholeWidth, MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 8.25
      f.Font.Bold = False
      f.Font.Underline = False
      f.BackColor = Drawing.Color.White
      '
      '
      '
      f = .Fields.Add("Man011", Texxt(29015), 0.5 * Margin, 1440 + 11 * MerkHeight, WholeWidth, 1.5 * MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 12
      f.Font.Bold = True
      f.Font.Underline = False
      f.BackColor = Drawing.Color.White
      '
      '
      '
      '
      '
      f = .Fields.Add("Man012", Texxt(29016), 0.5 * Margin, 1440 + 12.5 * MerkHeight, 0.5 * WholeWidth, MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 8.25
      f.Font.Bold = False
      f.Font.Underline = False
      f.BackColor = Drawing.Color.White
      '
      '
      '
      '
      '
      f = .Fields.Add("Man013", Texxt(29017), 0.5 * Margin + 2000, 1440 + 12.5 * MerkHeight, 0.5 * WholeWidth, MerkHeight)
      f.Align = FieldAlignEnum.LeftMiddle
      f.Visible = True
      f.Font.Size = 8.25
      f.Font.Bold = False
      f.Font.Underline = False
      f.BackColor = Drawing.Color.White
      '
      '
      '
      '
      '
      '
      f = .Fields.Add("Bem", Bemerkung, 0.5 * Margin, 1440 + 14.5 * MerkHeight, 0.5 * WholeWidth, 2 * MerkHeight)
      f.Align = FieldAlignEnum.LeftTop
      f.Visible = True
      f.Font.Size = 8.25
      f.Font.Bold = False
      f.Font.Underline = False
      f.BackColor = Drawing.Color.White



      f = .Fields.Add("DateTime", Now, 0, 1440 + 14.5 * MerkHeight, WholeWidth, 2 * MerkHeight)
      f.Align = FieldAlignEnum.RightTop
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 8.25
      f.Font.Bold = False

    End With
    '
    '
    '
    'Create Page footer
    '
    '
    '
    With ReportFarbMaster.Sections(SectionTypeEnum.PageFooter)
      .Height = 500
      .Visible = True
      'f = .Fields.Add("FldFtrLeft", Now, 0, 0, 4000, 300)
      'f.Calculated = True
      '
      '
      'Seitennummerierung
      '
      '
      f = .Fields.Add("SeitenText", "Seite", ReportFarbMaster.Layout.Width - 1200, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITE", "Page", ReportFarbMaster.Layout.Width - 1000, 0, 500, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEVON", "von", ReportFarbMaster.Layout.Width - 650, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEN", "Pages", ReportFarbMaster.Layout.Width - 500, 0, 500, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      '
      '
      'Linie
      '
      'f = .Fields.Add("FldLine", "", 0, 0, ReportFarbMaster.Layout.Width, 20)
      'f.LineSlant = LineSlantEnum.NoSlant
      'f.BorderStyle = BorderStyleEnum.Solid
      'f.BorderColor = Drawing.Color.FromArgb(0, 0, 100)
    End With
    '
    '
    '
    'Create a page header
    '
    '
    '
    With ReportFarbMaster.Sections(SectionTypeEnum.PageHeader)
      .Height = MerkHeight
      .Visible = True
      .BackColor = Drawing.Color.White
      Fwidth = NamWidth
      Fleft = 0
      For i = 0 To SectionTable.Columns.Count - 2
        f = .Fields.Add(SectionTable.Columns(i).Caption.Trim, SectionTable.Columns(i).Caption.Trim, Fleft, 100, Fwidth, FHeight)
        Fleft = Fleft + Fwidth
        Fwidth = MerkWidth
        f.Font.Bold = True
        f.Font.Size = 8.25
        If i = 0 Then
          f.Align = FieldAlignEnum.LeftMiddle
        Else
          f.Align = FieldAlignEnum.CenterMiddle
        End If
        '
        '
        '
        '
        'Linie (vertikal)
        '
        f = .Fields.Add("LINIEV1", "", Fleft, 0, 0, 2 * MerkHeight)
        f.LineSlant = LineSlantEnum.Down
        f.LineWidth = 5
        f.BorderStyle = BorderStyleEnum.Solid
        f.BorderColor = Drawing.Color.Black

      Next
      '
    End With
    '

    '
    'create groups
    '
    '
    '
    '
    grp = ReportFarbMaster.Groups.Add("ISEC", "ISEC", SortEnum.Ascending)
    With grp.SectionHeader


      .Height = MerkHeight
      .Visible = True
      .BackColor = Drawing.Color.White

      '
      'Linie (horizontal)
      '
      f = .Fields.Add("LINIEH1", "", 0, 0, WholeWidth, 10)
      f.LineSlant = LineSlantEnum.NoSlant
      f.LineWidth = 5
      f.BorderStyle = BorderStyleEnum.Solid
      f.BorderColor = Drawing.Color.Black

      '
      'Lichtart+Geometrie
      '
      '
      f = .Fields.Add(SectionTable.Columns(0).ColumnName, SectionTable.Columns(0).ColumnName, 0, 0, WholeWidth + 100, 500)
      f.Calculated = True
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      '
      '
      '
      Fwidth = NamWidth
      Fleft = 0
      For i = 0 To SectionTable.Columns.Count - 2
        Fleft = Fleft + Fwidth
        Fwidth = MerkWidth

        '
        'Linie (vertikal)
        '
        f = .Fields.Add("LINIEV2", "", Fleft, 0, 0, 500)
        f.LineSlant = LineSlantEnum.Down
        f.LineWidth = 5
        f.BorderStyle = BorderStyleEnum.Solid
        f.BorderColor = Drawing.Color.Black

      Next
      '

      grp.KeepTogether = KeepTogetherEnum.KeepWholeGroup


    End With
    '
    '
    '
    'Subreport (Details)
    'wird über ISEC mit Masterreport verknüpft
    '
    With ReportFarbMaster.Sections(SectionTypeEnum.Detail)
      f = .Fields.Add("FarbWerte", """ISEC="" & ISEC", 0, 0, 0, 0)
      f.Subreport = ReportFarbDetails
      f.Calculated = True
      f.CanGrow = True
      .Height = 1
      f.CanShrink = True
      .Visible = True
    End With
    ''
  End Sub
  Sub ColumnsNotDelete(ColMerk As String())
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    '
    '
    'SubReportTable
    '
    '
    '
    For j = SubReportTable.Columns.Count - 1 To 0 Step -1
      For i = 0 To ColMerk.Count - 1
        If SubReportTable.Columns(j).ColumnName = ColMerk(i) Then
          Exit For
        End If
      Next i
      If i = ColMerk.Count Then
        SubReportTable.Columns.Remove(SubReportTable.Columns(j).ColumnName)
      Else
        For k = 0 To SubReportTable.Rows.Count - 1
          If ColMerk(i).Length = 2 And IsNumeric(SubReportTable.Rows(k)(ColMerk(i))) Then
            SubReportTable.Rows(k)(ColMerk(i)) = Format(CSng(SubReportTable.Rows(k)(ColMerk(i))), "##0.0")
          End If
        Next k
      End If
    Next j
    '
    '
    'Sectiontable
    '
    For j = SectionTable.Columns.Count - 1 To 0 Step -1
      For i = 0 To ColMerk.Count - 1
        If SectionTable.Columns(j).ColumnName = ColMerk(i) Then
          Exit For
        End If
      Next i
      If i = ColMerk.Count Then
        SectionTable.Columns.Remove(SectionTable.Columns(j).ColumnName)
      End If
    Next j

  End Sub
  Sub ColumnsBlank(Isec As Integer, ColMerk As String())
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    For i = 0 To SubReportTable.Rows.Count - 1
      If SubReportTable.Rows(i)("ISEC") = Isec Then
        For j = 0 To SubReportTable.Columns.Count - 1
          For k = 0 To ColMerk.Count - 1
            If SubReportTable.Columns(j).ColumnName = ColMerk(k) Then
              SubReportTable.Rows(i)(SubReportTable.Columns(j).ColumnName) = ""
            End If
          Next
        Next j
      End If
    Next i
  End Sub
  '
  '
  '
  '*******************
  '*******************
  Private Sub Printdoc_PrintPage(sender As Object, e As System.Drawing.Printing.PrintPageEventArgs) Handles Printdoc.PrintPage
    Select Case DruckSelected
      Case 2  'NLD65 usw
        '
        '
        Call NLD65Plus(sender, e)
      Case 3  'DE(NLD65)
        Call DENL65(sender, e)

      Case 4  'F11
        Call F11(sender, e)

      Case 5  'Metallic
        Call Metallic(sender, e)

      Case 6  'Etiketten
        Call EtikettenDruck(sender, e)
    End Select
  End Sub
  Sub NLD65Plus(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Call MankiewiczKopf(sender, ev)
    Call MankiewiczUeber({350, 425, 500, 575, 650}, {"DL*", "Da*", "Db*", "DE*", "META"}, sender, ev)
    Call MankiewiczBlock(0, 500, {350, 425, 500, 575, 650}, {"BK", "BN", "BO", "BJ", "BI"}, sender, ev)
    Call MankiewiczBlock(1, 600, {350, 425, 500, 575, 650}, {"", "", "", "", "BI"}, sender, ev)
    Call MankiewiczBlock(2, 700, {350, 425, 500, 575, 650}, {"", "", "", "", "BI"}, sender, ev)

  End Sub
  Sub DENL65(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Call MankiewiczKopf(sender, ev)
    Call MankiewiczUeber({350}, {"DE*"}, sender, ev)
    Call MankiewiczBlock(0, 500, {350}, {"BJ"}, sender, ev)

  End Sub
  Sub F11(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Call MankiewiczKopf(sender, ev)
    Call MankiewiczUeber({350, 425, 500, 575}, {"DL*", "Da*", "Db*", "DE*"}, sender, ev)
    Call MankiewiczBlock(1, 500, {350, 425, 500, 575}, {"BK", "BN", "BO", "BJ"}, sender, ev)

  End Sub
  Sub Metallic(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Call MankiewiczKopf(sender, ev)
    Call MankiewiczUeber({350, 425, 500}, {"DL*", "Da*", "Db*"}, sender, ev)
    Call MankiewiczBlock(0, 500, {350, 425, 500}, {"BK", "BN", "BO"}, sender, ev)

  End Sub
  Sub MankiewiczKopf(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Dim TextSize As SizeF
    ev.Graphics.DrawImage(My.Resources.MG_Logo, New Point(ev.MarginBounds.X + ev.MarginBounds.Width - 250, ev.MarginBounds.Y))
    ev.Graphics.DrawString("Mankiewicz Gebr. & Co. (GmbH & Co. KG)", Fontx, Brush, ev.MarginBounds.X - 25, ev.MarginBounds.Y + 100)
    ev.Graphics.DrawString("Georg-Wilhelm-Strasse 189", Fonty, Brush, ev.MarginBounds.X - 25, ev.MarginBounds.Y + 130)
    ev.Graphics.DrawString("D-21107 Hamburg", Fonty, Brush, ev.MarginBounds.X - 25, ev.MarginBounds.Y + 145)
    TextSize = ev.Graphics.MeasureString("Farbmessprotokoll", Fontz)
    ev.Graphics.DrawString("Farbmessprotokoll", Fontz, Brush, ev.MarginBounds.X - 25 + 0.5 * (ev.MarginBounds.Width - TextSize.Width), ev.MarginBounds.Y + 170)
    ev.Graphics.DrawString("Messbedingungen:", Fontu, Brush, ev.MarginBounds.X - 25, ev.MarginBounds.Y + 250)
    ev.Graphics.DrawString("Messgerät:", Fonty, Brush, ev.MarginBounds.X - 25, ev.MarginBounds.Y + 280)
    ev.Graphics.DrawString(MessGeraet, Fonty, Brush, ev.MarginBounds.X + 100, ev.MarginBounds.Y + 280)
    ev.Graphics.DrawString("Messgeometrie:", Fonty, Brush, ev.MarginBounds.X - 25, ev.MarginBounds.Y + 295)
    ev.Graphics.DrawString(MessGeometrie, Fonty, Brush, ev.MarginBounds.X + 100, ev.MarginBounds.Y + 295)
    '
    '
    ev.Graphics.DrawString("Farbdifferenzen:", Fontu, Brush, ev.MarginBounds.X - 25, ev.MarginBounds.Y + 330)
    ev.Graphics.DrawString("Farbdifferenzformel:", Fonty, Brush, ev.MarginBounds.X - 25, ev.MarginBounds.Y + 360)
    ev.Graphics.DrawString("CIE L*a*b*", Fonty, Brush, ev.MarginBounds.X + 100, ev.MarginBounds.Y + 360)
    '
    '
    '
    ev.Graphics.DrawString(Bemerkung, Fontx, Brush, ev.MarginBounds.X, ev.MarginBounds.Y + 400)
    ev.Graphics.DrawString(Format("{dd.MM.yyyy  HH:mm}", Now()).Substring(0, 16), Fontx, Brush, ev.MarginBounds.X + ev.MarginBounds.Width - 50, ev.MarginBounds.Y + 400)

  End Sub
  '
  '
  Sub MankiewiczUeber(ByVal MerkLeft() As Single, ByVal Merkstring() As String, ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Dim i As Integer
    ev.Graphics.DrawString("Name", Fontu, Brush, ev.MarginBounds.X + 25, ev.MarginBounds.Y + 450)
    For i = 0 To Merkstring.Count - 1
      ev.Graphics.DrawString(Merkstring(i), Fontu, Brush, MerkLeft(i), ev.MarginBounds.Y + 450)
    Next i
  End Sub
  Sub MankiewiczBlock(ByVal ISEC As Integer, ByVal HeightShift As Single, ByVal MerkLeft() As Single, ByVal MerkKen() As String, ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Dim i As Integer
    Dim j As Integer
    Dim FieldWidth As Single = 35
    Dim MerkText As String
    Dim Textsize As SizeF
    ViewSecTable.RowFilter = "ISEC=" & ISEC
    If ViewSecTable.Count = 0 Then
      MsgBox("keine Auswertung vorhanden")
      Exit Sub
    End If
    ViewSubTable.RowFilter = "ISEC=" & ISEC
    If ViewSubTable.Count < 2 Then
      MsgBox("zu wenig Messungen vorhanden")
      Exit Sub
    End If

    ev.Graphics.DrawString(ViewSecTable(0)("NAME"), Fontu, Brush, ev.MarginBounds.X, ev.MarginBounds.Y + HeightShift)
    For j = 0 To 1
      ev.Graphics.DrawString(ViewSubTable(j)("NAME"), Fonty, Brush, ev.MarginBounds.X, ev.MarginBounds.Y + HeightShift + (j + 1) * 25)
      For i = 0 To MerkLeft.Count - 1
        If MerkKen(i).Trim <> "" Then
          If ViewSubTable(j)(MerkKen(i)).trim <> "" Then
            MerkText = Format(CSng(ViewSubTable(j)(MerkKen(i))), "##0.0")
            Textsize = ev.Graphics.MeasureString(MerkText, Fonty)
            ev.Graphics.DrawString(MerkText, Fonty, Brush, MerkLeft(i) + FieldWidth - Textsize.Width, ev.MarginBounds.Y + HeightShift + (j + 1) * 25)
          End If
        End If
      Next i
    Next j
  End Sub




  Sub EtikettenDruck(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Dim k As Integer
    Dim j As Integer
    Dim TextWidth As Single
    Dim TextHeight As Single
    If EtikValue Is Nothing OrElse EtikValue.Count = 0 Then
      ev.HasMorePages = False
      Currentpage = 0
      Exit Sub
    End If
    'ev.Graphics.DrawString("D", FontSymb, Brusheti, DWidth + 50, DHeight + 50)
    'ev.Graphics.DrawString("D", FontSymb, Brusheti, DWidth + 93, DHeight + 50)
    'ev.Graphics.DrawString("D", FontSymb, Brusheti, DWidth + 136, DHeight + 50)
    'ev.Graphics.DrawString("DL*", FontEti, Brusheti, DWidth + 60, DHeight + 50)
    'ev.Graphics.DrawString("Da*", FontEti, Brusheti, DWidth + 103, DHeight + 50)
    'ev.Graphics.DrawString("Db*", FontEti, Brusheti, DWidth + 146, DHeight + 50)
    '
    ev.Graphics.DrawString(Now, FontEti, Brusheti, DWidth + 40, DHeight + 50)
    EtikVorlage = CharString(EtikVorlage, DWidth + 40 + 100, FontEti, ev.Graphics)
    ev.Graphics.DrawString(EtikVorlage, FontEti, Brusheti, DWidth + 40, DHeight + 100)
    For k = 0 To EtikUeber.Count - 1
      If k = 0 Then
        ev.Graphics.DrawString(EtikUeber(k), FontEti, Brusheti, DWidth + 40, DHeight + 75)
        ev.Graphics.DrawString(EtikNachstellung(Currentpage), FontEti, Brusheti, DWidth + 40, DHeight + 125)
      Else
        TextWidth = ev.Graphics.MeasureString(EtikUeber(k), FontEti).Width
        ev.Graphics.DrawString(EtikUeber(k), FontEti, Brusheti, DWidth + 40 + 120 + (k + 1) * 35 - TextWidth, DHeight + 75)
        TextWidth = ev.Graphics.MeasureString(EtikValue(Currentpage)(k - 1), FontEti).Width
        ev.Graphics.DrawString(EtikValue(Currentpage)(k - 1), FontEti, Brusheti, DWidth + 40 + 120 + (k + 1) * 35 - TextWidth, DHeight + 125)
      End If
     
    Next k
    'Barcode.Text = ""
    'For k = 0 To 3
    ' Barcode.Text = Barcode.Text & EtikValue(Currentpage)(k)
    'Next
    'Barcode.Text = Barcode.Text.Replace(" ", Chr(13))
    'ev.Graphics.DrawImage(Barcode.Image, 40, 100)
    Currentpage = Currentpage + 1
    If (Currentpage < EtikValue.Count) Then
      ev.HasMorePages = True
    Else
      ev.HasMorePages = False
      Currentpage = 0
    End If

  End Sub
  Function CharString(Text As String, Width As Single, fonti As Font, Graph As Graphics) As String
    Dim i As Integer
    CharString = Text
    For i = 0 To Text.Length - 1
      If Graph.MeasureString(Text.Substring(0, i + 1), fonti).Width >= Width Then
        CharString = Text.Substring(0, i + 1)
        Exit For
      End If
    Next

  End Function
  '


  '
  '

  '
  '
  '
  '
  'Dosierprogramm Mankiewicz
  'Unterforsthuber 25.08.2011 
  '
  '
  '
  '
  ''
  Sub Dosieren(ByVal FarbID() As Integer, ByVal FarbName() As String, ByVal ProNummer() As String, ByVal ProAlt() As Single, ByVal ProNeu() As Single, ByVal ProZuw() As Single)
    Dim Auftrag As String
    Dim zuw As Boolean

    'Auftragsnummer
    '
    '
    '
    If ProZuw Is Nothing OrElse ProZuw(0) = -1 Then
      zuw = False
    Else
      zuw = True
    End If



    Auftrag = InputBox(Texxt(29000), Texxt(29000) & Space(1) & Texxt(29001), "")

    '
    '
    '
    '
    'Gesamtmenge (neu)
    '
    '
    Call Dos(False, Auftrag, FarbID, FarbName, ProNummer, ProNeu)
    '
    '
    If zuw Then
      '
      'Zuwaagemenge
      '
      '
      Call Dos(zuw, Auftrag, FarbID, FarbName, ProNummer, ProZuw)
    End If
  End Sub
  Sub SubSpeichern(ByRef FaWrt As Integer, ByRef UserID As Integer, ByRef MethID As Integer, ByRef MessgeraetID As Integer, ByRef FarbTable As DataTable)
  End Sub
  '
  Sub Dos(ByVal zuw As Boolean, ByVal Auftrag As String, ByVal FarbID() As Integer, ByVal FarbName() As String, ByVal ProNummer() As String, ByVal ProMenge() As Single)
    Dim i As Integer
    Dim GesMenge As Single
    Dim AnzRez As Integer
    '
    'Eingabegrößen
    '
    '
    'Farbname:   Name der verwendeten Farbmittel 
    'Pronummer:  Name der Produktnummern
    'Proalt:     Farbmittelmenge vor der Korrektur (Wird z.Zt. nicht verwendet)
    'Proneu:     Farbmittelmenge nach der Korrektur
    'Prozuw:     Farbmittelmenge für Zuwaage (Proneu-Proalt)
    '
    '
    '
    'KOPFDATEN 
    'Feld	Größe	INFO für Farbsystem	Beschreibung	Anmerkung
    '
    Dim mandt As String = Space(3) '	Nicht möglich	Mandant	Neues Feld
    Dim werks As String = Space(4) '	Nicht möglich	Werksnummer	Neues Feld
    Dim auftragsid As String = Space(10) '	Nicht möglich	AuftragsID	
    Dim aufnr As String = Space(12) '	Eingabe Feld	Auftragsnummer	Feldlänge vergrößert
    Dim modus As String = "10" '	Konstant = 10	Modus des Auftrages	
    Dim status As String = "1" '	Konstant = 1	Status des Datensatzes	
    Dim flatfile As String = "0" '	Konstant = 0	Filestatus Datensatz	Nicht relevant
    Dim plnbez As String = Space(18) '	Komponete	Materialnummer	
    Dim ktext As String = Space(40) '	Bezeichnung Komponente	Materialtext	
    Dim charg As String = Space(10) '	Nicht möglich	ChargenNr Kopfmaterial	
    Dim gamng As String = Space(13) '	Gesamtmenge Zuwaage	Menge Auftrag	  (Länge korrigiert 12)
    Dim gmein As String = " KG " '	Konstant = KG	Mengeneinheit	Immer KG  (Länge korrigiert  4)
    Dim gstri As String = Space(10) '	Systemdatum	Iststarttermin	
    Dim anzpos As String = Space(3) '	Anzahl Positionssätze	Anzahl zugehöriger Positionsätze	
    Dim verid As String = Space(4) '	Nicht möglich	Fertigungsversion	
    Dim ersda As String = Space(10) '	Systemdatum	Schnittstellen Datum	
    Dim uzeit As String = Space(8) '	Systemzeit	Schnittstellen Uhrzeit	


    'POSITIONSDATEN()
    'Feld	Größe	INFO für Farbsystem	Beschreibung	Anmerkung
    '
    'Dim mandt As String = Space(3) '	Nicht möglich	Mandant	Neues Feld
    'Dim werks As String = Space(4) '	Nicht möglich	Werksnummer	Neues Feld
    Dim posid As String = Space(14) '	Nicht möglich	PositionsID	Feld wird länger
    'Dim auftragsid As String = Space(10) '	Nicht möglich	AuftragsID	
    'Dim aufnr As String = Space(12) '	Eingabe Feld (=Kopfsatz)	Auftragsnummer	Feldlänge vergrößert
    'Dim status As String = Space(1) '	Konstant = 1	Status des Datensatzes	
    'Dim flatfile As String = Space(1) '	Konstant = 1	Filestatus Datensatz	Nicht relevant
    Dim vornr As String = Space(4) '	Nicht möglich	Vorgangsnummer	Neues Feld
    Dim matnr As String = Space(18) '	Komponete	Materialnummer	
    Dim maktx As String = Space(40) '	Bezeichnung Komponente	Materialtext	
    Dim lgort As String = Space(4) '	Nicht möglich	Lagerort	
    Dim in_rezeptur As String = Space(1) '	Nicht möglich	Rezeptmaterial	
    Dim bdmng As String = Space(13) '	Menge Zuwaage Position	Bedarfsmenge Position	(Prozuw)(File = DA_KOPF und DA_POSITION)
    '                               ' Gesamtmenge (Proneu) (File = DA_KOPFG und DA_POSITIONG) 
    Dim nmng As String = Space(13) '	Entnahmemenge ist immer=0
    Dim meins As String = " KG " '	Konstant = KG	Mengeneinheit	Immer KG
    'Dim charg As String = Space(10) '	Nicht möglich	Chargenummer Position	
    'Dim ersda As String = Space(10) '	Systemdatum	Schnittstellen Datum	
    'Dim uzeit As String = Space(8) '	Systemzeit	Schnittstellen Uhrzeit	
    '
    '
    '
    '
    '
    '
    '
    '
    Dim PrtLine As String
    Dim FileKopf As String
    Dim FilePosition As String
    Dim Folder As String = CurDir()
    Dim LUKopf As Integer = 8
    Dim LUPosition As Integer = 9
    '
    'Auftragsnummer
    '
    '
    '
    If zuw Then
      '
      'Zuwaage
      '
      '
      FileKopf = "DA_KOPF.TXT"
      FilePosition = "DA_POSITION.TXT"
    Else
      '
      'Neue Menge
      '
      '
      FileKopf = "DA_KOPFG.TXT"
      FilePosition = "DA_POSITIONG.TXT"
    End If


    aufnr = Auftrag
    If aufnr = "" Then Exit Sub
    If aufnr.Length < 12 Then
      aufnr = aufnr & Space(12 - aufnr.Length)
    End If
    aufnr = aufnr.Substring(0, 12)
    aufnr = aufnr.PadRight(12, " ")


    '
    'Datum
    '
    '
    ersda = String.Format("{0:d}", Now)
    '
    'Uhrzeit
    '
    '
    uzeit = String.Format("{0:T}", Now)
    gstri = ersda
    '
    'If Zuw Then
    'Gesamtzuwaage
    'ElseIf
    'neue Menge
    'End If

    '
    '
    GesMenge = 0.0
    For i = 0 To ProMenge.Count - 1
      GesMenge = GesMenge + ProMenge(i)
    Next i
    gamng = String.Format("{0:F3}", GesMenge)
    gamng = gamng.PadLeft(12, " ")
    anzpos = Format(ProMenge.Count, "000")


    '
    'Anzahl Farbmittel
    '
    '
    AnzRez = ProNummer.Count
    anzpos = Format(AnzRez, "000")
    '
    'Verzeichnis für DA_KOPF.TXT und DA_POSITION.TXT
    '
    '
    '
    Folder = GetPrivSettings("MANKIEWICZ", "FOLDER", Folder, COLORFileName())
    FileKopf = Folder & "\" & FileKopf
    FilePosition = Folder & "\" & FilePosition
    '
    '
    '
    'DA_KOPF.TXT

    '
    '
    '
    If File.Exists(FileKopf) Then
      FileOpen(LUKopf, FileKopf, OpenMode.Append)
    Else
      FileOpen(LUKopf, FileKopf, OpenMode.Output)
    End If
    '

    '
    PrtLine = ""
    PrtLine = PrtLine & mandt
    PrtLine = PrtLine & werks
    PrtLine = PrtLine & auftragsid
    PrtLine = PrtLine & aufnr
    PrtLine = PrtLine & modus
    PrtLine = PrtLine & status
    PrtLine = PrtLine & flatfile
    PrtLine = PrtLine & plnbez
    PrtLine = PrtLine & ktext
    PrtLine = PrtLine & charg
    PrtLine = PrtLine & gamng
    PrtLine = PrtLine & gmein
    PrtLine = PrtLine & gstri
    PrtLine = PrtLine & anzpos
    PrtLine = PrtLine & verid
    PrtLine = PrtLine & ersda
    PrtLine = PrtLine & uzeit

    PrintLine(LUKopf, PrtLine)
    FileClose(LUKopf)
    '
    '
    '
    '
    'DA_POSITION.TXT
    '
    '
    If File.Exists(FilePosition) Then
      FileOpen(LUPosition, FilePosition, OpenMode.Append)
    Else
      FileOpen(LUPosition, FilePosition, OpenMode.Output)
    End If

    For i = 0 To AnzRez - 1
      '
      'Materialnummer (Produktnummer)
      '
      '
      matnr = ProNummer(i)
      If matnr.Length < 18 Then
        matnr = matnr & Space(18 - matnr.Length)
      End If
      matnr = matnr.Substring(0, 18)
      '
      'Materialname (Farbmittelname)
      '
      '
      '
      maktx = FarbName(i)
      If maktx.Length < 40 Then
        maktx = maktx & Space(40 - maktx.Length)
      End If
      maktx = maktx.Substring(0, 40)
      '
      '
      bdmng = Space(12)

      If ProMenge(i) >= 0.0 Then
        bdmng = String.Format("{0:F3}", ProMenge(i))
        bdmng = bdmng.PadLeft(12, " ")
      End If
      '

      '
      '
      '
      '
      PrtLine = ""
      PrtLine = PrtLine & mandt
      PrtLine = PrtLine & werks
      PrtLine = PrtLine & posid
      PrtLine = PrtLine & auftragsid
      PrtLine = PrtLine & aufnr
      PrtLine = PrtLine & status
      PrtLine = PrtLine & flatfile
      PrtLine = PrtLine & vornr
      PrtLine = PrtLine & matnr
      PrtLine = PrtLine & maktx
      PrtLine = PrtLine & lgort
      PrtLine = PrtLine & in_rezeptur
      PrtLine = PrtLine & bdmng
      PrtLine = PrtLine & nmng
      PrtLine = PrtLine & meins
      PrtLine = PrtLine & charg
      PrtLine = PrtLine & ersda
      PrtLine = PrtLine & uzeit
      '
      '
      '
      '
      PrintLine(LUPosition, PrtLine)
    Next i
    FileClose(LUPosition)

  End Sub


  '*************************
  '*************************
  '
  '
  '
  Sub RenderFarbMasterAlt()
    Dim f As Field
    Dim i As Integer
    Dim Fwidth As Double
    Dim Fleft As Double
    ReportFarbMaster = New C1Report
    Dim grp As Group
    '
    '
    '
    'MasterReport
    '
    '
    'prevent reentrant calls
    '
    If ReportFarbMaster.IsBusy Then Beep() : Exit Sub
    '
    '
    '
    'initialize control
    '
    '
    With ReportFarbMaster
      .Clear()
      .Font.Name = "Tahoma"
      .Font.Size = 9
      .ReportName = "FarbMaster"
      .DataSource.Recordset = SectionTable
    End With

    '
    '
    'Initialize layout
    '
    '
    With ReportFarbMaster.Layout
      .Orientation = OrientationEnum.Auto
      .Width = 7.0 * 1440    '8.5 - margins
    End With
    '

    'Create a reportheader
    '
    '
    '
    With ReportFarbMaster.Sections(SectionTypeEnum.Header)
      .Height = 1000
      .Visible = True
      f = .Fields.Add("fldTitle", "Farbwerte Firma Mankiewicz", 1000, 500, 8000, 1440)
      f.Visible = True
      f.Font.Size = 18
      f.Font.Bold = True
      f.BackColor = Drawing.Color.LightGray
      f.Align = FieldAlignEnum.CenterMiddle

      f = .Fields.Add("fldMess", MessGeraet, 1000, 1500, 8000, 300)
      f.Visible = True
      f.Font.Size = 10
      f.Font.Bold = True
      f.BackColor = Drawing.Color.LightGray
      f.Align = FieldAlignEnum.CenterMiddle
      f = .Fields.Add("picture", "Picture", 8000, 750, 1000, 1440)
      f.Picture = My.Resources.MCS
    End With
    '
    '
    '
    'Create Page footer
    '
    '
    '
    With ReportFarbMaster.Sections(SectionTypeEnum.PageFooter)
      .Height = 500
      .Visible = True
      f = .Fields.Add("FldFtrLeft", Now, 0, 0, 4000, 300)
      f.Calculated = True
      '
      '
      'Seitennummerierung
      '
      '
      f = .Fields.Add("SeitenText", "Seite", ReportFarbMaster.Layout.Width - 1200, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITE", "Page", ReportFarbMaster.Layout.Width - 1000, 0, 500, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEVON", "von", ReportFarbMaster.Layout.Width - 650, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEN", "Pages", ReportFarbMaster.Layout.Width - 500, 0, 500, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      '
      '
      'Linie
      '
      'f = .Fields.Add("FldLine", "", 0, 0, ReportFarbMaster.Layout.Width, 20)
      'f.LineSlant = LineSlantEnum.NoSlant
      'f.BorderStyle = BorderStyleEnum.Solid
      'f.BorderColor = Drawing.Color.FromArgb(0, 0, 100)
    End With
    '
    '
    '
    'Create a page header
    '
    '
    '
    With ReportFarbMaster.Sections(SectionTypeEnum.PageHeader)
      .Height = 500
      .Visible = True
      .BackColor = Drawing.Color.LightGray
      Fwidth = 2200
      Fleft = 0
      For i = 0 To SectionTable.Columns.Count - 2
        f = .Fields.Add(SectionTable.Columns(i).Caption.Trim, SectionTable.Columns(i).Caption.Trim, Fleft, 100, Fwidth, 250)
        Fleft = Fleft + Fwidth
        Fwidth = 900
        f.Font.Bold = True
        f.Font.Size = 9
        If i = 0 Then
          f.Align = FieldAlignEnum.LeftMiddle
        Else
          f.Align = FieldAlignEnum.CenterMiddle
        End If
      Next
      '
      '
      'Linie
      '
      'f = .Fields.Add("LINIE", "", 0, 1000, ReportFarbMaster.Layout.Width, 20)
      'f.LineSlant = LineSlantEnum.NoSlant
      'f.LineWidth = 50
      'f.BorderStyle = BorderStyleEnum.Solid
      'f.BorderColor = Drawing.Color.DarkGray
    End With
    '

    '
    'create groups
    '
    '
    '
    '
    grp = ReportFarbMaster.Groups.Add("ISEC", "ISEC", SortEnum.Ascending)
    With grp.SectionHeader
      .Height = 500
      .Visible = True
      f = .Fields.Add(SectionTable.Columns(0).ColumnName, SectionTable.Columns(0).ColumnName, 0, 0, ReportFarbMaster.Layout.Width, 500)
      f.Calculated = True
      f.Align = FieldAlignEnum.LeftMiddle
      'f.Font.Bold = True
      grp.KeepTogether = KeepTogetherEnum.KeepWholeGroup
    End With
    '
    '
    '
    'Subreport (Details)
    'wird über ISEC mit Masterreport verknüpft
    '
    With ReportFarbMaster.Sections(SectionTypeEnum.Detail)
      f = .Fields.Add("FarbWerte", """ISEC="" & ISEC", 0, 0, 0, 0)
      f.Subreport = ReportFarbDetails
      f.Calculated = True
      f.CanGrow = True
      .Height = 1000
      f.CanShrink = True
      .Visible = True
    End With
    ''
  End Sub
  Sub RenderFarbDetailsAlt()
    Dim f As Field
    Dim Fwidth As Double
    Dim Fleft As Double
    Dim i As Integer

    ReportFarbDetails = New C1Report
    '
    '
    '
    'DetailReport
    '
    '
    'prevent reentrant calls
    '
    If ReportFarbDetails.IsBusy Then Beep() : Exit Sub
    '
    '
    '
    '
    With ReportFarbDetails
      .Clear()
      .Font.Name = "Tahoma"
      .Font.Size = 9
      .ReportName = "FarbWerte"
      .DataSource.Recordset = SubReportTable
    End With
    '
    '
    With ReportFarbDetails.Layout
      .Orientation = OrientationEnum.Portrait
      .Width = 7.0 * 1440    '8.5 - margins
    End With
    '
    '
    'create detail section
    '
    '
    '
    With ReportFarbDetails.Sections(SectionTypeEnum.Detail)
      .Height = 330
      .Visible = True
      Fwidth = 2200
      Fleft = 0
      For i = 0 To SubReportTable.Columns.Count - 2
        f = .Fields.Add(SubReportTable.Columns(i).ColumnName, SubReportTable.Columns(i).ColumnName, Fleft, 250, Fwidth, 250)
        f.Calculated = True
        f.Visible = True
        f.Font.Size = 9
        Fleft = Fleft + Fwidth
        Fwidth = 900
        If i = 0 Then
          f.Align = FieldAlignEnum.LeftMiddle
        Else
          f.Align = FieldAlignEnum.RightMiddle
        End If
      Next
    End With
    ''
  End Sub
End Class


