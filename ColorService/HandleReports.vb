Public Class HandleReports
  '
  'Ausdruck Farbwerte Mankiewicz
  'Unterforsthuber 26.06.2011)
  '
  '
  '
  '
  '
  Dim ReportFarbMaster As New C1Report
  Dim ReportFarbDetails As New C1Report
  Dim Sectiontable As DataTable
  Dim SubReporttable As DataTable
  Dim MnFarbTable As DataTable
  Dim MnFarbwerte As ValuesGrpsAssigns
  Dim MnGrpRwerte As RefValuesGrp
  Dim ColCount As Integer
  Dim Isec As Integer
  Dim Comment As String
  Dim MessBezeichnung As String
  Dim ppv As PrintPreviewDialog
  Dim pd As PrintDialog
  Dim Fwid As Double = 700
  Dim Flef As Double = 3750
  Dim Fleft As Double
  Dim Fwidth As Double

  Public Sub New()
    '
    ppv = New PrintPreviewDialog

    pd = New PrintDialog

  End Sub
  '
  WriteOnly Property Farbtable As DataTable
    Set(value As DataTable)
      MnFarbTable = value
    End Set
  End Property
  WriteOnly Property GrpRwerte As RefValuesGrp
    Set(value As RefValuesGrp)
      MnGrpRwerte = value
    End Set
  End Property
  WriteOnly Property Farbwerte As ValuesGrpsAssigns
    Set(value As ValuesGrpsAssigns)
      MnFarbwerte = value
    End Set
  End Property ''
  Sub SubDrucken()
    Dim Allgrow As DataRow
    Dim ZwiWert As String
    Dim UebSplit() As String
    Dim CountAnz As Integer
    Dim MinPlu As Boolean
    MessBezeichnung = Menueparam.Messg.Kbez
    Sectiontable = MnFarbTable.Clone
    SubReporttable = MnFarbTable.Clone
    ColCount = MnFarbTable.Columns.Count
    Sectiontable.Columns.Add("UEB1", GetType(String))
    Sectiontable.Columns.Add("UEB2", GetType(String))
    Sectiontable.Columns.Add("COUNT", GetType(Integer))
    Sectiontable.Columns.Add("ISEC", GetType(Integer))

    Sectiontable.Columns("DATE").DataType = GetType(String)
    SubReporttable.Columns.Add("ISEC", GetType(Integer))
    Isec = -1
    MinPlu = True
    For Each Allgrow In MnFarbTable.Rows
      If Allgrow(0) = "-" Then
        MinPlu = False
      ElseIf Allgrow(0) = "+" Then
        MinPlu = True
      End If
      If MinPlu And Not (Isec = -1 And Allgrow(0) = " ") Then
        If Allgrow(0) = "+" Or Allgrow(0) = "-" Then
          MinPlu = True
          '
          '
          'Überschrift gefunden
          '
          '
          Isec = Isec + 1
          UebSplit = Allgrow("NAME").split(Chr(0))
          Sectiontable.Rows.Add(Sectiontable.NewRow)
          Sectiontable.Rows(Sectiontable.Rows.Count - 1)("UEB1") = UebSplit(2)
          Sectiontable.Rows(Sectiontable.Rows.Count - 1)("UEB2") = UebSplit(0) & Space(2) & UebSplit(1)
          Sectiontable.Rows(Sectiontable.Rows.Count - 1)("ISEC") = Isec
          Continue For
        End If

        If Sectiontable.Rows.Count = (Isec + 1) And Allgrow(0) = "" Then
          '
          'Überschriften
          '
          '
          CountAnz = 0
          For j = 1 To MnFarbTable.Columns.Count - 1
            If IsNothing(Allgrow(j)) OrElse IsDBNull(Allgrow(j)) Then
              ZwiWert = ""
            Else
              ZwiWert = Allgrow(j)
            End If
            If Allgrow.Table.Columns(j).ColumnName = "DATE" Then
              ZwiWert = Texxt(375)
            End If
            Sectiontable.Rows(Sectiontable.Rows.Count - 1)(j) = ZwiWert
            If ZwiWert <> "" Then
              CountAnz = j
            End If
          Next j
          Sectiontable.Rows(Sectiontable.Rows.Count - 1)("ISEC") = Isec
          Sectiontable.Rows(Sectiontable.Rows.Count - 1)("COUNT") = CountAnz
        ElseIf Asc(Allgrow(0)) = 32 And Sectiontable.Rows.Count = (Isec + 1) Then
          '
          'Leerzeile
          '
          '
          SubReporttable.Rows.Add(SubReporttable.NewRow)
          SubReporttable.Rows(SubReporttable.Rows.Count - 1)(0) = " "
          SubReporttable.Rows(SubReporttable.Rows.Count - 1)("ISEC") = Isec
        ElseIf Asc(Allgrow(0)) > 96 And Sectiontable.Rows.Count = (Isec + 1) Then
          '
          '
          'Merkmale
          '
          '
          SubReporttable.Rows.Add(SubReporttable.NewRow)

          For j = 0 To MnFarbTable.Columns.Count - 1
            SubReporttable.Rows(SubReporttable.Rows.Count - 1)(j) = Allgrow(j)
          Next j
          SubReporttable.Rows(SubReporttable.Rows.Count - 1)("ISEC") = Isec
        End If
      End If
    Next
    '
    'VBscriptnamen wie z.B. CHR oder Zahlen dürfen nicht als text (und damit als ColumnName) in Reports verwendet werden
    'deshalb Ergänzung mit Buchstabe "X"
    For j = 0 To Sectiontable.Columns.Count - 2
      Sectiontable.Columns(j).ColumnName = "X" & Sectiontable.Columns(j).ColumnName
    Next j
    For j = 0 To SubReporttable.Columns.Count - 2
      SubReporttable.Columns(j).ColumnName = "X" & SubReporttable.Columns(j).ColumnName
    Next j
    '
    '
    '
    '
    '
    Sectiontable.AcceptChanges() '
    SubReporttable.AcceptChanges()

    Call RenderFarbMaster()
    Call RenderFarbDetails()
    ppv.Document = ReportFarbMaster.Document
    pd.Document = ReportFarbMaster.Document
    'If pd.ShowDialog = DialogResult.OK Then
    ppv.WindowState = FormWindowState.Maximized

    ppv.ShowDialog()
    'End If
  End Sub
  Sub RenderFarbMaster()
    Dim f As Field
    Dim i As Integer
    Dim UebWidth As Double = 2000
    Dim FHeight As Double
    Dim FheightK As Double
    Dim Ftop As Double
    Dim Strueb As String
    Dim grp As Group
    FHeight = 250
    FheightK = 270
    '
    '
    '
    'MasterReport
    '
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
      '
      'Datasource 
      '
      '
      .DataSource.Recordset = Sectiontable
    End With

    '
    '
    'Initialize layout
    '
    '
    If Sectiontable.Columns.Count > 18 Then
      With ReportFarbMaster.Layout
        .Orientation = OrientationEnum.Landscape
        .Width = 10.0 * 1440    '8.5 - margins
      End With
    Else
      With ReportFarbMaster.Layout
        .Orientation = OrientationEnum.Portrait
        .Width = 7.0 * 1440    '8.5 - margins
      End With
    End If
    '

    'Create a reportheader
    '
    '
    '
    With ReportFarbMaster.Sections(SectionTypeEnum.Header)
      .Height = 750
      .Visible = True
      .BackColor = Drawing.Color.LightGray
      .CanGrow = True
      '
      '
      '
      '
      'Überschrift
      '
      '
      '
      f = .Fields.Add("fldUeb1", Texxt(100), UebWidth, 0, ReportFarbMaster.Layout.Width - 2 * UebWidth, FHeight)
      f.Visible = True
      f.Font.Size = 10
      f.Font.Bold = True
      f.BackColor = Drawing.Color.LightGray
      f.Align = FieldAlignEnum.CenterMiddle
      f = .Fields.Add("fldUeb2", Texxt(101), UebWidth, 250, ReportFarbMaster.Layout.Width - 2 * UebWidth, FHeight)
      f.Visible = True
      f.Font.Size = 10
      f.Font.Bold = True
      f.BackColor = Drawing.Color.LightGray
      f.Align = FieldAlignEnum.CenterMiddle
      f = .Fields.Add("fldUeb3", MenueParam.Menue.MethBez & Space(2) & "(" & MenueParam.User.Name & ")", UebWidth, 500, ReportFarbMaster.Layout.Width - 2 * UebWidth, FHeight)
      f.Visible = True
      f.Font.Size = 10
      f.Font.Bold = True
      f.BackColor = Drawing.Color.LightGray
      f.Align = FieldAlignEnum.CenterMiddle
      '
      '
      '
      'Labor
      '
      '
      f = .Fields.Add("fldLab1", MenueParam.Messg.LaborText1, 0, 0, UebWidth, FHeight)
      f.Visible = True
      f.Font.Size = 10
      f.Font.Bold = True
      f.BackColor = Drawing.Color.LightGray
      f.Align = FieldAlignEnum.LeftMiddle
      f = .Fields.Add("fldLab2", MenueParam.Messg.LaborText2, 0, 250, UebWidth, FHeight)
      f.Visible = True
      f.Font.Size = 10
      f.Font.Bold = True
      f.BackColor = Drawing.Color.LightGray
      f.Align = FieldAlignEnum.LeftMiddle
      f = .Fields.Add("fldLab3", MenueParam.Messg.LaborText3, 0, 500, UebWidth, FHeight)
      f.Visible = True
      f.Font.Size = 10
      f.Font.Bold = True
      f.BackColor = Drawing.Color.LightGray
      f.Align = FieldAlignEnum.LeftMiddle
      '
      '
      '
      'Datum
      '
      '
      f = .Fields.Add("fldAux1", Now.ToString("d"), ReportFarbMaster.Layout.Width - UebWidth, 0, UebWidth, FHeight)
      f.Visible = True
      f.Font.Size = 10
      f.Font.Bold = True
      f.BackColor = Drawing.Color.LightGray
      f.Align = FieldAlignEnum.RightMiddle
      '
      'Uhrzeit
      '
      '
      f = .Fields.Add("fldAux2", Now.ToString("t"), ReportFarbMaster.Layout.Width - UebWidth, 250, UebWidth, FHeight)
      f.Visible = True
      f.Font.Size = 10
      f.Font.Bold = True
      f.BackColor = Drawing.Color.LightGray
      f.Align = FieldAlignEnum.RightMiddle
      '
      '
      'Messgerät
      '
      '
      '
      f = .Fields.Add("fldAux3", MenueParam.Messg.Kbez, ReportFarbMaster.Layout.Width - UebWidth, 500, UebWidth, FHeight)
      f.Visible = True
      f.Font.Size = 10
      f.Font.Bold = True
      f.BackColor = Drawing.Color.LightGray
      f.Align = FieldAlignEnum.RightMiddle
      '

      '
      '
      '
      '
      '
      '
      'Art der Farbstärkeberechnung
      '
      '
      '
      Ftop = 510
      Select Case MenueParam.MethID

        Case 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 22, 23
          Ftop = Ftop + FheightK
          f = .Fields.Add("fldfSTAE", Texxt(450 + MenueParam.Menue.FstaeID), 0, Ftop, ReportFarbMaster.Layout.Width, FHeight)
          f.Visible = True
          f.Font.Size = 10
          f.Font.Bold = False
          f.BackColor = Drawing.Color.White
          f.Align = FieldAlignEnum.LeftMiddle
      End Select
      '
      '
      'Deckvermögen
      '
      '
       Select MenueParam.MethID
        Case 15, 16
          Ftop = Ftop + FheightK
          f = .Fields.Add("fldfDECK", Texxt(186) & ":" & Format(MenueParam.Menue.KdeWS, " ##.00"), 0, Ftop, ReportFarbMaster.Layout.Width, FHeight)
          f.Visible = True
          f.Font.Size = 10
          f.Font.Bold = False
          f.BackColor = Drawing.Color.White
          f.Align = FieldAlignEnum.LeftMiddle
      End Select

      '
      'Linie
      '
      'Ftop = Ftop + FheightK
      ' f = .Fields.Add("FldLine1", "", 0, Ftop, ReportFarbMaster.Layout.Width, 40)
      ' f.LineSlant = LineSlantEnum.NoSlant
      ' f.BorderStyle = BorderStyleEnum.Solid
      ' f.LineWidth = 40
      ' f.BorderColor = Drawing.Color.Black
      '

      '
      '
      'Kommentar
      '
      If MnFarbwerte.Kommentar.Trim <> "" Then
        Ftop = Ftop + FheightK
        f = .Fields.Add("fldfKOM", MnFarbwerte.Kommentar, 0, Ftop, ReportFarbMaster.Layout.Width, FHeight)
        f.Visible = True
        f.Font.Size = 10
        f.Font.Bold = False
        f.BackColor = Drawing.Color.White
        f.Align = FieldAlignEnum.LeftMiddle
      End If
      '
      '
      ''Pruefmittel
      '
      '
      '
      If MnFarbwerte.PruefMittel.Trim <> "" Then
        Ftop = Ftop + FheightK
        f = .Fields.Add("fldfPRF", Texxt(1640) & ": " & MnFarbwerte.PruefMittel, 0, Ftop, ReportFarbMaster.Layout.Width, FHeight)
        f.Visible = True
        f.Font.Size = 10
        f.Font.Bold = False
        f.BackColor = Drawing.Color.White
        f.Align = FieldAlignEnum.LeftMiddle
      End If
      '
      '
      ''Pruefmittel
      '
      '
      '
      If MnFarbwerte.ProduktArt.Trim <> "" Then
        Ftop = Ftop + FheightK
        f = .Fields.Add("fldfPDR", Texxt(1641) & ": " & MnFarbwerte.ProduktArt, 0, Ftop, ReportFarbMaster.Layout.Width, FHeight)
        f.Visible = True
        f.Font.Size = 10
        f.Font.Bold = False
        f.BackColor = Drawing.Color.White
        f.Align = FieldAlignEnum.LeftMiddle
      End If
      '
      '
      '
      '
      'GK-Werte
      '
      '
      '
      '
      '
       Select MenueParam.MethID
        Case 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 22
          '
          'Linie
          '
         
          '
          '
          '
          '
          Strueb = Texxt(151)
          Strueb = Strueb & ":" & Space(2)
          For i = 0 To 9
            Strueb = Strueb & Format(MenueParam.User.Winkel(0).GK(i), " #0.00")
          Next i
          Ftop = Ftop + FheightK
          f = .Fields.Add("fldfGK0", Strueb, 0, Ftop, ReportFarbMaster.Layout.Width, FHeight)
          f.Visible = True
          f.Font.Size = 10
          f.Font.Bold = False
          f.BackColor = Drawing.Color.White
          f.Align = FieldAlignEnum.LeftMiddle
          '
          '
          '
          '

          
      End Select
      '
      'Untergrund weiß
      '
      '
      '
      Select Case MenueParam.MethID
        Case 5, 6, 8, 16, 19, 20
          Ftop = Ftop + FheightK
          f = .Fields.Add("fldfUNW", Texxt(355) & ":  " & MnGrpRwerte("W").RefUnt.Name, 0, Ftop, ReportFarbMaster.Layout.Width, FHeight)
          f.Visible = True
          f.Font.Size = 10
          f.Font.Bold = False
          f.BackColor = Drawing.Color.White
          f.Align = FieldAlignEnum.LeftMiddle



          'Y = Y + dy
          'Call claDRU.drzeil(1, X + 2 * dx, Y, 6 * dx - claDRU.ScWidth, Texxt(355), Printer)
          'Call claDRU.drzeil(0, X + 8 * dx, Y, ScWi - 8 * dx, ":  " & MnGrpRwerte("Z")(1).Name, Printer)
      End Select
      '

      '
      '
      '
      'Untergrund schwarz
      '
      '
      '
      Select Case MenueParam.MethID
        Case 8, 16, 20
          Ftop = Ftop + FheightK
          f = .Fields.Add("fldfUNS", Texxt(356) & ":  " & MnGrpRwerte("S").RefUnt.Name, 0, Ftop, ReportFarbMaster.Layout.Width, FHeight)
          f.Visible = True
          f.Font.Size = 10
          f.Font.Bold = False
          f.BackColor = Drawing.Color.White
          f.Align = FieldAlignEnum.LeftMiddle




          'Y = Y + dy
          'Call claDRU.drzeil(1, X + 2 * dx, Y, 6 * dx - claDRU.ScWidth, Texxt(356), Printer)
          'Call claDRU.drzeil(0, X + 8 * dx, Y, ScWi - 8 * dx, ":  " & MnGrpRwerte("Z")(2).Name, Printer)
      End Select
      '
      '
      '
      '
      '
      '
      '
      'Weiß-,Schwarz- und Grauverschnitt
      '
      '
      '
      Select Case MenueParam.MethID
        Case 7, 15
          Ftop = Ftop + FheightK
          f = .Fields.Add("fldfUNW", Texxt(352) & ":  " & MnGrpRwerte("W").RefUnt.Name, 0, Ftop, ReportFarbMaster.Layout.Width, FHeight)
          f.Visible = True
          f.Font.Size = 10
          f.Font.Bold = False
          f.BackColor = Drawing.Color.White
          f.Align = FieldAlignEnum.LeftMiddle
          '
          '
          '
          Ftop = Ftop + FheightK
          f = .Fields.Add("fldfUNS", Texxt(353) & ":  " & MnGrpRwerte("S").RefUnt.Name, 0, Ftop, ReportFarbMaster.Layout.Width, FHeight)
          f.Visible = True
          f.Font.Size = 10
          f.Font.Bold = False
          f.BackColor = Drawing.Color.White
          f.Align = FieldAlignEnum.LeftMiddle
          '
          '
          '
          Ftop = Ftop + FheightK
          f = .Fields.Add("fldfUNG", Texxt(354) & ":  " & MnGrpRwerte("G").RefUnt.Name, 0, Ftop, ReportFarbMaster.Layout.Width, FHeight)
          f.Visible = True
          f.Font.Size = 10
          f.Font.Bold = False
          f.BackColor = Drawing.Color.White
          f.Align = FieldAlignEnum.LeftMiddle

          'Y = Y + dy
          'Call claDRU.drzeil(1, X + 2 * dx, Y, 6 * dx - claDRU.ScWidth, Texxt(352), Printer)
          'Call claDRU.drzeil(0, X + 8 * dx, Y, ScWi - 8 * dx, ":  " & MnGrpRwerte("Z")(3).Name, Printer)
          'Y = Y + dy
          'Call claDRU.drzeil(1, X + 2 * dx, Y, 6 * dx - claDRU.ScWidth, Texxt(353), Printer)
          'Call claDRU.drzeil(0, X + 8 * dx, Y, ScWi - 8 * dx, ":  " & MnGrpRwerte("Z")(4).Name, Printer)
          'Y = Y + dy
          'Call claDRU.drzeil(1, X + 2 * dx, Y, 6 * dx - claDRU.ScWidth, Texxt(354), Printer)
          'Call claDRU.drzeil(0, X + 8 * dx, Y, ScWi - 8 * dx, ":  " & MnGrpRwerte("Z")(5).Name, Printer)
      End Select
      '

    End With
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
      f = .Fields.Add("SeitenText", "Seite", ReportFarbMaster.Layout.Width - 1350, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITE", "Page", ReportFarbMaster.Layout.Width - 1100, 0, 500, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEVON", "von", ReportFarbMaster.Layout.Width - 750, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEN", "Pages", ReportFarbMaster.Layout.Width - 500, 0, 500, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      '

    End With
    '
    '


    '
    'create groups
    '

    Fwidth = Fwid
    '
    '
    grp = ReportFarbMaster.Groups.Add("ISEC", "ISEC", SortEnum.Ascending)
    With grp.SectionHeader
      '
      '
      'Height muss groß genug gewählt werden, damit Keeptogether für die Gruppe wirksam ist
      '
      '
      '
      .Height = 1500
      .Visible = True
      .KeepTogether = KeepTogetherEnum.KeepWholeGroup
      '
      '
      '
      '
      'Linie
      '
      f = .Fields.Add("FldLine1", "", 0, 40, ReportFarbMaster.Layout.Width, 40)
      f.LineSlant = LineSlantEnum.NoSlant
      f.BorderStyle = BorderStyleEnum.Solid
      f.LineWidth = 40
      f.BorderColor = Drawing.Color.Black
      '
      '
      'Gruppenüberschrift
      '
      '
      '

      f = .Fields.Add(Sectiontable.Columns("XUEB1").ColumnName, Sectiontable.Columns("XUEB1").ColumnName, 0, 0, 0.5 * ReportFarbMaster.Layout.Width, 500)
      f.Calculated = True
      f.Visible = True
      f.Font.Bold = True
      f.Font.Size = 9
      f.Align = FieldAlignEnum.LeftMiddle
      '

      f = .Fields.Add(Sectiontable.Columns("XUEB2").ColumnName, Sectiontable.Columns("XUEB2").ColumnName, 0.5 * ReportFarbMaster.Layout.Width, 0, 0.5 * ReportFarbMaster.Layout.Width, 500)
      f.Calculated = True
      f.Visible = True
      f.Font.Bold = True
      f.Font.Size = 9
      f.Align = FieldAlignEnum.RightMiddle
      '
      '
      'Linie
      '
      Fleft = Flef
      f = .Fields.Add("FldLine2", "", Fleft, 750, ReportFarbMaster.Layout.Width - Fleft, 40)
      f.LineSlant = LineSlantEnum.NoSlant
      f.BorderStyle = BorderStyleEnum.Solid
      f.LineWidth = 20
      f.BorderColor = Drawing.Color.Black
      '
      '
      '
      '
      'Name und Merkmalsbezeichnung
      '
      '
      '

      f = .Fields.Add(Sectiontable.Columns(2).ColumnName, Sectiontable.Columns(2).ColumnName, 0, 400, Fleft, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 9
      f.Align = FieldAlignEnum.LeftMiddle

      For i = 5 To Min(20, Sectiontable.Columns.Count - 5)
        f = .Fields.Add(Sectiontable.Columns(i).ColumnName, Sectiontable.Columns(i).ColumnName, Fleft, 400, Fwidth, FHeight)
        f.Calculated = True
        f.Visible = True
        f.Font.Size = 9
        f.Align = FieldAlignEnum.CenterMiddle
        Fleft = Fleft + Fwidth
      Next
      '
      '
      'Subreport  
      'wird über ISEC mit Masterreport verknüpft
      '
      '
      f = .Fields.Add("FarbWerte1", """ISEC="" & ISEC", 0, 800, 0, 0)
      f.Subreport = ReportFarbDetails
      f.Calculated = True
      f.CanGrow = True
      f.CanShrink = True


      ''


    End With
    '
    '
    '
    '
    'Gruppe, falls zusätzliche Merkmale vorhanden
    '
    '
    '


    '


  End Sub
 
  Sub RenderFarbDetails()
    Dim f As Field
    Dim FHeight As Double
    Dim i As Integer
    FHeight = 250

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
    'ReportFarbDetails1
    '
    '
    '
    With ReportFarbDetails
      .Clear()
      .Font.Name = "Tahoma"
      .Font.Size = 9
      .ReportName = "FarbWerte1"
      '
      'Datasource 
      '
      .DataSource.Recordset = SubReporttable
    End With
    '
    '
    With ReportFarbDetails.Layout
      .Orientation = OrientationEnum.Auto
      .Width = 7.0 * 1440    '8.5 - margins
    End With
    '
    '
    'create detail section
    '
    '
    '
    Fleft = Flef
    Fwidth = Fwid
    With ReportFarbDetails.Sections(SectionTypeEnum.Detail)
      .Height = 300
      .Visible = True

      '
      'Name
      '
      '
      f = .Fields.Add(SubReporttable.Columns(2).ColumnName, SubReporttable.Columns(2).ColumnName, 0, 0, Fleft, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 9
      f.Align = FieldAlignEnum.LeftMiddle
      For i = 5 To Min(20, Sectiontable.Columns.Count - 5)
        f = .Fields.Add(SubReporttable.Columns(i).ColumnName, SubReporttable.Columns(i).ColumnName, Fleft, 0, Fwidth, FHeight)
        f.Calculated = True
        f.Visible = True
        f.Font.Size = 9
        Fleft = Fleft + Fwidth
        f.Align = FieldAlignEnum.RightMiddle
      Next i
      .KeepTogether = True
    End With
    '
    ''
  End Sub
  '
 
  '
  '
  '
 
 
End Class
