Option Explicit On
Option Compare Text
Option Strict Off
Public Class frmColorDrucken
  '
  'Scripts
  '

  Dim TabRwert As DataTable
  Dim TabFarbwert As DataTable
  Dim TabRezeptMaster As DataTable
  Dim TabRezeptDetail As DataTable
  Dim TabSortimentMaster As DataTable
  Dim TabSortimentDetail As DataTable
  Dim ppv As PrintPreviewDialog
  Dim Pd As PrintDialog

  Dim WithEvents Printdoc As PrintDocument
  Dim ReportRwertMaster As C1Report
  Dim ReportRwertDetail As C1Report

  Dim ReportFarbwertMaster As C1Report
  Dim ReportFarbwertdetail As C1Report

  Dim ReportRezeptMaster As C1Report
  Dim ReportRezeptDetail As C1Report
  Dim ReportSortimentMaster As C1Report
  Dim ReportSortimentDetail As C1Report

  Dim Adapt As OleDbDataAdapter
  Dim Command As OleDbCommand
  '
  '
  Dim GroupRwert As HandleRwerte
  Dim GroupRezept As HandleRezGeneral

  Private Sub frmColorDrucken_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Me.Text = Texxt(1906) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"

    GroupRwert = New HandleRwerte
    GroupRezept = New HandleRezGeneral

    ppv = New PrintPreviewDialog
    Pd = New PrintDialog

    TabRwert = New DataTable

    TabFarbwert = New DataTable
    TabRezeptMaster = New DataTable
    TabRezeptDetail = New DataTable

    TabSortimentMaster = New DataTable
    TabSortimentDetail = New DataTable

    Adapt = New OleDbDataAdapter
    Command = New OleDbCommand("", Cndat)
    Adapt.SelectCommand = Command
    '
    '
    tabDrucken.TabPages(0).Text = Texxt(3110)
    tabDrucken.TabPages(1).Text = Texxt(3111)
    tabDrucken.TabPages(2).Text = Texxt(3112)
    tabDrucken.TabPages(3).Text = Texxt(3113)
    lblUSE.Text = Texxt(201)
    lblMESS.Text = Texxt(204)
    lblMISCH.Text = Texxt(406)
    lblSQLRwert.Text = Texxt(369)
    lblGRPRwert.Text = Texxt(386)
    chkDATRwert.Text = Texxt(375)
    lblVONRwert.Text = Texxt(376)
    lblBISRwert.Text = Texxt(377)
    txtBISRwert.Text = Format$(DateAdd("d", 0, Today))
    txtVONRwert.Text = Format$(DateAdd("d", -1 * 365, Today))

    lblSQLRezept.Text = Texxt(369)
    lblGRPRezept.Text = Texxt(386)
    chkDATRezept.Text = Texxt(375)
    lblVONRezept.Text = Texxt(376)
    lblBISRezept.Text = Texxt(377)
    txtBISRezept.Text = Format$(DateAdd("d", 0, Today))
    txtVONRezept.Text = Format$(DateAdd("d", -1 * 365, Today))

    txtUse.Text = MenueParam.User.Name
    If MenueParam.MischID = -1 Then
      lblMISCH.Visible = False
      txtMisch.Visible = False
    End If
    txtMess.Text = MenueParam.Messg.Kbez
    txtMisch.Text = MenueParam.Misch.Kbez
    btnDRUCKRwert.Text = Texxt(397)
    btnDRUCKRezept.Text = Texxt(397)
    btnDruckFarbmittel.Text = Texxt(397)
    btnDruckSortiment.Text = Texxt(397)


    '
    '
    GroupRwert.cboGRP = cboGRPRwert
    GroupRwert.lblGRP = lblGRPRwert
    GroupRwert.GRoupList()
    '
    '
   
    If MenueParam.MischID = -1 Then
      For i = 1 To tabDrucken.TabPages.Count - 1
        tabDrucken.TabPages.RemoveAt(1)
      Next i
    Else
      GroupRezept.cboGRP = cboGRPRezept
      GroupRezept.lblGRP = lblGRPRezept
      GroupRezept.GroupList()
    End If

  End Sub

  Sub RenderRwert()
    Dim f As Field
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
    ReportRwertMaster = New C1Report
    ReportRwertDetail = New C1Report


    '
    '
    '
    'DetailReport
    '
    '
    'prevent reentrant calls
    '
    If ReportRwertDetail.IsBusy Then Beep() : Exit Sub
    '
    '
    '
    '
    With ReportRwertDetail


      .Clear()
      '.Font.Name = "Microsoft Sans Serif"
      '.Font.Size = 8.25
      .ReportName = "RwertDetails"
      .DataSource.Recordset = TabRwert
      .OnOpen = "cnt = 0"
    End With
    '
    '
    With ReportRwertDetail.Layout

      '.Orientation = OrientationEnum.Auto
      '.Width = 7.0 * 1440    '8.5 - margins
    End With
    '
    '
    'create detail section
    '
    '
    '

    With ReportRwertDetail.Sections(SectionTypeEnum.Detail)
      .Name = "Rwert"
      .Height = MerkHeight
      .Visible = True
      Fwidth = NamWidth
      Fleft = 20
      f = .Fields.Add(TabRwert.Columns("RWERT_NAME").ColumnName, TabRwert.Columns("RWERT_NAME").ColumnName, Fleft, 100.0, NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.LeftMiddle
      .OnPrint = "Rwert.BackColor = IIf(_cnt Mod 2 = 0, RGB(255, 255, 255), RGB(240, 240, 240))" & vbCrLf & _
                 "cnt=CnT +1"
      '
      Fleft = Fleft + 0.5 * NamWidth
      f = .Fields.Add(TabRwert.Columns("RWERT_BEM").ColumnName, TabRwert.Columns("RWERT_BEM").ColumnName, Fleft, 100.0, 1.5 * NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.RightMiddle
      '
      '
      '
      '
      Fleft = Fleft + 2.5 * NamWidth
      f = .Fields.Add(TabRwert.Columns("RWERT_CME").ColumnName, TabRwert.Columns("RWERT_CME").ColumnName, Fleft, 100.0, 0.5 * NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.RightMiddle
      '
      '
      '

    End With
    '
    '
    'MasterReport
    '
    '
    'prevent reentrant calls
    '
    If ReportRwertMaster.IsBusy Then Beep() : Exit Sub
    '
    'Initialize layout (Master)
    '
    '


    '
    '
    'initialize control
    '
    '
    With ReportRwertMaster
      .Clear()
      .Font.Name = "Microsoft Sans Serif"
      .Font.Size = 8.25
      .ReportName = "RwertMaster"
    End With
    '
    '
    '
    '
    '
    With ReportRwertMaster.Layout
      '
      '
      '1 Inch=1440 twips
      '
      '
      .Orientation = OrientationEnum.Portrait
      .MarginLeft = Margin
      .MarginTop = Margin
      .MarginRight = Margin
      .MarginBottom = Margin
      .Width = ReportRwertMaster.C1Document.PageLayout.PageSettings.Width.ConvertUnit(C1.C1Preview.UnitTypeEnum.Twip) - .MarginLeft - .MarginRight
      WholeWidth = .Width
    End With

    '
    '
    '

    'Create a reportheader
    '
    '
    '
    With ReportRwertMaster.Sections(SectionTypeEnum.Header)

      .Height = 50
      .Visible = True
      '
      'Linie (horizontal)
      '
      f = .Fields.Add("LINIEH1", "", 0, 0, WholeWidth, 10)
      f.LineSlant = LineSlantEnum.NoSlant
      f.LineWidth = 5
      f.BorderStyle = BorderStyleEnum.Solid
      f.BorderColor = Drawing.Color.Black
      f = .Fields.Add("fldTitle", "R-Werte für " & MenueParam.Messg.Kbez, 0.15 * WholeWidth, 0.2 * MerkHeight, ReportRwertMaster.Layout.Width, 2 * MerkHeight)
      f.Visible = True
      f.Font.Size = 14
      f.Font.Bold = True
      f.BackColor = Drawing.Color.White
      f.Align = FieldAlignEnum.LeftMiddle
      '




      f = .Fields.Add("DateTime", Now, 0, MerkHeight, WholeWidth, 2 * MerkHeight)
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
    With ReportRwertMaster.Sections(SectionTypeEnum.PageFooter)
      .Height = 500
      .Visible = True

      '
      'Seitennummerierung
      '
      '
      f = .Fields.Add("SeitenText", "Seite", ReportRwertMaster.Layout.Width - 1200, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITE", "Page", ReportRwertMaster.Layout.Width - 1000, 0, 500, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEVON", "von", ReportRwertMaster.Layout.Width - 650, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEN", "Pages", ReportRwertMaster.Layout.Width - 500, 0, 650, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      '
      '
    End With
    '
    '
    '
    'Create a page header
    '
    '
    '
    With ReportRwertMaster.Sections(SectionTypeEnum.PageHeader)
      .Height = 200
      .Visible = True


      f = .Fields.Add("R-Wertname", "R-Wertname", 0, 0, WholeWidth + 100, 500)
      f.Calculated = False
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      '
      '
      '
      '
      '
      f = .Fields.Add("Bemerkung", "Bemerkung", 1.5 * NamWidth, 0, WholeWidth + 100, 500)
      f.Calculated = False
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      '
      '
      f = .Fields.Add("Kennung", "Kennung", 3 * NamWidth, 0, WholeWidth + 100, 500)
      f.Calculated = False
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      '
      '
      '
    End With
    '

    '
    '
    'create groups
    '
    '
    '
    '
    grp = ReportRwertMaster.Groups.Add("RWERT_ID", "RWERT_ID", SortEnum.Ascending)
    With grp.SectionHeader


      .Height = MerkHeight
      .Visible = True
      .BackColor = Drawing.Color.White

    
      grp.KeepTogether = KeepTogetherEnum.KeepWholeGroup


    End With
    '
    '
    '
    '
    '
    With ReportRwertMaster.Sections(SectionTypeEnum.Detail)
      f = .Fields.Add("Rwerte", "", 0, 0, 0, 0)
      f.Subreport = ReportRwertDetail
      f.Calculated = True
      f.CanGrow = True
      .Height = 1
      f.CanShrink = True
      .Visible = True
    End With
    ''
  End Sub

  Sub RenderFarbmittel()
    Dim f As Field
    Dim grp As Group
    Dim Margin As Double = 770
    Dim FHeight As Double = 330
    Dim MerkWidth As Double = 770
    Dim NamWidth As Double = 8800
    Dim MerkHeight As Double = 215
    Dim WholeWidth As Double
    Dim Fwidth As Double
    Dim Fleft As Double
    '
    '
    '
    '
    ReportFarbwertMaster = New C1Report
    ReportFarbwertdetail = New C1Report


    '
    '
    '
    'DetailReport
    '
    '
    'prevent reentrant calls
    '
    If ReportFarbwertdetail.IsBusy Then Beep() : Exit Sub
    '
    '
    '
    '
    With ReportFarbwertdetail


      .Clear()
      '.Font.Name = "Microsoft Sans Serif"
      '.Font.Size = 8.25
      .ReportName = "RwertDetails"
      .DataSource.Recordset = TabFarbwert
      .OnOpen = "cnt = 0"
    End With
    '
    '
    With ReportFarbwertdetail.Layout

      '.Orientation = OrientationEnum.Auto
      '.Width = 7.0 * 1440    '8.5 - margins
    End With
    '
    '
    'create detail section
    '
    '
    '

    With ReportFarbwertdetail.Sections(SectionTypeEnum.Detail)
      .Name = "Farbmittel"
      .Height = MerkHeight
      .Visible = True
      Fwidth = NamWidth
      Fleft = 20
      f = .Fields.Add(TabFarbwert.Columns("FARBM_NAME").ColumnName, TabFarbwert.Columns("FARBM_NAME").ColumnName, Fleft, 100.0, NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.LeftMiddle
      .OnPrint = "Farbmittel.BackColor = IIf(_cnt Mod 2 = 0, RGB(255, 255, 255), RGB(240, 240, 240))" & vbCrLf & _
                 "cnt=CnT +1"
    End With
    '
    '
    'MasterReport
    '
    '
    'prevent reentrant calls
    '
    If ReportFarbwertMaster.IsBusy Then Beep() : Exit Sub
    '
    'Initialize layout (Master)
    '
    '


    '
    '
    'initialize control
    '
    '
    With ReportFarbwertMaster
      .Clear()
      .Font.Name = "Microsoft Sans Serif"
      .Font.Size = 8.25
      .ReportName = "FarbwertMaster"
    End With
    '
    '
    '
    '
    '
    With ReportFarbwertMaster.Layout
      '
      '
      '1 Inch=1440 twips
      '
      '
      .Orientation = OrientationEnum.Portrait
      .MarginLeft = Margin
      .MarginTop = Margin
      .MarginRight = Margin
      .MarginBottom = Margin
      .Width = ReportFarbwertMaster.C1Document.PageLayout.PageSettings.Width.ConvertUnit(C1.C1Preview.UnitTypeEnum.Twip) - .MarginLeft - .MarginRight
      WholeWidth = .Width
    End With

    '
    '
    '

    'Create a reportheader
    '
    '
    '
    With ReportFarbwertMaster.Sections(SectionTypeEnum.Header)

      .Height = 50
      .Visible = True
      '
      'Linie (horizontal)
      '
      f = .Fields.Add("LINIEH1", "", 0, 0, WholeWidth, 10)
      f.LineSlant = LineSlantEnum.NoSlant
      f.LineWidth = 5
      f.BorderStyle = BorderStyleEnum.Solid
      f.BorderColor = Drawing.Color.Black
      f = .Fields.Add("fldTitle", "Farbmittel für " & MenueParam.Misch.Kbez, 0.15 * WholeWidth, 0.2 * MerkHeight, ReportFarbwertMaster.Layout.Width, 2 * MerkHeight)
      f.Visible = True
      f.Font.Size = 14
      f.Font.Bold = True
      f.BackColor = Drawing.Color.White
      f.Align = FieldAlignEnum.LeftMiddle
      '




      f = .Fields.Add("DateTime", Now, 0, MerkHeight, WholeWidth, 2 * MerkHeight)
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
    With ReportFarbwertMaster.Sections(SectionTypeEnum.PageFooter)
      .Height = 500
      .Visible = True

      '
      'Seitennummerierung
      '
      '
      f = .Fields.Add("SeitenText", "Seite", ReportFarbwertMaster.Layout.Width - 1200, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITE", "Page", ReportFarbwertMaster.Layout.Width - 1000, 0, 500, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEVON", "von", ReportFarbwertMaster.Layout.Width - 650, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEN", "Pages", ReportFarbwertMaster.Layout.Width - 500, 0, 650, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      '
      '
    End With
    '
    '
    '
    'Create a page header
    '
    '
    '
    With ReportFarbwertMaster.Sections(SectionTypeEnum.PageHeader)
      .Height = 200
      .Visible = True


      f = .Fields.Add("Farbmittelname", "Farbmittelname", 0, 0, WholeWidth + 100, 500)
      f.Calculated = False
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      '
      '
      '
      '
      '
      '
    End With
    '

    '
    '
    'create groups
    '
    '
    '
    '
    grp = ReportFarbwertMaster.Groups.Add("FARBM_ID", "FARBM_ID", SortEnum.Ascending)
    With grp.SectionHeader


      .Height = MerkHeight
      .Visible = True
      .BackColor = Drawing.Color.White


      grp.KeepTogether = KeepTogetherEnum.KeepWholeGroup


    End With
    '
    '
    '
    '
    '
    With ReportFarbwertMaster.Sections(SectionTypeEnum.Detail)
      f = .Fields.Add("Farbmittel", "", 0, 0, 0, 0)
      f.Subreport = ReportFarbwertdetail
      f.Calculated = True
      f.CanGrow = True
      .Height = 1
      f.CanShrink = True
      .Visible = True
    End With
    ''
  End Sub
  Sub RenderRezept()
    Dim f As Field
    Dim grp As Group
    Dim Margin As Double = 770
    Dim FHeight As Double = 330
    Dim MerkWidth As Double = 770
    Dim NamWidth As Double = 3600
    Dim MerkHeight As Double = 215
    Dim WholeWidth As Double
    Dim Fwidth As Double
    Dim Fleft As Double
    '
    '
    '
    '
    ReportRezeptMaster = New C1Report
    ReportRezeptDetail = New C1Report


    '
    '
    '
    'DetailReport
    '
    '
    'prevent reentrant calls
    '
    If ReportRezeptDetail.IsBusy Then Beep() : Exit Sub
    '
    '
    '
    '
    With ReportRezeptDetail


      .Clear()
      '.Font.Name = "Microsoft Sans Serif"
      '.Font.Size = 8.25
      .ReportName = "RezeptDetails"
      .DataSource.Recordset = TabRezeptDetail
      .OnOpen = "cnt = 0"
    End With
    '
    '
    With ReportRezeptDetail.Layout

      '.Orientation = OrientationEnum.Auto
      '.Width = 7.0 * 1440    '8.5 - margins
    End With
    '
    '
    'create detail section
    '
    '
    '

    With ReportRezeptDetail.Sections(SectionTypeEnum.Detail)
      .Name = "Farbmittel"
      .Height = MerkHeight
      .Visible = True
      Fwidth = NamWidth
      Fleft = 20
      f = .Fields.Add(TabRezeptDetail.Columns("FARBM_NAME").ColumnName, TabRezeptDetail.Columns("FARBM_NAME").ColumnName, Fleft, 100.0, NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.LeftMiddle
      .OnPrint = "Farbmittel.BackColor = IIf(_cnt Mod 2 = 0, RGB(255, 255, 255), RGB(240, 240, 240))" & vbCrLf & _
                 "cnt=CnT +1"
      '
      Fleft = Fleft + NamWidth
      f = .Fields.Add(TabRezeptDetail.Columns("FARBM_MENGE").ColumnName, TabRezeptDetail.Columns("FARBM_MENGE").ColumnName, Fleft, 100.0, 0.5 * NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.RightMiddle
      f.Format = "###.00"
      '
      '
      '
      '
      Fleft = Fleft + 0.5 * NamWidth
      f = .Fields.Add(TabRezeptDetail.Columns("FARBM_OPERAT").ColumnName, TabRezeptDetail.Columns("FARBM_OPERAT").ColumnName, Fleft, 100.0, 0.5 * NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.RightMiddle
      '
      '
      '
      '
      '
      Fleft = Fleft + 0.5 * NamWidth
      f = .Fields.Add(TabRezeptDetail.Columns("FARBM_LIMMNG").ColumnName, TabRezeptDetail.Columns("FARBM_LIMMNG").ColumnName, Fleft, 100.0, 0.5 * NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.RightMiddle
      f.Format = "###.00"
      '


    End With
    '
    '
    'MasterReport
    '
    '
    'prevent reentrant calls
    '
    If ReportRezeptMaster.IsBusy Then Beep() : Exit Sub
    '
    'Initialize layout (Master)
    '
    '


    '
    '
    'initialize control
    '
    '
    With ReportRezeptMaster
      .Clear()
      .Font.Name = "Microsoft Sans Serif"
      .Font.Size = 8.25
      .ReportName = "FarbMaster"
      .DataSource.Recordset = TabRezeptMaster
    End With
    '
    '
    '
    '
    '
    With ReportRezeptMaster.Layout
      '
      '
      '1 Inch=1440 twips
      '
      '
      .Orientation = OrientationEnum.Portrait
      .MarginLeft = Margin
      .MarginTop = Margin
      .MarginRight = Margin
      .MarginBottom = Margin
      .Width = ReportRezeptMaster.C1Document.PageLayout.PageSettings.Width.ConvertUnit(C1.C1Preview.UnitTypeEnum.Twip) - .MarginLeft - .MarginRight
      WholeWidth = .Width
    End With

    '
    '
    '

    'Create a reportheader
    '
    '
    '
    With ReportRezeptMaster.Sections(SectionTypeEnum.Header)

      .Height = 50
      .Visible = True
      '
      'Linie (horizontal)
      '
      f = .Fields.Add("LINIEH1", "", 0, 0, WholeWidth, 10)
      f.LineSlant = LineSlantEnum.NoSlant
      f.LineWidth = 5
      f.BorderStyle = BorderStyleEnum.Solid
      f.BorderColor = Drawing.Color.Black
      f = .Fields.Add("fldTitle", "Rezepte für " & MenueParam.Misch.Kbez, 0.15 * WholeWidth, 0.2 * MerkHeight, ReportRezeptMaster.Layout.Width, 2 * MerkHeight)
      f.Visible = True
      f.Font.Size = 14
      f.Font.Bold = True
      f.BackColor = Drawing.Color.White
      f.Align = FieldAlignEnum.LeftMiddle
      '




      f = .Fields.Add("DateTime", Now, 0, MerkHeight, WholeWidth, 2 * MerkHeight)
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
    With ReportRezeptMaster.Sections(SectionTypeEnum.PageFooter)
      .Height = 500
      .Visible = True

      '
      'Seitennummerierung
      '
      '
      f = .Fields.Add("SeitenText", "Seite", ReportRezeptMaster.Layout.Width - 1200, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITE", "Page", ReportRezeptMaster.Layout.Width - 1000, 0, 500, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEVON", "von", ReportRezeptMaster.Layout.Width - 650, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEN", "Pages", ReportRezeptMaster.Layout.Width - 500, 0, 650, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      '
      '
    End With
    '
    '
    '
    'Create a page header
    '
    '
    '
    With ReportRezeptMaster.Sections(SectionTypeEnum.PageHeader)
      .Height = 200
      .Visible = True


      f = .Fields.Add("Rezeptname", "Rezeptname", 0, 0, WholeWidth + 100, 500)
      f.Calculated = False
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      '
      '
      '
      '
      '
      f = .Fields.Add("Bemerkung", "Bemerkung", NamWidth, 0, WholeWidth + 100, 500)
      f.Calculated = False
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      '
      '
      '
      '
      'Linie (vertikal)
      '
      'f = .Fields.Add("LINIEV1", "", Fleft, 0, 0, 2 * MerkHeight)
      'f.LineSlant = LineSlantEnum.Down
      'f.LineWidth = 5
      'f.BorderStyle = BorderStyleEnum.Solid
      'f.BorderColor = Drawing.Color.Black

      'Next
      '
    End With
    '

    '
    'create groups
    '
    '
    '
    '
    grp = ReportRezeptMaster.Groups.Add("REZEPT_ID", "REZEPT_ID", SortEnum.Ascending)
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
      'Rezepte
      '
      '
      f = .Fields.Add(TabRezeptMaster.Columns("REZEPT_NAME").ColumnName, TabRezeptMaster.Columns("REZEPT_NAME").ColumnName, 0, 0, WholeWidth + 100, 500)
      f.Calculated = True
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      '
      '
      '
      '
      '
      f = .Fields.Add(TabRezeptMaster.Columns("REZEPT_BEM").ColumnName, TabRezeptMaster.Columns("REZEPT_BEM").ColumnName, NamWidth, 0, WholeWidth + 100, 500)
      f.Calculated = True
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      grp.KeepTogether = KeepTogetherEnum.KeepWholeGroup


    End With
    '
    '
    '
    'Subreport (Details)
    'wird über REZEPT_ID mit Masterreport verknüpft
    '
    With ReportRezeptMaster.Sections(SectionTypeEnum.Detail)
      f = .Fields.Add("FarbWerte", """REZEPT_ID="" & REZEPT_ID", 0, 0, 0, 0)
      f.Subreport = ReportRezeptDetail
      f.Calculated = True
      f.CanGrow = True
      .Height = 1
      f.CanShrink = True
      .Visible = True
    End With
    ''
  End Sub
  Sub RenderSortiment()
    Dim f As Field
    Dim grp As Group
    Dim Margin As Double = 770
    Dim FHeight As Double = 330
    Dim MerkWidth As Double = 770
    Dim NamWidth As Double = 3600
    Dim MerkHeight As Double = 215
    Dim WholeWidth As Double
    Dim Fwidth As Double
    Dim Fleft As Double
    '
    '
    '
    '

    ReportSortimentMaster = New C1Report
    ReportSortimentDetail = New C1Report


    '
    '
    '
    'DetailReport
    '
    '
    'prevent reentrant calls
    '
    If ReportSortimentDetail.IsBusy Then Beep() : Exit Sub
    '
    '
    '
    '
    With ReportSortimentDetail


      .Clear()
      '.Font.Name = "Microsoft Sans Serif"
      '.Font.Size = 8.25
      .ReportName = "SortimentDetails"
      .DataSource.Recordset = TabSortimentDetail
      .OnOpen = "cnt = 0"
    End With
    '
    '
    With ReportSortimentDetail.Layout

      '.Orientation = OrientationEnum.Auto
      '.Width = 7.0 * 1440    '8.5 - margins
    End With
    '
    '
    'create detail section
    '
    '
    '

    With ReportSortimentDetail.Sections(SectionTypeEnum.Detail)
      .Name = "Farbmittel"
      .Height = MerkHeight
      .Visible = True
      Fwidth = NamWidth
      Fleft = 20
      f = .Fields.Add("Farbname", TabSortimentDetail.Columns("FARBM_NAME").ColumnName, Fleft, 100.0, NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.LeftMiddle
      '.OnPrint = "Farbname.FORECOLOR=rgb(255,0,0)" & vbCrLf & _
      '           "Farbmittel.BackColor = IIf(_cnt Mod 2 = 0, RGB(255, 255, 255), RGB(240, 240, 240))" & vbCrLf & _
      '           "cnt=CnT +1"
      .OnPrint = "Farbmittel.BackColor = IIf(_cnt Mod 2 = 0, RGB(255, 255, 255), RGB(240, 240, 240))" & vbCrLf & _
                 "cnt=cnt +1"
      '
      '
      Fleft = Fleft + NamWidth
      f = .Fields.Add(TabSortimentDetail.Columns("FARBM_MENGE").ColumnName, TabSortimentDetail.Columns("FARBM_MENGE").ColumnName, Fleft, 100.0, 0.5 * NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.RightMiddle
      f.Format = "###.00"
      '
      '
      '
      '
      Fleft = Fleft + 0.5 * NamWidth
      f = .Fields.Add(TabSortimentDetail.Columns("FARBM_OPERAT").ColumnName, TabSortimentDetail.Columns("FARBM_OPERAT").ColumnName, Fleft, 100.0, 0.5 * NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.RightMiddle
      '
      '
      '
      '
      '
      Fleft = Fleft + 0.5 * NamWidth
      f = .Fields.Add(TabSortimentDetail.Columns("FARBM_LIMMNG").ColumnName, TabSortimentDetail.Columns("FARBM_LIMMNG").ColumnName, Fleft, 100.0, 0.5 * NamWidth, FHeight)
      f.Calculated = True
      f.Visible = True
      f.Font.Size = 12.0
      f.Align = FieldAlignEnum.RightMiddle
      f.Format = "###.00"
      '


    End With
    '
    '
    'MasterReport
    '
    '
    'prevent reentrant calls
    '
    If ReportSortimentMaster.IsBusy Then Beep() : Exit Sub
    '
    'Initialize layout (Master)
    '
    '


    '
    '
    'initialize control
    '
    '
    With ReportSortimentMaster
      .Clear()
      .Font.Name = "Microsoft Sans Serif"
      .Font.Size = 8.25
      .ReportName = "FarbMaster"
      .DataSource.Recordset = TabSortimentMaster
    End With
    '
    '
    '
    '
    '
    With ReportSortimentMaster.Layout
      '
      '
      '1 Inch=1440 twips
      '
      '
      .Orientation = OrientationEnum.Portrait
      .MarginLeft = Margin
      .MarginTop = Margin
      .MarginRight = Margin
      .MarginBottom = Margin
      .Width = ReportSortimentMaster.C1Document.PageLayout.PageSettings.Width.ConvertUnit(C1.C1Preview.UnitTypeEnum.Twip) - .MarginLeft - .MarginRight
      WholeWidth = .Width
    End With

    '
    '
    '

    'Create a reportheader
    '
    '
    '
    With ReportSortimentMaster.Sections(SectionTypeEnum.Header)

      .Height = 50
      .Visible = True
      '
      'Linie (horizontal)
      '
      f = .Fields.Add("LINIEH1", "", 0, 0, WholeWidth, 10)
      f.LineSlant = LineSlantEnum.NoSlant
      f.LineWidth = 5
      f.BorderStyle = BorderStyleEnum.Solid
      f.BorderColor = Drawing.Color.Black
      f = .Fields.Add("fldTitle", "Sortimente für " & MenueParam.Misch.Kbez, 0.15 * WholeWidth, 0.2 * MerkHeight, ReportSortimentMaster.Layout.Width, 2 * MerkHeight)
      f.Visible = True
      f.Font.Size = 14
      f.Font.Bold = True
      f.BackColor = Drawing.Color.White
      f.Align = FieldAlignEnum.LeftMiddle
      '




      f = .Fields.Add("DateTime", Now, 0, MerkHeight, WholeWidth, 2 * MerkHeight)
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
    With ReportSortimentMaster.Sections(SectionTypeEnum.PageFooter)
      .Height = 500
      .Visible = True

      '
      'Seitennummerierung
      '
      '
      f = .Fields.Add("SeitenText", "Seite", ReportSortimentMaster.Layout.Width - 1200, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITE", "Page", ReportSortimentMaster.Layout.Width - 1000, 0, 500, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEVON", "von", ReportSortimentMaster.Layout.Width - 650, 0, 500, 300)
      f.Align = FieldAlignEnum.RightTop
      f = .Fields.Add("SEITEN", "Pages", ReportSortimentMaster.Layout.Width - 500, 0, 650, 300)
      f.Calculated = True
      f.Align = FieldAlignEnum.RightTop
      '
      '
    End With
    '
    '
    '
    'Create a page header
    '
    '
    '
    With ReportSortimentMaster.Sections(SectionTypeEnum.PageHeader)
      .Height = 200
      .Visible = True


      f = .Fields.Add("Sortimentsname", "Sortimentsname", 0, 0, WholeWidth + 100, 500)
      f.Calculated = False
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      '
      '
      '
      '
      '
      f = .Fields.Add("Bemerkung", "Bemerkung", NamWidth, 0, WholeWidth + 100, 500)
      f.Calculated = False
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      '
      '
      '
      '
      'Linie (vertikal)
      '
      'f = .Fields.Add("LINIEV1", "", Fleft, 0, 0, 2 * MerkHeight)
      'f.LineSlant = LineSlantEnum.Down
      'f.LineWidth = 5
      'f.BorderStyle = BorderStyleEnum.Solid
      'f.BorderColor = Drawing.Color.Black

      'Next
      '
    End With
    '

    '
    'create groups
    '
    '
    '
    '
    grp = ReportSortimentMaster.Groups.Add("SORTI_ID", "SORTI_ID", SortEnum.Ascending)
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
      'Rezepte
      '
      '
      f = .Fields.Add(TabSortimentMaster.Columns("SORTI_NAME").ColumnName, TabSortimentMaster.Columns("SORTI_NAME").ColumnName, 0, 0, WholeWidth + 100, 500)
      f.Calculated = True
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      '
      '
      '
      '
      '
      f = .Fields.Add(TabSortimentMaster.Columns("SORTI_BEM").ColumnName, TabSortimentMaster.Columns("SORTI_BEM").ColumnName, NamWidth, 0, WholeWidth + 100, 500)
      f.Calculated = True
      f.Align = FieldAlignEnum.LeftMiddle
      f.Font.Bold = True
      f.Font.Size = 12.0
      grp.KeepTogether = KeepTogetherEnum.KeepWholeGroup


    End With
    '
    '
    '
    'Subreport (Details)
    'wird über REZEPT_ID mit Masterreport verknüpft
    '
    With ReportSortimentMaster.Sections(SectionTypeEnum.Detail)
      f = .Fields.Add("FarbWerte", """SORTI_ID="" & SORTI_ID", 0, 0, 0, 0)
      f.Subreport = ReportSortimentDetail
      f.Calculated = True
      f.CanGrow = True
      .Height = 1
      f.CanShrink = True
      .Visible = True
    End With
    ''
  End Sub
  Private Sub btnDRUCKEN_cick(sender As Object, e As System.EventArgs) Handles btnDRUCKRwert.Click, btnDruckFarbmittel.Click, btnDRUCKRezept.Click, btnDruckSortiment.Click
    Dim SqlStmt As String
    Dim StrGid As String
    Dim StrLinRez As String
    Dim WithMessg As String
    ' If BitWrt(27, MenueParam.User.Writ) Then
    WithMessg = "MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    ' Else
    ' WithMessg = "MESSG_ID=" & MenueParam.Messg.MessgID
    'End If
    Select Case sender.name
      Case "btnDRUCKRwert"
        '
        '
        '
        '
        'Tabelle aufbauen
        '
        '
        StrGid = "("
        If cboGRPRwert.SelectedValue > 0 Then
          StrGid = "RWERT_GID= " & cboGRPRwert.SelectedValue & " AND " & StrGid
        End If
        StrGid = StrGid & " RWERT_IARCH=2 OR "
        StrGid = StrGid & " RWERT_IARCH=0)"
        StrGid = " WHERE " & WithMessg & " AND " & StrGid
        If chkDATRwert.Checked Then
          StrGid = StrGid & " AND (RWERT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVONRwert.Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtBISRwert.Text))) & ")"
        End If
        If Trim(txtSQLRwert.Text) <> "" Then
          StrGid = StrGid & " AND " & StrSelct("RWERT_NAME", txtSQLRwert.Text)
        End If
        '

        SqlStmt = "SELECT RWERT_ID,RWERT_DATTIM,RWERT_NAME,RWERT_BEM,RWERT_CME FROM TBL_RWERT" & StrGid
        '
        '
        SqlStmt = SqlStmt & " ORDER BY RWERT_NAME"
        '
        '
        '
        Command.CommandText = SqlStmt
        TabRwert.Clear()
        If Not FillDatset(Adapt, TabRwert) Then
          MsgBox("error")
        End If
        For i = 0 To TabRwert.Rows.Count - 1
          If Len(TabRwert.Rows(i)("RWERT_NAME")) > 20 Then
            TabRwert.Rows(i)("RWERT_NAME") = TabRwert.Rows(i)("RWERT_NAME").substring(0, 20)
          End If
          If IsDBNull(TabRwert.Rows(i)("RWERT_BEM")) Then
            TabRwert.Rows(i)("RWERT_BEM") = ""
          End If
          If Len(TabRwert.Rows(i)("RWERT_BEM")) > 20 Then
            TabRwert.Rows(i)("RWERT_BEM") = TabRwert.Rows(i)("RWERT_BEM").substring(0, 20)
          End If

        Next i

        TabRwert.AcceptChanges()
        If TabRwert.Rows.Count = 0 Then
          MsgBox(Texxt(2959))
          Exit Sub
        End If
        '
        '
        'Reports generieren

        '
        Call RenderRwert()
        '
        'Priview anzeigen
        '
        '
        '
        ppv.Document = ReportRwertMaster.Document
        Pd.Document = ReportRwertMaster.Document
        '
        '
        '
        '
        '
      Case "btnDRUCKFarbmittel"
        '
        '
        '
        '
        'Tabelle aufbauen
        '
        '


        SqlStmt = "SELECT FARBM_ID,FARBM_NAME FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " ORDER BY FARBM_NAME"

        '
        '
        Command.CommandText = SqlStmt
        TabFarbwert.Clear()
        If Not FillDatset(Adapt, TabFarbwert) Then
          MsgBox("error")
        End If
        For i = 0 To TabFarbwert.Rows.Count - 1
          If Len(TabFarbwert.Rows(i)("FARBM_NAME")) > 64 Then
            TabFarbwert.Rows(i)("FARBM_NAME") = TabFarbwert.Rows(i)("FARBM_NAME").substring(0, 64)
          End If

        Next i

        TabFarbwert.AcceptChanges()
        If TabFarbwert.Rows.Count = 0 Then
          MsgBox(Texxt(2953))
          Exit Sub
        End If
        '
        '
        'Reports generieren

        '
        Call RenderFarbmittel()
        '
        'Priview anzeigen
        '
        '
        '
        ppv.Document = ReportFarbwertMaster.Document
        Pd.Document = ReportFarbwertMaster.Document
        '
        '
        '
        '
        '
      Case "btnDRUCKRezept"
        '
        '
        '
        'Tabelle aufbauen
        '
        '
        StrGid = "("
        If cboGRPRezept.SelectedValue > 0 Then
          StrGid = "REZEPT_GID= " & cboGRPRezept.SelectedValue & " AND " & StrGid
        End If
        StrGid = StrGid & " REZEPT_IARCH=2 OR "
        StrGid = StrGid & " REZEPT_IARCH=0)"
        StrGid = " WHERE MISCH_ID=" & MenueParam.MischID & " AND " & StrGid
        If chkDATRezept.Checked Then
          StrGid = StrGid & " AND (REZEPT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVONRezept.Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtBISRezept.Text))) & ")"
        End If
        If Trim(txtSQLRezept.Text) <> "" Then
          StrGid = StrGid & " AND " & StrSelct("REZEPT_NAME", txtSQLRezept.Text)
        End If
        '

        SqlStmt = "SELECT REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME,REZEPT_BEM FROM TBL_REZEPT" & StrGid
        '
        '
        SqlStmt = SqlStmt & " ORDER BY REZEPT_NAME"
        '
        '
        '
        Command.CommandText = SqlStmt
        TabRezeptMaster.Clear()
        If Not FillDatset(Adapt, TabRezeptMaster) Then
          MsgBox("error")
        End If
        For i = 0 To TabRezeptMaster.Rows.Count - 1
          If Len(TabRezeptMaster.Rows(i)("REZEPT_NAME")) > 20 Then
            TabRezeptMaster.Rows(i)("REZEPT_NAME") = TabRezeptMaster.Rows(i)("REZEPT_NAME").substring(0, 20)
          End If
          If IsDBNull(TabRezeptMaster.Rows(i)("REZEPT_BEM")) Then
            TabRezeptMaster.Rows(i)("REZEPT_BEM") = ""
          End If
          If Len(TabRezeptMaster.Rows(i)("REZEPT_BEM")) > 20 Then
            TabRezeptMaster.Rows(i)("REZEPT_BEM") = TabRezeptMaster.Rows(i)("REZEPT_BEM").substring(0, 20)
          End If

        Next i

        TabRezeptMaster.AcceptChanges()
        If TabRezeptMaster.Rows.Count = 0 Then
          MsgBox(Texxt(2953))
          Exit Sub
        End If
        StrLinRez = StrLin(TabRezeptMaster, "REZEPT_ID")
        '
        '
        '
        SqlStmt = "SELECT REZEPT_ID,TBL_REZEPT_FARBM.FARBM_ID AS FARBM_ID,FARBM_NAME,FARBM_MENGE,FARBM_LIMMNG,FARBM_OPERAT,FARBM_PREIS,FARBM_PROZ,FARBM_PROB,FARBM_IPOS FROM TBL_REZEPT_FARBM INNER JOIN TBL_FARBM ON (TBL_REZEPT_FARBM.FARBM_ID=TBL_FARBM.FARBM_ID) AND (TBL_REZEPT_FARBM.MISCH_ID=TBL_FARBM.MISCH_ID) WHERE TBL_FARBM.MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN " & StrLinRez
        '
        '
        Command.CommandText = SqlStmt
        TabRezeptDetail.Clear()
        If Not FillDatset(Adapt, TabRezeptDetail) Then
          MsgBox("error")
        End If
        For i = 0 To TabRezeptDetail.Rows.Count - 1
          If Len(TabRezeptDetail.Rows(i)("FARBM_NAME")) > 20 Then
            TabRezeptDetail.Rows(i)("FARBM_NAME") = TabRezeptDetail.Rows(i)("FARBM_NAME").substring(0, 20)
          End If
        Next
        TabRezeptDetail.AcceptChanges()
        '
        'Reports generieren

        '
        Call RenderRezept()
        Application.DoEvents()
        '
        'Priview anzeigen
        '
        '
        '
        ppv.Document = ReportRezeptMaster.Document
        Pd.Document = ReportRezeptMaster.Document
        '
        '
        '
        '
        '
        '
        '
      Case "btnDRUCKSortiment"
        '
        '
        '
        'Tabelle aufbauen
        '
        '
        '
        SqlStmt = "SELECT SORTI_ID,SORTI_DATTIM,SORTI_NAME,SORTI_BEM FROM TBL_SORTI WHERE MISCH_ID=" & MenueParam.MischID & " ORDER BY SORTI_NAME"
        '
        '
        '
        '
        '
        Command.CommandText = SqlStmt
        TabSortimentMaster.Clear()
        If Not FillDatset(Adapt, TabSortimentMaster) Then
          MsgBox("error")
        End If
        For i = 0 To TabSortimentMaster.Rows.Count - 1
          If Len(TabSortimentMaster.Rows(i)("SORTI_NAME")) > 20 Then
            TabSortimentMaster.Rows(i)("SORTI_NAME") = TabSortimentMaster.Rows(i)("SORTI_NAME").substring(0, 20)
          End If
          If IsDBNull(TabSortimentMaster.Rows(i)("SORTI_BEM")) Then
            TabSortimentMaster.Rows(i)("SORTI_BEM") = ""
          End If
          If Len(TabSortimentMaster.Rows(i)("SORTI_BEM")) > 20 Then
            TabSortimentMaster.Rows(i)("SORTI_BEM") = TabSortimentMaster.Rows(i)("SORTI_BEM").substring(0, 20)
          End If

        Next i

        TabSortimentMaster.AcceptChanges()
        If TabSortimentMaster.Rows.Count = 0 Then
          MsgBox(Texxt(2953))
          Exit Sub
        End If
        StrLinRez = StrLin(TabSortimentMaster, "SORTI_ID")
        '
        '
        '
        SqlStmt = "SELECT SORTI_ID,TBL_SORTI_FARBM.FARBM_ID AS FARBM_ID,FARBM_NAME,FARBM_MENGE,FARBM_LIMMNG,FARBM_OPERAT,FARBM_PREIS,FARBM_PROZ,FARBM_PROB,FARBM_IPOS FROM TBL_SORTI_FARBM INNER JOIN TBL_FARBM ON (TBL_SORTI_FARBM.FARBM_ID=TBL_FARBM.FARBM_ID) AND (TBL_SORTI_FARBM.MISCH_ID=TBL_FARBM.MISCH_ID) WHERE TBL_FARBM.MISCH_ID=" & MenueParam.MischID & " AND SORTI_ID IN " & StrLinRez
        '
        '
        Command.CommandText = SqlStmt
        TabSortimentDetail.Clear()
        If Not FillDatset(Adapt, TabSortimentDetail) Then
          MsgBox("error")
        End If
        For i = 0 To TabSortimentDetail.Rows.Count - 1
          If Len(TabSortimentDetail.Rows(i)("FARBM_NAME")) > 40 Then
            TabSortimentDetail.Rows(i)("FARBM_NAME") = TabSortimentDetail.Rows(i)("FARBM_NAME").substring(0, 40)
          End If
        Next
        TabSortimentDetail.AcceptChanges()
        '
        'Reports generieren

        '
        Call RenderSortiment()
        Application.DoEvents()
        '
        'Priview anzeigen
        '
        '
        '
        ppv.Document = ReportSortimentMaster.Document
        Pd.Document = ReportSortimentMaster.Document
    End Select
    If Pd.ShowDialog = DialogResult.OK Then
      ppv.WindowState = FormWindowState.Maximized
      '
      'Priview anzeigen
      '
      '
      ppv.ShowDialog()
    End If
  End Sub

  Private Sub chkDAT_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkDATRwert.CheckedChanged, chkDATRezept.CheckedChanged
    Select Case sender.name
      Case "CHKDATRwert"

        lblVONRwert.Visible = chkDATRwert.Checked
        lblBISRwert.Visible = chkDATRwert.Checked
        txtVONRwert.Visible = chkDATRwert.Checked
        txtBISRwert.Visible = chkDATRwert.Checked
      Case "CHKDATRezept"

        lblVONRezept.Visible = chkDATRezept.Checked
        lblBISRezept.Visible = chkDATRezept.Checked
        txtVONRezept.Visible = chkDATRezept.Checked
        txtBISRezept.Visible = chkDATRezept.Checked

    End Select
  End Sub
  

  Private Sub txtDat_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtBISRezept.Validating, txtVONRezept.Validating, txtBISRwert.Validating, txtVONRwert.Validating
    If Not IsDate(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(2500))
    End If
  End Sub

End Class