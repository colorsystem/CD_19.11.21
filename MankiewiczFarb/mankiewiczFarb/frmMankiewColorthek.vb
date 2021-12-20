Option Compare Text
Option Strict Off
Option Explicit On
Public Class frmMankiewColorthek
  Inherits System.Windows.Forms.Form
  Dim ier As Integer
  Dim RezKor As String
  Dim RezAlt As String
  Dim RzNr As String
  Dim RezKey() As String
  Dim TypID(1) As Integer
  Dim UntID(1) As Integer
  Dim SmpID(1) As Integer
  Dim RzId As Integer
  Dim Preis As Single
  Dim KeyId As String
  Dim ZuMax As Single
  '
  '
  '
  Dim GrpRwerte As RefValuesGrp
  Dim FarbWerte As ValuesGrpsAssigns
  Dim FarbWrtStd As ValuesGrpsAssigns
  Dim RezSozpt As RecipesGrp
  Dim OptGesamt As OpticalData
  Dim CalcRezept As RezeptBerechnung
  Dim Quali As QualKontrolle
  Dim Umr As RezeptUmrechnung
  Dim RezTab As DataTable
  Dim RezepteTab As DataTable
  Dim FarbTab As DataTable
  Dim FarbRezeptTab As DataTable
  Dim RwertTab As DataTable
  Dim RezepteView As DataView
  Dim PicGraphic As HandleRezGrafik
  Dim RezDruck As HandlePlottDruck
  Dim ReWrRezept As ReadWriteRezept
  Dim ReWrFarbe As ReadWriteFarbe
  Dim ReWrGrund As ReadWriteGrund
  Dim CmdRezepte As OleDbCommand
  Dim OleRezepte As OleDbDataAdapter
  '
  Dim CmdFarb As OleDbCommand
  Dim OleFarb As OleDbDataAdapter
  Dim CmdFarbRezept As OleDbCommand
  Dim OleFarbRezept As OleDbDataAdapter
  Dim CmdRwert As OleDbCommand
  Dim OleRwert As OleDbDataAdapter
  Dim HscValue As Integer
  Dim MngMaxAlt As Single
  Dim MngMinAlt As Single
  Dim ZuPrz As Single
  Dim ListID() As Integer
  '
  Dim radRwerte As List(Of RadioButton)
   '
  Dim ppv As PrintPreviewDialog




  Private Sub frmMankiewColorthek_Activated(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Activated
    Call Activat(sender, e)
  End Sub

  Private Sub frmMankiewColorthek_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
    AufbauPar.MethID = -1
    ' Call DeActivat(sender, e)
  End Sub







  Private Sub frmMankiewColorthek_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim OneGlzGrd As String
    Dim WithMessg As String
   

    FormMDI.Icon = My.Resources.verwaltung
    FormMDI.Text = MenueParam.User.Name & " - " & MenueParam.Messg.Kbez & " - " & MenueParam.Menue.MethBez
    btnDrucken.Text = Texxt(4608)
    btnPictureZurück.Text = Texxt(4617)
    btnPrevZurück.Text = Texxt(4617)
    btnRezepte.Text = Texxt(4607)
    btnSAPRezepte.Text = Texxt(4700)
    lblBIS.Text = Texxt(377)
    chkDatum.Text = Texxt(140)
    radRwerte_0.Text = Texxt(1989)
    radRwerte_1.Text = Texxt(4702)
    radRwerte_2.Text = Texxt(4707)
    lblRezept.Text = Texxt(863)
    lblSuchName.Text = Texxt(4606)
    lblDicke.Text = Texxt(832)
    lblFAQ.Text = Texxt(802)
    lblFAR.Text = Texxt(738)
    btnVOR.Text = Texxt(806)
    chkRWRT.Text = Texxt(879)
    lblGewFakt.Text = Texxt(446)
    lblNachstellung.Text = Texxt(786)

    lblDEStern.Text = "DE* (" & MenueParam.Normfa(0).NormKenn & ")"
    lblDLStern.Text = "DL* (" & MenueParam.Normfa(0).NormKenn & ")"
    lblDaStern.Text = "Da* (" & MenueParam.Normfa(0).NormKenn & ")"
    lblDbStern.Text = "Db* (" & MenueParam.Normfa(0).NormKenn & ")"
    lblDCStern.Text = "DC* (" & MenueParam.Normfa(0).NormKenn & ")"
    lblDHStern.Text = "DH* (" & MenueParam.Normfa(0).NormKenn & ")"

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


    FarbWerte = New ValuesGrpsAssigns
    FarbWrtStd = New ValuesGrpsAssigns
    GrpRwerte = New RefValuesGrp
    RezSozpt = New RecipesGrp
    CalcRezept = New RezeptBerechnung
    ReWrRezept = New ReadWriteRezept
    OptGesamt = New OpticalData
    ReWrFarbe = New ReadWriteFarbe
    ReWrGrund = New ReadWriteGrund
    RezKor = ""
    RezAlt = ""
    Umr = New RezeptUmrechnung
    Quali = New QualKontrolle

    RezAlt = "MNG"
    RezSozpt.Rezepte.AddRez(RezAlt, New Recipe)
    radRwerte = New List(Of RadioButton)
    radRwerte.Add(radRwerte_0)
    radRwerte.Add(radRwerte_1)
    radRwerte.Add(radRwerte_2)

    '
    '
    'Tabelle für Einzelrezepte
    '
    '
    RezTab = New DataTable
    RezTab.Columns.Add("FAID", GetType(Integer))
    RezTab.Columns.Add("FNAME", GetType(String))
    RezTab.Columns.Add("GEWICHT", GetType(Single))
    RezTab.Columns.Add("FST", GetType(Single))
    RezTab.Columns.Add("PROZ", GetType(Single))
    RezTab.Columns.Add("PROB", GetType(Single))

    TDBRezept.DataSource = RezTab
    TDBRezept.Columns(1).HeaderText = Texxt(394)
    TDBRezept.Columns(2).HeaderText = Texxt(4630)
    TDBRezept.Columns(3).HeaderText = TexKt(21014)
    TDBRezept.Columns(4).HeaderText = Texxt(820)
    TDBRezept.Columns(5).HeaderText = Texxt(821)

    Application.DoEvents()
    '
    '
    SplitContainPreview.Hide()
    SplitContainPicture.Hide()
    SplitContainRezSuch.Show()
    '
    '
    '
    '
    '
    PicGraphic = New HandleRezGrafik
    RezDruck = New HandlePlottDruck
    RezDruck.printset = pd.PrinterSettings

    RezDruck.DrPar = False
    RezDruck.JProz = True
    RezDruck.JProb = False
    RezDruck.JStae = True
    RezDruck.JSpez = False

    RezDruck.AllRezepte = RezSozpt
    RezDruck.GrpRwerte = GrpRwerte
    RezDruck.Winkel = MenueParam.User.Winkel
    RezDruck.Plott = PicGraphic
    ppv = New PrintPreviewDialog
    PicGraphic.GrpRwerte = GrpRwerte
    PicGraphic.AllRezepte = RezSozpt
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
    '
    'Tabelle für alle Rezepte (gemäß Suchbegriff)
    '
    '
    '
    '
    '
    CmdRezepte = New OleDbCommand
    OleRezepte = New OleDbDataAdapter
    OleRezepte.SelectCommand = CmdRezepte
    CmdRezepte.Connection = Cndat()
    '
    '
    'Tabelle für Rezepte
    '
    '
    '
    RezepteTab = New DataTable
    RezepteView = New DataView(RezepteTab)
    '
    '
    'Tabelle für Rezepte mit Farbmitteln
    '
    '
    CmdFarbRezept = New OleDbCommand("", Cndat)
    OleFarbRezept = New OleDbDataAdapter
    OleFarbRezept.SelectCommand = CmdFarbRezept
    FarbRezeptTab = New DataTable
    '
    '
    'Tabelle Rwerte
    '
    '
    OleRwert = New OleDbDataAdapter
    CmdRwert = New OleDbCommand("", Cndat)
    OleRwert.SelectCommand = CmdRwert
    RwertTab = New DataTable
    '
    '
    '
    'Tabelle Farbmittel
    '
    '
    OleFarb = New OleDbDataAdapter
    CmdFarb = New OleDbCommand("", Cndat)
    OleFarb.SelectCommand = CmdFarb
    '
    OneGlzGrd = " AND TBL_FARBM.FARBM_ID=GLZGRD_ID"
    WithMessg = "TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID

    FarbTab = New DataTable
    CmdFarb.CommandText = "SELECT * FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID & OneGlzGrd & " ORDER BY FARBM_NAME"
    If Not FillDatset(OleFarb, FarbTab) Then
      Exit Sub
    End If
    FarbTab.AcceptChanges()
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
    '
    '
    Call Activat(sender, e)
    '
    ''
  End Sub
  Sub Activat(ByVal sender As Object, ByVal e As System.EventArgs)
    If IsNothing(RezSozpt) Then Exit Sub

    Cursor = Cursors.WaitCursor
    AufbauPar.MethID = 55
    'Measure.Umspeich(MenueParam.Messg, MenueParam.Normfa(0))
    '
    '
    'Unvisible
    '
    '
    TDBRezept.Visible = False
    btnDrucken.Visible = False
    lblRezept.Visible = False
    RezTab.Rows.Clear()

    '
    '
    '
    '
    GrpRwerte.clear()
    '
    If MenueParam.Misch.Vert = 0 Then
      GrpRwerte.Add("W", New RefValues)
      GrpRwerte.Add("S", New RefValues)
    Else
      GrpRwerte.Add("S", New RefValues)
      GrpRwerte.Add("W", New RefValues)
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
    txtBIS.Text = Date.Today.ToLocalTime
    txtVon.Text = Date.Today.AddDays(-MenueParam.Messg.Tdiff).ToLocalTime
    '
    '
    '
    '
    SplitContainPreview.Hide()
    SplitContainPicture.Hide()
    SplitContainRezSuch.Show()
    '
    '
    '
    '

    Call MakeVisible()
    '
    '
    '
    '
    btnRezepte.PerformClick()
    Cursor = Cursors.Arrow
    'Application.DoEvents()      '
  End Sub



  Private Sub btnRezepte_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRezepte.Click
    Dim StrRezepte As String
    Dim RB() As Single
    Dim RP() As Single
    Dim Rhilf() As Single
    Dim NWE As Integer
    Dim DE As Single
    Dim DL As Single
    Dim DC As Single
    Dim DH As Single
    Dim DA As Single
    Dim DB As Single
    RezepteTab.Columns.Clear()
    RezepteTab.Clear()
    RezepteView.Sort = ""
    RezepteView.RowFilter = ""
    RezTab.Rows.Clear()
    If radRwerte(1).Checked Then
      '
      'Mit Vorlagen
      '
      CmdRezepte.CommandText = "SELECT TOP " & MenueParam.Messg.Top & " TBL_REZEPT.REZEPT_ID AS REZEPT_ID,REZEPT_NAME FROM TBL_REZEPT " _
      & " INNER JOIN TBL_REZEPT_RWERT ON (TBL_REZEPT.REZEPT_ID=TBL_REZEPT_RWERT.REZEPT_ID AND TBL_REZEPT.MISCH_ID=TBL_REZEPT_RWERT.MISCH_ID)" _
      & " WHERE TBL_REZEPT.MISCH_ID=" & MenueParam.MischID & " AND TBL_REZEPT_RWERT.MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_KWB=" & 11
    ElseIf radRwerte(2).Checked Then
      '
      'Mit Nachstellungen
      '
      CmdRezepte.CommandText = "SELECT TOP " & MenueParam.Messg.Top & " TBL_REZEPT.REZEPT_ID AS REZEPT_ID,REZEPT_NAME FROM TBL_REZEPT " _
      & " INNER JOIN TBL_REZEPT_RWERT ON (TBL_REZEPT.REZEPT_ID=TBL_REZEPT_RWERT.REZEPT_ID AND TBL_REZEPT.MISCH_ID=TBL_REZEPT_RWERT.MISCH_ID)" _
      & " WHERE TBL_REZEPT.MISCH_ID=" & MenueParam.MischID & " AND TBL_REZEPT_RWERT.MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_KWB=" & 1

    Else
      CmdRezepte.CommandText = "SELECT TOP " & MenueParam.Messg.Top & " REZEPT_ID,REZEPT_NAME FROM TBL_REZEPT WHERE MISCH_ID=" & MenueParam.MischID
    End If
    If chkDatum.CheckState = CheckState.Checked Then
      CmdRezepte.CommandText = CmdRezepte.CommandText & " AND REZEPT_DATTIM BETWEEN ? AND ?"
    End If
    If txtSuchName.Text <> "" Then
      CmdRezepte.CommandText = CmdRezepte.CommandText & " AND " & StrSelct("REZEPT_NAME", AddHkomE(txtSuchName.Text))
    End If
    If MenueParam.Misch.UserRzpGID > 0 Then
      CmdRezepte.CommandText = CmdRezepte.CommandText & " AND REZEPT_GID=" & MenueParam.Misch.UserRzpGID
    End If
    CmdRezepte.CommandText = CmdRezepte.CommandText & " ORDER BY REZEPT_DATTIM DESC"
    CmdRezepte.Parameters.Clear()
    CmdRezepte.Parameters.Add(New OleDbParameter("DATVON", OleDbType.Date))
    CmdRezepte.Parameters.Add(New OleDbParameter("DATBIS", OleDbType.Date))
    CmdRezepte.Parameters("DATVON").Value = Date.Parse(txtVon.Text)
    CmdRezepte.Parameters("DATBIS").Value = Date.Parse(txtBIS.Text).AddDays(1.0)
    OleRezepte.SelectCommand = CmdRezepte
    If Not FillDatset(OleRezepte, RezepteTab) Then
      Exit Sub
    End If
    RezepteTab.AcceptChanges()
    lstRezepte.DataSource = RezepteView
    lstRezepte.DisplayMember = "REZEPT_NAME"
    lstRezepte.ValueMember = "REZEPT_ID"
    StrRezepte = StrLin(RezepteTab, "REZEPT_ID")
    '
    '
    'Zugehörige Farbmittel
    '
    '
    CmdFarbRezept.CommandText = "SELECT REZEPT_ID,FARBM_ID FROM TBL_REZEPT_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN " & StrRezepte
    If Not FillDatset(OleFarbRezept, FarbRezeptTab) Then
      Exit Sub
    End If
    FarbRezeptTab.AcceptChanges()
    If FarbRezeptTab.Rows.Count = 0 Then
      MsgBox(Texxt(2953))
      TDBRezept.Visible = False
      Exit Sub
    Else
      TDBRezept.Visible = True
    End If
    ReDim ListID(lstFAR.Items.Count - 1)
    For i = 0 To lstFAR.Items.Count - 1
      ListID(i) = lstFAR.Items(i).id
    Next


    Call NewRezeptTab(cboFAR.SelectedIndex, ListID, RezepteTab, FarbRezeptTab)
    RezepteTab.AcceptChanges()
    TDBRezept.Visible = False
    lblRezept.Visible = False
    '
    '
    txtDE.Text = ""
    txtDL.Text = ""
    txtDC.Text = ""
    txtDH.Text = ""
    txtDa.Text = ""
    txtDb.Text = ""
    '
    '
    If radRwerte(2).Checked And chkRWRT.Checked And GrpRwerte(0)("V").IVoNa Then
      '
      '
      'R-Werte einlesen
      '
      StrRezepte = StrLin(RezepteTab, "REZEPT_ID")
      '
      CmdRwert.CommandText = "SELECT * FROM TBL_RWERT INNER JOIN TBL_REZEPT_RWERT ON TBL_RWERT.RWERT_ID=TBL_REZEPT_RWERT.RWERT_ID" _
      & " WHERE TBL_RWERT.MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND MISCH_ID=" & MenueParam.MischID & " AND RWERT_KWB=1 AND REZEPT_ID IN " & StrRezepte
      If Not FillDatset(OleRwert, RwertTab) Then
        Exit Sub
      End If
      RwertTab.AcceptChanges()
      '
      '
      '
      '
      'Spalten hinzufügen
      '
      '
      RezepteTab.Columns.Add("DEMOD", GetType(Single))
      RezepteTab.Columns.Add("DE", GetType(Single))
      RezepteTab.Columns.Add("DL", GetType(Single))
      RezepteTab.Columns.Add("DC", GetType(Single))
      RezepteTab.Columns.Add("DH", GetType(Single))
      RezepteTab.Columns.Add("DA", GetType(Single))
      RezepteTab.Columns.Add("DB", GetType(Single))
      '
      '
      'Farbabstände berechnen
      '
      '
      NWE = MenueParam.Messg.Winkel.Wsol.Nwe
      RB = GrpRwerte(0)("V").RefKurv(0).R
      ReDim RP(NWE - 1)
      For i = 0 To RwertTab.Rows.Count - 1
        Rhilf = GetSingles(RwertTab.Rows(i)("RWERT_RWERT"))
        Array.Copy(Rhilf, 0, RP, 0, NWE) '
        Call Quali.FarbDifferenzRef(0, 0, 0, MenueParam.User.Winkel, RB, RP, DE, DL, DC, DH, DA, DB, ier)
        RezepteView.RowFilter = "REZEPT_ID=" & RwertTab.Rows(i)("REZEPT_ID")
        If RezepteView.Count > 0 Then
          RezepteView(0)("DEMOD") = Sqrt((txtGewDL.Text * DL) ^ 2 + (txtGewDC.Text * DC) ^ 2 + (txtGewDH.Text * DH) ^ 2)
          RezepteView(0)("DE") = DE
          RezepteView(0)("DL") = DL
          RezepteView(0)("DC") = DC
          RezepteView(0)("DH") = DH
          RezepteView(0)("DA") = DA
          RezepteView(0)("DB") = DB
        End If
      Next i
      RezepteTab.AcceptChanges()
      RezepteView.Sort = "DEMOD"
      RezepteView.RowFilter = ""

    End If
    If lstRezepte.Items.Count > 0 Then
      Call lstRezepte_Click(sender, e)
    End If
  End Sub

  Private Sub lstRezepte_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstRezepte.Click
    Dim i As Integer
    Dim ier As Integer
    Dim Preis As Single
    Dim Keyid As String

    Cursor = Cursors.WaitCursor
    '
    '
    'Sortiment einlesen
    '
    '
    '
    '
    RzId = lstRezepte.SelectedItem("REZEPT_ID")
    For i = 0 To 1
      TypID(i) = -1
      UntID(i) = -1
      SmpID(i) = -1
    Next i
    If GrpRwerte.RwArt(0) = "W" Then
      RezSozpt.Rezepte.kwb(0) = 0
      RezSozpt.Rezepte.kwb(1) = 1
    Else
      RezSozpt.Rezepte.kwb(0) = 1
      RezSozpt.Rezepte.kwb(1) = 0
    End If
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
    ReDim RezKey(0)
    RezKey(0) = RezAlt
    Umr.CalcBamng(RezAlt, RezSozpt, ier)
    lblRezept.Visible = True
    lblRezept.Text = RezSozpt.Rezepte(RezAlt).Name

    
    '


    'Application.DoEvents()
    '
    '
    'Plausibilitätsprüfungen für Rezepte
    '
    '
    '
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

    '
    '
    '
    'Aktuelles Rezept in Grid
    '
    Call GridKorrektur(RezSozpt)
    '
    '
    If RezepteTab.Columns.Contains("DEMOD") Then
      txtDE.Text = Format(lstRezepte.SelectedItem("DE"), "##0.00")
      txtDL.Text = Format(lstRezepte.SelectedItem("DL"), "##0.00")
      txtDC.Text = Format(lstRezepte.SelectedItem("DC"), "##0.00")
      txtDH.Text = Format(lstRezepte.SelectedItem("DH"), "##0.00")
      txtDa.Text = Format(lstRezepte.SelectedItem("DA"), "##0.00")
      txtDb.Text = Format(lstRezepte.SelectedItem("DB"), "##0.00")
    End If

    TDBRezept.AllowUserToAddRows = True
    TDBRezept.ReadOnly = True
    'MsgBox(hscRezepte.Maximum)
    '
    TDBRezept.Visible = True
    btnDrucken.Visible = True
    lblRezept.Visible = True
    '
    '
    '
    '
    '

    If GrpRwerte.RwArt(0) = "W" Then
      GrpRwerte(0)("N").kwb = 0
      GrpRwerte(1)("N").kwb = 1
      GrpRwerte(0)("R").kwb = 0
      GrpRwerte(1)("R").kwb = 1
      GrpRwerte(0).RefUnt.kwb = 0
      GrpRwerte(1).RefUnt.kwb = 1
    Else
      GrpRwerte(0)("N").kwb = 1
      GrpRwerte(1)("N").kwb = 0
      GrpRwerte(0)("R").kwb = 1
      GrpRwerte(1)("R").kwb = 0
      GrpRwerte(0).RefUnt.kwb = 1
      GrpRwerte(1).RefUnt.kwb = 0
    End If
    For i = 0 To 1
      GrpRwerte(i).RefUnt.Name = ""
      GrpRwerte(0)(i).Name = ""
      GrpRwerte(1)(i).Name = ""
    Next i
    PicGraphic.Winkel = MenueParam.User.Winkel
    PicGraphic.WeSc(0) = True
    PicGraphic.WeSc(1) = False
    PicGraphic.Knlz = 0
    PicGraphic.Kwop = 0
    PicGraphic.kwopt(0) = True
    PicGraphic.kwopt(1) = False
   
    PicGraphic.Rmin = 0
    PicGraphic.Rmax = 100


    '
    'Picgraphic mit aktuellen Daten versorgen
    '
    '
    For i = 0 To GrpRwerte.Count - 1
      GrpRwerte(i)("N").IVoNa = False
      GrpRwerte(i)("R").IVoNa = False
    Next i
    GrpRwerte(0).RefUnt.ID = UntID(0)
    GrpRwerte(1).RefUnt.ID = UntID(1)
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
    '
    '
    '
    'Einlesen R-Werte (Vorlage) (nur wenn keine Vergleichsvorlage vorhanden)
    '
    '
    '
    If Not chkRWRT.Checked And Not radRwerte_2.Checked And Not GrpRwerte(0)("V").IVoNa Then
      If GrpRwerte.RwArt(0) = "W" Then
        GrpRwerte(0)("V").kwb = 0
        GrpRwerte(1)("V").kwb = 1
      Else
        GrpRwerte(0)("V").kwb = 1
        GrpRwerte(1)("V").kwb = 0
      End If
      For i = 0 To GrpRwerte.Count - 1
        GrpRwerte(i)("V").IVoNa = False
      Next i
      GrpRwerte(0)("V").ID = TypID(0)
      GrpRwerte(1)("V").ID = TypID(1)
      For i = 0 To GrpRwerte.Count - 1
        GrpRwerte(i)("V").IVoNa = False
        If GrpRwerte(i)("V").ID >= 0 Then
          ReWrRwert.ReadRwert(GrpRwerte(i)("V").ID, GrpRwerte(i)("V"), ier)
        End If
      Next i
    End If
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


    ''
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
    Call MakeVisible()
    If radRwerte(2).Checked Then
      txtNachstellung.Visible = True
      lblNachstellung.Visible = True
      txtNachstellung.Text = GrpRwerte(0)("N").Name
    Else
      txtNachstellung.Visible = False
      lblNachstellung.Visible = False
    End If
    Cursor = Cursors.Arrow



  End Sub
  Sub MakeVisible()

    If GrpRwerte(0)("V").IVoNa And GrpRwerte(0)("N").IVoNa Then
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
      '
      '
      '
      '
      'Farbmetrik 
      '
      '
      '
      PicGraphic.RzName(0) = "MNG"

    End If
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
      RezTab.Rows(j)(1) = RezSozpt.Farben(KeyId).Name
      RezTab.Rows(j)(2) = Format(RezSozpt.Rezepte("MNG")(j).BaAmng, RezSozpt.Farben(KeyId).Form)
      RezTab.Rows(j)(3) = Format(RezSozpt.Rezepte("MNG")(j).Proz, "###.00")
      RezTab.Rows(j)(4) = Format(RezSozpt.Rezepte("MNG")(j).Prob, "###.00")
      SumA = SumA + RezSozpt.Rezepte("MNG")(j).BaAmng
    Next
    RezTab.AcceptChanges()

    RezTab.Rows.Add(RezTab.NewRow)
    RezTab.Rows(RezSozpt.Rezepte("MNG").KF)(1) = Texxt(825)
    RezTab.Rows(RezSozpt.Rezepte("MNG").KF)(2) = Format(SumA, "####0.00")

    '
    '
    '
    '
    '
    For j = 0 To RezSozpt.Rezepte("MNG").KF - 1
      TDBRezept.Rows(j).Height = Height
    Next
    TDBRezept.AllowUserToAddRows = False
    TDBRezept.AllowUserToResizeColumns = False
    TDBRezept.AllowUserToOrderColumns = False
    TDBRezept.AllowUserToDeleteRows = False
    TDBRezept.ReadOnly = True
    TDBRezept.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells
    TDBRezept.ScrollBars = ScrollBars.None
    TDBRezept.BorderStyle = BorderStyle.None
    TDBRezept.RowHeadersVisible = False
    TDBRezept.Columns(0).Visible = False
    TDBRezept.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.AllowUserToAddRows = False
    TDBRezept.AllowUserToResizeColumns = False
    TDBRezept.Columns(1).HeaderCell.Style.Alignment = TDBRezept.Columns(0).DefaultCellStyle.Alignment
    TDBRezept.Columns(2).HeaderCell.Style.Alignment = TDBRezept.Columns(1).DefaultCellStyle.Alignment
    TDBRezept.Columns(3).HeaderCell.Style.Alignment = TDBRezept.Columns(1).DefaultCellStyle.Alignment
    TDBRezept.Columns(4).HeaderCell.Style.Alignment = TDBRezept.Columns(1).DefaultCellStyle.Alignment
    TDBRezept.Columns(5).HeaderCell.Style.Alignment = TDBRezept.Columns(1).DefaultCellStyle.Alignment

    TDBRezept.Columns(1).SortMode = SortOrder.None
    TDBRezept.Columns(2).SortMode = SortOrder.None
    TDBRezept.Columns(3).SortMode = SortOrder.None
    TDBRezept.Columns(4).SortMode = SortOrder.None
    TDBRezept.Columns(5).SortMode = SortOrder.None
    TDBRezept.Columns(5).Visible = False
    '
    '
    TDBRezept.Columns(2).DefaultCellStyle.Format = "####.000"
    TDBRezept.Columns(3).DefaultCellStyle.Format = "###.00"
    TDBRezept.Columns(4).DefaultCellStyle.Format = "###.00"
    TDBRezept.Columns(5).DefaultCellStyle.Format = "###.00"

    '
    TDBRezept.Rows(RezSozpt.Rezepte("MNG").KF).Cells(2).Style.Font = New Font("ARIAL", 10.0, FontStyle.Bold)
    TDBRezept.Rows(RezSozpt.Rezepte("MNG").KF).Cells(1).Style.Font = New Font("ARIAL", 10.0, FontStyle.Bold)

    TDBRezept.Rows(RezSozpt.Rezepte("MNG").KF).Height = Height + 6
    TDBRezept.Height = (2 + RezSozpt.Rezepte("MNG").KF) * Height + 4
    TDBRezept.Rows(RezSozpt.Rezepte("MNG").KF).DefaultCellStyle.BackColor = Color.LightGray




    TDBRezept.Columns(1).Width = TDBRezept.Width * 0.6
    TDBRezept.Columns(2).Width = TDBRezept.Width * 0.3
    TDBRezept.Columns(2).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
    TDBRezept.Columns(3).Width = 75
    TDBRezept.Columns(4).Width = 75
    TDBRezept.Columns(5).Width = 75
    Application.DoEvents()
  End Sub



  Private Sub btnPictureZurück_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnPictureZurück.Click, btnPrevZurück.Click
    SplitContainPreview.Hide()
    SplitContainPicture.Hide()
    SplitContainRezSuch.Show()
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

  Private Sub btnDrucken_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnDrucken.Click
    SplitContainRezSuch.Hide()
    SplitContainPicture.Hide()
    PrintPrev.Show()



    Dim Printdoc As New PrintDocument
    Printdoc.PrinterSettings = pd.PrinterSettings
    ppv.Document = Printdoc
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

    '
    '
    '
    RezAusgabe.pldr = RezDruck
    AddHandler Printdoc.PrintPage, AddressOf RezAusgabe.MankDruckWiegeschein
    Printdoc.DefaultPageSettings.Landscape = False
    ppv.WindowState = FormWindowState.Maximized
    'Preview anzeigen      
    ppv.ShowDialog()
    RemoveHandler Printdoc.PrintPage, AddressOf RezAusgabe.MankDruckWiegeschein
    Printdoc.Dispose()
    SplitContainRezSuch.Show()
  End Sub

  Private Sub TDBRezept_FootClick(ByVal sender As Object, ByVal e As EventArgs)
    Dim X As Single
    Dim Y As Single
    Dim Wi As Single
    'If TDBRezept.Col <> 1 Then Exit Sub
    txtMenge.Visible = True
    txtMenge.Text = RezTab.Rows(RezSozpt.Rezepte("MNG").KF)(1)
    txtMenge.Focus()
    X = TDBRezept.Location.X
    Y = TDBRezept.Location.Y
    Wi = TDBRezept.RowHeadersWidth + TDBRezept.Columns(0).Width
    'Wi = TDBRezept.Splits(0, 0).RecordSelectorWidth + TDBRezept.Splits(0, 0).DisplayColumns(0).Width
    X = X + Wi + 3
    Y = Y - 4 + TDBRezept.Height - TDBRezept.Rows(RezSozpt.Rezepte("MNG").KF).Height
    'TDBRezept.LeftCol = 0
    txtMenge.SetBounds(X, Y, TDBRezept.Columns(1).Width, TDBRezept.Rows(RezSozpt.Rezepte("MNG").KF).Height)
  End Sub
  Private Sub txtMenge_Keypress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles txtMenge.KeyPress
    Dim ivol As Integer
    Dim NewMenge As Single
    If Asc(e.KeyChar) = 13 Then
      NewMenge = Singl(txtMenge.Text)
      Call RezeptNewMenge(ivol, NewMenge, Umr, RezKey, RezSozpt, ier)
      Call GridKorrektur(RezSozpt)
      txtMenge.Visible = False
    End If
  End Sub


  Private Sub txtMenge_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtMenge.Leave
    Dim ee As New System.Windows.Forms.KeyPressEventArgs(Chr(13))
    Call txtMenge_Keypress(sender, ee)
  End Sub

  Private Sub TDBRezept_CellEndEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles TDBRezept.CellEndEdit
    Dim NewMenge As Single
    Dim ivol As Integer
    If e.ColumnIndex = 3 And e.RowIndex = RezSozpt.Rezepte("MNG").KF Then
      NewMenge = RezTab.Rows(e.RowIndex)("GEWICHT")
      ivol = RezSozpt.IVOL
      Call RezeptNewMenge(ivol, NewMenge, Umr, RezKey, RezSozpt, ier)
      txtMenge.Text = NewMenge
      Call Umr.CalcFamng("MNG", RezSozpt, ier)
      Call GridKorrektur(RezSozpt)
    End If
  End Sub

  Private Sub TDBRezept_CellEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles TDBRezept.CellEnter
    If e.ColumnIndex = 3 And RezTab.Rows.Count > 0 Then
      If e.RowIndex = RezSozpt.Rezepte("MNG").KF Then
        TDBRezept.ReadOnly = False
      End If
    End If
  End Sub

  Private Sub TDBRezept_CellLeave(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles TDBRezept.CellLeave
    TDBRezept.ReadOnly = True
  End Sub



  Private Sub TDBRezept_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles TDBRezept.DataError
    If e.Cancel Then
      e.Cancel = False
    End If
  End Sub

  Private Sub TDBRezept_CellFormatting(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellFormattingEventArgs) Handles TDBRezept.CellFormatting
    If e.ColumnIndex = 3 And e.RowIndex >= 0 And e.RowIndex < RezSozpt.Rezepte("MNG").KF Then
      If Not IsDBNull(e.Value) Then
        e.Value = Format(e.Value, RezSozpt.Farben(KeyName(RezSozpt.Rezepte("MNG")(e.RowIndex).ID)).Form)
      End If
    End If
  End Sub

  Private Sub frmMankiewColorthek_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize, Me.Shown, Me.Activated
    Call ResizeChild(Me)

  End Sub

  Private Sub btnSAPRezepte_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnSAPRezepte.Click
    Dim StringLine As String
    Dim LU As Integer = 8
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim PRNR As String
    Dim RzKey As String
    Dim KeyID As String
    Dim FaNr As String
    Dim ID As Integer
    Dim FarbTab As DataTable
    Dim FarbSelCommand As New OleDbCommand("", Cndat)
    Dim FarbAdapter As New OleDbDataAdapter
    Dim StripString() As String
    Dim RezeptGruppe As New RecipesGrp
    Dim fileReader As System.IO.StreamReader
    UntID(0) = -1
    UntID(1) = -1
    TypID(0) = -1
    TypID(1) = -1
    SmpID(0) = -1
    SmpID(1) = -1
    btnSAPRezepte.Enabled = False
    btnRezepte.Enabled = False
    lblRezName.Visible = True
    lstRezepte.Enabled = False
    Cursor = Cursors.WaitCursor
    '
    'RezeptGruppe.Farben mit Farben des Mischsysztem füllen
    '
    '
    '
    FarbSelCommand.CommandText = "SELECT FARBM_ID,FARBM_PRNR FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID
    FarbAdapter.SelectCommand = FarbSelCommand
    FarbTab = New DataTable
    If Not FillDatset(FarbAdapter, FarbTab) Then
      GoTo sapend
    End If
    FarbTab.AcceptChanges()
    '
    'Umspeichern nach RezeptGruppe.Farben
    RezeptGruppe.Farben.clear()
    For i = 0 To FarbTab.Rows.Count - 1
      ID = KeyName(FarbTab.Rows(i)("FARBM_ID"))
      KeyID = KeyName(ID)
      RezeptGruppe.Farben.AddFarb(KeyID, New Colorant)
      RezeptGruppe.Farben(KeyID).ID = ID
      RezeptGruppe.Farben(KeyID).PrNr = FarbTab.Rows(i)("FARBM_PRNR")
    Next

    Try
      OpenFileDialogSAPRezepte.FileName = ""
      OpenFileDialogSAPRezepte.CheckFileExists = False
      If OpenFileDialogSAPRezepte.ShowDialog() = DialogResult.OK Then
      Else
        GoTo sapend
      End If
    Catch
      MsgBox("Error" & " OPENDialog")
    End Try
    '
    fileReader = My.Computer.FileSystem.OpenTextFileReader(OpenFileDialogSAPRezepte.FileName)
    RezeptGruppe.Rezepte.clear()
    RezeptGruppe.Rezepte.kwb(0) = 0
    RezeptGruppe.Rezepte.kwb(1) = 1
    i = -1
    StringLine = fileReader.ReadLine()
    StripString = StringLine.Split(";")
    If StripString(0) <> """RezeptID""" Or StripString(13) <> """ANLAGEDATUM""" Then
      '
      MsgBox("Falsche Datei")
      GoTo sapend
    End If
    Do
      Try

        StringLine = fileReader.ReadLine()
        '
        If IsNothing(StringLine) Then Exit Do
        StripString = StringLine.Split(";")
        '
        '0 und 10
        If StripString(3) = """10""" Or StripString(5) = "0" Then
          '
          If StripString(0).Length < 10 Then
            MsgBox("Wrong recipe name: " & StripString(0))
            MsgBox(StringLine)
            Continue Do
          End If

          'Neues Rezept
          '
          '
          i = i + 1
          RezeptGruppe.Rezepte.AddRez(KeyRe(i), New Recipe)
          RezeptGruppe.Rezepte(KeyRe(i)).Name = StripString(0).Replace("""", "").Substring(0, 13) & Space(1) & StripString(9).Replace("""", "")
          RezeptGruppe.Rezepte(KeyRe(i)).Bem = " "
          RezeptGruppe.Rezepte(KeyRe(i)).DatTim = StripString(13)
          RezeptGruppe.Rezepte(KeyRe(i)).Dicke(0) = CSng(txtDicke.Text)
          RezeptGruppe.Rezepte(KeyRe(i)).Dicke(1) = CSng(txtDicke.Text)
          RezeptGruppe.Rezepte(KeyRe(i)).Gid = MenueParam.Misch.UserRzpGID
          RezeptGruppe.Rezepte(KeyRe(i)).Iarch = 0
        Else
          '
          If RezeptGruppe.Rezepte.ContainsKey(KeyRe(i)) Then
            '
            If StripString(6).Length = 0 Then
              MsgBox("Wrong colorant name: " & StripString(6))
              MsgBox(StringLine)
              Continue Do
            End If
            '
            'FarbmittelID mit Hilfe der Produktnummer und RezeptGruppe.Farben ermitteln
            '
            PRNR = StripString(6).Replace("""", "")
            If PRNR.Length > 10 Then
              PRNR = PRNR.Substring(0, 13)
            End If
            For j = 0 To RezeptGruppe.Farben.FarbCount - 1
              If RezeptGruppe.Farben(j).PrNr = PRNR Then
                Exit For
              End If
            Next j
            If j = RezeptGruppe.Farben.FarbCount Then
              '
              'Produktnummer nicht vorhanden
              MsgBox(Texxt(917) & ": " & PRNR & " " & Texxt(2984))
              MsgBox(StringLine)
              'Rezept löschen
              RezeptGruppe.Rezepte.RemoveRez(KeyRe(i))
              Continue Do
            Else

              '
              '
              'Prüfen, ob Farbmittel bereits vorhanden
              '
              For k = 0 To RezeptGruppe.Rezepte(KeyRe(i)).KF - 1
                FaNr = KeyRe(k)
                If RezeptGruppe.Farben(j).ID = RezeptGruppe.Rezepte(KeyRe(i))(FaNr).ID Then
                  MsgBox(Texxt(917) & ": " & PRNR & Space(1) & Texxt(3051))
                  MsgBox(StringLine)
                  RezeptGruppe.Rezepte.RemoveRez(KeyRe(i))
                  Continue Do
                End If
              Next
              '
              'Farbmittel übernehmen 
              '
              '
              FaNr = KeyRe(RezeptGruppe.Rezepte(KeyRe(i)).KF)
              RezeptGruppe.Rezepte(KeyRe(i)).AddFaNr(FaNr, New ColorAmount)
              RezeptGruppe.Rezepte(KeyRe(i))(FaNr).ID = RezeptGruppe.Farben(j).ID
              RezeptGruppe.Rezepte(KeyRe(i))(FaNr).BaAmng = CSng(StripString(11))
              RezeptGruppe.Rezepte(KeyRe(i))(FaNr).FaAmng = CSng(StripString(11))
              RezeptGruppe.Rezepte(KeyRe(i))(FaNr).Proz = 100.0
              RezeptGruppe.Rezepte(KeyRe(i))(FaNr).Prob = 100.0

            End If
          End If
        End If
        '
        '
        '
        '
      Catch
        MsgBox("Error in text file: " & Chr(13) & StringLine)
        fileReader.Close()
        Exit Sub
      End Try

    Loop


    fileReader.Close()
    '
    '
    '
    '
    '
    'Alle Rezepte in Datenbank übernehmen
    '
    '
    For i = 0 To RezeptGruppe.Rezepte.RezCount - 1
      RzKey = RezeptGruppe.Rezepte.RezKey(i)
      lblRezName.Text = RezeptGruppe.Rezepte(RzKey).Name
      Application.DoEvents()
      '
      'Prüfen, ob Rezept vorhanden
      '
      ReWrRezept.ReadRezeptName(RezeptGruppe.Rezepte(RzKey).Name, RzId, ier)
      '
      '
      If RzId <> -1 Then
        '
        'Rezept löschen
        '
        ReWrRezept.DelRezept(RzId, RezeptGruppe.Rezepte(RzKey).Gid, ier)
      End If
      '
      '
      'Rezept hinzufügen
      '
      '
      ReWrRezept.AddRezept(RzKey, RzId, RezeptGruppe, UntID, TypID, SmpID, ier)
    Next i
    'ReWrRezept.AddRezept(
    ''
SAPEnd:
    btnSAPRezepte.Enabled = True
    btnRezepte.Enabled = True
    lblRezName.Visible = False
    lstRezepte.Enabled = True
    Cursor = Cursors.Default

  End Sub

  Private Sub radRwerte_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles radRwerte_0.CheckedChanged, radRwerte_1.CheckedChanged, radRwerte_2.CheckedChanged
    Dim index As Integer
    Dim i As Integer
    If sender.name = "" Then Exit Sub
    TDBRezept.Visible = False
    If radRwerte Is Nothing OrElse radRwerte.Count = 0 Then Exit Sub
    For i = 0 To radRwerte.Count - 1
      If radRwerte(i).Checked Then
        index = i
        Exit For
      End If
    Next
    If index = 2 Then
      chkRWRT.Visible = True
      panGewichte.Visible = True
      panFarbwerte.Visible = True
    Else
      chkRWRT.Checked = False
      chkRWRT.Visible = False
      panGewichte.Visible = False
      panFarbwerte.Visible = False
    End If
    index = CInt(sender.name.substring(10, 1))
    btnRezepte.PerformClick()
  End Sub

  Private Sub chkRWRT_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkRWRT.CheckedChanged
    If chkRWRT.Checked Then
      btnVOR.Visible = True
      lblVOO.Visible = True
    Else
      btnVOR.Visible = False
      lblVOO.Visible = False
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

   
  '
  '
  '
  Sub NewRezeptTab(ByVal Index As Integer, ByVal FarbID() As Integer, ByRef Rezepte As DataTable, ByRef FarbRezept As DataTable)
    Dim i As Integer
    Dim j As Integer
    Dim Jzaehl As Integer
    Dim FilterString As String
    Dim ViewFarben As DataView
    ViewFarben = New DataView(FarbRezept)
    If FarbID.Count <> 0 Then
      Select Case Index
        '
        '
        Case 0
          '
          '
          'Rezepte, die mindestens alle ausgewähltes Farbmittel aus FarbID enthalten
          '
          '
          FilterString = "REZEPT_ID="
          For i = 0 To Rezepte.Rows.Count - 1
            Jzaehl = 0
            For j = 0 To FarbID.Count - 1
              ViewFarben.RowFilter = FilterString & Rezepte.Rows(i)("REZEPT_ID") & " AND FARBM_ID=" & FarbID(j)
              If ViewFarben.Count = 1 Then
                Jzaehl = Jzaehl + 1
              End If
            Next j
            ViewFarben.RowFilter = FilterString & Rezepte.Rows(i)("REZEPT_ID")
            If Jzaehl < FarbID.Count Then
              Rezepte.Rows(i).Delete()
            End If
          Next i
          '
          '
        Case 1
          '
          '
          'Rezepte, die genau alle ausgewähltes Farbmittel aus FarbID enthalten
          '
          '
          FilterString = "REZEPT_ID="
          For i = 0 To Rezepte.Rows.Count - 1
            ViewFarben.RowFilter = FilterString & Rezepte.Rows(i)("REZEPT_ID")
            If ViewFarben.Count = FarbID.Count Then
              For j = 0 To FarbID.Count - 1
                ViewFarben.RowFilter = FilterString & Rezepte.Rows(i)("REZEPT_ID") & " AND FARBM_ID=" & FarbID(j)
                If ViewFarben.Count = 0 Then
                  Exit For
                End If
              Next j
              If j = FarbID.Count Then
                Continue For
              End If
            End If
            Rezepte.Rows(i).Delete()
          Next i
          '
          '
        Case 2
          '
          '
          'Rezepte, die höchstzens alle ausgewähltes Farbmittel aus FarbID enthalten
          '
          '
          FilterString = "REZEPT_ID="
          For i = 0 To Rezepte.Rows.Count - 1
            ViewFarben.RowFilter = FilterString & Rezepte.Rows(i)("REZEPT_ID")
            For j = 0 To ViewFarben.Count - 1
              If Not FarbID.Contains(ViewFarben(j)("FARBM_ID")) Then
                Rezepte.Rows(i).Delete()
                Exit For
              End If
            Next j
          Next i
          '
          '
        Case 3
          '
          '
          'Rezepte, die mindestens ein ausgewähltes Farbmittel aus FarbID enthalten
          '
          '
          FilterString = "("
          For j = 0 To FarbID.Count - 1
            If j > 0 Then
              FilterString = FilterString & " OR "
            End If
            FilterString = FilterString & "FARBM_ID=" & FarbID(j)
          Next j
          FilterString = FilterString & ")"
          For i = 0 To Rezepte.Rows.Count - 1
            ViewFarben.RowFilter = FilterString & " AND REZEPT_ID=" & Rezepte.Rows(i)("REZEPT_ID")
            If ViewFarben.Count = 0 Then
              Rezepte.Rows(i).Delete()
            End If
          Next i
          '
          '
          '
          '
        Case 4
          '
          '
          'Rezepte, die genau ein ausgewähltes Farbmittel aus FarbID enthalten
          '
          '

          FilterString = "("
          For j = 0 To FarbID.Count - 1
            If j > 0 Then
              FilterString = FilterString & " OR "
            End If
            FilterString = FilterString & "FARBM_ID=" & FarbID(j)
          Next j
          FilterString = FilterString & ")"
          For i = 0 To Rezepte.Rows.Count - 1
            ViewFarben.RowFilter = FilterString & " AND REZEPT_ID=" & Rezepte.Rows(i)("REZEPT_ID")
            If ViewFarben.Count <> 1 Then
              Rezepte.Rows(i).Delete()
            End If
          Next i
          '
          '
          '
        Case 5
          '
          '
          '
          'Rezepte, die höchstens ein ausgewähltes Farbmittel aus FarbID enthalten
          '
          '

          FilterString = "REZEPT_ID="
          For i = 0 To Rezepte.Rows.Count - 1
            Jzaehl = 0
            ViewFarben.RowFilter = FilterString & Rezepte.Rows(i)("REZEPT_ID")
            For j = 0 To ViewFarben.Count - 1
              If FarbID.Contains(ViewFarben(j)("FARBM_ID")) Then
                Jzaehl = Jzaehl + 1
              End If
            Next j
            If Jzaehl > 1 Then
              Rezepte.Rows(i).Delete()
            End If
          Next i
          '
          '
          '
        Case 6
          '
          '
          '
          'Rezepte, die kein ausgewähltes Farbmittel aus FarbID enthalten
          '
          '

          FilterString = "("
          For j = 0 To FarbID.Count - 1
            If j > 0 Then
              FilterString = FilterString & " OR "
            End If
            FilterString = FilterString & "FARBM_ID=" & FarbID(j)
          Next j
          FilterString = FilterString & ")"
          For i = 0 To Rezepte.Rows.Count - 1
            ViewFarben.RowFilter = FilterString & " AND REZEPT_ID=" & Rezepte.Rows(i)("REZEPT_ID")
            If ViewFarben.Count > 0 Then
              Rezepte.Rows(i).Delete()
            End If
          Next i
      End Select
    End If
  End Sub
  Sub GetNameID(ByRef ID As Integer, ByRef Name As String, ByRef Banum As String)
    If GetRwerte.HideSofort Then
      GetRwerte.InVisible()
    End If
  End Sub

  Private Sub btnVOR_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnVOR.Click
    Dim Dialo As DialogResult
    '
    'Vorlage
    '
    '
    TDBRezept.Visible = False
    lblRezept.Visible = False
    GrpRwerte(0)("V").IVoNa = False
    GetRwerte.Messrefel = GrpRwerte(0)("V")
    Me.Enabled = False
    GetRwerte.Iarch = 0
    GetRwerte.Captext = Texxt(861)
    Call GetRwerte.ReflexWerte(Dialo)

    Me.Enabled = True
    If Dialo <> DialogResult.OK Then Exit Sub
    '

    If GrpRwerte(0)("V").IVoNa Then

      lblVOO.Text = GrpRwerte(0)("V").Name
       
    End If
  End Sub
  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtMenge.Validating,txtDicke.Validating, _
      txtDa.Validating,txtDb.Validating,txtDC.Validating,txtDE.Validating,txtDH.Validating,txtDL.Validating, _
      txtGewDC.Validating,txtGewDH.Validating,txtGewDL.Validating
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



End Class