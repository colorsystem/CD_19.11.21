Option Explicit On
Option Compare Text
Option Strict Off
Public Class frmBlockverarbeitungKorr

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
  Dim RezGrid As HandleRezC1Screen

  '
  Dim RezSozpt As RecipesGrp
  Dim RefWerteUNT As List(Of RefValue)
  Dim RefWerteTYP As List(Of RefValue)
  Dim RefWerteSMP As List(Of RefValue)
  Dim KeySor As String
  Dim TypID() As Integer = {-1, -1}
  Dim SmpID() As Integer = {-1, -1}
  Dim UntID() As Integer = {-1, -1}

  '
  Dim GrpRwerte As RefValuesGrp
  Dim OptGesamt As OpticalData
  Dim RwWrRezept As ReadWriteRezept
  Dim ReWrRwert As ReadWriteRwert
  Dim ReWrFarbe As ReadWriteFarbe
  Dim FarbWrt As ValuesGrps
  Dim WinHilf As AngGeos
  Dim CalcRezept As RezeptBerechnung
  Dim quali As QualKontrolle
  Dim Umr As RezeptUmrechnung
  Dim HandleRezept As HandleRezGeneral
  Dim HandleRwrt As HandleRwerte
  Dim RezHLFSelecID As List(Of Integer)
  Dim RezHlfAvailID As List(Of Integer)
  '
  Dim Winkel As AngGeos
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
  Dim Igx As Integer
  Dim HscMax As Integer
  Dim InoMeng As Single
  Dim kwb As Integer
  Dim DatenFarb As HandleRezDsetFarb
  Dim Farbtab As DataTable
  Dim FarbView As DataView
  Dim ErsatzView As DataView
  Dim DataGridComboFarb As DataGridViewComboBoxColumn
  Dim DataGridComboErsatz As DataGridViewComboBoxColumn
  Dim TblRezept As DataTable
  Dim TblRefGew As DataTable
  Dim TabGridWerte As List(Of DataTable)
  Dim TabGridBase As List(Of DataTable)
  Dim grdWrt As List(Of DataGridView)
  Dim flggrid As List(Of DataGridView)
  Dim radGrid As List(Of RadioButton)
  Dim DbErr As Integer
  Dim KdrAll As Integer
  '
  '
 
  '
  '
  '
  Dim ListFarbid() As Integer




  '
  '
  '
  '
  '
  '
  '
  '
 

  Dim RezGraphics As HandleRezGrafik

 
  Private Sub frmBlockverarbeitungKorr_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim j As Integer
    '
    '
    Winkel = MenueParam.User.Winkel
    Me.Text = Texxt(1872) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    ConFlgKopieren.Text = Texxt(389)
    lblBIS.Text = Texxt(377)
    lblFAQ.Text = Texxt(802)
    lblFAR.Text = Texxt(738)
    lblSUC.Text = Texxt(4606)
    lblGRP.Text = Texxt(3669)
    lblARB.Text = Texxt(2002)
    chkDatum.Text = Texxt(140)
    chkGrundKorr.Text = Texxt(3954)
    chkCalculationOnly.Text = Texxt(3955)
    btnSUC.Text = Texxt(835)
    btnRechnung.Text = Texxt(886)
    btnAnzeigen.Text = Texxt(3690)
    radGrid_0.Text = Texxt(3413)
    radGrid_1.Text = Texxt(3414)
    btnCLIP.Text = Texxt(3662)
    btnZurück.Text = Texxt(4617)

    '
    HandleRwrt = New HandleRwerte

    HandleRezept = New HandleRezGeneral
    RezSozpt = New RecipesGrp
    GrpRwerte = New RefValuesGrp
    OptGesamt = New OpticalData
    RwWrRezept = New ReadWriteRezept
    ReWrFarbe = New ReadWriteFarbe
    ReWrRwert = New ReadWriteRwert
    CalcRezept = New RezeptBerechnung
    Umr = New RezeptUmrechnung
    quali = New QualKontrolle
    RezGraphics = New HandleRezGrafik
    '
    '
    'Gruppe Rezepte
    HandleRezept.cboGRP = cboGRP
    HandleRezept.lblGRP = lblGRP
    HandleRezept.GroupList()
    '
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
    'Tabelle Farbmittel
    '
    '
    DatenFarb = New HandleRezDsetFarb
    DatenFarb.DsetFarbCreate()
    DatenFarb.DsetFarbFill(1, False)
    '
    '
    Farbtab = DatenFarb.TblFarb
    '
    ErsatzView = New DataView(Farbtab)

    FarbView = New DataView(Farbtab)

    '
    'Mit farbmitteln
    '
    '
    lstFAQ.DataSource = Farbtab
    lstFAQ.ValueMember = "FARBM_ID"
    lstFAQ.DisplayMember = "FARBM_NAME"
    '
    lstFAR.ValueMember = "ID"
    lstFAR.DisplayMember = "TEXT"
    '
    '
    'Rezepte
    '
    TblRezept = New DataTable
    '
    '
    flgRezepte.DataSource = TblRezept
    RezHLFSelecID = New List(Of Integer)
    RezHlfAvailID = New List(Of Integer)

    '
    '
    RezSozpt = New RecipesGrp
    RezSozpt.Rezepte.AddRez("MEN", New Recipe)
    RezSozpt.Rezepte.AddRez("KOR", New Recipe)
    RezSozpt.Rezepte.AddRez("MZU", New Recipe)
    RezSozpt.Rezepte.AddRez("ZUW", New Recipe)
    RezSozpt.Rezepte.AddRez("NEU", RezSozpt.Rezepte("MEN"))
    RezSozpt.Rezepte.AddRez("ALT", RezSozpt.Rezepte("MEN"))
    RezSozpt.Rezepte.AddRez("KOO", RezSozpt.Rezepte("KOR"))
    '
    '
    '
    For j = 0 To 1
      UntID(j) = -1
      TypID(j) = -1
      SmpID(j) = -1
    Next j
    RefWerteUNT = New List(Of RefValue)
    RefWerteTYP = New List(Of RefValue)
    RefWerteSMP = New List(Of RefValue)

    '
    '
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
    '
    TabGridBase = New List(Of DataTable)
    TabGridWerte = New List(Of DataTable)
    TabGridBase.Add(New DataTable)
    TabGridBase.Add(New DataTable)
    TabGridWerte.Add(New DataTable)
    TabGridWerte.Add(New DataTable)
    '
    '
    grdWrt = New List(Of DataGridView)
    grdWrt.Add(grdWRT_0)
    grdWrt.Add(grdWRT_1)
   
    For i = 0 To 1
      grdWrt(i).AllowUserToAddRows = False
      grdWrt(i).AllowUserToDeleteRows = False
      grdWrt(i).AlternatingRowsDefaultCellStyle.BackColor = Color.AntiqueWhite
      grdWrt(i).RowHeadersVisible = False
      grdWrt(i).SelectionMode = DataGridViewSelectionMode.RowHeaderSelect

      grdWrt(i).DataSource = TabGridWerte(i)
      '
    Next i
    flggrid = New List(Of DataGridView)
    flggrid.Add(flgGrid_0)
    flggrid.Add(flgGrid_1)
    For i = 0 To 1
      flggrid(i).AllowUserToAddRows = False
      flggrid(i).AllowUserToDeleteRows = False
      flggrid(i).AlternatingRowsDefaultCellStyle.BackColor = Color.LightGreen
      flggrid(i).RowHeadersVisible = False
      flggrid(i).SelectionMode = DataGridViewSelectionMode.RowHeaderSelect
      flggrid(i).RowHeadersVisible = False
      '
      flggrid(i).DataSource = TabGridBase(i)
    Next i
    '
    '
    radGrid = New List(Of RadioButton)
    radGrid.Add(radGrid_0)
    radGrid.Add(radGrid_1)

    '
    '
    '
    '
    flgGridErsatz.Columns.Clear()

    '
    '
    DataGridComboFarb = New DataGridViewComboBoxColumn
    DataGridComboFarb.DisplayStyle = DataGridViewComboBoxDisplayStyle.Nothing
    DataGridComboFarb.DisplayStyleForCurrentCellOnly = DataGridViewComboBoxDisplayStyle.ComboBox
    DataGridComboFarb.HeaderText = Texxt(3410)
    DataGridComboFarb.DataSource = FarbView
    DataGridComboFarb.ValueMember = "FARBM_ID"
    DataGridComboFarb.DisplayMember = "FARBM_NAME"
    flgGridErsatz.Columns.Add(DataGridComboFarb)


    DataGridComboErsatz = New DataGridViewComboBoxColumn
    DataGridComboErsatz.DisplayStyle = DataGridViewComboBoxDisplayStyle.Nothing
    DataGridComboErsatz.DisplayStyleForCurrentCellOnly = DataGridViewComboBoxDisplayStyle.ComboBox
    DataGridComboErsatz.HeaderText = Texxt(3411)
    DataGridComboErsatz.DataSource = ErsatzView
    DataGridComboErsatz.ValueMember = "FARBM_ID"
    DataGridComboErsatz.DisplayMember = "FARBM_NAME"
    flgGridErsatz.AllowUserToDeleteRows = True
    flgGridErsatz.Columns.Add(DataGridComboErsatz)
    flgGridErsatz.Columns(0).Width = 0.4 * flgGridErsatz.Width
    flgGridErsatz.Columns(1).Width = 0.4 * flgGridErsatz.Width

  End Sub

  Private Sub btnSUC_Click(sender As Object, e As System.EventArgs) Handles btnSUC.Click
    Dim i As Integer

    '
    TblRezept.Rows.Clear()
    RezHLFSelecID.Clear()
    RezHlfAvailID.Clear()
    Application.DoEvents()
    ReDim ListFarbid(lstFAR.Items.Count - 1)
    For i = 0 To lstFAR.Items.Count - 1
      ListFarbid(i) = lstFAR.Items(i).id
    Next


    '
    'Rezepte mit Nachstelungen
    'If Not HandleRezept.SqlRezeptColth(cboFAR.SelectedIndex, cboGRP.SelectedValue, chkDatum.Checked, Date.Parse(txtVON.Text), Date.Parse(txtBIS.Text).AddDays(1.0), Not chkCalculationOnly.Checked, Not chkCalculationOnly.Checked, txtSuc.Text, ListFarbid, RezHlfAvailID, TblRezept) Then
    If Not HandleRezept.SqlRezeptKorr(cboGRP.SelectedValue, chkDatum.Checked, Not chkCalculationOnly.Checked, Date.Parse(txtVON.Text), Date.Parse(txtBIS.Text).AddDays(1.0), txtSuc.Text, RezHlfAvailID, TblRezept) Then

      Cursor = Cursors.Default
      Exit Sub
    End If

    flgRezepte.Columns(0).Visible = False
    flgRezepte.Columns(1).HeaderText = Texxt(140)
    flgRezepte.Columns(2).HeaderText = Texxt(863)
    flgRezepte.Columns(2).Width = 400

    If TblRezept.Rows.Count = 0 Then
      MsgBox(Texxt(2953))
      btnRechnung.Visible = False

      Exit Sub
    End If
    btnRechnung.Visible = True
    btnAnzeigen.Visible = True
    radGrid(0).Enabled = False
    radGrid(1).Enabled = False

  End Sub
  '
  '
  Private Sub btnRechnung_Click(sender As Object, e As System.EventArgs) Handles btnRechnung.Click
    Dim RezId As Integer
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim Fanr As String
    Dim KyName As String
    Dim FaID As Integer
    Dim ier As Integer
    Dim Nwe As Integer
    Dim Keykor As String


    If TblRezept.Rows.Count = 0 Then Exit Sub
    '
    'Prüfen, ob Ersatzfarbmittel korrekt angegeben sind
    '
    '
    For j = 0 To flgGridErsatz.Rows.Count - 1
      If Not (IsNothing(flgGridErsatz.Rows(j).Cells(0).Value) And IsNothing(flgGridErsatz.Rows(j).Cells(1).Value)) Then
        If IsNothing(flgGridErsatz.Rows(j).Cells(0).Value) Or IsNothing(flgGridErsatz.Rows(j).Cells(1).Value) Then
          MsgBox(Texxt(3412) & Space(1) & CStr(j))
          Exit Sub
        End If
      End If
    Next j

    SplitContainGrid.Visible = False
    SplitContColorthek.Visible = False
    Application.DoEvents()
    '
    '


    grdWRT_0.Visible = True
    '
    Cursor = Cursors.WaitCursor
    Nwe = Winkel.Wsol.Nwe

    Call MakeTabGRIDKorrWerte(0, TabGridBase(0), flggrid(0))
    '
    Call HandleRwrt.MakeTABRefwerte(True, MenueParam.Messg.Kbez, Winkel, TabGridWerte(0))
    Call HandleRwrt.MakeGRIDRefwerte(Winkel, grdWrt(0))
    '

    KeyMenge = "MEN"
    Keykor = "KOR"


    '
    ' 
    '
    For i = 0 To TblRezept.Rows.Count - 1
      RezId = TblRezept.Rows(i)("REZEPT_ID")
      '
      '
      'Rezept lesen
      '
      '
      RezSozpt.Rezepte(KeyMenge).clear()
      RezSozpt.Farben.clear()
      '
      '
      RwWrRezept.ReadRezeptFarbGrund(KeyMenge, RezId, RezSozpt, UntID, TypID, SmpID, ier)
      '
      'Falls erforderlich Ersatzfarbmittel einfügen
      '
      '
      '
      '
      'R-Werte einlesen
      '
      '

      If MenueParam.Misch.Transp Then
        If UntID(0) >= 0 Then
          ReWrRwert.ReadRwert(UntID(0), GrpRwerte("W").RefUnt, ier)
        Else
          MsgBox(Texxt(4050) & ":" & Space(1) & RezSozpt.Rezepte(KeyMenge).Name)
          Exit Sub
        End If
      End If
      If chkCalculationOnly.Checked Then
        '
        'R-Werte werden berechnet
        '
        '
        CalcRezept.MischRezept(1, Winkel, KeyMenge, RezSozpt, GrpRwerte, OptGesamt, ier)
        If ier <> 0 Then
          If MessageBox.Show(Texxt(1999) & "?", Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
            Continue For
          Else
            GoTo EndProgKorrRech
          End If
        End If
        '
        '    
        GrpRwerte("W")("V").RefKurv.clear()
        GrpRwerte("W")("N").RefKurv.clear()
        Call ADDCurves(GrpRwerte("W")("V").RefKurv)
        Call ADDCurves(GrpRwerte("W")("N").RefKurv)
        '
        For k = 0 To GrpRwerte("W")("R").RefKurv.Count - 1
          For j = 0 To Winkel.Wsol.Nwe - 1
            GrpRwerte("W")("V").RefKurv(k).R(j) = GrpRwerte("W")("R").RefKurv(k).R(j)
            GrpRwerte("W")("N").RefKurv(k).R(j) = GrpRwerte("W")("R").RefKurv(k).R(j)
          Next j
        Next k
        '
        GrpRwerte("W")("V").Name = RezSozpt.Rezepte(KeyMenge).Name & Space(1) & "###"
        GrpRwerte("W")("N").Name = RezSozpt.Rezepte(KeyMenge).Name & Space(1) & "###"
        GrpRwerte("W")("V").IVoNa = True
        GrpRwerte("W")("N").IVoNa = True

      Else
        '
        'R-Werte werden eingelesen
        '
        '
        '
        If TypID(0) >= 0 Then
          ReWrRwert.ReadRwert(TypID(0), GrpRwerte("W")("V"), ier)
        Else
          MsgBox(Texxt(4051) & ":" & Space(1) & RezSozpt.Rezepte(KeyMenge).Name)
          Exit Sub
        End If
        If SmpID(0) >= 0 Then
          ReWrRwert.ReadRwert(SmpID(0), GrpRwerte("W")("N"), ier)
        Else
          MsgBox(Texxt(4052) & ":" & Space(1) & RezSozpt.Rezepte(KeyMenge).Name)
          Exit Sub
        End If
      End If
      '
      For k = 0 To RezSozpt.Rezepte(KeyMenge).KF - 1
        For j = 0 To flgGridErsatz.Rows.Count - 1
          If RezSozpt.Rezepte(KeyMenge)(k).ID = flgGridErsatz.Rows(j).Cells(0).Value Then
            '
            'Limitierung für altes Farbmittel = 0.0
            '
            '
            FaID = flgGridErsatz.Rows(j).Cells(0).Value
            KyName = KeyName(FaID)
            RezSozpt.Farben(KyName).BoMng = 0.0
            RezSozpt.Farben(KyName).OP = "="
            '
            'Ersatzfarbmittel mit Menge=0.0  hinzufügen
            '
            '
            If Not RezSozpt.Farben.ContainsFarb(KeyName(flgGridErsatz.Rows(j).Cells(1).Value)) Then
              FaID = flgGridErsatz.Rows(j).Cells(1).Value
              KyName = KeyName(FaID)
              RezSozpt.Farben.AddFarb(KyName, New Colorant)
              Call ReWrFarbe.FarReaGrund(FaID, RezSozpt.Farben(KyName), False, ier)
              Fanr = KeyRe(RezSozpt.Rezepte(KeyMenge).KF)
              RezSozpt.Rezepte(KeyMenge).AddFaNr(Fanr, New ColorAmount)
              RezSozpt.Rezepte(KeyMenge)(Fanr).ID = flgGridErsatz.Rows(j).Cells(1).Value
              RezSozpt.Rezepte(KeyMenge)(Fanr).FaAmng = 0.0
              RezSozpt.Rezepte(KeyMenge)(Fanr).BaAmng = 0.0
              RezSozpt.Rezepte(KeyMenge)(Fanr).Proz = RezSozpt.Farben(KyName).Prf(0)
              RezSozpt.Rezepte(KeyMenge)(Fanr).Prob = RezSozpt.Farben(KyName).Prb(0)
            End If
            Exit For
          End If
        Next j
      Next k
      '
      '
      '
      Umr.CalcBamng(KeyMenge, RezSozpt, ier)
      Umr.MNGBamngNorm(KeyMenge, RezSozpt, RezSozpt.MngMax, ier)
      Umr.CalcFamng(KeyMenge, RezSozpt, ier)
      RezSozpt.Rezepte("KOR").Dicke(0) = RezSozpt.Rezepte("MEN").Dicke(0)
      RezSozpt.Rezepte("KOR").Name = RezSozpt.Rezepte("MEN").Name
      RezSozpt.Rezepte("KOR").Bem = RezSozpt.Rezepte("MEN").Bem
      RezSozpt.Rezepte("KOR").Nr = RezSozpt.Rezepte("MEN").Nr
      '
      '
      CalcRezept.KorrekturRezept(1 + 8 + 16 * chkGrundKorr.CheckState, Winkel, KeyMenge, Keykor, RezSozpt, GrpRwerte, ier)
      If ier <> 0 Then
        If MessageBox.Show(Texxt(1999) & "?", Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
          Continue For
        Else
          GoTo endprogkorrrech
        End If
      End If



      RefWerteUNT.Clear()
      RefWerteTYP.Clear()
      RefWerteSMP.Clear()
      For j = 0 To 1
        If GrpRwerte(j).RefUnt.IVoNa Then
          RefWerteUNT.Add(GrpRwerte(j).RefUnt)
        Else
          RefWerteUNT.Add(Nothing)
        End If
        If GrpRwerte(j)("V").IVoNa Then
          RefWerteTYP.Add(GrpRwerte(j)("V"))
        Else
          RefWerteTYP.Add(Nothing)
        End If
        If GrpRwerte(j)("R").IVoNa Then
          GrpRwerte(j)("R").Name = GrpRwerte(j)("V").Name & Space(1) & "###"
          RefWerteSMP.Add(GrpRwerte(j)("R"))
        Else
          RefWerteSMP.Add(Nothing)
        End If
      Next j
      Call HandleRezept.TabRezRecord(Winkel, Keykor, RezSozpt, RefWerteUNT, RefWerteTYP, RefWerteSMP, TabGridWerte(0), ier)
      Call FillGridBaseRow(0, Keykor, "V", "N", "R", RezSozpt, GrpRwerte)
      '
      '
      '
      '


    Next i
    '
    SplitContainGrid.Visible = True
    radGrid(0).Enabled = True
    radGrid(0).Checked = True

    grdWrt(0).Refresh()
    btnZurück.Enabled = True
    btnCLIP.Enabled = True
    Cursor = Cursors.Default
    Exit Sub
EndProgKorrRech:
    SplitContColorthek.Visible = True
    Cursor = Cursors.Default

  End Sub

  Private Sub btnAnzeigen_Click(sender As Object, e As System.EventArgs) Handles btnAnzeigen.Click
    Dim RezId As Integer
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim ier As Integer
    Dim Nwe As Integer


    If TblRezept.Rows.Count = 0 Then Exit Sub
    '
    'Prüfen, ob Ersatzfarbmittel korrekt angegeben sind
    '
    '

    SplitContainGrid.Visible = False
    SplitContColorthek.Visible = False
    Application.DoEvents()
    '
    '



    '
    Cursor = Cursors.WaitCursor
    Nwe = Winkel.Wsol.Nwe

    Call MakeTabGRIDKorrWerte(1, TabGridBase(1), flggrid(1))
    '
    Call HandleRwrt.MakeTABRefwerte(True, MenueParam.Messg.Kbez, Winkel, TabGridWerte(1))
    Call HandleRwrt.MakeGRIDRefwerte(Winkel, grdWrt(1))
    '

    KeyMenge = "MEN"


    '
    ' 
    '
    For i = 0 To TblRezept.Rows.Count - 1
      RezId = TblRezept.Rows(i)("REZEPT_ID")
      '
      '
      'Rezept lesen
      '
      '
      RezSozpt.Rezepte(KeyMenge).clear()
      RezSozpt.Farben.clear()
      '
      '
      RwWrRezept.ReadRezeptFarbGrund(KeyMenge, RezId, RezSozpt, UntID, TypID, SmpID, ier)
      '
      'Falls erforderlich Ersatzfarbmittel einfügen
      '
      '
      '
      '
      'R-Werte einlesen
      '
      '
      If MenueParam.Misch.Transp Then
        If UntID(0) >= 0 Then
          ReWrRwert.ReadRwert(UntID(0), GrpRwerte("W").RefUnt, ier)
        Else
          MsgBox(Texxt(4050) & ":" & Space(1) & RezSozpt.Rezepte(KeyMenge).Name)
          Exit Sub
        End If
      End If
      If chkCalculationOnly.Checked Then
        '
        'R-Werte werden berechnet
        '
        '
        CalcRezept.MischRezept(1, Winkel, KeyMenge, RezSozpt, GrpRwerte, OptGesamt, ier)
        If ier <> 0 Then
          If MessageBox.Show(Texxt(1999) & "?", Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
            Continue For
          Else
            GoTo EndProgKorrAnz
          End If
        End If
        '
        '    
        GrpRwerte("W")("V").RefKurv.clear()
        GrpRwerte("W")("N").RefKurv.clear()
        Call ADDCurves(GrpRwerte("W")("V").RefKurv)
        Call ADDCurves(GrpRwerte("W")("N").RefKurv)
        '
        '
        GrpRwerte("W")("V").Name = RezSozpt.Rezepte(KeyMenge).Name & Space(1) & "###"
        GrpRwerte("W")("N").Name = RezSozpt.Rezepte(KeyMenge).Name & Space(1) & "###"

        For k = 0 To GrpRwerte("W")("R").RefKurv.Count - 1
          For j = 0 To Winkel.Wsol.Nwe - 1
            GrpRwerte("W")("V").RefKurv(k).R(j) = GrpRwerte("W")("R").RefKurv(k).R(j)
            GrpRwerte("W")("N").RefKurv(k).R(j) = GrpRwerte("W")("R").RefKurv(k).R(j)
          Next j
        Next k
      Else
        '
        'R-Werte werden eingelesen
        '
        '
        '
        If TypID(0) >= 0 Then
          ReWrRwert.ReadRwert(TypID(0), GrpRwerte("W")("V"), ier)
        Else
          MsgBox(Texxt(4051) & ":" & Space(1) & RezSozpt.Rezepte(KeyMenge).Name)
          Exit Sub
        End If
        If SmpID(0) >= 0 Then
          ReWrRwert.ReadRwert(SmpID(0), GrpRwerte("W")("N"), ier)
        Else
          MsgBox(Texxt(4052) & ":" & Space(1) & RezSozpt.Rezepte(KeyMenge).Name)
          Exit Sub
        End If
      End If
      '
      '
      '
      Umr.CalcBamng(KeyMenge, RezSozpt, ier)
      Umr.MNGBamngNorm(KeyMenge, RezSozpt, RezSozpt.MngMax, ier)
      Umr.CalcFamng(KeyMenge, RezSozpt, ier)
      RefWerteUNT.Clear()
      RefWerteTYP.Clear()
      RefWerteSMP.Clear()
      For j = 0 To 1
        If GrpRwerte(j).RefUnt.IVoNa Then
          RefWerteUNT.Add(GrpRwerte(j).RefUnt)
        Else
          RefWerteUNT.Add(Nothing)
        End If
        If GrpRwerte(j)("V").IVoNa Then
          RefWerteTYP.Add(GrpRwerte(j)("V"))
        Else
          RefWerteTYP.Add(Nothing)
        End If
        If GrpRwerte(j)("N").IVoNa Then
          RefWerteSMP.Add(GrpRwerte(j)("N"))
        Else
          RefWerteSMP.Add(Nothing)
        End If
      Next j
      Call HandleRezept.TabRezRecord(Winkel, KeyMenge, RezSozpt, RefWerteUNT, RefWerteTYP, RefWerteSMP, TabGridWerte(1), ier)
      Call FillGridBaseRow(1, KeyMenge, "V", "N", "R", RezSozpt, GrpRwerte)
      '
      '
      '
      '


    Next i
    '
    SplitContainGrid.Visible = True

    grdWrt(1).Refresh()
    radGrid(1).Enabled = True
    radGrid(1).Checked = True
    btnZurück.Enabled = True
    btnCLIP.Enabled = True
EndProgKorrAnz:
    Cursor = Cursors.Default

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
  Private Sub chkDatum_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkDatum.CheckedChanged
    lblBIS.Visible = chkDatum.Checked
    txtBIS.Visible = chkDatum.Checked
    txtVON.Visible = chkDatum.Checked

  End Sub
  Private Sub btnCLIP_Click(sender As Object, e As System.EventArgs) Handles btnCLIP.Click
    Dim index As Integer
    index = 0
    If radGrid(1).Checked Then
      index = 1
    End If
    '
    'Schreiben nach Clipboard
    '
    Cursor = Cursors.WaitCursor
    Clipboard.Clear()
    '
    'Grid in Zwischenablage
    '
    '
    '
    '
    grdWrt(index).ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
    grdWrt(index).SelectAll()

    If grdWrt(index).GetCellCount(DataGridViewElementStates.Selected) > 0 Then
      Clipboard.SetDataObject(grdWrt(index).GetClipboardContent)
    End If
    grdWrt(index).ClearSelection()
    Cursor = Cursors.Default
  End Sub
  Private Sub frmBlockverarbeitungKorr_Resize(sender As Object, e As System.EventArgs) Handles Me.Resize
    Call ResizeChild(Me)

  End Sub

 
  Private Sub btnZurück_Click(sender As System.Object, e As System.EventArgs) Handles btnZurück.Click
    SplitContainGrid.Visible = False
    SplitContColorthek.Visible = True
    btnAnzeigen.Visible = False
    btnRechnung.Visible = False
  End Sub
  Sub MakeTabGRIDKorrWerte(ByVal index As Integer, ByRef Tabwerte As DataTable, ByRef flgGrid As DataGridView)
    Dim i As Integer
    Tabwerte.Rows.Clear()
    Tabwerte.Columns.Clear()
    Tabwerte.Columns.Add(Texxt(824), GetType(String))
    Tabwerte.Columns.Add("DE(alt)", GetType(Single))
    Tabwerte.Columns.Add("DL(alt)", GetType(Single))
    Tabwerte.Columns.Add("DC(alt)", GetType(Single))
    Tabwerte.Columns.Add("DH(alt)", GetType(Single))
    Tabwerte.Columns.Add("Da(alt)", GetType(Single))
    Tabwerte.Columns.Add("Db(alt)", GetType(Single))
    If index = 0 Then
      Tabwerte.Columns.Add("DE(neu)", GetType(Single))
      Tabwerte.Columns.Add("DL(neu)", GetType(Single))
      Tabwerte.Columns.Add("DC(neu)", GetType(Single))
      Tabwerte.Columns.Add("DH(neu)", GetType(Single))
      Tabwerte.Columns.Add("Da(neu)", GetType(Single))
      Tabwerte.Columns.Add("Db(neu)", GetType(Single))
    End If


    flgGrid.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgGrid.Columns(0).Width = 150
    flgGrid.Columns(0).SortMode = DataGridViewColumnSortMode.NotSortable
    For i = 1 To Tabwerte.Columns.Count - 1
      flgGrid.Columns(i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      flgGrid.Columns(i).DefaultCellStyle.Format = "###.00"
      flgGrid.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
      flgGrid.Columns(i).Width = 50

    Next
  End Sub
  Sub FillGridBaseRow(index As Integer, KeyRez As String, keyVor As String, KeyNach As String, KeyCalc As String, RezSozpt As RecipesGrp, GrpRwerte As RefValuesGrp)
    Dim l As Integer
    Dim ier As Integer
    Dim Nwe As Integer
    Dim GridRow As DataRow
    Dim RV() As Single
    Dim RN() As Single
    Dim RB() As Single
    Dim DEalt As Single
    Dim DLalt As Single
    Dim DCalt As Single
    Dim DHalt As Single
    Dim Daalt As Single
    Dim Dbalt As Single
    Dim DEneu As Single
    Dim DLneu As Single
    Dim DCneu As Single
    Dim DHneu As Single
    Dim Daneu As Single
    Dim Dbneu As Single
    '
    Nwe = Winkel.Wsol.Nwe
    ReDim RV(Nwe - 1)
    ReDim RN(Nwe - 1)
    ReDim RB(Nwe - 1)

    '
    'Ausgewählte Sätze in Grid
    '
    '
    '
    'Farbwerte für 0-ten Winkel (kw=0) berechnen
    '
    '
    For l = 0 To Nwe - 1
      RV(l) = GrpRwerte(0)(keyVor).RefKurv(Winkel(0).Chrm).R(l)
      RN(l) = GrpRwerte(0)(KeyNach).RefKurv(Winkel(0).Chrm).R(l)
      If index = 0 Then
        RB(l) = GrpRwerte(0)(KeyCalc).RefKurv(Winkel(0).Chrm).R(l)
      End If
    Next l
    '
    quali.FarbDifferenzRef(0, 0, 0, Winkel, RV, RN, DEalt, DLalt, DCalt, DHalt, Daalt, Dbalt, ier)
    If index = 0 Then
      quali.FarbDifferenzRef(0, 0, 0, Winkel, RV, RB, DEneu, DLneu, DCneu, DHneu, Daneu, Dbneu, ier)
    End If
    '
    '
    GridRow = TabGridBase(index).NewRow
    GridRow(0) = RezSozpt.Rezepte(KeyRez).Name
    GridRow("DE(alt)") = DEalt
    GridRow("DL(alt)") = DLalt
    GridRow("DC(alt)") = DCalt
    GridRow("DH(alt)") = DHalt
    GridRow("Da(alt)") = Daalt
    GridRow("Db(alt)") = Dbalt

    If index = 0 Then
      GridRow("DE(neu)") = DEneu
      GridRow("DL(neu)") = DLneu
      GridRow("DC(neu)") = DCneu
      GridRow("DH(neu)") = DHneu
      GridRow("Da(neu)") = Daneu
      GridRow("Db(neu)") = Dbneu
    End If

    TabGridBase(index).Rows.Add(GridRow)

    '
  End Sub

  Private Sub radGrid_CheckedChanged(sender As Object, e As System.EventArgs) Handles radGrid_0.CheckedChanged, radGrid_1.CheckedChanged
    Dim index As Integer
    If Len(sender.name) = 0 Then Exit Sub
    index = CInt(sender.name.substring(8, 1))


    grdWrt(index).Visible = sender.checked
    flggrid(index).Visible = sender.checked

  End Sub

  Private Sub ConFlgKopieren_Click(sender As Object, e As System.EventArgs) Handles ConFlgKopieren.Click
    Dim index As Integer
    Dim i As Integer
    index = -1
    For i = 0 To flggrid.Count - 1
      If flggrid(i).Visible Then
        index = i
      End If
    Next
    If index < 0 Then Exit Sub
    Cursor = Cursors.WaitCursor
    Clipboard.Clear()
    '
    'Grid in Zwischenablage
    '
    '
    '
    '
    flggrid(index).ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
    flggrid(index).SelectAll()

    If flggrid(index).GetCellCount(DataGridViewElementStates.Selected) > 0 Then
      Clipboard.SetDataObject(flggrid(index).GetClipboardContent)
    End If
    flggrid(index).ClearSelection()
    Cursor = Cursors.Default
  End Sub

  Private Sub chkCalculationOnly_Click(sender As Object, e As System.EventArgs) Handles chkCalculationOnly.Click
    btnAnzeigen.Visible = False
    btnRechnung.Visible = False
  End Sub
  

  Private Sub txtDat_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtBIS.Validating, txtVON.Validating
    If Not IsDate(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(2500))
    End If
  End Sub
End Class