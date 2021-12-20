Option Explicit On
Option Compare Text
Option Strict Off
Public Class frmBlockverarbeitungRefl

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
  Dim Winkel As AngGeos
  '
  Dim RezSozpt As RecipesGrp
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
  Dim TabGridWerte As DataTable
  Dim FarbView As DataView
  Dim ErsatzView As DataView
  Dim TblRezept As DataTable
  Dim TblRefGew As DataTable
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

  Private Property chkGrundKorr As Object

 

  Private Sub frmBlockverarbeitungRefl_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim j As Integer
    '
    '
    Me.Text = Texxt(1873) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    Winkel = MenueParam.User.Winkel
    lblBIS.Text = Texxt(377)
    lblFAQ.Text = Texxt(802)
    lblFAR.Text = Texxt(738)
    lblSUC.Text = Texxt(4606)
    lblGRP.Text = Texxt(3669)
    lblARB.Text = Texxt(2002)
    chkDatum.Text = Texxt(140)
    btnSUC.Text = Texxt(835)
    btnRechnung.Text = Texxt(886)
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
    '
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
    '
    '
    TabGridWerte = New DataTable


    grdWRT.AllowUserToAddRows = False
    grdWRT.AllowUserToDeleteRows = False
    grdWRT.AlternatingRowsDefaultCellStyle.BackColor = Color.AntiqueWhite
    grdWRT.RowHeadersVisible = False
    grdWRT.SelectionMode = DataGridViewSelectionMode.RowHeaderSelect

    grdWRT.DataSource = TabGridWerte
    '

    '
    '

    '
    '

    '
    '

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
    If Not HandleRezept.SqlRezeptColth(cboFAR.SelectedIndex, cboGRP.SelectedValue, chkDatum.Checked, Date.Parse(txtVON.Text), Date.Parse(txtBIS.Text).AddDays(1.0), False, False, txtSuc.Text, ListFarbid, RezHlfAvailID, TblRezept) Then
      Cursor = Cursors.Default
      Exit Sub
    End If
    If TblRezept.Rows.Count = 0 Then
      MsgBox(Texxt(2953))
      btnRechnung.Visible = False
      Exit Sub
    End If

    flgRezepte.Columns(0).Visible = False
    flgRezepte.Columns(1).HeaderText = Texxt(140)
    flgRezepte.Columns(2).HeaderText = Texxt(863)
    flgRezepte.Columns(2).Width = 400

   
    btnRechnung.Visible = True

  End Sub
  '
  '
  Private Sub btnRechnung_Click(sender As Object, e As System.EventArgs) Handles btnRechnung.Click
    Dim RezId As Integer
    Dim i As Integer
    Dim ier As Integer
    Dim Nwe As Integer
    Dim Keykor As String


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
    'Dummy für Vorlage
    '
    '
   

    grdWRT.Visible = True
    '
    Cursor = Cursors.WaitCursor
    Nwe = Winkel.Wsol.Nwe

    '
    Call HandleRwrt.MakeTABRefwerte(True, MenueParam.Messg.Kbez, Winkel, TabGridWerte)
    Call HandleRwrt.MakeGRIDRefwerte(Winkel, grdWRT)
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
      'R-Werte des Untergrunds einlesen
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


      '
      '
      '
      '
      CalcRezept.MischRezept(1, Winkel, KeyMenge, RezSozpt, GrpRwerte, OptGesamt, ier)
      If ier <> 0 Then
        If MessageBox.Show(Texxt(1999) & "?", Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
          Continue For
        Else
          GoTo endprogRefl
        End If
      End If
      '
      '    
      GrpRwerte("W")("R").Name = RezSozpt.Rezepte(KeyMenge).Name & Space(1) & "###"
      Call HandleRwrt.TabRefRecord(1, Winkel, 100.0, GrpRwerte("W")("R"), TabGridWerte, ier)
      '
      '
      '
      '
    Next i
    '
    SplitContainGrid.Visible = True


    grdWRT.Refresh()
    btnZurück.Enabled = True
    btnCLIP.Enabled = True
    Cursor = Cursors.Default
    Exit Sub
EndProgRefl:
    SplitContColorthek.Visible = True
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
    grdWRT.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
    grdWRT.SelectAll()

    If grdWRT.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
      Clipboard.SetDataObject(grdWRT.GetClipboardContent)
    End If
    grdWRT.ClearSelection()
    Cursor = Cursors.Default
  End Sub
  Private Sub frmBlockverarbeitungKorr_Resize(sender As Object, e As System.EventArgs) Handles Me.Resize
    Call ResizeChild(Me)

  End Sub


  Private Sub btnZurück_Click(sender As System.Object, e As System.EventArgs) Handles btnZurück.Click
    SplitContainGrid.Visible = False
    SplitContColorthek.Visible = True
    btnRechnung.Visible = False

  End Sub
  Sub MakeTabGRIDKorrWerte(ByVal index As Integer, ByRef Tabwerte As DataTable, ByRef flgGrid As DataGridView)

    Tabwerte.Rows.Clear()
    Tabwerte.Columns.Clear()
    Tabwerte.Columns.Add(Texxt(824), GetType(String))
    Tabwerte.Columns.Add("DEalt", GetType(Single))
    If index = 0 Then
      Tabwerte.Columns.Add("DEneu", GetType(Single))
    End If


    flgGrid.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgGrid.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgGrid.Columns(1).DefaultCellStyle.Format = "###.00"

    '
    '
    flgGrid.Columns(0).SortMode = DataGridViewColumnSortMode.NotSortable
    flgGrid.Columns(1).SortMode = DataGridViewColumnSortMode.NotSortable




    flgGrid.Columns(0).Width = 150
    flgGrid.Columns(1).Width = 50
    If index = 0 Then
      flgGrid.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      flgGrid.Columns(2).DefaultCellStyle.Format = "###.00"
      flgGrid.Columns(2).SortMode = DataGridViewColumnSortMode.NotSortable
      flgGrid.Columns(2).Width = 50
    End If
  End Sub
  

  Private Sub txtDat_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtBIS.Validating, txtVON.Validating
    If Not IsDate(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(2500))
    End If
  End Sub
End Class