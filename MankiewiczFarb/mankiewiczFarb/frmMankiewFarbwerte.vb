Option Compare Text
Option Strict Off
Option Explicit On
Public Class frmMankiewFarbwerte
  Inherits System.Windows.Forms.Form
  Dim CnHilf As OleDbConnection
  Dim WithEvents GetRwerte As HandleRwerte
  Dim FarbTabWerte As DataTable
  Dim RefTabWerte As DataTable
  Dim DataAdapter As OleDbDataAdapter
  Dim AllgCommand As OleDbCommand
  Dim SortiID As Integer
  Dim MischID As Integer
  Dim DataBaseName As String
  Dim ID As Integer
  Dim GrpRwerte As RefValuesGrp
  Dim RefFaUntgr As RefValues
  Dim RefWerte As RefValue
  Dim FarbMerkmale As ValuesGrpsAssigns
  Dim quali As QualKontrolle
  Dim ier As Integer
  Dim Ianwsg As Integer
  Dim AnwsgID As Integer
  Dim KeyAnwsg As String
  Dim ppv As PrintPreviewDialog
  Dim TabDruck As HandleGridDruck
  'Dim Ausgabe As AusgabeCreateObj
  Dim radwinkel As List(Of RadioButton)
  Dim chkwinkel As List(Of CheckBox)
  Dim lblWinkel As List(Of Label)
  Dim panwinkel As Panel
  Dim radLichtart As List(Of RadioButton)
  Dim chkLichtart As List(Of CheckBox)
  Dim lblLichtart As List(Of Label)
  Dim panLichtart As Panel
  Dim WithEvents cboAnweisungen As ComboBox



  Public Sub New(ByVal MethID As Integer)
    Dim i As Integer
    ' This call is required by the Windows Form Designer.
    InitializeComponent()

    ' Add any initialization after the InitializeComponent() call.

    '
    '
    '
    '
    '
    dataadapter = New OleDbDataAdapter
    dataadapter.SelectCommand = New OleDbCommand
    dataadapter.SelectCommand.Connection = Cndat()
    '
    ' 
    'flgKAL.TableStyles(0).GridColumnStyles.Add(New DataGridBoolColumn)
    'AufbauPar.MethID = 2
    'Measure.Umspeich(MenueParam.Messg, MenueParam.Normfa(0))
    'If AufbauPar.ier <> 0 Then
    ' Exit Sub
    ' End If
    '
    '
    '
    AufbauPar.MethID = MethID
    If AufbauPar.ier <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '

    ppv = New PrintPreviewDialog
    'Ausgabe = New AusgabeCreateObj
  End Sub



  Private Sub frmMankiewFarbwerte_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
    AufbauPar.MethID = -1

  End Sub


  

 
 
  Private Sub frmMankiewFarbwerte_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim j As Integer
    FormMDI.Icon = My.Resources.farbwerte
    FormMDI.Text = MenueParam.User.Name & " - " & MenueParam.Messg.Kbez & " - " & MenueParam.Menue.MethBez

    btnRemission.Text = Texxt(4651)
    btnRemissionNeu.Text = Texxt(4687)
    btnDrucken.Text = Texxt(4652)
    btnUserDruck.Text = Texxt(392)
    btnEtiketten.Text = Texxt(392) & "(E)"
    btnKopieren.Text = Texxt(389)
    radGrid_0.Text = Texxt(4650)
    radGrid_1.Text = Texxt(4685)
    '
    '
    radwinkel = FormMDI.radWinkel
    chkwinkel = FormMDI.chkWinkel
    panwinkel = FormMDI.panWinkel
    lblWinkel = FormMDI.lblWinkel
    radLichtart = FormMDI.radLichtart
    chkLichtart = FormMDI.chkLichtart
    lblLichtart = FormMDI.lblLichtart
    panLichtart = FormMDI.panLichtart
    '
    '
    '

    'Winkel
    '
    '
    For i = 0 To chkwinkel.Count - 1
      RemoveHandler chkwinkel(i).Click, AddressOf LichtartWinkel_click
      chkwinkel(i).Visible = False
      radwinkel(i).Visible = False
      lblWinkel(i).Visible = False
    Next
    '
    '
    '
    FormMDI.panWinkel.Visible = True
    FormMDI.panLichtart.Visible = True
    For i = 0 To MenueParam.User.Winkel.Km - 1
      chkwinkel(i).Visible = True
      chkwinkel(i).Checked = True
      lblWinkel(i).Visible = True
      lblWinkel(i).Text = MenueParam.User.Winkel(i).IhrmBez
      AddHandler chkwinkel(i).Click, AddressOf LichtartWinkel_click
    Next
    '
    '
    'Lichtarten
    '
    '
    For i = 0 To chkLichtart.Count - 1
      RemoveHandler chkLichtart(i).Click, AddressOf LichtartWinkel_click
      radLichtart(i).Visible = False
      chkLichtart(i).Visible = False
      lblLichtart(i).Visible = False
    Next
    '
    '
    '
    FormMDI.panLichtart.Visible = True
    For i = 0 To Min(MenueParam.Normfa.Nlz, chkLichtart.Count) - 1
      chkLichtart(i).Visible = True
      chkLichtart(i).Checked = True
      lblLichtart(i).Visible = True
      lblLichtart(i).Text = MenueParam.Normfa(i).NormNama
      AddHandler chkLichtart(i).Click, AddressOf LichtartWinkel_click

    Next
    Application.DoEvents()
    '
    '
    FarbTabWerte = New DataTable
    flgFarbwerte.DataSource = FarbTabWerte
    RefTabWerte = New DataTable
    flgRwerte.DataSource = RefTabWerte    '
    '
    GetRwerte = New HandleRwerte
    GetRwerte.Measure = Measure
    '

    GrpRwerte = New RefValuesGrp
    GrpRwerte.Add("M", New RefValues) '
  
    '
    '
    FarbMerkmale = New ValuesGrpsAssigns

    '

    quali = New QualKontrolle
    

    'Call AufbauPar.AufbauMerkStdFarbw(FarbMerkmale, ier)
    Me.Enabled = False
    Call AufbauPar.AufbauAnwsgMerk(MenueParam.UserID, MenueParam.MethID, FarbMerkmale, ier)
    '
    '
    If FarbMerkmale.Count = 0 Then
      MessageBox.Show(Texxt(4123), Texxt(2000), MessageBoxButtons.OK)
      Exit Sub
    End If
    '
    '
    '
    'Anweisungen in Combobox übernehmen

    Application.DoEvents()
    cboAnweisungen = FormMDI.cboAnweisungen
    cboAnweisungen.Items.Clear()

    cboAnweisungen.Items.Clear()
    cboAnweisungen.Visible = True
    j = 0
    cboAnweisungen.SelectedIndex = -1
    Application.DoEvents()
    For i = 0 To FarbMerkmale.Count - 1
      cboAnweisungen.Items.Add(New ListTextID(FarbMerkmale(i).AnwsgID, FarbMerkmale(i).AnwsgName))
      If MenueParam.MethID = 2 Then
        If FarbMerkmale(i).AnwsgID = 19 Then
          j = i
        End If
      End If
    Next i
    cboAnweisungen.SelectedIndex = j







    'flgFarbwerte.AutoResizeColumns(DataGridViewAutoSizeColumnMode.ColumnHeader)
    'flgFarbwerte.AutoResizeColumns(DataGridViewAutoSizeColumnsMode.AllCells)



    '
    '
    '

   
    '
    flgFarbwerte.RowHeadersVisible = False
    flgFarbwerte.AllowUserToOrderColumns = False
    flgFarbwerte.AllowUserToAddRows = False
    flgFarbwerte.AllowUserToDeleteRows = False

    flgFarbwerte.AllowUserToOrderColumns = False
    flgFarbwerte.ColumnHeadersDefaultCellStyle.Font = New Font(flgFarbwerte.ColumnHeadersDefaultCellStyle.Font, FontStyle.Bold)

    'flgFarbwerte.AutoResizeColumns(DataGridViewAutoSizeColumnMode.ColumnHeader)
    flgFarbwerte.AutoResizeColumns(DataGridViewAutoSizeColumnsMode.AllCells)
    Me.Enabled = True
    Application.DoEvents()
    
  End Sub

  Private Sub btnRemission_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRemission.Click, btnRemissionNeu.Click
    If IsNothing(FarbTabWerte) Then Exit Sub
    If sender.name.contains("Neu") Then
      GrpRwerte("M").clear()
      FarbTabWerte.Rows.Clear()
      RefTabWerte.Rows.Clear()
      RefWerte = New RefValue
      RefWerte.QuControl = New QuControls
      RefWerte.QuControl.Cart = "@TMW"
      RefWerte.QuControl.MethID = MenueParam.MethID
      RefWerte.Itp = True
      RefWerte.IVoNa = True
      RefWerte.kwb = 1
      RefWerte.Tag = "1"
      RefWerte.Nr = GrpRwerte("M").Count
    End If
    GetRwerte.Messrefel = RefWerte
    Me.Enabled = False
    GetRwerte.Iarch = 0
    Select Case MenueParam.MethID
      Case 2
        If GrpRwerte("M").Count = 0 Then
          GetRwerte.Captext = Texxt(1205)
        Else
          GetRwerte.Captext = Texxt(1206)
        End If
      Case 3
        If GrpRwerte("M").Count = 0 Then
          GetRwerte.Captext = Texxt(1207)
        ElseIf GrpRwerte("M").Count = 1 Then
          GetRwerte.Captext = Texxt(1209)
        ElseIf GrpRwerte("M").Count Mod 2 = 0 Then
          GetRwerte.Captext = Texxt(1208)
        Else
          GetRwerte.Captext = Texxt(1210)
        End If
    End Select
    Call GetRwerte.ReflexWerte(True)

    Me.Enabled = True
  End Sub
   
 
  Private Sub frmMankiewFarbwerte_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize, Me.Shown
    If Not Me.Visible Then Exit Sub
    Call ResizeChild(Me)
    
  End Sub

  
  Private Sub frmMankiewFarbwerte_VisibleChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.VisibleChanged
    If Me.Visible Then
      Try
        btnRemissionNeu.PerformClick()
      Catch
      End Try
    End If
  End Sub

  Private Sub btnDrucken_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnDrucken.Click
    '
    If flgFarbwerte.Visible Then
      '
      '
      TabDruck = New HandleGridDruck(flgFarbwerte)
    Else
      TabDruck = New HandleGridDruck(flgRwerte)
    End If
    TabDruck.Ueberschrift = FarbMerkmale(KeyAnwsg)(0).Bem
    If FormMDI.chkDrucken.Checked Then
      TabDruck.Ueberschrift = InputBox(Texxt(4658), Texxt(4659), TabDruck.Ueberschrift)
    End If
    'TabDruck.Ueberschrift = TabDruck.Ueberschrift & Space(1) & "(" & cboLichtarten.Text & ")"
    TabDruck.KopfZeile = "MANKIEWICZ COLOR SYSTEM (" & MenueParam.Messg.Kbez & ")"
    If TabDruck.Ueberschrift = "" Then
      TabDruck.dispose()
      Exit Sub
    End If
        printdoc = New PrintDocument
        printdoc.PrinterSettings = pd.PrinterSettings
        

    TabDruck.DefaultFont = New Font(TabDruck.DefaultFont.Name, 12, FontStyle.Bold)

    '
    'Hochformat(false)/Querformat(true)
    '
    'printdoc.DefaultPageSettings.Landscape = chkFormat.Checked
    ppv.Document = printdoc
    AddHandler printdoc.PrintPage, AddressOf TabDruck.DruckGrid
    If FormMDI.chkDrucken.Checked Then
      ppv.WindowState = FormWindowState.Maximized
      'Preview anzeigen 
      ppv.ShowDialog()
    Else
      printdoc.Print()
    End If

    RemoveHandler printdoc.PrintPage, AddressOf TabDruck.DruckGrid

    printdoc.Dispose()
    TabDruck.dispose()
  End Sub

   

  
  Private Sub GetRwerte_HdlGetNameID(ByRef ID As Integer, ByRef Name As String, ByRef Banum As String) Handles GetRwerte.HdlGetNameID
    Dim ier As Integer
    GrpRwerte("M").Add(KeyRe(GrpRwerte("M").Count), RefWerte)

    '
    '
    '
    'Farbwerte berechnen
    '
    '
    Select Case MenueParam.MethID
      Case 2
        '
        'Farbwerte
        '
        Call quali.CalcFarbWerte(MenueParam.User.Winkel, GrpRwerte, FarbMerkmale, ier)
        Call FarbWertGridTable(KeyAnwsg, GrpRwerte("M"), FarbMerkmale, FarbTabWerte, chkLichtart, chkwinkel, flgFarbwerte)
        '
        'Grid für R-Werte
        '
        Call RwertGridTable(MenueParam.Messg.Kbez, MenueParam.User.Winkel, GrpRwerte("M"), RefTabWerte, chkwinkel, flgRwerte)
        '
        '
        '
        RefWerte = New RefValue
        RefWerte.Itp = False
        RefWerte.IVoNa = True
        RefWerte.kwb = 1
        RefWerte.Tag = "1"
        RefWerte.Nr = GrpRwerte("M").Count
        RefWerte.QuControl = New QuControls
        RefWerte.QuControl.Cart = "@PMW"

        GetRwerte.Messrefel = RefWerte
        GetRwerte.Captext = Texxt(1206)
      Case 3
        '
        'FarbWerte(Weiß / Schwarz)
        '
        '
        '
        '
        '
        If GrpRwerte("M").Count Mod 2 = 0 Then
          Call quali.CalcFarbWerte(MenueParam.User.Winkel, GrpRwerte, FarbMerkmale, ier)
          Call FarbWertGridTable(KeyAnwsg, GrpRwerte("M"), FarbMerkmale, FarbTabWerte, chkLichtart, chkwinkel, flgFarbwerte)
          '
          'Grid für R-Werte
          '
          Call RwertGridTable(MenueParam.Messg.Kbez, MenueParam.User.Winkel, GrpRwerte("M"), RefTabWerte, chkwinkel, flgRwerte)
          '
        End If
        '
        '
        RefWerte = New RefValue
        RefWerte.Itp = False
        RefWerte.IVoNa = True
        RefWerte.kwb = 1
        RefWerte.Tag = "1"
        RefWerte.Nr = GrpRwerte("M").Count
        RefWerte.QuControl = New QuControls
        If GrpRwerte("M").Count = 0 Then
          GetRwerte.Captext = Texxt(1207)
          RefWerte.QuControl.Cart = "@TMW"
        ElseIf GrpRwerte("M").Count = 1 Then
          GetRwerte.Captext = Texxt(1209)
          RefWerte.QuControl.Cart = "@TMS"
        ElseIf GrpRwerte("M").Count Mod 2 = 0 Then
          GetRwerte.Captext = Texxt(1208)
          RefWerte.QuControl.Cart = "@PMW"
        Else
          GetRwerte.Captext = Texxt(1210)
          RefWerte.QuControl.Cart = "@PMS"
        End If
        GetRwerte.Messrefel = RefWerte
    End Select
  End Sub

  

  Private Sub radGrid_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles radGrid_0.Click, radGrid_1.Click
    If radGrid_0.Checked Then
      flgFarbwerte.Visible = True
      flgRwerte.Visible = False
    Else

      flgFarbwerte.Visible = False
      flgRwerte.Visible = True
    End If
  End Sub

  Private Sub btnKopieren_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnKopieren.Click
    Clipboard.Clear()
    If flgFarbwerte.Visible Then
      '
      'Tabellen für R-Werte oder Grunddaten in Zwischenablage
      '
      '
      '
      flgFarbwerte.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
      flgFarbwerte.SelectAll()

      If flgFarbwerte.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
        Clipboard.SetDataObject(flgFarbwerte.GetClipboardContent)
      End If
      flgFarbwerte.ClearSelection()
    ElseIf flgRwerte.Visible Then
      '
      'Tabellen für Farbwerte und Rezepte in Zwischenablage
      '
      '
      '
      flgRwerte.RowHeadersVisible = False
      flgRwerte.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
      flgRwerte.SelectAll()

      If flgRwerte.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
        Clipboard.SetDataObject(flgRwerte.GetClipboardContent)
      End If
      flgRwerte.ClearSelection()
      flgRwerte.RowHeadersVisible = True
    End If
  End Sub

 
  Sub LichtartWinkel_click(ByVal sender As Object, ByVal e As System.EventArgs)
    If Not Me.Visible Then Exit Sub

    Call RwertGridTable(MenueParam.Messg.Kbez, MenueParam.User.Winkel, GrpRwerte("M"), RefTabWerte, chkwinkel, flgRwerte)

    Call FarbWertGridTable(KeyAnwsg, GrpRwerte("M"), FarbMerkmale, FarbTabWerte, chkLichtart, chkwinkel, flgFarbwerte)

  End Sub
  '
  '
  'Aufruf User-Programm
  '
  '
  Private Sub btnUserDruck_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnUserDruck.Click, btnEtiketten.Click
    Dim Objectstring As String = "MankiewiczAusgabe.AusgabeMankiewicz"
    Dim Ausgabeobject As Object
    '
    '
    '
    Try
      Ausgabeobject = CreateObject(Objectstring)
    Catch ex As Exception
      MsgBox(Err.Description & ": " & Objectstring)
      Ausgabeobject = Nothing
      Exit Sub
    End Try
    '
    '
    '
    Try
      If sender.name = "btnUserDruck" Then
        If radGrid_0.Checked Then
          Ausgabeobject.SubDruck(0, MenueParam.UserID, MenueParam.MethID, MenueParam.Messg, flgFarbwerte.DataSource)
        Else
          Ausgabeobject.SubDruck(1, MenueParam.UserID, MenueParam.MethID, MenueParam.Messg, flgRwerte.DataSource)
        End If
      End If
    Catch
    End Try
    Ausgabeobject = Nothing
  End Sub

  

  Private Sub cboAnweisungen_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboAnweisungen.SelectedIndexChanged
    If cboAnweisungen.SelectedIndex = -1 Then Exit Sub
    KeyAnwsg = KeyName(cboAnweisungen.SelectedItem.id)
    If Not Me.Enabled Then Exit Sub
    '

    
    Call FarbWertGridTable(KeyAnwsg, GrpRwerte("M"), FarbMerkmale, FarbTabWerte, chkLichtart, chkwinkel, flgFarbwerte)
    '
    '
   

  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
   
  End Sub

  Private Sub frmMankiewFarbwerte_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
    
  End Sub

 
   
  Private Sub frmMankiewFarbwerte_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    If IsNothing(FarbMerkmale) Then Exit Sub
    FarbMerkmale.dispose()
    If IsNothing(FarbTabWerte) Then Exit Sub
    FarbTabWerte.Dispose()
    If IsNothing(GrpRwerte) Then Exit Sub
    GrpRwerte.clear()
    GrpRwerte.dispose()
    If IsNothing(RefWerte) Then Exit Sub
    RefWerte.RefKurv.clear()
    RefWerte.dispose()
    cboAnweisungen.Items.Clear()
    cboAnweisungen = Nothing
  End Sub
End Class