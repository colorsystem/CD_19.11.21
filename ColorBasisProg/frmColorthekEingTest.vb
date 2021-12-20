Option Explicit On
Option Compare Text
Option Strict Off
Public Class frmColorthekEing
  Dim AddID As Integer = 1000
  Dim RzID As Integer
  Dim SqlStmt As String
  Dim RcmdRef As Integer
  Dim KeyMenge As String
  Dim btnREF As List(Of Button)
  Dim btnUNT As List(Of Button)
  Dim ChkKWB As List(Of CheckBox)
  Dim radKWB As List(Of RadioButton)
  Dim radVONA As List(Of RadioButton)
  Dim txtCOL As List(Of TextBox)
  Dim AdaptRezepte As OleDbDataAdapter
  Dim CmdRezepte As OleDbCommand
  Dim TabRezepte As DataTable
  Dim WherePrimaryKEY() As String = {"MISCH_ID", "REZEPT_ID"}
  '
  '
  '
  '
  Dim ViewDelRezepte As DataView
  Dim ViewAddRezepte As DataView
  Dim ViewModRezepte As DataView
  '
  Dim viewhilf As DataView
  '
  '
  Dim AdaptMisch As OleDbDataAdapter
  Dim CmdMisch As OleDbCommand
  Dim TabMisch As DataTable
  Dim WithEvents ConnRezepte As BindingSource
  Dim PicAufBau As HandlePictures
  Dim RezGrid As HandleRezC1Screen
  Dim Rezsozpt As RecipesGrp
  Dim Calcrezept As RezeptBerechnung
  Dim DatenFarb As HandleRezDsetFarb
  Dim ReWrRwert As ReadWriteRwert
  Dim ReWrRezept As ReadWriteRezept
  Dim GrpRwerte As RefValuesGrp
  Dim RezGraphics As HandleRezGrafik
  Dim RezCheckRad As HandleCheckRad
  Dim MischGroup As HandleRezGroup
  Dim ReUn As String
  Dim SmpID(1) As Integer
  Dim UntID(1) As Integer
  Dim TypID(1) As Integer
  Dim UuuID(1) As Integer
  Dim WinHilf As AngGeos

  Sub PlottRwert(ByVal keymenge As String)
    Dim i As Integer
    Dim kw As Integer
    Dim j As Integer
    Dim Reun As String = ""
    'Plott
    '
    '

    For i = 0 To radKWB.Count - 1
      If radKWB(i).Checked Then
        Select Case i
          Case 0, 1
            j = i
            Reun = "A"
            Exit For
            '
            'Schicht über weiß/schwarz
            '
          Case 2, 3
            j = i - 2
            Reun = "U"
            Exit For
            '
            'Untergrund weiß/schwarz
            '

        End Select
      End If
    Next i


    For kw = 0 To MenueParam.Messg.Winkel.Km - 1
      RezGraphics.PlotRwerte(kw).RefKurv.clear()
      RezGraphics.PlotRwerte(kw).Iplott = False
      If GrpRwerte(Reun)(j).IVoNa = True Then
        RezGraphics.PlotRwerte(kw).RefKurv.Add(WinHilf(0).Chrm, GrpRwerte(Reun)(j).RefKurv(MenueParam.Messg.Winkel(kw).Chrm))
        RezGraphics.PlotRwerte(MenueParam.Messg.Winkel(kw).Chrm).Iplott = True
        RezGraphics.PlotRwerte(MenueParam.Messg.Winkel(kw).Chrm).IVoNa = True
      End If
    Next kw
    '


    RezGraphics.Text = GrpRwerte(Reun)(j).Name

    RezGraphics.Rmin = -1
    RezGraphics.Rmax = -1
    PicAufBau.Refresh()
    ''
  End Sub

  Private Sub frmColorthekEing_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    RemoveHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID

  End Sub
  Private Sub frmColorthekEing_VisibleChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.VisibleChanged
    ConnRezepte.CurrencyManager.EndCurrentEdit()
    ConnRezepte.CurrencyManager.Refresh()
  End Sub
  Private Sub frmColorthekEing_DeactivateClosing(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate, Me.FormClosing
    AufbauPar.MethID = -1
  End Sub


  Private Sub frmColorthekEing_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim kw As Integer
    Dim ier As Integer
    Me.Text = Texxt(1851)

    btnREF = New List(Of Button)
    btnUNT = New List(Of Button)
    ChkKWB = New List(Of CheckBox)
    radKWB = New List(Of RadioButton)
    radVONA = New List(Of RadioButton)
    txtCOL = New List(Of TextBox)
    btnREF.Clear()
    btnREF.Add(btnRef0)
    btnREF.Add(btnRef1)
    btnUNT.Clear()
    btnUNT.Add(btnUnt0)
    btnUNT.Add(btnUnt1)

    ChkKWB.Clear()
    ChkKWB.Add(chkRef0)
    ChkKWB.Add(chkRef1)
    ChkKWB.Add(chkUnt0)
    ChkKWB.Add(chkUnt1)
    radKWB.Clear()
    radKWB.Add(radREF0)
    radKWB.Add(radREF1)
    radKWB.Add(radUNT0)
    radKWB.Add(radUNT1)
    radVONA.Clear()
    radVONA.Add(radVoNa0)
    radVONA.Add(radVoNa1)
    txtCOL.Clear()
    txtCOL.Add(txtRezName)
    txtCOL.Add(txtRezBem)
    txtCOL.Add(txtDik0)
    txtCOL.Add(txtDik1)


    btnRef0.Text = Texxt(928)
    btnRef1.Text = Texxt(929)
    btnUnt0.Text = Texxt(808)
    btnUnt1.Text = Texxt(809)
    btnSUC.Text = Texxt(835)
    chkARC.Text = Texxt(374)
    chkUMR.Text = Texxt(444)
    chkVOL.Text = Texxt(830)
    lblCOL.Text = Texxt(837)
    lblDatum.Text = Texxt(375)
    lblDik0.Text = Texxt(936)
    lblDik1.Text = Texxt(937)
    lblGRP.Text = Texxt(386)
    lblMSH.Text = Texxt(422)
    lblRezName.Text = Texxt(931)
    lblRezBem.Text = Texxt(932)
    lblSuc.Text = Texxt(369)
    radREF0.Text = Texxt(921)
    radREF1.Text = Texxt(922)
    radUNT0.Text = Texxt(923)
    radUNT1.Text = Texxt(924)
    radVoNa0.Text = Texxt(449)
    radVoNa1.Text = Texxt(448)
    chkQuit.Text = Texxt(4667)
    '
    'Handler für R-Werte
    '
    AddHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
    MischGroup = New HandleRezGroup
    PicAufBau = New HandlePictures
    RezCheckRad = New HandleCheckRad
    RezGrid = New HandleRezC1Screen
    Rezsozpt = New RecipesGrp
    Calcrezept = New RezeptBerechnung
    RezGraphics = New HandleRezGrafik
    DatenFarb = New HandleRezDsetFarb
    ReWrRezept = New ReadWriteRezept
    ReWrRwert = New ReadWriteRwert
    GrpRwerte = New RefValuesGrp
    '
    KeyMenge = "NEU"
    Rezsozpt.Rezepte.AddRez(KeyMenge, New Recipe)
    Rezsozpt.Rezepte.AddRez("MNG", Rezsozpt.Rezepte(KeyMenge))
    Rezsozpt.Rezepte.kwb(0) = 0
    Rezsozpt.Rezepte.kwb(1) = 1
    RezGraphics.Vkwb(0) = 0
    RezGraphics.Vkwb(1) = 1
    MischGroup.cboGRP = cboGRP
    MischGroup.lblGRP = lblGRP
    RezGraphics.AllRezepte = Rezsozpt
    RezCheckRad.PicGraphic = RezGraphics
    RezCheckRad.Picauf = PicAufBau
    PicAufBau.PicGraphic = RezGraphics
    PicAufBau.Add("REF", picREF)
    RezGraphics.WeSc(0) = True
    RezGraphics.WeSc(1) = True
    RezGraphics.Kwop = 0
    '
    '
    GrpRwerte.Add("A", New RefValues)
    GrpRwerte.Add("U", New RefValues)
    GrpRwerte("A").Add(KeyRe(0), New RefValue)
    GrpRwerte("A").Add(KeyRe(1), New RefValue)
    GrpRwerte("U").Add(KeyRe(0), New RefValue)
    GrpRwerte("U").Add(KeyRe(1), New RefValue)
    GrpRwerte("A")(KeyRe(0)).kwb = 0
    GrpRwerte("A")(KeyRe(1)).kwb = 1
    GrpRwerte("U")(KeyRe(0)).kwb = 0
    GrpRwerte("U")(KeyRe(1)).kwb = 1
    For i = 0 To 1
      UntID(i) = -1
      TypID(i) = -1
    Next i
    'Tabelle für Rezepte
    '
    AdaptRezepte = New OleDbDataAdapter
    CmdRezepte = New OleDbCommand("", Cndat)
    AdaptRezepte.SelectCommand = CmdRezepte
    TabRezepte = New DataTable
    ViewDelRezepte = New DataView(TabRezepte)
    ViewDelRezepte.RowStateFilter = DataViewRowState.Deleted
    ViewAddRezepte = New DataView(TabRezepte)
    ViewAddRezepte.RowStateFilter = DataViewRowState.Added
    ViewModRezepte = New DataView(TabRezepte)
    ViewModRezepte.RowStateFilter = DataViewRowState.ModifiedCurrent

    viewhilf = New DataView(TabRezepte)
    ConnRezepte = New BindingSource
    ConnRezepte.DataSource = TabRezepte
    BindingNavigRezept.BindingSource = ConnRezepte
    cboCOL.DataSource = ConnRezepte
    TabRezepte.Columns.Add("MISCH_ID", GetType(Integer))
    TabRezepte.Columns.Add("REZEPT_ID", GetType(Integer))
    TabRezepte.Columns.Add("REZEPT_NAME", GetType(String))
    TabRezepte.Columns.Add("REZEPT_BEM", GetType(String))
    TabRezepte.Columns.Add("REZEPT_DIK1", GetType(Single))
    TabRezepte.Columns.Add("REZEPT_DIK2", GetType(Single))
    TabRezepte.Columns.Add("REZEPT_DATTIM", GetType(Date))
    TabRezepte.Columns.Add("REZEPT_GID", GetType(Integer))
    TabRezepte.Columns.Add("REZEPT_IARCH", GetType(Short))
    TabRezepte.Columns.Add("STATE", GetType(Boolean))
    txtRezID.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_ID")
    txtRezName.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_NAME")
    txtRezBem.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_BEM")
    txtDik0.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_DIK1")
    txtDik1.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_DIK2")
    txtDatValue.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_DATTIM", True, DataSourceUpdateMode.Never, "", "d")
    cboGRP.DataBindings.Add("SELECTEDVALUE", ConnRezepte, "REZEPT_GID")
    chkARC.DataBindings.Add("CHECKSTATE", ConnRezepte, "REZEPT_IARCH", True)
    chkRezRefState.DataBindings.Add("CHECKED", ConnRezepte, "STATE", True)


    ' btnSUC.PerformClick()

    Exit Sub









    PicAufBau.Add("REF", CType(picREF, PictureBox))
    RezGrid.AllRezepte = Rezsozpt
    RezGrid.DatenFarb = DatenFarb
    RezGrid.CalcRezept = Calcrezept
    RezGrid.Picauf = PicAufBau
    DatenFarb.DsetFarbCreate()
    '
    '
    '
    '
    RezGrid.TDBFar = TDBFar
    RezGrid.TDBDropFar = TDBDropFar
    RezGrid.TDBDropPre = TDBDropPre
    RezGrid.TDBDropPrb = TDBDropPrb
    RezGrid.TDBDropPro = TDBDropPro
    RezGrid.txtFooter = txtSum
    RezGrid.chkVOL = chkVOL
    ' 
    ' 
    ' 
    ' 
    '
    RezGrid.TDBFarGridRezept(ier)
    RezGrid.NewKeynam(KeyMenge)
    '
    '
    '
    'Zum Plotten
    '
    '
    '
    WinHilf = New AngGeos
    WinHilf.Add("XX", New AngGeo(0, 1, 1, "", 1.0, "XX"))
    WinHilf.Wsol = MenueParam.Messg.Winkel.Wsol
    WinHilf.Nwp = MenueParam.Messg.Winkel.Nwp
    RezGraphics.Winkel = WinHilf
    RezGraphics.kwopt(0) = True
    RezGraphics.WeSc(0) = True
    RezGraphics.WeSc(1) = True
    For kw = 0 To MenueParam.Messg.Winkel.Km - 1
      RezGraphics.PlotRwerte.Add(MenueParam.Messg.Winkel(kw).Chrm, New RefValue)
      RezGraphics.PlotRwerte(kw).Name = MenueParam.Messg.Winkel(kw).Chrm
    Next kw
    '

    '
    '
    '
    'Tabelle für Mischsysteme
    '
    AdaptMisch = New OleDbDataAdapter
    CmdMisch = New OleDbCommand("", Cncol)
    AdaptMisch.SelectCommand = CmdMisch
    TabMisch = New DataTable
    cboMSH.SelectedIndex = -1
    If MenueParam.MischID <> -1 Then
      AdaptMisch.SelectCommand.CommandText = MischSelectCommand(MenueParam)
      AdaptMisch.SelectCommand.Connection = Cncol()
      If Not FillDatset(AdaptMisch, TabMisch) Then
        Exit Sub
      End If

      cboMSH.DataSource = TabMisch
      cboMSH.DisplayMember = "MISCH_KBEZ"
      cboMSH.ValueMember = "MISCH_ID"
    Else
      MsgBox(Texxt(3601))
      Exit Sub
    End If
    cboMSH.SelectedIndex = -1
    cboMSH.Enabled = True

    cboMSH.SelectedValue = MenueParam.MischID
    '
    ' 
    '
    ''
  End Sub

  Private Sub GetNameID(ByVal ID As Integer, ByVal Name As String)
    Dim j As Integer
    Dim ier As Integer
    If Name <> "" Then
      '
      If GetPutReflex.HideSofort Then
        GetPutReflex.InVisible()
      End If
      'lblMES(RcmdRef).Caption = txtINP.text
      '
      If RcmdRef > 0 Then
        ReUn = "A"
        j = RcmdRef - 1
        ReWrRwert.ReadRwert(MenueParam, ID, GrpRwerte(ReUn)(KeyRe(j)), MenueParam.Messg.Winkel, ier)
        GrpRwerte(ReUn)(KeyRe(j)).IVoNa = True
      ElseIf RcmdRef < 0 Then
        j = -RcmdRef + 1
        ReUn = "U"
        ReWrRwert.ReadRwert(MenueParam, ID, GrpRwerte(ReUn)(KeyRe(j - 2)), MenueParam.Messg.Winkel, ier)
        GrpRwerte(ReUn)(KeyRe(j - 2)).IVoNa = True

      End If
      radKWB(j).Enabled = True
      radKWB(j).Checked = True
      ChkKWB(j).Enabled = True
      ChkKWB(j).Checked = True
      'RezGrid.TDBFarRezStart()
      Call PlottRwert(KeyMenge)

      Name = ""
      If Not IsNothing(ConnRezepte.Current) AndAlso ConnRezepte.Count > 0 Then
        chkRezRefState.Checked = Not ConnRezepte.Current.row("STATE")
      End If
    End If

  End Sub
  Private Sub frmColorthekEing_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize
    Call ResizeChild(Me)

  End Sub

  Private Sub chkKWB_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkRef0.Click, chkRef1.Click, chkUnt0.Click, chkUnt1.Click
    Dim BoVal As Boolean
    Dim index As Integer
    Select Case sender.name
      Case "chkRef0"
        index = 0
      Case "chkRef1"
        index = 1
      Case "chkUnt0"
        index = 2
      Case "chkUnt1"
        index = 3
    End Select
    BoVal = True
    If Not ChkKWB(index).Checked Then BoVal = False
    radKWB(index).Enabled = BoVal
    Select Case index
      Case 0, 1
        GrpRwerte("A")(index).IVoNa = BoVal
        If Not BoVal Then
          GrpRwerte("A")(index).ID = -1
          ChkKWB(index).Enabled = BoVal
        End If
      Case 2, 3
        GrpRwerte("U")(index - 2).IVoNa = BoVal
        If Not BoVal Then
          GrpRwerte("U")(index - 2).ID = -1
          ChkKWB(index - 2).Enabled = BoVal
        End If
    End Select
    If Not IsNothing(ConnRezepte.Current) AndAlso ConnRezepte.Count > 0 Then
      chkRezRefState.Checked = Not ConnRezepte.Current.row("STATE")
    End If
    Call PlottRwert(KeyMenge)
  End Sub






  Private Sub btnRef_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRef0.Click, btnRef1.Click
    Dim Index As Short
    Index = CInt(sender.name.substring(6, 1))
    '
    '
    RcmdRef = Index + 1
    GetPutReflex.Messrefel = GrpRwerte("A")(Index)
    GetPutReflex.Iarch = 0
    GetPutReflex.Captext = btnREF(Index).Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(Index), RezGraphics.Vkwb(Index), MenueParam.Messg.MeArtID)
    GetPutReflex.ReflexWerte(False)
    MenueParam.Messg.MeArtLock = BitInt(RezGraphics.Vkwb(Index), RezGraphics.Vkwb(Index), MenueParam.Messg.MeArtID)


  End Sub
  Private Sub btnSUC_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSUC.Click
    Dim SqlStmt As String
    Dim StrHilf As String
    Dim i As Integer
    Dim j As Integer
    'If cboGRP.SelectedIndex = -1 Then Exit Sub
    'If Not IsNumeric(cboGRP.SelectedValue) Then Exit Sub
    cboCOL.Enabled = False
    '
    'Defaultwerte
    '
    
    'If txtSUC.Text = "" Then
    StrHilf = ""
    'Else
    'StrHilf = " AND REZEPT_NAME LIKE '" & StrFil(txtSUC.Text, "'") & "'"
    'End If
    'If cboGRP.SelectedValue = 0 Then
    SqlStmt = "SELECT * FROM TBL_REZEPT WHERE MISCH_ID=" & MenueParam.MischID & StrHilf & " ORDER BY REZEPT_ID"
    'Else
    'SqlStmt = "SELECT * FROM TBL_REZEPT WHERE MISCH_ID=" & MenueParam.MischID & StrHilf & " AND REZEPT_GID=" & cboGRP.SelectedValue & " ORDER BY REZEPT_ID"
    'End If
    CmdRezepte.CommandText = SqlStmt
    TabRezepte.Clear()
    If Not FillDatset(AdaptRezepte, TabRezepte) Then Exit Sub
    'MsgBox(TabRezepte.Constraints(0).ConstraintName)
    TabRezepte.PrimaryKey(0).Unique = False
    TabRezepte.PrimaryKey(1).Unique = False
    TabRezepte.Columns("REZEPT_ID").Unique = False
    TabRezepte.PrimaryKey.Clear(WherePrimaryKEY, 0, 2)
    TabRezepte.Constraints.Clear()
    TabRezepte.AcceptChanges()
    '
    '
    '
    txtRezID.DataBindings.Clear()
    txtRezName.DataBindings.Clear()
    txtRezBem.DataBindings.Clear()
    txtDik0.DataBindings.Clear()
    txtDik1.DataBindings.Clear()
    txtDatValue.DataBindings.Clear()
    cboGRP.DataBindings.Clear()
    chkARC.DataBindings.Clear()
    chkRezRefState.DataBindings.Clear()
    
    txtRezName.MaxLength = TabRezepte.Columns("REZEPT_NAME").MaxLength
    txtRezBem.MaxLength = TabRezepte.Columns("REZEPT_BEM").MaxLength
    chkRezRefState.Visible = False
    txtRezID.Visible = False
    TabRezepte.Columns("REZEPT_NAME").DefaultValue = "???"
    TabRezepte.Columns("REZEPT_BEM").DefaultValue = "???"
    TabRezepte.Columns("REZEPT_DIK1").DefaultValue = MenueParam.Misch.Dicke
    TabRezepte.Columns("REZEPT_DIK2").DefaultValue = MenueParam.Misch.Dicke
    TabRezepte.Columns("REZEPT_DATTIM").DefaultValue = Date.Now
    TabRezepte.Columns("REZEPT_GID").DefaultValue = MenueParam.Misch.UserRzpGID
    TabRezepte.Columns("REZEPT_IARCH").DefaultValue = 0
    TabRezepte.Columns("STATE").DefaultValue = False
    chkRezRefState.Checked = False
    TabRezepte.Columns("REZEPT_ID").DefaultValue = -1
    Exit Sub

    
    For i = 0 To 1
      j = BitWrt(i, MenueParam.Messg.MeArtID)
      If j = 1 Then
        btnREF(i).Text = Texxt(3916 + i)
      End If
      btnUNT(i).Text = Texxt(355 + i) & Texxt(3918 + j)
    Next i
    cboCOL.DisplayMember = "REZEPT_NAME"
    cboCOL.ValueMember = "REZEPT_ID"
    '
    '
    cboCOL.SelectedIndex = -1
    If ConnRezepte.Count > 0 Then
      cboCOL.Enabled = True
      cboCOL.SelectedIndex = 0
    Else
      '
      'Defaultwerte
      '

      '
      '
      Rezsozpt.Rezepte(KeyMenge).Name = "???"
      Rezsozpt.Rezepte(KeyMenge).Bem = " "
      Rezsozpt.Rezepte(KeyMenge).Dicke(0) = MenueParam.Misch.Dicke
      Rezsozpt.Rezepte(KeyMenge).Dicke(1) = MenueParam.Misch.Dicke
      Rezsozpt.Rezepte(KeyMenge).Iarch = chkARC.CheckState
      Rezsozpt.IVOL = chkVOL.CheckState
      Rezsozpt.Rezepte(KeyMenge).Gid = MenueParam.Misch.UserRzpGID
      Rezsozpt.Rezepte(KeyMenge).DatTim = Now
      'MsgBox texxt(2971)
      Rezsozpt.Rezepte(KeyMenge).clear()
      RezGrid.TDBFarRezStart()
      cboCOL.Enabled = True
      cboCOL.Text = ""
      Call PutBackControlsValues()
    End If
  End Sub




  Private Sub radKWB_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles radREF0.Click, radREF1.Click, radUNT0.Click, radUNT1.Click
    Call PlottRwert(KeyMenge)
  End Sub

  Private Sub btnUnt_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnUnt0.Click, btnUnt1.Click
    Dim index As Integer
    index = CInt(sender.name.substring(6, 1))
    '
    '
    RcmdRef = -index - 1
    GetPutReflex.Messrefel = GrpRwerte("U")(index)
    GetPutReflex.Iarch = 0
    GetPutReflex.Captext = btnREF(index).Text
    GetPutReflex.Retr = BitInt(RezGraphics.Vkwb(index), RezGraphics.Vkwb(index), MenueParam.Messg.MeArtID)
    GetPutReflex.ReflexWerte(False)
    MenueParam.Messg.MeArtLock = BitInt(RezGraphics.Vkwb(index), RezGraphics.Vkwb(index), MenueParam.Messg.MeArtID)
  End Sub



  Private Sub cboMSH_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMSH.SelectedIndexChanged
    Dim i As Integer
    Dim j As Integer

    If cboMSH.SelectedIndex = -1 Then Exit Sub
    If Not IsNumeric(cboMSH.SelectedValue) Then Exit Sub
    If Not cboMSH.Enabled Then Exit Sub
    Cursor = Cursors.WaitCursor
    ConnRezepte.CurrencyManager.EndCurrentEdit()
    AufbauPar.MischID = cboMSH.SelectedValue
    DatenFarb.DsetFarbFill(True)
    UuuID(0) = -1
    UuuID(1) = -1
    '
    '
    '
    '
    'Gruppennamen
    '
    '
    '
    cboGRP.SelectedIndex = -1
    MischGroup.GroupList()
    'Application.DoEvents()
    '
    btnSUC.PerformClick()
    '
    Cursor = Cursors.Arrow
    'ConnRezepte.CurrencyManager.Refresh()

  End Sub


  Sub PutBackControlsValues()
    Dim i As Integer
    ClearRezepte(Rezsozpt)
    For i = 0 To RezGrid.AllRezepte.Rezepte.RezCount - 1
      RezGrid.AllRezepte.Rezepte(i).clear()
    Next i
    For i = 0 To ChkKWB.Count - 1
      ChkKWB(i).Checked = False
      ChkKWB(i).Enabled = False
      radKWB(i).Enabled = False
    Next i
    GrpRwerte("A")(KeyRe(0)).ID = -1
    GrpRwerte("A")(KeyRe(1)).ID = -1
    GrpRwerte("U")(KeyRe(0)).ID = -1
    GrpRwerte("U")(KeyRe(1)).ID = -1
    GrpRwerte("A")(KeyRe(0)).IVoNa = False
    GrpRwerte("A")(KeyRe(1)).IVoNa = False
    GrpRwerte("U")(KeyRe(0)).IVoNa = False
    GrpRwerte("U")(KeyRe(1)).IVoNa = False
  End Sub
  Private Sub chkARC_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkARC.CheckStateChanged
    If chkARC.CheckState <> CheckState.Indeterminate Then
      Rezsozpt.Rezepte(KeyMenge).Iarch = chkARC.CheckState
    End If
  End Sub


  Private Sub chkUMR_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkUMR.Click
    Dim ier As Integer
    If chkUMR.Checked Then
      KeyMenge = "MNG"
      RezGrid.NewKeynam(KeyMenge)
      RezGrid.TDBFarRezStart()


    Else
      KeyMenge = "NEU"
      RezGrid.NewKeynam(KeyMenge)
      RezGrid.TDBFarRezStart()

    End If
  End Sub


  '
  '
  '
  '
  '
  '
  '
  Private Sub ConnRezepte_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnRezepte.AddingNew
    Dim DefText As String
    Dim ier As Integer
    Debug.WriteLine("*******ConnRezepte_Addnew " & ViewModRezepte.Count & Space(2) & ViewAddRezepte.Count & Space(2) & ViewDelRezepte.Count & Space(2))
    Debug.WriteLine("Addnew ViewCount=" & ViewAddRezepte.Count)
    If IsNothing(ConnRezepte) Then Exit Sub
    If ConnRezepte.Position >= ConnRezepte.Count Then Exit Sub
    If Not IsNothing(ConnRezepte.Current) Then
      Call UpdateRezsozpt(ConnRezepte.Current.Row)
    End If
    '
    'Änderungen übernehmen oder verwerfen
    '
    '
    Call UpdateChanges()
    '
    'Defaultwerte für neues Rezept
    '
    '
    If ConnRezepte.Count = 0 Then
      TabRezepte.Columns("MISCH_ID").DefaultValue = MenueParam.MischID
      TabRezepte.Columns("REZEPT_NAME").DefaultValue = "???"
      TabRezepte.Columns("REZEPT_BEM").DefaultValue = "???"
      TabRezepte.Columns("REZEPT_DIK1").DefaultValue = MenueParam.Misch.Dicke
      TabRezepte.Columns("REZEPT_DIK2").DefaultValue = MenueParam.Misch.Dicke
      TabRezepte.Columns("REZEPT_DATTIM").DefaultValue = Date.Now
      TabRezepte.Columns("REZEPT_GID").DefaultValue = MenueParam.Misch.UserRzpGID
      TabRezepte.Columns("REZEPT_IARCH").DefaultValue = 0
    Else
      TabRezepte.Columns("MISCH_ID").DefaultValue = MenueParam.MischID
      '
      '
      DefText = Rezsozpt.Rezepte(KeyMenge).Name & "???"
      If DefText.Length > txtRezName.MaxLength Then
        DefText = DefText.Substring(0, txtRezName.MaxLength)
      End If
      TabRezepte.Columns("REZEPT_NAME").DefaultValue = DefText
      DefText = Rezsozpt.Rezepte(KeyMenge).Bem
      If DefText.Length > txtRezBem.MaxLength Then
        DefText = DefText.Substring(0, txtRezBem.MaxLength)
      End If
      TabRezepte.Columns("REZEPT_BEM").DefaultValue = DefText
      TabRezepte.Columns("REZEPT_DIK1").DefaultValue = Rezsozpt.Rezepte(KeyMenge).Dicke(0)
      TabRezepte.Columns("REZEPT_DIK2").DefaultValue = Rezsozpt.Rezepte(KeyMenge).Dicke(1)
      '
      '
      'TabRezepte.Columns("REZEPT_NAME").DefaultValue = txtRezName.Text & "???"
      'TabRezepte.Columns("REZEPT_BEM").DefaultValue = txtRezBem.Text
      'TabRezepte.Columns("REZEPT_DIK1").DefaultValue = txtDik0.Text
      'TabRezepte.Columns("REZEPT_DIK2").DefaultValue = txtDik1.Text
      TabRezepte.Columns("REZEPT_DATTIM").DefaultValue = Date.Now
      TabRezepte.Columns("REZEPT_GID").DefaultValue = Rezsozpt.Rezepte(KeyMenge).Gid
      TabRezepte.Columns("REZEPT_IARCH").DefaultValue = Rezsozpt.Rezepte(KeyMenge).Iarch
    End If
    TabRezepte.Columns("STATE").DefaultValue = False
    'Debug.WriteLine("letz " & TabRezepte.Rows(TabRezepte.Rows.Count - 1)("REZEPT_ID"))
    TabRezepte.Columns("REZEPT_ID").DefaultValue = MaxTabID("TBL_REZEPT", "REZEPT_ID", "MISCH_ID", MenueParam.MischID, Cndat) + 1
    TabRezepte.Columns("REZEPT_BEM").DefaultValue = MaxTabID("TBL_REZEPT", "REZEPT_ID", "MISCH_ID", MenueParam.MischID, Cndat) + 1
    TabRezepte.Columns("REZEPT_ID").DefaultValue = MaxTblID(TabRezepte) + 1
    TabRezepte.Columns("REZEPT_BEM").DefaultValue = TabRezepte.Columns("REZEPT_ID").DefaultValue
    If Not IsNothing(ConnRezepte.Current) AndAlso ConnRezepte.Count > 0 Then
      Debug.WriteLine("ADDNEW Maxid= " & TabRezepte.Columns("REZEPT_ID").DefaultValue & "curr ID=" & ConnRezepte.Current("REZEPT_ID") & " State=" & ConnRezepte.Current.row.rowstate)
    End If
    Debug.WriteLine("Addnew exist=" & TabRezepte.Select("REZEPT_ID=" & TabRezepte.Columns("REZEPT_ID").DefaultValue, Nothing, Nothing).Length)
  End Sub
  Private Sub ConnRezepte_CurrentItemChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnRezepte.CurrentItemChanged
    If IsNothing(ConnRezepte.Current) Then Exit Sub
    Dim i As Integer
    Debug.WriteLine("*******ConnRezepte_CurrentItemChanged " & ViewModRezepte.Count & Space(2) & ViewAddRezepte.Count & Space(2) & ViewDelRezepte.Count & Space(2))
    Debug.WriteLine("Currit exist=" & TabRezepte.Select("REZEPT_ID=" & ConnRezepte.Current("REZEPT_ID"), Nothing, Nothing).Length)
    For i = 0 To TabRezepte.Select("REZEPT_ID=" & ConnRezepte.Current("REZEPT_ID"), Nothing, Nothing).Length - 1
      Debug.WriteLine("Select " & TabRezepte.Select("REZEPT_ID=" & ConnRezepte.Current("REZEPT_ID"), Nothing, Nothing)(i)("REZEPT_ID") & "STATE " & TabRezepte.Select("REZEPT_ID=" & ConnRezepte.Current("REZEPT_ID"), Nothing, Nothing)(i).RowState)

    Next i
    Debug.WriteLine("curr " & ConnRezepte.Current("REZEPT_ID") & " State=" & ConnRezepte.Current.row.rowstate)
    Exit Sub
    If Not IsNothing(ConnRezepte.Current) Then
      Call UpdateRezsozpt(ConnRezepte.Current.Row)
    End If
    viewhilf.RowFilter = "REZEPT_ID=" & ConnRezepte.Current("REZEPT_ID")
    viewhilf.RowStateFilter = DataViewRowState.Added
    If viewhilf.Count > 0 Then
      MsgBox("Added Count " & viewhilf.Count)
    End If
    viewhilf.RowStateFilter = DataViewRowState.CurrentRows
    If viewhilf.Count > 0 Then
      MsgBox("CurrentRows Count " & viewhilf.Count)
    End If
    viewhilf.RowStateFilter = DataViewRowState.CurrentRows
    If viewhilf.Count > 0 Then
      MsgBox("CurrentRows Count " & viewhilf.Count)
    End If
    viewhilf.RowStateFilter = DataViewRowState.Deleted
    If viewhilf.Count > 0 Then
      MsgBox("Deleted Count " & viewhilf.Count)
    End If
    viewhilf.RowStateFilter = DataViewRowState.ModifiedCurrent
    If viewhilf.Count > 0 Then
      MsgBox("ModifiedCurrent Count " & viewhilf.Count)
    End If
    viewhilf.RowStateFilter = DataViewRowState.ModifiedOriginal
    If viewhilf.Count > 0 Then
      MsgBox("ModifiedOriginal  Count " & viewhilf.Count)
    End If
    viewhilf.RowStateFilter = DataViewRowState.OriginalRows
    If viewhilf.Count > 0 Then
      MsgBox("OriginalRows  Count " & viewhilf.Count)
    End If
    viewhilf.RowStateFilter = DataViewRowState.None
    If viewhilf.Count > 0 Then
      MsgBox("None  Count " & viewhilf.Count)
    End If
    viewhilf.RowStateFilter = DataViewRowState.Unchanged
    If viewhilf.Count > 0 Then
      MsgBox("Unchanged  Count " & viewhilf.Count)
    End If
  End Sub
  Private Sub ConnRezepte_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnRezepte.CurrentChanged
    Dim ier As Integer
    Dim i As Integer
    Dim j As Integer
    If TabRezepte.Rows.Count = 0 Then
      If Not IsNothing(PicAufBau) Then
        PicAufBau.PicRwert.Hide()
      Else
        Exit Sub
      End If
    End If
    Debug.WriteLine("******ConnRezepte_CurrentChanged " & ViewModRezepte.Count & Space(2) & ViewAddRezepte.Count & Space(2) & ViewDelRezepte.Count & Space(2))
    If Not IsNothing(ConnRezepte.Current) Then
      Debug.WriteLine("Curr RezID=" & ConnRezepte.Current("REZEPT_ID") & "State=" & ConnRezepte.Current.row.rowstate)
    End If
    BindingNavigRezept.Enabled = False
    '
    '
    'Änderungen übernehmen oder verwerfen
    '
    '
    '
    '
    Call UpdateChanges()

    '
    'Neues Rezsozpt aufbauen(einlesen)
    '
    '

    If Not IsNothing(ConnRezepte.Current) Then
      '
      '
      If ConnRezepte.Current.row.rowstate = DataRowState.Detached Then
        Debug.WriteLine("Det RezID=" & TabRezepte.Columns("REZEPT_ID").DefaultValue)
        GrpRwerte("A")(0).IVoNa = False
        GrpRwerte("A")(1).IVoNa = False
        GrpRwerte("A")(0).ID = -1
        GrpRwerte("A")(1).ID = -1
        ChkKWB(0).Checked = False
        ChkKWB(1).Checked = False
        ChkKWB(0).Enabled = False
        ChkKWB(1).Enabled = False
        radKWB(0).Enabled = False
        radKWB(1).Enabled = False
        'Call PlottRwert(KeyMenge)
        txtRezName.BackColor = Color.LightGray
        '
        '
        'Rezept einlesen
        '
      ElseIf ConnRezepte.Current.row.rowstate = DataRowState.Unchanged Then
        BindingNavigRezept.Enabled = True
        Exit Sub
        If ConnRezepte.Count <> 0 Then
          '
          '
          txtRezName.BackColor = Color.White

          '
          '
          'Aktuelles Rezept von  nach REZSOZPT übernehmen (Prüfen, ob z.B. Rezeptname mit Name in Tabelle (Connrezepte.current.row übereinstimmt)
          '
          '

          Cursor = Cursors.WaitCursor
          chkUMR.Checked = False

          'Call PutBackControlsValues()



          RzID = CInt(ConnRezepte.Current.row("REZEPT_ID"))
          If RzID = -1 Then
            MsgBox("ERROR Rezept_ID" & Space(2) & RzID)
            BindingNavigRezept.Enabled = True
            Cursor = Cursors.Arrow
            Exit Sub
          End If
          ReWrRezept.ReadRezeptFarbGrund(MenueParam, KeyMenge, RzID, Rezsozpt, UntID, TypID, SmpID, ier)
          '
          'Uchanged
          '
          '
          For i = 0 To 1
            If UntID(i) > 0 Then UuuID(i) = UntID(i)
          Next i
          '
          GrpRwerte("U")(KeyRe(0)).ID = UntID(0)
          GrpRwerte("U")(KeyRe(1)).ID = UntID(1)
          If radVONA(0).Checked Then
            GrpRwerte("A")(KeyRe(0)).ID = SmpID(0)
            GrpRwerte("A")(KeyRe(1)).ID = SmpID(1)
            TypID(0) = -1
            TypID(1) = -1
          Else
            GrpRwerte("A")(KeyRe(0)).ID = TypID(0)
            GrpRwerte("A")(KeyRe(1)).ID = TypID(1)
            SmpID(1) = -1
            SmpID(2) = -1
          End If
          For j = 0 To GrpRwerte.Count - 1
            For i = 0 To GrpRwerte(j).Count - 1
              If GrpRwerte(j)(i).ID >= 0 Then
                ReWrRwert.ReadRwert(MenueParam, GrpRwerte(j)(i).ID, GrpRwerte(j)(i), MenueParam.Messg.Winkel, ier)
                GrpRwerte(j)(i).IVoNa = True
                If ier = 0 Then
                  ChkKWB(i + 2 * j).Checked = True
                  ChkKWB(i + 2 * j).Enabled = True
                  radKWB(i + 2 * j).Enabled = True
                End If
              End If
            Next i
          Next j
          '
          '
          'ID=-1 für Untergründe zurücksetzen falls Untergrund bei früheren Rezepten vorhanden
          '
          '
          For i = 0 To 1
            If UuuID(i) > 0 And Not GrpRwerte("U")(i).IVoNa Then
              GrpRwerte("U")(i).ID = UuuID(i)
              GrpRwerte("U")(i).IVoNa = True
              ChkKWB(i + 2).Enabled = True
              ChkKWB(i + 2).Checked = True
              radKWB(i + 2).Enabled = True
            End If
          Next i
          '
          '
          '
          'Aufbau Grid
          '
          '
          RezGrid.TDBFarRezStart()
          '
          Call PlottRwert(KeyMenge)
          '
          '
          '
          '
          '
          RezGrid.chkVOL.CheckState = Rezsozpt.IVOL
          Cursor = Cursors.Arrow
        End If
      End If
    End If
    BindingNavigRezept.Enabled = True
  End Sub
  Private Sub ConnRezepte_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.BindingManagerDataErrorEventArgs) Handles ConnRezepte.DataError
    'MsgBox(e.Exception.Message)
  End Sub
  Sub UpdateChanges()
    Dim DialogUpdate As DialogResult
    Dim ier As Integer

    If ViewModRezepte.Count > 0 Then

      If ViewModRezepte.Count > 1 Then
        MsgBox("Zu viele Sätze gefunden " & ViewModRezepte.Count)
      End If
      '
      'Modified
      'Änderungen abspeichern
      '
      RzID = ViewModRezepte(0)("REZEPT_ID")
      DialogUpdate = DialogResult.No
      Debug.WriteLine("VIEWMOD Count " & ViewModRezepte.Count & "V-ID " & ViewModRezepte(0)("REZEPT_ID") & "R-ID " & Rezsozpt.Rezepte(KeyMenge).ID)
      If CheckUpdate(ViewModRezepte(0).Row) Then
        If Not chkQuit.Checked OrElse MessageBox.Show(Texxt(5001) & "? " & RzID, Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then

          RzID = ViewModRezepte(0)("REZEPT_ID")
          If RzID <> ViewModRezepte(0)("REZEPT_ID") Then
            MsgBox("Falsche ID R-ID=" & RzID & " V-ID=" & ViewModRezepte(0)("REZEPT_ID"))
          End If
          'ReWrRezept.UpdateRezept(MenueParam, KeyMenge, RzID, Rezsozpt, UntID, TypID, SmpID, ier)
          DialogUpdate = DialogResult.Yes
          '
          '
          Debug.WriteLine("Mod RzID=" & RzID)
          TabRezepte.Columns("STATE").DefaultValue = False
          chkRezRefState.Checked = False
          '
          '
          ' 
        End If
      End If
      '
      '
      'Accept oder reject für modified
      '
      '
      If DialogUpdate = DialogResult.Yes Then
        ViewModRezepte(0).Row.AcceptChanges()
      Else
        ViewModRezepte(0).Row.RejectChanges()
      End If
    End If
    '
    '
    If ViewAddRezepte.Count > 0 Then
      If ViewAddRezepte.Count > 1 Then
        MsgBox("Zu viele Sätze gefunden " & ViewAddRezepte.Count)
      End If
      '
      ''
      'Added
      '
      '
      '
      'Neues Rezept speichern
      '
      DialogUpdate = DialogResult.No
      Debug.WriteLine("VIEWADD Count " & ViewAddRezepte.Count & "V-ID " & ViewAddRezepte(0)("REZEPT_ID") & "R-ID " & Rezsozpt.Rezepte(KeyMenge).ID)


      If CheckUpdate(ViewAddRezepte(0).Row) Then


        '
        '
        RzID = ViewAddRezepte(0)("REZEPT_ID")
        If Not chkQuit.Checked OrElse MessageBox.Show(Texxt(5002) & "? " & RzID, Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
          'ReWrRezept.UpdateRezept(MenueParam, KeyMenge, RzID, Rezsozpt, UntID, TypID, SmpID, ier)
          'Call UpdateRezsozpt(ConnRezepte.Current.row)
          'RzID = -1
          'ReWrRezept.AddRezept(MenueParam, KeyMenge, RzID, Rezsozpt, UntID, TypID, SmpID, ier)
          DialogUpdate = DialogResult.Yes
          'Rezsozpt.Rezepte(KeyMenge).ID = RzID
          If RzID <> ViewAddRezepte(0)("REZEPT_ID") Then
            MsgBox("Falsche ID R-ID=" & RzID & " V-ID=" & ViewAddRezepte(0)("REZEPT_ID"))
          End If
          Debug.WriteLine("Add RzID=" & RzID)
          '
          TabRezepte.Columns("STATE").DefaultValue = False
          chkRezRefState.Checked = False
          '
          '
          TabRezepte.Columns("STATE").DefaultValue = False
          chkRezRefState.Checked = False
        End If
      End If
      '
      '
      'Accept oder reject für added
      '
      '
      If DialogUpdate = DialogResult.Yes Then
        ViewAddRezepte(0).Row.AcceptChanges()
      Else
        ViewAddRezepte(0).Row.RejectChanges()
      End If
    End If '
    '
    '
    'Deleted
    '
    '
    If ViewDelRezepte.Count > 0 Then
      If ViewDelRezepte.Count > 1 Then
        MsgBox("Zu viele Sätze gefunden " & ViewDelRezepte.Count)
      End If
      '
      '
      DialogUpdate = DialogResult.No
      RzID = ViewDelRezepte(0)("REZEPT_ID")
      Debug.WriteLine("VIEWDEL Count " & ViewDelRezepte.Count & "V-ID " & ViewDelRezepte(0)("REZEPT_ID") & "R-ID " & Rezsozpt.Rezepte(KeyMenge).ID)
      If Not chkQuit.Checked OrElse MessageBox.Show(Texxt(5003) & "? " & RzID, Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
        RzID = ViewDelRezepte(0)("REZEPT_ID")
        If ViewDelRezepte(0)("REZEPT_ID") <> RzID Then
          MsgBox("Falsche ID R-ID=" & RzID & " V-ID=" & ViewDelRezepte(0)("REZEPT_ID"))
        End If
        '
        'ReWrRezept.DelRezept(MenueParam, RzID, ier)
        Debug.WriteLine("Del RezID=" & RzID)
        DialogUpdate = DialogResult.Yes
      End If
      '
      '
      'Accept oder reject für delete
      '
      '
      If DialogUpdate = DialogResult.Yes Then
        Debug.WriteLine("DelCount vor" & TabRezepte.Rows.Count)
        Debug.WriteLine("Getchanges Count" & TabRezepte.GetChanges.Rows.Count)
        ViewDelRezepte(0).Row.AcceptChanges()
        Debug.WriteLine("DelCount nach" & TabRezepte.Rows.Count)

      Else
        ViewDelRezepte(0).Row.RejectChanges()
      End If
    End If
    Application.DoEvents() ''
  End Sub

  Sub UpdateRezsozpt(ByVal RezeptRow As DataRow)
    Dim i As Integer
    '
    '
    '
    'geänderte Werte übernehmen
    '
    '
    '
    '
    '
    Rezsozpt.Rezepte(KeyMenge).ID = RezeptRow("REZEPT_ID")
    Rezsozpt.Rezepte(KeyMenge).Name = RezeptRow("REZEPT_NAME")
    Rezsozpt.Rezepte(KeyMenge).Bem = RezeptRow("REZEPT_BEM")
    Rezsozpt.Rezepte(KeyMenge).Dicke(0) = RezeptRow("REZEPT_DIK1")
    Rezsozpt.Rezepte(KeyMenge).Dicke(1) = RezeptRow("REZEPT_DIK2")
    Rezsozpt.Rezepte(KeyMenge).Gid = cboGRP.SelectedValue
    Rezsozpt.Rezepte(KeyMenge).Iarch = chkARC.CheckState
    Rezsozpt.IVOL = chkVOL.CheckState
    For i = 0 To 1
      UntID(i) = -1
      TypID(i) = -1
      SmpID(i) = -1
    Next
    UntID(0) = GrpRwerte("U")(0).ID
    UntID(1) = GrpRwerte("U")(1).ID
    If radVONA(0).Checked Then
      SmpID(0) = GrpRwerte("A")(KeyRe(0)).ID
      SmpID(1) = GrpRwerte("A")(KeyRe(1)).ID
    Else
      TypID(0) = GrpRwerte("A")(KeyRe(0)).ID
      TypID(1) = GrpRwerte("A")(KeyRe(1)).ID
    End If
  End Sub

  Function CheckUpdate(ByVal RezeptRow As DataRow) As Boolean
    Dim i As Integer
    Dim imsg As DialogResult
    ''
    ' Fehlermeldungen
    '
    '
    CheckUpdate = True
    Exit Function
    If chkQuit.Checked Then
      For i = 0 To 1
        If Rezsozpt.Rezepte(KeyMenge).Dicke(i) = 0 And (GrpRwerte("A")(i).ID >= 0 And GrpRwerte("A")(i).IVoNa) Then
          imsg = MessageBox.Show(Texxt(2601), Texxt(2000), MessageBoxButtons.OK)
          Exit Function
        End If
      Next i
      For i = 0 To 1
        If Rezsozpt.Rezepte(KeyMenge).Dicke(i) > 0 And (GrpRwerte("A")(i).ID < 0 Or Not GrpRwerte("A")(i).IVoNa) Then
          imsg = MessageBox.Show(Texxt(2602), Texxt(2000), MessageBoxButtons.YesNo)
          If imsg = DialogResult.No Then
            RezeptRow.RejectChanges()
            CheckUpdate = False
            Exit Function
          Else
            Exit For
          End If
        End If
      Next i
      '
      '
      For i = 0 To 1
        If UntID(i) >= 0 And GrpRwerte("A")(i).ID < 0 Then
          imsg = MessageBox.Show(Texxt(2603), Texxt(2000), MessageBoxButtons.YesNo)
          If imsg = DialogResult.No Then
            RezeptRow.RejectChanges()
            CheckUpdate = False
            Exit Function
          Else
            Exit For
          End If
        End If
      Next i
      'If Not MeldSpeiAllRzp(cboGRP.SelectedValue, cboGRP.Text) Then
      'TabRezepte.Rows(ConnRezepte.Position).RejectChanges()
      'Exit Sub
      'End If
    End If
    ''

  End Function
  Private Sub ConnRezepte_CurrentChangedAlt(ByVal sender As Object, ByVal e As System.EventArgs)
    Dim ier As Integer
    Dim i As Integer
    Dim j As Integer
    Dim imsg As DialogResult
    '
    If TabRezepte.Rows.Count = 0 Then
      If Not IsNothing(PicAufBau) Then
        PicAufBau.PicRwert.Hide()
      End If
      Exit Sub
    End If

    If Rezsozpt.Rezepte(KeyMenge).Nr > 1 Then
      '
      '
      '
      'Abspeichern
      '
      '
      '
      '
      ' Fehlermeldungen
      '
      '
      If chkQuit.Checked Then
        For i = 0 To 1
          If Rezsozpt.Rezepte(KeyMenge).Dicke(i) = 0 And (GrpRwerte("A")(i).ID >= 0 And GrpRwerte("A")(i).IVoNa) Then
            imsg = MessageBox.Show(Texxt(2601), Texxt(2000), MessageBoxButtons.OK)
            TabRezepte.RejectChanges()
            Exit Sub
          End If
        Next i
        For i = 0 To 1
          If Rezsozpt.Rezepte(KeyMenge).Dicke(i) > 0 And (GrpRwerte("A")(i).ID < 0 Or Not GrpRwerte("A")(i).IVoNa) Then
            imsg = MessageBox.Show(Texxt(2602), Texxt(2000), MessageBoxButtons.YesNo)
            If imsg = DialogResult.No Then
              TabRezepte.RejectChanges()
              Exit Sub
            Else
              Exit For
            End If
          End If
        Next i
        '
        '
        For i = 0 To 1
          If UntID(i) >= 0 And GrpRwerte("A")(i).ID < 0 Then
            imsg = MessageBox.Show(Texxt(2603), Texxt(2000), MessageBoxButtons.YesNo)
            If imsg = DialogResult.No Then
              TabRezepte.RejectChanges()
              Exit Sub
            Else
              Exit For
            End If
          End If
        Next i
        'If Not MeldSpeiAllRzp(cboGRP.SelectedValue, cboGRP.Text) Then
        'TabRezepte.Rows(ConnRezepte.Position).RejectChanges()
        'Exit Sub
        'End If
      End If
      If Rezsozpt.Rezepte(KeyMenge).Nr = 16 Then
        '
        'Modified
        'Änderungen abspeichern
        '
        If Not chkQuit.Checked OrElse MessageBox.Show(Texxt(5001) & "?", Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then

          RzID = Rezsozpt.Rezepte(KeyMenge).ID
          ReWrRezept.UpdateRezept(MenueParam, KeyMenge, RzID, Rezsozpt, UntID, TypID, SmpID, ier)
          '
          '
          TabRezepte.Columns("STATE").DefaultValue = False
          chkRezRefState.Checked = False
          Rezsozpt.Rezepte(KeyMenge).Nr = 2
          '
          '
          'Führt zu Fehlern
          '
          '
          ConnRezepte.Current.row.acceptchanges()
        Else
          Rezsozpt.Rezepte(KeyMenge).Nr = 2
          ConnRezepte.Current.row.rejectchanges()
        End If
      ElseIf Rezsozpt.Rezepte(KeyMenge).Nr = 4 Then
        '
        'Added
        '
        '
        '
        'Neues Rezept speichern
        '
        '
        If Not chkQuit.Checked OrElse MessageBox.Show(Texxt(5002) & "?", Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then

          RzID = -1
          ReWrRezept.AddRezept(MenueParam, KeyMenge, RzID, Rezsozpt, UntID, TypID, SmpID, ier)
          Rezsozpt.Rezepte(KeyMenge).ID = RzID
          'ConnRezepte.Current.row("REZEPT_ID") = RzID

          '
          '
          TabRezepte.Columns("STATE").DefaultValue = False
          chkRezRefState.Checked = False
          ConnRezepte.Current.row.acceptchanges()
          Rezsozpt.Rezepte(KeyMenge).Nr = 2
        Else
          Rezsozpt.Rezepte(KeyMenge).Nr = 2
          ConnRezepte.Current.row.rejectchanges()

        End If
      ElseIf Rezsozpt.Rezepte(KeyMenge).Nr = 8 Then

      End If
    End If

    If IsNothing(ConnRezepte.Current) Then Exit Sub
    If ConnRezepte.Count = 0 Then Exit Sub
    If ConnRezepte.Current.row.rowstate = DataRowState.Unchanged Then
      txtRezName.BackColor = Color.White

      BindingNavigRezept.Enabled = False
      '
      '
      'Aktuelles Rezept von  nach REZSOZPT übernehmen (Prüfen, ob z.B. Rezeptname mit Name in Tabelle (Connrezepte.current.row übereinstimmt)
      '
      '

      Cursor = Cursors.WaitCursor
      chkUMR.Checked = False

      Call PutBackControlsValues()



      RzID = CInt(ConnRezepte.Current.row("REZEPT_ID"))
      If RzID = -1 Then
        MsgBox("ERROR Rezept_ID" & Space(2) & RzID)
        BindingNavigRezept.Enabled = True
        Cursor = Cursors.Arrow

        Exit Sub
      End If
      ReWrRezept.ReadRezeptFarbGrund(MenueParam, KeyMenge, RzID, Rezsozpt, UntID, TypID, SmpID, ier)
      '
      'Uchanged
      '
      '
      Rezsozpt.Rezepte(KeyMenge).Nr = 2
      For i = 0 To 1
        If UntID(i) > 0 Then UuuID(i) = UntID(i)
      Next i
      '
      GrpRwerte("U")(KeyRe(0)).ID = UntID(0)
      GrpRwerte("U")(KeyRe(1)).ID = UntID(1)
      If radVONA(0).Checked Then
        GrpRwerte("A")(KeyRe(0)).ID = SmpID(0)
        GrpRwerte("A")(KeyRe(1)).ID = SmpID(1)
        TypID(0) = -1
        TypID(1) = -1
      Else
        GrpRwerte("A")(KeyRe(0)).ID = TypID(0)
        GrpRwerte("A")(KeyRe(1)).ID = TypID(1)
        SmpID(1) = -1
        SmpID(2) = -1
      End If
      For j = 0 To GrpRwerte.Count - 1
        For i = 0 To GrpRwerte(j).Count - 1
          If GrpRwerte(j)(i).ID >= 0 Then
            ReWrRwert.ReadRwert(MenueParam, GrpRwerte(j)(i).ID, GrpRwerte(j)(i), MenueParam.Messg.Winkel, ier)
            GrpRwerte(j)(i).IVoNa = True
            If ier = 0 Then
              ChkKWB(i + 2 * j).Checked = True
              ChkKWB(i + 2 * j).Enabled = True
              radKWB(i + 2 * j).Enabled = True
            End If
          End If
        Next i
      Next j
      '
      '
      'ID=-1 für Untergründe zurücksetzen falls Untergrund bei früheren Rezepten vorhanden
      '
      '
      For i = 0 To 1
        If UuuID(i) > 0 And Not GrpRwerte("U")(i).IVoNa Then
          GrpRwerte("U")(i).ID = UuuID(i)
          GrpRwerte("U")(i).IVoNa = True
          ChkKWB(i + 2).Enabled = True
          ChkKWB(i + 2).Checked = True
          radKWB(i + 2).Enabled = True
        End If
      Next i
      '
      '
      '
      'Aufbau Grid
      '
      '
      RezGrid.TDBFarRezStart()
      '
      Call PlottRwert(KeyMenge)
      '
      '
      '
      '
      '
      RezGrid.chkVOL.CheckState = Rezsozpt.IVOL
      Cursor = Cursors.Arrow

      BindingNavigRezept.Enabled = True
    End If
    TabRezepte.AcceptChanges()
  End Sub

  Private Sub ConnRezepte_CurrentItemChangedAlt(ByVal sender As Object, ByVal e As System.EventArgs)

    Dim i As Integer
    Dim ier As Integer
    If IsNothing(ConnRezepte) Then Exit Sub
    If ConnRezepte.Position < 0 Then Exit Sub
    If IsNothing(ConnRezepte.Current) Then Exit Sub
    If ConnRezepte.Count = 0 Then Exit Sub
    TDBFar.EditActive = True
    TDBFar.EditActive = False

    If ConnRezepte.Current.row.rowstate = DataRowState.Modified Or ConnRezepte.Current.row.rowstate = DataRowState.Added Then
      Rezsozpt.Rezepte(KeyMenge).ID = CInt(txtRezID.Text)
      Rezsozpt.Rezepte(KeyMenge).Name = txtRezName.Text
      Rezsozpt.Rezepte(KeyMenge).Bem = txtRezBem.Text
      Rezsozpt.Rezepte(KeyMenge).Dicke(0) = CSng(txtDik0.Text)
      Rezsozpt.Rezepte(KeyMenge).Dicke(1) = CSng(txtDik1.Text)
      Rezsozpt.Rezepte(KeyMenge).Gid = cboGRP.SelectedValue
      Rezsozpt.Rezepte(KeyMenge).Iarch = chkARC.CheckState
      Rezsozpt.IVOL = chkVOL.CheckState
      Rezsozpt.Rezepte(KeyMenge).Name = txtRezName.Text
      For i = 0 To 1
        UntID(i) = -1
        TypID(i) = -1
        SmpID(i) = -1
      Next
      UntID(0) = GrpRwerte("U")(0).ID
      UntID(1) = GrpRwerte("U")(1).ID
      If radVONA(0).Checked Then
        SmpID(0) = GrpRwerte("A")(KeyRe(0)).ID
        SmpID(1) = GrpRwerte("A")(KeyRe(1)).ID
      Else
        TypID(0) = GrpRwerte("A")(KeyRe(0)).ID
        TypID(1) = GrpRwerte("A")(KeyRe(1)).ID
      End If
      '

    End If
    If ConnRezepte.Current.row.rowstate = DataRowState.Detached Then
      '
      'Detached
      '
      Rezsozpt.Rezepte(KeyMenge).Nr = 1

    ElseIf ConnRezepte.Current.row.rowstate = DataRowState.Unchanged Then
      '
      'Modified
      '
      Rezsozpt.Rezepte(KeyMenge).Nr = 2

    ElseIf ConnRezepte.Current.row.rowstate = DataRowState.Modified Then
      '
      'Modified
      '
      Rezsozpt.Rezepte(KeyMenge).Nr = 16

    ElseIf ConnRezepte.Current.row.rowstate = DataRowState.Added Then
      '
      'Added
      '
      Rezsozpt.Rezepte(KeyMenge).Nr = 4
      '


    End If
    If ViewDelRezepte.Count = 1 Then
      '
      'Deleted
      '
      Rezsozpt.Rezepte(KeyMenge).Nr = 8
      '
      'Deleted
      '
      If Not chkQuit.Checked OrElse MessageBox.Show(Texxt(5003) & "?", Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
        RzID = ViewDelRezepte(0)("REZEPT_ID")
        ReWrRezept.DelRezept(MenueParam, RzID, ier)
        Rezsozpt.Rezepte(KeyMenge).Nr = 2
        ConnRezepte.Current.row.acceptchanges()
        If ConnRezepte.Count = 0 Then
          Call PutBackControlsValues()
        End If
      Else
        Rezsozpt.Rezepte(KeyMenge).Nr = 2
        ConnRezepte.Current.row.rejectchanges()
        Exit Sub
      End If
    End If
    'TabRezepte.AcceptChanges()
    Application.DoEvents()
  End Sub
  Private Sub TDBFar_AfterColEdit(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.ColEventArgs) Handles TDBFar.AfterColEdit
    If Not IsNothing(ConnRezepte.Current) AndAlso ConnRezepte.Count > 0 Then
      chkRezRefState.Checked = Not ConnRezepte.Current.row("STATE")
    End If
  End Sub

  Private Sub TDBFar_AfterDelete(ByVal sender As Object, ByVal e As System.EventArgs) Handles TDBFar.AfterDelete
    If Not IsNothing(ConnRezepte.Current) AndAlso ConnRezepte.Count > 0 Then
      chkRezRefState.Checked = Not ConnRezepte.Current.row("STATE")
    End If
  End Sub

  Private Sub TDBFar_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles TDBFar.Click
    If Not IsNothing(ConnRezepte.Current) AndAlso ConnRezepte.Count > 0 Then
      chkRezRefState.Checked = Not ConnRezepte.Current.row("STATE")
    End If
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub


  '
  '
  '
  '
  '
  '

  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.

  End Sub




  Private Sub txtRezName_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtRezName.TextChanged, txtRezBem.TextChanged
    If sender.Text.Length > sender.MaxLength Then
      sender.Text = sender.Text.Substring(0, sender.MaxLength)
    End If
  End Sub
  Function MaxTblID(ByVal TestTable As DataTable) As Integer
    Dim ID As Integer
    Dim i As Integer
    ID = 0
    For i = 0 To TestTable.Rows.Count - 1
      If ID < TestTable.Rows(i)("REZEPT_ID") Then
        ID = TestTable.Rows(i)("REZEPT_ID")
      End If
    Next
    MaxTblID = ID
  End Function
 

 
End Class