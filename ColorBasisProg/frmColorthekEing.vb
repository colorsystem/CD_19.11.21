Option Explicit On
Option Compare Text
Option Strict Off
Public Class frmColorthekEing
  Dim RezeptID As Integer = -1
  Dim MischOld As Integer = -1
  Dim SqlStmt As String
  Dim RcmdRef As Integer
  Dim KeyMenge As String
  Dim btnREF As List(Of Button)
  Dim radRef As List(Of RadioButton)
  Dim radUnt As List(Of RadioButton)
  Dim btnUNT As List(Of Button)
  Dim ChkKWB As List(Of CheckBox)
  Dim radKWB As List(Of RadioButton)
  Dim radVONA As List(Of RadioButton)
  Dim txtCOL As List(Of TextBox)
  Dim DatRezepte As DataSet
  Dim AdaptRezepte As OleDbDataAdapter
  Dim CmdRezepte As OleDbCommand
  Dim TabRezepte As DataTable
  Dim AdaptFrbRezepte As OleDbDataAdapter
  Dim CmdFrbRezepte As OleDbCommand
  Dim TabFrbRezepte As DataTable
  Dim PrimColumns() As DataColumn
  Dim ParentColumns() As DataColumn
  Dim ChildColumns() As DataColumn
  '
  '
  '
  '
  Dim ViewDelRezepte As DataView
  Dim ViewAddRezepte As DataView
  Dim ViewModRezepte As DataView
  Dim ViewFrbRezepte As DataView
  '
  Dim RelFrbRezepte As DataRelation
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
  Dim HandleRezept As HandleRezGeneral
  Dim ReUn As String

  Dim WinHilf As AngGeos
  Dim ier As Integer

  Private Property chkQuit As Object

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

    If Reun = "" Then Exit Sub
    For kw = 0 To MenueParam.Messg.Winkel.Km - 1
      RezGraphics.PlotRwerte(kw).RefKurv.clear()
      RezGraphics.PlotRwerte(kw).Iplott = False
      If Reun <> "" Then
        If GrpRwerte(Reun)(j).IVoNa = True Then
          RezGraphics.PlotRwerte(kw).RefKurv.Add(WinHilf(0).Chrm, GrpRwerte(Reun)(j).RefKurv(MenueParam.Messg.Winkel(kw).Chrm))
          RezGraphics.PlotRwerte(MenueParam.Messg.Winkel(kw).Chrm).Iplott = True
          RezGraphics.PlotRwerte(MenueParam.Messg.Winkel(kw).Chrm).IVoNa = True
        End If
      End If
    Next kw
    RezGraphics.FawrtRwerte(0).clear()
    RezGraphics.FawrtRwerte(0).Add(KeyRe(0), GrpRwerte(Reun)(KeyRe(j)))

    '
    If Reun <> "" Then
      RezGraphics.Text = GrpRwerte(Reun)(j).Name
      PicAufBau.PicRwert.Visible = True
      PicAufBau.PicFarbXYZ.Visible = True
    End If

    RezGraphics.Rmin = -1
    RezGraphics.Rmax = -1

    PicAufBau.Refresh()
    ''
  End Sub

  Private Sub frmColorthekEing_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed

    RemoveHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
    AufbauPar.MethID = -1
  End Sub

  Private Sub frmColorthekEing_Closing(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.FormClosing
    TDBFar.EditActive = False
    ConnRezepte.EndEdit()

    If RezeptBackStore() = DialogResult.Yes Then
    End If
    ConnRezepte.CurrencyManager.Refresh()

  End Sub


  Private Sub frmColorthekEing_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim kw As Integer
    Dim XX As String
    Me.Text = Texxt(1851) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"

    '
    '
    btnREF = New List(Of Button)
    btnUNT = New List(Of Button)
    radRef = New List(Of RadioButton)
    radUnt = New List(Of RadioButton)
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
    radRef.Clear()
    radRef.Add(radREF0)
    radRef.Add(radREF1)
    radUnt.Clear()
    radUnt.Add(radUNT0)
    radUnt.Add(radUNT1)

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
    radVoNa0.Text = Texxt(449)
    radVoNa1.Text = Texxt(448)
    chkMessage.Text = Texxt(4692)
    btnQuit.Text = Texxt(3004)
    btnChangeAll.Text = Texxt(3960)
    chkTabelle.Text = Texxt(3682)
    Application.DoEvents()
    '
    'Handler für R-Werte
    '
    AddHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
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
    KeyMenge = "MNG"
    Rezsozpt.Rezepte.kwb(0) = 0
    Rezsozpt.Rezepte.kwb(1) = 1
    RezGraphics.Vkwb(0) = 0
    RezGraphics.Vkwb(1) = 1
    HandleRezept = New HandleRezGeneral
    HandleRezept.cboGRP = cboGRP
    HandleRezept.lblGRP = lblGRP
    RezGraphics.AllRezepte = Rezsozpt
    RezCheckRad.PicGraphic = RezGraphics
    PicAufBau.PicGraphic = RezGraphics
    RezCheckRad.Picauf = PicAufBau
    PicAufBau.PicGraphic = RezGraphics
    RezGrid.PicGraphic = RezGraphics
    PicAufBau.Add("REF", picREF)
    PicAufBau.Add("XYZ", picXYZ)
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
    GrpRwerte("A")(KeyRe(0)).Itp = True
    GrpRwerte("A")(KeyRe(1)).Itp = True
    GrpRwerte("U")(KeyRe(0)).Itp = True
    GrpRwerte("U")(KeyRe(1)).Itp = True
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
    '
    '
    'Zum Plotten
    '
    '
    '
    WinHilf = New AngGeos
    XX = MenueParam.User.Winkel(0).Chrm
    WinHilf.Add(XX, New AngGeo(0, 1, 1, "", 1.0, XX))
    WinHilf.Wsol = MenueParam.User.Winkel.Wsol
    WinHilf.Nwp = MenueParam.User.Winkel.Nwp
    WinHilf(XX).GK = MenueParam.User.Winkel(0).GK

    RezGraphics.Winkel = WinHilf
    RezGraphics.kwopt(0) = True
    RezGraphics.WeSc(0) = True
    RezGraphics.WeSc(1) = True
    For kw = 0 To MenueParam.Messg.Winkel.Km - 1
      RezGraphics.PlotRwerte.Add(MenueParam.Messg.Winkel(kw).Chrm, New RefValue)
      RezGraphics.PlotRwerte(kw).Name = MenueParam.Messg.Winkel(kw).Chrm
    Next kw
    'RezGraphics.FawrtRwerte = GrpRwerte
    '

    '
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

    ConnRezepte = New BindingSource
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
    TabRezepte.Columns.Add("UNT0", GetType(Integer))
    TabRezepte.Columns.Add("UNT1", GetType(Integer))
    TabRezepte.Columns.Add("TYP0", GetType(Integer))
    TabRezepte.Columns.Add("TYP1", GetType(Integer))
    TabRezepte.Columns.Add("SMP0", GetType(Integer))
    TabRezepte.Columns.Add("SMP1", GetType(Integer))
    TabRezepte.Columns("STATE").DefaultValue = False

    ReDim PrimColumns(1)
    PrimColumns(0) = TabRezepte.Columns("MISCH_ID")
    PrimColumns(1) = TabRezepte.Columns("REZEPT_ID")
    TabRezepte.Constraints.Add("PRIM", PrimColumns, True)
    TabRezepte.Columns("REZEPT_ID").AutoIncrement = True
    '
    '
    '
    AdaptFrbRezepte = New OleDbDataAdapter
    CmdFrbRezepte = New OleDbCommand("", Cndat)
    AdaptFrbRezepte.SelectCommand = CmdFrbRezepte
    TabFrbRezepte = New DataTable
    '
    '
    '
    TabFrbRezepte.Columns.Add("MISCH_ID", GetType(Integer))
    TabFrbRezepte.Columns.Add("REZEPT_ID", GetType(Integer))
    TabFrbRezepte.Columns.Add("FARBM_ID", GetType(Integer))
    TabFrbRezepte.Columns.Add("FARBM_MENGE", GetType(Single))
    TabFrbRezepte.Columns.Add("FARBM_LIMMNG", GetType(Single))
    TabFrbRezepte.Columns.Add("FARBM_OPERA", GetType(String))
    TabFrbRezepte.Columns.Add("FARBM_PREIS", GetType(Single))
    TabFrbRezepte.Columns.Add("FARBM_PROZ", GetType(Single))
    TabFrbRezepte.Columns.Add("FARBM_PROB", GetType(Single))
    TabFrbRezepte.Columns.Add("FARBM_IPOS", GetType(Byte))
    '
    TabFrbRezepte.Columns("FARBM_LIMMNG").DefaultValue = 0.0
    TabFrbRezepte.Columns("FARBM_OPERA").DefaultValue = " "
    TabFrbRezepte.Columns("FARBM_PREIS").DefaultValue = 0.0
    '
    ViewFrbRezepte = New DataView(TabFrbRezepte)
    '
    '

    '
    '
    '
    'Dataset
    '
    '
    '
    DatRezepte = New DataSet
    DatRezepte.Relations.Clear()

    DatRezepte.Tables.Clear()
    DatRezepte.Tables.Add(TabRezepte)
    DatRezepte.Tables.Add(TabFrbRezepte)
    ReDim ParentColumns(1)
    ReDim ChildColumns(1)
    ParentColumns(0) = TabRezepte.Columns("MISCH_ID")
    ParentColumns(1) = TabRezepte.Columns("REZEPT_ID")
    ChildColumns(0) = TabFrbRezepte.Columns("MISCH_ID")
    ChildColumns(1) = TabFrbRezepte.Columns("REZEPT_ID")
    RelFrbRezepte = New DataRelation("RELFRBREZ", ParentColumns, ChildColumns, True)
    DatRezepte.Relations.Add(RelFrbRezepte)



    ConnRezepte.DataSource = TabRezepte
    BindingNavigRezept.BindingSource = ConnRezepte
    cboCOL.DataSource = ConnRezepte

    txtRezID.DataBindings.Clear()
    txtRezName.DataBindings.Clear()
    txtRezBem.DataBindings.Clear()
    txtDik0.DataBindings.Clear()
    txtDik1.DataBindings.Clear()
    txtDatValue.DataBindings.Clear()
    cboGRP.DataBindings.Clear()
    chkARC.DataBindings.Clear()
    txtRezID.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_ID")
    txtRezName.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_NAME")
    txtRezBem.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_BEM")
    txtDik0.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_DIK1")
    txtDik1.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_DIK2")
    txtDatValue.DataBindings.Add("TEXT", ConnRezepte, "REZEPT_DATTIM", True, DataSourceUpdateMode.Never, "", "d")
    'cboGRP.DataBindings.Add("SELECTEDVALUE", ConnRezepte, "REZEPT_GID")
    'chkARC.DataBindings.Add("CHECKSTATE", ConnRezepte, "REZEPT_IARCH", True)


    cboCOL.DisplayMember = "REZEPT_NAME"
    cboCOL.ValueMember = "REZEPT_ID"


    '
    'Tabelle für Mischsysteme
    '
    AdaptMisch = New OleDbDataAdapter
    CmdMisch = New OleDbCommand("", Cncol)
    AdaptMisch.SelectCommand = CmdMisch
    TabMisch = New DataTable
    cboMSH.SelectedIndex = -1
    If MenueParam.MischID <> -1 Then
      AdaptMisch.SelectCommand.CommandText = HandleRezept.MischSelectCommand()
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
    If BitWrt(5, MenueParam.User.Sonst) = 0 Then
      chkUMR.Visible = False
    End If

    '
    '
    'Mischsysteme
    '
    '

    '
    '
    If BitWrt(18, MenueParam.User.Enabl) Then
      chkVOL.Enabled = True
    Else
      chkVOL.Enabled = False
    End If
    If BitWrt(18, MenueParam.User.Visbl) Then
      chkVOL.Visible = True
    Else
      chkVOL.Visible = False
    End If
    '
    '
    'Mischsystem Enabled / Visible
    '
    '
    If BitWrt(23, MenueParam.User.Enabl) Then
      cboMSH.Enabled = True
    Else
      cboMSH.Enabled = False
    End If
    If BitWrt(23, MenueParam.User.Visbl) Then
      cboMSH.Visible = True
      lblMSH.Visible = True
    Else
      cboMSH.Visible = False
      lblMSH.Visible = False
    End If
    '
    'Gruppe Enabled / Visible
    '
    '
    If BitWrt(24, MenueParam.User.Enabl) Then
      cboGRP.Enabled = True
      lblGRP.Enabled = True
    Else
      cboGRP.Enabled = False
      lblGRP.Enabled = False
    End If
    If BitWrt(24, MenueParam.User.Visbl) Then
      cboGRP.Visible = True
      lblGRP.Visible = True
    Else
      cboGRP.Visible = False
      lblGRP.Visible = False
    End If
    '
    'Archiv
    '
    If BitWrt(25, MenueParam.User.Enabl) Then
      chkARC.Enabled = True
    Else
      chkARC.Enabled = False
    End If
    If BitWrt(25, MenueParam.User.Visbl) Then
      chkARC.Visible = True
    Else
      chkARC.Visible = False
    End If
    '

    '
    '
    'R-Werte für Vorlage oder Nachstellung
    '
    If BitWrt(30, MenueParam.User.Enabl) Then
      radVONA(0).Enabled = True
      radVONA(1).Enabled = True
    Else
      radVONA(0).Enabled = False
      radVONA(1).Enabled = False
    End If

    If BitWrt(30, MenueParam.User.Visbl) Then
      panVoNa.Visible = True
      radVONA(0).Visible = True
      radVONA(1).Visible = True
    Else
      panVoNa.Visible = False
      radVONA(0).Visible = False
      radVONA(1).Visible = False
    End If
    '
    '
    '
    ''
  End Sub

  Private Sub GetNameID(ByRef ID As Integer, ByRef Name As String, ByRef Banum As String)
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
        ReWrRwert.ReadRwert(ID, GrpRwerte(ReUn)(KeyRe(j)), ier)
        GrpRwerte(ReUn)(KeyRe(j)).IVoNa = True
        If radVONA(0).Checked Then
          ConnRezepte.Current("SMP0") = GrpRwerte("A")(KeyRe(0)).ID
          ConnRezepte.Current("SMP1") = GrpRwerte("A")(KeyRe(1)).ID
        Else
          ConnRezepte.Current("TYP0") = GrpRwerte("A")(KeyRe(0)).ID
          ConnRezepte.Current("TYP1") = GrpRwerte("A")(KeyRe(1)).ID
        End If


      ElseIf RcmdRef < 0 Then
        j = -RcmdRef + 1
        ReUn = "U"
        ReWrRwert.ReadRwert(ID, GrpRwerte(ReUn)(KeyRe(j - 2)), ier)
        GrpRwerte(ReUn)(KeyRe(j - 2)).IVoNa = True
        ConnRezepte.Current("UNT0") = GrpRwerte("U")(KeyRe(0)).ID
        ConnRezepte.Current("UNT1") = GrpRwerte("U")(KeyRe(1)).ID
      End If
      radKWB(j).Enabled = True
      radKWB(j).Checked = True
      ChkKWB(j).Enabled = True
      ChkKWB(j).Checked = True
      'RezGrid.TDBFarRezStart()
      Call PlottRwert(KeyMenge)

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
          GrpRwerte("A")(index).IVoNa = False
          ChkKWB(index).Enabled = BoVal
          If radVONA(0).Checked Then
            If index = 0 Then
              ConnRezepte.Current("SMP0") = -1
            Else
              ConnRezepte.Current("SMP1") = -1
            End If
          ElseIf index = 1 Then
            If index = 0 Then
              ConnRezepte.Current("TYP0") = -1
            Else
              ConnRezepte.Current("TYP1") = -1
            End If
          End If
        End If
      Case 2, 3
        GrpRwerte("U")(index - 2).IVoNa = BoVal
        If Not BoVal Then
          GrpRwerte("U")(index - 2).ID = -1
          ChkKWB(index - 2).Enabled = BoVal
          If index = 2 Then
            ConnRezepte.Current("UNT0") = -1
          ElseIf index = 3 Then
            ConnRezepte.Current("UNT1") = -1
          End If
        End If
    End Select
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
    Dim ier As Integer
    Dim StrHilf As String
    Dim i As Integer
    Dim j As Integer
    Dim RezID As Integer
    Dim SmpID(1) As Integer
    Dim UntID(1) As Integer
    Dim TypID(1) As Integer
    '
    '
    '
    TDBFar.EditActive = False
    ConnRezepte.EndEdit()
    If MenueParam.MischID = MischOld Then
      If RezeptBackStore() = DialogResult.Cancel Then
        Exit Sub
      End If
    End If
    ConnRezepte.SuspendBinding()
    '
    '

    Rezsozpt.Rezepte(KeyMenge).clear()
    Rezsozpt.Farben.clear()
    TabFrbRezepte.Rows.Clear()
    TabRezepte.Rows.Clear()
    cboGRP.SelectedValue = MenueParam.Misch.UserRzpGID


    If cboGRP.SelectedIndex = -1 Then Exit Sub
    If Not IsNumeric(cboGRP.SelectedValue) Then Exit Sub
    cboCOL.Enabled = False
    '

    Application.DoEvents()

    Cursor = Cursors.WaitCursor
    ''
    If txtSUC.Text = "" Then
      StrHilf = ""
    Else
      StrHilf = " AND REZEPT_NAME LIKE '" & StrFil(txtSUC.Text, "'") & "'"
    End If
    If cboGRP.SelectedValue = 0 Then
      SqlStmt = "SELECT * FROM TBL_REZEPT WHERE MISCH_ID=" & MenueParam.MischID & StrHilf & " ORDER BY REZEPT_NAME"
    Else
      SqlStmt = "SELECT * FROM TBL_REZEPT WHERE MISCH_ID=" & MenueParam.MischID & StrHilf & " AND REZEPT_GID=" & cboGRP.SelectedValue & " ORDER BY REZEPT_NAME"
    End If
    CmdRezepte.CommandText = SqlStmt
    If Not FillDatset(AdaptRezepte, TabRezepte) Then Exit Sub
    Cndat.Close()
    Cndat.Open()
    For i = 0 To TabRezepte.Rows.Count - 1
      RezID = TabRezepte.Rows(i)("REZEPT_ID")
      ReWrRezept.ReadRezeptFarbGrund(KeyMenge, RezID, Rezsozpt, UntID, TypID, SmpID, ier)
      TabRezepte.Rows(i)("UNT0") = UntID(0)
      TabRezepte.Rows(i)("UNT1") = UntID(1)
      TabRezepte.Rows(i)("TYP0") = TypID(0)
      TabRezepte.Rows(i)("TYP1") = TypID(1)
      TabRezepte.Rows(i)("SMP0") = SmpID(0)
      TabRezepte.Rows(i)("SMP1") = SmpID(1)
    Next i
    Cndat.Close()
    TabRezepte.AcceptChanges()
    '
    txtRezName.MaxLength = TabRezepte.Columns("REZEPT_NAME").MaxLength
    txtRezBem.MaxLength = TabRezepte.Columns("REZEPT_BEM").MaxLength
    '
    '
    'Um Fehler zu vermeiden (s. Projekt: DataTableInternalIndexIsCorrupted5) wird die Tabelle umgespeichert
    '

    '
    'Defaultwerte
    '
    Cursor = Cursors.Default



    'txtRezID.Visible = True

  
    '
    '
    cboCOL.SelectedIndex = -1
    If ConnRezepte.Count > 0 Then
      cboCOL.Enabled = True
      cboCOL.SelectedIndex = 0
    Else
      '

      Call PutBackControlsValues()
      cboCOL.Enabled = True
      cboCOL.Text = ""

    End If
    '
    '
    '
    CmdFrbRezepte.CommandText = "SELECT * FROM TBL_REZEPT_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN " & StrLin(TabRezepte, "REZEPT_ID")
    If Not FillDatset(AdaptFrbRezepte, TabFrbRezepte) Then Exit Sub
    TabFrbRezepte.AcceptChanges()
    RezeptID = -1
    ConnRezepte.ResumeBinding()
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
    Dim ier As Integer
    Dim j As Integer
    If cboMSH.SelectedIndex = -1 Then Exit Sub
    If Not IsNumeric(cboMSH.SelectedValue) Then Exit Sub
    If Not cboMSH.Enabled Then Exit Sub
    Cursor = Cursors.WaitCursor

    Rezsozpt.Rezepte.clear()
    Rezsozpt.Farben.clear()
    Rezsozpt.Rezepte.AddRez(KeyMenge, New Recipe)
    Rezsozpt.Rezepte.AddRez("UMR", Rezsozpt.Rezepte(KeyMenge))

    '
    '
    '

    TDBFar.EditActive = False
    ConnRezepte.EndEdit()
    If RezeptBackStore() = DialogResult.Cancel Then
    End If
    '
    '
    'Änderungen zurückspeichern
    '
    '
    AufbauPar.MischID = cboMSH.SelectedValue
    RezGrid.TDBFarGridRezept(ier)
    RezGrid.NewKeynam(KeyMenge)
    '

    DatenFarb.DsetFarbFill(0, False)
    '
    Call PutBackControlsValues()

    Call PlottRwert("")
    radVONA(MenueParam.Misch.RezVor).Checked = True
    '
    '
    '
    'Gruppennamen
    '
    '

    '
    '

    cboGRP.SelectedIndex = -1
    HandleRezept.GroupList()
    Application.DoEvents()

    btnSUC.PerformClick()
    Cursor = Cursors.Default
    'ConnRezepte.CurrencyManager.Refresh()
    MischOld = cboMSH.SelectedValue
    If ConnRezepte.Count > 0 Then
      Call Afterfirst(True)
    Else
      Call Afterfirst(False)
    End If
    ' 
    '
    '
    '
    'Texte
    '
    '
   
    For i = 0 To 1
      btnREF(i).Text = Texxt(928 + i)
      j = BitInt(CShort(i), CShort(i), MenueParam.Messg.MeArtID)
      If j = 1 Then
        btnREF(i).Text = Texxt(3916 + i)
      End If
      btnUNT(i).Text = Texxt(355 + i) & Texxt(3918 + j)
      radRef(i).Text = btnREF(i).Text
      radUnt(i).Text = btnUNT(i).Text
    Next i

  End Sub
  Function RezeptBackStore() As DialogResult
    Dim RzID As Integer
    Dim GroupID As Integer
    Dim ier As Integer
    Dim UntID(1) As Integer
    Dim TypID(1) As Integer
    Dim SmpID(1) As Integer

    Call RezToView(RezeptID, KeyMenge, Rezsozpt, ViewFrbRezepte)
    If TabRezepte.Rows.Count = 0 Then
      RezeptBackStore = DialogResult.No
      Exit Function
    End If
    If ViewDelRezepte.Count + ViewModRezepte.Count + ViewAddRezepte.Count = 0 Then Exit Function
    RezeptBackStore = MessageBox.Show(Texxt(2990), Texxt(2000), MessageBoxButtons.YesNoCancel)
    If RezeptBackStore <> DialogResult.Yes Then
      Exit Function
    End If
    If Not HandleRezept.MeldSpeiAllRzp(False) Then
      RezeptBackStore = DialogResult.Cancel
      Exit Function
    End If
    '
    'Löschen
    '
    '

    Cursor = Cursors.WaitCursor
    btnQuit.Enabled = False
    Application.DoEvents()
    For i = 0 To ViewDelRezepte.Count - 1
      RzID = ViewDelRezepte(i)("REZEPT_ID")
      GroupID = ViewDelRezepte(i)("REZEPT_GID")
      '
      ReWrRezept.DelRezept(RzID, GroupID, ier)
    Next
    '
    '
    'Update
    '
    '
    '
    For i = 0 To ViewModRezepte.Count - 1
      RzID = ViewModRezepte(i)("REZEPT_ID")
      '
      '
      'Umspeichern
      '
      Call ViewToRez(RzID, KeyMenge, Rezsozpt, ViewFrbRezepte, ViewModRezepte(i).Row)
      UntID(0) = ViewModRezepte(i)("UNT0")
      UntID(1) = ViewModRezepte(i)("UNT1")
      TypID(0) = ViewModRezepte(i)("TYP0")
      TypID(1) = ViewModRezepte(i)("TYP1")
      SmpID(0) = ViewModRezepte(i)("SMP0")
      SmpID(1) = ViewModRezepte(i)("SMP1")

      If ReWrRezept.ContainsRezeptID(RzID) Then
        ReWrRezept.UpdateRezept(KeyMenge, RzID, Rezsozpt, UntID, TypID, SmpID, ier)
      Else
        ReWrRezept.DelRezept(RzID, GrpRwerte("A")(i).Gid, ier)
        ReWrRezept.AddRezept(KeyMenge, RzID, Rezsozpt, UntID, TypID, SmpID, ier)
      End If
    Next

    '
    'Hinzufügen
    '
    '
    '
    For i = 0 To ViewAddRezepte.Count - 1
      RzID = ViewAddRezepte(i)("REZEPT_ID")
      '
      '
      'Umspeichern
      '
      Call ViewToRez(RzID, KeyMenge, Rezsozpt, ViewFrbRezepte, ViewAddRezepte(i).Row)
      UntID(0) = ViewAddRezepte(i)("UNT0")
      UntID(1) = ViewAddRezepte(i)("UNT1")
      TypID(0) = ViewAddRezepte(i)("TYP0")
      TypID(1) = ViewAddRezepte(i)("TYP1")
      SmpID(0) = ViewAddRezepte(i)("SMP0")
      SmpID(1) = ViewAddRezepte(i)("SMP1")

      RzID = -1
      ReWrRezept.AddRezept(KeyMenge, RzID, Rezsozpt, UntID, TypID, SmpID, ier)

    Next
    TabFrbRezepte.AcceptChanges()
    TabRezepte.AcceptChanges()
    For i = 0 To TabRezepte.Rows.Count - 1
      TabRezepte.Rows(i)("STATE") = False
    Next
    TabRezepte.AcceptChanges()
    RezGraphics.DataChanged = False
    Cursor = Cursors.Default
    btnQuit.Enabled = True


  End Function
  Sub RezToView(ByRef RezeptID As Integer, ByRef Keymenge As String, ByRef Rezsozpt As RecipesGrp, ByRef RezView As DataView)
    Dim i As Integer
    Dim j As Integer
    Dim KeyID As String
    Dim Viewrow As DataRowView
    If RezeptID = -1 Then Exit Sub
    RezView.RowFilter = "REZEPT_ID=" & RezeptID & " AND MISCH_ID=" & MenueParam.MischID
    RezView.Sort = "FARBM_ID"
    '
    '
    'Prüfen, ob in Rezsozpt ein Farbmittel gelöscht wurde
    '
    '
    For i = RezView.Count - 1 To 0 Step -1
      KeyID = KeyName(RezView(i)("FARBM_ID"))
      For j = 0 To Rezsozpt.Rezepte(Keymenge).KF - 1
        If RezView(i)("FARBM_ID") = Rezsozpt.Rezepte(Keymenge)(j).ID Then
          Exit For
        End If
      Next j
      If RezView.Count <> 0 AndAlso j = Rezsozpt.Rezepte(Keymenge).KF Then
        '
        'Farbm_ID nicht gefunden
        '
        RezView(i).Delete()
      End If
    Next i
    '
    '
    If IsNothing(ConnRezepte.Current) Then
      Rezsozpt.Rezepte(Keymenge).clear()
    End If

    '
    For j = 0 To Rezsozpt.Rezepte(Keymenge).KF - 1
      i = RezView.Find(Rezsozpt.Rezepte(Keymenge)(j).ID)
      If i = -1 Then
        '
        '
        'Neues Farbmittel vorhanden
        '
        Viewrow = RezView.AddNew

      Else
        Viewrow = RezView(i)
      End If

      Viewrow("MISCH_ID") = MenueParam.MischID
      Viewrow("REZEPT_ID") = RezeptID
      Viewrow("FARBM_ID") = Rezsozpt.Rezepte(Keymenge)(j).ID
      Viewrow("FARBM_MENGE") = Rezsozpt.Rezepte(Keymenge)(j).FaAmng
      Viewrow("FARBM_PROZ") = Rezsozpt.Rezepte(Keymenge)(j).Proz
      Viewrow("FARBM_PROB") = Rezsozpt.Rezepte(Keymenge)(j).Prob
      Viewrow("FARBM_IPOS") = CByte(j)

      Viewrow.EndEdit()
    Next j
  End Sub
  Sub ViewToRez(ByRef RezeptID As Integer, ByRef Keymenge As String, ByRef Rezsozpt As RecipesGrp, ByRef RezView As DataView, ByRef RezRow As DataRow)
    Dim i As Integer
    If RezeptID = -1 Then Exit Sub
    RezView.RowFilter = "REZEPT_ID=" & RezeptID
    RezView.Sort = "FARBM_IPOS"
    '
    '
    '
    '
    'Alle Rezepte in Rezsozpt löschen
    '
    '
    Rezsozpt.Rezepte(Keymenge).clear()

    '
    '
    Rezsozpt.Rezepte(Keymenge).ID = RezRow("REZEPT_ID")
    Rezsozpt.Rezepte(Keymenge).Name = RezRow("REZEPT_NAME")
    Rezsozpt.Rezepte(Keymenge).Bem = RezRow("REZEPT_BEM")
    Rezsozpt.Rezepte(Keymenge).Gid = RezRow("REZEPT_GID")
    Rezsozpt.Rezepte(Keymenge).Dicke(0) = RezRow("REZEPT_DIK1")
    Rezsozpt.Rezepte(Keymenge).Dicke(1) = RezRow("REZEPT_DIK2")
    Rezsozpt.Rezepte(Keymenge).Iarch = RezRow("REZEPT_IARCH")

    For i = 0 To RezView.Count - 1
      ' Hinzufügen
      '
      '

      Rezsozpt.Rezepte(Keymenge).AddFaNr(KeyRe(i), New ColorAmount)
      Rezsozpt.Rezepte(Keymenge)(KeyRe(i)).ID = RezView(i)("FARBM_ID")
      Rezsozpt.Rezepte(Keymenge)(KeyRe(i)).FaAmng = RezView(i)("FARBM_MENGE")
      Rezsozpt.Rezepte(Keymenge)(KeyRe(i)).Proz = RezView(i)("FARBM_PROZ")
      Rezsozpt.Rezepte(Keymenge)(KeyRe(i)).Prob = RezView(i)("FARBM_PROB")
    Next i
  End Sub
  Sub PutBackControlsValues()
    Dim i As Integer
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
    If chkUMR.Checked Then
      KeyMenge = "UMR"
      RezGrid.NewKeynam(KeyMenge)
      RezGrid.TDBFarRezStart(ier)


    Else
      KeyMenge = "MNG"
      RezGrid.NewKeynam(KeyMenge)
      RezGrid.TDBFarRezStart(ier)

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
    Dim i As Integer
    Dim DefText As String
    If IsNothing(ConnRezepte) Then Exit Sub
    If ConnRezepte.Count = 0 Then
      Rezsozpt.Rezepte(KeyMenge).clear()
      Call PutBackControlsValues()
      For i = 0 To radKWB.Count - 1
        radKWB(i).Checked = False
      Next
      '
      'Defaultwerte für neues Rezept (falls noch kein Eintrag erfolgt ist)
      '
      '
      cboGRP.SelectedValue = MenueParam.Misch.UserRzpGID
      chkARC.CheckState = False
      TabRezepte.Columns("MISCH_ID").DefaultValue = MenueParam.MischID
      TabRezepte.Columns("REZEPT_NAME").DefaultValue = "???"
      TabRezepte.Columns("REZEPT_BEM").DefaultValue = "???"
      TabRezepte.Columns("REZEPT_DIK1").DefaultValue = MenueParam.Misch.Dicke
      TabRezepte.Columns("REZEPT_DIK2").DefaultValue = MenueParam.Misch.Dicke
      TabRezepte.Columns("REZEPT_DATTIM").DefaultValue = Date.Now
      TabRezepte.Columns("REZEPT_GID").DefaultValue = cboGRP.SelectedValue
      TabRezepte.Columns("REZEPT_IARCH").DefaultValue = chkARC.CheckState
      TabRezepte.Columns("UNT0").DefaultValue = -1
      TabRezepte.Columns("UNT1").DefaultValue = -1
      GrpRwerte("U")(0).IVoNa = False
      GrpRwerte("U")(1).IVoNa = False
      GrpRwerte("U")(0).ID = -1
      GrpRwerte("U")(1).ID = -1
      ChkKWB(2).Checked = False
      ChkKWB(3).Checked = False
      ChkKWB(2).Enabled = False
      ChkKWB(3).Enabled = False
      radKWB(2).Enabled = False
      radKWB(3).Enabled = False

      'Defaultwerte
      '

      '
      '
    Else
      If ConnRezepte.IsBindingSuspended Then Exit Sub
      If ConnRezepte.Position >= ConnRezepte.Count Then Exit Sub
      '

      TabRezepte.Columns("MISCH_ID").DefaultValue = MenueParam.MischID
      '
      '
      '
      'Defaultwerte für neues Rezept (Current-Einträge werdem übernommen)
      '
      '
      DefText = ConnRezepte.Current("REZEPT_NAME") & "???"
      If DefText.Length > txtRezName.MaxLength Then
        DefText = DefText.Substring(0, txtRezName.MaxLength)
      End If
      TabRezepte.Columns("REZEPT_NAME").DefaultValue = DefText
      DefText = ConnRezepte.Current("REZEPT_BEM")
      If DefText.Length > txtRezBem.MaxLength Then
        DefText = DefText.Substring(0, txtRezBem.MaxLength)
      End If


      TabRezepte.Columns("REZEPT_BEM").DefaultValue = DefText
      TabRezepte.Columns("REZEPT_DIK1").DefaultValue = ConnRezepte.Current("REZEPT_DIK1")
      TabRezepte.Columns("REZEPT_DIK2").DefaultValue = ConnRezepte.Current("REZEPT_DIK2")
      TabRezepte.Columns("REZEPT_DATTIM").DefaultValue = Date.Now
      TabRezepte.Columns("REZEPT_GID").DefaultValue = cboGRP.SelectedValue
      TabRezepte.Columns("REZEPT_IARCH").DefaultValue = chkARC.CheckState
      TabRezepte.Columns("UNT0").DefaultValue = ConnRezepte.Current("UNT0")
      TabRezepte.Columns("UNT1").DefaultValue = ConnRezepte.Current("UNT1")

    End If

    TabRezepte.Columns("TYP0").DefaultValue = -1
    TabRezepte.Columns("TYP1").DefaultValue = -1
    TabRezepte.Columns("SMP0").DefaultValue = -1
    TabRezepte.Columns("SMP1").DefaultValue = -1
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

    Call Afterfirst(True)
  End Sub

  Private Sub ConnRezepte_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnRezepte.CurrentChanged
    Dim ier As Integer
    Dim i As Integer
    Dim j As Integer
    Dim Detach As Boolean

    If ConnRezepte.Count = 0 Then
      TabFrbRezepte.Rows.Clear()
      Rezsozpt.Rezepte(KeyMenge).clear()
      RezGrid.TDBFarRezStart(ier)
      For j = 0 To GrpRwerte.Count - 1
        For i = 0 To GrpRwerte(j).Count - 1
          GrpRwerte(j)(i).ID = -1
        Next i
      Next j
      chkRef0.Checked = False
      chkRef1.Checked = False
      chkUnt0.Checked = False
      chkUnt1.Checked = False
      btnRef0.Enabled = False
      btnRef1.Enabled = False
      btnUnt0.Enabled = False
      btnUnt1.Enabled = False
      radREF0.Enabled = False
      radREF1.Enabled = False
      radUNT0.Enabled = False
      radUNT1.Enabled = False

      If Not IsNothing(PicAufBau) Then
        PicAufBau.PicRwert.Hide()
        PicAufBau.PicFarbXYZ.Hide()
      Else
        Exit Sub
      End If
    End If
    Detach = False
    If IsNothing(ConnRezepte.Current) Then
      Exit Sub
    End If
    If ConnRezepte.IsBindingSuspended Then Exit Sub
    BindingNavigRezept.Enabled = False
    '
    If ConnRezepte.Current.row.rowstate = DataRowState.Detached Then
      Detach = True
    End If
    '
    'Änderungen übernehmen oder verwerfen
    '
    TDBFar.EditActive = False
    '
    ConnRezepte.Current.endedit()

    '
    'Neues Rezsozpt aufbauen(einlesen)
    '
    '


    '
    '
    If ConnRezepte.Count <> 0 Then
      '
      '
      '
      'Rezept übernehmen
      '
      '
      For i = 0 To ConnRezepte.Count - 1
        If RezeptID = ConnRezepte.Item(i)("REZEPT_ID") And MenueParam.MischID = ConnRezepte.Item(i)("MISCH_ID") Then
          Call RezToView(RezeptID, KeyMenge, Rezsozpt, ViewFrbRezepte)
          Exit For
        End If
      Next




      '
      '
      'Aktuelles Rezept nach REZSOZPT übernehmen (Prüfen, ob z.B. Rezeptname mit Name in Tabelle (Connrezepte.current.row übereinstimmt)
      '
      '

      Cursor = Cursors.WaitCursor

      Call PutBackControlsValues()



      RezeptID = CInt(ConnRezepte.Current.row("REZEPT_ID"))
      If Detach Then
        txtRezName.BackColor = Color.Red
        '
        '
        'altes Rezept übernehmen
        '
        '
        Call RezToView(RezeptID, KeyMenge, Rezsozpt, ViewFrbRezepte)
      Else
        txtRezName.BackColor = Color.White
      End If


      If RezeptID = -1 Then
        MsgBox("ERROR Rezept_ID" & Space(2) & RezeptID)
        BindingNavigRezept.Enabled = True
        Cursor = Cursors.Arrow
        Exit Sub
      End If
      Call ViewToRez(RezeptID, KeyMenge, Rezsozpt, ViewFrbRezepte, ConnRezepte.Current.row)
      GrpRwerte("U")(KeyRe(0)).ID = ConnRezepte.Current("UNT0")
      GrpRwerte("U")(KeyRe(1)).ID = ConnRezepte.Current("UNT1")
      If radVONA(0).Checked Then
        GrpRwerte("A")(KeyRe(0)).ID = ConnRezepte.Current("SMP0")
        GrpRwerte("A")(KeyRe(1)).ID = ConnRezepte.Current("SMP1")
      Else
        GrpRwerte("A")(KeyRe(0)).ID = ConnRezepte.Current("TYP0")
        GrpRwerte("A")(KeyRe(1)).ID = ConnRezepte.Current("TYP1")
      End If

      For j = 0 To GrpRwerte.Count - 1
        For i = 0 To GrpRwerte(j).Count - 1
          If GrpRwerte(j)(i).ID >= 0 Then
            ReWrRwert.ReadRwert(GrpRwerte(j)(i).ID, GrpRwerte(j)(i), ier)
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
      '
      '
      '
      'Aufbau Grid
      '
      '
      RezGrid.TDBFarRezStart(ier)
      '
      Call PlottRwert(KeyMenge)
      '
      '
      '
      '
      '
      RezGrid.chkVOL.CheckState = Rezsozpt.IVOL

    End If
    Cursor = Cursors.Arrow
    Application.DoEvents()
    If Not Detach AndAlso Not CheckUpdate() Then
    End If

    BindingNavigRezept.Enabled = True
    ConnRezepte.ResumeBinding()
  End Sub

  Function CheckUpdate() As Boolean
    Dim i As Integer
    Dim imsg As DialogResult
    ''
    ' Fehlermeldungen
    '
    '
    CheckUpdate = True
    If chkMessage.Checked Then
      For i = 0 To 1
        If Rezsozpt.Rezepte(KeyMenge).Dicke(i) = 0 And (GrpRwerte("A")(i).ID >= 0 And GrpRwerte("A")(i).IVoNa) Then
          imsg = MessageBox.Show(Texxt(2601), Texxt(2000), MessageBoxButtons.OK)
          Exit Function
        End If
      Next i
      For i = 0 To 1
        If Rezsozpt.Rezepte(KeyMenge).Dicke(i) > 0 And (GrpRwerte("A")(i).ID < 0 Or Not GrpRwerte("A")(i).IVoNa) Then
          imsg = MessageBox.Show(Texxt(2602), Texxt(2000), MessageBoxButtons.OK)
          CheckUpdate = False
          Exit For
        End If
      Next i
      '
      '
      For i = 0 To 1
        If GrpRwerte("U")(i).ID >= 0 And GrpRwerte("A")(i).ID < 0 Then
          imsg = MessageBox.Show(Texxt(2603), Texxt(2000), MessageBoxButtons.OK)
          CheckUpdate = False
          Exit For
        End If
      Next i

    End If
    ''

  End Function

  Private Sub TDBFar_AfterColEditUpd(ByVal sender As Object, ByVal e As C1.Win.C1TrueDBGrid.ColEventArgs) Handles TDBFar.AfterColEdit, TDBFar.AfterColUpdate, TDBFar.ColEdit
    If Not IsNothing(ConnRezepte.Current) AndAlso ConnRezepte.Count > 0 And Not ConnRezepte.Current("STATE") Then
      ConnRezepte.Current("STATE") = Not ConnRezepte.Current("STATE")
    End If
  End Sub


  Private Sub TDBFar_AfterDelete(ByVal sender As Object, ByVal e As System.EventArgs) Handles TDBFar.AfterDelete, TDBFar.AfterInsert
    If Not IsNothing(ConnRezepte.Current) AndAlso ConnRezepte.Count > 0 And Not ConnRezepte.Current("STATE") Then
      ConnRezepte.Current("STATE") = Not ConnRezepte.Current("STATE")
    End If
  End Sub
  Private Sub TDBFar_Click(sender As Object, e As System.EventArgs) Handles TDBFar.Click
    If ConnRezepte.Count = 0 Then
      MsgBox(Texxt(2953))
      Exit Sub
    End If

    If Not IsNothing(ConnRezepte.Current) AndAlso ConnRezepte.Count > 0 And Not ConnRezepte.Current("STATE") Then
      ConnRezepte.Current("STATE") = Not ConnRezepte.Current("STATE")
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
    txtRezName.BackColor = txtRezID.BackColor
  End Sub




  Private Sub chkRezRefState_CheckedChanged(sender As Object, e As System.EventArgs)
    ' Call RezToView(Rezsozpt.Rezepte(KeyMenge).ID, KeyMenge, Rezsozpt, ViewFrbRezepte)
  End Sub

  Private Sub btnUntStandard_Click(sender As System.Object, e As System.EventArgs) Handles btnChangeAll.Click
    Dim UuuID(1) As Integer
    If IsNothing(ConnRezepte) Then Exit Sub
    If ConnRezepte.Count > 0 Then
      If MessageBox.Show(Texxt(3961) & Chr(13) & Chr(10) & GrpRwerte("U")(0).Name & Chr(13) & Chr(10) & GrpRwerte("U")(1).Name, Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
        UuuID(0) = ConnRezepte.Current("UNT0")
        UuuID(1) = ConnRezepte.Current("UNT1")
        For i = 0 To TabRezepte.Rows.Count - 1
          TabRezepte.Rows(i)("UNT0") = UuuID(0)
          TabRezepte.Rows(i)("UNT1") = UuuID(1)
        Next
      End If
      If MessageBox.Show(Texxt(3962) & Chr(13) & Chr(10) & cboGRP.Text, Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
        For i = 0 To TabRezepte.Rows.Count - 1
          TabRezepte.Rows(i)("REZEPT_GID") = cboGRP.SelectedValue
        Next
      End If
      If MessageBox.Show(Texxt(3963) & Chr(13) & Chr(10) & chkARC.CheckState, Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
        For i = 0 To TabRezepte.Rows.Count - 1
          TabRezepte.Rows(i)("REZEPT_IARCH") = chkARC.CheckState
        Next
      End If

    End If
  End Sub

  Private Sub btnQuit_Click(sender As Object, e As System.EventArgs) Handles btnQuit.Click
    TDBFar.EditActive = False
    ConnRezepte.EndEdit()
    If RezeptBackStore() = DialogResult.Yes Then
    End If
  End Sub




  Sub Afterfirst(bool As Boolean)
    TDBFar.Enabled = True
    btnRef0.Enabled = bool
    btnRef1.Enabled = bool
    btnUnt0.Enabled = bool
    btnUnt1.Enabled = bool
  End Sub
  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtDik0.Validating, txtDik1.Validating, txtSum.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub

  Private Sub txtDat_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtDatValue.Validating
    If Not IsDate(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(2500))
    End If
  End Sub
End Class