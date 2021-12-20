Option Explicit On
Option Compare Text
Option Strict Off
Public Class frmFarbUmrechnung


  Dim ier As Integer
  Dim i As Integer
  Dim k As Integer
  Dim j As Integer
  Dim Advanced As Boolean
  Dim quali As QualKontrolle
  Dim ValVal As Integer
  Dim Winkel As AngGeos
  '
  Dim TooltipFarbumrechnung As ToolTip
  Dim GrpRwerte As RefValuesGrp
  Dim TabRef As DataTable
  Dim TabTdb As DataTable
  Dim TabCMYK As DataTable
  Dim RezGraphics As HandleRezGrafik
  Dim ReadWrite As ReadWriteRwert
  Dim HandleRezept As HandleRezGeneral
  Dim HandleRwrt As HandleRwerte
  Dim Picauf As HandlePictures
  '
  Dim KeyR As String
  Dim DragValue As Object
  Dim Xvalue As Single
  Dim Yvalue As Single
  Dim ChEnd As Boolean
  Dim RcmdRef As Integer
  Dim Fako(,) As Single
  Dim Isch As Integer
  Dim Ifl As Integer
  Dim Ifw As Integer
  Dim Mdim As Integer
  Dim kwopt() As Boolean
  Dim WeSc() As Boolean
  Dim lblREF As List(Of Label)
  Dim btnREF As List(Of Button)
  Dim chkWIN As List(Of CheckBox)
  Dim radKODIF As List(Of RadioButton)
  Dim radANGL As List(Of RadioButton)
  Dim radKURV As List(Of RadioButton)
  '
  Public Sub New(Advan As Boolean)

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
    Advanced = Advan
  End Sub

  Private Sub frmFarbUmrechnung_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
    AufbauPar.MethID = -1

  End Sub '


  Private Sub frmFarbUmrechnung_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed

    Try
      RemoveHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID
      GrpRwerte = Nothing
      quali = Nothing
    Catch ex As Exception

    End Try
  End Sub
  Private Sub frmFarbUmrechnung_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    AufbauPar.MethID = 103
    Winkel = MenueParam.Messg.Winkel
    HandleRezept = New HandleRezGeneral
    HandleRwrt = New HandleRwerte
    RezGraphics = New HandleRezGrafik
    Picauf = New HandlePictures
    Picauf.PicGraphic = RezGraphics
    Picauf.Add("REF", CType(picREF, PictureBox))
    RezGraphics.WeSc(0) = True
    RezGraphics.WeSc(1) = True
    RezGraphics.Kwop = 0
    RezGraphics.Winkel = Winkel
    RezGraphics.Vkwb(0) = 0
    RezGraphics.Vkwb(1) = 1


    TooltipFarbumrechnung = New ToolTip
    GrpRwerte = New RefValuesGrp
    quali = New QualKontrolle
    btnREF = New List(Of Button)
    lblREF = New List(Of Label)
    chkWIN = New List(Of CheckBox)
    radKODIF = New List(Of RadioButton)
    radANGL = New List(Of RadioButton)
    radKURV = New List(Of RadioButton)
    TabTdb = New DataTable
    TabRef = New DataTable
    ReadWrite = New ReadWriteRwert
    '
    '
    btnREF.Clear()
    btnREF.Add(btnREF_0)
    btnREF.Add(btnREF_1)
    btnREF.Add(btnREF_2)
    '
    '
    '
    lblREF.Clear()
    lblREF.Add(lblREF_0)
    lblREF.Add(lblREF_1)
    lblREF.Add(lblREF_2)
    '
    '
    '
    chkWIN.Clear()
    chkWIN.Add(chkWIN_0)
    chkWIN.Add(chkWIN_1)
    chkWIN.Add(chkWIN_2)
    chkWIN.Add(chkWIN_3)
    chkWIN.Add(chkWIN_4)
    chkWIN.Add(chkWIN_5)
    chkWIN.Add(chkWIN_6)
    chkWIN.Add(chkWIN_7)
    chkWIN.Add(chkWIN_8)
    '
    '

    '
    '
    radKODIF.Clear()
    radKODIF.Add(radKODIF_0)
    radKODIF.Add(radKODIF_1)
    radKODIF.Add(radKODIF_2)
    radKODIF.Add(radKODIF_3)
    radKODIF.Add(radKODIF_4)
    radKODIF.Add(radKODIF_5)
    radKODIF.Add(radKODIF_6)
    radKODIF.Add(radKODIF_7)
    radKODIF.Add(radKODIF_8)

    '
    '
    radKURV.Clear()
    radKURV.Add(radKURV_0)
    radKURV.Add(radKURV_1)
    radKURV.Add(radKURV_2)
    '
    '
    radANGL.Clear()
    radANGL.Add(radANGL_0)
    radANGL.Add(radANGL_1)
    '
    '
    '
    AddHandler GetPutReflex.HdlGetNameID, AddressOf GetNameID

    '
    '
    'Aufbau RwerteGrp
    '
    '
    '
    GrpRwerte.Add("X", New RefValues)   'Messung und Rechnung
    GrpRwerte("X").Add(KeyRe(0), New RefValue)
    GrpRwerte("X").Add(KeyRe(1), New RefValue)
    GrpRwerte("X").Add(KeyRe(2), New RefValue)
    GrpRwerte("X").Add(KeyRe(3), New RefValue)
    '

    '
    '
    Me.Text = Texxt(1800 + MenueParam.MethID) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    lblGLZ.Text = Texxt(358)
    btnREF_0.Text = Texxt(1250)
    btnREF_1.Text = Texxt(1251)
    btnREF_2.Text = Texxt(1252)
    btnVER.Text = Texxt(360)
    btnSPEI.Text = Texxt(1253)
    radANGL_0.Text = Texxt(1247)
    radANGL_1.Text = Texxt(1248)
    radKODIF_0.Text = Texxt(1241)
    radKODIF_1.Text = Texxt(1242)
    radKODIF_2.Text = Texxt(1243)
    radKODIF_3.Text = Texxt(1244)
    radKODIF_4.Text = Texxt(1245)
    radKODIF_5.Text = Texxt(1246)
    radKODIF_6.Text = Texxt(1262)
    radKODIF_7.Text = Texxt(1263)
    radKODIF_8.Text = Texxt(1264)

    radKURV_0.Text = Texxt(1255)
    radKURV_1.Text = Texxt(1256)
    radKURV_2.Text = Texxt(1257)
    chkRwerte.Text = Texxt(370)
    lblLicht.Text = Texxt(171)
    lblWinkel.Text = Texxt(173)
    lblDE.Text = TexKt(10138)
    chkREF.Text = Texxt(529)
    btnKOP.Text = Texxt(389)
    TooltipFarbumrechnung.SetToolTip(cboGRPRwrt, Texxt(416))

    'cboGLZ.= "GGG"
    'ParForm = Me
    '
    '
    ' Werte für Glanzabzug
    '
    cboGLZ.Items.Clear()
    cboGLZ.Items.Clear()
    For i = 0 To GetPutReflex.GlanzWrt.Count - 1
      cboGLZ.Items.Add(GetPutReflex.GlanzWrt(i))
    Next
    cboGLZ.Text = Winkel(0).GK(0)
    '
    For i = 0 To chkWIN.Count - 1
      chkWIN(i).Visible = False
    Next i

    For i = 0 To Winkel.Km - 1
      chkWIN(i).Visible = True
      chkWIN(i).Text = LTrim(Winkel(i).Chrm)
    Next i
    '
    '
    '
    cboLicht.Items.Clear()
    For i = 0 To MenueParam.Normfa.Nlz - 1
      cboLicht.Items.Add(MenueParam.Normfa(i).NormKenn)
    Next
    cboLicht.SelectedIndex = 0
    '
    '
    '
    cboWinkel.Items.Clear()
    For i = 0 To Winkel.Km - 1
      cboWinkel.Items.Add(Winkel(i).Chrm)
    Next
    cboWinkel.SelectedIndex = 0

    '
    '
    'Grid
    '
    '
    GridTDB.DataSource = TabTdb
    Call GridFaStart()
    dbgRef.DataSource = TabRef
    HandleRwrt.MakeTABRefwerte(False, MenueParam.Messg.Kbez, Winkel, TabRef)
    ReDim kwopt(Winkel.Km - 1)
    kwopt(0) = True
    ReDim WeSc(0)
    WeSc(0) = True
    radKODIF(5).Checked = True
    '
    'Gruppr R-Werte
    '

    HandleRwrt.cboGRP = cboGRPRwrt
    Call HandleRwrt.GRoupList()
    '
    '
    '
    Advanced = Advanced Or BitWrt(26, MenueParam.User.Drum)
    If Not Advanced Then
      lblLicht.Visible = False
      cboLicht.Visible = False
      lblWinkel.Visible = False
      cboWinkel.Visible = False
      hscAlpha.Visible = False
      picFarbeI.Visible = False
      picFarbeS.Visible = False
      lblDE.Visible = False
      txtDE.Visible = False
      chkREF.Visible = False
      btnKOP.Visible = False
    End If
  End Sub
  Sub GridFaStart()
    Dim i As Integer
    Dim Km As Integer
    Km = Winkel.Km
    GridTDB.AllowUserToAddRows = False
    GridTDB.RowHeadersVisible = False
    '
    For i = 0 To Km
      Select Case i
        Case 0
          TabTdb.Columns.Add("  ", GetType(String))
          GridTDB.Columns(i).Visible = True
          GridTDB.Columns(i).ReadOnly = True
          GridTDB.Columns(i).Width = 200
          GridTDB.Columns(i).HeaderText = ""
          GridTDB.Columns(i).DefaultCellStyle.BackColor = Color.FromArgb(&H8000000F)
          GridTDB.Columns(i).DefaultCellStyle.BackColor = Color.White
        Case Else
          TabTdb.Columns.Add(Winkel(i - 1).Chrm, GetType(String))
          GridTDB.Columns(i).Visible = True
          GridTDB.Columns(i).ReadOnly = False
          GridTDB.Columns(i).Width = 50
          GridTDB.Columns(i).HeaderText = Winkel(i - 1).Chrm
          GridTDB.Columns(i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      End Select
    Next i
  End Sub

  Private Sub GetNameID(ByRef ID As Integer, ByRef Name As String, ByRef Banum As String)
    If KeyR Is Nothing Then Exit Sub
    GrpRwerte("X")(KeyR).ID = ID
    GrpRwerte("X")(KeyR).Nr = RcmdRef + 1
    GrpRwerte("X")(KeyR).kwb = 1
    GrpRwerte("X")(KeyR).IVoNa = False
    ReadWrite.ReadRwert(ID, GrpRwerte("X")(KeyR), ier)
    If ier = 0 Then
      GrpRwerte("X")(KeyR).IVoNa = True
      lblREF(RcmdRef).Text = GrpRwerte("X")(KeyR).Name
    End If
    'Call EnblRef
    GetPutReflex.InVisible()
  End Sub

  Private Sub frmFarbUmrechnung_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Resize, Me.Activated
    Call ResizeChild(Me)
  End Sub




  Private Sub radKODIF_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  radKODIF_0.CheckedChanged, radKODIF_1.CheckedChanged, radKODIF_2.CheckedChanged, radKODIF_3.CheckedChanged, _
  radKODIF_4.CheckedChanged, radKODIF_5.CheckedChanged, radKODIF_6.CheckedChanged, radKODIF_7.CheckedChanged, radKODIF_8.CheckedChanged
    Dim Index As Integer
    Dim CommandCMYK As New OleDbCommand("", Cndat)
    Dim AdaptCMYK As New OleDbDataAdapter
    Dim DRow As DataRow
    Dim SplitDrow() As String
    Dim Rhilf() As Single
    Dim Refel As RefValue
    Dim XYZ(2) As Single
    Dim ALAB(2) As Single
    Dim ALCH(2) As Single

    Dim k As Integer
    Dim i As Integer
    Dim j As Integer
    If sender.name = "" Then Exit Sub
    Index = CInt(sender.name.substring(9, 1))
    Dim IT() As Integer
    If Index > 2 Then
      radKURV(1).Enabled = True
      radKURV(2).Enabled = True
    Else
      radKURV(0).Checked = True
      radKURV(1).Enabled = False
      radKURV(2).Enabled = False
    End If
    ReDim IT(2)
    TabTdb.Rows.Clear()
    lstCMYK.Visible = False
    chkRwerte.Visible = False
    Select Case Index
      Case 0
        '
        '
        'DX,DY,DZ
        '
        '
        IT(0) = 10073
        IT(1) = 10074
        IT(2) = 10075
      Case 1
        '
        '
        'DL*,Da*,Db*
        '
        '
        IT(0) = 10139
        IT(1) = 10142
        IT(2) = 10143
      Case 2
        '
        '
        'DL*,DC*,DH*
        '
        '
        IT(0) = 10139
        IT(1) = 10140
        IT(2) = 10141
      Case 3
        '
        '
        'X,Y,Z
        '
        '
        IT(0) = 10065
        IT(1) = 10066
        IT(2) = 10067
      Case 4
        '
        '
        'L*,a*,b*
        '
        '
        IT(0) = 10129
        IT(1) = 10132
        IT(2) = 10133
      Case 5
        '
        '
        'L*,C*,h
        '
        '
        IT(0) = 10129
        IT(1) = 10130
        IT(2) = 10131
      Case 6
        '
        '
        'R,G,B
        '
        '
        IT(0) = 11665
        IT(1) = 11666
        IT(2) = 11667
        If MenueParam.Normfa.Nlz > 1 Then
          MsgBox(Texxt(4171))
          GoTo errrad
        End If
        If Winkel.Km > 1 Then
          MsgBox(Texxt(4172))
          GoTo errrad
        End If
      Case 7
        '
        '
        'C,M,Y,K
        '
        '
        ReDim IT(3)
        IT(0) = 9990
        IT(1) = 9991
        IT(2) = 9992
        IT(3) = 9993
        If MenueParam.Normfa.Nlz > 1 Then
          MsgBox(Texxt(4171))
          GoTo errrad
        End If
        If Winkel.Km > 1 Then
          MsgBox(Texxt(4172))
          GoTo errrad
        End If
        '
        '
        '
        'Tabelle TABCMYK aufbauen
        '
        '
        CommandCMYK.CommandText = "SELECT RWERT_ID,MESSGRW_ID,RWERT_NAME,RWERT_BEM,RWERT_GID,RWERT_RWERT FROM TBL_RWERT" _
        & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_GID=" & cboGRPRwrt.SelectedValue
        AdaptCMYK.SelectCommand = CommandCMYK
        TabCMYK = New DataTable
        TabCMYK.Rows.Clear()
        If Not FillDatset(AdaptCMYK, TabCMYK) Then
          Exit Sub
        End If
        TabCMYK.Columns.Add("X", GetType(Single))
        TabCMYK.Columns.Add("Y", GetType(Single))
        TabCMYK.Columns.Add("Z", GetType(Single))
        '
        '
        'Nicht numerische Bemerkungen löschebn
        '
        Refel = New RefValue
        For Each DRow In TabCMYK.Rows
          SplitDrow = DRow("RWERT_BEM").split("-")
          If SplitDrow.Length <> 4 _
          OrElse Not IsNumeric(SplitDrow(0)) OrElse Not IsNumeric(SplitDrow(1)) _
          OrElse Not IsNumeric(SplitDrow(2)) OrElse Not IsNumeric(SplitDrow(3)) Then
            DRow.Delete()
          Else
            Rhilf = GetSingles(DRow("RWERT_RWERT"))
            '
            '
            Refel.RefKurv.clear()
            '
            Refel.RefKurv.Add(Winkel(0).Chrm, New CurveRef(Winkel.Wsol.Nwe))
            Refel.De(0) = 0.0
            Array.Copy(Rhilf, 0, Refel.RefKurv(Winkel(0).Chrm).R, 0, Winkel.Wsol.Nwe)
            '
            '
            '
            'X,Y,Z berechnen und in Tabelle einfügen
            '
            '
            '
            '
            quali.FarbWerteXYZLABCH(Winkel, Refel, XYZ, ALAB, ALCH, ier)
            DRow("X") = XYZ(0)
            DRow("Y") = XYZ(1)
            DRow("Z") = XYZ(2)
          End If
        Next
        '
        '
        TabCMYK.AcceptChanges()

        AdaptCMYK.Dispose()
        CommandCMYK.Dispose()
        If TabCMYK.Rows.Count < 10 Then
          MsgBox(Texxt(4706))
          TabCMYK = Nothing
        End If
      Case 8
        '
        '
        'x,y,Y
        '
        '
        '
        IT(0) = 10068
        IT(1) = 10069
        IT(2) = 10066

    End Select
    For k = 0 To MenueParam.Normfa.Nlz - 1
      For i = 0 To IT.Length - 1
        TabTdb.Rows.Add(TabTdb.NewRow)
        TabTdb.Rows(3 * k + i)(0) = Trim(TexKt(IT(i))) & " (" & MenueParam.Normfa(k).NormNama & ")"
        For j = 0 To Winkel.Km - 1
          TabTdb.Rows(3 * k + i)(j + 1) = 0
        Next j
      Next i
    Next k
ErrRad:
    picREF.Visible = False
    picFarbeS.Visible = False
    picFarbeI.Visible = False

  End Sub
  Private Sub cboGLZ_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboGLZ.SelectedIndexChanged
    Winkel(0).GK(0) = Singl(cboGLZ.Text)
    TabCMYK = Nothing
  End Sub
  'Private Sub cboGLZ_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboGLZ.Click
  '  MenueParam.Messg.GK(0) = Singl(cboGLZ.Text)

  '  End Sub
  'Private Sub picREF_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles picREF.Paint
  '  If IsNothing(RezGraphics) Then Exit Sub
  '  Try
  '    e.Graphics.Clear(Color.White)
  '    RezGraphics.GraphBounds = New RectangleF(0.0 * sender.width, 0.0 * sender.height, 1.0 * sender.width, 1.0 * sender.height)
  '    RezGraphics.Skalier()
  '    RezGraphics.PicREFGraph(sender, e.Graphics)
  '  Finally
  '  End Try
  '
  '  End Sub
  Private Sub chkWIN_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  chkWIN_0.Click, chkWIN_1.Click, chkWIN_2.Click, chkWIN_3.Click, chkWIN_4.Click, chkWIN_5.Click, chkWIN_6.Click, chkWIN_7.Click, chkWIN_8.Click
    If sender.name = "" Then Exit Sub
    Call PlottUmrech(Ifl)



  End Sub
  Sub PlottUmrech(ByVal ifl As Integer)
    Dim i As Integer
    RezGraphics.PlotRwerte.clear()
    RezGraphics.PlotRwerte.Add("R", GrpRwerte("X")(KeyRe(0)))
    If ifl = 1 Then
      RezGraphics.PlotRwerte.Add("M", GrpRwerte("X")(KeyRe(1)))
    End If
    RezGraphics.PlotRwerte("R").Name = Texxt(886)
    RezGraphics.Text = ""
    For i = 0 To chkWIN.Count - 1
      RezGraphics.kwopt(i) = chkWIN(i).Checked
    Next

    RezGraphics.Rmax = -1.0
    RezGraphics.Rmin = -1.0

    picREF.Refresh()
    RezGraphics.Skalier()
    '
    '
    ''
  End Sub
  Private Sub btnREF_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  btnREF_0.Click, btnREF_1.Click, btnREF_2.Click
    Dim index As Integer
    If sender.name = "" Then Exit Sub
    index = CInt(sender.name.substring(7, 1))
    RcmdRef = index
    KeyR = KeyRe(RcmdRef + 1)
    GetPutReflex.Messrefel = GrpRwerte("X")(KeyR)
    GrpRwerte("X")(KeyR).kwb = 1
    GrpRwerte("X")(KeyR).IVoNa = False
    lblREF(index).Text = " "
    GetPutReflex.Iarch = 0
    GetPutReflex.Captext = btnREF(index).Text
    'GetPutReflex.Kenn = MenueParam.Messg.Kenn
    GetPutReflex.Retr = -1
    GetPutReflex.ReflexWerte(False)
  End Sub

  Private Sub btnSPEI_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSPEI.Click
    Dim ID As Integer
    If Not HandleRwrt.MeldSpeiAllRwrt(False) Then
      Exit Sub
    End If

    GrpRwerte("X")(KeyRe(0)).Name = InputBox(Texxt(370), Texxt(2000), GrpRwerte("X")(KeyRe(0)).Name)
    If GrpRwerte("X")(KeyRe(0)).Name <> "" Then
      If Len(GrpRwerte("X")(KeyRe(0)).Name) > 40 Then
        GrpRwerte("X")(KeyRe(0)).Name = Mid(GrpRwerte("X")(KeyRe(0)).Name, 1, 40)
      End If
      GrpRwerte("X")(KeyRe(0)).Cme = "$$"
      GrpRwerte("X")(KeyRe(0)).Gid = cboGRPRwrt.SelectedValue
      GrpRwerte("X")(KeyRe(0)).MessgID = MenueParam.MessgID
      ReadWrite.WriteRwert(ID, GrpRwerte("X")(KeyRe(0)), ier)
    End If
  End Sub

  Private Sub btnVER_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnVER.Click


    ValVal = CInt(hscAlpha.Maximum * (Log10(MenueParam.Menue.SPR) + 10) / 14)
    hscAlpha.Value = ValVal
    Call hscAlpha_Scroll(hscAlpha, New ScrollEventArgs(ScrollEventType.EndScroll, ValVal))

  End Sub

  Private Sub radKURV_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  radKURV_0.CheckedChanged, radKURV_1.CheckedChanged, radKURV_2.CheckedChanged
    If IsNothing(radKURV) Then Exit Sub
    If radKURV(0).Checked Then
      '
      '
      '
      'R-Kurve für Angleich
      '
      btnREF(0).Visible = True
      btnREF(1).Visible = False
      btnREF(2).Visible = False
      lblREF(0).Visible = True
      lblREF(1).Visible = False
      lblREF(2).Visible = False
    ElseIf radKURV(1).Checked Then
      '
      '
      '
      'R-Kurve untere und obere Grenze
      '
      btnREF(0).Visible = False
      btnREF(1).Visible = True
      btnREF(2).Visible = True
      lblREF(0).Visible = False
      lblREF(1).Visible = True
      lblREF(2).Visible = True
    ElseIf radKURV(2).Checked Then
      '
      '
      '
      'Standardgrenzen
      '
      btnREF(0).Visible = False
      btnREF(1).Visible = False
      btnREF(2).Visible = False
      lblREF(0).Visible = False
      lblREF(1).Visible = False
      lblREF(2).Visible = False
      '

    End If

  End Sub

  Private Sub hscAlpha_Scroll(sender As Object, e As System.Windows.Forms.ScrollEventArgs) Handles hscAlpha.Scroll
    Dim IflSel As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim i As Integer
    Dim j As Integer
    Dim l As Integer
    Dim Mall As Integer
    Dim M As Integer
    Dim N As Integer
    Dim MDB As Integer
    Dim NB As Integer
    Dim ier As Integer
    Dim CMYK(3) As Single
    Dim RGB(2) As Integer
    Dim XYZ(2) As Single
    Dim ALAB(2) As Single
    Dim ALCH(2) As Single
    Dim XYZS(,,) As Single
    Dim XYZI(,,) As Single
    Dim Diff() As Single
    Dim index() As Integer
    Dim SplitCMYK() As String
    Dim StrCMYK As String
    Dim Tau As Double = 0.000001
    Dim Krank As Integer
    Dim MaCMYK(,) As Double
    Dim MaXYZ(,) As Double
    Dim MaCMYKAll(,) As Double
    Dim MaXYZAll(,) As Double
    Dim XYZ0(2) As Single
    Dim xyY(2) As Single
    Dim Hilf As Single
    Dim CMYK0(3) As Single
    Dim StartKomb As Boolean
    Dim LK() As Integer
    Dim Sum As Single
    Dim DE As Single
    Dim DL As Single
    Dim DC As Single
    Dim DH As Single
    Dim DA As Single
    Dim DB As Single
    If IsNothing(TabTdb) OrElse TabTdb.Rows.Count = 0 Then Exit Sub
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    MenueParam.Menue.SPR = 10.0 ^ ((14.0 * hscAlpha.Value / hscAlpha.Maximum) - 10.0)
    chkREF.Checked = False
    btnSPEI.Enabled = False
    picREF.Visible = False
    picFarbeS.Visible = False
    picFarbeI.Visible = False
    Mdim = 3 * MenueParam.Normfa.Nlz - 1
    ReDim Fako(Winkel.Km - 1, Mdim)
    For k = 0 To Winkel.Km - 1
      chkWIN(k).Enabled = False
    Next k
    For i = 0 To radKODIF.Count - 1
      If radKODIF(i).Checked Then
        Isch = i + 1
      End If
    Next i
    If radANGL(0).Checked Then
      Ifw = 1
    Else
      Ifw = 2
    End If
    For i = 0 To 2
      If radKURV(i).Checked Then
        IflSel = i + 1
      End If
    Next i
    Ifl = 3
    Select Case IflSel
      Case 1
        '
        '
        'R-Kurve für Angleich
        '
        If GrpRwerte("X")(KeyRe(1)).IVoNa Then
          Ifl = 1
        End If
      Case 2
        If GrpRwerte("X")(KeyRe(2)).IVoNa And GrpRwerte("X")(KeyRe(3)).IVoNa Then
          Ifl = 2
        End If
    End Select
    radKURV(Ifl - 1).Checked = True
    If Isch < 7 Then
      For k = 0 To Mdim
        For i = 0 To Winkel.Km - 1
          Fako(i, k) = Singl(TabTdb.Rows(k)(i + 1))
        Next i
      Next k
    ElseIf Isch = 7 Then
      '
      'R,G,B
      ' 
      ' 
      '
      Isch = 4
      For k = 0 To 2
        RGB(k) = CInt(TabTdb.Rows(k)(1))
        If RGB(k) < 0 Or RGB(k) > 255 Then
          MsgBox(Texxt(4704) & Space(1) & CStr(RGB(k)))
          Exit Sub
        End If
      Next k
      Call RGBDar(RGB, XYZ, MenueParam.Normfa(0).NormFakt)
      For k = 0 To 2
        Fako(0, k) = XYZ(k)
      Next
    ElseIf Isch = 8 Then
      '
      '
      '
      '#######MANKISPEZIAL
      '
      '
      '
      '
      '
      'C,M,Y,K
      '
      '
      Isch = 4
      '
      '
      '
      'X,Y,Z Umrechnung
      '
      '
      '
      If TabCMYK Is Nothing Then
        Exit Sub
      End If
      For k = 0 To 3
        CMYK(k) = CInt(TabTdb.Rows(k)(1))
        If CMYK(k) < 0 Or CMYK(k) > 100 Then
          MsgBox(Texxt(4704) & Space(1) & CStr(CMYK(k)))
          Exit Sub
        End If
      Next k
      '
      '
      'Sortieren
      '
      '
      ReDim Diff(TabCMYK.Rows.Count - 1)
      For i = 0 To Diff.Length - 1
        SplitCMYK = TabCMYK.Rows(i)("RWERT_BEM").split("-")
        Diff(i) = 0.0
        For j = 0 To 3
          Diff(i) = Diff(i) + (CSng(SplitCMYK(j)) - CMYK(j)) ^ 2
        Next
      Next i
      index = HeapSort(Diff)
      Mall = Min(20, index.Length)
      M = Min(4, index.Length)
      N = 4
      MDB = Max(M, N)
      NB = 3
      ReDim MaXYZAll(NB - 1, Mall - 1)
      ReDim MaCMYKAll(N - 1, Mall - 1)
      ReDim LK(M - 1)
      ReDim MaXYZ(NB - 1, MDB - 1)
      ReDim MaCMYK(N - 1, M - 1)
      '
      'Umspeichern

      '
      For i = 0 To Mall - 1
        SplitCMYK = TabCMYK.Rows(index(i))("RWERT_BEM").split("-")
        For j = 0 To N - 1
          MaCMYKAll(j, i) = CSng(SplitCMYK(j))
        Next j
        MaXYZAll(0, i) = TabCMYK.Rows(index(i))("X")
        MaXYZAll(1, i) = TabCMYK.Rows(index(i))("Y")
        MaXYZAll(2, i) = TabCMYK.Rows(index(i))("Z")
      Next i
      '
      StartKomb = True
      Dim a(,) As Double
      ReDim a(N - 1, M - 1)
      Dim b(,) As Double
      ReDim b(NB - 1, MDB - 1)
      '
      '
      '
      '
      For k = N To N - 2 Step -1
        Do While GetKombination(StartKomb, M, Mall, LK)
          For i = 0 To M - 1
            For j = 0 To N - 1
              MaCMYK(j, i) = MaCMYKAll(j, LK(i))
            Next j
            For j = 0 To NB - 1
              MaXYZ(j, i) = MaXYZAll(j, LK(i))
            Next j
          Next i
          '
          '
          'Mittelwerte 
          '
          For j = 0 To N - 1
            Sum = 0.0
            For i = 0 To M - 1
              Sum = Sum + MaCMYK(j, i)
            Next i
            CMYK0(j) = Sum / M
          Next j
          For j = 0 To NB - 1
            Sum = 0.0
            For i = 0 To M - 1
              Sum = Sum + MaXYZ(j, i)
            Next i
            XYZ0(j) = Sum / M
          Next j
          '
          '
          '
          'Matrix korrigieren
          '
          For j = 0 To N - 1
            For i = 0 To M - 1
              MaCMYK(j, i) = MaCMYK(j, i) - CMYK0(j)
              a(j, i) = MaCMYK(j, i)
            Next i
          Next j
          '
          '
          '

          For j = 0 To NB - 1
            For i = 0 To M - 1
              MaXYZ(j, i) = MaXYZ(j, i) - XYZ0(j)
              b(j, i) = MaXYZ(j, i)
            Next i
          Next j



          Call MATInvers(M, N, MaCMYK, MDB, NB, MaXYZ, Tau, Krank, ier)
          If Krank = k Then
            Exit For
          End If

          If ier < 0 Then
            MsgBox("Error Matinvers")
            Exit Sub
          End If
        Loop
      Next k
      '
      '
      'Testen
      '
      '
      '
      '
      '
      'Neue X,Y,Z -Werte berechnen
      '
      '
      '
      For k = 0 To NB - 1
        Sum = 0.0
        For j = 0 To N - 1
          Sum = Sum + (CMYK(j) - CMYK0(j)) * MaXYZ(k, j)
        Next
        Fako(0, k) = XYZ0(k) + Sum
        If Fako(0, k) < 0 Then
          MsgBox(Texxt(4705))
          Fako(0, k) = 0.0
        End If
      Next k
      '
      '
      '
      lstCMYK.Visible = True
      chkRwerte.Visible = True
      lstCMYK.Items.Clear()
      For i = 0 To M - 1
        k = index(LK(i))
        If chkRwerte.Checked Then
          StrCMYK = TabCMYK.Rows(k)("RWERT_NAME")
        Else
          StrCMYK = TabCMYK.Rows(k)("RWERT_BEM")
        End If
        lstCMYK.Items.Add(StrCMYK)
      Next i
      '
      '
      '
      '#######MANKISPEZIAL
      '
      '
      '
      '
    ElseIf Isch = 9 Then
      '
      '
      '
      'x,y,Y
      '
      '
      'Umrechnung in X,Y,Z
      '
      Isch = 4
      '
      '
      '
      For i = 0 To Winkel.Km - 1
        For j = 0 To MenueParam.Normfa.Nlz - 1
          For l = 0 To 2
            k = 3 * j + l
            xyY(l) = Singl(TabTdb.Rows(k)(i + 1))
          Next l
          '
          'Y/y
          '
          Hilf = xyY(2) / (xyY(1) + Tau)
          '
          For l = 0 To 2
            k = 3 * j + l
            Select Case l
              Case 0
                Fako(i, k) = xyY(0) * Hilf
              Case 1
                Fako(i, k) = xyY(2)
              Case 2
                Fako(i, k) = (1.0 - xyY(0) - xyY(1)) * Hilf
            End Select
          Next l
        Next j
      Next i

    End If
    Cursor = Cursors.WaitCursor


    Call quali.RefFarbWerte(Winkel, Isch, Ifl, Ifw, Fako, XYZS, XYZI, GrpRwerte("X"), ier)
    If ier <> 0 Then
      MsgBox(Texxt(Abs(4099)))
      If ier > 0 Then
        Cursor = Cursors.Default
        Exit Sub
      End If
    End If
    TabRef.Rows.Clear()
    Call HandleRwrt.TabRefRecord(0, Winkel, 100.0, GrpRwerte("X")(0), TabRef, ier)
    If ier = 0 Then

      btnSPEI.Enabled = True
      Call PlottUmrech(Ifl)
      picREF.Visible = True
      picFarbeS.Visible = True
      picFarbeI.Visible = True
      For k = 0 To Winkel.Km - 1
        chkWIN(k).Enabled = True
      Next k
      Call PlottUmrech(Ifl)
      k = cboLicht.SelectedIndex
      kw = cboWinkel.SelectedIndex
      For i = 0 To 2
        XYZ(i) = XYZS(kw, k, i)
      Next
      Call FarbDar(RGB, XYZ, MenueParam.Normfa(k).NormFakt)
      picFarbeS.BackColor = Color.FromArgb(RGB(0), RGB(1), RGB(2))
      For i = 0 To 2
        XYZ(i) = XYZI(kw, k, i)
      Next
      Call FarbDar(RGB, XYZ, MenueParam.Normfa(k).NormFakt)
      picFarbeI.BackColor = Color.FromArgb(RGB(0), RGB(1), RGB(2))
      quali.FarbDifferenzXYZ(kw, k, Winkel, XYZS, XYZI, DE, DL, DC, DH, DA, DB, ier)
      txtDE.Text = Format(DE, "##0.00")
    End If
    Cursor = Cursors.Default
  End Sub


  Private Sub chkREF_CheckedChanged(sender As Object, e As System.EventArgs) Handles chkREF.CheckedChanged
    If Not Advanced Then Exit Sub
    If chkREF.Checked Then
      dbgRef.Visible = True
      picREF.Visible = False
      btnKOP.Enabled = True
    Else
      dbgRef.Visible = False
      picREF.Visible = True
      btnKOP.Enabled = False
    End If

  End Sub

  Private Sub btnKOP_Click(sender As Object, e As System.EventArgs) Handles btnKOP.Click
    '
    'Schreiben nach Clipboard
    '
    Clipboard.Clear()
    '
    'Grid in Zwischenablage
    '
    '
    '
    '
    dbgRef.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
    dbgRef.SelectAll()

    If dbgRef.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
      Clipboard.SetDataObject(dbgRef.GetClipboardContent)
    End If
    dbgRef.ClearSelection()
  End Sub
  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtDE.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub


  Private Sub radANGL_Click(sender As Object, e As System.EventArgs) Handles radANGL_0.Click, radANGL_1.Click
    btnVER.PerformClick()
  End Sub

  Private Sub cboWinkel_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboWinkel.SelectedIndexChanged, cboLicht.SelectedIndexChanged
    Call hscAlpha_Scroll(hscAlpha, New ScrollEventArgs(ScrollEventType.EndScroll, ValVal))

  End Sub


End Class