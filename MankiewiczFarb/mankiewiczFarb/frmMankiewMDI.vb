Option Compare Text
Option Strict Off
Option Explicit On
Public Class frmMankiewMDI
  Inherits System.Windows.Forms.Form
  Dim CmdMisch As OleDbCommand
  Dim OleMisch As OleDbDataAdapter
  Dim IerMess As Integer
  Dim ier As Integer
  Dim imsg As Integer
  Dim Ifrm As Integer
  Dim BoxItems As List(Of ItemList)
  Public radWinkel As List(Of RadioButton)
  Public chkWinkel As List(Of CheckBox)
  Public radLichtart As List(Of RadioButton)
  Public chkLichtart As List(Of CheckBox)
  Public lblLichtart As List(Of Label)
  Public lblWinkel As List(Of Label)
  Dim Einst As ColSettings
  Dim FRM As Form
  Dim lblDat As List(Of Label)
  Dim txtDat As List(Of TextBox)
  Dim TooltipDatabase As ToolTip
  Dim MethZus As String()
  Dim TabNamen() As String = _
     {"TBL_USER", "TBL_USER_METH", "TBL_USER_MESSG", "TBL_USER_MISCH", _
      "TBL_USER_METH_ANWSG", "TBL_USER_METH_LICHT", "TBL_USER_METH_MESSG", "TBL_USER_METH_MESSG_IHRM", _
      "TBL_USER_METH_PARAM", "TBL_USER_METH_MISCH", "TBL_USER_MISCH_MESSG", "TBL_USER_METH_ANWSG_MERK", _
      "TBL_USER_MESSG_GROUP_DONTSHOW", "TBL_USER_MESSG_GROUP_READONLY", "TBL_USER_MISCH_GROUP_DONTSHOW", "TBL_USER_MISCH_GROUP_READONLY"} 's. **)
  '
  '
  '
  Private Sub frmMankiewMDI_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
  End Sub

  Private Sub frmMankiewMDI_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
    Application.DoEvents()
    Einst = Nothing
  End Sub


  Private Sub frmMankiewMDI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim GlzGrd As Integer
    Dim StdnID As Integer = -1
    Dim sqlstmt As String
    Dim imsg As DialogResult
    TooltipDatabase = New ToolTip
    Application.DoEvents()
    If Not File.Exists(Cncol.DataSource) Then
      MsgBox("File" & " (CNNCOL): " & Cncol.DataSource & " " & "does not exist")
      Me.Close()
      Exit Sub
    End If
    Application.DoEvents()
    If Not File.Exists(Cndat.DataSource) Then
      MsgBox("File" & " (CNNDAT): " & Cndat.DataSource & " " & "does not exist")
      Me.Close()
      Exit Sub
    End If
    Application.DoEvents()
    If Not File.Exists(Cnkal.DataSource) Then
      MsgBox("File" & " (CNNKAL): " & Cnkal.DataSource & " " & "does not exist")
      Me.Close()
      Exit Sub
    End If
    Application.DoEvents()
    If Not File.Exists(Cnsys.DataSource) Then
      MsgBox("File" & " (CNNSYS): " & Cnsys.DataSource & " " & "does not exist")
      Me.Close()
      Exit Sub
    End If
    Try
      MethZus = GetPrivSettings("MANKIEWICZ", "METHID", "", COLORFileName).Split(",")
      For j = 0 To MethZus.Count - 1
        If Not IsNumeric(MethZus(j)) Then
          Throw New System.Exception("No number for METHID")
        End If
      Next
    Catch
      MsgBox("ERROR MANKIEWICZ METHID")
      Me.Close()
      Exit Sub
    End Try

    BasisProg = New clsColorBasis
    '
    lblDat = New List(Of Label)
    txtDat = New List(Of TextBox)
    lblDat.Clear()
    lblDat.Add(lblDat_0)
    lblDat.Add(lblDat_1)
    lblDat.Add(lblDat_2)
    txtDat.Clear()
    txtDat.Add(txtDAT_0)
    txtDat.Add(txtDAT_1)
    txtDat.Add(txtDAT_2)
    lblDat(0).Text = Texxt(342)
    lblDat(1).Text = Texxt(343)
    lblDat(2).Text = Texxt(3532)
    txtDat(0).Text = Cncol.DataSource
    txtDat(1).Text = Cndat.DataSource
    txtDat(2).Text = Cnkal.DataSource
    btnInit.Text = Texxt(208)
    btnKalib.Text = Texxt(206)
    lblMISCH.Text = Texxt(422)
    lblSortiment.Text = Texxt(4604)
    cboMISCH.Text = Texxt(406)
    cboSortiment.Text = Texxt(800)
    btnRezepte.Text = Texxt(4600)
    btnKorrekt.Text = Texxt(4601)
    btnColorthek.Text = Texxt(4602)
    btnFarbwerteWS.Text = Texxt(4688)
    btnFarbwerte.Text = Texxt(4650)
    btnDrucker.Text = Texxt(341)
    Application.DoEvents()
    '
    '
    '
    '
    ReWrRwert = New ReadWriteRwert
    RwWrSortim = New ReadWriteSortiment
    RwWrRezept = New ReadWriteRezept
    '

    'FormRwert.FormMDI = Me
    SortiTable = New DataTable
    AufbauPar.UserID = CInt(GetPrivSettings("IDNUMBERS", "USERID", "-1", COLORFileName()))
    If MenueParam.UserID = -1 Then
      MessageBox.Show(Texxt(3925))
      Me.Close()
      Exit Sub
    End If
    If AufbauPar.ier = -1 Then
      Me.Close()
      Exit Sub
    End If
    '
    '
    'Prüfen auf Standardeinstellungen
    '
    '
    '
    StdnID = CInt(GetPrivSettings("IDNUMBERS", "STDNID", "-1", COLORFileName()))
    If StdnID > -1 Then
      If Not TestUserSettings(StdnID, MenueParam.UserID, TabNamen) Then
        imsg = MessageBox.Show(Texxt(3821) & vbCrLf & Texxt(3822), Texxt(2010), MessageBoxButtons.YesNo)
        If imsg = Windows.Forms.DialogResult.Yes Then
          Cursor = Cursors.WaitCursor
          Call SetStandardSettings(StdnID, MenueParam.UserID, TabNamen)
          AufbauPar.UserID = CInt(GetPrivSettings("IDNUMBERS", "USERID", "-1", COLORFileName()))
          Cursor = Cursors.Default
        End If
      End If
    End If
    '
    '
    '
    '
    AufbauPar.MessgID = CInt(GetPrivSettings("IDNUMBERS", "MESSGID", "-1", COLORFileName()))
    If MenueParam.MessgID = -1 Then
      MessageBox.Show(Texxt(3926))
      Me.Close()
      Exit Sub
    End If

    If AufbauPar.ier = -1 Then
      Me.Close()
      Exit Sub
    End If
    AufbauPar.MischID = CInt(GetPrivSettings("IDNUMBERS", "MISCHID", "-1", COLORFileName()))
    If AufbauPar.ier = -1 Then
      Me.Close()
      Exit Sub
    End If
    Me.Text = MenueParam.User.Name & " - " & MenueParam.Messg.Kbez & " - MCS"
    '
    Warn = BitInt(28, 29, MenueParam.User.Writ)
    '
    '

    '
    'Glanzgrad
    '
    '
    GlzGrd = CInt(GetPrivSettings("MANKIEWICZ", "GLZGRD", "0", COLORFileName()))
    If GlzGrd = 0 Then
      MenueParam.User.Sonst = BitWrtID(False, 22, MenueParam.User.Sonst)
    Else
      MenueParam.User.Sonst = BitWrtID(True, 22, MenueParam.User.Sonst)
    End If
    '
    '
    '
    BtnList = New List(Of Button)
    BtnList.Add(btnInit)
    BtnList.Add(btnKalib)
    BtnList.Add(btnRezepte)
    BtnList.Add(btnKorrekt)
    BtnList.Add(btnColorthek)
    BtnList.Add(btnFarbwerte)
    BtnList.Add(btnFarbwerteWS)
    '
    '
    '
    'Radiobutton Winkel
    '
    '
    radWinkel = New List(Of RadioButton)
    radWinkel.Add(radWinkel_0)
    radWinkel.Add(radWinkel_1)
    radWinkel.Add(radWinkel_2)
    radWinkel.Add(radWinkel_3)
    radWinkel.Add(radWinkel_4)
    radWinkel.Add(radWinkel_5)
    radWinkel.Add(radWinkel_6)
    radWinkel.Add(radWinkel_7)
    '
    '
    '
    'Checkbox Winkel
    '
    '
    chkWinkel = New List(Of CheckBox)
    chkWinkel.Add(chkWinkel_0)
    chkWinkel.Add(chkWinkel_1)
    chkWinkel.Add(chkWinkel_2)
    chkWinkel.Add(chkWinkel_3)
    chkWinkel.Add(chkWinkel_4)
    chkWinkel.Add(chkWinkel_5)
    chkWinkel.Add(chkWinkel_6)
    chkWinkel.Add(chkWinkel_7)
    '
    'Label Winkel
    '
    '
    lblWinkel = New List(Of Label)
    lblWinkel.Add(lblWinkel_0)
    lblWinkel.Add(lblWinkel_1)
    lblWinkel.Add(lblWinkel_2)
    lblWinkel.Add(lblWinkel_3)
    lblWinkel.Add(lblWinkel_4)
    lblWinkel.Add(lblWinkel_5)
    lblWinkel.Add(lblWinkel_6)
    lblWinkel.Add(lblWinkel_7)
    '
    '
    For i = 0 To radWinkel.Count - 1
      radWinkel(i).Visible = False
    Next
    '
    For i = 0 To chkWinkel.Count - 1
      chkWinkel(i).Visible = False
    Next
    '
    '
    For i = 0 To lblWinkel.Count - 1
      lblWinkel(i).Visible = False
    Next
    '
    '
    '
    'Radiobutton Lichtart
    '
    '
    radLichtart = New List(Of RadioButton)
    radLichtart.Add(radLichtart_0)
    radLichtart.Add(radLichtart_1)
    radLichtart.Add(radLichtart_2)
    radLichtart.Add(radLichtart_3)
    radLichtart.Add(radLichtart_4)

    '
    '
    'Checkbox Lichtart
    '
    '
    chkLichtart = New List(Of CheckBox)
    chkLichtart.Add(chkLichtart_0)
    chkLichtart.Add(chkLichtart_1)
    chkLichtart.Add(chkLichtart_2)
    chkLichtart.Add(chkLichtart_3)
    chkLichtart.Add(chkLichtart_4)
    '
    '
    '
    '
    'Label Lichtart
    '
    '
    lblLichtart = New List(Of Label)
    lblLichtart.Add(lblLichtart_0)
    lblLichtart.Add(lblLichtart_1)
    lblLichtart.Add(lblLichtart_2)
    lblLichtart.Add(lblLichtart_3)
    lblLichtart.Add(lblLichtart_4)
    '
    '
    For i = 0 To radLichtart.Count - 1
      radLichtart(i).Visible = False
    Next
    '
    For i = 0 To chkLichtart.Count - 1
      chkLichtart(i).Visible = False
    Next
    '
    '
    '
    For i = 0 To lblLichtart.Count - 1
      lblLichtart(i).Visible = False
    Next
    '
    '
    '
    '
    For j = 0 To BtnList.Count - 1
      If Not BtnList(j) Is Nothing Then
        BtnList(j).Enabled = True
      End If
    Next '
    '
    BtnList(0).Enabled = True
    BtnList(1).Enabled = True
    For i = 2 To BtnList.Count - 1
      If Not BtnList(i) Is Nothing Then
        BtnList(i).Visible = False
      End If
      For j = 0 To MenueParam.User.MethNmeth - 1
        If Not BtnList(i) Is Nothing AndAlso MenueParam.User.MethodID(j) = Methall(i) Then
          BtnList(i).Visible = True
          BtnList(i).Enabled = True
        End If
      Next
    Next
    '
    '
    '
    '
    Einst = New ColSettings
    cboEinst.Visible = BitWrt(1, MenueParam.User.Writ)
    'Try
    'Einst = CreateObject("COLWINSettings.COLSettings", "")
    'cboEinst.Visible = BitWrt(1, MenueParam.User.Writ)
    'Catch
    'cboEinst.Visible = False
    'End Try
    '
    'Einstellungen benutzerabhängig installieren
    '
    'Einstellungen
    '
    '
    cboEinst.Items.Clear()
    For i = 0 To 4
      If BitWrt(13 + i, MenueParam.User.Sonst) Then
        cboEinst.Items.Add(New ListTextID(i, Texxt(411 + i)))
      End If
    Next

    If cboEinst.Items.Count > 0 Then
      cboEinst.SelectedIndex = 0
    Else
      cboEinst.Visible = False
    End If


    '
    '
    '
    '

    MischTable = New DataTable

    '
    '
    '
    OleMisch = New OleDbDataAdapter
    CmdMisch = New OleDbCommand
    CmdMisch.Connection = Cncol()

    Quali = New QualKontrolle
    sqlstmt = "SELECT DISTINCT TBL_MISCH.MISCH_ID AS MISCH_ID,TBL_MISCH.MISCH_KBEZ AS MISCH_KBEZ" _
       & " FROM (TBL_MISCH INNER JOIN TBL_USER_MISCH ON TBL_MISCH.MISCH_ID = TBL_USER_MISCH.MISCH_ID)" _
       & " INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID = TBL_MISCH_MESSG.MISCH_ID" _
       & " WHERE ((TBL_USER_MISCH.USER_ID=" & MenueParam.UserID & ") AND (TBL_MISCH_MESSG.MESSG_ID=" _
       & MenueParam.MessgID & ") AND (MISCH_SET=YES)) ORDER BY MISCH_KBEZ;"
    CmdMisch.CommandText = sqlstmt
    OleMisch.SelectCommand = CmdMisch
    If Not FillDatset(OleMisch, MischTable) Then
      Exit Sub
    End If
    cboMISCH.Enabled = False
    cboMISCH.DataSource = MischTable
    cboMISCH.DisplayMember = "MISCH_KBEZ"
    cboMISCH.ValueMember = "MISCH_ID"
    cboMISCH.Enabled = True
    cboMISCH.SelectedIndex = -1
    For i = 0 To MischTable.Rows.Count - 1
      If MenueParam.MischID = MischTable.Rows(i)("MISCH_ID") Then
        cboMISCH.SelectedIndex = i
        Exit For
      End If
    Next
    '
    '
    '
    '
    '
    BoxItems = New List(Of ItemList)
    BoxItems.Clear()
    BoxItems.Add(New ItemList(0, Texxt(4662)))
    If MethZus.Count > 0 AndAlso MethZus(0) <> "" Then
      For i = 0 To MenueParam.User.MethNmeth - 1
        For j = 0 To MethZus.Count - 1
          If MenueParam.User.MethodID(i) = CInt(MethZus(j)) Then
            Select Case MenueParam.User.MethodID(i)
              Case 110
                BoxItems.Add(New ItemList(110, Texxt(1910)))
              Case 111
                BoxItems.Add(New ItemList(111, Texxt(1911)))
              Case 103
                BoxItems.Add(New ItemList(103, Texxt(1903)))
              Case 50
                BoxItems.Add(New ItemList(50, Texxt(1850)))
              Case 51
                BoxItems.Add(New ItemList(51, Texxt(1851)))
              Case 52
                BoxItems.Add(New ItemList(52, Texxt(1852)))
              Case 53
                BoxItems.Add(New ItemList(53, Texxt(1853)))
              Case 54
                BoxItems.Add(New ItemList(54, Texxt(1854)))
              Case 2
                BoxItems.Add(New ItemList(2, Texxt(1802)))
              Case 3
                BoxItems.Add(New ItemList(3, Texxt(1803)))
              Case 48
                BoxItems.Add(New ItemList(48, Texxt(1848)))
              Case 49
                BoxItems.Add(New ItemList(49, Texxt(1849)))
              Case 47
                BoxItems.Add(New ItemList(47, Texxt(1847)))
            End Select
          End If
        Next j
      Next i
    End If
    cboProgramme.DataSource = BoxItems
    cboProgramme.ValueMember = "ID"
    cboProgramme.DisplayMember = "NAME"
    cboProgramme.SelectedIndex = 0
    If cboProgramme.Items.Count <= 1 Then
      cboProgramme.Visible = False
    End If
    '
    '
    'Messgerät
    '
    If MenueParam.Messg.Ini = 1 Then
      btnInit.Visible = True
    Else
      btnInit.Visible = False
    End If

    'If MenueParam.Messg.Kal = 1 Then
    btnKalib.Visible = True
    'Else
    'btnKalib.Visible = False
    'End If
    '
    Measure = New ColorMeasure.MeasureReflex
    GetRwerte = New HandleRwerte
    GetRwerte.Measure = Measure
    '
    Call Measure.Umspeich(MenueParam.Messg, MenueParam.Normfa(0))

    If Measure.ier <> -999 Then

      MenueParam.Messg.Exists = True
      If MenueParam.Messg.Exists Then
        IerMess = Measure.ier
        '
        '
        'Initialisierung fehlt
        ' 
        '
        If btnInit.Visible Then
          If IerMess = -3 Or IerMess = -4 Then
            If MenueParam.Messg.Ini = 1 Then
              imsg = MsgBox(Texxt(3512) & Chr(13) & Texxt(2307) & "?", MsgBoxStyle.YesNo, Texxt(2000))
              If imsg <> 7 Then
                btnInit.PerformClick()
              Else
                MenueParam.Messg.Exists = False
              End If
            End If
          End If
        End If
        '
        '
        'Kalibrierung fehlt
        '
        '
        '
        If MenueParam.Messg.Kal = 1 Then
          If MenueParam.Messg.Exists Then
            If IerMess = -2 Or IerMess = -4 Then
              If MenueParam.Messg.Kal = 1 Then
                imsg = MsgBox(Texxt(3513) & Chr(13) & Texxt(2308) & "?", MsgBoxStyle.YesNo, Texxt(2000))
                If imsg <> 7 Then
                  btnKalib.PerformClick()
                Else
                  MenueParam.Messg.Exists = False
                End If
              End If
            End If
            '
            '
            'Kalibrierung zu alt
            '
            '
            '
            '
            If MenueParam.Messg.Exists Then
              If IerMess = -1 Then
                imsg = MsgBox(Texxt(3516) & CStr(MenueParam.Messg.KalInt) & Texxt(3517) & Chr(13) & Texxt(2308) & "?", MsgBoxStyle.YesNo, Texxt(2000))
                If imsg <> 7 Then
                  btnKalib.PerformClick()
                Else
                  MenueParam.Messg.Exists = False
                End If
              End If
            End If
          End If
        End If
      End If
      '
    End If
    BasisProg.Resz = True
    BasisProg.GetPutRef = GetRwerte
    BasisProg.PrintSet = pd.PrinterSettings
    BasisProg.MDIForm = FormMDI

    cboEinst.Enabled = True
    cboProgramme.Enabled = True
    cboSortiment.Enabled = True
    Cncol.Close()

  End Sub

  Sub StartForm(ByVal Ifrm As Integer)
    Me.Icon = My.Resources.MCS


    AufbauPar.MethID = -1
    cboAnweisungen.Visible = False
    For i = 0 To radWinkel.Count - 1
      radWinkel(i).Visible = False
    Next
    '
    For i = 0 To chkWinkel.Count - 1
      chkWinkel(i).Visible = False
    Next
    For i = 0 To radLichtart.Count - 1
      radLichtart(i).Visible = False
    Next
    If Not IsNothing(FRM) Then
      Try
        FRM.Close()
        FRM.Dispose()
        FRM = Nothing
      Catch
      End Try
    End If
    Select Case Ifrm
      Case 0
        FRM = Nothing
      Case 1
        FRM = Nothing
      Case 2
        FRM = New frmMankiewRezept
      Case 3
        FRM = New frmMankiewKorrektur
      Case 4
        FRM = New frmMankiewColorthek
      Case 5
        FRM = New frmMankiewFarbwerte(2)
      Case 6
        FRM = New frmMankiewFarbwerte(3)
      Case 7
        FRM = New frmFarbmittelEing
      Case 8
        FRM = New frmColorthekEing
      Case 9
        FRM = New frmColorGrund
      Case 10
        FRM = New frmColorRezepte
      Case 11
        FRM = New frmColorSuchKorr(False)
      Case 12
        FRM = New frmFarbUmrechnung(False)
      Case 13
        FRM = New frmDBExportCOL
      Case 14
        FRM = New frmDBImportCOL
      Case 15
        FRM = New frmQualControl
      Case 16
        FRM = New frmQualControl
      Case 17
        FRM = New frmQualControl
      Case 18
        FRM = New frmQualControl
      Case 19
        FRM = New frmNassTrocken
    End Select
    GC.Collect()
    If Not IsNothing(FRM) Then
      Application.DoEvents()
      For j = 0 To BtnList.Count - 1
        If Not BtnList(j) Is Nothing Then
          BtnList(j).BackColor = Color.FromArgb(236, 233, 216)
          BtnList(j).Enabled = False
        End If
      Next
      If Ifrm < BtnList.Count Then
        BtnList(Ifrm).BackColor = Color.FromArgb(172, 168, 153)
        cboProgramme.SelectedIndex = 0
      End If
      'lblMankiewicz.Visible = False
      Select Case Ifrm
        Case 0, 1
        Case Else
          'btnKalib.Visible = False
          btnInit.Visible = False
      End Select
      Select Case Ifrm
        Case 2
          cboSortiment.Visible = True
          lblSortiment.Visible = True
          If cboSortiment.SelectedIndex > -1 Then
            If cboSortiment.DataSource.rows.count > 0 Then
              cboSortiment.SelectedIndex = 0
            End If
          End If
        Case Else
          cboSortiment.Visible = False
          lblSortiment.Visible = False
          cboSortiment.DataSource = Nothing
      End Select
      Select Case Ifrm
        Case 5, 6
          cboMISCH.Visible = False
          lblMISCH.Visible = False
        Case Else
          cboMISCH.Visible = True
          lblMISCH.Visible = True
          cboMISCH.SelectedIndex = -1
          cboMISCH.SelectedValue = MenueParam.MischID
          If Ifrm > 6 Then
            cboMISCH.Enabled = False
          Else
            cboMISCH.Enabled = True
          End If
      End Select
      '

      '
      '
      PanMDIProgram.Visible = False

      AufbauPar.MethID = Methall(Ifrm)
      Me.Text = ""
      If AufbauPar.ier <> 0 Then
        Exit Sub
      End If

      Try
        FRM.MdiParent = Me
        FRM.Show()
      Catch
        MsgBox("Error in user program")
      End Try
      '
      For j = 0 To BtnList.Count - 1
        If Not BtnList(j) Is Nothing Then
          BtnList(j).Enabled = True
        End If
      Next '
    End If
    Cncol.Close()
  End Sub
  Function Methall(ByVal ifrm As Integer) As Integer
    Dim MethAlle() As Integer = {-1, -2, 53, 54, 55, 2, 3, 50, 51, 52, 53, 54, 103, 110, 111, 2, 3, 48, 49, 47}
    Methall = MethAlle(ifrm)
  End Function



  Private Sub cboMISCH_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMISCH.SelectedIndexChanged
    Dim SqlStmt As String
    Dim WithUser As String
    Dim OleDbSorti As OleDbDataAdapter
    Dim cmdSorti As OleDbCommand
    If Not cboMISCH.Enabled Then Exit Sub
    If cboMISCH.SelectedIndex = -1 Then Exit Sub
    If Not IsNothing(AufbauPar) Then
      '
      '
      'Menueparameter neu aufbauen
      '
      '
      '
      AufbauPar.MischID = MischTable.Rows(cboMISCH.SelectedIndex)("MISCH_ID")
      '
      ' 
      '
      'If BitWrt(10, MenueParam.User.Writ) Then
      ' SqlStmt = "SELECT * FROM " & MenueParam.TableSorti & " WHERE USER_ID=" & MenueParam.UserID _
      ' & " AND MISCH_ID=" & MenueParam.MischID & " ORDER BY SORTI_NAME"
      'Else
      ' SqlStmt = "SELECT * FROM " & MenueParam.TableSorti & " WHERE MISCH_ID=" & MenueParam.MischID & " ORDER BY SORTI_NAME"
      'End If

      '
      'Sortimente
      '
      '
      WithUser = ""
      'If BitWrt(10, MenueParam.User.Writ) Then
      ' WithUser = " AND USER_ID=" & MenueParam.UserID
      'End If
      SqlStmt = "SELECT * FROM TBL_SORTI WHERE MISCH_ID=" & MenueParam.MischID & WithUser & " ORDER BY SORTI_NAME"

      SortiTable.Clear()
      SortID = -1
      OleDbSorti = New OleDbDataAdapter
      cmdSorti = New OleDbCommand(SqlStmt, Cndat)
      OleDbSorti.SelectCommand = cmdSorti
      If Not FillDatset(OleDbSorti, SortiTable) Then
        Exit Sub
      End If
      cboSortiment.DataSource = SortiTable
      cboSortiment.DisplayMember = "SORTI_NAME"
      cboSortiment.ValueMember = "SORTI_ID"
      cboSortiment.SelectedIndex = -1
      If SortiTable.Rows.Count <> 0 Then cboSortiment.SelectedIndex = 0
      '
      '
      'frmMankiewKorrektur
      '
      '
      'If Not IsNothing(Me.ActiveMdiChild) AndAlso Me.ActiveMdiChild Is New frmMankiewKorrektur Then
      If Not IsNothing(Me.ActiveMdiChild) AndAlso MenueParam.MethID = 54 Then
        Call StartForm(3)
      End If
      '
      '
      'frmMankiewColorthek
      '
      '
      If Not IsNothing(Me.ActiveMdiChild) AndAlso MenueParam.MethID = 55 Then
        Call StartForm(4)
      End If
    End If
  End Sub
  Private Sub btnInit_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnInit.Click
    Call Measure.MessDevInit()
    If MenueParam.Messg.Nkal = 1 Then
      Call Measure.MessDevNkalib()
    End If
  End Sub
  Private Sub btnKalib_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnKalib.Click
    Call Measure.MessDevKalib()
    If Measure.ier = 0 Or Measure.ier = 1 Then
      MenueParam.Messg.Exists = True
    Else
      MenueParam.Messg.Exists = False
    End If
  End Sub
  Private Sub btnRezepte_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRezepte.Click
    Ifrm = 2
    Call StartForm(Ifrm)
  End Sub
  Private Sub btnKorrekt_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnKorrekt.Click
    Ifrm = 3
    Call StartForm(Ifrm)
  End Sub
  Private Sub btnColorthek_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnColorthek.Click
    Ifrm = 4
    Call StartForm(Ifrm)
  End Sub

  Private Sub btnFarbwerte_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnFarbwerte.Click
    Ifrm = 5
    Call StartForm(Ifrm)
  End Sub
  Private Sub btnFarbwerteWS_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnFarbwerteWS.Click
    Ifrm = 6
    Call StartForm(Ifrm)
  End Sub
  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()


    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.

  End Sub

  Private Sub btnUpdate_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)   ' btnUpdate ist wech..!
    Dim Land() As String = {"GER", "ENG"}
    Dim i As Integer
    Dim cmdUpd As New OleDbCommand
    cmdUpd.Connection = Cncol()
    Cursor = Cursors.WaitCursor
    For i = 0 To 1
      cmdUpd.CommandText = "DELETE * FROM TBL_TEXTE_" & Land(i)
      If SQLExeNonQuery(cmdUpd, Cncol) <> 0 Then
        ier = -1
        Exit Sub
      End If
      cmdUpd.CommandText = "INSERT INTO TBL_TEXTE_" & Land(i) & " SELECT * FROM TBL_TEXTE_" & Land(i) & " IN '" & Cnsys.DataSource & "'"
      If SQLExeNonQuery(cmdUpd, Cncol) <> 0 Then
        ier = -1
        Exit Sub
      End If
    Next i
    '
    '
    '
    If StringLength("TBL_FARBM", "FARBM_NAME", Cndat) < 128 Then
      cmdUpd.Connection = Cndat()
      cmdUpd.CommandText = "ALTER TABLE TBL_FARBM ALTER COLUMN [FARBM_NAME] TEXT(128)"
      If SQLExeNonQuery(cmdUpd, Cndat) <> 0 Then
        Exit Sub
      End If
      cmdUpd.Dispose()
    End If
    '
    '
    Cursor = Cursors.Default
  End Sub




  Private Sub cboProgramme_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboProgramme.SelectedIndexChanged
    Dim i As Integer
    If Not IsNumeric(cboProgramme.SelectedValue) OrElse cboProgramme.SelectedValue = 0 Then Exit Sub
    For i = 7 To 19
      If Methall(i) = cboProgramme.SelectedValue Then
        Ifrm = i
        Exit For
      End If
    Next i
    Call StartForm(Ifrm)

  End Sub



  Private Sub cboEinst_DropDownClosed(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboEinst.DropDownClosed
    Dim BOOL As Boolean = False
    If Ifrm < 2 Then Exit Sub
    If IsNothing(cboEinst.SelectedItem) Then Exit Sub
    If cboEinst.SelectedItem.id = 2 AndAlso MenueParam.MethID < 50 Then Exit Sub
    If cboEinst.SelectedItem.id = 3 AndAlso MenueParam.MethID >= 50 Then Exit Sub
    If cboEinst.SelectedItem.id = 4 AndAlso MenueParam.MethID >= 50 Then Exit Sub
    Application.DoEvents()
    cboEinst.Text = cboEinst.SelectedItem.text
    Call Einst.EinstellForm(cboEinst.SelectedItem.id, MenueParam.UserID, MenueParam.MessgID, MenueParam.MethID, MenueParam.MischID, BOOL, BOOL, Nothing)
    Call StartForm(Ifrm)
  End Sub

  WriteOnly Property SetEinst() As Boolean
    Set(ByVal value As Boolean)
      cboEinst.Enabled = value
    End Set
  End Property




  Private Sub btnDrucker_Click(sender As System.Object, e As System.EventArgs) Handles btnDrucker.Click
    If pd.ShowDialog = Windows.Forms.DialogResult.OK Then
      Exit Sub
    End If
  End Sub

  Private Sub chkDrucken_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkDrucken.CheckedChanged
    btnDrucker.Enabled = chkDrucken.Checked
  End Sub

  Private Sub PanMDICommands_Click(sender As Object, e As System.EventArgs) Handles PanMDICommands.Click
    Dim Tooltext As String = ""
    Dim i As Integer
    For i = 0 To lblDat.Count - 1
      Tooltext = Tooltext & lblDat(i).Text & ": " & txtDat(i).Text & Chr(13)
    Next

    TooltipDatabase.SetToolTip(PanMDICommands, Tooltext)

  End Sub

 
End Class
