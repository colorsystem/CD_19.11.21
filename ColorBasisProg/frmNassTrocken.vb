Option Compare Text
Option Strict Off
Option Explicit On
Public Class frmNassTrocken

  Dim GrpRwerte As RefValuesGrp
  Dim quali As QualKontrolle
  Dim RwrtReadWrite As ReadWriteRwert
  Dim WithEvents GetRwerteNass As HandleRwerte
  Dim WithEvents GetRwerteTrocken As HandleRwerte
  Dim Handlerwert As HandleRwerte
  Dim TblRefWerte As DataTable
  Dim TblMessdevice As DataTable
  Dim AdaptMessdevice As OleDbDataAdapter
  Dim ViewNass As DataView
  Dim ViewTrocken As DataView
  Dim MessgTrockenID As Integer
  Dim MessgNassID As Integer
  Dim IanzTrocken As Integer
  Dim IanzNass As Integer
  Dim radWIN As List(Of RadioButton)
  Dim RefWerteNass As RefValue
  Dim RefWerteTrocken As RefValue
  Dim FarbDLDaDb As List(Of Single(,))
  Dim DLDaDb(,) As Single

  Private Sub frmNassTrocken_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    GetRwerteTrocken = Nothing
    GetRwerteNass = Nothing
    AufbauPar.MessgID = MessgNassID
    AufbauPar.MethID = -1

  End Sub

  Private Sub frmNassTrocken_FormClosing(sender As Object, e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
  End Sub

  Private Sub frmNassTrocken_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    'FormMDI.Text = MenueParam.Messg.Kbez & " - " & MenueParam.Menue.MethBez
    Me.Text = Texxt(1800 + MenueParam.MethID) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    Dim KM As Integer
    lblMessNass.Text = Texxt(3771)
    lblMessTrocken.Text = Texxt(3770)
    btnVorlageTrocken.Text = Texxt(3772)
    btnNachstellungTrocken.Text = Texxt(3773)
    btnVorlageNass.Text = Texxt(3774)
    btnNachstellungNass.Text = Texxt(3775)
    btnDelete.Text = Texxt(3776)
    btnStore.Text = Texxt(3777)
    lblGRP.Text = Texxt(386)
    chkRwerte.Text = Texxt(3110)
    btnKOP.Text = Texxt(389)

    '
    'If MenueParam.Messg.Winkel.Km > 1 Then
    'MsgBox(Texxt(3569))
    'Me.Close()
    'Exit Sub
    'End If
    '
    'Rechnung immer mit MESSG.WINKEL (nicht USER.WINKEL)
    '
    chkDLDADBN.Text = "DL,Da,Db (" & Texxt(3781) & ")"
    GetRwerteTrocken = New HandleRwerte
    GetRwerteNass = GetPutReflex

    '
    Handlerwert = New HandleRwerte
    Handlerwert.cboGRP = cboGRP
    Handlerwert.lblGRP = lblGRP
    Call Handlerwert.GRoupList()
    cboGRP.SelectedValue = MenueParam.Messg.RwrtGID

    For i = 0 To cboGRP.Items.Count - 1
      If cboGRP.Items(i)("GROUP_ID") = MenueParam.Messg.RwrtGID Then
        cboGRP.SelectedIndex = i
      End If
    Next

    If cboGRP.SelectedIndex = -1 Then
      cboGRP.SelectedIndex = 0
    End If

    quali = New QualKontrolle
    RwrtReadWrite = New ReadWriteRwert

    '
    '
    TblRefWerte = New DataTable
    Call AufbauTblRefWerte()
    dbgNassTrocken.DataSource = TblRefWerte
    Call AufbauGrid()

    '
    '
    '
    '
    RefWerteNass = New RefValue
    RefWerteTrocken = New RefValue

    '
    '
    '
    GrpRwerte = New RefValuesGrp
    GrpRwerte.Add("N", New RefValues)
    GrpRwerte.Add("T", New RefValues)
    GrpRwerte.Add("D", New RefValues)
    GrpRwerte.Add("X", New RefValues) '

    '
    '
    radWIN = New List(Of RadioButton)
    radWIN.Clear()
    radWIN.Add(radWIN_0)
    radWIN.Add(radWIN_1)
    radWIN.Add(radWIN_2)
    radWIN.Add(radWIN_3)
    radWIN.Add(radWIN_4)
    radWIN.Add(radWIN_5)
    radWIN.Add(radWIN_6)
    radWIN.Add(radWIN_7)
    radWIN.Add(radWIN_8)
    '
    KM = MenueParam.Messg.Winkel.Km
    For i = 0 To radWIN.Count - 1
      radWIN(i).Visible = False
    Next i
    For i = 0 To KM - 1
      radWIN(i).Visible = True
      radWIN(i).Text = MenueParam.Messg.Winkel(i).Chrm
    Next i
    '
    FarbDLDaDb = New List(Of Single(,))



    '
    '
    '
    TblMessdevice = New DataTable
    ViewNass = New DataView(TblMessdevice)
    ViewTrocken = New DataView(TblMessdevice)
    AdaptMessdevice = New OleDbDataAdapter
    AdaptMessdevice.SelectCommand = New OleDbCommand("", Cncol)
    AdaptMessdevice.SelectCommand.CommandText = "SELECT TBL_MESSG.MESSG_ID AS MESSG_ID,MESSG_KBEZ FROM (TBL_MESSG INNER JOIN TBL_USER_MESSG ON TBL_MESSG.MESSG_ID=TBL_USER_MESSG.MESSG_ID) WHERE USER_ID=" & MenueParam.UserID
    If Not FillDatset(AdaptMessdevice, TblMessdevice) Then
      Exit Sub
    End If
    cboMessNass.DataSource = ViewNass
    cboMessNass.ValueMember = "MESSG_ID"
    cboMessNass.DisplayMember = "MESSG_KBEZ"
    cboMessTrocken.DataSource = ViewTrocken
    cboMessTrocken.ValueMember = "MESSG_ID"
    cboMessTrocken.DisplayMember = "MESSG_KBEZ"
    cboMessNass.SelectedValue = MenueParam.MessgID
    cboMessTrocken.SelectedValue = MenueParam.MessgID
    MessgNassID = MenueParam.MessgID
    MessgTrockenID = MenueParam.MessgID
  End Sub
  Private Sub frmMankiewFarbwerte_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
    AufbauPar.MethID = -1

  End Sub


  Private Sub frmMankiewRezept_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize
    If Not Me.Visible Then Exit Sub

    Call ResizeChild(Me)
  End Sub


  Private Sub btnDelete_Click(sender As System.Object, e As System.EventArgs) Handles btnDelete.Click
    Call Neustart()
  End Sub


  Private Sub cboMessTrocken_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboMessTrocken.SelectedValueChanged
    If Not IsNumeric(cboMessTrocken.SelectedValue) Then Exit Sub
    MessgTrockenID = cboMessTrocken.SelectedValue
    Call Neustart()
  End Sub

  Private Sub cboMessNass_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboMessNass.SelectedValueChanged
    If Not IsNumeric(cboMessNass.SelectedValue) Then Exit Sub
    MessgNassID = cboMessNass.SelectedValue
  End Sub
  Private Sub GetRwerteNass_HdlGetNameID(ByRef ID As Integer, ByRef Name As String, ByRef Banum As String) Handles GetRwerteNass.HdlGetNameID
    If IanzNass >= IanzTrocken Then
      MsgBox(Texxt(3782))
      Exit Sub
    End If
    TblRefWerte.Rows(IanzNass)("IDN") = ID
    TblRefWerte.Rows(IanzNass)("NAMEN") = Name
    IanzNass = IanzNass + 1

    If IanzNass = IanzTrocken Then
      Me.Enabled = True
      btnVorlageNass.Enabled = True
    Else
      btnVorlageNass.Enabled = False
    End If

  End Sub
  Private Sub GetRwerteTrocken_HdlGetNameID(ByRef ID As Integer, ByRef Name As String, ByRef Banum As String) Handles GetRwerteTrocken.HdlGetNameID
    If TblRefWerte.Rows.Count <= IanzTrocken Then
      TblRefWerte.Rows.Add(TblRefWerte.NewRow)
    End If
    TblRefWerte.Rows(IanzTrocken)("IDT") = ID
    TblRefWerte.Rows(IanzTrocken)("NAMET") = Name
    TblRefWerte.Rows(IanzTrocken)("XXX") = "X"
    IanzTrocken = IanzTrocken + 1
  End Sub
  Private Sub btnTrocken_Click(sender As System.Object, e As System.EventArgs) Handles btnVorlageTrocken.Click, btnNachstellungTrocken.Click
    Dim i As Integer
    If sender.name = "btnVorlageTrocken" Then
      AufbauPar.MessgID = MessgTrockenID
      cboMessTrocken.Enabled = False
    End If
    Me.Enabled = False
    GetRwerteTrocken.Messrefel = RefWerteTrocken
    GetRwerteTrocken.Iarch = 0


    If sender.name = "btnVorlageTrocken" Then
      GetRwerteTrocken.Captext = Texxt(3772)
      Call GetRwerteTrocken.ReflexWerte(False)
      If IanzTrocken = 1 Then
        btnDelete.Enabled = True
        btnNachstellungTrocken.Enabled = True
        btnVorlageTrocken.Enabled = False

      End If
    Else
      GetRwerteTrocken.Captext = Texxt(3773)
      Call GetRwerteTrocken.ReflexWerte(True)
      If IanzTrocken > 1 Then
        btnNachstellungNass.Enabled = True
      End If
    End If
    Me.Enabled = True
  End Sub
  Private Sub btnNachstellungNass_Click(sender As Object, e As System.EventArgs) Handles btnNachstellungNass.Click
    AufbauPar.MessgID = MessgNassID

    Me.Enabled = False
    GetRwerteNass.Messrefel = RefWerteNass
    GetRwerteNass.Iarch = 0
    GetRwerteNass.Captext = Texxt(3775)
    Call GetRwerteNass.ReflexWerte(True)
    btnNachstellungNass.Enabled = True
    Me.Enabled = True
  End Sub
  Private Sub btnVorlageNass_Click(sender As System.Object, e As System.EventArgs) Handles btnVorlageNass.Click
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim KMTrocken As Integer
    Dim KMNass As Integer
    Dim ier As Integer
    Dim WSOLTrocken() As Single
    Dim Anz As Single
    Dim RMIT(,) As Single
    Dim DL As Single
    Dim DA As Single
    Dim DB As Single
    Dim DC As Single
    Dim DH As Single
    Dim DE As Single
    Dim DDE As Single
    Dim FAKO(,) As Single
    Dim XYZS(,,) As Single
    Dim XYZI(,,) As Single
    Dim KM As Integer
    '
    'Berechnung der R-Werte für die nasse Vorlage
    '
    '
    If IanzNass <> IanzTrocken Then
      MsgBox(Texxt(3783))
      Exit Sub
    End If
    '
    FarbDLDaDb.Clear()
    KM = MenueParam.Messg.Winkel.Km
    ReDim DLDaDb(KM - 1, 5) 'DL,DA,DB (Tracken), DL,DA,DB (Nass)
    ReDim FAKO(KM - 1, 2)
    For i = 0 To TblRefWerte.Rows.Count - 1
      FarbDLDaDb.Add(DLDaDb.Clone)
    Next
    ReDim XYZS(KM - 1, 0, 2)
    ReDim XYZI(KM - 1, 0, 2)


    '
    'R-Werte der trockenen Vorlage + Nachstellungen
    '
    '

    'Messgerät (trocken) wird aktiviert
    '
    AufbauPar.MessgID = MessgTrockenID
    KMTrocken = MenueParam.Messg.Winkel.Km
    ReDim WSOLTrocken(MenueParam.Messg.Winkel.Wsol.Nwe - 1)
    For i = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
      WSOLTrocken(i) = MenueParam.Messg.Winkel.Wsol.R(i)
    Next
    GrpRwerte("T").clear()
    For i = 0 To TblRefWerte.Rows.Count - 1
      GrpRwerte("T").Add(KeyRe(i), New RefValue)
      Call RwrtReadWrite.ReadRwert(TblRefWerte.Rows(i)("IDT"), GrpRwerte("T")(KeyRe(i)), ier)
    Next i
    '
    '
    'Messgerät (nass) wird aktiviert
    '
    '
    '
    AufbauPar.MessgID = MessgNassID
    KMNass = MenueParam.Messg.Winkel.Km
    If KMNass <> KMTrocken Then
      MessageBox.Show(Texxt(3024) & ": " & KMTrocken & "," & KMNass, Texxt(2004), MessageBoxButtons.OK)
      Exit Sub
    End If
    KM = KMNass

    '
    '
    '
    'Umrechnung in Wellenlängen der Nassmessung
    '
    GrpRwerte("D").clear()
    '
    For i = 0 To GrpRwerte("T").Count - 1
      GrpRwerte("D").Add(KeyRe(i), New RefValue)
      Call ADDCurves(GrpRwerte("D")(KeyRe(i)).RefKurv)
    Next i
    '
    '
    GrpRwerte("N").clear()
    '
    'Platzhalter für Mittelwert
    '
    GrpRwerte("N").Add(KeyRe(0), New RefValue)
    Call ADDCurves(GrpRwerte("N")(KeyRe(0)).RefKurv)
    '
    '
    'GrpRwerte("N")(KeyRe(0)).RefKurv.Add(MenueParam.Messg.Winkel(0).Chrm, New CurveRef(MenueParam.Messg.Winkel.Wsol.Nwe))

    '
    'R-Werte der nassen Nachstellungen
    '
    '
    For i = 1 To TblRefWerte.Rows.Count - 1
      GrpRwerte("N").Add(KeyRe(i), New RefValue)
      Call RwrtReadWrite.ReadRwert(TblRefWerte.Rows(i)("IDN"), GrpRwerte("N")(KeyRe(i)), ier)
    Next i
    '

    '
    'Umrechnung in   Wellenlängen des neuen Messgerätes
    '
    '
    For kw = 0 To KM - 1
      For i = 0 To GrpRwerte("T").Count - 1
        Call CalcSpline(WSOLTrocken, GrpRwerte("T")(KeyRe(i)).RefKurv(kw).R, MenueParam.Messg.Winkel.Wsol.R, GrpRwerte("D")(KeyRe(i)).RefKurv(kw).R, ier)
      Next i
      '
      '
      '
      '
      'Farbabstände der trockenen Messung  
      '
      '
      '
      For i = 1 To TblRefWerte.Rows.Count - 1
        Call quali.FarbDifferenzRef(0, kw, 0, MenueParam.Messg.Winkel, GrpRwerte("D")(KeyRe(0)).RefKurv(kw).R, GrpRwerte("D")(KeyRe(i)).RefKurv(kw).R, DE, DL, DC, DH, DA, DB, ier)
        FarbDLDaDb(i)(kw, 0) = DL 'DLT
        FarbDLDaDb(i)(kw, 1) = DA 'DAT
        FarbDLDaDb(i)(kw, 2) = DB 'DBT
        'TblRefWerte.Rows(i)("DLT") = Format(DL, "##0.000")
        'TblRefWerte.Rows(i)("DAT") = Format(DA, "##0.000")
        'TblRefWerte.Rows(i)("DBT") = Format(DB, "##0.000")

      Next i
      '
      '
      'R-Werte für DL,DA,DB Angleich
      '
      '

    Next kw
    '
    '
    '
    'Mittelwert  für  Rnass(Vorlage)=Rtrocken(Vorlage)+Rnass(Nachstellung)- Rtrocken(Nachstellung)
    '
    '
    ReDim RMIT(KM - 1, MenueParam.Messg.Winkel.Wsol.Nwe - 1)
    For kw = 0 To KM - 1
      For j = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
        RMIT(kw, j) = 0
      Next j
    Next kw

    Anz = 0.0
    For i = 1 To TblRefWerte.Rows.Count - 1
      If TblRefWerte.Rows(i)("XXX") = "X" Then
        Anz = Anz + 1.0
        If chkRwerte.Checked Then
          For kw = 0 To KM - 1
            For j = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
              RMIT(kw, j) = RMIT(kw, j) + GrpRwerte("N")(KeyRe(i)).RefKurv(kw).R(j) + GrpRwerte("D")(KeyRe(0)).RefKurv(kw).R(j) - GrpRwerte("D")(KeyRe(i)).RefKurv(kw).R(j)
            Next j
          Next kw
        Else
          For kw = 0 To KM - 1
            '
            'DL,Da,Db Nachstellung(trocken)(keyre(i)) - Vorlage(trocken)(keyre(0))
            '

            Call quali.FarbDifferenzRef(0, kw, 0, MenueParam.Messg.Winkel, GrpRwerte("D")(KeyRe(i)).RefKurv(kw).R, GrpRwerte("D")(KeyRe(0)).RefKurv(kw).R, DE, DL, DC, DH, DA, DB, ier)
            If ier <> 0 Then
              Exit Sub
            End If
            FAKO(kw, 0) = DL
            FAKO(kw, 1) = DA
            FAKO(kw, 2) = DB
          Next kw
          GrpRwerte("X").clear()
          GrpRwerte("X").Add(KeyRe(0), New RefValue)
          GrpRwerte("X").Add(KeyRe(1), GrpRwerte("N")(KeyRe(i)))
          GrpRwerte("X").Add(KeyRe(2), New RefValue)
          GrpRwerte("X").Add(KeyRe(3), New RefValue)
          '
          'R-Werte aus DL,Da,Db berechnen (i-te Trockenmessung ist Bezug)
          '
          '
          Call quali.RefFarbWerte(MenueParam.Messg.Winkel, 2, 1, 1, FAKO, XYZS, XYZI, GrpRwerte("X"), ier)
          If ier > 0 Then
            Exit Sub
          End If
          If ier = -4099 Then
            'TblRefWerte.Rows(i)("XXX") = "!"
          End If
          For kw = 0 To KM - 1
            For j = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
              RMIT(kw, j) = RMIT(kw, j) + GrpRwerte("X")(KeyRe(0)).RefKurv(kw).R(j)
            Next j
          Next kw
        End If
      End If
    Next i


    For kw = 0 To KM - 1
      For j = 0 To GrpRwerte("N")(KeyRe(0)).RefKurv(kw).Nwe - 1
        If Anz > 0 Then
          GrpRwerte("N")(KeyRe(0)).RefKurv(kw).R(j) = RMIT(kw, j) / Anz
        Else
          GrpRwerte("N")(KeyRe(0)).RefKurv(kw).R(j) = GrpRwerte("D")(KeyRe(0)).RefKurv(kw).R(j)
        End If
      Next j
    Next kw
    TblRefWerte.Rows(0)("NAMEN") = Texxt(3784)
    TblRefWerte.Rows(0)("IDN") = -1
    '
    '
    'Farbwerte für Nassmessung
    '
    For i = 1 To TblRefWerte.Rows.Count - 1
      For kw = 0 To KM - 1
        Call quali.FarbDifferenzRef(kw, 0, 0, MenueParam.Messg.Winkel, GrpRwerte("N")(KeyRe(0)).RefKurv(kw).R, GrpRwerte("N")(KeyRe(i)).RefKurv(kw).R, DE, DL, DC, DH, DA, DB, ier)
        FarbDLDaDb(i)(kw, 3) = DL 'DLN
        FarbDLDaDb(i)(kw, 4) = DA 'DAN
        FarbDLDaDb(i)(kw, 5) = DB 'DBN
      Next kw
    Next i
    Call FillTblRef()
    btnStore.Enabled = True
    btnKOP.Enabled = True
  End Sub
  Sub FillTblRef()
    Dim kw As Integer
    Dim i As Integer
    Dim DDE As Single
    If IsNothing(radWIN) OrElse radWIN.Count = 0 Then Exit Sub
    For i = 0 To radWIN.Count
      If radWIN(i).Checked Then
        kw = i
        Exit For
      End If
    Next i
    For i = 1 To TblRefWerte.Rows.Count - 1
      TblRefWerte.Rows(i)("DLT") = Format(FarbDLDaDb(i)(kw, 0), "##0.000")
      TblRefWerte.Rows(i)("DAT") = Format(FarbDLDaDb(i)(kw, 1), "##0.000")
      TblRefWerte.Rows(i)("DBT") = Format(FarbDLDaDb(i)(kw, 2), "##0.000")
      If chkDLDADBN.Checked Then
        '
        'Differenz D(nass)
        '
        '
        TblRefWerte.Rows(i)("DLN") = Format(FarbDLDaDb(i)(kw, 3), "##0.000")
        TblRefWerte.Rows(i)("DAN") = Format(FarbDLDaDb(i)(kw, 4), "##0.000")
        TblRefWerte.Rows(i)("DBN") = Format(FarbDLDaDb(i)(kw, 5), "##0.000")
        DDE = Sqrt(FarbDLDaDb(i)(kw, 3) - FarbDLDaDb(i)(kw, 0)) ^ 2 _
                + (FarbDLDaDb(i)(kw, 4) - FarbDLDaDb(i)(kw, 1)) ^ 2 _
                + (FarbDLDaDb(i)(kw, 5) - FarbDLDaDb(i)(kw, 2) ^ 2)
        TblRefWerte.Rows(i)("DDE") = Format(DDE, "##0.000")
      Else
        '
        'Differenz der Farbabstände (D(nass)-D(trocken))
        '
        '
        TblRefWerte.Rows(i)("DLN") = Format(FarbDLDaDb(i)(kw, 3) - FarbDLDaDb(i)(kw, 0), "##0.000")
        TblRefWerte.Rows(i)("DAN") = Format(FarbDLDaDb(i)(kw, 4) - FarbDLDaDb(i)(kw, 1), "##0.000")
        TblRefWerte.Rows(i)("DBN") = Format(FarbDLDaDb(i)(kw, 5) - FarbDLDaDb(i)(kw, 2), "##0.000")
        DDE = Sqrt((FarbDLDaDb(i)(kw, 3) - FarbDLDaDb(i)(kw, 0)) ^ 2 _
                + (FarbDLDaDb(i)(kw, 4) - FarbDLDaDb(i)(kw, 1)) ^ 2 _
                + (FarbDLDaDb(i)(kw, 5) - FarbDLDaDb(i)(kw, 2)) ^ 2)
        TblRefWerte.Rows(i)("DDE") = Format(DDE, "##0.000")
      End If
    Next i
  End Sub
  Private Sub btnStore_Click(sender As Object, e As System.EventArgs) Handles btnStore.Click
    Dim ID As Integer
    Dim ier As Integer
    If Not Handlerwert.MeldSpeiAllRwrt(False) Then
      Exit Sub
    End If
    GrpRwerte("N")(KeyRe(0)).Name = Texxt(3784)
    GrpRwerte("N")(KeyRe(0)).Name = InputBox(Texxt(370), Texxt(2000), GrpRwerte("N")(KeyRe(0)).Name)
    If GrpRwerte("N")(KeyRe(0)).Name <> "" Then
      If Len(GrpRwerte("N")(KeyRe(0)).Name) > 40 Then
        GrpRwerte("N")(KeyRe(0)).Name = Mid(GrpRwerte("N")(KeyRe(0)).Name, 1, 40)
      End If
      GrpRwerte("N")(KeyRe(0)).Cme = "$$"
      GrpRwerte("N")(KeyRe(0)).Gid = cboGRP.SelectedValue
      GrpRwerte("N")(KeyRe(0)).MessgID = MenueParam.MessgID
      Call RwrtReadWrite.WriteRwert(ID, GrpRwerte("N")(KeyRe(0)), ier)
      TblRefWerte.Rows(0)("NAMEN") = GrpRwerte("N")(KeyRe(0)).Name

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
    dbgNassTrocken.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
    dbgNassTrocken.SelectAll()

    If dbgNassTrocken.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
      Clipboard.SetDataObject(dbgNassTrocken.GetClipboardContent)
    End If
    dbgNassTrocken.ClearSelection()
  End Sub
  Private Sub chkDLDADBN_Click(sender As Object, e As System.EventArgs) Handles chkDLDADBN.Click
    Call AufbauGrid()
    Call FillTblRef()
  End Sub
  Private Sub dbgNassTrocken_CellClick(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dbgNassTrocken.CellClick
    If e.RowIndex = 0 Then
      dbgNassTrocken.AllowUserToDeleteRows = False
      Exit Sub
    Else
      dbgNassTrocken.AllowUserToDeleteRows = True
    End If
    If e.ColumnIndex <= 0 Then Exit Sub
    If dbgNassTrocken.Columns(e.ColumnIndex).Name = "XXX" Then
      If dbgNassTrocken.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = "X" Then
        dbgNassTrocken.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = " "
      Else
        dbgNassTrocken.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = "X"
      End If
      btnVorlageNass.PerformClick()
    End If

  End Sub
  Private Sub dbgNassTrocken_RowsRemoved(sender As Object, e As System.Windows.Forms.DataGridViewRowsRemovedEventArgs) Handles dbgNassTrocken.RowsRemoved
    If IanzTrocken <= 0 Then Exit Sub
    If IanzNass > e.RowIndex Then IanzNass = IanzNass - 1
    IanzTrocken = IanzTrocken - 1
    If IanzNass = IanzTrocken Then
      btnVorlageNass.Enabled = True
    End If
    GrpRwerte("N").clear()
    GrpRwerte("T").clear()

  End Sub

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
  Sub AufbauTblRefWerte()
    TblRefWerte.Columns.Add("IDT", GetType(Integer))
    TblRefWerte.Columns.Add("NAMET", GetType(String))
    TblRefWerte.Columns.Add("DLT", GetType(String))
    TblRefWerte.Columns.Add("DAT", GetType(String))
    TblRefWerte.Columns.Add("DBT", GetType(String))
    TblRefWerte.Columns.Add("IDN", GetType(Integer))
    TblRefWerte.Columns.Add("NAMEN", GetType(String))
    TblRefWerte.Columns.Add("DLN", GetType(String))
    TblRefWerte.Columns.Add("DAN", GetType(String))
    TblRefWerte.Columns.Add("DBN", GetType(String))
    TblRefWerte.Columns.Add("DDE", GetType(String))
    TblRefWerte.Columns.Add("XXX", GetType(String))
  End Sub
  Sub AufbauGrid()
    Dim i As Integer
    For i = 0 To dbgNassTrocken.Columns.Count - 1
      dbgNassTrocken.Columns(i).ReadOnly = True
      dbgNassTrocken.Columns(i).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
    Next i
    dbgNassTrocken.AllowUserToAddRows = False
    dbgNassTrocken.Columns("IDT").Visible = False
    dbgNassTrocken.Columns("IDN").Visible = False
    dbgNassTrocken.Columns("DLT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DAT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DBT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DLN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DAN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DBN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DDE").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("XXX").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter

    '
    '
    '
    dbgNassTrocken.Columns("NAMET").HeaderText = Texxt(3778)
    dbgNassTrocken.Columns("NAMEN").HeaderText = Texxt(3779)
    dbgNassTrocken.Columns("DLT").HeaderText = "DL" & "(" & Texxt(3780) & ")"
    dbgNassTrocken.Columns("DAT").HeaderText = "Da " & "(" & Texxt(3780) & ")"
    dbgNassTrocken.Columns("DBT").HeaderText = "Db " & "(" & Texxt(3780) & ")"
    If chkDLDADBN.Checked Then
      dbgNassTrocken.Columns("DLN").HeaderText = "DL " & "(" & Texxt(3781) & ")"
      dbgNassTrocken.Columns("DAN").HeaderText = "Da " & "(" & Texxt(3781) & ")"
      dbgNassTrocken.Columns("DBN").HeaderText = "Db " & "(" & Texxt(3781) & ")"
    Else
      dbgNassTrocken.Columns("DLN").HeaderText = "DDL " & "(" & Texxt(3781) & ")"
      dbgNassTrocken.Columns("DAN").HeaderText = "DDa " & "(" & Texxt(3781) & ")"
      dbgNassTrocken.Columns("DBN").HeaderText = "DDb " & "(" & Texxt(3781) & ")"
    End If

    dbgNassTrocken.Columns("DDE").HeaderText = "DDEnt "
    dbgNassTrocken.Columns("NAMET").Width = 200
    dbgNassTrocken.Columns("NAMEN").Width = 200
    dbgNassTrocken.Columns("XXX").Width = 30
    dbgNassTrocken.Columns("DLT").Width = 100
    dbgNassTrocken.Columns("DAT").Width = 100
    dbgNassTrocken.Columns("DBT").Width = 100
    dbgNassTrocken.Columns("DLN").Width = 100
    dbgNassTrocken.Columns("DAN").Width = 100
    dbgNassTrocken.Columns("DBN").Width = 100
    dbgNassTrocken.Columns("DDE").Width = 100

    dbgNassTrocken.Columns("DLT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DAT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DBT").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DLN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DAN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DBN").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    dbgNassTrocken.Columns("DDE").DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    '
    dbgNassTrocken.Columns("DLT").DefaultCellStyle.BackColor = Color.WhiteSmoke
    dbgNassTrocken.Columns("DAT").DefaultCellStyle.BackColor = Color.LightPink
    dbgNassTrocken.Columns("DBT").DefaultCellStyle.BackColor = Color.LightBlue
    dbgNassTrocken.Columns("DLN").DefaultCellStyle.BackColor = Color.WhiteSmoke
    dbgNassTrocken.Columns("DAN").DefaultCellStyle.BackColor = Color.LightPink
    dbgNassTrocken.Columns("DBN").DefaultCellStyle.BackColor = Color.LightBlue

  End Sub
  Sub Neustart()
    IanzNass = 1
    IanzTrocken = 0
    TblRefWerte.Rows.Clear()
    btnNachstellungNass.Enabled = False
    btnNachstellungTrocken.Enabled = False
    btnVorlageNass.Enabled = False
    TblRefWerte.Rows.Clear()
    btnVorlageTrocken.Enabled = True
    btnNachstellungTrocken.Enabled = False
    btnNachstellungNass.Enabled = False
    btnStore.Enabled = False
    btnKOP.Enabled = False
    cboMessTrocken.Enabled = True
    GrpRwerte("T").clear()
    GrpRwerte("D").clear()
    GrpRwerte("N").clear()


  End Sub


  Private Sub radWIN_0_CheckedChanged(sender As Object, e As System.EventArgs) Handles radWIN_0.CheckedChanged, radWIN_1.CheckedChanged, radWIN_2.CheckedChanged, radWIN_3.CheckedChanged, _
    radWIN_4.CheckedChanged, radWIN_5.CheckedChanged, radWIN_6.CheckedChanged, radWIN_7.CheckedChanged, radWIN_8.CheckedChanged
    Call FillTblRef()
  End Sub
End Class