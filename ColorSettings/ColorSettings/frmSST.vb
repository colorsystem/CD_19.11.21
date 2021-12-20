Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmSST
  Dim indexMAT As Integer = -1
  Dim indexSST As Integer = -1
  Dim MeasureDLL(99) As String
  Dim DBFil As String
  Dim DBDat As String
  Dim DBTmp As String
  Dim GridLayout As String
  Dim LogFile As String
  Dim Aktdir As String
  Dim i As Integer
  Dim XYZMat(2, 2) As Single
  Dim RoGrBl(2, 2) As Integer
  Dim AA(2, 2) As Single
  Dim Horgb(2) As Single
  Dim A(2, 2) As Single
  Dim UserEnd As Boolean
  Dim lblDAT As List(Of Label)
  Dim lblMAT As List(Of Label)
  Dim lblXYZ As List(Of Label)
  Dim radSST As List(Of RadioButton)
  Dim radMAT As List(Of RadioButton)
  Dim btnDat As List(Of Button)
  Dim txtXYZ As List(Of TextBox)
  Dim txtActive As List(Of TextBox)
  Dim lblActive As List(Of Label)
  Dim hscMAT As List(Of HScrollBar)
  Dim hscFAR As List(Of HScrollBar)
  '
  '
  Dim AdUser As OleDbDataAdapter
  Dim AdMess As OleDbDataAdapter
  Dim AdMisch As OleDbDataAdapter
  Dim CmdUser As OleDbCommand
  Dim CmdMess As OleDbCommand
  Dim CmdMisch As OleDbCommand
  Dim TabUser As DataTable
  Dim TabMess As DataTable
  Dim TabMisch As DataTable

  Private Sub frmSST_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Me.Size = MDiform.Size
    Me.Location = MDiform.Location
    '
   
    UserEnd = False
    lblDAT = New List(Of Label)
    btnDat = New List(Of Button)
    lblMAT = New List(Of Label)
    lblXYZ = New List(Of Label)
    txtXYZ = New List(Of TextBox)
    txtActive = New List(Of TextBox)
    hscMAT = New List(Of HScrollBar)
    hscFAR = New List(Of HScrollBar)
    radSST = New List(Of RadioButton)
    radMAT = New List(Of RadioButton)
    lblActive = New List(Of Label)

    lblMAT.Clear()
    lblMAT.Add(lblMat_0)
    lblMAT.Add(lblMat_1)
    lblMAT.Add(lblMat_2)

    lblXYZ.Clear()
    lblXYZ.Add(lblXYZ_0)
    lblXYZ.Add(lblXYZ_1)
    lblXYZ.Add(lblXYZ_2)

    lblDAT.Clear()
    lblDAT.Add(lblDAT_0)
    lblDAT.Add(lblDAT_1)
    lblDAT.Add(lblDAT_2)
    lblDAT.Add(lblDAT_3)
    lblDAT.Add(lblDAT_4)
    lblDAT.Add(lblDAT_5)
    lblDAT.Add(lblDAT_6)
    btnDat.Clear()
    btnDat.Add(btnDAT_0)
    btnDat.Add(btnDAT_1)
    btnDat.Add(btnDAT_2)
    btnDat.Add(btnDAT_3)
    btnDat.Add(btnDAT_4)
    btnDat.Add(btnDAT_5)
    btnDat.Add(btnDAT_6)
    txtXYZ.Clear()
    txtXYZ.Add(txtXYZ_0)
    txtXYZ.Add(txtXYZ_1)
    txtXYZ.Add(txtXYZ_2)
  

    txtActive.Clear()
    txtActive.Add(txtActive_00)
    txtActive.Add(txtActive_01)
    txtActive.Add(txtActive_02)
    txtActive.Add(txtActive_03)
    txtActive.Add(txtActive_04)
    txtActive.Add(txtActive_05)
    txtActive.Add(txtActive_06)
    txtActive.Add(txtActive_07)
    txtActive.Add(txtActive_08)
    lblActive.Clear()
    lblActive.Add(lblActive_00)
    lblActive.Add(lblActive_01)
    lblActive.Add(lblActive_02)
    lblActive.Add(lblActive_03)
    lblActive.Add(lblActive_04)
    lblActive.Add(lblActive_05)
    lblActive.Add(lblActive_06)
    lblActive.Add(lblActive_07)
    lblActive.Add(lblActive_08)
    '
    '
    TabControlINI.TabPages(0).Text = Texxt(336)
    TabControlINI.TabPages(1).Text = Texxt(337)
    TabControlINI.TabPages(2).Text = Texxt(338)
    TabControlINI.TabPages(3).Text = Texxt(3762)
    '
    '
    Me.Text = Texxt(349)
    lblSprach.Text = Texxt(30)
    chkDatBas.Text = Texxt(339)
    chkStart.Text = Texxt(23)
    lblMessgKE.Text = Texxt(324)
    btnEND.Text = Texxt(1999)
    '
    '
    btnDat(0).Text = Texxt(31)
    btnDat(1).Text = Texxt(32)
    btnDat(2).Text = Texxt(33)
    btnDat(3).Text = Texxt(34)
    btnDat(4).Text = Texxt(35)
    btnDat(5).Text = Texxt(340)
    btnDat(6).Text = Texxt(520)

    lblActive_00.Text = Texxt(325)
    lblActive_01.Text = Texxt(326)
    lblActive_02.Text = Texxt(327)
    lblActive_03.Text = Texxt(328)
    lblActive_04.Text = Texxt(329)
    lblActive_05.Text = Texxt(330)
    lblActive_06.Text = Texxt(331)
    lblActive_07.Text = Texxt(332)
    lblActive_08.Text = Texxt(333)
    lblMeasure.Text = Texxt(335)
    lblFar_0.Text = Texxt(345)
    lblFar_1.Text = Texxt(346)
    lblFar_2.Text = Texxt(347)
    lblMat_0.Text = Texxt(345)
    lblMat_1.Text = Texxt(346)
    lblMat_2.Text = Texxt(347)
    lblUsMeMiSetting.Text = Texxt(3762)
    lblUSER.Text = Texxt(201)
    lblMess.Text = Texxt(204)
    lblMisch.Text = Texxt(422)
    AdUser = New OleDbDataAdapter
    AdMess = New OleDbDataAdapter
    AdMisch = New OleDbDataAdapter
    CmdUser = New OleDbCommand("", Cncol)
    CmdMess = New OleDbCommand("", Cncol)
    CmdMisch = New OleDbCommand("", Cncol)
    TabUser = New DataTable
    TabMess = New DataTable
    TabMisch = New DataTable
    AdUser.SelectCommand = CmdUser
    AdMess.SelectCommand = CmdMess
    AdMisch.SelectCommand = CmdMisch
    For i = 0 To lblDAT.Count - 1
      lblDAT(i).Text = DbIName(i)
    Next i

    cboSPRACH.Items.Clear()
    cboSPRACH.Items.Add(NameLangName(1))
    cboSPRACH.Items.Add(NameLangName(2))
    cboSPRACH.Items.Add(NameLangName(3))
    cboSPRACH.Items.Add(NameLangName(4))
    cboSPRACH.Items.Add(NameLangName(5))
    cboSPRACH.Items.Add(NameLangName(6))
    cboSPRACH.SelectedIndex = Langindex - 1
    '


    hscMAT.Clear()
    hscMAT.Add(hscMat_0)
    hscMAT.Add(hscMat_1)
    hscMAT.Add(hscMat_2)
    hscFAR.Clear()
    hscFAR.Add(hscFar_0)
    hscFAR.Add(hscFar_1)
    hscFAR.Add(hscFar_2)

    radMAT.Clear()
    radMAT.Add(radMAT_0)
    radMAT.Add(radMAT_1)
    radMAT.Add(radMAT_2)

    radSST.Clear()
    radSST.Add(radSST_00)
    radSST.Add(radSST_01)
    radSST.Add(radSST_02)
    radSST.Add(radSST_03)
    radSST.Add(radSST_04)
    radSST.Add(radSST_05)
    radSST.Add(radSST_06)
    radSST.Add(radSST_07)
    radSST.Add(radSST_08)
    radSST.Add(radSST_09)
    radSST.Add(radSST_10)
    radSST.Add(radSST_11)
    radSST.Add(radSST_12)
    radSST.Add(radSST_13)
    radSST.Add(radSST_14)
    radSST.Add(radSST_15)
    '
    '
    'Messprogramme (Driver 000 bis 099)
    '
    '
    cboMeasure.Items.Clear()
    For i = 0 To 99
      cboMeasure.Items.Add(Format(i, "000"))
      MeasureDLL(i) = vbNullString
    Next

    '
    'Datenbanknamen anzeigen
    '
    '
    chkDatBas.CheckState = CInt(GetPrivSettings("STARTUP", "DISPLY", "0", COLORFileName()))
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    '
    '
    'Start wird automatisch ausgelöst (ohne Eingangsbild)
    '
    '
    chkStart.CheckState = CInt(GetPrivSettings("STARTUP", "STAUTO", "0", COLORFileName()))
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If

    '
    'Kennung für alternatives Messgerät
    '
    '

    lblMessgKE.Visible = True
    txtMessgKE.Visible = True
    txtMessgKE.Text = GetPrivSettings("STARTUP", "MESSGKE", "  ", COLORFileName())
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    If Not UserUfo Then
      txtMessgKE.Enabled = False
    End If

    'Active-X Komponenten Ausgabe Farb-/Bindemittel
    '
    '
    '
    txtActive(0).Text = GetPrivSettings("COMCLASS", "FARBMIT", "", COLORFileName())
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    '
    '
    'Active-X Komponenten Ausgabe Rezepte
    'z.Zt. nicht verwendet
    '
    '
    '
    txtActive(1).Text = GetPrivSettings("COMCLASS", "REZEPTE", "", COLORFileName())
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    '
    '
    'Active-X Komponenten Ausgabe Sortimente
    '
    '
    '
    txtActive(2).Text = GetPrivSettings("COMCLASS", "SORTIME", "", COLORFileName())
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    '
    '
    'Active-X Komponenten Ausgabe allgemein
    '
    '
    '
    txtActive(3).Text = GetPrivSettings("COMCLASS", "ALLGEME", "", COLORFileName())
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    '
    '
    'Active-X Komponenten Rezeptberechnung (alle)
    '
    '
    '
    txtActive(4).Text = GetPrivSettings("COMCLASS", "RZPALLE", "", COLORFileName())
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    '
    '
    'Active-X Komponenten Rezeptberechnung Wiegeschein
    '
    '
    '
    txtActive(5).Text = GetPrivSettings("COMCLASS", "WIEGESC", "", COLORFileName())
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    '
    '
    'Active-X Komponenten Korrekturberechnung
    '
    '
    '
    txtActive(6).Text = GetPrivSettings("COMCLASS", "KORREKT", "", COLORFileName())
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    '
    '
    'Active-X Komponenten Qualitätskontrolle
    '
    '
    '
    txtActive(7).Text = GetPrivSettings("COMCLASS", "QUALITA", "", COLORFileName())
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    '
    '
    'Active-X Komponenten Ausgabe Reflexionswerte
    '
    '
    '
    txtActive(8).Text = GetPrivSettings("COMCLASS", "REWERTE", "", COLORFileName())
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    '
    '
    'Messgeräte
    '
    '
    '
    For i = 0 To cboMeasure.Items.Count - 1
      MeasureDLL(i) = GetPrivSettings("MEASURE", "MEAS" & cboMeasure.Items(i), "", COLORFileName())
    Next
    cboMeasure.SelectedIndex = 0
    radSST_00.Checked = True
    Aktdir = CurDir()
    For i = 0 To btnDat.Count - 1
      btnDat(i).Enabled = True
    Next i
    For i = 0 To txtActive.Count - 1
      txtActive(i).Enabled = True
    Next i
    If Not UserUfo Then
      TabControlINI.TabPages(2).Enabled = False
      TabControlINI.TabPages(3).Enabled = False
      TabControlINI.TabPages(2).Text = ""
      TabControlINI.TabPages(3).Text = ""
    End If
    For i = 0 To radSST.Count - 1
      radSST(i).Text = Format(i + 1, "##") & Texxt(348)
    Next i
    For i = 0 To radMAT.Count - 1
      radMAT(i).Text = Format(i + 1, "##") & Texxt(348)
    Next i
    For i = 0 To lblMAT.Count - 1
      lblXYZ(i).Text = TexKt(10065 + i)
    Next i
  End Sub

  Private Sub frmSST_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
    Dim ier As Integer
    Dim i As Integer
    Dim j As Integer
    Dim ColColor As Color
    Dim rgbrgb(2) As Integer
    Dim xyzxyz(2) As Single
    'If UserUfo = True Then goto SSTEnd
    '
    'Settings speichern
    '
    'Farben für Plott
    '
    If MessageBox.Show(Texxt(12) & COLORFileName() & Texxt(13), Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
      Try

        For i = 0 To farbini.Count - 1
          ColColor = Color.FromArgb(0, farbini(i))
          LetPrivSettings("COLOR", "COLOR" & CStr(i + 1), COLORFileName()) = 256& * (256& * ColColor.R + ColColor.G) + ColColor.B
        Next i
        If Err.Number <> 0 Then
          MsgBox(Err.Description)
        End If
        '
        '
        'Datenbanken anzeigen
        '
        '
        '
        LetPrivSettings("STARTUP", "DISPLY", COLORFileName()) = CInt(chkDatBas.CheckState)
        If Err.Number <> 0 Then
          MsgBox(Err.Description)
        End If
        '
        '
        'Start wird automatisch ausgelöst (ohne Eingangsbild)
        '
        '
        LetPrivSettings("STARTUP", "STAUTO", COLORFileName()) = CInt(chkStart.CheckState)
        If Err.Number <> 0 Then
          MsgBox(Err.Description)
        End If


      Catch ex As Exception

      End Try
      '

      '
      '
      '
      'Matrix für RGB-Darstellung aus Tristimuluswerten und RGB-Werten berechnen
      '
      If indexMAT > -1 Then
        For i = 0 To 2
          For j = 0 To 2
            AA(i, j) = RoGrBl(i, j)
          Next j
        Next i
        '
        '
        Call MatSetBer(AA, XYZMat, A, ier)
        '
        '
        '
        '
        If ier <> 0 Then
          MsgBox(Texxt(439))
        Else
          '
          '
          Try
            Call WriteAHoRGB(A)
            If Err.Number <> 0 Then
              MsgBox(Err.Description)
            End If
          Catch ex As Exception

          End Try
        End If
      End If
      '
      'Dateinamen
      '
      Try
        Cnsys.ConnectionString = Cnsys.ConnectionString.Replace(Cnsys.DataSource, LetFileProfile(lblDAT(0).Text))
        LetPrivSettings("STARTUP", "CNNSYS", COLORFileName()) = Cnsys.ConnectionString
        If Err.Number <> 0 Then
          MsgBox(Err.Description)
        End If
        Cncol.ConnectionString = Cncol.ConnectionString.Replace(Cncol.DataSource, LetFileProfile(lblDAT(1).Text))
        LetPrivSettings("STARTUP", "CNNCOL", COLORFileName()) = Cncol.ConnectionString
        If Err.Number <> 0 Then
          MsgBox(Err.Description)
        End If
        Cndat.ConnectionString = Cndat.ConnectionString.Replace(Cndat.DataSource, LetFileProfile(lblDAT(2).Text))
        LetPrivSettings("STARTUP", "CNNDAT", COLORFileName()) = Cndat.ConnectionString
        If Err.Number <> 0 Then
          MsgBox(Err.Description)
        End If
        Cnkal.ConnectionString = Cnkal.ConnectionString.Replace(Cnkal.DataSource, LetFileProfile(lblDAT(3).Text))
        LetPrivSettings("STARTUP", "CNNKAL", COLORFileName()) = Cnkal.ConnectionString
        If Err.Number <> 0 Then
          MsgBox(Err.Description)
        End If
        CnTmp.ConnectionString = CnTmp.ConnectionString.Replace(CnTmp.DataSource, LetFileProfile(lblDAT(4).Text))
        LetPrivSettings("STARTUP", "CNNTMP", COLORFileName()) = CnTmp.ConnectionString
        If Err.Number <> 0 Then
          MsgBox(Err.Description)
        End If
        '
        'Datei für Grid-Layouts
        '
        '
        '
        LetPrivSettings("STARTUP", "GRIDFIL", COLORFileName()) = LetFileProfile(lblDAT(5).Text)
        If Err.Number <> 0 Then
          MsgBox(Err.Description)
        End If
        '
        '
        'Datei für Log-File
        '
        '
        '
        LetPrivSettings("STARTUP", "LOGFIL", COLORFileName()) = LetFileProfile(lblDAT(6).Text)
        If Err.Number <> 0 Then
          MsgBox(Err.Description)
        End If
        Cnsys.Close()
        Cncol.Close()
        Cndat.Close()
        Cnkal.Close()
        Cnsys.ConnectionString = ""
        Cncol.ConnectionString = ""
        Cndat.ConnectionString = ""
        Cnkal.ConnectionString = ""
        'Datenbank anzeigen
        '

        '
        '
        '
        If UserUfo Then
          If Len(txtMessgKE.Text.Trim) <> 2 Then
            txtMessgKE.Text = "  "
          End If
          LetPrivSettings("STARTUP", "MESSGKE", COLORFileName()) = txtMessgKE.Text
          If Err.Number <> 0 Then
            MsgBox(Err.Description)
          End If
        End If

        '
        '
        'automatischer Start
        '
        LetPrivSettings("STARTUP", "STAUTO", COLORFileName()) = CStr(chkStart.CheckState)
        If Err.Number <> 0 Then
          MsgBox(Err.Description)
        End If
        '


        '
        'Active-X Komponenten Ausgabe Farb-/Bindemittel
        '
        '
        '
        If txtActive(0).Text.Trim <> "" Then
          LetPrivSettings("COMCLASS", "FARBMIT", COLORFileName()) = txtActive(0).Text
          If Err.Number <> 0 Then
            MsgBox(Err.Description)
          End If
        End If
        '
        '
        'Active-X Komponenten Ausgabe Rezepteactive(0).text<> ""
        '
        '
        '
        If txtActive(1).Text.Trim <> "" Then
          LetPrivSettings("COMCLASS", "REZEPTE", COLORFileName()) = txtActive(1).Text
          If Err.Number <> 0 Then
            MsgBox(Err.Description)
          End If
        End If
        '
        '
        'Active-X Komponenten Ausgabe Sortimente
        '
        '
        '
        If txtActive(2).Text.Trim <> "" Then
          LetPrivSettings("COMCLASS", "SORTIME", COLORFileName()) = txtActive(2).Text
          If Err.Number <> 0 Then
            MsgBox(Err.Description)
          End If
        End If
        '
        '
        'Active-X Komponenten Ausgabe allgemein
        '
        '
        '
        If txtActive(3).Text.Trim <> "" Then
          LetPrivSettings("COMCLASS", "ALLGEME", COLORFileName()) = txtActive(3).Text
          If Err.Number <> 0 Then
            MsgBox(Err.Description)
          End If
        End If
        '
        '
        'Active-X Komponenten Rezeptberechnung (alle)
        '
        '
        '
        If txtActive(4).Text.Trim <> "" Then
          LetPrivSettings("COMCLASS", "RZPALLE", COLORFileName()) = txtActive(4).Text
          If Err.Number <> 0 Then
            MsgBox(Err.Description)
          End If
        End If
        '
        '
        'Active-X Komponenten Rezeptberechnung Wiegeschein
        '
        '
        '
        If txtActive(5).Text.Trim <> "" Then
          LetPrivSettings("COMCLASS", "WIEGESC", COLORFileName()) = txtActive(5).Text
          If Err.Number <> 0 Then
            MsgBox(Err.Description)
          End If
        End If
        '
        '
        'Active-X Komponenten Korrekturberechnung
        '
        '
        '
        If txtActive(6).Text.Trim <> "" Then
          LetPrivSettings("COMCLASS", "KORREKT", COLORFileName()) = txtActive(6).Text
          If Err.Number <> 0 Then
            MsgBox(Err.Description)
          End If
        End If
        '
        '
        'Active-X Komponenten Qualitätskontrolle
        '
        '
        '
        If txtActive(7).Text.Trim <> "" Then
          LetPrivSettings("COMCLASS", "QUALITA", COLORFileName()) = txtActive(7).Text
          If Err.Number <> 0 Then
            MsgBox(Err.Description)
          End If
        End If
        '
        '
        'Active-X Komponenten Ausgabe Reflexionswerte
        '
        '
        '
        If txtActive(8).Text.Trim <> "" Then
          LetPrivSettings("COMCLASS", "REWERTE", COLORFileName()) = txtActive(8).Text
          If Err.Number <> 0 Then
            MsgBox(Err.Description)
          End If
        End If
        '
        '
        'Messgerät
        '
        '
        '
        For i = 0 To cboMeasure.Items.Count - 1
          If MeasureDLL(i) <> vbNullString Then
            LetPrivSettings("MEASURE", "MEAS" & cboMeasure.Items(i), COLORFileName()) = MeasureDLL(i).Trim
          End If
        Next
      Catch ex As Exception

      End Try
    End If
    '
    '
    '
    '
    'ID-numbers
    '
    '
    '
    If Not IsNothing(cboUser.SelectedValue) And IsNumeric(cboUser.SelectedValue) Then
      LetPrivSettings("IDNUMBERS", "USERID", COLORFileName()) = cboUser.SelectedValue
    End If
    If Not IsNothing(cboMess.SelectedValue) And IsNumeric(cboMess.SelectedValue) Then
      LetPrivSettings("IDNUMBERS", "MESSGID", COLORFileName()) = cboMess.SelectedValue
    End If
    If Not IsNothing(cboMisch.SelectedValue) And IsNumeric(cboMisch.SelectedValue) Then
      LetPrivSettings("IDNUMBERS", "MISCHID", COLORFileName()) = cboMisch.SelectedValue
    End If
    '
    '
    '
    '
    '
    '

  End Sub

  Private Sub cboSPRACH_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboSPRACH.SelectedIndexChanged

    Dim Xlon As Long
    Langindex = cboSPRACH.SelectedIndex + 1
    If Trim(Texxt(2)) <> "???" Then
      Langindex = 2
      MsgBox(Texxt(3930))
    End If
    On Error Resume Next
    LetPrivSettings("LANGUAGE", "INDEX", COLORFileName()) = CStr(Langindex)
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    LetPrivSettings("LANGUAGE", "TABLE", COLORFileName()) = TabLangName(Langindex)
    If Err.Number <> 0 Then
      MsgBox(Err.Description)
    End If
    On Error GoTo 0
  End Sub

  Private Sub btnDAT_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  btnDAT_0.Click, btnDAT_1.Click, btnDAT_2.Click, btnDAT_3.Click, btnDAT_4.Click, btnDAT_5.Click, btnDAT_6.Click
    Dim index As Integer
    Static FilenameAct As String = ""
    index = CInt(sender.name.substring(7, 1))
    If index < 5 Then
      ccdFIL.Filter = "*.mdb|*.mdb"
    Else
      ccdFIL.Filter = "*.*|"
    End If
    If FilenameAct = "" Then
      ccdFIL.InitialDirectory = GetDirectory(DbIName(index))
    Else
      ccdFIL.InitialDirectory = GetDirectory(FilenameAct)
    End If
    ccdFIL.FileName = GetFilename(DbIName(index))
    If index >= 4 Then
      ccdFIL.CheckFileExists = False
    Else
      ccdFIL.CheckFileExists = True
    End If
    If ccdFIL.ShowDialog() = DialogResult.OK Then
      lblDAT(index).Text = ccdFIL.FileName
      FilenameAct = ccdFIL.FileName
      '
      If index = 2 And Cndat.DataSource = Cnkal.DataSource Then
        '
        'Kalibrierdatzen stehen in der Datenbank für Daten
        '
        '
        lblDAT(3).Text = lblDAT(2).Text
      End If
      If index = 1 Then
        TabControlINI.TabPages(3).Enabled = False
        TabControlINI.TabPages(3).Text = ""
      End If
    End If
  End Sub


  Private Sub radSST_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  radSST_00.CheckedChanged, radSST_01.CheckedChanged, radSST_02.CheckedChanged, radSST_03.CheckedChanged, radSST_04.CheckedChanged, radSST_05.CheckedChanged, _
  radSST_06.CheckedChanged, radSST_07.CheckedChanged, radSST_08.CheckedChanged, radSST_09.CheckedChanged, radSST_10.CheckedChanged, radSST_11.CheckedChanged, _
  radSST_12.CheckedChanged, radSST_13.CheckedChanged, radSST_14.CheckedChanged, radSST_15.CheckedChanged
    Dim Ihlf As Long
    Dim rgbi(2) As Integer
    Dim i As Integer
    indexSST = CInt(sender.name.substring(7, 2))
    Ihlf = farbini(indexSST).R
    rgbi(0) = Ihlf
    Ihlf = farbini(indexSST).G
    rgbi(1) = Ihlf
    Ihlf = farbini(indexSST).B
    rgbi(2) = Ihlf
    For i = 0 To 2
      If rgbi(i) > 255 Then
        rgbi(i) = 255
      End If
      hscFAR(i).Value = CInt(rgbi(i) - 0.5)
    Next i
  End Sub

  

  Private Sub btnEND_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnEND.Click
    Me.DialogResult = DialogResult.OK
  End Sub



  Private Sub TabControlINI_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TabControlINI.SelectedIndexChanged
    If Not TabControlINI.TabPages(TabControlINI.SelectedIndex).Enabled Then
      TabControlINI.SelectTab(0)
      TabControlINI.SelectedIndex = 0
    Else
      If TabControlINI.SelectedIndex = 3 Then
        TabMess.Rows.Clear()
        TabMisch.Rows.Clear()
        cboUser.Enabled = False
        CmdUser.CommandText = "SELECT * FROM TBL_USER ORDER BY USER_NAME"
        If Not FillDatset(AdUser, TabUser) Then
          Exit Sub
        End If
        cboUser.DataSource = TabUser
        cboUser.DisplayMember = "USER_NAME"
        cboUser.ValueMember = "USER_ID"
        cboUser.Enabled = True
        cboUser.SelectedIndex = -1
        cboUser.SelectedValue = CInt(GetPrivSettings("IDNUMBERS", "USERID", CStr(TabUser.Rows(0)("USER_ID")), COLORFileName()))
      End If
    End If
  End Sub


  Private Sub cboUser_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboUser.SelectedValueChanged
    If cboUser.SelectedIndex = -1 Then Exit Sub
    If Not cboUser.Enabled Or IsNothing(cboUser.SelectedValue) OrElse Not IsNumeric(cboUser.SelectedValue) Then Exit Sub
    '
    '
    'Messgeräte
    '
    '
    '
    TabMisch.Rows.Clear()
    cboMess.Enabled = False
    CmdMess.CommandText = "SELECT DISTINCT *,TBL_MESSG.MESSG_ID AS MESSG_ID FROM TBL_MESSG INNER JOIN TBL_USER_MESSG ON TBL_MESSG.MESSG_ID = TBL_USER_MESSG.MESSG_ID WHERE USER_ID=" & cboUser.SelectedValue & " ORDER BY MESSG_KBEZ"
    If Not FillDatset(AdMess, TabMess) Then
      Exit Sub
    End If
    If TabMess.Rows.Count = 0 Then Exit Sub
    cboMess.DataSource = TabMess
    cboMess.DisplayMember = "MESSG_KBEZ"
    cboMess.ValueMember = "MESSG_ID"
    cboMess.SelectedValue = -1
    cboMess.Enabled = True
    cboMess.SelectedValue = CInt(GetPrivSettings("IDNUMBERS", "MESSGID", CStr(TabMess.Rows(0)("MESSG_ID")), COLORFileName()))
    '
    '
    '
    '
    
  End Sub

  Private Sub cboMess_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboMess.SelectedValueChanged
    If Not cboMess.Enabled Or IsNothing(cboMess.SelectedValue) OrElse Not IsNumeric(cboMess.SelectedValue) Then Exit Sub
    '
    'Mischsysteme
    '
    '
    '
    cboMisch.Enabled = False
    CmdMisch.CommandText = "SELECT DISTINCT *,TBL_MISCH.MISCH_ID AS MISCH_ID FROM (TBL_MISCH INNER JOIN TBL_USER_MISCH ON TBL_MISCH.MISCH_ID=TBL_USER_MISCH.MISCH_ID) INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID=TBL_MISCH_MESSG.MISCH_ID " _
    & " WHERE ((TBL_USER_MISCH.USER_ID=" & cboUser.SelectedValue & ") AND (TBL_MISCH_MESSG.MESSG_ID=" & cboMess.SelectedValue & "))" & " ORDER BY MISCH_KBEZ"

    If Not FillDatset(AdMisch, TabMisch) Then
      Exit Sub
    End If
    If TabMisch.Rows.Count = 0 Then Exit Sub
    cboMisch.DataSource = TabMisch
    cboMisch.DisplayMember = "MISCH_KBEZ"
    cboMisch.ValueMember = "MISCH_ID"
    cboMisch.SelectedValue = -1
    cboMisch.Enabled = True
    cboMisch.SelectedValue = CInt(GetPrivSettings("IDNUMBERS", "MISCHID", CStr(TabMisch.Rows(0)("MISCH_ID")), COLORFileName()))
  End Sub

  Private Sub cboMisch_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboMisch.SelectedValueChanged

  End Sub
  Private Sub hscFar_ValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles hscFar_0.ValueChanged, hscFar_1.ValueChanged, hscFar_2.ValueChanged
    picFar.BackColor = Color.FromArgb(hscFAR(0).Value, hscFAR(1).Value, hscFAR(2).Value)
  End Sub

  Private Sub picFar_BackColorChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles picFar.BackColorChanged
    If indexSST > -1 Then
      farbini(indexSST) = picFar.BackColor
    End If
  End Sub

  Private Sub radMAT_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles radMAT_0.CheckedChanged, radMAT_1.CheckedChanged, radMAT_2.CheckedChanged
    Dim i As Integer
    Dim IndexHilf As Integer
    indexMAT = -1
    IndexHilf = CInt(sender.name.substring(7, 1))
    If sender.checked Then
      MsgBox(Texxt(438))
    End If
    For i = 0 To 2
      hscMAT(i).Value = 0
      If IsNumeric(RoGrBl(IndexHilf, i)) Then
        hscMAT(i).Value = RoGrBl(IndexHilf, i)
      End If
      If IsNumeric(XYZMat(IndexHilf, i)) Then
        txtXYZ(i).Text = CStr(XYZMat(IndexHilf, i))
      End If
      txtXYZ(i).Enabled = True
    Next i
    indexMAT = IndexHilf
  End Sub

  Private Sub hscMat_ValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles hscMat_0.ValueChanged, hscMat_1.ValueChanged, hscMat_2.ValueChanged
    picXYZ.BackColor = Color.FromArgb(hscMAT(0).Value, hscMAT(1).Value, hscMAT(2).Value)
  End Sub

  Private Sub picXYZ_BackColorChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles picXYZ.BackColorChanged
    Dim i As Integer
    If indexMAT > -1 Then
      For i = 0 To 2
        RoGrBl(indexMAT, i) = hscMAT(i).Value
      Next i
    End If
  End Sub

  Private Sub txtXYZ_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtXYZ_0.Leave, txtXYZ_1.Leave, txtXYZ_2.Leave
    Dim i As Integer
    If indexMAT = -1 Then
      MsgBox(Texxt(437))
      Exit Sub
    End If
    For i = 0 To 2
      If IsNumeric(txtXYZ(i).Text) Then
        XYZMat(indexMAT, i) = CSng(txtXYZ(i).Text)
      End If
    Next i
  End Sub


  Private Sub cboMeasure_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles cboMeasure.SelectedIndexChanged
    txtMeasure.Text = MeasureDLL(cboMeasure.SelectedIndex)
  End Sub

  Private Sub txtMeasure_Leave(sender As Object, e As System.EventArgs) Handles txtMeasure.Leave
    If txtMeasure.Text = "" Then
      MeasureDLL(cboMeasure.SelectedIndex) = Space(1)
    Else
      MeasureDLL(cboMeasure.SelectedIndex) = txtMeasure.Text
    End If
  End Sub

 
End Class