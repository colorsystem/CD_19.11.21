Option Strict Off
Option Explicit On
Option Compare Text
Friend Class MeasureRefl
  Implements IDisposable
  'Anzahl Reflexionskurven (+Zusatzinformation) QU-Kontrolle
  '
  Dim Nqu As Short
  Dim Npam As Short
  Dim Iprn As Short
  Dim Iheute As Short
  '

  '
  '
  ' Laufvariable
  Dim i As Short
  Dim l As Short
  Dim k As Short
  Dim j As Short
  Dim kw As Short
  ' Zwischengroessen
  Dim nkw As Short
  Dim Nku As Short
  Dim maxid As Integer 'Maximale ID (Primärschlüssel)
  '
  '
  Dim Aper As String
  Dim UVein As String
  Dim CrLf As String
  Dim iermes As Short
  '
  '
  Dim KdKa As Short
  '
  Dim MnIaktu As Short
  '
  '
  Dim AllgCommand As OleDbCommand
  Dim AllgReader As OleDbDataReader
  '
  '
  '0,1 kein Fehler
  '-1 Fehler
  '-5 nicht kalibriert
  '-3 ???
  '-6 IDKA(i)=0 ist nicht erlaubt
  '-7 keine Kalibrierdaten übernommen

  '
  '
  '
  'Properties für controls
  '
  '
  '
  '
  WriteOnly Property btnEND() As Button
    Set(ByVal value As Button)
      MNbtnEND = value
    End Set
  End Property
  WriteOnly Property btnLOE() As Button
    Set(ByVal value As Button)
      MNbtnLOE = value
    End Set
  End Property
  WriteOnly Property btnWIN() As Button
    Set(ByVal value As Button)
      MNbtnWIN = value
    End Set
  End Property
  WriteOnly Property btnMIT() As Button
    Set(ByVal value As Button)
      MNbtnMIT = value
    End Set
  End Property
  WriteOnly Property btnNXT() As Button
    Set(ByVal value As Button)
      MNbtnNXT = value
    End Set
  End Property
  WriteOnly Property btnABR() As Button
    Set(ByVal value As Button)
      MNbtnABR = value
    End Set
  End Property
  WriteOnly Property btnSTR() As Button
    Set(ByVal value As Button)
      MNbtnSTR = value
    End Set
  End Property
  WriteOnly Property btnSTP() As Button
    Set(ByVal value As Button)
      MNbtnSTP = value
    End Set
  End Property
  WriteOnly Property btnMES_0() As Button
    Set(ByVal value As Button)
      MNbtnMES_0 = value
    End Set
  End Property
  WriteOnly Property btnMES_1() As Button
    Set(ByVal value As Button)
      MNbtnMES_1 = value
    End Set
  End Property
  WriteOnly Property btnMES_2() As Button
    Set(ByVal value As Button)
      MNbtnMES_2 = value
    End Set
  End Property
  WriteOnly Property btnMES_3() As Button
    Set(ByVal value As Button)
      MNbtnMES_3 = value
    End Set
  End Property
  WriteOnly Property btnMES_4() As Button
    Set(ByVal value As Button)
      MNbtnMES_4 = value
    End Set
  End Property
  WriteOnly Property btnMES_5() As Button
    Set(ByVal value As Button)
      MNbtnMES_5 = value
    End Set
  End Property

  WriteOnly Property flgkal() As DataGridView
    Set(ByVal value As DataGridView)
      MNflgkal = value
    End Set
  End Property
  WriteOnly Property cboBAUD() As ComboBox
    Set(ByVal value As ComboBox)
      MNcboBAUD = value
      MNcboBAUD.Items.Clear()
      MNcboBAUD.Items.Add(CStr(110))
      MNcboBAUD.Items.Add(CStr(150))
      MNcboBAUD.Items.Add(CStr(300))
      MNcboBAUD.Items.Add(CStr(600))
      MNcboBAUD.Items.Add(CStr(1200))
      MNcboBAUD.Items.Add(CStr(2400))
      MNcboBAUD.Items.Add(CStr(4800))
      MNcboBAUD.Items.Add(CStr(9600))
      MNcboBAUD.Items.Add(CStr(19200))
      MNcboBAUD.Items.Add(CStr(38400))
    End Set
  End Property
  WriteOnly Property cboCOM() As ComboBox
    Set(ByVal value As ComboBox)
      Dim i As Integer
      MNcboCOM = value
      MNcboCOM.Items.Clear()
      For i = 0 To 16
        MNcboCOM.Items.Add(CStr(i))
      Next
    End Set
  End Property
  WriteOnly Property cboSPE_0() As ComboBox
    Set(ByVal value As ComboBox)
      MNcboSPE_0 = value
    End Set
  End Property
  WriteOnly Property cboSPE_1() As ComboBox
    Set(ByVal value As ComboBox)
      MNcboSPE_1 = value
    End Set
  End Property
  WriteOnly Property lblMES() As Label
    Set(ByVal value As Label)
      MNlblMES = value
    End Set
  End Property
  WriteOnly Property lblCOM() As Label
    Set(ByVal value As Label)
      MNlblCOM = value
    End Set
  End Property
  WriteOnly Property lblBAUD() As Label
    Set(ByVal value As Label)
      MNlblBAUD = value
    End Set
  End Property
  WriteOnly Property lblANZ() As Label
    Set(ByVal value As Label)
      MNlblANZ = value
    End Set
  End Property
  WriteOnly Property lblWID() As Label
    Set(ByVal value As Label)
      MNlblWID = value
    End Set
  End Property
  WriteOnly Property lblSPE_0() As Label
    Set(ByVal value As Label)
      MNlblSPE_0 = value
    End Set
  End Property
  WriteOnly Property lblSPE_1() As Label
    Set(ByVal value As Label)
      MNlblSPE_1 = value
    End Set
  End Property
  WriteOnly Property txtKEN() As TextBox
    Set(ByVal value As TextBox)
      MNtxtKEN = value
    End Set
  End Property
  WriteOnly Property txtProg() As TextBox
    Set(ByVal value As TextBox)
      MNtxtProg = value
    End Set
  End Property
  WriteOnly Property txtWID() As TextBox
    Set(ByVal value As TextBox)
      MNtxtWID = value
    End Set
  End Property
  WriteOnly Property Parentform() As Form
    Set(ByVal value As Form)
      MNParentform = value
    End Set
  End Property


  Sub MessStart()
    Dim i As Integer

    MNlblMES.Visible = False
    'txtKAL.Visible = False
    lblspe(0).Visible = False
    cboSPE(0).Visible = False
    lblspe(1).Visible = False
    cboSPE(1).Visible = False
    MNbtnWIN.Visible = False
    MNbtnLOE.Visible = False
    MNbtnSTR.Visible = False
    MNbtnSTP.Visible = False
    MNbtnABR.Visible = False
    MNbtnMIT.Visible = False
    MNbtnNXT.Visible = False
    MNcboCOM.Visible = False
    MNcboBAUD.Visible = False
    MNlblCOM.Visible = False
    MNlblBAUD.Visible = False
    MNlblWID.Visible = False
    MNtxtKEN.Visible = False
    MNtxtProg.Visible = False
    MNtxtWID.Visible = False
    '
    MNflgkal.Visible = False
    MNbtnEND.Visible = False




    For i = 0 To btnMES.Count - 1
      btnMES(i).Visible = False
    Next i



  End Sub



  WriteOnly Property MessgMenue() As Short
    Set(ByVal Value As Short)
      Dim i As Integer
      MnMessgMenue = Value
      Try
        For i = 0 To UBound(idka)
          idka(i) = 0
        Next i
        If IsNothing(clasm) Then Exit Property
        clasm.SetIdka(MnMessgMenue)
        If clasm.IchStat = -1 Then
          MNParentform.DialogResult = DialogResult.Abort
          Exit Property
        End If
        If MnMessgMenue = -1 Then
          Exit Property
        End If


        '
        '

        MNtxtKEN.Text = MessgKenn
        MNtxtProg.Text = MessgProg
        For i = 0 To 5
          If idka(i) = 0 Then
            Exit For
          End If
          MnMessg.IDka(i) = idka(i)
        Next i
        '
        Mnier = 0
        Call KalIntDat(Iheute)
      Catch
      End Try

      Exit Property
    End Set
  End Property

  '
  '


  ReadOnly Property ier() As Integer
    Get
      ier = Mnier
    End Get
  End Property
  Sub MeasDeviceKalib()
    Dim i As Integer


    Mnier = 0
    '
    '
    '
    'Kalibrieren  
    '
    IdkaID = 0
    For i = 0 To 5
      If idka(i) <> 0 Then
        IdkaID = IdkaID + 1
        BezNr(i) = 470 + idka(i)
      Else
        Exit For
      End If
    Next i
    Call KalWrt(2, Reftra, Iheute, Mnier)
    If Mnier <> 0 Then
      MNParentform.DialogResult = DialogResult.Abort
      Exit Sub
    End If
    If Iheute = 3 Then
      If MessageOut(MessgKalMess, 3512, Texxt(3512), MessageBoxButtons.OK, Texxt(2000), NamLogFile) = DialogResult.OK Then
        MNParentform.DialogResult = DialogResult.Abort
      End If
      Exit Sub
    End If
    MessgKalw = 1
    If Iheute = 0 Then
      System.Windows.Forms.Application.DoEvents()
      imsg = MessageOut(MessgKalMess, 3514, Texxt(3514) & Space(1) & CStr(MessgKalInt) & Texxt(3515), MessageBoxButtons.YesNo, Texxt(2000), NamLogFile)
      If Not imsg = DialogResult.Yes Then
        MNParentform.DialogResult = DialogResult.Yes
        Exit Sub
      End If
    End If
    MNtxtKEN.Visible = True
    MNtxtProg.Visible = True
    MNtxtKEN.Enabled = False
    MNtxtProg.Enabled = False

    MNbtnEND.Visible = True
    MNbtnEND.Enabled = False
    MNtxtKEN.Text = MessgKenn
    MNtxtProg.Text = MessgProg
    Call GrdMes(IdkaID, BezNr, MNflgkal)
    '
    For i = 0 To btnMES.Count - 1
      If idka(i) <> 0 Then
        If Trim(btnMES(idka(i)).Text) <> "" Then
          btnMES(idka(i)).Visible = False
        End If
        btnMES(idka(i)).Enabled = False
      Else
        Exit For
      End If
    Next i
    '
    If idka(0) = 0 Then
      MsgBox(Texxt(3521), 0)
      Mnier = -3
      MNParentform.DialogResult = DialogResult.Abort
    End If
    IdkaID = 0
    btnMES(idka(IdkaID)).Enabled = True
    btnMES(idka(IdkaID)).Visible = True
    If clasm.Autom = 1 Then
      Do While idka(IdkaID) > 0
        Call Messen(idka(IdkaID))
        If clasm.IchStat = -1 Then
          Mnier = clasm.IchStat
          MNParentform.DialogResult = DialogResult.Abort
          Exit Sub
        End If
      Loop
      Call Wait(MessgTwait)
      Exit Sub
    End If
    '
    ''



  End Sub

  Sub MeasDeviceManu(ByRef Refel As RefValue)
    Mnier = 0

    '
    '
    'Manuell
    '
    If MessgKM = 0 Then
      MNParentform.DialogResult = DialogResult.Abort
      Exit Sub
    End If
    '
    MNParentform.Text = Texxt(398) & "   (" & MessgKbez & ")"

    '
    MNbtnEND.Visible = True
    MNbtnEND.Enabled = True
    MNbtnABR.Visible = True
    MNbtnABR.Enabled = True
    MnReFel = Refel
    Call GrdMan(MNflgkal)



  End Sub
  Sub MeasDeviceMess(ByRef Refel As RefValue)
    Mnier = 0
    '
    '
    '
    '  Messen
    '
    '
    Try
      If Refel.ReTr = 1 And Reftra = "R" Then
        MsgBox(Texxt(3590), 0)
        Mnier = -7
        MNParentform.DialogResult = DialogResult.Cancel
        Refel = Nothing
        Exit Sub
      End If
    Catch
    End Try
    MNlblMES.Visible = True
    '
    '


    If MessgKalw = 0 Then
      MsgBox(Texxt(3520), 0)
      Mnier = -5
      MNParentform.DialogResult = DialogResult.No
      Refel = Nothing
      Exit Sub
    End If
    MnReFel = Refel
    Call Messen(0)
    If Mnier = -1 Then
      MNParentform.DialogResult = DialogResult.Abort
    End If







  End Sub


  Sub MeasDeviceNkalib()
    Dim i As Integer
    Mnier = 0
    '
    '
    '
    ', Nullkalibrieren  
    '
    Call KalWrt(2, Reftra, Iheute, Mnier)
    If Mnier <> 0 Then
      Exit Sub
    End If
    MNbtnEND.Visible = True
    MNbtnEND.Enabled = False
    MNtxtKEN.Text = MessgKenn
    MNtxtProg.Text = MessgProg
    IdkaID = 0
    For i = 0 To 4
      If idka(i) <> 0 Then
        IdkaID = IdkaID + 1
        BezNr(i) = 470 + idka(i)
      End If
    Next i

    Call GrdMes(IdkaID, BezNr, MNflgkal)
    '
    For i = 0 To btnMES.Count - 1
      If idka(i) <> 0 Then
        btnMES(i).Visible = False
        btnMES(i).Enabled = False
      End If
    Next i
    '
    If idka(0) = 0 Then
      MsgBox(Texxt(3521), 0)
      Call EndMess()
    End If
    IdkaID = 0
    btnMES(idka(IdkaID)).Enabled = True
    btnMES(idka(IdkaID)).Visible = True
    If clasm.Autom = 1 Then
      Do While idka(IdkaID) > 0
        btnMES(idka(IdkaID)).PerformClick()
        If clasm.IchStat = -1 Then
          Mnier = clasm.IchStat
          Call EndMess()
          Exit Sub
        End If
      Loop
      Call Wait(MessgTwait)
      Call EndMess()
    End If




  End Sub

  Sub MeasDeviceInit()
    Dim i As Integer
    If IsNothing(clasm) Then
      Mnier = -1
      Exit Sub
    End If
    '
    'Initialisieren
    '
    MNbtnEND.Visible = True
    MNbtnEND.Enabled = False
    MNcboCOM.Visible = True
    MNcboBAUD.Visible = True
    MNlblCOM.Visible = True
    MNlblBAUD.Visible = True
    MNtxtKEN.Visible = True
    MNtxtKEN.Enabled = False
    MNtxtKEN.Text = MessgKenn
    MNtxtProg.Visible = True
    MNtxtProg.Enabled = True
    MNtxtProg.Text = MessgProg
    MNcboCOM.Enabled = False
    MNcboBAUD.Enabled = False
    MNcboCOM.SelectedIndex = -1
    '
    MNcboCOM.SelectedIndex = MessgCom
    MNcboBAUD.SelectedValue = MessgBaud
    For i = 0 To MNcboBAUD.Items.Count - 1
      If MNcboBAUD.Items(i) = MessgBaud Then
        MNcboBAUD.SelectedIndex = i
        Exit For
      End If
    Next i
    '
    MNParentform.Text = Texxt(205 + MnMessgMenue) & "   (" & MessgKbez & ")"
    '
    '
    '
    IdkaID = 1
    For i = 0 To 4
      If idka(i) <> 0 Then
        IdkaID = IdkaID + 1
        BezNr(i + 1) = 476 + idka(i)
      Else
        Exit For
      End If
    Next i
    BezNr(0) = 469
    Call GrdMes(IdkaID, BezNr, MNflgkal)

    '
    Call KalWrt(1, Reftra, Iheute, Mnier)
    If Mnier <> 0 Then
      MNParentform.DialogResult = DialogResult.Abort
    End If
    '
    '
    '
    '
    '
    'geräteabhängige Initialisierung
    'z.B. Istwellenlängen überschreiben oder
    'Absolutwerte für Weiß- und Schwarzstandard einlesen
    '
    '
    MNcboCOM.Enabled = True
    MNcboBAUD.Enabled = True
    '
    If WithClasm Then
      clasm.MesIni()
      If clasm.IchStat <> -999 Then
        Mnier = clasm.IchStat
        Exit Sub
      End If '
    End If
    '
    Call GrdIni(MNflgkal)
    '
    'geräteabhängige Initialisierung
    '
    MNcboCOM.Enabled = True
    MNcboBAUD.Enabled = True
    '

  End Sub


  Sub MeasDeviceSond()
    Mnier = 0
    clasm.MesSon()
    Mnier = clasm.IchStat
    MNParentform.DialogResult = DialogResult.OK
  End Sub
  Friend Sub UmspeiUseMethMessg(ByRef messg As MeasParameters)
    Dim k As Integer
    Dim KW As Integer
    '
    'UserControl-Methoden abhängige Parameter
    '
    '
    MessgKM = messg.Winkel.Km
    ReDim gk(MessgKM - 1, NGK - 1)
    MessgAka = 1.0 - messg.Ka
    MessgGID = messg.RwrtGID
    For kw = 0 To MessgKM - 1
      For k = 0 To NGK - 1
        gk(KW, k) = messg.Winkel(KW).GK(k)
      Next k
    Next kw

  End Sub
  Private Sub UmSpeiMessg(ByRef Messg As MeasParameters)
    Dim k As Short
    Dim i As Short
    Dim nkw As Short
    Dim kw As Short
    Call FindMessgParameter(Messg, ier)

    AktDriver = ""
    MessgDriver = Messg.Driver
    NormfileID = Messg.BereichID
    MessgImes = Messg.Imes
    MessgKalMess = Messg.MessgKalMess
    MessgIkal = Messg.Ikal
    MessgKbez = Messg.Kbez
    MessgLbez = Messg.Lbez
    MessgDummy = Messg.Tbl
    MessgCom = Messg.Com
    MessgKenn = Messg.Kenn
    MessgProg = Messg.Prog
    MessgBaud = Messg.Baud
    MessgLength = Messg.Length
    MessgHands = Messg.Hands
    MessgTwait = Messg.Twait
    MessgDE = Messg.De
    MessgTdiff = Messg.Tdiff
    MessgProg = Messg.Prog
    MessgPar = Messg.Par
    MessgStop = Messg.Stopbit
    MessgKalInt = Messg.KalInt
    MessgNwe = Messg.Winkel.Wsol.Nwe
    MessgNwp = Messg.Winkel.Nwp
    MessgKM = Messg.Winkel.Km
    Reftra = Messg.RefTra

    ReDim MessgChrm(MessgKM - 1)
    ReDim DEwmes(MessgKM - 1)
    For kw = 0 To MessgKM - 1
      MessgChrm(kw) = Messg.Winkel(kw).Chrm
    Next kw
    Mdim = MessgNwe * MessgKM - 1
    ReDim MessgWsol(Mdim)
    For kw = 0 To MessgKM - 1
      nkw = kw * MessgNwe
      For i = 0 To MessgNwe - 1
        MessgWsol(nkw + i) = Messg.Winkel.Wsol.R(i)
      Next i
    Next kw
    ReDim rsmes(Mdim)
    ReDim Rumes(Mdim)
    ReDim Romes(Mdim)
    ReDim rsmes(Mdim)
    ReDim rkmes(Mdim)
    ReDim rmes(Mdim)
    ReDim MessgWist(Mdim)
    ReDim MessgQW(Mdim)
    ReDim MessgQS(Mdim)
    ReDim MessgQG(Mdim)
    ReDim MessgQT(Mdim)
    ReDim MessgQB(Mdim)
    ReDim MessgMW(Mdim)
    ReDim MessgMS(Mdim)
    ReDim MessgMG(Mdim)
    ReDim MessgMT(Mdim)
    ReDim MessgMB(Mdim)
    ReDim Rhilf(Mdim)
    ReDim rrmes(Mdim)
    'rrmes = New List(Of Single())
    'For kw = 0 To MessgKM - 1
    ' rrmes.Add(New Single(NWE))
    ' Next

    '
    '

    Call UmspeiUseMethMessg(Messg)
  End Sub
  Private Sub UmSpeiNormfa(ByRef Normfa As NormIlluminat)
    Dim i As Short
    Dim k As Short
    Dim nwe As Short
    nwe = Normfa.Normkurven(0).Nwe
    ReDim XYZE(2, nwe - 1)

    NormID = Normfa.LichtID
    For k = 0 To 2
      Fakt(k) = Normfa.NormFakt(k)
      For i = 0 To MessgNwe - 1
        XYZE(k, i) = Normfa.Normkurven(k).R(i)
      Next i
    Next k
  End Sub
  Sub FindMessgParameter(ByRef Messg As MeasParameters, ier As Integer)
    Dim Sqlstmt As String
    Dim AllgCommand As OleDbCommand
    Dim AllgReader As OleDbDataReader
    AllgCommand = New OleDbCommand("", Cnkal)
    If Cnkal.DataSource <> Cndat.DataSource Then

      '
      'Speichern von Messgerätewerten in Kalibrierdatei
      '(z.B. sinnvoll zur Unterscheidung, falls mehrere Messgeräte unter der gleichen Nummer an verschiedenen PCs verwendet werden)
      '
      '
      'Prüfen, ob Tabelle vorhanden
      '
      If Not TableExists("TBL_DEVICE", Cnkal) Then
        '
        'Tabelle erzeugen
        '
        Sqlstmt = "CREATE TABLE TBL_DEVICE (MESSG_ID LONG,MESSG_KENN TEXT(2),MESSG_COM SHORT,MESSG_BAUD LONG,MESSG_PROG TEXT(32))"
        AllgCommand.CommandText = Sqlstmt
        If SQLExeNonQuery(AllgCommand, Cnkal) <> 0 Then
          Exit Sub
        End If
        Sqlstmt = "ALTER TABLE TBL_DEVICE ADD CONSTRAINT DEVICE_MESSG PRIMARY KEY (MESSG_ID,MESSG_KENN)"
        AllgCommand.CommandText = Sqlstmt
        If SQLExeNonQuery(AllgCommand, Cnkal) <> 0 Then
          Exit Sub
        End If        '
        '
      End If
    End If
    '
    '
    'Feld MESSG_KENN in TBL_KALIB einbauen
    '
    '
    '
    If Not FieldExists("TBL_KALIB", "MESSG_KENN", Cnkal) Then
      Call CreateField("TBL_KALIB", "MESSG_KENN", "TEXT(2)", "'  '", ier, Cnkal)
      If ier > 0 Then Exit Sub
      Call DeletePrKey("KALIB_MESSG", "TBL_KALIB", ier, Cnkal)
      If ier > 0 Then Exit Sub
      Call CreatePrKey("KALIB_MESSG", "TBL_KALIB", {"MESSG_ID", "ART_ID", "MESSG_KENN"}, ier, Cnkal)
      If ier > 0 Then Exit Sub
      '
    End If
    '
    '
    'Feld MESSG_KENN füllen
    '
    '
    '
    AllgCommand.CommandText = "UPDATE TBL_KALIB SET MESSG_KENN='" & Messg.Kenn & "' WHERE MESSG_ID=" & Messg.MessgID & " AND MESSG_KENN='  '"
    '
    If SQLExeNonQuery(AllgCommand, Cnkal) <> 0 Then
      Exit Sub
    End If
    '
    '
    'Prüfen, ob Werte in TBL_DEVICE verfügbar
    '
    '
    If TableExists(TblMeasDevice, Cnkal) Then
      AllgCommand.CommandText = "SELECT * FROM " & TblMeasDevice & " WHERE MESSG_ID=" & Messg.MessgID & " AND MESSG_KENN='" & Messg.Kenn & "'"
      AllgReader = DataReader(AllgCommand, CommandBehavior.SingleRow And CommandBehavior.CloseConnection, Cnkal)
      If AllgReader.Read Then
        Messg.Com = AllgReader("MESSG_COM")
        Messg.Baud = AllgReader("MESSG_BAUD")
        Messg.Prog = AllgReader("MESSG_PROG")
      End If
      AllgReader.Close()
    End If
    AllgCommand.Dispose()
  End Sub
 






  Sub New(ByVal messg As MeasParameters, ByVal Normfa As NormIlluminat)
    MyBase.New()
    Mnier = 0
    '
    '
    'Parameter für Messgerät übernehmen
    '
    '
    '
    Mnier = 0
    MnMessg = messg
    MessgID = MnMessg.MessgID
    GKwrtID = MnMessg.GKwrtID
    MessgUserGID = MnMessg.UserRefGID

    Call UmSpeiMessg(MnMessg)
    AktDriver = MessgDriver
    '
    '
    '
    'Daten für Normlichtart übernehmen
    '
    '
    MnNormfa = Normfa
    Call UmSpeiNormfa(MnNormfa)



  End Sub
  Sub MesClasm()
    Dim i As Integer

    Mnier = 0
    Iheute = 0
    '
    'Datenbank für Kalibrierdaten öffnen
    '
    '
    NamLogFile = LogFileName()
    '
    '
    'LogFile löschen
    '
    '
    Try
      '
      If File.Exists(NamLogFile) Then
        File.Delete(NamLogFile)
      End If
      '
      '
    Catch
    End Try
    btnMES = New List(Of Button)
    lblspe = New List(Of Label)
    cboSPE = New List(Of ComboBox)
    '
    '
    '
    '
    '
    'Messgerät auswählen
    '
    '

    '
    '
    clasm = Nothing
    Select Case MessgDriver
      Case "000" To "099"
        clasm = New clsDLL
        If clasm.ichstat = -1 Then
          clasm = Nothing
        End If
      Case "XKU"
        clasm = New clsXKU
      Case "XKV"
        clasm = New clsXKU
      Case "XKW"
        clasm = New clsXKU
      Case "XCF"
        clasm = New clsXKU
      Case "XCA"
        clasm = New clsXKU
      Case "XMU"
        clasm = New clsXKU
      Case "XMV"
        clasm = New clsXKU
      Case "X45"
        clasm = New clsXKU
      Case "X53"
        clasm = New clsXKU
      Case "X62"
        clasm = New clsXKU
      Case "SPE"
        clasm = New clsSPE
      Case "SPF"
        clasm = New clsSPE
      Case "SPA"
        clasm = New clsSPA
      Case "U30"
        clasm = New clsU30
      Case "OPR"
        clasm = New clsALG
      Case "OPT"
        clasm = New clsALG
      Case "OPI"
        clasm = New clsALG
      Case "OPO"
        clasm = New clsALG
      Case "OPQ"
        clasm = New clsALG
      Case "D38"
        clasm = New clsALG
      Case "BYK"
        clasm = New clsBYK
      Case "BYC"
        clasm = New clsBYK
      Case "BYG"
        clasm = New clsBYK
      Case "MIL"
        clasm = New clsMIL
      Case "MI0"
        clasm = New clsMIN20
      Case "MI1"
        clasm = New clsMIN58
      Case "MI2"
        clasm = New clsMIN35
      Case "MI3"
        clasm = New clsMIN36
      Case "MI4"
        clasm = New clsMIN37
      Case "MI5"
        clasm = New clsMIN512M3
      Case "MI6"
        clasm = New clsMIN26
      Case "MI7"
        clasm = New clsmin25
      Case "MI8"
        clasm = New clsmin23
      Case "MI9"
        clasm = New clsmin700
      Case "MC0"
        clasm = New clsMAC
      Case "MC1"
        clasm = New clsMAC
      Case "MC2"
        clasm = New clsMAC
      Case "MC3"
        clasm = New clsMAC
      Case "MC4"
        clasm = New clsMAC
      Case "MC5"
        clasm = New clsMAC
      Case "MC6"
        clasm = New clsMAC
      Case "MC7"
        clasm = New clsMAC
      Case "HU1"
        'clasm = New clsHUN
      Case "HU2"
        'clasm = New clsHUN
        'UPGRADE_WARNING: Couldn't resolve default property of object clasm.LabScanXE. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        'clasm.LabScanXE = LabScanXE
        'UPGRADE_WARNING: Couldn't resolve default property of object clasm.cmdMesWe. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        'clasm.cmdMesWe = btnMES(0)
        'UPGRADE_WARNING: Couldn't resolve default property of object clasm.cmdMesSc. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        'clasm.cmdMesSc = btnMES(1)
        'UPGRADE_WARNING: Couldn't resolve default property of object clasm.cmdMesWe. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        'clasm.cmdMesWe.Caption = Texxt(467)
        'UPGRADE_WARNING: Couldn't resolve default property of object clasm.cmdMesSc. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        'clasm.cmdMesSc.Caption = ""
      Case "HU3"
        'clasm = New clsHUN
        'UPGRADE_WARNING: Couldn't resolve default property of object clasm.UltScanXE. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        'clasm.UltScanXE = UltScanXE
        'UPGRADE_WARNING: Couldn't resolve default property of object clasm.cmdMesWe. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        'clasm.cmdMesWe = btnMES(0)
        'UPGRADE_WARNING: Couldn't resolve default property of object clasm.cmdMesSc. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        'clasm.cmdMesSc = btnMES(1)
        'UPGRADE_WARNING: Couldn't resolve default property of object clasm.cmdMesWe. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        'clasm.cmdMesWe.Caption = Texxt(467)
        'UPGRADE_WARNING: Couldn't resolve default property of object clasm.cmdMesSc. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
        'clasm.cmdMesSc.Caption = ""

      Case "GK1"
        clasm = New clsGK1
      Case "GR0"
        clasm = New clsGRE
      Case "GR1"
        clasm = New clsGRE
      Case "PE1"
        clasm = New clsPerElm
      Case "LIC"
        clasm = New clsLico
      Case "MX0"
        clasm = New clsEyeOne
      Case "TEC"
        clasm = New clsTechkon
      Case Else
        MsgBox(Texxt(3510))
        Mnier = 3510
        Exit Sub
    End Select
    If clasm Is Nothing OrElse clasm.IchStat <> 0 Then
      Mnier = -999
      Exit Sub
    End If
    '

    '
    '
    clasm.IchStat = 0
    '
    '
    'Einlesen Kalibrierdaten
    '
    '
    '
    For i = 0 To UBound(idka)
      idka(i) = 0
    Next
    '
    '
    clasm.setidka(1)
    Call KalIntDat(Iheute)

  End Sub
  Protected Overrides Sub Finalize()
    If Not IsNothing(TblKal) Then
      'CmdKal.Dispose()
      MscMes.Dispose()
      TblKal.Dispose()
      'clasm.dispose()
    End If
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose

  End Sub

  Private Function NWE() As Object
    Throw New NotImplementedException
  End Function

End Class