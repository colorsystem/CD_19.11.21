Option Strict Off
Option Explicit On
Option Compare Text
Module MeasDevModul
  Friend Const Ipmax As Short = 25 'max. Anzahl Messungen für Mittelung
  Friend Const Jpmax As Short = 5 'Anzahl R-Werte in RH (s. frmMES)                    'zugleich max Anzahl von Auswahlen
  Friend Const NuWrt As Short = -999
  '
  Friend SQLEtmt As String
  Dim iermes As Integer
  Dim iheute As Integer
  '
  '
  '
  '
  Friend MnMessgMenue As Short 'Art der Messgeräteansteuerung (menue=0 Messen;

  '
  '
  Friend clasm As Object
  'menue=1 Kalibrieren;menue; menue =2 Null-Kal.;
  Friend MessgImess As Short 'Meßgeräteansteuerung (0 = Messen; 1=Weißkal.;2=Schwarzkal;3=Graukal. usw; gemäß IKAx
  '


  '
  ' PaGloAuf As New AufbauParameter
  '
  Friend Mdim As Short
  Friend TableKalib As String
  Friend TblMeasDevice As String
  Friend NamLogFile As String
  Friend SQLStmt As String
  Friend CmdKal As OleDbCommand
  Friend TblRs As OleDbDataReader
  Friend TblKal As DataTable
  Friend WithClasm As Boolean
  '
  Friend AktDriver As String

  '
  Friend Mnier As Integer
  Friend ierL As Integer
  Friend ier As Integer

  Friend MessgID As Integer
  Friend MessgUserGID As Integer
  Friend GKwrtID As Integer
  Friend NormID As Integer

  '
  '
  Friend MnMessg As MeasParameters
  Friend MnNormfa As NormIlluminat
  Friend MessgKM As Integer
  Friend MessgNwe As Integer
  Friend MessgNwp As Integer
  Friend MessgAka As Single
  Friend RetrL As Integer
  Friend imsg As DialogResult
  Friend CrLf As String
  Friend Alph As Single
  Friend demes(4) As Single
  Friend dlmes(4) As Single
  Friend dcmes(4) As Single
  Friend dhmes(4) As Single
  Friend dames(4) As Single
  Friend dbmes(4) As Single
  Friend rrmes() As Single
  Friend Rumes() As Single
  Friend Romes() As Single
  Friend rsmes() As Single
  Friend rkmes() As Single
  Friend rmes() As Single

  Friend Ipm As Short
  Friend MesZae As Short
  Friend Mesmax As Short
  Friend DEwmes() As Single
  Friend iami As Short
  Friend MnName As String
  Friend MnBEM As String
  Friend MnBanum As String
  '

  Friend BUF As String
  Friend Izaehl As Short
  Friend stv As String
  Friend Resum As Boolean
  Friend Hlfmes As Object
  Friend BezNr(4) As Short
  Friend hght As Short
  Friend Dif As Single
  Friend WinChk As Boolean
  Friend NXTChk As Boolean
  Friend COMchk As Boolean
  Friend MITchk As Boolean
  Friend ABRchk As Boolean
  Friend irmax As Short
  Friend Swidth As Single
  Friend Sheight As Single

  'UPGRADE_NOTE: CommonDialog variable has been upgraded to a different type
  Friend ccdFILOpen As OpenFileDialog
  Friend ccdFILSave As SaveFileDialog
  Friend ccdFILFont As FontDialog
  Friend ccdFILColor As ColorDialog
  Friend ccdFILPrint As PrintDialog
  Friend MessgChrm() As String
  Friend MessgQW() As Single 'Absolutwerte Weißstandard
  Friend MessgQS() As Single 'Absolutwerte Schwarzstandard
  Friend MessgQG() As Single 'Absolutwerte Graustandard
  Friend MessgQT() As Single 'Absolutwerte Transmission (Weiss)
  Friend MessgQB() As Single 'Absolutwerte Transmission (Schwarz)
  Friend MessgMW() As Single 'Kalibrierwerte Weißstandard
  Friend MessgMS() As Single 'Kalibrierwerte Schwarzstandard
  Friend MessgMG() As Single 'Kalibrierwerte Graustandard
  Friend MessgMT() As Single 'Kalibrierwerte Transmission (Weiss)
  Friend MessgMB() As Single 'Kalibrierwerte Transmission (Schwarz)
  Friend MessgWist() As Single 'Ist-Wellenlaengen
  Friend Rhilf() As Single
  Friend MessgWsol() As Single

  Dim SqlStmm As String
  '
  '
  Friend IdkaID As Short
  Friend idka(5) As Short 'Reihenfolge der Kalibrierung (1 weiss,2 schwarz, 3 grau,4 weiss Trans.,5 schwarz trans.))
  Friend MessgDriver As String
  Friend NormfileID As Short
  Friend MessgImes As Short
  Friend MessgKalMess As Boolean
  Friend MessgIkal As Short
  Friend MessgKbez As String
  Friend MessgLbez As String
  Friend MessgDummy As String
  Friend MessgKenn As String
  Friend MessgProg As String
  Friend MessgPar As String
  Friend MessgBaud As Integer
  Friend MessgStop As Short
  Friend MessgCom As Short
  Friend MessgLength As Short
  Friend MessgHands As Short
  Friend MessgTwait As Short
  Friend MessgTdiff As Short
  Friend MessgKalInt As Short
  Friend MessgKalw As Short 'Kalibrierwerte eingelesen (0) nicht eingelesen (0)
  Friend MessgDE As Single
  Friend Reftra As String
  Friend MessgGID As Integer
  Friend MnReFel As RefValue 'Rwerte

  '
  '
  '
  Friend NGK As Integer = 16
  Friend gk(,) As Single
  Friend Fakt(2) As Single
  Friend XYZE(,) As Single
  '
  Friend DbErr As Short
  Friend WithEvents MscMes As SerialPort

  '

  Friend WithEvents MNbtnEND As Button
  Friend WithEvents MNbtnLOE As Button
  Friend WithEvents MNbtnWIN As Button
  Friend WithEvents MNbtnMIT As Button
  Friend WithEvents MNbtnNXT As Button
  Friend WithEvents MNbtnABR As Button
  Friend WithEvents MNbtnSTR As Button
  Friend WithEvents MNbtnSTP As Button
  Friend WithEvents MNbtnMES_0 As Button
  Friend WithEvents MNbtnMES_1 As Button
  Friend WithEvents MNbtnMES_2 As Button
  Friend WithEvents MNbtnMES_3 As Button
  Friend WithEvents MNbtnMES_4 As Button
  Friend WithEvents MNbtnMES_5 As Button
  Friend WithEvents MNflgkal As DataGridView
  Friend WithEvents MNcboBAUD As ComboBox
  Friend WithEvents MNcboCOM As ComboBox
  Friend WithEvents MNcboSPE_0 As ComboBox
  Friend WithEvents MNcboSPE_1 As ComboBox
  Friend WithEvents MNlblMES As Label
  Friend WithEvents MNlblCOM As Label
  Friend WithEvents MNlblBAUD As Label
  Friend WithEvents MNlblANZ As Label
  Friend WithEvents MNlblWID As Label
  Friend WithEvents MNlblSPE_0 As Label
  Friend WithEvents MNlblSPE_1 As Label
  Friend WithEvents MNtxtKEN As TextBox
  Friend WithEvents MNtxtProg As TextBox
  Friend WithEvents MNtxtWID As TextBox
  Friend WithEvents MNParentform As Form
  Friend btnMES As List(Of Button)
  Friend lblspe As List(Of Label)
  Friend cboSPE As List(Of ComboBox)
  '
  '
  'Satznummern Kalibrierdaten
  '
  Private Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Integer)
  '
  '
  '
  Friend Declare Sub REFWRT Lib "MESSREF.DLL" (ByRef nwe As Integer, ByRef QW As Single, ByRef QS As Single, ByRef QG As Single, ByRef MW As Single, ByRef MS As Single, ByRef MG As Single, ByRef WIST As Single, ByRef WSOL As Single, ByRef MK As Single, ByRef RR As Single, ByRef ier As Integer)
  Friend Declare Sub MESSWRT Lib "MESSREF.DLL" (ByRef nwe As Integer, ByRef QW As Single, ByRef QS As Single, ByRef QG As Single, ByRef MW As Single, ByRef MS As Single, ByRef MG As Single, ByRef WIST As Single, ByRef WSOL As Single, ByRef RR As Single, ByRef MK As Single, ByRef ier As Integer)
  Private Declare Sub RMEMI Lib "MESSREF.DLL" (ByRef nwe As Integer, ByRef km As Integer, ByRef de As Single, ByRef dl As Single, ByRef dc As Single, ByRef dh As Single, ByRef da As Single, ByRef db As Single, ByRef Alph As Single, ByRef DEchk As Single, ByRef rr As Single, ByRef iami As Integer, ByRef dew As Single, ByRef ier As Integer)
  Private Declare Sub RMERD Lib "MESSREF.DLL" (ByRef nwe As Integer, ByRef km As Integer, ByRef rr As Single, ByRef ier As Integer)
  Private Declare Sub RMEPR Lib "MESSREF.DLL" (ByRef IRT As Integer, ByRef nwe As Integer, ByRef km As Integer, ByRef Ro As Single, ByRef rr As Single, ByRef Ru As Single, ByRef Kenn As Integer, ByRef ier As Integer)
  Friend Declare Sub RMNUL Lib "MESSREF.DLL" (ByRef Aka As Single, ByRef nwe As Integer, ByRef km As Integer, ByRef rr As Single, ByRef NG As Integer, ByRef GK As Single, ByRef MessgID As Integer, ByRef NormID As Integer, ByRef Fakt As Single, ByRef XYZE As Single, ByRef ier As Integer)
  Friend Declare Sub MESSFIT Lib "MESSREF.DLL" (ByRef GWI As Single, ByRef IWS As Integer, ByRef MESS As Integer, ByRef NWE As Integer, ByRef QW As Single, ByRef QS As Single, ByRef QG As Single, ByRef WIST As Single, ByRef WSOL As Single, ByRef RMAST As Single, ByRef RSLA As Single, ByRef RRECH As Single, ByRef Ier As Integer)
  Friend Declare Sub DIFFREF Lib "MESSREF.DLL" (ByRef NWE As Integer, ByRef IRT As Integer, ByRef KW As Integer, ByRef KNO As Integer, ByRef RB As Single, ByRef RP As Single, ByRef DE As Single, ByRef DL As Single, ByRef DC As Single, ByRef DH As Single, ByRef DA As Single, ByRef DB As Single, ByRef IER As Integer)
  '
  'Zusatzprogramm zur Manupulation der Reflexionswerte (wird z.Zt. nicht verwendet)
  '
  '
  '
  Private Declare Sub RMAUXIL Lib "RMESSAUX.DLL" (ByRef MessID As Integer, ByRef MessgID As Integer, ByRef ReTr As Integer, ByRef NG As Integer, ByRef GK As Single, ByRef nwe As Integer, ByRef km As Integer, ByRef Wsol As Single, ByRef rrmes As Single, ByRef ier As Integer)

  '
  '
  '
  '
 

  Private Sub txtWID_Leave(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MNtxtWID.Leave
    MNbtnEND.Enabled = False
    clasm.einst(0)
    MNbtnEND.Enabled = True
  End Sub




  'Dim claOPX As New clsOPX
  'Dim claOPX As New clsOPX
  'UPGRADE_WARNING: Event cboCOM.SelectedIndexChanged may fire when form is initialized. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="88B12AE1-6DE0-48A0-86F1-60C0686C026A"'
  Private Sub MNcboCOM_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MNcboCOM.SelectedIndexChanged, MNcboBAUD.SelectedIndexChanged
    If MNcboBAUD.SelectedIndex < 0 Or MNcboCOM.SelectedIndex < 0 Then Exit Sub
    MNbtnEND.Enabled = False
    MessgCom = MNcboCOM.SelectedIndex
    MessgBaud = MNcboBAUD.Items(MNcboBAUD.SelectedIndex)
    MNbtnEND.Enabled = True
  End Sub




  Private Sub MNcboSPE_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MNcboSPE_0.SelectedIndexChanged, MNcboSPE_1.SelectedIndexChanged

    Dim Index As Short
    Index = CInt(eventSender.name.ToString.Substring(7, 1))


    MNbtnEND.Enabled = False
    clasm.einst(Index)
    MNbtnEND.Enabled = True
  End Sub


  Private Sub MNbtnABR_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MNbtnABR.Click
    ABRchk = True
    NXTChk = True
    WinChk = True
    If IsNothing(clasm) Then Exit Sub
    clasm.IchStat = -1

    System.Windows.Forms.Application.DoEvents()
    If MnMessgMenue = 6 Then
      Call EndMess()
    End If
  End Sub

  Private Sub MNbtnEND_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MNbtnEND.Click
    Call EndMess()
  End Sub

  Private Sub MNbtnLOE_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MNbtnLOE.Click
    Call RmerdMod(ier)
    If ier > 0 Then
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    MNbtnLOE.Enabled = False
  End Sub

  Private Sub MNbtnMES_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) _
  Handles MNbtnMES_0.Click, MNbtnMES_1.Click, MNbtnMES_2.Click, MNbtnMES_3.Click, MNbtnMES_4.Click, MNbtnMES_5.Click
    Dim Index As Short
    Index = CInt(eventSender.name.ToString.Substring(7, 1))
    Call Messen(Index)
    MNbtnABR.Enabled = False
  End Sub


  Private Sub MNbtnMIT_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MNbtnMIT.Click
    clasm.IchStat = 0
    NXTChk = True
    MITchk = True
    System.Windows.Forms.Application.DoEvents()
  End Sub

  Private Sub MNbtnNXT_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MNbtnNXT.Click
    NXTChk = True
  End Sub

  Private Sub MNbtnSTR_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MNbtnSTR.Click
    clasm.IchStat = 1
    NXTChk = True
    MNflgkal.Visible = False
  End Sub

  Private Sub MNbtnWIN_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MNbtnWIN.Click
    WinChk = True
  End Sub
  Sub Messen(ByVal index As Integer)
    'index
    '-1 Ende Messung
    '0  Messen
    '1  Weißstandard
    '2  Schwarzstandard
    '3  Graustandard
    '4  Weissstandard (Transmission)
    '5  Schwarzstandard (Transmission)
    Dim Nwe As Integer
    Dim kw As Integer
    Dim nku As Integer
    Dim i As Integer
    ABRchk = False
    NXTChk = False
    MITchk = False
    MNlblMES.Visible = True
    If clasm.IchStat = -1 Then
      MNParentform.DialogResult = DialogResult.Abort
      Exit Sub
    End If
    '
    Select Case index
      '
      Case -1
        Call EndMess()
      Case 0
MESstr:

        'Messen
        '
        Try
          Mnier = 0
          iami = 0
          MnReFel.Iami = 0
          MnReFel.Name = ""
          MnReFel.Bem = ""
          MnReFel.Banum = ""
          MnName = ""
          MnBEM = ""
          MnBanum = ""
          System.Windows.Forms.Application.DoEvents()
          MnReFel.ReTr = MenueParam.Messg.ReTr
          clasm.ReTr = MnReFel.ReTr
          '
          '
          Application.DoEvents()
          clasm.MesMes()
          '
          '
        Catch ex As Exception
          MsgBox("MeasDevModul.Messen")
          MsgBox(ex.Message)
          clasm.IchStat = -1
        End Try

        Mnier = clasm.IchStat
        If clasm.IchStat = 1 Then GoTo MESstr
        If clasm.IchStat = -1 Then GoTo MESfeh
        Call Wait(MessgTwait)
        MNlblANZ.Visible = False
        If MnName <> "" Then
          MnReFel.Name = MnName
        End If
        If MnBEM <> "" Then
          MnReFel.Bem = MnBEM
        End If
        If MnBanum <> "" Then
          MnReFel.Banum = MnBanum
        End If
        MnReFel.Cme = MessgKenn
        MnReFel.RefKurv.clear()
        MnReFel.Iami = iami
        MnReFel.Gid = MessgGID
        Nwe = MessgNwe
        For kw = 0 To MessgKM - 1
          MnReFel.De(kw) = DEwmes(kw)
          MnReFel.RefKurv.Add(MessgChrm(kw), New CurveRef(Nwe))
          nku = kw * MessgNwe
          For i = 0 To MessgNwe - 1
            MnReFel.RefKurv(MessgChrm(kw)).R(i) = rrmes(nku + i)
          Next i
        Next kw
        MNParentform.DialogResult = DialogResult.OK
        Exit Sub
MESfeh:
        Call RwrtFeh(Rhilf)
        Mnier = clasm.IchStat
        MNlblANZ.Visible = False
        If MNParentform.Visible Then
          MNParentform.DialogResult = DialogResult.Abort
        End If
        Exit Sub
      Case Else
        '
        If MnMessgMenue = 1 Then
          '
          '
          '    Kalibrieren
          '
          '
          '
KalStr:
          '
          irmax = MessgIkal
          If irmax <= 0 Then
            irmax = 1
          End If




          btnMES(index).Enabled = False
          MessgImess = index
          '
          '
          '
          '
          'geräteabhängige Driver
          '
          '
          '
          Application.DoEvents()
          clasm.MesKal(MessgImess)
          '
          '
          Mnier = clasm.IchStat
          If clasm.IchStat = -1 Then
            MNParentform.DialogResult = DialogResult.Abort
            Exit Sub
          End If
          If clasm.IchStat = 1 Then
            GoTo KalStr
          End If
          '
          'Abspeichern
          '
          '

          Call KalSpei(LKal(index), Rhilf)
          IdkaID = IdkaID + 1
          Call KalGrid(IdkaID, rsmes)

          If idka(IdkaID) = 0 Then
            MNbtnEND.Enabled = True
          Else
            For i = 0 To btnMES.Count - 1
              btnMES(i).Visible = False
            Next i
            btnMES(idka(IdkaID)).Enabled = True
            btnMES(idka(IdkaID)).Visible = True
          End If
          MNlblMES.Visible = False

        ElseIf MnMessgMenue = 2 Then
          '
          '
          'Nullkalibrieren
          '
          '
          '
NulStr:
          '
          irmax = 1
          If irmax <= 0 Then
            irmax = 1
          End If




          btnMES(index).Enabled = False
          MessgImess = index + 5
          '
          '
          '
          'geräteabhängige Driver
          '
          '
          '
          Application.DoEvents()
          clasm.MesKal(MessgImess)
          Mnier = clasm.IchStat

          '
          '
          If clasm.IchStat = -1 Then
            MNParentform.DialogResult = DialogResult.Abort
            Exit Sub
          End If
          If clasm.IchStat = 1 Then
            GoTo NulStr
          End If
          IdkaID = IdkaID + 1
          Call KalGrid(IdkaID, rsmes)
          If idka(IdkaID) = 0 Then
            MNbtnEND.Enabled = True
          Else
            btnMES(idka(IdkaID)).Enabled = True
            btnMES(idka(IdkaID)).Visible = True
          End If
          MNlblMES.Visible = False

        End If
    End Select
    MNlblMES.Visible = False
    '
    '
    'Falls kein Text angegeben wird automatisch gestartet
    '
    '
    '
    'If btnMES(idka(IdkaID)).Text = "" And idka(IdkaID) <> 0 Then
    ' btnMES(idka(IdkaID)).PerformClick()
    ' End If
  End Sub
  Sub EndMess()
    If MnMessgMenue <> 6 AndAlso (IsNothing(clasm) OrElse clasm.IchStat = -1) Then
      MNParentform.DialogResult = DialogResult.OK
      Exit Sub
    End If
    Select Case MnMessgMenue
      Case 0
        '
        '
        'Messen
        '
        '
        '
        MNParentform.DialogResult = DialogResult.OK
      Case 1
        '
        '
        'Kalibrieren
        '
        '
        '
        '  Kalibrierwerte einlesen
        MessgKalw = 1
        Call KalWrt(2, Reftra, iheute, ier)

        '

        MNParentform.DialogResult = DialogResult.OK
      Case 2
        '
        '
        'Nullkalibrieren
        '
        '
        '
        MNParentform.DialogResult = DialogResult.OK
      Case 3
        '
        '     Initialisieren
        '

        '
        'Prüfen, ob Werte in Ordnung
        '
        Call IniGrid(MNflgkal, iermes)
        If MessgWist(0) > 500 Or MessgQW(0) > 500 Or MessgQW(0) <= 0.0 Then
          imsg = MessageBox.Show(Texxt(3546), Texxt(2000), MessageBoxButtons.YesNo)
          If imsg <> DialogResult.No Then
            Call DefaultValues()
            Call GrdIni(MNflgkal)
            Call IniGrid(MNflgkal, iermes)
          End If
        End If
        '
        'Abspeichern: ja/nein
        '
        '
        '
        imsg = MessageBox.Show(Texxt(3545), Texxt(2000), MessageBoxButtons.YesNo)
        If imsg <> DialogResult.No Then
          Call IniSpei()
          Call AlternativeSpei()
          '
          '
          '
          'geräteabhängige Abspeicherung
          'z.B. Istwellenlängen oder
          'Absolutwerte für Weiß- und Schwarzstandard
          'in den Speicher des Messgerätes zurückschreiben
          '
          '
          If WithClasm Then
            clasm.MesSpei()
            MessgKalw = 0
          End If
        End If
        MNParentform.DialogResult = DialogResult.OK


      Case 6
          '
          '
          'Manuel
          '
          Call ManGrid(MnReFel, iermes)
          MNParentform.DialogResult = DialogResult.OK
      Case Else
    End Select
  End Sub
  Sub DefaultValues()
    Dim kw As Short
    Dim i As Short
    Dim nkw As Short
    For kw = 0 To MessgKM - 1
      nkw = kw * MessgNwe
      For i = 0 To MessgNwe - 1
        MessgWist(nkw + i) = MessgWsol(nkw + i)
        MessgQW(nkw + i) = 1.0#
        MessgQS(nkw + i) = 0.0#
        MessgQG(nkw + i) = 0.0#
        MessgQT(nkw + i) = 1.0#
        MessgQB(nkw + i) = 0.0#
        MessgMW(nkw + i) = 1.0#
        MessgMS(nkw + i) = 0.0#
        MessgMG(nkw + i) = 0.0#
        MessgMT(nkw + i) = 1.0#
        MessgMB(nkw + i) = 0.0#
      Next i
    Next kw

  End Sub

  Sub KalIntDat(ByRef iheute As Short)
    Dim kw As Short
    Dim nkw As Short
    Dim i As Short
    MessgID = MnMessg.MessgID
    GKwrtID = MnMessg.GKwrtID
    MessgUserGID = MnMessg.UserRefGID
    MnMessg.QW.clear()
    MnMessg.QS.clear()
    MnMessg.QG.clear()
    MnMessg.QT.clear()
    MnMessg.QB.clear()
    MnMessg.MW.clear()
    MnMessg.MS.clear()
    MnMessg.MG.clear()
    MnMessg.MT.clear()
    MnMessg.MB.clear()
    MnMessg.Wist.clear()
    '
    '
    '
    '
    '
    '
    '
    
    Call KalWrt(2, Reftra, iheute, ier)
    If ier <> 0 Then
      Exit Sub
    End If
    If iheute = 0 Or iheute = 1 Then
      For kw = 0 To MnMessg.Winkel.Km - 1
        MnMessg.QW.Add(MnMessg.Winkel(kw).Chrm, New CurveRef(MnMessg.Winkel.Wsol.Nwe))
        MnMessg.QS.Add(MnMessg.Winkel(kw).Chrm, New CurveRef(MnMessg.Winkel.Wsol.Nwe))
        MnMessg.QG.Add(MnMessg.Winkel(kw).Chrm, New CurveRef(MnMessg.Winkel.Wsol.Nwe))
        MnMessg.MW.Add(MnMessg.Winkel(kw).Chrm, New CurveRef(MnMessg.Winkel.Wsol.Nwe))
        MnMessg.MS.Add(MnMessg.Winkel(kw).Chrm, New CurveRef(MnMessg.Winkel.Wsol.Nwe))
        MnMessg.MG.Add(MnMessg.Winkel(kw).Chrm, New CurveRef(MnMessg.Winkel.Wsol.Nwe))
        MnMessg.Wist.Add(MnMessg.Winkel(kw).Chrm, New CurveRef(MnMessg.Winkel.Wsol.Nwe))

        nkw = kw * MnMessg.Winkel.Wsol.Nwe
        For i = 0 To MnMessg.Winkel.Wsol.Nwe - 1
          MnMessg.QW((MnMessg.Winkel(kw).Chrm)).R(i) = MessgQW(nkw + i)
          MnMessg.QS((MnMessg.Winkel(kw).Chrm)).R(i) = MessgQS(nkw + i)
          MnMessg.QG((MnMessg.Winkel(kw).Chrm)).R(i) = MessgQG(nkw + i)
          MnMessg.MW((MnMessg.Winkel(kw).Chrm)).R(i) = MessgMW(nkw + i)
          MnMessg.MS((MnMessg.Winkel(kw).Chrm)).R(i) = MessgMS(nkw + i)
          MnMessg.MG((MnMessg.Winkel(kw).Chrm)).R(i) = MessgMG(nkw + i)
          MnMessg.Wist((MnMessg.Winkel(kw).Chrm)).R(i) = MessgWist(nkw + i)
        Next i
      Next kw
      MessgKalw = 1
      Mnier = -iheute
    Else
      MessgKalw = 0
      Mnier = -iheute
    End If




  End Sub

  '
  '
  '
  '
  '
  Function LKal(ByRef i As Short) As Short
    Select Case i
      Case 1
        '
        'Kalibrierweiss (Reflexion)
        '
        LKal = 1
      Case 2
        '
        'Kalibrierschwarz (Reflexion)
        '
        LKal = 2
      Case 3
        '
        'Kalibriergrau (Reflexion)
        '
        LKal = 3
      Case 4
        '
        'Kalibrierweiss (Transmission)
        '
        LKal = 8
      Case 5
        '
        'Kalibrierschwarz (Transmission)
        '
        LKal = 9
    End Select

  End Function
  Function LAbs(ByRef i As Short) As Short
    Select Case i
      Case 1
        '
        'Absolutweiss (Reflexion)
        '
        LAbs = 4
      Case 2
        '
        'Absolutschwarz (Reflexion)
        '
        LAbs = 5
      Case 3
        '
        'Absolutgrau (Reflexion)
        '
        LAbs = 6
      Case 4
        '
        'Absolutweiss (Transmission)
        '
        LAbs = 10
      Case 5
        '
        'Absolutschwarz (Transmission)
        '
        LAbs = 11
    End Select

  End Function





  Function BufFiltr(ByRef BUF As String) As String
    Dim i As Short
    Dim ilen As Short
    ilen = Len(BUF)
    Dim Iasc As Short
    Dim CCC As String
    BufFiltr = ""
    For i = 0 To ilen - 1
      CCC = Mid(BUF, i, 1)
      Iasc = Asc(CCC)
      If Iasc = 10 Or Iasc = 13 Or Iasc > 31 Then
        BufFiltr = BufFiltr & CCC
      End If
    Next i
  End Function


  Sub RmerdMod(ByRef ier As Short)
    Dim IIs As Short
    Call RMERD(MessgNwe, MessgKM, rrmes(0), ierL)
    ier = ierL
    '
    '
    '
    '
    'Aufruf von RMEMI (in RwrSpei) mit IAMI=-1 berechnet Farbkoordinaten neu;
    'speichert keine weitere Kurve ab!!
    'IAMI wird anhand der bereits gespeicherten Werte neu berechnet
    '
    '
    '
    iami = -1
    MesZae = MesZae - 1
    MNlblANZ.Text = LblAnze(MesZae, Mesmax)
    Call RwrSpei(IIs, MesZae)

  End Sub

  Sub Wait(ByRef Ianz As Integer)
    If Ianz = 0 Then Exit Sub
    '
    'Ianz in 1/100-Sekunden
    '
    '
    System.Windows.Forms.Application.DoEvents()
    Call Wart(Ianz)
  End Sub


  Function KnSTR(ByRef Text As String, ByRef comp As String) As Short
    Dim i As Short
    Dim j As Short
    Dim k As Short
    Dim Lcomp As Short
    '
    '
    'wie Fnction INSTR
    'in Comp dürfen ?-Zeichen als wild card character verwendet werden
    Lcomp = Len(comp)
    KnSTR = 0
    k = 1
    Do While Len(Mid(Text, k)) >= Lcomp
      j = 0
      For i = 1 To Len(comp)
        If Mid(comp, i, 1) <> "?" Then
          If Mid(Text, i + k - 1, 1) <> Mid(comp, i, 1) Then
            Exit For
          End If
        End If
        j = i
      Next i
      If j = Lcomp Then
        KnSTR = k
        Exit Function
      End If
      k = k + 1
    Loop
  End Function


  Sub RwrtSpei(ByRef istat As Short)
    Dim kw As Short
    Dim i As Short
    Dim nkw As Short
    Dim j As Short
    Dim imsg As DialogResult
    Dim DifNM As Single
    DifNM = 0.1 * (MessgWsol(2) - MessgWsol(0))
    For kw = 0 To MessgKM - 1
      nkw = kw * MessgNwe
      For i = 0 To MessgNwe - 1
        Rhilf(nkw + i) = rrmes(nkw + i)
      Next i
    Next kw
    '
    'Prüfen, ob benachbarte R-Werte nicht zu weit auseinander (nur für 1 bis MessgNwp sinnvoll)
    '
    For kw = 0 To MessgKM - 1
      nkw = kw * MessgNwe - 1
      For i = 0 To MessgNwp - 1
        j = nkw + i
        If i > 1 And i < MessgNwp Then
          If Rhilf(j - 1) > 0.0# Then
            Dif = 0.5 * Abs(Rhilf(j - 1) - 2.0# * Rhilf(j) + Rhilf(j + 1))
            If Dif > 0.25 * DifNM Then
              imsg = MessageBox.Show(CStr(MessgWsol(i)) & Texxt(4999), Texxt(2000), MessageBoxButtons.OKCancel)
              If imsg = DialogResult.OK Then
                istat = 1
              ElseIf imsg = DialogResult.Cancel Then
                istat = -1
              End If
            End If
          End If
        End If
      Next i
    Next kw
  End Sub

  Sub RwrSpei(ByRef istat As Short, ByRef MesZae As Short)
    Dim IamiL As Integer
    Dim imsg As DialogResult
    IamiL = iami
    Call RMEMI(MessgNwe, MessgKM, demes(0), dlmes(0), dcmes(0), dhmes(0), dames(0), dbmes(0), Alph, MessgDE, rrmes(0), IamiL, DEwmes(0), ierL)
    iami = IamiL
    Mnier = ierL

    If Abs(Mnier) <> 0 Then
      imsg = MessageOut(MessgKalMess, Abs(Mnier), Texxt(Abs(Mnier)), MessageBoxButtons.AbortRetryIgnore, Texxt(2000), NamLogFile)
      If imsg = DialogResult.Abort Then
        istat = -1
        Exit Sub
      ElseIf imsg = DialogResult.Retry Then
        Call RMERD(MessgNwe, MessgKM, rrmes(0), ierL)
        Mnier = ierL
        iami = -1
        MesZae = MesZae - 1
        MNlblANZ.Text = LblAnze(MesZae, Mesmax)
        '
        '
        'Aufruf von RMEMI mit IAMI=-1 berechnet Farbkoordinaten neu;
        '
        '
        IamiL = iami
        Call RMEMI(MessgNwe, MessgKM, demes(0), dlmes(0), dcmes(0), dhmes(0), dames(0), dbmes(0), Alph, MessgDE, rrmes(0), IamiL, DEwmes(0), ierL)
        iami = IamiL
        Mnier = ierL
      End If
    End If
    '
    '
    '
    If MesZae > 1 And iami > 1 Then
      '
      '
      '
      '
      Call SCmit(demes, dlmes, dcmes, dhmes, dames, dbmes, MNflgkal)
    Else
      MNflgkal.Visible = False
    End If

  End Sub

  Sub RwrtFeh(ByRef Rhilf() As Single)
    Dim i As Short
    Dim kw As Short
    Dim nkw As Short
    For kw = 0 To MessgKM - 1
      nkw = kw * MessgNwe
      For i = 0 To MessgNwe - 1
        Rhilf(nkw + i) = -999
      Next i
    Next kw
  End Sub

  Sub KalMit(ByRef irmax As Object, ByRef rsmes As Object)
    Dim i As Short
    Dim kw As Short
    Dim nkw As Short
    For kw = 0 To MessgKM - 1
      nkw = kw * MessgNwe
      For i = 0 To MessgNwe - 1
        rsmes(nkw + i) = rsmes(nkw + i) / CSng(irmax)
      Next i
    Next kw
  End Sub

  Sub KalSum(ByRef kw As Short, ByRef rsmes() As Single, ByRef rrmes() As Single)
    Dim i As Short
    Dim j As Short
    Dim nkw As Short
    nkw = kw * MessgNwe
    For i = 0 To MessgNwe - 1
      j = nkw + i
      rsmes(j) = rsmes(j) + rrmes(i)
    Next i
  End Sub


  Function LblAnze(ByRef MesZae As Short, ByRef Mesmax As Short) As String
    LblAnze = CStr(MesZae) & Texxt(492) & CStr(Mesmax)
  End Function






  Sub GrdIni(ByRef flgKAL As DataGridView)
    Dim kw As Short
    Dim i As Short
    Dim l As Short
    Dim ll As Short
    Dim jjmes As Short

    '
    'Ist-Wellenlängen
    '
    jjmes = 0
    flgKAL.Columns(1).ReadOnly = False
    For kw = 0 To MessgKM - 1
      For i = 0 To MessgNwe - 1
        If MessgWist(kw * MessgNwe + i) = 0 Then MessgWist(kw * MessgNwe + i) = MessgWsol(kw * MessgNwe + i)
        TblKal.Rows(jjmes)(0) = Format(MessgWsol(kw * MessgNwe + i), "#000.00")
        TblKal.Rows(jjmes)(1) = Format(MessgWist(kw * MessgNwe + i), "#000.00")
        jjmes = jjmes + 1
      Next i
      flgKAL.Rows(kw * MessgNwe).HeaderCell.Value = MessgChrm(kw)
    Next kw
    flgKAL.RowHeadersWidthSizeMode = DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders
   

    '
    '
    'Absolutwerte
    '
    '
    '
    For l = 0 To 3
      ll = l + 2
      If idka(l) <> 0 Then
        flgKAL.Columns(ll).ReadOnly = False
        jjmes = 0
        For i = 0 To Mdim
          Select Case idka(l)
            Case 1
              TblKal.Rows(jjmes)(ll) = Format(100 * CDbl(CStr(MessgQW(i))), "###.000")
            Case 2
              TblKal.Rows(jjmes)(ll) = Format(100 * CDbl(CStr(MessgQS(i))), "###.000")
            Case 3
              TblKal.Rows(jjmes)(ll) = Format(100 * CDbl(CStr(MessgQG(i))), "###.000")
            Case 4
              TblKal.Rows(jjmes)(ll) = Format(100 * CDbl(CStr(MessgQT(i))), "###.000")
            Case 5
              TblKal.Rows(jjmes)(ll) = Format(100 * CDbl(CStr(MessgQB(i))), "###.000")
          End Select
          jjmes = jjmes + 1
        Next i
      Else
        Exit For
      End If
    Next l

  End Sub


  Sub IniGrid(ByRef flgKAL As DataGridView, ByRef iermes As Short)
    Dim kw As Short
    Dim i As Short
    Dim jj As Short
    Dim l As Short
    Dim ll As Short
    '
    '
    'Initialisierungswerte übernehmen und abspeichern
    '
    'Ist-Wellenlängen
    '
    iermes = 0
    Try
      jj = 0
      For kw = 0 To MessgKM - 1
        For i = 0 To MessgNwe - 1
          MessgWist(kw * MessgNwe + i) = Singl(TblKal.Rows(jj)(1))
          jj = jj + 1
        Next i
      Next kw

      '
      '
      'Absolutwerte
      '
      '
      '

      '
      For l = 0 To 4
        ll = l + 2
        If idka(l) <> 0 Then
          jj = 0
          For i = 0 To Mdim
            Select Case idka(l)
              Case 1
                MessgQW(i) = 0.01 * Singl(TblKal.Rows(jj)(ll))
              Case 2
                MessgQS(i) = 0.01 * Singl(TblKal.Rows(jj)(ll))
              Case 3
                MessgQG(i) = 0.01 * Singl(TblKal.Rows(jj)(ll))
              Case 4
                MessgQT(i) = 0.01 * Singl(TblKal.Rows(jj)(ll))
              Case 5
                MessgQB(i) = 0.01 * Singl(TblKal.Rows(jj)(ll))
            End Select
            jj = jj + 1
          Next i
        Else
          Exit For
        End If
      Next l
      '
      '
    Catch
      iermes = 3550
      MsgBox(Texxt(3550) & CStr(i) & "," & CStr(jj))
    End Try



  End Sub
  Sub ManGrid(ByRef ReFel As RefValue, ByRef iermes As Short)
    Dim nwe As Short
    Dim kw As Short
    Dim i As Short
    '
    iermes = 0
    '
    '
    ReFel.RefKurv.clear()
    ReFel.Iami = 0
    ReFel.Name = ""
    ReFel.Bem = ""
    ReFel.Banum = ""
    ReFel.Cme = MessgKenn
    ReFel.Iami = 1
    nwe = MessgNwe
    For kw = 0 To MessgKM - 1
      ReFel.De(kw) = 0.0#
      ReFel.RefKurv.Add(MessgChrm(kw), New CurveRef(nwe))
      For i = 0 To MessgNwe - 1
        ReFel.RefKurv(MessgChrm(kw)).R(i) = 0.01 * Singl(TblKal.Rows(i)(kw + 1))
      Next i
    Next kw

    '
    '


  End Sub

  Sub IniSpei()
    Dim i As Short
    'Abspeichern
    '
    '
    For i = 1 To 5
      Select Case i
        Case 1
          '
          '
          'Absolutweiß
          '
          '
          Call KalSpei(LAbs(i), MessgQW)
        Case 2
          '
          '
          'Absolutschwarz
          '
          '
          Call KalSpei(LAbs(i), MessgQS)
        Case 3
          '
          '
          'Absolutgrau
          '
          '
          Call KalSpei(LAbs(i), MessgQG)
        Case 4
          '
          '
          'Absolutweiß(Transmission)
          '
          '
          Call KalSpei(LAbs(i), MessgQT)
        Case 5
          '
          '
          'Absolutschwarz(Transmission)
          '
          '
          Call KalSpei(LAbs(i), MessgQB)

      End Select
    Next i
    '
    '
    'Istwellenlänge
    '
    '

    Call KalSpei(7, MessgWist)
    '
    'Datum für Weißkalibrierung (Reflexion) zurücksetzen (damit ggf. neu kalibriert wird)
    '
    SQLStmt = "UPDATE " & TableKalib & " SET KALIB_DATTIM =? WHERE ART_ID=1 AND MESSG_ID=" & MessgID & " AND MESSG_KENN='" & MessgKenn & "'"
    CmdKal.CommandText = SQLStmt
    CmdKal.Parameters.Clear()
    CmdKal.Parameters.Add(New OleDbParameter("KALIB_DATTIM", OleDbType.Date))
    CmdKal.Parameters("KALIB_DATTIM").Value = Now

    If SQLExeNonQuery(CmdKal, Cnkal) <> 0 Then
      Exit Sub
    End If

    '
    'Datum für Weißkalibrierung (Transmission) zurücksetzen (damit ggf. neu kalibriert wird)
    '

    SQLStmt = "UPDATE " & TableKalib & " SET KALIB_DATTIM =? WHERE ART_ID=4 AND MESSG_ID=" & MessgID & " AND MESSG_KENN='" & MessgKenn & "'"
    CmdKal.CommandText = SQLStmt
    CmdKal.Parameters.Clear()
    CmdKal.Parameters.Add(New OleDbParameter("KALIB_DATTIM", OleDbType.Date))
    CmdKal.Parameters("KALIB_DATTIM").Value = Now

    If SQLExeNonQuery(CmdKal, Cnkal) <> 0 Then
      Exit Sub
    End If

  End Sub
  Sub AlternativeSpei()
    Dim Sqlstmt As String
    If Len(MessgProg) > 32 Then
      MessgProg = MessgProg.Substring(0, 32)
    End If
    If Not TableExists(TblMeasDevice, Cnkal) Then
      '
      'Speichern in TBL_MESSG
      '
      Sqlstmt = "UPDATE TBL_MESSG SET [MESSG_BAUD]=" & MessgBaud & ",[MESSG_COM]=" & MessgCom & ",[MESSG_PROG]='" & AddHkom(MessgProg, 32) & "' WHERE [MESSG_ID]=" & MessgID
      CmdKal.CommandText = Sqlstmt
      If SQLExeNonQuery(CmdKal, ) <> 0 Then
        Exit Sub
      End If

    Else
      '
      '
      'Speichern nach TBL_DEVICE
      '
      'Werte löschen
      '
      Sqlstmt = "DELETE FROM " & TblMeasDevice & " WHERE MESSG_ID=" & MessgID & " AND MESSG_KENN='" & AddHkom(MessgKenn, 2) & "'"
      CmdKal.CommandText = Sqlstmt
      If SQLExeNonQuery(CmdKal, Cnkal) <> 0 Then
      End If
      '
      '
      'Werte speichern
      '
      '
      Sqlstmt = "INSERT INTO " & TblMeasDevice & " (MESSG_ID,MESSG_KENN,MESSG_COM,MESSG_BAUD,MESSG_PROG)" _
              & " VALUES(" & MessgID & ",'" & AddHkom(MessgKenn, 2) & "'," & MessgCom & "," & MessgBaud & ",'" & AddHkom(MessgProg, 32) & "')"
      CmdKal.CommandText = Sqlstmt
      If SQLExeNonQuery(CmdKal, Cnkal) <> 0 Then
      End If
    End If
  End Sub


  Sub KalGrid(ByRef k As Short, ByRef rmes() As Single)
    Dim i As Short
    Dim nkw As Short
    Dim kw As Short
    Dim Sum As Single
    Dim Forr As String
    If k >= TblKal.Columns.Count Then Exit Sub
    '
    '
    'Mittelwert zur Festlegung des Formats
    '
    '
    Sum = 0.0#
    For kw = 0 To MessgKM - 1
      nkw = kw * MessgNwe
      For i = 0 To MessgNwe - 1
        Sum = Sum + rmes(nkw + i)
      Next i
    Next kw
    Sum = Sum / (MessgKM * MessgNwe)
    If Sum > 1.0# Then
      Forr = "##0.000"
    Else
      Forr = "0.0000"
    End If
    For kw = 0 To MessgKM - 1
      nkw = kw * MessgNwe
      For i = 0 To MessgNwe - 1
        TblKal.Rows(nkw + i)(k) = Format(rmes(nkw + i), Forr)
      Next i
    Next kw
  End Sub


  Sub RwrRhilf(ByRef rrmes() As Single, ByRef Rhilf() As Single)
    Dim i As Short
    Dim nkw As Short
    Dim kw As Short
    For kw = 0 To MessgKM - 1
      nkw = kw * MessgNwe
      For i = 0 To MessgNwe - 1
        Rhilf(nkw + i) = rrmes(nkw + i)
      Next i
    Next kw
  End Sub
  Sub RwrFin(ByRef ReTr As Short, ByRef istat As Short)
    Dim Kenn As Short
    Dim KennL As Integer
    '
    '
    'Die Verwendung von Integer führt bei DELPHI-DLL's zu Problemen
    '
    '
    '
    If iami <= 0 Then
      imsg = MessageOut(MessgKalMess, 4122, Texxt(4122), MessageBoxButtons.OK, Texxt(2000), NamLogFile)
      istat = -1
      Exit Sub
    End If
    KennL = Kenn
    RetrL = ReTr
    Call RMEPR(RetrL, MessgNwe, MessgKM, Romes(0), rrmes(0), Rumes(0), KennL, ierL)
    Mnier = ierL
    Kenn = KennL
    If Mnier <> 0 Then
      If Kenn <> 0 Then
        imsg = MessageOut(MessgKalMess, Abs(Mnier), CStr(MessgWsol(Kenn)) & Texxt(Abs(Mnier)) & Chr(13) & Texxt(1999), 2, Texxt(2000), NamLogFile)
        If imsg = 10 Then imsg = 5
      Else
        imsg = MessageOut(MessgKalMess, Mnier, Texxt(Mnier), 2, Texxt(2000), NamLogFile)
      End If
      If imsg = 4 Then
        istat = 1
      ElseIf imsg = 3 Or imsg = 7 Then
        istat = -1
      End If
    End If
    RetrL = ReTr

    'Call RMAUXIL(MessID, MessgID, RetrL, Iglz(0), gk(0),gkint(0), MessgNwe, MessgKM, MessgWsol(0), rrmes(0), ierL)
    'Mnier = CShort(ierL)
    If Mnier <> 0 Then
      istat = -1
    End If
  End Sub
  Sub KalSpei(ByRef Index As Short, ByRef Rhilf() As Single)
    Dim LDtext(11) As Short
    LDtext(7) = 469
    LDtext(1) = 471
    LDtext(2) = 472
    LDtext(3) = 473
    LDtext(4) = 477
    LDtext(5) = 478
    LDtext(6) = 479
    LDtext(8) = 474
    LDtext(9) = 475
    LDtext(10) = 480
    LDtext(11) = 481
    Mnier = 0
    SQLStmt = "DELETE * FROM TBL_KALIB WHERE MESSG_ID=" & MessgID & " AND ART_ID=" & Index & " AND MESSG_KENN='" & MessgKenn & "'"
    CmdKal.CommandText = SQLStmt
    If SQLExeNonQuery(CmdKal, Cnkal) <> 0 Then
      Mnier = -1
      Exit Sub
    End If
    SQLStmt = "INSERT INTO TBL_KALIB (MESSG_ID,ART_ID,MESSG_KENN,KALIB_BEZ,KALIB_DATTIM,KALIB_RWERT)" _
    & " VALUES (" & MessgID & "," & Index & ",'" & MessgKenn & "',?,?,?)"
    CmdKal.CommandText = SQLStmt
    CmdKal.Parameters.Clear()
    CmdKal.Parameters.Add(New OleDbParameter("KALIB_BEZ", OleDbType.Char))
    CmdKal.Parameters.Add(New OleDbParameter("KALIB_DATTIM", OleDbType.Date))
    CmdKal.Parameters.Add(New OleDbParameter("KALIB_RWERT", OleDbType.Binary))
    CmdKal.Parameters("KALIB_BEZ").Value = Texxt(LDtext(Index))
    CmdKal.Parameters("KALIB_DATTIM").Value = Now
    Hlfmes = GetBytes(Rhilf)
    CmdKal.Parameters("KALIB_RWERT").Value = Hlfmes
    If SQLExeNonQuery(CmdKal, Cnkal) <> 0 Then
      Mnier = -1
      Exit Sub
    End If
  End Sub

  Sub NormRwrt(ByRef RT As Short, ByRef kw As Short, ByRef rmes() As Single)
    Dim i As Short
    Dim j As Short
    Dim nkw As Short

    'falls RT>= 0 gilt
    'RRMES=RMES
    'falls
    'MESSGQW=MESSGMW=1.0
    'MESSGQS=MESSGMS=MESSGQG=MESSGMG=0.0
    'WIST=WSOL
    '
    i = 0
    nkw = kw * MessgNwe
    If RT = -1 Then
      '
      '
      '
      'Messwerte werden unkorrigiert übernommen (Korrektur wurde durch Messgerät vorgenommen)
      '
      '
      For i = 0 To MessgNwe - 1
        j = nkw + i
        rrmes(j) = rmes(i)
      Next i
    ElseIf RT = 0 Then
      '
      '
      'Reflexion
      '
      '
      '

      j = nkw + i
      Call REFWRT(MessgNwe, MessgQW(j), MessgQS(j), MessgQG(j), MessgMW(j), MessgMS(j), MessgMG(j), MessgWist(j), MessgWsol(j), rmes(0), rrmes(j), ier)
    ElseIf RT = 1 Then
      '
      '
      'Transmission
      '
      '

      j = nkw + i
      '
      'Grauwert=Schwarzwert ==> Grauwert wird nicht zur Berechnung vor rrmes verwendet
      'alternativ kann Grauwert=0.0 gesetzt werden
      '
      '
      Call REFWRT(MessgNwe, MessgQT(j), MessgQB(j), MessgQB(j), MessgMT(j), MessgMB(j), MessgMB(j), MessgWist(j), MessgWsol(j), rmes(0), rrmes(j), ier)

    End If
  End Sub

  Sub NullRwrt(ByRef ier As Integer)
    '

    Call RMNUL(MessgAka, MessgNwe, MessgKM, rsmes(0), NGK, gk(0, 0), MessgID, NormID, Fakt(0), XYZE(0, 0), ier)
  End Sub
  Sub PoClose(ByRef mscMES As SerialPort, ByRef iermes As Short)
    If iermes <> 0 Then
      Exit Sub
    End If
    iermes = 0
    Try
      If mscMES.IsOpen Then
        mscMES.Close()
      End If
    Catch
    End Try
  End Sub
  Sub PoOpen(ByRef mscMES As SerialPort, ByRef iermes As Short)
    iermes = 0
    Try
      If Not mscMES.IsOpen Then
        mscMES.PortName = "COM" & CStr(MessgCom)
        mscMES.BaudRate = MessgBaud
        mscMES.DataBits = MessgLength
        Select Case MessgPar
          Case "N"
            mscMES.Parity = Parity.None
          Case "O"
            mscMES.Parity = Parity.Odd
          Case "E"
            mscMES.Parity = Parity.Even
        End Select
        Select Case MessgHands
          Case 0
            mscMES.Handshake = Handshake.None
          Case 1
            mscMES.Handshake = Handshake.XOnXOff
          Case 2
            mscMES.Handshake = Handshake.RequestToSend
        End Select
        mscMES.DtrEnable = True
        mscMES.RtsEnable = True
        mscMES.StopBits = MessgStop
        mscMES.Open()


        ' mscMES.CommPort = MessgCom
        ' mscMES.Settings = RTrim(LTrim(CStr(MessgBaud))) & "," & MessgPar & CStr(MessgLength) & CStr(MessgStop)
        '
        '

        'mscMES.PortOpen = True
        'tmrMES.Interval = 10
        'mscMES.InputLen = 0
      End If
    Catch
      Call Wait(200)
      iermes = 3526
      imsg = MsgBox(Texxt(iermes), 1, Texxt(2000))
      iermes = 1
      If imsg = 2 Then
        iermes = -1
      End If
    End Try
  End Sub

  '
  '
  '
  '
  '
  '
  Function RefNorm(ByRef Mk As Single, ByRef Mw As Single, ByRef Ms As Single, ByRef Mg As Single, ByRef Rw As Single, ByRef Rs As Single, ByRef Rg As Single) As Single
    'UPGRADE_WARNING: Lower bound of array an was changed from 1 to 0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Dim an(2) As Single
    Dim eps As Single
    Dim i As Short
    eps = 0.0000001
    '
    '
    '
    'Mk      Messwert der Probe
    'Mw      Messwert des Weißstandards
    'Ms      Messwert des Schwarzstandards
    'Mg      Messwert des Graustandards
    '
    'Rw      Absolutwert des Weißstandards
    'Rs      Absolutwert des Schwarzstandards
    'Rg      Absolutert des Graustandards


    If Mg = 0.0# Or Rg = 0.0# Or Mg = Ms Or Rs = Rg Then
      If (Mw - Ms) < eps Then
        RefNorm = 0.0#
      Else
        RefNorm = Rs + (Rw - Rs) * (Mk - Ms) / (Mw - Ms)
      End If
    Else
      an(0) = (Mw - Ms) * (Mw - Mg)
      an(1) = (Ms - Mw) * (Ms - Mg)
      an(2) = (Mg - Mw) * (Mg - Ms)
      For i = 0 To 2
        If Abs(an(i)) < eps Then
          If (Mw - Ms) < eps Then
            RefNorm = 0.0#
          Else
            RefNorm = Rs + (Rw - Rs) * (Mk - Ms) / (Mw - Ms)
          End If
          Exit Function
        End If
      Next i
      an(0) = (Mk - Ms) * (Mk - Mg) / an(0)
      an(1) = (Mk - Mw) * (Mk - Mg) / an(1)
      an(2) = (Mk - Mw) * (Mk - Ms) / an(2)
      RefNorm = Rw * an(0) + Rs * an(1) + Rg * an(2)
    End If
  End Function
  Sub SCmit(ByRef de() As Single, ByRef dl() As Single, ByRef dc() As Single, ByRef dh() As Single, ByRef da() As Single, ByRef db() As Single, ByRef flgKAL As DataGridView)
    Dim i As Short
    Dim Colbr As Single


    TblKal.Rows.Clear()
    TblKal.Columns.Clear()
    'colbr = (Wid - 250) / 10 - 20
    TblKal.Columns.Add("      ", GetType(String))
    TblKal.Columns.Add(Trim(TexKt(10138)), GetType(String))
    TblKal.Columns.Add(Trim(TexKt(10139)), GetType(String))
    TblKal.Columns.Add(Trim(TexKt(10140)), GetType(String))
    TblKal.Columns.Add(Trim(TexKt(10141)), GetType(String))
    TblKal.Columns.Add(Trim(TexKt(10142)), GetType(String))
    TblKal.Columns.Add(Trim(TexKt(10143)), GetType(String))
    'flgKAL.Cols = 7
    'flgKAL.Rows = 6
    'flgKAL.Height = VB6.TwipsToPixelsY((flgKAL.Rows + 1) * flgKAL.get_RowHeight(0) + 150)
    'flgKAL.Row = 0
    'flgKAL.set_ColWidth(0, 4.0# * colbr)
    For i = 0 To 4
      TblKal.Rows.Add(TblKal.NewRow)
      TblKal.Rows(i)(0) = Texxt(495 + i)
      TblKal.Rows(i)(1) = Format(de(i), "#.000")
      TblKal.Rows(i)(2) = Format(dl(i), "#.000")
      TblKal.Rows(i)(3) = Format(dc(i), "#.000")
      TblKal.Rows(i)(4) = Format(dh(i), "#.000")
      TblKal.Rows(i)(5) = Format(da(i), "#.000")
      TblKal.Rows(i)(6) = Format(db(i), "#.000")
    Next i
    Colbr = (flgKAL.Size.Width - 80) / 7
    'flgKAL.TableStyles.Clear()
    'flgKAL.TableStyles.Add(New DataGridTableStyle)
    'flgKAL.TableStyles(0).MappingName = TblKal.TableName
    'flgKAL.CaptionText = Texxt(494)
    'flgKAL.TableStyles(0).AllowSorting = False
    'flgKAL.TableStyles(0).RowHeadersVisible = False
    flgKAL.AccessibleName = Texxt(494)
    flgKAL.DataSource = TblKal
    For i = 0 To TblKal.Columns.Count - 1
      'flgKAL.TableStyles(0).GridColumnStyles.Add(New DataGridTextBoxColumn)
      'flgKAL.TableStyles(0).GridColumnStyles.Add(New DataGridBoolColumn)
      flgKAL.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
      flgKAL.Columns(i).Name = TblKal.Columns(i).ColumnName
      flgKAL.Columns(i).HeaderText = TblKal.Columns(i).ColumnName & Space(2)
      flgKAL.Columns(i).DefaultCellStyle.Alignment = HorizontalAlignment.Right
      flgKAL.Columns(i).Width = Colbr
      flgKAL.Columns(i).ReadOnly = True
    Next i
    flgKAL.Columns(0).Width = 100
    flgKAL.Visible = True
  End Sub


  Sub RefWsolIstObs(ByRef nwe As Integer, ByRef r() As Single, ByRef wist() As Single, ByRef Wsol() As Single)
    'UPGRADE_WARNING: Lower bound of array RZ was changed from 1 to 0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Dim RZ(nwe - 1) As Single
    Dim fa2, fa1, fa3 As Single
    Dim i2, i1, i3 As Short
    Dim i As Short
    '
    '
    'Istwellenkorrektur
    '
    '
    For i = 0 To nwe - 1
      RZ(i) = r(i)
    Next i

    For i = 0 To nwe - 1
      If i = 0 Then
        i1 = 0
        i2 = 1
        i3 = 2
      ElseIf i = nwe - 1 Then
        i1 = nwe - 3
        i2 = nwe - 2
        i3 = nwe - 1
      Else
        i1 = i - 1
        i2 = i
        i3 = i + 1
      End If
      fa1 = (Wsol(i) - wist(i2)) * (Wsol(i) - wist(i3)) / ((wist(i1) - wist(i2)) * (wist(i1) - wist(i3)))
      fa2 = (Wsol(i) - wist(i1)) * (Wsol(i) - wist(i3)) / ((wist(i2) - wist(i1)) * (wist(i2) - wist(i3)))
      fa3 = (Wsol(i) - wist(i1)) * (Wsol(i) - wist(i2)) / ((wist(i3) - wist(i1)) * (wist(i3) - wist(i2)))
      r(i) = RZ(i1) * fa1 + RZ(i2) * fa2 + RZ(i3) * fa3
    Next i
  End Sub


  Sub KalWrt(ByRef ILkal As Short, ByRef Reftra As String, ByRef Iheute As Short, ByRef ier As Integer)
    Dim kw As Short
    Dim i As Short
    Dim nkw As Short
    Dim l As Short
    Dim imsg As Short
    Dim DataKalExists As Boolean = False
    Dim DataIniExists As Boolean = False
    Dim IheuR As Short
    Dim IheuT As Short
    Dim Wdif As Single
    '
    '
    ' 
    '
    'Iheute=0 Kalibrierwerte datumskonform eingelesen
    'Iheute=1 Kalibrierwerte mit zu altem Datum eingelesen
    'Iheute=2 Keine Kalibrierwerte gefunden (Kalibrierung fehlt)
    'iheute=3 Keine Absolutwerte gefunden (Inialisierung fehlt)
    'Iheute=4 Keine Absolutwerte und keine Kalibrierwerte (Kalibrierung und Initialisierung fehlt)
    'Iheute=6 Falsche Kalibrierwerte
    'Iheute=7 Falsche Absolutwerete
    '
    'Iheute=5 Falsches Messgerät
    '
    '
    ier = 0
    IheuT = 1
    IheuR = 1
    Iheute = 1
    '
    'Kalibrierwerte und Absolutwerte einlesen
    '
    'ILkal = 0 nur Kalibrierwerte
    'ILkal = 1 nur Absolutwerte
    'ILkal = 2 Kalibrierwerte und Absolutwerte
    '
    '
    SQLStmt = "SELECT * FROM " & TableKalib & " WHERE MESSG_ID=" & MessgID & " AND MESSG_KENN='" & MessgKenn & "'"
    CmdKal.CommandText = SQLStmt
    TblRs = DataReader(CmdKal, CommandBehavior.CloseConnection, Cnkal)
    If TblRs Is Nothing Then
      Exit Sub
    End If
    Call DefaultValues()
    Do While TblRs.Read
      If TblRs("Messg_ID") <> MessgID Then
        Iheute = 5
        Exit Do
      End If
      If TblRs("art_id") = 7 Then
        Hlfmes = TblRs("kalib_rwert")
        Rhilf = GetSingles(Hlfmes)
        If UBound(Rhilf) <> UBound(MessgWsol) Then
          MsgBox(Texxt(3649))
          ier = -1
          TblRs.Close()
          Exit Sub
        End If
        '
        For i = 0 To UBound(Rhilf)
          If Rhilf(i) <> 0.0# Then
            MessgWist(i) = Rhilf(i)
          End If
        Next i

        '
        '
        'Prüfen, ob MessgWist und MessgWsol kompatibel
        '
        Wdif = MessgWsol(1) - MessgWsol(0)
        If MessgWist(1) - MessgWist(0) < Wdif Then
          Wdif = MessgWist(1) - MessgWist(0)
        End If
        Wdif = 0.5 * Wdif
        If Abs(MessgWist(0) - MessgWsol(0)) > Wdif Or Abs((MessgWist(1) - MessgWist(0)) - (MessgWist(1) - MessgWist(0))) > Wdif Then
          MsgBox(Texxt(3649))
          ier = -1
          TblRs.Close()
          Exit Sub
        End If

      End If
      If ILkal = 0 Or ILkal = 2 Then
        '
        'Kalibrierwerte
        '
        For l = 0 To 4
          If idka(l) <> 0 And LKal(idka(l)) = TblRs("art_id") Then
            DataKalExists = True
            Hlfmes = TblRs("kalib_rwert")
            If UBound(Hlfmes) + 1 <> 4 * MessgKM * MessgNwe Then
              MsgBox(Texxt(3633))
              ier = -1
              TblRs.Close()

              Exit Sub
            End If

            Select Case idka(l)
              Case 1
                '
                'Kalibrierwerte (weiß)
                '
                MessgMW = GetSingles(Hlfmes)
                If Date.Now.Subtract(New TimeSpan(MessgKalInt, 0, 0)) < TblRs("kalib_dattim") Then
                  IheuR = 0
                End If
                'If DateDiff(Microsoft.VisualBasic.DateInterval.Hour, TblRs.Fields("kalib_date") , now) + Hour(TimeOfDay) - Hour(TblRs.Fields("kalib_time")) <= MessgKalInt Then
                'IheuR = 0
                'End If
              Case 2
                '
                'Kalibrierwerte (schwarz)
                '
                MessgMS = GetSingles(Hlfmes)
              Case 3
                '
                'Kalibrierwerte (grau)
                '
                MessgMG = GetSingles(Hlfmes)
              Case 4
                '
                'Kalibrierwerte (Transmission weiß)
                '
                If Date.Now.Subtract(New TimeSpan(MessgKalInt, 0, 0)) < TblRs("kalib_dattim") Then
                  IheuT = 0
                End If

                'If DateDiff(Microsoft.VisualBasic.DateInterval.Hour, TblRs.Fields("kalib_date"), now) + Hour(TimeOfDay) - Hour(TblRs.Fields("kalib_time")) <= MessgKalInt Then
                'IheuT = 0
                'End If
                MessgMT = GetSingles(Hlfmes)
              Case 5
                '
                'Kalibrierwerte (Transmission schwarz)
                '
                MessgMB = GetSingles(Hlfmes)
            End Select
          End If
        Next l
      End If
      If ILkal = 1 Or ILkal = 2 Then
        '
        'Absolutwerte
        '
        For l = 0 To 4
          If idka(l) <> 0 And LAbs(idka(l)) = TblRs("art_id") Then
            DataIniExists = True
            Hlfmes = TblRs("kalib_rwert")
            If UBound(Hlfmes) + 1 <> 4 * MessgKM * MessgNwe Then
              MsgBox(Texxt(3633))
              ier = -1
              TblRs.Close()
              Exit Sub
            End If
            Select Case idka(l)
              Case 1
                '
                'Absolutwerte (weiß)
                '
                MessgQW = GetSingles(Hlfmes)
              Case 2
                '
                'Absolutwerte (schwarz)
                '

                MessgQS = GetSingles(Hlfmes)
              Case 3
                '
                'Absolutwerte (grau)

                MessgQG = GetSingles(Hlfmes)
              Case 4
                '
                'Absolutwerte (Transmission weiß)

                MessgQT = GetSingles(Hlfmes)
              Case 5
                '
                'Absolutwerte (Transmission schwarz)

                MessgQB = GetSingles(Hlfmes)
                '
            End Select
          End If
        Next l
      End If
    Loop
    TblRs.Close()
    'If Not DataKalExists And Not DataIniExists Then
    'Iheute = 4
    'Exit Sub
    'End If

   
    If Not DataIniExists Then
      ' keine Einträge für Initialisierung gefunden
      Iheute = 3
      Exit Sub
    End If
    If Not DataKalExists Then
      ' keine Einträge für Kalibrierung gefunden
      Iheute = 2
      Exit Sub
    End If
    If Reftra = "R" And IheuR = 0 Then
      Iheute = 0
    End If
    If Reftra = "T" And IheuT = 0 Then
      Iheute = 0
    End If
    If Reftra = "A" And IheuR = 0 And IheuT = 0 Then
      Iheute = 0
    End If
    '
    '
    'Prüfen, ob Weiß-Schwarzdifferenz groß genug
    '
    '
    If MessgKalw = 1 Then
      For kw = 0 To MessgKM - 1
        nkw = kw * MessgNwe
        For i = 0 To MessgNwe - 1
          If Abs(MessgMW(nkw + i) - MessgMS(nkw + i)) > 0.01 Then
            Exit For
          End If
        Next i
        If i >= MessgNwe Then
          imsg = MessageOut(MessgKalMess, 3543, Texxt(3543), 0, Texxt(2000), NamLogFile)
          MessgKalw = 0
          Iheute = 6
          Exit Sub
        End If
        For i = 0 To MessgNwe - 1
          If Abs(MessgQW(nkw + i) - MessgQS(nkw + i)) < 0.01 Then
            imsg = MessageOut(MessgKalMess, 3544, Texxt(3544), 0, Texxt(2000), NamLogFile)
            MessgKalw = 0
            Iheute = 7

            Exit Sub
          End If
        Next i
      Next kw
    End If

  End Sub
  Sub GrdMan(ByRef flgKAL As DataGridView)
    Dim i As Short
    Dim kw As Short

    Dim j As Short
    '
    'Grid aufbauen
    '
    '
    'flgKAL.Cols = 1
    'flgKAL.Rows = 1
    'flgKal.fixed
    'flgKAL.set_RowHeight(0, 2 * hght)
    TblKal.Rows.Clear()
    TblKal.Columns.Clear()
    TblKal.Columns.Add(Texxt(468), GetType(String))
    For kw = 0 To MessgKM - 1
      TblKal.Columns.Add(MessgChrm(kw), GetType(String))
    Next
    For i = 0 To MessgNwe - 1
      TblKal.Rows.Add(TblKal.NewRow)
      TblKal.Rows(i)(0) = MessgWsol(i)
      For j = 1 To TblKal.Columns.Count - 1
        TblKal.Rows(i)(j) = 0
      Next
    Next
    'flgKAL.TableStyles.Clear()
    'flgKAL.TableStyles.Add(New DataGridTableStyle)
    'flgKAL.TableStyles(0).MappingName = TblKal.TableName
    'flgKAL.TableStyles(0).AllowSorting = False
    flgKAL.DataSource = TblKal
    For i = 0 To TblKal.Columns.Count - 1
      'flgKAL.TableStyles(0).GridColumnStyles.Add(New DataGridTextBoxColumn)
      'flgKAL.TableStyles(0).GridColumnStyles.Add(New DataGridBoolColumn)
      flgKAL.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
      flgKAL.Columns(i).Name = TblKal.Columns(i).ColumnName
      flgKAL.Columns(i).HeaderText = TblKal.Columns(i).ColumnName & Space(2)
      flgKAL.Columns(i).DefaultCellStyle.Alignment = HorizontalAlignment.Right

    Next i
    flgKAL.Columns(0).Width = 50
    flgKAL.Columns(0).ReadOnly = True
    flgKAL.Visible = True

  End Sub
  Sub GrdMes(ByRef IdkaID As Short, ByRef IbezNR() As Short, ByRef flgKAL As DataGridView)
    Dim i As Short
    Dim kw As Short
    Dim j As Short
    Dim nkw As Short
    '
    'Tabelle und Grid aufbauen
    '
    '
    '
    '
    '
    '
    TblKal.Rows.Clear()
    TblKal.Columns.Clear()
    TblKal.Columns.Add(Texxt(468), GetType(String))

    For i = 0 To IdkaID - 1
      TblKal.Columns.Add(Texxt(IbezNR(i)), GetType(String))
    Next i
    'flgKAL.TableStyles.Clear()
    'flgKAL.TableStyles.Add(New DataGridTableStyle)
    'flgKAL.TableStyles(0).MappingName = TblKal.TableName
    'flgKAL.TableStyles(0).AllowSorting = False
    flgKAL.DataSource = TblKal
    For i = 0 To TblKal.Columns.Count - 1
      'flgKAL.TableStyles(0).GridColumnStyles.Add(New DataGridTextBoxColumn)
      'flgKAL.TableStyles(0).GridColumnStyles.Add(New DataGridBoolColumn)
      flgKAL.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
      flgKAL.Columns(i).Name = TblKal.Columns(i).ColumnName
      flgKAL.Columns(i).HeaderText = TblKal.Columns(i).ColumnName & Space(2)
      flgKAL.Columns(i).ReadOnly = True
      flgKAL.Columns(i).Width = 120
      flgKAL.Columns(i).DefaultCellStyle.Alignment = HorizontalAlignment.Right
    Next i
    flgKAL.Columns(0).Width = 60
    flgKAL.Columns(0).DefaultCellStyle.Alignment = HorizontalAlignment.Left



    j = 0
    For kw = 0 To MessgKM - 1
      nkw = kw * MessgNwe
      For i = 0 To MessgNwe - 1
        TblKal.Rows.Add(TblKal.NewRow)
        TblKal.Rows(i + nkw)(0) = CStr(MessgWsol(i))
        For j = 1 To TblKal.Columns.Count - 1
          TblKal.Rows(i + nkw)(j) = Space(0)
        Next
      Next i
    Next kw
  

    flgKAL.Visible = True
    For kw = 0 To MessgChrm.Count - 1
      flgKAL.Rows(kw * MessgNwe).HeaderCell.Value = MessgChrm(kw)
    Next
    flgKAL.RowHeadersWidthSizeMode = DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders
  End Sub




  Sub GetComBuf(ByRef ineu As Short, ByRef stv As String, ByRef BUF As String, ByRef buflen As Short, ByRef iver As Short, ByRef delay As Short, ByRef wart As Short, ByRef ierMeld As Short, ByRef StrBuf As Short, ByRef Abuf As String, ByRef Ibuf As Short, ByRef Zbuf As String, ByRef mscMES As SerialPort, ByRef istat As Short)
    Dim ier As Short
    Dim ikv As Short
    Dim i2 As Short
    Dim Icheck As Boolean
    Dim Ist As Short
    Dim ilen As Short
    Dim Ipos As Short
    Dim Mbuf As String
    Dim imsg As Short
    Static chki As Boolean = False
    Dim BufLeA As Short
    Dim By() As Byte
    ABRchk = False
    MITchk = False
    'ineu    Anzahl neuer Starts (Senden und Lesen)
    'stv     Eingabepuffer
    'buf     Ausgabepuffer
    'buflen  minimale Bufferlänge, die bei Übernahme der Messwerte erreicht sein muß
    'iver    Anzahl Versuche um Daten zu erhalten
    'delay   Verzögerung in Timerintervallen
    'wart    Anzahl Wartezyclen in Timerintervallen
    'iermeld Textnummer für Fehlermeldung
    'STRBUF  Startposition  (>0) von BUF(=Eingabepuffer) zum Vergleich mit zbuf
    'Abuf    Vergleichsbuffer für Startposition
    'Ibuf    relative Position von BUF(INPUT-Buffer) bei der Vergleich mit Zbuf startet
    'Zbuf    zweiter Vergleichsbuffer
    'mscmes  MScomm-Control
    'tmrmes  Timer
    'ier     Nummer Fehlermeldung (0 oder iermeld
    '
    '
    'Abuf=""       IBUF=0         keine Prüfung
    'Abuf=""       IBUF<>0        Prüfen, ob zbuf=buf(ibuf......)
    'Abuf<>""      IBUF=0         Prüfen, ob abuf in Buf enthalten
    'Abuf<>""      IBUF<>0        Prüfen, ob abuf in Buf enthalten und zbuf=buf(pos(abuf)+ibuf)
    '  "           ibuf>0         von vorne
    '  "           ibuf<0         von hinten
    '
    '
    '
    '
    If chki Then
      Exit Sub
    End If
    chki = True
StartAfterErr:
    ier = 0
    If Not mscMES.IsOpen Then
      BUF = ""
      ier = 3523
      imsg = MessageOut(MessgKalMess, ier, Texxt(ier) & Chr(13) & Texxt(1999), 4, Texxt(2000), NamLogFile)
      If imsg = 7 Or imsg = 10 Then
        istat = -1
        chki = False
        Exit Sub
      Else
        Call PoOpen(mscMES, ier)
        If ier <> 0 Then
          istat = -1
          chki = False
          Exit Sub
        End If
      End If
    End If
    For i2 = 0 To ineu
      BufLeA = -1

      '
      '     Puffer löschen
      '
      mscMES.DiscardOutBuffer()
      mscMES.DiscardInBuffer()
      'mscMES.OutBufferCount = 0
      'mscMES.InBufferCount = 0
      BUF = ""
      '
      '
      Call Wait(delay)
      '
      '     Sende String zu Messgerät
      If Not mscMES.IsOpen Then GoTo StartAfterErr
      If stv <> "" Then
        Try
          mscMES.WriteTimeout = 100 * delay
          mscMES.Write(stv)
        Catch ex As Exception
          MsgBox(ex.Message)
          istat = -1
          ier = 3500
          chki = False
          Exit Sub
        End Try

      End If

      '

      Call Wait(delay)
      Call Wait(delay)

      For ikv = 0 To iver
        If ABRchk Then
          chki = False
          Exit Sub
        End If
        If MITchk Then
          chki = False
          Exit Sub
        End If
        'MsgBox(mscMES.BytesToRead)
        '
        '
        '
        '
        Call Wait(delay)
        Call Wait(wart)

        If mscMES.BytesToRead > 0 Or buflen = 0 Then
          If mscMES.BytesToRead = BufLeA Then
            Exit For
          End If
          BufLeA = mscMES.BytesToRead
        End If


        If istat = -1 Or istat = 1 Then
          chki = False
          Exit Sub
        End If

        '
        'Prüfe, ob Daten in Input-Buffer
        If mscMES.BytesToRead >= buflen Then
          Call Wait(delay)
          '
          '
          '            Uebernahme Daten
          '
          '
          ReDim By(BufLeA - 1)
          mscMES.Read(By, 0, BufLeA)
          BUF = GetString(By)
          If StrBuf < Len(BUF) And StrBuf <> 0 Then
            Mbuf = BUF.Substring(StrBuf - 1)
          Else
            Mbuf = ""
          End If
          Icheck = False
          If Abuf = "" Then
            Ist = 0
            Icheck = True
          Else
            If Ibuf >= 0 Then
              Ist = KnSTR(Mbuf, Abuf) 'von vorne
              If Ist > 0 Then Icheck = True
            Else
              Ist = JnSTR(Mbuf, Abuf) 'von hinten
              If Ist > 0 Then Icheck = True
            End If
          End If
          If Icheck Then
            If Ibuf = 0 Then
              chki = False
              Exit Sub
            Else
              Ipos = Ist + Ibuf
              ilen = Len(Zbuf)
              If Ipos > 0 And Ipos <= (Len(Mbuf) - ilen) Then
                'If Zbuf = Mid(Mbuf, Ipos, Ilen) Then
                If KnSTR(Mbuf.Substring(Ipos - 1, ilen), Zbuf) > 0 Then
                  chki = False
                  Exit Sub
                End If
              End If
            End If
            Call Wait(delay)
          End If
        End If
      Next ikv
    Next i2
    If buflen = 0 Then
      chki = False
      Exit Sub
    End If
    ier = ierMeld
    imsg = MessageOut(MessgKalMess, ier, Texxt(ier) & Chr(13) & Texxt(1999), 4, Texxt(2000), NamLogFile)
    If imsg = 7 Or imsg = 10 Then
      istat = -1
      chki = False
      Exit Sub
    Else
      GoTo StartAfterErr

    End If
    chki = False
  End Sub

  Sub GetComString(ByRef Autom As Short, ByRef stv As String, ByRef BUF As String, ByRef buflen As Short, ByRef TimeOut As Short, ByRef delay As Short, ByRef wart As Short, ByRef ierMeld As Short, ByRef StrBuf As Short, ByRef Abuf As String, ByRef Ibuf As Short, ByRef Zbuf As String, ByRef mscMES As SerialPort, ByRef istat As Short)
    Dim ier As Short
    Dim Icheck As Boolean
    Dim Ist As Short
    Dim ilen As Short
    Dim Ipos As Short
    Dim Mbuf As String
    Dim imsg As Short
    Static chki As Boolean
    Dim BufLeA As Short
    Dim TimAlt As Date
    Dim TimDiffSec As Short
    Dim BufferCountAlt As Short
    Dim By() As Byte
    'Autom   0=Auslösung der Messung durch Computer
    'Autom   1=Auslösung der Messung durch Meßgerät
    'stv     Eingabepuffer
    'buf     Ausgabepuffer
    'buflen  minimale Bufferlänge, die bei Übernahme der Messwerte erreicht sein muß
    'TimeOut Maximale Wartezeit 1/100 sec)
    'delay   Verzögerung in Timerintervallen (1/100 sec)
    'wart    Wartezeit vor Prüfung des Inputbuffers (1/100 sec)
    'iermeld Textnummer für Fehlermeldung
    'STRBUF  Startposition  (>0) von BUF(=Eingabepuffer) zum Vergleich mit zbuf
    'Abuf    Vergleichsbuffer für Startposition
    'Ibuf    relative Position von BUF(INPUT-Buffer) bei der Vergleich mit Zbuf startet
    'Zbuf    zweiter Vergleichsbuffer
    'mscmes  MScomm-Control
    'tmrmes  Timer
    'ier     Nummer Fehlermeldung (0 oder iermeld
    '
    '
    'Abuf=""       IBUF=0         keine Prüfung
    'Abuf=""       IBUF<>0        Prüfen, ob zbuf=buf(ibuf......)
    'Abuf<>""      IBUF=0         Prüfen, ob abuf in Buf enthalten
    'Abuf<>""      IBUF<>0        Prüfen, ob abuf in Buf enthalten und zbuf=buf(pos(abuf)+ibuf)
    '  "           ibuf>0         von vorne
    '  "           ibuf<0         von hinten
    '
    '
    '
    '
    If Not mscMES.IsOpen Then Exit Sub
    If chki Then Exit Sub
    chki = True
MeasStart:
    '
    'Puffer löschen
    '
    '
    mscMES.DiscardOutBuffer()
    mscMES.DiscardInBuffer()
    BUF = ""
    ier = 0
    TimAlt = Now
    ABRchk = False
    MITchk = False
    TimDiffSec = 0.01 * TimeOut
    '
    'Sende Buffer zum Meßgerät
    '
    '
    If stv <> "" Then
      Try
        mscMES.WriteTimeout = 100 * TimeOut
        mscMES.Write(stv)
      Catch ex As Exception
        MsgBox(ex.Message)
        istat = -1
        ier = 3500
        GoTo MeasError
      End Try
      'Warten
      '
      '
      '
    End If
    Call Wait(wart)

    '
    If Autom = 1 Then
      '
      'Messung wird vom Messgerät ausgelöst
      'Warten bis Auslösung erfolgt
      '
      COMchk = False
      Do
        System.Windows.Forms.Application.DoEvents()
        If COMchk Then Exit Do
        If istat = -1 Or istat = 1 Then
          chki = False
          Exit Sub
        End If
        If Not mscMES.IsOpen Then
          chki = False
          Exit Sub
        End If
        If ABRchk Then
          chki = False
          Exit Sub
        End If
        If MITchk Then
          chki = False
          Exit Sub
        End If
        If DateDiff("s", TimAlt, Now) > TimDiffSec Then
          chki = False
          ier = 3500
          GoTo MeasError
        End If
        Call Wait(delay)
      Loop
      Call Wait(wart)
    End If
    '
    '
    '
    '
    '
    TimAlt = Now
    '
    '
    '
    'Pufferlänge überprüfen
    '
    '
    Do
      System.Windows.Forms.Application.DoEvents()
      If Not mscMES.IsOpen Then
        chki = False
        Exit Sub
      End If
      '
      If istat = -1 Or istat = 1 Then
        chki = False
        Exit Sub
      End If
      '
      If ABRchk Then
        chki = False
        Exit Sub
      End If
      If MITchk Then
        chki = False
        Exit Sub
      End If
      If mscMES.BytesToRead > buflen Then
        BufferCountAlt = mscMES.BytesToRead
        Call Wait(delay)
        If mscMES.BytesToRead = BufferCountAlt Then
          Call Wait(delay)
          Exit Do
        End If
      Else
        Call Wait(delay)
      End If
      If DateDiff("s", TimAlt, Now) > TimDiffSec Then
        chki = False
        ier = 3500
        GoTo MeasError
      End If
    Loop
    '
    '
    '   Uebernahme Daten
    '
    '
    'MsgBox Asc(Mid(mscMES.Input, 1, 1))
    'UPGRADE_WARNING: Couldn't resolve default property of object mscMES.Input. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
    'BUF = ReadString(mscMES)
    BufLeA = mscMES.BytesToRead
    ReDim By(BufLeA - 1)
    mscMES.Read(By, 0, BufLeA)
    BUF = GetString(By)

    '
    '   Eingabepuffer prüfen
    '
    '
    If StrBuf < Len(BUF) And StrBuf <> 0 Then
      Mbuf = Mid(BUF, StrBuf)
    Else
      Mbuf = ""
    End If
    Icheck = False
    If Abuf = "" Then
      Ist = 0
      Icheck = True
    Else
      If Ibuf >= 0 Then
        Ist = KnSTR(Mbuf, Abuf) 'von vorne
        If Ist > 0 Then Icheck = True
      Else
        Ist = JnSTR(Mbuf, Abuf) 'von hinten
        If Ist > 0 Then Icheck = True
      End If
    End If
    If Icheck Then
      If Ibuf = 0 Then
        chki = False
        Exit Sub
      Else
        Ipos = Ist + Ibuf
        ilen = Len(Zbuf)
        If Ipos > 0 And Ipos <= (Len(Mbuf) - ilen) Then
          If KnSTR(Mid(Mbuf, Ipos, ilen), Zbuf) > 0 Then
            chki = False
            Exit Sub
          End If
        End If
      End If
    End If
    If buflen = 0 Then
      chki = False
      Exit Sub
    End If
    ier = ierMeld
    '
    '
    '
    '
    '
MeasError:
    'UPGRADE_WARNING: Couldn't resolve default property of object mscMES.Input. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
    BUF = ReadString(mscMES)
    'imsg = MsgBox(texxt(ier) & Chr(13) & texxt(1999), 4, texxt(2000))
    imsg = MessageOut(MessgKalMess, ier, Texxt(ier) & Chr(13) & Texxt(1999), MessageBoxButtons.YesNo, Texxt(2000), NamLogFile)
    If imsg = 7 Or imsg = 10 Then
      istat = -1
      chki = False
      Exit Sub
    Else
      Call Wait(200)
      GoTo MeasStart

    End If
    chki = False
  End Sub
  Function ReadString(ByVal mscmes As SerialPort) As String
    Dim BY() As Byte
    Dim BufLen As Integer
    BufLen = mscmes.BytesToRead
    ReDim BY(BufLen - 1)
    mscmes.Read(BY, 0, BufLen)
    ReadString = GetString(BY)
  End Function

  Function JnSTR(ByRef BUF As String, ByRef Abuf As String) As Short
    Dim Iii As Short
    Dim Ite As Short
    Dim Ile As Short
    Iii = KnSTR(BUF, Abuf)
    Ile = Len(BUF)
    If Iii <> 0 Then
      Do
        If Iii >= Ile Then
          Exit Do
        End If
        Ite = KnSTR(Mid(BUF, Iii + 1), Abuf)
        'UPGRADE_WARNING: Use of Null/IsNull() detected. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="2EED02CB-5C0E-4DC1-AE94-4FAA3A30F51A"'
        'If Ite = 0 Or Ite Is System.DBNull Then
        If Ite = 0 Then
          Exit Do
        Else
          Iii = Ite + Iii
        End If
      Loop
    End If
    JnSTR = Iii
  End Function
  Private Sub MscMes_DataReceived(ByVal sender As Object, ByVal e As System.IO.Ports.SerialDataReceivedEventArgs) Handles MscMes.DataReceived
    If Not COMchk Then
      COMchk = True
    End If
  End Sub
  Public Sub SplitInputString(ByVal InString As String, ByRef inp() As String, ByVal KO As String)
    inp = InString.Split(KO)
    If inp(UBound(inp)) = "" Then
      ReDim Preserve inp(UBound(inp) - 1)
    End If
  End Sub

  Private Sub MNtxtKEN_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles MNtxtKEN.LostFocus
    MessgKenn = MNtxtKEN.Text
  End Sub

  Private Sub MNtxtProg_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles MNtxtProg.LostFocus
    MessgProg = MNtxtProg.Text
  End Sub

  Private Function ex() As Object
    Throw New NotImplementedException
  End Function

End Module