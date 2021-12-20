Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsHUN
  Implements IDisposable

  '
  'HU1 Ultrascan
  'HU2 LabScabn XE
  'HU3 UltraScan XE
  '

  'istat = 0   kein Fehler
  'istat > 0   Fehler ist aufgetreten Programm neu starten
  'istat < 0   Fehler ist aufgetreten Programm nicht neu starten
  Private istat As Short
  Private iermes As Short
  Dim kw As Short
  Dim j As Short
  Dim i As Short
  Dim nkw As Short
  Dim MnReTr As Short
  Dim MnLabScanXE As AxLabScanXE.AxLSXE
  Dim MnUltScanXE As AxUltraScanXE.AxUSXE
  Dim MncmdMesWe As System.Windows.Forms.Button
  Dim MncmdMesSc As System.Windows.Forms.Button
  '
  '
  '
  Sub LabXEMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim i As Short
    LabScanXE.ReadSample(r)
    If Err.Number <> 0 Then GoTo MesLSXEerr
    For i = 1 To MessgNwe
      r(i) = 0.01 * r(i)
    Next i
    Exit Sub
MesLSXEerr:
    istat = -1

  End Sub
  Sub UltXEMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim i As Short
    UltScanXE.ReadSample(r)
    If Err.Number <> 0 Then GoTo MesUSXEerr
    For i = 1 To MessgNwe
      r(i) = 0.01 * r(i)
    Next i
    Exit Sub
MesUSXEerr:
    istat = -1

  End Sub


  Sub MesSon()

  End Sub


  Sub UltIni(ByRef istat As Short)

  End Sub
  Sub MesSpei()

  End Sub

  Sub SetIdka(ByRef MessgMenue As Short)
    '1  Weiﬂstandard
    '2  Schwarzstandard
    '3  Graustandard
    '4  Weissstandard (Transmission)
    '5  Schwarzstandard (Transmission)
    istat = 0
    If Reftra <> "R" Then
      MsgBox(Texxt(3590) & "  (" & Reftra & ")")
      istat = -1
      Exit Sub
    End If
    If MessgDriver = "HU1" Then
      If NormfileID <> 15 And NormfileID <> 1 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
      idka(0) = 2
      idka(1) = 1
      idka(2) = 3
    ElseIf MessgDriver = "HU2" Then
      idka(0) = 1
      idka(1) = 0
      idka(2) = 0
    ElseIf MessgDriver = "HU3" Then
      idka(0) = 1
      idka(1) = 0
      idka(2) = 0
    Else
      MsgBox(Texxt(3542) & MessgDriver)
    End If
  End Sub
  '
  Sub MesMes()
    Dim irt As Short
    istat = NuWrt
    WinChk = False
    NXTChk = False
    COMchk = False
    MITchk = False
    MNbtnNXT.Visible = False
    NXTChk = True
    Select Case MessgDriver
      Case "HU1"
        Call PoOpen(MscMes, iermes)
        If iermes <> 0 Then
          Call PoClose(MscMes, iermes)
          istat = -1
          Exit Sub
        End If
      Case "HU2"
        LabScanXE.CommPort = MessgCom
        If Err.Number <> 0 Then
          GoTo MesLSXEerr
        End If
        LabScanXE.BaudRate = MessgBaud
        If Err.Number <> 0 Then
          GoTo MesLSXEerr
        End If
        LabScanXE.OpenCommPort()
        If Err.Number <> 0 Then
          GoTo MesLSXEerr
        End If
        LabScanXE.SetTimeReference()
        If Err.Number <> 0 Then
          GoTo MesLSXEerr
        End If
      Case "HU3"
        UltScanXE.CommPort = MessgCom
        If Err.Number <> 0 Then
          GoTo MesLSXEerr
        End If
        UltScanXE.BaudRate = MessgBaud
        If Err.Number <> 0 Then
          GoTo MesLSXEerr
        End If
        UltScanXE.OpenCommPort()
        If Err.Number <> 0 Then
          GoTo MesLSXEerr
        End If
        'MsgBox UltScanXE.SpexDoorPosition
        UltScanXE.SetTimeReference()
        If Err.Number <> 0 Then
          GoTo MesLSXEerr
        End If
    End Select
    '
    '
    'Ausnullen
    '
    MesZae = 0
    Mesmax = 1
    If MessgImes > 1 Then
      Mesmax = MessgImes
    End If
    Alph = 1.0#
    Ipm = Ipmax
    Call NullRwrt(Mnier)
    '
    If MessgImes > 1 Then
      Mesmax = MessgImes
    End If
    '
    '
    MNbtnNXT.Visible = True
    MNbtnNXT.Enabled = False
    MNbtnSTR.Visible = True
    MNbtnABR.Visible = True
    MNbtnMIT.Visible = True
    MNbtnSTR.Enabled = True
    MNbtnABR.Enabled = True
    MNbtnLOE.Visible = True
    MNbtnLOE.Enabled = False
    MNbtnMIT.Enabled = False
    MNlblANZ.Visible = True
    '
    '
    Do While MesZae < Mesmax
      NXTChk = False
      MNbtnNXT.Enabled = False
      MNlblANZ.Text = LblAnze(MesZae + 1, Mesmax)
      MesZae = MesZae + 1
      '
      '
      '
      '
      '  Messung starten
      '
      '
      '
      '
      For kw = 1 To MessgKM
        Select Case MessgDriver
          Case "HU1"
            Call UltHunMes(0, kw, rmes, istat, MscMes)
            irt = MnReTr
          Case "HU2"
            Call LabXEMes(0, kw, rmes, istat, MscMes)
            irt = -1
          Case "HU3"
            Call UltXEMes(0, kw, rmes, istat, MscMes)
            irt = -1
        End Select
        If istat <> NuWrt Then Exit For
        Call NormRwrt(irt, kw, rmes)
        MNbtnMIT.Enabled = False
        MNbtnLOE.Enabled = False
      Next kw
      '
      '
      If istat <> NuWrt Then Exit Do
      '
      '
      '     Speichern
      '
      '
      Call RwrSpei(istat, MesZae)
      If istat <> NuWrt Then Exit Do
      If MesZae = Mesmax Then Exit Do
      If MesZae > 1 Then
        MNbtnLOE.Enabled = True
      End If
      MNbtnMIT.Enabled = True
      MNbtnNXT.Enabled = True
      MNbtnNXT.Focus()
      Do

        System.Windows.Forms.Application.DoEvents()
        If NXTChk Then
          Exit Do
        End If
      Loop
      If MITchk Then
        Exit Do
      End If
      If istat <> NuWrt Then Exit Do

      '
      '
    Loop
    If istat = NuWrt Then istat = 0
    If istat = 0 Then
      Call RwrFin(MnReTr, istat)
      Call RwrtSpei(istat)
    End If
    Select Case MessgDriver
      Case "HU1"
        Call PoClose(MscMes, iermes)
      Case "HU2"
        LabScanXE.CloseCommPort()
      Case "HU3"
        UltScanXE.CloseCommPort()
    End Select
    Exit Sub
MesLSXEerr:
    istat = -1
    MsgBox(Err.Description)

  End Sub
  Property IchStat() As Short
    Get
      IchStat = istat
    End Get
    Set(ByVal Value As Short)
      istat = Value
    End Set
  End Property

  Property LabScanXE() As AxLabScanXE.AxLSXE
    Get
      LabScanXE = MnLabScanXE
    End Get
    Set(ByVal Value As AxLabScanXE.AxLSXE)
      MnLabScanXE = Value
    End Set
  End Property
  Property UltScanXE() As AxUltraScanXE.AxUSXE
    Get
      UltScanXE = MnUltScanXE
    End Get
    Set(ByVal Value As AxUltraScanXE.AxUSXE)
      MnUltScanXE = Value
    End Set
  End Property

  Property cmdMesWe() As System.Windows.Forms.Button
    Get
      cmdMesWe = MncmdMesWe
    End Get
    Set(ByVal Value As System.Windows.Forms.Button)
      MncmdMesWe = Value
    End Set
  End Property
  Property cmdMesSc() As System.Windows.Forms.Button
    Get
      cmdMesSc = MncmdMesSc
    End Get
    Set(ByVal Value As System.Windows.Forms.Button)
      MncmdMesSc = Value
    End Set
  End Property
  Property ReTr() As Short
    Get
      ReTr = MnReTr
    End Get
    Set(ByVal Value As Short)
      MnReTr = Value
    End Set
  End Property

  Function Autom() As Short
    Autom = 0
  End Function

  Sub einst(ByRef Index As Short)
    '
    '
  End Sub



  Sub UltHunRwert(ByRef IBASF As Short, ByRef r() As Single, ByRef Ri() As Single, ByRef Rh() As Single, ByRef Rg() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim i As Short
    Dim j As Short
    Dim l As Short
    Dim k As Short
    Dim sav(5) As Single
    Dim SRi As Single
    Dim SRh As Single
    Dim SRg As Single
    Dim tex As String
    sav(1) = -0.0857143
    sav(2) = 0.342857
    sav(3) = 0.485714
    sav(4) = 0.342857
    sav(5) = -0.0857143
    '
    '
    '       400-700nm (20nm)   messgnwe=16
    '
    If NormfileID = 1 Then
      '
      i = 3
      For j = 1 To MessgNwe
        SRi = 0
        SRh = 0
        SRg = 0
        For l = 1 To 5
          SRi = SRi + sav(l) * Ri(i + l)
          SRh = SRh + sav(l) * Rh(i + l)
          SRg = SRg + sav(l) * Rg(i + l)
        Next l
        r(j) = (SRi - SRg) / (SRh - SRg)
        'MsgBox r(J)
        i = i + 5
      Next j
      '
      '
      '       375-750 (5) nm   messgnwe=76
    Else
      '
      For j = 1 To MessgNwe
        r(j) = (Ri(j) - Rh(j)) / (Rg(j) - Rh(j))
        'MsgBox r(J)
      Next j

    End If
  End Sub
  Sub UltHunMwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    Dim j As Short
    '
    'Statuszeile entfernen
    '
    '
    If Len(BUF) < 314 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    ' MsgBox Len(BUF)
    '
    '       375-750 (5) nm
    '
    'MsgBox BUF
    '
    For j = 1 To 76
      'MsgBox Mid(BUF, 7 + (J - 1) * 4, 4)
      r(j) = CSng("&H" & Mid(BUF, 7 + (j - 1) * 4, 4))
    Next j
    'MsgBox r(1) & "  " & r(75)
    '
  End Sub






  Sub UltHunMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim ive As Short
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    Dim sav(5) As Single
    '
    Dim IBASF As Short
    Dim Ri(76) As Single
    Dim Rh(76) As Single
    Dim Rg(76) As Single

    buflen = 300
    iver = 5
    wart = 50
    delay = 50
    resp = 2
    '     Null-Kalibrieren nicht unterst¸tzt
    '
    '
    '
    '      If imess = 3 Or imess = 4 Or imess = 7 Then
    If imess > 4 Then
      Exit Sub
    End If
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '     Beispiel
    '
    '     NB=15   375-750,5     nwe=76
    '
    IBASF = 0
    '
    CHRM = MessgChrm(kw)
    '      MsgBox CHRM & CStr(messg_ihrm(kw)), 0

    '
    stv = StvChk(Chr(1) & "001#I")
    '
    Call GetComBuf(resp, stv, BUF, buflen, iver, delay, wart, 3500, 1, "", 1, "????#", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    Call UltHunMwert(IBASF, BUF, Ri, istat)
    '
    '
    stv = StvChk(Chr(1) & "001#H")
    '
    Call GetComBuf(resp, stv, BUF, buflen, iver, delay, wart, 3500, 1, "", 1, "????#", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    Call UltHunMwert(IBASF, BUF, Rh, istat)
    '
    '
    stv = StvChk(Chr(1) & "001#G")
    '
    Call GetComBuf(resp, stv, BUF, buflen, iver, delay, wart, 3500, 1, "", 1, "????#", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    Call UltHunMwert(IBASF, BUF, Rg, istat)
    '
    'R-Werte berechnen
    '
    '
    Call UltHunRwert(IBASF, r, Ri, Rh, Rg, istat)
  End Sub

  Function StvChk(ByRef stv As String) As String
    Dim STW As String
    Dim ii As Short
    Dim i As Short
    '
    '     Checksum
    '
    '
    ii = 0
    For i = 1 To 6
      ii = ii + Asc(Mid(stv, i, 1))
    Next i
    STW = Trim(Hex(ii))
    StvChk = stv & Sline(4 - Len(STW), "0") & STW
    '

  End Function

  Sub MesIni()
    istat = NuWrt
    If MessgDriver = "HU1" Then
      Call UltIni(istat)
    ElseIf MessgDriver = "HU2" Then
    ElseIf MessgDriver = "HU3" Then
    Else
      MsgBox(Texxt(3542) & MessgDriver)
    End If
  End Sub

  Sub MesKal(ByRef imess As Short)
    Dim i As Short
    Select Case MessgDriver
      Case "HU1"
        Call UltKal(imess)
      Case "HU2"
        If imess = 1 Then
          LabScanXE.CommPort = MessgCom
          If Err.Number <> 0 Then GoTo KalLSXEerr
          LabScanXE.BaudRate = MessgBaud
          If Err.Number <> 0 Then GoTo KalLSXEerr
          LabScanXE.OpenCommPort()
          If Err.Number <> 0 Then GoTo KalLSXEerr
          LabScanXE.SetTimeReference()
          If Err.Number <> 0 Then GoTo KalLSXEerr
          LabScanXE.Standardize()
          If Err.Number <> 0 Then GoTo KalLSXEerr
          LabScanXE.CloseCommPort()
          For i = 1 To MessgNwe
            Rhilf(i) = 1.0#
            rsmes(i) = 1.0#
          Next i
          istat = 0
        ElseIf imess = 2 Then
          For i = 1 To MessgNwe
            Rhilf(i) = 0.0#
            rsmes(i) = 0.0#
          Next i
          istat = 0
        End If
      Case "HU3"
        If imess = 1 Then
          UltScanXE.CommPort = MessgCom
          If Err.Number <> 0 Then GoTo KalLSXEerr
          UltScanXE.BaudRate = MessgBaud
          If Err.Number <> 0 Then GoTo KalLSXEerr
          UltScanXE.OpenCommPort()
          If Err.Number <> 0 Then GoTo KalLSXEerr
          UltScanXE.SetTimeReference()
          If Err.Number <> 0 Then GoTo KalLSXEerr
          UltScanXE.Standardize()
          If Err.Number <> 0 Then GoTo KalLSXEerr
          UltScanXE.CloseCommPort()
          For i = 1 To MessgNwe
            Rhilf(i) = 1.0#
            rsmes(i) = 1.0#
          Next i
          istat = 0
        ElseIf imess = 2 Then
          For i = 1 To MessgNwe
            Rhilf(i) = 0.0#
            rsmes(i) = 0.0#
          Next i
          istat = 0
        End If
    End Select
    Exit Sub
KalLSXEerr:
    istat = -1
    MsgBox(Err.Description)
  End Sub
  Sub UltKal(ByRef imess As Short)
    Dim jj As Short
    istat = NuWrt
    WinChk = False
    NXTChk = False
    COMchk = False
    MNbtnABR.Visible = True
    MNbtnABR.Enabled = True
    Call PoOpen(MscMes, iermes)
    Call NullRwrt(Mnier)
    jj = 0
    For j = 1 To irmax
      '
      '
      'ger‰teabh‰ngige Driver
      '
      '
      '
      For kw = 1 To MessgKM
        If MessgDriver = "HU1" Then
          Call UltHunMes(imess, kw, rmes, istat, MscMes)
        Else
          MsgBox(Texxt(3542) & MessgDriver)
        End If
        If istat = 1 Or istat = -1 Then GoTo KalEnd
        Call KalSum(kw, rsmes, rmes)
      Next kw
      jj = jj + 1
      If istat = 0 Then GoTo KalEnd
    Next j
    istat = 0
KalEnd:
    If istat = 0 Then
      Call KalMit(jj, rsmes)
      Call RwrRhilf(rsmes, Rhilf)
      '
    End If
    '
    '
    '
    '
    '
    '

    Call PoClose(MscMes, iermes)
  End Sub




  'UPGRADE_NOTE: Class_Initialize was upgraded to Class_Initialize_Renamed. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="A9E4979A-37FA-4718-9994-97DD76ED70A7"'
  Private Sub Class_Initialize_Renamed()
    istat = 0
    MnReTr = 0

  End Sub
  Sub New()
    MyBase.New()
    Class_Initialize_Renamed()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
  End Sub
End Class