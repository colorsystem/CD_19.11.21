Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsMIL
  Implements IDisposable

  '
  '
  'Milton Roy Color Graph
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
  Sub MesSon()

  End Sub


  Sub MILini(ByRef istat As Short)

  End Sub

  Sub SetIdka(ByRef MessgMenue As Short)
    '1  Weißstandard
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
    idka(0) = 1
    idka(1) = 0
    idka(2) = 0
    If MessgDriver = "MIL" Then
      If NormfileID <> 2 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
      idka(0) = 1
      idka(1) = 0
      idka(2) = 0
    Else
      MsgBox(Texxt(3542) & MessgDriver)
    End If
  End Sub
  Sub MesSpei()

  End Sub

  Sub MesMes()
    Dim kw As Integer
    istat = NuWrt
    WinChk = False
    NXTChk = False
    COMchk = False
    MITchk = False
    MNbtnNXT.Visible = False
    NXTChk = True
    Call PoOpen(MscMes, iermes)
    If iermes <> 0 Then
      Call PoClose(MscMes, iermes)
      istat = -1
      Exit Sub
    End If
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
      For kw = 0 To MessgKM - 1
        If MessgDriver = "MIL" Then
          Call MILMes(0, kw, rmes, istat, MscMes)
        Else
          MsgBox(Texxt(3542) & MessgDriver)
        End If
        If istat <> NuWrt Then Exit For
        Call NormRwrt(MnReTr, kw, rmes)
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
    Call PoClose(MscMes, iermes)
  End Sub
  Property IchStat() As Short
    Get
      IchStat = istat
    End Get
    Set(ByVal Value As Short)
      istat = Value
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



  Sub MILrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim i As Short
    Dim rr(30) As Single
    '
    'Statuszeile entfernen
    '
    '       MsgBox BUF
    'MsgBox Len(BUF)
    '
    If Len(BUF) < 435 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    'MsgBox Len(BUF)

    '
    '       400-700nm (10nm)
    '
    For i = 0 To 30
      'MsgBox Mid(BUF, 6 + (i - 1) * 14, 7)
      rr(i) = Val(Mid(BUF, 6 + i * 14, 7))
      'MsgBox rr(i)
    Next i
    '
    '       400-700nm (20nm)
    '

    If MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = rr(2 * i + 1)
      Next i
    Else
      For i = 0 To MessgNwe - 1
        r(i) = rr(i)
      Next i
    End If
    '
    '
  End Sub



  Sub MILMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    Dim IBASF As Short
    CrLf = Chr(13)
    buflen = 4
    iver = 5
    wart = 50
    delay = 1
    resp = 1
    CHRM = MessgChrm(kw)

    If kw = 0 And imess = 1 Then
      '
      '
      '
      'Tastatur locken
      stv = "LOCK" & CrLf
      Call GetComBuf(resp, stv, BUF, 2, 1, delay, wart, 3506, 1, "", 1, "LOCK", mscMES, istat)
      If istat <> NuWrt Then Exit Sub

      'Parameter festlegen
      '
      '
      '
      '
      'Standardsettings
      '
      '
      stv = "RE" & CrLf
      Call GetComBuf(resp, stv, BUF, 2, 1, delay, wart, 3506, 1, "", 1, "RE", mscMES, istat)
      If istat <> NuWrt Then Exit Sub

      '
      'Schrittweite (nm) ändern


      stv = "WI=10" & CrLf
      Call GetComBuf(resp, stv, BUF, 2, 1, delay, wart, 3506, 1, "", 1, "WI", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      '
      '
      If Mid(LTrim(CHRM), 2, 1) = "O" Or Mid(LTrim(CHRM), 2, 1) = "0" Then
        'Exclusive Glanz
        stv = "SC=0" & CrLf
      Else
        'Inclusive Glanz
        stv = "SC=1" & CrLf
      End If

      Call GetComBuf(resp, stv, BUF, 2, 1, delay, wart, 3506, 1, "", 1, "SC", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      '
      'stv = "CF" & CrLf
      'Call GetComBuf(resp, stv, BUF, 4, 1, delay, wart, _
      ''3506, 1, "", 1, "?", mscmes , istat)
      'MsgBox BUF
      'If istat <> NuWrt Then Exit Sub

      '
      stv = "IL=0" & CrLf
      Call GetComBuf(resp, stv, BUF, 2, 1, delay, wart, 3506, 1, "", 1, "IL", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      stv = "OB=0" & CrLf
      Call GetComBuf(resp, stv, BUF, 2, 1, delay, wart, 3506, 1, "", 1, "OB", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      stv = "DF=0" & CrLf
      Call GetComBuf(resp, stv, BUF, 2, 1, delay, wart, 3506, 1, "", 1, "DF", mscMES, istat)
      If istat <> NuWrt Then Exit Sub


      'stv = "CF" & CrLf
      'Call GetComBuf(resp, stv, BUF, 4, 1, delay, wart, _
      ''3506, 1, "", 1, "?", mscmes , istat)
      'MsgBox BUF
      'If istat <> NuWrt Then Exit Sub
      '

      '
      'Lampe CALIBATE an
      stv = "LEDS=1" & CrLf
      Call GetComBuf(resp, stv, BUF, 4, 1, delay, wart, 3506, 1, "", 1, "LEDS", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      '
      '
      '       Weißkalibrierung
      '
      '
      stv = "CA" & CrLf
      Call GetComBuf(resp, stv, BUF, 20, 1, delay, 1000, 3506, 1, "CALIBRATION COMPLETE", 0, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      '
      '       Alle Lampen aus
      '
      stv = "LEDS=0" & CrLf
      Call GetComBuf(resp, stv, BUF, 4, 1, delay, wart, 3506, 1, "", 1, "LEDS", mscMES, istat)
      If istat <> NuWrt Then Exit Sub

    End If
    '
    'Normale Messung ausführen
    '
    'Lampe STANDARD an
    stv = "LEDS=4" & CrLf
    Call GetComBuf(resp, stv, BUF, 4, 1, delay, wart, 3506, 1, "", 1, "LEDS", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    '
    '
    '       Messung Standard übernehmen
    stv = "ST" & CrLf
    Call GetComBuf(resp, stv, BUF, 400, 1, delay, 1000, 3506, 1, "ST", 0, "", mscMES, istat)
    'MsgBox Len(BUF)
    'MsgBox BUF

    If istat <> NuWrt Then Exit Sub
    '

    'MsgBox Mid(BUF, 5)
    Call MILrwert(IBASF, Mid(BUF, 5), r, istat)
    If istat <> NuWrt Then Exit Sub

    '
    '       Alle Lampen aus
    '
    stv = "LEDS=0" & CrLf
    Call GetComBuf(resp, stv, BUF, 4, 1, delay, wart, 3506, 1, "", 1, "LEDS", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    '
  End Sub


  Sub MesIni()
    istat = NuWrt
    If MessgDriver = "MIL" Then
      Call MILini(istat)
    Else
      MsgBox(Texxt(3542) & MessgDriver)
    End If
  End Sub

  Sub MesKal(ByRef imess As Short)
    Dim jj As Short
    Dim j As Integer
    Dim kw As Integer
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
      'geräteabhängige Driver
      '
      '
      '
      For kw = 0 To MessgKM - 1
        If MessgDriver = "MIL" Then
          Call MILMes(imess, kw, rmes, istat, MscMes)
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