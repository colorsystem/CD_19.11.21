Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsGRE
  Implements IDisposable

  Private Iauswhl As Short
  Private Kauswhl As Short
  Private istat As Short
  Private iermes As Short
  Dim kw As Short
  Dim j As Short
  Dim i As Short
  Dim nkw As Short
  Dim MnReTr As Short
  Dim byt() As Byte

  Sub einst(ByRef Index As Short)

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



  Sub MesIni()
    Dim i As Integer
    istat = NuWrt
    Call PoOpen(MscMes, iermes)
    If iermes <> 0 Then
      istat = iermes
      Exit Sub
    End If
    Select Case MessgDriver
      Case "GR0"
        Call GR0Mes(10, 0, rmes, istat, MscMes)
      Case "GR1"
        Call GR1Mes(10, 0, rmes, istat, MscMes)
    End Select
    If istat = NuWrt Then
      For i = 0 To MessgNwe - 1
        MessgQW(i) = rmes(i)
      Next i
    End If
    'MsgBox rmes(1)
    Call PoClose(MscMes, iermes)
  End Sub
  Sub MesKal(ByRef imess As Short)
    Dim kw As Integer
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
    'For j = 1 To irmax
    '
    '
    'geräteabhängige Driver
    '
    '
    '
    For kw = 0 To MessgKM - 1
      Select Case MessgDriver
        Case "GR0"
          Call GR0Mes(imess, kw, rmes, istat, MscMes)
        Case "GR1"
          Call GR1Mes(imess, kw, rmes, istat, MscMes)
      End Select
      If istat = 1 Or istat = -1 Then GoTo KalEnd
      Call KalSum(kw, rsmes, rmes)

    Next kw
    jj = jj + 1
    If istat = 0 Then GoTo KalEnd
    'Next j
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
        Select Case MessgDriver
          Case "GR0"
            Call GR0Mes(0, kw, rmes, istat, MscMes)
          Case "GR1"
            Call GR1Mes(0, kw, rmes, istat, MscMes)
        End Select
        If istat <> NuWrt Then Exit For
        Call NormRwrt(MnReTr, kw, rmes)
        MNbtnMIT.Enabled = False
        MNbtnLOE.Enabled = False
        System.Windows.Forms.Application.DoEvents()

      Next kw
      '
      If MITchk Then Exit Do
      '
      If istat <> NuWrt Then Exit Do
      '
      '
      '     Speichern
      '
      '
      Call RwrSpei(istat, MesZae)
      If istat <> NuWrt Then Exit Do
      If MesZae > 1 Then
        MNbtnLOE.Enabled = True
      End If
      MNbtnMIT.Enabled = True
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
  Sub MesSon()

  End Sub

  Sub MesSpei()

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

    If NormfileID <> 1 And NormfileID <> 12 Then
      MsgBox(Texxt(3519))
      istat = -1
      Exit Sub
    End If
    idka(0) = 1
    idka(1) = 0
    idka(2) = 0
  End Sub
  Function RwGret(ByRef Text As String) As Single
    Dim Dreh(3) As Byte

    Dreh(0) = CByte("&H" & Text.Substring(0, 2))
    Dreh(1) = CByte("&H" & Text.Substring(2, 2))
    Dreh(2) = CByte("&H" & Text.Substring(4, 2))
    Dreh(3) = CByte("&H" & Text.Substring(6, 2))

    RwGret = GetSingle(Dreh)
  End Function



  Function Autom() As Short
    Autom = 1
  End Function
  '


  '

  '
  Sub GR0Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim i As Short
    Dim j As Integer
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim CrLf As String
    Dim Izero As Short
    Dim resp As Short
    '
    CrLf = Chr(13) & Chr(10)
    buflen = 0
    iver = 5
    wart = 10
    delay = 10
    resp = 2
    COMchk = True
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        12   380-730,10    NWE=36
    '
    CHRM = MessgChrm(kw)
    '       MsgBox CHRM & CStr(messg_ihrm(kw)), 0
    '
    '      STATUS ABFRAGEN
    '
    '       Stv = "S   ****:" & Chr$(13) & Chr$(10)
    '       Call
    'cntr(Stv, BUF, 10, 5, 5, 10, ier)
    '       If ier <> 0 Then Exit Sub
    '
    '
    If imess = 1 Or imess = 10 Then
      If kw = 0 Then

        '      Meldung für Display Meßgerät
        '
        '
        'MsgBox texxt(3524), 0
        Izero = 0
        '     Baud-rate (9600)
        '
        '
        '
        stv = WakeUp() & ";00" & CrLf

        Call GetComBuf(resp, stv, BUF, 12, iver, delay, 30, 3500, 1, ":0B", 0, "", mscMES, istat)
        'Call GetComBuf(resp, stv, BUF, 0, iver, delay, 30, _
        ''3500, 1, "", 0, "", mscMES , istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '      Reset
        '
        '
        '
        '
        stv = ";4A" & CrLf
        Call GetComBuf(resp, stv, BUF, 7, iver, delay, 30, 3500, 1, ":1F0000", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub

        '
        '
        '
        '      Handshake ON
        '
        '
        '
        '
        stv = ";4D1E" & CrLf
        Call GetComBuf(resp, stv, BUF, 7, iver, delay, 30, 3500, 1, ":1F0000", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub

        '
        '      Set Measurement type (remission)
        '
        '
        '
        stv = ";4D9B" & CrLf
        Call GetComBuf(resp, stv, BUF, 7, iver, delay, 30, 3500, 1, ":1F0000", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        'MsgBox BUF
        '
        '
        '
        'stv = ";4D54" & CrLf
        'Call GetComBuf(resp, stv, BUF, 7, iver, delay, 30, _
        ''3500, 1, ":1F0000", 0, "", mscmes , istat)
        'If istat <> NuWrt Then Exit Sub
        'MsgBox BUF
        ''      Weißkalibrierung
        If imess = 1 Then
          '
          For i = 1 To 2
            Beep()
          Next i

          '      Perform white measurement
          '
          stv = ";220907" & CrLf
          Call GetComBuf(1000, stv, BUF, 6, 1, delay, 30, 3500, 1, ":2513", 0, "", mscMES, istat)
          'If Mid(BUF, 1, 5) = ":2513" Then Exit Do
          If istat <> NuWrt Then Exit Sub

          'If istat <> NuWrt Then Exit Sub
          '
          '      Warten bis Messung ausgeführt
          '
        End If
        '
        stv = ";B301" & CrLf
        Call GetComBuf(resp, stv, BUF, 300, iver, delay, 50, 3500, 1, "", 1, ":B4", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call GR0rwert(1, BUF.Substring(5), r, istat)
        Exit Sub
      End If
    ElseIf imess = 2 Then
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 0 Then
      '
      If kw = 0 Then
        '
        '      Messung
        '      Prüfen, ob Messung vorhanden
        '
        Do
          stv = ";07" & CrLf
          Call GetComBuf(1, stv, BUF, 4, 3, 5, 4 * wart, 3500, 1, "", 0, "", mscMES, istat)
          'MsgBox BUF
          If BUF <> "" Then
            If BUF.Substring(0, 9) = ":120109" & CrLf Then
              stv = ";B80900" & CrLf
              Call GetComBuf(0, stv, BUF, 300, 2, delay, 50, 3500, 1, ":B9", 0, "", mscMES, istat)
            Else
              Exit Do
            End If
          End If
          Call Wait(30)
          If MITchk Then Exit Sub
          If istat <> NuWrt Then Exit Sub
        Loop


        stv = ";B80900" & CrLf
        Call GetComBuf(0, stv, BUF, 0, 2000, delay, 30, 3500, 1, "", 0, "", mscMES, istat)
        '

        '
        '
        '
        For j = 0 To 2
          Beep()
        Next j
        Do
          Call Wait(50)
          stv = ";07" & CrLf
          Call GetComBuf(1, stv, BUF, 4, 1, 3 * delay, wart, 3500, 1, "", 0, "", mscMES, istat)
          If MITchk Then Exit Sub
          If BUF <> "" Then
            If BUF.Substring(0, 7) = ":120109" Then
              Exit Do
            End If
          End If
          'If MITchk Then Exit Sub
          If istat <> NuWrt Then Exit Sub
        Loop


        '
        '
        '      Warten bis Messung ausgeführt
        '
        '
        stv = ";B80900" & CrLf
        Call GetComBuf(1, stv, BUF, 300, 1, delay, 50, 3500, 1, ":B9", 0, "", mscMES, istat)
        'MsgBox BUF
        If istat <> NuWrt Then Exit Sub
        Call GR0rwert(1, BUF.Substring(7), r, istat)
        If istat <> NuWrt Then Exit Sub
        Exit Sub
      End If
      '
    End If
  End Sub
  Sub GR1Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    Dim CrLf As String
    Dim Izero As Short
    '
    CrLf = Chr(13) & Chr(10)
    buflen = 0
    iver = 5
    wart = 10
    delay = 2
    resp = 2
    COMchk = True
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        12   380-730,10    NWE=36
    '
    CHRM = MessgChrm(kw)
    '       MsgBox CHRM & CStr(messg_ihrm(kw)), 0
    '
    '      STATUS ABFRAGEN
    '
    '       Stv = "S   ****:" & Chr$(13) & Chr$(10)
    '       Call
    'cntr(Stv, BUF, 10, 5, 5, 10, ier)
    '       If ier <> 0 Then Exit Sub
    '
    '
    If imess = 1 Or imess = 10 Then
      If kw = 0 Then

        '      Meldung für Display Meßgerät
        '
        '
        'MsgBox texxt(3524), 0
        Izero = 0
        '     Baud-rate (9600)
        '
        '
        '
        stv = WakeUp() & ";00" & CrLf
        Call GetComBuf(resp, stv, BUF, 12, iver, delay, 50, 3500, 1, ":0B", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        'MsgBox BUF
        If istat <> NuWrt Then Exit Sub
        '
        '
        '      Reset
        '
        '
        '
        '
        stv = WakeUp() & ";4A" & CrLf
        'Call GetComBuf(resp, stv, BUF, 7, iver, delay, 50, _
        ''3500, 1, ":1F0000", 0, "", mscmes , istat)
        Call GetComBuf(resp, stv, BUF, 7, iver, delay, 50, 3500, 1, ":2500", 0, "", mscMES, istat)

        If istat <> NuWrt Then Exit Sub

        '
        '
        '
        '      Handshake ON
        '
        '
        '
        '
        stv = WakeUp() & ";4D1E" & CrLf
        Call GetComBuf(resp, stv, BUF, 7, iver, delay, 50, 3500, 1, ":1F0000", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub

        '
        '      Set Measurement type (remission)
        '
        '
        '
        'stv = WakeUp() & ";4D9B" & CrLf
        'Call GetComBuf(resp, stv, BUF, 7, iver, delay, 30, _
        ''3500, 1, ":1F0000", 0, "", mscmes , istat)
        'If istat <> NuWrt Then Exit Sub
        'MsgBox BUF
        '
        '
        '
        'stv = ";4D54" & CrLf
        'Call GetComBuf(resp, stv, BUF, 7, iver, delay, 30, _
        ''3500, 1, ":1F0000", 0, "", mscmes , istat)
        'If istat <> NuWrt Then Exit Sub
        'MsgBox BUF
        ''      Weißkalibrierung
        If imess = 1 Then
          '
          For i = 1 To 2
            Beep()
          Next i

          '      Perform white measurement
          '
          stv = WakeUp() & ";220907" & CrLf
          Call GetComBuf(1, stv, BUF, 6, 1, delay, 200, 3500, 1, ":2513", 0, "", mscMES, istat)
          If istat <> NuWrt Then Exit Sub
          '
          '      Warten bis Messung ausgeführt
          '
        End If
        For i = 0 To MessgNwe - 1
          r(i) = 1.0#
        Next i

        '
        'stv = WakeUp() & ";B301" & CrLf
        'Call GetComBuf(resp, stv, BUF, 300, iver, delay, 100, _
        ''3500, 1, "", 1, ":B4", mscmes , istat)
        'If istat <> NuWrt Then Exit Sub
        'Call GR0rwert(1, Mid(BUF, 6), r, istat)
        'If istat <> NuWrt Then Exit Sub
        Exit Sub
      End If
    ElseIf imess = 2 Then
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 0 Then
      '
      If kw = 0 Then
        '
        '      Messung
        '      Prüfen, ob Messung vorhanden
        '

        'stv = ";B80900" & CrLf
        'Call GetComBuf(resp, stv, BUF, 0, 1, delay, 30, _
        ''3500, 1, "", 0, "", mscmes , istat)
        '
        '
        '
        '
        For i = 1 To 2
          Beep()
        Next i

        stv = WakeUp() & ";20" & CrLf
        Call GetComBuf(1, stv, BUF, 6, 1, delay, 150, 3500, 1, ":2500", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub

        'stv = WakeUp() & ";050900" & CrLf
        'Call GetComBuf(1, stv, BUF, 6, 1, delay, 200, _
        ''3500, 1, ":2500", 0, "", mscmes , istat)
        'If istat <> NuWrt Then Exit Sub
        'MsgBox Len(BUF)
        'Do
        'MsgBox mscmes.Input
        'Loop
        '
        '
        '      Warten bis Messung ausgeführt
        '
        Call Wait(20)
        Do
          stv = WakeUp() & ";050900" & CrLf
          Call GetComBuf(1, stv, BUF, 300, 1, 5, wart, 3500, 1, "", 0, "", mscMES, istat)
          'MsgBox BUF
          If Mid(BUF, 1, 7) = ":100900" Then
            Exit Do
          End If
          Call Wait(100)
          System.Windows.Forms.Application.DoEvents()
          If istat <> NuWrt Then Exit Sub
        Loop

        '
        'stv = ";B80900" & CrLf
        'Call GetComBuf(1, stv, BUF, 300, 1, delay, 50, _
        ''3500, 1, ":B9", 0, "", mscmes , istat)
        ''MsgBox BUF
        If istat <> NuWrt Then Exit Sub
        Call GR1rwert(1, Mid(BUF, 8), r, istat)
        If istat <> NuWrt Then Exit Sub
        Exit Sub
      End If
      '
    End If
  End Sub


  Sub GR0rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef ier As Short)
    Dim i As Short
    Dim rr(35) As Single
    '
    'Statuszeile entfernen
    '
    'MsgBox BUF
    If BUF.Length < 300 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If

    '
    For i = 0 To 35
      rr(i) = RwGret(BUF.Substring(8 * i, 8))
    Next i
    If MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = rr(2 * (i + 1))
      Next i
    Else
      For i = 0 To MessgNwe - 1
        r(i) = rr(i)
      Next i
    End If
    '
    '
  End Sub
  Sub GR1rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef ier As Short)
    Dim i As Short
    Dim rr(35) As Single
    '
    'Statuszeile entfernen
    '
    'MsgBox BUF
    If Len(BUF) < 296 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If

    '
    For i = 0 To 35
      rr(i) = RwGret(BUF.Substring(i * 8, 8))
    Next i
    If MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = rr(2 * i + 2)
      Next i
    Else
      For i = 0 To MessgNwe - 1
        r(i) = rr(i)
      Next i
    End If
    '
    '
  End Sub












  Function WakeUp() As String
    Dim i As Short
    WakeUp = ""
    For i = 0 To 255
      WakeUp = WakeUp & Chr(255)
    Next i
  End Function

  
  Sub New()
    MyBase.New()
    istat = 0
    MnReTr = 0
  End Sub
  Sub dispose() Implements IDisposable.Dispose
  End Sub
End Class