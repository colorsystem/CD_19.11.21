Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsSPEAlt
  Implements IDisposable

  Private istat As Short
  Private iermes As Short
  Dim kw As Short
  Dim j As Short
  Dim i As Short
  Dim nkw As Short
  Dim resp As Short
  Dim MnReTr As Short
  Sub MesSon()

  End Sub
  Sub MesSpei()

  End Sub

  Sub SetIdka(ByRef MessgMenue As Short)
    '1  Wei?standard
    '2  Schwarzstandard
    '3  Graustandard
    '4  Weissstandard (Transmission)
    '5  Schwarzstandard (Transmission)
    istat = 0
    If MessgDriver = "SPE" Then
      If NormfileID <> 1 And NormfileID <> 2 And NormfileID <> 9 And NormfileID <> 8 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
    End If
    If MessgDriver = "SPF" Then
      If NormfileID <> 1 And NormfileID <> 2 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
    End If
    If MessgMenue = 2 Then
      '
      '
      'Nullkalibrieren
      '
      '
      '
      '
      idka(0) = 2
      idka(1) = 1
      idka(2) = 0
    Else
      If Reftra = "A" Then
        idka(0) = 2
        idka(1) = 5
        idka(2) = 1
        idka(3) = 4
        idka(4) = 0
      ElseIf Reftra = "R" Then
        idka(0) = 2
        idka(1) = 1
        idka(2) = 0
      ElseIf Reftra = "T" Then
        idka(0) = 5
        idka(1) = 4
        idka(2) = 0
      End If
    End If

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
      MsgBox(Texxt(3526))
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
        Select Case MessgDriver
          Case "SPE"
            Call SPEMes(0, kw, rmes, istat, MscMes)
          Case "SPF"
            Call SPEMes(0, kw, rmes, istat, MscMes)
        End Select
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
    Dim iver As Short
    Dim delay As Short
    Dim wart As Short
    Dim buf As String

    '
    '
    'Einstellungen beim Initialisieren des Messger?tes
    '
    '
    If MscMes.IsOpen Then
      Exit Sub
    End If
    istat = NuWrt
    iver = 1000
    wart = 500
    delay = 10
    Call PoOpen(MscMes, iermes)
    cboSPE(0).Enabled = False
    cboSPE(1).Enabled = False
    If iermes <> 0 Then
      Call PoClose(MscMes, iermes)
      cboSPE(0).Enabled = True
      cboSPE(1).Enabled = True
      istat = -1
      Exit Sub
    End If
    CrLf = Chr(13) & Chr(10)
    '
    '
    'Apertur-Gr??e (Blenden-Gr??e)
    '
    '
    If Index = 0 Then
      stv = "*:" & CrLf
      '
      Call GetComString(Autom, stv, buf, 2, 1000, 5, 10, 3500, 1, "?", 0, "", MscMes, istat)
      If istat <> NuWrt Then Exit Sub

      stv = "A" & Mid(cboSPE(Index).Text, 1, 1) & "  ****:" & CrLf
      'MsgBox stv, 0
      Call GetComString(Autom, stv, buf, 10, iver, 100, wart, 3500, 1, Chr(13), -24, Mid(stv, 2, 1), MscMes, istat)
      If istat <> NuWrt Then Exit Sub
      'Call SPEcntr(stv, BUF, 10, 5, 5, 10, Iermes)
      'MsgBox BUF, 0
    ElseIf Index = 1 Then
      stv = "*:" & CrLf
      '
      Call GetComString(Autom, stv, buf, 2, 1000, 5, 10, 3500, 1, "?", 0, "", MscMes, istat)
      If istat <> NuWrt Then Exit Sub


      '
      'Fluoreszens (cut-off Frequenz)
      '
      '
      stv = "F" & Mid(cboSPE(Index).Text, 1, 3) & "****:" & CrLf
      'MsgBox stv, 0
      'Call SPEcntr(stv, BUF, 10, 5, 5, 200, Iermes)
      Call GetComString(Autom, stv, buf, 10, iver, delay, wart, 3500, 1, Chr(13), -22, Mid(stv, 2, 3), MscMes, istat)
      If istat <> NuWrt Then Exit Sub
      'MsgBox BUF, 0
    End If
    Call PoClose(MscMes, iermes)
    cboSPE(0).Enabled = True
    cboSPE(1).Enabled = True
  End Sub


  Sub SPErwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    'UPGRADE_WARNING: Lower bound of array RZ was changed from 1 to 0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Dim RZ(80) As Single
    Dim i As Short
    Dim j As Short
    Dim BUU As String
    '
    'Statuszeile entfernen
    '
    BUU = Mid(BUF, InStr(BUF, Chr(10)) + 1)
    '
    BUU = BUU.Replace(Chr(13), "")
    BUU = BUU.Replace(Chr(10), "")
    '
    '
    If IBASF = 1 Then
      '
      '       360-700nm (5nm)
      '
      '
      For j = 0 To 68
        'RZ(j) = Val(Mid(BUU, j * 8, 7))
        RZ(j) = Val(BUU.Substring(j * 8, 7))
      Next j
      If NormfileID = 1 Then
        For i = 0 To MessgNwe - 1
          r(i) = RZ(8 + 4 * i)
        Next i
      Else
        For i = 0 To MessgNwe - 1
          r(i) = RZ(i)
        Next i
      End If
    Else
      '
      '       NORMALGERAET 360-700nm (10nm)
      '
      '

      'MsgBox(BUU(Len(BUU) - 1))
      For j = 0 To 34
        'RZ(j) = Val(Mid(BUU, j * 8, 7))
        RZ(j) = Val(BUU.Substring(j * 8, 7))
      Next j

      If NormfileID = 1 Then
        For i = 0 To MessgNwe - 1
          r(i) = RZ(4 + 2 * i)
        Next i
      ElseIf NormfileID = 2 Then
        For i = 0 To MessgNwe - 1
          r(i) = RZ(4 + i)
        Next i
      ElseIf NormfileID = 8 Then
        For i = 0 To MessgNwe - 1
          r(i) = RZ(i)
        Next i
      End If
    End If
  End Sub

  Sub SPEMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)

    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim STW As String
    '
    Dim IBASF As Short
    '
    '
    '
    CrLf = Chr(13) & Chr(10)
    '
    buflen = 2
    iver = 1000
    wart = 5
    delay = 10
    '
    '
    'data IVERS/20/
    'DATA IDIF/100/
    'DATA CR/#0D/,LF/#0A/
    'DATA OO/'O'/,O0/'0'/
    'DATA STB/'B2R ****:'/
    'DATA STW/'W2R ****:'/
    'DATA STM/'M1@ ****:'/
    'DATA ST5/'M1@ 05****:'/
    'DATA STG/'Gx  ****:'/
    'DATA STI/'I050****:'/
    'DATA STF/'F000****:'/
    'DATA STS/'S   ****:'/
    'DATA SSI/'*:  '/
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '        8    360-700,10    NWE=35
    '        9    360-700,5     NWE=69
    '
    '      On Error GoTo labrefmes
    '      Err = 0
    '
    'Beispiel
    '
    '
    'Call GetComBuf(resp, Stv, BUF, buflen, iver, delay, wart, _
    '' 3500, CrLf, -25, "I", mscmes , ier)
    '
    CHRM = MessgChrm(kw)
    '      MsgBox CHRM & CStr(messg_ihrm(kw)), 0

    '
    If imess = 7 Then
      If kw = 0 Then
        '
        '        Synchronisieren
        '
        stv = "*:" & CrLf
        ' Call SPEcntr(Stv, BUF, 1, 5, 20, 50, ier)
        Call GetComString(Autom, stv, BUF, 2, 1000, 5, 10, 3500, 1, "?", 0, "", mscMES, istat)
        ' MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        '
        '        STATUS ABFRAGEN
        '
        stv = "S   ****:" & CrLf
        'Call SPEcntr(Stv, BUF, 10, 5, 10, 20, ier)
        Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3503, 1, Chr(13), -15, "1xxxx", mscMES, istat)
        'MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        '
        '
        '        Synchronisieren
        '
        stv = "*:" & CrLf
        ' Call SPEcntr(Stv, BUF, 1, 5, 20, 50, ier)
        Call GetComString(Autom, stv, BUF, 2, 1000, 5, 10, 3500, 1, "?", 0, "", mscMES, istat)
        ' MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub

        '           stv = "L050****:" & Chr$(13) & Chr$(10)
        '           Call SP600(stv, BUF, 10, 5, 10, 20)
        '           INITIALISIEREN
        stv = "I050****:" & CrLf
        ' Call SPEcntr(Stv, BUF, 10, 5, 10, 50, ier)
        Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, Chr(13), -15, "1xxxx", mscMES, istat)
        ' MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        '
        '        Synchronisieren
        '
        stv = "*:" & CrLf
        ' Call SPEcntr(Stv, BUF, 1, 5, 20, 50, ier)
        Call GetComString(Autom, stv, BUF, 2, 1000, 5, 10, 3500, 1, "?", 0, "", mscMES, istat)
        ' MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '
        stv = "B2R ****:" & CrLf
        ' Call SPEcntr(Stv, BUF, 10, 5, 10, 200, ier)

        Call GetComString(Autom, stv, BUF, 10, iver, 10, 200, 3501, 1, Chr(13), -15, "1xxxx", mscMES, istat)
        'MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        For i = 0 To MessgNwe - 1
          r(i) = 0.0#
        Next i
      End If
    ElseIf imess = 6 Or imess = 9 Then
      If kw = 0 Then
        '
        '        Synchronisieren
        '
        stv = "*:" & CrLf
        '
        Call GetComString(Autom, stv, BUF, 2, 1000, 5, 10, 3500, 1, "?", 0, "", mscMES, istat)
        ' MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '

        stv = "W2R ****:" & CrLf
        '
        Call GetComString(Autom, stv, BUF, 10, iver, 10, 200, 3501, 1, "1xxxx", 0, "", mscMES, istat)

        'MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        For i = 0 To MessgNwe - 1
          r(i) = 1.0#
        Next i
      End If
    Else
      '
      '
      '
      '       MIT GLANZFALLE (GLANZ EXCLUDED (E))
      '       OHNE GLANZFALLE (GLANZ INCLUDED (I))
      '
      If Mid(LTrim(CHRM), 2, 1) = "O" Or Mid(LTrim(CHRM), 2, 1) = "0" Then
        STW = "GE"
      Else
        STW = "GI"
      End If
      '
      '
      '        Synchronisieren
      '
      stv = "*:" & CrLf
      '
      Call GetComString(Autom, stv, BUF, 2, 1000, 1, 1, 3500, 1, "?", 0, "", mscMES, istat)
      ' MsgBox BUF, 0
      If istat <> NuWrt Then Exit Sub
      '
      '
      '
      '

      '        STATUS ABFRAGEN
      '
      stv = "S   ****:" & CrLf
      'Call SPEcntr(Stv, BUF, 10, 5, 10, 20, ier)
      Call GetComString(Autom, stv, BUF, buflen, 1000, 5, 10, 3503, 1, Chr(13), -15, "1xxxx", mscMES, istat)
      'MsgBox BUF, 0
      If istat <> NuWrt Then Exit Sub
      If InStr(Mid(BUF, 1, 3), Mid(STW, 2, 1)) = 0 Then
        '
        '         Synchronisieren
        '
        stv = "*:" & CrLf
        Call GetComString(Autom, stv, BUF, 2, 1000, 1, 1, 3500, 1, "?", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '

        '
        stv = STW & "  ****:" & CrLf
        Call GetComString(Autom, stv, BUF, 25, iver, 5, 10, 3504, 1, Chr(13), -25, Mid(stv, 2, 1), mscMES, istat)
        'MsgBox Len(BUF), 0
        If istat <> NuWrt Then Exit Sub
      End If
      If NormfileID = 9 Then
        '         Synchronisieren
        '
        stv = "*:" & CrLf
        Call GetComString(Autom, stv, BUF, 2, 1000, 1, 1, 3500, 1, "?", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '

        stv = "M1@ 05****:" & CrLf
        IBASF = 1
        '    Call SPEcntr(Stv, BUF, 10, 5, 10, 100, ier)
        Call GetComString(Autom, stv, BUF, 690, iver, 20, 100, 3501, 1, "1xxxx", 0, "", mscMES, istat)
        'MsgBox Len(BUF), 0
        If istat <> NuWrt Then Exit Sub
      Else
        '         Synchronisieren
        '
        stv = "*:" & CrLf
        Call GetComString(Autom, stv, BUF, 2, 1000, 1, 1, 3500, 1, "?", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '

        stv = "M1@ ****:" & CrLf
        '  Call SPEcntr(Stv, BUF, 10, 5, 10, 100, ier)
        Call GetComString(Autom, stv, BUF, 300, 1000, 20, 100, 3501, 1, "1xxxx", 0, "", mscMES, istat)
        'MsgBox Len(BUF), 0
        If istat <> NuWrt Then Exit Sub
        IBASF = 0
      End If
      Call SPErwert(IBASF, BUF, r, istat)
      If istat <> NuWrt Then Exit Sub
      'stv = "GI  ****:" & Chr$(13) & Chr$(10)
      'Call SPEcntr(Stv, BUF, 10, 5, 5, 20, ier)
      'Call GetComBuf(resp, stv, BUF, 10, 10, TrapTime, 5, _
      ''3504, 1, CrLf, -25, Mid(stv, 2, 1), mscmes , istat)
      'istat = -999
      'MsgBox BUF
      '
      '        Synchronisieren
      '
      stv = "*:" & CrLf
      '
      Call GetComString(Autom, stv, BUF, 2, 1000, 1, 1, 3500, 1, "?", 0, "", mscMES, istat)
      ' MsgBox BUF, 0
      If istat <> NuWrt Then Exit Sub
      '
      '
      '
      '

      '       STATUS ABFRAGEN
      '
      stv = "S   ****:" & CrLf
      'Call SPEcntr(Stv, BUF, 10, 5, 10, 20, ier)
      Call GetComString(Autom, stv, BUF, buflen, 1000, 10, 100, 3503, 1, Chr(13), -15, "1xxxx", mscMES, istat)
      'MsgBox BUF, 0
      If istat <> NuWrt Then Exit Sub
      If InStr(Mid(BUF, 1, 3), "I") = 0 Then
        '
        '           Synchronisieren
        '
        stv = "*:" & CrLf
        Call GetComString(Autom, stv, BUF, 2, 1000, 1, 1, 3500, 1, "?", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '
        'Einstellung Glanz included (wg. Gucki)
        '
        stv = "GI  ****:" & CrLf
        Call GetComString(Autom, stv, BUF, 25, iver, 1, 10, 3504, 1, Chr(13), -25, Mid(stv, 2, 1), mscMES, istat)
        'MsgBox Len(BUF), 0
        If istat <> NuWrt Then Exit Sub
      End If

    End If
  End Sub
  Sub SPFMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)

    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    Dim TrapTime As Short
    '
    Dim IBASF As Short
    CrLf = Chr(13) & Chr(10)
    TrapTime = 5
    If MessgDriver = "SPF" Then
      TrapTime = 40
    End If

    buflen = 2
    iver = 5
    wart = 5
    delay = 10
    resp = 2
    '
    '
    'data IVERS/20/
    'DATA IDIF/100/
    'DATA CR/#0D/,LF/#0A/
    'DATA OO/'O'/,O0/'0'/
    'DATA STB/'B2R ****:'/
    'DATA STW/'W2R ****:'/
    'DATA STM/'M1@ ****:'/
    'DATA ST5/'M1@ 05****:'/
    'DATA STG/'Gx  ****:'/
    'DATA STI/'I050****:'/
    'DATA STF/'F000****:'/
    'DATA STS/'S   ****:'/
    'DATA SSI/'*:  '/
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '        8    360-700,10    NWE=35
    '        9    360-700,5     NWE=69
    '
    '      On Error GoTo labrefmes
    '      Err = 0
    '
    'Beispiel
    '
    '
    'Call GetComBuf(resp, Stv, BUF, buflen, iver, delay, wart, _
    '' 3500, CrLf, -25, "I", mscmes , ier)
    '
    CHRM = MessgChrm(kw)
    '      MsgBox CHRM & CStr(messg_ihrm(kw)), 0

    '
    If imess = 7 Then
      If kw = 0 Then
        '
        '        Synchronisieren
        '
        stv = "*:" & CrLf
        ' Call SPEcntr(Stv, BUF, 1, 5, 20, 50, ier)
        Call GetComBuf(resp, stv, BUF, buflen, iver, delay, wart, 3503, 1, "?", 0, "", mscMES, istat)
        ' MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        '
        '        STATUS ABFRAGEN
        '
        stv = "S   ****:" & Chr(13) & Chr(10)
        'Call SPEcntr(Stv, BUF, 10, 5, 10, 20, ier)
        Call GetComBuf(resp, stv, BUF, buflen, iver, delay, wart, 3503, 1, Chr(13), -15, "1xxxx", mscMES, istat)
        'MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        '
        '           stv = "L050****:" & Chr$(13) & Chr$(10)
        '           Call SP600(stv, BUF, 10, 5, 10, 20)
        '           INITIALISIEREN
        stv = "I050****:" & CrLf
        ' Call SPEcntr(Stv, BUF, 10, 5, 10, 50, ier)
        Call GetComBuf(resp, stv, BUF, buflen, iver, delay, wart, 3500, 1, Chr(13), -15, "1xxxx", mscMES, istat)
        ' MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        stv = "B2R ****:" & CrLf
        ' Call SPEcntr(Stv, BUF, 10, 5, 10, 200, ier)
        Call GetComBuf(resp, stv, BUF, 10, 5, 10, 200, 3501, 1, CrLf, -15, "1xxxx", mscMES, istat)
        'MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        For i = 0 To MessgNwe - 1
          r(i) = 0.0#
        Next i
      End If
    ElseIf imess = 6 Or imess = 9 Then
      If kw = 1 Then
        stv = "W2R ****:" & CrLf
        'Call SPEcntr(Stv, BUF, 10, 5, 10, 200, ier)
        Call GetComBuf(resp, stv, BUF, 10, 5, 10, 200, 3501, 1, "1xxxx", 0, "", mscMES, istat)

        'MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        For i = 0 To MessgNwe - 1
          r(i) = 1.0#
        Next i
      End If
    Else
      '
      '
      '
      '       MIT GLANZFALLE (GLANZ EXCLUDED (E))
      '       OHNE GLANZFALLE (GLANZ INCLUDED (I))
      '
      If Mid(LTrim(CHRM), 2, 1) = "O" Or Mid(LTrim(CHRM), 2, 1) = "0" Then
        stv = "GE  ****:" & CrLf
      Else
        stv = "GI  ****:" & CrLf
      End If
      ' Call SPEcntr(Stv, BUF, 10, 5, 5, 20, ier)
      Call GetComBuf(resp, stv, BUF, 10, 10, TrapTime, 5, 3504, 1, CrLf, -25, Mid(stv, 2, 1), mscMES, istat)
      'MsgBox BUF, 0
      If istat <> NuWrt Then Exit Sub
      If NormfileID = 9 Then
        stv = "M1@ 05****:" & CrLf
        IBASF = 1
        '    Call SPEcntr(Stv, BUF, 10, 5, 10, 100, ier)
        Call GetComBuf(resp, stv, BUF, 10, 5, 10, 100, 3501, 1, "1xxxx", 0, "", mscMES, istat)
        'MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
      Else
        stv = "M1@ ****:" & CrLf
        '  Call SPEcntr(Stv, BUF, 10, 5, 10, 100, ier)
        Call GetComBuf(resp, stv, BUF, 10, 5, 10, 100, 3501, 1, "1xxxx", 0, "", mscMES, istat)
        'MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        IBASF = 0
      End If
      Call SPErwert(IBASF, BUF, r, istat)
      If istat <> NuWrt Then Exit Sub
      stv = "GI  ****:" & Chr(13) & Chr(10)
      'Call SPEcntr(Stv, BUF, 10, 5, 5, 20, ier)
      Call GetComBuf(resp, stv, BUF, 10, 10, TrapTime, 5, 3504, 1, CrLf, -25, Mid(stv, 2, 1), mscMES, istat)
      'istat = -999
      'MsgBox BUF

    End If
  End Sub

  Sub SPEini(ByRef istat As Short)
    '
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim CrLf As String
    Dim APECH As String
    Dim Aper As String
    Dim UVein As String
    '
    CrLf = Chr(13) & Chr(10)
    APECH = "NSU"
    buflen = 2
    iver = 32000
    wart = 5
    delay = 10
    '
    '
    '
    lblspe(0).Visible = True
    cboSPE(0).Visible = True
    lblspe(1).Visible = True
    cboSPE(1).Visible = True

    '
    '
    '
    'Apertureinstellung ermitteln
    '
    '
    '
    Call PoOpen(MscMes, iermes)
    If iermes <> 0 Then
      Call PoClose(MscMes, iermes)
      istat = -1
      Exit Sub
    End If
    stv = "*:" & CrLf
    '
    Call GetComString(Autom, stv, BUF, 2, 1000, 50, 100, 3500, 1, "?", 0, "", MscMes, istat)
    If istat <> NuWrt Then Exit Sub

    stv = "S   ****:" & Chr(13) & Chr(10)
    '
    Call GetComString(Autom, stv, BUF, buflen, iver, 10 * delay, 10 * wart, 3503, 1, Chr(13), -15, "1xxxx", MscMes, istat)
    Call PoClose(MscMes, iermes)
    If istat <> NuWrt Then Exit Sub
    i = InStr(BUF, "xx")
    'MsgBox BUF, 0
    Aper = Mid(BUF, i - 5, 1)
    UVein = Mid(BUF, i - 3, 3)
    'MsgBox Aper & "****" & UVein, 0
    '
    '
    '
    'Listindex Apertur
    '
    '
    '
    cboSPE(0).Items.Clear()
    For i = 0 To 2
      'cboSPE(0).Items.Add(New VB6.ListBoxItem(Mid(APECH, i + 1, 1), i))
      cboSPE(0).Items.Add(APECH.Substring(i, 1))
    Next i
    For i = 0 To cboSPE(0).Items.Count - 1
      'If Aper = VB6.GetItemString(cboSpe(0), i) Then
      If Aper = APECH.Substring(i, 1) Then
        cboSPE(0).SelectedIndex = i
        Exit For
      End If
    Next i
    '
    'Listindex UVkal
    '
    '
    cboSPE(1).Items.Clear()
    For i = 0 To 255
      cboSPE(1).Items.Add(Format(i, "000"))
    Next i
    For i = 0 To cboSPE(1).Items.Count - 1
      If UVein = Format(i, "000") Then
        cboSPE(1).SelectedIndex = i
        Exit For
      End If
    Next i


  End Sub
  Sub SPFini(ByRef istat As Short)
    '
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    Dim CrLf As String
    Dim APECH As String
    Dim Aper As String
    Dim UVein As String
    '
    CrLf = Chr(13) & Chr(10)
    APECH = "NSU"
    buflen = 2
    iver = 5
    wart = 5
    delay = 10
    resp = 2
    '
    '
    '
    lblspe(0).Visible = True
    cboSPE(0).Visible = True
    lblspe(1).Visible = True
    cboSPE(1).Visible = True
    '
    '
    '
    'Apertureinstellung ermitteln
    '
    '
    '
    Call PoOpen(MscMes, iermes)
    If iermes <> 0 Then
      Call PoClose(MscMes, iermes)
      istat = -1
      Exit Sub
    End If

    stv = "S   ****:" & Chr(13) & Chr(10)
    'Call SPEcntr(Stv, BUF, 10, 5, 5, 10, ier)
    Call GetComBuf(resp, stv, BUF, buflen, iver, delay, wart, 3503, 1, CrLf, -15, "1xxxx", MscMes, istat)
    Call PoClose(MscMes, iermes)
    If istat <> NuWrt Then Exit Sub
    i = InStr(BUF, "xx")
    'MsgBox BUF, 0
    Aper = Mid(BUF, i - 5, 1)
    UVein = Mid(BUF, i - 3, 3)
    'MsgBox Aper & "****" & UVein, 0
    '
    '
    '
    'Listindex Apertur
    '
    '
    '
    cboSPE(0).Items.Clear()
    For i = 0 To 2
      'cboSPE(0).Items.Add(New VB6.ListBoxItem(Mid(APECH, i + 1, 1), i))
      cboSPE(0).Items.Add(APECH.Substring(i, 1))
    Next i
    For i = 0 To cboSPE(0).Items.Count - 1
      'If Aper = VB6.GetItemString(cboSpe(0), i) Then
      If Aper = APECH.Substring(i, 1) Then
        cboSPE(0).SelectedIndex = i
        Exit For
      End If
    Next i
    '
    'Listindex UVkal
    '
    '
    cboSPE(1).Items.Clear()
    For i = 0 To 255
      cboSPE(1).Items.Add(Format(i, "000"))
    Next i
    For i = 0 To cboSPE(1).Items.Count - 1
      If UVein = Format(i, "000") Then
        cboSPE(1).SelectedIndex = i
        Exit For
      End If
    Next i

  End Sub

  Sub MesIni()
    istat = NuWrt
    Select Case MessgDriver
      Case "SPE"
        Call SPEini(istat)
      Case "SPF"
        Call SPEini(istat)
    End Select
  End Sub
  Sub MesKal(ByRef imess As Short)
    Dim kw As Integer
    Dim jj As Short
    Dim j As Integer
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
      'ger?teabh?ngige Driver
      '
      '
      '
      For kw = 0 To MessgKM - 1
        Select Case MessgDriver
          Case "SPE"
            Call SPEMes(imess, kw, rmes, istat, MscMes)
          Case "SPF"
            Call SPEMes(imess, kw, rmes, istat, MscMes)
        End Select
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


  Sub New()
    MyBase.New()
    istat = 0
    MnReTr = 0
  End Sub
  Sub dispose() Implements IDisposable.Dispose
  End Sub
End Class