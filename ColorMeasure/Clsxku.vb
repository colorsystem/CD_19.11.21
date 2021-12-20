Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsXKU
  Implements IDisposable

  'Private Iauswhl As Integer
  Private Kauswhl As Short
  Private istat As Short
  Private iermes As Short
  Dim kw As Short
  Dim j As Short
  Dim i As Short
  Dim nkw As Short
  Dim MnReTr As Short
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
    istat = NuWrt
  End Sub
  Sub MesKal(ByRef imess As Short)
    Dim jj As Short
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
    'For j = 1 To irmax
    '
    '
    'geräteabhängige Driver
    '
    '
    '
    For kw = 0 To MessgKM - 1
      Select Case MessgDriver
        Case "XKU"
          Call XKUMes(imess, kw, rmes, istat, MscMes)
        Case "XKV"
          Call XKU58Mes(imess, kw, rmes, istat, MscMes)
        Case "XKW"
          Call XKU58Mes(imess, kw, rmes, istat, MscMes)
        Case "XCA"
          Call XCAMes(imess, kw, rmes, istat, MscMes)
        Case "XCF"
          Call XCFMes(imess, kw, rmes, istat, MscMes)
        Case "XMU"
          Call XMUMes(imess, kw, rmes, istat, MscMes)
        Case "XMV"
          Call XMUMes(imess, kw, rmes, istat, MscMes)
        Case "X45"
          Call X45Mes(imess, kw, rmes, istat, MscMes)
        Case "X53"
          Call X530Mes(imess, kw, rmes, istat, MscMes)
        Case "X62"
          Call X62Mes(imess, kw, rmes, istat, MscMes)
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
    NXTChk = True
    MNbtnNXT.Visible = False

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
    If Autom() = 0 Then
      MNbtnNXT.Visible = True
    Else
      MNbtnNXT.Visible = False
    End If
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
          Case "XKU"
            Call XKUMes(0, kw, rmes, istat, MscMes)
          Case "XKV"
            Call XKU58Mes(0, kw, rmes, istat, MscMes)
          Case "XKW"
            Call XKU58Mes(0, kw, rmes, istat, MscMes)
          Case "XCA"
            Call XCAMes(0, kw, rmes, istat, MscMes)
          Case "XCF"
            Call XCFMes(0, kw, rmes, istat, MscMes)
          Case "XMU"
            Call XMUMes(0, kw, rmes, istat, MscMes)
          Case "XMV"
            Call XMUMes(0, kw, rmes, istat, MscMes)
          Case "X45"
            Call X45Mes(0, kw, rmes, istat, MscMes)
          Case "X53"
            Call X530Mes(0, kw, rmes, istat, MscMes)
          Case "X62"
            Call X62Mes(0, kw, rmes, istat, MscMes)
        End Select
        If istat <> NuWrt Then Exit For
        Call NormRwrt(MnReTr, kw, rmes)
        MNbtnMIT.Enabled = False
        MNbtnLOE.Enabled = False
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
      MNbtnNXT.Enabled = True
      If Autom() = 0 Then
        If MesZae = Mesmax Then Exit Do
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
      End If
      If istat <> NuWrt Then Exit Do

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

    If NormfileID <> 1 And NormfileID <> 2 Then
      MsgBox(Texxt(3519))
      istat = -1
      Exit Sub
    End If
    idka(0) = 1
    idka(1) = 2
    idka(2) = 0
  End Sub
  Function Autom() As Short
    If MessgDriver = "XKV" Or MessgDriver = "XMV" Or MessgDriver = "XKW" Or MessgDriver = "XCF" Then
      Autom = 0
    Else
      Autom = 1
    End If
  End Function
  '


  Sub XKUMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim Warte As Short
    Dim delay As Short
    'UPGRADE_WARNING: Lower bound of array Rsave was changed from 1,1 to 0,0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Static Rsave(31, 2) As Single
    buflen = 2
    iver = 2000
    Warte = 100
    wart = 30
    delay = 30
    COMchk = True
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '
    CHRM = MessgChrm(kw)
    '      STATUS ABFRAGEN
    '
    '
    If imess = 1 Then
      If kw = 0 Then
        '
        '      Reset ohne Löschen
        '
        stv = "p" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, wart, 3500, 1, "RESET", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        'Halt
        '
        '
        '
        '
        '      Tastatur sperren
        '

        '
        '
        stv = "0j" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, Warte, 3500, 1, "0", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Baud und Handshake
        '
        Call Wait(100)
        stv = "614sc" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, Warte, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
       
        '
        '         Meldung für Display Meßgerät
        '
        '

        MsgBox(Texxt(3524), 0)
        stv = "yw" & Chr(13)
        Call GetComString(Autom, stv, BUF, buflen, 32000, 10, wart, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Do While InStr(BUF, "OK") = 0
          Call Wait(10)
          BUF = mscMES.ReadLine
          System.Windows.Forms.Application.DoEvents()
          If istat <> NuWrt Then Exit Sub
        Loop
      End If 'kw

      For i = 0 To MessgNwe - 1
        r(i) = 100.0#
      Next i
      '
      '
    ElseIf imess = 2 Then
      If kw = 0 Then
       
        '
        '      Analyse Mode
        '
        stv = "0sm" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, wart, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Printout Format
        '
        stv = "211sp" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 20, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub



      End If 'kw
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 0 Then
      '
      If kw = 0 Then
        '
       
        '
        '      Beep zum Messen
        '
        stv = "00A04Cw" & Chr(13)
        Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call Wait(200)
        '
        '
        '
       
      End If 'kw
      '
      '
      '
      '      Messung starten
      '
      If Not (Mid(LTrim(CHRM), 2, 1) = "O" Or Mid(LTrim(CHRM), 2, 1) = "0") Then
        '
        '
        '
        'Meßwerte übernehmen mit Glanz
        '

        stv = "203D2000i" & Chr(13)
        Call GetComString(0, stv, BUF, 190, iver, 10, wart, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call XkUhex(1, BUF, r, istat)



        '
        '

        '
        '         Meßwerte übernehmen ohne Glanz
      Else
        '
        stv = "207B203Ei" & Chr(13)
        Call GetComString(0, stv, BUF, 190, iver, delay, wart, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub


        Call XkUhex(2, BUF, r, istat)
      End If
      '
      '
      'Display entsperren
      '
      '
      '
      If kw = 0 Then
        stv = "1j" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 5, 3500, 1, "1", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
      End If
    End If

    '

  End Sub

  Sub X530Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim Warte As Short
    Dim delay As Short
    'UPGRADE_WARNING: Lower bound of array Rsave was changed from 1,1 to 0,0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Static Rsave(30, 1) As Single
    '
    buflen = 2
    iver = 2000
    Warte = 100
    wart = 30
    delay = 30
    COMchk = True
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '
    MNlblMES.Visible = False

    CHRM = MessgChrm(kw)
    '     X-Rite 530
    '
    '      Clear errors
    '
    stv = "CE" & Chr(13)
    Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
    If istat <> NuWrt Then Exit Sub

    '
    '
    '
    '
    '
    '
    '

    If imess = 1 Then
      If kw = 0 Then
        '
        '
        '

        stv = "0107CF" & Chr(13)
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '       Weißkalibration
        '
        MsgBox(Texxt(3524), 0)

        stv = "UC" & Chr(13)
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call Wait(200)

        '
        '
        '
        '
        '
        '
        For i = 0 To MessgNwe - 1
          r(i) = 100.0#
        Next i
        '
        '
      End If
    ElseIf imess = 2 Then
      If kw = 0 Then

      End If 'kw
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 0 Then
      '
      If kw = 0 Then
        '
        '
        '
        'Kalibrierstatus abfragen
        '
        '
        '
        '
        MNlblMES.Text = Texxt(3528)
        MNlblMES.Visible = True

        stv = "01ST" & Chr(13)
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        stv = ""
        Call GetComString(0, stv, BUF, 4, 32000, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        stv = "03TS" & Chr(13)
        Call GetComString(0, stv, BUF, 200, 10, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        
        If istat <> NuWrt Then Exit Sub
        Call X530X62rwert(0, BUF, r, istat)
        MNlblMES.Visible = True

        '
      End If
    End If
    '
    '
    '

  End Sub
  Sub X62Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim Warte As Short
    Dim delay As Short
    'UPGRADE_WARNING: Lower bound of array Rsave was changed from 1,1 to 0,0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Static Rsave(30, 1) As Single
   
    '
    buflen = 2
    iver = 2000
    Warte = 100
    wart = 30
    delay = 30
    COMchk = True
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '
    MNlblMES.Visible = False

    CHRM = MessgChrm(kw)
    '     X-Rite 530
    '
    '      Clear errors
    '
    stv = "CE" & Chr(13)
    Call GetComString(0, stv, BUF, 4, iver, 0.2 * delay, 0.2 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
    If istat <> NuWrt Then Exit Sub

    '
    '
    '
    '
    '
    '
    '

    If imess = 1 Then
      If kw = 0 Then
        '
        '
        '

        stv = "0005CF" & Chr(13) 'Autotransmit OFF
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '

        stv = "0107CF" & Chr(13) 'Separator Komma
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '

        stv = "0108CF" & Chr(13) 'Delimiter CRLF
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '

        stv = "0040CF" & Chr(13) 'Emulation OFF
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        MsgBox(Texxt(3524), 0)
        '
        '
        '
        '
        '

        '
        '
        '
        '       Weißkalibration/Schwarzkalibrierung
        '
        '
        '
        '
        stv = "10XD" & Chr(13)
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call Wait(200)

        '
        '
        '
        '
        '
        stv = "BP" & Chr(13) 'Beep
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '
        '
        '
        For i = 0 To MessgNwe - 1
          r(i) = 100.0#
        Next i
        '
        '
      End If
    ElseIf imess = 2 Then
      If kw = 0 Then

      End If 'kw
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 0 Then
      '
      If kw = 0 Then
        '
        '
        '
        'Kalibrierstatus abfragen
        '
        '
        '
        '
        MNlblMES.Text = Texxt(3528)
        MNlblMES.Visible = True

        stv = "01XD" & Chr(13) 'Kalibrierstatus
        Call GetComString(0, stv, BUF, 4, iver, 0.2 * delay, 0.2 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '
        '
        '
        '
        '
        '
        stv = "BP" & Chr(13) 'Beep
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        stv = ""
        Call GetComString(0, stv, BUF, 3, 32000, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
      End If
      If Trim(CHRM) = "GO" Then
        stv = "101GM" & Chr(13)
      Else
        stv = "001GM" & Chr(13)
      End If
      Call GetComString(0, stv, BUF, 200, 10, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      Call X530X62rwert(0, BUF, r, istat)
      MNlblMES.Visible = True

    End If
    '
    '
    '

  End Sub
  Sub XS62Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim Warte As Short
    Dim delay As Short
    Static Rsave(31, 2) As Single

    buflen = 2
    iver = 2000
    Warte = 100
    wart = 30
    delay = 30
    COMchk = True
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '
    MNlblMES.Visible = False

    CHRM = MessgChrm(kw)
    '     X-Rite S62
    '
    '      Clear errors
    '
    stv = "CE" & Chr(13)
    Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
    If istat <> NuWrt Then Exit Sub

    '
    '
    '
    '
    '
    '
    '

    If imess = 1 Then
      If kw = 0 Then
        '
        stv = "0005CF" & Chr(13) 'Autotransmit OFF
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub

        '
        '

        stv = "0107CF" & Chr(13) 'Separartor = Komma
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '
        '
        '
        stv = "0108CF" & Chr(13) 'Delimiter CRLF
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '
        '
        stv = "0040CF" & Chr(13) 'Emulation OFF
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '       Weißkalibration
        '
        MsgBox(Texxt(3524), 0)

        stv = "10XD" & Chr(13)
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call Wait(200)

        '
        '
        '
        '
        '
        '
        For i = 0 To MessgNwe - 1
          r(i) = 100.0#
        Next i
        '
        '
      End If
    ElseIf imess = 2 Then
      If kw = 1 Then

      End If 'kw
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 0 Then
      '
      If kw = 0 Then
        '
        '
        '
        'Kalibrierstatus abfragen
        '
        '
        '
        '
        MNlblMES.Text = Texxt(3528)
        MNlblMES.Visible = True

        stv = "01XD" & Chr(13)
        Call GetComString(0, stv, BUF, 4, iver, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        stv = ""
        Call GetComString(0, stv, BUF, 4, 32000, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
      End If
      If Trim(CHRM) = "GO" Then
        stv = "101GM" & Chr(13)
      Else
        stv = "001GM" & Chr(13)
      End If
      Call GetComString(0, stv, BUF, 200, 10, delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      Call X530X62rwert(0, BUF, r, istat)
      MNlblMES.Visible = True


    End If
    '
    '
    '

  End Sub

  Sub X530X62rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim i As Short
    Dim j As Short
    Dim rr(30) As Single
    Dim Hlf As String
    '
    'Statuszeile entfernen
    '
    If Len(BUF) < 200 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If

    '
    '       400-700 nm (10nm)
    '
    '
    '     R-Werte übernehmen
    '
    '
    '
    j = -1
    Hlf = ""
    For i = 0 To Len(BUF) - 1
      If j = 30 Then Exit For
      If BUF.Substring(i, 1) = " " Or BUF.Substring(i, 1) = Chr(9) Or BUF.Substring(i, 1) = Chr(10) Or BUF.Substring(i, 1) = Chr(13) Or BUF.Substring(i, 1) = "," Then
        If Hlf <> "" Then
          j = j + 1
          If j > 31 Then
            Exit Sub
          End If
          rr(j) = Val(Hlf)
          Hlf = ""
        End If
      Else
        Hlf = Hlf & BUF.Substring(i, 1)

      End If
    Next i
    If j < 30 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    If MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = rr(2 * i)
      Next i
    ElseIf MessgNwe = 31 Then
      For i = 0 To MessgNwe - 1
        r(i) = rr(i)
      Next i
    End If


    '
  End Sub

  Sub XKU58Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim Warte As Short
    Dim delay As Short
    'UPGRADE_WARNING: Lower bound of array Rsave was changed from 1,1 to 0,0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Static Rsave(31, 2) As Single
    '
    buflen = 2
    iver = 2000
    Warte = 200
    wart = 50
    delay = 50
    COMchk = True
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '
    CHRM = MessgChrm(kw)
    '
    '      STATUS ABFRAGEN
    '
    '
    '
    If imess = 1 Then
      If kw = 0 Then
        '
        '      Reset ohne Löschen
        '
        stv = "p" & Chr(13)
        Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, "RESET", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        'Halt
        '
        stv = "h" & Chr(13)
        Call GetComString(Autom, stv, BUF, 5, iver, delay, wart, 3500, 1, "HALT", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub

        '

        '
        '
        '      Tastatur sperren
        '

        '
        '
        stv = "0j" & Chr(13)
        Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, "0", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Baud und Handshake
        '
        stv = "614sc" & Chr(13)
        Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        'If MessgDriver = "XKW" Then
        '


        '
        '         Weißstandard MessgDriver =XKW
        '
        stv = "yw" & Chr(13)
        Call GetComString(Autom, stv, BUF, 0, iver, delay, Warte, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        stv = "g" & Chr(13)
        Call GetComString(Autom, stv, BUF, 5, iver, delay, Warte, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        'Wait
        '
        '
        Call Wait(wart)
        '
        '
        '
        '
        '
        'Halt
        '
        '

        '
        '

        stv = "h" & Chr(13)
        Call GetComString(Autom, stv, BUF, 5, iver, delay, wart, 3500, 1, "HALT", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        'Read Display
        '
        '
        stv = "x" & Chr(13)
        Call GetComString(Autom, stv, BUF, 20, iver, delay, Warte, 3500, 1, "READ ZERO", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '         Meldung für Display Meßgerät
        '
        '
      End If 'kw
      For i = 0 To MessgNwe - 1
        r(i) = 100.0#
      Next i
      '
      '
    ElseIf imess = 2 Then
      If kw = 0 Then
        '
        'Schwarzstandard bei MessgDriver=XKW
        '
        '
        stv = "g" & Chr(13)
        Call GetComString(Autom, stv, BUF, 5, iver, delay, Warte, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        'Halt
        '
        '

        '
        '

        stv = "h" & Chr(13)
        Call GetComString(Autom, stv, BUF, 5, iver, delay, wart, 3500, 1, "HALT", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        'Read Display
        '
        '
        stv = "x" & Chr(13)
        Call GetComString(Autom, stv, BUF, 15, iver, delay, wart, 3500, 1, "ZERO REFLECTANCE", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Analyse Mode
        '
        stv = "0sm" & Chr(13)
        Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Printout Format
        '
        stv = "21sp" & Chr(13)
        Call GetComString(Autom, stv, BUF, buflen, iver, delay, 20, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '

        stv = "211sp" & Chr(13)
        Call GetComString(Autom, stv, BUF, buflen, iver, delay, 20, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub



      End If 'kw
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 0 Then
      '
      If kw = 0 Then
        '
        stv = "m" & Chr(13)
        Call GetComString(Autom, stv, BUF, 10, iver, delay, Warte, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Beep zum Messen
        '
        '
        '
        '
        '      Messung starten
        '
        stv = "me" & Chr(13)
        Call GetComString(Autom, stv, BUF, buflen, iver, 10, 10, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        'Meßwerte übernehmen mit Glanz
        '
        '
        '
        '        '
      End If
      '

      If Not (Mid(LTrim(CHRM), 2, 1) = "O" Or Mid(LTrim(CHRM), 2, 1) = "0") Then
        '
        '
        '
        'Meßwerte übernehmen mit Glanz
        '

        stv = "203D2000i" & Chr(13)
        Call GetComString(0, stv, BUF, 190, iver, 10, wart, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call XkUhex(1, BUF, r, istat)



        '
        '         Meßwerte übernehmen ohne Glanz
      Else
        '
        stv = "207B203Ei" & Chr(13)
        Call GetComString(0, stv, BUF, 190, iver, delay, wart, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call XkUhex(2, BUF, r, istat)
      End If
      '
      '
      'Display entsperren
      '
      '
      '
      If kw = 0 Then
        stv = "1j" & Chr(13)
        Call GetComString(Autom, stv, BUF, buflen, iver, delay, 5, 3500, 1, "1", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
      End If
    End If
  End Sub

  Sub XCAMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Static Rsave(31, 2) As Single
    buflen = 2
    iver = 32000
    wart = 50
    delay = 5
    COMchk = True
    '
    '
    'Driver für CA22 von X-Rite (Maus)
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '
    CHRM = MessgChrm(kw)
    '      STATUS ABFRAGEN
    '
    If imess = 1 Then
      If kw = 0 Then
        stv = "0PR" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 0.1 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub

        '
        '      Fehlercode löschen
        '

        stv = "CE" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 0.1 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Manuell messen
        '
        '
        '
        '      Echo off
        '
        stv = "0EC" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 0.1 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Handshake off
        '
        stv = "0004CF" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 0.1 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '      Decimalpunkt on
        '
        stv = "0106CF" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 0.1 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Separator = space
        '
        stv = "0007CF" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 0.1 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '

        '
        '
        '      Delimiter CRLF
        '

        '
        '
        stv = "0108CF" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 0.1 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      2 Dezimalstellen (precision)
        '
        stv = "010ACF" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 0.1 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Header off
        '
        stv = "0028CF" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 0.1 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '         Meldung für Display Meßgerät
        '
        '


        stv = "CE" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 0.1 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '      Weißkalibrierung
        '
        Beep()
        MsgBox(Texxt(461))

        stv = "1CA" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, 10 * delay, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        For i = 0 To MessgNwe - 1
          r(i) = 100.0#
        Next i
      End If 'Kw
      '
      '
    ElseIf imess = 2 Then
      If kw = 0 Then
        Beep()
        MsgBox(Texxt(462))
        stv = "1CB" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, 10 * delay, 3 * wart, 3500, 1, "<00>", 0, "", mscMES, istat)

        If istat <> NuWrt Then Exit Sub

        '


      End If 'kw
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 0 Then
      '
      If kw = 0 Then
        '
        '
        '      Beep zum Messen
        '

        stv = "CE" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, wart, 3500, 1, "00", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        Beep()
        MsgBox("Messen")

        stv = ""
        Call GetComString(0, stv, BUF, buflen, iver, delay, wart, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        stv = "1SR" & Chr(13)
        Call GetComString(0, stv, BUF, 349, iver, 10, wart, 3500, 1, "<00>", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call XCArwert(0, BUF, r, istat)
        '
      End If
    End If
    '

  End Sub
  Sub XCFMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Static Rsave(31, 2) As Single
    '
    buflen = 2
    iver = 2000
    wart = 50
    delay = 20
    COMchk = True
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '
    CHRM = MessgChrm(kw)
    '
    If imess = 1 Then
      If kw = 0 Then
        '
        '      Reset ohne Löschen
        '
        stv = "P" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, wart, 3500, 1, "00", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        'Weiß-Kalibrieren
        '
        stv = "C" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 10 * wart, 3500, 1, "00", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub

        '

        '
        '
      End If 'kw

      For i = 0 To MessgNwe - 1
        r(i) = 100.0#
      Next i
      '
      '
    ElseIf imess = 2 Then
      If kw = 0 Then
        '
        '      Analyse Mode
        '
        stv = "B" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, 10 * wart, 3500, 1, "00", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '


      End If 'kw
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 0 Then
      '
      If kw = 0 Then
        '
        '
        '      Beep zum Messen
        '
        stv = "M" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, wart, 3500, 1, "00", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '
        '
        'Meßwerte übernehmen mit Glanz
        '
        '
        '
        stv = "1SR" & Chr(13)
        Call GetComString(0, stv, BUF, 300, iver, delay, wart, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call XCFrwert(1, BUF, r, istat)
        '
        '      Meßwerte übernehmen ohne Glanz
        '
      End If
    End If
    '

    '
    '
    'Display entsperren
    '
    '
    '
  End Sub


  '
  Sub XMUMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim Stv58(3) As String
    Dim Stv68(5) As String
    Dim IhM58(3) As String
    Dim IhM68(5) As String
    Static Kanz As Short
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    '
    'Winkel für MA58
    '
    '
    IhM58(1) = "25"
    IhM58(2) = "45"
    IhM58(3) = "75"
    Stv58(1) = "207B2000i"
    Stv58(2) = "20F7207Ci"
    Stv58(3) = "217320F8i"

    '
    'Winkel für MA68
    '
    '
    IhM68(1) = "15"
    IhM68(2) = "25"
    IhM68(3) = "45"
    IhM68(4) = "75"
    IhM68(5) = "110"
    Stv68(1) = "207B2000i"
    Stv68(2) = "20F7207Ci"
    Stv68(3) = "217320F8i"
    Stv68(4) = "21EF2174i"
    Stv68(5) = "226B21F0i"



    '
    '

    buflen = 2
    iver = 32000
    wart = 50
    delay = 50
    COMchk = True
    '
    If kw = 0 Then
      stv = "v" & Chr(13)
      Call GetComString(0, stv, BUF, buflen, iver, 0.1 * delay, 0.1 * wart, 3500, 1, "X-RITE", 0, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      If InStr(BUF, "MA58") <> 0 Then
        Kanz = 3
      ElseIf InStr(BUF, "MA68") <> 0 Then
        Kanz = 5
      End If
    End If


    '
    '
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '
    CHRM = MessgChrm(kw)
    '
    '      STATUS ABFRAGEN
    '
    '
    '
    If imess = 1 Then
      If kw = 0 Then
        '
        '      Reset ohne Löschen
        '
        stv = "p" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, wart, 3500, 1, "RESET", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '

        '
        '
        '      Kalibrierstatus auf nicht kalibriert setzen
        '

        '
        stv = "3ys" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, wart, 3500, 1, "CALIBRATION", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        'Beschreibe Zelle
        '
        '
        '
        '
        '
        '
        '         Meldung für Display Meßgerät
        '
        '

        MsgBox(Texxt(3524), 0)
        stv = "yw" & Chr(13)
        Call GetComString(0, stv, BUF, buflen, iver, delay, wart, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        'Prüfen, ob kalibriert
        '
        '
        stv = "ys" & Chr(13)

        Call GetComString(0, stv, BUF, buflen, iver, delay, wart, 3500, 1, "CALIBRATION OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub

      End If 'kw

      For i = 0 To MessgNwe - 1
        r(i) = 100.0#
      Next i
      '
      '
    ElseIf imess = 2 Then
      '
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 0 Then
      '
      If kw = 0 Then
        '
        '
        '      Beep zum Messen
        '
        stv = "00A04Cw" & Chr(13)
        Call GetComString(Autom, stv, BUF, buflen, iver, 0.1 * delay, 0.1 * wart, 3500, 1, "OK", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        stv = ""
        Call GetComString(0, stv, BUF, 0, iver, delay, wart, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub

      End If 'kw
      'Meßwerte übernehmen für verschiedene Winkel
      '
      '

      '
      If Kanz = 3 Then
        '
        'MA58
        '
        For i = 1 To Kanz
          If Trim(CHRM) = Trim(IhM58(i)) Then
            stv = Stv58(i) & Chr(13)
            Exit For
          End If
        Next i
      ElseIf Kanz = 5 Then
        '
        'MA68
        '
        For i = 1 To Kanz
          If Trim(CHRM) = Trim(IhM68(i)) Then
            stv = Stv68(i) & Chr(13)
            Exit For
          End If
        Next i
      End If
      '
      '      Messwerte für verschiedene Winkel übernehmen
      '
      Call GetComString(0, stv, BUF, 300, iver, 0.1 * delay, 0.1 * wart, 3500, 1, "OK", 0, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      Call XMUrwert(0, BUF, r, istat)
    End If
    '
  End Sub


  '
  Sub X45Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim IveBlack As Short
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    Static Rsave(31) As Single
    Dim Izero As Short
    '
    buflen = 0
    iver = 5
    wart = 100
    delay = 10
    resp = 2
    COMchk = True
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '
    CHRM = MessgChrm(kw)
    '
    '      STATUS ABFRAGEN
    '
    '
    '
    If imess = 1 Then
      If kw = 0 Then

        '      Meldung für Display Meßgerät
        '
        '
        MsgBox(Texxt(3524), 0)
        Izero = 0
        '
        '      Reset ohne Löschen
        '
        stv = "p" & Chr(13)
        Call GetComBuf(resp, stv, BUF, 0, iver, delay, 30, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '     Baud-rate (9600)
        '
        '
        stv = "60t" & Chr(13)
        Call GetComBuf(resp, stv, BUF, 0, iver, delay, 30, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        'Version
        '
        '
        stv = "v" & Chr(13)
        Call GetComBuf(resp, stv, BUF, 6, iver, delay, 30, 3500, 1, "x-rite", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        stv = "1j" & Chr(13)
        Call GetComBuf(resp, stv, BUF, 0, iver, delay, 30, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '      Weißstandard
        '
        stv = "22y" & Chr(13)
        Call GetComBuf(resp, stv, BUF, 0, iver, 50, 100, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        Call Wait(200)
        '
        stv = "x" & Chr(13)
        Call GetComBuf(20, stv, BUF, 6, iver, delay, wart, 3500, 1, "CHANGE", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Beep
        '
        stv = "0B64si" & Chr(13)
        Call GetComBuf(resp, stv, BUF, 0, iver, delay, wart, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        Call Wait(100)
        '
        '      Read Display
        '
        IveBlack = 0
        Do
          stv = "x" & Chr(13)
          Call GetComBuf(1, stv, BUF, 0, 1, 50, wart, 3500, 1, "", 0, "", mscMES, istat)
          If InStr(BUF, "CHANGE") <> 0 Then
            Call Wait(500)
          End If
          If InStr(BUF, "READ ZERO REFL.") <> 0 And Izero = 0 Then
            Call Wait(200)
            '
            '           Beep
            '
3:          stv = "0B64si" & Chr(13)
            Call GetComBuf(resp, stv, BUF, 0, iver, delay, wart, 3500, 1, "", 0, "", mscMES, istat)
            Izero = 1
            Call Wait(500)
            Exit Do
          End If
          If InStr(BUF, "READ") <> 0 Then
            Call Wait(100)
          End If
          If InStr(BUF, "UPDATED") <> 0 Or InStr(BUF, "XYZ") <> 0 Then
            Exit Do
          End If
          If Izero = 1 Then
            IveBlack = IveBlack + 1
            Call Wait(100)
            If IveBlack > 5 Then
              Exit Do
            End If
          End If
          System.Windows.Forms.Application.DoEvents()
          If istat <> NuWrt Then Exit Sub
        Loop
        If istat <> NuWrt Then Exit Sub
        For i = 0 To MessgNwe - 1
          r(i) = 100.0#
        Next i
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
        '
        'Tastatur sperren
        '
        '
        'Tastatur  sperren
        '
        '
        stv = "0j" & Chr(13)
        Call GetComBuf(resp, stv, BUF, 0, iver, delay, wart, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '      Beep
        '
        stv = "0B64si" & Chr(13)
        Call GetComBuf(resp, stv, BUF, 0, iver, delay, wart, 3500, 1, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '      Messen
        '
        '
        stv = "g" & Chr(13)
        Call GetComBuf(32000, stv, BUF, 3, iver, delay, wart, 3500, 1, "REF", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call X45rwert(1, BUF, r, istat)
        '
      End If
      '

      '      MIT GLANZFALLE (GLANZ EXCLUDED (E))
      '      OHNE GLANZFALLE (GLANZ INCLUDED (I))
      '
      '      Messwerte gemäß messgnwe umspeichern
      '
      '
    End If
    '
    '
    'Display entsperren
    '
    '
    '
    stv = "1j" & Chr(13)
    Call GetComBuf(resp, stv, BUF, 0, iver, delay, wart, 3500, 1, "", 0, "", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
  End Sub

  Sub XKUrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef ier As Short)
    Dim i As Short
    Dim BChar As String
    Dim Rh(31) As Single
    '
    'Statuszeile entfernen
    '
    If Len(BUF) < 350 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    '
    '
    '
    '
    For i = 0 To 30
      BChar = Mid(BUF, i * 11 + 1, 11)
      Rh(i) = Val(Mid(BChar, 2))
    Next i
    If MessgNwe = 31 Then
      For i = 0 To MessgNwe - 1
        r(i) = Rh(i)
      Next i
    ElseIf MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = Rh(2 * i)
      Next i
    End If
  End Sub
  Sub XKU58rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r(,) As Single, ByRef ier As Short)
    Dim i As Short
    Dim BChar As String
    Dim Rh(31) As Single
    '
    'Statuszeile entfernen
    '
    If Len(BUF) < 350 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    '
    '
    '
    '
    For i = 0 To 30
      BChar = Mid(BUF, i * 11 + 1, 11)
      Rh(i) = Val(Mid(BChar, 2))
    Next i
    If MessgNwe = 31 Then
      For i = 0 To MessgNwe - 1
        r(i, IBASF) = Rh(i)
      Next i
    ElseIf MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i, IBASF) = Rh(2 * i)
      Next i
    End If
  End Sub

  Sub XCArwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef ier As Short)
    Dim i As Short
    Dim j As Short
    Dim Rh(31) As Single
    '
    'Statuszeile entfernen
    '
    If Len(BUF) < 349 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    j = InStr(BUF, "S")
    For i = 1 To 31
      Rh(i) = Val(Mid(BUF, j + (i - 1) * 11 + 1, 7))
    Next i
    If MessgNwe = 31 Then
      For i = 0 To MessgNwe - 1
        r(i) = Rh(i)
      Next i
    ElseIf MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = Rh(2 * i)
      Next i
    End If


    '
    '
    '
  End Sub
  Sub XCFrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef ier As Short)
    Dim i As Short
    Dim j As Short
    Dim k As Short
    Dim l As Short
    Dim rr(31) As Single
    '
    'Statuszeile entfernen
    '
    If Len(BUF) < 316 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    k = 1
    For i = 0 To 30
      j = InStr(Mid(BUF, k), "S")
      l = InStr(Mid(BUF, j + k), Chr(13))
      rr(i) = Val(Mid(BUF, j + k, l))
      k = k + j + l
    Next i
    If MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = rr(2 * i)
      Next i
    Else
      For i = 0 To MessgNwe - 1
        r(i) = rr(i)
      Next i
    End If




    '
    '
    '
  End Sub

  Sub XMUrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef ier As Short)
    Dim i As Short
    Dim j As Short
    Dim BChar As String
    Dim Iwert As Integer
    Dim rr(30) As Single
    '
    'Statuszeile entfernen
    '
    If Len(BUF) < 380 Or Len(BUF) > 400 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    '
    '
    '
    '
    BChar = ""
    For i = 1 To Len(BUF)
      If Mid(BUF, i, 1) <> " " Then
        BChar = BChar & Mid(BUF, i, 1)
      End If
    Next i
    BUF = BChar
    For i = 0 To 30
      Iwert = 0
      For j = 1 To 4
        Iwert = Iwert + (256 ^ (j - 1)) * CByte("&H" & Mid(BUF, i * 8 + (j - 1) * 2 + 1, 2))
      Next j
      rr(i) = CSng(Iwert) / 327.68
    Next i
    If MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = rr(2 * i)
      Next i
    Else
      For i = 0 To MessgNwe - 1
        r(i) = rr(i)
      Next i
    End If
    '
    '
  End Sub

  Sub XkUhex(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef ier As Short)
    Dim i As Short
    Dim j As Short
    Dim BChar As String
    Dim Iwert As Integer
    Dim rr(31) As Single
    '
    'Statuszeile entfernen
    '
    If Len(BUF) < 190 Or Len(BUF) > 200 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    '
    '
    '
    '
    BChar = ""
    For i = 1 To Len(BUF)
      If Mid(BUF, i, 1) <> " " Then
        BChar = BChar & Mid(BUF, i, 1)
      End If
    Next i
    BUF = BChar
    For i = 0 To 30
      Iwert = 0
      For j = 1 To 2
        Iwert = Iwert + (256 ^ (j - 1)) * CByte("&H" & Mid(BUF, i * 4 + (j - 1) * 2 + 1, 2))
      Next j
      rr(i) = CSng(Iwert) / 327.68
    Next i
    If MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = rr(2 * i)
      Next i
    Else
      For i = 0 To MessgNwe - 1
        r(i) = rr(i)
      Next i
    End If

    '
    '
  End Sub


  Sub XKVrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef ier As Short)
    Dim i As Short
    Dim j As Short
    Dim BChar As String
    Dim Iwert As Integer
    Dim Rh(31) As Single
    '
    'Statuszeile entfernen
    '
    If Len(BUF) < 380 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    '
    '
    '
    '
    BChar = ""
    For i = 1 To Len(BUF)
      If Asc(Mid(BUF, i, 1)) = 13 Then
        MsgBox("Carriage return")
      End If
      If Asc(Mid(BUF, i, 1)) = 10 Then
        MsgBox("Carriage return")
      End If

      If Mid(BUF, i, 1) <> " " And Asc(Mid(BUF, i, 1)) <> 13 Then
        BChar = BChar & Mid(BUF, i, 1)
      End If
    Next i
    BUF = BChar
    For i = 0 To 30
      Iwert = 0
      For j = 1 To 4
        If j = 3 Or j = 4 Then
          Iwert = Iwert + (256 ^ (j - 1)) * CByte("&H" & Mid(BUF, i * 8 + (j - 1) * 2 + 1, 2))
        End If
      Next j


      Rh(i) = CSng(Iwert / 65536) / 327.68
    Next i
    If MessgNwe = 31 Then
      For i = 0 To MessgNwe - 1
        r(i) = Rh(i)
      Next i
    ElseIf MessgNwe = 16 Then
      For i = 1 To MessgNwe - 1
        r(i) = Rh(2 * i)
      Next i
    End If

    '
    '
  End Sub


  Sub X45rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef ier As Short)
    Dim i As Short
    Dim Ic As Short
    Dim BChar As String
    Dim Rh(31) As Single
    '
    'Statuszeile entfernen
    '
    If Len(BUF) < 386 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    '
    '
    '
    For i = 1 To Len(BUF)
      Ic = Asc(Mid(BUF, i, 1))
      If Ic = 13 Or Ic = 9 Or Ic = 10 Or Ic > 64 Then
        Mid(BUF, i, 1) = Space(1)
      End If
    Next i
    For i = 0 To 30
      BChar = Mid(BUF, i * 11 + 1, 11)
      Rh(i) = Val(Mid(BChar, 2))
    Next i
    If MessgNwe = 31 Then
      For i = 0 To MessgNwe - 1
        r(i) = Rh(i)
      Next i
    ElseIf MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = Rh(2 * i)
      Next i
    End If




  End Sub






 
  Sub New()
    MyBase.New()
    istat = 0
    MnReTr = 0

  End Sub
  Sub dispose() Implements IDisposable.Dispose
  End Sub
End Class