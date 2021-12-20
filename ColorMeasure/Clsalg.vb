Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsALG
  Implements IDisposable

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

  Sub OPInitco()
    Dim i As Integer
    Dim PUST As String
    Dim QV(16) As String
    Dim BUF As String
    Dim stv As String
    Dim CrLf As String
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    '
    CrLf = Chr(13)
    buflen = 2
    iver = 5
    wart = 5
    delay = 10
    resp = 2
    '
    '
    '
    '
    '
    'logische Kanäle 33 - 48 als verstärkte deklarieren
    '
    '
    QV(1) = "Q33,0"
    QV(2) = "Q34,0"
    QV(3) = "Q35,0"
    QV(4) = "Q36,0"
    QV(5) = "Q37,0"
    QV(6) = "Q38,0"
    QV(7) = "Q39,0"
    QV(8) = "Q40,0"
    QV(9) = "Q41,0"
    QV(10) = "Q42,0"
    QV(11) = "Q43,0"
    QV(12) = "Q44,0"
    QV(13) = "Q45,0"
    QV(14) = "Q46,0"
    QV(15) = "Q47,0"
    QV(16) = "Q48,0"
    '
    CrLf = Chr(13)
    '
    '
    '
    'Zustand abfragen
    stv = "q" & CrLf
    Call GetComBuf(resp, stv, PUST, buflen, iver, delay, wart, 3506, 1, "", 1, "?", MscMes, istat)
    '
    stv = "q" & CrLf
    Call GetComBuf(resp, stv, PUST, buflen, iver, delay, wart, 3506, 1, "", 1, "?", MscMes, istat)
    'MsgBox PUST
    If istat <> NuWrt Then
      Exit Sub
    End If


    '
    '
    stv = "T48" & CrLf
    MscMes.DiscardOutBuffer()
    MscMes.WriteLine(stv)
    '
    Call Wait(5)


    '
    '
    'Initialisieren 14-Bit Colorflash Belegung IR7 (BASF-Gerät 121)
    '
    '
    '
    stv = "i,60" & CrLf
    MscMes.DiscardOutBuffer()
    MscMes.WriteLine(stv)
    '
    Call Wait(5)


    '
    '
    stv = "o,30" & CrLf
    MscMes.DiscardOutBuffer()
    MscMes.WriteLine(stv)
    '
    Call Wait(5)

    '
    '
    stv = "f,30" & CrLf
    MscMes.DiscardOutBuffer()
    MscMes.WriteLine(stv)

    '
    Call Wait(5)

    '
    'Initialisieren der verstärkten Kanäle
    '
    For i = 1 To 16
      '
      stv = QV(i) & CrLf
      MscMes.DiscardOutBuffer()
      MscMes.WriteLine(stv)

      Call Wait(5)

      '
    Next i
    '
    '
    '
    'Grundkanäle initialisieren
    '
    '
    'Grundkanäle 400 - 540
    '
    '
    '
    stv = "U1,6,38,5,37,4,36,3,35,2,34,1,33,18,50,17,49" & CrLf
    MscMes.DiscardOutBuffer()
    MscMes.WriteLine(stv)

    '
    Call Wait(5)

    '
    'Grundkanäle 560 - 700
    '
    '
    '
    stv = "U17,16,48,15,47,14,46,13,45,64,32" & CrLf
    MscMes.DiscardOutBuffer()
    MscMes.WriteLine(stv)

    '
    Call Wait(5)

    '
    stv = "U27,63,31,62,30,61,29" & CrLf
    MscMes.DiscardOutBuffer()
    MscMes.WriteLine(stv)

    '
    Call Wait(5)

    '
    '
    'verstärkte Kanäle
    '
    '
    '
    '
    '
    stv = "U33,6,5,4,3,2,1,18,17,16,15,14,13,64,63,62,61" & CrLf
    MscMes.DiscardOutBuffer()
    MscMes.WriteLine(stv)
    '
    Call Wait(5)

    '
    '
    'Restkanäle frei
    '
    '
    ' 7,8,9,10,11,12,26,27,51,52,53,54,55,56,57,58,59,60
    '
    '
    If Mid(PUST, 1, 1) = "A" Then
      '
      '  kleine Blende
      '
      stv = "V" & CrLf
      '
    Else
      '
      '  große Blende
      '
      stv = "W" & CrLf
      '
    End If

    Call GetComBuf(resp, stv, BUF, 0, iver, delay, wart, 3506, 0, "", 0, "", MscMes, istat)

    If istat <> NuWrt Then Exit Sub
    '


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
    idka(0) = 2
    idka(1) = 1
    idka(2) = 0
    If MessgDriver = "OPR" Then
      If NormfileID <> 1 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
      If Reftra = "R" Then
        idka(0) = 2
        idka(1) = 1
        idka(2) = 0
      ElseIf Reftra = "A" Then
        idka(0) = 2
        idka(1) = 5
        idka(2) = 1
        idka(3) = 4
        idka(4) = 0
      ElseIf Reftra = "T" Then
        idka(0) = 5
        idka(1) = 4
        idka(2) = 0
      End If
    ElseIf MessgDriver = "OPT" Then
      If Reftra <> "R" Then
        MsgBox(Texxt(3590) & "  (" & Reftra & ")")
        istat = -1
        Exit Sub
      End If

      If NormfileID <> 1 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
      idka(0) = 2
      idka(1) = 1
      idka(2) = 0
    ElseIf MessgDriver = "OPI" Then
      If Reftra <> "R" Then
        MsgBox(Texxt(3590) & "  (" & Reftra & ")")
        istat = -1
        Exit Sub
      End If
      If NormfileID <> 30 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
      idka(0) = 2
      idka(1) = 1
      idka(2) = 0
    ElseIf MessgDriver = "OPQ" Then
      If Reftra <> "R" Then
        MsgBox(Texxt(3590) & "  (" & Reftra & ")")
        istat = -1
        Exit Sub
      End If
      If NormfileID <> 1 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
      idka(0) = 2
      idka(1) = 1
      idka(2) = 0
    ElseIf MessgDriver = "OPO" Then
      If Reftra <> "R" Then
        MsgBox(Texxt(3590) & "  (" & Reftra & ")")
        istat = -1
        Exit Sub
      End If
      If NormfileID <> 1 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
      idka(0) = 2
      idka(1) = 1
      idka(2) = 0
    ElseIf MessgDriver = "D38" Then
      If Reftra <> "R" Then
        MsgBox(Texxt(3590) & "  (" & Reftra & ")")
        istat = -1
        Exit Sub
      End If
      If NormfileID <> 1 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
      idka(0) = 2
      idka(1) = 1
      idka(2) = 0
    Else
      MsgBox(Texxt(3542) & MessgDriver)
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
        If MessgDriver = "OPR" Then
          Call OPRMes(0, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "OPT" Then
          Call OPRMes(0, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "OPI" Then
          Call OPRMes(0, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "OPQ" Then
          Call OPQMes(0, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "OPO" Then
          Call OPOMes(0, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "D38" Then
          Call D38Mes(0, kw, rmes, istat, MscMes)
        Else
          MsgBox(Texxt(3542) & MessgDriver)
        End If
        If istat <> NuWrt Then Exit For
        '
        '
        'Ist-Wellenlängen-Korrektur
        '
        'Call RefWsolIst(MessgNwp, rmes, MessgWist, MessgWsol)
        Call NormRwrt(MnReTr, kw, rmes)
        MNbtnMIT.Enabled = False
        MNbtnLOE.Enabled = False
      Next kw
      '
      '
      'MsgBox iami
      If istat <> NuWrt Then Exit Do
      '
      '
      '     Speichern
      '
      '
      Call RwrSpei(istat, MesZae)
      'MsgBox "nach " & iami

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


  Sub OPRrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim j As Short
    Dim Smes As Single
    Dim Sref As Single
    '
    'Statuszeile entfernen
    '
    '
    'MsgBox BUF
    '
    '
    '
    '       400-700nm (20nm)
    '
    '
    'BUF = BufFiltr(BUF)
    '        MsgBox " Länge    " & Len(BUF)
    '        MsgBox BUF
    '        For J = 1 To 16
    '           MsgBox Mid(BUF, (J - 1) * 14 + 1, 14)
    '        Next J
    '        For J = 1 To 16
    '           bbuf = ""
    '           For i = 1 To 14
    '            bbuf = bbuf & Format(Asc(Mid(BUF, (J - 1) * 14 + i, 1)), "00")
    '           Next i
    '           MsgBox bbuf
    '        Next J
    If Len(BUF) < 226 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    For j = 0 To MessgNwe - 1
      If BUF.Substring(j * 14, 1) <> "+" And BUF.Substring(j * 14, 1) <> "-" Then
        imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
        If imsg = 7 Then
          istat = -1
        Else
          istat = 1
        End If
        Exit Sub
      End If
      Smes = Val(BUF.Substring(j * 14, 7))
      Sref = Val(BUF.Substring(j * 14 + 7, 7))
      ' MsgBox Sref & "   " & Smes
      r(j) = Smes / Sref
      'MsgBox r(j)
    Next j
    '

  End Sub
  Sub D38rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim j As Short
    '
    'Statuszeile entfernen
    '
    'MsgBox BUF
    '
    If Len(BUF) < 125 Then
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
    '
    '
    '       400-700nm (20nm)
    '
    '
    For j = 0 To 15
      r(j) = Val(Mid(BUF, j * 7 + 5, 7)) * 0.001
    Next j
    '

  End Sub

  Sub OPQrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim j As Short
    Dim Smes As Single
    Dim Sref As Single
    '
    'Statuszeile entfernen
    '
    '
    'MsgBox BUF
    '
    '
    '
    '       400-700nm (20nm)
    '
    '
    'buf = BufFiltr(buf)
    For j = 0 To 15
      If Mid(BUF, j * 12 + 1, 1) <> "+" And Mid(BUF, j * 12 + 1, 1) <> "-" Then
        imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
        If imsg = 7 Then
          istat = -1
        Else
          istat = 1
        End If
        Exit Sub
      End If
      Smes = Val(Mid(BUF, j * 12 + 1, 6))
      Sref = Val(Mid(BUF, j * 12 + 7, 6))
      'MsgBox Sref & "   " & Smes
      r(j) = Smes / Sref
      'MsgBox r(j)
    Next j
    '
  End Sub
  Sub OPOrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim j As Short
    Dim Smes As Single
    Dim Sref As Single
    '
    'Statuszeile entfernen
    '
    '
    'MsgBox BUF
    '
    '
    '
    '       400-700nm (20nm)
    '
    '
    'MsgBox (BUF)
    'buf = BufFiltr(buf)
    For j = 0 To 15
      If Len(BUF) <> 144 Then
        imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
        If imsg = 7 Then
          istat = -1
        Else
          istat = 1
        End If
        Exit Sub
      End If
      Smes = Val(Mid(BUF, j * 4 + 1, 4))
      Sref = Val(Mid(BUF, j * 4 + 73, 4))
      'MsgBox Sref & "   " & Smes
      r(j) = Smes / Sref
      'MsgBox r(j)
    Next j
    '
  End Sub












  Sub OPRMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim Kwr As Short
    Dim STVWI() As String
    Dim Ccrm() As String
    Dim CCRN() As String
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    '
    Dim IBASF As Short
    Kwr = 8
    ReDim STVWI(Kwr)
    ReDim Ccrm(Kwr)
    ReDim CCRN(Kwr)

    CrLf = Chr(13)
    buflen = 2
    iver = 5
    wart = 5
    delay = 10
    resp = 2
    '
    '     MA         1. Winkel            1    110,115 Grad
    '     MB         2. Winkel            2    75      Grad
    '     MC         3. Winkel            3    70,65   Grad
    '     MD         4. Winkel            4    55      Grad
    '     ME         5. Winkel (0/45 Grad)5    45      Grad
    '     MF         6. Winkel            6    35      Grad
    '     MG         7. Winkel            7    25      Grad
    '     MH         8. Winkel            8    20      Grad
    '
    '
    STVWI(1) = "MA"
    STVWI(2) = "MB"
    STVWI(3) = "MC"
    STVWI(4) = "MD"
    STVWI(5) = "ME"
    STVWI(6) = "MF"
    STVWI(7) = "MG"
    STVWI(8) = "MH"
    '
    Ccrm(1) = "110"
    Ccrm(2) = "75"
    Ccrm(3) = "70"
    Ccrm(4) = "55"
    Ccrm(5) = "45"
    Ccrm(6) = "35"
    Ccrm(7) = "25"
    Ccrm(8) = "20"
    '
    CCRN(1) = "115"
    CCRN(2) = "75"
    CCRN(3) = "65"
    CCRN(4) = "55"
    CCRN(5) = "45"
    CCRN(6) = "35"
    CCRN(7) = "25"
    CCRN(8) = "20"
    '
    '
    '     Null-Kalibrieren nicht unterstützt
    '
    '
    '
    If imess > 5 Then
      Exit Sub
    End If
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    'Beispiel
    '
    '
    'Call GetComBuf(resp, Stv, BUF, buflen, iver, delay, wart, _
    '' 3500, CrLf, -25, "I", mscmes , ier)
    '
    CHRM = MessgChrm(kw)
    '      MsgBox CHRM & CStr(messg_ihrm(kw)), 0

    '
    If imess = 2 Then
      If kw = 0 Then
        '
        '      Zustand abfragen (bei Schwarzstandard)
        '
        stv = "q" & CrLf
        Call GetComBuf(resp, stv, BUF, buflen, iver, delay, wart, 3506, 1, "", 1, "?", mscMES, istat)
        ' MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        If MessgDriver = "OPI" Then
          '
          '
          '       mit IR-Kanälen
          '
          '
          '
          stv = "T48" & CrLf
        Else
          stv = "T32" & CrLf
        End If
        Call GetComBuf(resp, stv, BUF, 0, iver, delay, wart, 3506, 1, "", 1, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '       Teleflash
        '
        '       Blitzzeit für Teleflash einstellen
        '
        '
        '
        If MessgDriver = "OPT" Then
          stv = "i,20" & CrLf
          Call GetComBuf(resp, stv, BUF, 0, iver, delay, wart, 3506, 1, "", 1, "", mscMES, istat)
          If istat <> NuWrt Then Exit Sub
        End If

        '
      End If
    End If
    '
    '
    '
    '     Messung starten
    '
    '
    '     Messung 45 Grad(optional)
    '
    stv = "MM" & CrLf
    '
    '
    For i = 1 To Kwr
      If Trim(CHRM) = Trim(Ccrm(i)) Or Trim(CHRM) = Trim(CCRN(i)) Then
        stv = STVWI(i) & CrLf
        Exit For
      End If
    Next i
    Call GetComBuf(resp, stv, BUF, 226, 5, 20, 20, 3500, 1, "", 1, "?", mscMES, istat)
    ' MsgBox BUF
    If istat <> NuWrt Then Exit Sub
    IBASF = 0
    Call OPRrwert(IBASF, BUF, r, istat)
  End Sub

  Sub D38Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim CU(7) As String
    '
    Dim IBASF As Short
    CrLf = Chr(13) & Chr(10)
    buflen = 2
    iver = 10000
    wart = 100
    delay = 10
    CU(0) = "M"
    CU(1) = "M"
    CU(2) = "M"
    CU(3) = " "
    CU(4) = " "
    CU(5) = " "
    CU(6) = "E"
    CU(7) = "B"
    '
    '     Null-Kalibrieren nicht unterstützt
    '
    '
    '
    If imess = 3 Or imess = 4 Or imess = 8 Then
      Exit Sub
    End If
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
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
        '      Zustand abfragen (bei Schwarzstandard)
        stv = "*" & CrLf
        Call GetComString(Autom, stv, BUF, 0, iver, delay, wart, 3506, 1, "", 0, "", mscMES, istat)
        'MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub

        '
        stv = "r" & CrLf
        Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3506, 1, "", 1, "*", mscMES, istat)
        'MsgBox BUF, 0
        If istat <> NuWrt Then Exit Sub
        '
      End If
    End If
    '

    stv = "*" & CrLf
    Call GetComString(Autom, stv, BUF, 0, iver, delay, wart, 3500, 1, "", 0, "", mscMES, istat)
    'MsgBox BUF
    If istat <> NuWrt Then Exit Sub

    '
    '
    '     Messung starten
    '
    '             Call Wait(delay )

    '
    stv = CU(imess) & CrLf
    '
    Call GetComString(Autom, stv, BUF, 120, iver, delay, wart, 3500, 1, "", 1, "#120", mscMES, istat)
    'MsgBox BUF
    'MsgBox Len(BUF)

    If istat <> NuWrt Then Exit Sub
    IBASF = 0
    Call D38rwert(IBASF, BUF, r, istat)
  End Sub

  Sub OPQMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    '
    Dim IBASF As Short
    CrLf = Chr(13)
    buflen = 2
    iver = 5
    wart = 5
    delay = 10
    resp = 2
    '
    '
    '     Null-Kalibrieren nicht unterstützt
    '
    '
    '
    If imess > 3 Then
      Exit Sub
    End If
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    'Beispiel
    '
    '
    'Call GetComBuf(resp, Stv, BUF, buflen, iver, delay, wart, _
    '' 3500, CrLf, -25, "I", mscmes , ier)
    '
    CHRM = MessgChrm(kw)
    '      MsgBox CHRM & CStr(messg_ihrm(kw)), 0

    '
    If imess = 2 Then
      If kw = 0 Then
        '
        '      Zustand abfragen (bei Schwarzstandard)
        '
        Call OPInitco()
        'STV = "q" & CrLf
        'Call GetComBuf(resp, STV, BUF, buflen, iver, delay, wart, _
        ''3506, 1, "", 1, "?", mscmes , istat)
        ' MsgBox BUF, 0
        'If istat <> NuWrt Then Exit Sub
        '
      End If
    End If
    '
    '
    '
    '     Messung starten
    '
    '
    '     Messung 45 Grad(optional)
    '
    stv = "M" & CrLf
    '
    '
    Call GetComBuf(resp, stv, BUF, 10, 5, 10, 100, 3500, 1, "", 1, "?", mscMES, istat)
    'MsgBox BUF
    If istat <> NuWrt Then Exit Sub
    IBASF = 0
    Call OPQrwert(IBASF, BUF, r, istat)
  End Sub
  Sub OPOMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    '
    Dim IBASF As Short
    CrLf = Chr(13)
    buflen = 2
    iver = 5
    wart = 5
    delay = 10
    resp = 2
    '
    '
    '     Null-Kalibrieren nicht unterstützt
    '
    '
    '
    If imess > 3 Then
      Exit Sub
    End If
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    'Beispiel
    '
    '
    'Call GetComBuf(resp, Stv, BUF, buflen, iver, delay, wart, _
    '' 3500, CrLf, -25, "I", mscmes , ier)
    '
    CHRM = MessgChrm(kw)
    '      MsgBox CHRM & CStr(messg_ihrm(kw)), 0

    '
    If imess = 2 Then
      If kw = 0 Then
        '
        '      Zustand abfragen (bei Schwarzstandard)
        '
        'STV = "M" & CrLf
        'Call GetComBuf(resp, STV, BUF, buflen, iver, delay, wart, _
        ''3506, 1, "", 1, "?", mscmes , istat)
        ' MsgBox BUF, 0
        'If istat <> NuWrt Then Exit Sub
        '
      End If
    End If
    '
    '
    '
    '     Messung starten
    '
    '
    '
    '
    stv = "M" & CrLf
    '
    '
    Call GetComBuf(resp, stv, BUF, 144, 5, 100, 10, 3500, 1, "", 1, "?", mscMES, istat)
    'MsgBox BUF
    If istat <> NuWrt Then Exit Sub
    IBASF = 0
    Call OPOrwert(IBASF, BUF, r, istat)
  End Sub








  Sub OPRini(ByRef istat As Short)
    '
    Dim i As Short
    Dim kw As Short
    Dim nkw As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    Dim CrLf As String
    '
    CrLf = Chr(13)
    buflen = 2
    iver = 5
    wart = 5
    delay = 10
    resp = 2
    '
    '
    '
    '
    '
    '
    'Istwellenlängen lesen
    '
    '
    '
    Call PoOpen(MscMes, iermes)

    If iermes <> 0 Then
      Call PoClose(MscMes, iermes)
      istat = -1
      Exit Sub
    End If
    '
    'Reset
    '
    stv = "RS" & Chr(13)
    Call GetComBuf(resp, stv, BUF, buflen, iver, delay, wart, 3506, 1, "", 1, "?", MscMes, istat)
    If istat <> NuWrt Then
      Call PoClose(MscMes, iermes)
      Exit Sub
    End If
    '
    '
    'Ist-Wellenlängen übernehmen
    '
    '
    stv = "x" & Chr(13)
    Call GetComBuf(resp, stv, BUF, buflen, iver, delay, wart, 3506, 1, "", 1, "?", MscMes, istat)

    Call PoClose(MscMes, iermes)
    If istat <> NuWrt Then Exit Sub
    '
    '
    '   Ist-Wellenlängen übernehmen
    '
    '
    '
    For kw = 0 To MessgKM - 1
      nkw = kw * MessgNwe
      For i = 0 To MessgNwe - 1
        MessgWist(nkw + i) = Val(BUF.Substring(i * 8, 7))
      Next i
    Next kw
  End Sub
  Sub OPQini(ByRef istat As Short)
    '
    '
    '
    '
    '
    '
    '
    '
  End Sub
  Sub D38ini(ByRef istat As Short)
    '
    '
    '
    '
    '
    '
    '
    '
  End Sub

  Sub OPOini(ByRef istat As Short)
    '
    '
    '
    '
    '
    '
    '
    '
  End Sub








  Sub MesIni()
    istat = NuWrt
    If MessgDriver = "OPR" Then
      Call OPRini(istat)
    ElseIf MessgDriver = "OPT" Then
      Call OPRini(istat)
    ElseIf MessgDriver = "OPI" Then
      Call OPRini(istat)
    ElseIf MessgDriver = "OPQ" Then
      Call OPQini(istat)
    ElseIf MessgDriver = "OPO" Then
      Call OPOini(istat)
    ElseIf MessgDriver = "D38" Then
      Call D38ini(istat)
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
        If MessgDriver = "OPR" Then
          Call OPRMes(imess, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "OPT" Then
          Call OPRMes(imess, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "OPI" Then
          Call OPRMes(imess, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "OPQ" Then
          Call OPQMes(imess, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "OPO" Then
          Call OPOMes(imess, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "D38" Then
          Call D38Mes(imess, kw, rmes, istat, MscMes)
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





  Sub New()
    MyBase.New()
    istat = 0
    MnReTr = 0
  End Sub
  Sub dispose() Implements IDisposable.Dispose
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub
End Class