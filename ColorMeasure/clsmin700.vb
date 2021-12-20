Option Compare Text
Option Explicit On
Option Strict Off
Public Class clsmin700
  'istat = 0   kein Fehler
  'istat > 0   Fehler ist aufgetreten Programm neu starten
  'istat < 0   Fehler ist aufgetreten Programm nicht neu starten
  Private istat As Integer
  Private iermes As Integer
  Dim kw As Integer
  Dim j As Integer
  Dim i As Integer
  Dim nkw As Integer
  Dim CHRM As String
  Dim MnReTr As Integer
  Sub MesSon()

  End Sub


  Sub MesSpei()

    MsgBox(Texxt(4690))

  End Sub

  Sub SetIdka(ByVal MessgMenue As Integer)
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
    idka(0) = 2
    idka(1) = 1
    idka(2) = 0
    If MessgDriver = "MI9" Then
      If NormfileID <> 2 And NormfileID <> 1 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
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
        If MessgDriver = "MI9" Then
          Call MinMes(0, kw, rmes, istat, MscMes)
        Else
          MsgBox(Texxt(3542) & MessgDriver)
        End If
        If istat <> NuWrt Then Exit For

        Call NormRwrt(-1, kw, rmes)
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

        Application.DoEvents()
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

  Sub einst(ByVal Index As Integer)


  End Sub



  Sub MINrwert(ByVal IBASF As Integer, ByVal BUF As String, ByVal nwe As Integer, ByRef r() As Single, ByRef istat As Integer)
    '      CHARACTER*1 LF,STE,FR
    Dim i As Integer
    Dim Dinp As Integer
    Dim inp() As String
    '
    'Statuszeile entfernen
    '
    '
    If Len(BUF) < 100 Then
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

    '
    '

    Call SplitInputString(BUF, inp, ",")
    Dinp = UBound(inp) - 30
    If nwe = 31 Then
      For i = 0 To nwe - 1
        r(i) = 0.0001 * Val(inp(i + Dinp))
      Next i
    ElseIf nwe = 16 Then
      '
      '400-700nm (20nm)
      '
      '
      For i = 0 To nwe - 1
        r(i) = 0.0001 * Val(inp(Dinp + 2 * i))
      Next i
    End If
  End Sub




  Sub MinMes(ByVal imess As Integer, ByVal kw As Integer, ByRef r() As Single, ByRef istat As Integer, _
  ByVal mscMES As SerialPort)
    Dim i As Integer
    Dim iver As Integer
    Dim buflen As Integer
    Dim wart As Integer
    Dim delay As Integer
    Dim Aper As String
    Dim Gloss As Integer
    Dim nwe As Integer
    Dim CU(0 To 6) As String
    '
    Dim IBASF As Integer
    nwe = MessgNwe
    CrLf = Chr(13) + Chr(10)
    buflen = 2
    iver = 2000
    wart = 10
    delay = 10
    CU(0) = "MES"
    CU(1) = "CAL"
    CU(2) = "ZRC"
    CU(3) = " "
    CU(4) = " "
    CU(5) = " "
    CU(6) = " "
    '
    '     Null-Kalibrieren nicht unterstützt
    '
    '
    '
    '      If imess = 3 Or imess = 4 Or imess = 7 Then
    If imess > 3 Then
      Exit Sub
    End If
    '



    If kw = 0 And imess = idka(0) Then
      stv = "IDR" & CrLf
      Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, _
      3506, 1, "OK", 0, "", mscMES, istat)
      'MsgBox BUF
      If istat <> NuWrt Then Exit Sub
      '
      stv = "STR" & CrLf
      Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, _
      3506, 1, "OK", 0, "", mscMES, istat)
      'MsgBox BUF
      If istat <> NuWrt Then Exit Sub
      stv = "CPR" & CrLf
      Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, _
      3506, 1, "OK", 0, "", mscMES, istat)
      'MsgBox BUF
      If istat <> NuWrt Then Exit Sub
      Aper = Mid(BUF, 6, 1)

      CHRM = MessgChrm(kw)
      '       MsgBox CHRM & CStr(messg_ihrm(kw)), 0

      If MessgKM = 2 Then
        Gloss = "3"
      Else
        If InStr(CHRM, "0") = 0 And InStr(CHRM, "O") = 0 Then
          '
          '
          'gloss included
          Gloss = "1"
        Else
          '
          '
          'gloss excluded
          Gloss = "2"
        End If
      End If
      Call Wait(10)
      stv = "CPS," & Aper & "," & Gloss & ",20,1,1" & CrLf
      Call GetComString(Autom, stv, BUF, 4, iver, 2 * delay, wart, _
      3506, 1, "OK", 1, "", mscMES, istat)
      '     MsgBox BUF
      If istat <> NuWrt Then Exit Sub
      '
      '
      '
      stv = "STR" & CrLf

      Call GetComString(Autom, stv, BUF, 2, iver, 2 * delay, 6 * wart, _
      3500, 1, "OK", 0, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
    End If
    '
    'Weißstandard
    '
    '
    '
    If imess = 1 Then
      If kw = 0 Then
        '
        'Weißkalibrierung
        '
        '
        '
        '
        Call Wait(10)

        stv = "CAL" & CrLf
        Call GetComString(Autom, stv, BUF, 2, iver, delay, 6 * wart, _
        3506, 1, "OK", 1, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
      End If
      For i = 0 To MessgNwe - 1
        r(i) = 100.0#
      Next i
      '
    ElseIf imess = 2 Then
      '
      'Scwarzkalibrierung
      '
      '
      '
      '
      If kw = 0 Then

        Call Wait(10)
        stv = "ZRC" & CrLf
        Call GetComString(Autom, stv, BUF, 4, iver, delay, 6 * wart, _
        3506, 1, "", 1, "OK", mscMES, istat)
        'MsgBox BUF
        If istat <> NuWrt Then Exit Sub
      End If
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 0 Then
      CHRM = MessgChrm(kw)
      '
      '
      If InStr(CHRM, "0") = 0 And InStr(CHRM, "O") = 0 Then
        '
        '
        'gloss included
        Gloss = "1"
      Else
        '
        '
        'gloss excluded
        Gloss = "2"
      End If

      '
      '
      '
      'Messung starten
      '
      '
      '
      If kw = 0 Then
        '
        '
        Call Wait(10)
        stv = "MES,1" & CrLf
        '
        Call GetComString(Autom, stv, BUF, 2, iver, 3 * delay, 6 * wart, _
         3500, 1, "OK", 1, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        Call Wait(400)

        stv = "STR" & CrLf

        Call GetComString(Autom, stv, BUF, 2, iver, 10 * delay, 6 * wart, _
        3500, 1, "", 0, "OK", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
      End If
      '
      stv = "MDR," & Gloss & CrLf
      '
      Call Wait(10)
      Call GetComString(Autom, stv, BUF, 100, iver, delay, 6 * wart, _
      3500, 1, "OK", 1, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      'MsgBox BUF
      '
      If istat <> NuWrt Then Exit Sub
      IBASF = 0
      'Gloss="1" SCI wird gemessen
      'Gloss="2" SCE wird gemessen
      Call MINrwert(IBASF, BUF, nwe, r, istat)
    End If

  End Sub


  Sub MesIni()
    istat = NuWrt
    MNflgkal.Visible = False
  End Sub
  Sub MesKal(ByVal imess As Integer)
    Dim jj As Integer
    Dim kw As Integer
    Dim j As Integer
    istat = NuWrt
    WinChk = False
    NXTChk = False
    COMchk = False
    MNbtnABR.Visible = True
    MNbtnABR.Enabled = True
    Call PoOpen(MscMes, iermes)
    If iermes <> 0 Then
      Exit Sub
    End If
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
        If MessgDriver = "MI9" Then
          Call MinMes(imess, kw, rmes, istat, MscMes)
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
 




  Public Sub New()
    istat = 0
    MnReTr = 0
  End Sub
End Class
