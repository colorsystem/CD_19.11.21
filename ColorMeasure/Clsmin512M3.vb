Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsMIN512M3
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
  Dim TarMsk As String
  Dim CHRM As String
  Dim MnReTr As Short
  Sub MesSon()

  End Sub


  Sub MINini(ByRef istat As Short)
    Dim i As Integer
    Dim kw As Integer
    Dim r() As Single
    ReDim r(MessgNwe)
    CrLf = Chr(13)
    Call PoOpen(MscMes, iermes)
    stv = "IDR" & CrLf
    Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, 3506, 1, "", 0, "OK", MscMes, istat)
    If istat <> NuWrt Then Exit Sub
    stv = "STR" & CrLf
    Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, 3506, 1, "", 0, "OK", MscMes, istat)
    If istat <> NuWrt Then Exit Sub
    TarMsk = Mid(BUF, 8, 1)
    stv = "CDR" & CrLf & "&" & CrLf & "&" & CrLf
    Call GetComString(Autom, stv, BUF, 117, 100, 5, 10, 3506, 1, "", 1, "OK", MscMes, istat)
    For kw = 1 To MessgKM
      nkw = (kw - 1) * MessgNwe
      If istat <> -999 Then
        For i = 1 To MessgNwe
          j = nkw + i
          MessgQW(j) = 1.0#
        Next i
        istat = -999
      Else
        Call MINrwert(0, Mid(BUF, 117 * (kw - 1) + 1), r, istat)
        For i = 1 To MessgNwe
          j = nkw + i
          If (i Mod 2) = 1 Then
            MessgQW(j) = 0.01 * r(Fix(i / 2) + 1)
          End If
        Next i
        For i = 1 To MessgNwe
          j = nkw + i
          If (i Mod 2) = 0 Then
            MessgQW(j) = 0.5 * (MessgQW(j - 1) + MessgQW(j + 1))
          End If
        Next i
      End If
    Next kw
    Call PoClose(MscMes, iermes)

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
    idka(0) = 1
    idka(1) = 2
    idka(2) = 0
    If MessgDriver = "MI5" Then
      If NormfileID <> 2 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
      idka(1) = 2
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
      For kw = 1 To MessgKM
        If MessgDriver = "MI5" Then
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



  Sub MINrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim i As Short
    Dim j As Short
    Dim k As Short
    Dim tex As String
    Dim rr(31) As Single
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
    j = 5
    For i = 1 To 31
      tex = Mid(BUF, j + 1, 6)
      rr(i) = Val(tex)
      k = InStr(Mid(BUF, j + 1), ",")
      j = j + k
    Next i
    If MessgNwe = 16 Then
      For j = 1 To MessgNwe
        r(j) = rr(2 * (j - 1) + 1)
      Next j
    Else
      For j = 1 To MessgNwe
        r(j) = rr(j)
      Next j
    End If
  End Sub



  Sub MinMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim i As Short
    Dim j As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim CU(6) As String
    Dim Win(3) As String
    Dim Sett(3) As String
    Dim Iwin As Short
    Dim Ibuf As Short
    Dim Ipoint(3) As Short
    Static Rhilf(93) As Single




    '
    Dim IBASF As Short
    CrLf = Chr(13)
    buflen = 2
    iver = 2000
    wart = 100
    delay = 10
    CU(0) = "MES"
    CU(1) = "CAL"
    CU(2) = "UZC"
    CU(3) = " "
    CU(4) = " "
    CU(5) = " "
    CU(6) = " "
    '
    '
    '
    Win(1) = "25"
    Win(2) = "45"
    Win(3) = "75"



    '
    '
    '
    Sett(1) = "A"
    Sett(2) = "B"
    Sett(3) = "C"
    '
    '
    '
    Ipoint(1) = 12
    Ipoint(2) = 291
    Ipoint(3) = 570
    Iwin = 0
    For i = 1 To 3
      If Trim(MessgChrm(kw)) = Win(i) Then
        Iwin = i
        Exit For
      End If
    Next i
    '     Null-Kalibrieren nicht unterst¸tzt
    '
    '
    '
    '      If imess = 3 Or imess = 4 Or imess = 7 Then
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
    If kw = 1 Then
      Call Wait(100)

      stv = "IDR" & CrLf
      Call GetComString(Autom, stv, BUF, 0, 50, 5, 10, 3506, 1, "", 0, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
    End If
    Select Case imess
      Case 1
        If kw = 1 Then
          '
          '           Zustand abfragen
          '
          stv = "IDR" & CrLf
          Call GetComString(Autom, stv, BUF, 12, 10, delay, 5, 3506, 1, "", 1, "OK", mscMES, istat)
          If istat <> NuWrt Then Exit Sub
          stv = CU(imess) & CrLf
          Call GetComString(Autom, stv, BUF, 4, iver, delay, 10 * wart, 3500, 1, "", 1, "", mscMES, istat)
          If istat <> NuWrt Then Exit Sub
        End If
        For i = 1 To MessgNwe
          r(i) = 100.0#
        Next i
      Case 2
        If kw = 1 Then
          '
          '           Zustand abfragen
          '
          stv = "IDR" & CrLf
          Call GetComString(Autom, stv, BUF, 12, 10, 5, 5, 3506, 1, "", 1, "OK", mscMES, istat)

          If istat <> NuWrt Then Exit Sub
          stv = CU(imess) & CrLf
          Call GetComString(Autom, stv, BUF, 4, iver, delay, 10 * wart, 3500, 1, "", 1, "", mscMES, istat)
          If istat <> NuWrt Then Exit Sub
        End If
        For i = 1 To MessgNwe
          r(i) = 0.0#
        Next i
      Case 0
        If kw = 1 Then
          '
          '           Zustand abfragen
          '
          stv = "IDR" & CrLf
          Call GetComString(Autom, stv, BUF, 12, 10, delay, 5, 3506, 1, "", 1, "OK", mscMES, istat)

          If istat <> NuWrt Then Exit Sub
          stv = CU(imess) & CrLf & "&" & CrLf & "&" & CrLf & "&" & CrLf & "&" & CrLf & "&" & CrLf & "&" & CrLf
          Call GetComString(Autom, stv, BUF, 4, 1, delay, 10 * wart, 3500, 1, "", 1, "OK", mscMES, istat)
          If istat <> NuWrt Then Exit Sub
          For j = 1 To MessgKM
            Ibuf = Ipoint(j) + InStr(Mid(BUF, Ipoint(j)), "OK") - 1
            Call MINrwert(IBASF, Mid(BUF, Ibuf), r, istat)
            For i = 1 To MessgNwe
              Rhilf((j - 1) * MessgNwe + i) = r(i)
            Next i
          Next j

        End If
        For i = 1 To MessgNwe
          r(i) = 0.01 * Rhilf((Iwin - 1) * MessgNwe + i)
        Next i
    End Select

    '
    '
  End Sub
  Sub MesSpei()
    Dim i As Integer
    Dim kw As Integer
    Dim Hlf As String
    CrLf = Chr(13)
    Call PoOpen(MscMes, iermes)
    stv = "STR" & CrLf
    Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, 3506, 1, "", 0, "OK", MscMes, istat)
    If istat <> NuWrt Then Exit Sub
    TarMsk = Mid(BUF, 8, 1)
    stv = "CDS"
    For kw = 1 To MessgKM
      If kw > 1 Then
        stv = stv & "&"
      End If
      For i = 1 To MessgNwe Step 2
        nkw = (kw - 1) * MessgNwe
        Hlf = Format(100.0# * MessgQW(nkw + i), "000.00")
        Mid(Hlf, 4, 1) = "."
        stv = stv & "," & Hlf
      Next i
      stv = stv & CrLf
    Next kw
    Call GetComString(Autom, stv, BUF, 4, 100, 5, 10, 3506, 1, "", 1, "OK", MscMes, istat)
    If istat <> NuWrt Then Exit Sub

    Call PoClose(MscMes, iermes)
  End Sub



  Sub MesIni()
    istat = NuWrt
    If MessgDriver = "MI5" Then
      Call MINini(istat)
    Else
      MsgBox(Texxt(3542) & MessgDriver)
    End If
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
      'ger‰teabh‰ngige Driver
      '
      '
      '
      For kw = 1 To MessgKM
        If MessgDriver = "MI5" Then
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