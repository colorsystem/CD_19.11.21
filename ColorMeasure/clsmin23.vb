Option Compare Text
Option Explicit On
Option Strict Off
Public Class clsmin23

  'istat = 0   kein Fehler
  'istat > 0   Fehler ist aufgetreten Programm neu starten
  'istat < 0   Fehler ist aufgetreten Programm nicht neu starten
  Private istat As Integer
  Private iermes As Integer
  Dim kw As Integer
  Dim j As Integer
  Dim i As Integer
  Dim nkw As Integer
  Dim TarMsk As String
  Dim CHRM As String
  Dim gloss As String
  Dim MnReTr As Integer
  Dim inp() As String
  Dim ID As String
  Dim WelAnz As Integer
  Dim WElStart As Single
  Dim WelEnd As Single
  Dim WelStep As Single
  Dim AbsWDevSCI() As Single
  Dim AbsWDevSCE() As Single
  Dim WelDev() As Single
  Sub MesSon()

  End Sub


  Sub MesSpei()
    Dim i As Integer
    Dim kw As Integer



    Dim Sqlstmt As String

    Dim Hlf As String
    CrLf = Chr(13) & Chr(10)
    Call PoOpen(MscMes, iermes)
    stv = "STR" & CrLf
    Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, _
    3506, 1, "", 0, "OK", MscMes, istat)
    If istat <> NuWrt Then Exit Sub

    For kw = 0 To MessgKM - 1
      CHRM = MessgChrm(kw)
      If InStr(CHRM, "0") = 0 And InStr(CHRM, "O") = 0 Then
        '
        '
        'gloss included (SCI)
        stv = "0"
      Else
        '
        '
        'gloss excluded (SCE)
        stv = "1"
      End If
      gloss = stv




      stv = "WMS,0" & CrLf
      Call GetComString(Autom, stv, BUF, 4, 100, 5, 10, _
      3506, 1, "", 1, "OK", MscMes, istat)
      If istat <> NuWrt Then Exit Sub

      stv = "CDS," & CStr(cboSPE(0).SelectedIndex) & "," & gloss & "," & ID
      j = 0
      For i = 0 To WelAnz - 1
        If Abs(MessgWsol(j) - WelDev(i)) < 0.01 Then
          Select Case gloss
            Case "0"
              AbsWDevSCI(i) = MessgQW(j + MessgNwe * kw)
            Case "1"
              AbsWDevSCE(i) = MessgQW(j + MessgNwe * kw)
          End Select
          j = j + 1
          If j = MessgNwe Then
            Exit For
          End If
        End If
      Next i


      For i = 0 To WelAnz - 1
        Select Case gloss
          Case "0"
            Hlf = Str(Format(100.0# * AbsWDevSCI(i), "00.000"))
          Case "1"
            Hlf = Str(Format(100.0# * AbsWDevSCE(i), "00.000"))
        End Select
        stv = stv & "," & Hlf
      Next i
      stv = stv + CrLf
      Call GetComString(Autom, stv, BUF, 4, 100, 5, 10, _
      3506, 1, "", 1, "OK", MscMes, istat)
      If istat <> NuWrt Then Exit Sub
      '
      '
      '
    Next kw
    stv = "WMS,1" & CrLf
    Call GetComString(Autom, stv, BUF, 4, 100, 5, 10, _
    3506, 1, "", 1, "OK", MscMes, istat)
    If istat <> NuWrt Then Exit Sub
    Call PoClose(MscMes, iermes)
    '
    '
    '
    '
    '
    Sqlstmt = "UPDATE " & TableKalib & " SET [KALIB_DATTIM]=" _
   & Sqldati(Now.AddDays(-1)) & " WHERE MESSG_ID=" & MessgID & " AND MESSG_KENN='" & MessgKenn & "'"
    CmdKal.CommandText = Sqlstmt
    If SQLExeNonQuery(CmdKal, Cnkal) <> 0 Then
      MsgBox("ERROR CNKAL")
    End If

  End Sub
  Sub MINini(ByRef istat As Integer)
    Dim i As Integer
    lblspe(0).Visible = True
    cboSPE(0).Visible = True
    cboSPE(0).Items.Clear()
    cboSPE(0).Items.Add("MAV")

    CrLf = Chr(13) & Chr(10)
    Call PoOpen(MscMes, iermes)
    stv = "IDR" & CrLf
    Call GetComString(Autom, stv, BUF, 25, 50, 10, 10, _
    3506, 1, "", 0, "OK", MscMes, istat)
    If istat <> NuWrt Then Exit Sub
    Call SplitInputString(BUF, inp, ",")
    ID = inp(3)
    WElStart = CSng(inp(5))
    WelEnd = CSng(inp(6))
    WelStep = CSng(inp(7))
    'Wellenlängen aufbauen
    WelAnz = CInt((WelEnd - WElStart) / WelStep + 0.5) + 1
    ReDim AbsWDevSCI(WelAnz - 1)
    ReDim AbsWDevSCE(WelAnz - 1)
    ReDim WelDev(WelAnz - 1)
    For i = 0 To WelAnz - 1
      WelDev(i) = WElStart + i * WelStep
    Next i

    stv = "CPR" & CrLf
    Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, _
    3506, 1, "", 0, "OK", MscMes, istat)
    If istat <> NuWrt Then Exit Sub
    Call PoClose(MscMes, iermes)
    If BUF <> "" Then
      cboSPE(0).SelectedIndex = CInt(BUF.Substring(5, 1))
    End If
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
    If MessgDriver = "MI8" Then
      If NormfileID <> 11 And NormfileID <> 1 And NormfileID <> 2 Then
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
        If MessgDriver = "MI8" Then
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






  Public Sub New()
    istat = 0
    MnReTr = 0
  End Sub
  Sub einst(ByVal Index As Integer)
    Dim i As Integer
    Dim kw As Integer
    Dim r() As Single
    ReDim r(MessgNwe)
    '
    Call PoOpen(MscMes, iermes)

    '
    Call Wait(20)
    If istat <> NuWrt Then Exit Sub
    For kw = 0 To MessgKM - 1
      CHRM = MessgChrm(kw)
      If InStr(CHRM, "0") = 0 And InStr(CHRM, "O") = 0 Then
        '
        'gloss included (SCI)
        stv = "0"
      Else
        '
        '
        'gloss excluded (SCE)
        stv = "1"
        '
      End If
      gloss = stv
      stv = "CDR" & "," & CStr(cboSPE(0).SelectedIndex) & "," & gloss & CrLf
      Call GetComString(Autom, stv, BUF, 4, 100, 5, 10, _
      3506, 1, "", 1, "OK", MscMes, istat)
      If istat <> NuWrt Then Exit Sub
      Select Case gloss
        Case "0"
          Call MINrwert(0, BUF, WelAnz, AbsWDevSCI, istat)
        Case "1"
          Call MINrwert(0, BUF, WelAnz, AbsWDevSCE, istat)
      End Select
      j = 0
      For i = 0 To WelAnz - 1
        If Abs(MessgWsol(j) - WelDev(i)) < 0.01 Then
          Select Case gloss
            Case "0"
              MessgQW(j + kw * MessgNwe) = AbsWDevSCI(i)
            Case "1"
              MessgQW(j + kw * MessgNwe) = AbsWDevSCE(i)
          End Select
          j = j + 1
          If j = MessgNwe Then
            Exit For
          End If
        End If
      Next i
    Next kw

    Call PoClose(MscMes, iermes)
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
    '
    '
    '

    '
    '

    Call SplitInputString(BUF, inp, ",")
    Dinp = UBound(inp) - 38
    If nwe = 39 Then
      For i = 0 To nwe - 1
        r(i) = 0.01 * Val(inp(i + Dinp))
      Next i
    ElseIf nwe = 16 Then
      '
      '400-700nm (20nm)
      '
      '
      For i = 0 To nwe - 1
        r(i) = 0.01 * Val(inp(Dinp + 4 + 2 * i))
      Next i
    ElseIf nwe = 31 Then
      '
      '400-700nm (10nm)
      '
      '
      For i = 0 To nwe - 1
        r(i) = 0.01 * Val(inp(4 + Dinp + i))
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
    Dim Aper As Integer
    Dim gloss As Integer
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
    CU(2) = "UZC"
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
    CHRM = MessgChrm(kw)
    If InStr(CHRM, "0") = 0 And InStr(CHRM, "O") = 0 Then
      '
      '
      'gloss included
      gloss = "0"
    Else
      '
      '
      'gloss excluded
      gloss = "1"
    End If


    If kw = 0 Then
      stv = "IDR" & CrLf
      Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, _
      3506, 1, "OK", 0, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      '
      stv = "STR" & CrLf
      Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, _
      3506, 1, "OK", 0, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
    End If
    stv = "CPR" & CrLf
    Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, _
    3506, 1, "OK", 0, "", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    Aper = CInt(Mid(BUF, 6, 1))

    '
    '
    Call Wait(10)
    stv = "CPS," & CStr(Aper) & "," & gloss & CrLf
    Call GetComString(Autom, stv, BUF, 4, iver, 2 * delay, wart, _
    3506, 1, "OK", 1, "", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    If imess = 1 Then
      '
      Call Wait(10)

      stv = "CAL" & CrLf
      Call GetComString(Autom, stv, BUF, 4, iver, 2 * delay, wart, _
      3506, 1, "OK", 1, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      For i = 0 To MessgNwe - 1
        r(i) = 100.0#
      Next i
      '
    ElseIf imess = 2 Then
      Call Wait(10)
      stv = "UZC" & CrLf
      Call GetComString(Autom, stv, BUF, 4, iver, 2 * delay, wart, _
      3506, 1, "", 1, "OK", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    Else

      '
      '
      '
      '     Messung starten
      '
      '
      '
      Call Wait(10)
      stv = "MES" & CrLf
      '
      Call GetComString(Autom, stv, BUF, 100, iver, 4 * delay, wart, _
      3500, 1, "", 0, "", mscMES, istat)
      '
      '
      '
      '
      If istat <> NuWrt Then Exit Sub
      IBASF = 0
      'Gloss=0 SCI wird gemessen
      'Gloss=1 SCI+SCE wird gemessen; nur SCE-Daten werden nach r gespeichert!!!!
      Call MINrwert(IBASF, BUF, nwe, r, istat)
    End If

  End Sub


  Sub MesIni()
    istat = NuWrt
    If MessgDriver = "MI8" Then
      Call MINini(istat)
    Else
      MsgBox(Texxt(3542) & MessgDriver)
    End If
  End Sub
  Sub MesKal(ByVal imess As Integer)
    Dim jj As Integer
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
        If MessgDriver = "MI8" Then
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




  Function TarGloss(ByVal stv As String, ByVal TarMsk As String) As String
    Dim Tarhlf As String
    Tarhlf = TarMsk
    If Tarhlf = "2" Then
      Tarhlf = "1"
    End If
    If Tarhlf = "3" Then
      Tarhlf = "0"
    End If
    TarGloss = stv & Tarhlf
    If TarMsk = "3" Then
      Mid(TarGloss, 1, 1) = "1"
    End If
  End Function


   


End Class
