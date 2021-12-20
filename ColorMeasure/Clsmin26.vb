Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsMIN26
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


  Sub MesSpei()
    Dim i As Integer
    Dim kw As Integer
    Dim Hlf As String
    CrLf = Chr(13)
    Call PoOpen(MscMes, iermes)
    stv = "STR" & CrLf
    Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, 3506, 1, "", 0, "OK", MscMes, istat)
    'MsgBox BUF
    If istat <> NuWrt Then Exit Sub
    TarMsk = Mid(BUF, 8, 1)
    For kw = 0 To MessgKM - 1
      CHRM = MessgChrm(kw)
      If InStr(CHRM, "0") = 0 And InStr(CHRM, "O") = 0 Then
        '
        '
        'gloss included
        stv = "0"
      Else
        '
        '
        'gloss excluded
        stv = "1"
      End If
      stv = "CDS," & TarGloss(stv, TarMsk)
      For i = 0 To MessgNwe
        nkw = (kw - 1) * MessgNwe - 1
        Hlf = Format(10.0# * MessgQW(nkw + i), "00.000")
        Mid(Hlf, 3, 1) = "."
        'Hlf = " 9.999"
        stv = stv & "," & Hlf
      Next i
      stv = stv & CrLf
      Call GetComString(Autom, stv, BUF, 4, 100, 5, 10, 3506, 1, "", 1, "OK", MscMes, istat)
      'MsgBox BUF
      If istat <> NuWrt Then Exit Sub
    Next kw
    Call PoClose(MscMes, iermes)


  End Sub
  Sub MINini(ByRef istat As Short)
    lblspe(0).Visible = True
    cboSPE(0).Visible = True
    cboSPE(0).Items.Clear()
    cboSPE(0).Items.Add("MAV")
    cboSPE(0).Items.Add("SAV")

    CrLf = Chr(13) & Chr(10)
    Call PoOpen(MscMes, iermes)
    stv = "IDR" & CrLf
    Call GetComString(Autom, stv, BUF, 25, 50, 10, 10, 3506, 1, "", 0, "OK", MscMes, istat)
    'MsgBox BUF
    If istat <> NuWrt Then Exit Sub
    stv = "CPR" & CrLf
    Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, 3506, 1, "", 0, "OK", MscMes, istat)
    'MsgBox BUF
    If istat <> NuWrt Then Exit Sub
    Call PoClose(MscMes, iermes)
    cboSPE(0).SelectedIndex = CShort(Mid(BUF, 6, 1))
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
    idka(0) = 2
    idka(1) = 1
    idka(2) = 0
    If MessgDriver = "MI6" Then
      If NormfileID <> 11 Then
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
        If MessgDriver = "MI6" Then
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
    Dim i As Integer
    Dim kw As Integer
    Dim r() As Single
    ReDim r(MessgNwe)
    Dim Sqlstmt As String
    Dim buf As String

    '
    Call PoOpen(MscMes, iermes)

    '
    Call Wait(200)
    stv = "CPS," & CStr(cboSPE(0).SelectedIndex) & ",3" & CrLf
    Call GetComString(Autom, stv, buf, 4, 50, 10, 10, 3506, 1, "OK", 0, "", MscMes, istat)
    If istat <> NuWrt Then Exit Sub
    For kw = 0 To MessgKM - 1
      CHRM = MessgChrm(kw)
      If InStr(CHRM, "0") = 0 And InStr(CHRM, "O") = 0 Then
        '
        '
        'gloss included
        stv = "0"
      Else
        '
        '
        'gloss excluded
        stv = "1"
      End If
      stv = "CDR" & "," & CStr(cboSPE(0).SelectedIndex) & "," & stv & CrLf
      Call GetComString(Autom, stv, buf, 4, 100, 5, 10, 3506, 1, "", 1, "OK", MscMes, istat)
      'MsgBox BUF
      If istat <> NuWrt Then Exit Sub
      Call MINrwert(0, Mid(buf, 9), r, istat)
      nkw = (kw - 1) * MessgNwe
      For i = 1 To MessgNwe
        j = nkw + i
        MessgQW(j) = r(i)
      Next i
    Next kw

    Call PoClose(MscMes, iermes)
    Call GrdIni(MNflgkal)

    Sqlstmt = "UPDATE " & TableKalib & " SET [KALIB_DATTIM]=? WHERE MESSG_ID=" & MessgID & " AND MESSG_KENN='" & MessgKenn & "'"
    CmdKal.CommandText = Sqlstmt
    CmdKal.Parameters.Clear()
    CmdKal.Parameters.Add(New OleDbParameter("KALIB_DATTIM", OleDbType.Date))
    CmdKal.Parameters("KALIB_DATTIM").Value = Date.Now.AddDays(-1)
    If SQLExeNonQuery(CmdKal, Cnkal) <> 0 Then
      Exit Sub
    End If
  End Sub



  Sub MINrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim i As Short
    Dim j As Short
    Dim k As Short
    Dim tex As String
    Dim rr(39) As Single
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
    '       400-700nm (20nm)
    '
    '
    j = InStr(BUF, ",")
    For i = 0 To MessgNwe - 1
      tex = Mid(BUF, j + 1, 6)
      rr(i) = 0.01 * Val(tex)
      k = InStr(Mid(BUF, j + 1), ",")
      j = j + k
    Next i
    For i = 0 To MessgNwe - 1
      r(i) = rr(i)
    Next i
  End Sub



  Sub MinMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim Aper As Short
    Dim gloss As Short
    Dim CU(6) As String
    '
    Dim IBASF As Short
    CrLf = Chr(13) & Chr(10)
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
    '     Null-Kalibrieren nicht unterst¸tzt
    '
    '
    '
    '      If imess = 3 Or imess = 4 Or imess = 7 Then
    If imess > 3 Then
      Exit Sub
    End If
    '
    If kw = 0 Then
      stv = "IDR" & CrLf
      Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, 3506, 1, "OK", 0, "", mscMES, istat)
      'MsgBox BUF
      If istat <> NuWrt Then Exit Sub
      '
      stv = "STR" & CrLf
      Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, 3506, 1, "OK", 0, "", mscMES, istat)
      'MsgBox BUF
      If istat <> NuWrt Then Exit Sub
      stv = "CPR" & CrLf
      Call GetComString(Autom, stv, BUF, 4, 50, 5, 10, 3506, 1, "OK", 0, "", mscMES, istat)
      'MsgBox BUF
      If istat <> NuWrt Then Exit Sub
      Aper = CShort(Mid(BUF, 6, 1))

      '
      '       NB=1    400-700,20    NWE=16

      '
      Call Wait(10)
      stv = "CPS," & CStr(Aper) & ",3" & CrLf
      Call GetComString(Autom, stv, BUF, 4, iver, 2 * delay, wart, 3506, 1, "OK", 1, "", mscMES, istat)
      ' MsgBox BUF
      If istat <> NuWrt Then Exit Sub
    End If
    If imess = 1 Then
      '
      If kw = 0 Then
        stv = "CAL" & CrLf
        Call GetComString(Autom, stv, BUF, 2, iver, delay, 6 * wart, 3506, 1, "OK", 1, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
      End If
      For i = 0 To MessgNwe - 1
        r(i) = 100.0#
      Next i
      '
    ElseIf imess = 2 Then

      If kw = 0 Then
        stv = "UZC" & CrLf
        Call GetComString(Autom, stv, BUF, 4, iver, delay, 6 * wart, 3506, 1, "", 1, "OK", mscMES, istat)
        'MsgBox BUF
        If istat <> NuWrt Then Exit Sub
      End If
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    Else

      '
      CHRM = MessgChrm(kw)
      '       MsgBox CHRM & CStr(messg_ihrm(kw)), 0

      If InStr(CHRM, "0") = 0 And InStr(CHRM, "O") = 0 Then
        '
        '
        'gloss included
        gloss = 0
      Else
        '
        '
        'gloss excluded
        gloss = 1
      End If

      '
      '
      '     Messung starten
      '
      '
      '
      If kw = 0 Then
        stv = "MES" & CrLf
        '
        Call GetComString(Autom, stv, BUF, 100, iver, delay, wart, 3500, 1, "", 0, "", mscMES, istat)

        If istat <> NuWrt Then Exit Sub
      End If
      IBASF = 0
      Call MINrwert(IBASF, Mid(BUF, gloss * 239 + 1), r, istat)
    End If

  End Sub


  Sub MesIni()
    istat = NuWrt
    If MessgDriver = "MI6" Then
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
      For kw = 0 To MessgKM - 1
        If MessgDriver = "MI6" Then
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




  Function TarGloss(ByRef stv As String, ByRef TarMsk As String) As String
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