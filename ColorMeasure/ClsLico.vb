Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsLico
  Implements IDisposable

  '
  '
  'Perkin Elmer Lambda 2
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


  Sub Licini(ByRef istat As Short)

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
    idka(0) = 1
    idka(1) = 0
    idka(2) = 0
    If MessgDriver = "LIC" Then
      If NormfileID <> 3 Then
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
    MNbtnSTR.Visible = False
    MNbtnABR.Visible = False
    MNbtnMIT.Visible = False
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
        If MessgDriver = "LIC" Then
          Call LicMes(0, kw, rmes, istat, MscMes)
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



  Sub Licrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim i As Short
    Dim j As Short
    Dim k As Short
    Dim rr(31) As Single
    Dim Hlf As String
    '
    'Statuszeile entfernen
    '
    '
    If Len(BUF) < 256 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    '
    '       380-720 nm (10nm)
    '
    '
    '
    '
    'Ersetze Sonderzeichen
    '
    For i = 1 To Len(BUF)
      If Mid(BUF, i, 1) = Chr(6) Then Mid(BUF, i, 1) = " "
      If Mid(BUF, i, 1) = Chr(2) Then Mid(BUF, i, 1) = " "
      If Mid(BUF, i, 1) = Chr(3) Then Mid(BUF, i, 1) = " "
    Next i
    '
    '
    '
    k = 1
    '
    '
    'Suche Punkt
    For i = 1 To MessgNwe
      j = InStr(Mid(BUF, k), ".")
      k = k + j
      Hlf = Mid(BUF, k - 5, 7)
      r(i) = 0.01 * Val(Trim(Hlf))
    Next i
    '
    '
    '     R-Werte übernehmen
    '
    '
    '
    '
  End Sub



  Sub LicMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim Izaehl As Short
    Dim i As Short
    '
    '
    '
    '
    ABRchk = False
    MITchk = False

    '
    If imess = 1 Then
      '
      '
      '
      'Remote aus
      '
      '
      '
      Izaehl = 0
      BUF = ""
      Do
        stv = Chr(2) & "Z" & Chr(3)
        mscMES.WriteLine(stv)
        Call Wait(20)
        BUF = BUF & mscMES.ReadLine
        Call Wait(20)
        Izaehl = Izaehl + 1
        If InStr(BUF, Chr(3)) > 0 Then
          mscMES.WriteLine(Chr(6))
          Exit Do
        End If
        If Izaehl > 5 Then
          Exit Do
        End If
      Loop
      '
      '
      'Remote an
      '
      '
      '
      '
      '
      Izaehl = 0
      BUF = ""
      Do
        stv = Chr(2) & "R" & Chr(3)
        mscMES.WriteLine(stv)
        Call Wait(20)
        BUF = BUF & mscMES.ReadLine
        Call Wait(20)
        Izaehl = Izaehl + 1
        If InStr(BUF, Chr(3)) > 0 Then
          mscMES.WriteLine(Chr(6))
          Exit Do
        End If
        If Izaehl > 5 Then
          GoTo ERRLic
        End If
      Loop
      '
      '
      'Version
      '
      '
      '
      '
      '
      Izaehl = 0
      BUF = ""
      Do
        stv = Chr(2) & "V" & Chr(3)
        mscMES.WriteLine(stv)
        Call Wait(20)
        BUF = BUF & mscMES.ReadLine
        Call Wait(20)
        Izaehl = Izaehl + 1
        If InStr(BUF, Chr(3)) > 0 Then
          mscMES.WriteLine(Chr(6))
          Exit Do
        End If
        If Izaehl > 5 Then
          GoTo ERRLic
        End If
      Loop
      '
      'Blitz an/aus
      '
      '
      Izaehl = 0
      BUF = ""
      Do
        stv = Chr(2) & "B" & Chr(3)
        mscMES.WriteLine(stv)
        Call Wait(20)
        BUF = BUF & mscMES.ReadLine
        Call Wait(20)
        Izaehl = Izaehl + 1
        If InStr(BUF, Chr(3)) > 0 Then
          mscMES.WriteLine(Chr(6))
          Exit Do
        End If
        If Izaehl > 5 Then
          GoTo ERRLic
        End If
      Loop

      '
      'Kalibrieren
      '
      '
      Izaehl = 0
      BUF = ""
      stv = Chr(2) & "CH" & Chr(3)
      mscMES.WriteLine(stv)
      Call Wait(10)
      mscMES.WriteLine(Chr(6))
      Izaehl = 0
      BUF = ""
      Do
        Call Wait(20)
        Izaehl = Izaehl + 1
        If Izaehl > 100 Then GoTo ERRLic
        If mscMES.ReadBufferSize > 0 Then
          BUF = BUF & mscMES.ReadLine
          mscMES.WriteLine(Chr(6))
        End If
        If InStr(BUF, "o") > 0 Then
          Exit Do
        End If
      Loop
    End If
    '
    '
    '
    '
    'Messen
    '

    '
    If ABRchk Then
      istat = -1
      Exit Sub
    End If

    '
    Izaehl = 0
    BUF = ""
    stv = Chr(2) & "MM" & Chr(3)
    mscMES.WriteLine(stv)
    Call Wait(10)
    mscMES.WriteLine(Chr(6))
    Izaehl = 0
    BUF = ""
    Do
      Call Wait(20)
      If ABRchk Then
        istat = -1
        Exit Sub
      End If
      If MITchk Then
        istat = 1
        Exit Sub
      End If
      If InStr(BUF, "err") > 0 Then
        istat = -1
        MsgBox(Texxt(3506))
        Exit Sub
      End If
      Izaehl = Izaehl + 1
      If Izaehl > 100 Then GoTo ERRLic
      If mscMES.ReadBufferSize > 0 Then
        BUF = BUF & mscMES.ReadLine
        mscMES.WriteLine(Chr(6))
      End If
      If InStr(BUF, "o") > 0 Then
        Exit Do
      End If
    Loop
    '
    '
    '

    Call Licrwert(0, BUF, r, istat)
    If istat <> NuWrt Then Exit Sub
    If imess = 1 Then
      For i = 0 To MessgNwe - 1
        r(i) = r(i) * 100.0#
      Next i
    End If
    Exit Sub
ERRLic:
    MsgBox(Texxt(3500))
    istat = -1
    '
  End Sub


  Sub MesIni()
    istat = NuWrt
    If MessgDriver = "LIC" Then
      Call Licini(istat)
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
      For kw = 1 To MessgKM
        If MessgDriver = "LIC" Then
          Call LicMes(imess, kw, rmes, istat, MscMes)
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