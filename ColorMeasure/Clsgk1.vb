Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsGK1
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
    If MessgDriver = "GK1" Then
      If NormfileID <> 7 And NormfileID <> 1 Then
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
    MNbtnWIN.Visible = True
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
    MNlblANZ.Visible = False
    MNbtnWIN.Visible = True
    '
    '
    Do While MesZae < Mesmax
      NXTChk = False
      MNbtnNXT.Enabled = False
      MesZae = MesZae + 1
      For kw = 0 To MessgKM - 1
        MNbtnWIN.Text = Texxt(485) & " (" & MessgChrm(kw) & ")"
        MNlblANZ.Text = LblAnze(MesZae, Mesmax) & " (" & MessgChrm(kw) & ")"
        MNlblANZ.Visible = True
        MNbtnWIN.Enabled = True
        MNbtnWIN.Focus()
        Do
          System.Windows.Forms.Application.DoEvents()
          If WinChk Then
            WinChk = False
            Exit Do
          End If
        Loop
        MNbtnWIN.Enabled = False
        '
        '
        '
        '
        '  Messung starten
        '
        '
        '
        '
        Call GK1Mes(0, kw, rmes, istat, MscMes)
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
    'Einstellungen beim Initialisieren des Messgerätes
    '
    '
  End Sub



  Sub GK1rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim i As Short
    Dim j As Short
    Dim k As Short
    Dim tex As String
    Dim Smes As Single
    Dim sav(5) As Single
    Dim re(40) As Single
    sav(1) = -0.0857143
    sav(2) = 0.342857
    sav(3) = 0.485714
    sav(4) = 0.342857
    sav(5) = -0.0857143
    '
    'Statuszeile entfernen
    '
    '
    If Len(BUF) < 286 Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    '
    For i = 0 To 39
      tex = BUF.Substring(4 + i * 7, 7)
      re(i) = Val(tex)
    Next i
    '
    '       400-700nm (20nm)
    '
    If NormfileID = 1 Then
      j = -1
      For i = 2 To 39 Step 2
        j = j + 1
        Smes = 0.0#
        For k = 1 To 5
          Smes = Smes + sav(k) * re(i - 3 + k)
        Next k
        r(j) = Smes
        If j = MessgNwe - 1 Then Exit For
      Next i
    Else
      '
      '       380-770nm (10nm)
      '

      For i = 0 To MessgNwe - 1
        r(i) = re(i)
      Next i
    End If

  End Sub

  Sub GK1Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim STW As String
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    '
    Dim XON As String
    Dim ENQ As String
    Dim ACK As String
    Dim IBASF As Short
    CrLf = Chr(10)
    buflen = 2
    iver = 5
    wart = 150
    delay = 30
    resp = 1
    ENQ = Chr(5)
    ACK = Chr(6)
    XON = Chr(17)
    CHRM = MessgChrm(kw)


    '
    If imess = 7 Then
      If kw = 0 Then
        '         Null-Kalibrieren
        '
        'Reset
        '
        '
        stv = ENQ
        Call GetComBuf(resp, stv, BUF, 1, iver, delay, 10, 3506, 1, "", 0, ACK, mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        STW = "IN1"
        stv = STW & CrLf
        Call GetComBuf(resp, stv, BUF, 0, iver, delay, wart, 3506, 0, "", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        stv = XON
        Call GetComBuf(resp, stv, BUF, 0, iver, delay, 10, 3506, 1, STW, 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        STW = "MD"
        stv = STW & CrLf
        Call GetComBuf(2, stv, BUF, 3, iver, delay, 2 * wart, 3506, 1, STW, 0, "", mscMES, istat)
        '
        If istat <> NuWrt Then Exit Sub
        '
      End If
      For i = 0 To MessgNwe - 1
        r(i) = 0.0#
      Next i
    ElseIf imess = 6 Then
      If kw = 0 Then

        '
        stv = "MR" & CrLf
        Call GetComBuf(2, stv, BUF, 3, iver, delay, 2 * wart, 3506, 1, "MR", 0, "", mscMES, istat)
        '
        If istat <> NuWrt Then Exit Sub
        '
      End If
      For i = 0 To MessgNwe - 1
        r(i) = 100.0#
      Next i
      '
    ElseIf imess < 4 Then
      If imess <> 0 Then
        '
        '
        '         Anzahl Blitze
        '
        '
        stv = "AV01" & CrLf
        'stv = XON
        Call GetComBuf(resp, stv, BUF, 3, iver, delay, wart, 3506, 1, "AV", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '
        '         Blitzintervall
        '
        '
        stv = "AI025" & CrLf
        'stv = XON
        Call GetComBuf(resp, stv, BUF, 3, iver, delay, wart, 3506, 1, "AII", 0, "", mscMES, istat)
        If istat <> NuWrt Then Exit Sub
        '
        '
        '
        '
      End If
      '
      '
      '         Messung starten
      '
      '
      '
      '
      Call Wait(200)
      STW = "MB1"
      stv = STW & CrLf
      '
      '
      Call GetComBuf(1, stv, BUF, 285, iver, delay, 200, 3500, 1, STW, 0, "", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      IBASF = 0
      Call GK1rwert(IBASF, BUF, r, istat)
    End If
  End Sub


  Sub GK1ini(ByRef istat As Short)
    '
    '
    '
  End Sub








  Sub MesIni()
    istat = NuWrt
    Call GK1ini(istat)
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
    MesZae = 0
    Mesmax = 1
    If MessgImes > 1 Then
      Mesmax = irmax
    End If
    Call PoOpen(MscMes, iermes)
    Call NullRwrt(Mnier)
    jj = 0
    For j = 1 To irmax
      jj = jj + 1
      For kw = 0 To MessgKM - 1
        MNbtnWIN.Text = Texxt(485) & " (" & MessgChrm(kw) & ")"
        If imess < 4 Then
          MNbtnWIN.Visible = True
          MNlblANZ.Visible = True
          MNlblANZ.Text = LblAnze(jj, irmax)
          MNbtnWIN.Enabled = True
          MNbtnWIN.Focus()
          Do

            System.Windows.Forms.Application.DoEvents()
            If WinChk Then
              WinChk = False
              Exit Do
            End If
          Loop
          MNbtnWIN.Enabled = False
        End If
        '
        '
        'geräteabhängige Driver
        '
        '
        '
        Call GK1Mes(imess, kw, rmes, istat, MscMes)
        If istat = 1 Or istat = -1 Then GoTo KalEnd
        Call KalSum(kw, rsmes, rmes)
        If istat = 0 Then GoTo KalEnd
      Next kw
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