Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsPerElm
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


  Sub PE1ini(ByRef istat As Short)

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
    If MessgDriver = "PE1" Then
      If NormfileID <> 10 And NormfileID <> 17 Then
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
        If MessgDriver = "PE1" Then
          Call PE1Mes(0, kw, rmes, istat, MscMes)
        Else
          MsgBox(Texxt(3542) & MessgDriver)
        End If
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
  End Sub



  Sub PE1rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    '      CHARACTER*1 LF,STE,FR
    Dim i As Short
    Dim j As Short
    Dim k As Short
    Dim NumCR As Short
    Dim rr(31) As Single
    Dim Hlf As String
    '
    'Statuszeile entfernen
    '
    '       MsgBox BUF
    'MsgBox Len(BUF)
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
    'MsgBox Len(BUF)

    '
    '       360-750 nm (5nm)
    '
    '
    '
    '
    'Suche 3. tes CR
    '
    '
    '
    '
    'MsgBox BUF
    NumCR = 0
    For i = 1 To Len(BUF)
      If Mid(BUF, i, 1) = Chr(13) Then
        NumCR = NumCR + 1
      End If
      If NumCR = 2 Then
        k = i
        Exit For
      End If
    Next i
    '
    '
    '
    '     R-Werte übernehmen
    '
    '
    '
    j = 0
    'MsgBox Mid(BUF, k)
    Hlf = ""
    For i = k To Len(BUF)
      If Mid(BUF, i, 1) = Chr(127) Or Mid(BUF, i, 1) = Chr(10) Or Mid(BUF, i, 1) = Chr(13) Or Mid(BUF, i, 1) = "," Then
        If Hlf <> "" Then
          If Hlf = "32767" Then
            Exit For
          Else
            j = j + 1
            If j > MessgNwe Then
              Exit Sub
            End If
            'MsgBox BUF
            r(MessgNwe - j + 1) = Val(Hlf)
            Hlf = ""
          End If
        End If
      Else
        Hlf = Hlf & Mid(BUF, i, 1)

      End If
    Next i
    If j < MessgNwe Then
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
        istat = -1
      Else
        istat = 1
      End If
      Exit Sub
    End If
    '
  End Sub



  Sub PE1Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    CrLf = Chr(13)
    buflen = 4
    iver = 50
    wart = 5
    delay = 5
    '
    '
    '
    'Responds Mode Freemode
    '
    '
    '
    stv = "$RE 0" & CrLf
    Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, "", 1, Chr(127) & "0000", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    '
    '
    '
    'Wavelength low
    '
    '
    '
    stv = "$AL 360" & CrLf
    Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, "", 1, Chr(127) & "0000", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    '
    'Wavelength hight
    '
    '
    '
    stv = "$AH 750" & CrLf
    Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, "", 1, Chr(127) & "0000", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    '
    'Data intervall
    '
    '
    '
    stv = "$DI 5.0" & CrLf
    Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, "", 1, Chr(127) & "0000", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    '
    'Messung der Transmission
    '
    '
    '
    stv = "$SB 0" & CrLf
    Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, "", 1, Chr(127) & "0000", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    '
    '
    '
    'Slit width 2nm
    '
    '
    '
    'Stv = "$SL 200" & CrLf
    'Call GetComString(Autom, Stv, BUF, buflen, iver, 10 * delay, 10 * wart, _
    ''3500, 1, "", 1, Chr(127) & "0000", mscMES , istat)
    'If istat <> NuWrt Then Exit Sub
    '
    '
    '
    '
    'Messung starten
    '
    '
    '
    stv = "$SC" & CrLf
    Call GetComString(Autom, stv, BUF, 200, 20000, 100 * delay, wart, 3500, 1, "", 1, Chr(127) & "0000", mscMES, istat)
    If istat <> NuWrt Then Exit Sub
    'MsgBox Asc(Mid(BUF, 1, 1))
    'MsgBox BUF
    ' MsgBox Len(BUF)
    Call PE1rwert(0, BUF, r, istat)
    If istat <> NuWrt Then Exit Sub

    '
  End Sub


  Sub MesIni()
    istat = NuWrt
    If MessgDriver = "PE1" Then
      Call PE1ini(istat)
    Else
      MsgBox(Texxt(3542) & MessgDriver)
    End If
  End Sub

  Sub MesKal(ByRef imess As Short)
    Dim j As Integer
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
    For j = 1 To irmax
      '
      '
      'geräteabhängige Driver
      '
      '
      '
      For kw = 1 To MessgKM
        If MessgDriver = "PE1" Then
          Call PE1Mes(imess, kw, rmes, istat, MscMes)
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