Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsEyeOne
  Implements IDisposable

  Private Iauswhl As Integer
  Private Kauswhl As Integer
  Private istat As Integer
  Private iermes As Integer
  Dim kw As Integer
  Dim j As Integer
  Dim i As Integer
  Dim nkw As Integer
  Dim MnReTr As Integer
  Dim EyeOne As Object

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

 

  Sub einst(ByRef Index As Short)
    '
    '
  End Sub



  Sub MesIni()
    istat = NuWrt
  End Sub
  Sub MesKal(ByRef imess As Integer)
    Dim kw As Integer
    Dim jj As Integer
    istat = NuWrt

    jj = EyeOne.setoption("RESET", "ALL")
    'If jj <> 0 Then
    ' istat = -1
    ' Exit Sub
    ' End If

    WinChk = False
    NXTChk = False
    COMchk = False
    MNbtnABR.Visible = True
    MNbtnABR.Enabled = True
    Call NullRwrt(Mnier)
    jj = 0
    '
    '
    'geräteabhängige Driver
    '
    '
    '
    'For j = 1 To irmax
    For kw = 0 To MessgKM - 1
      Call EyeOneMes(imess, kw, rmes, istat, MscMes)
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
      Call Wart(10)
      Application.DoEvents()
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
        Call EyeOneMes(0, kw, rmes, istat, MscMes)
        If istat <> NuWrt Then Exit For
        Call NormRwrt(MnReTr, kw, rmes)
        MNbtnMIT.Enabled = False
        MNbtnLOE.Enabled = False
      Next kw
      '
      If MITchk Then Exit Do
      '
      If MesZae > 1 Then
        MNbtnLOE.Enabled = True
      End If
      MNbtnMIT.Enabled = True
      MNbtnNXT.Enabled = True
      MNbtnNXT.Focus()

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
  End Sub
  Sub MesSon()

  End Sub
  Sub MesSpei()

  End Sub

  Sub SetIdka(ByRef MessgMenue As Integer)
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

    If NormfileID <> 1 And NormfileID <> 12 Then
      MsgBox(Texxt(3519))
      istat = -1
      Exit Sub
    End If
    idka(0) = 1
    idka(1) = 0
    idka(2) = 0
  End Sub




  Public Function Autom() As Integer
    Autom = 0
  End Function
  '


  '

  '
  Sub EyeOneMes(ByRef imess As Integer, ByRef kw As Integer, ByRef r() As Single, ByRef istat As Integer, _
                ByRef mscMES As SerialPort)
    Dim i As Integer
    Dim Result As Integer
    Dim CHRM As String
    Dim Rh(35) As Single
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        12   380-730,10    NWE=36
    '
    CHRM = MessgChrm(kw)
    '
    '     Messgerät angeschlossen
    '
    '
    If EyeOne.IsConnected <> 0 Then
      If EyeOne.GetOption("Version") = "" Then
        MsgBox(Texxt(3511))
        istat = -1
        Exit Sub
      End If
    End If


    '      STATUS ABFRAGEN
    '
    '
    If imess = 1 Or imess = 10 Then

      '      Kalibrieren (weiß)

      '
      If EyeOne.setoption("RESET", "ALL") <> 0 Then
        '
        'z.B. Driver nicht installiert
        '
        MsgBox(Texxt(3511))
        istat = -1
        Exit Sub
      End If
      If EyeOne.SetOption("MeasurementMode", "ScanningReflectance") <> 0 Then
        MsgBox(Texxt(3511))
        istat = -1
        Exit Sub
      End If


      '
      If EyeOne.Calibrate = 0 Then
        '
        For i = 0 To MessgNwe - 1
          r(i) = 1.0#
        Next i
      Else
        MsgBox(Texxt(3511))
        istat = -1
        Exit Sub
      End If
    ElseIf imess = 0 Then

      '
      '

      If EyeOne.TriggerMeasurement = 0 Then
        Result = EyeOne.GetSpectrum(Rh, 0)
      Else
        MsgBox(Texxt(3511))
        istat = -1
        Exit Sub
      End If
      If MessgNwe = 16 Then
        For i = 0 To MessgNwe - 1
          r(i) = Rh(2 + 2 * i)
        Next i
      ElseIf MessgNwe = 36 Then
        For i = 0 To MessgNwe - 1
          r(i) = Rh(i)
        Next i

      End If
      '
      '

      '
    End If

  End Sub
 
  Sub dispose() Implements IDisposable.Dispose
  End Sub

  Public Sub New()
    istat = 0
    MnReTr = 0
    Try
      EyeOne = CreateObject("EyeOneActiveX.EyeOneVB6")
    Catch ex As Exception
      'z.B. falls EyeOneActiveX.EyeOneVB6 oder eyeonectrl.dll nicht verfügbar
      MsgBox("EyeOneActiveX.EyeOneVB6  " & Texxt(2984))
      istat = 999
    End Try
  End Sub
  Protected Overrides Sub Finalize()
  End Sub
End Class
