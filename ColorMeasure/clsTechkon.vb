Option Strict Off
Option Compare Text
Option Explicit On
Friend Class clsTechkon
  Private Iauswhl As Short
  Private Kauswhl As Short
  Private istat As Short
  Private iermes As Short
  Dim kw As Short
  Dim j As Short
  Dim i As Short
  Dim nkw As Short
  Dim MnReTr As Short
  Dim TechKon As Object

  Sub einst(ByVal Index As Integer)

  End Sub
  Property IchStat() As Short
    Get
      IchStat = istat
    End Get
    Set(ByVal Value As Short)
      istat = Value
    End Set
  End Property


  Sub MesIni()
    istat = NuWrt
  End Sub
  Sub MesKal(ByVal imess As Integer)
    Dim kw As Integer
    Dim jj As Integer
    istat = NuWrt
    WinChk = False
    NXTChk = False
    COMchk = False
    MNbtnABR.Visible = True
    MNbtnABR.Enabled = True
    Call NullRwrt(Mnier)
    jj = 0
    '
    '
    'ger‰teabh‰ngige Driver
    '
    '
    '
    For kw = 0 To MessgKM - 1
      Call TechKonMes(imess, kw, rmes, istat, MscMes)
      If istat = 1 Or istat = -1 Then GoTo KalEnd
      Call KalSum(kw, rsmes, rmes)
    Next kw
    jj = jj + 1
    If istat = 0 Then GoTo KalEnd
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
        Call TechKonMes(0, kw, rmes, istat, MscMes)
        If istat <> NuWrt Then Exit For
        Call NormRwrt(-1, kw, rmes)
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

  Sub SetIdka(ByVal MessgMenue As Integer)
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

    If NormfileID <> 1 And NormfileID <> 2 Then
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
  Sub TechKonMes(ByVal imess As Integer, ByVal kw As Integer, ByRef r() As Single, ByRef istat As Integer, _
  ByVal mscMES As SerialPort)
    Dim CHRM As String
    Dim StdAbw As Single
    Dim i As Integer
    '
    '
    '
    '     NB=1    400-700,20    NWE=16
    '        2    400-700,10    NWE=31
    '
    CHRM = MessgChrm(kw)
    '
    '     Messger‰t angeschlossen
    '
    '

    TechKon.SampleChanged = False
    TechKon.MeasureSample()
    Do
      Application.DoEvents()
      If ABRchk Then
        Exit Sub
      End If
      If TechKon.SampleChanged Then Exit Do
    Loop
    If MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = TechKon.Sample_Remission(2 * i + 1)
      Next i
    Else
      For i = 0 To MessgNwe - 1
        r(i) = TechKon.Sample_Remission(i + 1)
      Next i
    End If
    '      
    If imess = 1 Or imess = 10 Then
      '
      'Kalibrieren (weiﬂ) am Messger‰t
      '
      StdAbw = 0.0#
      For i = 0 To MessgNwe - 1
        StdAbw = StdAbw + (MessgQW(i) - r(i)) ^ 2
        r(i) = 100.0# * r(i)
      Next i
      StdAbw = Sqrt(StdAbw / MessgNwe)
      If StdAbw > 0.01 Then
        MsgBox(Texxt(3530))
        MsgBox(Texxt(3529), 0)
        istat = -1
        Exit Sub
      End If
    End If

  End Sub
  Property ReTr() As Short
    Get
      ReTr = MnReTr
    End Get
    Set(ByVal Value As Short)
      MnReTr = Value
    End Set
  End Property
 
  Public Sub New()
    Dim i As Integer
    istat = 0
    MnReTr = 0
    TechKon = CreateObject("TKSDSDK.Connection")
    TechKon.KeyStart()
    For i = 0 To 5
      Application.DoEvents()
      Call Wart(5)
      If TechKon.OpenConnection Then
        'TechKon.MeasurementMode = TKSD_MeasurementModeType.TKSDMM_Remission_Curve
        TechKon.MeasurementMode = 12
        Exit For
      End If
    Next i
    If i > 5 Then
      MsgBox(Texxt(3500))
      istat = -1
    End If
  End Sub

  Protected Overrides Sub Finalize()
    'TechKon.CloseConnection()
    TechKon = Nothing
    MyBase.Finalize()
  End Sub
End Class
