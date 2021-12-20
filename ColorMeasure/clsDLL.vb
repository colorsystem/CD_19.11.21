Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsDLL
  Implements IDisposable
  'Allgemeines Messprogramm mit .DLL -Aufruf
  '
  '
  '
  'istat = 0   kein Fehler
  'istat > 0   Fehler ist aufgetreten Programm neu starten
  'istat < 0   Fehler ist aufgetreten Programm nicht neu starten
  Private istat As Integer
  Private iermes As Integer
  Dim kw As Integer
  Dim j As Integer
  Dim i As Integer
  Dim nkw As Integer
  Dim MnReTr As Integer
  Dim InString As String
  Dim MeasProg As String
  Dim StWel As Integer
  Dim EnWel As Integer
  Dim ScWel As Integer
  Dim Ndim As Integer
  Dim MeasActiveXCom As Object

  Sub MesSon()
    istat = NuWrt
    Call MeasActiveXCom.MesSon(istat)
    If istat <> NuWrt Then Exit Sub
  End Sub


  Sub SetIdka(ByVal MessgMenue As Integer)
    Dim Nidka As Integer
    Dim i As Integer
    'MessgMenue
    '0 Messen
    '1 Kalibrieren
    '2 Nullkalibrieren
    '3 Initialisieren

    'IDKA
    '1  Weiﬂstandard
    '2  Schwarzstandard
    '3  Graustandard
    '4  Weissstandard (Transmission)
    '5  Schwarzstandard (Transmission)
    istat = 0
    If IsNothing(MeasActiveXCom) Then Exit Sub
    If IsNumeric(MessgDriver) Then
      MeasActiveXCom.SetIdka(Nidka, idka, istat)
      If istat <> 0 Then
        Exit Sub
      End If
      idka(Nidka) = 0
      For i = 0 To Nidka - 1
        If idka(i) < 1 Or idka(i) > 5 Then
          MsgBox(Texxt(3531) & idka(i))
          istat = -1
          Exit Sub
        End If
      Next i
    Else
      istat = -1
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
    '
    '
    Call MeasActiveXCom.messaux(MnReFel.Bem)

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
      '  Messung starten; Werte nach rrmes(0..........(NWE-1)(KM-1)) abspeichern
      '
      '
      '
      '
      For kw = 0 To MessgKM - 1
        '
        'RRMES(kw*NWE.....(kw+1)*(NWE-1)) aufbauen
        '
        Call MeasActiveXCom.MesMes(kw, MessgChrm(kw), MnReTr, rmes, istat)
        If istat <> NuWrt And istat <> 0 Then
          istat = -1
          Exit Sub
        End If
        Call NormRwrt(MnReTr, kw, rmes)
        MNbtnMIT.Enabled = False
        MNbtnLOE.Enabled = False
      Next kw
      '
      '
      '
      '
      '
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
      '
      'Rhilf aufbauen
      '
      '
      Call RwrtSpei(istat)
    End If
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
    '
    '
    'Einstellungen beim Initialisieren des Messger‰tes
    '
    ''
  End Sub


  Sub MesSpei()

  End Sub




  Sub MesIni()
    If Not IsNothing(MeasActiveXCom) Then
      istat = NuWrt
      Call MeasActiveXCom.MesINI(MessgWist, MessgQW, MessgQS, MessgQG, MessgQT, MessgQB, istat)
      If istat <> NuWrt Then Exit Sub
    End If
  End Sub
  Sub MesKal(ByVal imess As Integer)
    Dim kw As Integer
    Dim j As Integer
    Dim i As Integer
    Dim ReTr As Integer
    'imess
    'Kalibrieren
    '1  Weiﬂstandard
    '2  Schwarzstandard
    '3  Graustandard
    '4  Weissstandard (Transmission)
    '5  Schwarzstandard (Transmission)
    'Nullkalibrieren
    '6  Weiﬂstandard
    '7  Schwarzstandard
    '8  Graustandard
    '9  Weissstandard (Transmission)
    '10  Schwarzstandard (Transmission)
  
    Dim jj As Integer
    ReTr = 0
    If (imess >= 4 And imess <= 5) Or (imess >= 9 And imess <= 10) Then
      ReTr = 1
    End If
    istat = NuWrt
    WinChk = False
    NXTChk = False
    COMchk = False
    MNbtnABR.Visible = True
    MNbtnABR.Enabled = True
    Call NullRwrt(Mnier)
    jj = 0
    For i = 0 To MessgNwe - 1
      Select Case imess
        Case 1, 4
          rmes(i) = 100.0#
        Case 2, 5
          rmes(i) = 0.0#
        Case 3
          rmes(i) = 50.0#
      End Select
    Next i

    For j = 1 To irmax
      '
      '
      'ger‰teabh‰ngige Driver
      '
      '
      '
      For kw = 0 To MessgKM - 1


        Call MeasActiveXCom.MesKal(imess, kw, MessgChrm(kw), ReTr, rmes, istat)
        If istat <> NuWrt Then istat = -1
        If istat = 1 Or istat = -1 Then GoTo KalEnd
        Call KalSum(kw, rsmes, rmes)
        If istat = 0 Then GoTo KalEnd
      Next kw
      jj = jj + 1
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
    ''
  End Sub


  Sub New()
    MyBase.New()
    MeasProg = GetPrivSettings("MEASURE", "MEAS" & MessgDriver, "", COLORFileName())
    Try
      MeasActiveXCom = CreateObject(MeasProg)
    Catch
      MsgBox(Texxt(409) & ": " & MeasProg & " " & Texxt(2984))
      istat = -1
      dispose()
      Exit Sub
    End Try

    StWel = MessgWsol(0)
    EnWel = MessgWsol(MessgNwe - 1)
    ScWel = MessgWsol(1) - MessgWsol(0)
    istat = 0
    MnReTr = MenueParam.Messg.ReTr
    Try
      Call MeasActiveXCom.MesStart(MessgID, MessgProg, MessgKenn, MessgCom, MessgBaud, MessgLength, MessgStop, MessgPar, MessgHands, MessgKM, MessgChrm, MessgNwe, StWel, EnWel, ScWel, istat)
      If istat <> 0 Then
        Exit Sub
      End If
    Catch ex As Exception

    End Try

  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If Not IsNothing(MeasActiveXCom) Then
      Try
        Call MeasActiveXCom.MesEnd()
      Catch ex As Exception
        MsgBox("ERROR MesEnd")
      End Try
    End If
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub
End Class
