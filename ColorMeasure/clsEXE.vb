Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsEXE
  Implements IDisposable
  'Allgemeines Messprogramm mit .EXE -Aufruf
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
  Dim MeasData As String
  Dim StWel As Integer
  Dim EnWel As Integer
  Dim ScWel As Integer
  Dim Ndim As Integer
  Dim InputString() As String
  Dim inp() As String
  Dim Sr As StreamReader
  Private Function PrgCommand(ByVal MeasProg As String, ByVal MessgMenu As Integer, ByVal imess As Integer, ByVal ReTr As Integer, ByVal StWel As Integer, ByVal EnWel As Integer, ByVal ScWel As Integer, ByVal DatFile As String, ByVal CHRM() As String) As String
    Dim i As Integer
    PrgCommand = MeasProg & Space(1) & Str(MessgMenu) & ";" & Str(imess) & ";" & Str(ReTr) & ";" & Str(StWel) & ";" & Str(EnWel) & ";" & Str(ScWel) & ";" & DatFile
    For i = 0 To UBound(CHRM)
      PrgCommand = PrgCommand & ";" & CHRM(i)
    Next i
  End Function
  Private Sub InpString(ByRef Ndim As Integer, ByRef InputString() As String, ByVal MeasProg As String, ByVal MessgMenu As Integer, ByVal imess As Integer, ByVal ReTr As Integer, ByVal StWel As Integer, ByVal EnWel As Integer, ByVal ScWel As Integer, ByVal DatFile As String, ByVal CHRM() As String)
    Dim InString As String
    Dim iret As Long
    Dim ProgCommand As String
    Erase InputString
    Ndim = -1
    If Not File.Exists(MeasProg) Then
      MsgBox(Texxt(4599) & Space(1) & MeasProg & Space(1) & Texxt(2984))
      istat = -1
      Exit Sub
    End If
    ProgCommand = PrgCommand(MeasProg, MessgMenu, imess, ReTr, StWel, EnWel, ScWel, MeasData, MessgChrm)
    iret = Shell(ProgCommand, AppWinStyle.MinimizedFocus, True)
    If iret <> 0 Then
      MsgBox(Texxt(3539) & ": " & ProgCommand & " (" & CStr(iret) & ")")
      istat = -1
    End If
    Application.DoEvents()
    If File.Exists(MeasData) Then
      Sr = New StreamReader(MeasData)
      Application.DoEvents()
      Call Wart(20)
      Application.DoEvents()
      Ndim = -1
      Erase InputString
      Do
        InString = ""
        InString = Sr.ReadLine
        If IsNothing(InString) Then
          Exit Do
        End If
        Ndim = Ndim + 1
        ReDim Preserve InputString(Ndim)
        InputString(Ndim) = InString
      Loop
      Sr.Close()
      Application.DoEvents()
      If File.Exists(MeasData) Then
        Try
          Kill(MeasData)
        Catch
        End Try
      End If
      Application.DoEvents()
    Else
      MsgBox(Texxt(3629) & ": " & MeasData)
      istat = -1
    End If
  End Sub
  Private Sub GetInputString(ByVal InString As String, ByRef inp() As String)
    Dim Ko As Char = ";"
    inp = InString.Split(Ko)
    If inp(UBound(inp)) = "" Then
      ReDim Preserve inp(UBound(inp) - 1)
    End If
  End Sub
  Sub MesSon()
    istat = NuWrt
    Call InpString(Ndim, InputString, MeasProg, MnMessgMenue, 0, MnReTr, StWel, EnWel, ScWel, MeasData, MessgChrm)
    If istat <> NuWrt Then Exit Sub
  End Sub


  Sub SetIdka(ByVal MessgMenue As Integer)
    'IDKA
    '1  Weiﬂstandard
    '2  Schwarzstandard
    '3  Graustandard
    '4  Weissstandard (Transmission)
    '5  Schwarzstandard (Transmission)
    istat = 0
    If MessgDriver = "000" Then
      Call InpString(Ndim, InputString, MeasProg, -1, 0, MnReTr, StWel, EnWel, ScWel, MeasData, MessgChrm)
      If istat <> 0 Then
        Exit Sub
      End If
      If Ndim = -1 Then
        '
        'Default-Werte (Weiﬂkalibrierung(1) und Schwarzkalibrierung(2))
        '
        idka(0) = 1
        idka(1) = 2
        Exit Sub
      End If
      InString = InputString(0)
      Call GetInputString(InString, inp)
      For i = 0 To UBound(inp)
        If Not IsNumeric(inp(i)) Then
          MsgBox(Texxt(3531) & inp(i))
          istat = -1
          Exit Sub
        End If
        idka(i) = CInt(inp(i))
        If idka(i) < 1 Or idka(i) > 5 Then
          MsgBox(Texxt(3531) & inp(i))
          istat = -1
          Exit Sub
        End If
      Next i
    Else
      MsgBox(Texxt(3542) & MessgDriver)
    End If
  End Sub
  Sub MesMes()
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
        If kw = 0 Then
          Call InpString(Ndim, InputString, MeasProg, MnMessgMenue, 0, MnReTr, StWel, EnWel, ScWel, MeasData, MessgChrm)
          If istat <> NuWrt Then Exit Sub
          If Ndim <> MessgKM - 1 Then
            MsgBox(Texxt(3633))
            istat = -1
            Exit Sub
          End If
        End If
        If Ndim = -1 Then
          istat = -1
          Exit Sub
        End If
        InString = InputString(kw)
        Call GetInputString(InString, inp)
        If UBound(inp) <> MessgNwe Then
          MsgBox(Texxt(3519) & ": " & InString)
          istat = -1
          Exit Sub
        End If
        If Trim(inp(MessgNwe)) <> Trim(MessgChrm(kw)) Then
          MsgBox(Texxt(3509) & ": " & InString)
          istat = -1
          Exit Sub
        End If
        For i = 0 To MessgNwe - 1
          rmes(i) = Val(inp(i))
        Next i
        If istat <> NuWrt Then Exit For
        Call RefWsolIst(MessgNwe, rmes, MessgWist, MessgWsol)
        Call NormRwrt(MnReTr, kw, rmes)
        MNbtnMIT.Enabled = False
        MNbtnLOE.Enabled = False
      Next kw
      '
      '
      '
      '
      '     Speichern
      '
      '
      Call RwrSpei(istat, MesZae)
      'MsgBox "nach " & iami

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
    '
  End Sub


  Sub MesSpei()

  End Sub




  Sub MesIni()
    istat = NuWrt
    Call InpString(Ndim, InputString, MeasProg, MnMessgMenue, 0, MnReTr, StWel, EnWel, ScWel, MeasData, MessgChrm)
    If istat <> NuWrt Then Exit Sub
    If Ndim >= 0 Then
      For j = 0 To UBound(InputString)
        InString = InputString(j)
        Call SplitInputString(InString, inp, ";")
        If UBound(inp) < MessgNwe - 1 Then
          MsgBox(Texxt(3519) & ": " & InString)
          istat = -1
          Exit Sub
        End If
        For i = 0 To MessgNwe - 1
          'Istwellenl‰ngen
          MessgWist(i) = Val(inp(i))
        Next i
      Next j
    End If
  End Sub
  Sub MesKal(ByVal imess As Integer)
    Dim jj As Integer
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
        If kw = 0 Then
          Call InpString(Ndim, InputString, MeasProg, MnMessgMenue, imess, MnReTr, StWel, EnWel, ScWel, MeasData, MessgChrm)
          If istat <> NuWrt Then Exit Sub
          If Ndim <> -1 Then
            If Ndim <> MessgKM - 1 Then
              MsgBox(Texxt(3633))
              istat = -1
              Exit Sub
            End If
          End If
        End If
        If Ndim > -1 Then
          If kw <= UBound(InputString) Then
            InString = InputString(kw)
            Call GetInputString(InString, inp)
            If UBound(inp) <> MessgNwe Then
              MsgBox(Texxt(3519) & ": " & InString)
              istat = -1
              Exit Sub
            End If
            If Trim(inp(MessgNwe)) <> Trim(MessgChrm(kw)) Then
              MsgBox(Texxt(3509) & ": " & InString)
              istat = -1
              Exit Sub
            End If
            For i = 0 To MessgNwe - 1
              rmes(i) = Val(inp(i))
            Next i
          End If
          If istat = 1 Or istat = -1 Then GoTo KalEnd
          Call KalSum(kw, rsmes, rmes)
          If istat = 0 Then GoTo KalEnd
        End If
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
    '
  End Sub


  Sub New()
    MyBase.New()
    MeasProg = GetPrivSettings("MEASURE", "MEASEXE", "", COLORFileName())
    MeasData = GetPrivSettings("MEASURE", "MEASFILE", "", COLORFileName())
    StWel = MessgWsol(0)
    EnWel = MessgWsol(MessgNwe - 1)
    ScWel = MessgWsol(1) - MessgWsol(0)
    istat = 0
    MnReTr = 0
  End Sub
  Sub dispose() Implements IDisposable.Dispose
  End Sub

  Protected Overrides Sub Finalize()
    Call InpString(Ndim, InputString, MeasProg, 5, 0, MnReTr, StWel, EnWel, ScWel, MeasData, MessgChrm)
    MyBase.Finalize()
  End Sub
End Class
