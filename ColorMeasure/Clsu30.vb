Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsU30
  Implements IDisposable

  Private istat As Short
  Private iermes As Short
  Dim kw As Short
  Dim j As Short
  Dim i As Short
  Dim nkw As Short
  Dim MnReTr As Short
  Sub MesSon()

  End Sub

  Sub RwrtUDS(ByRef filename As String, ByRef istat As Short)
    '
    'Transmissionswerte von .UDS-File lesen
    '
    Dim nwe As Short
    Dim k As Short
    Dim i As Short
    Dim j As Short
    Dim l As Short
    Dim lu As Short
    Dim ier As Short
    Dim Wel1 As Single
    Dim Wel2 As Single
    Dim Wschr As Single
    Dim wert As Single
    Dim Iabtr As Short
    Dim Ianz As Short
    Dim Ibyt As Short
    Dim name As New VB6.FixedLengthString(46)
    Dim bem As New VB6.FixedLengthString(48)
    Dim Hilf() As Single
    Dim Xhilf() As Single
    Dim abcd(,) As Single
    lu = 89
    Err.Clear()
    On Error GoTo RwrtUDSerr
    FileOpen(lu, filename, OpenMode.Binary, OpenAccess.Read, OpenShare.LockReadWrite)
    On Error GoTo 0

    'UPGRADE_WARNING: Get was upgraded to FileGet and has a new behavior. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="9B7D5ADD-D8FE-4819-A36C-6DEDAF088CC7"'
    FileGet(lu, name.Value, 5)
    'MsgBox name
    'UPGRADE_WARNING: Get was upgraded to FileGet and has a new behavior. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="9B7D5ADD-D8FE-4819-A36C-6DEDAF088CC7"'
    FileGet(lu, bem.Value, 51)
    'MsgBox Bem
    'UPGRADE_WARNING: Get was upgraded to FileGet and has a new behavior. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="9B7D5ADD-D8FE-4819-A36C-6DEDAF088CC7"'
    FileGet(lu, Ianz, 237)
    'MsgBox Ianz
    'UPGRADE_WARNING: Lower bound of array Hilf was changed from 1 to 0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    ReDim Hilf(Ianz)
    'UPGRADE_WARNING: Lower bound of array Xhilf was changed from 1 to 0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    ReDim Xhilf(Ianz)
    'UPGRADE_WARNING: Lower bound of array abcd was changed from 1,1 to 0,0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    ReDim abcd(Ianz, 4)
    'UPGRADE_WARNING: Get was upgraded to FileGet and has a new behavior. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="9B7D5ADD-D8FE-4819-A36C-6DEDAF088CC7"'
    FileGet(lu, Wel1, 239)
    'UPGRADE_WARNING: Get was upgraded to FileGet and has a new behavior. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="9B7D5ADD-D8FE-4819-A36C-6DEDAF088CC7"'
    FileGet(lu, Wel2, 243)
    'UPGRADE_WARNING: Get was upgraded to FileGet and has a new behavior. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="9B7D5ADD-D8FE-4819-A36C-6DEDAF088CC7"'
    FileGet(lu, Iabtr, 247)
    'UPGRADE_WARNING: Get was upgraded to FileGet and has a new behavior. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="9B7D5ADD-D8FE-4819-A36C-6DEDAF088CC7"'
    FileGet(lu, Wschr, 249)
    'MsgBox Wel1
    'MsgBox Wel2
    'MsgBox Iabtr
    ' MsgBox Wschr
    If Wel1 < MessgWsol(MessgNwe) Or Wel2 > MessgWsol(1) Then
      On Error GoTo 0
      MsgBox(Texxt(3541))
      istat = -1
      FileClose(lu)
      Exit Sub
    End If
    For i = 0 To Ianz - 1
      Xhilf(i) = Wel2 + i * Wschr
      Ibyt = 253 + i * 4
      'UPGRADE_WARNING: Get was upgraded to FileGet and has a new behavior. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="9B7D5ADD-D8FE-4819-A36C-6DEDAF088CC7"'
      FileGet(lu, wert, Ibyt)
      'MsgBox wert
      If Iabtr = 0 Then
        Hilf(Ianz - i) = 0.01 * wert
      ElseIf Iabtr = 256 Then
        Hilf(Ianz - i) = 10.0# ^ (-wert)
      Else
        On Error GoTo 0
        MsgBox(Texxt(3540))
        istat = -1
        Exit Sub
      End If
    Next i

    nwe = MessgNwe
    Call Akint(Ianz, Xhilf, Hilf, abcd, ier)
    Call Intaki(abcd, Ianz, Xhilf, nwe, MessgWsol, Rhilf)


    FileClose(lu)
    'MsgBox rhilf(1)
    'MsgBox rhilf(MessgNwe)
    iami = 1
    DEwmes(1) = 0
    MnName = name.Value
    MnBEM = bem.Value
    Exit Sub
RwrtUDSerr:
    On Error GoTo 0
    MsgBox(Texxt(3539), 0, filename)
    FileClose(lu)

  End Sub
  Sub MesSpei()

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
    istat = 0
    MessgKalw = 1
    If NormfileID <> 10 Then
      MsgBox(Texxt(3519))
      istat = -1
      Exit Sub
    End If
    idka(0) = 0
    idka(1) = 0
    idka(2) = 0
    idka(3) = 0
  End Sub

  Sub MesMes()
    Dim AktDir As String
    istat = NuWrt
    WinChk = False
    NXTChk = False
    COMchk = False
    AktDir = CurDir()
    ChDir(MessgLbez)
    'UPGRADE_WARNING: Filter has a new behavior. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="9B7D5ADD-D8FE-4819-A36C-6DEDAF088CC7"'
    ccdFILOpen.Filter = "*.uds|*.uds"
    ccdFILOpen.FileName = ""
    ccdFILOpen.ShowDialog()
    ChDrive(AktDir)
    ChDir(AktDir)
    'MsgBox SccdFIL.filename
    If Trim(ccdFILOpen.FileName) = "" Then
      istat = -1
      iami = 0
      Exit Sub
    End If
    Call RwrtUDS((ccdFILOpen.FileName), istat)
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
    Autom = 1
  End Function

  Sub einst(ByRef Index As Short)
  End Sub



  Sub MesIni()
    '
    'Initialisieren
    '
    '
    istat = NuWrt
  End Sub
  Sub MesKal(ByRef imess As Short)
    '
    'Kalibrieren
    '
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