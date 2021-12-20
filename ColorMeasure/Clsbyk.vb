Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsBYK
  Implements IDisposable

  '
  'istat = 0   kein Fehler
  'istat > 0   Fehler ist aufgetreten Programm neu starten
  'istat < 0   Fehler ist aufgetreten Programm nicht neu starten
  '
  'Eigenschaften für Byk-Gardner Color-Guide-DLL's
  '
  '
  '
  '
  Private V24error As Integer
  Private instrument As Short
  Private ComHandle As Integer
  Private strCmd As String
  Private lngCmdLen As Integer
  Private strResult As String
  Private lngMaxRes As Integer
  Private lngBytesWritten As Integer
  Private cSendOnly As Integer
  '
  '
  '
  '
  '
  Private istat As Short
  Private iermes As Short
  Dim kw As Short
  Dim j As Short
  Dim i As Short
  Dim nkw As Short
  Dim MnReTr As Short
  Private Declare Function sicOpen Lib "BYKSIC32.DLL" (ByVal wCOM As Short, ByVal wINSTR As Short, ByRef phSIC As Integer) As Integer
  Private Declare Function sicClose Lib "BYKSIC32.DLL" (ByVal phSIC As Integer) As Integer
  Private Declare Function sicFmtCommand Lib "BYKSIC32.DLL" (ByVal phSIC As Integer, ByVal pCmd As String, ByVal dwCmdLen As Integer, ByVal pRes As String, ByVal dwMaxRes As Integer, ByRef pdwBytesWritten As Integer) As Integer
  Private Declare Function sicRawCommand Lib "BYKSIC32.DLL" (ByVal phSIC As Integer, ByVal pCmd As String, ByVal dwCmdLen As Integer, ByVal pRes As String, ByVal dwMaxRes As Integer, ByRef pdwBytesWritten As Integer) As Integer
  Private Declare Function sicSendCommand Lib "BYKSIC32.DLL" (ByVal phSIC As Integer, ByVal pCmd As String, ByVal dwCmdLen As Integer, ByVal cSendOnly As Integer) As Integer
  Private Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Integer)








  Sub MesSon()

  End Sub

  Sub BYGini(ByRef istat As Short)

  End Sub


  Sub BYKini(ByRef istat As Short)

  End Sub

  Sub BYCini(ByRef istat As Short)

  End Sub
  Sub MesSpei()

  End Sub

  Sub SetIdka(ByVal MessgMenue As Integer)
    '1  Weißstandard
    '2  Schwarzstandard
    '3  Graustandard
    '4  Weissstandard (Transmission)
    '5  Schwarzstandard (Transmission)
    istat = 0

    If MessgDriver = "BYK" Then
      If Reftra <> "R" Then
        MsgBox(Texxt(3590) & "  (" & Reftra & ")")
        istat = -1
        Exit Sub
      End If
      idka(0) = 2
      idka(1) = 1
      idka(2) = 0
      If NormfileID <> 3 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
    ElseIf MessgDriver = "BYC" Then
      If Reftra <> "R" Then
        MsgBox(Texxt(3590) & "  (" & Reftra & ")")
        istat = -1
        Exit Sub
      End If
      idka(0) = 2
      idka(1) = 1
      idka(2) = 0
      If NormfileID <> 1 And NormfileID <> 2 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
    ElseIf MessgDriver = "BYG" Then
      If Reftra = "R" Then
        idka(0) = 2
        idka(1) = 1
        idka(2) = 0
      End If
      If Reftra = "T" Then
        idka(0) = 5
        idka(1) = 4
        idka(2) = 0
      End If
      If Reftra = "A" Then
        idka(0) = 5
        idka(1) = 2
        idka(2) = 4
        idka(3) = 1
        idka(4) = 0
      End If
      If NormfileID <> 1 And NormfileID <> 3 And NormfileID <> 2 Then
        MsgBox(Texxt(3519))
        istat = -1
        Exit Sub
      End If
    Else
      MsgBox(Texxt(3542) & MessgDriver)
    End If
  End Sub
  Sub HeaderCommand(ByVal HDR() As Byte, ByVal CMD() As Byte, ByVal mscMES As SerialPort)
    Dim By() As Byte
    Dim i As Integer
    Dim BY1 As Byte = 1
    Dim BY2 As Byte = 2
    '
    'Header
    '
    ReDim By(0)
    By(0) = BY1
    mscMES.Write(By, 0, 1)
    mscMES.Write(HDR, 0, UBound(HDR) + 1)
    '
    'Command
    '
    ReDim By(UBound(CMD) + 1)
    By(0) = BY2
    For i = 1 To UBound(CMD) + 1
      By(i) = CMD(i - 1)
    Next
    mscMES.Write(By, 0, UBound(By) + 1)
    '
    'Checksum
    '
    ReDim By(0)
    By(0) = CHKSUM(HDR, CMD)
    mscMES.Write(By, 0, 1)
  End Sub
  Function CHKSUM(ByRef HDR() As Byte, ByRef CMD() As Byte) As Byte
    Dim CKSUM As Long
    Dim i As Integer
    CKSUM = 0
    For i = 0 To UBound(HDR)
      CKSUM = CKSUM + HDR(i)
    Next i
    For i = 0 To UBound(CMD)
      CKSUM = CKSUM + CMD(i)
    Next i
    CKSUM = CKSUM Mod 256
    CHKSUM = CKSUM
  End Function



  Sub MesMes()
    Dim kw As Integer
    istat = NuWrt
    WinChk = False
    NXTChk = False
    COMchk = False
    MITchk = False
    MNbtnNXT.Visible = False
    NXTChk = True
    If MessgDriver = "BYK" Then
      Call PoOpen(MscMes, iermes)
      If iermes <> 0 Then
        Call PoClose(MscMes, iermes)
        istat = -1
        Exit Sub
      End If
    ElseIf MessgDriver = "BYC" Then
      V24error = sicOpen(MessgCom, instrument, ComHandle)
      If V24error <> 0 Then
        MsgBox(Texxt(3500) & "  " & Hex(V24error))
        istat = -1
        Exit Sub
      End If
    ElseIf MessgDriver = "BYG" Then
      Call PoOpen(MscMes, iermes)
      If iermes <> 0 Then
        Call PoClose(MscMes, iermes)
        istat = -1
        Exit Sub
      End If
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
        If MessgDriver = "BYK" Then
          Call BYKMes(0, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "BYC" Then
          Call BYCMes(0, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "BYG" Then
          Call BYGMes(0, kw, rmes, istat, MscMes)
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
    If MessgDriver = "BYK" Then
      Call PoClose(MscMes, iermes)
    ElseIf MessgDriver = "BYC" Then
      V24error = sicClose(ComHandle)
    ElseIf MessgDriver = "BYG" Then
      Call PoClose(MscMes, iermes)
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

  Sub einst(ByRef Index As Short)
    '
    '
  End Sub



  Sub BYKrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    Dim N As Short
    Dim M As Short
    Dim l As Short
    Dim MSB As Single
    Dim LSB As Single
    Dim MB As String
    Dim LB As String
    Dim Wand As Single
    Dim Schwa As Single
    Dim Probe As Single
    Dim Iwand As Short
    Dim ISchwa As Short
    Dim Iprobe As Short
    '
    '
    'Statuszeile entfernen
    '
    '
    If Len(BUF) < 220 Then
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
    '       380-720 (10nm)
    '
    '
    Iwand = 10
    ISchwa = 80
    Iprobe = 150

    For N = 1 To 35
      M = N * 2 + Iwand
      l = M - 1
      MB = Mid(BUF, M, 1)
      LB = Mid(BUF, l, 1)
      MSB = Asc(MB)
      LSB = Asc(LB)
      Wand = MSB * 256 + LSB
      M = N * 2 + ISchwa
      l = M - 1
      MB = Mid(BUF, M, 1)
      LB = Mid(BUF, l, 1)
      MSB = Asc(MB)
      LSB = Asc(LB)
      Schwa = MSB * 256 + LSB
      M = N * 2 + Iprobe
      l = M - 1
      MB = Mid(BUF, M, 1)
      LB = Mid(BUF, l, 1)
      MSB = Asc(MB)
      LSB = Asc(LB)
      Probe = MSB * 256 + LSB
      '
      'für VB.NET?
      r(N - 1) = (Probe - Schwa) / (Wand - Schwa)
    Next N
    '

  End Sub

  Sub BYCrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
    Dim i As Short
    Dim j As Short
    Dim rr(30) As Single
    Dim k As Short
    '
    '
    'Statuszeile entfernen
    '
    '
    If Len(BUF) < 307 Then
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
    ' Null durch Tab ersetzen
    i = InStr(BUF, Chr(0))
    Mid(BUF, i, 1) = Chr(9)
    '
    '
    '
    '       400-700nm (10nm)
    '
    '

    j = 1
    For i = 0 To 30
      k = InStr(Mid(BUF, j), Chr(9))
      rr(i) = Val(Mid(BUF, j, k - 1))
      j = j + k
    Next i
    If MessgNwe = 16 Then
      For i = 0 To MessgNwe - 1
        r(i) = rr(2 * i)
      Next i
    Else
      For i = 0 To MessgNwe - 1
        r(i) = rr(i)
      Next i
    End If
    '

  End Sub





  Sub BYKMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim CHRM As String
    Dim i As Short
    Dim iver As Short
    Dim buflen As Short
    Dim wart As Short
    Dim delay As Short
    Dim resp As Short
    Dim CU As String
    Dim Automodus As String
    Dim Eot As String
    Dim Akn As String
    Dim Messcmd As String
    Dim IBASF As Short
    CrLf = Chr(13)
    buflen = 4
    iver = 5
    wart = 5
    delay = 10
    resp = 2
    CU = Chr(1) & Chr(22) & Chr(0) & Chr(242) & Chr(0) & Chr(1)
    CU = CU & Chr(0) & Chr(0) & Chr(0) & Chr(2) & Chr(4) & Chr(13)

    Automodus = Chr(1) & Chr(22) & Chr(0) & Chr(242) & Chr(0) & Chr(4)
    Automodus = Automodus & Chr(0) & Chr(0) & Chr(0) & Chr(2) & Chr(3)
    Automodus = Automodus & Chr(32) & Chr(4) & Chr(35) & Chr(86)

    Messcmd = Chr(1) & Chr(22) & Chr(0) & Chr(242) & Chr(0) & Chr(1)
    Messcmd = Messcmd & Chr(0) & Chr(0) & Chr(0) & Chr(2) & Chr(4) & Chr(13)

    Eot = Chr(4)
    Akn = Chr(6)
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
    'BYK Colorview
    '
    '    380-720 (10)
    'Beispiel
    '
    '
    CHRM = MessgChrm(kw)
    '
    'Initialisierung
    '
    '
    Try
      mscMES.ReadTimeout = 3000
      For i = 1 To 20
        stv = Eot
        mscMES.WriteLine(stv)
        Call Wait(30)
        If mscMES.ReadLine = Akn Then
          Exit For
        End If
      Next i
    Catch
      MsgBox("Error")
      MsgBox(Texxt(3506))
      istat = -1
    End Try
    If i >= 20 Then
      MsgBox(Texxt(3506))
      istat = -1
      Exit Sub
    End If
    If istat <> NuWrt Then Exit Sub
    '
    'Automodus einschalten
    '
    '
    stv = Automodus
    mscMES.WriteLine(stv)
    For i = 1 To 50
      Call Wait(50)
      BUF = mscMES.ReadLine
      If InStr(BUF, Chr(6)) <> 0 Then
        Exit For
      End If
    Next i
    If i >= 50 Then
      MsgBox(Texxt(3506))
      istat = -1
      Exit Sub
    End If
    If istat <> NuWrt Then Exit Sub

    '
    'Initialisierung
    '
    '
    '
    For i = 1 To 20
      stv = Eot
      mscMES.WriteLine(stv)
      Call Wait(30)
      If mscMES.ReadLine = Akn Then
        Exit For
      End If
    Next i
    If i >= 20 Then
      MsgBox(Texxt(3506))
      istat = -1
      Exit Sub
    End If
    If istat <> NuWrt Then Exit Sub
    '
    '
    '     Messung starten
    '
    '
    '
    stv = Messcmd
    mscMES.DiscardInBuffer()
    mscMES.WriteLine(stv)
    For i = 1 To 50
      Call Wait(50)
      BUF = mscMES.ReadLine
      If InStr(BUF, Chr(4)) <> 0 Then
        Exit For
      End If
    Next i
    If i >= 50 Then
      MsgBox(Texxt(3506))
      istat = -1
      Exit Sub
    End If

    '
    '
    '     Quittieren
    '
    '
    '
    stv = Akn
    BUF = ""
    mscMES.WriteLine(Akn)
    For i = 0 To 49
      Call Wait(50)
      If mscMES.BytesToRead > 220 Then
        BUF = mscMES.ReadLine
        Exit For
      End If
    Next i
    If i >= 50 Then
      MsgBox(Texxt(3506))
      istat = -1
      Exit Sub
    End If
    mscMES.WriteLine(Akn)

    IBASF = 0
    '
    'Messung
    '
    Call BYKrwert(IBASF, BUF, r, istat)
    '
  End Sub

  Sub BYCMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
    Dim IBASF As Short
    Dim i As Integer
    '
    'BYK Colorguide
    '
    '
    '400-700 (10)
    '
    '
    IBASF = 0
    If imess = 2 Then
      '
      '
      'Schwarzkalibrierung
      '
      '
      '
      lngMaxRes = 10
      strCmd = Chr(54)
      lngCmdLen = 1
      strResult = New String(Chr(0), lngMaxRes)
      V24error = sicRawCommand(ComHandle, strCmd, lngCmdLen, strResult, lngMaxRes, lngBytesWritten)
      If V24error <> 0 Then
        MsgBox(Texxt(3500) & "  " & Hex(V24error))
        istat = -1
      End If
      For i = 0 To MessgNwe - 1
        r(i) = 0
      Next i
    ElseIf imess = 1 Then
      '
      '
      'Weißkalibrierung
      '
      '
      '
      lngMaxRes = 10
      strCmd = Chr(55)
      lngCmdLen = 1
      strResult = New String(Chr(0), lngMaxRes)
      V24error = sicRawCommand(ComHandle, strCmd, lngCmdLen, strResult, lngMaxRes, lngBytesWritten)
      If V24error <> 0 Then
        MsgBox(Texxt(3500) & "  " & Hex(V24error))
        istat = -1
      End If
      For i = 0 To MessgNwe - 1
        r(i) = 100
      Next i

    ElseIf imess = 0 Then
      '
      '
      'Probe messen
      '
      lngMaxRes = 10
      strCmd = Chr(46)
      lngCmdLen = 1
      strResult = New String(Chr(0), lngMaxRes)
      V24error = sicRawCommand(ComHandle, strCmd, lngCmdLen, strResult, lngMaxRes, lngBytesWritten)

      If V24error <> 0 Then
        MsgBox(Texxt(3500) & "  " & Hex(V24error))
        istat = -1
      End If
      '
      Call Wait(30)
      '
      '
      'Messung übernehmen
      '
      '
      lngMaxRes = 1000
      strCmd = Chr(52)
      lngCmdLen = 1
      strResult = New String(Chr(0), lngMaxRes)
      V24error = sicFmtCommand(ComHandle, strCmd, lngCmdLen, strResult, lngMaxRes, lngBytesWritten)

      If V24error <> 0 Then
        MsgBox(Texxt(3500) & "  " & Hex(V24error))
        istat = -1
        Exit Sub
      End If
      Call BYCrwert(IBASF, strResult, r, istat)

    End If
    '
  End Sub
  Sub BYGMes(ByVal imess As Short, ByVal kw As Integer, ByRef r() As Single, ByRef istat As Short, _
  ByVal mscMES As SerialPort)
    Dim BufW As String
    Dim BufS As String
    Dim BufD As String
    Dim HDR(7) As Byte
    Dim CMD(0) As Byte
    Dim Izaehl As Integer
    Dim IBASF As Integer
    Dim RTS(0) As Byte
    Dim ACK(0) As Byte
    RTS(0) = 4
    ACK(0) = 6
    HDR(0) = 0
    HDR(1) = 0
    HDR(2) = 242
    HDR(3) = 0
    HDR(4) = 1
    HDR(5) = 0
    HDR(6) = 0
    HDR(7) = 0
    IBASF = 0
    '
    '
    '
    'Spectrogard
    '
    'BYG
    '
    '
    '380-720,5
    '
    '380-720,10
    '
    '
    '400-700,20
    '
    '400-700,10
    '
    '
    '
    Izaehl = 0
    mscMES.Write(RTS, 0, 1)
    mscMES.DtrEnable = True
    Do While mscMES.BytesToRead = 0
      Call Sleep(500)
      Izaehl = Izaehl + 1
      Application.DoEvents()
      If ABRchk Then
        istat = -1
        Exit Sub
      End If
      If Izaehl > 1 Then
        mscMES.Write(RTS, 0, 1)
      End If
      If Izaehl = 10 Then
        istat = -1
        MsgBox(Texxt(3506))
        Exit Sub
      End If
    Loop
    BUF = ReadString(mscMES)
    If InStr(BUF, Chr(6)) = 0 Then
      istat = -1
      MsgBox(Texxt(3506))
      Exit Sub
    End If
    CMD(0) = 0
    HDR(0) = 0
    Call HeaderCommand(HDR, CMD, mscMES)
    Do While mscMES.BytesToRead < 2
      Call Sleep(100)
      Application.DoEvents()
      If ABRchk Then
        istat = -1
        Exit Sub
      End If
    Loop
    BUF = ReadString(mscMES)
    If BUF <> Chr(6) & Chr(4) Then
      istat = -1
      Exit Sub
    End If

    mscMES.DtrEnable = False
    mscMES.Write(ACK, 0, 1)
    mscMES.DtrEnable = True
    Call Sleep(100)
    '
    '
    '
    Do While mscMES.BytesToRead < 14
      Call Sleep(100)
      Application.DoEvents()
      If ABRchk Then
        istat = -1
        Exit Sub
      End If
    Loop
    BUF = ReadString(mscMES)
    '
    '
    '
    mscMES.DtrEnable = False
    mscMES.Write(ACK, 0, 1)
    mscMES.DtrEnable = True
    Call Sleep(100)
    mscMES.DtrEnable = False
    mscMES.Write(RTS, 0, 1)
    mscMES.DtrEnable = True
    Call Sleep(100)
    '
    '
    '
    Do While mscMES.BytesToRead < 1
      Call Sleep(100)
      Application.DoEvents()
      If ABRchk Then
        istat = -1
        Exit Sub
      End If
    Loop
    BUF = ReadString(mscMES)
    If BUF <> Chr(6) Then
      istat = -1
      Exit Sub
    End If
    mscMES.DtrEnable = False
    HDR(0) = 2
    CMD(0) = 1
    Call HeaderCommand(HDR, CMD, mscMES)
    mscMES.DtrEnable = True

    Do While mscMES.BytesToRead < 2
      Call Sleep(100)
      Application.DoEvents()
      If ABRchk Then
        istat = -1
        Exit Sub
      End If
    Loop
    BUF = ReadString(mscMES)
    If BUF <> Chr(6) & Chr(4) Then
      istat = -1
      Exit Sub
    End If
    Call Sleep(100)
    mscMES.Write(ACK, 0, 1)
    Call Sleep(100)
    '
    '
    'Wall Scan
    '
    '
    '
    '
    BufW = ""
    Do While mscMES.BytesToRead <> 0
      Application.DoEvents()
      If ABRchk Then
        istat = -1
        Exit Sub
      End If
      Call Sleep(100)
      BufW = BufW & ReadString(mscMES)

    Loop
    If Len(BufW) <> 149 Then
      MsgBox(Texxt(3505))
      istat = -1
      Exit Sub
    End If

    '
    '
    '
    '
    Call Sleep(100)
    mscMES.DtrEnable = False
    mscMES.Write(ACK, 0, 1)
    mscMES.DtrEnable = True
    Call Sleep(100)
    '
    '
    '
    '
    Do While mscMES.BytesToRead < 1
      Application.DoEvents()
      If ABRchk Then
        istat = -1
        Exit Sub
      End If
      Call Sleep(100)
    Loop
    BUF = ReadString(mscMES)
    If BUF <> Chr(4) Then
      istat = -1
      Exit Sub
    End If
    '
    '
    '
    mscMES.DtrEnable = False
    mscMES.Write(ACK, 0, 1)
    mscMES.DtrEnable = True
    Call Sleep(100)
    '
    '
    '
    '
    '
    '
    'Sample Scan
    '
    '
    '
    '
    BufS = ""
    Do While mscMES.BytesToRead <> 0
      Application.DoEvents()
      If ABRchk Then
        istat = -1
        Exit Sub
      End If
      Call Sleep(100)
      BufS = BufS & ReadString(mscMES)

    Loop
    If Len(BufS) <> 149 Then
      MsgBox(Texxt(3505))
      istat = -1
      Exit Sub
    End If

    '
    '
    '
    '
    '
    Call Sleep(100)
    mscMES.DtrEnable = False
    mscMES.Write(ACK, 0, 1)
    mscMES.DtrEnable = True
    Call Sleep(100)
    '
    '
    '
    '
    Do While mscMES.BytesToRead < 1
      Application.DoEvents()
      If ABRchk Then
        istat = -1
        Exit Sub
      End If
      Call Sleep(100)
    Loop
    BUF = ReadString(mscMES)
    If BUF <> Chr(4) Then
      Exit Sub
    End If
    '
    '
    '
    mscMES.DtrEnable = False
    mscMES.Write(ACK, 0, 1)
    mscMES.DtrEnable = True
    Call Sleep(100)
    '
    '
    '
    '
    '
    '
    'Dove Scan
    '
    '
    '
    '
    BufD = ""
    Do While mscMES.BytesToRead <> 0
      Application.DoEvents()
      If ABRchk Then
        istat = -1
        Exit Sub
      End If
      Call Sleep(100)
      BufD = BufD & ReadString(mscMES)

    Loop
    If Len(BufD) <> 19 Then
      MsgBox(Texxt(3505))
      istat = -1
      Exit Sub
    End If

    '
    '
    mscMES.DtrEnable = False
    mscMES.Write(ACK, 0, 1)
    mscMES.DtrEnable = True
    Call Sleep(100)
    '
    '
    '
    '
    '
    BUF = BufW.Substring(10, 138) & BufS.Substring(10, 138) & BufD.Substring(10, 8)
    Call BYGrwert(IBASF, BUF, r, istat)

  End Sub
  Sub BYGrwert(ByVal IBASF As Integer, ByVal BUF As String, ByRef r() As Single, ByRef istat As Integer)
    Dim RW As Single
    Dim RS As Single
    Dim Dove As Single
    Dim i As Integer
    Dim Rh(68) As Single
    Dove = 0.0#
    For i = 0 To 3
      Dove = Dove + 256.0# * Asc(BUF.Substring(276 + 2 * i, 1)) + Asc(BUF.Substring(276 + 2 * i + 1, 1))
    Next i
    Dove = 0.25 * Dove
    '
    '
    '
    '
    '
    For i = 0 To 68
      RW = 256.0# * Asc(BUF.Substring(2 * i, 1)) + Asc(BUF.Substring(2 * i + 1, 1))
      RS = 256.0# * Asc(BUF.Substring(138 + 2 * i, 1)) + Asc(BUF.Substring(138 + 2 * i + 1, 1))
      Rh(i) = (RS - Dove) / (RW - Dove)
    Next i
    If NormfileID = 1 Then
      '
      '
      '400-700,20
      '
      '
      j = 4
      For i = 0 To MessgNwe - 1
        r(i) = Rh(j)
        j = j + 4
      Next i
    ElseIf NormfileID = 2 Then
      '
      '
      '400-700,10
      '
      '
      j = 4
      For i = 0 To MessgNwe - 1
        r(i) = Rh(j)
        j = j + 2
      Next i
    ElseIf NormfileID = 3 Then
      '
      '
      '380-720,10
      '
      '

      j = 0
      For i = 0 To MessgNwe - 1
        r(i) = Rh(j)
        j = j + 2
      Next i
    ElseIf NormfileID = 4 Then
      '
      '
      '380-720,5
      '
      '

      For i = 0 To MessgNwe - 1
        r(i) = Rh(i)
      Next i
    End If
  End Sub





  Sub MesIni()
    istat = NuWrt
    If MessgDriver = "BYK" Then
      Call BYKini(istat)
    ElseIf MessgDriver = "BYC" Then
      Call BYCini(istat)
    ElseIf MessgDriver = "BYG" Then
      Call BYGini(istat)
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
    If MessgDriver = "BYK" Then
      Call PoOpen(MscMes, iermes)
    ElseIf MessgDriver = "BYC" Then
      V24error = sicOpen(MessgCom, instrument, ComHandle)
      If V24error <> 0 Then
        MsgBox(Texxt(3500) & "  " & Hex(V24error))
        istat = -1
      End If
    ElseIf MessgDriver = "BYG" Then
      Call PoOpen(MscMes, iermes)
    End If
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
        If MessgDriver = "BYK" Then
          Call BYKMes(imess, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "BYC" Then
          Call BYCMes(imess, kw, rmes, istat, MscMes)
        ElseIf MessgDriver = "BYG" Then
          Call BYGMes(imess, kw, rmes, istat, MscMes)
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

    If MessgDriver = "BYK" Then
      Call PoClose(MscMes, iermes)
    ElseIf MessgDriver = "BYC" Then
      V24error = sicClose(ComHandle)
    ElseIf MessgDriver = "BYG" Then
      Call PoClose(MscMes, iermes)
    End If
  End Sub
  


  'UPGRADE_NOTE: Class_Initialize was upgraded to Class_Initialize_Renamed. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="A9E4979A-37FA-4718-9994-97DD76ED70A7"'
  Private Sub Class_Initialize_Renamed()
    istat = 0
    MnReTr = 0

    instrument = 10
  End Sub
  Sub New()
    MyBase.New()
    Class_Initialize_Renamed()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
  End Sub
End Class