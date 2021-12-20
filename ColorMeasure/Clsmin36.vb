Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsMIN36
  Implements IDisposable
  '
  '
  '
  '
  'Für MINOLTA 3600d und 3300d
  '
  '
  Private Declare Function OpenToCmd Lib "CM3600S" (ByVal port As Integer, ByVal boudrate As Integer) As Integer
  Private Declare Function CloseToCmd Lib "CM3600S" (ByVal port As Integer) As Integer
  Private Declare Function SendToCmd Lib "CM3600S" (ByVal cmd As String, ByVal BUFF As String, ByVal buffsize As Integer) As Integer

  Private Const CMD_OK = 0
  Private Const CMD_WRN = -1
  Private Const CMD_ERR = -2
  Private Const CMD_COM_ERR = -3
  Private Const CMD_DLL_ERR = -4


  'istat = 0   kein Fehler
  'istat > 0   Fehler ist aufgetreten Programm neu starten
  'istat < 0   Fehler ist aufgetreten Programm nicht neu starten
  Private istat As Integer
  Private iermes As Integer
  Dim kw As Integer
  Dim j As Integer
  Dim i As Integer
  Dim nkw As Integer
  Dim ret As Integer
  Dim stv As String
  Dim Puffer As String
  Dim BufSize As Integer
  Dim port As Integer
  Dim BaudRate As Integer
  Dim NoEinst As Boolean
  Dim Sqlstmt As String
  Dim MnReTr As Integer
  Dim UVein As String
  Sub MesSon()

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
    If Reftra = "R" Then
      idka(0) = 2
      idka(1) = 1
      idka(2) = 0
    ElseIf Reftra = "T" Then
      idka(0) = 5
      idka(1) = 4
      idka(2) = 0
    ElseIf Reftra = "A" Then
      idka(0) = 2       'Reflexion(schwarz)
      idka(1) = 1       'Reflexion(weiß)
      idka(2) = 5       'Transmission(schwarz)
      idka(3) = 4       'Transmission(weiß)
      idka(4) = 0
    End If
    If MessgDriver = "MI3" Then
      If NormfileID <> 11 And NormfileID <> 1 And NormfileID <> 2 Then
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
    'Call PoOpen(Smscmes, Stmrmes, iermes)

    port = MessgCom
    BaudRate = MessgBaud
    ret = OpenToCmd(port, BaudRate)
    If ret <> CMD_OK Then
      MsgBox(Texxt(3526))
      istat = -1
      Exit Sub
    End If

    'If iermes <> 0 Then
    ' Call PoClose(Smscmes, iermes)
    ' istat = -1
    ' Exit Sub
    'End If
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
      Application.DoEvents()
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
        If MessgDriver = "MI3" Then
          Call MinMes(0, kw, rmes, istat, MscMes)
        Else
          MsgBox(Texxt(3542) & MessgDriver)
        End If
        If istat <> NuWrt Then Exit For
        ' Call NormRwrt(-1, kw, rmes)
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
    '
    'Call PoClose(Smscmes, iermes)
    ret = CloseToCmd(port)
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


  Sub MINrwert(ByVal IBASF As Integer, ByVal BUF As String, ByRef r() As Single, ByRef istat As Integer)
    '      CHARACTER*1 LF,STE,FR
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim Rh(38) As Single
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
    '
    '
    '

    '
    j = 1
    k = InStr(Mid(BUF, j), ",")
    For i = 0 To 38
      j = InStr(Mid(BUF, k + 1), ",")
      If j = 0 Then
        j = InStr(Mid(BUF, k + 1), Chr(0))
      End If
      Rh(i) = 0.01 * Val(Mid(BUF, k + 1, j - 1))
      k = k + j
    Next i
    If NormfileID = 1 Then
      j = 4
      For i = 0 To MessgNwe - 1
        r(i) = Rh(j)
        j = j + 2
      Next i
    ElseIf NormfileID = 2 Then
      j = 4
      For i = 0 To MessgNwe - 1
        r(i) = Rh(j)
        j = j + 1
      Next i

    Else
      For i = 0 To MessgNwe - 1
        r(i) = Rh(i)
      Next i
    End If
  End Sub



  Sub MinMes(ByVal imess As Integer, ByVal kw As Integer, ByRef r() As Single, ByRef istat As Integer, _
  ByVal mscMES As SerialPort)
    Dim CHRM As String
    Dim IGL As String
    Dim i As Integer
    Dim LenPos As String
    Dim Sts As String
    Dim UV As String
    Dim CU(0 To 6) As String
    '
    Dim IBASF As Integer
    CrLf = Chr(0)
    CU(0) = "MES"
    CU(1) = "CAL"
    CU(2) = "UZC"
    CU(3) = " "
    CU(4) = "CAL"
    CU(5) = "UZC"
    CU(6) = " "
    '
    '     Null-Kalibrieren nicht unterstützt
    '
    '
    '
    If imess = 3 Or imess = 6 Then
      Exit Sub
    End If
    '
    '
    '
    '     NB=11    360-740 , 10 NWE=39
    '     NB=1     400-700 , 20 NWE=16
    '     Beispiel
    '
    '
    CHRM = MessgChrm(kw)
    '
    '     Request output of measurement parameters
    '
    '
    '
    '
    stv = "CPR" & CrLf
    BufSize = 50
    Puffer = Space(BufSize - 1)
    ret = SendToCmd(stv, Puffer, BufSize)

    If ret <> CMD_OK Then
      Exit Sub
    End If
    LenPos = Mid(Puffer, 11, 1)
    UVein = Mid(Puffer, 13, 3)

    If kw = 0 And (imess = 2 Or imess = 5) Then
      '
      '           Zustand abfragen
      '
      stv = "IDR" & CrLf
      BufSize = 40
      Puffer = Space(BufSize - 1)
      ret = SendToCmd(stv, Puffer, BufSize)

      If ret <> CMD_OK Then
        MsgBox(Texxt(3527) & Space(1) & CStr(ret) & " ( " & Mid(stv, 1, Len(stv) - 1) & ")")
        istat = -1
        GoTo Min36D
      End If
      '
      stv = "STR" & CrLf
      BufSize = 50
      Puffer = Space(BufSize - 1)
      ret = SendToCmd(stv, Puffer, BufSize)
      If ret <> CMD_OK Then
        MsgBox("ERROR")
      End If
      If Mid(Puffer, 6, 1) = "1" Then
        MsgBox("No Lamp Charge")
      End If

      '
      '
      '
      'Parameter setzen
      '
      '

      Sts = "CPS,01,4," & LenPos & "," & UVein & "," & "0" & CrLf
      '
      '
      '
      If imess = 2 Then
        '
        'Reflexion
        '
        '
        '
        If InStr(CHRM, "0") = 0 And InStr(CHRM, "O") = 0 Then
          If MessgKM < 2 Then
            '
            'Nur Gloss included
            '
            Mid(Sts, 8, 1) = "0"
          End If
        End If
      ElseIf imess = 5 Then
        '
        '
        'Transmission
        '
        '
        Mid(Sts, 8, 1) = "3"
        Mid(Sts, 16, 1) = "1"

      End If
      BufSize = 50
      Puffer = Space(BufSize - 1)
      ret = SendToCmd(Sts, Puffer, BufSize)
      If ret <> CMD_OK Then
        stv = Sts
        istat = -1
        GoTo Min36D
      End If
      '
      '
      '
    End If
    '
    '


    '
    stv = CU(imess) & CrLf
    Select Case imess
      Case 0
        '
        If kw = 0 Then
          '
          '
          '
          'Prüfen, ob kalibriert
          '
          '
          '
          '
          '
          '
          '
          '
          Sts = "STR" & CrLf
          BufSize = 50
          Puffer = Space(BufSize - 1)
          ret = SendToCmd(Sts, Puffer, BufSize)
          If Mid(Puffer, 8, 1) = "0" Then
            MsgBox(Texxt(3502))
            stv = Sts
            istat = -1
            GoTo Min36D
          End If
          '


          '

          'Messung auslösen
          '
          BufSize = 300
          Puffer = Space(BufSize - 1)
          ret = SendToCmd(stv, Puffer, BufSize)

          If ret <> CMD_OK Then
            istat = -1
            GoTo Min36D
          End If
        End If
        '
        '
        'Messungwerte übernehmen
        '
        BufSize = 300
        '





        If ReTr = 1 Then
          '
          '
          'Transmission
          '
          '
          '
          IGL = 3

        Else
          '
          '
          '
          'Reflexion
          '
          '
          '
          '
          If InStr(CHRM, "0") = 0 And InStr(CHRM, "O") = 0 Then
            '
            '
            'gloss incuded
            IGL = "0"
          Else
            '
            '
            'gloss excluded
            IGL = "1"
          End If
        End If
        UV = 1
        If UVein = "999" Then
          UV = "0"
        End If
        stv = "MDR," & IGL & UV & CrLf
        Puffer = Space(BufSize - 1)
        ret = SendToCmd(stv, Puffer, BufSize)

        If ret <> CMD_OK Then
          istat = -1
          GoTo Min36D
        End If
        IBASF = 0

        Call MINrwert(IBASF, Puffer, r, istat)
      Case 1
        '
        'Weißkalibrierung(Reflexion)
        '
        '
        '

        '
        If kw = 0 Then
          BufSize = 50
          Puffer = Space(BufSize - 1)
          ret = SendToCmd(stv, Puffer, BufSize)

          If ret <> CMD_OK Then
            istat = -1
            GoTo Min36D
          End If
        End If
        For i = 0 To MessgNwe - 1
          r(i) = 1.0#
        Next i
        '
        '
      Case 4
        '
        'Weißkalibrierung(Transmission)
        '
        '
        '
        '
        If kw = 0 Then

          BufSize = 50
          Puffer = Space(BufSize - 1)
          ret = SendToCmd(stv, Puffer, BufSize)

          If ret <> CMD_OK Then
            istat = -1
            GoTo Min36D
          End If
        End If
        For i = 0 To MessgNwe - 1
          r(i) = 1.0#
        Next i
        '
        '
      Case 2
        '
        '
        'Schwarzkalibrierung(Reflexion)
        '
        '
        '
        '
        If kw = 0 Then
          '
          '
          '

          If ret <> CMD_OK Then
            istat = -1
            MsgBox(Texxt(3500))
            ret = CloseToCmd(port)
            Exit Sub
          End If

          BufSize = 50
          Puffer = Space(BufSize - 1)
          ret = SendToCmd(stv, Puffer, BufSize)

          If ret <> CMD_OK Then
            If Mid(Puffer, 1, 4) = "OK02" Then
              MsgBox("Light output by xenon lamp is low")
            Else
              istat = -1
              GoTo Min36D
            End If
          End If
        End If
        For i = 0 To MessgNwe - 1
          r(i) = 0.0#
        Next i
      Case 5
        '
        '
        'Schwarzkalibrierung(Transmission)
        '
        '
        '
        '
        If kw = 0 Then


          If ret <> CMD_OK Then
            istat = -1
            MsgBox(Texxt(3500))
            ret = CloseToCmd(port)
            Exit Sub
          End If

          BufSize = 50
          Puffer = Space(BufSize - 1)
          ret = SendToCmd(stv, Puffer, BufSize)

          If ret <> CMD_OK Then
            istat = -1
            GoTo Min36D
          End If
        End If
        For i = 0 To MessgNwe - 1
          r(i) = 0.0#
        Next i
    End Select
    '
    '
    '
    '
    '
    '

    Exit Sub
Min36D:
    MsgBox(Texxt(3527) & Space(1) & CStr(ret) & " ( " & Mid(stv, 1, Len(stv) - 1) & ")", 0, Texxt(2000))
    istat = -1
  End Sub


  Sub MesIni()
    istat = NuWrt
    If MessgDriver = "MI3" Then
      Call MINini(istat)
    Else
      MsgBox(Texxt(3542) & MessgDriver)
    End If
  End Sub
  Sub MINini(ByRef istat As Integer)
    '
    Dim i As Integer
    Dim CrLf As String
    '
    CrLf = Chr(0)
    '
    '
    '
    NoEinst = True
    lblspe(0).Visible = True
    cboSPE(0).Visible = True
    MNlblWID.Visible = True
    MNtxtWID.Visible = True
    lblspe(1).Visible = True
    cboSPE(1).Visible = True
    '
    '
    '
    'Apertureinstellung ermitteln
    '
    '
    '
    'Call PoOpen(Smscmes, Stmrmes, iermes)
    port = MessgCom
    BaudRate = MessgBaud
    ret = OpenToCmd(port, BaudRate)
    If ret <> 0 Then
      MsgBox(Texxt(3526))
      ret = CloseToCmd(port)
      Exit Sub
    End If

    '
    '
    'Read Measurement condition (white calibration plate)
    '
    '
    '
    stv = "CIR" & CrLf
    BufSize = 50
    Puffer = Space(BufSize - 1)
    ret = SendToCmd(stv, Puffer, BufSize)
    MNtxtWID.Text = InputBox(Texxt(3902), Texxt(2000), Mid(Puffer, 6, 8))
    stv = "CIS," & Trim(CStr(MNtxtWID.Text)) & CrLf
    BufSize = 50
    Puffer = Space(BufSize - 1)
    ret = SendToCmd(stv, Puffer, BufSize)
    If ret <> CMD_OK Then
      Exit Sub
    End If
    '
    cboSPE(0).Items.Clear()
    cboSPE(0).Items.Add("LAV")
    cboSPE(0).Items.Add("MAV")
    cboSPE(0).Items.Add("SAV")
    '
    cboSPE(1).Items.Clear()
    cboSPE(1).Items.Add("999")
    cboSPE(1).Items.Add("040")
    cboSPE(1).Items.Add("042")
    '
    'Read Measurement parameters
    '
    '
    '
    stv = "CPR" & CrLf
    BufSize = 50
    Puffer = Space(BufSize - 1)
    ret = SendToCmd(stv, Puffer, BufSize)

    If ret <> CMD_OK Then
      Exit Sub
    End If
    cboSPE(0).SelectedIndex = CInt(Mid(Puffer, 11, 1))
    '
    UVein = Mid(Puffer, 13, 3)
    cboSPE(1).Text = UVein
    '
    cboSPE(1).SelectedIndex = -1
    For i = 0 To cboSPE(1).Items.Count - 1
      If cboSPE(1).Items(i) = UVein Then
        cboSPE(1).SelectedIndex = i
        Exit For
      End If
    Next i
    '
    ret = CloseToCmd(port)
    NoEinst = False
  End Sub
  Sub einst(ByVal Index As Integer)

    '
    '
    'Einstellungen abspeichern
    '
    '
    If NoEinst Then Exit Sub
    CrLf = Chr(0)
    port = MessgCom
    BaudRate = MessgBaud
    ret = OpenToCmd(port, BaudRate)
    If ret <> 0 Then
      MsgBox(Texxt(3526))
      ret = CloseToCmd(port)
      istat = -1
      Exit Sub
    End If

    '
    '
    '
    '
    '
    stv = "CIS," & Trim(MNtxtWID.Text) & CrLf
    BufSize = 50
    Puffer = Space(BufSize - 1)
    ret = SendToCmd(stv, Puffer, BufSize)

    If ret <> CMD_OK Then
      MsgBox(Texxt(3500))
      ret = CloseToCmd(port)
      istat = -1
      Exit Sub
    End If
    '
    'Read Measurement parameters
    '
    '
    '
    stv = "CPR" & CrLf
    BufSize = 50
    Puffer = Space(BufSize - 1)
    ret = SendToCmd(stv, Puffer, BufSize)
    If ret <> CMD_OK Then
      Exit Sub
    End If

    '
    'Write Measurement condition (number maes.,SCI/SCE,LENS pos.,UV,mode Refl. or trans.)
    '
    '
    '
    UVein = cboSPE(1).Text
    'inolta 3600d

    stv = "CPS,01,4," & Trim(CStr(cboSPE(0).SelectedIndex)) & "," & UVein & "," & Mid(Puffer, 17, 1) & CrLf
    BufSize = 50
    Puffer = Space(BufSize - 1)
    ret = SendToCmd(stv, Puffer, BufSize)

    If ret <> CMD_OK Then
      istat = -1
      MsgBox(Texxt(3500))
      ret = CloseToCmd(port)
      Exit Sub
    End If

    ret = CloseToCmd(port)
    Sqlstmt = "UPDATE " & TableKalib & " SET [KALIB_DATTIM]=" _
    & Sqldati(Now.AddDays(-1)) & " WHERE MESSG_ID=" & MessgID & " AND MESSG_KENN='" & MessgKenn & "'"
    'Call SQLExeLock(0, Dkal, Sqlstmt, Mnier)
    CmdKal.CommandText = Sqlstmt
    If SQLExeNonQuery(CmdKal, Cnkal) <> 0 Then
      MsgBox("ERROR CNKAL")
    End If
  End Sub


  Sub MesKal(ByVal imess As Integer)
    Dim jj As Integer
    Dim j As Integer
    Dim kw As Integer
    istat = NuWrt
    WinChk = False
    NXTChk = False
    COMchk = False
    MNbtnABR.Visible = True
    MNbtnABR.Enabled = True
    'Call PoOpen(Smscmes, Stmrmes, iermes)

    port = MessgCom
    BaudRate = MessgBaud

    ret = OpenToCmd(port, BaudRate)
    If ret <> CMD_OK Then
      MsgBox(Texxt(3526))
      istat = -1
      Exit Sub
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
        If MessgDriver = "MI3" Then
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

    'Call PoClose(Smscmes, iermes)
    ret = CloseToCmd(port)
  End Sub
 



  





  Sub New()
    MyBase.New()
    istat = 0
    MnReTr = 0
  End Sub
  Sub dispose() Implements IDisposable.Dispose
  End Sub
End Class