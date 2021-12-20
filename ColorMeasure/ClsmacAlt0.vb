Option Strict Off
Option Explicit On
Option Compare Text
Friend Class clsMAC
    Implements IDisposable

    '
    '        MC0
    '        COLOR Checker (360-750nm(10))
    '
    '
    '
    '        MC1
    '        MACBETH 7000 (360-750nm(10))
    '
    '
    '
    '
    '        MC2
    '        MACBETH 2020+,3000,3100 (360-740nm(20))
    '
    '        MC3
    '        MACBETH MS20 (400-700(20))
    '
    '        MC4
    '        MACBETH CE-742 GL (360-750(10)) Mehrwinkel (
    '
    '        MC5
    '        MACBETH CE-2180 GL (360-750(10)) Kugel
    '
    '        MC6
    '        MACBETH COLOR i 5(Ci5) GL (360-750(5)) Kugel

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


    Sub CHKini(ByRef istat As Short)

    End Sub
    Sub CE2180ini(ByRef istat As Short)

    End Sub

    Sub CE742ini(ByRef istat As Short)

    End Sub
    Sub MACini(ByRef istat As Short)

    End Sub
    Sub CI5ini(ByRef istat As Short)
        '
        Dim ive As Short
        Dim CHRM As String
        Dim i As Short
        Dim iver As Short
        Dim CrLf As String
        Dim APECH As String
        Dim Aper As String
        Dim UVein As String
        Dim resp As Short
        Dim delay As Short
        Dim wart As Short
        Dim buflen As Short

        '
        Dim IBASF As Short
        buflen = 4
        iver = 5
        wart = 5
        delay = 10
        resp = 2

        CrLf = Chr(13)
        '
        '
        '
        'NoEinst = True
      lblSpe(0).Visible = True
      cboSpe(0).Visible = True
      'mnlblwid.Visible = True
      'mntxtwid.Visible = True
      lblSpe(1).Visible = True
      cboSpe(1).Visible = True
      '
      '
      '
      'Apertureinstellung ermitteln
      '
      '
      '

      '   Port = CLng(MessgCom)
      '   BaudRate = CLng(MessgBaud)
      '   ret = OpenToCmd(Port, BaudRate)
      '   If ret <> 0 Then
      '     MsgBox Texxt(3526)
      '     ret = CloseToCmd(Port)
      '     Exit Sub
      '   End If
      '
      '
      '
      'Read Measurement condition (white calibration plate)
      '
      '
      '
      '   stv = "CIR" & CrLf
      '   BufSize = 50
      '   ret = SendToCmd(stv, Puffer, BufSize)
      '   mntxtwid.Text = InputBox(Texxt(3902), Texxt(2000), Mid(Puffer, 6, 8))
      '   stv = "CIS," & Trim(CStr(mntxtwid.Text)) & CrLf
      '   BufSize = 50
      '   ret = SendToCmd(stv, Puffer, BufSize)
      '   If ret <> CMD_OK Then
      '        Exit Sub
      '   End If
      '
      cboSpe(0).Items.Clear()
      cboSpe(0).Items.Add("LAV")
      cboSpe(0).Items.Add("MAV")
      cboSpe(0).Items.Add("SAV")
      '
      '
      'Read Measurement parameters
      '
      '
      '
      '   stv = "CPR" & CrLf
      '   BufSize = 50
      '   ret = SendToCmd(stv, Puffer, BufSize)
      '
      '   If ret <> CMD_OK Then
      '        Exit Sub
      '   End If
      '   cbospe(0).ListIndex = CInt(Mid(Puffer, 11, 1))
      '
      '
      '   ret = CloseToCmd(Port)
      'NoEinst = False


      '
      'Listindex UVkal
      '
      '
      cboSpe(1).Items.Clear()
      cboSpe(1).Items.Add("D65")
      cboSpe(1).Items.Add("UV excluded")
      cboSpe(1).Items.Add("adj1")
      cboSpe(1).Items.Add("adj2")
      'cbospe(1).AddItem "4"
      'cbospe(1).AddItem "5"
      'cbospe(1).AddItem "6"
      'cbospe(1).AddItem "7"
      Call PoOpen(MscMes, iermes)
      If iermes <> 0 Then
         Call PoClose(MscMes, iermes)
         istat = -1
         Exit Sub
      End If
      '
      '
      '
      '
      'Konfigurieren Messger‰temodell
      '
      '
      '

      stv = "CONFIG -MODEL CE7000A" & CrLf
      Call GetComBuf(resp, stv, BUF, 5, iver, delay, wart, 3506, 1, "", 1, "?????????????s", MscMes, istat)
      If istat <> NuWrt Then Exit Sub


      stv = "STATUS" & CrLf
      Call GetComBuf(resp, stv, BUF, 5, iver, delay, wart, 3506, 1, "", 1, "????", MscMes, istat)
      Call PoClose(MscMes, iermes)

      If istat <> NuWrt Then Exit Sub
      cboSpe(0).SelectedIndex = CShort(Mid(BUF, 6, 1))
      cboSpe(1).SelectedIndex = CShort(Mid(BUF, 5, 1))
      cboSpe(0).Enabled = False


      '
      '
      'Testen
      '
      '       mscmes.Output = "MODELS" & Chr(13)
      '       MsgBox mscmes.InBufferCount

      '       MsgBox mscmes.Input
      '       stv = "R" & CrLf
      '       Call GetComBuf(resp, stv, BUF, 5, iver, delay, wart, _
      ''       3506, 1, "", 1, "????", mscMES , istat)
      '       If istat <> NuWrt Then Exit Sub



   End Sub

   Sub M20ini(ByRef istat As Short)

   End Sub
   Sub MS20ini(ByRef istat As Short)

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
      If Reftra <> "R" And (MessgDriver <> "MC1" And MessgDriver <> "MC6") Then
         MsgBox(Texxt(3590) & "  (" & Reftra & ")")
         istat = -1
         Exit Sub
      End If
      If MessgDriver = "MC0" Then
         '
         '
         'Color checker
         '
         '
         '
         If NormfileID <> 14 And NormfileID <> 1 Then
            MsgBox(Texxt(3519))
            istat = -1
            Exit Sub
         End If
         idka(0) = 1
         idka(1) = 0
         idka(2) = 0
      ElseIf MessgDriver = "MC1" Then
         '
         '
         'Macbeth 7000
         '
         '
         '
         '
         If NormfileID <> 14 And NormfileID <> 1 Then
            MsgBox(Texxt(3519))
            istat = -1
            Exit Sub
         End If
         If Reftra = "R" Then
            idka(0) = 2
            idka(1) = 1
            idka(2) = 0
         ElseIf Reftra = "A" Then
            idka(0) = 5 'Transmission(schwarz)
            idka(1) = 2 'Reflexion(schwarz)
            idka(2) = 4 'Transmission(weiﬂ)
            idka(3) = 1 'Reflexion(weiﬂ)
            idka(4) = 0
         ElseIf Reftra = "T" Then
            idka(0) = 5
            idka(1) = 4
            idka(2) = 0
            'MsgBox Texxt(3590) & "  (" & RefTra & ")"
            'istat = -1
            'Exit Sub
         End If
      ElseIf MessgDriver = "MC2" Then
         '
         '
         '16 oder 1 f¸r 2020+ ; 2 oder 1 f¸r X-Rite-Emulation
         '
         '
         If NormfileID <> 16 And NormfileID <> 1 And NormfileID <> 2 Then
            MsgBox(Texxt(3519))
            istat = -1
            Exit Sub
         End If
         idka(0) = 1
         idka(1) = 2
         idka(2) = 0
      ElseIf MessgDriver = "MC3" Then
         '
         '
         'MS20
         '
         '
         If NormfileID <> 1 Then
            MsgBox(Texxt(3519))
            istat = -1
            Exit Sub
         End If
         idka(0) = 1
         idka(1) = 2
         idka(2) = 0
      ElseIf MessgDriver = "MC4" Then
         '
         '
         'CE-742 GL
         '
         '
         If NormfileID <> 1 And NormfileID <> 14 Then
            MsgBox(Texxt(3519))
            istat = -1
            Exit Sub
         End If
         idka(0) = 1
         idka(1) = 2
         idka(2) = 0

      ElseIf MessgDriver = "MC5" Then
         '
         '
         'Color checker
         '
         '
         '
         If NormfileID <> 14 And NormfileID <> 1 Then
            MsgBox(Texxt(3519))
            istat = -1
            Exit Sub
         End If
         idka(0) = 1
         idka(1) = 2
         idka(2) = 0
      ElseIf MessgDriver = "MC6" Then
         '
         '
         'Macbeth Ci5
         '
         '
         '
         '
         If NormfileID <> 14 And NormfileID <> 10 And NormfileID <> 1 And NormfileID <> 2 Then
            MsgBox(Texxt(3519))
            istat = -1
            Exit Sub
         End If
         If Reftra = "R" Then
            idka(0) = 1
            idka(1) = 2
            idka(2) = 0
         ElseIf Reftra = "A" Then
            idka(0) = 4       'Transmission(weiﬂ)
            idka(1) = 1       'Reflexion(weiﬂ)
            idka(2) = 5       'Transmission(schwarz)
            idka(3) = 2       'Reflexion(schwarz)
            idka(4) = 0

         ElseIf Reftra = "T" Then
            idka(0) = 4
            idka(1) = 5
            idka(2) = 0
            'MsgBox Texxt(3590) & "  (" & RefTra & ")"
            'istat = -1
            'Exit Sub
         End If

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
         For kw = 0 To MessgKM - 1
            '
            '        COLOR Checker
            '
            If MessgDriver = "MC0" Then
               Call CHKMes(0, kw, rmes, istat, MscMes)
               '
               '
               '        MACBETH 7000
               '
               '
            ElseIf MessgDriver = "MC1" Then
               Call MACMes(0, kw, rmes, istat, MscMes)
               '
               '
               '        MACBETH 2020+
               '
               '
            ElseIf MessgDriver = "MC2" Then
               Call M20Mes(0, kw, rmes, istat, MscMes)
               '
               '        MACBETH MS20
               '
               '
            ElseIf MessgDriver = "MC3" Then
               Call MS20Mes(0, kw, rmes, istat, MscMes)
               '
               '        MACBETH CE-742 GL
               '
               '
            ElseIf MessgDriver = "MC4" Then
               Call CE742Mes(0, kw, rmes, istat, MscMes)
            ElseIf MessgDriver = "MC5" Then
               Call CE2180Mes(0, kw, rmes, istat, MscMes)
            ElseIf MessgDriver = "MC6" Then
               Call CI5Mes(0, kw, rmes, istat, MscMes)
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

   Public Function Autom() As Short
      Autom = 0
   End Function

   Sub einst(ByRef Index As Short)
      Dim iver As Short
      Dim delay As Short
      Dim wart As Short
      Dim resp As Short
      Dim buf As String
      '
      '
      'Einstellungen beim Initialisieren des Messger‰tes
      '
      '
      If MessgDriver = "MC6" Then
         If MscMes.IsOpen Then
            Exit Sub
         End If
         istat = NuWrt
         iver = 10
         wart = 50
         delay = 10
         resp = 2
         Call PoOpen(MscMes, iermes)
         cboSpe(0).Enabled = False
         cboSpe(1).Enabled = False
         If iermes <> 0 Then
            Call PoClose(MscMes, iermes)
            cboSpe(0).Enabled = True
            cboSpe(1).Enabled = True
            istat = -1
            Exit Sub
         End If
         CrLf = Chr(13)
         '
         '
         'Apertur-Grˆﬂe (Blenden-Grˆﬂe)
         '
         '
         If Index = 1 Then
            stv = "CONFIG -uv " & CStr(cboSpe(1).SelectedIndex) & CrLf
            '

            Call GetComBuf(resp, stv, buf, 5, iver, delay, wart, 3506, 1, "", 1, "????", MscMes, istat)
            If istat <> NuWrt Then Exit Sub


            'MsgBox BUF, 0
         End If
         Call PoClose(MscMes, iermes)
         cboSpe(0).Enabled = True
         cboSpe(1).Enabled = True
      End If
      '
      '
   End Sub



   Sub CHKrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
      '      CHARACTER*1 LF,STE,FR
      Dim i As Short
      Dim j As Short
      Dim l As Short
      Dim k As Short
      Dim Smes As Single
      Dim Sref As Single
      Dim tex As String
      '
      'Statuszeile entfernen
      '
      '

      If Len(BUF) < 300 Then
         imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
         If imsg = 7 Then
            istat = -1
         Else
            istat = 1
         End If
         Exit Sub
      End If
      'MsgBox Len(BUF)

      If NormfileID = 1 Then
         '
         '16 Wellenl‰ngen 400-700 (20) nm
         '
         '
         '
         i = InStr(BUF, ".")
         i = i + InStr(Mid(BUF, i + 1), ".")
         i = i + InStr(Mid(BUF, i + 1), ".")
         i = i + InStr(Mid(BUF, i + 1), ".")
         For j = 1 To MessgNwe
            i = i + InStr(Mid(BUF, i + 1), ".")

            tex = Mid(BUF, i - 3, 6)
            r(j) = Val(tex)
            'MsgBox tex & "  " & I & "  " & CStr(380 + J * 20) & "  " & r(J)
            i = i + InStr(Mid(BUF, i + 1), ".")
         Next j
      Else
         '
         '40 Wellenl‰ngen 360-750 (10) nm
         '
         '
         '
         i = InStr(BUF, ".")
         For j = 1 To MessgNwe
            tex = Mid(BUF, i - 3, 6)
            r(j) = Val(tex)
            'MsgBox I & "  " & CStr(350 + J * 10) & "  " & r(J)
            i = i + InStr(Mid(BUF, i + 1), ".")
         Next j
      End If
      '
      '

   End Sub
   Sub CE742rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
      '      CHARACTER*1 LF,STE,FR
      Dim i As Short
      Dim j As Short
      Dim l As Short
      Dim k As Short
      Dim Smes As Single
      Dim Sref As Single
      Dim tex As String
      '
      'Statuszeile entfernen
      '
      '
      'MsgBox BUF
      'MsgBox Len(BUF)
      If Len(BUF) < 300 Then
         imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
         If imsg = 7 Then
            istat = -1
         Else
            istat = 1
         End If
         Exit Sub
      End If
      'MsgBox Len(BUF)

      If NormfileID = 1 Then
         '
         '16 Wellenl‰ngen 400-700 (20) nm
         '
         '
         '
         i = InStr(BUF, ".")
         i = i + InStr(Mid(BUF, i + 1), ".")
         i = i + InStr(Mid(BUF, i + 1), ".")
         i = i + InStr(Mid(BUF, i + 1), ".")
         For j = 1 To MessgNwe
            i = i + InStr(Mid(BUF, i + 1), ".")

            tex = Mid(BUF, i - 3, 6)
            r(j) = Val(tex)
            'MsgBox tex & "  " & I & "  " & CStr(380 + J * 20) & "  " & r(J)
            i = i + InStr(Mid(BUF, i + 1), ".")
         Next j
      Else
         '
         '40 Wellenl‰ngen 360-750 (10) nm
         '
         '
         '
         i = InStr(BUF, ".")
         For j = 1 To MessgNwe
            tex = Mid(BUF, i - 3, 6)
            r(j) = Val(tex)
            'MsgBox I & "  " & CStr(350 + J * 10) & "  " & r(J)
            i = i + InStr(Mid(BUF, i + 1), ".")
         Next j
         '         MsgBox CStr(r(1 + (kw - 1) * MessgNwe)) & Space(3) & r(MessgNwe + (kw - 1) * MessgNwe)
      End If
      '
      '

   End Sub

   Sub MACrwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
      '      CHARACTER*1 LF,STE,FR
      Dim i As Short
      Dim j As Short
      Dim l As Short
      Dim k As Short
      Dim Smes As Single
      Dim Sref As Single
      Dim tex As String
      '
      'Statuszeile entfernen
      '
      '
      ' MsgBox Len(BUF)
      If Len(BUF) < 400 Then
         imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
         If imsg = 7 Then
            istat = -1
         Else
            istat = 1
         End If
         Exit Sub
      End If
      'MsgBox Len(BUF)
      'MsgBox BUF
      If NormfileID = 1 Then
         '
         '16 Wellenl‰ngen 400-700 (20) nm
         '
         '
         '
         i = InStr(BUF, ".")
         i = i + InStr(Mid(BUF, i + 1), ".")
         i = i + InStr(Mid(BUF, i + 1), ".")
         i = i + InStr(Mid(BUF, i + 1), ".")
         For j = 1 To MessgNwe
            i = i + InStr(Mid(BUF, i + 1), ".")

            tex = Mid(BUF, i - 4, 8)
            r(j) = Val(tex)
            'MsgBox tex & "  " & I & "  " & CStr(380 + J * 20) & "  " & r(J)
            i = i + InStr(Mid(BUF, i + 1), ".")
         Next j
      Else
         '
         '40 Wellenl‰ngen 360-750 (10) nm
         '
         '
         '
         i = InStr(BUF, ".")
         For j = 1 To MessgNwe
            tex = Mid(BUF, i - 4, 8)
            r(j) = Val(tex)
            'MsgBox I & "  " & CStr(350 + J * 10) & "  " & r(J)
            i = i + InStr(Mid(BUF, i + 1), ".")
         Next j
      End If
      '
      '

   End Sub
   Sub CI5rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
      Dim i As Short
      Dim j As Short
      Dim l As Short
      Dim k As Short
      Dim Smes As Single
      Dim Sref As Single
      Dim tex As String
      Dim rhi(79) As Single
      '
      'Statuszeile entfernen
      '
      '
      For i = 1 To Len(BUF)
         If Mid(BUF, i, 1) = Chr(13) Then
            Mid(BUF, i, 1) = " "
         End If
         If Mid(BUF, i, 1) = Chr(10) Then
            Mid(BUF, i, 1) = " "
         End If
         If Mid(BUF, i, 1) = "," Then
            Mid(BUF, i, 1) = " "
         End If

      Next i
      If Len(BUF) < 650 Then
         imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
         If imsg = 7 Then
            istat = -1
         Else
            istat = 1
         End If
         Exit Sub
      End If
      '
      '79 Wellenl‰ngen 360-750 (5) nm
      '
      '
      '
      i = InStr(BUF, ".")
      For j = 1 To 79
         tex = Mid(BUF, i - 4, 8)
         rhi(j) = Val(tex)
         'MsgBox I & "  " & CStr(350 + J * 10) & "  " & r(J)
         i = i + InStr(Mid(BUF, i + 1), ".")
      Next j
      Select Case NormfileID
         Case 1
            '
            '
            ' 400-700(20)
            '
            '
            i = 9
            For j = 1 To MessgNwe
               r(j - 1) = rhi(i)
               i = i + 4
            Next j
         Case 2
            '
            '
            ' 400-700(10)
            '
            '
            i = 9
            For j = 1 To MessgNwe
               r(j - 1) = rhi(i)
               i = i + 2
            Next j

         Case 10
            '
            '
            ' 360-750 (5)
            '
            '
            i = 1
            For j = 1 To MessgNwe
               r(j - 1) = rhi(i)
               i = i + 1
            Next j

         Case 14
            '
            '
            ' 360-750 (10)
            '
            '
            i = 1
            For j = 1 To MessgNwe
               r(j - 1) = rhi(i)
               i = i + 2
            Next j

      End Select
      '
      '

   End Sub

   Sub M20rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
      '      CHARACTER*1 LF,STE,FR
      Dim i As Short
      Dim j As Short
      Dim l As Short
      Dim k As Short
      Dim Smes As Single
      Dim Sref As Single
      Dim tex As String
      Dim Rh(40) As Single
      Dim Nww As Short
      '
      'Statuszeile entfernen
      '
      '
      'MsgBox BUF
      ' MsgBox Len(BUF)
      If Len(BUF) < 100 Then
         GoTo Mac20Err
      End If
      'MsgBox Len(BUF)
      'MsgBox BUF
      '
      '20 Wellenl‰ngen 360-740 (20) nm
      '
      '
      '
      i = InStr(BUF, ".")
      For j = 1 To 40
         tex = Mid(BUF, i - 3, 6)
         Rh(j) = Val(tex)
         Nww = j
         'MsgBox I & "  " & CStr(350 + J * 10) & "  " & r(J)
         k = InStr(Mid(BUF, i + 1), ".")
         If k = 0 Then Exit For
         i = i + k
      Next j
      '
      '
      '       Original Mcbeth 2020+ (20 Wellenl‰ngen)360_740 (20)
      '
      '
      If Nww = 20 Then
         If MessgNwe = 20 Then
            For i = 1 To MessgNwe
               r(i) = Rh(i)
            Next i
         ElseIf MessgNwe = 16 Then
            For i = 1 To MessgNwe
               r(i) = Rh(i + 2)
            Next i
         Else
            GoTo Mac20Err
         End If
         '
         '
         '       X-Rite simulation von Mcbeth 2020+ (31 Wellenl‰ngen) 400_700 (10)
         '
         '
      ElseIf Nww = 31 Then
         If MessgNwe = 31 Then
            For i = 1 To MessgNwe
               r(i) = Rh(i)
            Next i
         ElseIf MessgNwe = 16 Then
            For i = 1 To MessgNwe
               r(i) = Rh(2 * (i - 1) + 1)
            Next i
         Else
            GoTo Mac20Err
         End If

      Else
         GoTo Mac20Err
      End If
      Exit Sub
Mac20Err:
      imsg = MsgBox(Texxt(3507) & Chr(13) & Texxt(1999), 4, Texxt(2000))
      If imsg = 7 Then
         istat = -1
      Else
         istat = 1
      End If
      Exit Sub

   End Sub
   Sub MS20rwert(ByRef IBASF As Short, ByRef BUF As String, ByRef r() As Single, ByRef istat As Short)
      '      CHARACTER*1 LF,STE,FR
      Dim i As Short
      Dim j As Short
      Dim k As Short
      '
      If Len(BUF) < 100 Then
         Exit Sub
      End If
      'MsgBox Len(BUF)
      'MsgBox BUF
      k = 12
      For i = 1 To MessgNwe
         'MsgBox Mid(BUF, k, 6)
         r(i) = Val(Mid(BUF, k, 6))
         If i = MessgNwe Then Exit For
         j = InStr(Mid(BUF, k), ",")
         'MsgBox i & "  " & j
         If j > 7 Then
            j = InStr(Mid(BUF, k), Chr(10))
         End If
         k = k + j
         'MsgBox r(i)
      Next i
   End Sub
























   Sub CHKMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
      Dim ive As Short
      Dim CHRM As String
      Dim i As Short
      Dim iver As Short
      Dim buflen As Short
      Dim wart As Short
      Dim delay As Short
      Dim CU(6) As String
      Dim STW As String
      '
      Dim IBASF As Short
      CrLf = Chr(13)
      buflen = 4
      iver = 2000
      wart = 10
      delay = 15
      CU(0) = "M"
      CU(1) = "C"
      CU(2) = " "
      CU(3) = " "
      CU(4) = " "
      CU(5) = " "
      CU(6) = " "
      '     COLORCHECKER  545
      '     Null-Kalibrieren nicht unterst¸tzt
      '
      '
      '
      '      If imess = 3 Or imess = 4 Or imess = 7 Then
      If imess > 1 Then
         Exit Sub
      End If
      '
      '
      '
      '     NB=1    400-700,20    NWE=16
      'Beispiel
      '
      '
      'Call GetComBuf(resp, Stv, BUF, buflen, iver, delay, wart, _
      '' 3500, CrLf, -25, "I", mscmes , ier)
      '
      CHRM = MessgChrm(kw)
      '      MsgBox CHRM & CStr(messg_ihrm(kw)), 0
      '
      '      Wake up
      '
      '
      '
      i = 0
      BUF = ""
      stv = "v" & CrLf
      Do While i < 10
         Call GetComString(Autom, stv, BUF, 0, iver, delay, wart, 3506, 1, "", 1, "", mscMES, istat)
         Call Wait(50)
         If Len(BUF) > 0 Then Exit Do
         i = i + 1
         Call Wait(100)
      Loop
      Call GetComString(Autom, stv, BUF, 1, iver, delay, wart, 3506, 1, "", 1, "v", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      Call Wait(50)
      ' MsgBox BUF
      If istat <> NuWrt Then Exit Sub
      'stv = "S" & CrLf
      'Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, _
      ''3506, 1, "", 1, "????", mscmes , istat)
      'If istat <> NuWrt Then Exit Sub
      Call Wait(50)

      '
      '
      If imess = 1 Then
         If kw = 1 Then
            '
            '      Zustand abfragen (bei Weiﬂstandard)
            '
            stv = "R" & CrLf
            Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3506, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            ' MsgBox BUF

            '
         End If
      End If
      '
      '
      '     Messung starten
      '
      '
      '
      stv = CU(imess) & CrLf
      '
      IBASF = 0
      If imess = 0 Then
         '
         'Messung
         '
         For i = 1 To 5
            Call Wait(100)
            'Call GetComString(Autom, stv, BUF, 308, iver, delay, 100, _
            ''3500, 1, "", 1, "11", mscmes , istat)
            'MsgBox Mid(BUF, 1, 2)
            Call GetComString(Autom, stv, BUF, 18, iver, delay, 200, 3500, 1, "", 1, "11", mscMES, istat)
            'MsgBox Len(BUF) & Space(3) & Mid(BUF, 1, 18)
            If Len(BUF) >= 308 Then
               If Mid(BUF, 8, 1) = "0" And Mid(BUF, 13, 1) = "0" Then Exit For
            End If
         Next i
         If istat <> NuWrt Then Exit Sub

         Call CHKrwert(IBASF, BUF, r, istat)
      ElseIf imess = 1 Then
         '
         'Weiﬂstandard
         '
         Call Wait(500)
         Call GetComString(Autom, stv, BUF, 4, 2 * iver, delay, wart, 3500, 1, "", 1, "1101", mscMES, istat)
         'MsgBox BUF
         If istat <> NuWrt Then Exit Sub
         '
         '
         For i = 1 To MessgNwe
            r(i) = 100.0#
         Next i
      ElseIf imess = 2 Then

         '
         'Schwarzstandard
         '
         For i = 1 To MessgNwe
            r(i) = 0.0#
         Next i
      End If
   End Sub
   Sub CE2180Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
      Dim ive As Short
      Dim CHRM As String
      Dim i As Short
      Dim iver As Short
      Dim buflen As Short
      Dim wart As Short
      Dim delay As Short
      Dim CU(6) As String
      Dim STW As String
      '
      Dim IBASF As Short
      CrLf = Chr(13)
      buflen = 4
      iver = 2000
      wart = 10
      delay = 15
      CU(0) = "M"
      CU(1) = "C"
      CU(2) = "B"
      CU(3) = " "
      CU(4) = " "
      CU(5) = " "
      CU(6) = " "
      '     COLORCHECKER  545
      '     Null-Kalibrieren nicht unterst¸tzt
      '
      '
      '
      '      If imess = 3 Or imess = 4 Or imess = 7 Then
      If imess > 2 Then
         Exit Sub
      End If
      '
      '
      '
      '     NB=1    400-700,20    NWE=16
      'Beispiel
      '
      '
      'Call GetComBuf(resp, Stv, BUF, buflen, iver, delay, wart, _
      '' 3500, CrLf, -25, "I", mscmes , ier)
      '
      CHRM = MessgChrm(kw)
      '      MsgBox CHRM & CStr(messg_ihrm(kw)), 0
      '
      '      Wake up
      '
      '
      '

      '
      '
      If imess = 1 Then
         If kw = 1 Then
            BUF = ""
            stv = "v" & CrLf
            Call GetComString(Autom, stv, BUF, 1, iver, delay, wart, 3506, 1, "", 1, "?", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            Call Wait(50)

            '
            '      Zustand abfragen (bei Weiﬂstandard)
            '
            stv = "R" & CrLf
            Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3506, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            ' MsgBox BUF

            '
         End If
      End If
      '
      '
      '
      '
      '     Glanzfalle  "J" = SCI (GM) "K" = SCE (GO)
      '
      '
      '
      '
      If Trim(CHRM) = "GM" Then
         stv = "J" & CrLf
      Else
         stv = "K" & CrLf
      End If

      Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3500, 1, "", 1, "??", mscMES, istat)

      '
      '     Messung starten
      '
      '
      '
      stv = CU(imess) & CrLf
      '
      IBASF = 0
      If imess = 0 Then
         '
         'Messung
         '
         Call GetComString(Autom, stv, BUF, 300, iver, delay, 200, 3500, 1, "", 1, "11", mscMES, istat)
         If istat <> NuWrt Then Exit Sub

         Call CHKrwert(IBASF, BUF, r, istat)
      ElseIf imess = 1 Then
         '
         'Weiﬂstandard
         '
         Call Wait(100)
         Call GetComString(Autom, stv, BUF, 18, 2 * iver, delay, wart, 3500, 1, "", 1, "????", mscMES, istat)
         'MsgBox BUF
         If istat <> NuWrt Then Exit Sub
         '
         '
         For i = 1 To MessgNwe
            r(i) = 100.0#
         Next i
      ElseIf imess = 2 Then
         Call Wait(100)
         Call GetComString(Autom, stv, BUF, 18, 2 * iver, delay, wart, 3500, 1, "", 1, "????", mscMES, istat)
         'MsgBox BUF
         If istat <> NuWrt Then Exit Sub
         '
         '
         For i = 1 To MessgNwe
            r(i) = 100.0#
         Next i

         '
         'Schwarzstandard
         '
         For i = 1 To MessgNwe
            r(i) = 0.0#
         Next i
      End If
   End Sub

   Sub CE742Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
      Dim ive As Short
      Dim CHRM As String
      Dim i As Short
      Dim iver As Short
      Dim buflen As Short
      Dim wart As Short
      Dim delay As Short
      Dim CU(6) As String
      Dim STW As String
      Dim BufDim(4) As Short
      '
      Dim IBASF As Short
      CrLf = Chr(13)
      buflen = 4
      iver = 2000
      wart = 10
      delay = 15
      CU(0) = "M"
      CU(1) = "C"
      CU(2) = "B"
      CU(3) = " "
      CU(4) = " "
      CU(5) = " "
      CU(6) = " "
      BufDim(1) = 1
      BufDim(2) = 306
      BufDim(3) = 614
      BufDim(4) = 922
      '     CE 742 GL
      '     Null-Kalibrieren nicht unterst¸tzt
      '
      '
      '
      '      If imess = 3 Or imess = 4 Or imess = 7 Then
      If imess > 2 Then
         Exit Sub
      End If
      '
      '
      '
      '     NB=1,14    360-750(10)
      'Beispiel
      '
      '
      CHRM = MessgChrm(kw)
      '      MsgBox CHRM & CStr(messg_ihrm(kw)), 0
      '
      '      Wake up
      '
      '
      '
      If kw = 1 Then
         i = 0
         BUF = ""
         stv = "v" & CrLf
         Do While i < 10
            Call GetComString(Autom, stv, BUF, 0, iver, delay, wart, 3506, 1, "", 1, "", mscMES, istat)
            Call Wait(20)
            If Len(BUF) > 0 Then Exit Do
            i = i + 1
            Call Wait(100)
         Loop
         stv = "TM" & CrLf
         Call GetComString(Autom, stv, BUF, 4, iver, delay, wart, 3506, 1, "", 1, "????", mscMES, istat)
         If istat <> NuWrt Then Exit Sub
         Call Wait(10)
         ' MsgBox BUF
         If istat <> NuWrt Then Exit Sub
         'stv = "S" & CrLf
         'Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, _
         ''3506, 1, "", 1, "????", mscmes , istat)
         'If istat <> NuWrt Then Exit Sub
         Call Wait(20)

      End If

      '
      '
      If imess = 1 Then
         If kw = 1 Then
            '
            '      Zustand abfragen (bei Weiﬂstandard)
            '
            stv = "R" & CrLf
            Call GetComString(Autom, stv, BUF, buflen, iver, delay, wart, 3506, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            ' MsgBox BUF

            '
         End If
      End If
      '
      '
      '     Messung starten
      '
      '
      '
      stv = CU(imess) & CrLf
      '
      IBASF = 0
      If imess = 0 Then
         '
         'Messung
         '
         If kw = 1 Then
            For i = 1 To 5
               Call Wait(20)
               'Call GetComString(Autom, stv, BUF, 308, iver, delay, 100, _
               ''3500, 1, "", 1, "11", mscmes , istat)
               'MsgBox Mid(BUF, 1, 2)
               Call GetComString(Autom, stv, BUF, 1200, iver, delay, 10, 3500, 1, "", 1, "11", mscMES, istat)
               'MsgBox Len(BUF) & Space(3) & Mid(BUF, 1, 18)
               If Len(BUF) >= 1235 Then
                  If Mid(BUF, 8, 1) = "0" And Mid(BUF, 13, 1) = "0" Then Exit For
               End If
            Next i
            If istat <> NuWrt Then Exit Sub
         End If
         Call CE742rwert(IBASF, Mid(BUF, BufDim(kw)), r, istat)
      ElseIf imess = 1 Then
         '
         'Weiﬂstandard
         '
         If kw = 1 Then
            Call Wait(500)
            Call GetComString(Autom, stv, BUF, 4, 2 * iver, delay, wart, 3500, 1, "", 1, "????", mscMES, istat)
            'MsgBox BUF
            If istat <> NuWrt Then Exit Sub
         End If
         '
         '
         For i = 1 To MessgNwe
            r(i) = 100.0#
         Next i
      ElseIf imess = 2 Then

         '
         'Schwarzstandard
         '
         If kw = 1 Then
            Call Wait(500)
            Call GetComString(Autom, stv, BUF, 4, 2 * iver, delay, wart, 3500, 1, "", 1, "1101", mscMES, istat)
            'MsgBox BUF
            If istat <> NuWrt Then Exit Sub
         End If
         '
         '
         For i = 1 To MessgNwe
            r(i) = 0.0#
         Next i
      End If
   End Sub


   Sub CI5Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
      Dim ive As Short
      Dim CHRM As String
      Dim i As Short
      Dim iver As Short
      Dim buflen As Short
      Dim wart As Short
      Dim delay As Short
      Dim resp As Short
      Dim WLInt As Short
      Dim BuStat As String
      Dim STW As String
      Dim Mess As String
      Dim CU(6) As String
      '
      Dim IBASF As Short
      CrLf = Chr(13)
      buflen = 17
      iver = 5
      wart = 20
      delay = 20
      resp = 2
      CU(0) = "MEASURE"
      CU(1) = "WHITECAL"
      CU(2) = "BLACKCAL"
      CU(3) = " "
      CU(4) = "WHITECAL"
      CU(5) = "BLACKCAL"
      CU(6) = " "
      '
      '
      'Testen
      '

      '
      '     Null-Kalibrieren nicht unterst¸tzt
      '
      '
      '
      Mess = ""
      If imess = 3 Or imess > 5 Then
         Exit Sub
      End If
      '
      '
      '
      '     NB=1    360,750   NWE=79
      '
      '
      '
      '
      '
      'Status
      '
      '
      '
      '
      stv = "STATUS" & CrLf
      Call GetComBuf(resp, stv, BUF, buflen, iver, 10 * delay, wart, 3506, 1, "", 1, "?????????????s", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      BuStat = BUF
      '
      '
      '
      '
      '
      'Konfigurieren Wellenl‰ngenintervall
      '
      '
      '
      WLInt = 5
      stv = "CONFIG -WLEN " & CStr(WLInt) & CrLf
      Call GetComBuf(resp, stv, BUF, buflen, iver, delay, wart, 3506, 1, "", 1, "?????????????s", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      '

      '
      'Konfigurieren Glanz
      '
      '
      '
      CHRM = MessgChrm(kw)
      Select Case imess
         Case 0
            If ReTr = 0 Then
               '
               'Reflexion
               '
               STW = "LED REFLECTION -STATE ON TRANSMISSION -STATE OFF" & CrLf
               If Mid(LTrim(CHRM), 2, 1) = "O" Or Mid(LTrim(CHRM), 2, 1) = "0" Then
                  '
                  'ohne Glanz
                  '
                  stv = "CONFIG -MODE SCE" & CrLf
               Else
                  '
                  'mit Glanz
                  '
                  '
                  stv = "CONFIG -MODE SCI" & CrLf
               End If
            Else
               '
               'Transmission
               '
               STW = "LED REFLECTION -STATE OFF TRANSMISSION -STATE ON" & CrLf
               If Mid(LTrim(CHRM), 2, 1) = "O" Or Mid(LTrim(CHRM), 2, 1) = "0" Then
                  '
                  '
                  'direct
                  '
                  '
                  stv = "CONFIG -MODE DIRECT" & CrLf
                  If kw = 1 Then
                     Mess = "DIRECT"
                  End If
               Else
                  '
                  '
                  'total
                  '
                  '
                  stv = "CONFIG -MODE TOTAL" & CrLf
                  If kw = 1 Then
                     Mess = "TOTAL"
                  End If
               End If
            End If
         Case 1, 2
            'Reflexion
            '
            STW = "LED REFLECTION -STATE ON TRANSMISSION -STATE OFF" & CrLf
            If Mid(LTrim(CHRM), 2, 1) = "O" Or Mid(LTrim(CHRM), 2, 1) = "0" Then
               '
               'ohne Glanz
               '
               stv = "CONFIG -MODE SCE" & CrLf
            Else
               '
               'mit Glanz
               '
               '
               stv = "CONFIG -MODE SCI" & CrLf
            End If

         Case 4, 5
            '
            'Transmission
            '
            STW = "LED REFLECTION -STATE OFF TRANSMISSION -STATE ON" & CrLf
            If Mid(LTrim(CHRM), 2, 1) = "O" Or Mid(LTrim(CHRM), 2, 1) = "0" Then
               '
               '
               'direct
               '
               '
               stv = "CONFIG -MODE DIRECT" & CrLf
               If kw = 1 Then
                  Mess = "DIRECT"
               End If

            Else
               '
               '
               'total
               '
               '
               stv = "CONFIG -MODE TOTAL" & CrLf
               If kw = 1 Then
                  Mess = "TOTAL"
               End If

            End If
      End Select

      Call GetComBuf(resp, STW, BUF, buflen, iver, delay, wart, 3506, 1, "", 1, "?????????????s", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      Call GetComBuf(resp, stv, BUF, buflen, iver, 2 * delay, wart, 3506, 1, "", 1, "?????????????s", mscMES, istat)
      If istat <> NuWrt Then Exit Sub
      If Mess <> "" Then
         Mess = Texxt(205) & Space(1) & Texxt(226) & ":" & Space(1) & Mess
         MsgBox(Mess)
      End If
      Select Case imess
         Case 5
            '
            'Transmission
            'Schwarzstandard messen
            '
            '
            stv = CU(imess) & CrLf
            Call GetComBuf(resp, stv, BUF, buflen, iver, 40 * delay, wart, 3506, 1, "", 1, "?1???????????s", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            '
            '
            '
            '
            For i = 0 To MessgNwe - 1
               r(i) = 0.0#
            Next i
         Case 2
            '
            'Reflexion
            'Schwarzstandard messen
            '
            '
            stv = CU(imess) & CrLf
            Call GetComBuf(resp, stv, BUF, buflen, iver, 40 * delay, wart, 3506, 1, "", 1, "?1???????????s", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            '
            '
            For i = 0 To MessgNwe - 1
               r(i) = 0.0#
            Next i

         Case 4
            '
            '
            'Transmission
            'Weiﬂstandard messen
            '
            '
            stv = CU(imess) & CrLf
            Call GetComBuf(resp, stv, BUF, buflen, iver, 40 * delay, wart, 3506, 1, "", 1, "1????????????s", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            '
            '
            '
            '
            For i = 0 To MessgNwe - 1
               r(i) = 100.0#
            Next i
            '
         Case 1
            '
            '
            'Reflexion
            'Weiﬂstandard messen
            '
            '
            stv = CU(imess) & CrLf
            Call GetComBuf(resp, stv, BUF, buflen, iver, 40 * delay, wart, 3506, 1, "", 1, "1????????????s", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            '
            '
            '
            '
            For i = 0 To MessgNwe - 1
               r(i) = 100.0#
            Next i
            '
         Case 0
            '
            'Reflexion/Transmission messen
            '
            '
            '
            '
            '
            buflen = 654

            stv = CU(imess) & CrLf
            Call GetComBuf(1, stv, BUF, buflen, iver, delay, wart, 3506, 1, "", 1, "11???????????m", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            'MsgBox Len(BUF)
            'MsgBox BUF
            Call CI5rwert(IBASF, BUF, r, istat)


      End Select


   End Sub
   Sub MACMes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
      Dim ive As Short
      Dim CHRM As String
      Dim i As Short
      Dim iver As Short
      Dim buflen As Short
      Dim wart As Short
      Dim delay As Short
      Dim resp As Short
      Dim CU(6) As String
      '
      Dim IBASF As Short
      CrLf = Chr(13)
      buflen = 4
      iver = 5
      wart = 5
      delay = 10
      resp = 2
      CU(0) = "M"
      CU(1) = "C"
      CU(2) = "I"
      CU(3) = " "
      CU(4) = "B"
      CU(5) = "I"
      CU(6) = " "
      '
      '     Null-Kalibrieren nicht unterst¸tzt
      '
      '
      '
      If imess = 3 Or imess > 5 Then
         Exit Sub
      End If
      '
      '
      '
      '     NB=1    400-700,20    NWE=16
      'Beispiel
      '
      '
      'Call GetComBuf(resp, Stv, BUF, buflen, iver, delay, wart, _
      '' 3500, CrLf, -25, "I", mscmes , ier)
      '
      If imess = 2 Or imess = 5 Then
         If kw = 1 Then
            '
            '      Transmission (schwarz)
            '      Zustand abfragen (bei Schwarzstandard (Nullkal.))
            '
            stv = "R" & CrLf
            Call GetComBuf(resp, stv, BUF, 5, iver, delay, wart, 3506, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
         End If
      End If

      '

      CHRM = MessgChrm(kw)
      If Mid(LTrim(CHRM), 2, 1) = "O" Or Mid(LTrim(CHRM), 2, 1) = "0" Then
         stv = "E1" & CrLf
      Else
         stv = "E0" & CrLf
      End If
      Call GetComBuf(resp, stv, BUF, 5, 5, 50, 10, 3500, 1, "", 1, "????", mscMES, istat)

      If istat <> NuWrt Then Exit Sub
      '
      '       MsgBox CHRM & CStr(messg_ihrm(kw)), 0

      '
      Select Case imess
         Case 5
            '
            'Transmission
            '
            '
            '
            '
            '
            stv = "T1" & CrLf
            Call GetComBuf(resp, stv, BUF, 5, 5, 50, 10, 3500, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            '
            '
            'Schwarzstandard messen
            '
            '

            stv = CU(imess) & CrLf
            Call GetComBuf(resp, stv, BUF, 5, 5, 50, 10, 3500, 1, "", 1, "????", mscMES, istat)
            '
            '
            For i = 1 To MessgNwe
               r(i) = 0.0#
            Next i
         Case 2
            '
            'Reflexion
            '
            '
            '
            stv = "T0" & CrLf
            Call GetComBuf(resp, stv, BUF, 5, 5, 50, 10, 3500, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            '
            '
            'Schwarzstandard messen
            '
            '

            stv = CU(imess) & CrLf
            Call GetComBuf(resp, stv, BUF, 5, 5, 50, 10, 3500, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            For i = 1 To MessgNwe
               r(i) = 0.0#
            Next i

         Case 4
            '
            '
            'Transmission
            '
            stv = "T1" & CrLf
            Call GetComBuf(resp, stv, BUF, 5, 5, 50, 10, 3500, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            '
            '
            '
            '
            '
            stv = CU(imess) & CrLf
            Call GetComBuf(resp, stv, BUF, 5, 5, 50, 10, 3500, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            'MsgBox Len(BUF)
            'MsgBox BUF
            '
            '
            '
            For i = 1 To MessgNwe
               r(i) = 100.0#
            Next i
            '
         Case 1
            '
            '
            'Reflexion
            '
            stv = "T0" & CrLf
            Call GetComBuf(resp, stv, BUF, 5, 5, 50, 10, 3500, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            '
            '
            '
            '
            '
            stv = CU(imess) & CrLf
            Call GetComBuf(resp, stv, BUF, 5, 5, 50, 10, 3500, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            'MsgBox Len(BUF)
            'MsgBox BUF
            '
            '
            '
            For i = 1 To MessgNwe
               r(i) = 100.0#
            Next i
            '
         Case 0
            '
            'Reflexion
            '
            '
            stv = "T0" & CrLf
            If ReTr = 1 Then
               stv = "T1" & CrLf
            End If
            Call GetComBuf(resp, stv, BUF, 5, 5, 50, 10, 3500, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            '
            '
            '
            stv = CU(imess) & CrLf
            Call GetComBuf(resp, stv, BUF, 400, 5, 50, 10, 3500, 1, "", 1, "????", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            'MsgBox Len(BUF)
            'MsgBox BUF
            Call MACrwert(IBASF, BUF, r, istat)

            '
      End Select
      '
      '
   End Sub


   Sub M20Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
      Dim ive As Short
      Dim CHRM As String
      Dim i As Short
      Dim iver As Short
      Dim buflen As Short
      Dim wart As Short
      Dim delay As Short
      Dim resp As Short
      Dim CU(6) As String
      '
      Dim IBASF As Short
      CrLf = Chr(13)
      buflen = 4
      iver = 5
      wart = 5
      delay = 10
      resp = 2
      CU(0) = "m"
      CU(1) = "m"
      CU(2) = "m"
      CU(3) = " "
      CU(4) = " "
      CU(5) = " "
      CU(6) = " "
      '
      '     Null-Kalibrieren nicht unterst¸tzt
      '
      '
      '
      '      If imess = 3 Or imess = 4 Or imess = 7 Then
      If imess > 2 Then
         Exit Sub
      End If
      '
      '
      '
      '     NB=1    400-700,20    NWE=16
      'Beispiel
      '
      '
      'Call GetComBuf(resp, Stv, BUF, buflen, iver, delay, wart, _
      '' 3500, CrLf, -25, "I", mscmes , ier)
      '
      CHRM = MessgChrm(kw)
      '      MsgBox CHRM & CStr(messg_ihrm(kw)), 0

      '
      If imess = 1 Then
         If kw = 1 Then
            '
            '      Zustand abfragen (bei Weiﬂstandard)
            '
            stv = "r" & CrLf
            Call GetComBuf(resp, stv, BUF, 1, iver, delay, wart, 3506, 1, "", 1, "????", mscMES, istat)
            'MsgBox BUF
            If istat <> NuWrt Then Exit Sub
            stv = "c" & CrLf
            Call GetComBuf(resp, stv, BUF, 0, 5, 50, 50, 3500, 1, "", 1, "", mscMES, istat)
            If istat <> NuWrt Then Exit Sub

         End If
      End If
      '
      '
      '
      '     Messung starten
      '
      '
      '
      stv = CU(imess) & CrLf
      '
      IBASF = 0
      '
      'Messung/Weiﬂkalibrierung/Schwarzkalibrierung
      '
      'MsgBox stv
      Call GetComBuf(resp, stv, BUF, 100, 5, 100, 50, 3500, 1, "", 1, "?", mscMES, istat)
      'MsgBox BUF
      If istat <> NuWrt Then Exit Sub

      Call M20rwert(IBASF, BUF, r, istat)
      '
   End Sub
   Sub MS20Mes(ByRef imess As Short, ByRef kw As Short, ByRef r() As Single, ByRef istat As Short, ByRef mscMES As SerialPort)
      Dim ive As Short
      Dim CHRM As String
      Dim i As Short
      Dim iver As Short
      Dim buflen As Short
      Dim wart As Short
      Dim delay As Short
      Dim CU(6) As String
      '
      Dim IBASF As Short
      CrLf = Chr(13) & Chr(10) & Chr(4)
      buflen = 4
      iver = 1000
      wart = 400
      delay = 10
      CU(0) = "M"
      CU(1) = "M"
      CU(2) = "M"
      CU(3) = " "
      CU(4) = " "
      CU(5) = " "
      CU(6) = " "
      '
      '     Null-Kalibrieren nicht unterst¸tzt
      '
      '
      '
      '      If imess = 3 Or imess = 4 Or imess = 7 Then
      If imess > 2 Then
         Exit Sub
      End If
      '
      '
      '
      '     NB=1    400-700,20    NWE=16
      'Beispiel
      '
      '
      '
      CHRM = MessgChrm(kw)
      '      MsgBox CHRM & CStr(messg_ihrm(kw)), 0

      '
      If imess = 1 Then
         If kw = 1 Then
            stv = "A" & CrLf
            '
            Call GetComString(Autom, stv, BUF, 0, iver, delay, 100, 3500, 1, "", 1, "", mscMES, istat)
            'MsgBox BUF
            If istat <> NuWrt Then Exit Sub


            '
            '      Zustand abfragen (bei Weiﬂstandard)
            '
            stv = "R" & CrLf
            Call GetComString(Autom, stv, BUF, 0, iver, delay, 100, 3506, 1, "", 1, "A???", mscMES, istat)
            ' bei X-Rite kommt A und 000000
            'MsgBox BUF
            If istat <> NuWrt Then Exit Sub
            '
            '
            '
            '
            '
            stv = "A" & CrLf
            '
            Call GetComString(Autom, stv, BUF, 0, iver, delay, 100, 3500, 1, "", 1, "", mscMES, istat)
            'MsgBox BUF
            If istat <> NuWrt Then Exit Sub


            stv = "S" & CrLf
            Call GetComString(Autom, stv, BUF, 0, iver, delay, 100, 3500, 1, "", 1, "", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            ' bei X-Rite kommt A und DREIS
            'MsgBox BUF

            '
            '
            '
            '
            '
            stv = "A" & CrLf
            '
            Call GetComString(Autom, stv, BUF, 0, iver, delay, 100, 3500, 1, "", 1, "", mscMES, istat)
            'MsgBox BUF
            If istat <> NuWrt Then Exit Sub

            '
            '
            '      Kalibrieren
            '
            '
            '
            stv = "O" & CrLf
            Call GetComString(Autom, stv, BUF, 0, iver, delay, 100, 3506, 1, "", 1, "????", mscMES, istat)
            ' bei X-Rite kommt A und XREIS
            'MsgBox BUF
            '
            '
            '
            '
            '
            stv = "A" & CrLf
            '
            Call GetComString(Autom, stv, BUF, 0, iver, delay, 100, 3500, 1, "", 1, "", mscMES, istat)
            'MsgBox BUF
            If istat <> NuWrt Then Exit Sub
            stv = "C" & CrLf
            Call GetComString(Autom, stv, BUF, 8, iver, delay, wart, 3500, 1, "A", 1, "", mscMES, istat)
            If istat <> NuWrt Then Exit Sub
            ' bei X-Rite kommt A

            'MsgBox BUF

         End If
      End If
      '
      '
      stv = "A" & CrLf
      '
      Call GetComString(Autom, stv, BUF, 0, iver, delay, 100, 3500, 1, "", 1, "", mscMES, istat)
      'MsgBox BUF
      If istat <> NuWrt Then Exit Sub


      '
      '     Messung starten
      '
      '
      '
      Call Wait(50)
      stv = CU(imess) & CrLf
      '
      IBASF = 0
      '
      'Messung/Weiﬂkalibrierung/Schwarzkalibrierung
      '
      'MsgBox stv
      Call GetComString(Autom, stv, BUF, 120, iver, delay, wart, 3500, 1, "", 1, "A", mscMES, istat)
      'MsgBox BUF
      'MsgBox Len(BUF)
      If istat <> NuWrt Then Exit Sub

      Call MS20rwert(IBASF, BUF, r, istat)
      stv = "A" & CrLf
      '
      Call GetComString(Autom, stv, BUF, 0, iver, delay, 100, 3500, 1, "", 1, "", mscMES, istat)
      'MsgBox BUF
      If istat <> NuWrt Then Exit Sub


      '
   End Sub














   Sub MesIni()
      istat = NuWrt
      If MessgDriver = "MC0" Then
         Call CHKini(istat)
      ElseIf MessgDriver = "MC1" Then
         Call MACini(istat)
      ElseIf MessgDriver = "MC2" Then
         Call M20ini(istat)
      ElseIf MessgDriver = "MC3" Then
         Call MS20ini(istat)
      ElseIf MessgDriver = "MC4" Then
         Call CE742ini(istat)
      ElseIf MessgDriver = "MC5" Then
         Call CE2180ini(istat)
      ElseIf MessgDriver = "MC6" Then
         Call CI5ini(istat)
      Else
         MsgBox(Texxt(3542) & MessgDriver)
      End If
   End Sub

   Sub MesKal(ByRef imess As Short)
      Dim jj As Short
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
         'ger‰teabh‰ngige Driver
         '
         '
         '
         For kw = 0 To MessgKM - 1
            If MessgDriver = "MC0" Then
               Call CHKMes(imess, kw, rmes, istat, MscMes)
            ElseIf MessgDriver = "MC1" Then
               Call MACMes(imess, kw, rmes, istat, MscMes)
            ElseIf MessgDriver = "MC2" Then
               Call M20Mes(imess, kw, rmes, istat, MscMes)
            ElseIf MessgDriver = "MC3" Then
               Call MS20Mes(imess, kw, rmes, istat, MscMes)
            ElseIf MessgDriver = "MC4" Then
               Call CE742Mes(imess, kw, rmes, istat, MscMes)
            ElseIf MessgDriver = "MC5" Then
               Call CE2180Mes(imess, kw, rmes, istat, MscMes)
            ElseIf MessgDriver = "MC6" Then
               Call CI5Mes(imess, kw, rmes, istat, MscMes)
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





    Public Sub New()
        MyBase.New()
    End Sub
    Sub dispose() Implements IDisposable.Dispose
    End Sub
End Class