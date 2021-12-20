Public Class ColorLicenseKey
  Private Declare Function ZHXYZ Lib "FARBEN.DLL" () As Integer
  Private Declare Function CRYPTCMP Lib "COLORCRP.DLL" (ByRef OLDKEY As Byte, ByRef COMPKEY As Byte) As Integer
  Private Declare Sub STARTKEY Lib "COLORCRP.DLL" (ByRef PROCKEY As Byte, ByRef MOTHKEY As Byte, ByRef NEWKEY As Byte)
  Private Declare Function UNLOCK Lib "COLORCRP.DLL" (ByRef KEY As Integer) As Integer
  Dim PathCrypt As String = "SOFTWARE\COLORSYS"
  Dim ValCrypt As String = "ROLOC"
  Dim ValLock As String = "ROFREE"

  Public Function CryptFind(ByRef Level As Integer) As Boolean
    Dim PROCKEY As String
    Dim COLKEY As String
    Try
      CryptFind = False
      If Level = 1 Then
        COLKEY = RegRead(PathCrypt, ValCrypt)
        '
        If COLKEY <> "" Then
          PROCKEY = GetKeyMixed()
          If CMPCRYPT(PROCKEY, COLKEY) Then
            CryptFind = True
          End If
        End If
      End If
      'If Not CryptFind Then

      'If ZHXYZ() = 198769432 Then
      'CryptFind = True
      'End If

      'End If
    Catch
      CryptFind = False
    End Try
    If Not CryptFind Then
      COLKEY = RegRead(PathCrypt, ValLock)
      If COLKEY = "" OrElse UNLOCK(CInt(COLKEY)) = 0 Then
        COLKEY = InputBox("input", "unlock code" & Space(2) & "input", "")
        If COLKEY = "" OrElse UNLOCK(CInt(COLKEY)) = 0 Then
          MsgBox("wrong unlock code", , "Message")
          Exit Function
        End If
        If UNLOCK(CInt(COLKEY)) = 1 Then
          If RegWrite(PathCrypt, ValLock, COLKEY) Then
            CryptFind = True
          Else
            MsgBox("No administrator rights", , "Message")
          End If
        End If
      Else
        CryptFind = True
      End If
    End If
  End Function


  Public Function GetKeyMixed() As String
    Dim Cpu As String
    Dim Moth As String
    Dim LMoth As Integer
    Dim MothIn(15) As Byte
    Dim CPUIn(15) As Byte
    Dim NewKeyIn(15) As Byte
    Dim i As Integer
    Cpu = GetCpu()
    If Cpu = "" Then
      GetKeyMixed = Cpu
      Exit Function
    End If
    Moth = MBSerialNumber()
    If Len(Moth) < 16 Then
      LMoth = 16 - Len(Moth)
      For i = 0 To LMoth - 1
        Moth = Moth & Chr(63 + i)
      Next i
    ElseIf Len(Moth) > 16 Then
      Moth = Moth.Substring(Len(Moth) - 17, 16)
    End If
    For i = 0 To 15
      MothIn(i) = Asc(Moth.Substring(i, 1))
      CPUIn(i) = Asc(Cpu.Substring(i, 1))
    Next i

    Call STARTKEY(CPUIn(0), MothIn(0), NewKeyIn(0))
    GetKeyMixed = ""
    For i = 0 To 15
      GetKeyMixed = GetKeyMixed & Chr(NewKeyIn(i))
    Next i
  End Function

  Private Function CMPCRYPT(ByRef PROCKEY As String, ByRef COLKEY As String) As Boolean
    Dim ColKY(15) As Byte
    Dim ProcKy(15) As Byte
    Dim i As Integer
    Dim j As Integer
    CMPCRYPT = True
    If COLKEY.Length < 16 Or PROCKEY.Length < 16 Then
      CMPCRYPT = False
      Exit Function
    End If
    For i = 0 To 15
      ColKY(i) = Asc(COLKEY.Substring(i, 1))
      ProcKy(i) = Asc(PROCKEY.Substring(i, 1))
    Next i
    j = CRYPTCMP(ProcKy(0), ColKY(0))
    If j = 0 Then
      CMPCRYPT = False
    Else
      CMPCRYPT = True
    End If
  End Function


  '
  'Write a key
  '
  '
  Public Function RegWrite(RegPath As String, Valuename As String, Value As String) As Boolean
    Dim regKey As RegistryKey
    RegWrite = False
    Try
      regKey = Registry.LocalMachine
      regKey.CreateSubKey(RegPath)
      regKey.Close()
      regKey = Registry.LocalMachine.OpenSubKey(RegPath, True)
      regKey.SetValue(Valuename, Value)
      regKey.Close()
      RegWrite = True
    Catch ex As Exception

    End Try
  End Function
  '
  'Read a key
  '
  Public Function RegRead(RegPath As String, Valuename As String) As String
    Dim regKey As RegistryKey
    regKey = Registry.LocalMachine.OpenSubKey(RegPath, False)
    If Not IsNothing(regKey) Then
      RegRead = regKey.GetValue(Valuename)
      regKey.Close()
    Else
      RegRead = ""
    End If
  End Function
  '
  'Delete a key
  Public Sub RegDelete(RegPath As String)
    Dim regKey As RegistryKey
    regKey = Registry.LocalMachine
    Try
      regKey.DeleteSubKey(RegPath, True)
    Catch ex As Exception
    End Try
    regKey.Close()
  End Sub


  Private Function GetCpu() As String
    Dim objMOS As ManagementObjectSearcher
    Dim objMOC As Management.ManagementObjectCollection
    Dim objMO As Management.ManagementObject
    GetCpu = ""
    'Now, execute the query to get the results
    objMOS = New ManagementObjectSearcher("Select * From Win32_Processor")
    objMOC = objMOS.Get
    'Finally, get the CPU's id.
    For Each objMO In objMOC
      GetCpu = objMO("ProcessorID")
      Exit For
    Next
    'Dispose object variables.
    objMOS.Dispose()
    objMOS = Nothing
    objMO.Dispose()
    objMO = Nothing
  End Function

  Private Function MBSerialNumber() As String
    Dim procid As String
    'RETRIEVES SERIAL NUMBER OF MOTHERBOARD
    'IF THERE IS MORE THAN ONE MOTHERBOARD, THE SERIAL
    'NUMBERS WILL BE DELIMITED BY COMMAS

    Dim Q As New SelectQuery("Win32_BaseBoard")
    Dim SearchString As New ManagementObjectSearcher(Q)
    Dim Info As ManagementObject
    MBSerialNumber = ""
    For Each Info In SearchString.Get()
      procid = Info("SerialNumber").ToString
      procid = procid.Trim
      MBSerialNumber = procid
      Exit For
    Next
    SearchString.Dispose()
    Info.Dispose()
    SearchString = Nothing
    Info = Nothing
  End Function


End Class
