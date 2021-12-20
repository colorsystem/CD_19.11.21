Module modSetKey
  Sub main()
    Dim SetKey As String
    Dim Pathcrypt As String = "SOFTWARE\COLORSYS"
    Dim ValName As String = "ROLOC"
    Dim license As New ColorLicenseKey
    SetKey = InputBox("Please input the COLORSYS-key", "Set Key", "")
    If SetKey = "" Then Exit Sub
    '
    '
    'Abspeichern
    '
    '
    If license.RegRead(Pathcrypt, ValName) <> "" Then
      Call license.RegDelete(Pathcrypt)
      'MsgBox "Key is deleted"

    End If
    If license.RegWrite(Pathcrypt, ValName, SetKey) Then
      MsgBox("Key is saved")
    Else
      MsgBox("Key is not saved; probably no admin rights")
    End If
  End Sub
End Module
