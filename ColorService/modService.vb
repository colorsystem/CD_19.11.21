Module modService

  

  
  Function AddModDelP(ByRef nform As Short) As Boolean
    Dim imsg As Short
    imsg = MsgBox(Texxt(nform), 4, Texxt(2000))
    If imsg = 7 Then
      AddModDelP = False
    Else
      AddModDelP = True
    End If
  End Function

  
End Module
