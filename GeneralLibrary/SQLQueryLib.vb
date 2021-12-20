Option Compare Text
Option Strict Off
Option Explicit On

Public Class SQLQueryLib
  Shared Function SqlStr(ByVal dddd As String) As String
    Dim i As Integer
    SqlStr = dddd
    For i = 1 To Len(SqlStr)
      If Mid(SqlStr, i, 1) = "." Then
        Mid(SqlStr, i, 1) = "/"
      End If
    Next i
  End Function

  Shared Function StrSelct(ByVal Field As String, ByVal text As String) As String
    Dim INN As Integer
    Dim IKOM As Integer
    Dim SubText As String
    Dim Iber As Integer
    Iber = 1
    StrSelct = ""
    Do While Iber <> 0
      IKOM = InStr(Trim(Mid(text, Iber)), ",")
      If IKOM > 1 Then
        SubText = StrFil(Trim(Mid(text, Iber, IKOM - 1)), "'")
        Iber = Iber + IKOM
      Else
        SubText = Trim(Mid(text, Iber))
        Iber = 0
      End If
      INN = InStr(SubText, "_")
      If INN <> 0 Then
        StrSelct = StrSelct & Field & " BETWEEN  '" & Mid(Trim(SubText), 1, INN - 1) & "' AND '" & Mid(Trim(SubText), INN + 1) & "'"
      ElseIf SubText <> "" Then
        StrSelct = StrSelct & Field & " LIKE '" & SubText & "'"
      End If
      If Iber > 0 Then
        StrSelct = StrSelct & " OR "
      End If
    Loop
  End Function

  Shared Function StrFil(ByVal st As String, ByVal Zei As String) As String
    Dim i As Integer
    i = InStr(st, Zei)
    StrFil = st
    If i = 0 Then
      Exit Function
    Else
      Mid(StrFil, i, 1) = Space(0)
    End If
  End Function
  Shared Function sqlti(ByVal titi As Date) As String
    '
    'Zeit für SQL-Abfrage
    '
    sqlti = "#" & Format(titi.Hour, "00") & ":" & Format(titi.Minute, "00") & ":" & Format(titi.Second, "00") & "#"
    '
  End Function

  Shared Function sqlda(ByVal dada As Date) As String
    '
    'Datum für SQL-Abfrage
    '
    sqlda = "#" & Format(dada.Month, "00") & "/" & Format(dada.Day, "00") & "/" & Format(dada.Year, "0000") & "#"
  End Function

  Shared Function Sqldati(ByVal dada As Date) As String
    '
    'Datum * Zeit für SQL-Abfrage
    '
    sqldati = "#" & Format(dada.Month, "00") & "/" & Format(dada.Day, "00") & "/" & Format(dada.Year, "0000") & Space(1) & Format(dada.Hour, "00") & ":" & Format(dada.Minute, "00") & ":" & Format(dada.Second, "00") & "#"
  End Function

  Shared Function AddHkomE(ByVal text As String) As String
    Dim i As Integer
    Dim j As Integer
    AddHkomE = ""
    '
    'ersetzt Hochkomma durch 2 Hochkommata
    '
    If IsNothing(text) Then Exit Function
    If text = "" Then Exit Function
    j = 1
    AddHkomE = ""
    '
    Do
      i = InStr(Mid(text, j), "'")
      If i = 0 Then
        AddHkomE = AddHkomE & Mid(text, j)
        Exit Function
      End If
      AddHkomE = AddHkomE & Mid(text, j, i) & "'"
      j = j + i
    Loop
  End Function
  Shared Function AddHkom(ByVal text As String, ByVal Length As Integer) As String
    AddHkom = AddHkomE(text)
    If Len(AddHkom) > Length Then
      AddHkom = AddHkom.Substring(0, Length)
    End If
  End Function
  Shared Function SQLpunkt(ByVal text As String) As String
    Dim i As Integer
    SQLpunkt = text
    i = InStr(SQLpunkt, ",")
    If i <> 0 Then
      Mid(SQLpunkt, i, 1) = "."
    End If
  End Function
  

End Class
