Option Compare Text
Option Strict Off
Option Explicit On
Public Class BitsAndBytes
  Shared Function BitWrt(ByVal Ib As Short, ByVal ID As Integer) As Boolean
    Dim i As Short
    i = BitInt(Ib, Ib, ID)
    BitWrt = False
    If i = 1 Then
      BitWrt = True
    End If
  End Function
  Overloads Shared Function BitInt(ByVal Ib1 As Short, ByVal Ib2 As Short, ByVal ID As Integer) As Short
    If Ib2 > 30 Then
      BitInt = ID / 2 ^ Ib1
    Else
      BitInt = Int((ID Mod 2 ^ (Ib2 + 1)) / 2 ^ Ib1)
    End If
  End Function
  Shared Function BitWrtID(ByVal BitInt As Boolean, ByVal Ib As Short, ByVal ID As Integer) As Integer
    Dim i As Short
    i = 0
    If BitInt Then
      i = 1
    End If
    BitWrtID = BitIntID(i, Ib, Ib, ID)
  End Function
  Shared Function DoubleToString(ByVal Nanz As Integer, ByVal Value() As Double) As String
    Dim WriteString As String
    WriteString = ""
    For i = 0 To Nanz - 1
      WriteString = WriteString & i & Space(2) & Str(Value(i)) & Chr(13) & Chr(10)
    Next i
    DoubleToString = WriteString
  End Function
  Overloads Shared Function BitIntID(ByVal i As Short, ByVal Ib1 As Short, ByVal Ib2 As Short, ByRef ID As Integer) As Integer
    Dim Ihil1 As Long
    Dim Ihil2 As Long
    '
    'Der Wert i wird den bits ib1 bis ib2 zugeordnet (ergibt BitIntID)
    '
    '
    If Ib1 = 0 Then
      Ihil1 = 0
    Else
      Ihil1 = Int(ID Mod 2 ^ Ib1)
    End If
    If Ib2 > 30 Then
      Ihil2 = 0
    Else
      Ihil2 = Int((ID Mod 2 ^ 31) / 2 ^ (Ib2 + 1))
    End If
    BitIntID = Ihil1 + CLng(i) * 2 ^ Ib1 + Ihil2 * 2 ^ (Ib2 + 1)
  End Function
  '
 

  Overloads Shared Function BitInt(ByVal Ib1 As Short, ByVal Ib2 As Short, ByVal ID As Long) As Integer
    Dim IDIF As Long
    'Summe Bit(IB1) bis Bit(IB2)
    IDIF = 2 ^ (Ib2 + 1) - 2 ^ Ib1
    BitInt = (ID And IDIF) / 2 ^ Ib1
  End Function
  Overloads Shared Function BitIntID(ByVal i As Integer, ByVal Ib1 As Short, ByVal Ib2 As Short, ByRef ID As Long) As Long
    'Bits von i werden mit ID nach BitIntID(Ib1-Ib2) übertragen
    BitIntID = (ID Or (i * 2 ^ Ib1))
  End Function
  '
  '
  '
  'ArrayByteConverter
  '
  '
  '
  '
  '
  Overloads Shared Function GetBytes(ByVal A As Double) As Byte()
    GetBytes = BitConverter.GetBytes(A)
  End Function
  Overloads Shared Function GetBytes(ByVal A As Single) As Byte()
    GetBytes = BitConverter.GetBytes(A)
  End Function
  Overloads Shared Function GetBytes(ByVal IA As Integer) As Byte()
    GetBytes = BitConverter.GetBytes(IA)
  End Function
  Overloads Shared Function GetBytes(ByVal IA As Short) As Byte()
    GetBytes = BitConverter.GetBytes(IA)
  End Function
  Overloads Shared Function GetBytes(ByVal IA As Long) As Byte()
    GetBytes = BitConverter.GetBytes(IA)
  End Function
  Overloads Shared Function GetBytes(ByVal A() As Double) As Byte()
    Dim Iub As Integer
    Dim i As Integer
    Dim j As Integer
    Dim Byt(7) As Byte
    Dim Gbyt() As Byte
    Iub = UBound(A)
    ReDim Gbyt(8 * (Iub + 1) - 1)
    For i = 0 To Iub
      Byt = BitConverter.GetBytes(A(i))
      For j = 0 To 7
        Gbyt(8 * i + j) = Byt(j)
      Next
    Next
    GetBytes = Gbyt
  End Function
  Overloads Shared Function GetBytes(ByVal A() As Single) As Byte()
    Dim Iub As Integer
    Dim i As Integer
    Dim j As Integer
    Dim Byt(3) As Byte
    Dim Gbyt() As Byte
    Iub = UBound(A)
    ReDim Gbyt(4 * (Iub + 1) - 1)
    For i = 0 To Iub
      Byt = BitConverter.GetBytes(A(i))
      For j = 0 To 3
        Gbyt(4 * i + j) = Byt(j)
      Next
    Next
    GetBytes = Gbyt
  End Function
  Overloads Shared Function GetBytes(ByVal IA() As Integer) As Byte()
    Dim Iub As Integer
    Dim i As Integer
    Dim j As Integer
    Dim Byt(3) As Byte
    Dim Gbyt() As Byte
    Iub = UBound(IA)
    ReDim Gbyt(4 * (Iub + 1) - 1)
    For i = 0 To Iub
      Byt = BitConverter.GetBytes(IA(i))
      For j = 0 To 3
        Gbyt(4 * i + j) = Byt(j)
      Next
    Next
    GetBytes = Gbyt
  End Function
  Overloads Shared Function GetBytes(ByVal IA() As Short) As Byte()
    Dim Iub As Integer
    Dim i As Integer
    Dim j As Integer
    Dim Byt(1) As Byte
    Dim Gbyt() As Byte
    Iub = UBound(IA)
    ReDim Gbyt(2 * (Iub + 1) - 1)
    For i = 0 To Iub
      Byt = BitConverter.GetBytes(IA(i))
      For j = 0 To 1
        Gbyt(2 * i + j) = Byt(j)
      Next
    Next
    GetBytes = Gbyt
  End Function
  Overloads Shared Function GetBytes(ByVal IA() As Long) As Byte()
    Dim Iub As Integer
    Dim i As Integer
    Dim j As Integer
    Dim Byt(3) As Byte
    Dim Gbyt() As Byte
    Iub = UBound(IA)
    ReDim Gbyt(8 * (Iub + 1) - 1)
    For i = 0 To Iub
      Byt = BitConverter.GetBytes(IA(i))
      For j = 0 To 7
        Gbyt(8 * i + j) = Byt(j)
      Next
    Next
    GetBytes = Gbyt
  End Function
  Overloads Shared Function GetBytes(ByVal IA() As Byte) As Byte()
    GetBytes = IA
  End Function
  Overloads Shared Function GetBytes(ByVal Stri As String) As Byte()
    Dim Iub As Integer
    Dim j As Integer
    Dim Gbyt() As Byte
    Iub = Stri.Length
    ReDim Gbyt(Iub - 1)
    For j = 0 To Iub - 1
      Gbyt(j) = Asc(Stri.Substring(j, 1))
    Next
    GetBytes = Gbyt
  End Function

  Shared Function GetDoubles(ByVal byt() As Byte) As Double()
    Dim Iub As Integer
    Dim i As Integer
    Dim a() As Double
    Iub = UBound(byt)
    ReDim a(Int(Iub / 8))
    For i = 0 To Int(Iub / 8)
      a(i) = BitConverter.ToDouble(byt, 8 * i)
    Next i
    GetDoubles = a
  End Function
  Shared Function GetDouble(ByVal byt() As Byte) As Double
    Dim a As Double
    Dim by() As Byte = {32, 32, 32, 32, 32, 32, 32, 32}
    For i = 0 To byt.Length - 1
      by(i) = byt(i)
    Next
    a = BitConverter.ToDouble(by, 0)
    GetDouble = a
  End Function

  Shared Function GetSingles(ByVal byt() As Byte) As Single()
    Dim Iub As Integer
    Dim i As Integer
    Dim a() As Single
    Iub = UBound(byt)
    ReDim a(Int(Iub / 4))
    For i = 0 To Int(Iub / 4)
      a(i) = BitConverter.ToSingle(byt, 4 * i)
      If Single.IsNaN(a(i)) Then
        a(i) = 0.0
      End If
    Next i
    GetSingles = a
  End Function
  Shared Function GetSingle(ByVal byt() As Byte) As Single
    Dim a As Single
    a = BitConverter.ToSingle(byt, 0)
    GetSingle = a
  End Function
  Shared Function GetIntegers(ByVal byt() As Byte) As Integer()
    Dim Iub As Integer
    Dim i As Integer
    Dim IA() As Integer
    Iub = UBound(byt)
    ReDim IA(Int(Iub / 4))
    For i = 0 To Int(Iub / 4)
      IA(i) = BitConverter.ToInt32(byt, 4 * i)
    Next i
    GetIntegers = IA
  End Function
  Shared Function GetInteger(ByVal byt() As Byte) As Integer
    Dim IA As Integer
    IA = BitConverter.ToInt32(byt, 0)
    GetInteger = IA
  End Function
  Shared Function GetLongs(ByVal byt() As Byte) As Long()
    Dim Iub As Integer
    Dim i As Integer
    Dim IA() As Long
    Iub = UBound(byt)
    ReDim IA(Int(Iub / 8))
    For i = 0 To Int(Iub / 8)
      IA(i) = BitConverter.ToSingle(byt, 8 * i)
    Next i
    GetLongs = IA
  End Function
  Shared Function GetLong(ByVal byt() As Byte) As Long
    Dim IA As Long
    IA = BitConverter.ToSingle(byt, 0)
    GetLong = IA
  End Function
  Overloads Shared Function GetString(ByVal byt() As Byte) As String
    Dim Iub As Integer
    Dim i As Integer
    Dim Stri As String
    Iub = UBound(byt)
    Stri = ""
    For i = 0 To Iub
      Stri = Stri & Chr(byt(i))
    Next i
    GetString = Stri
  End Function
  Overloads Shared Function GetString(ByVal byt() As Byte, ByVal Startindex As Integer, ByVal length As Integer) As String
    Dim Iub As Integer
    Dim i As Integer
    Dim Stri As String
    Iub = UBound(byt)
    Stri = ""
    For i = Startindex To Startindex + length - 1
      Stri = Stri & Chr(byt(i))
    Next i
    GetString = Stri
  End Function
  Overloads Shared Function Singl(ByVal text As DBNull) As Single
    Singl = 0.0
  End Function

  Overloads Shared Function Singl(ByVal text As String) As Single
    Dim i As Short
    Dim j As Short
    Dim tex As String
    Dim tev As String
    Dim ten As String
    Dim idez As Boolean
    Dim vor As Single
    If text Is Nothing Then
      Singl = 0.0
      Exit Function
    End If
    tex = ""
    For i = 0 To text.Length - 1
      If text.Substring(i, 1) <> " " Then
        tex = tex & text.Substring(i, 1)
      End If
    Next i
    If tex = "" Then
      Singl = 0.0#
      Exit Function
    End If
    If Not IsNumeric(tex) Then
      Singl = 0.0#
      Exit Function
    End If

    Try
      If tex.IndexOf(".") > -1 Then
        Singl = Val(tex)
      Else
        Singl = CSng(tex)
      End If
    Catch
      tev = tex
      i = InStr(tev, ",")
      If i <> 0 Then
        Mid(tev, i, 1) = "."
      End If
      If InStr(tev, ".e") > 0 Then
        Try
          Singl = Val(tev)
          Exit Function
        Catch
        End Try
      End If
      ten = ""
      tev = ""
      idez = False
      vor = 1.0#
      For i = 0 To Len(tex) - 1
        j = Asc(tex.Substring(i, 1))
        If j < 48 Or j > 57 Then
          '        Prüfen auf Komma
          If j = 44 Then
            idez = True
            '        Prüfen auf Punkt
          ElseIf j = 46 Then
            idez = True
            '        Prüfen auf Punkt
          ElseIf j = 45 Then
            If i = 1 Then
              vor = -1.0#
            End If
          End If
        Else
          If idez Then
            ten = ten & Chr(j)
          Else
            tev = tev & Chr(j)
          End If
        End If

      Next i
      If ten = "" Then
        ten = "0"
      End If
      If tev = "" Then
        tev = "0"
      End If
      Singl = vor * (CSng(tev) + CSng(ten) * 10.0# ^ (-Len(ten)))
    End Try
  End Function
End Class
