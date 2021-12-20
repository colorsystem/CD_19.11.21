Option Compare Text
Option Strict Off
Option Explicit On
Public Class Auxiliary
  Private Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Integer)
  Private Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" _
  (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Integer, ByVal lpFileName As String) As Integer
  Private Declare Function WritePrivateProfileString Lib "kernel32" Alias "WritePrivateProfileStringA" _
  (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal lpString As String, ByVal lpFileName As String) As Integer

  Shared Property PrivSettings(ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal lpDefault As String, ByVal lpFileName As String) As String
    Get
      Dim Xlon As Integer
      Dim Getstr As String
      Getstr = Space(1024)
      Xlon = GetPrivateProfileString(lpApplicationName, lpKeyName, lpDefault, Getstr, 1024, lpFileName)

      '
      Getstr = Getstr.Trim

      PrivSettings = Trim(Getstr.Substring(0, Getstr.Length - 1) & Space(0))
      If PrivSettings = "" Then
        PrivSettings = lpDefault
      End If
    End Get
    Set(ByVal AcPrivSettings As String)
      Dim Xlon As Integer
      Xlon = WritePrivateProfileString(lpApplicationName, lpKeyName, AcPrivSettings, lpFileName)
    End Set
  End Property

  Shared Sub Wart(ByRef HundSec As Short)
    Dim TausSec As Integer
    TausSec = 10 * HundSec
    Call Sleep(TausSec)
  End Sub
  Shared Function CharTim() As String
    CharTim = Format(Now.Second, "0000000")
  End Function
  Shared Function ComplColor(ByVal OriginColor As Color) As Color
    ComplColor = Color.FromArgb(OriginColor.A, 255 - OriginColor.R, 255 - OriginColor.G, 255 - OriginColor.B)
  End Function
  Shared Function KeyName(ByRef i As Integer) As String
    KeyName = Format(i, "0000000000")
  End Function
  Shared Function KeyRe(ByRef i As Integer) As String
    KeyRe = Format(i, "000")
  End Function
  Shared WriteOnly Property LetPrivSettings(ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal lpFileName As String) As String
    Set(ByVal Value As String)
      Dim Xlon As Integer
      '
      'ist Value=vbnullstring (Nothing), so wird der Eintrag lpkeyname gelöscht
      '
      '
      Xlon = WritePrivateProfileString(lpApplicationName, lpKeyName, Value, lpFileName)
    End Set
  End Property
  Shared Function GetPrivSettings(ByRef lpApplicationName As String, ByRef lpKeyName As String, ByRef lpDefault As String, ByRef lpFileName As String) As String
    Dim Xlon As Integer
    Dim Getstr As String
    Getstr = Space(1024)
    Xlon = GetPrivateProfileString(lpApplicationName, lpKeyName, lpDefault, Getstr, 1024, lpFileName)
    '
    GetPrivSettings = Left(Getstr, Xlon)
    If GetPrivSettings = "" Then
      GetPrivSettings = lpDefault
    End If
  End Function
  '
  '
  '
  'KF Kombinationen (ohne Wiederholung) von NF Elementen
  '
  '
  Shared Function GetKombination(ByRef Start As Boolean, ByVal KF As Integer, ByVal NF As Integer, ByRef LK() As Integer) As Boolean
    Dim i As Integer
    Dim j As Integer
    Dim KJ As Integer
    Dim KJ1 As Integer
    GetKombination = True
    If Start Then
      For i = 0 To KF - 1
        LK(i) = i
      Next i
      Start = False
    Else

      For j = 0 To KF - 1
        KJ = KF - j - 1
        If LK(KJ) < (NF - j - 1) Then
          Exit For
        End If
      Next j
      If j = KF Then
        GetKombination = False
        Start = True
        Exit Function
      Else
        LK(KJ) = LK(KJ) + 1
        If KJ = KF - 1 Then
          Exit Function
        End If
        KJ1 = KJ + 1
        For i = KJ1 To KF - 1
          LK(i) = LK(i - 1) + 1
        Next i
      End If
    End If
  End Function
  '
  '
  
  '
  '
  ' Heap sort routine.
  ' Returns a sorted Index array for the Keys array.
  ' Author: Christian d'Heureuse (www.source-code.biz)
  Public Shared Function HeapSort(ByVal Keys As Object) As Integer()
    Dim index() As Integer
    Dim n As Integer
    n = UBound(Keys) + 1       ' array size
    ReDim index(n - 1)                                 ' allocate index array
    Dim i As Long, m As Long
    For i = 0 To n - 1                                        ' fill index array
      index(i) = i
    Next
    For i = n \ 2 - 1 To 0 Step -1                           ' generate ordered heap
      Heapify(Keys, index, i, n)
    Next
    For m = n To 2 Step -1
      Exchange(index, 0, m - 1)                              ' move highest element to top
      Heapify(Keys, index, 0, m - 1)
    Next
    HeapSort = index
  End Function

  Private Shared Sub Heapify(ByVal Keys As Object, ByVal Index() As Integer, ByVal i1 As Integer, ByVal n As Integer)
    ' Heap order rule: a[i] >= a[2*i+1] and a[i] >= a[2*i+2]
    Dim k As Integer
    Dim nDiv2 As Integer
    Dim i As Integer
    nDiv2 = n \ 2
    i = i1
    Do While i < nDiv2
      k = 2 * i + 1
      If k + 1 < n Then
        If Keys(Index(k)) < Keys(Index(k + 1)) Then k = k + 1
      End If
      If Keys(Index(i)) >= Keys(Index(k)) Then Exit Do
      Exchange(Index, i, k)
      i = k
    Loop
  End Sub

  Private Shared Sub Exchange(ByVal a() As Integer, ByVal i As Integer, ByVal j As Integer)
    Dim Temp As Integer
    Temp = a(i)
    a(i) = a(j)
    a(j) = Temp
  End Sub

  Public Shared Function GetFileProfile(ByVal ProfileString As String) As String
    Dim Inn As Integer
    Dim EnvVar() As String = {"COLORWIN", "USERPROFILE", "PROGRAMFILES", "WINDIR"}
    Dim i As Integer
    GetFileProfile = ProfileString
    '
    '
    For i = 0 To EnvVar.Count - 1
      Inn = GetFileProfile.IndexOf("%" & EnvVar(i) & "%")
      If Inn >= 0 Then
        GetFileProfile = GetFileProfile.Replace("%" & EnvVar(i) & "%", Environment.GetEnvironmentVariable(EnvVar(i)))
        Exit Function
      End If
    Next
    '
  End Function

  Public Shared Function LetFileProfile(ByVal ProfileString As String) As String
    Dim Inn As Integer
    Dim EnvVar() As String = {"COLORWIN", "USERPROFILE", "PROGRAMFILES", "WINDIR"}
    Dim i As Integer
    LetFileProfile = ProfileString
    '
    '
    For i = 0 To EnvVar.Count - 1
      If Not IsNothing(Environment.GetEnvironmentVariable(EnvVar(i))) Then
        Inn = LetFileProfile.IndexOf(Environment.GetEnvironmentVariable(EnvVar(i)))
        If Inn >= 0 Then
          LetFileProfile = LetFileProfile.Replace(Environment.GetEnvironmentVariable(EnvVar(i)), "%" & EnvVar(i) & "%")
          Exit Function
        End If
      End If
    Next
    '
  End Function
End Class
