Option Compare Text
Option Strict Off
Option Explicit On
Public Class Mathematic
  Declare Sub MATINVER Lib "MATHDLL" (ByRef M As Integer, ByRef N As Integer, ByRef A As Double, ByRef MDB As Integer, ByRef NB As Integer, ByRef B As Double, ByRef TAU As Double, ByRef KRANK As Integer, ByRef IER As Integer)
  'X(N-1),Y(N-1),Y2EXP(N-1)
  'Y2EXP berechnen
  Declare Sub SPLINDLL Lib "MATHDLL" (ByRef N As Integer, ByRef X As Double, ByRef Y As Double, ByRef Y2EXP As Double, ByRef XSI As Double, ByRef YP1 As Double, ByRef YPN As Double, ByRef ier As Integer)
  'YS aus XS berechnen
  Declare Sub SPLITDLL Lib "MATHDLL" (ByRef N As Integer, ByRef X As Double, ByRef Y As Double, ByRef Y2EXP As Double, ByRef XS As Double, ByRef YS As Double, ByRef XSI As Double, ByRef YSDX As Double, ByRef IER As Integer)
  Declare Sub AKIMAINT Lib "MATHDLL" (ByRef N As Integer, ByRef XSTZ As Double, ByRef YSTZ As Double, ByRef M As Integer, ByRef X As Double, ByRef Y As Double, ByRef IER As Integer)

  Shared Function Arsinh(ByRef X As Single) As Single
    If X > 0.0# Then
      If Abs(X) < 1000.0# Then
        Arsinh = Log(X + Sqrt(1.0# + X * X))
      Else
        Arsinh = Log(2.0# * X)
      End If
    Else
      If Abs(X) < 1000.0# Then
        Arsinh = -Log(-X + Sqrt(1.0# + X * X))
      Else
        Arsinh = -Log(-2.0# * X)
      End If
    End If
  End Function
  Shared Sub MATInvers(ByRef M As Integer, ByRef N As Integer, ByRef A(,) As Double, ByRef MDB As Integer, ByRef NB As Integer, ByRef B(,) As Double, ByRef TAU As Double, ByRef KRANK As Integer, ByRef IER As Integer)
    'Dimensionierung in VB-NET
    'A(N-1,M-1) Matrix des Gleichungssystems (M Gleichungen; N Unbekannte) 
    'B(NB-1,MDB-1) rechte Seite(Eingabe) bzw. Ergebnis X(Ausgabe); MDB>=MAX(M,N)
    'Ist B bei der Eingabe die Einheitsmatrix, dann ist B bei der Ausgabe die Pseudoinverse von A)
    Call MATINVER(M, N, A(0, 0), MDB, NB, B(0, 0), TAU, KRANK, IER)
  End Sub
  Shared Sub MatInv3(ByRef A(,) As Single, ByRef B(,) As Single, ByRef ier As Integer)
    Dim det As Single
    ier = 0
    det = A(0, 0) * A(1, 1) * A(2, 2) + A(0, 1) * A(1, 2) * A(2, 0) + A(0, 2) * A(1, 0) * A(2, 1) - A(0, 2) * A(1, 1) * A(2, 0) - A(0, 0) * A(1, 2) * A(2, 1) - A(0, 1) * A(1, 0) * A(2, 2)
    If Abs(det) < 0.00000001 Then
      ier = 1
      Exit Sub
    End If
    B(0, 0) = (A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)) / det
    B(1, 0) = (A(1, 2) * A(2, 0) - A(1, 0) * A(2, 2)) / det
    B(2, 0) = (A(1, 0) * A(2, 1) - A(1, 1) * A(2, 0)) / det
    B(0, 1) = (A(0, 2) * A(2, 1) - A(0, 1) * A(2, 2)) / det
    B(1, 1) = (A(0, 0) * A(2, 2) - A(0, 2) * A(2, 0)) / det
    B(2, 1) = (A(2, 0) * A(0, 1) - A(0, 0) * A(2, 1)) / det
    B(0, 2) = (A(0, 1) * A(1, 2) - A(1, 1) * A(0, 2)) / det
    B(1, 2) = (A(1, 0) * A(0, 2) - A(0, 0) * A(1, 2)) / det
    B(2, 2) = (A(0, 0) * A(1, 1) - A(0, 1) * A(1, 0)) / det
  End Sub
  Shared Function InnerPunkt(M As Integer, NB As Integer, DXYZS(,) As Single, DXYZ() As Single, TAU As Double) As Boolean
    '
    'rufendes Programm für Test mit Simplex
    '
    '
    ' M = 4 (4 Simplexpunkte)
    ' N = 3 (Dimension 3)
    'MDB = Max(M, N)
    'NB = 3
    'ReDim DXYZS(M - 1, NB - 1)
    'ReDim DXYZ(NB - 1)
    '
    '
    '
    Dim i As Integer
    Dim j As Integer
    Dim krank As Integer
    Dim ier As Integer
    Dim MDB As Integer
    Dim A(,) As Double
    Dim B(,) As Double
    MDB = Max(M, NB)
    ReDim A(M - 1, NB)
    ReDim B(0, MDB - 1)
    InnerPunkt = True
    For i = 0 To NB - 1
      B(0, i) = 0
      For j = 0 To M - 1
        A(j, i) = DXYZS(j, i) - DXYZ(i)
      Next
    Next
    B(0, NB) = 1
    For j = 0 To M - 1
      A(j, NB) = 1.0
    Next
    Call MATInvers(M, NB + 1, A, MDB, 1, B, TAU, krank, ier)
    For i = 0 To M - 1
      If B(0, i) < 0 Then
        InnerPunkt = False
        Exit For
      End If
    Next

  End Function
  Public Shared Function HUGE() As Single
    HUGE = 3.4E+38
  End Function
  Public Shared Function TINY() As Single
    TINY = 1.18E-38
  End Function
  Shared Sub CalcSpline(XSource() As Single, Ysource() As Single, XTarget() As Single, ByRef YTarget() As Single, ByRef ier As Integer)
    Dim j As Integer
    Dim YS() As Double
    Dim XS() As Double
    Dim XT() As Double
    Dim YA2() As Double
    Dim XSI As Double
    Dim YP1 As Double
    Dim YPN As Double
    Dim YSDX As Double
    Dim Xvalue As Double
    Dim Yvalue As Double
    XSI = 10.0
    YP1 = 0.0
    YPN = 0.0
    ier = 0
    ReDim XS(XSource.Count - 1)
    ReDim YS(Ysource.Count - 1)
    ReDim YA2(XSource.Count - 1)
    ReDim XT(XTarget.Count - 1)
    '
    '
    '
    For j = 0 To XSource.Count - 1
      YS(j) = Ysource(j)
      XS(j) = XSource(j)
    Next j
    For j = 0 To XTarget.Count - 1
      XT(j) = XTarget(j)
    Next j
    Call SPLINDLL(XS.Count, XS(0), YS(0), YA2(0), XSI, YP1, YPN, ier)
    '
    '
    '
    For j = 0 To XT.Count - 1
      Xvalue = XT(j)
      Call SPLITDLL(XS.Count, XS(0), YS(0), YA2(0), Xvalue, Yvalue, XSI, YSDX, ier)
      YTarget(j) = Yvalue
    Next j
  End Sub

End Class
