Option Compare Text
Option Strict Off
Option Explicit On

Public Class ColorMathLib

  Function Power(ByRef p As Single, ByRef X As Single) As Single
    Dim Pint As Short
    Dim Phil As Single
    Dim a As Double
    a = Log(X)
    If X > 0.0# Then
      Power = CSng(Exp(p * Log(X)))
    ElseIf X < 0.0# Then
      Pint = CShort(p)
      Phil = CSng(Exp(Pint * Log(Abs(X))))
      If (Pint Mod 2) = 0 Then
        Power = Phil
      Else
        Power = -Phil
      End If
    Else
      Power = 0.0#
    End If
  End Function


  Shared Sub Akint(ByRef N As Short, ByRef xein() As Single, ByRef yein() As Single, ByRef abcd(,) As Single, ByRef ier As Short)

    'UPGRADE_WARNING: Lower bound of array st was changed from 1 to 0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Dim st(N + 4) As Single
    'UPGRADE_WARNING: Lower bound of array t was changed from 1 to 0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Dim t(N + 4) As Single
    'UPGRADE_WARNING: Lower bound of array xneu was changed from 1 to 0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Dim xneu(N + 4) As Single
    'UPGRADE_WARNING: Lower bound of array yneu was changed from 1 to 0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Dim yneu(N + 4) As Single
    Dim Amabs, Amax, Anenn As Single
    Dim i, j As Integer
    Dim eps As Single
    '
    '
    'AKIMA-Interpolation
    '
    '
    '
    eps = 0.000001
    ier = 0
    For i = 0 To N - 1
      xneu(i) = xein(i)
      yneu(i) = yein(i)
    Next i
    Call Extrap(N, xneu, yneu, st, 1, ier)
    If ier <> 0 Then Exit Sub

    '
    '
    '
    'Berechnung der Steigungen
    '
    '
    For i = 2 To N + 1
      Amax = 0.0#
      For j = i - 1 To i
        Amabs = Abs(st(j))
        If Amabs > Amax Then
          Amax = Amabs
        End If
      Next j
      If (Abs(st(i + 1) - st(i)) + Abs(st(i - 1) - st(i - 2))) < eps Then
        t(i) = 0.5 * (st(i + 1) + st(i))
      Else
        t(i) = (Abs(st(i + 1) - st(i)) * st(i - 1) + Abs(st(i - 1) - st(i - 2)) * st(i)) / (Abs(st(i + 1) - st(i)) + Abs(st(i - 1) - st(i - 2)))

      End If
    Next i
    '
    '
    'Rückindizierung 2 Werte nach unten
    '
    '
    For i = 0 To N - 1
      t(i) = t(i + 2)
      st(i) = st(i + 2)
      yneu(i) = yneu(i + 2)
    Next i

    '
    '
    'Polynomkoeffizienten
    '
    '
    For i = 0 To N - 2
      abcd(i, 0) = yneu(i)
      abcd(i, 1) = t(i)
      Anenn = xein(i + 1) - xein(i)
      If Anenn = 0.0# Then
        ier = 1
        Exit Sub
      End If
      abcd(i, 2) = (3.0# * st(i) - 2.0# * t(i) - t(i + 1)) / Anenn
      Anenn = (xein(i + 1) - xein(i)) * (xein(i + 1) - xein(i))
      If Anenn = 0.0# Then
        ier = 1
        Exit Sub
      End If
      abcd(i, 3) = (t(i) + t(i + 1) - 2.0# * st(i)) / Anenn
    Next i
  End Sub
  Shared Sub Intaki(ByRef abcd(,) As Single, ByRef N As Short, ByRef X() As Single, ByRef M As Short, ByRef xa() As Single, ByRef ya() As Single)
    Dim v As Single
    Dim j, i, k As Short
    Dim Hilf As Single
    If X(0) < X(N - 1) Then
      v = 1.0#
    Else
      v = -1.0#
    End If
    For k = 0 To M - 1
      If v * xa(k) <= v * X(0) Then
        i = 0
      ElseIf v * xa(k) > v * X(N - 1) Then
        i = N - 2
      Else
        i = N - 2
        For j = 0 To N - 2
          If (v * xa(k) > v * X(j)) And (v * xa(k) <= v * X(j + 1)) Then
            i = j
            Exit For
          End If
        Next j
      End If
      Hilf = xa(k) - X(i)
      '     ya(k) = abcd(i, 1) + abcd(i, 2) * Power(1#, Hilf) _
      ''                       + abcd(i, 3) * Power(2#, Hilf) _
      ''                       + abcd(i, 4) * Power(3#, Hilf)
      ya(k) = abcd(i, 0) + abcd(i, 1) * Hilf + abcd(i, 2) * Hilf * Hilf + abcd(i, 3) * Hilf * Hilf * Hilf

    Next k
  End Sub
  Private Shared Sub Extrap(ByRef N As Short, ByRef xneu() As Single, ByRef yneu() As Single, ByRef st() As Single, ByRef mlin As Short, ByRef ier As Short)
    Dim i As Short
    Dim Hilf As Single
    '
    '
    '2 Indizes nach oben verschieben
    '
    '
    '
    For i = 0 To N - 1
      xneu(N + 3 - i) = xneu(N + 1 - i)
      yneu(N + 3 - i) = yneu(N + 1 - i)
    Next i
    '
    'extrapolierte x-Werte
    '
    '
    xneu(1) = xneu(2) + xneu(3) - xneu(4)
    xneu(0) = xneu(1) + xneu(2) - xneu(3)
    xneu(N + 2) = xneu(N + 1) + xneu(N) - xneu(N - 1)
    xneu(N + 3) = xneu(N + 2) + xneu(N + 1) - xneu(N)
    '
    '
    'Berechnung der Steigungen
    '
    '
    For i = 2 To N
      Hilf = xneu(i + 1) - xneu(i)
      If Hilf = 0.0# Then
        ier = 1
        Exit Sub
      Else
        st(i) = (yneu(i + 1) - yneu(i)) / Hilf
      End If
    Next i
    '
    'extrapolierte y-Werte
    '
    '
    If mlin = 1 Then
      yneu(1) = (xneu(2) - xneu(1)) * (st(3) - 2.0# * st(2)) + yneu(2)
    Else
      yneu(1) = (xneu(2) - xneu(1)) * (-st(2)) + yneu(2)
    End If
    Hilf = xneu(2) - xneu(1)
    If Hilf = 0.0# Then
      ier = 1
      Exit Sub
    Else
      st(1) = (yneu(2) - yneu(1)) / Hilf
    End If
    If mlin = 1 Then
      yneu(0) = (xneu(1) - xneu(0)) * (st(2) - 2.0# * st(1)) + yneu(1)
    Else
      yneu(0) = (xneu(1) - xneu(0)) * (-st(1)) + yneu(1)
    End If
    Hilf = xneu(1) - xneu(0)
    If Hilf = 0.0# Then
      ier = 1
      Exit Sub
    Else
      st(0) = (yneu(1) - yneu(0)) / Hilf
    End If
    If mlin = 1 Then
      yneu(N + 2) = (2.0# * st(N) - st(N - 1)) * (xneu(N + 2) - xneu(N + 1)) + yneu(N + 1)
    Else
      yneu(N + 2) = st(N) * (xneu(N + 2) - xneu(N + 1)) + yneu(N + 1)
    End If
    Hilf = xneu(N + 2) - xneu(N + 1)
    If Hilf = 0.0# Then
      ier = 1
      Exit Sub
    Else
      st(N + 1) = (yneu(N + 2) - yneu(N + 1)) / Hilf
    End If
    If mlin = 1 Then
      yneu(N + 3) = (2.0# * st(N + 1) - st(N)) * (xneu(N + 3) - xneu(N + 2)) + yneu(N + 2)
    Else
      yneu(N + 3) = st(N + 1) * (xneu(N + 3) - xneu(N + 2)) + yneu(N + 2)
    End If
    Hilf = xneu(N + 3) - xneu(N + 2)
    If Hilf = 0 Then
      ier = 1
      Exit Sub
    Else
      st(N + 2) = (yneu(N + 3) - yneu(N + 2)) / Hilf
    End If
  End Sub

  Function LinXY(ByRef XV As Single, ByRef XN As Single, ByRef Alp As Single) As Single
    LinXY = XV + Alp * (XN - XV)
  End Function





  

  Shared Sub MatSetBer(ByRef RGB(,) As Single, ByRef XYZ(,) As Single, ByRef A(,) As Single, ByRef ier As Integer)
    Dim i As Short
    Dim k As Short
    Dim j As Short
    Dim IT As Short
    Dim Ahlf As Single
    Dim B(2, 2) As Single
    Dim DA(2, 2) As Single
    Dim RGBC(2) As Integer
    Dim XYZC(2) As Single
    Dim DRGBMAT(2, 2) As Single
    Dim HoRGB(2) As Single
    ier = 0
    '
    'Startwert für A
    '
    ReadAHoRGB(A, HoRGB)
    '
    'Matrix XYZ invertieren
    '
    '
    Call MatInv3(XYZ, B, ier)
    If ier <> 0 Then
      Exit Sub
    End If
    '
    '
    'Iteration
    '
    '
    For IT = 0 To 10
      For i = 0 To 2
        For j = 0 To 2
          XYZC(j) = XYZ(i, j)
        Next
        Call RGBFromXYZ(RGBC, XYZC, XYZWNL65, A, HoRGB)
        For j = 0 To 2
          DRGBMAT(i, j) = (RGB(i, j) - RGBC(j))
        Next
      Next i

      For i = 0 To 2
        For k = 0 To 2
          Ahlf = 0
          For j = 0 To 2
            Ahlf = Ahlf + DRGBMAT(j, i) * B(k, j)
          Next j
          DA(i, k) = Ahlf
          A(i, k) = A(i, k) + DA(i, k)
        Next k
      Next i
    Next IT
  End Sub
End Class
