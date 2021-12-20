Option Strict Off
Option Explicit On
Option Compare Text
Public Class MessAdjust
  Sub CalculateNewAbsValues(GWI As Single, MessgSLA As MeasParameters, Normfa As NormIlluminat, Kennneu As String, ByRef GRPRwerte As RefValuesGrp, ByRef ier As Integer)
    Dim i As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim ISW As Integer
    Dim nkw As Integer
    Dim DE As Single
    Dim DL As Single
    Dim DC As Single
    Dim DH As Single
    Dim DA As Single
    Dim DB As Single
    Dim MessAnz As Integer
    Dim RMAST(,) As Single
    Dim RSLA(,) As Single
    Dim RRECH(,) As Single
    Dim Measure As MeasureReflex
    '
    '
    '
    If Kennneu.Trim = "" Then
      MsgBox("keine neue Kennung")
      ier = 12
      Exit Sub
    End If
    '
    '
    '
    '
    Measure = New MeasureReflex
    Mnier = Measure.ier

    Measure.Umspeich(MessgSLA, Normfa)
    WithClasm = False
    If Mnier = -3 Then
      MsgBox(Texxt(3512))
    End If
    Measure.MessDevInit()
    '
    '
    ISW = 1
    If idka(0) <> 0 And idka(1) <> 0 Then
      ISW = 2
    End If
    '
    '
    '
    Call RMNUL(1.0, MessgNwe, MessgKM, rsmes(0), NGK, gk(0, 0), MessgID, NormID, Fakt(0), XYZE(0, 0), ier)
    '
    '
    '
    MessAnz = GRPRwerte("M").Count
    ReDim RMAST(MessAnz - 1, MessgNwe - 1)
    ReDim RSLA(MessAnz - 1, MessgNwe - 1)
    ReDim RRECH(MessAnz - 1, MessgNwe - 1)
    '
    GRPRwerte("R").clear()
    For k = 0 To MessAnz - 1
      GRPRwerte("R").Add(KeyRe(k), New RefValue)
    Next
    For kw = 0 To MessgKM - 1
      For k = 0 To MessAnz - 1
        For i = 0 To MessgNwe - 1
          RMAST(k, i) = GRPRwerte("M")(k).RefKurv(MessgSLA.Winkel(kw).Chrm).R(i)
          RSLA(k, i) = GRPRwerte("S")(k).RefKurv(MessgSLA.Winkel(kw).Chrm).R(i)
        Next i
        If kw = 0 Then
          Call DIFFREF(MessgNwe, 0, 1, 1, RMAST(k, 0), RSLA(k, 0), DE, DL, DC, DH, DA, DB, ier)
          GRPRwerte("S")(k).De(0) = DE
        End If
      Next k
      nkw = kw * MessgNwe
      '
      For i = 0 To MessgNwe - 1
        If Abs(MessgQG(nkw + i)) > 0.0001 Then
          ier = 13
          GoTo endCalcFit
        End If
      Next
      '

      Call MESSFIT(GWI, ISW, MessAnz, MessgNwe, MessgQW(nkw), MessgQS(nkw), MessgQG(nkw), MessgWist(nkw), MessgWsol(nkw), RMAST(0, 0), RSLA(0, 0), RRECH(0, 0), ier)
      '
      '
      '
      'zurückspeichern
      '
      '
      '
      For k = 0 To MessAnz - 1
        If kw = 0 Then
          Call DIFFREF(MessgNwe, 0, 1, 1, RMAST(k, 0), RRECH(k, 0), DE, DL, DC, DH, DA, DB, ier)
          GRPRwerte("R")(k).Iami = 1
          GRPRwerte("R")(k).De(0) = DE
        End If

        GRPRwerte("R")(KeyRe(k)).RefKurv.Add(MessgSLA.Winkel(kw).Chrm, New CurveRef(MessgNwe))
        For i = 0 To MessgNwe - 1
          GRPRwerte("R")(k).RefKurv(MessgSLA.Winkel(kw).Chrm).R(i) = RRECH(k, i)
        Next i

      Next k
    Next kw
    MessgKenn = Kennneu
    Call IniSpei()
    Measure.MessDevInit()
endCalcFit:
    Measure.dispose()
  End Sub
  Sub CalculateNewRefValues(MessgSlA As MeasParameters, Normfa As NormIlluminat, Kennneu As String, ByRef GRPRwerte As RefValuesGrp, ByRef ier As Integer)
    Dim i As Integer
    Dim k As Integer
    Dim j As Integer
    Dim kw As Integer
    Dim iheute As Integer
    Dim RR() As Single
    Dim QWalt() As Single
    Dim QSalt() As Single
    Dim QGalt() As Single
    Dim WSalt() As Single
    Dim MW() As Single
    Dim MS() As Single
    Dim MG() As Single
    Dim MK() As Single
    Dim Measure As MeasureReflex
    If Kennneu.Trim = "" Then
      MsgBox("keine neue Kennung")
      ier = 12
      Exit Sub
    End If
    '
    '

    '
    '
    Measure = New MeasureReflex
    Mnier = Measure.ier
    WithClasm = False
    Measure.Umspeich(MessgSlA, Normfa)
    If Mnier = -3 Then
      MsgBox(Texxt(3512))
    End If
    '
    '
    ReDim QWalt(Mdim)
    ReDim QSalt(Mdim)
    ReDim QGalt(Mdim)
    ReDim MW(Mdim)
    ReDim MS(Mdim)
    ReDim MG(Mdim)
    ReDim WSalt(Mdim)
    ReDim RR(MessgNwe - 1)
    ReDim MK(MessgNwe - 1)
    '
    '
    '
    For i = 0 To Mdim
      MW(i) = 1.0
      MS(i) = 0.0
      MG(i) = 0.0
      QWalt(i) = MessgQW(i)
      QSalt(i) = MessgQS(i)
      QGalt(i) = MessgQG(i)
      WSalt(i) = MessgWist(i)
    Next
    '
    '
    MessgKenn = Kennneu
    '
    'korrigierte Absolutwerte einlesen
    Call KalIntDat(iheute)
    If iheute = 3 Then
      MsgBox("korrigierte Werte für neue Kennung " & Kennneu & " nicht vorhanden")
      ier = 14
      Exit Sub

    End If
    '
    '
    Call RMNUL(1.0, MessgNwe, MessgKM, MG(0), NGK, gk(0, 0), MessgID, NormID, Fakt(0), XYZE(0, 0), ier)

    '
    'Berechnung der ursprünglichen Messwerte MK
    '
    GRPRwerte("R").clear()
    For k = 0 To GRPRwerte("S").Count - 1
      GRPRwerte("R").Add(KeyRe(k), GRPRwerte("S")(KeyRe(k)).clone)
      '
      For kw = 0 To MessgKM - 1
        For i = 0 To MessgNwe - 1
          RR(i) = GRPRwerte("S")(k).RefKurv(kw).R(i)
        Next i
        '
        '
        j = kw * MessgNwe
        Call MESSWRT(MessgNwe, QWalt(j), QSalt(j), QGalt(j), MW(j), MS(j), MG(j), WSalt(j), MessgWsol(j), RR(0), MK(0), ier)
        '
        'Umrechnung in neue R-werte mit Hilfe der korrigierten Absolutwerten (Messgqw,Messgqs,MessgWist)
        '
        Call REFWRT(MessgNwe, MessgQW(j), MessgQS(j), MessgQG(j), MW(j), MS(j), MG(j), MessgWist(j), MessgWsol(j), MK(0), RR(0), ier)
        GRPRwerte("R")(k).Cme = Kennneu
        For i = 0 To MessgNwe - 1
          GRPRwerte("R")(k).RefKurv(kw).R(i) = RR(i)
        Next i
      Next kw
    Next k
    Try
      Measure.dispose()
    Catch
    End Try
  End Sub
End Class
