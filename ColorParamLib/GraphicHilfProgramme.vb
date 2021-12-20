Public Class GraphicHilfProgramme
  Dim MnGraphBounds As RectangleF
  Dim BrushZ As SolidBrush
  Dim Size As SizeF

  Sub RefPaant(ByRef Text As String, ByRef Namen() As String, ByRef Rmii As Single, ByRef Rmaa As Single, ByRef Winkel As AngGeos, ByRef Kurvs As CurvesRefGrp, ByRef Fakt As Single, ByRef ity As Short, ByRef istr As Short, ByRef graph As Graphics, ByRef RefPointF() As PointF, ByRef RefWert() As Single, ByRef ier As Short)
    Dim Rmin, Rmax As Single
    ier = 0
    If Kurvs.Count = 0 Then
      ier = 9
      Exit Sub
    End If
    If Kurvs(0).Count = 0 Then
      ier = 8
      Exit Sub
    End If
    If Rmaa < 0.0# Then
      Rmin = Rmii
      Rmax = Rmaa
      Call Skali(Kurvs, Rmin, Rmax, ity)
    Else
      Rmin = 0.0#
      Rmax = Rmaa
    End If
    Call Kwplot(Text, Namen, Winkel, Kurvs, Rmin, Rmax, graph, RefPointF, RefWert, Fakt, ity, istr)
    Rmii = Rmin
    Rmaa = Rmax
  End Sub

  '






  Function DrSty(ByRef k As Short) As DashStyle
    Dim km As Short
    DrSty = DashStyle.Solid
    km = k Mod 5
    Select Case km
      Case 0
        DrSty = DashStyle.Solid
      Case 1
        DrSty = DashStyle.Dot
      Case 2
        DrSty = DashStyle.DashDotDot
      Case 3
        DrSty = DashStyle.DashDot
      Case 4
        DrSty = DashStyle.Dash
    End Select
  End Function



  Private Sub Kwplot(ByRef Text As String, ByRef Namm() As String, ByRef Winkel As AngGeos, ByRef Kurvs As CurvesRefGrp, ByRef Rmin As Single, ByRef Rmax As Single, ByRef GraPh As Graphics, ByRef RefPointF() As PointF, ByRef RefWert() As Single, ByRef Fakt As Single, ByRef ity As Short, ByRef ist As Short)
    Dim jj, j, i, kk, k As Short
    Dim istr As Short
    Dim Innn As Short
    Dim aaa As Single
    Dim adif As Single
    Dim XX As Single
    Dim xa As Single
    Dim XM As Single
    Dim YM As Single
    Dim X0 As Single
    Dim Y0 As Single
    Dim XMMM As Single
    Dim Dif As Single
    Dim tif As Single
    Dim iws As Short
    Dim iblk As Short
    Dim Y As Single
    Dim X As Single
    Dim ams As Single
    Dim amy As Single
    Dim amx As Single
    Dim ddx As Single
    Dim ynam As String
    Dim aym As Single
    Dim ye As Single
    Dim npo As Short
    Dim mpp As Short
    Dim mpj As Short
    Dim jl As Short
    Dim Alp As Single
    Dim ay As Single
    Dim Hilf As Single
    Dim dx As Single
    Dim nk, Naus As Short
    Dim Nwp As Short
    Dim tiny As Single
    Dim Delta As Single
    Dim Deemax As Single
    Dim NammHeight As Single
    Dim Yline As Single
    Dim Kurve() As PointF
    Dim MnPenObj As New Pen(Color.Black, 1)
    Dim PenNew As New Pen(MnPenObj.Color, MnPenObj.Width)
    Dim PlNam As String
    Dim Iref As Integer
    '
    Erase RefPointF
    Erase RefWert
    tiny = 1.0E-30
    Delta = 0.001
    Deemax = 1000
    If Rmax < tiny Then
      Rmax = tiny
    End If
    '
    istr = ist Mod 2
    Innn = ist / 2
    '
    '
    '
    nk = Kurvs.Count
    If nk = 0 Then Exit Sub
    Naus = Kurvs(0).Count
    Nwp = Winkel.Wsol.Nwe


    PenNew.Color = Color.Black
    PenNew.DashStyle = DrSty(1)

    'MnGraphBounds.StrFormat.Trimming = StringTrimming.Word
    X0 = MnGraphBounds.Left + 0.13 * MnGraphBounds.Width
    XM = MnGraphBounds.Left + 0.9243359 * MnGraphBounds.Width
    Y0 = MnGraphBounds.Top + MnGraphBounds.Height - 3 * RezFont(2).GetHeight(GraPh)
    NammHeight = RezFont(2).GetHeight(GraPh)
    YM = MnGraphBounds.Top + 0.03 * MnGraphBounds.Height + Int((nk + 1) / 2) * NammHeight
    adif = Fakt * (Rmax - Rmin) / 4.0#
    amx = XM - X0
    XX = amx / (Winkel.Wsol.R(Nwp - 1) - Winkel.Wsol.R(0) + tiny)
    xa = XX * (400.0# - Winkel.Wsol.R(0))
    XMMM = XX * (Winkel.Wsol.R(Nwp - 1) - Winkel.Wsol.R(0))
    Dif = 100.0# * XX
    tif = 20.0# * XX
    iws = 400
    iblk = 0

    amy = Y0 - YM
    Y = Y0 + 0.0345 * amy
    '
    'X-Achse
    '
    '
    '
    PenNew.Color = Drawing.Color.Black
    GraPh.DrawLine(PenNew, X0, Y0, XM, Y0)

    '
    'Gitterlinien
    '
    'Beschriftung x-Achse
    '
    '
    i = 0
    ddx = 0.02 * amx
    X = X0 + xa - ddx - tif * Int((400.0# - Winkel.Wsol.R(0) + 0.5) / 20.0#)
    Do
      If Abs(X - X0 - xa + ddx - i * Dif) < Delta Then
        GraPh.DrawString(Format(iws, "###"), RezFont(2), BrushZ, X - 0.8 * ddx, Y - 0.01F * amy)

        GraPh.DrawLine(PenNew, X + ddx, Y - 0.0086F * amy, X + ddx, Y - 0.0603F * amy)

        i = i + 1
        iws = iws + 100
      Else
        GraPh.DrawLine(PenNew, X + ddx, Y - 0.0172F * amy, X + ddx, Y - 0.0517F * amy)
      End If
      PenNew.DashStyle = DashStyle.Dot
      GraPh.DrawLine(PenNew, X + ddx, YM, X + ddx, Y - 0.0517F * amy)

      PenNew.DashStyle = DashStyle.Solid

      X = X + tif
    Loop While (X - X0) <= XMMM
    '
    'Y-Achse

    Select Case ity
      Case 0
        '
        'linearer Maßstab
        '
        '
        '
        X = 0.0035 * amx
        ams = Fakt * Rmax
        GraPh.DrawLine(PenNew, X0, YM, X0, Y - 0.0172F * amy)

        For i = 0 To 3
          Y = YM + i * 0.25 * amy
          GraPh.DrawLine(PenNew, X0 - X, Y, X0 + 0.0052F * amx, Y)

          PenNew.DashStyle = DashStyle.Dot

          GraPh.DrawLine(PenNew, X0 + 0.0052F * amx, Y, XM, Y)
          PenNew.DashStyle = DashStyle.Solid

          If Fakt * Rmax >= 100.0# Then
            ynam = Format(ams, "### ")
          ElseIf Fakt * Rmax >= 10.0# Then
            ynam = Format(ams, "###.0 ")
          ElseIf Fakt * Rmax >= 1.0# Then
            ynam = Format(ams, "###.00 ")
          Else
            ynam = Format(ams, "###.00 ")
          End If
          Size = GraPh.MeasureString(ynam, RezFont(2))
          ddx = Size.Width
          GraPh.DrawString(ynam, RezFont(2), BrushZ, X0 - ddx, Y - 0.01F * amy)
          ams = ams - adif
        Next i
        aaa = (Y0 - YM) / (Fakt * (Rmax - Rmin))

        For j = 0 To nk - 1
          jj = j Mod 16
          For k = 0 To Naus - 1
            kk = k Mod 16
            Erase Kurve
            If istr = 0 Then
              PenNew.Color = farbini(jj)
              PenNew.DashStyle = DrSty(kk)
            Else
              PenNew.Color = farbini(k)
              PenNew.DashStyle = DrSty(j)
            End If
            For i = 0 To Nwp - 1
              X = X0 + XX * (Winkel.Wsol.R(i) - Winkel.Wsol.R(0))
              Y = Y0 - aaa * Fakt * (Kurvs(j)(k).R(i) - Rmin)
              ReDim Preserve Kurve(i)
              Kurve(i) = New PointF(X, Y)
              Iref = 0
              If Not IsNothing(RefPointF) Then
                Iref = RefPointF.Count
              End If
              ReDim Preserve RefPointF(Iref)
              RefPointF(Iref) = New PointF(X, Y)
              ReDim Preserve RefWert(Iref)
              RefWert(Iref) = Fakt * Kurvs(j)(k).R(i)
            Next i
            GraPh.DrawCurve(PenNew, Kurve, 0.5F)



          Next k
        Next j
        '
        '
        '
      Case 1
        '
        'logarithmischer Maßstab
        '
        '
        '
        Hilf = Log10(Fakt * Rmax)
        If Hilf < 0.0# Then
          npo = Int(Hilf) - 1
        Else
          npo = Int(Hilf)
        End If

        ye = 10.0# ^ npo
        aym = Deemax

        X = 0.0035 * amx
        ams = ye
        GraPh.DrawLine(PenNew, X0, YM, X0, Y - 0.0172F * amy)
        mpp = 5

        For j = 0 To 19
          mpj = j / mpp
          jl = j Mod mpp
          Alp = 0.2 * (mpp - jl)
          ay = amy * (-Log10(Alp) + mpj) / 4
          Y = YM + ay
          If jl = 0 Then
            GraPh.DrawLine(PenNew, X0 - X, Y, X0 + 0.0052F * amx, Y)

            PenNew.DashStyle = DashStyle.Dot
            GraPh.DrawLine(PenNew, X0 + 0.0052F * amx, Y, XM, Y)
            ynam = Format(npo + mpj - 3, "#,#00")
            '
            '
            ynam = "1.E" & ynam
            PenNew.DashStyle = DashStyle.Dot
            Size = GraPh.MeasureString(ynam, RezFont(2))
            ddx = Size.Width
            GraPh.DrawString(ynam, RezFont(2), BrushZ, X0 - ddx, Y - 0.01F * amy)
          Else
            GraPh.DrawLine(PenNew, X0 - X, Y, X0, Y)
          End If
        Next j

        '
        aaa = -Log10(ye)
        Hilf = 0.25 * amy
        '
        For j = 0 To nk - 1
          jj = j Mod 16
          For k = 0 To Naus - 1
            kk = k Mod 16
            Erase Kurve
            If istr = 0 Then
              PenNew.Color = farbini(jj)
              PenNew.DashStyle = DrSty(kk)
            Else
              PenNew.Color = farbini(kk)
              PenNew.DashStyle = DrSty(jj)
            End If
            For i = 0 To Nwp - 1
              X = X0 + XX * (Winkel.Wsol.R(i) - Winkel.Wsol.R(0))
              Y = YM - Hilf * (aaa + Log10(Kurvs(j)(k).R(i) + tiny))
              ReDim Preserve Kurve(i)
              Kurve(i) = New PointF(X, Y)
              Iref = 0
              If Not IsNothing(RefPointF) Then
                Iref = RefPointF.Count
              End If
              ReDim Preserve RefPointF(Iref)
              RefPointF(Iref) = New PointF(X, Y)
              ReDim Preserve RefWert(Iref)
              RefWert(Iref) = Fakt * Kurvs(j)(k).R(i)
            Next i
            GraPh.DrawCurve(PenNew, Kurve, 0.5F)

          Next k
        Next j
        '
        '
      Case 2
        '
        '
        'ARSINH Massstab
        '
        '
        '
        Hilf = Log10(Fakt * Rmax) + 0.9999
        If Hilf < 0.0# Then
          npo = Int(Hilf) - 0
        Else
          npo = Int(Hilf)
        End If

        ye = 10.0# ^ npo
        aym = Deemax

        X = 0.0035 * amx
        ams = ye
        GraPh.DrawLine(PenNew, X0, YM, X0, Y - 0.0172F * amy)

        mpp = 5

        For j = 0 To 19
          mpj = Int((j) / mpp)
          jl = ((j) Mod mpp) + 1
          Alp = 2.0# * jl * 10.0# ^ mpj
          ay = amy * (1.0# - Arsinh(Alp) / Arsinh(aym))
          Y = YM + ay
          If jl = 5 Then
            GraPh.DrawLine(PenNew, X0 - X, Y, X0 + 0.0052F * amx, Y)

            PenNew.DashStyle = DashStyle.Dot
            GraPh.DrawLine(PenNew, X0 + 0.0052F * amx, Y, XM, Y)

            ynam = Format(npo + mpj - 2, "#,#00")
            ynam = "1.E" & ynam
            PenNew.DashStyle = DashStyle.Solid

            Size = GraPh.MeasureString(ynam, RezFont(2))
            ddx = Size.Width
            GraPh.DrawString(ynam, RezFont(2), BrushZ, X0 - ddx, Y - 0.01F * amy)
          Else
            GraPh.DrawLine(PenNew, X0 - 1.5F * X, Y, X0, Y)

          End If
        Next j

        aaa = Arsinh(aym)
        For j = 0 To nk - 1
          jj = j Mod 16
          For k = 0 To Naus - 1
            kk = k Mod 16
            Erase Kurve
            If istr = 0 Then
              PenNew.Color = farbini(jj)
              PenNew.DashStyle = DrSty(kk)
            Else
              PenNew.Color = farbini(kk)
              PenNew.DashStyle = DrSty(jj)
            End If
            For i = 0 To Nwp - 1
              X = X0 + XX * (Winkel.Wsol.R(i) - Winkel.Wsol.R(0))
              Y = Y0 - amy * Arsinh(aym * Fakt * Kurvs(j)(k).R(i) / ye) / aaa
              ReDim Preserve Kurve(i)
              Kurve(i) = New PointF(X, Y)
              Iref = 0
              If Not IsNothing(RefPointF) Then
                Iref = RefPointF.Count
              End If
              ReDim Preserve RefPointF(Iref)
              RefPointF(Iref) = New PointF(X, Y)
              ReDim Preserve RefWert(Iref)
              RefWert(Iref) = Fakt * Kurvs(j)(k).R(i)
            Next i
            GraPh.DrawCurve(PenNew, Kurve, 0.5F)

          Next k
        Next j

      Case Else
    End Select
    X0 = MnGraphBounds.Left + 0.005 * MnGraphBounds.Width
    Y0 = MnGraphBounds.Top
    dx = 0.5 * MnGraphBounds.Width
    PenNew.DashStyle = DashStyle.Solid

    For j = 0 To nk - 1
      If istr = 0 Then
        jj = j Mod 16
        PenNew.Color = farbini(jj)
        PenNew.DashStyle = DashStyle.Solid
      Else
        'FaId = 0
        PenNew.Color = Color.Black
        PenNew.DashStyle = DrSty(jj)
      End If
      Yline = Y0 + 0.5F * NammHeight
      GraPh.DrawLine(PenNew, X0, Yline, X0 + 0.1F * MnGraphBounds.Width, Yline)
      PlNam = Namm(j)
      If Len(PlNam) > 25 Then
        PlNam = PlNam.Substring(0, 25)
      End If
      GraPh.DrawString(PlNam, RezFont(2), BrushZ, X0 + 0.11F * MnGraphBounds.Width, Y0)

      If (j Mod 2) = 1 Then
        X0 = MnGraphBounds.Left + 0.005 * MnGraphBounds.Width
        Y0 = Y0 + NammHeight
      Else
        X0 = X0 + dx
      End If
    Next j
    Y0 = MnGraphBounds.Top + MnGraphBounds.Height - 1.5 * RezFont(2).GetHeight(GraPh)
    Size = GraPh.MeasureString(Text, RezFont(2))
    X0 = MnGraphBounds.Left + 0.5 * (MnGraphBounds.Width - Size.Width)

    GraPh.DrawString(Text, RezFont(2), BrushZ, X0, Y0)
    PenNew.Dispose()
  End Sub

  Private Sub Skali(ByRef Kurvs As CurvesRefGrp, ByRef Rmin As Single, ByRef Rmax As Single, ByRef ity As Short)
    Dim rrr As Single
    Dim j, i, k As Short
    Dim nk, Naus As Short
    Dim Nwe As Short
    nk = Kurvs.Count
    If nk = 0 Then Exit Sub
    rrr = 0.0
    Naus = Kurvs(0).Count
    Nwe = Kurvs(0)(0).Nwe
    For j = 0 To nk - 1
      For k = 0 To Naus - 1
        For i = 0 To Nwe - 1
          rrr = Kurvs(j)(k).R(i)
          If rrr > Rmax Then Rmax = rrr
        Next i
      Next k
    Next j
    Rmin = 0.0#
    Select Case ity
      Case 0
        '
        'linear
        '
        For i = 0 To RweSkal.Count - 1
          If Rmax < 0.01 * RweSkal(i) Then
            Rmax = 0.01 * RweSkal(i)
            Exit For
          End If
        Next i
      Case 1
        '
        '
        'logarithmisch
        '
        '
        For i = -10 To 10
          If Rmax < 10.0# ^ i Then
            Rmax = 10.0# ^ i
            Exit For
          End If
        Next i
      Case 2
        '
        'ArSinh
        '
        '
        '
        Rmin = Rmax * 10 ^ (-4)
    End Select
  End Sub
  Function RweSkal() As ArrayList
    Static iop As Integer = 0
    Static X As ArrayList
    If iop = 0 Then
      X = New ArrayList
      X.Clear()
      X.Add(1.0#)
      X.Add(2.0#)
      X.Add(4.0#)
      X.Add(5.0#)
      X.Add(6.0#)
      X.Add(7.5#)
      X.Add(8.0#)
      X.Add(10.0#)
      X.Add(15.0#)
      X.Add(20.0#)
      X.Add(40.0#)
      X.Add(50.0#)
      X.Add(60.0#)
      X.Add(75.0#)
      X.Add(80.0#)
      X.Add(100.0#)
      X.Add(120.0#)
      X.Add(160.0#)
      X.Add(200.0#)
      X.Add(250.0#)
      X.Add(500.0#)
      X.Add(1000.0#)
      iop = 1
    End If
    RweSkal = X
  End Function '
  WriteOnly Property GraphBounds() As RectangleF
    Set(ByVal Value As RectangleF)
      MnGraphBounds = Value
    End Set
  End Property '

  Public Sub New()
    BrushZ = New SolidBrush(Color.Black)

  End Sub
End Class
