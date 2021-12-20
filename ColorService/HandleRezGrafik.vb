Option Compare Text
Option Strict Off
Option Explicit On 


Public Class HandleRezGrafik

  Implements IDisposable
  '
  '
  '
  '
  '
  'Deklarationen
  '
  '
  '
  Dim disposed As Boolean
  Dim Mnipl As Short
  Dim Mnistr As Short
  Dim WeiSch As Short
  Dim MnWeSc(1) As Boolean
  Dim MnKwopt(8) As Boolean
  Dim MnWSOpt As Short
  Dim MnRmax As Single
  Dim MnRmin As Single
  Dim MnColFarb As Color
  Dim MnIer As Short
  Dim MnKsrt As New List(Of Integer)
  Dim MnVkw(1) As Short
  Dim MnKnlz As Short
  Dim MnKwop As Short
  Dim MnIbuch As Short
  Dim MnPenObj As New Pen(Color.Black, 1)
  Dim MnTextKDS As TextBox
  Dim MnRzName() As String
  Dim MnWinkel As AngGeos
  Dim MnOptkonst As OpticalData
  Dim MnDataChanged As Boolean
  Dim MnGrpRwerte As RefValuesGrp 'R-Werte aus rufendem Programm
  Dim MnPlotRwerte As RefValues   'R-Werte zum Plotten
  Dim MnFawrtRwerte As RefValuesGrp   'R-Werte für Ausgabe + Berechnung FarbWerte
  Dim MnAllRezepte As RecipesGrp   'Rezepte (rein)mit Zusatzinformationen
  Dim GraHilf As GraphicHilfProgramme
  Dim GrdPointF() As PointF
  Dim GrdWert() As Single
  Dim RefPointF() As PointF
  Dim RefWert() As Single
  '
  '
  '
  '
  '
  Dim MnKFIT As Integer
  Dim quali As QualKontrolle
  Dim MnFarbWerte As ValuesGrpsAssigns

  '
  '
  '
  Dim i As Short
  Dim tiny As Single
  Dim MnVKwb(1) As Short
  Dim Size As SizeF
  Dim ParLbez() As String
  Dim MnText As String
  Dim FaID As Integer
  Dim KeyD As String
  Dim MncboSKAL As ComboBox
  Dim Kunamen() As String
  Dim Fontz As Font
  Dim BrushZ As SolidBrush
  Dim MnGraphBounds As RectangleF
 

  '
  Public Sub New()
    MnColFarb = Color.Ivory

    disposed = False
    quali = New QualKontrolle
    MnPlotRwerte = New RefValues              'R-Werte zum Plotten
    MnFawrtRwerte = New RefValuesGrp           'R-Werte für Ausgabe + Berechnung FarbWerte
    MnFawrtRwerte.Add(KeyRe(0), New RefValues)
    MnFawrtRwerte.Add(KeyRe(1), New RefValues)
    MnFarbWerte = New ValuesGrpsAssigns
    Mnipl = 0
    Mnistr = 0
    MnKFIT = 1
    tiny = 1.0E-30
    MnWeSc(0) = False
    MnWeSc(1) = False
    MnWSOpt = 1
    MnRmax = -1
    MnRmin = -1
    MnKwop = -1
    Fontz = RezFont(2)
    BrushZ = New SolidBrush(Color.Black)
    GraHilf = New GraphicHilfProgramme
    MnDataChanged = False
  End Sub


  Protected Overrides Sub Finalize()

    MyBase.Finalize()
    dispose()

  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If disposed Then Exit Sub
    Try
      quali = Nothing
      MnPlotRwerte = Nothing
      MnFawrtRwerte = Nothing
      MnGrpRwerte = Nothing
      MnFarbWerte = Nothing

      MnPenObj.Dispose()
      BrushZ.Dispose()
      MnKsrt = Nothing
    Catch ex As Exception

    End Try
    disposed = True

  End Sub
  '
  '
  Function GetRefWert(RefLoc As Point) As Single
    Dim i As Integer
    Dim Value As Single
    Dim AbsPointFMin As Single
    Dim AbstPointF As Single
    GetRefWert = -1.0
    AbsPointFMin = 1.0E+30
    If IsNothing(RefPointF) Then Exit Function
    For i = 0 To RefPointF.Count - 1
      AbstPointF = Sqrt((RefPointF(i).X - CSng(RefLoc.X)) ^ 2 + (RefPointF(i).Y - CSng(RefLoc.Y)) ^ 2)
      If AbsPointFMin > AbstPointF Then
        AbsPointFMin = AbstPointF
        Value = RefWert(i)
      End If
    Next i
    If AbsPointFMin < 10 Then
      GetRefWert = Value
    End If
  End Function
  Function GetGrdWert(GrdLoc As Point) As Single
    Dim i As Integer
    Dim Value As Single
    Dim AbsPointFMin As Single
    Dim AbstPointF As Single
    GetGrdWert = -1.0
    If IsNothing(GrdPointF) Then Exit Function
    AbsPointFMin = 1.0E+30
    For i = 0 To GrdPointF.Count - 1
      AbstPointF = Sqrt((GrdPointF(i).X - CSng(GrdLoc.X)) ^ 2 + (GrdPointF(i).Y - CSng(GrdLoc.Y)) ^ 2)
      If AbsPointFMin > AbstPointF Then
        AbsPointFMin = AbstPointF
        Value = GrdWert(i)
      End If
    Next i
    If AbsPointFMin < 10 Then
      GetGrdWert = Value
    End If
  End Function


  '
  'Properties
  '
  '
  '
  '
  '
  '
  '
  Property cboSKAL() As ComboBox
    Get
      cboSKAL = MncboSKAL
    End Get
    Set(ByVal Value As ComboBox)
      Dim i As Integer
      MncboSKAL = Value
      '
      '
      'Skalierungswerte für R-Kurven
      '
      '
      MncboSKAL.Items.Clear()
      For i = 0 To GraHilf.RweSkal.Count - 1
        MncboSKAL.Items.Add(GraHilf.RweSkal(i))
      Next
      MncboSKAL.Text = 100.0
      '
    End Set
  End Property
  '
  '
  '
  '


  WriteOnly Property GraphBounds() As RectangleF
    Set(ByVal Value As RectangleF)
      MnGraphBounds = Value
      GraHilf.GraphBounds = Value
    End Set
  End Property


  '
  '
  '

  '

  '
  Property DataChanged() As Boolean
    Get
      DataChanged = MnDataChanged
    End Get
    Set(ByVal Value As Boolean)
      MnDataChanged = Value
    End Set
  End Property
  '
  '
  '
  '
  '
  'Nummer Lichtart für Ausgabe
  '
  '
  Property Knlz() As Short
    Get
      Knlz = MnKnlz
    End Get
    Set(ByVal AcNlz As Short)
      MnKnlz = AcNlz
    End Set
  End Property
  '
  '
  '
  'Anzahl Winkel (Messgeometrien)
  '
  '
  '
  '
  Property Kwop() As Short
    Get
      Kwop = MnKwop
    End Get
    Set(ByVal AcKwop As Short)
      MnKwop = AcKwop
    End Set
  End Property
  '
  '
  Property AllRezepte() As RecipesGrp
    Get
      AllRezepte = MnAllRezepte
    End Get
    Set(ByVal AcAllRezepte As RecipesGrp)
      MnAllRezepte = AcAllRezepte
    End Set
  End Property

  '
  Property Winkel() As AngGeos
    Get
      Winkel = MnWinkel
    End Get
    Set(ByVal AcWinkel As AngGeos)
      Dim i As Integer
      MnWinkel = AcWinkel
      For i = 0 To MnKwopt.Count - 1
        MnKwopt(i) = False
      Next i
    End Set
  End Property
  '
  '
  '
  ReadOnly Property PlotRwerte() As RefValues
    Get     'R-Werte für Kurven
      PlotRwerte = MnPlotRwerte
    End Get
  End Property
  '
  '
  '
  ReadOnly Property FawrtRwerte() As RefValuesGrp
    Get     'R-Werte für Berechnung (müssen in MnFawrtRwerte enthalten sein)
      FawrtRwerte = MnFawrtRwerte
    End Get
  End Property
  '
  '
  '
  ReadOnly Property Farbwerte() As ValuesGrpsAssigns
    Get     'R-Werte für Berechnung (müssen in MnFawrtRwerte enthalten sein)
      Farbwerte = MnFarbWerte
    End Get
  End Property
  '
  '
  '
  Property GrpRwerte() As RefValuesGrp
    Get
      GrpRwerte = MnGrpRwerte
    End Get
    Set(ByVal AcGrpRwerte As RefValuesGrp)
      MnGrpRwerte = AcGrpRwerte
    End Set
  End Property
  '
  '
  Property OptKonst() As OpticalData
    Get
      OptKonst = MnOptkonst
    End Get
    Set(ByVal AcOptKonst As OpticalData)
      MnOptkonst = AcOptKonst
    End Set
  End Property
  Property ColFarb() As Color
    Get
      ColFarb = MnColFarb
    End Get
    Set(ByVal AcColFarb As Color)
      MnColFarb = AcColFarb
    End Set
  End Property
  '
  '
  ReadOnly Property kwopt() As Boolean()
    Get
      kwopt = MnKwopt
    End Get
  End Property
  ReadOnly Property Vkwb() As Short()
    Get
      Vkwb = MnVKwb
    End Get
  End Property
  '
  '
  '
  '
  'Reihenfoge Weiß-Schwarz
  '
  '
  '
  Property ipl As Short
    'ipl=0 linear
    'ipl=1 logariothmisch
    'ipl=2 Arsinh
    Get
      ipl = Mnipl
    End Get
    Set(value As Short)
      Mnipl = value
    End Set
  End Property
  Property istr As Short
    '
    'istr=0 Farbwechsel für Kurven; Strichwechsel für Winkel
    'istr=1 Farbwechsel für Winkel; Strichwechsel für Kurven
    Get
      istr = Mnistr
    End Get
    Set(value As Short)
      Mnistr = value
    End Set
  End Property
  ReadOnly Property WeSc() As Boolean()
    Get
      WeSc = MnWeSc
    End Get
  End Property
  Property WSOpt() As Short
    Get
      WSOpt = MnWSOpt
    End Get
    Set(ByVal AcWSOpt As Short)
      MnWSOpt = AcWSOpt
    End Set
  End Property
  '
  '
  '
  'Sortierreihenfolge
  Property Ksrt() As List(Of Integer)
    Get
      Ksrt = MnKsrt
    End Get
    Set(ByVal Value As List(Of Integer))
      MnKsrt = Value
    End Set
  End Property

  Property TextKDS() As TextBox
    Get
      TextKDS = MnTextKDS
    End Get
    Set(value As TextBox)
      MnTextKDS = value
    End Set
  End Property

  Property ier() As Short
    Get
      ier = MnIer
    End Get
    Set(ByVal AcIer As Short)
      MnIer = AcIer
    End Set
  End Property
  Property Text() As String
    Get
      Text = MnText
    End Get
    Set(ByVal AcText As String)
      MnText = AcText
    End Set
  End Property

  Property Rmax() As Single
    Get
      Rmax = MnRmax
    End Get
    Set(ByVal AcRmax As Single)
      MnRmax = AcRmax
    End Set
  End Property
  Property Rmin() As Single
    Get
      Rmin = MnRmin
    End Get
    Set(ByVal AcRmin As Single)
      MnRmin = AcRmin
    End Set
  End Property

  Property KFIT() As Integer
    Get
      KFIT = MnKFIT
    End Get
    Set(ByVal Value As Integer)
      MnKFIT = Value
    End Set
  End Property '
  '
  'Rezeptkennung
  '
  '
  '
  '
  '
  Property RzName(ByVal i As Short) As String
    Get
      RzName = MnRzName(i)
    End Get
    Set(ByVal AcRzName As String)
      ReDim Preserve MnRzName(i)
      MnRzName(i) = AcRzName
    End Set
  End Property
  '
  '
  '
  '
  '
  '
  '
  'Subroutinen + Functions
  '
  '

  Sub picWRTGraph(ByVal sender As Object, ByVal Graph As Graphics)
    '
    '
    '
    '
    'Werte für FarbWerte
    '
    '
    '
    '
    '
    Dim i As Integer
    Dim l As Integer
    Dim X0 As Single
    Dim Y0 As Single
    Dim ddy As Single
    Dim HeadName As String
    Dim Texte() As String
    Dim Forwidth As Single
    Dim Forrm As String
    Dim FormStd As String
    Dim LogPrint As Boolean
    Dim ZwiText As String
    Dim ForHeight As Single
    If MnFarbWerte.Count = 0 Then Exit Sub
    If IsNothing(MnFarbWerte(WeiSch)) Then Exit Sub
    If MnFawrtRwerte(WeiSch).Count = 0 Then Exit Sub
    Dim PenNew As New Pen(MnPenObj.Color, MnPenObj.Width)
    ddy = 0.9 * RezFont(2).Height
    FormStd = "###.000"
    Forwidth = Graph.MeasureString(FormStd, RezFont(2)).Width
    ForHeight = Graph.MeasureString(FormStd, RezFont(2)).Height
    X0 = MnGraphBounds.Left + 0.5 * (MnGraphBounds.Width - Graph.MeasureString(Texxt(858).Trim, RezFont(2)).Width)

    Y0 = MnGraphBounds.Top + ForHeight
    Graph.DrawString(Texxt(858).Trim, RezFont(2), BrushZ, X0, Y0)
    ZwiText = Trim(MenueParam.Normfa(MnKnlz).NormNama) & "(" & MnWinkel(MnKwop).Chrm & ")"
    If MenueParam.Misch.Transp Then
      ZwiText = ZwiText & Mid(MnFarbWerte(WeiSch).AufgArt, 4, 1)
    End If
    X0 = MnGraphBounds.Left + 0.5 * (MnGraphBounds.Width - Graph.MeasureString(ZwiText, RezFont(2)).Width)

    Y0 = Y0 + ForHeight
    Graph.DrawString(ZwiText, RezFont(2), BrushZ, X0, Y0)

    X0 = MnGraphBounds.Left + 0.1 * MnGraphBounds.Width
    Y0 = Y0 + ForHeight
    Graph.DrawString(Texxt(861), RezFont(2), BrushZ, X0, Y0)

    X0 = MnGraphBounds.Left + 0.4 * MnGraphBounds.Width

    If MnFawrtRwerte(0).Count < 2 OrElse IsNothing(MnFawrtRwerte(WeiSch)(1)) Then Exit Sub
    HeadName = MnFawrtRwerte(WeiSch)(1).Name
    If HeadName.Length > 16 Then
      HeadName = HeadName.Substring(0, 16)
    End If
    Graph.DrawString(HeadName, RezFont(2), BrushZ, X0, Y0)
    X0 = X0 + 0.34 * MnGraphBounds.Width
    If MnFarbWerte(WeiSch).Count = 3 Then
      HeadName = MnFawrtRwerte(WeiSch)(2).Name
      If HeadName.Length > 16 Then
        HeadName = HeadName.Substring(0, 16)
      End If
      Graph.DrawString(HeadName, RezFont(2), BrushZ, X0, Y0)
    End If
    'If MnFarbWerte(WeiSch)(0).Count = 2 Then
    ' Graph.DrawString(Texxt(860), rezfont(2), brushz, X0, Y0)
    ' ElseIf MnFarbWerte(WeiSch)(0).Count = 3 Then
    ' Graph.DrawString(Texxt(860), rezfont(2),  brushz, X0, Y0)
    ' End If
    X0 = MnGraphBounds.Left + 0.0# * MnGraphBounds.Width
    Y0 = Y0 + ForHeight
    PenNew.Width = 1
    Graph.DrawLine(PenNew, X0, Y0, X0 + MnGraphBounds.Width, Y0)
    Y0 = Y0 + ForHeight
    X0 = MnGraphBounds.Left + 0.02 * MnGraphBounds.Width
    ReDim Texte(MnFarbWerte(WeiSch).Count - 1)
    For l = 0 To MnFarbWerte(WeiSch).CountMerk - 1
      If MnFarbWerte(WeiSch).Merk(l).ID <> 0 Then
        LogPrint = False
        For i = 0 To UBound(Texte)

          If MnFarbWerte(WeiSch).Merk(l).Ken = "BT" Then
            ' MsgBox("  ")
          End If

          Texte(i) = ""
          If MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop).Count > 0 Then
            If Not Trim(MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop)(l)) = "" AndAlso Not (IsDBNull(MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop)(l)) OrElse Double.IsNaN(CDbl(MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop)(l)))) Then
              If CSng(MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop)(l)) < MnFarbWerte.HoleNum Then
                Forrm = MnFarbWerte(WeiSch).Merk(l).Form
                If Trim(Forrm) <> "" Then
                  If Graph.MeasureString(Forrm, RezFont(2)).Width > Forwidth Then
                    Forrm = FormStd
                  End If
                  Texte(i) = Format(CSng(MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop)(l)), Forrm)
                  LogPrint = True
                End If
              End If
            End If
          End If
        Next i
        If LogPrint And Y0 + ddy < MnGraphBounds.Top + MnGraphBounds.Height Then
          Call PicLinPrt(X0, Y0, Forwidth, Texte, "", MnFarbWerte(WeiSch).Merk(l).Kbez, Graph)
          Y0 = Y0 + ddy
        End If
      End If
    Next l
    '
    PenNew.Dispose()
  End Sub

  Sub PicLinPrt(ByRef x00 As Single, ByRef y00 As Single, ByRef Forwidth As Single, ByRef Text() As String, ByRef tesym As String, ByRef teUeb As String, ByVal Graph As Graphics)

    Dim i As Short
    Dim X0 As Single
    Dim Y0 As Single
    Dim Texte As String
    X0 = x00
    Y0 = y00
    '
    '
    '

    Graph.DrawString(tesym, RezFont(2), BrushZ, X0, Y0)
    X0 = x00 + Graph.MeasureString(tesym, RezFont(2)).Width
    Graph.DrawString(teUeb, RezFont(2), BrushZ, X0, Y0)

    For i = 0 To UBound(Text)
      Texte = Text(i)
      X0 = MnGraphBounds.Left + (0.12 + i * 0.34) * MnGraphBounds.Width + Forwidth - Graph.MeasureString(Texte, RezFont(2)).Width
      Graph.DrawString(Texte, RezFont(2), BrushZ, X0, Y0)
    Next i

  End Sub
  Sub PicXYZGraph(ByVal sender As Object, ByVal Graph As Graphics)
    Dim nk As Integer
    Dim XYZ(2) As Single
    Dim RoGrBl(2) As Integer
    Dim Ihlf As Long
    Dim Hoehe As Single
    Dim Hobre As Single
    Dim i As Integer
    Dim BruNew As SolidBrush
    If MnFarbWerte.Count = 0 Then Exit Sub
    If IsNothing(MnFarbWerte(WeiSch)) Then Exit Sub
    nk = MnFarbWerte(WeiSch).Count
    If nk = 0 Then
      Exit Sub
    End If
    '
    'Farbdarstellung in Picture-Box; XYZV=Tristimulus-Werte der Vorlage
    'X,Y,Z Tristimuluswerte der Nachstellung
    '

    '

    Hobre = MnGraphBounds.Width + MnGraphBounds.Left
    If nk > 1 Then
      Hobre = 0.5 * Hobre
    End If
    For i = 0 To nk - 1
      If Not MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop).ContainsKey("AA") Then Exit Sub
    Next i
    XYZ(0) = CSng(MnFarbWerte(WeiSch)(0)(MnKnlz)(MnKwop)("AA"))
    XYZ(1) = CSng(MnFarbWerte(WeiSch)(0)(MnKnlz)(MnKwop)("AB"))
    XYZ(2) = CSng(MnFarbWerte(WeiSch)(0)(MnKnlz)(MnKwop)("AC"))
    Call FarbDar(RoGrBl, XYZ, MenueParam.Normfa(MnKnlz).NormFakt)
    Ihlf = RoGrBl(0) + 256& * (RoGrBl(1) + 256& * RoGrBl(2))
    BruNew = New SolidBrush(Color.FromArgb(RoGrBl(0), RoGrBl(1), RoGrBl(2)))
    Graph.FillRectangle(BruNew, MnGraphBounds.Left, MnGraphBounds.Top, Hobre, MnGraphBounds.Height)
    BruNew.Dispose()
    If nk = 1 Then
      Exit Sub
    End If
    Hoehe = MnGraphBounds.Height / (nk - 1)
    For i = 1 To nk - 1
      XYZ(0) = CSng(MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop)("AA"))
      XYZ(1) = CSng(MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop)("AB"))
      XYZ(2) = CSng(MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop)("AC"))
      Call FarbDar(RoGrBl, XYZ, MenueParam.Normfa(MnKnlz).NormFakt)
      Ihlf = RoGrBl(0) + 256& * (RoGrBl(1) + 256& * RoGrBl(2))
      BruNew = New SolidBrush(Color.FromArgb(RoGrBl(0), RoGrBl(1), RoGrBl(2)))
      Graph.FillRectangle(BruNew, Hobre, (i - 1) * Hoehe + MnGraphBounds.Top, Hobre, Hoehe)
      BruNew.Dispose()
    Next i
  End Sub
  '
  '
  Sub PicGRDGraph(ByVal sender As Object, ByVal Graph As Graphics)
    '
    '
    '
    '
    'Grafische Darstellung der R.Kurven
    '
    '
    '
    '
    Call GrundPlot(2, MnRmin, MnRmax, MnKwopt, MnWinkel, MnText, OptKonst, Graph, GrdPointF, GrdWert, ier)

  End Sub
  Sub PicREFGraph(ByVal sender As Object, ByVal Graph As Graphics)
   
    '
    Erase RefPointF
    Erase RefWert
    '
    '
    Call ReflPlot(Mnipl, MnRmin, MnRmax, MnWeSc, MnKwopt, MnWinkel, MnText, MnPlotRwerte, Mnistr, Graph, RefPointF, RefWert)

  End Sub
  '
  Sub ReflPlot(ByRef Ipl As Short, ByRef Rmii As Single, ByRef Rmaa As Single, ByRef WeSc() As Boolean, ByRef kwopt() As Boolean, ByRef Winkel As AngGeos, ByRef Text As String, ByVal ReflRwert As RefValues, ByRef istr As Short, ByRef graph As Graphics, ByRef RefPointF() As PointF, ByRef RefWert() As Single)
    Dim Fakt As Single
    Dim CHRM As String
    Dim i As Short
    Dim j As Short
    Dim kw As Short
    Dim Rmi As Single
    Dim Rma As Single
    Dim Kurvs As CurvesRefGrp
    If ReflRwert Is Nothing OrElse ReflRwert.Count = 0 Then Exit Sub
    Kurvs = New CurvesRefGrp
    Kurvs.clear()
    If kwopt.Length = 0 Then Exit Sub
    ReDim Kunamen(ReflRwert.Count - 1)
    i = 0
    '
    'Grafische Darstellung der R.Kurven
    '
    '
    '
    For j = 0 To ReflRwert.Count - 1
      If ReflRwert(j).Iplott Then
        If (ReflRwert(j).kwb = MnVKwb(0) And MnWeSc(0)) OrElse (ReflRwert(j).kwb = MnVKwb(1) And MnWeSc(1)) Then
          Kurvs.Add(KeyName(i), New CurvesRef)
          Kunamen(i) = Mid(ReflRwert(j).Name, 1, 25)
          For kw = 0 To Winkel.Km - 1
            If kwopt(kw) Then
              CHRM = Winkel(kw).Chrm
              Kurvs(i).Add(CHRM, ReflRwert(j).RefKurv(CHRM))
            End If
          Next kw
          i = i + 1
        End If
      End If
    Next j
    If Kurvs.Count = 0 Then
      Exit Sub
    End If
    Fakt = 1.0#
    If Ipl = 0 Then
      Fakt = 100.0#
    End If
    Rmi = Rmii / Fakt
    Rma = Rmaa / Fakt
    Call GraHilf.RefPaant(Text, Kunamen, Rmi, Rma, Winkel, Kurvs, Fakt, Ipl, istr, graph, RefPointF, RefWert, ier)
    Rmii = Fakt * Rmi
    Rmaa = Fakt * Rma
    Application.DoEvents()
    Kurvs = Nothing
  End Sub
  Sub GrundPlot(ByVal Ipl As Integer, ByVal Rmii As Single, ByVal Rmaa As Single, ByVal kwopt() As Boolean, ByVal Winkel As AngGeos, ByVal Text As String, ByVal OptKonst As OpticalData, ByRef graph As Graphics, ByRef RefPointF() As PointF, ByRef RefWert() As Single, ByVal ier As Integer)
    Dim Fakt As Single
    Dim CHRM As String
    Dim i As Integer
    Dim kw As Integer
    Dim Nwe As Integer
    Nwe = Winkel.Wsol.Nwe
    Dim Kurvs As CurvesRefGrp
    Kurvs = New CurvesRefGrp
    If IsNothing(OptKonst) Then Exit Sub
    If OptKonst.Grund.Count = 0 Then Exit Sub
    If kwopt.Length = 0 Then Exit Sub
    Kurvs.clear()
    ReDim Kunamen(OptKonst.Grund.Count - 1)
    For i = 0 To OptKonst.Grund.Count - 1
      Kurvs.Add(KeyRe(i), New CurvesRef)
      Kunamen(i) = OptKonst.Grund(i)(0).NamPlott
      For kw = 0 To Winkel.Km - 1
        If kwopt(kw) Then
          CHRM = Winkel(kw).Chrm
          Kurvs(i).Add(CHRM, OptKonst.Grund(i)(CHRM))
        End If
      Next kw
    Next i
    If Kurvs.Count = 0 Then
      Exit Sub
    End If
    Fakt = 1.0#
    If Ipl = 0 Then
      Fakt = 100.0#
    End If
    Call GraHilf.RefPaant(Text, Kunamen, Rmii, Rmaa, Winkel, Kurvs, Fakt, Ipl, 0, graph, RefPointF, RefWert, ier)
    Application.DoEvents()
    Kurvs.dispose()
  End Sub


  '
  Sub PicLABGraph(ByVal sender As Object, ByVal Graph As Graphics)
    '
    '
    '
    '
    'Grafische Darstellung von L*,a*.b*/L9,a9,b9
    '
    '
    '
    '
    '
    Dim TexWidth As Single
    Dim Beschrift As String
    Dim nk As Short
    Dim i As Short
    Dim ii As Short
    Dim l As Short
    Dim Rnr As Short
    Dim dl As Single
    Dim da As Single
    Dim db As Single
    Dim dc As Single
    Dim Ast As Single
    Dim Bst As Single
    Dim dmm As Single
    Dim dgm(9) As Single
    Dim Xbr As Single
    Dim pi As Single
    Dim pi2 As Single
    Dim dx As Single
    Dim dy As Single
    Dim dll As Single
    Dim xc As Single
    Dim yc As Single
    Dim xxc As Single
    Dim yyc As Single
    Dim xx As Single
    Dim yy As Single
    Dim yyy As Single
    Dim x1 As Single
    Dim y1 As Single
    Dim y2 As Single
    Dim ddx As Single
    Dim ddy As Single
    Dim dxp As Single
    Dim rad As Single
    Dim ng As Short
    Dim verh As Single
    Dim pha As Single
    Dim Phi As Single
    Dim ph(4) As Single
    Dim dph As Single
    Dim ip As Short
    Dim h1 As Single
    Dim h2 As Single
    Dim r As Single
    Dim hi As Single
    Dim akon As Single
    Dim dd As Single
    Dim dcc As Single
    Dim RechtSei As Single
    Dim Merkken As String

    Dim PenNew As New Pen(MnPenObj.Color, MnPenObj.Width)
    Dim BruNew As SolidBrush
    '
    pi = 3.141593
    pi2 = 2.0# * pi
    dgm(0) = 0.1
    dgm(1) = 0.25
    dgm(2) = 0.5
    dgm(3) = 1.0#
    dgm(4) = 2.5
    dgm(5) = 5.0#
    dgm(6) = 10.0#
    dgm(7) = 20.0#
    dgm(8) = 50.0#
    dgm(9) = 100.0#
    verh = MnGraphBounds.Height / MnGraphBounds.Width
    If MnFarbWerte.Count = 0 Then Exit Sub
    If IsNothing(MnFarbWerte(WeiSch)) Then Exit Sub
    nk = MnFarbWerte(WeiSch).Count
    If nk = 0 Then Exit Sub
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '

    '
    '
    '
    '
    ' Maximum von dl bestimmen
    '
    '
    dx = 0.0#
    For i = 1 To nk - 1
      Merkken = MenueParam.Menue.StdMrkKen(16)
      If MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop).ContainsKey(Merkken) Then
        dll = Abs(CSng(MnFarbWerte(WeiSch)(i)(MnKnlz)(MnKwop)(Merkken)))
        If dll > dx Then dx = dll
      End If
    Next i
    dmm = dgm(9)
    For i = 0 To 9
      If dx <= dgm(i) Then
        dmm = dgm(i)
        Exit For
      End If
    Next i
    '
    '

    yc = MnGraphBounds.Top + 0.48 * MnGraphBounds.Height
    xc = MnGraphBounds.Left + 0.506 * MnGraphBounds.Width
    yy = 0.377 * MnGraphBounds.Height
    RechtSei = 0.04 * yy
    x1 = MnGraphBounds.Left + 0.15 * MnGraphBounds.Width
    y1 = yc - yy
    y2 = yc + yy
    ddx = 0.0125 * MnGraphBounds.Width
    ddy = RezFont(2).GetHeight(Graph)
    dxp = 7.5 * ddx

    Graph.DrawLine(MnPenObj, x1, y1, x1, y2)
    Graph.DrawLine(MnPenObj, x1 - ddx, y1, x1 + ddx, y1)

    Graph.DrawLine(MnPenObj, x1 - ddx, y2, x1 + ddx, y2)


    '
    'Label

    '

    Beschrift = Format(-dmm, "###0.0#;;;")
    TexWidth = Graph.MeasureString(Beschrift, RezFont(2)).Width + ddx
    Graph.DrawString(Beschrift, RezFont(2), BrushZ, x1 - TexWidth, y2 - 0.5 * ddy)
    Beschrift = Format(dmm, "###0.0#;;;")
    TexWidth = Graph.MeasureString(Beschrift, RezFont(2)).Width + ddx
    Graph.DrawString(Beschrift, RezFont(2), BrushZ, x1 - TexWidth, y1 - 0.5 * ddy)




    Graph.DrawLine(MnPenObj, x1 - ddx, yc, x1 + ddx, yc)

    '
    Beschrift = Format(0.0, "###0.0#;;;")
    TexWidth = Graph.MeasureString(Beschrift, RezFont(2)).Width + ddx
    Graph.DrawString(Beschrift, RezFont(2), BrushZ, x1 - TexWidth, yc - 0.5 * ddy)
    '
    '
    '
    '
    '
    '
    'Rechtecke
    '
    For i = 1 To nk - 1
      Merkken = MenueParam.Menue.StdMrkKen(16)
      If MnFarbWerte(WeiSch)(i)(Knlz)(Kwop).ContainsKey(Merkken) Then
        dl = CSng(MnFarbWerte(WeiSch)(i)(Knlz)(Kwop)(Merkken))
        yyy = yy * dl / dmm
        ii = i Mod 16
        BruNew = New SolidBrush(farbini(ii))
        Graph.FillRectangle(BruNew, x1 - 0.5F * RechtSei, yc - yyy - 0.5F * RechtSei, RechtSei, RechtSei)
        BruNew.Dispose()
        If MnIbuch = 1 Then
          Rnr = MnFarbWerte(WeiSch)(i).Nr
          If Rnr > 0 Then
            Graph.DrawString(CStr(96 + ((Rnr - 1) Mod 26) + 1), RezFont(2), BrushZ, x1 - 0.5 * ddx, yc + yyy + 1.5 * ddy)
          End If
        End If
      End If
    Next i
    '
    dx = 0.0#
    For i = 1 To nk - 1
      Merkken = MenueParam.Menue.StdMrkKen(19)
      If MnFarbWerte(WeiSch)(i)(Knlz)(Kwop).ContainsKey(Merkken) Then
        da = CSng(MnFarbWerte(WeiSch)(i)(Knlz)(Kwop)(Merkken))
        Merkken = MenueParam.Menue.StdMrkKen(20)
        db = CSng(MnFarbWerte(WeiSch)(i)(Knlz)(Kwop)(Merkken))
        dy = Sqrt(da ^ 2 + db ^ 2)
        If dy > dx Then dx = dy
      End If
    Next i
    ng = 9
    For i = 0 To 9
      If dx <= dgm(i) Then
        ng = i
        Exit For
      End If
    Next i
    rad = yy
    Graph.DrawLine(MnPenObj, xc - rad, yc, xc + rad, yc)
    Graph.DrawLine(MnPenObj, xc, yc + rad, xc, yc - rad)
    Graph.DrawEllipse(MnPenObj, xc - rad, yc - rad, 2 * rad, 2 * rad)
    '

    'Beschriftung Delta b*/b9
    '
    '

    Xbr = Graph.MeasureString("D", RezFont(1)).Width
    Graph.DrawString("D", RezFont(1), BrushZ, xc - Xbr, yc - rad - 2.0 * ddy)
    Merkken = MnFarbWerte(WeiSch).Merk(MenueParam.Menue.StdMrkKen(13)).Kbez

    Graph.DrawString(Merkken, RezFont(2), BrushZ, xc - 0.25 * Xbr, yc - rad - 2.0 * ddy)
    Graph.DrawString(Format(dgm(ng), "###0.0#;;;"), RezFont(2), BrushZ, xc - Xbr, yc - rad - ddy)
    '
    '
    'Beschriftung Delta a*/a9
    '
    '
    '
    '
    Xbr = Graph.MeasureString("D", RezFont(1)).Width
    Graph.DrawString("D", RezFont(1), BrushZ, xc + rad, yc - ddy)

    Merkken = MnFarbWerte(WeiSch).Merk(MenueParam.Menue.StdMrkKen(12)).Kbez

    Graph.DrawString(Merkken, RezFont(2), BrushZ, xc + rad + 0.75 * Xbr, yc - ddy)
    Graph.DrawString(Format(dgm(ng), "###0.0#;;;"), RezFont(2), BrushZ, xc + rad, yc)
    '
    '
    '
    'Winkel des Chroma-Strahles
    '
    Merkken = MenueParam.Menue.StdMrkKen(12)
    Ast = CSng(MnFarbWerte(WeiSch)(0)(Knlz)(Kwop)(Merkken))
    Merkken = MenueParam.Menue.StdMrkKen(13)
    Bst = CSng(MnFarbWerte(WeiSch)(0)(Knlz)(Kwop)(Merkken))
    pha = Atan2(Bst, Ast)
    '

    For i = 1 To nk - 1
      Merkken = MenueParam.Menue.StdMrkKen(19)
      If MnFarbWerte(WeiSch)(i)(Knlz)(Kwop).ContainsKey(Merkken) Then
        da = CSng(MnFarbWerte(WeiSch)(i)(Knlz)(Kwop)(Merkken))
      End If
      Merkken = MenueParam.Menue.StdMrkKen(20)
      If MnFarbWerte(WeiSch)(i)(Knlz)(Kwop).ContainsKey(Merkken) Then
        db = CSng(MnFarbWerte(WeiSch)(i)(Knlz)(Kwop)(Merkken))
      End If
      Merkken = MenueParam.Menue.StdMrkKen(17)
      If MnFarbWerte(WeiSch)(i)(Knlz)(Kwop).ContainsKey(Merkken) Then
        dc = CSng(MnFarbWerte(WeiSch)(i)(Knlz)(Kwop)(Merkken))
      End If
      ip = 0
      r = da ^ 2 + db ^ 2
      If r = 0.0# Then
        r = 1.0#
      End If
      hi = Sqrt(Abs(r - dc ^ 2))
      h1 = (dc * db + da * hi) / r
      h2 = (dc * db - da * hi) / r
      If Abs(h1) <= 1.0# Then
        ip = ip + 1
        ph(ip) = Asin(h1)
        ip = ip + 1
        ph(ip) = pi - ph(ip - 1)
      End If
      If Abs(h2) <= 1.0# Then
        ip = ip + 1
        ph(ip) = Asin(h2)
        ip = ip + 1
        ph(ip) = pi - ph(ip - 1)                '
      End If
      dph = 1.0E+30
      For l = 1 To ip
        akon = (da * Cos(ph(l)) + db * Sin(ph(l)) - dc)
        If Abs(akon) < 0.01 Then
          dd = Abs(ph(l) - pha)
          If dd < dph Then
            dph = dd
            Phi = ph(l)
          End If
        End If
      Next l
      xx = xc + da / dgm(ng) * rad
      yy = yc - db / dgm(ng) * rad
      '
      'Rechteck zeichnen
      '
      '
      ii = i Mod 16
      BruNew = New SolidBrush(farbini(ii))
      Graph.FillRectangle(BruNew, xx - 0.5F * RechtSei, yy - 0.5F * RechtSei, RechtSei, RechtSei)
      BruNew.Dispose()
      If MnIbuch = 1 Then
        Rnr = MnFarbWerte(WeiSch)(i).Nr
        If Rnr > 0 Then
          Graph.DrawString(CStr(96 + ((Rnr - 1) Mod 26) + 1), RezFont(2), BrushZ, xx - 0.0175 * rad, yy + 0.0525 * rad)
        End If
      End If
      '
      'Chroma und Hue zeichnen
      '

      dcc = dc / dgm(ng) * rad
      xxc = xc + dcc * Cos(Phi)
      yyc = yc - dcc * Sin(Phi)
      ii = i Mod 16
      PenNew.Color = farbini(ii)
      PenNew.Width = 1
      Graph.DrawLine(PenNew, xx, yy, xxc, yyc)

      Graph.DrawLine(PenNew, xc, yc, xxc, yyc)

    Next i
    '
    '
    'a*, b*/a9, b9 Diagramm
    '
    '
    xxc = xc + rad + 0.1 * rad
    yyc = yc - 0.925 * rad
    r = 0.33 * rad
    Graph.DrawLine(MnPenObj, xxc - r, yyc, xxc + r, yyc)
    Graph.DrawLine(MnPenObj, xxc, yyc + r, xxc, yyc - r)

    '
    'a*/a9-Vorlage
    'b*/b9-Vorlage
    xx = xxc + Ast * 0.01 * r - 0.05 * r
    yy = yyc - Bst * 0.01 * r - 0.05 * r
    Graph.DrawEllipse(MnPenObj, xx, yy, 0.1F * r, 0.1F * r)
    Merkken = MnFarbWerte(WeiSch).Merk(MenueParam.Menue.StdMrkKen(12)).Kbez
    Graph.DrawString(Merkken, RezFont(3), BrushZ, xxc + 0.95 * r, yyc)
    Merkken = MnFarbWerte(WeiSch).Merk(MenueParam.Menue.StdMrkKen(13)).Kbez
    Xbr = Graph.MeasureString(Merkken, RezFont(3)).Width
    Graph.DrawString(Merkken, RezFont(3), BrushZ, xxc - Xbr, yyc - r)

    '
    '
    '
    'Unterschrift
    '
    y2 = MnGraphBounds.Top + MnGraphBounds.Height - 2.5 * ddy
    Xbr = Graph.MeasureString("D", RezFont(1)).Width
    Graph.DrawString("D", RezFont(1), BrushZ, x1 - Xbr, y2)
    Merkken = MnFarbWerte(WeiSch).Merk(MenueParam.Menue.StdMrkKen(9)).Kbez
    Graph.DrawString(Merkken & Space(5), RezFont(2), BrushZ, x1 - 0.25 * Xbr, y2)

    '
    x1 = MnGraphBounds.Left + 0.5 * (MnGraphBounds.Width - Graph.MeasureString(Space(10) & Trim(Texxt(70)), RezFont(2)).Width)
    Graph.DrawString("D", RezFont(1), BrushZ, x1, y2)
    Xbr = Graph.MeasureString("D", RezFont(1)).Width
    Merkken = MnFarbWerte(WeiSch).Merk(MenueParam.Menue.StdMrkKen(12)).Kbez

    Graph.DrawString(Merkken, RezFont(2), BrushZ, x1 + 0.75 * Xbr, y2)
    Xbr = 0.75 * Xbr + Graph.MeasureString(Merkken, RezFont(2)).Width

    Graph.DrawString("D", RezFont(1), BrushZ, x1 + Xbr, y2)
    Xbr = Xbr + 0.75 * Graph.MeasureString("D", RezFont(1)).Width
    Merkken = MnFarbWerte(WeiSch).Merk(MenueParam.Menue.StdMrkKen(13)).Kbez
    Graph.DrawString(Merkken & " - " & Texxt(70), RezFont(2), BrushZ, x1 + Xbr, y2)
    x1 = MnGraphBounds.Left + 0.5 * (MnGraphBounds.Width - Graph.MeasureString(Trim(Text), RezFont(2)).Width)
    y2 = y2 + 1.0 * ddy
    Graph.DrawString(Text, RezFont(2), BrushZ, x1, y2)



    PenNew.Dispose()

  End Sub
  Sub PicREZGraph(ByVal sender As Object, ByVal Graph As Graphics)
    '
    '
    '
    '
    'Werte Für Farbrezept
    '
    '
    '
    '
    Dim k As Short
    Dim i As Integer
    Dim X0 As Single
    Dim Y0 As Single
    Dim dy As Single
    Dim ddy As Single
    Dim x1 As Single
    Dim x2 As Single
    Dim Texte As String
    Dim Ftext As String
    Dim Forwidth As Single
    Dim sum(1) As Single
    Dim Suf(1) As Single
    Dim Preis(1) As Single
    Dim Fak(1) As Single
    Dim Epsi As Single
    Dim FaDel As Single = 0.001
    If IsNothing(MnRzName) Then Exit Sub
    If Not MnAllRezepte.Rezepte.ContainsKey(MnRzName(0)) Then Exit Sub
    Dim FontNew As New Font(RezFont(2).Name, RezFont(2).Size, FontStyle.Regular)
    Dim PenNew As New Pen(MnPenObj.Color, MnPenObj.Width)
    Epsi = 0.0000000001
    ddy = RezFont(2).GetHeight(Graph)
    '
    ier = 0

    X0 = MnGraphBounds.Left + 0.05 * MnGraphBounds.Width
    Y0 = MnGraphBounds.Top + 0.08 * MnGraphBounds.Height
    Graph.DrawString(Texxt(862).Trim, RezFont(2), BrushZ, X0, Y0)
    FontNew = New Font(RezFont(2).Name, RezFont(2).Size, FontStyle.Regular)
    PenNew.Width = 1
    X0 = MnGraphBounds.Left + 0.05 * MnGraphBounds.Width
    Y0 = MnGraphBounds.Top + 0.15 * MnGraphBounds.Height
    If UBound(MnRzName) > 1 Then
      ier = 11
      Exit Sub
    End If
    If UBound(MnRzName) = 1 Then
      If MnAllRezepte.Rezepte(MnRzName(0)).KF <> MnAllRezepte.Rezepte(MnRzName(1)).KF Then
        ier = 12
        Exit Sub
      End If
    End If
    sum(0) = 0.0#
    sum(1) = 0.0#
    Preis(0) = 0.0
    Preis(1) = 0.0
    Forwidth = 0.0#
    For k = 0 To MnAllRezepte.Rezepte(MnRzName(0)).KF - 1
      FaID = MnAllRezepte.Rezepte(MnRzName(0))(k).ID
      If UBound(MnRzName) = 1 Then
        If FaID <> MnAllRezepte.Rezepte(MnRzName(0))(k).ID Then
          ier = 13
          Exit Sub
        End If
      End If
      KeyD = KeyName(FaID)
      If Graph.MeasureString(Trim(MnAllRezepte.Farben(KeyD).Form), FontNew).Width > Forwidth Then
        Forwidth = Graph.MeasureString(Trim(MnAllRezepte.Farben(KeyD).Form), FontNew).Width
      End If
      For i = 0 To UBound(MnRzName)
        If MnAllRezepte.Farben(KeyD).Ichf <> 1 Then
          '
          '
          'Farbmittelmengen ohne Bindemittel
          '
          Suf(i) = Suf(i) + MnAllRezepte.Rezepte(MnRzName(i))(k).FaAmng
        End If
        sum(i) = sum(i) + MnAllRezepte.Rezepte(MnRzName(i))(k).FaAmng
        Preis(i) = Preis(i) + MnAllRezepte.Farben(KeyD).Preis * MnAllRezepte.Rezepte(MnRzName(i))(k).FaAmng
      Next i
    Next k
    '
    '
    '
    '
    '
    Graph.DrawLine(PenNew, X0, Y0, X0 + Graph.MeasureString(Texxt(862).Trim, FontNew).Width, Y0)
    '
    '
    If UBound(MnRzName) = 1 Then
      Forwidth = 2.0# * Forwidth
      X0 = MnGraphBounds.Left + Forwidth
      Graph.DrawString(Texxt(880), FontNew, BrushZ, X0, Y0)
    End If

    Y0 = MnGraphBounds.Top + 0.2 * MnGraphBounds.Height
    X0 = MnGraphBounds.Left + 0.1 * MnGraphBounds.Width
    dy = 0.06 * MnGraphBounds.Height
    Fak(0) = 100 / (sum(0) + Epsi)
    Fak(1) = 100 / (sum(1) + Epsi)
    For k = 0 To MnAllRezepte.Rezepte(MnRzName(0)).KF - 1
      Texte = ""
      FaID = MnAllRezepte.Rezepte(MnRzName(0))(k).ID
      KeyD = KeyName(FaID)
      Select Case UBound(MnRzName)
        Case 0
          If MnAllRezepte.Rezepte(MnRzName(0))(k).FaAmng > FaDel Then
            Texte = Format(MnAllRezepte.Rezepte(MnRzName(0))(k).FaAmng * Fak(0), Trim(MnAllRezepte.Farben(KeyD).Form))
          End If
        Case 1
          If MnAllRezepte.Rezepte(MnRzName(0))(k).FaAmng > FaDel Or MnAllRezepte.Rezepte(MnRzName(1))(k).FaAmng > FaDel Then
            Texte = Format(MnAllRezepte.Rezepte(MnRzName(0))(k).FaAmng * Fak(0), Trim(MnAllRezepte.Farben(KeyD).Form))
            X0 = MnGraphBounds.Left + 0.1 * MnGraphBounds.Width + 0.5 * Forwidth - Graph.MeasureString(Texte, FontNew).Width()
            Graph.DrawString(Texte, FontNew, BrushZ, X0, Y0)
            Texte = Format(MnAllRezepte.Rezepte(MnRzName(1))(k).FaAmng * Fak(1), Trim(MnAllRezepte.Farben(KeyD).Form))
          End If
      End Select
      If Texte <> "" Then

        X0 = MnGraphBounds.Left + 0.1 * MnGraphBounds.Width + Forwidth - Graph.MeasureString(Texte, FontNew).Width()

        If Not BitWrt(25, MenueParam.User.Drum) Then
          Texte = Texte & "          " & MnAllRezepte.Farben(KeyD).Name
        Else
          Texte = Texte & "          " & MnAllRezepte.Farben(KeyD).Aname
        End If
        Graph.DrawString(Texte, FontNew, BrushZ, X0, Y0)

        Y0 = Y0 + ddy
      End If
    Next k
    x1 = MnGraphBounds.Left + 0.3 * MnGraphBounds.Width
    x2 = MnGraphBounds.Left + 0.5 * MnGraphBounds.Width
    '
    '
    'Preis
    '
    'Y0 = MnGraphBounds.top + MnGraphBounds.height - 4 * ddy
    Y0 = Y0 + 3 * ddy
    Texte = Texxt(2204)
    X0 = x1 - Graph.MeasureString(Texte, FontNew).Width()
    Graph.DrawString(Texte, FontNew, BrushZ, X0, Y0)

    '
    '
    Ftext = Format(Preis(0) * Fak(0), "########.000")
    X0 = x2 - Graph.MeasureString(Ftext, FontNew).Width()
    Graph.DrawString(Ftext, FontNew, BrushZ, X0, Y0)

    '
    '
    'Prozentigkeit
    '
    '
    'Y0 = MnGraphBounds.top + MnGraphBounds.height - 3 * ddy
    Y0 = Y0 - ddy
    Texte = Texxt(2202)
    X0 = x1 - Graph.MeasureString(Texte, FontNew).Width()

    Graph.DrawString(Texte, FontNew, BrushZ, X0, Y0)

    '
    '
    Ftext = Format(100.0# * Suf(0) / (sum(0) + tiny), "####.000")
    X0 = x2 - Graph.MeasureString(Ftext, FontNew).Width()
    Graph.DrawString(Ftext, FontNew, BrushZ, X0, Y0)

    '
    '
    'Schichtdicke
    '
    '
    'Y0 = MnGraphBounds.top + MnGraphBounds.height - 2 * ddy
    Y0 = Y0 - ddy
    Texte = Texxt(2203)
    X0 = x1 - Graph.MeasureString(Texte, FontNew).Width()
    Graph.DrawString(Texte, FontNew, BrushZ, X0, Y0)

    '
    '
    Ftext = Format(MnAllRezepte.Rezepte(MnRzName(0)).Dicke(0), "####.000")
    X0 = x2 - Graph.MeasureString(Ftext, FontNew).Width()
    Graph.DrawString(Ftext & " " & MenueParam.Misch.Eind, FontNew, BrushZ, X0, Y0)
    FontNew.Dispose()
    PenNew.Dispose()
  End Sub



  Public Sub CalcFarbWrt()
    Dim KDS As String
    Dim Irw As Integer
    MnFarbWerte.clear()
    WeiSch = MnWSOpt
    If MnFawrtRwerte.Count > 0 Then
      If MnFawrtRwerte(0).Count > 0 Then
        If MnFawrtRwerte(0)(0).IVoNa OrElse (MnFawrtRwerte(0).Count > 1 AndAlso MnFawrtRwerte(0)(1).IVoNa) Then


          AufbauPar.AufbauRezeptMerk(MnFarbWerte, ier)
          Call quali.FarbWrtCalc(MnWinkel, MnFawrtRwerte, MnFarbWerte, ier)
          If MnFarbWerte.Count = 1 Then
            WeiSch = 0
          End If
          If MnFarbWerte.Count = 2 Then
            If MnFarbWerte(1).Count = 0 Then
              WeiSch = 0
            End If
          End If

          '
          '
          '
          '
          'Kontrast-Farbabstand
          '
          '
          '
          If MnFarbWerte.Count = 2 Then
            KDS = MenueParam.Menue.StdMrkKen(21)
            '
            'Index für berechnete Reflexionswerte (IRW)
            '
            '
            Irw = MnFarbWerte(0).Count - 1
            If MnFarbWerte(MnWSOpt).Count > Irw AndAlso Not IsNothing(MnFarbWerte(0).RwKey(1)) AndAlso MnFarbWerte(0)(1).Count > 0 AndAlso MnFarbWerte(MnWSOpt)(Irw)(MnKnlz)(MnKwop).ContainsKey(KDS) Then
              If Not (Trim(MnFarbWerte(MnWSOpt)(Irw)(MnKnlz)(MnKwop)(KDS)) = "" OrElse Double.IsNaN(MnFarbWerte(MnWSOpt)(Irw)(MnKnlz)(MnKwop)(KDS))) Then
                If Not IsNothing(MnTextKDS) Then
                  MnTextKDS.Text = MnFarbWerte(MnWSOpt)(Irw)(MnKnlz)(MnKwop)(KDS)
                End If
              End If
            End If
          End If
        End If
      End If
    End If

  End Sub
  Public Sub Skalier()
    Dim i As Integer
    If IsNothing(cboSKAL) Then Exit Sub
    If MnRmax > 30000 Then Exit Sub
    For i = 0 To cboSKAL.Items.Count - 1
      If CInt(MnRmax) = CInt(cboSKAL.Items(i)) Then
        cboSKAL.SelectedIndex = i
        Exit For
      End If
    Next i
  End Sub
  '
  '
  '
  '
 
End Class
'
