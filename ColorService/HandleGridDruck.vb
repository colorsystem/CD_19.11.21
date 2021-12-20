Option Compare Text
Option Strict Off
Option Explicit On
Public Class HandleGridDruck

  Implements IDisposable
  '
  '
  'Standards für Graphicobjecte
  '
  '
  '
  '
  Dim MnUeberschrift As String
  Dim MnKopfZeile As String
  Dim MnBrushZ As SolidBrush
  Dim MnBrushF As SolidBrush
  Dim RecF As RectangleF
  Dim strueb As String
  Dim Disposed As Boolean
  Dim Zeil As New ZeilPrint
  Dim Grid As DataGridView
  Dim Caption As String
  Dim ZeilCount As Integer
  Dim MnDefaultFont As Font
  Dim MnHeaderfont As Font


  Sub New(ByRef DataGrid As DataGridView)
    Zeil = New ZeilPrint
    MnBrushZ = New SolidBrush(Color.Black)
    MnBrushF = New SolidBrush(Color.LightGray)
    MnDefaultFont = RezFont(2)
    MnHeaderfont = New Font(MnDefaultFont.Name, 12, FontStyle.Bold)
    MnUeberschrift = " "
    Zeil.FontZ = MnDefaultFont
    Zeil.BrushZ = MnBrushZ
    Grid = DataGrid
  End Sub
  Sub DruckGrid(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Zeil.Graphbounds = New RectangleF(0.5 * (ev.MarginBounds.Left + ev.PageBounds.Left), _
    0.5 * (ev.MarginBounds.Top + ev.PageBounds.Top), _
    0.5 * (ev.MarginBounds.Width + ev.PageBounds.Width), _
    0.5 * (ev.MarginBounds.Height + ev.PageBounds.Height))
    If Zeil.RecPrintCount = 0 Then
      Call PrintFlexGrid(ev.Graphics)
    End If
    Zeil.DruckEventRecprint(sender, ev)
  End Sub
  Private Sub Drucken(ByVal ev As PrintPageEventArgs)
    Dim i As Integer
    Dim k As Integer
    Dim x0 As Single
    Dim x As Single
    Dim y As Single
    Dim dx As Single
    Dim dy As Single
    Dim Strueb As String
    Dim UebWidth As Single
    Dim IAlign As Short
    Dim PrintToGrid As Single
    x = 0.0
    y = 0.0

    Zeil.FontZ = MnHeaderfont
    '
    'Überschrift
    '
    x = 0.0
    Strueb = Format(Date.Now, "dd.MM.yyyy HH:mm:ss")
    UebWidth = Zeil.Width - ev.Graphics.MeasureString(Strueb, Zeil.FontZ).Width
    Zeil.Add(Zeil.DrZeil(2, x, y, UebWidth, MnUeberschrift, ev.Graphics))
    Zeil.Add(Zeil.DrZeil(2, x, y, Zeil.Width, Sline(100, "_"), ev.Graphics))
    'Datum
    Zeil.Add(Zeil.DrZeil(1, x, 0, Zeil.Width, Strueb, ev.Graphics))
    '
    '
    'Grid
    '
    '
    'Header
    '
    '
    Zeil.FontZ = MnDefaultFont
    dy = Zeil.FontZ.Height
    y = y + 2 * dy
    PrintToGrid = ev.Graphics.MeasureString("O", Zeil.FontZ).Width / ev.Graphics.MeasureString("O", Grid.DefaultCellStyle.Font).Width
    '    
    'x0 = 0.5 * (Zeil.Width - Grid.Width) * PrintToGrid
    x0 = 0.0
    '
    '
    IAlign = 0
    x = x0
    For i = 0 To Grid.Columns.Count - 1
      dx = Grid.Columns(i).Width * PrintToGrid
      Strueb = Grid.Columns(i).HeaderText
      Zeil.Add(Zeil.DrZeil(IAlign, x, y, dx, Strueb, ev.Graphics))
      x = x + dx
      IAlign = 2
    Next
    Zeil.Add(Zeil.DrZeil(IAlign, x0, y, x, Sline(255, "_"), ev.Graphics))
    For k = 0 To Grid.Rows.Count - 1
      IAlign = 0
      y = y + dy
      x = x0
      For i = 0 To Grid.Columns.Count - 1
        dx = Grid.Columns(i).Width * PrintToGrid
        Strueb = Grid.Rows(k).Cells(i).Value
        Zeil.Add(Zeil.DrZeil(IAlign, x, y, dx, Strueb, ev.Graphics))
        x = x + dx
        IAlign = 1
      Next i
    Next k
  End Sub
  Property Ueberschrift() As String
    Get
      Ueberschrift = MnUeberschrift
    End Get
    Set(ByVal value As String)
      mnUeberschrift = value
    End Set
  End Property
  Property KopfZeile() As String
    Get
      KopfZeile = MnKopfZeile
    End Get
    Set(ByVal value As String)
      MnKopfZeile = value
    End Set
  End Property
  Property DefaultFont() As Font
    Get
      DefaultFont = MnDefaultFont
    End Get
    Set(ByVal value As Font)
      MnDefaultFont = value
    End Set
  End Property
  Property HeaderFont() As Font
    Get
      HeaderFont = MnHeaderfont
    End Get
    Set(ByVal value As Font)
      MnHeaderfont = value
    End Set
  End Property
  Property BrushZ() As SolidBrush
    Get
      BrushZ = MnBrushZ
    End Get
    Set(ByVal value As SolidBrush)
      MnBrushZ = value
    End Set
  End Property
  Property Brushf() As SolidBrush
    Get
      Brushf = MnBrushF
    End Get
    Set(ByVal value As SolidBrush)
      MnBrushZ = value
    End Set
  End Property

  Protected Overrides Sub Finalize()

    MyBase.Finalize()
    If Disposed Then Exit Sub
    dispose()

  End Sub
  Public Sub dispose() Implements IDisposable.Dispose
    If Disposed Then Exit Sub

    Disposed = True
  End Sub




  Sub PrintFlexGrid(ByVal Graph As Graphics)
    Dim Druwidth As Single
    Dim DruRand As Single
    Dim DruBlockHeight As Single
    Dim DruDiff As Single
    Dim DruTextDiff As Single
    Dim Iblock() As Integer
    Dim X As Single
    Dim Y As Single
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim Ivis As Integer
    Dim PageNumber As Integer = 0
    Dim FontGen As New Font(Grid.Font.Name, Grid.Font.Size, FontStyle.Regular)
    Dim FontBold As New Font(Grid.Font.Name, Grid.Font.Size, FontStyle.Bold)

    Zeil.FontZ = FontBold
    Zeil.BrushZ = New SolidBrush(Color.Black)

    'PrintToGrid = ev.Graphics.MeasureString("O", Zeil.FontZ).Width / ev.Graphics.MeasureString("O", Grid.DefaultCellStyle.Font).Width

    '
    '
    'Startparameter für Ausdruck

    Druwidth = 0
    DruRand = Druwidth
    '
    'Blöcke für Spalten definieren
    '
    ReDim Preserve Iblock(0)
    Iblock(0) = 0
    Ivis = 0
    Druwidth = Grid.RowHeadersWidth
    For i = 0 To Grid.Columns.Count - 1
      If Grid.Columns(i).Visible Then
        Druwidth = Druwidth + Grid.Columns(i).Width
        If Druwidth > Zeil.Width - 50 Then
          ReDim Preserve Iblock(UBound(Iblock) + 1)
          Iblock(UBound(Iblock)) = Min(i - Ivis, Grid.Columns.Count)
          Druwidth = Grid.Columns(i).Width
        End If
      Else
        Ivis = Ivis - 1
      End If
    Next i
    If Iblock(UBound(Iblock)) < Grid.Columns.Count Then
      ReDim Preserve Iblock(UBound(Iblock) + 1)
      Iblock(UBound(Iblock)) = Grid.Columns.Count
    End If
    If Grid.ColumnHeadersVisible Then
      DruBlockHeight = Grid.ColumnHeadersHeight
    Else
      DruBlockHeight = 0
    End If
    For j = 0 To Grid.Rows.Count - 1
      DruBlockHeight = DruBlockHeight + Grid.Rows(j).Height
    Next j
    '
    '
    'Ausgabe
    '
    '
    Zeil.FontZ = FontBold
    Call PrintFlexgridKopf(X, Y, MnUeberschrift, Graph)
    For j = 0 To UBound(Iblock) - 1
      For k = 0 To Grid.Rows.Count - 1
        If k = 0 Then
          Y = Y + Grid.ColumnHeadersHeight
        End If
        If k = 0 And Grid.ColumnHeadersVisible Then
          Zeil.FontZ = FontBold

          Druwidth = DruRand
          If Grid.RowHeadersVisible And j = 0 Then
            Druwidth = Druwidth + Grid.RowHeadersWidth
          End If
          For i = Iblock(j) To Iblock(j + 1) - 1
            If Grid.Columns(i).Visible Then
              '
              'Columnheader
              '
              '
              If Grid.Columns(i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft Then
                '
                'Alignment left
                '
                DruDiff = 0.0#
              Else
                '
                'Alignment right
                '
                DruDiff = Grid.Columns(i).Width - Graph.MeasureString(Trim(Grid.Columns(i).HeaderCell.Value), Zeil.FontZ).Width
                If DruDiff < 0.0# Then DruDiff = 0.0#
              End If
              X = Druwidth + DruDiff
              DruTextDiff = Grid.Columns(i).Width
              Zeil.FontZ = FontBold
              If Not IsNothing(Grid.Columns(i).HeaderCell.Style.Font) Then
                Zeil.FontZ = Grid.Columns(i).HeaderCell.Style.Font
              End If

              If DruTextDiff > Graph.MeasureString(Trim(Grid.Columns(i).HeaderCell.Value), Zeil.FontZ).Width Then DruTextDiff = Graph.MeasureString(Trim(Grid.Columns(i).HeaderCell.Value), Zeil.FontZ).Width

              'graph.Print(PrintText(Trim(Grid.Text), DruTextDiff, graph))
              'RecPrint.Add(Zeil.DrZeil(1, X, Y, DruTextDiff, Trim(Grid.Columns(i).HeaderCell.Value), graph))
              Zeil.Add(Zeil.PrintText(X, Y, DruTextDiff, Trim(Grid.Columns(i).HeaderCell.Value), Graph))
              Druwidth = Druwidth + Grid.Columns(i).Width
            End If
          Next i
          Y = Y + Grid.ColumnHeadersHeight
        End If
        'Zeil.FontZ = New Font(Zeil.FontZ.Name, Zeil.FontZ.Size, FontStyle.Regular)
        Druwidth = DruRand
        For i = Iblock(j) To Iblock(j + 1) - 1
          If Grid.Columns(i).Visible Then

            If i = 0 And Grid.RowHeadersVisible Then
              '
              'Rowheader
              '
              If Grid.RowHeadersDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft Then
                '
                'Alignment left
                '
                DruDiff = 0.0#
              Else
                '
                'Alignment right
                '
                DruDiff = Grid.RowHeadersWidth - Graph.MeasureString(Trim(Grid.Rows(k).HeaderCell.Value), Zeil.FontZ).Width
                If DruDiff < 0.0# Then DruDiff = 0.0#
              End If
              X = Druwidth + DruDiff
              DruTextDiff = Grid.RowHeadersWidth
              Zeil.FontZ = FontGen
              If Not IsNothing(Grid.Rows(k).HeaderCell.Style.Font) Then
                Zeil.FontZ = Grid.Rows(k).HeaderCell.Style.Font
              End If
              If DruTextDiff > Graph.MeasureString(Trim(Grid.Rows(k).HeaderCell.Value), Zeil.FontZ).Width Then DruTextDiff = Graph.MeasureString(Trim(Grid.Rows(k).HeaderCell.Value), Zeil.FontZ).Width

              'graph.Print(PrintText(Trim(Grid.Text), DruTextDiff, graph))
              'RecPrint.Add(Zeil.DrZeil(1, X, Y, DruTextDiff, Trim(Grid.Rows(k).HeaderCell.Value), graph))
              Zeil.Add(Zeil.PrintText(X, Y, DruTextDiff, Trim(Grid.Rows(k).HeaderCell.Value), Graph))

              Druwidth = Druwidth + Grid.RowHeadersWidth
            End If
            '
            '
            'Cells
            '
            '
            If Grid.Columns(i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft Then
              '
              'Alignment left
              '
              DruDiff = 0.0#
            Else
              '
              'Alignment right
              '
              DruDiff = Grid.Columns(i).Width - Graph.MeasureString(Trim(Grid.Rows(k).Cells(i).Value), Zeil.FontZ).Width
              If DruDiff < 0.0# Then DruDiff = 0.0#
            End If
            X = Druwidth + DruDiff
            DruTextDiff = Grid.Columns(i).Width
            Zeil.FontZ = Grid.Font
            If Not IsNothing(Grid.Rows(k).Cells(i).Style.Font) Then
              Zeil.FontZ = Grid.Rows(k).Cells(i).Style.Font
            End If
            If DruTextDiff > Graph.MeasureString(Trim(Grid.Rows(k).Cells(i).Value), Zeil.FontZ).Width Then DruTextDiff = Graph.MeasureString(Trim(Grid.Rows(k).Cells(i).Value), Zeil.FontZ).Width

            'graph.Print(PrintText(Trim(Grid.Text), DruTextDiff, graph))
            'RecPrint.Add(Zeil.DrZeil(1, X, Y, DruTextDiff, Trim(Grid.Rows(k).Cells(i).Value), graph))
            Zeil.Add(Zeil.PrintText(X, Y, DruTextDiff, Trim(Grid.Rows(k).Cells(i).Value), Graph))
            Druwidth = Druwidth + Grid.Columns(i).Width
          End If
        Next i
        Y = Y + Grid.Rows(k).Height
        If Y > Zeil.Height - Grid.Rows(k).Height Then
          PageNumber = PageNumber + 1
          Zeil.FontZ = FontBold
          Zeil.Add(Zeil.PageNumber(PageNumber, Graph))
          Zeil.Add(Zeil.NewPage)
          Call PrintFlexgridKopf(X, Y, MnUeberschrift, Graph)
          Y = Y + Grid.ColumnHeadersHeight
        End If
      Next k
      If Y > Zeil.Height - DruBlockHeight And j < UBound(Iblock) - 1 Then
        PageNumber = PageNumber + 1
        Zeil.FontZ = Grid.Font
        Zeil.Add(Zeil.PageNumber(PageNumber, Graph))
        Zeil.Add(Zeil.NewPage)
        Call PrintFlexgridKopf(X, Y, MnUeberschrift, Graph)
      End If
    Next j
    PageNumber = PageNumber + 1
    Zeil.Add(Zeil.PageNumber(PageNumber, Graph))

  End Sub
  Sub PrintFlexgridKopf(ByRef X As Single, ByRef Y As Single, ByVal Kommentar As String, ByVal dru As Graphics)
    Dim EmS As Single
    'dru.Font.Bold = True
    '
    X = 0
    Y = 0
    EmS = Zeil.FontZ.Size
    Zeil.FontZ = New Font(Zeil.FontZ.Name, 14, FontStyle.Bold)
    Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, MnKopfZeile, dru))
    X = 0
    Y = 1.5 * dru.MeasureString(MnKopfZeile, Zeil.FontZ).Height
    Zeil.FontZ = New Font(Zeil.FontZ.Name, EmS, FontStyle.Bold)

    'dru.Print(Kommentar)

    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, Kommentar, dru))
    X = 0
    Zeil.Add(Zeil.DrZeil(1, X, Y, Zeil.Width, CStr(Now), dru))

    Y = Y + dru.MeasureString(CStr(Now), Zeil.FontZ).Height
    Zeil.FontZ = New Font(Zeil.FontZ.Name, Zeil.FontZ.Size, FontStyle.Regular)
  End Sub


End Class
Public Class ZeilPrint
  Dim MnStrFormat As StringFormat
  Dim MnFontZ As Font
  Dim MnBrushZ As SolidBrush
  Dim MnGraphbounds As RectangleF
  Dim RecPrint As New List(Of TopLeftString)
  Dim MnRecPrintCount As Integer
  Dim Rec As RectangleF
  Dim Bru As SolidBrush

  Sub DruckEventRecprint(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    If RecPrint.Count = 0 Then
      MnRecPrintCount = 0
      Exit Sub
    End If
    Do
      Rec = RecPrint(MnRecPrintCount).RecF
      Bru = RecPrint(MnRecPrintCount).BrushF
      If Not IsNothing(Rec) And Not IsNothing(Bru) Then
        ev.Graphics.FillRectangle(Bru, Rec)
      End If
      ev.Graphics.DrawString(RecPrint(MnRecPrintCount).Text, RecPrint(MnRecPrintCount).FontZ, RecPrint(MnRecPrintCount).BrushZ, RecPrint(MnRecPrintCount).Left, RecPrint(MnRecPrintCount).Top)
      If MnRecPrintCount = RecPrint.Count - 1 Then
        ev.HasMorePages = False
        RecPrint.Clear()
        MnRecPrintCount = 0
        Exit Sub
      End If
      MnRecPrintCount = MnRecPrintCount + 1
      If RecPrint(MnRecPrintCount).Top < 0.0 Then
        ev.HasMorePages = True
        MnRecPrintCount = MnRecPrintCount + 1
        Exit Sub
      End If
    Loop
  End Sub
  'Function DrZeil(ByRef Ialign As Short, ByRef X As Single, ByRef Y As Single, ByRef DelX As Single, ByVal DruZeil As String, ByVal Graph As Graphics,Optional BitFont As Boolean, Optional ByVal Recf As RectangleF = Nothing, Optional ByVal Brushf As SolidBrush = Nothing) As TopLeftString

  Function DrZeil(ByRef Ialign As Short, ByRef X As Single, ByRef Y As Single, ByRef DelX As Single, ByVal DruZeil As String, ByVal Graph As Graphics, Optional ByVal Recf As RectangleF = Nothing, Optional ByVal Brushf As SolidBrush = Nothing) As TopLeftString
    Dim DrZ As New TopLeftString
    DrZ.Text = TextDeltax(DruZeil, DelX, Graph)
    DrZ.Left = X + XAlign(Ialign, DrZ.Text, DelX, Graph) + Me.Left
    DrZ.Top = Y + Top
    DrZ.FontZ = MnFontZ
    DrZ.BrushZ = MnBrushZ
    DrZ.BrushF = Brushf
    DrZ.RecF = Recf
    DrZeil = DrZ
  End Function
  Private Function XAlign(ByRef Ialign As Short, ByRef Text As String, ByRef DeltaX As Single, ByRef Graph As Graphics) As Single
    Dim TexWidth As Single
    TexWidth = Graph.MeasureString(Text, MnFontZ).Width

    Select Case Ialign
      Case 0
        '
        'Left
        '
        XAlign = 0
      Case 1
        '
        'Right
        '
        XAlign = DeltaX - TexWidth - Graph.MeasureString("l", MnFontZ).Width
      Case 2
        '
        'Center
        '
        XAlign = 0.5 * (DeltaX - TexWidth)
    End Select
  End Function
  Function NewPage() As TopLeftString
    Dim DrZ As New TopLeftString
    DrZ.Text = ""
    DrZ.Left = -1.0F
    DrZ.Top = -1.0F
    NewPage = DrZ
  End Function
  Function PageNumber(ByVal i As Integer, ByRef Graph As Graphics) As TopLeftString
    Dim DrZ As New TopLeftString
    DrZ.Text = "-" & Format(i, "##0") & "-"
    DrZ.Left = Left + (Width - Graph.MeasureString(DrZ.Text, MnFontZ).Width) / 2
    DrZ.Top = Top + Height
    DrZ.FontZ = MnFontZ
    DrZ.BrushZ = MnBrushZ
    PageNumber = DrZ
  End Function
  Private Function TextDeltax(ByRef Text As String, ByRef DeltaX As Single, ByVal Graph As Graphics) As String
    Dim num As Integer
    Dim Nli As Integer
    Graph.MeasureString(Text, MnFontZ, New SizeF(DeltaX, MnFontZ.Height), StrFormat, num, Nli)
    TextDeltax = Text.Substring(0, num)
  End Function
  Function PrintText(ByVal X As Single, ByVal Y As Single, ByVal DruTextDiff As Single, ByVal Text As String, ByVal dru As Graphics) As TopLeftString
    Dim HilfsText As String
    Dim PrintLength As Single
    Dim i As Integer
    Dim DrZ As New TopLeftString
    HilfsText = ""
    For i = 0 To Len(Text) - 1
      PrintLength = dru.MeasureString(HilfsText & Text.Substring(i, 1), MnFontZ).Width
      If PrintLength > DruTextDiff Then
        Exit For
      Else
        HilfsText = HilfsText & Text.Substring(i, 1)
      End If
    Next i
    DrZ.Text = HilfsText
    DrZ.Left = X + Left
    DrZ.Top = Y + Top
    DrZ.FontZ = MnFontZ
    DrZ.BrushZ = MnBrushZ
    PrintText = DrZ
  End Function

  WriteOnly Property Graphbounds() As RectangleF
    Set(ByVal value As RectangleF)
      MnGraphbounds = value
    End Set
  End Property


  ReadOnly Property Width() As Single
    Get
      Width = MnGraphbounds.Width
    End Get

  End Property
  ReadOnly Property Height() As Single
    Get
      Height = MnGraphbounds.Height

    End Get

  End Property
  ReadOnly Property Left() As Single
    Get
      Left = MnGraphbounds.Left
    End Get

  End Property
  ReadOnly Property Top() As Single
    Get
      Top = MnGraphbounds.Top
    End Get

  End Property
  Property FontZ() As Font
    Get
      FontZ = MnFontZ
    End Get
    Set(ByVal value As Font)
      MnFontZ = value
    End Set
  End Property
  Property BrushZ() As SolidBrush
    Get
      BrushZ = MnBrushZ
    End Get
    Set(ByVal value As SolidBrush)
      MnBrushZ = value
    End Set
  End Property
  ReadOnly Property StrFormat() As StringFormat
    Get
      StrFormat = MnStrFormat
    End Get

  End Property
  Sub Add(ByVal Value As TopLeftString)
    RecPrint.Add(Value)
  End Sub
  ReadOnly Property RecPrintCount() As Integer
    Get
      RecPrintCount = MnRecPrintCount
    End Get
  End Property

  Public Sub New()
    Rec = New Rectangle
    MnStrFormat = New StringFormat
    MnStrFormat.Trimming = StringTrimming.Character
    RecPrint.Clear()
    MnRecPrintCount = 0
  End Sub
End Class
'
'
'
'
'
Public Class TopLeftString
  Dim MnTop As Single
  Dim MnLeft As Single
  Dim MnText As String
  Dim MnFontz As Font
  Dim MnBrushZ As SolidBrush
  Dim MnBrushF As SolidBrush
  Dim MnRecF As RectangleF

  Property Top() As Single
    Get
      Top = MnTop
    End Get
    Set(ByVal AcTop As Single)
      MnTop = AcTop
    End Set
  End Property
  Property Left() As Single
    Get
      Left = MnLeft
    End Get
    Set(ByVal AcLeft As Single)
      MnLeft = AcLeft
    End Set
  End Property

  Property Text() As String
    Get
      Text = MnText
    End Get
    Set(ByVal Actext As String)
      MnText = Actext
    End Set
  End Property
  Property FontZ() As Font
    Get
      FontZ = MnFontz
    End Get
    Set(ByVal AcFontZ As Font)
      MnFontz = AcFontZ
    End Set
  End Property
  Property BrushZ() As SolidBrush
    Get
      BrushZ = MnBrushZ
    End Get
    Set(ByVal AcBrushZ As SolidBrush)
      MnBrushZ = AcBrushZ
    End Set
  End Property
  Property BrushF() As SolidBrush
    Get
      BrushF = MnBrushF
    End Get
    Set(ByVal AcBrushF As SolidBrush)
      MnBrushF = AcBrushF
    End Set
  End Property
  Property RecF() As RectangleF
    Get
      RecF = MnRecF
    End Get
    Set(ByVal AcRecF As RectangleF)
      MnRecF = AcRecF
    End Set
  End Property
End Class

