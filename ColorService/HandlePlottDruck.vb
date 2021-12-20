Option Compare Text
Option Strict Off
Option Explicit On 
'
'
'
'
'
'
Public Class HandlePlottDruck
  Implements IDisposable
  '
  '
  'Standards für Graphicobjecte
  '
  '
  '
  '
  Dim Disposed As Boolean
  Dim MnIsteu As Integer
  Dim MnPlott As New HandleRezGrafik
  Dim GridDruck As HandleGridDruck
  Dim MnPenObj As New Pen(Color.Black, 1)
  Dim MnColFarb As Color
  Dim Zeil As ZeilPrint
  '
  '
  Dim GrLeft As Single = 40
  Dim GrTop As Single = 50
  Dim GrWidth As Single = 750
  Dim GrHeight As Single = 1137
  Dim MnDrPar As Boolean
  '
  '
  '
  '
  '
  Dim dy As Single
  Dim i As Short
  Dim j As Short
  Dim k As Short
  Dim NlzMax As Short
  Dim WinMax As Short
  Dim FaId As Integer
  Dim KeyD As String
  Dim RezNam As String
  Dim RefNam(1) As String
  Dim StriRez As String
  Dim strueb As String
  Dim MnIer As Short
  Dim quali As QualKontrolle
  Dim Umr As RezeptUmrechnung

  Dim MnGrpRwerte As RefValuesGrp
  Dim MnFarbWerte As ValuesGrpsAssigns

  Dim MnAllRezepte As RecipesGrp  'Rezepte (rein)mit Zusatzinformationen
  Dim MnText0 As String
  Dim MnText1 As String
  Dim MnText2 As String
  Dim MnKommentar As String
  Dim MnUeberschrift As String
  Dim MnDatagrid As DataGridView
  '
  '
  '
  '
  '
  '
  '
  '
  Dim MnRr As BitArray
  Dim MnPlusBlock(,,) As Integer
  Dim MnPlus(,,) As Boolean
  Dim MnJProz As Boolean
  Dim MnJProb As Boolean
  Dim MnJStae As Boolean
  Dim MnJSpez As Boolean
  Dim MnPicNr As Integer


  '
  '

  Dim Size As SizeF

  Dim MnWinkel As AngGeos 'Winkel/Geometrie
  Dim MnText As String
  Dim MnTextEIN_0 As String
  Dim MnTextEIN_1 As String
  Dim MnTextEIN_2 As String
  Dim tiny As Single
  Dim ParLbez() As String
  Dim VKwb(1) As Short
  Dim MnRzName() As String
  Dim MnRefNr() As String
  Dim MnKeyNam As String
  Dim MnZusatzName As List(Of String)
  Dim MnZusatzMenge As List(Of Single)
  Dim MnInformationsName As List(Of String)
  Dim MnInformationsBez As List(Of String)

  Dim PrintPreview As PrintPreviewDialog
  Dim WithEvents Printdoc As PrintDocument
  Dim MnPrintset As PrinterSettings
  Protected Overrides Sub Finalize()

    MyBase.Finalize()
    If Disposed Then Exit Sub
    dispose()

  End Sub
  Public Sub dispose() Implements IDisposable.Dispose
    If Disposed Then Exit Sub
    If IsNothing(quali) Then Exit Sub
    quali = Nothing
    Umr = Nothing
    MnFarbWerte = Nothing
    MnPlott.dispose()
    MnPenObj.Dispose()

    Call Clear()
    Disposed = True
  End Sub
  Public Sub Clear()
    Erase MnRzName
    Erase MnRefNr
  End Sub

  '
  '
  '
  

  WriteOnly Property printset As PrinterSettings

    Set(value As PrinterSettings)
      MnPrintset = value
    End Set
  End Property

  '
  '
  '
  '
  '
  '
  'Properties
  '
  Property PicNr() As Integer
    Get
      PicNr = MnPicNr
    End Get
    Set(ByVal Value As Integer)
      MnPicNr = Value
    End Set
  End Property
  '
  '
  Property JProz() As Boolean
    Get
      JProz = MnJProz
    End Get
    Set(ByVal Value As Boolean)
      MnJProz = Value
    End Set
  End Property
  Property JProb() As Boolean
    Get
      JProb = MnJProb
    End Get
    Set(ByVal Value As Boolean)
      MnJProb = Value
    End Set
  End Property
  Property JStae() As Boolean
    Get
      JStae = MnJStae
    End Get
    Set(ByVal Value As Boolean)
      MnJStae = Value
    End Set
  End Property
  Property JSpez() As Boolean
    Get
      JSpez = MnJSpez
    End Get
    Set(ByVal Value As Boolean)
      MnJSpez = Value
    End Set
  End Property
  Property KeyNam() As String
    Get
      KeyNam = MnKeyNam
    End Get
    Set(ByVal Value As String)
      MnKeyNam = Value
    End Set
  End Property
  '
  Property DrPar() As Boolean
    Get
      DrPar = MnDrPar
    End Get
    Set(ByVal Value As Boolean)
      MnDrPar = Value
    End Set
  End Property
  Property Kommentar() As String
    Get
      Kommentar = MnKommentar
    End Get
    Set(ByVal Value As String)
      MnKommentar = Value
    End Set
  End Property

  Property Ueberschrift() As String
    Get
      Ueberschrift = MnUeberschrift
    End Get
    Set(ByVal Value As String)
      MnUeberschrift = Value
    End Set
  End Property
  Property DataGrid() As DataGridView
    Get
      DataGrid = MnDatagrid
    End Get
    Set(ByVal Value As DataGridView)
      MnDatagrid = Value
      GridDruck = New HandleGridDruck(MnDatagrid)
    End Set
  End Property
  ''
  '
  '
  '
  '
  '
  '
  '
  Property Plott() As HandleRezGrafik
    Get
      Plott = MnPlott
    End Get
    Set(ByVal Value As HandleRezGrafik)
      MnPlott = Value
    End Set
  End Property

  Property ZusatzName As List(Of String)
    Get
      ZusatzName = MnZusatzName
    End Get
    Set(ByVal Value As List(Of String))
      MnZusatzName = Value
    End Set
  End Property
  Property ZusatzMenge As List(Of Single)
    Get
      ZusatzMenge = MnZusatzMenge
    End Get
    Set(ByVal Value As List(Of Single))
      MnZusatzMenge = Value
    End Set
  End Property
  '
  '
  '
  Property InformationsName As List(Of String)
    Get
      InformationsName = MnInformationsName
    End Get
    Set(ByVal Value As List(Of String))
      MnInformationsName = Value
    End Set
  End Property
  Property InformationsBez As List(Of String)
    Get
      InformationsBez = MnInformationsBez
    End Get
    Set(ByVal Value As List(Of String))
      MnInformationsBez = Value
    End Set
  End Property
  '
  '
  '
  Property GrpRwerte() As RefValuesGrp
    Get
      GrpRwerte = MnGrpRwerte
    End Get
    Set(ByVal Value As RefValuesGrp)
      MnGrpRwerte = Value
    End Set
  End Property
  '

  '

  '

  'Texte
  ' 
  '
  '
  Property Text0() As String
    Get
      Text0 = MnText0
    End Get
    Set(ByVal AcText0 As String)
      MnText0 = AcText0
    End Set
  End Property
  '
  '
  Property Text1() As String
    Get
      Text1 = MnText1
    End Get
    Set(ByVal AcText1 As String)
      MnText1 = AcText1
    End Set
  End Property
  '
  '
  '
  '
  Property Text2() As String
    Get
      Text2 = MnText2
    End Get
    Set(ByVal AcText2 As String)
      MnText2 = AcText2
    End Set
  End Property

  '

  '
  '
  '
  '
  '
  Property FarbWerte As ValuesGrpsAssigns
    Get
      FarbWerte = MnFarbWerte
    End Get
    Set(value As ValuesGrpsAssigns)
      MnFarbWerte = value
    End Set
  End Property
  '
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
      MnWinkel = AcWinkel
    End Set
  End Property
  '
  '
  '
  '
  '
  '

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


  '
  '
  'Rezeptkennung
  '
  '
  '
  '
  '
  Property RzName As String()
    Get
      RzName = MnRzName
    End Get
    Set(ByVal AcRzName As String())
      MnRzName = AcRzName
    End Set
  End Property
  '
  '
  '
  'Kennung für R-Werte 
  '
  '
  '

  '
  '
  '
  '
  Property RefNr As String()
    Get
      RefNr = MnRefNr
    End Get
    Set(ByVal AcRefNr As String())
      MnRefNr = AcRefNr
    End Set
  End Property
  

  '
  '
  '
  '
  Sub DruckMerkmale(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    If Zeil.RecPrintCount = 0 Then
      Zeil.Graphbounds = New RectangleF(GrLeft, GrTop, GrWidth, GrHeight)
      Call DruMerk(ev, Zeil)
    End If
    Call Zeil.DruckEventRecprint(sender, ev)
  End Sub
  '
  '
  '
  Sub DruMerk(ByVal ev As PrintPageEventArgs, ByRef Zeil As ZeilPrint)
    Dim Ianwsg As Integer
    Dim i As Integer
    Dim iv As Integer
    Dim j As Integer
    Dim k As Integer
    Dim kn As Integer
    Dim kw As Integer
    Dim Jleer As Boolean
    Dim KNull As Integer
    Dim ZmatAlt As Integer
    Dim ZMat As Integer
    Dim SMat As Integer
    Dim strueb As String
    Dim WinNor As String
    Dim AnwsgName As String
    Dim llen As Integer
    Dim Vmat(0, 0) As Object
    Dim Umat(0, 0) As Object
    Dim VmatH(0, 0) As Object
    Dim UmatH(0, 0) As Object
    Dim druzeil As String
    Dim Dadru As Integer
    Dim X As Single
    Dim Y As Single
    Dim dx As Single
    Dim dy As Single
    Dim SWirk As Integer
    Dim MrkAnz As Short
    Dim MrkStart As Single
    Dim ParamID As New List(Of Integer)
    Dim ZmatStart As Integer
    Dim ZmatEnde As Integer
    Dim ZmatDelta As Integer
    Dim ZmatDelta0 As Integer
    Dim Page As Integer
    Dim Zdelta As Integer = 5
    Dim flag As Boolean
    '
    '
    '
    '
    Dadru = 0
    If (MenueParam.User.Druq Mod 32) > 1 Then
      Dadru = 1
    End If

    '

    llen = 0
    '
    'Initialisierung der Druckausgabe

    Call DruIni(BitInt(0, 0, MenueParam.User.Druq), MrkAnz, MrkStart, X, Y, dx, dy, Zeil, ev)
    '
    'Überschrift
    '
    '
    '
    '
    strueb = Sline(255, "=")
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(2, X - 0.125 * dx, Y, Zeil.Width, strueb, ev.Graphics))
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, MenueParam.Messg.LaborText1, ev.Graphics))
    Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, Texxt(100), ev.Graphics))
    Zeil.Add(Zeil.DrZeil(1, X, Y, Zeil.Width - 0.125 * dx, Format(Today, "dd.mm.yyyy"), ev.Graphics))
    '

    '
    '
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, MenueParam.Messg.LaborText2, ev.Graphics))
    Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, Texxt(101), ev.Graphics))
    Zeil.Add(Zeil.DrZeil(1, X, Y, Zeil.Width - 0.125 * dx, Format(TimeOfDay, "hh.mm") & " H", ev.Graphics))
    '
    '
    '
    '
    '
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, MenueParam.Messg.LaborText3, ev.Graphics))
    Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, MenueParam.Menue.MethBez & Space(2) & MenueParam.User.Name, ev.Graphics))
    Zeil.Add(Zeil.DrZeil(1, X, Y, Zeil.Width - 0.125 * dx, MenueParam.Messg.Kbez, ev.Graphics))
    '
    '
    'Art der Farbstärkeberechnung
    '
    '
    '
    Select Case MenueParam.MethID
      Case 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 22, 23
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, Texxt(450 + MenueParam.Menue.FstaeID), ev.Graphics))
    End Select
    '
    '
    '
    'Deckvermögen
    '
    '
    Select Case MenueParam.MethID
      Case 15, 16
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, Texxt(186) & ":" & Format(MenueParam.Menue.KdeWS, " ##.00"), ev.Graphics))

    End Select
    '

    strueb = Sline(255, "=")
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(2, X - 0.125 * dx, Y, Zeil.Width, strueb, ev.Graphics))
    '
    If Trim(MnFarbWerte.Kommentar) <> "" Then
      'Kommentar
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, MnFarbWerte.Kommentar, ev.Graphics))
    End If
    If Trim(MnFarbWerte.PruefMittel) <> "" Then
      'Pruefmittel
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, Texxt(1640) & ": " & MnFarbWerte.PruefMittel, ev.Graphics))
    End If
    If Trim(MnFarbWerte.ProduktArt) <> "" Then
      'Produktart
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, Texxt(1641) & ": " & MnFarbWerte.ProduktArt, ev.Graphics))
    End If
    strueb = Space(1)
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, strueb, ev.Graphics))
    '
    '
    'GK-Werte
    '
    '
    '
    Select Case MenueParam.MethID
      Case 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 22
        strueb = Texxt(151)
        strueb = strueb & ":" & Space(2)
        For i = 0 To 5
          strueb = strueb & Format(MenueParam.User.Winkel(0).GK(i), " #0.00")
        Next i
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(1, X, Y, Zeil.Width - 4 * dx, strueb, ev.Graphics))
        strueb = ""
        For i = 0 To 5
          strueb = strueb & Format(MenueParam.User.Winkel(0).GK(i + 6), " #0.00")
        Next i
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(1, X, Y, Zeil.Width - 4 * dx, strueb, ev.Graphics))
        druzeil = Space(255)
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, druzeil, ev.Graphics))
    End Select
    '
    '
    '
    '
    'Untergrund weiß
    '
    '
    '
    Select Case MenueParam.MethID
      Case 5, 6, 8, 16, 19, 20
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(1, X + 2 * dx, Y, 6 * dx, Texxt(355), ev.Graphics))
        If Not IsNothing(GrpRwerte("W").RefUnt) Then
          Zeil.Add(Zeil.DrZeil(0, X + 8 * dx, Y, Zeil.Width - 8 * dx, ":  " & GrpRwerte("W").RefUnt.Name, ev.Graphics))
        End If
    End Select
    '
    '
    '
    '
    'Untergrund schwarz
    '
    '
    '
    Select Case MenueParam.MethID
      Case 8, 16, 20
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(1, X + 2 * dx, Y, 6 * dx, Texxt(356), ev.Graphics))
        Zeil.Add(Zeil.DrZeil(0, X + 8 * dx, Y, Zeil.Width - 8 * dx, ":  " & GrpRwerte("S").RefUnt.Name, ev.Graphics))
    End Select
    '
    '
    '
    '
    'Weiß-,Schwarz- und Grauverschnitt
    '
    '
    '
    Select Case MenueParam.MethID
      Case 7, 15
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(1, X + 2 * dx, Y, 6 * dx, Texxt(352), ev.Graphics))
        Zeil.Add(Zeil.DrZeil(0, X + 8 * dx, Y, Zeil.Width - 8 * dx, ":  " & MnGrpRwerte("W").RefUnt.Name, ev.Graphics))
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(1, X + 2 * dx, Y, 6 * dx, Texxt(353), ev.Graphics))
        Zeil.Add(Zeil.DrZeil(0, X + 8 * dx, Y, Zeil.Width - 8 * dx, ":  " & MnGrpRwerte("S").RefUnt.Name, ev.Graphics))
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(1, X + 2 * dx, Y, 6 * dx, Texxt(354), ev.Graphics))
        Zeil.Add(Zeil.DrZeil(0, X + 8 * dx, Y, Zeil.Width - 8 * dx, ":  " & MnGrpRwerte("G").RefUnt.Name, ev.Graphics))
    End Select
    '
    '
    '
    Ianwsg = MnFarbWerte.Count
    If Ianwsg = 0 Then
      Exit Sub
    End If

    '
    '
    '
    'Drucken
    '
    '
    '
    '
    'Parameter Drucken
    '

    '
    '
    If MenueParam.ParaMerks.ParamCount > 0 And BitWrt(6, MenueParam.User.Drum) Then
      ParamID.Clear()
      ReDim Umat(2, 0)
      druzeil = Sline(255, "_")
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, druzeil, ev.Graphics))
      For Ianwsg = 0 To MnFarbWerte.Count - 1
        For kn = 0 To MnFarbWerte(Ianwsg).AuswID.Count - 1
          For i = 0 To MenueParam.ParaMerks.ParamCount - 1
            For j = 0 To MenueParam.ParaMerks(i).AuswID.Count - 1
              If MnFarbWerte(Ianwsg).AuswID(kn) = MenueParam.ParaMerks(i).AuswID(j) Then
                For k = 0 To ParamID.Count - 1
                  If ParamID(k) = MenueParam.ParaMerks(i).ID Then
                    Exit For
                  End If
                Next k
                If k >= ParamID.Count Then
                  k = ParamID.Count + 1
                  ReDim Preserve Umat(2, k - 1)
                  ParamID.Add(KeyRe(k - 1))
                  ParamID(k - 1) = MenueParam.ParaMerks(i).ID
                  Umat(0, k - 1) = ""
                  If MenueParam.ParaMerks(i).BezWrt <> "" Then
                    Umat(0, k - 1) = "(" & Trim(MenueParam.ParaMerks(i).BezWrt) & ")"
                  End If
                  Umat(1, k - 1) = MenueParam.ParaMerks(i).Kbez
                  Umat(2, k - 1) = Format(MenueParam.ParaMerks(i).Wert, MenueParam.ParaMerks(i).Form)
                End If
              End If
            Next j
          Next i
        Next kn
      Next Ianwsg
      If Not IsNothing(ParamID) AndAlso ParamID.Count > 0 Then
        Call MatrixDru(1, 377.0, -120.0, Y, dx, dy, 0, ParamID.Count - 1, 0, 2, Umat, ev.Graphics, False)
      End If
    End If
    '
    '
    'Anweisungen
    '
    'Lichtarten
    '
    '
    ReDim MnPlusBlock(MnFarbWerte.Count, MenueParam.Normfa.Nlz, MenueParam.User.Winkel.Km)
    ReDim MnPlus(MnFarbWerte.Count, MenueParam.Normfa.Nlz, MenueParam.User.Winkel.Km)
    For kn = 0 To MenueParam.Normfa.Nlz - 1

      '
      ' Merkmale für Z_Nr-te Überschrift
      '
      '
      '
      '
      'Winkel/Meßgeometrien
      '
      '
      For kw = 0 To MenueParam.User.Winkel.Km - 1

        For Ianwsg = 0 To MnFarbWerte.Count - 1
          If MnFarbWerte(Ianwsg).PrintNLAWIN(kn, kw) Then
            ZMat = 0
            SMat = MnFarbWerte(Ianwsg).CountMerk
            ReDim Umat(SMat, ZMat)
            Umat(0, ZMat) = Texxt(600)
            For j = 0 To SMat - 1
              Umat(j + 1, ZMat) = Trim(MnFarbWerte(Ianwsg).Merk(j).Kbez) & Space(1)
            Next j
            ZMat = MnFarbWerte(Ianwsg).Count
            If ZMat > 0 Then


              ReDim Vmat(SMat, ZMat)
              '       V-Mat aufbauen
              '
              KNull = 0
              iv = -1
              For i = 0 To ZMat - 1
                Jleer = False
                If MnFarbWerte(Ianwsg)(i).Itp Or i = 0 Then
                  Jleer = True
                End If
                If i > 0 Then
                  If MnFarbWerte(Ianwsg)(i).Nr = MnFarbWerte(Ianwsg)(i - 1).Nr Then
                    Jleer = False
                  End If
                End If
                If Jleer Then
                  iv = iv + 1
                  ReDim Preserve Vmat(SMat, iv)
                  For j = 0 To SMat
                    Vmat(j, iv) = ""
                  Next j
                End If
                iv = iv + 1
                ReDim Preserve Vmat(SMat, iv)
                For j = 0 To SMat
                  Vmat(j, iv) = ""
                Next j
                If Dadru = 1 Then
                  Vmat(0, iv) = CStr(MnFarbWerte(Ianwsg)(i).Dattim) & Space(1) & MnFarbWerte(Ianwsg)(i).Name
                Else
                  Vmat(0, iv) = MnFarbWerte(Ianwsg)(i).Name
                End If
                If Vmat(0, iv).length > 25 Then
                  Vmat(0, iv) = Vmat(0, iv).substring(0, 25)
                End If
                For j = 0 To SMat - 1
                  Vmat(j + 1, iv) = MnFarbWerte(Ianwsg)(i)(kn)(kw)(j)
                  If IsDBNull(Vmat(j + 1, iv)) Then
                    Vmat(j + 1, iv) = ""
                  End If

                  If Trim(Vmat(j + 1, iv)) <> "" Then
                    KNull = KNull + 1
                  End If
                Next j
              Next i
              ZmatAlt = iv
              ZMat = ZmatAlt
              '
              '
              '
              If SMat > MrkAnz Then
                'Umspeichern, falls mehr als 8 Merkmale in Zeile
                ReDim UmatH(SMat - MrkAnz, 0)

                ReDim VmatH(SMat - MrkAnz, ZMat)
                For i = 0 To ZMat
                  VmatH(0, i) = Vmat(0, i)
                Next
                UmatH(0, 0) = Umat(0, 0)
                For j = 1 To SMat - MrkAnz
                  UmatH(j, 0) = Umat(MrkAnz + j, 0)
                  For i = 0 To ZMat
                    VmatH(j, i) = Vmat(MrkAnz + j, i)
                  Next i
                Next j
              End If
              '
              flag = True
              '
              'Max Anzahl Zeilen pro Seite
              ZmatDelta = (Zeil.Height - Y) / dy - 12
              ZmatDelta0 = Zeil.Height / dy - 12
              If SMat > MrkAnz Then
                ZmatDelta = (ZmatDelta - Zdelta) / 2
                ZmatDelta0 = (ZmatDelta0 - Zdelta) / 2
              End If
              If ZMat <= ZmatDelta Then
                '
                'noch Platz auf aktueller Seite
                '
                ZmatStart = 0
                ZmatEnde = ZMat
              Else
                ZmatStart = 0
                ZmatEnde = ZmatDelta
                If ZMat < ZmatDelta0 Then
                  flag = False
                End If
              End If
            End If
            '
            '         Schreibe Zeile für Überschrift
            '
            '         Name für Anweisung
            '
            If KNull > 0 Then
              '


NextPage:     AnwsgName = MnFarbWerte(Ianwsg).AnwsgName
              WinNor = MenueParam.Normfa(kn).NormNama & Space(1) & MenueParam.User.Winkel(kw).Chrm & "(" & Format(100 * MenueParam.User.Winkel(kw).Iglz * MenueParam.Messg.Winkel(kw).GK(0), "0.0") & ")"
              '
              'Prüfen ob Seite voll
              '
              If kn >= 0 Or kw >= 0 Or Ianwsg >= 0 Then
                If Y + 15 * dy > Zeil.Height Then
                  Page = Page + 1
                  Zeil.Add(Zeil.DrZeil(2, X, Zeil.Height - 3 * dy, Zeil.Width, "-" & Format(Page, "##0") & "-", ev.Graphics))
                  Zeil.Add(Zeil.NewPage)
                  Y = 0
                End If
              End If
              druzeil = Sline(255, "_")
              Y = Y + dy
              Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, druzeil, ev.Graphics))
              If kw = 0 Then
                druzeil = Sline(255, "_")
                Y = Y + dy
                Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, druzeil, ev.Graphics))
              End If
              '
              druzeil = Space(255)
              Y = Y + dy
              Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, druzeil, ev.Graphics))
              '
              '             Name für Winkel(Geometrie) und Lichtart
              '
              '
              Y = Y + dy
              Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, AnwsgName, ev.Graphics))
              Zeil.Add(Zeil.DrZeil(1, X, Y, Zeil.Width - 0.125 * dx, WinNor, ev.Graphics))

              '
              SWirk = SMat
              If SMat > MrkAnz Then
                SWirk = MrkAnz
              End If
              Call MatrixDru(2, X, MrkStart, Y, dx, dy, 0, 0, 0, SWirk, Umat, ev.Graphics, False)
              druzeil = Sline(255, "_")
              Y = Y + dy
              Zeil.Add(Zeil.DrZeil(0, X + MrkStart, Y, Zeil.Width - MrkStart, druzeil, ev.Graphics))
              '
              '
              Call MatrixDru(1, X, MrkStart, Y, dx, dy, ZmatStart, ZmatEnde, 0, SWirk, Vmat, ev.Graphics, True)
              If SMat > MrkAnz Then
                '
                'Prüfen ob Seite voll
                '
                If kn > 0 Or kw > 0 Or Ianwsg > 0 Then
                  If Y + 5 * dy > Zeil.Height Then
                    Page = Page + 1
                    Zeil.Add(Zeil.DrZeil(2, X, Zeil.Height - 3 * dy, Zeil.Width, "-" & Format(Page, "##0") & "-", ev.Graphics))
                    Zeil.Add(Zeil.NewPage)
                    Y = 0
                  End If
                End If
                druzeil = Sline(255, "_")
                Y = Y + dy
                Zeil.Add(Zeil.DrZeil(0, X + MrkStart, Y, Zeil.Width - MrkStart, druzeil, ev.Graphics))
                druzeil = Space(255)
                Y = Y + dy
                Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, druzeil, ev.Graphics))
                SWirk = SWirk + 1
                Call MatrixDru(2, X, MrkStart, Y, dx, dy, 0, 0, 0, SMat - MrkAnz, UmatH, ev.Graphics, False)
                druzeil = Sline(255, "_")
                Y = Y + dy
                Zeil.Add(Zeil.DrZeil(0, X + MrkStart, Y, Zeil.Width - MrkStart, druzeil, ev.Graphics))
                '
                Call MatrixDru(1, X, MrkStart, Y, dx, dy, ZmatStart, ZmatEnde, 0, SMat - MrkAnz, VmatH, ev.Graphics, True)
              End If
              '
              'Prüfen, ob alle Zeilen gedruckt
              '
              '
              ZmatDelta = ZmatDelta0
              If flag Then
                ZMat = ZMat - ZmatDelta - 1
                ZmatStart = ZmatEnde + 1
                ZmatEnde = ZmatStart + Min(ZMat, ZmatDelta)
              Else
                ZmatStart = ZmatEnde + 1
                ZmatEnde = ZMat
                If ZmatStart <= ZmatEnde Then
                  ZMat = 1
                Else
                  ZMat = -1
                End If
              End If

              If ZMat > 0 Then
                Page = Page + 1
                Zeil.Add(Zeil.DrZeil(2, X, Zeil.Height - 3 * dy, Zeil.Width, "-" & Format(Page, "##0") & "-", ev.Graphics))
                Zeil.Add(Zeil.NewPage)
                Y = 0
                GoTo nextpage
              Else
                If ZmatAlt > ZmatDelta0 Or (Zeil.Height - Y) / dy - 12 <= Zdelta Then
                  If ZmatStart > 0 And Not (kn = MenueParam.Normfa.Nlz - 1 And kw = MenueParam.User.Winkel.Km - 1 And Ianwsg = MnFarbWerte.Count - 1) Then
                    Page = Page + 1
                    Zeil.Add(Zeil.DrZeil(2, X, Zeil.Height - 3 * dy, Zeil.Width, "-" & Format(Page, "##0") & "-", ev.Graphics))
                    Zeil.Add(Zeil.NewPage)
                    Y = 0
                  End If

                End If


              End If                      'KNULL
            End If
          End If
          'Zmat
        Next Ianwsg
      Next kw
    Next kn
    druzeil = Sline(255, "_")
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, druzeil, ev.Graphics))
    Page = Page + 1
    Zeil.Add(Zeil.DrZeil(2, X, Zeil.Height - 3 * dy, Zeil.Width, "-" & Format(Page, "##0") & "-", ev.Graphics))


  End Sub



  '
  '
  '
  '
  '


  Sub DrKorrDru(ByRef X0 As Single, ByRef Y As Single, ByRef dx As Single, ByRef dy As Single, ByRef kwb As Short, ByRef RezSozpt As RecipesGrp, ByRef zeil As ZeilPrint, ByRef Graph As Graphics)
    Dim Scwe As Single
    Dim sum As Single
    Dim i As Short
    Dim j As Short
    Dim MxMe As Short
    Dim Y0 As Single
    Dim GesMeng() As Single
    Dim GesPreis() As Single
    Dim X As Single
    Dim Kenn(1) As String
    '
    X = X0
    MxMe = UBound(MnRzName) + 1
    ReDim GesMeng(MxMe - 1)
    ReDim GesPreis(MxMe - 1)
    For j = 0 To MxMe - 1
      GesMeng(j) = 0.0#
      GesPreis(j) = 0
      For i = 0 To RezSozpt.Rezepte(MnRzName(j)).KF - 1
        FaId = RezSozpt.Rezepte(MnRzName(j))(i).ID
        KeyD = KeyName(FaId)
        GesPreis(j) = GesPreis(j) + RezSozpt.Rezepte(MnRzName(j))(i).FaAmng * RezSozpt.Farben(KeyD).Preis
        GesMeng(j) = GesMeng(j) + RezSozpt.Rezepte(MnRzName(j))(i).FaAmng
      Next i
      GesMeng(j) = GesMeng(j) / 100.0#
    Next j
    If MxMe > 1 Then
      If RezSozpt.Rezepte(MnRzName(0)).KF > 0 Then
        strueb = Space(30)
        Y = Y + dy
        Kenn(0) = MenueParam.Normfa(0).NormKenn
        If MenueParam.Normfa.Nlz = 1 Then
          Kenn(1) = Space(6)
        Else
          Kenn(1) = MenueParam.Normfa(1).NormKenn
        End If
        zeil.Add(zeil.DrZeil(0, X, Y, zeil.Width, strueb, Graph))
        If MnRzName.Count > 1 Then
          X = 230
          Y0 = Y
          Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21001) & "A", "----------------", Format(RezSozpt.Rezepte(MnRzName(0)).Dicke(0), "###0.00")}, zeil, Graph)
          Y = Y0
          X = X + 70
          Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21001) & "N", "----------------", Format(RezSozpt.Rezepte(MnRzName(1)).Dicke(0), "###0.00")}, zeil, Graph)
        Else
          X = 300
          Y0 = Y
          Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21001), "----------------", Format(RezSozpt.Rezepte(MnRzName(0)).Dicke(0), "###0.00")}, zeil, Graph)
        End If
        Y = Y0
        X = X + 70
        Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21000), "----" & Texxt(891 + kwb) & "--------", Format(RezSozpt.Rezepte(MnRzName(1)).Sorkrit(0)(1), "###0.00")}, zeil, Graph)
        Y = Y0
        X = X + 70
        Call BlockZeil(0, X, Y, dy, {Space(1) & Kenn(0), "----" & Texxt(891 + kwb) & "--------", Format(RezSozpt.Rezepte(MnRzName(1)).Sorkrit(0)(5), "###0.00")}, zeil, Graph)
        Y = Y0
        X = X + 70
        Call BlockZeil(0, X, Y, dy, {Space(1) & Kenn(1), "----" & Texxt(891 + kwb) & "--------", Format(RezSozpt.Rezepte(MnRzName(1)).Sorkrit(0)(6), "###0.00")}, zeil, Graph)
        Y = Y0
        X = X + 70
        Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21002), "----------------", Format(RezSozpt.Rezepte(MnRzName(1)).Sorkrit(0)(3), "###0.00")}, zeil, Graph)
        Y = Y0
        X = X + 70
        Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21003), "----------------", Format(GesPreis(0), "######0.00")}, zeil, Graph)
      End If
      End If

      strueb = Sline(255, "-")
      Y = Y + dy
      zeil.Add(zeil.DrZeil(2, X0, Y, zeil.Width, strueb, Graph))

      '
      '
      Y = Y + 2 * dy
      '
      'Name
      '
      Call AnteilMenge(X0, Y, dy, MnRzName, RezSozpt, zeil, Graph, {"01", "02", "05", "06", "07", "09", "12"})
      '


      Y = Y + dy




  End Sub

  Sub BlockZeil(ByRef Ialign As Short, ByRef X As Single, ByRef Y As Single, ByRef dy As Single, Strueb() As String, ByRef Zeil As ZeilPrint, ByRef Graph As Graphics)
    Dim k As Integer
    For k = 0 To Strueb.Count - 1
      Zeil.Add(Zeil.DrZeil(Ialign, X, Y, Zeil.Width, Strueb(k), Graph))
      Y = Y + dy
    Next
    Y = Y - dy
  End Sub

  '
  Sub DrRezKopf(ByRef X As Single, ByRef Y As Single, ByRef dy As Single, ByRef Zeil As ZeilPrint, ByRef Graph As Graphics)
    MnIer = 0
    Y = 0.0
    X = 0.0
    '
    '
    '
    ' Ueberschrift
    '
    strueb = Sline(255, "=")
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, strueb, Graph))

    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, MenueParam.Messg.LaborText1, Graph))

    Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, Texxt(100), Graph))

    Zeil.Add(Zeil.DrZeil(1, X, Y, Zeil.Width, Format(Date.Now, "dd.MM.yyyy") & Space(3) & Format(Date.Now, "HH:mm") & " H", Graph))

    '

    '
    '
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, MenueParam.Messg.LaborText2, Graph))

    Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, Texxt(101), Graph))

    Zeil.Add(Zeil.DrZeil(1, X, Y, Zeil.Width, Texxt(306 + MenueParam.Menue.JABST), Graph))

    '
    '
    '
    '
    '
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, MenueParam.Messg.LaborText3, Graph))
    Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, MenueParam.Menue.MethBez & Space(2) & MenueParam.User.Name, Graph))
    Zeil.Add(Zeil.DrZeil(1, X, Y, Zeil.Width, MenueParam.Messg.Kbez, Graph))

    '
    '
    strueb = Sline(255, "=")
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, strueb, Graph))



  End Sub

  Sub DrRezDru(ByRef Isch As Short, ByRef X0 As Single, ByRef Y As Single, ByRef dy As Single, ByRef RzNr As String, ByRef kwb As Short, ByRef RezSozpt As RecipesGrp, ByRef Zeil As ZeilPrint, ByVal Graph As Graphics)
    Dim i As Integer
    Dim Y0 As Single
    Dim GesPreis As Single
    Dim GesDichte As Single
    Dim GesMenge As Single
    Dim X As Single
    Dim Kenn(1) As String
    Dim Rzkey(0) As String
    Dim Fakey As String
    With RezSozpt
      X = X0
      GesDichte = 0.0
      GesPreis = 0.0
      GesMenge = 0.0
      For i = 0 To .Rezepte(RzNr).KF - 1
        GesMenge = .Rezepte(RzNr)(i).FaAmng
        Fakey = KeyName(.Rezepte(RzNr)(i).ID)
        GesPreis = .Rezepte(RzNr)(i).FaAmng * .Farben(Fakey).Preis
        GesDichte = .Rezepte(RzNr)(i).FaAmng / .Farben(Fakey).Spz
      Next
      GesDichte = GesMenge / GesDichte
      GesPreis = GesPreis / GesMenge
      strueb = Texxt(388)
      If .Rezepte(RzNr).Sorkrit(0).Count > 0 Then
        Y = Y + dy
        Kenn(0) = MenueParam.Normfa(0).NormKenn
        If MenueParam.Normfa.Nlz = 1 Then
          Kenn(1) = Space(6)
        Else
          Kenn(1) = MenueParam.Normfa(1).NormKenn
        End If


        strueb = strueb & Space(2) & Format(.Rezepte(RzNr).Nr, "00")

        Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, strueb, Graph))
        '
        '
        '
        X = 300
        Y0 = Y
        Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21001), "----------------", Format(.Rezepte(RzNr).Dicke(0), "###0.00")}, Zeil, Graph)
        Y = Y0
        X = X + 70
        Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21000), "----" & Texxt(891 + kwb) & "--------", Format(.Rezepte(RzNr).Sorkrit(0)(KeyRe(1)), "###0.00")}, Zeil, Graph)
        Y = Y0
        X = X + 70
        Call BlockZeil(0, X, Y, dy, {Space(1) & Kenn(0), "----" & Texxt(891 + kwb) & "--------", Format(.Rezepte(RzNr).Sorkrit(0)(KeyRe(5)), "###0.00")}, Zeil, Graph)
        Y = Y0
        X = X + 70
        Call BlockZeil(0, X, Y, dy, {Space(1) & Kenn(1), "----" & Texxt(891 + kwb) & "--------", Format(.Rezepte(RzNr).Sorkrit(0)(KeyRe(6)), "###0.00")}, Zeil, Graph)
        Y = Y0
        X = X + 70
        Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(20999), "----------------", Format(GesDichte, "###0.000")}, Zeil, Graph)
        Y = Y0
        X = X + 70
        Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21003), "----------------", Format(GesPreis, "######0.00")}, Zeil, Graph)

        X = 0.0
        strueb = Sline(255, "-")
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, strueb, Graph))
      Else
        X = 300
        Y = Y + dy
        Y0 = Y
        Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21001), "----------------", Format(.Rezepte(RzNr).Dicke(0), "###0.00")}, Zeil, Graph)
        Y = Y0
        X = X + 70
        Call BlockZeil(0, X, Y, dy, {Space(1) & TexKt(21003), "----------------", Format(GesPreis, "######0.00")}, Zeil, Graph)

        X = 0.0
        strueb = Sline(255, "-")
        Y = Y + dy
        Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, strueb, Graph))
      End If

      Y = Y + dy
      X = 0
      '
      Rzkey(0) = RzNr
      Call AnteilMenge(X, Y, dy, Rzkey, RezSozpt, Zeil, Graph, {"13", "14", "09", "10", "12", "11"})



    End With

  End Sub
  '
  '
  '
  '
  'Ausdruck Sortiment
  '
  '
  '
  '
  '
  '
  Sub DrSorDru(ByRef X0 As Single, ByRef Y As Single, ByRef dy As Single, ByRef RezSozpt As RecipesGrp, ByRef Zeil As ZeilPrint, ByRef Graph As Graphics)
    Dim Scwe As Single
    Dim i As Short
    Dim Y0 As Single
    Dim X As Single
    '
    'Name
    '
    '
    Y = Y + 2 * dy
    strueb = Texxt(824)
    Scwe = 0.39 * Zeil.Width
    X = 0.01 * Zeil.Width
    Y0 = Y + 0.5 * dy
    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, strueb, Graph))

    For i = 0 To RezSozpt.Farben.FarbCount - 1
      Y0 = Y0 + dy
      If Not BitWrt(25, MenueParam.User.Drum) Then
        strueb = RezSozpt.Farben(i).Name
      Else
        strueb = RezSozpt.Farben(i).Aname
      End If
      Zeil.Add(Zeil.DrZeil(0, X, Y0, Scwe, strueb, Graph))
    Next i
    '
    '
    'Kennung Farb-/Bindemittel usw.
    '
    '
    Scwe = 0.1 * Zeil.Width
    X = 0.28 * Zeil.Width
    Y0 = Y + 0.5 * dy
    For i = 0 To RezSozpt.Farben.FarbCount - 1
      Y0 = Y0 + dy
      strueb = TexKt(21020 + RezSozpt.Farben(i).Ichf)
      Zeil.Add(Zeil.DrZeil(2, X, Y0, Scwe, strueb, Graph))
    Next i
    '
    'Preis
    '
    '
    strueb = TexKt(21003)
    X = 0.35 * Zeil.Width
    Y0 = Y + 0.5 * dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))

    For i = 0 To RezSozpt.Farben.FarbCount - 1
      Y0 = Y0 + dy
      strueb = Format(RezSozpt.Farben(i).Preis, "###0.00")
      Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))

    Next i

    '
    'spezifisches Gewicht
    '
    '
    strueb = TexKt(21019)
    X = 0.45 * Zeil.Width
    Y0 = Y + 0.5 * dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))
    For i = 0 To RezSozpt.Farben.FarbCount - 1
      Y0 = Y0 + dy
      strueb = Format(RezSozpt.Farben(i).Spz, "##.000")
      Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
    Next i
    '
    'Farbstaerke
    '
    '
    strueb = TexKt(21014)
    X = 0.55 * Zeil.Width
    Y0 = Y + 0.5 * dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))
    For i = 0 To RezSozpt.Farben.FarbCount - 1
      Y0 = Y0 + dy
      strueb = Format(RezSozpt.Farben(i).Fst, "###.00")
      Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
    Next i
    strueb = Sline(255, "=")
    Y = Y0 + dy
    Zeil.Add(Zeil.DrZeil(2, X0, Y, Zeil.Width, strueb, Graph))

  End Sub
  '
  '
  '
  '
  '
  '
  '
  Sub DruckPicturebox(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Dim LeftPic As Single
    Dim TopPic As Single
    Dim WidthPic As Single
    Dim HeightPic As Single

    Dim dy As Single
    dy = Zeil.FontZ.Height
    Zeil.Graphbounds = New RectangleF(ev.PageBounds.Left, ev.PageBounds.Top, ev.PageBounds.Width, ev.PageBounds.Height)
    LeftPic = ev.PageBounds.Left + 100
    TopPic = ev.PageBounds.Top + 100
    WidthPic = ev.PageBounds.Width - 200
    HeightPic = ev.PageBounds.Height - 200

    '
    'Kurve R-Werte
    '
    'ev.Graphics.DrawRectangle(MnPenObj, LeftPic, TopPic + 0.5F * HeightPic, 0.5F * WidthPic, 0.5F * HeightPic)
    MnPlott.GraphBounds = New RectangleF(LeftPic, TopPic, WidthPic, HeightPic)

    Select Case MnPicNr
      Case 1
        MnPlott.PicREFGraph(sender, ev.Graphics)
      Case 2
        MnPlott.PicLABGraph(sender, ev.Graphics)
      Case 3
      Case 4
        MnPlott.PicXYZGraph(sender, ev.Graphics)
    End Select

  End Sub
  '

  '
  '
  'Ausdruck der Pictureboxes
  '
  '
  '
  '
  '
  '
  '
  Sub DruckPictureboxes(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Dim Texte As String
    Dim x As Single
    Dim y As Single
    Dim LeftPic As Single
    Dim TopPic As Single
    Dim WidthPic As Single
    Dim HeightPic As Single

    Dim dy As Single
    dy = Zeil.FontZ.Height
    Zeil.Graphbounds = New RectangleF(ev.PageBounds.Left, ev.PageBounds.Top, ev.PageBounds.Width, ev.PageBounds.Height)
    LeftPic = 0.05 * Zeil.Width
    TopPic = 4.5 * dy
    WidthPic = 0.9 * Zeil.Width
    HeightPic = Zeil.Height - 200.0

    '
    '
    '
    'Rezeptname
    '
    '
    '
    x = LeftPic
    y = TopPic - 1.5 * dy
    Texte = " " & MnAllRezepte.Rezepte(KeyNam).Name
    If Texte.Length > 40 Then
      Texte = Texte.Substring(0, 40)
    End If
    Texte = Texxt(863) & Texte
    ev.Graphics.DrawString(Texte, Zeil.FontZ, Zeil.BrushZ, x, y)
    '
    '
    'Kunde
    '
    '
    x = LeftPic + 0.5 * WidthPic
    Texte = MnText0
    If Texte.Length > 50 Then
      Texte = Texte.Substring(0, 50)
    End If
    Texte = Texxt(864) & Texte
    ev.Graphics.DrawString(Texte, Zeil.FontZ, Zeil.BrushZ, x, y)
    '
    '
    '
    'Text1   
    '
    '
    x = LeftPic
    y = TopPic + HeightPic + 0.5 * dy
    Texte = MnText1
    If Texte.Length > 100 Then
      Texte = Texte.Substring(0, 100)
    End If
    ev.Graphics.DrawString(Texte, Zeil.FontZ, Zeil.BrushZ, x, y)
    '
    '
    '
    'Datum
    '
    '

    Texte = Texxt(866) & " " & CStr(Today)
    x = LeftPic + WidthPic - ev.Graphics.MeasureString(Texte, Zeil.FontZ).Width
    ev.Graphics.DrawString(Texte, Zeil.FontZ, Zeil.BrushZ, x, y)
    '
    '
    'Text2
    ' 
    '
    '

    Texte = MnText2
    If Texte.Length > 100 Then
      Texte = Texte.Substring(0, 100)
    End If
    x = LeftPic
    y = y + dy
    ev.Graphics.DrawString(Texte, Zeil.FontZ, Zeil.BrushZ, x, y)
    '
    '
    ' Tageszeit
    '
    '
    Texte = Texxt(865) & " " & CStr(TimeOfDay)
    x = LeftPic + WidthPic - ev.Graphics.MeasureString(Texte, Zeil.FontZ).Width
    ev.Graphics.DrawString(Texte, Zeil.FontZ, Zeil.BrushZ, x, y)
    '
    '
    '
    '
    '
    '
    'Boxen
    '
    '
    '
    '
    '
    '
    ev.Graphics.DrawRectangle(MnPenObj, LeftPic, TopPic - 2 * dy, WidthPic, 2 * dy)
    ev.Graphics.DrawRectangle(MnPenObj, LeftPic, TopPic + HeightPic, WidthPic, 3 * dy)


    '
    '
    'Pigmentvorschlag
    '
    ev.Graphics.DrawRectangle(MnPenObj, LeftPic, TopPic, 0.5F * WidthPic, 0.5F * HeightPic)
    MnPlott.GraphBounds = New RectangleF(LeftPic, TopPic, 0.5F * WidthPic, 0.5F * HeightPic)
    MnPlott.PicREZGraph(sender, ev.Graphics)
    '
    '
    '
    'Farbkoordinaten
    '
    ev.Graphics.DrawRectangle(MnPenObj, LeftPic + 0.5F * WidthPic, TopPic, 0.5F * WidthPic, 0.5F * HeightPic)
    MnPlott.GraphBounds = New RectangleF(LeftPic + 0.5F * WidthPic, TopPic, 0.5F * WidthPic, 0.5F * HeightPic)
    MnPlott.picWRTGraph(sender, ev.Graphics)
    '
    '
    '
    'Kurve R-Werte
    '
    ev.Graphics.DrawRectangle(MnPenObj, LeftPic, TopPic + 0.5F * HeightPic, 0.5F * WidthPic, 0.5F * HeightPic)
    MnPlott.GraphBounds = New RectangleF(LeftPic, TopPic + 0.5F * HeightPic, 0.5F * WidthPic, 0.5F * HeightPic)
    MnPlott.PicREFGraph(sender, ev.Graphics)

    '
    '
    'L*,a*,b*-Diagramm
    '
    ev.Graphics.DrawRectangle(MnPenObj, LeftPic + 0.5F * WidthPic, TopPic + 0.5F * HeightPic, 0.5F * WidthPic, 0.5F * HeightPic)
    MnPlott.GraphBounds = New RectangleF(LeftPic + 0.5F * WidthPic, TopPic + 0.5F * HeightPic, 0.5F * WidthPic, 0.5F * HeightPic)
    MnPlott.PicLABGraph(sender, ev.Graphics)

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
    ev.HasMorePages = False

  End Sub

  '
  '
  '
  '
  '
  '
  '
  Sub DruckKorrekturRezept(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    If Zeil.RecPrintCount = 0 Then
      'GraphBounds = New RectangleF(ev.MarginBounds.Left, ev.MarginBounds.Top, ev.MarginBounds.Width, ev.MarginBounds.Height)
      Zeil.Graphbounds = New RectangleF(GrLeft, GrTop, GrWidth, GrHeight)

      Call DruKorrRez(MnKeyNam, MnRzName, MnRefNr, MnAllRezepte, MnGrpRwerte, Zeil, ev.Graphics)
    End If
    Call Zeil.DruckEventRecprint(sender, ev)
  End Sub
  '
  '
  '
  '
  '
  '
  '
  'Ausdruck des Wiegeschein - Rezeptes (MnKeynam)
  '
  Sub DruckWiegeschein(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    If Zeil.RecPrintCount = 0 Then
      Zeil.Graphbounds = New RectangleF(GrLeft, GrTop, GrWidth, GrHeight)
      Call DruWiegRez(MnKeyNam, MnRefNr, Zeil, ev.Graphics)
    End If
    Call Zeil.DruckEventRecprint(sender, ev)
  End Sub
  '

  '
  '
  Sub DruWiegRez(ByRef RzNr As String, ByRef RefNr() As String, ByRef Zeil As ZeilPrint, ByVal Graph As Graphics)
    Dim X As Single
    Dim Y As Single
    Dim kwb As Short
    '
    '
    Call DrRezKopf(X, Y, dy, Zeil, Graph)
    If ier <> 0 Then Exit Sub
    Y = Y + dy
    '
    '
    'Rezeptname
    '
    '
    strueb = Texxt(888) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

    strueb = Trim(MnAllRezepte.Rezepte(RzNr).Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    '
    'Vorlage
    '
    '
    strueb = Texxt(861) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

    strueb = Trim(GrpRwerte(0)("V").Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    '
    'Untergrund
    '
    '
    If GrpRwerte.Count > 0 Then
      strueb = Texxt(890) & ":"
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

      strueb = Trim(GrpRwerte(0).RefUnt.Name)
      Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    End If
    '
    'Rezept Drucken
    '
    '
    '
    kwb = GrpRwerte(0)(0).kwb
    Call DrRezDru(2, X, Y, dy, RzNr, kwb, MnAllRezepte, Zeil, Graph)


    '
    MakeFarbWrt(X, Y, dy, Texxt(875), RzNr, RefNr, MnGrpRwerte, MnFarbWerte, Zeil, Graph)
    '
    '
    If Y + 7 * dy > Zeil.Height Then
      Zeil.Add(Zeil.NewPage)
      Y = 0
    End If
    Y = Y + 4 * dy
    Call DruckParameter(Y, Zeil.Width, dy, Zeil, Graph)
    '
    '
  End Sub
  '
  '
  '
  '
  '
  'Ausdruck der Rezeptkorrektur - Rezepte (RzNr)
  '
  '
  '
  '
  Sub DruKorrRez(ByRef RzNr As String, ByRef MnRzName() As String, ByRef RefNr() As String, ByRef RezSozpt As RecipesGrp, ByRef GrpRwerte As RefValuesGrp, ByRef Zeil As ZeilPrint, ByRef Graph As Graphics)
    Dim i As Integer
    Dim X As Single
    Dim Y As Single
    Dim dx As Single
    Dim ReefNr(1) As String
    Dim Ubo As Short
    Dim kwb As Short
    '
    '
    Call DrRezKopf(X, Y, dy, Zeil, Graph)
    If ier <> 0 Then Exit Sub
    Y = Y + dy
    '
    '
    'Rezeptname
    '
    '
    strueb = Texxt(888) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

    strueb = Trim(RezSozpt.Rezepte(RzNr).Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))
    For i = 0 To RezSozpt.Rezepte.RezCount - 1
      If IsNumeric(RezSozpt.Rezepte.RezKey(i)) Then
        Y = Y + dy
        strueb = Trim(RezSozpt.Rezepte(RezSozpt.Rezepte.RezKey(i)).Name)
        Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))
      End If

    Next i

    '
    '
    'Vorlage
    '
    '
    strueb = Texxt(861) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

    strueb = Trim(GrpRwerte(0)("V").Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    '
    'Nachstellung
    '
    '
    strueb = Texxt(786) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

    strueb = Trim(GrpRwerte(0)("N").Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    '
    'Untergrund
    '
    '
    If GrpRwerte.Count > 0 Then
      strueb = Texxt(890) & ":"
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

      strueb = Trim(GrpRwerte(0).RefUnt.Name)
      Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    End If
    '
    'Rezept Drucken
    '
    '
    '
    kwb = GrpRwerte(0)(0).kwb
    Call DrKorrDru(X, Y, dx, dy, kwb, RezSozpt, Zeil, Graph)

    Try
      Ubo = UBound(RefNr)
      '
      If Ubo = 2 Then
        ReefNr(0) = RefNr(0)
        ReefNr(1) = RefNr(2)
        MakeFarbWrt(X, Y, dy, Texxt(875) & " (" & Texxt(877) & ")", MnRzName(0), ReefNr, MnGrpRwerte, MnFarbWerte, Zeil, Graph)
        ReefNr(0) = RefNr(0)
        ReefNr(1) = RefNr(1)
        MakeFarbWrt(X, Y, dy, Texxt(875) & " (" & Texxt(876) & ")", MnRzName(1), ReefNr, MnGrpRwerte, MnFarbWerte, Zeil, Graph)
      ElseIf Ubo = 1 Then
        MakeFarbWrt(X, Y, dy, Texxt(875), MnRzName(0), RefNr, MnGrpRwerte, MnFarbWerte, Zeil, Graph)
      End If
      '
      '
      '
      '
    Catch

    End Try
    If Y + 7 * dy > Zeil.Height Then
      Zeil.Add(Zeil.NewPage)
      Y = 0
    End If
    Y = Y + 4 * dy
    Call DruckParameter(Y, dx, dy, Zeil, Graph)
    Y = Y + 4 * dy
    strueb = RezSozpt.Comment
    Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, strueb, Graph))
  End Sub
  '
  '

  '
  '
  '
  'Ausdruck aller Rezepte
  '
  '
  Sub DruckRezAlle(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    If Zeil.RecPrintCount = 0 Then
      Zeil.Graphbounds = New RectangleF(GrLeft, GrTop, GrWidth, GrHeight)
      Call DruRezAll(Zeil, ev.Graphics)
    End If
    Call Zeil.DruckEventRecprint(sender, ev)
  End Sub

  '
  '
  '
  Sub DruRezAll(ByRef Zeil As ZeilPrint, ByRef Graph As Graphics)
    Dim strueb As String
    Dim X As Single
    Dim Y As Single
    Dim k As Integer
    Dim ier As Short
    Dim kwb As Short
    '
    '
    Call DrRezKopf(X, Y, dy, Zeil, Graph)
    If ier <> 0 Then Exit Sub
    '
    'Sortimentname
    '
    '
    strueb = Texxt(850) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

    strueb = MnAllRezepte.Rezepte("SOR").Name
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    '
    '
    '
    'Vorlage
    '
    '
    strueb = Texxt(861) & ":"
    Y = Y + dy
    Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

    strueb = Trim(GrpRwerte(0)("V").Name)
    Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    '
    'Untergrund
    '
    '
    If GrpRwerte.Count > 0 Then
      strueb = Texxt(890) & ":"
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(1, X, Y, 0.12 * Zeil.Width, strueb, Graph))

      strueb = Trim(GrpRwerte(0).RefUnt.Name)
      Zeil.Add(Zeil.DrZeil(0, X + 0.125 * Zeil.Width, Y, Zeil.Width, strueb, Graph))

    End If

    Y = Y + dy
    Call DrSorDru(X, Y, dy, MnAllRezepte, Zeil, Graph)
    Y = Y + 3 * dy

    RefNam(0) = "V"
    NlzMax = MenueParam.Normfa.Nlz
    WinMax = MenueParam.User.Winkel.Km

    For k = 0 To MnPlott.Ksrt.Count - 1
      RezNam = KeyRe(MnPlott.Ksrt(k))
      RefNam(1) = RezNam
      '
      If (Y + 10 * dy + Winkel.Km * MenueParam.Normfa.Nlz * dy + MnAllRezepte.Rezepte(RezNam).KF * dy) > Zeil.Height Then
        Zeil.Add(Zeil.NewPage)
        Y = 0
      End If
      '
      '
      '
      'Rezept drucken
      '
      '
      '
      '
      '
      If Y + ((NlzMax + 3) * WinMax) * dy + MnAllRezepte.Rezepte(0).KF * dy > Zeil.Height Then
        Zeil.Add(Zeil.NewPage)
        Y = 0
      End If

      Y = Y + 2 * dy
      kwb = GrpRwerte(0)(0).kwb
      Call DrRezDru(1, X, Y, dy, RezNam, kwb, MnAllRezepte, Zeil, Graph)
      '
      '
      '
      MakeFarbWrt(X, Y, dy, Texxt(875), RezNam, RefNam, MnGrpRwerte, MnFarbWerte, Zeil, Graph)

    Next k

  End Sub
  Sub MatrixDru(ByRef Ialign As Short, ByRef X As Single, ByRef Mrkstart As Single, ByRef Y As Single, ByRef dx As Single, ByRef dy As Single, ByRef ZMatStart As Integer, ByRef ZmatEnde As Integer, ByRef SMatStart As Integer, SmatEnde As Integer, ByRef Vmat(,) As Object, ByRef Graph As Graphics, ByVal AltRow As Boolean)
    Dim tex As String
    Dim j As Short
    Dim i As Short
    Dim Xact As Single
    Dim jj As Integer
    For i = ZMatStart To ZmatEnde
      Y = Y + dy
      If Y + 2 * dy > Zeil.Height Then
        Zeil.Add(Zeil.NewPage)
        Y = 0
      End If
      For j = SMatStart To SmatEnde
        tex = Vmat(j, i)
        If j = 0 Then
          If i Mod 2 = 0 And AltRow Then
            Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, tex, Graph, New RectangleF(Zeil.Left, Y + Zeil.Top, Zeil.Width, dy), Brushes.LightGray))
          Else
            Zeil.Add(Zeil.DrZeil(0, X, Y, Zeil.Width, tex, Graph))
          End If
        Else
          jj = j - SMatStart
          Xact = X + Mrkstart + (j - 1) * dx
          Zeil.Add(Zeil.DrZeil(Ialign, Xact, Y, dx, tex, Graph))
        End If

      Next j
    Next i
  End Sub

  Sub DruIni(ByRef DruChar As Short, ByRef MrkAnz As Short, ByRef Mrkstart As Single, ByRef X As Single, ByRef Y As Single, ByRef dx As Single, ByRef dy As Single, ByRef Zeil As ZeilPrint, ByVal ev As PrintPageEventArgs)
    Dim ScWeit As Single
    If DruChar = 0 Then
      '
      '12 Merkmale/Zeile
      '



      X = 0.005 * ev.PageBounds.Width
      Y = 0.005 * ev.PageBounds.Height
      MrkAnz = 15
    Else
      '
      '8 Merkmale/Zeile
      '

      X = 0.05 * ev.PageBounds.Width
      Y = 0.005 * ev.PageBounds.Height

      MrkAnz = 8
    End If
    dx = ev.Graphics.MeasureString("OOOOO", Zeil.FontZ).Width
    dy = 16.0

    ScWeit = 0.99 * ev.PageBounds.Width

    Mrkstart = ev.Graphics.MeasureString("OOOOOOOOOOOOOOOOOOO", Zeil.FontZ).Width
  End Sub


  Sub DruFawert(ByRef X As Single, ByRef Y As Single, ByRef FaTxt As String, ByRef Wrt As Single, ByRef Zeil As ZeilPrint, ByRef Graph As Graphics)
    strueb = FaTxt & "="
    Zeil.Add(Zeil.DrZeil(0, X, Y, 0.06 * Zeil.Width, strueb, Graph))

    strueb = Format(Wrt, "##0.00")
    Zeil.Add(Zeil.DrZeil(1, X + 0.06 * Zeil.Width, Y, 0.065 * Zeil.Width, strueb, Graph))


  End Sub

  Sub MakeFarbWrt(ByRef X As Single, ByRef Y As Single, ByRef dy As Single, ByRef Teext As String, ByRef RezNam As String, ByRef RefNr() As String, ByRef MnGrpRwerte As RefValuesGrp, ByRef FarbWerte As ValuesGrpsAssigns, ByRef Zeil As ZeilPrint, ByVal Graph As Graphics)
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim l As Integer
    Dim ier As Short
    Dim acht As Integer = 8
    Dim Mkwb As Short
    Dim VKwb(1) As Short
    Dim Ueb() As String
    Dim KEN() As String
    Dim DELX As Single
    Dim XX As Single
    Dim Value As Object
    Dim Mrez As Integer
    Dim AnzMerk As Integer
    If RefNr.Count > 2 Then
      MsgBox("Error MakeFarbWrt ")
      Exit Sub
    End If
    If MnGrpRwerte(0).Count > 0 Then
      Mkwb = MnGrpRwerte.Count
      For i = 0 To Mkwb - 1
        VKwb(i) = MnGrpRwerte(i)(0).kwb
      Next i
    End If
    Call quali.FarbwerteCalc(RefNr, MenueParam.User.Winkel, GrpRwerte, FarbWerte, MnIer)
    If FarbWerte(0).Count = 2 Then
      Mrez = 1
    Else
      Mrez = 2
    End If

    ReDim Ueb(FarbWerte(0).CountMerk - 1)
    ReDim KEN(FarbWerte(0).CountMerk - 1)
    For k = 0 To Ueb.Count - 1
      Ueb(k) = Trim(FarbWerte(0).Merk(k).Kbez)
      KEN(k) = FarbWerte(0).Merk(k).Ken
    Next
    '
    NlzMax = FarbWerte(0)(0).Count
    WinMax = MenueParam.User.Winkel.Km

    '
    'Kontrast DE nach FarbWerte(0)(NumRw) übernehmen
    '

    If FarbWerte(0).Count > Mrez + 1 Then
      '
      'KDE Kontrastfarbabstand von Rechnung über Schwarz nach Rechnung über Weiß übertragen
      '
      For k = 0 To FarbWerte(0).CountMerk - 1
        If FarbWerte(0).MerkKey(k) = "BT" Then
          For i = 0 To NlzMax - 1
            For j = 0 To WinMax - 1
              FarbWerte(0)(Mrez)(i)(j)("BT") = FarbWerte(0)(Mrez + 1)(i)(j)("BT")
            Next j
          Next i
          Exit For
        End If
      Next k
      '
      'KDEmit Mittelwert des Kontrstfarbabstandes von Rechnung über Schwarz nach Rechnung über Weiß übertragen
      'für Winkel 45
      '
      For k = 0 To FarbWerte(0).CountMerk - 1
        If FarbWerte(0).MerkKey(k) = "QK" Then
          For i = 0 To NlzMax - 1
            For j = 0 To WinMax - 1
              FarbWerte(0)(Mrez)(i)(j)("QK") = FarbWerte(0)(Mrez + 1)(i)(j)("QK")
            Next j
          Next i
          Exit For
        End If
      Next k
    End If
    'Drucken
    '
    '
    '
    '
    If Y + ((NlzMax + 3) * WinMax) * dy > Zeil.Height Then
      Zeil.Add(Zeil.NewPage)
      Y = 0
    End If
    '
    '
    '
    'Überschrift Merkmale
    '
    For l = 0 To Fix((Ueb.Count - 1) / 8)
      AnzMerk = Min(Ueb.Count - l * 8, 8)
      XX = X
      DELX = 0.09 * Zeil.Width
      XX = 0.19 * Zeil.Width + DELX
      Y = Y + dy
      strueb = Teext
      Zeil.Add(Zeil.DrZeil(0, 0.0, Y, 0.5 * Zeil.Width, strueb, Graph))
      For k = 0 To AnzMerk - 1
        Zeil.Add(Zeil.DrZeil(2, XX, Y, DELX, Ueb(k + l * 8), Graph))
        XX = XX + DELX
      Next k

      ' Y = Y + dy
      ' strueb = Teext & " (" & MenueParam.User.Winkel(j).Chrm & ")"
      ' Zeil.Add(Zeil.DrZeil(0, X, Y, 0.5 * Zeil.Width, strueb, Graph))
      ' Y = Y + dy
      'Überschrift Merkmale
      '

      '
      '
      'Normlichtarten und Merkmale
      '

      For i = 0 To NlzMax - 1
        XX = X
        For j = 0 To WinMax - 1
          If l = 0 Then
            If Not (IsNothing(MnAllRezepte.Rezepte(RezNam).Sorkrit(0)) OrElse MnAllRezepte.Rezepte(RezNam).Sorkrit(0).Count = 0) Then
              'Preis
              If FarbWerte(0).ContainsMerk("@P") Then
                FarbWerte(0)(Mrez)(i)(j)("@P") = Format(MnAllRezepte.Rezepte(RezNam).Sorkrit(0)(2), FarbWerte(0).Merk("@P").Form)
              End If
              'Sensibilität
              If FarbWerte(0).ContainsMerk("@S") Then
                FarbWerte(0)(Mrez)(i)(j)("@S") = Format(MnAllRezepte.Rezepte(RezNam).Sorkrit(0)(12), FarbWerte(0).Merk("@S").Form)
              End If
              'Korrigierbarkeit
              If FarbWerte(0).ContainsMerk("@R") Then
                FarbWerte(0)(Mrez)(i)(j)("@R") = Format(MnAllRezepte.Rezepte(RezNam).Sorkrit(0)(3), FarbWerte(0).Merk("@R").Form)
              End If
            End If
          End If
          Y = Y + dy
          strueb = MenueParam.Normfa(i).NormNama & Space(1) & Texxt(891 + VKwb(0)) & " (" & MenueParam.User.Winkel(j).Chrm & ")"
          Zeil.Add(Zeil.DrZeil(0, X, Y, 0.25 * Zeil.Width, strueb, Graph))
          XX = 0.19 * Zeil.Width + DELX
          For k = 0 To AnzMerk - 1
            Value = FarbWerte(0)(Mrez)(i)(j)(KEN(k + l * 8))
            If Not IsDBNull(Value) Then
              Zeil.Add(Zeil.DrZeil(1, XX, Y, DELX, Value, Graph))
            End If
            XX = XX + DELX
          Next k
        Next j
      Next i
      strueb = Sline(255, "-")
      Y = Y + dy
      Zeil.Add(Zeil.DrZeil(2, X, Y, Zeil.Width, strueb, Graph))

    Next l
  End Sub
  '
  '
  '
  '
  'Ausdruck der PictureBoxes auf dem Drucker 
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
  Sub RezDru(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    Dim Xst As Single
    Dim Yst As Single
    Dim Bst As Single
    Dim Texte As String
    'ev.PageSettings.Landscape()
    '
    'Querformat
    '
    Try
      MnPenObj.DashStyle = DashStyle.Solid

      Bst = 0.38 * Zeil.Width + 0.38 * Zeil.Height
      Xst = 0.5 * (Zeil.Width - Bst)
      Yst = 0.05 * Zeil.Height
      '

      'Farbvorlage
      Texte = " " & MnAllRezepte.Rezepte(KeyNam).Name
      If Len(Texte) > 24 Then
        Texte = Texte.Substring(1, 24)
      End If
      ev.Graphics.DrawString(Texxt(863) & Texte, Zeil.FontZ, Zeil.BrushZ, Xst + 0.05 * Bst, Yst)
      '
      '
      'Kunde
      '

      Texte = " " & MnTextEIN_0
      If Len(Texte) > 32 Then
        Texte = Texte.Substring(1, 32)
      End If
      ev.Graphics.DrawString(Texxt(864) & Texte, Zeil.FontZ, Zeil.BrushZ, Xst + 0.5 * Bst, Yst)

      'Unterschrift (1)
      '
      '
      Texte = MnTextEIN_1
      If Len(Texte) > 50 Then
        Texte = Texte.Substring(1, 50)
      End If
      ev.Graphics.DrawString(Texte, Zeil.FontZ, Zeil.BrushZ, Xst + 0.05 * Bst, Yst + 0.79 * Zeil.Height)

      '
      'Datum
      '
      '
      Texte = Texxt(866) & " " & CStr(Today)
      ev.Graphics.DrawString(Texte, Zeil.FontZ, Zeil.BrushZ, Xst + Bst - ev.Graphics.MeasureString(Texte, Zeil.FontZ).Width, Yst + 0.79 * Zeil.Height)

      '
      '
      'Unterschrift (2)
      '
      '
      Texte = MnTextEIN_2
      If Len(Texte) > 50 Then
        Texte = Texte.Substring(1, 50)
      End If
      ev.Graphics.DrawString(Texte, Zeil.FontZ, Zeil.BrushZ, Xst + 0.05 * Bst, Yst + 0.82 * Zeil.Height)

      '
      'Zeit
      '
      '
      Texte = Texxt(865) & " " & CStr(TimeOfDay) & "  "
      ev.Graphics.DrawString(Texte, Zeil.FontZ, Zeil.BrushZ, Xst + Bst - ev.Graphics.MeasureString(Texte, Zeil.FontZ).Width, Yst + 0.82 * Zeil.Height)
      '
      Call MnPlott.PicREZGraph(sender, ev.Graphics)
      Call MnPlott.picWRTGraph(sender, ev.Graphics)

      Call MnPlott.PicREFGraph(sender, ev.Graphics)

      Call MnPlott.PicLABGraph(sender, ev.Graphics)

      '
      'Boxen
      '
      ev.Graphics.DrawRectangle(MnPenObj, New Rectangle(Xst, Yst - 0.02F * Zeil.Height, Bst, 0.04 * Zeil.Height))
      ev.Graphics.DrawRectangle(MnPenObj, New Rectangle(Xst, Yst + 0.02F * Zeil.Height, 0.38 * Zeil.Width, 0.38 * Zeil.Height))
      ev.Graphics.DrawRectangle(MnPenObj, New Rectangle(Xst + 0.38 * Zeil.Width, Yst + 0.02 * Zeil.Height, 0.38 * Zeil.Width, 0.38 * Zeil.Height))
      ev.Graphics.DrawRectangle(MnPenObj, New Rectangle(Xst, Yst + 0.4 * Zeil.Height, 0.38 * Zeil.Width, 0.38 * Zeil.Height))
      ev.Graphics.DrawRectangle(MnPenObj, New Rectangle(Xst, Yst + 0.78 * Zeil.Height, Bst, 0.08 * Zeil.Height))
      ev.HasMorePages = False
    Catch ex As Exception
      MsgBox(ex.ToString)
    End Try

  End Sub
  Sub MngUmrech(ByVal keyMeng As String)
    Dim k As Short
    If keyMeng <> "" Then
      Umr.CalcBamng(keyMeng, MnAllRezepte, ier)
    Else
      For k = 0 To MnPlott.Ksrt.Count - 1
        Umr.CalcBamng(KeyRe(k), MnAllRezepte, ier)
      Next k
    End If
  End Sub

  Function MngSum(ByVal keyMenge As String) As Single
    '
    '
    Dim i As Short
    Dim k As Short
    If keyMenge = "" Then Exit Function
    For k = 0 To MnAllRezepte.Rezepte(keyMenge).KF - 1
      If Abs(MnAllRezepte.Rezepte(keyMenge)(k).BaAmng + 999.99) > 0.001 Then
        Exit For
      End If
    Next k
    If k > MnAllRezepte.Rezepte(keyMenge).KF Then
      MngSum = -999.99
    Else
      MngSum = 0.0#
      For i = 0 To MnAllRezepte.Rezepte(keyMenge).KF
        MngSum = MngSum + MnAllRezepte.Rezepte(keyMenge)(i).BaAmng
      Next i
    End If
  End Function
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
  Public Sub New()
    PrintPreview = New PrintPreviewDialog
    Printdoc = New PrintDocument
    Printdoc.PrinterSettings = MnPrintset
    PrintPreview.Document = Printdoc


    MnColFarb = Color.Ivory

    quali = New QualKontrolle
    Umr = New RezeptUmrechnung

    quali = New QualKontrolle
    MnDrPar = True
    MnFarbWerte = New ValuesGrpsAssigns
    MnText0 = ""
    MnText1 = ""
    MnText2 = ""

    tiny = 1.0E-30


    Zeil = New ZeilPrint
    Zeil.FontZ = RezFont(2)
    Zeil.BrushZ = New SolidBrush(Color.Black)
    dy = Zeil.FontZ.Height
    Disposed = False
    MnJProz = True
    MnJProb = True
    MnJStae = True
    MnJSpez = True
    '
    '
    MnZusatzName = New List(Of String)
    MnZusatzMenge = New List(Of Single)
    MnInformationsName = New List(Of String)
    MnInformationsBez = New List(Of String)
  End Sub

  Sub DruckParameter(ByRef Y0 As Single, ByRef DX As Single, ByRef DY As Single, ByRef Zeil As ZeilPrint, ByRef Graph As Graphics)
    Dim i As Integer
    Dim X As Single
    Dim Y As Single
    Dim DELX As Single
    Dim Wrt As String
    Dim Strueb As String
    DELX = DX
    Y = Y0

    '
    'Gewichtsfaktoren Normlichtarten
    '
    '
    '
    Wrt = ":"
    For i = 0 To MenueParam.Normfa.Nlz - 1
      Wrt = Wrt & "  " & Format(MenueParam.Normfa(i).NormGew, "##0.00") & ";"
    Next i
    Mid(Wrt, Len(Wrt), 1) = " "
    Strueb = "  " & Texxt(446) & " (" & Texxt(312) & ")" & Wrt
    Zeil.Add(Zeil.DrZeil(0, X, Y, DELX, Strueb, Graph))
    Y = Y + DY
    '
    'Gewichtsfaktoren DL*,DC*,DH*
    '
    '
    '
    Wrt = ":"
    For i = 0 To 2
      Select Case i
        Case 0
          Wrt = Wrt & "  " & Format(MenueParam.Menue.Lgew, "##0.00") & ";"
        Case 1
          Wrt = Wrt & "  " & Format(MenueParam.Menue.Cgew, "##0.00") & ";"
        Case 2
          Wrt = Wrt & "  " & Format(MenueParam.Menue.Hgew, "##0.00") & ";"
      End Select
    Next i
    Mid(Wrt, Len(Wrt), 1) = " "
    Strueb = "  " & Texxt(446) & " (DL,DC,DH)" & Wrt
    Zeil.Add(Zeil.DrZeil(0, X, Y, DELX, Strueb, Graph))
    Y = Y + DY
    '
    'Gewichtsfaktoren Winkel
    '
    '
    '
    Wrt = ":"
    For i = 0 To MenueParam.User.Winkel.Km - 1
      Wrt = Wrt & "  " & Format(MenueParam.User.Winkel(i).IhrmGew, "##0.00") & ";"
    Next i
    Mid(Wrt, Len(Wrt), 1) = " "
    Strueb = "  " & Texxt(446) & " (" & Texxt(153) & ")" & Wrt
    Zeil.Add(Zeil.DrZeil(0, X, Y, DELX, Strueb, Graph))
    Y = Y + DY
    '
    '
    '
    'Zusatzinformationen
    '
    '
    '
    '
    For i = 0 To MnInformationsName.Count - 1
      If MnInformationsBez(i) <> "" Then
        Y = Y + DY
        Strueb = MnInformationsName(i) & ":"
        Zeil.Add(Zeil.DrZeil(0, X, Y, DELX, Strueb, Graph))
        Strueb = MnInformationsBez(i)
        Zeil.Add(Zeil.DrZeil(0, X + 200, Y, DELX, Strueb, Graph))
      End If
    Next
  End Sub
  '
  '
  '
  Sub DruckenAllgemein(Isteu As Integer)
    MnIsteu = Isteu
    '
    '

    '
    '
    '
    '
    '
    Select Case Isteu
      Case 0
        MnPrintset.DefaultPageSettings.Landscape = False
      Case (1)
        MnPrintset.DefaultPageSettings.Landscape = True
    End Select
    If Not BitWrt(0, MenueParam.User.Druq) Then
      MnPrintset.DefaultPageSettings.Landscape = True
    End If
    GrWidth = 750
    GrHeight = 1137
    If MnPrintset.DefaultPageSettings.Landscape Then
      GrWidth = 1137
      GrHeight = 750
    End If

    '
    '
    Printdoc.PrinterSettings = MnPrintset
    PrintPreview.WindowState = FormWindowState.Maximized
    PrintPreview.ShowDialog()

  End Sub

  Sub AnteilMenge(ByRef X0 As Single, ByRef Y As Single, ByRef dy As Single, ByVal RzKey() As String, ByRef RezSozpt As RecipesGrp, ByRef Zeil As ZeilPrint, ByRef Graph As Graphics, Drubild() As String)
    Dim sumzusa As Single
    Dim sumzusm As Single
    Dim zus As Single
    Dim sum As Single
    Dim i As Short
    Dim j As Short
    Dim k As Short
    Dim MxMe As Short
    Dim Y0 As Single
    Dim YZ As Single
    Dim Yzus As Single
    Dim Scwe As Single
    Dim X As Single
    Dim dx As Single
    Dim GesPigm() As Single
    Dim GesPast() As Single
    '
    '
    MxMe = UBound(RzKey) + 1
    ReDim GesPigm(MxMe - 1)
    ReDim GesPast(MxMe - 1)
    For j = 0 To MxMe - 1
      GesPigm(j) = 0.0#
      GesPast(j) = 0.0
      For i = 0 To RezSozpt.Rezepte(RzKey(j)).KF - 1
        FaId = RezSozpt.Rezepte(RzKey(j))(i).ID
        KeyD = KeyName(FaId)
        GesPigm(j) = GesPigm(j) + RezSozpt.Rezepte(RzKey(j))(i).FaAmng
        GesPast(j) = GesPast(j) + RezSozpt.Rezepte(RzKey(j))(i).BaAmng
      Next i
      GesPigm(j) = GesPigm(j) / 100.0
      GesPast(j) = GesPast(j) / 100.0
    Next j
    '
    '
    X = X0
    Yzus = ZusatzMenge.Count * dy
    '
    '
    '
    '
    '
    'Name
    '
    '
    '
    Scwe = 0.0
    X = 0.05 * Zeil.Width
    Y0 = Y + 0.5 * dy
    For i = 0 To RezSozpt.Rezepte(RzKey(0)).KF - 1
      Y0 = Y0 + dy
      FaId = RezSozpt.Rezepte(RzKey(0))(i).ID
      KeyD = KeyName(FaId)
      If Not BitWrt(25, MenueParam.User.Drum) Then
        strueb = RezSozpt.Farben(KeyD).Name
      Else
        strueb = RezSozpt.Farben(KeyD).Aname
      End If
      If strueb.Count > 30 Then
        strueb = strueb.Substring(0, 30)
      End If
      If i Mod 2 = 1 Then
        Zeil.Add(Zeil.DrZeil(0, X, Y0, Zeil.Width, strueb, Graph, New RectangleF(Zeil.Left, Y0 + Zeil.Top, Zeil.Width, dy), Brushes.LightGray))
      Else
        Zeil.Add(Zeil.DrZeil(0, X, Y0, Zeil.Width, strueb, Graph))
      End If

    Next i
    For i = 0 To ZusatzName.Count - 1
      Y0 = Y0 + dy
      strueb = ZusatzName(i)
      If strueb.Count > 30 Then
        strueb = strueb.Substring(0, 30)
      End If
      Zeil.Add(Zeil.DrZeil(0, X, Y0, Zeil.Width, strueb, Graph, New RectangleF(Zeil.Left, Y0 + Zeil.Top, Zeil.Width, dy), Brushes.AntiqueWhite))
    Next i
    '
    '
    '
    'Kennung Farb-/Bindemittel usw.
    '
    '
    '
    Scwe = 0.1 * Zeil.Width
    X = 0.3 * Zeil.Width
    Y0 = Y + 0.5 * dy
    For i = 0 To RezSozpt.Rezepte(RzKey(0)).KF - 1
      FaId = RezSozpt.Rezepte(RzKey(0))(i).ID
      KeyD = KeyName(FaId)
      Y0 = Y0 + dy
      strueb = TexKt(21020 + RezSozpt.Farben(KeyD).Ichf) + RezSozpt.Farben(KeyD).OP
      Zeil.Add(Zeil.DrZeil(2, X, Y0, Scwe, strueb, Graph))

    Next i
    '
    '
    If Drubild.Count = 0 Then Exit Sub
    YZ = Y + 0.5 * dy
    X = 0.275 * Zeil.Width
    dx = 8.0 * 0.075 * Zeil.Width / Min(Drubild.Count, 8)
    For k = 0 To Min(Drubild.Count, 8) - 1
      '
      '
      '

      Y0 = Y + 0.5 * dy
      Select Case Drubild(k)
        '
        '
        'Anteil (alt) Pigment
        '
        '
        Case "01", "13"
          X = X + dx
          If Drubild(k) = "01" Then
            strueb = TexKt(21004) & "(PI)"
          Else
            strueb = Texxt(823)
          End If
          Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))
          If GesPigm(0) > 0.0 Then
            For i = 0 To RezSozpt.Rezepte(RzKey(0)).KF - 1
              Y0 = Y0 + dy
              strueb = Format(RezSozpt.Rezepte(RzKey(0))(i).FaAmng / GesPigm(0), "###0.000")
              Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
            Next i
          End If
          '
          '
          '
          sumzusa = 0.0
          For i = 0 To ZusatzMenge.Count - 1
            Y0 = Y0 + dy
            zus = ZusatzMenge(i) / GesPigm(0)
            sumzusa = sumzusa + zus
            strueb = Format(zus, "###.000")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
          Next i
          '

          Y0 = Y0 + 2 * dy
          sum = 100 + sumzusa
          strueb = Format(sum, "####.00")
          Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
          Y0 = Y0 - 2 * dy
        Case "02"
          '
          'Anteil (neu) Pigment
          '
          '
          If MxMe > 1 Then
            If RezSozpt.Rezepte(RzKey(1)).KF > 0 Then
              X = X + dx
              Y0 = Y + 0.5 * dy
              strueb = TexKt(21005) & "(PI)"
              Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))
              If GesPigm(1) > 0.0 Then
                For i = 0 To RezSozpt.Rezepte(RzKey(1)).KF - 1
                  Y0 = Y0 + dy
                  strueb = Format(RezSozpt.Rezepte(RzKey(1))(i).FaAmng / GesPigm(1), "###0.000")
                  Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
                Next i
              End If
            End If

            '
            '
            Y0 = Y0 + 2 * dy
            sum = 100
            strueb = Format(sum, "####.00")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
            Y0 = Y0 - 2 * dy
          End If
          '
          '
          '
          '
          '
          '
          'Anteil (alt) Paste
          '
          '
        Case "03"

          If GesPast(0) > 0.0 Then
            X = X + dx
            strueb = TexKt(21004)
            Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))
            For i = 0 To RezSozpt.Rezepte(RzKey(0)).KF - 1
              Y0 = Y0 + dy
              strueb = Format(RezSozpt.Rezepte(RzKey(0))(i).BaAmng / GesPast(0), "###0.000")
              Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
            Next i
          End If
          '
          '
          '
          sumzusa = 0.0
          For i = 0 To ZusatzMenge.Count - 1
            Y0 = Y0 + dy
            zus = ZusatzMenge(i) / GesPast(0)
            sumzusa = sumzusa + zus
            strueb = Format(zus, "###.000")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
          Next i
          '

          Y0 = Y0 + 2 * dy
          sum = 100 + sumzusa
          strueb = Format(sum, "####.00")
          Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
          Y0 = Y0 - 2 * dy
        Case "04"
          '
          'Anteil (neu) Paste
          '
          '
          If MxMe > 1 Then
            If RezSozpt.Rezepte(RzKey(1)).KF > 0 Then
              X = X + dx
              strueb = TexKt(21005)
              Y0 = Y + 0.5 * dy
              Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))
              If GesPast(1) > 0.0 Then
                For i = 0 To RezSozpt.Rezepte(RzKey(1)).KF - 1
                  Y0 = Y0 + dy
                  strueb = Format(RezSozpt.Rezepte(RzKey(1))(i).BaAmng / GesPast(1), "###0.000")
                  Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
                Next i
              End If
            End If

            '
            '
            Y0 = Y0 + 2 * dy
            sum = 100
            strueb = Format(sum, "####.00")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
            Y0 = Y0 - 2 * dy
          End If
          '
          '
          '
        Case "05", "14"
          '
          '
          '
          'Menge(alt)
          '
          '
          X = X + dx
          Y0 = Y + 0.5 * dy
          If Drubild(k) = "05" Then
            strueb = TexKt(21006)
          Else
            strueb = Texxt(816)
          End If
          Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))

          For i = 0 To RezSozpt.Rezepte(RzKey(0)).KF - 1
            FaId = RezSozpt.Rezepte(RzKey(0))(i).ID
            KeyD = KeyName(FaId)
            Y0 = Y0 + dy
            strueb = Format(RezSozpt.Rezepte(RzKey(0))(i).BaAmng, Trim(RezSozpt.Farben(KeyD).Form))
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
          Next i
          '
          '
          '
          'ZusatzMenge
          '
          '
          '
          '
          sumzusm = 0.0
          For i = 0 To ZusatzMenge.Count - 1
            Y0 = Y0 + dy
            sumzusm = sumzusm + ZusatzMenge(i)
            strueb = Format(ZusatzMenge(i), "###.000")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
          Next i
          '
          '
          '
          'Summe
          '
          '
          Y0 = Y0 + 2 * dy
          sum = sumzusm
          For i = 0 To RezSozpt.Rezepte(RzKey(0)).KF - 1
            sum = sum + RezSozpt.Rezepte(RzKey(0))(i).BaAmng
          Next i
          strueb = Format(sum, "####.00")
          Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
          Y0 = Y0 - 2 * dy
          '
          '
          '
          '
        Case "06"
          '
          '
          '
          '
          'Menge(neu)
          '
          '
          If MxMe > 1 Then

            If RezSozpt.Rezepte(RzKey(1)).KF > 0 Then
              X = X + dx
              Y0 = Y + 0.5 * dy
              strueb = TexKt(21007)
              Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))
              For i = 0 To RezSozpt.Rezepte(RzKey(1)).KF - 1
                FaId = RezSozpt.Rezepte(RzKey(1))(i).ID
                KeyD = KeyName(FaId)
                Y0 = Y0 + dy
                strueb = Format(RezSozpt.Rezepte(RzKey(1))(i).BaAmng, Trim(RezSozpt.Farben(KeyD).Form))
                Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
              Next i
            End If

            '
            '
            'Summe
            '
            '
            Y0 = Y0 + 2 * dy
            sum = 0.0
            For i = 0 To RezSozpt.Rezepte(RzKey(1)).KF - 1
              sum = sum + RezSozpt.Rezepte(RzKey(1))(i).BaAmng
            Next i
            strueb = Format(sum, "####.00")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
            Y0 = Y0 - 2 * dy
          End If
          '
          '
          '
          '
        Case "07"
          '
          '
          '
          '
          'Zuwaage
          '
          '
          '
          '


          '
          If RezSozpt.Rezepte.ContainsKey("ZUW") AndAlso RezSozpt.Rezepte("ZUW").KF > 0 Then
            X = X + dx
            Y0 = Y + 0.5 * dy
            strueb = TexKt(21009)
            Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))

            For i = 0 To RezSozpt.Rezepte("ZUW").KF - 1
              FaId = RezSozpt.Rezepte("ZUW")(i).ID
              KeyD = KeyName(FaId)
              Y0 = Y0 + dy
              strueb = Format(RezSozpt.Rezepte("ZUW")(i).BaAmng, Trim(RezSozpt.Farben(KeyD).Form))
              Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))

            Next i
            '
            'Summe
            '
            '
            Y0 = Y0 + 2 * dy
            sum = 0.0
            For i = 0 To RezSozpt.Rezepte("ZUW").KF - 1
              sum = sum + RezSozpt.Rezepte("ZUW")(i).BaAmng
            Next i
            strueb = Format(sum, "####.00")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
            Y0 = Y0 - 2 * dy
            '
          End If


          '
          '
          '
          '
          '
        Case "08"
          '

          '
          'Menge(neuzu)
          '
          '
          If RezSozpt.Rezepte.ContainsKey("MZU") AndAlso RezSozpt.Rezepte("MZU").KF > 0 Then
            X = X + dx
            Y0 = Y + 0.5 * dy
            strueb = TexKt(21008)
            Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))

            For i = 0 To RezSozpt.Rezepte("MZU").KF - 1
              FaId = RezSozpt.Rezepte("MZU")(i).ID
              KeyD = KeyName(FaId)
              Y0 = Y0 + dy
              strueb = Format(RezSozpt.Rezepte("MZU")(i).BaAmng, Trim(RezSozpt.Farben(KeyD).Form))
              Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
            Next i

            '
            'Summe
            '
            '
            Y0 = Y0 + 2 * dy
            sum = 0.0
            For i = 0 To RezSozpt.Rezepte("MZU").KF - 1
              sum = sum + RezSozpt.Rezepte("MZU")(i).BaAmng
            Next i
            strueb = Format(sum, "####.00")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
            Y0 = Y0 - 2 * dy
          End If

          '
          '
          '
        Case "09"
          '
          '
          '
          '
          'Prozentigkeit
          '
          '

          X = X + dx
          Y0 = Y + 0.5 * dy
          strueb = TexKt(21013)
          Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))
          For i = 0 To RezSozpt.Rezepte(RzKey(0)).KF - 1
            FaId = RezSozpt.Rezepte(RzKey(0))(i).ID
            KeyD = KeyName(FaId)
            Y0 = Y0 + dy
            strueb = Format(RezSozpt.Rezepte(RzKey(0))(i).Proz, "###.00")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))

          Next i
          '
          '
          '
          '
          '
        Case "10"
          '
          '
          '
          '
          'Bindemittel-Prozentigkeit
          '
          '

          X = X + dx
          Y0 = Y + 0.5 * dy
          strueb = TexKt(21018)
          Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))
          For i = 0 To RezSozpt.Rezepte(RzKey(0)).KF - 1
            FaId = RezSozpt.Rezepte(RzKey(0))(i).ID
            KeyD = KeyName(FaId)
            Y0 = Y0 + dy
            strueb = Format(RezSozpt.Rezepte(RzKey(0))(i).Prob, "###.00")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))

          Next i
          '
          '
          '
          '
          '
        Case "11"
          '
          '
          '
          '
          '
          '


          'spez.Gewicht
          '
          '

          X = X + dx
          Y0 = Y + 0.5 * dy
          strueb = TexKt(21019)
          Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))

          For i = 0 To RezSozpt.Rezepte(RzKey(0)).KF - 1
            FaId = RezSozpt.Rezepte(RzKey(0))(i).ID
            KeyD = KeyName(FaId)
            Y0 = Y0 + dy
            strueb = Format(RezSozpt.Farben(KeyD).Spz, "##.000")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))

          Next i




        Case "12"
          '
          '
          '
          'Farbstärke
          '
          '
          X = X + dx
          Y0 = Y + 0.5 * dy
          strueb = TexKt(21014)
          Zeil.Add(Zeil.DrZeil(1, X, Y, Scwe, strueb, Graph))
          For i = 0 To RezSozpt.Rezepte(RzKey(0)).KF - 1
            FaId = RezSozpt.Rezepte(RzKey(0))(i).ID
            KeyD = KeyName(FaId)
            Y0 = Y0 + dy
            strueb = Format(RezSozpt.Farben(KeyD).Fst, "###.00")
            Zeil.Add(Zeil.DrZeil(1, X, Y0, Scwe, strueb, Graph))
          Next i
          '
      End Select
    Next k
    '
    strueb = Sline(255, "-")
    Y0 = Y0 + dy
    Zeil.Add(Zeil.DrZeil(2, X0, Y0, Zeil.Width, strueb, Graph))
    '
    '
    strueb = Sline(255, "-")
    Y = YZ + (RezSozpt.Rezepte(RzKey(0)).KF + 2.5) * dy + Yzus
    Zeil.Add(Zeil.DrZeil(2, X0, Y, Zeil.Width, strueb, Graph))
  End Sub

  Private Sub Printdoc_PrintPage(sender As Object, e As System.Drawing.Printing.PrintPageEventArgs) Handles Printdoc.PrintPage
    Select Case MnIsteu
      Case 0
        Call DruckMerkmale(sender, e)
      Case 1
        Call DruckDatagrid(sender, e)
    End Select

  End Sub
  '
  '
  '
  '
  '
  Sub DruckDatagrid(ByVal sender As Object, ByVal ev As PrintPageEventArgs)
    GridDruck.KopfZeile = MnKommentar
    GridDruck.Ueberschrift = MnUeberschrift
    Call GridDruck.DruckGrid(sender, ev)
  End Sub
  '






End Class