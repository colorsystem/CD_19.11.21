Option Strict Off
Option Explicit On
Option Compare Text
Public Class Colorant
  Implements IDisposable
  Implements ICloneable
  Private MnID As Integer 'Farbmittel ID
  Private MnName As String 'Farbmittelname
  Private MnBem As String 'Bemerkung
  Private MnAname As String 'Farbmittelalternativname
  Private MnForm As String 'Formatangabe
  Private MnPrNr As String 'Produktnummer
  Private MnPreis As Single 'Preis
  Private MnFst As Single 'Farbstärke (Grunddaten)
  Private MnBel As Single 'Farbstärke (Gewicht)
  Private MnSpz As Single 'spez. Gewicht des Farbmittels
  Private MnEff As Single 'Faktor für effektive Menge
  Private MnGlzGrd As Single 'GlanzGrad
  Private MnGlzGrdID As Integer 'GlanzgradID
  Private MnSmenge As Single 'Startmenge
  Private MnIchf As Short 'Art des Farb-/Bindemittels
  '0 = Farbmittel
  '1 = Bindemittel
  '2 = Weißpigment
  '3 = Schwarzpigment
  '4 = Metallic
  '5 = Effektpigment
  '6 = Zusatzmittel
  '7 = Restfarbe
  Private MnIbas As Short 'Basis-Farbmittel
  Private MnOP As String 'Operator (<,>,=)
  Private MnKto As String 'Topf
  Private MnBuMng As Single 'untere Menge gemäß Normierungsart (INM)
  Private MnBoMng As Single 'obere Menge gemäß Normierungsart (INM)
  Private MnPre() As Single 'Preise
  Private MnPrf() As Single 'Prozentigkeiten (Farbmittel)
  Private MnPrb() As Single 'Prozentigkeiten (Bindemittel)
  Private MnFarbID As Integer 'ID für Farbdarstellung
  Private MnOptData As OpticalData
  '
  '
  
  '
  '
  '
  '
  Property ID() As Integer
    Get
      ID = MnID
    End Get
    Set(ByVal Value As Integer)
      MnID = Value
    End Set
  End Property
  Property GlzGrdID() As Integer
    Get
      GlzGrdID = MnGlzGrdID
    End Get
    Set(ByVal Value As Integer)
      MnGlzGrdID = Value
    End Set
  End Property


  Property Name() As String
    Get
      Return MnName
    End Get
    Set(ByVal Value As String)
      MnName = Value
    End Set
  End Property

  Property Bem() As String
    Get
      Bem = MnBem
    End Get
    Set(ByVal Value As String)
      MnBem = Value
    End Set
  End Property

  Property Aname() As String
    Get
      Aname = MnAname
    End Get
    Set(ByVal Value As String)
      MnAname = Value
    End Set
  End Property

  Property Form() As String
    Get
      Form = MnForm
    End Get
    Set(ByVal Value As String)
      MnForm = Value
    End Set
  End Property

  Property PrNr() As String
    Get
      PrNr = MnPrNr
    End Get
    Set(ByVal Value As String)
      MnPrNr = Value
    End Set
  End Property

  Property Preis() As Single
    Get
      Preis = MnPreis
    End Get
    Set(ByVal Value As Single)
      If Value >= 0.0# Then
        MnPreis = Value
      End If
    End Set
  End Property

  Property Pre(ByVal i As Short) As Single
    Get
      Pre = MnPre(i)
    End Get
    Set(ByVal Value As Single)
      ReDim Preserve MnPre(i)
      MnPre(i) = Value
    End Set
  End Property
  '
  ReadOnly Property PreCount() As Short
    Get
      PreCount = 0
      If Not IsNothing(MnPre) Then
        PreCount = UBound(MnPre) + 1
      End If
    End Get
  End Property
  '
  '
  Property Prf(ByVal i As Short) As Single
    Get
      Prf = MnPrf(i)
    End Get
    Set(ByVal Value As Single)
      ReDim Preserve MnPrf(i)
      MnPrf(i) = Value
    End Set
  End Property
  '
  ReadOnly Property PrfCount() As Short
    Get
      PrfCount = 0
      If Not IsNothing(MnPrf) Then
        PrfCount = UBound(MnPrf) + 1
      End If
    End Get
  End Property
  '
  '
  Property Prb(ByVal i As Short) As Single
    Get
      Prb = MnPrb(i)
    End Get
    Set(ByVal Value As Single)
      ReDim Preserve MnPrb(i)
      MnPrb(i) = Value
    End Set
  End Property
  '
  '
  ReadOnly Property PrbCount() As Short
    Get
      PrbCount = 0
      If Not IsNothing(MnPrb) Then
        PrbCount = UBound(MnPrb) + 1
      End If
    End Get
  End Property
  '
  '
  Property Fst() As Single
    Get
      Fst = MnFst
    End Get
    Set(ByVal Value As Single)
      MnFst = Value
    End Set
  End Property

  Property Bel() As Single
    Get
      Bel = MnBel
    End Get
    Set(ByVal Value As Single)
      MnBel = Value
    End Set
  End Property

  Property Spz() As Single
    Get
      Spz = MnSpz
    End Get
    Set(ByVal Value As Single)
      MnSpz = Value
    End Set
  End Property
  Property Eff() As Single
    Get
      Eff = MnEff
    End Get
    Set(ByVal Value As Single)
      MnEff = Value
    End Set
  End Property

  Property Smenge() As Single
    Get
      Smenge = MnSmenge
    End Get
    Set(ByVal Value As Single)
      MnSmenge = Value
    End Set
  End Property

  Property Ichf() As Short
    Get
      Ichf = MnIchf
    End Get
    Set(ByVal Value As Short)
      MnIchf = Value
    End Set
  End Property
  Property Ibas() As Short
    Get
      Ibas = MnIbas
    End Get
    Set(ByVal Value As Short)
      MnIbas = Value
    End Set
  End Property
  Property FarbID() As Integer
    Get
      FarbID = MnFarbID
    End Get
    Set(ByVal Value As Integer)
      MnFarbID = Value
    End Set
  End Property

  Property OP() As String
    Get
      OP = MnOP
    End Get
    Set(ByVal Value As String)
      MnOP = Value
    End Set
  End Property

  Property Kto() As String
    Get
      Kto = MnKto
    End Get
    Set(ByVal Value As String)
      MnKto = Value
    End Set
  End Property

  Property BuMng() As Single
    Get
      BuMng = MnBuMng
    End Get
    Set(ByVal Value As Single)
      MnBuMng = Value
    End Set
  End Property

  Property BoMng() As Single
    Get
      BoMng = MnBoMng
    End Get
    Set(ByVal Value As Single)
      MnBoMng = Value
    End Set
  End Property
  Property GlzGrd() As Single
    Get
      GlzGrd = MnGlzGrd
    End Get
    Set(ByVal Value As Single)
      MnGlzGrd = Value
    End Set
  End Property
  Property OptData() As OpticalData
    Get
      OptData = MnOptData
    End Get
    Set(ByVal Value As OpticalData)
      MnOptData = Value
    End Set
  End Property

  Public Sub New()
    MyBase.New()
    MnName = ""
    MnBem = ""
    MnAname = ""
    MnPrNr = ""
    MnBuMng = 0.0#
    MnBoMng = 100.0#
    MnOP = " "
    MnKto = " "
    MnFst = 100.0#
    MnBel = 1.0#
    MnSpz = 1.0#
    MnGlzGrd = 0.0#
    MnGlzGrdID = -1
    MnSmenge = 0.0#
    MnIchf = 0
    MnID = -1
    MnPreis = 0.0#
    MnForm = "##0.000"
    MnFarbID = Color.Gray.ToArgb
    'System.Drawing.ColorTranslator.ToOle(System.Drawing.SystemColors.Window)
    MnEff = 1.0#
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    MnPre = Nothing
    MnPrf = Nothing
    MnPrb = Nothing
  End Sub
  '
  '
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As Colorant
    clone = CType(Me.MemberwiseClone, Colorant)
    If Not MnPre Is Nothing Then
      clone.MnPre = MnPre.Clone
    End If
    If Not MnPrf Is Nothing Then
      clone.MnPrf = MnPrf.Clone
    End If
    If Not MnPrb Is Nothing Then
      clone.MnPrb = MnPrb.Clone
    End If
    If Not OptData Is Nothing Then
      clone.OptData = OptData.Clone
    End If
    '
    '
  End Function

  

End Class