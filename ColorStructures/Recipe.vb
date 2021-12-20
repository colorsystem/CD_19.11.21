Option Strict Off
Option Explicit On
Option Compare Text
Public Class Recipe
  Implements IDisposable
  Implements ICloneable
  Dim i As Short
  Dim MnID As Integer 'Rezept ID
  Dim MnNr As Short
  Dim MnName As String
  Dim MnBem As String
  Dim MnDatTim As Date
  Dim MnDicke(1) As Single 'Schichtdicke
  Dim MnFrbMng As ColorAmount
  Dim MnGid As Integer
  Dim MnIarch As Short
  Dim MnSorKrit As List(Of Values) 'Sortierkriterien
  Dim MnUmMng As Single 'Umrechnungsfaktor für Limitierungsmengen
  Dim MnGlzGrd As Single 'Glanzgrad
  Dim MnRezMin As Short 'Minimale Anzahl Farb-/Bindemittel pro Rezept
  Dim MnRezMax As Short 'Maximale Anzahl Farb-/Bindemittel pro Rezept

  Private CollFarbMengen As DictionaryInd(Of String, ColorAmount) 'Collection für Farb-/Bindemittelmengen
  '
  '
  '
  '
  Public Sub New()
    MyBase.New()
    MnGlzGrd = -1.0
    MnUmMng = 1.0#
    MnID = -1
    MnGid = 0
    MnIarch = 0
    MnDicke(0) = 0.0
    MnDicke(1) = 0.0
    CollFarbMengen = New DictionaryInd(Of String, ColorAmount)
    MnSorKrit = New List(Of Values)
    MnSorKrit.Add(New Values)
    MnSorKrit.Add(New Values)
  End Sub

  Sub dispose() Implements IDisposable.Dispose
    CollFarbMengen = Nothing
    MnSorKrit = Nothing
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  '
  
  Property ID() As Integer
    Get
      ID = MnID
    End Get
    Set(ByVal Value As Integer)
      MnID = Value
    End Set
  End Property

  Property Nr() As Short
    Get
      Nr = MnNr
    End Get
    Set(ByVal Value As Short)
      MnNr = Value
    End Set
  End Property
  Property RezMin() As Short
    Get
      RezMin = MnRezMin
    End Get
    Set(ByVal Value As Short)
      MnRezMin = Value
    End Set
  End Property
  Property RezMax() As Short
    Get
      RezMax = MnRezMax
    End Get
    Set(ByVal Value As Short)
      MnRezMax = Value
    End Set
  End Property
  '
  ReadOnly Property Dicke() As Single()
    Get
      Dicke = MnDicke
    End Get
  End Property
  '
  '
  '
  '
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
  Property DatTim() As Date
    Get
      DatTim = MnDatTim
    End Get
    Set(ByVal Value As Date)
      MnDatTim = Value
    End Set
  End Property
  '
  '
  Property Gid() As Integer
    Get
      Gid = MnGid
    End Get
    Set(ByVal Value As Integer)
      MnGid = Value
    End Set
  End Property
  Property UmMng() As Single
    Get
      UmMng = MnUmMng
    End Get
    Set(ByVal Value As Single)
      MnUmMng = Value
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
  Property Iarch() As Short
    Get
      Iarch = MnIarch
    End Get
    Set(ByVal Value As Short)
      MnIarch = Value
    End Set
  End Property
  '
  '
  Property Sorkrit(ByVal i As Integer) As Values
    Get
      Sorkrit = MnSorKrit(i)
    End Get
    Set(ByVal Value As Values)
      MnSorKrit(i) = Value
    End Set
  End Property



  Public Sub AddFaNr(ByVal FaNr As String, ByVal value As ColorAmount)
    CollFarbMengen.Add(FaNr, value)
  End Sub


  Default Overloads ReadOnly Property Item(ByVal FaNr As String) As ColorAmount
    Get
      Item = CollFarbMengen.Item(FaNr)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As ColorAmount
    Get
      Item = CollFarbMengen.Item(index)
    End Get
  End Property
  ReadOnly Property FaNr(ByVal Index As Integer) As String
    Get
      FaNr = CollFarbMengen.Key(Index)
    End Get
  End Property
  Public Function KF() As Integer
    KF = CollFarbMengen.Count
  End Function
  Public Sub clear()
    CollFarbMengen.Clear()
  End Sub
  Public Overloads Sub RemoveFaNr(ByVal FaNr As String)
    CollFarbMengen.Remove(FaNr)
  End Sub
  Public Overloads Sub RemoveFaNr(ByVal Index As Integer)
    CollFarbMengen.Remove(Index)
  End Sub
  ReadOnly Property ContainsKey(ByVal FaNr As String) As Boolean
    Get
      ContainsKey = CollFarbMengen.ContainsKey(FaNr)
    End Get
  End Property
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As Recipe
    Dim i As Integer
    clone = CType(Me.MemberwiseClone, Recipe)

    If Not MnSorKrit Is Nothing Then
      clone.MnSorKrit = New List(Of Values)
      For i = 0 To MnSorKrit.Count - 1
        clone.MnSorKrit.Add(MnSorKrit(i).clone)
      Next
    End If
    If Not CollFarbMengen Is Nothing Then
      clone.CollFarbMengen = New DictionaryInd(Of String, ColorAmount)
      For i = 0 To CollFarbMengen.Count - 1
        clone.CollFarbMengen.Add(CollFarbMengen.Keys(i), CollFarbMengen(i).clone)
      Next
    End If
  End Function
End Class