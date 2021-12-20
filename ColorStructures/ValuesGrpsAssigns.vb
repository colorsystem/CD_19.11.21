Option Strict Off
Option Explicit On
Option Compare Text
Public Class ValuesGrpsAssigns
  Implements IDisposable
  Dim MnKommentar As String
  Dim MnPruefID As Integer
  Dim MnProArtID As Integer
  Dim MnPruefMittel As String
  Dim MnProduktArt As String
  Dim MnHoleNum As Double
  Dim MnHoleText As String
  Private CollAnweisungen As DictionaryInd(Of String, ValuesGrpsAssign) 'Collection für alle Anweisungen
  '
  Private CollAuxiliary As DictionaryInd(Of String, ValuesAux) 'Collection für z.B. Färbecharacteristiken

  Public Sub New()
    MyBase.New()
    CollAnweisungen = New DictionaryInd(Of String, ValuesGrpsAssign)
    CollAuxiliary = New DictionaryInd(Of String, ValuesAux)
    MnHoleText = ""
    MnHoleNum = HUGE()
    MnKommentar = ""
    MnPruefMittel = ""
    MnProduktArt = ""
    MnPruefID = -1
    MnProArtID = -1
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    CollAnweisungen = Nothing
  End Sub
  Sub Add(ByVal AnwsgKey As String, ByVal value As ValuesGrpsAssign)
    CollAnweisungen.Add(AnwsgKey, value)
  End Sub

  Default Overloads ReadOnly Property Item(ByVal AnwsgKey As String) As ValuesGrpsAssign
    Get
      Item = CollAnweisungen.Item(AnwsgKey)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As ValuesGrpsAssign
    Get
      Item = CollAnweisungen.Item(index)
    End Get
  End Property
  ReadOnly Property AnwsgKey(ByVal index As Integer) As String
    Get
      AnwsgKey = CollAnweisungen.Key(index)
    End Get
  End Property
  Public Function Count() As Integer
    Count = CollAnweisungen.Count
  End Function
  Public Sub clear()
    CollAnweisungen.Clear()
  End Sub
  Public Overloads Sub Remove(ByVal AnwsgKey As String)
    CollAnweisungen.Remove(AnwsgKey)
  End Sub
  Public Overloads Sub Remove(ByVal Index As Integer)
    CollAnweisungen.Remove(Index)
  End Sub
  ReadOnly Property ContainsAnwsg(ByVal AnwsgKey As String) As Boolean
    Get
      ContainsAnwsg = CollAnweisungen.ContainsKey(AnwsgKey)
    End Get
  End Property '
  '
  'ValuesAuxiliary
  '
  '
  Sub AddAux(ByVal AuxKey As String, ByVal value As ValuesAux)
    CollAuxiliary.Add(AuxKey, value)
  End Sub

  Overloads ReadOnly Property Auxiliary(ByVal AuxKey As String) As ValuesAux
    Get
      Auxiliary = CollAuxiliary.Item(AuxKey)
    End Get
  End Property

  Overloads ReadOnly Property Auxiliary(ByVal index As Integer) As ValuesAux
    Get
      Auxiliary = CollAuxiliary.Item(index)
    End Get
  End Property
  ReadOnly Property AuxKey(ByVal index As Integer) As String
    Get
      AuxKey = CollAuxiliary.Key(index)
    End Get
  End Property
  Public Function AuxCount() As Integer
    AuxCount = CollAuxiliary.Count
  End Function
  Public Sub Auxclear()
    CollAuxiliary.Clear()
  End Sub
  Public Sub AuxRemove(ByVal AuxKey As String)
    CollAuxiliary.Remove(AuxKey)
  End Sub


  Property Kommentar() As String
    Get
      Kommentar = MnKommentar
    End Get
    Set(ByVal Value As String)
      MnKommentar = Value
    End Set
  End Property
  Property PruefID() As Integer
    Get
      PruefID = MnPruefID
    End Get
    Set(ByVal Value As Integer)
      MnPruefID = Value
    End Set
  End Property

  Property PruefMittel() As String
    Get
      PruefMittel = MnPruefMittel
    End Get
    Set(ByVal Value As String)
      MnPruefMittel = Value
    End Set
  End Property
  Property ProArtID() As Integer
    Get
      ProArtID = MnProArtID
    End Get
    Set(ByVal Value As Integer)
      MnProArtID = Value
    End Set
  End Property

  Property ProduktArt() As String
    Get
      ProduktArt = MnProduktArt
    End Get
    Set(ByVal Value As String)
      MnProduktArt = Value
    End Set
  End Property
  Property HoleNum() As Object
    Get
      HoleNum = MnHoleNum
    End Get
    Set(ByVal Value As Object)
      MnHoleNum = Value
    End Set
  End Property
  Property HoleText() As Object
    Get
      HoleText = MnHoleText
    End Get
    Set(ByVal Value As Object)
      MnHoleText = Value
    End Set
  End Property
 

  '
  '
 

End Class