Option Strict Off
Option Explicit On
Option Compare Text
Public Class ValuesGrpsAssign
  Implements IDisposable
  Dim i As Short
  Private CollWertens As DictionaryInd(Of String, ValuesGrps) 'Collection für alle R-Kurven einer Anweisung
  Private CollMerkmale As DictionaryInd(Of String, Sign) 'Collection zur Characterisierung der Merkmale
  Private MnAuswID As Values
  Private MnAufgArt As String
  Private MnAnwsgID As Integer
  Private MnAnwsgName As String
  Private CollWinkel As DictionaryInd(Of String, AnglExist) 'Collection zur Characterisierung der Merkmale
  Private MnPrintNLAWIN(,) As Boolean


  Public Sub New()
    MyBase.New()
    MnAufgArt = ""
    MnAnwsgName = ""
    CollWertens = New DictionaryInd(Of String, ValuesGrps)
    CollMerkmale = New DictionaryInd(Of String, Sign)
    CollWinkel = New DictionaryInd(Of String, AnglExist)
    MnAuswID = New Values
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    CollWertens = Nothing
    CollMerkmale = Nothing
    CollWinkel = Nothing
  End Sub
  Sub Add(ByVal RwKey As String, ByVal value As ValuesGrps)
    CollWertens.Add(RwKey, value)
  End Sub
 
  Default Overloads ReadOnly Property Item(ByVal RwKey As String) As ValuesGrps
    Get
      Item = CollWertens.Item(RwKey)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As ValuesGrps
    Get
      Item = CollWertens.Item(index)
    End Get
  End Property
  ReadOnly Property RwKey(ByVal index As Integer) As String
    Get
      RwKey = CollWertens.Key(index)
    End Get
  End Property
  Public Function Count() As Integer
    Count = CollWertens.Count
  End Function
  Public Sub clear()
    CollWertens.Clear()
  End Sub
  Public Overloads Sub Remove(ByVal RwKey As String)
    CollWertens.Remove(RwKey)
  End Sub
  Public Overloads Sub Remove(ByVal index As Integer)
    CollWertens.Remove(index)
  End Sub
  ReadOnly Property ContainsRwKey(ByVal RwKey As String) As Boolean
    Get
      ContainsRwKey = CollWertens.ContainsKey(RwKey)
    End Get
  End Property

  '
  Property AnwsgID() As Integer
    Get
      AnwsgID = MnAnwsgID
    End Get
    Set(ByVal Value As Integer)
      MnAnwsgID = Value
    End Set
  End Property
 
  ReadOnly Property AuswID() As Values
    Get
      AuswID = MnAuswID
    End Get
  End Property
  Property AufgArt() As String
    Get
      AufgArt = MnAufgArt
    End Get
    Set(ByVal Value As String)
      MnAufgArt = Value
    End Set
  End Property
  Property AnwsgName() As String
    Get
      AnwsgName = MnAnwsgName
    End Get
    Set(ByVal Value As String)
      MnAnwsgName = Value
    End Set
  End Property
  '
  '
  '
  '
  '
  'Merkmale
  '
  '
  '
  '
  '
  '
  Sub AddMerk(ByVal MerkKey As String, ByRef Merk As Sign)
    CollMerkmale.Add(MerkKey, Merk)
  End Sub
  Public Sub RemoveMerk(ByRef MerkKey As String)
    CollMerkmale.Remove(MerkKey)
  End Sub
  Overloads ReadOnly Property Merk(ByVal MerkKey As String) As Sign
    Get
      Merk = CollMerkmale.Item(MerkKey)
    End Get
  End Property
  ReadOnly Property ContainsMerk(ByVal MerkKey As String) As Boolean
    Get
      ContainsMerk = CollMerkmale.ContainsKey(MerkKey)
    End Get
  End Property
  Overloads ReadOnly Property Merk(ByVal index As Integer) As Sign
    Get
      Merk = CollMerkmale.Item(index)
    End Get
  End Property
  ReadOnly Property MerkKey(ByVal index As Integer) As String
    Get
      MerkKey = CollMerkmale.Key(index)
    End Get
  End Property
  Public Function CountMerk() As Integer
    CountMerk = CollMerkmale.Count()
  End Function
  Sub ClearMerk()
    CollMerkmale.Clear()
  End Sub
  '
  Property PrintNLAWIN As Boolean(,)
    Get
      PrintNLAWIN = MnPrintNLAWIN
    End Get
    Set(ByVal Value As Boolean(,))
      MnPrintNLAWIN = Value
    End Set
  End Property
  '
  '
  '
  '
  'Winkel
  '
  '
  '
  '
  '
  '
  Sub AddWinkel(ByVal WinKey As String, ByRef Exist As AnglExist)
    CollWinkel.Add(WinKey, Exist)
  End Sub
  Public Sub RemoveWinkel(ByRef WinKey As String)
    CollWinkel.Remove(WinKey)
  End Sub
  ReadOnly Property ContainsWink(ByVal WinKey As String) As Boolean
    Get
      ContainsWink = CollWinkel.ContainsKey(WinKey)
    End Get
  End Property
  Overloads ReadOnly Property Winkel(ByVal WinKey As String) As AnglExist
    Get
      Winkel = CollWinkel.Item(WinKey)
    End Get
  End Property

  Overloads ReadOnly Property Winkel(ByVal index As Integer) As AnglExist
    Get
      Winkel = CollWinkel.Item(index)
    End Get
  End Property
  ReadOnly Property WinKey(ByVal index As Integer) As String
    Get
      WinKey = CollWinkel.Key(index)
    End Get
  End Property

  Public Function CountWinkel() As Integer
    CountWinkel = CollWinkel.Count()
  End Function
  Sub Clearwinkel()
    CollWinkel.Clear()
  End Sub
  '
  '
End Class