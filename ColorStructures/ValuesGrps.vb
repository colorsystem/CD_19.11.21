Option Strict Off
Option Explicit On
Option Compare Text
Public Class ValuesGrps
  Implements IDisposable
  Private MnItp As Boolean 'Typ(True)
  Private MnNr As Short
  Private MnName As String
  Private MnBem As String
  Private MnBanum As String
  Private MnDatTim As Date
  Private CollWerten As DictionaryInd(Of String, ValuesGrp) 'Collection für eine R-Kurve




  Public Sub New()
    'MyBase.New()
    CollWerten = New DictionaryInd(Of String, ValuesGrp)
    MnNr = -1
  End Sub

  Protected Overrides Sub Finalize()
    'MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    CollWerten = Nothing
  End Sub

  Sub Add(ByVal IllKey As String, ByVal value As ValuesGrp)
    CollWerten.Add(IllKey, value)
  End Sub
  Default Overloads ReadOnly Property Item(ByVal IllKey As String) As ValuesGrp
    Get
      Item = CollWerten.Item(IllKey)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As ValuesGrp
    Get
      Item = CollWerten.Item(index)
    End Get
  End Property
  ReadOnly Property IllKey(ByVal index As Integer) As String
    Get
      IllKey = CollWerten.Key(index)
    End Get
  End Property
  Public Function Count() As Integer
    Count = CollWerten.Count
  End Function
  Public Sub clear()
    CollWerten.Clear()
  End Sub
  Public Sub Remove(ByVal IllKey As String)
    CollWerten.Remove(IllKey)
  End Sub

  Property Itp() As Boolean
    Get
      Itp = MnItp
    End Get
    Set(ByVal Value As Boolean)
      MnItp = Value
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

  Property Name() As String
    Get
      Return MnName
    End Get
    Set(ByVal Value As String)
      MnName = Value
    End Set
  End Property
  Property Banum() As String
    Get
      Return MnBanum
    End Get
    Set(ByVal Value As String)
      MnBanum = Value
    End Set
  End Property
  Property Bem() As String
    Get
      Return MnBem
    End Get
    Set(ByVal Value As String)
      MnBem = Value
    End Set
  End Property
  Property Dattim() As Date
    Get
      Dattim = MnDatTim
    End Get
    Set(ByVal Value As Date)
      MnDatTim = Value
    End Set
  End Property
  
End Class