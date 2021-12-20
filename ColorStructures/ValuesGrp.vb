Option Strict Off
Option Explicit On
Option Compare Text
Public Class ValuesGrp
  Implements IDisposable
  Private CollWerte As DictionaryInd(Of String, Values)



  Public Sub New()
    MyBase.New()
    CollWerte = New DictionaryInd(Of String, Values)
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    CollWerte = Nothing
  End Sub

  Sub Add(ByVal Chrm As String, ByVal value As Values)
    CollWerte.Add(Chrm, value)
  End Sub
  ReadOnly Property Chrm(ByVal index As Integer) As String
    Get
      Chrm = CollWerte.Key(index)
    End Get
  End Property
  Default Overloads ReadOnly Property Item(ByVal Chrm As String) As Values
    Get
      Item = CollWerte.Item(Chrm)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As Values
    Get
      Item = CollWerte.Item(index)
    End Get
  End Property
  Public Function Count() As Integer
    Count = CollWerte.Count
  End Function
  Public Sub clear()
    CollWerte.Clear()
  End Sub
  Public Sub Remove(ByVal Chrm As String)
    CollWerte.Remove(Chrm)
  End Sub
End Class