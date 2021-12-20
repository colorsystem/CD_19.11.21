Option Strict Off
Option Explicit On
Option Compare Text
Public Class Values
  Implements IDisposable
  Implements ICloneable
  Private CollWert As DictionaryInd(Of String, Object)


  Public Sub New()
    MyBase.New()
    CollWert = New DictionaryInd(Of String, Object)
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    CollWert = Nothing
  End Sub
  '
  Sub Add(ByVal key As String, ByVal Wert As Object)
    CollWert.Add(key, Wert)
    CollWert(key) = DBNull.Value
  End Sub

  ReadOnly Property Count() As Integer
    Get
      Count = CollWert.Count()
    End Get
  End Property
  Default Overloads Property Item(ByVal key As String) As Object
    Get
      Item = CollWert.Item(key)
    End Get
    Set(ByVal value As Object)
      CollWert.Item(key) = value
    End Set
  End Property
  Default Overloads Property Item(ByVal index As Integer) As Object
    Get
      Item = CollWert.Item(index)
    End Get
    Set(ByVal value As Object)
      CollWert.Item(index) = value
    End Set
  End Property
  ReadOnly Property Key(ByVal index As Integer) As String
    Get
      Key = CollWert.Key(index)
    End Get
  End Property
  Public Sub clear()
    CollWert.Clear()
  End Sub
  Public Sub Remove(ByRef key As String)
    CollWert.Remove(key)
  End Sub

  ReadOnly Property ContainsKey(ByVal RezKey As String) As Boolean
    Get
      ContainsKey = CollWert.ContainsKey(RezKey)
    End Get
  End Property
  '
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As Values
    clone = CType(Me.MemberwiseClone, Values)
    If Not CollWert Is Nothing Then
      clone.CollWert = New DictionaryInd(Of String, Object)
      For i = 0 To CollWert.Count - 1
        clone.CollWert.Add(CollWert.Keys(i), CType(CollWert(i), Object))
      Next
    End If
  End Function

End Class