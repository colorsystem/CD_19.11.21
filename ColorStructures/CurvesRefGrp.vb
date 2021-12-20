Option Strict Off
Option Explicit On
Option Compare Text
Public Class CurvesRefGrp
  Implements IDisposable
  Implements ICloneable
  Private CollKurven As DictionaryInd(Of String, CurvesRef)
  Public Sub New()
    MyBase.New()
    CollKurven = New DictionaryInd(Of String, CurvesRef)
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    CollKurven = Nothing
  End Sub
  Public Sub Add(ByVal Key As String, ByVal value As CurvesRef)
    CollKurven.Add(Key, value)
  End Sub
  ReadOnly Property Key(ByVal index As Integer) As String
    Get
      Key = CollKurven.Key(index)
    End Get
  End Property
  Default Overloads ReadOnly Property Item(ByVal Key As String) As CurvesRef
    Get
      Item = CollKurven.Item(Key)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As CurvesRef
    Get
      Item = CollKurven.Item(index)
    End Get
  End Property

  Public Function Count() As Integer
    Count = CollKurven.Count
  End Function
  Public Sub clear()
    CollKurven.Clear()
  End Sub
  Public Sub Remove(ByVal key As String)
    CollKurven.Remove(key)
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
  Function clone() As CurvesRefGrp
    clone = CType(Me.MemberwiseClone, CurvesRefGrp)

    If Not CollKurven Is Nothing Then
      clone.CollKurven = New DictionaryInd(Of String, CurvesRef)
      For i = 0 To CollKurven.Count - 1
        clone.CollKurven.Add(CollKurven.Keys(i), CollKurven(i).clone)
      Next
    End If
   
    '
    '
  End Function
End Class