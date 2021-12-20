Option Strict Off
Option Explicit On
Option Compare Text

Public Class CurvesRef
  Implements IDisposable
  Implements ICloneable
  Private CollKurve As DictionaryInd(Of String, CurveRef)
  '
  'Plotten True/False
  '


  Public Sub New()
    MyBase.New()
    CollKurve = New DictionaryInd(Of String, CurveRef)
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    CollKurve = Nothing
  End Sub

  Public Sub Add(ByVal WinKey As String, ByVal value As CurveRef)
    CollKurve.Add(WinKey, value)
  End Sub
  Default Overloads ReadOnly Property Item(ByVal WinKey As String) As CurveRef
    Get
      Item = CollKurve.Item(WinKey)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As CurveRef
    Get
      Item = CollKurve.Item(index)
    End Get
  End Property
  ReadOnly Property Winkey(ByVal index As Integer) As String
    Get
      Winkey = CollKurve.Key(index)
    End Get
  End Property
  Public Function Count() As Integer
    Count = CollKurve.Count
  End Function
  Public Sub clear()
    CollKurve.Clear()
  End Sub
  Public Sub Remove(ByVal WinKey As String)
    CollKurve.Remove(WinKey)
  End Sub
  '
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As CurvesRef
    Dim i As Integer
    clone = CType(Me.MemberwiseClone, CurvesRef)
    If Not CollKurve Is Nothing Then
      clone.CollKurve = New DictionaryInd(Of String, CurveRef)
      For i = 0 To CollKurve.Count - 1
        clone.CollKurve.Add(CollKurve.Keys(i), CollKurve(i).clone)
      Next
    End If
  End Function
End Class