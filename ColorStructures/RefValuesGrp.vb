Option Strict Off
Option Explicit On
Option Compare Text

Public Class RefValuesGrp
  Implements IDisposable
  Implements ICloneable
  'RwArt
  'Schlüsselwort z.B. 
  'W weißer Untergrund; Weißpigment
  'S schwarzer Untergrund; Schwarzpigment
  'G Graupigment

  Dim RwerteColl As DictionaryInd(Of String, RefValues)
  Public Sub New()
    MyBase.New()
    RwerteColl = New DictionaryInd(Of String, RefValues)
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    RwerteColl = Nothing
  End Sub
  Public Sub Add(ByVal RwArt As String, ByVal value As RefValues)
    RwerteColl.Add(RwArt, value)
  End Sub
  Default Overloads ReadOnly Property Item(ByVal RwArt As String) As RefValues
    Get
      Item = RwerteColl.Item(RwArt)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As RefValues
    Get
      Item = RwerteColl.Item(index)
    End Get
  End Property
  ReadOnly Property RwArt(ByVal index As Integer) As String
    Get
      RwArt = RwerteColl.Key(index)
    End Get
  End Property
  Public Function Count() As Integer
    Count = RwerteColl.Count
  End Function
  Public Sub clear()
    RwerteColl.Clear()
  End Sub
  Public Sub Remove(ByVal RwArt As String)
    RwerteColl.Remove(RwArt)
  End Sub
  ReadOnly Property ContainsKey(ByVal RwArt As String) As Boolean
    Get
      ContainsKey = RwerteColl.ContainsKey(RwArt)
    End Get
  End Property
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As RefValuesGrp
    clone = CType(Me.MemberwiseClone, RefValuesGrp)

    If Not RwerteColl Is Nothing Then
      clone.RwerteColl = New DictionaryInd(Of String, RefValues)
      For i = 0 To RwerteColl.Count - 1
        clone.RwerteColl.Add(RwerteColl.Keys(i), RwerteColl(i).clone)
      Next
    End If
  End Function

End Class
