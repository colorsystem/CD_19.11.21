Option Strict Off
Option Explicit On
Option Compare Text
Public Class Colorants
  Implements IDisposable
  Implements ICloneable

  Private CollFarben As DictionaryInd(Of String, Colorant)  'Collection für Farb-/Bindemittel
  '
  Public Sub New()
    MyBase.New()
    CollFarben = New DictionaryInd(Of String, Colorant)
  End Sub


  Sub dispose() Implements IDisposable.Dispose
    CollFarben = Nothing
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub

  Public Sub AddFarb(ByVal FaKey As String, ByVal value As Colorant)
    CollFarben.Add(FaKey, value)
  End Sub


  Default Overloads ReadOnly Property Item(ByVal FaKey As String) As Colorant
    Get
      Item = CollFarben.Item(FaKey)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As Colorant
    Get
      Item = CollFarben.Item(index)
    End Get
  End Property
  ReadOnly Property FaKey(ByVal index As Integer) As String
    Get
      FaKey = CollFarben.Key(index)
    End Get
  End Property
  Public Function FarbCount() As Integer
    FarbCount = CollFarben.Count
  End Function
  Public Sub clear()
    CollFarben.Clear()
  End Sub
  Public Sub RemoveFarb(ByVal FaKey As String)
    CollFarben.Remove(FaKey)
  End Sub
  ReadOnly Property ContainsFarb(ByVal FaKey As String) As Boolean
    Get
      ContainsFarb = CollFarben.ContainsKey(FaKey)
    End Get
  End Property
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As Colorants
    clone = CType(Me.MemberwiseClone, Colorants)
    If Not CollFarben Is Nothing Then
      clone.CollFarben = New DictionaryInd(Of String, Colorant)
      For i = 0 To CollFarben.Count - 1
        clone.CollFarben.Add(CollFarben.Keys(i), CollFarben(i).clone)
      Next
    End If
    '
  End Function
  '
End Class
