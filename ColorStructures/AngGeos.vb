Option Strict Off
Option Explicit On
Option Compare Text
Public Class AngGeos
  Implements IDisposable
  Implements ICloneable
  Private CollWinkel As DictionaryInd(Of String, AngGeo)
  Private MnWsol As CurveRef  'Sollwellenlängen
  Private MnNwp As Short   'Anzahl Wellenlaängen zum Plotten
  Private MnStrtNwp As Short
  Public Sub New()
    MyBase.New()
    CollWinkel = New DictionaryInd(Of String, AngGeo)
  End Sub

  Private Sub dispose() Implements IDisposable.Dispose
    CollWinkel = Nothing
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  '
  Property Wsol() As CurveRef
    Get
      Wsol = MnWsol
    End Get
    Set(ByVal Value As CurveRef)
      MnWsol = Value
    End Set
  End Property
  Property Nwp() As Short
    Get
      Nwp = MnNwp
    End Get
    Set(ByVal value As Short)
      MnNwp = value
    End Set
  End Property
  Property StrtNwp() As Short
    Get
      StrtNwp = MnStrtNwp
    End Get
    Set(ByVal value As Short)
      MnStrtNwp = value
    End Set
  End Property



  Sub Add(ByVal Key As String, ByVal value As AngGeo)
    CollWinkel.Add(Key, value)
  End Sub

  Default Overloads ReadOnly Property Item(ByVal Key As String) As AngGeo
    Get
      Item = CollWinkel.Item(Key)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As AngGeo
    Get
      Item = CollWinkel.Item(index)
    End Get
  End Property
  ReadOnly Property Key(ByVal index As Integer) As String
    Get
      Key = CollWinkel.Key(index)
    End Get
  End Property
  ReadOnly Property Km() As Short
    Get
      Km = CollWinkel.Count()
    End Get
  End Property
  Public Sub clear()
    CollWinkel.Clear()
  End Sub
  Public Sub Remove(ByVal Key As String)
    CollWinkel.Remove(Key)
  End Sub
  ReadOnly Property ContainsWink(ByVal CHRM As String) As Boolean
    Get
      ContainsWink = CollWinkel.ContainsKey(CHRM)
    End Get
  End Property
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As AngGeos
    clone = CType(Me.MemberwiseClone, AngGeos)
    clone.CollWinkel = New DictionaryInd(Of String, AngGeo)
    For i = 0 To CollWinkel.Count - 1
      clone.CollWinkel.Add(CollWinkel.Keys(i), CollWinkel(i).clone)
    Next
  End Function
End Class