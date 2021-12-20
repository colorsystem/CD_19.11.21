Option Strict Off
Option Explicit On
Option Compare Text

Public Class RefValues
  Implements IDisposable
  Implements ICloneable
  'RwKey As String
  'Schlüsselwort   z.B.
  'V Vorlage
  'N Nachstellung
  'R Rechnung
  Private MnRefUnt As RefValue
  Private MnRefMix As RefValue
  Private Reflexion As DictionaryInd(Of String, RefValue) 'Collection von Reflexionswerten

  Public Sub New()
    MyBase.New()
    Reflexion = New DictionaryInd(Of String, RefValue)
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    Reflexion = Nothing
  End Sub
  Public Sub Add(ByVal RwKey As String, ByVal value As RefValue)
    Reflexion.Add(RwKey, value)
  End Sub

   

  Default Overloads ReadOnly Property Item(ByVal RwKey As String) As RefValue
    Get
      Item = Reflexion.Item(RwKey)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As RefValue
    Get
      Item = Reflexion.Item(index)
    End Get
  End Property
  ReadOnly Property RwKey(ByVal index As Integer) As String
    Get
      RwKey = Reflexion.Key(index)
    End Get
  End Property
  Public Function Count() As Integer
    Count = Reflexion.Count
  End Function
  Public Sub clear()
    Reflexion.Clear()
  End Sub
  Public Overloads Sub Remove(ByVal RwKey As String)
    Reflexion.Remove(RwKey)
  End Sub
  Public Overloads Sub Remove(ByVal index As Integer)
    Reflexion.Remove(index)
  End Sub

  ReadOnly Property ContainsKey(ByVal RwKey As String) As Boolean
    Get
      ContainsKey = Reflexion.ContainsKey(RwKey)
    End Get
  End Property
  '
  'R-Werte Untergrund
  '
  Property RefUnt() As RefValue
    Get
      RefUnt = MnRefUnt
    End Get
    Set(ByVal value As RefValue)
      MnRefUnt = value
    End Set
  End Property
  '
  'R-Werte für Weiß/Schwarzmischung
  '
  Property RefMix() As RefValue
    Get
      RefMix = MnRefMix
    End Get
    Set(ByVal value As RefValue)
      MnRefMix = value
    End Set
  End Property
  '
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As RefValues
    clone = CType(Me.MemberwiseClone, RefValues)
    If Not MnRefUnt Is Nothing Then
      clone.MnRefUnt = MnRefUnt.clone
    End If
    If Not MnRefMix Is Nothing Then
      clone.MnRefMix = MnRefMix.clone
    End If
    If Not Reflexion Is Nothing Then
      clone.Reflexion = New DictionaryInd(Of String, RefValue)
      For i = 0 To Reflexion.Count - 1
        clone.Reflexion.Add(Reflexion.Keys(i), Reflexion(i).clone)
      Next
    End If
  End Function


End Class
