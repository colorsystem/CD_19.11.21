Option Strict Off
Option Explicit On
Option Compare Text
Public Class Recipes
  Implements IDisposable
  Implements ICloneable
  'RezKey 'Schlüssel für Rezeptgruppe, z.B. Sortiment, berechnete Rezepte usw.
  Private CollRezpte As DictionaryInd(Of String, Recipe) 'Collection für Rezepte
  Private Mnkwb(1) As Short
  Public Sub New()
    MyBase.New()
    CollRezpte = New DictionaryInd(Of String, Recipe)
    Mnkwb(0) = -1
    Mnkwb(1) = -1
  End Sub

  Sub dispose() Implements IDisposable.Dispose
    CollRezpte = Nothing
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub

  '
  '
  '
  '
  
  '
  '
  '
  '
  Public Sub AddRez(ByVal RezKey As String, ByVal value As Recipe)
    CollRezpte.Add(RezKey, value)
  End Sub
  ReadOnly Property kwb() As Short()
    Get
      kwb = Mnkwb
    End Get
  End Property
  

  Default Overloads ReadOnly Property Item(ByVal RezKey As String) As Recipe
    Get
      Item = CollRezpte.Item(RezKey)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As Recipe
    Get
      Item = CollRezpte.Item(index)
    End Get
  End Property
  ReadOnly Property RezKey(ByVal Index As Integer) As String
    Get
      RezKey = CollRezpte.Key(Index)
    End Get
  End Property
  Public Function RezCount() As Integer
    RezCount = CollRezpte.Count
  End Function

  Public Sub clear()
    CollRezpte.Clear()
  End Sub
  Public Overloads Sub RemoveRez(ByVal RezKey As String)
    CollRezpte.Remove(RezKey)
  End Sub
  Public Overloads Sub RemoveRez(ByVal index As Integer)
    CollRezpte.Remove(index)
  End Sub
  ReadOnly Property ContainsKey(ByVal RezKey As String) As Boolean
    Get
      ContainsKey = CollRezpte.ContainsKey(RezKey)
    End Get
  End Property
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As Recipes
    clone = CType(Me.MemberwiseClone, Recipes)
    
    If Not CollRezpte Is Nothing Then
      clone.CollRezpte = New DictionaryInd(Of String, Recipe)
      For i = 0 To CollRezpte.Count - 1
        clone.CollRezpte.Add(CollRezpte.Keys(i), CollRezpte(i).clone)
      Next
    End If
  End Function
   
End Class


