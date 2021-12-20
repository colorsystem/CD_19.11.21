Public Class DictionaryBinding(Of Tvalue)
  Inherits BindingList(Of Tvalue)
  'Implements ICloneable
  Implements IDisposable
  Dim KeyList As List(Of String)
  Dim Index As Integer
  Sub dispose() Implements IDisposable.Dispose

  End Sub
  Public Overloads Sub ADD(key As String, Wert As Tvalue)
    If KeyList.IndexOf(key) = -1 Then
      KeyList.Add(key)
    Else
      MessageBox.Show("Key already exists: " & key)
      Exit Sub
    End If
    ADD(Wert)
  End Sub

  Default Overloads Property Item(ByVal key As String) As Tvalue
    Get
      Index = KeyList.IndexOf(key)
      If Index = -1 Then
        MessageBox.Show("Key not exists: " & key)
        Exit Property
      End If
      Item = Item(Index)
    End Get
    Set(value As Tvalue)
      Index = KeyList.IndexOf(key)
      If Index = -1 Then
        MessageBox.Show("Key does not exist: " & key)
        Exit Property
      End If
      Item(Index) = value
    End Set
  End Property

  Overloads Sub Remove(ByVal key As String)
    Index = KeyList.IndexOf(key)
    If Index = -1 Then
      MessageBox.Show("Key does not exist: " & key)
      Exit Sub
    End If
    Remove(Item(key))
    KeyList.RemoveAt(Index)
  End Sub
  Overloads Sub RemoveAT(ByVal indx As Integer)
    If indx > KeyList.Count - 1 Then
      MessageBox.Show("Index out of range: " & indx)
      Exit Sub
    End If
    Remove(KeyList(indx))
  End Sub
  ReadOnly Property Containskey(ByVal Key As String) As Boolean
    Get
      Index = KeyList.IndexOf(Key)
      If Index = -1 Then
        Containskey = False
      Else
        Containskey = True
      End If
    End Get
  End Property
  '
  '
  ReadOnly Property Keys(i As Integer) As String
    Get
      Keys = KeyList(i)
    End Get
  End Property
  '
  'BindingList hat im Gegensatz zu Arraylist keine clone-Schnittstelle
  'Shadows Function clone() As DictionaryBinding(Of Tvalue)
  ' Dim i As Integer
  '   clone = CType(Me.MemberwiseClone, DictionaryBinding(Of Tvalue))
  '   clone.KeyList = New List(Of String)
  '   For i = 0 To KeyList.Count - 1
  '     clone.KeyList.Add(KeyList(i))
  '   Next
  ' End Function


  Public Sub New()
    KeyList = New List(Of String)
  End Sub
End Class

