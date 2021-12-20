Public Class DictionaryInd(Of Tkey, Tvalue)
  Inherits Dictionary(Of Tkey, Tvalue)
  Implements ICloneable
  Default Overloads Property Item(ByVal Index As Integer) As Tvalue
    Get
      Item = Item(Key(Index))
    End Get
    Set(ByVal value As Tvalue)
      Item(Key(Index)) = value
    End Set
  End Property
  Overloads Sub Remove(ByVal Index As Integer)
    Remove(Key(index))
  End Sub
  ReadOnly Property Key(ByVal Index As Integer) As Tkey
    Get
      Dim Seach As Tkey
      Dim SKey As Tkey
      Dim i As Integer
      SKey = Nothing
      i = 0
      For Each Seach In Keys
        If i = index Then
          SKey = Seach
          Exit For
        End If
        i = i + 1
      Next
      Key = SKey
    End Get
  End Property
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Dim e As DictionaryInd(Of Tkey, Tvalue) = CType(Me.MemberwiseClone, DictionaryInd(Of Tkey, Tvalue))
    Return e
  End Function
  Function clone() As DictionaryInd(Of Tkey, Tvalue)
    clone = CType(Me.MemberwiseClone, DictionaryInd(Of Tkey, Tvalue))
  End Function
End Class