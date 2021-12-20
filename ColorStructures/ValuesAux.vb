Option Strict Off
Option Explicit On
Option Compare Text

Public Class ValuesAux
  Implements IDisposable
  Private FaeChar As CurvesRef  'Färbecharacteristik
  Public Sub New()
    MyBase.New()
    FaeChar = New CurvesRef
  End Sub

  Sub dispose() Implements IDisposable.Dispose
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  
  '
  '
  '
  '
  'Key Auxiliarycollection
  '
  '
  '
  
  '
  '
  '
  
  '
  '
  'Färbecharacteristik
  '
  '
  Public Sub Add(ByVal FaeKey As String, ByVal value As CurveRef)
    FaeChar.Add(FaeKey, value)
  End Sub



  Default Overloads ReadOnly Property Item(ByVal FaeKey As String) As CurveRef
    Get
      Item = FaeChar.Item(FaeKey)
    End Get
  End Property

  Default Overloads ReadOnly Property Item(ByVal index As Integer) As CurveRef
    Get
      Item = FaeChar.Item(index)
    End Get
  End Property
  ReadOnly Property AuxKey(ByVal index As Integer) As String
    Get
      AuxKey = FaeChar.Winkey(index)
    End Get
  End Property
  Public Function Count() As Integer
    Count = FaeChar.Count
  End Function

  Public Sub clear()
    FaeChar.Clear()
  End Sub
  Public Overloads Sub Remove(ByVal FaeKey As String)
    FaeChar.Remove(FaeKey)
  End Sub
  Public Overloads Sub Remove(ByVal index As Integer)
    FaeChar.Remove(index)
  End Sub
End Class
