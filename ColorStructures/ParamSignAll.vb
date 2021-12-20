Option Strict Off
Option Explicit On
Option Compare Text
Public Class ParamSignAll
  Implements IDisposable
  Private CollParameter As DictionaryInd(Of String, ParamSign) 'Collection zur Characterisierung der Parameter
  Sub New()
    CollParameter = New DictionaryInd(Of String, ParamSign)

  End Sub
  Sub dispose() Implements IDisposable.Dispose
    CollParameter = Nothing
  End Sub
  '
  '
  '
  'Paramer zur Berechnung von Merkmalen
  '
  '
  '
  '
  '
  '
  Sub AddParam(ByVal Key As String, ByRef Param As ParamSign)
    CollParameter.Add(Key, Param)
  End Sub
  Public Sub RemoveParam(ByRef Key As String)
    CollParameter.Remove(Key)
  End Sub
  ReadOnly Property ContainsParamKey(ByVal Key As String) As Boolean
    Get
      ContainsParamKey = CollParameter.ContainsKey(Key)
    End Get
  End Property
  Default Overloads ReadOnly Property Param(ByVal Key As String) As ParamSign
    Get
      Param = CollParameter.Item(Key)
    End Get
  End Property

  Default Overloads ReadOnly Property Param(ByVal index As Integer) As ParamSign
    Get
      Param = CollParameter.Item(index)
    End Get
  End Property
  ReadOnly Property ParamKey(ByVal index As Integer) As String
    Get
      ParamKey = CollParameter.Key(index)
    End Get
  End Property
  ReadOnly Property ParamCount() As Integer
    Get
      ParamCount = CollParameter.Count()
    End Get
  End Property
  Sub ClearParam()
    CollParameter.Clear()
  End Sub

End Class
