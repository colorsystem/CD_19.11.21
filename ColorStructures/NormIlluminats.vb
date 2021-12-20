Option Strict Off
Option Explicit On
Option Compare Text
Public Class NormIlluminats
    Implements IDisposable

  Private CollNormFarben As DictionaryInd(Of String, NormIlluminat) 'Gewichtsfaktoren für Normlichtarten (NWE,3,:)
    Public Sub New()
        MyBase.New()
    CollNormFarben = New DictionaryInd(Of String, NormIlluminat)
    End Sub

    Sub dispose() Implements IDisposable.Dispose
        CollNormFarben = Nothing
    End Sub

    Protected Overrides Sub Finalize()
        MyBase.Finalize()
        dispose()
    End Sub

    '
    '
    '
    Sub Add(ByVal nwe As Short, ByVal Key As String, ByRef value As NormIlluminat)
        CollNormFarben.Add(Key, value)
    CollNormFarben(Key).Normkurven.Add("X", New CurveRef(nwe))
    CollNormFarben(Key).Normkurven.Add("Y", New CurveRef(nwe))
    CollNormFarben(Key).Normkurven.Add("Z", New CurveRef(nwe))
    End Sub





    Default Overloads ReadOnly Property Item(ByVal Key As String) As NormIlluminat
    Get
      Item = CollNormFarben.Item(Key)
    End Get
    End Property

    Default Overloads ReadOnly Property Item(ByVal index As Integer) As NormIlluminat
    Get
      Item = CollNormFarben.Item(index)
    End Get
  End Property
  ReadOnly Property Key(ByVal index As Integer) As String
    Get
      Key = CollNormFarben.Key(index)
    End Get
  End Property
    Public Function Nlz() As Integer
        Nlz = CollNormFarben.Count
    End Function
    Public Sub clear()
        CollNormFarben.Clear()
    End Sub
    Public Sub Remove(ByVal key As String)
        CollNormFarben.Remove(key)
    End Sub
End Class