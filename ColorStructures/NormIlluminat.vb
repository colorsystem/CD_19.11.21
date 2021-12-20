Option Strict Off
Option Explicit On
Option Compare Text
Public Class NormIlluminat
    Implements IDisposable
  Private MnLichtID As Short 'Nummern der Normlichtarten
    Private MnNormKenn As String 'Kennungen für Normlichtarten
    Private MnNormNama As String 'Namen für Normlichtarten
    Private MnNormGew As Single 'Gewichte für Normlichtarten
    'UPGRADE_WARNING: Lower bound of array MnNormFakt was changed from 1 to 0. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
    Private MnNormFakt(2) As Single 'Faktoren für für Normlichtarten (3,:)
  Private MnNormKurven As CurvesRef




  Property LichtID() As Short
    Get
      LichtID = MnLichtID
    End Get
    Set(ByVal Value As Short)
      MnLichtID = Value
    End Set
  End Property
  Property NormKenn() As String
    Get
      NormKenn = MnNormKenn
    End Get
    Set(ByVal Value As String)
      MnNormKenn = Value
    End Set
  End Property
  Property NormNama() As String
    Get
      NormNama = MnNormNama
    End Get
    Set(ByVal Value As String)
      MnNormNama = Value
    End Set
  End Property
  Property NormGew() As Single
    Get
      NormGew = MnNormGew
    End Get
    Set(ByVal Value As Single)
      MnNormGew = Value
    End Set
  End Property
  ReadOnly Property NormFakt() As Single()
    Get
      NormFakt = MnNormFakt
    End Get
  End Property
  ReadOnly Property Normkurven() As CurvesRef
    Get
      Normkurven = MnNormKurven
    End Get
  End Property
  Public Sub New()
    MyBase.New()
    MnNormKurven = New CurvesRef
    MnLichtID = -1
  End Sub

    Sub dispose() Implements IDisposable.Dispose
        MnNormKurven = Nothing
    End Sub

    Protected Overrides Sub Finalize()
        MyBase.Finalize()
        dispose()
    End Sub
End Class