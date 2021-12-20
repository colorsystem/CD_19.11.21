Option Strict Off
Option Explicit On
Option Compare Text
Public Class OpticalData
  Implements IDisposable
  Implements ICloneable

  Dim MnOptID As Integer 'Farbmittel ID
  Dim MnGKID As Short 'ID für GK-Werte
  Dim MnNpx As Short 'Anzahl Streu- und Absorptionskoeffizienten pro Winkel und Wellenlänge
  Dim MnDatTim As Date 'Datum + Zeit
  Dim MnFest As Short 'Farbmittel ist fest (1)
  Dim MnNst As Short 'Anzahl Stützstellen
  Dim MnCst() As Single 'Stützstellen (Konzentrationen*Dicke)
  Private MnGrund As CurvesRefGrp 'Grunddaten
  '
  '
  Property OptID() As Integer
    Get
      OptID = MnOptID
    End Get
    Set(ByVal Value As Integer)
      MnOptID = Value
    End Set
  End Property
  Property Fest() As Short
    Get
      Fest = MnFest
    End Get
    Set(ByVal Value As Short)
      MnFest = Value
    End Set
  End Property
  Property GKID() As Short
    Get
      GKID = MnGKID
    End Get
    Set(ByVal Value As Short)
      MnGKID = Value
    End Set
  End Property
  Property Npx() As Short
    Get
      Npx = MnNpx
    End Get
    Set(ByVal Value As Short)
      MnNpx = Value
    End Set
  End Property

  Property Nst() As Short
    Get
      Nst = MnNst
    End Get
    Set(ByVal Value As Short)
      MnNst = Value
      ReDim Preserve MnCst(MnNst)
    End Set
  End Property
  ReadOnly Property Cst() As Single()
    Get
      Cst = MnCst
    End Get
  End Property

  Property DatTim() As Date
    Get
      DatTim = MnDatTim
    End Get
    Set(ByVal Value As Date)
      MnDatTim = Value
    End Set
  End Property

  '
  Property Grund() As CurvesRefGrp
    Get
      Grund = MnGrund
    End Get
    Set(ByVal Value As CurvesRefGrp)
      MnGrund = Value
    End Set
  End Property
  '
  '


  Public Sub New()
    MyBase.New()
    Grund = New CurvesRefGrp
    MnNst = 0
    MnFest = 0
    MnOptID = -1
  End Sub


  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    Grund = Nothing
  End Sub
  '
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As OpticalData
    clone = CType(Me.MemberwiseClone, OpticalData)
    If Not MnCst Is Nothing Then
      clone.MnCst = MnCst.Clone
    End If
    If Not MnGrund Is Nothing Then
      clone.MnGrund = MnGrund.Clone
    End If
    '
    '
  End Function
End Class