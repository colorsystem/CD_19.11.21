Option Strict Off
Option Explicit On
Option Compare Text
Public Class AngGeo
  Implements ICloneable
  Private MnLkm As Short '1,2......kwr
  Private MnIhrmID As Integer
  Private MnChrm As String 'Kennung für Meßgerät (String)
  Private MnIGlz As Short 'Glanzfaktor für verschiedene Geometrien
  Private MnIhrmGew As Single 'Gewichte für Winkel
  Private MnIhrmBez As String 'Bezeichnung
  Private MNGK(15) As Single
  '
  Sub New(ByVal IhrmID As Integer, ByVal Lkm As Short, ByVal Iglz As Short, ByVal IhrmBez As String, ByVal IhrmGew As Single, ByVal Chrm As String)
    MnIhrmID = IhrmID
    MnLkm = Lkm
    MnIGlz = Iglz
    MnIhrmGew = IhrmGew
    MnChrm = Chrm
    MnIhrmBez = IhrmBez
    MNGK(0) = 0.0
    MNGK(1) = 0.0
    MNGK(2) = 0.0
    MNGK(3) = 0.0
    MNGK(4) = 0.0
    MNGK(5) = 0.5
    MNGK(6) = 0.5
    MNGK(7) = 0.0
    MNGK(8) = 0.0
    MNGK(9) = 1.0
    MNGK(10) = 0.0
    MNGK(11) = 0.0
    MNGK(12) = 0.0
    MNGK(13) = 0.0
    MNGK(14) = 0.0
    MNGK(15) = 0.0
  End Sub
  '

  Property GK() As Single()
    Get
      GK = MNGK
    End Get
    Set(value As Single())
      MNGK = value
    End Set
  End Property

  '
  ReadOnly Property IhrmID() As Integer
    Get
      IhrmID = MnIhrmID
    End Get
  End Property

  ReadOnly Property Lkm() As Short
    Get
      Lkm = MnLkm
    End Get
  End Property
  ReadOnly Property Iglz() As Short
    Get
      Iglz = MnIGlz
    End Get
  End Property
  ReadOnly Property IhrmGew() As Single
    Get
      IhrmGew = MnIhrmGew
    End Get
  End Property
  ReadOnly Property Chrm() As String
    Get
      Chrm = MnChrm
    End Get
  End Property
  ReadOnly Property IhrmBez() As String
    Get
      IhrmBez = MnIhrmBez
    End Get
  End Property
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As AngGeo
    clone = CType(Me.MemberwiseClone, AngGeo)
  End Function

End Class