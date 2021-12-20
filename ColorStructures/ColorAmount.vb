Option Strict Off
Option Explicit On
Option Compare Text
Public Class ColorAmount
  Implements ICloneable
  Private MnID As Integer 'Farbmittel ID
  Private MnFaAmng As Single 'Farb-/Bindemittelmenge(rein)
  Private MnBaAmng As Single 'Farb-/Bindemittelmenge(gemäß Normierung INF in Rezeptes))
  Private MnProz As Single 'Prozentigkeit Farbmittel
  Private MnProb As Single 'Prozentigkeit Bindemittel

  '

  '
  '
  Property ID() As Integer
    Get
      ID = MnID
    End Get
    Set(ByVal Value As Integer)
      MnID = Value
    End Set
  End Property
  '
  '
  '
  Property FaAmng() As Single
    Get
      FaAmng = MnFaAmng
    End Get
    Set(ByVal Value As Single)
      MnFaAmng = Value
    End Set
  End Property
  '
  '
  '
  Property BaAmng() As Single
    Get
      BaAmng = MnBaAmng
    End Get
    Set(ByVal Value As Single)
      MnBaAmng = Value
    End Set
  End Property
  '
  Property Proz() As Single
    Get
      Proz = MnProz
    End Get
    Set(ByVal Value As Single)
      If Value > 0.0# Then
        MnProz = Value
      End If
    End Set
  End Property

  Property Prob() As Single
    Get
      Prob = MnProb
    End Get
    Set(ByVal Value As Single)
      If Value >= 0.0# Then
        MnProb = Value
      End If
    End Set
  End Property
  '

  Public Sub New()
    MyBase.New()
    MnProb = 100.0#
    MnProz = 100.0#
    MnBaAmng = -1
    MnFaAmng = -1
    MnID = -1
  End Sub
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As ColorAmount
    clone = CType(Me.MemberwiseClone, ColorAmount)
  End Function
End Class