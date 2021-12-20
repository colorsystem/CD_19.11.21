Option Strict Off
Option Explicit On
Option Compare Text
Public Class QuControls
  Implements ICloneable
  '
  'Qualitätsdaten
  '
  'Zusatzinformationen für Qualitätskontrolle
  Private MnMethId As Integer 'Kennung fuer Bedeutung der camp-Werte (Methodengruppe)
  Private MnIuntID As Integer 'Rwert_ID für Untergrund
  Private MnCamp(15) As Single 'Werte gemaess Meth_ID
  Private MnCart As String 'Art der Messung/Kennung fuer Untergrund (caru)
  Private MnMedNr As Short

  '
  '
  '
  Property MethID() As Integer
    Get
      MethID = MnMethId
    End Get
    Set(ByVal Value As Integer)
      MnMethId = Value
    End Set
  End Property
  '
  '
  '
  Property MedNr() As Short
    Get
      MedNr = MnMedNr
    End Get
    Set(ByVal Value As Short)
      MnMedNr = Value
    End Set
  End Property


  Property IuntID() As Integer
    Get
      IuntID = MnIuntID
    End Get
    Set(ByVal Value As Integer)
      MnIuntID = Value
    End Set
  End Property
  ReadOnly Property Camp() As Single()
    Get
      Camp = MnCamp
    End Get
  End Property
  Property Cart() As String
    Get
      Cart = MnCart
    End Get
    Set(ByVal Value As String)
      MnCart = Value
    End Set
  End Property


  Public Sub New()
    MyBase.New()
    Dim i As Short
    MnCart = "    "
    MnMethId = -1
    MnIuntID = -1
    MnMedNr = -1
    For i = 0 To 15
      MnCamp(i) = -1.0#
    Next i
  End Sub
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As QuControls
    clone = CType(Me.MemberwiseClone, QuControls)
  End Function
End Class