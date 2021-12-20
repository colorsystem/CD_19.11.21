Option Strict Off
Option Explicit On
Option Compare Text
'<System.Runtime.InteropServices.ProgId("Kurve_NET.Kurve")> 
Public Class CurveRef
  Implements ICloneable
  '
  'Name zum Plotten
  '
  Dim MnNamPlott As String
  '
  'Nwe Anzahl Elemente von MnRwrt (z.B.Anzahl Wellenlängen)
  '
  'Schlüssel (nur über Kurven beschreibbar) z.B. Kennung für Winkel/Messgeometrie
  '
  '
  Private MnRwrt() As Single
  Sub New(ByVal Nwe As Short)
    Dim j As Short
    ReDim MnRwrt(Nwe - 1)
    For j = 0 To Nwe - 1
      MnRwrt(j) = 0.0
    Next
    MnNamPlott = ""
  End Sub
  Property NamPlott() As String
    Get
      NamPlott = MnNamPlott
    End Get
    Set(ByVal Value As String)
      MnNamPlott = Value
    End Set
  End Property
  ReadOnly Property R As Single()
    Get
      R = MnRwrt
    End Get
  End Property

  ReadOnly Property Nwe() As Short
    Get
      Nwe = UBound(MnRwrt) + 1
    End Get
  End Property
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As CurveRef
    clone = CType(Me.MemberwiseClone, CurveRef)
    If Not MnRwrt Is Nothing Then
      clone.MnRwrt = MnRwrt.Clone
    End If
  End Function
End Class