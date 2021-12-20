Option Strict Off
Option Explicit On
Option Compare Text
Public Class AnglExist
  Dim MnExist As Boolean 'Werte für entsprechenden Winkel sollen berechnet werden (Truie)
  '
  '
  '
  Public Sub New()
    MnExist = True
  End Sub
  '
  '
  '
  '
  '
   


  Property Exist() As Boolean
    Get
      Exist = MnExist
    End Get
    Set(ByVal value As Boolean)
      MnExist = value
    End Set
  End Property

End Class
