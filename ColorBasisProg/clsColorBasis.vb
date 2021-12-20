Option Strict Off
Option Explicit On
Option Compare Text
Public Class clsColorBasis

  

  
  WriteOnly Property GetPutRef() As HandleRwerte
    Set(ByVal value As HandleRwerte)
      GetPutReflex = value
    End Set
  End Property
  WriteOnly Property PrintSet() As PrinterSettings
    Set(ByVal value As PrinterSettings)
      MnPrintSet = value
    End Set
  End Property
  WriteOnly Property MDIForm() As Object
    Set(ByVal value As Object)
      FormMDI = value
    End Set
  End Property
  Property Resz() As Boolean
    Get
      Resz = MnResz
    End Get
    Set(ByVal value As Boolean)
      MnResz = value
    End Set
  End Property

End Class
