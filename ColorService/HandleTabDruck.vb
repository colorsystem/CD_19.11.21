Option Compare Text
Option Strict Off
Option Explicit On
Public Class HandleTabDruck
 
  Implements IDisposable
  '
  '
  'Standards für Graphicobjecte
  '
  '
  '
  '
  Dim Disposed As Boolean
  Dim Zeil As PrintZeil
  Sub New(ByRef Tabelle As DataTable)
    Zeil = New PrintZeil
    Zeil.FontZ = RezFont(2)
  End Sub
  Protected Overrides Sub Finalize()

    MyBase.Finalize()
    If Disposed Then Exit Sub
    dispose()

  End Sub
  Public Sub dispose() Implements IDisposable.Dispose
    If Disposed Then Exit Sub
    
    Disposed = True
  End Sub
End Class
