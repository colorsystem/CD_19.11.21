Option Compare Text
Option Strict Off
Option Explicit On
Public Class ListTextID

  Dim MnID As Integer
  Dim MnText As String
  Sub New()
    MnText = ""
    MnID = 0
  End Sub
  Sub New(ByVal ValID As Integer, ByVal ValText As String)
    Me.MnID = ValID
    Me.MnText = ValText
  End Sub
  ReadOnly Property ID() As Integer
    Get
      ID = MnID
    End Get
  End Property
  ReadOnly Property Text() As String
    Get
      Text = MnText
    End Get
  End Property
  Public Overrides Function ToString() As String
    Return MnText
  End Function
End Class
Public Class ItemList
  Dim MnID As Integer
  Dim MnName As String
  Sub New(ByRef ID As Integer, ByRef Name As String)
    MnID = ID
    MnName = Name
  End Sub
  ReadOnly Property ID() As Integer
    Get
      ID = MnID
    End Get
  End Property
  ReadOnly Property Name() As String
    Get
      Name = MnName
    End Get
  End Property
End Class
Public Class ItemObject
  Dim MnID As Integer
  Dim MnObj As Object
  Sub New(ByRef ID As Integer, ByRef Obj As Object)
    MnID = ID
    MnObj = Obj
  End Sub
  ReadOnly Property ID() As Integer
    Get
      ID = MnID
    End Get
  End Property
  ReadOnly Property Obj() As Object
    Get
      Obj = MnObj
    End Get
  End Property
End Class

