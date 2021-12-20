Option Explicit On 
Option Compare Text
Option Strict Off
Public Class HandlePictures
  Inherits System.Collections.CollectionBase
  Implements IDisposable

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()

  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If disposed Then Exit Sub
    disposed = True
  End Sub
  Dim disposed As Boolean
  Dim i As Short
  Dim ier As Integer
  Dim MnPicGraphic As HandleRezGrafik
  Dim WithEvents MnPicRwert As PictureBox
  Dim WithEvents MnPicFarbLAB As PictureBox
  Dim WithEvents MnPicFarbXYZ As PictureBox
  Dim WithEvents MnPicRezFarb As PictureBox
  Dim WithEvents MnPicRezRezept As PictureBox
  Dim WithEvents MnPicRezGrd As PictureBox
  Dim ReftoolTip As ToolTip
 
 
  ReadOnly Property PicRwert() As PictureBox
    Get
      PicRwert = MnPicRwert
    End Get
  End Property
  '
  ReadOnly Property PicFarbLAB() As PictureBox
    Get
      PicFarbLAB = MnPicFarbLAB
    End Get
  End Property

  ReadOnly Property PicFarbXYZ() As PictureBox
    Get
      PicFarbXYZ = MnPicFarbXYZ
    End Get
  End Property
  '
  '
  ReadOnly Property PicRezFarb() As PictureBox
    Get
      PicRezFarb = MnPicRezFarb
    End Get
  End Property
  ReadOnly Property PicRezRezept() As PictureBox
    Get
      PicRezRezept = MnPicRezRezept
    End Get
  End Property
  ReadOnly Property PicRezGrd() As PictureBox
    Get
      PicRezGrd = MnPicRezGrd
    End Get
  End Property
  '
  '
  '
  Default Property Picboxes(ByVal index As Integer) As PictureBox
    Get
      Return CType(InnerList.Item(index), PictureBox)
    End Get
    Set(ByVal Value As PictureBox)
      InnerList.Item(index) = Value
    End Set
  End Property
  Sub Add(ByVal KeyPic As String, ByVal value As PictureBox)
    InnerList.Add(CType(value, PictureBox))
    Select Case KeyPic
      Case "REF"
        MnPicRwert = value
      Case "LAB"
        MnPicFarbLAB = value
      Case "XYZ"
        MnPicFarbXYZ = value
      Case "REZ"
        MnPicRezRezept = value
      Case "FRB"
        MnPicRezFarb = value
      Case "GRD"
        MnPicRezGrd = value
    End Select
  End Sub


  Property PicGraphic() As HandleRezGrafik
    Get
      PicGraphic = MnPicGraphic
    End Get
    Set(ByVal Value As HandleRezGrafik)
      MnPicGraphic = Value
    End Set
  End Property
 

 
  Private Sub PicRwert_MouseMove(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles MnPicRwert.MouseMove
    Dim Wert As Single
    If IsNothing(sender) Then Exit Sub
    Wert = MnPicGraphic.GetRefWert(e.Location)
    If Wert > -1.0 Then
      ReftoolTip.SetToolTip(MnPicRwert, Format(Wert, "##0.00"))
    End If
  End Sub
  Private Sub PicRwert_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles MnPicRwert.Paint
    If IsNothing(MnPicGraphic) Then Exit Sub
    If IsNothing(sender) Then Exit Sub
    Try
      e.Graphics.Clear(Color.White)
      MnPicGraphic.GraphBounds = New RectangleF(0.0 * sender.width, 0.0 * sender.height, 1.0 * sender.width, 1.0 * sender.height)
      MnPicGraphic.PicREFGraph(sender, e.Graphics)
      MnPicGraphic.Skalier()
    Finally
    End Try
  End Sub
  Private Sub PicFarbLAB_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles MnPicFarbLAB.Paint
    If IsNothing(MnPicGraphic) Then Exit Sub
    If IsNothing(sender) Then Exit Sub
    Try
      e.Graphics.Clear(Color.White)
      MnPicGraphic.GraphBounds = New RectangleF(0.0 * sender.width, 0.0 * sender.height, 1.0 * sender.width, 1.0 * sender.height)
      MnPicGraphic.CalcFarbWrt()
      MnPicGraphic.PicLABGraph(sender, e.Graphics)
    Finally
    End Try
  End Sub
  Private Sub PicRezFarb_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles MnPicRezFarb.Paint
    If IsNothing(MnPicGraphic) Then Exit Sub
    If IsNothing(sender) Then Exit Sub
    Try
      e.Graphics.Clear(Color.White)
      MnPicGraphic.GraphBounds = New RectangleF(0.0 * sender.width, 0.0 * sender.height, 1.0 * sender.width, 1.0 * sender.height)
      MnPicGraphic.CalcFarbWrt()
      MnPicGraphic.picWRTGraph(sender, e.Graphics)
    Finally
    End Try
  End Sub
  Private Sub PicRezRezept_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles MnPicRezRezept.Paint
    If IsNothing(MnPicGraphic) Then Exit Sub
    If IsNothing(sender) Then Exit Sub
    Try
      e.Graphics.Clear(Color.White)
      MnPicGraphic.GraphBounds = New RectangleF(0.0 * sender.width, 0.0 * sender.height, 1.0 * sender.width, 1.0 * sender.height)
      MnPicGraphic.PicREZGraph(sender, e.Graphics)
    Finally
    End Try
  End Sub
  Private Sub PicFarbXYZ_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles MnPicFarbXYZ.Paint
    If IsNothing(MnPicGraphic) Then Exit Sub
    If IsNothing(sender) Then Exit Sub
    Try
      e.Graphics.Clear(Color.White)
      MnPicGraphic.GraphBounds = New RectangleF(0.0F, 0.0F, 1.0 * sender.width, 1.0 * sender.height)
      MnPicGraphic.CalcFarbWrt()
      MnPicGraphic.PicXYZGraph(sender, e.Graphics)
    Finally
    End Try
  End Sub
  Private Sub PicRezGrd_MouseMove(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles MnPicRezGrd.MouseMove
    Dim Wert As Single
    If IsNothing(sender) Then Exit Sub
    Wert = MnPicGraphic.GetGrdWert(e.Location)
    If Wert > -1.0 Then
      ReftoolTip.SetToolTip(MnPicRezGrd, Format(Wert, "###0.0000"))
    End If
  End Sub

  Private Sub PicRezGrd_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles MnPicRezGrd.Paint
    If IsNothing(MnPicGraphic) Then Exit Sub
    If IsNothing(sender) Then Exit Sub
    Try
      e.Graphics.Clear(Color.White)
      MnPicGraphic.GraphBounds = New RectangleF(0.0 * sender.width, 0.0 * sender.height, 1.0 * sender.width, 1.0 * sender.height)
      MnPicGraphic.PicGRDGraph(sender, e.Graphics)
    Finally
    End Try
  End Sub

  Sub Refresh()
    Dim i As Integer
    MnPicGraphic.Rmax = -1.0
    For i = 0 To InnerList.Count - 1
      If InnerList(i).visible Then
        'InnerList(i).show()
        If Not IsNothing(InnerList(i)) Then
          InnerList(i).Refresh()
        End If
      End If
    Next
  End Sub

  Private Sub MnPicGraphic_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  MnPicFarbXYZ.Resize, MnPicRwert.Resize, MnPicFarbLAB.Resize, MnPicRezFarb.Resize, MnPicRezRezept.Resize, MnPicRezGrd.Resize
    sender.refresh()
  End Sub

  Public Sub New()
    disposed = False
    ReftoolTip = New ToolTip
    ReftoolTip.AutoPopDelay = 500
    ReftoolTip.ShowAlways = True
  End Sub



End Class
'
'
'
'
'
'
