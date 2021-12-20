Option Explicit On 
Option Compare Text
Option Strict Off
Public Class HandleCheckRad
  Implements IDisposable
  Public Sub New()
    disposed = False
  End Sub

  Protected Overrides Sub Finalize()

    MyBase.Finalize()
    dispose()

  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If disposed Then Exit Sub
    MnchkUUU = Nothing
    MnradUUU = Nothing
    MnradNLA = Nothing
    MnchkWIN = Nothing
    MnradWIN = Nothing
    disposed = True
  End Sub
  '
  '
  '
  '
  Dim index As Short
  Dim i As Short
  Dim disposed As Boolean
  '
  '
  '
  Dim MnchkUUU As New List(Of CheckBox)
  Dim WithEvents chkUUU_0 As CheckBox
  Dim WithEvents chkUUU_1 As CheckBox

  Dim MnradUUU As New ArrayList
  Dim WithEvents radUUU_0 As RadioButton
  Dim WithEvents radUUU_1 As RadioButton

  Dim MnradNLA As New ArrayList
  Dim WithEvents radNLA_0 As RadioButton
  Dim WithEvents radNLA_1 As RadioButton
  Dim WithEvents radNLA_2 As RadioButton
  Dim WithEvents radNLA_3 As RadioButton
  Dim WithEvents radNLA_4 As RadioButton
  Dim WithEvents radNLA_5 As RadioButton

  Dim MnchkWIN As New ArrayList
  Dim WithEvents chkWIN_0 As CheckBox
  Dim WithEvents chkWIN_1 As CheckBox
  Dim WithEvents chkWIN_2 As CheckBox
  Dim WithEvents chkWIN_3 As CheckBox
  Dim WithEvents chkWIN_4 As CheckBox
  Dim WithEvents chkWIN_5 As CheckBox
  Dim WithEvents chkWIN_6 As CheckBox
  Dim WithEvents chkWIN_7 As CheckBox
  Dim WithEvents chkWIN_8 As CheckBox

  Dim MnradWIN As New ArrayList
  Dim WithEvents radWIN_0 As RadioButton
  Dim WithEvents radWIN_1 As RadioButton
  Dim WithEvents radWIN_2 As RadioButton
  Dim WithEvents radWIN_3 As RadioButton
  Dim WithEvents radWIN_4 As RadioButton
  Dim WithEvents radWIN_5 As RadioButton
  Dim WithEvents radWIN_6 As RadioButton
  Dim WithEvents radWIN_7 As RadioButton
  Dim WithEvents radWIN_8 As RadioButton
  Dim WithEvents MncboSKAL As ComboBox
  Dim MnPicGraphic As HandleRezGrafik
  Dim MnPicauf As HandlePictures
  Dim MnFillGrid As HandleFillGrid
  '
  '
  '
  '
  'Properties
  '
  '
  '

  '
  '
  '
  '
  'Relexionswerte für Untergründe
  '
  '
  ReadOnly Property chkUUU() As List(Of CheckBox)
    Get
      chkUUU = MnchkUUU
    End Get
  End Property
  WriteOnly Property AddchkUUU() As CheckBox
    Set(ByVal Value As CheckBox)
      MnchkUUU.Add(Value)
      Select Case Value.Name
        Case "chkUUU_0"
          chkUUU_0 = Value
        Case "chkUUU_1"
          chkUUU_1 = Value
      End Select
    End Set
  End Property
  '
  '
  '
  '
  'Untergrund für Farbwerte
  '
  '
  '
  '
  ReadOnly Property radUUU() As ArrayList
    Get
      radUUU = MnradUUU
    End Get
  End Property
  WriteOnly Property AddradUUU() As RadioButton
    Set(ByVal Value As RadioButton)
      MnradUUU.Add(Value)
      Select Case Value.Name
        Case "radUUU_0"
          radUUU_0 = Value
        Case "radUUU_1"
          radUUU_1 = Value
      End Select
    End Set
  End Property
  '
  '
  '
  '
  'Normlichtart für Farbwerte
  '
  '
  '
  ReadOnly Property radNLA() As ArrayList
    Get
      radNLA = MnradNLA
    End Get
  End Property
  WriteOnly Property AddradNLA() As RadioButton
    Set(ByVal Value As RadioButton)
      MnradNLA.Add(Value)
      Select Case Value.Name
        Case "radNLA_0"
          radNLA_0 = Value
        Case "radNLA_1"
          radNLA_1 = Value
        Case "radNLA_2"
          radNLA_2 = Value
        Case "radNLA_3"
          radNLA_3 = Value
        Case "radNLA_4"
          radNLA_4 = Value
        Case "radNLA_5"
          radNLA_5 = Value
      End Select
    End Set
  End Property
  '
  '
  '
  '
  'Winkel für Reflexionswerte
  '
  '
  '
  ReadOnly Property chkWIN() As ArrayList
    Get
      chkWIN = MnchkWIN
    End Get
  End Property

  WriteOnly Property AddchkWIN() As CheckBox
    Set(ByVal Value As CheckBox)
      MnchkWIN.Add(Value)
      Select Case Value.Name
        Case "chkWIN_0"
          chkWIN_0 = Value
        Case "chkWIN_1"
          chkWIN_1 = Value
        Case "chkWIN_2"
          chkWIN_2 = Value
        Case "chkWIN_3"
          chkWIN_3 = Value
        Case "chkWIN_4"
          chkWIN_4 = Value
        Case "chkWIN_5"
          chkWIN_5 = Value
        Case "chkWIN_6"
          chkWIN_6 = Value
        Case "chkWIN_7"
          chkWIN_7 = Value
        Case "chkWIN_8"
          chkWIN_8 = Value
      End Select
    End Set
  End Property
  '
  'Winkel für Farbwerte
  '
  '
  ReadOnly Property radWIN() As ArrayList
    Get
      radWIN = MnradWIN
    End Get
  End Property
  WriteOnly Property AddradWIN() As RadioButton
    Set(ByVal Value As RadioButton)
      MnradWIN.Add(Value)
      Select Case Value.Name
        Case "radWIN_0"
          radWIN_0 = Value
        Case "radWIN_1"
          radWIN_1 = Value
        Case "radWIN_2"
          radWIN_2 = Value
        Case "radWIN_3"
          radWIN_3 = Value
        Case "radWIN_4"
          radWIN_4 = Value
        Case "radWIN_5"
          radWIN_5 = Value
        Case "radWIN_6"
          radWIN_6 = Value
        Case "radWIN_7"
          radWIN_7 = Value
        Case "radWIN_8"
          radWIN_8 = Value
      End Select
    End Set
  End Property
  Property FillGrid() As HandleFillGrid
    Get
      FillGrid = MnFillGrid
    End Get
    Set(ByVal Value As HandleFillGrid)
      MnFillGrid = Value
    End Set
  End Property


  Property PicGraphic() As HandleRezGrafik
    Get
      PicGraphic = MnPicGraphic
    End Get
    Set(ByVal Value As HandleRezGrafik)
      Dim i As Integer
      MnPicGraphic = Value
      '
      '
      'Startwerte setzen
      '
      '
      '
      For i = 0 To chkUUU.Count - 1
        MnPicGraphic.WeSc(i) = chkUUU(i).Checked
      Next
      For i = 0 To chkWIN.Count - 1
        MnPicGraphic.kwopt(index) = chkWIN(i).checked
      Next
      For i = 0 To radNLA.Count - 1
        If radNLA(i).checked Then
          MnPicGraphic.Knlz = i
          Exit For
        End If
      Next
      For i = 0 To radUUU.Count - 1
        If radUUU(i).checked Then
          MnPicGraphic.WSOpt = i
          Exit For
        End If
      Next
    End Set
  End Property
  '
  'Pictureboxes
  '
  '
  '
  Property Picauf() As HandlePictures
    Get
      Picauf = MnPicauf
    End Get
    Set(ByVal Value As HandlePictures)
      MnPicauf = Value
    End Set
  End Property
  '
  '
 
  
  'Skalierung
  '
  '
  '
  '
  Property cboSKAL() As ComboBox
    Get
      cboSKAL = MncboSKAL
    End Get
    Set(ByVal Value As ComboBox)
      MncboSKAL = Value
    End Set
  End Property
  '
  '
  '
  
  '
  '
  '
  '
  '
  '
  '
  'Events
  '
  '
  '
  '
  '
  '
  '

  Private Sub radUUU_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  radUUU_0.CheckedChanged, radUUU_1.CheckedChanged
    Dim index As Short
    index = CInt(sender.name.ToString.Substring(7, 1))
    MnPicGraphic.WSOpt = index
    '
    '
    '
    'Grafische Ausgabe auslösen
    '
    '
    '
    '
    '
    If Not IsNothing(MnPicauf) Then
      MnPicauf.PicFarbLAB.Refresh()
      MnPicauf.PicRezFarb.Refresh()
      MnPicauf.PicFarbXYZ.Refresh()
    End If
    If Not IsNothing(MnFillGrid) Then
      MnFillGrid.FillUeGrid()
    End If
  End Sub

  Private Sub radNLA_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  radNLA_0.CheckedChanged, radNLA_1.CheckedChanged, radNLA_2.CheckedChanged, _
  radNLA_3.CheckedChanged, radNLA_4.CheckedChanged, radNLA_5.CheckedChanged
    Dim index As Short
    index = CInt(sender.name.ToString.Substring(7, 1))
    MnPicGraphic.Knlz = index
    '
    '
    '
    'Grafische Ausgabe auslösen
    '
    '
    '
    '
    '
    If Not IsNothing(MnPicauf) Then
      If Not IsNothing(MnPicauf.PicFarbLAB) Then
        MnPicauf.PicFarbLAB.Refresh()
      End If
      If Not IsNothing(MnPicauf.PicRezFarb) Then
        MnPicauf.PicRezFarb.Refresh()
      End If
      If Not IsNothing(MnPicauf.PicFarbXYZ) Then
        MnPicauf.PicFarbXYZ.Refresh()
      End If
    End If
    If Not IsNothing(MnFillGrid) Then
      MnFillGrid.FillUeGrid()
    End If

  End Sub
  '
  '
  '
  '
  Private Sub radWIN_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  radWIN_0.CheckedChanged, radWIN_1.CheckedChanged, radWIN_2.CheckedChanged, _
  radWIN_3.CheckedChanged, radWIN_4.CheckedChanged, radWIN_5.CheckedChanged, _
  radWIN_6.CheckedChanged, radWIN_7.CheckedChanged, radWIN_8.CheckedChanged
    Dim index As Short

    index = CInt(sender.name.ToString.Substring(7, 1))
    MnPicGraphic.Kwop = index
    '
    '
    '
    'Grafische Ausgabe auslösen
    '
    '
    '
    '
    '
    If Not IsNothing(MnPicauf) Then
      If Not IsNothing(MnPicauf.PicFarbLAB) Then
        MnPicauf.PicFarbLAB.Refresh()
      End If
      If Not IsNothing(MnPicauf.PicRezFarb) Then
        MnPicauf.PicRezFarb.Refresh()
      End If
      If Not IsNothing(MnPicauf.PicFarbXYZ) Then
        MnPicauf.PicFarbXYZ.Refresh()
      End If
    End If
    If Not IsNothing(MnFillGrid) Then
      MnFillGrid.FillUeGrid()
    End If

  End Sub

  Private Sub chkWIN_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  chkWIN_0.CheckStateChanged, chkWIN_1.CheckStateChanged, chkWIN_2.CheckStateChanged, _
  chkWIN_3.CheckStateChanged, chkWIN_4.CheckStateChanged, chkWIN_5.CheckStateChanged, _
  chkWIN_6.CheckStateChanged, chkWIN_7.CheckStateChanged, chkWIN_8.CheckStateChanged
    Dim index As Short
    index = CInt(sender.name.ToString.Substring(7, 1))
    MnPicGraphic.kwopt(index) = chkWIN(index).checked
    '
    '
    '
    'Grafische Ausgabe auslösen
    '
    '
    '
    '
    '
    If Not IsNothing(MnPicauf) AndAlso Not IsNothing(MnPicauf.PicRwert) Then
      MnPicGraphic.Rmax = -1
      MnPicGraphic.Rmin = -1
      MnPicauf.PicRwert.Refresh()
    End If
  End Sub

  Private Sub chkUUU_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkUUU_0.CheckStateChanged, chkUUU_1.CheckStateChanged
    Dim Check(1) As Boolean
    Check(0) = chkUUU_0.Checked
    Check(1) = chkUUU_1.Checked
    Dim index As Short
    index = CInt(sender.name.ToString.Substring(7, 1))
    MnPicGraphic.WeSc(index) = chkUUU(index).Checked
    MnPicGraphic.Rmax = -1
    MnPicGraphic.Rmin = -1
    '
    'Zu plottende R-Werte festlegen
    '
    '
    '
    'Grafische Ausgabe auslösen
    '
    '
    '
    '
    '
    If Not IsNothing(MnPicauf) AndAlso Not IsNothing(MnPicauf.PicRwert) Then
      MnPicauf.PicRwert.Refresh()
    End If
  End Sub

  

  Private Sub MncboSKAL_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles MncboSKAL.SelectedIndexChanged
    'MnPicGraphic.Kwop = cboWIN.SelectedIndex
    '
    '
    '
    'Grafische Ausgabe auslösen
    '
    '
    '
    '
    '
    If Not IsNothing(MnPicauf) AndAlso Not IsNothing(MnPicauf.PicRwert) Then
      If MncboSKAL.Text = "" Then Exit Sub
      MnPicGraphic.Rmax = MncboSKAL.Text
      MnPicauf.PicRwert.Refresh()
    End If
  End Sub
End Class
