Public Class frmMankiewiczAusgabe
  Dim MnDruckSelected As Integer
  Dim Sc As System.Windows.Forms.Screen = _
System.Windows.Forms.Screen.AllScreens(0)

  Public Sub New()

    ' Dieser Aufruf ist für den Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.

    lstMankiewiczAusgabe.ClearSelected()
    lstMankiewiczAusgabe.Items.Add(texxt(29002))
    lstMankiewiczAusgabe.Items.Add(texxt(29003))
    lstMankiewiczAusgabe.Items.Add(texxt(29004))
    lstMankiewiczAusgabe.Items.Add(texxt(29005))
    lstMankiewiczAusgabe.Items.Add(texxt(29006))
    lstMankiewiczAusgabe.Items.Add(texxt(29007))
    lstMankiewiczAusgabe.Items.Add(texxt(29008))
    lstMankiewiczAusgabe.Items.Add(texxt(29009))
    lstMankiewiczAusgabe.SelectedIndex = 0
  End Sub
  ReadOnly Property DruckSelected() As Integer
    Get
      DruckSelected = MndruckSelected
    End Get
  End Property
  Private Sub lstMankiewiczAusgabe_SelectedIndexChanged(sender As Object, e As System.EventArgs) Handles lstMankiewiczAusgabe.SelectedIndexChanged
    MnDruckSelected = lstMankiewiczAusgabe.SelectedIndex
    DialogResult = Windows.Forms.DialogResult.OK
  End Sub

  Private Sub frmMankiewiczAusgabe_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Me.Location = New Point(0.5 * (Sc.Bounds.Width - Me.Bounds.Width), 0.5 * (Sc.Bounds.Height - Me.Bounds.Height))

  End Sub
End Class