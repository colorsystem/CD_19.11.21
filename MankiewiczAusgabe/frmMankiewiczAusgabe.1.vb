Public Class frmMankiewiczAusgabe
  Dim MnDruckSelected As Integer
  Dim Sc As System.Windows.Forms.Screen = _
System.Windows.Forms.Screen.AllScreens(0)

  Public Sub New()

    ' Dieser Aufruf ist für den Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.

    lstMankiewiczAusgabe.ClearSelected()
    lstMankiewiczAusgabe.Items.Add("Ausdruck auswählen")
    lstMankiewiczAusgabe.Items.Add("Freigabeprotokoll Standard")
    lstMankiewiczAusgabe.Items.Add("NLD65,MI:F11+LED")
    lstMankiewiczAusgabe.Items.Add("DE(NLD65)")
    lstMankiewiczAusgabe.Items.Add("F11")
    lstMankiewiczAusgabe.Items.Add("Metallic")
    lstMankiewiczAusgabe.Items.Add("Etikettendruck")
    lstMankiewiczAusgabe.Items.Add("Abbrechen")
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