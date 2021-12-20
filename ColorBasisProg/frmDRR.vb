Option Strict Off
Option Explicit On
Option Compare Text
Public Class frmDRR
  Inherits System.Windows.Forms.Form
#Region "Vom Windows Form-Designer generierter Code "
  Public Sub New()
    MyBase.New()

    'Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()
  End Sub
  'Das Formular überschreibt den Löschvorgang, um die Komponentenliste zu bereinigen.
  Protected Overloads Overrides Sub Dispose(ByVal Disposing As Boolean)
    If Disposing Then
      If Not components Is Nothing Then
        components.Dispose()
      End If
    End If
    MyBase.Dispose(Disposing)
  End Sub
  'Wird vom Windows Form-Designer benötigt.
  Private components As System.ComponentModel.IContainer
  Public ToolTip1 As System.Windows.Forms.ToolTip
  Public WithEvents cmdEIN As System.Windows.Forms.Button
  'Hinweis: Die folgende Prozedur wird vom Windows Form-Designer benötigt.
  'Das Verändern mit dem Windows Form-Designer ist nicht möglich.
  'Das Verändern mit dem Code-Editor ist nicht möglich.
  Public WithEvents txtEIN_2 As System.Windows.Forms.TextBox
  Public WithEvents txtEIN_1 As System.Windows.Forms.TextBox
  Public WithEvents txtEIN_0 As System.Windows.Forms.TextBox
  Public WithEvents lblEIN_2 As System.Windows.Forms.Label
  Public WithEvents lblEIN_1 As System.Windows.Forms.Label
  Public WithEvents lblEIN_0 As System.Windows.Forms.Label
  <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
    Me.components = New System.ComponentModel.Container()
    Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
    Me.txtEIN_2 = New System.Windows.Forms.TextBox()
    Me.txtEIN_1 = New System.Windows.Forms.TextBox()
    Me.cmdEIN = New System.Windows.Forms.Button()
    Me.txtEIN_0 = New System.Windows.Forms.TextBox()
    Me.lblEIN_2 = New System.Windows.Forms.Label()
    Me.lblEIN_1 = New System.Windows.Forms.Label()
    Me.lblEIN_0 = New System.Windows.Forms.Label()
    Me.SuspendLayout()
    '
    'txtEIN_2
    '
    Me.txtEIN_2.AcceptsReturn = True
    Me.txtEIN_2.AutoSize = False
    Me.txtEIN_2.BackColor = System.Drawing.SystemColors.Window
    Me.txtEIN_2.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtEIN_2.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtEIN_2.Location = New System.Drawing.Point(192, 64)
    Me.txtEIN_2.MaxLength = 0
    Me.txtEIN_2.Name = "txtEIN_2"
    Me.txtEIN_2.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.txtEIN_2.Size = New System.Drawing.Size(337, 20)
    Me.txtEIN_2.TabIndex = 6
    Me.txtEIN_2.Text = "Text1"
    '
    'txtEIN_1
    '
    Me.txtEIN_1.AcceptsReturn = True
    Me.txtEIN_1.AutoSize = False
    Me.txtEIN_1.BackColor = System.Drawing.SystemColors.Window
    Me.txtEIN_1.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtEIN_1.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtEIN_1.Location = New System.Drawing.Point(192, 40)
    Me.txtEIN_1.MaxLength = 0
    Me.txtEIN_1.Name = "txtEIN_1"
    Me.txtEIN_1.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.txtEIN_1.Size = New System.Drawing.Size(337, 20)
    Me.txtEIN_1.TabIndex = 4
    Me.txtEIN_1.Text = "Text1"
    '
    'cmdEIN
    '
    Me.cmdEIN.BackColor = System.Drawing.SystemColors.Control
    Me.cmdEIN.Cursor = System.Windows.Forms.Cursors.Default
    Me.cmdEIN.ForeColor = System.Drawing.SystemColors.ControlText
    Me.cmdEIN.Location = New System.Drawing.Point(200, 96)
    Me.cmdEIN.Name = "cmdEIN"
    Me.cmdEIN.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.cmdEIN.Size = New System.Drawing.Size(33, 17)
    Me.cmdEIN.TabIndex = 2
    Me.cmdEIN.Text = "5"
    '
    'txtEIN_0
    '
    Me.txtEIN_0.AcceptsReturn = True
    Me.txtEIN_0.AutoSize = False
    Me.txtEIN_0.BackColor = System.Drawing.SystemColors.Window
    Me.txtEIN_0.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtEIN_0.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtEIN_0.Location = New System.Drawing.Point(192, 16)
    Me.txtEIN_0.MaxLength = 0
    Me.txtEIN_0.Name = "txtEIN_0"
    Me.txtEIN_0.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.txtEIN_0.Size = New System.Drawing.Size(337, 20)
    Me.txtEIN_0.TabIndex = 1
    Me.txtEIN_0.Text = "Text1"
    '
    'lblEIN_2
    '
    Me.lblEIN_2.BackColor = System.Drawing.SystemColors.Control
    Me.lblEIN_2.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblEIN_2.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblEIN_2.Location = New System.Drawing.Point(40, 64)
    Me.lblEIN_2.Name = "lblEIN_2"
    Me.lblEIN_2.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblEIN_2.Size = New System.Drawing.Size(145, 17)
    Me.lblEIN_2.TabIndex = 5
    Me.lblEIN_2.Text = "2212"
    Me.lblEIN_2.TextAlign = System.Drawing.ContentAlignment.TopRight
    '
    'lblEIN_1
    '
    Me.lblEIN_1.BackColor = System.Drawing.SystemColors.Control
    Me.lblEIN_1.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblEIN_1.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblEIN_1.Location = New System.Drawing.Point(40, 40)
    Me.lblEIN_1.Name = "lblEIN_1"
    Me.lblEIN_1.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblEIN_1.Size = New System.Drawing.Size(145, 17)
    Me.lblEIN_1.TabIndex = 3
    Me.lblEIN_1.Text = "2211"
    Me.lblEIN_1.TextAlign = System.Drawing.ContentAlignment.TopRight
    '
    'lblEIN_0
    '
    Me.lblEIN_0.BackColor = System.Drawing.SystemColors.Control
    Me.lblEIN_0.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblEIN_0.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblEIN_0.Location = New System.Drawing.Point(40, 16)
    Me.lblEIN_0.Name = "lblEIN_0"
    Me.lblEIN_0.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblEIN_0.Size = New System.Drawing.Size(145, 17)
    Me.lblEIN_0.TabIndex = 0
    Me.lblEIN_0.Text = "2210"
    Me.lblEIN_0.TextAlign = System.Drawing.ContentAlignment.TopRight
    '
    'frmDRR
    '
    Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
    Me.ClientSize = New System.Drawing.Size(544, 118)
    Me.Controls.AddRange(New System.Windows.Forms.Control() {Me.txtEIN_2, Me.txtEIN_1, Me.cmdEIN, Me.txtEIN_0, Me.lblEIN_2, Me.lblEIN_1, Me.lblEIN_0})
    Me.Location = New System.Drawing.Point(108, 155)
    Me.Name = "frmDRR"
    Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
    Me.Text = "2200"
    Me.ResumeLayout(False)

  End Sub
#End Region
#Region "Aktualisierungssupport "

#End Region
  Dim i As Short
  Private Sub cmdEIN_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdEIN.Click
    Me.Hide()
  End Sub

  Private Sub frmDRR_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
    cmdEIN.Text = Texxt(5)
    lblEIN_0.Text = Texxt(2210)
    lblEIN_1.Text = Texxt(2211)
    lblEIN_2.Text = Texxt(2212)
    txtEIN_0.Text = ""
    txtEIN_1.Text = ""
    txtEIN_2.Text = ""
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
  End Sub
End Class