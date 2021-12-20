<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmMankiewiczAusgabe
    Inherits System.Windows.Forms.Form

    'Das Formular überschreibt den Löschvorgang, um die Komponentenliste zu bereinigen.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Wird vom Windows Form-Designer benötigt.
    Private components As System.ComponentModel.IContainer

    'Hinweis: Die folgende Prozedur ist für den Windows Form-Designer erforderlich.
    'Das Bearbeiten ist mit dem Windows Form-Designer möglich.  
    'Das Bearbeiten mit dem Code-Editor ist nicht möglich.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
    Me.lstMankiewiczAusgabe = New System.Windows.Forms.ListBox()
    Me.SuspendLayout()
    '
    'lstMankiewiczAusgabe
    '
    Me.lstMankiewiczAusgabe.Dock = System.Windows.Forms.DockStyle.Fill
    Me.lstMankiewiczAusgabe.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lstMankiewiczAusgabe.FormattingEnabled = True
    Me.lstMankiewiczAusgabe.ItemHeight = 20
    Me.lstMankiewiczAusgabe.Location = New System.Drawing.Point(0, 0)
    Me.lstMankiewiczAusgabe.Name = "lstMankiewiczAusgabe"
    Me.lstMankiewiczAusgabe.Size = New System.Drawing.Size(299, 182)
    Me.lstMankiewiczAusgabe.TabIndex = 0
    '
    'frmMankiewiczAusgabe
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(299, 182)
    Me.ControlBox = False
    Me.Controls.Add(Me.lstMankiewiczAusgabe)
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmMankiewiczAusgabe"
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents lstMankiewiczAusgabe As System.Windows.Forms.ListBox
End Class
