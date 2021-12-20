<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmReport
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
    Me.components = New System.ComponentModel.Container
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmReport))
    Me.picFarb = New System.Windows.Forms.PictureBox
    Me.PrintPreviewDialog1 = New System.Windows.Forms.PrintPreviewDialog
    Me.MenuStrip1 = New System.Windows.Forms.MenuStrip
    Me.I78i9ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
    Me.IToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem
    Me.ToolBarArray1 = New Microsoft.VisualBasic.Compatibility.VB6.ToolBarArray(Me.components)
    Me.PrintDialogArray1 = New Microsoft.VisualBasic.Compatibility.VB6.PrintDialogArray(Me.components)
    CType(Me.picFarb, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.MenuStrip1.SuspendLayout()
    CType(Me.ToolBarArray1, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.PrintDialogArray1, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'picFarb
    '
    Me.picFarb.Image = Global.MankiewiczAusgabe.My.Resources.Resources.MCS
    Me.picFarb.Location = New System.Drawing.Point(116, 98)
    Me.picFarb.Name = "picFarb"
    Me.picFarb.Size = New System.Drawing.Size(150, 138)
    Me.picFarb.TabIndex = 2
    Me.picFarb.TabStop = False
    '
    'PrintPreviewDialog1
    '
    Me.PrintPreviewDialog1.AutoScrollMargin = New System.Drawing.Size(0, 0)
    Me.PrintPreviewDialog1.AutoScrollMinSize = New System.Drawing.Size(0, 0)
    Me.PrintPreviewDialog1.ClientSize = New System.Drawing.Size(400, 300)
    Me.PrintPreviewDialog1.Enabled = True
    Me.PrintPreviewDialog1.Icon = CType(resources.GetObject("PrintPreviewDialog1.Icon"), System.Drawing.Icon)
    Me.PrintPreviewDialog1.MainMenuStrip = Me.MenuStrip1
    Me.PrintPreviewDialog1.Name = "PrintPreviewDialog1"
    Me.PrintPreviewDialog1.Visible = False
    '
    'MenuStrip1
    '
    Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.I78i9ToolStripMenuItem, Me.IToolStripMenuItem})
    Me.MenuStrip1.Location = New System.Drawing.Point(0, 0)
    Me.MenuStrip1.Name = "MenuStrip1"
    Me.MenuStrip1.Size = New System.Drawing.Size(1053, 24)
    Me.MenuStrip1.TabIndex = 3
    Me.MenuStrip1.Text = "MenuStrip1"
    '
    'I78i9ToolStripMenuItem
    '
    Me.I78i9ToolStripMenuItem.Name = "I78i9ToolStripMenuItem"
    Me.I78i9ToolStripMenuItem.Size = New System.Drawing.Size(55, 20)
    Me.I78i9ToolStripMenuItem.Text = "78i78i9"
    '
    'IToolStripMenuItem
    '
    Me.IToolStripMenuItem.Name = "IToolStripMenuItem"
    Me.IToolStripMenuItem.Size = New System.Drawing.Size(52, 20)
    Me.IToolStripMenuItem.Text = "78757i"
    '
    'frmReport
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(1053, 654)
    Me.Controls.Add(Me.picFarb)
    Me.Controls.Add(Me.MenuStrip1)
    Me.Name = "frmReport"
    Me.Text = "frmReport"
    CType(Me.picFarb, System.ComponentModel.ISupportInitialize).EndInit()
    Me.MenuStrip1.ResumeLayout(False)
    Me.MenuStrip1.PerformLayout()
    CType(Me.ToolBarArray1, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.PrintDialogArray1, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)
    Me.PerformLayout()

  End Sub
  Friend WithEvents picFarb As System.Windows.Forms.PictureBox
  Friend WithEvents PrintPreviewDialog1 As System.Windows.Forms.PrintPreviewDialog

  Private Sub frmReport_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    PrintPreviewDialog1.ShowDialog()
  End Sub
  Friend WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
  Friend WithEvents I78i9ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
  Friend WithEvents IToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
  Friend WithEvents ToolBarArray1 As Microsoft.VisualBasic.Compatibility.VB6.ToolBarArray
  Friend WithEvents PrintDialogArray1 As Microsoft.VisualBasic.Compatibility.VB6.PrintDialogArray
End Class
