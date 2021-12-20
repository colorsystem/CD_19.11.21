<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmTestForm
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
    Me.DataGridTest = New System.Windows.Forms.DataGrid
    Me.ComboBox1 = New System.Windows.Forms.ComboBox
    CType(Me.DataGridTest, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'DataGridTest
    '
    Me.DataGridTest.DataMember = ""
    Me.DataGridTest.HeaderForeColor = System.Drawing.SystemColors.ControlText
    Me.DataGridTest.Location = New System.Drawing.Point(248, 137)
    Me.DataGridTest.Name = "DataGridTest"
    Me.DataGridTest.Size = New System.Drawing.Size(403, 197)
    Me.DataGridTest.TabIndex = 0
    '
    'ComboBox1
    '
    Me.ComboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.Simple
    Me.ComboBox1.FormattingEnabled = True
    Me.ComboBox1.Location = New System.Drawing.Point(243, 45)
    Me.ComboBox1.Name = "ComboBox1"
    Me.ComboBox1.Size = New System.Drawing.Size(150, 37)
    Me.ComboBox1.TabIndex = 1
    '
    'frmTestForm
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(779, 445)
    Me.Controls.Add(Me.ComboBox1)
    Me.Controls.Add(Me.DataGridTest)
    Me.Name = "frmTestForm"
    Me.Text = "frmTestForm"
    CType(Me.DataGridTest, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents DataGridTest As System.Windows.Forms.DataGrid
  Friend WithEvents ComboBox1 As System.Windows.Forms.ComboBox
End Class
