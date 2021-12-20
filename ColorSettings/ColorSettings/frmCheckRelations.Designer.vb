<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmCheckRelations
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
    Me.lblDatenbank = New System.Windows.Forms.Label()
    Me.btnAdjust = New System.Windows.Forms.Button()
    Me.btnIDNumber = New System.Windows.Forms.Button()
    Me.SuspendLayout()
    '
    'lblDatenbank
    '
    Me.lblDatenbank.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblDatenbank.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblDatenbank.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblDatenbank.Location = New System.Drawing.Point(135, 54)
    Me.lblDatenbank.Name = "lblDatenbank"
    Me.lblDatenbank.Size = New System.Drawing.Size(652, 33)
    Me.lblDatenbank.TabIndex = 11
    Me.lblDatenbank.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'btnAdjust
    '
    Me.btnAdjust.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnAdjust.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnAdjust.Location = New System.Drawing.Point(300, 202)
    Me.btnAdjust.Name = "btnAdjust"
    Me.btnAdjust.Size = New System.Drawing.Size(259, 79)
    Me.btnAdjust.TabIndex = 12
    Me.btnAdjust.Text = "2008"
    Me.btnAdjust.UseVisualStyleBackColor = True
    '
    'btnIDNumber
    '
    Me.btnIDNumber.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnIDNumber.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnIDNumber.Location = New System.Drawing.Point(300, 340)
    Me.btnIDNumber.Name = "btnIDNumber"
    Me.btnIDNumber.Size = New System.Drawing.Size(259, 79)
    Me.btnIDNumber.TabIndex = 13
    Me.btnIDNumber.Text = "2040"
    Me.btnIDNumber.UseVisualStyleBackColor = True
    '
    'frmCheckRelations
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(923, 476)
    Me.Controls.Add(Me.btnIDNumber)
    Me.Controls.Add(Me.lblDatenbank)
    Me.Controls.Add(Me.btnAdjust)
    Me.Name = "frmCheckRelations"
    Me.Text = "frmCheckRelations"
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents lblDatenbank As System.Windows.Forms.Label
  Friend WithEvents btnAdjust As System.Windows.Forms.Button
  Friend WithEvents btnIDNumber As System.Windows.Forms.Button
End Class
