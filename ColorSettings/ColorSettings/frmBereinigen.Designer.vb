<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmBereinigen
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
    Me.btnEnd = New System.Windows.Forms.Button()
    Me.btnUpdate = New System.Windows.Forms.Button()
    Me.lblDatenbank = New System.Windows.Forms.Label()
    Me.btnAdjust = New System.Windows.Forms.Button()
    Me.lblPanFarbmittel = New System.Windows.Forms.Label()
    Me.radFarbmittel_0 = New System.Windows.Forms.RadioButton()
    Me.radFarbmittel_1 = New System.Windows.Forms.RadioButton()
    Me.radFarbmittel_2 = New System.Windows.Forms.RadioButton()
    Me.panFarbmittel = New System.Windows.Forms.Panel()
    Me.chkAdjust_0 = New System.Windows.Forms.CheckBox()
    Me.chkAdjust_1 = New System.Windows.Forms.CheckBox()
    Me.chkAdjust_2 = New System.Windows.Forms.CheckBox()
    Me.chkAdjust_3 = New System.Windows.Forms.CheckBox()
    Me.panAdjust = New System.Windows.Forms.Panel()
    Me.panFarbmittel.SuspendLayout
    Me.panAdjust.SuspendLayout
    Me.SuspendLayout
    '
    'btnEnd
    '
    Me.btnEnd.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnEnd.Location = New System.Drawing.Point(248, 464)
    Me.btnEnd.Name = "btnEnd"
    Me.btnEnd.Size = New System.Drawing.Size(182, 23)
    Me.btnEnd.TabIndex = 9
    Me.btnEnd.Text = "1999"
    Me.btnEnd.UseVisualStyleBackColor = true
    '
    'btnUpdate
    '
    Me.btnUpdate.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnUpdate.Enabled = false
    Me.btnUpdate.Font = New System.Drawing.Font("Microsoft Sans Serif", 12!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0,Byte))
    Me.btnUpdate.Location = New System.Drawing.Point(216, 406)
    Me.btnUpdate.Name = "btnUpdate"
    Me.btnUpdate.Size = New System.Drawing.Size(263, 52)
    Me.btnUpdate.TabIndex = 2
    Me.btnUpdate.Text = "387"
    Me.btnUpdate.UseVisualStyleBackColor = true
    '
    'lblDatenbank
    '
    Me.lblDatenbank.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left)  _
            Or System.Windows.Forms.AnchorStyles.Right),System.Windows.Forms.AnchorStyles)
    Me.lblDatenbank.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblDatenbank.Font = New System.Drawing.Font("Microsoft Sans Serif", 12!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0,Byte))
    Me.lblDatenbank.Location = New System.Drawing.Point(52, 118)
    Me.lblDatenbank.Name = "lblDatenbank"
    Me.lblDatenbank.Size = New System.Drawing.Size(652, 33)
    Me.lblDatenbank.TabIndex = 0
    Me.lblDatenbank.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'btnAdjust
    '
    Me.btnAdjust.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnAdjust.Font = New System.Drawing.Font("Microsoft Sans Serif", 12!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0,Byte))
    Me.btnAdjust.Location = New System.Drawing.Point(111, 176)
    Me.btnAdjust.Name = "btnAdjust"
    Me.btnAdjust.Size = New System.Drawing.Size(259, 79)
    Me.btnAdjust.TabIndex = 1
    Me.btnAdjust.Text = "2008"
    Me.btnAdjust.UseVisualStyleBackColor = true
    '
    'lblPanFarbmittel
    '
    Me.lblPanFarbmittel.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left)  _
            Or System.Windows.Forms.AnchorStyles.Right),System.Windows.Forms.AnchorStyles)
    Me.lblPanFarbmittel.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblPanFarbmittel.Location = New System.Drawing.Point(3, 0)
    Me.lblPanFarbmittel.Name = "lblPanFarbmittel"
    Me.lblPanFarbmittel.Size = New System.Drawing.Size(365, 21)
    Me.lblPanFarbmittel.TabIndex = 0
    Me.lblPanFarbmittel.Text = "3970"
    Me.lblPanFarbmittel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'radFarbmittel_0
    '
    Me.radFarbmittel_0.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left)  _
            Or System.Windows.Forms.AnchorStyles.Right),System.Windows.Forms.AnchorStyles)
    Me.radFarbmittel_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radFarbmittel_0.Checked = true
    Me.radFarbmittel_0.Location = New System.Drawing.Point(0, 24)
    Me.radFarbmittel_0.Name = "radFarbmittel_0"
    Me.radFarbmittel_0.Size = New System.Drawing.Size(368, 22)
    Me.radFarbmittel_0.TabIndex = 1
    Me.radFarbmittel_0.TabStop = true
    Me.radFarbmittel_0.Text = "3971"
    Me.radFarbmittel_0.UseVisualStyleBackColor = false
    '
    'radFarbmittel_1
    '
    Me.radFarbmittel_1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left)  _
            Or System.Windows.Forms.AnchorStyles.Right),System.Windows.Forms.AnchorStyles)
    Me.radFarbmittel_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radFarbmittel_1.Location = New System.Drawing.Point(0, 43)
    Me.radFarbmittel_1.Name = "radFarbmittel_1"
    Me.radFarbmittel_1.Size = New System.Drawing.Size(368, 22)
    Me.radFarbmittel_1.TabIndex = 2
    Me.radFarbmittel_1.Text = "3972"
    Me.radFarbmittel_1.UseVisualStyleBackColor = false
    '
    'radFarbmittel_2
    '
    Me.radFarbmittel_2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left)  _
            Or System.Windows.Forms.AnchorStyles.Right),System.Windows.Forms.AnchorStyles)
    Me.radFarbmittel_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radFarbmittel_2.Location = New System.Drawing.Point(0, 65)
    Me.radFarbmittel_2.Name = "radFarbmittel_2"
    Me.radFarbmittel_2.Size = New System.Drawing.Size(368, 22)
    Me.radFarbmittel_2.TabIndex = 3
    Me.radFarbmittel_2.Text = "3973"
    Me.radFarbmittel_2.UseVisualStyleBackColor = false
    '
    'panFarbmittel
    '
    Me.panFarbmittel.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.panFarbmittel.Controls.Add(Me.radFarbmittel_2)
    Me.panFarbmittel.Controls.Add(Me.radFarbmittel_1)
    Me.panFarbmittel.Controls.Add(Me.radFarbmittel_0)
    Me.panFarbmittel.Controls.Add(Me.lblPanFarbmittel)
    Me.panFarbmittel.Location = New System.Drawing.Point(111, 284)
    Me.panFarbmittel.Name = "panFarbmittel"
    Me.panFarbmittel.Size = New System.Drawing.Size(368, 87)
    Me.panFarbmittel.TabIndex = 10
    '
    'chkAdjust_0
    '
    Me.chkAdjust_0.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left)  _
            Or System.Windows.Forms.AnchorStyles.Right),System.Windows.Forms.AnchorStyles)
    Me.chkAdjust_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkAdjust_0.Checked = true
    Me.chkAdjust_0.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkAdjust_0.Location = New System.Drawing.Point(0, 2)
    Me.chkAdjust_0.Name = "chkAdjust_0"
    Me.chkAdjust_0.Size = New System.Drawing.Size(312, 19)
    Me.chkAdjust_0.TabIndex = 0
    Me.chkAdjust_0.UseVisualStyleBackColor = false
    '
    'chkAdjust_1
    '
    Me.chkAdjust_1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left)  _
            Or System.Windows.Forms.AnchorStyles.Right),System.Windows.Forms.AnchorStyles)
    Me.chkAdjust_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkAdjust_1.Checked = true
    Me.chkAdjust_1.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkAdjust_1.Location = New System.Drawing.Point(0, 22)
    Me.chkAdjust_1.Name = "chkAdjust_1"
    Me.chkAdjust_1.Size = New System.Drawing.Size(310, 19)
    Me.chkAdjust_1.TabIndex = 1
    Me.chkAdjust_1.UseVisualStyleBackColor = false
    '
    'chkAdjust_2
    '
    Me.chkAdjust_2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left)  _
            Or System.Windows.Forms.AnchorStyles.Right),System.Windows.Forms.AnchorStyles)
    Me.chkAdjust_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkAdjust_2.Checked = true
    Me.chkAdjust_2.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkAdjust_2.Location = New System.Drawing.Point(0, 41)
    Me.chkAdjust_2.Name = "chkAdjust_2"
    Me.chkAdjust_2.Size = New System.Drawing.Size(310, 19)
    Me.chkAdjust_2.TabIndex = 3
    Me.chkAdjust_2.UseVisualStyleBackColor = false
    '
    'chkAdjust_3
    '
    Me.chkAdjust_3.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left)  _
            Or System.Windows.Forms.AnchorStyles.Right),System.Windows.Forms.AnchorStyles)
    Me.chkAdjust_3.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkAdjust_3.Checked = true
    Me.chkAdjust_3.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkAdjust_3.Location = New System.Drawing.Point(-1, 60)
    Me.chkAdjust_3.Name = "chkAdjust_3"
    Me.chkAdjust_3.Size = New System.Drawing.Size(313, 19)
    Me.chkAdjust_3.TabIndex = 4
    Me.chkAdjust_3.UseVisualStyleBackColor = false
    '
    'panAdjust
    '
    Me.panAdjust.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.panAdjust.Controls.Add(Me.chkAdjust_3)
    Me.panAdjust.Controls.Add(Me.chkAdjust_2)
    Me.panAdjust.Controls.Add(Me.chkAdjust_1)
    Me.panAdjust.Controls.Add(Me.chkAdjust_0)
    Me.panAdjust.Location = New System.Drawing.Point(362, 176)
    Me.panAdjust.Name = "panAdjust"
    Me.panAdjust.Size = New System.Drawing.Size(313, 79)
    Me.panAdjust.TabIndex = 3
    '
    'frmBereinigen
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6!, 13!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(759, 545)
    Me.ControlBox = false
    Me.Controls.Add(Me.panFarbmittel)
    Me.Controls.Add(Me.btnEnd)
    Me.Controls.Add(Me.lblDatenbank)
    Me.Controls.Add(Me.panAdjust)
    Me.Controls.Add(Me.btnAdjust)
    Me.Controls.Add(Me.btnUpdate)
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
    Me.MaximizeBox = false
    Me.MinimizeBox = false
    Me.Name = "frmBereinigen"
    Me.Text = "frmDatenbank"
    Me.panFarbmittel.ResumeLayout(false)
    Me.panAdjust.ResumeLayout(false)
    Me.ResumeLayout(false)

End Sub
  Friend WithEvents lblDatenbank As System.Windows.Forms.Label
  Friend WithEvents btnAdjust As System.Windows.Forms.Button
  Friend WithEvents btnUpdate As System.Windows.Forms.Button
  Friend WithEvents btnEnd As System.Windows.Forms.Button
  Friend WithEvents lblPanFarbmittel As System.Windows.Forms.Label
  Friend WithEvents radFarbmittel_0 As System.Windows.Forms.RadioButton
  Friend WithEvents radFarbmittel_1 As System.Windows.Forms.RadioButton
  Friend WithEvents radFarbmittel_2 As System.Windows.Forms.RadioButton
  Friend WithEvents panFarbmittel As System.Windows.Forms.Panel
  Friend WithEvents chkAdjust_0 As System.Windows.Forms.CheckBox
  Friend WithEvents chkAdjust_1 As System.Windows.Forms.CheckBox
  Friend WithEvents chkAdjust_2 As System.Windows.Forms.CheckBox
  Friend WithEvents chkAdjust_3 As System.Windows.Forms.CheckBox
  Friend WithEvents panAdjust As System.Windows.Forms.Panel
End Class
