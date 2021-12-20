<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmMTH
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
    Me.components = New System.ComponentModel.Container()
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmMTH))
    Me.btnORD = New System.Windows.Forms.Button()
    Me.BindingMTH = New System.Windows.Forms.BindingNavigator(Me.components)
    Me.ToolStripButton14 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton15 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton16 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton17 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton18 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
    Me.lblMTH = New System.Windows.Forms.Label()
    Me.lblMET = New System.Windows.Forms.Label()
    Me.lstANW = New System.Windows.Forms.ListBox()
    Me.lstANM = New System.Windows.Forms.ListBox()
    Me.lblANW = New System.Windows.Forms.Label()
    Me.lblANM = New System.Windows.Forms.Label()
    Me.cboMTH = New System.Windows.Forms.ComboBox()
    CType(Me.BindingMTH, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.BindingMTH.SuspendLayout()
    Me.SuspendLayout()
    '
    'btnORD
    '
    Me.btnORD.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnORD.Location = New System.Drawing.Point(576, 24)
    Me.btnORD.Name = "btnORD"
    Me.btnORD.Size = New System.Drawing.Size(148, 26)
    Me.btnORD.TabIndex = 47
    Me.btnORD.Text = "1999"
    Me.btnORD.UseVisualStyleBackColor = True
    '
    'BindingMTH
    '
    Me.BindingMTH.AddNewItem = Nothing
    Me.BindingMTH.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.BindingMTH.CountItem = Nothing
    Me.BindingMTH.DeleteItem = Me.ToolStripButton14
    Me.BindingMTH.Dock = System.Windows.Forms.DockStyle.None
    Me.BindingMTH.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton15, Me.ToolStripButton16, Me.ToolStripButton17, Me.ToolStripButton18, Me.ToolStripSeparator1, Me.ToolStripButton14})
    Me.BindingMTH.Location = New System.Drawing.Point(301, 25)
    Me.BindingMTH.MoveFirstItem = Me.ToolStripButton15
    Me.BindingMTH.MoveLastItem = Me.ToolStripButton18
    Me.BindingMTH.MoveNextItem = Me.ToolStripButton17
    Me.BindingMTH.MovePreviousItem = Me.ToolStripButton16
    Me.BindingMTH.Name = "BindingMTH"
    Me.BindingMTH.PositionItem = Nothing
    Me.BindingMTH.Size = New System.Drawing.Size(110, 25)
    Me.BindingMTH.TabIndex = 96
    Me.BindingMTH.Text = "BindingNavigator1"
    '
    'ToolStripButton14
    '
    Me.ToolStripButton14.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton14.Image = CType(resources.GetObject("ToolStripButton14.Image"), System.Drawing.Image)
    Me.ToolStripButton14.Name = "ToolStripButton14"
    Me.ToolStripButton14.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton14.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton14.Text = "Löschen"
    Me.ToolStripButton14.Visible = False
    '
    'ToolStripButton15
    '
    Me.ToolStripButton15.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton15.Image = CType(resources.GetObject("ToolStripButton15.Image"), System.Drawing.Image)
    Me.ToolStripButton15.Name = "ToolStripButton15"
    Me.ToolStripButton15.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton15.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton15.Text = "Erste verschieben"
    '
    'ToolStripButton16
    '
    Me.ToolStripButton16.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton16.Image = CType(resources.GetObject("ToolStripButton16.Image"), System.Drawing.Image)
    Me.ToolStripButton16.Name = "ToolStripButton16"
    Me.ToolStripButton16.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton16.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton16.Text = "Vorherige verschieben"
    '
    'ToolStripButton17
    '
    Me.ToolStripButton17.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton17.Image = CType(resources.GetObject("ToolStripButton17.Image"), System.Drawing.Image)
    Me.ToolStripButton17.Name = "ToolStripButton17"
    Me.ToolStripButton17.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton17.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton17.Text = "Nächste verschieben"
    '
    'ToolStripButton18
    '
    Me.ToolStripButton18.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton18.Image = CType(resources.GetObject("ToolStripButton18.Image"), System.Drawing.Image)
    Me.ToolStripButton18.Name = "ToolStripButton18"
    Me.ToolStripButton18.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton18.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton18.Text = "Letzte verschieben"
    '
    'ToolStripSeparator1
    '
    Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
    Me.ToolStripSeparator1.Size = New System.Drawing.Size(6, 25)
    '
    'lblMTH
    '
    Me.lblMTH.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMTH.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMTH.Location = New System.Drawing.Point(301, 66)
    Me.lblMTH.Name = "lblMTH"
    Me.lblMTH.Size = New System.Drawing.Size(423, 20)
    Me.lblMTH.TabIndex = 97
    Me.lblMTH.Text = "999"
    Me.lblMTH.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    '
    'lblMET
    '
    Me.lblMET.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMET.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMET.Location = New System.Drawing.Point(8, 66)
    Me.lblMET.Name = "lblMET"
    Me.lblMET.Size = New System.Drawing.Size(293, 20)
    Me.lblMET.TabIndex = 98
    Me.lblMET.Text = "421"
    Me.lblMET.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lstANW
    '
    Me.lstANW.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstANW.FormattingEnabled = True
    Me.lstANW.Location = New System.Drawing.Point(301, 126)
    Me.lstANW.Name = "lstANW"
    Me.lstANW.Size = New System.Drawing.Size(423, 251)
    Me.lstANW.TabIndex = 99
    '
    'lstANM
    '
    Me.lstANM.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstANM.FormattingEnabled = True
    Me.lstANM.Location = New System.Drawing.Point(301, 396)
    Me.lstANM.Name = "lstANM"
    Me.lstANM.Size = New System.Drawing.Size(423, 225)
    Me.lstANM.TabIndex = 100
    '
    'lblANW
    '
    Me.lblANW.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblANW.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblANW.Location = New System.Drawing.Point(9, 126)
    Me.lblANW.Name = "lblANW"
    Me.lblANW.Size = New System.Drawing.Size(293, 20)
    Me.lblANW.TabIndex = 101
    Me.lblANW.Text = "504"
    Me.lblANW.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblANM
    '
    Me.lblANM.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblANM.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblANM.Location = New System.Drawing.Point(9, 396)
    Me.lblANM.Name = "lblANM"
    Me.lblANM.Size = New System.Drawing.Size(293, 20)
    Me.lblANM.TabIndex = 102
    Me.lblANM.Text = "505"
    Me.lblANM.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'cboMTH
    '
    Me.cboMTH.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMTH.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.cboMTH.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboMTH.FormattingEnabled = True
    Me.cboMTH.Location = New System.Drawing.Point(301, 89)
    Me.cboMTH.Name = "cboMTH"
    Me.cboMTH.Size = New System.Drawing.Size(423, 21)
    Me.cboMTH.TabIndex = 104
    '
    'frmMTH
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.BackColor = System.Drawing.Color.Silver
    Me.ClientSize = New System.Drawing.Size(775, 662)
    Me.ControlBox = False
    Me.Controls.Add(Me.cboMTH)
    Me.Controls.Add(Me.lblANM)
    Me.Controls.Add(Me.lblANW)
    Me.Controls.Add(Me.lstANM)
    Me.Controls.Add(Me.lstANW)
    Me.Controls.Add(Me.lblMET)
    Me.Controls.Add(Me.lblMTH)
    Me.Controls.Add(Me.BindingMTH)
    Me.Controls.Add(Me.btnORD)
    Me.Name = "frmMTH"
    Me.Text = "160"
    CType(Me.BindingMTH, System.ComponentModel.ISupportInitialize).EndInit()
    Me.BindingMTH.ResumeLayout(False)
    Me.BindingMTH.PerformLayout()
    Me.ResumeLayout(False)
    Me.PerformLayout()

  End Sub
  Friend WithEvents btnORD As System.Windows.Forms.Button
  Friend WithEvents BindingMTH As System.Windows.Forms.BindingNavigator
  Friend WithEvents ToolStripButton14 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton15 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton16 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton17 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton18 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
  Friend WithEvents lblMTH As System.Windows.Forms.Label
  Friend WithEvents lblMET As System.Windows.Forms.Label
  Friend WithEvents lstANW As System.Windows.Forms.ListBox
  Friend WithEvents lstANM As System.Windows.Forms.ListBox
  Friend WithEvents lblANW As System.Windows.Forms.Label
  Friend WithEvents lblANM As System.Windows.Forms.Label
  Friend WithEvents cboMTH As System.Windows.Forms.ComboBox
End Class
