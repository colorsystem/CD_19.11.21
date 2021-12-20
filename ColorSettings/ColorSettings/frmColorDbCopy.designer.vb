<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmColorDbCopy
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
    Me.btnVoid = New System.Windows.Forms.Button()
    Me.splColorDBCopy = New System.Windows.Forms.SplitContainer()
    Me.btnFetchMisch = New System.Windows.Forms.Button()
    Me.btnFetchMessg = New System.Windows.Forms.Button()
    Me.btnCopyVoid = New System.Windows.Forms.Button()
    Me.btnMARKMisch = New System.Windows.Forms.Button()
    Me.btnMARKMessg = New System.Windows.Forms.Button()
    Me.lblMisch = New System.Windows.Forms.Label()
    Me.lblMessg = New System.Windows.Forms.Label()
    Me.chkDAT = New System.Windows.Forms.CheckBox()
    Me.lblVON = New System.Windows.Forms.Label()
    Me.txtBIS = New System.Windows.Forms.TextBox()
    Me.txtVON = New System.Windows.Forms.TextBox()
    Me.lblBIS = New System.Windows.Forms.Label()
    Me.splMessgMisch = New System.Windows.Forms.SplitContainer()
    Me.dbgMessg = New System.Windows.Forms.DataGridView()
    Me.dbgMisch = New System.Windows.Forms.DataGridView()
    CType(Me.splColorDBCopy, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splColorDBCopy.Panel1.SuspendLayout()
    Me.splColorDBCopy.Panel2.SuspendLayout()
    Me.splColorDBCopy.SuspendLayout()
    CType(Me.splMessgMisch, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splMessgMisch.Panel1.SuspendLayout()
    Me.splMessgMisch.Panel2.SuspendLayout()
    Me.splMessgMisch.SuspendLayout()
    CType(Me.dbgMessg, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.dbgMisch, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'btnVoid
    '
    Me.btnVoid.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnVoid.Location = New System.Drawing.Point(62, 61)
    Me.btnVoid.Name = "btnVoid"
    Me.btnVoid.Size = New System.Drawing.Size(211, 24)
    Me.btnVoid.TabIndex = 3
    Me.btnVoid.Text = "144"
    Me.btnVoid.UseVisualStyleBackColor = True
    '
    'splColorDBCopy
    '
    Me.splColorDBCopy.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splColorDBCopy.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.splColorDBCopy.Location = New System.Drawing.Point(0, 0)
    Me.splColorDBCopy.Name = "splColorDBCopy"
    '
    'splColorDBCopy.Panel1
    '
    Me.splColorDBCopy.Panel1.Controls.Add(Me.btnFetchMisch)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.btnFetchMessg)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.btnCopyVoid)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.btnMARKMisch)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.btnMARKMessg)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.lblMisch)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.lblMessg)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.chkDAT)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.lblVON)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.txtBIS)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.txtVON)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.lblBIS)
    Me.splColorDBCopy.Panel1.Controls.Add(Me.btnVoid)
    '
    'splColorDBCopy.Panel2
    '
    Me.splColorDBCopy.Panel2.Controls.Add(Me.splMessgMisch)
    Me.splColorDBCopy.Size = New System.Drawing.Size(778, 478)
    Me.splColorDBCopy.SplitterDistance = 319
    Me.splColorDBCopy.TabIndex = 4
    '
    'btnFetchMisch
    '
    Me.btnFetchMisch.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnFetchMisch.Enabled = False
    Me.btnFetchMisch.Location = New System.Drawing.Point(3, 336)
    Me.btnFetchMisch.Name = "btnFetchMisch"
    Me.btnFetchMisch.Size = New System.Drawing.Size(313, 30)
    Me.btnFetchMisch.TabIndex = 19
    Me.btnFetchMisch.Text = "2027"
    Me.btnFetchMisch.UseVisualStyleBackColor = True
    '
    'btnFetchMessg
    '
    Me.btnFetchMessg.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnFetchMessg.Enabled = False
    Me.btnFetchMessg.Location = New System.Drawing.Point(3, 195)
    Me.btnFetchMessg.Name = "btnFetchMessg"
    Me.btnFetchMessg.Size = New System.Drawing.Size(313, 30)
    Me.btnFetchMessg.TabIndex = 18
    Me.btnFetchMessg.Text = "2026"
    Me.btnFetchMessg.UseVisualStyleBackColor = True
    '
    'btnCopyVoid
    '
    Me.btnCopyVoid.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnCopyVoid.Enabled = False
    Me.btnCopyVoid.Location = New System.Drawing.Point(93, 422)
    Me.btnCopyVoid.Name = "btnCopyVoid"
    Me.btnCopyVoid.Size = New System.Drawing.Size(140, 30)
    Me.btnCopyVoid.TabIndex = 17
    Me.btnCopyVoid.Text = "2025"
    Me.btnCopyVoid.UseVisualStyleBackColor = True
    '
    'btnMARKMisch
    '
    Me.btnMARKMisch.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMARKMisch.Enabled = False
    Me.btnMARKMisch.Location = New System.Drawing.Point(93, 281)
    Me.btnMARKMisch.Name = "btnMARKMisch"
    Me.btnMARKMisch.Size = New System.Drawing.Size(140, 30)
    Me.btnMARKMisch.TabIndex = 16
    Me.btnMARKMisch.Text = "3670"
    Me.btnMARKMisch.UseVisualStyleBackColor = True
    '
    'btnMARKMessg
    '
    Me.btnMARKMessg.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMARKMessg.Enabled = False
    Me.btnMARKMessg.Location = New System.Drawing.Point(93, 147)
    Me.btnMARKMessg.Name = "btnMARKMessg"
    Me.btnMARKMessg.Size = New System.Drawing.Size(140, 30)
    Me.btnMARKMessg.TabIndex = 15
    Me.btnMARKMessg.Text = "3670"
    Me.btnMARKMessg.UseVisualStyleBackColor = True
    '
    'lblMisch
    '
    Me.lblMisch.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMisch.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMisch.Location = New System.Drawing.Point(59, 257)
    Me.lblMisch.Name = "lblMisch"
    Me.lblMisch.Size = New System.Drawing.Size(214, 21)
    Me.lblMisch.TabIndex = 14
    Me.lblMisch.Text = "406"
    Me.lblMisch.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblMessg
    '
    Me.lblMessg.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMessg.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMessg.Location = New System.Drawing.Point(59, 123)
    Me.lblMessg.Name = "lblMessg"
    Me.lblMessg.Size = New System.Drawing.Size(214, 21)
    Me.lblMessg.TabIndex = 0
    Me.lblMessg.Text = "403"
    Me.lblMessg.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'chkDAT
    '
    Me.chkDAT.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDAT.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkDAT.Location = New System.Drawing.Point(39, 12)
    Me.chkDAT.Name = "chkDAT"
    Me.chkDAT.Size = New System.Drawing.Size(246, 20)
    Me.chkDAT.TabIndex = 9
    Me.chkDAT.Text = "2030"
    Me.chkDAT.UseVisualStyleBackColor = False
    '
    'lblVON
    '
    Me.lblVON.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblVON.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblVON.Location = New System.Drawing.Point(36, 35)
    Me.lblVON.Name = "lblVON"
    Me.lblVON.Size = New System.Drawing.Size(41, 21)
    Me.lblVON.TabIndex = 10
    Me.lblVON.Text = "376"
    Me.lblVON.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblVON.Visible = False
    '
    'txtBIS
    '
    Me.txtBIS.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtBIS.Location = New System.Drawing.Point(204, 35)
    Me.txtBIS.Name = "txtBIS"
    Me.txtBIS.Size = New System.Drawing.Size(81, 20)
    Me.txtBIS.TabIndex = 13
    Me.txtBIS.Text = "31.12.2011"
    Me.txtBIS.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtBIS.Visible = False
    '
    'txtVON
    '
    Me.txtVON.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtVON.Location = New System.Drawing.Point(78, 35)
    Me.txtVON.Name = "txtVON"
    Me.txtVON.Size = New System.Drawing.Size(81, 20)
    Me.txtVON.TabIndex = 11
    Me.txtVON.Text = "01.01.2000"
    Me.txtVON.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtVON.Visible = False
    '
    'lblBIS
    '
    Me.lblBIS.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblBIS.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblBIS.Location = New System.Drawing.Point(162, 35)
    Me.lblBIS.Name = "lblBIS"
    Me.lblBIS.Size = New System.Drawing.Size(41, 21)
    Me.lblBIS.TabIndex = 12
    Me.lblBIS.Text = "377"
    Me.lblBIS.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblBIS.Visible = False
    '
    'splMessgMisch
    '
    Me.splMessgMisch.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splMessgMisch.Location = New System.Drawing.Point(0, 0)
    Me.splMessgMisch.Name = "splMessgMisch"
    Me.splMessgMisch.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splMessgMisch.Panel1
    '
    Me.splMessgMisch.Panel1.Controls.Add(Me.dbgMessg)
    '
    'splMessgMisch.Panel2
    '
    Me.splMessgMisch.Panel2.Controls.Add(Me.dbgMisch)
    Me.splMessgMisch.Size = New System.Drawing.Size(455, 478)
    Me.splMessgMisch.SplitterDistance = 239
    Me.splMessgMisch.TabIndex = 0
    '
    'dbgMessg
    '
    Me.dbgMessg.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgMessg.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgMessg.Location = New System.Drawing.Point(0, 0)
    Me.dbgMessg.Name = "dbgMessg"
    Me.dbgMessg.Size = New System.Drawing.Size(455, 239)
    Me.dbgMessg.TabIndex = 1
    '
    'dbgMisch
    '
    Me.dbgMisch.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgMisch.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgMisch.Location = New System.Drawing.Point(0, 0)
    Me.dbgMisch.Name = "dbgMisch"
    Me.dbgMisch.Size = New System.Drawing.Size(455, 235)
    Me.dbgMisch.TabIndex = 2
    '
    'frmColorDbCopy
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(778, 478)
    Me.Controls.Add(Me.splColorDBCopy)
    Me.Name = "frmColorDbCopy"
    Me.Text = "frmColorDbDelete"
    Me.splColorDBCopy.Panel1.ResumeLayout(False)
    Me.splColorDBCopy.Panel1.PerformLayout()
    Me.splColorDBCopy.Panel2.ResumeLayout(False)
    CType(Me.splColorDBCopy, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splColorDBCopy.ResumeLayout(False)
    Me.splMessgMisch.Panel1.ResumeLayout(False)
    Me.splMessgMisch.Panel2.ResumeLayout(False)
    CType(Me.splMessgMisch, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splMessgMisch.ResumeLayout(False)
    CType(Me.dbgMessg, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.dbgMisch, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents btnVoid As System.Windows.Forms.Button
  Friend WithEvents splColorDBCopy As System.Windows.Forms.SplitContainer
  Friend WithEvents splMessgMisch As System.Windows.Forms.SplitContainer
  Friend WithEvents lblMisch As System.Windows.Forms.Label
  Friend WithEvents lblMessg As System.Windows.Forms.Label
  Friend WithEvents chkDAT As System.Windows.Forms.CheckBox
  Friend WithEvents lblVON As System.Windows.Forms.Label
  Friend WithEvents txtBIS As System.Windows.Forms.TextBox
  Friend WithEvents txtVON As System.Windows.Forms.TextBox
  Friend WithEvents lblBIS As System.Windows.Forms.Label
  Friend WithEvents btnMARKMisch As System.Windows.Forms.Button
  Friend WithEvents btnMARKMessg As System.Windows.Forms.Button
  Friend WithEvents btnCopyVoid As System.Windows.Forms.Button
  Friend WithEvents dbgMessg As System.Windows.Forms.DataGridView
  Friend WithEvents dbgMisch As System.Windows.Forms.DataGridView
  Friend WithEvents btnFetchMisch As System.Windows.Forms.Button
  Friend WithEvents btnFetchMessg As System.Windows.Forms.Button
End Class
