<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmWriteMessgMisch
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
    Me.splMessgMisch = New System.Windows.Forms.SplitContainer()
    Me.btnMischSelect = New System.Windows.Forms.Button()
    Me.btnMessgSelect = New System.Windows.Forms.Button()
    Me.lblDBCopy = New System.Windows.Forms.Label()
    Me.btnRezepte = New System.Windows.Forms.Button()
    Me.btnRwerte = New System.Windows.Forms.Button()
    Me.btnMisch = New System.Windows.Forms.Button()
    Me.btnMessg = New System.Windows.Forms.Button()
    Me.btnVoid = New System.Windows.Forms.Button()
    Me.lstMisch = New System.Windows.Forms.ListBox()
    Me.lstMessg = New System.Windows.Forms.ListBox()
    Me.txtMessgKE = New System.Windows.Forms.TextBox()
    Me.lblMessgKE = New System.Windows.Forms.Label()
    Me.dbgMessgeräte = New System.Windows.Forms.DataGridView()
    Me.dbgMeasure00x = New System.Windows.Forms.DataGridView()
    Me.lblShowMeasDevices = New System.Windows.Forms.Label()
    CType(Me.splMessgMisch, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splMessgMisch.Panel1.SuspendLayout()
    Me.splMessgMisch.Panel2.SuspendLayout()
    Me.splMessgMisch.SuspendLayout()
    CType(Me.dbgMessgeräte, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.dbgMeasure00x, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'splMessgMisch
    '
    Me.splMessgMisch.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splMessgMisch.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.splMessgMisch.Location = New System.Drawing.Point(0, 0)
    Me.splMessgMisch.Name = "splMessgMisch"
    '
    'splMessgMisch.Panel1
    '
    Me.splMessgMisch.Panel1.Controls.Add(Me.btnMischSelect)
    Me.splMessgMisch.Panel1.Controls.Add(Me.btnMessgSelect)
    Me.splMessgMisch.Panel1.Controls.Add(Me.lblDBCopy)
    Me.splMessgMisch.Panel1.Controls.Add(Me.btnRezepte)
    Me.splMessgMisch.Panel1.Controls.Add(Me.btnRwerte)
    Me.splMessgMisch.Panel1.Controls.Add(Me.btnMisch)
    Me.splMessgMisch.Panel1.Controls.Add(Me.btnMessg)
    Me.splMessgMisch.Panel1.Controls.Add(Me.btnVoid)
    Me.splMessgMisch.Panel1.Controls.Add(Me.lstMisch)
    Me.splMessgMisch.Panel1.Controls.Add(Me.lstMessg)
    '
    'splMessgMisch.Panel2
    '
    Me.splMessgMisch.Panel2.Controls.Add(Me.txtMessgKE)
    Me.splMessgMisch.Panel2.Controls.Add(Me.lblMessgKE)
    Me.splMessgMisch.Panel2.Controls.Add(Me.dbgMessgeräte)
    Me.splMessgMisch.Panel2.Controls.Add(Me.dbgMeasure00x)
    Me.splMessgMisch.Panel2.Controls.Add(Me.lblShowMeasDevices)
    Me.splMessgMisch.Size = New System.Drawing.Size(823, 561)
    Me.splMessgMisch.SplitterDistance = 462
    Me.splMessgMisch.TabIndex = 0
    '
    'btnMischSelect
    '
    Me.btnMischSelect.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMischSelect.Enabled = False
    Me.btnMischSelect.Location = New System.Drawing.Point(235, 98)
    Me.btnMischSelect.Name = "btnMischSelect"
    Me.btnMischSelect.Size = New System.Drawing.Size(211, 24)
    Me.btnMischSelect.TabIndex = 19
    Me.btnMischSelect.Text = "406 3670"
    Me.btnMischSelect.UseVisualStyleBackColor = True
    '
    'btnMessgSelect
    '
    Me.btnMessgSelect.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMessgSelect.Enabled = False
    Me.btnMessgSelect.Location = New System.Drawing.Point(12, 99)
    Me.btnMessgSelect.Name = "btnMessgSelect"
    Me.btnMessgSelect.Size = New System.Drawing.Size(211, 24)
    Me.btnMessgSelect.TabIndex = 18
    Me.btnMessgSelect.Text = "403 3670"
    Me.btnMessgSelect.UseVisualStyleBackColor = True
    '
    'lblDBCopy
    '
    Me.lblDBCopy.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblDBCopy.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblDBCopy.Location = New System.Drawing.Point(104, 13)
    Me.lblDBCopy.Name = "lblDBCopy"
    Me.lblDBCopy.Size = New System.Drawing.Size(278, 18)
    Me.lblDBCopy.TabIndex = 17
    Me.lblDBCopy.Text = "389 141 144"
    Me.lblDBCopy.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'btnRezepte
    '
    Me.btnRezepte.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnRezepte.Enabled = False
    Me.btnRezepte.Location = New System.Drawing.Point(239, 516)
    Me.btnRezepte.Name = "btnRezepte"
    Me.btnRezepte.Size = New System.Drawing.Size(211, 24)
    Me.btnRezepte.TabIndex = 16
    Me.btnRezepte.Text = "730 143"
    Me.btnRezepte.UseVisualStyleBackColor = True
    '
    'btnRwerte
    '
    Me.btnRwerte.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnRwerte.Enabled = False
    Me.btnRwerte.Location = New System.Drawing.Point(12, 516)
    Me.btnRwerte.Name = "btnRwerte"
    Me.btnRwerte.Size = New System.Drawing.Size(211, 24)
    Me.btnRwerte.TabIndex = 15
    Me.btnRwerte.Text = "3110 143"
    Me.btnRwerte.UseVisualStyleBackColor = True
    '
    'btnMisch
    '
    Me.btnMisch.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMisch.Enabled = False
    Me.btnMisch.Location = New System.Drawing.Point(235, 71)
    Me.btnMisch.Name = "btnMisch"
    Me.btnMisch.Size = New System.Drawing.Size(211, 24)
    Me.btnMisch.TabIndex = 14
    Me.btnMisch.Text = "406"
    Me.btnMisch.UseVisualStyleBackColor = True
    '
    'btnMessg
    '
    Me.btnMessg.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMessg.Enabled = False
    Me.btnMessg.Location = New System.Drawing.Point(12, 69)
    Me.btnMessg.Name = "btnMessg"
    Me.btnMessg.Size = New System.Drawing.Size(211, 24)
    Me.btnMessg.TabIndex = 13
    Me.btnMessg.Text = "403"
    Me.btnMessg.UseVisualStyleBackColor = True
    '
    'btnVoid
    '
    Me.btnVoid.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnVoid.Location = New System.Drawing.Point(12, 41)
    Me.btnVoid.Name = "btnVoid"
    Me.btnVoid.Size = New System.Drawing.Size(211, 24)
    Me.btnVoid.TabIndex = 12
    Me.btnVoid.Text = "144"
    Me.btnVoid.UseVisualStyleBackColor = True
    '
    'lstMisch
    '
    Me.lstMisch.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstMisch.FormattingEnabled = True
    Me.lstMisch.Location = New System.Drawing.Point(239, 128)
    Me.lstMisch.Name = "lstMisch"
    Me.lstMisch.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended
    Me.lstMisch.Size = New System.Drawing.Size(207, 368)
    Me.lstMisch.TabIndex = 11
    '
    'lstMessg
    '
    Me.lstMessg.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstMessg.FormattingEnabled = True
    Me.lstMessg.Location = New System.Drawing.Point(12, 128)
    Me.lstMessg.Name = "lstMessg"
    Me.lstMessg.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended
    Me.lstMessg.Size = New System.Drawing.Size(211, 368)
    Me.lstMessg.TabIndex = 10
    '
    'txtMessgKE
    '
    Me.txtMessgKE.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
    Me.txtMessgKE.Location = New System.Drawing.Point(203, 278)
    Me.txtMessgKE.Name = "txtMessgKE"
    Me.txtMessgKE.Size = New System.Drawing.Size(34, 20)
    Me.txtMessgKE.TabIndex = 17
    '
    'lblMessgKE
    '
    Me.lblMessgKE.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
    Me.lblMessgKE.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMessgKE.Location = New System.Drawing.Point(77, 280)
    Me.lblMessgKE.Name = "lblMessgKE"
    Me.lblMessgKE.Size = New System.Drawing.Size(130, 18)
    Me.lblMessgKE.TabIndex = 16
    Me.lblMessgKE.Text = "MESSGKE"
    Me.lblMessgKE.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'dbgMessgeräte
    '
    Me.dbgMessgeräte.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.dbgMessgeräte.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgMessgeräte.Location = New System.Drawing.Point(47, 62)
    Me.dbgMessgeräte.Name = "dbgMessgeräte"
    Me.dbgMessgeräte.Size = New System.Drawing.Size(266, 215)
    Me.dbgMessgeräte.TabIndex = 15
    '
    'dbgMeasure00x
    '
    Me.dbgMeasure00x.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.dbgMeasure00x.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgMeasure00x.Location = New System.Drawing.Point(47, 304)
    Me.dbgMeasure00x.Name = "dbgMeasure00x"
    Me.dbgMeasure00x.Size = New System.Drawing.Size(266, 215)
    Me.dbgMeasure00x.TabIndex = 14
    '
    'lblShowMeasDevices
    '
    Me.lblShowMeasDevices.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblShowMeasDevices.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblShowMeasDevices.Location = New System.Drawing.Point(44, 41)
    Me.lblShowMeasDevices.Name = "lblShowMeasDevices"
    Me.lblShowMeasDevices.Size = New System.Drawing.Size(269, 18)
    Me.lblShowMeasDevices.TabIndex = 13
    Me.lblShowMeasDevices.Text = "403 142"
    Me.lblShowMeasDevices.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'frmZeigMessgMisch
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(823, 561)
    Me.Controls.Add(Me.splMessgMisch)
    Me.Name = "frmZeigMessgMisch"
    Me.Text = "frmZeigMessgMisch"
    Me.splMessgMisch.Panel1.ResumeLayout(False)
    Me.splMessgMisch.Panel2.ResumeLayout(False)
    Me.splMessgMisch.Panel2.PerformLayout()
    CType(Me.splMessgMisch, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splMessgMisch.ResumeLayout(False)
    CType(Me.dbgMessgeräte, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.dbgMeasure00x, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents splMessgMisch As System.Windows.Forms.SplitContainer
  Friend WithEvents btnMischSelect As System.Windows.Forms.Button
  Friend WithEvents btnMessgSelect As System.Windows.Forms.Button
  Friend WithEvents lblDBCopy As System.Windows.Forms.Label
  Friend WithEvents btnRezepte As System.Windows.Forms.Button
  Friend WithEvents btnRwerte As System.Windows.Forms.Button
  Friend WithEvents btnMisch As System.Windows.Forms.Button
  Friend WithEvents btnMessg As System.Windows.Forms.Button
  Friend WithEvents btnVoid As System.Windows.Forms.Button
  Friend WithEvents lstMisch As System.Windows.Forms.ListBox
  Friend WithEvents lstMessg As System.Windows.Forms.ListBox
  Friend WithEvents txtMessgKE As System.Windows.Forms.TextBox
  Friend WithEvents lblMessgKE As System.Windows.Forms.Label
  Friend WithEvents dbgMessgeräte As System.Windows.Forms.DataGridView
  Friend WithEvents dbgMeasure00x As System.Windows.Forms.DataGridView
  Friend WithEvents lblShowMeasDevices As System.Windows.Forms.Label
End Class
