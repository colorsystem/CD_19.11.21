<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmNassTrocken
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
    Me.splNassTrocken = New System.Windows.Forms.SplitContainer()
    Me.btnKOP = New System.Windows.Forms.Button()
    Me.chkDLDADBN = New System.Windows.Forms.CheckBox()
    Me.chkRwerte = New System.Windows.Forms.CheckBox()
    Me.btnStore = New System.Windows.Forms.Button()
    Me.cboGRP = New System.Windows.Forms.ComboBox()
    Me.lblGRP = New System.Windows.Forms.Label()
    Me.cboMessNass = New System.Windows.Forms.ComboBox()
    Me.lblMessNass = New System.Windows.Forms.Label()
    Me.lblMessTrocken = New System.Windows.Forms.Label()
    Me.cboMessTrocken = New System.Windows.Forms.ComboBox()
    Me.btnDelete = New System.Windows.Forms.Button()
    Me.btnNachstellungNass = New System.Windows.Forms.Button()
    Me.btnVorlageNass = New System.Windows.Forms.Button()
    Me.btnNachstellungTrocken = New System.Windows.Forms.Button()
    Me.btnVorlageTrocken = New System.Windows.Forms.Button()
    Me.dbgNassTrocken = New System.Windows.Forms.DataGridView()
    CType(Me.splNassTrocken, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splNassTrocken.Panel1.SuspendLayout()
    Me.splNassTrocken.Panel2.SuspendLayout()
    Me.splNassTrocken.SuspendLayout()
    CType(Me.dbgNassTrocken, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'splNassTrocken
    '
    Me.splNassTrocken.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splNassTrocken.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.splNassTrocken.Location = New System.Drawing.Point(0, 0)
    Me.splNassTrocken.Name = "splNassTrocken"
    Me.splNassTrocken.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splNassTrocken.Panel1
    '
    Me.splNassTrocken.Panel1.Controls.Add(Me.btnKOP)
    Me.splNassTrocken.Panel1.Controls.Add(Me.chkDLDADBN)
    Me.splNassTrocken.Panel1.Controls.Add(Me.chkRwerte)
    Me.splNassTrocken.Panel1.Controls.Add(Me.btnStore)
    Me.splNassTrocken.Panel1.Controls.Add(Me.cboGRP)
    Me.splNassTrocken.Panel1.Controls.Add(Me.lblGRP)
    Me.splNassTrocken.Panel1.Controls.Add(Me.cboMessNass)
    Me.splNassTrocken.Panel1.Controls.Add(Me.lblMessNass)
    Me.splNassTrocken.Panel1.Controls.Add(Me.lblMessTrocken)
    Me.splNassTrocken.Panel1.Controls.Add(Me.cboMessTrocken)
    Me.splNassTrocken.Panel1.Controls.Add(Me.btnDelete)
    Me.splNassTrocken.Panel1.Controls.Add(Me.btnNachstellungNass)
    Me.splNassTrocken.Panel1.Controls.Add(Me.btnVorlageNass)
    Me.splNassTrocken.Panel1.Controls.Add(Me.btnNachstellungTrocken)
    Me.splNassTrocken.Panel1.Controls.Add(Me.btnVorlageTrocken)
    '
    'splNassTrocken.Panel2
    '
    Me.splNassTrocken.Panel2.Controls.Add(Me.dbgNassTrocken)
    Me.splNassTrocken.Size = New System.Drawing.Size(971, 512)
    Me.splNassTrocken.SplitterDistance = 146
    Me.splNassTrocken.TabIndex = 0
    '
    'btnKOP
    '
    Me.btnKOP.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnKOP.Enabled = False
    Me.btnKOP.Location = New System.Drawing.Point(705, 62)
    Me.btnKOP.Name = "btnKOP"
    Me.btnKOP.Size = New System.Drawing.Size(186, 22)
    Me.btnKOP.TabIndex = 33
    Me.btnKOP.Text = "389"
    Me.btnKOP.UseVisualStyleBackColor = True
    '
    'chkDLDADBN
    '
    Me.chkDLDADBN.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDLDADBN.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkDLDADBN.Location = New System.Drawing.Point(427, 38)
    Me.chkDLDADBN.Name = "chkDLDADBN"
    Me.chkDLDADBN.Size = New System.Drawing.Size(186, 20)
    Me.chkDLDADBN.TabIndex = 32
    Me.chkDLDADBN.UseVisualStyleBackColor = False
    '
    'chkRwerte
    '
    Me.chkRwerte.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkRwerte.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkRwerte.Location = New System.Drawing.Point(427, 12)
    Me.chkRwerte.Name = "chkRwerte"
    Me.chkRwerte.Size = New System.Drawing.Size(186, 20)
    Me.chkRwerte.TabIndex = 31
    Me.chkRwerte.Text = "3110"
    Me.chkRwerte.UseVisualStyleBackColor = False
    '
    'btnStore
    '
    Me.btnStore.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnStore.Enabled = False
    Me.btnStore.Location = New System.Drawing.Point(427, 93)
    Me.btnStore.Name = "btnStore"
    Me.btnStore.Size = New System.Drawing.Size(186, 22)
    Me.btnStore.TabIndex = 30
    Me.btnStore.Text = "3777"
    Me.btnStore.UseVisualStyleBackColor = True
    '
    'cboGRP
    '
    Me.cboGRP.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboGRP.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboGRP.FormattingEnabled = True
    Me.cboGRP.Location = New System.Drawing.Point(427, 66)
    Me.cboGRP.Name = "cboGRP"
    Me.cboGRP.Size = New System.Drawing.Size(186, 21)
    Me.cboGRP.TabIndex = 29
    '
    'lblGRP
    '
    Me.lblGRP.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblGRP.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRP.Location = New System.Drawing.Point(296, 67)
    Me.lblGRP.Name = "lblGRP"
    Me.lblGRP.Size = New System.Drawing.Size(131, 20)
    Me.lblGRP.TabIndex = 28
    Me.lblGRP.Text = "386"
    Me.lblGRP.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'cboMessNass
    '
    Me.cboMessNass.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMessNass.Enabled = False
    Me.cboMessNass.FormattingEnabled = True
    Me.cboMessNass.Location = New System.Drawing.Point(702, 26)
    Me.cboMessNass.Name = "cboMessNass"
    Me.cboMessNass.Size = New System.Drawing.Size(189, 21)
    Me.cboMessNass.TabIndex = 8
    '
    'lblMessNass
    '
    Me.lblMessNass.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMessNass.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMessNass.Location = New System.Drawing.Point(702, 4)
    Me.lblMessNass.Name = "lblMessNass"
    Me.lblMessNass.Size = New System.Drawing.Size(189, 19)
    Me.lblMessNass.TabIndex = 7
    Me.lblMessNass.Text = "3771"
    Me.lblMessNass.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblMessTrocken
    '
    Me.lblMessTrocken.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMessTrocken.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMessTrocken.Location = New System.Drawing.Point(103, 4)
    Me.lblMessTrocken.Name = "lblMessTrocken"
    Me.lblMessTrocken.Size = New System.Drawing.Size(189, 19)
    Me.lblMessTrocken.TabIndex = 6
    Me.lblMessTrocken.Text = "3770"
    Me.lblMessTrocken.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'cboMessTrocken
    '
    Me.cboMessTrocken.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMessTrocken.FormattingEnabled = True
    Me.cboMessTrocken.Location = New System.Drawing.Point(103, 26)
    Me.cboMessTrocken.Name = "cboMessTrocken"
    Me.cboMessTrocken.Size = New System.Drawing.Size(189, 21)
    Me.cboMessTrocken.TabIndex = 5
    '
    'btnDelete
    '
    Me.btnDelete.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnDelete.Enabled = False
    Me.btnDelete.Location = New System.Drawing.Point(427, 121)
    Me.btnDelete.Name = "btnDelete"
    Me.btnDelete.Size = New System.Drawing.Size(186, 22)
    Me.btnDelete.TabIndex = 4
    Me.btnDelete.Text = "3776"
    Me.btnDelete.UseVisualStyleBackColor = True
    '
    'btnNachstellungNass
    '
    Me.btnNachstellungNass.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnNachstellungNass.Enabled = False
    Me.btnNachstellungNass.Location = New System.Drawing.Point(705, 118)
    Me.btnNachstellungNass.Name = "btnNachstellungNass"
    Me.btnNachstellungNass.Size = New System.Drawing.Size(186, 22)
    Me.btnNachstellungNass.TabIndex = 3
    Me.btnNachstellungNass.Text = "3775"
    Me.btnNachstellungNass.UseVisualStyleBackColor = True
    '
    'btnVorlageNass
    '
    Me.btnVorlageNass.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnVorlageNass.Enabled = False
    Me.btnVorlageNass.Location = New System.Drawing.Point(705, 90)
    Me.btnVorlageNass.Name = "btnVorlageNass"
    Me.btnVorlageNass.Size = New System.Drawing.Size(186, 22)
    Me.btnVorlageNass.TabIndex = 2
    Me.btnVorlageNass.Text = "3774"
    Me.btnVorlageNass.UseVisualStyleBackColor = True
    '
    'btnNachstellungTrocken
    '
    Me.btnNachstellungTrocken.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnNachstellungTrocken.Enabled = False
    Me.btnNachstellungTrocken.Location = New System.Drawing.Point(103, 118)
    Me.btnNachstellungTrocken.Name = "btnNachstellungTrocken"
    Me.btnNachstellungTrocken.Size = New System.Drawing.Size(189, 22)
    Me.btnNachstellungTrocken.TabIndex = 1
    Me.btnNachstellungTrocken.Text = "3773"
    Me.btnNachstellungTrocken.UseVisualStyleBackColor = True
    '
    'btnVorlageTrocken
    '
    Me.btnVorlageTrocken.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnVorlageTrocken.Location = New System.Drawing.Point(103, 90)
    Me.btnVorlageTrocken.Name = "btnVorlageTrocken"
    Me.btnVorlageTrocken.Size = New System.Drawing.Size(189, 22)
    Me.btnVorlageTrocken.TabIndex = 0
    Me.btnVorlageTrocken.Text = "3772"
    Me.btnVorlageTrocken.UseVisualStyleBackColor = True
    '
    'dbgNassTrocken
    '
    Me.dbgNassTrocken.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgNassTrocken.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgNassTrocken.Location = New System.Drawing.Point(0, 0)
    Me.dbgNassTrocken.Name = "dbgNassTrocken"
    Me.dbgNassTrocken.Size = New System.Drawing.Size(971, 362)
    Me.dbgNassTrocken.TabIndex = 0
    '
    'frmNassTrocken
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(971, 512)
    Me.ControlBox = False
    Me.Controls.Add(Me.splNassTrocken)
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmNassTrocken"
    Me.ShowIcon = False
    Me.ShowInTaskbar = False
    Me.Text = "frmMankiewNassTrocken"
    Me.splNassTrocken.Panel1.ResumeLayout(False)
    Me.splNassTrocken.Panel2.ResumeLayout(False)
    CType(Me.splNassTrocken, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splNassTrocken.ResumeLayout(False)
    CType(Me.dbgNassTrocken, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents splNassTrocken As System.Windows.Forms.SplitContainer
  Friend WithEvents btnDelete As System.Windows.Forms.Button
  Friend WithEvents btnNachstellungNass As System.Windows.Forms.Button
  Friend WithEvents btnVorlageNass As System.Windows.Forms.Button
  Friend WithEvents btnNachstellungTrocken As System.Windows.Forms.Button
  Friend WithEvents btnVorlageTrocken As System.Windows.Forms.Button
  Friend WithEvents cboMessNass As System.Windows.Forms.ComboBox
  Friend WithEvents lblMessNass As System.Windows.Forms.Label
  Friend WithEvents lblMessTrocken As System.Windows.Forms.Label
  Friend WithEvents cboMessTrocken As System.Windows.Forms.ComboBox
  Friend WithEvents dbgNassTrocken As System.Windows.Forms.DataGridView
  Friend WithEvents cboGRP As System.Windows.Forms.ComboBox
  Friend WithEvents lblGRP As System.Windows.Forms.Label
  Friend WithEvents btnStore As System.Windows.Forms.Button
  Friend WithEvents chkRwerte As System.Windows.Forms.CheckBox
  Friend WithEvents chkDLDADBN As System.Windows.Forms.CheckBox
  Friend WithEvents btnKOP As System.Windows.Forms.Button
End Class
