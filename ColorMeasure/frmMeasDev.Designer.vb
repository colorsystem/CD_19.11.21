<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
 Partial Friend Class frmMeasDev
  Inherits System.Windows.Forms.Form

  'Form overrides dispose to clean up the component list.
  <System.Diagnostics.DebuggerNonUserCode()> _
  Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
    If disposing AndAlso components IsNot Nothing Then
      components.Dispose()
    End If
    MyBase.Dispose(disposing)
  End Sub

  'Required by the Windows Form Designer
  Private components As System.ComponentModel.IContainer

  'NOTE: The following procedure is required by the Windows Form Designer
  'It can be modified using the Windows Form Designer.  
  'Do not modify it using the code editor.
  <System.Diagnostics.DebuggerStepThrough()> _
   Private Sub InitializeComponent()
    Me.components = New System.ComponentModel.Container()
    Me.SplitContain = New System.Windows.Forms.SplitContainer()
    Me.txtProg = New System.Windows.Forms.TextBox()
    Me.lblANZ = New System.Windows.Forms.Label()
    Me.cboBAUD = New System.Windows.Forms.ComboBox()
    Me.cboCOM = New System.Windows.Forms.ComboBox()
    Me.btnSTP = New System.Windows.Forms.Button()
    Me.btnEND = New System.Windows.Forms.Button()
    Me.btnMES_5 = New System.Windows.Forms.Button()
    Me.btnMES_4 = New System.Windows.Forms.Button()
    Me.btnMES_3 = New System.Windows.Forms.Button()
    Me.btnMES_2 = New System.Windows.Forms.Button()
    Me.btnMES_1 = New System.Windows.Forms.Button()
    Me.btnWIN = New System.Windows.Forms.Button()
    Me.btnABR = New System.Windows.Forms.Button()
    Me.btnSTR = New System.Windows.Forms.Button()
    Me.btnMIT = New System.Windows.Forms.Button()
    Me.btnLOE = New System.Windows.Forms.Button()
    Me.btnNXT = New System.Windows.Forms.Button()
    Me.txtKEN = New System.Windows.Forms.TextBox()
    Me.txtWID = New System.Windows.Forms.TextBox()
    Me.cboSPE_1 = New System.Windows.Forms.ComboBox()
    Me.cboSPE_0 = New System.Windows.Forms.ComboBox()
    Me.lblWID = New System.Windows.Forms.Label()
    Me.lblSPE_1 = New System.Windows.Forms.Label()
    Me.lblSPE_0 = New System.Windows.Forms.Label()
    Me.btnMES_0 = New System.Windows.Forms.Button()
    Me.lblCOM = New System.Windows.Forms.Label()
    Me.lblBAUD = New System.Windows.Forms.Label()
    Me.lblMES = New System.Windows.Forms.Label()
    Me.flgKAL = New System.Windows.Forms.DataGridView()
    Me.ContextMenuStripRwerte = New System.Windows.Forms.ContextMenuStrip(Me.components)
    Me.ToolStripMenuItemKopieren = New System.Windows.Forms.ToolStripMenuItem()
    Me.ToolStripMenuItemEinfügen = New System.Windows.Forms.ToolStripMenuItem()
    CType(Me.SplitContain, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContain.Panel1.SuspendLayout()
    Me.SplitContain.Panel2.SuspendLayout()
    Me.SplitContain.SuspendLayout()
    CType(Me.flgKAL, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.ContextMenuStripRwerte.SuspendLayout()
    Me.SuspendLayout()
    '
    'SplitContain
    '
    Me.SplitContain.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContain.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitContain.IsSplitterFixed = True
    Me.SplitContain.Location = New System.Drawing.Point(0, 0)
    Me.SplitContain.MinimumSize = New System.Drawing.Size(0, 467)
    Me.SplitContain.Name = "SplitContain"
    Me.SplitContain.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'SplitContain.Panel1
    '
    Me.SplitContain.Panel1.Controls.Add(Me.txtProg)
    Me.SplitContain.Panel1.Controls.Add(Me.lblANZ)
    Me.SplitContain.Panel1.Controls.Add(Me.cboBAUD)
    Me.SplitContain.Panel1.Controls.Add(Me.cboCOM)
    Me.SplitContain.Panel1.Controls.Add(Me.btnSTP)
    Me.SplitContain.Panel1.Controls.Add(Me.btnEND)
    Me.SplitContain.Panel1.Controls.Add(Me.btnMES_5)
    Me.SplitContain.Panel1.Controls.Add(Me.btnMES_4)
    Me.SplitContain.Panel1.Controls.Add(Me.btnMES_3)
    Me.SplitContain.Panel1.Controls.Add(Me.btnMES_2)
    Me.SplitContain.Panel1.Controls.Add(Me.btnMES_1)
    Me.SplitContain.Panel1.Controls.Add(Me.btnWIN)
    Me.SplitContain.Panel1.Controls.Add(Me.btnABR)
    Me.SplitContain.Panel1.Controls.Add(Me.btnSTR)
    Me.SplitContain.Panel1.Controls.Add(Me.btnMIT)
    Me.SplitContain.Panel1.Controls.Add(Me.btnLOE)
    Me.SplitContain.Panel1.Controls.Add(Me.btnNXT)
    Me.SplitContain.Panel1.Controls.Add(Me.txtKEN)
    Me.SplitContain.Panel1.Controls.Add(Me.txtWID)
    Me.SplitContain.Panel1.Controls.Add(Me.cboSPE_1)
    Me.SplitContain.Panel1.Controls.Add(Me.cboSPE_0)
    Me.SplitContain.Panel1.Controls.Add(Me.lblWID)
    Me.SplitContain.Panel1.Controls.Add(Me.lblSPE_1)
    Me.SplitContain.Panel1.Controls.Add(Me.lblSPE_0)
    Me.SplitContain.Panel1.Controls.Add(Me.btnMES_0)
    Me.SplitContain.Panel1.Controls.Add(Me.lblCOM)
    Me.SplitContain.Panel1.Controls.Add(Me.lblBAUD)
    Me.SplitContain.Panel1.Controls.Add(Me.lblMES)
    '
    'SplitContain.Panel2
    '
    Me.SplitContain.Panel2.Controls.Add(Me.flgKAL)
    Me.SplitContain.Size = New System.Drawing.Size(448, 582)
    Me.SplitContain.SplitterDistance = 307
    Me.SplitContain.TabIndex = 28
    Me.SplitContain.Text = "SplitContainer1"
    '
    'txtProg
    '
    Me.txtProg.Location = New System.Drawing.Point(198, 103)
    Me.txtProg.Name = "txtProg"
    Me.txtProg.Size = New System.Drawing.Size(216, 20)
    Me.txtProg.TabIndex = 55
    '
    'lblANZ
    '
    Me.lblANZ.BackColor = System.Drawing.SystemColors.ButtonHighlight
    Me.lblANZ.Font = New System.Drawing.Font("Arial", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblANZ.Location = New System.Drawing.Point(198, 193)
    Me.lblANZ.Name = "lblANZ"
    Me.lblANZ.Size = New System.Drawing.Size(217, 20)
    Me.lblANZ.TabIndex = 53
    Me.lblANZ.Text = "492"
    Me.lblANZ.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblANZ.Visible = False
    '
    'cboBAUD
    '
    Me.cboBAUD.FormattingEnabled = True
    Me.cboBAUD.Location = New System.Drawing.Point(304, 173)
    Me.cboBAUD.Name = "cboBAUD"
    Me.cboBAUD.Size = New System.Drawing.Size(110, 21)
    Me.cboBAUD.TabIndex = 52
    '
    'cboCOM
    '
    Me.cboCOM.FormattingEnabled = True
    Me.cboCOM.Location = New System.Drawing.Point(304, 151)
    Me.cboCOM.Name = "cboCOM"
    Me.cboCOM.Size = New System.Drawing.Size(110, 21)
    Me.cboCOM.TabIndex = 50
    '
    'btnSTP
    '
    Me.btnSTP.BackColor = System.Drawing.Color.LightCyan
    Me.btnSTP.Location = New System.Drawing.Point(208, 246)
    Me.btnSTP.Name = "btnSTP"
    Me.btnSTP.Size = New System.Drawing.Size(164, 19)
    Me.btnSTP.TabIndex = 48
    Me.btnSTP.Text = "490"
    Me.btnSTP.UseVisualStyleBackColor = False
    '
    'btnEND
    '
    Me.btnEND.BackColor = System.Drawing.Color.LightCyan
    Me.btnEND.Location = New System.Drawing.Point(32, 271)
    Me.btnEND.Name = "btnEND"
    Me.btnEND.Size = New System.Drawing.Size(166, 20)
    Me.btnEND.TabIndex = 47
    Me.btnEND.Text = "1999"
    Me.btnEND.UseVisualStyleBackColor = False
    '
    'btnMES_5
    '
    Me.btnMES_5.BackColor = System.Drawing.Color.LightCyan
    Me.btnMES_5.Location = New System.Drawing.Point(32, 229)
    Me.btnMES_5.Name = "btnMES_5"
    Me.btnMES_5.Size = New System.Drawing.Size(166, 20)
    Me.btnMES_5.TabIndex = 46
    Me.btnMES_5.Text = "999"
    Me.btnMES_5.UseVisualStyleBackColor = False
    '
    'btnMES_4
    '
    Me.btnMES_4.BackColor = System.Drawing.Color.LightCyan
    Me.btnMES_4.Location = New System.Drawing.Point(32, 213)
    Me.btnMES_4.Name = "btnMES_4"
    Me.btnMES_4.Size = New System.Drawing.Size(166, 20)
    Me.btnMES_4.TabIndex = 45
    Me.btnMES_4.Text = "999"
    Me.btnMES_4.UseVisualStyleBackColor = False
    '
    'btnMES_3
    '
    Me.btnMES_3.BackColor = System.Drawing.Color.LightCyan
    Me.btnMES_3.Location = New System.Drawing.Point(33, 246)
    Me.btnMES_3.Name = "btnMES_3"
    Me.btnMES_3.Size = New System.Drawing.Size(166, 20)
    Me.btnMES_3.TabIndex = 44
    Me.btnMES_3.Text = "999"
    Me.btnMES_3.UseVisualStyleBackColor = False
    '
    'btnMES_2
    '
    Me.btnMES_2.BackColor = System.Drawing.Color.LightCyan
    Me.btnMES_2.Location = New System.Drawing.Point(33, 229)
    Me.btnMES_2.Name = "btnMES_2"
    Me.btnMES_2.Size = New System.Drawing.Size(166, 20)
    Me.btnMES_2.TabIndex = 43
    Me.btnMES_2.Text = "999"
    Me.btnMES_2.UseVisualStyleBackColor = False
    '
    'btnMES_1
    '
    Me.btnMES_1.BackColor = System.Drawing.Color.LightCyan
    Me.btnMES_1.Location = New System.Drawing.Point(33, 213)
    Me.btnMES_1.Name = "btnMES_1"
    Me.btnMES_1.Size = New System.Drawing.Size(166, 20)
    Me.btnMES_1.TabIndex = 42
    Me.btnMES_1.Text = "999"
    Me.btnMES_1.UseVisualStyleBackColor = False
    '
    'btnWIN
    '
    Me.btnWIN.BackColor = System.Drawing.Color.LightCyan
    Me.btnWIN.Location = New System.Drawing.Point(33, 193)
    Me.btnWIN.Name = "btnWIN"
    Me.btnWIN.Size = New System.Drawing.Size(166, 20)
    Me.btnWIN.TabIndex = 40
    Me.btnWIN.Text = "485"
    Me.btnWIN.UseVisualStyleBackColor = False
    '
    'btnABR
    '
    Me.btnABR.BackColor = System.Drawing.Color.LightCyan
    Me.btnABR.Location = New System.Drawing.Point(33, 171)
    Me.btnABR.Name = "btnABR"
    Me.btnABR.Size = New System.Drawing.Size(166, 20)
    Me.btnABR.TabIndex = 39
    Me.btnABR.Text = "489"
    Me.btnABR.UseVisualStyleBackColor = False
    '
    'btnSTR
    '
    Me.btnSTR.BackColor = System.Drawing.Color.LightCyan
    Me.btnSTR.Location = New System.Drawing.Point(33, 154)
    Me.btnSTR.Name = "btnSTR"
    Me.btnSTR.Size = New System.Drawing.Size(166, 20)
    Me.btnSTR.TabIndex = 38
    Me.btnSTR.Text = "484"
    Me.btnSTR.UseVisualStyleBackColor = False
    '
    'btnMIT
    '
    Me.btnMIT.BackColor = System.Drawing.Color.LightCyan
    Me.btnMIT.Location = New System.Drawing.Point(33, 137)
    Me.btnMIT.Name = "btnMIT"
    Me.btnMIT.Size = New System.Drawing.Size(166, 20)
    Me.btnMIT.TabIndex = 37
    Me.btnMIT.Text = "486"
    Me.btnMIT.UseVisualStyleBackColor = False
    '
    'btnLOE
    '
    Me.btnLOE.BackColor = System.Drawing.Color.LightCyan
    Me.btnLOE.Location = New System.Drawing.Point(32, 120)
    Me.btnLOE.Name = "btnLOE"
    Me.btnLOE.Size = New System.Drawing.Size(166, 20)
    Me.btnLOE.TabIndex = 36
    Me.btnLOE.Text = "487"
    Me.btnLOE.UseVisualStyleBackColor = False
    '
    'btnNXT
    '
    Me.btnNXT.BackColor = System.Drawing.Color.LightCyan
    Me.btnNXT.Location = New System.Drawing.Point(32, 104)
    Me.btnNXT.Name = "btnNXT"
    Me.btnNXT.Size = New System.Drawing.Size(166, 20)
    Me.btnNXT.TabIndex = 35
    Me.btnNXT.Text = "488"
    Me.btnNXT.UseVisualStyleBackColor = False
    '
    'txtKEN
    '
    Me.txtKEN.Location = New System.Drawing.Point(378, 81)
    Me.txtKEN.Name = "txtKEN"
    Me.txtKEN.Size = New System.Drawing.Size(36, 20)
    Me.txtKEN.TabIndex = 34
    '
    'txtWID
    '
    Me.txtWID.Location = New System.Drawing.Point(196, 81)
    Me.txtWID.Name = "txtWID"
    Me.txtWID.Size = New System.Drawing.Size(176, 20)
    Me.txtWID.TabIndex = 33
    '
    'cboSPE_1
    '
    Me.cboSPE_1.FormattingEnabled = True
    Me.cboSPE_1.Location = New System.Drawing.Point(196, 58)
    Me.cboSPE_1.Name = "cboSPE_1"
    Me.cboSPE_1.Size = New System.Drawing.Size(107, 21)
    Me.cboSPE_1.TabIndex = 32
    '
    'cboSPE_0
    '
    Me.cboSPE_0.FormattingEnabled = True
    Me.cboSPE_0.Location = New System.Drawing.Point(196, 35)
    Me.cboSPE_0.Name = "cboSPE_0"
    Me.cboSPE_0.Size = New System.Drawing.Size(107, 21)
    Me.cboSPE_0.TabIndex = 31
    '
    'lblWID
    '
    Me.lblWID.BackColor = System.Drawing.SystemColors.Control
    Me.lblWID.Font = New System.Drawing.Font("Arial", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblWID.Location = New System.Drawing.Point(30, 81)
    Me.lblWID.Name = "lblWID"
    Me.lblWID.Size = New System.Drawing.Size(166, 20)
    Me.lblWID.TabIndex = 30
    Me.lblWID.Text = "3902"
    Me.lblWID.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblSPE_1
    '
    Me.lblSPE_1.BackColor = System.Drawing.SystemColors.Control
    Me.lblSPE_1.Font = New System.Drawing.Font("Arial", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblSPE_1.Location = New System.Drawing.Point(30, 58)
    Me.lblSPE_1.Name = "lblSPE_1"
    Me.lblSPE_1.Size = New System.Drawing.Size(166, 20)
    Me.lblSPE_1.TabIndex = 29
    Me.lblSPE_1.Text = "3901"
    Me.lblSPE_1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblSPE_0
    '
    Me.lblSPE_0.BackColor = System.Drawing.SystemColors.Control
    Me.lblSPE_0.Font = New System.Drawing.Font("Arial", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblSPE_0.Location = New System.Drawing.Point(30, 35)
    Me.lblSPE_0.Name = "lblSPE_0"
    Me.lblSPE_0.Size = New System.Drawing.Size(166, 20)
    Me.lblSPE_0.TabIndex = 28
    Me.lblSPE_0.Text = "3900"
    Me.lblSPE_0.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'btnMES_0
    '
    Me.btnMES_0.BackColor = System.Drawing.Color.LightCyan
    Me.btnMES_0.Location = New System.Drawing.Point(33, 171)
    Me.btnMES_0.Name = "btnMES_0"
    Me.btnMES_0.Size = New System.Drawing.Size(166, 20)
    Me.btnMES_0.TabIndex = 41
    Me.btnMES_0.Text = "999"
    Me.btnMES_0.UseVisualStyleBackColor = False
    '
    'lblCOM
    '
    Me.lblCOM.BackColor = System.Drawing.SystemColors.Control
    Me.lblCOM.Font = New System.Drawing.Font("Arial", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblCOM.Location = New System.Drawing.Point(33, 153)
    Me.lblCOM.Name = "lblCOM"
    Me.lblCOM.Size = New System.Drawing.Size(270, 20)
    Me.lblCOM.TabIndex = 49
    Me.lblCOM.Text = "286"
    Me.lblCOM.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblBAUD
    '
    Me.lblBAUD.BackColor = System.Drawing.SystemColors.Control
    Me.lblBAUD.Font = New System.Drawing.Font("Arial", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblBAUD.Location = New System.Drawing.Point(33, 173)
    Me.lblBAUD.Name = "lblBAUD"
    Me.lblBAUD.Size = New System.Drawing.Size(270, 20)
    Me.lblBAUD.TabIndex = 51
    Me.lblBAUD.Text = "287"
    Me.lblBAUD.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMES
    '
    Me.lblMES.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(192, Byte), Integer))
    Me.lblMES.Font = New System.Drawing.Font("Arial", 18.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblMES.Location = New System.Drawing.Point(74, 9)
    Me.lblMES.Name = "lblMES"
    Me.lblMES.Size = New System.Drawing.Size(340, 69)
    Me.lblMES.TabIndex = 54
    Me.lblMES.Text = "2001"
    Me.lblMES.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'flgKAL
    '
    Me.flgKAL.AllowUserToAddRows = False
    Me.flgKAL.AllowUserToDeleteRows = False
    Me.flgKAL.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.flgKAL.ContextMenuStrip = Me.ContextMenuStripRwerte
    Me.flgKAL.Dock = System.Windows.Forms.DockStyle.Fill
    Me.flgKAL.Location = New System.Drawing.Point(0, 0)
    Me.flgKAL.Name = "flgKAL"
    Me.flgKAL.RowTemplate.Height = 24
    Me.flgKAL.Size = New System.Drawing.Size(448, 271)
    Me.flgKAL.TabIndex = 0
    '
    'ContextMenuStripRwerte
    '
    Me.ContextMenuStripRwerte.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItemKopieren, Me.ToolStripMenuItemEinfügen})
    Me.ContextMenuStripRwerte.Name = "ContextMenuStripRwerte"
    Me.ContextMenuStripRwerte.Size = New System.Drawing.Size(99, 48)
    '
    'ToolStripMenuItemKopieren
    '
    Me.ToolStripMenuItemKopieren.Name = "ToolStripMenuItemKopieren"
    Me.ToolStripMenuItemKopieren.Size = New System.Drawing.Size(98, 22)
    Me.ToolStripMenuItemKopieren.Text = "3922"
    '
    'ToolStripMenuItemEinfügen
    '
    Me.ToolStripMenuItemEinfügen.Name = "ToolStripMenuItemEinfügen"
    Me.ToolStripMenuItemEinfügen.Size = New System.Drawing.Size(98, 22)
    Me.ToolStripMenuItemEinfügen.Text = "3923"
    '
    'frmMeasDev
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.BackColor = System.Drawing.Color.White
    Me.ClientSize = New System.Drawing.Size(448, 582)
    Me.ControlBox = False
    Me.Controls.Add(Me.SplitContain)
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmMeasDev"
    Me.Text = "frmMeasDev"
    Me.SplitContain.Panel1.ResumeLayout(False)
    Me.SplitContain.Panel1.PerformLayout()
    Me.SplitContain.Panel2.ResumeLayout(False)
    CType(Me.SplitContain, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContain.ResumeLayout(False)
    CType(Me.flgKAL, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ContextMenuStripRwerte.ResumeLayout(False)
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents SplitContain As System.Windows.Forms.SplitContainer
  Friend WithEvents lblANZ As System.Windows.Forms.Label
  Friend WithEvents cboBAUD As System.Windows.Forms.ComboBox
  Friend WithEvents cboCOM As System.Windows.Forms.ComboBox
  Friend WithEvents btnSTP As System.Windows.Forms.Button
  Friend WithEvents btnEND As System.Windows.Forms.Button
  Friend WithEvents btnMES_5 As System.Windows.Forms.Button
  Friend WithEvents btnMES_4 As System.Windows.Forms.Button
  Friend WithEvents btnMES_3 As System.Windows.Forms.Button
  Friend WithEvents btnMES_2 As System.Windows.Forms.Button
  Friend WithEvents btnMES_1 As System.Windows.Forms.Button
  Friend WithEvents btnWIN As System.Windows.Forms.Button
  Friend WithEvents btnABR As System.Windows.Forms.Button
  Friend WithEvents btnSTR As System.Windows.Forms.Button
  Friend WithEvents btnMIT As System.Windows.Forms.Button
  Friend WithEvents btnLOE As System.Windows.Forms.Button
  Friend WithEvents btnNXT As System.Windows.Forms.Button
  Friend WithEvents txtKEN As System.Windows.Forms.TextBox
  Friend WithEvents txtWID As System.Windows.Forms.TextBox
  Friend WithEvents cboSPE_1 As System.Windows.Forms.ComboBox
  Friend WithEvents cboSPE_0 As System.Windows.Forms.ComboBox
  Friend WithEvents lblWID As System.Windows.Forms.Label
  Friend WithEvents lblSPE_1 As System.Windows.Forms.Label
  Friend WithEvents lblSPE_0 As System.Windows.Forms.Label
  Friend WithEvents btnMES_0 As System.Windows.Forms.Button
  Friend WithEvents lblCOM As System.Windows.Forms.Label
  Friend WithEvents lblBAUD As System.Windows.Forms.Label
  Friend WithEvents lblMES As System.Windows.Forms.Label
  Friend WithEvents flgKAL As System.Windows.Forms.DataGridView
  Friend WithEvents ContextMenuStripRwerte As System.Windows.Forms.ContextMenuStrip
  Friend WithEvents ToolStripMenuItemKopieren As System.Windows.Forms.ToolStripMenuItem
  Friend WithEvents ToolStripMenuItemEinfügen As System.Windows.Forms.ToolStripMenuItem
  Friend WithEvents txtProg As System.Windows.Forms.TextBox
End Class
