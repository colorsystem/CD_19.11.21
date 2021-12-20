<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Public Class frmMankiewFarbwerte
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
    Me.SplitContainFarbwerte = New System.Windows.Forms.SplitContainer()
    Me.SplitContainErgebnisse = New System.Windows.Forms.SplitContainer()
    Me.flgFarbwerte = New System.Windows.Forms.DataGridView()
    Me.flgRwerte = New System.Windows.Forms.DataGridView()
    Me.btnEtiketten = New System.Windows.Forms.Button()
    Me.btnRemissionNeu = New System.Windows.Forms.Button()
    Me.btnUserDruck = New System.Windows.Forms.Button()
    Me.btnKopieren = New System.Windows.Forms.Button()
    Me.panRadGrid = New System.Windows.Forms.Panel()
    Me.radGrid_1 = New System.Windows.Forms.RadioButton()
    Me.radGrid_0 = New System.Windows.Forms.RadioButton()
    Me.btnDrucken = New System.Windows.Forms.Button()
    Me.btnRemission = New System.Windows.Forms.Button()
    CType(Me.SplitContainFarbwerte, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContainFarbwerte.Panel1.SuspendLayout()
    Me.SplitContainFarbwerte.Panel2.SuspendLayout()
    Me.SplitContainFarbwerte.SuspendLayout()
    CType(Me.SplitContainErgebnisse, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContainErgebnisse.Panel1.SuspendLayout()
    Me.SplitContainErgebnisse.SuspendLayout()
    CType(Me.flgFarbwerte, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.flgRwerte, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.panRadGrid.SuspendLayout()
    Me.SuspendLayout()
    '
    'SplitContainFarbwerte
    '
    Me.SplitContainFarbwerte.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.SplitContainFarbwerte.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContainFarbwerte.FixedPanel = System.Windows.Forms.FixedPanel.Panel2
    Me.SplitContainFarbwerte.IsSplitterFixed = True
    Me.SplitContainFarbwerte.Location = New System.Drawing.Point(0, 0)
    Me.SplitContainFarbwerte.Name = "SplitContainFarbwerte"
    Me.SplitContainFarbwerte.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'SplitContainFarbwerte.Panel1
    '
    Me.SplitContainFarbwerte.Panel1.BackColor = System.Drawing.SystemColors.Control
    Me.SplitContainFarbwerte.Panel1.Controls.Add(Me.SplitContainErgebnisse)
    '
    'SplitContainFarbwerte.Panel2
    '
    Me.SplitContainFarbwerte.Panel2.BackColor = System.Drawing.SystemColors.ButtonShadow
    Me.SplitContainFarbwerte.Panel2.Controls.Add(Me.btnEtiketten)
    Me.SplitContainFarbwerte.Panel2.Controls.Add(Me.btnRemissionNeu)
    Me.SplitContainFarbwerte.Panel2.Controls.Add(Me.btnUserDruck)
    Me.SplitContainFarbwerte.Panel2.Controls.Add(Me.btnKopieren)
    Me.SplitContainFarbwerte.Panel2.Controls.Add(Me.panRadGrid)
    Me.SplitContainFarbwerte.Panel2.Controls.Add(Me.btnDrucken)
    Me.SplitContainFarbwerte.Panel2.Controls.Add(Me.btnRemission)
    Me.SplitContainFarbwerte.Size = New System.Drawing.Size(900, 609)
    Me.SplitContainFarbwerte.SplitterDistance = 521
    Me.SplitContainFarbwerte.TabIndex = 11
    '
    'SplitContainErgebnisse
    '
    Me.SplitContainErgebnisse.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContainErgebnisse.Location = New System.Drawing.Point(0, 0)
    Me.SplitContainErgebnisse.Name = "SplitContainErgebnisse"
    '
    'SplitContainErgebnisse.Panel1
    '
    Me.SplitContainErgebnisse.Panel1.Controls.Add(Me.flgFarbwerte)
    Me.SplitContainErgebnisse.Panel1.Controls.Add(Me.flgRwerte)
    Me.SplitContainErgebnisse.Size = New System.Drawing.Size(900, 521)
    Me.SplitContainErgebnisse.SplitterDistance = 585
    Me.SplitContainErgebnisse.TabIndex = 1
    '
    'flgFarbwerte
    '
    Me.flgFarbwerte.AllowUserToResizeRows = False
    Me.flgFarbwerte.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.AllCells
    Me.flgFarbwerte.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.flgFarbwerte.Dock = System.Windows.Forms.DockStyle.Fill
    Me.flgFarbwerte.Location = New System.Drawing.Point(0, 0)
    Me.flgFarbwerte.Name = "flgFarbwerte"
    Me.flgFarbwerte.Size = New System.Drawing.Size(585, 521)
    Me.flgFarbwerte.StandardTab = True
    Me.flgFarbwerte.TabIndex = 2
    '
    'flgRwerte
    '
    Me.flgRwerte.AllowUserToResizeRows = False
    Me.flgRwerte.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.AllCells
    Me.flgRwerte.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.EnableWithoutHeaderText
    Me.flgRwerte.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.flgRwerte.ColumnHeadersVisible = False
    Me.flgRwerte.Dock = System.Windows.Forms.DockStyle.Fill
    Me.flgRwerte.Location = New System.Drawing.Point(0, 0)
    Me.flgRwerte.Name = "flgRwerte"
    Me.flgRwerte.Size = New System.Drawing.Size(585, 521)
    Me.flgRwerte.StandardTab = True
    Me.flgRwerte.TabIndex = 3
    Me.flgRwerte.Visible = False
    '
    'btnEtiketten
    '
    Me.btnEtiketten.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnEtiketten.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnEtiketten.Location = New System.Drawing.Point(430, 29)
    Me.btnEtiketten.Name = "btnEtiketten"
    Me.btnEtiketten.Size = New System.Drawing.Size(178, 25)
    Me.btnEtiketten.TabIndex = 20
    Me.btnEtiketten.Text = "392"
    Me.btnEtiketten.UseVisualStyleBackColor = False
    Me.btnEtiketten.Visible = False
    '
    'btnRemissionNeu
    '
    Me.btnRemissionNeu.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnRemissionNeu.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnRemissionNeu.Location = New System.Drawing.Point(3, 3)
    Me.btnRemissionNeu.Name = "btnRemissionNeu"
    Me.btnRemissionNeu.Size = New System.Drawing.Size(187, 26)
    Me.btnRemissionNeu.TabIndex = 19
    Me.btnRemissionNeu.Text = "4687"
    Me.btnRemissionNeu.UseVisualStyleBackColor = False
    '
    'btnUserDruck
    '
    Me.btnUserDruck.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnUserDruck.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnUserDruck.Location = New System.Drawing.Point(430, 4)
    Me.btnUserDruck.Name = "btnUserDruck"
    Me.btnUserDruck.Size = New System.Drawing.Size(178, 25)
    Me.btnUserDruck.TabIndex = 18
    Me.btnUserDruck.Text = "392"
    Me.btnUserDruck.UseVisualStyleBackColor = False
    '
    'btnKopieren
    '
    Me.btnKopieren.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnKopieren.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnKopieren.Location = New System.Drawing.Point(221, 29)
    Me.btnKopieren.Name = "btnKopieren"
    Me.btnKopieren.Size = New System.Drawing.Size(186, 26)
    Me.btnKopieren.TabIndex = 17
    Me.btnKopieren.Text = "389"
    Me.btnKopieren.UseVisualStyleBackColor = False
    '
    'panRadGrid
    '
    Me.panRadGrid.Controls.Add(Me.radGrid_1)
    Me.panRadGrid.Controls.Add(Me.radGrid_0)
    Me.panRadGrid.Location = New System.Drawing.Point(632, 13)
    Me.panRadGrid.Name = "panRadGrid"
    Me.panRadGrid.Size = New System.Drawing.Size(180, 45)
    Me.panRadGrid.TabIndex = 16
    '
    'radGrid_1
    '
    Me.radGrid_1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radGrid_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radGrid_1.Location = New System.Drawing.Point(0, 22)
    Me.radGrid_1.Name = "radGrid_1"
    Me.radGrid_1.Size = New System.Drawing.Size(180, 20)
    Me.radGrid_1.TabIndex = 1
    Me.radGrid_1.Text = "4685"
    Me.radGrid_1.UseVisualStyleBackColor = False
    '
    'radGrid_0
    '
    Me.radGrid_0.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radGrid_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radGrid_0.Checked = True
    Me.radGrid_0.Location = New System.Drawing.Point(0, 0)
    Me.radGrid_0.Name = "radGrid_0"
    Me.radGrid_0.Size = New System.Drawing.Size(180, 20)
    Me.radGrid_0.TabIndex = 0
    Me.radGrid_0.TabStop = True
    Me.radGrid_0.Text = "4650"
    Me.radGrid_0.UseVisualStyleBackColor = False
    '
    'btnDrucken
    '
    Me.btnDrucken.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnDrucken.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnDrucken.Location = New System.Drawing.Point(221, 3)
    Me.btnDrucken.Name = "btnDrucken"
    Me.btnDrucken.Size = New System.Drawing.Size(186, 25)
    Me.btnDrucken.TabIndex = 14
    Me.btnDrucken.Text = "4652"
    Me.btnDrucken.UseVisualStyleBackColor = False
    '
    'btnRemission
    '
    Me.btnRemission.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnRemission.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnRemission.Location = New System.Drawing.Point(3, 29)
    Me.btnRemission.Name = "btnRemission"
    Me.btnRemission.Size = New System.Drawing.Size(187, 26)
    Me.btnRemission.TabIndex = 13
    Me.btnRemission.Text = "4651"
    Me.btnRemission.UseVisualStyleBackColor = False
    '
    'frmMankiewFarbwerte
    '
    Me.AccessibleRole = System.Windows.Forms.AccessibleRole.None
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(192, Byte), Integer))
    Me.ClientSize = New System.Drawing.Size(900, 609)
    Me.ControlBox = False
    Me.Controls.Add(Me.SplitContainFarbwerte)
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmMankiewFarbwerte"
    Me.ShowIcon = False
    Me.ShowInTaskbar = False
    Me.SplitContainFarbwerte.Panel1.ResumeLayout(False)
    Me.SplitContainFarbwerte.Panel2.ResumeLayout(False)
    CType(Me.SplitContainFarbwerte, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContainFarbwerte.ResumeLayout(False)
    Me.SplitContainErgebnisse.Panel1.ResumeLayout(False)
    CType(Me.SplitContainErgebnisse, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContainErgebnisse.ResumeLayout(False)
    CType(Me.flgFarbwerte, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.flgRwerte, System.ComponentModel.ISupportInitialize).EndInit()
    Me.panRadGrid.ResumeLayout(False)
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents SplitContainFarbwerte As System.Windows.Forms.SplitContainer
  Friend WithEvents btnDrucken As System.Windows.Forms.Button
  Friend WithEvents btnRemission As System.Windows.Forms.Button
  Friend WithEvents SplitContainErgebnisse As System.Windows.Forms.SplitContainer
  Friend WithEvents flgFarbwerte As System.Windows.Forms.DataGridView
  Friend WithEvents flgRwerte As System.Windows.Forms.DataGridView
  Friend WithEvents panRadGrid As System.Windows.Forms.Panel
  Friend WithEvents radGrid_0 As System.Windows.Forms.RadioButton
  Friend WithEvents radGrid_1 As System.Windows.Forms.RadioButton
  Friend WithEvents btnKopieren As System.Windows.Forms.Button
  Friend WithEvents btnUserDruck As System.Windows.Forms.Button
  Friend WithEvents btnRemissionNeu As System.Windows.Forms.Button
  Friend WithEvents btnEtiketten As System.Windows.Forms.Button
End Class
