<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmNormlicht
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
    Me.SplitAnzeigen = New System.Windows.Forms.SplitContainer()
    Me.lblHinweis = New System.Windows.Forms.Label()
    Me.txtSumZ = New System.Windows.Forms.TextBox()
    Me.lblSumZ = New System.Windows.Forms.Label()
    Me.txtSumY = New System.Windows.Forms.TextBox()
    Me.lblSumY = New System.Windows.Forms.Label()
    Me.txtSumX = New System.Windows.Forms.TextBox()
    Me.lblSumX = New System.Windows.Forms.Label()
    Me.btnDelete = New System.Windows.Forms.Button()
    Me.btnNewLicht = New System.Windows.Forms.Button()
    Me.btnKopieren = New System.Windows.Forms.Button()
    Me.btnEnd = New System.Windows.Forms.Button()
    Me.chkBld = New System.Windows.Forms.CheckBox()
    Me.lblAnz = New System.Windows.Forms.Label()
    Me.lblNormlicht = New System.Windows.Forms.Label()
    Me.lblBereich = New System.Windows.Forms.Label()
    Me.cboNormlicht = New System.Windows.Forms.ComboBox()
    Me.cboBereich = New System.Windows.Forms.ComboBox()
    Me.dbgNormlicht = New System.Windows.Forms.DataGridView()
    Me.picNormlicht = New System.Windows.Forms.PictureBox()
    CType(Me.SplitAnzeigen, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitAnzeigen.Panel1.SuspendLayout()
    Me.SplitAnzeigen.Panel2.SuspendLayout()
    Me.SplitAnzeigen.SuspendLayout()
    CType(Me.dbgNormlicht, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.picNormlicht, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'SplitAnzeigen
    '
    Me.SplitAnzeigen.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitAnzeigen.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitAnzeigen.IsSplitterFixed = True
    Me.SplitAnzeigen.Location = New System.Drawing.Point(0, 0)
    Me.SplitAnzeigen.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.SplitAnzeigen.Name = "SplitAnzeigen"
    '
    'SplitAnzeigen.Panel1
    '
    Me.SplitAnzeigen.Panel1.BackColor = System.Drawing.SystemColors.Control
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.lblHinweis)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.txtSumZ)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.lblSumZ)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.txtSumY)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.lblSumY)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.txtSumX)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.lblSumX)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.btnDelete)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.btnNewLicht)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.btnKopieren)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.btnEnd)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.chkBld)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.lblAnz)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.lblNormlicht)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.lblBereich)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.cboNormlicht)
    Me.SplitAnzeigen.Panel1.Controls.Add(Me.cboBereich)
    '
    'SplitAnzeigen.Panel2
    '
    Me.SplitAnzeigen.Panel2.BackColor = System.Drawing.SystemColors.Control
    Me.SplitAnzeigen.Panel2.Controls.Add(Me.dbgNormlicht)
    Me.SplitAnzeigen.Panel2.Controls.Add(Me.picNormlicht)
    Me.SplitAnzeigen.Size = New System.Drawing.Size(1191, 698)
    Me.SplitAnzeigen.SplitterDistance = 289
    Me.SplitAnzeigen.SplitterWidth = 5
    Me.SplitAnzeigen.TabIndex = 1
    '
    'lblHinweis
    '
    Me.lblHinweis.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblHinweis.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblHinweis.Location = New System.Drawing.Point(4, 640)
    Me.lblHinweis.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblHinweis.Name = "lblHinweis"
    Me.lblHinweis.Size = New System.Drawing.Size(281, 58)
    Me.lblHinweis.TabIndex = 2
    Me.lblHinweis.Text = "3078"
    '
    'txtSumZ
    '
    Me.txtSumZ.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtSumZ.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.txtSumZ.Location = New System.Drawing.Point(161, 588)
    Me.txtSumZ.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.txtSumZ.Name = "txtSumZ"
    Me.txtSumZ.ReadOnly = True
    Me.txtSumZ.Size = New System.Drawing.Size(91, 22)
    Me.txtSumZ.TabIndex = 16
    Me.txtSumZ.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
    '
    'lblSumZ
    '
    Me.lblSumZ.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSumZ.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblSumZ.Location = New System.Drawing.Point(31, 587)
    Me.lblSumZ.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblSumZ.Name = "lblSumZ"
    Me.lblSumZ.Size = New System.Drawing.Size(129, 26)
    Me.lblSumZ.TabIndex = 15
    Me.lblSumZ.Text = "3077"
    Me.lblSumZ.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtSumY
    '
    Me.txtSumY.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtSumY.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.txtSumY.Location = New System.Drawing.Point(161, 560)
    Me.txtSumY.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.txtSumY.Name = "txtSumY"
    Me.txtSumY.ReadOnly = True
    Me.txtSumY.Size = New System.Drawing.Size(91, 22)
    Me.txtSumY.TabIndex = 14
    Me.txtSumY.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
    '
    'lblSumY
    '
    Me.lblSumY.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSumY.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblSumY.Location = New System.Drawing.Point(31, 559)
    Me.lblSumY.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblSumY.Name = "lblSumY"
    Me.lblSumY.Size = New System.Drawing.Size(129, 26)
    Me.lblSumY.TabIndex = 13
    Me.lblSumY.Text = "3077"
    Me.lblSumY.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtSumX
    '
    Me.txtSumX.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtSumX.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.txtSumX.Location = New System.Drawing.Point(161, 532)
    Me.txtSumX.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.txtSumX.Name = "txtSumX"
    Me.txtSumX.ReadOnly = True
    Me.txtSumX.Size = New System.Drawing.Size(91, 22)
    Me.txtSumX.TabIndex = 12
    Me.txtSumX.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
    '
    'lblSumX
    '
    Me.lblSumX.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSumX.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblSumX.Location = New System.Drawing.Point(31, 530)
    Me.lblSumX.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblSumX.Name = "lblSumX"
    Me.lblSumX.Size = New System.Drawing.Size(129, 26)
    Me.lblSumX.TabIndex = 2
    Me.lblSumX.Text = "3077"
    Me.lblSumX.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'btnDelete
    '
    Me.btnDelete.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnDelete.Location = New System.Drawing.Point(21, 423)
    Me.btnDelete.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.btnDelete.Name = "btnDelete"
    Me.btnDelete.Size = New System.Drawing.Size(232, 28)
    Me.btnDelete.TabIndex = 11
    Me.btnDelete.Text = "3073"
    Me.btnDelete.UseVisualStyleBackColor = True
    Me.btnDelete.Visible = False
    '
    'btnNewLicht
    '
    Me.btnNewLicht.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnNewLicht.Location = New System.Drawing.Point(21, 369)
    Me.btnNewLicht.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.btnNewLicht.Name = "btnNewLicht"
    Me.btnNewLicht.Size = New System.Drawing.Size(232, 28)
    Me.btnNewLicht.TabIndex = 10
    Me.btnNewLicht.Text = "3951"
    Me.btnNewLicht.UseVisualStyleBackColor = True
    Me.btnNewLicht.Visible = False
    '
    'btnKopieren
    '
    Me.btnKopieren.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnKopieren.Location = New System.Drawing.Point(21, 334)
    Me.btnKopieren.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.btnKopieren.Name = "btnKopieren"
    Me.btnKopieren.Size = New System.Drawing.Size(232, 28)
    Me.btnKopieren.TabIndex = 9
    Me.btnKopieren.Text = "389"
    Me.btnKopieren.UseVisualStyleBackColor = True
    '
    'btnEnd
    '
    Me.btnEnd.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnEnd.Location = New System.Drawing.Point(21, 482)
    Me.btnEnd.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.btnEnd.Name = "btnEnd"
    Me.btnEnd.Size = New System.Drawing.Size(232, 28)
    Me.btnEnd.TabIndex = 8
    Me.btnEnd.Text = "1999"
    Me.btnEnd.UseVisualStyleBackColor = True
    '
    'chkBld
    '
    Me.chkBld.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkBld.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkBld.Location = New System.Drawing.Point(21, 287)
    Me.chkBld.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.chkBld.Name = "chkBld"
    Me.chkBld.Size = New System.Drawing.Size(232, 26)
    Me.chkBld.TabIndex = 7
    Me.chkBld.Text = "382"
    Me.chkBld.UseVisualStyleBackColor = False
    '
    'lblAnz
    '
    Me.lblAnz.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblAnz.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblAnz.Location = New System.Drawing.Point(85, 233)
    Me.lblAnz.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblAnz.Name = "lblAnz"
    Me.lblAnz.Size = New System.Drawing.Size(101, 28)
    Me.lblAnz.TabIndex = 6
    Me.lblAnz.Text = "3940"
    Me.lblAnz.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblNormlicht
    '
    Me.lblNormlicht.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblNormlicht.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblNormlicht.Location = New System.Drawing.Point(-21, 155)
    Me.lblNormlicht.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblNormlicht.Name = "lblNormlicht"
    Me.lblNormlicht.Size = New System.Drawing.Size(317, 28)
    Me.lblNormlicht.TabIndex = 5
    Me.lblNormlicht.Text = "3941"
    Me.lblNormlicht.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblBereich
    '
    Me.lblBereich.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblBereich.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblBereich.Location = New System.Drawing.Point(0, 74)
    Me.lblBereich.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblBereich.Name = "lblBereich"
    Me.lblBereich.Size = New System.Drawing.Size(285, 28)
    Me.lblBereich.TabIndex = 4
    Me.lblBereich.Text = "3940"
    Me.lblBereich.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'cboNormlicht
    '
    Me.cboNormlicht.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboNormlicht.FormattingEnabled = True
    Me.cboNormlicht.Location = New System.Drawing.Point(0, 187)
    Me.cboNormlicht.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.cboNormlicht.Name = "cboNormlicht"
    Me.cboNormlicht.Size = New System.Drawing.Size(286, 24)
    Me.cboNormlicht.TabIndex = 1
    '
    'cboBereich
    '
    Me.cboBereich.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboBereich.FormattingEnabled = True
    Me.cboBereich.Location = New System.Drawing.Point(0, 106)
    Me.cboBereich.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.cboBereich.Name = "cboBereich"
    Me.cboBereich.Size = New System.Drawing.Size(285, 24)
    Me.cboBereich.TabIndex = 0
    '
    'dbgNormlicht
    '
    Me.dbgNormlicht.AllowUserToAddRows = False
    Me.dbgNormlicht.AllowUserToDeleteRows = False
    Me.dbgNormlicht.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgNormlicht.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgNormlicht.Location = New System.Drawing.Point(0, 0)
    Me.dbgNormlicht.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.dbgNormlicht.Name = "dbgNormlicht"
    Me.dbgNormlicht.RowTemplate.Height = 24
    Me.dbgNormlicht.Size = New System.Drawing.Size(897, 698)
    Me.dbgNormlicht.TabIndex = 1
    '
    'picNormlicht
    '
    Me.picNormlicht.Dock = System.Windows.Forms.DockStyle.Fill
    Me.picNormlicht.Location = New System.Drawing.Point(0, 0)
    Me.picNormlicht.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.picNormlicht.Name = "picNormlicht"
    Me.picNormlicht.Size = New System.Drawing.Size(897, 698)
    Me.picNormlicht.TabIndex = 0
    Me.picNormlicht.TabStop = False
    '
    'frmNormlicht
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(8.0!, 16.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(1191, 698)
    Me.ControlBox = False
    Me.Controls.Add(Me.SplitAnzeigen)
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
    Me.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmNormlicht"
    Me.Text = "frmNormLicht"
    Me.SplitAnzeigen.Panel1.ResumeLayout(False)
    Me.SplitAnzeigen.Panel1.PerformLayout
    Me.SplitAnzeigen.Panel2.ResumeLayout(false)
    CType(Me.SplitAnzeigen,System.ComponentModel.ISupportInitialize).EndInit
    Me.SplitAnzeigen.ResumeLayout(false)
    CType(Me.dbgNormlicht,System.ComponentModel.ISupportInitialize).EndInit
    CType(Me.picNormlicht,System.ComponentModel.ISupportInitialize).EndInit
    Me.ResumeLayout(false)

End Sub
  Friend WithEvents SplitAnzeigen As System.Windows.Forms.SplitContainer
  Friend WithEvents btnKopieren As System.Windows.Forms.Button
  Friend WithEvents btnEnd As System.Windows.Forms.Button
  Friend WithEvents chkBld As System.Windows.Forms.CheckBox
  Friend WithEvents lblAnz As System.Windows.Forms.Label
  Friend WithEvents lblNormlicht As System.Windows.Forms.Label
  Friend WithEvents lblBereich As System.Windows.Forms.Label
  Friend WithEvents cboNormlicht As System.Windows.Forms.ComboBox
  Friend WithEvents cboBereich As System.Windows.Forms.ComboBox
  Friend WithEvents dbgNormlicht As System.Windows.Forms.DataGridView
  Friend WithEvents picNormlicht As System.Windows.Forms.PictureBox
  Friend WithEvents btnNewLicht As System.Windows.Forms.Button
  Friend WithEvents btnDelete As System.Windows.Forms.Button
  Friend WithEvents txtSumZ As System.Windows.Forms.TextBox
  Friend WithEvents lblSumZ As System.Windows.Forms.Label
  Friend WithEvents txtSumY As System.Windows.Forms.TextBox
  Friend WithEvents lblSumY As System.Windows.Forms.Label
  Friend WithEvents txtSumX As System.Windows.Forms.TextBox
  Friend WithEvents lblSumX As System.Windows.Forms.Label
  Friend WithEvents lblHinweis As System.Windows.Forms.Label
End Class
