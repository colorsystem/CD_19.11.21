<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmConvertMisch
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
    Me.splConvertMsh = New System.Windows.Forms.SplitContainer()
    Me.splFarbmittel = New System.Windows.Forms.SplitContainer()
    Me.lstSOR = New System.Windows.Forms.ListBox()
    Me.lstFAR = New System.Windows.Forms.ListBox()
    Me.splDatFarbmittel = New System.Windows.Forms.SplitContainer()
    Me.txtRestGew = New System.Windows.Forms.TextBox()
    Me.lblRestGew = New System.Windows.Forms.Label()
    Me.lblMSH = New System.Windows.Forms.Label()
    Me.txtDick = New System.Windows.Forms.TextBox()
    Me.lblDick = New System.Windows.Forms.Label()
    Me.lblPrefix = New System.Windows.Forms.Label()
    Me.txtPrefix = New System.Windows.Forms.TextBox()
    Me.btnSpeichern = New System.Windows.Forms.Button()
    Me.btnCalculate = New System.Windows.Forms.Button()
    Me.btnMessUnt = New System.Windows.Forms.Button()
    Me.lblMessUnt = New System.Windows.Forms.Label()
    Me.cboMsh = New System.Windows.Forms.ComboBox()
    Me.dbgFarbmittel = New System.Windows.Forms.DataGridView()
    CType(Me.splConvertMsh, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splConvertMsh.Panel1.SuspendLayout()
    Me.splConvertMsh.Panel2.SuspendLayout()
    Me.splConvertMsh.SuspendLayout()
    CType(Me.splFarbmittel, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splFarbmittel.Panel1.SuspendLayout()
    Me.splFarbmittel.Panel2.SuspendLayout()
    Me.splFarbmittel.SuspendLayout()
    CType(Me.splDatFarbmittel, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splDatFarbmittel.Panel1.SuspendLayout()
    Me.splDatFarbmittel.Panel2.SuspendLayout()
    Me.splDatFarbmittel.SuspendLayout()
    CType(Me.dbgFarbmittel, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'splConvertMsh
    '
    Me.splConvertMsh.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splConvertMsh.Location = New System.Drawing.Point(0, 0)
    Me.splConvertMsh.Name = "splConvertMsh"
    '
    'splConvertMsh.Panel1
    '
    Me.splConvertMsh.Panel1.Controls.Add(Me.splFarbmittel)
    '
    'splConvertMsh.Panel2
    '
    Me.splConvertMsh.Panel2.Controls.Add(Me.splDatFarbmittel)
    Me.splConvertMsh.Size = New System.Drawing.Size(1013, 569)
    Me.splConvertMsh.SplitterDistance = 264
    Me.splConvertMsh.TabIndex = 0
    '
    'splFarbmittel
    '
    Me.splFarbmittel.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splFarbmittel.Location = New System.Drawing.Point(0, 0)
    Me.splFarbmittel.Name = "splFarbmittel"
    Me.splFarbmittel.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splFarbmittel.Panel1
    '
    Me.splFarbmittel.Panel1.Controls.Add(Me.lstSOR)
    '
    'splFarbmittel.Panel2
    '
    Me.splFarbmittel.Panel2.Controls.Add(Me.lstFAR)
    Me.splFarbmittel.Size = New System.Drawing.Size(264, 569)
    Me.splFarbmittel.SplitterDistance = 353
    Me.splFarbmittel.TabIndex = 61
    '
    'lstSOR
    '
    Me.lstSOR.Dock = System.Windows.Forms.DockStyle.Fill
    Me.lstSOR.FormattingEnabled = True
    Me.lstSOR.Location = New System.Drawing.Point(0, 0)
    Me.lstSOR.Name = "lstSOR"
    Me.lstSOR.Size = New System.Drawing.Size(264, 353)
    Me.lstSOR.TabIndex = 30
    '
    'lstFAR
    '
    Me.lstFAR.Dock = System.Windows.Forms.DockStyle.Fill
    Me.lstFAR.FormattingEnabled = True
    Me.lstFAR.Location = New System.Drawing.Point(0, 0)
    Me.lstFAR.Name = "lstFAR"
    Me.lstFAR.Size = New System.Drawing.Size(264, 212)
    Me.lstFAR.TabIndex = 62
    Me.lstFAR.Visible = False
    '
    'splDatFarbmittel
    '
    Me.splDatFarbmittel.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splDatFarbmittel.Location = New System.Drawing.Point(0, 0)
    Me.splDatFarbmittel.Name = "splDatFarbmittel"
    Me.splDatFarbmittel.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splDatFarbmittel.Panel1
    '
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.txtRestGew)
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.lblRestGew)
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.lblMSH)
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.txtDick)
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.lblDick)
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.lblPrefix)
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.txtPrefix)
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.btnSpeichern)
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.btnCalculate)
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.btnMessUnt)
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.lblMessUnt)
    Me.splDatFarbmittel.Panel1.Controls.Add(Me.cboMsh)
    '
    'splDatFarbmittel.Panel2
    '
    Me.splDatFarbmittel.Panel2.Controls.Add(Me.dbgFarbmittel)
    Me.splDatFarbmittel.Size = New System.Drawing.Size(745, 569)
    Me.splDatFarbmittel.SplitterDistance = 93
    Me.splDatFarbmittel.TabIndex = 0
    '
    'txtRestGew
    '
    Me.txtRestGew.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtRestGew.Location = New System.Drawing.Point(684, 68)
    Me.txtRestGew.Name = "txtRestGew"
    Me.txtRestGew.Size = New System.Drawing.Size(44, 20)
    Me.txtRestGew.TabIndex = 113
    Me.txtRestGew.Text = "0,0001"
    Me.txtRestGew.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
    '
    'lblRestGew
    '
    Me.lblRestGew.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblRestGew.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblRestGew.Location = New System.Drawing.Point(518, 68)
    Me.lblRestGew.Name = "lblRestGew"
    Me.lblRestGew.Size = New System.Drawing.Size(160, 19)
    Me.lblRestGew.TabIndex = 112
    Me.lblRestGew.Text = "172"
    Me.lblRestGew.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMSH
    '
    Me.lblMSH.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblMSH.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.lblMSH.Location = New System.Drawing.Point(10, 10)
    Me.lblMSH.Name = "lblMSH"
    Me.lblMSH.Size = New System.Drawing.Size(226, 20)
    Me.lblMSH.TabIndex = 88
    Me.lblMSH.Text = "422"
    Me.lblMSH.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'txtDick
    '
    Me.txtDick.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtDick.Location = New System.Drawing.Point(518, 36)
    Me.txtDick.Name = "txtDick"
    Me.txtDick.Size = New System.Drawing.Size(40, 20)
    Me.txtDick.TabIndex = 87
    Me.txtDick.Text = "1"
    Me.txtDick.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
    '
    'lblDick
    '
    Me.lblDick.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblDick.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblDick.Location = New System.Drawing.Point(240, 37)
    Me.lblDick.Name = "lblDick"
    Me.lblDick.Size = New System.Drawing.Size(272, 20)
    Me.lblDick.TabIndex = 86
    Me.lblDick.Text = "797"
    Me.lblDick.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblPrefix
    '
    Me.lblPrefix.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblPrefix.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblPrefix.Location = New System.Drawing.Point(564, 36)
    Me.lblPrefix.Name = "lblPrefix"
    Me.lblPrefix.Size = New System.Drawing.Size(114, 19)
    Me.lblPrefix.TabIndex = 85
    Me.lblPrefix.Text = "3820"
    Me.lblPrefix.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtPrefix
    '
    Me.txtPrefix.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtPrefix.Location = New System.Drawing.Point(684, 35)
    Me.txtPrefix.Name = "txtPrefix"
    Me.txtPrefix.Size = New System.Drawing.Size(44, 20)
    Me.txtPrefix.TabIndex = 84
    Me.txtPrefix.Text = "#"
    '
    'btnSpeichern
    '
    Me.btnSpeichern.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnSpeichern.BackColor = System.Drawing.SystemColors.Control
    Me.btnSpeichern.Enabled = False
    Me.btnSpeichern.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnSpeichern.Location = New System.Drawing.Point(242, 66)
    Me.btnSpeichern.Name = "btnSpeichern"
    Me.btnSpeichern.Size = New System.Drawing.Size(219, 25)
    Me.btnSpeichern.TabIndex = 83
    Me.btnSpeichern.Text = "854"
    Me.btnSpeichern.UseVisualStyleBackColor = False
    '
    'btnCalculate
    '
    Me.btnCalculate.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnCalculate.BackColor = System.Drawing.SystemColors.Control
    Me.btnCalculate.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnCalculate.Location = New System.Drawing.Point(13, 64)
    Me.btnCalculate.Name = "btnCalculate"
    Me.btnCalculate.Size = New System.Drawing.Size(223, 25)
    Me.btnCalculate.TabIndex = 82
    Me.btnCalculate.Text = "360"
    Me.btnCalculate.UseVisualStyleBackColor = False
    '
    'btnMessUnt
    '
    Me.btnMessUnt.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMessUnt.BackColor = System.Drawing.SystemColors.Control
    Me.btnMessUnt.Location = New System.Drawing.Point(242, 6)
    Me.btnMessUnt.Name = "btnMessUnt"
    Me.btnMessUnt.Size = New System.Drawing.Size(148, 23)
    Me.btnMessUnt.TabIndex = 80
    Me.btnMessUnt.Text = "808"
    Me.btnMessUnt.UseVisualStyleBackColor = False
    '
    'lblMessUnt
    '
    Me.lblMessUnt.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMessUnt.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMessUnt.Location = New System.Drawing.Point(396, 9)
    Me.lblMessUnt.Name = "lblMessUnt"
    Me.lblMessUnt.Size = New System.Drawing.Size(178, 20)
    Me.lblMessUnt.TabIndex = 81
    Me.lblMessUnt.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    '
    'cboMsh
    '
    Me.cboMsh.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMsh.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboMsh.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.cboMsh.FormattingEnabled = True
    Me.cboMsh.Location = New System.Drawing.Point(13, 36)
    Me.cboMsh.Margin = New System.Windows.Forms.Padding(3, 1, 1, 1)
    Me.cboMsh.Name = "cboMsh"
    Me.cboMsh.Size = New System.Drawing.Size(223, 21)
    Me.cboMsh.TabIndex = 79
    '
    'dbgFarbmittel
    '
    Me.dbgFarbmittel.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgFarbmittel.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgFarbmittel.Location = New System.Drawing.Point(0, 0)
    Me.dbgFarbmittel.Name = "dbgFarbmittel"
    Me.dbgFarbmittel.Size = New System.Drawing.Size(745, 472)
    Me.dbgFarbmittel.TabIndex = 0
    '
    'frmConvertMisch
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(1013, 569)
    Me.ControlBox = False
    Me.Controls.Add(Me.splConvertMsh)
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmConvertMisch"
    Me.Text = "frmConvertMisch"
    Me.splConvertMsh.Panel1.ResumeLayout(False)
    Me.splConvertMsh.Panel2.ResumeLayout(False)
    CType(Me.splConvertMsh, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splConvertMsh.ResumeLayout(False)
    Me.splFarbmittel.Panel1.ResumeLayout(False)
    Me.splFarbmittel.Panel2.ResumeLayout(False)
    CType(Me.splFarbmittel, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splFarbmittel.ResumeLayout(False)
    Me.splDatFarbmittel.Panel1.ResumeLayout(False)
    Me.splDatFarbmittel.Panel1.PerformLayout()
    Me.splDatFarbmittel.Panel2.ResumeLayout(False)
    CType(Me.splDatFarbmittel, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splDatFarbmittel.ResumeLayout(False)
    CType(Me.dbgFarbmittel, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents splConvertMsh As System.Windows.Forms.SplitContainer
  Friend WithEvents splDatFarbmittel As System.Windows.Forms.SplitContainer
  Friend WithEvents cboMsh As System.Windows.Forms.ComboBox
  Friend WithEvents splFarbmittel As System.Windows.Forms.SplitContainer
  Friend WithEvents btnMessUnt As System.Windows.Forms.Button
  Friend WithEvents lblMessUnt As System.Windows.Forms.Label
  Friend WithEvents lstSOR As System.Windows.Forms.ListBox
  Friend WithEvents lstFAR As System.Windows.Forms.ListBox
  Friend WithEvents btnCalculate As System.Windows.Forms.Button
  Friend WithEvents txtPrefix As System.Windows.Forms.TextBox
  Friend WithEvents btnSpeichern As System.Windows.Forms.Button
  Friend WithEvents lblPrefix As System.Windows.Forms.Label
  Friend WithEvents dbgFarbmittel As System.Windows.Forms.DataGridView
  Friend WithEvents txtDick As System.Windows.Forms.TextBox
  Friend WithEvents lblDick As System.Windows.Forms.Label
  Friend WithEvents lblMSH As System.Windows.Forms.Label
  Friend WithEvents txtRestGew As System.Windows.Forms.TextBox
  Friend WithEvents lblRestGew As System.Windows.Forms.Label
End Class
