<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmMiscel
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
    Me.dbgPruefm = New System.Windows.Forms.DataGridView()
    Me.dbgProart = New System.Windows.Forms.DataGridView()
    Me.btnMiscel = New System.Windows.Forms.Button()
    Me.lblPruefm = New System.Windows.Forms.Label()
    Me.lblProart = New System.Windows.Forms.Label()
    Me.splMiscel = New System.Windows.Forms.SplitContainer()
    Me.TabMiscel = New System.Windows.Forms.TabControl()
    Me.TabPage1 = New System.Windows.Forms.TabPage()
    Me.dbgParameterList = New System.Windows.Forms.DataGridView()
    Me.lblParameterList = New System.Windows.Forms.Label()
    Me.dbgParameter = New System.Windows.Forms.DataGridView()
    Me.lblParameter = New System.Windows.Forms.Label()
    Me.TabPage2 = New System.Windows.Forms.TabPage()
    CType(Me.dbgPruefm, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.dbgProart, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.splMiscel, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splMiscel.Panel1.SuspendLayout()
    Me.splMiscel.Panel2.SuspendLayout()
    Me.splMiscel.SuspendLayout()
    Me.TabMiscel.SuspendLayout()
    Me.TabPage1.SuspendLayout()
    CType(Me.dbgParameterList, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.dbgParameter, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.TabPage2.SuspendLayout()
    Me.SuspendLayout()
    '
    'dbgPruefm
    '
    Me.dbgPruefm.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.dbgPruefm.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgPruefm.Location = New System.Drawing.Point(33, 66)
    Me.dbgPruefm.Name = "dbgPruefm"
    Me.dbgPruefm.Size = New System.Drawing.Size(362, 308)
    Me.dbgPruefm.TabIndex = 0
    '
    'dbgProart
    '
    Me.dbgProart.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.dbgProart.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgProart.Location = New System.Drawing.Point(416, 66)
    Me.dbgProart.Name = "dbgProart"
    Me.dbgProart.Size = New System.Drawing.Size(362, 308)
    Me.dbgProart.TabIndex = 1
    '
    'btnMiscel
    '
    Me.btnMiscel.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMiscel.Location = New System.Drawing.Point(360, 15)
    Me.btnMiscel.Name = "btnMiscel"
    Me.btnMiscel.Size = New System.Drawing.Size(138, 33)
    Me.btnMiscel.TabIndex = 4
    Me.btnMiscel.Text = "1999"
    Me.btnMiscel.UseVisualStyleBackColor = True
    '
    'lblPruefm
    '
    Me.lblPruefm.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblPruefm.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblPruefm.Location = New System.Drawing.Point(34, 42)
    Me.lblPruefm.Name = "lblPruefm"
    Me.lblPruefm.Size = New System.Drawing.Size(361, 21)
    Me.lblPruefm.TabIndex = 5
    Me.lblPruefm.Text = "1640"
    Me.lblPruefm.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblProart
    '
    Me.lblProart.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblProart.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblProart.Location = New System.Drawing.Point(417, 42)
    Me.lblProart.Name = "lblProart"
    Me.lblProart.Size = New System.Drawing.Size(361, 21)
    Me.lblProart.TabIndex = 6
    Me.lblProart.Text = "1641"
    Me.lblProart.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'splMiscel
    '
    Me.splMiscel.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splMiscel.FixedPanel = System.Windows.Forms.FixedPanel.Panel2
    Me.splMiscel.Location = New System.Drawing.Point(0, 0)
    Me.splMiscel.Name = "splMiscel"
    Me.splMiscel.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splMiscel.Panel1
    '
    Me.splMiscel.Panel1.Controls.Add(Me.TabMiscel)
    '
    'splMiscel.Panel2
    '
    Me.splMiscel.Panel2.Controls.Add(Me.btnMiscel)
    Me.splMiscel.Size = New System.Drawing.Size(830, 525)
    Me.splMiscel.SplitterDistance = 461
    Me.splMiscel.TabIndex = 7
    '
    'TabMiscel
    '
    Me.TabMiscel.Controls.Add(Me.TabPage1)
    Me.TabMiscel.Controls.Add(Me.TabPage2)
    Me.TabMiscel.Dock = System.Windows.Forms.DockStyle.Fill
    Me.TabMiscel.Location = New System.Drawing.Point(0, 0)
    Me.TabMiscel.Name = "TabMiscel"
    Me.TabMiscel.SelectedIndex = 0
    Me.TabMiscel.Size = New System.Drawing.Size(830, 461)
    Me.TabMiscel.TabIndex = 0
    '
    'TabPage1
    '
    Me.TabPage1.Controls.Add(Me.dbgParameterList)
    Me.TabPage1.Controls.Add(Me.lblParameterList)
    Me.TabPage1.Controls.Add(Me.dbgParameter)
    Me.TabPage1.Controls.Add(Me.lblParameter)
    Me.TabPage1.Location = New System.Drawing.Point(4, 22)
    Me.TabPage1.Name = "TabPage1"
    Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
    Me.TabPage1.Size = New System.Drawing.Size(822, 435)
    Me.TabPage1.TabIndex = 0
    Me.TabPage1.Text = "TabPage1"
    Me.TabPage1.UseVisualStyleBackColor = True
    '
    'dbgParameterList
    '
    Me.dbgParameterList.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.dbgParameterList.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgParameterList.Location = New System.Drawing.Point(511, 65)
    Me.dbgParameterList.Name = "dbgParameterList"
    Me.dbgParameterList.Size = New System.Drawing.Size(305, 308)
    Me.dbgParameterList.TabIndex = 8
    '
    'lblParameterList
    '
    Me.lblParameterList.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblParameterList.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblParameterList.Location = New System.Drawing.Point(511, 41)
    Me.lblParameterList.Name = "lblParameterList"
    Me.lblParameterList.Size = New System.Drawing.Size(305, 21)
    Me.lblParameterList.TabIndex = 9
    Me.lblParameterList.Text = "419"
    Me.lblParameterList.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'dbgParameter
    '
    Me.dbgParameter.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.dbgParameter.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgParameter.Location = New System.Drawing.Point(8, 65)
    Me.dbgParameter.Name = "dbgParameter"
    Me.dbgParameter.Size = New System.Drawing.Size(497, 308)
    Me.dbgParameter.TabIndex = 6
    '
    'lblParameter
    '
    Me.lblParameter.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblParameter.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblParameter.Location = New System.Drawing.Point(6, 41)
    Me.lblParameter.Name = "lblParameter"
    Me.lblParameter.Size = New System.Drawing.Size(499, 21)
    Me.lblParameter.TabIndex = 7
    Me.lblParameter.Text = "415"
    Me.lblParameter.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'TabPage2
    '
    Me.TabPage2.Controls.Add(Me.dbgPruefm)
    Me.TabPage2.Controls.Add(Me.lblProart)
    Me.TabPage2.Controls.Add(Me.lblPruefm)
    Me.TabPage2.Controls.Add(Me.dbgProart)
    Me.TabPage2.Location = New System.Drawing.Point(4, 22)
    Me.TabPage2.Name = "TabPage2"
    Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
    Me.TabPage2.Size = New System.Drawing.Size(822, 435)
    Me.TabPage2.TabIndex = 1
    Me.TabPage2.Text = "TabPage2"
    Me.TabPage2.UseVisualStyleBackColor = True
    '
    'frmMiscel
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(830, 525)
    Me.ControlBox = False
    Me.Controls.Add(Me.splMiscel)
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmMiscel"
    Me.Text = "frmMiscel"
    CType(Me.dbgPruefm, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.dbgProart, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splMiscel.Panel1.ResumeLayout(False)
    Me.splMiscel.Panel2.ResumeLayout(False)
    CType(Me.splMiscel, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splMiscel.ResumeLayout(False)
    Me.TabMiscel.ResumeLayout(False)
    Me.TabPage1.ResumeLayout(False)
    CType(Me.dbgParameterList, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.dbgParameter, System.ComponentModel.ISupportInitialize).EndInit()
    Me.TabPage2.ResumeLayout(False)
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents dbgPruefm As System.Windows.Forms.DataGridView
  Friend WithEvents dbgProart As System.Windows.Forms.DataGridView
  Friend WithEvents btnMiscel As System.Windows.Forms.Button
  Friend WithEvents lblPruefm As System.Windows.Forms.Label
  Friend WithEvents lblProart As System.Windows.Forms.Label
  Friend WithEvents splMiscel As System.Windows.Forms.SplitContainer
  Friend WithEvents TabMiscel As System.Windows.Forms.TabControl
  Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
  Friend WithEvents dbgParameterList As System.Windows.Forms.DataGridView
  Friend WithEvents lblParameterList As System.Windows.Forms.Label
  Friend WithEvents dbgParameter As System.Windows.Forms.DataGridView
  Friend WithEvents lblParameter As System.Windows.Forms.Label
  Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
End Class
