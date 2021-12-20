<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmDARF
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
    Me.TabDARF = New System.Windows.Forms.TabControl()
    Me.TabPage0 = New System.Windows.Forms.TabPage()
    Me.SplitDARF_0 = New System.Windows.Forms.SplitContainer()
    Me.panDARF_0 = New System.Windows.Forms.Panel()
    Me.lblHinweis = New System.Windows.Forms.Label()
    Me.txtSQL_0 = New System.Windows.Forms.TextBox()
    Me.btnRUN_0 = New System.Windows.Forms.Button()
    Me.lblSQL_0 = New System.Windows.Forms.Label()
    Me.btnMARK_0 = New System.Windows.Forms.Button()
    Me.lblGRP_0 = New System.Windows.Forms.Label()
    Me.txtANZ_0 = New System.Windows.Forms.TextBox()
    Me.cboGRP_0 = New System.Windows.Forms.ComboBox()
    Me.lblANZ_0 = New System.Windows.Forms.Label()
    Me.chkDAT_0 = New System.Windows.Forms.CheckBox()
    Me.btnZEIG_0 = New System.Windows.Forms.Button()
    Me.lblVON_0 = New System.Windows.Forms.Label()
    Me.txtBIS_0 = New System.Windows.Forms.TextBox()
    Me.txtVON_0 = New System.Windows.Forms.TextBox()
    Me.lblBIS_0 = New System.Windows.Forms.Label()
    Me.dbgREF_0 = New System.Windows.Forms.DataGridView()
    Me.TabPage1 = New System.Windows.Forms.TabPage()
    Me.SplitDARF_1 = New System.Windows.Forms.SplitContainer()
    Me.panDARF_1 = New System.Windows.Forms.Panel()
    Me.cboGRPN = New System.Windows.Forms.ComboBox()
    Me.chkGRPN = New System.Windows.Forms.CheckBox()
    Me.lblGRPN = New System.Windows.Forms.Label()
    Me.chkANEU = New System.Windows.Forms.CheckBox()
    Me.lblSQL_1 = New System.Windows.Forms.Label()
    Me.chkARC_1 = New System.Windows.Forms.CheckBox()
    Me.txtSQL_1 = New System.Windows.Forms.TextBox()
    Me.chkARCN = New System.Windows.Forms.CheckBox()
    Me.lblGRP_1 = New System.Windows.Forms.Label()
    Me.txtKENN_1 = New System.Windows.Forms.TextBox()
    Me.cboGRP_1 = New System.Windows.Forms.ComboBox()
    Me.lblKENN_1 = New System.Windows.Forms.Label()
    Me.chkDAT_1 = New System.Windows.Forms.CheckBox()
    Me.chkDUP = New System.Windows.Forms.CheckBox()
    Me.lblVON_1 = New System.Windows.Forms.Label()
    Me.btnRUN_1 = New System.Windows.Forms.Button()
    Me.txtVON_1 = New System.Windows.Forms.TextBox()
    Me.btnMARK_1 = New System.Windows.Forms.Button()
    Me.lblBIS_1 = New System.Windows.Forms.Label()
    Me.txtANZ_1 = New System.Windows.Forms.TextBox()
    Me.txtBIS_1 = New System.Windows.Forms.TextBox()
    Me.lblANZ_1 = New System.Windows.Forms.Label()
    Me.btnZEIG_1 = New System.Windows.Forms.Button()
    Me.dbgREF_1 = New System.Windows.Forms.DataGridView()
    Me.TabPage2 = New System.Windows.Forms.TabPage()
    Me.SplitDARF_2 = New System.Windows.Forms.SplitContainer()
    Me.panDARF_2 = New System.Windows.Forms.Panel()
    Me.btnCLIP_2 = New System.Windows.Forms.Button()
    Me.txtBIS_2 = New System.Windows.Forms.TextBox()
    Me.chkARC_2 = New System.Windows.Forms.CheckBox()
    Me.lblSQL_2 = New System.Windows.Forms.Label()
    Me.btnRUN_2 = New System.Windows.Forms.Button()
    Me.txtSQL_2 = New System.Windows.Forms.TextBox()
    Me.btnMARK_2 = New System.Windows.Forms.Button()
    Me.lblGRP_2 = New System.Windows.Forms.Label()
    Me.txtANZ_2 = New System.Windows.Forms.TextBox()
    Me.cboGRP_2 = New System.Windows.Forms.ComboBox()
    Me.lblANZ_2 = New System.Windows.Forms.Label()
    Me.chkDAT_2 = New System.Windows.Forms.CheckBox()
    Me.btnZEIG_2 = New System.Windows.Forms.Button()
    Me.lblVON_2 = New System.Windows.Forms.Label()
    Me.txtVON_2 = New System.Windows.Forms.TextBox()
    Me.lblBIS_2 = New System.Windows.Forms.Label()
    Me.dbgREF_2 = New System.Windows.Forms.DataGridView()
    Me.grdWRT_2 = New System.Windows.Forms.DataGridView()
    Me.TabPage3 = New System.Windows.Forms.TabPage()
    Me.SplitDARF_3 = New System.Windows.Forms.SplitContainer()
    Me.panDARF_3 = New System.Windows.Forms.Panel()
    Me.txtKENN_3 = New System.Windows.Forms.TextBox()
    Me.btnCLIP_3 = New System.Windows.Forms.Button()
    Me.lblKENN_3 = New System.Windows.Forms.Label()
    Me.lblGRP_3 = New System.Windows.Forms.Label()
    Me.cboGRP_3 = New System.Windows.Forms.ComboBox()
    Me.chkARC_3 = New System.Windows.Forms.CheckBox()
    Me.btnRUN_3 = New System.Windows.Forms.Button()
    Me.grdWRT_3 = New System.Windows.Forms.DataGridView()
    Me.TabPage4 = New System.Windows.Forms.TabPage()
    Me.SplitDARF_4 = New System.Windows.Forms.SplitContainer()
    Me.panDARF_4 = New System.Windows.Forms.Panel()
    Me.txtANZ_4 = New System.Windows.Forms.TextBox()
    Me.txtSQL_4 = New System.Windows.Forms.TextBox()
    Me.lblANZ_4 = New System.Windows.Forms.Label()
    Me.lblSQL_4 = New System.Windows.Forms.Label()
    Me.chkARCR = New System.Windows.Forms.CheckBox()
    Me.lblGRP_4 = New System.Windows.Forms.Label()
    Me.txtDIK = New System.Windows.Forms.TextBox()
    Me.cboGRP_4 = New System.Windows.Forms.ComboBox()
    Me.lblDIK = New System.Windows.Forms.Label()
    Me.chkDAT_4 = New System.Windows.Forms.CheckBox()
    Me.cboGRPR = New System.Windows.Forms.ComboBox()
    Me.lblVON_4 = New System.Windows.Forms.Label()
    Me.lblGRPR = New System.Windows.Forms.Label()
    Me.txtVON_4 = New System.Windows.Forms.TextBox()
    Me.cboMSH = New System.Windows.Forms.ComboBox()
    Me.lblBIS_4 = New System.Windows.Forms.Label()
    Me.lblMSH = New System.Windows.Forms.Label()
    Me.txtBIS_4 = New System.Windows.Forms.TextBox()
    Me.chkARC_4 = New System.Windows.Forms.CheckBox()
    Me.btnZEIG_4 = New System.Windows.Forms.Button()
    Me.btnRUN_4 = New System.Windows.Forms.Button()
    Me.btnMARK_4 = New System.Windows.Forms.Button()
    Me.dbgREF_4 = New System.Windows.Forms.DataGridView()
    Me.lblMESS = New System.Windows.Forms.Label()
    Me.cboMESS = New System.Windows.Forms.ComboBox()
    Me.SplitBasis = New System.Windows.Forms.SplitContainer()
    Me.cboUSER = New System.Windows.Forms.ComboBox()
    Me.lblUSE = New System.Windows.Forms.Label()
    Me.btnEnd = New System.Windows.Forms.Button()
    Me.TabDARF.SuspendLayout()
    Me.TabPage0.SuspendLayout()
    CType(Me.SplitDARF_0, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitDARF_0.Panel1.SuspendLayout()
    Me.SplitDARF_0.Panel2.SuspendLayout()
    Me.SplitDARF_0.SuspendLayout()
    Me.panDARF_0.SuspendLayout()
    CType(Me.dbgREF_0, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.TabPage1.SuspendLayout()
    CType(Me.SplitDARF_1, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitDARF_1.Panel1.SuspendLayout()
    Me.SplitDARF_1.Panel2.SuspendLayout()
    Me.SplitDARF_1.SuspendLayout()
    Me.panDARF_1.SuspendLayout()
    CType(Me.dbgREF_1, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.TabPage2.SuspendLayout()
    CType(Me.SplitDARF_2, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitDARF_2.Panel1.SuspendLayout()
    Me.SplitDARF_2.Panel2.SuspendLayout()
    Me.SplitDARF_2.SuspendLayout()
    Me.panDARF_2.SuspendLayout()
    CType(Me.dbgREF_2, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.grdWRT_2, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.TabPage3.SuspendLayout()
    CType(Me.SplitDARF_3, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitDARF_3.Panel1.SuspendLayout()
    Me.SplitDARF_3.Panel2.SuspendLayout()
    Me.SplitDARF_3.SuspendLayout()
    Me.panDARF_3.SuspendLayout()
    CType(Me.grdWRT_3, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.TabPage4.SuspendLayout()
    CType(Me.SplitDARF_4, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitDARF_4.Panel1.SuspendLayout()
    Me.SplitDARF_4.Panel2.SuspendLayout()
    Me.SplitDARF_4.SuspendLayout()
    Me.panDARF_4.SuspendLayout()
    CType(Me.dbgREF_4, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.SplitBasis, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitBasis.Panel1.SuspendLayout()
    Me.SplitBasis.Panel2.SuspendLayout()
    Me.SplitBasis.SuspendLayout()
    Me.SuspendLayout()
    '
    'TabDARF
    '
    Me.TabDARF.Appearance = System.Windows.Forms.TabAppearance.Buttons
    Me.TabDARF.Controls.Add(Me.TabPage0)
    Me.TabDARF.Controls.Add(Me.TabPage1)
    Me.TabDARF.Controls.Add(Me.TabPage2)
    Me.TabDARF.Controls.Add(Me.TabPage3)
    Me.TabDARF.Controls.Add(Me.TabPage4)
    Me.TabDARF.Dock = System.Windows.Forms.DockStyle.Fill
    Me.TabDARF.ItemSize = New System.Drawing.Size(150, 21)
    Me.TabDARF.Location = New System.Drawing.Point(0, 0)
    Me.TabDARF.Margin = New System.Windows.Forms.Padding(0)
    Me.TabDARF.Name = "TabDARF"
    Me.TabDARF.Padding = New System.Drawing.Point(0, 0)
    Me.TabDARF.SelectedIndex = 0
    Me.TabDARF.Size = New System.Drawing.Size(900, 554)
    Me.TabDARF.SizeMode = System.Windows.Forms.TabSizeMode.Fixed
    Me.TabDARF.TabIndex = 0
    '
    'TabPage0
    '
    Me.TabPage0.BackColor = System.Drawing.SystemColors.Control
    Me.TabPage0.Controls.Add(Me.SplitDARF_0)
    Me.TabPage0.Location = New System.Drawing.Point(4, 25)
    Me.TabPage0.Margin = New System.Windows.Forms.Padding(0)
    Me.TabPage0.Name = "TabPage0"
    Me.TabPage0.Padding = New System.Windows.Forms.Padding(3)
    Me.TabPage0.Size = New System.Drawing.Size(892, 525)
    Me.TabPage0.TabIndex = 0
    Me.TabPage0.Text = "3680"
    '
    'SplitDARF_0
    '
    Me.SplitDARF_0.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitDARF_0.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitDARF_0.Location = New System.Drawing.Point(3, 3)
    Me.SplitDARF_0.Name = "SplitDARF_0"
    '
    'SplitDARF_0.Panel1
    '
    Me.SplitDARF_0.Panel1.Controls.Add(Me.panDARF_0)
    '
    'SplitDARF_0.Panel2
    '
    Me.SplitDARF_0.Panel2.Controls.Add(Me.dbgREF_0)
    Me.SplitDARF_0.Size = New System.Drawing.Size(886, 519)
    Me.SplitDARF_0.SplitterDistance = 592
    Me.SplitDARF_0.TabIndex = 0
    '
    'panDARF_0
    '
    Me.panDARF_0.Controls.Add(Me.lblHinweis)
    Me.panDARF_0.Controls.Add(Me.txtSQL_0)
    Me.panDARF_0.Controls.Add(Me.btnRUN_0)
    Me.panDARF_0.Controls.Add(Me.lblSQL_0)
    Me.panDARF_0.Controls.Add(Me.btnMARK_0)
    Me.panDARF_0.Controls.Add(Me.lblGRP_0)
    Me.panDARF_0.Controls.Add(Me.txtANZ_0)
    Me.panDARF_0.Controls.Add(Me.cboGRP_0)
    Me.panDARF_0.Controls.Add(Me.lblANZ_0)
    Me.panDARF_0.Controls.Add(Me.chkDAT_0)
    Me.panDARF_0.Controls.Add(Me.btnZEIG_0)
    Me.panDARF_0.Controls.Add(Me.lblVON_0)
    Me.panDARF_0.Controls.Add(Me.txtBIS_0)
    Me.panDARF_0.Controls.Add(Me.txtVON_0)
    Me.panDARF_0.Controls.Add(Me.lblBIS_0)
    Me.panDARF_0.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panDARF_0.Location = New System.Drawing.Point(0, 0)
    Me.panDARF_0.Margin = New System.Windows.Forms.Padding(2)
    Me.panDARF_0.Name = "panDARF_0"
    Me.panDARF_0.Size = New System.Drawing.Size(592, 519)
    Me.panDARF_0.TabIndex = 1
    '
    'lblHinweis
    '
    Me.lblHinweis.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.lblHinweis.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblHinweis.Location = New System.Drawing.Point(84, 452)
    Me.lblHinweis.Name = "lblHinweis"
    Me.lblHinweis.Size = New System.Drawing.Size(420, 41)
    Me.lblHinweis.TabIndex = 14
    Me.lblHinweis.Text = "3566"
    '
    'txtSQL_0
    '
    Me.txtSQL_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtSQL_0.Location = New System.Drawing.Point(192, 34)
    Me.txtSQL_0.Name = "txtSQL_0"
    Me.txtSQL_0.Size = New System.Drawing.Size(312, 20)
    Me.txtSQL_0.TabIndex = 1
    '
    'btnRUN_0
    '
    Me.btnRUN_0.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnRUN_0.Enabled = False
    Me.btnRUN_0.Location = New System.Drawing.Point(233, 395)
    Me.btnRUN_0.Name = "btnRUN_0"
    Me.btnRUN_0.Size = New System.Drawing.Size(140, 30)
    Me.btnRUN_0.TabIndex = 13
    Me.btnRUN_0.Text = "3691"
    Me.btnRUN_0.UseVisualStyleBackColor = True
    '
    'lblSQL_0
    '
    Me.lblSQL_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSQL_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblSQL_0.Location = New System.Drawing.Point(84, 30)
    Me.lblSQL_0.Name = "lblSQL_0"
    Me.lblSQL_0.Size = New System.Drawing.Size(108, 21)
    Me.lblSQL_0.TabIndex = 0
    Me.lblSQL_0.Text = "369"
    Me.lblSQL_0.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'btnMARK_0
    '
    Me.btnMARK_0.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnMARK_0.Enabled = False
    Me.btnMARK_0.Location = New System.Drawing.Point(233, 338)
    Me.btnMARK_0.Name = "btnMARK_0"
    Me.btnMARK_0.Size = New System.Drawing.Size(140, 30)
    Me.btnMARK_0.TabIndex = 12
    Me.btnMARK_0.Text = "3670"
    Me.btnMARK_0.UseVisualStyleBackColor = True
    '
    'lblGRP_0
    '
    Me.lblGRP_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRP_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRP_0.Location = New System.Drawing.Point(84, 73)
    Me.lblGRP_0.Name = "lblGRP_0"
    Me.lblGRP_0.Size = New System.Drawing.Size(108, 21)
    Me.lblGRP_0.TabIndex = 2
    Me.lblGRP_0.Text = "386"
    Me.lblGRP_0.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtANZ_0
    '
    Me.txtANZ_0.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.txtANZ_0.Location = New System.Drawing.Point(275, 268)
    Me.txtANZ_0.Name = "txtANZ_0"
    Me.txtANZ_0.Size = New System.Drawing.Size(99, 20)
    Me.txtANZ_0.TabIndex = 11
    '
    'cboGRP_0
    '
    Me.cboGRP_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboGRP_0.FormattingEnabled = True
    Me.cboGRP_0.Location = New System.Drawing.Point(192, 77)
    Me.cboGRP_0.Name = "cboGRP_0"
    Me.cboGRP_0.Size = New System.Drawing.Size(312, 21)
    Me.cboGRP_0.TabIndex = 3
    '
    'lblANZ_0
    '
    Me.lblANZ_0.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.lblANZ_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblANZ_0.Location = New System.Drawing.Point(170, 265)
    Me.lblANZ_0.Name = "lblANZ_0"
    Me.lblANZ_0.Size = New System.Drawing.Size(108, 21)
    Me.lblANZ_0.TabIndex = 10
    Me.lblANZ_0.Text = "3689"
    Me.lblANZ_0.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'chkDAT_0
    '
    Me.chkDAT_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDAT_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkDAT_0.Location = New System.Drawing.Point(151, 117)
    Me.chkDAT_0.Name = "chkDAT_0"
    Me.chkDAT_0.Size = New System.Drawing.Size(76, 20)
    Me.chkDAT_0.TabIndex = 4
    Me.chkDAT_0.Text = "375"
    Me.chkDAT_0.UseVisualStyleBackColor = False
    '
    'btnZEIG_0
    '
    Me.btnZEIG_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZEIG_0.Location = New System.Drawing.Point(234, 167)
    Me.btnZEIG_0.Name = "btnZEIG_0"
    Me.btnZEIG_0.Size = New System.Drawing.Size(140, 30)
    Me.btnZEIG_0.TabIndex = 9
    Me.btnZEIG_0.Text = "3690"
    Me.btnZEIG_0.UseVisualStyleBackColor = True
    '
    'lblVON_0
    '
    Me.lblVON_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblVON_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblVON_0.Location = New System.Drawing.Point(220, 117)
    Me.lblVON_0.Name = "lblVON_0"
    Me.lblVON_0.Size = New System.Drawing.Size(41, 21)
    Me.lblVON_0.TabIndex = 5
    Me.lblVON_0.Text = "376"
    Me.lblVON_0.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblVON_0.Visible = False
    '
    'txtBIS_0
    '
    Me.txtBIS_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtBIS_0.Location = New System.Drawing.Point(388, 117)
    Me.txtBIS_0.Name = "txtBIS_0"
    Me.txtBIS_0.Size = New System.Drawing.Size(81, 20)
    Me.txtBIS_0.TabIndex = 8
    Me.txtBIS_0.Text = "31.12.2011"
    Me.txtBIS_0.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtBIS_0.Visible = False
    '
    'txtVON_0
    '
    Me.txtVON_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtVON_0.Location = New System.Drawing.Point(262, 117)
    Me.txtVON_0.Name = "txtVON_0"
    Me.txtVON_0.Size = New System.Drawing.Size(81, 20)
    Me.txtVON_0.TabIndex = 6
    Me.txtVON_0.Text = "01.01.2000"
    Me.txtVON_0.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtVON_0.Visible = False
    '
    'lblBIS_0
    '
    Me.lblBIS_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblBIS_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblBIS_0.Location = New System.Drawing.Point(346, 117)
    Me.lblBIS_0.Name = "lblBIS_0"
    Me.lblBIS_0.Size = New System.Drawing.Size(41, 21)
    Me.lblBIS_0.TabIndex = 7
    Me.lblBIS_0.Text = "377"
    Me.lblBIS_0.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblBIS_0.Visible = False
    '
    'dbgREF_0
    '
    Me.dbgREF_0.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgREF_0.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgREF_0.Location = New System.Drawing.Point(0, 0)
    Me.dbgREF_0.Name = "dbgREF_0"
    Me.dbgREF_0.RowTemplate.Height = 24
    Me.dbgREF_0.Size = New System.Drawing.Size(290, 519)
    Me.dbgREF_0.TabIndex = 0
    '
    'TabPage1
    '
    Me.TabPage1.BackColor = System.Drawing.SystemColors.Control
    Me.TabPage1.Controls.Add(Me.SplitDARF_1)
    Me.TabPage1.Location = New System.Drawing.Point(4, 25)
    Me.TabPage1.Name = "TabPage1"
    Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
    Me.TabPage1.Size = New System.Drawing.Size(892, 534)
    Me.TabPage1.TabIndex = 1
    Me.TabPage1.Text = "3681"
    '
    'SplitDARF_1
    '
    Me.SplitDARF_1.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitDARF_1.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitDARF_1.Location = New System.Drawing.Point(3, 3)
    Me.SplitDARF_1.Name = "SplitDARF_1"
    '
    'SplitDARF_1.Panel1
    '
    Me.SplitDARF_1.Panel1.Controls.Add(Me.panDARF_1)
    '
    'SplitDARF_1.Panel2
    '
    Me.SplitDARF_1.Panel2.Controls.Add(Me.dbgREF_1)
    Me.SplitDARF_1.Size = New System.Drawing.Size(886, 528)
    Me.SplitDARF_1.SplitterDistance = 640
    Me.SplitDARF_1.TabIndex = 1
    '
    'panDARF_1
    '
    Me.panDARF_1.Controls.Add(Me.cboGRPN)
    Me.panDARF_1.Controls.Add(Me.chkGRPN)
    Me.panDARF_1.Controls.Add(Me.lblGRPN)
    Me.panDARF_1.Controls.Add(Me.chkANEU)
    Me.panDARF_1.Controls.Add(Me.lblSQL_1)
    Me.panDARF_1.Controls.Add(Me.chkARC_1)
    Me.panDARF_1.Controls.Add(Me.txtSQL_1)
    Me.panDARF_1.Controls.Add(Me.chkARCN)
    Me.panDARF_1.Controls.Add(Me.lblGRP_1)
    Me.panDARF_1.Controls.Add(Me.txtKENN_1)
    Me.panDARF_1.Controls.Add(Me.cboGRP_1)
    Me.panDARF_1.Controls.Add(Me.lblKENN_1)
    Me.panDARF_1.Controls.Add(Me.chkDAT_1)
    Me.panDARF_1.Controls.Add(Me.chkDUP)
    Me.panDARF_1.Controls.Add(Me.lblVON_1)
    Me.panDARF_1.Controls.Add(Me.btnRUN_1)
    Me.panDARF_1.Controls.Add(Me.txtVON_1)
    Me.panDARF_1.Controls.Add(Me.btnMARK_1)
    Me.panDARF_1.Controls.Add(Me.lblBIS_1)
    Me.panDARF_1.Controls.Add(Me.txtANZ_1)
    Me.panDARF_1.Controls.Add(Me.txtBIS_1)
    Me.panDARF_1.Controls.Add(Me.lblANZ_1)
    Me.panDARF_1.Controls.Add(Me.btnZEIG_1)
    Me.panDARF_1.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panDARF_1.Location = New System.Drawing.Point(0, 0)
    Me.panDARF_1.Margin = New System.Windows.Forms.Padding(2)
    Me.panDARF_1.Name = "panDARF_1"
    Me.panDARF_1.Size = New System.Drawing.Size(640, 528)
    Me.panDARF_1.TabIndex = 1
    '
    'cboGRPN
    '
    Me.cboGRPN.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboGRPN.FormattingEnabled = True
    Me.cboGRPN.Location = New System.Drawing.Point(234, 340)
    Me.cboGRPN.Name = "cboGRPN"
    Me.cboGRPN.Size = New System.Drawing.Size(312, 21)
    Me.cboGRPN.TabIndex = 21
    Me.cboGRPN.Visible = False
    '
    'chkGRPN
    '
    Me.chkGRPN.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkGRPN.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkGRPN.Location = New System.Drawing.Point(85, 340)
    Me.chkGRPN.Name = "chkGRPN"
    Me.chkGRPN.Size = New System.Drawing.Size(147, 20)
    Me.chkGRPN.TabIndex = 20
    Me.chkGRPN.Text = "3661"
    Me.chkGRPN.UseVisualStyleBackColor = False
    '
    'lblGRPN
    '
    Me.lblGRPN.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRPN.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRPN.Location = New System.Drawing.Point(535, 343)
    Me.lblGRPN.Name = "lblGRPN"
    Me.lblGRPN.Size = New System.Drawing.Size(10, 13)
    Me.lblGRPN.TabIndex = 22
    Me.lblGRPN.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'chkANEU
    '
    Me.chkANEU.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkANEU.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkANEU.Location = New System.Drawing.Point(234, 314)
    Me.chkANEU.Name = "chkANEU"
    Me.chkANEU.Size = New System.Drawing.Size(66, 20)
    Me.chkANEU.TabIndex = 19
    Me.chkANEU.Text = "374"
    Me.chkANEU.UseVisualStyleBackColor = False
    Me.chkANEU.Visible = False
    '
    'lblSQL_1
    '
    Me.lblSQL_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSQL_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblSQL_1.Location = New System.Drawing.Point(82, 34)
    Me.lblSQL_1.Name = "lblSQL_1"
    Me.lblSQL_1.Size = New System.Drawing.Size(108, 21)
    Me.lblSQL_1.TabIndex = 0
    Me.lblSQL_1.Text = "369"
    Me.lblSQL_1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'chkARC_1
    '
    Me.chkARC_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkARC_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkARC_1.Location = New System.Drawing.Point(88, 11)
    Me.chkARC_1.Name = "chkARC_1"
    Me.chkARC_1.Size = New System.Drawing.Size(66, 20)
    Me.chkARC_1.TabIndex = 18
    Me.chkARC_1.Text = "374"
    Me.chkARC_1.UseVisualStyleBackColor = False
    '
    'txtSQL_1
    '
    Me.txtSQL_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtSQL_1.Location = New System.Drawing.Point(189, 35)
    Me.txtSQL_1.Name = "txtSQL_1"
    Me.txtSQL_1.Size = New System.Drawing.Size(356, 20)
    Me.txtSQL_1.TabIndex = 1
    '
    'chkARCN
    '
    Me.chkARCN.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkARCN.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkARCN.Location = New System.Drawing.Point(85, 314)
    Me.chkARCN.Name = "chkARCN"
    Me.chkARCN.Size = New System.Drawing.Size(147, 20)
    Me.chkARCN.TabIndex = 17
    Me.chkARCN.Text = "3660"
    Me.chkARCN.UseVisualStyleBackColor = False
    '
    'lblGRP_1
    '
    Me.lblGRP_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRP_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRP_1.Location = New System.Drawing.Point(82, 73)
    Me.lblGRP_1.Name = "lblGRP_1"
    Me.lblGRP_1.Size = New System.Drawing.Size(108, 21)
    Me.lblGRP_1.TabIndex = 2
    Me.lblGRP_1.Text = "386"
    Me.lblGRP_1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtKENN_1
    '
    Me.txtKENN_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtKENN_1.Location = New System.Drawing.Point(303, 274)
    Me.txtKENN_1.Name = "txtKENN_1"
    Me.txtKENN_1.ReadOnly = True
    Me.txtKENN_1.Size = New System.Drawing.Size(25, 20)
    Me.txtKENN_1.TabIndex = 16
    '
    'cboGRP_1
    '
    Me.cboGRP_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboGRP_1.FormattingEnabled = True
    Me.cboGRP_1.Location = New System.Drawing.Point(190, 74)
    Me.cboGRP_1.Name = "cboGRP_1"
    Me.cboGRP_1.Size = New System.Drawing.Size(356, 21)
    Me.cboGRP_1.TabIndex = 3
    '
    'lblKENN_1
    '
    Me.lblKENN_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblKENN_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblKENN_1.Location = New System.Drawing.Point(85, 271)
    Me.lblKENN_1.Name = "lblKENN_1"
    Me.lblKENN_1.Size = New System.Drawing.Size(217, 21)
    Me.lblKENN_1.TabIndex = 15
    Me.lblKENN_1.Text = "281"
    Me.lblKENN_1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'chkDAT_1
    '
    Me.chkDAT_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDAT_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkDAT_1.Location = New System.Drawing.Point(225, 109)
    Me.chkDAT_1.Name = "chkDAT_1"
    Me.chkDAT_1.Size = New System.Drawing.Size(81, 20)
    Me.chkDAT_1.TabIndex = 4
    Me.chkDAT_1.Text = "375"
    Me.chkDAT_1.UseVisualStyleBackColor = False
    '
    'chkDUP
    '
    Me.chkDUP.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDUP.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkDUP.Location = New System.Drawing.Point(225, 144)
    Me.chkDUP.Name = "chkDUP"
    Me.chkDUP.Size = New System.Drawing.Size(198, 19)
    Me.chkDUP.TabIndex = 14
    Me.chkDUP.Text = "3652"
    Me.chkDUP.UseVisualStyleBackColor = False
    '
    'lblVON_1
    '
    Me.lblVON_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblVON_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblVON_1.Location = New System.Drawing.Point(298, 109)
    Me.lblVON_1.Name = "lblVON_1"
    Me.lblVON_1.Size = New System.Drawing.Size(41, 21)
    Me.lblVON_1.TabIndex = 5
    Me.lblVON_1.Text = "376"
    Me.lblVON_1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblVON_1.Visible = False
    '
    'btnRUN_1
    '
    Me.btnRUN_1.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnRUN_1.Enabled = False
    Me.btnRUN_1.Location = New System.Drawing.Point(261, 477)
    Me.btnRUN_1.Name = "btnRUN_1"
    Me.btnRUN_1.Size = New System.Drawing.Size(140, 30)
    Me.btnRUN_1.TabIndex = 13
    Me.btnRUN_1.Text = "3692"
    Me.btnRUN_1.UseVisualStyleBackColor = True
    '
    'txtVON_1
    '
    Me.txtVON_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtVON_1.Location = New System.Drawing.Point(340, 109)
    Me.txtVON_1.Name = "txtVON_1"
    Me.txtVON_1.Size = New System.Drawing.Size(81, 20)
    Me.txtVON_1.TabIndex = 6
    Me.txtVON_1.Text = "01.01.2000"
    Me.txtVON_1.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtVON_1.Visible = False
    '
    'btnMARK_1
    '
    Me.btnMARK_1.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnMARK_1.Enabled = False
    Me.btnMARK_1.Location = New System.Drawing.Point(261, 412)
    Me.btnMARK_1.Name = "btnMARK_1"
    Me.btnMARK_1.Size = New System.Drawing.Size(140, 30)
    Me.btnMARK_1.TabIndex = 12
    Me.btnMARK_1.Text = "3670"
    Me.btnMARK_1.UseVisualStyleBackColor = True
    '
    'lblBIS_1
    '
    Me.lblBIS_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblBIS_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblBIS_1.Location = New System.Drawing.Point(423, 109)
    Me.lblBIS_1.Name = "lblBIS_1"
    Me.lblBIS_1.Size = New System.Drawing.Size(41, 21)
    Me.lblBIS_1.TabIndex = 7
    Me.lblBIS_1.Text = "377"
    Me.lblBIS_1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblBIS_1.Visible = False
    '
    'txtANZ_1
    '
    Me.txtANZ_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtANZ_1.Location = New System.Drawing.Point(303, 231)
    Me.txtANZ_1.Name = "txtANZ_1"
    Me.txtANZ_1.Size = New System.Drawing.Size(72, 20)
    Me.txtANZ_1.TabIndex = 11
    '
    'txtBIS_1
    '
    Me.txtBIS_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtBIS_1.Location = New System.Drawing.Point(465, 109)
    Me.txtBIS_1.Name = "txtBIS_1"
    Me.txtBIS_1.Size = New System.Drawing.Size(81, 20)
    Me.txtBIS_1.TabIndex = 8
    Me.txtBIS_1.Text = "31.12.2011"
    Me.txtBIS_1.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtBIS_1.Visible = False
    '
    'lblANZ_1
    '
    Me.lblANZ_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblANZ_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblANZ_1.Location = New System.Drawing.Point(193, 228)
    Me.lblANZ_1.Name = "lblANZ_1"
    Me.lblANZ_1.Size = New System.Drawing.Size(108, 21)
    Me.lblANZ_1.TabIndex = 10
    Me.lblANZ_1.Text = "3689"
    Me.lblANZ_1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'btnZEIG_1
    '
    Me.btnZEIG_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZEIG_1.Location = New System.Drawing.Point(321, 182)
    Me.btnZEIG_1.Name = "btnZEIG_1"
    Me.btnZEIG_1.Size = New System.Drawing.Size(140, 30)
    Me.btnZEIG_1.TabIndex = 9
    Me.btnZEIG_1.Text = "3690"
    Me.btnZEIG_1.UseVisualStyleBackColor = True
    '
    'dbgREF_1
    '
    Me.dbgREF_1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgREF_1.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgREF_1.Location = New System.Drawing.Point(0, 0)
    Me.dbgREF_1.Name = "dbgREF_1"
    Me.dbgREF_1.RowTemplate.Height = 24
    Me.dbgREF_1.Size = New System.Drawing.Size(242, 528)
    Me.dbgREF_1.TabIndex = 0
    '
    'TabPage2
    '
    Me.TabPage2.BackColor = System.Drawing.SystemColors.Control
    Me.TabPage2.Controls.Add(Me.SplitDARF_2)
    Me.TabPage2.Location = New System.Drawing.Point(4, 25)
    Me.TabPage2.Name = "TabPage2"
    Me.TabPage2.Size = New System.Drawing.Size(892, 534)
    Me.TabPage2.TabIndex = 2
    Me.TabPage2.Text = "3682"
    '
    'SplitDARF_2
    '
    Me.SplitDARF_2.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitDARF_2.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitDARF_2.Location = New System.Drawing.Point(0, 0)
    Me.SplitDARF_2.Name = "SplitDARF_2"
    '
    'SplitDARF_2.Panel1
    '
    Me.SplitDARF_2.Panel1.Controls.Add(Me.panDARF_2)
    '
    'SplitDARF_2.Panel2
    '
    Me.SplitDARF_2.Panel2.Controls.Add(Me.dbgREF_2)
    Me.SplitDARF_2.Panel2.Controls.Add(Me.grdWRT_2)
    Me.SplitDARF_2.Size = New System.Drawing.Size(892, 534)
    Me.SplitDARF_2.SplitterDistance = 638
    Me.SplitDARF_2.TabIndex = 2
    '
    'panDARF_2
    '
    Me.panDARF_2.Controls.Add(Me.btnCLIP_2)
    Me.panDARF_2.Controls.Add(Me.txtBIS_2)
    Me.panDARF_2.Controls.Add(Me.chkARC_2)
    Me.panDARF_2.Controls.Add(Me.lblSQL_2)
    Me.panDARF_2.Controls.Add(Me.btnRUN_2)
    Me.panDARF_2.Controls.Add(Me.txtSQL_2)
    Me.panDARF_2.Controls.Add(Me.btnMARK_2)
    Me.panDARF_2.Controls.Add(Me.lblGRP_2)
    Me.panDARF_2.Controls.Add(Me.txtANZ_2)
    Me.panDARF_2.Controls.Add(Me.cboGRP_2)
    Me.panDARF_2.Controls.Add(Me.lblANZ_2)
    Me.panDARF_2.Controls.Add(Me.chkDAT_2)
    Me.panDARF_2.Controls.Add(Me.btnZEIG_2)
    Me.panDARF_2.Controls.Add(Me.lblVON_2)
    Me.panDARF_2.Controls.Add(Me.txtVON_2)
    Me.panDARF_2.Controls.Add(Me.lblBIS_2)
    Me.panDARF_2.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panDARF_2.Location = New System.Drawing.Point(0, 0)
    Me.panDARF_2.Margin = New System.Windows.Forms.Padding(2)
    Me.panDARF_2.Name = "panDARF_2"
    Me.panDARF_2.Size = New System.Drawing.Size(638, 534)
    Me.panDARF_2.TabIndex = 2
    '
    'btnCLIP_2
    '
    Me.btnCLIP_2.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnCLIP_2.Enabled = False
    Me.btnCLIP_2.Location = New System.Drawing.Point(229, 498)
    Me.btnCLIP_2.Name = "btnCLIP_2"
    Me.btnCLIP_2.Size = New System.Drawing.Size(219, 30)
    Me.btnCLIP_2.TabIndex = 19
    Me.btnCLIP_2.Text = "3662"
    Me.btnCLIP_2.UseVisualStyleBackColor = True
    '
    'txtBIS_2
    '
    Me.txtBIS_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtBIS_2.Location = New System.Drawing.Point(475, 151)
    Me.txtBIS_2.Name = "txtBIS_2"
    Me.txtBIS_2.Size = New System.Drawing.Size(81, 20)
    Me.txtBIS_2.TabIndex = 8
    Me.txtBIS_2.Text = "31.12.2011"
    Me.txtBIS_2.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtBIS_2.Visible = False
    '
    'chkARC_2
    '
    Me.chkARC_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkARC_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkARC_2.Location = New System.Drawing.Point(99, 13)
    Me.chkARC_2.Name = "chkARC_2"
    Me.chkARC_2.Size = New System.Drawing.Size(66, 20)
    Me.chkARC_2.TabIndex = 18
    Me.chkARC_2.Text = "374"
    Me.chkARC_2.UseVisualStyleBackColor = False
    '
    'lblSQL_2
    '
    Me.lblSQL_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSQL_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblSQL_2.Location = New System.Drawing.Point(94, 47)
    Me.lblSQL_2.Name = "lblSQL_2"
    Me.lblSQL_2.Size = New System.Drawing.Size(108, 21)
    Me.lblSQL_2.TabIndex = 0
    Me.lblSQL_2.Text = "369"
    Me.lblSQL_2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'btnRUN_2
    '
    Me.btnRUN_2.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnRUN_2.Enabled = False
    Me.btnRUN_2.Location = New System.Drawing.Point(269, 449)
    Me.btnRUN_2.Name = "btnRUN_2"
    Me.btnRUN_2.Size = New System.Drawing.Size(140, 30)
    Me.btnRUN_2.TabIndex = 13
    Me.btnRUN_2.Text = "3682"
    Me.btnRUN_2.UseVisualStyleBackColor = True
    '
    'txtSQL_2
    '
    Me.txtSQL_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtSQL_2.Location = New System.Drawing.Point(201, 47)
    Me.txtSQL_2.Name = "txtSQL_2"
    Me.txtSQL_2.Size = New System.Drawing.Size(356, 20)
    Me.txtSQL_2.TabIndex = 1
    '
    'btnMARK_2
    '
    Me.btnMARK_2.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnMARK_2.Enabled = False
    Me.btnMARK_2.Location = New System.Drawing.Point(270, 392)
    Me.btnMARK_2.Name = "btnMARK_2"
    Me.btnMARK_2.Size = New System.Drawing.Size(140, 30)
    Me.btnMARK_2.TabIndex = 12
    Me.btnMARK_2.Text = "3670"
    Me.btnMARK_2.UseVisualStyleBackColor = True
    '
    'lblGRP_2
    '
    Me.lblGRP_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRP_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRP_2.Location = New System.Drawing.Point(94, 95)
    Me.lblGRP_2.Name = "lblGRP_2"
    Me.lblGRP_2.Size = New System.Drawing.Size(108, 21)
    Me.lblGRP_2.TabIndex = 2
    Me.lblGRP_2.Text = "386"
    Me.lblGRP_2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtANZ_2
    '
    Me.txtANZ_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtANZ_2.Location = New System.Drawing.Point(310, 259)
    Me.txtANZ_2.Name = "txtANZ_2"
    Me.txtANZ_2.Size = New System.Drawing.Size(99, 20)
    Me.txtANZ_2.TabIndex = 11
    '
    'cboGRP_2
    '
    Me.cboGRP_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboGRP_2.FormattingEnabled = True
    Me.cboGRP_2.Location = New System.Drawing.Point(201, 95)
    Me.cboGRP_2.Name = "cboGRP_2"
    Me.cboGRP_2.Size = New System.Drawing.Size(356, 21)
    Me.cboGRP_2.TabIndex = 3
    '
    'lblANZ_2
    '
    Me.lblANZ_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblANZ_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblANZ_2.Location = New System.Drawing.Point(207, 255)
    Me.lblANZ_2.Name = "lblANZ_2"
    Me.lblANZ_2.Size = New System.Drawing.Size(108, 21)
    Me.lblANZ_2.TabIndex = 10
    Me.lblANZ_2.Text = "3689"
    Me.lblANZ_2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'chkDAT_2
    '
    Me.chkDAT_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDAT_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkDAT_2.Location = New System.Drawing.Point(229, 151)
    Me.chkDAT_2.Name = "chkDAT_2"
    Me.chkDAT_2.Size = New System.Drawing.Size(86, 20)
    Me.chkDAT_2.TabIndex = 4
    Me.chkDAT_2.Text = "375"
    Me.chkDAT_2.UseVisualStyleBackColor = False
    '
    'btnZEIG_2
    '
    Me.btnZEIG_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZEIG_2.Location = New System.Drawing.Point(270, 190)
    Me.btnZEIG_2.Name = "btnZEIG_2"
    Me.btnZEIG_2.Size = New System.Drawing.Size(140, 30)
    Me.btnZEIG_2.TabIndex = 9
    Me.btnZEIG_2.Text = "3690"
    Me.btnZEIG_2.UseVisualStyleBackColor = True
    '
    'lblVON_2
    '
    Me.lblVON_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblVON_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblVON_2.Location = New System.Drawing.Point(309, 151)
    Me.lblVON_2.Name = "lblVON_2"
    Me.lblVON_2.Size = New System.Drawing.Size(41, 21)
    Me.lblVON_2.TabIndex = 5
    Me.lblVON_2.Text = "376"
    Me.lblVON_2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblVON_2.Visible = False
    '
    'txtVON_2
    '
    Me.txtVON_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtVON_2.Location = New System.Drawing.Point(351, 151)
    Me.txtVON_2.Name = "txtVON_2"
    Me.txtVON_2.Size = New System.Drawing.Size(81, 20)
    Me.txtVON_2.TabIndex = 6
    Me.txtVON_2.Text = "01.01.2000"
    Me.txtVON_2.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtVON_2.Visible = False
    '
    'lblBIS_2
    '
    Me.lblBIS_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblBIS_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblBIS_2.Location = New System.Drawing.Point(433, 151)
    Me.lblBIS_2.Name = "lblBIS_2"
    Me.lblBIS_2.Size = New System.Drawing.Size(41, 21)
    Me.lblBIS_2.TabIndex = 7
    Me.lblBIS_2.Text = "377"
    Me.lblBIS_2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblBIS_2.Visible = False
    '
    'dbgREF_2
    '
    Me.dbgREF_2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgREF_2.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgREF_2.Location = New System.Drawing.Point(0, 0)
    Me.dbgREF_2.Name = "dbgREF_2"
    Me.dbgREF_2.RowTemplate.Height = 24
    Me.dbgREF_2.Size = New System.Drawing.Size(250, 534)
    Me.dbgREF_2.TabIndex = 0
    '
    'grdWRT_2
    '
    Me.grdWRT_2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.grdWRT_2.Dock = System.Windows.Forms.DockStyle.Fill
    Me.grdWRT_2.Location = New System.Drawing.Point(0, 0)
    Me.grdWRT_2.Name = "grdWRT_2"
    Me.grdWRT_2.RowTemplate.Height = 24
    Me.grdWRT_2.Size = New System.Drawing.Size(250, 534)
    Me.grdWRT_2.TabIndex = 1
    Me.grdWRT_2.Visible = False
    '
    'TabPage3
    '
    Me.TabPage3.BackColor = System.Drawing.SystemColors.Control
    Me.TabPage3.Controls.Add(Me.SplitDARF_3)
    Me.TabPage3.Location = New System.Drawing.Point(4, 25)
    Me.TabPage3.Name = "TabPage3"
    Me.TabPage3.Size = New System.Drawing.Size(892, 534)
    Me.TabPage3.TabIndex = 3
    Me.TabPage3.Text = "3683"
    '
    'SplitDARF_3
    '
    Me.SplitDARF_3.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitDARF_3.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitDARF_3.Location = New System.Drawing.Point(0, 0)
    Me.SplitDARF_3.Name = "SplitDARF_3"
    '
    'SplitDARF_3.Panel1
    '
    Me.SplitDARF_3.Panel1.Controls.Add(Me.panDARF_3)
    '
    'SplitDARF_3.Panel2
    '
    Me.SplitDARF_3.Panel2.Controls.Add(Me.grdWRT_3)
    Me.SplitDARF_3.Size = New System.Drawing.Size(892, 534)
    Me.SplitDARF_3.SplitterDistance = 648
    Me.SplitDARF_3.TabIndex = 3
    '
    'panDARF_3
    '
    Me.panDARF_3.Controls.Add(Me.txtKENN_3)
    Me.panDARF_3.Controls.Add(Me.btnCLIP_3)
    Me.panDARF_3.Controls.Add(Me.lblKENN_3)
    Me.panDARF_3.Controls.Add(Me.lblGRP_3)
    Me.panDARF_3.Controls.Add(Me.cboGRP_3)
    Me.panDARF_3.Controls.Add(Me.chkARC_3)
    Me.panDARF_3.Controls.Add(Me.btnRUN_3)
    Me.panDARF_3.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panDARF_3.Location = New System.Drawing.Point(0, 0)
    Me.panDARF_3.Margin = New System.Windows.Forms.Padding(2)
    Me.panDARF_3.Name = "panDARF_3"
    Me.panDARF_3.Size = New System.Drawing.Size(648, 534)
    Me.panDARF_3.TabIndex = 2
    '
    'txtKENN_3
    '
    Me.txtKENN_3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtKENN_3.Location = New System.Drawing.Point(403, 199)
    Me.txtKENN_3.Name = "txtKENN_3"
    Me.txtKENN_3.ReadOnly = True
    Me.txtKENN_3.Size = New System.Drawing.Size(25, 20)
    Me.txtKENN_3.TabIndex = 21
    '
    'btnCLIP_3
    '
    Me.btnCLIP_3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnCLIP_3.Location = New System.Drawing.Point(208, 134)
    Me.btnCLIP_3.Name = "btnCLIP_3"
    Me.btnCLIP_3.Size = New System.Drawing.Size(219, 30)
    Me.btnCLIP_3.TabIndex = 19
    Me.btnCLIP_3.Text = "3663"
    Me.btnCLIP_3.UseVisualStyleBackColor = True
    '
    'lblKENN_3
    '
    Me.lblKENN_3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblKENN_3.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblKENN_3.Location = New System.Drawing.Point(208, 198)
    Me.lblKENN_3.Name = "lblKENN_3"
    Me.lblKENN_3.Size = New System.Drawing.Size(194, 21)
    Me.lblKENN_3.TabIndex = 20
    Me.lblKENN_3.Text = "281"
    Me.lblKENN_3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblGRP_3
    '
    Me.lblGRP_3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRP_3.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRP_3.Location = New System.Drawing.Point(92, 233)
    Me.lblGRP_3.Name = "lblGRP_3"
    Me.lblGRP_3.Size = New System.Drawing.Size(108, 21)
    Me.lblGRP_3.TabIndex = 2
    Me.lblGRP_3.Text = "386"
    Me.lblGRP_3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'cboGRP_3
    '
    Me.cboGRP_3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboGRP_3.FormattingEnabled = True
    Me.cboGRP_3.Location = New System.Drawing.Point(196, 233)
    Me.cboGRP_3.Name = "cboGRP_3"
    Me.cboGRP_3.Size = New System.Drawing.Size(356, 21)
    Me.cboGRP_3.TabIndex = 3
    '
    'chkARC_3
    '
    Me.chkARC_3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkARC_3.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkARC_3.Location = New System.Drawing.Point(95, 198)
    Me.chkARC_3.Name = "chkARC_3"
    Me.chkARC_3.Size = New System.Drawing.Size(66, 20)
    Me.chkARC_3.TabIndex = 18
    Me.chkARC_3.Text = "374"
    Me.chkARC_3.UseVisualStyleBackColor = False
    '
    'btnRUN_3
    '
    Me.btnRUN_3.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnRUN_3.Enabled = False
    Me.btnRUN_3.Location = New System.Drawing.Point(245, 368)
    Me.btnRUN_3.Name = "btnRUN_3"
    Me.btnRUN_3.Size = New System.Drawing.Size(140, 30)
    Me.btnRUN_3.TabIndex = 13
    Me.btnRUN_3.Text = "3683"
    Me.btnRUN_3.UseVisualStyleBackColor = True
    '
    'grdWRT_3
    '
    Me.grdWRT_3.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.grdWRT_3.Dock = System.Windows.Forms.DockStyle.Fill
    Me.grdWRT_3.Location = New System.Drawing.Point(0, 0)
    Me.grdWRT_3.Name = "grdWRT_3"
    Me.grdWRT_3.RowTemplate.Height = 24
    Me.grdWRT_3.Size = New System.Drawing.Size(240, 534)
    Me.grdWRT_3.TabIndex = 1
    Me.grdWRT_3.Visible = False
    '
    'TabPage4
    '
    Me.TabPage4.BackColor = System.Drawing.SystemColors.Control
    Me.TabPage4.Controls.Add(Me.SplitDARF_4)
    Me.TabPage4.Location = New System.Drawing.Point(4, 25)
    Me.TabPage4.Name = "TabPage4"
    Me.TabPage4.Size = New System.Drawing.Size(892, 534)
    Me.TabPage4.TabIndex = 4
    Me.TabPage4.Text = "3685"
    '
    'SplitDARF_4
    '
    Me.SplitDARF_4.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitDARF_4.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitDARF_4.Location = New System.Drawing.Point(0, 0)
    Me.SplitDARF_4.Name = "SplitDARF_4"
    '
    'SplitDARF_4.Panel1
    '
    Me.SplitDARF_4.Panel1.Controls.Add(Me.panDARF_4)
    '
    'SplitDARF_4.Panel2
    '
    Me.SplitDARF_4.Panel2.Controls.Add(Me.dbgREF_4)
    Me.SplitDARF_4.Size = New System.Drawing.Size(892, 534)
    Me.SplitDARF_4.SplitterDistance = 642
    Me.SplitDARF_4.TabIndex = 2
    '
    'panDARF_4
    '
    Me.panDARF_4.Controls.Add(Me.txtANZ_4)
    Me.panDARF_4.Controls.Add(Me.txtSQL_4)
    Me.panDARF_4.Controls.Add(Me.lblANZ_4)
    Me.panDARF_4.Controls.Add(Me.lblSQL_4)
    Me.panDARF_4.Controls.Add(Me.chkARCR)
    Me.panDARF_4.Controls.Add(Me.lblGRP_4)
    Me.panDARF_4.Controls.Add(Me.txtDIK)
    Me.panDARF_4.Controls.Add(Me.cboGRP_4)
    Me.panDARF_4.Controls.Add(Me.lblDIK)
    Me.panDARF_4.Controls.Add(Me.chkDAT_4)
    Me.panDARF_4.Controls.Add(Me.cboGRPR)
    Me.panDARF_4.Controls.Add(Me.lblVON_4)
    Me.panDARF_4.Controls.Add(Me.lblGRPR)
    Me.panDARF_4.Controls.Add(Me.txtVON_4)
    Me.panDARF_4.Controls.Add(Me.cboMSH)
    Me.panDARF_4.Controls.Add(Me.lblBIS_4)
    Me.panDARF_4.Controls.Add(Me.lblMSH)
    Me.panDARF_4.Controls.Add(Me.txtBIS_4)
    Me.panDARF_4.Controls.Add(Me.chkARC_4)
    Me.panDARF_4.Controls.Add(Me.btnZEIG_4)
    Me.panDARF_4.Controls.Add(Me.btnRUN_4)
    Me.panDARF_4.Controls.Add(Me.btnMARK_4)
    Me.panDARF_4.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panDARF_4.Location = New System.Drawing.Point(0, 0)
    Me.panDARF_4.Margin = New System.Windows.Forms.Padding(2)
    Me.panDARF_4.Name = "panDARF_4"
    Me.panDARF_4.Size = New System.Drawing.Size(642, 534)
    Me.panDARF_4.TabIndex = 1
    '
    'txtANZ_4
    '
    Me.txtANZ_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtANZ_4.Location = New System.Drawing.Point(257, 384)
    Me.txtANZ_4.Name = "txtANZ_4"
    Me.txtANZ_4.Size = New System.Drawing.Size(99, 20)
    Me.txtANZ_4.TabIndex = 27
    '
    'txtSQL_4
    '
    Me.txtSQL_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtSQL_4.Location = New System.Drawing.Point(202, 73)
    Me.txtSQL_4.Name = "txtSQL_4"
    Me.txtSQL_4.Size = New System.Drawing.Size(356, 20)
    Me.txtSQL_4.TabIndex = 1
    '
    'lblANZ_4
    '
    Me.lblANZ_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblANZ_4.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblANZ_4.Location = New System.Drawing.Point(152, 384)
    Me.lblANZ_4.Name = "lblANZ_4"
    Me.lblANZ_4.Size = New System.Drawing.Size(108, 21)
    Me.lblANZ_4.TabIndex = 26
    Me.lblANZ_4.Text = "3689"
    Me.lblANZ_4.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblSQL_4
    '
    Me.lblSQL_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSQL_4.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblSQL_4.Location = New System.Drawing.Point(96, 72)
    Me.lblSQL_4.Name = "lblSQL_4"
    Me.lblSQL_4.Size = New System.Drawing.Size(108, 21)
    Me.lblSQL_4.TabIndex = 0
    Me.lblSQL_4.Text = "369"
    Me.lblSQL_4.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'chkARCR
    '
    Me.chkARCR.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkARCR.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkARCR.Location = New System.Drawing.Point(151, 351)
    Me.chkARCR.Name = "chkARCR"
    Me.chkARCR.Size = New System.Drawing.Size(355, 20)
    Me.chkARCR.TabIndex = 25
    Me.chkARCR.Text = "2425"
    Me.chkARCR.UseVisualStyleBackColor = False
    '
    'lblGRP_4
    '
    Me.lblGRP_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRP_4.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRP_4.Location = New System.Drawing.Point(96, 112)
    Me.lblGRP_4.Name = "lblGRP_4"
    Me.lblGRP_4.Size = New System.Drawing.Size(108, 21)
    Me.lblGRP_4.TabIndex = 2
    Me.lblGRP_4.Text = "386"
    Me.lblGRP_4.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtDIK
    '
    Me.txtDIK.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtDIK.Location = New System.Drawing.Point(257, 324)
    Me.txtDIK.Name = "txtDIK"
    Me.txtDIK.Size = New System.Drawing.Size(56, 20)
    Me.txtDIK.TabIndex = 24
    Me.txtDIK.Text = "1"
    Me.txtDIK.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    '
    'cboGRP_4
    '
    Me.cboGRP_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboGRP_4.FormattingEnabled = True
    Me.cboGRP_4.Location = New System.Drawing.Point(202, 112)
    Me.cboGRP_4.Name = "cboGRP_4"
    Me.cboGRP_4.Size = New System.Drawing.Size(356, 21)
    Me.cboGRP_4.TabIndex = 3
    '
    'lblDIK
    '
    Me.lblDIK.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblDIK.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblDIK.Location = New System.Drawing.Point(148, 324)
    Me.lblDIK.Name = "lblDIK"
    Me.lblDIK.Size = New System.Drawing.Size(112, 21)
    Me.lblDIK.TabIndex = 23
    Me.lblDIK.Text = "832"
    Me.lblDIK.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'chkDAT_4
    '
    Me.chkDAT_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDAT_4.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkDAT_4.Location = New System.Drawing.Point(232, 147)
    Me.chkDAT_4.Name = "chkDAT_4"
    Me.chkDAT_4.Size = New System.Drawing.Size(85, 20)
    Me.chkDAT_4.TabIndex = 4
    Me.chkDAT_4.Text = "375"
    Me.chkDAT_4.UseVisualStyleBackColor = False
    '
    'cboGRPR
    '
    Me.cboGRPR.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboGRPR.FormattingEnabled = True
    Me.cboGRPR.Location = New System.Drawing.Point(257, 299)
    Me.cboGRPR.Name = "cboGRPR"
    Me.cboGRPR.Size = New System.Drawing.Size(249, 21)
    Me.cboGRPR.TabIndex = 22
    '
    'lblVON_4
    '
    Me.lblVON_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblVON_4.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblVON_4.Location = New System.Drawing.Point(310, 146)
    Me.lblVON_4.Name = "lblVON_4"
    Me.lblVON_4.Size = New System.Drawing.Size(41, 21)
    Me.lblVON_4.TabIndex = 5
    Me.lblVON_4.Text = "376"
    Me.lblVON_4.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblVON_4.Visible = False
    '
    'lblGRPR
    '
    Me.lblGRPR.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRPR.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRPR.Location = New System.Drawing.Point(148, 299)
    Me.lblGRPR.Name = "lblGRPR"
    Me.lblGRPR.Size = New System.Drawing.Size(112, 21)
    Me.lblGRPR.TabIndex = 21
    Me.lblGRPR.Text = "3669"
    Me.lblGRPR.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtVON_4
    '
    Me.txtVON_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtVON_4.Location = New System.Drawing.Point(352, 146)
    Me.txtVON_4.Name = "txtVON_4"
    Me.txtVON_4.Size = New System.Drawing.Size(81, 20)
    Me.txtVON_4.TabIndex = 6
    Me.txtVON_4.Text = "01.01.2000"
    Me.txtVON_4.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtVON_4.Visible = False
    '
    'cboMSH
    '
    Me.cboMSH.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMSH.FormattingEnabled = True
    Me.cboMSH.Location = New System.Drawing.Point(151, 263)
    Me.cboMSH.Name = "cboMSH"
    Me.cboMSH.Size = New System.Drawing.Size(355, 21)
    Me.cboMSH.TabIndex = 20
    '
    'lblBIS_4
    '
    Me.lblBIS_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblBIS_4.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblBIS_4.Location = New System.Drawing.Point(434, 146)
    Me.lblBIS_4.Name = "lblBIS_4"
    Me.lblBIS_4.Size = New System.Drawing.Size(41, 21)
    Me.lblBIS_4.TabIndex = 7
    Me.lblBIS_4.Text = "377"
    Me.lblBIS_4.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblBIS_4.Visible = False
    '
    'lblMSH
    '
    Me.lblMSH.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMSH.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMSH.Location = New System.Drawing.Point(148, 238)
    Me.lblMSH.Name = "lblMSH"
    Me.lblMSH.Size = New System.Drawing.Size(358, 21)
    Me.lblMSH.TabIndex = 19
    Me.lblMSH.Text = "96"
    Me.lblMSH.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'txtBIS_4
    '
    Me.txtBIS_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtBIS_4.Location = New System.Drawing.Point(475, 146)
    Me.txtBIS_4.Name = "txtBIS_4"
    Me.txtBIS_4.Size = New System.Drawing.Size(81, 20)
    Me.txtBIS_4.TabIndex = 8
    Me.txtBIS_4.Text = "31.12.2011"
    Me.txtBIS_4.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtBIS_4.Visible = False
    '
    'chkARC_4
    '
    Me.chkARC_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkARC_4.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkARC_4.Location = New System.Drawing.Point(100, 39)
    Me.chkARC_4.Name = "chkARC_4"
    Me.chkARC_4.Size = New System.Drawing.Size(66, 20)
    Me.chkARC_4.TabIndex = 18
    Me.chkARC_4.Text = "374"
    Me.chkARC_4.UseVisualStyleBackColor = False
    '
    'btnZEIG_4
    '
    Me.btnZEIG_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZEIG_4.Location = New System.Drawing.Point(269, 186)
    Me.btnZEIG_4.Name = "btnZEIG_4"
    Me.btnZEIG_4.Size = New System.Drawing.Size(140, 30)
    Me.btnZEIG_4.TabIndex = 9
    Me.btnZEIG_4.Text = "3690"
    Me.btnZEIG_4.UseVisualStyleBackColor = True
    '
    'btnRUN_4
    '
    Me.btnRUN_4.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnRUN_4.Enabled = False
    Me.btnRUN_4.Location = New System.Drawing.Point(269, 485)
    Me.btnRUN_4.Name = "btnRUN_4"
    Me.btnRUN_4.Size = New System.Drawing.Size(140, 30)
    Me.btnRUN_4.TabIndex = 13
    Me.btnRUN_4.Text = "3693"
    Me.btnRUN_4.UseVisualStyleBackColor = True
    '
    'btnMARK_4
    '
    Me.btnMARK_4.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnMARK_4.Enabled = False
    Me.btnMARK_4.Location = New System.Drawing.Point(269, 433)
    Me.btnMARK_4.Name = "btnMARK_4"
    Me.btnMARK_4.Size = New System.Drawing.Size(140, 30)
    Me.btnMARK_4.TabIndex = 12
    Me.btnMARK_4.Text = "3670"
    Me.btnMARK_4.UseVisualStyleBackColor = True
    '
    'dbgREF_4
    '
    Me.dbgREF_4.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgREF_4.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgREF_4.Location = New System.Drawing.Point(0, 0)
    Me.dbgREF_4.Name = "dbgREF_4"
    Me.dbgREF_4.RowTemplate.Height = 24
    Me.dbgREF_4.Size = New System.Drawing.Size(246, 534)
    Me.dbgREF_4.TabIndex = 0
    '
    'lblMESS
    '
    Me.lblMESS.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMESS.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMESS.Location = New System.Drawing.Point(424, 27)
    Me.lblMESS.Name = "lblMESS"
    Me.lblMESS.Size = New System.Drawing.Size(149, 21)
    Me.lblMESS.TabIndex = 29
    Me.lblMESS.Text = "204"
    Me.lblMESS.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'cboMESS
    '
    Me.cboMESS.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMESS.Enabled = False
    Me.cboMESS.FormattingEnabled = True
    Me.cboMESS.Location = New System.Drawing.Point(573, 27)
    Me.cboMESS.Name = "cboMESS"
    Me.cboMESS.Size = New System.Drawing.Size(315, 21)
    Me.cboMESS.TabIndex = 28
    '
    'SplitBasis
    '
    Me.SplitBasis.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitBasis.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitBasis.Location = New System.Drawing.Point(0, 0)
    Me.SplitBasis.Name = "SplitBasis"
    Me.SplitBasis.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'SplitBasis.Panel1
    '
    Me.SplitBasis.Panel1.Controls.Add(Me.btnEnd)
    Me.SplitBasis.Panel1.Controls.Add(Me.cboUSER)
    Me.SplitBasis.Panel1.Controls.Add(Me.lblUSE)
    Me.SplitBasis.Panel1.Controls.Add(Me.cboMESS)
    Me.SplitBasis.Panel1.Controls.Add(Me.lblMESS)
    '
    'SplitBasis.Panel2
    '
    Me.SplitBasis.Panel2.Controls.Add(Me.TabDARF)
    Me.SplitBasis.Size = New System.Drawing.Size(900, 609)
    Me.SplitBasis.SplitterDistance = 51
    Me.SplitBasis.TabIndex = 1
    '
    'cboUSER
    '
    Me.cboUSER.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboUSER.Enabled = False
    Me.cboUSER.FormattingEnabled = True
    Me.cboUSER.Location = New System.Drawing.Point(139, 26)
    Me.cboUSER.Name = "cboUSER"
    Me.cboUSER.Size = New System.Drawing.Size(275, 21)
    Me.cboUSER.TabIndex = 30
    '
    'lblUSE
    '
    Me.lblUSE.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblUSE.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblUSE.Location = New System.Drawing.Point(25, 25)
    Me.lblUSE.Name = "lblUSE"
    Me.lblUSE.Size = New System.Drawing.Size(115, 21)
    Me.lblUSE.TabIndex = 31
    Me.lblUSE.Text = "201"
    Me.lblUSE.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'btnEnd
    '
    Me.btnEnd.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnEnd.Location = New System.Drawing.Point(708, 3)
    Me.btnEnd.Name = "btnEnd"
    Me.btnEnd.Size = New System.Drawing.Size(180, 23)
    Me.btnEnd.TabIndex = 37
    Me.btnEnd.Text = "490"
    Me.btnEnd.UseVisualStyleBackColor = True
    '
    'frmDARF
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(900, 609)
    Me.ControlBox = False
    Me.Controls.Add(Me.SplitBasis)
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmDARF"
    Me.Text = "frmDARF"
    Me.TabDARF.ResumeLayout(False)
    Me.TabPage0.ResumeLayout(False)
    Me.SplitDARF_0.Panel1.ResumeLayout(False)
    Me.SplitDARF_0.Panel2.ResumeLayout(False)
    CType(Me.SplitDARF_0, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitDARF_0.ResumeLayout(False)
    Me.panDARF_0.ResumeLayout(False)
    Me.panDARF_0.PerformLayout()
    CType(Me.dbgREF_0, System.ComponentModel.ISupportInitialize).EndInit()
    Me.TabPage1.ResumeLayout(False)
    Me.SplitDARF_1.Panel1.ResumeLayout(False)
    Me.SplitDARF_1.Panel2.ResumeLayout(False)
    CType(Me.SplitDARF_1, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitDARF_1.ResumeLayout(False)
    Me.panDARF_1.ResumeLayout(False)
    Me.panDARF_1.PerformLayout()
    CType(Me.dbgREF_1, System.ComponentModel.ISupportInitialize).EndInit()
    Me.TabPage2.ResumeLayout(False)
    Me.SplitDARF_2.Panel1.ResumeLayout(False)
    Me.SplitDARF_2.Panel2.ResumeLayout(False)
    CType(Me.SplitDARF_2, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitDARF_2.ResumeLayout(False)
    Me.panDARF_2.ResumeLayout(False)
    Me.panDARF_2.PerformLayout()
    CType(Me.dbgREF_2, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.grdWRT_2, System.ComponentModel.ISupportInitialize).EndInit()
    Me.TabPage3.ResumeLayout(False)
    Me.SplitDARF_3.Panel1.ResumeLayout(False)
    Me.SplitDARF_3.Panel2.ResumeLayout(False)
    CType(Me.SplitDARF_3, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitDARF_3.ResumeLayout(False)
    Me.panDARF_3.ResumeLayout(False)
    Me.panDARF_3.PerformLayout()
    CType(Me.grdWRT_3, System.ComponentModel.ISupportInitialize).EndInit()
    Me.TabPage4.ResumeLayout(False)
    Me.SplitDARF_4.Panel1.ResumeLayout(False)
    Me.SplitDARF_4.Panel2.ResumeLayout(False)
    CType(Me.SplitDARF_4, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitDARF_4.ResumeLayout(False)
    Me.panDARF_4.ResumeLayout(False)
    Me.panDARF_4.PerformLayout()
    CType(Me.dbgREF_4, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitBasis.Panel1.ResumeLayout(False)
    Me.SplitBasis.Panel2.ResumeLayout(False)
    CType(Me.SplitBasis, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitBasis.ResumeLayout(False)
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents TabDARF As System.Windows.Forms.TabControl
  Friend WithEvents TabPage0 As System.Windows.Forms.TabPage
  Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
  Friend WithEvents SplitDARF_0 As System.Windows.Forms.SplitContainer
  Friend WithEvents txtSQL_0 As System.Windows.Forms.TextBox
  Friend WithEvents lblSQL_0 As System.Windows.Forms.Label
  Friend WithEvents dbgREF_0 As System.Windows.Forms.DataGridView
  Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
  Friend WithEvents TabPage3 As System.Windows.Forms.TabPage
  Friend WithEvents TabPage4 As System.Windows.Forms.TabPage
  Friend WithEvents btnRUN_0 As System.Windows.Forms.Button
  Friend WithEvents btnMARK_0 As System.Windows.Forms.Button
  Friend WithEvents txtANZ_0 As System.Windows.Forms.TextBox
  Friend WithEvents lblANZ_0 As System.Windows.Forms.Label
  Friend WithEvents btnZEIG_0 As System.Windows.Forms.Button
  Friend WithEvents txtBIS_0 As System.Windows.Forms.TextBox
  Friend WithEvents lblBIS_0 As System.Windows.Forms.Label
  Friend WithEvents txtVON_0 As System.Windows.Forms.TextBox
  Friend WithEvents lblVON_0 As System.Windows.Forms.Label
  Friend WithEvents chkDAT_0 As System.Windows.Forms.CheckBox
  Friend WithEvents cboGRP_0 As System.Windows.Forms.ComboBox
  Friend WithEvents lblGRP_0 As System.Windows.Forms.Label
  Friend WithEvents SplitDARF_1 As System.Windows.Forms.SplitContainer
  Friend WithEvents chkARCN As System.Windows.Forms.CheckBox
  Friend WithEvents txtKENN_1 As System.Windows.Forms.TextBox
  Friend WithEvents lblKENN_1 As System.Windows.Forms.Label
  Friend WithEvents chkDUP As System.Windows.Forms.CheckBox
  Friend WithEvents btnRUN_1 As System.Windows.Forms.Button
  Friend WithEvents btnMARK_1 As System.Windows.Forms.Button
  Friend WithEvents txtANZ_1 As System.Windows.Forms.TextBox
  Friend WithEvents lblANZ_1 As System.Windows.Forms.Label
  Friend WithEvents btnZEIG_1 As System.Windows.Forms.Button
  Friend WithEvents txtBIS_1 As System.Windows.Forms.TextBox
  Friend WithEvents lblBIS_1 As System.Windows.Forms.Label
  Friend WithEvents txtVON_1 As System.Windows.Forms.TextBox
  Friend WithEvents lblVON_1 As System.Windows.Forms.Label
  Friend WithEvents chkDAT_1 As System.Windows.Forms.CheckBox
  Friend WithEvents cboGRP_1 As System.Windows.Forms.ComboBox
  Friend WithEvents lblGRP_1 As System.Windows.Forms.Label
  Friend WithEvents txtSQL_1 As System.Windows.Forms.TextBox
  Friend WithEvents lblSQL_1 As System.Windows.Forms.Label
  Friend WithEvents dbgREF_1 As System.Windows.Forms.DataGridView
  Friend WithEvents cboGRPN As System.Windows.Forms.ComboBox
  Friend WithEvents chkGRPN As System.Windows.Forms.CheckBox
  Friend WithEvents chkANEU As System.Windows.Forms.CheckBox
  Friend WithEvents chkARC_1 As System.Windows.Forms.CheckBox
  Friend WithEvents SplitDARF_2 As System.Windows.Forms.SplitContainer
  Friend WithEvents btnCLIP_2 As System.Windows.Forms.Button
  Friend WithEvents chkARC_2 As System.Windows.Forms.CheckBox
  Friend WithEvents btnRUN_2 As System.Windows.Forms.Button
  Friend WithEvents btnMARK_2 As System.Windows.Forms.Button
  Friend WithEvents txtANZ_2 As System.Windows.Forms.TextBox
  Friend WithEvents lblANZ_2 As System.Windows.Forms.Label
  Friend WithEvents btnZEIG_2 As System.Windows.Forms.Button
  Friend WithEvents txtBIS_2 As System.Windows.Forms.TextBox
  Friend WithEvents lblBIS_2 As System.Windows.Forms.Label
  Friend WithEvents txtVON_2 As System.Windows.Forms.TextBox
  Friend WithEvents lblVON_2 As System.Windows.Forms.Label
  Friend WithEvents chkDAT_2 As System.Windows.Forms.CheckBox
  Friend WithEvents cboGRP_2 As System.Windows.Forms.ComboBox
  Friend WithEvents lblGRP_2 As System.Windows.Forms.Label
  Friend WithEvents txtSQL_2 As System.Windows.Forms.TextBox
  Friend WithEvents lblSQL_2 As System.Windows.Forms.Label
  Friend WithEvents dbgREF_2 As System.Windows.Forms.DataGridView
  Friend WithEvents grdWRT_2 As System.Windows.Forms.DataGridView
  Friend WithEvents SplitDARF_3 As System.Windows.Forms.SplitContainer
  Friend WithEvents txtKENN_3 As System.Windows.Forms.TextBox
  Friend WithEvents lblKENN_3 As System.Windows.Forms.Label
  Friend WithEvents btnCLIP_3 As System.Windows.Forms.Button
  Friend WithEvents chkARC_3 As System.Windows.Forms.CheckBox
  Friend WithEvents btnRUN_3 As System.Windows.Forms.Button
  Friend WithEvents cboGRP_3 As System.Windows.Forms.ComboBox
  Friend WithEvents lblGRP_3 As System.Windows.Forms.Label
  Friend WithEvents grdWRT_3 As System.Windows.Forms.DataGridView
  Friend WithEvents SplitDARF_4 As System.Windows.Forms.SplitContainer
  Friend WithEvents txtANZ_4 As System.Windows.Forms.TextBox
  Friend WithEvents lblANZ_4 As System.Windows.Forms.Label
  Friend WithEvents chkARCR As System.Windows.Forms.CheckBox
  Friend WithEvents txtDIK As System.Windows.Forms.TextBox
  Friend WithEvents lblDIK As System.Windows.Forms.Label
  Friend WithEvents cboGRPR As System.Windows.Forms.ComboBox
  Friend WithEvents lblGRPR As System.Windows.Forms.Label
  Friend WithEvents chkARC_4 As System.Windows.Forms.CheckBox
  Friend WithEvents btnRUN_4 As System.Windows.Forms.Button
  Friend WithEvents btnMARK_4 As System.Windows.Forms.Button
  Friend WithEvents btnZEIG_4 As System.Windows.Forms.Button
  Friend WithEvents txtBIS_4 As System.Windows.Forms.TextBox
  Friend WithEvents lblBIS_4 As System.Windows.Forms.Label
  Friend WithEvents txtVON_4 As System.Windows.Forms.TextBox
  Friend WithEvents lblVON_4 As System.Windows.Forms.Label
  Friend WithEvents chkDAT_4 As System.Windows.Forms.CheckBox
  Friend WithEvents cboGRP_4 As System.Windows.Forms.ComboBox
  Friend WithEvents lblGRP_4 As System.Windows.Forms.Label
  Friend WithEvents txtSQL_4 As System.Windows.Forms.TextBox
  Friend WithEvents lblSQL_4 As System.Windows.Forms.Label
  Friend WithEvents dbgREF_4 As System.Windows.Forms.DataGridView
  Friend WithEvents lblMESS As System.Windows.Forms.Label
  Friend WithEvents cboMESS As System.Windows.Forms.ComboBox
  Friend WithEvents SplitBasis As System.Windows.Forms.SplitContainer
  Friend WithEvents lblHinweis As System.Windows.Forms.Label
  Friend WithEvents cboMSH As System.Windows.Forms.ComboBox
  Friend WithEvents lblMSH As System.Windows.Forms.Label
  Friend WithEvents lblGRPN As System.Windows.Forms.Label
  Friend WithEvents cboUSER As System.Windows.Forms.ComboBox
  Friend WithEvents lblUSE As System.Windows.Forms.Label
  Friend WithEvents panDARF_0 As System.Windows.Forms.Panel
  Friend WithEvents panDARF_1 As System.Windows.Forms.Panel
  Friend WithEvents panDARF_2 As System.Windows.Forms.Panel
  Friend WithEvents panDARF_3 As System.Windows.Forms.Panel
  Friend WithEvents panDARF_4 As System.Windows.Forms.Panel
  Friend WithEvents btnEnd As System.Windows.Forms.Button
End Class
