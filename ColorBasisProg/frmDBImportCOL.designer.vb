<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmDBImportCOL
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
    Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
    Me.OpenFileDialogImport = New System.Windows.Forms.OpenFileDialog()
    Me.SplitContImport = New System.Windows.Forms.SplitContainer()
    Me.btnImport4 = New System.Windows.Forms.Button()
    Me.btnImport3 = New System.Windows.Forms.Button()
    Me.btnImport2 = New System.Windows.Forms.Button()
    Me.btnImport1 = New System.Windows.Forms.Button()
    Me.btnImport0 = New System.Windows.Forms.Button()
    Me.PanelDBase = New System.Windows.Forms.Panel()
    Me.panDURW = New System.Windows.Forms.Panel()
    Me.radDURW_2 = New System.Windows.Forms.RadioButton()
    Me.radDURW_1 = New System.Windows.Forms.RadioButton()
    Me.radDURW_0 = New System.Windows.Forms.RadioButton()
    Me.btnPRF = New System.Windows.Forms.Button()
    Me.chkDUPSO = New System.Windows.Forms.CheckBox()
    Me.chkDUPRZ = New System.Windows.Forms.CheckBox()
    Me.btnDBase = New System.Windows.Forms.Button()
    Me.lblQuel = New System.Windows.Forms.Label()
    Me.lblQQQ = New System.Windows.Forms.Label()
    Me.lblZiel = New System.Windows.Forms.Label()
    Me.lblZZZ = New System.Windows.Forms.Label()
    Me.SplitCont4 = New System.Windows.Forms.SplitContainer()
    Me.btnRUN4 = New System.Windows.Forms.Button()
    Me.txtANZ4 = New System.Windows.Forms.TextBox()
    Me.lblANZ4 = New System.Windows.Forms.Label()
    Me.btnZeig4 = New System.Windows.Forms.Button()
    Me.dbgIMP4 = New System.Windows.Forms.DataGridView()
    Me.SplitCont3 = New System.Windows.Forms.SplitContainer()
    Me.btnRUN3 = New System.Windows.Forms.Button()
    Me.txtANZ3 = New System.Windows.Forms.TextBox()
    Me.lblANZ3 = New System.Windows.Forms.Label()
    Me.btnZeig3 = New System.Windows.Forms.Button()
    Me.dbgIMP3 = New System.Windows.Forms.DataGridView()
    Me.SplitCont2 = New System.Windows.Forms.SplitContainer()
    Me.btnRUN2 = New System.Windows.Forms.Button()
    Me.txtANZ2 = New System.Windows.Forms.TextBox()
    Me.lblANZ2 = New System.Windows.Forms.Label()
    Me.btnZeig2 = New System.Windows.Forms.Button()
    Me.dbgIMP2 = New System.Windows.Forms.DataGridView()
    Me.SplitCont1 = New System.Windows.Forms.SplitContainer()
    Me.chkFAR1 = New System.Windows.Forms.CheckBox()
    Me.chkFAR0 = New System.Windows.Forms.CheckBox()
    Me.chkOVGK = New System.Windows.Forms.CheckBox()
    Me.chkOVGR = New System.Windows.Forms.CheckBox()
    Me.chkOVFA = New System.Windows.Forms.CheckBox()
    Me.btnRUN1 = New System.Windows.Forms.Button()
    Me.txtANZ1 = New System.Windows.Forms.TextBox()
    Me.lblANZ1 = New System.Windows.Forms.Label()
    Me.btnZEIG1 = New System.Windows.Forms.Button()
    Me.dbgIMP1 = New System.Windows.Forms.DataGridView()
    Me.BackgroundWorker1 = New System.ComponentModel.BackgroundWorker()
    CType(Me.SplitContImport, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContImport.Panel1.SuspendLayout()
    Me.SplitContImport.Panel2.SuspendLayout()
    Me.SplitContImport.SuspendLayout()
    Me.PanelDBase.SuspendLayout()
    Me.panDURW.SuspendLayout()
    CType(Me.SplitCont4, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitCont4.Panel1.SuspendLayout()
    Me.SplitCont4.Panel2.SuspendLayout()
    Me.SplitCont4.SuspendLayout()
    CType(Me.dbgIMP4, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.SplitCont3, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitCont3.Panel1.SuspendLayout()
    Me.SplitCont3.Panel2.SuspendLayout()
    Me.SplitCont3.SuspendLayout()
    CType(Me.dbgIMP3, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.SplitCont2, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitCont2.Panel1.SuspendLayout()
    Me.SplitCont2.Panel2.SuspendLayout()
    Me.SplitCont2.SuspendLayout()
    CType(Me.dbgIMP2, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.SplitCont1, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitCont1.Panel1.SuspendLayout()
    Me.SplitCont1.Panel2.SuspendLayout()
    Me.SplitCont1.SuspendLayout()
    CType(Me.dbgIMP1, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'OpenFileDialogImport
    '
    Me.OpenFileDialogImport.FileName = "OpenFileDialog1"
    '
    'SplitContImport
    '
    Me.SplitContImport.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContImport.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitContImport.IsSplitterFixed = True
    Me.SplitContImport.Location = New System.Drawing.Point(0, 0)
    Me.SplitContImport.Name = "SplitContImport"
    Me.SplitContImport.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'SplitContImport.Panel1
    '
    Me.SplitContImport.Panel1.Controls.Add(Me.btnImport4)
    Me.SplitContImport.Panel1.Controls.Add(Me.btnImport3)
    Me.SplitContImport.Panel1.Controls.Add(Me.btnImport2)
    Me.SplitContImport.Panel1.Controls.Add(Me.btnImport1)
    Me.SplitContImport.Panel1.Controls.Add(Me.btnImport0)
    '
    'SplitContImport.Panel2
    '
    Me.SplitContImport.Panel2.Controls.Add(Me.PanelDBase)
    Me.SplitContImport.Panel2.Controls.Add(Me.SplitCont4)
    Me.SplitContImport.Panel2.Controls.Add(Me.SplitCont3)
    Me.SplitContImport.Panel2.Controls.Add(Me.SplitCont2)
    Me.SplitContImport.Panel2.Controls.Add(Me.SplitCont1)
    Me.SplitContImport.Size = New System.Drawing.Size(904, 609)
    Me.SplitContImport.SplitterDistance = 39
    Me.SplitContImport.TabIndex = 1
    '
    'btnImport4
    '
    Me.btnImport4.BackColor = System.Drawing.SystemColors.ControlLight
    Me.btnImport4.Enabled = False
    Me.btnImport4.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnImport4.Location = New System.Drawing.Point(655, 0)
    Me.btnImport4.Name = "btnImport4"
    Me.btnImport4.Size = New System.Drawing.Size(158, 37)
    Me.btnImport4.TabIndex = 9
    Me.btnImport4.Text = "3624"
    Me.btnImport4.UseVisualStyleBackColor = False
    '
    'btnImport3
    '
    Me.btnImport3.BackColor = System.Drawing.SystemColors.ControlLight
    Me.btnImport3.Enabled = False
    Me.btnImport3.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnImport3.Location = New System.Drawing.Point(493, 0)
    Me.btnImport3.Name = "btnImport3"
    Me.btnImport3.Size = New System.Drawing.Size(158, 37)
    Me.btnImport3.TabIndex = 8
    Me.btnImport3.Text = "3623"
    Me.btnImport3.UseVisualStyleBackColor = False
    '
    'btnImport2
    '
    Me.btnImport2.BackColor = System.Drawing.SystemColors.ControlLight
    Me.btnImport2.Enabled = False
    Me.btnImport2.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnImport2.Location = New System.Drawing.Point(329, 0)
    Me.btnImport2.Name = "btnImport2"
    Me.btnImport2.Size = New System.Drawing.Size(158, 37)
    Me.btnImport2.TabIndex = 7
    Me.btnImport2.Text = "3622"
    Me.btnImport2.UseVisualStyleBackColor = False
    '
    'btnImport1
    '
    Me.btnImport1.BackColor = System.Drawing.SystemColors.ControlLight
    Me.btnImport1.Enabled = False
    Me.btnImport1.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnImport1.Location = New System.Drawing.Point(165, 0)
    Me.btnImport1.Name = "btnImport1"
    Me.btnImport1.Size = New System.Drawing.Size(158, 37)
    Me.btnImport1.TabIndex = 6
    Me.btnImport1.Text = "3621"
    Me.btnImport1.UseVisualStyleBackColor = False
    '
    'btnImport0
    '
    Me.btnImport0.AllowDrop = True
    Me.btnImport0.BackColor = System.Drawing.SystemColors.ControlLight
    Me.btnImport0.Enabled = False
    Me.btnImport0.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnImport0.Location = New System.Drawing.Point(3, 0)
    Me.btnImport0.Name = "btnImport0"
    Me.btnImport0.Size = New System.Drawing.Size(158, 37)
    Me.btnImport0.TabIndex = 5
    Me.btnImport0.Text = "3620"
    Me.btnImport0.UseVisualStyleBackColor = False
    '
    'PanelDBase
    '
    Me.PanelDBase.Controls.Add(Me.panDURW)
    Me.PanelDBase.Controls.Add(Me.btnPRF)
    Me.PanelDBase.Controls.Add(Me.chkDUPSO)
    Me.PanelDBase.Controls.Add(Me.chkDUPRZ)
    Me.PanelDBase.Controls.Add(Me.btnDBase)
    Me.PanelDBase.Controls.Add(Me.lblQuel)
    Me.PanelDBase.Controls.Add(Me.lblQQQ)
    Me.PanelDBase.Controls.Add(Me.lblZiel)
    Me.PanelDBase.Controls.Add(Me.lblZZZ)
    Me.PanelDBase.Dock = System.Windows.Forms.DockStyle.Fill
    Me.PanelDBase.Location = New System.Drawing.Point(0, 0)
    Me.PanelDBase.Name = "PanelDBase"
    Me.PanelDBase.Size = New System.Drawing.Size(904, 566)
    Me.PanelDBase.TabIndex = 0
    '
    'panDURW
    '
    Me.panDURW.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.panDURW.Controls.Add(Me.radDURW_2)
    Me.panDURW.Controls.Add(Me.radDURW_1)
    Me.panDURW.Controls.Add(Me.radDURW_0)
    Me.panDURW.Location = New System.Drawing.Point(275, 339)
    Me.panDURW.Name = "panDURW"
    Me.panDURW.Size = New System.Drawing.Size(352, 68)
    Me.panDURW.TabIndex = 18
    '
    'radDURW_2
    '
    Me.radDURW_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.radDURW_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDURW_2.Location = New System.Drawing.Point(2, 45)
    Me.radDURW_2.Name = "radDURW_2"
    Me.radDURW_2.Size = New System.Drawing.Size(347, 20)
    Me.radDURW_2.TabIndex = 2
    Me.radDURW_2.Text = "3678"
    Me.radDURW_2.UseVisualStyleBackColor = False
    '
    'radDURW_1
    '
    Me.radDURW_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.radDURW_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDURW_1.Checked = True
    Me.radDURW_1.Location = New System.Drawing.Point(2, 24)
    Me.radDURW_1.Name = "radDURW_1"
    Me.radDURW_1.Size = New System.Drawing.Size(347, 20)
    Me.radDURW_1.TabIndex = 1
    Me.radDURW_1.TabStop = True
    Me.radDURW_1.Text = "3677"
    Me.radDURW_1.UseVisualStyleBackColor = False
    '
    'radDURW_0
    '
    Me.radDURW_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.radDURW_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDURW_0.Location = New System.Drawing.Point(2, 3)
    Me.radDURW_0.Name = "radDURW_0"
    Me.radDURW_0.Size = New System.Drawing.Size(347, 20)
    Me.radDURW_0.TabIndex = 0
    Me.radDURW_0.Text = "3676"
    Me.radDURW_0.UseVisualStyleBackColor = False
    '
    'btnPRF
    '
    Me.btnPRF.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnPRF.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnPRF.Enabled = False
    Me.btnPRF.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnPRF.Location = New System.Drawing.Point(32, 479)
    Me.btnPRF.Name = "btnPRF"
    Me.btnPRF.Size = New System.Drawing.Size(837, 26)
    Me.btnPRF.TabIndex = 17
    Me.btnPRF.Text = "304"
    Me.btnPRF.UseVisualStyleBackColor = False
    '
    'chkDUPSO
    '
    Me.chkDUPSO.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDUPSO.BackColor = System.Drawing.Color.White
    Me.chkDUPSO.Checked = True
    Me.chkDUPSO.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkDUPSO.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.chkDUPSO.Location = New System.Drawing.Point(275, 440)
    Me.chkDUPSO.Name = "chkDUPSO"
    Me.chkDUPSO.Size = New System.Drawing.Size(352, 21)
    Me.chkDUPSO.TabIndex = 16
    Me.chkDUPSO.Text = "3672"
    Me.chkDUPSO.UseVisualStyleBackColor = False
    '
    'chkDUPRZ
    '
    Me.chkDUPRZ.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDUPRZ.BackColor = System.Drawing.Color.White
    Me.chkDUPRZ.Checked = True
    Me.chkDUPRZ.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkDUPRZ.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.chkDUPRZ.Location = New System.Drawing.Point(275, 413)
    Me.chkDUPRZ.Name = "chkDUPRZ"
    Me.chkDUPRZ.Size = New System.Drawing.Size(352, 21)
    Me.chkDUPRZ.TabIndex = 15
    Me.chkDUPRZ.Text = "3654"
    Me.chkDUPRZ.UseVisualStyleBackColor = False
    '
    'btnDBase
    '
    Me.btnDBase.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnDBase.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnDBase.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnDBase.Location = New System.Drawing.Point(34, 298)
    Me.btnDBase.Name = "btnDBase"
    Me.btnDBase.Size = New System.Drawing.Size(835, 26)
    Me.btnDBase.TabIndex = 13
    Me.btnDBase.Text = "3620"
    Me.btnDBase.UseVisualStyleBackColor = False
    '
    'lblQuel
    '
    Me.lblQuel.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblQuel.BackColor = System.Drawing.Color.White
    Me.lblQuel.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblQuel.Location = New System.Drawing.Point(3, 237)
    Me.lblQuel.Name = "lblQuel"
    Me.lblQuel.Size = New System.Drawing.Size(898, 39)
    Me.lblQuel.TabIndex = 12
    Me.lblQuel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblQQQ
    '
    Me.lblQQQ.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblQQQ.BackColor = System.Drawing.Color.White
    Me.lblQQQ.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblQQQ.Location = New System.Drawing.Point(31, 206)
    Me.lblQQQ.Name = "lblQQQ"
    Me.lblQQQ.Size = New System.Drawing.Size(839, 21)
    Me.lblQQQ.TabIndex = 11
    Me.lblQQQ.Text = "3620"
    Me.lblQQQ.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblZiel
    '
    Me.lblZiel.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblZiel.BackColor = System.Drawing.Color.White
    Me.lblZiel.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblZiel.Location = New System.Drawing.Point(6, 146)
    Me.lblZiel.Name = "lblZiel"
    Me.lblZiel.Size = New System.Drawing.Size(895, 39)
    Me.lblZiel.TabIndex = 10
    Me.lblZiel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblZZZ
    '
    Me.lblZZZ.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblZZZ.BackColor = System.Drawing.Color.White
    Me.lblZZZ.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblZZZ.Location = New System.Drawing.Point(30, 112)
    Me.lblZZZ.Name = "lblZZZ"
    Me.lblZZZ.Size = New System.Drawing.Size(839, 21)
    Me.lblZZZ.TabIndex = 9
    Me.lblZZZ.Text = "3619"
    Me.lblZZZ.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'SplitCont4
    '
    Me.SplitCont4.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitCont4.Location = New System.Drawing.Point(0, 0)
    Me.SplitCont4.Name = "SplitCont4"
    '
    'SplitCont4.Panel1
    '
    Me.SplitCont4.Panel1.Controls.Add(Me.btnRUN4)
    Me.SplitCont4.Panel1.Controls.Add(Me.txtANZ4)
    Me.SplitCont4.Panel1.Controls.Add(Me.lblANZ4)
    Me.SplitCont4.Panel1.Controls.Add(Me.btnZeig4)
    '
    'SplitCont4.Panel2
    '
    Me.SplitCont4.Panel2.Controls.Add(Me.dbgIMP4)
    Me.SplitCont4.Size = New System.Drawing.Size(904, 566)
    Me.SplitCont4.SplitterDistance = 423
    Me.SplitCont4.TabIndex = 5
    '
    'btnRUN4
    '
    Me.btnRUN4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnRUN4.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnRUN4.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnRUN4.Location = New System.Drawing.Point(174, 216)
    Me.btnRUN4.Name = "btnRUN4"
    Me.btnRUN4.Size = New System.Drawing.Size(152, 25)
    Me.btnRUN4.TabIndex = 23
    Me.btnRUN4.Text = "3693"
    Me.btnRUN4.UseVisualStyleBackColor = False
    '
    'txtANZ4
    '
    Me.txtANZ4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtANZ4.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.txtANZ4.Location = New System.Drawing.Point(289, 164)
    Me.txtANZ4.Name = "txtANZ4"
    Me.txtANZ4.Size = New System.Drawing.Size(34, 22)
    Me.txtANZ4.TabIndex = 20
    '
    'lblANZ4
    '
    Me.lblANZ4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblANZ4.BackColor = System.Drawing.Color.White
    Me.lblANZ4.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblANZ4.Location = New System.Drawing.Point(171, 163)
    Me.lblANZ4.Name = "lblANZ4"
    Me.lblANZ4.Size = New System.Drawing.Size(119, 22)
    Me.lblANZ4.TabIndex = 19
    Me.lblANZ4.Text = "3689"
    Me.lblANZ4.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'btnZeig4
    '
    Me.btnZeig4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZeig4.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnZeig4.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnZeig4.Location = New System.Drawing.Point(174, 118)
    Me.btnZeig4.Name = "btnZeig4"
    Me.btnZeig4.Size = New System.Drawing.Size(152, 25)
    Me.btnZeig4.TabIndex = 18
    Me.btnZeig4.Text = "3690"
    Me.btnZeig4.UseVisualStyleBackColor = False
    '
    'dbgIMP4
    '
    Me.dbgIMP4.AllowUserToAddRows = False
    Me.dbgIMP4.AllowUserToDeleteRows = False
    DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
    DataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Control
    DataGridViewCellStyle1.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    DataGridViewCellStyle1.ForeColor = System.Drawing.SystemColors.WindowText
    DataGridViewCellStyle1.NullValue = "gggggggg"
    DataGridViewCellStyle1.SelectionBackColor = System.Drawing.SystemColors.Highlight
    DataGridViewCellStyle1.SelectionForeColor = System.Drawing.SystemColors.HighlightText
    DataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
    Me.dbgIMP4.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle1
    Me.dbgIMP4.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgIMP4.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgIMP4.Location = New System.Drawing.Point(0, 0)
    Me.dbgIMP4.Name = "dbgIMP4"
    Me.dbgIMP4.ReadOnly = True
    Me.dbgIMP4.RowTemplate.Height = 24
    Me.dbgIMP4.Size = New System.Drawing.Size(477, 566)
    Me.dbgIMP4.TabIndex = 0
    '
    'SplitCont3
    '
    Me.SplitCont3.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitCont3.Location = New System.Drawing.Point(0, 0)
    Me.SplitCont3.Name = "SplitCont3"
    '
    'SplitCont3.Panel1
    '
    Me.SplitCont3.Panel1.Controls.Add(Me.btnRUN3)
    Me.SplitCont3.Panel1.Controls.Add(Me.txtANZ3)
    Me.SplitCont3.Panel1.Controls.Add(Me.lblANZ3)
    Me.SplitCont3.Panel1.Controls.Add(Me.btnZeig3)
    '
    'SplitCont3.Panel2
    '
    Me.SplitCont3.Panel2.Controls.Add(Me.dbgIMP3)
    Me.SplitCont3.Size = New System.Drawing.Size(904, 566)
    Me.SplitCont3.SplitterDistance = 423
    Me.SplitCont3.TabIndex = 4
    '
    'btnRUN3
    '
    Me.btnRUN3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnRUN3.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnRUN3.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnRUN3.Location = New System.Drawing.Point(148, 343)
    Me.btnRUN3.Name = "btnRUN3"
    Me.btnRUN3.Size = New System.Drawing.Size(152, 25)
    Me.btnRUN3.TabIndex = 23
    Me.btnRUN3.Text = "3693"
    Me.btnRUN3.UseVisualStyleBackColor = False
    '
    'txtANZ3
    '
    Me.txtANZ3.AccessibleRole = System.Windows.Forms.AccessibleRole.None
    Me.txtANZ3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtANZ3.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.txtANZ3.Location = New System.Drawing.Point(255, 296)
    Me.txtANZ3.Name = "txtANZ3"
    Me.txtANZ3.Size = New System.Drawing.Size(45, 22)
    Me.txtANZ3.TabIndex = 20
    '
    'lblANZ3
    '
    Me.lblANZ3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblANZ3.BackColor = System.Drawing.Color.White
    Me.lblANZ3.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblANZ3.Location = New System.Drawing.Point(148, 293)
    Me.lblANZ3.Name = "lblANZ3"
    Me.lblANZ3.Size = New System.Drawing.Size(108, 22)
    Me.lblANZ3.TabIndex = 19
    Me.lblANZ3.Text = "3689"
    Me.lblANZ3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'btnZeig3
    '
    Me.btnZeig3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZeig3.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnZeig3.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnZeig3.Location = New System.Drawing.Point(148, 245)
    Me.btnZeig3.Name = "btnZeig3"
    Me.btnZeig3.Size = New System.Drawing.Size(152, 25)
    Me.btnZeig3.TabIndex = 18
    Me.btnZeig3.Text = "3690"
    Me.btnZeig3.UseVisualStyleBackColor = False
    '
    'dbgIMP3
    '
    Me.dbgIMP3.AllowUserToAddRows = False
    Me.dbgIMP3.AllowUserToDeleteRows = False
    Me.dbgIMP3.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.DisplayedHeaders
    Me.dbgIMP3.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgIMP3.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgIMP3.Location = New System.Drawing.Point(0, 0)
    Me.dbgIMP3.Name = "dbgIMP3"
    Me.dbgIMP3.ReadOnly = True
    Me.dbgIMP3.RowTemplate.Height = 24
    Me.dbgIMP3.Size = New System.Drawing.Size(477, 566)
    Me.dbgIMP3.TabIndex = 0
    '
    'SplitCont2
    '
    Me.SplitCont2.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitCont2.Location = New System.Drawing.Point(0, 0)
    Me.SplitCont2.Name = "SplitCont2"
    '
    'SplitCont2.Panel1
    '
    Me.SplitCont2.Panel1.Controls.Add(Me.btnRUN2)
    Me.SplitCont2.Panel1.Controls.Add(Me.txtANZ2)
    Me.SplitCont2.Panel1.Controls.Add(Me.lblANZ2)
    Me.SplitCont2.Panel1.Controls.Add(Me.btnZeig2)
    '
    'SplitCont2.Panel2
    '
    Me.SplitCont2.Panel2.Controls.Add(Me.dbgIMP2)
    Me.SplitCont2.Size = New System.Drawing.Size(904, 566)
    Me.SplitCont2.SplitterDistance = 423
    Me.SplitCont2.TabIndex = 3
    '
    'btnRUN2
    '
    Me.btnRUN2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnRUN2.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnRUN2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnRUN2.Location = New System.Drawing.Point(174, 274)
    Me.btnRUN2.Name = "btnRUN2"
    Me.btnRUN2.Size = New System.Drawing.Size(152, 25)
    Me.btnRUN2.TabIndex = 23
    Me.btnRUN2.Text = "3693"
    Me.btnRUN2.UseVisualStyleBackColor = False
    '
    'txtANZ2
    '
    Me.txtANZ2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtANZ2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.txtANZ2.Location = New System.Drawing.Point(280, 224)
    Me.txtANZ2.Name = "txtANZ2"
    Me.txtANZ2.Size = New System.Drawing.Size(46, 22)
    Me.txtANZ2.TabIndex = 20
    '
    'lblANZ2
    '
    Me.lblANZ2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblANZ2.BackColor = System.Drawing.Color.White
    Me.lblANZ2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblANZ2.Location = New System.Drawing.Point(174, 222)
    Me.lblANZ2.Name = "lblANZ2"
    Me.lblANZ2.Size = New System.Drawing.Size(108, 22)
    Me.lblANZ2.TabIndex = 19
    Me.lblANZ2.Text = "3689"
    Me.lblANZ2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'btnZeig2
    '
    Me.btnZeig2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZeig2.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnZeig2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnZeig2.Location = New System.Drawing.Point(174, 193)
    Me.btnZeig2.Name = "btnZeig2"
    Me.btnZeig2.Size = New System.Drawing.Size(152, 25)
    Me.btnZeig2.TabIndex = 18
    Me.btnZeig2.Text = "3690"
    Me.btnZeig2.UseVisualStyleBackColor = False
    '
    'dbgIMP2
    '
    Me.dbgIMP2.AllowUserToAddRows = False
    Me.dbgIMP2.AllowUserToDeleteRows = False
    Me.dbgIMP2.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.DisplayedHeaders
    Me.dbgIMP2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgIMP2.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgIMP2.Location = New System.Drawing.Point(0, 0)
    Me.dbgIMP2.Name = "dbgIMP2"
    Me.dbgIMP2.ReadOnly = True
    Me.dbgIMP2.RowTemplate.Height = 24
    Me.dbgIMP2.Size = New System.Drawing.Size(477, 566)
    Me.dbgIMP2.TabIndex = 0
    '
    'SplitCont1
    '
    Me.SplitCont1.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitCont1.Location = New System.Drawing.Point(0, 0)
    Me.SplitCont1.Name = "SplitCont1"
    '
    'SplitCont1.Panel1
    '
    Me.SplitCont1.Panel1.Controls.Add(Me.chkFAR1)
    Me.SplitCont1.Panel1.Controls.Add(Me.chkFAR0)
    Me.SplitCont1.Panel1.Controls.Add(Me.chkOVGK)
    Me.SplitCont1.Panel1.Controls.Add(Me.chkOVGR)
    Me.SplitCont1.Panel1.Controls.Add(Me.chkOVFA)
    Me.SplitCont1.Panel1.Controls.Add(Me.btnRUN1)
    Me.SplitCont1.Panel1.Controls.Add(Me.txtANZ1)
    Me.SplitCont1.Panel1.Controls.Add(Me.lblANZ1)
    Me.SplitCont1.Panel1.Controls.Add(Me.btnZEIG1)
    '
    'SplitCont1.Panel2
    '
    Me.SplitCont1.Panel2.Controls.Add(Me.dbgIMP1)
    Me.SplitCont1.Size = New System.Drawing.Size(904, 566)
    Me.SplitCont1.SplitterDistance = 423
    Me.SplitCont1.TabIndex = 2
    '
    'chkFAR1
    '
    Me.chkFAR1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkFAR1.BackColor = System.Drawing.Color.White
    Me.chkFAR1.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.chkFAR1.Location = New System.Drawing.Point(66, 253)
    Me.chkFAR1.Name = "chkFAR1"
    Me.chkFAR1.Size = New System.Drawing.Size(311, 20)
    Me.chkFAR1.TabIndex = 27
    Me.chkFAR1.Text = "3627"
    Me.chkFAR1.UseVisualStyleBackColor = False
    '
    'chkFAR0
    '
    Me.chkFAR0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkFAR0.BackColor = System.Drawing.Color.White
    Me.chkFAR0.Checked = True
    Me.chkFAR0.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkFAR0.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.chkFAR0.Location = New System.Drawing.Point(67, 232)
    Me.chkFAR0.Name = "chkFAR0"
    Me.chkFAR0.Size = New System.Drawing.Size(311, 20)
    Me.chkFAR0.TabIndex = 26
    Me.chkFAR0.Text = "3626"
    Me.chkFAR0.UseVisualStyleBackColor = False
    '
    'chkOVGK
    '
    Me.chkOVGK.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkOVGK.BackColor = System.Drawing.Color.White
    Me.chkOVGK.Checked = True
    Me.chkOVGK.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkOVGK.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.chkOVGK.Location = New System.Drawing.Point(66, 352)
    Me.chkOVGK.Name = "chkOVGK"
    Me.chkOVGK.Size = New System.Drawing.Size(311, 20)
    Me.chkOVGK.TabIndex = 25
    Me.chkOVGK.Text = "3647"
    Me.chkOVGK.UseVisualStyleBackColor = False
    '
    'chkOVGR
    '
    Me.chkOVGR.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkOVGR.BackColor = System.Drawing.Color.White
    Me.chkOVGR.Checked = True
    Me.chkOVGR.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkOVGR.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.chkOVGR.Location = New System.Drawing.Point(67, 330)
    Me.chkOVGR.Name = "chkOVGR"
    Me.chkOVGR.Size = New System.Drawing.Size(311, 20)
    Me.chkOVGR.TabIndex = 24
    Me.chkOVGR.Text = "3646"
    Me.chkOVGR.UseVisualStyleBackColor = False
    '
    'chkOVFA
    '
    Me.chkOVFA.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkOVFA.BackColor = System.Drawing.Color.White
    Me.chkOVFA.Checked = True
    Me.chkOVFA.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkOVFA.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.chkOVFA.Location = New System.Drawing.Point(66, 309)
    Me.chkOVFA.Name = "chkOVFA"
    Me.chkOVFA.Size = New System.Drawing.Size(311, 20)
    Me.chkOVFA.TabIndex = 13
    Me.chkOVFA.Text = "3645"
    Me.chkOVFA.UseVisualStyleBackColor = False
    '
    'btnRUN1
    '
    Me.btnRUN1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnRUN1.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnRUN1.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnRUN1.Location = New System.Drawing.Point(174, 436)
    Me.btnRUN1.Name = "btnRUN1"
    Me.btnRUN1.Size = New System.Drawing.Size(152, 25)
    Me.btnRUN1.TabIndex = 23
    Me.btnRUN1.Text = "3693"
    Me.btnRUN1.UseVisualStyleBackColor = False
    '
    'txtANZ1
    '
    Me.txtANZ1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtANZ1.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.txtANZ1.Location = New System.Drawing.Point(284, 171)
    Me.txtANZ1.Name = "txtANZ1"
    Me.txtANZ1.Size = New System.Drawing.Size(42, 22)
    Me.txtANZ1.TabIndex = 20
    '
    'lblANZ1
    '
    Me.lblANZ1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblANZ1.BackColor = System.Drawing.Color.White
    Me.lblANZ1.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblANZ1.Location = New System.Drawing.Point(174, 168)
    Me.lblANZ1.Name = "lblANZ1"
    Me.lblANZ1.Size = New System.Drawing.Size(113, 22)
    Me.lblANZ1.TabIndex = 19
    Me.lblANZ1.Text = "3689"
    Me.lblANZ1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'btnZEIG1
    '
    Me.btnZEIG1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZEIG1.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnZEIG1.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnZEIG1.Location = New System.Drawing.Point(174, 140)
    Me.btnZEIG1.Name = "btnZEIG1"
    Me.btnZEIG1.Size = New System.Drawing.Size(152, 25)
    Me.btnZEIG1.TabIndex = 18
    Me.btnZEIG1.Text = "3690"
    Me.btnZEIG1.UseVisualStyleBackColor = False
    '
    'dbgIMP1
    '
    Me.dbgIMP1.AllowUserToAddRows = False
    Me.dbgIMP1.AllowUserToDeleteRows = False
    Me.dbgIMP1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgIMP1.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgIMP1.Location = New System.Drawing.Point(0, 0)
    Me.dbgIMP1.Name = "dbgIMP1"
    Me.dbgIMP1.ReadOnly = True
    Me.dbgIMP1.RowTemplate.Height = 24
    Me.dbgIMP1.Size = New System.Drawing.Size(477, 566)
    Me.dbgIMP1.TabIndex = 0
    '
    'frmDBImportCOL
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(904, 609)
    Me.Controls.Add(Me.SplitContImport)
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmDBImportCOL"
    Me.ShowIcon = False
    Me.ShowInTaskbar = False
    Me.Text = "frmDBImportCOL"
    Me.SplitContImport.Panel1.ResumeLayout(False)
    Me.SplitContImport.Panel2.ResumeLayout(False)
    CType(Me.SplitContImport, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContImport.ResumeLayout(False)
    Me.PanelDBase.ResumeLayout(False)
    Me.panDURW.ResumeLayout(False)
    Me.SplitCont4.Panel1.ResumeLayout(False)
    Me.SplitCont4.Panel1.PerformLayout()
    Me.SplitCont4.Panel2.ResumeLayout(False)
    CType(Me.SplitCont4, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitCont4.ResumeLayout(False)
    CType(Me.dbgIMP4, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitCont3.Panel1.ResumeLayout(False)
    Me.SplitCont3.Panel1.PerformLayout()
    Me.SplitCont3.Panel2.ResumeLayout(False)
    CType(Me.SplitCont3, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitCont3.ResumeLayout(False)
    CType(Me.dbgIMP3, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitCont2.Panel1.ResumeLayout(False)
    Me.SplitCont2.Panel1.PerformLayout()
    Me.SplitCont2.Panel2.ResumeLayout(False)
    CType(Me.SplitCont2, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitCont2.ResumeLayout(False)
    CType(Me.dbgIMP2, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitCont1.Panel1.ResumeLayout(False)
    Me.SplitCont1.Panel1.PerformLayout()
    Me.SplitCont1.Panel2.ResumeLayout(False)
    CType(Me.SplitCont1, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitCont1.ResumeLayout(False)
    CType(Me.dbgIMP1, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents OpenFileDialogImport As System.Windows.Forms.OpenFileDialog
  Friend WithEvents SplitContImport As System.Windows.Forms.SplitContainer
  Friend WithEvents PanelDBase As System.Windows.Forms.Panel
  Friend WithEvents btnPRF As System.Windows.Forms.Button
  Friend WithEvents chkDUPSO As System.Windows.Forms.CheckBox
  Friend WithEvents chkDUPRZ As System.Windows.Forms.CheckBox
  Friend WithEvents btnDBase As System.Windows.Forms.Button
  Friend WithEvents lblQuel As System.Windows.Forms.Label
  Friend WithEvents lblQQQ As System.Windows.Forms.Label
  Friend WithEvents lblZiel As System.Windows.Forms.Label
  Friend WithEvents lblZZZ As System.Windows.Forms.Label
  Friend WithEvents SplitCont1 As System.Windows.Forms.SplitContainer
  Friend WithEvents chkFAR1 As System.Windows.Forms.CheckBox
  Friend WithEvents chkFAR0 As System.Windows.Forms.CheckBox
  Friend WithEvents chkOVGK As System.Windows.Forms.CheckBox
  Friend WithEvents chkOVGR As System.Windows.Forms.CheckBox
  Friend WithEvents chkOVFA As System.Windows.Forms.CheckBox
  Friend WithEvents btnRUN1 As System.Windows.Forms.Button
  Friend WithEvents txtANZ1 As System.Windows.Forms.TextBox
  Friend WithEvents lblANZ1 As System.Windows.Forms.Label
  Friend WithEvents btnZEIG1 As System.Windows.Forms.Button
  Friend WithEvents dbgIMP1 As System.Windows.Forms.DataGridView
  Friend WithEvents SplitCont2 As System.Windows.Forms.SplitContainer
  Friend WithEvents btnRUN2 As System.Windows.Forms.Button
  Friend WithEvents txtANZ2 As System.Windows.Forms.TextBox
  Friend WithEvents lblANZ2 As System.Windows.Forms.Label
  Friend WithEvents btnZeig2 As System.Windows.Forms.Button
  Friend WithEvents dbgIMP2 As System.Windows.Forms.DataGridView
  Friend WithEvents SplitCont3 As System.Windows.Forms.SplitContainer
  Friend WithEvents btnRUN3 As System.Windows.Forms.Button
  Friend WithEvents txtANZ3 As System.Windows.Forms.TextBox
  Friend WithEvents lblANZ3 As System.Windows.Forms.Label
  Friend WithEvents btnZeig3 As System.Windows.Forms.Button
  Friend WithEvents dbgIMP3 As System.Windows.Forms.DataGridView
  Friend WithEvents SplitCont4 As System.Windows.Forms.SplitContainer
  Friend WithEvents btnRUN4 As System.Windows.Forms.Button
  Friend WithEvents txtANZ4 As System.Windows.Forms.TextBox
  Friend WithEvents lblANZ4 As System.Windows.Forms.Label
  Friend WithEvents btnZeig4 As System.Windows.Forms.Button
  Friend WithEvents dbgIMP4 As System.Windows.Forms.DataGridView
  Friend WithEvents btnImport4 As System.Windows.Forms.Button
  Friend WithEvents btnImport3 As System.Windows.Forms.Button
  Friend WithEvents btnImport2 As System.Windows.Forms.Button
  Friend WithEvents btnImport1 As System.Windows.Forms.Button
  Friend WithEvents btnImport0 As System.Windows.Forms.Button
  Friend WithEvents panDURW As System.Windows.Forms.Panel
  Friend WithEvents radDURW_0 As System.Windows.Forms.RadioButton
  Friend WithEvents radDURW_2 As System.Windows.Forms.RadioButton
  Friend WithEvents radDURW_1 As System.Windows.Forms.RadioButton
  Friend WithEvents BackgroundWorker1 As System.ComponentModel.BackgroundWorker
End Class
