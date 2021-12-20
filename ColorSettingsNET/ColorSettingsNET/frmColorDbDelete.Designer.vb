<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmColorDbDelete
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
    Me.SplitContCopyVoid = New System.Windows.Forms.SplitContainer()
    Me.splCopyStirGrid = New System.Windows.Forms.SplitContainer()
    Me.txtBIS = New System.Windows.Forms.TextBox()
    Me.panSelectGrid = New System.Windows.Forms.Panel()
    Me.radSelectGrid_2 = New System.Windows.Forms.RadioButton()
    Me.radSelectGrid_1 = New System.Windows.Forms.RadioButton()
    Me.radSelectGrid_0 = New System.Windows.Forms.RadioButton()
    Me.lblVON = New System.Windows.Forms.Label()
    Me.lblBIS = New System.Windows.Forms.Label()
    Me.txtVON = New System.Windows.Forms.TextBox()
    Me.splColRezRwrtCommand = New System.Windows.Forms.SplitContainer()
    Me.panColRezRwrt_1 = New System.Windows.Forms.Panel()
    Me.btnZEIG_1 = New System.Windows.Forms.Button()
    Me.btnMARK_1 = New System.Windows.Forms.Button()
    Me.lblColRezRwrt_1 = New System.Windows.Forms.Label()
    Me.panColRezRwrt_0 = New System.Windows.Forms.Panel()
    Me.btnZEIG_0 = New System.Windows.Forms.Button()
    Me.panFarbmittel = New System.Windows.Forms.Panel()
    Me.radFarbmittel_2 = New System.Windows.Forms.RadioButton()
    Me.radFarbmittel_1 = New System.Windows.Forms.RadioButton()
    Me.radFarbmittel_0 = New System.Windows.Forms.RadioButton()
    Me.lblPanFarbmittel = New System.Windows.Forms.Label()
    Me.btnMARK_0 = New System.Windows.Forms.Button()
    Me.lblColRezRwrt_0 = New System.Windows.Forms.Label()
    Me.cboArtFarbm = New System.Windows.Forms.ComboBox()
    Me.panColRezRwrt_2 = New System.Windows.Forms.Panel()
    Me.btnZEIG_2 = New System.Windows.Forms.Button()
    Me.btnMARK_2 = New System.Windows.Forms.Button()
    Me.lblColRezRwrt_2 = New System.Windows.Forms.Label()
    Me.btnDelete = New System.Windows.Forms.Button()
    Me.dbgColRezRwrt_0 = New System.Windows.Forms.DataGridView()
    Me.dbgColRezRwrt_2 = New System.Windows.Forms.DataGridView()
    Me.dbgColRezRwrt_1 = New System.Windows.Forms.DataGridView()
    Me.cboMischGroup = New System.Windows.Forms.ComboBox()
    Me.cboMessgGroup = New System.Windows.Forms.ComboBox()
    CType(Me.SplitContCopyVoid, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContCopyVoid.Panel1.SuspendLayout()
    Me.SplitContCopyVoid.Panel2.SuspendLayout()
    Me.SplitContCopyVoid.SuspendLayout()
    CType(Me.splCopyStirGrid, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splCopyStirGrid.Panel1.SuspendLayout()
    Me.splCopyStirGrid.Panel2.SuspendLayout()
    Me.splCopyStirGrid.SuspendLayout()
    Me.panSelectGrid.SuspendLayout()
    CType(Me.splColRezRwrtCommand, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splColRezRwrtCommand.Panel1.SuspendLayout()
    Me.splColRezRwrtCommand.Panel2.SuspendLayout()
    Me.splColRezRwrtCommand.SuspendLayout()
    Me.panColRezRwrt_1.SuspendLayout()
    Me.panColRezRwrt_0.SuspendLayout()
    Me.panFarbmittel.SuspendLayout()
    Me.panColRezRwrt_2.SuspendLayout()
    CType(Me.dbgColRezRwrt_0, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.dbgColRezRwrt_2, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.dbgColRezRwrt_1, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'SplitContCopyVoid
    '
    Me.SplitContCopyVoid.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContCopyVoid.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitContCopyVoid.Location = New System.Drawing.Point(0, 0)
    Me.SplitContCopyVoid.Name = "SplitContCopyVoid"
    '
    'SplitContCopyVoid.Panel1
    '
    Me.SplitContCopyVoid.Panel1.Controls.Add(Me.splCopyStirGrid)
    '
    'SplitContCopyVoid.Panel2
    '
    Me.SplitContCopyVoid.Panel2.Controls.Add(Me.dbgColRezRwrt_0)
    Me.SplitContCopyVoid.Panel2.Controls.Add(Me.dbgColRezRwrt_2)
    Me.SplitContCopyVoid.Panel2.Controls.Add(Me.dbgColRezRwrt_1)
    Me.SplitContCopyVoid.Size = New System.Drawing.Size(813, 478)
    Me.SplitContCopyVoid.SplitterDistance = 348
    Me.SplitContCopyVoid.TabIndex = 0
    '
    'splCopyStirGrid
    '
    Me.splCopyStirGrid.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splCopyStirGrid.Location = New System.Drawing.Point(0, 0)
    Me.splCopyStirGrid.Name = "splCopyStirGrid"
    Me.splCopyStirGrid.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splCopyStirGrid.Panel1
    '
    Me.splCopyStirGrid.Panel1.Controls.Add(Me.txtBIS)
    Me.splCopyStirGrid.Panel1.Controls.Add(Me.panSelectGrid)
    Me.splCopyStirGrid.Panel1.Controls.Add(Me.lblVON)
    Me.splCopyStirGrid.Panel1.Controls.Add(Me.lblBIS)
    Me.splCopyStirGrid.Panel1.Controls.Add(Me.txtVON)
    '
    'splCopyStirGrid.Panel2
    '
    Me.splCopyStirGrid.Panel2.Controls.Add(Me.splColRezRwrtCommand)
    Me.splCopyStirGrid.Size = New System.Drawing.Size(348, 478)
    Me.splCopyStirGrid.SplitterDistance = 120
    Me.splCopyStirGrid.TabIndex = 19
    '
    'txtBIS
    '
    Me.txtBIS.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.txtBIS.Location = New System.Drawing.Point(205, 11)
    Me.txtBIS.Name = "txtBIS"
    Me.txtBIS.Size = New System.Drawing.Size(81, 20)
    Me.txtBIS.TabIndex = 17
    Me.txtBIS.Text = "31.12.2011"
    Me.txtBIS.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    '
    'panSelectGrid
    '
    Me.panSelectGrid.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.panSelectGrid.Controls.Add(Me.radSelectGrid_2)
    Me.panSelectGrid.Controls.Add(Me.radSelectGrid_1)
    Me.panSelectGrid.Controls.Add(Me.radSelectGrid_0)
    Me.panSelectGrid.Location = New System.Drawing.Point(77, 37)
    Me.panSelectGrid.Name = "panSelectGrid"
    Me.panSelectGrid.Size = New System.Drawing.Size(181, 55)
    Me.panSelectGrid.TabIndex = 18
    '
    'radSelectGrid_2
    '
    Me.radSelectGrid_2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radSelectGrid_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radSelectGrid_2.Location = New System.Drawing.Point(0, 35)
    Me.radSelectGrid_2.Name = "radSelectGrid_2"
    Me.radSelectGrid_2.Size = New System.Drawing.Size(181, 18)
    Me.radSelectGrid_2.TabIndex = 21
    Me.radSelectGrid_2.Text = "3110"
    Me.radSelectGrid_2.UseVisualStyleBackColor = False
    '
    'radSelectGrid_1
    '
    Me.radSelectGrid_1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radSelectGrid_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radSelectGrid_1.Location = New System.Drawing.Point(0, 18)
    Me.radSelectGrid_1.Name = "radSelectGrid_1"
    Me.radSelectGrid_1.Size = New System.Drawing.Size(181, 18)
    Me.radSelectGrid_1.TabIndex = 20
    Me.radSelectGrid_1.Text = "730"
    Me.radSelectGrid_1.UseVisualStyleBackColor = False
    '
    'radSelectGrid_0
    '
    Me.radSelectGrid_0.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radSelectGrid_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radSelectGrid_0.Checked = True
    Me.radSelectGrid_0.Location = New System.Drawing.Point(0, 3)
    Me.radSelectGrid_0.Name = "radSelectGrid_0"
    Me.radSelectGrid_0.Size = New System.Drawing.Size(181, 18)
    Me.radSelectGrid_0.TabIndex = 19
    Me.radSelectGrid_0.TabStop = True
    Me.radSelectGrid_0.Text = "980"
    Me.radSelectGrid_0.UseVisualStyleBackColor = False
    '
    'lblVON
    '
    Me.lblVON.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblVON.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblVON.Location = New System.Drawing.Point(38, 11)
    Me.lblVON.Name = "lblVON"
    Me.lblVON.Size = New System.Drawing.Size(41, 21)
    Me.lblVON.TabIndex = 14
    Me.lblVON.Text = "376"
    Me.lblVON.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblBIS
    '
    Me.lblBIS.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblBIS.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblBIS.Location = New System.Drawing.Point(163, 11)
    Me.lblBIS.Name = "lblBIS"
    Me.lblBIS.Size = New System.Drawing.Size(41, 21)
    Me.lblBIS.TabIndex = 16
    Me.lblBIS.Text = "377"
    Me.lblBIS.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'txtVON
    '
    Me.txtVON.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.txtVON.Location = New System.Drawing.Point(80, 11)
    Me.txtVON.Name = "txtVON"
    Me.txtVON.Size = New System.Drawing.Size(81, 20)
    Me.txtVON.TabIndex = 15
    Me.txtVON.Text = "01.01.2000"
    Me.txtVON.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    '
    'splColRezRwrtCommand
    '
    Me.splColRezRwrtCommand.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splColRezRwrtCommand.FixedPanel = System.Windows.Forms.FixedPanel.Panel2
    Me.splColRezRwrtCommand.Location = New System.Drawing.Point(0, 0)
    Me.splColRezRwrtCommand.Name = "splColRezRwrtCommand"
    Me.splColRezRwrtCommand.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splColRezRwrtCommand.Panel1
    '
    Me.splColRezRwrtCommand.Panel1.Controls.Add(Me.panColRezRwrt_2)
    Me.splColRezRwrtCommand.Panel1.Controls.Add(Me.panColRezRwrt_1)
    Me.splColRezRwrtCommand.Panel1.Controls.Add(Me.panColRezRwrt_0)
    '
    'splColRezRwrtCommand.Panel2
    '
    Me.splColRezRwrtCommand.Panel2.Controls.Add(Me.btnDelete)
    Me.splColRezRwrtCommand.Size = New System.Drawing.Size(348, 354)
    Me.splColRezRwrtCommand.SplitterDistance = 264
    Me.splColRezRwrtCommand.TabIndex = 3
    '
    'panColRezRwrt_1
    '
    Me.panColRezRwrt_1.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.panColRezRwrt_1.Controls.Add(Me.cboMischGroup)
    Me.panColRezRwrt_1.Controls.Add(Me.btnZEIG_1)
    Me.panColRezRwrt_1.Controls.Add(Me.btnMARK_1)
    Me.panColRezRwrt_1.Controls.Add(Me.lblColRezRwrt_1)
    Me.panColRezRwrt_1.Location = New System.Drawing.Point(0, 0)
    Me.panColRezRwrt_1.Name = "panColRezRwrt_1"
    Me.panColRezRwrt_1.Size = New System.Drawing.Size(348, 264)
    Me.panColRezRwrt_1.TabIndex = 1
    '
    'btnZEIG_1
    '
    Me.btnZEIG_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZEIG_1.Location = New System.Drawing.Point(104, 173)
    Me.btnZEIG_1.Name = "btnZEIG_1"
    Me.btnZEIG_1.Size = New System.Drawing.Size(140, 30)
    Me.btnZEIG_1.TabIndex = 16
    Me.btnZEIG_1.Text = "3690"
    Me.btnZEIG_1.UseVisualStyleBackColor = True
    '
    'btnMARK_1
    '
    Me.btnMARK_1.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnMARK_1.Enabled = False
    Me.btnMARK_1.Location = New System.Drawing.Point(104, 216)
    Me.btnMARK_1.Name = "btnMARK_1"
    Me.btnMARK_1.Size = New System.Drawing.Size(140, 30)
    Me.btnMARK_1.TabIndex = 14
    Me.btnMARK_1.Text = "3670"
    Me.btnMARK_1.UseVisualStyleBackColor = True
    '
    'lblColRezRwrt_1
    '
    Me.lblColRezRwrt_1.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblColRezRwrt_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblColRezRwrt_1.Location = New System.Drawing.Point(18, 15)
    Me.lblColRezRwrt_1.Name = "lblColRezRwrt_1"
    Me.lblColRezRwrt_1.Size = New System.Drawing.Size(308, 25)
    Me.lblColRezRwrt_1.TabIndex = 2
    Me.lblColRezRwrt_1.Text = "2015"
    Me.lblColRezRwrt_1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'panColRezRwrt_0
    '
    Me.panColRezRwrt_0.Controls.Add(Me.btnZEIG_0)
    Me.panColRezRwrt_0.Controls.Add(Me.panFarbmittel)
    Me.panColRezRwrt_0.Controls.Add(Me.btnMARK_0)
    Me.panColRezRwrt_0.Controls.Add(Me.lblColRezRwrt_0)
    Me.panColRezRwrt_0.Controls.Add(Me.cboArtFarbm)
    Me.panColRezRwrt_0.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panColRezRwrt_0.Location = New System.Drawing.Point(0, 0)
    Me.panColRezRwrt_0.Name = "panColRezRwrt_0"
    Me.panColRezRwrt_0.Size = New System.Drawing.Size(348, 264)
    Me.panColRezRwrt_0.TabIndex = 0
    '
    'btnZEIG_0
    '
    Me.btnZEIG_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZEIG_0.Location = New System.Drawing.Point(104, 173)
    Me.btnZEIG_0.Name = "btnZEIG_0"
    Me.btnZEIG_0.Size = New System.Drawing.Size(140, 30)
    Me.btnZEIG_0.TabIndex = 15
    Me.btnZEIG_0.Text = "3690"
    Me.btnZEIG_0.UseVisualStyleBackColor = True
    '
    'panFarbmittel
    '
    Me.panFarbmittel.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.panFarbmittel.Controls.Add(Me.radFarbmittel_2)
    Me.panFarbmittel.Controls.Add(Me.radFarbmittel_1)
    Me.panFarbmittel.Controls.Add(Me.radFarbmittel_0)
    Me.panFarbmittel.Controls.Add(Me.lblPanFarbmittel)
    Me.panFarbmittel.Location = New System.Drawing.Point(12, 70)
    Me.panFarbmittel.Name = "panFarbmittel"
    Me.panFarbmittel.Size = New System.Drawing.Size(321, 87)
    Me.panFarbmittel.TabIndex = 14
    '
    'radFarbmittel_2
    '
    Me.radFarbmittel_2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radFarbmittel_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radFarbmittel_2.Location = New System.Drawing.Point(0, 65)
    Me.radFarbmittel_2.Name = "radFarbmittel_2"
    Me.radFarbmittel_2.Size = New System.Drawing.Size(321, 22)
    Me.radFarbmittel_2.TabIndex = 3
    Me.radFarbmittel_2.Text = "3973"
    Me.radFarbmittel_2.UseVisualStyleBackColor = False
    '
    'radFarbmittel_1
    '
    Me.radFarbmittel_1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radFarbmittel_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radFarbmittel_1.Location = New System.Drawing.Point(0, 43)
    Me.radFarbmittel_1.Name = "radFarbmittel_1"
    Me.radFarbmittel_1.Size = New System.Drawing.Size(321, 22)
    Me.radFarbmittel_1.TabIndex = 2
    Me.radFarbmittel_1.Text = "3972"
    Me.radFarbmittel_1.UseVisualStyleBackColor = False
    '
    'radFarbmittel_0
    '
    Me.radFarbmittel_0.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radFarbmittel_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radFarbmittel_0.Checked = True
    Me.radFarbmittel_0.Location = New System.Drawing.Point(0, 24)
    Me.radFarbmittel_0.Name = "radFarbmittel_0"
    Me.radFarbmittel_0.Size = New System.Drawing.Size(321, 22)
    Me.radFarbmittel_0.TabIndex = 1
    Me.radFarbmittel_0.TabStop = True
    Me.radFarbmittel_0.Text = "3971"
    Me.radFarbmittel_0.UseVisualStyleBackColor = False
    '
    'lblPanFarbmittel
    '
    Me.lblPanFarbmittel.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblPanFarbmittel.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblPanFarbmittel.Location = New System.Drawing.Point(3, 0)
    Me.lblPanFarbmittel.Name = "lblPanFarbmittel"
    Me.lblPanFarbmittel.Size = New System.Drawing.Size(318, 21)
    Me.lblPanFarbmittel.TabIndex = 0
    Me.lblPanFarbmittel.Text = "3970"
    Me.lblPanFarbmittel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'btnMARK_0
    '
    Me.btnMARK_0.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnMARK_0.Enabled = False
    Me.btnMARK_0.Location = New System.Drawing.Point(104, 216)
    Me.btnMARK_0.Name = "btnMARK_0"
    Me.btnMARK_0.Size = New System.Drawing.Size(140, 30)
    Me.btnMARK_0.TabIndex = 13
    Me.btnMARK_0.Text = "3670"
    Me.btnMARK_0.UseVisualStyleBackColor = True
    '
    'lblColRezRwrt_0
    '
    Me.lblColRezRwrt_0.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblColRezRwrt_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblColRezRwrt_0.Location = New System.Drawing.Point(17, 7)
    Me.lblColRezRwrt_0.Name = "lblColRezRwrt_0"
    Me.lblColRezRwrt_0.Size = New System.Drawing.Size(308, 25)
    Me.lblColRezRwrt_0.TabIndex = 1
    Me.lblColRezRwrt_0.Text = "2014"
    Me.lblColRezRwrt_0.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'cboArtFarbm
    '
    Me.cboArtFarbm.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboArtFarbm.FormattingEnabled = True
    Me.cboArtFarbm.Location = New System.Drawing.Point(47, 43)
    Me.cboArtFarbm.Name = "cboArtFarbm"
    Me.cboArtFarbm.Size = New System.Drawing.Size(252, 21)
    Me.cboArtFarbm.TabIndex = 0
    '
    'panColRezRwrt_2
    '
    Me.panColRezRwrt_2.Controls.Add(Me.cboMessgGroup)
    Me.panColRezRwrt_2.Controls.Add(Me.btnZEIG_2)
    Me.panColRezRwrt_2.Controls.Add(Me.btnMARK_2)
    Me.panColRezRwrt_2.Controls.Add(Me.lblColRezRwrt_2)
    Me.panColRezRwrt_2.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panColRezRwrt_2.Location = New System.Drawing.Point(0, 0)
    Me.panColRezRwrt_2.Name = "panColRezRwrt_2"
    Me.panColRezRwrt_2.Size = New System.Drawing.Size(348, 264)
    Me.panColRezRwrt_2.TabIndex = 2
    '
    'btnZEIG_2
    '
    Me.btnZEIG_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZEIG_2.Location = New System.Drawing.Point(104, 163)
    Me.btnZEIG_2.Name = "btnZEIG_2"
    Me.btnZEIG_2.Size = New System.Drawing.Size(140, 30)
    Me.btnZEIG_2.TabIndex = 16
    Me.btnZEIG_2.Text = "3690"
    Me.btnZEIG_2.UseVisualStyleBackColor = True
    '
    'btnMARK_2
    '
    Me.btnMARK_2.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.btnMARK_2.Enabled = False
    Me.btnMARK_2.Location = New System.Drawing.Point(104, 209)
    Me.btnMARK_2.Name = "btnMARK_2"
    Me.btnMARK_2.Size = New System.Drawing.Size(140, 30)
    Me.btnMARK_2.TabIndex = 14
    Me.btnMARK_2.Text = "3670"
    Me.btnMARK_2.UseVisualStyleBackColor = True
    '
    'lblColRezRwrt_2
    '
    Me.lblColRezRwrt_2.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblColRezRwrt_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblColRezRwrt_2.Location = New System.Drawing.Point(18, 15)
    Me.lblColRezRwrt_2.Name = "lblColRezRwrt_2"
    Me.lblColRezRwrt_2.Size = New System.Drawing.Size(308, 25)
    Me.lblColRezRwrt_2.TabIndex = 2
    Me.lblColRezRwrt_2.Text = "2016"
    Me.lblColRezRwrt_2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'btnDelete
    '
    Me.btnDelete.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnDelete.Location = New System.Drawing.Point(47, 26)
    Me.btnDelete.Name = "btnDelete"
    Me.btnDelete.Size = New System.Drawing.Size(260, 26)
    Me.btnDelete.TabIndex = 0
    Me.btnDelete.Text = "381"
    Me.btnDelete.UseVisualStyleBackColor = True
    '
    'dbgColRezRwrt_0
    '
    Me.dbgColRezRwrt_0.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgColRezRwrt_0.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgColRezRwrt_0.Location = New System.Drawing.Point(0, 0)
    Me.dbgColRezRwrt_0.Name = "dbgColRezRwrt_0"
    Me.dbgColRezRwrt_0.Size = New System.Drawing.Size(461, 478)
    Me.dbgColRezRwrt_0.TabIndex = 0
    '
    'dbgColRezRwrt_2
    '
    Me.dbgColRezRwrt_2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgColRezRwrt_2.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgColRezRwrt_2.Location = New System.Drawing.Point(0, 0)
    Me.dbgColRezRwrt_2.Name = "dbgColRezRwrt_2"
    Me.dbgColRezRwrt_2.Size = New System.Drawing.Size(461, 478)
    Me.dbgColRezRwrt_2.TabIndex = 2
    '
    'dbgColRezRwrt_1
    '
    Me.dbgColRezRwrt_1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgColRezRwrt_1.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgColRezRwrt_1.Location = New System.Drawing.Point(0, 0)
    Me.dbgColRezRwrt_1.Name = "dbgColRezRwrt_1"
    Me.dbgColRezRwrt_1.Size = New System.Drawing.Size(461, 478)
    Me.dbgColRezRwrt_1.TabIndex = 1
    '
    'cboMischGroup
    '
    Me.cboMischGroup.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMischGroup.FormattingEnabled = True
    Me.cboMischGroup.Location = New System.Drawing.Point(90, 71)
    Me.cboMischGroup.Name = "cboMischGroup"
    Me.cboMischGroup.Size = New System.Drawing.Size(168, 21)
    Me.cboMischGroup.TabIndex = 17
    '
    'cboMessgGroup
    '
    Me.cboMessgGroup.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMessgGroup.FormattingEnabled = True
    Me.cboMessgGroup.Location = New System.Drawing.Point(90, 73)
    Me.cboMessgGroup.Name = "cboMessgGroup"
    Me.cboMessgGroup.Size = New System.Drawing.Size(168, 21)
    Me.cboMessgGroup.TabIndex = 18
    '
    'frmColorDbDelete
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(813, 478)
    Me.Controls.Add(Me.SplitContCopyVoid)
    Me.Name = "frmColorDbDelete"
    Me.Text = "frmColorDbDelete"
    Me.SplitContCopyVoid.Panel1.ResumeLayout(False)
    Me.SplitContCopyVoid.Panel2.ResumeLayout(False)
    CType(Me.SplitContCopyVoid, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContCopyVoid.ResumeLayout(False)
    Me.splCopyStirGrid.Panel1.ResumeLayout(False)
    Me.splCopyStirGrid.Panel1.PerformLayout()
    Me.splCopyStirGrid.Panel2.ResumeLayout(False)
    CType(Me.splCopyStirGrid, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splCopyStirGrid.ResumeLayout(False)
    Me.panSelectGrid.ResumeLayout(False)
    Me.splColRezRwrtCommand.Panel1.ResumeLayout(False)
    Me.splColRezRwrtCommand.Panel2.ResumeLayout(False)
    CType(Me.splColRezRwrtCommand, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splColRezRwrtCommand.ResumeLayout(False)
    Me.panColRezRwrt_1.ResumeLayout(False)
    Me.panColRezRwrt_0.ResumeLayout(False)
    Me.panFarbmittel.ResumeLayout(False)
    Me.panColRezRwrt_2.ResumeLayout(False)
    CType(Me.dbgColRezRwrt_0, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.dbgColRezRwrt_2,System.ComponentModel.ISupportInitialize).EndInit
    CType(Me.dbgColRezRwrt_1,System.ComponentModel.ISupportInitialize).EndInit
    Me.ResumeLayout(false)

End Sub
  Friend WithEvents SplitContCopyVoid As System.Windows.Forms.SplitContainer
  Friend WithEvents txtBIS As System.Windows.Forms.TextBox
  Friend WithEvents lblBIS As System.Windows.Forms.Label
  Friend WithEvents txtVON As System.Windows.Forms.TextBox
  Friend WithEvents lblVON As System.Windows.Forms.Label
  Friend WithEvents splCopyStirGrid As System.Windows.Forms.SplitContainer
  Friend WithEvents panSelectGrid As System.Windows.Forms.Panel
  Friend WithEvents radSelectGrid_2 As System.Windows.Forms.RadioButton
  Friend WithEvents radSelectGrid_1 As System.Windows.Forms.RadioButton
  Friend WithEvents radSelectGrid_0 As System.Windows.Forms.RadioButton
  Friend WithEvents splColRezRwrtCommand As System.Windows.Forms.SplitContainer
  Friend WithEvents panColRezRwrt_0 As System.Windows.Forms.Panel
  Friend WithEvents lblColRezRwrt_0 As System.Windows.Forms.Label
  Friend WithEvents cboArtFarbm As System.Windows.Forms.ComboBox
  Friend WithEvents panColRezRwrt_2 As System.Windows.Forms.Panel
  Friend WithEvents lblColRezRwrt_2 As System.Windows.Forms.Label
  Friend WithEvents panColRezRwrt_1 As System.Windows.Forms.Panel
  Friend WithEvents lblColRezRwrt_1 As System.Windows.Forms.Label
  Friend WithEvents btnDelete As System.Windows.Forms.Button
  Friend WithEvents dbgColRezRwrt_0 As System.Windows.Forms.DataGridView
  Friend WithEvents dbgColRezRwrt_2 As System.Windows.Forms.DataGridView
  Friend WithEvents dbgColRezRwrt_1 As System.Windows.Forms.DataGridView
  Friend WithEvents btnMARK_0 As System.Windows.Forms.Button
  Friend WithEvents btnMARK_2 As System.Windows.Forms.Button
  Friend WithEvents btnMARK_1 As System.Windows.Forms.Button
  Friend WithEvents panFarbmittel As System.Windows.Forms.Panel
  Friend WithEvents radFarbmittel_2 As System.Windows.Forms.RadioButton
  Friend WithEvents radFarbmittel_1 As System.Windows.Forms.RadioButton
  Friend WithEvents radFarbmittel_0 As System.Windows.Forms.RadioButton
  Friend WithEvents lblPanFarbmittel As System.Windows.Forms.Label
  Friend WithEvents btnZEIG_0 As System.Windows.Forms.Button
  Friend WithEvents btnZEIG_1 As System.Windows.Forms.Button
  Friend WithEvents btnZEIG_2 As System.Windows.Forms.Button
  Friend WithEvents cboMessgGroup As System.Windows.Forms.ComboBox
  Friend WithEvents cboMischGroup As System.Windows.Forms.ComboBox
End Class
