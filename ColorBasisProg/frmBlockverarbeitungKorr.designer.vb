<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmBlockverarbeitungKorr
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
    Me.components = New System.ComponentModel.Container()
    Me.SplitContColorthek = New System.Windows.Forms.SplitContainer()
    Me.chkCalculationOnly = New System.Windows.Forms.CheckBox()
    Me.chkGrundKorr = New System.Windows.Forms.CheckBox()
    Me.btnAnzeigen = New System.Windows.Forms.Button()
    Me.cboGRP = New System.Windows.Forms.ComboBox()
    Me.lblGRP = New System.Windows.Forms.Label()
    Me.btnRechnung = New System.Windows.Forms.Button()
    Me.chkDatum = New System.Windows.Forms.CheckBox()
    Me.txtSuc = New System.Windows.Forms.TextBox()
    Me.btnSUC = New System.Windows.Forms.Button()
    Me.lblSUC = New System.Windows.Forms.Label()
    Me.txtBIS = New System.Windows.Forms.TextBox()
    Me.txtVON = New System.Windows.Forms.TextBox()
    Me.lblBIS = New System.Windows.Forms.Label()
    Me.flgGridErsatz = New System.Windows.Forms.DataGridView()
    Me.lstFAR = New System.Windows.Forms.ListBox()
    Me.lblFAR = New System.Windows.Forms.Label()
    Me.lstFAQ = New System.Windows.Forms.ListBox()
    Me.lblFAQ = New System.Windows.Forms.Label()
    Me.cboFAR = New System.Windows.Forms.ComboBox()
    Me.flgRezepte = New System.Windows.Forms.DataGridView()
    Me.SplitContainGrid = New System.Windows.Forms.SplitContainer()
    Me.panGrid = New System.Windows.Forms.Panel()
    Me.radGrid_1 = New System.Windows.Forms.RadioButton()
    Me.radGrid_0 = New System.Windows.Forms.RadioButton()
    Me.btnZurück = New System.Windows.Forms.Button()
    Me.btnCLIP = New System.Windows.Forms.Button()
    Me.SplitContGrid = New System.Windows.Forms.SplitContainer()
    Me.flgGrid_0 = New System.Windows.Forms.DataGridView()
    Me.ConFlg = New System.Windows.Forms.ContextMenuStrip(Me.components)
    Me.ConFlgKopieren = New System.Windows.Forms.ToolStripMenuItem()
    Me.flgGrid_1 = New System.Windows.Forms.DataGridView()
    Me.grdWRT_0 = New System.Windows.Forms.DataGridView()
    Me.grdWRT_1 = New System.Windows.Forms.DataGridView()
    Me.lblARB = New System.Windows.Forms.Label()
    CType(Me.SplitContColorthek, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContColorthek.Panel1.SuspendLayout()
    Me.SplitContColorthek.Panel2.SuspendLayout()
    Me.SplitContColorthek.SuspendLayout()
    CType(Me.flgGridErsatz, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.flgRezepte, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.SplitContainGrid, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContainGrid.Panel1.SuspendLayout()
    Me.SplitContainGrid.Panel2.SuspendLayout()
    Me.SplitContainGrid.SuspendLayout()
    Me.panGrid.SuspendLayout()
    CType(Me.SplitContGrid, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContGrid.Panel1.SuspendLayout()
    Me.SplitContGrid.Panel2.SuspendLayout()
    Me.SplitContGrid.SuspendLayout()
    CType(Me.flgGrid_0, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.ConFlg.SuspendLayout()
    CType(Me.flgGrid_1, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.grdWRT_0, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.grdWRT_1, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'SplitContColorthek
    '
    Me.SplitContColorthek.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContColorthek.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitContColorthek.Location = New System.Drawing.Point(0, 0)
    Me.SplitContColorthek.Name = "SplitContColorthek"
    '
    'SplitContColorthek.Panel1
    '
    Me.SplitContColorthek.Panel1.BackColor = System.Drawing.SystemColors.ControlDark
    Me.SplitContColorthek.Panel1.Controls.Add(Me.chkCalculationOnly)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.chkGrundKorr)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.btnAnzeigen)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.cboGRP)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.lblGRP)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.btnRechnung)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.chkDatum)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.txtSuc)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.btnSUC)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.lblSUC)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.txtBIS)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.txtVON)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.lblBIS)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.flgGridErsatz)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.lstFAR)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.lblFAR)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.lstFAQ)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.lblFAQ)
    Me.SplitContColorthek.Panel1.Controls.Add(Me.cboFAR)
    '
    'SplitContColorthek.Panel2
    '
    Me.SplitContColorthek.Panel2.BackColor = System.Drawing.SystemColors.ControlDark
    Me.SplitContColorthek.Panel2.Controls.Add(Me.flgRezepte)
    Me.SplitContColorthek.Size = New System.Drawing.Size(871, 665)
    Me.SplitContColorthek.SplitterDistance = 397
    Me.SplitContColorthek.TabIndex = 9
    '
    'chkCalculationOnly
    '
    Me.chkCalculationOnly.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.chkCalculationOnly.BackColor = System.Drawing.Color.White
    Me.chkCalculationOnly.Location = New System.Drawing.Point(77, 114)
    Me.chkCalculationOnly.Name = "chkCalculationOnly"
    Me.chkCalculationOnly.Size = New System.Drawing.Size(236, 20)
    Me.chkCalculationOnly.TabIndex = 185
    Me.chkCalculationOnly.Text = "3955"
    Me.chkCalculationOnly.UseVisualStyleBackColor = False
    '
    'chkGrundKorr
    '
    Me.chkGrundKorr.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.chkGrundKorr.BackColor = System.Drawing.Color.White
    Me.chkGrundKorr.Location = New System.Drawing.Point(96, 480)
    Me.chkGrundKorr.Name = "chkGrundKorr"
    Me.chkGrundKorr.Size = New System.Drawing.Size(209, 20)
    Me.chkGrundKorr.TabIndex = 184
    Me.chkGrundKorr.Text = "3954"
    Me.chkGrundKorr.UseVisualStyleBackColor = False
    '
    'btnAnzeigen
    '
    Me.btnAnzeigen.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnAnzeigen.BackColor = System.Drawing.SystemColors.Control
    Me.btnAnzeigen.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnAnzeigen.Location = New System.Drawing.Point(96, 421)
    Me.btnAnzeigen.Name = "btnAnzeigen"
    Me.btnAnzeigen.Size = New System.Drawing.Size(209, 26)
    Me.btnAnzeigen.TabIndex = 183
    Me.btnAnzeigen.Text = "3690"
    Me.btnAnzeigen.UseVisualStyleBackColor = False
    Me.btnAnzeigen.Visible = False
    '
    'cboGRP
    '
    Me.cboGRP.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboGRP.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboGRP.Enabled = False
    Me.cboGRP.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.cboGRP.FormattingEnabled = True
    Me.cboGRP.Location = New System.Drawing.Point(177, 85)
    Me.cboGRP.Margin = New System.Windows.Forms.Padding(3, 1, 1, 1)
    Me.cboGRP.Name = "cboGRP"
    Me.cboGRP.Size = New System.Drawing.Size(209, 21)
    Me.cboGRP.TabIndex = 182
    '
    'lblGRP
    '
    Me.lblGRP.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblGRP.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.lblGRP.Location = New System.Drawing.Point(21, 86)
    Me.lblGRP.Name = "lblGRP"
    Me.lblGRP.Size = New System.Drawing.Size(157, 20)
    Me.lblGRP.TabIndex = 181
    Me.lblGRP.Text = "3669"
    Me.lblGRP.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'btnRechnung
    '
    Me.btnRechnung.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnRechnung.BackColor = System.Drawing.SystemColors.Control
    Me.btnRechnung.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnRechnung.Location = New System.Drawing.Point(96, 453)
    Me.btnRechnung.Name = "btnRechnung"
    Me.btnRechnung.Size = New System.Drawing.Size(209, 26)
    Me.btnRechnung.TabIndex = 180
    Me.btnRechnung.Text = "886"
    Me.btnRechnung.UseVisualStyleBackColor = False
    Me.btnRechnung.Visible = False
    '
    'chkDatum
    '
    Me.chkDatum.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.chkDatum.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.chkDatum.Checked = True
    Me.chkDatum.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkDatum.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.chkDatum.Location = New System.Drawing.Point(22, 10)
    Me.chkDatum.Name = "chkDatum"
    Me.chkDatum.Size = New System.Drawing.Size(156, 19)
    Me.chkDatum.TabIndex = 178
    Me.chkDatum.Text = "140"
    Me.chkDatum.UseVisualStyleBackColor = False
    '
    'txtSuc
    '
    Me.txtSuc.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.txtSuc.Location = New System.Drawing.Point(19, 61)
    Me.txtSuc.Name = "txtSuc"
    Me.txtSuc.Size = New System.Drawing.Size(367, 20)
    Me.txtSuc.TabIndex = 173
    '
    'btnSUC
    '
    Me.btnSUC.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnSUC.BackColor = System.Drawing.SystemColors.Control
    Me.btnSUC.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnSUC.Location = New System.Drawing.Point(96, 390)
    Me.btnSUC.Name = "btnSUC"
    Me.btnSUC.Size = New System.Drawing.Size(209, 25)
    Me.btnSUC.TabIndex = 179
    Me.btnSUC.Text = "835"
    Me.btnSUC.UseVisualStyleBackColor = False
    '
    'lblSUC
    '
    Me.lblSUC.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblSUC.BackColor = System.Drawing.Color.White
    Me.lblSUC.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblSUC.Location = New System.Drawing.Point(19, 35)
    Me.lblSUC.Name = "lblSUC"
    Me.lblSUC.Size = New System.Drawing.Size(204, 23)
    Me.lblSUC.TabIndex = 174
    Me.lblSUC.Text = "4606"
    Me.lblSUC.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'txtBIS
    '
    Me.txtBIS.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.txtBIS.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.txtBIS.Location = New System.Drawing.Point(316, 10)
    Me.txtBIS.Name = "txtBIS"
    Me.txtBIS.Size = New System.Drawing.Size(70, 20)
    Me.txtBIS.TabIndex = 175
    Me.txtBIS.Text = "01.01.2006"
    Me.txtBIS.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    '
    'txtVON
    '
    Me.txtVON.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.txtVON.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.txtVON.Location = New System.Drawing.Point(184, 10)
    Me.txtVON.Name = "txtVON"
    Me.txtVON.Size = New System.Drawing.Size(72, 20)
    Me.txtVON.TabIndex = 176
    Me.txtVON.Text = "01.01.2005"
    Me.txtVON.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    '
    'lblBIS
    '
    Me.lblBIS.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblBIS.BackColor = System.Drawing.Color.White
    Me.lblBIS.Location = New System.Drawing.Point(260, 11)
    Me.lblBIS.Name = "lblBIS"
    Me.lblBIS.Size = New System.Drawing.Size(54, 24)
    Me.lblBIS.TabIndex = 177
    Me.lblBIS.Text = "377"
    Me.lblBIS.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'flgGridErsatz
    '
    Me.flgGridErsatz.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.flgGridErsatz.BackgroundColor = System.Drawing.SystemColors.ControlLight
    Me.flgGridErsatz.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.flgGridErsatz.Location = New System.Drawing.Point(3, 503)
    Me.flgGridErsatz.Name = "flgGridErsatz"
    Me.flgGridErsatz.Size = New System.Drawing.Size(392, 162)
    Me.flgGridErsatz.TabIndex = 163
    '
    'lstFAR
    '
    Me.lstFAR.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lstFAR.FormattingEnabled = True
    Me.lstFAR.Location = New System.Drawing.Point(75, 302)
    Me.lstFAR.Name = "lstFAR"
    Me.lstFAR.Size = New System.Drawing.Size(239, 82)
    Me.lstFAR.TabIndex = 162
    '
    'lblFAR
    '
    Me.lblFAR.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblFAR.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblFAR.Location = New System.Drawing.Point(74, 250)
    Me.lblFAR.Name = "lblFAR"
    Me.lblFAR.Size = New System.Drawing.Size(239, 22)
    Me.lblFAR.TabIndex = 161
    Me.lblFAR.Text = "738"
    Me.lblFAR.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lstFAQ
    '
    Me.lstFAQ.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lstFAQ.FormattingEnabled = True
    Me.lstFAQ.Location = New System.Drawing.Point(75, 165)
    Me.lstFAQ.Name = "lstFAQ"
    Me.lstFAQ.Size = New System.Drawing.Size(239, 82)
    Me.lstFAQ.TabIndex = 146
    '
    'lblFAQ
    '
    Me.lblFAQ.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblFAQ.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblFAQ.Location = New System.Drawing.Point(75, 137)
    Me.lblFAQ.Name = "lblFAQ"
    Me.lblFAQ.Size = New System.Drawing.Size(239, 25)
    Me.lblFAQ.TabIndex = 145
    Me.lblFAQ.Text = "802"
    Me.lblFAQ.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'cboFAR
    '
    Me.cboFAR.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboFAR.FormattingEnabled = True
    Me.cboFAR.Location = New System.Drawing.Point(75, 275)
    Me.cboFAR.Name = "cboFAR"
    Me.cboFAR.Size = New System.Drawing.Size(239, 21)
    Me.cboFAR.TabIndex = 160
    '
    'flgRezepte
    '
    Me.flgRezepte.AllowUserToAddRows = False
    Me.flgRezepte.AllowUserToDeleteRows = False
    Me.flgRezepte.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.flgRezepte.Dock = System.Windows.Forms.DockStyle.Fill
    Me.flgRezepte.Location = New System.Drawing.Point(0, 0)
    Me.flgRezepte.Name = "flgRezepte"
    Me.flgRezepte.Size = New System.Drawing.Size(470, 665)
    Me.flgRezepte.TabIndex = 1
    '
    'SplitContainGrid
    '
    Me.SplitContainGrid.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContainGrid.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitContainGrid.Location = New System.Drawing.Point(0, 0)
    Me.SplitContainGrid.Name = "SplitContainGrid"
    '
    'SplitContainGrid.Panel1
    '
    Me.SplitContainGrid.Panel1.Controls.Add(Me.panGrid)
    Me.SplitContainGrid.Panel1.Controls.Add(Me.btnZurück)
    Me.SplitContainGrid.Panel1.Controls.Add(Me.btnCLIP)
    '
    'SplitContainGrid.Panel2
    '
    Me.SplitContainGrid.Panel2.Controls.Add(Me.SplitContGrid)
    Me.SplitContainGrid.Size = New System.Drawing.Size(871, 665)
    Me.SplitContainGrid.SplitterDistance = 248
    Me.SplitContainGrid.TabIndex = 10
    Me.SplitContainGrid.Visible = False
    '
    'panGrid
    '
    Me.panGrid.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.panGrid.Controls.Add(Me.radGrid_1)
    Me.panGrid.Controls.Add(Me.radGrid_0)
    Me.panGrid.Location = New System.Drawing.Point(15, 241)
    Me.panGrid.Name = "panGrid"
    Me.panGrid.Size = New System.Drawing.Size(219, 51)
    Me.panGrid.TabIndex = 2
    '
    'radGrid_1
    '
    Me.radGrid_1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radGrid_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radGrid_1.Enabled = False
    Me.radGrid_1.Location = New System.Drawing.Point(0, 28)
    Me.radGrid_1.Name = "radGrid_1"
    Me.radGrid_1.Size = New System.Drawing.Size(219, 23)
    Me.radGrid_1.TabIndex = 3
    Me.radGrid_1.Text = "3414"
    Me.radGrid_1.UseVisualStyleBackColor = False
    '
    'radGrid_0
    '
    Me.radGrid_0.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radGrid_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radGrid_0.Checked = True
    Me.radGrid_0.Enabled = False
    Me.radGrid_0.Location = New System.Drawing.Point(0, 3)
    Me.radGrid_0.Name = "radGrid_0"
    Me.radGrid_0.Size = New System.Drawing.Size(219, 23)
    Me.radGrid_0.TabIndex = 2
    Me.radGrid_0.TabStop = True
    Me.radGrid_0.Text = "3413"
    Me.radGrid_0.UseVisualStyleBackColor = False
    '
    'btnZurück
    '
    Me.btnZurück.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnZurück.Enabled = False
    Me.btnZurück.Location = New System.Drawing.Point(15, 376)
    Me.btnZurück.Name = "btnZurück"
    Me.btnZurück.Size = New System.Drawing.Size(219, 30)
    Me.btnZurück.TabIndex = 21
    Me.btnZurück.Text = "4617"
    Me.btnZurück.UseVisualStyleBackColor = True
    '
    'btnCLIP
    '
    Me.btnCLIP.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnCLIP.Enabled = False
    Me.btnCLIP.Location = New System.Drawing.Point(15, 317)
    Me.btnCLIP.Name = "btnCLIP"
    Me.btnCLIP.Size = New System.Drawing.Size(219, 30)
    Me.btnCLIP.TabIndex = 20
    Me.btnCLIP.Text = "3662"
    Me.btnCLIP.UseVisualStyleBackColor = True
    '
    'SplitContGrid
    '
    Me.SplitContGrid.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContGrid.Location = New System.Drawing.Point(0, 0)
    Me.SplitContGrid.Name = "SplitContGrid"
    '
    'SplitContGrid.Panel1
    '
    Me.SplitContGrid.Panel1.Controls.Add(Me.flgGrid_0)
    Me.SplitContGrid.Panel1.Controls.Add(Me.flgGrid_1)
    '
    'SplitContGrid.Panel2
    '
    Me.SplitContGrid.Panel2.Controls.Add(Me.grdWRT_0)
    Me.SplitContGrid.Panel2.Controls.Add(Me.grdWRT_1)
    Me.SplitContGrid.Size = New System.Drawing.Size(619, 665)
    Me.SplitContGrid.SplitterDistance = 269
    Me.SplitContGrid.TabIndex = 1
    '
    'flgGrid_0
    '
    Me.flgGrid_0.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.flgGrid_0.ContextMenuStrip = Me.ConFlg
    Me.flgGrid_0.Dock = System.Windows.Forms.DockStyle.Fill
    Me.flgGrid_0.Location = New System.Drawing.Point(0, 0)
    Me.flgGrid_0.Name = "flgGrid_0"
    Me.flgGrid_0.Size = New System.Drawing.Size(269, 665)
    Me.flgGrid_0.TabIndex = 0
    '
    'ConFlg
    '
    Me.ConFlg.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ConFlgKopieren})
    Me.ConFlg.Name = "ConFlg"
    Me.ConFlg.Size = New System.Drawing.Size(93, 26)
    '
    'ConFlgKopieren
    '
    Me.ConFlgKopieren.Name = "ConFlgKopieren"
    Me.ConFlgKopieren.Size = New System.Drawing.Size(92, 22)
    Me.ConFlgKopieren.Text = "389"
    '
    'flgGrid_1
    '
    Me.flgGrid_1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.flgGrid_1.ContextMenuStrip = Me.ConFlg
    Me.flgGrid_1.Dock = System.Windows.Forms.DockStyle.Fill
    Me.flgGrid_1.Location = New System.Drawing.Point(0, 0)
    Me.flgGrid_1.Name = "flgGrid_1"
    Me.flgGrid_1.Size = New System.Drawing.Size(269, 665)
    Me.flgGrid_1.TabIndex = 1
    '
    'grdWRT_0
    '
    Me.grdWRT_0.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.grdWRT_0.Dock = System.Windows.Forms.DockStyle.Fill
    Me.grdWRT_0.Location = New System.Drawing.Point(0, 0)
    Me.grdWRT_0.Name = "grdWRT_0"
    Me.grdWRT_0.Size = New System.Drawing.Size(346, 665)
    Me.grdWRT_0.TabIndex = 0
    '
    'grdWRT_1
    '
    Me.grdWRT_1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.grdWRT_1.Dock = System.Windows.Forms.DockStyle.Fill
    Me.grdWRT_1.Location = New System.Drawing.Point(0, 0)
    Me.grdWRT_1.Name = "grdWRT_1"
    Me.grdWRT_1.Size = New System.Drawing.Size(346, 665)
    Me.grdWRT_1.TabIndex = 1
    '
    'lblARB
    '
    Me.lblARB.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblARB.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(192, Byte), Integer))
    Me.lblARB.Font = New System.Drawing.Font("Microsoft Sans Serif", 36.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblARB.Location = New System.Drawing.Point(182, 270)
    Me.lblARB.Name = "lblARB"
    Me.lblARB.Size = New System.Drawing.Size(506, 124)
    Me.lblARB.TabIndex = 59
    Me.lblARB.Text = "2002"
    Me.lblARB.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'frmBlockverarbeitungKorr
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(871, 665)
    Me.ControlBox = False
    Me.Controls.Add(Me.SplitContColorthek)
    Me.Controls.Add(Me.SplitContainGrid)
    Me.Controls.Add(Me.lblARB)
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmBlockverarbeitungKorr"
    Me.Text = "1872"
    Me.SplitContColorthek.Panel1.ResumeLayout(False)
    Me.SplitContColorthek.Panel1.PerformLayout()
    Me.SplitContColorthek.Panel2.ResumeLayout(False)
    CType(Me.SplitContColorthek, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContColorthek.ResumeLayout(False)
    CType(Me.flgGridErsatz, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.flgRezepte, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContainGrid.Panel1.ResumeLayout(False)
    Me.SplitContainGrid.Panel2.ResumeLayout(False)
    CType(Me.SplitContainGrid, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContainGrid.ResumeLayout(False)
    Me.panGrid.ResumeLayout(False)
    Me.SplitContGrid.Panel1.ResumeLayout(False)
    Me.SplitContGrid.Panel2.ResumeLayout(False)
    CType(Me.SplitContGrid, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContGrid.ResumeLayout(False)
    CType(Me.flgGrid_0, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ConFlg.ResumeLayout(False)
    CType(Me.flgGrid_1, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.grdWRT_0, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.grdWRT_1, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents SplitContColorthek As System.Windows.Forms.SplitContainer
  Friend WithEvents lstFAR As System.Windows.Forms.ListBox
  Friend WithEvents lblFAR As System.Windows.Forms.Label
  Friend WithEvents lstFAQ As System.Windows.Forms.ListBox
  Friend WithEvents lblFAQ As System.Windows.Forms.Label
  Friend WithEvents cboFAR As System.Windows.Forms.ComboBox
  Friend WithEvents SplitContainGrid As System.Windows.Forms.SplitContainer
  Friend WithEvents btnZurück As System.Windows.Forms.Button
  Friend WithEvents btnCLIP As System.Windows.Forms.Button
  Friend WithEvents SplitContGrid As System.Windows.Forms.SplitContainer
  Friend WithEvents flgGrid_0 As System.Windows.Forms.DataGridView
  Friend WithEvents grdWRT_0 As System.Windows.Forms.DataGridView
  Friend WithEvents lblARB As System.Windows.Forms.Label
  Friend WithEvents flgGridErsatz As System.Windows.Forms.DataGridView
  Friend WithEvents btnRechnung As System.Windows.Forms.Button
  Friend WithEvents chkDatum As System.Windows.Forms.CheckBox
  Friend WithEvents txtSuc As System.Windows.Forms.TextBox
  Friend WithEvents btnSUC As System.Windows.Forms.Button
  Friend WithEvents lblSUC As System.Windows.Forms.Label
  Friend WithEvents txtBIS As System.Windows.Forms.TextBox
  Friend WithEvents txtVON As System.Windows.Forms.TextBox
  Friend WithEvents lblBIS As System.Windows.Forms.Label
  Friend WithEvents cboGRP As System.Windows.Forms.ComboBox
  Friend WithEvents lblGRP As System.Windows.Forms.Label
  Friend WithEvents flgRezepte As System.Windows.Forms.DataGridView
  Friend WithEvents btnAnzeigen As System.Windows.Forms.Button
  Friend WithEvents grdWRT_1 As System.Windows.Forms.DataGridView
  Friend WithEvents panGrid As System.Windows.Forms.Panel
  Friend WithEvents radGrid_1 As System.Windows.Forms.RadioButton
  Friend WithEvents radGrid_0 As System.Windows.Forms.RadioButton
  Friend WithEvents flgGrid_1 As System.Windows.Forms.DataGridView
  Friend WithEvents chkGrundKorr As System.Windows.Forms.CheckBox
  Friend WithEvents chkCalculationOnly As System.Windows.Forms.CheckBox
  Friend WithEvents ConFlg As System.Windows.Forms.ContextMenuStrip
  Friend WithEvents ConFlgKopieren As System.Windows.Forms.ToolStripMenuItem
End Class
