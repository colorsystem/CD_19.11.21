<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmQualControl
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
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmQualControl))
    Dim Style1 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style2 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style3 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style4 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style5 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style6 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style7 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style8 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Me.PanQual = New System.Windows.Forms.Panel()
    Me.splQual = New System.Windows.Forms.SplitContainer()
    Me.panStandard = New System.Windows.Forms.Panel()
    Me.panNextWIN = New System.Windows.Forms.Panel()
    Me.chkNextWIN_08 = New System.Windows.Forms.CheckBox()
    Me.chkNextWIN_07 = New System.Windows.Forms.CheckBox()
    Me.chkNextWIN_06 = New System.Windows.Forms.CheckBox()
    Me.chkNextWIN_05 = New System.Windows.Forms.CheckBox()
    Me.chkNextWIN_04 = New System.Windows.Forms.CheckBox()
    Me.chkNextWIN_03 = New System.Windows.Forms.CheckBox()
    Me.chkNextWIN_02 = New System.Windows.Forms.CheckBox()
    Me.chkNextWIN_01 = New System.Windows.Forms.CheckBox()
    Me.chkNextWIN_00 = New System.Windows.Forms.CheckBox()
    Me.txtTra = New System.Windows.Forms.TextBox()
    Me.lblTRA = New System.Windows.Forms.Label()
    Me.txtKDE = New System.Windows.Forms.ComboBox()
    Me.lblKDE = New System.Windows.Forms.Label()
    Me.btnREF_1 = New System.Windows.Forms.Button()
    Me.btnREF_0 = New System.Windows.Forms.Button()
    Me.btnREF_2 = New System.Windows.Forms.Button()
    Me.txtGRA = New System.Windows.Forms.TextBox()
    Me.btnREF_5 = New System.Windows.Forms.Button()
    Me.txtANZSuch = New System.Windows.Forms.TextBox()
    Me.lblGRA = New System.Windows.Forms.Label()
    Me.lblREF_1 = New System.Windows.Forms.Label()
    Me.lblANZSuch = New System.Windows.Forms.Label()
    Me.cboIND = New System.Windows.Forms.ComboBox()
    Me.lblREF_4 = New System.Windows.Forms.Label()
    Me.lblREF_2 = New System.Windows.Forms.Label()
    Me.cboGRP = New System.Windows.Forms.ComboBox()
    Me.lblIND = New System.Windows.Forms.Label()
    Me.lblREF_3 = New System.Windows.Forms.Label()
    Me.lblREF_5 = New System.Windows.Forms.Label()
    Me.lblGRP = New System.Windows.Forms.Label()
    Me.cboINR = New System.Windows.Forms.ComboBox()
    Me.btnREF_4 = New System.Windows.Forms.Button()
    Me.btnVER = New System.Windows.Forms.Button()
    Me.cboWEL = New System.Windows.Forms.ComboBox()
    Me.lblINR = New System.Windows.Forms.Label()
    Me.btnREF_3 = New System.Windows.Forms.Button()
    Me.btnBWE = New System.Windows.Forms.Button()
    Me.cboBWE = New System.Windows.Forms.ComboBox()
    Me.cboGLZ = New System.Windows.Forms.ComboBox()
    Me.lblGLZ = New System.Windows.Forms.Label()
    Me.lblWEL = New System.Windows.Forms.Label()
    Me.panAuxiliary = New System.Windows.Forms.Panel()
    Me.dbgFarbwerte = New System.Windows.Forms.DataGridView()
    Me.panANGL = New System.Windows.Forms.Panel()
    Me.radANGL_1 = New System.Windows.Forms.RadioButton()
    Me.radANGL_0 = New System.Windows.Forms.RadioButton()
    Me.chkLAB = New System.Windows.Forms.CheckBox()
    Me.btnVERAux = New System.Windows.Forms.Button()
    Me.btnCalcRwerte = New System.Windows.Forms.Button()
    Me.cboINDAux = New System.Windows.Forms.ComboBox()
    Me.lblINDAux = New System.Windows.Forms.Label()
    Me.cboINRAux = New System.Windows.Forms.ComboBox()
    Me.lblINRAux = New System.Windows.Forms.Label()
    Me.cboGLZAux = New System.Windows.Forms.ComboBox()
    Me.lblGLZAux = New System.Windows.Forms.Label()
    Me.splWerte = New System.Windows.Forms.SplitContainer()
    Me.TDBGrid = New System.Windows.Forms.DataGridView()
    Me.ContextTDBGrid = New System.Windows.Forms.ContextMenuStrip(Me.components)
    Me.mnuCopy = New System.Windows.Forms.ToolStripMenuItem()
    Me.mnuPaste = New System.Windows.Forms.ToolStripMenuItem()
    Me.mnuDelete = New System.Windows.Forms.ToolStripMenuItem()
    Me.TDBParameter = New C1.Win.C1TrueDBGrid.C1TrueDBGrid()
    Me.TDBDropParameter = New C1.Win.C1TrueDBGrid.C1TrueDBDropdown()
    Me.splParameter = New System.Windows.Forms.SplitContainer()
    Me.lblMiscel_1 = New System.Windows.Forms.Label()
    Me.lblMiscel_0 = New System.Windows.Forms.Label()
    Me.txtKOM = New System.Windows.Forms.TextBox()
    Me.lblKOM = New System.Windows.Forms.Label()
    Me.cboMiscel_1 = New System.Windows.Forms.ComboBox()
    Me.cboMiscel_0 = New System.Windows.Forms.ComboBox()
    Me.chkABS = New System.Windows.Forms.CheckBox()
    Me.cboFST = New System.Windows.Forms.ComboBox()
    Me.cboANZ = New System.Windows.Forms.ComboBox()
    Me.lblANZ = New System.Windows.Forms.Label()
    Me.lblTYP = New System.Windows.Forms.Label()
    Me.lblSTB = New System.Windows.Forms.Label()
    Me.btnLOE = New System.Windows.Forms.Button()
    Me.chkPARAM = New System.Windows.Forms.CheckBox()
    Me.lblQCE = New System.Windows.Forms.Label()
    Me.panResult = New System.Windows.Forms.Panel()
    Me.splResult = New System.Windows.Forms.SplitContainer()
    Me.splDRQ = New System.Windows.Forms.SplitContainer()
    Me.radDRQ_11 = New System.Windows.Forms.RadioButton()
    Me.radDRQ_10 = New System.Windows.Forms.RadioButton()
    Me.radDRQ_09 = New System.Windows.Forms.RadioButton()
    Me.radDRQ_08 = New System.Windows.Forms.RadioButton()
    Me.radDRQ_07 = New System.Windows.Forms.RadioButton()
    Me.radDRQ_06 = New System.Windows.Forms.RadioButton()
    Me.radDRQ_05 = New System.Windows.Forms.RadioButton()
    Me.radDRQ_04 = New System.Windows.Forms.RadioButton()
    Me.radDRQ_03 = New System.Windows.Forms.RadioButton()
    Me.radDRQ_02 = New System.Windows.Forms.RadioButton()
    Me.radDRQ_01 = New System.Windows.Forms.RadioButton()
    Me.radDRQ_00 = New System.Windows.Forms.RadioButton()
    Me.splDRR = New System.Windows.Forms.SplitContainer()
    Me.splWinNLA = New System.Windows.Forms.SplitContainer()
    Me.panWinChk = New System.Windows.Forms.Panel()
    Me.chkWIN_08 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_07 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_06 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_05 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_04 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_03 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_02 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_01 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_00 = New System.Windows.Forms.CheckBox()
    Me.panWinRad = New System.Windows.Forms.Panel()
    Me.radWIN_08 = New System.Windows.Forms.RadioButton()
    Me.radWIN_07 = New System.Windows.Forms.RadioButton()
    Me.radWIN_06 = New System.Windows.Forms.RadioButton()
    Me.radWIN_05 = New System.Windows.Forms.RadioButton()
    Me.radWIN_04 = New System.Windows.Forms.RadioButton()
    Me.radWIN_03 = New System.Windows.Forms.RadioButton()
    Me.radWIN_02 = New System.Windows.Forms.RadioButton()
    Me.radWIN_01 = New System.Windows.Forms.RadioButton()
    Me.radWIN_00 = New System.Windows.Forms.RadioButton()
    Me.panNlaRad = New System.Windows.Forms.Panel()
    Me.radNLA_4 = New System.Windows.Forms.RadioButton()
    Me.radNLA_3 = New System.Windows.Forms.RadioButton()
    Me.radNLA_2 = New System.Windows.Forms.RadioButton()
    Me.radNLA_1 = New System.Windows.Forms.RadioButton()
    Me.radNLA_0 = New System.Windows.Forms.RadioButton()
    Me.panNlaChk = New System.Windows.Forms.Panel()
    Me.chkNLA_4 = New System.Windows.Forms.CheckBox()
    Me.chkNLA_3 = New System.Windows.Forms.CheckBox()
    Me.chkNLA_2 = New System.Windows.Forms.CheckBox()
    Me.chkNLA_1 = New System.Windows.Forms.CheckBox()
    Me.chkNLA_0 = New System.Windows.Forms.CheckBox()
    Me.splGPL = New System.Windows.Forms.SplitContainer()
    Me.radGPL_4 = New System.Windows.Forms.RadioButton()
    Me.radGPL_3 = New System.Windows.Forms.RadioButton()
    Me.radGPL_2 = New System.Windows.Forms.RadioButton()
    Me.radGPL_1 = New System.Windows.Forms.RadioButton()
    Me.radGPL_0 = New System.Windows.Forms.RadioButton()
    Me.chkLinear = New System.Windows.Forms.CheckBox()
    Me.cboSKAL = New System.Windows.Forms.ComboBox()
    Me.splAnzeige = New System.Windows.Forms.SplitContainer()
    Me.chkDru = New System.Windows.Forms.CheckBox()
    Me.btnUser = New System.Windows.Forms.Button()
    Me.btnKOP = New System.Windows.Forms.Button()
    Me.btnDRU = New System.Windows.Forms.Button()
    Me.btnABR = New System.Windows.Forms.Button()
    Me.btnVOR = New System.Windows.Forms.Button()
    Me.btnGRF = New System.Windows.Forms.Button()
    Me.btnAUS = New System.Windows.Forms.Button()
    Me.flgMerk = New System.Windows.Forms.DataGridView()
    Me.picDRQ = New System.Windows.Forms.PictureBox()
    Me.picLAB = New System.Windows.Forms.PictureBox()
    Me.flgRwert = New System.Windows.Forms.DataGridView()
    Me.picXYZ = New System.Windows.Forms.PictureBox()
    Me.PanQual.SuspendLayout()
    CType(Me.splQual, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splQual.Panel1.SuspendLayout()
    Me.splQual.Panel2.SuspendLayout()
    Me.splQual.SuspendLayout()
    Me.panStandard.SuspendLayout()
    Me.panNextWIN.SuspendLayout()
    Me.panAuxiliary.SuspendLayout()
    CType(Me.dbgFarbwerte, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.panANGL.SuspendLayout()
    CType(Me.splWerte, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splWerte.Panel1.SuspendLayout()
    Me.splWerte.Panel2.SuspendLayout()
    Me.splWerte.SuspendLayout()
    CType(Me.TDBGrid, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.ContextTDBGrid.SuspendLayout()
    CType(Me.TDBParameter, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.TDBDropParameter, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.splParameter, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splParameter.Panel1.SuspendLayout()
    Me.splParameter.Panel2.SuspendLayout()
    Me.splParameter.SuspendLayout()
    Me.panResult.SuspendLayout()
    CType(Me.splResult, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splResult.Panel1.SuspendLayout()
    Me.splResult.Panel2.SuspendLayout()
    Me.splResult.SuspendLayout()
    CType(Me.splDRQ, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splDRQ.Panel1.SuspendLayout()
    Me.splDRQ.Panel2.SuspendLayout()
    Me.splDRQ.SuspendLayout()
    CType(Me.splDRR, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splDRR.Panel1.SuspendLayout()
    Me.splDRR.Panel2.SuspendLayout()
    Me.splDRR.SuspendLayout()
    CType(Me.splWinNLA, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splWinNLA.Panel1.SuspendLayout()
    Me.splWinNLA.Panel2.SuspendLayout()
    Me.splWinNLA.SuspendLayout()
    Me.panWinChk.SuspendLayout()
    Me.panWinRad.SuspendLayout()
    Me.panNlaRad.SuspendLayout()
    Me.panNlaChk.SuspendLayout()
    CType(Me.splGPL, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splGPL.Panel1.SuspendLayout()
    Me.splGPL.Panel2.SuspendLayout()
    Me.splGPL.SuspendLayout()
    CType(Me.splAnzeige, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splAnzeige.Panel1.SuspendLayout()
    Me.splAnzeige.Panel2.SuspendLayout()
    Me.splAnzeige.SuspendLayout()
    CType(Me.flgMerk, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.picDRQ, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.picLAB, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.flgRwert, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.picXYZ, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'PanQual
    '
    Me.PanQual.Controls.Add(Me.splQual)
    Me.PanQual.Dock = System.Windows.Forms.DockStyle.Fill
    Me.PanQual.Location = New System.Drawing.Point(0, 0)
    Me.PanQual.Name = "PanQual"
    Me.PanQual.Size = New System.Drawing.Size(975, 605)
    Me.PanQual.TabIndex = 0
    '
    'splQual
    '
    Me.splQual.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splQual.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.splQual.IsSplitterFixed = True
    Me.splQual.Location = New System.Drawing.Point(0, 0)
    Me.splQual.Name = "splQual"
    Me.splQual.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splQual.Panel1
    '
    Me.splQual.Panel1.BackColor = System.Drawing.SystemColors.ControlDark
    Me.splQual.Panel1.Controls.Add(Me.panStandard)
    Me.splQual.Panel1.Controls.Add(Me.panAuxiliary)
    '
    'splQual.Panel2
    '
    Me.splQual.Panel2.Controls.Add(Me.splWerte)
    Me.splQual.Size = New System.Drawing.Size(975, 605)
    Me.splQual.SplitterDistance = 153
    Me.splQual.TabIndex = 0
    '
    'panStandard
    '
    Me.panStandard.Controls.Add(Me.panNextWIN)
    Me.panStandard.Controls.Add(Me.txtTra)
    Me.panStandard.Controls.Add(Me.lblTRA)
    Me.panStandard.Controls.Add(Me.txtKDE)
    Me.panStandard.Controls.Add(Me.lblKDE)
    Me.panStandard.Controls.Add(Me.btnREF_1)
    Me.panStandard.Controls.Add(Me.btnREF_0)
    Me.panStandard.Controls.Add(Me.btnREF_2)
    Me.panStandard.Controls.Add(Me.txtGRA)
    Me.panStandard.Controls.Add(Me.btnREF_5)
    Me.panStandard.Controls.Add(Me.txtANZSuch)
    Me.panStandard.Controls.Add(Me.lblGRA)
    Me.panStandard.Controls.Add(Me.lblREF_1)
    Me.panStandard.Controls.Add(Me.lblANZSuch)
    Me.panStandard.Controls.Add(Me.cboIND)
    Me.panStandard.Controls.Add(Me.lblREF_4)
    Me.panStandard.Controls.Add(Me.lblREF_2)
    Me.panStandard.Controls.Add(Me.cboGRP)
    Me.panStandard.Controls.Add(Me.lblIND)
    Me.panStandard.Controls.Add(Me.lblREF_3)
    Me.panStandard.Controls.Add(Me.lblREF_5)
    Me.panStandard.Controls.Add(Me.lblGRP)
    Me.panStandard.Controls.Add(Me.cboINR)
    Me.panStandard.Controls.Add(Me.btnREF_4)
    Me.panStandard.Controls.Add(Me.btnVER)
    Me.panStandard.Controls.Add(Me.cboWEL)
    Me.panStandard.Controls.Add(Me.lblINR)
    Me.panStandard.Controls.Add(Me.btnREF_3)
    Me.panStandard.Controls.Add(Me.btnBWE)
    Me.panStandard.Controls.Add(Me.cboBWE)
    Me.panStandard.Controls.Add(Me.cboGLZ)
    Me.panStandard.Controls.Add(Me.lblGLZ)
    Me.panStandard.Controls.Add(Me.lblWEL)
    Me.panStandard.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panStandard.Location = New System.Drawing.Point(0, 0)
    Me.panStandard.Name = "panStandard"
    Me.panStandard.Size = New System.Drawing.Size(975, 153)
    Me.panStandard.TabIndex = 0
    '
    'panNextWIN
    '
    Me.panNextWIN.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.panNextWIN.Controls.Add(Me.chkNextWIN_08)
    Me.panNextWIN.Controls.Add(Me.chkNextWIN_07)
    Me.panNextWIN.Controls.Add(Me.chkNextWIN_06)
    Me.panNextWIN.Controls.Add(Me.chkNextWIN_05)
    Me.panNextWIN.Controls.Add(Me.chkNextWIN_04)
    Me.panNextWIN.Controls.Add(Me.chkNextWIN_03)
    Me.panNextWIN.Controls.Add(Me.chkNextWIN_02)
    Me.panNextWIN.Controls.Add(Me.chkNextWIN_01)
    Me.panNextWIN.Controls.Add(Me.chkNextWIN_00)
    Me.panNextWIN.Location = New System.Drawing.Point(508, 132)
    Me.panNextWIN.Name = "panNextWIN"
    Me.panNextWIN.Size = New System.Drawing.Size(463, 18)
    Me.panNextWIN.TabIndex = 64
    Me.panNextWIN.Visible = False
    '
    'chkNextWIN_08
    '
    Me.chkNextWIN_08.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkNextWIN_08.Location = New System.Drawing.Point(408, 1)
    Me.chkNextWIN_08.Name = "chkNextWIN_08"
    Me.chkNextWIN_08.Size = New System.Drawing.Size(51, 16)
    Me.chkNextWIN_08.TabIndex = 8
    Me.chkNextWIN_08.Text = "CheckBox1"
    Me.chkNextWIN_08.UseVisualStyleBackColor = False
    Me.chkNextWIN_08.Visible = False
    '
    'chkNextWIN_07
    '
    Me.chkNextWIN_07.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkNextWIN_07.Location = New System.Drawing.Point(358, 0)
    Me.chkNextWIN_07.Name = "chkNextWIN_07"
    Me.chkNextWIN_07.Size = New System.Drawing.Size(51, 16)
    Me.chkNextWIN_07.TabIndex = 7
    Me.chkNextWIN_07.Text = "CheckBox1"
    Me.chkNextWIN_07.UseVisualStyleBackColor = False
    Me.chkNextWIN_07.Visible = False
    '
    'chkNextWIN_06
    '
    Me.chkNextWIN_06.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkNextWIN_06.Location = New System.Drawing.Point(307, 1)
    Me.chkNextWIN_06.Name = "chkNextWIN_06"
    Me.chkNextWIN_06.Size = New System.Drawing.Size(51, 16)
    Me.chkNextWIN_06.TabIndex = 6
    Me.chkNextWIN_06.Text = "CheckBox1"
    Me.chkNextWIN_06.UseVisualStyleBackColor = False
    Me.chkNextWIN_06.Visible = False
    '
    'chkNextWIN_05
    '
    Me.chkNextWIN_05.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkNextWIN_05.Location = New System.Drawing.Point(256, 1)
    Me.chkNextWIN_05.Name = "chkNextWIN_05"
    Me.chkNextWIN_05.Size = New System.Drawing.Size(51, 16)
    Me.chkNextWIN_05.TabIndex = 5
    Me.chkNextWIN_05.Text = "CheckBox1"
    Me.chkNextWIN_05.UseVisualStyleBackColor = False
    Me.chkNextWIN_05.Visible = False
    '
    'chkNextWIN_04
    '
    Me.chkNextWIN_04.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkNextWIN_04.Location = New System.Drawing.Point(204, 2)
    Me.chkNextWIN_04.Name = "chkNextWIN_04"
    Me.chkNextWIN_04.Size = New System.Drawing.Size(51, 16)
    Me.chkNextWIN_04.TabIndex = 4
    Me.chkNextWIN_04.Text = "CheckBox1"
    Me.chkNextWIN_04.UseVisualStyleBackColor = False
    Me.chkNextWIN_04.Visible = False
    '
    'chkNextWIN_03
    '
    Me.chkNextWIN_03.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkNextWIN_03.Location = New System.Drawing.Point(153, 2)
    Me.chkNextWIN_03.Name = "chkNextWIN_03"
    Me.chkNextWIN_03.Size = New System.Drawing.Size(51, 16)
    Me.chkNextWIN_03.TabIndex = 3
    Me.chkNextWIN_03.Text = "CheckBox1"
    Me.chkNextWIN_03.UseVisualStyleBackColor = False
    Me.chkNextWIN_03.Visible = False
    '
    'chkNextWIN_02
    '
    Me.chkNextWIN_02.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkNextWIN_02.Location = New System.Drawing.Point(103, 2)
    Me.chkNextWIN_02.Name = "chkNextWIN_02"
    Me.chkNextWIN_02.Size = New System.Drawing.Size(51, 16)
    Me.chkNextWIN_02.TabIndex = 2
    Me.chkNextWIN_02.Text = "CheckBox1"
    Me.chkNextWIN_02.UseVisualStyleBackColor = False
    Me.chkNextWIN_02.Visible = False
    '
    'chkNextWIN_01
    '
    Me.chkNextWIN_01.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkNextWIN_01.Location = New System.Drawing.Point(51, 1)
    Me.chkNextWIN_01.Name = "chkNextWIN_01"
    Me.chkNextWIN_01.Size = New System.Drawing.Size(51, 16)
    Me.chkNextWIN_01.TabIndex = 1
    Me.chkNextWIN_01.Text = "CheckBox1"
    Me.chkNextWIN_01.UseVisualStyleBackColor = False
    Me.chkNextWIN_01.Visible = False
    '
    'chkNextWIN_00
    '
    Me.chkNextWIN_00.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkNextWIN_00.Location = New System.Drawing.Point(0, 0)
    Me.chkNextWIN_00.Name = "chkNextWIN_00"
    Me.chkNextWIN_00.Size = New System.Drawing.Size(51, 16)
    Me.chkNextWIN_00.TabIndex = 0
    Me.chkNextWIN_00.Text = "CheckBox1"
    Me.chkNextWIN_00.UseVisualStyleBackColor = False
    Me.chkNextWIN_00.Visible = False
    '
    'txtTra
    '
    Me.txtTra.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.txtTra.Location = New System.Drawing.Point(537, 5)
    Me.txtTra.Name = "txtTra"
    Me.txtTra.Size = New System.Drawing.Size(70, 20)
    Me.txtTra.TabIndex = 41
    Me.txtTra.Text = "1.0"
    Me.txtTra.Visible = False
    '
    'lblTRA
    '
    Me.lblTRA.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblTRA.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblTRA.Location = New System.Drawing.Point(452, 5)
    Me.lblTRA.Name = "lblTRA"
    Me.lblTRA.Size = New System.Drawing.Size(88, 20)
    Me.lblTRA.TabIndex = 40
    Me.lblTRA.Text = "513"
    Me.lblTRA.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblTRA.Visible = False
    '
    'txtKDE
    '
    Me.txtKDE.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.txtKDE.DropDownStyle = System.Windows.Forms.ComboBoxStyle.Simple
    Me.txtKDE.Enabled = False
    Me.txtKDE.FormattingEnabled = True
    Me.txtKDE.Location = New System.Drawing.Point(687, 5)
    Me.txtKDE.Name = "txtKDE"
    Me.txtKDE.Size = New System.Drawing.Size(69, 21)
    Me.txtKDE.TabIndex = 7
    Me.txtKDE.Visible = False
    '
    'lblKDE
    '
    Me.lblKDE.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblKDE.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblKDE.Location = New System.Drawing.Point(606, 5)
    Me.lblKDE.Name = "lblKDE"
    Me.lblKDE.Size = New System.Drawing.Size(84, 20)
    Me.lblKDE.TabIndex = 6
    Me.lblKDE.Text = "362"
    Me.lblKDE.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblKDE.Visible = False
    '
    'btnREF_1
    '
    Me.btnREF_1.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnREF_1.BackColor = System.Drawing.SystemColors.Control
    Me.btnREF_1.Location = New System.Drawing.Point(101, 29)
    Me.btnREF_1.Name = "btnREF_1"
    Me.btnREF_1.Size = New System.Drawing.Size(198, 23)
    Me.btnREF_1.TabIndex = 11
    Me.btnREF_1.Text = "355"
    Me.btnREF_1.UseVisualStyleBackColor = False
    Me.btnREF_1.Visible = False
    '
    'btnREF_0
    '
    Me.btnREF_0.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnREF_0.BackColor = System.Drawing.SystemColors.Control
    Me.btnREF_0.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnREF_0.Location = New System.Drawing.Point(101, 108)
    Me.btnREF_0.Name = "btnREF_0"
    Me.btnREF_0.Size = New System.Drawing.Size(198, 23)
    Me.btnREF_0.TabIndex = 10
    Me.btnREF_0.Text = "351"
    Me.btnREF_0.UseVisualStyleBackColor = False
    Me.btnREF_0.Visible = False
    '
    'btnREF_2
    '
    Me.btnREF_2.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnREF_2.BackColor = System.Drawing.SystemColors.Control
    Me.btnREF_2.Location = New System.Drawing.Point(101, 54)
    Me.btnREF_2.Name = "btnREF_2"
    Me.btnREF_2.Size = New System.Drawing.Size(198, 23)
    Me.btnREF_2.TabIndex = 12
    Me.btnREF_2.Text = "356"
    Me.btnREF_2.UseVisualStyleBackColor = False
    Me.btnREF_2.Visible = False
    '
    'txtGRA
    '
    Me.txtGRA.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.txtGRA.Location = New System.Drawing.Point(897, 3)
    Me.txtGRA.Name = "txtGRA"
    Me.txtGRA.Size = New System.Drawing.Size(66, 20)
    Me.txtGRA.TabIndex = 9
    Me.txtGRA.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtGRA.Visible = False
    '
    'btnREF_5
    '
    Me.btnREF_5.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnREF_5.BackColor = System.Drawing.SystemColors.Control
    Me.btnREF_5.Location = New System.Drawing.Point(101, 79)
    Me.btnREF_5.Name = "btnREF_5"
    Me.btnREF_5.Size = New System.Drawing.Size(198, 23)
    Me.btnREF_5.TabIndex = 13
    Me.btnREF_5.Text = "354"
    Me.btnREF_5.UseVisualStyleBackColor = False
    Me.btnREF_5.Visible = False
    '
    'txtANZSuch
    '
    Me.txtANZSuch.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.txtANZSuch.Location = New System.Drawing.Point(775, 79)
    Me.txtANZSuch.Name = "txtANZSuch"
    Me.txtANZSuch.Size = New System.Drawing.Size(51, 20)
    Me.txtANZSuch.TabIndex = 37
    Me.txtANZSuch.Text = "20"
    Me.txtANZSuch.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtANZSuch.Visible = False
    '
    'lblGRA
    '
    Me.lblGRA.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblGRA.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRA.Location = New System.Drawing.Point(756, 5)
    Me.lblGRA.Name = "lblGRA"
    Me.lblGRA.Size = New System.Drawing.Size(140, 20)
    Me.lblGRA.TabIndex = 8
    Me.lblGRA.Text = "357"
    Me.lblGRA.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblGRA.Visible = False
    '
    'lblREF_1
    '
    Me.lblREF_1.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblREF_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblREF_1.Location = New System.Drawing.Point(298, 28)
    Me.lblREF_1.Name = "lblREF_1"
    Me.lblREF_1.Size = New System.Drawing.Size(345, 21)
    Me.lblREF_1.TabIndex = 16
    Me.lblREF_1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    Me.lblREF_1.Visible = False
    '
    'lblANZSuch
    '
    Me.lblANZSuch.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblANZSuch.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblANZSuch.Location = New System.Drawing.Point(646, 79)
    Me.lblANZSuch.Name = "lblANZSuch"
    Me.lblANZSuch.Size = New System.Drawing.Size(122, 20)
    Me.lblANZSuch.TabIndex = 36
    Me.lblANZSuch.Text = "3689 "
    Me.lblANZSuch.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblANZSuch.Visible = False
    '
    'cboIND
    '
    Me.cboIND.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboIND.Enabled = False
    Me.cboIND.FormattingEnabled = True
    Me.cboIND.Location = New System.Drawing.Point(383, 3)
    Me.cboIND.Name = "cboIND"
    Me.cboIND.Size = New System.Drawing.Size(69, 21)
    Me.cboIND.TabIndex = 5
    Me.cboIND.Visible = False
    '
    'lblREF_4
    '
    Me.lblREF_4.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblREF_4.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblREF_4.Location = New System.Drawing.Point(298, 56)
    Me.lblREF_4.Name = "lblREF_4"
    Me.lblREF_4.Size = New System.Drawing.Size(345, 20)
    Me.lblREF_4.TabIndex = 20
    Me.lblREF_4.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    Me.lblREF_4.Visible = False
    '
    'lblREF_2
    '
    Me.lblREF_2.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblREF_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblREF_2.Location = New System.Drawing.Point(298, 55)
    Me.lblREF_2.Name = "lblREF_2"
    Me.lblREF_2.Size = New System.Drawing.Size(345, 19)
    Me.lblREF_2.TabIndex = 17
    Me.lblREF_2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    Me.lblREF_2.Visible = False
    '
    'cboGRP
    '
    Me.cboGRP.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboGRP.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboGRP.Enabled = False
    Me.cboGRP.FormattingEnabled = True
    Me.cboGRP.Location = New System.Drawing.Point(775, 108)
    Me.cboGRP.Name = "cboGRP"
    Me.cboGRP.Size = New System.Drawing.Size(192, 21)
    Me.cboGRP.TabIndex = 27
    Me.cboGRP.Visible = False
    '
    'lblIND
    '
    Me.lblIND.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblIND.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblIND.Location = New System.Drawing.Point(294, 5)
    Me.lblIND.Name = "lblIND"
    Me.lblIND.Size = New System.Drawing.Size(88, 20)
    Me.lblIND.TabIndex = 4
    Me.lblIND.Text = "368"
    Me.lblIND.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblIND.Visible = False
    '
    'lblREF_3
    '
    Me.lblREF_3.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblREF_3.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblREF_3.Location = New System.Drawing.Point(298, 32)
    Me.lblREF_3.Name = "lblREF_3"
    Me.lblREF_3.Size = New System.Drawing.Size(345, 20)
    Me.lblREF_3.TabIndex = 19
    Me.lblREF_3.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    Me.lblREF_3.Visible = False
    '
    'lblREF_5
    '
    Me.lblREF_5.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblREF_5.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblREF_5.Location = New System.Drawing.Point(298, 80)
    Me.lblREF_5.Name = "lblREF_5"
    Me.lblREF_5.Size = New System.Drawing.Size(345, 20)
    Me.lblREF_5.TabIndex = 18
    Me.lblREF_5.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    Me.lblREF_5.Visible = False
    '
    'lblGRP
    '
    Me.lblGRP.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblGRP.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRP.Location = New System.Drawing.Point(643, 108)
    Me.lblGRP.Name = "lblGRP"
    Me.lblGRP.Size = New System.Drawing.Size(122, 20)
    Me.lblGRP.TabIndex = 26
    Me.lblGRP.Text = "386"
    Me.lblGRP.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblGRP.Visible = False
    '
    'cboINR
    '
    Me.cboINR.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboINR.Enabled = False
    Me.cboINR.FormattingEnabled = True
    Me.cboINR.Location = New System.Drawing.Point(221, 4)
    Me.cboINR.Name = "cboINR"
    Me.cboINR.Size = New System.Drawing.Size(74, 21)
    Me.cboINR.TabIndex = 3
    Me.cboINR.Visible = False
    '
    'btnREF_4
    '
    Me.btnREF_4.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnREF_4.BackColor = System.Drawing.SystemColors.Control
    Me.btnREF_4.Location = New System.Drawing.Point(101, 54)
    Me.btnREF_4.Name = "btnREF_4"
    Me.btnREF_4.Size = New System.Drawing.Size(198, 23)
    Me.btnREF_4.TabIndex = 15
    Me.btnREF_4.Text = "353"
    Me.btnREF_4.UseVisualStyleBackColor = False
    Me.btnREF_4.Visible = False
    '
    'btnVER
    '
    Me.btnVER.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnVER.BackColor = System.Drawing.SystemColors.Control
    Me.btnVER.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnVER.Location = New System.Drawing.Point(297, 108)
    Me.btnVER.Name = "btnVER"
    Me.btnVER.Size = New System.Drawing.Size(343, 23)
    Me.btnVER.TabIndex = 21
    Me.btnVER.Text = "360"
    Me.btnVER.UseVisualStyleBackColor = False
    Me.btnVER.Visible = False
    '
    'cboWEL
    '
    Me.cboWEL.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboWEL.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboWEL.Enabled = False
    Me.cboWEL.FormattingEnabled = True
    Me.cboWEL.Location = New System.Drawing.Point(775, 53)
    Me.cboWEL.Name = "cboWEL"
    Me.cboWEL.Size = New System.Drawing.Size(117, 21)
    Me.cboWEL.TabIndex = 25
    Me.cboWEL.Visible = False
    '
    'lblINR
    '
    Me.lblINR.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblINR.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblINR.Location = New System.Drawing.Point(135, 4)
    Me.lblINR.Name = "lblINR"
    Me.lblINR.Size = New System.Drawing.Size(86, 20)
    Me.lblINR.TabIndex = 2
    Me.lblINR.Text = "359"
    Me.lblINR.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblINR.Visible = False
    '
    'btnREF_3
    '
    Me.btnREF_3.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnREF_3.BackColor = System.Drawing.SystemColors.Control
    Me.btnREF_3.Location = New System.Drawing.Point(101, 28)
    Me.btnREF_3.Name = "btnREF_3"
    Me.btnREF_3.Size = New System.Drawing.Size(198, 23)
    Me.btnREF_3.TabIndex = 14
    Me.btnREF_3.Text = "352"
    Me.btnREF_3.UseVisualStyleBackColor = False
    Me.btnREF_3.Visible = False
    '
    'btnBWE
    '
    Me.btnBWE.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnBWE.BackColor = System.Drawing.SystemColors.Control
    Me.btnBWE.Location = New System.Drawing.Point(645, 32)
    Me.btnBWE.Name = "btnBWE"
    Me.btnBWE.Size = New System.Drawing.Size(123, 23)
    Me.btnBWE.TabIndex = 22
    Me.btnBWE.Text = "185"
    Me.btnBWE.UseVisualStyleBackColor = False
    Me.btnBWE.Visible = False
    '
    'cboBWE
    '
    Me.cboBWE.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboBWE.Enabled = False
    Me.cboBWE.FormattingEnabled = True
    Me.cboBWE.Location = New System.Drawing.Point(646, 55)
    Me.cboBWE.Name = "cboBWE"
    Me.cboBWE.Size = New System.Drawing.Size(123, 21)
    Me.cboBWE.TabIndex = 24
    Me.cboBWE.Visible = False
    '
    'cboGLZ
    '
    Me.cboGLZ.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboGLZ.Enabled = False
    Me.cboGLZ.FormattingEnabled = True
    Me.cboGLZ.Location = New System.Drawing.Point(75, 3)
    Me.cboGLZ.Name = "cboGLZ"
    Me.cboGLZ.Size = New System.Drawing.Size(53, 21)
    Me.cboGLZ.TabIndex = 1
    Me.cboGLZ.Visible = False
    '
    'lblGLZ
    '
    Me.lblGLZ.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblGLZ.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGLZ.Location = New System.Drawing.Point(21, 3)
    Me.lblGLZ.Name = "lblGLZ"
    Me.lblGLZ.Size = New System.Drawing.Size(53, 20)
    Me.lblGLZ.TabIndex = 0
    Me.lblGLZ.Text = "358"
    Me.lblGLZ.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblGLZ.Visible = False
    '
    'lblWEL
    '
    Me.lblWEL.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblWEL.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblWEL.Location = New System.Drawing.Point(775, 25)
    Me.lblWEL.Name = "lblWEL"
    Me.lblWEL.Size = New System.Drawing.Size(117, 25)
    Me.lblWEL.TabIndex = 23
    Me.lblWEL.Text = "192"
    Me.lblWEL.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblWEL.Visible = False
    '
    'panAuxiliary
    '
    Me.panAuxiliary.Controls.Add(Me.dbgFarbwerte)
    Me.panAuxiliary.Controls.Add(Me.panANGL)
    Me.panAuxiliary.Controls.Add(Me.chkLAB)
    Me.panAuxiliary.Controls.Add(Me.btnVERAux)
    Me.panAuxiliary.Controls.Add(Me.btnCalcRwerte)
    Me.panAuxiliary.Controls.Add(Me.cboINDAux)
    Me.panAuxiliary.Controls.Add(Me.lblINDAux)
    Me.panAuxiliary.Controls.Add(Me.cboINRAux)
    Me.panAuxiliary.Controls.Add(Me.lblINRAux)
    Me.panAuxiliary.Controls.Add(Me.cboGLZAux)
    Me.panAuxiliary.Controls.Add(Me.lblGLZAux)
    Me.panAuxiliary.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panAuxiliary.Location = New System.Drawing.Point(0, 0)
    Me.panAuxiliary.Name = "panAuxiliary"
    Me.panAuxiliary.Size = New System.Drawing.Size(975, 153)
    Me.panAuxiliary.TabIndex = 1
    '
    'dbgFarbwerte
    '
    Me.dbgFarbwerte.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgFarbwerte.Location = New System.Drawing.Point(75, 31)
    Me.dbgFarbwerte.Name = "dbgFarbwerte"
    Me.dbgFarbwerte.Size = New System.Drawing.Size(427, 114)
    Me.dbgFarbwerte.TabIndex = 41
    '
    'panANGL
    '
    Me.panANGL.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.panANGL.Controls.Add(Me.radANGL_1)
    Me.panANGL.Controls.Add(Me.radANGL_0)
    Me.panANGL.Location = New System.Drawing.Point(714, 34)
    Me.panANGL.Name = "panANGL"
    Me.panANGL.Size = New System.Drawing.Size(188, 40)
    Me.panANGL.TabIndex = 40
    '
    'radANGL_1
    '
    Me.radANGL_1.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radANGL_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radANGL_1.Location = New System.Drawing.Point(0, 18)
    Me.radANGL_1.Name = "radANGL_1"
    Me.radANGL_1.Size = New System.Drawing.Size(185, 18)
    Me.radANGL_1.TabIndex = 4
    Me.radANGL_1.Text = "1248"
    Me.radANGL_1.UseVisualStyleBackColor = False
    '
    'radANGL_0
    '
    Me.radANGL_0.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radANGL_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radANGL_0.Checked = True
    Me.radANGL_0.Location = New System.Drawing.Point(0, 3)
    Me.radANGL_0.Name = "radANGL_0"
    Me.radANGL_0.Size = New System.Drawing.Size(185, 18)
    Me.radANGL_0.TabIndex = 3
    Me.radANGL_0.TabStop = True
    Me.radANGL_0.Text = "1247"
    Me.radANGL_0.UseVisualStyleBackColor = False
    '
    'chkLAB
    '
    Me.chkLAB.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.chkLAB.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkLAB.Checked = True
    Me.chkLAB.CheckState = System.Windows.Forms.CheckState.Indeterminate
    Me.chkLAB.Location = New System.Drawing.Point(562, 47)
    Me.chkLAB.Name = "chkLAB"
    Me.chkLAB.Size = New System.Drawing.Size(123, 21)
    Me.chkLAB.TabIndex = 39
    Me.chkLAB.UseVisualStyleBackColor = False
    Me.chkLAB.Visible = False
    '
    'btnVERAux
    '
    Me.btnVERAux.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnVERAux.BackColor = System.Drawing.SystemColors.Control
    Me.btnVERAux.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnVERAux.Location = New System.Drawing.Point(562, 121)
    Me.btnVERAux.Name = "btnVERAux"
    Me.btnVERAux.Size = New System.Drawing.Size(320, 23)
    Me.btnVERAux.TabIndex = 38
    Me.btnVERAux.Text = "360"
    Me.btnVERAux.UseVisualStyleBackColor = False
    Me.btnVERAux.Visible = False
    '
    'btnCalcRwerte
    '
    Me.btnCalcRwerte.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnCalcRwerte.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnCalcRwerte.Location = New System.Drawing.Point(562, 80)
    Me.btnCalcRwerte.Name = "btnCalcRwerte"
    Me.btnCalcRwerte.Size = New System.Drawing.Size(191, 35)
    Me.btnCalcRwerte.TabIndex = 37
    Me.btnCalcRwerte.Text = "3110"
    Me.btnCalcRwerte.UseVisualStyleBackColor = False
    '
    'cboINDAux
    '
    Me.cboINDAux.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboINDAux.Enabled = False
    Me.cboINDAux.FormattingEnabled = True
    Me.cboINDAux.Location = New System.Drawing.Point(435, 8)
    Me.cboINDAux.Name = "cboINDAux"
    Me.cboINDAux.Size = New System.Drawing.Size(69, 21)
    Me.cboINDAux.TabIndex = 35
    Me.cboINDAux.Visible = False
    '
    'lblINDAux
    '
    Me.lblINDAux.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblINDAux.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblINDAux.Location = New System.Drawing.Point(346, 10)
    Me.lblINDAux.Name = "lblINDAux"
    Me.lblINDAux.Size = New System.Drawing.Size(88, 20)
    Me.lblINDAux.TabIndex = 34
    Me.lblINDAux.Text = "368"
    Me.lblINDAux.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblINDAux.Visible = False
    '
    'cboINRAux
    '
    Me.cboINRAux.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboINRAux.Enabled = False
    Me.cboINRAux.FormattingEnabled = True
    Me.cboINRAux.Location = New System.Drawing.Point(273, 9)
    Me.cboINRAux.Name = "cboINRAux"
    Me.cboINRAux.Size = New System.Drawing.Size(74, 21)
    Me.cboINRAux.TabIndex = 33
    Me.cboINRAux.Visible = False
    '
    'lblINRAux
    '
    Me.lblINRAux.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblINRAux.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblINRAux.Location = New System.Drawing.Point(187, 9)
    Me.lblINRAux.Name = "lblINRAux"
    Me.lblINRAux.Size = New System.Drawing.Size(86, 20)
    Me.lblINRAux.TabIndex = 32
    Me.lblINRAux.Text = "359"
    Me.lblINRAux.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblINRAux.Visible = False
    '
    'cboGLZAux
    '
    Me.cboGLZAux.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboGLZAux.Enabled = False
    Me.cboGLZAux.FormattingEnabled = True
    Me.cboGLZAux.Location = New System.Drawing.Point(127, 8)
    Me.cboGLZAux.Name = "cboGLZAux"
    Me.cboGLZAux.Size = New System.Drawing.Size(53, 21)
    Me.cboGLZAux.TabIndex = 31
    Me.cboGLZAux.Visible = False
    '
    'lblGLZAux
    '
    Me.lblGLZAux.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblGLZAux.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGLZAux.Location = New System.Drawing.Point(73, 8)
    Me.lblGLZAux.Name = "lblGLZAux"
    Me.lblGLZAux.Size = New System.Drawing.Size(53, 20)
    Me.lblGLZAux.TabIndex = 30
    Me.lblGLZAux.Text = "358"
    Me.lblGLZAux.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblGLZAux.Visible = False
    '
    'splWerte
    '
    Me.splWerte.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splWerte.FixedPanel = System.Windows.Forms.FixedPanel.Panel2
    Me.splWerte.Location = New System.Drawing.Point(0, 0)
    Me.splWerte.Name = "splWerte"
    Me.splWerte.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splWerte.Panel1
    '
    Me.splWerte.Panel1.Controls.Add(Me.TDBGrid)
    Me.splWerte.Panel1.Controls.Add(Me.TDBParameter)
    Me.splWerte.Panel1.Controls.Add(Me.TDBDropParameter)
    '
    'splWerte.Panel2
    '
    Me.splWerte.Panel2.Controls.Add(Me.splParameter)
    Me.splWerte.Size = New System.Drawing.Size(975, 448)
    Me.splWerte.SplitterDistance = 292
    Me.splWerte.TabIndex = 0
    '
    'TDBGrid
    '
    Me.TDBGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.TDBGrid.ContextMenuStrip = Me.ContextTDBGrid
    Me.TDBGrid.Dock = System.Windows.Forms.DockStyle.Fill
    Me.TDBGrid.Location = New System.Drawing.Point(0, 0)
    Me.TDBGrid.Name = "TDBGrid"
    Me.TDBGrid.RowTemplate.Height = 24
    Me.TDBGrid.Size = New System.Drawing.Size(975, 292)
    Me.TDBGrid.TabIndex = 0
    '
    'ContextTDBGrid
    '
    Me.ContextTDBGrid.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.mnuCopy, Me.mnuPaste, Me.mnuDelete})
    Me.ContextTDBGrid.Name = "ContextTDBGrid"
    Me.ContextTDBGrid.Size = New System.Drawing.Size(99, 70)
    '
    'mnuCopy
    '
    Me.mnuCopy.Name = "mnuCopy"
    Me.mnuCopy.Size = New System.Drawing.Size(98, 22)
    Me.mnuCopy.Text = "3922"
    '
    'mnuPaste
    '
    Me.mnuPaste.Name = "mnuPaste"
    Me.mnuPaste.Size = New System.Drawing.Size(98, 22)
    Me.mnuPaste.Text = "3923"
    '
    'mnuDelete
    '
    Me.mnuDelete.Name = "mnuDelete"
    Me.mnuDelete.Size = New System.Drawing.Size(98, 22)
    Me.mnuDelete.Text = "3924"
    '
    'TDBParameter
    '
    Me.TDBParameter.Dock = System.Windows.Forms.DockStyle.Fill
    Me.TDBParameter.Images.Add(CType(resources.GetObject("TDBParameter.Images"), System.Drawing.Image))
    Me.TDBParameter.Location = New System.Drawing.Point(0, 0)
    Me.TDBParameter.Name = "TDBParameter"
    Me.TDBParameter.PreviewInfo.Location = New System.Drawing.Point(0, 0)
    Me.TDBParameter.PreviewInfo.Size = New System.Drawing.Size(0, 0)
    Me.TDBParameter.PreviewInfo.ZoomFactor = 75.0R
    Me.TDBParameter.PrintInfo.PageSettings = CType(resources.GetObject("TDBParameter.PrintInfo.PageSettings"), System.Drawing.Printing.PageSettings)
    Me.TDBParameter.Size = New System.Drawing.Size(975, 292)
    Me.TDBParameter.TabIndex = 1
    Me.TDBParameter.Text = "C1TrueDBGrid1"
    Me.TDBParameter.PropBag = resources.GetString("TDBParameter.PropBag")
    '
    'TDBDropParameter
    '
    Me.TDBDropParameter.AllowColMove = True
    Me.TDBDropParameter.AllowColSelect = True
    Me.TDBDropParameter.AllowRowSizing = C1.Win.C1TrueDBGrid.RowSizingEnum.AllRows
    Me.TDBDropParameter.AlternatingRows = False
    Me.TDBDropParameter.CaptionStyle = Style1
    Me.TDBDropParameter.ColumnCaptionHeight = 17
    Me.TDBDropParameter.ColumnFooterHeight = 17
    Me.TDBDropParameter.EvenRowStyle = Style2
    Me.TDBDropParameter.FetchRowStyles = False
    Me.TDBDropParameter.FooterStyle = Style3
    Me.TDBDropParameter.HeadingStyle = Style4
    Me.TDBDropParameter.HighLightRowStyle = Style5
    Me.TDBDropParameter.Images.Add(CType(resources.GetObject("TDBDropParameter.Images"), System.Drawing.Image))
    Me.TDBDropParameter.Location = New System.Drawing.Point(713, 221)
    Me.TDBDropParameter.Name = "TDBDropParameter"
    Me.TDBDropParameter.OddRowStyle = Style6
    Me.TDBDropParameter.RecordSelectorStyle = Style7
    Me.TDBDropParameter.RowDivider.Color = System.Drawing.Color.DarkGray
    Me.TDBDropParameter.RowDivider.Style = C1.Win.C1TrueDBGrid.LineStyleEnum.[Single]
    Me.TDBDropParameter.RowSubDividerColor = System.Drawing.Color.DarkGray
    Me.TDBDropParameter.ScrollTips = False
    Me.TDBDropParameter.Size = New System.Drawing.Size(84, 23)
    Me.TDBDropParameter.Style = Style8
    Me.TDBDropParameter.TabIndex = 2
    Me.TDBDropParameter.Text = "C1TrueDBDropdown1"
    Me.TDBDropParameter.Visible = False
    Me.TDBDropParameter.PropBag = resources.GetString("TDBDropParameter.PropBag")
    '
    'splParameter
    '
    Me.splParameter.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splParameter.FixedPanel = System.Windows.Forms.FixedPanel.Panel2
    Me.splParameter.IsSplitterFixed = True
    Me.splParameter.Location = New System.Drawing.Point(0, 0)
    Me.splParameter.Name = "splParameter"
    Me.splParameter.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splParameter.Panel1
    '
    Me.splParameter.Panel1.BackColor = System.Drawing.SystemColors.ControlDark
    Me.splParameter.Panel1.Controls.Add(Me.lblMiscel_1)
    Me.splParameter.Panel1.Controls.Add(Me.lblMiscel_0)
    Me.splParameter.Panel1.Controls.Add(Me.txtKOM)
    Me.splParameter.Panel1.Controls.Add(Me.lblKOM)
    Me.splParameter.Panel1.Controls.Add(Me.cboMiscel_1)
    Me.splParameter.Panel1.Controls.Add(Me.cboMiscel_0)
    Me.splParameter.Panel1.Controls.Add(Me.chkABS)
    Me.splParameter.Panel1.Controls.Add(Me.cboFST)
    Me.splParameter.Panel1.Controls.Add(Me.cboANZ)
    Me.splParameter.Panel1.Controls.Add(Me.lblANZ)
    Me.splParameter.Panel1.Controls.Add(Me.lblTYP)
    Me.splParameter.Panel1.Controls.Add(Me.lblSTB)
    Me.splParameter.Panel1.Controls.Add(Me.btnLOE)
    '
    'splParameter.Panel2
    '
    Me.splParameter.Panel2.BackColor = System.Drawing.SystemColors.ControlDark
    Me.splParameter.Panel2.Controls.Add(Me.chkPARAM)
    Me.splParameter.Size = New System.Drawing.Size(975, 152)
    Me.splParameter.SplitterDistance = 118
    Me.splParameter.TabIndex = 0
    '
    'lblMiscel_1
    '
    Me.lblMiscel_1.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblMiscel_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMiscel_1.Location = New System.Drawing.Point(581, 54)
    Me.lblMiscel_1.Name = "lblMiscel_1"
    Me.lblMiscel_1.Size = New System.Drawing.Size(263, 20)
    Me.lblMiscel_1.TabIndex = 44
    Me.lblMiscel_1.Text = "2456"
    Me.lblMiscel_1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblMiscel_0
    '
    Me.lblMiscel_0.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblMiscel_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMiscel_0.Location = New System.Drawing.Point(316, 54)
    Me.lblMiscel_0.Name = "lblMiscel_0"
    Me.lblMiscel_0.Size = New System.Drawing.Size(259, 20)
    Me.lblMiscel_0.TabIndex = 43
    Me.lblMiscel_0.Text = "2455"
    Me.lblMiscel_0.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'txtKOM
    '
    Me.txtKOM.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.txtKOM.Location = New System.Drawing.Point(315, 95)
    Me.txtKOM.Name = "txtKOM"
    Me.txtKOM.Size = New System.Drawing.Size(529, 20)
    Me.txtKOM.TabIndex = 42
    Me.txtKOM.Visible = False
    '
    'lblKOM
    '
    Me.lblKOM.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblKOM.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblKOM.Location = New System.Drawing.Point(88, 94)
    Me.lblKOM.Name = "lblKOM"
    Me.lblKOM.Size = New System.Drawing.Size(221, 20)
    Me.lblKOM.TabIndex = 41
    Me.lblKOM.Text = "384"
    Me.lblKOM.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblKOM.Visible = False
    '
    'cboMiscel_1
    '
    Me.cboMiscel_1.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboMiscel_1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboMiscel_1.Enabled = False
    Me.cboMiscel_1.FormattingEnabled = True
    Me.cboMiscel_1.Location = New System.Drawing.Point(581, 75)
    Me.cboMiscel_1.Name = "cboMiscel_1"
    Me.cboMiscel_1.Size = New System.Drawing.Size(263, 21)
    Me.cboMiscel_1.TabIndex = 40
    '
    'cboMiscel_0
    '
    Me.cboMiscel_0.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboMiscel_0.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboMiscel_0.Enabled = False
    Me.cboMiscel_0.FormattingEnabled = True
    Me.cboMiscel_0.Location = New System.Drawing.Point(313, 75)
    Me.cboMiscel_0.Name = "cboMiscel_0"
    Me.cboMiscel_0.Size = New System.Drawing.Size(260, 21)
    Me.cboMiscel_0.TabIndex = 39
    '
    'chkABS
    '
    Me.chkABS.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.chkABS.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkABS.Location = New System.Drawing.Point(89, 69)
    Me.chkABS.Name = "chkABS"
    Me.chkABS.Size = New System.Drawing.Size(221, 22)
    Me.chkABS.TabIndex = 38
    Me.chkABS.Text = "363"
    Me.chkABS.UseVisualStyleBackColor = False
    Me.chkABS.Visible = False
    '
    'cboFST
    '
    Me.cboFST.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboFST.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboFST.Enabled = False
    Me.cboFST.FormattingEnabled = True
    Me.cboFST.Location = New System.Drawing.Point(89, 30)
    Me.cboFST.Name = "cboFST"
    Me.cboFST.Size = New System.Drawing.Size(413, 21)
    Me.cboFST.TabIndex = 37
    Me.cboFST.Visible = False
    '
    'cboANZ
    '
    Me.cboANZ.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboANZ.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboANZ.FormattingEnabled = True
    Me.cboANZ.Location = New System.Drawing.Point(764, 8)
    Me.cboANZ.Name = "cboANZ"
    Me.cboANZ.Size = New System.Drawing.Size(80, 21)
    Me.cboANZ.TabIndex = 36
    Me.cboANZ.Visible = False
    '
    'lblANZ
    '
    Me.lblANZ.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblANZ.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblANZ.Location = New System.Drawing.Point(599, 8)
    Me.lblANZ.Name = "lblANZ"
    Me.lblANZ.Size = New System.Drawing.Size(166, 20)
    Me.lblANZ.TabIndex = 35
    Me.lblANZ.Text = "190"
    Me.lblANZ.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblANZ.Visible = False
    '
    'lblTYP
    '
    Me.lblTYP.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblTYP.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblTYP.Location = New System.Drawing.Point(379, 7)
    Me.lblTYP.Name = "lblTYP"
    Me.lblTYP.Size = New System.Drawing.Size(46, 20)
    Me.lblTYP.TabIndex = 34
    Me.lblTYP.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblTYP.Visible = False
    '
    'lblSTB
    '
    Me.lblSTB.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblSTB.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblSTB.Location = New System.Drawing.Point(307, 7)
    Me.lblSTB.Name = "lblSTB"
    Me.lblSTB.Size = New System.Drawing.Size(74, 20)
    Me.lblSTB.TabIndex = 33
    Me.lblSTB.Text = "1205"
    Me.lblSTB.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblSTB.Visible = False
    '
    'btnLOE
    '
    Me.btnLOE.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnLOE.BackColor = System.Drawing.SystemColors.Control
    Me.btnLOE.Location = New System.Drawing.Point(89, 5)
    Me.btnLOE.Name = "btnLOE"
    Me.btnLOE.Size = New System.Drawing.Size(188, 23)
    Me.btnLOE.TabIndex = 32
    Me.btnLOE.Text = "381"
    Me.btnLOE.UseVisualStyleBackColor = False
    Me.btnLOE.Visible = False
    '
    'chkPARAM
    '
    Me.chkPARAM.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.chkPARAM.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkPARAM.Location = New System.Drawing.Point(643, 3)
    Me.chkPARAM.Name = "chkPARAM"
    Me.chkPARAM.Size = New System.Drawing.Size(201, 22)
    Me.chkPARAM.TabIndex = 26
    Me.chkPARAM.Text = "415"
    Me.chkPARAM.UseVisualStyleBackColor = False
    Me.chkPARAM.Visible = False
    '
    'lblQCE
    '
    Me.lblQCE.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblQCE.BackColor = System.Drawing.Color.PaleGreen
    Me.lblQCE.Font = New System.Drawing.Font("Microsoft Sans Serif", 24.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblQCE.Location = New System.Drawing.Point(124, 222)
    Me.lblQCE.Name = "lblQCE"
    Me.lblQCE.Size = New System.Drawing.Size(727, 138)
    Me.lblQCE.TabIndex = 2
    Me.lblQCE.Text = "2002"
    Me.lblQCE.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'panResult
    '
    Me.panResult.Controls.Add(Me.splResult)
    Me.panResult.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panResult.Location = New System.Drawing.Point(0, 0)
    Me.panResult.Name = "panResult"
    Me.panResult.Size = New System.Drawing.Size(975, 605)
    Me.panResult.TabIndex = 3
    '
    'splResult
    '
    Me.splResult.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splResult.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.splResult.IsSplitterFixed = True
    Me.splResult.Location = New System.Drawing.Point(0, 0)
    Me.splResult.Name = "splResult"
    '
    'splResult.Panel1
    '
    Me.splResult.Panel1.Controls.Add(Me.splDRQ)
    '
    'splResult.Panel2
    '
    Me.splResult.Panel2.BackColor = System.Drawing.SystemColors.ControlDark
    Me.splResult.Panel2.Controls.Add(Me.splAnzeige)
    Me.splResult.Size = New System.Drawing.Size(975, 605)
    Me.splResult.SplitterDistance = 201
    Me.splResult.TabIndex = 0
    '
    'splDRQ
    '
    Me.splDRQ.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.splDRQ.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.splDRQ.Location = New System.Drawing.Point(0, 0)
    Me.splDRQ.Name = "splDRQ"
    Me.splDRQ.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splDRQ.Panel1
    '
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_11)
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_10)
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_09)
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_08)
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_07)
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_06)
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_05)
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_04)
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_03)
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_02)
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_01)
    Me.splDRQ.Panel1.Controls.Add(Me.radDRQ_00)
    '
    'splDRQ.Panel2
    '
    Me.splDRQ.Panel2.Controls.Add(Me.splDRR)
    Me.splDRQ.Size = New System.Drawing.Size(200, 662)
    Me.splDRQ.SplitterDistance = 241
    Me.splDRQ.TabIndex = 0
    '
    'radDRQ_11
    '
    Me.radDRQ_11.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_11.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_11.Location = New System.Drawing.Point(3, 220)
    Me.radDRQ_11.Name = "radDRQ_11"
    Me.radDRQ_11.Size = New System.Drawing.Size(194, 22)
    Me.radDRQ_11.TabIndex = 11
    Me.radDRQ_11.Text = "671"
    Me.radDRQ_11.UseVisualStyleBackColor = False
    Me.radDRQ_11.Visible = False
    '
    'radDRQ_10
    '
    Me.radDRQ_10.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_10.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_10.Location = New System.Drawing.Point(3, 200)
    Me.radDRQ_10.Name = "radDRQ_10"
    Me.radDRQ_10.Size = New System.Drawing.Size(194, 22)
    Me.radDRQ_10.TabIndex = 10
    Me.radDRQ_10.Text = "669"
    Me.radDRQ_10.UseVisualStyleBackColor = False
    Me.radDRQ_10.Visible = False
    '
    'radDRQ_09
    '
    Me.radDRQ_09.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_09.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_09.Location = New System.Drawing.Point(3, 180)
    Me.radDRQ_09.Name = "radDRQ_09"
    Me.radDRQ_09.Size = New System.Drawing.Size(194, 22)
    Me.radDRQ_09.TabIndex = 9
    Me.radDRQ_09.Text = "668"
    Me.radDRQ_09.UseVisualStyleBackColor = False
    '
    'radDRQ_08
    '
    Me.radDRQ_08.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_08.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_08.Location = New System.Drawing.Point(3, 160)
    Me.radDRQ_08.Name = "radDRQ_08"
    Me.radDRQ_08.Size = New System.Drawing.Size(194, 22)
    Me.radDRQ_08.TabIndex = 8
    Me.radDRQ_08.Text = "667"
    Me.radDRQ_08.UseVisualStyleBackColor = False
    '
    'radDRQ_07
    '
    Me.radDRQ_07.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_07.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_07.Location = New System.Drawing.Point(3, 140)
    Me.radDRQ_07.Name = "radDRQ_07"
    Me.radDRQ_07.Size = New System.Drawing.Size(192, 22)
    Me.radDRQ_07.TabIndex = 7
    Me.radDRQ_07.Text = "666"
    Me.radDRQ_07.UseVisualStyleBackColor = False
    '
    'radDRQ_06
    '
    Me.radDRQ_06.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_06.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_06.Location = New System.Drawing.Point(3, 120)
    Me.radDRQ_06.Name = "radDRQ_06"
    Me.radDRQ_06.Size = New System.Drawing.Size(194, 22)
    Me.radDRQ_06.TabIndex = 6
    Me.radDRQ_06.Text = "665"
    Me.radDRQ_06.UseVisualStyleBackColor = False
    '
    'radDRQ_05
    '
    Me.radDRQ_05.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_05.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_05.Location = New System.Drawing.Point(3, 100)
    Me.radDRQ_05.Name = "radDRQ_05"
    Me.radDRQ_05.Size = New System.Drawing.Size(194, 22)
    Me.radDRQ_05.TabIndex = 5
    Me.radDRQ_05.Text = "664"
    Me.radDRQ_05.UseVisualStyleBackColor = False
    '
    'radDRQ_04
    '
    Me.radDRQ_04.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_04.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_04.Location = New System.Drawing.Point(3, 80)
    Me.radDRQ_04.Name = "radDRQ_04"
    Me.radDRQ_04.Size = New System.Drawing.Size(194, 22)
    Me.radDRQ_04.TabIndex = 4
    Me.radDRQ_04.Text = "663"
    Me.radDRQ_04.UseVisualStyleBackColor = False
    '
    'radDRQ_03
    '
    Me.radDRQ_03.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_03.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_03.Location = New System.Drawing.Point(2, 60)
    Me.radDRQ_03.Name = "radDRQ_03"
    Me.radDRQ_03.Size = New System.Drawing.Size(194, 22)
    Me.radDRQ_03.TabIndex = 3
    Me.radDRQ_03.Text = "662"
    Me.radDRQ_03.UseVisualStyleBackColor = False
    '
    'radDRQ_02
    '
    Me.radDRQ_02.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_02.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_02.Location = New System.Drawing.Point(3, 40)
    Me.radDRQ_02.Name = "radDRQ_02"
    Me.radDRQ_02.Size = New System.Drawing.Size(194, 22)
    Me.radDRQ_02.TabIndex = 2
    Me.radDRQ_02.Text = "670"
    Me.radDRQ_02.UseVisualStyleBackColor = False
    '
    'radDRQ_01
    '
    Me.radDRQ_01.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_01.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_01.Location = New System.Drawing.Point(3, 20)
    Me.radDRQ_01.Name = "radDRQ_01"
    Me.radDRQ_01.Size = New System.Drawing.Size(194, 22)
    Me.radDRQ_01.TabIndex = 1
    Me.radDRQ_01.Text = "661"
    Me.radDRQ_01.UseVisualStyleBackColor = False
    '
    'radDRQ_00
    '
    Me.radDRQ_00.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radDRQ_00.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radDRQ_00.Checked = True
    Me.radDRQ_00.Location = New System.Drawing.Point(3, 0)
    Me.radDRQ_00.Name = "radDRQ_00"
    Me.radDRQ_00.Size = New System.Drawing.Size(194, 22)
    Me.radDRQ_00.TabIndex = 0
    Me.radDRQ_00.TabStop = True
    Me.radDRQ_00.Text = "660"
    Me.radDRQ_00.UseVisualStyleBackColor = False
    '
    'splDRR
    '
    Me.splDRR.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splDRR.Location = New System.Drawing.Point(0, 0)
    Me.splDRR.Name = "splDRR"
    Me.splDRR.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splDRR.Panel1
    '
    Me.splDRR.Panel1.Controls.Add(Me.splWinNLA)
    '
    'splDRR.Panel2
    '
    Me.splDRR.Panel2.Controls.Add(Me.splGPL)
    Me.splDRR.Size = New System.Drawing.Size(200, 417)
    Me.splDRR.SplitterDistance = 174
    Me.splDRR.TabIndex = 2
    '
    'splWinNLA
    '
    Me.splWinNLA.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.splWinNLA.Location = New System.Drawing.Point(0, 3)
    Me.splWinNLA.Name = "splWinNLA"
    '
    'splWinNLA.Panel1
    '
    Me.splWinNLA.Panel1.Controls.Add(Me.panWinChk)
    Me.splWinNLA.Panel1.Controls.Add(Me.panWinRad)
    '
    'splWinNLA.Panel2
    '
    Me.splWinNLA.Panel2.Controls.Add(Me.panNlaRad)
    Me.splWinNLA.Panel2.Controls.Add(Me.panNlaChk)
    Me.splWinNLA.Size = New System.Drawing.Size(200, 184)
    Me.splWinNLA.SplitterDistance = 108
    Me.splWinNLA.TabIndex = 3
    '
    'panWinChk
    '
    Me.panWinChk.Controls.Add(Me.chkWIN_08)
    Me.panWinChk.Controls.Add(Me.chkWIN_07)
    Me.panWinChk.Controls.Add(Me.chkWIN_06)
    Me.panWinChk.Controls.Add(Me.chkWIN_05)
    Me.panWinChk.Controls.Add(Me.chkWIN_04)
    Me.panWinChk.Controls.Add(Me.chkWIN_03)
    Me.panWinChk.Controls.Add(Me.chkWIN_02)
    Me.panWinChk.Controls.Add(Me.chkWIN_01)
    Me.panWinChk.Controls.Add(Me.chkWIN_00)
    Me.panWinChk.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panWinChk.Location = New System.Drawing.Point(0, 0)
    Me.panWinChk.Name = "panWinChk"
    Me.panWinChk.Size = New System.Drawing.Size(108, 184)
    Me.panWinChk.TabIndex = 0
    '
    'chkWIN_08
    '
    Me.chkWIN_08.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkWIN_08.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkWIN_08.Location = New System.Drawing.Point(0, 154)
    Me.chkWIN_08.Name = "chkWIN_08"
    Me.chkWIN_08.Size = New System.Drawing.Size(104, 19)
    Me.chkWIN_08.TabIndex = 8
    Me.chkWIN_08.UseVisualStyleBackColor = False
    '
    'chkWIN_07
    '
    Me.chkWIN_07.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkWIN_07.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkWIN_07.Location = New System.Drawing.Point(0, 136)
    Me.chkWIN_07.Name = "chkWIN_07"
    Me.chkWIN_07.Size = New System.Drawing.Size(104, 19)
    Me.chkWIN_07.TabIndex = 7
    Me.chkWIN_07.UseVisualStyleBackColor = False
    '
    'chkWIN_06
    '
    Me.chkWIN_06.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkWIN_06.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkWIN_06.Location = New System.Drawing.Point(0, 115)
    Me.chkWIN_06.Name = "chkWIN_06"
    Me.chkWIN_06.Size = New System.Drawing.Size(104, 21)
    Me.chkWIN_06.TabIndex = 6
    Me.chkWIN_06.UseVisualStyleBackColor = False
    '
    'chkWIN_05
    '
    Me.chkWIN_05.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkWIN_05.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkWIN_05.Location = New System.Drawing.Point(0, 98)
    Me.chkWIN_05.Name = "chkWIN_05"
    Me.chkWIN_05.Size = New System.Drawing.Size(104, 19)
    Me.chkWIN_05.TabIndex = 5
    Me.chkWIN_05.UseVisualStyleBackColor = False
    '
    'chkWIN_04
    '
    Me.chkWIN_04.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkWIN_04.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkWIN_04.Location = New System.Drawing.Point(0, 77)
    Me.chkWIN_04.Name = "chkWIN_04"
    Me.chkWIN_04.Size = New System.Drawing.Size(104, 21)
    Me.chkWIN_04.TabIndex = 4
    Me.chkWIN_04.UseVisualStyleBackColor = False
    '
    'chkWIN_03
    '
    Me.chkWIN_03.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkWIN_03.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkWIN_03.Location = New System.Drawing.Point(0, 60)
    Me.chkWIN_03.Name = "chkWIN_03"
    Me.chkWIN_03.Size = New System.Drawing.Size(104, 19)
    Me.chkWIN_03.TabIndex = 3
    Me.chkWIN_03.UseVisualStyleBackColor = False
    '
    'chkWIN_02
    '
    Me.chkWIN_02.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkWIN_02.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkWIN_02.Location = New System.Drawing.Point(0, 39)
    Me.chkWIN_02.Name = "chkWIN_02"
    Me.chkWIN_02.Size = New System.Drawing.Size(104, 21)
    Me.chkWIN_02.TabIndex = 2
    Me.chkWIN_02.UseVisualStyleBackColor = False
    '
    'chkWIN_01
    '
    Me.chkWIN_01.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkWIN_01.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkWIN_01.Location = New System.Drawing.Point(0, 21)
    Me.chkWIN_01.Name = "chkWIN_01"
    Me.chkWIN_01.Size = New System.Drawing.Size(104, 19)
    Me.chkWIN_01.TabIndex = 1
    Me.chkWIN_01.UseVisualStyleBackColor = False
    '
    'chkWIN_00
    '
    Me.chkWIN_00.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkWIN_00.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkWIN_00.Location = New System.Drawing.Point(0, 0)
    Me.chkWIN_00.Name = "chkWIN_00"
    Me.chkWIN_00.Size = New System.Drawing.Size(104, 21)
    Me.chkWIN_00.TabIndex = 0
    Me.chkWIN_00.UseVisualStyleBackColor = False
    '
    'panWinRad
    '
    Me.panWinRad.Controls.Add(Me.radWIN_08)
    Me.panWinRad.Controls.Add(Me.radWIN_07)
    Me.panWinRad.Controls.Add(Me.radWIN_06)
    Me.panWinRad.Controls.Add(Me.radWIN_05)
    Me.panWinRad.Controls.Add(Me.radWIN_04)
    Me.panWinRad.Controls.Add(Me.radWIN_03)
    Me.panWinRad.Controls.Add(Me.radWIN_02)
    Me.panWinRad.Controls.Add(Me.radWIN_01)
    Me.panWinRad.Controls.Add(Me.radWIN_00)
    Me.panWinRad.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panWinRad.Location = New System.Drawing.Point(0, 0)
    Me.panWinRad.Name = "panWinRad"
    Me.panWinRad.Size = New System.Drawing.Size(108, 184)
    Me.panWinRad.TabIndex = 1
    '
    'radWIN_08
    '
    Me.radWIN_08.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radWIN_08.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radWIN_08.Location = New System.Drawing.Point(3, 149)
    Me.radWIN_08.Name = "radWIN_08"
    Me.radWIN_08.Size = New System.Drawing.Size(48, 22)
    Me.radWIN_08.TabIndex = 24
    Me.radWIN_08.UseVisualStyleBackColor = False
    '
    'radWIN_07
    '
    Me.radWIN_07.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radWIN_07.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radWIN_07.Location = New System.Drawing.Point(3, 130)
    Me.radWIN_07.Name = "radWIN_07"
    Me.radWIN_07.Size = New System.Drawing.Size(48, 22)
    Me.radWIN_07.TabIndex = 23
    Me.radWIN_07.UseVisualStyleBackColor = False
    '
    'radWIN_06
    '
    Me.radWIN_06.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radWIN_06.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radWIN_06.Location = New System.Drawing.Point(3, 112)
    Me.radWIN_06.Name = "radWIN_06"
    Me.radWIN_06.Size = New System.Drawing.Size(48, 22)
    Me.radWIN_06.TabIndex = 22
    Me.radWIN_06.UseVisualStyleBackColor = False
    '
    'radWIN_05
    '
    Me.radWIN_05.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radWIN_05.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radWIN_05.Location = New System.Drawing.Point(3, 93)
    Me.radWIN_05.Name = "radWIN_05"
    Me.radWIN_05.Size = New System.Drawing.Size(48, 22)
    Me.radWIN_05.TabIndex = 21
    Me.radWIN_05.UseVisualStyleBackColor = False
    '
    'radWIN_04
    '
    Me.radWIN_04.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radWIN_04.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radWIN_04.Location = New System.Drawing.Point(3, 74)
    Me.radWIN_04.Name = "radWIN_04"
    Me.radWIN_04.Size = New System.Drawing.Size(48, 22)
    Me.radWIN_04.TabIndex = 20
    Me.radWIN_04.UseVisualStyleBackColor = False
    '
    'radWIN_03
    '
    Me.radWIN_03.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radWIN_03.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radWIN_03.Location = New System.Drawing.Point(3, 56)
    Me.radWIN_03.Name = "radWIN_03"
    Me.radWIN_03.Size = New System.Drawing.Size(48, 22)
    Me.radWIN_03.TabIndex = 19
    Me.radWIN_03.UseVisualStyleBackColor = False
    '
    'radWIN_02
    '
    Me.radWIN_02.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radWIN_02.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radWIN_02.Location = New System.Drawing.Point(3, 37)
    Me.radWIN_02.Name = "radWIN_02"
    Me.radWIN_02.Size = New System.Drawing.Size(48, 22)
    Me.radWIN_02.TabIndex = 18
    Me.radWIN_02.UseVisualStyleBackColor = False
    '
    'radWIN_01
    '
    Me.radWIN_01.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radWIN_01.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radWIN_01.Location = New System.Drawing.Point(3, 18)
    Me.radWIN_01.Name = "radWIN_01"
    Me.radWIN_01.Size = New System.Drawing.Size(48, 22)
    Me.radWIN_01.TabIndex = 17
    Me.radWIN_01.UseVisualStyleBackColor = False
    '
    'radWIN_00
    '
    Me.radWIN_00.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radWIN_00.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radWIN_00.Checked = True
    Me.radWIN_00.Location = New System.Drawing.Point(3, -1)
    Me.radWIN_00.Name = "radWIN_00"
    Me.radWIN_00.Size = New System.Drawing.Size(48, 22)
    Me.radWIN_00.TabIndex = 16
    Me.radWIN_00.TabStop = True
    Me.radWIN_00.UseVisualStyleBackColor = False
    '
    'panNlaRad
    '
    Me.panNlaRad.Controls.Add(Me.radNLA_4)
    Me.panNlaRad.Controls.Add(Me.radNLA_3)
    Me.panNlaRad.Controls.Add(Me.radNLA_2)
    Me.panNlaRad.Controls.Add(Me.radNLA_1)
    Me.panNlaRad.Controls.Add(Me.radNLA_0)
    Me.panNlaRad.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panNlaRad.Location = New System.Drawing.Point(0, 0)
    Me.panNlaRad.Name = "panNlaRad"
    Me.panNlaRad.Size = New System.Drawing.Size(88, 184)
    Me.panNlaRad.TabIndex = 1
    '
    'radNLA_4
    '
    Me.radNLA_4.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radNLA_4.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radNLA_4.Location = New System.Drawing.Point(3, 77)
    Me.radNLA_4.Name = "radNLA_4"
    Me.radNLA_4.Size = New System.Drawing.Size(81, 22)
    Me.radNLA_4.TabIndex = 25
    Me.radNLA_4.UseVisualStyleBackColor = False
    '
    'radNLA_3
    '
    Me.radNLA_3.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radNLA_3.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radNLA_3.Location = New System.Drawing.Point(3, 57)
    Me.radNLA_3.Name = "radNLA_3"
    Me.radNLA_3.Size = New System.Drawing.Size(81, 22)
    Me.radNLA_3.TabIndex = 24
    Me.radNLA_3.UseVisualStyleBackColor = False
    '
    'radNLA_2
    '
    Me.radNLA_2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radNLA_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radNLA_2.Location = New System.Drawing.Point(3, 39)
    Me.radNLA_2.Name = "radNLA_2"
    Me.radNLA_2.Size = New System.Drawing.Size(81, 22)
    Me.radNLA_2.TabIndex = 23
    Me.radNLA_2.UseVisualStyleBackColor = False
    '
    'radNLA_1
    '
    Me.radNLA_1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radNLA_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radNLA_1.Location = New System.Drawing.Point(3, 18)
    Me.radNLA_1.Name = "radNLA_1"
    Me.radNLA_1.Size = New System.Drawing.Size(81, 22)
    Me.radNLA_1.TabIndex = 22
    Me.radNLA_1.UseVisualStyleBackColor = False
    '
    'radNLA_0
    '
    Me.radNLA_0.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radNLA_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radNLA_0.Checked = True
    Me.radNLA_0.Location = New System.Drawing.Point(3, -1)
    Me.radNLA_0.Name = "radNLA_0"
    Me.radNLA_0.Size = New System.Drawing.Size(81, 22)
    Me.radNLA_0.TabIndex = 21
    Me.radNLA_0.TabStop = True
    Me.radNLA_0.UseVisualStyleBackColor = False
    '
    'panNlaChk
    '
    Me.panNlaChk.Controls.Add(Me.chkNLA_4)
    Me.panNlaChk.Controls.Add(Me.chkNLA_3)
    Me.panNlaChk.Controls.Add(Me.chkNLA_2)
    Me.panNlaChk.Controls.Add(Me.chkNLA_1)
    Me.panNlaChk.Controls.Add(Me.chkNLA_0)
    Me.panNlaChk.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panNlaChk.Location = New System.Drawing.Point(0, 0)
    Me.panNlaChk.Name = "panNlaChk"
    Me.panNlaChk.Size = New System.Drawing.Size(88, 184)
    Me.panNlaChk.TabIndex = 0
    '
    'chkNLA_4
    '
    Me.chkNLA_4.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkNLA_4.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkNLA_4.Location = New System.Drawing.Point(0, 77)
    Me.chkNLA_4.Name = "chkNLA_4"
    Me.chkNLA_4.Size = New System.Drawing.Size(88, 21)
    Me.chkNLA_4.TabIndex = 5
    Me.chkNLA_4.UseVisualStyleBackColor = False
    '
    'chkNLA_3
    '
    Me.chkNLA_3.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkNLA_3.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkNLA_3.Location = New System.Drawing.Point(0, 57)
    Me.chkNLA_3.Name = "chkNLA_3"
    Me.chkNLA_3.Size = New System.Drawing.Size(85, 21)
    Me.chkNLA_3.TabIndex = 4
    Me.chkNLA_3.UseVisualStyleBackColor = False
    '
    'chkNLA_2
    '
    Me.chkNLA_2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkNLA_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkNLA_2.Location = New System.Drawing.Point(0, 37)
    Me.chkNLA_2.Name = "chkNLA_2"
    Me.chkNLA_2.Size = New System.Drawing.Size(88, 21)
    Me.chkNLA_2.TabIndex = 3
    Me.chkNLA_2.UseVisualStyleBackColor = False
    '
    'chkNLA_1
    '
    Me.chkNLA_1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkNLA_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkNLA_1.Location = New System.Drawing.Point(0, 19)
    Me.chkNLA_1.Name = "chkNLA_1"
    Me.chkNLA_1.Size = New System.Drawing.Size(85, 21)
    Me.chkNLA_1.TabIndex = 2
    Me.chkNLA_1.UseVisualStyleBackColor = False
    '
    'chkNLA_0
    '
    Me.chkNLA_0.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkNLA_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkNLA_0.Location = New System.Drawing.Point(0, -1)
    Me.chkNLA_0.Name = "chkNLA_0"
    Me.chkNLA_0.Size = New System.Drawing.Size(85, 21)
    Me.chkNLA_0.TabIndex = 1
    Me.chkNLA_0.UseVisualStyleBackColor = False
    '
    'splGPL
    '
    Me.splGPL.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.splGPL.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.splGPL.Location = New System.Drawing.Point(2, 5)
    Me.splGPL.Name = "splGPL"
    Me.splGPL.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splGPL.Panel1
    '
    Me.splGPL.Panel1.Controls.Add(Me.radGPL_4)
    Me.splGPL.Panel1.Controls.Add(Me.radGPL_3)
    Me.splGPL.Panel1.Controls.Add(Me.radGPL_2)
    Me.splGPL.Panel1.Controls.Add(Me.radGPL_1)
    Me.splGPL.Panel1.Controls.Add(Me.radGPL_0)
    '
    'splGPL.Panel2
    '
    Me.splGPL.Panel2.Controls.Add(Me.chkLinear)
    Me.splGPL.Panel2.Controls.Add(Me.cboSKAL)
    Me.splGPL.Size = New System.Drawing.Size(200, 176)
    Me.splGPL.SplitterDistance = 106
    Me.splGPL.TabIndex = 2
    '
    'radGPL_4
    '
    Me.radGPL_4.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radGPL_4.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radGPL_4.Location = New System.Drawing.Point(3, 78)
    Me.radGPL_4.Name = "radGPL_4"
    Me.radGPL_4.Size = New System.Drawing.Size(192, 22)
    Me.radGPL_4.TabIndex = 25
    Me.radGPL_4.Text = "526"
    Me.radGPL_4.UseVisualStyleBackColor = False
    '
    'radGPL_3
    '
    Me.radGPL_3.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radGPL_3.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radGPL_3.Location = New System.Drawing.Point(3, 59)
    Me.radGPL_3.Name = "radGPL_3"
    Me.radGPL_3.Size = New System.Drawing.Size(192, 22)
    Me.radGPL_3.TabIndex = 24
    Me.radGPL_3.Text = "529"
    Me.radGPL_3.UseVisualStyleBackColor = False
    '
    'radGPL_2
    '
    Me.radGPL_2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radGPL_2.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radGPL_2.Location = New System.Drawing.Point(3, 40)
    Me.radGPL_2.Name = "radGPL_2"
    Me.radGPL_2.Size = New System.Drawing.Size(192, 22)
    Me.radGPL_2.TabIndex = 23
    Me.radGPL_2.Text = "528"
    Me.radGPL_2.UseVisualStyleBackColor = False
    '
    'radGPL_1
    '
    Me.radGPL_1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radGPL_1.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radGPL_1.Checked = True
    Me.radGPL_1.Location = New System.Drawing.Point(3, 22)
    Me.radGPL_1.Name = "radGPL_1"
    Me.radGPL_1.Size = New System.Drawing.Size(192, 22)
    Me.radGPL_1.TabIndex = 22
    Me.radGPL_1.TabStop = True
    Me.radGPL_1.Text = "527"
    Me.radGPL_1.UseVisualStyleBackColor = False
    '
    'radGPL_0
    '
    Me.radGPL_0.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radGPL_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radGPL_0.Location = New System.Drawing.Point(3, 2)
    Me.radGPL_0.Name = "radGPL_0"
    Me.radGPL_0.Size = New System.Drawing.Size(192, 22)
    Me.radGPL_0.TabIndex = 21
    Me.radGPL_0.Text = "997"
    Me.radGPL_0.UseVisualStyleBackColor = False
    '
    'chkLinear
    '
    Me.chkLinear.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.chkLinear.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkLinear.Location = New System.Drawing.Point(3, 30)
    Me.chkLinear.Name = "chkLinear"
    Me.chkLinear.Size = New System.Drawing.Size(195, 19)
    Me.chkLinear.TabIndex = 0
    Me.chkLinear.Text = "532"
    Me.chkLinear.UseVisualStyleBackColor = False
    Me.chkLinear.Visible = False
    '
    'cboSKAL
    '
    Me.cboSKAL.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.cboSKAL.Enabled = False
    Me.cboSKAL.FormattingEnabled = True
    Me.cboSKAL.Location = New System.Drawing.Point(2, 3)
    Me.cboSKAL.Name = "cboSKAL"
    Me.cboSKAL.Size = New System.Drawing.Size(195, 21)
    Me.cboSKAL.TabIndex = 1
    Me.cboSKAL.Visible = False
    '
    'splAnzeige
    '
    Me.splAnzeige.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splAnzeige.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.splAnzeige.IsSplitterFixed = True
    Me.splAnzeige.Location = New System.Drawing.Point(0, 0)
    Me.splAnzeige.Name = "splAnzeige"
    Me.splAnzeige.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splAnzeige.Panel1
    '
    Me.splAnzeige.Panel1.BackColor = System.Drawing.SystemColors.ControlDark
    Me.splAnzeige.Panel1.Controls.Add(Me.chkDru)
    Me.splAnzeige.Panel1.Controls.Add(Me.btnUser)
    Me.splAnzeige.Panel1.Controls.Add(Me.btnKOP)
    Me.splAnzeige.Panel1.Controls.Add(Me.btnDRU)
    Me.splAnzeige.Panel1.Controls.Add(Me.btnABR)
    Me.splAnzeige.Panel1.Controls.Add(Me.btnVOR)
    Me.splAnzeige.Panel1.Controls.Add(Me.btnGRF)
    Me.splAnzeige.Panel1.Controls.Add(Me.btnAUS)
    '
    'splAnzeige.Panel2
    '
    Me.splAnzeige.Panel2.Controls.Add(Me.flgMerk)
    Me.splAnzeige.Panel2.Controls.Add(Me.picDRQ)
    Me.splAnzeige.Panel2.Controls.Add(Me.picLAB)
    Me.splAnzeige.Panel2.Controls.Add(Me.flgRwert)
    Me.splAnzeige.Panel2.Controls.Add(Me.picXYZ)
    Me.splAnzeige.Size = New System.Drawing.Size(770, 605)
    Me.splAnzeige.SplitterDistance = 80
    Me.splAnzeige.TabIndex = 0
    '
    'chkDru
    '
    Me.chkDru.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDru.BackColor = System.Drawing.SystemColors.Control
    Me.chkDru.Location = New System.Drawing.Point(57, 49)
    Me.chkDru.Name = "chkDru"
    Me.chkDru.Size = New System.Drawing.Size(14, 18)
    Me.chkDru.TabIndex = 6
    Me.chkDru.UseVisualStyleBackColor = False
    '
    'btnUser
    '
    Me.btnUser.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnUser.BackColor = System.Drawing.SystemColors.Control
    Me.btnUser.Location = New System.Drawing.Point(502, 49)
    Me.btnUser.Name = "btnUser"
    Me.btnUser.Size = New System.Drawing.Size(168, 21)
    Me.btnUser.TabIndex = 5
    Me.btnUser.Text = "392"
    Me.btnUser.UseVisualStyleBackColor = False
    '
    'btnKOP
    '
    Me.btnKOP.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnKOP.BackColor = System.Drawing.SystemColors.Control
    Me.btnKOP.Location = New System.Drawing.Point(281, 49)
    Me.btnKOP.Name = "btnKOP"
    Me.btnKOP.Size = New System.Drawing.Size(168, 21)
    Me.btnKOP.TabIndex = 4
    Me.btnKOP.Text = "396"
    Me.btnKOP.UseVisualStyleBackColor = False
    '
    'btnDRU
    '
    Me.btnDRU.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnDRU.BackColor = System.Drawing.SystemColors.Control
    Me.btnDRU.Location = New System.Drawing.Point(77, 49)
    Me.btnDRU.Name = "btnDRU"
    Me.btnDRU.Size = New System.Drawing.Size(168, 21)
    Me.btnDRU.TabIndex = 3
    Me.btnDRU.Text = "397"
    Me.btnDRU.UseVisualStyleBackColor = False
    '
    'btnABR
    '
    Me.btnABR.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnABR.BackColor = System.Drawing.SystemColors.Control
    Me.btnABR.Location = New System.Drawing.Point(502, 10)
    Me.btnABR.Name = "btnABR"
    Me.btnABR.Size = New System.Drawing.Size(168, 21)
    Me.btnABR.TabIndex = 2
    Me.btnABR.Text = "379"
    Me.btnABR.UseVisualStyleBackColor = False
    '
    'btnVOR
    '
    Me.btnVOR.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnVOR.BackColor = System.Drawing.SystemColors.Control
    Me.btnVOR.Location = New System.Drawing.Point(281, 10)
    Me.btnVOR.Name = "btnVOR"
    Me.btnVOR.Size = New System.Drawing.Size(168, 21)
    Me.btnVOR.TabIndex = 1
    Me.btnVOR.Text = "391"
    Me.btnVOR.UseVisualStyleBackColor = False
    '
    'btnGRF
    '
    Me.btnGRF.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnGRF.BackColor = System.Drawing.SystemColors.Control
    Me.btnGRF.Location = New System.Drawing.Point(77, 10)
    Me.btnGRF.Name = "btnGRF"
    Me.btnGRF.Size = New System.Drawing.Size(168, 21)
    Me.btnGRF.TabIndex = 0
    Me.btnGRF.Text = "393"
    Me.btnGRF.UseVisualStyleBackColor = False
    '
    'btnAUS
    '
    Me.btnAUS.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnAUS.BackColor = System.Drawing.SystemColors.Control
    Me.btnAUS.Location = New System.Drawing.Point(502, 49)
    Me.btnAUS.Name = "btnAUS"
    Me.btnAUS.Size = New System.Drawing.Size(168, 21)
    Me.btnAUS.TabIndex = 7
    Me.btnAUS.Text = "395"
    Me.btnAUS.UseVisualStyleBackColor = False
    Me.btnAUS.Visible = False
    '
    'flgMerk
    '
    Me.flgMerk.AllowUserToAddRows = False
    Me.flgMerk.AllowUserToDeleteRows = False
    Me.flgMerk.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.flgMerk.ColumnHeadersVisible = False
    Me.flgMerk.Dock = System.Windows.Forms.DockStyle.Fill
    Me.flgMerk.Location = New System.Drawing.Point(0, 0)
    Me.flgMerk.Name = "flgMerk"
    Me.flgMerk.RowTemplate.Height = 24
    Me.flgMerk.Size = New System.Drawing.Size(770, 521)
    Me.flgMerk.TabIndex = 0
    '
    'picDRQ
    '
    Me.picDRQ.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.picDRQ.Dock = System.Windows.Forms.DockStyle.Fill
    Me.picDRQ.Location = New System.Drawing.Point(0, 0)
    Me.picDRQ.Name = "picDRQ"
    Me.picDRQ.Size = New System.Drawing.Size(770, 521)
    Me.picDRQ.TabIndex = 2
    Me.picDRQ.TabStop = False
    '
    'picLAB
    '
    Me.picLAB.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.picLAB.Dock = System.Windows.Forms.DockStyle.Fill
    Me.picLAB.Location = New System.Drawing.Point(0, 0)
    Me.picLAB.Name = "picLAB"
    Me.picLAB.Size = New System.Drawing.Size(770, 521)
    Me.picLAB.TabIndex = 1
    Me.picLAB.TabStop = False
    '
    'flgRwert
    '
    Me.flgRwert.AllowUserToAddRows = False
    Me.flgRwert.AllowUserToDeleteRows = False
    Me.flgRwert.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.flgRwert.Dock = System.Windows.Forms.DockStyle.Fill
    Me.flgRwert.Location = New System.Drawing.Point(0, 0)
    Me.flgRwert.Name = "flgRwert"
    Me.flgRwert.RowTemplate.Height = 24
    Me.flgRwert.Size = New System.Drawing.Size(770, 521)
    Me.flgRwert.TabIndex = 4
    '
    'picXYZ
    '
    Me.picXYZ.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.picXYZ.Dock = System.Windows.Forms.DockStyle.Fill
    Me.picXYZ.Location = New System.Drawing.Point(0, 0)
    Me.picXYZ.Name = "picXYZ"
    Me.picXYZ.Size = New System.Drawing.Size(770, 521)
    Me.picXYZ.TabIndex = 3
    Me.picXYZ.TabStop = False
    '
    'frmQualControl
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(975, 605)
    Me.ControlBox = False
    Me.Controls.Add(Me.PanQual)
    Me.Controls.Add(Me.panResult)
    Me.Controls.Add(Me.lblQCE)
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmQualControl"
    Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
    Me.PanQual.ResumeLayout(False)
    Me.splQual.Panel1.ResumeLayout(False)
    Me.splQual.Panel2.ResumeLayout(False)
    CType(Me.splQual, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splQual.ResumeLayout(False)
    Me.panStandard.ResumeLayout(False)
    Me.panStandard.PerformLayout()
    Me.panNextWIN.ResumeLayout(False)
    Me.panAuxiliary.ResumeLayout(False)
    CType(Me.dbgFarbwerte, System.ComponentModel.ISupportInitialize).EndInit()
    Me.panANGL.ResumeLayout(False)
    Me.splWerte.Panel1.ResumeLayout(False)
    Me.splWerte.Panel2.ResumeLayout(False)
    CType(Me.splWerte, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splWerte.ResumeLayout(False)
    CType(Me.TDBGrid, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ContextTDBGrid.ResumeLayout(False)
    CType(Me.TDBParameter, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.TDBDropParameter, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splParameter.Panel1.ResumeLayout(False)
    Me.splParameter.Panel1.PerformLayout()
    Me.splParameter.Panel2.ResumeLayout(False)
    CType(Me.splParameter, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splParameter.ResumeLayout(False)
    Me.panResult.ResumeLayout(False)
    Me.splResult.Panel1.ResumeLayout(False)
    Me.splResult.Panel2.ResumeLayout(False)
    CType(Me.splResult, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splResult.ResumeLayout(False)
    Me.splDRQ.Panel1.ResumeLayout(False)
    Me.splDRQ.Panel2.ResumeLayout(False)
    CType(Me.splDRQ, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splDRQ.ResumeLayout(False)
    Me.splDRR.Panel1.ResumeLayout(False)
    Me.splDRR.Panel2.ResumeLayout(False)
    CType(Me.splDRR, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splDRR.ResumeLayout(False)
    Me.splWinNLA.Panel1.ResumeLayout(False)
    Me.splWinNLA.Panel2.ResumeLayout(False)
    CType(Me.splWinNLA, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splWinNLA.ResumeLayout(False)
    Me.panWinChk.ResumeLayout(False)
    Me.panWinRad.ResumeLayout(False)
    Me.panNlaRad.ResumeLayout(False)
    Me.panNlaChk.ResumeLayout(False)
    Me.splGPL.Panel1.ResumeLayout(False)
    Me.splGPL.Panel2.ResumeLayout(False)
    CType(Me.splGPL, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splGPL.ResumeLayout(False)
    Me.splAnzeige.Panel1.ResumeLayout(False)
    Me.splAnzeige.Panel2.ResumeLayout(False)
    CType(Me.splAnzeige, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splAnzeige.ResumeLayout(false)
    CType(Me.flgMerk,System.ComponentModel.ISupportInitialize).EndInit
    CType(Me.picDRQ,System.ComponentModel.ISupportInitialize).EndInit
    CType(Me.picLAB,System.ComponentModel.ISupportInitialize).EndInit
    CType(Me.flgRwert,System.ComponentModel.ISupportInitialize).EndInit
    CType(Me.picXYZ,System.ComponentModel.ISupportInitialize).EndInit
    Me.ResumeLayout(false)

End Sub
  Friend WithEvents PanQual As System.Windows.Forms.Panel
  Friend WithEvents splQual As System.Windows.Forms.SplitContainer
  Friend WithEvents splWerte As System.Windows.Forms.SplitContainer
  Friend WithEvents chkPARAM As System.Windows.Forms.CheckBox
  Friend WithEvents cboWEL As System.Windows.Forms.ComboBox
  Friend WithEvents cboBWE As System.Windows.Forms.ComboBox
  Friend WithEvents lblWEL As System.Windows.Forms.Label
  Friend WithEvents btnBWE As System.Windows.Forms.Button
  Friend WithEvents btnVER As System.Windows.Forms.Button
  Friend WithEvents lblREF_5 As System.Windows.Forms.Label
  Friend WithEvents lblREF_2 As System.Windows.Forms.Label
  Friend WithEvents lblREF_1 As System.Windows.Forms.Label
  Friend WithEvents btnREF_5 As System.Windows.Forms.Button
  Friend WithEvents btnREF_2 As System.Windows.Forms.Button
  Friend WithEvents btnREF_1 As System.Windows.Forms.Button
  Friend WithEvents btnREF_0 As System.Windows.Forms.Button
  Friend WithEvents txtGRA As System.Windows.Forms.TextBox
  Friend WithEvents lblGRA As System.Windows.Forms.Label
  Friend WithEvents txtKDE As System.Windows.Forms.ComboBox
  Friend WithEvents lblKDE As System.Windows.Forms.Label
  Friend WithEvents cboIND As System.Windows.Forms.ComboBox
  Friend WithEvents lblIND As System.Windows.Forms.Label
  Friend WithEvents cboINR As System.Windows.Forms.ComboBox
  Friend WithEvents lblINR As System.Windows.Forms.Label
  Friend WithEvents cboGLZ As System.Windows.Forms.ComboBox
  Friend WithEvents lblGLZ As System.Windows.Forms.Label
  Friend WithEvents btnREF_3 As System.Windows.Forms.Button
  Friend WithEvents btnREF_4 As System.Windows.Forms.Button
  Friend WithEvents lblREF_3 As System.Windows.Forms.Label
  Friend WithEvents lblREF_4 As System.Windows.Forms.Label
  Friend WithEvents TDBGrid As System.Windows.Forms.DataGridView
  Friend WithEvents lblQCE As System.Windows.Forms.Label
  Friend WithEvents panResult As System.Windows.Forms.Panel
  Friend WithEvents splResult As System.Windows.Forms.SplitContainer
  Friend WithEvents splDRQ As System.Windows.Forms.SplitContainer
  Friend WithEvents radDRQ_10 As System.Windows.Forms.RadioButton
  Friend WithEvents radDRQ_09 As System.Windows.Forms.RadioButton
  Friend WithEvents radDRQ_08 As System.Windows.Forms.RadioButton
  Friend WithEvents radDRQ_07 As System.Windows.Forms.RadioButton
  Friend WithEvents radDRQ_06 As System.Windows.Forms.RadioButton
  Friend WithEvents radDRQ_05 As System.Windows.Forms.RadioButton
  Friend WithEvents radDRQ_04 As System.Windows.Forms.RadioButton
  Friend WithEvents radDRQ_03 As System.Windows.Forms.RadioButton
  Friend WithEvents radDRQ_02 As System.Windows.Forms.RadioButton
  Friend WithEvents radDRQ_01 As System.Windows.Forms.RadioButton
  Friend WithEvents radDRQ_00 As System.Windows.Forms.RadioButton
  Friend WithEvents splAnzeige As System.Windows.Forms.SplitContainer
  Friend WithEvents btnUser As System.Windows.Forms.Button
  Friend WithEvents btnKOP As System.Windows.Forms.Button
  Friend WithEvents btnDRU As System.Windows.Forms.Button
  Friend WithEvents btnABR As System.Windows.Forms.Button
  Friend WithEvents btnVOR As System.Windows.Forms.Button
  Friend WithEvents btnGRF As System.Windows.Forms.Button
  Friend WithEvents flgMerk As System.Windows.Forms.DataGridView
  Friend WithEvents picDRQ As System.Windows.Forms.PictureBox
  Friend WithEvents picLAB As System.Windows.Forms.PictureBox
  Friend WithEvents splParameter As System.Windows.Forms.SplitContainer
  Friend WithEvents txtKOM As System.Windows.Forms.TextBox
  Friend WithEvents lblKOM As System.Windows.Forms.Label
  Friend WithEvents cboMiscel_1 As System.Windows.Forms.ComboBox
  Friend WithEvents cboMiscel_0 As System.Windows.Forms.ComboBox
  Friend WithEvents chkABS As System.Windows.Forms.CheckBox
  Friend WithEvents cboFST As System.Windows.Forms.ComboBox
  Friend WithEvents cboANZ As System.Windows.Forms.ComboBox
  Friend WithEvents lblANZ As System.Windows.Forms.Label
  Friend WithEvents lblTYP As System.Windows.Forms.Label
  Friend WithEvents lblSTB As System.Windows.Forms.Label
  Friend WithEvents btnLOE As System.Windows.Forms.Button
  Friend WithEvents ContextTDBGrid As System.Windows.Forms.ContextMenuStrip
  Friend WithEvents mnuCopy As System.Windows.Forms.ToolStripMenuItem
  Friend WithEvents mnuPaste As System.Windows.Forms.ToolStripMenuItem
  Friend WithEvents mnuDelete As System.Windows.Forms.ToolStripMenuItem
  Friend WithEvents splDRR As System.Windows.Forms.SplitContainer
  Friend WithEvents splWinNLA As System.Windows.Forms.SplitContainer
  Friend WithEvents splGPL As System.Windows.Forms.SplitContainer
  Friend WithEvents radGPL_4 As System.Windows.Forms.RadioButton
  Friend WithEvents radGPL_3 As System.Windows.Forms.RadioButton
  Friend WithEvents radGPL_2 As System.Windows.Forms.RadioButton
  Friend WithEvents radGPL_1 As System.Windows.Forms.RadioButton
  Friend WithEvents radGPL_0 As System.Windows.Forms.RadioButton
  Friend WithEvents cboSKAL As System.Windows.Forms.ComboBox
  Friend WithEvents chkLinear As System.Windows.Forms.CheckBox
  Friend WithEvents panWinChk As System.Windows.Forms.Panel
  Friend WithEvents chkWIN_00 As System.Windows.Forms.CheckBox
  Friend WithEvents panWinRad As System.Windows.Forms.Panel
  Friend WithEvents radWIN_08 As System.Windows.Forms.RadioButton
  Friend WithEvents radWIN_07 As System.Windows.Forms.RadioButton
  Friend WithEvents radWIN_06 As System.Windows.Forms.RadioButton
  Friend WithEvents radWIN_05 As System.Windows.Forms.RadioButton
  Friend WithEvents radWIN_04 As System.Windows.Forms.RadioButton
  Friend WithEvents radWIN_03 As System.Windows.Forms.RadioButton
  Friend WithEvents radWIN_02 As System.Windows.Forms.RadioButton
  Friend WithEvents radWIN_01 As System.Windows.Forms.RadioButton
  Friend WithEvents radWIN_00 As System.Windows.Forms.RadioButton
  Friend WithEvents chkWIN_08 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_07 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_06 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_05 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_04 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_03 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_02 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_01 As System.Windows.Forms.CheckBox
  Friend WithEvents panNlaRad As System.Windows.Forms.Panel
  Friend WithEvents radNLA_4 As System.Windows.Forms.RadioButton
  Friend WithEvents radNLA_3 As System.Windows.Forms.RadioButton
  Friend WithEvents radNLA_2 As System.Windows.Forms.RadioButton
  Friend WithEvents radNLA_1 As System.Windows.Forms.RadioButton
  Friend WithEvents radNLA_0 As System.Windows.Forms.RadioButton
  Friend WithEvents panNlaChk As System.Windows.Forms.Panel
  Friend WithEvents chkNLA_4 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNLA_3 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNLA_2 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNLA_1 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNLA_0 As System.Windows.Forms.CheckBox
  Friend WithEvents picXYZ As System.Windows.Forms.PictureBox
  Friend WithEvents flgRwert As System.Windows.Forms.DataGridView
  Friend WithEvents chkDru As System.Windows.Forms.CheckBox
  Friend WithEvents cboGRP As System.Windows.Forms.ComboBox
  Friend WithEvents lblGRP As System.Windows.Forms.Label
  Friend WithEvents txtANZSuch As System.Windows.Forms.TextBox
  Friend WithEvents lblANZSuch As System.Windows.Forms.Label
  Friend WithEvents btnAUS As System.Windows.Forms.Button
  Friend WithEvents TDBParameter As C1.Win.C1TrueDBGrid.C1TrueDBGrid
  Friend WithEvents TDBDropParameter As C1.Win.C1TrueDBGrid.C1TrueDBDropdown
  Friend WithEvents lblMiscel_1 As System.Windows.Forms.Label
  Friend WithEvents lblMiscel_0 As System.Windows.Forms.Label
  Friend WithEvents radDRQ_11 As System.Windows.Forms.RadioButton
  Friend WithEvents panStandard As System.Windows.Forms.Panel
  Friend WithEvents panAuxiliary As System.Windows.Forms.Panel
  Friend WithEvents panANGL As System.Windows.Forms.Panel
  Friend WithEvents radANGL_1 As System.Windows.Forms.RadioButton
  Friend WithEvents radANGL_0 As System.Windows.Forms.RadioButton
  Friend WithEvents chkLAB As System.Windows.Forms.CheckBox
  Friend WithEvents btnVERAux As System.Windows.Forms.Button
  Friend WithEvents btnCalcRwerte As System.Windows.Forms.Button
  Friend WithEvents cboINDAux As System.Windows.Forms.ComboBox
  Friend WithEvents lblINDAux As System.Windows.Forms.Label
  Friend WithEvents cboINRAux As System.Windows.Forms.ComboBox
  Friend WithEvents lblINRAux As System.Windows.Forms.Label
  Friend WithEvents cboGLZAux As System.Windows.Forms.ComboBox
  Friend WithEvents lblGLZAux As System.Windows.Forms.Label
  Friend WithEvents lblTRA As System.Windows.Forms.Label
  Friend WithEvents txtTra As System.Windows.Forms.TextBox
  Friend WithEvents dbgFarbwerte As System.Windows.Forms.DataGridView
  Friend WithEvents panNextWIN As System.Windows.Forms.Panel
  Friend WithEvents chkNextWIN_08 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNextWIN_07 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNextWIN_06 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNextWIN_05 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNextWIN_04 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNextWIN_03 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNextWIN_02 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNextWIN_01 As System.Windows.Forms.CheckBox
  Friend WithEvents chkNextWIN_00 As System.Windows.Forms.CheckBox
End Class
