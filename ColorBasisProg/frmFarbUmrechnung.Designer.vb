<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmFarbUmrechnung
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
    Me.SplitContainFarbUmrechnung = New System.Windows.Forms.SplitContainer()
    Me.chkRwerte = New System.Windows.Forms.CheckBox()
    Me.lstCMYK = New System.Windows.Forms.ListBox()
    Me.panANGL = New System.Windows.Forms.Panel()
    Me.radANGL_1 = New System.Windows.Forms.RadioButton()
    Me.radANGL_0 = New System.Windows.Forms.RadioButton()
    Me.panKURV = New System.Windows.Forms.Panel()
    Me.radKURV_2 = New System.Windows.Forms.RadioButton()
    Me.radKURV_1 = New System.Windows.Forms.RadioButton()
    Me.radKURV_0 = New System.Windows.Forms.RadioButton()
    Me.lblREF_2 = New System.Windows.Forms.Label()
    Me.lblREF_0 = New System.Windows.Forms.Label()
    Me.btnREF_2 = New System.Windows.Forms.Button()
    Me.btnREF_0 = New System.Windows.Forms.Button()
    Me.panOptKODIF = New System.Windows.Forms.Panel()
    Me.radKODIF_8 = New System.Windows.Forms.RadioButton()
    Me.radKODIF_7 = New System.Windows.Forms.RadioButton()
    Me.radKODIF_6 = New System.Windows.Forms.RadioButton()
    Me.radKODIF_5 = New System.Windows.Forms.RadioButton()
    Me.radKODIF_4 = New System.Windows.Forms.RadioButton()
    Me.radKODIF_3 = New System.Windows.Forms.RadioButton()
    Me.radKODIF_1 = New System.Windows.Forms.RadioButton()
    Me.radKODIF_2 = New System.Windows.Forms.RadioButton()
    Me.radKODIF_0 = New System.Windows.Forms.RadioButton()
    Me.btnREF_1 = New System.Windows.Forms.Button()
    Me.lblREF_1 = New System.Windows.Forms.Label()
    Me.SplitContainWinkel = New System.Windows.Forms.SplitContainer()
    Me.SplitContainGridPic = New System.Windows.Forms.SplitContainer()
    Me.GridTDB = New System.Windows.Forms.DataGridView()
    Me.picREF = New System.Windows.Forms.PictureBox()
    Me.dbgRef = New System.Windows.Forms.DataGridView()
    Me.btnKOP = New System.Windows.Forms.Button()
    Me.chkREF = New System.Windows.Forms.CheckBox()
    Me.txtDE = New System.Windows.Forms.TextBox()
    Me.lblDE = New System.Windows.Forms.Label()
    Me.cboWinkel = New System.Windows.Forms.ComboBox()
    Me.cboLicht = New System.Windows.Forms.ComboBox()
    Me.lblWinkel = New System.Windows.Forms.Label()
    Me.lblLicht = New System.Windows.Forms.Label()
    Me.panPic = New System.Windows.Forms.Panel()
    Me.picFarbeI = New System.Windows.Forms.PictureBox()
    Me.picFarbeS = New System.Windows.Forms.PictureBox()
    Me.hscAlpha = New System.Windows.Forms.HScrollBar()
    Me.cboGLZ = New System.Windows.Forms.ComboBox()
    Me.lblGLZ = New System.Windows.Forms.Label()
    Me.cboGRPRwrt = New System.Windows.Forms.ComboBox()
    Me.btnSPEI = New System.Windows.Forms.Button()
    Me.btnVER = New System.Windows.Forms.Button()
    Me.panWIN = New System.Windows.Forms.Panel()
    Me.chkWIN_8 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_7 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_6 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_5 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_4 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_3 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_2 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_1 = New System.Windows.Forms.CheckBox()
    Me.chkWIN_0 = New System.Windows.Forms.CheckBox()
    CType(Me.SplitContainFarbUmrechnung, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContainFarbUmrechnung.Panel1.SuspendLayout()
    Me.SplitContainFarbUmrechnung.Panel2.SuspendLayout()
    Me.SplitContainFarbUmrechnung.SuspendLayout()
    Me.panANGL.SuspendLayout()
    Me.panKURV.SuspendLayout()
    Me.panOptKODIF.SuspendLayout()
    CType(Me.SplitContainWinkel, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContainWinkel.Panel1.SuspendLayout()
    Me.SplitContainWinkel.Panel2.SuspendLayout()
    Me.SplitContainWinkel.SuspendLayout()
    CType(Me.SplitContainGridPic, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContainGridPic.Panel1.SuspendLayout()
    Me.SplitContainGridPic.Panel2.SuspendLayout()
    Me.SplitContainGridPic.SuspendLayout()
    CType(Me.GridTDB, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.picREF, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.dbgRef, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.panPic.SuspendLayout()
    CType(Me.picFarbeI, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.picFarbeS, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.panWIN.SuspendLayout()
    Me.SuspendLayout()
    '
    'SplitContainFarbUmrechnung
    '
    Me.SplitContainFarbUmrechnung.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContainFarbUmrechnung.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.SplitContainFarbUmrechnung.Location = New System.Drawing.Point(0, 0)
    Me.SplitContainFarbUmrechnung.Name = "SplitContainFarbUmrechnung"
    Me.SplitContainFarbUmrechnung.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'SplitContainFarbUmrechnung.Panel1
    '
    Me.SplitContainFarbUmrechnung.Panel1.Controls.Add(Me.chkRwerte)
    Me.SplitContainFarbUmrechnung.Panel1.Controls.Add(Me.lstCMYK)
    Me.SplitContainFarbUmrechnung.Panel1.Controls.Add(Me.panANGL)
    Me.SplitContainFarbUmrechnung.Panel1.Controls.Add(Me.panKURV)
    Me.SplitContainFarbUmrechnung.Panel1.Controls.Add(Me.lblREF_2)
    Me.SplitContainFarbUmrechnung.Panel1.Controls.Add(Me.lblREF_0)
    Me.SplitContainFarbUmrechnung.Panel1.Controls.Add(Me.btnREF_2)
    Me.SplitContainFarbUmrechnung.Panel1.Controls.Add(Me.btnREF_0)
    Me.SplitContainFarbUmrechnung.Panel1.Controls.Add(Me.panOptKODIF)
    Me.SplitContainFarbUmrechnung.Panel1.Controls.Add(Me.btnREF_1)
    Me.SplitContainFarbUmrechnung.Panel1.Controls.Add(Me.lblREF_1)
    '
    'SplitContainFarbUmrechnung.Panel2
    '
    Me.SplitContainFarbUmrechnung.Panel2.Controls.Add(Me.SplitContainWinkel)
    Me.SplitContainFarbUmrechnung.Size = New System.Drawing.Size(996, 603)
    Me.SplitContainFarbUmrechnung.SplitterDistance = 171
    Me.SplitContainFarbUmrechnung.TabIndex = 0
    '
    'chkRwerte
    '
    Me.chkRwerte.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkRwerte.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkRwerte.Location = New System.Drawing.Point(598, 126)
    Me.chkRwerte.Name = "chkRwerte"
    Me.chkRwerte.Size = New System.Drawing.Size(185, 28)
    Me.chkRwerte.TabIndex = 10
    Me.chkRwerte.Text = "370"
    Me.chkRwerte.UseVisualStyleBackColor = False
    Me.chkRwerte.Visible = False
    '
    'lstCMYK
    '
    Me.lstCMYK.Anchor = System.Windows.Forms.AnchorStyles.Right
    Me.lstCMYK.FormattingEnabled = True
    Me.lstCMYK.Location = New System.Drawing.Point(789, 86)
    Me.lstCMYK.Name = "lstCMYK"
    Me.lstCMYK.Size = New System.Drawing.Size(204, 82)
    Me.lstCMYK.TabIndex = 9
    Me.lstCMYK.Visible = False
    '
    'panANGL
    '
    Me.panANGL.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.panANGL.Controls.Add(Me.radANGL_1)
    Me.panANGL.Controls.Add(Me.radANGL_0)
    Me.panANGL.Location = New System.Drawing.Point(599, 80)
    Me.panANGL.Name = "panANGL"
    Me.panANGL.Size = New System.Drawing.Size(188, 40)
    Me.panANGL.TabIndex = 8
    '
    'radANGL_1
    '
    Me.radANGL_1.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radANGL_1.Location = New System.Drawing.Point(0, 18)
    Me.radANGL_1.Name = "radANGL_1"
    Me.radANGL_1.Size = New System.Drawing.Size(185, 18)
    Me.radANGL_1.TabIndex = 4
    Me.radANGL_1.Text = "1248"
    Me.radANGL_1.UseVisualStyleBackColor = True
    '
    'radANGL_0
    '
    Me.radANGL_0.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radANGL_0.Checked = True
    Me.radANGL_0.Location = New System.Drawing.Point(0, 3)
    Me.radANGL_0.Name = "radANGL_0"
    Me.radANGL_0.Size = New System.Drawing.Size(185, 18)
    Me.radANGL_0.TabIndex = 3
    Me.radANGL_0.TabStop = True
    Me.radANGL_0.Text = "1247"
    Me.radANGL_0.UseVisualStyleBackColor = True
    '
    'panKURV
    '
    Me.panKURV.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.panKURV.Controls.Add(Me.radKURV_2)
    Me.panKURV.Controls.Add(Me.radKURV_1)
    Me.panKURV.Controls.Add(Me.radKURV_0)
    Me.panKURV.Location = New System.Drawing.Point(294, 80)
    Me.panKURV.Name = "panKURV"
    Me.panKURV.Size = New System.Drawing.Size(290, 54)
    Me.panKURV.TabIndex = 7
    '
    'radKURV_2
    '
    Me.radKURV_2.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKURV_2.Location = New System.Drawing.Point(0, 34)
    Me.radKURV_2.Name = "radKURV_2"
    Me.radKURV_2.Size = New System.Drawing.Size(290, 18)
    Me.radKURV_2.TabIndex = 2
    Me.radKURV_2.Text = "1257"
    Me.radKURV_2.UseVisualStyleBackColor = True
    '
    'radKURV_1
    '
    Me.radKURV_1.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKURV_1.Location = New System.Drawing.Point(0, 16)
    Me.radKURV_1.Name = "radKURV_1"
    Me.radKURV_1.Size = New System.Drawing.Size(290, 18)
    Me.radKURV_1.TabIndex = 1
    Me.radKURV_1.Text = "1256"
    Me.radKURV_1.UseVisualStyleBackColor = True
    '
    'radKURV_0
    '
    Me.radKURV_0.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKURV_0.Checked = True
    Me.radKURV_0.Location = New System.Drawing.Point(0, 0)
    Me.radKURV_0.Name = "radKURV_0"
    Me.radKURV_0.Size = New System.Drawing.Size(290, 18)
    Me.radKURV_0.TabIndex = 0
    Me.radKURV_0.TabStop = True
    Me.radKURV_0.Text = "1255"
    Me.radKURV_0.UseVisualStyleBackColor = True
    '
    'lblREF_2
    '
    Me.lblREF_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblREF_2.BackColor = System.Drawing.Color.White
    Me.lblREF_2.Location = New System.Drawing.Point(527, 56)
    Me.lblREF_2.Name = "lblREF_2"
    Me.lblREF_2.Size = New System.Drawing.Size(360, 21)
    Me.lblREF_2.TabIndex = 6
    Me.lblREF_2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    Me.lblREF_2.Visible = False
    '
    'lblREF_0
    '
    Me.lblREF_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblREF_0.BackColor = System.Drawing.Color.White
    Me.lblREF_0.Location = New System.Drawing.Point(527, 35)
    Me.lblREF_0.Name = "lblREF_0"
    Me.lblREF_0.Size = New System.Drawing.Size(360, 21)
    Me.lblREF_0.TabIndex = 4
    Me.lblREF_0.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    '
    'btnREF_2
    '
    Me.btnREF_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnREF_2.Location = New System.Drawing.Point(294, 53)
    Me.btnREF_2.Name = "btnREF_2"
    Me.btnREF_2.Size = New System.Drawing.Size(234, 21)
    Me.btnREF_2.TabIndex = 3
    Me.btnREF_2.Text = "1252"
    Me.btnREF_2.UseVisualStyleBackColor = True
    Me.btnREF_2.Visible = False
    '
    'btnREF_0
    '
    Me.btnREF_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnREF_0.Location = New System.Drawing.Point(294, 34)
    Me.btnREF_0.Name = "btnREF_0"
    Me.btnREF_0.Size = New System.Drawing.Size(234, 21)
    Me.btnREF_0.TabIndex = 1
    Me.btnREF_0.Text = "1250"
    Me.btnREF_0.UseVisualStyleBackColor = True
    '
    'panOptKODIF
    '
    Me.panOptKODIF.Controls.Add(Me.radKODIF_8)
    Me.panOptKODIF.Controls.Add(Me.radKODIF_7)
    Me.panOptKODIF.Controls.Add(Me.radKODIF_6)
    Me.panOptKODIF.Controls.Add(Me.radKODIF_5)
    Me.panOptKODIF.Controls.Add(Me.radKODIF_4)
    Me.panOptKODIF.Controls.Add(Me.radKODIF_3)
    Me.panOptKODIF.Controls.Add(Me.radKODIF_1)
    Me.panOptKODIF.Controls.Add(Me.radKODIF_2)
    Me.panOptKODIF.Controls.Add(Me.radKODIF_0)
    Me.panOptKODIF.Location = New System.Drawing.Point(0, 0)
    Me.panOptKODIF.Name = "panOptKODIF"
    Me.panOptKODIF.Size = New System.Drawing.Size(194, 159)
    Me.panOptKODIF.TabIndex = 0
    '
    'radKODIF_8
    '
    Me.radKODIF_8.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKODIF_8.Location = New System.Drawing.Point(1, 135)
    Me.radKODIF_8.Name = "radKODIF_8"
    Me.radKODIF_8.Size = New System.Drawing.Size(194, 22)
    Me.radKODIF_8.TabIndex = 8
    Me.radKODIF_8.Text = "1264"
    Me.radKODIF_8.UseVisualStyleBackColor = True
    '
    'radKODIF_7
    '
    Me.radKODIF_7.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKODIF_7.Location = New System.Drawing.Point(1, 116)
    Me.radKODIF_7.Name = "radKODIF_7"
    Me.radKODIF_7.Size = New System.Drawing.Size(194, 22)
    Me.radKODIF_7.TabIndex = 7
    Me.radKODIF_7.Text = "1263"
    Me.radKODIF_7.UseVisualStyleBackColor = True
    '
    'radKODIF_6
    '
    Me.radKODIF_6.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKODIF_6.Location = New System.Drawing.Point(1, 99)
    Me.radKODIF_6.Name = "radKODIF_6"
    Me.radKODIF_6.Size = New System.Drawing.Size(194, 22)
    Me.radKODIF_6.TabIndex = 6
    Me.radKODIF_6.Text = "1262"
    Me.radKODIF_6.UseVisualStyleBackColor = True
    '
    'radKODIF_5
    '
    Me.radKODIF_5.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKODIF_5.Location = New System.Drawing.Point(2, 83)
    Me.radKODIF_5.Name = "radKODIF_5"
    Me.radKODIF_5.Size = New System.Drawing.Size(194, 22)
    Me.radKODIF_5.TabIndex = 5
    Me.radKODIF_5.Text = "1246"
    Me.radKODIF_5.UseVisualStyleBackColor = True
    '
    'radKODIF_4
    '
    Me.radKODIF_4.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKODIF_4.Location = New System.Drawing.Point(2, 65)
    Me.radKODIF_4.Name = "radKODIF_4"
    Me.radKODIF_4.Size = New System.Drawing.Size(194, 23)
    Me.radKODIF_4.TabIndex = 4
    Me.radKODIF_4.Text = "1245"
    Me.radKODIF_4.UseVisualStyleBackColor = True
    '
    'radKODIF_3
    '
    Me.radKODIF_3.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKODIF_3.Location = New System.Drawing.Point(2, 50)
    Me.radKODIF_3.Name = "radKODIF_3"
    Me.radKODIF_3.Size = New System.Drawing.Size(194, 22)
    Me.radKODIF_3.TabIndex = 3
    Me.radKODIF_3.Text = "1244"
    Me.radKODIF_3.UseVisualStyleBackColor = True
    '
    'radKODIF_1
    '
    Me.radKODIF_1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKODIF_1.Location = New System.Drawing.Point(1, 15)
    Me.radKODIF_1.Name = "radKODIF_1"
    Me.radKODIF_1.Size = New System.Drawing.Size(194, 22)
    Me.radKODIF_1.TabIndex = 2
    Me.radKODIF_1.Text = "1242"
    Me.radKODIF_1.UseVisualStyleBackColor = True
    '
    'radKODIF_2
    '
    Me.radKODIF_2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKODIF_2.Location = New System.Drawing.Point(1, 29)
    Me.radKODIF_2.Name = "radKODIF_2"
    Me.radKODIF_2.Size = New System.Drawing.Size(194, 27)
    Me.radKODIF_2.TabIndex = 1
    Me.radKODIF_2.Text = "1243"
    Me.radKODIF_2.UseVisualStyleBackColor = True
    '
    'radKODIF_0
    '
    Me.radKODIF_0.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radKODIF_0.Checked = True
    Me.radKODIF_0.Location = New System.Drawing.Point(1, -2)
    Me.radKODIF_0.Name = "radKODIF_0"
    Me.radKODIF_0.Size = New System.Drawing.Size(194, 22)
    Me.radKODIF_0.TabIndex = 0
    Me.radKODIF_0.TabStop = True
    Me.radKODIF_0.Text = "1241"
    Me.radKODIF_0.UseVisualStyleBackColor = True
    '
    'btnREF_1
    '
    Me.btnREF_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnREF_1.Location = New System.Drawing.Point(294, 34)
    Me.btnREF_1.Name = "btnREF_1"
    Me.btnREF_1.Size = New System.Drawing.Size(234, 21)
    Me.btnREF_1.TabIndex = 2
    Me.btnREF_1.Text = "1251"
    Me.btnREF_1.UseVisualStyleBackColor = True
    Me.btnREF_1.Visible = False
    '
    'lblREF_1
    '
    Me.lblREF_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblREF_1.BackColor = System.Drawing.Color.White
    Me.lblREF_1.Location = New System.Drawing.Point(527, 35)
    Me.lblREF_1.Name = "lblREF_1"
    Me.lblREF_1.Size = New System.Drawing.Size(360, 21)
    Me.lblREF_1.TabIndex = 5
    Me.lblREF_1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    Me.lblREF_1.Visible = False
    '
    'SplitContainWinkel
    '
    Me.SplitContainWinkel.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContainWinkel.FixedPanel = System.Windows.Forms.FixedPanel.Panel2
    Me.SplitContainWinkel.Location = New System.Drawing.Point(0, 0)
    Me.SplitContainWinkel.Name = "SplitContainWinkel"
    Me.SplitContainWinkel.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'SplitContainWinkel.Panel1
    '
    Me.SplitContainWinkel.Panel1.Controls.Add(Me.SplitContainGridPic)
    '
    'SplitContainWinkel.Panel2
    '
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.btnKOP)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.chkREF)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.txtDE)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.lblDE)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.cboWinkel)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.cboLicht)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.lblWinkel)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.lblLicht)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.panPic)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.hscAlpha)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.cboGLZ)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.lblGLZ)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.cboGRPRwrt)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.btnSPEI)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.btnVER)
    Me.SplitContainWinkel.Panel2.Controls.Add(Me.panWIN)
    Me.SplitContainWinkel.Size = New System.Drawing.Size(996, 428)
    Me.SplitContainWinkel.SplitterDistance = 317
    Me.SplitContainWinkel.TabIndex = 0
    '
    'SplitContainGridPic
    '
    Me.SplitContainGridPic.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContainGridPic.Location = New System.Drawing.Point(0, 0)
    Me.SplitContainGridPic.Name = "SplitContainGridPic"
    '
    'SplitContainGridPic.Panel1
    '
    Me.SplitContainGridPic.Panel1.Controls.Add(Me.GridTDB)
    '
    'SplitContainGridPic.Panel2
    '
    Me.SplitContainGridPic.Panel2.Controls.Add(Me.picREF)
    Me.SplitContainGridPic.Panel2.Controls.Add(Me.dbgRef)
    Me.SplitContainGridPic.Size = New System.Drawing.Size(996, 317)
    Me.SplitContainGridPic.SplitterDistance = 506
    Me.SplitContainGridPic.TabIndex = 0
    '
    'GridTDB
    '
    Me.GridTDB.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.GridTDB.Dock = System.Windows.Forms.DockStyle.Fill
    Me.GridTDB.Location = New System.Drawing.Point(0, 0)
    Me.GridTDB.Name = "GridTDB"
    Me.GridTDB.RowTemplate.Height = 24
    Me.GridTDB.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
    Me.GridTDB.Size = New System.Drawing.Size(506, 317)
    Me.GridTDB.TabIndex = 0
    '
    'picREF
    '
    Me.picREF.Dock = System.Windows.Forms.DockStyle.Fill
    Me.picREF.Location = New System.Drawing.Point(0, 0)
    Me.picREF.Name = "picREF"
    Me.picREF.Size = New System.Drawing.Size(486, 317)
    Me.picREF.TabIndex = 0
    Me.picREF.TabStop = False
    '
    'dbgRef
    '
    Me.dbgRef.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgRef.Dock = System.Windows.Forms.DockStyle.Fill
    Me.dbgRef.Location = New System.Drawing.Point(0, 0)
    Me.dbgRef.Name = "dbgRef"
    Me.dbgRef.Size = New System.Drawing.Size(486, 317)
    Me.dbgRef.TabIndex = 1
    Me.dbgRef.Visible = False
    '
    'btnKOP
    '
    Me.btnKOP.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnKOP.Enabled = False
    Me.btnKOP.Location = New System.Drawing.Point(725, 32)
    Me.btnKOP.Name = "btnKOP"
    Me.btnKOP.Size = New System.Drawing.Size(212, 24)
    Me.btnKOP.TabIndex = 17
    Me.btnKOP.Text = "389"
    Me.btnKOP.UseVisualStyleBackColor = True
    '
    'chkREF
    '
    Me.chkREF.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.chkREF.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkREF.Location = New System.Drawing.Point(725, 7)
    Me.chkREF.Name = "chkREF"
    Me.chkREF.Size = New System.Drawing.Size(212, 19)
    Me.chkREF.TabIndex = 16
    Me.chkREF.Text = "529"
    Me.chkREF.UseVisualStyleBackColor = False
    '
    'txtDE
    '
    Me.txtDE.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.txtDE.Location = New System.Drawing.Point(671, 41)
    Me.txtDE.Name = "txtDE"
    Me.txtDE.Size = New System.Drawing.Size(48, 20)
    Me.txtDE.TabIndex = 15
    Me.txtDE.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
    '
    'lblDE
    '
    Me.lblDE.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblDE.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblDE.Location = New System.Drawing.Point(620, 41)
    Me.lblDE.Name = "lblDE"
    Me.lblDE.Size = New System.Drawing.Size(52, 21)
    Me.lblDE.TabIndex = 14
    Me.lblDE.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'cboWinkel
    '
    Me.cboWinkel.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboWinkel.FormattingEnabled = True
    Me.cboWinkel.Location = New System.Drawing.Point(598, 64)
    Me.cboWinkel.Name = "cboWinkel"
    Me.cboWinkel.Size = New System.Drawing.Size(77, 21)
    Me.cboWinkel.TabIndex = 13
    '
    'cboLicht
    '
    Me.cboLicht.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboLicht.FormattingEnabled = True
    Me.cboLicht.Location = New System.Drawing.Point(285, 62)
    Me.cboLicht.Name = "cboLicht"
    Me.cboLicht.Size = New System.Drawing.Size(133, 21)
    Me.cboLicht.TabIndex = 12
    '
    'lblWinkel
    '
    Me.lblWinkel.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblWinkel.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblWinkel.Location = New System.Drawing.Point(424, 64)
    Me.lblWinkel.Name = "lblWinkel"
    Me.lblWinkel.Size = New System.Drawing.Size(171, 21)
    Me.lblWinkel.TabIndex = 11
    Me.lblWinkel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblLicht
    '
    Me.lblLicht.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.lblLicht.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblLicht.Location = New System.Drawing.Point(113, 62)
    Me.lblLicht.Name = "lblLicht"
    Me.lblLicht.Size = New System.Drawing.Size(171, 21)
    Me.lblLicht.TabIndex = 10
    Me.lblLicht.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'panPic
    '
    Me.panPic.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.panPic.Controls.Add(Me.picFarbeI)
    Me.panPic.Controls.Add(Me.picFarbeS)
    Me.panPic.Location = New System.Drawing.Point(725, 55)
    Me.panPic.Name = "panPic"
    Me.panPic.Size = New System.Drawing.Size(212, 52)
    Me.panPic.TabIndex = 9
    '
    'picFarbeI
    '
    Me.picFarbeI.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.picFarbeI.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.picFarbeI.Location = New System.Drawing.Point(109, 0)
    Me.picFarbeI.Name = "picFarbeI"
    Me.picFarbeI.Size = New System.Drawing.Size(103, 52)
    Me.picFarbeI.TabIndex = 8
    Me.picFarbeI.TabStop = False
    '
    'picFarbeS
    '
    Me.picFarbeS.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
    Me.picFarbeS.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.picFarbeS.Location = New System.Drawing.Point(0, 0)
    Me.picFarbeS.Name = "picFarbeS"
    Me.picFarbeS.Size = New System.Drawing.Size(110, 52)
    Me.picFarbeS.TabIndex = 7
    Me.picFarbeS.TabStop = False
    '
    'hscAlpha
    '
    Me.hscAlpha.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.hscAlpha.Location = New System.Drawing.Point(553, 85)
    Me.hscAlpha.Name = "hscAlpha"
    Me.hscAlpha.Size = New System.Drawing.Size(122, 22)
    Me.hscAlpha.TabIndex = 8
    '
    'cboGLZ
    '
    Me.cboGLZ.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.cboGLZ.FormattingEnabled = True
    Me.cboGLZ.Location = New System.Drawing.Point(488, 85)
    Me.cboGLZ.Name = "cboGLZ"
    Me.cboGLZ.RightToLeft = System.Windows.Forms.RightToLeft.Yes
    Me.cboGLZ.Size = New System.Drawing.Size(65, 21)
    Me.cboGLZ.TabIndex = 6
    '
    'lblGLZ
    '
    Me.lblGLZ.Anchor = System.Windows.Forms.AnchorStyles.Bottom
    Me.lblGLZ.Location = New System.Drawing.Point(419, 86)
    Me.lblGLZ.Name = "lblGLZ"
    Me.lblGLZ.Size = New System.Drawing.Size(71, 20)
    Me.lblGLZ.TabIndex = 5
    Me.lblGLZ.Text = "358"
    Me.lblGLZ.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'cboGRPRwrt
    '
    Me.cboGRPRwrt.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.cboGRPRwrt.FormattingEnabled = True
    Me.cboGRPRwrt.Location = New System.Drawing.Point(467, 8)
    Me.cboGRPRwrt.Name = "cboGRPRwrt"
    Me.cboGRPRwrt.Size = New System.Drawing.Size(208, 21)
    Me.cboGRPRwrt.TabIndex = 4
    '
    'btnSPEI
    '
    Me.btnSPEI.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnSPEI.Enabled = False
    Me.btnSPEI.Location = New System.Drawing.Point(293, 7)
    Me.btnSPEI.Name = "btnSPEI"
    Me.btnSPEI.Size = New System.Drawing.Size(168, 24)
    Me.btnSPEI.TabIndex = 3
    Me.btnSPEI.Text = "1253"
    Me.btnSPEI.UseVisualStyleBackColor = True
    '
    'btnVER
    '
    Me.btnVER.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.btnVER.Location = New System.Drawing.Point(116, 7)
    Me.btnVER.Name = "btnVER"
    Me.btnVER.Size = New System.Drawing.Size(168, 24)
    Me.btnVER.TabIndex = 2
    Me.btnVER.Text = "360"
    Me.btnVER.UseVisualStyleBackColor = True
    '
    'panWIN
    '
    Me.panWIN.Anchor = System.Windows.Forms.AnchorStyles.Top
    Me.panWIN.Controls.Add(Me.chkWIN_8)
    Me.panWIN.Controls.Add(Me.chkWIN_7)
    Me.panWIN.Controls.Add(Me.chkWIN_6)
    Me.panWIN.Controls.Add(Me.chkWIN_5)
    Me.panWIN.Controls.Add(Me.chkWIN_4)
    Me.panWIN.Controls.Add(Me.chkWIN_3)
    Me.panWIN.Controls.Add(Me.chkWIN_2)
    Me.panWIN.Controls.Add(Me.chkWIN_1)
    Me.panWIN.Controls.Add(Me.chkWIN_0)
    Me.panWIN.Location = New System.Drawing.Point(117, 38)
    Me.panWIN.Name = "panWIN"
    Me.panWIN.Size = New System.Drawing.Size(504, 25)
    Me.panWIN.TabIndex = 1
    '
    'chkWIN_8
    '
    Me.chkWIN_8.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkWIN_8.Location = New System.Drawing.Point(448, -2)
    Me.chkWIN_8.Name = "chkWIN_8"
    Me.chkWIN_8.Size = New System.Drawing.Size(56, 21)
    Me.chkWIN_8.TabIndex = 8
    Me.chkWIN_8.Text = "CheckBox1"
    Me.chkWIN_8.UseVisualStyleBackColor = False
    '
    'chkWIN_7
    '
    Me.chkWIN_7.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkWIN_7.Location = New System.Drawing.Point(392, 0)
    Me.chkWIN_7.Name = "chkWIN_7"
    Me.chkWIN_7.Size = New System.Drawing.Size(56, 21)
    Me.chkWIN_7.TabIndex = 7
    Me.chkWIN_7.Text = "CheckBox1"
    Me.chkWIN_7.UseVisualStyleBackColor = False
    '
    'chkWIN_6
    '
    Me.chkWIN_6.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkWIN_6.Location = New System.Drawing.Point(336, 0)
    Me.chkWIN_6.Name = "chkWIN_6"
    Me.chkWIN_6.Size = New System.Drawing.Size(56, 21)
    Me.chkWIN_6.TabIndex = 6
    Me.chkWIN_6.Text = "CheckBox1"
    Me.chkWIN_6.UseVisualStyleBackColor = False
    '
    'chkWIN_5
    '
    Me.chkWIN_5.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkWIN_5.Location = New System.Drawing.Point(280, 0)
    Me.chkWIN_5.Name = "chkWIN_5"
    Me.chkWIN_5.Size = New System.Drawing.Size(56, 21)
    Me.chkWIN_5.TabIndex = 5
    Me.chkWIN_5.Text = "CheckBox1"
    Me.chkWIN_5.UseVisualStyleBackColor = False
    '
    'chkWIN_4
    '
    Me.chkWIN_4.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkWIN_4.Location = New System.Drawing.Point(224, 0)
    Me.chkWIN_4.Name = "chkWIN_4"
    Me.chkWIN_4.Size = New System.Drawing.Size(56, 21)
    Me.chkWIN_4.TabIndex = 4
    Me.chkWIN_4.Text = "CheckBox1"
    Me.chkWIN_4.UseVisualStyleBackColor = False
    '
    'chkWIN_3
    '
    Me.chkWIN_3.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkWIN_3.Location = New System.Drawing.Point(168, 0)
    Me.chkWIN_3.Name = "chkWIN_3"
    Me.chkWIN_3.Size = New System.Drawing.Size(56, 21)
    Me.chkWIN_3.TabIndex = 3
    Me.chkWIN_3.Text = "CheckBox1"
    Me.chkWIN_3.UseVisualStyleBackColor = False
    '
    'chkWIN_2
    '
    Me.chkWIN_2.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkWIN_2.Location = New System.Drawing.Point(112, 0)
    Me.chkWIN_2.Name = "chkWIN_2"
    Me.chkWIN_2.Size = New System.Drawing.Size(56, 21)
    Me.chkWIN_2.TabIndex = 2
    Me.chkWIN_2.Text = "CheckBox1"
    Me.chkWIN_2.UseVisualStyleBackColor = False
    '
    'chkWIN_1
    '
    Me.chkWIN_1.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkWIN_1.Location = New System.Drawing.Point(56, 0)
    Me.chkWIN_1.Name = "chkWIN_1"
    Me.chkWIN_1.Size = New System.Drawing.Size(56, 21)
    Me.chkWIN_1.TabIndex = 1
    Me.chkWIN_1.Text = "CheckBox1"
    Me.chkWIN_1.UseVisualStyleBackColor = False
    '
    'chkWIN_0
    '
    Me.chkWIN_0.BackColor = System.Drawing.SystemColors.InactiveBorder
    Me.chkWIN_0.Checked = True
    Me.chkWIN_0.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkWIN_0.Location = New System.Drawing.Point(0, 0)
    Me.chkWIN_0.Name = "chkWIN_0"
    Me.chkWIN_0.Size = New System.Drawing.Size(56, 21)
    Me.chkWIN_0.TabIndex = 0
    Me.chkWIN_0.Text = "CheckBox1"
    Me.chkWIN_0.UseVisualStyleBackColor = False
    '
    'frmFarbUmrechnung
    '
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Inherit
    Me.ClientSize = New System.Drawing.Size(996, 603)
    Me.ControlBox = False
    Me.Controls.Add(Me.SplitContainFarbUmrechnung)
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmFarbUmrechnung"
    Me.ShowIcon = False
    Me.Text = "1903"
    Me.SplitContainFarbUmrechnung.Panel1.ResumeLayout(False)
    Me.SplitContainFarbUmrechnung.Panel2.ResumeLayout(False)
    CType(Me.SplitContainFarbUmrechnung, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContainFarbUmrechnung.ResumeLayout(False)
    Me.panANGL.ResumeLayout(False)
    Me.panKURV.ResumeLayout(False)
    Me.panOptKODIF.ResumeLayout(False)
    Me.SplitContainWinkel.Panel1.ResumeLayout(False)
    Me.SplitContainWinkel.Panel2.ResumeLayout(False)
    Me.SplitContainWinkel.Panel2.PerformLayout()
    CType(Me.SplitContainWinkel, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContainWinkel.ResumeLayout(False)
    Me.SplitContainGridPic.Panel1.ResumeLayout(False)
    Me.SplitContainGridPic.Panel2.ResumeLayout(False)
    CType(Me.SplitContainGridPic, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContainGridPic.ResumeLayout(False)
    CType(Me.GridTDB, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.picREF, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.dbgRef, System.ComponentModel.ISupportInitialize).EndInit()
    Me.panPic.ResumeLayout(False)
    CType(Me.picFarbeI, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.picFarbeS, System.ComponentModel.ISupportInitialize).EndInit()
    Me.panWIN.ResumeLayout(False)
    Me.ResumeLayout(False)

End Sub
  Friend WithEvents SplitContainFarbUmrechnung As System.Windows.Forms.SplitContainer
  Friend WithEvents SplitContainGridPic As System.Windows.Forms.SplitContainer
  Friend WithEvents SplitContainWinkel As System.Windows.Forms.SplitContainer
  Friend WithEvents GridTDB As System.Windows.Forms.DataGridView
  Friend WithEvents picREF As System.Windows.Forms.PictureBox
  Friend WithEvents panWIN As System.Windows.Forms.Panel
  Friend WithEvents chkWIN_8 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_7 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_6 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_5 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_4 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_3 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_2 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_1 As System.Windows.Forms.CheckBox
  Friend WithEvents chkWIN_0 As System.Windows.Forms.CheckBox
  Friend WithEvents panOptKODIF As System.Windows.Forms.Panel
  Friend WithEvents radKODIF_1 As System.Windows.Forms.RadioButton
  Friend WithEvents radKODIF_2 As System.Windows.Forms.RadioButton
  Friend WithEvents radKODIF_0 As System.Windows.Forms.RadioButton
  Friend WithEvents btnREF_0 As System.Windows.Forms.Button
  Friend WithEvents radKODIF_4 As System.Windows.Forms.RadioButton
  Friend WithEvents radKODIF_3 As System.Windows.Forms.RadioButton
  Friend WithEvents lblREF_0 As System.Windows.Forms.Label
  Friend WithEvents btnREF_2 As System.Windows.Forms.Button
  Friend WithEvents btnREF_1 As System.Windows.Forms.Button
  Friend WithEvents panKURV As System.Windows.Forms.Panel
  Friend WithEvents radKURV_0 As System.Windows.Forms.RadioButton
  Friend WithEvents lblREF_2 As System.Windows.Forms.Label
  Friend WithEvents lblREF_1 As System.Windows.Forms.Label
  Friend WithEvents radKURV_2 As System.Windows.Forms.RadioButton
  Friend WithEvents radKURV_1 As System.Windows.Forms.RadioButton
  Friend WithEvents panANGL As System.Windows.Forms.Panel
  Friend WithEvents radANGL_1 As System.Windows.Forms.RadioButton
  Friend WithEvents radANGL_0 As System.Windows.Forms.RadioButton
  Friend WithEvents cboGLZ As System.Windows.Forms.ComboBox
  Friend WithEvents lblGLZ As System.Windows.Forms.Label
  Friend WithEvents cboGRPRwrt As System.Windows.Forms.ComboBox
  Friend WithEvents btnSPEI As System.Windows.Forms.Button
  Friend WithEvents btnVER As System.Windows.Forms.Button
  Friend WithEvents radKODIF_5 As System.Windows.Forms.RadioButton
  Friend WithEvents picFarbeS As System.Windows.Forms.PictureBox
  Friend WithEvents radKODIF_7 As System.Windows.Forms.RadioButton
  Friend WithEvents radKODIF_6 As System.Windows.Forms.RadioButton
  Friend WithEvents lstCMYK As System.Windows.Forms.ListBox
  Friend WithEvents chkRwerte As System.Windows.Forms.CheckBox
  Friend WithEvents hscAlpha As System.Windows.Forms.HScrollBar
  Friend WithEvents panPic As System.Windows.Forms.Panel
  Friend WithEvents picFarbeI As System.Windows.Forms.PictureBox
  Friend WithEvents cboWinkel As System.Windows.Forms.ComboBox
  Friend WithEvents cboLicht As System.Windows.Forms.ComboBox
  Friend WithEvents lblWinkel As System.Windows.Forms.Label
  Friend WithEvents lblLicht As System.Windows.Forms.Label
  Friend WithEvents txtDE As System.Windows.Forms.TextBox
  Friend WithEvents lblDE As System.Windows.Forms.Label
  Friend WithEvents radKODIF_8 As System.Windows.Forms.RadioButton
  Friend WithEvents dbgRef As System.Windows.Forms.DataGridView
  Friend WithEvents chkREF As System.Windows.Forms.CheckBox
  Friend WithEvents btnKOP As System.Windows.Forms.Button
End Class
