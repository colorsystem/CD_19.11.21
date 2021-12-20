Option Strict Off
Option Explicit On
Option Compare Text
Public Class frmFarbmittelEing
  Inherits Windows.Forms.Form
#Region "Vom Windows Form-Designer generierter Code "
  Public Sub New()
    MyBase.New()
    'Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

  End Sub
  'Das Formular überschreibt den Löschvorgang, um die Komponentenliste zu bereinigen.
  Protected Overloads Overrides Sub Dispose(ByVal Disposing As Boolean)
    If Disposing Then
      If Not components Is Nothing Then
        components.Dispose()
      End If
    End If
    MyBase.Dispose(Disposing)
  End Sub
  'Wird vom Windows Form-Designer benötigt.
  Private components As System.ComponentModel.IContainer
  Friend WithEvents cboART_1 As System.Windows.Forms.ComboBox
  Friend WithEvents cboART_0 As System.Windows.Forms.ComboBox
  Friend WithEvents lblFAR_04 As System.Windows.Forms.Label
  Friend WithEvents lblFAR_00 As System.Windows.Forms.Label
  Friend WithEvents lblMSH As System.Windows.Forms.Label
  Friend WithEvents lblFAR_03 As System.Windows.Forms.Label
  Friend WithEvents lblFAR_10 As System.Windows.Forms.Label
  Friend WithEvents lblFAR_09 As System.Windows.Forms.Label
  Friend WithEvents lblFAR_12 As System.Windows.Forms.Label
  Friend WithEvents lblFAR_08 As System.Windows.Forms.Label
  Friend WithEvents lblFAR_07 As System.Windows.Forms.Label
  Friend WithEvents lblFAR_06 As System.Windows.Forms.Label
  Friend WithEvents lblFAR_05 As System.Windows.Forms.Label
  Friend WithEvents lblFAR_11 As System.Windows.Forms.Label
  Friend WithEvents lblFAR_02 As System.Windows.Forms.Label
  Friend WithEvents lblFAR_01 As System.Windows.Forms.Label
  Friend WithEvents txtFAR_00 As System.Windows.Forms.TextBox
  Friend WithEvents txtFAR_01 As System.Windows.Forms.TextBox
  Friend WithEvents txtFAR_02 As System.Windows.Forms.TextBox
  Friend WithEvents txtFAR_03 As System.Windows.Forms.TextBox
  Friend WithEvents txtFAR_04 As System.Windows.Forms.TextBox
  Friend WithEvents txtFAR_05 As System.Windows.Forms.TextBox
  Friend WithEvents txtFAR_06 As System.Windows.Forms.TextBox
  Friend WithEvents txtFAR_07 As System.Windows.Forms.TextBox
  Friend WithEvents txtFAR_08 As System.Windows.Forms.TextBox
  Friend WithEvents txtFAR_09 As System.Windows.Forms.TextBox
  Friend WithEvents txtFAR_10 As System.Windows.Forms.TextBox
  Friend WithEvents picGridFarbe As System.Windows.Forms.PictureBox
  Friend WithEvents hscFarbe_0 As System.Windows.Forms.HScrollBar
  Friend WithEvents lblFarbe_0 As System.Windows.Forms.Label
  Friend WithEvents lblFarbe_1 As System.Windows.Forms.Label
  Friend WithEvents hscFarbe_1 As System.Windows.Forms.HScrollBar
  Friend WithEvents lblFarbe_2 As System.Windows.Forms.Label
  Friend WithEvents hscFarbe_2 As System.Windows.Forms.HScrollBar
  Friend WithEvents DataGridViewPreis As System.Windows.Forms.DataGridView
  Friend WithEvents DataGridViewProz As System.Windows.Forms.DataGridView
  Friend WithEvents DataGridViewProb As System.Windows.Forms.DataGridView
  Friend WithEvents BindingNavigatorAddNewPreis As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorDeletePreis As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorPreis As System.Windows.Forms.BindingNavigator
  Friend WithEvents BindingNavigatorProz As System.Windows.Forms.BindingNavigator
  Friend WithEvents BindingNavigatorAddNewProz As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorDeleteProz As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorProb As System.Windows.Forms.BindingNavigator
  Friend WithEvents BindingNavigatorAddNewProb As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorDeleteProb As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorFarbe As System.Windows.Forms.BindingNavigator
  Friend WithEvents BindingNavigatorAddNewFarbe As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorDeleteFarbe As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMoveFirstFarbe As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMovePreviousFarbe As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripLabelFarbe As System.Windows.Forms.ToolStripLabel
  Friend WithEvents BindingNavigatorMoveNextFarbe As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMoveLastFarbe As System.Windows.Forms.ToolStripButton
  Friend WithEvents lblFarbe_3 As System.Windows.Forms.Label
  Friend WithEvents hscFarbe_3 As System.Windows.Forms.HScrollBar
  Friend WithEvents btnQuit As System.Windows.Forms.Button
  Friend WithEvents lblFarbmittel As System.Windows.Forms.Label
  Friend WithEvents cboFarbmittel As System.Windows.Forms.ComboBox
  Friend WithEvents txtColor As System.Windows.Forms.TextBox
  Friend WithEvents lblFAR_13 As System.Windows.Forms.Label
  Friend WithEvents txtFAR_11 As System.Windows.Forms.TextBox
  Friend WithEvents cboART_2 As System.Windows.Forms.ComboBox
  Friend WithEvents lblFAR_14 As System.Windows.Forms.Label
  Friend WithEvents dbgGlanzGrad As System.Windows.Forms.DataGridView
  Friend WithEvents cboMSH As System.Windows.Forms.ComboBox
  'Hinweis: Die folgende Prozedur wird vom Windows Form-Designer benötigt.
  'Das Verändern mit dem Windows Form-Designer ist nicht möglich.
  'Das Verändern mit dem Code-Editor ist nicht möglich.
  <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
    Me.components = New System.ComponentModel.Container()
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmFarbmittelEing))
    Me.cboART_1 = New System.Windows.Forms.ComboBox()
    Me.cboART_0 = New System.Windows.Forms.ComboBox()
    Me.lblFAR_04 = New System.Windows.Forms.Label()
    Me.lblFAR_00 = New System.Windows.Forms.Label()
    Me.lblMSH = New System.Windows.Forms.Label()
    Me.lblFAR_03 = New System.Windows.Forms.Label()
    Me.lblFAR_10 = New System.Windows.Forms.Label()
    Me.lblFAR_09 = New System.Windows.Forms.Label()
    Me.lblFAR_12 = New System.Windows.Forms.Label()
    Me.lblFAR_08 = New System.Windows.Forms.Label()
    Me.lblFAR_07 = New System.Windows.Forms.Label()
    Me.lblFAR_06 = New System.Windows.Forms.Label()
    Me.lblFAR_05 = New System.Windows.Forms.Label()
    Me.lblFAR_11 = New System.Windows.Forms.Label()
    Me.lblFAR_02 = New System.Windows.Forms.Label()
    Me.lblFAR_01 = New System.Windows.Forms.Label()
    Me.txtFAR_00 = New System.Windows.Forms.TextBox()
    Me.txtFAR_01 = New System.Windows.Forms.TextBox()
    Me.txtFAR_02 = New System.Windows.Forms.TextBox()
    Me.txtFAR_03 = New System.Windows.Forms.TextBox()
    Me.txtFAR_04 = New System.Windows.Forms.TextBox()
    Me.txtFAR_05 = New System.Windows.Forms.TextBox()
    Me.txtFAR_06 = New System.Windows.Forms.TextBox()
    Me.txtFAR_07 = New System.Windows.Forms.TextBox()
    Me.txtFAR_08 = New System.Windows.Forms.TextBox()
    Me.txtFAR_09 = New System.Windows.Forms.TextBox()
    Me.txtFAR_10 = New System.Windows.Forms.TextBox()
    Me.picGridFarbe = New System.Windows.Forms.PictureBox()
    Me.hscFarbe_0 = New System.Windows.Forms.HScrollBar()
    Me.lblFarbe_0 = New System.Windows.Forms.Label()
    Me.lblFarbe_1 = New System.Windows.Forms.Label()
    Me.hscFarbe_1 = New System.Windows.Forms.HScrollBar()
    Me.lblFarbe_2 = New System.Windows.Forms.Label()
    Me.hscFarbe_2 = New System.Windows.Forms.HScrollBar()
    Me.cboMSH = New System.Windows.Forms.ComboBox()
    Me.DataGridViewPreis = New System.Windows.Forms.DataGridView()
    Me.DataGridViewProz = New System.Windows.Forms.DataGridView()
    Me.DataGridViewProb = New System.Windows.Forms.DataGridView()
    Me.BindingNavigatorAddNewPreis = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorDeletePreis = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorPreis = New System.Windows.Forms.BindingNavigator(Me.components)
    Me.BindingNavigatorProz = New System.Windows.Forms.BindingNavigator(Me.components)
    Me.BindingNavigatorAddNewProz = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorDeleteProz = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorProb = New System.Windows.Forms.BindingNavigator(Me.components)
    Me.BindingNavigatorAddNewProb = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorDeleteProb = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorFarbe = New System.Windows.Forms.BindingNavigator(Me.components)
    Me.BindingNavigatorAddNewFarbe = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorDeleteFarbe = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMoveFirstFarbe = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMovePreviousFarbe = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripLabelFarbe = New System.Windows.Forms.ToolStripLabel()
    Me.BindingNavigatorMoveNextFarbe = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMoveLastFarbe = New System.Windows.Forms.ToolStripButton()
    Me.lblFarbe_3 = New System.Windows.Forms.Label()
    Me.hscFarbe_3 = New System.Windows.Forms.HScrollBar()
    Me.btnQuit = New System.Windows.Forms.Button()
    Me.lblFarbmittel = New System.Windows.Forms.Label()
    Me.cboFarbmittel = New System.Windows.Forms.ComboBox()
    Me.txtColor = New System.Windows.Forms.TextBox()
    Me.lblFAR_13 = New System.Windows.Forms.Label()
    Me.txtFAR_11 = New System.Windows.Forms.TextBox()
    Me.cboART_2 = New System.Windows.Forms.ComboBox()
    Me.lblFAR_14 = New System.Windows.Forms.Label()
    Me.dbgGlanzGrad = New System.Windows.Forms.DataGridView()
    CType(Me.picGridFarbe, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.DataGridViewPreis, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.DataGridViewProz, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.DataGridViewProb, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.BindingNavigatorPreis, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.BindingNavigatorPreis.SuspendLayout()
    CType(Me.BindingNavigatorProz, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.BindingNavigatorProz.SuspendLayout()
    CType(Me.BindingNavigatorProb, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.BindingNavigatorProb.SuspendLayout()
    CType(Me.BindingNavigatorFarbe, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.BindingNavigatorFarbe.SuspendLayout()
    CType(Me.dbgGlanzGrad, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'cboART_1
    '
    Me.cboART_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboART_1.BackColor = System.Drawing.SystemColors.Window
    Me.cboART_1.Cursor = System.Windows.Forms.Cursors.Default
    Me.cboART_1.ForeColor = System.Drawing.SystemColors.WindowText
    Me.cboART_1.FormattingEnabled = True
    Me.cboART_1.Location = New System.Drawing.Point(300, 321)
    Me.cboART_1.Name = "cboART_1"
    Me.cboART_1.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.cboART_1.Size = New System.Drawing.Size(315, 21)
    Me.cboART_1.TabIndex = 17
    Me.cboART_1.Visible = False
    '
    'cboART_0
    '
    Me.cboART_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboART_0.BackColor = System.Drawing.SystemColors.Window
    Me.cboART_0.Cursor = System.Windows.Forms.Cursors.Default
    Me.cboART_0.ForeColor = System.Drawing.SystemColors.WindowText
    Me.cboART_0.FormattingEnabled = True
    Me.cboART_0.Location = New System.Drawing.Point(300, 261)
    Me.cboART_0.Name = "cboART_0"
    Me.cboART_0.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.cboART_0.Size = New System.Drawing.Size(315, 21)
    Me.cboART_0.TabIndex = 15
    '
    'lblFAR_04
    '
    Me.lblFAR_04.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_04.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_04.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_04.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_04.Location = New System.Drawing.Point(102, 123)
    Me.lblFAR_04.Name = "lblFAR_04"
    Me.lblFAR_04.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_04.Size = New System.Drawing.Size(193, 17)
    Me.lblFAR_04.TabIndex = 31
    Me.lblFAR_04.Text = "917"
    Me.lblFAR_04.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblFAR_00
    '
    Me.lblFAR_00.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_00.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_00.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_00.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_00.Location = New System.Drawing.Point(102, 44)
    Me.lblFAR_00.Name = "lblFAR_00"
    Me.lblFAR_00.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_00.Size = New System.Drawing.Size(193, 16)
    Me.lblFAR_00.TabIndex = 29
    Me.lblFAR_00.Text = "913"
    Me.lblFAR_00.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMSH
    '
    Me.lblMSH.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMSH.BackColor = System.Drawing.SystemColors.Control
    Me.lblMSH.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblMSH.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblMSH.Location = New System.Drawing.Point(614, 552)
    Me.lblMSH.Name = "lblMSH"
    Me.lblMSH.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblMSH.Size = New System.Drawing.Size(251, 20)
    Me.lblMSH.TabIndex = 28
    Me.lblMSH.Text = "422"
    Me.lblMSH.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblFAR_03
    '
    Me.lblFAR_03.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_03.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_03.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_03.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_03.Location = New System.Drawing.Point(102, 105)
    Me.lblFAR_03.Name = "lblFAR_03"
    Me.lblFAR_03.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_03.Size = New System.Drawing.Size(193, 17)
    Me.lblFAR_03.TabIndex = 24
    Me.lblFAR_03.Text = "916"
    Me.lblFAR_03.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblFAR_10
    '
    Me.lblFAR_10.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_10.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_10.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_10.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_10.Location = New System.Drawing.Point(102, 242)
    Me.lblFAR_10.Name = "lblFAR_10"
    Me.lblFAR_10.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_10.Size = New System.Drawing.Size(193, 18)
    Me.lblFAR_10.TabIndex = 22
    Me.lblFAR_10.Text = "914"
    Me.lblFAR_10.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblFAR_09
    '
    Me.lblFAR_09.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_09.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_09.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_09.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_09.Location = New System.Drawing.Point(102, 225)
    Me.lblFAR_09.Name = "lblFAR_09"
    Me.lblFAR_09.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_09.Size = New System.Drawing.Size(193, 17)
    Me.lblFAR_09.TabIndex = 19
    Me.lblFAR_09.Text = "912"
    Me.lblFAR_09.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblFAR_12
    '
    Me.lblFAR_12.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_12.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_12.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_12.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_12.Location = New System.Drawing.Point(100, 322)
    Me.lblFAR_12.Name = "lblFAR_12"
    Me.lblFAR_12.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_12.Size = New System.Drawing.Size(193, 17)
    Me.lblFAR_12.TabIndex = 16
    Me.lblFAR_12.Text = "905"
    Me.lblFAR_12.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblFAR_12.Visible = False
    '
    'lblFAR_08
    '
    Me.lblFAR_08.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_08.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_08.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_08.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_08.Location = New System.Drawing.Point(102, 205)
    Me.lblFAR_08.Name = "lblFAR_08"
    Me.lblFAR_08.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_08.Size = New System.Drawing.Size(193, 17)
    Me.lblFAR_08.TabIndex = 14
    Me.lblFAR_08.Text = "911"
    Me.lblFAR_08.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblFAR_07
    '
    Me.lblFAR_07.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_07.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_07.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_07.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_07.Location = New System.Drawing.Point(102, 184)
    Me.lblFAR_07.Name = "lblFAR_07"
    Me.lblFAR_07.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_07.Size = New System.Drawing.Size(193, 18)
    Me.lblFAR_07.TabIndex = 13
    Me.lblFAR_07.Text = "910"
    Me.lblFAR_07.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblFAR_06
    '
    Me.lblFAR_06.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_06.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_06.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_06.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_06.Location = New System.Drawing.Point(102, 164)
    Me.lblFAR_06.Name = "lblFAR_06"
    Me.lblFAR_06.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_06.Size = New System.Drawing.Size(193, 18)
    Me.lblFAR_06.TabIndex = 12
    Me.lblFAR_06.Text = "909"
    Me.lblFAR_06.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblFAR_05
    '
    Me.lblFAR_05.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_05.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_05.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_05.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_05.Location = New System.Drawing.Point(102, 144)
    Me.lblFAR_05.Name = "lblFAR_05"
    Me.lblFAR_05.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_05.Size = New System.Drawing.Size(193, 18)
    Me.lblFAR_05.TabIndex = 11
    Me.lblFAR_05.Text = "908"
    Me.lblFAR_05.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblFAR_11
    '
    Me.lblFAR_11.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_11.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_11.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_11.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_11.Location = New System.Drawing.Point(102, 265)
    Me.lblFAR_11.Name = "lblFAR_11"
    Me.lblFAR_11.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_11.Size = New System.Drawing.Size(193, 16)
    Me.lblFAR_11.TabIndex = 10
    Me.lblFAR_11.Text = "903"
    Me.lblFAR_11.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblFAR_02
    '
    Me.lblFAR_02.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_02.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_02.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_02.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_02.Location = New System.Drawing.Point(102, 86)
    Me.lblFAR_02.Name = "lblFAR_02"
    Me.lblFAR_02.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_02.Size = New System.Drawing.Size(193, 17)
    Me.lblFAR_02.TabIndex = 9
    Me.lblFAR_02.Text = "902"
    Me.lblFAR_02.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblFAR_01
    '
    Me.lblFAR_01.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_01.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_01.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_01.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_01.Location = New System.Drawing.Point(102, 66)
    Me.lblFAR_01.Name = "lblFAR_01"
    Me.lblFAR_01.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_01.Size = New System.Drawing.Size(193, 17)
    Me.lblFAR_01.TabIndex = 0
    Me.lblFAR_01.Text = "901"
    Me.lblFAR_01.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtFAR_00
    '
    Me.txtFAR_00.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_00.Enabled = False
    Me.txtFAR_00.Location = New System.Drawing.Point(302, 43)
    Me.txtFAR_00.Name = "txtFAR_00"
    Me.txtFAR_00.Size = New System.Drawing.Size(65, 20)
    Me.txtFAR_00.TabIndex = 35
    '
    'txtFAR_01
    '
    Me.txtFAR_01.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_01.Location = New System.Drawing.Point(302, 63)
    Me.txtFAR_01.Name = "txtFAR_01"
    Me.txtFAR_01.Size = New System.Drawing.Size(313, 20)
    Me.txtFAR_01.TabIndex = 36
    '
    'txtFAR_02
    '
    Me.txtFAR_02.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_02.Location = New System.Drawing.Point(302, 83)
    Me.txtFAR_02.Name = "txtFAR_02"
    Me.txtFAR_02.Size = New System.Drawing.Size(313, 20)
    Me.txtFAR_02.TabIndex = 37
    '
    'txtFAR_03
    '
    Me.txtFAR_03.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_03.Location = New System.Drawing.Point(302, 102)
    Me.txtFAR_03.Name = "txtFAR_03"
    Me.txtFAR_03.Size = New System.Drawing.Size(313, 20)
    Me.txtFAR_03.TabIndex = 38
    '
    'txtFAR_04
    '
    Me.txtFAR_04.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_04.Location = New System.Drawing.Point(300, 122)
    Me.txtFAR_04.Name = "txtFAR_04"
    Me.txtFAR_04.Size = New System.Drawing.Size(315, 20)
    Me.txtFAR_04.TabIndex = 39
    '
    'txtFAR_05
    '
    Me.txtFAR_05.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_05.Location = New System.Drawing.Point(302, 142)
    Me.txtFAR_05.Name = "txtFAR_05"
    Me.txtFAR_05.Size = New System.Drawing.Size(313, 20)
    Me.txtFAR_05.TabIndex = 40
    '
    'txtFAR_06
    '
    Me.txtFAR_06.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_06.Location = New System.Drawing.Point(302, 162)
    Me.txtFAR_06.Name = "txtFAR_06"
    Me.txtFAR_06.Size = New System.Drawing.Size(313, 20)
    Me.txtFAR_06.TabIndex = 41
    '
    'txtFAR_07
    '
    Me.txtFAR_07.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_07.Location = New System.Drawing.Point(302, 182)
    Me.txtFAR_07.Name = "txtFAR_07"
    Me.txtFAR_07.Size = New System.Drawing.Size(313, 20)
    Me.txtFAR_07.TabIndex = 42
    '
    'txtFAR_08
    '
    Me.txtFAR_08.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_08.Location = New System.Drawing.Point(302, 202)
    Me.txtFAR_08.Name = "txtFAR_08"
    Me.txtFAR_08.Size = New System.Drawing.Size(313, 20)
    Me.txtFAR_08.TabIndex = 43
    '
    'txtFAR_09
    '
    Me.txtFAR_09.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_09.Location = New System.Drawing.Point(302, 222)
    Me.txtFAR_09.Name = "txtFAR_09"
    Me.txtFAR_09.Size = New System.Drawing.Size(313, 20)
    Me.txtFAR_09.TabIndex = 44
    '
    'txtFAR_10
    '
    Me.txtFAR_10.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_10.Location = New System.Drawing.Point(302, 242)
    Me.txtFAR_10.Name = "txtFAR_10"
    Me.txtFAR_10.Size = New System.Drawing.Size(313, 20)
    Me.txtFAR_10.TabIndex = 45
    '
    'picGridFarbe
    '
    Me.picGridFarbe.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.picGridFarbe.Location = New System.Drawing.Point(787, 419)
    Me.picGridFarbe.Name = "picGridFarbe"
    Me.picGridFarbe.Size = New System.Drawing.Size(78, 104)
    Me.picGridFarbe.TabIndex = 48
    Me.picGridFarbe.TabStop = False
    '
    'hscFarbe_0
    '
    Me.hscFarbe_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.hscFarbe_0.LargeChange = 1
    Me.hscFarbe_0.Location = New System.Drawing.Point(650, 419)
    Me.hscFarbe_0.Maximum = 255
    Me.hscFarbe_0.Name = "hscFarbe_0"
    Me.hscFarbe_0.Size = New System.Drawing.Size(135, 17)
    Me.hscFarbe_0.TabIndex = 49
    Me.hscFarbe_0.Visible = False
    '
    'lblFarbe_0
    '
    Me.lblFarbe_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFarbe_0.Location = New System.Drawing.Point(614, 419)
    Me.lblFarbe_0.Name = "lblFarbe_0"
    Me.lblFarbe_0.Size = New System.Drawing.Size(35, 24)
    Me.lblFarbe_0.TabIndex = 50
    Me.lblFarbe_0.Text = "345"
    Me.lblFarbe_0.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblFarbe_0.Visible = False
    '
    'lblFarbe_1
    '
    Me.lblFarbe_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFarbe_1.Location = New System.Drawing.Point(614, 447)
    Me.lblFarbe_1.Name = "lblFarbe_1"
    Me.lblFarbe_1.Size = New System.Drawing.Size(35, 24)
    Me.lblFarbe_1.TabIndex = 52
    Me.lblFarbe_1.Text = "345"
    Me.lblFarbe_1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'hscFarbe_1
    '
    Me.hscFarbe_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.hscFarbe_1.LargeChange = 1
    Me.hscFarbe_1.Location = New System.Drawing.Point(650, 447)
    Me.hscFarbe_1.Maximum = 255
    Me.hscFarbe_1.Name = "hscFarbe_1"
    Me.hscFarbe_1.Size = New System.Drawing.Size(135, 16)
    Me.hscFarbe_1.TabIndex = 51
    '
    'lblFarbe_2
    '
    Me.lblFarbe_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFarbe_2.Location = New System.Drawing.Point(614, 475)
    Me.lblFarbe_2.Name = "lblFarbe_2"
    Me.lblFarbe_2.Size = New System.Drawing.Size(35, 24)
    Me.lblFarbe_2.TabIndex = 54
    Me.lblFarbe_2.Text = "346"
    Me.lblFarbe_2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'hscFarbe_2
    '
    Me.hscFarbe_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.hscFarbe_2.LargeChange = 1
    Me.hscFarbe_2.Location = New System.Drawing.Point(650, 475)
    Me.hscFarbe_2.Maximum = 255
    Me.hscFarbe_2.Name = "hscFarbe_2"
    Me.hscFarbe_2.Size = New System.Drawing.Size(135, 17)
    Me.hscFarbe_2.TabIndex = 53
    '
    'cboMSH
    '
    Me.cboMSH.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMSH.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboMSH.FormattingEnabled = True
    Me.cboMSH.Location = New System.Drawing.Point(614, 571)
    Me.cboMSH.Name = "cboMSH"
    Me.cboMSH.Size = New System.Drawing.Size(251, 21)
    Me.cboMSH.TabIndex = 60
    '
    'DataGridViewPreis
    '
    Me.DataGridViewPreis.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.DataGridViewPreis.ColumnHeadersHeight = 46
    Me.DataGridViewPreis.Enabled = False
    Me.DataGridViewPreis.Location = New System.Drawing.Point(97, 406)
    Me.DataGridViewPreis.Name = "DataGridViewPreis"
    Me.DataGridViewPreis.RowTemplate.Height = 24
    Me.DataGridViewPreis.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
    Me.DataGridViewPreis.Size = New System.Drawing.Size(160, 157)
    Me.DataGridViewPreis.TabIndex = 62
    Me.DataGridViewPreis.Text = "DataGridView1"
    '
    'DataGridViewProz
    '
    Me.DataGridViewProz.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.DataGridViewProz.ColumnHeadersHeight = 46
    Me.DataGridViewProz.Enabled = False
    Me.DataGridViewProz.Location = New System.Drawing.Point(270, 406)
    Me.DataGridViewProz.Name = "DataGridViewProz"
    Me.DataGridViewProz.RowTemplate.Height = 24
    Me.DataGridViewProz.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
    Me.DataGridViewProz.Size = New System.Drawing.Size(160, 157)
    Me.DataGridViewProz.TabIndex = 63
    Me.DataGridViewProz.Text = "DataGridView1"
    '
    'DataGridViewProb
    '
    Me.DataGridViewProb.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.DataGridViewProb.ColumnHeadersHeight = 46
    Me.DataGridViewProb.Enabled = False
    Me.DataGridViewProb.Location = New System.Drawing.Point(442, 406)
    Me.DataGridViewProb.Name = "DataGridViewProb"
    Me.DataGridViewProb.RowTemplate.Height = 24
    Me.DataGridViewProb.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
    Me.DataGridViewProb.Size = New System.Drawing.Size(160, 157)
    Me.DataGridViewProb.TabIndex = 64
    Me.DataGridViewProb.Text = "DataGridView1"
    '
    'BindingNavigatorAddNewPreis
    '
    Me.BindingNavigatorAddNewPreis.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorAddNewPreis.Image = CType(resources.GetObject("BindingNavigatorAddNewPreis.Image"), System.Drawing.Image)
    Me.BindingNavigatorAddNewPreis.Name = "BindingNavigatorAddNewPreis"
    Me.BindingNavigatorAddNewPreis.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorAddNewPreis.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorAddNewPreis.Text = "Neu hinzufügen"
    '
    'BindingNavigatorDeletePreis
    '
    Me.BindingNavigatorDeletePreis.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorDeletePreis.Image = CType(resources.GetObject("BindingNavigatorDeletePreis.Image"), System.Drawing.Image)
    Me.BindingNavigatorDeletePreis.Name = "BindingNavigatorDeletePreis"
    Me.BindingNavigatorDeletePreis.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorDeletePreis.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorDeletePreis.Text = "Löschen"
    '
    'BindingNavigatorPreis
    '
    Me.BindingNavigatorPreis.AddNewItem = Me.BindingNavigatorAddNewPreis
    Me.BindingNavigatorPreis.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.BindingNavigatorPreis.CountItem = Nothing
    Me.BindingNavigatorPreis.DeleteItem = Me.BindingNavigatorDeletePreis
    Me.BindingNavigatorPreis.Dock = System.Windows.Forms.DockStyle.None
    Me.BindingNavigatorPreis.Enabled = False
    Me.BindingNavigatorPreis.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BindingNavigatorAddNewPreis, Me.BindingNavigatorDeletePreis})
    Me.BindingNavigatorPreis.Location = New System.Drawing.Point(137, 571)
    Me.BindingNavigatorPreis.MoveFirstItem = Nothing
    Me.BindingNavigatorPreis.MoveLastItem = Nothing
    Me.BindingNavigatorPreis.MoveNextItem = Nothing
    Me.BindingNavigatorPreis.MovePreviousItem = Nothing
    Me.BindingNavigatorPreis.Name = "BindingNavigatorPreis"
    Me.BindingNavigatorPreis.PositionItem = Nothing
    Me.BindingNavigatorPreis.Size = New System.Drawing.Size(58, 25)
    Me.BindingNavigatorPreis.TabIndex = 66
    Me.BindingNavigatorPreis.Text = "BindingNavigPreis"
    '
    'BindingNavigatorProz
    '
    Me.BindingNavigatorProz.AddNewItem = Me.BindingNavigatorAddNewProz
    Me.BindingNavigatorProz.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.BindingNavigatorProz.CountItem = Nothing
    Me.BindingNavigatorProz.DeleteItem = Me.BindingNavigatorDeleteProz
    Me.BindingNavigatorProz.Dock = System.Windows.Forms.DockStyle.None
    Me.BindingNavigatorProz.Enabled = False
    Me.BindingNavigatorProz.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BindingNavigatorAddNewProz, Me.BindingNavigatorDeleteProz})
    Me.BindingNavigatorProz.Location = New System.Drawing.Point(310, 571)
    Me.BindingNavigatorProz.MoveFirstItem = Nothing
    Me.BindingNavigatorProz.MoveLastItem = Nothing
    Me.BindingNavigatorProz.MoveNextItem = Nothing
    Me.BindingNavigatorProz.MovePreviousItem = Nothing
    Me.BindingNavigatorProz.Name = "BindingNavigatorProz"
    Me.BindingNavigatorProz.PositionItem = Nothing
    Me.BindingNavigatorProz.Size = New System.Drawing.Size(58, 25)
    Me.BindingNavigatorProz.TabIndex = 67
    Me.BindingNavigatorProz.Text = "BindingNavigator1"
    '
    'BindingNavigatorAddNewProz
    '
    Me.BindingNavigatorAddNewProz.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorAddNewProz.Image = CType(resources.GetObject("BindingNavigatorAddNewProz.Image"), System.Drawing.Image)
    Me.BindingNavigatorAddNewProz.Name = "BindingNavigatorAddNewProz"
    Me.BindingNavigatorAddNewProz.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorAddNewProz.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorAddNewProz.Text = "Neu hinzufügen"
    '
    'BindingNavigatorDeleteProz
    '
    Me.BindingNavigatorDeleteProz.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorDeleteProz.Image = CType(resources.GetObject("BindingNavigatorDeleteProz.Image"), System.Drawing.Image)
    Me.BindingNavigatorDeleteProz.Name = "BindingNavigatorDeleteProz"
    Me.BindingNavigatorDeleteProz.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorDeleteProz.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorDeleteProz.Text = "Löschen"
    '
    'BindingNavigatorProb
    '
    Me.BindingNavigatorProb.AddNewItem = Me.BindingNavigatorAddNewProb
    Me.BindingNavigatorProb.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.BindingNavigatorProb.CountItem = Nothing
    Me.BindingNavigatorProb.DeleteItem = Me.BindingNavigatorDeleteProb
    Me.BindingNavigatorProb.Dock = System.Windows.Forms.DockStyle.None
    Me.BindingNavigatorProb.Enabled = False
    Me.BindingNavigatorProb.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BindingNavigatorAddNewProb, Me.BindingNavigatorDeleteProb})
    Me.BindingNavigatorProb.Location = New System.Drawing.Point(485, 571)
    Me.BindingNavigatorProb.MoveFirstItem = Nothing
    Me.BindingNavigatorProb.MoveLastItem = Nothing
    Me.BindingNavigatorProb.MoveNextItem = Nothing
    Me.BindingNavigatorProb.MovePreviousItem = Nothing
    Me.BindingNavigatorProb.Name = "BindingNavigatorProb"
    Me.BindingNavigatorProb.PositionItem = Nothing
    Me.BindingNavigatorProb.Size = New System.Drawing.Size(58, 25)
    Me.BindingNavigatorProb.TabIndex = 68
    Me.BindingNavigatorProb.Text = "BindingNavigator1"
    '
    'BindingNavigatorAddNewProb
    '
    Me.BindingNavigatorAddNewProb.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorAddNewProb.Image = CType(resources.GetObject("BindingNavigatorAddNewProb.Image"), System.Drawing.Image)
    Me.BindingNavigatorAddNewProb.Name = "BindingNavigatorAddNewProb"
    Me.BindingNavigatorAddNewProb.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorAddNewProb.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorAddNewProb.Text = "Neu hinzufügen"
    '
    'BindingNavigatorDeleteProb
    '
    Me.BindingNavigatorDeleteProb.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorDeleteProb.Image = CType(resources.GetObject("BindingNavigatorDeleteProb.Image"), System.Drawing.Image)
    Me.BindingNavigatorDeleteProb.Name = "BindingNavigatorDeleteProb"
    Me.BindingNavigatorDeleteProb.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorDeleteProb.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorDeleteProb.Text = "Löschen"
    '
    'BindingNavigatorFarbe
    '
    Me.BindingNavigatorFarbe.AddNewItem = Me.BindingNavigatorAddNewFarbe
    Me.BindingNavigatorFarbe.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.BindingNavigatorFarbe.CountItem = Nothing
    Me.BindingNavigatorFarbe.DeleteItem = Me.BindingNavigatorDeleteFarbe
    Me.BindingNavigatorFarbe.Dock = System.Windows.Forms.DockStyle.None
    Me.BindingNavigatorFarbe.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BindingNavigatorMoveFirstFarbe, Me.BindingNavigatorMovePreviousFarbe, Me.ToolStripLabelFarbe, Me.BindingNavigatorMoveNextFarbe, Me.BindingNavigatorMoveLastFarbe, Me.BindingNavigatorAddNewFarbe, Me.BindingNavigatorDeleteFarbe})
    Me.BindingNavigatorFarbe.Location = New System.Drawing.Point(298, 344)
    Me.BindingNavigatorFarbe.MoveFirstItem = Me.BindingNavigatorMoveFirstFarbe
    Me.BindingNavigatorFarbe.MoveLastItem = Me.BindingNavigatorMoveLastFarbe
    Me.BindingNavigatorFarbe.MoveNextItem = Me.BindingNavigatorMoveNextFarbe
    Me.BindingNavigatorFarbe.MovePreviousItem = Me.BindingNavigatorMovePreviousFarbe
    Me.BindingNavigatorFarbe.Name = "BindingNavigatorFarbe"
    Me.BindingNavigatorFarbe.PositionItem = Nothing
    Me.BindingNavigatorFarbe.Size = New System.Drawing.Size(350, 25)
    Me.BindingNavigatorFarbe.TabIndex = 69
    Me.BindingNavigatorFarbe.Text = "BindingNavigator1"
    '
    'BindingNavigatorAddNewFarbe
    '
    Me.BindingNavigatorAddNewFarbe.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorAddNewFarbe.Image = CType(resources.GetObject("BindingNavigatorAddNewFarbe.Image"), System.Drawing.Image)
    Me.BindingNavigatorAddNewFarbe.Name = "BindingNavigatorAddNewFarbe"
    Me.BindingNavigatorAddNewFarbe.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorAddNewFarbe.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorAddNewFarbe.Text = "Neu hinzufügen"
    '
    'BindingNavigatorDeleteFarbe
    '
    Me.BindingNavigatorDeleteFarbe.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorDeleteFarbe.Image = CType(resources.GetObject("BindingNavigatorDeleteFarbe.Image"), System.Drawing.Image)
    Me.BindingNavigatorDeleteFarbe.Name = "BindingNavigatorDeleteFarbe"
    Me.BindingNavigatorDeleteFarbe.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorDeleteFarbe.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorDeleteFarbe.Text = "Löschen"
    '
    'BindingNavigatorMoveFirstFarbe
    '
    Me.BindingNavigatorMoveFirstFarbe.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorMoveFirstFarbe.Image = CType(resources.GetObject("BindingNavigatorMoveFirstFarbe.Image"), System.Drawing.Image)
    Me.BindingNavigatorMoveFirstFarbe.Name = "BindingNavigatorMoveFirstFarbe"
    Me.BindingNavigatorMoveFirstFarbe.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorMoveFirstFarbe.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorMoveFirstFarbe.Text = "Erste verschieben"
    '
    'BindingNavigatorMovePreviousFarbe
    '
    Me.BindingNavigatorMovePreviousFarbe.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorMovePreviousFarbe.Image = CType(resources.GetObject("BindingNavigatorMovePreviousFarbe.Image"), System.Drawing.Image)
    Me.BindingNavigatorMovePreviousFarbe.Name = "BindingNavigatorMovePreviousFarbe"
    Me.BindingNavigatorMovePreviousFarbe.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorMovePreviousFarbe.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorMovePreviousFarbe.Text = "Vorherige verschieben"
    '
    'ToolStripLabelFarbe
    '
    Me.ToolStripLabelFarbe.AutoSize = False
    Me.ToolStripLabelFarbe.BackColor = System.Drawing.SystemColors.Control
    Me.ToolStripLabelFarbe.Name = "ToolStripLabelFarbe"
    Me.ToolStripLabelFarbe.Size = New System.Drawing.Size(200, 22)
    Me.ToolStripLabelFarbe.Text = "Farbmittel"
    '
    'BindingNavigatorMoveNextFarbe
    '
    Me.BindingNavigatorMoveNextFarbe.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorMoveNextFarbe.Image = CType(resources.GetObject("BindingNavigatorMoveNextFarbe.Image"), System.Drawing.Image)
    Me.BindingNavigatorMoveNextFarbe.Name = "BindingNavigatorMoveNextFarbe"
    Me.BindingNavigatorMoveNextFarbe.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorMoveNextFarbe.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorMoveNextFarbe.Text = "Nächste verschieben"
    '
    'BindingNavigatorMoveLastFarbe
    '
    Me.BindingNavigatorMoveLastFarbe.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorMoveLastFarbe.Image = CType(resources.GetObject("BindingNavigatorMoveLastFarbe.Image"), System.Drawing.Image)
    Me.BindingNavigatorMoveLastFarbe.Name = "BindingNavigatorMoveLastFarbe"
    Me.BindingNavigatorMoveLastFarbe.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorMoveLastFarbe.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorMoveLastFarbe.Text = "Letzte verschieben"
    '
    'lblFarbe_3
    '
    Me.lblFarbe_3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFarbe_3.Location = New System.Drawing.Point(614, 499)
    Me.lblFarbe_3.Name = "lblFarbe_3"
    Me.lblFarbe_3.Size = New System.Drawing.Size(35, 24)
    Me.lblFarbe_3.TabIndex = 70
    Me.lblFarbe_3.Text = "347"
    Me.lblFarbe_3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'hscFarbe_3
    '
    Me.hscFarbe_3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.hscFarbe_3.LargeChange = 1
    Me.hscFarbe_3.Location = New System.Drawing.Point(650, 506)
    Me.hscFarbe_3.Maximum = 255
    Me.hscFarbe_3.Name = "hscFarbe_3"
    Me.hscFarbe_3.Size = New System.Drawing.Size(135, 17)
    Me.hscFarbe_3.TabIndex = 71
    '
    'btnQuit
    '
    Me.btnQuit.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnQuit.Location = New System.Drawing.Point(676, 361)
    Me.btnQuit.Name = "btnQuit"
    Me.btnQuit.Size = New System.Drawing.Size(171, 24)
    Me.btnQuit.TabIndex = 72
    Me.btnQuit.Text = "3004"
    Me.btnQuit.UseVisualStyleBackColor = True
    '
    'lblFarbmittel
    '
    Me.lblFarbmittel.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFarbmittel.BackColor = System.Drawing.SystemColors.Control
    Me.lblFarbmittel.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFarbmittel.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFarbmittel.Location = New System.Drawing.Point(100, 16)
    Me.lblFarbmittel.Name = "lblFarbmittel"
    Me.lblFarbmittel.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFarbmittel.Size = New System.Drawing.Size(193, 16)
    Me.lblFarbmittel.TabIndex = 73
    Me.lblFarbmittel.Text = "801"
    Me.lblFarbmittel.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'cboFarbmittel
    '
    Me.cboFarbmittel.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboFarbmittel.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboFarbmittel.FormattingEnabled = True
    Me.cboFarbmittel.Location = New System.Drawing.Point(300, 16)
    Me.cboFarbmittel.Name = "cboFarbmittel"
    Me.cboFarbmittel.Size = New System.Drawing.Size(312, 21)
    Me.cboFarbmittel.TabIndex = 74
    '
    'txtColor
    '
    Me.txtColor.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtColor.Location = New System.Drawing.Point(787, 479)
    Me.txtColor.Name = "txtColor"
    Me.txtColor.Size = New System.Drawing.Size(78, 20)
    Me.txtColor.TabIndex = 75
    '
    'lblFAR_13
    '
    Me.lblFAR_13.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_13.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_13.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_13.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_13.Location = New System.Drawing.Point(100, 283)
    Me.lblFAR_13.Name = "lblFAR_13"
    Me.lblFAR_13.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_13.Size = New System.Drawing.Size(193, 17)
    Me.lblFAR_13.TabIndex = 76
    Me.lblFAR_13.Text = "870"
    Me.lblFAR_13.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtFAR_11
    '
    Me.txtFAR_11.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtFAR_11.Location = New System.Drawing.Point(300, 283)
    Me.txtFAR_11.Name = "txtFAR_11"
    Me.txtFAR_11.Size = New System.Drawing.Size(313, 20)
    Me.txtFAR_11.TabIndex = 77
    '
    'cboART_2
    '
    Me.cboART_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboART_2.BackColor = System.Drawing.SystemColors.Window
    Me.cboART_2.Cursor = System.Windows.Forms.Cursors.Default
    Me.cboART_2.ForeColor = System.Drawing.SystemColors.WindowText
    Me.cboART_2.FormattingEnabled = True
    Me.cboART_2.Location = New System.Drawing.Point(300, 301)
    Me.cboART_2.Name = "cboART_2"
    Me.cboART_2.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.cboART_2.Size = New System.Drawing.Size(315, 21)
    Me.cboART_2.TabIndex = 78
    '
    'lblFAR_14
    '
    Me.lblFAR_14.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblFAR_14.BackColor = System.Drawing.SystemColors.Control
    Me.lblFAR_14.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblFAR_14.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblFAR_14.Location = New System.Drawing.Point(10, 300)
    Me.lblFAR_14.Name = "lblFAR_14"
    Me.lblFAR_14.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblFAR_14.Size = New System.Drawing.Size(284, 20)
    Me.lblFAR_14.TabIndex = 79
    Me.lblFAR_14.Text = "871"
    Me.lblFAR_14.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'dbgGlanzGrad
    '
    Me.dbgGlanzGrad.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.dbgGlanzGrad.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgGlanzGrad.Location = New System.Drawing.Point(650, 16)
    Me.dbgGlanzGrad.Name = "dbgGlanzGrad"
    Me.dbgGlanzGrad.Size = New System.Drawing.Size(306, 126)
    Me.dbgGlanzGrad.TabIndex = 80
    '
    'frmFarbmittelEing
    '
    Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
    Me.BackColor = System.Drawing.SystemColors.Control
    Me.ClientSize = New System.Drawing.Size(960, 606)
    Me.ControlBox = False
    Me.Controls.Add(Me.dbgGlanzGrad)
    Me.Controls.Add(Me.lblFAR_14)
    Me.Controls.Add(Me.cboART_2)
    Me.Controls.Add(Me.txtFAR_11)
    Me.Controls.Add(Me.lblFAR_13)
    Me.Controls.Add(Me.cboFarbmittel)
    Me.Controls.Add(Me.lblFarbmittel)
    Me.Controls.Add(Me.btnQuit)
    Me.Controls.Add(Me.hscFarbe_3)
    Me.Controls.Add(Me.lblFarbe_3)
    Me.Controls.Add(Me.BindingNavigatorFarbe)
    Me.Controls.Add(Me.BindingNavigatorProb)
    Me.Controls.Add(Me.BindingNavigatorProz)
    Me.Controls.Add(Me.BindingNavigatorPreis)
    Me.Controls.Add(Me.DataGridViewProb)
    Me.Controls.Add(Me.DataGridViewProz)
    Me.Controls.Add(Me.DataGridViewPreis)
    Me.Controls.Add(Me.cboMSH)
    Me.Controls.Add(Me.lblFarbe_2)
    Me.Controls.Add(Me.hscFarbe_2)
    Me.Controls.Add(Me.lblFarbe_1)
    Me.Controls.Add(Me.hscFarbe_1)
    Me.Controls.Add(Me.lblFarbe_0)
    Me.Controls.Add(Me.hscFarbe_0)
    Me.Controls.Add(Me.picGridFarbe)
    Me.Controls.Add(Me.txtFAR_10)
    Me.Controls.Add(Me.txtFAR_09)
    Me.Controls.Add(Me.txtFAR_08)
    Me.Controls.Add(Me.txtFAR_07)
    Me.Controls.Add(Me.txtFAR_06)
    Me.Controls.Add(Me.txtFAR_05)
    Me.Controls.Add(Me.txtFAR_04)
    Me.Controls.Add(Me.txtFAR_03)
    Me.Controls.Add(Me.txtFAR_02)
    Me.Controls.Add(Me.txtFAR_01)
    Me.Controls.Add(Me.txtFAR_00)
    Me.Controls.Add(Me.cboART_1)
    Me.Controls.Add(Me.cboART_0)
    Me.Controls.Add(Me.lblFAR_04)
    Me.Controls.Add(Me.lblFAR_00)
    Me.Controls.Add(Me.lblMSH)
    Me.Controls.Add(Me.lblFAR_03)
    Me.Controls.Add(Me.lblFAR_10)
    Me.Controls.Add(Me.lblFAR_09)
    Me.Controls.Add(Me.lblFAR_12)
    Me.Controls.Add(Me.lblFAR_08)
    Me.Controls.Add(Me.lblFAR_07)
    Me.Controls.Add(Me.lblFAR_06)
    Me.Controls.Add(Me.lblFAR_05)
    Me.Controls.Add(Me.lblFAR_11)
    Me.Controls.Add(Me.lblFAR_02)
    Me.Controls.Add(Me.lblFAR_01)
    Me.Controls.Add(Me.txtColor)
    Me.Cursor = System.Windows.Forms.Cursors.Default
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
    Me.Location = New System.Drawing.Point(16, 76)
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmFarbmittelEing"
    Me.RightToLeft = System.Windows.Forms.RightToLeft.No
    CType(Me.picGridFarbe, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.DataGridViewPreis, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.DataGridViewProz, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.DataGridViewProb, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.BindingNavigatorPreis, System.ComponentModel.ISupportInitialize).EndInit()
    Me.BindingNavigatorPreis.ResumeLayout(False)
    Me.BindingNavigatorPreis.PerformLayout()
    CType(Me.BindingNavigatorProz, System.ComponentModel.ISupportInitialize).EndInit()
    Me.BindingNavigatorProz.ResumeLayout(False)
    Me.BindingNavigatorProz.PerformLayout()
    CType(Me.BindingNavigatorProb, System.ComponentModel.ISupportInitialize).EndInit()
    Me.BindingNavigatorProb.ResumeLayout(False)
    Me.BindingNavigatorProb.PerformLayout()
    CType(Me.BindingNavigatorFarbe, System.ComponentModel.ISupportInitialize).EndInit()
    Me.BindingNavigatorFarbe.ResumeLayout(False)
    Me.BindingNavigatorFarbe.PerformLayout()
    CType(Me.dbgGlanzGrad, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)
    Me.PerformLayout()

  End Sub
#End Region

  Dim check As Boolean

  'Anzahl Reflexionskurven (+Zusatzinformation) QU-Kontrolle
  '
  Dim Nqu As Short
  Dim ier As Short
  Dim Npam As Short
  Dim Iprn As Short
  Dim SqlStmt As String
  '
  '

  ' Laufvariable
  Dim i As Short
  Dim l As Short
  Dim k As Short
  Dim j As Short
  Dim kw As Short
  ' Zwischengroessen
  Dim nkw As Short
  'Message Kennung
  Dim imsg As Short 'Messagebox (Rückgabewert)
  Dim MaxID As Integer 'Maximale ID (Primärschlüssel)
  '
  '
  Dim LiIndPr As Short
  Dim LiIndPf As Short
  Dim LiIndPb As Short
  '
  '
  Dim PicFarbe As Integer
  Dim PicColor As Color
  Dim DataFarbe As DataSet
  Dim OleAdMisch As OleDbDataAdapter
  Dim OleAdFarb As OleDbDataAdapter
  Dim OleAdPreis As OleDbDataAdapter
  Dim OleAdProz As OleDbDataAdapter
  Dim OleAdProb As OleDbDataAdapter
  Dim HandleRezept As HandleRezGeneral
  Dim WithEvents ConnFarbe As BindingSource
  Dim WithEvents ConnPreis As BindingSource
  Dim WithEvents ConnProz As BindingSource
  Dim WithEvents ConnProb As BindingSource
  Dim ViewFarbGlzGrd As DataView
  Dim ViewGlzGrd As DataView
  Dim TblMisch As DataTable
  Dim TblFarb As DataTable
  Dim TblPreis As DataTable
  Dim TblProz As DataTable
  Dim TblProb As DataTable
  Dim ViewPreis As DataView
  Dim ViewProz As DataView
  Dim ViewProb As DataView
  Dim claFAR As New ColorReadWriteDaten.ReadWriteFarbe
  Dim ArbFarb As New Colorant
  Dim OldMischID As Integer = -1
  Dim lblFAR As List(Of System.Windows.Forms.Label)
  Dim txtfar As List(Of TextBox)
  Dim cboART As List(Of ComboBox)
  Dim hscfarbe As List(Of HScrollBar)
  Dim lblFarbe As List(Of System.Windows.Forms.Label)

  Private Sub frmFarbmittelEing_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    AufbauPar.MethID = -1

  End Sub

  Private Sub frmFarbmittelEing_Closing(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.FormClosing
    Call SpeiFarbmittel()

  End Sub



  Private Sub frmFarbmittelEing_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
    Dim i As Integer
    '
    '
    '
    HandleRezept = New HandleRezGeneral
    '
    '
    '
    Me.Text = Texxt(1850) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    lblFAR = New List(Of System.Windows.Forms.Label)
    txtfar = New List(Of TextBox)
    cboART = New List(Of ComboBox)
    hscfarbe = New List(Of HScrollBar)
    lblFarbe = New List(Of System.Windows.Forms.Label)

    '
    '
    DataFarbe = New DataSet
    '
    '
    '
    'Bindingsource
    '
    ConnFarbe = New BindingSource
    ConnPreis = New BindingSource
    ConnProz = New BindingSource
    ConnProb = New BindingSource
    '
    'Tabellen
    '
    '
    '
    TblMisch = New DataTable
    TblFarb = New DataTable
    TblPreis = New DataTable
    TblProz = New DataTable
    TblProb = New DataTable
    ViewPreis = New DataView
    ViewProz = New DataView
    ViewProb = New DataView


    '
    '
    DataFarbe.Tables.Add(TblFarb)
    DataFarbe.Tables.Add(TblPreis)
    DataFarbe.Tables.Add(TblProz)
    DataFarbe.Tables.Add(TblProb)

    ConnPreis.DataSource = ViewPreis
    ConnProz.DataSource = ViewProz
    ConnProb.DataSource = ViewProb
    ConnFarbe.DataSource = TblFarb
    '
    BindingNavigatorFarbe.BindingSource = ConnFarbe
    BindingNavigatorPreis.BindingSource = ConnPreis
    BindingNavigatorProz.BindingSource = ConnProz
    BindingNavigatorProb.BindingSource = ConnProb
    '
    '
    '
    '
    OleAdMisch = New OleDbDataAdapter
    OleAdMisch.SelectCommand = New OleDbCommand("", Cncol)
    '
    '
    '
    '
    OleAdFarb = New OleDbDataAdapter
    OleAdPreis = New OleDbDataAdapter
    OleAdProz = New OleDbDataAdapter
    OleAdProb = New OleDbDataAdapter
    OleAdFarb.SelectCommand = New OleDbCommand("SELECT * FROM TBL_FARBM WHERE MISCH_ID=? ORDER BY FARBM_NAME", Cndat)
    OleAdFarb.SelectCommand.Parameters.Add("MISCH_ID", OleDbType.Integer)
    OleAdPreis.SelectCommand = New OleDbCommand("SELECT * FROM TBL_FARBM_PREIS WHERE MISCH_ID=?", Cndat)
    OleAdPreis.SelectCommand.Parameters.Add("MISCH_ID", OleDbType.Integer)
    OleAdProz.SelectCommand = New OleDbCommand("SELECT * FROM TBL_FARBM_PROZ WHERE MISCH_ID=?", Cndat)
    OleAdProz.SelectCommand.Parameters.Add("MISCH_ID", OleDbType.Integer)
    OleAdProb.SelectCommand = New OleDbCommand("SELECT * FROM TBL_FARBM_PROB WHERE MISCH_ID=?", Cndat)
    OleAdProb.SelectCommand.Parameters.Add("MISCH_ID", OleDbType.Integer)
    '
    'Schemata einlesen
    '
    '
    OleAdFarb.SelectCommand.Parameters(0).Value = -100
    If Not FillDatset(OleAdFarb, TblFarb) Then
      Exit Sub
    End If
    TblFarb.Columns("FARBM_ID").Unique = True
    TblFarb.Columns("FARBM_ID").AutoIncrement = True
    '
    '
    TblFarb.PrimaryKey = New DataColumn() {TblFarb.Columns("FARBM_ID")}
    '
    '
   

   
    '
    '
    'Preise
    '
    '
    OleAdPreis.SelectCommand.Parameters(0).Value = -100
    If Not FillDatset(OleAdPreis, TblPreis) Then
      Exit Sub
    End If
    TblPreis.TableName = "TBL_PREIS"
    TblPreis.Columns("FARBM_PREIS").DefaultValue = 0.0
    TblPreis.PrimaryKey = New DataColumn() {TblPreis.Columns("FARBM_ID"), TblPreis.Columns("FARBM_IRPA")}
    TblPreis.Columns("MISCH_ID").DefaultValue = MenueParam.MischID
    DataGridViewPreis.DataSource = ConnPreis
    '
    '
    '
    ViewPreis.Table = TblPreis
    ViewPreis.Sort = "FARBM_IRPA"
    '
    '
    'Prozentigkeiten
    '
    '
    '
    OleAdProz.SelectCommand.Parameters(0).Value = -100
    If Not FillDatset(OleAdProz, TblProz) Then
      Exit Sub
    End If
    '
    TblProz.TableName = "TBL_PROZ"
    TblProz.Columns("FARBM_PROZ").DefaultValue = 0.0
    TblProz.PrimaryKey = New DataColumn() {TblProz.Columns("FARBM_ID"), TblProz.Columns("FARBM_IRFA")}
    TblProz.Columns("MISCH_ID").DefaultValue = MenueParam.MischID
    DataGridViewProz.DataSource = ConnProz
    '
    '
    '
    ViewProz.Table = TblProz
    ViewProz.Sort = "FARBM_IRFA"
    '
    '
    '
    '
    'Bindemittelprozentigkeiten
    '
    '
    '
    OleAdProb.SelectCommand.Parameters(0).Value = -100
    If Not FillDatset(OleAdProb, TblProb) Then
      Exit Sub
    End If
    TblProb.TableName = "TBL_PROB"
    TblProb.Columns("FARBM_PROB").DefaultValue = 0.0
    TblProb.PrimaryKey = New DataColumn() {TblProb.Columns("FARBM_ID"), TblProb.Columns("FARBM_IRBA")}
    TblProb.Columns("MISCH_ID").DefaultValue = MenueParam.MischID
    DataGridViewProb.DataSource = ConnProb
    '
    '
    '
    ViewProb.Table = TblProb
    ViewProb.Sort = "FARBM_IRBA"
    '
    '
    '
    'Relationen werden nicht verwendet 
    '
    '
    '
    'DataFarbe.Relations.Add(New DataRelation("FARBPREIS", New DataColumn() {TblFarb.Columns("FARBM_ID")}, New DataColumn() {TblPreis.Columns("FARBM_ID")}))
    'DataFarbe.Relations("FARBPREIS").ChildKeyConstraint.DeleteRule = Rule.Cascade
    'DataFarbe.Relations("FARBPREIS").ChildKeyConstraint.UpdateRule = Rule.SetDefault

    '
    '
    '
    '
    'Gridaufbau
    '
    '

    '
    '
    Call GridDetailsCustomize(Texxt(904), DataGridViewPreis)
    Call GridDetailsCustomize(Texxt(906), DataGridViewProz)
    Call GridDetailsCustomize(Texxt(907), DataGridViewProb)
    '   
    '
    '
    '
    '
    lblFAR.Add(lblFAR_00)
    lblFAR.Add(lblFAR_01)
    lblFAR.Add(lblFAR_02)
    lblFAR.Add(lblFAR_03)
    lblFAR.Add(lblFAR_04)
    lblFAR.Add(lblFAR_05)
    lblFAR.Add(lblFAR_06)
    lblFAR.Add(lblFAR_07)
    lblFAR.Add(lblFAR_08)
    lblFAR.Add(lblFAR_09)
    lblFAR.Add(lblFAR_10)
    lblFAR.Add(lblFAR_11)
    lblFAR.Add(lblFAR_12)
    lblFAR.Add(lblFAR_13)
    lblFAR.Add(lblFAR_14)
    '
    '
    'Farbdarstellung
    '
    lblFarbe.Add(lblFarbe_0)
    lblFarbe.Add(lblFarbe_1)
    lblFarbe.Add(lblFarbe_2)
    lblFarbe.Add(lblFarbe_3)
    '
    '
    lblFarbe(0).Text = "Alpha"
    lblFarbe(1).Text = Texxt(CInt(lblFarbe(1).Text))
    lblFarbe(2).Text = Texxt(CInt(lblFarbe(2).Text))
    lblFarbe(3).Text = Texxt(CInt(lblFarbe(3).Text))

    '
    '
    hscfarbe.Add(hscFarbe_0)
    hscfarbe.Add(hscFarbe_1)
    hscfarbe.Add(hscFarbe_2)
    hscfarbe.Add(hscFarbe_3)

    '
    btnQuit.Text = Texxt(3004)
    '
    For i = 0 To lblFAR.Count - 1
      lblFAR(i).Text = Texxt(CInt(lblFAR(i).Text))
    Next
    '
    '
    lblMSH.Text = Texxt(CInt(lblMSH.Text))
    '
    '
    '
    '
    txtfar.Add(txtFAR_00)
    txtfar.Add(txtFAR_01)
    txtfar.Add(txtFAR_02)
    txtfar.Add(txtFAR_03)
    txtfar.Add(txtFAR_04)
    txtfar.Add(txtFAR_05)
    txtfar.Add(txtFAR_06)
    txtfar.Add(txtFAR_07)
    txtfar.Add(txtFAR_08)
    txtfar.Add(txtFAR_09)
    txtfar.Add(txtFAR_10)
    txtfar.Add(txtFAR_11)


    '
    '

    '
    '
    '
    '
    cboART.Add(cboART_0)
    cboART.Add(cboART_1)
    cboART.Add(cboART_2)
    '
    '
    '
    '
    '

    '
    '
    '
    check = False
    cboART(0).Items.Clear()
    For i = 0 To 8
      cboART(0).Items.Add(Texxt(980 + i))
    Next i
    cboART(0).SelectedIndex = -1
    cboART(1).Items.Clear()
    For i = 0 To 1
      cboART(1).Items.Add(Texxt(990 + i))
    Next i
    cboART(1).SelectedIndex = -1
    ViewFarbGlzGrd = New DataView(TblFarb)
    cboART_2.DataSource = ViewFarbGlzGrd
    '
    '
    '
    '
    ViewGlzGrd = New DataView(TblFarb)
    '
    '
    '
    dbgGlanzGrad.DataSource = ViewGlzGrd
    For i = 0 To dbgGlanzGrad.Columns.Count - 1
      dbgGlanzGrad.Columns(i).Visible = False
    Next
    dbgGlanzGrad.Columns("FARBM_GLZGRD").Visible = True
    dbgGlanzGrad.Columns("FARBM_NAME").Visible = True
    dbgGlanzGrad.Columns("FARBM_GLZGRD").HeaderText = Texxt(870)
    dbgGlanzGrad.Columns("FARBM_NAME").HeaderText = Texxt(815)
    dbgGlanzGrad.RowHeadersVisible = False
    dbgGlanzGrad.ReadOnly = True
    dbgGlanzGrad.AllowUserToAddRows = False
    dbgGlanzGrad.AllowUserToDeleteRows = False
    dbgGlanzGrad.Columns("FARBM_NAME").Width = 200
    Application.DoEvents()
    '
    '
    'Mischsysteme
    '
    '
    If MenueParam.MischID <> -1 Then
      OleAdMisch.SelectCommand.CommandText = HandleRezept.MischSelectCommand()
      If Not FillDatset(OleAdMisch, TblMisch) Then
        Exit Sub
      End If
      TblMisch.AcceptChanges()
      cboMSH.DataSource = TblMisch
      cboMSH.DisplayMember = "MISCH_KBEZ"
      cboMSH.ValueMember = "MISCH_ID"

      cboMSH.SelectedIndex = -1
      cboMSH.SelectedValue = MenueParam.MischID
    Else
      MsgBox(Texxt(3601))
    End If
    '
    'Mischsystem Enabled / Visible
    '
    '
    If BitWrt(23, MenueParam.User.Enabl) Then
      cboMSH.Enabled = True
    Else
      cboMSH.Enabled = False
    End If
    If BitWrt(23, MenueParam.User.Visbl) Then
      cboMSH.Visible = True
      lblMSH.Visible = True
    Else
      cboMSH.Visible = False
      lblMSH.Visible = False
    End If
    '
    '
    '
    '
    lblFarbmittel.Text = Texxt(801)
    '
    '
    '
    '
    '
    '
    '
    For i = 0 To txtfar.Count - 1
      txtfar(i).DataBindings.Clear()
    Next

    txtfar(0).DataBindings.Add("TEXT", ConnFarbe, "FARBM_ID")
    txtfar(1).DataBindings.Add("TEXT", ConnFarbe, "FARBM_NAME")
    txtfar(2).DataBindings.Add("TEXT", ConnFarbe, "FARBM_ANAME")
    txtfar(3).DataBindings.Add("TEXT", ConnFarbe, "FARBM_BEM")
    txtfar(4).DataBindings.Add("TEXT", ConnFarbe, "FARBM_PRNR")
    txtfar(5).DataBindings.Add("TEXT", ConnFarbe, "FARBM_SPZ")
    txtfar(6).DataBindings.Add("TEXT", ConnFarbe, "FARBM_FST")
    txtfar(7).DataBindings.Add("TEXT", ConnFarbe, "FARBM_BEL")
    txtfar(8).DataBindings.Add("TEXT", ConnFarbe, "FARBM_SMENGE")
    txtfar(9).DataBindings.Add("TEXT", ConnFarbe, "FARBM_FORMAT")
    txtfar(10).DataBindings.Add("TEXT", ConnFarbe, "FARBM_EFF")
    txtfar(11).DataBindings.Add("TEXT", ConnFarbe, "FARBM_GLZGRD")

    txtColor.DataBindings.Clear()
    txtColor.DataBindings.Add("TEXT", ConnFarbe, "FARBM_FARBID")
    cboART(0).DataBindings.Clear()
    cboART(1).DataBindings.Clear()
    cboART(2).DataBindings.Clear()
    cboART(0).DataBindings.Add("SELECTEDINDEX", ConnFarbe, "FARBM_ICHF")
    cboART(1).DataBindings.Add("SELECTEDINDEX", ConnFarbe, "FARBM_IBAS")

    '
    cboART_2.DataSource = ViewFarbGlzGrd

    cboART(2).DisplayMember = "FARBM_NAME"
    cboART(2).ValueMember = "FARBM_ID"
    cboART(2).DataBindings.Add("SELECTEDVALUE", ConnFarbe, "GLZGRD_ID")
    '
    cboFarbmittel.DataSource = ConnFarbe
    cboFarbmittel.DisplayMember = "FARBM_NAME"
    cboFarbmittel.ValueMember = "FARBM_ID"
    check = True
  End Sub

  Private Sub frmFarbmittelEing_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize
    Call ResizeChild(Me)
  End Sub

  '
  '
  'Mischsystem auswählen
  '
  '
  '
  Sub cboMSH_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMSH.SelectedIndexChanged
    If sender.Selectedindex = -1 Then Exit Sub
    If Not IsNumeric(sender.Selectedvalue) Then Exit Sub
    If sender.SelectedValue = OldMischID Then Exit Sub
    If OldMischID >= 0 Then
      Call SpeiFarbmittel()
    End If
    OldMischID = sender.SelectedValue
    AufbauPar.MischID = sender.SelectedValue
    '
    '
    '
    Call FillFarbTable()
    '
    If ConnFarbe.Count > 0 Then
      Call Afterfirst(True)
    Else
      Call Afterfirst(False)

    End If

    '
    ''
  End Sub
  Sub FillFarbTable()
    Dim i As Integer
    '

    '
    ConnFarbe.CurrencyManager.SuspendBinding()
    TblPreis.Rows.Clear()
    TblProz.Rows.Clear()
    TblProb.Rows.Clear()
    TblFarb.Rows.Clear()
    '
    '
    '
    'Farb-/Bindemittel
    '
    '
    '
    '
    OleAdFarb.SelectCommand.Parameters(0).Value = MenueParam.MischID
    If Not FillDatset(OleAdFarb, TblFarb) Then
      Exit Sub
    End If
    TblFarb.AcceptChanges()

    '
    ConnFarbe.CurrencyManager.ResumeBinding()
    '
    '
    txtfar(1).MaxLength = TblFarb.Columns("FARBM_NAME").MaxLength
    txtfar(2).MaxLength = TblFarb.Columns("FARBM_ANAME").MaxLength
    txtfar(3).MaxLength = TblFarb.Columns("FARBM_BEM").MaxLength
    txtfar(4).MaxLength = TblFarb.Columns("FARBM_PRNR").MaxLength
    TblFarb.Columns("MISCH_ID").DefaultValue = MenueParam.MischID
    If TblFarb.Rows.Count = 0 Then
      TblFarb.Columns("FARBM_ID").AutoIncrementSeed = 0
    Else
      TblFarb.Columns("FARBM_ID").AutoIncrementSeed = TblFarb.Rows(TblFarb.Rows.Count - 1)("FARBM_ID") + 1
    End If
    TblFarb.AcceptChanges()
    For i = 0 To TblFarb.Rows.Count - 1
      If IsDBNull(TblFarb.Rows(i)("GLZGRD_ID")) Then
        TblFarb.Rows(i)("GLZGRD_ID") = TblFarb.Rows(i)("FARBM_ID")
      End If
      If IsDBNull(TblFarb.Rows(i)("FARBM_GLZGRD")) Then
        TblFarb.Rows(i)("FARBM_GLZGRD") = 0.0
      End If
    Next
    Call DefaultStartValuesFarbe()

    '    '
    '
    'Preise
    '
    '
    '
    OleAdPreis.SelectCommand.Parameters(0).Value = MenueParam.MischID
    If Not FillDatset(OleAdPreis, TblPreis) Then
      Exit Sub
    End If
    TblPreis.AcceptChanges()
    '
    '
    '
    '
    '
    '
    'Prozentigkeiten
    '
    '
    '
    OleAdProz.SelectCommand.Parameters(0).Value = MenueParam.MischID
    If Not FillDatset(OleAdProz, TblProz) Then
      Exit Sub
    End If
    TblProz.AcceptChanges()

    '
    '
    '
    'Bindemittelprozentigkeiten
    '
    '
    '
    OleAdProb.SelectCommand.Parameters(0).Value = MenueParam.MischID
    If Not FillDatset(OleAdProb, TblProb) Then
      Exit Sub
    End If
    TblProb.AcceptChanges()
    '
    ConnFarbe.ResetBindings(True)
    Application.DoEvents()
    '
    ''

  End Sub
  Private Sub GridDetailsCustomize(ByVal Head As String, ByVal DatagridDetail As DataGridView)
    Dim i As Integer
    '
    '
    '
    For i = 0 To DataGridViewPreis.Columns.Count - 1
      DatagridDetail.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
    Next

    DatagridDetail.RowHeadersWidth = 20
    DatagridDetail.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
    DatagridDetail.Columns(0).Visible = False
    DatagridDetail.Columns(1).Visible = False
    DatagridDetail.Columns(2).Visible = False
    DatagridDetail.Columns(3).HeaderText = Head
    DatagridDetail.Columns(3).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    DatagridDetail.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    DatagridDetail.Columns(3).DefaultCellStyle.Format = "###.000"
    ''
  End Sub
  Private Sub DefaultStartValuesFarbe()

    '
    '
    '
    If TblFarb.Columns.Count = 0 Then Exit Sub
    TblFarb.Columns("MISCH_ID").DefaultValue = MenueParam.MischID
    TblFarb.Columns("FARBM_NAME").DefaultValue = "???"
    TblFarb.Columns("FARBM_BEM").DefaultValue = ""
    TblFarb.Columns("FARBM_DATTIM").DefaultValue = Now
    TblFarb.Columns("FARBM_PRNR").DefaultValue = ""
    TblFarb.Columns("FARBM_SPZ").DefaultValue = 1.0
    TblFarb.Columns("FARBM_FST").DefaultValue = 100.0
    TblFarb.Columns("FARBM_BEL").DefaultValue = 1.0
    TblFarb.Columns("FARBM_SMENGE").DefaultValue = 0.0001
    TblFarb.Columns("FARBM_FORMAT").DefaultValue = "###.000"
    TblFarb.Columns("FARBM_EFF").DefaultValue = 1.0
    TblFarb.Columns("FARBM_ICHF").DefaultValue = 0
    TblFarb.Columns("FARBM_IBAS").DefaultValue = 0
    TblFarb.Columns("FARBM_FARBID").DefaultValue = 0
    TblFarb.Columns("FARBM_GLZGRD").DefaultValue = 0.0#

  End Sub

  Private Sub DefaultValuesFarbe(ByVal OldRow As DataRow)
    TblFarb.Columns("FARBM_NAME").DefaultValue = OldRow("FARBM_NAME") & "???"
    TblFarb.Columns("FARBM_BEM").DefaultValue = OldRow("FARBM_BEM")
    TblFarb.Columns("FARBM_DATTIM").DefaultValue = Now
    TblFarb.Columns("FARBM_PRNR").DefaultValue = OldRow("FARBM_PRNR")
    TblFarb.Columns("FARBM_SPZ").DefaultValue = OldRow("FARBM_SPZ")
    TblFarb.Columns("FARBM_FST").DefaultValue = OldRow("FARBM_FST")
    TblFarb.Columns("FARBM_BEL").DefaultValue = OldRow("FARBM_BEL")
    TblFarb.Columns("FARBM_SMENGE").DefaultValue = OldRow("FARBM_SMENGE")
    TblFarb.Columns("FARBM_FORMAT").DefaultValue = OldRow("FARBM_FORMAT")
    TblFarb.Columns("FARBM_EFF").DefaultValue = OldRow("FARBM_EFF")
    TblFarb.Columns("FARBM_ICHF").DefaultValue = OldRow("FARBM_ICHF")
    TblFarb.Columns("FARBM_IBAS").DefaultValue = OldRow("FARBM_IBAS")
    TblFarb.Columns("FARBM_FARBID").DefaultValue = OldRow("FARBM_FARBID")
    TblFarb.Columns("FARBM_GLZGRD").DefaultValue = OldRow("FARBM_GLZGRD")

  End Sub


  Sub SpeiFarbmittel()
    Dim ViewPre As DataView
    Dim ViewPrz As DataView
    Dim ViewPrb As DataView

    Dim FarbName As String
    Dim ViewFarbe As DataView
    Dim ArbFarb As Colorant
    Dim ReadWrite As ReadWriteFarbe
    Dim ViewRow As DataRowView
    Dim ier As Integer
    Dim Res As DialogResult
    ViewPre = New DataView
    ViewPrz = New DataView
    ViewPrb = New DataView
    ViewPre.Table = TblPreis
    ViewPre.Sort = "FARBM_IRPA"
    ViewPrz.Table = TblProz
    ViewPrz.Sort = "FARBM_IRFA"
    ViewPrb.Table = TblProb
    ViewPrb.Sort = "FARBM_IRBA"

    '
    'Farbmitteltabellen abspeichern
    '
    ConnPreis.CurrencyManager.EndCurrentEdit()
    ConnProz.CurrencyManager.EndCurrentEdit()
    ConnProb.CurrencyManager.EndCurrentEdit()
    ConnFarbe.CurrencyManager.EndCurrentEdit()

    '
    '
    'Unfertige ADDNEW's beenden
    '
    If Not IsNothing(ConnPreis.Current) AndAlso ConnPreis.Position > -1 AndAlso ConnPreis.Current.row.rowstate = DataRowState.Detached Then
      ConnPreis.EndEdit()
    End If
    '
    '
    If Not IsNothing(ConnProz.Current) AndAlso ConnProz.Position > -1 AndAlso ConnProz.Current.row.rowstate = DataRowState.Detached Then
      ConnProz.EndEdit()
    End If
    '
    '
    '
    If Not IsNothing(ConnProb.Current) AndAlso ConnProb.Position > -1 AndAlso ConnProb.Current.row.rowstate = DataRowState.Detached Then
      ConnProb.EndEdit()
    End If
    '
    '
    '
    '
    '
    If Not IsNothing(ConnFarbe.Current) AndAlso ConnFarbe.Position > -1 AndAlso ConnFarbe.Current.row.rowstate = DataRowState.Detached Then
      ConnFarbe.EndEdit()
    End If
    '
    '
    ConnFarbe.SuspendBinding()
    Cursor = Cursors.WaitCursor
    Me.Enabled = False
    ReadWrite = New ReadWriteFarbe
    ViewFarbe = New DataView(TblFarb)
    ArbFarb = New Colorant
    '
    '
    '
    'Delete Farbmittel
    '
    '
    '
    '
    '
    If ConnOpen(Cndat) Then
      ViewFarbe.RowStateFilter = DataViewRowState.Deleted

      ViewFarbe.RowFilter = ""
      '
      If ViewFarbe.Count > 0 Then
        Res = MessageBox.Show(Texxt(3006), Texxt(2000), MessageBoxButtons.YesNo)
        If Res = Windows.Forms.DialogResult.Yes Then
          '
          '
          '
          '
          For Each ViewRow In ViewFarbe
            Do While ViewRow.Row.RowState = DataRowState.Deleted
              ViewRow.Row.RejectChanges()
              Application.DoEvents()
            Loop
            ArbFarb.ID = ViewRow.Row("FARBM_ID")
            ArbFarb.Name = ViewRow.Row("FARBM_Name")
            ViewRow.Row.Delete()
            Call ReadWrite.FarDel(ArbFarb, True, ier)
          Next
        End If
      End If
      '
      '
      'Update Farbmittel
      '
      '
      'Prüfen, ob CONNPreis,CONNProb oder CONNProz geändert
      '
      ViewFarbe.RowStateFilter = DataViewRowState.Unchanged
      ViewPre.RowStateFilter = DataViewRowState.Added Or DataViewRowState.Deleted Or DataViewRowState.ModifiedCurrent
      ViewPrz.RowStateFilter = DataViewRowState.Added Or DataViewRowState.Deleted Or DataViewRowState.ModifiedCurrent
      ViewPrb.RowStateFilter = DataViewRowState.Added Or DataViewRowState.Deleted Or DataViewRowState.ModifiedCurrent

      '
      ViewFarbe.RowFilter = ""
      If ViewFarbe.Count > 0 Then
        '
        '
        '
        For Each ViewRow In ViewFarbe
          '
          'ModifiedCurrent in ViewFarbe erzeugen, falls Preise geändert wurden
          '
          '
          ViewPre.RowFilter = "FARBM_ID=" & ViewRow.Row("FARBM_ID")
          If ViewPre.Count <> 0 Then
            FarbName = ViewRow.Row("FARBM_NAME")
            ViewRow.Row("FARBM_NAME") = ""
            ViewRow.Row("FARBM_NAME") = FarbName
          End If
          '
          'ModifiedCurrent in ViewFarbe erzeugen, falls Prozentigkeiten geändert wurden
          '
          '

          ViewPrz.RowFilter = "FARBM_ID=" & ViewRow.Row("FARBM_ID")
          If ViewPrz.Count <> 0 Then
            FarbName = ViewRow.Row("FARBM_NAME")
            ViewRow.Row("FARBM_NAME") = ""
            ViewRow.Row("FARBM_NAME") = FarbName
          End If
          '
          'ModifiedCurrent in ViewFarbe erzeugen, falls Bindemittelprozentigkeiten geändert wurden
          '
          '
          ViewPrb.RowFilter = "FARBM_ID=" & ViewRow.Row("FARBM_ID")
          If ViewPrb.Count <> 0 Then
            FarbName = ViewRow.Row("FARBM_NAME")
            ViewRow.Row("FARBM_NAME") = ""
            ViewRow.Row("FARBM_NAME") = FarbName
          End If

        Next
      End If
      '
      '
      ViewFarbe.RowStateFilter = DataViewRowState.ModifiedCurrent
      '
      ViewFarbe.RowFilter = ""
      '
      If ViewFarbe.Count > 0 Then
        Res = MessageBox.Show(Texxt(3008), Texxt(2000), MessageBoxButtons.YesNo)
        If Res = Windows.Forms.DialogResult.Yes Then

          '
          '
          '
          For Each ViewRow In ViewFarbe
            '
            Call UmspeiFarbe(ArbFarb, ViewRow.Row)
            '
            Call ReadWrite.FarUpd(ArbFarb, ier)
          Next
        End If
      End If
      '
      '
      'Add Farbmittel
      '
      '
      '
      '
      '
      ViewFarbe.RowStateFilter = DataViewRowState.Added
      '
      ViewFarbe.RowFilter = ""
      '
      '
      If ViewFarbe.Count > 0 Then
        Res = MessageBox.Show(Texxt(3007), Texxt(2000), MessageBoxButtons.YesNo)
        If Res = Windows.Forms.DialogResult.Yes Then
          '
          '
          '
          For Each ViewRow In ViewFarbe
            '
            Call UmspeiFarbe(ArbFarb, ViewRow.Row)
            '
            Call ReadWrite.FarAdd(ArbFarb, ier)
          Next
        End If
      End If
    End If
    '
    ViewPre.Dispose()
    ViewPrz.Dispose()
    ViewPrb.Dispose()

    ReadWrite.dispose()
    ViewFarbe.Dispose()
    ArbFarb.dispose()
    TblFarb.AcceptChanges()
    TblPreis.AcceptChanges()
    TblProz.AcceptChanges()
    TblProb.AcceptChanges()
    Me.Enabled = True
    Cursor = Cursors.Default

    ConnFarbe.ResumeBinding()
    Application.DoEvents()
  End Sub
  Sub UmspeiFarbe(ByVal ArbFarb As Colorant, ByVal FarbRow As DataRow)
    Dim j As Integer

    ArbFarb.ID = FarbRow("FARBM_ID")
    ArbFarb.Name = FarbRow("FARBM_NAME")
    If IsDBNull(FarbRow("FARBM_BEM")) Then
      ArbFarb.Bem = ""
    Else
      ArbFarb.Bem = FarbRow("FARBM_BEM")
    End If
    If IsDBNull(FarbRow("FARBM_ANAME")) Then
      FarbRow("FARBM_ANAME") = ""
    End If
    ArbFarb.Aname = FarbRow("FARBM_ANAME")
    ArbFarb.Ichf = FarbRow("FARBM_ICHF")
    ArbFarb.Ibas = FarbRow("FARBM_IBAS")
    ArbFarb.Fst = FarbRow("FARBM_FST")
    If ArbFarb.Fst = 0 Then ArbFarb.Fst = 100
    ArbFarb.Bel = FarbRow("FARBM_BEL")
    If ArbFarb.Bel = 0 Then ArbFarb.Bel = 1
    ArbFarb.Spz = FarbRow("FARBM_SPZ")
    If ArbFarb.Spz = 0 Then ArbFarb.Spz = 1
    ArbFarb.Eff = FarbRow("FARBM_EFF")
    If ArbFarb.Eff = 0 Then ArbFarb.Eff = 1
    ArbFarb.Form = FarbRow("FARBM_FORMAT")
    If Trim(ArbFarb.Form) = "" Then ArbFarb.Form = "###.000"
    ArbFarb.PrNr = FarbRow("FARBM_PRNR")
    ArbFarb.Smenge = FarbRow("FARBM_SMENGE")
    If ArbFarb.Smenge = 0 Then ArbFarb.Smenge = 0.001
    ArbFarb.FarbID = FarbRow("FARBM_FARBID")
    If IsDBNull(FarbRow("GLZGRD_ID")) Then
      FarbRow("GLZGRD_ID") = ArbFarb.ID
    End If
    ArbFarb.GlzGrdID = FarbRow("GLZGRD_ID")
    If IsDBNull(FarbRow("FARBM_GLZGRD")) Then
      FarbRow("FARBM_GLZGRD") = 0.0
    End If
    ArbFarb.GlzGrd = FarbRow("FARBM_GLZGRD")
    '
    'Preise
    '
    '
    ViewPreis.RowStateFilter = DataViewRowState.CurrentRows
    ViewPreis.RowFilter = "FARBM_ID=" & ArbFarb.ID
    For j = 0 To ViewPreis.Count - 1
      ArbFarb.Pre(j) = ViewPreis(j).Row("FARBM_PREIS")
    Next j
    If ViewPreis.Count = 0 Then ArbFarb.Pre(0) = 0.0
    '
    '
    'Prozentigkeiten
    '
    '
    ViewProz.RowStateFilter = DataViewRowState.CurrentRows
    ViewProz.RowFilter = "FARBM_ID=" & ArbFarb.ID
    For j = 0 To ViewProz.Count - 1
      ArbFarb.Prf(j) = ViewProz(j).Row("FARBM_PROZ")
    Next j
    If ViewProz.Count = 0 Then ArbFarb.Prf(0) = 100.0
    If ArbFarb.Prf(0) <> 100.0 Then ArbFarb.Prf(0) = 100.0

    '
    'Bindemittelprozentigkeiten
    '
    '
    ViewProb.RowStateFilter = DataViewRowState.CurrentRows
    ViewProb.RowFilter = "FARBM_ID=" & ArbFarb.ID
    For j = 0 To ViewProb.Count - 1
      ArbFarb.Prb(j) = ViewProb(j).Row("FARBM_PROB")
    Next j
    If ViewProb.Count = 0 Then ArbFarb.Prb(0) = 100.0
    If ArbFarb.Prb(0) <> 100.0 Then ArbFarb.Prb(0) = 100.0

  End Sub

  Private Sub ConnFarbe_AddingNew(sender As Object, e As System.ComponentModel.AddingNewEventArgs) Handles ConnFarbe.AddingNew
    Call Afterfirst(True)
  End Sub
  '
  '
  '
  '
  '
  'Events Farbe
  '
  '

  Private Sub ConnFarbe_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnFarbe.CurrentChanged
    Dim i As Integer
    Dim DatRow As DataRowView
    If ConnFarbe.Count = 0 Then Exit Sub

    '
    If ConnFarbe.Current.row.rowstate = DataRowState.Detached Then
      txtFAR_01.BackColor = Color.Red
      ConnFarbe.Current("GLZGRD_ID") = ConnFarbe.Current("FARBM_ID")
    End If
    If ConnFarbe.IsBindingSuspended Then Exit Sub
    ToolStripLabelFarbe.Text = ConnFarbe.Current.row("FARBM_NAME")
    '
    '
    'Farbfestlegung
    '
    PicFarbe = ConnFarbe.Current.row("FARBM_FARBID")
    PicColor = Color.FromArgb(PicFarbe)
    For i = 0 To 3
      hscfarbe(i).Enabled = False
    Next
    hscfarbe(0).Value = PicColor.A
    hscfarbe(1).Value = PicColor.R
    hscfarbe(2).Value = PicColor.G
    hscfarbe(3).Value = PicColor.B
    For i = 0 To 3
      hscfarbe(i).Enabled = True
    Next
    hscfarbe(0).Value = 255
    picGridFarbe.Refresh()
    '
    '
    'Nicht erledigte Addnew's beenden
    '
    '
    '
    If Not IsNothing(ConnPreis.Current) AndAlso ConnPreis.Position > -1 AndAlso ConnPreis.Current.row.rowstate = DataRowState.Detached Then

      '
      '
      'AddingNew für Preise beenden
      '
      '

      ConnPreis.EndEdit()

    End If
    '
    If Not IsNothing(ConnProz.Current) AndAlso ConnProz.Position > -1 AndAlso ConnProz.Current.row.rowstate = DataRowState.Detached Then

      '
      '
      'AddingNew für Prozentigkeiten beenden
      '
      '
      ConnProz.EndEdit()

    End If
    '
    '
    If Not IsNothing(ConnProb.Current) AndAlso ConnProb.Position > -1 AndAlso ConnProb.Current.row.rowstate = DataRowState.Detached Then

      '
      '
      'AddingNew für Bindemittelprozentigkeiten beenden
      '
      '
      ConnProb.EndEdit()

    End If
    '
    '
    '
    '
    '
    If IsNothing(sender.current) OrElse sender.position = -1 Then
      '
      '
      'Keine Zeile vorhanden
      '
      '
      '
      'Preise
      '
      '
      ConnPreis.AllowNew = False
      ViewPreis.RowFilter = "FARBM_ID=-1"
      If ViewPreis.Count > 0 Then
        '
        '
        MsgBox("Viewpreis kann nicht gelöscht werden; aktuelle Zeile ist detached")
      End If
      '
      'Prozentigkeiten
      '
      '
      ConnProz.AllowNew = False
      ViewProz.RowFilter = "FARBM_ID=-1"
      If ViewProz.Count > 0 Then
        '
        '
        MsgBox("Viewproz kann nicht gelöscht werden; aktuelle Zeile ist detached")
      End If
      '
      'Bindemittelprozentigkeiten
      '
      '
      ConnProb.AllowNew = False
      ViewProb.RowFilter = "FARBM_ID=-1"
      If ViewProb.Count > 0 Then
        '
        '
        MsgBox("Viewprob kann nicht gelöscht werden; aktuelle Zeile ist detached")
      End If

    Else
      '
      '
      '
      '
      '
      '
      'End Addnew für Preise (Viewcount wird um 1 erniedrigt)
      '
      If sender.count > 1 And Not sender.current.row.rowstate = DataRowState.Detached Then
        ConnPreis.EndEdit()
      End If
      '
      'End Addnew für Prozentigkeiten (Viewcount wird um 1 erniedrigt)
      '
      If sender.count > 1 And Not sender.current.row.rowstate = DataRowState.Detached Then
        ConnProz.EndEdit()
      End If

      '
      '
      '
      '
      'Bindemittelprozentigkeiten für neues Farbmittel übernehmen
      '
      'End Addnew für Bindemittelprozentigkeiten (Viewcount wird um 1 erniedrigt)
      '
      If sender.count > 1 And Not sender.current.row.rowstate = DataRowState.Detached Then
        ConnProb.EndEdit()
      End If

      '
    End If
    '
    'Dataview der aktuellen Preise übernehmen
    '
    ViewPreis.RowFilter = "FARBM_ID=" & sender.Current.row("FARBM_ID")
    '
    'Dataview der aktuellen Prozentigkeiten übernehmen
    '
    ViewProz.RowFilter = "FARBM_ID=" & sender.Current.row("FARBM_ID")
    '
    'Dataview der aktuellen Bindemittelprozentigkeiten übernehmen
    '
    ViewProb.RowFilter = "FARBM_ID=" & sender.Current.row("FARBM_ID")

    If sender.current.row.rowstate = DataRowState.Detached Then
      'Viewxxx.count wird nach Addnew um 1 erhöht (um die detached Zeile)
      'nach Aufruf eines zweiten Addnews(erzeugt ein Endedit)
      'oder eines Endedit wird Anzahl wieder zurückgesetzt (detached ===> Added)
      'Die RowFilter - Festlegung gilt nicht für detached Zeilen!!!!)
      '
      '
      If ConnPreis.Count = 0 Then
        DatRow = ConnPreis.AddNew
      End If
      If ConnProz.Count = 0 Then
        DatRow = ConnProz.AddNew
      End If
      If ConnProb.Count = 0 Then
        DatRow = ConnProb.AddNew()
      End If
    End If
    '
    '
   
    ViewGlzGrd.RowFilter = "GLZGRD_ID=" & ConnFarbe.Current("FARBM_ID")

  
  End Sub

  Private Sub ConnFarbe_ListChanged(ByVal sender As Object, ByVal e As System.ComponentModel.ListChangedEventArgs) Handles ConnFarbe.ListChanged
    If ConnFarbe.IsBindingSuspended Then Exit Sub
    If IsNothing(sender.current) OrElse sender.position = -1 Then
      ConnPreis.AllowNew = False
      ConnProz.AllowNew = False
      ConnProb.AllowNew = False
    Else
      '
      'Refresh
      '
      '
      ConnPreis.AllowNew = True
      ConnProz.AllowNew = True
      ConnProb.AllowNew = True
      ConnFarbe.CurrencyManager.Refresh()

    End If
  End Sub
  Private Sub ConnFarbe_PositionChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnFarbe.PositionChanged
    If ConnFarbe.IsBindingSuspended Then Exit Sub
    If IsNothing(ConnFarbe.Current) OrElse ConnFarbe.Position = -1 Then Exit Sub


  End Sub

  Private Sub ConnFarbe_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.BindingManagerDataErrorEventArgs) Handles ConnFarbe.DataError
    Try

    Catch ex As Exception
      MsgBox(e.Exception)
    End Try
  End Sub
  '
  '
  '
  '
  '
  'Events Preise
  '
  '
  '
  Private Sub ConnPreis_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnPreis.AddingNew
    '
    'Defaultwerte für Preise
    '
    '
    ViewPreis.Table.Columns("MISCH_ID").DefaultValue = ConnFarbe.Current.row("MISCH_ID")
    ViewPreis.Table.Columns("FARBM_ID").DefaultValue = ConnFarbe.Current.row("FARBM_ID")
    ViewPreis.Table.Columns("FARBM_IRPA").DefaultValue = ViewPreis.Count
    ViewPreis.Table.Columns("FARBM_PREIS").DefaultValue = 0.0

  End Sub


  Private Sub ConnPreis_ListChanged(ByVal sender As Object, ByVal e As System.ComponentModel.ListChangedEventArgs) Handles ConnPreis.ListChanged
    Dim i As Integer
    If IsNothing(sender.current) OrElse sender.position = -1 Then Exit Sub
    '
    'Refresh für Preise
    '
    sender.CurrencyManager.Refresh()
    '
    '
    'Nummering der Preise neu ordnen
    '
    '
    For i = 0 To ViewPreis.Count - 1
      ViewPreis(i).Row("FARBM_IRPA") = i
    Next
  End Sub
  '
  '
  Private Sub ConnPreis_PositionChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnPreis.PositionChanged
    If IsNothing(sender.current) OrElse sender.position = -1 Then Exit Sub
    If sender.position = 0 Then
      sender.DataSource.allowdelete = False
    Else
      sender.DataSource.allowdelete = True
    End If
  End Sub
  '
  '
  '
  '
  '
  '
  'Events Prozentigkeiten
  '
  '
  '
  Private Sub ConnProz_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnProz.AddingNew
    '
    'Defaultwerte für Prozentigkeiten
    '
    '
    ViewProz.Table.Columns("MISCH_ID").DefaultValue = ConnFarbe.Current.row("MISCH_ID")
    ViewProz.Table.Columns("FARBM_ID").DefaultValue = ConnFarbe.Current.row("FARBM_ID")
    ViewProz.Table.Columns("FARBM_IRFA").DefaultValue = ViewProz.Count
    ViewProz.Table.Columns("FARBM_PROZ").DefaultValue = 100.0

  End Sub


  Private Sub ConnProz_ListChanged(ByVal sender As Object, ByVal e As System.ComponentModel.ListChangedEventArgs) Handles ConnProz.ListChanged
    Dim i As Integer
    If IsNothing(sender.current) OrElse sender.position = -1 Then Exit Sub
    '
    'Refresh für Prozentigkeiten
    '
    sender.CurrencyManager.Refresh()
    '
    '
    'Nummering der Prozentigkeiten neu ordnen
    '
    '
    If ViewProz.Count = 0 Then Exit Sub
    For i = 0 To ViewProz.Count - 1
      ViewProz(i).Row("FARBM_IRFA") = i
    Next
    '
    'Standardwert muss vorhanden sein
    '
    ViewProz(0).Row("FARBM_PROZ") = 100.0
  End Sub
  '
  Private Sub ConnProz_PositionChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnProz.PositionChanged
    If IsNothing(sender.current) OrElse sender.position = -1 Then Exit Sub
    If sender.position = 0 Then
      sender.DataSource.allowdelete = False
      sender.DataSource.allowedit = False
    Else
      sender.DataSource.allowdelete = True
      sender.DataSource.allowedit = True
    End If
  End Sub
  '
  '
  '
  '
  '
  'Events Bindemittelprozentigkeiten
  '
  '
  '
  Private Sub ConnProb_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnProb.AddingNew
    '
    'Defaultwerte für Prozentigkeiten
    '
    '
    ViewProb.Table.Columns("MISCH_ID").DefaultValue = ConnFarbe.Current.row("MISCH_ID")
    ViewProb.Table.Columns("FARBM_ID").DefaultValue = ConnFarbe.Current.row("FARBM_ID")
    ViewProb.Table.Columns("FARBM_IRBA").DefaultValue = ViewProb.Count
    ViewProb.Table.Columns("FARBM_PROB").DefaultValue = 100.0

  End Sub


  Private Sub ConnProb_ListChanged(ByVal sender As Object, ByVal e As System.ComponentModel.ListChangedEventArgs) Handles ConnProb.ListChanged
    Dim i As Integer
    If IsNothing(sender.current) OrElse sender.position = -1 Then Exit Sub
    '
    'Refresh für Prozentigkeiten
    '
    sender.CurrencyManager.Refresh()
    '
    '
    'Nummering der Prozentigkeiten neu ordnen
    '
    '
    If ViewProb.Count = 0 Then Exit Sub
    For i = 0 To ViewProb.Count - 1
      ViewProb(i).Row("FARBM_IRBA") = i
    Next
    '
    'Standardwert muss vorhanden sein
    '
    ViewProb(0).Row("FARBM_PROB") = 100.0
  End Sub
  '
  '
  Private Sub ConnProb_PositionChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnProb.PositionChanged
    If IsNothing(sender.current) OrElse sender.position = -1 Then Exit Sub
    If sender.position = 0 Then
      sender.DataSource.allowdelete = False
      sender.DataSource.allowedit = False
    Else
      sender.DataSource.allowdelete = True
      sender.DataSource.allowedit = True
    End If
  End Sub

  '
  '
  'Datenfehler-Behandlung
  '
  '
  Private Sub DataGridViewFarbe_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs)
    MsgBox(e.Exception.Message)
    e.Cancel = False
  End Sub


  Private Sub picGridFarbe_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles picGridFarbe.Paint
    ' Try
    e.Graphics.Clear(PicColor)
    'Finally
    'End Try
  End Sub

  Private Sub hscFarbe_ValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles hscFarbe_0.ValueChanged, hscFarbe_1.ValueChanged, hscFarbe_2.ValueChanged, hscFarbe_3.ValueChanged
    If Not sender.enabled Then Exit Sub
    PicColor = Color.FromArgb(hscfarbe(0).Value, hscfarbe(1).Value, hscfarbe(2).Value, hscfarbe(3).Value)
    txtColor.Text = PicColor.ToArgb()
    picGridFarbe.Refresh()
  End Sub



  Private Sub btnQuit_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnQuit.Click
    Call SpeiFarbmittel()
    ' Call FillFarbTable()
  End Sub

  Private Sub BindingNavigatorDeleteFarbe_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles BindingNavigatorDeleteFarbe.Click

    If ConnFarbe.Position = 0 Then
      ConnFarbe.Position = ConnFarbe.Position + 1
      ConnFarbe.Position = ConnFarbe.Position - 1
    Else
      ConnFarbe.Position = ConnFarbe.Position - 1
      ConnFarbe.Position = ConnFarbe.Position + 1
    End If
  End Sub



  Private Sub DataGridViewPreis_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles DataGridViewPreis.DataError
    e.Cancel = False
  End Sub
  Private Sub DataGridViewProz_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles DataGridViewProz.DataError
    e.Cancel = False
  End Sub

  Private Sub DataGridViewProb_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles DataGridViewProb.DataError
    e.Cancel = False
  End Sub

  Private Sub txtFAR_01_TextChanged(sender As Object, e As System.EventArgs) Handles txtFAR_01.TextChanged
    sender.backcolor = txtFAR_00.BackColor
  End Sub
  Sub Afterfirst(Bool As Boolean)
    DataGridViewPreis.Enabled = Bool
    DataGridViewPreis.Enabled = Bool
    DataGridViewProz.Enabled = Bool
    DataGridViewProb.Enabled = Bool
    BindingNavigatorPreis.Enabled = Bool
    BindingNavigatorProz.Enabled = Bool
    BindingNavigatorProb.Enabled = Bool
  End Sub
  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtFAR_05.Validating, txtFAR_06.Validating, txtFAR_07.Validating, txtFAR_08.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub
End Class