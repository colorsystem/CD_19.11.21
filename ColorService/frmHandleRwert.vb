Option Strict Off
Option Explicit On
Option Compare Text
Imports System.IO

Public Class frmHandleRwert
  Inherits System.Windows.Forms.Form

#Region " Vom Windows Form Designer generierter Code "

  Public Sub New()
    MyBase.New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()
    LayoutChanged = False
    DontRead = False


    ' Initialisierungen nach dem Aufruf InitializeComponent() hinzufügen

  End Sub

  ' Die Form überschreibt den Löschvorgang der Basisklasse, um Komponenten zu bereinigen.
  Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
    If disposing Then
      If Not (components Is Nothing) Then
        components.Dispose()
      End If
    End If
    MyBase.Dispose(disposing)

  End Sub

  ' Für Windows Form-Designer erforderlich
  Private components As System.ComponentModel.IContainer




  Friend WithEvents chkART As System.Windows.Forms.CheckBox
  Friend WithEvents chkREF As System.Windows.Forms.CheckBox
  Friend WithEvents cboGRP As System.Windows.Forms.ComboBox
  Friend WithEvents cmdALL As System.Windows.Forms.Button
  Friend WithEvents txtBAN As System.Windows.Forms.TextBox
  Friend WithEvents txtBEM As System.Windows.Forms.TextBox
  Friend WithEvents chkBLD As System.Windows.Forms.CheckBox
  Friend WithEvents txtMES As System.Windows.Forms.TextBox
  Friend WithEvents chkHEU As System.Windows.Forms.CheckBox
  Friend WithEvents chkARC As System.Windows.Forms.CheckBox
  Friend WithEvents txtBIS As System.Windows.Forms.TextBox
  Friend WithEvents txtVON As System.Windows.Forms.TextBox
  Friend WithEvents txtSQL As System.Windows.Forms.TextBox
  Friend WithEvents cmdEND As System.Windows.Forms.Button
  Friend WithEvents cmdLES As System.Windows.Forms.Button
  Friend WithEvents lblMES As System.Windows.Forms.Label
  Friend WithEvents lblSQL As System.Windows.Forms.Label
  Friend WithEvents lblGRP As System.Windows.Forms.Label
  Friend WithEvents lblBAN As System.Windows.Forms.Label
  Friend WithEvents lblBEM As System.Windows.Forms.Label
  Friend WithEvents lblBIS As System.Windows.Forms.Label
  Friend WithEvents lblVON As System.Windows.Forms.Label
  Friend WithEvents lblDAT As System.Windows.Forms.Label
  'Hinweis: Die folgende Prozedur wird vom Windows Form-Designer benötigt.
  'Das Verändern mit dem Windows Form-Designer ist nicht möglich.
  'Das Verändern mit dem Code-Editor ist nicht möglich.
  Friend WithEvents cmdMes As System.Windows.Forms.Button
  Friend WithEvents cmdMan As System.Windows.Forms.Button
  Friend WithEvents cmdAEND_0 As System.Windows.Forms.Button
  Friend WithEvents cmdAEND_1 As System.Windows.Forms.Button
  Friend WithEvents cmdAEND_2 As System.Windows.Forms.Button
  Friend WithEvents cboREFTRA As System.Windows.Forms.ComboBox
  Friend WithEvents chkDAT As System.Windows.Forms.CheckBox
  Friend WithEvents dbgREF As System.Windows.Forms.DataGridView
  Friend WithEvents lblKEN As System.Windows.Forms.Label
  Friend WithEvents lblMessg As System.Windows.Forms.Label
  Friend WithEvents picREF As System.Windows.Forms.PictureBox
  <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
    Me.cboGRP = New System.Windows.Forms.ComboBox()
    Me.chkART = New System.Windows.Forms.CheckBox()
    Me.chkREF = New System.Windows.Forms.CheckBox()
    Me.cmdALL = New System.Windows.Forms.Button()
    Me.txtBAN = New System.Windows.Forms.TextBox()
    Me.txtBEM = New System.Windows.Forms.TextBox()
    Me.chkBLD = New System.Windows.Forms.CheckBox()
    Me.txtMES = New System.Windows.Forms.TextBox()
    Me.chkHEU = New System.Windows.Forms.CheckBox()
    Me.chkARC = New System.Windows.Forms.CheckBox()
    Me.txtBIS = New System.Windows.Forms.TextBox()
    Me.txtVON = New System.Windows.Forms.TextBox()
    Me.txtSQL = New System.Windows.Forms.TextBox()
    Me.cmdEND = New System.Windows.Forms.Button()
    Me.cmdLES = New System.Windows.Forms.Button()
    Me.lblMES = New System.Windows.Forms.Label()
    Me.lblSQL = New System.Windows.Forms.Label()
    Me.lblGRP = New System.Windows.Forms.Label()
    Me.lblBAN = New System.Windows.Forms.Label()
    Me.lblBEM = New System.Windows.Forms.Label()
    Me.lblBIS = New System.Windows.Forms.Label()
    Me.lblVON = New System.Windows.Forms.Label()
    Me.lblDAT = New System.Windows.Forms.Label()
    Me.cmdAEND_0 = New System.Windows.Forms.Button()
    Me.cmdAEND_1 = New System.Windows.Forms.Button()
    Me.cmdAEND_2 = New System.Windows.Forms.Button()
    Me.cmdMes = New System.Windows.Forms.Button()
    Me.cmdMan = New System.Windows.Forms.Button()
    Me.cboREFTRA = New System.Windows.Forms.ComboBox()
    Me.chkDAT = New System.Windows.Forms.CheckBox()
    Me.picREF = New System.Windows.Forms.PictureBox()
    Me.dbgREF = New System.Windows.Forms.DataGridView()
    Me.lblKEN = New System.Windows.Forms.Label()
    Me.lblMessg = New System.Windows.Forms.Label()
    CType(Me.picREF, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.dbgREF, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'cboGRP
    '
    Me.cboGRP.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.cboGRP.Cursor = System.Windows.Forms.Cursors.Default
    Me.cboGRP.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboGRP.ForeColor = System.Drawing.SystemColors.WindowText
    Me.cboGRP.Location = New System.Drawing.Point(124, 32)
    Me.cboGRP.Name = "cboGRP"
    Me.cboGRP.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.cboGRP.Size = New System.Drawing.Size(324, 21)
    Me.cboGRP.TabIndex = 21
    '
    'chkART
    '
    Me.chkART.BackColor = System.Drawing.SystemColors.Control
    Me.chkART.Checked = True
    Me.chkART.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkART.Cursor = System.Windows.Forms.Cursors.Default
    Me.chkART.ForeColor = System.Drawing.SystemColors.ControlText
    Me.chkART.Location = New System.Drawing.Point(4, 288)
    Me.chkART.Name = "chkART"
    Me.chkART.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.chkART.Size = New System.Drawing.Size(116, 17)
    Me.chkART.TabIndex = 26
    Me.chkART.Text = "387"
    Me.chkART.UseVisualStyleBackColor = False
    '
    'chkREF
    '
    Me.chkREF.BackColor = System.Drawing.SystemColors.Control
    Me.chkREF.Cursor = System.Windows.Forms.Cursors.Default
    Me.chkREF.ForeColor = System.Drawing.SystemColors.ControlText
    Me.chkREF.Location = New System.Drawing.Point(400, 56)
    Me.chkREF.Name = "chkREF"
    Me.chkREF.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.chkREF.Size = New System.Drawing.Size(45, 17)
    Me.chkREF.TabIndex = 24
    Me.chkREF.Text = "388"
    Me.chkREF.UseVisualStyleBackColor = False
    '
    'cmdALL
    '
    Me.cmdALL.BackColor = System.Drawing.SystemColors.Control
    Me.cmdALL.Cursor = System.Windows.Forms.Cursors.Default
    Me.cmdALL.ForeColor = System.Drawing.SystemColors.ControlText
    Me.cmdALL.Location = New System.Drawing.Point(4, 312)
    Me.cmdALL.Name = "cmdALL"
    Me.cmdALL.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.cmdALL.Size = New System.Drawing.Size(116, 24)
    Me.cmdALL.TabIndex = 19
    Me.cmdALL.Text = "385"
    Me.cmdALL.UseVisualStyleBackColor = False
    '
    'txtBAN
    '
    Me.txtBAN.AcceptsReturn = True
    Me.txtBAN.BackColor = System.Drawing.SystemColors.Window
    Me.txtBAN.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtBAN.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtBAN.Location = New System.Drawing.Point(124, 128)
    Me.txtBAN.MaxLength = 0
    Me.txtBAN.Name = "txtBAN"
    Me.txtBAN.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.txtBAN.Size = New System.Drawing.Size(356, 20)
    Me.txtBAN.TabIndex = 18
    '
    'txtBEM
    '
    Me.txtBEM.AcceptsReturn = True
    Me.txtBEM.BackColor = System.Drawing.SystemColors.Window
    Me.txtBEM.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtBEM.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtBEM.Location = New System.Drawing.Point(124, 104)
    Me.txtBEM.MaxLength = 0
    Me.txtBEM.Name = "txtBEM"
    Me.txtBEM.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.txtBEM.Size = New System.Drawing.Size(356, 20)
    Me.txtBEM.TabIndex = 15
    '
    'chkBLD
    '
    Me.chkBLD.BackColor = System.Drawing.Color.White
    Me.chkBLD.Checked = True
    Me.chkBLD.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkBLD.Cursor = System.Windows.Forms.Cursors.Default
    Me.chkBLD.Enabled = False
    Me.chkBLD.ForeColor = System.Drawing.SystemColors.ControlText
    Me.chkBLD.Location = New System.Drawing.Point(4, 344)
    Me.chkBLD.Name = "chkBLD"
    Me.chkBLD.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.chkBLD.Size = New System.Drawing.Size(116, 17)
    Me.chkBLD.TabIndex = 14
    Me.chkBLD.Text = "382"
    Me.chkBLD.UseVisualStyleBackColor = False
    '
    'txtMES
    '
    Me.txtMES.AcceptsReturn = True
    Me.txtMES.BackColor = System.Drawing.SystemColors.Window
    Me.txtMES.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtMES.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtMES.Location = New System.Drawing.Point(124, 80)
    Me.txtMES.MaxLength = 0
    Me.txtMES.Name = "txtMES"
    Me.txtMES.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.txtMES.Size = New System.Drawing.Size(356, 20)
    Me.txtMES.TabIndex = 1
    '
    'chkHEU
    '
    Me.chkHEU.BackColor = System.Drawing.SystemColors.Window
    Me.chkHEU.Checked = True
    Me.chkHEU.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkHEU.Cursor = System.Windows.Forms.Cursors.Default
    Me.chkHEU.ForeColor = System.Drawing.SystemColors.WindowText
    Me.chkHEU.Location = New System.Drawing.Point(312, 7)
    Me.chkHEU.Name = "chkHEU"
    Me.chkHEU.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.chkHEU.Size = New System.Drawing.Size(73, 18)
    Me.chkHEU.TabIndex = 2
    Me.chkHEU.Text = "378"
    Me.chkHEU.UseVisualStyleBackColor = False
    Me.chkHEU.Visible = False
    '
    'chkARC
    '
    Me.chkARC.BackColor = System.Drawing.SystemColors.Window
    Me.chkARC.Cursor = System.Windows.Forms.Cursors.Default
    Me.chkARC.ForeColor = System.Drawing.SystemColors.WindowText
    Me.chkARC.Location = New System.Drawing.Point(4, 32)
    Me.chkARC.Name = "chkARC"
    Me.chkARC.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.chkARC.Size = New System.Drawing.Size(64, 17)
    Me.chkARC.TabIndex = 11
    Me.chkARC.Text = "374"
    Me.chkARC.UseVisualStyleBackColor = False
    '
    'txtBIS
    '
    Me.txtBIS.AcceptsReturn = True
    Me.txtBIS.BackColor = System.Drawing.SystemColors.Window
    Me.txtBIS.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
    Me.txtBIS.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtBIS.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtBIS.Location = New System.Drawing.Point(246, 6)
    Me.txtBIS.MaxLength = 0
    Me.txtBIS.Name = "txtBIS"
    Me.txtBIS.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.txtBIS.Size = New System.Drawing.Size(60, 20)
    Me.txtBIS.TabIndex = 9
    Me.txtBIS.Text = "31.12.1999"
    Me.txtBIS.Visible = False
    '
    'txtVON
    '
    Me.txtVON.AcceptsReturn = True
    Me.txtVON.BackColor = System.Drawing.SystemColors.Window
    Me.txtVON.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
    Me.txtVON.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtVON.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtVON.Location = New System.Drawing.Point(147, 8)
    Me.txtVON.MaxLength = 0
    Me.txtVON.Name = "txtVON"
    Me.txtVON.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.txtVON.Size = New System.Drawing.Size(63, 20)
    Me.txtVON.TabIndex = 8
    Me.txtVON.Text = "01.01.1995"
    Me.txtVON.Visible = False
    '
    'txtSQL
    '
    Me.txtSQL.AcceptsReturn = True
    Me.txtSQL.BackColor = System.Drawing.SystemColors.Window
    Me.txtSQL.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
    Me.txtSQL.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtSQL.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtSQL.Location = New System.Drawing.Point(124, 56)
    Me.txtSQL.MaxLength = 0
    Me.txtSQL.Name = "txtSQL"
    Me.txtSQL.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.txtSQL.Size = New System.Drawing.Size(272, 20)
    Me.txtSQL.TabIndex = 12
    '
    'cmdEND
    '
    Me.cmdEND.BackColor = System.Drawing.SystemColors.Control
    Me.cmdEND.Cursor = System.Windows.Forms.Cursors.Default
    Me.cmdEND.DialogResult = System.Windows.Forms.DialogResult.Cancel
    Me.cmdEND.ForeColor = System.Drawing.SystemColors.ControlText
    Me.cmdEND.Location = New System.Drawing.Point(4, 236)
    Me.cmdEND.Name = "cmdEND"
    Me.cmdEND.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.cmdEND.Size = New System.Drawing.Size(116, 24)
    Me.cmdEND.TabIndex = 4
    Me.cmdEND.Text = "371"
    Me.cmdEND.UseVisualStyleBackColor = False
    '
    'cmdLES
    '
    Me.cmdLES.BackColor = System.Drawing.SystemColors.Control
    Me.cmdLES.Cursor = System.Windows.Forms.Cursors.Default
    Me.cmdLES.ForeColor = System.Drawing.SystemColors.ControlText
    Me.cmdLES.Location = New System.Drawing.Point(4, 208)
    Me.cmdLES.Name = "cmdLES"
    Me.cmdLES.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.cmdLES.Size = New System.Drawing.Size(116, 24)
    Me.cmdLES.TabIndex = 3
    Me.cmdLES.Text = "372"
    Me.cmdLES.UseVisualStyleBackColor = False
    '
    'lblMES
    '
    Me.lblMES.BackColor = System.Drawing.SystemColors.Control
    Me.lblMES.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblMES.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblMES.Location = New System.Drawing.Point(4, 80)
    Me.lblMES.Name = "lblMES"
    Me.lblMES.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblMES.Size = New System.Drawing.Size(116, 17)
    Me.lblMES.TabIndex = 23
    Me.lblMES.Text = "370"
    Me.lblMES.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblSQL
    '
    Me.lblSQL.BackColor = System.Drawing.SystemColors.Control
    Me.lblSQL.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblSQL.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblSQL.Location = New System.Drawing.Point(4, 56)
    Me.lblSQL.Name = "lblSQL"
    Me.lblSQL.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblSQL.Size = New System.Drawing.Size(116, 17)
    Me.lblSQL.TabIndex = 22
    Me.lblSQL.Text = "369"
    Me.lblSQL.TextAlign = System.Drawing.ContentAlignment.TopRight
    '
    'lblGRP
    '
    Me.lblGRP.BackColor = System.Drawing.SystemColors.Control
    Me.lblGRP.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblGRP.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblGRP.Location = New System.Drawing.Point(64, 32)
    Me.lblGRP.Name = "lblGRP"
    Me.lblGRP.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblGRP.Size = New System.Drawing.Size(56, 17)
    Me.lblGRP.TabIndex = 20
    Me.lblGRP.Text = "386"
    Me.lblGRP.TextAlign = System.Drawing.ContentAlignment.TopRight
    '
    'lblBAN
    '
    Me.lblBAN.BackColor = System.Drawing.SystemColors.Control
    Me.lblBAN.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblBAN.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblBAN.Location = New System.Drawing.Point(4, 128)
    Me.lblBAN.Name = "lblBAN"
    Me.lblBAN.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblBAN.Size = New System.Drawing.Size(116, 17)
    Me.lblBAN.TabIndex = 17
    Me.lblBAN.Text = "918"
    Me.lblBAN.TextAlign = System.Drawing.ContentAlignment.TopRight
    '
    'lblBEM
    '
    Me.lblBEM.BackColor = System.Drawing.SystemColors.Control
    Me.lblBEM.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblBEM.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblBEM.Location = New System.Drawing.Point(4, 104)
    Me.lblBEM.Name = "lblBEM"
    Me.lblBEM.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblBEM.Size = New System.Drawing.Size(116, 17)
    Me.lblBEM.TabIndex = 16
    Me.lblBEM.Text = "916"
    Me.lblBEM.TextAlign = System.Drawing.ContentAlignment.TopRight
    '
    'lblBIS
    '
    Me.lblBIS.BackColor = System.Drawing.SystemColors.Window
    Me.lblBIS.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblBIS.ForeColor = System.Drawing.SystemColors.WindowText
    Me.lblBIS.Location = New System.Drawing.Point(216, 8)
    Me.lblBIS.Name = "lblBIS"
    Me.lblBIS.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblBIS.Size = New System.Drawing.Size(24, 17)
    Me.lblBIS.TabIndex = 7
    Me.lblBIS.Text = "377"
    Me.lblBIS.Visible = False
    '
    'lblVON
    '
    Me.lblVON.BackColor = System.Drawing.SystemColors.Window
    Me.lblVON.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblVON.ForeColor = System.Drawing.SystemColors.WindowText
    Me.lblVON.Location = New System.Drawing.Point(115, 8)
    Me.lblVON.Name = "lblVON"
    Me.lblVON.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblVON.Size = New System.Drawing.Size(32, 17)
    Me.lblVON.TabIndex = 6
    Me.lblVON.Text = "376"
    Me.lblVON.Visible = False
    '
    'lblDAT
    '
    Me.lblDAT.BackColor = System.Drawing.SystemColors.Window
    Me.lblDAT.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblDAT.ForeColor = System.Drawing.SystemColors.WindowText
    Me.lblDAT.Location = New System.Drawing.Point(65, 7)
    Me.lblDAT.Name = "lblDAT"
    Me.lblDAT.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblDAT.Size = New System.Drawing.Size(44, 17)
    Me.lblDAT.TabIndex = 5
    Me.lblDAT.Text = "375"
    Me.lblDAT.Visible = False
    '
    'cmdAEND_0
    '
    Me.cmdAEND_0.BackColor = System.Drawing.SystemColors.Control
    Me.cmdAEND_0.Location = New System.Drawing.Point(4, 368)
    Me.cmdAEND_0.Name = "cmdAEND_0"
    Me.cmdAEND_0.Size = New System.Drawing.Size(116, 24)
    Me.cmdAEND_0.TabIndex = 34
    Me.cmdAEND_0.Text = "381"
    Me.cmdAEND_0.UseVisualStyleBackColor = False
    Me.cmdAEND_0.Visible = False
    '
    'cmdAEND_1
    '
    Me.cmdAEND_1.BackColor = System.Drawing.SystemColors.Control
    Me.cmdAEND_1.Location = New System.Drawing.Point(4, 400)
    Me.cmdAEND_1.Name = "cmdAEND_1"
    Me.cmdAEND_1.Size = New System.Drawing.Size(116, 24)
    Me.cmdAEND_1.TabIndex = 35
    Me.cmdAEND_1.Text = "389"
    Me.cmdAEND_1.UseVisualStyleBackColor = False
    Me.cmdAEND_1.Visible = False
    '
    'cmdAEND_2
    '
    Me.cmdAEND_2.BackColor = System.Drawing.SystemColors.Control
    Me.cmdAEND_2.Location = New System.Drawing.Point(4, 432)
    Me.cmdAEND_2.Name = "cmdAEND_2"
    Me.cmdAEND_2.Size = New System.Drawing.Size(116, 24)
    Me.cmdAEND_2.TabIndex = 36
    Me.cmdAEND_2.Text = "380"
    Me.cmdAEND_2.UseVisualStyleBackColor = False
    Me.cmdAEND_2.Visible = False
    '
    'cmdMes
    '
    Me.cmdMes.BackColor = System.Drawing.SystemColors.Control
    Me.cmdMes.Location = New System.Drawing.Point(4, 152)
    Me.cmdMes.Name = "cmdMes"
    Me.cmdMes.Size = New System.Drawing.Size(116, 24)
    Me.cmdMes.TabIndex = 37
    Me.cmdMes.Text = "373"
    Me.cmdMes.UseVisualStyleBackColor = False
    '
    'cmdMan
    '
    Me.cmdMan.BackColor = System.Drawing.SystemColors.Control
    Me.cmdMan.Location = New System.Drawing.Point(4, 180)
    Me.cmdMan.Name = "cmdMan"
    Me.cmdMan.Size = New System.Drawing.Size(116, 24)
    Me.cmdMan.TabIndex = 38
    Me.cmdMan.Text = "398"
    Me.cmdMan.UseVisualStyleBackColor = False
    '
    'cboREFTRA
    '
    Me.cboREFTRA.Location = New System.Drawing.Point(4, 264)
    Me.cboREFTRA.Name = "cboREFTRA"
    Me.cboREFTRA.Size = New System.Drawing.Size(116, 21)
    Me.cboREFTRA.TabIndex = 40
    Me.cboREFTRA.Text = "997"
    '
    'chkDAT
    '
    Me.chkDAT.Location = New System.Drawing.Point(400, 7)
    Me.chkDAT.Name = "chkDAT"
    Me.chkDAT.Size = New System.Drawing.Size(80, 16)
    Me.chkDAT.TabIndex = 41
    Me.chkDAT.Text = "140"
    '
    'picREF
    '
    Me.picREF.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
    Me.picREF.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
    Me.picREF.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
    Me.picREF.Cursor = System.Windows.Forms.Cursors.Default
    Me.picREF.ForeColor = System.Drawing.SystemColors.ControlText
    Me.picREF.Location = New System.Drawing.Point(124, 152)
    Me.picREF.Name = "picREF"
    Me.picREF.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.picREF.Size = New System.Drawing.Size(356, 400)
    Me.picREF.TabIndex = 13
    Me.picREF.TabStop = False
    Me.picREF.Visible = False
    '
    'dbgREF
    '
    Me.dbgREF.AllowUserToAddRows = False
    Me.dbgREF.AllowUserToDeleteRows = False
    Me.dbgREF.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
    Me.dbgREF.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgREF.Location = New System.Drawing.Point(126, 154)
    Me.dbgREF.Name = "dbgREF"
    Me.dbgREF.RowTemplate.Height = 24
    Me.dbgREF.Size = New System.Drawing.Size(354, 398)
    Me.dbgREF.TabIndex = 42
    '
    'lblKEN
    '
    Me.lblKEN.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblKEN.Location = New System.Drawing.Point(4, 6)
    Me.lblKEN.Name = "lblKEN"
    Me.lblKEN.Size = New System.Drawing.Size(26, 17)
    Me.lblKEN.TabIndex = 43
    Me.lblKEN.Text = "  "
    '
    'lblMessg
    '
    Me.lblMessg.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMessg.Location = New System.Drawing.Point(36, 6)
    Me.lblMessg.Name = "lblMessg"
    Me.lblMessg.Size = New System.Drawing.Size(26, 17)
    Me.lblMessg.TabIndex = 44
    Me.lblMessg.Text = "-1"
    '
    'frmHandleRwert
    '
    Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
    Me.BackColor = System.Drawing.SystemColors.Window
    Me.ClientSize = New System.Drawing.Size(481, 552)
    Me.ControlBox = False
    Me.Controls.Add(Me.lblMessg)
    Me.Controls.Add(Me.lblKEN)
    Me.Controls.Add(Me.dbgREF)
    Me.Controls.Add(Me.chkDAT)
    Me.Controls.Add(Me.cboREFTRA)
    Me.Controls.Add(Me.cmdMan)
    Me.Controls.Add(Me.cmdMes)
    Me.Controls.Add(Me.cmdAEND_2)
    Me.Controls.Add(Me.cmdAEND_1)
    Me.Controls.Add(Me.cmdAEND_0)
    Me.Controls.Add(Me.chkART)
    Me.Controls.Add(Me.chkREF)
    Me.Controls.Add(Me.cboGRP)
    Me.Controls.Add(Me.cmdALL)
    Me.Controls.Add(Me.txtBAN)
    Me.Controls.Add(Me.txtBEM)
    Me.Controls.Add(Me.txtMES)
    Me.Controls.Add(Me.txtBIS)
    Me.Controls.Add(Me.txtVON)
    Me.Controls.Add(Me.txtSQL)
    Me.Controls.Add(Me.chkBLD)
    Me.Controls.Add(Me.picREF)
    Me.Controls.Add(Me.chkHEU)
    Me.Controls.Add(Me.chkARC)
    Me.Controls.Add(Me.cmdEND)
    Me.Controls.Add(Me.cmdLES)
    Me.Controls.Add(Me.lblMES)
    Me.Controls.Add(Me.lblSQL)
    Me.Controls.Add(Me.lblGRP)
    Me.Controls.Add(Me.lblBAN)
    Me.Controls.Add(Me.lblBEM)
    Me.Controls.Add(Me.lblBIS)
    Me.Controls.Add(Me.lblVON)
    Me.Controls.Add(Me.lblDAT)
    Me.ForeColor = System.Drawing.SystemColors.WindowText
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmHandleRwert"
    Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
    Me.Text = "999"
    CType(Me.picREF, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.dbgREF, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)
    Me.PerformLayout()

  End Sub
#End Region

  Dim DontRead As Boolean
  Dim LayoutChanged As Boolean
  Dim RefID As Integer
  Dim DbErr As Short
  Dim DataReader As OleDbDataReader
  Dim DataAdapter As New OleDbDataAdapter()
  Dim DataCommand As New OleDbCommand("", Cncol)
  Dim ReadWrite As New ReadWriteRwert
  Dim MnHandleRwert As HandleRwerte
  Dim Plott As New GraphicHilfProgramme
  Dim MnMessRefel As RefValue
  Dim DsetRwert As New DataSet
  Dim FilSys As FileSystemInfo

  Dim MnReTr As Short '=0 Reflexionn;=1 Transmission
  Dim MnIalle As Boolean

  Dim RefMaxID As Integer 'Maximale ID (Primärschlüssel)
  Dim RefMinID As Integer 'Minimale ID (Primärschlüssel) gemäß Datum
  Dim MessgAltID As Integer = -1
  '
  '
  Dim RefPointF() As PointF
  Dim RefWert() As Single
  '
  '
  '
  '
  '
  Dim Kunamen() As String
  Dim Rread As Boolean
  '
  '
  '

  '
  '
  '
  '
  Private ID As Integer
  Dim LesAen As Short

  'Anzahl Reflexionskurven (+Zusatzinformation) QU-Kontrolle
  '
  Dim ier As Short
  '
  '
  '

  ' Laufvariable
  Dim kw As Short
  ' Zwischengroessen
  Dim nkw As Short
  'Message Kennung
  Dim imsg As Short   'Messagebox (Rückgabewert)
  '
  '
  Dim RefxName As String
  Dim RefxBem As String
  Dim RefxBanum As String
  Dim MnMeasure As MeasureReflex


  Property Messrefel() As RefValue
    Get
      Messrefel = MnMessRefel
    End Get
    Set(ByVal value As RefValue)
      MnMessRefel = value
    End Set
  End Property
  Property Measure() As Object
    Get
      Measure = MnMeasure
    End Get
    Set(ByVal value As Object)
      MnMeasure = value
    End Set
  End Property
  Sub AendLoeKop(ByRef ITrue As CheckState)

    BitWrt(18, MenueParam.User.Writ)

    If dbgREF.Columns.Count = 0 Then Exit Sub
    If ITrue = CheckState.Unchecked Then
      dbgREF.Columns(3).ReadOnly = False
      If BitWrt(18, MenueParam.User.Writ) Then
        dbgREF.Columns(4).ReadOnly = False
      End If
      If BitWrt(17, MenueParam.User.Writ) Then
        dbgREF.Columns(5).ReadOnly = False
      End If
    Else
      dbgREF.Columns(3).ReadOnly = True
      dbgREF.Columns(4).ReadOnly = True
      dbgREF.Columns(5).ReadOnly = True
      cmdAEND_0.Visible = False
      cmdAEND_1.Visible = False
      cmdAEND_2.Visible = False
      Exit Sub
    End If

    '
    '
    'Bit 21: Ändern
    '
    '
    '

    If Not BitWrt(21, MenueParam.User.Writ) Then
      cmdAEND_2.Visible = False
    Else
      cmdAEND_2.Visible = True
    End If
    '
    'Bit 22: Löschen
    '
    '
    '
    If Not BitWrt(22, MenueParam.User.Writ) Then
      cmdAEND_0.Visible = False
    Else
      cmdAEND_0.Visible = True
    End If
    '
    'Bit 23: Kopieren
    '
    '
    '
    If Not BitWrt(23, MenueParam.User.Writ) Then
      cmdAEND_1.Visible = False
    Else
      cmdAEND_1.Visible = True
    End If

  End Sub

  Sub ReflNamen(ByRef Index As Integer, ByRef RefName As String, ByRef RefBem As String, ByRef RefBanum As String)
    Try
      RefName = dbgREF.SelectedRows(Index).Cells("RWERT_NAME").Value
      RefBem = dbgREF.SelectedRows(Index).Cells("RWERT_BEM").Value
      If IsDBNull(dbgREF.SelectedRows(Index).Cells("RWERT_KENN").Value) Then
        RefBanum = ""
      Else
        RefBanum = dbgREF.SelectedRows(Index).Cells("RWERT_KENN").Value
      End If
    Catch
    End Try
  End Sub


  Sub TxtMesActivate()
    Dim i As Integer
    Dim FRM As String
    Dim Lee As Integer
    If chkREF.Checked Then
      txtMES.Text = RefName(ReadWrite.RefMax(MenueParam.TableRwert, MenueParam.Messg.MessgRwID) + 1, (txtMES.Text), MenueParam.Messg.Stell)
    Else
      Lee = txtMES.Text.Count
      FRM = ""
      If Lee > 0 Then
        For i = Lee - 1 To 0 Step -1
          If Not IsNumeric(txtMES.Text.Substring(i, 1)) Then
            Exit For
          End If
          FRM = FRM & "0"
        Next
        If i < Lee - 1 Then
          txtMES.Text = txtMES.Text.Substring(0, i + 1) & Format(CInt(txtMES.Text.Substring(i + 1, Lee - i - 1)) + 1, FRM)
        End If
      End If
      End If
      If txtMES.Enabled And txtMES.Visible Then
        txtMES.Focus()
        If chkREF.Checked Then
          txtMES.SelectAll()
          txtMES.SelectionStart = MenueParam.Messg.Stell
        Else
          txtMES.SelectAll()
          txtMES.SelectionStart = txtMES.Text.Length
        End If
      End If
      If txtMES.Text = "" Or txtMES.Text = "xxxx" Then
        txtMES.Text = "xxxx"
        txtMES.Focus()
        txtMES.SelectAll()
        txtMES.SelectionStart = 0
      End If
  End Sub

  Private Sub cboRefTra_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cboREFTRA.SelectedIndexChanged
    If cboREFTRA.SelectedIndex = -1 Then Exit Sub
    If Not IsNumeric(cboGRP.SelectedValue) Then Exit Sub
    MnReTr = cboREFTRA.SelectedIndex
    MenueParam.Messg.ReTr = MnReTr
    picREF.Visible = False
    If chkART.Checked Then
      Call Liste(1)
    End If



  End Sub




  Private Sub chkARC_KeyPress(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.KeyPressEventArgs) Handles chkARC.KeyPress
    Dim KeyAscii As Short = Asc(eventArgs.KeyChar)
    If KeyAscii = 13 Then
      chkARC.CheckState = System.Windows.Forms.CheckState.Indeterminate
    End If
    If KeyAscii = 0 Then
      eventArgs.Handled = True
    End If
  End Sub


  Private Sub chkART_CheckStateChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles chkART.CheckStateChanged
    Call AendLoeKop(chkART.CheckState)
  End Sub

  Private Sub chkBLD_CheckStateChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles chkBLD.CheckStateChanged
    If chkBLD.CheckState = CheckState.Unchecked Then
      picREF.Visible = False
      If Rread Then
        dbgREF.Visible = True
      End If
    End If
  End Sub

  Private Sub chkHEU_CheckStateChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles chkHEU.CheckStateChanged
    If IsNothing(MenueParam) Then Exit Sub
    txtBIS.Text = FormatDateTime(Today, DateFormat.ShortDate)
    txtVON.Text = FormatDateTime(Date.Today.Subtract(New TimeSpan(MenueParam.Messg.Tdiff, 0, 0, 0)), DateFormat.ShortDate)

    If Not IsDate(txtVON.Text) Then
      MsgBox(Texxt(2500))
      Exit Sub
    End If

  End Sub




  Private Sub cmdaend_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdAEND_0.Click, cmdAEND_1.Click, cmdAEND_2.Click
    Dim LokalRwert As New RefValue
    Dim Index As Short
    Dim MaxID As Integer
    Index = CInt(eventSender.name.ToString.Substring(8, 1))
    Dim i As Short
    Dim ier As Integer
    Dim Iloe As Short
    Select Case Index
      Case 0
        '
        If Not AddModDelP(3006) Then Exit Sub
        If Not MnHandleRwert.MeldSpeiAllRwrt(False) Then Exit Sub
        '
        'Löschen (Iarch=2 setzen)
        '
        '
        Iloe = 0




        For i = 0 To dbgREF.SelectedRows.Count - 1
          If dbgREF.SelectedRows(i).Cells("RWERT_IARCH").Value = 1 Then
            MsgBox(Texxt(2919) & Chr(13) & dbgREF.SelectedRows(i).Cells("RWERT_NAME").Value)
          Else
            Iloe = Iloe + 1
            ReflNamen(i, RefxName, RefxBem, RefxBanum)
            Call ReadWrite.Update(dbgREF.SelectedRows(i).Cells("RWERT_ID").Value, cboGRP.SelectedValue, 2, RefxName, RefxBem, RefxBanum, CStr(lblMessg.Text), CStr(lblKEN.Text), ier)
          End If
        Next i
        If ier = 0 Then
          MsgBox(CStr(Iloe) & Space(1) & Texxt(3036))
        End If
      Case 1
        '
        If Not AddModDelP(3007) Then Exit Sub
        If Not MnHandleRwert.MeldSpeiAllRwrt(False) Then Exit Sub

        '
        '
        'Kopieren
        '
        '
        '
        For i = 0 To dbgREF.SelectedRows.Count - 1
          Call ReadWrite.ReadRwert(dbgREF.SelectedRows(i).Cells("RWERT_ID").Value, LokalRwert, ier)
          MaxID = ReadWrite.RefMax(MenueParam.TableRwert, MenueParam.Messg.MessgRwID) + 1
          ReflNamen(i, RefxName, RefxBem, RefxBanum)
          LokalRwert.Gid = cboGRP.SelectedValue
          LokalRwert.Iarch = chkARC.CheckState
          LokalRwert.Name = RefxName
          LokalRwert.Bem = RefxBem
          LokalRwert.Banum = RefxBanum
          Call ReadWrite.WriteRwert(MaxID, LokalRwert, ier)
        Next i
        If ier = 0 Then
          MsgBox(CStr(dbgREF.SelectedRows.Count) & Space(1) & Texxt(3037))
        End If
      Case 2
        '
        If Not AddModDelP(3008) Then Exit Sub
        If Not MnHandleRwert.MeldSpeiAllRwrt(False) Then Exit Sub
        '
        '
        'Ändern
        '
        '
        '
        For i = 0 To dbgREF.SelectedRows.Count - 1
          ReflNamen(i, RefxName, RefxBem, RefxBanum)
          Call ReadWrite.Update(dbgREF.SelectedRows(i).Cells("RWERT_ID").Value, cboGRP.SelectedValue, chkARC.CheckState, RefxName, RefxBem, RefxBanum, CInt(lblMessg.Text), CStr(lblKEN.Text), ier)
        Next i
        If ier = 0 Then
          MsgBox(CStr(dbgREF.SelectedRows.Count) & Space(1) & Texxt(3038))
        End If
    End Select
    LokalRwert.dispose()
  End Sub

  Private Sub cmdALL_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdALL.Click
    Dim i As Short
    '
    '
    '
    '
    'Alle ausgewählten Kurven werden übertragen
    '
    '
    '
    '
    For i = dbgREF.SelectedRows.Count - 1 To 0 Step -1
      RefID = dbgREF.SelectedRows(i).Cells("RWERT_ID").Value
      Call ReadWrite.ReadRwert(RefID, Messrefel, ier)
      txtMES.Text = Messrefel.Name
      txtBEM.Text = Messrefel.Bem
      txtBAN.Text = Messrefel.Banum
      chkARC.CheckState = Messrefel.Iarch
      lblKEN.Text = Messrefel.Cme
      lblMessg.Text = Messrefel.MessgID
      Call MnHandleRwert.RaiGetNameID(Messrefel.ID, Messrefel.Name, Messrefel.Banum)
    Next i
  End Sub

  Private Sub cmdEND_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdEND.Click
    Call MnHandleRwert.InVisible()
  End Sub

  Private Sub cmdLES_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdLES.Click
    Call Liste(1)
  End Sub

  Private Sub cmdMEAS_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdMes.Click, cmdMan.Click
    Dim Index As Short
    If eventSender.name = "CMDMES" Then
      Index = 0
    Else
      Index = 1
    End If
    Dim chki As Boolean
    If chki Then Exit Sub
    chki = True
    picREF.Visible = False
    LesAen = 0
    '
    'Prüfen auf *
    '
    '
    'Messung starten
    '
    picREF.Visible = False


    '
    Messrefel.Iami = 0

    Select Case Index

      Case 0
        'Messen
        If Not MnHandleRwert.MeldSpeiAllRwrt(False) Then
          chki = False
          Exit Sub
        Else
          Call MnMeasure.MessDevMess(MenueParam.Messg, MnMessRefel)
        End If
      Case 1
        'Manuell
        If Not MnHandleRwert.MeldSpeiAllRwrt(False) Then
          chki = False
          Exit Sub
        Else
          Call MnMeasure.MessDevManu(MnMessRefel)
        End If
    End Select

    '
    '
    '
    If MnMeasure.ier <> 0 Then
      chki = False
      If Not MnIalle Then
        Me.DialogResult = Windows.Forms.DialogResult.Abort
      End If
      Exit Sub
    End If
    If Messrefel.Iami = 0 Then
      chki = False
      If Not MnIalle Then
        Me.DialogResult = Windows.Forms.DialogResult.Abort
      End If
      Exit Sub
    End If
    If Messrefel.Name = "" Then
      Messrefel.Name = Mid(txtMES.Text, 1, 40)
    End If
    If Trim(Messrefel.Name) = "" Then
      MsgBox(Texxt(2609))
      chki = False
      Messrefel.Iami = 0
      Exit Sub
    End If
    If Messrefel.Bem = "" Then
      Messrefel.Bem = Mid(txtBEM.Text, 1, 40)
    End If
    If Messrefel.Banum = "" Then
      Messrefel.Banum = Mid(txtBAN.Text, 1, 20)
    End If
    If Messrefel.Cme = "" Then
      Messrefel.Cme = lblKEN.Text
    End If
    Messrefel.Gid = cboGRP.SelectedValue
    Messrefel.Iarch = chkARC.CheckState
    Messrefel.DatTim = Date.Now
    Messrefel.MessgID = MenueParam.Messg.MessgID
    ReadWrite.WriteRwert(ID, MnMessRefel, ier)
    '
    '
    '

    If chkBLD.CheckState Then
      dbgREF.Visible = False
      picREF.Visible = True
      picREF.Refresh()
    End If
    chkBLD.Enabled = True
    If chkART.CheckState = 1 Then
      If ier = 0 Then
        MnMessRefel.IVoNa = True
      End If
      Call MnHandleRwert.RaiGetNameID(MnMessRefel.ID, MnMessRefel.Name, MnMessRefel.Banum)
      If ier <> 0 Then
        chki = False
        If Not MnIalle Then
          Me.DialogResult = Windows.Forms.DialogResult.Abort
        End If
        Exit Sub
      End If
      If Not MnIalle Then
        Me.DialogResult = Windows.Forms.DialogResult.OK
      End If
    End If
    'Call TxtMesActivate()
    chki = False


  End Sub









  Sub ReflPaint(ByVal graph As Graphics)
    Dim ier As Short
    Dim kw As Short
    Dim Kunamen() As String
    Dim Kurvs As CurvesRefGrp
    If MnMessRefel.RefKurv.Count = 0 Then Exit Sub
    Kurvs = New CurvesRefGrp
    Kurvs.clear()
    For kw = 0 To MenueParam.User.Winkel.Km - 1
      Kurvs.Add(KeyName(kw), New CurvesRef)
    Next kw
    ReDim Kunamen(MenueParam.User.Winkel.Km - 1)
    For kw = 0 To MenueParam.User.Winkel.Km - 1
      Kunamen(kw) = MenueParam.User.Winkel(kw).Chrm
      Kurvs(kw).Add(Kunamen(kw), MnMessRefel.RefKurv(Kunamen(kw)))
    Next kw
    Plott.GraphBounds = New RectangleF(0.0, 0.0, picREF.Width, picREF.Height)

    Call Plott.RefPaant(MnMessRefel.Name, Kunamen, -1.0#, -1.0#, MenueParam.User.Winkel, Kurvs, 100.0#, 0, 0, graph, RefPointF, RefWert, ier)


    Kurvs = Nothing
    Erase Kunamen

  End Sub






  Private Sub frmHandleRwert_Activated(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Activated
    Dim i As Integer

    DontRead = False
    Call TxtMesActivate()

  End Sub
  Private Sub frmHandleRwert_Deactivate(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Deactivate
    DontRead = True
  End Sub


  Private Sub frmHandleRwert_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
    Dim i As Integer
    Me.Location = New Point(Screen.PrimaryScreen.WorkingArea.Width - Me.Size.Width, 0)
    '
    If MessgAltID <> MenueParam.MessgID Then
      If Not IsNothing(dbgREF.DataSource) Then
        dbgREF.DataSource.clear()
      End If
      MessgAltID = MenueParam.MessgID
    End If

    'chkDAT
    '

    Me.chkDAT.Text = Texxt(140)
    '

    'chkART
    '

    Me.chkART.Text = Texxt(387)
    '
    'chkREF
    '

    Me.chkREF.Text = Texxt(388)
    '
    'cmdALL
    '

    Me.cmdALL.Text = Texxt(385)
    '

    '
    'chkBLD
    '

    Me.chkBLD.Text = Texxt(382)
    '
    '
    '
    'chkHEU
    '
    Me.chkHEU.Text = Texxt(378)
    '
    'chkARC
    '
    Me.chkARC.Text = Texxt(374)
    '
    '
    '
    '
    'cmdEND
    Me.cmdEND.Text = Texxt(371)
    '
    'cmdLES
    '
    Me.cmdLES.Text = Texxt(372)
    '
    'lblMES
    '

    Me.lblMES.Text = Texxt(370)
    '
    'lblSQL
    '
    Me.lblSQL.Text = Texxt(369)
    '
    'lblGRP
    '

    Me.lblGRP.Text = Texxt(386)
    '
    'lblBAN
    '

    Me.lblBAN.Text = Texxt(918)
    '
    'lblBEM
    '
    Me.lblBEM.Text = Texxt(916)
    '
    'lblBIS
    '
    Me.lblBIS.Text = Texxt(377)
    '
    'lblVON
    '
    Me.lblVON.Text = Texxt(376)
    '
    'lblDAT
    '
    Me.lblDAT.Text = Texxt(375)
    '
    'cmdAEND_0
    '
    Me.cmdAEND_0.Text = Texxt(381)
    '
    'cmdAEND_1
    '
    Me.cmdAEND_1.Text = Texxt(389)
    '
    'cmdAEND_2
    '
    Me.cmdAEND_2.Text = Texxt(380)
    '
    'cmdMes
    '
    Me.cmdMes.Text = Texxt(373)
    '
    'cmdMan
    '
    Me.cmdMan.Text = Texxt(398)
    '
    'dbgREF
    '

    '
    'cboREFTRA
    '
    Me.cboREFTRA.Text = Texxt(997)
    '
    '
    '
    lblKEN.Text = MenueParam.Messg.Kenn
    lblMessg.Text = MenueParam.Messg.MessgID
    '
    '
    '
    DataAdapter.SelectCommand = DataCommand
    '
    '
    '
    dbgREF.Tag = False
    Cursor = System.Windows.Forms.Cursors.WaitCursor
    chkHEU.CheckState = CheckState.Indeterminate

    chkHEU.CheckState = CheckState.Checked
    'Call Aend(Me, Me.ToolTipHdlRwert)
    Rread = False
    '
    If IsNothing(DsetRwert.Tables("TabCboRefTra")) Then
      DsetRwert.Tables.Add("TabCboRefTra")
      DsetRwert.Tables("TabCboRefTra").Columns.Add("ID")
      DsetRwert.Tables("TabCboRefTra").Columns.Add("Name")
      DsetRwert.Tables("TabCboRefTra").Clear()
      DsetRwert.Tables("TabCboRefTra").Rows.Add(DsetRwert.Tables("TabCboRefTra").NewRow)
      DsetRwert.Tables("TabCboRefTra").Rows(0)("ID") = 0
      DsetRwert.Tables("TabCboRefTra").Rows(0)("Name") = Texxt(3910)
      DsetRwert.Tables("TabCboRefTra").Rows.Add(DsetRwert.Tables("TabCboRefTra").NewRow)
      DsetRwert.Tables("TabCboRefTra").Rows(1)("ID") = 1
      DsetRwert.Tables("TabCboRefTra").Rows(1)("Name") = Texxt(3911)

      cboREFTRA.DataSource = DsetRwert.Tables("TabCboRefTra")
      cboREFTRA.DisplayMember = "Name"
      cboREFTRA.ValueMember = "ID"
    End If

    If BitWrt(11, MenueParam.User.Enabl) And MenueParam.Messg.RefTra = "A" And MenueParam.Messg.MeArtLock = -1 Then
      cboREFTRA.Enabled = True
    Else
      cboREFTRA.Enabled = False
    End If
    If BitWrt(11, MenueParam.User.Visbl) Then
      cboREFTRA.Visible = True
    End If
   

    HandleRwert.cboGRP = cboGRP
    HandleRwert.lblGRP = lblGRP
    Call HandleRwert.GRoupList()
    '
    'cboREFTRA.SelectedIndex = -1

    txtVON.Focus()
    '
    '
    '
    'Combobox cboGRP für Gruppen R-Werte
    '
    '
    '
    '
    '




    'DataAdapter.SelectCommand.CommandText = ""
    'DataAdapter.SelectCommand.Parameters.Add(New OleDbParameter("DateVon", OleDbType.Date))
    'DataAdapter.SelectCommand.Parameters.Add(New OleDbParameter("DateBis", OleDbType.Date))'

    '

    dbgREF.Tag = True
    dbgREF.Visible = True
    cboREFTRA.SelectedIndex = MenueParam.Messg.ReTr
    cboGRP.SelectedValue = MenueParam.Messg.RwrtGID

    For i = 0 To cboGRP.Items.Count - 1
      If cboGRP.Items(i)("GROUP_ID") = MenueParam.Messg.RwrtGID Then
        cboGRP.SelectedIndex = i
      End If
    Next

    If cboGRP.SelectedIndex = -1 Then
      cboGRP.SelectedIndex = 0
    End If
    Call Liste(1)
    dbgREF.DataSource = DsetRwert.Tables("TabRwerte")
    If dbgREF.Columns.Count < 7 Then
      Cursor = System.Windows.Forms.Cursors.Default
      Exit Sub
    End If

    If MnIalle Then
      cmdALL.Visible = True
    Else
      cmdALL.Visible = False
    End If


    '
    'Maximale ID für Reflexionswert
    '
    '
    '
    '

    LesAen = 0
    txtSQL.Visible = True
    txtSQL.Enabled = True
    txtMES.Visible = True
    txtBEM.Visible = True
    lblBEM.Visible = True
    txtBAN.Visible = True
    lblBAN.Visible = True
    MnReTr = MenueParam.Messg.ReTr
    Cursor = System.Windows.Forms.Cursors.Default




    FilSys = New DirectoryInfo(GridFileName())
    dbgREF.Columns(1).Width = 0
    dbgREF.Columns(2).Width = 0
    dbgREF.Columns(3).Width = 350
    dbgREF.Columns(4).Width = 300
    dbgREF.Columns(5).Width = 300
    dbgREF.Columns(1).DefaultCellStyle.Format = ""
    dbgREF.Columns(2).DefaultCellStyle.Format = ""
    dbgREF.Columns(0).Visible = False
    dbgREF.Columns(1).Visible = True
    dbgREF.Columns(2).Visible = True
    dbgREF.Columns(3).Visible = True
    dbgREF.Columns(4).Visible = True
    dbgREF.Columns(5).Visible = True
    dbgREF.Columns(6).Visible = False
    dbgREF.Columns(7).Visible = False
    '
    dbgREF.Columns(0).ReadOnly = True
    dbgREF.Columns(1).ReadOnly = True
    dbgREF.Columns(2).ReadOnly = True
    dbgREF.Columns(3).ReadOnly = True
    dbgREF.Columns(4).ReadOnly = True
    dbgREF.Columns(5).ReadOnly = True
    dbgREF.Columns(6).ReadOnly = True
    dbgREF.Columns(7).ReadOnly = True

    dbgREF.Columns(0).HeaderText = Texxt(521)
    dbgREF.Columns(1).HeaderText = "  "
    dbgREF.Columns(2).HeaderText = Texxt(2205)
    dbgREF.Columns(3).HeaderText = Texxt(2206)
    dbgREF.Columns(4).HeaderText = Texxt(2207)
    dbgREF.Columns(5).HeaderText = Texxt(2208)
    dbgREF.Columns(6).HeaderText = Texxt(374)
    dbgREF.Columns(7).HeaderText = "MESSG_ID"
    dbgREF.Columns(0).DataPropertyName = "RWERT_ID"
    dbgREF.Columns(1).DataPropertyName = "RWERT_CME"
    dbgREF.Columns(2).DataPropertyName = "RWERT_DATTIM"
    dbgREF.Columns(3).DataPropertyName = "RWERT_NAME"
    dbgREF.Columns(4).DataPropertyName = "RWERT_BEM"
    dbgREF.Columns(5).DataPropertyName = "RWERT_KENN"
    dbgREF.Columns(6).DataPropertyName = "RWERT_IARCH"
    dbgREF.Columns(7).DataPropertyName = "MESSG_ID"

    dbgREF.FirstDisplayedScrollingColumnIndex = 3



    'Archiv enabled/visible
    '
    '
    '
    If Not BitWrt(1, MenueParam.User.Visbl) Then
      chkARC.Visible = False
    Else
      chkARC.Visible = True
    End If
    If Not BitWrt(1, MenueParam.User.Enabl) Then
      chkARC.Enabled = False
    Else
      chkARC.Enabled = True
    End If
    If Not BitWrt(17, MenueParam.User.Writ) Then
      txtBAN.Visible = False
      lblBAN.Visible = False
    Else
      txtBAN.Visible = True
      lblBAN.Visible = True
    End If
    If Not BitWrt(18, MenueParam.User.Writ) Then
      txtBEM.Visible = False
      lblBEM.Visible = False
    Else
      txtBEM.Visible = True
      lblBEM.Visible = True
    End If
    If Not BitWrt(19, MenueParam.User.Writ) Then
      cmdMan.Visible = False
    Else
      cmdMan.Visible = True
      Call TxtMesActivate()
    End If
    chkART.CheckState = CheckState.Checked
    If Not BitWrt(20, MenueParam.User.Writ) Then
      chkART.Visible = False
    Else
      chkART.Visible = True
    End If
    If Not MenueParam.Messg.Exists Or IsNothing(MnMeasure) Then
      cmdMes.Visible = False
    Else
      cmdMes.Visible = True
      Call TxtMesActivate()
    End If
    picREF.Visible = False
    chkREF.Checked = BitWrt(24, MenueParam.User.Writ)
    chkREF.Enabled = BitWrt(9, MenueParam.User.Enabl)
    chkREF.Visible = BitWrt(9, MenueParam.User.Visbl)
    Call TxtMesActivate()
   
    '
    '
    '
    '
    '
    '
    '
    Cncol.Close()

  End Sub
  '
  '
  '
  '
  '
  '
  '

  Private Sub Liste(ByRef Index As Short)
    Dim Strgid As String
    Dim Ttop As Integer
    Dim SqlStmt As String
    Dim Rmi As Integer

    picREF.Visible = False
    dbgREF.Visible = False
    If Not IsNumeric(cboGRP.SelectedValue) Then Exit Sub
    Try
      RefMaxID = ReadWrite.RefMax(MenueParam.TableRwert, MenueParam.Messg.MessgRwID)
      Ttop = MenueParam.Messg.Top
      If RefMaxID < MenueParam.Messg.Top Then
        Ttop = RefMaxID + 1
      End If
      If Ttop <= 0 Then
        Ttop = 10
      End If
      Rmi = RefMinID
      If Trim(txtSQL.Text) = "" And cboGRP.SelectedValue <= 0 Then
        If Rmi > RefMaxID - Ttop Then
          Rmi = RefMaxID - Ttop
        End If
      End If
      If Rmi <= 0 Then
        Rmi = 0
      End If
      If cboGRP.SelectedIndex < cboGRP.Items.Count And cboGRP.SelectedIndex <> -1 Then
        If cboGRP.SelectedValue <= 0 Then
          Strgid = " RWERT_IARCH<2"
        Else
          Strgid = " RWERT_IARCH<2 AND RWERT_GID=" & cboGRP.SelectedValue
        End If
      Else
        Strgid = " RWERT_IARCH<2 "
      End If
      Strgid = " RWERT_ID>=" & Rmi & " AND " & Strgid
      Strgid = Strgid & " AND RWERT_RETR=" & MnReTr
      If Trim(txtSQL.Text) = "" Then
        SqlStmt = "SELECT TOP " & Ttop & " RWERT_ID,RWERT_CME,RWERT_DATTIM,RWERT_NAME,RWERT_BEM,RWERT_KENN,RWERT_IARCH,MESSG_ID FROM " & MenueParam.TableRwert & " WHERE" & Strgid
      Else
        SqlStmt = "SELECT TOP " & Ttop & " RWERT_ID,RWERT_CME,RWERT_DATTIM,RWERT_NAME,RWERT_BEM,RWERT_KENN,RWERT_IARCH,MESSG_ID FROM " & MenueParam.TableRwert & " WHERE" & Strgid & " AND (" & StrSelct("RWERT_NAME", AddHkomE((txtSQL.Text))) & ")"
      End If
      If chkDAT.CheckState = CheckState.Checked Then
        SqlStmt = SqlStmt & " AND RWERT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON.Text)) & " AND " & Sqldati(Date.Parse(txtBIS.Text).AddDays(1.0))
      End If
      If BitWrt(26, MenueParam.User.Writ) Then
        SqlStmt = SqlStmt & " AND USER_ID=" & MenueParam.UserID
      End If
      If BitWrt(27, MenueParam.User.Writ) Then
        '
        'alle R-Werte der kombinierten Messgeräte
        '
        '
        SqlStmt = SqlStmt & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID
      Else
        '
        'nur R-Werte des speziellen Messgeräts
        '
        SqlStmt = SqlStmt & " AND MESSG_ID=" & MenueParam.Messg.MessgID
      End If
      If Index = 1 Then
        SqlStmt = SqlStmt & " ORDER BY RWERT_DATTIM DESC "
      ElseIf Index = 2 Then
        SqlStmt = SqlStmt & " ORDER BY RWERT_NAME DESC"
      Else
        SqlStmt = SqlStmt & " ORDER BY RWERT_ID DESC"
      End If
      Cursor = System.Windows.Forms.Cursors.WaitCursor
      dbgREF.Text = Texxt(3914 + MnReTr)
      Cncol.Close()
      DataAdapter.SelectCommand.CommandText = SqlStmt
      DataAdapter.SelectCommand.Connection = Cndat()
      If Not FillDatset(DataAdapter, DsetRwert, "TabRwerte") Then Exit Sub
      If IsNothing(DsetRwert.Tables("TabRwerte")) Then Exit Sub
      Cursor = System.Windows.Forms.Cursors.Default
    Catch ex As Exception
      MsgBox(Texxt(4128) & SqlStmt)
      Cncol.Close()
      MsgBox(ex.Message)
    End Try
    If DsetRwert.Tables("TabRwerte").Rows.Count = 0 Then
      MsgBox(Texxt(2959), 0)
      Cncol.Close()
      dbgREF.Visible = False
      Rread = False
      Exit Sub
    Else
      dbgREF.Visible = True
      If dbgREF.Columns.Count > 0 Then
        dbgREF.Columns(0).Visible = False
        dbgREF.FirstDisplayedScrollingColumnIndex = 3
      End If
    End If

    chkBLD.Enabled = True
    If Not Rread Then chkBLD.CheckState = CheckState.Unchecked
    Rread = True
    picREF.Visible = False

  End Sub


  

  Private Sub txtMES_KeyPress(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.KeyPressEventArgs) Handles txtMES.KeyPress
    Dim KeyAscii As Short = Asc(eventArgs.KeyChar)
    If KeyAscii = 13 Then
      If cmdMes.Visible And cmdMes.Enabled Then
        cmdMes.Focus()
      End If
    End If
    If KeyAscii = 0 Then
      eventArgs.Handled = True
    End If
  End Sub


  
  WriteOnly Property Ialle() As Boolean
    Set(ByVal Value As Boolean)
      MnIalle = Value
    End Set
  End Property

  Public WriteOnly Property Captext() As String
    Set(ByVal AcCaptext As String)
      Me.Text = AcCaptext
    End Set
  End Property






  Public Property HandleRwert() As HandleRwerte
    Get
      HandleRwert = MnHandleRwert
    End Get
    Set(ByVal AcHandleRwert As HandleRwerte)
      MnHandleRwert = AcHandleRwert
    End Set
  End Property



  Public ReadOnly Property HideSofort() As Boolean
    Get
      If chkBLD.CheckState = CheckState.Unchecked Then
        HideSofort = True
      Else
        HideSofort = False
      End If
    End Get
  End Property

  Public WriteOnly Property Iarch() As Short
    Set(ByVal AcIarch As Short)
      chkARC.CheckState = AcIarch
    End Set
  End Property

  Public Property Retr() As Short
    Get
      Retr = MnReTr
    End Get
    Set(ByVal Value As Short)
      MnReTr = Value
      If MenueParam.Messg.RefTra = "T" Then
        MnReTr = 1
      ElseIf MenueParam.Messg.RefTra = "R" Then
        MnReTr = 0
      Else
        If MnReTr = -1 Then
          MnReTr = MenueParam.Messg.ReTr
        End If
      End If
      MenueParam.Messg.ReTr = MnReTr
      If cboREFTRA.Items.Count > MnReTr + 1 Then
        cboREFTRA.SelectedIndex = MnReTr
      End If
    End Set

  End Property

  Private Sub picREF_Paint(ByVal sender As System.Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles picREF.Paint
    Call ReflPaint(e.Graphics)
  End Sub

  Private Sub dbgREF_CellClick(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dbgREF.CellClick
    If e.ColumnIndex <> 3 Or e.RowIndex = -1 Then Exit Sub
    If chkART.CheckState = CheckState.Checked Then
      '
      If IsNothing(dbgREF.CurrentRow.Cells("RWERT_ID").Value) OrElse IsDBNull(dbgREF.CurrentRow.Cells("RWERT_ID").Value) Then Exit Sub
      dbgREF.Enabled = False
      ReadWrite.ReadRwert(CInt(dbgREF.CurrentRow.Cells("RWERT_ID").Value), Messrefel, ier)
      If ier <> 0 And MnIalle Then
        Me.DialogResult = Windows.Forms.DialogResult.Abort
      End If
      txtMES.Text = Messrefel.Name
      txtBEM.Text = Messrefel.Bem
      txtBAN.Text = Messrefel.Banum
      chkARC.CheckState = Messrefel.Iarch
      lblKEN.Text = Messrefel.Cme
      lblMessg.Text = Messrefel.MessgID
      '
      '
      '
      'Übernehmen
      '
      '
      If Messrefel.Iami = 0 Then
        Messrefel.De(0) = 0
      End If
      If chkBLD.CheckState Then
        dbgREF.Visible = False
        picREF.Visible = True
        picREF.Refresh()
      End If
      dbgREF.Enabled = True
      dbgREF.Tag = False

      Call MnHandleRwert.RaiGetNameID(Messrefel.ID, Messrefel.Name, Messrefel.Banum)
      If Not MnIalle Then
        Me.DialogResult = Windows.Forms.DialogResult.OK
      End If
      '
      '
    End If
  End Sub

  Private Sub dbgREF_CellEndEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dbgREF.CellEndEdit
    If e.ColumnIndex = 3 Or e.ColumnIndex = 4 Then
      If IsDBNull(dbgREF.CurrentRow.Cells("RWERT_BEM").Value) Then
        dbgREF.CurrentRow.Cells("RWERT_BEM").Value = " "
      End If
      If IsDBNull(dbgREF.CurrentRow.Cells("RWERT_KENN").Value) Then
        dbgREF.CurrentRow.Cells("RWERT_KENN").Value = " "
      End If
      Call ReadWrite.Update(dbgREF.CurrentRow.Cells("RWERT_ID").Value, cboGRP.SelectedValue, chkARC.CheckState, dbgREF.CurrentRow.Cells("RWERT_NAME").Value, dbgREF.CurrentRow.Cells("RWERT_BEM").Value, dbgREF.CurrentRow.Cells("RWERT_KENN").Value, CInt(lblMessg.Text), CStr(lblKEN.Text), ier)
    End If
  End Sub

  Private Sub dbgREF_ColumnHeaderMouseClick(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles dbgREF.ColumnHeaderMouseClick
    dbgREF.Tag = False
    Call Liste(e.ColumnIndex)
  End Sub




  Protected Overrides Sub Finalize()
    If Not IsNothing(MenueParam) Then
      If Not IsNothing(MenueParam.User) Then
        If BitWrt(2, MenueParam.User.Drum) Then
          If LayoutChanged Then
            imsg = MsgBox(Texxt(351) & Chr(13) & Texxt(2949), 4, Texxt(2000))
            If imsg <> 7 Then
            End If
          End If
        End If
        LayoutChanged = False
      End If
    End If
    Try
      If Not IsNothing(Messrefel) Then
        Messrefel.dispose()
      End If
      If Not IsNothing(ReadWrite) Then
        ReadWrite.dispose()
      End If
    Catch
    End Try
    MyBase.Finalize()
  End Sub






  Private Sub chkDAT_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkDAT.Click
    If chkDAT.CheckState = CheckState.Checked Then
      txtVON.Visible = True
      txtBIS.Visible = True
      lblVON.Visible = True
      lblBIS.Visible = True
      chkHEU.Visible = True
      lblDAT.Visible = True
      dbgREF.Columns("RWERT_DATTIM").ReadOnly = False
    Else
      txtVON.Visible = False
      txtBIS.Visible = False
      lblVON.Visible = False
      lblBIS.Visible = False
      chkHEU.Visible = False
      lblDAT.Visible = False
      dbgREF.Columns("RWERT_DATTIM").ReadOnly = True
    End If
  End Sub

  Private Sub chkREF_CheckStateChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles chkREF.CheckStateChanged
    If IsNothing(MenueParam) Then Exit Sub
    txtMES.Text = ""
    Call TxtMesActivate()
  End Sub

  Private Sub cboGRP_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboGRP.SelectedIndexChanged
    If cboGRP.SelectedIndex >= 0 And cboREFTRA.SelectedIndex >= 0 Then
      If IsNumeric(cboGRP.SelectedValue) Then
        MnReTr = cboREFTRA.SelectedIndex
        MenueParam.Messg.ReTr = MnReTr
        MenueParam.Messg.RwrtGID = cboGRP.SelectedValue
      End If
      If chkART.CheckState = CheckState.Checked Then
        Call Liste(1)
      End If
    End If
  End Sub
 

  Private Sub txtDat_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtBIS.Validating, txtVON.Validating
    If Not IsDate(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(2500))
    End If
  End Sub

  Private Sub dbgREF_RowHeaderMouseClick(sender As Object, e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles dbgREF.RowHeaderMouseClick
    lblKEN.Text = dbgREF.CurrentRow.Cells("RWERT_CME").Value
    lblMessg.Text = dbgREF.CurrentRow.Cells("MESSG_ID").Value
  End Sub

  Private Sub txtBAN_Leave(sender As Object, e As System.EventArgs) Handles txtBAN.Leave, txtBEM.Leave
    If Not chkART.Checked AndAlso Not IsNothing(dbgREF.CurrentRow) Then
      Call ReadWrite.Update(dbgREF.CurrentRow.Cells("RWERT_ID").Value, cboGRP.SelectedValue, chkARC.CheckState, dbgREF.CurrentRow.Cells("RWERT_NAME").Value, txtBEM.Text, txtBAN.Text, CInt(lblMessg.Text), CStr(lblKEN.Text), ier)
    End If

  End Sub
End Class