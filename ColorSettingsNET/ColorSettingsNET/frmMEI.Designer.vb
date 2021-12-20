<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmMEI
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
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmMEI))
    Me.BindingMEI = New System.Windows.Forms.BindingNavigator(Me.components)
    Me.BindingNavigatorAddNewItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorDeleteItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMoveFirstItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMovePreviousItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMoveNextItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMoveLastItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorSeparatorMei = New System.Windows.Forms.ToolStripSeparator()
    Me.cboMEI = New System.Windows.Forms.ComboBox()
    Me.txtMEI_01 = New System.Windows.Forms.TextBox()
    Me.txtMEI_00 = New System.Windows.Forms.TextBox()
    Me.txtMEI_02 = New System.Windows.Forms.TextBox()
    Me.txtMEI_03 = New System.Windows.Forms.TextBox()
    Me.txtMEI_04 = New System.Windows.Forms.TextBox()
    Me.txtMEI_05 = New System.Windows.Forms.TextBox()
    Me.cboNOR = New System.Windows.Forms.ComboBox()
    Me.cboCOM = New System.Windows.Forms.ComboBox()
    Me.cboBAUD = New System.Windows.Forms.ComboBox()
    Me.cboPAR = New System.Windows.Forms.ComboBox()
    Me.cboLength = New System.Windows.Forms.ComboBox()
    Me.cboSTOP = New System.Windows.Forms.ComboBox()
    Me.cboHands = New System.Windows.Forms.ComboBox()
    Me.cboDRIVER = New System.Windows.Forms.ComboBox()
    Me.cboGLIN = New System.Windows.Forms.ComboBox()
    Me.cboREFTRA = New System.Windows.Forms.ComboBox()
    Me.txtMEI_06 = New System.Windows.Forms.TextBox()
    Me.txtMEI_07 = New System.Windows.Forms.TextBox()
    Me.txtMEI_08 = New System.Windows.Forms.TextBox()
    Me.lblMEI_00 = New System.Windows.Forms.Label()
    Me.lblMEI_01 = New System.Windows.Forms.Label()
    Me.lblMEI_02 = New System.Windows.Forms.Label()
    Me.lblMEI_03 = New System.Windows.Forms.Label()
    Me.lblMEI_04 = New System.Windows.Forms.Label()
    Me.lblMEI_05 = New System.Windows.Forms.Label()
    Me.lblMEI_06 = New System.Windows.Forms.Label()
    Me.lblMEI_07 = New System.Windows.Forms.Label()
    Me.lblMEI_08 = New System.Windows.Forms.Label()
    Me.lblMEI_09 = New System.Windows.Forms.Label()
    Me.lblMEI_10 = New System.Windows.Forms.Label()
    Me.lblMEI_11 = New System.Windows.Forms.Label()
    Me.lblMEI_12 = New System.Windows.Forms.Label()
    Me.lblMEI_13 = New System.Windows.Forms.Label()
    Me.lblMEI_14 = New System.Windows.Forms.Label()
    Me.lblMEI_15 = New System.Windows.Forms.Label()
    Me.lblMEI_16 = New System.Windows.Forms.Label()
    Me.lblNWE = New System.Windows.Forms.Label()
    Me.cboSTD_0 = New System.Windows.Forms.ComboBox()
    Me.lblSTD_0 = New System.Windows.Forms.Label()
    Me.lstNOR = New System.Windows.Forms.ListBox()
    Me.lblWIN = New System.Windows.Forms.Label()
    Me.lblWIA = New System.Windows.Forms.Label()
    Me.lstWIN = New System.Windows.Forms.ListBox()
    Me.lstWIA = New System.Windows.Forms.ListBox()
    Me.btnORD = New System.Windows.Forms.Button()
    Me.lblSTD_1 = New System.Windows.Forms.Label()
    Me.lblSTD_2 = New System.Windows.Forms.Label()
    Me.lblSTD_3 = New System.Windows.Forms.Label()
    Me.lblSTD_4 = New System.Windows.Forms.Label()
    Me.lblSTD_5 = New System.Windows.Forms.Label()
    Me.cboSTD_1 = New System.Windows.Forms.ComboBox()
    Me.cboSTD_2 = New System.Windows.Forms.ComboBox()
    Me.cboSTD_3 = New System.Windows.Forms.ComboBox()
    Me.cboSTD_4 = New System.Windows.Forms.ComboBox()
    Me.cboSTD_5 = New System.Windows.Forms.ComboBox()
    Me.txtMEI_09 = New System.Windows.Forms.TextBox()
    CType(Me.BindingMEI, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.BindingMEI.SuspendLayout()
    Me.SuspendLayout()
    '
    'BindingMEI
    '
    Me.BindingMEI.AddNewItem = Me.BindingNavigatorAddNewItem
    Me.BindingMEI.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.BindingMEI.CountItem = Nothing
    Me.BindingMEI.DeleteItem = Me.BindingNavigatorDeleteItem
    Me.BindingMEI.Dock = System.Windows.Forms.DockStyle.None
    Me.BindingMEI.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BindingNavigatorMoveFirstItem, Me.BindingNavigatorMovePreviousItem, Me.BindingNavigatorMoveNextItem, Me.BindingNavigatorMoveLastItem, Me.BindingNavigatorSeparatorMei, Me.BindingNavigatorAddNewItem, Me.BindingNavigatorDeleteItem})
    Me.BindingMEI.Location = New System.Drawing.Point(302, 559)
    Me.BindingMEI.MoveFirstItem = Me.BindingNavigatorMoveFirstItem
    Me.BindingMEI.MoveLastItem = Me.BindingNavigatorMoveLastItem
    Me.BindingMEI.MoveNextItem = Me.BindingNavigatorMoveNextItem
    Me.BindingMEI.MovePreviousItem = Me.BindingNavigatorMovePreviousItem
    Me.BindingMEI.Name = "BindingMEI"
    Me.BindingMEI.PositionItem = Nothing
    Me.BindingMEI.Size = New System.Drawing.Size(156, 25)
    Me.BindingMEI.TabIndex = 0
    '
    'BindingNavigatorAddNewItem
    '
    Me.BindingNavigatorAddNewItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorAddNewItem.Image = CType(resources.GetObject("BindingNavigatorAddNewItem.Image"), System.Drawing.Image)
    Me.BindingNavigatorAddNewItem.Name = "BindingNavigatorAddNewItem"
    Me.BindingNavigatorAddNewItem.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorAddNewItem.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorAddNewItem.Text = "Neu hinzufügen"
    '
    'BindingNavigatorDeleteItem
    '
    Me.BindingNavigatorDeleteItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorDeleteItem.Image = CType(resources.GetObject("BindingNavigatorDeleteItem.Image"), System.Drawing.Image)
    Me.BindingNavigatorDeleteItem.Name = "BindingNavigatorDeleteItem"
    Me.BindingNavigatorDeleteItem.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorDeleteItem.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorDeleteItem.Text = "Löschen"
    '
    'BindingNavigatorMoveFirstItem
    '
    Me.BindingNavigatorMoveFirstItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorMoveFirstItem.Image = CType(resources.GetObject("BindingNavigatorMoveFirstItem.Image"), System.Drawing.Image)
    Me.BindingNavigatorMoveFirstItem.Name = "BindingNavigatorMoveFirstItem"
    Me.BindingNavigatorMoveFirstItem.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorMoveFirstItem.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorMoveFirstItem.Text = "Erste verschieben"
    '
    'BindingNavigatorMovePreviousItem
    '
    Me.BindingNavigatorMovePreviousItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorMovePreviousItem.Image = CType(resources.GetObject("BindingNavigatorMovePreviousItem.Image"), System.Drawing.Image)
    Me.BindingNavigatorMovePreviousItem.Name = "BindingNavigatorMovePreviousItem"
    Me.BindingNavigatorMovePreviousItem.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorMovePreviousItem.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorMovePreviousItem.Text = "Vorherige verschieben"
    '
    'BindingNavigatorMoveNextItem
    '
    Me.BindingNavigatorMoveNextItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorMoveNextItem.Image = CType(resources.GetObject("BindingNavigatorMoveNextItem.Image"), System.Drawing.Image)
    Me.BindingNavigatorMoveNextItem.Name = "BindingNavigatorMoveNextItem"
    Me.BindingNavigatorMoveNextItem.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorMoveNextItem.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorMoveNextItem.Text = "Nächste verschieben"
    '
    'BindingNavigatorMoveLastItem
    '
    Me.BindingNavigatorMoveLastItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.BindingNavigatorMoveLastItem.Image = CType(resources.GetObject("BindingNavigatorMoveLastItem.Image"), System.Drawing.Image)
    Me.BindingNavigatorMoveLastItem.Name = "BindingNavigatorMoveLastItem"
    Me.BindingNavigatorMoveLastItem.RightToLeftAutoMirrorImage = True
    Me.BindingNavigatorMoveLastItem.Size = New System.Drawing.Size(23, 22)
    Me.BindingNavigatorMoveLastItem.Text = "Letzte verschieben"
    '
    'BindingNavigatorSeparatorMei
    '
    Me.BindingNavigatorSeparatorMei.Name = "BindingNavigatorSeparatorMei"
    Me.BindingNavigatorSeparatorMei.Size = New System.Drawing.Size(6, 25)
    '
    'cboMEI
    '
    Me.cboMEI.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMEI.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboMEI.FormattingEnabled = True
    Me.cboMEI.Location = New System.Drawing.Point(302, 168)
    Me.cboMEI.Name = "cboMEI"
    Me.cboMEI.Size = New System.Drawing.Size(156, 21)
    Me.cboMEI.TabIndex = 1
    '
    'txtMEI_01
    '
    Me.txtMEI_01.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMEI_01.Location = New System.Drawing.Point(302, 195)
    Me.txtMEI_01.Name = "txtMEI_01"
    Me.txtMEI_01.Size = New System.Drawing.Size(156, 20)
    Me.txtMEI_01.TabIndex = 39
    '
    'txtMEI_00
    '
    Me.txtMEI_00.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMEI_00.Enabled = False
    Me.txtMEI_00.Location = New System.Drawing.Point(464, 169)
    Me.txtMEI_00.Name = "txtMEI_00"
    Me.txtMEI_00.Size = New System.Drawing.Size(27, 20)
    Me.txtMEI_00.TabIndex = 40
    '
    'txtMEI_02
    '
    Me.txtMEI_02.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMEI_02.Location = New System.Drawing.Point(302, 215)
    Me.txtMEI_02.Name = "txtMEI_02"
    Me.txtMEI_02.Size = New System.Drawing.Size(156, 20)
    Me.txtMEI_02.TabIndex = 41
    '
    'txtMEI_03
    '
    Me.txtMEI_03.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMEI_03.Location = New System.Drawing.Point(302, 235)
    Me.txtMEI_03.Name = "txtMEI_03"
    Me.txtMEI_03.Size = New System.Drawing.Size(241, 20)
    Me.txtMEI_03.TabIndex = 42
    '
    'txtMEI_04
    '
    Me.txtMEI_04.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMEI_04.Location = New System.Drawing.Point(302, 255)
    Me.txtMEI_04.Name = "txtMEI_04"
    Me.txtMEI_04.Size = New System.Drawing.Size(77, 20)
    Me.txtMEI_04.TabIndex = 43
    '
    'txtMEI_05
    '
    Me.txtMEI_05.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMEI_05.Location = New System.Drawing.Point(381, 255)
    Me.txtMEI_05.Name = "txtMEI_05"
    Me.txtMEI_05.Size = New System.Drawing.Size(77, 20)
    Me.txtMEI_05.TabIndex = 44
    '
    'cboNOR
    '
    Me.cboNOR.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboNOR.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboNOR.FormattingEnabled = True
    Me.cboNOR.Location = New System.Drawing.Point(302, 276)
    Me.cboNOR.Name = "cboNOR"
    Me.cboNOR.Size = New System.Drawing.Size(156, 21)
    Me.cboNOR.TabIndex = 45
    '
    'cboCOM
    '
    Me.cboCOM.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboCOM.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboCOM.FormattingEnabled = True
    Me.cboCOM.Location = New System.Drawing.Point(302, 296)
    Me.cboCOM.Name = "cboCOM"
    Me.cboCOM.Size = New System.Drawing.Size(156, 21)
    Me.cboCOM.TabIndex = 46
    '
    'cboBAUD
    '
    Me.cboBAUD.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboBAUD.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboBAUD.FormattingEnabled = True
    Me.cboBAUD.Location = New System.Drawing.Point(302, 316)
    Me.cboBAUD.Name = "cboBAUD"
    Me.cboBAUD.Size = New System.Drawing.Size(156, 21)
    Me.cboBAUD.TabIndex = 47
    '
    'cboPAR
    '
    Me.cboPAR.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboPAR.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboPAR.FormattingEnabled = True
    Me.cboPAR.Location = New System.Drawing.Point(302, 335)
    Me.cboPAR.Name = "cboPAR"
    Me.cboPAR.Size = New System.Drawing.Size(156, 21)
    Me.cboPAR.TabIndex = 48
    '
    'cboLength
    '
    Me.cboLength.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboLength.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboLength.FormattingEnabled = True
    Me.cboLength.Location = New System.Drawing.Point(302, 356)
    Me.cboLength.Name = "cboLength"
    Me.cboLength.Size = New System.Drawing.Size(156, 21)
    Me.cboLength.TabIndex = 49
    '
    'cboSTOP
    '
    Me.cboSTOP.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboSTOP.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboSTOP.FormattingEnabled = True
    Me.cboSTOP.Location = New System.Drawing.Point(303, 377)
    Me.cboSTOP.Name = "cboSTOP"
    Me.cboSTOP.Size = New System.Drawing.Size(156, 21)
    Me.cboSTOP.TabIndex = 50
    '
    'cboHands
    '
    Me.cboHands.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboHands.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboHands.FormattingEnabled = True
    Me.cboHands.Location = New System.Drawing.Point(303, 398)
    Me.cboHands.Name = "cboHands"
    Me.cboHands.Size = New System.Drawing.Size(156, 21)
    Me.cboHands.TabIndex = 51
    '
    'cboDRIVER
    '
    Me.cboDRIVER.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboDRIVER.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboDRIVER.FormattingEnabled = True
    Me.cboDRIVER.Location = New System.Drawing.Point(302, 419)
    Me.cboDRIVER.Name = "cboDRIVER"
    Me.cboDRIVER.Size = New System.Drawing.Size(156, 21)
    Me.cboDRIVER.TabIndex = 52
    '
    'cboGLIN
    '
    Me.cboGLIN.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboGLIN.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboGLIN.FormattingEnabled = True
    Me.cboGLIN.Location = New System.Drawing.Point(302, 440)
    Me.cboGLIN.Name = "cboGLIN"
    Me.cboGLIN.Size = New System.Drawing.Size(156, 21)
    Me.cboGLIN.TabIndex = 53
    '
    'cboREFTRA
    '
    Me.cboREFTRA.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboREFTRA.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboREFTRA.FormattingEnabled = True
    Me.cboREFTRA.Location = New System.Drawing.Point(302, 460)
    Me.cboREFTRA.Name = "cboREFTRA"
    Me.cboREFTRA.Size = New System.Drawing.Size(156, 21)
    Me.cboREFTRA.TabIndex = 54
    '
    'txtMEI_06
    '
    Me.txtMEI_06.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMEI_06.Location = New System.Drawing.Point(302, 481)
    Me.txtMEI_06.Name = "txtMEI_06"
    Me.txtMEI_06.Size = New System.Drawing.Size(156, 20)
    Me.txtMEI_06.TabIndex = 55
    '
    'txtMEI_07
    '
    Me.txtMEI_07.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMEI_07.Location = New System.Drawing.Point(302, 499)
    Me.txtMEI_07.Name = "txtMEI_07"
    Me.txtMEI_07.Size = New System.Drawing.Size(156, 20)
    Me.txtMEI_07.TabIndex = 56
    '
    'txtMEI_08
    '
    Me.txtMEI_08.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMEI_08.Location = New System.Drawing.Point(302, 519)
    Me.txtMEI_08.Name = "txtMEI_08"
    Me.txtMEI_08.Size = New System.Drawing.Size(156, 20)
    Me.txtMEI_08.TabIndex = 57
    Me.txtMEI_08.Visible = False
    '
    'lblMEI_00
    '
    Me.lblMEI_00.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_00.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_00.Location = New System.Drawing.Point(9, 195)
    Me.lblMEI_00.Name = "lblMEI_00"
    Me.lblMEI_00.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_00.TabIndex = 102
    Me.lblMEI_00.Text = "281"
    Me.lblMEI_00.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_01
    '
    Me.lblMEI_01.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_01.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_01.Location = New System.Drawing.Point(9, 215)
    Me.lblMEI_01.Name = "lblMEI_01"
    Me.lblMEI_01.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_01.TabIndex = 103
    Me.lblMEI_01.Text = "282"
    Me.lblMEI_01.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_02
    '
    Me.lblMEI_02.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_02.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_02.Location = New System.Drawing.Point(9, 235)
    Me.lblMEI_02.Name = "lblMEI_02"
    Me.lblMEI_02.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_02.TabIndex = 104
    Me.lblMEI_02.Text = "283"
    Me.lblMEI_02.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_03
    '
    Me.lblMEI_03.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_03.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_03.Location = New System.Drawing.Point(9, 255)
    Me.lblMEI_03.Name = "lblMEI_03"
    Me.lblMEI_03.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_03.TabIndex = 105
    Me.lblMEI_03.Text = "298"
    Me.lblMEI_03.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_04
    '
    Me.lblMEI_04.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_04.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_04.Location = New System.Drawing.Point(9, 275)
    Me.lblMEI_04.Name = "lblMEI_04"
    Me.lblMEI_04.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_04.TabIndex = 106
    Me.lblMEI_04.Text = "285"
    Me.lblMEI_04.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_05
    '
    Me.lblMEI_05.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_05.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_05.Location = New System.Drawing.Point(9, 295)
    Me.lblMEI_05.Name = "lblMEI_05"
    Me.lblMEI_05.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_05.TabIndex = 107
    Me.lblMEI_05.Text = "286"
    Me.lblMEI_05.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_06
    '
    Me.lblMEI_06.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_06.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_06.Location = New System.Drawing.Point(9, 316)
    Me.lblMEI_06.Name = "lblMEI_06"
    Me.lblMEI_06.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_06.TabIndex = 108
    Me.lblMEI_06.Text = "287"
    Me.lblMEI_06.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_07
    '
    Me.lblMEI_07.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_07.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_07.Location = New System.Drawing.Point(9, 336)
    Me.lblMEI_07.Name = "lblMEI_07"
    Me.lblMEI_07.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_07.TabIndex = 109
    Me.lblMEI_07.Text = "288"
    Me.lblMEI_07.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_08
    '
    Me.lblMEI_08.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_08.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_08.Location = New System.Drawing.Point(9, 357)
    Me.lblMEI_08.Name = "lblMEI_08"
    Me.lblMEI_08.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_08.TabIndex = 110
    Me.lblMEI_08.Text = "289"
    Me.lblMEI_08.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_09
    '
    Me.lblMEI_09.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_09.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_09.Location = New System.Drawing.Point(9, 377)
    Me.lblMEI_09.Name = "lblMEI_09"
    Me.lblMEI_09.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_09.TabIndex = 111
    Me.lblMEI_09.Text = "290"
    Me.lblMEI_09.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_10
    '
    Me.lblMEI_10.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_10.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_10.Location = New System.Drawing.Point(9, 398)
    Me.lblMEI_10.Name = "lblMEI_10"
    Me.lblMEI_10.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_10.TabIndex = 112
    Me.lblMEI_10.Text = "299"
    Me.lblMEI_10.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_11
    '
    Me.lblMEI_11.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_11.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_11.Location = New System.Drawing.Point(9, 419)
    Me.lblMEI_11.Name = "lblMEI_11"
    Me.lblMEI_11.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_11.TabIndex = 113
    Me.lblMEI_11.Text = "399"
    Me.lblMEI_11.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_12
    '
    Me.lblMEI_12.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_12.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_12.Location = New System.Drawing.Point(9, 439)
    Me.lblMEI_12.Name = "lblMEI_12"
    Me.lblMEI_12.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_12.TabIndex = 114
    Me.lblMEI_12.Text = "212"
    Me.lblMEI_12.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_13
    '
    Me.lblMEI_13.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_13.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_13.Location = New System.Drawing.Point(9, 459)
    Me.lblMEI_13.Name = "lblMEI_13"
    Me.lblMEI_13.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_13.TabIndex = 115
    Me.lblMEI_13.Text = "214"
    Me.lblMEI_13.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_14
    '
    Me.lblMEI_14.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_14.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_14.Location = New System.Drawing.Point(9, 479)
    Me.lblMEI_14.Name = "lblMEI_14"
    Me.lblMEI_14.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_14.TabIndex = 116
    Me.lblMEI_14.Text = "294"
    Me.lblMEI_14.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_15
    '
    Me.lblMEI_15.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_15.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_15.Location = New System.Drawing.Point(9, 499)
    Me.lblMEI_15.Name = "lblMEI_15"
    Me.lblMEI_15.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_15.TabIndex = 117
    Me.lblMEI_15.Text = "215"
    Me.lblMEI_15.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMEI_16
    '
    Me.lblMEI_16.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMEI_16.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMEI_16.Location = New System.Drawing.Point(9, 519)
    Me.lblMEI_16.Name = "lblMEI_16"
    Me.lblMEI_16.Size = New System.Drawing.Size(293, 20)
    Me.lblMEI_16.TabIndex = 118
    Me.lblMEI_16.Text = "284"
    Me.lblMEI_16.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.lblMEI_16.Visible = False
    '
    'lblNWE
    '
    Me.lblNWE.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblNWE.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblNWE.Location = New System.Drawing.Point(804, 61)
    Me.lblNWE.Name = "lblNWE"
    Me.lblNWE.Size = New System.Drawing.Size(56, 20)
    Me.lblNWE.TabIndex = 119
    Me.lblNWE.Text = "999"
    Me.lblNWE.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'cboSTD_0
    '
    Me.cboSTD_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboSTD_0.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboSTD_0.FormattingEnabled = True
    Me.cboSTD_0.Location = New System.Drawing.Point(721, 409)
    Me.cboSTD_0.Name = "cboSTD_0"
    Me.cboSTD_0.Size = New System.Drawing.Size(57, 21)
    Me.cboSTD_0.TabIndex = 120
    '
    'lblSTD_0
    '
    Me.lblSTD_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSTD_0.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblSTD_0.Location = New System.Drawing.Point(549, 410)
    Me.lblSTD_0.Name = "lblSTD_0"
    Me.lblSTD_0.Size = New System.Drawing.Size(173, 20)
    Me.lblSTD_0.TabIndex = 121
    Me.lblSTD_0.Text = "205"
    Me.lblSTD_0.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lstNOR
    '
    Me.lstNOR.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstNOR.FormattingEnabled = True
    Me.lstNOR.Location = New System.Drawing.Point(804, 87)
    Me.lstNOR.Name = "lstNOR"
    Me.lstNOR.Size = New System.Drawing.Size(56, 446)
    Me.lstNOR.TabIndex = 122
    '
    'lblWIN
    '
    Me.lblWIN.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblWIN.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblWIN.Location = New System.Drawing.Point(549, 61)
    Me.lblWIN.Name = "lblWIN"
    Me.lblWIN.Size = New System.Drawing.Size(229, 20)
    Me.lblWIN.TabIndex = 123
    Me.lblWIN.Text = "296"
    Me.lblWIN.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblWIA
    '
    Me.lblWIA.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblWIA.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblWIA.Location = New System.Drawing.Point(549, 265)
    Me.lblWIA.Name = "lblWIA"
    Me.lblWIA.Size = New System.Drawing.Size(229, 20)
    Me.lblWIA.TabIndex = 124
    Me.lblWIA.Text = "297"
    Me.lblWIA.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lstWIN
    '
    Me.lstWIN.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstWIN.ForeColor = System.Drawing.SystemColors.WindowText
    Me.lstWIN.FormattingEnabled = True
    Me.lstWIN.Location = New System.Drawing.Point(549, 82)
    Me.lstWIN.Name = "lstWIN"
    Me.lstWIN.Size = New System.Drawing.Size(229, 173)
    Me.lstWIN.TabIndex = 125
    '
    'lstWIA
    '
    Me.lstWIA.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstWIA.FormattingEnabled = True
    Me.lstWIA.Location = New System.Drawing.Point(549, 288)
    Me.lstWIA.Name = "lstWIA"
    Me.lstWIA.Size = New System.Drawing.Size(229, 121)
    Me.lstWIA.TabIndex = 126
    '
    'btnORD
    '
    Me.btnORD.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnORD.Location = New System.Drawing.Point(302, 136)
    Me.btnORD.Name = "btnORD"
    Me.btnORD.Size = New System.Drawing.Size(156, 26)
    Me.btnORD.TabIndex = 127
    Me.btnORD.Text = "1999"
    Me.btnORD.UseVisualStyleBackColor = True
    '
    'lblSTD_1
    '
    Me.lblSTD_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSTD_1.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblSTD_1.Location = New System.Drawing.Point(549, 430)
    Me.lblSTD_1.Name = "lblSTD_1"
    Me.lblSTD_1.Size = New System.Drawing.Size(173, 20)
    Me.lblSTD_1.TabIndex = 128
    Me.lblSTD_1.Text = "206"
    Me.lblSTD_1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblSTD_2
    '
    Me.lblSTD_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSTD_2.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblSTD_2.Location = New System.Drawing.Point(549, 450)
    Me.lblSTD_2.Name = "lblSTD_2"
    Me.lblSTD_2.Size = New System.Drawing.Size(173, 20)
    Me.lblSTD_2.TabIndex = 129
    Me.lblSTD_2.Text = "207"
    Me.lblSTD_2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblSTD_3
    '
    Me.lblSTD_3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSTD_3.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblSTD_3.Location = New System.Drawing.Point(549, 470)
    Me.lblSTD_3.Name = "lblSTD_3"
    Me.lblSTD_3.Size = New System.Drawing.Size(173, 20)
    Me.lblSTD_3.TabIndex = 130
    Me.lblSTD_3.Text = "208"
    Me.lblSTD_3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblSTD_4
    '
    Me.lblSTD_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSTD_4.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblSTD_4.Location = New System.Drawing.Point(549, 490)
    Me.lblSTD_4.Name = "lblSTD_4"
    Me.lblSTD_4.Size = New System.Drawing.Size(173, 20)
    Me.lblSTD_4.TabIndex = 131
    Me.lblSTD_4.Text = "209"
    Me.lblSTD_4.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblSTD_5
    '
    Me.lblSTD_5.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSTD_5.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblSTD_5.Location = New System.Drawing.Point(549, 510)
    Me.lblSTD_5.Name = "lblSTD_5"
    Me.lblSTD_5.Size = New System.Drawing.Size(173, 20)
    Me.lblSTD_5.TabIndex = 132
    Me.lblSTD_5.Text = "210"
    Me.lblSTD_5.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'cboSTD_1
    '
    Me.cboSTD_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboSTD_1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboSTD_1.FormattingEnabled = True
    Me.cboSTD_1.Location = New System.Drawing.Point(721, 431)
    Me.cboSTD_1.Name = "cboSTD_1"
    Me.cboSTD_1.Size = New System.Drawing.Size(57, 21)
    Me.cboSTD_1.TabIndex = 133
    '
    'cboSTD_2
    '
    Me.cboSTD_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboSTD_2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboSTD_2.FormattingEnabled = True
    Me.cboSTD_2.Location = New System.Drawing.Point(721, 451)
    Me.cboSTD_2.Name = "cboSTD_2"
    Me.cboSTD_2.Size = New System.Drawing.Size(57, 21)
    Me.cboSTD_2.TabIndex = 134
    '
    'cboSTD_3
    '
    Me.cboSTD_3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboSTD_3.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboSTD_3.FormattingEnabled = True
    Me.cboSTD_3.Location = New System.Drawing.Point(721, 471)
    Me.cboSTD_3.Name = "cboSTD_3"
    Me.cboSTD_3.Size = New System.Drawing.Size(57, 21)
    Me.cboSTD_3.TabIndex = 135
    '
    'cboSTD_4
    '
    Me.cboSTD_4.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboSTD_4.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboSTD_4.FormattingEnabled = True
    Me.cboSTD_4.Location = New System.Drawing.Point(721, 491)
    Me.cboSTD_4.Name = "cboSTD_4"
    Me.cboSTD_4.Size = New System.Drawing.Size(57, 21)
    Me.cboSTD_4.TabIndex = 136
    '
    'cboSTD_5
    '
    Me.cboSTD_5.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboSTD_5.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboSTD_5.FormattingEnabled = True
    Me.cboSTD_5.Location = New System.Drawing.Point(721, 511)
    Me.cboSTD_5.Name = "cboSTD_5"
    Me.cboSTD_5.Size = New System.Drawing.Size(57, 21)
    Me.cboSTD_5.TabIndex = 137
    '
    'txtMEI_09
    '
    Me.txtMEI_09.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMEI_09.Location = New System.Drawing.Point(464, 140)
    Me.txtMEI_09.Name = "txtMEI_09"
    Me.txtMEI_09.Size = New System.Drawing.Size(27, 20)
    Me.txtMEI_09.TabIndex = 138
    '
    'frmMEI
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.BackColor = System.Drawing.Color.Silver
    Me.ClientSize = New System.Drawing.Size(884, 662)
    Me.ControlBox = False
    Me.Controls.Add(Me.txtMEI_09)
    Me.Controls.Add(Me.cboSTD_5)
    Me.Controls.Add(Me.cboSTD_4)
    Me.Controls.Add(Me.cboSTD_3)
    Me.Controls.Add(Me.cboSTD_2)
    Me.Controls.Add(Me.cboSTD_1)
    Me.Controls.Add(Me.lblSTD_5)
    Me.Controls.Add(Me.lblSTD_4)
    Me.Controls.Add(Me.lblSTD_3)
    Me.Controls.Add(Me.lblSTD_2)
    Me.Controls.Add(Me.lblSTD_1)
    Me.Controls.Add(Me.btnORD)
    Me.Controls.Add(Me.lstWIA)
    Me.Controls.Add(Me.lstWIN)
    Me.Controls.Add(Me.lblWIA)
    Me.Controls.Add(Me.lblWIN)
    Me.Controls.Add(Me.lstNOR)
    Me.Controls.Add(Me.lblSTD_0)
    Me.Controls.Add(Me.cboSTD_0)
    Me.Controls.Add(Me.lblNWE)
    Me.Controls.Add(Me.lblMEI_16)
    Me.Controls.Add(Me.lblMEI_15)
    Me.Controls.Add(Me.lblMEI_14)
    Me.Controls.Add(Me.lblMEI_13)
    Me.Controls.Add(Me.lblMEI_12)
    Me.Controls.Add(Me.lblMEI_11)
    Me.Controls.Add(Me.lblMEI_10)
    Me.Controls.Add(Me.lblMEI_09)
    Me.Controls.Add(Me.lblMEI_08)
    Me.Controls.Add(Me.lblMEI_07)
    Me.Controls.Add(Me.lblMEI_06)
    Me.Controls.Add(Me.lblMEI_05)
    Me.Controls.Add(Me.lblMEI_04)
    Me.Controls.Add(Me.lblMEI_03)
    Me.Controls.Add(Me.lblMEI_02)
    Me.Controls.Add(Me.lblMEI_01)
    Me.Controls.Add(Me.lblMEI_00)
    Me.Controls.Add(Me.txtMEI_08)
    Me.Controls.Add(Me.txtMEI_07)
    Me.Controls.Add(Me.txtMEI_06)
    Me.Controls.Add(Me.cboREFTRA)
    Me.Controls.Add(Me.cboGLIN)
    Me.Controls.Add(Me.cboDRIVER)
    Me.Controls.Add(Me.cboHands)
    Me.Controls.Add(Me.cboSTOP)
    Me.Controls.Add(Me.cboLength)
    Me.Controls.Add(Me.cboPAR)
    Me.Controls.Add(Me.cboBAUD)
    Me.Controls.Add(Me.cboCOM)
    Me.Controls.Add(Me.cboNOR)
    Me.Controls.Add(Me.txtMEI_05)
    Me.Controls.Add(Me.txtMEI_04)
    Me.Controls.Add(Me.txtMEI_03)
    Me.Controls.Add(Me.txtMEI_02)
    Me.Controls.Add(Me.txtMEI_00)
    Me.Controls.Add(Me.txtMEI_01)
    Me.Controls.Add(Me.cboMEI)
    Me.Controls.Add(Me.BindingMEI)
    Me.Name = "frmMEI"
    Me.Text = "280"
    CType(Me.BindingMEI, System.ComponentModel.ISupportInitialize).EndInit()
    Me.BindingMEI.ResumeLayout(False)
    Me.BindingMEI.PerformLayout()
    Me.ResumeLayout(False)
    Me.PerformLayout()

  End Sub
  Friend WithEvents BindingMEI As System.Windows.Forms.BindingNavigator
  Friend WithEvents BindingNavigatorAddNewItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorDeleteItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMoveFirstItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMovePreviousItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMoveNextItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMoveLastItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorSeparatorMei As System.Windows.Forms.ToolStripSeparator
  Friend WithEvents cboMEI As System.Windows.Forms.ComboBox
  Friend WithEvents txtMEI_01 As System.Windows.Forms.TextBox
  Friend WithEvents txtMEI_00 As System.Windows.Forms.TextBox
  Friend WithEvents txtMEI_02 As System.Windows.Forms.TextBox
  Friend WithEvents txtMEI_03 As System.Windows.Forms.TextBox
  Friend WithEvents txtMEI_04 As System.Windows.Forms.TextBox
  Friend WithEvents txtMEI_05 As System.Windows.Forms.TextBox
  Friend WithEvents cboNOR As System.Windows.Forms.ComboBox
  Friend WithEvents cboCOM As System.Windows.Forms.ComboBox
  Friend WithEvents cboBAUD As System.Windows.Forms.ComboBox
  Friend WithEvents cboPAR As System.Windows.Forms.ComboBox
  Friend WithEvents cboLength As System.Windows.Forms.ComboBox
  Friend WithEvents cboSTOP As System.Windows.Forms.ComboBox
  Friend WithEvents cboHands As System.Windows.Forms.ComboBox
  Friend WithEvents cboDRIVER As System.Windows.Forms.ComboBox
  Friend WithEvents cboGLIN As System.Windows.Forms.ComboBox
  Friend WithEvents cboREFTRA As System.Windows.Forms.ComboBox
  Friend WithEvents txtMEI_06 As System.Windows.Forms.TextBox
  Friend WithEvents txtMEI_07 As System.Windows.Forms.TextBox
  Friend WithEvents txtMEI_08 As System.Windows.Forms.TextBox
  Friend WithEvents lblMEI_00 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_01 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_02 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_03 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_04 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_05 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_06 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_07 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_08 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_09 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_10 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_11 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_12 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_13 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_14 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_15 As System.Windows.Forms.Label
  Friend WithEvents lblMEI_16 As System.Windows.Forms.Label
  Friend WithEvents lblNWE As System.Windows.Forms.Label
  Friend WithEvents cboSTD_0 As System.Windows.Forms.ComboBox
  Friend WithEvents lblSTD_0 As System.Windows.Forms.Label
  Friend WithEvents lstNOR As System.Windows.Forms.ListBox
  Friend WithEvents lblWIN As System.Windows.Forms.Label
  Friend WithEvents lblWIA As System.Windows.Forms.Label
  Friend WithEvents lstWIN As System.Windows.Forms.ListBox
  Friend WithEvents lstWIA As System.Windows.Forms.ListBox
  Friend WithEvents btnORD As System.Windows.Forms.Button
  Friend WithEvents lblSTD_1 As System.Windows.Forms.Label
  Friend WithEvents lblSTD_2 As System.Windows.Forms.Label
  Friend WithEvents lblSTD_3 As System.Windows.Forms.Label
  Friend WithEvents lblSTD_4 As System.Windows.Forms.Label
  Friend WithEvents lblSTD_5 As System.Windows.Forms.Label
  Friend WithEvents cboSTD_1 As System.Windows.Forms.ComboBox
  Friend WithEvents cboSTD_2 As System.Windows.Forms.ComboBox
  Friend WithEvents cboSTD_3 As System.Windows.Forms.ComboBox
  Friend WithEvents cboSTD_4 As System.Windows.Forms.ComboBox
  Friend WithEvents cboSTD_5 As System.Windows.Forms.ComboBox
  Friend WithEvents txtMEI_09 As System.Windows.Forms.TextBox
End Class
