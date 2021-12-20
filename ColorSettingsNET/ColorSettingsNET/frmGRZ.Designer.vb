<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmGRZ
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
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmGRZ))
    Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton()
    Me.BindingMSY = New System.Windows.Forms.BindingNavigator(Me.components)
    Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton3 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton4 = New System.Windows.Forms.ToolStripButton()
    Me.lblMSY = New System.Windows.Forms.Label()
    Me.txtMSY = New System.Windows.Forms.TextBox()
    Me.lblGRP_0 = New System.Windows.Forms.Label()
    Me.lblGRP_1 = New System.Windows.Forms.Label()
    Me.lblGRP_2 = New System.Windows.Forms.Label()
    Me.txtGRP_0 = New System.Windows.Forms.TextBox()
    Me.txtGRP_1 = New System.Windows.Forms.TextBox()
    Me.txtGRP_2 = New System.Windows.Forms.TextBox()
    Me.lblUserND = New System.Windows.Forms.Label()
    Me.lblUserRO = New System.Windows.Forms.Label()
    Me.lblNDSelect = New System.Windows.Forms.Label()
    Me.lblROSelect = New System.Windows.Forms.Label()
    Me.lstUseND = New System.Windows.Forms.ListBox()
    Me.lstUseRO = New System.Windows.Forms.ListBox()
    Me.lstUseNDSelect = New System.Windows.Forms.ListBox()
    Me.lstUseROSelect = New System.Windows.Forms.ListBox()
    Me.lblGRP_3 = New System.Windows.Forms.Label()
    Me.cboMesGRP = New System.Windows.Forms.ComboBox()
    Me.lblMES = New System.Windows.Forms.Label()
    Me.txtMES = New System.Windows.Forms.TextBox()
    Me.BindingMES = New System.Windows.Forms.BindingNavigator(Me.components)
    Me.ToolStripButton5 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton6 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton7 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton8 = New System.Windows.Forms.ToolStripButton()
    Me.btnORD = New System.Windows.Forms.Button()
    Me.BindingGRP = New System.Windows.Forms.BindingNavigator(Me.components)
    Me.BindingNavigatorAddNewItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMoveFirstItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMovePreviousItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMoveNextItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMoveLastItem = New System.Windows.Forms.ToolStripButton()
    Me.cboMSY = New System.Windows.Forms.ComboBox()
    Me.cboMES = New System.Windows.Forms.ComboBox()
    Me.cboGRP = New System.Windows.Forms.ComboBox()
    CType(Me.BindingMSY, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.BindingMSY.SuspendLayout()
    CType(Me.BindingMES, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.BindingMES.SuspendLayout()
    CType(Me.BindingGRP, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.BindingGRP.SuspendLayout()
    Me.SuspendLayout()
    '
    'ToolStripButton2
    '
    Me.ToolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton2.Image = CType(resources.GetObject("ToolStripButton2.Image"), System.Drawing.Image)
    Me.ToolStripButton2.Name = "ToolStripButton2"
    Me.ToolStripButton2.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton2.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton2.Text = "Vorherige verschieben"
    '
    'BindingMSY
    '
    Me.BindingMSY.AddNewItem = Nothing
    Me.BindingMSY.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.BindingMSY.CountItem = Nothing
    Me.BindingMSY.DeleteItem = Nothing
    Me.BindingMSY.Dock = System.Windows.Forms.DockStyle.None
    Me.BindingMSY.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton1, Me.ToolStripButton2, Me.ToolStripButton3, Me.ToolStripButton4})
    Me.BindingMSY.Location = New System.Drawing.Point(677, 11)
    Me.BindingMSY.MoveFirstItem = Me.ToolStripButton1
    Me.BindingMSY.MoveLastItem = Me.ToolStripButton4
    Me.BindingMSY.MoveNextItem = Me.ToolStripButton3
    Me.BindingMSY.MovePreviousItem = Me.ToolStripButton2
    Me.BindingMSY.Name = "BindingMSY"
    Me.BindingMSY.PositionItem = Nothing
    Me.BindingMSY.Size = New System.Drawing.Size(104, 25)
    Me.BindingMSY.TabIndex = 12
    '
    'ToolStripButton1
    '
    Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton1.Image = CType(resources.GetObject("ToolStripButton1.Image"), System.Drawing.Image)
    Me.ToolStripButton1.Name = "ToolStripButton1"
    Me.ToolStripButton1.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton1.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton1.Text = "Erste verschieben"
    '
    'ToolStripButton3
    '
    Me.ToolStripButton3.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton3.Image = CType(resources.GetObject("ToolStripButton3.Image"), System.Drawing.Image)
    Me.ToolStripButton3.Name = "ToolStripButton3"
    Me.ToolStripButton3.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton3.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton3.Text = "Nächste verschieben"
    '
    'ToolStripButton4
    '
    Me.ToolStripButton4.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton4.Image = CType(resources.GetObject("ToolStripButton4.Image"), System.Drawing.Image)
    Me.ToolStripButton4.Name = "ToolStripButton4"
    Me.ToolStripButton4.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton4.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton4.Text = "Letzte verschieben"
    '
    'lblMSY
    '
    Me.lblMSY.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMSY.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMSY.Location = New System.Drawing.Point(122, 11)
    Me.lblMSY.Name = "lblMSY"
    Me.lblMSY.Size = New System.Drawing.Size(215, 20)
    Me.lblMSY.TabIndex = 10
    Me.lblMSY.Text = "422"
    Me.lblMSY.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtMSY
    '
    Me.txtMSY.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMSY.Location = New System.Drawing.Point(337, 11)
    Me.txtMSY.Name = "txtMSY"
    Me.txtMSY.Size = New System.Drawing.Size(337, 20)
    Me.txtMSY.TabIndex = 20
    '
    'lblGRP_0
    '
    Me.lblGRP_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRP_0.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRP_0.Location = New System.Drawing.Point(342, 116)
    Me.lblGRP_0.Name = "lblGRP_0"
    Me.lblGRP_0.Size = New System.Drawing.Size(151, 20)
    Me.lblGRP_0.TabIndex = 21
    Me.lblGRP_0.Text = "386"
    Me.lblGRP_0.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblGRP_1
    '
    Me.lblGRP_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRP_1.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblGRP_1.Location = New System.Drawing.Point(106, 139)
    Me.lblGRP_1.Name = "lblGRP_1"
    Me.lblGRP_1.Size = New System.Drawing.Size(236, 20)
    Me.lblGRP_1.TabIndex = 22
    Me.lblGRP_1.Text = "282"
    Me.lblGRP_1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblGRP_2
    '
    Me.lblGRP_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRP_2.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblGRP_2.Location = New System.Drawing.Point(106, 159)
    Me.lblGRP_2.Name = "lblGRP_2"
    Me.lblGRP_2.Size = New System.Drawing.Size(236, 20)
    Me.lblGRP_2.TabIndex = 23
    Me.lblGRP_2.Text = "283"
    Me.lblGRP_2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtGRP_0
    '
    Me.txtGRP_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtGRP_0.Location = New System.Drawing.Point(496, 116)
    Me.txtGRP_0.Name = "txtGRP_0"
    Me.txtGRP_0.Size = New System.Drawing.Size(28, 20)
    Me.txtGRP_0.TabIndex = 24
    '
    'txtGRP_1
    '
    Me.txtGRP_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtGRP_1.Location = New System.Drawing.Point(342, 139)
    Me.txtGRP_1.Name = "txtGRP_1"
    Me.txtGRP_1.Size = New System.Drawing.Size(151, 20)
    Me.txtGRP_1.TabIndex = 25
    '
    'txtGRP_2
    '
    Me.txtGRP_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtGRP_2.Location = New System.Drawing.Point(342, 159)
    Me.txtGRP_2.Name = "txtGRP_2"
    Me.txtGRP_2.Size = New System.Drawing.Size(363, 20)
    Me.txtGRP_2.TabIndex = 26
    '
    'lblUserND
    '
    Me.lblUserND.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblUserND.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblUserND.Location = New System.Drawing.Point(106, 213)
    Me.lblUserND.Name = "lblUserND"
    Me.lblUserND.Size = New System.Drawing.Size(277, 20)
    Me.lblUserND.TabIndex = 28
    Me.lblUserND.Text = "316"
    Me.lblUserND.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblUserRO
    '
    Me.lblUserRO.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblUserRO.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblUserRO.Location = New System.Drawing.Point(456, 213)
    Me.lblUserRO.Name = "lblUserRO"
    Me.lblUserRO.Size = New System.Drawing.Size(277, 20)
    Me.lblUserRO.TabIndex = 29
    Me.lblUserRO.Text = "316"
    Me.lblUserRO.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblNDSelect
    '
    Me.lblNDSelect.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblNDSelect.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblNDSelect.Location = New System.Drawing.Point(106, 389)
    Me.lblNDSelect.Name = "lblNDSelect"
    Me.lblNDSelect.Size = New System.Drawing.Size(277, 20)
    Me.lblNDSelect.TabIndex = 30
    Me.lblNDSelect.Text = "318"
    Me.lblNDSelect.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lblROSelect
    '
    Me.lblROSelect.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblROSelect.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblROSelect.Location = New System.Drawing.Point(456, 389)
    Me.lblROSelect.Name = "lblROSelect"
    Me.lblROSelect.Size = New System.Drawing.Size(277, 20)
    Me.lblROSelect.TabIndex = 31
    Me.lblROSelect.Text = "317"
    Me.lblROSelect.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'lstUseND
    '
    Me.lstUseND.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstUseND.FormattingEnabled = True
    Me.lstUseND.Location = New System.Drawing.Point(105, 236)
    Me.lstUseND.Name = "lstUseND"
    Me.lstUseND.Size = New System.Drawing.Size(278, 134)
    Me.lstUseND.TabIndex = 170
    '
    'lstUseRO
    '
    Me.lstUseRO.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstUseRO.FormattingEnabled = True
    Me.lstUseRO.Location = New System.Drawing.Point(455, 236)
    Me.lstUseRO.Name = "lstUseRO"
    Me.lstUseRO.Size = New System.Drawing.Size(278, 134)
    Me.lstUseRO.TabIndex = 171
    '
    'lstUseNDSelect
    '
    Me.lstUseNDSelect.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstUseNDSelect.FormattingEnabled = True
    Me.lstUseNDSelect.Location = New System.Drawing.Point(106, 412)
    Me.lstUseNDSelect.Name = "lstUseNDSelect"
    Me.lstUseNDSelect.Size = New System.Drawing.Size(277, 108)
    Me.lstUseNDSelect.TabIndex = 172
    '
    'lstUseROSelect
    '
    Me.lstUseROSelect.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstUseROSelect.FormattingEnabled = True
    Me.lstUseROSelect.Location = New System.Drawing.Point(455, 412)
    Me.lstUseROSelect.Name = "lstUseROSelect"
    Me.lstUseROSelect.Size = New System.Drawing.Size(278, 108)
    Me.lstUseROSelect.TabIndex = 173
    '
    'lblGRP_3
    '
    Me.lblGRP_3.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRP_3.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblGRP_3.Location = New System.Drawing.Point(106, 179)
    Me.lblGRP_3.Name = "lblGRP_3"
    Me.lblGRP_3.Size = New System.Drawing.Size(236, 20)
    Me.lblGRP_3.TabIndex = 174
    Me.lblGRP_3.Text = "264"
    Me.lblGRP_3.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'cboMesGRP
    '
    Me.cboMesGRP.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMesGRP.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboMesGRP.FormattingEnabled = True
    Me.cboMesGRP.Location = New System.Drawing.Point(342, 180)
    Me.cboMesGRP.Name = "cboMesGRP"
    Me.cboMesGRP.Size = New System.Drawing.Size(363, 21)
    Me.cboMesGRP.TabIndex = 175
    '
    'lblMES
    '
    Me.lblMES.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMES.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMES.Location = New System.Drawing.Point(122, 73)
    Me.lblMES.Name = "lblMES"
    Me.lblMES.Size = New System.Drawing.Size(215, 20)
    Me.lblMES.TabIndex = 176
    Me.lblMES.Text = "420"
    Me.lblMES.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtMES
    '
    Me.txtMES.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMES.Location = New System.Drawing.Point(337, 73)
    Me.txtMES.Name = "txtMES"
    Me.txtMES.Size = New System.Drawing.Size(337, 20)
    Me.txtMES.TabIndex = 177
    '
    'BindingMES
    '
    Me.BindingMES.AddNewItem = Nothing
    Me.BindingMES.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.BindingMES.CountItem = Nothing
    Me.BindingMES.DeleteItem = Nothing
    Me.BindingMES.Dock = System.Windows.Forms.DockStyle.None
    Me.BindingMES.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton5, Me.ToolStripButton6, Me.ToolStripButton7, Me.ToolStripButton8})
    Me.BindingMES.Location = New System.Drawing.Point(677, 73)
    Me.BindingMES.MoveFirstItem = Me.ToolStripButton5
    Me.BindingMES.MoveLastItem = Me.ToolStripButton8
    Me.BindingMES.MoveNextItem = Me.ToolStripButton7
    Me.BindingMES.MovePreviousItem = Me.ToolStripButton6
    Me.BindingMES.Name = "BindingMES"
    Me.BindingMES.PositionItem = Nothing
    Me.BindingMES.Size = New System.Drawing.Size(104, 25)
    Me.BindingMES.TabIndex = 178
    '
    'ToolStripButton5
    '
    Me.ToolStripButton5.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton5.Image = CType(resources.GetObject("ToolStripButton5.Image"), System.Drawing.Image)
    Me.ToolStripButton5.Name = "ToolStripButton5"
    Me.ToolStripButton5.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton5.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton5.Text = "Erste verschieben"
    '
    'ToolStripButton6
    '
    Me.ToolStripButton6.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton6.Image = CType(resources.GetObject("ToolStripButton6.Image"), System.Drawing.Image)
    Me.ToolStripButton6.Name = "ToolStripButton6"
    Me.ToolStripButton6.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton6.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton6.Text = "Vorherige verschieben"
    '
    'ToolStripButton7
    '
    Me.ToolStripButton7.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton7.Image = CType(resources.GetObject("ToolStripButton7.Image"), System.Drawing.Image)
    Me.ToolStripButton7.Name = "ToolStripButton7"
    Me.ToolStripButton7.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton7.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton7.Text = "Nächste verschieben"
    '
    'ToolStripButton8
    '
    Me.ToolStripButton8.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
    Me.ToolStripButton8.Image = CType(resources.GetObject("ToolStripButton8.Image"), System.Drawing.Image)
    Me.ToolStripButton8.Name = "ToolStripButton8"
    Me.ToolStripButton8.RightToLeftAutoMirrorImage = True
    Me.ToolStripButton8.Size = New System.Drawing.Size(23, 22)
    Me.ToolStripButton8.Text = "Letzte verschieben"
    '
    'btnORD
    '
    Me.btnORD.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnORD.Location = New System.Drawing.Point(782, 41)
    Me.btnORD.Name = "btnORD"
    Me.btnORD.Size = New System.Drawing.Size(148, 26)
    Me.btnORD.TabIndex = 179
    Me.btnORD.Text = "1999"
    Me.btnORD.UseVisualStyleBackColor = True
    '
    'BindingGRP
    '
    Me.BindingGRP.AddNewItem = Me.BindingNavigatorAddNewItem
    Me.BindingGRP.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.BindingGRP.CountItem = Nothing
    Me.BindingGRP.DeleteItem = Nothing
    Me.BindingGRP.Dock = System.Windows.Forms.DockStyle.None
    Me.BindingGRP.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BindingNavigatorMoveFirstItem, Me.BindingNavigatorMovePreviousItem, Me.BindingNavigatorMoveNextItem, Me.BindingNavigatorMoveLastItem, Me.BindingNavigatorAddNewItem})
    Me.BindingGRP.Location = New System.Drawing.Point(719, 134)
    Me.BindingGRP.MoveFirstItem = Me.BindingNavigatorMoveFirstItem
    Me.BindingGRP.MoveLastItem = Me.BindingNavigatorMoveLastItem
    Me.BindingGRP.MoveNextItem = Me.BindingNavigatorMoveNextItem
    Me.BindingGRP.MovePreviousItem = Me.BindingNavigatorMovePreviousItem
    Me.BindingGRP.Name = "BindingGRP"
    Me.BindingGRP.PositionItem = Nothing
    Me.BindingGRP.Size = New System.Drawing.Size(127, 25)
    Me.BindingGRP.TabIndex = 180
    Me.BindingGRP.Text = "BindingNavigator1"
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
    'cboMSY
    '
    Me.cboMSY.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMSY.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.cboMSY.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboMSY.FormattingEnabled = True
    Me.cboMSY.Location = New System.Drawing.Point(337, 29)
    Me.cboMSY.Name = "cboMSY"
    Me.cboMSY.Size = New System.Drawing.Size(337, 21)
    Me.cboMSY.TabIndex = 181
    '
    'cboMES
    '
    Me.cboMES.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboMES.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.cboMES.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboMES.FormattingEnabled = True
    Me.cboMES.Location = New System.Drawing.Point(337, 92)
    Me.cboMES.Name = "cboMES"
    Me.cboMES.Size = New System.Drawing.Size(337, 21)
    Me.cboMES.TabIndex = 182
    '
    'cboGRP
    '
    Me.cboGRP.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboGRP.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.cboGRP.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
    Me.cboGRP.FormattingEnabled = True
    Me.cboGRP.Location = New System.Drawing.Point(496, 138)
    Me.cboGRP.Name = "cboGRP"
    Me.cboGRP.Size = New System.Drawing.Size(209, 21)
    Me.cboGRP.TabIndex = 183
    '
    'frmGRZ
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.BackColor = System.Drawing.Color.Silver
    Me.ClientSize = New System.Drawing.Size(984, 662)
    Me.ControlBox = False
    Me.Controls.Add(Me.cboGRP)
    Me.Controls.Add(Me.cboMES)
    Me.Controls.Add(Me.cboMSY)
    Me.Controls.Add(Me.BindingGRP)
    Me.Controls.Add(Me.btnORD)
    Me.Controls.Add(Me.BindingMES)
    Me.Controls.Add(Me.txtMES)
    Me.Controls.Add(Me.lblMES)
    Me.Controls.Add(Me.cboMesGRP)
    Me.Controls.Add(Me.lblGRP_3)
    Me.Controls.Add(Me.lstUseROSelect)
    Me.Controls.Add(Me.lstUseNDSelect)
    Me.Controls.Add(Me.lstUseRO)
    Me.Controls.Add(Me.lstUseND)
    Me.Controls.Add(Me.lblROSelect)
    Me.Controls.Add(Me.lblNDSelect)
    Me.Controls.Add(Me.lblUserRO)
    Me.Controls.Add(Me.lblUserND)
    Me.Controls.Add(Me.txtGRP_2)
    Me.Controls.Add(Me.txtGRP_1)
    Me.Controls.Add(Me.txtGRP_0)
    Me.Controls.Add(Me.lblGRP_2)
    Me.Controls.Add(Me.lblGRP_1)
    Me.Controls.Add(Me.lblGRP_0)
    Me.Controls.Add(Me.txtMSY)
    Me.Controls.Add(Me.BindingMSY)
    Me.Controls.Add(Me.lblMSY)
    Me.Name = "frmGRZ"
    Me.Text = "frmGRZ"
    CType(Me.BindingMSY, System.ComponentModel.ISupportInitialize).EndInit()
    Me.BindingMSY.ResumeLayout(False)
    Me.BindingMSY.PerformLayout()
    CType(Me.BindingMES, System.ComponentModel.ISupportInitialize).EndInit()
    Me.BindingMES.ResumeLayout(False)
    Me.BindingMES.PerformLayout()
    CType(Me.BindingGRP, System.ComponentModel.ISupportInitialize).EndInit()
    Me.BindingGRP.ResumeLayout(False)
    Me.BindingGRP.PerformLayout()
    Me.ResumeLayout(False)
    Me.PerformLayout()

  End Sub
  Friend WithEvents ToolStripButton2 As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingMSY As System.Windows.Forms.BindingNavigator
  Friend WithEvents ToolStripButton1 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton3 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton4 As System.Windows.Forms.ToolStripButton
  Friend WithEvents lblMSY As System.Windows.Forms.Label
  Friend WithEvents txtMSY As System.Windows.Forms.TextBox
  Friend WithEvents lblGRP_0 As System.Windows.Forms.Label
  Friend WithEvents lblGRP_1 As System.Windows.Forms.Label
  Friend WithEvents lblGRP_2 As System.Windows.Forms.Label
  Friend WithEvents txtGRP_0 As System.Windows.Forms.TextBox
  Friend WithEvents txtGRP_1 As System.Windows.Forms.TextBox
  Friend WithEvents txtGRP_2 As System.Windows.Forms.TextBox
  Friend WithEvents lblUserND As System.Windows.Forms.Label
  Friend WithEvents lblUserRO As System.Windows.Forms.Label
  Friend WithEvents lblNDSelect As System.Windows.Forms.Label
  Friend WithEvents lblROSelect As System.Windows.Forms.Label
  Friend WithEvents lstUseND As System.Windows.Forms.ListBox
  Friend WithEvents lstUseRO As System.Windows.Forms.ListBox
  Friend WithEvents lstUseNDSelect As System.Windows.Forms.ListBox
  Friend WithEvents lstUseROSelect As System.Windows.Forms.ListBox
  Friend WithEvents lblGRP_3 As System.Windows.Forms.Label
  Friend WithEvents cboMesGRP As System.Windows.Forms.ComboBox
  Friend WithEvents lblMES As System.Windows.Forms.Label
  Friend WithEvents txtMES As System.Windows.Forms.TextBox
  Friend WithEvents BindingMES As System.Windows.Forms.BindingNavigator
  Friend WithEvents ToolStripButton5 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton6 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton7 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton8 As System.Windows.Forms.ToolStripButton
  Friend WithEvents btnORD As System.Windows.Forms.Button
  Friend WithEvents BindingGRP As System.Windows.Forms.BindingNavigator
  Friend WithEvents BindingNavigatorAddNewItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMoveFirstItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMovePreviousItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMoveNextItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMoveLastItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents cboMSY As System.Windows.Forms.ComboBox
  Friend WithEvents cboMES As System.Windows.Forms.ComboBox
  Friend WithEvents cboGRP As System.Windows.Forms.ComboBox
End Class
