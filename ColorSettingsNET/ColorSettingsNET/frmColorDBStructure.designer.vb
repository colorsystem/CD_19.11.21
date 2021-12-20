<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmColorDBStructure
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
    Me.splColorTreeStruct = New System.Windows.Forms.SplitContainer()
    Me.lblTreeView = New System.Windows.Forms.Label()
    Me.btnUserMethodeMessgerätGkwert = New System.Windows.Forms.Button()
    Me.btnUserMischMessgGKWerte = New System.Windows.Forms.Button()
    Me.btnUserMessg = New System.Windows.Forms.Button()
    Me.btnUserMethAnwsgMerk = New System.Windows.Forms.Button()
    Me.btnUserMethLicht = New System.Windows.Forms.Button()
    Me.btnMischGroupReadonly = New System.Windows.Forms.Button()
    Me.btnMischGroupDontShow = New System.Windows.Forms.Button()
    Me.btnMessgWinkel = New System.Windows.Forms.Button()
    Me.chkOrder = New System.Windows.Forms.CheckBox()
    Me.btnMessgGroupReadonly = New System.Windows.Forms.Button()
    Me.btnMessgGroupDontShow = New System.Windows.Forms.Button()
    Me.chkWithID = New System.Windows.Forms.CheckBox()
    Me.btnMessgMisch = New System.Windows.Forms.Button()
    Me.btnMischMessg = New System.Windows.Forms.Button()
    Me.btnUserMisch = New System.Windows.Forms.Button()
    Me.btnMessgMasterSlave = New System.Windows.Forms.Button()
    Me.btnUserMethMessgWinkel = New System.Windows.Forms.Button()
    Me.TreeColorStruct = New System.Windows.Forms.TreeView()
    Me.splColorDataBase = New System.Windows.Forms.SplitContainer()
    Me.btnMischSelect = New System.Windows.Forms.Button()
    Me.btnMessgSelect = New System.Windows.Forms.Button()
    Me.lblDBCopy = New System.Windows.Forms.Label()
    Me.btnRezepte = New System.Windows.Forms.Button()
    Me.btnRwerte = New System.Windows.Forms.Button()
    Me.btnMisch = New System.Windows.Forms.Button()
    Me.btnMessg = New System.Windows.Forms.Button()
    Me.btnVoid = New System.Windows.Forms.Button()
    Me.lstMisch = New System.Windows.Forms.ListBox()
    Me.lstMessg = New System.Windows.Forms.ListBox()
    Me.txtMessgKE = New System.Windows.Forms.TextBox()
    Me.lblMessgKE = New System.Windows.Forms.Label()
    Me.dbgMessgeräte = New System.Windows.Forms.DataGridView()
    Me.dbgMeasure00x = New System.Windows.Forms.DataGridView()
    Me.lblShowMeasDevices = New System.Windows.Forms.Label()
    Me.splBasis = New System.Windows.Forms.SplitContainer()
    Me.btnDataBase = New System.Windows.Forms.Button()
    Me.btnTreeView = New System.Windows.Forms.Button()
    CType(Me.splColorTreeStruct, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splColorTreeStruct.Panel1.SuspendLayout()
    Me.splColorTreeStruct.Panel2.SuspendLayout()
    Me.splColorTreeStruct.SuspendLayout()
    CType(Me.splColorDataBase, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splColorDataBase.Panel1.SuspendLayout()
    Me.splColorDataBase.Panel2.SuspendLayout()
    Me.splColorDataBase.SuspendLayout()
    CType(Me.dbgMessgeräte, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.dbgMeasure00x, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.splBasis, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splBasis.Panel1.SuspendLayout()
    Me.splBasis.Panel2.SuspendLayout()
    Me.splBasis.SuspendLayout()
    Me.SuspendLayout()
    '
    'splColorTreeStruct
    '
    Me.splColorTreeStruct.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splColorTreeStruct.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.splColorTreeStruct.Location = New System.Drawing.Point(0, 0)
    Me.splColorTreeStruct.Name = "splColorTreeStruct"
    '
    'splColorTreeStruct.Panel1
    '
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.lblTreeView)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnUserMethodeMessgerätGkwert)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnUserMischMessgGKWerte)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnUserMessg)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnUserMethAnwsgMerk)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnUserMethLicht)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnMischGroupReadonly)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnMischGroupDontShow)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnMessgWinkel)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.chkOrder)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnMessgGroupReadonly)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnMessgGroupDontShow)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.chkWithID)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnMessgMisch)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnMischMessg)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnUserMisch)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnMessgMasterSlave)
    Me.splColorTreeStruct.Panel1.Controls.Add(Me.btnUserMethMessgWinkel)
    '
    'splColorTreeStruct.Panel2
    '
    Me.splColorTreeStruct.Panel2.Controls.Add(Me.TreeColorStruct)
    Me.splColorTreeStruct.Size = New System.Drawing.Size(1147, 532)
    Me.splColorTreeStruct.SplitterDistance = 361
    Me.splColorTreeStruct.TabIndex = 0
    '
    'lblTreeView
    '
    Me.lblTreeView.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblTreeView.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblTreeView.Location = New System.Drawing.Point(12, 6)
    Me.lblTreeView.Name = "lblTreeView"
    Me.lblTreeView.Size = New System.Drawing.Size(337, 18)
    Me.lblTreeView.TabIndex = 17
    Me.lblTreeView.Text = "147 145"
    Me.lblTreeView.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'btnUserMethodeMessgerätGkwert
    '
    Me.btnUserMethodeMessgerätGkwert.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnUserMethodeMessgerätGkwert.Location = New System.Drawing.Point(1, 133)
    Me.btnUserMethodeMessgerätGkwert.Name = "btnUserMethodeMessgerätGkwert"
    Me.btnUserMethodeMessgerätGkwert.Size = New System.Drawing.Size(358, 22)
    Me.btnUserMethodeMessgerätGkwert.TabIndex = 16
    Me.btnUserMethodeMessgerätGkwert.Text = "402 405 403 151"
    Me.btnUserMethodeMessgerätGkwert.UseVisualStyleBackColor = True
    '
    'btnUserMischMessgGKWerte
    '
    Me.btnUserMischMessgGKWerte.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnUserMischMessgGKWerte.Location = New System.Drawing.Point(1, 236)
    Me.btnUserMischMessgGKWerte.Name = "btnUserMischMessgGKWerte"
    Me.btnUserMischMessgGKWerte.Size = New System.Drawing.Size(358, 22)
    Me.btnUserMischMessgGKWerte.TabIndex = 15
    Me.btnUserMischMessgGKWerte.Text = "402 406 403 151"
    Me.btnUserMischMessgGKWerte.UseVisualStyleBackColor = True
    '
    'btnUserMessg
    '
    Me.btnUserMessg.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnUserMessg.Location = New System.Drawing.Point(1, 256)
    Me.btnUserMessg.Name = "btnUserMessg"
    Me.btnUserMessg.Size = New System.Drawing.Size(358, 22)
    Me.btnUserMessg.TabIndex = 14
    Me.btnUserMessg.Text = "402 403 407"
    Me.btnUserMessg.UseVisualStyleBackColor = True
    '
    'btnUserMethAnwsgMerk
    '
    Me.btnUserMethAnwsgMerk.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnUserMethAnwsgMerk.Location = New System.Drawing.Point(1, 161)
    Me.btnUserMethAnwsgMerk.Name = "btnUserMethAnwsgMerk"
    Me.btnUserMethAnwsgMerk.Size = New System.Drawing.Size(358, 22)
    Me.btnUserMethAnwsgMerk.TabIndex = 13
    Me.btnUserMethAnwsgMerk.Text = "402 405 404 414"
    Me.btnUserMethAnwsgMerk.UseVisualStyleBackColor = True
    '
    'btnUserMethLicht
    '
    Me.btnUserMethLicht.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnUserMethLicht.Location = New System.Drawing.Point(1, 189)
    Me.btnUserMethLicht.Name = "btnUserMethLicht"
    Me.btnUserMethLicht.Size = New System.Drawing.Size(358, 22)
    Me.btnUserMethLicht.TabIndex = 12
    Me.btnUserMethLicht.Text = "402 405 149 "
    Me.btnUserMethLicht.UseVisualStyleBackColor = True
    '
    'btnMischGroupReadonly
    '
    Me.btnMischGroupReadonly.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnMischGroupReadonly.Location = New System.Drawing.Point(1, 422)
    Me.btnMischGroupReadonly.Name = "btnMischGroupReadonly"
    Me.btnMischGroupReadonly.Size = New System.Drawing.Size(358, 22)
    Me.btnMischGroupReadonly.TabIndex = 11
    Me.btnMischGroupReadonly.Text = "406 386 14 139 138"
    Me.btnMischGroupReadonly.UseVisualStyleBackColor = True
    '
    'btnMischGroupDontShow
    '
    Me.btnMischGroupDontShow.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnMischGroupDontShow.Location = New System.Drawing.Point(1, 394)
    Me.btnMischGroupDontShow.Name = "btnMischGroupDontShow"
    Me.btnMischGroupDontShow.Size = New System.Drawing.Size(358, 22)
    Me.btnMischGroupDontShow.TabIndex = 10
    Me.btnMischGroupDontShow.Text = "406386 14 142"
    Me.btnMischGroupDontShow.UseVisualStyleBackColor = True
    '
    'btnMessgWinkel
    '
    Me.btnMessgWinkel.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnMessgWinkel.Location = New System.Drawing.Point(1, 53)
    Me.btnMessgWinkel.Name = "btnMessgWinkel"
    Me.btnMessgWinkel.Size = New System.Drawing.Size(358, 22)
    Me.btnMessgWinkel.TabIndex = 9
    Me.btnMessgWinkel.Text = "403 173"
    Me.btnMessgWinkel.UseVisualStyleBackColor = True
    '
    'chkOrder
    '
    Me.chkOrder.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkOrder.Location = New System.Drawing.Point(210, 32)
    Me.chkOrder.Name = "chkOrder"
    Me.chkOrder.Size = New System.Drawing.Size(138, 16)
    Me.chkOrder.TabIndex = 8
    Me.chkOrder.Text = "855"
    Me.chkOrder.UseVisualStyleBackColor = False
    '
    'btnMessgGroupReadonly
    '
    Me.btnMessgGroupReadonly.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnMessgGroupReadonly.Location = New System.Drawing.Point(1, 366)
    Me.btnMessgGroupReadonly.Name = "btnMessgGroupReadonly"
    Me.btnMessgGroupReadonly.Size = New System.Drawing.Size(358, 22)
    Me.btnMessgGroupReadonly.TabIndex = 7
    Me.btnMessgGroupReadonly.Text = "403 386 14 139 138"
    Me.btnMessgGroupReadonly.UseVisualStyleBackColor = True
    '
    'btnMessgGroupDontShow
    '
    Me.btnMessgGroupDontShow.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnMessgGroupDontShow.Location = New System.Drawing.Point(1, 340)
    Me.btnMessgGroupDontShow.Name = "btnMessgGroupDontShow"
    Me.btnMessgGroupDontShow.Size = New System.Drawing.Size(358, 22)
    Me.btnMessgGroupDontShow.TabIndex = 6
    Me.btnMessgGroupDontShow.Text = "403 386 14 142"
    Me.btnMessgGroupDontShow.UseVisualStyleBackColor = True
    '
    'chkWithID
    '
    Me.chkWithID.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkWithID.Location = New System.Drawing.Point(12, 32)
    Me.chkWithID.Name = "chkWithID"
    Me.chkWithID.Size = New System.Drawing.Size(138, 16)
    Me.chkWithID.TabIndex = 5
    Me.chkWithID.Text = "148"
    Me.chkWithID.UseVisualStyleBackColor = False
    '
    'btnMessgMisch
    '
    Me.btnMessgMisch.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnMessgMisch.Location = New System.Drawing.Point(1, 312)
    Me.btnMessgMisch.Name = "btnMessgMisch"
    Me.btnMessgMisch.Size = New System.Drawing.Size(358, 22)
    Me.btnMessgMisch.TabIndex = 4
    Me.btnMessgMisch.Text = "403 406"
    Me.btnMessgMisch.UseVisualStyleBackColor = True
    '
    'btnMischMessg
    '
    Me.btnMischMessg.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnMischMessg.Location = New System.Drawing.Point(1, 284)
    Me.btnMischMessg.Name = "btnMischMessg"
    Me.btnMischMessg.Size = New System.Drawing.Size(358, 22)
    Me.btnMischMessg.TabIndex = 3
    Me.btnMischMessg.Text = "406 403"
    Me.btnMischMessg.UseVisualStyleBackColor = True
    '
    'btnUserMisch
    '
    Me.btnUserMisch.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnUserMisch.Location = New System.Drawing.Point(1, 208)
    Me.btnUserMisch.Name = "btnUserMisch"
    Me.btnUserMisch.Size = New System.Drawing.Size(358, 22)
    Me.btnUserMisch.TabIndex = 2
    Me.btnUserMisch.Text = "402 406"
    Me.btnUserMisch.UseVisualStyleBackColor = True
    '
    'btnMessgMasterSlave
    '
    Me.btnMessgMasterSlave.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnMessgMasterSlave.Location = New System.Drawing.Point(1, 77)
    Me.btnMessgMasterSlave.Name = "btnMessgMasterSlave"
    Me.btnMessgMasterSlave.Size = New System.Drawing.Size(358, 22)
    Me.btnMessgMasterSlave.TabIndex = 1
    Me.btnMessgMasterSlave.Text = "403"
    Me.btnMessgMasterSlave.UseVisualStyleBackColor = True
    '
    'btnUserMethMessgWinkel
    '
    Me.btnUserMethMessgWinkel.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnUserMethMessgWinkel.Location = New System.Drawing.Point(1, 105)
    Me.btnUserMethMessgWinkel.Name = "btnUserMethMessgWinkel"
    Me.btnUserMethMessgWinkel.Size = New System.Drawing.Size(358, 22)
    Me.btnUserMethMessgWinkel.TabIndex = 0
    Me.btnUserMethMessgWinkel.Text = "402 405 403 173"
    Me.btnUserMethMessgWinkel.UseVisualStyleBackColor = True
    '
    'TreeColorStruct
    '
    Me.TreeColorStruct.Dock = System.Windows.Forms.DockStyle.Fill
    Me.TreeColorStruct.Location = New System.Drawing.Point(0, 0)
    Me.TreeColorStruct.Name = "TreeColorStruct"
    Me.TreeColorStruct.Size = New System.Drawing.Size(782, 532)
    Me.TreeColorStruct.TabIndex = 0
    '
    'splColorDataBase
    '
    Me.splColorDataBase.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splColorDataBase.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.splColorDataBase.Location = New System.Drawing.Point(0, 0)
    Me.splColorDataBase.Name = "splColorDataBase"
    '
    'splColorDataBase.Panel1
    '
    Me.splColorDataBase.Panel1.Controls.Add(Me.btnMischSelect)
    Me.splColorDataBase.Panel1.Controls.Add(Me.btnMessgSelect)
    Me.splColorDataBase.Panel1.Controls.Add(Me.lblDBCopy)
    Me.splColorDataBase.Panel1.Controls.Add(Me.btnRezepte)
    Me.splColorDataBase.Panel1.Controls.Add(Me.btnRwerte)
    Me.splColorDataBase.Panel1.Controls.Add(Me.btnMisch)
    Me.splColorDataBase.Panel1.Controls.Add(Me.btnMessg)
    Me.splColorDataBase.Panel1.Controls.Add(Me.btnVoid)
    Me.splColorDataBase.Panel1.Controls.Add(Me.lstMisch)
    Me.splColorDataBase.Panel1.Controls.Add(Me.lstMessg)
    '
    'splColorDataBase.Panel2
    '
    Me.splColorDataBase.Panel2.Controls.Add(Me.txtMessgKE)
    Me.splColorDataBase.Panel2.Controls.Add(Me.lblMessgKE)
    Me.splColorDataBase.Panel2.Controls.Add(Me.dbgMessgeräte)
    Me.splColorDataBase.Panel2.Controls.Add(Me.dbgMeasure00x)
    Me.splColorDataBase.Panel2.Controls.Add(Me.lblShowMeasDevices)
    Me.splColorDataBase.Size = New System.Drawing.Size(1147, 532)
    Me.splColorDataBase.SplitterDistance = 454
    Me.splColorDataBase.TabIndex = 1
    '
    'btnMischSelect
    '
    Me.btnMischSelect.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMischSelect.Enabled = False
    Me.btnMischSelect.Location = New System.Drawing.Point(234, 84)
    Me.btnMischSelect.Name = "btnMischSelect"
    Me.btnMischSelect.Size = New System.Drawing.Size(211, 24)
    Me.btnMischSelect.TabIndex = 9
    Me.btnMischSelect.Text = "406 3670"
    Me.btnMischSelect.UseVisualStyleBackColor = True
    '
    'btnMessgSelect
    '
    Me.btnMessgSelect.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMessgSelect.Enabled = False
    Me.btnMessgSelect.Location = New System.Drawing.Point(11, 85)
    Me.btnMessgSelect.Name = "btnMessgSelect"
    Me.btnMessgSelect.Size = New System.Drawing.Size(211, 24)
    Me.btnMessgSelect.TabIndex = 8
    Me.btnMessgSelect.Text = "403 3670"
    Me.btnMessgSelect.UseVisualStyleBackColor = True
    '
    'lblDBCopy
    '
    Me.lblDBCopy.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblDBCopy.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblDBCopy.Location = New System.Drawing.Point(28, 6)
    Me.lblDBCopy.Name = "lblDBCopy"
    Me.lblDBCopy.Size = New System.Drawing.Size(406, 18)
    Me.lblDBCopy.TabIndex = 7
    Me.lblDBCopy.Text = "389 141 144"
    Me.lblDBCopy.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'btnRezepte
    '
    Me.btnRezepte.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnRezepte.Enabled = False
    Me.btnRezepte.Location = New System.Drawing.Point(238, 502)
    Me.btnRezepte.Name = "btnRezepte"
    Me.btnRezepte.Size = New System.Drawing.Size(211, 24)
    Me.btnRezepte.TabIndex = 6
    Me.btnRezepte.Text = "730 143"
    Me.btnRezepte.UseVisualStyleBackColor = True
    '
    'btnRwerte
    '
    Me.btnRwerte.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnRwerte.Enabled = False
    Me.btnRwerte.Location = New System.Drawing.Point(11, 502)
    Me.btnRwerte.Name = "btnRwerte"
    Me.btnRwerte.Size = New System.Drawing.Size(211, 24)
    Me.btnRwerte.TabIndex = 5
    Me.btnRwerte.Text = "3110 143"
    Me.btnRwerte.UseVisualStyleBackColor = True
    '
    'btnMisch
    '
    Me.btnMisch.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMisch.Enabled = False
    Me.btnMisch.Location = New System.Drawing.Point(234, 57)
    Me.btnMisch.Name = "btnMisch"
    Me.btnMisch.Size = New System.Drawing.Size(211, 24)
    Me.btnMisch.TabIndex = 4
    Me.btnMisch.Text = "406"
    Me.btnMisch.UseVisualStyleBackColor = True
    '
    'btnMessg
    '
    Me.btnMessg.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnMessg.Enabled = False
    Me.btnMessg.Location = New System.Drawing.Point(11, 55)
    Me.btnMessg.Name = "btnMessg"
    Me.btnMessg.Size = New System.Drawing.Size(211, 24)
    Me.btnMessg.TabIndex = 3
    Me.btnMessg.Text = "403"
    Me.btnMessg.UseVisualStyleBackColor = True
    '
    'btnVoid
    '
    Me.btnVoid.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnVoid.Location = New System.Drawing.Point(11, 27)
    Me.btnVoid.Name = "btnVoid"
    Me.btnVoid.Size = New System.Drawing.Size(211, 24)
    Me.btnVoid.TabIndex = 2
    Me.btnVoid.Text = "144"
    Me.btnVoid.UseVisualStyleBackColor = True
    '
    'lstMisch
    '
    Me.lstMisch.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstMisch.FormattingEnabled = True
    Me.lstMisch.Location = New System.Drawing.Point(238, 114)
    Me.lstMisch.Name = "lstMisch"
    Me.lstMisch.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended
    Me.lstMisch.Size = New System.Drawing.Size(207, 368)
    Me.lstMisch.TabIndex = 1
    '
    'lstMessg
    '
    Me.lstMessg.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstMessg.FormattingEnabled = True
    Me.lstMessg.Location = New System.Drawing.Point(11, 114)
    Me.lstMessg.Name = "lstMessg"
    Me.lstMessg.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended
    Me.lstMessg.Size = New System.Drawing.Size(211, 368)
    Me.lstMessg.TabIndex = 0
    '
    'txtMessgKE
    '
    Me.txtMessgKE.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
    Me.txtMessgKE.Location = New System.Drawing.Point(162, 270)
    Me.txtMessgKE.Name = "txtMessgKE"
    Me.txtMessgKE.Size = New System.Drawing.Size(34, 20)
    Me.txtMessgKE.TabIndex = 12
    '
    'lblMessgKE
    '
    Me.lblMessgKE.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
    Me.lblMessgKE.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMessgKE.Location = New System.Drawing.Point(36, 272)
    Me.lblMessgKE.Name = "lblMessgKE"
    Me.lblMessgKE.Size = New System.Drawing.Size(130, 18)
    Me.lblMessgKE.TabIndex = 11
    Me.lblMessgKE.Text = "MESSGKE"
    Me.lblMessgKE.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'dbgMessgeräte
    '
    Me.dbgMessgeräte.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.dbgMessgeräte.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgMessgeräte.Location = New System.Drawing.Point(6, 30)
    Me.dbgMessgeräte.Name = "dbgMessgeräte"
    Me.dbgMessgeräte.Size = New System.Drawing.Size(680, 239)
    Me.dbgMessgeräte.TabIndex = 10
    '
    'dbgMeasure00x
    '
    Me.dbgMeasure00x.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.dbgMeasure00x.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    Me.dbgMeasure00x.Location = New System.Drawing.Point(6, 296)
    Me.dbgMeasure00x.Name = "dbgMeasure00x"
    Me.dbgMeasure00x.Size = New System.Drawing.Size(680, 215)
    Me.dbgMeasure00x.TabIndex = 9
    '
    'lblShowMeasDevices
    '
    Me.lblShowMeasDevices.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblShowMeasDevices.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblShowMeasDevices.Location = New System.Drawing.Point(3, 9)
    Me.lblShowMeasDevices.Name = "lblShowMeasDevices"
    Me.lblShowMeasDevices.Size = New System.Drawing.Size(683, 18)
    Me.lblShowMeasDevices.TabIndex = 8
    Me.lblShowMeasDevices.Text = "403 142"
    Me.lblShowMeasDevices.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'splBasis
    '
    Me.splBasis.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splBasis.FixedPanel = System.Windows.Forms.FixedPanel.Panel2
    Me.splBasis.Location = New System.Drawing.Point(0, 0)
    Me.splBasis.Name = "splBasis"
    Me.splBasis.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splBasis.Panel1
    '
    Me.splBasis.Panel1.Controls.Add(Me.splColorDataBase)
    Me.splBasis.Panel1.Controls.Add(Me.splColorTreeStruct)
    '
    'splBasis.Panel2
    '
    Me.splBasis.Panel2.Controls.Add(Me.btnDataBase)
    Me.splBasis.Panel2.Controls.Add(Me.btnTreeView)
    Me.splBasis.Size = New System.Drawing.Size(1147, 565)
    Me.splBasis.SplitterDistance = 532
    Me.splBasis.TabIndex = 2
    '
    'btnDataBase
    '
    Me.btnDataBase.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnDataBase.Location = New System.Drawing.Point(667, 3)
    Me.btnDataBase.Name = "btnDataBase"
    Me.btnDataBase.Size = New System.Drawing.Size(165, 23)
    Me.btnDataBase.TabIndex = 1
    Me.btnDataBase.Text = "146"
    Me.btnDataBase.UseVisualStyleBackColor = True
    '
    'btnTreeView
    '
    Me.btnTreeView.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnTreeView.Location = New System.Drawing.Point(192, 3)
    Me.btnTreeView.Name = "btnTreeView"
    Me.btnTreeView.Size = New System.Drawing.Size(165, 23)
    Me.btnTreeView.TabIndex = 0
    Me.btnTreeView.Text = "147"
    Me.btnTreeView.UseVisualStyleBackColor = True
    '
    'frmColorDBStructure
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(1147, 565)
    Me.Controls.Add(Me.splBasis)
    Me.Name = "frmColorDBStructure"
    Me.Text = "frmColorDBStructure"
    Me.splColorTreeStruct.Panel1.ResumeLayout(False)
    Me.splColorTreeStruct.Panel2.ResumeLayout(False)
    CType(Me.splColorTreeStruct, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splColorTreeStruct.ResumeLayout(False)
    Me.splColorDataBase.Panel1.ResumeLayout(False)
    Me.splColorDataBase.Panel2.ResumeLayout(False)
    Me.splColorDataBase.Panel2.PerformLayout()
    CType(Me.splColorDataBase, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splColorDataBase.ResumeLayout(False)
    CType(Me.dbgMessgeräte, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.dbgMeasure00x, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splBasis.Panel1.ResumeLayout(False)
    Me.splBasis.Panel2.ResumeLayout(False)
    CType(Me.splBasis, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splBasis.ResumeLayout(False)
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents splColorTreeStruct As System.Windows.Forms.SplitContainer
  Friend WithEvents btnUserMethMessgWinkel As System.Windows.Forms.Button
  Friend WithEvents TreeColorStruct As System.Windows.Forms.TreeView
  Friend WithEvents btnMessgMasterSlave As System.Windows.Forms.Button
  Friend WithEvents btnUserMisch As System.Windows.Forms.Button
  Friend WithEvents btnMischMessg As System.Windows.Forms.Button
  Friend WithEvents btnMessgMisch As System.Windows.Forms.Button
  Friend WithEvents chkWithID As System.Windows.Forms.CheckBox
  Friend WithEvents btnMessgGroupReadonly As System.Windows.Forms.Button
  Friend WithEvents btnMessgGroupDontShow As System.Windows.Forms.Button
  Friend WithEvents chkOrder As System.Windows.Forms.CheckBox
  Friend WithEvents btnMessgWinkel As System.Windows.Forms.Button
  Friend WithEvents btnMischGroupReadonly As System.Windows.Forms.Button
  Friend WithEvents btnMischGroupDontShow As System.Windows.Forms.Button
  Friend WithEvents btnUserMethLicht As System.Windows.Forms.Button
  Friend WithEvents btnUserMethAnwsgMerk As System.Windows.Forms.Button
  Friend WithEvents btnUserMessg As System.Windows.Forms.Button
  Friend WithEvents splColorDataBase As System.Windows.Forms.SplitContainer
  Friend WithEvents btnUserMischMessgGKWerte As System.Windows.Forms.Button
  Friend WithEvents btnUserMethodeMessgerätGkwert As System.Windows.Forms.Button
  Friend WithEvents splBasis As System.Windows.Forms.SplitContainer
  Friend WithEvents btnDataBase As System.Windows.Forms.Button
  Friend WithEvents btnTreeView As System.Windows.Forms.Button
  Friend WithEvents btnMessg As System.Windows.Forms.Button
  Friend WithEvents btnVoid As System.Windows.Forms.Button
  Friend WithEvents lstMisch As System.Windows.Forms.ListBox
  Friend WithEvents lstMessg As System.Windows.Forms.ListBox
  Friend WithEvents btnMisch As System.Windows.Forms.Button
  Friend WithEvents btnRezepte As System.Windows.Forms.Button
  Friend WithEvents btnRwerte As System.Windows.Forms.Button
  Friend WithEvents lblDBCopy As System.Windows.Forms.Label
  Friend WithEvents lblTreeView As System.Windows.Forms.Label
  Friend WithEvents btnMischSelect As System.Windows.Forms.Button
  Friend WithEvents btnMessgSelect As System.Windows.Forms.Button
  Friend WithEvents txtMessgKE As System.Windows.Forms.TextBox
  Friend WithEvents lblMessgKE As System.Windows.Forms.Label
  Friend WithEvents dbgMessgeräte As System.Windows.Forms.DataGridView
  Friend WithEvents dbgMeasure00x As System.Windows.Forms.DataGridView
  Friend WithEvents lblShowMeasDevices As System.Windows.Forms.Label
End Class
