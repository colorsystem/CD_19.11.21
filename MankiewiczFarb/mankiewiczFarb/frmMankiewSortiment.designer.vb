<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Public Class frmMankiewSortiment
  Inherits System.Windows.Forms.Form

  'Form overrides dispose to clean up the component list.
  <System.Diagnostics.DebuggerNonUserCode()> _
  Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
    If disposing AndAlso components IsNot Nothing Then
      components.Dispose()
    End If
    MyBase.Dispose(disposing)
  End Sub

  'Required by the Windows Form Designer
  Private components As System.ComponentModel.IContainer

  'NOTE: The following procedure is required by the Windows Form Designer
  'It can be modified using the Windows Form Designer.  
  'Do not modify it using the code editor.
  <System.Diagnostics.DebuggerStepThrough()> _
  Private Sub InitializeComponent()
    Me.SplitContainSortiment = New System.Windows.Forms.SplitContainer()
    Me.SplitContainFarbmittel = New System.Windows.Forms.SplitContainer()
    Me.flgFarbmittel = New System.Windows.Forms.DataGridView()
    Me.FARBM_ID = New System.Windows.Forms.DataGridViewTextBoxColumn()
    Me.FARBM_NAME = New System.Windows.Forms.DataGridViewTextBoxColumn()
    Me.FARBM_MENGE = New System.Windows.Forms.DataGridViewTextBoxColumn()
    Me.FARBM_PROZ = New System.Windows.Forms.DataGridViewTextBoxColumn()
    Me.FARBM_PROB = New System.Windows.Forms.DataGridViewTextBoxColumn()
    Me.FARBM_PREIS = New System.Windows.Forms.DataGridViewTextBoxColumn()
    Me.FARBM_TOPF = New System.Windows.Forms.DataGridViewComboBoxColumn()
    Me.FARBM_OPERAT = New System.Windows.Forms.DataGridViewComboBoxColumn()
    Me.FARBM_LIMMNG = New System.Windows.Forms.DataGridViewTextBoxColumn()
    Me.FARBM_ICHF = New System.Windows.Forms.DataGridViewTextBoxColumn()
    Me.lstFarbmittel = New System.Windows.Forms.ListBox()
    Me.btnChangesUpdate = New System.Windows.Forms.Button()
    CType(Me.SplitContainSortiment, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContainSortiment.Panel1.SuspendLayout()
    Me.SplitContainSortiment.Panel2.SuspendLayout()
    Me.SplitContainSortiment.SuspendLayout()
    CType(Me.SplitContainFarbmittel, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContainFarbmittel.Panel1.SuspendLayout()
    Me.SplitContainFarbmittel.Panel2.SuspendLayout()
    Me.SplitContainFarbmittel.SuspendLayout()
    CType(Me.flgFarbmittel, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'SplitContainSortiment
    '
    Me.SplitContainSortiment.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.SplitContainSortiment.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContainSortiment.FixedPanel = System.Windows.Forms.FixedPanel.Panel2
    Me.SplitContainSortiment.IsSplitterFixed = True
    Me.SplitContainSortiment.Location = New System.Drawing.Point(0, 0)
    Me.SplitContainSortiment.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.SplitContainSortiment.Name = "SplitContainSortiment"
    Me.SplitContainSortiment.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'SplitContainSortiment.Panel1
    '
    Me.SplitContainSortiment.Panel1.BackColor = System.Drawing.SystemColors.ControlDark
    Me.SplitContainSortiment.Panel1.Controls.Add(Me.SplitContainFarbmittel)
    '
    'SplitContainSortiment.Panel2
    '
    Me.SplitContainSortiment.Panel2.BackColor = System.Drawing.SystemColors.ButtonShadow
    Me.SplitContainSortiment.Panel2.Controls.Add(Me.btnChangesUpdate)
    Me.SplitContainSortiment.Size = New System.Drawing.Size(1200, 750)
    Me.SplitContainSortiment.SplitterDistance = 653
    Me.SplitContainSortiment.SplitterWidth = 5
    Me.SplitContainSortiment.TabIndex = 11
    '
    'SplitContainFarbmittel
    '
    Me.SplitContainFarbmittel.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContainFarbmittel.Location = New System.Drawing.Point(0, 0)
    Me.SplitContainFarbmittel.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.SplitContainFarbmittel.Name = "SplitContainFarbmittel"
    '
    'SplitContainFarbmittel.Panel1
    '
    Me.SplitContainFarbmittel.Panel1.Controls.Add(Me.flgFarbmittel)
    '
    'SplitContainFarbmittel.Panel2
    '
    Me.SplitContainFarbmittel.Panel2.Controls.Add(Me.lstFarbmittel)
    Me.SplitContainFarbmittel.Size = New System.Drawing.Size(1200, 653)
    Me.SplitContainFarbmittel.SplitterDistance = 953
    Me.SplitContainFarbmittel.SplitterWidth = 5
    Me.SplitContainFarbmittel.TabIndex = 0
    '
    'flgFarbmittel
    '
    Me.flgFarbmittel.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.FARBM_ID, Me.FARBM_NAME, Me.FARBM_MENGE, Me.FARBM_PROZ, Me.FARBM_PROB, Me.FARBM_PREIS, Me.FARBM_TOPF, Me.FARBM_OPERAT, Me.FARBM_LIMMNG, Me.FARBM_ICHF})
    Me.flgFarbmittel.Dock = System.Windows.Forms.DockStyle.Fill
    Me.flgFarbmittel.Location = New System.Drawing.Point(0, 0)
    Me.flgFarbmittel.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.flgFarbmittel.Name = "flgFarbmittel"
    Me.flgFarbmittel.Size = New System.Drawing.Size(953, 653)
    Me.flgFarbmittel.TabIndex = 12
    Me.flgFarbmittel.Text = "DataGridView1"
    '
    'FARBM_ID
    '
    Me.FARBM_ID.DataPropertyName = "FARBM_ID"
    Me.FARBM_ID.HeaderText = "FARBM_ID"
    Me.FARBM_ID.Name = "FARBM_ID"
    Me.FARBM_ID.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
    '
    'FARBM_NAME
    '
    Me.FARBM_NAME.DataPropertyName = "FARBM_NAME"
    Me.FARBM_NAME.HeaderText = "FARBM_NAME"
    Me.FARBM_NAME.Name = "FARBM_NAME"
    Me.FARBM_NAME.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
    Me.FARBM_NAME.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
    '
    'FARBM_MENGE
    '
    Me.FARBM_MENGE.DataPropertyName = "FARBM_MENGE"
    Me.FARBM_MENGE.HeaderText = "FARBM_MENGE"
    Me.FARBM_MENGE.Name = "FARBM_MENGE"
    Me.FARBM_MENGE.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
    '
    'FARBM_PROZ
    '
    Me.FARBM_PROZ.DataPropertyName = "FARBM_PROZ"
    Me.FARBM_PROZ.HeaderText = "FARBM_PROZ"
    Me.FARBM_PROZ.Name = "FARBM_PROZ"
    '
    'FARBM_PROB
    '
    Me.FARBM_PROB.DataPropertyName = "FARBM_PROB"
    Me.FARBM_PROB.HeaderText = "FARBM_PROB"
    Me.FARBM_PROB.Name = "FARBM_PROB"
    Me.FARBM_PROB.Visible = False
    '
    'FARBM_PREIS
    '
    Me.FARBM_PREIS.DataPropertyName = "FARBM_PREIS"
    Me.FARBM_PREIS.HeaderText = "FARBM_PREIS"
    Me.FARBM_PREIS.Name = "FARBM_PREIS"
    Me.FARBM_PREIS.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
    '
    'FARBM_TOPF
    '
    Me.FARBM_TOPF.DataPropertyName = "FARBM_TOPF"
    Me.FARBM_TOPF.HeaderText = "FARBM_TOPF"
    Me.FARBM_TOPF.Items.AddRange(New Object() {" ", "-", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "I", "J", "M", "N", "S", "T", "V", "W", "X", "Y", "(", ")", "[", "]", "{", "}", "<", ">", ""})
    Me.FARBM_TOPF.MaxDropDownItems = 36
    Me.FARBM_TOPF.Name = "FARBM_TOPF"
    Me.FARBM_TOPF.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
    '
    'FARBM_OPERAT
    '
    Me.FARBM_OPERAT.DataPropertyName = "FARBM_OPERAT"
    Me.FARBM_OPERAT.HeaderText = "FARBM_OPERAT"
    Me.FARBM_OPERAT.Items.AddRange(New Object() {" ", "=", "<", ">", "*", "+", "/", ":", "\", "A", "B", "C", "1", "2", "3", "X", "Y", "Z", ""})
    Me.FARBM_OPERAT.Name = "FARBM_OPERAT"
    Me.FARBM_OPERAT.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
    '
    'FARBM_LIMMNG
    '
    Me.FARBM_LIMMNG.DataPropertyName = "FARBM_LIMMNG"
    Me.FARBM_LIMMNG.HeaderText = "FARBM_LIMMNG"
    Me.FARBM_LIMMNG.Name = "FARBM_LIMMNG"
    Me.FARBM_LIMMNG.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
    '
    'FARBM_ICHF
    '
    Me.FARBM_ICHF.DataPropertyName = "FARBM_ICHF"
    Me.FARBM_ICHF.HeaderText = "FARBM_ICHF"
    Me.FARBM_ICHF.Name = "FARBM_ICHF"
    '
    'lstFarbmittel
    '
    Me.lstFarbmittel.Dock = System.Windows.Forms.DockStyle.Fill
    Me.lstFarbmittel.FormattingEnabled = True
    Me.lstFarbmittel.ItemHeight = 16
    Me.lstFarbmittel.Location = New System.Drawing.Point(0, 0)
    Me.lstFarbmittel.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.lstFarbmittel.Name = "lstFarbmittel"
    Me.lstFarbmittel.Size = New System.Drawing.Size(242, 653)
    Me.lstFarbmittel.TabIndex = 0
    '
    'btnChangesUpdate
    '
    Me.btnChangesUpdate.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnChangesUpdate.BackColor = System.Drawing.SystemColors.ButtonFace
    Me.btnChangesUpdate.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnChangesUpdate.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
    Me.btnChangesUpdate.Location = New System.Drawing.Point(495, 19)
    Me.btnChangesUpdate.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.btnChangesUpdate.Name = "btnChangesUpdate"
    Me.btnChangesUpdate.Size = New System.Drawing.Size(241, 47)
    Me.btnChangesUpdate.TabIndex = 12
    Me.btnChangesUpdate.Text = "4610"
    Me.btnChangesUpdate.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.btnChangesUpdate.UseVisualStyleBackColor = False
    '
    'frmMankiewSortiment
    '
    Me.AccessibleRole = System.Windows.Forms.AccessibleRole.None
    Me.AutoScaleDimensions = New System.Drawing.SizeF(8.0!, 16.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(192, Byte), Integer))
    Me.ClientSize = New System.Drawing.Size(1200, 750)
    Me.ControlBox = False
    Me.Controls.Add(Me.SplitContainSortiment)
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
    Me.Margin = New System.Windows.Forms.Padding(4, 4, 4, 4)
    Me.MaximizeBox = False
    Me.MinimizeBox = False
    Me.Name = "frmMankiewSortiment"
    Me.SplitContainSortiment.Panel1.ResumeLayout(False)
    Me.SplitContainSortiment.Panel2.ResumeLayout(False)
    CType(Me.SplitContainSortiment, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContainSortiment.ResumeLayout(False)
    Me.SplitContainFarbmittel.Panel1.ResumeLayout(False)
    Me.SplitContainFarbmittel.Panel2.ResumeLayout(False)
    CType(Me.SplitContainFarbmittel, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContainFarbmittel.ResumeLayout(False)
    CType(Me.flgFarbmittel, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents SplitContainSortiment As System.Windows.Forms.SplitContainer
  Friend WithEvents btnChangesUpdate As System.Windows.Forms.Button
  Friend WithEvents SplitContainFarbmittel As System.Windows.Forms.SplitContainer
  Friend WithEvents flgFarbmittel As System.Windows.Forms.DataGridView
  Friend WithEvents lstFarbmittel As System.Windows.Forms.ListBox
  Friend WithEvents FARBM_ID As System.Windows.Forms.DataGridViewTextBoxColumn
  Friend WithEvents FARBM_NAME As System.Windows.Forms.DataGridViewTextBoxColumn
  Friend WithEvents FARBM_MENGE As System.Windows.Forms.DataGridViewTextBoxColumn
  Friend WithEvents FARBM_PROZ As System.Windows.Forms.DataGridViewTextBoxColumn
  Friend WithEvents FARBM_PROB As System.Windows.Forms.DataGridViewTextBoxColumn
  Friend WithEvents FARBM_PREIS As System.Windows.Forms.DataGridViewTextBoxColumn
  Friend WithEvents FARBM_TOPF As System.Windows.Forms.DataGridViewComboBoxColumn
  Friend WithEvents FARBM_OPERAT As System.Windows.Forms.DataGridViewComboBoxColumn
  Friend WithEvents FARBM_LIMMNG As System.Windows.Forms.DataGridViewTextBoxColumn
  Friend WithEvents FARBM_ICHF As System.Windows.Forms.DataGridViewTextBoxColumn
End Class
