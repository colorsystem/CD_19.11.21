<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmPAR
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
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmPAR))
    Dim Style1 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style2 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style3 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style4 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style5 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style6 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style7 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Dim Style8 As C1.Win.C1TrueDBGrid.Style = New C1.Win.C1TrueDBGrid.Style()
    Me.btnORD = New System.Windows.Forms.Button()
    Me.BindingMET = New System.Windows.Forms.BindingNavigator(Me.components)
    Me.ToolStripButton5 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton6 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton7 = New System.Windows.Forms.ToolStripButton()
    Me.ToolStripButton8 = New System.Windows.Forms.ToolStripButton()
    Me.BindingUSE = New System.Windows.Forms.BindingNavigator(Me.components)
    Me.BindingNavigatorMoveFirstItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMovePreviousItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMoveNextItem = New System.Windows.Forms.ToolStripButton()
    Me.BindingNavigatorMoveLastItem = New System.Windows.Forms.ToolStripButton()
    Me.lblMET = New System.Windows.Forms.Label()
    Me.lblMTH = New System.Windows.Forms.Label()
    Me.lblUSE = New System.Windows.Forms.Label()
    Me.lblPAR = New System.Windows.Forms.Label()
    Me.flgParameter = New C1.Win.C1TrueDBGrid.C1TrueDBGrid()
    Me.flgDropParameter = New C1.Win.C1TrueDBGrid.C1TrueDBDropdown()
    CType(Me.BindingMET, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.BindingMET.SuspendLayout()
    CType(Me.BindingUSE, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.BindingUSE.SuspendLayout()
    CType(Me.flgParameter, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.flgDropParameter, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'btnORD
    '
    Me.btnORD.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnORD.Location = New System.Drawing.Point(673, 36)
    Me.btnORD.Name = "btnORD"
    Me.btnORD.Size = New System.Drawing.Size(148, 26)
    Me.btnORD.TabIndex = 27
    Me.btnORD.Text = "1999"
    Me.btnORD.UseVisualStyleBackColor = True
    '
    'BindingMET
    '
    Me.BindingMET.AddNewItem = Nothing
    Me.BindingMET.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.BindingMET.CountItem = Nothing
    Me.BindingMET.DeleteItem = Nothing
    Me.BindingMET.Dock = System.Windows.Forms.DockStyle.None
    Me.BindingMET.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton5, Me.ToolStripButton6, Me.ToolStripButton7, Me.ToolStripButton8})
    Me.BindingMET.Location = New System.Drawing.Point(369, 102)
    Me.BindingMET.MoveFirstItem = Me.ToolStripButton5
    Me.BindingMET.MoveLastItem = Me.ToolStripButton8
    Me.BindingMET.MoveNextItem = Me.ToolStripButton7
    Me.BindingMET.MovePreviousItem = Me.ToolStripButton6
    Me.BindingMET.Name = "BindingMET"
    Me.BindingMET.PositionItem = Nothing
    Me.BindingMET.Size = New System.Drawing.Size(104, 25)
    Me.BindingMET.TabIndex = 26
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
    'BindingUSE
    '
    Me.BindingUSE.AddNewItem = Nothing
    Me.BindingUSE.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.BindingUSE.CountItem = Nothing
    Me.BindingUSE.DeleteItem = Nothing
    Me.BindingUSE.Dock = System.Windows.Forms.DockStyle.None
    Me.BindingUSE.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BindingNavigatorMoveFirstItem, Me.BindingNavigatorMovePreviousItem, Me.BindingNavigatorMoveNextItem, Me.BindingNavigatorMoveLastItem})
    Me.BindingUSE.Location = New System.Drawing.Point(369, 56)
    Me.BindingUSE.MoveFirstItem = Me.BindingNavigatorMoveFirstItem
    Me.BindingUSE.MoveLastItem = Me.BindingNavigatorMoveLastItem
    Me.BindingUSE.MoveNextItem = Me.BindingNavigatorMoveNextItem
    Me.BindingUSE.MovePreviousItem = Me.BindingNavigatorMovePreviousItem
    Me.BindingUSE.Name = "BindingUSE"
    Me.BindingUSE.PositionItem = Nothing
    Me.BindingUSE.Size = New System.Drawing.Size(104, 25)
    Me.BindingUSE.TabIndex = 25
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
    'lblMET
    '
    Me.lblMET.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMET.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMET.Location = New System.Drawing.Point(286, 82)
    Me.lblMET.Name = "lblMET"
    Me.lblMET.Size = New System.Drawing.Size(336, 20)
    Me.lblMET.TabIndex = 24
    Me.lblMET.Text = "999"
    Me.lblMET.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    '
    'lblMTH
    '
    Me.lblMTH.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMTH.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblMTH.Location = New System.Drawing.Point(70, 82)
    Me.lblMTH.Name = "lblMTH"
    Me.lblMTH.Size = New System.Drawing.Size(215, 20)
    Me.lblMTH.TabIndex = 23
    Me.lblMTH.Text = "421"
    Me.lblMTH.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblUSE
    '
    Me.lblUSE.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblUSE.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblUSE.Location = New System.Drawing.Point(286, 36)
    Me.lblUSE.Name = "lblUSE"
    Me.lblUSE.Size = New System.Drawing.Size(336, 20)
    Me.lblUSE.TabIndex = 22
    Me.lblUSE.Text = "999"
    Me.lblUSE.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
    '
    'lblPAR
    '
    Me.lblPAR.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblPAR.BackColor = System.Drawing.SystemColors.ControlLight
    Me.lblPAR.Location = New System.Drawing.Point(72, 36)
    Me.lblPAR.Name = "lblPAR"
    Me.lblPAR.Size = New System.Drawing.Size(215, 20)
    Me.lblPAR.TabIndex = 21
    Me.lblPAR.Text = "201"
    Me.lblPAR.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'flgParameter
    '
    Me.flgParameter.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.flgParameter.Images.Add(CType(resources.GetObject("flgParameter.Images"), System.Drawing.Image))
    Me.flgParameter.Location = New System.Drawing.Point(40, 178)
    Me.flgParameter.Name = "flgParameter"
    Me.flgParameter.PreviewInfo.Location = New System.Drawing.Point(0, 0)
    Me.flgParameter.PreviewInfo.Size = New System.Drawing.Size(0, 0)
    Me.flgParameter.PreviewInfo.ZoomFactor = 75.0R
    Me.flgParameter.PrintInfo.PageSettings = CType(resources.GetObject("flgParameter.PrintInfo.PageSettings"), System.Drawing.Printing.PageSettings)
    Me.flgParameter.Size = New System.Drawing.Size(781, 368)
    Me.flgParameter.TabIndex = 28
    Me.flgParameter.Text = "C1TrueDBGrid1"
    Me.flgParameter.PropBag = resources.GetString("flgParameter.PropBag")
    '
    'flgDropParameter
    '
    Me.flgDropParameter.AllowColMove = True
    Me.flgDropParameter.AllowColSelect = True
    Me.flgDropParameter.AllowRowSizing = C1.Win.C1TrueDBGrid.RowSizingEnum.AllRows
    Me.flgDropParameter.AlternatingRows = False
    Me.flgDropParameter.CaptionStyle = Style1
    Me.flgDropParameter.ColumnCaptionHeight = 17
    Me.flgDropParameter.ColumnFooterHeight = 17
    Me.flgDropParameter.EvenRowStyle = Style2
    Me.flgDropParameter.FetchRowStyles = False
    Me.flgDropParameter.FooterStyle = Style3
    Me.flgDropParameter.HeadingStyle = Style4
    Me.flgDropParameter.HighLightRowStyle = Style5
    Me.flgDropParameter.Images.Add(CType(resources.GetObject("flgDropParameter.Images"), System.Drawing.Image))
    Me.flgDropParameter.Location = New System.Drawing.Point(514, 393)
    Me.flgDropParameter.Name = "flgDropParameter"
    Me.flgDropParameter.OddRowStyle = Style6
    Me.flgDropParameter.RecordSelectorStyle = Style7
    Me.flgDropParameter.RowDivider.Color = System.Drawing.Color.DarkGray
    Me.flgDropParameter.RowDivider.Style = C1.Win.C1TrueDBGrid.LineStyleEnum.[Single]
    Me.flgDropParameter.RowSubDividerColor = System.Drawing.Color.DarkGray
    Me.flgDropParameter.ScrollTips = False
    Me.flgDropParameter.Size = New System.Drawing.Size(159, 26)
    Me.flgDropParameter.Style = Style8
    Me.flgDropParameter.TabIndex = 29
    Me.flgDropParameter.Text = "C1TrueDBDropdown1"
    Me.flgDropParameter.Visible = False
    Me.flgDropParameter.PropBag = resources.GetString("flgDropParameter.PropBag")
    '
    'frmPAR
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.BackColor = System.Drawing.Color.Silver
    Me.ClientSize = New System.Drawing.Size(884, 662)
    Me.ControlBox = False
    Me.Controls.Add(Me.flgDropParameter)
    Me.Controls.Add(Me.flgParameter)
    Me.Controls.Add(Me.btnORD)
    Me.Controls.Add(Me.BindingMET)
    Me.Controls.Add(Me.BindingUSE)
    Me.Controls.Add(Me.lblMET)
    Me.Controls.Add(Me.lblMTH)
    Me.Controls.Add(Me.lblUSE)
    Me.Controls.Add(Me.lblPAR)
    Me.Name = "frmPAR"
    Me.Text = "415"
    CType(Me.BindingMET, System.ComponentModel.ISupportInitialize).EndInit()
    Me.BindingMET.ResumeLayout(False)
    Me.BindingMET.PerformLayout()
    CType(Me.BindingUSE, System.ComponentModel.ISupportInitialize).EndInit()
    Me.BindingUSE.ResumeLayout(False)
    Me.BindingUSE.PerformLayout()
    CType(Me.flgParameter, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.flgDropParameter, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)
    Me.PerformLayout()

  End Sub
  Friend WithEvents btnORD As System.Windows.Forms.Button
  Friend WithEvents BindingMET As System.Windows.Forms.BindingNavigator
  Friend WithEvents ToolStripButton5 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton6 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton7 As System.Windows.Forms.ToolStripButton
  Friend WithEvents ToolStripButton8 As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingUSE As System.Windows.Forms.BindingNavigator
  Friend WithEvents BindingNavigatorMoveFirstItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMovePreviousItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMoveNextItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents BindingNavigatorMoveLastItem As System.Windows.Forms.ToolStripButton
  Friend WithEvents lblMET As System.Windows.Forms.Label
  Friend WithEvents lblMTH As System.Windows.Forms.Label
  Friend WithEvents lblUSE As System.Windows.Forms.Label
  Friend WithEvents lblPAR As System.Windows.Forms.Label
  Friend WithEvents flgParameter As C1.Win.C1TrueDBGrid.C1TrueDBGrid
  Friend WithEvents flgDropParameter As C1.Win.C1TrueDBGrid.C1TrueDBDropdown
End Class
