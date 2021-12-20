<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmTestReport
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
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmTestReport))
    Me.C1PrintPreviewControl1 = New C1.Win.C1Preview.C1PrintPreviewControl
    Me.C1PrintDocument1 = New C1.C1Preview.C1PrintDocument
    Me.C1PrintPreviewDialog1 = New C1.Win.C1Preview.C1PrintPreviewDialog
    CType(Me.C1PrintPreviewControl1, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.C1PrintPreviewControl1.PreviewPane, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.C1PrintPreviewControl1.SuspendLayout()
    CType(Me.C1PrintPreviewDialog1.PrintPreviewControl, System.ComponentModel.ISupportInitialize).BeginInit()
    CType(Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewPane, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.C1PrintPreviewDialog1.PrintPreviewControl.SuspendLayout()
    Me.C1PrintPreviewDialog1.SuspendLayout()
    Me.SuspendLayout()
    '
    'C1PrintPreviewControl1
    '
    Me.C1PrintPreviewControl1.Location = New System.Drawing.Point(12, 12)
    Me.C1PrintPreviewControl1.Name = "C1PrintPreviewControl1"
    '
    'C1PrintPreviewControl1.OutlineView
    '
    Me.C1PrintPreviewControl1.PreviewOutlineView.Dock = System.Windows.Forms.DockStyle.Fill
    Me.C1PrintPreviewControl1.PreviewOutlineView.Location = New System.Drawing.Point(0, 0)
    Me.C1PrintPreviewControl1.PreviewOutlineView.Name = "OutlineView"
    Me.C1PrintPreviewControl1.PreviewOutlineView.Size = New System.Drawing.Size(165, 427)
    Me.C1PrintPreviewControl1.PreviewOutlineView.TabIndex = 0
    '
    'C1PrintPreviewControl1.PreviewPane
    '
    Me.C1PrintPreviewControl1.PreviewPane.IntegrateExternalTools = True
    Me.C1PrintPreviewControl1.PreviewPane.TabIndex = 0
    '
    'C1PrintPreviewControl1.PreviewTextSearchPanel
    '
    Me.C1PrintPreviewControl1.PreviewTextSearchPanel.Dock = System.Windows.Forms.DockStyle.Right
    Me.C1PrintPreviewControl1.PreviewTextSearchPanel.Location = New System.Drawing.Point(530, 0)
    Me.C1PrintPreviewControl1.PreviewTextSearchPanel.MinimumSize = New System.Drawing.Size(200, 240)
    Me.C1PrintPreviewControl1.PreviewTextSearchPanel.Name = "PreviewTextSearchPanel"
    Me.C1PrintPreviewControl1.PreviewTextSearchPanel.Size = New System.Drawing.Size(200, 453)
    Me.C1PrintPreviewControl1.PreviewTextSearchPanel.TabIndex = 0
    Me.C1PrintPreviewControl1.PreviewTextSearchPanel.Visible = False
    '
    'C1PrintPreviewControl1.ThumbnailView
    '
    Me.C1PrintPreviewControl1.PreviewThumbnailView.Dock = System.Windows.Forms.DockStyle.Fill
    Me.C1PrintPreviewControl1.PreviewThumbnailView.HideSelection = False
    Me.C1PrintPreviewControl1.PreviewThumbnailView.Location = New System.Drawing.Point(0, 0)
    Me.C1PrintPreviewControl1.PreviewThumbnailView.Name = "ThumbnailView"
    Me.C1PrintPreviewControl1.PreviewThumbnailView.OwnerDraw = True
    Me.C1PrintPreviewControl1.PreviewThumbnailView.Size = New System.Drawing.Size(165, 494)
    Me.C1PrintPreviewControl1.PreviewThumbnailView.TabIndex = 0
    Me.C1PrintPreviewControl1.PreviewThumbnailView.ThumbnailSize = New System.Drawing.Size(96, 96)
    Me.C1PrintPreviewControl1.PreviewThumbnailView.UseImageAsThumbnail = False
    Me.C1PrintPreviewControl1.Size = New System.Drawing.Size(745, 567)
    Me.C1PrintPreviewControl1.TabIndex = 0
    Me.C1PrintPreviewControl1.Text = "C1PrintPreviewControl1"
    '
    'C1PrintPreviewDialog1
    '
    Me.C1PrintPreviewDialog1.ClientSize = New System.Drawing.Size(716, 543)
    Me.C1PrintPreviewDialog1.Name = "C1PrintPreviewDialog"
    '
    'C1PrintPreviewDialog1.PrintPreviewControl
    '
    '
    'C1PrintPreviewDialog1.PrintPreviewControl.OutlineView
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewOutlineView.Dock = System.Windows.Forms.DockStyle.Fill
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewOutlineView.LineColor = System.Drawing.Color.Empty
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewOutlineView.Location = New System.Drawing.Point(0, 0)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewOutlineView.Name = "OutlineView"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewOutlineView.Size = New System.Drawing.Size(162, 527)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewOutlineView.TabIndex = 0
    '
    'C1PrintPreviewDialog1.PrintPreviewControl.PreviewPane
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewPane.IntegrateExternalTools = True
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewPane.TabIndex = 0
    '
    'C1PrintPreviewDialog1.PrintPreviewControl.PreviewTextSearchPanel
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewTextSearchPanel.Dock = System.Windows.Forms.DockStyle.Right
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewTextSearchPanel.Location = New System.Drawing.Point(516, 0)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewTextSearchPanel.MinimumSize = New System.Drawing.Size(200, 240)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewTextSearchPanel.Name = "PreviewTextSearchPanel"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewTextSearchPanel.Size = New System.Drawing.Size(200, 496)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewTextSearchPanel.TabIndex = 0
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewTextSearchPanel.Visible = False
    '
    'C1PrintPreviewDialog1.PrintPreviewControl.ThumbnailView
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewThumbnailView.Dock = System.Windows.Forms.DockStyle.Fill
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewThumbnailView.HideSelection = False
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewThumbnailView.Location = New System.Drawing.Point(0, 0)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewThumbnailView.Name = "ThumbnailView"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewThumbnailView.OwnerDraw = True
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewThumbnailView.Size = New System.Drawing.Size(165, 464)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewThumbnailView.TabIndex = 0
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewThumbnailView.ThumbnailSize = New System.Drawing.Size(96, 96)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewThumbnailView.UseImageAsThumbnail = False
    Me.C1PrintPreviewDialog1.PrintPreviewControl.Text = "c1PrintPreviewControl1"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Open.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Open.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Open.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Open.Name = "btnFileOpen"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Open.Size = New System.Drawing.Size(32, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Open.Tag = "C1PreviewActionEnum.FileOpen"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Open.ToolTipText = "Open File"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.PageSetup.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.PageSetup.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.PageSetup.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.PageSetup.Name = "btnPageSetup"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.PageSetup.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.PageSetup.Tag = "C1PreviewActionEnum.PageSetup"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.PageSetup.ToolTipText = "Page Setup"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Print.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Print.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Print.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Print.Name = "btnPrint"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Print.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Print.Tag = "C1PreviewActionEnum.Print"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Print.ToolTipText = "Print"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Reflow.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Reflow.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Reflow.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Reflow.Name = "btnReflow"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Reflow.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Reflow.Tag = "C1PreviewActionEnum.Reflow"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Reflow.ToolTipText = "Reflow"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Save.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Save.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Save.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Save.Name = "btnFileSave"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Save.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Save.Tag = "C1PreviewActionEnum.FileSave"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.File.Save.ToolTipText = "Save File"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoFirst.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoFirst.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoFirst.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoFirst.Name = "btnGoFirst"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoFirst.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoFirst.Tag = "C1PreviewActionEnum.GoFirst"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoFirst.ToolTipText = "Go To First Page"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoLast.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoLast.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoLast.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoLast.Name = "btnGoLast"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoLast.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoLast.Tag = "C1PreviewActionEnum.GoLast"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoLast.ToolTipText = "Go To Last Page"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoNext.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoNext.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoNext.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoNext.Name = "btnGoNext"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoNext.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoNext.Tag = "C1PreviewActionEnum.GoNext"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoNext.ToolTipText = "Go To Next Page"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoPrev.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoPrev.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoPrev.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoPrev.Name = "btnGoPrev"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoPrev.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoPrev.Tag = "C1PreviewActionEnum.GoPrev"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.GoPrev.ToolTipText = "Go To Previous Page"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryNext.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryNext.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryNext.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryNext.Name = "btnHistoryNext"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryNext.Size = New System.Drawing.Size(32, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryNext.Tag = "C1PreviewActionEnum.HistoryNext"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryNext.ToolTipText = "Next View"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryPrev.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryPrev.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryPrev.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryPrev.Name = "btnHistoryPrev"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryPrev.Size = New System.Drawing.Size(32, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryPrev.Tag = "C1PreviewActionEnum.HistoryPrev"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.HistoryPrev.ToolTipText = "Previous View"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.LblOfPages.Name = "lblOfPages"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.LblOfPages.Size = New System.Drawing.Size(27, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.LblOfPages.Tag = "C1PreviewActionEnum.GoPageCount"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.LblOfPages.Text = "of 0"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.LblPage.Name = "lblPage"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.LblPage.Size = New System.Drawing.Size(33, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.LblPage.Tag = "C1PreviewActionEnum.GoPageLabel"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.LblPage.Text = "Page"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.PageNo.Name = "txtPageNo"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.PageNo.Size = New System.Drawing.Size(34, 25)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.PageNo.Tag = "C1PreviewActionEnum.GoPageNumber"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.PageNo.Text = "1"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Navigation.ToolTipPageNo = Nothing
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Continuous.Checked = True
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Continuous.CheckState = System.Windows.Forms.CheckState.Checked
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Continuous.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Continuous.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Continuous.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Continuous.Name = "btnPageContinuous"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Continuous.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Continuous.Tag = "C1PreviewActionEnum.PageContinuous"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Continuous.ToolTipText = "Continuous View"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Facing.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Facing.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Facing.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Facing.Name = "btnPageFacing"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Facing.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Facing.Tag = "C1PreviewActionEnum.PageFacing"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Facing.ToolTipText = "Pages Facing View"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.FacingContinuous.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.FacingContinuous.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.FacingContinuous.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.FacingContinuous.Name = "btnPageFacingContinuous"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.FacingContinuous.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.FacingContinuous.Tag = "C1PreviewActionEnum.PageFacingContinuous"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.FacingContinuous.ToolTipText = "Pages Facing Continuous View"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Single.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Single.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Single.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Single.Name = "btnPageSingle"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Single.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Single.Tag = "C1PreviewActionEnum.PageSingle"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Page.Single.ToolTipText = "Single Page View"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Find.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Find.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Find.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Find.Name = "btnFind"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Find.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Find.Tag = "C1PreviewActionEnum.Find"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Find.ToolTipText = "Find Text"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Hand.Checked = True
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Hand.CheckState = System.Windows.Forms.CheckState.Checked
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Hand.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Hand.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Hand.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Hand.Name = "btnHandTool"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Hand.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Hand.Tag = "C1PreviewActionEnum.HandTool"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.Hand.ToolTipText = "Hand Tool"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.SelectText.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.SelectText.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.SelectText.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.SelectText.Name = "btnSelectTextTool"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.SelectText.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.SelectText.Tag = "C1PreviewActionEnum.SelectTextTool"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Text.SelectText.ToolTipText = "Text Select Tool"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.DropZoomFactor.Name = "dropZoomFactor"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.DropZoomFactor.Size = New System.Drawing.Size(13, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.DropZoomFactor.Tag = "C1PreviewActionEnum.ZoomFactor"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ToolTipZoomFactor = Nothing
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomFactor.Name = "txtZoomFactor"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomFactor.Size = New System.Drawing.Size(34, 25)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomFactor.Tag = "C1PreviewActionEnum.ZoomFactor"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomFactor.Text = "100%"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomIn.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomIn.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomIn.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomIn.Name = "btnZoomIn"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomIn.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomIn.Tag = "C1PreviewActionEnum.ZoomIn"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomIn.ToolTipText = "Zoom In"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomOut.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomOut.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomOut.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomOut.Name = "btnZoomOut"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomOut.Size = New System.Drawing.Size(23, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomOut.Tag = "C1PreviewActionEnum.ZoomOut"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomOut.ToolTipText = "Zoom Out"
    '
    '
    '
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomTool.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomInTool, Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomOutTool})
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomTool.Image = CType(resources.GetObject("C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomTool.Image"), System.Drawing.Image)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomTool.ImageTransparentColor = System.Drawing.Color.Magenta
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomTool.Name = "btnZoomTool"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomTool.Size = New System.Drawing.Size(32, 22)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomTool.Tag = "C1PreviewActionEnum.ZoomInTool"
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ToolBars.Zoom.ZoomTool.ToolTipText = "Zoom In Tool"
    Me.C1PrintPreviewDialog1.SizeGripStyle = System.Windows.Forms.SizeGripStyle.[Auto]
    Me.C1PrintPreviewDialog1.Text = "C1PrintPreviewDialog1"
    '
    'frmTestReport
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(849, 654)
    Me.Controls.Add(Me.C1PrintPreviewControl1)
    Me.Name = "frmTestReport"
    Me.Text = "fmrTestReport"
    CType(Me.C1PrintPreviewControl1.PreviewPane, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.C1PrintPreviewControl1, System.ComponentModel.ISupportInitialize).EndInit()
    Me.C1PrintPreviewControl1.ResumeLayout(False)
    Me.C1PrintPreviewControl1.PerformLayout()
    CType(Me.C1PrintPreviewDialog1.PrintPreviewControl.PreviewPane, System.ComponentModel.ISupportInitialize).EndInit()
    CType(Me.C1PrintPreviewDialog1.PrintPreviewControl, System.ComponentModel.ISupportInitialize).EndInit()
    Me.C1PrintPreviewDialog1.PrintPreviewControl.ResumeLayout(False)
    Me.C1PrintPreviewDialog1.PrintPreviewControl.PerformLayout()
    Me.C1PrintPreviewDialog1.ResumeLayout(False)
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents C1PrintPreviewControl1 As C1.Win.C1Preview.C1PrintPreviewControl
  Friend WithEvents C1PrintDocument1 As C1.C1Preview.C1PrintDocument
  Friend WithEvents C1PrintPreviewDialog1 As C1.Win.C1Preview.C1PrintPreviewDialog
End Class
