Public Class frmTestReport
  Dim pd As New PrintDialog
  Private Sub fmrTestReport_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    C1PrintPreviewControl1.AvailablePreviewActions = C1.Win.C1Preview.C1PreviewActionFlags.All '
    'C1PrintDocument1.UsePrinterForDefaultPageSettings = True
    C1PrintPreviewDialog1.Document = C1PrintDocument1
    pd.Document = C1PrintDocument1
    C1PrintDocument1.
    C1PrintPreviewDialog1.
    C1PrintPreviewControl1.Document = C1PrintDocument1
    C1PrintPreviewControl1.
    C1PrintPreviewControl1.Show()
  End Sub
End Class