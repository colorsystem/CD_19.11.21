Public Class frmGetKey

  Private Sub frmGetKey_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim license As New ColorLicenseKey
    txtColGet.Text = license.GetKeyMixed()
  End Sub
End Class