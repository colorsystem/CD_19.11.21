Public Class frmColorSet
  Dim MnuMam As New List(Of ToolStripMenuItem)
  Dim MnuUsm As New List(Of ToolStripMenuItem)
  Dim MnuSpz As New List(Of ToolStripMenuItem)
  Dim FormAllg As Object
  Dim Setting As ColSettings

 
  Private Sub frmColorSet_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    
    MDiform = Me
    '

    Setting = New ColSettings
    Screen.GetBounds(Me)
    Me.Left = Screen.GetBounds(Me).Left + 0.5 * (Screen.GetBounds(Me).Width - Me.Width)
    Me.Top = Screen.GetBounds(Me).Top + 0.5 * (Screen.GetBounds(Me).Height - Me.Height)
    Screen.GetBounds(Me)
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    lblMeld.Text = Texxt(2006)
    Me.Text = Texxt(99) & " {" & COLORFileName() & "}"
    btnOKY.Text = Texxt(5)
    lblUsBez.Text = Texxt(221)
    lblPasBez.Text = Texxt(202)
    MnuEnd.Text = Texxt(150)
    MnuUsy.Text = Texxt(410)
    MnuMsy.Text = Texxt(401)
    MnuExtra.Text = Texxt(94)
    mnuInitial.Text = Texxt(304)
    mnuSpez.Text = Texxt(310)
    MnuMam_0.Text = Texxt(402)
    MnuMam_1.Text = Texxt(403)
    MnuMam_2.Text = Texxt(404)
    MnuMam_3.Text = Texxt(405)
    MnuMam_4.Text = Texxt(406)
    MnuMam_5.Text = Texxt(407)
    MnuMam_6.Text = Texxt(408)
   
    '
    MnuUsm_0.Text = Texxt(411)
    MnuUsm_1.Text = Texxt(412)
    MnuUsm_2.Text = Texxt(413)
    MnuUsm_3.Text = Texxt(414)
    MnuUsm_4.Text = Texxt(415)
    MnuUsm_5.Text = Texxt(416)
    MnuUsm_6.Text = Texxt(417)
    '
    '
    MnuExt_0.Text = Texxt(1908)
    MnuExt_1.Text = Texxt(1909)
    '
    '
    '
    MnuSpz_0.Text = Texxt(2008)
    MnuSpz_1.Text = Texxt(312)
    mnuspz_2.text = Texxt(313)
    MnuSpz_3.Text = Texxt(147)
    MnuSpz_4.Text = Texxt(2019)
    MnuSpz_5.Text = Texxt(2020)
    MnuSpz_6.Text = Texxt(143)

    MnuMam.Add(MnuMam_0)
    MnuMam.Add(MnuMam_1)
    MnuMam.Add(MnuMam_2)
    MnuMam.Add(MnuMam_3)
    MnuMam.Add(MnuMam_4)
    MnuMam.Add(MnuMam_5)
    MnuMam.Add(MnuMam_6)
    
    '
    MnuUsm.Add(MnuUsm_0)
    MnuUsm.Add(MnuUsm_1)
    MnuUsm.Add(MnuUsm_2)
    MnuUsm.Add(MnuUsm_3)
    MnuUsm.Add(MnuUsm_4)
    MnuUsm.Add(MnuUsm_5)
    MnuUsm.Add(MnuUsm_6)
    '
    '
    MnuSpz.Add(MnuSpz_0)
    MnuSpz.Add(MnuSpz_1)
    MnuSpz.Add(MnuSpz_2)
    MnuSpz.Add(MnuSpz_3)
    MnuSpz.Add(MnuSpz_4)
    MnuSpz.Add(MnuSpz_5)
    MnuSpz.Add(MnuSpz_6)
    '
    txtUsBez.Focus()
    Application.DoEvents()
  End Sub
  Private Sub frmColorSet_FormClosing(sender As Object, e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
    If MessageBox.Show(Texxt(2992), Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
      e.Cancel = False
    Else
      e.Cancel = True
    End If
  End Sub
  Private Sub MnuEnd_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnuEnd.Click
    Me.Close()
  End Sub

  Private Sub btnOKY_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnOKY.Click

    If txtUsBez.Text = ManagerUser Then
      UserMan = False
      UserUfo = False
      If txtPasBez.Text = ManagerPassa Then
        UserUfo = True
      End If
      If txtPasBez.Text = ManagerPassw Or txtPasBez.Text = ManagerPassa Then
        UserMan = True
        Password = txtPasBez.Text
        FormAllg = New frmSST
        FormAllg.showdialog()
        If FormAllg.dialogresult = DialogResult.OK Then

          lblMeld.Visible = True
          '
          lblPasBez.Visible = False
          lblUsBez.Visible = False
          txtPasBez.Visible = False
          txtUsBez.Visible = False
          btnOKY.Visible = False
          MnuEnd.Enabled = False
          Cursor = Cursors.WaitCursor
          Application.DoEvents()
          If ConnOpen(Cncol) Then
            Call CopyDaText()
            Call TextNewStart()
            Call CopyDaBase()
            Cncol.Close()
          End If
          lblMeld.Visible = False

          MnuEnd.Enabled = True

          MnuMsy.Enabled = True
          MnuUsy.Enabled = True
          MnuMsy.Visible = True
          MnuUsy.Visible = True
          MnuExtra.Visible = True
          MnuExtra.Enabled = True
          'mnuDDD.Visible = False
          mnuSpez.Enabled = True
          mnuSpez.Visible = True
          mnuInitial.Visible = True
          If Not UserUfo Then
            MnuMam(3).Visible = False
            mnuSpez.Visible = False
            mnuInitial.Visible = False
          End If
          Cursor = Cursors.Default
        End If
      End If
    End If
    Me.Text = Texxt(99)
  End Sub
  

  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.

  End Sub

  Private Sub MnuMam_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  MnuMam_0.Click, MnuMam_1.Click, MnuMam_2.Click, MnuMam_3.Click, MnuMam_4.Click, _
  MnuMam_5.Click, MnuMam_6.Click
    Dim index As Integer
    Cursor = Cursors.WaitCursor
    index = CInt(sender.name.substring(7, 1))
    Call MenueEnabled(False)
    Setting.SysEinstellForm(index, True, UserUfo, Me)
    Call MenueEnabled(True)
    Cursor = Cursors.Default
  End Sub
  '
  '
  Private Sub MnuUsm_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  MnuUsm_0.Click, MnuUsm_1.Click, MnuUsm_2.Click, MnuUsm_3.Click, MnuUsm_4.Click, MnuUsm_5.Click, MnuUsm_6.Click
    Dim index As Integer
    Cursor = Cursors.WaitCursor
    Call MenueEnabled(False)
    index = CInt(sender.name.substring(7, 1))
    Setting.EinstellForm(index, -1, -1, -1, -1, True, UserUfo, Me)
    Call MenueEnabled(True)
    Cursor = Cursors.Default
  End Sub
  Private Sub MnuExt_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  MnuExt_0.Click, MnuExt_1.Click
    Dim index As Integer
    Cursor = Cursors.WaitCursor
    Call MenueEnabled(False)
    index = CInt(sender.name.substring(7, 1))
    Setting.ExtEinstellForm(index, -1, -1, -1, -1, True, MDiform)
    Call MenueEnabled(True)
    Cursor = Cursors.Default
  End Sub
  Private Sub MnuSpz_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnuSpz_0.Click, MnuSpz_1.Click, MnuSpz_2.Click, MnuSpz_3.Click, MnuSpz_4.Click, MnuSpz_5.Click, MnuSpz_6.Click
    Dim index As Integer
    Cursor = Cursors.WaitCursor
    Call MenueEnabled(False)
    index = CInt(sender.name.substring(7, 1))
    Try

      If Not IsNothing(FormAllg) Then
        FormAllg.close()
      End If


    Catch ex As Exception

    End Try
    Cursor = Cursors.WaitCursor
    Select Case index
      Case 0
        FormAllg = New frmCheckRelations
      Case 1
        FormAllg = New frmNormlicht
      Case 2
        FormAllg = New frmMessAdjust
      Case 2
        FormAllg = New frmMessAdjust
      Case 3
        FormAllg = New frmColorDBStructure
      Case 4
        FormAllg = New frmColorDbDelete
      Case 5
        FormAllg = New frmColorDbCopy
      Case 6
        FormAllg = New frmWriteMessgMisch
    End Select
    FormAllg.MdiParent = Me
    Try

  
    FormAllg.Show()
    FormAllg.windowstate = FormWindowState.Maximized

    Catch ex As Exception

    End Try
    Call MenueEnabled(True)
    Cursor = Cursors.Default
  End Sub
  Sub MenueEnabled(switch As Boolean)
    MnuExtra.Enabled = switch
    MnuMsy.Enabled = switch
    MnuSettings.Enabled = switch
    mnuSpez.Enabled = switch
  End Sub

  Private Sub mnuInitial_Click(sender As Object, e As System.EventArgs) Handles mnuInitial.Click
    FormAllg = New frmSST
    FormAllg.showdialog()
  End Sub
End Class