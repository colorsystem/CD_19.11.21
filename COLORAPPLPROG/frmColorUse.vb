Option Explicit On 
Option Compare Text
Option Strict Off
Public Class frmColorUse
  Inherits System.Windows.Forms.Form

#Region " Vom Windows Form Designer generierter Code "

  Public Sub New()
    MyBase.New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

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
    RemoveHandler lstMES.SelectedIndexChanged, AddressOf lstMES_SelectedIndexChanged
    RemoveHandler lstMSY.SelectedIndexChanged, AddressOf lstMSY_SelectedIndexChanged

    DsetUseMisMes.Dispose()
    DatAdUse.Dispose()
    DatAdMis.Dispose()
    DatAdMes.Dispose()
  End Sub

  ' Für Windows Form-Designer erforderlich
  Private components As System.ComponentModel.IContainer

  'HINWEIS: Die folgende Prozedur ist für den Windows Form-Designer erforderlich
  'Sie kann mit dem Windows Form-Designer modifiziert werden.
  'Verwenden Sie nicht den Code-Editor zur Bearbeitung.
  Public WithEvents lblUSBEZ As System.Windows.Forms.Label
  Public WithEvents txtDBAS_2 As System.Windows.Forms.TextBox
  Public WithEvents txtDBAS_1 As System.Windows.Forms.TextBox
  Public WithEvents txtDBAS_0 As System.Windows.Forms.TextBox
  Public WithEvents cmdOKY As System.Windows.Forms.Button
  Public WithEvents lstMSY As System.Windows.Forms.ListBox
  Public WithEvents cmdMESSAGE As System.Windows.Forms.Button
  Public WithEvents lstMES As System.Windows.Forms.ListBox
  Public WithEvents txtPASBEZ As System.Windows.Forms.TextBox
  Public WithEvents lblDBAS_2 As System.Windows.Forms.Label
  Public WithEvents lblDBAS_1 As System.Windows.Forms.Label
  Public WithEvents lblDBAS_0 As System.Windows.Forms.Label
  Public WithEvents lblMSY As System.Windows.Forms.Label
  Public WithEvents lblMES As System.Windows.Forms.Label
  Public WithEvents lblPASBEZ As System.Windows.Forms.Label
  Friend WithEvents cboUser As System.Windows.Forms.ComboBox
  Friend WithEvents btnEnde As System.Windows.Forms.Button
  <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
    Me.lblUSBEZ = New System.Windows.Forms.Label()
    Me.txtDBAS_2 = New System.Windows.Forms.TextBox()
    Me.txtDBAS_1 = New System.Windows.Forms.TextBox()
    Me.txtDBAS_0 = New System.Windows.Forms.TextBox()
    Me.cmdOKY = New System.Windows.Forms.Button()
    Me.lstMSY = New System.Windows.Forms.ListBox()
    Me.cmdMESSAGE = New System.Windows.Forms.Button()
    Me.lstMES = New System.Windows.Forms.ListBox()
    Me.txtPASBEZ = New System.Windows.Forms.TextBox()
    Me.lblDBAS_2 = New System.Windows.Forms.Label()
    Me.lblDBAS_1 = New System.Windows.Forms.Label()
    Me.lblDBAS_0 = New System.Windows.Forms.Label()
    Me.lblMSY = New System.Windows.Forms.Label()
    Me.lblMES = New System.Windows.Forms.Label()
    Me.lblPASBEZ = New System.Windows.Forms.Label()
    Me.cboUser = New System.Windows.Forms.ComboBox()
    Me.btnEnde = New System.Windows.Forms.Button()
    Me.SuspendLayout()
    '
    'lblUSBEZ
    '
    Me.lblUSBEZ.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblUSBEZ.BackColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer))
    Me.lblUSBEZ.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblUSBEZ.ForeColor = System.Drawing.SystemColors.WindowText
    Me.lblUSBEZ.Location = New System.Drawing.Point(135, 153)
    Me.lblUSBEZ.Name = "lblUSBEZ"
    Me.lblUSBEZ.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblUSBEZ.Size = New System.Drawing.Size(116, 18)
    Me.lblUSBEZ.TabIndex = 3
    Me.lblUSBEZ.Tag = "lblUSBEZ"
    Me.lblUSBEZ.Text = "220"
    Me.lblUSBEZ.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtDBAS_2
    '
    Me.txtDBAS_2.AcceptsReturn = True
    Me.txtDBAS_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtDBAS_2.BackColor = System.Drawing.SystemColors.Window
    Me.txtDBAS_2.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtDBAS_2.Enabled = False
    Me.txtDBAS_2.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtDBAS_2.Location = New System.Drawing.Point(248, 437)
    Me.txtDBAS_2.MaxLength = 0
    Me.txtDBAS_2.Name = "txtDBAS_2"
    Me.txtDBAS_2.Size = New System.Drawing.Size(656, 20)
    Me.txtDBAS_2.TabIndex = 18
    '
    'txtDBAS_1
    '
    Me.txtDBAS_1.AcceptsReturn = True
    Me.txtDBAS_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtDBAS_1.BackColor = System.Drawing.SystemColors.Window
    Me.txtDBAS_1.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtDBAS_1.Enabled = False
    Me.txtDBAS_1.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtDBAS_1.Location = New System.Drawing.Point(248, 412)
    Me.txtDBAS_1.MaxLength = 0
    Me.txtDBAS_1.Name = "txtDBAS_1"
    Me.txtDBAS_1.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.txtDBAS_1.Size = New System.Drawing.Size(656, 20)
    Me.txtDBAS_1.TabIndex = 28
    '
    'txtDBAS_0
    '
    Me.txtDBAS_0.AcceptsReturn = True
    Me.txtDBAS_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtDBAS_0.BackColor = System.Drawing.SystemColors.Window
    Me.txtDBAS_0.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtDBAS_0.Enabled = False
    Me.txtDBAS_0.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtDBAS_0.Location = New System.Drawing.Point(248, 387)
    Me.txtDBAS_0.MaxLength = 0
    Me.txtDBAS_0.Name = "txtDBAS_0"
    Me.txtDBAS_0.Size = New System.Drawing.Size(656, 20)
    Me.txtDBAS_0.TabIndex = 29
    '
    'cmdOKY
    '
    Me.cmdOKY.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cmdOKY.BackColor = System.Drawing.SystemColors.Control
    Me.cmdOKY.Cursor = System.Windows.Forms.Cursors.Default
    Me.cmdOKY.ForeColor = System.Drawing.SystemColors.ControlText
    Me.cmdOKY.Location = New System.Drawing.Point(283, 257)
    Me.cmdOKY.Name = "cmdOKY"
    Me.cmdOKY.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.cmdOKY.Size = New System.Drawing.Size(38, 25)
    Me.cmdOKY.TabIndex = 26
    Me.cmdOKY.Text = "5"
    Me.cmdOKY.UseVisualStyleBackColor = False
    '
    'lstMSY
    '
    Me.lstMSY.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstMSY.BackColor = System.Drawing.SystemColors.Window
    Me.lstMSY.Cursor = System.Windows.Forms.Cursors.Default
    Me.lstMSY.ForeColor = System.Drawing.SystemColors.WindowText
    Me.lstMSY.FormattingEnabled = True
    Me.lstMSY.Location = New System.Drawing.Point(453, 153)
    Me.lstMSY.Name = "lstMSY"
    Me.lstMSY.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lstMSY.Size = New System.Drawing.Size(168, 95)
    Me.lstMSY.TabIndex = 25
    Me.lstMSY.Visible = False
    '
    'cmdMESSAGE
    '
    Me.cmdMESSAGE.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cmdMESSAGE.BackColor = System.Drawing.SystemColors.Control
    Me.cmdMESSAGE.Cursor = System.Windows.Forms.Cursors.Default
    Me.cmdMESSAGE.ForeColor = System.Drawing.SystemColors.ControlText
    Me.cmdMESSAGE.Location = New System.Drawing.Point(253, 296)
    Me.cmdMESSAGE.Name = "cmdMESSAGE"
    Me.cmdMESSAGE.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.cmdMESSAGE.Size = New System.Drawing.Size(110, 25)
    Me.cmdMESSAGE.TabIndex = 23
    Me.cmdMESSAGE.Text = "6"
    Me.cmdMESSAGE.UseVisualStyleBackColor = False
    '
    'lstMES
    '
    Me.lstMES.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lstMES.BackColor = System.Drawing.SystemColors.Window
    Me.lstMES.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
    Me.lstMES.Cursor = System.Windows.Forms.Cursors.Default
    Me.lstMES.ForeColor = System.Drawing.SystemColors.WindowText
    Me.lstMES.FormattingEnabled = True
    Me.lstMES.Location = New System.Drawing.Point(637, 153)
    Me.lstMES.Name = "lstMES"
    Me.lstMES.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lstMES.Size = New System.Drawing.Size(177, 93)
    Me.lstMES.TabIndex = 21
    Me.lstMES.Visible = False
    '
    'txtPASBEZ
    '
    Me.txtPASBEZ.AcceptsReturn = True
    Me.txtPASBEZ.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtPASBEZ.BackColor = System.Drawing.SystemColors.Window
    Me.txtPASBEZ.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
    Me.txtPASBEZ.Cursor = System.Windows.Forms.Cursors.IBeam
    Me.txtPASBEZ.ForeColor = System.Drawing.SystemColors.WindowText
    Me.txtPASBEZ.Location = New System.Drawing.Point(255, 224)
    Me.txtPASBEZ.MaxLength = 10
    Me.txtPASBEZ.Name = "txtPASBEZ"
    Me.txtPASBEZ.PasswordChar = Global.Microsoft.VisualBasic.ChrW(42)
    Me.txtPASBEZ.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.txtPASBEZ.Size = New System.Drawing.Size(140, 20)
    Me.txtPASBEZ.TabIndex = 19
    Me.txtPASBEZ.Tag = "txtPASBEZ"
    '
    'lblDBAS_2
    '
    Me.lblDBAS_2.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblDBAS_2.BackColor = System.Drawing.SystemColors.Control
    Me.lblDBAS_2.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblDBAS_2.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblDBAS_2.Location = New System.Drawing.Point(18, 441)
    Me.lblDBAS_2.Name = "lblDBAS_2"
    Me.lblDBAS_2.Size = New System.Drawing.Size(222, 16)
    Me.lblDBAS_2.TabIndex = 30
    Me.lblDBAS_2.Text = "344"
    Me.lblDBAS_2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblDBAS_1
    '
    Me.lblDBAS_1.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblDBAS_1.BackColor = System.Drawing.SystemColors.Control
    Me.lblDBAS_1.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblDBAS_1.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblDBAS_1.Location = New System.Drawing.Point(15, 414)
    Me.lblDBAS_1.Name = "lblDBAS_1"
    Me.lblDBAS_1.Size = New System.Drawing.Size(225, 18)
    Me.lblDBAS_1.TabIndex = 31
    Me.lblDBAS_1.Text = "343"
    Me.lblDBAS_1.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblDBAS_0
    '
    Me.lblDBAS_0.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblDBAS_0.BackColor = System.Drawing.SystemColors.Control
    Me.lblDBAS_0.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblDBAS_0.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblDBAS_0.Location = New System.Drawing.Point(12, 387)
    Me.lblDBAS_0.Name = "lblDBAS_0"
    Me.lblDBAS_0.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblDBAS_0.Size = New System.Drawing.Size(228, 20)
    Me.lblDBAS_0.TabIndex = 27
    Me.lblDBAS_0.Text = "342"
    Me.lblDBAS_0.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMSY
    '
    Me.lblMSY.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMSY.BackColor = System.Drawing.SystemColors.Control
    Me.lblMSY.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblMSY.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblMSY.Location = New System.Drawing.Point(457, 138)
    Me.lblMSY.Name = "lblMSY"
    Me.lblMSY.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblMSY.Size = New System.Drawing.Size(164, 15)
    Me.lblMSY.TabIndex = 24
    Me.lblMSY.Text = "213"
    Me.lblMSY.TextAlign = System.Drawing.ContentAlignment.TopCenter
    Me.lblMSY.Visible = False
    '
    'lblMES
    '
    Me.lblMES.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMES.BackColor = System.Drawing.SystemColors.Control
    Me.lblMES.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblMES.ForeColor = System.Drawing.SystemColors.ControlText
    Me.lblMES.Location = New System.Drawing.Point(637, 138)
    Me.lblMES.Name = "lblMES"
    Me.lblMES.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblMES.Size = New System.Drawing.Size(177, 15)
    Me.lblMES.TabIndex = 22
    Me.lblMES.Text = "203"
    Me.lblMES.TextAlign = System.Drawing.ContentAlignment.TopCenter
    Me.lblMES.Visible = False
    '
    'lblPASBEZ
    '
    Me.lblPASBEZ.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblPASBEZ.BackColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer))
    Me.lblPASBEZ.Cursor = System.Windows.Forms.Cursors.Default
    Me.lblPASBEZ.ForeColor = System.Drawing.SystemColors.WindowText
    Me.lblPASBEZ.Location = New System.Drawing.Point(135, 224)
    Me.lblPASBEZ.Name = "lblPASBEZ"
    Me.lblPASBEZ.RightToLeft = System.Windows.Forms.RightToLeft.No
    Me.lblPASBEZ.Size = New System.Drawing.Size(116, 19)
    Me.lblPASBEZ.TabIndex = 20
    Me.lblPASBEZ.Tag = "lblPASBEZ"
    Me.lblPASBEZ.Text = "202"
    Me.lblPASBEZ.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'cboUser
    '
    Me.cboUser.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboUser.FormattingEnabled = True
    Me.cboUser.Location = New System.Drawing.Point(255, 153)
    Me.cboUser.Name = "cboUser"
    Me.cboUser.Size = New System.Drawing.Size(140, 21)
    Me.cboUser.TabIndex = 32
    '
    'btnEnde
    '
    Me.btnEnde.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnEnde.Location = New System.Drawing.Point(253, 338)
    Me.btnEnde.Name = "btnEnde"
    Me.btnEnde.Size = New System.Drawing.Size(112, 23)
    Me.btnEnde.TabIndex = 33
    Me.btnEnde.Text = "379"
    '
    'frmColorUse
    '
    Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
    Me.ClientSize = New System.Drawing.Size(1035, 585)
    Me.ControlBox = False
    Me.Controls.Add(Me.btnEnde)
    Me.Controls.Add(Me.cboUser)
    Me.Controls.Add(Me.txtDBAS_2)
    Me.Controls.Add(Me.txtDBAS_1)
    Me.Controls.Add(Me.txtDBAS_0)
    Me.Controls.Add(Me.txtPASBEZ)
    Me.Controls.Add(Me.cmdOKY)
    Me.Controls.Add(Me.lstMSY)
    Me.Controls.Add(Me.cmdMESSAGE)
    Me.Controls.Add(Me.lstMES)
    Me.Controls.Add(Me.lblDBAS_2)
    Me.Controls.Add(Me.lblDBAS_1)
    Me.Controls.Add(Me.lblDBAS_0)
    Me.Controls.Add(Me.lblMSY)
    Me.Controls.Add(Me.lblMES)
    Me.Controls.Add(Me.lblPASBEZ)
    Me.Controls.Add(Me.lblUSBEZ)
    Me.Name = "frmColorUse"
    Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
    Me.Text = "frmColorUse"
    Me.ResumeLayout(False)
    Me.PerformLayout()

  End Sub

#End Region
  Dim DsetUseMisMes As New DataSet()
  Dim DatAdUse As New OleDbDataAdapter()
  Dim DatAdMis As New OleDbDataAdapter()
  Dim DatAdMes As New OleDbDataAdapter()
  Dim MnUserID As Integer
  Dim MnMischID As Integer
  Dim MnMessgID As Integer
  Dim MnSonder As Boolean
  Private Sub frmColorUse_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
    Me.lblUSBEZ.Text = Texxt(220)
    Me.cmdOKY.Text = Texxt(5)
    Me.cmdMESSAGE.Text = Texxt(6)
    Me.lblDBAS_2.Text = Texxt(344)
    Me.lblDBAS_1.Text = Texxt(343)
    Me.lblDBAS_0.Text = Texxt(342)
    Me.lblMSY.Text = Texxt(213)
    Me.lblMES.Text = Texxt(203)
    Me.lblPASBEZ.Text = Texxt(202)
    Me.btnEnde.Text = Texxt(379)


    'Call Aend(Me, Me.ToolTipUse)
    cmdMESSAGE.Enabled = False
    MnUserID = -1
    MnMischID = -1
    MnMessgID = -1
    cboUser.Focus()
    txtPASBEZ.SelectionStart = 0
    txtPASBEZ.SelectionLength = 1


    txtDBAS_0.Text = Cncol.DataSource
    txtDBAS_1.Text = Cndat.DataSource
    txtDBAS_2.Text = CnTmp.DataSource
    '
    '
    If CShort(GetPrivSettings("STARTUP", "DISPLY", "0", COLORFileName())) = 0 Then
      txtDBAS_0.Visible = False
      txtDBAS_1.Visible = False
      txtDBAS_2.Visible = False
      lblDBAS_0.Visible = False
      lblDBAS_1.Visible = False
      lblDBAS_2.Visible = False
    End If
    '
    '
    '
    '
    'USER
    '
    '
    '
    '
    If Cncol.State = 0 Then
      Cncol.Open()
    End If
    DatAdUse.SelectCommand = New OleDbCommand("SELECT USER_ID,USER_NAME,USER_PASSW FROM TBL_USER ORDER BY USER_NAME", Cncol)
    DatAdUse.FillSchema(DsetUseMisMes, SchemaType.Source, "USER")
    Cncol.Close()
    cboUser.DataSource = Nothing
    cboUser.DisplayMember = "USER_NAME"
    cboUser.ValueMember = "USER_ID"
    '
    '
    '
    'MISCH
    '
    '
    '
    '
    DsetUseMisMes.Tables.Add(New DataTable("MISCH"))
    DsetUseMisMes.Tables("MISCH").Columns.Add("MISCH_KBEZ", GetType(String))
    DsetUseMisMes.Tables("MISCH").Columns.Add("MISCH_ID", GetType(Integer))
    DatAdMis.SelectCommand = New OleDbCommand("", Cncol)

    '
    '
    '
    'MESSG
    '
    '
    '
    '
    DsetUseMisMes.Tables.Add(New DataTable("MESSG"))
    DsetUseMisMes.Tables("MESSG").Columns.Add("MESSG_KBEZ", GetType(String))
    DsetUseMisMes.Tables("MESSG").Columns.Add("MESSG_ID", GetType(Integer))
    lstMES.DataSource = Nothing
    DatAdMes.SelectCommand = New OleDbCommand("", Cncol)




    If Not FillDatset(DatAdUse, DsetUseMisMes, "USER") Then Exit Sub
    cboUser.DataSource = DsetUseMisMes.Tables("USER")
    cboUser.Select()
  End Sub



  Private Sub cboUser_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboUser.SelectedIndexChanged
    AufbauPar.UserID = -1
    lstMSY.Visible = False
    lblMSY.Visible = False
    lstMES.Visible = False
    lblMES.Visible = False
    cmdMESSAGE.Enabled = False
    cmdOKY.Enabled = True
    txtPASBEZ.Text = ""
    lstMSY.SelectedIndex = -1
    lstMES.SelectedIndex = -1
    lstMES.Tag = -1
    lstMSY.Tag = -1
  End Sub

  Private Sub frmColorUse_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Resize
    'Me.SetDesktopBounds(0.5 * (Screen.PrimaryScreen.WorkingArea.Width - FormMDI.Size.Width), 0.5 * (Screen.PrimaryScreen.WorkingArea.Height - FormMDI.Size.Height), FormMDI.Size.Width, FormMDI.Size.Height)
  End Sub

  Private Sub lstMSY_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstMSY.SelectedIndexChanged
    Dim i As Integer
    If lstMSY.SelectedIndex = -1 Then Exit Sub
    If cboUser.SelectedValue = Nothing Or lstMSY.SelectedValue = Nothing Then Exit Sub
    RemoveHandler lstMES.SelectedIndexChanged, AddressOf lstMES_SelectedIndexChanged
    lstMES.DataSource = Nothing
    DatAdMes.SelectCommand.CommandText = "SELECT DISTINCT TBL_USER_MESSG.MESSG_ID, TBL_MESSG.MESSG_ID AS MESSG_ID, TBL_MISCH_MESSG.MESSG_ID, TBL_USER_MESSG.USER_ID, TBL_MISCH_MESSG.MISCH_ID, TBL_MESSG.MESSG_KBEZ" & " FROM (TBL_MESSG INNER JOIN TBL_USER_MESSG ON TBL_MESSG.MESSG_ID = TBL_USER_MESSG.MESSG_ID) INNER JOIN TBL_MISCH_MESSG ON TBL_MESSG.MESSG_ID = TBL_MISCH_MESSG.MESSG_ID" & " WHERE ((TBL_USER_MESSG.USER_ID=" & cboUser.SelectedValue & ") AND (TBL_MISCH_MESSG.MISCH_ID=" & lstMSY.SelectedValue & "))" & " ORDER BY MESSG_KBEZ"
    If Not FillDatset(DatAdMes, DsetUseMisMes, "MESSG") Then Exit Sub
    lstMES.DataSource = DsetUseMisMes.Tables("MESSG")
    lstMES.DisplayMember = "MESSG_KBEZ"
    lstMES.ValueMember = "MESSG_ID"
    For i = 0 To DsetUseMisMes.Tables("MESSG").Rows.Count - 1
      If DsetUseMisMes.Tables("MESSG").Rows(i)("MESSG_ID") = lstMES.Tag Then
        lstMES.SelectedValue = lstMES.Tag
        Exit For
      End If
    Next

    If i = DsetUseMisMes.Tables("MESSG").Rows.Count Then
      If Me.BindingContext(DsetUseMisMes.Tables("MESSG")).Count = 1 Then
        lstMES.SelectedIndex = 0
      Else
        lstMES.SelectedIndex = -1
        AufbauPar.MessgID = -1
      End If
    End If
    lstMSY.Tag = lstMSY.SelectedValue
    AddHandler lstMES.SelectedIndexChanged, AddressOf lstMES_SelectedIndexChanged

  End Sub

  Private Sub cmdOKY_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles cmdOKY.Click
    If cboUser.SelectedValue = Nothing Then Exit Sub
    '
    '
    '
    'MISCH
    '
    '
    '
    If txtPASBEZ.Text <> DsetUseMisMes.Tables("USER").Rows(cboUser.SelectedIndex).Item("USER_PASSW") Then
      MsgBox(Texxt(15))
      Exit Sub
    End If
    RemoveHandler lstMSY.SelectedIndexChanged, AddressOf lstMSY_SelectedIndexChanged
    DatAdMis.SelectCommand.CommandText = "SELECT TBL_USER_MISCH.MISCH_ID AS MISCH_ID,USER_ID,MISCH_KBEZ FROM TBL_USER_MISCH" & " INNER JOIN TBL_MISCH ON TBL_USER_MISCH.MISCH_ID=TBL_MISCH.MISCH_ID WHERE USER_ID=" & cboUser.SelectedValue & " ORDER BY MISCH_KBEZ"
    lstMSY.DataSource = Nothing
    If Not FillDatset(DatAdMis, DsetUseMisMes, "MISCH") Then Exit Sub
    lstMSY.DataSource = DsetUseMisMes.Tables("MISCH")
    lstMSY.DisplayMember = "MISCH_KBEZ"
    lstMSY.ValueMember = "MISCH_ID"
    lstMSY.Visible = True
    lblMSY.Visible = True
    lstMSY.SelectedIndex = -1
    AddHandler lstMSY.SelectedIndexChanged, AddressOf lstMSY_SelectedIndexChanged


    '
    '
    '
    'MESSG
    '
    '
    '
    RemoveHandler lstMES.SelectedIndexChanged, AddressOf lstMES_SelectedIndexChanged
    DatAdMes.SelectCommand.CommandText = "SELECT DISTINCTROW  TBL_USER_MESSG.MESSG_ID AS MESSG_ID,USER_ID,MESSG_KBEZ FROM TBL_USER_MESSG" & " INNER JOIN TBL_MESSG ON TBL_USER_MESSG.MESSG_ID=TBL_MESSG.MESSG_ID WHERE USER_ID=" & cboUser.SelectedValue & " ORDER BY MESSG_KBEZ"
    lstMES.DataSource = Nothing
    If Not FillDatset(DatAdMes, DsetUseMisMes, "MESSG") Then Exit Sub
    lstMES.DisplayMember = "MESSG_KBEZ"
    lstMES.ValueMember = "MESSG_ID"
    lstMES.DataSource = DsetUseMisMes.Tables("MESSG")
    lstMES.Visible = True
    lblMES.Visible = True
    AddHandler lstMES.SelectedIndexChanged, AddressOf lstMES_SelectedIndexChanged

  End Sub



  Private Sub lstMES_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstMES.SelectedIndexChanged
    Dim i As Integer
    If lstMES.SelectedIndex = -1 Then Exit Sub
    cmdMESSAGE.Enabled = True
    cmdOKY.Enabled = False
    '
    '
    '
    '
    If cboUser.SelectedValue = Nothing Or lstMES.SelectedValue = Nothing Then Exit Sub
    RemoveHandler lstMSY.SelectedIndexChanged, AddressOf lstMSY_SelectedIndexChanged
    lstMSY.DataSource = Nothing
    DatAdMis.SelectCommand.CommandText = "SELECT DISTINCT TBL_USER_MISCH.MISCH_ID, TBL_MISCH.MISCH_ID AS MISCH_ID, TBL_MISCH_MESSG.MISCH_ID, TBL_USER_MISCH.USER_ID, TBL_MISCH_MESSG.MESSG_ID, TBL_MISCH.MISCH_KBEZ" & " FROM (TBL_MISCH INNER JOIN TBL_USER_MISCH ON TBL_MISCH.MISCH_ID = TBL_USER_MISCH.MISCH_ID) INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID = TBL_MISCH_MESSG.MISCH_ID" & " WHERE ((TBL_USER_MISCH.USER_ID=" & cboUser.SelectedValue & ") AND (TBL_MISCH_MESSG.MESSG_ID=" & lstMES.SelectedValue & "))" & " ORDER BY MISCH_KBEZ"
    If Not FillDatset(DatAdMis, DsetUseMisMes, "MISCH") Then Exit Sub
    lstMSY.DataSource = DsetUseMisMes.Tables("MISCH")
    lstMSY.DisplayMember = "MISCH_KBEZ"
    lstMSY.ValueMember = "MISCH_ID"
    AufbauPar.MischID = -1
    For i = 0 To DsetUseMisMes.Tables("MISCH").Rows.Count - 1
      If DsetUseMisMes.Tables("MISCH").Rows(i)("MISCH_ID") = lstMSY.Tag Then
        lstMSY.SelectedValue = lstMSY.Tag
        Exit For
      End If
    Next
    If i = DsetUseMisMes.Tables("MISCH").Rows.Count Then
      If Me.BindingContext(DsetUseMisMes.Tables("MISCH")).Count = 1 Then
        lstMSY.SelectedIndex = -1
      Else
        lstMSY.SelectedIndex = -1
        AufbauPar.MessgID = -1
      End If
    End If
    lstMES.Tag = lstMES.SelectedValue
    AddHandler lstMSY.SelectedIndexChanged, AddressOf lstMSY_SelectedIndexChanged
  End Sub

  Private Sub cmdMES_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles cmdMESSAGE.Click
    Dim check As Boolean
    Dim i As Short
    Dim j As Short
    If cboUser.SelectedIndex >= 0 Then
      MnUserID = cboUser.SelectedValue
    Else
      MnUserID = -1
    End If
    If lstMSY.SelectedIndex >= 0 Then
      MnMischID = lstMSY.SelectedValue
    Else
      MnMischID = -1
    End If
    If lstMES.SelectedIndex >= 0 Then
      MnMessgID = lstMES.SelectedValue
    Else
      MnMessgID = -1
    End If
    AufbauPar.UserID = MnUserID
    If AufbauPar.ier = -1 Then
      Exit Sub
    End If

    AufbauPar.MischID = MnMischID
    If AufbauPar.ier = -1 Then
      Exit Sub
    End If

    AufbauPar.MessgID = MnMessgID
    If AufbauPar.ier = -1 Then
      Exit Sub
    End If

    '
    '
    '
    '
    If lstMES.SelectedIndex < 0 Then
      Me.DialogResult = DialogResult.Abort
      Exit Sub
    End If
    cmdMESSAGE.Enabled = False
    '
    '
    Cursor = System.Windows.Forms.Cursors.WaitCursor
    '
    '
    '
    '
    'Messgeräte_ID
    '
    '
    '
    '
   
    FormMDI.mnuMSG.Enabled = True
    '
    If menueparam.Messg.Winkel.Km = 0 Then
      MsgBox(Texxt(3603))
      cmdMESSAGE.Enabled = True
      lstMES.SelectedIndex = -1
      Cursor = System.Windows.Forms.Cursors.Arrow
      Me.DialogResult = DialogResult.Abort

      Exit Sub
    End If
    '
    '
    '
    '
    '

    '
    'Qualitätskontrolle
    '
    check = False
    For i = 0 To menueparam.User.MethNmeth - 1
      If menueparam.User.MethodID(i) < 50 Then
        check = True
        Exit For
      End If
    Next i

    FormMDI.mnuQC.Visible = False
    menueparam.User.Qual = False
    If check Then
      For j = 0 To FormMDI.mnuQC.MenuItems.Count - 1
        FormMDI.mnuQC.MenuItems(j).Visible = False
        For i = 0 To menueparam.User.MethNmeth - 1
          If menueparam.User.MethodID(i) = j Then
            FormMDI.mnuQC.MenuItems(j).Visible = True
            Exit For
          End If
        Next i
      Next j
      menueparam.User.Qual = True
    End If
    FormMDI.mnuQC.Visible = menueparam.User.Qual
    '
    'Rezeptrechnung
    '
    menueparam.User.Rezpt = False
    FormMDI.mnuRZ.Visible = False
    If menueparam.MischID >= 0 Then
      check = False
      For i = 0 To menueparam.User.MethNmeth - 1
        If menueparam.User.MethodID(i) >= 50 And menueparam.User.MethodID(i) < 100 Then
          check = True
          Exit For
        End If
      Next i
      FormMDI.mnuRZ.Visible = False
      menueparam.User.Rezpt = False
      If check Then
        For j = 0 To FormMDI.mnuRZ.MenuItems.Count - 1
          FormMDI.mnuRZ.MenuItems(j).Visible = False
          For i = 0 To menueparam.User.MethNmeth - 1
            If menueparam.User.MethodID(i) = j + 50 Then
              FormMDI.mnuRZ.MenuItems(j).Visible = True
              Exit For
            End If
          Next i
        Next j
        menueparam.User.Rezpt = True
      End If
    End If
    FormMDI.mnuRZ.Visible = menueparam.User.Rezpt

    '
    'Extras
    '
    check = False
    FormMDI.mnuSPE.Visible = False
    menueparam.User.Extr = False
    For i = 0 To menueparam.User.MethNmeth - 1
      If menueparam.User.MethodID(i) >= 100 And menueparam.User.MethodID(i) < 150 Then
        check = True
        Exit For
      End If
    Next i

    If check Then
      For j = 0 To FormMDI.mnuSPE.MenuItems.Count - 1
        FormMDI.mnuSPE.MenuItems(j).Visible = False
        For i = 0 To menueparam.User.MethNmeth - 1
          If menueparam.User.MethodID(i) = j + 100 Then
            FormMDI.mnuSPE.MenuItems(j).Visible = True
            Exit For
          End If
        Next i
      Next j
      menueparam.User.Extr = True
    End If
    FormMDI.mnuSPE.Visible = menueparam.User.Extr
    '
    '
    FormMDI.mnuDRU.Visible = True
    If PrinterSettings.InstalledPrinters.Count = 0 Or BitWrt(0, menueparam.User.Writ) = 0 Then
      FormMDI.mnuDRU.Visible = False
    Else
      FormMDI.mnuDRU.Visible = True
      FormMDI.mnuDRU.MenuItems.Clear()
      For i = 0 To PrinterSettings.InstalledPrinters.Count - 1
        FormMDI.mnuDRU.MenuItems.Add(PrinterSettings.InstalledPrinters.Item(i), AddressOf FormMDI.mnuDRR_Click)
        FormMDI.mnuDRU.MenuItems(i).Name = "mnuDRR_" & Format(i, "00")
        If Printset.PrinterName = PrinterSettings.InstalledPrinters.Item(i) Then
          FormMDI.mnuDRU.MenuItems(i).Text = "*" & FormMDI.mnuDRU.MenuItems(i).Text
        End If
      Next i
      FormMDI.mnuDRU.Enabled = True
    End If
    '
    '
    'Menüs für Sonderprogramme (ausgeschaltet)
    '
    '
    '
    'If MnSonder Then
    ' FormMDI.mnuQC.Visible = False
    ' FormMDI.mnuRZ.Visible = False
    ' FormMDI.mnuSPE.Visible = False
    '
    'MenueParam.User.Rezpt = False
    'For i = 0 To MenueParam.User.MethNmeth - 1
    ' If MenueParam.User.MethodID(i) >= 70 And MenueParam.User.MethodID(i) < 100 Then
    'FormMDI.mnuRZ.MenuItems(MenueParam.User.MethodID(i) - 50).Visible = True
    'FormMDI.mnuRZ.Visible = True
    'MenueParam.User.Rezpt = True
    'End If
    'Next i
    'If FormMDI.mnuRZ.Visible Then
    'For i = 0 To 19
    ' FormMDI.mnuRZ.MenuItems(i).Visible = True
    ' Next i
    ' End If
    ' MenueParam.User.Qual = False
    ' MenueParam.User.Extr = False
    ' Else
    ' For i = 20 To 25
    ' FormMDI.mnuRZ.MenuItems(i).Visible = False
    ' Next i
    ' End If





    FormMDI.mnuQC.Visible = MenueParam.User.Qual
    If MenueParam.User.Rezpt Then
      FormMDI.mnuRZ.Visible = True
    Else
      FormMDI.mnuRZ.Visible = False
    End If
    FormMDI.mnuSPE.Visible = MenueParam.User.Extr
    '
    '
    '
    '
    '
    '

    Me.DialogResult = DialogResult.OK

  End Sub






  Private Sub btnEnde_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnEnde.Click
    Me.DialogResult = DialogResult.Cancel
  End Sub

  WriteOnly Property Sonder() As Boolean
    Set(ByVal Value As Boolean)
      MnSonder = Value
    End Set
  End Property

  ReadOnly Property UserID() As Integer
    Get
      UserID = MnUserID
    End Get
  End Property

  ReadOnly Property MischID() As Integer
    Get
      MischID = MnMischID
    End Get
  End Property

  ReadOnly Property MessgID() As Integer
    Get
      MessgID = MnMessgID
    End Get
  End Property

  


  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub
 
End Class
