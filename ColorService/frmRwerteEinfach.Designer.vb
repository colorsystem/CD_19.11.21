<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Public Class frmRwerteEinfach
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
    Me.lblSQL = New System.Windows.Forms.Label()
    Me.txtSql = New System.Windows.Forms.TextBox()
    Me.btnRwerteLesen = New System.Windows.Forms.Button()
    Me.lblBIS = New System.Windows.Forms.Label()
    Me.txtVon = New System.Windows.Forms.TextBox()
    Me.txtBIS = New System.Windows.Forms.TextBox()
    Me.dbgREF = New System.Windows.Forms.DataGrid()
    Me.btnAbbruch = New System.Windows.Forms.Button()
    Me.chkDatum = New System.Windows.Forms.CheckBox()
    Me.btnMessen = New System.Windows.Forms.Button()
    CType(Me.dbgREF, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'lblSQL
    '
    Me.lblSQL.Font = New System.Drawing.Font("Arial", 11.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblSQL.Location = New System.Drawing.Point(284, 78)
    Me.lblSQL.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblSQL.Name = "lblSQL"
    Me.lblSQL.Size = New System.Drawing.Size(360, 19)
    Me.lblSQL.TabIndex = 1
    Me.lblSQL.Text = "369"
    Me.lblSQL.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'txtSql
    '
    Me.txtSql.Location = New System.Drawing.Point(284, 101)
    Me.txtSql.Margin = New System.Windows.Forms.Padding(4)
    Me.txtSql.Name = "txtSql"
    Me.txtSql.Size = New System.Drawing.Size(360, 26)
    Me.txtSql.TabIndex = 2
    '
    'btnRwerteLesen
    '
    Me.btnRwerteLesen.Font = New System.Drawing.Font("Arial", 14.0!, System.Drawing.FontStyle.Bold)
    Me.btnRwerteLesen.ForeColor = System.Drawing.Color.Gray
    Me.btnRwerteLesen.Location = New System.Drawing.Point(284, 124)
    Me.btnRwerteLesen.Margin = New System.Windows.Forms.Padding(4)
    Me.btnRwerteLesen.Name = "btnRwerteLesen"
    Me.btnRwerteLesen.Size = New System.Drawing.Size(360, 40)
    Me.btnRwerteLesen.TabIndex = 3
    Me.btnRwerteLesen.Text = "4629"
    '
    'lblBIS
    '
    Me.lblBIS.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.lblBIS.BackColor = System.Drawing.Color.White
    Me.lblBIS.Location = New System.Drawing.Point(441, 49)
    Me.lblBIS.Name = "lblBIS"
    Me.lblBIS.Size = New System.Drawing.Size(37, 24)
    Me.lblBIS.TabIndex = 101
    Me.lblBIS.Text = "377"
    Me.lblBIS.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    '
    'txtVon
    '
    Me.txtVon.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.txtVon.BackColor = System.Drawing.SystemColors.Control
    Me.txtVon.Location = New System.Drawing.Point(345, 52)
    Me.txtVon.Name = "txtVon"
    Me.txtVon.Size = New System.Drawing.Size(87, 26)
    Me.txtVon.TabIndex = 100
    Me.txtVon.Text = "01.01.2005"
    Me.txtVon.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    '
    'txtBIS
    '
    Me.txtBIS.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.txtBIS.BackColor = System.Drawing.SystemColors.Control
    Me.txtBIS.Location = New System.Drawing.Point(484, 50)
    Me.txtBIS.Name = "txtBIS"
    Me.txtBIS.Size = New System.Drawing.Size(89, 26)
    Me.txtBIS.TabIndex = 99
    Me.txtBIS.Text = "01.01.2006"
    Me.txtBIS.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    '
    'dbgREF
    '
    Me.dbgREF.DataMember = ""
    Me.dbgREF.HeaderForeColor = System.Drawing.SystemColors.ControlText
    Me.dbgREF.Location = New System.Drawing.Point(284, 161)
    Me.dbgREF.Name = "dbgREF"
    Me.dbgREF.Size = New System.Drawing.Size(360, 444)
    Me.dbgREF.TabIndex = 103
    '
    'btnAbbruch
    '
    Me.btnAbbruch.Font = New System.Drawing.Font("Arial", 14.0!, System.Drawing.FontStyle.Bold)
    Me.btnAbbruch.ForeColor = System.Drawing.Color.Gray
    Me.btnAbbruch.Location = New System.Drawing.Point(5, 535)
    Me.btnAbbruch.Margin = New System.Windows.Forms.Padding(4)
    Me.btnAbbruch.Name = "btnAbbruch"
    Me.btnAbbruch.Size = New System.Drawing.Size(265, 38)
    Me.btnAbbruch.TabIndex = 104
    Me.btnAbbruch.Text = "379"
    '
    'chkDatum
    '
    Me.chkDatum.AutoSize = True
    Me.chkDatum.BackColor = System.Drawing.SystemColors.Control
    Me.chkDatum.Checked = True
    Me.chkDatum.CheckState = System.Windows.Forms.CheckState.Checked
    Me.chkDatum.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.chkDatum.Location = New System.Drawing.Point(421, 24)
    Me.chkDatum.Name = "chkDatum"
    Me.chkDatum.Size = New System.Drawing.Size(71, 24)
    Me.chkDatum.TabIndex = 105
    Me.chkDatum.Text = "4628"
    Me.chkDatum.UseVisualStyleBackColor = False
    '
    'btnMessen
    '
    Me.btnMessen.BackColor = System.Drawing.SystemColors.Control
    Me.btnMessen.Font = New System.Drawing.Font("Arial", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnMessen.Image = Global.ColorService.My.Resources.Resources.GrrZap
    Me.btnMessen.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
    Me.btnMessen.Location = New System.Drawing.Point(32, 161)
    Me.btnMessen.Margin = New System.Windows.Forms.Padding(4)
    Me.btnMessen.Name = "btnMessen"
    Me.btnMessen.Size = New System.Drawing.Size(214, 45)
    Me.btnMessen.TabIndex = 0
    Me.btnMessen.Text = "205"
    Me.btnMessen.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    Me.btnMessen.UseVisualStyleBackColor = False
    '
    'frmRwerteEinfach
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(9.0!, 20.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.BackColor = System.Drawing.Color.WhiteSmoke
    Me.ClientSize = New System.Drawing.Size(655, 608)
    Me.Controls.Add(Me.chkDatum)
    Me.Controls.Add(Me.btnAbbruch)
    Me.Controls.Add(Me.dbgREF)
    Me.Controls.Add(Me.lblBIS)
    Me.Controls.Add(Me.txtVon)
    Me.Controls.Add(Me.txtBIS)
    Me.Controls.Add(Me.btnRwerteLesen)
    Me.Controls.Add(Me.txtSql)
    Me.Controls.Add(Me.lblSQL)
    Me.Controls.Add(Me.btnMessen)
    Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
    Me.Margin = New System.Windows.Forms.Padding(4)
    Me.Name = "frmRwerteEinfach"
    CType(Me.dbgREF, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)
    Me.PerformLayout()

  End Sub
  Friend WithEvents btnMessen As System.Windows.Forms.Button
  Friend WithEvents lblSQL As System.Windows.Forms.Label
  Friend WithEvents txtSql As System.Windows.Forms.TextBox
  Friend WithEvents btnRwerteLesen As System.Windows.Forms.Button
  Friend WithEvents lblBIS As System.Windows.Forms.Label
  Friend WithEvents txtVon As System.Windows.Forms.TextBox
  Friend WithEvents txtBIS As System.Windows.Forms.TextBox
  Friend WithEvents dbgREF As System.Windows.Forms.DataGrid

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    DataAdapter.Dispose()
    RwrtTable.Dispose()
  End Sub
  Friend WithEvents btnAbbruch As System.Windows.Forms.Button
  Friend WithEvents chkDatum As System.Windows.Forms.CheckBox
End Class
