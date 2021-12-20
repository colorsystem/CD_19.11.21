<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmCopyDataDB
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
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmCopyDataDB))
    Me.PanButton = New System.Windows.Forms.Panel()
    Me.splDatenbank = New System.Windows.Forms.SplitContainer()
    Me.btnColorfileSpeichern = New System.Windows.Forms.Button()
    Me.btnColorfileDisplay = New System.Windows.Forms.Button()
    Me.btnDataRepair = New System.Windows.Forms.Button()
    Me.btnFileRepair = New System.Windows.Forms.Button()
    Me.dbgColorfile = New System.Windows.Forms.DataGridView()
    Me.PanButton.SuspendLayout()
    CType(Me.splDatenbank, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splDatenbank.Panel1.SuspendLayout()
    Me.splDatenbank.Panel2.SuspendLayout()
    Me.splDatenbank.SuspendLayout()
    CType(Me.dbgColorfile, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'PanButton
    '
    Me.PanButton.Controls.Add(Me.splDatenbank)
    resources.ApplyResources(Me.PanButton, "PanButton")
    Me.PanButton.Name = "PanButton"
    '
    'splDatenbank
    '
    resources.ApplyResources(Me.splDatenbank, "splDatenbank")
    Me.splDatenbank.FixedPanel = System.Windows.Forms.FixedPanel.Panel1
    Me.splDatenbank.Name = "splDatenbank"
    '
    'splDatenbank.Panel1
    '
    Me.splDatenbank.Panel1.Controls.Add(Me.btnColorfileSpeichern)
    Me.splDatenbank.Panel1.Controls.Add(Me.btnColorfileDisplay)
    Me.splDatenbank.Panel1.Controls.Add(Me.btnDataRepair)
    Me.splDatenbank.Panel1.Controls.Add(Me.btnFileRepair)
    '
    'splDatenbank.Panel2
    '
    Me.splDatenbank.Panel2.Controls.Add(Me.dbgColorfile)
    '
    'btnColorfileSpeichern
    '
    resources.ApplyResources(Me.btnColorfileSpeichern, "btnColorfileSpeichern")
    Me.btnColorfileSpeichern.Name = "btnColorfileSpeichern"
    Me.btnColorfileSpeichern.UseVisualStyleBackColor = True
    '
    'btnColorfileDisplay
    '
    resources.ApplyResources(Me.btnColorfileDisplay, "btnColorfileDisplay")
    Me.btnColorfileDisplay.Name = "btnColorfileDisplay"
    Me.btnColorfileDisplay.UseVisualStyleBackColor = True
    '
    'btnDataRepair
    '
    resources.ApplyResources(Me.btnDataRepair, "btnDataRepair")
    Me.btnDataRepair.Name = "btnDataRepair"
    Me.btnDataRepair.UseVisualStyleBackColor = True
    '
    'btnFileRepair
    '
    resources.ApplyResources(Me.btnFileRepair, "btnFileRepair")
    Me.btnFileRepair.Name = "btnFileRepair"
    Me.btnFileRepair.UseVisualStyleBackColor = True
    '
    'dbgColorfile
    '
    Me.dbgColorfile.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
    resources.ApplyResources(Me.dbgColorfile, "dbgColorfile")
    Me.dbgColorfile.Name = "dbgColorfile"
    '
    'frmCopyDataDB
    '
    resources.ApplyResources(Me, "$this")
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.Controls.Add(Me.PanButton)
    Me.Name = "frmCopyDataDB"
    Me.PanButton.ResumeLayout(False)
    Me.splDatenbank.Panel1.ResumeLayout(False)
    Me.splDatenbank.Panel2.ResumeLayout(False)
    CType(Me.splDatenbank, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splDatenbank.ResumeLayout(False)
    CType(Me.dbgColorfile, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents PanButton As System.Windows.Forms.Panel
  Friend WithEvents btnFileRepair As System.Windows.Forms.Button
  Friend WithEvents btnDataRepair As System.Windows.Forms.Button
  Friend WithEvents splDatenbank As System.Windows.Forms.SplitContainer
  Friend WithEvents btnColorfileDisplay As System.Windows.Forms.Button
  Friend WithEvents dbgColorfile As System.Windows.Forms.DataGridView
  Friend WithEvents btnColorfileSpeichern As System.Windows.Forms.Button
End Class
