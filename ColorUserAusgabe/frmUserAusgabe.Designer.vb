<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmUserAusgabe
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
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmUserAusgabe))
    Me.splAusgabeRad = New System.Windows.Forms.SplitContainer()
    Me.splRadioButton = New System.Windows.Forms.SplitContainer()
    Me.panUserAusgabe = New System.Windows.Forms.Panel()
    Me.radTable_12 = New System.Windows.Forms.RadioButton()
    Me.radTable_11 = New System.Windows.Forms.RadioButton()
    Me.radTable_10 = New System.Windows.Forms.RadioButton()
    Me.radTable_09 = New System.Windows.Forms.RadioButton()
    Me.radTable_00 = New System.Windows.Forms.RadioButton()
    Me.radTable_04 = New System.Windows.Forms.RadioButton()
    Me.radTable_08 = New System.Windows.Forms.RadioButton()
    Me.radTable_03 = New System.Windows.Forms.RadioButton()
    Me.radTable_05 = New System.Windows.Forms.RadioButton()
    Me.radTable_07 = New System.Windows.Forms.RadioButton()
    Me.radTable_02 = New System.Windows.Forms.RadioButton()
    Me.radTable_01 = New System.Windows.Forms.RadioButton()
    Me.radTable_06 = New System.Windows.Forms.RadioButton()
    Me.btnKOP = New System.Windows.Forms.Button()
    Me.TdbGridUserAusgabe = New C1.Win.C1TrueDBGrid.C1TrueDBGrid()
    CType(Me.splAusgabeRad, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splAusgabeRad.Panel1.SuspendLayout()
    Me.splAusgabeRad.Panel2.SuspendLayout()
    Me.splAusgabeRad.SuspendLayout()
    CType(Me.splRadioButton, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splRadioButton.Panel1.SuspendLayout()
    Me.splRadioButton.Panel2.SuspendLayout()
    Me.splRadioButton.SuspendLayout()
    Me.panUserAusgabe.SuspendLayout()
    CType(Me.TdbGridUserAusgabe, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'splAusgabeRad
    '
    Me.splAusgabeRad.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splAusgabeRad.Location = New System.Drawing.Point(0, 0)
    Me.splAusgabeRad.Name = "splAusgabeRad"
    '
    'splAusgabeRad.Panel1
    '
    Me.splAusgabeRad.Panel1.Controls.Add(Me.splRadioButton)
    '
    'splAusgabeRad.Panel2
    '
    Me.splAusgabeRad.Panel2.Controls.Add(Me.TdbGridUserAusgabe)
    Me.splAusgabeRad.Size = New System.Drawing.Size(1150, 447)
    Me.splAusgabeRad.SplitterDistance = 257
    Me.splAusgabeRad.TabIndex = 0
    '
    'splRadioButton
    '
    Me.splRadioButton.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splRadioButton.IsSplitterFixed = True
    Me.splRadioButton.Location = New System.Drawing.Point(0, 0)
    Me.splRadioButton.Name = "splRadioButton"
    Me.splRadioButton.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splRadioButton.Panel1
    '
    Me.splRadioButton.Panel1.Controls.Add(Me.panUserAusgabe)
    '
    'splRadioButton.Panel2
    '
    Me.splRadioButton.Panel2.Controls.Add(Me.btnKOP)
    Me.splRadioButton.Size = New System.Drawing.Size(257, 447)
    Me.splRadioButton.SplitterDistance = 305
    Me.splRadioButton.TabIndex = 9
    '
    'panUserAusgabe
    '
    Me.panUserAusgabe.Controls.Add(Me.radTable_12)
    Me.panUserAusgabe.Controls.Add(Me.radTable_11)
    Me.panUserAusgabe.Controls.Add(Me.radTable_10)
    Me.panUserAusgabe.Controls.Add(Me.radTable_09)
    Me.panUserAusgabe.Controls.Add(Me.radTable_00)
    Me.panUserAusgabe.Controls.Add(Me.radTable_04)
    Me.panUserAusgabe.Controls.Add(Me.radTable_08)
    Me.panUserAusgabe.Controls.Add(Me.radTable_03)
    Me.panUserAusgabe.Controls.Add(Me.radTable_05)
    Me.panUserAusgabe.Controls.Add(Me.radTable_07)
    Me.panUserAusgabe.Controls.Add(Me.radTable_02)
    Me.panUserAusgabe.Controls.Add(Me.radTable_01)
    Me.panUserAusgabe.Controls.Add(Me.radTable_06)
    Me.panUserAusgabe.Dock = System.Windows.Forms.DockStyle.Fill
    Me.panUserAusgabe.Location = New System.Drawing.Point(0, 0)
    Me.panUserAusgabe.Name = "panUserAusgabe"
    Me.panUserAusgabe.Size = New System.Drawing.Size(257, 305)
    Me.panUserAusgabe.TabIndex = 9
    '
    'radTable_12
    '
    Me.radTable_12.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_12.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_12.Location = New System.Drawing.Point(3, 278)
    Me.radTable_12.Name = "radTable_12"
    Me.radTable_12.Size = New System.Drawing.Size(251, 26)
    Me.radTable_12.TabIndex = 12
    Me.radTable_12.TabStop = True
    Me.radTable_12.UseVisualStyleBackColor = False
    Me.radTable_12.Visible = False
    '
    'radTable_11
    '
    Me.radTable_11.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_11.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_11.Location = New System.Drawing.Point(3, 255)
    Me.radTable_11.Name = "radTable_11"
    Me.radTable_11.Size = New System.Drawing.Size(251, 26)
    Me.radTable_11.TabIndex = 11
    Me.radTable_11.TabStop = True
    Me.radTable_11.UseVisualStyleBackColor = False
    Me.radTable_11.Visible = False
    '
    'radTable_10
    '
    Me.radTable_10.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_10.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_10.Location = New System.Drawing.Point(3, 230)
    Me.radTable_10.Name = "radTable_10"
    Me.radTable_10.Size = New System.Drawing.Size(251, 26)
    Me.radTable_10.TabIndex = 10
    Me.radTable_10.TabStop = True
    Me.radTable_10.UseVisualStyleBackColor = False
    Me.radTable_10.Visible = False
    '
    'radTable_09
    '
    Me.radTable_09.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_09.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_09.Location = New System.Drawing.Point(3, 207)
    Me.radTable_09.Name = "radTable_09"
    Me.radTable_09.Size = New System.Drawing.Size(251, 26)
    Me.radTable_09.TabIndex = 9
    Me.radTable_09.TabStop = True
    Me.radTable_09.UseVisualStyleBackColor = False
    Me.radTable_09.Visible = False
    '
    'radTable_00
    '
    Me.radTable_00.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_00.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_00.Location = New System.Drawing.Point(3, 3)
    Me.radTable_00.Name = "radTable_00"
    Me.radTable_00.Size = New System.Drawing.Size(251, 26)
    Me.radTable_00.TabIndex = 0
    Me.radTable_00.TabStop = True
    Me.radTable_00.UseVisualStyleBackColor = False
    '
    'radTable_04
    '
    Me.radTable_04.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_04.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_04.Location = New System.Drawing.Point(3, 93)
    Me.radTable_04.Name = "radTable_04"
    Me.radTable_04.Size = New System.Drawing.Size(251, 26)
    Me.radTable_04.TabIndex = 4
    Me.radTable_04.TabStop = True
    Me.radTable_04.UseVisualStyleBackColor = False
    '
    'radTable_08
    '
    Me.radTable_08.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_08.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_08.Location = New System.Drawing.Point(3, 184)
    Me.radTable_08.Name = "radTable_08"
    Me.radTable_08.Size = New System.Drawing.Size(251, 26)
    Me.radTable_08.TabIndex = 8
    Me.radTable_08.TabStop = True
    Me.radTable_08.UseVisualStyleBackColor = False
    '
    'radTable_03
    '
    Me.radTable_03.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_03.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_03.Location = New System.Drawing.Point(3, 70)
    Me.radTable_03.Name = "radTable_03"
    Me.radTable_03.Size = New System.Drawing.Size(251, 26)
    Me.radTable_03.TabIndex = 3
    Me.radTable_03.TabStop = True
    Me.radTable_03.UseVisualStyleBackColor = False
    '
    'radTable_05
    '
    Me.radTable_05.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_05.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_05.Location = New System.Drawing.Point(3, 116)
    Me.radTable_05.Name = "radTable_05"
    Me.radTable_05.Size = New System.Drawing.Size(251, 26)
    Me.radTable_05.TabIndex = 5
    Me.radTable_05.TabStop = True
    Me.radTable_05.UseVisualStyleBackColor = False
    '
    'radTable_07
    '
    Me.radTable_07.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_07.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_07.Location = New System.Drawing.Point(3, 162)
    Me.radTable_07.Name = "radTable_07"
    Me.radTable_07.Size = New System.Drawing.Size(251, 26)
    Me.radTable_07.TabIndex = 7
    Me.radTable_07.TabStop = True
    Me.radTable_07.UseVisualStyleBackColor = False
    '
    'radTable_02
    '
    Me.radTable_02.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_02.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_02.Location = New System.Drawing.Point(3, 47)
    Me.radTable_02.Name = "radTable_02"
    Me.radTable_02.Size = New System.Drawing.Size(251, 26)
    Me.radTable_02.TabIndex = 2
    Me.radTable_02.TabStop = True
    Me.radTable_02.UseVisualStyleBackColor = False
    '
    'radTable_01
    '
    Me.radTable_01.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_01.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_01.Location = New System.Drawing.Point(3, 26)
    Me.radTable_01.Name = "radTable_01"
    Me.radTable_01.Size = New System.Drawing.Size(251, 26)
    Me.radTable_01.TabIndex = 1
    Me.radTable_01.TabStop = True
    Me.radTable_01.UseVisualStyleBackColor = False
    '
    'radTable_06
    '
    Me.radTable_06.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_06.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_06.Location = New System.Drawing.Point(3, 139)
    Me.radTable_06.Name = "radTable_06"
    Me.radTable_06.Size = New System.Drawing.Size(251, 26)
    Me.radTable_06.TabIndex = 6
    Me.radTable_06.TabStop = True
    Me.radTable_06.UseVisualStyleBackColor = False
    '
    'btnKOP
    '
    Me.btnKOP.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnKOP.Location = New System.Drawing.Point(6, 39)
    Me.btnKOP.Name = "btnKOP"
    Me.btnKOP.Size = New System.Drawing.Size(251, 30)
    Me.btnKOP.TabIndex = 1
    Me.btnKOP.Text = "Kopieren Zwischenablage"
    Me.btnKOP.UseVisualStyleBackColor = True
    '
    'TdbGridUserAusgabe
    '
    Me.TdbGridUserAusgabe.Dock = System.Windows.Forms.DockStyle.Fill
    Me.TdbGridUserAusgabe.Location = New System.Drawing.Point(0, 0)
    Me.TdbGridUserAusgabe.Name = "TdbGridUserAusgabe"
    Me.TdbGridUserAusgabe.PreviewInfo.Location = New System.Drawing.Point(0, 0)
    Me.TdbGridUserAusgabe.PreviewInfo.Size = New System.Drawing.Size(0, 0)
    Me.TdbGridUserAusgabe.PreviewInfo.ZoomFactor = 75.0R
    Me.TdbGridUserAusgabe.PrintInfo.PageSettings = CType(resources.GetObject("TdbGridUserAusgabe.PrintInfo.PageSettings"), System.Drawing.Printing.PageSettings)
    Me.TdbGridUserAusgabe.PropBag = resources.GetString("TdbGridUserAusgabe.PropBag")
    Me.TdbGridUserAusgabe.Size = New System.Drawing.Size(889, 447)
    Me.TdbGridUserAusgabe.TabIndex = 0
    Me.TdbGridUserAusgabe.Text = "C1TrueDBGrid1"
    '
    'frmUserAusgabe
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(1150, 447)
    Me.Controls.Add(Me.splAusgabeRad)
    Me.Name = "frmUserAusgabe"
    Me.Text = "frmQualita"
    Me.splAusgabeRad.Panel1.ResumeLayout(False)
    Me.splAusgabeRad.Panel2.ResumeLayout(False)
    CType(Me.splAusgabeRad, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splAusgabeRad.ResumeLayout(False)
    Me.splRadioButton.Panel1.ResumeLayout(False)
    Me.splRadioButton.Panel2.ResumeLayout(False)
    CType(Me.splRadioButton, System.ComponentModel.ISupportInitialize).EndInit()
    Me.splRadioButton.ResumeLayout(False)
    Me.panUserAusgabe.ResumeLayout(False)
    CType(Me.TdbGridUserAusgabe, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents splAusgabeRad As System.Windows.Forms.SplitContainer
  Friend WithEvents radTable_00 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_06 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_05 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_04 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_03 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_02 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_01 As System.Windows.Forms.RadioButton
  Friend WithEvents TdbGridUserAusgabe As C1.Win.C1TrueDBGrid.C1TrueDBGrid
  Friend WithEvents radTable_08 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_07 As System.Windows.Forms.RadioButton
  Friend WithEvents btnKOP As System.Windows.Forms.Button
  Friend WithEvents splRadioButton As System.Windows.Forms.SplitContainer
  Friend WithEvents panUserAusgabe As System.Windows.Forms.Panel
  Friend WithEvents radTable_11 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_10 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_09 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_12 As System.Windows.Forms.RadioButton
End Class
