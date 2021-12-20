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
    Me.SplitContainer1 = New System.Windows.Forms.SplitContainer()
    Me.btnKOP = New System.Windows.Forms.Button()
    Me.radTable_08 = New System.Windows.Forms.RadioButton()
    Me.radTable_07 = New System.Windows.Forms.RadioButton()
    Me.radTable_06 = New System.Windows.Forms.RadioButton()
    Me.radTable_05 = New System.Windows.Forms.RadioButton()
    Me.radTable_04 = New System.Windows.Forms.RadioButton()
    Me.radTable_03 = New System.Windows.Forms.RadioButton()
    Me.radTable_02 = New System.Windows.Forms.RadioButton()
    Me.radTable_01 = New System.Windows.Forms.RadioButton()
    Me.radTable_00 = New System.Windows.Forms.RadioButton()
    Me.TdbGridQualita = New C1.Win.C1TrueDBGrid.C1TrueDBGrid()
    CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SplitContainer1.Panel1.SuspendLayout()
    Me.SplitContainer1.Panel2.SuspendLayout()
    Me.SplitContainer1.SuspendLayout()
    CType(Me.TdbGridQualita, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.SuspendLayout()
    '
    'SplitContainer1
    '
    Me.SplitContainer1.Dock = System.Windows.Forms.DockStyle.Fill
    Me.SplitContainer1.Location = New System.Drawing.Point(0, 0)
    Me.SplitContainer1.Name = "SplitContainer1"
    '
    'SplitContainer1.Panel1
    '
    Me.SplitContainer1.Panel1.Controls.Add(Me.btnKOP)
    Me.SplitContainer1.Panel1.Controls.Add(Me.radTable_08)
    Me.SplitContainer1.Panel1.Controls.Add(Me.radTable_07)
    Me.SplitContainer1.Panel1.Controls.Add(Me.radTable_06)
    Me.SplitContainer1.Panel1.Controls.Add(Me.radTable_05)
    Me.SplitContainer1.Panel1.Controls.Add(Me.radTable_04)
    Me.SplitContainer1.Panel1.Controls.Add(Me.radTable_03)
    Me.SplitContainer1.Panel1.Controls.Add(Me.radTable_02)
    Me.SplitContainer1.Panel1.Controls.Add(Me.radTable_01)
    Me.SplitContainer1.Panel1.Controls.Add(Me.radTable_00)
    '
    'SplitContainer1.Panel2
    '
    Me.SplitContainer1.Panel2.Controls.Add(Me.TdbGridQualita)
    Me.SplitContainer1.Size = New System.Drawing.Size(1150, 447)
    Me.SplitContainer1.SplitterDistance = 262
    Me.SplitContainer1.TabIndex = 0
    '
    'btnKOP
    '
    Me.btnKOP.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.btnKOP.Location = New System.Drawing.Point(3, 295)
    Me.btnKOP.Name = "btnKOP"
    Me.btnKOP.Size = New System.Drawing.Size(256, 30)
    Me.btnKOP.TabIndex = 1
    Me.btnKOP.Text = "Kopieren Zwischenablage"
    Me.btnKOP.UseVisualStyleBackColor = True
    '
    'radTable_08
    '
    Me.radTable_08.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_08.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_08.Location = New System.Drawing.Point(3, 184)
    Me.radTable_08.Name = "radTable_08"
    Me.radTable_08.Size = New System.Drawing.Size(256, 26)
    Me.radTable_08.TabIndex = 8
    Me.radTable_08.TabStop = True
    Me.radTable_08.UseVisualStyleBackColor = False
    '
    'radTable_07
    '
    Me.radTable_07.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_07.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_07.Location = New System.Drawing.Point(3, 162)
    Me.radTable_07.Name = "radTable_07"
    Me.radTable_07.Size = New System.Drawing.Size(256, 26)
    Me.radTable_07.TabIndex = 7
    Me.radTable_07.TabStop = True
    Me.radTable_07.UseVisualStyleBackColor = False
    '
    'radTable_06
    '
    Me.radTable_06.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_06.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_06.Location = New System.Drawing.Point(3, 139)
    Me.radTable_06.Name = "radTable_06"
    Me.radTable_06.Size = New System.Drawing.Size(256, 26)
    Me.radTable_06.TabIndex = 6
    Me.radTable_06.TabStop = True
    Me.radTable_06.UseVisualStyleBackColor = False
    '
    'radTable_05
    '
    Me.radTable_05.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_05.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_05.Location = New System.Drawing.Point(3, 116)
    Me.radTable_05.Name = "radTable_05"
    Me.radTable_05.Size = New System.Drawing.Size(256, 26)
    Me.radTable_05.TabIndex = 5
    Me.radTable_05.TabStop = True
    Me.radTable_05.UseVisualStyleBackColor = False
    '
    'radTable_04
    '
    Me.radTable_04.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_04.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_04.Location = New System.Drawing.Point(3, 93)
    Me.radTable_04.Name = "radTable_04"
    Me.radTable_04.Size = New System.Drawing.Size(256, 26)
    Me.radTable_04.TabIndex = 4
    Me.radTable_04.TabStop = True
    Me.radTable_04.UseVisualStyleBackColor = False
    '
    'radTable_03
    '
    Me.radTable_03.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_03.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_03.Location = New System.Drawing.Point(3, 70)
    Me.radTable_03.Name = "radTable_03"
    Me.radTable_03.Size = New System.Drawing.Size(256, 26)
    Me.radTable_03.TabIndex = 3
    Me.radTable_03.TabStop = True
    Me.radTable_03.UseVisualStyleBackColor = False
    '
    'radTable_02
    '
    Me.radTable_02.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_02.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_02.Location = New System.Drawing.Point(3, 47)
    Me.radTable_02.Name = "radTable_02"
    Me.radTable_02.Size = New System.Drawing.Size(256, 26)
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
    Me.radTable_01.Size = New System.Drawing.Size(256, 26)
    Me.radTable_01.TabIndex = 1
    Me.radTable_01.TabStop = True
    Me.radTable_01.UseVisualStyleBackColor = False
    '
    'radTable_00
    '
    Me.radTable_00.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
    Me.radTable_00.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.radTable_00.Location = New System.Drawing.Point(3, 3)
    Me.radTable_00.Name = "radTable_00"
    Me.radTable_00.Size = New System.Drawing.Size(256, 26)
    Me.radTable_00.TabIndex = 0
    Me.radTable_00.TabStop = True
    Me.radTable_00.UseVisualStyleBackColor = False
    '
    'TdbGridQualita
    '
    Me.TdbGridQualita.Dock = System.Windows.Forms.DockStyle.Fill
    Me.TdbGridQualita.Location = New System.Drawing.Point(0, 0)
    Me.TdbGridQualita.Name = "TdbGridQualita"
    Me.TdbGridQualita.PreviewInfo.Location = New System.Drawing.Point(0, 0)
    Me.TdbGridQualita.PreviewInfo.Size = New System.Drawing.Size(0, 0)
    Me.TdbGridQualita.PreviewInfo.ZoomFactor = 75.0R
    Me.TdbGridQualita.PrintInfo.PageSettings = CType(resources.GetObject("TdbGridQualita.PrintInfo.PageSettings"), System.Drawing.Printing.PageSettings)
    Me.TdbGridQualita.PropBag = resources.GetString("TdbGridQualita.PropBag")
    Me.TdbGridQualita.Size = New System.Drawing.Size(884, 447)
    Me.TdbGridQualita.TabIndex = 0
    Me.TdbGridQualita.Text = "C1TrueDBGrid1"
    '
    'frmUserAusgabe
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(1150, 447)
    Me.Controls.Add(Me.SplitContainer1)
    Me.Name = "frmUserAusgabe"
    Me.Text = "frmQualita"
    Me.SplitContainer1.Panel1.ResumeLayout(False)
    Me.SplitContainer1.Panel2.ResumeLayout(False)
    CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).EndInit()
    Me.SplitContainer1.ResumeLayout(False)
    CType(Me.TdbGridQualita, System.ComponentModel.ISupportInitialize).EndInit()
    Me.ResumeLayout(False)

  End Sub
  Friend WithEvents SplitContainer1 As System.Windows.Forms.SplitContainer
  Friend WithEvents radTable_00 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_06 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_05 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_04 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_03 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_02 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_01 As System.Windows.Forms.RadioButton
  Friend WithEvents TdbGridQualita As C1.Win.C1TrueDBGrid.C1TrueDBGrid
  Friend WithEvents radTable_08 As System.Windows.Forms.RadioButton
  Friend WithEvents radTable_07 As System.Windows.Forms.RadioButton
  Friend WithEvents btnKOP As System.Windows.Forms.Button
End Class
