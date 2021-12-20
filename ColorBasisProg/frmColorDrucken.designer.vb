<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmColorDrucken
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
    Me.splDrucken = New System.Windows.Forms.SplitContainer()
    Me.txtMisch = New System.Windows.Forms.TextBox()
    Me.txtMess = New System.Windows.Forms.TextBox()
    Me.txtUse = New System.Windows.Forms.TextBox()
    Me.lblUSE = New System.Windows.Forms.Label()
    Me.lblMESS = New System.Windows.Forms.Label()
    Me.lblMISCH = New System.Windows.Forms.Label()
    Me.tabDrucken = New System.Windows.Forms.TabControl()
    Me.TabPage1 = New System.Windows.Forms.TabPage()
    Me.btnDRUCKRwert = New System.Windows.Forms.Button()
    Me.txtBISRwert = New System.Windows.Forms.TextBox()
    Me.lblBISRwert = New System.Windows.Forms.Label()
    Me.txtVONRwert = New System.Windows.Forms.TextBox()
    Me.lblVONRwert = New System.Windows.Forms.Label()
    Me.chkDATRwert = New System.Windows.Forms.CheckBox()
    Me.cboGRPRwert = New System.Windows.Forms.ComboBox()
    Me.lblGRPRwert = New System.Windows.Forms.Label()
    Me.txtSQLRwert = New System.Windows.Forms.TextBox()
    Me.lblSQLRwert = New System.Windows.Forms.Label()
    Me.TabPage2 = New System.Windows.Forms.TabPage()
    Me.btnDruckFarbmittel = New System.Windows.Forms.Button()
    Me.TabPage3 = New System.Windows.Forms.TabPage()
    Me.btnDRUCKRezept = New System.Windows.Forms.Button()
    Me.txtBISRezept = New System.Windows.Forms.TextBox()
    Me.lblBISRezept = New System.Windows.Forms.Label()
    Me.txtVONRezept = New System.Windows.Forms.TextBox()
    Me.lblVONRezept = New System.Windows.Forms.Label()
    Me.chkDATRezept = New System.Windows.Forms.CheckBox()
    Me.cboGRPRezept = New System.Windows.Forms.ComboBox()
    Me.lblGRPRezept = New System.Windows.Forms.Label()
    Me.txtSQLRezept = New System.Windows.Forms.TextBox()
    Me.lblSQLRezept = New System.Windows.Forms.Label()
    Me.TabPage4 = New System.Windows.Forms.TabPage()
    Me.btnDruckSortiment = New System.Windows.Forms.Button()
    CType(Me.splDrucken, System.ComponentModel.ISupportInitialize).BeginInit()
    Me.splDrucken.Panel1.SuspendLayout()
    Me.splDrucken.Panel2.SuspendLayout()
    Me.splDrucken.SuspendLayout()
    Me.tabDrucken.SuspendLayout()
    Me.TabPage1.SuspendLayout()
    Me.TabPage2.SuspendLayout()
    Me.TabPage3.SuspendLayout()
    Me.TabPage4.SuspendLayout()
    Me.SuspendLayout()
    '
    'splDrucken
    '
    Me.splDrucken.Dock = System.Windows.Forms.DockStyle.Fill
    Me.splDrucken.Location = New System.Drawing.Point(0, 0)
    Me.splDrucken.Name = "splDrucken"
    Me.splDrucken.Orientation = System.Windows.Forms.Orientation.Horizontal
    '
    'splDrucken.Panel1
    '
    Me.splDrucken.Panel1.Controls.Add(Me.txtMisch)
    Me.splDrucken.Panel1.Controls.Add(Me.txtMess)
    Me.splDrucken.Panel1.Controls.Add(Me.txtUse)
    Me.splDrucken.Panel1.Controls.Add(Me.lblUSE)
    Me.splDrucken.Panel1.Controls.Add(Me.lblMESS)
    Me.splDrucken.Panel1.Controls.Add(Me.lblMISCH)
    '
    'splDrucken.Panel2
    '
    Me.splDrucken.Panel2.Controls.Add(Me.tabDrucken)
    Me.splDrucken.Size = New System.Drawing.Size(1273, 682)
    Me.splDrucken.SplitterDistance = 72
    Me.splDrucken.TabIndex = 0
    '
    'txtMisch
    '
    Me.txtMisch.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMisch.Location = New System.Drawing.Point(276, 40)
    Me.txtMisch.Name = "txtMisch"
    Me.txtMisch.Size = New System.Drawing.Size(368, 22)
    Me.txtMisch.TabIndex = 44
    '
    'txtMess
    '
    Me.txtMess.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtMess.Location = New System.Drawing.Point(851, 5)
    Me.txtMess.Name = "txtMess"
    Me.txtMess.Size = New System.Drawing.Size(368, 22)
    Me.txtMess.TabIndex = 43
    '
    'txtUse
    '
    Me.txtUse.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtUse.Location = New System.Drawing.Point(276, 0)
    Me.txtUse.Name = "txtUse"
    Me.txtUse.Size = New System.Drawing.Size(368, 22)
    Me.txtUse.TabIndex = 42
    '
    'lblUSE
    '
    Me.lblUSE.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblUSE.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblUSE.Location = New System.Drawing.Point(87, 0)
    Me.lblUSE.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblUSE.Name = "lblUSE"
    Me.lblUSE.Size = New System.Drawing.Size(189, 26)
    Me.lblUSE.TabIndex = 41
    Me.lblUSE.Text = "201"
    Me.lblUSE.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMESS
    '
    Me.lblMESS.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMESS.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMESS.Location = New System.Drawing.Point(651, 3)
    Me.lblMESS.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblMESS.Name = "lblMESS"
    Me.lblMESS.Size = New System.Drawing.Size(200, 26)
    Me.lblMESS.TabIndex = 39
    Me.lblMESS.Text = "204"
    Me.lblMESS.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'lblMISCH
    '
    Me.lblMISCH.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblMISCH.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblMISCH.Location = New System.Drawing.Point(87, 36)
    Me.lblMISCH.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblMISCH.Name = "lblMISCH"
    Me.lblMISCH.Size = New System.Drawing.Size(189, 26)
    Me.lblMISCH.TabIndex = 37
    Me.lblMISCH.Text = "406"
    Me.lblMISCH.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'tabDrucken
    '
    Me.tabDrucken.Appearance = System.Windows.Forms.TabAppearance.Buttons
    Me.tabDrucken.Controls.Add(Me.TabPage1)
    Me.tabDrucken.Controls.Add(Me.TabPage2)
    Me.tabDrucken.Controls.Add(Me.TabPage3)
    Me.tabDrucken.Controls.Add(Me.TabPage4)
    Me.tabDrucken.Dock = System.Windows.Forms.DockStyle.Fill
    Me.tabDrucken.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.tabDrucken.ItemSize = New System.Drawing.Size(66, 30)
    Me.tabDrucken.Location = New System.Drawing.Point(0, 0)
    Me.tabDrucken.Name = "tabDrucken"
    Me.tabDrucken.SelectedIndex = 0
    Me.tabDrucken.Size = New System.Drawing.Size(1273, 606)
    Me.tabDrucken.SizeMode = System.Windows.Forms.TabSizeMode.FillToRight
    Me.tabDrucken.TabIndex = 0
    '
    'TabPage1
    '
    Me.TabPage1.BackColor = System.Drawing.SystemColors.ControlDark
    Me.TabPage1.Controls.Add(Me.btnDRUCKRwert)
    Me.TabPage1.Controls.Add(Me.txtBISRwert)
    Me.TabPage1.Controls.Add(Me.lblBISRwert)
    Me.TabPage1.Controls.Add(Me.txtVONRwert)
    Me.TabPage1.Controls.Add(Me.lblVONRwert)
    Me.TabPage1.Controls.Add(Me.chkDATRwert)
    Me.TabPage1.Controls.Add(Me.cboGRPRwert)
    Me.TabPage1.Controls.Add(Me.lblGRPRwert)
    Me.TabPage1.Controls.Add(Me.txtSQLRwert)
    Me.TabPage1.Controls.Add(Me.lblSQLRwert)
    Me.TabPage1.Location = New System.Drawing.Point(4, 34)
    Me.TabPage1.Name = "TabPage1"
    Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
    Me.TabPage1.Size = New System.Drawing.Size(1265, 568)
    Me.TabPage1.TabIndex = 0
    Me.TabPage1.Text = "3110"
    '
    'btnDRUCKRwert
    '
    Me.btnDRUCKRwert.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnDRUCKRwert.BackColor = System.Drawing.SystemColors.Control
    Me.btnDRUCKRwert.Font = New System.Drawing.Font("Microsoft Sans Serif", 10.2!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnDRUCKRwert.Location = New System.Drawing.Point(521, 369)
    Me.btnDRUCKRwert.Margin = New System.Windows.Forms.Padding(4)
    Me.btnDRUCKRwert.Name = "btnDRUCKRwert"
    Me.btnDRUCKRwert.Size = New System.Drawing.Size(187, 37)
    Me.btnDRUCKRwert.TabIndex = 18
    Me.btnDRUCKRwert.Text = "397"
    Me.btnDRUCKRwert.UseVisualStyleBackColor = False
    '
    'txtBISRwert
    '
    Me.txtBISRwert.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtBISRwert.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.txtBISRwert.Location = New System.Drawing.Point(709, 243)
    Me.txtBISRwert.Margin = New System.Windows.Forms.Padding(4)
    Me.txtBISRwert.Name = "txtBISRwert"
    Me.txtBISRwert.Size = New System.Drawing.Size(107, 22)
    Me.txtBISRwert.TabIndex = 17
    Me.txtBISRwert.Text = "31.12.2011"
    Me.txtBISRwert.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtBISRwert.Visible = False
    '
    'lblBISRwert
    '
    Me.lblBISRwert.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblBISRwert.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblBISRwert.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblBISRwert.Location = New System.Drawing.Point(653, 243)
    Me.lblBISRwert.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblBISRwert.Name = "lblBISRwert"
    Me.lblBISRwert.Size = New System.Drawing.Size(55, 26)
    Me.lblBISRwert.TabIndex = 16
    Me.lblBISRwert.Text = "377"
    Me.lblBISRwert.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblBISRwert.Visible = False
    '
    'txtVONRwert
    '
    Me.txtVONRwert.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtVONRwert.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.txtVONRwert.Location = New System.Drawing.Point(543, 243)
    Me.txtVONRwert.Margin = New System.Windows.Forms.Padding(4)
    Me.txtVONRwert.Name = "txtVONRwert"
    Me.txtVONRwert.Size = New System.Drawing.Size(107, 22)
    Me.txtVONRwert.TabIndex = 15
    Me.txtVONRwert.Text = "01.01.2000"
    Me.txtVONRwert.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtVONRwert.Visible = False
    '
    'lblVONRwert
    '
    Me.lblVONRwert.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblVONRwert.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblVONRwert.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblVONRwert.Location = New System.Drawing.Point(487, 243)
    Me.lblVONRwert.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblVONRwert.Name = "lblVONRwert"
    Me.lblVONRwert.Size = New System.Drawing.Size(55, 26)
    Me.lblVONRwert.TabIndex = 14
    Me.lblVONRwert.Text = "376"
    Me.lblVONRwert.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblVONRwert.Visible = False
    '
    'chkDATRwert
    '
    Me.chkDATRwert.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDATRwert.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkDATRwert.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.chkDATRwert.Location = New System.Drawing.Point(394, 244)
    Me.chkDATRwert.Margin = New System.Windows.Forms.Padding(4)
    Me.chkDATRwert.Name = "chkDATRwert"
    Me.chkDATRwert.Size = New System.Drawing.Size(101, 25)
    Me.chkDATRwert.TabIndex = 13
    Me.chkDATRwert.Text = "375"
    Me.chkDATRwert.UseVisualStyleBackColor = False
    '
    'cboGRPRwert
    '
    Me.cboGRPRwert.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboGRPRwert.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.cboGRPRwert.FormattingEnabled = True
    Me.cboGRPRwert.Location = New System.Drawing.Point(452, 152)
    Me.cboGRPRwert.Margin = New System.Windows.Forms.Padding(4)
    Me.cboGRPRwert.Name = "cboGRPRwert"
    Me.cboGRPRwert.Size = New System.Drawing.Size(415, 24)
    Me.cboGRPRwert.TabIndex = 12
    '
    'lblGRPRwert
    '
    Me.lblGRPRwert.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRPRwert.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRPRwert.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblGRPRwert.Location = New System.Drawing.Point(306, 150)
    Me.lblGRPRwert.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblGRPRwert.Name = "lblGRPRwert"
    Me.lblGRPRwert.Size = New System.Drawing.Size(144, 26)
    Me.lblGRPRwert.TabIndex = 11
    Me.lblGRPRwert.Text = "386"
    Me.lblGRPRwert.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtSQLRwert
    '
    Me.txtSQLRwert.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtSQLRwert.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.txtSQLRwert.Location = New System.Drawing.Point(452, 99)
    Me.txtSQLRwert.Margin = New System.Windows.Forms.Padding(4)
    Me.txtSQLRwert.Name = "txtSQLRwert"
    Me.txtSQLRwert.Size = New System.Drawing.Size(415, 22)
    Me.txtSQLRwert.TabIndex = 10
    '
    'lblSQLRwert
    '
    Me.lblSQLRwert.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSQLRwert.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblSQLRwert.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblSQLRwert.Location = New System.Drawing.Point(306, 95)
    Me.lblSQLRwert.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblSQLRwert.Name = "lblSQLRwert"
    Me.lblSQLRwert.Size = New System.Drawing.Size(144, 26)
    Me.lblSQLRwert.TabIndex = 9
    Me.lblSQLRwert.Text = "369"
    Me.lblSQLRwert.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'TabPage2
    '
    Me.TabPage2.BackColor = System.Drawing.SystemColors.ControlDark
    Me.TabPage2.Controls.Add(Me.btnDruckFarbmittel)
    Me.TabPage2.Location = New System.Drawing.Point(4, 34)
    Me.TabPage2.Name = "TabPage2"
    Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
    Me.TabPage2.Size = New System.Drawing.Size(1265, 568)
    Me.TabPage2.TabIndex = 1
    Me.TabPage2.Text = "3111"
    '
    'btnDruckFarbmittel
    '
    Me.btnDruckFarbmittel.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnDruckFarbmittel.BackColor = System.Drawing.SystemColors.Control
    Me.btnDruckFarbmittel.Font = New System.Drawing.Font("Microsoft Sans Serif", 10.2!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnDruckFarbmittel.Location = New System.Drawing.Point(484, 226)
    Me.btnDruckFarbmittel.Margin = New System.Windows.Forms.Padding(4)
    Me.btnDruckFarbmittel.Name = "btnDruckFarbmittel"
    Me.btnDruckFarbmittel.Size = New System.Drawing.Size(187, 37)
    Me.btnDruckFarbmittel.TabIndex = 19
    Me.btnDruckFarbmittel.Text = "397"
    Me.btnDruckFarbmittel.UseVisualStyleBackColor = False
    '
    'TabPage3
    '
    Me.TabPage3.BackColor = System.Drawing.SystemColors.ControlDark
    Me.TabPage3.Controls.Add(Me.btnDRUCKRezept)
    Me.TabPage3.Controls.Add(Me.txtBISRezept)
    Me.TabPage3.Controls.Add(Me.lblBISRezept)
    Me.TabPage3.Controls.Add(Me.txtVONRezept)
    Me.TabPage3.Controls.Add(Me.lblVONRezept)
    Me.TabPage3.Controls.Add(Me.chkDATRezept)
    Me.TabPage3.Controls.Add(Me.cboGRPRezept)
    Me.TabPage3.Controls.Add(Me.lblGRPRezept)
    Me.TabPage3.Controls.Add(Me.txtSQLRezept)
    Me.TabPage3.Controls.Add(Me.lblSQLRezept)
    Me.TabPage3.Location = New System.Drawing.Point(4, 34)
    Me.TabPage3.Name = "TabPage3"
    Me.TabPage3.Size = New System.Drawing.Size(1265, 568)
    Me.TabPage3.TabIndex = 2
    Me.TabPage3.Text = "3112"
    '
    'btnDRUCKRezept
    '
    Me.btnDRUCKRezept.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnDRUCKRezept.BackColor = System.Drawing.SystemColors.Control
    Me.btnDRUCKRezept.Font = New System.Drawing.Font("Microsoft Sans Serif", 10.2!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnDRUCKRezept.Location = New System.Drawing.Point(567, 403)
    Me.btnDRUCKRezept.Margin = New System.Windows.Forms.Padding(4)
    Me.btnDRUCKRezept.Name = "btnDRUCKRezept"
    Me.btnDRUCKRezept.Size = New System.Drawing.Size(187, 37)
    Me.btnDRUCKRezept.TabIndex = 28
    Me.btnDRUCKRezept.Text = "397"
    Me.btnDRUCKRezept.UseVisualStyleBackColor = False
    '
    'txtBISRezept
    '
    Me.txtBISRezept.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtBISRezept.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.txtBISRezept.Location = New System.Drawing.Point(755, 277)
    Me.txtBISRezept.Margin = New System.Windows.Forms.Padding(4)
    Me.txtBISRezept.Name = "txtBISRezept"
    Me.txtBISRezept.Size = New System.Drawing.Size(107, 22)
    Me.txtBISRezept.TabIndex = 27
    Me.txtBISRezept.Text = "31.12.2011"
    Me.txtBISRezept.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtBISRezept.Visible = False
    '
    'lblBISRezept
    '
    Me.lblBISRezept.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblBISRezept.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblBISRezept.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblBISRezept.Location = New System.Drawing.Point(699, 277)
    Me.lblBISRezept.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblBISRezept.Name = "lblBISRezept"
    Me.lblBISRezept.Size = New System.Drawing.Size(55, 26)
    Me.lblBISRezept.TabIndex = 26
    Me.lblBISRezept.Text = "377"
    Me.lblBISRezept.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblBISRezept.Visible = False
    '
    'txtVONRezept
    '
    Me.txtVONRezept.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtVONRezept.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.txtVONRezept.Location = New System.Drawing.Point(589, 277)
    Me.txtVONRezept.Margin = New System.Windows.Forms.Padding(4)
    Me.txtVONRezept.Name = "txtVONRezept"
    Me.txtVONRezept.Size = New System.Drawing.Size(107, 22)
    Me.txtVONRezept.TabIndex = 25
    Me.txtVONRezept.Text = "01.01.2000"
    Me.txtVONRezept.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
    Me.txtVONRezept.Visible = False
    '
    'lblVONRezept
    '
    Me.lblVONRezept.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblVONRezept.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblVONRezept.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblVONRezept.Location = New System.Drawing.Point(533, 277)
    Me.lblVONRezept.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblVONRezept.Name = "lblVONRezept"
    Me.lblVONRezept.Size = New System.Drawing.Size(55, 26)
    Me.lblVONRezept.TabIndex = 24
    Me.lblVONRezept.Text = "376"
    Me.lblVONRezept.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
    Me.lblVONRezept.Visible = False
    '
    'chkDATRezept
    '
    Me.chkDATRezept.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.chkDATRezept.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.chkDATRezept.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.chkDATRezept.Location = New System.Drawing.Point(440, 278)
    Me.chkDATRezept.Margin = New System.Windows.Forms.Padding(4)
    Me.chkDATRezept.Name = "chkDATRezept"
    Me.chkDATRezept.Size = New System.Drawing.Size(101, 25)
    Me.chkDATRezept.TabIndex = 23
    Me.chkDATRezept.Text = "375"
    Me.chkDATRezept.UseVisualStyleBackColor = False
    '
    'cboGRPRezept
    '
    Me.cboGRPRezept.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.cboGRPRezept.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.cboGRPRezept.FormattingEnabled = True
    Me.cboGRPRezept.Location = New System.Drawing.Point(498, 186)
    Me.cboGRPRezept.Margin = New System.Windows.Forms.Padding(4)
    Me.cboGRPRezept.Name = "cboGRPRezept"
    Me.cboGRPRezept.Size = New System.Drawing.Size(415, 24)
    Me.cboGRPRezept.TabIndex = 22
    '
    'lblGRPRezept
    '
    Me.lblGRPRezept.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblGRPRezept.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblGRPRezept.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblGRPRezept.Location = New System.Drawing.Point(352, 184)
    Me.lblGRPRezept.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblGRPRezept.Name = "lblGRPRezept"
    Me.lblGRPRezept.Size = New System.Drawing.Size(144, 26)
    Me.lblGRPRezept.TabIndex = 21
    Me.lblGRPRezept.Text = "386"
    Me.lblGRPRezept.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'txtSQLRezept
    '
    Me.txtSQLRezept.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.txtSQLRezept.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.txtSQLRezept.Location = New System.Drawing.Point(498, 133)
    Me.txtSQLRezept.Margin = New System.Windows.Forms.Padding(4)
    Me.txtSQLRezept.Name = "txtSQLRezept"
    Me.txtSQLRezept.Size = New System.Drawing.Size(415, 22)
    Me.txtSQLRezept.TabIndex = 20
    '
    'lblSQLRezept
    '
    Me.lblSQLRezept.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.lblSQLRezept.BackColor = System.Drawing.SystemColors.ControlLightLight
    Me.lblSQLRezept.Font = New System.Drawing.Font("Microsoft Sans Serif", 7.8!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.lblSQLRezept.Location = New System.Drawing.Point(352, 129)
    Me.lblSQLRezept.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
    Me.lblSQLRezept.Name = "lblSQLRezept"
    Me.lblSQLRezept.Size = New System.Drawing.Size(144, 26)
    Me.lblSQLRezept.TabIndex = 19
    Me.lblSQLRezept.Text = "369"
    Me.lblSQLRezept.TextAlign = System.Drawing.ContentAlignment.MiddleRight
    '
    'TabPage4
    '
    Me.TabPage4.BackColor = System.Drawing.SystemColors.ControlDark
    Me.TabPage4.Controls.Add(Me.btnDruckSortiment)
    Me.TabPage4.Location = New System.Drawing.Point(4, 34)
    Me.TabPage4.Name = "TabPage4"
    Me.TabPage4.Size = New System.Drawing.Size(1265, 568)
    Me.TabPage4.TabIndex = 3
    Me.TabPage4.Text = "3113"
    '
    'btnDruckSortiment
    '
    Me.btnDruckSortiment.Anchor = System.Windows.Forms.AnchorStyles.None
    Me.btnDruckSortiment.BackColor = System.Drawing.SystemColors.Control
    Me.btnDruckSortiment.Font = New System.Drawing.Font("Microsoft Sans Serif", 10.2!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
    Me.btnDruckSortiment.Location = New System.Drawing.Point(468, 220)
    Me.btnDruckSortiment.Margin = New System.Windows.Forms.Padding(4)
    Me.btnDruckSortiment.Name = "btnDruckSortiment"
    Me.btnDruckSortiment.Size = New System.Drawing.Size(187, 37)
    Me.btnDruckSortiment.TabIndex = 38
    Me.btnDruckSortiment.Text = "397"
    Me.btnDruckSortiment.UseVisualStyleBackColor = False
    '
    'frmColorDrucken
    '
    Me.AutoScaleDimensions = New System.Drawing.SizeF(8!, 16!)
    Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
    Me.ClientSize = New System.Drawing.Size(1273, 682)
    Me.Controls.Add(Me.splDrucken)
    Me.Name = "frmColorDrucken"
    Me.Text = "frmColorDrucken"
    Me.splDrucken.Panel1.ResumeLayout(false)
    Me.splDrucken.Panel1.PerformLayout
    Me.splDrucken.Panel2.ResumeLayout(false)
    CType(Me.splDrucken,System.ComponentModel.ISupportInitialize).EndInit
    Me.splDrucken.ResumeLayout(false)
    Me.tabDrucken.ResumeLayout(false)
    Me.TabPage1.ResumeLayout(false)
    Me.TabPage1.PerformLayout
    Me.TabPage2.ResumeLayout(false)
    Me.TabPage3.ResumeLayout(false)
    Me.TabPage3.PerformLayout
    Me.TabPage4.ResumeLayout(false)
    Me.ResumeLayout(false)

End Sub
  Friend WithEvents splDrucken As System.Windows.Forms.SplitContainer
  Friend WithEvents lblUSE As System.Windows.Forms.Label
  Friend WithEvents lblMESS As System.Windows.Forms.Label
  Friend WithEvents lblMISCH As System.Windows.Forms.Label
  Friend WithEvents tabDrucken As System.Windows.Forms.TabControl
  Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
  Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
  Friend WithEvents TabPage3 As System.Windows.Forms.TabPage
  Friend WithEvents TabPage4 As System.Windows.Forms.TabPage
  Friend WithEvents txtBISRwert As System.Windows.Forms.TextBox
  Friend WithEvents lblBISRwert As System.Windows.Forms.Label
  Friend WithEvents txtVONRwert As System.Windows.Forms.TextBox
  Friend WithEvents lblVONRwert As System.Windows.Forms.Label
  Friend WithEvents chkDATRwert As System.Windows.Forms.CheckBox
  Friend WithEvents cboGRPRwert As System.Windows.Forms.ComboBox
  Friend WithEvents lblGRPRwert As System.Windows.Forms.Label
  Friend WithEvents txtSQLRwert As System.Windows.Forms.TextBox
  Friend WithEvents lblSQLRwert As System.Windows.Forms.Label
  Friend WithEvents btnDRUCKRwert As System.Windows.Forms.Button
  Friend WithEvents txtMisch As System.Windows.Forms.TextBox
  Friend WithEvents txtMess As System.Windows.Forms.TextBox
  Friend WithEvents txtUse As System.Windows.Forms.TextBox
  Friend WithEvents btnDRUCKRezept As System.Windows.Forms.Button
  Friend WithEvents txtBISRezept As System.Windows.Forms.TextBox
  Friend WithEvents lblBISRezept As System.Windows.Forms.Label
  Friend WithEvents txtVONRezept As System.Windows.Forms.TextBox
  Friend WithEvents lblVONRezept As System.Windows.Forms.Label
  Friend WithEvents chkDATRezept As System.Windows.Forms.CheckBox
  Friend WithEvents cboGRPRezept As System.Windows.Forms.ComboBox
  Friend WithEvents lblGRPRezept As System.Windows.Forms.Label
  Friend WithEvents txtSQLRezept As System.Windows.Forms.TextBox
  Friend WithEvents lblSQLRezept As System.Windows.Forms.Label
  Friend WithEvents btnDruckFarbmittel As System.Windows.Forms.Button
  Friend WithEvents btnDruckSortiment As System.Windows.Forms.Button
End Class
