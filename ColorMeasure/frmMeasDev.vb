Option Strict Off
Option Explicit On
Option Compare Text
Friend Class frmMeasDev
  Inherits System.Windows.Forms.Form
  Dim MnMessgMenu As Integer
  Dim MnMeasure As MeasureRefl
  Dim MnRefel As RefValue
  Dim ActTrue As Boolean = False
  Dim Sc As System.Windows.Forms.Screen = _
  System.Windows.Forms.Screen.AllScreens(0)



  Private Sub frmMeasDev_Activated(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Activated
    If ActTrue Then Exit Sub
    ActTrue = True
    Select Case MnMessgMenue
      Case 0
        Call MnMeasure.MeasDeviceMess(MnRefel)
      Case 1
        Call MnMeasure.MeasDeviceKalib()
      Case 2
        Call MnMeasure.MeasDeviceNkalib()
      Case 3
        Call MnMeasure.MeasDeviceInit()
      Case 4
        Call MnMeasure.MeasDeviceSond()
      Case 6
        Call MnMeasure.MeasDeviceManu(MnRefel)
    End Select
  End Sub



  Private Sub frmMeasDev_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    ActTrue = False

  End Sub

  Private Sub frmMeasDev_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
    
    '
    Me.Location = New Point(Sc.Bounds.Width - Me.Bounds.Width, 0)
    ToolStripMenuItemKopieren.Text = Texxt(3922)
    ToolStripMenuItemEinfügen.Text = Texxt(3923)
    If MnMessgMenu > 2 Then
      ContextMenuStripRwerte.Enabled = True
    Else
      ContextMenuStripRwerte.Enabled = False
    End If

    Cncol.Close()
  End Sub
  Friend WriteOnly Property MessgMenu() As Integer
    Set(ByVal value As Integer)
      If MnMeasure Is Nothing Then Exit Property
      MnMessgMenu = value
      MnMeasure.MessgMenue = MnMessgMenu
      MnMeasure.MessStart()
      If MnMessgMenue = 6 Then
        Me.Text = Texxt(398) & "   (" & MessgKbez & ")"
      Else
        Me.Text = Texxt(205 + MnMessgMenue) & "   (" & MessgKbez & ")"
      End If

    End Set
  End Property

  Friend WriteOnly Property Measure() As MeasureRefl
    Set(ByVal value As MeasureRefl)
      MnMeasure = value
      MnMeasure.btnEND = btnEND
      MnMeasure.btnLOE = btnLOE
      MnMeasure.btnWIN = btnWIN
      MnMeasure.btnMIT = btnMIT
      MnMeasure.btnNXT = btnNXT
      MnMeasure.btnABR = btnABR
      MnMeasure.btnSTR = btnSTR
      MnMeasure.btnSTP = btnSTP
      MnMeasure.btnMES_0 = btnMES_0
      MnMeasure.btnMES_1 = btnMES_1
      MnMeasure.btnMES_2 = btnMES_2
      MnMeasure.btnMES_3 = btnMES_3
      MnMeasure.btnMES_4 = btnMES_4
      MnMeasure.btnMES_5 = btnMES_5
      MnMeasure.flgkal = flgKAL
      MnMeasure.cboBAUD = cboBAUD
      MnMeasure.cboCOM = cboCOM
      MnMeasure.cboSPE_0 = cboSPE_0
      MnMeasure.cboSPE_1 = cboSPE_1
      MnMeasure.lblMES = lblMES
      MnMeasure.lblCOM = lblCOM
      MnMeasure.lblBAUD = lblBAUD
      MnMeasure.lblANZ = lblANZ
      MnMeasure.lblWID = lblWID
      MnMeasure.lblSPE_0 = lblSPE_0
      MnMeasure.lblSPE_1 = lblSPE_1
      MnMeasure.txtKEN = txtKEN
      MnMeasure.txtProg = txtProg
      MnMeasure.txtWID = txtWID
      MnMeasure.Parentform = Me
      btnMES.Add(MNbtnMES_0)
      btnMES.Add(MNbtnMES_1)
      btnMES.Add(MNbtnMES_2)
      btnMES.Add(MNbtnMES_3)
      btnMES.Add(MNbtnMES_4)
      btnMES.Add(MNbtnMES_5)
      lblspe.Add(MNlblSPE_0)
      lblspe.Add(MNlblSPE_1)
      cboSPE.Add(MNcboSPE_0)
      cboSPE.Add(MNcboSPE_1)
    End Set
  End Property

  Friend WriteOnly Property Refel() As RefValue
    Set(ByVal value As RefValue)
      MnRefel = value
    End Set
  End Property


  Private Sub ToolStripMenuItemEinfügen_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolStripMenuItemEinfügen.Click
    Dim i As Integer
    Dim j As Integer
    Dim TextSplit() As String
    If flgKAL.Visible Then
      If flgKAL.ColumnCount = 0 Then Exit Sub
      '
      'Zeile aus EXCEL in Tabelle übertzragen
      '
      '
      '
      If Clipboard.GetDataObject.GetData(DataFormats.Text, True) = "" Then Exit Sub
      i = flgKAL.CurrentCell.ColumnIndex
      If i = 0 Or i > flgKAL.Columns.Count - 1 Then Exit Sub
      TextSplit = Clipboard.GetDataObject.GetData(DataFormats.Text, True).Split(vbCrLf)
      If TextSplit.Count < flgKAL.Rows.Count Then Exit Sub
      For j = 0 To flgKAL.Rows.Count - 1
        flgKAL.Rows(j).Cells(i).Value = TextSplit(j)
      Next
    End If
  End Sub

  Private Sub ToolStripMenuItemKopieren_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolStripMenuItemKopieren.Click
    Dim i As Integer
    Dim j As Integer
    Dim TextRwert As String
    Clipboard.Clear()
    '
    'Tabellen für R-Werte in Zwischenablage
    '
    '
    '
    If flgKAL.Visible Then
      If flgKAL.ColumnCount = 0 Then Exit Sub
      '
      'Zeile aus EXCEL in Tabelle übertzragen
      '
      '
      '
      i = flgKAL.CurrentCell.ColumnIndex
      If i = 0 Then Exit Sub
      TextRwert = ""
      For j = 0 To flgKAL.Rows.Count - 1
        TextRwert = TextRwert & flgKAL.Rows(j).Cells(i).Value & vbCrLf
      Next
      Clipboard.SetDataObject(TextRwert.Substring(0, TextRwert.Count - 1))
    End If
  End Sub







  Friend Sub New()
    ' This call is required by the Windows Form Designer.
    InitializeComponent()

    ' Add any initialization after the InitializeComponent() call.
    btnEND.Text = Texxt(CInt(btnEND.Text))
    btnLOE.Text = Texxt(CInt(btnLOE.Text))
    btnWIN.Text = Texxt(CInt(btnWIN.Text))
    btnMIT.Text = Texxt(CInt(btnMIT.Text))
    btnNXT.Text = Texxt(CInt(btnNXT.Text))
    btnABR.Text = Texxt(CInt(btnABR.Text))
    btnSTR.Text = Texxt(CInt(btnSTR.Text))
    btnSTP.Text = Texxt(CInt(btnSTP.Text))
    btnMES_0.Text = Texxt(460)
    btnMES_1.Text = Texxt(461)
    btnMES_2.Text = Texxt(462)
    btnMES_3.Text = Texxt(463)
    btnMES_4.Text = Texxt(464)
    btnMES_5.Text = Texxt(465)
    lblMES.Text = Texxt(CInt(lblMES.Text))
    lblCOM.Text = Texxt(CInt(lblCOM.Text))
    lblBAUD.Text = Texxt(CInt(lblBAUD.Text))
    lblANZ.Text = Texxt(CInt(lblANZ.Text))
    lblWID.Text = Texxt(CInt(lblWID.Text))
    lblSPE_0.Text = Texxt(CInt(lblSPE_0.Text))
    lblSPE_1.Text = Texxt(CInt(lblSPE_1.Text))


  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub

   
  
End Class