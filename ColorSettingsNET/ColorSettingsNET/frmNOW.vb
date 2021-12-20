Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmNOW
  Dim MnUserid As Integer
  Dim MnMessgID As Integer
  Dim MnMethID As Integer
  Dim MnMischid As Integer
  Dim Iopenart As Boolean
  Dim StrLinUse As String
  Dim StrLinMet As String
  Dim MaxMenID As Integer
  Dim MaxLchID As Integer
  Dim MaxMatID As Integer
  Dim LchgID As Integer            'ID Gewichte DL,DC und DH
  Dim MatpaID As Long           'ID für mathematische Parameter
  Dim ier As Integer
  Dim Iprn As Integer
  Dim imsg As Integer
  '
  '
  Dim SqlStmt As String
  Dim SqlEtmt As String

  '
  '
  Dim txtMEN As List(Of TextBox)
  Dim txtLCH As List(Of TextBox)
  Dim txtMAT As List(Of TextBox)
  '
  Dim CmdUSE As OleDbCommand
  Dim CmdMET As OleDbCommand
  Dim CmdMEN As OleDbCommand
  Dim CmdLCH As OleDbCommand
  Dim CmdMAT As OleDbCommand
  Dim CmdNOR As OleDbCommand
  Dim CmdNOA As OleDbCommand
  Dim CmdBWE As OleDbCommand
  Dim CmdFST As OleDbCommand
  '
  '
  '
  Dim AdaptUSE As OleDbDataAdapter
  Dim AdaptMET As OleDbDataAdapter
  Dim AdaptMEN As OleDbDataAdapter
  Dim AdaptLCH As OleDbDataAdapter
  Dim AdaptMAT As OleDbDataAdapter
  Dim AdaptNOR As OleDbDataAdapter
  Dim AdaptNOA As OleDbDataAdapter
  Dim AdaptBWE As OleDbDataAdapter
  Dim AdaptFST As OleDbDataAdapter
  '
  '
  Dim TblUSE As DataTable
  Dim TblMET As DataTable
  Dim TblMEN As DataTable
  Dim TblLCH As DataTable
  Dim TblMAT As DataTable
  Dim TblNOR As DataTable
  Dim TblNOA As DataTable
  Dim TblBWE As DataTable
  Dim TblFST As DataTable
  '
  '
  '
  Dim ViewUSE As DataView
  Dim ViewMET As DataView
  Dim ViewMEN As DataView
  Dim ViewLCH As DataView
  Dim ViewMAT As DataView
  Dim ViewNOR As DataView
  Dim ViewNOA As DataView
  '
  '
  '
  Dim WithEvents ConnUSE As BindingSource
  Dim WithEvents ConnMET As BindingSource
  Dim WithEvents ConnMEN As BindingSource
  Dim WithEvents ConnLCH As BindingSource
  Dim WithEvents ConnMAT As BindingSource
  Private Sub frmNOW_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    Dim DataGridCombo As DataGridViewComboBoxColumn
    '
    Me.Text = Texxt(412)
    btnORD.Text = Texxt(1999)
    lblABS.Text = Texxt(191)
    lblBWE.Text = Texxt(185)
    lblFST.Text = Texxt(262)
    lblNOW.Text = Texxt(201)
    lblMTH.Text = Texxt(421)
    lblNOR.Text = Texxt(231)
    lblNOA.Text = Texxt(232)
    lblMEN_0.Text = Texxt(169)
    lblMEN_1.Text = Texxt(189)
    lblMEN_2.Text = Texxt(184)
    lblMEN_3.Text = Texxt(168)
    lblMEN_4.Text = Texxt(186)
    lblMEN_5.Text = Texxt(187)
    lblMEN_6.Text = Texxt(188)
    lblMEN_7.Text = Texxt(306) & "(0);" & Texxt(307) & "(1);" & Texxt(308) & "(2)"
    lblMEN_8.Text = Texxt(183)
    lblLCH_0.Text = Texxt(179)
    lblLCH_1.Text = Texxt(180)
    lblLCH_2.Text = Texxt(181)
    lblUNT_0.Text = Texxt(194)
    lblUNT_1.Text = Texxt(195)
    lblMAT_0.Text = Texxt(540)
    lblMAT_1.Text = Texxt(541)
    lblMAT_2.Text = Texxt(542)
    lblMAT_3.Text = Texxt(543)
    lblMAT_4.Text = Texxt(544)
    lblMAT_5.Text = Texxt(539)
    lblUNT_0.Text = Texxt(194)
    lblUNT_1.Text = Texxt(195)


    TabNOW.TabPages(0).Text = Texxt(320)
    TabNOW.TabPages(1).Text = Texxt(442)
    TabNOW.TabPages(2).Text = Texxt(446)
    TabNOW.TabPages(3).Text = Texxt(426)
    '
    '
    '
    '
    '
    '
    '
    TblUSE = New DataTable
    TblMET = New DataTable
    TblMEN = New DataTable
    TblLCH = New DataTable
    TblMAT = New DataTable
    TblNOR = New DataTable
    TblNOA = New DataTable
    TblBWE = New DataTable
    TblFST = New DataTable
    '
    '
    '
    ViewUse = New DataView(TblUSE)
    ViewMET = New DataView(TblMET)
    ViewMEN = New DataView(TblMEN)
    ViewLCH = New DataView(TblLCH)
    ViewMAT = New DataView(TblMAT)
    ViewNOR = New DataView(TblNOR)
    ViewNOA = New DataView(TblNOA)

    '
    '
    CmdUSE = New OleDbCommand("", Cncol)
    CmdMET = New OleDbCommand("", Cncol)
    CmdMEN = New OleDbCommand("", Cncol)
    CmdLCH = New OleDbCommand("", Cncol)
    CmdMAT = New OleDbCommand("", Cncol)
    CmdNOR = New OleDbCommand("", Cncol)
    CmdNOA = New OleDbCommand("", Cncol)
    CmdBWE = New OleDbCommand("", Cncol)
    CmdFST = New OleDbCommand("", Cncol)
    '
    '
    '
    AdaptUSE = New OleDbDataAdapter
    AdaptMET = New OleDbDataAdapter
    AdaptMEN = New OleDbDataAdapter
    AdaptLCH = New OleDbDataAdapter
    AdaptMAT = New OleDbDataAdapter
    AdaptNOR = New OleDbDataAdapter
    AdaptNOA = New OleDbDataAdapter
    AdaptBWE = New OleDbDataAdapter
    AdaptFST = New OleDbDataAdapter

    '
    AdaptUSE.SelectCommand = CmdUSE
    AdaptMET.SelectCommand = CmdMET
    AdaptMEN.SelectCommand = CmdMEN
    AdaptLCH.SelectCommand = CmdLCH
    AdaptMAT.SelectCommand = CmdMAT
    AdaptNOR.SelectCommand = CmdNOR
    AdaptNOA.SelectCommand = CmdNOA
    AdaptBWE.SelectCommand = CmdBWE
    AdaptFST.SelectCommand = CmdFST

    '
    '
    ConnUSE = New BindingSource
    ConnMET = New BindingSource
    ConnMEN = New BindingSource
    ConnLCH = New BindingSource
    ConnMAT = New BindingSource
    '
    '
    '
    '
    '
    '
    '
    BindingUSE.BindingSource = ConnUSE
    BindingMET.BindingSource = ConnMET
    BindingMEN.BindingSource = ConnMEN
    BindingLCH.BindingSource = ConnLCH
    BindingMAT.BindingSource = ConnMAT



    txtMEN = New List(Of TextBox)
    txtMEN.Add(txtMEN_0)
    txtMEN.Add(txtMEN_1)
    txtMEN.Add(txtMEN_2)
    txtMEN.Add(txtMEN_3)
    txtMEN.Add(txtMEN_4)
    txtMEN.Add(txtMEN_5)
    txtMEN.Add(txtMEN_6)
    txtMEN.Add(txtMEN_7)
    txtMEN.Add(txtMEN_8)
    txtMEN.Add(txtMEN_9)
    '
    '
    txtMAT = New List(Of TextBox)
    txtMAT.Add(txtMAT_0)
    txtMAT.Add(txtMAT_1)
    txtMAT.Add(txtMAT_2)
    txtMAT.Add(txtMAT_3)
    txtMAT.Add(txtMAT_4)
    txtMAT.Add(txtMAT_5)
    txtMAT.Add(txtMAT_6)
    '
    '
    txtLCH = New List(Of TextBox)
    txtLCH.Add(txtLCH_0)
    txtLCH.Add(txtLCH_1)
    txtLCH.Add(txtLCH_2)
    txtLCH.Add(txtLCH_3)
    txtLCH.Add(txtLCH_4)
    txtLCH.Add(txtLCH_5)
    '
    '
    '
    '
    '
    '
    '
    'B-Wert in Liste übernehmen
    '
    SqlStmt = "SELECT * FROM TBL_BWERT"
    CmdBWE.CommandText = SqlStmt
    If Not FillDatset(AdaptBWE, TblBWE) Then
      Exit Sub
    End If
    TblBWE.AcceptChanges()
    If TblBWE.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_BWERT")
    End If ''
    cboBWE.DataSource = TblBWE
    cboBWE.DisplayMember = "BWERT_BEZ"
    cboBWE.ValueMember = "BWERT_ID"
    '
    '
    '
    '
    'Liste für Absolutwerte Bezug
    '
    cboABS.Items.Clear()
    cboABS.Items.Add(New ListTextID(0, Texxt(3)))
    cboABS.Items.Add(New ListTextID(1, Texxt(4)))
    cboABS.DisplayMember = "TEXT"
    cboABS.ValueMember = "ID"
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    'Rechenverfahren für Farbstärkebestimmung in Liste übernehmen
    '
    SqlStmt = "SELECT * FROM TBL_FSTAE"
    CmdFST.CommandText = SqlStmt
    If Not FillDatset(AdaptFST, TblFST) Then
      Exit Sub
    End If
    Call UpdateLangText(TblFST, 450, "FSTAE_ID", "", "FSTAE_BEZ")
    TblFST.AcceptChanges()
    If TblFST.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_FSTAE")
    End If
    cboFST.DataSource = TblFST
    cboFST.DisplayMember = "FSTAE_BEZ"
    cboFST.ValueMember = "FSTAE_ID"
    '
    '
    '
    'TBL_LCHG Tabelle für Gewichte DL,DC,DH
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_LCHG ORDER BY LCHG_ID"
    CmdLCH.CommandText = SqlStmt
    If Not FillDatset(AdaptLCH, TblLCH) Then
      Exit Sub
    End If
    TblLCH.AcceptChanges()
    If TblLCH.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_LCHG")
    End If
    '
    '
    '
    '
    '
    '
    '
    '
    'TBL_MENU Menuparameter  
    '
    '
    SqlStmt = "SELECT * FROM TBL_MENU ORDER BY MENU_ID"
    CmdMEN.CommandText = SqlStmt
    If Not FillDatset(AdaptMEN, TblMEN) Then
      Exit Sub
    End If
    TblMEN.AcceptChanges()
    If TblMEN.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_MENU")
    End If '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    'TBL_MATPA mathematische Parameter
    '
    SqlStmt = "SELECT * FROM TBL_MATPA ORDER BY MATPA_ID"
    CmdMAT.CommandText = SqlStmt
    If Not FillDatset(AdaptMAT, TblMAT) Then
      Exit Sub
    End If
    TblMAT.AcceptChanges()
    If TblMAT.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_MATPA")
    End If '
    ''
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    
    If Not Iopenart Then
      BindingLCH.AddNewItem.Visible = False
      BindingMEN.AddNewItem.Visible = False
      BindingMAT.AddNewItem.Visible = False

      For i = 0 To txtLCH.Count - 1
        txtLCH(i).Enabled = False
      Next i
      For i = 0 To txtMAT.Count - 1
        txtMAT(i).Enabled = False
      Next i
      For i = 0 To txtMEN.Count - 1
        txtMEN(i).Enabled = False
      Next i
      BindingUSE.Visible = False
      BindingMET.Visible = False
    End If
    '
    '
    'Benutzer
    '
    ' 
    '
    If MnUserid = -1 Then
      SqlStmt = "SELECT DISTINCTROW TBL_USER.USER_ID AS USER_ID,USER_NAME FROM TBL_USER INNER JOIN TBL_USER_METH ON TBL_USER.USER_ID=TBL_USER_METH.USER_ID ORDER BY USER_NAME"

      'SqlStmt = "SELECT * FROM TBL_USER"
    Else
      SqlStmt = "SELECT * FROM TBL_USER WHERE [USER_ID]=" & MnUserid
    End If
    CmdUSE.CommandText = SqlStmt
    'datGKW.Recordset = DBas.OpenRecordset(SqlStmt, dbOpenDynaset, dbConsistent, dbReadOnly)
    If Not FillDatset(AdaptUSE, TblUSE) Then
      Exit Sub
    End If
    TblUSE.AcceptChanges()
    If TblUSE.Rows.Count = 0 Then
      MsgBox(Texxt(2993))
      Exit Sub
    Else
      StrLinUse = StrLin(TblUSE, "USER_ID")
    End If
    '
    '
    '
    '
    'Methoden
    '
    If MnMethID = -1 Then
      SqlStmt = "SELECT USER_ID,METH_BEZ,TBL_USER_METH.METH_ID AS METH_ID,MENU_ID,SOND_ID,LCHG_ID,KURV_ID,MATPA_ID,AUSG_ID,BW_ID,FS_ID,ABS_ID" & _
      " FROM TBL_USER_METH,TBL_METH WHERE [USER_ID] IN" & StrLinUse & _
      " AND TBL_USER_METH.METH_ID=TBL_METH.METH_ID ORDER BY TBL_USER_METH.METH_ID"
    Else
      SqlStmt = "SELECT USER_ID,METH_BEZ,TBL_USER_METH.METH_ID AS METH_ID,MENU_ID,SOND_ID,LCHG_ID,KURV_ID,MATPA_ID,AUSG_ID,BW_ID,FS_ID,ABS_ID" & _
      " FROM TBL_USER_METH,TBL_METH WHERE [USER_ID] IN" & StrLinUse & _
      " AND TBL_USER_METH.METH_ID=TBL_METH.METH_ID AND TBL_USER_METH.METH_ID=" & MnMethID & _
      " ORDER BY TBL_USER_METH.METH_ID"
    End If ' 
    '
    CmdMET.CommandText = SqlStmt
    If Not FillDatset(AdaptMET, TblMET) Then
      Exit Sub
    End If
    Call UpdateLangText(TblMET, 1800, "METH_ID", "", "METH_BEZ")
    TblMET.AcceptChanges()
    If TblMET.Rows.Count = 0 Then
      MsgBox(Texxt(2993))
      Exit Sub
    Else
      StrLinMet = StrLin(TblMET, "METH_ID")
    End If
    '
    '
    '
    '
    '
    'Recordset aller Normlichtarten
    '
    SqlStmt = "SELECT * FROM TBL_LICHT ORDER BY LICHT_ID"
    CmdNOR.CommandText = SqlStmt
    If Not FillDatset(AdaptNOR, TblNOR) Then
      Exit Sub
    End If
    TblNOR.AcceptChanges()
    If TblNOR.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_LICHT")
    End If
    lstNOR.DataSource = ViewNOR
    lstNOR.DisplayMember = "LICHT_KBEZ"
    lstNOR.ValueMember = "LICHT_ID"
    '
    '
    '
    'User/Methoden spezifische Licharten
    DataGridCombo = New DataGridViewComboBoxColumn
    DataGridCombo.DisplayStyle = DataGridViewComboBoxDisplayStyle.Nothing
    DataGridCombo.DisplayStyleForCurrentCellOnly = DataGridViewComboBoxDisplayStyle.ComboBox
    For i = 0 To GewWert.Length - 1
      DataGridCombo.Items.Add(GewWert(i))
    Next i
    DatNOA.Columns.Clear()
    DatNOA.Columns.Add("KBEZ", Texxt(173))
    DataGridCombo.Name = "GEW"
    DataGridCombo.HeaderText = Texxt(172)
    DatNOA.Columns.Add(DataGridCombo)
    DatNOA.Columns.Add("ID", " ")
    DatNOA.DataSource = ViewNOA
    DatNOA.Columns(0).DataPropertyName = "LICHT_KBEZ"
    DatNOA.Columns(1).DataPropertyName = "NORM_GEW"
    DatNOA.Columns(2).DataPropertyName = "LICHT_ID"
    DatNOA.Columns(2).Visible = False
    DatNOA.AutoGenerateColumns = False
    DatNOA.AllowUserToAddRows = False
    DatNOA.AllowUserToDeleteRows = False
    DatNOA.ScrollBars = ScrollBars.Vertical
    '
    '
    '
    '
    SqlStmt = "SELECT USER_ID,METH_ID,TBL_USER_METH_LICHT.LICHT_ID AS LICHT_ID,POS_ID,NORM_GEW,LICHT_KBEZ" _
    & " FROM TBL_USER_METH_LICHT INNER JOIN TBL_LICHT ON TBL_USER_METH_LICHT.LICHT_ID=TBL_LICHT.LICHT_ID " _
    & " ORDER BY POS_ID"
    CmdNOA.CommandText = SqlStmt
    If Not FillDatset(AdaptNOA, TblNOA) Then
      Exit Sub
    End If
    TblNOA.AcceptChanges()
    If TblNOA.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_USER_METH_LICHT")
    End If
    ViewNOA.Sort = "POS_ID"

    '
    
    '
    '
    '
    ConnLCH.DataSource = ViewLCH
    ConnMEN.DataSource = ViewMEN
    ConnMAT.DataSource = ViewMAT
    ConnMET.DataSource = ViewMET
    ConnUSE.DataSource = ViewUSE '
    '
    cboUSE.DataSource = ConnUSE
    cboUSE.DisplayMember = "USER_NAME"
    cboUSE.ValueMember = "USER_ID"
    '
    '
    '
    cboMET.DataSource = ConnMET
    cboMET.DisplayMember = "METH_BEZ"
    cboMET.ValueMember = "METH_ID" '
    '
    '
    lblUSE.DataBindings.Add("TEXT", ConnUSE, "USER_NAME")
    lblMET.DataBindings.Add("TEXT", ConnMET, "METH_BEZ")
    '
    '
    txtMEN_0.DataBindings.Add("TEXT", ConnMEN, "MENU_ID")
    txtMEN_1.DataBindings.Add("TEXT", ConnMEN, "MENU_BEZ")
    txtMEN_1.MaxLength = TblMEN.Columns("MENU_BEZ").MaxLength
    txtMEN_7.DataBindings.Add("TEXT", ConnMEN, "VERH_WEI_SCHW")
    txtMEN_8.DataBindings.Add("TEXT", ConnMEN, "DE_SCHWELL")
    txtMEN_9.DataBindings.Add("TEXT", ConnMEN, "FOPT")
    txtMEN_4.DataBindings.Add("TEXT", ConnMEN, "DE_W_S")
    txtMEN_5.DataBindings.Add("TEXT", ConnMEN, "GEW_DE_W_S")
    txtMEN_6.DataBindings.Add("TEXT", ConnMEN, "NR_IHRM_DE")
    txtMEN_2.DataBindings.Add("TEXT", ConnMEN, "JABST")
    txtMEN_3.DataBindings.Add("TEXT", ConnMEN, "CMCC_GEW")
    '
    '
    '
    txtLCH_0.DataBindings.Add("TEXT", ConnLCH, "LCHG_ID")
    txtLCH_1.DataBindings.Add("TEXT", ConnLCH, "LGEW")
    txtLCH_2.DataBindings.Add("TEXT", ConnLCH, "CGEW")
    txtLCH_3.DataBindings.Add("TEXT", ConnLCH, "HGEW")
    txtLCH_4.DataBindings.Add("TEXT", ConnLCH, "UGEW_W")
    txtLCH_5.DataBindings.Add("TEXT", ConnLCH, "UGEW_S")
    '
    '
    '
    txtMAT_0.DataBindings.Add("TEXT", ConnMAT, "MATPA_ID")
    txtMAT_1.DataBindings.Add("TEXT", ConnMAT, "MATPA_ITM")
    txtMAT_2.DataBindings.Add("TEXT", ConnMAT, "MATPA_FTO")
    txtMAT_3.DataBindings.Add("TEXT", ConnMAT, "MATPA_ATO")
    txtMAT_4.DataBindings.Add("TEXT", ConnMAT, "MATPA_DEL")
    txtMAT_5.DataBindings.Add("TEXT", ConnMAT, "MATPA_SPR")
    txtMAT_6.DataBindings.Add("TEXT", ConnMAT, "MATPA_EXPO")




   
    cboBWE.DataBindings.Add("SELECTEDVALUE", ConnMET, "BW_ID")
    cboFST.DataBindings.Add("SELECTEDVALUE", ConnMET, "FS_ID")
    cboABS.DataBindings.Add("SELECTEDINDEX", ConnMET, "ABS_ID")


  End Sub

  Private Sub frmNOW_Paint(sender As Object, e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint
    TabNOW.ItemSize = New Size(0.25 * Me.Width - 20, TabNOW.ItemSize.Height)

  End Sub
  WriteOnly Property openart() As Boolean
    Set(ByVal value As Boolean)
      Iopenart = value
    End Set
  End Property
  WriteOnly Property UserID() As Integer
    Set(ByVal value As Integer)
      MnUserid = value
    End Set
  End Property
  WriteOnly Property MessgID() As Integer
    Set(ByVal value As Integer)
      MnMessgID = value
    End Set
  End Property

  WriteOnly Property MethID() As Integer
    Set(ByVal value As Integer)
      MnMethID = value
    End Set
  End Property
  WriteOnly Property MischID() As Integer
    Set(ByVal value As Integer)
      MnMischid = value
    End Set
  End Property

  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
    Iopenart = True
    MnUserid = -1
    MnMessgID = -1
    MnMethID = -1
    MnMischid = -1
  End Sub

  Private Sub ConnUSE_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnUSE.CurrentChanged
    '
    If ConnUSE.Current Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    BindingUSE.Enabled = False

    '
    'Userabhängige Methode auswählen
    '

    'MsgBox SQLSTmt, 0
    If Not IsNothing(ConnMET.Current) Then
      ConnMET.Current.endedit()
    End If
    ViewMET.RowFilter = "USER_ID=" & ConnUSE.Current("User_id")
    If ViewMET.Count = 0 Then
      imsg = MsgBox(Texxt(3600), 0, Texxt(2000))
    End If
    ConnMET.CurrencyManager.Refresh()
    ConnMET.Position = 0

    BindingUSE.Enabled = True


  End Sub
  Private Sub ConnMET_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMET.CurrentChanged
    '
    If ConnUSE.Current Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    If ConnMET.Current Is Nothing OrElse ConnMET.Count = 0 Then Exit Sub

    BindingMET.Enabled = False
    '
    '
    '
    'Recordset der User-Methoden-abhängigen Normlichtarten
    '
    '
    '
    '
    ViewNOA.RowFilter = "USER_ID=" & ConnUSE.Current("User_id") & " AND METH_ID=" & ConnMET.Current("METH_ID")

    '
    '
     
    '
    '
    'Menu
    '
    '
    '
    ConnMEN.Position = ConnMEN.Find("MENU_ID", ConnMET.Current("MENU_ID"))
    '
    '
    '
    '
    'LCH
    '
    ConnLCH.Position = ConnLCH.Find("LCHG_ID", ConnMET.Current("LCHG_ID"))

    '
    '
    '
    '
    '
    'MATPA
    '
    '
    ConnMAT.Position = ConnMAT.Find("MATPA_ID", ConnMET.Current("MATPA_ID"))
    '
    '
    '   
    '
    '
    '
    '
    '
    BindingMET.Enabled = True
  End Sub


  Private Sub lstNOR_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstNOR.Click
    Dim i As Integer
    Dim RowView As DataRowView
    If lstNOR.SelectedIndex < 0 Then Exit Sub
    If ViewNOA.Count >= 5 Then
      MessageBox.Show(Texxt(3998), Texxt(2000), MessageBoxButtons.OK)
      Exit Sub
    End If
    'If IsEmpty(dblWIN.SelectedItem) Or LenB(dblWIN.SelectedItem) = 0 Then
    ' Exit Sub
    ' End If

    For i = 0 To ViewNOA.Count - 1
      If lstNOR.SelectedValue = ViewNOA(i)("LICHT_ID") Then
        Exit Sub
      End If
    Next
    '
    '
    '
    RowView = ViewNOA.AddNew()
    RowView("USER_ID") = ConnUSE.Current("USER_ID")
    RowView("METH_ID") = ConnMET.Current("METH_ID")
    RowView("LICHT_ID") = lstNOR.SelectedValue
    RowView("POS_ID") = ViewNOA.Count - 1
    RowView("NORM_GEW") = 1.0
    RowView("LICHT_KBEZ") = lstNOR.SelectedItem("LICHT_KBEZ")
    RowView.EndEdit()

  End Sub

   

  Private Sub DatNOA_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles DatNOA.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call DatNOA_KeyDown(sender, ev)
  End Sub
  Private Sub DatNOA_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles DatNOA.KeyDown
    Dim i As Integer
    If DatNOA.RowCount <= 1 Then Exit Sub
    For i = DatNOA.CurrentCell.RowIndex + 1 To DatNOA.RowCount - 1
      ViewNOA(i)("POS_ID") = ViewNOA(i)("POS_ID") - 1
    Next
    ViewNOA.Delete(DatNOA.CurrentCell.RowIndex)
  End Sub

  Private Sub ConnLCH_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnLCH.AddingNew
    For i = 0 To TblLCH.Columns.Count - 1
      TblLCH.Columns(i).DefaultValue = ConnLCH.Current(i)
    Next i
    MaxLchID = MaxDatTableID(TblLCH, "LCHG_ID", {""}, {-1}) + 1
    TblLCH.Columns("LCHG_ID").DefaultValue = MaxLchID
    '
  End Sub

  Private Sub ConnLCH_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnLCH.CurrentChanged
    If Not ConnMET.Current Is Nothing AndAlso ConnMET.Count <> 0 Then
      ConnMET.Current("LCHG_ID") = ConnLCH.Current("LCHG_ID")
    End If
  End Sub

  Private Sub ConnMAT_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnMAT.AddingNew

    For i = 0 To TblMAT.Columns.Count - 1
      TblMAT.Columns(i).DefaultValue = ConnMAT.Current(i)
    Next i
    MaxMatID = MaxDatTableID(TblMAT, "MATPA_ID", {""}, {-1}) + 1
    TblMAT.Columns("MATPA_ID").DefaultValue = MaxMatID
    '
  End Sub
  Private Sub ConnMAT_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMAT.CurrentChanged
    If Not ConnMET.Current Is Nothing AndAlso ConnMET.Count <> 0 Then
      ConnMET.Current("MATPA_ID") = ConnMAT.Current("MATPA_ID")
    End If
  End Sub

  Private Sub ConnMEN_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnMEN.AddingNew
    For i = 0 To TblMEN.Columns.Count - 1
      TblMEN.Columns(i).DefaultValue = ConnMEN.Current(i)
    Next i
    MaxMenID = MaxDatTableID(TblMEN, "MENU_ID", {""}, {-1}) + 1
    TblMEN.Columns("MENU_ID").DefaultValue = MaxMenID
    '
    TblMEN.Columns("MENU_BEZ").DefaultValue = TblMEN.Columns("MENU_BEZ").DefaultValue & " ????"
    If TblMEN.Columns("MENU_BEZ").DefaultValue.length > TblMEN.Columns("MENU_BEZ").MaxLength Then
      TblMEN.Columns("MENU_BEZ").DefaultValue = TblMEN.Columns("MENU_BEZ").DefaultValue.substring(0, TblMEN.Columns("MENU_BEZ").MaxLength)
    End If
  End Sub

  Private Sub ConnMEN_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMEN.CurrentChanged
    If Not ConnMET.Current Is Nothing AndAlso ConnMET.Count <> 0 Then
      ConnMET.Current("MENU_ID") = ConnMEN.Current("MENU_ID")
    End If
  End Sub

  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    Dim WhereKeyID() As String
   

    If AddDelP(2990) Then
      '
      '
      '
      '
      'TBL_USER_METH_LICHT
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptNOA.InsertCommand = OleDBInsertCmd("TBL_USER_METH_LICHT", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "LICHT_ID"
      AdaptNOA.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_LICHT", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptNOA.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_LICHT", WhereKeyID, Cncol)
      '
      '

      '
      '
      '
      'TBL_MENU
      '
      '
      '
      '
      'nur DIN6174 DIN6176 und CMC(Colli) erlaubt
      '
      '
      If CInt(ConnMEN.Current("JABST")) > 2 Or CInt(ConnMEN.Current("JABST")) < 0 Then
        ConnMEN.Current("JABST") = 0
      End If
      '
      ConnMEN.CurrencyManager.EndCurrentEdit()
      '
      '
      'Insertcommand

      '
      '
      AdaptMEN.InsertCommand = OleDBInsertCmd("TBL_MENU", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "MENU_ID"
      AdaptMEN.UpdateCommand = OleDBUpdateCmd("TBL_MENU", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptMEN.DeleteCommand = OleDBDeleteCmd("TBL_MENU", WhereKeyID, Cncol)
      '
      '
      '
      '
      '
      '
      'TBL_LCHG
      '
      ConnLCH.CurrencyManager.EndCurrentEdit()
      '
      '
      'Insertcommand

      '
      '
      AdaptLCH.InsertCommand = OleDBInsertCmd("TBL_LCHG", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "LCHG_ID"
      AdaptLCH.UpdateCommand = OleDBUpdateCmd("TBL_LCHG", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptLCH.DeleteCommand = OleDBDeleteCmd("TBL_LCHG", WhereKeyID, Cncol)
      '
      '
      '
      '
      'TBL_MATPA
      '
      ConnMAT.CurrencyManager.EndCurrentEdit()
      '
      '
      'Insertcommand

      '
      '
      AdaptMAT.InsertCommand = OleDBInsertCmd("TBL_MATPA", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "MATPA_ID"
      AdaptMAT.UpdateCommand = OleDBUpdateCmd("TBL_MATPA", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptMAT.DeleteCommand = OleDBDeleteCmd("TBL_MATPA", WhereKeyID, Cncol)
      '
      ''
      '
      '
      '
      '
      'TBL_USER_METH
      '
      '
      '
      '
      ConnMET.CurrencyManager.EndCurrentEdit()
      '
      '
      'Insertcommand

      '
      '
      AdaptMET.InsertCommand = OleDBInsertCmd("TBL_USER_METH", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      AdaptMET.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptMET.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH", WhereKeyID, Cncol)
      '
      '
      '
      '
      '
      'Delete TBL_USER_METH_LICHT
      '
      AdaptNOA.Update(TblNOA.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Delete TBL_USER_METH
      '
      AdaptMET.Update(TblMET.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      '
      'Delete TBL_MENU
      '
      AdaptMEN.Update(TblMEN.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Delete TBL_LCHG
      '
      AdaptLCH.Update(TblLCH.Select(Nothing, Nothing, DataViewRowState.Deleted)) '
      '
      'Delete TBL_MATPA
      '
      AdaptMAT.Update(TblMAT.Select(Nothing, Nothing, DataViewRowState.Deleted)) '
      '
      '
      'Insert TBL_MATPA
      '
      '
      AdaptMAT.Update(TblMAT.Select(Nothing, Nothing, DataViewRowState.Added)) '
      ''
      '
      'Insert TBL_LCHG
      '
      '
      AdaptLCH.Update(TblLCH.Select(Nothing, Nothing, DataViewRowState.Added)) '
      ''
      '
      'Insert TBL_MENU
      '
      '
      AdaptMEN.Update(TblMEN.Select(Nothing, Nothing, DataViewRowState.Added)) '
      ''
      'Insert TBL_USER_METH
      '
      '
      AdaptMET.Update(TblMET.Select(Nothing, Nothing, DataViewRowState.Added)) '
      '
      '
      'Insert TBL_USER_METH_LICHT
      '
      '
      AdaptNOA.Update(TblNOA.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Update TBL_MATPA
      '
      AdaptMAT.Update(TblMAT.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_LCHG
      '
      AdaptLCH.Update(TblLCH.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_MENU
      '
      AdaptMEN.Update(TblMEN.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      '
      'Update TBL_USER_METH_LICHT
      '
      AdaptNOA.Update(TblNOA.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_USER_METH
      '
      AdaptMET.Update(TblMET.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      '
      '
      '
      '
    End If
    Me.Close()
    Me.Dispose()
  End Sub
End Class