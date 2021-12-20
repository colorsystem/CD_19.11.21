Option Compare Text
Option Explicit On
Option Strict Off
Class frmMMR
  '
  Dim MnUserid As Integer
  Dim MnMethID As Integer
  '
  Dim StrLinUse As String
  Dim StrLinMet As String
  '
  Dim MnIopenArt As Boolean
  Dim MnUserUfo As Boolean
  '
  '
  '
  Dim TblUSE As DataTable
  Dim TblMET As DataTable
  Dim TblANWSG As DataTable
  Dim TblMerk As DataTable
  Dim TblUsMeAnMerk As DataTable
  '
  '
  '
  '
  Dim ViewMET As DataView
  Dim ViewANWSG As DataView
  Dim ViewMerk As List(Of DataView)
  Dim ViewUsMeAnMerk As DataView
  '
  '
  '
  '
  Dim AdaptUSE As OleDbDataAdapter
  Dim AdaptMET As OleDbDataAdapter
  Dim AdaptANWSG As OleDbDataAdapter
  Dim AdaptMerk As OleDbDataAdapter
  Dim AdaptUsMeAnMerk As OleDbDataAdapter
  '
  '
  '
  '
  Dim CmdUSE As OleDbCommand
  Dim CmdMET As OleDbCommand
  Dim CmdANWSG As OleDbCommand
  Dim CmdMerk As OleDbCommand
  Dim CmdUsMeAnMerk As OleDbCommand
  '
  '
  '
  Dim WithEvents ConnUSE As BindingSource
  Dim WithEvents ConnMET As BindingSource
  Dim cboMER As List(Of ComboBox)
  Dim lblFRM As List(Of Label)
  Dim lblMER As List(Of Label)








  Dim SpaIndex As Integer
  Dim SpaltNR As Integer
  Dim SqlStmt As String
  'Anzahl Reflexionskurven (+Zusatzinformation) QU-Kontrolle
  '
  Dim Nqu As Integer
  Dim ier As Integer
  Dim Npam As Integer
  Dim Iprn As Integer
  
  '
  '
  '

  REM Laufvariable
  Dim i As Integer
  Dim l As Integer
  Dim k As Integer
  Dim j As Integer
  Dim kw As Integer
  REM Zwischengroessen
  Dim nkw As Integer
  'Message Kennung
  Dim imsg As Integer            'Messagebox (Rückgabewert)
  '
  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
    MnIopenart = True
    MnUserid = -1
    MnMethID = -1
  End Sub '
  Private Sub frmMMR_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    Me.Text = Texxt(414)
    lblZEI.Text = Texxt(511)
    lblMMR.Text = Texxt(201)
    lblMTH.Text = Texxt(421)
    lblFAK.Text = Texxt(240)
    lblKBEZ.Text = Texxt(241)
    lblFORM.Text = Texxt(242)
    btnORD.Text = Texxt(1999)
    '
    '
    '
    cboMER = New List(Of ComboBox)()
    cboMER.Add(cboMER_00)
    cboMER.Add(cboMER_01)
    cboMER.Add(cboMER_02)
    cboMER.Add(cboMER_03)
    cboMER.Add(cboMER_04)
    cboMER.Add(cboMER_05)
    cboMER.Add(cboMER_06)
    cboMER.Add(cboMER_07)
    cboMER.Add(cboMER_08)
    cboMER.Add(cboMER_09)
    cboMER.Add(cboMER_10)
    cboMER.Add(cboMER_11)
    cboMER.Add(cboMER_12)
    cboMER.Add(cboMER_13)
    cboMER.Add(cboMER_14)
    cboMER.Add(cboMER_15)
    cboMER.Add(cboMER_16)
    cboMER.Add(cboMER_17)
    cboMER.Add(cboMER_18)
    cboMER.Add(cboMER_19)
    cboMER.Add(cboMER_20)
    cboMER.Add(cboMER_21)
    cboMER.Add(cboMER_22)
    cboMER.Add(cboMER_23)
    '
    '
    '
    '
    lblMER = New List(Of Label)
    lblMER.Add(lblMER_00)
    lblMER.Add(lblMER_01)
    lblMER.Add(lblMER_02)
    lblMER.Add(lblMER_03)
    lblMER.Add(lblMER_04)
    lblMER.Add(lblMER_05)
    lblMER.Add(lblMER_06)
    lblMER.Add(lblMER_07)
    lblMER.Add(lblMER_08)
    lblMER.Add(lblMER_09)
    lblMER.Add(lblMER_10)
    lblMER.Add(lblMER_11)
    lblMER.Add(lblMER_12)
    lblMER.Add(lblMER_13)
    lblMER.Add(lblMER_14)
    lblMER.Add(lblMER_15)
    lblMER.Add(lblMER_16)
    lblMER.Add(lblMER_17)
    lblMER.Add(lblMER_18)
    lblMER.Add(lblMER_19)
    lblMER.Add(lblMER_20)
    lblMER.Add(lblMER_21)
    lblMER.Add(lblMER_22)
    lblMER.Add(lblMER_23)


    lblFRM = New List(Of Label)
    lblFRM.add(lblFRM_00)
    lblFRM.add(lblFRM_01)
    lblFRM.add(lblFRM_02)
    lblFRM.add(lblFRM_03)
    lblFRM.add(lblFRM_04)
    lblFRM.add(lblFRM_05)
    lblFRM.add(lblFRM_06)
    lblFRM.add(lblFRM_07)
    lblFRM.add(lblFRM_08)
    lblFRM.add(lblFRM_09)
    lblFRM.add(lblFRM_10)
    lblFRM.add(lblFRM_11)
    lblFRM.add(lblFRM_12)
    lblFRM.add(lblFRM_13)
    lblFRM.add(lblFRM_14)
    lblFRM.add(lblFRM_15)
    lblFRM.add(lblFRM_16)
    lblFRM.add(lblFRM_17)
    lblFRM.add(lblFRM_18)
    lblFRM.add(lblFRM_19)
    lblFRM.add(lblFRM_20)
    lblFRM.add(lblFRM_21)
    lblFRM.add(lblFRM_22)
    lblFRM.add(lblFRM_23)
    '
    '
    '
    For i = 16 To cboMER.Count - 1
      cboMER(i).Visible = False
      lblMER(i).Visible = False
      lblFRM(i).Visible = False
    Next
    For i = 0 To lblMER.Count - 1
      lblMER(i).Text = ""
      lblFRM(i).Text = ""
    Next
    '
    '
    '
    '
    '
    '
    TblUSE = New DataTable
    TblMET = New DataTable
    TblANWSG = New DataTable
    TblMerk = New DataTable
    TblUsMeAnMerk = New DataTable
    '
    '
    '
    '
    ViewMET = New DataView(TblMET)
    ViewANWSG = New DataView(TblANWSG)
    ViewUsMeAnMerk = New DataView(TblUsMeAnMerk)
    ViewMerk = New List(Of DataView)
    '
    '
    '
    AdaptUSE = New OleDbDataAdapter
    AdaptMET = New OleDbDataAdapter
    AdaptANWSG = New OleDbDataAdapter
    AdaptMerk = New OleDbDataAdapter
    AdaptUsMeAnMerk = New OleDbDataAdapter
    '
    '
    '
    '
    CmdUSE = New OleDbCommand("", Cncol)
    CmdMET = New OleDbCommand("", Cncol)
    CmdANWSG = New OleDbCommand("", Cncol)
    CmdMerk = New OleDbCommand("", Cncol)
    CmdUsMeAnMerk = New OleDbCommand("", Cncol)
    '
    '
    '
    AdaptUSE.SelectCommand = CmdUSE
    AdaptMET.SelectCommand = CmdMET
    AdaptANWSG.SelectCommand = CmdANWSG
    AdaptMerk.SelectCommand = CmdMerk
    AdaptUsMeAnMerk.SelectCommand = CmdUsMeAnMerk

    '
    '
    ConnUSE = New BindingSource
    ConnMET = New BindingSource
    '
    '
    BindingUSE.BindingSource = ConnUSE
    BindingMET.BindingSource = ConnMET
    '
    '
    If Not MnIopenArt Then
      BindingUSE.Visible = False
      BindingMET.Visible = False
    End If
    '
    'USER
    '
    If MnUserid = -1 Then
      'SqlStmt = "SELECT * FROM TBL_USER"
      SqlStmt = "SELECT DISTINCTROW TBL_USER.USER_ID AS USER_ID,USER_NAME FROM TBL_USER INNER JOIN TBL_USER_METH ON TBL_USER.USER_ID=TBL_USER_METH.USER_ID"
    Else
      SqlStmt = "SELECT * FROM TBL_USER WHERE [USER_ID]=" & MnUserid
    End If
    '
    CmdUSE.CommandText = SqlStmt
    If Not FillDatset(AdaptUSE, TblUSE) Then
      Exit Sub
    End If
    TblUSE.AcceptChanges()
    If TblUSE.Rows.Count = 0 Then
      MsgBox(Texxt(3506) & ": " & "TBL_USER")
      Exit Sub
    End If
    StrLinUse = StrLin(TblUSE, "USER_ID")
    '
    '
    '
    '
    'Userabhängige Methode auswählen
    '
    If MnMethID = -1 Then
      SqlStmt = "SELECT TBL_USER_METH.USER_ID, TBL_USER_METH.METH_ID AS METH_ID,METH_BEZ FROM TBL_METH INNER JOIN TBL_USER_METH ON TBL_METH.METH_ID=TBL_USER_METH.METH_ID" _
      & " WHERE TBL_USER_METH.USER_ID IN " & StrLinUse & " AND TBL_USER_METH.METH_ID IN (SELECT METH_ID FROM TBL_USER_METH_ANWSG WHERE USER_ID IN " & StrLinUse & ")"
      'MsgBox SqlStmt
    Else
      SqlStmt = "SELECT TBL_USER_METH.USER_ID, TBL_USER_METH.METH_ID AS METH_ID,METH_BEZ FROM TBL_METH INNER JOIN TBL_USER_METH ON TBL_METH.METH_ID=TBL_USER_METH.METH_ID" _
      & " WHERE TBL_USER_METH.METH_ID=" & MnMethID & " AND TBL_USER_METH.USER_ID IN " & StrLinUse & " AND TBL_USER_METH.METH_ID IN (SELECT METH_ID FROM TBL_USER_METH_ANWSG WHERE USER_ID IN " & StrLinUse & ")"
    End If
    SqlStmt = SqlStmt & " ORDER BY TBL_USER_METH.METH_ID"
    CmdMET.CommandText = SqlStmt
    If Not FillDatset(AdaptMET, TblMET) Then
      Exit Sub
    End If
    Call UpdateLangText(TblMET, 1800, "METH_ID", "", "METH_BEZ")
    TblMET.AcceptChanges()
    If TblMET.Rows.Count = 0 Then
      imsg = MsgBox(Texxt(3600), 0, Texxt(2000))
      Exit Sub
    End If
    StrLinMet = StrLin(TblMET, "METH_ID")
    '
    'StrLinUse = "(1)"
    'StrLinMet = "(2)"
    '
    '
    'USER-Methoden-abhängige Anweisungen
    '
    '
    '
    '
    '
    SqlStmt = "SELECT DISTINCT TBL_ANWSG.ANWSG_ID, TBL_ANWSG.ANWSG_KBEZ,TBL_USER_METH_ANWSG.USER_ID,TBL_USER_METH_ANWSG.METH_ID,ZEIL_NR" _
    & " FROM TBL_ANWSG INNER JOIN TBL_USER_METH_ANWSG ON TBL_ANWSG.ANWSG_ID = TBL_USER_METH_ANWSG.ANWSG_ID" _
    & " WHERE TBL_USER_METH_ANWSG.USER_ID IN " & StrLinUse & " AND TBL_USER_METH_ANWSG.METH_ID IN " & StrLinMet
    '
    '
    CmdANWSG.CommandText = SqlStmt
    If Not FillDatset(AdaptANWSG, TblANWSG) Then
      Exit Sub
    End If
    Call UpdateLangText(TblANWSG, 30000, "ANWSG_ID", "ANWSG_KBEZ", "")
    TblANWSG.AcceptChanges()
    ViewANWSG.Sort = "ZEIL_NR"
    '
    '
    '
    '
    '
    'Tabelle aller Anweisungsabhängigen Merkmale
    '
    '
    '
    SqlStmt = "SELECT ANWSG_ID,TBL_ANWSG_MERK.MERK_ID AS MERK_ID,MERK_KBEZ,AUSW_ID,MERK_LBEZ,MERK_KEN,MERK_TYP,MERK_FORM,MERK_FAKT FROM TBL_ANWSG_MERK INNER JOIN TBL_MERK ON TBL_ANWSG_MERK.MERK_ID=TBL_MERK.MERK_ID ORDER BY TBL_ANWSG_MERK.MERK_ID"
    '
    '
    CmdMerk.CommandText = SqlStmt
    If Not FillDatset(AdaptMerk, TblMerk) Then
      Exit Sub
    End If
    Call UpdateLangText(TblMerk, 10000, "MERK_ID", "MERK_KBEZ", "MERK_LBEZ")
    TblMerk.AcceptChanges()

    '
    '
    'Tabelle TBL_USER_METH_ANWSG_MERK
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_USER_METH_ANWSG_MERK"
    '
    '
    CmdUsMeAnMerk.CommandText = SqlStmt
    If Not FillDatset(AdaptUsMeAnMerk, TblUsMeAnMerk) Then
      Exit Sub
    End If
    TblUsMeAnMerk.AcceptChanges()
    ViewUsMeAnMerk.Sort = "SPALT_NR"
    '

    For i = 0 To cboMER.Count - 1
      ViewMerk.Add(New DataView(TblMerk))
      cboMER(i).DataSource = ViewMerk(i)
      cboMER(i).DisplayMember = "MERK_KBEZ"
      cboMER(i).ValueMember = "MERK_ID"
    Next

    lstANW.DataSource = ViewANWSG
    lstANW.DisplayMember = "ANWSG_KBEZ"
    lstANW.ValueMember = "ANWSG_ID"
    '
    ConnMET.DataSource = ViewMET
    ConnUSE.DataSource = TblUSE
    '
    '
    lblUSE.DataBindings.Add("TEXT", ConnUSE, "USER_NAME")
    lblMET.DataBindings.Add("TEXT", ConnMET, "METH_BEZ")
    txtKBEZ.MaxLength = TblUsMeAnMerk.Columns("MERK_KBEZ").MaxLength
    txtFORM.MaxLength = TblUsMeAnMerk.Columns("MERK_FORM").MaxLength
    '
    '
    '
    '
    cboUSE.DataSource = ConnUSE
    cboUSE.ValueMember = "USER_ID"
    cboUSE.DisplayMember = "USER_NAME"
    '
    cboMET.DataSource = ConnMET
    cboMET.ValueMember = "METH_ID"
    cboMET.DisplayMember = "METH_BEZ"


    '
    ' 
  End Sub


  
  WriteOnly Property openart() As Boolean
    Set(ByVal value As Boolean)
      MnIopenArt = value
    End Set
  End Property

  WriteOnly Property UserID() As Integer
    Set(ByVal value As Integer)
      MnUserid = value
    End Set
  End Property
  WriteOnly Property MethID() As Integer
    Set(ByVal value As Integer)
      MnMethID = value
    End Set
  End Property

  Private Sub ConnUSE_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnUSE.CurrentChanged
    '
    '
    '
    If ConnUSE Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    '
    '
    'Userabhängige Methode auswählen
    '
    '
    '
    '
    If Not IsNothing(ConnMET.Current) Then
      ConnMET.Current.endedit()
    End If
    ViewMET.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID")
    If ViewMET.Count = 0 Then
      imsg = MsgBox(Texxt(3600), 0, Texxt(2000))
      Exit Sub
    Else
      ConnMET.CurrencyManager.Refresh()
      ConnMET.Position = 0
      '
      '
    End If

  End Sub

  Private Sub ConnMET_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMET.CurrentChanged
    '
    'Recordset der Methoden-abhängigen Anweisungen
    '
    '
    If ConnUSE Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    If ConnMET Is Nothing OrElse ConnMET.Count = 0 Then Exit Sub
    '
    '
    'Liste der user und methodenabhängigen Anweisungen
    '
    '
    '
    ViewANWSG.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID") & " AND METH_ID=" & ConnMET.Current("METH_ID")
    '
    lstANW.Refresh()

    lstANW.SelectedIndex = -1
    If lstANW.Items.Count > 0 Then
      lstANW.SelectedIndex = 0
    End If
  End Sub

 
  
  Private Sub lstANW_SelectedValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstANW.SelectedValueChanged
    Dim i As Integer
    Dim SpaNr As Integer
    If lstANW.SelectedValue Is Nothing OrElse Not IsNumeric(lstANW.SelectedValue) Then Exit Sub
    For i = 0 To ViewMerk.Count - 1
      cboMER(i).Enabled = False
      ViewMerk(i).RowFilter = "ANWSG_ID=" & lstANW.SelectedValue
      cboMER(i).SelectedIndex = -1
      lblMER(i).Visible = False
      lblFRM(i).Visible = False
      lblMER(i).Text = ""
      lblFRM(i).Text = ""
      cboMER(i).Enabled = True
    Next
    If ConnUSE Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    If ConnMET Is Nothing OrElse ConnMET.Count = 0 Then Exit Sub
    If lstANW.Items.Count = 0 Then Exit Sub
    ViewUsMeAnMerk.RowFilter = "USER_ID=" & ConnUSE.Current("USER_ID") & " AND METH_ID=" & ConnMET.Current("METH_ID") & " AND ANWSG_ID=" & lstANW.SelectedValue
    For i = 0 To ViewUsMeAnMerk.Count - 1
      SpaNr = ViewUsMeAnMerk(i)("SPALT_NR")
      lblMER(SpaNr - 1).Text = ""
      lblFRM(SpaNr - 1).Text = ""
      If IsDBNull(ViewUsMeAnMerk(i)("MERK_KBEZ")) OrElse Trim(ViewUsMeAnMerk(i)("MERK_KBEZ")) = "" Then
        lblMER(SpaNr - 1).Text = ""
      Else
        lblMER(SpaNr - 1).Text = ViewUsMeAnMerk(i)("MERK_KBEZ")
      End If
      If IsDBNull(ViewUsMeAnMerk(i)("MERK_FORM")) OrElse Trim(ViewUsMeAnMerk(i)("MERK_FORM")) = "" Then
        lblFRM(SpaNr - 1).Text = ""
      Else
        lblFRM(SpaNr - 1).Text = ViewUsMeAnMerk(i)("MERK_FORM")
      End If

      cboMER(SpaNr - 1).SelectedValue = ViewUsMeAnMerk(i)("MERK_ID")
    Next
    SpaIndex = -1
    SpaltNR = -1
    txtKBEZ.Text = ""
    txtFORM.Text = ""
    txtFAK.Text = ""
    lblMRK.Text = ""
    lblFORM.BackColor = Color.FromArgb(255, 240, 240, 240)
    lblKBEZ.BackColor = Color.FromArgb(255, 240, 240, 240)


  End Sub
 
  Private Sub cboMER_SelectedValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _
  cboMER_00.SelectedValueChanged, cboMER_01.SelectedValueChanged, cboMER_02.SelectedValueChanged, cboMER_03.SelectedValueChanged, cboMER_04.SelectedValueChanged, cboMER_05.SelectedValueChanged, cboMER_06.SelectedValueChanged, cboMER_07.SelectedValueChanged, _
  cboMER_08.SelectedValueChanged, cboMER_09.SelectedValueChanged, cboMER_10.SelectedValueChanged, cboMER_11.SelectedValueChanged, cboMER_12.SelectedValueChanged, cboMER_13.SelectedValueChanged, cboMER_14.SelectedValueChanged, cboMER_15.SelectedValueChanged, _
  cboMER_16.SelectedValueChanged, cboMER_17.SelectedValueChanged, cboMER_18.SelectedValueChanged, cboMER_19.SelectedValueChanged, cboMER_20.SelectedValueChanged, cboMER_21.SelectedValueChanged, cboMER_22.SelectedValueChanged, cboMER_23.SelectedValueChanged
    Dim RowView As DataRowView
    Dim i As Integer
    Dim SpaNr As Integer
    Dim inde As Integer
    If ConnUSE Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    If ConnMET Is Nothing OrElse ConnMET.Count = 0 Then Exit Sub
    If lstANW.SelectedValue Is Nothing OrElse lstANW.Items.Count = 0 Then Exit Sub
    SpaNr = CInt(sender.name.substring(7, 2))
    If cboMER(SpaNr).SelectedIndex = -1 Then Exit Sub
    If Not cboMER(SpaNr).Enabled Then Exit Sub
    If IsNothing(cboMER(SpaNr).SelectedValue) OrElse Not IsNumeric(cboMER(SpaNr).SelectedValue) Then
      Exit Sub
    End If
    inde = ViewUsMeAnMerk.Find(SpaNr + 1)

    If cboMER(SpaNr).SelectedItem("MERK_ID") = 0 And inde >= 0 Then
      lblMRK.Text = ""
      txtKBEZ.Text = ""
      txtFORM.Text = ""
      txtFAK.Text = ""
      cboMER(SpaNr).SelectedIndex = -1
      ViewUsMeAnMerk(inde).Delete()
      Exit Sub
    End If

    'Prüfen, ob Merkmal bereits ausgewählt
    '
    '
    For i = 0 To ViewUsMeAnMerk.Count - 1
      If i = inde Then
       
        Continue For
      End If
      If ViewUsMeAnMerk(i)("USER_ID") = ConnUSE.Current("USER_ID") And _
          ViewUsMeAnMerk(i)("METH_ID") = ConnMET.Current("METH_ID") And _
          ViewUsMeAnMerk(i)("ANWSG_ID") = lstANW.SelectedValue And _
          ViewUsMeAnMerk(i)("MERK_ID") = cboMER(SpaNr).SelectedItem("MERK_ID") Then
        cboMER(SpaNr).SelectedIndex = -1
        MsgBox(Texxt(3013))
        Exit Sub
      End If
    Next

    If inde > -1 AndAlso ViewUsMeAnMerk(inde)("MERK_ID") <> cboMER(SpaNr).SelectedItem("MERK_ID") Then
      '
      'Merkmale sind verschieden
      '
      '
      ViewUsMeAnMerk(inde).Delete()
      inde = -1
      '
    End If
    If inde = -1 Then
      '
      'Neues Merkmal hinzufügen
      '
      RowView = ViewUsMeAnMerk.AddNew
      RowView("USER_ID") = ConnUSE.Current("USER_ID")
      RowView("METH_ID") = ConnMET.Current("METH_ID")
      RowView("ANWSG_ID") = lstANW.SelectedValue
      RowView("MERK_ID") = cboMER(SpaNr).SelectedItem("MERK_ID")
      RowView("SPALT_NR") = SpaNr + 1
      RowView("MERK_FORM") = ""
      RowView("MERK_KBEZ") = ""
      RowView.EndEdit()
      inde = ViewUsMeAnMerk.Find(SpaNr + 1)
    Else

      ViewUsMeAnMerk(inde)("SPALT_NR") = SpaNr + 1
      ViewUsMeAnMerk(inde)("MERK_ID") = cboMER(SpaNr).SelectedItem("MERK_ID")
    End If
    lblMRK.Text = Texxt(10000 + cboMER(SpaNr).SelectedValue)
    If IsDBNull(ViewUsMeAnMerk(inde)("Merk_Form")) OrElse Trim(ViewUsMeAnMerk(inde)("Merk_Form")) = "" Then
      txtFORM.Text = cboMER(SpaNr).SelectedItem("MERK_FORM")
    Else
      txtFORM.Text = ViewUsMeAnMerk(inde)("Merk_Form")
    End If
    If IsDBNull(ViewUsMeAnMerk(inde)("Merk_KBEZ")) OrElse Trim(ViewUsMeAnMerk(inde)("Merk_KBEZ")) = "" Then
      txtKBEZ.Text = TexKt(10000 + cboMER(SpaNr).SelectedValue)
    Else
      txtKBEZ.Text = ViewUsMeAnMerk(inde)("Merk_KBEZ")
    End If

    txtFAK.Text = cboMER(SpaNr).SelectedItem("MERK_FAKT")
    SpaIndex = inde
    SpaltNR = SpaNr
  End Sub

  
 


 






  Private Sub txtKBEZ_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtKBEZ.Leave
   
    If SpaIndex = -1 Or SpaltNR = -1 Then Exit Sub
    lblMER(SpaltNR).Text = txtKBEZ.Text
    ViewUsMeAnMerk(SpaIndex)("MERK_KBEZ") = txtKBEZ.Text
  End Sub

  Private Sub txtFORM_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtFORM.Leave
    
    If SpaIndex = -1 Or SpaltNR = -1 Then Exit Sub
    lblFRM(SpaltNR).Text = txtFORM.Text
    ViewUsMeAnMerk(SpaIndex)("MERK_FORM") = txtFORM.Text
  End Sub

  Private Sub lblFORM_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblFORM.Click
    Dim i As Integer
    For i = 0 To 15
      lblMER(i).Visible = False
      lblFRM(i).Visible = True
    Next i
    lblKBEZ.BackColor = Color.FromArgb(255, 240, 240, 240)
    lblFORM.BackColor = Color.FromArgb(255, 185, 209, 234)
  End Sub
  Private Sub lblKBEZ_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblKBEZ.Click
    Dim i As Integer
    For i = 0 To 15
      lblMER(i).Visible = True
      lblFRM(i).Visible = False
    Next i
    lblFORM.BackColor = Color.FromArgb(255, 240, 240, 240)
    lblKBEZ.BackColor = Color.FromArgb(255, 185, 209, 234)
  End Sub


  Private Sub lblZEI_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lblZEI.DoubleClick
    Dim i As Integer
    If lstANW.SelectedIndex = -1 Then Exit Sub
    ViewANWSG(lstANW.SelectedIndex)("ZEIL_NR") = ViewANWSG.Count + 1
    For i = lstANW.SelectedIndex + 1 To lstANW.Items.Count - 1
      ViewANWSG(i)("ZEIL_NR") = i
    Next
    ViewANWSG(lstANW.SelectedIndex)("ZEIL_NR") = ViewANWSG.Count
    lstANW.SelectedValue = ViewANWSG(ViewANWSG.Count - 1)("ANWSG_ID")
    lstANW.SelectedValue = ViewANWSG(0)("ANWSG_ID")
    lstANW.SelectedValue = ViewANWSG(ViewANWSG.Count - 1)("ANWSG_ID")
  End Sub

  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    Dim WhereKeyID() As String
    If AddDelP(2990) Then
      '
      'TBL_USER_METH_ANWSG
      '

      '
      '
      'Insertcommand
      TblUsMeAnMerk.EndInit()
      TblANWSG.EndInit()
      '
      '
      AdaptANWSG.InsertCommand = OleDBInsertCmd("TBL_USER_METH_ANWSG", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(2)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "ANWSG_ID"
      AdaptANWSG.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_ANWSG", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptANWSG.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_ANWSG", WhereKeyID, Cncol)
      '
      '
      '
      '
      '
      'TBL_USER_METH_ANWSG_MERK
      '
      '
      '
      '
      '
      '
      'Insertcommand

      '
      '
      AdaptUsMeAnMerk.InsertCommand = OleDBInsertCmd("TBL_USER_METH_ANWSG_MERK", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(3)
      WhereKeyID(0) = "USER_ID"
      WhereKeyID(1) = "METH_ID"
      WhereKeyID(2) = "ANWSG_ID"
      WhereKeyID(3) = "SPALT_NR"
      AdaptUsMeAnMerk.UpdateCommand = OleDBUpdateCmd("TBL_USER_METH_ANWSG_MERK", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptUsMeAnMerk.DeleteCommand = OleDBDeleteCmd("TBL_USER_METH_ANWSG_MERK", WhereKeyID, Cncol)
      '
      '
      'Delete/Update/Insert TBL_USER_METH_ANWSG_MERK und TBL_USER_METH_ANWSG

      'Delete  TBL_USER_METH_ANWSG_MERK
      '
      AdaptUsMeAnMerk.Update(TblUsMeAnMerk.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Delete TBL_USER_METH_ANWSG
      '
      AdaptANWSG.Update(TblANWSG.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      'Insert TBL_USER_METH_ANWSG
      '
      '
      AdaptANWSG.Update(TblANWSG.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Insert TBL_USER_METH_ANWSG_MERK
      '
      '
      AdaptUsMeAnMerk.Update(TblUsMeAnMerk.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      'Update TBL_USER_METH_ANWSG
      '
      AdaptANWSG.Update(TblANWSG.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_USER_METH_ANWSG_MERK
      '
      AdaptUsMeAnMerk.Update(TblUsMeAnMerk.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
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