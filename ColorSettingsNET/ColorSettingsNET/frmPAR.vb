Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmPAR
  REM Laufvariable
  Dim i As Integer
  Dim l As Integer
  Dim k As Integer
  Dim j As Integer
  Dim kw As Integer
  REM Zwischengroessen
  Dim nkw As Integer
  'Message Kennung
  Dim imsg As Integer           'Messagebox (Rückgabewert)
  Dim MaxID As Long             'Maximale ID (Primärschlüssel)
  Dim ier As Integer
  Dim MnUserid As Integer
  Dim MnMethID As Integer
  Dim Iopenart As Boolean
  Dim HandleParameter As HandleParamMerk
  Dim SqlStmt As String
  Dim StrLinUse As String
  '
  '
  Dim TblUSE As DataTable
  Dim TblMET As DataTable
  '
  Dim SCmdUSE As OleDbCommand
  Dim SCmdMET As OleDbCommand
  '
  '
  '
  '
  '
  Dim AdaptUSE As OleDbDataAdapter
  Dim AdaptMET As OleDbDataAdapter
  '
  '
  '
  '
  Dim ViewUse As DataView
  Dim ViewMET As DataView
  '
  '
  Dim WithEvents ConnUSE As BindingSource
  Dim WithEvents ConnMET As BindingSource
  Dim DataGridCombo As DataGridViewComboBoxColumn


  Private Sub frmPAR_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    '
    Me.Text = Texxt(415)
    btnORD.Text = Texxt(1999)
    lblPAR.Text = Texxt(201)
    lblMTH.Text = Texxt(421)
    '
    '
    '
    TblUSE = New DataTable
    TblMet = New DataTable
    '
    '
    SCmdUSE = New OleDbCommand("", Cncol)
    SCmdMET = New OleDbCommand("", Cncol)
    '
    '
    '
    AdaptUSE = New OleDbDataAdapter
    AdaptMET = New OleDbDataAdapter
    '
    '
    '
    ViewUse = New DataView(TblUSE)
    ViewMET = New DataView(TblMET)
    '
    '
    ConnUSE = New BindingSource
    ConnMET = New BindingSource
    '
    AdaptUSE.SelectCommand = SCmdUSE
    AdaptMET.SelectCommand = SCmdMET

    '
    '
    ConnUSE = New BindingSource
    ConnMET = New BindingSource
    '
    '
    '
    '
    '
    '
    '
    BindingUSE.BindingSource = ConnUSE
    BindingMET.BindingSource = ConnMET
    '
    '
    '
    '
    '
    HandleParameter = New HandleParamMerk(flgParameter, flgDropParameter, Nothing)
    '
    '
    '
    '
    '
    '
    '
    '
    'USER
    '
    If MnUserid = -1 Then
      SqlStmt = "SELECT * FROM TBL_USER"
    Else
      SqlStmt = "SELECT * FROM TBL_USER WHERE [USER_ID]=" & MnUserid
    End If

    If Not Iopenart Then
      BindingUSE.Visible = False
      BindingMET.Visible = False
    End If
    '
    SCmdUSE.CommandText = SqlStmt
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
    ''
    '
    '
    '
    'Methoden
    '

    '
    'Userabhängige Methode auswählen


    If MnMethID = -1 Then
      SqlStmt = "SELECT DISTINCT TBL_USER_METH.METH_ID, TBL_USER_METH.USER_ID,TBL_METH.METH_BEZ AS METH_BEZ" _
      & " FROM (TBL_METH_ANWSG INNER JOIN (TBL_METH INNER JOIN TBL_USER_METH ON TBL_METH.METH_ID = TBL_USER_METH.METH_ID)" _
      & " ON TBL_METH_ANWSG.METH_ID = TBL_METH.METH_ID)" _
      & " INNER JOIN TBL_ANWSG ON TBL_METH_ANWSG.ANWSG_ID = TBL_ANWSG.ANWSG_ID" _
      & " WHERE TBL_USER_METH.USER_ID IN " & StrLinUse _
      & " ORDER BY TBL_USER_METH.METH_ID"
    Else
      SqlStmt = "SELECT USER_ID,METH_BEZ,TBL_USER_METH.METH_ID AS METH_ID FROM TBL_USER_METH,TBL_METH WHERE [USER_ID] IN " & StrLinUse & _
      " AND TBL_USER_METH.METH_ID=TBL_METH.METH_ID AND TBL_USER_METH.METH_ID=" & MnMethID
    End If
    '
    SCmdMET.CommandText = SqlStmt
    If Not FillDatset(AdaptMET, TblMet) Then
      Exit Sub
    End If
    Call UpdateLangText(TblMET, 1800, "METH_ID", "", "METH_BEZ")

    TblMet.AcceptChanges()
    If TblMet.Rows.Count = 0 Then
      imsg = MsgBox(Texxt(3600), 0, Texxt(2000))
      Exit Sub
    End If
    '
    '
    '
    '
    ConnMET.DataSource = ViewMET
    ConnUSE.DataSource = TblUSE
    lblUSE.DataBindings.Add("TEXT", ConnUSE, "USER_NAME")
    lblMET.DataBindings.Add("TEXT", ConnMET, "METH_BEZ")
    Application.DoEvents()


  End Sub
  '
  '
  '
  '


  Private Sub ConnMET_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMET.CurrentChanged
    If ConnUSE.Current Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    If ConnMET.Current Is Nothing OrElse ConnMET.Count = 0 Then Exit Sub

    BindingMET.Enabled = False
    '
    '
    HandleParameter.FillDatabase()
    HandleParameter.FillGrid(ConnUSE.Current("USER_ID"), ConnMET.Current("METH_ID"))
    '
    '
    ViewMET.RowFilter = "USER_ID=" & ConnUSE.Current("User_id")
    If ViewMET.Count = 0 Then
      imsg = MsgBox(Texxt(3600), 0, Texxt(2000))
    End If
    '
    '
    BindingMET.Enabled = True
    ''
  End Sub


  Private Sub ConnUSE_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnUSE.CurrentChanged
    '
    If ConnUSE.Current Is Nothing OrElse ConnUSE.Count = 0 Then Exit Sub
    BindingUSE.Enabled = False

    '
    'Userabhängige Methode auswählen
    '
    If Not IsNothing(ConnMET.Current) Then
      ConnMET.Current.endedit()
    End If
    ViewMET.RowFilter = "USER_ID=" & ConnUSE.Current("User_id")
    If ViewMET.Count = 0 Then
      imsg = MsgBox(Texxt(3600), 0, Texxt(2000))
    End If
    ConnMET.EndEdit()
    ConnMET.Position = 0

    BindingUSE.Enabled = True


  End Sub


  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    HandleParameter.FillDatabase()
    Me.Close()
    Me.Dispose()
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


  WriteOnly Property MethID() As Integer
    Set(ByVal value As Integer)
      MnMethID = value
    End Set
  End Property


  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
    Iopenart = True
    MnUserid = -1
    MnMethID = -1
  End Sub




  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub

End Class

