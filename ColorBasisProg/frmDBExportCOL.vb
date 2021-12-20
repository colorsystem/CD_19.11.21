Option Strict Off
Option Explicit On
Option Compare Text
Public Class frmDBExportCOL
  Inherits System.Windows.Forms.Form
  Dim imsg As Short
  Dim ier As Short
  Dim i As Short
  Dim j As Short
  Dim k As Short
  Dim kw As Short
  Dim HandleRezept As HandleRezGeneral
  Dim RwrtGroup As HandleRwerte
  Dim DBHilfName As String
  Dim CnDBHilf As OleDbConnection
  Dim SqlStmt As String
  Dim Strgid As String
  Dim TblListZeig As String
  Dim TblListFarbm As String
  Dim TblListRezept As String
  Dim TblListSorti As String
  Dim TblListRwert As String
  Dim DynCmd As OleDbCommand
  Dim DynAdapt As OleDbDataAdapter
  Dim btnRUN As List(Of Button)
  Dim btnMARK As List(Of Button)
  Dim btnZeig As List(Of Button)
  Dim lblGRP As List(Of System.Windows.Forms.Label)
  Dim lblANZ As List(Of System.Windows.Forms.Label)
  Dim lblSQL As List(Of System.Windows.Forms.Label)
  Dim lblDAT As List(Of System.Windows.Forms.Label)
  Dim lblVON As List(Of System.Windows.Forms.Label)
  Dim lblBIS As List(Of System.Windows.Forms.Label)
  Dim txtSQL As List(Of TextBox)
  Dim txtVON As List(Of TextBox)
  Dim txtBIS As List(Of TextBox)
  Dim txtANZ As List(Of TextBox)
  Dim chkARC As List(Of CheckBox)
  Dim SplitCONT As List(Of SplitContainer)
  Dim btnExport As List(Of Button)
  Dim dbgEXP As List(Of DataGridView)
  Dim TabExp As List(Of DataTable)

  Private Sub frmDBExportCOL_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
    AufbauPar.MethID = -1
  End Sub

  Private Sub frmDBExportCOL_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    CnDBHilf.Close()
    CnDBHilf.Dispose()
    DynCmd.Dispose()
  End Sub

  Private Sub frmDBExportCOL_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
    'TblListZeig
    '
    '
    '
    If TableExists(TblListZeig, CnTmp) Then
      SqlStmt = "DROP INDEX [REZEPT_ID] ON " & TblListZeig
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
      SqlStmt = "DROP TABLE " & TblListZeig
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    '
    '
    'TblListFarbm
    '
    '
    '
    If TableExists(TblListFarbm, CnTmp) Then
      SqlStmt = "DROP INDEX [FARBM_ID] ON " & TblListFarbm
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
      SqlStmt = "DROP TABLE " & TblListFarbm
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    '
    '
    'TblListRezept
    '
    '
    '
    If TableExists(TblListRezept, CnTmp) Then
      SqlStmt = "DROP INDEX [REZEPT_ID] ON " & TblListRezept
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
      SqlStmt = "DROP TABLE " & TblListRezept
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    '
    '
    'TblListRwert
    '
    '
    '
    If TableExists(TblListRwert, CnTmp) Then
      SqlStmt = "DROP INDEX [RWERT_ID] ON " & TblListRwert
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
      SqlStmt = "DROP TABLE " & TblListRwert
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'TblListSorti
    '
    '
    '
    If TableExists(TblListSorti, CnTmp) Then
      SqlStmt = "DROP INDEX [SORTI_ID] ON " & TblListSorti
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
      SqlStmt = "DROP TABLE " & TblListSorti
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    ''
  End Sub
  '



  Private Sub frmDBExportCOL_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim j As Integer
    Dim i As Integer
    '
    Me.Text = Texxt(1910) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    lblQUEL.Text = Cndat.DataSource
    btnDBase.Text = Texxt(3619)
    lblQQQ.Text = Texxt(3620)
    lblZZZ.Text = Texxt(3619)
    chkGRD.Text = Texxt(3648)
    chkTYP.Text = Texxt(3656)
    chkSMP.Text = Texxt(3657)
    chkUUU.Text = Texxt(3658)

    '
    '
    btnRUN = New List(Of Button)
    btnMARK = New List(Of Button)
    btnZeig = New List(Of Button)
    lblANZ = New List(Of System.Windows.Forms.Label)
    lblSQL = New List(Of System.Windows.Forms.Label)
    lblDAT = New List(Of System.Windows.Forms.Label)
    lblVON = New List(Of System.Windows.Forms.Label)
    lblBIS = New List(Of System.Windows.Forms.Label)
    txtSQL = New List(Of TextBox)
    txtVON = New List(Of TextBox)
    txtBIS = New List(Of TextBox)
    txtANZ = New List(Of TextBox)
    SplitCONT = New List(Of SplitContainer)
    btnExport = New List(Of Button)
    dbgEXP = New List(Of DataGridView)
    chkARC = New List(Of CheckBox)
    TabExp = New List(Of DataTable)
    TabExp.Add(Nothing)
    TabExp.Add(New DataTable)
    TabExp.Add(New DataTable)
    TabExp.Add(New DataTable)
    TabExp.Add(New DataTable)

    '
    '
    btnRUN1.Text = Texxt(3693)
    btnRUN2.Text = Texxt(3693)
    btnRUN3.Text = Texxt(3693)
    btnRUN4.Text = Texxt(3693)
    '
    '
    btnRUN.Add(Nothing)
    btnRUN.Add(btnRUN1)
    btnRUN.Add(btnRUN2)
    btnRUN.Add(btnRUN3)
    btnRUN.Add(btnRUN4)
    '
    '
    '
    btnMARK1.Text = Texxt(3670)
    btnMARK2.Text = Texxt(3670)
    btnMARK3.Text = Texxt(3670)
    btnMARK4.Text = Texxt(3670)
    '
    '
    btnMARK.Add(Nothing)
    btnMARK.Add(btnMARK1)
    btnMARK.Add(btnMARK2)
    btnMARK.Add(btnMARK3)
    btnMARK.Add(btnMARK4)
    '
    '
    '
    btnZEIG1.Text = Texxt(3690)
    btnZeig2.Text = Texxt(3690)
    btnZeig3.Text = Texxt(3690)
    btnZeig4.Text = Texxt(3690)
    '
    '
    btnZeig.Add(Nothing)
    btnZeig.Add(btnZEIG1)
    btnZeig.Add(btnZeig2)
    btnZeig.Add(btnZeig3)
    btnZeig.Add(btnZeig4)
    '
    '
    '
    lblSQL1.Text = Texxt(369)
    lblSQL2.Text = Texxt(369)
    lblSQL3.Text = Texxt(369)
    lblSQL4.Text = Texxt(369)
    '
    '
    lblSQL.Add(Nothing)
    lblSQL.Add(lblSQL1)
    lblSQL.Add(lblSQL2)
    lblSQL.Add(lblSQL3)
    lblSQL.Add(lblSQL4)
    '
    '
    txtSQL.Add(Nothing)
    txtSQL.Add(txtSQL1)
    txtSQL.Add(txtSQL2)
    txtSQL.Add(txtSQL3)
    txtSQL.Add(txtSQL4)
    '
    '
    '
    lblANZ1.Text = Texxt(3689)
    lblANZ2.Text = Texxt(3689)
    lblANZ3.Text = Texxt(3689)
    lblANZ4.Text = Texxt(3689)
    '
    '
    lblANZ.Add(Nothing)
    lblANZ.Add(lblANZ1)
    lblANZ.Add(lblANZ2)
    lblANZ.Add(lblANZ3)
    lblANZ.Add(lblANZ4)
    '
    '
    txtANZ.Add(Nothing)
    txtANZ.Add(txtANZ1)
    txtANZ.Add(txtANZ2)
    txtANZ.Add(txtANZ3)
    txtANZ.Add(txtANZ4)
    '
    '
    '
    '
    '
    '
    lblGRP2.Text = Texxt(386)
    lblGRP3.Text = Texxt(386)
    '
    '

    '
    '
    '
    lblDAT2.Text = Texxt(375)
    lblDAT3.Text = Texxt(375)
    '
    '
    lblDAT.Add(Nothing)
    lblDAT.Add(Nothing)
    lblDAT.Add(lblDAT2)
    lblDAT.Add(lblDAT3)
    '
    '
    '
    lblBIS2.Text = Texxt(377)
    lblBIS3.Text = Texxt(377)
    '
    '
    lblBIS.Add(Nothing)
    lblBIS.Add(Nothing)
    lblBIS.Add(lblBIS2)
    lblBIS.Add(lblBIS3)
    '
    '
    '
    lblVON2.Text = Texxt(376)
    lblVON3.Text = Texxt(376)
    Application.DoEvents()
    '
    '
    lblVON.Add(Nothing)
    lblVON.Add(Nothing)
    lblVON.Add(lblVON2)
    lblVON.Add(lblVON3)
    ''
    txtBIS.Add(Nothing)
    txtBIS.Add(Nothing)
    txtBIS.Add(txtBIS2)
    txtBIS.Add(txtBIS3)
    '
    '
    txtVON.Add(Nothing)
    txtVON.Add(Nothing)
    txtVON.Add(txtVON2)
    txtVON.Add(txtVON3)
    For j = 2 To 3
      '
      txtBIS(j).Text = Format(Today)
      txtVON(j).Text = Format(DateAdd(DateInterval.Day, -1 * 365, Today))
    Next j '
    '
    SplitCONT.Add(Nothing)
    SplitCONT.Add(SplitCont1)
    SplitCONT.Add(SplitCont2)
    SplitCONT.Add(SplitCont3)
    SplitCONT.Add(SplitCont4)
    '
    '
    btnExport0.Text = Texxt(3619)
    btnExport1.Text = Texxt(3621)
    btnExport2.Text = Texxt(3622)
    btnExport3.Text = Texxt(3623)
    btnExport4.Text = Texxt(3624)
    '
    '
    btnExport.Add(btnExport0)
    btnExport.Add(btnExport1)
    btnExport.Add(btnExport2)
    btnExport.Add(btnExport3)
    btnExport.Add(btnExport4)
    '
    '
    btnExport(0).Enabled = True
    '
    dbgEXP.Add(Nothing)
    dbgEXP.Add(dbgEXP1)
    dbgEXP.Add(dbgEXP2)
    dbgEXP.Add(dbgEXP3)
    dbgEXP.Add(dbgEXP4)
    '
    '
    '
    chkARC2.Text = Texxt(374)
    chkARC3.Text = Texxt(374)
    '
    '
    chkARC.Add(Nothing)
    chkARC.Add(Nothing)
    chkARC.Add(chkARC2)
    chkARC.Add(chkARC3)
    For i = 1 To dbgEXP.Count - 1
      dbgEXP(i).DataSource = TabExp(i)
      dbgEXP(i).SelectionMode = DataGridViewSelectionMode.FullRowSelect
      dbgEXP(i).Font = dbgEXP(i).DefaultCellStyle.Font
    Next
    '
    'TblListZeig
    '
    '
    '
    '
    DynAdapt = New OleDbDataAdapter
    DynCmd = New OleDbCommand
    DynAdapt.SelectCommand = DynCmd
    CnDBHilf = New OleDbConnection
    TblListZeig = "TBL_LIST_ZEIG" & CharTim()
    If TableExists(TblListZeig, CnTmp) Then
      SqlStmt = "DROP TABLE " & TblListZeig
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If

    SqlStmt = "CREATE TABLE " & TblListZeig & " ([REZEPT_ID] LONG CONSTRAINT [REZEPT_ID] PRIMARY KEY)"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    '
    'TblListFarbm
    '
    '
    '
    'Hilftabelle für Suche (Zwischenablage)
    '
    TblListFarbm = "TBL_LIST_FARBM" & CharTim()
    If TableExists(TblListFarbm, CnTmp) Then
      SqlStmt = "DROP TABLE " & TblListFarbm
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If

    SqlStmt = "CREATE TABLE " & TblListFarbm & " ([FARBM_ID] LONG CONSTRAINT [FARBM_ID] PRIMARY KEY)"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If


    '
    'TblListRezept
    '
    '
    '
    'Hilftabelle für Suche (Zwischenablage)
    '
    TblListRezept = "TBL_LIST_REZEPT" & CharTim()
    If TableExists(TblListRezept, CnTmp) Then
      SqlStmt = "DROP TABLE " & TblListRezept
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If

    SqlStmt = "CREATE TABLE " & TblListRezept & " ([REZEPT_ID] LONG CONSTRAINT [REZEPT_ID] PRIMARY KEY)"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If

    '
    '

    '
    'TblListRwert
    '
    '
    '
    'Hilftabelle für Suche (Zwischenablage)
    '
    TblListRwert = "TBL_LIST_RWERT" & CharTim()
    If TableExists(TblListRwert, CnTmp) Then
      SqlStmt = "DROP TABLE " & TblListRwert
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If

    SqlStmt = "CREATE TABLE " & TblListRwert & " ([RWERT_ID] LONG CONSTRAINT [RWERT_ID] PRIMARY KEY)"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If


    '
    'TblListSorti
    '
    '
    '
    'Hilftabelle für Suche (Zwischenablage)
    '
    TblListSorti = "TBL_LIST_SORTI" & CharTim()
    If TableExists(TblListSorti, CnTmp) Then
      SqlStmt = "DROP TABLE " & TblListSorti
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If

    SqlStmt = "CREATE TABLE " & TblListSorti & " ([SORTI_ID] LONG CONSTRAINT [SORTI_ID] PRIMARY KEY)"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    '
    RwrtGroup = New HandleRwerte
    '
    'Gruppe R-Werte
    '
    '
    RwrtGroup.cboGRP = cboGRP3
    RwrtGroup.lblGRP = lblGRP3
    RwrtGroup.GRoupList()

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
    'Gruppen für Rezepte
    '
    '
    If MenueParam.MischID > -1 Then
      HandleRezept = New HandleRezGeneral
      HandleRezept.cboGRP = cboGRP2
      HandleRezept.lblGRP = lblGRP2
      HandleRezept.GroupList()
    End If
  End Sub

  Public Sub New()
    Dim i As Integer
    Dim TableList As List(Of String)
    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.
    '
    '
    'Prüfen, ob CNTMP vorhanden
    '
    '
    '
    If Not File.Exists(CnTmp.DataSource) Then
      If Not CreateDatabase(CnTmp) Then
        MsgBox(Texxt(3760) & CnTmp.DataSource)
        Me.Close()
      End If
    End If
    TableList = New List(Of String)
    Call GetTabledefs(TableList, CnTmp)
    '
    '
    'Prüfen, ob alle Tabellen in Tabname
    '
    '
    DynCmd = New OleDbCommand("", CnTmp)
    For i = 0 To TableList.Count - 1
      If InStr(TableList(i), "TBL_LIST_FARBM") > 0 _
        Or InStr(TableList(i), "TBL_LIST_REZEPT") > 0 _
        Or InStr(TableList(i), "TBL_LIST_SORTI") > 0 _
        Or InStr(TableList(i), "TBL_LIST_RWERT") > 0 _
        Or InStr(TableList(i), "TBL_LIST_ZEIG") > 0 Then
        DynCmd.CommandText = "DROP TABLE " & TableList(i)
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If
      End If
    Next i

  End Sub

  Private Sub btnDBase_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnDBase.Click
    Dim OldCurdir As String = ""
StartDialog:
    Try
      OldCurdir = My.Computer.FileSystem.CurrentDirectory
    Catch
    End Try
    OpenFileDialogExport.FileName = NewFileName(CnTmp.DataSource, "COLHILF.MDB")
    OpenFileDialogExport.CheckFileExists = False
    If Not OpenFileDialogExport.ShowDialog() = DialogResult.OK Then
      Exit Sub
    End If
    DBHilfName = OpenFileDialogExport.FileName
    If DBHilfName = Cndat.DataSource Then
      MsgBox(Texxt(3637) & DBHilfName)
      Exit Sub
    End If
    If DBHilfName = Cncol.DataSource Then
      MsgBox(Texxt(3637) & DBHilfName)
      Exit Sub
    End If
    If DBHilfName = CnTmp.DataSource Then
      MsgBox(Texxt(3637) & DBHilfName)
      Exit Sub
    End If
    Try
      My.Computer.FileSystem.CurrentDirectory = OldCurdir
    Catch ex As Exception
    End Try '
    'Datenbank wird immerneu erstellt
    '
    '
    If File.Exists(DBHilfName) Then
      If MessageBox.Show(Texxt(3050) & ": " & DBHilfName & " " & Texxt(3051) & Chr(13) & Texxt(3052), Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
        File.Delete(DBHilfName)
      Else
        Exit Sub
      End If
    End If
    CnDBHilf.ConnectionString = NewConnectionstring(CnTmp.ConnectionString, DBHilfName)
    If Not CreateDatabase(CnDBHilf) Then
      MsgBox(Texxt(3670) & CnDBHilf.DataSource)
      GoTo startdialog
    End If
    '
    '
    '   
    '
    '
    'TBL_FARBM
    '
    '
    If Not TableExists("TBL_FARBM", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_FARBM IN '" & CnDBHilf.DataSource & "' FROM TBL_FARBM WHERE MISCH_ID=-1 AND FARBM_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'TBL_FARBM_PROZ
    '
    '
    If Not TableExists("TBL_FARBM_PROZ", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_FARBM_PROZ IN '" & CnDBHilf.DataSource & "' FROM TBL_FARBM_PROZ WHERE MISCH_ID=-1 AND FARBM_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'TBL_FARBM_PROB
    '
    '
    If Not TableExists("TBL_FARBM_PROB", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_FARBM_PROB IN '" & CnDBHilf.DataSource & "' FROM TBL_FARBM_PROB WHERE MISCH_ID=-1 AND FARBM_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'TBL_FARBM_PREIS
    '
    '
    If Not TableExists("TBL_FARBM_PREIS", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_FARBM_PREIS IN '" & CnDBHilf.DataSource & "' FROM TBL_FARBM_PREIS WHERE MISCH_ID=-1 AND FARBM_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'TBL_GRUND_FARBM
    '
    '
    If Not TableExists("TBL_GRUND_FARBM", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_GRUND_FARBM IN '" & CnDBHilf.DataSource & "' FROM TBL_GRUND_FARBM WHERE MISCH_ID=-1 AND FARBM_ID=-1 AND MESSGRW_ID=-1 AND GKWRT_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If '
    '
    '
    'TBL_RWERT
    '
    '
    If Not TableExists("TBL_RWERT", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_RWERT IN '" & CnDBHilf.DataSource & "' FROM TBL_RWERT WHERE MESSGRW_ID=-1 AND RWERT_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'TBL_REZEPT
    '
    '
    If Not TableExists("TBL_REZEPT", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_REZEPT IN '" & CnDBHilf.DataSource & "' FROM TBL_REZEPT WHERE MISCH_ID=-1 AND REZEPT_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    '
    'TBL_RWERTRZ
    '
    '
    If Not TableExists("TBL_RWERTRZ", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_RWERTRZ IN '" & CnDBHilf.DataSource & "' FROM TBL_RWERT WHERE MESSGRW_ID=-1 AND RWERT_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'TBL_REZEPT_FARBM
    '
    '
    If Not TableExists("TBL_REZEPT_FARBM", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_REZEPT_FARBM IN '" & CnDBHilf.DataSource & "' FROM TBL_REZEPT_FARBM WHERE MISCH_ID=-1 AND REZEPT_ID=-1 AND FARBM_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'TBL_REZEPT_RWERT
    '
    '
    If Not TableExists("TBL_REZEPT_RWERT", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_REZEPT_RWERT IN '" & CnDBHilf.DataSource & "' FROM TBL_REZEPT_RWERT WHERE MISCH_ID=-1 AND REZEPT_ID=-1 AND MESSGRW_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'TBL_SORTI
    '
    '
    If Not TableExists("TBL_SORTI", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_SORTI IN '" & CnDBHilf.DataSource & "' FROM TBL_SORTI WHERE MISCH_ID=-1 AND SORTI_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    '
    'TBL_RWERTSO
    '
    '
    If Not TableExists("TBL_RWERTSO", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_RWERTSO IN '" & CnDBHilf.DataSource & "' FROM TBL_RWERT WHERE MESSGRW_ID=-1 AND RWERT_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'TBL_SORTI_FARBM
    '
    '
    If Not TableExists("TBL_SORTI_FARBM", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_SORTI_FARBM IN '" & CnDBHilf.DataSource & "' FROM TBL_SORTI_FARBM WHERE MISCH_ID=-1 AND SORTI_ID=-1 AND FARBM_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'TBL_SORTI_RWERT
    '
    '
    If Not TableExists("TBL_SORTI_RWERT", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_SORTI_RWERT IN '" & CnDBHilf.DataSource & "' FROM TBL_SORTI_RWERT WHERE MISCH_ID=-1 AND SORTI_ID=-1 AND MESSGRW_ID=-1"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
        Exit Sub
      End If
    End If
    '
    '
    'Ziel-Datenbank wird zugeordnet
    '
    '
    '
    btnDBase.Enabled = False
    lblZiel.Text = DBHilfName
    RwrtGroup.cboGRP.Enabled = True
    RwrtGroup.cboGRP.Enabled = True
    btnZeig(1).Enabled = True
    btnZeig(2).Enabled = True
    btnZeig(3).Enabled = True
    btnZeig(4).Enabled = True
    txtVON(2).Enabled = True
    txtBIS(2).Enabled = True
    txtVON(3).Enabled = True
    txtBIS(3).Enabled = True


    '
    'Tabelle Messgeräte übernehmen (um später Prüfungen vorzunehmen)
    '
    '
    If TableExists("TBL_MESSG", CnDBHilf) Then
      SqlStmt = "DROP TABLE TBL_MESSG"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
    End If

    SqlStmt = "SELECT * INTO TBL_MESSG IN '" & CnDBHilf.DataSource & "' FROM TBL_MESSG"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
      Exit Sub
    End If
    '
    'Tabelle Messgeräte (Geometrien/Winkel) übernehmen (um später Prüfungen vorzunehmen)
    '
    '
    '
    If TableExists("TBL_MESSG_IHRM", CnDBHilf) Then
      SqlStmt = "DROP TABLE TBL_MESSG_IHRM"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
    End If

    SqlStmt = "SELECT * INTO TBL_MESSG_IHRM IN '" & CnDBHilf.DataSource & "' FROM TBL_MESSG_IHRM"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    'Tabelle GK-Werte übernehmen (um später Prüfungen vorzunehmen)
    '
    '
    '
    If TableExists("TBL_GKWRT", CnDBHilf) Then
      SqlStmt = "DROP TABLE TBL_GKWRT"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
    End If
    SqlStmt = "SELECT * INTO TBL_GKWRT IN '" & CnDBHilf.DataSource & "' FROM TBL_GKWRT"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
      Exit Sub
    End If
    '
    'Tabelle USER_MISCH_MESSG übernehmen (um später Prüfungen vorzunehmen)
    '
    '
    '
    If TableExists("TBL_USER_MISCH_MESSG", CnDBHilf) Then
      SqlStmt = "DROP TABLE TBL_USER_MISCH_MESSG"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
    End If
    SqlStmt = "SELECT * INTO TBL_USER_MISCH_MESSG IN '" & CnDBHilf.DataSource & "' FROM TBL_USER_MISCH_MESSG" & " WHERE USER_ID=" & MenueParam.UserID & " AND MISCH_ID=" & MenueParam.MischID & " AND MESSG_ID=" & MenueParam.MessgID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
      Exit Sub
    End If


    '
    'Tabelle Gruppen R-Werte übernehmen (um später Prüfungen vorzunehmen)
    '
    If TableExists("TBL_MESSG_GROUP", CnDBHilf) Then
      SqlStmt = "DROP TABLE TBL_MESSG_GROUP"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
    End If
    '
    '
    SqlStmt = "SELECT * INTO TBL_MESSG_GROUP IN '" & CnDBHilf.DataSource & "' FROM TBL_MESSG_GROUP"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
      Exit Sub
    End If
    '
    '
    'Tabelle Gruppen Rezepte übernehmen (um später Prüfungen vorzunehmen)
    '
    If TableExists("TBL_MISCH_GROUP", CnDBHilf) Then
      SqlStmt = "DROP TABLE TBL_MISCH_GROUP"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
    End If
    '
    '
    SqlStmt = "SELECT * INTO TBL_MISCH_GROUP IN '" & CnDBHilf.DataSource & "' FROM TBL_MISCH_GROUP"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    If MenueParam.MischID > -1 Then
      btnExport(1).Enabled = True
    End If
    btnExport(3).Enabled = True
  End Sub


  Private Sub btnZEIG_Click(ByVal sender As Object, ByVal e As System.EventArgs) _
  Handles btnZEIG1.Click, btnZeig2.Click, btnZeig3.Click, btnZeig4.Click

    Dim Index As Short = CShort(sender.Name.Substring(7, 1))
    If Index > 1 And Index < 4 Then
      If Not IsDate(txtVON(Index).Text) Then
        MsgBox(Texxt(2500))
        Cursor = System.Windows.Forms.Cursors.Arrow
        Exit Sub
      End If
      If Not IsDate(txtBIS(Index).Text) Then
        MsgBox(Texxt(2500))
        Cursor = System.Windows.Forms.Cursors.Arrow
        Exit Sub
      End If
    End If
    Me.Cursor = System.Windows.Forms.Cursors.WaitCursor
    dbgEXP(Index).AllowUserToAddRows = True
    dbgEXP(Index).AllowUserToDeleteRows = True
    TabExp(Index).Rows.Clear()
    Select Case Index

      Case 1
        '
        '
        '
        '
        'Aufbau der dbgEXP-Tabelle für Farbmittel
        '
        '
        '
        '
        btnExport(0).Enabled = False

        btnRUN(Index).Enabled = False
        btnMARK(Index).Enabled = False
        Application.DoEvents()
        dbgEXP(Index).Visible = True
        txtANZ(Index).Visible = False
        lblANZ(Index).Visible = False
        SqlStmt = "SELECT FARBM_ID,FARBM_DATTIM,FARBM_NAME,FARBM_PRNR FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID
        If Trim(txtSQL(Index).Text) <> "" Then
          SqlStmt = SqlStmt & " AND " & StrSelct("FARBM_NAME", txtSQL(Index).Text)
        End If
        '
        '
        SqlStmt = SqlStmt & " ORDER BY FARBM_ID"
        DynCmd.CommandText = SqlStmt
        DynCmd.Connection = Cndat()
        If Not FillDatset(DynAdapt, TabExp(Index)) Then
          MsgBox(Texxt(2973))
        Else
          txtANZ(Index).Visible = True
          lblANZ(Index).Visible = True
          txtANZ(Index).Text = CStr(CShort(TabExp(Index).Rows.Count))
        End If
        '

        dbgEXP(Index).Columns(0).Visible = False
        dbgEXP(Index).Columns(1).HeaderText = Texxt(375)
        dbgEXP(Index).Columns(2).HeaderText = Texxt(394)
        dbgEXP(Index).Columns(3).HeaderText = Texxt(917)

        '
        '
        '
        btnRUN(Index).Enabled = True
        btnMARK(Index).Enabled = True
        Me.Cursor = Cursors.Arrow
      Case 2
        '
        'Hilftabelle für Suche (Zwischenablage)
        '
        '
        '
        SqlStmt = "DELETE * FROM " & TblListZeig
        'Call ColwinParamLIBDBHilfProgramme_definst.SQLExeLock(1, Dtmp, SqlStmt, ier)
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If '
        '
        '
        '
        'RezeptID's für Rezepte die keine der ausgewählten Farbmittel enthalten
        '
        SqlStmt = "INSERT INTO " & TblListZeig & " IN '" & CnTmp.DataSource & "'" & " SELECT DISTINCT TBL_REZEPT_FARBM.REZEPT_ID AS REZEPT_ID FROM TBL_REZEPT_FARBM" & " INNER JOIN TBL_REZEPT ON TBL_REZEPT.REZEPT_ID=TBL_REZEPT_FARBM.REZEPT_ID AND " & " TBL_REZEPT.MISCH_ID=TBL_REZEPT_FARBM.MISCH_ID" & " WHERE TBL_REZEPT.MISCH_ID=" & MenueParam.MischID & " AND TBL_REZEPT_FARBM.FARBM_ID NOT IN " & " (SELECT FARBM_ID FROM TBL_FARBM IN '" & CnDBHilf.DataSource & "')"
        'Call ColwinParamLIBDBHilfProgramme_definst.SQLExeLock(1, Ddat, SqlStmt, ier)
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          Exit Sub
        End If
        '
        '
        'Aufbau der dbgEXP-Tabelle für Rezepte
        '
        '
        '
        '

        btnRUN(Index).Enabled = False
        btnMARK(Index).Enabled = False
        Application.DoEvents()
        dbgEXP(Index).Visible = True
        txtANZ(Index).Visible = False
        lblANZ(Index).Visible = False
        Strgid = ""
        If HandleRezept.cboGRP.SelectedIndex > 0 Then
          Strgid = "REZEPT_GID= " & HandleRezept.cboGRP.SelectedValue & " AND " & Strgid
        End If
        If chkARC(Index).CheckState <> 2 Then
          Strgid = Strgid & "REZEPT_IARCH=" & chkARC(Index).CheckState & " AND "
        End If
        SqlStmt = "SELECT REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME FROM TBL_REZEPT" & " WHERE MISCH_ID=" & MenueParam.MischID & " AND " & Strgid & "REZEPT_DATTIM BETWEEN " & Sqldati(CDate(txtVON(Index).Text)) & " AND " & Sqldati(DateAdd(DateInterval.Day, 1, CDate(txtBIS(Index).Text)))
        If Trim(txtSQL(Index).Text) <> "" Then
          SqlStmt = SqlStmt & " AND " & StrSelct("REZEPT_NAME", txtSQL(Index).Text)
        End If
        '
        '
        SqlStmt = SqlStmt & " AND REZEPT_ID NOT IN (SELECT REZEPT_ID FROM " & TblListZeig & " IN '" & CnTmp.DataSource & "')" & " ORDER BY REZEPT_NAME"
        DynCmd.CommandText = SqlStmt
        DynCmd.Connection = Cndat()
        If Not FillDatset(DynAdapt, TabExp(Index)) Then
          Exit Sub
        End If
        If TabExp(Index).Rows.Count = 0 Then
          MsgBox(Texxt(2953))
        Else
          txtANZ(Index).Visible = True
          lblANZ(Index).Visible = True
          txtANZ(Index).Text = CStr(CShort(TabExp(Index).Rows.Count))

        End If
        '
        '
        dbgEXP(Index).Columns(0).Visible = False
        dbgEXP(Index).Columns(1).HeaderText = Texxt(375)
        dbgEXP(Index).Columns(2).HeaderText = Texxt(394)
        '
        '
        btnRUN(Index).Enabled = True
        btnMARK(Index).Enabled = True
        Me.Cursor = Cursors.Arrow

        '

        '
      Case 3
        '
        '
        '
        '
        'Aufbau der dbgEXP-Tabelle für R-Werte
        '
        '
        '
        '

        btnRUN(Index).Enabled = False
        btnMARK(Index).Enabled = False
        System.Windows.Forms.Application.DoEvents()
        dbgEXP(Index).Visible = True
        txtANZ(Index).Visible = False
        lblANZ(Index).Visible = False



        Strgid = ""
        If RwrtGroup.cboGRP.SelectedIndex > 0 Then
          Strgid = "RWERT_GID= " & RwrtGroup.cboGRP.SelectedValue & " AND " & Strgid
        End If
        If chkARC(Index).CheckState <> 2 Then
          Strgid = Strgid & " RWERT_IARCH=" & chkARC(Index).CheckState & " AND "
        End If
        SqlStmt = "SELECT RWERT_ID,RWERT_DATTIM,RWERT_NAME FROM TBL_RWERT" & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND " & Strgid & "RWERT_DATTIM BETWEEN " & Sqldati(CDate(txtVON(Index).Text)) & " AND " & Sqldati(DateAdd(DateInterval.Day, 1, CDate(txtBIS(Index).Text)))

        If Trim(txtSQL(Index).Text) <> "" Then
          SqlStmt = SqlStmt & " AND " & StrSelct("RWERT_NAME", txtSQL(Index).Text)
        End If
        '
        '
        SqlStmt = SqlStmt & " ORDER BY RWERT_NAME"
        DynCmd.CommandText = SqlStmt
        DynCmd.Connection = Cndat()
        If Not FillDatset(DynAdapt, TabExp(Index)) Then
          Exit Sub
        End If
        If TabExp(Index).Rows.Count = 0 Then
          MsgBox(Texxt(2959))
        Else
          txtANZ(Index).Visible = True
          lblANZ(Index).Visible = True
          txtANZ(Index).Text = CStr(CShort(TabExp(Index).Rows.Count))
        End If
        '
        dbgEXP(Index).Columns(0).Visible = False
        dbgEXP(Index).Columns(1).HeaderText = Texxt(375)
        dbgEXP(Index).Columns(2).HeaderText = Texxt(394)
        '
        '
        '
        btnRUN(Index).Enabled = True
        btnMARK(Index).Enabled = True
        Me.Cursor = Cursors.Arrow

      Case 4
        '
        'Hilftabelle für Suche (Zwischenablage)
        '
        '
        '
        SqlStmt = "DELETE * FROM " & TblListZeig
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        '
        '
        '
        'Aufbau der dbgEXP-Tabelle für Sortimente
        '
        '
        '
        '
        '
        btnRUN(Index).Enabled = False
        btnMARK(Index).Enabled = False
        System.Windows.Forms.Application.DoEvents()
        dbgEXP(Index).Visible = True
        txtANZ(Index).Visible = False
        lblANZ(Index).Visible = False
        Strgid = ""
        SqlStmt = "SELECT SORTI_ID,SORTI_DATTIM,SORTI_NAME FROM TBL_SORTI" & " WHERE MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        DynCmd.Connection = Cndat()
        If Not FillDatset(DynAdapt, TabExp(Index)) Then
          Exit Sub
        End If
        If TabExp(Index).Rows.Count = 0 Then
          MsgBox(Texxt(2953))
        Else
          txtANZ(Index).Visible = True
          lblANZ(Index).Visible = True
          txtANZ(Index).Text = CStr(CShort(TabExp(Index).Rows.Count))
        End If
        '
        '
        dbgEXP(Index).Columns(0).Visible = False
        dbgEXP(Index).Columns(1).HeaderText = Texxt(375)
        dbgEXP(Index).Columns(2).HeaderText = Texxt(394)
        '
        '
        btnRUN(Index).Enabled = True
        btnMARK(Index).Enabled = True
        Me.Cursor = System.Windows.Forms.Cursors.Arrow

    End Select
    dbgEXP(Index).AllowUserToAddRows = False
    dbgEXP(Index).AllowUserToDeleteRows = False

  End Sub
  Private Sub chkARC_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) _
  Handles chkARC2.KeyPress, chkARC3.KeyPress
    Dim KeyAscii As Short = Asc(e.KeyChar)
    Dim Index As Short = CShort(sender.name.substring(7, 1))
    If KeyAscii = 13 Then
      chkARC(Index).CheckState = System.Windows.Forms.CheckState.Indeterminate
    End If
    e.KeyChar = Chr(KeyAscii)
    If KeyAscii = 0 Then
      e.Handled = True
    End If
  End Sub
  Private Sub btnMARK_Click(ByVal Sender As Object, ByVal eventArgs As System.EventArgs) _
  Handles btnMARK1.Click, btnMARK2.Click, btnMARK3.Click, btnMARK4.Click

    Dim Index As Short = CShort(Sender.name.substring(7, 1))
    If TabExp(Index).Rows.Count = 0 Then Exit Sub
    btnMARK(Index).Enabled = False
    btnRUN(Index).Enabled = False
    Cursor = Cursors.WaitCursor

    dbgEXP(Index).SelectAll()

    Cursor = System.Windows.Forms.Cursors.Arrow
    btnMARK(Index).Enabled = True
    btnRUN(Index).Enabled = True
  End Sub

  Private Sub btnRUN_Click(ByVal Sender As Object, ByVal eventArgs As System.EventArgs) _
 Handles btnRUN1.Click, btnRUN2.Click, btnRUN3.Click, btnRUN4.Click

    Dim Index As Short = CShort(Sender.name.substring(6, 1))
    Dim i As Short
    Dim WrGrund As ReadWriteGrund
    Dim OptKonst As New OpticalData
    Dim TabFarb As New DataTable
    Dim TabHilf As New DataTable
    Dim TabPreis As New DataTable
    Dim TabProz As New DataTable
    Dim TabProb As New DataTable
    Dim TabGrund As New DataTable
    Dim WhereID() As String
    Dim WhereMessID() As String
    '
    '
    Me.Cursor = Cursors.WaitCursor
    btnRUN(Index).Enabled = False
    '
    '
    '

    Select Case Index

      Case 1
        '
        '
        'Schreiben Farbmittel in Ziel-Datenbank
        '

        '
        '
        'Sätze Löschen
        '
        SqlStmt = "DELETE * FROM " & TblListFarbm
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Cursor = Cursors.Arrow
          btnRUN(Index).Enabled = True
          Exit Sub
        End If

        '
        '
        '
        'Ausgewählte Sätze
        '
        '
        If ConnOpen(CnTmp) Then
          SqlStmt = ""
          For i = 0 To dbgEXP(Index).SelectedRows.Count - 1
            'Schreiben nach Tabelle TblListFarbm
            '
            '
            SqlStmt = "INSERT INTO " & TblListFarbm & "([FARBM_ID]) VALUES(" & dbgEXP(Index).SelectedRows(i).Cells("FARBM_ID").Value & ")"
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              Cursor = Cursors.Arrow
              btnRUN(Index).Enabled = True
              Exit Sub
            End If

          Next i
          CnTmp.Close()
        End If
        '
        '
        '
        '
        '
        '
        WrGrund = New ReadWriteGrund
        OptKonst = New OpticalData
        ReDim WhereID(1)
        ReDim WhereMessID(2)
        WhereID(0) = "MISCH_ID"
        WhereID(1) = "FARBM_ID"
        WhereMessID(0) = "MISCH_ID"
        WhereMessID(1) = "MESSGRW_ID"
        WhereMessID(2) = "FARBM_ID"
        
        For i = 0 To dbgEXP(Index).SelectedRows.Count - 1
          TabFarb.Clear()
          TabHilf.Clear()
          TabPreis.Clear()
          TabProz.Clear()
          TabProb.Clear()
          TabGrund.Clear()
          SqlStmt = "SELECT * FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & dbgEXP(Index).SelectedRows(i).Cells("Farbm_ID").Value
          DynCmd.CommandText = SqlStmt
          DynCmd.Connection = CnDBHilf
          If Not FillDatset(DynAdapt, TabHilf) Then
            Exit Sub
          End If
          DynCmd.CommandText = SqlStmt
          DynCmd.Connection = Cndat()
          If Not FillDatset(DynAdapt, TabFarb) Then
            Exit Sub
          End If
          If IsDBNull(TabFarb.Rows(0)("GLZGRD_ID")) OrElse TabFarb.Rows(0)("GLZGRD_ID") = 0 Then
            TabFarb.Rows(0)("GLZGRD_ID") = TabFarb.Rows(0)("FARBM_ID")
          End If
          '
          SqlStmt = "SELECT * FROM TBL_FARBM_PREIS WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & dbgEXP(Index).SelectedRows(i).Cells("Farbm_ID").Value
          DynCmd.CommandText = SqlStmt
          DynCmd.Connection = Cndat()
          If Not FillDatset(DynAdapt, TabPreis) Then
            Exit Sub
          End If
          '
          SqlStmt = "SELECT * FROM TBL_FARBM_PROZ WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & dbgEXP(Index).SelectedRows(i).Cells("Farbm_ID").Value
          DynCmd.CommandText = SqlStmt
          DynCmd.Connection = Cndat()
          If Not FillDatset(DynAdapt, TabProz) Then
            Exit Sub
          End If
          '
          SqlStmt = "SELECT * FROM TBL_FARBM_PROB WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & dbgEXP(Index).SelectedRows(i).Cells("Farbm_ID").Value
          DynCmd.CommandText = SqlStmt
          DynCmd.Connection = Cndat()
          If Not FillDatset(DynAdapt, TabProb) Then
            Exit Sub
          End If
          '
          SqlStmt = "SELECT * FROM TBL_GRUND_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND FARBM_ID=" & dbgEXP(Index).SelectedRows(i).Cells("Farbm_ID").Value
          DynCmd.CommandText = SqlStmt
          DynCmd.Connection = Cndat()
          If Not FillDatset(DynAdapt, TabGrund) Then
            Exit Sub
          End If
          If TabHilf.Rows.Count = 0 Then
            '
            '
            'Falls Farbmittel nicht vorhanden, wird es in Hilfsdatenbank eingefügt
            '
            Call RowAddDatabase("TBL_FARBM", TabFarb.Rows(0), CnDBHilf)
          Else
            '
            '
            '
            'andernfalls wird es überschrieben
            '
            '
            '
            '
            '
            Call RowUpdateDatabase("TBL_FARBM", TabFarb.Rows(0), WhereID, CnDBHilf)
            '
            'Löschen
            '
            Call RowDeleteDatabase("TBL_FARBM_PREIS", TabFarb.Rows(0), WhereID, CnDBHilf)
            '
            '
            Call RowDeleteDatabase("TBL_FARBM_PROZ", TabFarb.Rows(0), WhereID, CnDBHilf)
            '
            '
            Call RowDeleteDatabase("TBL_FARBM_PROB", TabFarb.Rows(0), WhereID, CnDBHilf)

            If chkGRD.CheckState <> 0 Then
              '
              'Mit Grunddaten
              '
              '
              '
              '

              Call RowDeleteDatabase("TBL_GRUND_FARBM", TabFarb.Rows(0), WhereMessID, CnDBHilf)
            End If
          End If
          '
          '
          'zugehörige Dateien übernehmen
          '
          '
          '
          '
          TabPreis.AcceptChanges()
          Call RowsAddDatabase("TBL_FARBM_PREIS", TabPreis.Select("", "FARBM_IRPA"), CnDBHilf)
          '
          '
          TabProz.AcceptChanges()
          Call RowsAddDatabase("TBL_FARBM_PROZ", TabProz.Select("", "FARBM_IRFA"), CnDBHilf)
          '
          '
          TabProb.AcceptChanges()
          Call RowsAddDatabase("TBL_FARBM_PROB", TabProb.Select("", "FARBM_IRBA"), CnDBHilf)
          '
          '
          '
          If chkGRD.CheckState <> 0 Then
            '
            TabGrund.AcceptChanges()
            Call RowsAddDatabase("TBL_GRUND_FARBM", TabGrund.Select, CnDBHilf)

          End If

        Next i
        '
        '
        WrGrund.dispose()
        OptKonst.dispose()
        TabFarb.Dispose()
        TabHilf.Dispose()
        TabPreis.Dispose()
        TabProz.Dispose()
        TabProb.Dispose()
        TabGrund.Dispose()
        Me.Cursor = Cursors.Arrow
        btnExport(2).Enabled = True
        btnExport(4).Enabled = True
        '
      Case 2
        '
        '
        'Schreiben Rezepte in Ziel-Datenbank
        '
        '
        '
        '
        'Sätze Löschen
        '
        SqlStmt = "DELETE * FROM " & TblListRezept
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        '
        '
        'Ausgewählte Sätze
        '
        '
        If ConnOpen(CnTmp) Then
          For i = 0 To dbgEXP(Index).SelectedRows.Count - 1
            '
            'Schreiben nach Tabelle TblListRezept
            '
            '
            SqlStmt = "INSERT INTO " & TblListRezept & "([REZEPT_ID]) VALUES(" & dbgEXP(Index).SelectedRows(i).Cells("REZEPT_ID").Value & ")"
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              Exit Sub
            End If
          Next i
          CnTmp.Close()
        End If
        '
        '
        '
        '
        '
        '
        'Rwerte für Rezepte löschen
        '
        '
        SqlStmt = "DELETE * FROM TBL_RWERTRZ" & " WHERE RWERT_ID IN (SELECT DISTINCT RWERT_ID FROM TBL_REZEPT_RWERT)"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
          Exit Sub
        End If
        '
        '
        'Einträge für Misch_ID löschen
        '
        '
        '
        SqlStmt = "DELETE * FROM TBL_REZEPT_FARBM WHERE MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
          Exit Sub
        End If '
        SqlStmt = "DELETE * FROM TBL_REZEPT_RWERT WHERE MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
          Exit Sub
        End If '
        '
        SqlStmt = "DELETE * FROM TBL_REZEPT WHERE MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        'Einträge zu Tabelle TBL_REZEPT hinzufügen
        '
        '
        '
        '
        SqlStmt = "INSERT INTO TBL_REZEPT IN '" & CnDBHilf.DataSource & "' SELECT * FROM TBL_REZEPT" & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN (SELECT REZEPT_ID FROM " & TblListRezept & " IN '" & CnTmp.DataSource & "')"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst0
        End If

        '
        '
        '
        'Einträge zu Tabelle TBL_REZEPT_FARBM hinzufügen
        '
        '
        '
        '
        SqlStmt = "INSERT INTO TBL_REZEPT_FARBM IN '" & CnDBHilf.DataSource & "'" & " SELECT * FROM TBL_REZEPT_FARBM" & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN (SELECT REZEPT_ID FROM " & TblListRezept & " IN '" & CnTmp.DataSource & "')"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst0
        End If

        '
        '
        'Einträge zu Tabelle TBL_REZEPT_RWERT hinzufügen
        '
        '
        '
        If chkTYP.CheckState = 1 Then
          '
          'Mit R-Werten (Vorlagen)
          '
          '
          SqlStmt = "INSERT INTO TBL_REZEPT_RWERT IN '" & CnDBHilf.DataSource & "'" & " SELECT * FROM TBL_REZEPT_RWERT" & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN" _
          & " (SELECT REZEPT_ID FROM " & TblListRezept & " IN '" & CnTmp.DataSource & "') AND (RWERT_KWB=11 OR RWERT_KWB=12)"
          DynCmd.CommandText = SqlStmt
          If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
            GoTo ErrEinst0
          End If

        End If
        If chkSMP.CheckState = 1 Then
          '
          'Mit R-Werten (Nachstellungen)
          '
          '
          SqlStmt = "INSERT INTO TBL_REZEPT_RWERT IN '" & CnDBHilf.DataSource & "'" & " SELECT * FROM TBL_REZEPT_RWERT" & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN (SELECT REZEPT_ID FROM " & TblListRezept & " IN '" & CnTmp.DataSource & "') AND (RWERT_KWB=1 OR RWERT_KWB=2)"
          DynCmd.CommandText = SqlStmt
          If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
            GoTo ErrEinst0
          End If

        End If
        If chkUUU.CheckState = 1 Then
          '
          'Mit R-Werten (Untergründe)
          '
          '
          SqlStmt = "INSERT INTO TBL_REZEPT_RWERT IN '" & CnDBHilf.DataSource & "'" & " SELECT * FROM TBL_REZEPT_RWERT" & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN (SELECT REZEPT_ID FROM " & TblListRezept & " IN '" & CnTmp.DataSource & "') AND (RWERT_KWB=-1 OR RWERT_KWB=-2)"
          DynCmd.CommandText = SqlStmt
          If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
            GoTo ErrEinst0
          End If

        End If
        '
        '
        'Einträge zu Tabelle TBL_RWERTRZ hinzufügen
        '
        '
        SqlStmt = "INSERT INTO TBL_RWERTRZ IN '" & CnDBHilf.DataSource & "'" & " SELECT * FROM TBL_RWERT" & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID IN (SELECT DISTINCT RWERT_ID FROM TBL_REZEPT_RWERT IN '" & CnDBHilf.DataSource & "')"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst0
        End If


        Me.Cursor = Cursors.Arrow
      Case 3
        '
        '
        'Schreiben R-Werte in Ziel-Datenbank
        '
        '
        '
        '
        '
        'Sätze Löschen
        '
        SqlStmt = "DELETE * FROM " & TblListRwert
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          GoTo ErrEinst0
        End If
        '
        '
        '
        '
        '
        'Ausgewählte Sätze
        '
        '
        'For i = 0 To dbgEXP(Index).SelBookmarks.Count - 1
        If ConnOpen(CnTmp) Then
          For i = 0 To dbgEXP(Index).SelectedRows.Count - 1
            'Schreiben nach Tabelle TblListRwert
            '
            '
            SqlStmt = "INSERT INTO " & TblListRwert & "([RWERT_ID]) VALUES(" & dbgEXP(Index).SelectedRows(i).Cells("RWERT_ID").Value & ")"
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              GoTo ErrEinst0
            End If
          Next i
          CnTmp.Close()
        End If
        '
        '
        '
        '
        '
        'Rwerte löschen
        '
        '
        SqlStmt = "DELETE * FROM TBL_RWERT"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
          GoTo ErrEinst0
        End If
        '
        '
        '
        '
        '
        'Einträge zu Tabelle TBL_RWERT hinzufügen
        '
        '
        '
        '
        SqlStmt = "INSERT INTO TBL_RWERT IN '" & CnDBHilf.DataSource & "'" & " SELECT * FROM TBL_RWERT" & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID IN (SELECT RWERT_ID FROM " & TblListRwert & " IN '" & CnTmp.DataSource & "')"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst0
        End If
        '
        '
        '

        Me.Cursor = Cursors.Arrow
      Case 4
        '
        '
        'Schreiben Sortimente in Ziel-Datenbank
        '
        '
        '
        '
        'Sätze Löschen
        '
        SqlStmt = "DELETE * FROM " & TblListSorti
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          GoTo ErrEinst0
        End If
        '
        '
        '
        '
        '
        'Ausgewählte Sätze
        '
        '

        If ConnOpen(CnTmp) Then
          For i = 0 To dbgEXP(Index).SelectedRows.Count - 1
            '
            'Schreiben nach Tabelle TblListSorti
            '
            '
            SqlStmt = "INSERT INTO " & TblListSorti & "([SORTI_ID]) VALUES(" & dbgEXP(Index).SelectedRows(i).Cells("SORTI_ID").Value & ")"
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              GoTo ErrEinst0
            End If
            '
          Next i
          CnTmp.Close()
        End If

        '
        '
        '
        '
        '
        '
        'Rwerte löschen
        '
        '
        SqlStmt = "DELETE * FROM TBL_RWERTSO" & " WHERE RWERT_ID IN (SELECT DISTINCT RWERT_ID FROM TBL_SORTI_RWERT)"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
          GoTo ErrEinst0
        End If
        '
        '
        'Einträge für Misch_ID löschen
        '
        '
        '
        SqlStmt = "DELETE * FROM TBL_SORTI_FARBM WHERE MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
          GoTo ErrEinst0
        End If
        '
        SqlStmt = "DELETE * FROM TBL_SORTI_RWERT WHERE MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
          GoTo ErrEinst0
        End If
        '
        '
        SqlStmt = "DELETE * FROM TBL_SORTI WHERE MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
          GoTo ErrEinst0
        End If
        '
        '
        '
        'Einträge zu Tabelle TBL_SORTI hinzufügen
        '
        '
        '
        '
        SqlStmt = "INSERT INTO TBL_SORTI IN '" & CnDBHilf.DataSource & "' SELECT * FROM TBL_SORTI" & " WHERE MISCH_ID=" & MenueParam.MischID & " AND SORTI_ID IN (SELECT SORTI_ID FROM " & TblListSorti & " IN '" & CnTmp.DataSource & "')"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst0
        End If
        '
        '
        '
        'Einträge zu Tabelle TBL_SORTI_FARBM hinzufügen
        '
        '
        '
        '
        SqlStmt = "INSERT INTO TBL_SORTI_FARBM IN '" & CnDBHilf.DataSource & "'" & " SELECT * FROM TBL_SORTI_FARBM" & " WHERE MISCH_ID=" & MenueParam.MischID & " AND SORTI_ID IN" _
          & "(SELECT SORTI_ID FROM " & TblListSorti & " IN '" & CnTmp.DataSource & "')" & " AND FARBM_ID IN (SELECT FARBM_ID FROM TBL_FARBM IN '" & CnDBHilf.DataSource & "' WHERE MISCH_ID=" & MenueParam.MischID & ")"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst0
        End If
        '
        '
        'Einträge zu Tabelle TBL_REZEPT_RWERT hinzufügen
        '
        '
        SqlStmt = "INSERT INTO TBL_SORTI_RWERT IN '" & CnDBHilf.DataSource & "'" & " SELECT * FROM TBL_SORTI_RWERT" & " WHERE MISCH_ID=" & MenueParam.MischID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID _
          & " AND SORTI_ID IN (SELECT SORTI_ID FROM " & TblListSorti & " IN '" & CnTmp.DataSource & "')"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst0
        End If
        '
        '
        'Einträge zu Tabelle TBL_RWERTSO hinzufügen
        '
        '
        SqlStmt = "INSERT INTO TBL_RWERTSO IN '" & CnDBHilf.DataSource & "'" & " SELECT * FROM TBL_RWERT" & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID IN (SELECT DISTINCT RWERT_ID FROM TBL_SORTI_RWERT IN '" & CnDBHilf.DataSource & "')"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst0
        End If

        Me.Cursor = Cursors.Arrow
        '
        '
    End Select
    btnExport(Index).Enabled = False
    Exit Sub
ErrEinst0:
    Me.Cursor = System.Windows.Forms.Cursors.Arrow
    btnRUN(Index).Enabled = True
    Exit Sub
  End Sub

  Private Sub frmDBExportCOL_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize, Me.Shown, Me.Activated
    If MnResz Then
      Call ResizeChild(Me)
    End If
  End Sub


  Private Sub btnExport_Click(ByVal sender As Object, ByVal e As System.EventArgs) _
  Handles btnExport0.Click, btnExport1.Click, btnExport2.Click, btnExport3.Click, btnExport4.Click
    Dim Index As Short = CShort(sender.name.substring(9, 1))

    Dim i As Short
    For i = 1 To 4
      SplitCONT(i).Hide()
    Next
    PanelDBase.Hide()
    Select Case Index
      Case 0
        PanelDBase.Show()
      Case 1, 2, 3, 4
        SplitCONT(Index).Show()
    End Select
  End Sub




  Private Sub txtDat_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtBIS2.Validating, txtVON2.Validating, txtBIS3.Validating, txtVON3.Validating
    If Not IsDate(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(2500))
    End If
  End Sub





End Class