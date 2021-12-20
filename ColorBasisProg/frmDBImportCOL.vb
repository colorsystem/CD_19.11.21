Option Strict Off
Option Explicit On
Option Compare Text
Public Class frmDBImportCOL

  Dim TblListZeig As String
  Dim TblListFarbm As String
  Dim TblListRezept As String
  Dim TblListSorti As String
  Dim TblListRwert As String
  Dim DynCmd As OleDbCommand
  Dim DynAdapt As OleDbDataAdapter
  Dim DaRead As OleDbDataReader
  Dim btnRUN As List(Of Button)
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
  Dim chkFAR As List(Of CheckBox)
  Dim SplitCONT As List(Of SplitContainer)
  Dim btnImport As List(Of Button)
  Dim dbgIMP As List(Of DataGridView)
  Dim TabImp As List(Of DataTable)
  Dim radDURW As List(Of RadioButton)
  Dim DBHilfName As String
  Dim CnDBHilf As OleDbConnection
  Dim SqlStmt As String
  Dim FeldNamen() As String
  Dim LaeKmNWE As Integer
  Dim LaeLongB As Integer
  Dim MaxID As Integer
  Dim AltID() As Integer
  Dim NeuID() As Integer
  Dim SlaveFarbNames() As String
  Dim SlaveRezeptNames() As String
  Dim SlaveSortiNames() As String
  Dim SlaveHlfTableNames() As String


  '
  Dim ReWrRwert As ReadWriteRwert
  Dim RefWert As RefValue
  Dim Namelength As Integer
  '
  Dim QuMessgID As Integer
  Dim QuGID() As Integer
  Dim QuNormFileID As Integer
  Dim AdaptHilf As OleDbDataAdapter
  Dim CmdHilf As OleDbCommand
  Dim StrHilf As String
  Dim TabHilf As DataTable

  Function ControlLength(TabDat As DataTable, Conn As OleDbConnection, Tablename As String, TableField As String, LenCorrect As Boolean) As Boolean
    Dim NamLength As String
    ControlLength = True
    '
    '
    'Prüfen, ob Länge in Ordnung
    '
    NamLength = StringLength(Tablename, TableField, Conn)
    For i = 0 To TabDat.Rows.Count - 1
      If Not IsDBNull(TabDat.Rows(i)(TableField)) AndAlso Len(TabDat.Rows(i)(TableField)) > NamLength Then
        If LenCorrect Then
          TabDat.Rows(i)(TableField) = TabDat.Rows(i)(TableField).substring(0, NamLength)
        Else
          MsgBox(TableField & ": " & TabDat.Rows(i)(TableField) & " " & Texxt(3978), MsgBoxStyle.OkOnly, Texxt(2000))
          ControlLength = False
          Exit For
        End If
      End If
    Next
  End Function
  Sub RowToRef(ByRef RefRow As DataRow, ByRef Refel As RefValue, ByRef ier As Integer)
    Dim Mdim As Integer
    Dim Nku As Integer
    Dim Rhilf() As Single
    Dim DE() As Single
    Dim hlf() As Byte
    Dim kw As Integer


    Mdim = MenueParam.Messg.Winkel.Wsol.Nwe * MenueParam.Messg.Winkel.Km
    Refel.ID = RefRow("RWERT_ID")
    Refel.ReTr = RefRow("rwert_retr")
    hlf = RefRow("rwert_rwert")
    If hlf.Length <> 4 * Mdim Then
      MsgBox(Texxt(2986), 0, Texxt(2000))
      ier = -20
      Exit Sub
    End If
    Rhilf = GetSingles(hlf)
    hlf = RefRow("rwert_de")
    DE = GetSingles(hlf)
    '
    '
    Refel.RefKurv.clear()
    '
    For kw = 0 To MenueParam.Messg.Winkel.Km - 1
      Refel.RefKurv.Add(MenueParam.Messg.Winkel(kw).Chrm, New CurveRef(MenueParam.Messg.Winkel.Wsol.Nwe))
      Refel.De(kw) = DE(MenueParam.Messg.Winkel(kw).Lkm)
      Nku = MenueParam.Messg.Winkel(kw).Lkm * MenueParam.Messg.Winkel.Wsol.Nwe
      Array.Copy(Rhilf, Nku, Refel.RefKurv(MenueParam.Messg.Winkel(kw).Chrm).R, 0, MenueParam.Messg.Winkel.Wsol.Nwe)
    Next kw

    Refel.Name = RefRow("Rwert_name")
    If IsDBNull(RefRow("rwert_kenn")) OrElse RefRow("rwert_kenn") = "" Then
      Refel.Banum = " "
    Else
      Refel.Banum = RefRow("rwert_kenn")
    End If
    If IsDBNull(RefRow("rwert_bem")) OrElse RefRow("rwert_bem") = "" Then
      Refel.Bem = " "
    Else
      Refel.Bem = RefRow("rwert_bem")
    End If
    Refel.DatTim = RefRow("RWERT_DATTIM")
    Refel.Iami = RefRow("rwert_iami")
    If IsDBNull(RefRow("rwert_cme")) OrElse RefRow("rwert_cme") = "" Then
      Refel.Cme = "  "
    Else
      Refel.Cme = RefRow("rwert_cme")
    End If
    Refel.Iarch = RefRow("rwert_iarch")
    If Refel.Iarch < 0 Then Refel.Iarch = 2
    Refel.Gid = 0
    If RefRow("rwert_gid") <> 0 Then
      Refel.Gid = RefRow("rwert_gid")
    End If
    Erase DE
    Erase Rhilf
    Erase hlf
  End Sub

  Private Sub frmDBImportCOL_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
    AufbauPar.MethID = -1

  End Sub

  Private Sub frmDBImportCOL_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    Try
      AdaptHilf.Dispose()
      CmdHilf.Dispose()
      TabHilf.Dispose()
    Catch ex As Exception

    End Try



  End Sub
  Private Sub frmDBImportCOL_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing

  End Sub

  Private Sub frmDBImportCOL_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Me.Text = Texxt(1911) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    radDURW = New List(Of RadioButton)
    radDURW.Add(radDURW_0)
    radDURW.Add(radDURW_1)
    radDURW.Add(radDURW_2)

    ReDim SlaveFarbNames(5)
    SlaveFarbNames(0) = "TBL_FARBM_PREIS"
    SlaveFarbNames(1) = "TBL_FARBM_PROZ"
    SlaveFarbNames(2) = "TBL_FARBM_PROB"
    SlaveFarbNames(3) = "TBL_GRUND_FARBM"
    SlaveFarbNames(4) = "TBL_REZEPT_FARBM"
    SlaveFarbNames(5) = "TBL_SORTI_FARBM"

    '
    '
    '
    ReDim SlaveRezeptNames(1)
    SlaveRezeptNames(0) = "TBL_REZEPT_FARBM"
    SlaveRezeptNames(1) = "TBL_REZEPT_RWERT"
    '
    '
    ReDim SlaveSortiNames(1)
    SlaveSortiNames(0) = "TBL_SORTI_FARBM"
    SlaveSortiNames(1) = "TBL_SORTI_RWERT"

    '
    'Menue-Parameter uebernehmen
    '
    '
    lblZiel.Text = Cndat.DataSource
    btnDBase.Text = Texxt(3620)
    btnPRF.Text = Texxt(304)
    lblQQQ.Text = Texxt(3620)
    lblZZZ.Text = Texxt(3619)
    radDURW_0.Text = Texxt(3676)
    radDURW_1.Text = Texxt(3677)
    radDURW_2.Text = Texxt(3678)
    chkDUPRZ.Text = Texxt(3654)
    chkDUPSO.Text = Texxt(3672)
    chkOVFA.Text = Texxt(3645)
    chkOVGK.Text = Texxt(3647)
    chkOVGR.Text = Texxt(3646)
    '
    '
    chkFAR = New List(Of CheckBox)
    btnRUN = New List(Of Button)
    btnZeig = New List(Of Button)
    lblGRP = New List(Of System.Windows.Forms.Label)
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
    btnImport = New List(Of Button)
    dbgIMP = New List(Of DataGridView)
    chkARC = New List(Of CheckBox)
    TabImp = New List(Of DataTable)
    TabImp.Add(Nothing)
    TabImp.Add(New DataTable)
    TabImp.Add(New DataTable)
    TabImp.Add(New DataTable)
    TabImp.Add(New DataTable)
    '
    '
    '
    chkFAR0.Text = Texxt(3626)
    chkFAR1.Text = Texxt(3627)
    chkFAR.Add(chkFAR0)
    chkFAR.Add(chkFAR1)
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
    SplitCONT.Add(Nothing)
    SplitCONT.Add(SplitCont1)
    SplitCONT.Add(SplitCont2)
    SplitCONT.Add(SplitCont3)
    SplitCONT.Add(SplitCont4)
    '
    '
    btnImport0.Text = Texxt(3620)
    btnImport1.Text = Texxt(3621)
    btnImport2.Text = Texxt(3622)
    btnImport3.Text = Texxt(3623)
    btnImport4.Text = Texxt(3624)
    '
    Application.DoEvents()
    '
    btnImport.Add(btnImport0)
    btnImport.Add(btnImport1)
    btnImport.Add(btnImport2)
    btnImport.Add(btnImport3)
    btnImport.Add(btnImport4)
    For i = 1 To 4
      btnImport(i).Enabled = False
    Next i '
    '
    dbgIMP.Add(Nothing)
    dbgIMP.Add(dbgIMP1)
    dbgIMP.Add(dbgIMP2)
    dbgIMP.Add(dbgIMP3)
    dbgIMP.Add(dbgIMP4)
    '
    '
    '
    '
    For i = 1 To dbgIMP.Count - 1
      dbgIMP(i).DataSource = TabImp(i)
      dbgIMP(i).SelectionMode = DataGridViewSelectionMode.FullRowSelect
    Next
    '
    'TblListZeig
    '
    '
    '
    '
    DynAdapt = New OleDbDataAdapter
    ReWrRwert = New ReadWriteRwert
    DynCmd = New OleDbCommand
    DynAdapt.SelectCommand = DynCmd
    CnDBHilf = New OleDbConnection
    '
    '
    DynCmd.Connection = CnTmp()
    '
    '
    AdaptHilf = New OleDbDataAdapter
    CmdHilf = New OleDbCommand
    TabHilf = New DataTable
    AdaptHilf.SelectCommand = CmdHilf

    '





  End Sub

  Sub NewRwertIndices(ByRef MasterTableName As String, ByVal MessgRwID As Integer, ByRef SlaveTableNames() As String, ByVal CnTmp As OleDbConnection)
    Dim Ianz As Integer
    Dim DaReadHlf As OleDbDataReader
    Dim CmdHlf As OleDbCommand
    MaxID = ReWrRwert.RefMax("TBL_RWERT", MessgRwID, Cndat) + 1
    '
    Ianz = 0
    Erase AltID
    Erase NeuID
    CmdHlf = New OleDbCommand
    SqlStmt = "SELECT * FROM " & MasterTableName
    CmdHlf.Connection = CnTmp
    CmdHlf.CommandText = SqlStmt
    DaReadHlf = DataReader(CmdHlf, CommandBehavior.Default, CnTmp)
    'DynHlf = Dtmp.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset, DAO.RecordsetOptionEnum.dbConsistent, DAO.RecordsetOptionEnum.dbReadOnly)
    Do While DaReadHlf.Read
      'UPGRADE_WARNING: Die untere Begrenzung des Arrays AltID wurde von 1 in 0 geändert. Klicken Sie hier für weitere Informationen: 'ms-help://MS.VSCC.v80/dv_commoner/local/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
      ReDim Preserve AltID(Ianz)
      'UPGRADE_WARNING: Die untere Begrenzung des Arrays NeuID wurde von 1 in 0 geändert. Klicken Sie hier für weitere Informationen: 'ms-help://MS.VSCC.v80/dv_commoner/local/redirect.htm?keyword="0F1C9BE1-AF9D-476E-83B1-17D43BECFF20"'
      ReDim Preserve NeuID(Ianz)
      AltID(Ianz) = DaReadHlf("Rwert_ID")
      NeuID(Ianz) = MaxID + Ianz
      Ianz = Ianz + 1
    Loop
    DaReadHlf.Close()
    '
    'Index für R-Werte neu festlegen
    '
    '
    Call NewIndices("RWERT_ID", Ianz, AltID, NeuID, MasterTableName, SlaveTableNames, CnTmp)


  End Sub

  Private Sub btnDBASE_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles btnDBase.Click
    Dim OldCurdir As String = ""
    Try
      OldCurdir = My.Computer.FileSystem.CurrentDirectory
    Catch
    End Try
    OpenFileDialogImport.FileName = NewFileName(CnTmp.DataSource, "COLHILF.MDB")
    OpenFileDialogImport.CheckFileExists = False
    If Not OpenFileDialogImport.ShowDialog() = DialogResult.OK Then
      Exit Sub
    End If
    DBHilfName = OpenFileDialogImport.FileName
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
    End Try
    '
    '
    lblQuel.Text = DBHilfName
    Cursor = Cursors.WaitCursor
    '
    '
    '
    'Prüfen, ob Hilfsdatenbank vorhanden
    '
    '
    '
    If Not File.Exists(DBHilfName) Then
      MsgBox(Texxt(3630))
      GoTo ErrEinst1
    Else
      CnDBHilf.ConnectionString = NewConnectionstring(CnTmp.ConnectionString, DBHilfName)
      btnZeig(1).Enabled = True
      btnZeig(2).Enabled = True
      btnZeig(3).Enabled = True
      btnZeig(4).Enabled = True
      btnPRF.Enabled = True
    End If
ErrEinst1:
    Cursor = Cursors.Arrow
    ''
  End Sub

  Private Sub btnPRF_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles btnPRF.Click
    Dim ier As Integer
    Dim i As Integer
    Dim Check As New HandleHilfLib
    '
    '
    'Dateien in DTMP löschen und
    '
    '
    'Dateien umspeichern von DBHILF nach DTMP
    '
    '
    '
    '
    Cursor = Cursors.WaitCursor
    '
    '
    'Tabelle Messgeräte
    '
    '
    '
    'Messgerät prüfen, ob Winkel/Messgeometrie übereinstimmt
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_GRUND_FARBM"
    DynCmd.CommandText = SqlStmt
    DaRead = DataReader(DynCmd, CommandBehavior.SingleResult, CnDBHilf)
    If DaRead.Read Then
      QuMessgID = DaRead("Messg_id")
      Call Check.CheckMeasureDev(Cncol, CnDBHilf, MenueParam.MessgID, QuMessgID, ier)
    End If
    DaRead.Close()
    If ier <> 0 Then
      Cursor = Cursors.Arrow
      Exit Sub
    End If
    '
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_RWERT"
    DynCmd.CommandText = SqlStmt
    DaRead = DataReader(DynCmd, CommandBehavior.SingleResult, CnDBHilf)
    If DaRead.Read Then
      QuMessgID = DaRead("Messg_id")
      Call Check.CheckMeasureDev(Cncol, CnDBHilf, MenueParam.MessgID, QuMessgID, ier)
      If ier <> 0 Then
        DaRead.Close()
        Cursor = Cursors.Arrow
        Exit Sub
      End If
      LaeLongB = UBound(DaRead("rwert_rwert")) + 1
      If LaeLongB <> 4 * MenueParam.Messg.Winkel.Km * MenueParam.Messg.Winkel.Wsol.Nwe Then
        MsgBox(Texxt(3636))
        Cursor = System.Windows.Forms.Cursors.Arrow
        DaRead.Close()
        Exit Sub
      End If
    End If
    DaRead.Close()
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_RWERTRZ"
    DynCmd.CommandText = SqlStmt
    DaRead = DataReader(DynCmd, CommandBehavior.SingleResult, CnDBHilf)
    If DaRead.Read Then
      QuMessgID = DaRead("Messg_id")
      Call Check.CheckMeasureDev(Cncol, CnDBHilf, MenueParam.MessgID, QuMessgID, ier)
      If ier <> 0 Then
        DaRead.Close()
        Cursor = System.Windows.Forms.Cursors.Arrow
        Exit Sub
      End If
      LaeLongB = UBound(DaRead("rwert_rwert")) + 1
      If LaeLongB <> 4 * MenueParam.Messg.Winkel.Km * MenueParam.Messg.Winkel.Wsol.Nwe Then
        MsgBox(Texxt(3636))
        Cursor = Cursors.Arrow
        DaRead.Close()
        Exit Sub
      End If
    End If
    DaRead.Close()
    '
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_RWERTSO"
    DynCmd.CommandText = SqlStmt
    DaRead = DataReader(DynCmd, CommandBehavior.SingleResult, CnDBHilf)
    If DaRead.Read Then
      QuMessgID = DaRead("Messg_id")
      Call Check.CheckMeasureDev(Cncol, CnDBHilf, MenueParam.MessgID, QuMessgID, ier)
      If ier <> 0 Then
        DaRead.Close()
        Cursor = System.Windows.Forms.Cursors.Arrow
        Exit Sub
      End If
      LaeLongB = UBound(DaRead("rwert_rwert")) + 1
      If LaeLongB <> 4 * MenueParam.Messg.Winkel.Km * MenueParam.Messg.Winkel.Wsol.Nwe Then
        MsgBox(Texxt(3636))
        Cursor = Cursors.Arrow
        DaRead.Close()
        Exit Sub
      End If
    End If
    DaRead.Close()
    '
    '
    '
    If TableExists("TBL_MESSG", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_MESSG"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
      'Call ColwinParamLIBDBHilfProgramme_definst.SQLExeLock(1, Dtmp, SqlStmt, ier)
    End If

    SqlStmt = "SELECT * INTO TBL_MESSG IN '" & CnTmp.DataSource & "' FROM TBL_MESSG"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
      Exit Sub
    End If
    '
    'Tabelle Messgeräte (Geometrien/Winkel) übernehmen (um später Prüfungen vorzunehmen)
    '
    '
    '
    If TableExists("TBL_MESSG_IHRM", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_MESSG_IHRM"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If

    SqlStmt = "SELECT * INTO TBL_MESSG_IHRM IN '" & CnTmp.DataSource & "' FROM TBL_MESSG_IHRM"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
      Exit Sub
    End If
    '
    '
    'Tabelle Gruppen Messgeräte
    '
    '
    '
    If TableExists("TBL_MESSG_GROUP", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_MESSG_GROUP"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If

    SqlStmt = "SELECT * INTO TBL_MESSG_GROUP IN '" & CnTmp.DataSource & "' FROM TBL_MESSG_GROUP"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
      Exit Sub
    End If
    '
    '
    'Tabelle Gruppen Mischsysteme
    '
    '
    '
    If TableExists("TBL_MISCH_GROUP", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_MISCH_GROUP"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    SqlStmt = "SELECT * INTO TBL_MISCH_GROUP IN '" & CnTmp.DataSource & "' FROM TBL_MISCH_GROUP"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    'Tabelle GK-Werte übernehmen (um später Prüfungen vorzunehmen)
    '
    '
    '
    If TableExists("TBL_GKWRT", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_GKWRT"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    SqlStmt = "SELECT * INTO TBL_GKWRT IN '" & CnTmp.DataSource & "' FROM TBL_GKWRT"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    'Tabelle USER_MISCH_MESSG übernehmen (um später Prüfungen vorzunehmen)
    '
    '
    '
    If TableExists("TBL_USER_MISCH_MESSG", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_USER_MISCH_MESSG"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    If TableExists("TBL_USER_MISCH_MESSG", CnDBHilf) Then
      SqlStmt = "SELECT * INTO TBL_USER_MISCH_MESSG IN '" & CnTmp.DataSource & "' FROM TBL_USER_MISCH_MESSG"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
    End If
    '
    'Farbmittel
    '
    '
    '
    If TableExists("TBL_FARBM", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_FARBM"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    SqlStmt = "SELECT * INTO TBL_FARBM IN '" & CnTmp.DataSource & "' FROM TBL_FARBM"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
      Exit Sub
    End If
    '
    '
    'MISCH_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_FARBM SET MISCH_ID=" & MenueParam.MischID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    If TableExists("TBL_FARBM_PREIS", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_FARBM_PREIS"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    SqlStmt = "SELECT * INTO TBL_FARBM_PREIS IN '" & CnTmp.DataSource & "' FROM TBL_FARBM_PREIS"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    'MISCH_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_FARBM_PREIS SET MISCH_ID=" & MenueParam.MischID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    '
    '

    '
    If TableExists("TBL_FARBM_PROZ", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_FARBM_PROZ"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    SqlStmt = "SELECT * INTO TBL_FARBM_PROZ IN '" & CnTmp.DataSource & "' FROM TBL_FARBM_PROZ"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    'MISCH_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_FARBM_PROZ SET MISCH_ID=" & MenueParam.MischID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    '
    '

    '
    If TableExists("TBL_FARBM_PROB", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_FARBM_PROB"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    SqlStmt = "SELECT * INTO TBL_FARBM_PROB IN '" & CnTmp.DataSource & "' FROM TBL_FARBM_PROB"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    'MISCH_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_FARBM_PROB SET MISCH_ID=" & MenueParam.MischID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    '
    '

    '
    If TableExists("TBL_GRUND_FARBM", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_GRUND_FARBM"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    SqlStmt = "SELECT * INTO TBL_GRUND_FARBM IN '" & CnTmp.DataSource & "' FROM TBL_GRUND_FARBM"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    'MISCH_ID
    '
    '
    SqlStmt = "UPDATE TBL_GRUND_FARBM SET MISCH_ID=" & MenueParam.MischID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    'MESSGRW_ID und MESSG_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_GRUND_FARBM SET MESSGRW_ID=" & MenueParam.Messg.MessgRwID & ", MESSG_ID=" & MenueParam.MessgID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '

    '
    '
    'R-Werte
    '
    '
    '
    If TableExists("TBL_RWERT", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_RWERT"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    If radDURW(1).Checked Then
      '
      '
      'Tabelle R-Werte nur für nicht doppelte R-Wert-Namen
      '
      '
      '
      '
      SqlStmt = "SELECT * INTO TBL_RWERT IN '" & CnTmp.DataSource & "' FROM TBL_RWERT" & " WHERE RWERT_NAME NOT IN (SELECT RWERT_NAME FROM TBL_RWERT IN '" & Cndat.DataSource & "' WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & ")"
    Else

      SqlStmt = "SELECT * INTO TBL_RWERT IN '" & CnTmp.DataSource & "' FROM TBL_RWERT"
    End If
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    'MESSGRW_ID und MESSG_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_RWERT SET MESSGRW_ID=" & MenueParam.Messg.MessgRwID & ", MESSG_ID=" & MenueParam.MessgID & ",USER_ID=" & MenueParam.UserID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '

    '
    '
    '
    'Rezepte
    '
    '
    '
    '
    If TableExists("TBL_REZEPT", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_REZEPT"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    If TableExists("TBL_REZEPT_FARBM", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_REZEPT_FARBM"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    If TableExists("TBL_REZEPT_RWERT", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_REZEPT_RWERT"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    If TableExists("TBL_RWERTRZ", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_RWERTRZ"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If

    If chkDUPRZ.CheckState = 0 Then
      SqlStmt = "SELECT * INTO TBL_REZEPT IN '" & CnTmp.DataSource & "' FROM TBL_REZEPT"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      SqlStmt = "SELECT * INTO TBL_REZEPT_FARBM IN '" & CnTmp.DataSource & "' FROM TBL_REZEPT_FARBM"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      SqlStmt = "SELECT * INTO TBL_REZEPT_RWERT IN '" & CnTmp.DataSource & "' FROM TBL_REZEPT_RWERT"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      SqlStmt = "SELECT * INTO TBL_RWERTRZ IN '" & CnTmp.DataSource & "' FROM TBL_RWERTRZ"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
    Else

      '
      '
      'Tabelle RZ-Werte nur für nicht doppelte Rezept-Namen
      '
      '
      '
      '
      SqlStmt = "SELECT * INTO TBL_REZEPT IN '" & CnTmp.DataSource & "' FROM TBL_REZEPT" & " WHERE REZEPT_NAME NOT IN (SELECT REZEPT_NAME FROM TBL_REZEPT IN '" & Cndat.DataSource & "' WHERE MISCH_ID=" & MenueParam.MischID & ")"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      '
      '
      '
      SqlStmt = "SELECT * INTO TBL_REZEPT_FARBM IN '" & CnTmp.DataSource & "' FROM TBL_REZEPT_FARBM" & " WHERE REZEPT_ID IN (SELECT REZEPT_ID FROM TBL_REZEPT IN '" & CnTmp.DataSource & "')"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      '
      '
      SqlStmt = "SELECT * INTO TBL_REZEPT_RWERT IN '" & CnTmp.DataSource & "' FROM TBL_REZEPT_RWERT" & " WHERE REZEPT_ID IN (SELECT REZEPT_ID FROM TBL_REZEPT IN '" & CnTmp.DataSource & "')"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      '
      '
      '
      'Tabelle R-Werte nur für nicht doppelte R-Wert-Namen
      '
      '
      '
      '
      SqlStmt = "SELECT * INTO TBL_RWERTRZ IN '" & CnTmp.DataSource & "' FROM TBL_RWERTRZ" & " WHERE RWERT_ID IN (SELECT RWERT_ID FROM TBL_REZEPT_RWERT IN '" & CnTmp.DataSource & "')"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
    End If
    '
    '
    '
    '
    '
    'MISCH_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_REZEPT SET MISCH_ID=" & MenueParam.MischID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    '
    'MISCH_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_REZEPT_FARBM SET MISCH_ID=" & MenueParam.MischID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    '
    'MISCH_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_REZEPT_RWERT SET MISCH_ID=" & MenueParam.MischID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    'MESSGRW_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_REZEPT_RWERT SET MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '

    '
    '
    'MESSGRW_ID und MESSG_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_RWERTRZ SET MESSGRW_ID=" & MenueParam.Messg.MessgRwID & ", MESSG_ID=" & MenueParam.MessgID & ",USER_ID=" & MenueParam.UserID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '

    '
    '
    'Sortimente
    '
    '
    '
    '
    If TableExists("TBL_SORTI", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_SORTI"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    If TableExists("TBL_SORTI_FARBM", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_SORTI_FARBM"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    If TableExists("TBL_SORTI_RWERT", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_SORTI_RWERT"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If
    If TableExists("TBL_RWERTSO", CnTmp) Then
      SqlStmt = "DROP TABLE TBL_RWERTSO"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
    End If

    If chkDUPSO.CheckState = 0 Then
      SqlStmt = "SELECT * INTO TBL_SORTI IN '" & CnTmp.DataSource & "' FROM TBL_SORTI"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      SqlStmt = "SELECT * INTO TBL_SORTI_FARBM IN '" & CnTmp.DataSource & "' FROM TBL_SORTI_FARBM"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      SqlStmt = "SELECT * INTO TBL_SORTI_RWERT IN '" & CnTmp.DataSource & "' FROM TBL_SORTI_RWERT"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      SqlStmt = "SELECT * INTO TBL_RWERTSO IN '" & CnTmp.DataSource & "' FROM TBL_RWERTSO"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
    Else

      '
      '
      'Tabelle RZ-Werte nur für nicht doppelte Rezept-Namen
      '
      '
      '
      '
      SqlStmt = "SELECT * INTO TBL_SORTI IN '" & CnTmp.DataSource & "' FROM TBL_SORTI" & " WHERE SORTI_NAME NOT IN (SELECT SORTI_NAME FROM TBL_SORTI IN '" & Cndat.DataSource & "' WHERE MISCH_ID=" & MenueParam.MischID & ")"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      '
      '
      '
      SqlStmt = "SELECT * INTO TBL_SORTI_FARBM IN '" & CnTmp.DataSource & "' FROM TBL_SORTI_FARBM" & " WHERE SORTI_ID IN (SELECT SORTI_ID FROM TBL_SORTI IN '" & CnTmp.DataSource & "')"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      '
      '
      '
      '
      SqlStmt = "SELECT * INTO TBL_SORTI_RWERT IN '" & CnTmp.DataSource & "' FROM TBL_SORTI_RWERT" & " WHERE SORTI_ID IN (SELECT SORTI_ID FROM TBL_SORTI IN '" & CnTmp.DataSource & "')"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
      '
      '
      '
      'Tabelle R-Werte nur für nicht doppelte R-Wert-Namen
      '
      '
      '
      '
      SqlStmt = "SELECT * INTO TBL_RWERTSO IN '" & CnTmp.DataSource & "' FROM TBL_RWERTSO" & " WHERE RWERT_ID IN (SELECT RWERT_ID FROM TBL_SORTI_RWERT IN '" & CnTmp.DataSource & "')"
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnDBHilf) <> 0 Then
        Exit Sub
      End If
    End If
    '
    '
    '
    'MISCH_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_SORTI SET MISCH_ID=" & MenueParam.MischID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    'USER_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_SORTI SET USER_ID=" & MenueParam.UserID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    'MISCH_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_SORTI_FARBM SET MISCH_ID=" & MenueParam.MischID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    '
    'MISCH_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_SORTI_RWERT SET MISCH_ID=" & MenueParam.MischID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '
    'MESSGRW_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_SORTI_RWERT SET MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '

    '
    '
    'MESSGRW_ID und MESSG_ID neu setzen
    '
    '
    SqlStmt = "UPDATE TBL_RWERTSO SET MESSGRW_ID=" & MenueParam.Messg.MessgRwID & ", MESSG_ID=" & MenueParam.MessgID & ",USER_ID=" & MenueParam.UserID
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    '

    '
    'Dtmp.TableDefs.Refresh()
    '
    '
    '

    '
    '
    'Prüfen, ob Farbmitteldatei vorhanden
    '
    '
    '
    btnImport(1).Enabled = True
    SqlStmt = "SELECT * FROM TBL_FARBM"
    DynCmd.CommandText = SqlStmt
    DaRead = DataReader(DynCmd, CommandBehavior.Default, CnTmp)
    If Not DaRead.Read Or MenueParam.MischID = -1 Then
      btnImport(1).Enabled = False
      btnImport(3).Enabled = True
    End If
    DaRead.Close()
    btnZeig(1).Enabled = True
    btnRUN(1).Enabled = True
    Cursor = Cursors.Arrow
    btnDBase.Enabled = False
    panDURW.Enabled = False
    chkDUPRZ.Enabled = False
    chkDUPSO.Enabled = False
    btnImport(0).Enabled = False
    For i = 1 To TabImp.Count - 1
      TabImp(i).Clear()
    Next
    CnTmp.Close()
    CnDBHilf.Close()

  End Sub


  Private Sub btnRUN_Click(ByVal Sender As Object, ByVal eventArgs As System.EventArgs) _
  Handles btnRUN1.Click, btnRUN2.Click, btnRUN3.Click, btnRUN4.Click

    Dim Index As Short = CShort(Sender.name.substring(6, 1))
    Dim i As Integer
    Dim j As Integer
    Dim ier As Integer
    Dim RwertID As Integer
    Dim ianz As Integer
    Dim HandleDatabase As New DBHilfProgramme
    Dim TabQuell As New DataTable
    Dim TabZiel As New DataTable
    Dim DaZiel As OleDbDataReader
    Dim DaHilf As OleDbDataReader
    Dim CmdZiel As New OleDbCommand("", Cncol)
    Dim CmdQuell As New OleDbCommand("", CnTmp)
    Dim AdaptQuell As New OleDbDataAdapter
    Dim AdaptZiel As New OleDbDataAdapter
    Dim Islave As Short
    Dim NewFarbmID As Integer
    Dim GkwrtID As Integer
    Dim NeuFarbmID As Integer
    Dim NeuGlzGrdID As Integer

    Me.Cursor = Cursors.WaitCursor
    btnRUN(Index).Enabled = False
    AdaptQuell.SelectCommand = CmdQuell
    AdaptZiel.SelectCommand = CmdZiel
    Me.Enabled = False
    Select Case Index

      '
      '
      Case 1
        '
        '
        '
        'Prüfen, ob GK-Werte in Ordnung
        '
        '
        '
        '
        If Not ConnOpen(Cncol) Then
          Exit Sub
        End If
        If Not ConnOpen(Cndat) Then
          Exit Sub
        End If
        If Not ConnOpen(CnTmp) Then
          Exit Sub
        End If
        Erase FeldNamen
        SqlStmt = "SELECT * FROM TBL_GKWRT"
        CmdQuell.CommandText = SqlStmt
        CmdQuell.Connection = CnTmp()
        TabQuell.Rows.Clear()
        TabQuell.Columns.Clear()
        If Not FillDatset(AdaptQuell, TabQuell) Then
          Exit Sub
        End If
        If chkOVGK.CheckState = 1 Then
          ReDim FeldNamen(0)
          FeldNamen(0) = "GKWRT_ID"
          TabQuell.AcceptChanges()
          Call RowsAddUpdateDatabase("TBL_GKWRT", TabQuell.Select, FeldNamen, Cncol)
        End If
        '
        '
        '
        'Vergleich alte und neue GK-Werte
        '
        '
        '
        '
        CmdZiel.Connection = Cncol()
        For i = 0 To TabQuell.Rows.Count - 1
          SqlStmt = "SELECT * FROM TBL_GKWRT WHERE GKWRT_ID=" & TabQuell.Rows(i)("GKWRT_ID")
          CmdZiel.CommandText = SqlStmt
          DaZiel = DataReader(CmdZiel, CommandBehavior.SingleRow, Cncol)
          If DaZiel.Read Then
            '
            'Felder werden überprüft
            '
            '
            For j = 0 To TabQuell.Columns.Count - 1
              If Not (IsDBNull(TabQuell.Rows(i)(j)) Or IsDBNull(DaZiel(j))) AndAlso TabQuell.Rows(i)(j) <> DaZiel(j) Then
                MsgBox(Texxt(3640) & TabQuell.Rows(i)("gkwrt_id"))
                GoTo ErrEinst1
              End If
            Next j
          Else
            SqlStmt = "INSERT INTO TBL_GKWRT SELECT * FROM TBL_GKWRT IN '" & CnTmp.DataSource & "' WHERE GKWRT_ID=" & TabQuell.Rows(i)("gkwrt_id")
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
              Exit Sub
            End If
          End If
          DaZiel.Close()
        Next i
        TabQuell.Clear()
        '
        '
        '
        '
        'Laengenprüfung von TBL_GRUND_FARBM
        '
        '
        '
        LaeKmNWE = MenueParam.Messg.Winkel.Km * MenueParam.Messg.Winkel.Wsol.Nwe
        SqlStmt = "SELECT * FROM TBL_GRUND_FARBM"
        CmdQuell.CommandText = SqlStmt
        CmdQuell.Connection = CnTmp()
        TabQuell.Rows.Clear()
        TabQuell.Columns.Clear()
        If Not FillDatset(AdaptQuell, TabQuell) Then
          Exit Sub
        End If
        For i = 0 To TabQuell.Rows.Count - 1
          LaeLongB = UBound(TabQuell.Rows(i)("farbm_absstr")) + 1
          If LaeLongB Mod 4 * LaeKmNWE <> 0 Then
            MsgBox(Texxt(3643))
            GoTo ErrEinst1
          End If
        Next i
        TabQuell.Clear()
        '
        '
        '
        '
        'Farbmittel-ID überprüfen ggf. FarbID ändern
        '
        '
        '
        SqlStmt = "SELECT * FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID
        CmdQuell.CommandText = SqlStmt
        CmdQuell.Connection = CnTmp()
        TabQuell.Rows.Clear()
        TabQuell.Columns.Clear()
        If Not FillDatset(AdaptQuell, TabQuell) Then
          Exit Sub
        End If
        '
        '
        '
        NewFarbmID = MaxDBTableID("TBL_FARBM", "FARBM_ID", {"MISCH_ID"}, {MenueParam.MischID}, Cndat)
        '
        '
        'Prüfen, ob Name bzw. Prnr in Quelldatei vorhanden
        '
        Namelength = StringLength("TBL_FARBM", "FARBM_NAME", Cndat)
        '
        '
        For i = 0 To TabQuell.Rows.Count - 1
          NeuFarbmID = -1
          SqlStmt = ""
          If chkFAR(0).CheckState = 1 And chkFAR(1).CheckState = 1 Then
            SqlStmt = " AND FARBM_NAME='" & AddHkom(TabQuell.Rows(i)("farbm_name"), Namelength) & "' AND FARBM_PRNR='" & AddHkomE(TabQuell.Rows(i)("farbm_prnr")) & "'"
          ElseIf chkFAR(0).CheckState = 1 Then
            SqlStmt = " AND FARBM_NAME='" & AddHkomE(TabQuell.Rows(i)("farbm_name")) & "'"
          ElseIf chkFAR(1).CheckState = 1 Then
            SqlStmt = " AND FARBM_PRNR='" & AddHkomE(TabQuell.Rows(i)("farbm_prnr")) & "'"
          End If
          If SqlStmt = "" Then
            NewFarbmID = NewFarbmID + 1
            NeuFarbmID = NewFarbmID
          Else
            SqlStmt = "SELECT * FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID & SqlStmt
            CmdZiel.CommandText = SqlStmt
            DaZiel = DataReader(CmdZiel, CommandBehavior.SingleRow, Cndat)
            If DaZiel.Read Then
              '
              '
              'Farbnummer prüfen
              '
              If DaZiel("FARBM_ID") <> TabQuell.Rows(i)("FARBM_ID") Then
                NeuFarbmID = DaZiel("FARBM_ID")
              End If
              '
              '
            Else
              NewFarbmID = NewFarbmID + 1
              NeuFarbmID = NewFarbmID
            End If
          End If
          If NeuFarbmID >= 0 Then
            '
            '
            'Farbnummern in Quell-Tabellen ändern
            '
            'TBL_FARBM
           

            'FARBM_ID
            '
            SqlStmt = "UPDATE TBL_FARBM SET FARBM_ID=" & -NeuFarbmID & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              Exit Sub
            End If
            '
            '
            '
            '
            'GLZGRD_ID
            '
            '
            SqlStmt = "UPDATE TBL_FARBM SET GLZGRD_ID=" & -NeuFarbmID & " WHERE GLZGRD_ID=" & TabQuell.Rows(i)("FARBM_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              Exit Sub
            End If
            '
            '
            'TBL_FARBM_PREIS
            '
            '
            SqlStmt = "UPDATE TBL_FARBM_PREIS SET FARBM_ID=" & -NeuFarbmID & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              Exit Sub
            End If
            '
            '
            'TBL_FARBM_PROZ
            '
            '
            SqlStmt = "UPDATE TBL_FARBM_PROZ SET FARBM_ID=" & -NeuFarbmID & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              Exit Sub
            End If
            '
            '
            'TBL_FARBM_PROB
            '
            '
            SqlStmt = "UPDATE TBL_FARBM_PROB SET FARBM_ID=" & -NeuFarbmID & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              Exit Sub
            End If
            '
            '
            'TBL_GRUND_FARBM
            '
            '
            SqlStmt = "UPDATE TBL_GRUND_FARBM SET FARBM_ID=" & -NeuFarbmID & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              Exit Sub
            End If
            '
            'TBL_REZEPT_FARBM
            '
            '
            SqlStmt = "UPDATE TBL_REZEPT_FARBM SET FARBM_ID=" & -NeuFarbmID & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              Exit Sub
            End If
            '
            'TBL_SORTI_FARBM
            '
            '
            SqlStmt = "UPDATE TBL_SORTI_FARBM SET FARBM_ID=" & -NeuFarbmID & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              Exit Sub
            End If

          End If
          DaZiel.Close()
        Next i
        '

        '
        '
        'Negative FarbID's in positive verwandeln
        SqlStmt = "UPDATE TBL_FARBM SET FARBM_ID=-1*FARBM_ID WHERE FARBM_ID<0 AND MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If
        '
        '
        'TBL_FARBM_PREIS
        '
        '
        SqlStmt = "UPDATE TBL_FARBM_PREIS SET FARBM_ID=-1*FARBM_ID WHERE FARBM_ID<0 AND MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If
        '
        '
        'TBL_FARBM_PROZ
        '
        '
        SqlStmt = "UPDATE TBL_FARBM_PROZ SET FARBM_ID=-1*FARBM_ID WHERE FARBM_ID<0 AND MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If
        '
        '
        'TBL_FARBM_PROB
        '
        '
        SqlStmt = "UPDATE TBL_FARBM_PROB SET FARBM_ID=-1*FARBM_ID WHERE FARBM_ID<0 AND MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If
        '
        '
        'TBL_GRUND_FARBM
        '
        '
        SqlStmt = "UPDATE TBL_GRUND_FARBM SET FARBM_ID=-1*FARBM_ID WHERE FARBM_ID<0 AND MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If
        '
        'TBL_REZEPT_FARBM
        '
        '
        SqlStmt = "UPDATE TBL_REZEPT_FARBM SET FARBM_ID=-1*FARBM_ID WHERE FARBM_ID<0 AND MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If
        '
        'TBL_SORTI_FARBM
        '
        '
        SqlStmt = "UPDATE TBL_SORTI_FARBM SET FARBM_ID=-1*FARBM_ID WHERE FARBM_ID<0 AND MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If

        '
        'Negative GlzGrdID's in positive verwandeln
        SqlStmt = "UPDATE TBL_FARBM SET GLZGRD_ID=-1*GLZGRD_ID WHERE GLZGRD_ID<0 AND MISCH_ID=" & MenueParam.MischID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
          Exit Sub
        End If
        '

        TabQuell.Clear()
        '
        '
        '*****************************************************
        'Daten nach COLORDATA.MDB schreiben
        '
        '
        '
        '
        '
        'Vergleich Einträge Quelldatei mit Zieldatei für Farb-/Bindemittel
        '
        SqlStmt = "SELECT * FROM TBL_FARBM"
        CmdQuell.CommandText = SqlStmt
        CmdQuell.Connection = CnTmp()
        TabQuell.Rows.Clear()
        TabQuell.Columns.Clear()
        If Not FillDatset(AdaptQuell, TabQuell) Then
          Exit Sub
        End If
        TabQuell.AcceptChanges()
        '
        If Not ControlLength(TabQuell, Cndat, "TBL_FARBM", "FARBM_NAME", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If Not ControlLength(TabQuell, Cndat, "TBL_FARBM", "FARBM_ANAME", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If Not ControlLength(TabQuell, Cndat, "TBL_FARBM", "FARBM_BEM", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If Not ControlLength(TabQuell, Cndat, "TBL_FARBM", "FARBM_PRNR", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        '
        '
        For i = 0 To TabQuell.Rows.Count - 1
          SqlStmt = "SELECT * FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID")
          CmdZiel.CommandText = SqlStmt
          DaZiel = DataReader(CmdZiel, CommandBehavior.SingleRow, Cndat)
          If Not DaZiel.Read Then
            '
            'Einfügen des neuen Farb-/Bindemittels
            '
            '
            'TBL_FARBM
            '
            '
            '
            SqlStmt = "INSERT INTO TBL_FARBM SELECT * FROM TBL_FARBM IN '" & CnTmp.DataSource & "'" & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
              GoTo ErrEinst1
            End If
            '
            'TBL_FARBM_PROB
            '
            '
            SqlStmt = "INSERT INTO TBL_FARBM_PROB SELECT * FROM TBL_FARBM_PROB IN '" & CnTmp.DataSource & "'" & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
              GoTo ErrEinst1
            End If


            '
            '
            'TBL_FARBM_PREIS
            '
            '
            SqlStmt = "INSERT INTO TBL_FARBM_PREIS SELECT * FROM TBL_FARBM_PREIS IN '" & CnTmp.DataSource & "'" & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
              GoTo ErrEinst1
            End If

            '
            'TBL_FARBM_PROZ
            '
            '
            SqlStmt = "INSERT INTO TBL_FARBM_PROZ SELECT * FROM TBL_FARBM_PROZ IN '" & CnTmp.DataSource & "'" & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
              GoTo ErrEinst1
            End If
            '
            '
            'TBL_GRUND_FARBM
            '
            '
            SqlStmt = "INSERT INTO TBL_GRUND_FARBM SELECT * FROM TBL_GRUND_FARBM IN '" & CnTmp.DataSource & "'" & " WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
              GoTo ErrEinst1
            End If

          Else
            '
            '
            'Prüfen auf Übereinstimmung von Farb-/Bindemittelname bzw. Produktnummer
            '
            '
            '
            '
            If chkFAR(0).CheckState = 1 And chkFAR(1).CheckState = 1 Then
              If DaZiel("farbm_name") <> TabQuell.Rows(i)("farbm_name") Or DaZiel("farbm_prnr") <> TabQuell.Rows(i)("farbm_prnr") Then
                MsgBox(Texxt(4649) & " " & Texxt(901) & "(" & DaZiel("farbm_name") & "," & TabQuell.Rows(i)("farbm_name") & ")," & Texxt(917) & "(" & DaZiel("farbm_prnr") & "," & TabQuell.Rows(i)("farbm_prnr") & ")" & "(" & DaZiel("Farbm_ID") & ")")
                GoTo ErrEinst1
              End If
            ElseIf chkFAR(0).CheckState = 1 Then
              If DaZiel("farbm_name") <> TabQuell.Rows(i)("farbm_name") Then
                MsgBox(Texxt(4649) & " " & Texxt(901) & "(" & DaZiel("farbm_name") & "," & TabQuell.Rows(i)("farbm_name") & ")" & "(" & DaZiel("Farbm_ID") & ")")
                GoTo ErrEinst1
              End If
            ElseIf chkFAR(1).CheckState = 1 Then
              If DaZiel("farbm_prnr") <> TabQuell.Rows(i)("farbm_prnr") Then
                MsgBox(Texxt(4649) & " " & Texxt(917) & "(" & DaZiel("farbm_prnr") & "," & TabQuell.Rows(i)("farbm_prnr") & ")" & "(" & DaZiel("Farbm_ID") & ")")
                GoTo ErrEinst1
              End If
            End If
            If chkOVFA.CheckState = 1 Then
              '
              '
              'Farb-/Bindemittel von Zieltabelle mit Quelltabelle überschreiben (Inhalte von Zieltabellen =Inhalte von Quelltabellen (falls FARBM_ID in Quelltabelle enthalten)
              '
              'TBL_FARBM
              '
              '
              '
              SqlStmt = "SELECT * FROM TBL_FARBM WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
              CmdHilf.CommandText = SqlStmt
              CmdHilf.Connection = CnTmp()
              TabHilf.Clear()
              TabHilf.Columns.Clear()
              If Not FillDatset(AdaptHilf, TabHilf) Then
                Exit Sub
              End If
              ReDim FeldNamen(1)
              FeldNamen(0) = "MISCH_ID"
              FeldNamen(1) = "FARBM_ID"
              TabHilf.AcceptChanges()
              Call RowsAddUpdateDatabase("TBL_FARBM", TabHilf.Select, FeldNamen, Cndat)

              '
              ReDim FeldNamen(2)
              FeldNamen(0) = "MISCH_ID"
              FeldNamen(1) = "FARBM_ID"
              '
              '
              '
              'TBL_FARBM_PROB
              '
              '
              '
              FeldNamen(2) = "FARBM_IRBA"
              SqlStmt = "SELECT * FROM TBL_FARBM_PROB WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
              CmdHilf.CommandText = SqlStmt
              CmdHilf.Connection = CnTmp()
              TabHilf.Columns.Clear()
              TabHilf.Clear()
              If Not FillDatset(AdaptHilf, TabHilf) Then
                Exit Sub
              End If
              'DynHlf = Dtmp.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset, DAO.RecordsetOptionEnum.dbConsistent, DAO.RecordsetOptionEnum.dbReadOnly)
              TabHilf.AcceptChanges()
              Call RowsAddUpdateDatabase("TBL_FARBM_PROB", TabHilf.Select("", "FARBM_IRBA"), FeldNamen, Cndat)
              '
              '
              '
              'TBL_FARBM_PREIS
              '
              '
              '
              FeldNamen(2) = "FARBM_IRPA"
              SqlStmt = "SELECT * FROM TBL_FARBM_PREIS WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
              CmdHilf.CommandText = SqlStmt
              CmdHilf.Connection = CnTmp()
              TabHilf.Clear()
              TabHilf.Columns.Clear()
              If Not FillDatset(AdaptHilf, TabHilf) Then
                Exit Sub
              End If
              'DynHlf = Dtmp.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset, DAO.RecordsetOptionEnum.dbConsistent, DAO.RecordsetOptionEnum.dbReadOnly)
              TabHilf.AcceptChanges()
              Call RowsAddUpdateDatabase("TBL_FARBM_PREIS", TabHilf.Select("", "FARBM_IRPA"), FeldNamen, Cndat)

              '
              '
              'TBL_FARBM_PROZ
              '
              '
              '
              FeldNamen(2) = "FARBM_IRFA"
              SqlStmt = "SELECT * FROM TBL_FARBM_PROZ WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID
              CmdHilf.CommandText = SqlStmt
              CmdHilf.Connection = CnTmp()
              TabHilf.Clear()
              TabHilf.Columns.Clear()
              If Not FillDatset(AdaptHilf, TabHilf) Then
                Exit Sub
              End If
              'DynHlf = Dtmp.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset, DAO.RecordsetOptionEnum.dbConsistent, DAO.RecordsetOptionEnum.dbReadOnly)
              TabHilf.AcceptChanges()
              Call RowsAddUpdateDatabase("TBL_FARBM_PROZ", TabHilf.Select("", "FARBM_IRFA"), FeldNamen, Cndat)
            End If
            If chkOVGR.CheckState = 1 Then

              ReDim FeldNamen(3)
              FeldNamen(0) = "MISCH_ID"
              FeldNamen(1) = "FARBM_ID"
              '
              '
              'TBL_GRUND_FARBM
              '
              '
              '
              FeldNamen(2) = "MESSGRW_ID"
              FeldNamen(3) = "GKWRT_ID"
              SqlStmt = "SELECT * FROM TBL_GRUND_FARBM WHERE FARBM_ID=" & TabQuell.Rows(i)("Farbm_ID") & " AND MISCH_ID=" & MenueParam.MischID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID
              CmdHilf.CommandText = SqlStmt
              CmdHilf.Connection = CnTmp()
              TabHilf.Clear()
              TabHilf.Columns.Clear()
              If Not FillDatset(AdaptHilf, TabHilf) Then
                Exit Sub
              End If
              'DynHlf = Dtmp.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset, DAO.RecordsetOptionEnum.dbConsistent, DAO.RecordsetOptionEnum.dbReadOnly)
              TabHilf.AcceptChanges()
              Call RowsAddUpdateDatabase("TBL_GRUND_FARBM", TabHilf.Select, FeldNamen, Cndat)
            End If
          End If
          DaZiel.Close()
        Next i




        '
        '
        '
        '
        '
        'Nicht vorhandene Einträge für R-Wert-Gruppen übernehmen
        '
        '
        '
        '
        '
        '
        '
        'TBL_MESSG_GROUP
        '
        '
        '
        SqlStmt = "SELECT MESSG_ID,GROUP_ID FROM TBL_MESSG_GROUP"
        CmdQuell.CommandText = SqlStmt
        CmdQuell.Connection = CnTmp()
        TabQuell.Rows.Clear()
        TabQuell.Columns.Clear()
        If Not FillDatset(AdaptQuell, TabQuell) Then
          Exit Sub
        End If
        'Dynset = Dtmp.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset, DAO.RecordsetOptionEnum.dbConsistent, DAO.RecordsetOptionEnum.dbReadOnly)

        For i = 0 To TabQuell.Rows.Count - 1
          SqlStmt = "SELECT * FROM TBL_MESSG_GROUP WHERE MESSG_ID=" & MenueParam.MessgID & " AND GROUP_ID=" & TabQuell.Rows(i)("Group_id")
          CmdHilf.CommandText = SqlStmt
          CmdHilf.Connection = Cncol()
          DaHilf = DataReader(CmdHilf, CommandBehavior.SingleRow, Cncol)
          'DynHlf = DBas.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset)
          If Not DaHilf.Read Then
            SqlStmt = "INSERT INTO TBL_MESSG_GROUP SELECT * FROM TBL_MESSG_GROUP IN '" & CnTmp.DataSource & "'" & " WHERE MESSG_ID=" & MenueParam.MessgID & " AND GROUP_ID =" & TabQuell.Rows(i)("Group_id")
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
              Exit Sub
            End If
            'Call ColwinParamLIBDBHilfProgramme_definst.SQLExeLock(1, DBas, SqlStmt, ier)
          End If
          DaHilf.Close()
        Next i
        TabQuell.Clear()
        '
        '
        'Nicht vorhandene Einträge für Rezept-Gruppen übernehmen
        '
        '
        '
        'TBL_MISCH_GROUP
        '
        '
        '
        SqlStmt = "SELECT MISCH_ID,GROUP_ID FROM TBL_MISCH_GROUP"
        'Dynset = Dtmp.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset, DAO.RecordsetOptionEnum.dbConsistent, DAO.RecordsetOptionEnum.dbReadOnly)
        CmdQuell.CommandText = SqlStmt
        CmdQuell.Connection = CnTmp()
        TabQuell.Rows.Clear()
        TabQuell.Columns.Clear()
        If Not FillDatset(AdaptQuell, TabQuell) Then
          Exit Sub
        End If

        For i = 0 To TabQuell.Rows.Count - 1
          SqlStmt = "SELECT * FROM TBL_MISCH_GROUP WHERE MISCH_ID=" & MenueParam.MischID & " AND GROUP_ID=" & TabQuell.Rows(i)("Group_id")
          CmdHilf.CommandText = SqlStmt
          CmdHilf.Connection = Cncol()
          DaHilf = DataReader(CmdHilf, CommandBehavior.SingleRow, Cncol)
          'DynHlf = DBas.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset)
          If Not DaHilf.Read Then
            SqlStmt = "INSERT INTO TBL_MISCH_GROUP SELECT * FROM TBL_MISCH_GROUP IN '" & CnTmp.DataSource & "'" & " WHERE MISCH_ID=" & MenueParam.MischID & " AND GROUP_ID =" & TabQuell.Rows(i)("Group_id")
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
              Exit Sub
            End If
          End If
          DaHilf.Close()
        Next i
        TabQuell.Clear()
        '
        '
        'GKWrtID für neues Mischsystem übernehmen
        '
        GkwrtID = 0
        If TableExists("TBL_USER_MISCH_MESSG", CnTmp) Then
          SqlStmt = "SELECT * FROM TBL_USER_MISCH_MESSG"
          CmdHilf.CommandText = SqlStmt
          CmdHilf.Connection = CnTmp()
          DaHilf = DataReader(CmdHilf, CommandBehavior.SingleRow, CnTmp)
          'Dynset = Dtmp.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset)
          If DaHilf.Read Then
            GkwrtID = DaHilf("gkwrt_id")
          End If
          DaHilf.Close()
        End If
        SqlStmt = "UPDATE TBL_USER_MISCH_MESSG SET GKWRT_ID=" & GkwrtID & " WHERE USER_ID=" & MenueParam.UserID & " AND MISCH_ID=" & MenueParam.MischID & " AND MESSG_ID=" & MenueParam.MessgID
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
          Exit Sub
        End If
        '
        '
        '
        btnImport(2).Enabled = False
        btnImport(3).Enabled = False
        btnImport(4).Enabled = False
        '
        '
        'Prüfen, ob Einträge in R-Wert-Datei vorhanden
        '
        SqlStmt = "SELECT * FROM TBL_RWERT"
        CmdHilf.CommandText = SqlStmt
        CmdHilf.Connection = CnTmp()
        DaHilf = DataReader(CmdHilf, CommandBehavior.Default, CnTmp)
        If DaHilf.Read Then
          btnImport(3).Enabled = True
        Else
          MsgBox(Texxt(4685) & Space(1) & Texxt(2984))
        End If
        DaHilf.Close()
        '
        '
        '
        'Prüfen, ob Einträge in Rezeptdatei vorhanden
        '
        SqlStmt = "SELECT * FROM TBL_REZEPT"
        CmdHilf.CommandText = SqlStmt
        CmdHilf.Connection = CnTmp()
        DaHilf = DataReader(CmdHilf, CommandBehavior.Default, CnTmp)
        If DaHilf.Read Then
          btnImport(2).Enabled = True
        Else
          MsgBox(Texxt(730) & Space(1) & Texxt(4703))

        End If
        DaHilf.Close()
        '
        '
        '
        'Prüfen, ob Einträge in Sortimentsdatei vorhanden
        '
        SqlStmt = "SELECT * FROM TBL_SORTI"
        CmdHilf.CommandText = SqlStmt
        CmdHilf.Connection = CnTmp()
        DaHilf = DataReader(CmdHilf, CommandBehavior.Default, CnTmp)
        If DaHilf.Read Then
          btnImport(4).Enabled = True
        Else
          MsgBox(Texxt(800) & Space(1) & Texxt(4703))
          btnImport(4).Enabled = True
        End If
        DaHilf.Close()
        btnImport(1).Enabled = False
        Cncol.Close()
        CnTmp.Close()
        Cndat.Close()

        '
        '

      Case 2
        '
        '
        If Not ConnOpen(Cndat) Then
          Exit Sub
        End If
        If Not ConnOpen(CnTmp) Then
          Exit Sub
        End If
        AdaptHilf.SelectCommand.CommandText = "SELECT FARBM_ID FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID
        AdaptHilf.SelectCommand.Connection = Cndat()
        TabHilf.Rows.Clear()
        TabHilf.Columns.Clear()
        If Not FillDatset(AdaptHilf, TabHilf) Then
          Exit Sub
        End If
        TabHilf.AcceptChanges()
        StrHilf = StrLin(TabHilf, "FARBM_ID")
        '
        '
        'Prüfen, ob nur Rezepte mit vorhandenen Farbmittel
        '
        '
        '
        SqlStmt = "SELECT FARBM_ID FROM TBL_REZEPT_FARBM WHERE FARBM_ID NOT IN " & StrHilf
        CmdHilf.CommandText = SqlStmt
        CmdHilf.Connection = CnTmp()
        DaHilf = DataReader(CmdHilf, CommandBehavior.Default, CnTmp)
        If DaHilf.Read Then
          MsgBox(Texxt(3644))
          GoTo ErrEinst1
        End If
        DaHilf.Close()
        '
        'Index für Rezepte neu festlegen
        '
        '
        '
        '
        '
        MaxID = MaxDBTableID("TBL_REZEPT", "REZEPT_ID", {"MISCH_ID"}, {MenueParam.MischID}, Cndat) + 1
        Erase AltID
        Erase NeuID
        '
        ianz = 0
        SqlStmt = "SELECT * FROM TBL_REZEPT"
        CmdHilf.CommandText = SqlStmt
        CmdHilf.Connection = CnTmp()
        TabHilf.Rows.Clear()
        TabHilf.Columns.Clear()
        If Not FillDatset(AdaptHilf, TabHilf) Then
          Exit Sub
        End If

        For i = 0 To TabHilf.Rows.Count - 1
          ReDim Preserve AltID(ianz)
          ReDim Preserve NeuID(ianz)
          AltID(ianz) = TabHilf.Rows(i)("rezept_id")
          NeuID(ianz) = MaxID + ianz
          ianz = ianz + 1
        Next i
        TabHilf.Clear()
        '
        'Index für R-Werte neu festlegen
        '
        '
        Islave = UBound(SlaveRezeptNames)
        Call NewIndices("REZEPT_ID", ianz, AltID, NeuID, "TBL_REZEPT", SlaveRezeptNames, CnTmp)


        '
        '
        'R-Werte neu indizieren
        '
        '
        ReDim SlaveHlfTableNames(0)
        SlaveHlfTableNames(0) = "TBL_REZEPT_RWERT"
        Call NewRwertIndices("TBL_RWERTRZ", MenueParam.Messg.MessgRwID, SlaveHlfTableNames, CnTmp)

        '
        '
        '
        '
        'Daten nach COLORDATA.MDB schreiben
        '
        '
        '
        '
        '
        'Prüfen, ob Gruppe R-Werte vorhanden
        '
        '
        '
        '
        '
        SqlStmt = "SELECT * FROM TBL_RWERTRZ"
        CmdQuell.CommandText = SqlStmt
        CmdQuell.Connection = CnTmp()
        TabQuell.Rows.Clear()
        TabQuell.Columns.Clear()
        If Not FillDatset(AdaptQuell, TabQuell) Then
          Exit Sub
        End If
        '
        'Prüfen, ob Länge in Ordnung
        '
        If Not ControlLength(TabQuell, Cndat, "TBL_RWERT", "RWERT_NAME", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If Not ControlLength(TabQuell, Cndat, "TBL_RWERT", "RWERT_BEM", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If Not ControlLength(TabQuell, Cndat, "TBL_RWERT", "RWERT_KENN", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        '
        DynCmd.Connection = Cncol()
        For i = 0 To TabQuell.Rows.Count - 1
          SqlStmt = "SELECT * FROM TBL_MESSG_GROUP WHERE MESSG_ID=" & MenueParam.MessgID & " AND GROUP_ID=" & TabQuell.Rows(i)("RWERT_GID")
          CmdHilf.CommandText = SqlStmt
          DaHilf = DataReader(CmdHilf, CommandBehavior.SingleRow, Cncol)
          If Not DaHilf.Read Then
            MsgBox(Texxt(3635))
            DaHilf.Close()
            GoTo ErrEinst1
          End If
          DaHilf.Close()
        Next
        TabQuell.Clear()

        '
        '
        '
        'Prüfen, ob Gruppe Rezepte vorhanden
        '
        '
        '
        SqlStmt = "SELECT * FROM TBL_REZEPT"
        CmdQuell.CommandText = SqlStmt
        CmdQuell.Connection = CnTmp()
        TabQuell.Rows.Clear()
        TabQuell.Columns.Clear()
        If Not FillDatset(AdaptQuell, TabQuell) Then
          Exit Sub
        End If
        '
        '
        '
        '
        'Prüfen, ob Länge in Ordnung
        '
        If Not ControlLength(TabQuell, Cndat, "TBL_REZEPT", "REZEPT_NAME", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If Not ControlLength(TabQuell, Cndat, "TBL_REZEPT", "REZEPT_BEM", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        '
        '
        '
        '
        '
        DynCmd.Connection = Cncol()
        For i = 0 To TabQuell.Rows.Count - 1
          SqlStmt = "SELECT * FROM TBL_MISCH_GROUP WHERE MISCH_ID=" & MenueParam.MischID & " AND GROUP_ID=" & TabQuell.Rows(i)("REZEPT_GID")
          CmdHilf.CommandText = SqlStmt
          DaHilf = DataReader(CmdHilf, CommandBehavior.SingleRow, Cncol)
          If Not DaHilf.Read Then
            If MessageBox.Show(Texxt(3635) & Chr(13) & Texxt(1253), Texxt(2000), MessageBoxButtons.YesNo) = Forms.DialogResult.No Then
              GoTo ErrEinst1
            Else
              SqlStmt = "INSERT INTO TBL_MISCH_GROUP ([MISCH_ID],[GROUP_ID],[GROUP_KBEZ],[GROUP_NAME])" & " VALUES(" & MenueParam.MischID & "," & TabQuell.Rows(i)("Rezept_gid") & ",'" & Texxt(3019) & "','" & Texxt(3019) & "')"
              DynCmd.CommandText = SqlStmt
              If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
                Exit Sub
              End If
            End If
          End If
          DaHilf.Close()
        Next
        TabQuell.Clear()
        '
        '
        '
        '
        'Tabelle nach ddat einfügen
        '
        '
        '
        'TBL_REZEPT
        '
        '
        CnTmp.Close()
        SqlStmt = "INSERT INTO TBL_REZEPT SELECT * FROM TBL_REZEPT IN '" & CnTmp.DataSource & "'"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst1
        End If
        '
        '
        'TBL_RWERTRZ
        '
        '
        SqlStmt = "INSERT INTO TBL_RWERT SELECT * FROM TBL_RWERTRZ IN '" & CnTmp.DataSource & "'"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst1
        End If
        '
        'TBL_REZEPT_FARBM
        '
        '
        SqlStmt = "INSERT INTO TBL_REZEPT_FARBM SELECT * FROM TBL_REZEPT_FARBM IN '" & CnTmp.DataSource & "'"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst1
        End If
        '
        '
        '
        '
        'TBL_REZEPT_RWERT
        '
        '
        SqlStmt = "INSERT INTO TBL_REZEPT_RWERT SELECT * FROM TBL_REZEPT_RWERT IN '" & CnTmp.DataSource & "'"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst1
        End If
        '
        '
        '
        '
        btnImport(Index).Enabled = False
        Cndat.Close()
        CnTmp.Close()

      Case 3
        '
        '
        If Not ConnOpen(Cndat) Then
          Exit Sub
        End If
        If Not ConnOpen(CnTmp) Then
          Exit Sub
        End If
        '
        'TBL_MESSG_GROUP
        '
        '
        '
        SqlStmt = "SELECT MESSG_ID,GROUP_ID FROM TBL_MESSG_GROUP"
        CmdQuell.CommandText = SqlStmt
        CmdQuell.Connection = CnTmp()
        TabQuell.Rows.Clear()
        TabQuell.Columns.Clear()
        If Not FillDatset(AdaptQuell, TabQuell) Then
          Exit Sub
        End If
        For i = 0 To TabQuell.Rows.Count - 1
          SqlStmt = "SELECT * FROM TBL_MESSG_GROUP WHERE MESSG_ID=" & MenueParam.MessgID & " AND GROUP_ID=" & TabQuell.Rows(i)("Group_id")
          CmdHilf.CommandText = SqlStmt
          DaHilf = DataReader(CmdHilf, CommandBehavior.SingleRow, Cncol)
          If Not DaHilf.Read Then
            SqlStmt = "INSERT INTO TBL_MESSG_GROUP SELECT * FROM TBL_MESSG_GROUP IN '" & CnTmp.DataSource & "'" & " WHERE MESSG_ID=" & MenueParam.MessgID & " AND GROUP_ID =" & TabQuell.Rows(i)("Group_id")
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, Cncol) <> 0 Then
              GoTo ErrEinst1
            End If
          End If
          DaHilf.Close()
        Next i
        TabQuell.Clear()
        '
        '
        '
        '
        '
        '
        '
        'Prüfen, ob Gruppe R-Werte vorhanden
        '
        '
        '
        '
        '
        SqlStmt = "SELECT * FROM TBL_RWERT"
        CmdQuell.CommandText = SqlStmt
        CmdQuell.Connection = CnTmp()
        TabQuell.Rows.Clear()
        TabQuell.Columns.Clear()
        If Not FillDatset(AdaptQuell, TabQuell) Then
          Exit Sub
        End If
        '
        '
        'Prüfen, ob Länge in Ordnung
        '
        If Not ControlLength(TabQuell, Cndat, "TBL_RWERT", "RWERT_NAME", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If Not ControlLength(TabQuell, Cndat, "TBL_RWERT", "RWERT_BEM", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If Not ControlLength(TabQuell, Cndat, "TBL_RWERT", "RWERT_KENN", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        '
        '
        DynCmd.Connection = Cncol()
        For i = 0 To TabQuell.Rows.Count - 1
          SqlStmt = "SELECT * FROM TBL_MESSG_GROUP WHERE MESSG_ID=" & MenueParam.MessgID & " AND GROUP_ID=" & TabQuell.Rows(i)("RWERT_GID")
          CmdHilf.CommandText = SqlStmt
          DaHilf = DataReader(CmdHilf, CommandBehavior.SingleRow, Cncol)
          If Not DaHilf.Read Then
            MsgBox(Texxt(3635))
            DaHilf.Close()
            GoTo ErrEinst1
          End If
          DaHilf.Close()
        Next
        '
        '
        If radDURW(0).Checked Then
          Refel = New RefValue
          '
          'Doppelte Namen der R-Werte überschreiben
          '
          For i = 0 To TabQuell.Rows.Count - 1
            RwertID = ReWrRwert.ReadRwertID(TabQuell.Rows(i)("RWERT_NAME"), ier)
            '
            If RwertID >= 0 Then
              '
              'Überschreiben
              '
              Call RowToRef(TabQuell.Rows(i), Refel, ier)
              Refel.ID = RwertID


              ReWrRwert.UpdateRwert(Refel, ier)

              If ier <> 0 Then
                GoTo ErrEinst1
              End If
              '
              'Doppelten R-Wert aus temporärer Datei löschen löschen
              '
              SqlStmt = "DELETE * FROM TBL_RWERT WHERE RWERT_ID=" & TabQuell.Rows(i)("RWERT_ID")
              DynCmd.CommandText = SqlStmt
              If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
                GoTo ErrEinst1
              End If
            End If
          Next i
          Refel.dispose()

        End If
        TabQuell.Clear()

        '
        'Indices für R-Werte neu festlegen
        '
        '
        '
        Erase SlaveHlfTableNames
        Call NewRwertIndices("TBL_RWERT", MenueParam.Messg.MessgRwID, SlaveHlfTableNames, CnTmp)
        CnTmp.Close()


        '
        '
        '
        'ColwinGlobalGlobalParameter_definst.Worksp.BeginTrans()
        '
        '
        'Tabelle nach Ddat einfügen
        '
        '
        '
        SqlStmt = "INSERT INTO TBL_RWERT SELECT * FROM TBL_RWERT IN '" & CnTmp.DataSource & "'"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst1
        End If
        '
        btnImport(Index).Enabled = False
        CnTmp.Close()
        Cndat.Close()
      Case 4
        '
        '
        '
        '
        If Not ConnOpen(Cndat) Then
          Exit Sub
        End If
        If Not ConnOpen(CnTmp) Then
          Exit Sub
        End If
        '
        '
        '
        'Prüfen, ob nur Sortimente mit vorhandenen Farbmittel
        '
        '
        '
        SqlStmt = "SELECT FARBM_ID FROM TBL_SORTI_FARBM WHERE FARBM_ID NOT IN " & "(SELECT FARBM_ID FROM TBL_FARBM IN '" & Cndat.DataSource & "' WHERE MISCH_ID=" & MenueParam.MischID & ")"
        CmdHilf.CommandText = SqlStmt
        CmdHilf.Connection = CnTmp()
        DaHilf = DataReader(CmdHilf, CommandBehavior.Default, CnTmp)

        If DaHilf.Read Then
          MsgBox(Texxt(3644))
          GoTo ErrEinst1
        End If
        DaHilf.Close()
        '
        'Index für Sortimente neu festlegen
        '
        '
        '
        '
        '
        MaxID = MaxDBTableID("TBL_SORTI", "SORTI_ID", {"MISCH_ID"}, {MenueParam.MischID}, Cndat) + 1
        Erase AltID
        Erase NeuID
        '
        ianz = 0
        SqlStmt = "SELECT * FROM TBL_SORTI"
        CmdHilf.CommandText = SqlStmt
        CmdHilf.Connection = CnTmp()
        TabHilf.Rows.Clear()
        TabHilf.Columns.Clear()
        If Not FillDatset(AdaptHilf, TabHilf) Then
          Exit Sub
        End If
        '
        'Prüfen, ob Länge in Ordnung
        '
        '
        '
        If Not ControlLength(TabHilf, Cndat, "TBL_SORTI", "SORTI_NAME", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If Not ControlLength(TabHilf, Cndat, "TBL_SORTI", "SORTI_BEM", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        '
        '
        '
        '
        For i = 0 To TabHilf.Rows.Count - 1
          ReDim Preserve AltID(ianz)
          ReDim Preserve NeuID(ianz)
          AltID(ianz) = TabHilf.Rows(i)("SORTI_ID")
          NeuID(ianz) = MaxID + ianz
          ianz = ianz + 1
        Next i
        TabHilf.Clear()
        '
        'Index für R-Werte neu festlegen
        '
        '
        Call NewIndices("SORTI_ID", ianz, AltID, NeuID, "TBL_SORTI", SlaveSortiNames, CnTmp)


        '
        '
        'R-Werte neu indizieren
        '
        '
        ReDim SlaveHlfTableNames(0)
        SlaveHlfTableNames(0) = "TBL_SORTI_RWERT"
        Call NewRwertIndices("TBL_RWERTSO", MenueParam.Messg.MessgRwID, SlaveHlfTableNames, CnTmp)
        '
        '
        '
        '
        'Daten nach COLORDATA.MDB schreiben
        '
        '
        '
        '
        '
        'Prüfen, ob Gruppe R-Werte vorhanden
        '
        '
        '
        '
        '
        SqlStmt = "SELECT * FROM TBL_RWERTSO"
        CmdQuell.CommandText = SqlStmt
        CmdQuell.Connection = CnTmp()
        TabQuell.Rows.Clear()
        TabQuell.Columns.Clear()
        If Not FillDatset(AdaptQuell, TabQuell) Then
          Exit Sub
        End If
        '
        'Prüfen, ob Länge in Ordnung
        '
        If Not ControlLength(TabQuell, Cndat, "TBL_RWERT", "RWERT_NAME", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If Not ControlLength(TabQuell, Cndat, "TBL_RWERT", "RWERT_BEM", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        If Not ControlLength(TabQuell, Cndat, "TBL_RWERT", "RWERT_KENN", False) Then
          Cursor = Cursors.Default
          Exit Sub
        End If
        '
        '
        DynCmd.Connection = Cncol()
        For i = 0 To TabQuell.Rows.Count - 1
          SqlStmt = "SELECT * FROM TBL_MESSG_GROUP WHERE MESSG_ID=" & MenueParam.MessgID & " AND GROUP_ID=" & TabQuell.Rows(i)("RWERT_GID")
          CmdHilf.CommandText = SqlStmt
          DaHilf = DataReader(CmdHilf, CommandBehavior.SingleRow, Cncol)
          If Not DaHilf.Read Then
            MsgBox(Texxt(3635))
            DaHilf.Close()
            GoTo ErrEinst1
          End If
          DaHilf.Close()
        Next
        TabQuell.Clear()

        '
        '
        '
        '
        '
        '
        'Tabelle nach ddat einfügen
        '
        '
        '
        'TBL_SORTI
        '
        '
        CnTmp.Close()
        SqlStmt = "INSERT INTO TBL_SORTI SELECT * FROM TBL_SORTI IN '" & CnTmp.DataSource & "'"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst1
        End If
        'Call ColwinParamLIBDBHilfProgramme_definst.SQLExeLock(1, Ddat, SqlStmt, ier)

        '
        '
        'TBL_RWERTRZ
        '
        '
        SqlStmt = "INSERT INTO TBL_RWERT SELECT * FROM TBL_RWERTSO IN '" & CnTmp.DataSource & "'"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst1
        End If

        '
        'TBL_REZEPT_FARBM
        '
        '
        SqlStmt = "INSERT INTO TBL_SORTI_FARBM SELECT * FROM TBL_SORTI_FARBM IN '" & CnTmp.DataSource & "'"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst1
        End If

        '
        '
        '
        '
        'TBL_REZEPT_RWERT
        '
        '
        SqlStmt = "INSERT INTO TBL_SORTI_RWERT SELECT * FROM TBL_SORTI_RWERT IN '" & CnTmp.DataSource & "'"
        DynCmd.CommandText = SqlStmt
        If SQLExeNonQuery(DynCmd, Cndat) <> 0 Then
          GoTo ErrEinst1
        End If

        '
        '
        '
        '
        '
        '
        '
        '
        btnImport(Index).Enabled = False
        CnTmp.Close()
        Cndat.Close()
    End Select
    '
    '
    '
    '
    '
    '
    '
    '
    '
ErrEinst1:

    TabQuell.Dispose()
    TabZiel.Dispose()
    CmdZiel.Dispose()
    CmdQuell.Dispose()
    AdaptQuell.Dispose()
    AdaptZiel.Dispose()
    Me.Cursor = System.Windows.Forms.Cursors.Arrow
    Me.Enabled = True
    Cncol.Close()
    Cndat.Close()
    CnTmp.Close()
    btnZeig(Index).Enabled = False
  End Sub

  Sub NewIndices(ByRef FieldName As String, ByRef Ianz As Short, ByRef AltID() As Integer, ByRef NeuID() As Integer, ByRef MasterTableName As String, ByRef SlaveTableNames() As String, ByRef CnTmp As OleDbConnection)
    Dim SqlStmt As String
    Dim i As Short
    Dim k As Short
    Dim DynCmd As New OleDbCommand("", CnTmp)
    'Feld umbenennen und neu hinzufügen für Mastertable
    '
    '
    '
    '
    'Feld-ID's neu setzen (zuerst auf negativen Wert (Zweideutigkeiten vermeiden))
    '
    '

    For k = 0 To Ianz - 1
      'MasterTable.Close
      SqlStmt = "UPDATE " & MasterTableName & " SET [" & FieldName & "]=" & -NeuID(k) & " WHERE [" & FieldName & "]=" & AltID(k)
      DynCmd.CommandText = SqlStmt
      If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
        Exit Sub
      End If
      If Not IsNothing(SlaveTableNames) Then
        For i = 0 To UBound(SlaveTableNames)
          If TableExists(SlaveTableNames(i), CnTmp) Then
            SqlStmt = "UPDATE " & SlaveTableNames(i) & " SET [" & FieldName & "]=" & -NeuID(k) & " WHERE [" & FieldName & "]=" & AltID(k)
            DynCmd.CommandText = SqlStmt
            If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
              Exit Sub
            End If
          End If
        Next i
      End If
    Next k
    '
    '
    'negative Werte in positive Werte verwandeln
    '
    '
    SqlStmt = "UPDATE " & MasterTableName & " SET [" & FieldName & "]=-1*[" & FieldName & "]"
    DynCmd.CommandText = SqlStmt
    If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
      Exit Sub
    End If
    If Not IsNothing(SlaveTableNames) Then
      For i = 0 To UBound(SlaveTableNames)
        If TableExists(SlaveTableNames(i), CnTmp) Then
          SqlStmt = "UPDATE " & SlaveTableNames(i) & " SET [" & FieldName & "]=-1*[" & FieldName & "]"
          DynCmd.CommandText = SqlStmt
          If SQLExeNonQuery(DynCmd, CnTmp) <> 0 Then
            Exit Sub
          End If
        End If
      Next i
    End If
  End Sub





  Private Sub btnZEIG_Click(ByVal Sender As Object, ByVal eventArgs As System.EventArgs) _
  Handles btnZEIG1.Click, btnZeig2.Click, btnZeig3.Click, btnZeig4.Click

    Dim Index As Short = CShort(Sender.name.substring(7, 1))
    Me.Cursor = System.Windows.Forms.Cursors.WaitCursor
    Me.Enabled = False
    TabImp(Index).Rows.Clear()
    TabHilf.Rows.Clear()
    TabHilf.Columns.Clear()
    Select Case Index
      Case 1
        '
        '
        '
        '
        '
        '
        SqlStmt = "SELECT FARBM_ID,FARBM_DATTIM,FARBM_NAME,FARBM_PRNR FROM TBL_FARBM"
        SqlStmt = SqlStmt & " ORDER BY FARBM_ID"
        DynCmd.CommandText = SqlStmt
        DynCmd.Connection = CnTmp()
        If Not FillDatset(DynAdapt, TabImp(Index)) Then
          Exit Sub
        Else
          If TabImp(Index).Rows.Count = 0 Then
            MsgBox(Texxt(2973))
            Cursor = Cursors.Arrow
            Me.Enabled = True
            Exit Sub
          End If
        End If
        dbgIMP(Index).Columns(0).Visible = False
        dbgIMP(Index).Columns(1).HeaderText = Texxt(375)
        dbgIMP(Index).Columns(2).HeaderText = Texxt(394)
        dbgIMP(Index).Columns(3).HeaderText = Texxt(917)

        btnRUN(Index).Enabled = True
        txtANZ(1).Text = TabImp(Index).Rows.Count
        '
        '
      Case 2
        '
        '
        '
        '
        '
        '
        '
        '
        SqlStmt = "SELECT * FROM TBL_RWERTRZ"
        DynCmd.CommandText = SqlStmt
        DynCmd.Connection = CnTmp()
        DaRead = DataReader(DynCmd, CommandBehavior.SingleRow, CnTmp)
        If Not DaRead.Read Then
        End If
        DaRead.Close()
        '

        CmdHilf.CommandText = "SELECT REZEPT_ID FROM TBL_REZEPT"
        CmdHilf.Connection = CnTmp()
        If Not FillDatset(AdaptHilf, TabHilf) Then
          Exit Sub
        End If
        StrHilf = StrLin(TabHilf, "REZEPT_ID")
        SqlStmt = "SELECT REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME,REZEPT_BEM FROM TBL_REZEPT  WHERE REZEPT_ID IN " & StrHilf

        SqlStmt = SqlStmt & " ORDER BY REZEPT_ID"
        DynCmd.CommandText = SqlStmt
        DynCmd.Connection = CnDBHilf
        If Not FillDatset(DynAdapt, TabImp(Index)) Then
          Exit Sub
        Else
          If TabImp(Index).Rows.Count = 0 Then
            MsgBox(Texxt(3664))
            Cursor = Cursors.Arrow
            Me.Enabled = True
            Exit Sub
          End If
        End If
        '
        '
        dbgIMP(Index).Columns(0).Visible = False
        dbgIMP(Index).Columns(1).HeaderText = Texxt(375)
        dbgIMP(Index).Columns(2).HeaderText = Texxt(394)
        dbgIMP(Index).Columns(3).HeaderText = Texxt(916)
        btnRUN(Index).Enabled = True

      Case 3
        '

        '
        'Suche Master-Tabelle (TBL_RWERT)
        '
        '
        '
        '
        'Prüfen, ob Meßgerät i.O.
        '
        '
        '

        SqlStmt = "SELECT * FROM TBL_RWERT"
        DynCmd.CommandText = SqlStmt
        DynCmd.Connection = CnTmp()
        DaRead = DataReader(DynCmd, CommandBehavior.SingleRow, CnTmp)
        Application.DoEvents()
        If Not DaRead.Read Then
        End If
        DaRead.Close()
        '
        ' 
        '
        CmdHilf.CommandText = "SELECT RWERT_ID FROM TBL_RWERT"
        CmdHilf.Connection = CnTmp()
        If Not FillDatset(AdaptHilf, TabHilf) Then
          Exit Sub
        End If
        StrHilf = StrLin(TabHilf, "RWERT_ID")

        SqlStmt = "SELECT RWERT_ID,RWERT_DATTIM,RWERT_NAME,RWERT_BEM FROM TBL_RWERT WHERE RWERT_ID IN " & StrHilf
        DynCmd.CommandText = SqlStmt
        DynCmd.Connection = CnDBHilf
        Application.DoEvents()
        If Not FillDatset(DynAdapt, TabImp(Index)) Then
          Exit Sub
        Else
          dbgIMP(Index).Columns(0).Visible = False
          dbgIMP(Index).Columns(1).HeaderText = Texxt(375)
          dbgIMP(Index).Columns(2).HeaderText = Texxt(394)
          dbgIMP(Index).Columns(3).HeaderText = Texxt(916)
          If TabImp(Index).Rows.Count = 0 Then
            MsgBox(Texxt(2959))
            Cursor = Cursors.Arrow
            Me.Enabled = True
            Exit Sub
          End If
        End If
        '

        btnRUN(Index).Enabled = True
        '

        '


      Case 4
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
        SqlStmt = "SELECT * FROM TBL_RWERTSO"
        DynCmd.CommandText = SqlStmt
        DynCmd.Connection = CnTmp()
        DaRead = DataReader(DynCmd, CommandBehavior.SingleRow, CnTmp)
        If Not DaRead.Read Then
        End If
        DaRead.Close()
        '
        '
        '
        CmdHilf.CommandText = "SELECT SORTI_ID FROM TBL_SORTI"
        CmdHilf.Connection = CnTmp()
        If Not FillDatset(AdaptHilf, TabHilf) Then
          Exit Sub
        End If
        StrHilf = StrLin(TabHilf, "SORTI_ID")

        'SqlStmt = "SELECT SORTI_ID,SORTI_DATTIM,SORTI_NAME,SORTI_BEM FROM TBL_SORTI" & " WHERE SORTI_ID IN (SELECT SORTI_ID FROM " & TblSortiList & " IN '" & CnTmp.DataSource & "')"
        SqlStmt = "SELECT SORTI_ID,SORTI_DATTIM,SORTI_NAME,SORTI_BEM FROM TBL_SORTI" & " WHERE SORTI_ID IN " & StrHilf
        SqlStmt = SqlStmt & " ORDER BY SORTI_ID"
        DynCmd.CommandText = SqlStmt
        DynCmd.Connection = CnDBHilf
        If Not FillDatset(DynAdapt, TabImp(Index)) Then
          Exit Sub
        Else
          If TabImp(Index).Rows.Count = 0 Then
            MsgBox(Texxt(3664))
            Cursor = Cursors.Arrow
            Me.Enabled = True
            Exit Sub
          End If
        End If
        '
        '
        dbgIMP(Index).Columns(0).Visible = False
        dbgIMP(Index).Columns(1).HeaderText = Texxt(375)
        dbgIMP(Index).Columns(2).HeaderText = Texxt(394)
        dbgIMP(Index).Columns(3).HeaderText = Texxt(916)
        btnRUN(Index).Enabled = True
    End Select
    txtANZ(Index).Text = TabImp(Index).Rows.Count
    dbgIMP(Index).AllowUserToAddRows = False
    dbgIMP(Index).AllowUserToDeleteRows = False
    dbgIMP(Index).AutoResizeColumns()
    Me.Enabled = True
    Me.Cursor = Cursors.Arrow
    CnTmp.Close()
    CnDBHilf.Close()
  End Sub

  'UPGRADE_WARNING: Form Ereignis frmDBImport.Activate hat ein neues Verhalten. Klicken Sie hier für weitere Informationen: 'ms-help://MS.VSCC.v80/dv_commoner/local/redirect.htm?keyword="6BA9B8D2-2A32-4B6E-8D36-44949974A5B4"'
  Private Sub frmDBImport_Activated(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Activated
    Dim i As Integer
    If IsNothing(btnImport) Then Exit Sub
    'Call EinstAct((AufbauPar.ier), Me)
    If MnResz Then
      Call ResizeChild(Me)
    End If
    For i = 1 To btnImport.Count - 1
      btnImport(i).Enabled = False
      SplitCONT(i).Hide()
    Next
    btnImport(0).Enabled = True
    PanelDBase.Show()
  End Sub
  Private Sub frmDBImportCOL_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize, Me.Shown, Me.Activated

    If MnResz Then
      Call ResizeChild(Me)
    End If

  End Sub

  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()
    '
    'Prüfen, ob CNTMP vorhanden
    '
    '
    '
    If Not File.Exists(CnTmp.DataSource) Then
      Call CreateDatabase(CnTmp)
    End If
  End Sub

  Protected Overrides Sub Finalize()



    MyBase.Finalize()
  End Sub



  Private Sub btnImport_Click(ByVal sender As Object, ByVal e As System.EventArgs) _
  Handles btnImport0.Click, btnImport1.Click, btnImport2.Click, btnImport3.Click, btnImport4.Click
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




End Class