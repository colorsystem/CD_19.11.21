Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmMEI
  REM Laufvariable
  Dim i As Integer
  Dim l As Integer
  Dim k As Integer
  Dim j As Integer
  Dim kw As Integer
  Dim MnIopenArt As Boolean
  Dim MnUserUfo As Boolean
  Dim WellStart As Integer
  Dim WellEnde As Integer
  Dim WellStep As Integer
  Dim MessgNormFileID As Integer
  Dim MessgNwe As Integer
  Dim MessgNwp As Integer
  'REM Zwischengroessen
  Dim nkw As Integer
  'Message Kennung
  Dim imsg As Integer            'Messagebox (Rückgabewert)
  Dim MaxID As Long             'Maximale ID (Primärschlüssel)
  Dim ier As Integer
  Dim SqlStmt As String
  Dim BAUD() As Integer = {110, 150, 300, 600, 1200, 2400, 4800, 9600, 19200, 38400}
  Dim LENGTH() As Integer = {8, 7, 6, 5}
  Dim STOPBit() As Integer = {0, 1, 2}
  Dim HandShake() As Integer = {0, 1, 2}
  Dim Parity() As String = {"N", "O", "E"}
  Dim DRIVER() As String = {"SPE", "OPO", "OPQ", "OPR", "GK1", "X45", "XKU", "MC0", "MC1", _
                            "MC2", "HU1", "HU2", "MI0", "U30", "D38", "BYK", "GR0", "GR1", "GR2", _
                            "OPT", "SPA", "SPF", "MIL", "BYC", "MC3", "XKV", "XMU", "XMV", "XMO", _
                            "XCF", "XKW", "MI1", "MI2", "MI3", "MI4", "MC4", "MI5", "PE1", "X53", _
                            "ZE0", "LIC", "HU3", "OPI", "CAR", "X62", "MC5", "MI6", "MC6", "ALG", _
                            "MC7", "MX0", "TEC", "BYG", "MI7", "MI8", "MI9"}


  Dim REFTRA() As String = {"R", "T", "A"}
  Dim txtMEI As List(Of TextBox)
  Dim cboSTD As List(Of ComboBox)
  '
  Dim dsMEI As DataSet
  Dim RelMEI As DataRelation
  '
  Dim TblMEI As DataTable
  Dim TblNOR As DataTable
  Dim TblWIN As DataTable
  Dim TblWIA As DataTable
  '
  Dim CmdMEI As OleDbCommand
  Dim CmdNOR As OleDbCommand
  Dim CmdWIN As OleDbCommand
  Dim CmdWIA As OleDbCommand
  '
  '
  '
  '
  '
  Dim AdaptMEI As OleDbDataAdapter
  Dim AdaptNOR As OleDbDataAdapter
  Dim AdaptWIN As OleDbDataAdapter
  Dim AdaptWIA As OleDbDataAdapter
  '
  '
  '
  '
  Dim ViewNOR As DataView
  Dim ViewWIA As DataView
  '
  '
  Dim WithEvents ConnMEI As BindingSource


  Private Sub frmMEI_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim j As Integer
    Me.Location = New Point(0.5 * (My.Computer.Screen.WorkingArea.Size.Width - Me.Size.Width), 0.5 * (My.Computer.Screen.WorkingArea.Size.Height - Me.Size.Height))
    '
    Me.Text = Texxt(280)
    lblSTD_0.Text = Texxt(205)
    lblSTD_1.Text = Texxt(206)
    lblSTD_2.Text = Texxt(207)
    lblSTD_3.Text = Texxt(208)
    lblSTD_4.Text = Texxt(209)
    lblSTD_5.Text = Texxt(210)
    '
    '
    '
    lblMEI_00.Text = Texxt(281)
    lblMEI_01.Text = Texxt(282)
    lblMEI_02.Text = Texxt(283)
    lblMEI_03.Text = Texxt(298)
    lblMEI_04.Text = Texxt(285)
    lblMEI_05.Text = Texxt(286)
    lblMEI_06.Text = Texxt(287)
    lblMEI_07.Text = Texxt(288)
    lblMEI_08.Text = Texxt(289)
    lblMEI_09.Text = Texxt(290)
    lblMEI_10.Text = Texxt(299)
    lblMEI_11.Text = Texxt(399)
    lblMEI_12.Text = Texxt(212)
    lblMEI_13.Text = Texxt(214)
    lblMEI_14.Text = Texxt(294)
    lblMEI_15.Text = Texxt(215)
    lblMEI_16.Text = Texxt(284)
    '
    lblWIN.Text = Texxt(296)
    lblWIA.Text = Texxt(297)
    btnORD.Text = Texxt(1999)
    '
    '
    dsMEI = New DataSet
    '
    '

    TblMEI = New DataTable
    TblNOR = New DataTable
    TblWIN = New DataTable
    TblWIA = New DataTable
    '
    TblMEI.TableName = "TBLMEI"
    TblWIA.TableName = "TBLWIA"
    '
    CmdMEI = New OleDbCommand("", Cncol)
    CmdNOR = New OleDbCommand("", Cncol)
    CmdWIN = New OleDbCommand("", Cncol)
    CmdWIA = New OleDbCommand("", Cncol)
    '
    '
    '
    AdaptMEI = New OleDbDataAdapter
    AdaptNOR = New OleDbDataAdapter
    AdaptWIN = New OleDbDataAdapter
    AdaptWIA = New OleDbDataAdapter
    '
    '
    '
    ViewNOR = New DataView(TblNOR)
    ViewWIA = New DataView(TblWIA)
    '
    '
    ConnMEI = New BindingSource
    '
    AdaptMEI.SelectCommand = CmdMEI
    AdaptNOR.SelectCommand = CmdNOR
    AdaptWIN.SelectCommand = CmdWIN
    AdaptWIA.SelectCommand = CmdWIA
    '
    '
    '
    '
    txtMEI = New List(Of TextBox)
    txtMEI.Add(txtMEI_00)
    txtMEI.Add(txtMEI_01)
    txtMEI.Add(txtMEI_02)
    txtMEI.Add(txtMEI_03)
    txtMEI.Add(txtMEI_04)
    txtMEI.Add(txtMEI_05)
    txtMEI.Add(txtMEI_06)
    txtMEI.Add(txtMEI_07)
    txtMEI.Add(txtMEI_08)
    txtMEI.Add(txtMEI_09)

    cboSTD = New List(Of ComboBox)
    cboSTD.Add(cboSTD_0)
    cboSTD.Add(cboSTD_1)
    cboSTD.Add(cboSTD_2)
    cboSTD.Add(cboSTD_3)
    cboSTD.Add(cboSTD_4)
    cboSTD.Add(cboSTD_5)
    '
    '

    '
    '
    '
   
    WellStart = 0
    WellEnde = 0
    WellStep = 0
    '
    '
    'COMM-Port
    '
    For i = 0 To 16
      cboCOM.Items.Add(i)
    Next
    '
    'Remission,Transmission oder Beides
    '
    For i = 0 To REFTRA.Count - 1
      cboREFTRA.Items.Add(REFTRA(i))
    Next '
    'BAUD-Rate
    '
    For i = 0 To BAUD.Count - 1
      cboBAUD.Items.Add(BAUD(i))
    Next
    '
    '
    'LENGTH-Rate (Datanlänge pro Zeichen)
    '
    For i = 0 To LENGTH.Count - 1
      cboLength.Items.Add(LENGTH(i))
    Next
    '
    '
    'STOPBit-Rate (Anzahl STOPBits))
    '
    For i = 0 To STOPBit.Count - 1
      cboSTOP.Items.Add(STOPBit(i))
    Next
    '
    '
    'Handshake  (0=kein;1 XON/XOFF ;2=RTS/CTS)
    '
    For i = 0 To HandShake.Count - 1
      cboHands.Items.Add(HandShake(i))
    Next
    '
    '
    '
    'Parity (N,O,E)
    '
    For i = 0 To Parity.Count - 1
      cboPAR.Items.Add(Parity(i))
    Next
    '
    '
    '
    'DRIVER
    '
    For i = 0 To DRIVER.Count - 1
      cboDRIVER.Items.Add(DRIVER(i))
    Next
    '
    'DriverDLL (COM-Programme
    '
    For i = 0 To 99
      cboDRIVER.Items.Add(Format(i, "000"))
    Next


    '
    '
    '
    'Innere Reflexion=Glanz
    '
    '
    cboGLIN.Items.Clear()
    cboGLIN.Items.Add(Texxt(4))
    cboGLIN.Items.Add(Texxt(3))
    '
    If MnIopenArt And MnUserUfo Then
      '
      'MessgRW_ID
      '
      txtMEI(9).Enabled = True
    End If
    '
    '
    'Texte für Weiss-,Schwarz-,Graustandard
    '
    '
    '
    For i = 0 To 5
      cboSTD(i).Items.Clear()
      For j = 0 To 1
        cboSTD(i).Items.Add(Texxt(3 + j))
      Next j
    Next i
     '
    'Namen und ID's für Wellenlängenbereiche 
    'TBL_BEREICH
    '
    '
    SqlStmt = "SELECT * FROM TBL_BEREICH"
    CmdNOR.CommandText = SqlStmt
    If Not FillDatset(AdaptNOR, TblNOR) Then
      Exit Sub
    End If
    TblNOR.AcceptChanges()

    '
    '
    '
    '
    'Messgeräte
    '
    'TBL_MESSG
    '
    '
    SqlStmt = "SELECT * FROM TBL_MESSG ORDER BY MESSG_KBEZ"
    CmdMEI.CommandText = SqlStmt
    If Not FillDatset(AdaptMEI, TblMEI) Then
      Exit Sub
    End If
    TblMEI.AcceptChanges()
    TblMEI.Columns("MESSG_ID").Unique = True
    'datMEI.Recordset = DBas.OpenRecordset("TBL_MESSG", dbOpenDynaset, dbConsistent, dbReadOnly) 
    '
    '
    '
    '
    '
    '
    '
    'Winkel/Messgeometrien
    '
    '
    SqlStmt = "SELECT * FROM TBL_IHRM"
    CmdWIN.CommandText = SqlStmt
    If Not FillDatset(AdaptWIN, TblWIN) Then
      Exit Sub
    End If
    TblWIN.AcceptChanges()
    '
    'datWIN.Recordset = DBas.OpenRecordset("TBL_IHRM", dbOpenDynaset, dbConsistent, dbReadOnly)
    '
    '
    'Winkel/Messgeometrien
    '
    '
    SqlStmt = "SELECT MESSG_ID,TBL_MESSG_IHRM.IHRM_ID,POS_ID,IHRM_BEZ FROM TBL_MESSG_IHRM,TBL_IHRM WHERE TBL_MESSG_IHRM.IHRM_ID=TBL_IHRM.IHRM_ID" _
    & " ORDER BY POS_ID"
    CmdWIA.CommandText = SqlStmt
    If Not FillDatset(AdaptWIA, TblWIA) Then
      Exit Sub
    End If
    TblWIA.AcceptChanges()
    '
    '
    '
    '
    '
    '
    dsMEI.Tables.Add(TblMEI)
    dsMEI.Tables.Add(TblWIA)
    '
    RelMEI = New DataRelation("RELMEI", TblMEI.Columns("MESSG_ID"), TblWIA.Columns("MESSG_ID"))
    dsMEI.Relations.Add(RelMEI)
    dsMEI.EnforceConstraints = True
    '
    '
    '
    '
    '
    cboNOR.DataSource = ViewNOR
    cboNOR.DisplayMember = "BEREICH_NAME"
    cboNOR.ValueMember = "BEREICH_ID"

    lstWIN.DataSource = TblWIN
    lstWIN.DisplayMember = "IHRM_BEZ"
    lstWIN.ValueMember = "IHRM_ID"

    lstWIA.DataSource = ViewWIA
    lstWIA.DisplayMember = "IHRM_BEZ"
    lstWIA.ValueMember = "IHRM_ID"
    '
    '
    ConnMEI.DataSource = TblMEI
    cboMEI.DataSource = ConnMEI
    cboMEI.DisplayMember = "MESSG_KBEZ"
    cboMEI.ValueMember = "MESSG_ID"
    '
    '
    BindingMEI.BindingSource = ConnMEI
    '
    '
    txtMEI_00.DataBindings.Add("TEXT", ConnMEI, "MESSG_ID")
    txtMEI_01.DataBindings.Add("TEXT", ConnMEI, "MESSG_KENN")
    txtMEI_01.MaxLength = TblMEI.Columns("MESSG_KENN").MaxLength
    txtMEI_02.DataBindings.Add("TEXT", ConnMEI, "MESSG_KBEZ")
    txtMEI_02.MaxLength = TblMEI.Columns("MESSG_KBEZ").MaxLength
    txtMEI_03.DataBindings.Add("TEXT", ConnMEI, "MESSG_LBEZ")
    txtMEI_03.MaxLength = TblMEI.Columns("MESSG_LBEZ").MaxLength
    txtMEI_04.DataBindings.Add("TEXT", ConnMEI, "MESSG_WANF")
    txtMEI_05.DataBindings.Add("TEXT", ConnMEI, "MESSG_WEND")
    txtMEI_06.DataBindings.Add("TEXT", ConnMEI, "MESSG_PROG")
    txtMEI_06.MaxLength = TblMEI.Columns("MESSG_PROG").MaxLength
    txtMEI_07.DataBindings.Add("TEXT", ConnMEI, "MESSG_KALINT")
    txtMEI_08.DataBindings.Add("TEXT", ConnMEI, "MESSG_TBL")
    txtMEI_08.MaxLength = TblMEI.Columns("MESSG_TBL").MaxLength
    txtMEI_09.DataBindings.Add("TEXT", ConnMEI, "MESSGRW_ID")
    cboNOR.DataBindings.Add("SELECTEDVALUE", ConnMEI, "MESSG_NORM_FILE_ID")
    cboCOM.DataBindings.Add("TEXT", ConnMEI, "MESSG_COM")
    cboBAUD.DataBindings.Add("TEXT", ConnMEI, "MESSG_BAUD")
    cboPAR.DataBindings.Add("TEXT", ConnMEI, "MESSG_PAR")
    cboLength.DataBindings.Add("TEXT", ConnMEI, "MESSG_LENGTH")
    cboSTOP.DataBindings.Add("TEXT", ConnMEI, "MESSG_STOP")
    cboHands.DataBindings.Add("TEXT", ConnMEI, "MESSG_HANDS")
    cboDRIVER.DataBindings.Add("TEXT", ConnMEI, "MESSG_DRIVER")
    cboREFTRA.DataBindings.Add("TEXT", ConnMEI, "MESSG_REFTRA")
    cboSTD_0.DataBindings.Add("SELECTEDINDEX", ConnMEI, "MESSG_MES")
    cboSTD_1.DataBindings.Add("SELECTEDINDEX", ConnMEI, "MESSG_KAL")
    cboSTD_2.DataBindings.Add("SELECTEDINDEX", ConnMEI, "MESSG_NKAL")
    cboSTD_3.DataBindings.Add("SELECTEDINDEX", ConnMEI, "MESSG_INI")
    cboSTD_4.DataBindings.Add("SELECTEDINDEX", ConnMEI, "MESSG_SOND")
    cboSTD_5.DataBindings.Add("SELECTEDINDEX", ConnMEI, "MESSG_KEIN")
  End Sub
  '
  '
  '
  '
  Private Sub ConnMEI_AddingNew(ByVal sender As Object, ByVal e As System.ComponentModel.AddingNewEventArgs) Handles ConnMEI.AddingNew
    Dim i As Integer
    Dim Leng As Integer
    For i = 0 To TblMEI.Columns.Count - 1
      If Not TblMEI.Columns(i).AutoIncrement Then
        TblMEI.Columns(i).DefaultValue = ConnMEI.Current(i)
      End If
    Next i
    Leng = Min(TblMEI.Columns("MESSG_KBEZ").MaxLength - 4, TblMEI.Columns("MESSG_KBEZ").DefaultValue.length)
    TblMEI.Columns("MESSG_KBEZ").DefaultValue = TblMEI.Columns("MESSG_KBEZ").DefaultValue.substring(0, Leng) & " ????"
    If TblMEI.Columns("MESSG_KBEZ").DefaultValue.length > TblMEI.Columns("MESSG_KBEZ").MaxLength Then
      TblMEI.Columns("MESSG_KBEZ").DefaultValue = TblMEI.Columns("MESSG_KBEZ").DefaultValue.substring(0, TblMEI.Columns("MESSG_KBEZ").MaxLength)
    End If
  End Sub


  


  Private Sub ConnMEI_CurrentChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ConnMEI.CurrentChanged
    If ConnMEI Is Nothing OrElse ConnMEI.Count = 0 Then Exit Sub
    If IsNothing(ViewWIA) Then Exit Sub
    'Case 0
    
    BindingMEI.Enabled = False
    If ConnMEI.Current.row.rowstate = DataRowState.Detached Then
      ConnMEI.CurrencyManager.EndCurrentEdit()
      ConnMEI.Current("MESSGRW_ID") = ConnMEI.Current("MESSG_ID")
      ConnMEI.Current("MESSG_SET") = False
      '
      Call AddNewViewItem(ConnMEI.Current("MESSG_ID"), "MESSG_ID", ViewWIA)
    End If
    ViewWIA.RowFilter = "MESSG_ID=" & ConnMEI.Current("MESSG_ID")

    If ConnMEI.Current("MESSG_GLIN") Then
      cboGLIN.SelectedIndex = 0
    Else
      cboGLIN.SelectedIndex = 1

    End If

    If ConnMEI.Current("MESSG_SET") And Not MnUserUfo Then
      cboNOR.Enabled = False
      txtMEI(1).Enabled = False
      txtMEI(8).Enabled = False
      txtMEI(9).Enabled = False
      txtMEI(4).Enabled = False
      txtMEI(5).Enabled = False
      cboDRIVER.Enabled = False
      cboREFTRA.Enabled = False
      lstWIN.Enabled = False
      lstWIA.Enabled = False
      lstWIN.SelectedIndex = -1
      lstWIA.SelectedIndex = -1
    Else
      cboNOR.Enabled = True
      txtMEI(1).Enabled = True
      txtMEI(8).Enabled = True
      txtMEI(9).Enabled = True
      txtMEI(4).Enabled = True
      txtMEI(5).Enabled = True
      cboDRIVER.Enabled = True
      cboREFTRA.Enabled = True
      lstWIN.Enabled = True
      lstWIA.Enabled = True
    End If

    BindingMEI.Enabled = True
    If ConnMEI.Current.row.rowstate = DataRowState.Detached Then
      ConnMEI.EndEdit()
    End If
  End Sub

   

 


  Private Sub btnORD_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnORD.Click
    Dim i As Integer
    Dim j As Integer
    Dim Tabname() As String = _
  {"TBL_MESSG_GROUP", "TBL_MISCH_MESSG", "TBL_MISCH_GROUP_MESSG", _
   "TBL_USER_MESSG", "TBL_USER_METH_MESSG", "TBL_USER_METH_MESSG_IHRM", _
   "TBL_USER_MESSG_GROUP_DONTSHOW", "TBL_USER_MESSG_GROUP_READONLY", "TBL_USER_MISCH_MESSG"}

    Dim MessgKbez As String
    Dim MessgID As Integer
    Dim MessgRwID As Integer
    Dim MessAddSource() As Integer
    Dim MessAddTarget() As Integer
    Dim MessSynch() As Boolean

    Dim SqlEtmt As String
    Dim cmdHILF As New OleDbCommand
    Dim ViewHilf As New DataView(TblMEI)
    Dim ViewSlave As New DataView(TblMEI)
    Dim ViewMaster As New DataView(TblMEI)
    Dim WhereKeyID() As String
    '
    '
    Dim RowView As DataRowView
    Dim RowSlave As DataRowView
    Dim ViewMasterWIA As DataView
    Dim ViewSlaveWIA As DataView
     

    ' 
    '
    '
    If ConnMEI.Count = 0 Then
      MsgBox(Texxt(3761))
      Me.Close()
      Exit Sub
    End If
    If AddDelP(2990) Then
      '
      '
      ConnMEI.CurrencyManager.EndCurrentEdit()
      ViewWIA.EndInit()
      '
      ' 
      '
     
      ViewHilf.RowStateFilter = DataViewRowState.Deleted And DataViewRowState.OriginalRows
      If ViewHilf.Count > 0 Then
        MessgKbez = Chr(13)
        For i = 0 To ViewHilf.Count - 1
          MessgKbez = MessgKbez & ViewHilf(i)("MESSG_KBEZ") & Chr(13)
        Next
        If MessageBox.Show(MessgKbez & Texxt(2660), Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.No Then
          ViewHilf.Dispose()
          Me.Close()
          Me.Dispose()
          Exit Sub
        End If
      End If
      '
      '
      'Einträge für Löschen
      '
      '
      '
      For Each RowView In ViewHilf
        MessgID = RowView("MESSG_ID")
        MessgRwID = RowView("MESSGRW_ID")
        If MessgRwID = MessgID Then
          '
          '
          '
          'Slavegeräte werden gelöscht
          '
          '
          ViewSlave.RowFilter = "MESSGRW_ID=" & MessgID

          For Each RowSlave In ViewSlave
            RowSlave.Delete()
          Next

          '
          '
          '
          'Daten werden gelöscht, falls Mastergerät  MessgRwID = MessgID
          '
          '
          SqlStmt = "DELETE * FROM TBL_REZEPT_RWERT WHERE MESSGRW_ID=" & MessgRwID 
          cmdHILF.CommandText = SqlStmt
          If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
            Exit Sub
          End If

          '
          'TBL_SORTI_RWERT
          '
          SqlStmt = "DELETE * FROM TBL_SORTI_RWERT WHERE MESSGRW_ID=" & MessgRwID 
          cmdHILF.CommandText = SqlStmt
          If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
            Exit Sub
          End If
          '
          '
          '
          'TBL_QUALI
          '
          SqlStmt = "DELETE * FROM TBL_QUALI WHERE MESSGRW_ID=" & MessgRwID 
          cmdHILF.CommandText = SqlStmt
          If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
            Exit Sub
          End If
          '
          '
          '
          'TBL_RWERT
          '
          SqlStmt = "DELETE * FROM TBL_RWERT WHERE MESSGRW_ID=" & MessgRwID
          cmdHILF.CommandText = SqlStmt
          If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
            Exit Sub
          End If
          '
          '
          '
          '
          'TBL_GRUND_FARBM
          '
          SqlStmt = "DELETE * FROM TBL_GRUND_FARBM WHERE MESSGRW_ID=" & MessgRwID
          cmdHILF.CommandText = SqlStmt
          If SQLExeNonQuery(cmdHILF, Cndat) <> 0 Then
            Exit Sub
          End If
          '
          '

        End If
      Next
      For i = 0 To ViewHilf.Count - 1
        '
        '"DELETE * FROM " & Tabname(j)
        '
        '
        MessgID = ViewHilf(i)("MESSG_ID")
        For j = Tabname.Count - 1 To 0 Step -1
          cmdHILF.CommandText = "DELETE * FROM " & Tabname(j) & " WHERE MESSG_ID=" & MessgID
          If SQLExeNonQuery(cmdHILF, Cncol) <> 0 Then
            Exit Sub
          End If
        Next
      Next i

      '
      ViewHilf.RowStateFilter = DataViewRowState.Added
      ReDim MessAddSource(ViewHilf.Count - 1)
      ReDim MessAddTarget(ViewHilf.Count - 1)
      ReDim MessSynch(ViewHilf.Count - 1)
      For i = 0 To ViewHilf.Count - 1
        MessAddTarget(i) = ViewHilf(i)("MESSG_ID")
        MessAddSource(i) = ViewHilf(i)("MESSGRW_ID")
      Next
      '
      '
      'TBL_MESSG_IHRM
      '
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptWIA.InsertCommand = OleDBInsertCmd("TBL_MESSG_IHRM", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(1)
      WhereKeyID(0) = "MESSG_ID"
      WhereKeyID(1) = "IHRM_ID"
      AdaptWIA.UpdateCommand = OleDBUpdateCmd("TBL_MESSG_IHRM", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      AdaptWIA.DeleteCommand = OleDBDeleteCmd("TBL_MESSG_IHRM", WhereKeyID, Cncol)
      '
      '
      '
      '
      'TBL_MESSG
      '
      '
      '
      '
      '
      'Insertcommand
      '
      '
      '
      AdaptMEI.InsertCommand = OleDBInsertCmd("TBL_MESSG", Cncol)
      '
      '
      'Updatecommand
      '
      '
      '
      ReDim WhereKeyID(0)
      WhereKeyID(0) = "MESSG_ID"
      AdaptMEI.UpdateCommand = OleDBUpdateCmd("TBL_MESSG", WhereKeyID, Cncol)
      '
      '
      'Deletecommand
      '
      '
      '
      AdaptMEI.DeleteCommand = OleDBDeleteCmd("TBL_MESSG", WhereKeyID, Cncol)
      '
      '
      '
      '
      'Delete/Update/Insert TBL_MESSG_IHRM und TBL_MESSG
      '
      'Delete TBL_MESSG_IHRM  
      '
      '      
      ' 
      '
      AdaptWIA.Update(TblWIA.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '
      '
      '
      '
      'Delete TBL_MESSG
      '
      AdaptMEI.Update(TblMEI.Select(Nothing, Nothing, DataViewRowState.Deleted))
      '

      '
      'Update TBL_MESSG
      '
      '
      AdaptMEI.Update(TblMEI.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      'Update TBL_MESSG_IHRM
      '
      '
      AdaptWIA.Update(TblWIA.Select(Nothing, Nothing, DataViewRowState.ModifiedCurrent))
      '
      '
      '
      '
      '
      ' 
      '
      '
      ViewMaster = New DataView(TblMEI, Nothing, Nothing, DataViewRowState.CurrentRows)
      ViewSlave = New DataView(TblMEI, Nothing, Nothing, DataViewRowState.CurrentRows)
      ViewMasterWIA = New DataView(TblWIA, Nothing, "POS_ID", DataViewRowState.CurrentRows)
      ViewSlaveWIA = New DataView(TblWIA, Nothing, "POS_ID", DataViewRowState.CurrentRows)
      '
      '
      For i = 0 To MessAddSource.Count - 1
        MessSynch(i) = False
        MessgRwID = MessAddSource(i)
        MessgID = MessAddTarget(i)
        If MessgID <> MessgRwID Then
          '
          '
          ViewMaster.RowFilter = "MESSG_ID=" & MessgRwID & " AND MESSGRW_ID=" & MessgRwID
          If ViewMaster.Count = 0 Then
            MessageBox.Show(Texxt(204) & ": " & MessgRwID & Space(2) & Texxt(2984), Texxt(2004), MessageBoxButtons.OK)
            Exit Sub
          End If
          '
          '
          'PRÜFEN, ob Satz  MESSGRWID existiert
          '
          '
          'PRÜFEN, ob Satz mit MESSGID=MESSGRWID existiert
          '
          '
          ViewMaster.RowFilter = "MESSGRW_ID=" & MessgRwID & " AND MESSG_ID=" & MessgRwID
          If ViewMaster.Count <> 0 Then
            '
            '
            '
            'Prüfen, ob Anzahl Winkel/Messgeometrien (TBL_MESSG_IHRM) gleich groß und Reihenfolge bzw. Art übereinstimmt
            '
            '
            '
            '
            '
            ViewSlave.RowFilter = "MESSG_ID=" & MessgID
            '
            'Prüfen, ob Welenlängen kompatibel sind
            '
            '
            '
            If ViewSlave(0)("MESSG_NORM_FILE_ID") <> ViewMaster(0)("MESSG_NORM_FILE_ID") _
            Or ViewSlave(0)("MESSG_WANF") <> ViewMaster(0)("MESSG_WANF") _
            Or ViewSlave(0)("MESSG_WANF") <> ViewMaster(0)("MESSG_WANF") _
            Or ViewSlave(0)("MESSG_REFTRA") <> ViewMaster(0)("MESSG_REFTRA") Then
              MessageBox.Show(Texxt(4500) & vbCrLf & "MESSG_NORM_FILE_ID: " & ViewSlave(0)("MESSG_NORM_FILE_ID") & "," & ViewMaster(0)("MESSG_NORM_FILE_ID") & vbCrLf _
                    & "MESSG_WANF: " & ViewSlave(0)("MESSG_WANF") & "," & ViewMaster(0)("MESSG_WANF") & vbCrLf _
                    & "MESSG_WEND: " & ViewSlave(0)("MESSG_WEND") & "," & ViewMaster(0)("MESSG_WEND") & vbCrLf _
                    & "MESSG_REFTRA: " & ViewSlave(0)("MESSG_REFTRA") & "," & ViewMaster(0)("MESSG_REFTRA") & vbCrLf _
                     , Texxt(2004), MessageBoxButtons.OK)
              Exit Sub
            End If

            '
            '
            '
            ViewMasterWIA.RowFilter = "MESSG_ID=" & MessgRwID
            '
            '
            '
            ViewSlaveWIA.RowFilter = "MESSG_ID=" & MessgID
            '
            '
            If ViewMasterWIA.Count <> ViewSlaveWIA.Count Then
              MessageBox.Show(Texxt(3024) & ": " & ViewSlaveWIA.Count & "," & ViewMasterWIA.Count, Texxt(2004), MessageBoxButtons.OK)
              Exit Sub
            End If
            For j = 0 To ViewMasterWIA.Count - 1
              If ViewSlaveWIA(j)("IHRM_ID") <> ViewMasterWIA(j)("IHRM_ID") Then
                MessageBox.Show(Texxt(3509) & ": " & ViewSlaveWIA(j)("IHRM_ID") & "," & ViewMasterWIA(j)("IHRM_ID"), Texxt(2004), MessageBoxButtons.OK)
                Exit Sub
              End If
            Next j
            MessSynch(i) = True
          End If
        End If
      Next i
      '
      'Insert TBL_MESSG
      '
      '
      For i = 0 To TblMEI.Select(Nothing, Nothing, DataViewRowState.Added).Count - 1
        TblMEI.Select(Nothing, Nothing, DataViewRowState.Added)(i)("MESSG_SET") = True
      Next i
      AdaptMEI.Update(TblMEI.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      '
      'Insert TBL_MESSG_IHRM
      '
      '
      AdaptWIA.Update(TblWIA.Select(Nothing, Nothing, DataViewRowState.Added))
      '
      For i = 0 To MessAddSource.Count - 1
        MessgID = MessAddTarget(i)
        MessgRwID = MessAddSource(i)
        '
        If MessSynch(i) Then
          '
          '
          Call SynchronizeMessgID(MessgRwID, MessgID, Tabname)
        Else
          'Insert TBL_MESSG_GROUP 
          '
          '
          For j = 0 To 3
            SqlEtmt = "INSERT INTO TBL_MESSG_GROUP (MESSG_ID,GROUP_ID,GROUP_KBEZ,GROUP_NAME)" _
             & " VALUES(" & MessgID & "," & j & ",'" & Texxt(3015 + j) & "','" & Texxt(3015 + j) & "')"
            cmdHILF.CommandText = SqlEtmt
            If SQLExeNonQuery(cmdHILF, Cncol) <> 0 Then
              Exit Sub
            End If
          Next j
        End If
            '
            '
      Next i
    End If
    ViewHilf.Dispose()
    Me.Close()
    Me.Dispose()
  End Sub



  Sub SynchronizeMessgID(MessgRwID As Integer, MessgID As Integer, Tabname() As String)
    Dim i As Integer
    Dim Sqlstmt As String
    Dim DatSynch As OleDbDataReader
    Dim CommandSource As OleDbCommand
    Dim CommandTarget As OleDbCommand
    

    CommandSource = New OleDbCommand("", Cncol)
    CommandTarget = New OleDbCommand("", Cncol)
    '
  
    '
    For i = 0 To Tabname.Count - 1
      CommandSource.CommandText = "SELECT * FROM " & Tabname(i) & " WHERE MESSG_ID=" & MessgRwID
      DatSynch = DataRead(CommandSource, System.Data.CommandBehavior.CloseConnection, Cncol)
      '
      'INSERTCommand
      '
      '
      Sqlstmt = "INSERT INTO " & Tabname(i) & " ("
      For j = 0 To DatSynch.FieldCount - 1
        Sqlstmt = Sqlstmt & DatSynch.GetName(j)
        If j = DatSynch.FieldCount - 1 Then
          Sqlstmt = Sqlstmt & ") VALUES("
        Else
          Sqlstmt = Sqlstmt & ","
        End If
      Next
      For j = 0 To DatSynch.FieldCount - 1
        If j = DatSynch.FieldCount - 1 Then
          Sqlstmt = Sqlstmt & "?)"
        Else
          Sqlstmt = Sqlstmt & "?,"
        End If
      Next
      '
      '
      'CommandParameter
      '
      '
      CommandTarget.CommandText = Sqlstmt
      CommandTarget.Parameters.Clear()
      For j = 0 To DatSynch.FieldCount - 1
        CommandTarget.Parameters.Add(DatSynch.GetName(j), GetOleDbType(DatSynch.GetFieldType(j)))
      Next
      Do While DatSynch.Read
        For j = 0 To DatSynch.FieldCount - 1
          CommandTarget.Parameters(DatSynch.GetName(j)).Value = DatSynch.GetValue(j)
        Next
        CommandTarget.Parameters("MESSG_ID").Value = MessgID
        If SQLExeNonQuery(CommandTarget, Cncol) <> 0 Then
          Exit Sub
        End If
      Loop
      DatSynch.Close()
    Next

    '

  End Sub




  Public Sub New()

    ' Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()

    ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.

  End Sub




  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub
  Private Sub cboGLIN_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboGLIN.SelectedIndexChanged
    cboGLIN.Text = cboGLIN.Items(cboGLIN.SelectedIndex)
    If cboGLIN.SelectedIndex = 0 Then
      ConnMEI.Current("MESSG_GLIN") = True
    Else
      ConnMEI.Current("MESSG_GLIN") = False
    End If
  End Sub
  Private Sub cboNOR_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboNOR.SelectedIndexChanged
    Dim i As Integer
    Dim ier As Integer
    Dim Whilf() As Single
    If ConnMEI Is Nothing OrElse ConnMEI.Count = 0 Then Exit Sub
    If Not IsNumeric(cboNOR.SelectedValue) Then Exit Sub
    If IsNothing(cboNOR.SelectedItem) OrElse cboNOR.SelectedIndex < 0 Then Exit Sub
    '
    '
    '
    'Wellenlängen aus Datei TBL_LICHT_BEREICH_SPEK
    '
    MessgNormFileID = cboNOR.SelectedValue
    Call GETNwe(MessgNormFileID, MessgNwe, MessgNwp, WellStart, WellEnde, WellStep, ier)

    Call GETWsol(MessgNormFileID, WellStart, WellEnde, WellStep, Whilf, ier)
    lblNWE.Text = CStr(MessgNwe)
    lstNOR.Items.Clear()
    For i = 0 To MessgNwe - 1
      lstNOR.Items.Add(Whilf(i))
    Next i





  End Sub




  Private Sub lstWIN_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstWIN.Click
    Dim i As Integer
    Dim RowView As DataRowView
    If lstWIN.SelectedIndex < 0 Then Exit Sub
    'If IsEmpty(dblWIN.SelectedItem) Or LenB(dblWIN.SelectedItem) = 0 Then
    ' Exit Sub
    ' End If
    If ViewWIA.Count >= 9 Then
      MsgBox(Texxt(436))
      Exit Sub
    End If

    For i = 0 To ViewWIA.Count - 1
      If lstWIN.SelectedValue = ViewWIA(i)("IHRM_ID") Then
        Exit Sub
      End If
    Next
    '
    '
    '
    RowView = ViewWIA.AddNew()
    RowView("MESSG_ID") = ConnMEI.Current("MESSG_ID")
    RowView("IHRM_ID") = lstWIN.SelectedValue
    RowView("POS_ID") = ViewWIA.Count - 1
    RowView("IHRM_BEZ") = lstWIN.SelectedItem("IHRM_BEZ")
    RowView.EndEdit()

  End Sub


  Private Sub lstWIA_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstWIA.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call lstWIA_KeyDown(sender, ev)
  End Sub

  Private Sub lstWIA_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstWIA.KeyDown
    Dim i As Integer
    For i = lstWIA.SelectedIndex + 1 To lstWIA.Items.Count - 1
      ViewWIA(i)("POS_ID") = ViewWIA(i)("POS_ID") - 1
    Next
    ViewWIA.Delete(lstWIA.SelectedIndex)
  End Sub

  Private Sub txtMEI_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtMEI_04.Leave, txtMEI_05.Leave
    If CInt(txtMEI(4).Text) <> 0 Then
      If lstNOR.Items(0) < CInt(txtMEI(4).Text) Then
        MsgBox(Texxt(4167))
        txtMEI(4).Text = "0"
      End If
    End If
    If CInt(txtMEI(5).Text) <> 0 Then
      If lstNOR.Items(MessgNwe - 1) > CInt(txtMEI(5).Text) Then
        MsgBox(Texxt(4168))
        txtMEI(5).Text = "0"
      End If
    End If
  End Sub
  WriteOnly Property IopenArt() As Boolean
    Set(ByVal value As Boolean)
      MnIopenArt = value
    End Set
  End Property
  WriteOnly Property UserUfo() As Boolean
    Set(ByVal value As Boolean)
      MnUserUfo = value
    End Set
  End Property
End Class


