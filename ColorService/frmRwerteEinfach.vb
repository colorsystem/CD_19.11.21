Option Compare Text
Option Strict Off
Option Explicit On
Public Class frmRwerteEinfach
  Inherits System.Windows.Forms.Form
  Dim ReadWrite As ReadWriteRwert
  Dim MnRwert As RefValue
  Dim DataAdapter As OleDbDataAdapter
  Dim RwrtTable As DataTable
  Dim Rhilf() As Single
  Dim DE() As Single
  Dim MnMeasure As MeasureReflex
  Dim MnFormMDI As Form
  Private Sub btnMessen_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnMessen.Click
    Dim MaxID As Integer
    Dim ier As Integer
    Call MnMeasure.MessDevMess(MenueParam.Messg, MnRwert)
    If MnMeasure.ier <> 0 Then
      Exit Sub
    End If
    MnRwert.IVoNa = True
    MnRwert.Iplott = True
    MnRwert.Gid = MenueParam.Messg.RwrtGID
    MaxID = ReadWrite.RefMax(MenueParam.TableRwert, MenueParam.Messg.MessgRwID)
    MnRwert.Name = InputBox(Texxt(4631), Texxt(4632), Format((MaxID + 1), "0000000"))
    MnRwert.MessgID = MenueParam.MessgID
    If MnRwert.Name <> "" Then
      Call ReadWrite.WriteRwert(MaxID, MnRwert, ier)
      Me.DialogResult = Windows.Forms.DialogResult.OK
    Else
      Me.DialogResult = Windows.Forms.DialogResult.Abort
    End If

  End Sub


  Private Sub frmRwerteEinfach_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    btnAbbruch.Text = Texxt(379)
    btnMessen.Text = Texxt(205)
    btnRwerteLesen.Text = Texxt(4629)
    chkDatum.Text = Texxt(4628)
    lblBIS.Text = Texxt(377)
    lblSQL.Text = Texxt(369)
    Me.Top = 0.5 * (MnFormMDI.Top + MnFormMDI.Height - Me.Height)
    Me.Left = 0.5 * (MnFormMDI.Left + MnFormMDI.Width - 0.5 * Me.Width)
    txtBIS.Text = Today.ToShortDateString
    txtVon.Text = Date.Today.AddDays(-MenueParam.Messg.Tdiff).ToShortDateString
    btnMessen.Visible = MenueParam.Messg.Exists
    btnRwerteLesen.PerformClick()
  End Sub
  Private Sub Liste(ByRef Index As Short)
    Dim RefMaxID As Integer
    Dim RefMinID As Integer
    Dim Strgid As String
    Dim Ttop As Integer
    Dim SqlStmt As String = ""
    Dim Rmi As Integer

    dbgREF.Visible = False
    Cursor = Cursors.WaitCursor

    Try
      RefMaxID = ReadWrite.RefMax(MenueParam.TableRwert, MenueParam.Messg.MessgRwID)
      Ttop = MenueParam.Messg.Top
      If RefMaxID < MenueParam.Messg.Top Then
        Ttop = RefMaxID + 1
      End If
      If Ttop <= 0 Then
        Ttop = 10
      End If
      Rmi = 0
      If chkDatum.Checked Then
        RefMinID = ReadWrite.RefMin(MenueParam.TableRwert, Date.Parse(txtVon.Text), MenueParam.Messg.MessgRwID)
        Rmi = RefMinID
      End If
      Strgid = " RWERT_IARCH<2 "
      If MenueParam.Messg.RwrtGID <> 0 Then
        Strgid = Strgid & " AND RWERT_GID= " & MenueParam.Messg.RwrtGID
      End If
      Strgid = " RWERT_ID>=" & Rmi & " AND " & Strgid
      Strgid = Strgid & " AND RWERT_RETR=" & MenueParam.Messg.ReTr
      If Trim(txtSql.Text) = "" Then
        SqlStmt = "SELECT TOP " & Ttop & " * FROM " & MenueParam.TableRwert & " WHERE" & Strgid
      Else
        SqlStmt = "SELECT TOP " & Ttop & " * FROM " & MenueParam.TableRwert & " WHERE" & Strgid & " AND (" & StrSelct("RWERT_NAME", AddHkomE((txtSql.Text))) & ")"
      End If
      If chkDatum.Checked Then
        SqlStmt = SqlStmt & " AND RWERT_DATTIM BETWEEN ? AND ?"
      End If
      SqlStmt = SqlStmt & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID
      If Index = 1 Then
        SqlStmt = SqlStmt & " ORDER BY RWERT_DATTIM DESC "
      ElseIf Index = 2 Then
        SqlStmt = SqlStmt & " ORDER BY RWERT_NAME DESC"
      Else
        SqlStmt = SqlStmt & " ORDER BY RWERT_ID DESC"
      End If
      Cursor = Cursors.WaitCursor
      dbgREF.Text = Texxt(3914 + MenueParam.Messg.ReTr)
      DataAdapter.SelectCommand.CommandText = SqlStmt
      DataAdapter.SelectCommand.Parameters.Clear()
      If chkDatum.Checked Then
        DataAdapter.SelectCommand.Parameters.Add(New OleDbParameter("DATVON", OleDbType.Date))
        DataAdapter.SelectCommand.Parameters.Add(New OleDbParameter("DATBIS", OleDbType.Date))
        If IsDate(txtVon.Text) Then
          DataAdapter.SelectCommand.Parameters("DATVON").Value = Date.Parse(txtVon.Text)
        Else
          DataAdapter.SelectCommand.Parameters("DATVON").Value = Today
        End If
        If IsDate(txtBIS.Text) Then
          DataAdapter.SelectCommand.Parameters("DATBIS").Value = Date.Parse(txtBIS.Text).AddDays(1.0)
        Else
          DataAdapter.SelectCommand.Parameters("DATBIS").Value = Today
        End If
      End If
      RwrtTable.Rows.Clear()
      dbgREF.DataSource = RwrtTable
      DataAdapter.SelectCommand.Connection = Cndat()
      If Not FillDatset(DataAdapter, RwrtTable) Then
      End If
      Cursor = System.Windows.Forms.Cursors.Arrow
    Catch ex As Exception
      MsgBox(Texxt(4128) & SqlStmt)
      MsgBox(ex.Message)
    End Try
    If RwrtTable.Rows.Count = 0 Then
      MsgBox(Texxt(2959), 0)
      dbgREF.Visible = False
      Exit Sub
    Else
      dbgREF.Visible = True
    End If
    Cursor = Cursors.Default

  End Sub
  WriteOnly Property Rwert() As RefValue
    Set(ByVal value As RefValue)
      MnRwert = value
    End Set
  End Property
  WriteOnly Property Measure() As MeasureReflex
    Set(ByVal value As MeasureReflex)
      MnMeasure = value
    End Set
  End Property
  WriteOnly Property FormMDI() As Form
    Set(ByVal value As Form)
      Mnformmdi = value
    End Set
  End Property


  Public Sub New()

    ' This call is required by the Windows Form Designer.
    InitializeComponent()

    ' Add any initialization after the InitializeComponent() call.
    ReadWrite = New ReadWriteRwert
    DataAdapter = New OleDbDataAdapter
    DataAdapter.SelectCommand = New OleDbCommand
    RwrtTable = New DataTable
    RwrtTable.Clear()
    dbgREF.Update()
    dbgREF.TableStyles.Clear()
    dbgREF.TableStyles.Add(New DataGridTableStyle)
    dbgREF.CaptionVisible = False
    dbgREF.TableStyles(0).MappingName = RwrtTable.TableName
    dbgREF.TableStyles(0).AllowSorting = False
    dbgREF.TableStyles(0).RowHeadersVisible = True
    dbgREF.RowHeaderWidth = 10
    dbgREF.ReadOnly = True
    dbgREF.AllowNavigation = True
    '
    ' 
    dbgREF.TableStyles(0).GridColumnStyles.Clear()
    dbgREF.TableStyles(0).GridColumnStyles.Add(New DataGridTextBoxColumn)
    'flgKAL.TableStyles(0).GridColumnStyles.Add(New DataGridBoolColumn)
    dbgREF.TableStyles(0).GridColumnStyles(0).MappingName = "RWERT_ID"
    dbgREF.TableStyles(0).GridColumnStyles(0).HeaderText = "ID"
    dbgREF.TableStyles(0).GridColumnStyles(0).Alignment = HorizontalAlignment.Right
    dbgREF.TableStyles(0).GridColumnStyles(0).Width = 0
    dbgREF.TableStyles(0).GridColumnStyles(0).ReadOnly = True
    '
    ' 
    dbgREF.TableStyles(0).GridColumnStyles.Add(New DataGridTextBoxColumn)
    dbgREF.TableStyles(0).GridColumnStyles(1).MappingName = "RWERT_DATTIM"
    dbgREF.TableStyles(0).GridColumnStyles(1).HeaderText = Texxt(140)
    dbgREF.TableStyles(0).GridColumnStyles(1).Alignment = HorizontalAlignment.Right
    dbgREF.TableStyles(0).GridColumnStyles(1).Width = 0
    dbgREF.TableStyles(0).GridColumnStyles(1).ReadOnly = True
    '
    '
    dbgREF.TableStyles(0).GridColumnStyles.Add(New DataGridTextBoxColumn)
    dbgREF.TableStyles(0).GridColumnStyles(2).MappingName = "RWERT_NAME"
    dbgREF.TableStyles(0).GridColumnStyles(2).HeaderText = Texxt(394)
    dbgREF.TableStyles(0).GridColumnStyles(2).Alignment = HorizontalAlignment.Left
    dbgREF.TableStyles(0).GridColumnStyles(2).Width = dbgREF.Width
    dbgREF.TableStyles(0).GridColumnStyles(2).ReadOnly = True
    '
    '
    '
    '
  End Sub

  Private Sub btnRwerteLesen_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRwerteLesen.Click
    Call Liste(1)
  End Sub

  Private Sub dbgREF_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles dbgREF.Click, btnAbbruch.Click
    Dim i As Integer
    Dim kw As Integer
    Dim nku As Integer
    Dim RowNumber As Integer
    MnRwert.IVoNa = False
    MnRwert.Iplott = False
    If sender.name = btnAbbruch.Name Then
      Me.DialogResult = Windows.Forms.DialogResult.Abort
    Else
      RowNumber = dbgREF.CurrentCell.RowNumber
      MnRwert.ID = RwrtTable.Rows(RowNumber)("RWERT_ID")
      MnRwert.Name = RwrtTable.Rows(RowNumber)("RWERT_NAME")
      If IsDBNull(RwrtTable.Rows(RowNumber)("RWERT_BEM")) Then
        MnRwert.Bem = ""
      Else
        MnRwert.Bem = RwrtTable.Rows(RowNumber)("RWERT_BEM")
      End If
      If IsDBNull(RwrtTable.Rows(RowNumber)("RWERT_KENN")) Then
        MnRwert.Banum = ""
      Else
        MnRwert.Banum = RwrtTable.Rows(RowNumber)("RWERT_KENN")
      End If
      MnRwert.Cme = RwrtTable.Rows(RowNumber)("RWERT_CME")
      MnRwert.DatTim = RwrtTable.Rows(RowNumber)("RWERT_DATTIM")
      MnRwert.Iarch = RwrtTable.Rows(RowNumber)("RWERT_IARCH")
      MnRwert.ReTr = RwrtTable.Rows(RowNumber)("RWERT_RETR")
      MnRwert.Iami = RwrtTable.Rows(RowNumber)("RWERT_IAMI")
      MnRwert.Gid = RwrtTable.Rows(RowNumber)("RWERT_GID")
      DE = GetSingles(RwrtTable.Rows(RowNumber)("RWERT_DE"))
      Rhilf = GetSingles(RwrtTable.Rows(RowNumber)("RWERT_RWERT"))
      MnRwert.RefKurv.clear()
      For kw = 0 To MenueParam.User.Winkel.Km - 1
        nku = MenueParam.User.Winkel(kw).Lkm * MenueParam.User.Winkel.Wsol.Nwe
        MnRwert.De(kw) = DE(kw)
        MnRwert.RefKurv.Add(MenueParam.User.Winkel(kw).Chrm, New CurveRef(MenueParam.User.Winkel.Wsol.Nwe))
        For i = 0 To MenueParam.User.Winkel.Wsol.Nwe - 1
          MnRwert.RefKurv(kw).R(i) = Rhilf(nku + i)
        Next i
      Next kw
      MnRwert.IVoNa = True
      MnRwert.Iplott = True
      Me.DialogResult = Windows.Forms.DialogResult.OK
    End If
    Me.Close()
    '
    '
  End Sub


  Private Sub chkDatum_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkDatum.CheckedChanged
    If chkDatum.Checked Then
      txtBIS.Visible = True
      txtVon.Visible = True
      lblBIS.Visible = True
    Else
      txtBIS.Visible = False
      txtVon.Visible = False
      lblBIS.Visible = False

    End If
  End Sub
End Class