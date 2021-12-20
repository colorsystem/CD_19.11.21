Option Compare Text
Option Explicit On
Option Strict Off
Public Class frmColorDbDelete
  Dim TabColRezRwrt As List(Of DataTable)
  Dim TabMischGroup As DataTable
  Dim TabMisch As DataTable
  Dim TabMessgGroup As DataTable
  Dim TabMessg As DataTable
  Dim ViewMessg As DataView
  Dim TabGroupMessg As DataTable
  Dim TabGroupMisch As DataTable
  Dim ViewMischMessg As DataView
  Dim AdaptColRezRwrt As OleDbDataAdapter
  Dim CmdDelete As OleDbCommand

  Dim dbgColRezRwrt As List(Of DataGridView)
  Dim panColRezRwrt As List(Of Panel)
  Dim lblColRezRwrt As List(Of Label)
  Dim btnMark As List(Of Button)
  Dim btnZeig As List(Of Button)
  Dim radSelectGrid As List(Of RadioButton)
  Dim radFarbmittel As List(Of RadioButton)
  Dim lblAnzahl As List(Of Label)
  Dim txtAnzahl As List(Of TextBox)
  Private Sub frmColorDbDelete_Load(sender As Object, e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    Dim MessgID As Integer
    Dim MischID As Integer
    Dim Ende As Boolean
    txtBIS.Text = Format$(DateAdd("d", -1 * 365, Today))
    Cursor = Cursors.WaitCursor
    Call CreateCopyDatabase(Ende)
    Cursor = Cursors.Default

    If Ende Then
      Me.Visible = False
      Exit Sub
    End If
    Me.Text = Texxt(2019)
    AdaptColRezRwrt = New OleDbDataAdapter
    CmdDelete = New OleDbCommand("", Cndat)
    AdaptColRezRwrt.SelectCommand = New OleDbCommand("", Cncol)
    lblVON.Text = Texxt(376)
    lblBIS.Text = Texxt(377)
    lblPanFarbmittel.Text = Texxt(3970)
    radFarbmittel_0.Text = Texxt(3971)
    radFarbmittel_1.Text = Texxt(3972)
    radFarbmittel_2.Text = Texxt(3973)
    radSelectGrid_0.Text = Texxt(980)
    radSelectGrid_1.Text = Texxt(730)
    radSelectGrid_2.Text = Texxt(800)
    radSelectGrid_3.Text = Texxt(3110)
    btnZEIG_0.Text = Texxt(3690)
    btnZEIG_1.Text = Texxt(3690)
    btnZEIG_2.Text = Texxt(3690)
    btnZEIG_3.Text = Texxt(3690)
    btnMARK_0.Text = Texxt(3670)
    btnMARK_1.Text = Texxt(3670)
    btnMARK_2.Text = Texxt(3670)
    btnMARK_3.Text = Texxt(3670)
    btnDelete.Text = Texxt(381)
    lblHinweis.Text = Texxt(3566)
    '
    btnMark = New List(Of Button)
    btnMark.Add(btnMARK_0)
    btnMark.Add(btnMARK_1)
    btnMark.Add(btnMARK_2)
    btnMark.Add(btnMARK_3)
    '
    btnZeig = New List(Of Button)
    btnZeig.Add(btnZEIG_0)
    btnZeig.Add(btnZEIG_1)
    btnZeig.Add(btnZEIG_2)
    btnZeig.Add(btnZEIG_3)
    '
    radSelectGrid = New List(Of RadioButton)
    radSelectGrid.Add(radSelectGrid_0)
    radSelectGrid.Add(radSelectGrid_1)
    radSelectGrid.Add(radSelectGrid_2)
    radSelectGrid.Add(radSelectGrid_3)
    '
    '
    radFarbmittel = New List(Of RadioButton)
    radFarbmittel.Add(radFarbmittel_0)
    radFarbmittel.Add(radFarbmittel_1)
    radFarbmittel.Add(radFarbmittel_2)
    '
    '
    '

    dbgColRezRwrt = New List(Of DataGridView)
    dbgColRezRwrt.Add(dbgColRezRwrt_0)
    dbgColRezRwrt.Add(dbgColRezRwrt_1)
    dbgColRezRwrt.Add(dbgColRezRwrt_2)
    dbgColRezRwrt.Add(dbgColRezRwrt_3)
    panColRezRwrt = New List(Of Panel)
    panColRezRwrt.Add(panColRezRwrt_0)
    panColRezRwrt.Add(panColRezRwrt_1)
    panColRezRwrt.Add(panColRezRwrt_2)
    panColRezRwrt.Add(panColRezRwrt_3)
    '
    '
    lblColRezRwrt = New List(Of Label)
    lblColRezRwrt.Add(lblColRezRwrt_0)
    lblColRezRwrt.Add(lblColRezRwrt_1)
    lblColRezRwrt.Add(lblColRezRwrt_2)
    lblColRezRwrt.Add(lblColRezRwrt_3)
    For i = 0 To lblColRezRwrt.Count - 1
      lblColRezRwrt(i).Text = Texxt(2014 + i)
    Next
    '
    lblAnzahl = New List(Of Label)
    lblAnzahl.Add(lblAnzahl_0)
    lblAnzahl.Add(lblAnzahl_1)
    lblAnzahl.Add(lblAnzahl_2)
    lblAnzahl.Add(lblAnzahl_3)
    For i = 0 To lblAnzahl.Count - 1
      lblAnzahl(i).Text = Texxt(3689)
    Next
    txtAnzahl = New List(Of TextBox)
    txtAnzahl.Add(txtAnzahl_0)
    txtAnzahl.Add(txtAnzahl_1)
    txtAnzahl.Add(txtAnzahl_2)
    txtAnzahl.Add(txtAnzahl_3)
    '
    TabColRezRwrt = New List(Of DataTable)
    For i = 0 To 3
      TabColRezRwrt.Add(New DataTable)
    Next
    '
    '
    For i = 0 To dbgColRezRwrt.Count - 1
      dbgColRezRwrt(i).ReadOnly = True
      dbgColRezRwrt(i).AllowUserToAddRows = False
      dbgColRezRwrt(i).AllowUserToDeleteRows = False
      dbgColRezRwrt(i).AllowUserToOrderColumns = False
      dbgColRezRwrt(i).SelectionMode = DataGridViewSelectionMode.RowHeaderSelect
    Next
    '
    'Tabelle für Farbmittelart
    '
    cboArtFarbm.Items.Clear()
    For i = 0 To 8
      cboArtFarbm.Items.Add(Texxt(980 + i))
    Next i
    cboArtFarbm.SelectedIndex = 6


    'Tabelle für Messgeräte

    AdaptColRezRwrt.SelectCommand.CommandText = _
       "SELECT MESSGRW_ID,MESSG_ID,MESSG_KBEZ FROM TBL_MESSG"

    TabMessg = New DataTable
    If FillDatset(AdaptColRezRwrt, TabMessg) Then

    End If
    TabMessg.AcceptChanges()
    ViewMessg = New DataView(TabMessg)
    ViewMessg.RowFilter = "MESSGRW_ID=MESSG_ID"
    '
    '
    '

    'Tabelle für Messgeräte-Gruppen 
    '
    '
    AdaptColRezRwrt.SelectCommand.CommandText = _
      "SELECT TBL_MESSG_GROUP.MESSG_ID AS MESSG_ID,MESSG_KBEZ,GROUP_ID,GROUP_KBEZ FROM TBL_MESSG_GROUP" _
      & " INNER JOIN TBL_MESSG ON TBL_MESSG_GROUP.MESSG_ID=TBL_MESSG.MESSG_ID"

    TabMessgGroup = New DataTable
    If FillDatset(AdaptColRezRwrt, TabMessgGroup) Then

    End If
    TabMessgGroup.AcceptChanges()

    'Tabelle für Mischsysteme

    AdaptColRezRwrt.SelectCommand.CommandText = _
       "SELECT MISCH_ID,MISCH_KBEZ FROM TBL_MISCH"

    TabMisch = New DataTable
    If FillDatset(AdaptColRezRwrt, TabMisch) Then

    End If
    TabMisch.AcceptChanges()
    '
    '
    '

    'Tabelle für Mischsystem-Gruppen 
    '
    '
    AdaptColRezRwrt.SelectCommand.CommandText = _
      "SELECT TBL_MISCH_GROUP.MISCH_ID AS MISCH_ID,MISCH_KBEZ,GROUP_ID,GROUP_KBEZ FROM TBL_MISCH_GROUP" _
    & " INNER JOIN TBL_MISCH ON TBL_MISCH_GROUP.MISCH_ID=TBL_MISCH.MISCH_ID"

    TabMischGroup = New DataTable
    If FillDatset(AdaptColRezRwrt, TabMischGroup) Then

    End If
    TabMischGroup.AcceptChanges()
    '
    '
    '

    'Tabelle für Gruppen eines ausgewählten Messgerätes
    '
    '
    MessgID = CInt(GetPrivSettings("IDNUMBERS", "MESSGID", "-1", COLORFileName))
    '
    AdaptColRezRwrt.SelectCommand.CommandText = _
      "SELECT GROUP_ID,GROUP_KBEZ FROM TBL_MESSG_GROUP WHERE MESSG_ID=" & MessgID

    TabGroupMessg = New DataTable
    If FillDatset(AdaptColRezRwrt, TabGroupMessg) Then

    End If
    TabGroupMessg.AcceptChanges()
    cboMessgGroup.DataSource = TabGroupMessg
    cboMessgGroup.DisplayMember = "GROUP_KBEZ"
    cboMessgGroup.ValueMember = "GROUP_ID"
    If TabGroupMessg.Rows.Count = 0 Then
      MsgBox(Texxt(2031) & " MESSGID=-1 ?")
      Exit Sub
    Else
      cboMessgGroup.SelectedValue = TabGroupMessg.Rows(0)("GROUP_ID")
    End If


    '

    'Tabelle für Gruppen eines ausgewählten Mischsystems
    '
    '
    MischID = CInt(GetPrivSettings("IDNUMBERS", "MISCHID", "-1", COLORFileName))
    '

    AdaptColRezRwrt.SelectCommand.CommandText = _
      "SELECT GROUP_ID,GROUP_KBEZ FROM TBL_MISCH_GROUP WHERE MISCH_ID=" & MischID

    TabGroupMisch = New DataTable
    If FillDatset(AdaptColRezRwrt, TabGroupMisch) Then

    End If
    TabGroupMisch.AcceptChanges()
    cboMischGroup.DataSource = TabGroupMisch
    cboMischGroup.DisplayMember = "GROUP_KBEZ"
    cboMischGroup.ValueMember = "GROUP_ID"
    If TabGroupMisch.Rows.Count = 0 Then
      MsgBox(Texxt(2032) & " MISCHID=-1 ?")
      Exit Sub
    Else
      cboMischGroup.SelectedValue = TabGroupMisch.Rows(0)("GROUP_ID")
    End If


    '
    Call DbgPanVisible(0)
  End Sub

  Sub DbgPanVisible(index As Integer)
    Dim i As Integer
    For i = 0 To dbgColRezRwrt.Count - 1
      dbgColRezRwrt(i).Visible = False
    Next
    For i = 0 To panColRezRwrt.Count - 1
      panColRezRwrt(i).Visible = False
    Next
    dbgColRezRwrt(index).Visible = True
    panColRezRwrt(index).Visible = True
  End Sub

  Private Sub btnZEIG_Click(sender As Object, e As System.EventArgs) Handles btnZEIG_0.Click, btnZEIG_1.Click, btnZEIG_2.Click, btnZEIG_3.Click
    Dim index As Integer
    Dim i As Integer
    Dim k As Integer
    Dim StrRwrtRezept As String
    Dim StrRwrtSorti As String


    Cursor = Cursors.WaitCursor


    '
    index = CInt(sender.name.substring(8, 1))
    '
    Select Case index
      Case 0
        '
        '
        'Tabelle Farbmittel
        '
        '
        TabColRezRwrt(index) = New DataTable

        AdaptColRezRwrt.SelectCommand.Connection = Cndat
        AdaptColRezRwrt.SelectCommand.CommandText = _
          "SELECT FARBM_DATTIM,MISCH_ID,FARBM_ID,FARBM_NAME FROM TBL_FARBM" _
          & " WHERE FARBM_ICHF=" & cboArtFarbm.SelectedIndex & " AND (FARBM_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON.Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtBIS.Text))) _
          & ") ORDER BY MISCH_ID,FARBM_ID"
        If FillDatset(AdaptColRezRwrt, TabColRezRwrt(index)) Then

        End If
        TabColRezRwrt(index).Columns.Add("MISCH_KBEZ", GetType(String))
        ViewMischMessg = New DataView(TabMisch)
        For i = 0 To TabColRezRwrt(index).Rows.Count - 1
          ViewMischMessg.RowFilter = "MISCH_ID=" & TabColRezRwrt(index).Rows(i)("MISCH_ID")
          If ViewMischMessg.Count > 0 Then
            TabColRezRwrt(index).Rows(i)("MISCH_KBEZ") = ViewMischMessg(0)("MISCH_KBEZ")
          End If
        Next
        TabColRezRwrt(index).AcceptChanges()
        dbgColRezRwrt(index).DataSource = TabColRezRwrt(index)
        dbgColRezRwrt(index).Columns("MISCH_ID").Visible = False
        dbgColRezRwrt(index).Columns("FARBM_ID").Visible = False

        btnMark(index).Enabled = True
      Case 1
        '
        '
        'Tabelle Rezepte
        '
        '
        TabColRezRwrt(index) = New DataTable

        AdaptColRezRwrt.SelectCommand.Connection = Cndat
        If cboMischGroup.SelectedValue = 0 Then
          AdaptColRezRwrt.SelectCommand.CommandText = _
            "SELECT REZEPT_DATTIM,MISCH_ID,REZEPT_ID,REZEPT_NAME,REZEPT_GID FROM TBL_REZEPT" _
            & " WHERE (REZEPT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON.Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtBIS.Text))) _
            & ") ORDER BY MISCH_ID,REZEPT_ID"
        Else
          AdaptColRezRwrt.SelectCommand.CommandText = _
            "SELECT REZEPT_DATTIM,MISCH_ID,REZEPT_ID,REZEPT_NAME,REZEPT_GID FROM TBL_REZEPT" _
            & " WHERE REZEPT_GID=" & cboMischGroup.SelectedValue & " AND (REZEPT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON.Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtBIS.Text))) _
            & ") ORDER BY MISCH_ID,REZEPT_ID"
        End If
        If FillDatset(AdaptColRezRwrt, TabColRezRwrt(index)) Then

        End If
        TabColRezRwrt(index).Columns.Add("MISCH_KBEZ", GetType(String))
        TabColRezRwrt(index).Columns.Add("GROUP_KBEZ", GetType(String))
        ViewMischMessg = New DataView(TabMischGroup)
        For i = 0 To TabColRezRwrt(index).Rows.Count - 1
          ViewMischMessg.RowFilter = "MISCH_ID=" & TabColRezRwrt(index).Rows(i)("MISCH_ID") & " AND GROUP_ID=" & TabColRezRwrt(index).Rows(i)("REZEPT_GID")
          If ViewMischMessg.Count > 0 Then
            TabColRezRwrt(index).Rows(i)("MISCH_KBEZ") = ViewMischMessg(0)("MISCH_KBEZ")
            TabColRezRwrt(index).Rows(i)("GROUP_KBEZ") = ViewMischMessg(0)("GROUP_KBEZ")
          End If
        Next
        TabColRezRwrt(index).AcceptChanges()
        dbgColRezRwrt(index).DataSource = TabColRezRwrt(index)
        dbgColRezRwrt(index).Columns("MISCH_ID").Visible = False
        dbgColRezRwrt(index).Columns("REZEPT_ID").Visible = False
        dbgColRezRwrt(index).Columns("REZEPT_GID").Visible = False
        btnMark(index).Enabled = True
      Case 2
        '
        '
        'Tabelle Sortimente
        '
        '
        TabColRezRwrt(index) = New DataTable

        AdaptColRezRwrt.SelectCommand.Connection = Cndat
        AdaptColRezRwrt.SelectCommand.CommandText = _
            "SELECT SORTI_DATTIM,MISCH_ID,SORTI_ID,SORTI_NAME FROM TBL_SORTI" _
            & " WHERE (SORTI_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON.Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtBIS.Text))) _
            & ") ORDER BY MISCH_ID,SORTI_ID"

        If FillDatset(AdaptColRezRwrt, TabColRezRwrt(index)) Then

        End If
        TabColRezRwrt(index).Columns.Add("MISCH_KBEZ", GetType(String))
        ViewMischMessg = New DataView(TabMischGroup)
        For i = 0 To TabColRezRwrt(index).Rows.Count - 1
          ViewMischMessg.RowFilter = "MISCH_ID=" & TabColRezRwrt(index).Rows(i)("MISCH_ID") & " AND GROUP_ID=" & cboMischGroup.SelectedValue
          If ViewMischMessg.Count > 0 Then
            TabColRezRwrt(index).Rows(i)("MISCH_KBEZ") = ViewMischMessg(0)("MISCH_KBEZ")
          End If
        Next
        TabColRezRwrt(index).AcceptChanges()
        dbgColRezRwrt(index).DataSource = TabColRezRwrt(index)
        dbgColRezRwrt(index).Columns("MISCH_ID").Visible = False
        dbgColRezRwrt(index).Columns("SORTI_ID").Visible = False

        btnMark(index).Enabled = True
      Case 3
        '
        '
        'Tabelle Rwerte
        '
        '
        If ConnOpen(Cndat) Then
          TabColRezRwrt(index) = New DataTable

          If ViewMessg.Count > 0 Then
            AdaptColRezRwrt.SelectCommand.Connection = Cndat
            For k = 0 To ViewMessg.Count - 1
              'StrRwrtRezept = StringWhere("TBL_REZEPT_RWERT", "RWERT_ID", {"MESSGRW_ID"}, {ViewMessg(k)("MESSGRW_ID")}, Cndat)
              'StrRwrtSorti = StringWhere("TBL_SORTI_RWERT", "RWERT_ID", {"MESSGRW_ID"}, {ViewMessg(k)("MESSGRW_ID")}, Cndat)
              StrRwrtRezept = "(SELECT RWERT_ID FROM TBL_REZEPT_RWERT WHERE MESSGRW_ID=" & ViewMessg(k)("MESSGRW_ID") & ")"
              StrRwrtSorti = "(SELECT RWERT_ID FROM TBL_SORTI_RWERT WHERE MESSGRW_ID=" & ViewMessg(k)("MESSGRW_ID") & ")"
              'StrSorti = "(SELECT SORTI_ID FROM TBL_SORTI IN '" & ConnNew.DataSource & "' WHERE MISCH_ID=" & MischID & ")"

              If cboMessgGroup.SelectedValue = 0 Then
                AdaptColRezRwrt.SelectCommand.CommandText = _
                  "SELECT RWERT_DATTIM,MESSGRW_ID,MESSG_ID,RWERT_ID,RWERT_NAME,RWERT_GID FROM TBL_RWERT" _
                  & " WHERE (RWERT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON.Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtBIS.Text))) & ")" _
                  & " AND MESSGRW_ID=" & ViewMessg(k)("MESSGRW_ID") _
                  & " AND RWERT_ID NOT IN " & StrRwrtRezept & " AND RWERT_ID NOT IN " & StrRwrtSorti _
                  & " ORDER BY MESSG_ID,RWERT_ID"
              Else
                AdaptColRezRwrt.SelectCommand.CommandText = _
                  "SELECT RWERT_DATTIM,MESSGRW_ID,MESSG_ID,RWERT_ID,RWERT_NAME,RWERT_GID FROM TBL_RWERT" _
                  & " WHERE RWERT_GID=" & cboMessgGroup.SelectedValue & " AND (RWERT_DATTIM BETWEEN " & Sqldati(Date.Parse(txtVON.Text)) & " AND " & Sqldati(Date.Parse(DateAdd("d", 1, txtBIS.Text))) & ")" _
                  & " AND MESSGRW_ID=" & ViewMessg(k)("MESSGRW_ID") _
                  & " AND RWERT_ID NOT IN " & StrRwrtRezept & " AND RWERT_ID NOT IN " & StrRwrtSorti _
                  & " ORDER BY MESSG_ID,RWERT_ID"
              End If
              If FillDatset(AdaptColRezRwrt, TabColRezRwrt(index)) Then

              End If
            Next k
          End If
          Cndat.Close()
          TabColRezRwrt(index).Columns.Add("MESSG_KBEZ", GetType(String))
          TabColRezRwrt(index).Columns.Add("GROUP_KBEZ", GetType(String))
          ViewMischMessg = New DataView(TabMessgGroup)
          For i = 0 To TabColRezRwrt(index).Rows.Count - 1
            ViewMischMessg.RowFilter = "MESSG_ID=" & TabColRezRwrt(index).Rows(i)("MESSG_ID") & " AND GROUP_ID=" & TabColRezRwrt(index).Rows(i)("RWERT_GID")
            If ViewMischMessg.Count > 0 Then
              TabColRezRwrt(index).Rows(i)("MESSG_KBEZ") = ViewMischMessg(0)("MESSG_KBEZ")
              TabColRezRwrt(index).Rows(i)("GROUP_KBEZ") = ViewMischMessg(0)("GROUP_KBEZ")
            End If
          Next
          TabColRezRwrt(index).AcceptChanges()
          dbgColRezRwrt(index).DataSource = TabColRezRwrt(index)
          dbgColRezRwrt(index).Columns("MESSGRW_ID").Visible = False
          dbgColRezRwrt(index).Columns("MESSG_ID").Visible = False
          dbgColRezRwrt(index).Columns("RWERT_ID").Visible = False
          dbgColRezRwrt(index).Columns("RWERT_GID").Visible = False
          btnMark(index).Enabled = True
        End If
    End Select
    txtAnzahl(index).Text = TabColRezRwrt(index).Rows.Count
    Cursor = Cursors.Default

  End Sub

  Private Sub btnMARK_Click(sender As Object, e As System.EventArgs) Handles btnMARK_0.Click, btnMARK_1.Click, btnMARK_2.Click, btnMARK_3.Click
    Dim index As Integer
    index = CInt(sender.name.substring(8, 1))
    btnMark(index).Enabled = False
    dbgColRezRwrt(index).SelectAll()
    btnMark(index).Enabled = True
  End Sub

  Private Sub radSelectGrid_Click(sender As Object, e As System.EventArgs) Handles radSelectGrid_0.Click, radSelectGrid_1.Click, radSelectGrid_2.Click, radSelectGrid_3.Click
    Dim index As Integer
    If sender.checked Then
      index = CInt(sender.name.substring(14, 1))
      Call DbgPanVisible(index)
    End If
  End Sub

  Private Sub btnDelete_Click(sender As Object, e As System.EventArgs) Handles btnDelete.Click
    Dim MischID As Integer
    Dim MessgRwID As Integer
    Dim RezeptID As Integer
    Dim SortiID As Integer
    Dim FarbmID As Integer
    Dim RwertID As Integer
    Dim StrFarbmRezept As String
    Dim DatRow As DataGridViewRow
    Cursor = Cursors.WaitCursor

    If ConnOpen(Cndat) Then
      If Not IsNothing(TabColRezRwrt(0)) AndAlso dbgColRezRwrt(0).SelectedRows.Count > 0 Then
        '
        '
        'Lösche Farbmittel
        '
        '
        For Each DatRow In dbgColRezRwrt(0).SelectedRows
          FarbmID = DatRow.Cells("FARBM_ID").Value
          MischID = DatRow.Cells("MISCH_ID").Value
          StrFarbmRezept = StringWhere("TBL_REZEPT_FARBM", "REZEPT_ID", {"MISCH_ID", "FARBM_ID"}, {MischID, FarbmID}, Cndat)

          '
          '
          'TBL_PREIS
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_FARBM_PREIS WHERE MISCH_ID=" & MischID & " AND FARBM_ID=" & FarbmID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          '
          '
          'TBL_PROZ
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_FARBM_PROZ WHERE MISCH_ID=" & MischID & " AND FARBM_ID=" & FarbmID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          '
          '
          'TBL_PROB
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_FARBM_PROB WHERE MISCH_ID=" & MischID & " AND FARBM_ID=" & FarbmID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          '
          '
          'TBL_GRUND_FARBM
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_GRUND_FARBM WHERE MISCH_ID=" & MischID & " AND FARBM_ID=" & FarbmID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          '
          '
          'TBL_SORTI_FARBM
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_SORTI_FARBM WHERE MISCH_ID=" & MischID & " AND FARBM_ID=" & FarbmID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          '
          '
          'TBL_REZEPT_FARBM
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_REZEPT_FARBM WHERE MISCH_ID=" & MischID & " AND FARBM_ID=" & FarbmID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          '
          If radFarbmittel(1).Checked Or radFarbmittel(2).Checked Then
            '
            'Alle zum Rezept gehörigen Farbmittel werden gelöscht
            '
            '
            '
            '
            'TBL_REZEPT_FARBM
            '
            '
            CmdDelete.CommandText = "DELETE * FROM TBL_REZEPT_FARBM WHERE MISCH_ID=" & MischID & " AND REZEPT_ID IN " & StrFarbmRezept
            If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
              MsgBox("ERROR")
            End If
          End If
          If radfarbmittel(2).Checked Then
            '
            'Alle  Rezepte werden gelöscht
            '
            '
            'TBL_REZEPT_RWERT
            '
            '
            CmdDelete.CommandText = "DELETE * FROM TBL_REZEPT_RWERT WHERE MISCH_ID=" & MischID & " AND REZEPT_ID IN " & StrFarbmRezept
            If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
              MsgBox("ERROR")
            End If
            '
            '
            'TBL_REZEPT
            '
            '
            CmdDelete.CommandText = "DELETE * FROM TBL_REZEPT WHERE MISCH_ID=" & MischID & " AND REZEPT_ID IN " & StrFarbmRezept
            If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
              MsgBox("ERROR")
            End If
          End If

          '
          '
          '
          'TBL_FARBM wird gelöscht
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_FARBM WHERE MISCH_ID=" & MischID & " AND FARBM_ID=" & FarbmID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          dbgColRezRwrt(0).Rows.RemoveAt(DatRow.Index)
        Next
        TabColRezRwrt(0).AcceptChanges()
      End If
      '
      '
      '
      '
      'Rezepte
      '
      '
      If Not IsNothing(TabColRezRwrt(1)) AndAlso dbgColRezRwrt(1).SelectedRows.Count > 0 Then
        '
        '
        'Lösche Rezepte
        '
        '
        For Each DatRow In dbgColRezRwrt(1).SelectedRows
          RezeptID = DatRow.Cells("REZEPT_ID").Value
          MischID = DatRow.Cells("MISCH_ID").Value
          '
          '
          'TBL_REZEPT_RWERT
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_REZEPT_RWERT WHERE MISCH_ID=" & MischID & " AND REZEPT_ID=" & RezeptID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          '
          '
          '
          '
          'TBL_REZEPT_FARBM
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_REZEPT_FARBM WHERE MISCH_ID=" & MischID & " AND REZEPT_ID=" & RezeptID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If

          '
          'TBL_REZEPT
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_REZEPT WHERE MISCH_ID=" & MischID & " AND REZEPT_ID=" & RezeptID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          dbgColRezRwrt(1).Rows.RemoveAt(DatRow.Index)
        Next
        TabColRezRwrt(1).AcceptChanges()
      End If
      '
      '
      '
      'Sortimente
      '
      '
      If Not IsNothing(TabColRezRwrt(2)) AndAlso dbgColRezRwrt(2).SelectedRows.Count > 0 Then
        '
        '
        'Lösche Sortimente
        '
        '
        For Each DatRow In dbgColRezRwrt(2).SelectedRows
          SortiID = DatRow.Cells("SORTI_ID").Value
          MischID = DatRow.Cells("MISCH_ID").Value
          '
          '
          'TBL_SORTI_RWERT
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_SORTI_RWERT WHERE MISCH_ID=" & MischID & " AND SORTI_ID=" & SortiID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          '
          '
          '
          '
          'TBL_SORTI_FARBM
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_SORTI_FARBM WHERE MISCH_ID=" & MischID & " AND SORTI_ID=" & SortiID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If

          '
          'TBL_SORTI
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_SORTI WHERE MISCH_ID=" & MischID & " AND SORTI_ID=" & SortiID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          dbgColRezRwrt(2).Rows.RemoveAt(DatRow.Index)
        Next
        TabColRezRwrt(2).AcceptChanges()
      End If


      'R-Werte
      '
      If Not IsNothing(TabColRezRwrt(3)) AndAlso dbgColRezRwrt(3).SelectedRows.Count > 0 Then
        '
        '
        'Lösche Rwerte
        '
        '
        For Each DatRow In dbgColRezRwrt(3).SelectedRows
          MessgRwID = DatRow.Cells("MESSGRW_ID").Value
          RwertID = DatRow.Cells("RWERT_ID").Value
          '
          '
          '
          'TBL_QUALI
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_QUALI WHERE MESSGRW_ID=" & MessgRwID & " AND QUALI_ID=" & RwertID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          '
          '
          '
          'TBL_RWERT
          '
          '
          CmdDelete.CommandText = "DELETE * FROM TBL_RWERT WHERE MESSGRW_ID=" & MessgRwID & " AND RWERT_ID=" & RwertID
          If SQLExeNonQuery(CmdDelete, Cndat) <> 0 Then
            MsgBox("ERROR")
          End If
          dbgColRezRwrt(3).Rows.RemoveAt(DatRow.Index)
          'TabColRezRwrt(3).Rows.RemoveAt(DatRow.Index)
        Next
        TabColRezRwrt(3).AcceptChanges()
      End If
    End If
    Cndat.Close()
    Cursor = Cursors.Default
    MessageBox.Show(Texxt(2021), Texxt(2000), MessageBoxButtons.OK)
  End Sub
 

End Class