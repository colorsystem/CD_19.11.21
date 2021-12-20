Public Class HandleHilfLib
  Sub CheckMeasureDev(ByVal CnCol As OleDbConnection, ByVal CnTmp As OleDbConnection, ByVal MessgIDBas As Integer, ByVal MessgIDTmp As Integer, ByRef ier As Integer)
    Dim SqlStmt As String
    Dim SqlMessg As String
    Dim BoReadSet As Boolean
    Dim BoReadMessg As Boolean
    Dim DaReadset As OleDbDataReader
    Dim DaReadMessg As OleDbDataReader
    Dim Cmdset As OleDbCommand
    Dim CmdMessg As OleDbCommand
    ier = 0
    Cmdset = New OleDbCommand
    CmdMessg = New OleDbCommand
    Cmdset.Connection = CnTmp
    CmdMessg.Connection = CnCol
    '
    'altes Gerät
    '
    SqlMessg = "SELECT * FROM TBL_MESSG WHERE MESSG_ID=" & MessgIDBas
    CmdMessg.CommandText = SqlMessg
    DaReadMessg = DataReader(CmdMessg, CommandBehavior.SingleRow, CnCol)
    '
    '
    'neues Gerät
    '
    '
    SqlStmt = "SELECT * FROM TBL_MESSG WHERE MESSG_ID=" & MessgIDTmp
    Cmdset.CommandText = SqlStmt
    DaReadset = DataReader(Cmdset, CommandBehavior.SingleRow, CnTmp)
    If DaReadset.Read And DaReadMessg.Read Then
      If DaReadset("messg_norm_file_ID") <> DaReadMessg("messg_norm_file_ID") Then
        MsgBox(Texxt(3632) & ":  " & MessgIDBas & "," & MessgIDTmp)
        DaReadset.Close()
        DaReadMessg.Close()
        ier = 1
        Exit Sub
      End If
    End If
    DaReadset.Close()
    DaReadMessg.Close()
    '
    '
    'Tabelle TBL_MESSG_IHRM
    '
    '
    SqlMessg = "SELECT * FROM TBL_MESSG_IHRM WHERE MESSG_ID=" & MessgIDBas & " ORDER BY POS_ID"
    CmdMessg.CommandText = SqlMessg
    DaReadMessg = DataReader(CmdMessg, CommandBehavior.SingleResult, CnCol)
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_MESSG_IHRM WHERE MESSG_ID=" & MessgIDTmp & " ORDER BY POS_ID"
    Cmdset.CommandText = SqlStmt
    DaReadset = DataReader(Cmdset, CommandBehavior.SingleResult, CnTmp)
    Do
      BoReadSet = DaReadset.Read
      BoReadMessg = DaReadMessg.Read
      If BoReadSet Or BoReadMessg Then
        If Not (BoReadSet And BoReadMessg) Then
          MsgBox(Texxt(3633) & ":  " & MessgIDBas & "," & MessgIDTmp)
          DaReadset.Close()
          DaReadMessg.Close()
          ier = 1
          Exit Sub
        Else
          If DaReadset("ihrm_id") <> DaReadMessg("ihrm_id") Then
            MsgBox(Texxt(3634) & ":  " & MessgIDBas & "," & MessgIDTmp & Space(5) & Chrum(DaReadset("ihrm_id")) & "," & Chrum(DaReadMessg("ihrm_id")))
            DaReadset.Close()
            DaReadMessg.Close()
            ier = 1
            Exit Sub
          End If
        End If
      Else
        Exit Do
      End If
    Loop
    DaReadset.Close()
    DaReadMessg.Close()
    Cmdset.Dispose()
    CmdMessg.Dispose()
  End Sub
End Class
