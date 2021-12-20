Option Strict Off
Option Explicit On
Option Compare Text


Public Class ReadWriteGrund
  Implements IDisposable
  Dim hlf() As Byte
  Dim Nwe As Short
  Dim Mdim As Short
  Dim Rhilf() As Single
  Dim Cst() As Single
  Dim kw As Short
  Dim nkw, Nku As Integer
  Dim NPS As Integer
  Dim Daset As OleDbDataReader
  Dim CmdDas As OleDbCommand
  Dim SqlStmt As String
  Dim KeyD As String
  Dim FaID As Integer



  Sub New()
    CmdDas = New OleDbCommand
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    CmdDas.Dispose()
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub





  Sub ReadGrund(ByRef ID As Integer, ByRef OptKonst As OpticalData, ByRef ier As Integer, Optional ByRef Combeh As CommandBehavior = CommandBehavior.Default)
    Dim k As Integer
    Dim kw As Integer
    Dim WithMessg As String
    Dim Winkel As AngGeos
    '
    '
    '
    Winkel = MenueParam.Messg.Winkel
    ier = 0
    '
    '
    Nwe = Winkel.Wsol.Nwe
    '
    '
    'If BitWrt(27, MenueParam.User.Writ) Then
    WithMessg = "TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    'Else
    'WithMessg = "TBL_GRUND_FARBM.MESSG_ID=" & MenueParam.Messg.MessgID
    'End If

    SqlStmt = "SELECT * FROM " & MenueParam.TableGrundFarbm _
    & " WHERE " & WithMessg & " AND MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & ID & " AND GKWRT_ID=" & MenueParam.Misch.GKwrtID & " AND FARBM_NPX=" & MenueParam.Misch.Npx
    CmdDas.CommandText = SqlStmt
    Daset = DataReader(CmdDas, CommandBehavior.SingleRow Or Combeh, Cndat)
    If Daset.Read Then
      '
      '
      '
      'Kurven löschen
      '
      '
      OptKonst = New OpticalData
      OptKonst.Grund.clear()
      OptKonst.Nst = 0
      OptKonst.Cst(0) = 0
      OptKonst.OptID = ID
      OptKonst.Npx = MenueParam.Misch.Npx
      OptKonst.GKID = MenueParam.Misch.GKwrtID
      OptKonst.Fest = 0

      OptKonst.DatTim = Daset("Farbm_dattim")
      OptKonst.Nst = Daset("farbm_NST")
      OptKonst.Fest = Daset("farbm_fix")
      OptKonst.Npx = Daset("Farbm_npx")
      OptKonst.GKID = Daset("gkwrt_ID")
      NPS = NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, OptKonst.Nst)
      For k = 0 To NPS - 1
        OptKonst.Grund.Add(KeyName(k), New CurvesRef)
        Call ADDCurves(OptKonst.Grund(k))
        'For kw = 0 To Winkel.Km - 1
        ' OptKonst.Grund(k).Add(Winkel(kw).Chrm, New CurveRef(Nwe))
        'Next kw
      Next k
      If OptKonst.Nst > 0 Then
        hlf = Daset("farbm_CST")
        Cst = GetSingles(hlf)
        For i = 0 To Cst.Length - 1
          OptKonst.Cst(i) = Cst(i)
        Next i
      End If
      hlf = Daset("farbm_absstr")
      Rhilf = GetSingles(hlf)
      For k = 0 To NPS - 1
        For kw = 0 To Winkel.Km - 1
          Nku = Winkel(kw).Lkm * NPS * Nwe
          For i = 0 To Nwe - 1
            OptKonst.Grund(k)(Winkel(kw).Chrm).R(i) = Rhilf(i + k * Nwe + Nku)
          Next i
        Next kw
      Next k
      Erase Rhilf
    Else
      ier = 2503
    End If
    Daset.Close()
  End Sub
  Sub WriteGrund(ByRef ID As Integer, ByRef OptKonst As OpticalData, ByRef WinkMes As AngGeos, ByRef ier As Integer, Optional ByRef Conn As OleDbConnection = Nothing)
    Dim AcConn As OleDbConnection
    Dim kw As Integer
    If IsNothing(Conn) Then
      AcConn = Cndat()
    Else
      AcConn = Conn
    End If


    '
    '
    'Löschen Grunddaten, falls vorhanden
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableGrundFarbm _
    & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & ID & " AND GKWRT_ID=" & MenueParam.Misch.GKwrtID
    CmdDas.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDas, AcConn) <> 0 Then
      ier = -1
      Exit Sub
    End If
    '
    '
    'Schreiben Grunddaten
    '
    '
    '
    '
    OptKonst.OptID = ID
    NPS = NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, OptKonst.Nst)
    SqlStmt = "INSERT INTO " & MenueParam.TableGrundFarbm & " (FARBM_ID,MISCH_ID,MESSGRW_ID,MESSG_ID,GKWRT_ID,FARBM_FIX,FARBM_NPX,FARBM_DATTIM,FARBM_NST,FARBM_CST,FARBM_ABSSTR)" _
    & " VALUES(?,?,?,?,?,?,?,?,?,?,?)"
    CmdDas.CommandText = SqlStmt
    CmdDas.Parameters.Clear()
    CmdDas.Parameters.Add(New OleDbParameter("farbm_id", OleDbType.Integer))
    CmdDas.Parameters.Add(New OleDbParameter("Misch_id", OleDbType.Integer))
    CmdDas.Parameters.Add(New OleDbParameter("Messgrw_id", OleDbType.Integer))
    CmdDas.Parameters.Add(New OleDbParameter("Messg_id", OleDbType.Integer))
    CmdDas.Parameters.Add(New OleDbParameter("gkwrt_id", OleDbType.Integer))
    CmdDas.Parameters.Add(New OleDbParameter("farbm_fix", OleDbType.SmallInt))
    CmdDas.Parameters.Add(New OleDbParameter("farbm_npx", OleDbType.SmallInt))
    CmdDas.Parameters.Add(New OleDbParameter("Farbm_dattim", OleDbType.DBDate))
    CmdDas.Parameters.Add(New OleDbParameter("farbm_NST", OleDbType.SmallInt))
    CmdDas.Parameters.Add(New OleDbParameter("farbm_CST", OleDbType.Binary))
    CmdDas.Parameters.Add(New OleDbParameter("farbm_absstr", OleDbType.Binary))
    '
    CmdDas.Parameters("farbm_id").Value = ID
    CmdDas.Parameters("misch_id").Value = MenueParam.MischID
    CmdDas.Parameters("messgrw_id").Value = MenueParam.Messg.MessgRwID
    CmdDas.Parameters("messg_id").Value = MenueParam.MessgID
    CmdDas.Parameters("gkwrt_id").Value = MenueParam.Misch.GKwrtID
    CmdDas.Parameters("farbm_fix").Value = OptKonst.Fest
    CmdDas.Parameters("farbm_npx").Value = MenueParam.Misch.Npx
    CmdDas.Parameters("Farbm_dattim").Value = Date.Now
    CmdDas.Parameters("farbm_NST").Value = OptKonst.Nst
    ReDim Cst(OptKonst.Nst)
    For i = 0 To OptKonst.Nst
      Cst(i) = OptKonst.Cst(i)
    Next i
    hlf = GetBytes(Cst)
    CmdDas.Parameters("farbm_CST").Value = hlf
    Nwe = WinkMes.Wsol.Nwe
    Mdim = WinkMes.Wsol.Nwe * NPS * WinkMes.Km - 1
    ReDim Rhilf(Mdim)
    For k = 0 To NPS - 1
      For kw = 0 To WinkMes.Km - 1
        For i = 0 To Nwe - 1
          Rhilf(i + k * Nwe + NPS * Nwe * kw) = OptKonst.Grund(k)((WinkMes(kw).Chrm)).R(i)
        Next i
      Next kw
    Next k
    hlf = GetBytes(Rhilf)
    CmdDas.Parameters("farbm_absstr").Value = hlf
    CmdDas.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDas, AcConn) <> 0 Then
      ier = -1
      Exit Sub
    End If
    Erase Cst
    Erase Rhilf
  End Sub
End Class