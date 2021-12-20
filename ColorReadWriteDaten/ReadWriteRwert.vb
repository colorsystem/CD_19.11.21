Option Strict Off
Option Explicit On
Option Compare Text

Public Class ReadWriteRwert
  Implements IDisposable
  Dim NameLength As Integer
  Dim BemLength As Integer
  Dim BanumLength As Integer
  Dim ID As Integer
  Dim hlf() As Byte
  Dim Dehlf() As Byte
  Dim Mdim As Short
  Dim Rhilf() As Single
  Dim De() As Single
  Dim Camp() As Single
  Dim Lcamp As Short
  Dim i, kw As Short
  Dim Nku As Short
  Dim CmdDys As OleDbCommand
  Dim Dyset As OleDbDataReader
  Dim ier As Integer
  Dim SqlStmt As String


  Sub New()
    CmdDys = New OleDbCommand
    NameLength = StringLength("TBL_RWERT", "RWERT_NAME", Cndat)
    BemLength = StringLength("TBL_RWERT", "RWERT_BEM", Cndat)
    BanumLength = StringLength("TBL_RWERT", "RWERT_KENN", Cndat)


  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If Not IsNothing(CmdDys) Then
      CmdDys.Dispose()
    End If
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Function ReadRwertID(ByRef RwrtName As String, ByRef ier As Integer) As Integer
    ier = 0
    ReadRwertID = -1
    '

    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableRwert _
    & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_NAME='" & AddHkom(RwrtName, NameLength) & "'"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.CloseConnection Or CommandBehavior.SingleRow, Cndat)
    If Dyset.Read Then
      ReadRwertID = Dyset("RWERT_ID")
    End If
    Dyset.Close()
  End Function
  Sub DelRwert(ByRef ID As Integer, ByRef GID As Integer, ByRef ier As Integer)
    ier = 0
    If ReadRwrtOnly(GID) Then
      ier = 3014
      MsgBox(Texxt(ier))
      Exit Sub
    End If

    '
    '
    '
    'Löschen von Tabelle MenueParam.TableSortiRwert
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableSortiRwert _
    & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND [RWERT_ID]=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      Exit Sub
    End If
    '
    '
    'Löschen von Tabelle MenueParam.TableRezeptRwert
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableRezeptRwert _
    & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND [RWERT_ID]=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      Exit Sub
    End If
    '
    'Löschen von Tabelle Quali
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableQuali _
    & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND [QUALI_ID]=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      Exit Sub
    End If
    '
    '
    'Löschen von Tabelle Rwert
    '
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableRwert _
    & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND [RWERT_ID]=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      Exit Sub
    End If
  End Sub
  Function ContainsRwertID(ID As Integer) As Boolean
    ContainsRwertID = False
    SqlStmt = "SELECT * FROM " & MenueParam.TableRwert _
    & " WHERE [MESSGRW_ID]=" & MenueParam.Messg.MessgRwID & " AND [RWERT_ID]=" & ID & ";"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow, Cndat)
    If Dyset.Read Then
      ContainsRwertID = True
    End If
    Dyset.Close()
  End Function
  Sub ReadRwert(ByRef ID As Integer, ByRef ReFel As RefValue, ByRef ier As Integer)
    Dim kw As Integer
    Dim Winkel As AngGeos
    Dim CommBehav As CommandBehavior
    Winkel = MenueParam.Messg.Winkel
    '
    ier = 0
    ReFel.IVoNa = False
    ReFel.Iplott = False
    ReFel.Name = ""
    ReFel.Bem = ""
    '
    CommBehav = CommandBehavior.SingleRow
    '
    If Cndat.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If '
    '
    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableRwert _
    & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID=" & ID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommBehav, Cndat)
    Mdim = Winkel.Wsol.Nwe * Winkel.Km
    If Dyset.Read Then
      ReFel.ID = ID
      ReFel.ReTr = Dyset("rwert_retr")
      hlf = Dyset("rwert_rwert")
      If hlf.Length <> 4 * Mdim Then
        MsgBox(Texxt(2986), 0, Texxt(2000))
        ier = -20
        Exit Sub
      End If
      Rhilf = GetSingles(hlf)
      hlf = Dyset("rwert_de")
      De = GetSingles(hlf)
      '
      '
      ReFel.RefKurv.clear()
      '
      For kw = 0 To Winkel.Km - 1
        ReFel.RefKurv.Add(Winkel(kw).Chrm, New CurveRef(Winkel.Wsol.Nwe))
        ReFel.De(kw) = De(Winkel(kw).Lkm)
        Nku = Winkel(kw).Lkm * Winkel.Wsol.Nwe
        Array.Copy(Rhilf, Nku, ReFel.RefKurv(Winkel(kw).Chrm).R, 0, Winkel.Wsol.Nwe)
      Next kw
      ReFel.MessgID = Dyset("Messg_id")
      ReFel.Name = Dyset("Rwert_name")
      If IsDBNull(Dyset("rwert_kenn")) OrElse Dyset("rwert_kenn") = "" Then
        ReFel.Banum = " "
      Else
        ReFel.Banum = Dyset("rwert_kenn")
      End If
      If IsDBNull(Dyset("rwert_bem")) OrElse Dyset("rwert_bem") = "" Then
        ReFel.Bem = " "
      Else
        ReFel.Bem = Dyset("rwert_bem")
      End If
      ReFel.DatTim = Dyset("RWERT_DATTIM")
      ReFel.Iami = Dyset("rwert_iami")
      If IsDBNull(Dyset("rwert_cme")) OrElse Dyset("rwert_cme") = "" Then
        ReFel.Cme = "  "
      Else
        ReFel.Cme = Dyset("rwert_cme")
      End If
      ReFel.Iarch = Dyset("rwert_iarch")
      If ReFel.Iarch < 0 Then ReFel.Iarch = 2
      ReFel.Gid = 0
      If Dyset("rwert_gid") <> 0 Then
        ReFel.Gid = Dyset("rwert_gid")
      End If
      ReFel.IVoNa = True
      ReFel.Iplott = True
    Else
      ier = 3564
      MsgBox(Texxt(ier) & CStr(ID))
    End If
    Dyset.Close()
    Erase Rhilf
    Erase De
  End Sub
  Sub ReadQuali(ByRef ID As Integer, ByRef ReFel As RefValue, ByRef ier As Integer)
    Dim CommBehav As CommandBehavior
    Dim i As Integer
    ier = -1
    If ID < 0 Then Exit Sub
    '
    '
    CommBehav = CommandBehavior.SingleRow
    '
    If Cndat.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If
    ier = -2
    If IsNothing(ReFel.QuControl) Then
      Exit Sub
    End If
    '
    '
    ier = 0
    ReFel.QuControl.Cart = "  "
    ReFel.QuControl.MethID = -1
    ReFel.QuControl.IuntID = -1
    ReFel.QuControl.MedNr = -1
    Lcamp = 16
    For i = 0 To Lcamp - 1
      ReFel.QuControl.Camp(i) = -1.0
    Next i
    SqlStmt = "SELECT *  FROM " & MenueParam.TableQuali _
    & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND QUALI_ID=" & ID & " AND METH_ID=" & MenueParam.MethID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommBehav, Cndat)
    '
    If Dyset.Read Then
      If Not IsDBNull(Dyset("Quali_Cart")) Then
        ReFel.QuControl.Cart = Dyset("Quali_Cart")
      End If
      ReFel.QuControl.MethID = Dyset("METH_ID")
      If Not IsDBNull(Dyset("Quali_IUNT")) Then
        ReFel.QuControl.IuntID = Dyset("Quali_IUNT")
      End If
      If Not IsDBNull(Dyset("Quali_mednr")) Then
        ReFel.QuControl.MedNr = Dyset("Quali_mednr")
      End If
      hlf = Dyset("Quali_CAMP")
      If Not IsDBNull(hlf) Then
        Lcamp = hlf.Length / 4
        If Lcamp > 0 Then
          Camp = GetSingles(hlf)
          For i = 0 To Lcamp - 1
            ReFel.QuControl.Camp(i) = Camp(i)
          Next i
        End If
      End If
    End If
    Dyset.Close()
    Erase Camp
    Exit Sub
ErrQuTable:
  End Sub

  Sub Update(ByRef ID As Integer, ByRef GID As Integer, ByRef IARCH As Short, ByRef Name As String, ByRef Bem As String, ByRef Banum As String, ByRef MessgID As Integer, ByRef CME As String, ByRef ier As Integer)
    Dim CC As String
    ier = 0
    If ReadRwrtOnly(GID) Then
      ier = 3014
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    CC = CME
    If CC = "" Then
      CC = "  "
    End If

    SqlStmt = "UPDATE " & MenueParam.TableRwert & " SET [MESSG_ID]=" & MessgID & ",[RWERT_GID]=" & GID & ",[RWERT_CME]='" & AddHkomE(CC) & "',[RWERT_IARCH]=" & IARCH & ",[RWERT_NAME]='" & AddHkom(Name, NameLength) _
    & "',[RWERT_BEM]='" & AddHkom(Bem, BemLength) & "'" & ",[RWERT_KENN]='" & AddHkom(Banum, BanumLength) & "'" & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      Exit Sub
    End If
  End Sub
  Sub UpdateRwert(ByVal Refel As RefValue, ByRef ier As Integer)
    Dim kw As Integer
    Dim i As Integer
    ier = 0
    If ReadRwrtOnly(Refel.Gid) Then
      ier = 3014
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    If Refel.Cme = "" Then
      Refel.Cme = "  "
    End If
    If Len(Refel.Banum) > BanumLength Then
      Refel.Banum = Refel.Banum.Substring(0, BanumLength)
    End If
    SqlStmt = "UPDATE " & MenueParam.TableRwert & " SET [MESSG_ID]=?" _
    & ",RWERT_GID=?,RWERT_NAME=?,RWERT_BEM=?,RWERT_KENN=?,RWERT_IARCH=?,RWERT_DATTIM=?,RWERT_CME=?,RWERT_RETR=?,RWERT_IAMI=?,RWERT_DE=?,RWERT_RWERT=?" _
    & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_ID=" & Refel.ID

    CmdDys.CommandText = SqlStmt
    CmdDys.Parameters.Clear()
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_MESSGID", OleDbType.Integer))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_GID", OleDbType.Integer))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_NAME", OleDbType.Char))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_BEM", OleDbType.Char))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_KENN", OleDbType.Char))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_IARCH", OleDbType.SmallInt))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_DATTIM", OleDbType.Date))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_CME", OleDbType.Char))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_RETR", OleDbType.SmallInt))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_IAMI", OleDbType.SmallInt))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_DE", OleDbType.Binary))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_RWERT", OleDbType.Binary))

    Mdim = MenueParam.Messg.Winkel.Km * MenueParam.Messg.Winkel.Wsol.Nwe - 1
    ReDim Rhilf(Mdim)
    ReDim De(MenueParam.Messg.Winkel.Km - 1)
    For kw = 0 To MenueParam.Messg.Winkel.Km - 1
      De(MenueParam.Messg.Winkel(kw).Lkm) = Refel.De(kw)
      Nku = MenueParam.Messg.Winkel(kw).Lkm * MenueParam.Messg.Winkel.Wsol.Nwe
      For i = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
        Rhilf(Nku + i) = Refel.RefKurv(MenueParam.Messg.Winkel(kw).Chrm).R(i)
      Next i
    Next kw
    hlf = GetBytes(Rhilf)
    Dehlf = GetBytes(De)
    Erase Rhilf
    Erase De
    CmdDys.Parameters("rwert_messgid").Value = Refel.MessgID
    CmdDys.Parameters("rwert_gid").Value = Refel.Gid
    CmdDys.Parameters("Rwert_name").Value = AddHkom(Refel.Name, NameLength)
    CmdDys.Parameters("rwert_bem").Value = AddHkom(Refel.Bem, BemLength)
    CmdDys.Parameters("rwert_kenn").Value = AddHkom(Refel.Banum, BanumLength)
    CmdDys.Parameters("rwert_iarch").Value = Refel.Iarch
    CmdDys.Parameters("RWERT_DATTIM").Value = Refel.DatTim
    CmdDys.Parameters("rwert_cme").Value = Refel.Cme
    CmdDys.Parameters("rwert_retr").Value = Refel.ReTr
    CmdDys.Parameters("rwert_iami").Value = Refel.Iami
    CmdDys.Parameters("rwert_de").Value = Dehlf
    CmdDys.Parameters("rwert_rwert").Value = hlf
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
    End If
    CmdDys.Parameters.Clear()
  End Sub
  Sub WriteRwert(ByRef ID As Integer, ByRef ReFel As RefValue, ByRef ier As Integer)
    Dim i As Integer
    Dim kw As Integer
    Dim Winkel As AngGeos
    ier = 0
    Winkel = MenueParam.Messg.Winkel
    If ReadRwrtOnly(ReFel.Gid) Then
      ier = 3014
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    SqlStmt = "INSERT INTO " & MenueParam.TableRwert & " (RWERT_ID,MESSGRW_ID,MESSG_ID,USER_ID,RWERT_GID,RWERT_NAME,RWERT_BEM,RWERT_KENN,RWERT_IARCH,RWERT_DATTIM,RWERT_CME,RWERT_RETR,RWERT_IAMI,RWERT_DE,RWERT_RWERT)" _
    & " VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    CmdDys.CommandText = SqlStmt
    CmdDys.Parameters.Clear()
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_ID", OleDbType.Integer))
    CmdDys.Parameters.Add(New OleDbParameter("MESSGRW_ID", OleDbType.Integer))
    CmdDys.Parameters.Add(New OleDbParameter("MESSG_ID", OleDbType.Integer))
    CmdDys.Parameters.Add(New OleDbParameter("USER_ID", OleDbType.Integer))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_GID", OleDbType.Integer))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_NAME", OleDbType.Char))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_BEM", OleDbType.Char))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_KENN", OleDbType.Char))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_IARCH", OleDbType.SmallInt))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_DATTIM", OleDbType.Date))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_CME", OleDbType.Char))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_RETR", OleDbType.SmallInt))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_IAMI", OleDbType.SmallInt))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_DE", OleDbType.Binary))
    CmdDys.Parameters.Add(New OleDbParameter("RWERT_RWERT", OleDbType.Binary))




    ID = RefMax(MenueParam.TableRwert, MenueParam.Messg.MessgRwID) + 1
    ReFel.ID = ID
    Mdim = Winkel.Km * Winkel.Wsol.Nwe - 1
    ReDim Rhilf(Mdim)
    ReDim De(Winkel.Km - 1)
    For kw = 0 To Winkel.Km - 1
      De(Winkel(kw).Lkm) = ReFel.De(kw)
      Nku = Winkel(kw).Lkm * Winkel.Wsol.Nwe
      For i = 0 To Winkel.Wsol.Nwe - 1
        Rhilf(Nku + i) = ReFel.RefKurv(Winkel(kw).Chrm).R(i)
      Next i
    Next kw
    hlf = GetBytes(Rhilf)
    Dehlf = GetBytes(De)
    Erase Rhilf
    Erase De
    CmdDys.Parameters("Rwert_ID").Value = ID
    CmdDys.Parameters("messgrw_id").Value = MenueParam.Messg.MessgRwID
    CmdDys.Parameters("messg_id").Value = MenueParam.MessgID
    CmdDys.Parameters("user_id").Value = MenueParam.UserID
    CmdDys.Parameters("rwert_gid").Value = ReFel.Gid
    CmdDys.Parameters("Rwert_name").Value = AddHkom(ReFel.Name, NameLength)
    CmdDys.Parameters("rwert_bem").Value = AddHkom(ReFel.Bem, BemLength)
    CmdDys.Parameters("rwert_kenn").Value = AddHkom(ReFel.Banum, BanumLength)
    CmdDys.Parameters("rwert_iarch").Value = ReFel.Iarch
    CmdDys.Parameters("RWERT_DATTIM").Value = Date.Now
    CmdDys.Parameters("rwert_cme").Value = ReFel.Cme
    CmdDys.Parameters("rwert_retr").Value = ReFel.ReTr
    CmdDys.Parameters("rwert_iami").Value = ReFel.Iami
    CmdDys.Parameters("rwert_de").Value = Dehlf
    CmdDys.Parameters("rwert_rwert").Value = hlf
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
    End If
    CmdDys.Parameters.Clear()
  End Sub
  Sub WriteQuali(ByRef ID As Integer, ByRef ReFel As RefValue, ByRef ier As Integer)
    Dim i As Integer
    If ID < 0 Then Exit Sub
    ier = 0
    Lcamp = 16
    ReDim Camp(Lcamp - 1)
    For i = 0 To Lcamp - 1
      Camp(i) = ReFel.QuControl.Camp(i)
    Next i
    hlf = GetBytes(Camp)
    Erase Camp
    SqlStmt = "DELETE * FROM " & MenueParam.TableQuali & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND QUALI_ID=" & ID & " AND METH_ID=" & ReFel.QuControl.MethID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
    End If



    SqlStmt = "INSERT INTO " & MenueParam.TableQuali & " (QUALI_ID,MESSGRW_ID,METH_ID,QUALI_CART,QUALI_IUNT,QUALI_MEDNR,QUALI_DATTIM,QUALI_CAMP)" _
    & " VALUES(?,?,?,?,?,?,?,?)"
    CmdDys.CommandText = SqlStmt
    CmdDys.Parameters.Clear()
    CmdDys.Parameters.Add(New OleDbParameter("QUALI_ID", OleDbType.Integer))
    CmdDys.Parameters.Add(New OleDbParameter("MESSGRW_ID", OleDbType.Integer))
    CmdDys.Parameters.Add(New OleDbParameter("METH_ID", OleDbType.Char))
    CmdDys.Parameters.Add(New OleDbParameter("QUALI_CART", OleDbType.Char))
    CmdDys.Parameters.Add(New OleDbParameter("QUALI_IUNT", OleDbType.Char))
    CmdDys.Parameters.Add(New OleDbParameter("QUALI_MEDNR", OleDbType.Integer))
    CmdDys.Parameters.Add(New OleDbParameter("QUALI_DATTIM", OleDbType.Date))
    CmdDys.Parameters.Add(New OleDbParameter("QUALI_CAMP", OleDbType.Binary))
    '
    '
    '
    CmdDys.Parameters("Quali_ID").Value = ID
    CmdDys.Parameters("Messgrw_ID").Value = MenueParam.Messg.MessgRwID
    CmdDys.Parameters("METH_ID").Value = ReFel.QuControl.MethID
    CmdDys.Parameters("Quali_Cart").Value = ReFel.QuControl.Cart
    CmdDys.Parameters("Quali_IUNT").Value = ReFel.QuControl.IuntID
    CmdDys.Parameters("Quali_mednr").Value = ReFel.QuControl.MedNr
    CmdDys.Parameters("Quali_dattim").Value = Date.Now
    CmdDys.Parameters("Quali_CAMP").Value = hlf
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
    End If
  End Sub


  Function RefMax(ByRef TableRwert As String, ByRef MessgRwID As Integer, Optional ByRef CnDBase As OleDbConnection = Nothing) As Integer
    Dim SqlStmt As String
    Dim AcConn As OleDbConnection
    Dim CommBehav As CommandBehavior
    If IsNothing(CnDBase) Then
      AcConn = Cndat()
    Else
      AcConn = CnDBase
    End If
    '
    CommBehav = CommandBehavior.SingleRow
    '
    If AcConn.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If
    SqlStmt = "SELECT MAX([RWERT_ID]) AS  [MAX_ID]  FROM " & TableRwert & " WHERE MESSGRW_ID=" & MessgRwID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommBehav, AcConn)
    If Dyset.Read AndAlso Not IsDBNull(Dyset("max_id")) Then
      RefMax = Dyset("max_id")
    Else
      RefMax = 0
    End If
    Dyset.Close()
  End Function
  Function RefMin(ByRef TableRwert As String, ByRef Dat As Date, ByRef MessgRwID As Integer) As Integer
    Dim CommBehav As CommandBehavior
    If DateDiff(DateInterval.Day, Dat, Today) > 5000 Then
      RefMin = 0
      Exit Function
    End If
    CommBehav = CommandBehavior.SingleRow
    '
    If Cndat.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If
    SqlStmt = "SELECT MIN([RWERT_ID]) AS [MIN_ID] FROM " & TableRwert & " WHERE RWERT_DATTIM > ? AND MESSGRW_ID=" & MessgRwID
    'SqlStmt = "SELECT * FROM " & TableRwert & " ORDER BY RWERT_ID DESC WHERE RWERT_DATE > " & sqlda(Dat) & " AND MESSG_ID=" & MessgID
    CmdDys.CommandText = SqlStmt
    CmdDys.Parameters.Clear()
    CmdDys.Parameters.Add(New OleDbParameter("DATTIM", OleDbType.Date))
    CmdDys.Parameters("DATTIM").Value = Dat.AddDays(-1.0)

    Dyset = DataReader(CmdDys, CommBehav, Cndat)
    If Dyset.Read Then
      If Not IsDBNull(Dyset("MIN_ID")) Then
        RefMin = Dyset("MIN_ID")
      Else
        RefMin = 0
      End If
    Else
      RefMin = 0
    End If
    Dyset.Close()
  End Function
End Class