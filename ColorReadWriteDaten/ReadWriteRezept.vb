Option Strict Off
Option Explicit On
Option Compare Text
Public Class ReadWriteRezept
  Implements IDisposable

  Dim i As Integer
  Dim j As Integer
  Dim FaId As Integer
  Dim FaNr As String
  Dim CmdDys As OleDbCommand
  Dim dyset As OleDbDataReader
  Dim SqlStmt As String
  Dim KeyD As String
  Dim ReWrFarbe As ReadWriteFarbe
  Dim ReWrGrund As ReadWriteGrund
  Dim NameLength As Integer
  Dim BemLength As Integer
  Dim Tol As Single = 0.001




  Sub New()
    CmdDys = New OleDbCommand
    ReWrFarbe = New ReadWriteFarbe
    ReWrGrund = New ReadWriteGrund
    NameLength = StringLength("TBL_REZEPT", "REZEPT_NAME", Cndat)
    BemLength = StringLength("TBL_REZEPT", "REZEPT_BEM", Cndat)
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    Try
      If Not IsNothing(CmdDys) Then
        CmdDys.Dispose()
      End If
    Catch
    End Try
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub ReadRezeptName(ByRef RzName As String, ByRef ID As Integer, ByRef ier As Integer)
    Dim CommBehav As CommandBehavior
    '


    ier = 0
    ID = -1
    '
    CommBehav = CommandBehavior.SingleRow
    '
    If Cndat.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If '
    SqlStmt = "SELECT * FROM " & MenueParam.TableRezept _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_NAME='" & AddHkom(RzName, NameLength) & "'"
    CmdDys.CommandText = SqlStmt
    dyset = DataReader(CmdDys, CommBehav, Cndat)
    If dyset.Read Then
      ID = dyset("REZEPT_ID")
    End If
    dyset.Close()
  End Sub
  Function ContainsRezeptID(ID As Integer) As Boolean
    ContainsRezeptID = False
    SqlStmt = "SELECT * FROM " & MenueParam.TableRezept _
    & " WHERE [MISCH_ID]=" & MenueParam.MischID & " AND [REZEPT_ID]=" & ID & ";"
    CmdDys.CommandText = SqlStmt
    dyset = DataReader(CmdDys, CommandBehavior.SingleRow, Cndat)
    If dyset.Read Then
      ContainsRezeptID = True
    End If
    dyset.Close()
  End Function
  Sub ReadRezept(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef SmpID() As Integer, ByRef ier As Integer)
    Dim i As Integer
    Dim CommBehav As CommandBehavior

    ier = 0
  
    For i = 0 To 1
      UntID(i) = -1
    Next i
    For i = 0 To 1
      TypID(i) = -1
    Next i
    For i = 0 To 1
      SmpID(i) = -1
    Next i
    '
    '
    CommBehav = CommandBehavior.Default
    '
    If Cndat.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If
    '
    '
    'Vorhandene Farbmittel in Rezepten Löschen
    '
    '
    RezALL.Rezepte(RzNr).clear()
    RezALL.Rezepte(RzNr).Sorkrit(0).clear()
    RezALL.Rezepte(RzNr).Sorkrit(1).clear()

    '
    '
    '
    '
    'Neuaufbau
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableRezept _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID=" & ID
    CmdDys.CommandText = SqlStmt
    dyset = DataReader(CmdDys, CommBehav, Cndat)
    '
    'Rezept hinzufügen
    '
    '
    '
    '
    '
    '
    '
    If dyset.Read Then
      RezALL.IVOL = dyset("rezept_ivol")
      RezALL.INM = dyset("rezept_inm")
      RezALL.INF = dyset("rezept_inf")
      RezALL.INO = dyset("rezept_ino")
      RezALL.MngMin = dyset("rezept_mngmin")
      RezALL.MngMax = dyset("rezept_mngmax")
      RezALL.INP = dyset("rezept_inp")
      If RezALL.INP < 0 Then RezALL.INP = 0
      RezALL.INQ = dyset("rezept_inq")
      If RezALL.INQ < 0 Then RezALL.INQ = 0
      RezALL.ProzMin = dyset("rezept_prozmin")
      RezALL.ProzMax = dyset("rezept_prozmax")
      If RezALL.INP = 0 And RezALL.INQ = 0 Then
        RezALL.ProzMin = 0.0
        RezALL.ProzMax = 0.0
      End If
      RezALL.Rezepte(RzNr).ID = dyset("rezept_id")
      RezALL.Rezepte(RzNr).Name = dyset("rezept_name")
      If IsDBNull(dyset("Rezept_bem")) Then
        RezALL.Rezepte(RzNr).Bem = ""
      Else
        RezALL.Rezepte(RzNr).Bem = dyset("Rezept_bem")
      End If
      RezALL.Rezepte(RzNr).Iarch = dyset("rezept_iarch")
      RezALL.Rezepte(RzNr).Gid = dyset("Rezept_GID")
      RezALL.Rezepte(RzNr).DatTim = dyset("Rezept_DATTIM")
      RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(0)) = dyset("rezept_dik1")
      RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(1)) = dyset("rezept_dik2")
      RezALL.Rezepte(RzNr).GlzGrd = dyset("rezept_glzgrd")
      RezALL.Rezepte(RzNr).UmMng = 1.0#
    Else
      ier = 4672
      dyset.Close()
      Exit Sub
    End If
    dyset.Close()
    '
    '
    'Farbmittel einlesen
    '
    '
    '
    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableRezeptFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID=" & ID & " ORDER BY FARBM_IPOS"
    CmdDys.CommandText = SqlStmt
    dyset = DataReader(CmdDys, CommBehav, Cndat)
    '
    '
    i = 0
    Do While dyset.Read
      FaNr = KeyRe(i)
      '
      '
      '
      '
      ' Menge für Rezept
      '
      RezALL.Rezepte(RzNr).AddFaNr(FaNr, New ColorAmount)
      RezALL.Rezepte(RzNr)(FaNr).ID = dyset("farbm_id")
      RezALL.Rezepte(RzNr)(FaNr).FaAmng = dyset("farbm_menge")
      RezALL.Rezepte(RzNr)(FaNr).Proz = Singl(dyset("FARBM_PROZ"))
      RezALL.Rezepte(RzNr)(FaNr).Prob = dyset("FARBM_PROB")
      '
      '   Farbmittel

      i = i + 1
    Loop
    dyset.Close()


    '
    '
    '
    'R-Wert_ID einlesen
    '
    '

    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableRezeptRwert _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND REZEPT_ID=" & ID
    CmdDys.CommandText = SqlStmt
    dyset = DataReader(CmdDys, CommBehav, Cndat)
    '
    '
    '
    Do While dyset.Read
      Select Case dyset("Rwert_kwb")
        Case 1
          '
          '
          'Nachstellung (weiß)
          '
          '
          SmpID(RezALL.Rezepte.kwb(0)) = dyset("Rwert_ID")
        Case 2
          '
          '
          'Nachstellung (schwarz)
          '
          '
          SmpID(RezALL.Rezepte.kwb(1)) = dyset("Rwert_ID")
        Case -1
          '
          '
          'Untergrund (weiß)
          '
          '
          UntID(RezALL.Rezepte.kwb(0)) = dyset("Rwert_ID")
        Case -2
          '
          '
          'Untergrund (schwarz)
          '
          '
          UntID(RezALL.Rezepte.kwb(1)) = dyset("Rwert_ID")
        Case 11
          '
          '
          'Vorlage (weiß)
          '
          '
          TypID(RezALL.Rezepte.kwb(0)) = dyset("Rwert_ID")
        Case 12
          '
          '
          'Vorlage (schwarz)
          '
          '
          TypID(RezALL.Rezepte.kwb(1)) = dyset("Rwert_ID")
      End Select
    Loop
    dyset.Close()
  End Sub
  Sub ReadRezeptFarbGrund(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef SmpID() As Integer, ByRef ier As Integer)
    Dim j As Integer
    Dim CommBehav As CommandBehavior

    ier = 0
    '
    CommBehav = CommandBehavior.Default
    '
    If Cndat.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If
    Call ReadRezept(RzNr, ID, RezALL, UntID, TypID, SmpID, ier)
    '
    '
    '   Farbmittel
    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableRezeptFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID=" & ID & " ORDER BY FARBM_IPOS"
    CmdDys.CommandText = SqlStmt
    dyset = DataReader(CmdDys, CommBehav, Cndat)
    '
    '
    Do While dyset.Read
      FaId = dyset("farbm_id")
      KeyD = KeyName(FaId)
      '
      '
      'Prüfen, ob Farbmittel bereits vorhanden
      '
      '
      For j = 0 To RezALL.Farben.FarbCount - 1
        If RezALL.Farben(j).ID = FaId Then Exit For
      Next j
      If j > RezALL.Farben.FarbCount - 1 Then
        '
        '
        '     Neues Farbmittel hinzufügen
        '
        '
        RezALL.Farben.AddFarb(KeyD, New Colorant)
        '
        '
        '
        '
        RezALL.Farben(KeyD).ID = FaId
        RezALL.Farben(KeyD).BuMng = 0.0#
        RezALL.Farben(KeyD).BoMng = dyset("farbm_limmng")
        RezALL.Farben(KeyD).OP = Space(1)
        If Not IsDBNull(dyset("farbm_operat")) Then
          RezALL.Farben(KeyD).OP = UCase(dyset("farbm_operat"))
        End If
        RezALL.Farben(KeyD).Kto = Space(1)
        RezALL.Farben(KeyD).Preis = Singl(dyset("farbm_preis"))
        '
        '
        '

      End If
    Loop
    '
    '
    '
    '
    'Farbmittel und Grunddaten einlesen
    '
    '
    '
    For j = 0 To RezALL.Farben.FarbCount - 1
      '
      'von Tabelle Farben einlesen
      '
      FaId = RezALL.Farben(j).ID
      KeyD = KeyName(FaId)
      If RezALL.Farben(KeyD).PrbCount = 0 Or RezALL.Farben(KeyD).PreCount = 0 Or RezALL.Farben(KeyD).PrfCount = 0 Then
        '
        ReWrFarbe.FarReaGrund(FaId, RezALL.Farben(KeyD), False, ier)
        If ier = 2503 Then
          'Falls keine Grunddaten vorhanden, wird Fehlermeldung unterdrückt
          'Optdata ist dann =Nothing
          ier = 0
        ElseIf ier <> 0 Then
          Exit For
        End If
      End If
      ''
    Next j
    dyset.Close()


  End Sub
  Private Sub EinfFARezept(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef ier As Integer)
    Dim LiMenge As Single
    Dim i As Integer
    ier = 0
    If ReadRezeptOnly(RezALL.Rezepte(RzNr).Gid) Then
      ier = 3014
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    If ID < 0 Then
      ier = 3552
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    For i = 0 To RezALL.Rezepte(RzNr).KF - 1
      FaId = RezALL.Rezepte(RzNr)(i).ID
      KeyD = KeyName(FaId)
      If RezALL.Farben(KeyD).OP = "" Then
        RezALL.Farben(KeyD).OP = Space(1)
      End If
      LiMenge = RezALL.Rezepte(RzNr).UmMng * RezALL.Farben(KeyD).BoMng
      SqlStmt = "INSERT INTO " & MenueParam.TableRezeptFarbm _
      & "([REZEPT_ID],[MISCH_ID],[FARBM_ID],[FARBM_MENGE],[FARBM_LIMMNG],[FARBM_OPERAT]," _
      & "[FARBM_PREIS],[FARBM_PROZ],[FARBM_PROB],[FARBM_IPOS])" _
      & " VALUES(" & ID & "," & MenueParam.MischID & "," & FaId & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr)(i).FaAmng)) _
      & "," & SQLpunkt(CStr(LiMenge)) & ",'" & RezALL.Farben(KeyD).OP & "'," _
      & SQLpunkt(CStr(RezALL.Farben(KeyD).Preis)) & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr)(i).Proz)) _
      & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr)(i).Prob)) & "," & CByte(i) & ")"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit For
      End If
    Next i

  End Sub

  Public Sub EinfRWRezept(ByRef ID As Integer, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef SmpID() As Integer, ByRef Rezepte As Recipes, ByRef ier As Integer)
    Dim i As Integer
    Dim kwb As Integer
    Dim Rkwb As Integer
    ier = 0
    If ID < 0 Then
      ier = 3552
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    For i = 0 To 1
      '
      'Speichern Vorlage
      '
      '
      Rkwb = Rezepte.kwb(i)
      If TypID(Rkwb) >= 0 Then
        kwb = 11 + i
        SqlStmt = "DELETE * FROM " & MenueParam.TableRezeptRwert _
        & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID=" & ID & " AND RWERT_KWB=" & kwb
        CmdDys.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
          ier = -1
          Exit For
        End If
        SqlStmt = "INSERT INTO " & MenueParam.TableRezeptRwert & "([REZEPT_ID],[MISCH_ID],[RWERT_ID],[MESSGRW_ID],[RWERT_KWB])" _
        & " VALUES(" & ID & "," & MenueParam.MischID & "," & TypID(Rkwb) & "," & MenueParam.Messg.MessgRwID & "," & kwb & ")"
        CmdDys.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
          ier = -1
          Exit For
        End If
      End If
    Next i
    '
    '
    '
    '
    '

    For i = 0 To 1
      '
      'Speichern Nachstellung
      '
      '
      Rkwb = Rezepte.kwb(i)
      If SmpID(Rkwb) >= 0 Then
        kwb = 1 + i
        SqlStmt = "DELETE * FROM " & MenueParam.TableRezeptRwert _
        & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID=" & ID & " AND RWERT_KWB=" & kwb
        CmdDys.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
          ier = -1
          Exit For
        End If
        SqlStmt = "INSERT INTO " & MenueParam.TableRezeptRwert & "([REZEPT_ID],[MISCH_ID],[RWERT_ID],[MESSGRW_ID],[RWERT_KWB])" _
        & " VALUES(" & ID & "," & MenueParam.MischID & "," & SmpID(Rkwb) & "," & MenueParam.Messg.MessgRwID & "," & kwb & ")"
        CmdDys.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
          ier = -1
          Exit For
        End If
      End If
    Next i
    '
    '
    'Untergrund
    '
    '
    For i = 0 To 1
      If UntID(Rezepte.kwb(i)) >= 0 Then
        SqlStmt = "DELETE * FROM " & MenueParam.TableRezeptRwert _
        & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID=" & ID & " AND RWERT_KWB=" & -i - 1
        CmdDys.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
          ier = -1
          Exit For
        End If
        SqlStmt = "INSERT INTO " & MenueParam.TableRezeptRwert & "([REZEPT_ID],[MISCH_ID],[RWERT_ID],[MESSGRW_ID],[RWERT_KWB])" _
        & " VALUES(" & ID & "," & MenueParam.MischID & "," & UntID(Rezepte.kwb(i)) & "," & MenueParam.Messg.MessgRwID & "," & -i - 1 & ")"
        CmdDys.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
          ier = -1
          Exit For
        End If
      End If
    Next i

  End Sub

  Sub AddRezept(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef SmpID() As Integer, ByRef ier As Integer)
    ier = 0
    If ReadRezeptOnly(RezALL.Rezepte(RzNr).Gid) Then
      ier = 3014
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    '
    '
    'Einfügen
    '
    If Trim(RezALL.Rezepte(RzNr).Name) = "" Then
      ier = 3555
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    '
    ID = MaxDBTableID(MenueParam.TableRezept, "REZEPT_ID", {"MISCH_ID"}, {MenueParam.MischID}, Cndat)
    ID = ID + 1
    RezALL.Rezepte(RzNr).ID = ID
    If RezALL.Rezepte(RzNr).Bem = "" Then
      RezALL.Rezepte(RzNr).Bem = Space(1)
    End If
    SqlStmt = "INSERT INTO " & MenueParam.TableRezept _
    & "([REZEPT_ID],[MISCH_ID],[REZEPT_NAME],[REZEPT_BEM],[REZEPT_GID],[REZEPT_DATTIM],[USER_ID],[REZEPT_IARCH]," _
    & " [REZEPT_IVOL],[REZEPT_INF],[REZEPT_INM],[REZEPT_INO],[REZEPT_MNGMIN],[REZEPT_MNGMAX]," _
    & " [REZEPT_INP],[REZEPT_INQ],[REZEPT_PROZMIN],[REZEPT_PROZMAX],[REZEPT_DIK1],[REZEPT_DIK2],[REZEPT_GLZGRD])" _
    & " VALUES(" & ID & "," & MenueParam.MischID & ",'" & AddHkom((RezALL.Rezepte(RzNr).Name), NameLength) & "','" & AddHkom((RezALL.Rezepte(RzNr).Bem), BemLength) _
    & "'," & RezALL.Rezepte(RzNr).Gid & ",?," & MenueParam.UserID & "," & RezALL.Rezepte(RzNr).Iarch _
    & "," & RezALL.IVOL & "," & RezALL.INF & "," & RezALL.INM & "," & RezALL.INO & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr).UmMng * RezALL.MngMin)) _
    & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr).UmMng * RezALL.MngMax)) & "," & RezALL.INP & "," & RezALL.INQ _
    & "," & SQLpunkt(CStr(RezALL.ProzMin)) & "," & SQLpunkt(CStr(RezALL.ProzMax)) & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(0)))) & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(1)))) _
    & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr).GlzGrd)) & ")"
    CmdDys.CommandText = SqlStmt
    CmdDys.Parameters.Clear()
    CmdDys.Parameters.Add(New OleDbParameter("DATTIM", OleDbType.Date))
    CmdDys.Parameters("DATTIM").Value = Date.Now
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If
    '
    Call EinfFARezept(RzNr, ID, RezALL, ier)
    If ier > 0 Then Exit Sub
    '
    '
    '
    'Einfügen Rezept-Rwert_ID's
    '
    Call EinfRWRezept(ID, UntID, TypID, SmpID, RezALL.Rezepte, ier)
    If ier > 0 Then Exit Sub
    '
    '

  End Sub
  Sub DelRezept(ByRef ID As Integer, ByRef GID As Integer, ByRef ier As Integer)
    ier = 0
    If ReadRezeptOnly(GID) Then
      ier = 3014
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    '
    ' Löschen Rezept-Farb-/Bindemittel
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableRezeptFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If
    '
    '
    '
    '
    '
    '
    'Lösche Rezept-Rwert
    '
    '
    '
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableRezeptRwert _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If
    '
    'Löschen Rezept
    '
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableRezept _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If


  End Sub
  '
  Private Sub Update(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef ier As Integer)

    '
    RezALL.Rezepte(RzNr).ID = ID
    If RezALL.Rezepte(RzNr).Bem = "" Then
      RezALL.Rezepte(RzNr).Bem = Space(1)
    End If
    SqlStmt = "UPDATE " & MenueParam.TableRezept _
    & " SET [REZEPT_NAME]='" & AddHkom((RezALL.Rezepte(RzNr).Name), NameLength) _
    & "'," & "[REZEPT_BEM]='" & AddHkom((RezALL.Rezepte(RzNr).Bem), BemLength) _
    & "'," & "[REZEPT_GID]=" & RezALL.Rezepte(RzNr).Gid & "," & "[REZEPT_DATTIM]=?" _
    & "," & "[REZEPT_IARCH]=" & RezALL.Rezepte(RzNr).Iarch _
    & ",[REZEPT_IVOL]=" & RezALL.IVOL & "," & "[REZEPT_INF]=" & RezALL.INF & "," & "[REZEPT_INM]=" & RezALL.INM & "," & "[REZEPT_INO]=" & RezALL.INO _
    & "," & "[REZEPT_MNGMIN]=" & SQLpunkt(CStr(RezALL.Rezepte(RzNr).UmMng * RezALL.MngMin)) _
    & "," & "[REZEPT_MNGMAX]=" & SQLpunkt(CStr(RezALL.Rezepte(RzNr).UmMng * RezALL.MngMax)) _
    & "," & "[REZEPT_INP]=" & RezALL.INP & "," & "[REZEPT_INQ]=" & RezALL.INQ & "," & "[REZEPT_PROZMIN]=" & SQLpunkt(CStr(RezALL.ProzMin)) _
    & "," & "[REZEPT_PROZMAX]=" & SQLpunkt(CStr(RezALL.ProzMax)) _
    & "," & "[REZEPT_DIK1]=" & SQLpunkt(CStr(RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(0)))) & "," & "[REZEPT_DIK2]=" & SQLpunkt(CStr(RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(1)))) _
    & "," & "[REZEPT_GLZGRD]=" & SQLpunkt(CStr(RezALL.Rezepte(RzNr).GlzGrd)) & " WHERE MISCH_ID=" & MenueParam.MischID & " AND [REZEPT_ID]=" & ID & ";"
    CmdDys.CommandText = SqlStmt
    CmdDys.Parameters.Clear()
    CmdDys.Parameters.Add(New OleDbParameter("DATTIM", OleDbType.Date))
    CmdDys.Parameters("DATTIM").Value = Date.Now
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If
  End Sub
  Sub UpdateRezept(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef SmpID() As Integer, ByRef ier As Integer)
    ier = 0
    If ReadRezeptOnly(RezALL.Rezepte(RzNr).Gid) Then
      ier = 3014
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    '
    'Update
    '
    If Trim(RezALL.Rezepte(RzNr).Name) = "" Then
      ier = 3555
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    '
    '
    Call Update(RzNr, ID, RezALL, ier)
    If ier <> 0 Then
      Exit Sub
    End If

    '
    'Einfügen Farbmittel
    '
    Call UpdFarbmRezept(RzNr, ID, RezALL, ier)
    If ier > 0 Then Exit Sub
    '
    '
    '
    'Update R-Werte für Rezept
    '
    '
    Call UpdRwrtRezept(ID, UntID, TypID, SmpID, RezALL.Rezepte, ier)
    If ier > 0 Then Exit Sub

    '
    ''

  End Sub
  Private Sub UpdRwrtRezept(ByRef ID As Integer, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef SmpID() As Integer, ByRef Rezepte As Recipes, ByRef ier As Integer)
    '
    'Update R-Werte
    '
    '
    '
    '
    '
    'Lösche Rezept-Rwert
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableRezeptRwert _
    & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If
    '

    '
    '
    'Einfügen Rezept-Rwert_ID's
    '
    Call EinfRWRezept(ID, UntID, TypID, SmpID, Rezepte, ier)
    '

  End Sub
  Private Sub UpdFarbmRezept(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef ier As Integer)
    '
    'Update Farbmittelmengen
    '
    '
    '
    '
    '
    ' Löschen Farb-/Bindemittel
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableRezeptFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID = " & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If        '
    'Einfügen
    '
    Call EinfFARezept(RzNr, ID, RezALL, ier)
    If ier > 0 Then Exit Sub
    '

    'Worksp.CommitTrans
  End Sub
End Class