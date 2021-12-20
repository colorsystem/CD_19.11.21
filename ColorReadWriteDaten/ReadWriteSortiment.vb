Option Strict Off
Option Explicit On
Option Compare Text

Public Class ReadWriteSortiment
  Implements IDisposable
  Dim FaId As Integer
  Dim CmdDys As OleDbCommand
  Dim Dyset As OleDbDataReader
  Dim SqlStmt As String
  Dim KeyD As String
  Dim FaNr As String
  Dim ReWrFarbe As ReadWriteFarbe
  Dim ReWrGrund As ReadWriteGrund
  Dim NameLength As Integer
  Dim BemLength As Integer



  Sub New()
    CmdDys = New OleDbCommand
    ReWrFarbe = New ReadWriteFarbe
    ReWrGrund = New ReadWriteGrund
    NameLength = StringLength("TBL_SORTI", "SORTI_NAME", Cndat)
    BemLength = StringLength("TBL_SORTI", "SORTI_BEM", Cndat)

  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If Not IsNothing(CmdDys) Then
      CmdDys.Dispose()
    End If
    ReWrFarbe.dispose()
    ReWrGrund.dispose()

  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub ReadSortiName(ByRef SortiName As String, ByRef ID As Integer, ByRef ier As Integer)
    Dim CommBehav As CommandBehavior
    CommBehav = CommandBehavior.SingleRow
    '
    If Cndat.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If
    ier = 0
    ID = -1
    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableSorti _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND SORTI_NAME='" & AddHkom(SortiName, NameLength) & "'"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommBehav, Cndat)
    If Dyset.Read Then
      ID = Dyset("SORTI_ID")
    End If
    Dyset.Close()
  End Sub
  Function ContainsSortiID(ID As Integer) As Boolean
    ContainsSortiID = False
    SqlStmt = "SELECT * FROM " & MenueParam.TableSorti _
    & " WHERE [MISCH_ID]=" & MenueParam.MischID & " AND [SORTI_ID]=" & ID & ";"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow, Cndat)
    If Dyset.Read Then
      ContainsSortiID = True
    End If
    Dyset.Close()
  End Function
  Sub ReadSortim(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef ier As Integer)
    Dim i As Integer
    Dim WithMessg As String
    Dim CommBehav As CommandBehavior
    CommBehav = CommandBehavior.Default
    '
    If Cndat.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If
    ier = 0
    For i = 0 To 1
      UntID(i) = -1
      TypID(i) = -1
    Next i
    '
    '
    '
    'Vorhandene Farbmittel in Sortiment löschen
    '
    '
    RezALL.Rezepte(RzNr).clear()
    RezALL.Farben.clear()
    '
    '
    '
    '
    '
    'Neuaufbau
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableSorti _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND SORTI_ID=" & ID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommBehav, Cndat)
    '
    'Sortiment übernehmen
    '
    '
    '
    '
    '
    '
    '
    If Dyset.Read Then
      RezALL.IVOL = Dyset("sorti_ivol")
      RezALL.INM = Dyset("sorti_inm")
      RezALL.INF = Dyset("sorti_inf")
      RezALL.INO = Dyset("sorti_ino")
      RezALL.MngMin = Dyset("SORTI_mngmin")
      RezALL.MngMax = Dyset("SORTI_mngmax")
      RezALL.INP = Dyset("sorti_inp")
      RezALL.INQ = Dyset("sorti_inq")
      RezALL.ProzMin = Dyset("SORTI_prozmin")
      RezALL.ProzMax = Dyset("SORTI_prozmax")
      If RezALL.INP = 0 And RezALL.INQ = 0 Then
        RezALL.ProzMin = 0.0
        RezALL.ProzMax = 0.0
      End If
      RezALL.Rezepte(RzNr).ID = Dyset("SORTI_ID")
      RezALL.Rezepte(RzNr).Name = Dyset("Sorti_name")
      If Dyset("sorti_bem") = "" OrElse IsDBNull(Dyset("sorti_bem")) Then
        RezALL.Rezepte(RzNr).Bem = ""
      Else
        RezALL.Rezepte(RzNr).Bem = Dyset("sorti_bem")
      End If
      RezALL.Rezepte(RzNr).Iarch = 0
      RezALL.Rezepte(RzNr).DatTim = Dyset("sorti_DATTIM")
      RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(0)) = Dyset("sorti_dik1")
      RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(1)) = Dyset("sorti_dik2")
      RezALL.Rezepte(RzNr).GlzGrd = Dyset("sorti_glzgrd")
      RezALL.Rezepte(RzNr).RezMin = Dyset("sorti_rezmin")
      RezALL.Rezepte(RzNr).RezMax = Dyset("sorti_rezmax")
      RezALL.Rezepte(RzNr).UmMng = 1.0#
      Dyset.Close()
    Else
      ier = 4661
      Dyset.Close()
      Exit Sub
    End If
    Dyset.Close()

    '
    '
    'Farbmittel einlesen
    '
    '
   
    '
    'If BitWrt(27, MenueParam.User.Writ) Then
    WithMessg = "TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    ' Else
    ' WithMessg = "TBL_GRUND_FARBM.MESSG_ID=" & MenueParam.Messg.MessgID
    ' End If
    '
    '
    '
    SqlStmt = "SELECT DISTINCTROW *," & MenueParam.TableSortiFarbm & ".FARBM_ID AS FARBM_ID," & MenueParam.TableGrundFarbm _
    & ".GKWRT_ID" & " FROM " & MenueParam.TableSortiFarbm & " INNER JOIN " & MenueParam.TableGrundFarbm _
    & " ON (" & MenueParam.TableSortiFarbm & ".FARBM_ID = " & MenueParam.TableGrundFarbm & ".FARBM_ID)" _
    & " AND (" & MenueParam.TableSortiFarbm & ".MISCH_ID = " & MenueParam.TableGrundFarbm & ".MISCH_ID)" _
    & " WHERE ((" & MenueParam.TableSortiFarbm & ".MISCH_ID=" & MenueParam.MischID & ") AND (" & MenueParam.TableSortiFarbm & ".SORTI_ID=" & ID _
    & ") AND (" & MenueParam.TableGrundFarbm & ".GKWRT_ID=" & MenueParam.Misch.GKwrtID & ") AND (" & WithMessg & "))" _
    & " ORDER BY FARBM_IPOS;"
    '
    '
    '
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommBehav, Cndat)

    i = 0
    Do While Dyset.Read
      FaNr = KeyRe(i)
      '
      '   Menge für Sortiment
      '

      RezALL.Rezepte(RzNr).AddFaNr(FaNr, New ColorAmount)
      RezALL.Rezepte(RzNr)(FaNr).ID = Dyset("farbm_id")
      RezALL.Rezepte(RzNr)(FaNr).FaAmng = Dyset("farbm_menge")
      RezALL.Rezepte(RzNr)(FaNr).Proz = Dyset("FARBM_PROZ")
      RezALL.Rezepte(RzNr)(FaNr).Prob = Dyset("FARBM_PROB")
      '
      '   Farbmittel
      '

      i = i + 1
    Loop
    Dyset.Close()


    '
    '
    '
    'R-Wert_ID einlesen
    '
    '

    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableSortiRwert _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND SORTI_ID=" & ID
    '
    '
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommBehav, Cndat)

    Do While Dyset.Read
      Select Case Dyset("rwert_kwb")
        Case -1
          '
          '
          'Untergrund (weiß)
          '
          '
          UntID(RezALL.Rezepte.kwb(0)) = Dyset("Rwert_ID")
        Case -2
          '
          '
          'Untergrund (schwarz)
          '
          '
          UntID(RezALL.Rezepte.kwb(1)) = Dyset("Rwert_ID")
        Case 11
          '
          '
          'Vorlage (weiß)
          '
          '
          TypID(RezALL.Rezepte.kwb(0)) = Dyset("Rwert_ID")
        Case 12
          '
          '
          'Vorlage (schwarz)
          '
          '
          TypID(RezALL.Rezepte.kwb(1)) = Dyset("Rwert_ID")
      End Select
    Loop
    Dyset.Close()
  End Sub
  Sub ReadSortimFarbGrund(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef ier As Integer)
    Dim j As Integer
    Dim WithMessg As String
    Dim CommBehav As CommandBehavior
    CommBehav = CommandBehavior.Default
    '
    If Cndat.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If
    ier = 0

    Call ReadSortim(RzNr, ID, RezALL, UntID, TypID, ier)
    '
    '
    '
    '
    'Farbmittel einlesen
    '
    '
    '
   
    'If BitWrt(27, MenueParam.User.Writ) Then
    WithMessg = "TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    ' Else
    'WithMessg = "TBL_GRUND_FARBM.MESSG_ID=" & MenueParam.Messg.MessgID
    'End If
    '
    SqlStmt = "SELECT DISTINCTROW *," & MenueParam.TableSortiFarbm & ".FARBM_ID AS FARBM_ID," & MenueParam.TableGrundFarbm _
& ".GKWRT_ID" & " FROM " & MenueParam.TableSortiFarbm & " INNER JOIN " & MenueParam.TableGrundFarbm _
& " ON (" & MenueParam.TableSortiFarbm & ".FARBM_ID = " & MenueParam.TableGrundFarbm & ".FARBM_ID)" _
& " AND (" & MenueParam.TableSortiFarbm & ".MISCH_ID = " & MenueParam.TableGrundFarbm & ".MISCH_ID)" _
& " WHERE ((" & MenueParam.TableSortiFarbm & ".MISCH_ID=" & MenueParam.MischID & ") AND (" & MenueParam.TableSortiFarbm & ".SORTI_ID=" & ID _
& ") AND (" & MenueParam.TableGrundFarbm & ".GKWRT_ID=" & MenueParam.Misch.GKwrtID & ") AND (" & WithMessg & "))" _
& " ORDER BY FARBM_IPOS;"



    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommBehav, Cndat)
    Do While Dyset.Read
      '
      '   Farbmittel
      '
      FaId = Dyset("farbm_id")
      KeyD = KeyName(FaId)
      '
      '
      '   Prüfen, ob Farbmittel bereits vorhanden
      '
      For j = 0 To RezALL.Farben.FarbCount - 1
        If RezALL.Farben(j).ID = FaId Then Exit For
      Next j
      If j > RezALL.Farben.FarbCount - 1 Then
        '
        '
        '     Neues Farbmittel hinzufügen
        '
        RezALL.Farben.AddFarb(KeyD, New Colorant)

        '
        '
        RezALL.Farben(KeyD).ID = FaId
        RezALL.Farben(KeyD).BuMng = 0.0#
        RezALL.Farben(KeyD).BoMng = Dyset("farbm_limmng")
        RezALL.Farben(KeyD).OP = Space(1)
        If Not IsDBNull(Dyset("farbm_operat")) Then
          RezALL.Farben(KeyD).OP = UCase(Dyset("farbm_operat"))
        End If
        RezALL.Farben(KeyD).Kto = Dyset("Farbm_Topf")
        RezALL.Farben(KeyD).Preis = Dyset("farbm_preis")


        '
        '
        '
      End If

    Loop
    For j = 0 To RezALL.Farben.FarbCount - 1
      '
      'von Tabelle Farben einlesen
      '
      FaId = RezALL.Farben(j).ID
      KeyD = KeyName(FaId)
      If RezALL.Farben(KeyD).PrbCount = 0 Or RezALL.Farben(KeyD).PreCount = 0 Or RezALL.Farben(KeyD).PrfCount = 0 Then
        '
        Call ReWrFarbe.FarReaGrund(FaId, RezALL.Farben(KeyD), True, ier)
      End If
      ''
    Next j
    Dyset.Close()
  End Sub
  Private Sub EinfFASortim(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef ier As Integer)
    Dim i As Integer
    Dim LiMenge As Single
    ier = 0
    If ID < 0 Then
      ier = 3553
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
      SqlStmt = "INSERT INTO " & MenueParam.TableSortiFarbm & "([SORTI_ID],[MISCH_ID],[FARBM_ID],[FARBM_MENGE],[FARBM_LIMMNG],[FARBM_OPERAT],[FARBM_TOPF]," _
      & "[FARBM_PREIS],[FARBM_PROZ],[FARBM_PROB],[FARBM_IPOS])" _
      & " VALUES(" & ID & "," & MenueParam.MischID & "," & FaId & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr)(i).FaAmng)) & "," & SQLpunkt(CStr(LiMenge)) _
      & ",'" & RezALL.Farben(KeyD).OP & "','" & RezALL.Farben(KeyD).Kto & "'," & SQLpunkt(CStr(RezALL.Farben(KeyD).Preis)) _
      & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr)(i).Proz)) & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr)(i).Prob)) & "," & CByte(i) & ")"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit For
      End If
    Next i

  End Sub

  Private Sub EinfRWSortim(ByRef ID As Integer, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef Rezepte As Recipes, ByRef ier As Integer)
    Dim i As Integer
    ier = 0
    If ID < 0 Then
      ier = 3553
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    For i = 0 To 1
      '
      'Speichern Vorlage
      '
      '
      If TypID(Rezepte.kwb(i)) >= 0 Then
        SqlStmt = "INSERT INTO " & MenueParam.TableSortiRwert & "([SORTI_ID],[MISCH_ID],[RWERT_ID],[MESSGRW_ID],[RWERT_KWB])" _
        & " VALUES(" & ID & "," & MenueParam.MischID & "," & TypID(Rezepte.kwb(i)) & "," & MenueParam.Messg.MessgRwID & "," & 11 + i & ")"
        CmdDys.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
          ier = -1
          Exit Sub
        End If
      End If
      '
      'Untergrund
      '
      '
      If UntID(Rezepte.kwb(i)) >= 0 Then
        SqlStmt = "INSERT INTO " & MenueParam.TableSortiRwert & "([SORTI_ID],[MISCH_ID],[RWERT_ID],[MESSGRW_ID],[RWERT_KWB])" _
        & " VALUES(" & ID & "," & MenueParam.MischID & "," & UntID(Rezepte.kwb(i)) & "," & MenueParam.Messg.MessgRwID & "," & -i - 1 & ")"
        CmdDys.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
          ier = -1
          Exit Sub
        End If
      End If
    Next i

  End Sub

  Sub AddSortim(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef ier As Integer)
    ier = 0
    '
    If Trim(RezALL.Rezepte(RzNr).Name) = "" Then
      ier = 3556
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    '
    'Einfügen
    '
    '
    ID = MaxDBTableID((MenueParam.TableSorti), "SORTI_ID", {"MISCH_ID"}, {MenueParam.MischID}, Cndat)
    ID = ID + 1
    RezALL.Rezepte(RzNr).ID = ID
    If RezALL.Rezepte(RzNr).Name = "" Then
      RezALL.Rezepte(RzNr).Name = Space(1)
    End If
    If RezALL.Rezepte(RzNr).Bem = "" Then
      RezALL.Rezepte(RzNr).Bem = Space(1)
    End If
    SqlStmt = "INSERT INTO " & MenueParam.TableSorti _
    & "([SORTI_ID],[MISCH_ID],[SORTI_NAME],[SORTI_BEM],[SORTI_DATTIM],[USER_ID]," _
    & " [SORTI_IVOL],[SORTI_INF],[SORTI_INM],[SORTI_INO],[SORTI_MNGMIN],[SORTI_MNGMAX]," _
    & " [SORTI_INP],[SORTI_INQ],[SORTI_PROZMIN],[SORTI_PROZMAX],[SORTI_DIK1],[SORTI_DIK2],[SORTI_GLZGRD],[SORTI_REZMIN],[SORTI_REZMAX])" _
    & " VALUES(" & ID & "," & MenueParam.MischID & ",'" & AddHkom((RezALL.Rezepte(RzNr).Name), NameLength) & "','" _
    & AddHkom((RezALL.Rezepte(RzNr).Bem), BemLength) & "'," & Sqldati(Date.Now) _
    & "," & MenueParam.UserID & "," & RezALL.IVOL & "," & RezALL.INF & "," & RezALL.INM _
    & "," & RezALL.INO & "," & SQLpunkt(CStr(RezALL.MngMin)) & "," & SQLpunkt(CStr(RezALL.MngMax)) & "," & RezALL.INP & "," & RezALL.INQ _
    & "," & SQLpunkt(CStr(RezALL.ProzMin)) & "," & SQLpunkt(CStr(RezALL.ProzMax)) _
    & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(0)))) & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(1)))) _
    & "," & SQLpunkt(CStr(RezALL.Rezepte(RzNr).GlzGrd)) & "," & RezALL.Rezepte(RzNr).RezMin & "," & RezALL.Rezepte(RzNr).RezMax & ")"
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If
    '
    '
    'Einfügen Sortiment-Farbmittel_ID's
    Call EinfFASortim(RzNr, ID, RezALL, ier)
    If ier > 0 Then Exit Sub
    '
    '
    '
    'Einfügen Sortiment-Rwert_ID's
    '
    Call EinfRWSortim(ID, UntID, TypID, RezALL.Rezepte, ier)
    '
    '

  End Sub
  Sub DelSortim(ByRef ID As Integer, ByRef ier As Integer)
    '
    ' Löschen Sortiment-Farb-/Bindemittel
    '
    'Worksp.BeginTrans
    SqlStmt = "DELETE * FROM " & MenueParam.TableSortiFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND SORTI_ID=" & ID
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
    'Lösche Sortiment-Rwert (für alle Meßgeräte)
    '
    '
    '
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableSortiRwert _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND SORTI_ID=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If

    '
    'Löschen Sortiment
    '
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableSorti _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND SORTI_ID=" & ID
    '
    '
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If

  End Sub
  '
  Sub UpdSortim(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef ier As Integer)

    '
    'Update
    '
    If Trim(RezALL.Rezepte(RzNr).Name) = "" Then
      ier = 3556
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    '
    '
    RezALL.Rezepte(RzNr).ID = ID
    If RezALL.Rezepte(RzNr).Bem = "" Then
      RezALL.Rezepte(RzNr).Bem = Space(1)
    End If
    SqlStmt = "UPDATE " & MenueParam.TableSorti & " SET [SORTI_NAME]='" & AddHkom((RezALL.Rezepte(RzNr).Name), NameLength) _
    & "'," & "[SORTI_BEM]='" & AddHkom((RezALL.Rezepte(RzNr).Bem), BemLength) & "'," & "[SORTI_DATTIM]=" & Sqldati(Date.Now) _
    & "," & "[SORTI_IVOL]=" & RezALL.IVOL & "," & "[SORTI_INF]=" & RezALL.INF & "," & "[SORTI_INM]=" & RezALL.INM & "," & "[SORTI_INO]=" & RezALL.INO _
    & "," & "[SORTI_MNGMIN]=" & SQLpunkt(CStr(RezALL.MngMin)) & "," & "[SORTI_MNGMAX]=" & SQLpunkt(CStr(RezALL.MngMax)) _
    & "," & "[SORTI_INP]=" & RezALL.INP & "," & "[SORTI_INQ]=" & RezALL.INQ & "," & "[SORTI_PROZMIN]=" & SQLpunkt(CStr(RezALL.ProzMin)) _
    & "," & "[SORTI_PROZMAX]=" & SQLpunkt(CStr(RezALL.ProzMax)) _
    & "," & "[SORTI_DIK1]=" & SQLpunkt(CStr(RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(0)))) & "," & "[SORTI_DIK2]=" & SQLpunkt(CStr(RezALL.Rezepte(RzNr).Dicke(RezALL.Rezepte.kwb(1)))) _
    & "," & "[SORTI_GLZGRD]=" & SQLpunkt(CStr(RezALL.Rezepte(RzNr).GlzGrd)) & "," & "[SORTI_REZMIN]=" & RezALL.Rezepte(RzNr).RezMin & "," & "[SORTI_REZMAX]=" & RezALL.Rezepte(RzNr).RezMax _
    & " WHERE [MISCH_ID]=" & MenueParam.MischID & " AND [SORTI_ID]=" & ID & ";"
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If
    '

    '
    'Einfügen
    '
    Call UpdFarbmSortim(RzNr, ID, RezALL, ier)
    '
    '
    '
    'Update R-Werte für Rezept
    '
    '
    Call UpdRwrtSortim(ID, UntID, TypID, RezALL.Rezepte, ier)

    '
    '

  End Sub
  Private Sub UpdRwrtSortim(ByRef ID As Integer, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef Rezepte As Recipes, ByRef ier As Integer)
    '
    'Update R-Werte
    '
    '
    '
    '
    'Lösche Sortiment-Rwert
    '
    '

    SqlStmt = "DELETE * FROM " & MenueParam.TableSortiRwert _
    & " WHERE MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND MISCH_ID=" & MenueParam.MischID & " AND SORTI_ID=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If

    '
    '
    'Einfügen Sortiment-Rwert_ID's
    '
    Call EinfRWSortim(ID, UntID, TypID, Rezepte, ier)
    '

  End Sub
  Private Sub UpdFarbmSortim(ByRef RzNr As String, ByRef ID As Integer, ByRef RezALL As RecipesGrp, ByRef ier As Integer)
    '
    'Update Farbmittelmengen
    '
    '
    '
    '
    '
    ' Löschen Farb-/Bindemittel
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableSortiFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND SORTI_ID=" & ID
    CmdDys.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If
    '
    'Einfügen
    '
    Call EinfFASortim(RzNr, ID, RezALL, ier)
    '

    'Worksp.CommitTrans
  End Sub








End Class