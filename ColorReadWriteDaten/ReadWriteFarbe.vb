Option Strict Off
Option Explicit On
Option Compare Text


Public Class ReadWriteFarbe
  Implements IDisposable
  Dim Ier As Integer
  Dim imsg As Integer
  Public IARCH As Short
  Dim MaxmID As Integer
  Dim NameLength As Integer
  Dim BemLength As Integer
  Dim AnameLength As Integer
  Dim PrNrLength As Integer
  Dim Daset As OleDbDataReader
  Dim Dyset As OleDbDataReader
  Dim CmdDas As OleDbCommand
  Dim CmdDys As OleDbCommand
  Dim SqlStmt As String
  Dim ReWrGrund As ReadWriteGrund



  Sub New()
    ReWrGrund = New ReadWriteGrund
    CmdDas = New OleDbCommand
    CmdDys = New OleDbCommand
    NameLength = StringLength("TBL_FARBM", "FARBM_NAME", Cndat)
    AnameLength = StringLength("TBL_FARBM", "FARBM_ANAME", Cndat)
    BemLength = StringLength("TBL_FARBM", "FARBM_BEM", Cndat)
    PrNrLength = StringLength("TBL_FARBM", "FARBM_PRNR", Cndat)
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If Not IsNothing(CmdDas) Then
      CmdDas.Dispose()
    End If
    If Not IsNothing(CmdDys) Then
      CmdDys.Dispose()
    End If
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub

  Sub ReadFarbmName(ByRef FarbmName As String, ByRef ID As Integer, ByRef ier As Integer)
    Dim CommBehav As CommandBehavior
    ier = 0
    ID = -1
    '
    '
    CommBehav = CommandBehavior.SingleRow
    '
    If Cndat.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If ''
    SqlStmt = "SELECT * FROM " & MenueParam.TableFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_NAME='" & AddHkom(FarbmName, NameLength) & "'"
    CmdDas.CommandText = SqlStmt
    Daset = DataReader(CmdDas, CommBehav, Cndat)
    If Daset.Read Then
      ID = Daset("FARBM_ID")
    End If
    Daset.Close()
  End Sub
  Sub FarReaGrund(ByRef ID As Integer, ByRef ArbFarb As Colorant, ByRef JSmng As Boolean, ByRef ier As Integer)
    ier = 0
    '
    Call FarRea(ID, ArbFarb, JSmng, ier)
    If ier <> 0 Then
      Exit Sub
    End If
    '
    '
    'Optische Daten einlesen
    '
    '
    '
    Call ReWrGrund.ReadGrund(ID, ArbFarb.OptData, ier)
    '
    '
    ''
  End Sub
  Sub FarRea(ByRef ID As Integer, ByRef ArbFarb As Colorant, ByRef JSmng As Boolean, ByRef ier As Integer)
    Dim i As Integer
    Dim CommBehav As CommandBehavior
    'Einlesen
    '
    ier = 0
    '
    CommBehav = CommandBehavior.SingleRow
    '
    If Cndat.State = 0 Then
      CommBehav = CommBehav Or CommandBehavior.CloseConnection
    End If '
    SqlStmt = "SELECT * FROM " & MenueParam.TableFarbm _
     & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & ID
    '
    CmdDas.CommandText = SqlStmt
    Daset = DataReader(CmdDas, CommBehav, Cndat)
    '
    If Daset.Read Then
      ArbFarb.ID = Daset("farbm_id")
      ArbFarb.Name = Daset("farbm_Name")
      If Daset("farbm_bem") = "" Or IsDBNull(Daset("farbm_bem")) Then
        ArbFarb.Bem = ""
      Else
        ArbFarb.Bem = Daset("farbm_bem")
      End If
      If Daset("farbm_Aname") = "" OrElse IsDBNull(Daset("farbm_Aname")) Then
        ArbFarb.Aname = ""
      Else
        ArbFarb.Aname = Daset("farbm_Aname")
      End If
      If Daset("farbm_PrNr") = "" OrElse IsDBNull(Daset("farbm_PrNr")) Then
        ArbFarb.PrNr = ""
      Else
        ArbFarb.PrNr = Daset("farbm_PrNr")
      End If
      ArbFarb.Ichf = Daset("Farbm_ichf")
      ArbFarb.Ibas = Daset("Farbm_ibas")
      ArbFarb.Spz = Daset("Farbm_spz")
      ArbFarb.Eff = Daset("Farbm_eff")
      ArbFarb.Fst = Daset("FARBM_FST")
      ArbFarb.Bel = Daset("FARBM_BEL")
      ArbFarb.GlzGrd = Daset("FARBM_GLZGRD")
      ArbFarb.GlzGrdID = Daset("GLZGRD_ID")
      If JSmng Then
        ArbFarb.Smenge = Daset("Farbm_smenge")
      Else
        ArbFarb.Smenge = 0.0#
      End If
      ArbFarb.Form = Daset("Farbm_format")
      ArbFarb.FarbID = Daset("Farbm_FarbID")
    Else
      ier = 4063
      Daset.Close()
      Exit Sub
    End If
    '
    '
    '
    'Erster Farb-/Bindemittelpreis
    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableFarbmPreis _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & ArbFarb.ID & " ORDER BY FARBM_IRPA"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.Default, Cndat)
    'dyset = DbUse.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset, DAO.RecordsetOptionEnum.dbConsistent, DAO.RecordsetOptionEnum.dbReadOnly)
    '
    '
    'Alle Farbmittelpreise übernehmen
    '
    '
    '
    i = 0
    Do While Dyset.Read
      ArbFarb.Pre(i) = Dyset("farbm_preis")
      i = i + 1
    Loop
    If ArbFarb.PreCount = 0 Then
      ArbFarb.Pre(0) = 0.0
    End If
    Dyset.Close()
    '
    '
    '
    'Alle Farbmittelprozentigkeiten übernehmen
    '
    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableFarbmProz _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & ArbFarb.ID & " ORDER BY FARBM_IRFA"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.Default, Cndat)
    'dyset = DbUse.OpenRecordset(SqlStmt, DAO.RecordsetTypeEnum.dbOpenDynaset, DAO.RecordsetOptionEnum.dbConsistent, DAO.RecordsetOptionEnum.dbReadOnly)
    i = 0
    Do While Dyset.Read
      ArbFarb.Prf(i) = Dyset("FARBM_PROZ")
      i = i + 1
    Loop
    If ArbFarb.PrfCount = 0 Then
      ArbFarb.Prf(0) = 100.0
    End If

    Dyset.Close()
    '
    '
    'Alle Bindemittelprozentigkeiten übernehmen
    '
    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableFarbmProb _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & ArbFarb.ID & " ORDER BY FARBM_IRBA"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.Default, Cndat)

    i = 0
    Do While Dyset.Read
      ArbFarb.Prb(i) = Dyset("FARBM_PROB")
      i = i + 1
    Loop
    If ArbFarb.PrbCount = 0 Then
      ArbFarb.Prb(0) = 100.0
    End If
    Dyset.Close()
    Daset.Close()
  End Sub
  Sub ReadFarbmPrNr(ByRef FarbmPrNr As String, ByRef ID As Integer, ByRef ier As Integer)
    ier = 0
    ID = -1
    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_PRNR='" & AddHkom(FarbmPrNr, PrNrLength) & "'"
    CmdDas.CommandText = SqlStmt
    Daset = DataReader(CmdDas, CommandBehavior.CloseConnection Or CommandBehavior.SingleRow, Cndat)
    If Daset.Read Then
      ID = Daset("FARBM_ID")
    End If
    Daset.Close()
  End Sub

  Sub FarAdd(ByRef ArbFarb As Colorant, ByRef ier As Integer)
    ier = 0
    '


    MaxmID = MaxDBTableID(MenueParam.TableFarbm, "FARBM_ID", {"MISCH_ID"}, {MenueParam.MischID}, Cndat) + 1
    ArbFarb.ID = MaxmID
    If ArbFarb.GlzGrdID = -1 Then
      ArbFarb.GlzGrdID = MaxmID
    End If
    Call FarWri(ArbFarb, ier)

    '
    ' 
    ''
  End Sub
  Sub FarWri(ByRef ArbFarb As Colorant, ByRef ier As Integer)
    ier = 0
    '
    '
    'Hinzufügen
    '
    If Trim(ArbFarb.Name) = "" Then
      MsgBox(Texxt(3554))
      ier = 3554
      Exit Sub
    End If
    '
    'Worksp.BeginTrans
    SqlStmt = "INSERT INTO " & MenueParam.TableFarbm & "([FARBM_ID],[MISCH_ID],[FARBM_NAME],[FARBM_BEM],[FARBM_ANAME],[FARBM_PRNR],[FARBM_DATTIM],[FARBM_ICHF]," _
    & " [FARBM_IBAS],[FARBM_SPZ],[FARBM_EFF],[FARBM_FST],[FARBM_BEL],[FARBM_SMENGE],[FARBM_FORMAT],[FARBM_FARBID],[FARBM_GLZGRD],[GLZGRD_ID])" _
    & " VALUES(" & ArbFarb.ID & "," & MenueParam.MischID & ",'" & AddHkom(ArbFarb.Name, NameLength) & "','" & AddHkom(ArbFarb.Bem, BemLength) & "','" & AddHkom(ArbFarb.Aname, AnameLength) & "','" & AddHkom(ArbFarb.PrNr, PrNrLength) & "'," _
    & Sqldati(Date.Now) & "," & ArbFarb.Ichf & "," & ArbFarb.Ibas & "," & SQLpunkt(CStr(ArbFarb.Spz)) _
    & "," & SQLpunkt(CStr(ArbFarb.Eff)) & "," & SQLpunkt(CStr(ArbFarb.Fst)) & "," _
    & SQLpunkt(CStr(ArbFarb.Bel)) & "," & SQLpunkt(CStr(ArbFarb.Smenge)) & ",'" & ArbFarb.Form & "'," & ArbFarb.FarbID & "," & SQLpunkt(CStr(ArbFarb.GlzGrd)) & "," & ArbFarb.GlzGrdID & ")"
    CmdDas.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    'Einfügen Preis,Prozentigkeit Bindemittel-Prozentigkeit
    '
    '
    '
    '
    Call FillPrPrBi(ArbFarb, ier)
    '
    ' 
    ''
  End Sub
  Sub FarDel(ByRef ArbFarb As Colorant, ByRef Meld As Boolean, ByRef ier As Integer)
    Dim RecCount As Integer
    Dim StrRez As String
    Dim AdaptTemp As OleDbDataAdapter
    Dim TabTemp As DataTable
    Dim CmdTemp As OleDbCommand
    CmdTemp = New OleDbCommand("", Cndat)
    AdaptTemp = New OleDbDataAdapter
    AdaptTemp.SelectCommand = CmdTemp
    '
    '
    '
    '
    'Hilftabelle für Rezeptsuche (Zwischenablage)
    '
    '
    ier = 0
    
    '
    'Prüfen, ob Farb-/Bindemittel in Sortiment vorhanden
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableSortiFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID= " & ArbFarb.ID
    '
    CmdTemp.CommandText = SqlStmt
    TabTemp = New DataTable
    If Not FillDatset(AdaptTemp, TabTemp) Then
      Exit Sub
    End If
    '
    '
    RecCount = TabTemp.Rows.Count
    If RecCount > 0 Then
      imsg = 0
      If Meld Then
        imsg = MsgBox(CSng(RecCount) & Texxt(2620) & ArbFarb.Name & Texxt(2621), 4, Texxt(2000))
      End If
      If imsg = 7 Then
        Exit Sub
      Else
        SqlStmt = "DELETE * FROM " & MenueParam.TableSortiFarbm _
        & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & ArbFarb.ID
        CmdDas.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
          Exit Sub
        End If
      End If
    End If


    '
    'Prüfen, ob Farb-/Bindemittel in Rezept vorhanden
    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableRezeptFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID= " & ArbFarb.ID
    '
    CmdTemp.CommandText = SqlStmt
    TabTemp = New DataTable
    If Not FillDatset(AdaptTemp, TabTemp) Then
      Exit Sub
    End If
    StrRez = StrLin(TabTemp, "REZEPT_ID")
    '
    RecCount = TabTemp.Rows.Count
    If RecCount > 0 Then
      imsg = 0
      If Meld Then
        imsg = MsgBox(CSng(RecCount) & Texxt(2623) & ArbFarb.Name & Texxt(2624), 4, Texxt(2000))
      End If
      If imsg = 7 Then
        Exit Sub
      Else
        '
        '
        '
        '
        '
        'FARB-/Bindemittel in TBL_REZEPT_FARBM löschen
        '
        '
        '
        SqlStmt = "DELETE * FROM " & MenueParam.TableRezeptFarbm _
        & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN " & StrRez
        CmdDas.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
          Exit Sub
        End If

        '
        '
        'Rezepte löschen in TBL_REZEPT_RWERT
        '
        '
        '
        SqlStmt = "DELETE * FROM " & MenueParam.TableRezeptRwert _
        & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN " & StrRez
        'MsgBox SqlStmt
        CmdDas.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
          Exit Sub
        End If

        '
        'Rezepte löschen in TBL_REZEPT
        '
        '
        '
        SqlStmt = "DELETE * FROM " & MenueParam.TableRezept _
        & " WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN " & StrRez
        CmdDas.CommandText = SqlStmt
        If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
          Exit Sub
        End If

      End If
    End If
    '
    '
    'Prüfen, ob Farb-/Bindemittel in Grunddaten vorhanden und Grunddaten löschen
    '
    '
    SqlStmt = "SELECT * FROM " & MenueParam.TableGrundFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID= " & ArbFarb.ID
    '
    CmdTemp.CommandText = SqlStmt
    TabTemp = New DataTable
    If Not FillDatset(AdaptTemp, TabTemp) Then
      Exit Sub
    End If


   
    If TabTemp.Rows.Count > 0 Then
      SqlStmt = "DELETE * FROM " & MenueParam.TableGrundFarbm _
      & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & ArbFarb.ID
      CmdDas.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
        Exit Sub
      End If
    End If

    '
    'Lösche Preis,Prozentigkeit und Bindemittelprozentigkeit
    '
    '
    Call DelPrPrBi(ArbFarb.ID, ier)
    '
    'Löschen Farbmittel
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableFarbm _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & ArbFarb.ID
    CmdDas.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
      Exit Sub
    End If
    '
    '
    AdaptTemp.Dispose()
    TabTemp.Dispose()
    CmdTemp.Dispose()


    '
   
    ''
  End Sub
  '
  Function ContainsFarbID(ID As Integer) As Boolean
    ContainsFarbID = False
    SqlStmt = "SELECT * FROM " & MenueParam.TableFarbm _
    & " WHERE [MISCH_ID]=" & MenueParam.MischID & " AND [FARBM_ID]=" & ID & ";"
    CmdDas.CommandText = SqlStmt
    Daset = DataReader(CmdDas, CommandBehavior.SingleRow, Cndat)
    If Daset.Read Then
      ContainsFarbID = True
    End If
    Daset.Close()
  End Function
  Sub FarUpd(ByRef ArbFarb As Colorant, ByRef ier As Integer)
    '
    'Update
    '
    ier = 0
    If Trim(ArbFarb.Name) = "" Then
      MsgBox(Texxt(3554))
      ier = 3554
      Exit Sub
    End If
    '


    If ContainsFarbID(ArbFarb.ID) Then

      SqlStmt = "UPDATE " & MenueParam.TableFarbm & " SET [FARBM_NAME]='" & AddHkom(ArbFarb.Name, NameLength) & "'," & "[FARBM_BEM]='" _
      & AddHkom(ArbFarb.Bem, BemLength) & "'," & "[FARBM_ANAME]='" & AddHkom(ArbFarb.Aname, AnameLength) & "'," & "[FARBM_PRNR]='" & AddHkom(ArbFarb.PrNr, PrNrLength) _
      & "'," & "[FARBM_DATTIM]=" & Sqldati(Date.Now) _
      & "," & "[FARBM_ICHF]=" & ArbFarb.Ichf & "," & "[FARBM_IBAS]=" & ArbFarb.Ibas & "," & "[FARBM_SPZ]=" & SQLpunkt(CStr(ArbFarb.Spz)) _
      & "," & "[FARBM_EFF]=" & SQLpunkt(CStr(ArbFarb.Eff)) & "," & "[FARBM_FST]=" & SQLpunkt(CStr(ArbFarb.Fst)) _
      & "," & "[FARBM_BEL]=" & SQLpunkt(CStr(ArbFarb.Bel)) & "," & "[FARBM_SMENGE]=" & SQLpunkt(CStr(ArbFarb.Smenge)) _
      & "," & "[FARBM_FORMAT]='" & ArbFarb.Form & "'," & "[FARBM_FARBID]=" & ArbFarb.FarbID & "," & "[FARBM_GLZGRD]=" & SQLpunkt(CStr(ArbFarb.GlzGrd)) & "," & "[GLZGRD_ID]=" & ArbFarb.GlzGrdID _
      & " WHERE [MISCH_ID]=" & MenueParam.MischID & " AND [FARBM_ID]=" & ArbFarb.ID & ";"
      CmdDas.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
        Exit Sub
      End If
    Else
      '
      '
      'Datensatz nicht gefunden
      '
      '
      '
      Daset.Close()
      Exit Sub
    End If

    '
    'Löschen Preis,Prozentigkeit und Bindemittelproz.
    '
    Call DelPrPrBi(ArbFarb.ID, ier)
    If ier > 0 Then Exit Sub
    '
    '
    'Schreiben Preis,Prozentigkeit und Bindemittelproz.
    '
    '
    '
    Call FillPrPrBi(ArbFarb, ier)
    If ier > 0 Then Exit Sub
  End Sub
  Private Sub DelPrPrBi(ByRef FaId As Integer, ByRef ier As Integer)
    '
    ier = 0
    '
    '
    '
    'Löschen Preis
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableFarbmPreis _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & FaId
    CmdDas.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If
    '
    '
    '
    'Löschen Prozentigkeit
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableFarbmProz _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & FaId
    CmdDas.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If
    '
    '
    '
    'Löschen Bindemittel-Prozentigkeit
    '
    '
    SqlStmt = "DELETE * FROM " & MenueParam.TableFarbmProb _
    & " WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & FaId
    CmdDas.CommandText = SqlStmt
    If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
      ier = -1
      Exit Sub
    End If

  End Sub
  Private Sub FillPrPrBi(ByRef ArbFarb As Colorant, ByRef ier As Short)
    Dim i As Short
    Dim j As Short
    '
    ier = 0
    '
    '
    '
    'Preise einfügen
    '
    '
    '
    SqlStmt = "INSERT INTO " & MenueParam.TableFarbmPreis & " (FARBM_ID,MISCH_ID,FARBM_IRPA,FARBM_PREIS)" & " VALUES(?,?,?,?)"
    CmdDas.CommandText = SqlStmt
    CmdDas.Parameters.Clear()
    CmdDas.Parameters.Add(New OleDbParameter("FARBM_ID", OleDbType.Integer))
    CmdDas.Parameters.Add(New OleDbParameter("MISCH_ID", OleDbType.Integer))
    CmdDas.Parameters.Add(New OleDbParameter("FARB_IRPA", OleDbType.SmallInt))
    CmdDas.Parameters.Add(New OleDbParameter("FARB_PREIS", OleDbType.Single))

    j = 0
    For i = 0 To ArbFarb.PreCount - 1
      If ArbFarb.Pre(i) >= 0 Then
        CmdDas.Parameters(0).Value = ArbFarb.ID
        CmdDas.Parameters(1).Value = MenueParam.MischID
        CmdDas.Parameters(2).Value = j
        CmdDas.Parameters(3).Value = ArbFarb.Pre(i)
        If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
          ier = -1
          Exit Sub
        End If
        j = j + 1
      End If
    Next i
    '
    '
    '
    '
    'Prozentigkeiten einfügen
    '
    '
    '
    SqlStmt = "INSERT INTO " & MenueParam.TableFarbmProz & " (FARBM_ID,MISCH_ID,FARBM_IRFA,FARBM_PROZ)" & " VALUES(?,?,?,?)"
    CmdDas.CommandText = SqlStmt
    CmdDas.Parameters.Clear()
    CmdDas.Parameters.Add(New OleDbParameter("FARBM_ID", OleDbType.Integer))
    CmdDas.Parameters.Add(New OleDbParameter("MISCH_ID", OleDbType.Integer))
    CmdDas.Parameters.Add(New OleDbParameter("FARBM_IRFA", OleDbType.SmallInt))
    CmdDas.Parameters.Add(New OleDbParameter("FARBM_PROZ", OleDbType.Single))

    j = 0
    For i = 0 To ArbFarb.PrfCount - 1
      If ArbFarb.Prf(i) >= 0 Then
        CmdDas.Parameters(0).Value = ArbFarb.ID
        CmdDas.Parameters(1).Value = MenueParam.MischID
        CmdDas.Parameters(2).Value = j
        CmdDas.Parameters(3).Value = ArbFarb.Prf(i)
        If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
          ier = -1
          Exit Sub
        End If
        j = j + 1
      End If
    Next i
    '
    '
    '
    '
    'Bindemittel-Prozentigkeiten einfügen
    '
    '
    '
    SqlStmt = "INSERT INTO " & MenueParam.TableFarbmProb & " (FARBM_ID,MISCH_ID,FARBM_IRBA,FARBM_PROB)" & " VALUES(?,?,?,?)"
    CmdDas.CommandText = SqlStmt
    CmdDas.Parameters.Clear()
    CmdDas.Parameters.Add(New OleDbParameter("FARBM_ID", OleDbType.Integer))
    CmdDas.Parameters.Add(New OleDbParameter("MISCH_ID", OleDbType.Integer))
    CmdDas.Parameters.Add(New OleDbParameter("FARBM_IRBA", OleDbType.SmallInt))
    CmdDas.Parameters.Add(New OleDbParameter("FARBM_PROZ", OleDbType.Single))
    j = 0
    For i = 0 To ArbFarb.PrbCount - 1
      If ArbFarb.Prb(i) >= 0 Then
        CmdDas.Parameters(0).Value = ArbFarb.ID
        CmdDas.Parameters(1).Value = MenueParam.MischID
        CmdDas.Parameters(2).Value = j
        CmdDas.Parameters(3).Value = ArbFarb.Prb(i)
        If SQLExeNonQuery(CmdDas, Cndat) <> 0 Then
          ier = -1
          Exit Sub
        End If
        j = j + 1
      End If
    Next i
    CmdDas.Parameters.Clear()
  End Sub
End Class