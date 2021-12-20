Option Strict Off
Option Explicit On
Option Compare Text






Public Class AufbauParameters
  Implements IDisposable

  '
  '0 kein Fehler
  '
  '-1 allg. Fehler
  '
  '
  '-10 Fehler Meßgerätekonfiguration
  '
  '-20 Fehler Normlichtarten
  '
  '
  '
  '
  '
  '
  '
  Dim Key As String
  Dim i As Integer
  Dim j As Integer
  Dim l As Integer
  Dim KM As Short
  Dim kw As Short
  Dim k As Integer
  Dim Menuid As Integer
  Dim LchgID As Integer
  Dim MatpaID As Integer
  Dim RezmnID As Integer
  Dim hlf() As Byte
  Dim Rhilf() As Single
  Dim SqlStmt As String
  Dim imsg As Short
  Dim Nlz As Short
  Dim LichtID() As Short
  Dim Nwe As Short
  Dim Nwp As Short
  Dim WellStart As Short
  Dim WellEnde As Short
  Dim WellStep As Short
  Dim NweD6164 As Short
  Dim NwpD6164 As Short
  Dim WellStartD6164 As Short
  Dim WellEndeD6164 As Short
  Dim WellStepD6164 As Short
  Dim BereichIDD6164 As Short
  Dim MnIer As Short
  Dim DbErr As Short
  Private CmdMerk As OleDbCommand
  Private CmdDys As OleDbCommand
  Private CmdDas As OleDbCommand
  Private TblMerk As OleDbDataReader
  Private Dyset As OleDbDataReader
  Private Daset As OleDbDataReader
  Private ErrCode As Integer
  Dim TableHilf As DataTable
  Dim ViewHilf As DataView


  Sub AufbauAnwsgMerk(ByRef UserID As Integer, ByRef MethID As Integer, ByRef ParamAlle As ValuesGrpsAssigns, ByRef ier As Integer)
    Dim AnwsgID As Integer
    Dim SqlStmt As String
    Dim Aufgart As String
    Dim MKbez As String
    Dim MForm As String
    Dim KeyAnsw As String
    Dim i As Short
    Dim k As Short
    Dim l As Short
    'Recordsets
    '
    '

    'Tabelle Merkmale
    '

    If ConnOpen(Cncol) Then
      ier = 0
      ParamAlle.clear()


      '
      'TblMerk = Dbas.OpenRecordset("TBL_MERK", DAO.RecordsetTypeEnum.dbOpenTable, DAO.RecordsetOptionEnum.dbConsistent, DAO.RecordsetOptionEnum.dbReadOnly)
      'TblMerk.Index = "MERK_ID"
      '
      '
      '
      'Anweisungen
      '
      '
      '
      '
      SqlStmt = "SELECT DISTINCT TBL_USER_METH_ANWSG.ANWSG_ID, TBL_MERK.AUSW_ID, TBL_AUFG.AUFG_ART, TBL_ANWSG.ANWSG_KBEZ, TBL_USER_METH_ANWSG.ZEIL_NR" _
      & " FROM (TBL_AUFG INNER JOIN TBL_ANWSG ON TBL_AUFG.AUFG_ID = TBL_ANWSG.AUFG_ID) INNER JOIN (((TBL_USER_METH INNER JOIN TBL_USER_METH_ANWSG ON (TBL_USER_METH.USER_ID = TBL_USER_METH_ANWSG.USER_ID)" _
      & " AND (TBL_USER_METH.METH_ID = TBL_USER_METH_ANWSG.METH_ID)) INNER JOIN TBL_USER_METH_ANWSG_MERK ON (TBL_USER_METH_ANWSG.USER_ID = TBL_USER_METH_ANWSG_MERK.USER_ID)" _
      & " AND (TBL_USER_METH_ANWSG.METH_ID = TBL_USER_METH_ANWSG_MERK.METH_ID) AND (TBL_USER_METH_ANWSG.ANWSG_ID = TBL_USER_METH_ANWSG_MERK.ANWSG_ID)" _
      & " AND (TBL_USER_METH.USER_ID = TBL_USER_METH_ANWSG_MERK.USER_ID)) INNER JOIN TBL_MERK ON TBL_USER_METH_ANWSG_MERK.MERK_ID = TBL_MERK.MERK_ID)" _
      & " ON (TBL_ANWSG.ANWSG_ID = TBL_USER_METH_ANWSG.ANWSG_ID) AND (TBL_USER_METH_ANWSG_MERK.ANWSG_ID = TBL_ANWSG.ANWSG_ID)" & " WHERE ((TBL_USER_METH.USER_ID =" & UserID & ") AND (TBL_USER_METH.Meth_id =" _
      & MethID & "))ORDER BY ZEIL_NR;"
      '
      CmdDys.CommandText = SqlStmt
      Dyset = DataReader(CmdDys, CommandBehavior.Default)
      '
      'Ausw_id ermitteln und abspeichern
      '
      '
      AnwsgID = -1
      KeyAnsw = ""
      Do While Dyset.Read
        If AnwsgID <> Dyset("Anwsg_ID") Then
          AnwsgID = Dyset("Anwsg_ID")
          KeyAnsw = KeyName(AnwsgID)
          Aufgart = Dyset("Aufg_Art")
          ParamAlle.Add(KeyAnsw, (New ValuesGrpsAssign))
          ParamAlle(KeyAnsw).AnwsgID = AnwsgID
          ParamAlle(KeyAnsw).AnwsgName = TexKt(AnwsgID + 30000)
          ParamAlle(KeyAnsw).AufgArt = Aufgart
          i = 0
          '
          '
          '
          '
          If Mid(Aufgart, 5, 1) <> Mid(Aufgart, 11, 1) Then
            MsgBox(Aufgart & Texxt(2969), 0)
            ier = 2669
            Dyset.Close()
            GoTo LabClose
          End If
          If Mid(Aufgart, 6, 1) = "?" And Mid(Aufgart, 12, 1) <> "?" Then
            MsgBox(Aufgart & Texxt(2968), 0)
            ier = 2668
            Dyset.Close()
            GoTo LabClose
          End If
        End If

        ParamAlle(KeyAnsw).AuswID.Add(KeyRe(Dyset("Ausw_ID")), New Object)
        ParamAlle(KeyAnsw).AuswID(KeyRe(Dyset("Ausw_ID"))) = Dyset("Ausw_ID")
        i = i + 1
        '
        '
        '
        '
      Loop
      Dyset.Close()
      For k = 0 To ParamAlle.Count - 1
        '
        'Parameter (zur Berechnung der Merkmale) für Auswahlen übernehmen
        '
        '
        '
        '
        '
        Aufgart = ParamAlle(k).AufgArt


        '
        'Merkmale für Anweisungen übernehmen
        '
        '
        '
        '
        SqlStmt = "SELECT * FROM TBL_USER_METH_ANWSG_MERK WHERE USER_ID=" & UserID & " AND METH_ID=" & MethID & " AND ANWSG_ID=" & ParamAlle(k).AnwsgID & " ORDER BY SPALT_NR"
        CmdDys.CommandText = SqlStmt
        Dyset = DataReader(CmdDys, CommandBehavior.Default)
        Do While Dyset.Read

          CmdMerk.CommandText = "SELECT * FROM TBL_MERK WHERE MERK_ID=" & Dyset("MERK_ID")
          TblMerk = DataReader(CmdMerk, CommandBehavior.SingleRow)

          If TblMerk.Read Then
            For l = 0 To ParamAlle(k).CountMerk - 1
              If TblMerk("MERK_KEN") = ParamAlle(k).Merk(l).Ken Then
                MsgBox(Texxt(3013) & TexKt(10000 + Dyset("MERK_ID")))
                Dyset.Close()
                TblMerk.Close()
                ier = 3013
                GoTo LabClose
              End If
            Next l
          End If
          MKbez = TexKt(10000 + Dyset("MERK_ID"))
          If Trim(MKbez) = "" Then
            MKbez = Dyset("MERK_KBEZ")
          End If
          If Not IsDBNull(Dyset("MERK_FORM")) AndAlso Dyset("MERK_FORM") <> "" Then
            MForm = Dyset("MERK_FORM")
          Else
            MForm = TblMerk("MERK_FORM")
          End If

          ParamAlle(k).AddMerk(TblMerk("MERK_KEN"), New Sign(TblMerk("MERK_ID"), TblMerk("MERK_KEN"), TblMerk("MERK_FAKT"), TblMerk("MERK_TYP"), MForm, MKbez, Dyset("SPALT_NR")))
          TblMerk.Close()

        Loop
        Dyset.Close()
      Next k
      '
LabClose:
      Cncol.Close()
    End If
  End Sub





  '
  '
  '
  ReadOnly Property ier() As Short
    Get
      ier = MnIer
    End Get
  End Property

  WriteOnly Property UserID() As Integer
    Set(ByVal Value As Integer)
      If MenueParam.UserID = Value And MnIer = 0 Then Exit Property
      MnIer = 0
      If ConnOpen(Cncol) Then
        Try
          MenueParam.AufUserID = Value
          ErrCode = 1
          Call AufbauUser(MenueParam.UserID, MenueParam.User)
          If MnIer <> 0 Then GoTo usecls
          ErrCode = 2
          Call AufbauUserMessg(MenueParam.UserID, MenueParam.MessgID, MenueParam.Messg)
          If MnIer <> 0 Then GoTo usecls
          ErrCode = 3
          Call AufbauUserMeth(MenueParam.UserID, MenueParam.MethID, MenueParam.Menue, MenueParam.ParaMerks)
          If MnIer <> 0 Then GoTo usecls
          ErrCode = 4
          Call AufbauUserMethMessg(MenueParam.UserID, MenueParam.MethID, MenueParam.MessgID, MenueParam.User, MenueParam.Messg)
          If MnIer <> 0 Then GoTo usecls
          ErrCode = 5
          Call AufbauUserMisch(MenueParam.UserID, MenueParam.MischID, MenueParam.Misch)
          If MnIer <> 0 Then GoTo usecls
          ErrCode = 6
          Call AufbauUserMischMessgMeth(MenueParam.UserID, MenueParam.MischID, MenueParam.MessgID, MenueParam.MethID, MenueParam.Misch, MenueParam.Messg)
          If MnIer <> 0 Then GoTo usecls
          ErrCode = 7
          Call AufbauUserMethMisch(MenueParam.UserID, MenueParam.MethID, MenueParam.MischID, MenueParam.Misch)
          If MnIer <> 0 Then GoTo usecls
          ErrCode = 8
          Call AufbauUserMethNormID(MenueParam.UserID, MenueParam.MethID)
          If MnIer <> 0 Then GoTo usecls
          ErrCode = 9
          Call AufbauNormFa(MenueParam.UserID, MenueParam.MethID, MenueParam.Messg.BereichID, Nwe, WellStart, WellEnde, WellStep, MenueParam.Normfa)
          If MnIer <> 0 Then GoTo usecls
          ErrCode = 10
          Call GETNwe(BereichIDD6164, NweD6164, NwpD6164, WellStartD6164, WellEndeD6164, WellStepD6164, MnIer)
          If MnIer <> 0 Then GoTo usecls
          ErrCode = 11
          Call AufbauNormFa(MenueParam.UserID, MenueParam.MethID, BereichIDD6164, NweD6164, WellStartD6164, WellEndeD6164, WellStepD6164, MenueParam.NormFaD6164)
          If MnIer <> 0 Then GoTo usecls
          GoTo usecls
        Catch ex As Exception
          MsgBox(ex.Message)
          MsgBox(Texxt(8) & " USERID= " & CStr(MenueParam.UserID) & " Errcode= " & ErrCode)
          MnIer = -1
        End Try
UseCls: Cncol.Close()
      End If
    End Set
  End Property
  WriteOnly Property MessgID() As Integer
    Set(ByVal Value As Integer)
      If MenueParam.MessgID = Value And MnIer = 0 Then Exit Property
      MnIer = 0
      If ConnOpen(Cncol) Then

        Try
          MenueParam.AufMessgID = Value
          ErrCode = 1
          Call AufbauMessg(MenueParam.MessgID, MenueParam.Messg)
          If MnIer <> 0 Then GoTo mescls
          ErrCode = 2
          Call AufbauUserMessg(MenueParam.UserID, MenueParam.MessgID, MenueParam.Messg)
          If MnIer <> 0 Then Exit Property
          ErrCode = 3
          Call AufbauUserMethMessg(MenueParam.UserID, MenueParam.MethID, MenueParam.MessgID, MenueParam.User, MenueParam.Messg)
          If MnIer <> 0 Then GoTo mescls
          ErrCode = 4
          Call AufbauUserMischMessgMeth(MenueParam.UserID, MenueParam.MischID, MenueParam.MessgID, MenueParam.MethID, MenueParam.Misch, MenueParam.Messg)
          If MnIer <> 0 Then GoTo mescls
          ErrCode = 5
          Call AufbauNormFa(MenueParam.UserID, MenueParam.MethID, MenueParam.Messg.BereichID, Nwe, WellStart, WellEnde, WellStep, MenueParam.Normfa)
          If MnIer <> 0 Then GoTo mescls
          ErrCode = 6
          Call GETNwe(BereichIDD6164, NweD6164, NwpD6164, WellStartD6164, WellEndeD6164, WellStepD6164, MnIer)
          If MnIer <> 0 Then GoTo mescls
          ErrCode = 7
          Call AufbauNormFa(MenueParam.UserID, MenueParam.MethID, BereichIDD6164, NweD6164, WellStartD6164, WellEndeD6164, WellStepD6164, MenueParam.NormFaD6164)
          If MnIer <> 0 Then GoTo mescls
          GoTo mescls
        Catch ex As Exception

          MsgBox(ex.Message)
          MsgBox(Texxt(8) & " MESSGID= " & CStr(MenueParam.MessgID) & " Errcode= " & ErrCode)
          MnIer = -1
        End Try
MesCls: Cncol.Close()
      End If
    End Set
  End Property
  WriteOnly Property MischID() As Integer
    Set(ByVal Value As Integer)
      If MenueParam.MischID = Value And MnIer = 0 Then Exit Property
      MnIer = 0
      If ConnOpen(Cncol) Then
        Try
          MenueParam.AufMischID = Value
          ErrCode = 1
          Call AufbauMisch(MenueParam.MischID, MenueParam.Misch)
          If MnIer <> 0 Then GoTo MisCls
          ErrCode = 2
          Call AufbauUserMisch(MenueParam.UserID, MenueParam.MischID, MenueParam.Misch)
          If MnIer <> 0 Then Exit Property
          ErrCode = 3
          'Call AufbauUserMischMessgMeth(MenueParam.UserID, MenueParam.MischID, MenueParam.MessgID, MenueParam.MethID, MenueParam.Misch, MenueParam.Messg)
          'If MnIer <> 0 Then GoTo MisCls
          'ErrCode = 4
          Call AufbauUserMethMisch(MenueParam.UserID, MenueParam.MethID, MenueParam.MischID, MenueParam.Misch)
          If MnIer <> 0 Then GoTo MisCls
          ErrCode = 5
          Call AufbauUserMischMessgMeth(MenueParam.UserID, MenueParam.MischID, MenueParam.MessgID, MenueParam.MethID, MenueParam.Misch, MenueParam.Messg)
          If MnIer <> 0 Then GoTo MisCls
          Call AufbauUserMethMessg(MenueParam.UserID, MenueParam.MethID, MenueParam.MessgID, MenueParam.User, MenueParam.Messg)
          If MnIer <> 0 Then GoTo MisCls
          ErrCode = 6
          GoTo MisCls
        Catch ex As Exception
          MsgBox(ex.Message)
          MsgBox(Texxt(8) & " MISCHID= " & CStr(MenueParam.MischID) & " Errcode= " & ErrCode)
          MnIer = -1
        End Try
MisCls: Cncol.Close()
      End If
    End Set
  End Property
  WriteOnly Property MethID() As Integer
    Set(ByVal Value As Integer)
      If MenueParam.MethID = Value And MnIer = 0 Then Exit Property
      MnIer = 0
      If ConnOpen(Cncol) Then

        Try
          MenueParam.AufMethID = Value
          ErrCode = 1
          Call AufbauUserMessg(MenueParam.UserID, MenueParam.MessgID, MenueParam.Messg)
          If MnIer <> 0 Then GoTo MthCls
          ErrCode = 2
          Call AufbauUserMisch(MenueParam.UserID, MenueParam.MischID, MenueParam.Misch)
          If MnIer <> 0 Then Exit Property
          ErrCode = 3
          Call AufbauUserMeth(MenueParam.UserID, MenueParam.MethID, MenueParam.Menue, MenueParam.ParaMerks)
          If MnIer <> 0 Then GoTo MthCls
          ErrCode = 4
          ' Call AufbauUserMethMessg(MenueParam.UserID, MenueParam.MethID, MenueParam.MessgID, MenueParam.User, MenueParam.Messg)
          ' If MnIer <> 0 Then GoTo MthCls
          ' ErrCode = 5
          Call AufbauUserMischMessgMeth(MenueParam.UserID, MenueParam.MischID, MenueParam.MessgID, MenueParam.MethID, MenueParam.Misch, MenueParam.Messg)
          If MnIer <> 0 Then GoTo MthCls
          ErrCode = 7
          Call AufbauUserMethMisch(MenueParam.UserID, MenueParam.MethID, MenueParam.MischID, MenueParam.Misch)
          If MnIer <> 0 Then GoTo MthCls
          ErrCode = 6
          'Call AufbauUserMischMessgMeth(MenueParam.UserID, MenueParam.MischID, MenueParam.MessgID, MenueParam.MethID, MenueParam.Misch, MenueParam.Messg)
          'If MnIer <> 0 Then GoTo MthCls
          'ErrCode = 7
          Call AufbauUserMethMessg(MenueParam.UserID, MenueParam.MethID, MenueParam.MessgID, MenueParam.User, MenueParam.Messg)
          If MnIer <> 0 Then GoTo MthCls
          ErrCode = 5
          Call AufbauUserMethNormID(MenueParam.UserID, MenueParam.MethID)
          If MnIer <> 0 Then GoTo MthCls
          ErrCode = 8
          Call AufbauNormFa(MenueParam.UserID, MenueParam.MethID, MenueParam.Messg.BereichID, Nwe, WellStart, WellEnde, WellStep, MenueParam.Normfa)
          If MnIer <> 0 Then GoTo MthCls
          ErrCode = 9
          Call GETNwe(BereichIDD6164, NweD6164, NwpD6164, WellStartD6164, WellEndeD6164, WellStepD6164, MnIer)
          If MnIer <> 0 Then GoTo MthCls
          ErrCode = 10
          Call AufbauNormFa(MenueParam.UserID, MenueParam.MethID, BereichIDD6164, NweD6164, WellStartD6164, WellEndeD6164, WellStepD6164, MenueParam.NormFaD6164)
          If MnIer <> 0 Then GoTo MthCls
          GoTo MthCls
        Catch ex As Exception
          MsgBox(ex.Message)
          MsgBox(Texxt(8) & " METHID= " & CStr(MenueParam.MethID) & " Errcode= " & ErrCode)
          MnIer = -1
        End Try
MthCls: Cncol.Close()
      End If
    End Set
  End Property

  Private Sub StartClear()
    ReDim LichtID(0)
    Nlz = 1
    LichtID(0) = 3
    MnIer = 0
    Nwe = -1
    BereichIDD6164 = 3
    WellStartD6164 = 0
    WellEndeD6164 = 0
    WellStepD6164 = 0
  End Sub

  Private Sub AufbauUser(ByRef UserID As Integer, ByRef User As UserParameters)
    '
    '
    '
    MnIer = 0
    '
    If UserID = -1 Then Exit Sub
    '
    '
    Call CreateUserGroups(UserID)
    '
    '
    '
    SqlStmt = "Select * FROM TBL_USER WHERE USER_ID=" & UserID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)
    If Dyset.Read Then
      User.Name = Dyset("USER_NAME")
      User.Passw = Dyset("USER_PASSW")
      '
      '
      '
      'Standardeinstellungen für User übernehmen
      '
      '
      User.Enabl = Dyset("user_Enabl")
      User.Visbl = Dyset("user_Visbl")
      User.Writ = Dyset("user_Write")
      User.Sonst = Dyset("user_Sonst")
      User.Druq = Dyset("user_Druq")
      User.Drum = Dyset("user_Drum")
    Else
      MnIer = -1
      MsgBox(Texxt(2993))
      Dyset.Close()
      Exit Sub
    End If
    Dyset.Close()
    '
    '
    'User-Methoden
    '
    '
    '

    SqlStmt = "SELECT * FROM TBL_USER_METH WHERE USER_ID=" & UserID & " ORDER BY METH_ID"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.Default)

    i = 0
    Do While Dyset.Read
      User.MethodID(i) = Dyset("Meth_id")
      i = i + 1
    Loop
    '
    Dyset.Close()

  End Sub
  Private Sub AufbauUserMessg(ByRef UserID As Integer, ByRef MessgID As Integer, ByRef Messg As MeasParameters)
    '
    '
    '
    MnIer = 0
    If UserID = -1 Then Exit Sub
    If MessgID = -1 Then Exit Sub

    '
    '
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_USER_MESSG WHERE MESSG_ID=" & MessgID & " AND USER_ID=" & UserID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)
    '
    If Dyset.Read Then
      Messg.Imes = Dyset("messg_imes")
      Messg.Ikal = Dyset("messg_ikal")
      Messg.De = Dyset("messg_DE")
      Messg.Stell = Dyset("messg_Stell")
      Messg.Tdiff = Dyset("messg_Tdiff")
      Messg.Twait = Dyset("messg_Twait")
      Messg.Top = Dyset("messg_Top")
      Messg.LaborID = Dyset("messg_Labor_ID")
      If Dyset("messg_Ini") = 0 Then
        Messg.Ini = Dyset("messg_Ini")
        Messg.Sond = 0
      End If
      If Dyset("messg_kal") = 0 Then
        Messg.Kal = Dyset("messg_kal")
        Messg.Sond = 0
        Messg.Nkal = 0
      End If
    End If
    '
    Dyset.Close()
    '
    '
    '
    'Labor
    '
    '
    SqlStmt = "SELECT * from TBL_LABOR WHERE LABOR_ID=" & Messg.LaborID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)

    If Dyset.Read Then
      Messg.LaborKbez = Dyset("Labor_Kbez")
      Messg.LaborAbtc = Dyset("Labor_Abtc")
      Messg.LaborKst = Dyset("Labor_Kst")
      Messg.LaborText1 = Space(1)
      Messg.LaborText2 = Space(1)
      Messg.LaborText2 = Space(1)
      If Not IsDBNull(Dyset("Labor_Text1")) AndAlso Dyset("Labor_Text1") <> "" Then
        Messg.LaborText1 = Dyset("Labor_Text1")
      Else
        Messg.LaborText1 = ""
      End If
      If Not IsDBNull(Dyset("Labor_Text2")) AndAlso Dyset("Labor_Text2") <> "" Then
        Messg.LaborText2 = Dyset("Labor_Text2")
      Else
        Messg.LaborText2 = ""
      End If
      If Not IsDBNull(Dyset("Labor_Text3")) AndAlso Dyset("Labor_Text3") <> "" Then
        Messg.LaborText3 = Dyset("Labor_Text3")
      Else
        Messg.LaborText3 = ""
      End If
    End If
    Dyset.Close()
    '


  End Sub
  Private Sub AufbauUserMeth(ByRef UserID As Integer, ByRef MethID As Integer, ByRef Menue As MenuParameters, ByRef ParaMerks As ParamSignAll)
    '
    Dim i As Integer
    '
    MnIer = 0
    If UserID = -1 Then Exit Sub
    If MethID = -1 Then Exit Sub
    '
    '
    Menue.MethBez = Texxt(1800 + MethID)
    Menue.MethKbez = TexKt(1800 + MethID)
    '
    'Parameter (zur Berechnung der Merkmale) für Auswahlen übernehmen
    '
    ParaMerks.ClearParam()
    SqlStmt = "SELECT * FROM TBL_PARAM "
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.Default)
    '
    '
    Do While Dyset.Read
      'ParaMerks.AddParam(KeyRe(Dyset!param_ID), New ParamSign(Dyset!param_ID, Dyset!param_kbez, Dyset!param_lbez, Dyset!param_bezwrt, "N", Dyset!param_form, Dyset!param_wert))
      ParaMerks.AddParam(KeyRe(Dyset!param_ID), New ParamSign(Dyset!param_ID, TexKt(25000 + Dyset!param_ID), Texxt(25000 + Dyset!param_ID), Dyset!param_bezwrt, "N", Dyset!param_form, Dyset!param_wert))

    Loop
    Dyset.Close()
    '
    '****************
    'TBL_
    '
    '
    '
    '
    '
    'AuswID zuordnen
    '
    '
    SqlStmt = "Select * FROM TBL_AUSW_PARAM"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.Default)

    Do While Dyset.Read
      For i = 0 To ParaMerks.ParamCount - 1
        If ParaMerks(i).ID = Dyset!PARAM_ID Then
          ParaMerks(i).AuswID.Add(KeyRe(Dyset!AUSW_ID), New Object)
          ParaMerks(i).AuswID(KeyRe(Dyset!AUSW_ID)) = Dyset!AUSW_ID
        End If
      Next i
    Loop
    Dyset.Close()
    '
    '
    'ÜBERSCHREIBEN mit USER-spezifischen Werten
    '
    '
    '
    '
    '
    '
    '
    For i = 0 To ParaMerks.ParamCount - 1
      SqlStmt = "SELECT * FROM TBL_USER_METH_PARAM" _
       & " WHERE USER_ID=" & UserID _
       & " AND METH_ID=" & MethID _
       & " AND PARAM_ID=" & ParaMerks(i).ID
      CmdDys.CommandText = SqlStmt
      Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)
      If Dyset.Read Then
        ParaMerks(KeyRe(ParaMerks(i).ID)).Wert = Dyset("PARAM_WERT")
        ParaMerks(KeyRe(ParaMerks(i).ID)).BezWrt = Dyset("PARAM_BEZWRT")
      End If
      Dyset.Close()
    Next i
    ''
    '
    '
    'ID's für MENU,BWERT,LCHG,MATPA ermitteln
    'Parameter übernehmen
    '
    SqlStmt = "SELECT * FROM TBL_USER_METH WHERE USER_ID=" & UserID & " AND METH_ID=" & MethID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)


    If Dyset.Read Then
      Menuid = Dyset("menu_id")
      LchgID = Dyset("lchg_ID")
      MatpaID = Dyset("matpa_ID")
      '
      '
      'BwertID
      'FSTAEID
      'Absid (Absolutwerte für Typ bei Differenzen)
      '
      'Call BwrtFstAbs(Dyset("bw_fs_abs_id"), BwertID, FstaeID, AbsID)
      Menue.BwertID = Dyset("BW_ID")
      Menue.FstaeID = Dyset("FS_ID")
      Menue.AbsID = Dyset("ABS_ID")
      Dyset.Close()
    Else
      MsgBox(Texxt(3558))
      Dyset.Close()
      MnIer = -1
      Exit Sub
    End If
    Dyset.Close()

    '

    '
    'Parameter aus Tabelle TBL_MENU
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_MENU WHERE MENU_ID=" & Menuid
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)
    If Dyset.Read Then
      Menue.KdeWS = Dyset("de_w_s")
      Menue.GewDEWS = Dyset("Gew_DE_W_S")
      Menue.NrIhrmDE = Dyset("NR_IHRM_DE")
      Menue.VerWS = Dyset("Verh_Wei_Schw")
      Menue.Schwell = Dyset("de_schwell")
      Menue.Fopt = Dyset("Fopt")
      Try
        Menue.JABST = Dyset("JABST")
      Catch ex As Exception
        Menue.JABST = 0
      End Try
    End If
    Dyset.Close()
    '
    '
    '
    'Parameter aus TBL_LCHG (Gewichte für DL DC,DH)
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_LCHG WHERE LCHG_ID=" & LchgID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)
    If Dyset.Read Then
      Menue.Lgew = Dyset("Lgew")
      Menue.Cgew = Dyset("Cgew")
      Menue.Hgew = Dyset("Hgew")
      Menue.Fu(0) = Dyset("ugew_w")
      Menue.Fu(1) = Dyset("ugew_s")
    End If
    Dyset.Close()
    '
    '

    '
    'Parameter TBL_MATPA
    '
    SqlStmt = "SELECT * FROM TBL_MATPA WHERE MATPA_ID=" & MatpaID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)
    If Dyset.Read Then
      Menue.Fto = Dyset("matpa_fto")
      Menue.Ato = Dyset("matpa_ato")
      Menue.Itm = Dyset("matpa_itm")
      Menue.Del = Dyset("matpa_Del")
      Menue.SPR = Dyset("matpa_SPR")
      Menue.EXPO = Dyset("matpa_EXPO")
      Menue.Delta = 0.0001
      Menue.TolInd = 0.1
      Menue.Tolsze = 0.1
      Menue.Fac = 1
      Menue.Wt = 1
      Menue.Mval = 1
      Menue.ItMax = 1
    End If
    Dyset.Close()
    '
    '
    '
    'Name B-Wert einlesen (MerkID=775)
    '
    '
    SqlStmt = "SELECT * FROM TBL_BWERT ORDER BY BWERT_ID"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.Default)

    Do While Dyset.Read
      Menue.BwertName(Dyset("bwert_id")) = Dyset("bwert_bez")
    Loop
    Dyset.Close()

    ''

  End Sub
  Private Sub AufbauMisch(ByRef MischID As Integer, ByRef Misch As MixingParameter)
    '

    MnIer = 0
    '
    '
    If MischID = -1 Then Exit Sub
    '
    '
    '
    '
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_MISCH WHERE MISCH_ID=" & MischID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)
    If Dyset.Read Then
      Misch.Tbl = Dyset("Misch_tbl")
      Misch.Setting = Dyset("misch_set")
      Misch.Kbez = Dyset("misch_kbez")
      Misch.Lbez = Dyset("misch_lbez")
      If Not IsDBNull(Dyset("Misch_Einm")) Then
        Misch.Einm = Dyset("Misch_Einm")
      Else
        Misch.Einm = " "
      End If
      If Not IsDBNull(Dyset("Misch_Eind")) Then
        Misch.Eind = Dyset("Misch_Eind")
      Else
        Misch.Eind = " "
      End If
    Else
      MnIer = -1
      MsgBox(Texxt(422) & Space(1) & Texxt(2984))
      Dyset.Close()
      Exit Sub
    End If
    Dyset.Close()
  End Sub

  Private Sub AufbauUserMisch(ByRef UserID As Integer, ByRef MischID As Integer, ByRef Misch As MixingParameter)
    '
    '
    '
    MnIer = 0
    If UserID = -1 Then Exit Sub
    If MischID = -1 Then Exit Sub
    '
    '
    '
    '
    '
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_USER_MISCH WHERE USER_ID=" & UserID & " AND MISCH_ID=" & MischID
    '
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)

    If Dyset.Read Then
      Misch.Top = Dyset("Misch_Top")
      Misch.Tdiff = Dyset("Misch_tdiff")
      Misch.Sond = Dyset("Misch_sond")
    End If
    Dyset.Close()

  End Sub

  Private Sub AufbauUserMethMessg(ByRef UserID As Integer, ByRef MethID As Integer, ByRef MessgID As Integer, ByRef User As UserParameters, ByRef Messg As MeasParameters)
    Dim GkwrtID As Integer
    Dim kw As Short
    Dim i As Integer
    '
    '
    '
    '
    MnIer = 0
    '
    If UserID = -1 Then Exit Sub
    If MethID = -1 Then Exit Sub
    If MessgID = -1 Then Exit Sub
    '
    '

    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_USER_METH_MESSG WHERE USER_ID=" & UserID & " AND METH_ID=" & MethID & " AND MESSG_ID=" & MessgID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)

    If Dyset.Read Then
      If IsDBNull(Dyset("User_Ref_GID")) Then
        Messg.UserRefGID = 0
      Else
        Messg.UserRefGID = Dyset("User_Ref_GID")
      End If
      Messg.RwrtGID = Messg.UserRefGID

      If IsDBNull(Dyset("Messg_ka")) Then
        Messg.Ka = 0
      Else
        Messg.Ka = Dyset("Messg_ka")
      End If
      If Messg.RefTra = "A" Then
        If IsDBNull(Dyset("Messg_ReTr")) Then
          Messg.ReTr = 0
        Else
          Messg.ReTr = Dyset("Messg_ReTr")
        End If
      ElseIf Messg.RefTra = "R" Then
        Messg.ReTr = 0
      ElseIf Messg.RefTra = "T" Then
        Messg.ReTr = 1
      End If
      If MethID < 50 Or MethID >= 100 Then
        GkwrtID = Dyset("gkwrt_id")
        Dyset.Close()
        Call ParGkw(GkwrtID, Messg)
      End If
    End If
    Dyset.Close()
    '
   
    'Anzahl Winkel pro Messgerät und User km
    '
    '
    'ihrm Winkel bzw. Messgeometrien für User
    '
    '
    'Glanz (1=Ja;0=Nein), d.h. der vorgegebene glanz wird verwendet oder nicht verwendet
    '
    '
    'Inref innere Reflexion (s. Glanz)
    '
    '
    KM = 0
    SqlStmt = "SELECT * FROM TBL_USER_METH_MESSG_IHRM WHERE USER_ID=" & UserID & " AND METH_ID=" & MethID & " AND MESSG_ID=" & MessgID & " ORDER BY POS_ID"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.Default)


    User.Winkel.clear()
    KM = -1
    k = -1
    Do While Dyset.Read
      KM = KM + 1
      For kw = 0 To Messg.Winkel.Km - 1
        If Messg.Winkel(kw).IhrmID = Dyset("ihrm_id") Then
          k = k + 1
          Key = Trim(Messg.Winkel(kw).Chrm)
          User.Winkel.Add(Key, New AngGeo(Messg.Winkel(Key).IhrmID, kw, Messg.Winkel(Key).Iglz, Messg.Winkel(Key).IhrmBez, Dyset("ihrm_gew"), Key))
          User.Winkel(Key).GK = Messg.Winkel(Key).GK
          Exit For
        End If
      Next kw
    Loop
    Dyset.Close()

    User.Winkel.Wsol = Messg.Winkel.Wsol
    User.Winkel.Nwp = Messg.Winkel.Nwp
    If k <> KM Then
      MsgBox(Texxt(3509))
      MnIer = -10
    End If
    If KM = -1 Then
      MsgBox(Texxt(2995) & " (" & MenueParam.Menue.MethBez & ")", 0)
      MnIer = -10
    End If
    If Not IsNothing(Messg.RefGew) Then
      For i = 0 To Messg.RefGew.Nwe - 1
        Messg.RefGew.R(i) = 0.0
      Next
    End If
    If Not IsNothing(Messg.RefOne) Then
      For i = 0 To Messg.RefOne.Nwe - 1
        Messg.RefOne.R(i) = 1.0
      Next
    End If

  End Sub

  Private Sub AufbauMessg(ByRef MessgID As Integer, ByRef Messg As MeasParameters)
    Dim MessgKE As String
    Dim i As Integer
    '
    '
    '
    MnIer = 0
    If MessgID < 0 Then Exit Sub
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_MESSG WHERE MESSG_ID=" & MessgID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)

    '
    If Dyset.Read Then
      Messg.MessgID = MessgID
      If IsDBNull(Dyset("messgrw_ID")) Then
        Messg.MessgRwID = Messg.MessgID
      Else
        Messg.MessgRwID = Dyset("messgrw_ID")
      End If

      Messg.Tbl = Dyset("messg_Tbl")
      Messg.Com = Dyset("messg_com")
      Messg.Kenn = Dyset("messg_kenn")
      '
      '
      MessgKE = GetPrivSettings("STARTUP", "MESSGKE", "  ", COLORFileName())
      '
      If Len(MessgKE.Trim) > 1 Then
        Messg.Kenn = MessgKE.Substring(0, 2)
      End If
      Messg.Kenn = Messg.Kenn.Replace("'", "#")

      Messg.Kbez = Dyset("messg_kbez")
      Messg.Lbez = Dyset("messg_lbez")
      Messg.Baud = Dyset("messg_baud")
      Messg.Length = Dyset("messg_length")
      Messg.Stopbit = Dyset("messg_stop")
      Messg.Hands = Dyset("messg_hands")
      Messg.Driver = Dyset("messg_Driver")
      Messg.Par = Dyset("messg_par")
      Messg.RefTra = Dyset("Messg_RefTra")
      If IsDBNull(Dyset("messg_prog")) Then
        Messg.Prog = ""
      Else
        Messg.Prog = Dyset("messg_prog")
      End If
      Messg.Mes = Dyset("messg_mes")
      Messg.Kal = Dyset("messg_kal")
      Messg.Nkal = Dyset("messg_nkal")
      Messg.Ini = Dyset("messg_Ini")
      Messg.Sond = Dyset("messg_sond")
      Messg.Kein = Dyset("messg_kein")
      Messg.BereichID = Dyset("messg_norm_file_ID")
      WellStart = 0
      WellEnde = 0
      If Dyset.FieldCount > 25 Then
        WellStart = Dyset!MESSG_WANF
        WellEnde = Dyset!MESSG_WEND
      End If
      Messg.Setting = Dyset("messg_Set")
      Messg.GlIn = Dyset("messg_GlIn")
      Messg.KalInt = Dyset("messg_KalInt")
    Else
      MnIer = -1
      MsgBox(Texxt(204) & Space(1) & Texxt(2984))
      Dyset.Close()
      Exit Sub
    End If
    Dyset.Close()
    '
    '
    'Messgertäteparameter, falls verschiedene Messgeräte mit unterschiedlichen Kalöibrierdatenbanken zur gleichen MessgID gehören
    '
    '
    '
    '
    '
    'Anzahl Winkel pro Messgerät km
    '
    '
    'ihrm Winkel bzw. Messgeometrien
    '
    '
    'IGlz (1=Ja;0=Nein), d.h. der vorgegebene Glanz wird verwendet oder nicht verwendet
    '
    '
    'Inref innere Reflexion (s. Glanz)
    '
    SqlStmt = "SELECT TBL_MESSG_IHRM.IHRM_ID AS IHRM_ID,IHRM_IHRM,IHRM_GLANZ,POS_ID,IHRM_BEZ FROM TBL_MESSG_IHRM,TBL_IHRM" & " WHERE TBL_MESSG_IHRM.IHRM_ID=TBL_IHRM.IHRM_ID" & " AND MESSG_ID=" & MessgID & " ORDER BY POS_ID"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.Default)
    Messg.Winkel.clear()
    KM = -1
    Do While Dyset.Read
      KM = KM + 1
      Key = Trim(Chrum(Dyset("ihrm_ihrm")))
      Messg.Winkel.Add(Key, New AngGeo(Dyset("ihrm_id"), KM, Dyset("ihrm_glanz"), Dyset("ihrm_BEZ"), 1.0#, Key))
    Loop
    Dyset.Close()
    Call AufbauWsol(Nwe, Messg.BereichID, WellStart, WellEnde, WellStep, Messg.Winkel)
    Messg.WellStart = WellStart
    Messg.WellEnde = WellEnde
    Messg.WellStep = WellStep
    Messg.RefGew = New CurveRef(Nwe)
    For i = 0 To Nwe - 1
      Messg.RefGew.R(i) = 0.0
    Next i
    Messg.RefOne = New CurveRef(Nwe)
    For i = 0 To Nwe - 1
      Messg.RefOne.R(i) = 1.0
    Next i
  End Sub
  Public Sub ParGkw(ByRef GKWrtID As Integer, ByRef Messg As MeasParameters, Optional ByRef Misch As MixingParameter = Nothing)
    Dim Dysm As OleDbDataReader
    Dim Cmdsym As OleDbCommand
    Dim GK(15) As Single
    Dim kw As Integer
    Cmdsym = New OleDbCommand
    Messg.GKwrtID = GKWrtID
    Cmdsym.CommandText = "SELECT * FROM TBL_GKWRT WHERE GKWRT_ID=" & GKWrtID
    Dysm = DataReader(Cmdsym, CommandBehavior.SingleRow, Cncol)
    If Dysm.Read() Then
      Messg.CDE = Dysm("CDE")
      Messg.GKsperr = Dysm("Sperr")
      Messg.GKBez = Dysm("GKWRT_BEZ")

      GK(0) = CSng(Dysm("GK01"))
      GK(1) = CSng(Dysm("GK02"))
      GK(2) = CSng(Dysm("GK03"))
      GK(3) = CSng(Dysm("GK04"))
      GK(4) = CSng(Dysm("GK05"))
      GK(5) = CSng(Dysm("GK06"))
      GK(6) = CSng(Dysm("GK07"))
      GK(7) = CSng(Dysm("GK08"))
      GK(8) = CSng(Dysm("GK09"))
      GK(9) = CSng(Dysm("GK10"))
      GK(10) = CSng(Dysm("GK11"))
      GK(11) = CSng(Dysm("GK12"))
      GK(12) = CSng(Dysm("GK13"))
      GK(13) = CSng(Dysm("GK14"))
      GK(14) = CSng(Dysm("GK15"))
      GK(15) = CSng(Dysm("GK16"))
    End If
    Dysm.Close()
    '
    '
    '
    For kw = 0 To Messg.Winkel.Km - 1
      Messg.Winkel(kw).GK = GK.Clone
    Next
    '
    '
    '
    '
    'Extra-GK_Werte
    '
    '
    Cmdsym.CommandText = "SELECT * FROM TBL_GKWRTEXT WHERE GKWRT_ID=" & GKWrtID
    Dysm = DataReader(Cmdsym, CommandBehavior.Default, Cncol)
    Do While Dysm.Read
      For kw = 0 To Messg.Winkel.Km - 1
        If Dysm("IHRM_ID") = Messg.Winkel(kw).IhrmID Then
          '
          '
          'ExtraGK-Werte übernehmen
          '
          Messg.Winkel(kw).GK(0) = Dysm("GK01")
          Messg.Winkel(kw).GK(1) = Dysm("GK02")
          Messg.Winkel(kw).GK(2) = Dysm("GK03")
          Messg.Winkel(kw).GK(3) = Dysm("GK04")
          Messg.Winkel(kw).GK(4) = Dysm("GK05")
          Messg.Winkel(kw).GK(5) = Dysm("GK06")
          Messg.Winkel(kw).GK(6) = Dysm("GK07")
          Messg.Winkel(kw).GK(7) = Dysm("GK08")
          Messg.Winkel(kw).GK(8) = Dysm("GK09")
          Messg.Winkel(kw).GK(9) = Dysm("GK10")
          Messg.Winkel(kw).GK(10) = Dysm("GK11")
          Messg.Winkel(kw).GK(11) = Dysm("GK12")
          Messg.Winkel(kw).GK(12) = Dysm("GK13")
          Messg.Winkel(kw).GK(13) = Dysm("GK14")
          Messg.Winkel(kw).GK(14) = Dysm("GK15")
          Messg.Winkel(kw).GK(15) = Dysm("GK16")
        End If
      Next
    Loop
    Dysm.Close()
    '
    '
    '
    For kw = 0 To Messg.Winkel.Km - 1
      '
      'Glanzwerte berücksichtigen
      '
      '
      Messg.Winkel(kw).GK(0) = Messg.Winkel(kw).Iglz * Messg.Winkel(kw).GK(0)
    Next
    If Not IsNothing(Misch) Then
      Misch.Npx = NopNPX(Messg.CDE)
      Misch.Transp = Transp(Messg.CDE)
      If Not Misch.Transp Then
        Misch.Kgx = False
      End If
      Misch.GKwrtID = Messg.GKwrtID
    End If
    Cmdsym.Dispose()
  End Sub

  Private Sub AufbauWsol(ByRef NWE As Short, ByRef BereichID As Integer, ByRef WellStart As Short, ByRef WellEnde As Short, ByRef WellStep As Short, ByRef Winkel As AngGeos)

    Dim i As Integer
    If BereichID < 0 Then
      NWE = -1
      Exit Sub
    End If
    '

    'File-Name für Normspektralwert_Datei
    '

    Call GETNwe(BereichID, NWE, Nwp, WellStart, WellEnde, WellStep, MnIer)
    If MnIer <> 0 Then Exit Sub
    Call GETWsol(BereichID, WellStart, WellEnde, WellStep, Rhilf, MnIer)
    If MnIer <> 0 Then Exit Sub
    If UBound(Rhilf) + 1 <> NWE Then
      MsgBox(Texxt(3022) & CStr(NWE) & Space(2) & CStr(UBound(Rhilf) + 1))
    End If
    NWE = (WellEnde - WellStart) / WellStep + 1
    Winkel.Wsol = New CurveRef(NWE)
    For i = 0 To Winkel.Wsol.Nwe - 1
      Winkel.Wsol.R(i) = WellStart + i * WellStep
      If Winkel.Wsol.R(i) = Rhilf(0) Then
        Winkel.StrtNwp = i
      End If
    Next i
    Winkel.Nwp = Nwp
    '
  End Sub
  Private Sub AufbauUserMethNormID(ByRef UserID As Integer, ByRef MethID As Integer)
    MnIer = 0
    If UserID < 0 Then Exit Sub
    If MethID < 0 Then Exit Sub

    Nlz = 0
    '
    SqlStmt = "SELECT * FROM TBL_USER_METH_LICHT WHERE" & " USER_ID=" & UserID & " AND METH_ID=" & MethID & " ORDER BY POS_ID"
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.Default)

    i = -1
    Do While Dyset.Read
      i = i + 1
      ReDim Preserve LichtID(i)
      LichtID(i) = Dyset("LICHT_ID")
    Loop
    Dyset.Close()
    Nlz = i + 1
    '
    '
    If Nlz = 0 Then
      MnIer = -20
      MsgBox(Texxt(2994) & " (" & MenueParam.Menue.MethBez & ")")
    End If
  End Sub
  Private Sub AufbauNormFa(ByRef UserID As Integer, ByRef MethID As Integer, ByRef BereichID As Integer, ByRef Nwe As Short, ByRef WellStart As Short, ByRef WellEnde As Short, ByRef WellStep As Short, ByRef Normfa As NormIlluminats)
    Dim NormKey As String
    Dim Welh As Short
    Dim Well() As Single
    Dim Nww As Short
    Dim IwelStart As Short
    Dim IwelEnde As Short
    Dim i As Short
    Dim l As Short
    Dim k As Short
    Dim XYZExist As Boolean
    Dim XYZSum As Single
    '
    MnIer = 0
    NormKey = ""
    '
    If Nwe <= 0 Then
      MnIer = -30
      Exit Sub
    End If
    Normfa.clear()
    '
    SqlStmt = "SELECT * FROM TBL_LICHT_BEREICH_SPEK WHERE [XYZ_ID]=0 AND BEREICH_ID=" & BereichID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)
    If Dyset.Read() Then
      hlf = Dyset("LICHT_SPEK")
      Well = GetSingles(hlf)
    Else
      Dyset.Close()
      Exit Sub
    End If
    Dyset.Close()
    Nww = UBound(Well) + 1
    '
    For i = 0 To Nwe - 1
      Welh = WellStart + i * WellStep
      If Well(0) = Welh Then
        IwelStart = i
        Exit For
      End If
    Next i
    IwelEnde = IwelStart + Nww
    '
    '
    'Normspektralwerte für NLZ Normlichtarten
    '
    '
    For i = 0 To Nlz - 1
      '
      '
      'Name und Kennung der Normlichtarten
      '
      '

      SqlStmt = "SELECT * FROM TBL_LICHT WHERE LICHT_ID=" & LichtID(i)
      CmdDys.CommandText = SqlStmt
      Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)
      If Dyset.Read() Then
        NormKey = Dyset("licht_kbez")
        Normfa.Add(Nwe, NormKey, New NormIlluminat)
        Normfa(NormKey).NormKenn = Dyset("licht_kenn")
        Normfa(NormKey).NormNama = Dyset("licht_kbez")
        Normfa(NormKey).LichtID = LichtID(i)
      Else
        Dyset.Close()
        Exit Sub
      End If
      Dyset.Close()
      '
      '
      'Normspektralwerte
      '
      For k = 0 To 2
        Normfa(NormKey).NormFakt(k) = 100.0
        For l = 0 To IwelEnde - 1
          Normfa(NormKey).Normkurven(k).R(l) = 0.0
        Next l
      Next k
      '
      SqlStmt = "SELECT * FROM TBL_LICHT_BEREICH_SPEK WHERE LICHT_ID=" & LichtID(i) & " AND BEREICH_ID=" & BereichID & " ORDER BY XYZ_ID"
      CmdDys.CommandText = SqlStmt
      Dyset = DataReader(CmdDys, CommandBehavior.Default)
      XYZExist = True
      Do While Dyset.Read
        '
        'XYZ_ID
        '     0    Wellenlängen (Lambda)
        '     1    x(Lambda)
        '     2    y(Lambda)
        '     3    z(Lambda)
        '
        '
        If Dyset("xyz_id") > 0 Then
          '
          '
          'Einlesen der Normspektralwerte
          '
          '
          hlf = Dyset("LICHT_SPEK")
          If hlf.Length <> 4 * Nww Then
            Dyset.Close()
            MsgBox(Texxt(2986), 0, Texxt(2000))
            MnIer = -20
            Exit Sub
          End If
          Rhilf = GetSingles(hlf)
          XYZSum = 0.0
          For l = 0 To Rhilf.Count - 1
            XYZSum = XYZSum + Rhilf(l)
          Next l
          If XYZSum < 0.001 Then
            XYZExist = False
            Continue Do
          End If
          '
          'Normspektralwerte für Wellenlängen kleiner als in Datenbank (z.B. UV)
          '
          '
          For l = 0 To IwelStart - 1
            Normfa(NormKey).Normkurven(Dyset("xyz_id") - 1).R(l) = 0.0
          Next l
          '
          'Normspektralwerte für Wellenlängen die in Datenbank enthalten
          '
          '
          For l = 0 To Nww - 1
            Normfa(NormKey).Normkurven(Dyset("xyz_id") - 1).R(l + IwelStart) = Rhilf(l)
          Next l
          '
          'Normspektralwerte für Wellenlängen größer als in Datenbank (z.B. IR)
          '
          '
          For l = IwelEnde To Nwe - 1
            Normfa(NormKey).Normkurven(Dyset("xyz_id") - 1).R(l) = 0.0
          Next l
          '
          '
          '
          Normfa(NormKey).NormFakt(Dyset("xyz_id") - 1) = Dyset("licht_fakt")
        End If
      Loop
      Dyset.Close()
      If Not XYZExist Then
        MsgBox(Texxt(3076) & Space(1) & Normfa(NormKey).NormNama & " (" & Nww & ")")
      End If

      SqlStmt = "SELECT * FROM TBL_USER_METH_LICHT WHERE USER_ID=" & UserID & " AND METH_ID=" & MethID & " AND LICHT_ID=" & LichtID(i)
      CmdDys.CommandText = SqlStmt
      Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)


      If Dyset.Read Then
        Normfa(NormKey).NormGew = Dyset("norm_gew")
      Else
        Normfa(NormKey).NormGew = 1.0#
      End If
      Dyset.Close()
    Next i


  End Sub

  Private Sub AufbauUserMischMessgMeth(ByRef UserID As Integer, ByRef MischID As Integer, ByRef MessgID As Integer, ByRef MethID As Integer, ByRef Misch As MixingParameter, ByRef Messg As MeasParameters)
    Dim GKWrtID As Integer

    '
    '
    '
    MnIer = 0
    Messg.MeArtID = 0

    '
    '
    If UserID = -1 Then Exit Sub
    If MischID = -1 Then Exit Sub
    If MessgID = -1 Then Exit Sub
    If MethID < 50 Or MethID >= 100 Then Exit Sub
    '
    '
    '
    '
    '
    '


    ' GK-Wert_ID für Mischsystem
    '

    '
    '
    '
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_USER_MISCH_MESSG WHERE USER_ID=" & UserID & " AND MISCH_ID=" & MischID & " AND MESSG_ID=" & MessgID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)

    If Dyset.Read Then
      GKWrtID = Dyset("gkwrt_id")
    Else
      GKWrtID = 0
    End If
    Dyset.Close()
    '
    'GK-Werte
    '
    '
    Call ParGkw(GKWrtID, Messg, Misch)
    '
    '
    '
    'Kopplungsparameter
    '
    '
    Call KoppDae(Misch.GKwrtID, Misch)
    '
    '
    '
    '
    'GruppenID der den Rezepten zugeordneten R-Werten
    '
    '
    '
    '
    '
    '
    '
    '
    '
    'Art der Messung (Reflexion/Transmission)
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_MISCH_MESSG WHERE MISCH_ID=" & MischID & " AND MESSG_ID=" & MessgID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)
    If Dyset.Read Then
      Messg.MeArtID = Dyset("MessArt_ID")
    End If
    Dyset.Close()
    If Messg.RefTra = "R" Then
      Messg.MeArtID = 0
    End If
    If Messg.RefTra = "T" Then
      Messg.MeArtID = 3
    End If
  End Sub

  Private Sub AufbauUserMethMisch(ByRef UserID As Integer, ByRef MethID As Integer, ByRef MischID As Integer, ByRef Misch As MixingParameter)
    '
    '
    '
    '
    MnIer = 0
    '
    '
    If UserID = -1 Then Exit Sub
    If MethID = -1 Then Exit Sub
    If MischID = -1 Then Exit Sub

    '
    '
    '
    '
    '
    'ID's für REZMN

    SqlStmt = "SELECT * FROM TBL_USER_METH_MISCH WHERE USER_ID=" & UserID & " AND METH_ID=" & MethID & " AND MISCH_ID=" & MischID
    CmdDys.CommandText = SqlStmt
    Dyset = DataReader(CmdDys, CommandBehavior.SingleRow)

    If Dyset.Read Then
      If IsDBNull(Dyset("rezmn_ID")) Then
        RezmnID = 0
      Else
        RezmnID = Dyset("rezmn_ID")
      End If
      If IsDBNull(Dyset("User_Rzp_GID")) Then
        Misch.UserRzpGID = 0
      Else
        Misch.UserRzpGID = Dyset("User_Rzp_GID")
      End If
      '
      '
      '

      '
      'Bwert_ID
      'FSTAE_ID
      'Absw_id (Absolutwerte für Typ bei Differenzen)
      '
      Misch.RezVor = CBool(Dyset("RezVor"))
      Misch.Schwrz = CBool(Dyset("Schwrz"))
      Misch.Vert = CBool(Dyset("Vert"))
      Misch.Ichi = Dyset("ICHI")
      Misch.Igx = Dyset("igx")
      Misch.Kgx = CBool(Dyset("kgx"))
      If Not Misch.Transp Then
        Misch.Kgx = False
      End If
      Misch.Isor = Dyset("Isor")
      Misch.Bprob = CBool(Dyset("Bprob"))
      Misch.Kwe = Dyset("kwe")
      Misch.Kwd = Dyset("kwd")
      Misch.MGGE = CBool(Dyset("MME"))
      Misch.MinDos = Dyset("MINDOS")
      'Dyset.Close()
      '
      '
       
      '


      '
      '
      'Parameter TBL_REZMN
      '
      SqlStmt = "SELECT * FROM TBL_REZMN WHERE REZMN_ID=" & RezmnID
      CmdDas.CommandText = SqlStmt
      Daset = DataReader(CmdDas, CommandBehavior.SingleRow)

      If Daset.Read Then
        Misch.Rzp = Daset("rezmn_rzp")
        Misch.MinFarb = Daset("rezmn_Min")
        Misch.MaxFarb = Daset("rezmn_Max")
        Misch.DeGut = Daset("rezmn_degut")
        Misch.Zuwag = 0.01 * Daset("rezmn_azu")
        Misch.Dto = Daset("rezmn_dto")
        Misch.Gge = Daset("rezmn_gge")
        Misch.Fde = Daset("rezmn_fde")
        Misch.Gde = Daset("rezmn_gde")
        Misch.Gto = Daset("rezmn_gto")
        Misch.Dicke = Daset("rezmn_dicke")
        Misch.DeHlf = Daset("rezmn_denhlf")
        Misch.Deok = Daset("rezmn_deok")
      End If
      Daset.Close()
    End If
    Dyset.Close()

  End Sub

  Private Sub KoppDae(ByRef GKwrtID As Integer, ByRef Misch As MixingParameter)
    '
    '
    'Kopplung und Dämpfung
    '
    '
    '
    SqlStmt = "SELECT * FROM TBL_GRUSTART WHERE GKWRT_ID=" & GKwrtID
    CmdDas.CommandText = SqlStmt
    Daset = DataReader(CmdDas, CommandBehavior.Default)


    Do While Daset.Read
      j = Daset("Frart_NR")
      Misch.Kowrt(j) = Daset("Koppwrt")
      Misch.GrStr(0, j) = Daset("start1")
      Misch.GrStr(1, j) = Daset("start2")
      Misch.GrStr(2, j) = Daset("start3")
      Misch.GrStr(3, j) = Daset("start4")
      Misch.GrStr(4, j) = Daset("start5")
      Misch.GrStr(5, j) = Daset("start6")
      Misch.GrDae(0, j) = Daset("dawrt1")
      Misch.GrDae(1, j) = Daset("dawrt2")
      Misch.GrDae(2, j) = Daset("dawrt3")
      Misch.GrDae(3, j) = Daset("dawrt4")
      Misch.GrDae(4, j) = Daset("dawrt5")
      Misch.GrDae(5, j) = Daset("dawrt6")
      Misch.GrKop(0, j) = HandleNullString(Daset("kopp1"))
      Misch.GrKop(1, j) = HandleNullString(Daset("kopp2"))
      Misch.GrKop(2, j) = HandleNullString(Daset("kopp3"))
      Misch.GrKop(3, j) = HandleNullString(Daset("kopp4"))
      Misch.GrKop(4, j) = HandleNullString(Daset("kopp5"))
      Misch.GrKop(5, j) = HandleNullString(Daset("kopp6"))
    Loop
    Daset.Close()
    '
    '

  End Sub
  Sub AufbauRezeptMerk(ByRef ParamAlle As ValuesGrpsAssigns, ByRef ier As Integer)
    Dim k As Integer
    Dim i As Integer
    Dim Ken(17) As String

    '
    '

    '
    'DIN 6164/6167
    '
    '
    'h
    Ken(0) = MenueParam.Menue.StdMrkKen(11)
    'C*
    Ken(1) = MenueParam.Menue.StdMrkKen(10)
    'L*
    Ken(2) = MenueParam.Menue.StdMrkKen(9)
    'a*
    Ken(3) = MenueParam.Menue.StdMrkKen(12)
    'b*
    Ken(4) = MenueParam.Menue.StdMrkKen(13)
    'DH*
    Ken(5) = MenueParam.Menue.StdMrkKen(18)
    'DC*
    Ken(6) = MenueParam.Menue.StdMrkKen(17)
    'DL*
    Ken(7) = MenueParam.Menue.StdMrkKen(16)
    'Da*
    Ken(8) = MenueParam.Menue.StdMrkKen(19)
    'Db*
    Ken(9) = MenueParam.Menue.StdMrkKen(20)
    'Meta
    Ken(10) = MenueParam.Menue.StdMrkKen(14)
    'DE*
    Ken(11) = MenueParam.Menue.StdMrkKen(15)
    'K-De
    Ken(12) = MenueParam.Menue.StdMrkKen(21)
    '
    'X
    Ken(13) = MenueParam.Menue.StdMrkKen(4)
    'Y
    Ken(14) = MenueParam.Menue.StdMrkKen(5)
    'Z
    Ken(15) = MenueParam.Menue.StdMrkKen(6)
    'x
    Ken(16) = MenueParam.Menue.StdMrkKen(7)
    'y
    Ken(17) = MenueParam.Menue.StdMrkKen(8)
    For k = 0 To 1
      ParamAlle.Add(KeyRe(k), New ValuesGrpsAssign)
      ParamAlle(KeyRe(k)).AnwsgID = 0
      ParamAlle(KeyRe(k)).AnwsgName = " "
      If Trim(ParamAlle(KeyRe(k)).AufgArt) = "" OrElse Asc(Mid(ParamAlle(KeyRe(k)).AufgArt, 1, 1)) = 0 Then
        ParamAlle(KeyRe(k)).AufgArt = "@TM$??@PM$??"
      End If
    Next k
    If ConnOpen(Cncol) Then
      For k = 0 To 1
        For i = 0 To UBound(Ken)
          CmdMerk.CommandText = "SELECT * FROM TBL_MERK WHERE MERK_KEN='" & Ken(i) & "'"
          TblMerk = DataReader(CmdMerk, CommandBehavior.SingleRow)
          If TblMerk.Read Then
            'ParamAlle(KeyRe(k)).AddMerk(TblMerk("MERK_KEN"), New Sign(TblMerk("MERK_ID"), TblMerk("MERK_KEN"), TblMerk("MERK_FAKT"), TblMerk("MERK_TYP"), TblMerk("MERK_FORM"), TblMerk("MERK_KBEZ"), i))
            ParamAlle(KeyRe(k)).AddMerk(TblMerk("MERK_KEN"), New Sign(TblMerk("MERK_ID"), TblMerk("MERK_KEN"), TblMerk("MERK_FAKT"), TblMerk("MERK_TYP"), TblMerk("MERK_FORM"), TexKt(10000 + TblMerk("MERK_ID")), i))
          End If
          TblMerk.Close()
        Next i
      Next k
      Cncol.Close()
    End If
  End Sub

  '
  '
  Public Sub New()

    MyBase.New()
    CmdDys = New OleDbCommand
    CmdDas = New OleDbCommand
    CmdMerk = New OleDbCommand
    TableHilf = New DataTable
    TableHilf.TableName = "HILFSTABELLE"
    ViewHilf = New DataView(TableHilf)

    Call StartClear()
    '
    '
    '
    '
    '
    '#######MANKISPEZIAL
    '
    '
    '
    '
    '
    'DE-Eff für Spezialanwendung VW
    '
    '
    If TableExists("TBL_VW_EFFEKT", Cncol) Then
      If FillMenueparTable("SELECT * FROM TBL_VW_EFFEKT", MenueParam.VWTable) Then
        MenueParam.VWDataView = New DataView(MenueParam.VWTable)
      End If
    End If

  End Sub
  Sub CreateUserGroups(UserID As Integer)
    Dim Maxl As Integer
    Dim i As Integer

    'Gruppe R-Werte (mit Readonly und DontShow) 
    's. Grouplist in Colorservice.HandleRwerte
    '
    '
    If FillMenueparTable("SELECT USER_ID,TBL_MESSG_GROUP.MESSG_ID AS MESSG_ID,GROUP_ID,GROUP_KBEZ FROM TBL_USER_MESSG INNER JOIN TBL_MESSG_GROUP ON TBL_USER_MESSG.MESSG_ID =TBL_MESSG_GROUP.MESSG_ID WHERE USER_ID=" & MenueParam.UserID, MenueParam.GroupTableRwert) Then
      '
      'Neue Spalten
      '
      ViewHilf.RowFilter = ""
      TableHilf.Constraints.Clear()
      TableHilf.Columns.Clear()
      If FillMenueparTable("SELECT * FROM TBL_USER_MESSG_GROUP_READONLY WHERE USER_ID=" & UserID, TableHilf) Then
        For i = 0 To MenueParam.GroupTableRwert.Rows.Count - 1
          ViewHilf.RowFilter = "USER_ID=" & MenueParam.GroupTableRwert.Rows(i)("USER_ID") & " AND MESSG_ID=" & MenueParam.GroupTableRwert.Rows(i)("MESSG_ID") & " AND GROUP_ID=" & MenueParam.GroupTableRwert.Rows(i)("GROUP_ID")
          MenueParam.GroupTableRwert.Rows(i)("READ_ONLY") = False
          If ViewHilf.Count > 0 Then
            Maxl = MenueParam.GroupTableRwert.Columns("GROUP_KBEZ").MaxLength
            MenueParam.GroupTableRwert.Rows(i)("GROUP_KBEZ") = ("*" & MenueParam.GroupTableRwert.Rows(i)("GROUP_KBEZ") & Space(Maxl)).substring(0, Maxl).trim
            MenueParam.GroupTableRwert.Rows(i)("READ_ONLY") = True
          End If
        Next i
      End If
      ViewHilf.RowFilter = ""
      TableHilf.Constraints.Clear()
      TableHilf.Columns.Clear()
      If FillMenueparTable("SELECT * FROM TBL_USER_MESSG_GROUP_DONTSHOW WHERE USER_ID=" & UserID, TableHilf) Then
        For i = 0 To MenueParam.GroupTableRwert.Rows.Count - 1
          ViewHilf.RowFilter = "USER_ID=" & MenueParam.GroupTableRwert.Rows(i)("USER_ID") & " AND MESSG_ID=" & MenueParam.GroupTableRwert.Rows(i)("MESSG_ID") & " AND GROUP_ID=" & MenueParam.GroupTableRwert.Rows(i)("GROUP_ID")
          MenueParam.GroupTableRwert.Rows(i)("DONT_SHOW") = False
          If ViewHilf.Count > 0 Then
            MenueParam.GroupTableRwert.Rows(i)("DONT_SHOW") = True
          End If
        Next i
      End If
    End If
    MenueParam.GroupTableRwert.AcceptChanges()
    '
    'Gruppe Rezepte (mit Readonly, DontShow, MessgID und MessgGID aus TBL_MISCH_GROUP_MESSG ) 
    's. Grouplist in Colorservice.HandleRezGroup
    '
    If FillMenueparTable("SELECT USER_ID,TBL_MISCH_GROUP.MISCH_ID AS MISCH_ID,GROUP_ID,GROUP_KBEZ FROM TBL_USER_MISCH INNER JOIN TBL_MISCH_GROUP ON TBL_USER_MISCH.MISCH_ID =TBL_MISCH_GROUP.MISCH_ID WHERE USER_ID=" & MenueParam.UserID, MenueParam.GroupTableRezept) Then
      '
      'Neue Spalten
      '
      ViewHilf.RowFilter = ""
      TableHilf.Constraints.Clear()
      TableHilf.Columns.Clear()
      If FillMenueparTable("SELECT * FROM TBL_USER_MISCH_GROUP_READONLY WHERE USER_ID=" & UserID, TableHilf) Then
        For i = 0 To MenueParam.GroupTableRezept.Rows.Count - 1
          ViewHilf.RowFilter = "USER_ID=" & MenueParam.GroupTableRezept.Rows(i)("USER_ID") & " AND MISCH_ID=" & MenueParam.GroupTableRezept.Rows(i)("MISCH_ID") & " AND GROUP_ID=" & MenueParam.GroupTableRezept.Rows(i)("GROUP_ID")
          MenueParam.GroupTableRezept.Rows(i)("READ_ONLY") = False
          If ViewHilf.Count > 0 Then
            MenueParam.GroupTableRezept.Rows(i)("GROUP_KBEZ") = "*" & MenueParam.GroupTableRezept.Rows(i)("GROUP_KBEZ")
            MenueParam.GroupTableRezept.Rows(i)("READ_ONLY") = True
          End If
        Next i
      End If
      ViewHilf.RowFilter = ""
      TableHilf.Constraints.Clear()
      TableHilf.Columns.Clear()
      If FillMenueparTable("SELECT * FROM TBL_USER_MISCH_GROUP_DONTSHOW WHERE USER_ID=" & UserID, TableHilf) Then
        For i = 0 To MenueParam.GroupTableRezept.Rows.Count - 1
          ViewHilf.RowFilter = "USER_ID=" & MenueParam.GroupTableRezept.Rows(i)("USER_ID") & " AND MISCH_ID=" & MenueParam.GroupTableRezept.Rows(i)("MISCH_ID") & " AND GROUP_ID=" & MenueParam.GroupTableRezept.Rows(i)("GROUP_ID")
          MenueParam.GroupTableRezept.Rows(i)("DONT_SHOW") = False
          If ViewHilf.Count > 0 Then
            MenueParam.GroupTableRezept.Rows(i)("DONT_SHOW") = True
          End If
        Next i
      End If
      '
      'zugeordnete Messgeräte
      '
      '
      '
      ViewHilf.RowFilter = ""
      TableHilf.Constraints.Clear()
      TableHilf.Columns.Clear()

      If FillMenueparTable("SELECT * FROM TBL_MISCH_GROUP_MESSG", TableHilf) Then
        For i = 0 To MenueParam.GroupTableRezept.Rows.Count - 1
          ViewHilf.RowFilter = "MISCH_ID=" & MenueParam.GroupTableRezept.Rows(i)("MISCH_ID") & " AND GROUP_ID=" & MenueParam.GroupTableRezept.Rows(i)("GROUP_ID")
          MenueParam.GroupTableRezept.Rows(i)("MESSG_ID") = -1
          MenueParam.GroupTableRezept.Rows(i)("MESSG_GID") = -1
          If ViewHilf.Count > 0 Then
            MenueParam.GroupTableRezept.Rows(i)("MESSG_ID") = ViewHilf(0)("MESSG_ID")
            MenueParam.GroupTableRezept.Rows(i)("MESSG_GID") = ViewHilf(0)("MESSG_GID")
          End If
        Next i
      End If
    End If
    MenueParam.GroupTableRezept.AcceptChanges()
  
    '
    'Grouplist in Colorservice modifizieren
  End Sub
  Function FillMenueparTable(ByRef SelectCommand As String, ByRef MenueparTable As DataTable) As Boolean
    Dim AllgAdapter As OleDbDataAdapter
    FillMenueparTable = False
    MenueparTable.Rows.Clear()
    'MenueparTable.Constraints.Clear()
    'MenueparTable.Columns.Clear()
    MenueparTable.AcceptChanges()
    AllgAdapter = New OleDbDataAdapter
      AllgAdapter.SelectCommand = New OleDbCommand
      AllgAdapter.SelectCommand.Connection = Cncol()
      AllgAdapter.SelectCommand.CommandText = SelectCommand
      FillMenueparTable = FillDatset(AllgAdapter, MenueparTable)
      MenueparTable.AcceptChanges()
      AllgAdapter.Dispose()
      AllgAdapter = Nothing
  End Function


  Sub dispose() Implements IDisposable.Dispose
    If Not IsNothing(CmdDys) Then
      CmdDys.Dispose()
      CmdDas.Dispose()
      CmdMerk.Dispose()
      CmdDys = Nothing
      CmdDas = Nothing
      CmdMerk = Nothing
    End If
  End Sub
  Sub ClearMenueparameters()
    Call StartClear()
    Call MenueParam.Clear()
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub

End Class