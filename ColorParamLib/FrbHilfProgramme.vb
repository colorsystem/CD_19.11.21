Option Strict Off
Option Explicit On
Option Compare Text
Public Class FrbHilfProgramme
  Implements IDisposable
  Private Shared CmdDhs As OleDbCommand
  Private Shared dhset As OleDbDataReader
  Shared Sub New()
    CmdDhs = New OleDbCommand
  End Sub




  Sub dispose() Implements IDisposable.Dispose
    CmdDhs.Dispose()
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub

  'Sub getwsol(ByRef Dbas As OleDbConnection, ByRef TabName As String, ByRef NormFileID As Short, ByRef Nwe As Short, ByRef Nwp As Short, ByRef Rhilf() As Single, ByRef ier As Short)
  '   Call GETNwe(Dbas, NormFileID, TabName, Nwe, Nwp, ier)
  '  ReDim Rhilf(Nwe)
  ' Call GETNormfa(Dbas, 0, TabName, Nwe, Rhilf)
  'End Sub
  Shared Sub ItextGrund(ByRef CDE As String, ByRef IText() As Integer)
    Dim i As Short
    Dim Kcme As Short
    For i = 0 To 12
      If Trim(CDE) = ArtCME(i) Then
        Kcme = i
        Exit For
      End If
    Next i
    '
    'CME=D0,T0
    '
    '
    Select Case Kcme
      '
      'CME=D0,T0
      '
      '
      Case 0, 1
        ReDim IText(1)
        IText(0) = 956
        IText(1) = 957
        '
        '
        'CME=E0,S0
        '
        '
      Case 2, 3
        ReDim IText(5)
        IText(0) = 956
        IText(1) = 957
        IText(2) = 958
        IText(3) = 959
        IText(4) = 960
        IText(5) = 961
        '
        '
        'X0,H0
        '
      Case 4, 5
        ReDim IText(3)
        IText(0) = 962
        IText(1) = 963
        IText(2) = 964
        IText(3) = 965
        '
        'CME=V0
        '
        '
      Case 6
        ReDim IText(2)
        IText(0) = 956
        IText(1) = 957
        IText(2) = 962
        '
        '
        '
        'CME=I0,Y0
        '
        '
      Case 7, 8
        ReDim IText(3)
        IText(0) = 956
        IText(1) = 957
        IText(2) = 964
        IText(3) = 965
        'CME=F0,R0

        '
      Case 9, 10
        ReDim IText(3)
        IText(0) = 956
        IText(1) = 957
        IText(2) = 960
        IText(3) = 961
        '
        'CME=G0,W0
        '
        '
      Case 11, 12
        ReDim IText(2)
        IText(0) = 956
        IText(1) = 957
        IText(2) = 966
    End Select
    '

  End Sub

  Shared Function NopNPX(ByRef CDE As String) As Short
    Dim Cde1 As String
    '
    '
    '
    '
    '
    '
    'Bestimmung von Npx
    '
    '
    '
    'Npx Anzahl Parameter pro Farb/Bindemittel
    'Npf Anzahl Absorptions- und Streukoeffizienten
    '
    '
    '
    NopNPX = 0
    Cde1 = CDE.Substring(0, 1)
    Select Case Cde1
      Case "U", "C"
        '
        'A,S-abhängige Grunddaten
        '
        '
        NopNPX = 10

      Case "T", "D"
        '
        '2-Konstanten
        '
        '
        NopNPX = 2
      Case "V"
        '
        '
        '
        '
        NopNPX = 3
      Case "X", "H"
        '
        '
        'konz. abhängig
        '
        '
        NopNPX = 2
      Case "Y", "I"
        '
        '
        'konz. abhängig
        '
        '
        NopNPX = 2
      Case "S", "E"
        '
        '
        '4-Fluß-Theorie
        '
        '
        '
        NopNPX = 6
      Case "F", "R"
        '
        '
        '4-Parameter
        '
        '
        '
        NopNPX = 4
      Case "G", "W"
        '
        '
        'Transversal
        '
        '
        '
        NopNPX = 3
    End Select

  End Function
  Shared Function NPNPS(CDE As String, Npx As Integer, NST As Integer) As Integer
    '
    'Anzahl Grunddaten pro Farbmittel,Winkel und Wellenlänge
    '
    NPNPS = Npx
    If CnzDep(CDE) = 1 Then
      NPNPS = NST + 1
    ElseIf CnzDep(CDE) = 2 Then
      NPNPS = 2 * NST
    End If
  End Function

  Shared Function Transp(ByRef CDE As String) As Boolean
    Dim Ih As Short
    '
    '
    'Bestimmung von IdeTr
    '
    '
    '
    Transp = False
    Ih = Asc(CDE.Substring(0, 1))
    '
    'CDE(0) zwischen "O" und "Z" (= Transparent oder transluzent)
    'sonst = deckend
    '
    '
    '
    If Ih >= 79 And Ih <= 90 Then
      Transp = True
    End If
    ''

  End Function

  Shared Function CnzDep(CDE As String) As Integer
    Dim CCC As String
    '
    '
    CnzDep = 0
    '
    '     PRUEFEN, OB CDE(1:1)='X','H','Y','I'
    '     D.H.KONZENTRATIONSABHÄNGIG
    '
    CCC = Mid(CDE, 1, 1)
    If CCC = "X" Or CCC = "H" Then
      CnzDep = 1
    ElseIf CCC = "Y" Or CCC = "I" Then
      CnzDep = 2
    End If
  End Function

  Shared Function ArtCME(ByRef i As Short) As String
    ArtCME = "  "
    Select Case i
      Case 0
        ArtCME = "D0"
      Case 1
        ArtCME = "T0"
      Case 2
        ArtCME = "E0"
      Case 3
        ArtCME = "S0"
      Case 4
        ArtCME = "H0"
      Case 5
        ArtCME = "X0"
      Case 6
        ArtCME = "V0"
      Case 7
        ArtCME = "I0"
      Case 8
        ArtCME = "Y0"
      Case 9
        ArtCME = "F0"
      Case 10
        ArtCME = "R0"
      Case 11
        ArtCME = "G0"
      Case 12
        ArtCME = "W0"
      Case 13
        ArtCME = "C0"
      Case 14
        ArtCME = "U0"
    End Select
  End Function








  Shared Sub GETNwe(ByRef BereichID As Integer, ByRef Nwe As Short, ByRef Nwp As Short, ByRef WellStart As Short, ByRef WellEnde As Short, ByRef WellStep As Short, ByRef ier As Short)
    'UPGRADE_WARNING: Arrays in structure Dyset may need to be initialized before they can be used. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="814DF224-76BD-4BB4-BFFB-EA359CB9FC48"'
    Dim SqlStmt As String
    If BereichID < 0 Then
      ier = -40
      Nwe = 0
      Exit Sub
    End If
    SqlStmt = "SELECT * FROM TBL_BEREICH WHERE BEREICH_ID=" & BereichID
    CmdDhs.CommandText = SqlStmt
    dhset = DataReader(CmdDhs, CommandBehavior.SingleRow Or CommandBehavior.CloseConnection)
    If dhset.Read Then
      Nwe = dhset("WELL_nwe")
      Nwp = dhset("WELL_nwp")
      If WellStart = 0 Then
        If WellStart > dhset("WELL_Start") Then
          ier = 4167
          dhset.Close()
          Exit Sub
        End If
        WellStart = dhset("WELL_Start")
      End If
      If WellEnde = 0 Then
        If WellEnde > dhset("WELL_Ende") Then
          ier = 4168
          dhset.Close()
          Exit Sub
        End If
        WellEnde = dhset("WELL_Ende")
      End If
      WellStep = dhset("WELL_Step")
    End If
    dhset.Close()
  End Sub
  Shared Sub GETWsol(ByRef BereichID As Integer, ByRef WellStart As Short, ByRef WellEnde As Short, ByRef WellStep As Short, ByRef Rhilf() As Single, ByRef ier As Short)
    Dim hlf() As Byte
    Dim SqlStmt As String

    SqlStmt = "SELECT * FROM TBL_LICHT_BEREICH_SPEK WHERE [BEREICH_ID]=" & BereichID & " AND XYZ_ID=0"
    CmdDhs.CommandText = SqlStmt
    dhset = DataReader(CmdDhs, CommandBehavior.SingleRow Or CommandBehavior.CloseConnection)
    If dhset.Read() Then
      hlf = dhset("LICHT_SPEK")
      Rhilf = GetSingles(hlf)
    End If
    dhset.Close()
    If WellStep <> Rhilf(1) - Rhilf(0) Then
      ier = 4169
      Exit Sub
    End If
    '
    ''
  End Sub
  Shared Function WinUSeqMe(ByRef WinUse As AngGeos, ByRef WinMes As AngGeos) As Boolean
    Dim i As Integer
    WinUSeqMe = True
    If WinUse.Km <> WinMes.Km Then
      WinUSeqMe = False
      Exit Function
    End If
    For i = 0 To WinMes.Km - 1
      If WinUse(i).Chrm <> WinMes(i).Chrm Or WinUse(i).Lkm <> WinMes(i).Lkm Then
        WinUSeqMe = False
        Exit Function
      End If
    Next i
  End Function
  '
  '
  '
  Shared Function ReadRezeptOnly(GrpID As Integer) As Boolean
    Dim ViewRezept As New DataView(MenueParam.GroupTableRezept)
    ViewRezept.RowFilter = "USER_ID=" & MenueParam.UserID & " AND MISCH_ID=" & MenueParam.MischID & " AND GROUP_ID=" & GrpID
    If ViewRezept.Count > 0 AndAlso ViewRezept(0)("READ_ONLY") Then
      ReadRezeptOnly = True
    Else
      ReadRezeptOnly = False
    End If
  End Function
  '
  '
  '
  Shared Function ReadRwrtOnly(GrpID As Integer) As Boolean
    Dim ViewRwert As New DataView(MenueParam.GroupTableRwert)
    ViewRwert.RowFilter = "USER_ID=" & MenueParam.UserID & " AND MESSG_ID=" & MenueParam.MessgID & " AND GROUP_ID=" & GrpID
    If ViewRwert.Count > 0 AndAlso ViewRwert(0)("READ_ONLY") Then
      ReadRwrtOnly = True
    Else
      ReadRwrtOnly = False
    End If
  End Function

  Shared Sub ADDCurves(ByRef Kurven As CurvesRef)
    Dim KM As Integer
    Dim NWE As Integer
    Dim kw As Integer
    KM = MenueParam.Messg.Winkel.Km
    NWE = MenueParam.Messg.Winkel.Wsol.Nwe
    Kurven.clear()
    For kw = 0 To KM - 1
      Kurven.Add(MenueParam.Messg.Winkel(kw).Chrm, New CurveRef(NWE))
    Next kw
  End Sub
  
End Class