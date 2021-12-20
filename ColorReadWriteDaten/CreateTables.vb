Option Strict Off
Option Explicit On
Option Compare Text

Public Class CreateTables
  Implements IDisposable

  'UPGRADE_WARNING: Arrays in structure dyset may need to be initialized before they can be used. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="814DF224-76BD-4BB4-BFFB-EA359CB9FC48"'
  Dim dyset As OleDbDataReader
  Dim CmdDys As OleDbCommand
  Dim TableText As String
  Dim TabFields() As String
  Dim ForFields() As String
  Dim SqlStmt As String



  Sub New()
    CmdDys = New OleDbCommand
  End Sub
  Private Sub dispose() Implements IDisposable.Dispose
    CmdDys.Dispose()
    'CmdAllg = Nothing
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub

  Sub CreaMisch(ByRef OnUpdDel As String, ByRef ier As Short)

    '

    '
    ier = 0
    '
    '
    '
    '

    '
    'Tabellen für Mischsystem erstellen, falls nicht vorhanden
    '
    '
    'TBL_FARBM
    '
    '
    TableText = MenueParam.TableFarbm
    '
    ' Farbmittel-Datei erstellen
    '
    If Not TableExists(TableText, Cndat) Then
      SqlStmt = "CREATE TABLE " & TableText & " ([MISCH_ID] LONG,[FARBM_ID] LONG,[FARBM_NAME] TEXT (128),[FARBM_ANAME] TEXT (128)," _
      & "[FARBM_DATTIM] DATETIME,[FARBM_ICHF] SHORT,[FARBM_IBAS] SHORT," _
      & "[FARBM_SPZ] SINGLE,[FARBM_EFF] SINGLE,[FARBM_FST] SINGLE,[FARBM_BEL] SINGLE,[FARBM_SMENGE] SINGLE," _
      & "[FARBM_FORMAT] TEXT(12),[FARBM_BEM] TEXT(64),[FARBM_PRNR] TEXT(10),[FARBM_FARBID] LONG)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "FARBM_ID"
      Call CreatePrKey("FARBM_MISCH", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
    End If
    '
    '
    'TBL_FARBM_PREIS 
    '
    '
    TableText = MenueParam.TableFarbmPreis
    '
    ' Farbmittel-Preis-Datei für Mischsystem erstellen
    '
    If Not TableExists(TableText, Cndat) Then
      SqlStmt = "CREATE TABLE " & TableText & " ([MISCH_ID] LONG,[FARBM_ID] LONG," _
      & "[FARBM_IRPA] SHORT,[FARBM_PREIS] SINGLE)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If

      '
      'Primary Key
      '
      '
      '
      '

      Erase TabFields
      ReDim TabFields(2)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "FARBM_ID"
      TabFields(2) = "FARBM_IRPA"
      Call CreatePrKey("FARBM_IRPA_ID", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      '
      '
      '
      '
      'Foreign key TBL_FARBM und TBL_FARBM_PREIS
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "FARBM_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MISCH_ID"
      ForFields(1) = "FARBM_ID"


      '
      Call CreateRelat("FARBM_PREIS_ID", MenueParam.TableFarbm, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub
    End If

    '
    'TBL_FARBM_PROZ
    '
    '
    TableText = MenueParam.TableFarbmProz
    '
    ' Farbmittel-Proz-Datei für Mischsystem erstellen
    '
    If Not TableExists(TableText, Cndat) Then
      SqlStmt = "CREATE TABLE " & TableText & " ([MISCH_ID] LONG,[FARBM_ID] LONG," _
      & "[FARBM_IRFA] SHORT,[FARBM_PROZ] SINGLE)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If

      '
      'Primary Key
      '
      '
      '
      '

      Erase TabFields
      ReDim TabFields(2)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "FARBM_ID"
      TabFields(2) = "FARBM_IRFA"
      Call CreatePrKey("FARBM_IRFA_ID", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      '
      '
      '
      '
      'Foreign key TBL_FARBM und TBL_FARBM_PROZ
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "FARBM_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MISCH_ID"
      ForFields(1) = "FARBM_ID"


      '
      Call CreateRelat("FARBM_PROZ_ID", MenueParam.TableFarbm, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub
    End If

    '

    '
    'TBL_FARBM_PROB
    '
    '
    TableText = MenueParam.TableFarbmProb
    '
    ' Farbmittel-Prob-Datei für Mischsystem erstellen
    '
    If Not TableExists(TableText, Cndat) Then
      SqlStmt = "CREATE TABLE " & TableText & " ([MISCH_ID] LONG,[FARBM_ID] LONG," _
      & "[FARBM_IRBA] SHORT,[FARBM_PROB] SINGLE)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If

      '
      'Primary Key
      '
      '
      '
      '

      Erase TabFields
      ReDim TabFields(2)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "FARBM_ID"
      TabFields(2) = "FARBM_IRBA"
      Call CreatePrKey("FARBM_IRBA_ID", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      '
      '
      '
      '
      'Foreign key TBL_FARBM und TBL_FARBM_PROZ
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "FARBM_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MISCH_ID"
      ForFields(1) = "FARBM_ID"


      '
      Call CreateRelat("FARBM_PROB_ID", MenueParam.TableFarbm, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub



    End If
    '
    '
    '
    '
    '
    'TBL_GRUND_FARBM   
    '
    '
    ' Rezept-FARBM-GrunDbUseen Datei für Mischsystem erstellen
    '
    '
    TableText = MenueParam.TableGrundFarbm
    If Not TableExists(MenueParam.TableGrundFarbm, Cndat) Then
      SqlStmt = "CREATE TABLE " & TableText & " ([MISCH_ID] LONG,[FARBM_ID] LONG,[MESSGRW_ID] LONG,[MESSG_ID] LONG," _
      & "[GKWRT_ID] LONG,[FARBM_DATTIM] DATETIME, [FARBM_FIX] SHORT,[FARBM_NPX] SHORT,[FARBM_NST] SHORT," _
      & "[FARBM_CST] LONGBINARY,[FARBM_ABSSTR] LONGBINARY)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If

      '
      'Primary Key
      '
      '
      '
      '

      Erase TabFields
      ReDim TabFields(3)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "MESSGRW_ID"
      TabFields(2) = "FARBM_ID"
      TabFields(3) = "GKWRT_ID"
      Call CreatePrKey("GRUND_FARBM_ID", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      '
      'Foreign key TBL_FARBM und TBL_GRUND_FARBM
      '
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "FARBM_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MISCH_ID"
      ForFields(1) = "FARBM_ID"


      '
      Call CreateRelat("FARBM_GRUND", MenueParam.TableFarbm, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub
    End If
    '
    '
    '
    '
    'TBL_SORTI 
    '
    '
    '
    TableText = MenueParam.TableSorti
    '
    ' Sortimente-Datei für Mischsystem erstellen
    '
    '
    If Not TableExists(TableText, Cndat) Then
      SqlStmt = "CREATE TABLE " & MenueParam.TableSorti & " ([MISCH_ID] LONG,[SORTI_ID] LONG," _
      & "[SORTI_NAME] TEXT (64),[SORTI_BEM] TEXT (64)," & "[SORTI_DATTIM] DATETIME," _
      & "[USER_ID] LONG,[SORTI_IVOL] SHORT,[SORTI_INF] SHORT,[SORTI_INM] SHORT," _
      & "[SORTI_INO] SHORT,[SORTI_MNGMIN] SINGLE,[SORTI_MNGMAX] SINGLE," _
      & "[SORTI_INP] SHORT,[SORTI_INQ] SHORT,[SORTI_PROZMIN] SINGLE,[SORTI_PROZMAX] SINGLE," _
      & "[SORTI_DIK1] SINGLE,[SORTI_DIK2] SINGLE)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If
      '
      '
      'Primary Key
      '
      '
      '
      '

      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "SORTI_ID"
      Call CreatePrKey("SORTI_MISCH", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
    End If
    '
    'TBL_SORTI_FARBM 
    '
    '
    TableText = MenueParam.TableSortiFarbm
    '
    '
    ' Sortiment-Farbm-Datei für Mischsystem erstellen
    '
    '
    If Not TableExists(TableText, Cndat) Then
      SqlStmt = "CREATE TABLE " & TableText & " ([MISCH_ID] LONG,[SORTI_ID] LONG," _
      & "[FARBM_ID] LONG,[FARBM_MENGE] SINGLE," & "[FARBM_LIMMNG] SINGLE,[FARBM_OPERAT] TEXT(1)," _
      & "[FARBM_TOPF] TEXT(1),[FARBM_PREIS] SINGLE,[FARBM_PROZ] SINGLE,[FARBM_PROB] SINGLE,[FARBM_IPOS] BYTE)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If
      '
      '
      'Primary Key
      '
      '
      '
      '
      Erase TabFields
      ReDim TabFields(2)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "SORTI_ID"
      TabFields(2) = "FARBM_ID"
      Call CreatePrKey("SORTI_MISCH_FARBM", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      '
      '
      '
      '
      'Foreign key TBL_SORTI und TBL_SORTI_FARBM
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "SORTI_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MISCH_ID"
      ForFields(1) = "SORTI_ID"


      '
      Call CreateRelat("SORTI_FARBM", MenueParam.TableSorti, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      'Foreign key TBL_FARBM und TBL_SORTI_FARBM
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "FARBM_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MISCH_ID"
      ForFields(1) = "FARBM_ID"


      '
      Call CreateRelat("FARBM_SORTI", MenueParam.TableFarbm, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub

    End If
    '
    '
    '
    'TBL_SORTI_RWERT
    '
    '
    TableText = MenueParam.TableSortiRwert
    '
    '
    'Sortiment-Rwert-Datei für Mischsystem erstellen
    '
    '
    If Not TableExists(TableText, Cndat) Then
      '
      '
      '    KWB = -1 R-Werte weißer Untergrund
      '    KWB = -2 R-Werte schwarzer Untergrund
      '    KWB = 11  R-Werte Farbmittelschicht über weißem Untergrund (Vorlage)
      '    KWB = 12 R-Werte Farbmittelschicht über schwarzem Untergrund (Vorlage)

      SqlStmt = "CREATE TABLE " & TableText & " ([MISCH_ID] LONG,[SORTI_ID] LONG ," _
      & "[MESSGRW_ID] LONG,[RWERT_ID] LONG,[RWERT_KWB] SHORT)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If
      '
      '
      'Primary Key
      '
      '
      '
      '
      Erase TabFields
      ReDim TabFields(3)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "MESSGRW_ID"
      TabFields(2) = "SORTI_ID"
      TabFields(3) = "RWERT_KWB"
      Call CreatePrKey("SORTI_MISCH_RWERT", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      '
      '
      '
      '
      'Foreign key TBL_SORTI und TBL_SORTI_RWERT
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "SORTI_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MISCH_ID"
      ForFields(1) = "SORTI_ID"


      '
      Call CreateRelat("SORTI_RWERT", MenueParam.TableSorti, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      '
      '
      '
      'Foreign key TBL_RWERT und TBL_SORTI_RWERT
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MESSGRW_ID"
      TabFields(1) = "RWERT_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MESSGRW_ID"
      ForFields(1) = "RWERT_ID"


      '
      Call CreateRelat("RWERT_SORTI", MenueParam.TableRwert, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub
    End If        '

    '
    '
    '
    '
    'TBL_REZEPT
    '
    '
    '
    TableText = MenueParam.TableRezept
    '
    'Rezept-Datei für Mischsystem erstellen
    '
    '
    If Not TableExists(TableText, Cndat) Then
      SqlStmt = "CREATE TABLE " & TableText & " ([MISCH_ID] LONG,[REZEPT_ID] LONG ,[REZEPT_GID] LONG," _
      & "[REZEPT_NAME] TEXT (64),[REZEPT_BEM] TEXT (64)," _
      & "[REZEPT_DATTIM] DATETIME,[REZEPT_IARCH] SHORT,[REZEPT_IVOL] SHORT," _
      & "[REZEPT_INF] SHORT,[REZEPT_INM] SHORT,[REZEPT_INO] SHORT,[REZEPT_MNGMIN] SINGLE,[REZEPT_MNGMAX] SINGLE," _
      & "[REZEPT_INP] SHORT,[REZEPT_INQ] SHORT,[REZEPT_PROZMIN] SINGLE,[REZEPT_PROZMAX] SINGLE," & "[REZEPT_DIK1] SINGLE,[REZEPT_DIK2] SINGLE)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If
      '
      '
      'Primary Key
      '
      '
      '
      '

      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "REZEPT_ID"
      Call CreatePrKey("REZEPT_MISCH", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      'Index für RWERT-GID
      '
      '
      Erase TabFields
      ReDim TabFields(0)
      TabFields(0) = "REZEPT_GID"
      Call CreateIndex("REZEPT_GROUP", TableText, TabFields, False, ier, Cndat)
      If ier > 0 Then Exit Sub
    End If
    '
    'TBL_REZEPT_FARBM 
    '
    '
    TableText = MenueParam.TableRezeptFarbm
    '
    '
    'Rezept-Farbm-Datei für Mischsystem erstellen
    '
    '
    If Not TableExists(TableText, Cndat) Then
      SqlStmt = "CREATE TABLE " & TableText & " ([MISCH_ID] LONG,[REZEPT_ID] LONG," _
      & "[FARBM_ID] LONG,[FARBM_MENGE] SINGLE," _
      & "[FARBM_LIMMNG] SINGLE,[FARBM_OPERAT] TEXT(1)," _
      & "[FARBM_PREIS] SINGLE,[FARBM_PROZ] SINGLE,[FARBM_PROB] SINGLE,[FARBM_IPOS] BYTE)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If
      '
      '
      'Primary Key
      '
      '
      '
      '
      Erase TabFields
      ReDim TabFields(2)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "REZEPT_ID"
      TabFields(2) = "FARBM_ID"
      Call CreatePrKey("REZEPT_MISCH_FARBM", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      '
      '
      '
      '
      'Foreign key TBL_REZEPT und TBL_REZEPT_FARBM
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "REZEPT_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MISCH_ID"
      ForFields(1) = "REZEPT_ID"


      '
      Call CreateRelat("REZEPT_FARBM", MenueParam.TableRezept, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      'Foreign key TBL_FARBM und TBL_REZEPT_FARBM
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "FARBM_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MISCH_ID"
      ForFields(1) = "FARBM_ID"


      '
      Call CreateRelat("FARBM_REZEPT", MenueParam.TableFarbm, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub

    End If
    '
    '
    '
    'TBL_REZEPT_RWERT
    '
    '
    TableText = MenueParam.TableRezeptRwert
    '
    '
    'Rezept-Rwert-Datei für Mischsystem erstellen
    '
    '
    If Not TableExists(TableText, Cndat) Then
      '
      '
      '    KWB = -1 R-Werte weißer Untergrund
      '    KWB = -2 R-Werte schwarzer Untergrund
      '    KWB = 1  R-Werte Farbmittelschicht über weißem Untergrund (Nachstellung)
      '    KWB = 2  R-Werte Farbmittelschicht über schwarzem Untergrund (Nachstellung)
      '    KWB = 11  R-Werte Farbmittelschicht über weißem Untergrund (Vorlage)
      '    KWB = 12 R-Werte Farbmittelschicht über schwarzem Untergrund (Vorlage)
      '

      SqlStmt = "CREATE TABLE " & TableText & " ([MISCH_ID] LONG,[REZEPT_ID] LONG ," _
      & "[MESSGRW_ID] LONG,[RWERT_ID] LONG,[RWERT_KWB] SHORT)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If
      '
      '
      'Primary Key
      '
      '
      '
      '
      Erase TabFields
      ReDim TabFields(3)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "MESSGRW_ID"
      TabFields(2) = "REZEPT_ID"
      TabFields(3) = "RWERT_KWB"
      Call CreatePrKey("REZEPT_MISCH_RWERT", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      '
      '
      '
      '
      'Foreign key TBL_REZEPT und TBL_REZEPT_RWERT
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MISCH_ID"
      TabFields(1) = "REZEPT_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MISCH_ID"
      ForFields(1) = "REZEPT_ID"


      '
      Call CreateRelat("REZEPT_RWERT", MenueParam.TableRezept, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      '
      '
      '
      '
      'Foreign key TBL_RWERT und TBL_REZEPT_RWERT
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MESSGRW_ID"
      TabFields(1) = "RWERT_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MESSGRW_ID"
      ForFields(1) = "RWERT_ID"


      '
      Call CreateRelat("RWERT_REZEPT", MenueParam.TableRwert, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub
    End If
    '

  End Sub
  Sub CreaRwrt(ByRef OnUpdDel As String, ByRef ier As Short)

    '
    ier = 0
    '
    '

    '
    'TBL_RWERT
    '
    TableText = MenueParam.TableRwert
    '
    ' R-Wert-Datei für Messgerät erstellen
    '

    If Not TableExists(TableText, Cndat) Then
      SqlStmt = "CREATE TABLE " & TableText & " ([MESSGRW_ID] LONG,[RWERT_ID] LONG ,[MESSG_ID] LONG,[USER_ID] LONG,[RWERT_GID] LONG," _
      & "[RWERT_NAME] TEXT (64),[RWERT_BEM] TEXT (64),[RWERT_KENN] TEXT(32)," _
      & "[RWERT_IARCH] SHORT,[RWERT_DATTIM] DATETIME," _
      & "[RWERT_CME] TEXT (2),[RWERT_RETR] SHORT," _
      & "[RWERT_IAMI] SHORT,[RWERT_DE] LONGBINARY," & "[RWERT_RWERT] LONGBINARY)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If
      '
      '
      'Primary Key
      '
      '
      '
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MESSGRW_ID"
      TabFields(1) = "RWERT_ID"
      Call CreatePrKey("RWERT_MESSGRW", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      'Index für RWERT-GID
      '
      '
      Erase TabFields
      ReDim TabFields(0)
      TabFields(0) = "RWERT_GID"
      Call CreateIndex("RWERT_GROUP", TableText, TabFields, False, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      'Index für RWERT-DATE
      '
      '
      Erase TabFields
      ReDim TabFields(0)
      TabFields(0) = "RWERT_DATTIM"
      Call CreateIndex("RWERT_DATETIME", TableText, TabFields, False, ier, Cndat)
      If ier > 0 Then Exit Sub

    End If
    '
    '
    '
    'TBL_Quali  
    TableText = MenueParam.TableQuali
    '
    '
    '
    ' Datei für Qualtitätsangaben (Bezug oder Probe Mengen)
    '
    '
    '
    If Not TableExists(TableText, Cndat) Then

      '
      SqlStmt = "CREATE TABLE " & MenueParam.TableQuali & " ([MESSGRW_ID] LONG,[QUALI_ID] LONG," _
      & "[METH_ID] LONG,[QUALI_CART] TEXT (4)," _
      & "[QUALI_MEDNR] SHORT,[QUALI_IUNT] LONG,[QUALI_DATTIM] DATETIME,[QUALI_CAMP] LONGBINARY)"
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If
      '
      '
      'Primary Key
      '
      '
      '
      '
      Erase TabFields
      ReDim TabFields(2)
      TabFields(0) = "MESSGRW_ID"
      TabFields(1) = "QUALI_ID"
      TabFields(2) = "METH_ID"

      Call CreatePrKey("RWERT_MESSGRW", TableText, TabFields, ier, Cndat)
      If ier > 0 Then Exit Sub
      '
      '
      'Foreign key TBL_RWERT und TBL_QUALI
      '
      'TabFields
      '
      Erase TabFields
      ReDim TabFields(1)
      TabFields(0) = "MESSGRW_ID"
      TabFields(1) = "RWERT_ID"
      '
      'ForeignFields
      '
      Erase ForFields
      ReDim ForFields(1)
      ForFields(0) = "MESSGRW_ID"
      ForFields(1) = "QUALI_ID"


      '
      Call CreateRelat("RWERT_QUALI", MenueParam.TableRwert, TableText, TabFields, ForFields, OnUpdDel, OnUpdDel, ier, Cndat)
      If ier > 0 Then Exit Sub

    End If

    '
    'TBL_KALIB
    '
    TableText = MenueParam.TableKALIB
    '
    ' R-Wert-Datei für Messgerät erstellen
    '

    If Not TableExists(TableText, Cnkal) Then
      SqlStmt = "CREATE TABLE " & TableText & " ([MESSG_ID] LONG ,[ART_ID] LONG,[MESSG_KENN] TEXT(2),[KALIB_BEZ] TEXT(30),[KALIB_DATTIM] DATETIME,[KALIB_RWERT] LONGBINARY) "
      CmdDys.CommandText = SqlStmt
      If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
        ier = -1
        Exit Sub
      End If
      '
      '
      'Primary Key
      '
      '
      '
      '
      Erase TabFields
      ReDim TabFields(2)
      TabFields(0) = "MESSG_ID"
      TabFields(1) = "ART_ID"
      TabFields(2) = "MESSG_KENN"
      Call CreatePrKey("KALIB_MESSG", TableText, TabFields, ier, Cnkal)
      If ier > 0 Then Exit Sub
      '
      '
    End If

  End Sub
End Class