Option Strict Off
Option Explicit On
Option Compare Text
Public Class AllParameters
  Implements IDisposable

  Dim MnUserID As Integer 'ID User
  Dim MnMessgID As Integer 'ID Messgerät
  Dim MnMischID As Integer 'ID Mischsystem
  Dim MnMethId As Integer 'ID Methode
  Dim MnUntID(5) As Integer 'ID für Untergründe(1=weiß;2=schwarz;3=Weißpigment;4=Schwarzpigment;5=Grauausmischung)
  Dim MnMenue As MenuParameters
  Dim MnNormFa As NormIlluminats
  Dim MnNormFaD6164 As NormIlluminats
  Dim MnMessg As MeasParameters
  Dim MnMisch As MixingParameter
  Dim MnUser As UserParameters
  Dim MnParaMerks As ParamSignAll
  '
  '
  '
  '#######MANKISPEZIAL
  '
  '
  '
  '
  Dim MnVWTable As DataTable
  Dim MnVWDataView As DataView
  Dim MnGroupTableRwert As DataTable
  Dim MnGroupTableRezept As DataTable
  ReadOnly Property GroupTableRwert() As DataTable
    Get
      GroupTableRwert = MnGroupTableRwert
    End Get
    'Set(ByVal value As DataTable)
    '  MnGroupTableRwert = value
    'End Set
  End Property
 readonly  Property GroupTableRezept() As DataTable
    Get
      GroupTableRezept = MnGroupTableRezept
    End Get
    'Set(ByVal value As DataTable)
    '  MnGroupTableRezept = value
    'End Set
  End Property
  ReadOnly Property VWTable() As DataTable
    Get
      VWTable = MnVWTable
    End Get

  End Property
  '
  Property VWDataView() As DataView
    Get
      VWDataView = MnVWDataView
    End Get
    Set(ByVal value As DataView)
      MnVWDataView = value
    End Set
  End Property
  '
  '
  ReadOnly Property User() As UserParameters
    Get
      User = MnUser
    End Get
  End Property
  ReadOnly Property Messg() As MeasParameters
    Get
      Messg = MnMessg
    End Get
  End Property
  ReadOnly Property Misch() As MixingParameter
    Get
      Misch = MnMisch
    End Get
  End Property
  ReadOnly Property Menue() As MenuParameters
    Get
      Menue = MnMenue
    End Get
  End Property
  ReadOnly Property Normfa() As NormIlluminats
    Get
      Normfa = MnNormFa
    End Get
  End Property

  ReadOnly Property NormFaD6164() As NormIlluminats
    Get
      NormFaD6164 = MnNormFaD6164
    End Get
  End Property

  ReadOnly Property ParaMerks() As ParamSignAll
    Get
      ParaMerks = MnParaMerks
    End Get
  End Property
  '
  '
  '
  '
  '
  ReadOnly Property TableRwert() As String
    Get 'Dateiname für Reflexionswerte
      TableRwert = "TBL_RWERT"
    End Get
  End Property
  ReadOnly Property TableQuali() As String
    Get 'Dateiname für Mengen usw.
      TableQuali = "TBL_QUALI"
    End Get
  End Property
  ReadOnly Property TableKALIB() As String
    Get 'Dateiname für Mengen usw.
      TableKALIB = "TBL_KALIB"
    End Get
  End Property
  ReadOnly Property TableFarbm() As String
    Get 'Dateiname für Farbmittel
      TableFarbm = "TBL_FARBM"
    End Get
  End Property
  ReadOnly Property TableFarbmPreis() As String
    Get 'Dateiname für Farbmittelpreise
      TableFarbmPreis = "TBL_FARBM_PREIS"
    End Get
  End Property
  ReadOnly Property TableFarbmProz() As String
    Get 'Dateiname für Farbmittelkonzentrationen
      TableFarbmProz = "TBL_FARBM_PROZ"
    End Get
  End Property
  ReadOnly Property TableFarbmProb() As String
    Get 'Dateiname für Bindemittelkonzentrationen im Farbmittelbatch
      TableFarbmProb = "TBL_FARBM_PROB"
    End Get
  End Property
  ReadOnly Property TableSorti() As String
    Get 'Dateiname für Sortiment
      TableSorti = "TBL_SORTI"
    End Get
  End Property
  ReadOnly Property TableSortiFarbm() As String
    Get 'Dateiname für Sortiment
      TableSortiFarbm = "TBL_SORTI_FARBM"
    End Get
  End Property
  ReadOnly Property TableSortiRwert() As String
    Get 'Dateiname für Sortiment-Rwert Kombination
      TableSortiRwert = "TBL_SORTI_RWERT"
    End Get
  End Property

  ReadOnly Property TableRezept() As String
    Get 'Dateiname für Rezepte
      TableRezept = "TBL_REZEPT"
    End Get
  End Property
  ReadOnly Property TableRezeptFarbm() As String
    Get 'Dateiname für Rezept-Farbmittelkombinationen
      TableRezeptFarbm = "TBL_REZEPT_FARBM"
    End Get
  End Property
  ReadOnly Property TableRezeptRwert() As String
    Get 'Datei Rezept-Rwert Kombination
      TableRezeptRwert = "TBL_REZEPT_RWERT"
    End Get
  End Property
  ReadOnly Property TableGrundFarbm() As String
    Get 'Datei Farbmittel-Grunddaten
      TableGrundFarbm = "TBL_GRUND_FARBM"
    End Get
  End Property







  ReadOnly Property TblRwert() As String
    Get 'Dateiname für Reflexionswerte
      TblRwert = "R_R_" & MnMessg.Tbl
    End Get
  End Property
  ReadOnly Property TblQuali() As String
    Get 'Dateiname für Mengen usw.
      TblQuali = "QU_" & MnMessg.Tbl
    End Get
  End Property
  ReadOnly Property TblKalib() As String
    Get 'Dateiname für Kalibrierwerte
      TblKalib = "KLB_" & MnMessg.Tbl
    End Get
  End Property
  ReadOnly Property TblFarbm() As String
    Get 'Dateiname für Farbmittel
      TblFarbm = "F_F_" & MnMisch.Tbl
    End Get
  End Property
  ReadOnly Property TblFarbmPreis() As String
    Get 'Dateiname für Farbmittelpreise
      TblFarbmPreis = "F_PR_" & MnMisch.Tbl
    End Get
  End Property
  ReadOnly Property TblFarbmProz() As String
    Get 'Dateiname für Farbmittelkonzentrationen
      TblFarbmProz = "F_PZ_" & MnMisch.Tbl
    End Get
  End Property
  ReadOnly Property TblFarbmProb() As String
    Get 'Dateiname für Bindemittelkonzentrationen im Farbmittelbatch
      TblFarbmProb = "F_PB_" & MnMisch.Tbl
    End Get
  End Property
  ReadOnly Property TblSorti() As String
    Get 'Dateiname für Sortiment
      TblSorti = "S_S_" & MnMisch.Tbl
    End Get
  End Property
  ReadOnly Property TblSortiFarbm() As String
    Get 'Dateiname für Sortiment
      TblSortiFarbm = "S_F_" & MnMisch.Tbl
    End Get
  End Property
  ReadOnly Property TblSortiRwert() As String
    Get 'Dateiname für Sortiment-Rwert Kombination
      TblSortiRwert = "S_R_" & MnMisch.Tbl & "_" & MnMessg.Tbl
    End Get
  End Property
  ReadOnly Property TblUserFarbm() As String
    Get 'Dateiname für Farmittel mit User-spezifischen Angaben
      TblUserFarbm = ""
    End Get
  End Property
  ReadOnly Property TblRezept() As String
    Get 'Dateiname für Rezepte
      TblRezept = "RZ_RZ_" & MnMisch.Tbl
    End Get
  End Property
  ReadOnly Property TblRezeptFarbm() As String
    Get 'Dateiname für Rezept-Farbmittelkombinationen
      TblRezeptFarbm = "RZ_F_" & MnMisch.Tbl
    End Get
  End Property
  ReadOnly Property TblRezeptRwert() As String
    Get 'Datei Rezept-Rwert Kombination
      TblRezeptRwert = "RZ_R_" & MnMisch.Tbl & "_" & MnMessg.Tbl
    End Get
  End Property
  ReadOnly Property TblGrundFarbm() As String
    Get 'Datei Farbmittel-Grunddaten
      TblGrundFarbm = "GR_F_" & MnMisch.Tbl & "_" & MnMessg.Tbl
    End Get
  End Property

  '
  '
  '
  ReadOnly Property UserID() As Integer
    Get
      UserID = MnUserID
    End Get
  End Property
  WriteOnly Property AufUserID() As Integer

    Set(ByVal Value As Integer)
      MnUserID = Value
    End Set
  End Property
  ReadOnly Property MessgID() As Integer
    Get
      MessgID = MnMessgID
    End Get
  End Property
  WriteOnly Property AufMessgID() As Integer
    Set(ByVal Value As Integer)
      MnMessgID = Value
    End Set
  End Property

  ReadOnly Property MischID() As Integer
    Get
      MischID = MnMischID
    End Get
  End Property
  WriteOnly Property AufMischID() As Integer
    Set(ByVal Value As Integer)
      MnMischID = Value
    End Set
  End Property
  ReadOnly Property MethID() As Integer
    Get
      MethID = MnMethId
    End Get
  End Property
  WriteOnly Property AufMethID() As Integer
    Set(ByVal Value As Integer)
      MnMethId = Value
    End Set
  End Property
  Property UntID(ByVal i As Short) As Integer
    Get
      UntID = MnUntID(i)
    End Get
    Set(ByVal Value As Integer)
      MnUntID(i) = Value
    End Set
  End Property
  Sub Clear()
    Dim i As Short
    MnUserID = -1
    MnMessgID = -1
    MnMischID = -1
    MnMethId = -1
    MnMessg.BereichID = -1
    MnNormFa.clear()
    MnMessg.Winkel.clear()
    MnUser.Winkel.clear()
    For i = 0 To 5
      MnUntID(i) = -1
    Next i
  End Sub


  Public Sub New()
    MyBase.New()
    MnParaMerks = New ParamSignAll
    MnMenue = New MenuParameters
    MnNormFa = New NormIlluminats
    MnNormFaD6164 = New NormIlluminats
    MnMessg = New MeasParameters
    MnMisch = New MixingParameter
    MnUser = New UserParameters
    MnVWTable = New DataTable
    MnGroupTableRwert = New DataTable
    MnGroupTableRezept = New DataTable
    MnGroupTableRwert.TableName = "GROUPRWERT"
    MnGroupTableRwert.Columns.Add("USER_ID", GetType(Integer))
    MnGroupTableRwert.Columns.Add("MESSG_ID", GetType(Integer))
    MnGroupTableRwert.Columns.Add("GROUP_ID", GetType(Integer))
    MnGroupTableRwert.Columns.Add("GROUP_KBEZ", GetType(String))
    MnGroupTableRwert.Columns.Add("READ_ONLY", GetType(Boolean))
    MnGroupTableRwert.Columns.Add("DONT_SHOW", GetType(Boolean))
    '
    '
    '
    MnGroupTableRezept.TableName = "GROUPREZEPT"
    MnGroupTableRezept.Columns.Add("USER_ID", GetType(Integer))
    MnGroupTableRezept.Columns.Add("MISCH_ID", GetType(Integer))
    MnGroupTableRezept.Columns.Add("GROUP_ID", GetType(Integer))
    MnGroupTableRezept.Columns.Add("GROUP_KBEZ", GetType(String))
    MnGroupTableRezept.Columns.Add("READ_ONLY", GetType(Boolean))
    MnGroupTableRezept.Columns.Add("DONT_SHOW", GetType(Boolean))
    MnGroupTableRezept.Columns.Add("MESSG_ID", GetType(Integer))
    MnGroupTableRezept.Columns.Add("MESSG_GID", GetType(Integer))

    MnVWTable.TableName = "VWTABLE"
    Call Clear()
    '
    'Bit 0 bis 30 = ON
    '
    '
    MnUser.Visbl = 2147483647
    MnUser.Enabl = 2147483647
  End Sub


  Sub dispose() Implements IDisposable.Dispose
    MnMenue = Nothing
    MnNormFa = Nothing
    MnNormFaD6164 = Nothing
    MnMessg = Nothing
    MnMisch = Nothing
    MnUser = Nothing
    MnParaMerks = Nothing
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
End Class