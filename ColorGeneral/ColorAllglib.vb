Option Compare Text
Option Strict Off
Option Explicit On

Public Class ColorAllgLib
  Implements IDisposable
  '
  '
  Shared MnDbas As OleDbConnection
  Shared MnDdat As OleDbConnection
  Shared MnDtmp As OleDbConnection
  Shared MnDkal As OleDbConnection
  '
  '
  '
  '
  Shared MnCnCol As OleDbConnection
  Shared MnCnDat As OleDbConnection
  Shared MnCnKal As OleDbConnection
  Shared MnCnSys As OleDbConnection
  Shared MnCnTmp As OleDbConnection









  'Standard-Höhe (Height)
  '
  '
  Shared MnStHeight As Short
  '
  'Standard-Breite (Width)
  '
  '
  Shared MnSTWidth As Short
  '
  '
  '
  '
  'Standard-Left(Left)
  '
  '
  Shared MnStLeft As Short
  '
  'Standard-Top (Top)
  '
  '
  Shared MnSTTop As Short
  '
  '
  '
  'Menueparameter
  '
  '
  '
  '
  '
  '
  '
  '
  '
  Shared Property STHeight() As Short
    Get
      STHeight = MnStHeight
    End Get
    Set(ByVal Value As Short)
      MnStHeight = Value
    End Set
  End Property
  Shared Property STWidth() As Short
    Get
      STWidth = MnSTWidth
    End Get
    Set(ByVal Value As Short)
      MnSTWidth = Value
    End Set
  End Property
  Shared Property STLeft() As Short
    Get
      STLeft = MnStLeft
    End Get
    Set(ByVal Value As Short)
      MnStLeft = Value
    End Set
  End Property
  Shared Property STTop() As Short
    Get
      STTop = MnSTTop
    End Get
    Set(ByVal Value As Short)
      MnSTTop = Value
    End Set
  End Property











  

  Public Shared Function DataReader(ByRef cmd As OleDbCommand, ByRef Behavior As System.Data.CommandBehavior, Optional ByRef Conn As OleDbConnection = Nothing) As OleDbDataReader
    Dim AcConn As OleDbConnection
    Dim ConnBehav As CommandBehavior
    ConnBehav = Behavior
    DataReader = Nothing
    If IsNothing(Conn) Then
      AcConn = MnCnCol
    Else
      AcConn = Conn
    End If
    If AcConn.State = 0 Then
      ConnBehav = ConnBehav Or CommandBehavior.CloseConnection
    End If
    DataReader = DataRead(cmd, ConnBehav, AcConn)
  End Function
  Shared Function SQLExeNonQuery(ByVal cmd As OleDbCommand, Optional ByRef Conn As OleDbConnection = Nothing) As Integer
    Dim AcConn As OleDbConnection
    If IsNothing(Conn) Then
      AcConn = MnCnCol
    Else
      AcConn = Conn
    End If
    SQLExeNonQuery = SQLNonQuery(cmd, AcConn)

  End Function


  Shared Property DBas() As OleDbConnection
    Get
      If MnDbas Is Nothing OrElse MnDbas.ConnectionString = "" Then
        MnDbas = New OleDbConnection(GetFileProfile(PrivSettings("STARTUP", "SYSCNN", "", COLORFileName())))
      End If
      DBas = MnDbas
    End Get
    Set(value As OleDbConnection)
      MnDbas = value
    End Set
  End Property
  Shared Property Ddat() As OleDbConnection
    Get
      If MnDdat Is Nothing OrElse MnDdat.ConnectionString = "" Then
        MnDdat = New OleDbConnection(GetFileProfile(PrivSettings("STARTUP", "DATCNN", "", COLORFileName())))
      End If
      Ddat = MnDdat
    End Get
    Set(value As OleDbConnection)
      MnDdat = value
    End Set
  End Property
  Shared Property Dtmp() As OleDbConnection
    Get
      If MnDtmp Is Nothing OrElse MnDtmp.ConnectionString = "" Then
        MnDtmp = New OleDbConnection(GetFileProfile(PrivSettings("STARTUP", "TMPCNN", "", COLORFileName())))
      End If
      Dtmp = MnDtmp
    End Get
    Set(value As OleDbConnection)
      MnDtmp = value
    End Set
  End Property
  Shared Property Dkal() As OleDbConnection
    Get
      If MnDkal Is Nothing OrElse MnDkal.ConnectionString = "" Then
        MnDkal = New OleDbConnection(GetFileProfile(PrivSettings("STARTUP", "KALCNN", PrivSettings("STARTUP", "DATCNN", "", ColorHilfLib.COLORFileName()), COLORFileName())))
      End If
      Dkal = MnDkal
    End Get
    Set(value As OleDbConnection)
      MnDkal = value
    End Set
  End Property
  Shared Property Cncol As OleDbConnection
    Get
      If MnCnCol Is Nothing OrElse MnCnCol.ConnectionString = "" Then
        MnCnCol = New OleDbConnection(GetFileProfile(PrivSettings("STARTUP", "CNNCOL", "", COLORFileName())))
      End If
      Cncol = MnCnCol
    End Get
    Set(value As OleDbConnection)
      MnCnCol = value
    End Set
  End Property

  Shared Property Cndat() As OleDbConnection
    Get
      If MnCnDat Is Nothing OrElse MnCnDat.ConnectionString = "" Then
        MnCnDat = New OleDbConnection(GetFileProfile(PrivSettings("STARTUP", "CNNDAT", "", COLORFileName())))
      End If
      Cndat = MnCnDat
    End Get
    Set(value As OleDbConnection)
      MnCnDat = value
    End Set
  End Property
  Shared Property Cnkal() As OleDbConnection
    Get
      If MnCnKal Is Nothing OrElse MnCnKal.ConnectionString = "" Then
        MnCnKal = New OleDbConnection(GetFileProfile(PrivSettings("STARTUP", "CNNKAL", PrivSettings("STARTUP", "CNNDAT", "", COLORFileName()), COLORFileName())))
      End If
      Cnkal = MnCnKal
    End Get
    Set(value As OleDbConnection)
      MnCnKal = value
    End Set
  End Property
  Shared Property Cnsys() As OleDbConnection
    Get
      If MnCnSys Is Nothing OrElse MnCnSys.ConnectionString = "" Then
        MnCnSys = New OleDbConnection(GetFileProfile(PrivSettings("STARTUP", "CNNSYS", "", COLORFileName())))
      End If
      Cnsys = MnCnSys
    End Get
    Set(value As OleDbConnection)
      MnCnSys = value
    End Set
  End Property
  Shared Property CnTmp() As OleDbConnection
    Get
      If MnCnTmp Is Nothing OrElse MnCnTmp.ConnectionString = "" Then
        MnCnTmp = New OleDbConnection(GetFileProfile(PrivSettings("STARTUP", "CNNTMP", "", COLORFileName())))
      End If
      CnTmp = MnCnTmp
    End Get
    Set(value As OleDbConnection)
      MnCnTmp = value
    End Set
  End Property



  Private Sub dispose() Implements IDisposable.Dispose
    Exit Sub
    MnCnCol.Close()
    MnCnDat.Close()
    MnCnSys.Close()
    MnCnTmp.Close()
    '
    MnDbas.Close()
    MnDdat.Close()
    MnDtmp.Close()
    MnDkal.Close()
    '
    MnCnCol.Dispose()
    MnCnDat.Dispose()
    MnCnSys.Dispose()
    MnCnTmp.Dispose()
    '
    MnDbas.Dispose()
    MnDdat.Dispose()
    MnDtmp.Dispose()
    MnDkal.Dispose()
    '
    MnCnCol = Nothing
    MnCnDat = Nothing
    MnCnSys = Nothing
    MnCnTmp = Nothing
    '
    MnDbas = Nothing
    MnDdat = Nothing
    MnDtmp = Nothing
    MnDkal = Nothing

  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
End Class

