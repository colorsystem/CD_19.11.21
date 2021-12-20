Option Strict Off
Option Explicit On
Option Compare Text
Public Class UserParameters
    Implements IDisposable


    '
    '
    '
    '    User abhaengige Variable
    '
    Private MnEnabl As Integer
    Private MnVisbl As Integer
    Private MnSonst As Integer
    Private MnDrum As Integer
    Private MnDruq As Integer
    Private MnWrit As Integer
    Private MnName As String 'Name des Users
    Private MnPassw As String 'Passwort User
    Private MnExtr As Boolean
    Private MnRezpt As Boolean
    Private MnQual As Boolean

    '
    '
    'Methoden (userabhängig)
    '
    Private MnMethodID() As Integer
    '
    'Anzahl UserMethoden
    Private MnMethNmeth As Short
    Private MnWinkel As AngGeos



    '
    '
    '
    '
    '
    '
    '
    '
    Property Winkel() As AngGeos
        Get
            Winkel = MnWinkel
        End Get
        Set(ByVal Value As AngGeos)
            MnWinkel = Value
        End Set
    End Property
    Property Enabl() As Integer
        Get
            Enabl = MnEnabl
        End Get
        Set(ByVal Value As Integer)
            MnEnabl = Value
        End Set
    End Property
    Property Visbl() As Integer
        Get
            Visbl = MnVisbl
        End Get
        Set(ByVal Value As Integer)
            MnVisbl = Value
        End Set
    End Property
    Property Sonst() As Integer
        Get
            Sonst = MnSonst
        End Get
        Set(ByVal Value As Integer)
            MnSonst = Value
        End Set
    End Property
    Property Drum() As Integer
        Get
            Drum = MnDrum
        End Get
        Set(ByVal Value As Integer)
            MnDrum = Value
        End Set
    End Property
    Property Druq() As Integer
        Get
            Druq = MnDruq
        End Get
        Set(ByVal Value As Integer)
            MnDruq = Value
        End Set
    End Property
    Property Writ() As Integer
        Get
            Writ = MnWrit
        End Get
        Set(ByVal Value As Integer)
            MnWrit = Value
        End Set
    End Property
    Property Name() As String
        Get
            Return MnName
        End Get
        Set(ByVal Value As String)
            MnName = Value
        End Set
    End Property
    Property Passw() As String
        Get
            Passw = MnPassw
        End Get
        Set(ByVal Value As String)
            MnPassw = Value
        End Set
    End Property
    Property MethodID(ByVal i As Short) As Integer
        Get
            MethodID = MnMethodID(i)
        End Get
        Set(ByVal Value As Integer)
            MnMethNmeth = i + 1
            ReDim Preserve MnMethodID(i)
            MnMethodID(i) = Value
        End Set
    End Property
    ReadOnly Property MethNmeth() As Short
        Get
            MethNmeth = MnMethNmeth
        End Get
    End Property
    Property Extr() As Boolean
        Get
            Extr = MnExtr
        End Get
        Set(ByVal Value As Boolean)
            MnExtr = Value
        End Set
    End Property
    Property Qual() As Boolean
        Get
            Qual = MnQual
        End Get
        Set(ByVal Value As Boolean)
            MnQual = Value
        End Set
    End Property
    Property Rezpt() As Boolean
        Get
            Rezpt = MnRezpt
        End Get
        Set(ByVal Value As Boolean)
            MnRezpt = Value
        End Set
    End Property



    Public Sub New()
        MyBase.New()
        MnEnabl = 0
        MnVisbl = 0
        MnSonst = 0
        MnDrum = 0
        MnDruq = 0
        MnWrit = 0
        MnName = ""
        MnPassw = ""
        '
        MnWinkel = New AngGeos
    End Sub
    Sub dispose() Implements IDisposable.Dispose
        MnWinkel = Nothing
    End Sub
    Protected Overrides Sub Finalize()
        MyBase.Finalize()
        dispose()
    End Sub
End Class