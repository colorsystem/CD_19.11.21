Option Strict Off
Option Explicit On
Option Compare Text
Public Class RefValue
  Implements IDisposable
  Implements ICloneable
  Private MnRefKurv As CurvesRef
  Dim kw As Short
  Private MnQuControl As QuControls 'Zusatzinformation für QU-Kontrolle
  Private MnID As Integer 'ID Nummer
  Private MnNr As Short 'laufende Nummer
  'V Vorlage
  'N Nachstellung
  'R Rechnung
  'Kennung für Nummer
  Private MnName As String 'Name der Reflexionswerte
  Private MnBem As String 'Bemerkung
  Private MnBanum As String 'BA-Nummer
  Private MnDatTim As Date  'Datum + Zeit
  Private MnCME As String 'Kennung fuer Art der Messung; MessgKenn  
  Private MnGid As Integer 'Gruppen-ID
  Private MnIarch As Short '0 = normal, 1 = archiviert; 2 = gesperrt
  Private MnIVoNa As Boolean
  Private MnIplott As Boolean
  Private MnTag As String
  Private MnMessgID As Integer

  ' True   wird zur Rezeptberechnung und zur Berechnung von Farbwerten verwendet
  ' False  wird nicht verwendet
  Private MnItp As Boolean 'True =Bezug;False=Probe
  Private MnDe() As Single 'Farbdifferenzen nach statistischer Auswertung
  Private MnDik() As Single 'Schichtdicken z.B. bei Grunddatenberechnung mit max. Likelihood berechnet
  Private MnIami As Short 'Anzahl Messungen für Mittelung
  Dim MnReTr As Short
  '0 Reflexion 
  '1 Transmission
  Dim MnKwb As Short
  '0 Weisser Untergrund
  '1 schwarzer Untergrund

  '

  '
  '
  Public Sub New()
    MyBase.New()
    MnName = ""
    MnBem = ""
    MnBanum = ""
    MnIami = 0
    MnID = -1
    MnNr = -1
    MnDatTim = Date.Now
    MnIVoNa = False
    MnIplott = False
    MnItp = False
    MnIarch = 0
    MnCME = "  "
    MnKwb = 0
    MnReTr = 0
    MnTag = ""
    MnMessgID = -1
    MnRefKurv = New CurvesRef
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub

  Sub dispose() Implements IDisposable.Dispose
    MnRefKurv = Nothing
    QuControl = Nothing
  End Sub


  '
  '
  '
  '

  Property QuControl() As QuControls
    Get
      QuControl = MnQuControl
    End Get
    Set(ByVal Value As QuControls)
      MnQuControl = Value
    End Set
  End Property
  ReadOnly Property RefKurv() As CurvesRef
    Get
      RefKurv = MnRefKurv
    End Get
  End Property


  Property ID() As Integer
    Get
      ID = MnID
    End Get
    Set(ByVal Value As Integer)
      MnID = Value
    End Set
  End Property
  Property MessgID() As Integer
    Get
      MessgID = MnMessgID
    End Get
    Set(ByVal Value As Integer)
      MnMessgID = Value
    End Set
  End Property
  Property Tag() As String
    Get
      Tag = MnTag
    End Get
    Set(ByVal Value As String)
      MnTag = Value
    End Set
  End Property
  Property Nr() As Short
    Get
      Nr = MnNr
    End Get
    Set(ByVal Value As Short)
      MnNr = Value
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
  Property Bem() As String
    Get
      Bem = MnBem
    End Get
    Set(ByVal Value As String)
      MnBem = Value
    End Set
  End Property
  Property Banum() As String
    Get
      Banum = MnBanum
    End Get
    Set(ByVal Value As String)
      MnBanum = Value
    End Set
  End Property
  Property DatTim() As Date
    Get
      DatTim = MnDatTim
    End Get
    Set(ByVal Value As Date)
      MnDatTim = Value
    End Set
  End Property
  Property Cme() As String
    Get
      Cme = MnCME
    End Get
    Set(ByVal Value As String)
      MnCME = Value
    End Set
  End Property
  Property Gid() As Integer
    Get
      Gid = MnGid
    End Get
    Set(ByVal Value As Integer)
      MnGid = Value
    End Set
  End Property
  Property Iarch() As Short
    Get
      Iarch = MnIarch
    End Get
    Set(ByVal Value As Short)
      MnIarch = Value
    End Set
  End Property

  Property IVoNa() As Boolean
    Get
      IVoNa = MnIVoNa
    End Get
    Set(ByVal Value As Boolean)
      MnIVoNa = Value
    End Set
  End Property
  Property Iplott() As Boolean
    Get
      Iplott = MnIplott
    End Get
    Set(ByVal Value As Boolean)
      MnIplott = Value
    End Set
  End Property

  Property Itp() As Boolean
    Get
      Itp = MnItp
    End Get
    Set(ByVal Value As Boolean)
      MnItp = Value
    End Set
  End Property
  Property ReTr() As Short
    Get
      ReTr = MnReTr
    End Get
    Set(ByVal value As Short)
      MnReTr = value
    End Set
  End Property
  Property kwb() As Short
    Get
      kwb = MnKwb
    End Get
    Set(ByVal value As Short)
      MnKwb = value
    End Set
  End Property
  Property De(ByVal i As Short) As Single
    Get
      If Not IsNothing(MnDe) AndAlso (MnIami > 0 And i <= UBound(MnDe)) Then
        De = MnDe(i)
      Else
        De = 0.0#
      End If
    End Get
    Set(ByVal Value As Single)
      ReDim Preserve MnDe(i)
      MnDe(i) = Value
    End Set
  End Property
  Property Dik(ByVal i As Short) As Single
    Get
      If Not IsNothing(MnDik) Then
        Dik = MnDik(i)
      Else
        Dik = 0.0#
      End If
    End Get
    Set(ByVal Value As Single)
      ReDim Preserve MnDik(i)
      MnDik(i) = Value
    End Set
  End Property
  Property Iami() As Short
    Get
      Iami = MnIami
    End Get
    Set(ByVal Value As Short)
      MnIami = Value
    End Set
  End Property
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As RefValue
    clone = CType(Me.MemberwiseClone, RefValue)
    If Not MnQuControl Is Nothing Then
      clone.MnQuControl = MnQuControl.clone
    End If
    If Not MnRefKurv Is Nothing Then
      clone.MnRefKurv = MnRefKurv.clone
    End If
    If Not MnDe Is Nothing Then
      clone.MnDe = MnDe.Clone
    End If
    If Not MnDik Is Nothing Then
      clone.MnDik = MnDik.Clone
    End If
  End Function

End Class