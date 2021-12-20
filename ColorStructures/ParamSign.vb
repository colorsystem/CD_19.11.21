Option Strict Off
Option Explicit On
Option Compare Text
Public Class ParamSign
  Dim MnID As Integer 'ID für Parameter
  Dim MnBezWrt As String 'Zusatzangabe für Parameter (z.B. wg. translate)
  Dim MnWert As Object 'Defaultwert
  Dim MnTyp As String 'Typ des Parameteres (T=Text;N=Numerisch)
  Dim MnForm As String 'Formatangabe für Ausgabe
  Dim MnKey As String 'Key des Parameters
  Dim MnKbez As String '(Kurzbezeichnung)
  Dim MnLbez As String 'Name für Parameter (Langbezeichnung)
  Dim MnCrntlID As Integer 'Bit-Verschlüsselung
  Dim MnAuswid As Values
  '
  '
  '
  Public Sub New(ByVal IDn As Integer, ByVal Kbezn As String, ByVal Lbezn As String, ByVal BezWrtn As String, ByVal Typn As String, ByVal Formn As String, ByVal Wertn As Object)
    MyBase.New()
    MnID = IDn
    MnKey = ""
    If IsDBNull(BezWrt) Then
      MnBezWrt = ""
    Else
      MnBezWrt = BezWrtn
    End If
    MnBezWrt = BezWrtn
    MnKbez = Kbezn
    MnLbez = Lbezn
    MnWert = Wertn
    MnTyp = Typn
    MnForm = Formn
    MnCrntlID = 0
    MnAuswid = New Values
  End Sub


  Property ID() As Integer
    Get
      ID = MnID
    End Get
    Set(ByVal Value As Integer)
      MnID = Value
    End Set
  End Property
  Property Kbez() As String
    Get
      Kbez = MnKbez
    End Get
    Set(ByVal Value As String)
      MnKbez = Value
    End Set
  End Property

  Property Lbez() As String
    Get
      Lbez = MnLbez
    End Get
    Set(ByVal Value As String)
      MnLbez = Value
    End Set
  End Property
  Property BezWrt() As String
    Get
      BezWrt = MnBezWrt
    End Get
    Set(ByVal Value As String)
      MnBezWrt = Value
    End Set
  End Property
  Property Wert() As Object
    Get
      Wert = MnWert
    End Get
    Set(ByVal Value As Object)
      MnWert = Value
    End Set
  End Property
  Property Typ() As String
    Get
      Typ = MnTyp
    End Get
    Set(ByVal Value As String)
      MnTyp = Value
    End Set
  End Property

  Property Form() As String
    Get
      Form = MnForm
    End Get
    Set(ByVal Value As String)
      MnForm = Value
    End Set
  End Property
  Property CrntlID() As Integer
    Get
      CrntlID = MnCrntlID
    End Get
    Set(ByVal Value As Integer)
      MnCrntlID = Value
    End Set
  End Property
  ReadOnly Property AuswID() As Values
    Get
      AuswID = MnAuswid
    End Get
  End Property

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    MnAuswid = Nothing
  End Sub
End Class