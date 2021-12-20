Option Strict Off
Option Explicit On
Option Compare Text
Public Class Sign
  Dim MnID As Integer 'ID für Merkmal
  Dim MnKen As String 'Kennung für Merkmal (2 Zeichen)
  Dim MnFakt As Single 'Faktor für Merkmal
  Dim MnTyp As String 'Typ des Merkmals (T =Text N=Numerisch)
  Dim MnForm As String 'Formatangabe für Ausgabe
  Dim MnKbez As String 'Bezeichnung des Merkmals
  Dim MnDruPos As Short
  '
  '
  '
  Public Sub New(ByVal ID As Integer, ByVal Ken As String, ByVal Fakt As Single, ByVal Typ As String, ByVal Form As String, ByVal Kbez As String, ByVal DruPos As Short)
    MnID = ID
    MnKen = Ken
    MnFakt = Fakt
    MnTyp = Typ
    MnForm = Form
    MnKbez = Kbez
    MnDruPos = DruPos
  End Sub
  '
  '
  '
  '
  '

  ReadOnly Property ID() As Integer
    Get
      ID = MnID
    End Get

  End Property
  ReadOnly Property Ken() As String
    Get
      Ken = MnKen
    End Get

  End Property
  ReadOnly Property Fakt() As Single
    Get
      Fakt = MnFakt
    End Get
  End Property
  ReadOnly Property Typ() As String
    Get
      Typ = MnTyp
    End Get
  End Property
  Property Kbez() As String
    Get
      Kbez = MnKbez
    End Get
    Set(ByVal value As String)
      MnKbez = value
    End Set
  End Property
  ReadOnly Property Form() As String
    Get
      Form = MnForm
    End Get
  End Property
  ReadOnly Property DruPos() As Short
    Get
      DruPos = MnDruPos
    End Get
  End Property
End Class