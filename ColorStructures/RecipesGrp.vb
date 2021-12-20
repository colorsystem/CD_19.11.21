Option Strict Off
Option Explicit On
Option Compare Text
Public Class RecipesGrp
  Implements IDisposable
  Implements ICloneable
  Dim MnMngMin As Single 'Untere Menge(Volumen) fuer Normierung
  Dim MnMngMax As Single 'Obere Menge(Volumen) fuer  Normierung
  Dim MnProzMin As Single 'Untere Grenze für Prozentigkeit gemäß INP
  Dim MnProzMax As Single 'Obere Grenze für Prozentigkeit gemäß INP
  Dim MnIVOL As Short 'Menge(0) oder Volumen(1)
  Dim MnINF As Short 'Art der Mengen- bzw Volumenangabe
  Dim MnINO As Short 'Art der ersten Normierung
  Dim MnINP As Short 'Art der zweiten Normierung (für Prozentigkeit im Zähler)
  Dim MnINQ As Short 'Art der zweiten Normierung (für Prozentigkeit im Nenner)
  Dim MnINM As Short 'Art der Mengen- bzw Volumenangabe (für Grenzen =,<,>)
  Dim MnSploe As Single 'Spez. Gewicht des Lösemittels
  Dim MnComment As String 'Kommentar
  Private MnRezepte As Recipes 'Collection für Rezepte
  Private MnFarben As Colorants  'Collection für Farb-/Bindemittel
  Private MnFarbAux As Colorants  'Collection für Farb-/Bindemittel(z.B. bei Korrekturberechnung mit neuen Grunddaten)

  '
  Public Sub New()
    MyBase.New()
    MnINO = 1
    MnINM = 0
    MnINP = 0
    MnINQ = 0
    MnINF = 0
    MnIVOL = 0
    MnProzMax = 0.05
    MnProzMin = 0.05
    MnMngMin = 100
    MnMngMax = 100
    MnSploe = 1.0#
    MnFarbAux = Nothing
    MnFarben = New Colorants
    MnRezepte = New Recipes
    MnComment = ""
  End Sub


  Sub dispose() Implements IDisposable.Dispose
    MnFarben = Nothing
    MnRezepte = Nothing
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Property Rezepte() As Recipes
    Get
      Rezepte = MnRezepte
    End Get
    Set(ByVal value As Recipes)
      MnRezepte = value
    End Set
  End Property
  Property Farben() As Colorants
    Get
      Farben = MnFarben
    End Get
    Set(ByVal value As Colorants)
      MnFarben = value
    End Set
  End Property
  Property FarbAux() As Colorants
    Get
      FarbAux = MnFarbAux
    End Get
    Set(ByVal value As Colorants)
      MnFarbAux = value
    End Set
  End Property
  '
  Property Comment As String
    Get
      Comment = MnComment
    End Get
    Set(ByVal Value As String)
      MnComment = Value
    End Set
  End Property
  '
  Property INM() As Short
    Get
      INM = MnINM
    End Get
    Set(ByVal Value As Short)
      MnINM = Value
    End Set
  End Property
  '
  '
  Property SpLoe() As Single
    Get
      SpLoe = MnSploe
    End Get
    Set(ByVal Value As Single)
      MnSploe = Value
    End Set
  End Property
  '
  '
  Property INO() As Short
    Get
      INO = MnINO
    End Get
    Set(ByVal Value As Short)
      MnINO = Value
    End Set
  End Property
  '
  '
  Property INF() As Short
    Get
      INF = MnINF
    End Get
    Set(ByVal Value As Short)
      MnINF = Value
    End Set
  End Property
  '
  Property INP() As Short
    Get
      INP = MnINP
    End Get
    Set(ByVal Value As Short)
      MnINP = Value
    End Set
  End Property
  Property INQ() As Short
    Get
      INQ = MnINQ
    End Get
    Set(ByVal Value As Short)
      MnINQ = Value
    End Set
  End Property
  '
  '
  Property IVOL() As Short
    Get
      IVOL = MnIVOL
    End Get
    Set(ByVal Value As Short)
      MnIVOL = Value
    End Set
  End Property
  '
  '
  Property MngMin() As Single
    Get
      MngMin = MnMngMin
    End Get
    Set(ByVal Value As Single)
      MnMngMin = Value
    End Set
  End Property
  '
  '
  '
  Property MngMax() As Single
    Get
      MngMax = MnMngMax
    End Get
    Set(ByVal Value As Single)
      MnMngMax = Value
    End Set
  End Property
  '
  '
  Property ProzMin() As Single
    Get
      ProzMin = MnProzMin
    End Get
    Set(ByVal Value As Single)
      MnProzMin = Value
    End Set
  End Property
  '
  '
  Property ProzMax() As Single
    Get
      ProzMax = MnProzMax
    End Get
    Set(ByVal Value As Single)
      MnProzMax = Value
    End Set
  End Property
  '
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As RecipesGrp
    clone = CType(Me.MemberwiseClone, RecipesGrp)
    If Not MnRezepte Is Nothing Then
      clone.MnRezepte = MnRezepte.clone
    End If
    '
    If Not MnFarben Is Nothing Then
      clone.MnFarben = MnFarben.clone
    End If
    '
  End Function

End Class