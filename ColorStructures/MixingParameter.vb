Option Strict Off
Option Explicit On
Option Compare Text
Public Class MixingParameter
  Implements ICloneable
  '
  Dim i As Short
  Dim j As Short
  '
  '
  'system
  '
  '
  Private MnKbez As String 'Kurzbez.
  Private MnLbez As String 'Langbez.
  Private MnTbl As String 'Tabellenname für Mischsystem (Farbmittel,Rezepte usw.)
  Private MnEinm As String 'Einheit für Farb-/Bindemittelmengen
  Private MnEind As String 'Einheit für Schichdicke
  Private MnTop As Integer 'max Anzahl Rezepte in Listbox
  Private MnTdiff As Short 'Anzahl Tage für Datum (ab heute)
  Private MnSond As Short '
  Private MnSetting As Boolean 'Kennung, ob Dateisystem bereits angelegt
  Private MnUserRzpGID As Integer 'Gruppen-ID für Rezepte
  Private MnDicke As Single 'Standarddicke
  Private MnGewr As Single 'Gewicht für R-Werte (Grunddaten)
  Private MnGKwrtID As Integer 'ID für GK-Werte
  Private MnKowrt(9) As Single 'Kopplunsstärke für Grunddaten
  '0 Farbmittel
  '1 Bindemittel
  '2 Weißpigment
  '3 Schwarzpigmen
  '4 Metallic
  '5 Effektpigment
  '6 Restfarbe
  '7 Laborfarbe
  '8 Zusatzmittel
  '9 frei
  Private MnGrStr(5, 9) As Single 'Startwerte für Grunddaten
  Private MnGrDae(5, 9) As Single 'Daempfungswerte für Grunddaten
  Private MnGrKop(5, 9) As String  'Kopplungen für Grunddaten
  '
  '
  '
  Private MnIchi As Short 'Art der Korrekturrechnung für Hilfskorrekturen
  Private MnRzp As Short 'Max. Anzahl Rezepte bei Erstrezeptur
  Private MnMinFarb As Short 'Min. Anzahl Farb-/Bindemittel pro Rezept
  Private MnMaxFarb As Short 'Max. Anzahl Farb-/Bindemittel pro Rezept
  Private MnAmng As Single 'Normierungsmenge (Standardvorgabe)
  Private MnDeok As Single 'Tol. für Farbabstand zwischen R(Messung) und R(Rechnung)
  Private MnDeHlf As Single 'Tol. für Farbabstand zwischen R(Messung) und R(Rechnung) bei Hilfkorrekturen
  Private MnDeGut As Single 'Tol. für Farbabstand bei Ausgabe der Rezepte (DE(Rezepte)<Abs(DEGut) dann Sortierung nach vorne;
  'negativ bedeutet Unterdrückung der Ausgabe falls >)
  Private MnZuwag As Single 'Relative max. Zuwaage (<1.)
  Private MnMngOpt As Single 'Mengenvorgabe zur Berechnung einer optimalen Batchkonzentration )
  Private MnDto As Single 'Farbabstand für kleine Mengenänderungen
  Private MnGto As Single 'zugehöriges Gewicht
  Private MnFde As Single 'Kontrastfarbabstand für Rezeptrechnung
  Private MnGde As Single 'zugehöriges Gewicht
  Private MnGge As Single 'Gewicht für zusätzliche Minimierungsbedingungen (Igx=1,2,4)
  Private MnGXG As Single 'Gewicht für Zuwaageminimierung (IGX=3,5)
  Private MnIsor As Short 'Art der Rezeptsortierung
  Private MnBprob As Boolean 'Art der Bindemittelprozentigkeit
  '0 Bindemittelkonzentration (Prob) ist auf Bind- und Lösemittelbatchmenge bezogen
  '1 Bindemittelkonzentration (Prob) ist auf Gesamtbatchmenge bezogen
  'Achtung MnProb ist immer auf Binde- und Lösemittel bezogen!!!
  Private MnIgx As Short 'Art der Rezept bzw. Korrekturberechnung (s. Gge bzw. Zuwag)
  '0 Minimierung des Farbabstandes
  '1 Minimierung des Farbabstandes bei minimaler Bindemittelmenge
  '2 Minimierung des Farbabstandes bei minimaler Farbmittelmenge
  '3 Minimierung des Farbabstandes bei geg. max Zuwaage
  '4 Minimierung des Farbabstandes bei minimaler Mengenänderung
  '5 Farbabstand=DTO bei minimaler Zuwaage
  '6 Farbabstand=DTO bei minimaler Mengenänderung
  Private MnKwe As Short 'Nummer des Winkels bzw. Meßgeometrie für Berechnung des Farbabstandes (0 alle)
  Private MnKwm As Short 'Nummer des Winkels bzw. Meßgeometrie für minimale Mengenänderung bzw. Zuwaage (DTO)
  Private MnKgx As Boolean 'true mit Berücksichtigung des Kontrastfarbabstandes
  Private MnKwd As Short 'Nummer des Winkels bzw. Meßgeometrie für Kontrastfarbabstand
  Private MnKwh As Short 'Winkel für Raumrechnung
  Private MnMGGE As Boolean 'Startrezept mit(1) oder ohne(0) Gewicht
  Private MnTransp As Boolean 'false deckend; True transparent
  Private MnNpx As Short 'Anzahl Grunddaten pro Wellenlänge (<NPQ)
  Private MnLik As Short 'Max. Likelihood (0 nein; 1 ja)
  Private MnGrAlt As Short 'Alte Grunddaten werden als Startwerte übernommen
  Private MnRezVor As Boolean 'false Reflexionswerte (Colorthek) für Nachstellung; true ~ für Vorlage
  Private MnSchwrz As Boolean 'true Standardmäßig werden Messungen über schwarzem Untergrund verwendet
  Private MnVert As Boolean 'true Standardmäßig werden die erste und zweite Messung vertauscht
  Private MnMinDos As Single 'Minimale Dosierung
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
  '
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
  Property Tbl() As String
    Get
      Tbl = MnTbl
    End Get
    Set(ByVal Value As String)
      MnTbl = Value
    End Set
  End Property
  Property Einm() As String
    Get
      Einm = MnEinm
    End Get
    Set(ByVal Value As String)
      MnEinm = Value
    End Set
  End Property
  Property Eind() As String
    Get
      Eind = MnEind
    End Get
    Set(ByVal Value As String)
      MnEind = Value
    End Set
  End Property
  Property Top() As Integer
    Get
      Return MnTop
    End Get
    Set(ByVal Value As Integer)
      MnTop = Value
      If MnTop <= 0 Then
        MnTop = 1
      End If
    End Set
  End Property
  Property Tdiff() As Short
    Get
      Tdiff = MnTdiff
    End Get
    Set(ByVal Value As Short)
      MnTdiff = Value
    End Set
  End Property
  Property Sond() As Short
    Get
      Sond = MnSond
    End Get
    Set(ByVal Value As Short)
      MnSond = Value
    End Set
  End Property
  Property Setting() As Boolean
    Get
      Setting = MnSetting
    End Get
    Set(ByVal Value As Boolean)
      MnSetting = Value
    End Set
  End Property
  Property UserRzpGID() As Integer
    Get
      UserRzpGID = MnUserRzpGID
    End Get
    Set(ByVal Value As Integer)
      MnUserRzpGID = Value
    End Set
  End Property
  Property Dicke() As Single
    Get
      Dicke = MnDicke
    End Get
    Set(ByVal Value As Single)
      MnDicke = Value
    End Set
  End Property



  Property Gewr() As Single
    Get
      Gewr = MnGewr
    End Get
    Set(ByVal Value As Single)
      MnGewr = Value
    End Set
  End Property
  Property GKwrtID() As Integer
    Get
      GKwrtID = MnGKwrtID
    End Get
    Set(ByVal Value As Integer)
      MnGKwrtID = Value
    End Set
  End Property


  ReadOnly Property Kowrt() As Single()
    Get
      Kowrt = MnKowrt
    End Get
  End Property
  ReadOnly Property GrStr() As Single(,)
    Get
      GrStr = MnGrStr
    End Get
  End Property
  ReadOnly Property GrDae() As Single(,)
    Get
      GrDae = MnGrDae
    End Get
  End Property
  ReadOnly Property GrKop() As String(,)
    Get
      GrKop = MnGrKop
    End Get
  End Property
  '
  '
  '
  '
  Property RezVor() As Boolean
    Get
      RezVor = MnRezVor
    End Get
    Set(ByVal Value As Boolean)
      MnRezVor = Value
    End Set
  End Property
  Property Schwrz() As Boolean
    Get
      Schwrz = MnSchwrz
    End Get
    Set(ByVal Value As Boolean)
      MnSchwrz = Value
    End Set
  End Property
  Property Vert() As Boolean
    Get
      Vert = MnVert
    End Get
    Set(ByVal Value As Boolean)
      MnVert = Value
      MnVert = False
      '
      'Keine Vertauschung der Untergründe
      '
      '
    End Set
  End Property
  Property Ichi() As Short
    Get
      Ichi = MnIchi
    End Get
    Set(ByVal Value As Short)
      MnIchi = Value
    End Set
  End Property

  Property Rzp() As Short
    Get
      Rzp = MnRzp
    End Get
    Set(ByVal Value As Short)
      MnRzp = Value
    End Set
  End Property
  Property MinFarb() As Short
    Get
      MinFarb = MnMinFarb
    End Get
    Set(ByVal Value As Short)
      MnMinFarb = Value
    End Set
  End Property
  Property MaxFarb() As Short
    Get
      MaxFarb = MnMaxFarb
    End Get
    Set(ByVal Value As Short)
      MnMaxFarb = Value
    End Set
  End Property
  Property Amng() As Single
    Get
      Amng = MnAmng
    End Get
    Set(ByVal Value As Single)
      MnAmng = Value
    End Set
  End Property
  Property Deok() As Single
    Get
      Deok = MnDeok
    End Get
    Set(ByVal Value As Single)
      MnDeok = Value
    End Set
  End Property
  Property DeHlf() As Single
    Get
      DeHlf = MnDeHlf
    End Get
    Set(ByVal Value As Single)
      MnDeHlf = Value
    End Set
  End Property
  Property DeGut() As Single
    Get
      DeGut = MnDeGut
    End Get
    Set(ByVal Value As Single)
      MnDeGut = Value
    End Set
  End Property
  Property MinDos() As Single
    Get
      MinDos = MnMinDos
    End Get
    Set(ByVal Value As Single)
      MnMinDos = Value
    End Set
  End Property
  Property Zuwag() As Single
    Get
      Zuwag = MnZuwag
    End Get
    Set(ByVal Value As Single)
      MnZuwag = Value
    End Set
  End Property
  Property MngOpt() As Single
    Get
      MngOpt = MnMngOpt
    End Get
    Set(ByVal Value As Single)
      MnMngOpt = Value
    End Set
  End Property
  Property Dto() As Single
    Get
      Dto = MnDto
    End Get
    Set(ByVal Value As Single)
      MnDto = Value
    End Set
  End Property
  Property Gto() As Single
    Get
      Gto = MnGto
    End Get
    Set(ByVal Value As Single)
      MnGto = Value
    End Set
  End Property
  Property Fde() As Single
    Get
      Fde = MnFde
    End Get
    Set(ByVal Value As Single)
      MnFde = Value
    End Set
  End Property
  Property Gde() As Single
    Get
      Gde = MnGde
    End Get
    Set(ByVal Value As Single)
      MnGde = Value
    End Set
  End Property
  Property Gge() As Single
    Get
      Gge = MnGge
    End Get
    Set(ByVal Value As Single)
      MnGge = Value
    End Set
  End Property
  Property Gxg() As Single
    Get
      Gxg = MnGXG
    End Get
    Set(ByVal Value As Single)
      MnGXG = Value
    End Set
  End Property
  Property Isor() As Short
    Get
      Isor = MnIsor
    End Get
    Set(ByVal Value As Short)
      MnIsor = Value
    End Set
  End Property
  Property Bprob() As Boolean
    Get
      Bprob = MnBprob
    End Get
    Set(ByVal Value As Boolean)
      MnBprob = Value
    End Set
  End Property




  Property Igx() As Short
    Get
      Igx = MnIgx
    End Get
    Set(ByVal Value As Short)
      MnIgx = Value
    End Set
  End Property
  Property MGGE() As Boolean
    Get
      MGGE = MnMGGE
    End Get
    Set(ByVal Value As Boolean)
      MnMGGE = Value
    End Set
  End Property
  Property Kwe() As Short
    Get
      Kwe = MnKwe
    End Get
    Set(ByVal Value As Short)
      MnKwe = Value
    End Set
  End Property
  Property Kwh() As Short
    Get
      Kwh = MnKwh
    End Get
    Set(ByVal Value As Short)
      MnKwh = Value
    End Set
  End Property
  Property Kgx() As Boolean
    Get
      Kgx = MnKgx
    End Get
    Set(ByVal Value As Boolean)
      MnKgx = Value
    End Set
  End Property
  Property Kwd() As Short
    Get
      Kwd = MnKwd
    End Get
    Set(ByVal Value As Short)
      MnKwd = Value
    End Set
  End Property
  Property Transp() As Boolean
    Get
      Transp = MnTransp
    End Get
    Set(ByVal Value As Boolean)
      MnTransp = Value
    End Set
  End Property
  Property Npx() As Short
    Get
      Npx = MnNpx
    End Get
    Set(ByVal Value As Short)
      MnNpx = Value
    End Set
  End Property
  Property Lik() As Short
    Get
      Lik = MnLik
    End Get
    Set(ByVal Value As Short)
      MnLik = Value
    End Set
  End Property
  Property GrAlt() As Short
    Get
      GrAlt = MnGrAlt
    End Get
    Set(ByVal Value As Short)
      MnGrAlt = Value
    End Set
  End Property



  Public Sub New()
    MyBase.New()
    Dim i As Integer
    Dim j As Integer
    MnDicke = 1.0#
    '
    '     Standardwerte übernehmen
    '

    For j = 0 To 9
      MnKowrt(j) = 0
      For i = 0 To 5
        MnGrStr(i, j) = 0.0#
        MnGrDae(i, j) = 0.0#
        MnGrKop(i, j) = "#"
      Next i
    Next j
    MnKbez = ""
    MnLbez = ""
    MnTbl = ""
    MnEinm = ""
    MnEind = ""
    MnTop = 200
    MnTdiff = 30
    MnSond = 0
    MnSetting = False
    MnUserRzpGID = 0
    MnGewr = 1.0#
    MnGKwrtID = 0
    '
    '
    MnIchi = 0
    MnRzp = 40
    MnMinFarb = 4
    MnMaxFarb = 4
    MnAmng = 100
    MnDeok = 10
    MnDeHlf = 10
    MnDeGut = 1
    MnZuwag = 20
    MnRezVor = 0
    MnSchwrz = 0
    MnMngOpt = 1
    MnDto = 0.3
    MnGto = 10
    MnFde = 1
    MnGde = 10
    MnGge = 10
    MnGXG = 0.005
    MnIsor = 0
    MnBprob = 0
    MnIgx = 0
    MnKwe = 1
    MnKwh = 0
    MnKgx = False
    MnKwd = 1
    MnTransp = False
    MnNpx = 2
    MnLik = 0
    MnMinDos = 0.0
  End Sub
  '
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As MixingParameter
    clone = CType(Me.MemberwiseClone, MixingParameter)
  End Function
End Class