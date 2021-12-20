Option Strict Off
Option Explicit On
Option Compare Text
Public Class MeasParameters
  Implements IDisposable
  Implements ICloneable

  '
  '
  '

  'Messgeraeteabhaengige Variable
  '
  '
  Private MnMessgID As Integer
  Dim MnMessgRwID As Integer 'ID MessgerätGruppe (falls mehrere Messgeräte die gleiche R-Wert-Datei verwenden)
  Private MnTbl As String 'Tabellenname für R-Werte Grunddaten usw
  Private MnNormFile As String 'Tabellenname fuer Gewichtsfaktoren der NormFarbWerte
  Private MnCom As Short 'Serielle Schnittstelle
  Private MnKenn As String 'Kennung zur Bezeichnung der R-Werte
  Private MnLbez As String 'Langbezeichnung Messgerät
  Private MnKbez As String 'Kurzbez. Messg.
  Private MnBaud As Integer 'Baudrate Messgeraet
  Private MnLength As Short 'Anzahl Bit pro Zeichen
  Private MnStopbit As Short 'Anzahl Stopbits
  Private MnPar As String 'Paritaet
  Private MnImes As Short 'Anzahl Messungen für Mittelung
  Private MnIkal As Short 'Anzahl Messungen für Kalibrierung
  Private MnDe As Single 'Toleranz für Farbabstand bei Messung
  Private MnProg As String 'Kennung für Messgerät
  Private MnSetting As Boolean 'Kennung , ob Dateisystem für Messgerät bereits erstellt
  Private MnKm As Short 'Anzahl Winkel /Messgeometrien Messgerät
  Private MnHands As Short 'Handshakeparameter s. (VISUAL BASIC MSCOMM)
  Private MnDriver As String 'Kennung für Messprogramm
  Private MnBereichID As Short 'BEREICH_ID Kennung über Art und Anzahl der Wellenlängen
  Private MnWellStart As Short  'Anfangswellenlänge
  Private MnWellEnde As Short   'Endwellenlänge
  Private MnWellStep As Short   'Schrittweite Wellenlänge
  Private MnLaborID As Object 'ID für Labor
  Private MnStell As Short 'Anzahl Stellen(Ziffern) für Name R-Werte
  Private MnTdiff As Short 'Differenz in Tagen vor aktuellem Datum
  Private MnTop As Integer 'max Anzahl R-Werte in Liste
  '                                    '
  '                                    '0 =aus; 1 = an
  Private MnMes As Short 'Menue Messen (an/aus)
  Private MnKal As Short 'Menue Kalibrieren (an/aus)
  Private MnNkal As Short 'Menue Null-Kalibrieren (an/aus)
  Private MnIni As Short 'Menue Initialisieren (an/aus)
  Private MnSond As Short 'Menue Sonderprogramm (an/aus)
  Private MnKein As Short 'Menue kein Meßgerät (an/aus)
  Private MnTwait As Short 'Anzahl Sekunden (Warten mit Darstellung des Grids für Fehler)
  Private MnGlIn As Boolean 'Der inneren Reflexion wird automatisch der Wert des Glanzes zugeordnet
  Private MnKa As Short 'Art der Mittelung (1=arith;2=geom.;3=harm)
  Private MnUserRefGID As Integer 'User-Gruppen-ID für R-Werte
  Private MnRwrtGID As Integer 'Aktuelle ID Für Gruppen-ID   R-Werte
  Private MnMessgKalMess As Boolean 'Message "Heute bereits kalibriert " entfällt
  '
  Private MnGKwrtID As Integer 'ID für GK-Werte
  Private MnGKsperr As Short 'Kennung (z.Z. nicht verwendet)
  Private MnGKBez As String 'Bezeichnung für GK-Werte
  Private MnGK(15) As Single 'GK-Werte
  Private MnCDE As String 'Kennung für Art der Grunddatenberechnungb (z.B. deckend, transparent usw.)
  Private MnExists As Boolean 'Kalibriert (ja/nein)
  Private MnGlanz As Single
  Private MnRefTra As String 'R=Reflexion;T=Transmission;A=Beides
  Private MnReTr As Short 'Standardeinstellung, ob Reflexion (0) oder Transmission(1) gemessen werden soll
  Private MnMeArtID As Integer 'Bit0=0 erste Messung=Reflexion;
  'Bit0=1 erste Messung=Transmission
  'dsgl für Bit1
  Private MnMeArtLock As Short ' Bestimmte Methoden der Q-Komntrolle können nur für Reflexionsmesungen(0)
  'oder nur für Transmissionsmessungen aufgerufen werden (-1 beide erlaubt)
  Private MnKalInt As Short 'Kalibrierintervall in Stunden
  '
  'Labor
  '
  '
  '
  '
  Private MnLaborKbez As String 'Kurzbezeichnug Labor
  Private MnLaborAbtc As String 'Abteilungscode
  Private MnLaborBau As String 'Bau
  Private MnLaborKst As Integer 'Kostenstelle
  Private MnLaborText1 As String '1. Text für Ausdruck
  Private MnLaborText2 As String '2. Text für Ausdruck
  Private MnLaborText3 As String '3. Text für Ausdruck
  Private MnIDka() As Short 'Art und Reihenfolge der Kalibrierung (0 keine;1 weiß;2 schwarz;3 grau;)

  '
  Private MnWinkel As AngGeos 'Winkel/Meßgeometrien
  Private MnQW As CurvesRef 'Absolutwerte (Reflexion Weiss)
  Private MnQS As CurvesRef 'Absolutwerte (Reflexion vSchwarz)
  Private MnQG As CurvesRef 'Absolutwerte (Reflexion Grau)
  Private MnQT As CurvesRef 'Absolutwerte (Transmission Weiss)
  Private MnQB As CurvesRef 'Absolutwerte (Transmission Schwarz)

  Private MnMW As CurvesRef 'Kalibrierwerte (Weiß)
  Private MnMS As CurvesRef 'Kalibrierwerte (Schwarz)
  Private MnMG As CurvesRef 'Kalibrierwerte  (Grau)
  Private MnMT As CurvesRef 'Kalibrierwerte(Transmission Weiss)
  Private MnMB As CurvesRef 'Kalibrierwerte (Transmission Schwarz)
  Private MnWist As CurvesRef 'Ist-Wellenlängen
  Private MnRefGew As CurveRef
  Private MnRefOne As CurveRef

  '
  '
  '
  '
  '
  '
  '
  '
  '
  Property MessgID() As Integer
    Get
      MessgID = MnMessgID
    End Get
    Set(ByVal Value As Integer)
      MnMessgID = Value
    End Set
  End Property
  Property MessgRwID() As Integer
    Get
      MessgRwID = MnMessgRwID
    End Get
    Set(ByVal Value As Integer)
      MnMessgRwID = Value
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
  Property NormFile() As String
    Get
      NormFile = MnNormFile
    End Get
    Set(ByVal Value As String)
      MnNormFile = Value
    End Set
  End Property
  Property BereichID() As Short
    Get
      BereichID = MnBereichID
    End Get
    Set(ByVal Value As Short)
      MnBereichID = Value
    End Set
  End Property
  Property WellStart() As Short
    Get
      WellStart = MnWellStart
    End Get
    Set(ByVal Value As Short)
      MnWellStart = Value
    End Set
  End Property
  Property WellEnde() As Short
    Get
      WellEnde = MnWellEnde
    End Get
    Set(ByVal Value As Short)
      MnWellEnde = Value
    End Set
  End Property
  Property WellStep() As Short
    Get
      WellStep = MnWellStep
    End Get
    Set(ByVal Value As Short)
      MnWellStep = Value
    End Set
  End Property
  Property LaborID() As Short
    Get
      LaborID = MnLaborID
    End Get
    Set(ByVal Value As Short)
      MnLaborID = Value
    End Set
  End Property

  Property Com() As Short
    Get
      Com = MnCom
    End Get
    Set(ByVal Value As Short)
      MnCom = Value
    End Set
  End Property
  Property KalInt() As Short
    Get
      KalInt = MnKalInt
    End Get
    Set(ByVal Value As Short)
      MnKalInt = Value
    End Set
  End Property
  Property Kenn() As String
    Get
      Kenn = MnKenn
    End Get
    Set(ByVal Value As String)
      MnKenn = Value
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
  Property Kbez() As String
    Get
      Kbez = MnKbez
    End Get
    Set(ByVal Value As String)
      MnKbez = Value
    End Set
  End Property
  Property Baud() As Integer
    Get
      Baud = MnBaud
    End Get
    Set(ByVal Value As Integer)
      MnBaud = Value
    End Set
  End Property
  Property MeArtID() As Integer
    Get
      MeArtID = MnMeArtID
    End Get
    Set(ByVal Value As Integer)
      MnMeArtID = Value
    End Set
  End Property
  Property MeArtLock() As Short
    Get
      MeArtLock = MnMeArtLock
    End Get
    Set(ByVal Value As Short)
      MnMeArtLock = Value
    End Set
  End Property

  Property RefTra() As String
    Get
      RefTra = MnRefTra
    End Get
    Set(ByVal Value As String)
      MnRefTra = Value
    End Set
  End Property
  Property ReTr() As Short
    Get
      ReTr = MnReTr
    End Get
    Set(ByVal Value As Short)
      MnReTr = Value
    End Set
  End Property
  Property Length() As Short
    Get
      Length = MnLength
    End Get
    Set(ByVal Value As Short)
      MnLength = Value
    End Set
  End Property
  Property Stopbit() As Short
    Get
      Stopbit = MnStopbit
    End Get
    Set(ByVal Value As Short)
      MnStopbit = Value
    End Set
  End Property
  Property Par() As String
    Get
      Par = MnPar
    End Get
    Set(ByVal Value As String)
      MnPar = Value
    End Set
  End Property
  Property Imes() As Short
    Get
      Imes = MnImes
    End Get
    Set(ByVal Value As Short)
      MnImes = Value
    End Set
  End Property
  Property Ikal() As Short
    Get
      Ikal = MnIkal
    End Get
    Set(ByVal Value As Short)
      MnIkal = Value
    End Set
  End Property
  Property De() As Single
    Get
      De = MnDe
    End Get
    Set(ByVal Value As Single)
      MnDe = Value
    End Set
  End Property
  Property Prog() As String
    Get
      Prog = MnProg
    End Get
    Set(ByVal Value As String)
      MnProg = Value
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
  Property Hands() As Short
    Get
      Hands = MnHands
    End Get
    Set(ByVal Value As Short)
      MnHands = Value
    End Set
  End Property
  Property Driver() As String
    Get
      Driver = MnDriver
    End Get
    Set(ByVal Value As String)
      MnDriver = Value
    End Set
  End Property

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
  Property Stell() As Short
    Get
      Stell = MnStell
    End Get
    Set(ByVal Value As Short)
      MnStell = Value
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
  Property Mes() As Short
    Get
      Mes = MnMes
    End Get
    Set(ByVal Value As Short)
      MnMes = Value
    End Set
  End Property
  Property Kal() As Short
    Get
      Kal = MnKal
    End Get
    Set(ByVal Value As Short)
      MnKal = Value
    End Set
  End Property
  Property Nkal() As Short
    Get
      Nkal = MnNkal
    End Get
    Set(ByVal Value As Short)
      MnNkal = Value
    End Set
  End Property
  Property Ini() As Short
    Get
      Ini = MnIni
    End Get
    Set(ByVal Value As Short)
      MnIni = Value
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
  Property Kein() As Short
    Get
      Kein = MnKein
    End Get
    Set(ByVal Value As Short)
      MnKein = Value
    End Set
  End Property
  Property Twait() As Short
    Get
      Twait = MnTwait
    End Get
    Set(ByVal Value As Short)
      MnTwait = Value
    End Set
  End Property
  Property GlIn() As Boolean
    Get
      GlIn = MnGlIn
    End Get
    Set(ByVal Value As Boolean)
      MnGlIn = Value
    End Set
  End Property
  Property MessgKalMess() As Boolean
    Get
      MessgKalMess = MnMessgKalMess
    End Get
    Set(ByVal Value As Boolean)
      MnMessgKalMess = Value
    End Set
  End Property
  Property Ka() As Short
    Get
      Ka = MnKa
    End Get
    Set(ByVal Value As Short)
      MnKa = Value
    End Set
  End Property
  Property UserRefGID() As Integer
    Get
      UserRefGID = MnUserRefGID
    End Get
    Set(ByVal Value As Integer)
      MnUserRefGID = Value
    End Set
  End Property
  Property RwrtGID() As Integer
    Get
      RwrtGID = MnRwrtGID
    End Get
    Set(ByVal Value As Integer)
      MnRwrtGID = Value
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
  Property GKsperr() As Short
    Get
      GKsperr = MnGKsperr
    End Get
    Set(ByVal Value As Short)
      MnGKsperr = Value
    End Set
  End Property
  Property GKBez() As String
    Get
      GKBez = MnGKBez
    End Get
    Set(ByVal Value As String)
      MnGKBez = Value
    End Set
  End Property
  Property Exists() As Boolean
    Get
      Exists = MnExists
    End Get
    Set(ByVal Value As Boolean)
      MnExists = Value
    End Set
  End Property


 
  Property CDE() As String
    Get
      CDE = MnCDE
    End Get
    Set(ByVal Value As String)
      MnCDE = Value
    End Set
  End Property
 
  Property IDka(ByVal i As Short) As Short
    Get
      IDka = MnIDka(i)
    End Get
    Set(ByVal Value As Short)
      ReDim Preserve MnIDka(i)
      MnIDka(i) = Value
    End Set
  End Property
  ReadOnly Property IDkaCount() As Short
    Get
      IDkaCount = -1
      If Not IsNothing(MnIDka) Then
        IDkaCount = UBound(MnIDka)
      End If
    End Get
  End Property
  '
  '
  'Labor
  '
  '
  '
  '
  Property LaborKbez() As String
    Get
      LaborKbez = MnLaborKbez
    End Get
    Set(ByVal Value As String)
      MnLaborKbez = Value
    End Set
  End Property
  Property LaborAbtc() As String
    Get
      LaborAbtc = MnLaborAbtc
    End Get
    Set(ByVal Value As String)
      MnLaborAbtc = Value
    End Set
  End Property
  Property LaborBau() As String
    Get
      LaborBau = MnLaborBau
    End Get
    Set(ByVal Value As String)
      MnLaborBau = Value
    End Set
  End Property
  Property LaborKst() As Integer
    Get
      LaborKst = MnLaborKst
    End Get
    Set(ByVal Value As Integer)
      MnLaborKst = Value
    End Set
  End Property
  Property LaborText1() As String
    Get
      LaborText1 = MnLaborText1
    End Get
    Set(ByVal Value As String)
      MnLaborText1 = Value
    End Set
  End Property
  Property LaborText2() As String
    Get
      LaborText2 = MnLaborText2
    End Get
    Set(ByVal Value As String)
      MnLaborText2 = Value
    End Set
  End Property
  Property LaborText3() As String
    Get
      LaborText3 = MnLaborText3
    End Get
    Set(ByVal Value As String)
      MnLaborText3 = Value
    End Set
  End Property
  Property QW() As CurvesRef
    Get
      QW = MnQW
    End Get
    Set(ByVal Value As CurvesRef)
      MnQW = Value
    End Set
  End Property
  Property QS() As CurvesRef
    Get
      QS = MnQS
    End Get
    Set(ByVal Value As CurvesRef)
      MnQS = Value
    End Set
  End Property

  Property QG() As CurvesRef
    Get
      QG = MnQG
    End Get
    Set(ByVal Value As CurvesRef)
      MnQG = Value
    End Set
  End Property

  Property QB() As CurvesRef
    Get
      QB = MnQB
    End Get
    Set(ByVal Value As CurvesRef)
      MnQB = Value
    End Set
  End Property
  Property QT() As CurvesRef
    Get
      QT = MnQT
    End Get
    Set(ByVal Value As CurvesRef)
      MnQT = Value
    End Set
  End Property

  Property MW() As CurvesRef
    Get
      MW = MnMW
    End Get
    Set(ByVal Value As CurvesRef)
      MnMW = Value
    End Set
  End Property
  Property MT() As CurvesRef
    Get
      MT = MnMT
    End Get
    Set(ByVal Value As CurvesRef)
      MnMT = Value
    End Set
  End Property

  Property MB() As CurvesRef
    Get
      MB = MnMB
    End Get
    Set(ByVal Value As CurvesRef)
      MnMB = Value
    End Set
  End Property

  Property MS() As CurvesRef
    Get
      MS = MnMS
    End Get
    Set(ByVal Value As CurvesRef)
      MnMS = Value
    End Set
  End Property

  Property MG() As CurvesRef
    Get
      MG = MnMG
    End Get
    Set(ByVal Value As CurvesRef)
      MnMG = Value
    End Set
  End Property

  Property Wist() As CurvesRef
    Get
      Wist = MnWist
    End Get
    Set(ByVal Value As CurvesRef)
      MnWist = Value
    End Set
  End Property
  Property RefGew As CurveRef
    Get
      RefGew = MnRefGew
    End Get
    Set(ByVal Value As CurveRef)
      MnRefGew = Value
    End Set
  End Property
  Property RefOne As CurveRef
    Get
      RefOne = MnRefOne
    End Get
    Set(ByVal Value As CurveRef)
      MnRefOne = Value
    End Set
  End Property
  Public Sub New()
    MyBase.New()
    '
    '
    '
    'Startwerte
    '
    '
    '
    '
    MnMeArtID = 0
    MnMeArtLock = -1
    MnMessgID = -1
    MnGKwrtID = -1
    MnBereichID = -1
    MnTbl = ""
    MnNormFile = ""
    MnCom = 1
    MnKenn = "XX"
    MnLbez = ""
    MnKbez = ""
    MnBaud = 9600
    MnLength = 8
    MnStopbit = 1
    MnPar = "N"
    MnImes = 1
    MnIkal = 1
    MnMessgKalMess = True
    MnDe = 10.0#
    MnProg = " "
    MnSetting = False
    MnKm = 0
    MnHands = 0
    MnDriver = "   "
    'UPGRADE_WARNING: Couldn't resolve default property of object MnLaborID. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
    MnLaborID = 1
    MnStell = 5
    MnTdiff = 30
    MnTop = 200
    '
    '
    MnMes = 0
    MnKal = 1
    MnNkal = 0
    MnIni = 1
    MnSond = 0
    MnKein = 1
    MnTwait = 100 '1 sec
    MnGlIn = True
    MnKa = 1
    MnExists = False

    MnGK(0) = 0.0#
    MnGK(1) = 0.0#
    MnGK(2) = 0.0#
    MnGK(3) = 0.0#
    MnGK(4) = 1.0#
    MnGK(5) = 0.0#
    MnGK(6) = 1.0#
    MnGK(7) = 0.0#
    MnGK(8) = 0.0#
    MnGK(9) = 1.0#
    MnGK(10) = 0.0#
    MnGK(11) = 0.0#
    MnGK(12) = 0.0#
    MnGK(13) = 0.0#
    MnGK(14) = 0.0#
    MnGK(15) = 0.0#
    MnCDE = "  "
    MnRefTra = "R"
    MnRwrtGID = 0 '
    MnUserRefGID = 0
    '
    '
    'Labor
    '
    '
    '
    '
    MnLaborKbez = ""
    MnLaborAbtc = ""
    MnLaborBau = ""
    MnLaborKst = 0
    MnLaborText1 = ""
    MnLaborText2 = ""
    MnLaborText3 = ""
    MnWinkel = New AngGeos
    MnQW = New CurvesRef
    MnQS = New CurvesRef
    MnQG = New CurvesRef
    MnQT = New CurvesRef
    MnQB = New CurvesRef
    MnMW = New CurvesRef
    MnMS = New CurvesRef
    MnMG = New CurvesRef
    MnMT = New CurvesRef
    MnMB = New CurvesRef
    MnWist = New CurvesRef
  End Sub

  Sub dispose() Implements IDisposable.Dispose
    MnWinkel = Nothing
    MnWist = Nothing
    MnQW = Nothing
    MnQS = Nothing
    MnQG = Nothing
    MnQT = Nothing
    MnQB = Nothing
    MnMS = Nothing
    MnMG = Nothing
    MnMT = Nothing
    MnMB = Nothing
    MnWist = Nothing
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As MeasParameters
    clone = CType(Me.MemberwiseClone, MeasParameters)
    '
    If Not MnWinkel Is Nothing Then
      clone.MnWinkel = MnWinkel.clone
    End If
    If Not MnRefGew Is Nothing Then
      clone.MnRefGew = MnRefGew.clone
    End If
    If Not MnRefOne Is Nothing Then
      clone.MnRefOne = MnRefOne.clone
    End If
    If Not MnWist Is Nothing Then
      clone.MnWist = MnWist.clone
    End If

    If Not MnQW Is Nothing Then
      clone.MnQW = MnQW.clone
    End If
    If Not MnQS Is Nothing Then
      clone.MnQS = MnQS.clone
    End If
    If Not MnQG Is Nothing Then
      clone.MnQG = MnQG.clone
    End If
    If Not MnQT Is Nothing Then
      clone.MnQT = MnQT.clone
    End If
    If Not MnQB Is Nothing Then
      clone.MnQB = MnQB.clone
    End If
    If Not MnMW Is Nothing Then
      clone.MnMW = MnMW.clone
    End If
    If Not MnMS Is Nothing Then
      clone.MnMS = MnMS.clone
    End If
    If Not MnMG Is Nothing Then
      clone.MnMG = MnMG.clone
    End If
    If Not MnMT Is Nothing Then
      clone.MnMT = MnMT.clone
    End If
    If Not MnMB Is Nothing Then
      clone.MnMB = MnMB.clone
    End If
  End Function

End Class