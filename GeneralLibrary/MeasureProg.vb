Option Strict Off
Option Compare Text
Option Explicit On
<ComClass(MeasureProg.ClassId, MeasureProg.InterfaceId, MeasureProg.EventsId)> _
Public Class MeasureProg

#Region "COM-GUIDs"
  ' Diese GUIDs stellen die COM-Identität für diese Klasse 
  ' und ihre COM-Schnittstellen bereit. Wenn Sie sie ändern, können vorhandene 
  ' Clients nicht mehr auf die Klasse zugreifen.
  Public Const ClassId As String = "f0b76ffb-430a-4d03-8113-09e11e7f0661"
  Public Const InterfaceId As String = "ccebefe6-c5fe-45e8-b8ef-9c93eccf08df"
  Public Const EventsId As String = "8a40af83-f597-4510-86b9-3363673c348f"
#End Region

  ' Eine erstellbare COM-Klasse muss eine Public Sub New() 
  ' ohne Parameter aufweisen. Andernfalls wird die Klasse 
  ' nicht in der COM-Registrierung registriert und kann nicht 
  ' über CreateObject erstellt werden.
  Dim MessgProg As String
  Dim MessgKenn As String
  Dim MessgCom As Short
  Dim MessgBaud As Integer
  Dim MessgLength As Short
  Dim MessgStop As Short
  Dim MessgPar As String
  Dim MessgHands As Short
  Dim MessgKM As Integer
  Dim MessgChrm() As String
  Dim MessgNwe As Integer
  Dim StWel As Integer
  Dim EnWel As Integer
  Dim ScWel As Integer
  'Dim Datafile As String = "[USERNAME]\RWERTE.TXT"
  Dim Datafile As String = "C:\USERS\UNTERFORSTHUBER\RWERTE.TXT"

  Dim DataReader As StreamReader
  Public Sub New()
    MyBase.New()

    Datafile = InputBox("Messwertdatei", "Dateiname für Reflexionswert", Datafile)

  End Sub
  Sub MesStart(ByRef MessgProgL As String, ByRef MessgKennL As String, ByRef MessgComL As Short, ByRef MessgBaudL As Integer, ByRef MessgLengthL As Short, ByRef MessgStopL As Short, ByRef MessgParL As String, ByRef MessgHandsL As Short, ByRef MessgKML As Integer, ByRef MessgChrmL() As String, ByRef MessgNweL As Integer, ByRef StWelL As Integer, ByRef EnWelL As Integer, ByRef ScWelL As Integer, ByRef istatL As Integer)
    Dim i As Integer
    'Parameterbeschreibung
    'Parameter                COLEINST-Systemeinstellungen-Messgeräte
    '
    'MessgProg(32 Zeichen)                spez. Kennung für Messgerät  
    'MessgKenn (2 Zeichen)                Kennung Messgerät (2 Zeichen)
    'MessgCom                             COMx (Nummer serielle Schnittstelle)
    'MessgBasud                           Baudrate
    'MessgLength                          Anzahl Bit pro Zeichen
    'MessgStop                            Anzahl Stopbits
    'MessgPAR(1 Zeichen)                  Parität
    'MessgHands                           Handshake
    'MessgKM                              Anzahl Winkel (bzw. Messgeometrien)  
    'MessgCHRM()                          Winkel bzw Messgeometrien (z.B. GO,GM oder 45)
    'MessgNWE                             Anzahl Wellenlänbgen
    'StWel                                Startwellenlänge
    'EnWel                                EndWellenlänge
    'ScWel                                Schrittweite für Wellenlänge
    'Istat                                Fehlermeldung (0 oder -999 ist in Ordnung)
    '
    '
    '
    'Mit diesem Programm können die Einstellungen überprüft werden
    'Rückgabewerte werden nicht übernommen (außer Fehlercode)
    MessgProg = MessgProgL
    MessgKenn = MessgKennL
    MessgCom = MessgComL
    MessgBaud = MessgBaudL
    MessgLength = MessgLengthL
    MessgStop = MessgStopL
    MessgPar = MessgParL
    MessgHands = MessgHandsL
    MessgKM = MessgKML
    ReDim MessgChrm(MessgKML - 1)
    For i = 0 To MessgKM - 1
      MessgChrm(i) = MessgChrmL(i)
    Next i
    MessgNwe = MessgNweL
    StWel = StWelL
    EnWel = EnWelL
    ScWel = ScWelL
  End Sub

  Sub SetIdka(ByRef Nidka As Integer, ByRef idka() As Short, ByRef istat As Integer)
    'Nidka   Anzahl Kalibriermessungen

    'IDKA  Art und Reihenfolge der Kalibrierung
    '1  Weißstandard
    '2  Schwarzstandard
    '3  Graustandard
    '4  Weissstandard (Transmission)
    '5  Schwarzstandard (Transmission)
    Nidka = 2
    idka(0) = 1
    idka(1) = 2
  End Sub
  Sub MesSon(ByRef istat As Integer)

  End Sub


  Sub MesKal(ByRef imess As Integer, ByRef kw As Integer, ByVal Chrm As String, ByRef MnReTr As Integer, ByRef rmes() As Single, ByRef istat As Integer)
    Dim i As Integer
    'Kalibrierung des Messgerätes
    'Werden bei der Messung bereits kalibrierte Messwerte ausgegeben, so muss für den Weißstandard rmes(i)=100. und für den Schwarzstandard rmes(i)=0.0 gesetzt werden
    'diese Werte werden bereits vom rufenden Programm vorgegeben!!!
    '
    'Imess Art der Kalibrierung (s. IDKA)
    'Imess+5 entsprechend für Nullkolibrieren 

    'Call EyeOneMes(imess, kw, rmes, istat)
    For i = 0 To MessgNwe - 1
      Select Case imess
        Case 1
          rmes(i) = 1.0
        Case 2
          rmes(i) = 0.0
      End Select
    Next


  End Sub
  Sub MesINI(ByRef MessgWist() As Single, ByRef istat As Integer)
    'MessWist  Istwellenlängen
    'werden vom rufenden Programm bereits als Istwellenlängen vorgegeben!!
    'Im Allg. handelt es sich dabei um die Sollwellenlängen 
    ''
  End Sub
  Sub MesMes(ByRef kw As Integer, ByRef Chrm As String, ByRef MnReTr As Integer, ByRef rmes() As Single, ByRef istat As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim ReadText() As String
    '
    'Messung durchführen
    '
    'kw     Nummer des Winkels bzw. der Messgeometrie für die die Messung durchgeführt werden soll
    'Chrm   dsgl. für Kennung Winkel oder Messgeometrie
    'MnReTr 0 Remissionsmessung; 1 Transmissionsmessung
    'rmes   Messwerte
    'Istat Fehlercode

    j = InputBox("Nummer der Messung", "Messung übernehmen", "1")
    DataReader = File.OpenText(Datafile)
    'DataReader.BaseStream.Seek(0, SeekOrigin.Begin)

    For i = 0 To j - 1
      ReadText = DataReader.ReadLine.Split(Chr(9))
    Next i

    'Call EyeOneMes(0, kw, rmes, istat)
    For i = 0 To MessgNwe - 1
      rmes(i) = 0.01 * CSng(ReadText(i))
    Next
    DataReader.Close()
  End Sub
  Sub MesEnd()
    'Ende 
  End Sub
 
End Class


