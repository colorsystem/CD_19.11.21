Option Compare Text
Option Strict Off
Option Explicit On
Imports Microsoft.Win32
Public Class SystemSettings
  Declare Function COLORCMP Lib "COLORCRP.DLL" (ByRef PROCKEY As Byte, ByRef COLKEY As Byte) As Long

  'Registry-Zugriff à la .NET
  'Die Common Language Runtime (CLR) des .NET Frameworks stellt Ihnen mehrere Klassen zur Verfügung, die im Zusammenspiel den Zugriff auf die sehr viel größere Welt der gesamten Registry deutlich einfacher macht als das Registry-API, das Sie für diesen Zweck unter Visual Basic 6 verwenden mussten (sofern Sie nicht auf Fremdhersteller-Tools zurückgegriffen haben).

  'In den beiden Klassen Microsoft.Win32.Registry und Microsoft.Win32.RegistryKey finden Sie alle Instrumente, die Sie zum Durchstöbern, Lesen und Schreiben von Registryzweigen und -schlüsseln benötigen. Importieren Sie den Namespace Microsoft.Win32, um sich beim Umgang mit der Registry Tipparbeit zu ersparen:

  ' Für alle weiteren Beispiele: 
  ' Namespace "Microsoft.Win32" zur abkürzenden 
  ' Schreibweise importieren: 
  'Die Klasse Registry ist sehr simpel aufgebaut: Sie bietet direkten Zugang zu den Hauptzweigen der Registry. Ihre hierfür angebotenen Objekte ClassesRoot, CurrentConfig, CurrentUser, DynData, LocalMachine, PerformanceData und Users sind bereits alles, was die Klasse anbietet. Jedes dieser Objekte ist vom Typ RegistryKey. In der Klasse RegistryKey ist die Funktionalität für den Registry-Zugriff über das .NET Framework gekapselt. Zugriffsrechte für Registryzweige lassen sich über die Klasse RegistryPermission ermitteln und setzen.

  'Einen Registryzweig öffnen 
  'Ausgehend von einer Instanz der Klasse RegistryKey werden sie einen Registryzweig üblicherweise zunächst öffnen wollen, um ihn auslesen oder bearbeiten zu können. Hierfür steht die Funktion OpenSubKey zur Verfügung, die wiederum ein Objekt des Typs RegistryKey zurückgibt. Somit können Sie verschachtelte Pfade in der Registry folgendermaßen öffnen:
  Shared Function RegRead(ByVal PathName As String, ByVal ValueName As String) As String
    Dim ColKey As RegistryKey
    '
    'HKEY_CURRENT_USER
    '
    '
    ColKey = Registry.CurrentUser.OpenSubKey(PathName)
    RegRead = ColKey.GetValue(ValueName)
    ColKey.Close()
  End Function
  'Registrykey speichern

  ' Schreibt den Wert aus "Value" als den Typ aus "Typ"
  ' in den in "Pathname" angegebenen Schlüssel
  Shared Function RegWrite(ByVal PathName As String, ByVal Valuename As String, ByVal value As String) As Boolean
    Dim ColKey As RegistryKey
    RegWrite = False
    Try
      ColKey = Registry.CurrentUser.CreateSubKey(PathName)
      ColKey.SetValue(Valuename, value)
      ColKey.Close()
      RegWrite = True
    Catch ex As Exception
    End Try
  End Function
  'Registrykey löschen

  ' Löscht den Schlüssel aus "PathName"
  Shared Function RegDelete(ByVal PathName As String, ByVal Valuename As String) As Boolean
    Dim ColKey As RegistryKey
    '
    'HKEY_CURRENT_USER
    '
    '
    RegDelete = False
    ColKey = Registry.CurrentUser.OpenSubKey(PathName, True)
    RegDelete = False
    Try
      ColKey.DeleteValue(Valuename)
      RegDelete = True
    Catch ex As Exception
    End Try
    ColKey.Close()
  End Function


  Function CMPCRYPT(ByRef PROCKEY As String, ByRef COLKEY As String) As Boolean
    '
    '
    'Vergleiche Schlüssel
    '
    '
    '
    Dim ColKY(16) As Byte
    Dim ProcKy(16) As Byte
    Dim i As Integer
    Dim j As Long
    For i = 1 To 16
      ColKY(i) = Asc(Mid(COLKEY, i, 1))
      ProcKy(i) = Asc(Mid(PROCKEY, i, 1))
    Next i
    j = COLORCMP(ProcKy(1), ColKY(1))
    If j = 0 Then
      CMPCRYPT = False
    Else
      CMPCRYPT = True
    End If
  End Function


  Shared Function getProcID(ByVal i As Integer) As String
    '
    'Verweis
    'System.Management
    '
    'ProcessorID
    '
    Dim j As Integer
    Dim objMOS As ManagementObjectSearcher

    Dim objMOC As Management.ManagementObjectCollection

    Dim objMO As Management.ManagementObject = Nothing

    'Declare following three object variables



    'Now, execute the query to get the results

    objMOS = New ManagementObjectSearcher("Select * From Win32_Processor")

    objMOC = objMOS.Get

    'Finally, get the CPU's id.
    j = 0
    getProcID = ""
    For Each objMO In objMOC
      getProcID = objMO("ProcessorID")
      If j = i Then Exit For
    Next

    'Dispose object variables.

    objMOS.Dispose()

    objMOS = Nothing

    objMO.Dispose()

    objMO = Nothing


  End Function

 

End Class
