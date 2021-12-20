Option Compare Text
Option Explicit On
Option Strict Off
'
'
Public Class ColSettings
  Dim FormBasis As New clsColorBasis
  Dim FrmEinst As Object
  Public Sub SysEinstellForm(ByRef Index As Integer, ByRef Iopenart As Boolean, ByRef UserUfo As Boolean, ByRef MDIForm As Form)
    If Not IsNothing(FrmEinst) Then
      FrmEinst.close()
    End If

    ' 
    '
    Select Case Index
      Case 0
        '
        '
        'Benutzer
        '
        '
        '
        FrmEinst = New frmUSE

        FrmEinst.iopenart = Iopenart
        FrmEinst.userufo = UserUfo
      Case 1
        '
        '
        'Messgeräte
        '
        '
        '
        FrmEinst = New frmMEI
        FrmEinst.iopenart = Iopenart
        FrmEinst.userufo = UserUfo
      Case 2
        '
        '
        'Anweisungen
        '
        '
        '
        FrmEinst = New frmANW
        FrmEinst.iopenart = Iopenart
        FrmEinst.userufo = UserUfo
      Case 3
        '
        '
        'Methoden
        '
        '
        '
        FrmEinst = New frmMTH
        FrmEinst.iopenart = Iopenart
        FrmEinst.userufo = UserUfo
      Case 4
        '
        '
        'Mischsysteme
        '
        '
        '
        FrmEinst = New frmMSY
        FrmEinst.iopenart = Iopenart
        FrmEinst.userufo = UserUfo
      Case 5
        '
        '
        'Labors
        '
        '
        '
        FrmEinst = New frmLAB
        'FrmEinst.iopenart = Iopenart
        'FrmEinst.userufo = UserUfo
      Case 6
        '
        '
        'Verschiedenes
        '
        '
        '
        FrmEinst = New frmMiscel
     
    End Select
    '
    '
    If IsNothing(MDIForm) Then
      FrmEinst.showdialog()
    Else
      
      Try

        FrmEinst.MdiParent = MDIForm
        FrmEinst.show()
        FrmEinst.WindowState = FormWindowState.Normal
        FrmEinst.WindowState = FormWindowState.Maximized
        Application.DoEvents()
      Catch ex As Exception

      End Try
    End If
    ''
    '
    'frmANW.openart = Iopenart
    'frmANW.ShowDialog()
    'frmMEI.ShowDialog()
    'frmMSY.ShowDialog()
    'FrmEinst = New frmUSE
    'FrmEinst.ShowDialog()
    ''Unload(frmGRZ)
  End Sub
  '
  '
  ''
  Public Sub EinstellForm(ByRef Index As Integer, ByRef UserID As Integer, ByRef MessgID As Integer, ByRef MethID As Integer, ByRef MischID As Integer, ByRef Iopenart As Boolean, ByRef UserUfo As Boolean, ByRef MDIForm As Form)
    Dim MessgRwID As Integer
    Dim SqlStmt As String
    Dim Dyset As OleDbDataReader
    Dim cmdset As New OleDbCommand("", Cncol)
    SqlStmt = "SELECT * FROM TBL_MESSG WHERE MESSG_ID=" & MessgID
    cmdset.CommandText = SqlStmt
    Dyset = DataReader(cmdset, CommandBehavior.CloseConnection & CommandBehavior.SingleRow)
    If Dyset.Read Then
      MessgRwID = Dyset("MESSGRW_ID")
    End If
    Dyset.Close()
    'Call OpenAll()
    If Not IsNothing(FrmEinst) Then
      FrmEinst.close()
    End If
    Select Case Index
      Case 0
        '
        '
        'Messgeräte konfigurieren
        '
        '
        '
        FrmEinst = New frmNWI
        FrmEinst.iopenart = Iopenart
        FrmEinst.UserID = UserID
        FrmEinst.MessgID = MessgID
        FrmEinst.MethID = MethID
        FrmEinst.MischID = MischID
      Case 1
        '
        '
        'Methoden konfigurieren
        '
        '
        '
        '
        ''
        '
        FrmEinst = New frmNOW
        FrmEinst.openart = Iopenart
        FrmEinst.UserID = UserID
        FrmEinst.MethID = MethID
        'Unload(frmNOW)
      Case 2
        '
        '
        'Mischsysteme (GK-Werte) konfigurieren
        '
        '
        FrmEinst = New frmNWU
        FrmEinst.iopenart = Iopenart
        FrmEinst.UserID = UserID
        FrmEinst.MessgID = MessgID
        FrmEinst.MessgrwID = MessgRwID
        FrmEinst.MethID = MethID
        FrmEinst.MischID = MischID
        'Unload(frmNWU)
      Case 3
        '
        '
        'Merkmale für Methoden installieren
        '
        '
        '
        '
        ''
        '
        FrmEinst = New frmMMR
        FrmEinst.openart = Iopenart
        FrmEinst.UserID = UserID
        FrmEinst.MethID = MethID

        'Unload(frmMMR)

      Case 4
        '
        '
        'Parameter für Methoden installieren
        '
        FrmEinst = New frmPAR
        FrmEinst.UserID = UserID
        FrmEinst.MethID = MethID
        'Unload(frmpar)
        '
        '
      Case 5
        '
        '
        'Gruppennamen R-Werte
        '
        '
        '
        '
        ''
        '
        FrmEinst = New frmGRW
        FrmEinst.Openart = Iopenart
        FrmEinst.MessgID = MessgID
        '
        ' Unload(frmGRW)

      Case 6
        '
        '
        'Gruppennamen Rezepte
        '
        '
        '
        '
        ''
        '
        FrmEinst = New frmGRZ
        FrmEinst.Openart = Iopenart
        FrmEinst.MischID = MischID
        FrmEinst.MessgID = MessgID
        'Unload(frmGRZ)
        '

    End Select
    If IsNothing(MDIForm) Then
      FrmEinst.showdialog()
    Else
      Try
        FrmEinst.MdiParent = MDIForm



        FrmEinst.Show()
        FrmEinst.WindowState = FormWindowState.Normal
        FrmEinst.WindowState = FormWindowState.Maximized
        Application.DoEvents()
      Catch ex As Exception

      End Try
     
    End If
    'MsgBox("End Program")

  End Sub



  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    '
    '
    '
    ''
  End Sub
  Public Sub ExtEinstellForm(ByRef Index As Integer, ByRef UserID As Integer, ByRef MessgID As Integer, ByRef MethID As Integer, ByRef MischID As Integer, ByRef Iopenart As Boolean, ByRef MDIForm As Form)
    If Not IsNothing(FrmEinst) Then
      FrmEinst.close()
    End If
    Select Case Index
      Case 0
        '
        '
        'R-Werte löschen/korrigieren/kopieren/einfügen
        '
        '
        '

        FrmEinst = New frmDARF
        FrmEinst.iopenart = Iopenart
        FrmEinst.messgID = MessgID
        FrmEinst.MethID = MethID
        FrmEinst.MischID = MischID
        FrmEinst.UserID = UserID
        FrmEinst.controlbox = True
        FrmEinst.formborderstyle = FormBorderStyle.Sizable

      Case 1
        '
        '
        'Rezepte löschen/korrigieren/kopieren/einfügen
        '
        '
        '
        FrmEinst = New frmDARZ
        FrmEinst.iopenart = Iopenart
        FrmEinst.messgID = MessgID
        FrmEinst.MethID = MethID
        FrmEinst.MischID = MischID
        FrmEinst.UserID = UserID
        FrmEinst.controlbox = True
        FrmEinst.formborderstyle = FormBorderStyle.Sizable
    End Select
    
      Try
        FrmEinst.MdiParent = MDIForm
        Application.DoEvents()
        FrmEinst.Show()
      FrmEinst.WindowState = FormWindowState.Maximized
      Catch ex As Exception

      End Try

  End Sub
  Public Sub Kopieren(ByRef Ende As Boolean)
    Dim imsg As Short
    Dim DbName As String
    Dim FilNaa As String
    Ende = False
    DbName = Cndat.DataSource
    imsg = MsgBox(Texxt(2929), 4, Texxt(2000))

    If imsg = 7 Then
      FilNaa = ""
      Do
        FilNaa = InputBox(Texxt(3675), Texxt(2000), "BCSDNE.MDB")
        If FilNaa = "" Or InStr(FilNaa, "*") > 0 Then
          imsg = MsgBox(Texxt(3615) & Chr(13) & Texxt(1999), 4, Texxt(2000))
          If imsg = 7 Then
            Ende = True
            Exit Sub
          End If
        Else
          Exit Do
        End If
      Loop

      If Not File.Exists(FilNaa) Then
        Err.Clear()
        Try

          File.Copy(DbName, FilNaa, False)
          MsgBox(Texxt(3611))
        Catch ex As Exception
          MsgBox(Texxt(3612) & Space(2) & ex.Message)
          Exit Sub
        End Try
      Else
        MsgBox(Texxt(3616))
      End If
    End If
  End Sub
End Class
Public Class HandleMerk
  
  Dim i As Integer
  Dim j As Integer
  Dim SqlStmt As String
  Dim hlf As Object
  Dim ier As Integer
  '

  Dim MnRowMax As Integer
  Dim MnCancel As Integer
  Dim MnXarrayStart() As Object
  Dim MnUser As Boolean
  Dim ColMax As Integer
  

 
End Class



