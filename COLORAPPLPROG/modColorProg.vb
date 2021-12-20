Option Strict On
Option Explicit On
Option Compare Text

Module modColorProg
  
  Friend FormMDI As New frmColorMDI
  Public Aform As Object
  Public BasisKlasse As New clsColorBasis
  Public Printset As New PrinterSettings
  Public Measure As MeasureReflex
  Public GetPutReflex As HandleRwerte
  Public BasisProg As clsColorBasis
  Dim License As ColorLicenseKey
  
  Sub main()
    License = New ColorLicenseKey
    If Not License.CryptFind(1) Then
      Exit Sub
    End If
    If Today > #1/6/2016# Then
      'MsgBox("Zeitüberschreitung")
      'Exit Sub
    End If
    Try
      Cncol.Open()
      Cncol.Close()
    Catch ex As OleDbException
      MsgBox("OleDbErrorDescription: " & ex.ToString)
      MsgBox("OleDbErrorCode: " & CStr(ex.ErrorCode))
      MsgBox("OleDbErrorSource: " & ex.Source)
      MsgBox("OleDbErrorMessage: " & ex.Message)
      Exit Sub
    Catch ex As Exception
      MsgBox("ErrorDescription: " & ex.ToString)
      MsgBox("ErrorSource: " & ex.Source)
      MsgBox("ErrorMessage: " & ex.Message)
      MsgBox("CONNECTION COLORFILE FAILED")
      MsgBox(Cncol.ConnectionString)
      Exit Sub
    End Try
    Try
      Cndat.Open()
      Cndat.Close()
    Catch ex As OleDbException
      MsgBox("OleDbErrorDescription: " & ex.ToString)
      MsgBox("OleDbErrorCode: " & CStr(ex.ErrorCode))
      MsgBox("OleDbErrorSource: " & ex.Source)
      MsgBox("OleDbErrorMessage: " & ex.Message)
      Exit Sub
    Catch ex As Exception
      MsgBox("ErrorDescription: " & ex.ToString)
      MsgBox("ErrorSource: " & ex.Source)
      MsgBox("ErrorMessage: " & ex.Message)
      MsgBox("CONNECTION COLORDATA FAILED")
      MsgBox(Cndat.ConnectionString)
      Exit Sub
    End Try
    If Not File.Exists(Cnkal.DataSource) Then
      LetPrivSettings("STARTUP", "CNNKAL", COLORFileName) = Cndat.ConnectionString
    End If
    Try
      Cnkal.Open()
      Cnkal.Close()
    Catch ex As OleDbException
      MsgBox("OleDbErrorDescription: " & ex.ToString)
      MsgBox("OleDbErrorCode: " & CStr(ex.ErrorCode))
      MsgBox("OleDbErrorSource: " & ex.Source)
      MsgBox("OleDbErrorMessage: " & ex.Message)
      Exit Sub
    Catch ex As Exception
      MsgBox("ErrorDescription: " & ex.ToString)
      MsgBox("ErrorSource: " & ex.Source)
      MsgBox("ErrorMessage: " & ex.Message)
      MsgBox("CONNECTION COLORKALIB FAILED")
      MsgBox(Cndat.ConnectionString)
      Exit Sub
    End Try
    Try
      If Not File.Exists(CnTmp.DataSource) Then
        If Not CreateDatabase(CnTmp) Then
          MsgBox(Texxt(3760) & CnTmp.DataSource)
          Exit Sub
        End If
      End If
      CnTmp.Open()
      CnTmp.Close()
    Catch ex As OleDbException
      MsgBox("OleDbErrorDescription: " & ex.ToString)
      MsgBox("OleDbErrorCode: " & CStr(ex.ErrorCode))
      MsgBox("OleDbErrorSource: " & ex.Source)
      MsgBox("OleDbErrorMessage: " & ex.Message)
      MessageBox.Show(Texxt(9) & Space(1) & CnTmp.DataSource, Texxt(2000))

    Catch ex As Exception
      MsgBox("ErrorDescription: " & ex.ToString)
      MsgBox("ErrorSource: " & ex.Source)
      MsgBox("ErrorMessage: " & ex.Message)
      MsgBox("CONNECTION COLORTEMP FAILED")
      MsgBox(CnTmp.ConnectionString)
      Exit Sub
    End Try
    Measure = New MeasureReflex
    Application.Run(FormMDI)
    Application.Exit()
    'FormMDI.Dispose()
    Printset = Nothing
  End Sub


End Module

