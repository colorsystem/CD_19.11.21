Option Strict Off
Option Explicit On
Option Compare Text
'UPGRADE_WARNING: Class instancing was changed to public. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="ED41034B-3890-49FC-8076-BD6FC2F42A85"'
Public Class ColorHilfLib
  'UPGRADE_ISSUE: Declaring a parameter 'As Any' is not supported. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="FAE78A8D-8978-4FD4-8208-5B7324A8F795"'
  Private Declare Function MoveWindow Lib "user32" (ByVal hwnd As Integer, ByVal X As Integer, ByVal Y As Integer, ByVal nw As Integer, ByVal nh As Integer, ByVal brepaint As Integer) As Integer
  Private Declare Function GetActiveWindow Lib "user32" () As Integer
  Private Declare Function GetDesktopWindow Lib "user32" () As Integer
  'UPGRADE_WARNING: Structure WU_RECT may require marshalling attributes to be passed as an argument in this Declare statement. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="C429C3A5-5D47-4CD9-8F51-74A1616405DC"'
  Private Declare Function GetWindowRect Lib "user32" (ByVal hwnd As Integer, ByRef lpRect As WU_RECT) As Integer
  Private Declare Function FindWindow Lib "user32" Alias "FindWindowA" (ByVal lpClassName As String, ByVal lpWindowName As String) As Integer
  Private Declare Function WaitForSingleObject Lib "kernel32" (ByVal hHandle As Integer, ByVal dwMilliseconds As Integer) As Integer
  'UPGRADE_WARNING: Structure PROCESS_INFORMATION may require marshalling attributes to be passed as an argument in this Declare statement. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="C429C3A5-5D47-4CD9-8F51-74A1616405DC"'
  'UPGRADE_WARNING: Structure STARTUPINFO may require marshalling attributes to be passed as an argument in this Declare statement. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_vsexpcc/scripts/redirect.htm?keyword="C429C3A5-5D47-4CD9-8F51-74A1616405DC"'
  Private Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Integer) As Integer
  Private Declare Function GetExitCodeProcess Lib "kernel32" (ByVal hProcess As Integer, ByRef lpExitCode As Integer) As Integer
  Private Const NORMAL_PRIORITY_CLASS As Integer = &H20

  '
  '

  Private Structure WU_RECT
    Dim x1 As Integer
    Dim y1 As Integer
    Dim x2 As Integer
    Dim y2 As Integer
  End Structure

  '
  Public Shared XYZWNL65() As Single = {94.801, 100.0, 107.338} 'Weißwerte für NLD65 10Grad

  Public Shared farbini(15) As Color


  Function ContTabName(ByRef TabName As String, ByRef SubTabName As String) As Boolean
    '
    '
    'Ist SUBTABNAME in TABNAME enthalten?
    '
    '
    '
    '
    Dim j As Short
    Dim Name As String
    Dim SubName As String
    ContTabName = False
    Name = TabName & "_"
    SubName = "_" & SubTabName & "_"
    j = InStr(Name, SubName)
    If j <> 0 Then
      ContTabName = True
    End If
  End Function

  Shared Function NstrKen(ByRef i As Short) As String
    Static chki As Boolean = False
    '
    '
    's. auch Texxt(1200 ff)
    '
    '
    '
    Static Teex(50) As String
    If Not chki Then
      Teex(0) = "@UMW"
      Teex(1) = "@UMS"
      Teex(2) = "@WMW"
      Teex(3) = "@SMS"
      Teex(4) = "@GMG"
      Teex(5) = "@TMW"
      Teex(6) = "@PMW"
      Teex(7) = "@TMW"
      Teex(8) = "@PMW"
      Teex(9) = "@TMS"
      Teex(10) = "@PMS"
      Teex(11) = "@TMW"
      Teex(12) = "@PMW"
      Teex(13) = "@TMS"
      Teex(14) = "@PMS"
      Teex(15) = "@TMW"
      Teex(16) = "ATMW"
      Teex(17) = "    "
      Teex(18) = "ATMW"
      Teex(19) = "BTMW"
      Teex(20) = "    "
      Teex(21) = "@PMW"
      Teex(22) = "APMW"
      Teex(23) = "    "
      Teex(24) = "APMW"
      Teex(25) = "BPMW"
      Teex(26) = "@TMW"
      Teex(27) = "@TMW"
      Teex(28) = "    "
      Teex(29) = "    "
      Teex(30) = "@TMW"
      Teex(31) = "@TMS"
      Teex(32) = "@TMW"
      Teex(33) = "@PMW"
      Teex(34) = "@PMS"
      Teex(35) = "@PMW"
      Teex(36) = "    "
      Teex(37) = "    "
      Teex(38) = "    "
      Teex(39) = "@TMS"
      Teex(40) = "@PMS    "

      chki = True
    End If
    NstrKen = Teex(i)
  End Function


  
  Shared Sub HorRGB(ByRef HoRGB() As Single)
    HoRGB(0) = CSng(GetPrivSettings("EXPONENT", "HOR", CStr(2.4), COLORFileName()))
    HoRGB(1) = CSng(GetPrivSettings("EXPONENT", "HOG", CStr(2.4), COLORFileName()))
    HoRGB(2) = CSng(GetPrivSettings("EXPONENT", "HOB", CStr(2.4), COLORFileName()))
  End Sub
  Shared Sub RGBDar(ByRef RoGrBl() As Integer, ByRef XYZ() As Single, ByRef xwywzw() As Single)
    Static A(2, 2) As Single
    Static Ia As Boolean = False
    Static HoRGB(2) As Single
    If Not Ia Then
      Call ReadAHoRGB(A, HoRGB)
      Ia = True
    End If
    Call XYZFromRGB(RoGrBl, XYZ, xwywzw, A, HoRGB)
  End Sub

  Shared Sub FarbDar(ByRef RoGrBl() As Integer, ByRef XYZ() As Single, ByRef xwywzw() As Single)
    Static A(2, 2) As Single
    Static Ia As Boolean = False
    Static HoRGB(2) As Single
    If Not Ia Then
      Call ReadAHoRGB(A, HoRGB)
      Ia = True
    End If
    Call RGBFromXYZ(RoGrBl, XYZ, xwywzw, A, HoRGB)
  End Sub
  Shared Sub ReadAHoRGB(ByRef A(,) As Single, ByRef HoRGB() As Single)
    Dim Adef As Object
    Dim Ahlf As Single
    Dim Keey As String
    Dim i As Short
    Dim j As Short
    'sRGB (D65)
    'Adef = New Single() {3.24071, -1.53726, -0.498571, -0.969258, 1.87599, 0.0415557, 0.0556352, -0.203996, 1.05707}
    'ADOBE RGB(D65)
    'Adef = New Single() {2.04148, -0.564977, -0.344713, -0.969258, 1.87599, 0.0415557, 0.0134455, -0.118373, 1.01527}
    'Apple RGB(D65)
    Adef = New Single() {2.95176, -1.28951, -0.47388, -1.0851, 1.99084, 0.0372023, 0.0854804, -0.269456, 1.09113}

    Call HorRGB(HoRGB)
    For i = 0 To 2
      For j = 0 To 2
        Keey = "A" & Format(i + 1, "#") & Format(j + 1, "#")
        Ahlf = Singl(GetPrivSettings("MATRIX", Keey, CStr(Adef(i * 3 + j)), COLORFileName()))
        A(i, j) = Ahlf
      Next j
    Next i
  End Sub
  
  Shared Sub WriteAHoRGB(ByRef A(,) As Single)
    Dim Keey As String
    Dim i As Short
    Dim j As Short

    For i = 0 To 2
      For j = 0 To 2
        Keey = "A" & Format(i + 1, "#") & Format(j + 1, "#")
        '
        '
        LetPrivSettings("MATRIX", Keey, COLORFileName()) = CStr(A(i, j))
      Next j
    Next i
  End Sub
  Shared Sub RGBFromXYZ(ByRef RoGrBl() As Integer, ByRef XYZ() As Single, ByRef xwywzw() As Single, ByRef A(,) As Single, ByRef HoRGB() As Single)
    Dim xyzN(2) As Single
    Dim Ahlf As Single
    Dim i As Integer
    Dim j As Integer

    'XYZ to RGB www.tecgraf.org/~mgattass/color/XYZtoRGB.htm


    'RGB()

    'The most famous color encoding is an aditive system with 3 components, Red, Green and Blue.  Computer monitors emit color as RGB.



    'XYZ()

    'The XYZ color system, also called “norm color system”.  It is superset of the RGB color system.  It uses tristimulus values when encoding.  X, Y and Z are all calculated through color-matching functions and are always positive.  It was a system created by CIE.  The conversion is obtained through a matrix operation after a simple adjustment of R, G and B:



    '[r’ g’ b’] = [X Y Z] [M]-1



    'RGB values from 0 to 1

    'X() 'YZ values from 0 to tristimulus reference


    'double ref_X = _ccTristimulusValues[ObsIll][ccX]; 

    'double ref_Y = _ccTristimulusValues[ObsIll][ccY];

    'double ref_Z = _ccTristimulusValues[ObsIll][ccZ];
    'double var_X = x / 100;        //X = From 0 to ref_X

    'double var_Y = y / 100;        //Y = From 0 to ref_Y

    'double var_Z = z / 100;        //Z = From 0 to ref_Y



    'double var_R = var_X *  3.2406 + var_Y * -1.5372 + var_Z * -0.4986;

    'double var_G = var_X * -0.9689 + var_Y *  1.8758 + var_Z *  0.0415;

    'double var_B = var_X *  0.0557 + var_Y * -0.2040 + var_Z *  1.0570;

    '*r = var_R;

    '*g = var_G;

    '*b = var_B;
    'xwywzw FAKT für Normlichtart
    For i = 0 To 2
      xyzN(i) = XYZWNL65(i) * XYZ(i) / (100.0 * xwywzw(i))
    Next i
    For i = 0 To 2
      Ahlf = 0.0
      For j = 0 To 2
        Ahlf = Ahlf + A(i, j) * xyzN(j)
      Next j
      If Ahlf > 0.0031308 Then
        Ahlf = 1.055 * Ahlf ^ (1.0 / HoRGB(i)) - 0.055
      Else
        Ahlf = 12.92 * Ahlf
      End If
      RoGrBl(i) = CInt(255 * Ahlf)
      If RoGrBl(i) < 0 Then
        RoGrBl(i) = 0
      End If
      If RoGrBl(i) > 255 Then
        RoGrBl(i) = 255
      End If
    Next i
  End Sub
  Shared Sub XYZFromRGB(ByRef RoGrBl() As Integer, ByRef XYZ() As Single, ByRef xwywzw() As Single, ByRef A(,) As Single, ByRef HoRGB() As Single)
    Dim xyzN(0, 2) As Double
    Dim AA(2, 2) As Double
    Dim Ahlf As Single
    Dim TAU As Double = 0.0000001
    Dim i As Integer
    Dim j As Integer
    Dim krank As Integer
    Dim ier As Integer
    For i = 0 To 2
      Ahlf = CSng(RoGrBl(i)) / 255.0
      If Ahlf < 0.00024232198 Then
        Ahlf = Ahlf / 12.92
      Else
        Ahlf = ((Ahlf + 0.055) / 1.055) ^ HoRGB(i)
      End If
      xyzN(0, i) = Ahlf
      For j = 0 To 2
        AA(i, j) = A(j, i)
      Next
    Next
    Call MATInvers(3, 3, AA, 3, 1, xyzN, TAU, krank, ier)
    For i = 0 To 2
      XYZ(i) = 100.0 * xyzN(0, i) * xwywzw(i) / XYZWNL65(i)
    Next
  End Sub
  Public Shared Function ExecCmd(ByRef cmdline As String, ByRef dwWait As Boolean) As Integer
    ExecCmd = Shell(cmdline, dwWait)

  End Function

  Shared Function RepTexxt(ByRef text As String, ByRef RepText As String) As Object
    Dim i As Short
    Dim j As Short
    j = 1
    RepTexxt = ""
    For i = 1 To Len(text)
      If Mid(text, i, 1) = "$" Then
        RepTexxt = RepTexxt & Mid(text, j, i - j) & RepText
        j = i + 1
      End If
    Next i
    If j <= Len(text) Then
      RepTexxt = RepTexxt + Mid(text, j, Len(text) - j + 1)
    End If
  End Function
  Shared Sub FormSize()
    Dim hwnd As Integer
    Dim idum As Integer
    Dim Ttop As Integer
    Dim Wwid As Integer
    Dim WU As WU_RECT
    hwnd = GetActiveWindow()
    idum = WuGetscreensize(WU)
    Ttop = 0.5 * (WU.y2 - 480)
    Wwid = 0.5 * (WU.x2 - 640)
    idum = MoveWindow(hwnd, Ttop, Wwid, 640, 480, True)
  End Sub
  Shared Function IsZiffer(ByRef text As String) As Boolean
    Dim i As Short
    IsZiffer = True
    For i = 0 To Len(text) - 1
      If Asc(text.Substring(i, 1)) < 48 Or Asc(text.Substring(i, 1)) > 57 Then
        IsZiffer = False
        Exit Function
      End If
    Next i
  End Function
  Shared Sub ResizeChild(ByRef ChildForm As Form)
    If IsNothing(ChildForm.ParentForm) Then Exit Sub
    With ChildForm
      '
      'Muss bei Child-Formularen im Load-Ereignis stehen
      '
      .FormBorderStyle = Windows.Forms.FormBorderStyle.None
      .MaximizeBox = False
      .MinimizeBox = False
      .StartPosition = FormStartPosition.CenterScreen

      'Remove the control box so the form will only display client area.

      .ControlBox = False
      .MaximizeBox = False
      .MinimizeBox = False
      .ControlBox = False
      .WindowState = FormWindowState.Maximized


    End With
    Application.DoEvents()
  End Sub



  Private Shared Function WuGetscreensize(ByRef r As WU_RECT) As Integer
    Dim hwnd, f As Integer
    hwnd = GetDesktopWindow()
    f = GetWindowRect(hwnd, r)
    WuGetscreensize = f
  End Function
  Shared Function HandleNullString(ByRef Kopp As Object) As String
    If IsDBNull(Kopp) Then
      HandleNullString = " "
    Else
      HandleNullString = Kopp
    End If
  End Function
  Shared Function RefName(ByRef RefMaxID As Integer, ByRef text As String, ByVal Stell As Short) As String
    If Stell > 10 Then Stell = 10
    RefName = Format(RefMaxID, "0000000000").Substring(10 - Stell)
    If Len(text) > Stell Then
      RefName = RefName.Substring(0, Stell) & text.Substring(Stell)
    End If
  End Function

  Shared Function StrFak10(ByVal i As Short) As String
    Dim ivtw As Integer
    Dim vtw() As Single
    Dim vtt As Single
    ivtw = 35
    ReDim vtw(ivtw)
    vtw(0) = 0.0#
    vtw(1) = 0.00001
    vtw(2) = 0.00002
    vtw(3) = 0.00005
    vtw(4) = 0.0001
    vtw(5) = 0.0002
    vtw(6) = 0.0005
    vtw(7) = 0.001
    vtw(8) = 0.002
    vtw(9) = 0.005
    vtw(10) = 0.01
    vtw(11) = 0.02
    vtw(12) = 0.05
    vtw(13) = 0.1
    vtw(14) = 0.2
    vtw(15) = 0.5
    vtw(16) = 0.7
    vtw(17) = 0.8
    vtw(18) = 0.9
    vtw(19) = 0.95
    vtw(20) = 1.0#
    vtw(21) = 2.0#
    vtw(22) = 5.0#
    vtw(23) = 10.0#
    vtw(24) = 20.0#
    vtw(25) = 50.0#
    vtw(26) = 100.0#
    vtw(27) = 200.0#
    vtw(28) = 500.0#
    vtw(29) = 1000.0#
    vtw(30) = 2000.0#
    vtw(31) = 5000.0#
    vtw(32) = 10000.0#
    vtw(33) = 100000.0#
    vtw(34) = 1000000.0#
    vtw(35) = 10000000.0#
    If i < 0 Or i > ivtw Then
      vtt = 0.0
    Else
      vtt = vtw(i)
    End If
    StrFak10 = Format(vtt, "####0.00######;;00000.00")
  End Function

  Shared Function ChngChar(ByRef text As String, ByRef Cko As String, ByRef Cpu As String) As String
    Dim i As Short
    ChngChar = text
    i = InStr(ChngChar, Cko)
    If i <> 0 Then
      Mid(ChngChar, i, 1) = Cpu
    End If
  End Function


  Shared Function MrkID(ByRef ccc As String) As Short
    Dim ID As Short
    Dim I1 As Short
    Dim I2 As Short
    ID = Asc(ccc.Substring(0, 1))
    I1 = -1
    I2 = -1
    If ID >= 64 And ID <= 90 Then
      'Buchstaben(groß)
      I1 = ID - 64
    ElseIf ID >= 33 And ID <= 63 Then
      'Sonderzeichen
      I1 = 64 + ID
    End If
    ID = Asc(ccc.Substring(1, 1))
    If ID >= 64 And ID <= 90 Then
      '  Buchstaben(groß)
      I2 = ID - 64
    ElseIf ID >= 48 And ID <= 57 Then
      '  Ziffern
      I2 = ID - 21
    ElseIf ID >= 97 And ID <= 124 Then
      ' kleine Buchstaben
      '
      I2 = ID - 60
    End If
    MrkID = 64 * I1 + I2
  End Function
  Shared Function MrkCH(ByRef MerkID As Short) As String
    Dim C1 As String
    Dim C2 As String
    Dim ID As Short
    C1 = ""
    C2 = ""
    ID = Fix((MerkID - 1) / 64)
    If ID >= 0 And ID <= 26 Then
      'Buchstaben
      C1 = Chr(64 + ID)
    ElseIf ID >= 97 And ID <= 127 Then
      'Sonderzeichen
      C1 = Chr(ID - 64)
    End If
    ID = MerkID - 64 * ID
    If ID >= 0 And ID <= 26 Then
      '
      'große Buchstaben
      '
      C2 = Chr(64 + ID)
    ElseIf ID >= 27 And ID <= 36 Then
      '
      'Ziffern
      '
      C2 = Chr(21 + ID)
    ElseIf ID >= 37 And ID <= 64 Then
      '
      'kleine Buchstaben
      '
      C2 = Chr(60 + ID)
    End If
    MrkCH = C1 & C2
  End Function
  Shared Function Sline(ByRef N As Short, ByRef Zei As String) As String
    Dim i As Short
    Sline = ""
    For i = 0 To N - 1
      Sline = Sline & Zei
    Next i
  End Function
  '
  Shared Function MessageOut(ByRef MessgKalMess As Boolean, ByRef ErrNr As Short, ByRef Ttext As String, ByRef im As MessageBoxButtons, ByRef MeldText As String, ByRef NamLogFile As String) As MessageBoxButtons
    'Dim logfile As String
    If MessgKalMess OrElse IsNothing(NamLogFile) Then
      MessageOut = MessageBox.Show(CStr(ErrNr) & Space(2) & Ttext, MeldText, im)
      'MessageOut = MsgBox(CStr(ErrNr) & Space(2) & Ttext, im, MeldText)
    Else
      '
      '
      '
      'Logdatei öffnen
      '
      '
      '
      Try

        FileOpen(9, NamLogFile, OpenMode.Append)
        '
        '
        'Fehlerausgabe
        '
        '
        '
        PrintLine(9, ErrNr & Chr(9) & Now & Chr(9) & Ttext)

        FileClose(9)
      Catch ex As Exception
        MessageBox.Show("Logfile doesn't exist")
      End Try
      MessageOut = 10
    End If
  End Function
  '



  Shared Function cjnt(ByRef Zeich As Object) As Short
    If Zeich = "" Then
      cjnt = 997
      Exit Function
    End If
    Try
      cjnt = CShort(Zeich)
    Catch
      cjnt = 997
    End Try
    Exit Function
  End Function
  Shared Function TriSp(ByRef text As String) As String
    Dim i As Short
    Dim ccc As String
    TriSp = ""
    For i = 0 To Len(text) - 1
      ccc = text.Substring(i, 1)
      If ccc <> " " Then
        TriSp = TriSp & ccc
      End If
    Next i
  End Function


  Shared Function Chrum(ByRef ihrmWin As Short) As String
    Static ii As Short
    Static ij As Short
    '
    '
    'Integer in zwei oder drei Zeichen umwandeln
    '
    If ihrmWin < 1000 Then
      Chrum = Format(ihrmWin, "000")
      If Chrum.Substring(0, 1) = "0" Then
        Mid(Chrum, 1, 1) = " "
      ElseIf ihrmWin < 0 Then
        Chrum = Format(ihrmWin, "00")
      End If
    Else
      ii = ihrmWin Mod 256
      ij = ihrmWin / 256
      Chrum = " " & Chr(ii) & Chr(ij)
    End If
  End Function

  Shared Function ihrum(ByRef CHRM As String) As Short
    Static ii As Short
    Static ij As Short
    If CHRM <> "" And Len(CHRM) > 1 Then
      ii = Asc(Mid(Left(CHRM, 3), 1, 1))
      ij = Asc(Mid(Left(CHRM, 3), 2, 2))
      If ii < 58 And ii > 47 And ij < 58 And ij > 47 Then
        ihrum = CShort(Mid(Left(CHRM, 3), 1, 3))
      ElseIf ii = 45 Then
        '- Zeichen gefunden
        ihrum = CShort(Mid(Left(CHRM, 3), 1, 3))
      Else
        '
        '
        'Zeichen in Integer
        '
        '
        ii = Asc(CHRM.Substring(0, 1))
        ihrum = 256 * Asc(CHRM.Substring(1, 1)) + ii

      End If
    End If
  End Function




  Shared Function RezFont(ByVal i As Integer) As Font
    Dim FontName As String = ""
    Dim ff As FontFamily
    Select Case i

      Case 0
        FontName = "Microsoft SANS SERIF"
        FontName = GetPrivSettings("FONTS", "FONTGENL", FontName, COLORFileName())
      Case 1
        '
        'Symbole
        '
        FontName = "SYMBOL"
        FontName = GetPrivSettings("FONTS", "FONTSYMB", FontName, COLORFileName())
      Case 2
        '
        'Proportionalschrift
        '
        FontName = "ARIAL"
        FontName = GetPrivSettings("FONTS", "FONTPROP", FontName, COLORFileName())
      Case 3
        '
        'Nicht-Proportionalschrift
        '
        '
        FontName = "COURIER NEW"
        FontName = GetPrivSettings("FONTS", "FONTNONP", FontName, COLORFileName())
    End Select
    RezFont = New Font("MS SANS SERIF", 10, FontStyle.Regular)
    For Each ff In System.Drawing.FontFamily.Families
      If ff.Name = FontName Then
        RezFont = New Font(ff, 10, FontStyle.Regular)
        Exit Function
      End If
    Next
  End Function

  Shared Function FileData(ByRef FileName As String) As String
    Dim Laeng As Short
    Dim DBNam As String
    Dim FileSystemPro As FileSystemProxy = Nothing
    Laeng = Len(Trim(FileSystemPro.CurrentDirectory))
    If Mid(FileSystemPro.CurrentDirectory, Laeng - 4, 5) <> "\PROG" Then
      If Mid(FileSystemPro.CurrentDirectory, Laeng - 4, 5) = "\PRGM" Then
        DBNam = Mid(FileSystemPro.CurrentDirectory, 1, Laeng - 4) & "DATA" & "\" & FileName
      Else
        DBNam = FileSystemPro.CurrentDirectory & "\DATA\BCSDAT.MDB"
      End If
    Else
      DBNam = Mid(FileSystemPro.CurrentDirectory, 1, Laeng - 4) & "DATA" & "\" & FileName
    End If
    FileData = DBNam
  End Function
  Shared Function TmPName(ByRef ColName As String) As String
    TmPName = Environment.GetEnvironmentVariable("tmp")
    If TmPName = "" Then
      TmPName = Environment.GetEnvironmentVariable("temp")
    End If
    If TmPName = "" Then
      TmPName = "c:\tmp"
    End If
    TmPName = TmPName & "\" & ColName
  End Function

  Shared Function COLORFileName() As String
    Dim COLFile As String
    Dim MyFile As String
    COLORFileName = ""
    MyFile = "COLORFILE.INI"
    COLFile = Environment.CurrentDirectory & "\" & MyFile
    If File.Exists(COLFile) Then
      COLORFileName = COLFile
      Exit Function
    End If
    COLFile = Environment.GetEnvironmentVariable("COLORWIN") & "\" & MyFile
    If File.Exists(COLFile) Then
      COLORFileName = COLFile
      Exit Function
    End If
    COLFile = Environment.GetEnvironmentVariable("USERPROFILE") & "\" & MyFile
    If File.Exists(COLFile) Then
      COLORFileName = COLFile
      Exit Function
    End If
    COLFile = Environment.GetEnvironmentVariable("WINDIR") & "\" & MyFile
    If File.Exists(COLFile) Then
      COLORFileName = COLFile
      Exit Function
    End If
  End Function
  Shared Function GridFileName() As String
    Dim GrdName As String
    GrdName = TmPName("GRLAYOUT.GRD")
    GridFileName = GetFileProfile(GetPrivSettings("STARTUP", "GRIDFIL", GrdName, COLORFileName()))
  End Function
  Shared Function LogFileName() As String
    Dim LogName As String
    LogName = TmPName("FILELOG.LOG")
    LogFileName = GetFileProfile(GetPrivSettings("STARTUP", "LOGFIL", LogName, COLORFileName()))
  End Function

  Shared Sub Farbsetting()
    Dim Ifa As Long
    Dim i As Integer
    Dim IfaDum(15) As Long
    IfaDum(0) = 254
    IfaDum(1) = 65024
    IfaDum(2) = 16646144
    IfaDum(3) = 40702
    IfaDum(4) = 12011168
    IfaDum(5) = 35370
    IfaDum(6) = 16646398
    IfaDum(7) = 12875430
    IfaDum(8) = 33406
    IfaDum(9) = 0
    IfaDum(10) = 0
    IfaDum(11) = 0
    IfaDum(12) = 0
    IfaDum(13) = 0
    IfaDum(14) = 0
    IfaDum(15) = 0

    For i = 0 To farbini.Count - 1
      'Ifa = RGB(128 * (i Mod 2) + 127 * (i / 8 Mod 2), 128 * (i / 2 Mod 2) + 127 * (i / 16 Mod 2), 128 * (i / 4 Mod 2) + 127 * (i / 32 Mod 2))
      Ifa = IfaDum(i)
      farbini(i) = Color.FromArgb(CInt(GetPrivSettings("COLOR", "COLOR" & CStr(i + 1), CStr(Ifa), COLORFileName())))
      farbini(i) = Color.FromArgb(255, farbini(i))
    Next i
  End Sub
  Sub FarbComp(ByRef Farbi As Integer, ByRef ired As Short, ByRef igreen As Short, ByRef iblue As Short)
    Dim ihlf As Integer
    ihlf = Farbi Mod 256&
    ired = ihlf
    ihlf = (Farbi Mod 256& * 256&) / 256&
    igreen = ihlf
    ihlf = Farbi / (256& * 256&)
    iblue = ihlf
  End Sub

  Shared Sub New()
    Call Farbsetting()
  End Sub
  Shared Sub TestBeziehungen(conn As OleDbConnection, TblMaster As String, FieldsMaster() As String, TblSlave As String, FieldsSlave() As String, ByRef DelAut As Boolean)
    Dim i As Integer
    Dim j As Integer
    Dim MastSort() As Object
    Dim Sort As String
    Dim SqlDelete As String
    Dim RowfilterTextslave As String
    Dim ViewMaster As DataView
    Dim Viewslave As DataView
    Dim TablMaster As DataTable
    Dim TablSlave As DataTable
    Dim AdaptMaster As OleDbDataAdapter
    Dim AdaptSlave As OleDbDataAdapter
    Dim DeleteCommand As OleDbCommand
    Dim PkCols() As DataColumn
    TablMaster = New DataTable
    TablSlave = New DataTable
    AdaptMaster = New OleDbDataAdapter
    AdaptSlave = New OleDbDataAdapter
    DeleteCommand = New OleDbCommand("", conn)
    If FieldsMaster.Count <> FieldsSlave.Count Then
      MsgBox("Anzahl Felder verschieden groß")
      Exit Sub
    End If
    AdaptMaster.SelectCommand = New OleDbCommand("SELECT * FROM " & TblMaster, conn)
    AdaptSlave.SelectCommand = New OleDbCommand("SELECT * FROM " & TblSlave, conn)
    If Not FillDatset(AdaptMaster, TablMaster) Then
      Exit Sub
    End If
    If Not FillDatset(AdaptSlave, TablSlave) Then
      Exit Sub
    End If

    ReDim PkCols(FieldsMaster.Count - 1)
    ReDim MastSort(FieldsMaster.Count - 1)
    Sort = ""
    For i = 0 To FieldsMaster.Count - 1
      PkCols(i) = TablMaster.Columns(FieldsMaster(i))
      Sort = Sort & FieldsMaster(i)
      If i < FieldsMaster.Count - 1 Then
        Sort = Sort & ","
      End If
    Next '
    '
    '
    'Primarykey anlegen
    '
    '
    '
    TablMaster.PrimaryKey = PkCols
    TablMaster.AcceptChanges()
    TablSlave.AcceptChanges()
    '
    '
    '
    'Felder werden geprüft
    '
    '
    '
    ViewMaster = New DataView(TablMaster)
    ViewMaster.Sort = Sort
    Viewslave = New DataView(TablSlave)
    '
    '
    '
    SqlDelete = "DELETE * FROM " & TblSlave & " WHERE "
    For i = 0 To Viewslave.Count - 1
      '
      '
      '
      '
      For j = 0 To FieldsSlave.Count - 1
        MastSort(j) = Viewslave(i)(FieldsSlave(j))
      Next
      '
      '
      If ViewMaster.Find(MastSort) < 0 Then
        RowfilterTextslave = ""
        For j = 0 To FieldsSlave.Count - 1
          RowfilterTextslave = RowfilterTextslave & FieldsSlave(j) & "=" & Viewslave(i)(FieldsSlave(j))
          If j < FieldsMaster.Count - 1 Then
            RowfilterTextslave = RowfilterTextslave & " AND "
          End If
        Next
        If DelAut OrElse MessageBox.Show("Error :" & RowfilterTextslave, "Eintrag löschen?", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
          '
          'Child-Eintrag wird gelöscht, da Parent-Eintrag fehlt
          '
          '
          DeleteCommand.CommandText = SqlDelete & RowfilterTextslave
          If SQLNonQuery(DeleteCommand, conn) <> 0 Then
            MsgBox("ERRor")
          End If
        End If
      End If
    Next i
    TablMaster.Dispose()
    TablSlave.Dispose()
    ViewMaster.Dispose()
    Viewslave.Dispose()
    AdaptMaster.Dispose()
    AdaptSlave.Dispose()
    DeleteCommand.Dispose()
  End Sub
  '
  '



End Class

